{-# LANGUAGE Strict #-}
-- | The one item-materialization boundary (#1418, epic #1231 PLC-3).
--
--   Four things are proved here, and they are deliberately different
--   KINDS of proof:
--
--   * the recursive @contents:@ authoring contract, decoded through the
--     production YAML loader — including the three-way omitted \/
--     explicitly-empty \/ replaced distinction, which a decoder written
--     the obvious way silently collapses;
--   * what 'materializeItem' produces — field preservation, per-node id
--     allocation, root-scoped overrides, and the random source and draws
--     a definition without defaults consumes;
--   * that SHIPPED data materializes: the first-aid kit the technomule
--     carries arrives stocked to its authored depth; and
--   * that no production module mints an 'ItemInstance' any other way.
--     That last one is a STRUCTURAL allowlist over all of @src\/@ and
--     @app\/@ rather than a denylist over today's mint modules, so a
--     ninth mint site introduced in a NEW module fails it too.
module Test.Headless.Item.Materialize (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Data.Char (isAlphaNum)
import Control.Monad (filterM)
import Data.IORef
    (newIORef, readIORef, writeIORef, modifyIORef', atomicModifyIORef')
import Data.List (isPrefixOf, nub, sort)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml
import qualified HsLua as Lua
import System.Directory
    ( getTemporaryDirectory, createDirectoryIfMissing, doesFileExist
    , removeDirectoryRecursive )
import System.FilePath ((</>))
import System.Random (StdGen, mkStdGen)
import Engine.Asset.Discovery (walkFilesWithExtension)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.YamlItems (ItemYamlDef(..), loadItemYaml)
import Engine.Asset.YamlLootTables
    ( LootTableYamlDef(..), LootTableYamlEntry(..) )
import Engine.Asset.YamlUnits
    ( UnitYamlDef(..), UnitYamlInventoryEntry(..), loadUnitYaml )
import Engine.Core.Log
    ( initLogger, defaultLogConfig, LogConfig(..), LogBackend(..)
    , LogCategory(..), LogEntry(..), LogLevel(..), LoggerState(..) )
import Engine.Scripting.Lua.API.Items.Defs (itemDefFromYaml)
import Engine.Scripting.Lua.API.Items.Contents (pushGroupedContents)
import Item.Materialize
    ( ItemOverrides(..), materializeItem, pristineItem
    , pristineCondition, pristineSharpness )
import Item.Roll (rollItemSpec, rollItemWeight)
import Item.Types
    ( ItemContainer(..), ItemContentEntry(..), ItemDef(..)
    , ItemInstance(..), ItemManager(..), ItemStorage(..), lookupItemDef
    , itemTotalWeight )
import LootTable.Roll (LootRollContext(..), rollLootTableFor)
import LootTable.Types (LootTableDef(..), LootTableEntry(..))

-- * Fixtures

-- | A definition with nothing rolled and nothing held. Every fixture
--   below is this plus the one property it is about, so a test that
--   passes says something about that property rather than about a
--   twenty-field literal.
bareDef ∷ Text → ItemDef
bareDef name = ItemDef
    { idName = name, idDisplayName = name, idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
    , idWeight = 0.5, idWeightSpec = Nothing, idBulk = 1.0
    , idStorage = Nothing, idKind = "misc"
    , idCategory = "Misc", idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = Nothing, idDefaultContents = []
    , idFood = Nothing, idWeapon = Nothing, idArmor = Nothing
    , idUnequippable = False, idBuffs = [], idInsulation = 0
    , idSourcePath = "test-fixture"
    }

-- | An authored entry with no nested @contents:@ — the historical flat
--   shape, which is what "omitted delegates to the child definition"
--   applies to.
entry ∷ Text → Int → ItemContentEntry
entry name n = ItemContentEntry
    { iceItem = name, iceCount = n, iceFill = Nothing, iceContents = Nothing }

-- | The fixture registry. @kit@ authors flat defaults two levels deep
--   (a pouch of its own), so "materialise to the authored depth" has a
--   depth to reach; @bottle@ is a fluid container with a non-zero
--   default fill; @gem@ rolls both quality and weight, so the draw
--   comparison has draws to compare.
testItems ∷ ItemManager
testItems = ItemManager $ HM.fromList
    [ ("kit", (bareDef "kit")
        { idDefaultContents = [entry "bandage" 2, entry "pouch" 1]
        , idStorage = Just (ItemStorage 5.0 6.0) })
    , ("pouch", (bareDef "pouch")
        { idDefaultContents = [entry "bandage" 1, entry "bottle" 1] })
    , ("bandage", bareDef "bandage")
    , ("bottle", (bareDef "bottle")
        { idContainer = Just ItemContainer
            { icCapacity = 2.0, icHolds = "antiseptic"
            , icFillWeight = 1.0, icDefaultFill = 1.5 } })
    , ("gem", (bareDef "gem")
        { idQualitySpec = Just (20, 80)
        , idWeightSpec = Just (0.4, 0.2) })
    ]

-- | Every def name reachable from the fixture registry, for the tree
--   walks below.
allNames ∷ ItemInstance → [Text]
allNames i = iiDefName i : concatMap allNames (iiContents i)

allInstances ∷ ItemInstance → [ItemInstance]
allInstances i = i : concatMap allInstances (iiContents i)

-- | A counting instance-id allocator: hands out 1, 2, 3, … and records
--   how many were spent, so "exactly one advance per materialized node"
--   is measured rather than inferred from the ids that came out.
newAllocator ∷ IO (IO Word64, IO Word64)
newAllocator = do
    ref ← newIORef (0 ∷ Word64)
    pure ( atomicModifyIORef' ref (\n → (n + 1, n + 1))
         , readIORef ref )

-- | Materialise against the fixture registry with a pinned generator.
mint ∷ ItemOverrides → Text → IO (Maybe ItemInstance, Word64)
mint ovr name = do
    logger ← silentLogger
    rng ← newIORef (mkStdGen 90210)
    (alloc, spent) ← newAllocator
    inst ← materializeItem testItems logger rng alloc ovr name
    n ← spent
    pure (inst, n)

mintWith ∷ ItemManager → ItemOverrides → Text → IO (Maybe ItemInstance)
mintWith mgr ovr name = do
    logger ← silentLogger
    rng ← newIORef (mkStdGen 90210)
    (alloc, _) ← newAllocator
    materializeItem mgr logger rng alloc ovr name

-- | 'mintWith' against a CAPTURING logger: the instance, the instance
--   ids spent, and every entry the mint logged, in order. The cycle
--   cases below assert on the whole list rather than on a match, which
--   is what makes "reported once, by nobody unwinding" testable.
mintLogged ∷ ItemManager → Text
           → IO (Maybe ItemInstance, Word64, [LogEntry])
mintLogged mgr name = do
    entriesRef ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (⧺ [e])) }
    -- initLogger honours ENGINE_LOG_LEVEL, so pin the threshold: a
    -- developer running with it set to `error` would otherwise watch
    -- every warning assertion below quietly stop asserting anything.
    writeIORef (lsMinLevel logger) LevelDebug
    rng ← newIORef (mkStdGen 90210)
    (alloc, spent) ← newAllocator
    inst ← materializeItem mgr logger rng alloc pristineItem name
    n ← spent
    entries ← readIORef entriesRef
    pure (inst, n, entries)

-- | The fixture registry with one definition's default contents
--   replaced, so the three-way composition rule can be authored on a
--   PARENT and observed on its child.
withDefaults ∷ Text → [ItemContentEntry] → ItemManager
withDefaults name entries = withManyDefaults [(name, entries)]

-- | The same, for several definitions at once — which is what a cycle
--   ACROSS definitions needs.
withManyDefaults ∷ [(Text, [ItemContentEntry])] → ItemManager
withManyDefaults edits = ItemManager $ foldl' one (imDefs testItems) edits
  where
    one m (name, entries) =
        HM.adjust (\d → d { idDefaultContents = entries }) name m

-- * YAML fixtures

silentLogger ∷ IO LoggerState
silentLogger = initLogger defaultLogConfig
    { lcBackend = LogToCallback (\_ → pure ()) }

withTempItemYaml ∷ String → (FilePath → IO α) → IO α
withTempItemYaml contents action = do
    tmp ← getTemporaryDirectory
    let dir  = tmp </> "synarchy-1418-materialize"
        path = dir </> "probe_items.yaml"
    createDirectoryIfMissing True dir
    writeFile path contents
    action path `finally` removeDirectoryRecursive dir

-- | Load one temp item file and return the authored entries by
--   definition name, THROUGH the production decoder and the production
--   YAML→registry mapping. A test that reimplemented either would prove
--   the reimplementation.
authoredContents ∷ String → IO [(Text, [ItemContentEntry])]
authoredContents body = withTempItemYaml body $ \path → do
    logger ← silentLogger
    defs ← loadItemYaml logger path
    pure [ ( iydName d
           , idDefaultContents (itemDefFromYaml path (TextureHandle 0) (TextureHandle 0) d) )
         | d ← defs ]

-- * The structural guard

-- | Where an 'ItemInstance' record may be CONSTRUCTED in production
--   (#1418 requirement 10). Deliberately an allowlist over the whole
--   tree, not a denylist over today's six mint modules: a ninth mint
--   site added in a brand-new module has to fail this too, and under a
--   denylist it would not.
--
--   Record UPDATES (@it { iiSharpness = … }@ in combat wear, equipment
--   degradation) are not constructions and never appear here — they
--   name a value, not the constructor.
constructionAllowlist ∷ [FilePath]
constructionAllowlist =
    [ -- The one mint boundary.
      "src/Item/Materialize.hs"
      -- Save reconstruction: rebuilds an instance that was already
      -- materialized once, from its own persisted fields.
    , "src/World/Save/Component/Page.hs"
    ]

-- | Every production module that must reach the boundary — the six
--   holding the eight mint sites. Named so the guard cannot pass by the
--   mint sites having quietly moved somewhere unwatched.
mintModules ∷ [FilePath]
mintModules =
    [ "src/Unit/Thread/Command/Spawn.hs"                     -- ×3
    , "src/World/Thread/Command/Edit/Dig.hs"
    , "src/Engine/Scripting/Lua/API/Craft/Execute.hs"
    , "src/Engine/Scripting/Lua/API/Items/Ground.hs"
    , "src/Engine/Scripting/Lua/API/Units/Inventory.hs"
    , "src/Engine/Scripting/Lua/API/Forage/Harvest.hs"
    ]

-- | Production Haskell sources, in a stable order.
productionSources ∷ IO [FilePath]
productionSources = do
    srcs ← under "src"
    apps ← under "app"
    pure (sort (srcs ⧺ apps))
  where
    -- walkFilesWithExtension answers paths RELATIVE to its root.
    under root = map (root </>) <$> walkFilesWithExtension root ".hs"

-- | Every @ItemInstance {@ record construction in one file, as
--   @path:line@.
--
--   Three rules keep this from crying wolf, each earned:
--
--   * the token must be a whole identifier — @ItemInstanceDTO@ is a
--     different type and appears all over the persistence layer;
--   * the next non-blank, non-comment character must be @{@, so a type
--     signature (@→ ItemInstance@), a list element (@[ItemInstance]@)
--     and an import all pass by, while a construction whose brace sits
--     on the following line does not;
--   * the data DECLARATION in "Item.Types" is not a construction. It is
--     recognised by the @data ItemInstance =@ that precedes it, so that
--     module keeps NO blanket exemption and a real mint literal added
--     there would still be reported.
--
--   Comments are line-based: everything from the first @--@ is dropped,
--   which is what makes a commented-out literal (and a haddock naming
--   the type) not a finding. The last case in this spec mutation-checks
--   all of that against the real tree.
constructionSites ∷ FilePath → IO [String]
constructionSites path = do
    body ← TIO.readFile path
    let ls = zip [1 ∷ Int ..] (map stripComment (T.lines body))
    pure [ path <> ":" <> show n
         | (n, line) ← ls
         , col ← occurrences line
         , not (isDeclaration line col)
         , bracedFrom (T.drop (col + T.length token) line)
                      (map snd (drop n ls)) ]
  where
    token = "ItemInstance"
    stripComment line = T.strip (fst (T.breakOn "--" line))
    -- Offsets of `token` as a whole identifier.
    occurrences line = go 0 line
      where
        go acc rest = case T.breakOn token rest of
            (_, after) | T.null after → []
            (before, after) →
                let at   = acc + T.length before
                    tail' = T.drop (T.length token) after
                    ok = maybe True (not ∘ identChar) (lastMaybe before)
                       ∧ maybe True (not ∘ identChar) (fstMaybe tail')
                in [at | ok] ⧺ go (at + T.length token) tail'
    identChar c = isAlphaNum c ∨ c ≡ '_' ∨ c ≡ '\'' ∨ c ≡ '.'
    lastMaybe t = if T.null t then Nothing else Just (T.last t)
    fstMaybe t  = if T.null t then Nothing else Just (T.head t)
    isDeclaration line col =
        "data ItemInstance =" `T.isSuffixOf` T.strip (T.take col line)
    -- The first non-blank character at or after this point, looking on
    -- to later lines when the rest of this one is empty.
    bracedFrom rest laterLines = case T.uncons (T.stripStart rest) of
        Just (c, _) → c ≡ '{'
        Nothing → case dropWhile T.null (map T.strip laterLines) of
            (l : _) → T.isPrefixOf "{" l
            []      → False

-- | The three hand tools the field toolbox ships stocked with (#1855),
--   as (name, sprite, (display name, weight, bulk, kind, category,
--   make, material)). @kind@ is @"misc"@ because the definitions author
--   no @kind:@ at all — that IS the non-equippable kind.
toolMetadata ∷ [(Text, Text, (Text, Float, Float, Text, Text, Text, Text))]
toolMetadata =
    [ ( "crescent_wrench", "assets/textures/items/tool/wrench_1.png"
      , ("Crescent Wrench", 0.5, 0.6, "misc", "Tools", "factory", "steel") )
    , ( "hammer", "assets/textures/items/tool/hammer.png"
      , ("Hammer", 0.8, 1.2, "misc", "Tools", "factory", "steel") )
    , ( "phillips_screwdriver"
      , "assets/textures/items/tool/screwdriver_1.png"
      , ("Phillips Screwdriver", 0.1, 0.2, "misc", "Tools", "factory"
        , "steel") )
    ]

-- | The engine's shipped loot-table YAML conversion, kept here so the
--   toolbox test rolls the live table rather than an in-test approximation.
toLootTableDef ∷ LootTableYamlDef → LootTableDef
toLootTableDef d = LootTableDef
    { ltdId = ltydId d
    , ltdEntries = [ LootTableEntry (ltyeId e) (ltyeWeight e)
                   | e ← ltydEntries d ]
    }

-- | Read the exact grouped row fields the Contents view consumes, through
--   the production row builder shared by unit and remembered-container APIs.
groupedContentRows
    ∷ ItemManager → [ItemInstance] → IO [(Text, Text, Int)]
groupedContentRows mgr contents = Lua.run @Lua.Exception $ do
    Lua.openlibs
    pushGroupedContents mgr contents
    rawCount ← Lua.rawlen (-1)
    let count = fromIntegral rawCount ∷ Int
    rows ← forM [1 .. count] $ \idx → do
        _ ← Lua.rawgeti (-1) (fromIntegral idx)
        defName ← textField "defName"
        displayName ← textField "displayName"
        itemCount ← intField "count"
        Lua.pop 1
        pure (defName, displayName, itemCount)
    Lua.pop 1
    pure rows
  where
    textField field = do
        _ ← Lua.getfield (-1) field
        val ← Lua.tostring (-1)
        Lua.pop 1
        pure (maybe "" TE.decodeUtf8Lenient val)
    intField field = do
        _ ← Lua.getfield (-1) field
        val ← Lua.tointeger (-1)
        Lua.pop 1
        pure (maybe (-1) fromIntegral val)

spec ∷ Spec
spec = do
    describe "recursive default-content authoring (requirement 6)" $ do
        it "decodes the historical flat entry completely unchanged" $ do
            got ← authoredContents $ unlines
                [ "items:"
                , "  - name: flat_kit"
                , "    sprite: a.png"
                , "    bulk: 1.0"
                , "    contents:"
                , "      - { item: bandage, count: 2, fill: 1 }"
                ]
            map snd got `shouldBe`
                [[ ItemContentEntry { iceItem = "bandage", iceCount = 2
                                    , iceFill = Just 1.0
                                    , iceContents = Nothing } ]]

        it "distinguishes an OMITTED contents from an explicitly EMPTY \
           \one — the distinction requirement 6 is built on" $ do
            got ← authoredContents $ unlines
                [ "items:"
                , "  - name: three_ways"
                , "    sprite: a.png"
                , "    bulk: 1.0"
                , "    contents:"
                , "      - { item: delegating, count: 1 }"
                , "      - { item: emptied, count: 1, contents: [] }"
                , "      - item: replaced"
                , "        count: 1"
                , "        contents:"
                , "          - { item: bandage, count: 3 }"
                ]
            map (map iceContents ∘ snd) got `shouldBe`
                [[ Nothing
                 , Just []
                 , Just [ ItemContentEntry { iceItem = "bandage"
                                           , iceCount = 3
                                           , iceFill = Nothing
                                           , iceContents = Nothing } ] ]]

        it "reads an explicit `contents: null` as OMITTED, not as empty" $ do
            -- Aeson's `.:?` treats an authored null as absent. That is
            -- the behaviour we want (saying nothing leaves it to the
            -- child definition) and it is asserted so it stays
            -- deliberate rather than incidental.
            got ← authoredContents $ unlines
                [ "items:"
                , "  - name: nulled"
                , "    sprite: a.png"
                , "    bulk: 1.0"
                , "    contents:"
                , "      - { item: bandage, count: 1, contents: null }"
                ]
            map (map iceContents ∘ snd) got `shouldBe` [[Nothing]]

        it "nests to arbitrary depth" $ do
            got ← authoredContents $ unlines
                [ "items:"
                , "  - name: deep"
                , "    sprite: a.png"
                , "    bulk: 1.0"
                , "    contents:"
                , "      - item: a"
                , "        contents:"
                , "          - item: b"
                , "            contents:"
                , "              - { item: c, count: 1 }"
                ]
            let depth e = case iceContents e of
                    Just (x : _) → 1 + depth x
                    _            → 0 ∷ Int
            map (map depth ∘ snd) got `shouldBe` [[2]]

    describe "materializing a tree" $ do
        it "materialises a definition's defaults to the AUTHORED depth" $ do
            (mInst, _) ← mint pristineItem "kit"
            case mInst of
                Nothing → expectationFailure "kit did not materialize"
                Just k → do
                    sort (map iiDefName (iiContents k))
                        `shouldBe` ["bandage", "bandage", "pouch"]
                    sort (allNames k) `shouldBe`
                        sort [ "kit", "bandage", "bandage"
                             , "pouch", "bandage", "bottle" ]

        it "spends exactly one instance id per materialized node, all \
           \distinct" $ do
            (mInst, spent) ← mint pristineItem "kit"
            case mInst of
                Nothing → expectationFailure "kit did not materialize"
                Just k → do
                    let ids = map iiInstanceId (allInstances k)
                    spent `shouldBe` fromIntegral (length ids)
                    sort ids `shouldBe` [1 .. fromIntegral (length ids)]

        it "gives every count occurrence its OWN subtree, never one \
           \shared twice" $ do
            inst ← mintWith (withDefaults "kit" [entry "pouch" 2])
                            pristineItem "kit"
            case inst of
                Nothing → expectationFailure "kit did not materialize"
                Just k → case iiContents k of
                    [p, q] → do
                        let idsOf t = map iiInstanceId (allInstances t)
                        -- A pouch is a three-node subtree of its own.
                        (length (idsOf p), length (idsOf q))
                            `shouldBe` (3, 3)
                        (any (`elem` idsOf q) (idsOf p)) `shouldBe` False
                    other → expectationFailure
                        ("expected two pouches, got "
                          <> show (map iiDefName other))

        it "materialises nothing for a non-positive count, and skips an \
           \entry naming an unknown definition" $ do
            inst ← mintWith
                (withDefaults "kit"
                    [ entry "bandage" 0, entry "bandage" (-3)
                    , entry "no_such_item" 2, entry "bandage" 1 ])
                pristineItem "kit"
            (map iiDefName ∘ iiContents <$> inst) `shouldBe` Just ["bandage"]

        it "returns Nothing for an unknown ROOT name, spending no id" $ do
            (inst, spent) ← mint pristineItem "no_such_item"
            (isNothing inst, spent) `shouldBe` (True, 0)

    describe "cyclic default contents (#1420)" $ do
        -- Default contents recurse by definition NAME, so a definition
        -- listing itself — or two listing each other — used to recurse
        -- forever and hang the minting thread silently. Every case here
        -- asserts the WHOLE captured log, not a match against it: the
        -- rule is "one warning per rejected back edge, from the frame
        -- that rejected it", and an unwinding frame or a caller adding
        -- a second one has to fail.
        let logged es = [ (leLevel e, leCategory e, leMessage e) | e ← es ]
            childNames = map iiDefName ∘ iiContents
            cycleWarning names repeated =
                ( LevelWarn, CatAsset
                , "Item default contents: cyclic contents graph "
                    <> T.intercalate " → " [ "'" <> n <> "'" | n ← names ]
                    <> " — dropping the repeated '" <> repeated
                    <> "' entry" )

        it "terminates on a self-referential definition, keeping the \
           \instance and its other contents" $ do
            (inst, spent, es) ← mintLogged
                (withDefaults "kit" [entry "kit" 1, entry "bandage" 2])
                "kit"
            (childNames <$> inst) `shouldBe` Just ["bandage", "bandage"]
            -- The dropped back edge is a skip, not a truncation: the
            -- sibling AFTER it still materialises, and the entry that
            -- went nowhere spends no instance id.
            spent `shouldBe` 3
            logged es `shouldBe` [cycleWarning ["kit", "kit"] "kit"]

        it "terminates on a mutually-referential pair, keeping the outer \
           \instance and the nested one, minus only the back edge" $ do
            (inst, _, es) ← mintLogged
                (withManyDefaults
                    [ ("kit",   [entry "pouch" 1])
                    , ("pouch", [entry "bandage" 1, entry "kit" 1]) ])
                "kit"
            case inst of
                Nothing → expectationFailure "kit did not materialize"
                Just k → do
                    childNames k `shouldBe` ["pouch"]
                    map childNames (iiContents k) `shouldBe` [["bandage"]]
            logged es
                `shouldBe` [cycleWarning ["kit", "pouch", "kit"] "kit"]

        it "names every definition ON the cycle and nothing that merely \
           \led to it" $ do
            -- gem → kit → pouch → bandage → kit. `gem` is an acyclic
            -- prefix, not a participant, and reporting it would point
            -- the author at a definition that is not the problem.
            (_, _, es) ← mintLogged
                (withManyDefaults
                    [ ("gem",     [entry "kit" 1])
                    , ("kit",     [entry "pouch" 1])
                    , ("pouch",   [entry "bandage" 1])
                    , ("bandage", [entry "kit" 1]) ])
                "gem"
            logged es `shouldBe`
                [cycleWarning ["kit", "pouch", "bandage", "kit"] "kit"]
            [ m | (_, _, m) ← logged es, "gem" `T.isInfixOf` m ]
                `shouldBe` []

        it "reports ONE authoring mistake once, however many copies the \
           \cyclic entry\'s count asks for" $ do
            (_, _, es) ← mintLogged
                (withDefaults "kit" [entry "kit" 5]) "kit"
            logged es `shouldBe` [cycleWarning ["kit", "kit"] "kit"]

        it "leaves an acyclic graph that REUSES one definition in \
           \separate branches completely alone — detection is scoped to \
           \the current path, never a global visited set" $ do
            (inst, _, es) ← mintLogged
                (withManyDefaults
                    [ ("kit",     [entry "pouch" 2, entry "bandage" 1])
                    , ("pouch",   [entry "bandage" 1, entry "bottle" 1]) ])
                "kit"
            case inst of
                Nothing → expectationFailure "kit did not materialize"
                Just k → sort (allNames k) `shouldBe` sort
                    [ "kit", "pouch", "bandage", "bottle"
                    , "pouch", "bandage", "bottle", "bandage" ]
            logged es `shouldBe` []

        it "is not triggered by an entry authoring its OWN contents: a \
           \kit holding a kit is legal nesting, not a cycle" $ do
            -- Such an entry never consults the named definition\'s
            -- defaults, so it is a finite authored subtree that
            -- terminates on its own. Rejecting it would drop contents an
            -- author explicitly wrote.
            (inst, _, es) ← mintLogged
                (withDefaults "kit"
                    [ (entry "kit" 1)
                        { iceContents = Just [entry "bandage" 1] } ])
                "kit"
            case inst of
                Nothing → expectationFailure "kit did not materialize"
                Just k → do
                    childNames k `shouldBe` ["kit"]
                    map childNames (iiContents k) `shouldBe` [["bandage"]]
            logged es `shouldBe` []

        it "still catches a back edge one level UNDER such an authored \
           \nesting" $ do
            (inst, _, es) ← mintLogged
                (withDefaults "kit"
                    [ (entry "kit" 1)
                        { iceContents = Just [entry "kit" 1] } ])
                "kit"
            (map childNames ∘ iiContents <$> inst) `shouldBe` Just [[]]
            logged es `shouldBe` [cycleWarning ["kit", "kit"] "kit"]

        it "materialises the acyclic fixture and the SHIPPED first-aid \
           \kit silently, to the depth and contents they always had" $ do
            (fixture, _, fixtureLog) ← mintLogged testItems "kit"
            mgr ← shippedItems
            (shipped, _, shippedLog) ← mintLogged mgr "first_aid_kit"
            (sort ∘ allNames <$> fixture) `shouldBe` Just (sort
                [ "kit", "bandage", "bandage"
                , "pouch", "bandage", "bottle" ])
            (length ∘ allInstances <$> shipped) `shouldBe` Just 30
            (logged fixtureLog, logged shippedLog) `shouldBe` ([], [])

    describe "the three-way nested composition rule (requirement 6)" $ do
        let childrenOfPouch mgr = do
                inst ← mintWith mgr pristineItem "kit"
                pure $ case iiContents <$> inst of
                    Just (p : _) → map iiDefName (iiContents p)
                    _            → ["<no pouch materialized>"]

        it "OMITTED contents delegates to the child definition's own \
           \defaults" $ do
            got ← childrenOfPouch (withDefaults "kit" [entry "pouch" 1])
            sort got `shouldBe` ["bandage", "bottle"]

        it "an explicitly EMPTY contents materialises that child empty, \
           \overriding the child definition's defaults" $ do
            got ← childrenOfPouch $ withDefaults "kit"
                [ (entry "pouch" 1) { iceContents = Just [] } ]
            got `shouldBe` []

        it "a NON-EMPTY contents REPLACES the child definition's \
           \defaults, in authored order" $ do
            got ← childrenOfPouch $ withDefaults "kit"
                [ (entry "pouch" 1)
                    { iceContents = Just [ entry "bottle" 1
                                         , entry "bandage" 2 ] } ]
            got `shouldBe` ["bottle", "bandage", "bandage"]

        it "an authored child fill wins over the child definition's \
           \default_fill, and clamps to capacity" $ do
            inst ← mintWith
                (withDefaults "kit"
                    [ (entry "bottle" 1) { iceFill = Just 0.25 }
                    , (entry "bottle" 1) { iceFill = Just 99 }
                    , entry "bottle" 1 ])
                pristineItem "kit"
            (map iiCurrentFill ∘ iiContents <$> inst)
                `shouldBe` Just [0.25, 2.0, 1.5]

    describe "root-scoped overrides (requirement 5)" $ do
        it "applies every override to the ROOT" $ do
            (inst, _) ← mint ItemOverrides { ovFill = Just 1.25
                                           , ovQuality = Just 42
                                           , ovCondition = Just 37
                                           , ovTemp = Just 91 } "bottle"
            ( iiCurrentFill <$> inst, iiQuality <$> inst
              , iiCondition <$> inst, iiTemp <$> inst )
                `shouldBe` (Just 1.25, Just 42, Just 37, Just (Just 91))

        it "propagates NONE of them into default-content descendants" $ do
            -- The regression this exists for: a ground item's Lua
            -- quality/condition props, or a crafted item's output
            -- temperature, silently re-describing everything the
            -- container spawns holding.
            inst ← mintWith
                (withDefaults "kit" [entry "bottle" 1])
                ItemOverrides { ovFill = Just 0.1, ovQuality = Just 42
                              , ovCondition = Just 37, ovTemp = Just 91 }
                "kit"
            case iiContents <$> inst of
                Just [child] →
                    ( iiCurrentFill child, iiQuality child
                    , iiCondition child, iiTemp child )
                        `shouldBe` (1.5, 100, pristineCondition, Nothing)
                other → expectationFailure ("expected one child: "
                                              <> show (length <$> other))

    describe "field preservation (requirement 1)" $
        it "fills in every instance field from the definition" $ do
            (inst, _) ← mint pristineItem "kit"
            case inst of
                Nothing → expectationFailure "kit did not materialize"
                Just k → do
                    iiDefName k    `shouldBe` "kit"
                    iiCurrentFill k `shouldBe` 0        -- not a container
                    iiQuality k    `shouldBe` 100       -- no quality spec
                    iiCondition k  `shouldBe` pristineCondition
                    iiWeight k     `shouldBe` 0.5       -- no weight spec
                    iiSharpness k  `shouldBe` pristineSharpness
                    iiTemp k       `shouldBe` Nothing
                    -- #1233 snapshots, taken from the definition.
                    iiBulk k       `shouldBe` Just 1.0
                    iiStorage k    `shouldBe` Just (ItemStorage 5.0 6.0)
                    length (iiContents k) `shouldBe` 3
                    iiInstanceId k `shouldBe` 6

    describe "random source and draw behaviour (requirement 3)" $
        it "a definition without defaults consumes exactly the quality \
           \and weight draws it always did, from the supplied source" $ do
            -- Same generator, same order, same end state — so the shared
            -- stat stream is left exactly where the hand-written mint
            -- sites left it for every later consumer.
            logger ← silentLogger
            rngA ← newIORef (mkStdGen 4242)
            (alloc, _) ← newAllocator
            minted ← materializeItem testItems logger rngA alloc
                                     pristineItem "gem"
            endA ← readIORef rngA
            rngB ← newIORef (mkStdGen 4242)
            let gemDef = fromMaybe (error "gem fixture missing")
                                   (lookupItemDef "gem" testItems)
            q ← rollItemSpec (idQualitySpec gemDef) rngB
            w ← rollItemWeight gemDef rngB
            endB ← readIORef rngB
            ( iiQuality <$> minted, iiWeight <$> minted, show endA )
                `shouldBe` (Just q, Just w, show (endB ∷ StdGen))

    describe "shipped data (requirement 7)" $ do
        it "the technomule's starting inventory really does carry the \
           \first-aid kit" $ do
            logger ← silentLogger
            defs ← loadUnitYaml logger "data/units/technomule.yaml"
            let carried = [ uyieItem e
                          | d ← defs, e ← uydStartingInventory d ]
            ("first_aid_kit" `elem` carried) `shouldBe` True

        it "materialises the SHIPPED first-aid kit stocked to its \
           \authored loadout" $ do
            mgr ← shippedItems
            inst ← mintWith mgr pristineItem "first_aid_kit"
            case inst of
                Nothing → expectationFailure "shipped kit did not materialize"
                Just k → do
                    let counts = HM.toList $ HM.fromListWith (+)
                            [ (iiDefName c, 1 ∷ Int) | c ← iiContents k ]
                    sort counts `shouldBe` sort
                        [ ("bandage", 10), ("gauze", 10)
                        , ("elastic_wrap", 5), ("antibiotics", 1)
                        , ("antiseptic", 1), ("tweezers", 1)
                        , ("scissors", 1) ]
                    -- The two authored fills survive; every instance in
                    -- the tree got its own id.
                    let fillOf n = [ iiCurrentFill c | c ← iiContents k
                                                     , iiDefName c ≡ n ]
                    fillOf "antibiotics" `shouldBe` [60]
                    fillOf "antiseptic" `shouldBe` [1.0]
                    let ids = map iiInstanceId (allInstances k)
                    length ids `shouldBe` 30
                    sort ids `shouldBe` [1 .. 30]

    describe "field toolbox content" $ do
        it "loads the three shipped hand-tool definitions with their exact \
           \authored metadata" $ do
            logger ← silentLogger
            yamlDefs ← loadItemYaml logger "data/items/hand_tools.yaml"
            mapM_ (\d → doesFileExist (T.unpack (iydSprite d))
                          `shouldReturn` True)
                  yamlDefs

            [ (iydName d, iydSprite d) | d ← yamlDefs ] `shouldBe`
                [ (name, sprite) | (name, sprite, _) ← toolMetadata ]

            mgr ← shippedItems
            forM_ toolMetadata $ \(name, _, expected) →
                case lookupItemDef name mgr of
                    Nothing → expectationFailure
                        (T.unpack name
                         <> " missing from the shipped item registry")
                    Just def → do
                        ( idDisplayName def, idWeight def, idBulk def
                          , idKind def, idCategory def, idMake def
                          , idMaterial def )
                            `shouldBe` expected
                        -- A fixed scalar `weight:` decodes to WeightFixed,
                        -- so the def carries NO {mean, range} spec: every
                        -- instance masses exactly idWeight.
                        idWeightSpec def `shouldBe` Nothing
                        -- Ordinary and non-equippable: `kind` left at its
                        -- "misc" default, no weapon/armor block, and
                        -- `unequippable` (which means an accessory that
                        -- cannot be REMOVED) untouched.
                        idWeapon def `shouldBe` Nothing
                        idArmor def `shouldBe` Nothing
                        idUnequippable def `shouldBe` False
                        idDefaultContents def `shouldBe` []
                        idStorage def `shouldBe` Nothing
                        idContainer def `shouldBe` Nothing

        it "loads and materialises the shipped definition's exact Contents view" $ do
            logger ← silentLogger
            yamlDefs ← loadItemYaml logger "data/items/field_toolbox.yaml"
            case yamlDefs of
                [yamlDef] → do
                    iydName yamlDef `shouldBe` "field_toolbox"
                    iydSprite yamlDef
                        `shouldBe` "assets/textures/items/tool/toolbox.png"
                    doesFileExist (T.unpack (iydSprite yamlDef))
                        `shouldReturn` True
                other → expectationFailure
                    ("expected one field toolbox definition, got "
                     <> show (length other))

            mgr ← shippedItems
            case lookupItemDef "field_toolbox" mgr of
                Nothing → expectationFailure
                    "field_toolbox missing from the shipped item registry"
                Just def → do
                    ( idDisplayName def, idWeight def, idBulk def
                      , idKind def, idCategory def, idMake def
                      , idMaterial def )
                        `shouldBe` ( "Field Toolbox", 1.2, 6.0
                                   , "container", "Tools", "factory"
                                   , "steel" )
                    idContainer def `shouldBe` Nothing
                    -- Requirement 3: still no container-capacity or
                    -- portable-storage block; the box is inspectable
                    -- content, not player-managed storage.
                    idStorage def `shouldBe` Nothing
                    let authored =
                            [ ( iceItem e, iceCount e
                              , iceFill e, iceContents e )
                            | e ← idDefaultContents def ]
                    authored `shouldBe`
                        [ ("crescent_wrench", 1, Nothing, Nothing)
                        , ("hammer", 1, Nothing, Nothing)
                        , ("phillips_screwdriver", 1, Nothing, Nothing)
                        ]

            inst ← mintWith mgr pristineItem "field_toolbox"
            case inst of
                Nothing → expectationFailure
                    "shipped field_toolbox did not materialize"
                Just toolbox → do
                    let children = iiContents toolbox
                        ids = map iiInstanceId (allInstances toolbox)
                    -- Three DISTINCT child instances in the authored
                    -- order, each with its own id — not a flattened stack.
                    map iiDefName children `shouldBe`
                        [ "crescent_wrench", "hammer", "phillips_screwdriver" ]
                    length children `shouldBe` 3
                    length (nub ids) `shouldBe` 4
                    -- 1.20 kg empty case + 0.50 + 0.80 + 0.10 of tools.
                    itemTotalWeight mgr toolbox `shouldSatisfy`
                        (\weight → abs (weight - 2.6) < 1.0e-5)
                    rows ← groupedContentRows mgr children
                    rows `shouldBe`
                        [ ("crescent_wrench", "Crescent Wrench", 1)
                        , ("hammer", "Hammer", 1)
                        , ("phillips_screwdriver", "Phillips Screwdriver", 1)
                        ]

        it "is no longer reachable from the shipped ruin loot table" $ do
            shipped ← Yaml.decodeFileEither
                "data/loot_tables/ruin_common.yaml"
            case shipped of
                Left err → expectationFailure (show err)
                Right yamlDef → do
                    let table = toLootTableDef yamlDef
                    map ltyeId (ltydEntries yamlDef)
                        `shouldNotContain` ["field_toolbox"]
                    -- and no context in a wide sweep of the roll space
                    -- selects it either, which the entry list alone
                    -- would not prove if the roller ignored the table.
                    let rolled =
                            [ rollLootTableFor table
                                LootRollContext
                                    { lrcWorldSeed = seed
                                    , lrcInstanceId = i
                                    , lrcEntryIndex = e
                                    , lrcRollIndex = r }
                            | seed ← [42, 1337], i ← [1 .. 20]
                            , e ← [1 .. 5], r ← [1 .. 5] ]
                    nub rolled `shouldNotContain` [Just "field_toolbox"]

    describe "field toolbox starting loadout" $ do
        it "gives the shipped technomule exactly one field toolbox and no \
           \loose hand tools" $ do
            logger ← silentLogger
            defs ← loadUnitYaml logger "data/units/technomule.yaml"
            let carried = [ (uyieItem e, uyieCount e)
                          | d ← defs, e ← uydStartingInventory d ]
            [ c | (n, c) ← carried, n ≡ "field_toolbox" ] `shouldBe` [1]
            [ n | (n, _) ← carried
                , n `elem` [ tn | (tn, _, _) ← toolMetadata ] ]
                `shouldBe` []

        it "materialises that entry as one toolbox holding the three tools" $ do
            logger ← silentLogger
            defs ← loadUnitYaml logger "data/units/technomule.yaml"
            mgr ← shippedItems
            -- The same mint boundary Unit.Thread.Command.Spawn's
            -- buildStartingInventory delegates to, driven by the unit
            -- def's own authored entries.
            boxes ← sequence
                [ mintWith mgr pristineItem (uyieItem e)
                | d ← defs, e ← uydStartingInventory d
                , uyieItem e ≡ "field_toolbox"
                , _ ← [1 .. max 1 (uyieCount e)] ]
            case boxes of
                [Just toolbox] → do
                    map iiDefName (iiContents toolbox) `shouldBe`
                        [ "crescent_wrench", "hammer"
                        , "phillips_screwdriver" ]
                    itemTotalWeight mgr toolbox `shouldSatisfy`
                        (\weight → abs (weight - 2.6) < 1.0e-5)
                other → expectationFailure
                    ("expected exactly one materialized field toolbox, got "
                     <> show (length other))

    describe "no other production mint site (requirement 10)" $ do
        it "every mint module reaches the materializer" $ do
            missing ← filterM
                (fmap (not ∘ T.isInfixOf "materializeItem") ∘ TIO.readFile)
                mintModules
            missing `shouldBe` []

        it "constructs an ItemInstance ONLY at the materializer and the \
           \save decoder" $ do
            sites ← allConstructionSites
            let stray = [ s | s ← sites
                        , not (any (\a → (a <> ":") `isPrefixOf` s)
                                   constructionAllowlist) ]
            stray `shouldBe` []

        it "and the guard itself is not vacuous: it FINDS both allowed \
           \constructions, and reads a record UPDATE as no construction \
           \at all" $ do
            sites ← allConstructionSites
            nub (map (takeWhile (≢ ':')) sites)
                `shouldBe` sort constructionAllowlist
            -- Combat wear updates an existing instance in place; if the
            -- guard called that a construction, no allowlist could ever
            -- be exhaustive.
            wear ← constructionSites "src/Combat/Resolution/Wear.hs"
            wear `shouldBe` []

-- | Every production 'ItemInstance' construction site, tree-wide.
allConstructionSites ∷ IO [String]
allConstructionSites = do
    paths ← productionSources
    concat <$> mapM constructionSites paths

-- | Every shipped item definition, through the production loader and
--   the production YAML→registry mapping.
shippedItems ∷ IO ItemManager
shippedItems = do
    logger ← silentLogger
    files ← map ("data/items" </>)
        <$> walkFilesWithExtension "data/items" ".yaml"
    defs ← concat <$> mapM (\p → map ((,) p) <$> loadItemYaml logger p)
                           (sort files)
    pure ∘ ItemManager $ HM.fromList
        [ (iydName d, itemDefFromYaml p (TextureHandle 0) (TextureHandle 0) d)
        | (p, d) ← defs ]
