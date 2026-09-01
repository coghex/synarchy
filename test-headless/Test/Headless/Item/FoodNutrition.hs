-- | The one-positive-nutrition-mode invariant on food item definitions
--   (#1716).
--
--   'Item.Types.ItemFood' has always DOCUMENTED two mutually exclusive
--   shapes — discrete (@calories@ kcal per whole item) and bulk
--   (@calories_per_kg@ kcal per kg of the item's own fill) — and
--   nothing enforced it, so four out-of-contract shapes reached
--   @unit.feed@:
--
--     * __neither mode positive__ — the discrete branch removes the
--       item and then credits @0@, and Lua's @0@ is TRUTHY, so
--       @if not unit.feed(…)@ reads a wasted item as a meal;
--     * __negative discrete calories__ — same branch, and the credit
--       has an upper clamp but no lower one, so the eater ends up
--       HUNGRIER than before eating;
--     * __both modes positive__ — @Survival.hs@ guards the bulk branch
--       first, so the authored discrete value is silently unreachable;
--     * __bulk nutrition with no @container:@__ — the bulk branch draws
--       from @iiCurrentFill@, which is 0 for every non-container, so
--       the food can never be eaten at all.
--
--   The fix is at the AUTHORING boundary and nowhere else (requirement
--   9): this spec therefore gates the DECODER, not a clamp in any
--   consumer. Two halves:
--
--     * 'spec' — pure, decoder-level. Every rejected shape, every
--       accepted shape, the numeric domain (finite and nonnegative
--       AFTER narrowing to the engine's 32-bit 'Float', which is what
--       makes both @1.0e+100@ and @1.0e-60@ interesting), the exact
--       diagnostic content, and the shipped corpus.
--     * 'feedSpec' — live, against a real engine and the REAL
--       registered @unit.feed@: a valid discrete and a valid bulk
--       definition, decoded through the production YAML→registry
--       mapping, still feed exactly as they did (requirement 7).
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Item.FoodNutrition"'@ — and the acceptance
--   selector @--match "Item"@ reaches both halves.
module Test.Headless.Item.FoodNutrition (spec, feedSpec) where

import UPrelude
import Test.Hspec
import Data.List (sort)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.IORef (readIORef, writeIORef)
import System.FilePath ((</>))
import Engine.Asset.Discovery (walkFilesWithExtension)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.YamlItems
    ( ItemYamlDef(..), ItemYamlFile(..), ItemYamlFood(..), loadItemYaml )
import Engine.Core.Log
    (initLogger, defaultLogConfig, LogConfig(..), LogBackend(..), LoggerState)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.API.Items.Defs (itemDefFromYaml)
import Engine.Scripting.Lua.Types (LuaBackendState)
import Item.Types
    (ItemDef(..), ItemFood(..), ItemInstance(..), ItemManager(..))
import Unit.Faction (Faction(..))
import Unit.Types
    (UnitId(..), UnitInstance(..), UnitManager(..), emptyUnitManager)
import Test.Headless.Unit.TransferApi
    (evalDebug, mkItem, mkUnit, minimalDef, newBareLuaBackend)

-- * YAML fixtures
--
--   Every fixture is raw source text rather than a constructed value,
--   because half of what is under test is how the YAML scalar resolver
--   and the 'Float' narrowing interact — @.nan@ resolving to a STRING
--   and @1.0e+100@ resolving to a perfectly ordinary 'Scientific' are
--   both facts about the source text, invisible to a fixture built from
--   Haskell values.

-- | A minimally valid, non-food definition named @n@, with @extra@
--   appended verbatim so a @food:@ / @container:@ block (or a
--   deliberately malformed one) is expressible exactly as an author
--   would write it.
probeNamed ∷ String → String → String
probeNamed n extra = unlines
    [ "items:"
    , "  - name: \"" ⧺ n ⧺ "\""
    , "    sprite: \"assets/textures/items/probe.png\""
    , "    weight: 0.5"
    , "    bulk: 1.0"
    ] ⧺ extra

-- | The same, under the name every rejection assertion looks for.
probeDef ∷ String → String
probeDef = probeNamed "probe_meal"

-- | A @food:@ block whose @nutrition:@ body is @body@ verbatim.
foodWith ∷ String → String
foodWith body = "    food:\n      nutrition:\n" ⧺ body

-- | A quinoa-sack-shaped @container:@ block — the thing bulk nutrition
--   draws its fill from.
containerBlock ∷ String
containerBlock = unlines
    [ "    container:"
    , "      capacity: 5.0"
    , "      holds: \"quinoa\""
    , "      fill_weight: 1.0"
    , "      default_fill: 5.0"
    ]

decodeDef ∷ String → Either String ItemYamlDef
decodeDef src = case Yaml.decodeEither' (BSC.pack src) of
    Left err                 → Left (show err)
    Right (ItemYamlFile [d]) → Right d
    Right (ItemYamlFile ds)  →
        Left ("expected exactly one definition, got " ⧺ show (length ds))

-- | Did the decode fail, and does the message name the definition AND
--   every token the reader needs?
--
--   All three halves are load-bearing (requirement 5). A rejection that
--   does not name @probe_meal@ is the unusable @$.items[0].food@
--   diagnostic this exists to rule out; one that does not name the
--   offending KEY leaves an author guessing which of two nutrition
--   fields is wrong; one that does not report the VALUE hides an
--   effective value that differs from what was authored, which is
--   exactly the @1.0e-60@-underflows-to-zero case.
--
--   Tokens are matched as whole WORDS of a punctuation-scrubbed
--   message, not substrings, so @calories@ cannot be satisfied by a
--   message that only ever says @calories_per_kg@. The scrub
--   deliberately leaves @.@ and @-@ alone: they are inside the values
--   (@-35.0@, @1.0e100@) the tokens have to match.
rejectsNaming ∷ [String] → String → Expectation
rejectsNaming tokens src = case decodeDef src of
    Right d → expectationFailure $
        "expected a rejection, but the definition parsed: " ⧺ show d
    Left err →
        let ws      = words (map scrub err)
            missing = [t | t ← "probe_meal" : tokens, t `notElem` ws]
        in if null missing
             then pure ()
             else expectationFailure $
                 "rejected, but the message does not name "
                 ⧺ show missing ⧺ ": " ⧺ err
  where
    scrub c = if c `elem` ("'\"(),:;=\8212" ∷ String) then ' ' else c

-- | The decoded nutrition of a definition that is expected to PARSE.
foodOf ∷ String → Either String (Maybe ItemYamlFood)
foodOf = fmap iydFood ∘ decodeDef

-- * Pure decoder coverage

spec ∷ Spec
spec = do
    describe "accepted shapes" $ do
        it "discrete: calories alone, and calories_per_kg stays exactly \
           \zero" $
            foodOf (probeDef (foodWith "        calories: 250\n"))
                `shouldBe` Right (Just (ItemYamlFood 250 0))

        it "discrete: an integral spelling narrows to the same Float" $
            foodOf (probeDef (foodWith "        calories: 250.0\n"))
                `shouldBe` Right (Just (ItemYamlFood 250 0))

        it "discrete: the inactive key may be written explicitly, so \
           \long as it is zero" $
            foodOf (probeDef (foodWith
                "        calories: 250\n        calories_per_kg: 0\n"))
                `shouldBe` Right (Just (ItemYamlFood 250 0))

        it "discrete: an arbitrarily small positive value is still a \
           \mode — the boundary is zero, and it is exclusive" $
            foodOf (probeDef (foodWith "        calories: 0.001\n"))
                `shouldBe` Right (Just (ItemYamlFood 0.001 0))

        it "discrete food MAY also be a container — the container rule \
           \constrains bulk nutrition only" $
            foodOf (probeDef
                (containerBlock ⧺ foodWith "        calories: 250\n"))
                `shouldBe` Right (Just (ItemYamlFood 250 0))

        it "bulk: calories_per_kg on a definition that authors a \
           \container:" $
            foodOf (probeDef
                (containerBlock ⧺ foodWith "        calories_per_kg: 3680\n"))
                `shouldBe` Right (Just (ItemYamlFood 0 3680))

        it "bulk: the inactive key may be written explicitly, so long \
           \as it is zero" $
            foodOf (probeDef (containerBlock ⧺ foodWith
                "        calories: 0\n        calories_per_kg: 3680\n"))
                `shouldBe` Right (Just (ItemYamlFood 0 3680))

        it "a definition with no food: block is simply not food" $
            foodOf (probeDef "") `shouldBe` Right Nothing

    describe "no mode selected (requirement 1)" $ do
        -- Every message here reports BOTH effective values, because
        -- which key the author meant to fill in is exactly what they
        -- have to decide.
        it "rejects an empty nutrition: block" $
            rejectsNaming ["calories", "calories_per_kg", "0.0"]
                (probeDef "    food:\n      nutrition: {}\n")

        it "rejects a zero calories" $
            rejectsNaming ["calories", "calories_per_kg", "0.0"]
                (probeDef (foodWith "        calories: 0\n"))

        it "rejects both keys authored as zero" $
            rejectsNaming ["calories", "calories_per_kg", "0.0"]
                (probeDef (foodWith
                    "        calories: 0.0\n        calories_per_kg: 0\n"))

        it "rejects a null calories — an authored null is the absent \
           \case, and absent selects no mode" $
            rejectsNaming ["calories", "calories_per_kg", "0.0"]
                (probeDef (foodWith "        calories: null\n"))

        it "rejects a positive value that UNDERFLOWS to zero in the \
           \engine's Float, reporting the effective 0.0 rather than \
           \the authored 1.0e-60" $
            -- The mirror image of the 1.0e+100 trap below: positivity
            -- is evaluated after narrowing, so a Scientific the YAML
            -- parser is perfectly happy with cannot select a mode the
            -- runtime would never see.
            rejectsNaming ["calories", "calories_per_kg", "0.0"]
                (probeDef (foodWith "        calories: 1.0e-60\n"))

    describe "negative nutrition (requirement 1)" $ do
        it "rejects negative discrete calories, naming the key and the \
           \value" $
            -- Unfixed, this is the worst shape of the four: the item is
            -- consumed and the eater's hunger stat goes DOWN.
            rejectsNaming ["calories", "-35.0"]
                (probeDef (foodWith "        calories: -35\n"))

        it "rejects negative bulk calories_per_kg, naming the key and \
           \the value" $
            rejectsNaming ["calories_per_kg", "-3680.0"]
                (probeDef (containerBlock ⧺ foodWith
                    "        calories_per_kg: -3680\n"))

        it "rejects a negative value even when the OTHER key selects a \
           \valid mode" $
            -- Requirement 1 reads "absent, zero, or negative", and the
            -- review pinned this combination explicitly: a negative
            -- inactive key is not a spelling of "inactive".
            rejectsNaming ["calories", "-35.0"]
                (probeDef (containerBlock ⧺ foodWith
                    "        calories: -35\n\
                    \        calories_per_kg: 3680\n"))

    describe "both modes selected (requirement 2)" $ do
        it "rejects both positive, naming BOTH keys and BOTH values" $
            -- The runtime honours calories_per_kg by branch order, so
            -- the authored 250 would silently never be credited.
            rejectsNaming ["calories", "calories_per_kg", "250.0", "3680.0"]
                (probeDef (containerBlock ⧺ foodWith
                    "        calories: 250\n\
                    \        calories_per_kg: 3680\n"))

        it "rejects both positive on a definition with no container \
           \either — mutual exclusion is decided on the values alone" $
            rejectsNaming ["calories", "calories_per_kg", "250.0", "3680.0"]
                (probeDef (foodWith
                    "        calories: 250\n\
                    \        calories_per_kg: 3680\n"))

    describe "non-finite nutrition (requirement 3)" $ do
        -- Two genuinely different faults share this heading. `.nan` and
        -- `.inf` are STRINGS after the YAML scalar resolver runs — the
        -- resolver only recognizes ordinary numeric syntax — so they
        -- never reach the Float at all. `1.0e+100` does: it is a valid
        -- Scientific that becomes Infinity only once narrowed.
        it "rejects .nan calories" $
            rejectsNaming ["calories"]
                (probeDef (foodWith "        calories: .nan\n"))

        it "rejects .inf calories" $
            rejectsNaming ["calories"]
                (probeDef (foodWith "        calories: .inf\n"))

        it "rejects -.inf calories" $
            rejectsNaming ["calories"]
                (probeDef (foodWith "        calories: -.inf\n"))

        it "rejects .nan calories_per_kg" $
            rejectsNaming ["calories_per_kg"]
                (probeDef (containerBlock ⧺ foodWith
                    "        calories_per_kg: .nan\n"))

        it "rejects .inf calories_per_kg" $
            rejectsNaming ["calories_per_kg"]
                (probeDef (containerBlock ⧺ foodWith
                    "        calories_per_kg: .inf\n"))

        it "rejects calories that are finite in YAML but INFINITE once \
           \narrowed to the engine's 32-bit Float" $
            rejectsNaming ["calories", "1.0e100"]
                (probeDef (foodWith "        calories: 1.0e+100\n"))

        it "rejects calories_per_kg on the same terms" $
            rejectsNaming ["calories_per_kg", "1.0e100"]
                (probeDef (containerBlock ⧺ foodWith
                    "        calories_per_kg: 1.0e+100\n"))

        it "rejects a quoted numeric string rather than coercing it" $
            rejectsNaming ["calories"]
                (probeDef (foodWith "        calories: \"250\"\n"))

        it "rejects a boolean" $
            rejectsNaming ["calories"]
                (probeDef (foodWith "        calories: true\n"))

    describe "bulk nutrition without a container (requirement 4)" $ do
        -- `iiCurrentFill` is 0 for every non-container, and the bulk
        -- feed branch requires it to be positive, so this shape is a
        -- silent permanent no-op rather than a load error. Each message
        -- names calories_per_kg, its value, and the missing container.
        let bulk = foodWith "        calories_per_kg: 3680\n"

        it "rejects bulk nutrition on a definition with no container: \
           \key" $
            rejectsNaming ["calories_per_kg", "3680.0", "container"]
                (probeDef bulk)

        -- `container:` keeps `.:?`, which reads an explicit null as
        -- ABSENT — and that is exactly the field the runtime will see,
        -- so all three null spellings must fail as bulk-WITHOUT-
        -- container rather than as some separate container fault.
        it "rejects bulk nutrition beside an explicitly null container" $
            rejectsNaming ["calories_per_kg", "3680.0", "container"]
                (probeDef ("    container: null\n" ⧺ bulk))

        it "rejects bulk nutrition beside a container: key with no \
           \value at all" $
            rejectsNaming ["calories_per_kg", "3680.0", "container"]
                (probeDef ("    container:\n" ⧺ bulk))

        it "rejects bulk nutrition beside a tilde-null container" $
            rejectsNaming ["calories_per_kg", "3680.0", "container"]
                (probeDef ("    container: ~\n" ⧺ bulk))

    describe "malformed food: and nutrition: blocks" $ do
        it "rejects a food: block that authors no nutrition: at all" $
            rejectsNaming ["nutrition"] (probeDef "    food: {}\n")

        it "rejects a non-object nutrition:" $
            rejectsNaming ["nutrition"]
                (probeDef "    food:\n      nutrition: 250\n")

        it "rejects a null nutrition:" $
            rejectsNaming ["nutrition"]
                (probeDef "    food:\n      nutrition: null\n")

        it "rejects a non-object food:" $
            rejectsNaming ["food"] (probeDef "    food: 250\n")

        -- Same reasoning as `storage:` in this decoder: a key the
        -- author WROTE is present, and silently reading it as "this
        -- item is not edible" is the same quiet capability drop.
        it "rejects an explicitly null food: block rather than reading \
           \it as absent" $
            rejectsNaming ["food"] (probeDef "    food: null\n")

        it "rejects a food: key written with no value at all" $
            rejectsNaming ["food"] (probeDef "    food:\n")

    describe "the shipped corpus (requirement 6)" $ do
        it "every shipped item file still decodes, and the loader drops \
           \nothing" $ do
            -- `loadYamlList` WARNS and returns [] on a decode failure
            -- (#1008), so a definition this change newly rejected would
            -- vanish silently. Comparing the loader's output against a
            -- strict decode of the same bytes is what makes that
            -- visible, per file, with no count to keep up to date.
            logger ← silentLogger
            files  ← shippedItemFiles
            files `shouldNotSatisfy` null
            forM_ files $ \p → do
                raw ← BS.readFile p
                case Yaml.decodeEither' raw of
                    Left err → expectationFailure (p ⧺ ": " ⧺ show err)
                    Right (ItemYamlFile ds) → do
                        loaded ← loadItemYaml logger p
                        map iydName loaded `shouldBe` map iydName ds

        it "the six shipped food definitions keep their exact nutrition" $ do
            defs ← shippedDefs
            let foods = sort
                    [ (iydName d, iyfCalories f, iyfCaloriesPerKg f)
                    | d ← defs, Just f ← [iydFood d] ]
            foods `shouldBe` sort
                [ ("quinoa_sack",  0,   3680)
                , ("rations",      250, 0)
                , ("tomato",       35,  0)
                , ("wheat_grain",  80,  0)
                , ("wild_berries", 60,  0)
                , ("wild_greens",  25,  0) ]

        it "every shipped food definition satisfies the invariant — \
           \exactly one positive mode, and bulk only with a container" $ do
            -- A property over whatever is shipped, so a food definition
            -- added later is covered without editing this spec.
            defs ← shippedDefs
            forM_ [(d, f) | d ← defs, Just f ← [iydFood d]] $ \(d, f) → do
                let discrete = iyfCalories f > 0
                    bulk     = iyfCaloriesPerKg f > 0
                (iydName d, discrete ≢ bulk) `shouldBe` (iydName d, True)
                (iydName d, bulk ∧ iydContainer d ≡ Nothing)
                    `shouldBe` (iydName d, False)

-- * Live @unit.feed@ coverage (requirement 7)

-- | uid 1 is the only unit in every scenario below.
eaterUid ∷ UnitId
eaterUid = UnitId 1

-- | The eater's stomach capacity, a round number near the ~713 kcal a
--   default acolyte actually carries. Both modes clamp against it.
stomachCap ∷ Float
stomachCap = 700

-- | A valid DISCRETE definition and a valid BULK one, as an author
--   writes them.
discreteYaml, bulkYaml ∷ String
discreteYaml = probeNamed "probe_ration" (foodWith "        calories: 250\n")
bulkYaml     = probeNamed "probe_sack"
    (containerBlock ⧺ foodWith "        calories_per_kg: 3680\n")

-- | Install both probe definitions through the PRODUCTION YAML →
--   registry mapping, so what feeds is what the decoder produced and
--   not a hand-built 'ItemDef' free to disagree with it.
installProbeDefs ∷ EngineEnv → IO ()
installProbeDefs env = do
    ds ← mapM decodeOrFail [discreteYaml, bulkYaml]
    writeIORef (itemManagerRef env) ∘ ItemManager $ HM.fromList
        [ (iydName d, itemDefFromYaml "probe.yaml" (TextureHandle 0) (TextureHandle 0) d)
        | d ← ds ]
  where
    decodeOrFail src = case decodeDef src of
        Right d  → pure d
        Left err → fail ("probe definition did not decode: " ⧺ err)

-- | One player acolyte carrying @inv@, with a live hunger pool.
resetEater ∷ EngineEnv → Float → [ItemInstance] → IO ()
resetEater env hunger inv =
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs      = HM.singleton "acolyte" (minimalDef "acolyte" "Acolyte")
        , umInstances = HM.singleton eaterUid
            (mkUnit "acolyte" FactionPlayer (10, 10) 100 inv [])
                { uiStats = HM.fromList
                    [ ("carrying_capacity", 100)
                    , ("hunger",            hunger)
                    , ("max_hunger",        stomachCap) ] } }

-- | @unit.feed@ through the REAL registered verb, with the credit
--   formatted in Lua so the assertion never depends on how the debug
--   console happens to render a number.
feed ∷ LuaBackendState → Text → IO Text
feed ls defName = evalDebug ls $ T.concat
    [ "local c = unit.feed(", tshow uid, ", '", defName, "'); "
    , "if c == nil then return 'nil' end; "
    , "return string.format('%.2f', c)" ]
  where
    uid = case eaterUid of UnitId u → u

-- | The eater's inventory (def name and fill, both gameplay-visible)
--   and stomach reading, straight from the live manager ref.
eaterState ∷ EngineEnv → IO ([(Text, Float)], Maybe Float)
eaterState env = do
    um ← readIORef (unitManagerRef env)
    case HM.lookup eaterUid (umInstances um) of
        Nothing → pure ([], Nothing)
        Just u  → pure
            ( [(iiDefName it, iiCurrentFill it) | it ← uiInventory u]
            , HM.lookup "hunger" (uiStats u) )

-- | Float comparison with a tolerance the bulk arithmetic needs: the
--   drawn kg is a quotient, so the residual fill is not exact.
near ∷ Float → Float → Bool
near a b = abs (a - b) < 1e-3

feedSpec ∷ SpecWith EngineEnv
feedSpec = describe "Item.FoodNutrition (live unit.feed)" $ do

    it "the probe definitions reach the registry as the two documented \
       \shapes" $ \env → do
        installProbeDefs env
        ItemManager m ← readIORef (itemManagerRef env)
        (idFood <$> HM.lookup "probe_ration" m)
            `shouldBe` Just (Just (ItemFood 250 0))
        (idFood <$> HM.lookup "probe_sack" m)
            `shouldBe` Just (Just (ItemFood 0 3680))

    it "DISCRETE: the whole item is removed and its full kcal credited" $ \env → do
        installProbeDefs env
        resetEater env 0 [mkItem "probe_ration" 101 0.1]
        ls ← newBareLuaBackend env
        credited ← feed ls "probe_ration"
        credited `shouldBe` "\"250.00\""
        (inv, hunger) ← eaterState env
        inv `shouldBe` []
        hunger `shouldBe` Just 250

    it "DISCRETE: the credit is clamped at max_hunger, and the item is \
       \still consumed whole" $ \env → do
        -- Overflow past a full stomach is wasted BY DESIGN (#1219 is
        -- the AI-side policy that avoids reaching this state, not a
        -- change to it).
        installProbeDefs env
        resetEater env (stomachCap - 13) [mkItem "probe_ration" 101 0.1]
        ls ← newBareLuaBackend env
        credited ← feed ls "probe_ration"
        credited `shouldBe` "\"13.00\""
        (inv, hunger) ← eaterState env
        inv `shouldBe` []
        hunger `shouldBe` Just stomachCap

    it "BULK: only the kg needed to top the stomach up is drawn, and \
       \the sack persists with reduced fill" $ \env → do
        installProbeDefs env
        resetEater env 0 [(mkItem "probe_sack" 201 0.15)
                              { iiCurrentFill = 5.0 }]
        ls ← newBareLuaBackend env
        credited ← feed ls "probe_sack"
        credited `shouldBe` "\"700.00\""
        (inv, hunger) ← eaterState env
        map fst inv `shouldBe` ["probe_sack"]
        -- 700 kcal / 3680 kcal-per-kg = 0.19022 kg drawn.
        map snd inv `shouldSatisfy` \fills →
            case fills of
                [f] → near f (5.0 - stomachCap / 3680)
                _   → False
        hunger `shouldSatisfy` maybe False (near stomachCap)

    it "BULK: a sack eaten dry is removed, crediting only what its \
       \remaining fill was worth" $ \env → do
        installProbeDefs env
        resetEater env 0 [(mkItem "probe_sack" 201 0.15)
                              { iiCurrentFill = 0.1 }]
        ls ← newBareLuaBackend env
        credited ← feed ls "probe_sack"
        -- 0.1 kg is less than the stomach wanted, so the whole sack
        -- goes: 0.1 * 3680 = 368 kcal.
        credited `shouldBe` "\"368.00\""
        (inv, hunger) ← eaterState env
        inv `shouldBe` []
        hunger `shouldSatisfy` maybe False (near 368)

    it "feeding an item the unit does not carry still fails with nil, \
       \consuming nothing" $ \env → do
        installProbeDefs env
        resetEater env 0 [mkItem "probe_ration" 101 0.1]
        ls ← newBareLuaBackend env
        r ← feed ls "probe_sack"
        r `shouldBe` "\"nil\""
        (inv, hunger) ← eaterState env
        map fst inv `shouldBe` ["probe_ration"]
        hunger `shouldBe` Just 0

-- * Shipped-corpus helpers

shippedItemFiles ∷ IO [FilePath]
shippedItemFiles =
    sort ∘ map ("data/items" </>)
        <$> walkFilesWithExtension "data/items" ".yaml"

shippedDefs ∷ IO [ItemYamlDef]
shippedDefs = do
    logger ← silentLogger
    files  ← shippedItemFiles
    concat <$> mapM (loadItemYaml logger) files

silentLogger ∷ IO LoggerState
silentLogger = initLogger defaultLogConfig
    { lcBackend = LogToCallback (\_ → pure ()) }
