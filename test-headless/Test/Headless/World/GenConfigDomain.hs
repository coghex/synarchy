{-# LANGUAGE ScopedTypeVariables #-}
-- | The floating-point world-generation domain (#2288), at all four of
--   its boundaries.
--
--   Before this, every floating setting was installed unjudged. The Lua
--   verb read it with @Lua.tonumber@, narrowed it and wrote it, returning
--   nothing at all — so a caller could not tell an applied update from a
--   dropped one — and the YAML loader accepted whatever decoded, which
--   includes @1e40@ becoming @Infinity@ in the 'Float' the config stores.
--   Nothing crashed downstream: @+∞@ volcanic activity saturates
--   @min 1.0 (chance * activity)@ so EVERY eruption fires, NaN makes none
--   fire, and both are then persisted.
--
--   Every example here is derived from the shared tables rather than
--   retyped: 'configFloatLeaves' supplies each leaf's field name, domain
--   and accessors, and the field name IS the YAML path (and, minus its
--   @world_gen.@ prefix, the Lua path). A setting added to the domain
--   without a home in these tables therefore gains coverage
--   automatically, and one whose bound moves cannot leave a stale
--   expectation behind.
module Test.Headless.World.GenConfigDomain (pureSpec, spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Yaml
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified HsLua as Lua
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import World.Generate.Config
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Load.Stage (stagedGenParamsWarning)
import World.Page.Types (WorldPageId(..))

-- * The leaf table every example is driven from

-- | The full YAML path of a leaf, split. @world_gen.climate.wind_drag@
--   becomes @["world_gen","climate","wind_drag"]@.
yamlPath ∷ FloatLeaf α → [Text]
yamlPath = T.splitOn "." . flField

-- | The same path as the Lua table sees it: the @world_gen@ prefix is
--   the document's own top-level key and is not part of the table
--   @world.setGenConfig@ takes.
luaPath ∷ FloatLeaf α → [Text]
luaPath = drop 1 . yamlPath

-- | The leaf's own key: the last segment of its path.
leafKey ∷ FloatLeaf α → Text
leafKey leaf = case reverse (yamlPath leaf) of
    (k : _) → k
    []      → flField leaf

-- | Values ON the domain's boundary, which must all be ACCEPTED. Derived
--   from the domain itself, so a moved bound moves its own test.
edgeValues ∷ FloatDomain → [Float]
edgeValues (InRange lo hi)   = [lo, hi, lo + (hi - lo) / 2]
edgeValues FiniteNonNegative = [0, 1, 1000000]
edgeValues AnyFinite         = [-1000000, 0, 1000000]

-- | A FINITE value outside the domain, where the domain has one.
--   'AnyFinite' has none: only a non-finite value can miss it, which the
--   shared non-finite cases already cover.
outOfRangeValue ∷ FloatDomain → Maybe Float
outOfRangeValue (InRange _ hi)  = Just (hi + 1)
outOfRangeValue FiniteNonNegative = Just (-1)
outOfRangeValue AnyFinite         = Nothing

-- | A valid value that is DIFFERENT from the shipped default, so a
--   sibling leaf surviving a neighbour's rejection is observable rather
--   than trivially true. Every one of these is checked against the
--   default it must differ from, below.
distinctValue ∷ FloatDomain → Float
distinctValue (InRange lo hi)   = lo + (hi - lo) * 0.75
distinctValue FiniteNonNegative = 2.5
distinctValue AnyFinite         = -2.5

-- | One value that is outside EVERY domain, in each of the spellings the
--   four boundaries see it in.
--
--   @1e40@ is the reported exploit and the reason the check happens
--   AFTER narrowing: it is an ordinary finite 'Double' that the
--   create-world advanced tab will happily accept, and it is only
--   infinite once it reaches the 'Float' the config stores. It is also
--   the one case whose YAML rejection quotes a DIFFERENT text than the
--   document wrote, because aeson renders the number it decoded rather
--   than the digits — which is still the source value, not the infinity.
data BadValue = BadValue
    { bvLabel    ∷ String
    , bvFloat    ∷ Float        -- ^ as the pure boundary sees it
    , bvLua      ∷ BS.ByteString -- ^ the Lua expression producing it
    , bvYaml     ∷ Text          -- ^ the scalar written into the document
    , bvYamlSeen ∷ Text          -- ^ how the YAML rejection quotes it
    , bvNeedle   ∷ Text          -- ^ substring the Lua diagnostic carries
    }

badValues ∷ [BadValue]
badValues =
    [ BadValue "NaN" (0 / 0) "0/0" ".nan" ".nan" "NaN"
    , BadValue "+infinity" (1 / 0) "math.huge" ".inf" ".inf" "Infinity"
    , BadValue "-infinity" (-1 / 0) "-math.huge" "-.inf" "-.inf" "-Infinity"
    , BadValue "a finite number that overflows Float"
        (narrowWorldGenFloat 1e40) "1e40" "1e40" (tshow (1e40 ∷ Double))
        "Infinity"
    ]

-- * Pure helpers

-- | Set one leaf of the shipped default config.
withLeaf ∷ FloatLeaf WorldGenConfig → Float → WorldGenConfig
withLeaf leaf x = flSet leaf x defaultWorldGenConfig

-- | Every leaf of a config set to its distinct value.
distinctConfig ∷ WorldGenConfig
distinctConfig = foldl' step defaultWorldGenConfig configFloatLeaves
  where
    step cfg leaf = flSet leaf (distinctValue (flDomain leaf)) cfg

-- * YAML helpers

-- | The whole tracked document shape, every float leaf spelled with the
--   scalar text this table supplies. Nested exactly as the file nests:
--   two top-level floats, and the rest under @sun@ \/ @moon@ \/
--   @resources@ \/ @climate@.
renderDocument ∷ [(Text, Text)] → BS.ByteString
renderDocument scalars = TE.encodeUtf8 (T.unlines ("world_gen:" : body))
  where
    body = concatMap section groups
    groups = [ (Nothing, topLevel), (Just "sun", sun), (Just "moon", moon)
             , (Just "resources", res), (Just "climate", clim) ]
    section (Nothing, keys)  = map (leafLine "  ") keys
    section (Just tbl, keys) = ("  " <> tbl <> ":") : map (leafLine "    ") keys
    leafLine indent key = indent <> key <> ": " <> scalarFor key
    scalarFor key = case lookup key scalars of
        Just t  → t
        Nothing → "0"
    topLevel = ["erosion_intensity", "volcanic_activity"]
    sun  = ["tilt_angle", "day_length"]
    moon = ["phase_offset"]
    res  = ["ore_abundance", "iron_abundance", "copper_abundance"]
    clim = [ "coriolis_scale", "wind_drag", "thermal_inertia"
           , "orographic_scale", "evap_scale", "albedo_feedback"
           , "thc_threshold" ]

-- | The document with every leaf at its distinct value, except @field@,
--   which carries @scalar@ verbatim.
documentWith ∷ Text → Text → BS.ByteString
documentWith field scalar = renderDocument
    [ (key leaf, if flField leaf ≡ field
                     then scalar
                     else tshow (distinctValue (flDomain leaf)))
    | leaf ← configFloatLeaves ]
  where
    key = leafKey

-- | Decode + resolve one document exactly as
--   'World.Generate.Config.IO.loadWorldGenConfig' does, minus the log.
resolveDocument
    ∷ HasCallStack ⇒ BS.ByteString
    → IO (WorldGenConfig, [(WorldGenFieldRejection, Text)])
resolveDocument doc = case Yaml.decodeEither' doc of
    Left err → expectationFailure
        ("the document must decode structurally, got: " ⧺ show err)
        ≫ error "unreachable"
    Right raw → pure (resolveWorldGenConfigRaw raw)

-- * Lua helpers

-- | A Lua state carrying the full production API, so the verb under test
--   is the shipped binding.
newBackend ∷ EngineEnv → IO LuaBackendState
newBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Everything the return contract pins about one @world.setGenConfig@
--   call: whether the chunk raised, the ARITY (observed through
--   @table.pack@ rather than inferred), and the first two results.
data ConfigCall = ConfigCall
    { ccRaised ∷ Bool
    , ccArity  ∷ Int
    , ccFirst  ∷ Maybe Bool
    , ccSecond ∷ Maybe Text
    } deriving (Eq, Show)

callSetGenConfig ∷ LuaBackendState → BS.ByteString → IO ConfigCall
callSetGenConfig ls table = Lua.runWith (lbsLuaState ls) $ do
    status ← Lua.dostring
        ("local r = table.pack(world.setGenConfig(" <> table <> "))\n\
         \__gc_n, __gc_a, __gc_b = r.n, r[1], r[2]")
    case status of
        Lua.OK → do
            arity ← globalInt "__gc_n"
            first ← globalBool "__gc_a"
            second ← globalString "__gc_b"
            pure (ConfigCall False (maybe (-1) fromIntegral arity) first second)
        _ → do
            Lua.pop 1
            pure (ConfigCall True (-1) Nothing Nothing)

globalInt ∷ BS.ByteString → Lua.LuaE Lua.Exception (Maybe Lua.Integer)
globalInt name = do
    ty ← Lua.getglobal (Lua.Name name)
    v ← if ty ≡ Lua.TypeNumber then Lua.tointeger (-1) else pure Nothing
    Lua.pop 1
    pure v

globalBool ∷ BS.ByteString → Lua.LuaE Lua.Exception (Maybe Bool)
globalBool name = do
    ty ← Lua.getglobal (Lua.Name name)
    v ← if ty ≡ Lua.TypeBoolean then Just ⊚ Lua.toboolean (-1) else pure Nothing
    Lua.pop 1
    pure v

globalString ∷ BS.ByteString → Lua.LuaE Lua.Exception (Maybe Text)
globalString name = do
    ty ← Lua.getglobal (Lua.Name name)
    v ← if ty ≡ Lua.TypeString
            then fmap TE.decodeUtf8Lenient ⊚ Lua.tostring (-1)
            else pure Nothing
    Lua.pop 1
    pure v

-- | One leaf read back through @world.getGenDefaults()@ — the registered
--   reader, and the only Lua-visible view of the stored configuration.
readGenDefault ∷ LuaBackendState → FloatLeaf α → IO (Maybe Double)
readGenDefault ls leaf = Lua.runWith (lbsLuaState ls) $ do
    _ ← Lua.dostring ("local d = world.getGenDefaults()\nreturn d"
                      <> foldMap (\k → "." <> TE.encodeUtf8 k) (luaPath leaf))
    n ← Lua.tonumber (-1)
    Lua.pop 1
    pure ((\(Lua.Number d) → d) ⊚ n)

-- | The single-leaf table a call under test passes, with @expr@ spliced
--   in as the value: @{erosion_intensity = 1e40}@,
--   @{climate = {wind_drag = -1.0}}@.
leafTable ∷ FloatLeaf α → BS.ByteString → BS.ByteString
leafTable leaf expr = go (map TE.encodeUtf8 (luaPath leaf))
  where
    go [key]      = "{" <> key <> " = " <> expr <> "}"
    go (key : ks) = "{" <> key <> " = " <> go ks <> "}"
    go []         = expr   -- unreachable: every field has a leaf segment

-- | A Float as a Lua numeric literal. 'show' round-trips exactly, so the
--   value the verb narrows back to is the value written here.
luaLiteral ∷ Float → BS.ByteString
luaLiteral = BS.pack . show

-- * Assertions

expectRefusal ∷ HasCallStack ⇒ ConfigCall → [Text] → Expectation
expectRefusal call expected = do
    ccRaised call `shouldBe` False
    ccArity call `shouldBe` 2
    ccFirst call `shouldBe` Just False
    case ccSecond call of
        Nothing → expectationFailure
            ("expected a diagnostic string, got " ⧺ show call)
        Just diagnostic → forM_ expected $ \needle →
            unless (needle `T.isInfixOf` diagnostic) $ expectationFailure
                ("diagnostic " ⧺ show diagnostic ⧺ " does not mention "
                 ⧺ show needle)

expectAcceptance ∷ HasCallStack ⇒ ConfigCall → Expectation
expectAcceptance call = do
    ccRaised call `shouldBe` False
    ccArity call `shouldBe` 1
    ccFirst call `shouldBe` Just True
    ccSecond call `shouldBe` Nothing

-- * The pure spec

pureSpec ∷ Spec
pureSpec = describe "world-generation setting domains" $ do

    describe "the leaf tables" $ do
        it "covers the same fifteen fields on both record shapes, with \
           \the same domains" $ do
            map flField paramsFloatLeaves `shouldBe` map flField configFloatLeaves
            map flDomain paramsFloatLeaves `shouldBe` map flDomain configFloatLeaves
            length configFloatLeaves `shouldBe` 15

        it "agrees value for value on the shipped defaults, so a save's \
           \repair and a config's repair cannot drift apart" $
            forM_ (zip configFloatLeaves paramsFloatLeaves) $ \(c, p) →
                (flField c, flGet p defaultWorldGenParams)
                    `shouldBe` (flField c, flGet c defaultWorldGenConfig)

        it "accepts every shipped default, so a valid configuration is \
           \untouched" $ do
            worldGenConfigRejections defaultWorldGenConfig `shouldBe` []
            worldGenParamsRejections defaultWorldGenParams `shouldBe` []

        it "names each leaf by its full YAML path, under world_gen" $
            forM_ configFloatLeaves $ \leaf →
                take 1 (yamlPath leaf) `shouldBe` ["world_gen"]

    describe "the pure boundary, per field" $
        forM_ configFloatLeaves $ \leaf → describe (T.unpack (flField leaf)) $ do
            let field = flField leaf
                rejectionsFor x = worldGenConfigRejections (withLeaf leaf x)
                fieldsRejected x = map wgrField (rejectionsFor x)

            it "accepts every edge of its domain" $
                forM_ (edgeValues (flDomain leaf)) $ \x →
                    (x, rejectionsFor x) `shouldBe` (x, [])

            forM_ badValues $ \bad →
                it ("refuses " ⧺ bvLabel bad) $
                    fieldsRejected (bvFloat bad) `shouldBe` [field]

            case outOfRangeValue (flDomain leaf) of
                Nothing → pure ()
                Just x  → it "refuses a finite value outside its range" $
                    fieldsRejected x `shouldBe` [field]

            it "uses a value distinct from its own default, so a sibling \
               \check cannot pass by coincidence" $
                distinctValue (flDomain leaf)
                    `shouldNotBe` flGet leaf defaultWorldGenConfig

    describe "narrowing" $ do
        it "judges the stored Float, so a finite source that overflows \
           \it is refused" $ do
            -- 1e40 is an ordinary finite Double. It is only unsafe once
            -- narrowed to the Float the config stores, which is exactly
            -- what forty digits in the advanced tab produce.
            (1e40 ∷ Double) `shouldSatisfy` \d → not (isInfinite d)
            narrowWorldGenFloat 1e40 `shouldSatisfy` isInfinite

        it "preserves NaN and both infinities across the narrowing" $ do
            narrowWorldGenFloat (0 / 0) `shouldSatisfy` isNaN
            narrowWorldGenFloat (1 / 0) `shouldBe` (1 / 0)
            narrowWorldGenFloat (-1 / 0) `shouldBe` (-1 / 0)

    describe "the YAML loader" $
        it "keeps every leaf when the document is entirely in domain" $ do
            (cfg, rejections) ← resolveDocument (documentWith "none" "0")
            rejections `shouldBe` []
            cfg `shouldBe` distinctConfig

    describe "the YAML loader, per field" $
        forM_ configFloatLeaves $ \leaf → describe (T.unpack (flField leaf)) $ do
            let field = flField leaf

            forM_ badValues $ \bad →
                it ("defaults only this leaf, and reports the source \
                    \spelling, for " ⧺ bvLabel bad) $ do
                    (cfg, rejections) ← resolveDocument
                        (documentWith field (bvYaml bad))
                    map (wgrField . fst) rejections `shouldBe` [field]
                    map (wgrValue . fst) rejections `shouldBe` [bvYamlSeen bad]
                    -- Only this leaf defaulted...
                    flGet leaf cfg `shouldBe` flGet leaf defaultWorldGenConfig
                    -- ...and every sibling survived at its distinct value.
                    forM_ configFloatLeaves $ \other →
                        unless (flField other ≡ field) $
                            (flField other, flGet other cfg)
                                `shouldBe` (flField other
                                           , distinctValue (flDomain other))

            case outOfRangeValue (flDomain leaf) of
                Nothing → pure ()
                Just x  → it "defaults only this leaf for a finite value \
                             \outside its range" $ do
                    (cfg, rejections) ← resolveDocument
                        (documentWith field (tshow x))
                    map (wgrField . fst) rejections `shouldBe` [field]
                    flGet leaf cfg `shouldBe` flGet leaf defaultWorldGenConfig
                    forM_ configFloatLeaves $ \other →
                        unless (flField other ≡ field) $
                            (flField other, flGet other cfg)
                                `shouldBe` (flField other
                                           , distinctValue (flDomain other))

    describe "the YAML loader, structurally" $ do
        it "keeps a non-finite spelling field-local rather than failing \
           \the whole document" $ do
            -- Before #2288 a bare Float leaf could not decode ".inf" at
            -- all, so ONE such leaf discarded every other setting in the
            -- file. It must stay a domain rejection, not a parse error.
            (cfg, rejections) ← resolveDocument
                (documentWith fieldThcThreshold ".inf")
            length rejections `shouldBe` 1
            wgcErosionIntensity cfg
                `shouldBe` distinctValue (InRange erosionIntensityMin
                                                  erosionIntensityMax)

        it "still fails structurally on a scalar that is not a number at \
           \all" $
            (Yaml.decodeEither' (documentWith fieldEvapScale "\"banana\"")
                ∷ Either Yaml.ParseException WorldGenConfigRaw)
                `shouldSatisfy` \r → case r of
                    Left _  → True
                    Right _ → False

        it "leaves an absent leaf at its default without reporting it" $ do
            (cfg, rejections) ← resolveDocument
                (TE.encodeUtf8 "world_gen:\n  world_size: 64\n")
            rejections `shouldBe` []
            cfg `shouldBe` defaultWorldGenConfig { wgcWorldSize = 64 }

    describe "the config-side repair" $ do
        -- What 'World.Thread.Command.Init' relies on: whatever it reads
        -- from worldGenConfigRef, the configuration it hands to
        -- buildTimeline is in domain. Neither producer can write an
        -- out-of-domain value any more, so this is the guard that makes
        -- the guarantee independent of them.
        let poisonedConfig = foldl' poison distinctConfig
                                    (zip configFloatLeaves (cycle badValues))
            poison cfg (leaf, bad) = flSet leaf (bvFloat bad) cfg

        it "leaves nothing out of domain, whatever it was handed" $ do
            worldGenConfigRejections poisonedConfig
                `shouldSatisfy` \rs → length rs ≡ length configFloatLeaves
            worldGenConfigRejections (fst (repairWorldGenConfig poisonedConfig))
                `shouldBe` []

        it "reports every leaf it repaired, once each, in table order" $
            map (wgrField . fst) (snd (repairWorldGenConfig poisonedConfig))
                `shouldBe` map flField configFloatLeaves

        it "returns a valid config untouched and reports nothing" $
            repairWorldGenConfig distinctConfig `shouldBe` (distinctConfig, [])

    describe "the save-side repair" $ do
        let distinctParams = foldl' step defaultWorldGenParams paramsFloatLeaves
            step p l = flSet l (distinctValue (flDomain l)) p
            -- One setting a save could plausibly carry after this bug:
            -- an infinity that generation saturated on and then persisted.
            poisoned = distinctParams { wgpVolcanicActivity = 1 / 0 }

        it "defaults only the invalid setting and keeps every sibling" $ do
            let (repaired, rejections) = repairWorldGenParams poisoned
            map (wgrField . fst) rejections `shouldBe` [fieldVolcanicActivity]
            wgpVolcanicActivity repaired
                `shouldBe` wgpVolcanicActivity defaultWorldGenParams
            forM_ paramsFloatLeaves $ \other →
                unless (flField other ≡ fieldVolcanicActivity) $
                    (flField other, flGet other repaired)
                        `shouldBe` (flField other
                                   , distinctValue (flDomain other))

        it "touches nothing else about the params" $ do
            let (repaired, _) = repairWorldGenParams poisoned
            wgpSeed repaired `shouldBe` wgpSeed poisoned
            wgpWorldSize repaired `shouldBe` wgpWorldSize poisoned
            wgpLavaPoolDepth repaired `shouldBe` wgpLavaPoolDepth poisoned
            wgpWaterfallQuantum repaired `shouldBe` wgpWaterfallQuantum poisoned

        it "is idempotent: repairing a repaired value reports nothing" $ do
            let (repaired, _) = repairWorldGenParams poisoned
            snd (repairWorldGenParams repaired) `shouldBe` []

        it "warns with the page identity, the full field and the value" $ do
            let (_, rejections) = repairWorldGenParams poisoned
                page = WorldPageId "saved_page_7"
            case rejections of
                [r] → do
                    let msg = stagedGenParamsWarning page r
                    msg `shouldSatisfy` T.isInfixOf "saved_page_7"
                    msg `shouldSatisfy` T.isInfixOf fieldVolcanicActivity
                    msg `shouldSatisfy` T.isInfixOf "Infinity"
                    msg `shouldSatisfy` T.isInfixOf
                        (tshow (wgpVolcanicActivity defaultWorldGenParams))
                other → expectationFailure
                    ("expected exactly one rejection, got " ⧺ show other)

-- * The Lua spec

spec ∷ SpecWith EngineEnv
spec = describe "world-generation setting domains" $
    describe "world.setGenConfig" $ do

        let fresh env = do
                writeIORef (worldGenConfigRef env) defaultWorldGenConfig
                newBackend env

            refuse env leaf expr expected = do
                ls ← fresh env
                before ← readIORef (worldGenConfigRef env)
                call ← callSetGenConfig ls (leafTable leaf expr)
                expectRefusal call (flField leaf : expected)
                -- Nothing was written: the stored record is identical...
                readIORef (worldGenConfigRef env) `shouldReturn` before
                -- ...and the registered reader agrees.
                readGenDefault ls leaf `shouldReturn`
                    Just (realToFrac (flGet leaf before))

            accept env leaf x = do
                ls ← fresh env
                call ← callSetGenConfig ls (leafTable leaf (luaLiteral x))
                expectAcceptance call
                flGet leaf ⊚ readIORef (worldGenConfigRef env)
                    `shouldReturn` x
                readGenDefault ls leaf `shouldReturn` Just (realToFrac x)

        forM_ configFloatLeaves $ \leaf →
          describe (T.unpack (flField leaf)) $ do
            it "accepts every edge of its domain, and stores it" $ \env →
                forM_ (edgeValues (flDomain leaf)) (accept env leaf)

            forM_ badValues $ \bad →
                it ("refuses " ⧺ bvLabel bad ⧺ ", changing nothing") $ \env →
                    refuse env leaf (bvLua bad) [bvNeedle bad]

            case outOfRangeValue (flDomain leaf) of
                Nothing → pure ()
                Just x  → it "refuses a finite value outside its range, \
                             \changing nothing" $ \env →
                    refuse env leaf (luaLiteral x) [tshow x]

        describe "the update as a whole" $ do
            it "refuses every field when only one is bad, leaving the \
               \accepted siblings unwritten too" $ \env → do
                ls ← fresh env
                before ← readIORef (worldGenConfigRef env)
                call ← callSetGenConfig ls
                    "{erosion_intensity = 1.5, volcanic_activity = 0/0}"
                expectRefusal call [fieldVolcanicActivity]
                -- The good sibling in the SAME table did not land either.
                readIORef (worldGenConfigRef env) `shouldReturn` before

            it "accepts a whole valid table and stores every field" $ \env → do
                ls ← fresh env
                call ← callSetGenConfig ls
                    "{erosion_intensity = 1.5, volcanic_activity = 2.0,\
                    \ climate = {wind_drag = 0.9}}"
                expectAcceptance call
                cfg ← readIORef (worldGenConfigRef env)
                wgcErosionIntensity cfg `shouldBe` 1.5
                wgcVolcanicActivity cfg `shouldBe` 2.0
                clWindDrag (wgcClimate cfg) `shouldBe` 0.9

            it "leaves an absent field at its current value" $ \env → do
                ls ← fresh env
                _ ← callSetGenConfig ls "{erosion_intensity = 1.5}"
                cfg ← callSetGenConfig ls "{volcanic_activity = 2.0}"
                        ≫ readIORef (worldGenConfigRef env)
                wgcErosionIntensity cfg `shouldBe` 1.5
                wgcVolcanicActivity cfg `shouldBe` 2.0

            it "refuses a PRESENT field that is not a number, rather than \
               \silently inheriting the current value" $ \env → do
                -- The old helper folded absent and uncoercible together,
                -- so a typo generated a different world in silence.
                ls ← fresh env
                before ← readIORef (worldGenConfigRef env)
                call ← callSetGenConfig ls "{erosion_intensity = true}"
                expectRefusal call [fieldErosionIntensity, "boolean"]
                readIORef (worldGenConfigRef env) `shouldReturn` before

            it "still accepts a numeric string, which is what the \
               \create-world text boxes produce" $ \env → do
                ls ← fresh env
                call ← callSetGenConfig ls "{erosion_intensity = '1.5'}"
                expectAcceptance call
                cfg ← readIORef (worldGenConfigRef env)
                wgcErosionIntensity cfg `shouldBe` 1.5
