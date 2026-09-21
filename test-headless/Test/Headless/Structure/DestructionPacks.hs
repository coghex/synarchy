{-# LANGUAGE OverloadedStrings #-}
-- | The PACK-LOADING half of #2491: a structure pack YAML declares a
--   teardown clip per appearance, and the real Lua modules hand it — and
--   every authored VARIANT appearance's static sprite — to the engine
--   beside the art they already register.
--
--   == Why fixture YAML rather than a hand-built payload
--
--   "Test.Headless.Structure.DestructionFrames" proves what the
--   catalogue does with a registration. It cannot prove that
--   @scripts\/structures.lua@ and @scripts\/wire.lua@ actually READ a
--   @destruction:@ block out of a pack, forward its @fps@ without
--   defaulting it, resolve a variant's override without inheriting the
--   default's, load a texture per frame, or send each variant
--   appearance's own sprite — and every one of those is a step a
--   shipped pack will depend on. So this module writes complete fixture
--   packs (YAML plus REAL images) into the scratch resource root and
--   points the production loaders at them through the one field each
--   already exposes for the purpose, exactly as
--   "Test.Headless.Structure.ConstructionPacks" does for #2488.
--
--   == What this adds beyond the construction suite
--
--   A teardown declaration has a second half the construction one does
--   not: its own rate. `fps` is the field a loader is most tempted to
--   default, and a defaulted rate is a silently wrong DURATION rather
--   than a visible refusal — so a missing and a non-numeric rate each
--   get an example, at the YAML boundary where the temptation lives.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "structure destruction presentation lifecycle"'@
module Test.Headless.Structure.DestructionPacks (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef, readIORef)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import qualified Codec.Picture as JP
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V

import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Structure.ArtCatalog
import Structure.Facing (WallEdge(..))
import Structure.Wire (WireShape(..), wireShapeName)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)

-- * Fixture geometry on disk

-- | Where the fixture packs live, RELATIVE to the scratch resource root
--   the suite runs inside. Relative because a declared frame path is
--   refused if it escapes that root.
fixtureDir ∷ FilePath
fixtureDir = "destruction_fixture_packs"

fixtureDirText ∷ Text
fixtureDirText = T.pack fixtureDir <> "/"

-- | Each fixture pack gets its OWN art directory.
--
--   The appearance index is keyed by PATH across the whole catalogue,
--   which is the point: one sprite identifies one appearance. Two
--   fixture packs sharing an image would therefore make it ambiguous
--   for both — a true answer about a situation no real pack is in, and
--   a false failure here. Real packs ship their own art; so do these.
artDirFor ∷ Text → Text
artDirFor pack = fixtureDirText <> pack <> "_art/"

canvasW, canvasH ∷ Int
(canvasW, canvasH) = (96, 64)

spec ∷ Spec
spec = aroundAll setup $ describe "structure destruction presentation lifecycle" $ do
    pieceLoadSpec
    variantLoadSpec
    wireLoadSpec
    refusalSpec
  where
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        createDirectoryIfMissing True (fixtureDir </> "art")
        act env

-- * The piece pack, through scripts/structures.lua

pieceLoadSpec ∷ SpecWith EngineEnv
pieceLoadSpec = describe "a pack whose YAML declares destruction frames" $ do

    it "registers each appearance's own clip, at its own authored fps" $
        \env → do
            _ ← loadPiecePack env "df_basic" defaultPack
            cat ← readIORef (structureArtCatalogRef env)
            clipOf cat "df_basic" (appearance Nothing ApFloor)
                `shouldBe` Just (framesFor "df_basic" "floor" 3, 12)
            forM_ wallEdgeNames $ \e →
                clipOf cat "df_basic" (appearance Nothing (wallSlot e))
                    `shouldBe` Just (framesFor "df_basic" ("wall_" <> e) 2, 10)

    it "registers NO clip for an appearance that declares none" $ \env → do
        _ ← loadPiecePack env "df_gaps" defaultPack
        cat ← readIORef (structureArtCatalogRef env)
        clipOf cat "df_gaps" (appearance Nothing ApCeiling) `shouldBe` Nothing
        clipOf cat "df_gaps" (appearance Nothing ApPost)    `shouldBe` Nothing

    it "keeps a pack that declares nothing at all registering exactly as \
       \it always did" $ \env → do
        _ ← loadPiecePack env "df_bare" barePack
        cat ← readIORef (structureArtCatalogRef env)
        packArtResolves cat "df_bare" `shouldBe` True
        forM_ [ApFloor, ApCeiling, ApPost] $ \slot →
            clipOf cat "df_bare" (appearance Nothing slot) `shouldBe` Nothing

    it "resolves a placed piece's sprite back to its appearance" $
        \env → do
            _ ← loadPiecePack env "df_index" defaultPack
            cat ← readIORef (structureArtCatalogRef env)
            appearanceForTexturePath cat (artDirFor "df_index" <> "floor.png")
                `shouldBe` Just ("df_index", appearance Nothing ApFloor)

-- * Variants, through the same loader

variantLoadSpec ∷ SpecWith EngineEnv
variantLoadSpec = describe "a pack with an authored variant" $ do

    it "sends every authored variant appearance's own sprite, including \
       \ones declaring no lifecycle frames" $ \env → do
        _ ← loadPiecePack env "df_variant" variantPack
        cat ← readIORef (structureArtCatalogRef env)
        -- The override that DOES declare a clip…
        clipOf cat "df_variant"
                (appearance (Just "damaged") ApFloor)
            `shouldBe` Just (framesFor "df_variant" "damaged_floor" 2, 6)
        -- …and the one that declares NONE, which is only reachable
        -- because its static art was registered (#2491 review round 1).
        appearanceForTexturePath cat
                (artDirFor "df_variant" <> "damaged_post.png")
            `shouldBe` Just ("df_variant", appearance (Just "damaged") ApPost)
        clipOf cat "df_variant" (appearance (Just "damaged") ApPost)
            `shouldBe` Nothing

    it "never lets a variant inherit the default's clip" $ \env → do
        _ ← loadPiecePack env "df_variant_inherit" variantPack
        cat ← readIORef (structureArtCatalogRef env)
        -- `damaged` overrides the ne wall's sprite but declares no clip
        -- for it; the default ne wall's clip must not answer for it.
        clipOf cat "df_variant_inherit"
                (appearance (Just "damaged") (wallSlot "ne"))
            `shouldBe` Nothing
        clipOf cat "df_variant_inherit" (appearance Nothing (wallSlot "ne"))
            `shouldBe` Just (framesFor "df_variant_inherit" "wall_ne" 2, 10)

    it "marks a sprite the variant INHERITS as ambiguous, so neither \
       \claimant's clip plays" $ \env → do
        -- `damaged` overrides floor, post and the ne wall and inherits
        -- the ceiling, so the ceiling sprite is claimed twice.
        _ ← loadPiecePack env "df_inherited" inheritingPack
        cat ← readIORef (structureArtCatalogRef env)
        let sharedCeiling = artDirFor "df_inherited" <> "ceiling.png"
        appearanceForTexturePath cat sharedCeiling `shouldBe` Nothing
        -- EXACTLY the ceiling: this variant overrides everything else,
        -- so an over-broad ambiguity would show up here as extra rows.
        ambiguousAppearancePaths cat "df_inherited" `shouldBe` [sharedCeiling]
        -- …while every sprite the variant really overrode still resolves.
        appearanceForTexturePath cat
                (artDirFor "df_inherited" <> "damaged_floor.png")
            `shouldBe` Just ("df_inherited", appearance (Just "damaged") ApFloor)
        clipOf cat "df_inherited" (appearance Nothing ApFloor)
            `shouldBe` Just (framesFor "df_inherited" "floor" 3, 12)

-- * The wire pack, through scripts/wire.lua

wireLoadSpec ∷ SpecWith EngineEnv
wireLoadSpec = describe "the wire pack" $ do

    it "registers a clip per declaring connection and none for a legacy \
       \scalar one" $ \env → do
        writeWirePack
        ls ← newBareLuaBackend env
        runLua ls (T.concat
            [ "local w = require('scripts.wire'); "
            , "w.packPath = '", fixtureDirText, "wire_fx.yaml'; "
            , "w.registerPackArt();" ])
        cat ← readIORef (structureArtCatalogRef env)
        clipOf cat "wire" (appearance Nothing (ApWire WireCross))
            `shouldBe` Just (framesFor "wire_fx" "wire_cross" 4, 15)
        clipOf cat "wire" (appearance Nothing (ApWire WireStraightNS))
            `shouldBe` Just (framesFor "wire_fx" "wire_straight_ns" 3, 5)
        -- A connection in the legacy scalar form declares nothing, which
        -- is every shipped connection's state.
        clipOf cat "wire" (appearance Nothing (ApWire WireTeeN))
            `shouldBe` Nothing

-- * Refusals, at the YAML boundary

refusalSpec ∷ SpecWith EngineEnv
refusalSpec = describe "a malformed destruction block" $ do

    it "refuses a clip that states no fps rather than defaulting one" $
        \env → do
            _ ← loadPiecePack env "df_nofps" noFpsPack
            registered env "df_nofps" `shouldReturn` False

    it "refuses a NON-NUMERIC fps rather than coercing it" $ \env → do
        _ ← loadPiecePack env "df_textfps" textFpsPack
        registered env "df_textfps" `shouldReturn` False

    it "refuses a non-positive fps" $ \env → do
        _ ← loadPiecePack env "df_zerofps" zeroFpsPack
        registered env "df_zerofps" `shouldReturn` False

    it "refuses a frame list with a HOLE rather than silently truncating \
       \it" $ \env → do
        -- `engine.loadYaml` decodes a YAML null to a Lua nil, and
        -- `ipairs` would stop at it — handing the engine a dense
        -- one-frame clip indistinguishable from an authored one. The
        -- loader copies the gap through so the engine can refuse it.
        _ ← loadPiecePack env "df_gap" gappedPack
        registered env "df_gap" `shouldReturn` False

    it "refuses an authored EMPTY frame list" $ \env → do
        _ ← loadPiecePack env "df_empty" emptyListPack
        registered env "df_empty" `shouldReturn` False

    it "refuses a `destruction:` that is not a table at all" $ \env → do
        _ ← loadPiecePack env "df_scalar" scalarPack
        registered env "df_scalar" `shouldReturn` False

    it "refuses a `frames:` that is not an array, with a VALID fps beside \
       \it" $ \env → do
        -- Isolated deliberately. A block malformed in BOTH halves is
        -- rejected on the fps, which runs first, so it proves nothing
        -- about the frame-array check — which could then regress to
        -- accepting a non-array unnoticed.
        _ ← loadPiecePack env "df_notarray" notAnArrayPack
        registered env "df_notarray" `shouldReturn` False

    it "refuses a clip that states an fps and NO frames at all" $ \env → do
        _ ← loadPiecePack env "df_noframes" noFramesPack
        registered env "df_noframes" `shouldReturn` False

    it "refuses a wall family that declares SOME directions but not all \
       \four" $ \env → do
        _ ← loadPiecePack env "df_partial" partialWallPack
        registered env "df_partial" `shouldReturn` False

    it "refuses a wall family whose directions run to different lengths" $
        \env → do
            _ ← loadPiecePack env "df_uneven" unevenWallPack
            registered env "df_uneven" `shouldReturn` False

    it "refuses a wall family whose directions disagree about fps" $
        \env → do
            _ ← loadPiecePack env "df_mixedfps" mixedFpsWallPack
            registered env "df_mixedfps" `shouldReturn` False

-- * Pack specs

-- | One appearance's authored teardown block. 'Nothing' anywhere is an
--   OMITTED key, which is a different state from a present-but-wrong
--   one — the distinction every refusal example turns on.
data Clip = Clip
    { clFps       ∷ Maybe Text
    , clFrames    ∷ Maybe [Text]
    , clRawFrames ∷ Maybe Text
      -- ^ Emit @frames:@ as this raw SCALAR instead of a list — not an
      --   array at all. Separate from 'clFrames' because the frame-array
      --   rule has to be reachable with a VALID fps beside it: a block
      --   that is malformed in both halves is rejected on the fps and
      --   proves nothing about the array check.
    }

clip ∷ Text → Text → Int → Text → Clip
clip pack stem n fps =
    Clip (Just fps) (Just (framesFor pack stem n)) Nothing

data PackSpec = PackSpec
    { psPieces  ∷ [(Text, Maybe Clip)]
    , psWalls   ∷ [(Text, Maybe Clip)]
    , psVariant ∷ Maybe VariantSpec
    , psRawFloorDestruction ∷ Maybe Text
      -- ^ Emit the floor's @destruction:@ as this raw scalar rather than
      --   a block — not a table at all, which the loader must forward
      --   unchanged for the engine to refuse.
    }

-- | The @damaged@ block: which appearances it overrides, and the floor
--   clip it declares.
data VariantSpec = VariantSpec
    { vsOverrideCeiling ∷ Bool
      -- ^ When False the variant INHERITS the default ceiling sprite,
      --   which is what makes that sprite ambiguous.
    }

wallEdgeNames ∷ [Text]
wallEdgeNames = ["ne", "nw", "se", "sw"]

wallSlot ∷ Text → AppearanceSlot
wallSlot e = ApWall $ case e of
    "ne" → WallNE; "nw" → WallNW; "se" → WallSE; _ → WallSW

-- | Nothing declared anywhere — today's shipped shape.
barePack ∷ Text → PackSpec
barePack _ = PackSpec
    { psPieces = [ (k, Nothing) | k ← ["floor", "ceiling", "post"] ]
    , psWalls  = [ (e, Nothing) | e ← wallEdgeNames ]
    , psVariant = Nothing
    , psRawFloorDestruction = Nothing }

-- | A floor clip at 12 fps and a COMPLETE wall family at 10, with the
--   ceiling and post declaring nothing.
defaultPack ∷ Text → PackSpec
defaultPack pack = (barePack pack)
    { psPieces = [ ("floor", Just (clip pack "floor" 3 "12"))
                 , ("ceiling", Nothing), ("post", Nothing) ]
    , psWalls  = [ (e, Just (clip pack ("wall_" <> e) 2 "10"))
                 | e ← wallEdgeNames ] }

-- | The same, plus a @damaged@ variant overriding floor, post and the ne
--   wall, declaring a clip only for the floor.
variantPack ∷ Text → PackSpec
variantPack pack = (defaultPack pack) { psVariant = Just (VariantSpec True) }

-- | The same variant, but INHERITING the default ceiling sprite.
inheritingPack ∷ Text → PackSpec
inheritingPack pack = (defaultPack pack) { psVariant = Just (VariantSpec False) }

withFloorClip ∷ Text → Clip → PackSpec
withFloorClip pack c = (defaultPack pack)
    { psPieces = [ ("floor", Just c), ("ceiling", Nothing), ("post", Nothing) ] }

noFpsPack, textFpsPack, zeroFpsPack, gappedPack, emptyListPack ∷ Text → PackSpec
noFpsPack p   = withFloorClip p
                    (Clip Nothing (Just (framesFor p "floor" 3)) Nothing)
textFpsPack p = withFloorClip p
                    (Clip (Just "\"fast\"") (Just (framesFor p "floor" 3))
                          Nothing)
zeroFpsPack p = withFloorClip p
                    (Clip (Just "0") (Just (framesFor p "floor" 3)) Nothing)
gappedPack p  = withFloorClip p (Clip (Just "12")
                  (Just [ artDirFor p <> "floor_break_0.png", yamlNull
                        , artDirFor p <> "floor_break_2.png" ])
                  Nothing)
emptyListPack p = withFloorClip p (Clip (Just "12") (Just []) Nothing)

-- | A VALID fps beside a `frames:` that is not an array at all, and one
--   with no `frames:` key at all. Both isolate the frame-array rule from
--   the fps rule, which is checked first.
notAnArrayPack, noFramesPack ∷ Text → PackSpec
notAnArrayPack p = withFloorClip p (Clip (Just "12") Nothing (Just "false"))
noFramesPack   p = withFloorClip p (Clip (Just "12") Nothing Nothing)

scalarPack ∷ Text → PackSpec
scalarPack p = (defaultPack p) { psRawFloorDestruction = Just "false" }

partialWallPack ∷ Text → PackSpec
partialWallPack p = (defaultPack p)
    { psWalls = [ (e, if e ≡ "sw" then Nothing
                                  else Just (clip p ("wall_" <> e) 2 "10"))
               | e ← wallEdgeNames ] }

unevenWallPack ∷ Text → PackSpec
unevenWallPack p = (defaultPack p)
    { psWalls = [ (e, Just (clip p ("wall_" <> e)
                               (if e ≡ "ne" then 3 else 2) "10"))
               | e ← wallEdgeNames ] }

mixedFpsWallPack ∷ Text → PackSpec
mixedFpsWallPack p = (defaultPack p)
    { psWalls = [ (e, Just (clip p ("wall_" <> e) 2
                               (if e ≡ "nw" then "30" else "10")))
               | e ← wallEdgeNames ] }

-- * Writing the fixture

loadPiecePack ∷ EngineEnv → Text → (Text → PackSpec) → IO LuaBackendState
loadPiecePack env name build = do
    let ps = build name
    writeFixtureImages name
    TIO.writeFile (fixtureDir </> T.unpack name ⧺ ".yaml") (piecePackYaml name ps)
    ls ← newBareLuaBackend env
    runLua ls $ T.concat
        [ "local s = require('scripts.structures'); "
        , "s.packDir = '", fixtureDirText, "'; "
        , "s.pack = '", name, "'; "
        , "s.registerPackArt();" ]
    pure ls

piecePackYaml ∷ Text → PackSpec → Text
piecePackYaml name ps = T.concat $
    let artDir = artDirFor name in
    [ T.unlines
        [ "name: " <> name
        , "build:"
        , "  floor:   { build_work: 3.0, materials: { steel_plate: 1 } }"
        , "  ceiling: { build_work: 3.0, materials: { steel_plate: 1 } }"
        , "  post:    { build_work: 2.0, materials: { wood_log: 1 } }"
        , "  wall:    { build_work: 4.0, materials: { steel_bar: 2 } }"
        , "pieces:" ] ]
    ⧺ [ T.unlines
          ([ "  " <> k <> ":"
           , "    texture: " <> artDir <> k <> ".png"
           , "    facemap: " <> artDir <> "face.png" ]
           ⧺ (case (k, psRawFloorDestruction ps) of
                ("floor", Just raw) → ["    destruction: " <> raw]
                _                   → destructionLines 4 c))
      | (k, c) ← psPieces ps ]
    ⧺ [ "walls:\n" ]
    ⧺ [ T.unlines
          ([ "  " <> e <> ":"
           , "    texture: " <> artDir <> "wall_" <> e <> ".png"
           , "    facemaps:" ]
           ⧺ [ "      \"" <> cc <> "\": " <> artDir <> "face.png"
             | cc ← ["00", "01", "10", "11"] ]
           ⧺ destructionLines 4 c)
      | (e, c) ← psWalls ps ]
    ⧺ maybe [] (variantBlock name) (psVariant ps)

-- | A @destruction:@ block at @indent@ spaces, or nothing at all.
--
--   The two omissions are deliberate and separately reachable: an
--   absent @fps@ and an absent @frames@ are each a malformed block the
--   loader must forward for the engine to refuse, and an authored EMPTY
--   list is @[]@ because a bare key decodes as NULL, which is
--   indistinguishable from an absent one.
destructionLines ∷ Int → Maybe Clip → [Text]
destructionLines indent mc = case mc of
    Nothing → []
    Just c  → (pad <> "destruction:")
                : maybe [] (\f → [pad <> "  fps: " <> f]) (clFps c)
                ⧺ maybe (framesLines (clFrames c))
                        (\raw → [pad <> "  frames: " <> raw])
                        (clRawFrames c)
  where
    pad = T.replicate indent " "
    framesLines Nothing   = []
    framesLines (Just []) = [pad <> "  frames: []"]
    framesLines (Just ps) = (pad <> "  frames:")
                              : [ pad <> "    -" <> item p | p ← ps ]
    item p | p ≡ yamlNull = ""
           | otherwise    = " " <> p

-- | The frame-list entry that decodes to a Lua @nil@.
yamlNull ∷ Text
yamlNull = "\SOHnull"

variantBlock ∷ Text → VariantSpec → [Text]
variantBlock pack vs = [ T.unlines $ let artDir = artDirFor pack in
    [ "variants:"
    , "  damaged:"
    , "    pieces:"
    , "      floor:"
    , "        texture: " <> artDir <> "damaged_floor.png" ]
    ⧺ destructionLines 8 (Just (clip pack "damaged_floor" 2 "6"))
    ⧺ [ "      post:"
      , "        texture: " <> artDir <> "damaged_post.png" ]
    ⧺ (if vsOverrideCeiling vs
         then [ "      ceiling:"
              , "        texture: " <> artDir <> "damaged_ceiling.png" ]
         else [])
    -- All four wall directions overridden, as the shipped
    -- @dungeon_1.damaged@ does, so the only sprite this variant can
    -- share with the default is the ceiling — and whether it does is
    -- 'vsOverrideCeiling''s single knob.
    ⧺ [ "    walls:" ]
    ⧺ concat [ [ "      " <> e <> ":"
               , "        texture: " <> artDir <> "damaged_wall_" <> e
                   <> ".png" ]
             | e ← wallEdgeNames ] ]

-- | The wire fixture: two connections in the table form with clips at
--   DIFFERENT rates and lengths, and the rest in the legacy scalar form.
writeWirePack ∷ IO ()
writeWirePack = do
    writeFixtureImages "wire_fx"
    TIO.writeFile (fixtureDir </> "wire_fx.yaml") $ T.unlines $
        let wireArt = artDirFor "wire_fx" in
        [ "name: wire_fx"
        , "build:"
        , "  wire: { build_work: 1.5, materials: { wiring: 1 } }"
        , "facemap: " <> wireArt <> "face.png"
        , "connections:" ]
        ⧺ concat
            [ case lookup shape wireClips of
                Just (n, fps) →
                    [ "  " <> shape <> ":"
                    , "    texture: " <> wireArt <> "wire_" <> shape <> ".png" ]
                    ⧺ destructionLines 4
                          (Just (clip "wire_fx" ("wire_" <> shape) n fps))
                Nothing →
                    [ "  " <> shape <> ": " <> wireArt <> "wire_" <> shape
                        <> ".png" ]
            | shape ← map wireShapeName [minBound .. maxBound] ]

wireClips ∷ [(Text, (Int, Text))]
wireClips = [ (wireShapeName WireCross, (4, "15"))
            , (wireShapeName WireStraightNS, (3, "5")) ]

framesFor ∷ Text → Text → Int → [Text]
framesFor pack stem n = [ artDirFor pack <> stem <> "_break_" <> tshow i
                            <> ".png"
                        | i ← [0 .. n - 1] ]

writeFixtureImages ∷ Text → IO ()
writeFixtureImages pack = do
    createDirectoryIfMissing True (fixtureDir </> T.unpack pack ⧺ "_art")
    forM_ statics $ \stem → writeImageAt (artDirFor pack <> stem <> ".png")
    forM_ (concat [ framesFor pack stem n | (stem, n) ← sequences ])
          writeImageAt
  where
    statics = [ "floor", "ceiling", "post", "face", "damaged_floor"
              , "damaged_post", "damaged_ceiling" ]
              ⧺ [ "wall_" <> e | e ← wallEdgeNames ]
              ⧺ [ "damaged_wall_" <> e | e ← wallEdgeNames ]
              ⧺ [ "wire_" <> wireShapeName s | s ← [minBound .. maxBound] ]
    sequences = [ ("floor", 3), ("damaged_floor", 2) ]
              ⧺ [ ("wall_" <> e, 3) | e ← wallEdgeNames ]
              ⧺ [ ("wire_cross", 4), ("wire_straight_ns", 3) ]

writeImageAt ∷ Text → IO ()
writeImageAt path =
    JP.writePng (T.unpack path)
        (JP.generateImage pixel canvasW canvasH ∷ JP.Image JP.PixelRGBA8)
  where
    pixel x y = JP.PixelRGBA8 (fromIntegral (x `mod` 256))
                              (fromIntegral (y `mod` 256)) 128 255

-- * Assertions and plumbing

appearance ∷ Maybe Text → AppearanceSlot → AppearanceKey
appearance = AppearanceKey

-- | One appearance's registered clip as (frame paths, fps).
clipOf ∷ StructureArtCatalog → Text → AppearanceKey → Maybe ([Text], Double)
clipOf cat pack ak =
    (\ds → (map aaPath (V.toList (dsFrames ds)), dsFps ds))
        <$> resolveDestructionSequence cat pack ak

-- | Did the pack register at all? A refused registration stores nothing,
--   so its absence from the catalogue IS the refusal.
registered ∷ EngineEnv → Text → IO Bool
registered env pack = do
    cat ← readIORef (structureArtCatalogRef env)
    pure (HM.member pack (sacPacks cat))

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls src = T.dropAround (≡ '"') <$> executeDebugLua (lbsLuaState ls) src

runLua ∷ LuaBackendState → Text → IO ()
runLua ls src = do
    r ← evalDebug ls (src <> " return 'ok'")
    unless (r ≡ "ok") $ fail ("lua statement failed: " <> T.unpack src
                                <> " -> " <> T.unpack r)
