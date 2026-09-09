{-# LANGUAGE OverloadedStrings #-}
-- | The PACK-LOADING half of #2488: a structure pack YAML declares an
--   ordered construction sequence per appearance, and the real Lua
--   modules hand it to the engine beside the static art they already
--   register.
--
--   == Why fixture YAML rather than a hand-built payload
--
--   "Test.Headless.Structure.ConstructionFrames" proves what the
--   catalogue does with a registration. It cannot prove that
--   @scripts\/structures.lua@ and @scripts\/wire.lua@ actually READ a
--   @construction:@ list out of a pack, resolve a variant's override
--   without inheriting the default's, load a texture per frame, or send
--   the appearance's static sprite along with it — and those are the
--   steps a shipped pack will depend on. So this module writes a
--   complete fixture pack (YAML plus REAL images, because requirement
--   6's dimension check measures the files) into the scratch resource
--   root and points the production loaders at it through the one
--   field each already exposes for the purpose.
--
--   The images are real for a second reason. The dimension check
--   deliberately does not consult
--   'Engine.Core.Capability.RenderView.rvTextureSizeRef': that cache is
--   filled by a completed GPU upload, which never happens headless, so a
--   check written against it would pass here having compared nothing.
--
--   == Why the wire pack is exercised once and the piece pack many times
--
--   @scripts\/structures.lua@ resolves its pack by NAME, so each example
--   can write its own fixture and register under its own name in the one
--   shared catalogue. @scripts\/wire.lua@ registers under the literal
--   @wire@ that a wire designation carries, which is deliberate and not
--   worth loosening for a test — so its whole contract, legacy scalar
--   connections and the new table form together, is one example.
--
--   Run just this gate:
--   @cabal test synarchy-test-headless --test-options='--match "structure construction frames"'@
module Test.Headless.Structure.ConstructionPacks (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import qualified Codec.Picture as JP
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V

import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.Log
    ( LogBackend(..), LogConfig(..), LogEntry(..), LogLevel(..)
    , defaultLogConfig, initLogger )
import Engine.Asset.Handle (toInt)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Thread.Dispatch (processLuaMsg)
import Engine.Scripting.Lua.Types (LuaBackendState(..), LuaMsg(..))
import Structure.ArtCatalog
import Structure.Facing (WallEdge(..))
import Structure.Palette (TexPalette(..))
import Structure.Wire (WireShape(..), wireShapeName)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)

-- * Fixture geometry on disk

-- | Where the fixture pack lives, RELATIVE to the scratch resource root
--   the suite runs inside. Relative because a declared frame path is
--   refused if it escapes that root — an absolute temp path is exactly
--   what the escape rule exists to reject — and because the isolation
--   fixture removes anything a spec created under its root.
fixtureDir ∷ FilePath
fixtureDir = "construction_fixture_packs"

fixtureDirText ∷ Text
fixtureDirText = T.pack fixtureDir <> "/"

artDir ∷ Text
artDir = fixtureDirText <> "art/"

-- | The canvas every fixture image but the odd one out reports.
canvasW, canvasH ∷ Int
(canvasW, canvasH) = (96, 64)

spec ∷ Spec
spec = aroundAll setup $ describe "structure construction frames" $ do
    pieceLoadSpec
    wireLoadSpec
    diagnosticSpec
    payloadSpec
    residueSpec
  where
    setup act = withIsolatedResourceRoot $ do
        EngineInitResult env ← initializeEngineHeadlessQuiet
        createDirectoryIfMissing True (fixtureDir </> "art")
        act env

-- * The piece pack, through scripts/structures.lua

pieceLoadSpec ∷ SpecWith EngineEnv
pieceLoadSpec = describe "a pack whose YAML declares construction frames" $ do

    it "registers each appearance's own sequence, and no sequence for an \
       \appearance that declares none" $ \env → do
        ls ← loadPiecePack env "cf_basic" defaultPack
        _  ← pure ls
        cat ← readIORef (structureArtCatalogRef env)
        framePathsOf cat "cf_basic" (appearance Nothing ApFloor)
            `shouldBe` Just (framesFor "floor" 3)
        framePathsOf cat "cf_basic" (appearance Nothing (ApWall WallNE))
            `shouldBe` Just (framesFor "wall_ne" 2)
        framePathsOf cat "cf_basic" (appearance Nothing (ApWall WallSW))
            `shouldBe` Just (framesFor "wall_sw" 2)
        -- `ceiling` and `post` declare nothing in this fixture.
        framePathsOf cat "cf_basic" (appearance Nothing ApCeiling)
            `shouldBe` Nothing
        framePathsOf cat "cf_basic" (appearance Nothing ApPost)
            `shouldBe` Nothing

    it "keeps a VARIANT's override to itself and never inherits the \
       \default's" $ \env → do
        _ ← loadPiecePack env "cf_variant" variantPack
        cat ← readIORef (structureArtCatalogRef env)
        -- The variant declares a floor sequence of its own…
        framePathsOf cat "cf_variant" (appearance (Just "damaged") ApFloor)
            `shouldBe` Just (framesFor "damaged_floor" 2)
        -- …and nothing for the wall it overrides the SPRITE of, even
        -- though the default wall does declare one.
        framePathsOf cat "cf_variant" (appearance (Just "damaged")
                                          (ApWall WallNE))
            `shouldBe` Nothing
        framePathsOf cat "cf_variant" (appearance Nothing (ApWall WallNE))
            `shouldBe` Just (framesFor "wall_ne" 2)

    it "sends the appearance's OWN static sprite along with the sequence, \
       \so the handoff frame is measured against the right canvas" $ \env → do
        _ ← loadPiecePack env "cf_static" defaultPack
        cat ← readIORef (structureArtCatalogRef env)
        fmap (aaPath ∘ csStatic)
             (resolveConstructionSequence cat "cf_static"
                  (appearance Nothing (ApWall WallSE)))
            `shouldBe` Just (artDir <> "wall_se.png")

    it "loads a real texture handle for every frame" $ \env → do
        _ ← loadPiecePack env "cf_handles" defaultPack
        cat ← readIORef (structureArtCatalogRef env)
        let handles = maybe [] (map aaHandle ∘ V.toList ∘ csFrames)
                (resolveConstructionSequence cat "cf_handles"
                     (appearance Nothing ApFloor))
        length handles `shouldBe` 3
        handles `shouldSatisfy` all ((> 0) ∘ toInt)

    it "registers the pack whole when it declares NO construction at all — \
       \today's shipped shape" $ \env → do
        _ ← loadPiecePack env "cf_none" barePack
        cat ← readIORef (structureArtCatalogRef env)
        packArtResolves cat "cf_none" `shouldBe` True
        forM_ [ApFloor, ApCeiling, ApPost, ApWall WallNE] $ \slot →
            framePathsOf cat "cf_none" (appearance Nothing slot)
                `shouldBe` Nothing

    it "refuses the whole pack when a sequence's last frame is a different \
       \size from its static sprite" $ \env → do
        (ls, entries) ← withCapturedLog env $
            loadPiecePack env "cf_dims" oddSizePack
        _ ← pure ls
        cat ← readIORef (structureArtCatalogRef env)
        packArtResolves cat "cf_dims" `shouldBe` False
        warningsOf entries `shouldSatisfy`
            any (namesAll ["cf_dims", "96x64", "48x32"])

    it "refuses the whole pack when its wall directions run to different \
       \lengths" $ \env → do
        (_, entries) ← withCapturedLog env $
            loadPiecePack env "cf_lengths" unevenWallPack
        cat ← readIORef (structureArtCatalogRef env)
        packArtResolves cat "cf_lengths" `shouldBe` False
        warningsOf entries `shouldSatisfy`
            any (namesAll ["cf_lengths", "different lengths"])

    it "refuses the whole pack when a declared list is empty" $ \env → do
        (_, entries) ← withCapturedLog env $
            loadPiecePack env "cf_empty" emptyListPack
        cat ← readIORef (structureArtCatalogRef env)
        packArtResolves cat "cf_empty" `shouldBe` False
        warningsOf entries `shouldSatisfy` any (namesAll ["cf_empty", "empty"])

    it "makes the pack resolve nothing once a declared FRAME terminally \
       \fails to load, naming the frame" $ \env → do
        ls ← loadPiecePack env "cf_failframe" defaultPack
        cat0 ← readIORef (structureArtCatalogRef env)
        packArtResolves cat0 "cf_failframe" `shouldBe` True
        (_, entries) ← withCapturedLog env $
            assetFailed env ls (artDir <> "floor_build_2.png")
        cat1 ← readIORef (structureArtCatalogRef env)
        packArtResolves cat1 "cf_failframe" `shouldBe` False
        warningsOf entries `shouldSatisfy`
            any (namesAll ["cf_failframe", "floor construction frame 3"])

-- * The wire pack, through scripts/wire.lua

wireLoadSpec ∷ SpecWith EngineEnv
wireLoadSpec = describe "the wire pack" $
    it "accepts a LEGACY scalar connection beside the new table form, and \
       \gives each shape only its own frames" $ \env → do
        writeWirePack
        ls ← newBareLuaBackend env
        runLua ls $ T.concat
            [ "local w = require('scripts.wire'); "
            , "w.packPath = '", fixtureDirText, "wire_fx.yaml'; "
            , "w.registerPackArt();" ]
        cat ← readIORef (structureArtCatalogRef env)
        packArtResolves cat "wire" `shouldBe` True
        -- The table form carries a sequence…
        framePathsOf cat "wire" (appearance Nothing (ApWire WireCross))
            `shouldBe` Just (framesFor "wire_cross" 4)
        framePathsOf cat "wire" (appearance Nothing (ApWire WireStraightNS))
            `shouldBe` Just (framesFor "wire_straight_ns" 4)
        -- …the legacy scalar form still registers its static art and
        -- declares no sequence, which is exactly requirement 8.
        framePathsOf cat "wire" (appearance Nothing (ApWire WireIsolated))
            `shouldBe` Nothing
        resolveUnplacedArt cat "wire" "wire" Nothing
            (defaultPieceArtContext { pacWireShape = WireIsolated })
            `shouldSatisfy` isJust
        -- …and a table-form connection resolves its OWN static sprite.
        fmap (aaPath ∘ paTexture)
             (resolveUnplacedArt cat "wire" "wire" Nothing
                  (defaultPieceArtContext { pacWireShape = WireCross }))
            `shouldBe` Just (artDir <> "wire_cross.png")

-- * Requirement 8's diagnostic

diagnosticSpec ∷ SpecWith EngineEnv
diagnosticSpec = describe "the missing-declaration report" $ do

    it "fires once per (pack, appearance) on a fresh registration, and \
       \never for an appearance that declares frames" $ \env → do
        (_, entries) ← withCapturedLog env $
            loadPiecePack env "cf_report" defaultPack
        let lines' = [ leMessage e | e ← entries
                     , "declares no construction frames" `T.isInfixOf`
                         leMessage e
                     , "cf_report" `T.isInfixOf` leMessage e ]
        -- ceiling and post declare nothing; floor and the four walls do.
        length lines' `shouldBe` 2
        lines' `shouldSatisfy` any (T.isInfixOf "'ceiling'")
        lines' `shouldSatisfy` any (T.isInfixOf "'post'")
        lines' `shouldNotSatisfy` any (T.isInfixOf "'floor'")
        lines' `shouldNotSatisfy` any (T.isInfixOf "wall ne")

    it "says nothing at all on an idempotent repeat" $ \env → do
        ls ← loadPiecePack env "cf_repeat" defaultPack
        (r, entries) ← withCapturedLog env $
            evalDebug ls (registerAgain "cf_repeat")
        r `shouldBe` "true"
        [ leMessage e | e ← entries
                      , "declares no construction frames"
                          `T.isInfixOf` leMessage e ] `shouldBe` []

-- * The registration verb itself

payloadSpec ∷ SpecWith EngineEnv
payloadSpec = describe "the construction payload" $ do

    it "refuses a frame path that escapes the resource root" $ \env → do
        ls ← newBareLuaBackend env
        forM_ [ "../secret.png", "/etc/passwd.png", "art/../../out.png" ]
            $ \bad → do
                before ← readIORef (structureArtCatalogRef env)
                (r, entries) ← withCapturedLog env $ evalDebug ls
                    (floorPayload "cf_escape" [bad])
                r `shouldBe` "false"
                readIORef (structureArtCatalogRef env) ⌦ \after →
                    sacPacks after `shouldBe` sacPacks before
                warningsOf entries `shouldSatisfy`
                    any (namesAll ["cf_escape", "escapes"])

    it "refuses a SPARSE construction array rather than dropping what is \
       \past the gap" $ \env → do
        ls ← newBareLuaBackend env
        before ← readIORef (structureArtCatalogRef env)
        (r, entries) ← withCapturedLog env $ evalDebug ls $ T.concat
            [ "return tostring(structure.registerPackArt{ pack='cf_sparse', "
            , "kinds={{kind='floor', buildable=true}}, "
            , "art={{kind='floor', texture='a.png', texHandle=41, "
            , "facemap='f.png', faceHandle=42}}, "
            , "construction={[1]={kind='floor', texture='a.png', texHandle=41, "
            , "frames={{texture='b.png', texHandle=43}}}, "
            , "[3]={kind='ceiling', texture='c.png', texHandle=41, "
            , "frames={{texture='d.png', texHandle=44}}}} })" ]
        r `shouldBe` "false"
        readIORef (structureArtCatalogRef env) ⌦ \after →
            sacPacks after `shouldBe` sacPacks before
        warningsOf entries `shouldSatisfy`
            any (namesAll ["cf_sparse", "sparse"])

    it "refuses a SPARSE frame list too" $ \env → do
        ls ← newBareLuaBackend env
        (r, entries) ← withCapturedLog env $ evalDebug ls $ T.concat
            [ "return tostring(structure.registerPackArt{ pack='cf_sparsef', "
            , "kinds={{kind='floor', buildable=true}}, "
            , "art={{kind='floor', texture='a.png', texHandle=41, "
            , "facemap='f.png', faceHandle=42}}, "
            , "construction={{kind='floor', texture='a.png', texHandle=41, "
            , "frames={[1]={texture='b.png', texHandle=43}, "
            , "[3]={texture='d.png', texHandle=44}}}} })" ]
        r `shouldBe` "false"
        warningsOf entries `shouldSatisfy`
            any (namesAll ["cf_sparsef", "sparse"])

    it "refuses a frames list that is not an array at all" $ \env → do
        ls ← newBareLuaBackend env
        (r, entries) ← withCapturedLog env $ evalDebug ls $ T.concat
            [ "return tostring(structure.registerPackArt{ pack='cf_notarr', "
            , "kinds={{kind='floor', buildable=true}}, "
            , "art={{kind='floor', texture='a.png', texHandle=41, "
            , "facemap='f.png', faceHandle=42}}, "
            , "construction={{kind='floor', texture='a.png', texHandle=41, "
            , "frames='b.png'}} })" ]
        r `shouldBe` "false"
        warningsOf entries `shouldSatisfy`
            any (namesAll ["cf_notarr", "not an array"])

    it "refuses a frame entry with no texture or no handle" $ \env → do
        ls ← newBareLuaBackend env
        forM_ [ "{texture='b.png'}", "{texHandle=43}"
              , "{texture='b.png', texHandle='43'}" ] $ \frame → do
            (r, entries) ← withCapturedLog env $ evalDebug ls $ T.concat
                [ "return tostring(structure.registerPackArt{ pack='cf_frame', "
                , "kinds={{kind='floor', buildable=true}}, "
                , "art={{kind='floor', texture='a.png', texHandle=41, "
                , "facemap='f.png', faceHandle=42}}, "
                , "construction={{kind='floor', texture='a.png', texHandle=41, "
                , "frames={", frame, "}}} })" ]
            r `shouldBe` "false"
            warningsOf entries `shouldSatisfy` any (T.isInfixOf "cf_frame")

    it "refuses a wall construction entry that names a cap code — a \
       \sequence is per EDGE, not per cap" $ \env → do
        ls ← newBareLuaBackend env
        (r, entries) ← withCapturedLog env $ evalDebug ls $ T.concat
            [ "return tostring(structure.registerPackArt{ pack='cf_caps', "
            , "kinds={{kind='floor', buildable=true}}, "
            , "art={{kind='floor', texture='a.png', texHandle=41, "
            , "facemap='f.png', faceHandle=42}}, "
            , "construction={{kind='floor', caps='00', texture='a.png', "
            , "texHandle=41, frames={{texture='b.png', texHandle=43}}}} })" ]
        r `shouldBe` "false"
        warningsOf entries `shouldSatisfy` any (T.isInfixOf "cf_caps")

-- * No palette residue

residueSpec ∷ SpecWith EngineEnv
residueSpec = describe "registering construction frames" $
    it "interns nothing into the saved texture palette" $ \env → do
        beforePal ← readIORef (texPaletteRef env)
        beforeHandles ← readIORef (texPaletteHandlesRef env)
        _ ← loadPiecePack env "cf_residue" defaultPack
        cat ← readIORef (structureArtCatalogRef env)
        -- Resolve every declared sequence, which is what a render frame
        -- does — the read must not intern either.
        forM_ [ appearance Nothing s
              | s ← [ApFloor, ApWall WallNE, ApWall WallNW, ApWall WallSE
                    , ApWall WallSW] ] $ \ak →
            resolveConstructionFrame cat "cf_residue" ak 0.5
                `shouldSatisfy` isJust
        afterPal ← readIORef (texPaletteRef env)
        afterHandles ← readIORef (texPaletteHandlesRef env)
        tpPathToId afterPal `shouldBe` tpPathToId beforePal
        tpIdToPath afterPal `shouldBe` tpIdToPath beforePal
        tpNextId afterPal `shouldBe` tpNextId beforePal
        HM.keys afterHandles `shouldMatchList` HM.keys beforeHandles

-- * Fixture pack authoring

-- | One fixture pack's construction declarations: which pieces and which
--   wall edges declare a sequence, and what the @damaged@ variant adds.
--
--   A single description drives the YAML writer, the image writer and
--   the assertions, so a fixture cannot half-rename itself or declare a
--   frame it never wrote to disk.
data PackSpec = PackSpec
    { psPieces  ∷ [(Text, Maybe [Text])]   -- ^ floor \/ ceiling \/ post
    , psWalls   ∷ [(Text, Maybe [Text])]   -- ^ ne \/ nw \/ se \/ sw
    , psVariant ∷ Bool                     -- ^ emit the @damaged@ block
    }

-- | Nothing declared anywhere — today's shipped shape.
barePack ∷ PackSpec
barePack = PackSpec
    { psPieces = [ (k, Nothing) | k ← ["floor", "ceiling", "post"] ]
    , psWalls  = [ (e, Nothing) | e ← wallEdges ]
    , psVariant = False }

wallEdges ∷ [Text]
wallEdges = ["ne", "nw", "se", "sw"]

-- | A floor sequence of three and a two-frame sequence on every wall
--   direction. Ceiling and post declare nothing, which is what gives
--   requirement 8's report something to say.
defaultPack ∷ PackSpec
defaultPack = barePack
    { psPieces = [ ("floor", Just (framesFor "floor" 3))
                 , ("ceiling", Nothing), ("post", Nothing) ]
    , psWalls  = [ (e, Just (framesFor ("wall_" <> e) 2)) | e ← wallEdges ] }

-- | The same, plus a @damaged@ variant overriding the floor AND the ne
--   wall sprite but declaring a sequence only for the floor.
variantPack ∷ PackSpec
variantPack = defaultPack { psVariant = True }

-- | A floor whose LAST frame is a different canvas from its sprite.
oddSizePack ∷ PackSpec
oddSizePack = defaultPack
    { psPieces = [ ("floor", Just (framesFor "floor" 2
                                     ⧺ [artDir <> "odd_size.png"]))
                 , ("ceiling", Nothing), ("post", Nothing) ] }

-- | Wall directions that disagree about how long the build takes.
unevenWallPack ∷ PackSpec
unevenWallPack = defaultPack
    { psWalls = ("ne", Just (framesFor "wall_ne" 3))
                  : [ (e, Just (framesFor ("wall_" <> e) 2))
                    | e ← drop 1 wallEdges ] }

-- | An AUTHORED empty list, which is a typo'd pack and not an absent
--   declaration — the loader passes it straight through so the engine
--   can refuse it by name.
emptyListPack ∷ PackSpec
emptyListPack = defaultPack
    { psPieces = [ ("floor", Just []), ("ceiling", Nothing)
                 , ("post", Nothing) ] }

-- | Write the fixture pack for @name@ and drive the REAL loader at it.
loadPiecePack ∷ EngineEnv → Text → PackSpec → IO LuaBackendState
loadPiecePack env name ps = do
    writeFixtureImages
    TIO.writeFile (fixtureDir </> T.unpack name ⧺ ".yaml") (piecePackYaml name ps)
    ls ← newBareLuaBackend env
    runLua ls (registerScript name)
    pure ls

registerScript ∷ Text → Text
registerScript name = T.concat
    [ "local s = require('scripts.structures'); "
    , "s.packDir = '", fixtureDirText, "'; "
    , "s.pack = '", name, "'; "
    , "s.registerPackArt();" ]

-- | The same registration a second time, from a FRESH Lua state, so the
--   idempotence being asserted is the catalogue's and not the module's
--   own @registeredPack@ guard.
registerAgain ∷ Text → Text
registerAgain name = registerScript name <> " return tostring(true)"

-- | The fixture pack. Deliberately the same schema as
--   @data\/structure_packs\/dungeon_1.yaml@ — the loader is the
--   production one and reads exactly that.
piecePackYaml ∷ Text → PackSpec → Text
piecePackYaml name ps = T.concat $
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
           ⧺ constructionLines 4 frames)
      | (k, frames) ← psPieces ps ]
    ⧺ [ "walls:\n" ]
    ⧺ [ T.unlines
          ([ "  " <> e <> ":"
           , "    texture: " <> artDir <> "wall_" <> e <> ".png"
           , "    facemaps:" ]
           ⧺ [ "      \"" <> c <> "\": " <> artDir <> "face.png"
             | c ← ["00", "01", "10", "11"] ]
           ⧺ constructionLines 4 frames)
      | (e, frames) ← psWalls ps ]
    ⧺ [ variantBlock | psVariant ps ]

-- | A @construction:@ list at @indent@ spaces, or nothing at all. An
--   authored EMPTY list emits the key with no items, because "declared
--   empty" and "not declared" are different states the engine treats
--   differently.
constructionLines ∷ Int → Maybe [Text] → [Text]
constructionLines indent frames = case frames of
    Nothing  → []
    -- A bare `construction:` with no items decodes as NULL, which is
    -- indistinguishable from an absent key; an authored empty list is
    -- spelled `[]`, and that is the state the engine refuses by name.
    Just []  → [pad <> "construction: []"]
    Just ps  → (pad <> "construction:")
                 : [ pad <> "  - " <> p | p ← ps ]
  where pad = T.replicate indent " "

variantBlock ∷ Text
variantBlock = T.unlines $
    [ "variants:"
    , "  damaged:"
    , "    pieces:"
    , "      floor:"
    , "        texture: " <> artDir <> "damaged_floor.png" ]
    ⧺ constructionLines 8 (Just (framesFor "damaged_floor" 2))
    ⧺ [ "    walls:"
      , "      ne:"
      , "        texture: " <> artDir <> "damaged_wall_ne.png" ]

-- | The wire fixture: two connections in the NEW table form with
--   sequences, and fourteen in the LEGACY scalar form.
writeWirePack ∷ IO ()
writeWirePack = do
    writeFixtureImages
    TIO.writeFile (fixtureDir </> "wire_fx.yaml") $ T.unlines $
        [ "name: wire_fx"
        , "build:"
        , "  wire: { build_work: 1.5, materials: { wiring: 1 } }"
        , "facemap: " <> artDir <> "face.png"
        , "connections:" ]
        ⧺ concat
            [ if shape `elem` tableFormShapes
                then [ "  " <> shape <> ":"
                     , "    texture: " <> artDir <> "wire_" <> shape <> ".png"
                     , "    construction:" ]
                     ⧺ [ "      - " <> p
                       | p ← framesFor ("wire_" <> shape) 4 ]
                else [ "  " <> shape <> ": " <> artDir <> "wire_" <> shape
                         <> ".png" ]
            | shape ← map wireShapeName [minBound .. maxBound] ]

tableFormShapes ∷ [Text]
tableFormShapes = [wireShapeName WireCross, wireShapeName WireStraightNS]

-- | The frame paths a sequence of @n@ declares for @stem@.
framesFor ∷ Text → Int → [Text]
framesFor stem n = [ artDir <> stem <> "_build_" <> tshow i <> ".png"
                   | i ← [0 .. n - 1] ]

-- | Every image the fixture packs name, at the pack canvas — plus the
--   one deliberately odd size the dimension example needs.
writeFixtureImages ∷ IO ()
writeFixtureImages = do
    createDirectoryIfMissing True (fixtureDir </> "art")
    forM_ statics $ \stem → writeImageAt (artDir <> stem <> ".png")
                                        canvasW canvasH
    forM_ (concat [ framesFor stem n | (stem, n) ← sequences ]) $ \path →
        writeImageAt path canvasW canvasH
    writeImageAt (artDir <> "odd_size.png") 48 32
  where
    statics = [ "floor", "ceiling", "post", "face", "damaged_floor"
              , "damaged_wall_ne" ]
              ⧺ [ "wall_" <> e | e ← wallEdges ]
              ⧺ [ "wire_" <> wireShapeName s | s ← [minBound .. maxBound] ]
    sequences = [ ("floor", 3), ("damaged_floor", 2), ("wall_ne", 3) ]
              ⧺ [ ("wall_" <> e, 2) | e ← wallEdges ]
              ⧺ [ ("wire_cross", 4), ("wire_straight_ns", 4) ]

writeImageAt ∷ Text → Int → Int → IO ()
writeImageAt path w h =
    JP.writePng (T.unpack path)
        (JP.generateImage pixel w h ∷ JP.Image JP.PixelRGBA8)
  where
    pixel x y = JP.PixelRGBA8 (fromIntegral (x `mod` 256))
                              (fromIntegral (y `mod` 256)) 128 255

-- * Assertions and plumbing

appearance ∷ Maybe Text → AppearanceSlot → AppearanceKey
appearance = AppearanceKey

framePathsOf ∷ StructureArtCatalog → Text → AppearanceKey → Maybe [Text]
framePathsOf cat pack ak =
    map aaPath ∘ V.toList ∘ csFrames <$>
        resolveConstructionSequence cat pack ak

-- | A payload declaring one floor sequence with these frame paths.
floorPayload ∷ Text → [Text] → Text
floorPayload pack frames = T.concat
    [ "return tostring(structure.registerPackArt{ pack='", pack, "', "
    , "kinds={{kind='floor', buildable=true}}, "
    , "art={{kind='floor', texture='a.png', texHandle=41, "
    , "facemap='f.png', faceHandle=42}}, "
    , "construction={{kind='floor', texture='a.png', texHandle=41, frames={"
    , T.intercalate ", "
        [ "{texture='" <> p <> "', texHandle=43}" | p ← frames ]
    , "}}} })" ]

namesAll ∷ [Text] → Text → Bool
namesAll needles message = all (`T.isInfixOf` message) needles

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

-- | Drive the production terminal-load-failure path for one path.
assetFailed ∷ EngineEnv → LuaBackendState → Text → IO ()
assetFailed env ls path = do
    stateRef ← newIORef ThreadRunning
    processLuaMsg env ls stateRef
        (LuaAssetFailed "texture" 4242 path "fixture failure")

withCapturedLog ∷ EngineEnv → IO α → IO (α, [LogEntry])
withCapturedLog env act = do
    capturedRef ← newIORef []
    original ← readIORef (loggerRef env)
    capturing ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' capturedRef (e :)) }
    result ← (writeIORef (loggerRef env) capturing ≫ act)
                 `finally` writeIORef (loggerRef env) original
    entries ← readIORef capturedRef
    pure (result, reverse entries)

warningsOf ∷ [LogEntry] → [Text]
warningsOf = map leMessage ∘ filter ((≡ LevelWarn) ∘ leLevel)
