-- | The @--preview structures/\<name\>@ PACK viewer (#2495, BDA-17).
--
--   Three layers, each against the real code:
--
--   * the pre-boot decoder and resolver ('Engine.Preview.StructurePack')
--     over the two SHIPPED packs and over synthetic packs written into an
--     exclusively-owned temporary directory;
--   * the pre-boot refusals, including every manifest shape that must
--     not fall back to the folder browser;
--   * the shipped Lua viewer, fed through the REAL Haskell-to-Lua browse
--     boundary ('Engine.Scripting.Lua.API.Core.getPreviewBrowseFn'), so a
--     marshalling mistake fails here rather than in the manual-only,
--     @needs-gpu@ @tools/preview_probe.py@.
module Test.Headless.Preview.StructurePack (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import Data.List (find, nub)
import System.Directory (createDirectoryIfMissing, createDirectoryLink
                        , createFileLink)
import System.FilePath ((</>))
import System.Posix.Files (setFileMode, nullFileMode, ownerModes)
import Control.Exception (finally)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Types
    ( EngineConfig(..), PreviewBrowse(..), PreviewStructPath(..)
    , PreviewStructFacemap(..), PreviewStructLifecycle(..)
    , PreviewStructAppearance(..), PreviewStructurePack(..) )
import Engine.Preview.Discovery (ItemDirError(..), itemDirErrorMessage)
import Engine.Preview.StructurePack
import Engine.Scripting.Lua.API.Core (getPreviewBrowseFn)
import Test.Headless.Harness (withHeadlessEngineNoWorld)
import Test.Headless.Harness.Isolation (withExclusiveTempDirectory)
import Test.Headless.Preview.LuaHarness (harness, lns)

-- * Fixture plumbing

-- | A pack root and a texture root inside one exclusively-owned
--   temporary directory. @T@ in a fixture document is replaced by the
--   texture root, so every declared path is a real, absolute path under
--   it — the same containment question the shipped @assets/textures@
--   answers.
data Fixture = Fixture { fxPacks ∷ FilePath, fxTex ∷ FilePath }

withFixture ∷ (Fixture → IO ()) → IO ()
withFixture action =
    withExclusiveTempDirectory "synarchy-preview-structure-pack" $ \base → do
        let fx = Fixture (base </> "data" </> "structure_packs")
                         (base </> "assets" </> "textures")
        createDirectoryIfMissing True (fxPacks fx)
        createDirectoryIfMissing True (fxTex fx)
        action fx

-- | Create empty texture files under the fixture's texture root. A
--   preview path verdict never reads pixels, only the file's type.
touch ∷ Fixture → [FilePath] → IO ()
touch fx = mapM_ (\p → writeFile (fxTex fx </> p) "")

writePack ∷ Fixture → String → Text → IO ()
writePack fx name doc =
    writeFile (fxPacks fx </> (name ⧺ ".yaml"))
              (T.unpack (T.replace "T/" (T.pack (fxTex fx) <> "/") doc))

load ∷ Fixture → String
     → IO (Either StructurePackError (Maybe PreviewStructurePack))
load fx = loadStructurePackFrom (fxPacks fx) (fxTex fx)

loadOk ∷ Fixture → String → IO PreviewStructurePack
loadOk fx name = load fx name ⌦ \case
    Right (Just p) → pure p
    other → expectationFailure ("expected a pack, got " ⧺ show other)
              >> error "unreachable"

tpath ∷ Fixture → FilePath → Text
tpath fx rel = T.pack (fxTex fx </> rel)

appearance ∷ PreviewStructurePack → Text → PreviewStructAppearance
appearance p ident = fromMaybe (error ("no appearance " ⧺ T.unpack ident))
    (find ((≡ ident) ∘ psaIdentity) (pspkAppearances p))

staticPath ∷ PreviewStructAppearance → Text
staticPath a = case pslFrames (lifecycle a "static") of
    f : _ → pstPath f
    []    → error "a static lifecycle always has one frame"

lifecycle ∷ PreviewStructAppearance → Text → PreviewStructLifecycle
lifecycle a name = fromMaybe (error ("no lifecycle " ⧺ T.unpack name))
    (find ((≡ name) ∘ pslName) (psaLifecycles a))

-- | The synthetic pack most cases share. Declared deliberately OUT of
--   ascending key order (post before floor, worn's floor before its
--   post) so a decoder that let map ordering win is caught.
fxDoc ∷ Text
fxDoc = T.unlines
    [ "name: fx"
    , "pieces:"
    , "  post:"
    , "    texture: T/post.png"
    , "    facemap: T/postface.png"
    , "    construction: [T/post_c0.png, T/post_c1.png, T/post_c2.png]"
    , "  floor:"
    , "    texture: T/floor.png"
    , "    facemap: T/floorface.png"
    , "    destruction:"
    , "      fps: 12"
    , "      frames: [T/floor_d0.png, T/floor_d1_absent.png]"
    , "walls:"
    , "  sw:"
    , "    texture: T/wall_sw.png"
    , "    facemaps:"
    , "      \"00\": T/w00.png"
    , "      \"10\": T/w10.png"
    , "      \"01\": T/w01.png"
    , "      \"11\": T/w11.png"
    , "    construction: [T/wall_c0.png, T/wall_c1.png]"
    , "variants:"
    , "  worn:"
    , "    pieces:"
    , "      floor:"
    , "        destruction:"
    , "          fps: 6"
    , "          frames: [T/worn_floor_d0_absent.png]"
    , "      post:"
    , "        texture: T/worn_post.png"
    , "    walls:"
    , "      sw:"
    , "        facemaps:"
    , "          \"10\": T/worn_w10.png"
    ]

fxTextures ∷ [FilePath]
fxTextures =
    [ "post.png", "postface.png", "post_c0.png", "post_c1.png", "post_c2.png"
    , "floor.png", "floorface.png", "floor_d0.png"
    , "wall_sw.png", "w00.png", "w10.png", "w01.png", "w11.png"
    , "wall_c0.png", "wall_c1.png", "worn_post.png", "worn_w10.png" ]

withFx ∷ (Fixture → PreviewStructurePack → IO ()) → IO ()
withFx action = withFixture $ \fx → do
    touch fx fxTextures
    writePack fx "fx" fxDoc
    loadOk fx "fx" ⌦ action fx

-- | The malformed-manifest refusal for one document, naming the file.
rejects ∷ Text → Text → Expectation
rejects doc needle = withFixture $ \fx → do
    writePack fx "bad" doc
    load fx "bad" ⌦ \case
        Left err@(PackManifestMalformed path why) → do
            path `shouldBe` (fxPacks fx </> "bad.yaml")
            T.unpack why `shouldContain` T.unpack needle
            T.unpack (structurePackErrorMessage err)
                `shouldContain` (fxPacks fx </> "bad.yaml")
        other → expectationFailure ("expected a malformed-pack refusal, got "
                                    ⧺ show other)

-- * The Haskell-to-Lua boundary

-- | Run a Lua chunk against the REAL @engine.getPreviewBrowse@ marshaller
--   for @pack@: the chunk reads the payload from @hsGetPreviewBrowse()@
--   exactly as the shipped viewer reads it from the engine.
runsWithPack ∷ PreviewStructurePack → Text → Expectation
runsWithPack pack chunk = withHeadlessEngineNoWorld $ \env → do
    let env' = env { engineConfig = (engineConfig env)
                        { ecPreviewBrowse = Just (PreviewStructureAssets pack) } }
    result ← Lua.run $ do
        Lua.openlibs
        Lua.pushHaskellFunction (getPreviewBrowseFn env')
        Lua.setglobal "hsGetPreviewBrowse"
        status ← Lua.dostring (TE.encodeUtf8 chunk)
        case status of
            Lua.OK → pure Nothing
            _ → do
                err ← Lua.tostring (-1)
                pure (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    forM_ result (expectationFailure ∘ T.unpack)

-- | Boot the real preview manager on the marshalled payload.
bootFromEngine ∷ Text
bootFromEngine = lns
    [ harness
    , "local browse = hsGetPreviewBrowse()"
    , "assert(browse and browse.mode == 'structure', 'mode ' .. tostring(browse and browse.mode))"
    , "NOW = 10"
    , "pm = bootPreview(browse, {category='structures', item=browse.structure.name})"
    , "pm.update(0.016)"
    , "function d() return pm.dump() end"
    , "function loaded(path)"
    , "  for _, p in ipairs(d().loadedPaths) do if p == path then return true end end"
    , "  return false"
    , "end"
    , "function lifecycleCell(name)"
    , "  for _, c in ipairs(d().lifecycleRow) do if c.lifecycle == name then return c end end"
    , "end"
    , "function capCell(cap)"
    , "  for _, c in ipairs(d().capRow) do if c.cap == cap then return c end end"
    , "end"
    , "function selectRow(identity)"
    , "  for _, r in ipairs(d().rows) do"
    , "    if r.identity == identity then"
    , "      assetBrowserStub.selectEntry(1, r.key); pm.update(0.016); return"
    , "    end"
    , "  end"
    , "  error('no row ' .. identity)"
    , "end"
    , "function info(i) return UI.getElementInfo(d().infoElements[i]) end"
    ]

spec ∷ Spec
spec = do
    describe "the shipped packs" $ do
        it "dungeon_1: seven default appearances, six damaged overrides, four cap facemaps per wall edge" $ do
            loadStructurePack "dungeon_1" ⌦ \case
                Right (Just p) → do
                    map psaIdentity (pspkAppearances p) `shouldBe`
                        [ "floor", "floor@damaged", "ceiling"
                        , "post", "post@damaged"
                        , "wall:ne", "wall:ne@damaged"
                        , "wall:nw", "wall:nw@damaged"
                        , "wall:se", "wall:se@damaged"
                        , "wall:sw", "wall:sw@damaged" ]
                    let apps = pspkAppearances p
                    length (filter ((≡ "default") ∘ psaVariant) apps) `shouldBe` 7
                    length (filter ((≡ "damaged") ∘ psaVariant) apps) `shouldBe` 6
                    pspkDefault p `shouldBe` "floor"
                    forM_ (filter (isJust ∘ psaEdge) apps) $ \a → do
                        map psfCap (psaFacemaps a) `shouldBe` map Just wallCaps
                        all (isJust ∘ psfFile) (psaFacemaps a) `shouldBe` True
                    -- A damaged wall keeps the intact silhouette, so it
                    -- INHERITS every cap facemap (the pack says so).
                    let dw = appearance p "wall:ne@damaged"
                    all psfInherited (psaFacemaps dw) `shouldBe` True
                    psaTextureInherited dw `shouldBe` False
                    staticPath dw
                        `shouldBe` "assets/textures/buildings/dungeon_1/damaged/wall_ne.png"
                other → expectationFailure (show other)

        it "wire: 16 connections in declared order, sharing one facemap" $ do
            loadStructurePack "wire" ⌦ \case
                Right (Just p) → do
                    let apps = pspkAppearances p
                    map psaConnection apps `shouldBe` map Just
                        [ "isolated", "end_n", "end_e", "end_s", "end_w"
                        , "straight_ns", "straight_ew", "corner_ne", "corner_nw"
                        , "corner_se", "corner_sw", "tee_n", "tee_e", "tee_s"
                        , "tee_w", "cross" ]
                    nub (map psaGroup apps) `shouldBe` ["wire"]
                    nub [ fmap pstPath (psfFile f) | a ← apps, f ← psaFacemaps a ]
                        `shouldBe` [Just "assets/textures/facemap/floorface.png"]
                    pspkDefault p `shouldBe` "wire:isolated"
                other → expectationFailure (show other)

        it "every shipped appearance's static path and facemap is a regular, loadable file" $
            forM_ ["dungeon_1", "wire"] $ \name →
                loadStructurePack name ⌦ \case
                    Right (Just p) → forM_ (pspkAppearances p) $ \a → do
                        let st = lifecycle a "static"
                        map pstMissing (pslFrames st) `shouldBe` [False]
                        [ pstReason f | fm ← psaFacemaps a, Just f ← [psfFile fm]
                                      , pstMissing f ] `shouldBe` []
                    other → expectationFailure (name ⧺ ": " ⧺ show other)

        it "no shipped appearance declares a lifecycle yet, and says so as UNDECLARED" $
            forM_ ["dungeon_1", "wire"] $ \name →
                loadStructurePack name ⌦ \case
                    Right (Just p) → forM_ (pspkAppearances p) $ \a →
                        forM_ ["construction", "destruction"] $ \l → do
                            let lc = lifecycle a l
                            (pslDeclared lc, pslFrames lc, pslFpsSource lc)
                                `shouldBe` (False, [], "undeclared")
                    other → expectationFailure (show other)

    describe "a synthetic pack" $ do
        it "keeps the document's declaration order and groups each owner's variants under it" $ withFx $ \_ p → do
            map psaIdentity (pspkAppearances p) `shouldBe`
                [ "post", "post@worn", "floor", "floor@worn"
                , "wall:sw", "wall:sw@worn" ]
            pspkDefault p `shouldBe` "post"

        it "reads construction and destruction in declared order, with their timing and alpha policy" $ withFx $ \fx p → do
            let post = appearance p "post"
                c = lifecycle post "construction"
            map pstPath (pslFrames c) `shouldBe`
                map (tpath fx) ["post_c0.png", "post_c1.png", "post_c2.png"]
            (pslFps c, pslFpsSource c) `shouldBe` (structurePreviewDefaultFps, "preview-default")
            let d = lifecycle (appearance p "floor") "destruction"
            (pslFps d, pslFpsSource d) `shouldBe` (12, "authored")
            forM_ (pspkAppearances p) $ \a → do
                pslAlphaPolicy (lifecycle a "static") `shouldBe` "facemap-alpha"
                pslAlphaPolicy (lifecycle a "construction") `shouldBe` "frame-alpha"
                pslAlphaPolicy (lifecycle a "destruction") `shouldBe` "frame-alpha"

        it "reports a missing destruction frame IN ITS POSITION, without substitution" $ withFx $ \fx p → do
            let d = lifecycle (appearance p "floor") "destruction"
            map pstPath (pslFrames d) `shouldBe`
                map (tpath fx) ["floor_d0.png", "floor_d1_absent.png"]
            map pstMissing (pslFrames d) `shouldBe` [False, True]
            pstReason (pslFrames d !! 1) `shouldBe` Just "absent"

        it "an appearance with no construction list is undeclared, distinct from missing" $ withFx $ \_ p → do
            let c = lifecycle (appearance p "floor") "construction"
            (pslDeclared c, pslFrames c) `shouldBe` (False, [])

        it "a variant inherits texture and facemaps but NEVER a lifecycle" $ withFx $ \fx p → do
            let worn = appearance p "post@worn"
            staticPath worn
                `shouldBe` tpath fx "worn_post.png"
            psaTextureInherited worn `shouldBe` False
            map (\f → (fmap pstPath (psfFile f), psfInherited f)) (psaFacemaps worn)
                `shouldBe` [(Just (tpath fx "postface.png"), True)]
            -- The default post declares construction; its override does
            -- not, and must not borrow it.
            pslDeclared (lifecycle worn "construction") `shouldBe` False
            let wornFloor = appearance p "floor@worn"
            psaTextureInherited wornFloor `shouldBe` True
            let d = lifecycle wornFloor "destruction"
            (pslDeclared d, map pstMissing (pslFrames d), pslFps d)
                `shouldBe` (True, [True], 6)

        it "a wall override replaces only the caps it names" $ withFx $ \fx p → do
            let w = appearance p "wall:sw@worn"
            map (\f → (psfCap f, fmap pstPath (psfFile f), psfInherited f))
                (psaFacemaps w) `shouldBe`
                [ (Just "00", Just (tpath fx "w00.png"), True)
                , (Just "10", Just (tpath fx "worn_w10.png"), False)
                , (Just "01", Just (tpath fx "w01.png"), True)
                , (Just "11", Just (tpath fx "w11.png"), True) ]
            psaTextureInherited w `shouldBe` True
            pslDeclared (lifecycle w "construction") `shouldBe` False

        it "a variant NAMED default is its own appearance, distinct from the base" $ withFixture $ \fx → do
            touch fx ["a.png", "b.png", "f.png"]
            writePack fx "named" $ T.unlines
                [ "pieces:"
                , "  floor: { texture: T/a.png, facemap: T/f.png }"
                , "variants:"
                , "  default:"
                , "    pieces:"
                , "      floor: { texture: T/b.png, construction: [T/b.png] }" ]
            p ← loadOk fx "named"
            map (\a → (psaIdentity a, psaVariant a, psaOverride a, staticPath a))
                (pspkAppearances p) `shouldBe`
                [ ("floor", "default", False, tpath fx "a.png")
                , ("floor@default", "default", True, tpath fx "b.png") ]
            nub (map psaLabel (pspkAppearances p)) `shouldBe`
                ["floor / default", "floor / variants.default"]
            pslDeclared (lifecycle (appearance p "floor@default") "construction")
                `shouldBe` True
            pslDeclared (lifecycle (appearance p "floor") "construction")
                `shouldBe` False
            runsWithPack p $ lns
                [ bootFromEngine
                , "assert(d().selectedAppearance == 'floor' and d().path == '" <> tpath fx "a.png" <> "')"
                , "selectRow('floor@default')"
                , "local s = d()"
                , "assert(s.selectedAppearance == 'floor@default', tostring(s.selectedAppearance))"
                , "assert(s.path == '" <> tpath fx "b.png" <> "' and s.appearances[2].override == true)"
                , "assert(s.appearances[1].override == false)"
                ]

        it "falls back to the first wall edge, then the first connection, for the default" $ withFixture $ \fx → do
            touch fx ["a.png", "f.png"]
            writePack fx "walls" $ T.unlines
                [ "walls:"
                , "  se: { texture: T/a.png, facemaps: { \"00\": T/f.png } }"
                , "  ne: { texture: T/a.png, facemaps: { \"00\": T/f.png } }" ]
            pspkDefault ⊚ loadOk fx "walls" ⌦ (`shouldBe` "wall:se")
            writePack fx "wires" $ T.unlines
                [ "facemap: T/f.png"
                , "connections:"
                , "  tee_w: T/a.png"
                , "  cross: { texture: T/a.png, construction: [T/a.png] }" ]
            p ← loadOk fx "wires"
            pspkDefault p `shouldBe` "wire:tee_w"
            pslDeclared (lifecycle (appearance p "wire:cross") "construction")
                `shouldBe` True

    describe "declared paths are judged under assets/textures" $ do
        it "a traversal, a symlinked ancestor, a directory and an unsupported file are each missing with their own reason" $ withFixture $ \fx → do
            createDirectoryIfMissing True (fxTex fx </> "real")
            touch fx ["real/a.png", "face.png", "a.jpg"]
            createDirectoryLink (fxTex fx </> "real") (fxTex fx </> "linked")
            createDirectoryIfMissing True (fxTex fx </> "adir.png")
            writePack fx "paths" $ T.unlines
                [ "pieces:"
                , "  floor:"
                , "    texture: T/real/a.png"
                , "    facemap: T/face.png"
                , "    construction:"
                , "      - T/../escape.png"
                , "      - T/linked/a.png"
                , "      - T/adir.png"
                , "      - T/a.jpg"
                , "      - T/real/a.png" ]
            p ← loadOk fx "paths"
            let c = lifecycle (appearance p "floor") "construction"
            map pstReason (pslFrames c) `shouldBe`
                [ Just "outside_root", Just "symlink", Just "directory"
                , Just "unsupported_extension", Nothing ]
            -- The declared spelling is kept for the diagnostic.
            map pstPath (take 1 (pslFrames c)) `shouldBe` [tpath fx "../escape.png"]

        it "attributes a missing facemap separately from the sprite" $ withFixture $ \fx → do
            touch fx ["a.png"]
            writePack fx "face" $ T.unlines
                [ "pieces:", "  floor: { texture: T/a.png, facemap: T/nope.png }" ]
            a ← (`appearance` "floor") ⊚ loadOk fx "face"
            map pstMissing (pslFrames (lifecycle a "static")) `shouldBe` [False]
            map (fmap pstReason ∘ psfFile) (psaFacemaps a) `shouldBe` [Just (Just "absent")]

    describe "pre-boot resolution" $ do
        it "an ABSENT manifest falls back to the folder browser" $ withFixture $ \fx →
            load fx "nosuch" ⌦ (`shouldBe` Right Nothing)

        it "an unsafe name is refused with the folder browser's own wording, before any path is built" $ withFixture $ \fx →
            forM_ ["", ".", "..", "../fx", "a/b", "/etc"] $ \name → do
                r ← load fx name
                r `shouldBe` Left PackNameInvalid
                structurePackErrorMessage PackNameInvalid
                    `shouldBe` itemDirErrorMessage ItemDirEscapesRoot

        it "a symlinked, directory, or dangling manifest never falls back" $ withFixture $ \fx → do
            touch fx ["a.png", "f.png"]
            writePack fx "real" "pieces: { floor: { texture: T/a.png, facemap: T/f.png } }"
            createFileLink (fxPacks fx </> "real.yaml") (fxPacks fx </> "link.yaml")
            createFileLink (fxPacks fx </> "gone.yaml") (fxPacks fx </> "dangling.yaml")
            createDirectoryIfMissing True (fxPacks fx </> "dir.yaml")
            load fx "link" ⌦ (`shouldBe` Left (PackManifestSymlink (fxPacks fx </> "link.yaml")))
            load fx "dangling" ⌦ (`shouldBe` Left (PackManifestSymlink (fxPacks fx </> "dangling.yaml")))
            load fx "dir" ⌦ (`shouldBe` Left (PackManifestNotAFile (fxPacks fx </> "dir.yaml")))

        it "a manifest that cannot be inspected is a diagnostic, not a fallback" $ withFixture $ \fx → do
            writePack fx "locked" "name: locked"
            let restore = setFileMode (fxPacks fx) ownerModes
            -- No permission to search the pack directory: lstat of the
            -- manifest fails with EACCES, which is NOT absence.
            (setFileMode (fxPacks fx) nullFileMode >> load fx "locked")
                `finally` restore ⌦ \case
                Left (PackManifestUnreadable path why) → do
                    path `shouldBe` (fxPacks fx </> "locked.yaml")
                    T.null why `shouldBe` False
                other → expectationFailure ("expected an unreadable-manifest \
                                            \refusal, got " ⧺ show other)

        it "a destruction fps that overflows the playback rate is refused pre-boot" $ do
            rejects "pieces: { floor: { texture: T/a.png, facemap: T/f.png, destruction: { fps: 1.0e100, frames: [T/a.png] } } }\n"
                    "outside the representable playback-rate range"
            rejects "pieces: { floor: { texture: T/a.png, facemap: T/f.png, destruction: { fps: 1.0e-100, frames: [T/a.png] } } }\n"
                    "outside the representable playback-rate range"

        it "a malformed pack is a pre-boot refusal naming the file and the fault" $ do
            rejects "- just\n- a list\n" "not a mapping"
            rejects "pieces: [\n" ""
            rejects "name: empty\n" "declares no appearances"
            rejects "pieces: [1, 2]\n" "pieces: expected a mapping"
            rejects "pieces: { floor: { facemap: T/f.png } }\n" "pieces.floor: missing required `texture`"
            rejects "pieces: { floor: { texture: T/a.png, facemap: T/f.png, construction: T/a.png } }\n"
                    "pieces.floor.construction: expected a list"
            rejects "pieces: { floor: { texture: T/a.png, facemap: T/f.png, construction: [] } }\n"
                    "the frame list is empty"
            rejects "pieces: { floor: { texture: T/a.png, facemap: T/f.png, construction: [T/a.png, ~] } }\n"
                    "construction[1]: expected a texture path"
            rejects "pieces: { floor: { texture: T/a.png, facemap: T/f.png, destruction: { frames: [T/a.png] } } }\n"
                    "missing required `fps`"
            rejects "pieces: { floor: { texture: T/a.png, facemap: T/f.png, destruction: { fps: 0, frames: [T/a.png] } } }\n"
                    "finite positive"
            rejects "walls: { up: { texture: T/a.png, facemaps: {} } }\n" "unknown wall edge `up`"
            rejects "walls: { ne: { texture: T/a.png, facemaps: { \"22\": T/f.png } } }\n" "unknown cap `22`"
            rejects "pieces: { floor: { texture: T/a.png, facemap: T/f.png } }\nvariants: { worn: { pieces: { post: { texture: T/a.png } } } }\n"
                    "variants.worn.pieces.post: overrides a piece"
            rejects "connections: { cross: T/a.png }\n" "missing required `facemap`"

    describe "playback timing" $ do
        it "a lifecycle's frame index at a clock phase follows its fps, replaying past a complete cycle" $ withFx $ \_ p → do
            let d = lifecycle (appearance p "floor") "destruction"   -- 12 fps, 2 frames
                c = lifecycle (appearance p "post") "construction"    -- 8 fps default, 3 frames
                eps = 1e-6
            map (lifecycleFrameIndexAt d) [0, 1/12 + eps, 2/12 + eps, 3/12 + eps]
                `shouldBe` [0, 1, 0, 1]
            map (lifecycleFrameIndexAt c) [0, 0.125 + eps, 0.25 + eps, 0.375 + eps, 1.0 + eps]
                `shouldBe` [0, 1, 2, 0, 2]
            -- The missing second destruction frame is still frame 1: the
            -- count is the DECLARED one.
            length (pslFrames d) `shouldBe` 2

        it "static and undeclared lifecycles never advance" $ withFx $ \_ p → do
            let a = appearance p "floor"
            lifecycleFrameIndexAt (lifecycle a "static") 5 `shouldBe` 0
            lifecycleFrameIndexAt (lifecycle a "construction") 5 `shouldBe` 0

    describe "the shipped Lua viewer, through the real browse boundary" $ do
        it "opens on the default appearance's static sprite and loads only what it shows" $ withFx $ \fx p →
            runsWithPack p $ lns
                [ bootFromEngine
                , "local s = d()"
                , "assert(s.mode == 'structure' and s.state == 'ready', tostring(s.state))"
                , "assert(s.pack == 'fx' and s.appearanceCount == 6)"
                , "assert(s.selectedAppearance == 'post' and s.selectedVariant == 'default')"
                , "assert(s.selectedLifecycle == 'static' and s.selectedCap == nil)"
                , "assert(s.path == '" <> tpath fx "post.png" <> "', tostring(s.path))"
                , "assert(s.facemap == '" <> tpath fx "postface.png" <> "')"
                , "assert(s.alphaPolicy == 'facemap-alpha')"
                , "assert(#s.loadedPaths == 1 and s.loadedPaths[1] == s.path,"
                , "  'only the displayed frame is requested: ' .. table.concat(s.loadedPaths, ','))"
                , "assert(s.totals.missing == 2 and s.totals.undeclared == 8,"
                , "  'missing=' .. s.totals.missing .. ' undeclared=' .. s.totals.undeclared)"
                , "local a1 = s.appearances[1]"
                , "assert(a1.kind == 'post' and a1.variant == 'default' and a1.texture == s.path)"
                , "assert(a1.lifecycles.construction.frameCount == 3)"
                , "assert(a1.lifecycles.destruction.undeclared == true)"
                , "local fl = s.appearances[3].lifecycles.destruction"
                , "assert(fl.frameCount == 2 and fl.missing == 1 and fl.missingFrames[1].index == 1)"
                , "for i, r in ipairs(s.rows) do"
                , "  assert(r.identity == s.appearances[i].identity and r.bounds, 'row ' .. i)"
                , "end"
                , "for _, c in ipairs(s.lifecycleRow) do"
                , "  assert(c.hitHandle and c.bounds and c.bounds.w > 0, 'lifecycle cell bounds')"
                , "end"
                ]

        it "plays a lifecycle on one clock, replays it, and never requests or retains a missing frame" $ withFx $ \fx p →
            runsWithPack p $ lns
                [ bootFromEngine
                , "selectRow('floor')"
                , "local staticPath = d().path"
                , "NOW = 20"
                , "assert(pm.onPreviewLifecycleClick(lifecycleCell('destruction').hitHandle))"
                , "pm.update(0.016)"
                , "local s = d()"
                , "assert(s.selectedLifecycle == 'destruction' and s.frameIndex == 0)"
                , "assert(s.path == '" <> tpath fx "floor_d0.png" <> "' and s.alphaPolicy == 'frame-alpha')"
                , "assert(s.facemap == '" <> tpath fx "floorface.png" <> "', 'a lifecycle frame reuses the appearance facemap')"
                , "assert(s.playback and s.playback.fps == 12)"
                , "NOW = 20 + 1/12 + 1e-4; pm.update(0.016); s = d()"
                , "assert(s.frameIndex == 1 and s.frameCount == 2, 'idx ' .. s.frameIndex)"
                , "assert(s.missing == true and s.missingReason == 'absent')"
                , "local sprite = UI.getElementInfo(s.spriteElement)"
                , "local marker = UI.getElementInfo(s.missingElement)"
                , "assert(sprite.visible == false, 'no earlier sprite may linger')"
                , "assert(marker.visible == true and marker.text == 'X')"
                , "assert(not loaded('" <> tpath fx "floor_d1_absent.png" <> "'),"
                , "  'a missing frame is never requested')"
                , "assert(s.state == 'ready', 'a missing frame is terminal, not loading')"
                , "NOW = 20 + 2/12 + 1e-4; pm.update(0.016); s = d()"
                , "assert(s.frameIndex == 0 and UI.getElementInfo(s.spriteElement).visible,"
                , "  'the clip replays past its end')"
                , "assert(loaded(staticPath))"
                ]

        it "shows an undeclared lifecycle as undeclared, without the previous sprite" $ withFx $ \_ p →
            runsWithPack p $ lns
                [ bootFromEngine
                , "assert(pm.onKeyDown('Right'))"
                , "pm.update(0.016)"
                , "assert(d().selectedLifecycle == 'construction')"
                , "selectRow('post@worn')"
                , "assert(d().selectedLifecycle == 'static', 'an appearance change shows its static sprite')"
                , "assert(pm.onKeyDown('Right')); pm.update(0.016)"
                , "local s = d()"
                , "assert(s.selectedLifecycle == 'construction' and s.undeclared == true)"
                , "assert(s.path == nil and s.frameCount == 0)"
                , "assert(UI.getElementInfo(s.spriteElement).visible == false)"
                , "assert(UI.getElementInfo(s.missingElement).text == 'undeclared')"
                , "assert(lifecycleCell('construction').caption == 'construction -')"
                , "assert(s.state == 'ready')"
                , "assert(pm.onKeyDown('Left') and pm.onKeyDown('Left')); pm.update(0.016)"
                , "assert(d().selectedLifecycle == 'destruction', 'Left wraps from static to the last lifecycle')"
                ]

        it "a wall cap changes only the reported facemap, mid-playback" $ withFx $ \fx p →
            runsWithPack p $ lns
                [ bootFromEngine
                , "selectRow('wall:sw')"
                , "local s = d()"
                , "assert(s.selectedCap == '00' and #s.capRow == 4)"
                , "assert(s.facemap == '" <> tpath fx "w00.png" <> "')"
                , "pm.onUIScroll(s.zoom.surface, 0, 2)"
                , "local zoom = d().zoom.multiplier"
                , "assert(zoom < 1)"
                , "NOW = 30"
                , "assert(pm.onPreviewLifecycleClick(lifecycleCell('construction').hitHandle))"
                , "NOW = 30.2; pm.update(0.016)"
                , "local before = d()"
                , "assert(before.frameIndex == 1 and before.path == '" <> tpath fx "wall_c1.png" <> "')"
                , "assert(pm.onPreviewCapClick(capCell('10').hitHandle))"
                , "pm.update(0.016)"
                , "local after = d()"
                , "assert(after.selectedCap == '10' and after.facemap == '" <> tpath fx "w10.png" <> "')"
                , "assert(after.path == before.path and after.selectedLifecycle == 'construction')"
                , "assert(after.frameIndex == 1 and after.zoom.multiplier == zoom)"
                , "assert(after.alphaPolicy == 'frame-alpha')"
                , "assert(capCell('10').selected and not capCell('00').selected)"
                , "assert(string.find(info(2).text, 'cap 10', 1, true), info(2).text)"
                , "NOW = 30.3; pm.update(0.016)"
                , "assert(d().frameIndex == 0, 'the cap change did not restart the clock')"
                , "-- The cap is remembered across edges; the override lights"
                , "-- with its OWN 10 and inherits the rest."
                , "selectRow('wall:sw@worn')"
                , "s = d()"
                , "assert(s.selectedCap == '10' and s.facemap == '" <> tpath fx "worn_w10.png" <> "')"
                , "assert(s.appearances[6].facemaps[1].inherited == true)"
                , "assert(s.appearances[6].facemaps[2].inherited == false)"
                , "selectRow('post'); assert(d().selectedCap == nil and #d().capRow == 0)"
                ]

        it "a failed frame describes only itself: a lifecycle or frame change recovers, and a late failure of an earlier frame changes nothing" $ withFx $ \_ p →
            runsWithPack p $ lns
                [ bootFromEngine
                , "NOW = 50"
                , "assert(pm.onPreviewLifecycleClick(lifecycleCell('construction').hitHandle))"
                , "pm.update(0.016)"
                , "local s = d()"
                , "local failed, failedPath = s.handle, s.path"
                , "assert(failed and s.frameIndex == 0 and s.state == 'ready')"
                , "pm.onAssetFailed('texture', failed, failedPath, 'boom', true)"
                , "pm.update(0.016)"
                , "assert(d().state == 'empty', 'the displayed frame failed')"
                , "-- The clip advancing to its next frame is a new frame."
                , "NOW = 50.13; pm.update(0.016)"
                , "s = d()"
                , "assert(s.frameIndex == 1 and s.state == 'ready', 'state ' .. s.state)"
                , "local second = s.handle"
                , "-- A lifecycle change recovers too, including to an undeclared one."
                , "pm.onAssetFailed('texture', second, s.path, 'boom', true)"
                , "pm.update(0.016); assert(d().state == 'empty')"
                , "assert(pm.onPreviewLifecycleClick(lifecycleCell('destruction').hitHandle))"
                , "pm.update(0.016)"
                , "assert(d().undeclared and d().state == 'ready', 'state ' .. d().state)"
                , "assert(pm.onPreviewLifecycleClick(lifecycleCell('static').hitHandle))"
                , "pm.update(0.016)"
                , "s = d()"
                , "assert(s.state == 'ready' and s.handle)"
                , "-- A DELAYED failure of a frame no longer on screen leaves the"
                , "-- current selection alone."
                , "pm.onAssetFailed('texture', failed, failedPath, 'late', true)"
                , "pm.update(0.016)"
                , "assert(d().state == 'ready', 'a stale failure blanked the view')"
                ]

        it "a failed displayed frame is terminal, never silently retried, and recovers through a display change" $ withFx $ \fx p →
            runsWithPack p $ lns
                [ bootFromEngine
                , "local s = d()"
                , "assert(s.state == 'ready' and s.handle)"
                , "pm.onAssetFailed('texture', s.handle, s.path, 'boom', true)"
                , "local loads = LOAD_COUNT"
                , "for _ = 1, 5 do pm.update(0.016) end"
                , "s = d()"
                , "assert(LOAD_COUNT == loads, 'the failed frame was re-requested')"
                , "assert(s.state == 'empty' and s.failed == true and s.handle == nil)"
                , "assert(UI.getElementInfo(s.spriteElement).visible == false)"
                , "assert(UI.getElementInfo(s.missingElement).text == 'failed')"
                , "-- The same frame again, through a genuine display change."
                , "assert(pm.onPreviewLifecycleClick(lifecycleCell('construction').hitHandle))"
                , "assert(pm.onPreviewLifecycleClick(lifecycleCell('static').hitHandle))"
                , "pm.update(0.016)"
                , "s = d()"
                , "assert(s.path == '" <> tpath fx "post.png" <> "' and s.failed == false)"
                , "assert(LOAD_COUNT > loads, 'the frame is requested afresh')"
                , "assert(s.state == 'ready' and UI.getElementInfo(s.spriteElement).visible,"
                , "    'and a successful retry reports ready')"
                ]

        it "a frame still uploading is not ready, even right after a ready one" $ withFx $ \_ p →
            runsWithPack p $ lns
                [ bootFromEngine
                , "-- Hold every NEW upload in flight until released."
                , "local pending = {}"
                , "local realLoad, realSize = engine.loadTexture, engine.getTextureSize"
                , "engine.loadTexture = function(path)"
                , "  local h = realLoad(path); if HOLD then pending[h] = true end; return h"
                , "end"
                , "engine.getTextureSize = function(h)"
                , "  if pending[h] then return nil end; return realSize(h)"
                , "end"
                , "NOW = 70"
                , "assert(pm.onPreviewLifecycleClick(lifecycleCell('construction').hitHandle))"
                , "pm.update(0.016)"
                , "assert(d().frameIndex == 0 and d().state == 'ready')"
                , "HOLD = true"
                , "NOW = 70.13; pm.update(0.016)"
                , "local s = d()"
                , "assert(s.frameIndex == 1 and s.state == 'loading',"
                , "  'frame 1 is still uploading, got ' .. s.state)"
                , "pm.update(0.016); assert(d().state == 'loading')"
                , "pending = {}"
                , "pm.update(0.016)"
                , "assert(d().state == 'ready', 'the upload landed')"
                ]

        it "a framebuffer resize preserves appearance, lifecycle, cap, scroll, phase and zoom" $ withFx $ \_ p →
            runsWithPack p $ lns
                [ bootFromEngine
                , "selectRow('wall:sw')"
                , "assert(pm.onPreviewCapClick(capCell('11').hitHandle))"
                , "NOW = 40"
                , "assert(pm.onPreviewLifecycleClick(lifecycleCell('construction').hitHandle))"
                , "pm.onUIScroll(d().zoom.surface, 0, 3)"
                , "assetBrowserStub.setScrollOffset(1, 2)"
                , "NOW = 40.2; pm.update(0.016)"
                , "local before = d()"
                , "assert(before.frameIndex == 1, 'idx ' .. before.frameIndex)"
                , "pm.onFramebufferResize(700, 500)"
                , "pm.update(0.016)"
                , "local after = d()"
                , "assert(after.selectedAppearance == 'wall:sw')"
                , "assert(after.selectedVariant == 'default' and after.selectedCap == '11')"
                , "assert(after.selectedLifecycle == 'construction' and after.frameIndex == 1)"
                , "assert(after.scrollOffset == 2, 'scroll ' .. tostring(after.scrollOffset))"
                , "assert(after.zoom.multiplier == before.zoom.multiplier)"
                , "assert(after.panelBounds.width ~= before.panelBounds.width)"
                , "NOW = 40.3; pm.update(0.016)"
                , "assert(d().frameIndex == 0, 'the phase kept running from the same clock')"
                ]

        it "the shipped dungeon_1 pack crosses the boundary intact" $
            loadStructurePack "dungeon_1" ⌦ \case
                Right (Just p) → runsWithPack p $ lns
                    [ bootFromEngine
                    , "local s = d()"
                    , "assert(s.appearanceCount == 13 and s.selectedAppearance == 'floor')"
                    , "assert(s.path == 'assets/textures/buildings/dungeon_1/floor.png')"
                    , "assert(s.totals.missing == 0 and s.totals.missingFacemaps == 0)"
                    , "assert(s.totals.undeclared == 26)"
                    , "assert(s.appearances[6].edge == 'ne' and #s.appearances[6].facemaps == 4)"
                    ]
                other → expectationFailure (show other)
