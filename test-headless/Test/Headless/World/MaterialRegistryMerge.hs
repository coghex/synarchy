-- | #2278: normal @world.init@ MERGES the live material registry over a
--   freshly-loaded @data/materials@ base instead of replacing it
--   wholesale.
--
--   @engine.loadMaterialYaml@ is a public Lua verb that overlays a
--   caller-selected YAML's physical properties into the ONE
--   process-global registry
--   ('Engine.Core.State.materialRegistryRef'). Before this change,
--   @world.init@ rebuilt that registry from @data/materials@ alone and
--   wrote it back wholesale, so every id registered from a file OUTSIDE
--   @data/materials@ silently lost its name, its physical properties,
--   and its known-id membership at the next world creation — leaving
--   its tiles on 'defaultMaterialProps' and getting a save that
--   references the id refused later as an unknown material. The load
--   path has merged live-over-base since #763; this pins the same one
--   collision policy on normal initialization, so a fresh world and a
--   loaded world interpret the same session's registrations
--   identically.
--
--   Driven through the real engine boundary: the registered
--   @engine.loadMaterialYaml@ closure for the registration, real
--   @WorldInit@ commands for the two pages, and a direct read of the
--   live ref every material-dependent subsystem shares. Reverting the
--   merge (restoring the wholesale @writeIORef@) fails the first
--   example here.
module Test.Headless.World.MaterialRegistryMerge (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (finally)
import Data.IORef (newIORef, readIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import System.Directory (getTemporaryDirectory, removePathForcibly)

import Engine.Asset.YamlMaterials (loadPopulatedMaterialRegistry)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (sendWorldCommand, waitForWorldInit)
import World.Material
    ( MaterialId(..), MaterialProps(..), MaterialRegistry
    , getMaterialProps, isKnownMaterial )
import World.Types

-- | An id no file under @data/materials@ defines, so it can only ever
--   reach the registry through @engine.loadMaterialYaml@ — the exact
--   case a wholesale rewrite erased.
customMaterialId ∷ Word8
customMaterialId = 200

-- | A SHIPPED id the fixture deliberately re-registers with distinctive
--   properties, to pin the collision half of the policy: the live
--   registration wins.
overriddenShippedId ∷ Word8
overriddenShippedId = 70   -- lignite, data/materials/carbonaceous.yaml

-- | A shipped id the fixture never touches. It must keep resolving to
--   its @data/materials@ properties, which is what proves the merge
--   overlays the live registrations rather than replacing the base.
controlShippedId ∷ Word8
controlShippedId = 71      -- bituminous_coal, same file

-- | The out-of-tree material YAML. Written to the system temp directory,
--   not under @data/materials@ and not under the resource root at all,
--   so the base pass genuinely cannot see it. The texture paths do not
--   need to exist: 'Engine.Scripting.Lua.API.YamlTextures
--   .resolveTexturePath' substitutes the no-texture fallback and logs,
--   which is orthogonal to the property registration under test.
customMaterialYaml ∷ Text
customMaterialYaml = T.unlines
    [ "materials:"
    , "  - id: " <> tshow customMaterialId
    , "    name: issue2278_custom"
    , "    hardness: 0.125"
    , "    density: 3.75"
    , "    albedo: 0.0625"
    , "    drainage: 0.875"
    , "    pick_speed: 1.75"
    , "    shovel_speed: 0.25"
    , "    dig_bulking: 1.5"
    , "    move_cost: 2.25"
    , "    tile: \"assets/textures/world/issue2278/tile.png\""
    , "    zoom: \"assets/textures/world/issue2278/zoom.png\""
    , "    bg:   \"assets/textures/world/issue2278/bg.png\""
    , "  - id: " <> tshow overriddenShippedId
    , "    name: issue2278_override"
    , "    hardness: 0.9375"
    , "    density: 1.0625"
    , "    albedo: 0.5625"
    , "    drainage: 0.3125"
    , "    pick_speed: 0.375"
    , "    shovel_speed: 1.625"
    , "    move_cost: 3.5"
    , "    tile: \"assets/textures/world/issue2278/tile.png\""
    , "    zoom: \"assets/textures/world/issue2278/zoom.png\""
    , "    bg:   \"assets/textures/world/issue2278/bg.png\""
    ]

-- | A bare Lua interpreter carrying the engine's REAL API tables, so the
--   registration below goes through the same @engine.loadMaterialYaml@
--   closure a mod script would call.
newLuaBackend ∷ EngineEnv → IO LuaBackendState
newLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                              (assetPoolRef env) (nextObjectIdRef env)
                              (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | Run a chunk against that interpreter, failing the example with Lua's
--   own message.
runLua ∷ HasCallStack ⇒ LuaBackendState → [Text] → Expectation
runLua ls chunkLines = do
    status ← Lua.runWith (lbsLuaState ls) $ do
        st ← Lua.dostring (TE.encodeUtf8 (T.intercalate "\n" chunkLines))
        case st of
            Lua.OK → pure Nothing
            _ → do
                err ← Lua.tostring (-1)
                Lua.pop 1
                pure (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    case status of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)

-- | Create one small page and wait it out. Size 8 is the minimum
--   'World.Generate.Config.normalizeWorldGenInputs' passes through, so
--   this is the cheapest real initialization there is.
initSmallPage ∷ EngineEnv → Text → Word64 → IO ()
initSmallPage env name seed = do
    let pageId = WorldPageId name
    sendWorldCommand env (WorldInit pageId seed 8 3 Nothing)
    _ ← waitForWorldInit env pageId 300
    pure ()

liveRegistry ∷ EngineEnv → IO MaterialRegistry
liveRegistry env = readIORef (materialRegistryRef env)

-- | Every field of a registered material, so a comparison cannot pass by
--   checking only the one the implementation happens to preserve
--   ('MaterialProps' has no 'Eq' instance).
propsTuple ∷ MaterialProps
           → (Text, Float, Float, Float, Float, Float, Float
             , Maybe Text, Float, Maybe Text, Bool, Float)
propsTuple p =
    ( mpName p, mpHardness p, mpDensity p, mpAlbedo p, mpDrainage p
    , mpPickSpeed p, mpShovelSpeed p, mpDigSpoil p, mpDigBulking p
    , mpDigChunk p, mpDigGems p, mpMoveCost p )

propsOf ∷ MaterialRegistry → Word8
        → (Text, Float, Float, Float, Float, Float, Float
          , Maybe Text, Float, Maybe Text, Bool, Float)
propsOf reg mid = propsTuple (getMaterialProps reg (MaterialId mid))

spec ∷ SpecWith EngineEnv
spec = describe "live material registrations survive world.init (#2278)" $ do

    it "a custom id registered from a YAML outside data/materials is \
       \still known, with its registered properties, after a LATER \
       \world.init — and a live override of a shipped id keeps winning \
       \while an untouched shipped id keeps its data/materials values" $
        \env → do
            tmpDir ← getTemporaryDirectory
            let yamlPath = tmpDir ⊘ "synarchy_issue2278_materials.yaml"
            (`finally` removePathForcibly yamlPath) $ do
                writeFile yamlPath (T.unpack customMaterialYaml)

                -- A first page, so the second init below happens while a
                -- page is already live (requirement 3).
                initSmallPage env "matmerge_first" 4242

                ls ← newLuaBackend env
                -- The second argument opts in to the parse OUTCOME
                -- (#2203); the count is this verb's first result.
                runLua ls
                    [ "local n, ok = engine.loadMaterialYaml('"
                      <> T.pack yamlPath <> "', true)"
                    , "assert(ok == true, 'the custom material YAML must \
                      \parse')"
                    , "assert(n == 6, 'two definitions queue six textures, \
                      \got ' .. tostring(n))"
                    ]

                before ← liveRegistry env
                isKnownMaterial before (MaterialId customMaterialId)
                    `shouldBe` True
                let customBefore   = propsOf before customMaterialId
                    overrideBefore = propsOf before overriddenShippedId
                    controlBefore  = propsOf before controlShippedId
                fst12 customBefore `shouldBe` "issue2278_custom"
                fst12 overrideBefore `shouldBe` "issue2278_override"

                -- The initialization that used to erase all of it.
                initSmallPage env "matmerge_second" 909

                after ← liveRegistry env

                -- Requirement 1: the custom id survives, unchanged.
                isKnownMaterial after (MaterialId customMaterialId)
                    `shouldBe` True
                propsOf after customMaterialId `shouldBe` customBefore

                -- Requirement 2, collision side: the live registration
                -- wins over the shipped definition of the SAME id.
                propsOf after overriddenShippedId `shouldBe` overrideBefore

                -- Requirement 2, base side: an id the session never
                -- registered still resolves to its shipped properties,
                -- so the merge is an overlay and not a replacement of
                -- the base by the live registry.
                logger ← readIORef (loggerRef env)
                shipped ← loadPopulatedMaterialRegistry logger "data/materials"
                propsOf after controlShippedId
                    `shouldBe` propsOf shipped controlShippedId

                -- Requirement 3: creating the second page did not change
                -- how the already-live first page resolves any of these
                -- ids — it reads this same process-global ref.
                propsOf after controlShippedId `shouldBe` controlBefore

    it "world.init still loads data/materials in full: every shipped id \
       \is registered, and an id the session never registered matches \
       \the shipped definition exactly" $ \env → do
        initSmallPage env "matmerge_shipped" 1717
        logger ← readIORef (loggerRef env)
        shipped ← loadPopulatedMaterialRegistry logger "data/materials"
        live ← liveRegistry env
        let missing = [ mid | mid ← [minBound .. maxBound]
                      , isKnownMaterial shipped (MaterialId mid)
                      , not (isKnownMaterial live (MaterialId mid)) ]
        missing `shouldBe` []
        propsOf live controlShippedId `shouldBe` propsOf shipped controlShippedId

-- | First component of 'propsTuple' — the registered material NAME.
fst12 ∷ (Text, Float, Float, Float, Float, Float, Float
        , Maybe Text, Float, Maybe Text, Bool, Float) → Text
fst12 (n, _, _, _, _, _, _, _, _, _, _, _) = n
