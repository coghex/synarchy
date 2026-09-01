-- | #2020 (WML-2), the engine-boundary half: @world.init@'s synchronous
--   admission, the non-enqueuing @world.checkMapImagePlan@ query Create
--   World needs, and a loaded page whose map image is refused failing
--   the whole load transaction before anything is published.
--
--   The headless engine is 'Engine.Core.Types.ModeHeadless', which is
--   GPU-free by design and therefore applies NO device ceiling — that is
--   the contract, not a gap. So the refusals driven through the real
--   verbs here are the mode-independent ones (a plan that cannot be
--   represented at all); the device-ceiling half of the same decision is
--   proved against injected limits in "Test.Headless.World.MapImagePlan",
--   which is the seam a headless suite cannot cross.
module Test.Headless.World.MapImageAdmission (spec) where

import UPrelude
import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.IORef (readIORef, writeIORef)
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HsLua as Lua
import System.Directory (doesFileExist, removePathForcibly)

import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.API.Internal (registerLuaFunction)
import Engine.Scripting.Lua.API.World.Lifecycle
    (worldCheckMapImagePlanFn, worldInitFn)
import Test.Headless.Harness (sendWorldCommand, waitForWorldInit)
import World.Load.Stage (renderStageError, stageSession)
import World.Save.Serialize (loadWorld)
import World.Types

-- | A world size whose atlas cannot be described to Vulkan at all: its
--   packed width overruns a @Word32@ extent. Refused in EVERY boot mode,
--   which is what makes it drivable through the real verbs headlessly.
--   It is also a multiple of 8, so @normalizeWorldSize@ passes it
--   through untouched and the plan is asked about exactly this number.
unrepresentableWorldSize ∷ Int
unrepresentableWorldSize = 200000000

-- | Run a Lua chunk against the REAL @world.init@ and
--   @world.checkMapImagePlan@ closures this engine registers, in a
--   fresh stdlib-only interpreter. Failure is signalled by Lua's own
--   @assert@.
withWorldApi ∷ HasCallStack ⇒ EngineEnv → [Text] → Expectation
withWorldApi env chunkLines = do
    result ← Lua.run @Lua.Exception $ do
        Lua.openlibs
        Lua.newtable
        registerLuaFunction "init" (worldInitFn env)
        registerLuaFunction "checkMapImagePlan" (worldCheckMapImagePlanFn env)
        Lua.setglobal "world"
        status ← Lua.dostring (TE.encodeUtf8 (T.intercalate "\n" chunkLines))
        case status of
            Lua.OK → return Nothing
            _ → do
                err ← Lua.tostring (-1)
                return (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    case result of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)

registeredPages ∷ EngineEnv → IO [WorldPageId]
registeredPages env = map fst . wmWorlds <$> readIORef (worldManagerRef env)

waitForFile ∷ FilePath → IO ()
waitForFile path = go (300 ∷ Int)
  where
    go 0 = expectationFailure $ "save file never appeared: " ⧺ path
    go n = do
        exists ← doesFileExist path
        if exists then pure () else threadDelay 100000 ≫ go (n - 1)

spec ∷ SpecWith EngineEnv
spec = describe "map image plan admission at the engine boundary (#2020)" $ do

    it "world.init refuses an unadmissible world SYNCHRONOUSLY, \
       \registering no page and starting no generation" $ \env → do
        before ← registeredPages env
        withWorldApi env
            [ "local ok, msg = world.init('mapplan_refused', 42, "
              <> tshow unrepresentableWorldSize <> ", 3)"
            , "assert(ok == false, 'world.init must report the refusal')"
            , "assert(type(msg) == 'string', 'a refusal must carry its \
              \diagnostic')"
            , "assert(msg:find('Word32', 1, true), 'diagnostic was: ' \
              \.. tostring(msg))"
            ]
        -- Give the world thread every chance to have picked up a command
        -- that must never have been enqueued in the first place.
        threadDelay 300000
        after ← registeredPages env
        after `shouldMatchList` before
        after `shouldNotContain` [WorldPageId "mapplan_refused"]

    it "world.init keeps its existing queueing behaviour for an \
       \admitted world, and now reports acceptance" $ \env → do
        withWorldApi env
            [ "local ok, msg = world.init('mapplan_admitted', 7, 8, 3)"
            , "assert(ok == true, 'an admitted world.init must report true')"
            , "assert(msg == nil, 'acceptance carries no diagnostic')"
            ]
        _ ← waitForWorldInit env (WorldPageId "mapplan_admitted") 120
        pages ← registeredPages env
        pages `shouldContain` [WorldPageId "mapplan_admitted"]

    it "world.checkMapImagePlan answers the same question without \
       \enqueuing anything — the query Create World runs before it \
       \destroys the player's world" $ \env → do
        before ← registeredPages env
        withWorldApi env
            [ "local ok = world.checkMapImagePlan(64)"
            , "assert(ok == true, 'worldSize 64 must be admissible')"
            , "local bad, msg = world.checkMapImagePlan("
              <> tshow unrepresentableWorldSize <> ")"
            , "assert(bad == false, 'an unrepresentable size must refuse')"
            , "assert(msg:find('Word32', 1, true), 'diagnostic was: ' \
              \.. tostring(msg))"
            ]
        threadDelay 200000
        after ← registeredPages env
        after `shouldMatchList` before

    it "world.checkMapImagePlan normalizes exactly as world.init does, \
       \so it answers about the size that would really be generated" $
        \env → withWorldApi env
            [ "assert(world.checkMapImagePlan(3) == true,"
            , "       'a sub-minimum size normalizes up to 8 and is fine')"
            , "assert(world.checkMapImagePlan(100) == true,"
            , "       'a non-multiple normalizes up to 104 and is fine')"
            ]

    it "a loaded page whose map image is refused fails the load through \
       \the existing transactional path, before any page is staged and \
       \with the live session untouched" $ \env →
        let slot = "mapplan_load_refusal"
            cleanup = do
                removePathForcibly ("saves/" <> slot)
                writeIORef (enginePausedRef env) False
        in (`finally` cleanup) $ do
            removePathForcibly ("saves/" <> slot)
            let pageId = WorldPageId "mapplan_load_w8"
            sendWorldCommand env (WorldInit pageId 11 8 3 Nothing)
            _ ← waitForWorldInit env pageId 120
            sendWorldCommand env
                (WorldSave pageId (T.pack slot)
                           "2026-08-31T00:00:00.000000Z" [] [] Nothing)
            waitForFile ("saves/" <> slot <> "/world.synworld")

            logger ← readIORef (loggerRef env)
            (sd, _, _) ← loadWorld logger (T.pack slot) HS.empty HS.empty
                ⌦ either (\(_, e) → expectationFailure (T.unpack e)
                                  ≫ error "unreachable") pure

            -- Rewrite every saved page's geometry to one whose atlas
            -- cannot exist. Nothing else about the save changes, so the
            -- ONLY reason staging can fail is the map-image plan.
            let bump wps = wps
                    { wpsGenParams = (wpsGenParams wps)
                        { wgpWorldSize = unrepresentableWorldSize } }
                sdBad = sd { sdWorlds = map bump (sdWorlds sd) }

            livePagesBefore ← registeredPages env
            matReg ← readIORef (materialRegistryRef env)
            staged ← stageSession env logger sdBad matReg
            case staged of
                Right _ → expectationFailure
                    "staging accepted a page whose map image cannot exist"
                Left err → do
                    let msg = renderStageError err
                    T.unpack msg `shouldContain` "cannot stage page"
                    T.unpack msg `shouldContain` "Word32"

            -- Staging never touches live refs, and a refusal has to keep
            -- it that way: the current session must be exactly what it
            -- was, with nothing partially replaced.
            livePagesAfter ← registeredPages env
            livePagesAfter `shouldMatchList` livePagesBefore

            -- Deliberately NOT re-staged unmodified as a control: every
            -- live page in this shared engine rides into the save, and
            -- staging them all rebuilds every zoom cache. The refusal
            -- above already names the Word32 bound, which nothing but
            -- the map-image plan produces.
