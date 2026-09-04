{-# LANGUAGE Strict #-}
-- | Non-finite camera coordinates are refused at both boundaries
--   (#2337).
--
--   @camera.move@ and @camera.setPosition@ used to store @realToFrac@ of
--   whatever Lua number arrived, and staging installed a saved camera
--   verbatim. Neither the main loop nor the save path can recover from
--   the result: @wrapCoord@ subtracts @w * floor (shifted / w)@ and
--   'floor' of a non-finite value is 0, so the coordinate comes back
--   unchanged, while @clampF@ returns its input because neither @x < lo@
--   nor @x > hi@ holds of a NaN. Nothing throws — the view simply
--   renders nothing and chunk selection resolves the origin, on every
--   tick, and the poisoned value then persists into the save.
--
--   So the coordinate is stopped where it enters, and the three groups
--   below are the three places it can:
--
--     * 'pureSpec' — the load-time repair as a pure function, plus the
--       line it warns with. No engine.
--     * 'spec' — the two REGISTERED Lua verbs, against a real engine's
--       camera ref: every rejected shape, in each coordinate slot, with
--       finite controls for both verbs.
--     * 'stagingSpec' — a forged save whose live-camera DTO carries a
--       non-finite value, through the real 'stageSession'.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Camera"'@.
module Test.Headless.Camera.Finite (pureSpec, spec, stagingSpec) where

import UPrelude
import Test.Hspec
import Control.Exception (bracket)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef'
                  , atomicModifyIORef')
import Data.List (find)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Engine.Core.Log
    ( LogEntry(..), LogLevel(..), LogCategory(..), LoggerState
    , LogBackend(..), initLogger, defaultLogConfig, lcBackend )
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Core.Capability.RenderView
    (RenderViewCapability(..), toRenderViewCapability)
import Engine.Graphics.Camera
    (Camera2D(..), CameraFacing(..), defaultCamera, repairCameraView)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Structure.Palette (emptyTexPalette)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Load.Stage
    (stageSession, renderStageError, repairSavedCameraView
    , stagedCameraWarning)
import World.Load.Types (StagedPage(..), StagedSession(..))
import World.Page.Types (WorldPageId(..))
import World.Render.Camera.Types (WorldCamera(..))
import World.Save.Component.Page (blankPageSnapshot)
import World.Save.Snapshot
    (LiveCameraSnapshot(..), PageSnapshot(..), SessionSnapshot(..))
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotToSaveData)
import World.Save.Types (SaveData(..), WorldPageSave(..))
import World.State.Types (WorldState(..))
import World.Time.Types (WorldTime(..))

-----------------------------------------------------------------------
-- * The values a save (or a script) can carry
-----------------------------------------------------------------------

-- | Every non-finite 'Float' a stored camera field can hold, named for
--   the failure message.
badFloats ∷ [(String, Float)]
badFloats = [ ("NaN", 0 / 0), ("Infinity", 1 / 0), ("-Infinity", -1 / 0) ]

-- | The view 'repairCameraView' falls back to.
defaultView ∷ (Float, Float, Float)
defaultView = (dx, dy, camZoom defaultCamera)
  where (dx, dy) = camPosition defaultCamera

-- | A healthy stored view, distinct in all three components and equal
--   to no part of 'defaultView', so "kept" and "defaulted" can never be
--   the same observation.
goodView ∷ (Float, Float, Float)
goodView = (17.5, -42.25, 3.75)

-----------------------------------------------------------------------
-- * The pure repair
-----------------------------------------------------------------------

pureSpec ∷ Spec
pureSpec = describe "Camera.Finite (#2337)" $ do

    describe "the load-time repair" $ do

        it "returns a finite view untouched and reports no repair" $
            repairCameraView goodView `shouldBe` (goodView, False)

        it "defaults the WHOLE view for a bad value in any slot" $
            -- Requirement 2 replaces position AND zoom together: a view
            -- with one poisoned component is not somewhere the player
            -- was looking, so the two survivors would frame a place
            -- they never chose.
            forM_ badFloats $ \(name, bad) → do
                let (gx, gy, gz) = goodView
                forM_ [ ("x",    (bad, gy, gz))
                      , ("y",    (gx, bad, gz))
                      , ("zoom", (gx, gy, bad)) ] $ \(slot, poisoned) →
                    (slot ⧺ "/" ⧺ name, repairCameraView poisoned)
                        `shouldBe` (slot ⧺ "/" ⧺ name, (defaultView, True))

        it "defaults once when every slot is bad" $
            repairCameraView (0 / 0, 1 / 0, -1 / 0)
                `shouldBe` (defaultView, True)

        it "is idempotent: repairing a repaired view reports nothing" $
            repairCameraView (fst (repairCameraView (0 / 0, 0, 0)))
                `shouldBe` (defaultView, False)

        it "hands back a default view that is itself finite" $
            -- The fallback is only a repair if it is in domain. Reading
            -- it off 'defaultCamera' makes that the shipped value's
            -- problem, so pin it.
            snd (repairCameraView defaultView) `shouldBe` False

    describe "the saved-page repair" $ do

        it "moves only the three view fields" $ do
            let poisoned = pageWith (0 / 0, snd3 goodView, thd3 goodView)
                (repaired, didRepair) = repairSavedCameraView poisoned
                (dx, dy, dz) = defaultView
            didRepair `shouldBe` True
            (wpsCameraX repaired, wpsCameraY repaired, wpsCameraZoom repaired)
                `shouldBe` (dx, dy, dz)
            -- The saved facing is a constructor with no out-of-domain
            -- value, and requirement 2 keeps it...
            wpsCameraFacing repaired `shouldBe` savedFacing
            -- ...along with every other field of the page.
            wpsTimeHour repaired `shouldBe` sentinelHour
            wpsPageId repaired `shouldBe` stagedPageId
            wgpSeed (wpsGenParams repaired)
                `shouldBe` wgpSeed (wpsGenParams poisoned)

        it "returns a healthy page reporting no repair, view intact" $ do
            let (repaired, didRepair) = repairSavedCameraView (pageWith goodView)
            didRepair `shouldBe` False
            (wpsCameraX repaired, wpsCameraY repaired, wpsCameraZoom repaired)
                `shouldBe` goodView

    describe "the warning" $
        it "names the page, all three stored values and the default view" $ do
            let poisoned = pageWith (0 / 0, snd3 goodView, thd3 goodView)
                msg = stagedCameraWarning stagedPageId poisoned
                (dx, dy, dz) = defaultView
            forM_ [ unWorldPageId stagedPageId
                  , "NaN", tshow (snd3 goodView), tshow (thd3 goodView)
                  , tshow dx, tshow dy, tshow dz ] $ \needle →
                (needle, msg) `shouldSatisfy` (T.isInfixOf needle ∘ snd)

-----------------------------------------------------------------------
-- * The two Lua verbs
-----------------------------------------------------------------------

-- | A rejected call: the Lua argument list, and the verb it is made on.
--   Both verbs take the same coordinate pair, so one table drives both.
rejectedArguments ∷ [(String, String)]
rejectedArguments =
    [ ("a NaN in slot 1",            "0/0, 1")
    , ("a NaN in slot 2",            "1, 0/0")
    , ("+infinity in slot 1",        "math.huge, 1")
    , ("+infinity in slot 2",        "1, math.huge")
    , ("-infinity in slot 1",        "-math.huge, 1")
    , ("-infinity in slot 2",        "1, -math.huge")
      -- Finite as a Lua 'Double', an infinity only once narrowed to the
      -- 'Float' the camera stores. The check that catches this one is
      -- on the narrowed value, not the argument.
    , ("a slot-1 double that overflows Float", "1e39, 1")
    , ("a slot-2 double that overflows Float", "1, -1e39")
    , ("a missing second argument",  "3")
    , ("no arguments at all",        "")
    , ("a non-numeric slot 1",       "{}, 1")
    , ("a non-numeric slot 2",       "1, {}")
    , ("a boolean slot 1",           "true, 1")
    , ("a nil slot 2",               "1, nil")
    ]

spec ∷ SpecWith EngineEnv
spec = describe "Camera.Finite (#2337)" $ do

    describe "camera.setPosition" $ do

        forM_ rejectedArguments $ \(label, args) →
            it ("refuses " ⧺ label ⧺ ", warns once, and moves nothing") $
                \env → expectRefusal env "camera.setPosition" args

        it "still applies a finite pair, silently" $ \env → do
            ls ← newBackend env
            writeCamera env (7, 9)
            (_, warns) ← runVerb env ls "camera.setPosition" "3, 4"
            warns `shouldBe` []
            camPosition ⊚ readCamera env `shouldReturn` (3, 4)

    describe "camera.move" $ do

        forM_ rejectedArguments $ \(label, args) →
            it ("refuses " ⧺ label ⧺ ", warns once, and moves nothing") $
                \env → expectRefusal env "camera.move" args

        it "refuses two finite deltas whose SUM overflows Float" $ \env → do
            -- Neither argument is out of domain: 3e38 narrows to a
            -- finite 'Float'. Only the candidate position the verb
            -- derives is infinite, which is why the check has to run on
            -- the sum and not just on the arguments.
            writeCamera env (3.0e38, 0)
            expectRefusal env "camera.move" "3.0e38, 0"

        it "still applies a finite pair, silently" $ \env → do
            ls ← newBackend env
            writeCamera env (3, 4)
            (_, warns) ← runVerb env ls "camera.move" "1.5, -2.5"
            warns `shouldBe` []
            camPosition ⊚ readCamera env `shouldReturn` (4.5, 1.5)

        it "still reaches a large finite position it can represent" $
            \env → do
                -- The guard rejects an infinity, not a big number. A sum
                -- that stays inside 'Float' must still land.
                ls ← newBackend env
                writeCamera env (1.0e38, 0)
                (_, warns) ← runVerb env ls "camera.move" "1.0e38, 0"
                warns `shouldBe` []
                camPosition ⊚ readCamera env `shouldReturn` (2.0e38, 0)

-- | The whole refusal contract for one call: exactly one warning naming
--   the verb, and a camera record — position, zoom, facing, velocities,
--   every field — identical to the one the call found.
expectRefusal ∷ HasCallStack ⇒ EngineEnv → Text → String → IO ()
expectRefusal env verb args = do
    ls ← newBackend env
    before ← readCamera env
    reported ← reportedPosition ls
    (reply, warns) ← runVerb env ls verb args
    -- A refusal is a refusal, not a Lua error: the verb returns
    -- normally so a script that mis-computes one pan does not die.
    reply `shouldSatisfy` (not ∘ isLuaError)
    case warns of
        [_] → pure ()
        other → expectationFailure
            ("expected exactly one " ⧺ T.unpack verb ⧺ " warning, got "
             ⧺ show other)
    -- The whole record, which catches a write to any field...
    readCamera env `shouldReturn` before
    -- ...and the position as a SCRIPT sees it, which is the surface the
    -- acceptance criteria name and the only one a caller can check.
    reportedPosition ls `shouldReturn` reported

-- | Run @<verb>(<args>)@ through the REGISTERED binding and collect the
--   warnings it named itself in. The engine's logger is swapped only
--   across the call, since the verb reads @loggerRef@ at the moment it
--   warns.
runVerb ∷ EngineEnv → LuaBackendState → Text → String → IO (Text, [Text])
runVerb env ls verb args = do
    entriesRef ← newIORef []
    captured ← initLogger defaultLogConfig
        { lcBackend = LogToCallback (\e → modifyIORef' entriesRef (e :)) }
    reply ← bracket (swapLogger env captured) (swapLogger env) $ \_ →
        executeDebugLua (lbsLuaState ls)
            (verb <> "(" <> T.pack args <> ")")
    entries ← readIORef entriesRef
    -- Keyed on the verb name rather than "the only warning captured":
    -- the harness may log for its own reasons at any moment, and
    -- "camera.move" is not a prefix of "camera.setPosition", so the two
    -- verbs cannot claim each other's lines.
    pure ( reply
         , [ leMessage e | e ← reverse entries
           , leLevel e ≡ LevelWarn
           , leCategory e ≡ CatLua
           , verb `T.isPrefixOf` leMessage e ] )

swapLogger ∷ EngineEnv → LoggerState → IO LoggerState
swapLogger env logger = do
    previous ← readIORef (loggerRef env)
    writeIORef (loggerRef env) logger
    pure previous

readCamera ∷ EngineEnv → IO Camera2D
readCamera env = readIORef (rvCameraRef (toRenderViewCapability env))

-- | @camera.getPosition()@ as a script reads it, rendered at full
--   precision so no two distinct positions can print the same. A NaN
--   would compare unequal to itself as a number, so the comparison is
--   made on the RENDERED text: an unchanged camera must report an
--   identical string, poisoned or not.
reportedPosition ∷ LuaBackendState → IO Text
reportedPosition ls = executeDebugLua (lbsLuaState ls)
    "local x, y = camera.getPosition() \
    \return string.format('%.17g,%.17g', x, y)"

writeCamera ∷ EngineEnv → (Float, Float) → IO ()
writeCamera env pos =
    atomicModifyIORef' (rvCameraRef (toRenderViewCapability env)) $ \cam →
        (cam { camPosition = pos }, ())

-- | A Lua state carrying the full production API, so the verb under
--   test is the shipped binding. No @scripts/@ are loaded: @camera@ is
--   a registered engine table.
newBackend ∷ EngineEnv → IO LuaBackendState
newBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-----------------------------------------------------------------------
-- * A forged save, through real staging
-----------------------------------------------------------------------

-- | The page every staging example loads. An ARENA page (seed 0 with
--   the empty timeline), so staging takes the flat-chunk rebuild
--   instead of generating a world — this is an example about the
--   repair, not about worldgen.
stagedPageId ∷ WorldPageId
stagedPageId = WorldPageId "camera_finite_staged"

-- | The saved facing requirement 2 must KEEP. Not the default, so
--   "preserved" and "reset" are different observations.
savedFacing ∷ CameraFacing
savedFacing = FaceWest

-- | An unrelated restored value, written right beside the camera in
--   'World.Load.Stage.stagePage'. It must survive the repair.
sentinelHour ∷ Int
sentinelHour = 13

arenaParams ∷ WorldGenParams
arenaParams = defaultWorldGenParams { wgpSeed = 0 }

-- | A one-page save whose LIVE camera carries @view@, built the way a
--   DECODED save is: 'blankPageSnapshot' is the construction every
--   @world-pages@ version's decoder converges on, and
--   'snapshotToSaveData' is the adapter staging consumes. The live
--   camera owns this page, so its x/y reach @wpsCameraX@/@wpsCameraY@
--   and its zoom/facing reach the page unconditionally.
saveWith ∷ (Float, Float, Float) → SaveData
saveWith (x, y, zoom) = snapshotToSaveData
    (SaveRequestMeta "camera_finite_slot" "2026-09-04T00:00:00.000000Z" False)
    SessionSnapshot
        { snapGameTime       = 0
        , snapTexPalette     = emptyTexPalette
        , snapNextItemId     = 1
        , snapNextBuildingId = 1
        , snapNextUnitId     = 1
        , snapActivePage     = stagedPageId
        , snapVisiblePages   = [stagedPageId]
        , snapLiveCamera     = LiveCameraSnapshot
            { lcsOwnerPage = Just stagedPageId
            , lcsX = x, lcsY = y, lcsZoom = zoom, lcsFacing = savedFacing }
        , snapPages          = HM.singleton stagedPageId
            (blankPageSnapshot stagedPageId arenaParams)
                { pgsTimeHour = sentinelHour }
        }

-- | The decoded page such a save carries, for the pure examples above.
pageWith ∷ (Float, Float, Float) → WorldPageSave
pageWith view = case sdWorlds (saveWith view) of
    (p : _) → p
    []      → error "pageWith: the forged save carries no page"

stagingSpec ∷ SpecWith EngineEnv
stagingSpec = describe "Camera.Finite (#2337)" $
  describe "a persisted save, through staging" $ do

    forM_ badFloats $ \(name, bad) →
        forM_ [ ("x",    (bad, 22.5, 3.5))
              , ("y",    (22.5, bad, 3.5))
              , ("zoom", (22.5, 11.5, bad)) ] $ \(slot, view) →
            it ("loads the default view when the saved " ⧺ slot
                ⧺ " is " ⧺ name ⧺ ", keeping everything else") $
              \env → do
                (staged, page, entries) ← stageWith env view
                let (dx, dy, dz) = defaultView

                -- The whole fallback, as one unit. The live camera...
                camPosition (ssCamera staged) `shouldBe` (dx, dy)
                camZoom (ssCamera staged) `shouldBe` dz
                -- ...the saved facing, kept...
                camFacing (ssCamera staged) `shouldBe` savedFacing
                -- ...the per-page camera the same repair fed...
                cam ← readIORef (wsCameraRef (spWorldState page))
                cam `shouldBe` WorldCamera dx dy
                -- ...an unrelated restored value, untouched...
                wtHour ⊚ readIORef (wsTimeRef (spWorldState page))
                    `shouldReturn` sentinelHour
                -- ...and exactly one warning about it.
                case cameraWarnings entries of
                    [entry] → do
                        leCategory entry `shouldBe` CatWorld
                        leMessage entry `shouldSatisfy`
                            T.isInfixOf (unWorldPageId stagedPageId)
                    other → expectationFailure
                        ("expected exactly one camera warning, got "
                         ⧺ show (map leMessage other))

    it "stages a finite saved view verbatim and says nothing" $ \env → do
        (staged, page, entries) ← stageWith env goodView
        let (gx, gy, gz) = goodView
        camPosition (ssCamera staged) `shouldBe` (gx, gy)
        camZoom (ssCamera staged) `shouldBe` gz
        camFacing (ssCamera staged) `shouldBe` savedFacing
        readIORef (wsCameraRef (spWorldState page))
            `shouldReturn` WorldCamera gx gy
        map leMessage (cameraWarnings entries) `shouldBe` []

-- | Stage a save carrying @view@ and hand back the staged session, its
--   one page, and everything staging logged.
stageWith ∷ HasCallStack ⇒ EngineEnv → (Float, Float, Float)
          → IO (StagedSession, StagedPage, [LogEntry])
stageWith env view = do
    (logger, drain) ← capturingLogger
    matReg ← readIORef (materialRegistryRef env)
    staged ← stageSession env logger (saveWith view) matReg ⌦ either
        (\e → expectationFailure (T.unpack (renderStageError e))
                ≫ error "unreachable")
        pure
    entries ← drain
    case find ((≡ stagedPageId) ∘ spPageId) (ssPages staged) of
        Nothing → expectationFailure "the staged page is missing"
                    ≫ error "unreachable"
        Just sp → pure (staged, sp, entries)

-- | Staging's warnings about the camera view, and only those.
cameraWarnings ∷ [LogEntry] → [LogEntry]
cameraWarnings = filter $ \e →
    leLevel e ≡ LevelWarn ∧ "camera view" `T.isInfixOf` leMessage e

capturingLogger ∷ IO (LoggerState, IO [LogEntry])
capturingLogger = do
    ref ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback
            (\e → atomicModifyIORef' ref (\es → (e : es, ()))) }
    pure (logger, reverse ⊚ readIORef ref)

snd3 ∷ (α, β, γ) → β
snd3 (_, b, _) = b

thd3 ∷ (α, β, γ) → γ
thd3 (_, _, c) = c
