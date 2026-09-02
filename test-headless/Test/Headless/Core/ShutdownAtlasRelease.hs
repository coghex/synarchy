-- | Regression coverage for issue #1691: shutdown now releases the
--   texture atlases loaded from disk before the Vulkan device is
--   destroyed.
--
--   Every disk texture stores an explicit @taCleanup@ closure in
--   @apTextureAtlases@, and 'Engine.Asset.Manager.cleanupAssetManager'
--   is the only code that runs one. Nothing called it, so every
--   loaded atlas's image, view and memory was still alive at
--   @vkDestroyDevice@. 'Engine.Loop.Shutdown.shutdownEngine' calls it
--   now, and the three properties that call has to hold are the three
--   this module drives:
--
--   * WHEN it runs — a device and its queues are both required, since
--     the teardown fails loudly without them and a device-less boot
--     mode has no atlas to release anyway;
--   * WHERE it runs — after the device-idle barrier and before the
--     generic Vulkan cleanup sweep;
--   * WHAT A FAILURE COSTS — the atlases, and nothing below it in the
--     shutdown sequence.
--
--   None of that needs a GPU, and none of it is checked here against
--   one: 'Engine.Loop.Shutdown.atlasReleaseDecision' reads presence
--   only, and 'Engine.Loop.Shutdown.shutdownEngineWith' takes the
--   release step as an argument, so the ordering is observable with
--   @vulkanDevice@ still 'Nothing' and every real Vulkan step in the
--   sequence a no-op. Real-device evidence is the dev-profile
--   validation-layer boot the issue asks for, not an hspec case.
module Test.Headless.Core.ShutdownAtlasRelease (spec) where

import UPrelude
import Test.Hspec
import Control.Exception (ErrorCall(..), fromException, throwIO, try)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import System.Exit (ExitCode(..))
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Error.Exception
    (EngineException(..), ExceptionType(..), SystemError(..), mkErrorContext)
import Engine.Core.Init (EngineInitResult(..))
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Engine.Core.Monad
    (EngineM', MonadError(..), MonadIO(..), runEngineM, modifyGraphicsState)
import Engine.Core.State
    ( EngineEnv, EngineLifecycle(..), GraphicsState(..)
    , TransientTexture(..), lifecycleRef )
import Engine.Core.Workers (EngineWorkers(..))
import Engine.Graphics.Vulkan.Types.Cleanup (Cleanup(..), emptyCleanup)
import Engine.Loop.Shutdown
    ( AtlasRelease(..), ShutdownTargets(..), atlasReleaseDecision
    , releaseLoadedAtlasesWith, shutdownEngineWith )

-- | An ordered trace of the shutdown steps a case cares about.
newtype Trace = Trace (IORef [Text])

newTrace ∷ IO Trace
newTrace = Trace ⊚ newIORef []

-- | Record one step. 'IO' rather than 'EngineM' so the same recorder
--   serves the injected 'Cleanup' fields and the transient-texture
--   closures, which are plain @IO ()@.
step ∷ Trace → Text → IO ()
step (Trace ref) name = atomicModifyIORef' ref $ \ns → (name : ns, ())

steps ∷ Trace → IO [Text]
steps (Trace ref) = reverse ⊚ readIORef ref

-- | A boot mode with no window and no workers: everything
--   'shutdownEngineWith' does besides the step under test is then a
--   no-op, so the trace is exactly what the case injected.
bareTargets ∷ ShutdownTargets
bareTargets = ShutdownTargets
  { stWindow  = Nothing
  , stWorkers = EngineWorkers
      { ewCombat = Nothing
      , ewSim    = Nothing
      , ewUnit   = Nothing
      , ewWorld  = Nothing
      , ewInput  = Nothing
      , ewLua    = Nothing
      }
  }

-- | Run an engine action against a fresh headless engine.
withEngine ∷ (EngineEnv → EngineM' ()) → IO (EngineEnv, Either EngineException ())
withEngine act = do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    result ← runEngineM (act env) env pure
    pure (env, result)

-- | The two graphics-state fields the ordering case observes: a
--   transient texture destroyed just BEFORE the release, and one
--   'Cleanup' slot run by the sweep just AFTER it.
instrumentTeardown ∷ Trace → EngineM' ()
instrumentTeardown trace = modifyGraphicsState $ \gs → gs
    { previewTexture = Just TransientTexture
        { ttHandle  = TextureHandle 1
        , ttCleanup = step trace "transient"
        }
    , vulkanCleanup = emptyCleanup
        { cleanupFontUI = step trace "sweep" }
    }

-- | A release that fails through the CPS error channel, the way
--   'Engine.Asset.Manager.cleanupAssetManager' reports.
throwingRelease ∷ EngineM' ()
throwingRelease = throwError $ EngineException
    (ExSystem (IOError "injected release failure")) "injected" mkErrorContext

-- | A release that fails the other way: a native IO exception thrown
--   straight from underneath, bypassing the CPS channel entirely.
nativeRelease ∷ EngineM' ()
nativeRelease = liftIO $ throwIO (ErrorCall "injected native release failure")

spec ∷ Spec
spec = describe "shutdown loaded-atlas release (#1691)" $ do

  describe "atlasReleaseDecision" $ do
    -- The teardown checks device and queues BEFORE it looks at whether
    -- the pool holds anything, so reaching it without either turns a
    -- shutdown with nothing to release into a shutdown that errors.
    it "releases only when a device and its queues are both present" $ do
      atlasReleaseDecision (Just ()) (Just ()) `shouldBe` ReleaseAtlases
      atlasReleaseDecision (Nothing ∷ Maybe ()) (Just ())
        `shouldBe` SkipAtlasRelease "no Vulkan device"
      atlasReleaseDecision (Just ()) (Nothing ∷ Maybe ())
        `shouldBe` SkipAtlasRelease "no device queues"
      atlasReleaseDecision (Nothing ∷ Maybe ()) (Nothing ∷ Maybe ())
        `shouldBe` SkipAtlasRelease "no Vulkan device or queues"

  describe "releaseLoadedAtlasesWith" $ do
    it "runs the release exactly once on a release decision" $ do
      trace ← newTrace
      (_, result) ← withEngine $ \_ → releaseLoadedAtlasesWith
        ReleaseAtlases (liftIO (step trace "release"))
      result `shouldBe` Right ()
      steps trace `shouldReturn` ["release"]

    it "runs nothing at all on a skip decision" $ do
      trace ← newTrace
      (_, result) ← withEngine $ \_ → releaseLoadedAtlasesWith
        (SkipAtlasRelease "no Vulkan device") (liftIO (step trace "release"))
      result `shouldBe` Right ()
      steps trace `shouldReturn` []

    -- Both failure channels are contained: neither may propagate out
    -- of the release and skip the teardown phases that follow it.
    forM_ [ ("an EngineException", throwingRelease)
          , ("a native exception",  nativeRelease)
          ] $ \(label, failing) →
      it ("contains " ⧺ label ⧺ " and returns normally") $ do
        (_, result) ← withEngine $ \_ →
          releaseLoadedAtlasesWith ReleaseAtlases failing
        result `shouldBe` Right ()

    -- Containment stops at the two exceptions that are not release
    -- failures. Swallowing an explicit exit would be the new way to
    -- fail to exit that the issue forbids.
    it "propagates an ExitCode rather than containing it" $ do
      EngineInitResult env ← initializeEngineHeadlessQuiet
      let action ∷ EngineM' ()
          action = releaseLoadedAtlasesWith ReleaseAtlases
                     (liftIO (throwIO (ExitFailure 3)))
      raised ← try (runEngineM action env pure)
      case raised of
        Left e | Just code ← fromException e → code `shouldBe` ExitFailure 3
        Left e  → expectationFailure $ "unexpected exception: " ⧺ show e
        Right _ → expectationFailure "ExitCode was swallowed by the release guard"

  describe "shutdownEngineWith" $ do
    it "releases the atlases after the transient textures and before the sweep" $ do
      trace ← newTrace
      (env, result) ← withEngine $ \_ → do
        instrumentTeardown trace
        shutdownEngineWith (liftIO (step trace "release")) bareTargets
      result `shouldBe` Right ()
      steps trace `shouldReturn` ["transient", "release", "sweep"]
      readIORef (lifecycleRef env) `shouldReturn` EngineStopped

    -- The whole point of containing the failure: a release that cannot
    -- complete still leaves a shutdown that completes.
    forM_ [ ("an EngineException", throwingRelease)
          , ("a native exception",  nativeRelease)
          ] $ \(label, failing) →
      it ("still reaches the sweep and EngineStopped when the release fails with "
            ⧺ label) $ do
        trace ← newTrace
        (env, result) ← withEngine $ \_ → do
          instrumentTeardown trace
          shutdownEngineWith
            (releaseLoadedAtlasesWith ReleaseAtlases failing) bareTargets
        result `shouldBe` Right ()
        steps trace `shouldReturn` ["transient", "sweep"]
        readIORef (lifecycleRef env) `shouldReturn` EngineStopped
