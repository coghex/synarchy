-- | The worker threads one boot mode started, and the one definition of
--   the order they stop in.
--
--   Both teardown paths go through this module. The clean exit reaches
--   it via 'Engine.Loop.Shutdown.shutdownEngine', which stops
--   'preRenderWorkers' before its Vulkan\/GLFW teardown and
--   'postRenderWorkers' after; the fatal-error tail
--   (@App.Boot.handleBootResult@, #1021) and the two modes that
--   initialize neither Vulkan nor GLFW (@App.Headless@ and @App.Dump@)
--   reach it via 'shutdownEngineWorkers', which stops every worker in
--   one pass. Neither writes an order of its own: the record lives
--   library-side (#1036) precisely so there is nothing to keep in
--   agreement.
module Engine.Core.Workers
  ( EngineWorkers(..)
  , WorkerSlot
  , preRenderWorkers
  , postRenderWorkers
  , stopWorkers
  , shutdownEngineWorkers
  ) where

import UPrelude
import Control.Monad.IO.Class (MonadIO, liftIO)
import Engine.Core.Thread (ThreadState, shutdownThread)

-- | The worker threads one boot mode started — 'Nothing' for each one
--   its topology never starts (headless and dump run no input thread;
--   preview runs neither world, unit, sim nor combat).
--
--   Build it with record syntax and nothing else: there is no
--   all-'Nothing' value to update from, so every slot has to be named
--   at the construction site, and @-Wall@'s @-Wmissing-fields@ flags a
--   boot module that leaves one out — a build failure under CI's
--   @-Werror@. Naming a slot is all the type can enforce; supplying
--   'Nothing' for a thread the mode did start, or handing a slot the
--   wrong 'ThreadState', still type-checks.
data EngineWorkers = EngineWorkers
  { ewCombat ∷ Maybe ThreadState
  , ewSim    ∷ Maybe ThreadState
  , ewUnit   ∷ Maybe ThreadState
  , ewWorld  ∷ Maybe ThreadState
  , ewInput  ∷ Maybe ThreadState
  , ewLua    ∷ Maybe ThreadState
  }

-- | One worker in teardown order: the name shutdown announces it under,
--   and its thread when the mode started one.
type WorkerSlot = (Text, Maybe ThreadState)

-- | The workers that stop /before/ Vulkan and GLFW teardown, in order.
--
--   Each leads its own consumer. Combat is a producer for the unit
--   thread: wound ticks and combat resolution enqueue
--   UnitCollapse\/UnitKill onto the unit queue, so combat has to stop
--   before the thread that drains it. Sim is a producer for the world
--   thread: its fluid writebacks go onto the world command queue, which
--   the world thread alone drains, so sim has to stop before that
--   thread.
--
--   Neither dependency picks this phase on its own — both of those
--   consumers stop after the render teardown, so the later phase would
--   order them just as correctly. They stop ahead of the render
--   teardown because that is where the windowed modes have always
--   stopped them.
preRenderWorkers ∷ EngineWorkers → [WorkerSlot]
preRenderWorkers w = [ ("combat", ewCombat w)
                     , ("sim",    ewSim w) ]

-- | The workers that stop /after/ Vulkan and GLFW teardown, in order.
postRenderWorkers ∷ EngineWorkers → [WorkerSlot]
postRenderWorkers w = [ ("unit",  ewUnit w)
                      , ("world", ewWorld w)
                      , ("input", ewInput w)
                      , ("Lua",   ewLua w) ]

-- | Every worker, in the single teardown order: combat → sim → unit →
--   world → input → Lua. Composed from the two phases above so the
--   order cannot be stated twice.
allWorkers ∷ EngineWorkers → [WorkerSlot]
allWorkers w = preRenderWorkers w ⧺ postRenderWorkers w

-- | Stop one phase's workers in list order, announcing each by name
--   first, and calling 'shutdownThread' for every slot that started
--   one. It is production's one caller of 'shutdownThread' outside
--   'Engine.Core.Thread' itself:
--   'Engine.Loop.Shutdown.shutdownEngineWith' calls it once per phase,
--   'shutdownEngineWorkers' calls it once for @allWorkers@ with a
--   no-op announce, and @App.Boot.luaThreadOrAbort@ calls it with the
--   already-started slots filtered down to the ones a partial boot
--   actually forked. 'announce' runs for every slot, 'Nothing'
--   included; only the join beneath it is conditional.
--
--   'shutdownThread' is idempotent, so a mode whose error path fires
--   after a partial clean shutdown re-stops nothing.
stopWorkers ∷ MonadIO m ⇒ (Text → m ()) → [WorkerSlot] → m ()
stopWorkers announce = mapM_ $ \(name, mThread) → do
    announce name
    liftIO $ forM_ mThread shutdownThread

-- | Stop every worker the mode started, in @allWorkers@ order, without
--   announcing them: this convenience exposes no announcer parameter
--   and hands 'stopWorkers' a no-op one. The plain 'IO' type is not
--   what silences it — 'stopWorkers' announces in any 'MonadIO', so
--   the announcing path is simply the one whose caller has an
--   announcer to pass, which @Engine.Loop.Shutdown.shutdownEngineWith@
--   supplies from its own logging combinators.
--
--   Three call sites use it: the fatal-error tail
--   (@App.Boot.handleBootResult@, #1021), and the two modes that run
--   no render teardown, @--headless@ and @--dump@. The latter two
--   reach it on their ordinary success path and call @shutdownLogger@
--   on that still-live logger immediately afterwards.
shutdownEngineWorkers ∷ EngineWorkers → IO ()
shutdownEngineWorkers = stopWorkers (\_ → pure ()) ∘ allWorkers
