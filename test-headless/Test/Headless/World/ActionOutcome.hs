-- | F4 (#646): the action-outcome oracle tap records a designation's
--   requested/applied/dropped counts and reason even though
--   'handleWorldDesignateTillCommand' itself is fire-and-forget (no
--   return value the Lua caller could inspect) — the whole point of the
--   oracle is to surface exactly this kind of silent outcome.
--
--   Drives the real command handler directly (same technique as
--   'Test.Headless.World.SelectTileZ') against an anchor tile far
--   outside any generated chunk, so the designation's tillable-tile
--   filter drops every requested tile deterministically — no dependency
--   on the shared world's actual geography (fluid/flora placement).
--
--   'ringCapSpec' covers the second contract on the same buffer: the
--   ring is BOUNDED at 'actionOutcomeCap' and drops its oldest records
--   on overflow (#2284), through the shared helper and through Lua's
--   debug.recordOutcome alike.
module Test.Headless.World.ActionOutcome (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Foldable (toList)
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.ActionOutcome
    (ActionOutcome(..), actionOutcomeCap, pushActionOutcome)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import World.Page.Types (WorldPageId(..))
import World.Thread.Command.Cursor (handleWorldDesignateTillCommand)
import Test.Headless.Harness (sharedWorld)

spec ∷ SpecWith EngineEnv
spec = do
    oracleSpec
    ringCapSpec

oracleSpec ∷ SpecWith EngineEnv
oracleSpec = describe "action-outcome oracle (#646)" $
    it "records a rejected outcome with requested/applied/dropped counts \
       \when the whole designation sweep is unloaded" $ \env → do
        _ ← sharedWorld env 42 64 3
        logger ← readIORef (loggerRef env)
        let pid = WorldPageId "shared_42_64_3"
            -- Nowhere near any chunk this suite has generated or loaded
            -- — tillableAt's chunk lookup misses for every tile in the
            -- rectangle, so entries stays empty regardless of geography.
            gx1 = 1000000 ∷ Int
            gy1 = 1000000 ∷ Int
            gx2 = 1000005 ∷ Int
            gy2 = 1000005 ∷ Int

        -- Drain any pre-existing records so this assertion only sees
        -- what THIS command produces.
        _ ← atomicModifyIORef' (actionOutcomeRef env) $ \_ → (Seq.empty, ())

        handleWorldDesignateTillCommand env logger pid gx1 gy1 gx2 gy2

        drained ← atomicModifyIORef' (actionOutcomeRef env) $
            \buf → (Seq.empty, buf)
        case toList drained of
            [] → expectationFailure
                "expected handleWorldDesignateTillCommand to push an \
                \ActionOutcome record"
            (ev : _) → do
                aoKind ev `shouldBe` "till.designate"
                aoOutcome ev `shouldBe` "rejected"
                aoWhereX ev `shouldBe` Just (fromIntegral gx1)
                aoWhereY ev `shouldBe` Just (fromIntegral gy1)
                aoRequested ev `shouldBe` Just 36  -- (5+1) * (5+1)
                aoApplied ev `shouldBe` Just 0
                aoDropped ev `shouldBe` Just 36
                aoReason ev `shouldNotBe` Nothing

-- | The bound itself (#2284). Every example here starts by emptying
--   the shared engine's ring and ends by draining it, so neither the
--   oracle example above nor any later spec sharing this engine sees
--   the thousand-odd synthetic records.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "action-outcome ring cap"'@.
ringCapSpec ∷ SpecWith EngineEnv
ringCapSpec = describe "action-outcome ring cap (#2284)" $ do

    it "retains the whole ring at the cap and drops exactly the oldest \
       \record on the first overflowing push" $ \env → do
        writeIORef (actionOutcomeRef env) Seq.empty
        forM_ [1 .. actionOutcomeCap] $
            pushActionOutcome (actionOutcomeRef env) ∘ marker
        atCap ← readIORef (actionOutcomeRef env)
        Seq.length atCap `shouldBe` actionOutcomeCap
        markerIds atCap `shouldBe` [1 .. actionOutcomeCap]

        -- One more: the bound bites here and nowhere earlier, and it
        -- costs the ring its OLDEST record, not its newest.
        pushActionOutcome (actionOutcomeRef env) (marker (actionOutcomeCap + 1))
        overCap ← drainRing env
        Seq.length overCap `shouldBe` actionOutcomeCap
        markerIds overCap `shouldBe` [2 .. actionOutcomeCap + 1]

    it "holds exactly the newest cap records after a long undrained run \
       \through the shared helper" $ \env → do
        writeIORef (actionOutcomeRef env) Seq.empty
        forM_ [1 .. overflowTotal] $
            pushActionOutcome (actionOutcomeRef env) ∘ marker
        buf ← drainRing env
        Seq.length buf `shouldBe` actionOutcomeCap
        markerIds buf `shouldBe` [overflowCount + 1 .. overflowTotal]

    -- Requirement 2 and 3 together, through the REGISTERED Lua verbs
    -- rather than the Haskell helper: debug.recordOutcome must be
    -- bounded by the same cap (it used to re-spell the append), and
    -- debug.drainActionOutcomes must still hand back oldest-first and
    -- leave the ring empty behind it.
    it "bounds debug.recordOutcome by the same cap and drains it \
       \oldest-first, leaving the ring empty" $ \env → do
        writeIORef (actionOutcomeRef env) Seq.empty
        ls ← bareLuaBackend env

        recorded ← executeDebugLua (lbsLuaState ls) $ T.concat
            [ "local n = 0 "
            , "for i = 1, ", tshow overflowTotal, " do "
            , "  if debug.recordOutcome{ kind = 'ring.cap', "
            , "       outcome = 'accepted', target = i } then n = n + 1 end "
            , "end "
            , "return tostring(n)" ]
        -- Every call accepted: a rejected record would make a short
        -- ring look like a working cap.
        recorded `shouldBe` quoted (tshow overflowTotal)

        -- length | oldest kept | newest kept | contiguous ascending? |
        -- length of a second drain
        drained ← executeDebugLua (lbsLuaState ls) $ T.concat
            [ "local d = debug.drainActionOutcomes() "
            , "local ordered = 'yes' "
            , "for i = 2, #d do "
            , "  if d[i].target ~= d[i-1].target + 1 then ordered = 'no' end "
            , "end "
            , "local again = debug.drainActionOutcomes() "
            , "return tostring(#d) .. '|' "
            , "    .. tostring(d[1] and d[1].target) .. '|' "
            , "    .. tostring(d[#d] and d[#d].target) .. '|' "
            , "    .. ordered .. '|' .. tostring(#again)" ]
        drained `shouldBe` quoted (T.intercalate "|"
            [ tshow actionOutcomeCap
            , tshow (overflowCount + 1)
            , tshow overflowTotal
            , "yes"
            , "0" ])

-- | How far past the cap the bulk examples push. Small enough to keep
--   the run cheap, large enough that an off-by-one in the drop count
--   changes both the retained identities and the drain summary.
overflowCount ∷ Int
overflowCount = 25

-- | Derived from the exported constant, never a duplicated literal: a
--   cap change moves these examples with it instead of silently
--   testing a number the engine no longer uses.
overflowTotal ∷ Int
overflowTotal = actionOutcomeCap + overflowCount

-- | One uniquely identified record. 'aoTarget' carries the ordinal, so
--   the retained set names exactly WHICH records survived rather than
--   only how many.
marker ∷ Int → ActionOutcome
marker i = ActionOutcome
    { aoTs        = fromIntegral i
    , aoKind      = "ring.cap"
    , aoOutcome   = "accepted"
    , aoWhereX    = Nothing
    , aoWhereY    = Nothing
    , aoTarget    = Just (fromIntegral i)
    , aoRequested = Nothing
    , aoApplied   = Nothing
    , aoDropped   = Nothing
    , aoReason    = Nothing
    , aoHandler   = Nothing
    }

-- | The ordinals 'marker' stamped, in ring order (oldest first).
markerIds ∷ Seq.Seq ActionOutcome → [Int]
markerIds = map (maybe (-1) fromIntegral ∘ aoTarget) ∘ toList

-- | Read the ring and leave it empty — the same atomic swap
--   debug.drainActionOutcomes performs.
drainRing ∷ EngineEnv → IO (Seq.Seq ActionOutcome)
drainRing env =
    atomicModifyIORef' (actionOutcomeRef env) $ \buf → (Seq.empty, buf)

-- | The registered production Lua API on a bare backend: no GPU, no Lua
--   thread and no scripts loaded, so @debug.recordOutcome@ and
--   @debug.drainActionOutcomes@ are reached exactly as a real boot
--   registers them. Mirrors 'Test.Headless.World.Render.SceneStats'.
bareLuaBackend ∷ EngineEnv → IO LuaBackendState
bareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

-- | The debug console JSON-encodes its result, so a Lua string comes
--   back quoted.
quoted ∷ Text → Text
quoted t = "\"" <> t <> "\""
