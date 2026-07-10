{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
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
module Test.Headless.World.ActionOutcome (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (atomicModifyIORef', readIORef)
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Engine.Core.State (EngineEnv(..))
import Engine.ActionOutcome (ActionOutcome(..))
import World.Page.Types (WorldPageId(..))
import World.Thread.Command.Cursor (handleWorldDesignateTillCommand)
import Test.Headless.Harness (sharedWorld)

spec ∷ SpecWith EngineEnv
spec = describe "action-outcome oracle (#646)" $
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
                aoWhereX ev `shouldBe` Just gx1
                aoWhereY ev `shouldBe` Just gy1
                aoRequested ev `shouldBe` Just 36  -- (5+1) * (5+1)
                aoApplied ev `shouldBe` Just 0
                aoDropped ev `shouldBe` Just 36
                aoReason ev `shouldNotBe` Nothing
