{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Physical + synthetic CHARACTER dispatch (#787): routes a raw
--   'InputCharEvent' to shell-text or UI-text input, plus the F4
--   (#730) char-aggregate batching that collapses a run of characters
--   into one truthful @input.type@ outcome. Split out of
--   'Engine.Input.Thread' to keep that facade a thin thread-loop
--   entrypoint; reached only through
--   'Engine.Input.Thread.Dispatch.dispatchInput' /
--   'Engine.Input.Thread.Dispatch.processInput'.
module Engine.Input.Thread.Char
  ( dispatchCharEvent
  , accumulateCharOutcome
  , flushPendingCharBatch
  ) where

import UPrelude
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State
import Engine.Input.Types
import Engine.Scripting.Lua.Types
import Engine.ActionOutcome (ActionOutcome(..), pushActionOutcome)
import qualified Engine.Core.Queue as Q
import UI.Manager (validateFocus)
import UI.Types (ElementHandle(..))
import UI.Focus (FocusId(..), fmCurrentFocus)

-- | F4 (#730): push the pending char-batch aggregate (if any) as ONE
--   ActionOutcome record and clear it — so a synthetic multi-character
--   @input.type@ sequence (N 'InputCharEvent's with no other event
--   interleaved before its trailing 'InputBarrier') collapses to a
--   single truthful record instead of N misleadingly-separate ones
--   (#730 review). Uniform delivery (every char applied, or every char
--   dropped) reads "accepted"/"noop"; a mixed batch reads "partial"
--   with consistent requested/applied/dropped counts.
flushPendingCharBatch ∷ EngineEnv → InputState → IO InputState
flushPendingCharBatch env inpSt = case inpCharBatch inpSt of
    Nothing → return inpSt
    Just batch → do
        gt ← readIORef (gameTimeRef env)
        let req = cbRequested batch
            app = cbApplied batch
            drp = cbDropped batch
            outcome
              | drp ≡ 0   = "accepted"
              | app ≡ 0   = "noop"
              | otherwise = "partial"
        pushActionOutcome (actionOutcomeRef env) ActionOutcome
            { aoTs = gt, aoKind = "input.type", aoOutcome = outcome
            , aoWhereX = Nothing, aoWhereY = Nothing
            , aoTarget = cbTarget batch
            , aoRequested = Just req, aoApplied = Just app, aoDropped = Just drp
            , aoReason = cbDropReason batch
            , aoHandler = cbHandler batch
            }
        return inpSt { inpCharBatch = Nothing }

-- | F4 (#730): fold one char's routing outcome into the pending batch
--   (starting a fresh one if none is in flight). `domain` names the
--   delivery target's kind when `applied`, or the drop classification
--   otherwise; an applied char's domain/target always overwrite the
--   batch's reported handler (an accepted delivery is more informative
--   than a drop classification), while a drop only sets the fallback
--   handler if nothing has been applied yet.
accumulateCharOutcome ∷ InputState → Bool → Text → Maybe Word32 → InputState
accumulateCharOutcome inpSt applied domain target =
    let batch0 = case inpCharBatch inpSt of
            Just b  → b
            Nothing → emptyCharBatch
        batch1
          | applied = batch0
              { cbRequested = cbRequested batch0 + 1
              , cbApplied   = cbApplied batch0 + 1
              , cbHandler   = Just domain
              , cbTarget    = target
              }
          | otherwise = batch0
              { cbRequested  = cbRequested batch0 + 1
              , cbDropped    = cbDropped batch0 + 1
              , cbDropReason = Just domain
              , cbHandler    = case cbHandler batch0 of
                  Just h  → Just h
                  Nothing → Just domain
              }
    in inpSt { inpCharBatch = Just batch1 }

-- | The real per-event dispatch for 'InputCharEvent' — split out of
--   'dispatchInput' the same way 'Engine.Input.Thread.Keyboard.dispatchKeyEvent'
--   is; the batch flush that precedes a non-char event lives in
--   'Engine.Input.Thread.Dispatch.processInput' instead, since a char
--   event never flushes here (it's the thing BEING accumulated).
dispatchCharEvent ∷ EngineEnv → InputState → Char → IO InputState
dispatchCharEvent env inpSt c =
    -- Backtick is INTENTIONALLY untypeable in every text field:
    -- it's the shell-toggle key (KeyGrave above), and letting the
    -- char through would type a '`' into the shell or textbox the
    -- press just toggled/defocused. F4 (#730): each branch folds
    -- its routing outcome into the pending char-aggregate batch
    -- (see 'accumulateCharOutcome') rather than pushing its own
    -- record — 'flushPendingCharBatch' collapses a whole run of
    -- these (a synthetic multi-character @input.type@, or one
    -- real keystroke's char) into a single truthful record.
    if c ≡ '`'
      then return $ accumulateCharOutcome inpSt False "dropped_backtick" Nothing
      else do
        focusMgr ← readIORef (focusManagerRef env)
        uiFocus ← atomicModifyIORef' (uiManagerRef env) validateFocus
        case (fmCurrentFocus focusMgr, uiFocus) of
          (Just (FocusId fid), _) → do
            Q.writeQueue (luaQueue env) (LuaCharInput fid c)
            return $ accumulateCharOutcome inpSt True "shell_text" (Just fid)
          (Nothing, Just (ElementHandle eh)) → do
            Q.writeQueue (luaQueue env) (LuaUICharInput c)
            return $ accumulateCharOutcome inpSt True "ui_text" (Just eh)
          (Nothing, Nothing) →
            return $ accumulateCharOutcome inpSt False "dropped_unfocused" Nothing
