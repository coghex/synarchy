{-# LANGUAGE Strict #-}
-- | The game clock's fresh-session origin (#2291).
--
--   'Engine.Core.State.gameTimeRef' is one process-wide counter, not a
--   per-world one, so "what does a brand-new session's clock read?" has
--   to be answered in exactly one place or the two sessions a process
--   can start — the first one, and every one after an Exit to Menu —
--   answer it differently. This module is that place.
--
--   __The three transitions, and which of them is this constant.__
--
--   * __Process boot__ seeds the ref with 'freshSessionGameTime'
--     ("Engine.Core.Init"). Menu time then accrues into it until the
--     first world is created, because "Unit.Thread"\'s tick advances the
--     clock whenever the engine is unpaused and never asks whether a
--     page is live.
--   * __Exit to Menu__ restores it to 'freshSessionGameTime'
--     ("Unit.Thread"\'s end-of-session step, reached from
--     'World.Thread.Command.Basic.handleWorldDestroyAllCommand'), so the
--     next world starts from the same reading the first one did rather
--     than from the previous session's accumulated total. Menu time
--     accrues after it exactly as it does after boot.
--   * __A load__ does NOT use this constant: it installs the save's own
--     @sdGameTime@ ('World.Load.Publish.publishStagedSession'), which is
--     the whole point of persisting the field. Nor does creating an
--     ADDITIONAL page inside a live session touch the clock —
--     'World.Thread.Command.Init.handleWorldInitCommand' writes it
--     never, in either the first-page or the multi-page case.
module Engine.Core.SessionEpoch
  ( freshSessionGameTime
  ) where

import UPrelude

-- | The game-clock reading a session that was not loaded from a save
--   starts at: zero seconds. Shared by the boot seeding and the
--   Exit-to-Menu reset so the two can never drift apart.
freshSessionGameTime ∷ Double
freshSessionGameTime = 0
