{-# LANGUAGE Strict #-}
-- | #2056: the PRESENTATION BOUNDARY — the handshake a Lua surface uses
--   to prove its own laid-out content actually reached the renderer,
--   rather than assuming it did because a frame usually happens.
--
--   The problem it solves is narrow and real. @UI.showPage@ and
--   @UI.deleteElement@ mutate the shared UI-manager 'Data.IORef.IORef'
--   synchronously from the Lua thread; the renderer learns of either
--   only when 'UI.Render.renderUIPages' snapshots that same ref. Lua
--   can therefore show a page, act on the assumption that the player
--   saw it, and destroy the elements again — all inside one
--   uninterrupted Lua call, with no snapshot in between. Anything whose
--   correctness rests on the player having SEEN something (the tutorial
--   checklist's #996 sticky suppression, retired by #1941) cannot be
--   built on that assumption.
--
--   The handshake is three operations:
--
--   [@armPresentation@] Lua mints a token AFTER its elements are built
--   and its page is showing.
--   [@witnessPresentation@] the renderer, having rendered a snapshot to
--   completion, publishes that SNAPSHOT's armed token.
--   [@isPresented@] Lua acts once its own token has been published.
--
--   Two properties make it a proof rather than a heuristic, and both
--   come from the witness living INSIDE 'UIPageManager':
--
--     * /Causality./ 'UI.Render.renderUIPages' reads the pages and
--       'pwArmed' out of one 'readIORef', so a snapshot carrying token
--       @t@ necessarily carries every element and page-visibility
--       mutation that preceded @t@'s arm — they are writes to the same
--       ref, and the arm is one of them. There is no separate counter
--       whose ordering against the manager would have to be argued.
--     * /Staleness rejection./ Evidence is published against the token
--       the snapshot ACTUALLY held, never the one armed by the time the
--       publication lands. A surface that hides, collapses, scrolls or
--       rebuilds simply arms again; the older publication cannot reach
--       the newer token, so evidence gathered for content that is no
--       longer on screen authorises nothing. This is why
--       'witnessPresentation' takes the token as an argument instead of
--       re-deriving it from the manager it is applied to — see the
--       WARNING there.
--
--   Both counters are transient and never persisted: a fresh
--   'emptyUIPageManager' answers "nothing armed, nothing witnessed",
--   which denies presentation rather than inventing it.
--   Every operation here is PURE, applied by each caller to the shared
--   'Data.IORef.IORef' at its own site. There is deliberately no
--   IORef-taking wrapper: routing the write through one would hide it
--   from @tools/engine_env_capability_audit.py@'s §5 writing-module
--   map, which is what records that the render thread mutates
--   @uiManagerRef@ at all.
module UI.Manager.Presentation
  ( armPresentation
  , snapshotArmedToken
  , witnessPresentation
  , presentationWitnessed
  , isPresented
  ) where

import UPrelude
import UI.Types

-- | Mint the next presentation token. Call it from the Lua thread only
--   once the content that must be seen is fully built AND its page is
--   showing: everything written to this ref before this point is what
--   the token stands for.
armPresentation ∷ UIPageManager → (UIPageManager, Word64)
armPresentation mgr =
    let w     = upmPresentation mgr
        token = pwArmed w + 1
    in ( mgr { upmPresentation = w { pwArmed = token } }, token )

-- | The token a snapshot was carrying. This is the ONLY value a
--   renderer may publish for that snapshot.
snapshotArmedToken ∷ UIPageManager → Word64
snapshotArmedToken = pwArmed ∘ upmPresentation

-- | Record that @token@ was inside a snapshot the renderer rendered to
--   completion. Monotonic: a late publication from an older snapshot
--   cannot lower the witness.
--
--   WARNING: @token@ MUST come from 'snapshotArmedToken' applied to the
--   snapshot that was rendered — never from the manager this is being
--   applied to. Between the snapshot and the write the Lua thread may
--   have armed a token for content that frame never saw, and publishing
--   that value would authorise exactly the race this module closes.
witnessPresentation ∷ Word64 → UIPageManager → UIPageManager
witnessPresentation token mgr =
    let w = upmPresentation mgr
    in mgr { upmPresentation =
               w { pwWitnessed = max (pwWitnessed w) token } }


-- | The greatest token proven presented so far.
presentationWitnessed ∷ UIPageManager → Word64
presentationWitnessed = pwWitnessed ∘ upmPresentation

-- | Has @token@'s content been in front of a completed renderer
--   snapshot? Token @0@ is never presented: it is the value a surface
--   holds when it has armed nothing.
isPresented ∷ Word64 → UIPageManager → Bool
isPresented token mgr =
    token > 0 ∧ presentationWitnessed mgr ≥ token
