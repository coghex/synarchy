-- | Shared cursor-designation constants. Split out of
--   "World.Thread.Command.Cursor" (issue #564).
module World.Thread.Command.Cursor.Common
    ( maxDesignateSide
    , designateRect
    , recordDesignationOutcome
    , recordMissingWorldOutcome
    ) where

import UPrelude
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Data.IORef (readIORef)
import Engine.Core.State (EngineEnv)
import Engine.ActionOutcome (ActionOutcome(..), pushActionOutcome)
import World.Types (WorldPageId(..))
import World.Generate.Coordinates (localizeTileToAnchor)

-- | Cap on the designation rectangle's side length for MINE, TILL and
--   CHOP. Guards against a misclick across the map turning into a
--   100k-tile designation. (Structure planning has its own, tighter cap:
--   'World.Construct.Extent.maxStructureDragSide'.)
maxDesignateSide ∷ Int
maxDesignateSide = 128

-- | The rectangle a two-click drag actually drew, in the ANCHOR's local
--   alias frame (#1175 — see "World.Render.HitTest"'s frame contract).
--
--   Shared by MINE, TILL and CHOP so they cannot disagree about what a
--   seam-crossing drag means. STRUCTURE planning is deliberately not one
--   of them since #1844: it has its own 64-cell, anchor-preserving
--   helper ('World.Construct.Extent.structureDragExtent'), because
--   clamping from the LOW-coordinate end — which is what this one does —
--   can clamp the anchor itself out of a long negative drag, and the
--   preview it has to agree with never did that. The three tools here
--   keep this 128-cell behaviour unchanged.
--
--   The second endpoint is re-expressed
--   relative to the anchor BEFORE the @min@/@max@ and the
--   'maxDesignateSide' clamp: both picks come back canonical, and two
--   physically adjacent tiles across the seam sit a whole world apart in
--   that frame, so a raw @min@/@max@ would form a world-sized rectangle
--   and the cap would then sweep 128 tiles of unrelated terrain.
--
--   The returned corners stay in the anchor's frame: canonicalisation is
--   per enumerated tile, at lookup and storage only. Identity away from
--   the seam.
designateRect ∷ Int                        -- ^ world size in chunks
              → (Int, Int)                 -- ^ anchor (first click)
              → (Int, Int)                 -- ^ second endpoint, any alias
              → ((Int, Int), (Int, Int))   -- ^ ((xLo, yLo), (xHi, yHi))
designateRect worldSize anchor@(gx1, gy1) end =
    let (gx2, gy2) = localizeTileToAnchor worldSize anchor end
        xLo = min gx1 gx2
        yLo = min gy1 gy2
        xHi = min (max gx1 gx2) (xLo + maxDesignateSide - 1)
        yHi = min (max gy1 gy2) (yLo + maxDesignateSide - 1)
    in ((xLo, yLo), (xHi, yHi))

-- | F4 (#646) action-outcome oracle tap for a rectangle-sweep
--   designation commit: "accepted" if every requested tile/tree landed,
--   "partial" (with drop counts) if the filter dropped some but not all,
--   "rejected" (with the caller's own reason — till/mine's anchor
--   eligibility and chop's harvestable-target check mean different
--   things by "nothing landed", so a single generic reason would
--   misdescribe one of them) if nothing landed at all, or "noop" if the
--   swept rectangle itself was empty (requested == 0). Shared by
--   till/chop/mine, whose commit handlers all fire-and-forget from Lua
--   — only the world thread that actually runs the filter knows
--   requested vs applied.
recordDesignationOutcome
    ∷ EngineEnv → Text → Text → Int → Int → Int → Int → IO ()
recordDesignationOutcome env kind rejectedReason gx1 gy1 requested applied = do
    gt ← readIORef (wsGameTimeRef (toWorldSimCapability env))
    let dropped = requested - applied
        (outcome, reason)
            | requested ≡ 0 =
                ("noop", Just "nothing in the swept rectangle to designate")
            | applied ≡ 0 =
                ("rejected", Just rejectedReason)
            | dropped > 0 =
                ("partial", Just "designation filter dropped tiles in the swept rectangle")
            | otherwise = ("accepted", Nothing)
    pushActionOutcome (ucActionOutcomeRef (toUnitCombatCapability env)) ActionOutcome
        { aoTs        = gt
        , aoKind      = kind
        , aoOutcome   = outcome
        , aoWhereX    = Just (fromIntegral gx1)
        , aoWhereY    = Just (fromIntegral gy1)
        , aoTarget    = Nothing
        , aoRequested = Just requested
        , aoApplied   = Just applied
        , aoDropped   = Just dropped
        , aoReason    = reason
        , aoHandler   = Nothing
        }

-- | F4 (#646): the queued page no longer exists — destroyed between
--   enqueue and drain, or a stale/typo'd page id. Distinct from
--   recordDesignationOutcome's "rejected" (which means "the page
--   exists but nothing in the sweep qualified"): there's no world
--   state here to even attempt a filter against, so this is its own
--   reason rather than routed through the generic requested/applied
--   calculus. All four designation verbs (till/chop/mine/plant)
--   previously dropped this case silently, with no F4 record at all.
recordMissingWorldOutcome ∷ EngineEnv → Text → WorldPageId → Int → Int → IO ()
recordMissingWorldOutcome env kind pageId gx1 gy1 = do
    gt ← readIORef (wsGameTimeRef (toWorldSimCapability env))
    pushActionOutcome (ucActionOutcomeRef (toUnitCombatCapability env)) ActionOutcome
        { aoTs        = gt
        , aoKind      = kind
        , aoOutcome   = "rejected"
        , aoWhereX    = Just (fromIntegral gx1)
        , aoWhereY    = Just (fromIntegral gy1)
        , aoTarget    = Nothing
        , aoRequested = Nothing
        , aoApplied   = Nothing
        , aoDropped   = Nothing
        , aoReason    = Just ("world page not found: " <> unWorldPageId pageId)
        , aoHandler   = Nothing
        }
