-- | Shared cursor-designation constants. Split out of
--   "World.Thread.Command.Cursor" (issue #564).
module World.Thread.Command.Cursor.Common
    ( maxDesignateSide
    , canonicalDesignationTile
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
import World.Types (WorldPageId, WorldState(..), WorldGenParams(..))
import World.Generate.Coordinates (canonicalTileCoord)
import World.Thread.Helpers (unWorldPageId)

-- | Cap on the designation rectangle's side length. Guards against a
--   misclick across the map turning into a 100k-tile designation.
maxDesignateSide ∷ Int
maxDesignateSide = 128

-- | Move a designation tile coord into the canonical (u-wrapped) frame
--   chunks are stored under (#1135).
--
--   Designation coords enter the engine from Lua — world.setMineAnchor,
--   world.designateMine and their chop/till/construct/plant siblings all
--   round arbitrary numbers — so nothing guarantees they are already in
--   that frame. Normalising HERE, at the entry point, is what keeps the
--   anchor, the live preview (World.Render.CursorQuads) and the commit
--   all talking about the same tile: an anchor left in a u-seam alias
--   would sit a whole world away from a canonical hover, turning a
--   one-tile designation into a capped 128-wide sweep and missing its
--   own loaded chunk. Identity for a coord already canonical.
canonicalDesignationTile ∷ WorldState → Int → Int → IO (Int, Int)
canonicalDesignationTile worldState gx gy = do
    paramsM ← readIORef (wsGenParamsRef worldState)
    pure (canonicalTileCoord (maybe 128 wgpWorldSize paramsM) gx gy)

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
