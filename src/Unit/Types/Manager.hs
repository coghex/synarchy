{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
-- | Unit identity and the top-level unit registry (`UnitManager`),
--   split out of "Unit.Types" (#575) — re-exported there so the public
--   API is unchanged.
module Unit.Types.Manager
    ( UnitId(..)
    , UnitManager(..)
    , emptyUnitManager
    , nextUnitId
    , retirePageUnits
    , unitsOnPages
    , unitsOnPage
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import Control.DeepSeq (NFData)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import World.Page.Types (WorldPageId(..))
import Unit.Types.Def (UnitDef(..))
import Unit.Types.Instance (UnitInstance(..))

-- | Unique identifier for a spawned unit instance.
newtype UnitId = UnitId { unUnitId ∷ Word32 }
    deriving stock (Show, Eq, Ord, Generic)
    -- anyclass (Generic-default) matches what GHC picked implicitly;
    -- same wire bytes as the raw Word32, so no save bump.
    deriving anyclass (Hashable, NFData, Serialize)

-- | Holds all unit definitions and all spawned instances.
--   Lives behind an IORef in EngineEnv.
--
--   Selection state lives here so it's a single source of truth: the
--   renderer and the info-panel both read from the same struct, and
--   destroy-time cleanup is one atomic modify (delete from both maps).
data UnitManager = UnitManager
    { umDefs      ∷ !(HM.HashMap Text UnitDef)
    , umInstances ∷ !(HM.HashMap UnitId UnitInstance)
    , umSelected  ∷ !(HS.HashSet UnitId)
    , umNextId    ∷ !Word32
    } deriving (Show, Eq)

emptyUnitManager ∷ UnitManager
emptyUnitManager = UnitManager
    { umDefs      = HM.empty
    , umInstances = HM.empty
    , umSelected  = HS.empty
    , umNextId    = 1
    }

nextUnitId ∷ UnitManager → (UnitId, UnitManager)
nextUnitId um =
    let uid = UnitId (umNextId um)
    in (uid, um { umNextId = umNextId um + 1 })

-- | #2476: retire one page incarnation's units — every instance on
--   @pageId@ whose 'UnitId' is strictly below the exclusive @cutoff@,
--   and those ids' selection entries. Answers the set removed, so the
--   caller's sim-state removal is driven by the ids this transition
--   actually took rather than by a second, independently recomputed
--   filter.
--
--   PURE, and shared by both halves of a page teardown deliberately.
--   The lifecycle transition applies it DIRECTLY, under the lifecycle
--   lock, so an old incarnation's unit stops being addressable the
--   instant its page is destroyed or replaced — a page id is a reusable
--   name, so every verb that resolves a unit's page would otherwise
--   keep finding it and could spend it on the replacement. The queued
--   'Unit.Command.Types.UnitClearPage' applies the identical function
--   afterwards, which is what retires a spawn that was already in
--   flight when the transition ran (#58). One body, two callers: the
--   immediate removal and the queued mop-up cannot disagree about what
--   belongs to the departed incarnation.
--
--   The allocator is untouched on purpose: rewinding it would let the
--   replacement reissue an id this very transition is retiring.
retirePageUnits ∷ WorldPageId → UnitId → UnitManager
                → (UnitManager, HS.HashSet UnitId)
retirePageUnits pageId cutoff um = (um', doomed)
  where
    doomed = HM.keysSet (HM.filterWithKey matches (umInstances um))
    matches uid inst = uiPage inst ≡ pageId ∧ uid < cutoff
    um' = um { umInstances = HM.filterWithKey
                                 (\uid _ → not (HS.member uid doomed))
                                 (umInstances um)
             , umSelected  = HS.difference (umSelected um) doomed
             }

-- | Instances belonging to any of the given world pages — the
--   world-scoping filter for render (the visible set) and queries.
unitsOnPages ∷ HS.HashSet WorldPageId
             → HM.HashMap UnitId UnitInstance
             → HM.HashMap UnitId UnitInstance
unitsOnPages pages = HM.filter (\inst → HS.member (uiPage inst) pages)

-- | Instances belonging to one specific world page (the active world).
unitsOnPage ∷ WorldPageId
            → HM.HashMap UnitId UnitInstance
            → HM.HashMap UnitId UnitInstance
unitsOnPage pid = HM.filter (\inst → uiPage inst ≡ pid)
