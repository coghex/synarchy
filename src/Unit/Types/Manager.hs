{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Unit identity and the top-level unit registry (`UnitManager`),
--   split out of "Unit.Types" (#575) — re-exported there so the public
--   API is unchanged.
module Unit.Types.Manager
    ( UnitId(..)
    , UnitManager(..)
    , emptyUnitManager
    , nextUnitId
    , unitsOnPages
    , unitsOnPage
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
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
    deriving anyclass (Hashable, Serialize)

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
