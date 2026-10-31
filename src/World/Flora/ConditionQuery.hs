{-# LANGUAGE Strict #-}
-- | The one page-qualified query over a page's persistent flora
--   conditions (#2552, EFM-6).
--
--   A page keeps a condition in one of two owners, by what the subject
--   is: a wild or row-flora OCCURRENCE has a record in the
--   identity-keyed 'World.Flora.Condition.FloraConditions' store
--   (#2549), and a groundcover PLOT carries its record embedded in the
--   tile-keyed 'World.Flora.CropPlot.CropPlot' value (#2552). Both hold
--   the same 'FloraConditionRecord', and this is the single place a
--   caller names the subject and gets that record back, so EFM-7's
--   Lua inspection verb has one Haskell function to call rather than
--   two stores to reconcile.
--
--   The query is page-QUALIFIED because neither key is global: an
--   instance id is page-scoped (an id is unique only within its owning
--   page) and a plot key is a tile of one page's frame. A subject is
--   therefore always answered against the named page's stores, and a
--   page the caller does not hold answers 'Nothing'.
--
--   Pure and read-only. It changes no store, and it never reads the
--   reserved 'World.Flora.Identity.floraInstanceIdNone': that value
--   names no plant (a plot's growth adapter carries it), so it answers
--   'Nothing' rather than being looked up.
module World.Flora.ConditionQuery
    ( ConditionSubject(..)
    , PageConditionStores(..)
    , lookupConditionRecord
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import World.Flora.Condition (FloraConditionRecord, FloraConditions)
import World.Flora.CropPlot (CropPlots, cpCondition)
import World.Flora.Identity (FloraInstanceId, isFloraInstanceIdNone)
import World.Page.Types (WorldPageId)

-- | What a condition belongs to.
data ConditionSubject
    = SubjectOccurrence !FloraInstanceId
      -- ^ Any wild or row-flora occurrence, by its #1854 identity.
    | SubjectPlot !(Int, Int)
      -- ^ A groundcover plot, by its CANONICAL tile (the key
      --   'World.Flora.CropPlot.CropPlots' stores it under).
    deriving (Show, Eq)

-- | The two condition owners of ONE page, as that page's session state
--   holds them.
data PageConditionStores = PageConditionStores
    { pcsOccurrences ∷ !FloraConditions
    , pcsPlots       ∷ !CropPlots
    }

-- | The stored condition of @subject@ on page @pid@, or its absence.
lookupConditionRecord
    ∷ WorldPageId → ConditionSubject
    → HM.HashMap WorldPageId PageConditionStores
    → Maybe FloraConditionRecord
lookupConditionRecord pid subject pages = do
    stores ← HM.lookup pid pages
    case subject of
        SubjectOccurrence iid
            | isFloraInstanceIdNone iid → Nothing
            | otherwise → HM.lookup iid (pcsOccurrences stores)
        SubjectPlot tile → HM.lookup tile (pcsPlots stores) ≫= cpCondition
