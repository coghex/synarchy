{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Page-scoped world components (issue #760, save-overhaul B2). Each
--   carries a slice PER world page, keyed by 'WorldPageId', and every
--   one is validated against the authoritative page set the
--   @"world-pages"@ component establishes (requirement 8):
--
--   - @"world-pages"@ (required, page-set authority) — per page: identity,
--     generation params, dates/clocks, map mode, and the page's OWN
--     remembered camera position. Owner: the world page. Boundary reason:
--     this is the spine every other page-scoped component's page set is
--     checked against; the world-generation seed lives in its gen params
--     (requirement 10). No dependencies — it is the root of the page
--     dependency graph.
--   - @"world-edits"@ (required) — per page: the terrain + structure edit
--     log. Owner: the world edit layer. Boundary reason: player terrain/
--     structure modifications are a distinct, replay-on-load concern.
--   - @"world-activity"@ (required) — per page: designations (mine/
--     construct/chop/till/plant), flora harvests, crop plots, ground
--     items, and spoil piles. Owner: the mutable-world-activity layer.
--     Boundary reason: requirement 2 bullet 4's "designations, jobs,
--     progress, flora, crops, ground items, spoil" — the transient-ish
--     but persisted world activity, grouped away from the terrain spine
--     and the entity managers.
--
--   Every slice list is encoded in canonical (page-id ascending) order
--   so identical input produces identical bytes (requirement 10).
module World.Save.Component.Page
    ( worldPagesCodec
    , worldEditsCodec
    , worldActivityCodec
    , PageCoreDTO(..)
    , WorldPagesDTO(..)
    , PageEditsDTO(..)
    , WorldEditsDTO(..)
    , PageActivityDTO(..)
    , WorldActivityDTO(..)
    , basePageSnapshots
    , applyWorldEdits
    , applyWorldActivity
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Craft.Bills (emptyCraftBills)
import Power.Types (emptyPowerNodes)
import World.Generate.Types (WorldGenParams)
import World.Page.Types (WorldPageId, WorldIdentity)
import World.Render.Zoom.Types (ZoomMapMode)
import World.Edit.Types (WorldEdits, emptyWorldEdits)
import World.Mine.Types (MineDesignations)
import World.Construct.Types (ConstructDesignations)
import World.Chop.Types (ChopDesignations)
import World.Till.Types (TillDesignations)
import World.Plant.Types (PlantDesignations)
import World.Spoil.Types (SpoilPiles, emptySpoilPiles)
import World.Flora.Harvest (FloraHarvests, emptyFloraHarvests)
import World.Flora.CropPlot (CropPlots, emptyCropPlots)
import Item.Ground (GroundItems, emptyGroundItems)
import World.Save.Types
    ( BuildingSnapshot(..), UnitSnapshot(..) )
import World.Save.Snapshot (SessionSnapshot(..), PageSnapshot(..))
import World.Save.Component.Types

-- Canonical (page-id ascending) ordered list of a snapshot's pages.
orderedPages ∷ SessionSnapshot → [PageSnapshot]
orderedPages = L.sortOn pgsPageId . HM.elems . snapPages

-- world-pages -------------------------------------------------------

-- | One page's identity / clock / camera core.
data PageCoreDTO = PageCoreDTO
    { pcPageId      ∷ !WorldPageId
    , pcGenParams   ∷ !WorldGenParams
    , pcCameraX     ∷ !Float
    , pcCameraY     ∷ !Float
    , pcTimeHour    ∷ !Int
    , pcTimeMinute  ∷ !Int
    , pcDateYear    ∷ !Int
    , pcDateMonth   ∷ !Int
    , pcDateDay     ∷ !Int
    , pcMapMode     ∷ !ZoomMapMode
    , pcIdentity    ∷ !(Maybe WorldIdentity)
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTO = WorldPagesDTO { wpdPages ∷ [PageCoreDTO] }
    deriving (Show, Generic, Serialize)

worldPagesCodec ∷ ComponentCodec WorldPagesDTO
worldPagesCodec = serializeCodec
    worldPagesComponentId 1 True []
    encodePages (\_ d → Right d) validatePages
  where
    encodePages snap = WorldPagesDTO (map toPageCore (orderedPages snap))
    toPageCore p = PageCoreDTO
        { pcPageId     = pgsPageId p
        , pcGenParams  = pgsGenParams p
        , pcCameraX    = pgsCameraX p
        , pcCameraY    = pgsCameraY p
        , pcTimeHour   = pgsTimeHour p
        , pcTimeMinute = pgsTimeMinute p
        , pcDateYear   = pgsDateYear p
        , pcDateMonth  = pgsDateMonth p
        , pcDateDay    = pgsDateDay p
        , pcMapMode    = pgsMapMode p
        , pcIdentity   = pgsIdentity p
        }
    -- Component-local invariant (requirement 3): the page-set authority
    -- must not itself carry a duplicate or empty page set.
    validatePages (WorldPagesDTO ps)
        | null ps = [ ComponentError worldPagesComponentId 1 ValidatePhase
                        "no world pages in save" ]
        | otherwise =
            [ ComponentError worldPagesComponentId 1 ValidatePhase
                ("duplicate page id " <> tshow pid)
            | (pid, n) ← HM.toList
                          (HM.fromListWith (+) [ (pcPageId p, 1 ∷ Int) | p ← ps ])
            , n > 1 ]

tshow ∷ Show a ⇒ a → Text
tshow = T.pack . show

-- | Turn the decoded page cores into the base 'PageSnapshot' map every
--   other page-scoped component then writes onto (assembly). All entity/
--   activity/edit fields start empty and are overwritten by their own
--   REQUIRED components; a valid save leaves none of these placeholders.
basePageSnapshots ∷ WorldPagesDTO → HM.HashMap WorldPageId PageSnapshot
basePageSnapshots (WorldPagesDTO ps) =
    HM.fromList [ (pcPageId p, toBase p) | p ← ps ]
  where
    toBase p = PageSnapshot
        { pgsPageId       = pcPageId p
        , pgsGenParams    = pcGenParams p
        , pgsCameraX      = pcCameraX p
        , pgsCameraY      = pcCameraY p
        , pgsTimeHour     = pcTimeHour p
        , pgsTimeMinute   = pcTimeMinute p
        , pgsDateYear     = pcDateYear p
        , pgsDateMonth    = pcDateMonth p
        , pgsDateDay      = pcDateDay p
        , pgsMapMode      = pcMapMode p
        , pgsIdentity     = pcIdentity p
        , pgsEdits        = emptyWorldEdits
        , pgsMineDesignations      = HM.empty
        , pgsConstructDesignations = HM.empty
        , pgsGroundItems  = emptyGroundItems
        , pgsSpoilPiles   = emptySpoilPiles
        , pgsBuildings    = BuildingSnapshot { bsnInstances = HM.empty, bsnNextId = 0 }
        , pgsUnits        = UnitSnapshot { usnInstances = HM.empty, usnNextId = 0 }
        , pgsUnitSimStates = HM.empty
        , pgsFloraHarvests = emptyFloraHarvests
        , pgsChopDesignations = HM.empty
        , pgsCraftBills   = emptyCraftBills
        , pgsPowerNodes   = emptyPowerNodes
        , pgsTillDesignations = HM.empty
        , pgsCropPlots    = emptyCropPlots
        , pgsPlantDesignations = HM.empty
        }

-- world-edits -------------------------------------------------------

data PageEditsDTO = PageEditsDTO
    { pedPageId ∷ !WorldPageId
    , pedEdits  ∷ !WorldEdits
    } deriving (Show, Generic, Serialize)

newtype WorldEditsDTO = WorldEditsDTO { wedPages ∷ [PageEditsDTO] }
    deriving (Show, Generic, Serialize)

worldEditsCodec ∷ ComponentCodec WorldEditsDTO
worldEditsCodec = serializeCodec
    worldEditsComponentId 1 True [worldPagesComponentId]
    (\snap → WorldEditsDTO
        [ PageEditsDTO (pgsPageId p) (pgsEdits p) | p ← orderedPages snap ])
    (\_ d → Right d) (const [])

applyWorldEdits
    ∷ WorldEditsDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyWorldEdits (WorldEditsDTO slices) =
    applyPageSlices worldEditsComponentId pedPageId
        (\s p → p { pgsEdits = pedEdits s }) slices

-- world-activity ----------------------------------------------------

data PageActivityDTO = PageActivityDTO
    { padPageId        ∷ !WorldPageId
    , padMine          ∷ !MineDesignations
    , padConstruct     ∷ !ConstructDesignations
    , padChop          ∷ !ChopDesignations
    , padTill          ∷ !TillDesignations
    , padPlant         ∷ !PlantDesignations
    , padFloraHarvests ∷ !FloraHarvests
    , padCropPlots     ∷ !CropPlots
    , padGroundItems   ∷ !GroundItems
    , padSpoilPiles    ∷ !SpoilPiles
    } deriving (Show, Generic, Serialize)

newtype WorldActivityDTO = WorldActivityDTO { wadPages ∷ [PageActivityDTO] }
    deriving (Show, Generic, Serialize)

worldActivityCodec ∷ ComponentCodec WorldActivityDTO
worldActivityCodec = serializeCodec
    worldActivityComponentId 1 True [worldPagesComponentId]
    (\snap → WorldActivityDTO (map toActivity (orderedPages snap)))
    (\_ d → Right d) (const [])
  where
    toActivity p = PageActivityDTO
        { padPageId        = pgsPageId p
        , padMine          = pgsMineDesignations p
        , padConstruct     = pgsConstructDesignations p
        , padChop          = pgsChopDesignations p
        , padTill          = pgsTillDesignations p
        , padPlant         = pgsPlantDesignations p
        , padFloraHarvests = pgsFloraHarvests p
        , padCropPlots     = pgsCropPlots p
        , padGroundItems   = pgsGroundItems p
        , padSpoilPiles    = pgsSpoilPiles p
        }

applyWorldActivity
    ∷ WorldActivityDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyWorldActivity (WorldActivityDTO slices) =
    applyPageSlices worldActivityComponentId padPageId writeActivity slices
  where
    writeActivity s p = p
        { pgsMineDesignations      = padMine s
        , pgsConstructDesignations = padConstruct s
        , pgsChopDesignations      = padChop s
        , pgsTillDesignations      = padTill s
        , pgsPlantDesignations     = padPlant s
        , pgsFloraHarvests         = padFloraHarvests s
        , pgsCropPlots             = padCropPlots s
        , pgsGroundItems           = padGroundItems s
        , pgsSpoilPiles            = padSpoilPiles s
        }
