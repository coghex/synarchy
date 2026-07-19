{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | The bridge from 'SessionSnapshot' to the legacy positional
--   'SaveData'/'WorldPageSave' shape (#758, requirement 12). Originally
--   (#759, B1) this was also the ON-DISK wire format; since #760 (B2)
--   the disk format is the tagged component envelope
--   ("World.Save.Envelope", "World.Save.Component") and this module is
--   ONLY the in-memory LOAD-side bridge — 'World.Save.Serialize's
--   decode path reconstructs a 'SessionSnapshot' from the envelope's
--   components, then calls 'snapshotToSaveData' here to hand the
--   world-thread load path ('World.Load.Stage' etc.)
--   the legacy shape it still consumes. Nothing here encodes to disk
--   any more; 'World.Save.Types.currentSaveVersion' now only versions
--   this in-memory bridge shape, not a wire contract.
--
--   The adapter never reads live engine state — its only inputs are
--   the already-captured, already-validated 'SessionSnapshot' plus
--   request metadata that was never part of gameplay state to begin
--   with (requirement 11). Because the legacy schema has fields this
--   snapshot deliberately excludes (per-page time scale, tool mode) or
--   duplicates per page (the global camera zoom/facing, the building/
--   unit id allocators), this module is the one place allowed to
--   FABRICATE a v88-only field from a load-policy default, or
--   duplicate a genuinely global snapshot value into every legacy
--   per-page slot — never a re-read of live state, only a
--   transformation of already-captured values.
module World.Save.Snapshot.Adapter
    ( SaveRequestMeta(..)
    , snapshotToSaveData
    , snapshotSaveMetadata
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import World.Save.Snapshot
import World.Save.Types
import World.Generate.Types (WorldGenParams(..))
import World.Page.Types (WorldIdentity(..))
import World.Tool.Types (ToolMode(..))

-- | Everything about a save that is request/storage metadata, not
--   gameplay state (requirement 11) — the slot name and request
--   timestamp are supplied by the caller (`engine.saveWorld`'s Lua
--   verb), never derived from the snapshot, and never enter any
--   snapshot structural-equivalence comparison.
data SaveRequestMeta = SaveRequestMeta
    { srmSlotName  ∷ !Text
    , srmTimestamp ∷ !Text
    } deriving (Show, Eq)

-- | The active page, resolved the same defensively-falls-back-to-any-
--   page way 'World.Save.Types.activeWorldPage' already does for
--   'SaveData' — 'Nothing' only for a snapshot with no pages at all,
--   which 'captureSessionSnapshot' already refuses to produce.
resolvedActivePage ∷ SessionSnapshot → Maybe PageSnapshot
resolvedActivePage snap = case HM.lookup (snapActivePage snap) pages of
    Just p  → Just p
    Nothing → case HM.elems pages of
        (p:_) → Just p
        []    → Nothing
  where
    pages = snapPages snap

-- | Encode an already-captured, already-validated snapshot into the
--   legacy 'SaveData' shape. Pure — no IO, no live-state re-read.
-- | The listing 'SaveMetadata' the @"metadata"@ component carries.
--   Derived from the active page's authoritative gen params + identity
--   (so it can never disagree with the gameplay components on load —
--   requirement 12) plus the caller-supplied slot name + timestamp
--   (request metadata, never gameplay state — requirement 11). Shared by
--   the save-encode path ("World.Save.Envelope.encodeSessionSnapshot")
--   and the load bridge below, so both agree on exactly what the
--   metadata component says.
snapshotSaveMetadata ∷ SaveRequestMeta → SessionSnapshot → SaveMetadata
snapshotSaveMetadata req snap = SaveMetadata
    { smName       = srmSlotName req
    , smSeed       = maybe 0 (wgpSeed ∘ pgsGenParams) mActive
    , smWorldSize  = maybe 0 (wgpWorldSize ∘ pgsGenParams) mActive
    , smPlateCount = maybe 0 (wgpPlateCount ∘ pgsGenParams) mActive
    , smTimestamp  = srmTimestamp req
    , smWorldName  = mActive ⌦ (\p → wiName ⊚ pgsIdentity p)
    , smWorldGloss = mActive ⌦ (\p → pgsIdentity p ⌦ wiGloss)
    }
  where mActive = resolvedActivePage snap

snapshotToSaveData ∷ SaveRequestMeta → SessionSnapshot → SaveData
snapshotToSaveData req snap = SaveData
    { sdMetadata = snapshotSaveMetadata req snap
    , sdGameTime           = snapGameTime snap
      -- Always True: a captured snapshot is understood to load paused
      -- (contract requirement 4), never the toggle's live value.
    , sdEnginePaused       = True
    , sdTexPalette         = snapTexPalette snap
    , sdNextItemInstanceId = snapNextItemId snap
    , sdActivePage         = snapActivePage snap
    , sdVisiblePages       = snapVisiblePages snap
    , sdWorlds             = map (pageToWorldPageSave (snapLiveCamera snap)
                                     (snapNextBuildingId snap)
                                     (snapNextUnitId snap))
                                 (HM.elems (snapPages snap))
    }

-- | One page's legacy 'WorldPageSave'. Camera position uses the live
--   camera's values for whichever page it's attributed to and this
--   page's own stored position otherwise; zoom/facing are the SAME
--   global live-camera value on every page, matching v88's existing
--   behaviour exactly (there is no per-page zoom/facing to fall back
--   to) — but now an explicit adapter decision instead of an implicit
--   side effect of the old capture code's shared @cam@ variable.
--
--   @nextBid@/@nextUid@ are the snapshot's own CANONICAL, validated
--   allocators (review round 1): 'pgsBuildings'/'pgsUnits' carry their
--   own embedded 'bsnNextId'/'usnNextId' (an artifact of reusing
--   'BuildingSnapshot'/'UnitSnapshot' as-is), but the adapter must
--   never trust those over the snapshot's own fields — passing them
--   through unchanged would let a page-local counter that happens to
--   differ from (in particular, sit BELOW) the validated session-wide
--   allocator silently defeat 'buildingAllocatorErrors'/
--   'unitAllocatorErrors' by persisting a lower allocator than what
--   was actually validated, permitting id reuse after load.
pageToWorldPageSave ∷ LiveCameraSnapshot → Word32 → Word32 → PageSnapshot
                    → WorldPageSave
pageToWorldPageSave cam nextBid nextUid page = WorldPageSave
    { wpsPageId       = pgsPageId page
    , wpsGenParams    = pgsGenParams page
    , wpsCameraX      = cx
    , wpsCameraY      = cy
    , wpsCameraZoom   = lcsZoom cam
    , wpsCameraFacing = lcsFacing cam
    , wpsTimeHour     = pgsTimeHour page
    , wpsTimeMinute   = pgsTimeMinute page
    , wpsDateYear     = pgsDateYear page
    , wpsDateMonth    = pgsDateMonth page
    , wpsDateDay      = pgsDateDay page
      -- Fabricated (requirement 12/#795-style adapter latitude): a
      -- loaded session never restores a pre-save speed, so there is no
      -- snapshot value to carry here at all — only a fixed
      -- decode-compatible default (contract requirement 4).
    , wpsTimeScale    = 1
    , wpsMapMode      = pgsMapMode page
      -- Fabricated: reset-to-default, matching #103's existing load
      -- behaviour (the field is already ignored at load time).
    , wpsToolMode     = DefaultTool
    , wpsEdits        = pgsEdits page
    , wpsMineDesignations      = pgsMineDesignations page
    , wpsConstructDesignations = pgsConstructDesignations page
    , wpsGroundItems  = pgsGroundItems page
    , wpsSpoilPiles   = pgsSpoilPiles page
    , wpsBuildings    = (pgsBuildings page) { bsnNextId = nextBid }
    , wpsUnits        = (pgsUnits page) { usnNextId = nextUid }
    , wpsUnitSimStates = pgsUnitSimStates page
    , wpsFloraHarvests = pgsFloraHarvests page
    , wpsChopDesignations = pgsChopDesignations page
    , wpsCraftBills   = pgsCraftBills page
    , wpsPowerNodes   = pgsPowerNodes page
    , wpsTillDesignations = pgsTillDesignations page
    , wpsCropPlots    = pgsCropPlots page
    , wpsPlantDesignations = pgsPlantDesignations page
    , wpsIdentity     = pgsIdentity page
    }
  where
    isOwner = lcsOwnerPage cam ≡ Just (pgsPageId page)
    (cx, cy) = if isOwner then (lcsX cam, lcsY cam)
                          else (pgsCameraX page, pgsCameraY page)
