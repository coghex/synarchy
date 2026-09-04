{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | The per-page ATTACHED-SYSTEM registries: the @"craft-bills"@ and
--   @"power-nodes"@ save components (issue #760, save-overhaul B2;
--   extracted from "World.Save.Component.Entities" by #2150).
--
--   - @"craft-bills"@ (required) — per page: the craft-bill queue.
--     Owner: 'Craft.Bills'. Depends on @"world-pages"@ + @"buildings"@
--     (bills reference stations by 'BuildingId'; a demolished station's
--     lingering bill is tolerated gameplay behaviour, so the dependency
--     is for ordering, not a hard orphan reject — see the
--     "World.Save.Snapshot" haddock).
--   - @"power-nodes"@ (required) — per page: the power-node registry
--     (source/storage nodes + a storage node's stored charge). Owner:
--     'Power.Types'. Depends on @"world-pages"@ + @"buildings"@, same
--     tolerated-dangling-reference reasoning as craft bills.
--
--   Both are REGISTRIES attached to a page's buildings rather than
--   entities in their own right, and both carry their own per-page
--   allocator — which is what makes them a pair: they are the only two
--   entity components with a 'csValidate' ('validateCraftBills' /
--   'validatePowerNodes', #1667), each checking its slice's next-id
--   against the ids actually present and each key against its element's
--   embedded id. That shared validation contract is the reason they own
--   one module together instead of two.
--
--   Requirement 4 — the on-disk contract is FROZEN, distinct from every
--   mutable runtime record. The live 'CraftBill'/'CraftBills' and
--   'PowerNode'/'PowerNodes' records are mirrored by
--   'CraftBillDTO'/'BillQueueDTO' and 'PowerNodeDTO'/'NodeRegistryDTO'
--   with explicit field-by-field conversions; neither is embedded
--   directly. Leaf enums ('BillMode'/'PowerRole') and the durable
--   'BillId'/'PowerNodeId'/'BuildingId'/'UnitId' newtypes are reused
--   as-is — append-only content references with no independent mutable
--   identity (boundary rule leaf clause (a), see
--   "World.Save.Component.Types").
module World.Save.Component.EntitySystems
    ( craftBillsCodec
    , PageCraftBillsDTO(..)
    , CraftBillsDTO(..)
    , CraftBillDTO(..)
    , BillQueueDTO(..)
    , CraftBillDTOv1(..)
    , BillQueueDTOv1(..)
    , PageCraftBillsDTOv1(..)
    , CraftBillsDTOv1(..)
    , migrateCraftBillDTOv1
    , migrateCraftBillsDTOv1
    , toCraftBillDTO
    , toBillQueueDTO
    , fromBillQueueDTO
    , validateCraftBills
    , applyCraftBills
    , powerNodesCodec
    , PagePowerNodesDTO(..)
    , PowerNodesDTO(..)
    , PowerNodeDTO(..)
    , NodeRegistryDTO(..)
    , PowerNodeDTOv1(..)
    , NodeRegistryDTOv1(..)
    , PagePowerNodesDTOv1(..)
    , PowerNodesDTOv1(..)
    , migratePowerNodeDTOv1
    , migratePowerNodesDTOv1
    , toNodeRegistryDTO
    , fromNodeRegistryDTO
    , validatePowerNodes
    , applyPowerNodes
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import World.Page.Types (WorldPageId)
import Building.Types (BuildingId)
import Craft.Bills
    ( CraftBills(..), CraftBill(..), BillId(..), BillMode, emptyCraftBills )
import Power.Types
    ( PowerNodes(..), PowerNode(..), PowerNodeId(..), PowerRole
    , emptyPowerNodes )
import Unit.Types (UnitId)
import World.Save.Snapshot (PageSnapshot(..))
import World.Save.Component.Types
import World.Save.Reference (SamePageRef(..))
import World.Save.PageOrder (orderedPages)

-- craft-bills -------------------------------------------------------

-- | Frozen mirror of 'CraftBill' (a mutable runtime record appended to
--   across #329/#590/#795). Reuses the stable 'BillId'/'BuildingId'/
--   'UnitId'/'BillMode' leaf types. Issue #764 (save-overhaul C3):
--   'bilStation'/'bilClaimant' are typed as same-page persistent
--   references ("World.Save.Reference"'s 'SamePageRef') rather than
--   bare ids — a bill's station/claimant are always expected on the
--   SAME page as the bill itself, a fact that used to live only in a
--   comment (see "World.Save.Integrity"'s wrong-page check, which reads
--   this declaration). Bumped this component to v2; v1 decodes via
--   'migrateCraftBillDTOv1' below (requirement 12/14).
data CraftBillDTO = CraftBillDTO
    { bilId         ∷ !BillId
    , bilStation    ∷ !(SamePageRef BuildingId)
    , bilRecipe     ∷ !Text
    , bilRemaining  ∷ !Int
    , bilClaimant   ∷ !(Maybe (SamePageRef UnitId))
    , bilClaimedAt  ∷ !Double
    , bilProgress   ∷ !Float
    , bilSeq        ∷ !Int
    , bilPaused     ∷ !Bool
    , bilWorking    ∷ !Bool
    , bilMode       ∷ !BillMode
    , bilTarget     ∷ !Int
    , bilOutputItem ∷ !Text
    } deriving (Show, Eq, Generic, Serialize)

-- | The FROZEN v1 shape (issue #760), preserved verbatim for decode-only
--   backward compatibility — 'bilStation'/'bilClaimant' here are the
--   original bare ids, exactly as they shipped. Never edited; a further
--   schema change adds a v3 type instead (frozen-DTO boundary rule, see
--   "World.Save.Component.Types").
data CraftBillDTOv1 = CraftBillDTOv1
    { bil1Id         ∷ !BillId
    , bil1Station    ∷ !BuildingId
    , bil1Recipe     ∷ !Text
    , bil1Remaining  ∷ !Int
    , bil1Claimant   ∷ !(Maybe UnitId)
    , bil1ClaimedAt  ∷ !Double
    , bil1Progress   ∷ !Float
    , bil1Seq        ∷ !Int
    , bil1Paused     ∷ !Bool
    , bil1Working    ∷ !Bool
    , bil1Mode       ∷ !BillMode
    , bil1Target     ∷ !Int
    , bil1OutputItem ∷ !Text
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of the 'CraftBills' queue (bills + its embedded id
--   counter).
data BillQueueDTO = BillQueueDTO
    { bqBills  ∷ !(HM.HashMap BillId CraftBillDTO)
    , bqNextId ∷ !Word32
    } deriving (Show, Eq, Generic, Serialize)

data BillQueueDTOv1 = BillQueueDTOv1
    { bq1Bills  ∷ !(HM.HashMap BillId CraftBillDTOv1)
    , bq1NextId ∷ !Word32
    } deriving (Show, Eq, Generic, Serialize)

-- | Translate an unambiguous v1 bill into v2: 'bil1Station' and
--   'bil1Claimant' are already known, by construction, to be same-page
--   references (v1 never carried anything else — every bill's station/
--   claimant has always been resolved against its OWN page, see
--   "World.Load.Stage"), so this is a total, never-ambiguous wrap
--   (requirement 14: "translate only when kind and page can be
--   determined unambiguously from the old component's owning context" —
--   satisfied here since the OLD component's very shape only ever meant
--   one thing).
migrateCraftBillDTOv1 ∷ CraftBillDTOv1 → CraftBillDTO
migrateCraftBillDTOv1 d = CraftBillDTO
    { bilId         = bil1Id d
    , bilStation    = SamePageRef (bil1Station d)
    , bilRecipe     = bil1Recipe d
    , bilRemaining  = bil1Remaining d
    , bilClaimant   = SamePageRef <$> bil1Claimant d
    , bilClaimedAt  = bil1ClaimedAt d
    , bilProgress   = bil1Progress d
    , bilSeq        = bil1Seq d
    , bilPaused     = bil1Paused d
    , bilWorking    = bil1Working d
    , bilMode       = bil1Mode d
    , bilTarget     = bil1Target d
    , bilOutputItem = bil1OutputItem d
    }

toCraftBillDTO ∷ CraftBill → CraftBillDTO
toCraftBillDTO b = CraftBillDTO
    { bilId         = cbId b
    , bilStation    = SamePageRef (cbStation b)
    , bilRecipe     = cbRecipe b
    , bilRemaining  = cbRemaining b
    , bilClaimant   = SamePageRef <$> cbClaimant b
    , bilClaimedAt  = cbClaimedAt b
    , bilProgress   = cbProgress b
    , bilSeq        = cbSeq b
    , bilPaused     = cbPaused b
    , bilWorking    = cbWorking b
    , bilMode       = cbMode b
    , bilTarget     = cbTarget b
    , bilOutputItem = cbOutputItem b
    }

fromCraftBillDTO ∷ CraftBillDTO → CraftBill
fromCraftBillDTO d = CraftBill
    { cbId         = bilId d
    , cbStation    = unSamePageRef (bilStation d)
    , cbRecipe     = bilRecipe d
    , cbRemaining  = bilRemaining d
    , cbClaimant   = unSamePageRef <$> bilClaimant d
    , cbClaimedAt  = bilClaimedAt d
    , cbProgress   = bilProgress d
    , cbSeq        = bilSeq d
    , cbPaused     = bilPaused d
    , cbWorking    = bilWorking d
    , cbMode       = bilMode d
    , cbTarget     = bilTarget d
    , cbOutputItem = bilOutputItem d
    }

toBillQueueDTO ∷ CraftBills → BillQueueDTO
toBillQueueDTO q = BillQueueDTO
    { bqBills = HM.map toCraftBillDTO (cbsBills q), bqNextId = cbsNextId q }

fromBillQueueDTO ∷ BillQueueDTO → CraftBills
fromBillQueueDTO d = CraftBills
    { cbsBills = HM.map fromCraftBillDTO (bqBills d), cbsNextId = bqNextId d }

data PageCraftBillsDTO = PageCraftBillsDTO
    { pcbPageId ∷ !WorldPageId
    , pcbBills  ∷ !BillQueueDTO
    } deriving (Show, Generic, Serialize)

data PageCraftBillsDTOv1 = PageCraftBillsDTOv1
    { pcb1PageId ∷ !WorldPageId
    , pcb1Bills  ∷ !BillQueueDTOv1
    } deriving (Show, Generic, Serialize)

newtype CraftBillsDTO = CraftBillsDTO { cbdPages ∷ [PageCraftBillsDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

newtype CraftBillsDTOv1 = CraftBillsDTOv1 { cbd1Pages ∷ [PageCraftBillsDTOv1] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

migrateCraftBillsDTOv1 ∷ CraftBillsDTOv1 → CraftBillsDTO
migrateCraftBillsDTOv1 (CraftBillsDTOv1 ps) = CraftBillsDTO
    [ PageCraftBillsDTO (pcb1PageId p)
        (BillQueueDTO (HM.map migrateCraftBillDTOv1 (bq1Bills (pcb1Bills p)))
                      (bq1NextId (pcb1Bills p)))
    | p ← ps ]

-- | Component-local invariant (#760, mirrors
--   "World.Save.Component.PageCore"'s @worldPagesCodec@ @validatePages@
--   precedent): every bill's own id must sit below that PAGE's queue
--   allocator ('bqNextId') — 'BillId' is allocated per-page (see
--   'Craft.Bills.emptyCraftBills'), unlike the item/building/unit
--   allocators, which are global. A literal duplicate key within one
--   page's @bqBills@ map is structurally impossible once decoded (a
--   'HashMap' cannot carry two entries under the same key), so there is
--   nothing further to check there.
--
--   A companion check covers what the allocator check alone misses:
--   the map KEY and the DTO's own embedded 'bilId' are two independent
--   copies of the same identity (mirrored from live 'CraftBill'/
--   'CraftBills', which always keeps them in sync by construction — but
--   a decoded-from-disk envelope has no such guarantee). A hand-crafted
--   or corrupted envelope could carry @bqBills = {#1 -> bill{bilId=#2}}@
--   and the allocator check alone would accept it (both #1 and #2 sit
--   below the allocator), yet runtime APIs (which key off BOTH the
--   registry's map key via 'Craft.Bills.claimBill'/'releaseBill' AND the
--   bill's own 'cbId' field) would then disagree about which bill this
--   is. Reject any entry where the two disagree.
--
--   Deliberately OUT OF SCOPE here (this component-local validator sees
--   only ONE component's own DTO, never the assembled cross-component
--   picture): a dangling 'cbStation'/'cbClaimant' reference (a station
--   'BuildingId'/claimant 'UnitId' absent from the WHOLE session, not
--   just this page) is NOT hard-validated here, and never rejects the
--   load on that ground alone. This is not an oversight — it would
--   contradict an existing, deliberate design decision from #758:
--   "World.Save.Snapshot" (~line 199-207) documents that a demolished
--   station leaving its bills "lingering, visible + cancellable" is
--   tolerated gameplay behaviour, not corruption, and hard-failing on
--   it would reject otherwise-valid saves. "World.Load.Stage" restores
--   bills/nodes VERBATIM (never prunes them, issue #763) —
--   exactly the "do not drop the source record" contract requirement 11
--   asks for. Issue #764 (save-overhaul C3) adds the cross-component
--   check this component-local validator structurally cannot:
--   "World.Save.Integrity".'World.Save.Integrity.sessionIntegrityErrors'
--   runs once the WHOLE session is assembled and DOES hard-reject a
--   station/claimant that resolves on a DIFFERENT page than the bill
--   itself (a genuine wrong-page violation, never legitimate) while
--   still tolerating one absent from the entire session.
--
--   #1680 does not change any of that, and specifically does not make a
--   dangling 'cbClaimant' a load-time rejection: the restore stays
--   verbatim, and the repair happens afterwards, in LIVE state.
--   'World.Thread.CraftBills.tickCraftBillOwners' runs on every loaded
--   page and independently of the pause flag, so the first ordinary
--   world tick after 'World.Load.Publish' — which brings a session up
--   paused — drops the claim and its 'cbWorking' flag, and the station
--   stops drawing the recipe's wattage for a worker the save no longer
--   contains.
--
--   #1667: the page's OWN allocator floor is a separate clause, so it
--   is checked even when the map it guards is empty — an empty map used
--   to certify any cursor, 0 included, which the live allocator would
--   then hand out as a real id against the "never 0" convention
--   'emptyCraftBills'\/'emptyPowerNodes' establish.
--   'World.Save.Component.Transfer.validateTransferOrders' is the
--   precedent; this is the same check, generalized.
validateCraftBills ∷ CraftBillsDTO → [ComponentError]
validateCraftBills (CraftBillsDTO slices) = concat
    [ [ ComponentError craftBillsComponentId 2 ValidatePhase
          ("page '" <> tshow (pcbPageId s) <> "': craft-bill allocator \
             \is " <> tshow (bqNextId (pcbBills s)) <> ", below the \
             \first valid bill id (" <> tshow firstBillId <> ")")
      | s ← slices
      , bqNextId (pcbBills s) < firstBillId
      ]
    , [ ComponentError craftBillsComponentId 2 ValidatePhase
          ("page '" <> tshow (pcbPageId s) <> "': bill #"
           <> tshow (unBillId bid) <> " is not below the page's bill \
              \allocator (" <> tshow (bqNextId (pcbBills s)) <> ")")
      | s   ← slices
      , bid ← HM.keys (bqBills (pcbBills s))
      , unBillId bid ≥ bqNextId (pcbBills s)
      ]
    , [ ComponentError craftBillsComponentId 2 ValidatePhase
          ("page '" <> tshow (pcbPageId s) <> "': bill map key #"
           <> tshow (unBillId k) <> " holds a bill whose own id is #"
           <> tshow (unBillId (bilId v)))
      | s      ← slices
      , (k, v) ← HM.toList (bqBills (pcbBills s))
      , k ≢ bilId v
      ]
    ]
  where
    -- The floor IS what a fresh allocator starts at, read from
    -- 'emptyCraftBills' itself so the two cannot drift.
    firstBillId = cbsNextId emptyCraftBills

-- | Issue #764 (save-overhaul C3): the current schema is v2 (typed
--   'SamePageRef' station/claimant, see 'CraftBillDTO'); v1 decodes
--   through its own frozen 'CraftBillsDTOv1' via
--   'migrateCraftBillsDTOv1', and encoding always writes the current v2
--   shape. This was the FIRST component to actually need multi-version
--   decode, and hand-rolled the whole record because 'componentCodec's
--   predecessor only ever accepted its own current version; issue #1093
--   gave the shared construction that dispatch, so this is expressed
--   through it again.
craftBillsCodec ∷ ComponentCodec CraftBillsDTO
craftBillsCodec = componentCodec ComponentSpec
    { csComponent     = craftBillsComponentId
    , csVersion       = 2
    , csRequired      = True
    , csDeps          = [worldPagesComponentId, buildingsComponentId]
    , csEncode        = \snap → CraftBillsDTO
        [ PageCraftBillsDTO (pgsPageId p) (toBillQueueDTO (pgsCraftBills p))
        | p ← orderedPages snap ]
    , csDecode        = id
    , csOlderVersions = [ atVersion 1 migrateCraftBillsDTOv1 ]
    , csValidate      = validateCraftBills
    }

applyCraftBills
    ∷ Word32 → CraftBillsDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyCraftBills ver (CraftBillsDTO slices) =
    applyPageSlices craftBillsComponentId ver pcbPageId
        (\s p → p { pgsCraftBills = fromBillQueueDTO (pcbBills s) }) slices

-- power-nodes -------------------------------------------------------

-- | Frozen mirror of 'PowerNode' (a mutable runtime record appended to
--   across #358/#360). Reuses the stable 'PowerNodeId'/'BuildingId'/
--   'PowerRole' leaf types. Issue #764 (save-overhaul C3): 'nodBuilding'
--   is typed as a same-page persistent reference ("World.Save.Reference"'s
--   'SamePageRef') rather than a bare id — a node's host building is
--   always expected on the SAME page as the node itself (see
--   'CraftBillDTO''s identical reasoning). Bumped this component to v2;
--   v1 decodes via 'migratePowerNodeDTOv1' below.
data PowerNodeDTO = PowerNodeDTO
    { nodId         ∷ !PowerNodeId
    , nodBuilding   ∷ !(SamePageRef BuildingId)
    , nodRole       ∷ !PowerRole
    , nodPeakWatts  ∷ !Float
    , nodCapacityWh ∷ !Float
    , nodStoredWh   ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

-- | The FROZEN v1 shape (issue #760), preserved verbatim for decode-only
--   backward compatibility.
data PowerNodeDTOv1 = PowerNodeDTOv1
    { nod1Id         ∷ !PowerNodeId
    , nod1Building   ∷ !BuildingId
    , nod1Role       ∷ !PowerRole
    , nod1PeakWatts  ∷ !Float
    , nod1CapacityWh ∷ !Float
    , nod1StoredWh   ∷ !Float
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of the 'PowerNodes' registry (nodes + its embedded id
--   counter).
data NodeRegistryDTO = NodeRegistryDTO
    { regNodes  ∷ !(HM.HashMap PowerNodeId PowerNodeDTO)
    , regNextId ∷ !Word32
    } deriving (Show, Eq, Generic, Serialize)

data NodeRegistryDTOv1 = NodeRegistryDTOv1
    { reg1Nodes  ∷ !(HM.HashMap PowerNodeId PowerNodeDTOv1)
    , reg1NextId ∷ !Word32
    } deriving (Show, Eq, Generic, Serialize)

-- | Unambiguous v1→v2 translation, same reasoning as
--   'migrateCraftBillDTOv1': v1's 'nod1Building' has always meant "the
--   host building on THIS node's own page" (see "World.Load.Stage"), so
--   wrapping it in 'SamePageRef' never guesses (requirement 14).
migratePowerNodeDTOv1 ∷ PowerNodeDTOv1 → PowerNodeDTO
migratePowerNodeDTOv1 d = PowerNodeDTO
    { nodId         = nod1Id d
    , nodBuilding   = SamePageRef (nod1Building d)
    , nodRole       = nod1Role d
    , nodPeakWatts  = nod1PeakWatts d
    , nodCapacityWh = nod1CapacityWh d
    , nodStoredWh   = nod1StoredWh d
    }

toPowerNodeDTO ∷ PowerNode → PowerNodeDTO
toPowerNodeDTO n = PowerNodeDTO
    { nodId         = pnId n
    , nodBuilding   = SamePageRef (pnBuilding n)
    , nodRole       = pnRole n
    , nodPeakWatts  = pnPeakWatts n
    , nodCapacityWh = pnCapacityWh n
    , nodStoredWh   = pnStoredWh n
    }

fromPowerNodeDTO ∷ PowerNodeDTO → PowerNode
fromPowerNodeDTO d = PowerNode
    { pnId         = nodId d
    , pnBuilding   = unSamePageRef (nodBuilding d)
    , pnRole       = nodRole d
    , pnPeakWatts  = nodPeakWatts d
    , pnCapacityWh = nodCapacityWh d
    , pnStoredWh   = nodStoredWh d
    }

toNodeRegistryDTO ∷ PowerNodes → NodeRegistryDTO
toNodeRegistryDTO r = NodeRegistryDTO
    { regNodes = HM.map toPowerNodeDTO (pnsNodes r), regNextId = pnsNextId r }

fromNodeRegistryDTO ∷ NodeRegistryDTO → PowerNodes
fromNodeRegistryDTO d = PowerNodes
    { pnsNodes = HM.map fromPowerNodeDTO (regNodes d), pnsNextId = regNextId d }

migrateNodeRegistryDTOv1 ∷ NodeRegistryDTOv1 → NodeRegistryDTO
migrateNodeRegistryDTOv1 d = NodeRegistryDTO
    { regNodes = HM.map migratePowerNodeDTOv1 (reg1Nodes d)
    , regNextId = reg1NextId d }

data PagePowerNodesDTO = PagePowerNodesDTO
    { ppnPageId ∷ !WorldPageId
    , ppnNodes  ∷ !NodeRegistryDTO
    } deriving (Show, Generic, Serialize)

data PagePowerNodesDTOv1 = PagePowerNodesDTOv1
    { ppn1PageId ∷ !WorldPageId
    , ppn1Nodes  ∷ !NodeRegistryDTOv1
    } deriving (Show, Generic, Serialize)

newtype PowerNodesDTO = PowerNodesDTO { pndPages ∷ [PagePowerNodesDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

newtype PowerNodesDTOv1 = PowerNodesDTOv1 { pnd1Pages ∷ [PagePowerNodesDTOv1] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

migratePowerNodesDTOv1 ∷ PowerNodesDTOv1 → PowerNodesDTO
migratePowerNodesDTOv1 (PowerNodesDTOv1 ps) = PowerNodesDTO
    [ PagePowerNodesDTO (ppn1PageId p) (migrateNodeRegistryDTOv1 (ppn1Nodes p))
    | p ← ps ]

-- | Component-local invariant (#760), same shape as
--   'validateCraftBills': every node's own id must sit below that
--   page's node-registry allocator ('regNextId') — 'PowerNodeId' is
--   allocated per-page (see 'Power.Types.emptyPowerNodes'). A literal
--   duplicate key within one page's @regNodes@ map is structurally
--   impossible once decoded, same reasoning as bills.
--
--   The same key/value identity check 'validateCraftBills' applies
--   here: the map key and the DTO's own embedded 'nodId' must agree —
--   a decoded envelope with @regNodes = {#1 -> node{nodId=#2}}@ would
--   otherwise pass the allocator check yet leave runtime APIs (which key
--   off both identities) disagreeing about which node this is.
--
--   Deliberately OUT OF SCOPE here (same component-local-vs-cross-
--   component reasoning as 'validateCraftBills' above): a dangling
--   'pnBuilding' reference (a host building absent from the WHOLE
--   session) is NOT hard-validated here. See "World.Save.Snapshot"
--   (~line 199-207) and "World.Save.Integrity"'s
--   'World.Save.Integrity.sessionIntegrityErrors' (issue #764) — the
--   latter DOES hard-reject a host building that resolves on a
--   DIFFERENT page than the node itself, once the whole session is
--   assembled and cross-component checking is actually possible.
--
--   #1667: the page's OWN allocator floor is a separate clause, so it
--   is checked even when the map it guards is empty — an empty map used
--   to certify any cursor, 0 included, which the live allocator would
--   then hand out as a real id against the "never 0" convention
--   'emptyCraftBills'\/'emptyPowerNodes' establish.
--   'World.Save.Component.Transfer.validateTransferOrders' is the
--   precedent; this is the same check, generalized.
validatePowerNodes ∷ PowerNodesDTO → [ComponentError]
validatePowerNodes (PowerNodesDTO slices) = concat
    [ [ ComponentError powerNodesComponentId 2 ValidatePhase
          ("page '" <> tshow (ppnPageId s) <> "': power-node allocator \
             \is " <> tshow (regNextId (ppnNodes s)) <> ", below the \
             \first valid node id (" <> tshow firstPowerNodeId <> ")")
      | s ← slices
      , regNextId (ppnNodes s) < firstPowerNodeId
      ]
    , [ ComponentError powerNodesComponentId 2 ValidatePhase
          ("page '" <> tshow (ppnPageId s) <> "': power node #"
           <> tshow (unPowerNodeId nid) <> " is not below the page's node \
              \allocator (" <> tshow (regNextId (ppnNodes s)) <> ")")
      | s   ← slices
      , nid ← HM.keys (regNodes (ppnNodes s))
      , unPowerNodeId nid ≥ regNextId (ppnNodes s)
      ]
    , [ ComponentError powerNodesComponentId 2 ValidatePhase
          ("page '" <> tshow (ppnPageId s) <> "': power node map key #"
           <> tshow (unPowerNodeId k) <> " holds a node whose own id is #"
           <> tshow (unPowerNodeId (nodId v)))
      | s      ← slices
      , (k, v) ← HM.toList (regNodes (ppnNodes s))
      , k ≢ nodId v
      ]
    ]
  where
    -- Same tie as 'validateCraftBills': the floor is read from
    -- 'emptyPowerNodes' rather than restated as a literal.
    firstPowerNodeId = pnsNextId emptyPowerNodes

-- | Same reasoning as 'craftBillsCodec'. Current schema is v2 (typed
--   'SamePageRef' host building, see 'PowerNodeDTO'); v1 decodes through
--   its own frozen 'PowerNodesDTOv1' via 'migratePowerNodesDTOv1'.
powerNodesCodec ∷ ComponentCodec PowerNodesDTO
powerNodesCodec = componentCodec ComponentSpec
    { csComponent     = powerNodesComponentId
    , csVersion       = 2
    , csRequired      = True
    , csDeps          = [worldPagesComponentId, buildingsComponentId]
    , csEncode        = \snap → PowerNodesDTO
        [ PagePowerNodesDTO (pgsPageId p) (toNodeRegistryDTO (pgsPowerNodes p))
        | p ← orderedPages snap ]
    , csDecode        = id
    , csOlderVersions = [ atVersion 1 migratePowerNodesDTOv1 ]
    , csValidate      = validatePowerNodes
    }

applyPowerNodes
    ∷ Word32 → PowerNodesDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyPowerNodes ver (PowerNodesDTO slices) =
    applyPageSlices powerNodesComponentId ver ppnPageId
        (\s p → p { pgsPowerNodes = fromNodeRegistryDTO (ppnNodes s) }) slices
