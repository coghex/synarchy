{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | The @"world-edits"@ owner (issue #760, save-overhaul B2; split out
--   of "World.Save.Component.Page" by #2135).
--
--   @"world-edits"@ (required) — per page: the terrain + structure edit
--   log. Owner: the world edit layer. Boundary reason: player terrain/
--   structure modifications are a distinct, replay-on-load concern.
--   Its slice list is encoded in the canonical (page-id ascending) order
--   'World.Save.Component.PageCore.orderedPages' establishes, so
--   identical input produces identical bytes (requirement 10).
--
--   The only cross-owner import is that ordering helper. This owner
--   never imports "World.Save.Component.PageActivity" — its sibling —
--   nor the "World.Save.Component.Page" façade; the shared apply
--   scaffolding both siblings run their slices through
--   ('World.Save.Component.Types.applyPageSlices') already lives one
--   level down, in the module both already depend on.
--
--   Requirement 4 — the on-disk contract is FROZEN, distinct from every
--   mutable runtime record; see "World.Save.Component.Page" for the
--   page-scoped statement of that rule. The live record this owner
--   mirrors is:
--
--   - 'WorldEdit'           → 'WorldEditDTO' (its own frozen tag order,
--                             decoupled from the live sum's constructor
--                             order, so REORDERING the live type can no
--                             longer silently corrupt v1 bytes; the
--                             pre-#2243 shape stays as 'WorldEditDTOv2'
--                             and the pre-#1854 one as 'WorldEditDTOv1')
--
--   Its leaf payload references — 'FluidType', 'MaterialId', 'FloraId',
--   the durable authored 'World.Flora.Reference.FloraRef', and the
--   opaque 'World.Flora.Identity.FloraInstanceId' — are reused as-is
--   rather than mirrored, per the frozen-DTO boundary rule stated in
--   "World.Save.Component.Types".
module World.Save.Component.PageEdits
    ( -- * The frozen edit sum
      WorldEditDTO(..)
    , WorldEditDTOv1(..)
    , WorldEditDTOv2(..)
      -- * The @"world-edits"@ wire shapes
    , PageEditsDTO(..)
    , WorldEditsDTO(..)
    , PageEditsDTOv1(..)
    , WorldEditsDTOv1(..)
    , PageEditsDTOv2(..)
    , WorldEditsDTOv2(..)
      -- * The component
    , worldEditsCodec
    , validateWorldEdits
    , applyWorldEdits
    , migrateWorldEditsV1
    , migrateWorldEditsV2
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import World.Generate.Types (WorldGenParams(..))
import World.Chunk.Types (ChunkCoord(..), wrapChunkCoordU)
import World.Page.Types (WorldPageId)
import World.Edit.Types (WorldEdit(..), WorldEdits)
import World.Fluid.Types (FluidType)
import World.Material.Id (MaterialId)
import World.Flora.Types (FloraId)
import World.Flora.Reference (FloraRef(..))
import World.Flora.Identity
    ( FloraInstanceId, floraInstanceIdToLua, floraInstanceIdNone
    , isFloraInstanceIdNone, isPlantedFloraInstanceId
    , firstPlantedFloraCursor, nextPlantedFloraCursor
    , plantedFloraCursorAbove )
import World.Save.Snapshot (PageSnapshot(..))
import World.Save.Component.PageCore (orderedPages)
import World.Save.Component.Types

-- | Frozen mirror of 'WorldEdit'. Its OWN constructor order is the wire
--   contract, decoupled from the live sum's — so reordering the live
--   constructors (which the live type's own append-only comment warns
--   against) can no longer silently corrupt a shipped v1 save. Adding a
--   live constructor makes 'toWorldEditDTO' non-exhaustive (a compile
--   warning under @-Werror@), forcing a conscious DTO extension. Leaf
--   payload references ('FluidType'/'MaterialId'/'FloraId') are reused.
data WorldEditDTO
    = WeDeleteTileD !Int !Int
    | WeSetFluidTileD !Int !Int !FluidType
    | WeAddTileD !Int !Int !MaterialId
    | WeSetSlopeD !Int !Int !Int !Word8
    | WeSetCellD !Int !Int !Int !MaterialId
    | WeSetStructureD !Int !Int !Word8 !Int !Int !Int
    | WeClearStructureD !Int !Int !Word8
    | WeSetVegD !Int !Int !Int !Word8
    | WePlaceFloraD !Int !Int !FloraId !Int !Float
    | WeSetFluidSnapshotD !Int !Int !FluidType !Int
    | WeClearFluidSnapshotD !Int !Int
      -- #1854: appended at the END, never grown in place. This sum is
      -- positionally serialized, so a sixth field on @WePlaceFloraD@
      -- would have reinterpreted tag 8 in every shipped v1 log; a new
      -- trailing constructor leaves all eleven older tags meaning exactly
      -- what they meant (tools/enum_append_only_audit.py).
    | WePlaceFloraWithIdD !Int !Int !FloraId !Int !Float !FloraInstanceId
      -- #2243: appended at the END on exactly the same terms. The
      -- species is now a durable 'FloraRef' — an authored name — so the
      -- two numeric constructors above became decode-only the day this
      -- one landed, and @world-edits@ v3 payloads carry only this tag
      -- for a planted crop. Retyping @WePlaceFloraWithIdD@'s 'FloraId'
      -- slot in place would have reinterpreted tag 11 in every shipped
      -- v2 log (tools/enum_append_only_audit.py records each
      -- constructor's payload, not just its name).
    | WePlaceFloraRefD !Int !Int !FloraRef !Int !Float !FloraInstanceId
    deriving (Show, Eq, Generic, Serialize)

-- | The FROZEN pre-#2243 edit sum (@world-edits@ v2), preserved
--   verbatim for decode-only backward compatibility. Byte-compatible
--   with 'WorldEditDTO' for every tag it declares — an append cannot
--   move an earlier one — but frozen anyway, on the discipline
--   'WorldEditDTOv1' established: a LATER append to the live sum then
--   cannot reach a v2 payload, and the shape a shipped v2 log was
--   written with stays readable in one place instead of being inferred
--   from the live type's history.
data WorldEditDTOv2
    = WeDeleteTileDv2 !Int !Int
    | WeSetFluidTileDv2 !Int !Int !FluidType
    | WeAddTileDv2 !Int !Int !MaterialId
    | WeSetSlopeDv2 !Int !Int !Int !Word8
    | WeSetCellDv2 !Int !Int !Int !MaterialId
    | WeSetStructureDv2 !Int !Int !Word8 !Int !Int !Int
    | WeClearStructureDv2 !Int !Int !Word8
    | WeSetVegDv2 !Int !Int !Int !Word8
    | WePlaceFloraDv2 !Int !Int !FloraId !Int !Float
    | WeSetFluidSnapshotDv2 !Int !Int !FluidType !Int
    | WeClearFluidSnapshotDv2 !Int !Int
    | WePlaceFloraWithIdDv2 !Int !Int !FloraId !Int !Float !FloraInstanceId
    deriving (Show, Eq, Generic, Serialize)

-- | The FROZEN pre-#1854 edit sum, preserved verbatim for decode-only
--   backward compatibility (@world-edits@ v1, and the v90 compatibility
--   tree). #1854 appended a 'FloraInstanceId' to @WePlaceFloraD@'s
--   PAYLOAD — which moves no constructor tag but changes that
--   constructor's bytes, so a shipped v1 log must decode through this
--   copy of the old shape rather than the live one.
--
--   Its constructor ORDER is a wire contract of its own and is never
--   touched; a new live constructor is added to 'WorldEditDTO' above,
--   which makes 'migrateWorldEditDTOv1' non-exhaustive in the safe
--   direction (a compile error here, never silent drift on disk).
data WorldEditDTOv1
    = WeDeleteTileDv1 !Int !Int
    | WeSetFluidTileDv1 !Int !Int !FluidType
    | WeAddTileDv1 !Int !Int !MaterialId
    | WeSetSlopeDv1 !Int !Int !Int !Word8
    | WeSetCellDv1 !Int !Int !Int !MaterialId
    | WeSetStructureDv1 !Int !Int !Word8 !Int !Int !Int
    | WeClearStructureDv1 !Int !Int !Word8
    | WeSetVegDv1 !Int !Int !Int !Word8
    | WePlaceFloraDv1 !Int !Int !FloraId !Int !Float
    | WeSetFluidSnapshotDv1 !Int !Int !FluidType !Int
    | WeClearFluidSnapshotDv1 !Int !Int
    deriving (Show, Eq, Generic, Serialize)

-- | v1 → current. Every constructor crosses unchanged except the
--   planted-flora one, which crosses into the id-LESS @WePlaceFloraD@
--   it has always been: a v1 log records no ids, and there is nothing in
--   its bytes to recover one from. 'applyWorldEdits' rewrites each into
--   the identity-bearing constructor once the page's own world size is
--   in hand (see its note) — the same "repair lives in ONE place, at
--   apply time" rule #1175's canonical-frame repair follows.
migrateWorldEditDTOv1 ∷ WorldEditDTOv1 → WorldEditDTOv2
migrateWorldEditDTOv1 e = case e of
    WeDeleteTileDv1 a b            → WeDeleteTileDv2 a b
    WeSetFluidTileDv1 a b f        → WeSetFluidTileDv2 a b f
    WeAddTileDv1 a b m             → WeAddTileDv2 a b m
    WeSetSlopeDv1 a b c w          → WeSetSlopeDv2 a b c w
    WeSetCellDv1 a b c m           → WeSetCellDv2 a b c m
    WeSetStructureDv1 a b w c d f  → WeSetStructureDv2 a b w c d f
    WeClearStructureDv1 a b w      → WeClearStructureDv2 a b w
    WeSetVegDv1 a b c w            → WeSetVegDv2 a b c w
    WePlaceFloraDv1 a b fl d fx    → WePlaceFloraDv2 a b fl d fx
    WeSetFluidSnapshotDv1 a b f z  → WeSetFluidSnapshotDv2 a b f z
    WeClearFluidSnapshotDv1 a b    → WeClearFluidSnapshotDv2 a b

-- | v2 → current (#2243). Every constructor crosses unchanged except
--   the two planting ones, which cross into the named 'WePlaceFloraRefD'
--   carrying 'FloraByLegacyId': a pre-#2243 log records an ORDINAL, and
--   there is nothing in its bytes and nothing available to a pure
--   component migration ('atVersion' sees only the decoded payload) that
--   could turn one into a name. The catalog that could is read at the
--   load boundary — 'World.Save.Types.missingFloraReferences' refuses
--   the load if the ordinal resolves to nothing there, and
--   'World.Load.Stage.stagePage' resolves the rest — which is the same
--   "the repair that needs outside context happens later, in ONE place"
--   rule 'migrateWorldEditsV1' already follows for #1854's ids.
--
--   The id-LESS v1 form keeps crossing into the id-less state it has
--   always had: 'applyWorldEdits' allocates its 'FloraInstanceId' below,
--   and does so for the named constructor exactly as it did for
--   'WePlaceFloraWithIdD'.
migrateWorldEditDTOv2 ∷ WorldEditDTOv2 → WorldEditDTO
migrateWorldEditDTOv2 e = case e of
    WeDeleteTileDv2 a b            → WeDeleteTileD a b
    WeSetFluidTileDv2 a b f        → WeSetFluidTileD a b f
    WeAddTileDv2 a b m             → WeAddTileD a b m
    WeSetSlopeDv2 a b c w          → WeSetSlopeD a b c w
    WeSetCellDv2 a b c m           → WeSetCellD a b c m
    WeSetStructureDv2 a b w c d f  → WeSetStructureD a b w c d f
    WeClearStructureDv2 a b w      → WeClearStructureD a b w
    WeSetVegDv2 a b c w            → WeSetVegD a b c w
    WePlaceFloraDv2 a b fl d fx    →
        WePlaceFloraRefD a b (FloraByLegacyId fl) d fx floraInstanceIdNone
    WeSetFluidSnapshotDv2 a b f z  → WeSetFluidSnapshotD a b f z
    WeClearFluidSnapshotDv2 a b    → WeClearFluidSnapshotD a b
    WePlaceFloraWithIdDv2 a b fl d fx i →
        WePlaceFloraRefD a b (FloraByLegacyId fl) d fx i

toWorldEditDTO ∷ WorldEdit → WorldEditDTO
toWorldEditDTO (WeDeleteTile a b)              = WeDeleteTileD a b
toWorldEditDTO (WeSetFluidTile a b f)          = WeSetFluidTileD a b f
toWorldEditDTO (WeAddTile a b m)               = WeAddTileD a b m
toWorldEditDTO (WeSetSlope a b c w)            = WeSetSlopeD a b c w
toWorldEditDTO (WeSetCell a b c m)             = WeSetCellD a b c m
toWorldEditDTO (WeSetStructure a b w c d e)    = WeSetStructureD a b w c d e
toWorldEditDTO (WeClearStructure a b w)        = WeClearStructureD a b w
toWorldEditDTO (WeSetVeg a b c w)              = WeSetVegD a b c w
toWorldEditDTO (WePlaceFlora a b fl d fx)     = WePlaceFloraD a b fl d fx
toWorldEditDTO (WePlaceFloraWithId a b fl d fx i) =
    WePlaceFloraWithIdD a b fl d fx i
toWorldEditDTO (WePlaceFloraRef a b ref d fx i) =
    WePlaceFloraRefD a b ref d fx i
toWorldEditDTO (WeSetFluidSnapshot a b f z)    = WeSetFluidSnapshotD a b f z
toWorldEditDTO (WeClearFluidSnapshot a b)      = WeClearFluidSnapshotD a b

fromWorldEditDTO ∷ WorldEditDTO → WorldEdit
fromWorldEditDTO (WeDeleteTileD a b)           = WeDeleteTile a b
fromWorldEditDTO (WeSetFluidTileD a b f)       = WeSetFluidTile a b f
fromWorldEditDTO (WeAddTileD a b m)            = WeAddTile a b m
fromWorldEditDTO (WeSetSlopeD a b c w)         = WeSetSlope a b c w
fromWorldEditDTO (WeSetCellD a b c m)          = WeSetCell a b c m
fromWorldEditDTO (WeSetStructureD a b w c d e) = WeSetStructure a b w c d e
fromWorldEditDTO (WeClearStructureD a b w)     = WeClearStructure a b w
fromWorldEditDTO (WeSetVegD a b c w)           = WeSetVeg a b c w
fromWorldEditDTO (WePlaceFloraD a b fl d fx)  = WePlaceFlora a b fl d fx
fromWorldEditDTO (WePlaceFloraWithIdD a b fl d fx i) =
    WePlaceFloraWithId a b fl d fx i
fromWorldEditDTO (WePlaceFloraRefD a b ref d fx i) =
    WePlaceFloraRef a b ref d fx i
fromWorldEditDTO (WeSetFluidSnapshotD a b f z) = WeSetFluidSnapshot a b f z
fromWorldEditDTO (WeClearFluidSnapshotD a b)   = WeClearFluidSnapshot a b

-- Chunk-keyed edit-log conversion (each value goes through its own
-- DTO; keys are plain chunk-coordinate leaves).
toEditsDTO ∷ WorldEdits → HM.HashMap ChunkCoord [WorldEditDTO]
toEditsDTO = HM.map (map toWorldEditDTO)
fromEditsDTO ∷ HM.HashMap ChunkCoord [WorldEditDTO] → WorldEdits
fromEditsDTO = HM.map (map fromWorldEditDTO)

-- world-edits -------------------------------------------------------

data PageEditsDTO = PageEditsDTO
    { pedPageId ∷ !WorldPageId
    , pedEdits  ∷ !(HM.HashMap ChunkCoord [WorldEditDTO])
    , pedPlantedFloraCursor ∷ !Word64
      -- ^ #1854: this page's planted-flora id allocator cursor, saved
      --   beside the very edits whose ids it accounts for so the two
      --   can never be restored out of step.
    } deriving (Show, Generic, Serialize)

-- | The FROZEN @world-edits@ v1 page slice: the page id and its edit
--   log, with no allocator cursor and with every entry in the frozen
--   'WorldEditDTOv1' shape.
data PageEditsDTOv1 = PageEditsDTOv1
    { ped1PageId ∷ !WorldPageId
    , ped1Edits  ∷ !(HM.HashMap ChunkCoord [WorldEditDTOv1])
    } deriving (Show, Generic, Serialize)

-- | The FROZEN @world-edits@ v2 page slice (#2243): the v2 shape
--   verbatim — page id, edit log at 'WorldEditDTOv2', and #1854's
--   allocator cursor.
data PageEditsDTOv2 = PageEditsDTOv2
    { ped2PageId ∷ !WorldPageId
    , ped2Edits  ∷ !(HM.HashMap ChunkCoord [WorldEditDTOv2])
    , ped2PlantedFloraCursor ∷ !Word64
    } deriving (Show, Generic, Serialize)

newtype WorldEditsDTO = WorldEditsDTO { wedPages ∷ [PageEditsDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN @world-edits@ v1 component payload.
newtype WorldEditsDTOv1 = WorldEditsDTOv1 { wed1Pages ∷ [PageEditsDTOv1] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN @world-edits@ v2 component payload (#2243).
newtype WorldEditsDTOv2 = WorldEditsDTOv2 { wed2Pages ∷ [PageEditsDTOv2] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | v1 → v2 (#1854): every edit crosses through
--   'migrateWorldEditDTOv1', and the cursor starts at the fresh-page
--   floor. Both the ids and the real cursor are established by
--   'applyWorldEdits', which is the only place that knows the page's
--   world size — see its note.
--
--   #2243: this lands on the FROZEN v2 shape and then crosses into the
--   current one through 'migrateWorldEditsV2', so a v1 payload takes
--   exactly the same species translation a v2 payload does rather than
--   a second copy of it.
migrateWorldEditsV1 ∷ WorldEditsDTOv1 → WorldEditsDTO
migrateWorldEditsV1 (WorldEditsDTOv1 slices) = migrateWorldEditsV2 $
    WorldEditsDTOv2
        [ PageEditsDTOv2
            { ped2PageId = ped1PageId s
            , ped2Edits  = HM.map (map migrateWorldEditDTOv1) (ped1Edits s)
            , ped2PlantedFloraCursor = firstPlantedFloraCursor
            }
        | s ← slices ]

-- | v2 → v3 (#2243): every edit crosses through
--   'migrateWorldEditDTOv2' — which names the two planting forms by
--   their legacy ordinal — and the cursor crosses verbatim.
migrateWorldEditsV2 ∷ WorldEditsDTOv2 → WorldEditsDTO
migrateWorldEditsV2 (WorldEditsDTOv2 slices) = WorldEditsDTO
    [ PageEditsDTO
        { pedPageId = ped2PageId s
        , pedEdits  = HM.map (map migrateWorldEditDTOv2) (ped2Edits s)
        , pedPlantedFloraCursor = ped2PlantedFloraCursor s
        }
    | s ← slices ]

-- | Component-local invariant (#1854 requirement 5): a page's
--   planted-flora allocator cursor must be strictly above every planted
--   'FloraInstanceId' its own edit log carries, or planting after a load
--   would reissue an id that is already standing in the world. Modelled
--   on 'validateWorldActivity''s ground-item allocator clause.
--
--   GENERATED ids are deliberately not checked against it: they come
--   from a disjoint namespace ("World.Flora.Identity") that this
--   allocator does not own and can never collide with.
validateWorldEdits ∷ WorldEditsDTO → [ComponentError]
validateWorldEdits (WorldEditsDTO slices) =
    [ ComponentError worldEditsComponentId 2 ValidatePhase
        ("page '" <> tshow (pedPageId s) <> "': planted flora id "
         <> tshow (floraInstanceIdToLua iid) <> " is not below the page's \
            \planted-flora allocator ("
         <> tshow (pedPlantedFloraCursor s) <> ")")
    | s     ← slices
    , edits ← HM.elems (pedEdits s)
    , iid   ← plantedIds edits
    , isPlantedFloraInstanceId iid
    , plantedFloraCursorAbove [iid] > pedPlantedFloraCursor s
    ]
  where
    -- Both identity-bearing planting forms, so the invariant keeps
    -- holding for a v2 payload's ids after #2243 renamed which
    -- constructor a CURRENT payload writes. The id-less 'WePlaceFloraD'
    -- carries nothing to check.
    plantedIds edits =
        [ iid | WePlaceFloraWithIdD _ _ _ _ _ iid ← edits ]
        ⧺ [ iid | WePlaceFloraRefD _ _ _ _ _ iid ← edits ]

worldEditsCodec ∷ ComponentCodec WorldEditsDTO
worldEditsCodec = componentCodec ComponentSpec
    { csComponent     = worldEditsComponentId
    , csVersion       = 3
    , csRequired      = True
    , csDeps          = [worldPagesComponentId]
    , csEncode        = \snap → WorldEditsDTO
        [ PageEditsDTO (pgsPageId p) (toEditsDTO (pgsEdits p))
                       (pgsPlantedFloraCursor p)
        | p ← orderedPages snap ]
    , csDecode        = id
    , csOlderVersions = [ atVersion 2 migrateWorldEditsV2
                        , atVersion 1 migrateWorldEditsV1 ]
    , csValidate      = validateWorldEdits
    }

-- | #1854: a v1 slice records no planted-flora ids, and the LEGACY
--   'WePlaceFlora' constructor that carries none is decode-only, so the
--   rewrite into the identity-bearing form happens HERE — the one place
--   that has both the edit log and the page's own world size, exactly as
--   'applyWorldActivity' owns #1175's canonical-frame repair for every
--   accepted version rather than splitting it between a migration and an
--   apply step.
--
--   It runs for EVERY accepted version, which is what makes it total: a
--   v2 slice's own ids and cursor are authoritative and nothing moves
--   (the rewrite touches only id-less entries, and the cursor only
--   grows), while a v1 slice gets both. Assignment is deterministic and
--   repeatable — chunk keys visited in ascending CANONICAL
--   chunk-coordinate order, never 'Data.HashMap.Strict' iteration order,
--   and each chunk's log in its own stored oldest-first order — so
--   migrating the same save twice produces the same ids.
applyWorldEdits
    ∷ Word32 → WorldEditsDTO → HM.HashMap WorldPageId PageSnapshot
    → Either [ComponentError] (HM.HashMap WorldPageId PageSnapshot)
applyWorldEdits ver (WorldEditsDTO slices) =
    applyPageSlices worldEditsComponentId ver pedPageId writeEdits slices
  where
    writeEdits s p =
        let decoded   = fromEditsDTO (pedEdits s)
            worldSize = wgpWorldSize (pgsGenParams p)
            -- A v1 payload has no cursor field at all; its migration
            -- supplies the fresh-page floor, which is where assignment
            -- must start.
            seed = if ver ≥ 2 then pedPlantedFloraCursor s
                              else firstPlantedFloraCursor
            (edits, cursor) = assignPlantedIds worldSize seed decoded
        in p { pgsEdits = edits, pgsPlantedFloraCursor = cursor }

-- | Give every id-LESS planting edit an identity, allocating from
--   @seed@ upward, and return the log beside a cursor that sits strictly
--   above every planted id it now carries.
--
--   #2243: every planting edit reaching here is a 'WePlaceFloraRef' —
--   'migrateWorldEditDTOv2' turns both legacy numeric forms into one,
--   and a current payload only ever carried it — so the id-less v1 case
--   is now spelled as one bearing 'World.Flora.Identity.floraInstanceIdNone'
--   rather than as its own constructor. The allocation itself is
--   unchanged.
assignPlantedIds ∷ Int → Word64 → WorldEdits → (WorldEdits, Word64)
assignPlantedIds worldSize seed edits =
    let ordered = L.sortOn (chunkOrderKey worldSize . fst) (HM.toList edits)
        (rebuilt, cursor) = L.foldl' perChunk ([], max firstPlantedFloraCursor seed)
                                     ordered
        allocated = [ iid | (_, es) ← rebuilt
                           , WePlaceFloraRef _ _ _ _ _ iid ← es ]
    in ( HM.fromList (reverse rebuilt)
       , max cursor (plantedFloraCursorAbove allocated) )
  where
    perChunk (acc, cursor) (coord, es) =
        let (es', cursor') = L.foldl' perEdit ([], cursor) es
        in ((coord, reverse es') : acc, cursor')
    perEdit (acc, cursor) e = case e of
        WePlaceFloraRef _ _ _ _ _ iid
            | isFloraInstanceIdNone iid →
                let (iid', cursor') = nextPlantedFloraCursor cursor
                in (reId e iid' : acc, cursor')
        _ → (e : acc, cursor)
    reId (WePlaceFloraRef gx gy ref day w _) iid =
        WePlaceFloraRef gx gy ref day w iid
    reId e _ = e
    -- Canonical coordinate FIRST, then the raw key as a tie-break. The
    -- canonical key alone is not a total order over this map: near the
    -- seam two distinct alias keys canonicalize to the same chunk (which
    -- is exactly the case 'World.Edit.Types.canonicalizeWorldEdits'
    -- exists to merge), and 'L.sortOn' is stable — so their relative
    -- order would have fallen back to 'HM.toList''s, making the ids this
    -- assignment hands out depend on hashmap traversal. Ordering the
    -- alias before its canonical twin also matches canonicalizeWorldEdits'
    -- own merge order, so the two agree about which entry came first.
    chunkOrderKey ws coord =
        let ChunkCoord cx cy = wrapChunkCoordU ws coord
            ChunkCoord rx ry = coord
        in ((cx, cy), (cx, cy) ≡ (rx, ry), (rx, ry))
