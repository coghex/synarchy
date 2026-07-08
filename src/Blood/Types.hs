{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, DeriveAnyClass #-}
-- | Blood decal model (#604): the world-scoped data + pure transitions
--   behind procedural injury blood, ahead of any rendering or combat
--   hook (see docs/blood_decals.md, the #603 epic's design record).
--
--   Two registries, kept in one 'BloodStore' rather than two separate
--   'Craft.Bills'/'Power.Types'-style refs, because eviction from the
--   texture pool must cascade into decal removal (requirement #4) —
--   splitting them across two IORefs would make that cascade
--   non-atomic against a concurrent Lua call. 'spawnDecal' is the one
--   entrypoint that resolves a texture (reuse or create), evicts if
--   the pool is over its cap, and places a decal, all as one pure
--   step so a single atomicModifyIORef' covers the whole operation.
--
--   Deliberately never persisted (issue #604 scope) — mirrors
--   'World.State.Types.wsStructureStageRef': a fresh 'WorldState' gets
--   an empty 'BloodStore' and it dies with the world, no
--   'WorldPageSave' field, no save-version bump.
module Blood.Types
    ( BloodStyle(..)
    , SeverityBucket(..)
    , FootprintBucket(..)
    , AnisotropyBucket(..)
    , EdgeBucket(..)
    , BloodTextureId(..)
    , BloodTextureDescriptor(..)
    , BloodTextureRequest(..)
    , BloodTexturePool(..)
    , emptyBloodTexturePool
    , requestDistance
    , matchThreshold
    , findMatch
    , requestTexture
    , lookupTexture
    , allTextures
    , BloodDecalId(..)
    , BloodDecal(..)
    , BloodDecalSpec(..)
    , BloodDecals(..)
    , emptyBloodDecals
    , addDecal
    , lookupDecal
    , allDecals
    , removeDecalsForTexture
    , BloodStore(..)
    , emptyBloodStore
    , defaultBloodTextureCap
    , spawnDecal
    , clearBlood
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.List (sortOn, foldl')
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))
import World.Page.Types (WorldPageId)
import Unit.Types (UnitId)

-- | The generated-texture archetype a request resolves to. Mirrors
--   docs/blood_decals.md's five-way split; a request that can't be
--   sensibly bucketed into one of these doesn't happen (the Lua
--   surface validates the incoming string).
data BloodStyle
    = StylePool
    | StyleDrops
    | StyleSpatter
    | StyleStreak
    | StyleSmear
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Combined severity/amount bucket (design doc: "severity/amount
--   bucket"). A hard-match dimension — two requests in different
--   buckets always get distinct textures, never reused across each
--   other, since a minor scratch and a fatal wound shouldn't share a
--   mark's visual weight.
data SeverityBucket
    = SeverityMinor
    | SeverityModerate
    | SeveritySevere
    | SeverityCatastrophic
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Approximate footprint size bucket — a near-match (soft) dimension.
data FootprintBucket = FootprintSmall | FootprintMedium | FootprintLarge
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Directionality/anisotropy bucket (round pool vs. directional
--   streak/spatter) — a near-match (soft) dimension.
data AnisotropyBucket = AnisotropyNone | AnisotropyLow | AnisotropyHigh
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Edge roughness / droplet-density bucket — a near-match (soft)
--   dimension.
data EdgeBucket = EdgeSmooth | EdgeModerate | EdgeRough
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- | Texture ids start at 1 (see 'emptyBloodTexturePool'), same
--   convention as 'Power.Types.PowerNodeId' — 0 never names a real
--   texture.
newtype BloodTextureId = BloodTextureId { unBloodTextureId ∷ Word32 }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable)

-- | One generated-texture descriptor: everything a real texture
--   generator (a future issue, #3 in the design doc's suggested
--   split) would need to reproduce the same look, plus what the
--   matcher here reads. Field order isn't load-bearing (no Serialize
--   instance — this store is never saved).
data BloodTextureDescriptor = BloodTextureDescriptor
    { btdId         ∷ !BloodTextureId
    , btdStyle      ∷ !BloodStyle
    , btdWoundKind  ∷ !Text
      -- ^ Free-form, mirroring 'Combat.Wounds.woundKind' — the engine
      --   has no closed WoundKind sum type, wound kinds are Text
      --   throughout (stab/slash/blunt/arterial/severed/...).
    , btdSeverity   ∷ !SeverityBucket
    , btdFootprint  ∷ !FootprintBucket
    , btdAnisotropy ∷ !AnisotropyBucket
    , btdEdge       ∷ !EdgeBucket
    , btdSeed       ∷ !Int
      -- ^ Generation seed / lineage — opaque here, meaningful once a
      --   real texture generator (design doc's "3. Procedural blood
      --   texture generation") reads it.
    } deriving (Show, Eq, Generic)

-- | What a blood request asks the pool to resolve to. Same shape as
--   the descriptor minus the id (which the pool assigns on miss).
data BloodTextureRequest = BloodTextureRequest
    { btrStyle      ∷ !BloodStyle
    , btrWoundKind  ∷ !Text
    , btrSeverity   ∷ !SeverityBucket
    , btrFootprint  ∷ !FootprintBucket
    , btrAnisotropy ∷ !AnisotropyBucket
    , btrEdge       ∷ !EdgeBucket
    , btrSeed       ∷ !Int
    } deriving (Show, Eq, Generic)

-- | The bounded FIFO of generated textures. 'btpOrder' is the actual
--   oldest-first queue eviction walks — a HashMap alone can't answer
--   "which one is oldest", and the FIFO-order requirement (design doc
--   + issue #604 requirement #2) is this queue, not a per-descriptor
--   field that could drift from it.
data BloodTexturePool = BloodTexturePool
    { btpTextures ∷ !(HM.HashMap BloodTextureId BloodTextureDescriptor)
    , btpOrder    ∷ !(Seq BloodTextureId)
    , btpNextId   ∷ !Word32
    , btpCap      ∷ !Int
    } deriving (Show, Eq, Generic)

-- | The #604 default cap. Deliberately small — this issue is the
--   model + debug surface, not final tuning (design doc's suggested
--   split leaves "aging, caps, and cleanup tuning" as its own later
--   issue); a small cap keeps eviction reachable in a short headless
--   probe run.
defaultBloodTextureCap ∷ Int
defaultBloodTextureCap = 24

emptyBloodTexturePool ∷ Int → BloodTexturePool
emptyBloodTexturePool cap = BloodTexturePool HM.empty Seq.empty 1 cap

-- | How far a request is from an existing descriptor, or 'Nothing' if
--   they can never be considered the same mark. Style and severity
--   bucket are hard gates (issue #604 acceptance: "different styles or
--   severity buckets create distinct descriptors") — any mismatch
--   there rules the descriptor out entirely, regardless of the other
--   dimensions. Wound kind, footprint, anisotropy, and edge are soft:
--   each mismatch adds to a Manhattan-style distance that 'findMatch'
--   compares against 'matchThreshold'. A wound-kind mismatch is worth
--   a flat 2 (categorical, no natural ordering); the bucketed
--   dimensions contribute their ordinal difference.
requestDistance ∷ BloodTextureRequest → BloodTextureDescriptor → Maybe Int
requestDistance req d
    | btrStyle req    ≢ btdStyle d    = Nothing
    | btrSeverity req ≢ btdSeverity d = Nothing
    | otherwise = Just $
        (if btrWoundKind req ≡ btdWoundKind d then 0 else 2)
        + bucketDist (btrFootprint req)  (btdFootprint d)
        + bucketDist (btrAnisotropy req) (btdAnisotropy d)
        + bucketDist (btrEdge req)       (btdEdge d)
  where
    bucketDist ∷ Enum a ⇒ a → a → Int
    bucketDist a b = abs (fromEnum a - fromEnum b)

-- | Requests within this total soft-distance of an existing descriptor
--   reuse it rather than minting a new one. Chosen so a request that
--   differs by one near-match bucket step still reuses (e.g. same
--   style/severity/wound kind, footprint one bucket off), but two or
--   more differences — or any wound-kind mismatch, worth 2 on its own
--   — force a new descriptor.
matchThreshold ∷ Int
matchThreshold = 1

-- | The closest descriptor within threshold, if any. Ties break on
--   the lower texture id (oldest) — an arbitrary but deterministic
--   choice, since distance alone doesn't order equally-close matches.
findMatch ∷ BloodTextureRequest → BloodTexturePool → Maybe BloodTextureId
findMatch req pool = case sortOn snd candidates of
    (tid, _) : _ → Just tid
    []           → Nothing
  where
    candidates =
        [ (tid, dist)
        | (tid, d) ← HM.toList (btpTextures pool)
        , Just dist ← [requestDistance req d]
        , dist ≤ matchThreshold
        ]

lookupTexture ∷ BloodTextureId → BloodTexturePool → Maybe BloodTextureDescriptor
lookupTexture tid = HM.lookup tid . btpTextures

-- | Every descriptor, oldest first.
allTextures ∷ BloodTexturePool → [BloodTextureDescriptor]
allTextures pool =
    mapMaybe (`HM.lookup` btpTextures pool) (toList (btpOrder pool))

-- | Resolve @req@ against the pool: reuse a near-match if one exists,
--   otherwise allocate a fresh descriptor and evict from the front
--   until back under 'btpCap'. Returns the resolved id, whether a NEW
--   descriptor was minted, and every id evicted along the way (the
--   caller must also drop any decal referencing an evicted id — see
--   'spawnDecal').
requestTexture ∷ BloodTextureRequest → BloodTexturePool
              → (BloodTexturePool, BloodTextureId, Bool, [BloodTextureId])
requestTexture req pool = case findMatch req pool of
    Just tid → (pool, tid, False, [])
    Nothing  →
        let tid = BloodTextureId (btpNextId pool)
            d   = BloodTextureDescriptor
                    { btdId         = tid
                    , btdStyle      = btrStyle req
                    , btdWoundKind  = btrWoundKind req
                    , btdSeverity   = btrSeverity req
                    , btdFootprint  = btrFootprint req
                    , btdAnisotropy = btrAnisotropy req
                    , btdEdge       = btrEdge req
                    , btdSeed       = btrSeed req
                    }
            inserted = pool
                { btpTextures = HM.insert tid d (btpTextures pool)
                , btpOrder    = btpOrder pool |> tid
                , btpNextId   = btpNextId pool + 1
                }
            (evicted, capped) = evictOverCap inserted
        in (capped, tid, True, evicted)

-- | Drop from the front of the FIFO until at or under cap. Recurses
--   (rather than dropping once) so a cap lowered below the current
--   size still converges in one call.
evictOverCap ∷ BloodTexturePool → ([BloodTextureId], BloodTexturePool)
evictOverCap pool
    | HM.size (btpTextures pool) ≤ max 0 (btpCap pool) = ([], pool)
    | otherwise = case Seq.viewl (btpOrder pool) of
        Seq.EmptyL → ([], pool)
        oldest Seq.:< rest →
            let pool' = pool
                    { btpTextures = HM.delete oldest (btpTextures pool)
                    , btpOrder    = rest
                    }
                (more, pool'') = evictOverCap pool'
            in (oldest : more, pool'')

-- | Decal ids start at 1, same convention as texture ids.
newtype BloodDecalId = BloodDecalId { unBloodDecalId ∷ Word32 }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable)

-- | One placed blood mark. Field order isn't load-bearing (no
--   Serialize — never saved, #604 scope).
data BloodDecal = BloodDecal
    { bdeId         ∷ !BloodDecalId
    , bdeTexture    ∷ !BloodTextureId
    , bdePage       ∷ !WorldPageId
    , bdeX          ∷ !Float
      -- ^ World tile-space x (float, sub-tile) — mirrors
      --   'Item.Ground.giX'. Serves both "tile" (integer-valued) and
      --   "continuous" placement per the design doc.
    , bdeY          ∷ !Float
    , bdeSurfaceZ   ∷ !Int
    , bdeOffsetX    ∷ !Float
      -- ^ Local render-space jitter, distinct from world position —
      --   nudges the decal quad within its tile so nearby marks don't
      --   perfectly overlap.
    , bdeOffsetY    ∷ !Float
    , bdeRotation   ∷ !Float
    , bdeScale      ∷ !Float
    , bdeCreatedAt  ∷ !Double
      -- ^ Game time at placement. "Current age" (design doc) is
      --   derived at read time (now - bdeCreatedAt), not stored — no
      --   ticking system owns this yet (#604 scope excludes rain/
      --   fluid interaction and aging renders).
    , bdeWoundKind  ∷ !Text
    , bdeSeverity   ∷ !SeverityBucket
    , bdeSourceUnit ∷ !(Maybe UnitId)
    , bdeOpacity    ∷ !Float
      -- ^ Amount/opacity, design doc's "amount/opacity".
    } deriving (Show, Eq, Generic)

-- | Everything 'addDecal' needs except the id it assigns.
data BloodDecalSpec = BloodDecalSpec
    { bspTexture    ∷ !BloodTextureId
    , bspPage       ∷ !WorldPageId
    , bspX          ∷ !Float
    , bspY          ∷ !Float
    , bspSurfaceZ   ∷ !Int
    , bspOffsetX    ∷ !Float
    , bspOffsetY    ∷ !Float
    , bspRotation   ∷ !Float
    , bspScale      ∷ !Float
    , bspCreatedAt  ∷ !Double
    , bspWoundKind  ∷ !Text
    , bspSeverity   ∷ !SeverityBucket
    , bspSourceUnit ∷ !(Maybe UnitId)
    , bspOpacity    ∷ !Float
    } deriving (Show, Eq, Generic)

data BloodDecals = BloodDecals
    { bdlDecals ∷ !(HM.HashMap BloodDecalId BloodDecal)
    , bdlNextId ∷ !Word32
    } deriving (Show, Eq, Generic)

emptyBloodDecals ∷ BloodDecals
emptyBloodDecals = BloodDecals HM.empty 1

addDecal ∷ BloodDecalSpec → BloodDecals → (BloodDecals, BloodDecalId)
addDecal spec decals =
    let did = BloodDecalId (bdlNextId decals)
        d   = BloodDecal
            { bdeId         = did
            , bdeTexture    = bspTexture spec
            , bdePage       = bspPage spec
            , bdeX          = bspX spec
            , bdeY          = bspY spec
            , bdeSurfaceZ   = bspSurfaceZ spec
            , bdeOffsetX    = bspOffsetX spec
            , bdeOffsetY    = bspOffsetY spec
            , bdeRotation   = bspRotation spec
            , bdeScale      = bspScale spec
            , bdeCreatedAt  = bspCreatedAt spec
            , bdeWoundKind  = bspWoundKind spec
            , bdeSeverity   = bspSeverity spec
            , bdeSourceUnit = bspSourceUnit spec
            , bdeOpacity    = bspOpacity spec
            }
    in ( decals { bdlDecals = HM.insert did d (bdlDecals decals)
                , bdlNextId = bdlNextId decals + 1 }
       , did )

lookupDecal ∷ BloodDecalId → BloodDecals → Maybe BloodDecal
lookupDecal did = HM.lookup did . bdlDecals

-- | Every decal, oldest (lowest id) first.
allDecals ∷ BloodDecals → [BloodDecal]
allDecals = sortOn bdeId . HM.elems . bdlDecals

-- | Drop every decal referencing an evicted texture (issue #604
--   requirement #4: eviction cascades to dependent decals).
removeDecalsForTexture ∷ BloodTextureId → BloodDecals → BloodDecals
removeDecalsForTexture tid decals = decals
    { bdlDecals = HM.filter ((≢ tid) . bdeTexture) (bdlDecals decals) }

-- | The combined per-world blood state (see module haddock for why
--   pool + decals share one record instead of two refs).
data BloodStore = BloodStore
    { bstPool   ∷ !BloodTexturePool
    , bstDecals ∷ !BloodDecals
    } deriving (Show, Eq, Generic)

emptyBloodStore ∷ Int → BloodStore
emptyBloodStore cap = BloodStore (emptyBloodTexturePool cap) emptyBloodDecals

-- | The one spawn entrypoint: resolve @req@ against the texture pool
--   (reuse or create + evict), cascade-remove any decal orphaned by an
--   eviction, then place a new decal against the resolved texture.
--   @mkSpec@ receives the resolved texture id — the caller doesn't
--   need to know it up front, since whether a match is reused or a
--   fresh id is minted is decided inside this same step.
spawnDecal ∷ BloodTextureRequest → (BloodTextureId → BloodDecalSpec)
          → BloodStore → (BloodStore, BloodDecalId, BloodTextureId, Bool)
spawnDecal req mkSpec store =
    let (pool', tid, isNew, evicted) = requestTexture req (bstPool store)
        decalsAfterEviction =
            foldl' (flip removeDecalsForTexture) (bstDecals store) evicted
        (decals', did) = addDecal (mkSpec tid) decalsAfterEviction
    in (BloodStore pool' decals', did, tid, isNew)

-- | Reset to empty — both descriptor and decal id counters restart at
--   1, same as a fresh 'BloodStore' (issue #604 acceptance: "clear
--   leaves both descriptor and decal lists empty").
clearBlood ∷ BloodStore → BloodStore
clearBlood store = emptyBloodStore (btpCap (bstPool store))
