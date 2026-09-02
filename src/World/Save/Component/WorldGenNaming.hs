{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | Worldgen NAME and PLACED-LOCATION DTOs (issue #2098 owner split of
--   the #760 frozen worldgen graph).
--
--   This owner holds every generated-name and placed-location wire shape
--   the @"world-pages"@ component carries, current and historical
--   together, because they evolve as one family: a location's stored
--   display name, gloss and etymology arrived with #1101/#1104, its
--   bounds with #911, its discovery lifecycle with #1230 and its
--   encounter with #916, and each of those landed a new frozen
--   @LocationInstanceDTOv{1,2,3,4}@ beside the current shape.
--
--   Held here:
--
--   - 'NameExprDTO' and 'EtymologySourceDTO' — the generated-name
--     expression and the language it was rendered from (#1104).
--     'NameExprDTO' is the one append-only guarded sum in the worldgen
--     graph ('docs/save_compat/enum_baseline.json'); its constructor
--     order is the wire contract.
--   - 'AbsBoundsDTO' — the inclusive tile box a location occupies.
--   - 'LocationEncounterDTO' / 'LocationEncounterOccupantDTO' (#916).
--   - 'LocationInstanceDTO' / 'LocationInstancesDTO' (current) and the
--     frozen decode-only 'LocationInstanceDTOv1' … 'LocationInstanceDTOv4'.
--   - 'RiverNameDTO' / 'RiverNamesDTO' (current) and the frozen
--     decode-only 'RiverNameDTOv1' / 'RiverNamesDTOv1' (#1102).
--
--   ALL river-name shapes live here rather than beside the worldgen
--   parameters that embed them, so requirement 6's one-definition rule
--   has a single unambiguous target for the family.
--
--   The historical @to…DTOv{1,2,3,4}@ encoders are exported rather than
--   module-private precisely because their only callers are the
--   historical worldgen-parameter conversions in
--   "World.Save.Component.WorldGenHistory"; a private helper that its
--   caller cannot reach is the exact pressure that produces a duplicate
--   wire declaration.
--
--   Every declaration below is a positional cereal wire contract. The
--   boundary rule itself is stated ONCE, in
--   "World.Save.Component.Types".
module World.Save.Component.WorldGenNaming
    ( NameExprDTO(..)
    , EtymologySourceDTO(..)
    , AbsBoundsDTO(..)
    , LocationEncounterOccupantDTO(..)
    , LocationEncounterDTO(..)
    , LocationInstanceDTO(..)
    , LocationInstancesDTO(..)
    , LocationInstanceDTOv4(..)
    , LocationInstancesDTOv4(..)
    , LocationInstanceDTOv1(..)
    , LocationInstancesDTOv1(..)
    , LocationInstanceDTOv2(..)
    , LocationInstancesDTOv2(..)
    , LocationInstanceDTOv3(..)
    , LocationInstancesDTOv3(..)
    , RiverNameDTO(..)
    , RiverNamesDTO(..)
    , RiverNameDTOv1(..)
    , RiverNamesDTOv1(..)
    , toNameExprDTO
    , fromNameExprDTO
    , toEtymologySourceDTO
    , fromEtymologySourceDTO
    , toAbsBoundsDTO
    , fromAbsBoundsDTO
    , toLocationEncounterDTO
    , fromLocationEncounterDTO
    , toLocationInstanceDTO
    , fromLocationInstanceDTO
    , toLocationInstancesDTO
    , fromLocationInstancesDTO
    , fromLocationInstanceDTOv4
    , toLocationInstancesDTOv4
    , fromLocationInstancesDTOv4
    , fromLocationInstanceDTOv1
    , toLocationInstancesDTOv1
    , fromLocationInstancesDTOv1
    , fromLocationInstanceDTOv2
    , toLocationInstancesDTOv2
    , fromLocationInstancesDTOv2
    , fromLocationInstanceDTOv3
    , toLocationInstancesDTOv3
    , fromLocationInstancesDTOv3
    , historicalDiscoveryMargin
    , toRiverNameDTO
    , fromRiverNameDTO
    , toRiverNamesDTO
    , fromRiverNamesDTO
    , fromRiverNameDTOv1
    , toRiverNamesDTOv1
    , fromRiverNamesDTOv1
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Location.Bounds (AbsBounds(..))
import World.Base (GeoFeatureId)
import Language.Etymology.Source (EtymologySource(..))
import Language.Generated.Types
    ( GeneratorVersion(..), LangSeed(..), LanguageProvenance(..)
    , generatorVersionInt, langSeedWord )
import Language.Semantic.Types (ConceptId, GramNumber, NameExpr(..))
import World.River.Naming (RiverName(..), RiverNames(..))
import Location.Instance
    ( LocationEncounter(..), LocationEncounterOccupant(..)
    , LocationInstance(..), LocationInstances(..), LocationInstanceId
    , LocationLifecycle )
import World.Chunk.Types (ChunkCoord)
import Unit.Types.Manager (UnitId)
import World.Save.Reference (SamePageRef(..))

-- Etymology sources (#1104) ------------------------------------------

-- | Frozen mirror of 'Language.Semantic.Types.NameExpr'. Its OWN
--   constructor order is the wire contract, decoupled from the live
--   sum's, exactly like 'World.Save.Component.Page.WorldEditDTO':
--   adding a live constructor makes 'toNameExprDTO' non-exhaustive (a
--   compile error under @-Werror@) rather than silently shifting every
--   stored expression's tag.
--
--   'ConceptId' is reused as a leaf (a newtype over 'Text', like
--   'World.Material.Id.MaterialId'), and 'GramNumber' as a payload-free
--   append-only enum (like 'ZoomMapMode' / 'LocationLifecycle').
data NameExprDTO
    = BareD !ConceptId
    | ModifierD !ConceptId !ConceptId
    | OfD !ConceptId !GramNumber !ConceptId
    | PossessiveD !ConceptId !ConceptId
    deriving (Show, Eq, Generic, Serialize)

toNameExprDTO ∷ NameExpr → NameExprDTO
toNameExprDTO (Bare c)         = BareD c
toNameExprDTO (Modifier m h)   = ModifierD m h
toNameExprDTO (Of h n c)       = OfD h n c
toNameExprDTO (Possessive o h) = PossessiveD o h

fromNameExprDTO ∷ NameExprDTO → NameExpr
fromNameExprDTO (BareD c)         = Bare c
fromNameExprDTO (ModifierD m h)   = Modifier m h
fromNameExprDTO (OfD h n c)       = Of h n c
fromNameExprDTO (PossessiveD o h) = Possessive o h

-- | Frozen mirror of 'Language.Etymology.Source.EtymologySource' (#1104):
--   the expression a generated name was rendered from, plus the language
--   that rendered it.
--
--   The provenance is stored FLAT (seed and version as primitives) rather
--   than as a nested DTO, so this type is reachable from both the page
--   identity and the per-page location / river tables without either
--   half of the component graph having to import the other's leaf.
--   Seed and version still travel together in ONE optional record, so a
--   decode can never produce a seed without a version — the same
--   invariant "World.Save.Component.Page"'s 'LanguageProvenanceDTO'
--   protects.
data EtymologySourceDTO = EtymologySourceDTO
    { esdExpr    ∷ !NameExprDTO
    , esdSeed    ∷ !Word64
    , esdVersion ∷ !Int
    } deriving (Show, Eq, Generic, Serialize)

toEtymologySourceDTO ∷ EtymologySource → EtymologySourceDTO
toEtymologySourceDTO e = EtymologySourceDTO
    { esdExpr    = toNameExprDTO (esExpr e)
    , esdSeed    = langSeedWord (lpSeed (esLanguage e))
    , esdVersion = generatorVersionInt (lpVersion (esLanguage e))
    }

fromEtymologySourceDTO ∷ EtymologySourceDTO → EtymologySource
fromEtymologySourceDTO d = EtymologySource
    { esExpr     = fromNameExprDTO (esdExpr d)
    , esLanguage = LanguageProvenance
        { lpSeed    = LangSeed (esdSeed d)
        , lpVersion = GeneratorVersion (esdVersion d)
        }
    }

-- Placed-location instances (#911) -----------------------------------

-- | Frozen mirror of 'Location.Bounds.AbsBounds'. An inclusive tile box
--   is definitionally four coordinates, so this could arguably be a
--   leaf like 'ChunkCoord' — it is frozen anyway because it is reached
--   only through 'LocationInstanceDTO', whose whole point is that the
--   live location record WILL gain fields (encounter, loot, and cleared
--   state are the next expedition-loop issues), and freezing the pair
--   together keeps the boundary in one place.
data AbsBoundsDTO = AbsBoundsDTO
    { abdMinX ∷ !Int
    , abdMinY ∷ !Int
    , abdMaxX ∷ !Int
    , abdMaxY ∷ !Int
    } deriving (Show, Eq, Generic, Serialize)

toAbsBoundsDTO ∷ AbsBounds → AbsBoundsDTO
toAbsBoundsDTO b = AbsBoundsDTO (abMinX b) (abMinY b) (abMaxX b) (abMaxY b)

fromAbsBoundsDTO ∷ AbsBoundsDTO → AbsBounds
fromAbsBoundsDTO d = AbsBounds (abdMinX d) (abdMinY d) (abdMaxX d) (abdMaxY d)

-- | Frozen mirror of one persisted encounter occupant. The unit id is a
--   typed same-page reference: absence is tolerated by the shared integrity
--   graph, while resolution on a different page is a hard load error.
data LocationEncounterOccupantDTO = LocationEncounterOccupantDTO
    { leodUnitId              ∷ !(SamePageRef UnitId)
    , leodHomeX               ∷ !Float
    , leodHomeY               ∷ !Float
    , leodEngaged             ∷ !Bool
    , leodReturning           ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

data LocationEncounterDTO = LocationEncounterDTO
    { ledRolledCount        ∷ !Int
    , ledOccupants          ∷ ![LocationEncounterOccupantDTO]
    , ledRosterComplete     ∷ !Bool
    , ledDeathOnlyClearance ∷ !Bool
    , ledActivated          ∷ !Bool
    , ledEpisodeActive      ∷ !Bool
    , ledAggressionAnnounced ∷ !Bool
    , ledDisengageAnnounced  ∷ !Bool
    , ledCleared            ∷ !Bool
    , ledClearEventEmitted  ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

toLocationEncounterDTO ∷ LocationEncounter → LocationEncounterDTO
toLocationEncounterDTO e = LocationEncounterDTO
    { ledRolledCount        = leRolledCount e
    , ledOccupants          = map toOccupant (leOccupants e)
    , ledRosterComplete     = leRosterComplete e
    , ledDeathOnlyClearance = leDeathOnlyClearance e
    , ledActivated          = leActivated e
    , ledEpisodeActive      = leEpisodeActive e
    , ledAggressionAnnounced = leAggressionAnnounced e
    , ledDisengageAnnounced  = leDisengageAnnounced e
    , ledCleared            = leCleared e
    , ledClearEventEmitted  = leClearEventEmitted e
    }
  where
    toOccupant o = LocationEncounterOccupantDTO
        { leodUnitId              = SamePageRef (leoUnitId o)
        , leodHomeX               = fst (leoHome o)
        , leodHomeY               = snd (leoHome o)
        , leodEngaged             = leoEngaged o
        , leodReturning           = leoReturning o
        }

fromLocationEncounterDTO ∷ LocationEncounterDTO → LocationEncounter
fromLocationEncounterDTO d = LocationEncounter
    { leRolledCount        = ledRolledCount d
    , leOccupants          = map fromOccupant (ledOccupants d)
    , leRosterComplete     = ledRosterComplete d
    , leDeathOnlyClearance = ledDeathOnlyClearance d
    , leActivated          = ledActivated d
    , leEpisodeActive      = ledEpisodeActive d
    , leAggressionAnnounced = ledAggressionAnnounced d
    , leDisengageAnnounced  = ledDisengageAnnounced d
    , leCleared            = ledCleared d
    , leClearEventEmitted  = ledClearEventEmitted d
    }
  where
    fromOccupant o = LocationEncounterOccupant
        { leoUnitId              = unSamePageRef (leodUnitId o)
        , leoHome                = (leodHomeX o, leodHomeY o)
        , leoEngaged             = leodEngaged o
        , leoReturning           = leodReturning o
        }

-- | Frozen mirror of 'Location.Instance.LocationInstance' — a LIVE
--   gameplay record by construction (its lifecycle and content-spawn
--   flag are mutated in place, and the expedition arc adds encounter /
--   loot / progression fields to it), so the boundary rule
--   ("World.Save.Component.Types") requires an explicit field-by-field
--   conversion rather than embedding it. 'LocationInstanceId' is a leaf
--   id and 'LocationLifecycle' a payload-free append-only enum, both
--   reused as-is exactly like 'ChunkCoord' / 'ZoomMapMode'.
--
--   This is the CURRENT shape, carried by @world-pages@ v8: #1101's
--   English gloss beside the display name and #1104's optional
--   etymology source, no discovery margin since #1230, and #916's optional
--   persistent encounter. 'LocationInstanceDTOv4' below is the frozen
--   pre-#916 shape (v7), 'LocationInstanceDTOv3' the pre-#1230 shape (v6),
--   'LocationInstanceDTOv2' the pre-#1104 one and
--   'LocationInstanceDTOv1' the pre-#1101 one.
data LocationInstanceDTO = LocationInstanceDTO
    { lidId              ∷ !LocationInstanceId
    , lidDefId           ∷ !Text
    , lidChunk           ∷ !ChunkCoord
    , lidAnchorX         ∷ !Int
    , lidAnchorY         ∷ !Int
    , lidBounds          ∷ !AbsBoundsDTO
    , lidDisplayName     ∷ !Text
    , lidGloss           ∷ !(Maybe Text)
    , lidEtymology       ∷ !(Maybe EtymologySourceDTO)
    , lidLifecycle       ∷ !LocationLifecycle
    , lidContentsSpawned ∷ !Bool
    , lidEncounter       ∷ !(Maybe LocationEncounterDTO)
    } deriving (Show, Eq, Generic, Serialize)

toLocationInstanceDTO ∷ LocationInstance → LocationInstanceDTO
toLocationInstanceDTO i = LocationInstanceDTO
    { lidId              = liId i
    , lidDefId           = liDefId i
    , lidChunk           = liChunk i
    , lidAnchorX         = fst (liAnchor i)
    , lidAnchorY         = snd (liAnchor i)
    , lidBounds          = toAbsBoundsDTO (liBounds i)
    , lidDisplayName     = liDisplayName i
    , lidGloss           = liGloss i
    , lidEtymology       = toEtymologySourceDTO <$> liEtymology i
    , lidLifecycle       = liLifecycle i
    , lidContentsSpawned = liContentsSpawned i
    , lidEncounter       = toLocationEncounterDTO <$> liEncounter i
    }

fromLocationInstanceDTO ∷ LocationInstanceDTO → LocationInstance
fromLocationInstanceDTO d = LocationInstance
    { liId              = lidId d
    , liDefId           = lidDefId d
    , liChunk           = lidChunk d
    , liAnchor          = (lidAnchorX d, lidAnchorY d)
    , liBounds          = fromAbsBoundsDTO (lidBounds d)
    , liDisplayName     = lidDisplayName d
    , liGloss           = lidGloss d
    , liEtymology       = fromEtymologySourceDTO <$> lidEtymology d
    , liLifecycle       = lidLifecycle d
    , liContentsSpawned = lidContentsSpawned d
    , liEncounter       = fromLocationEncounterDTO <$> lidEncounter d
    }

-- | Frozen mirror of the per-page instance table: its allocator plus
--   its instances. 'Location.Instance.lisPendingLegacy' has no field
--   here on purpose — it is a transient v1-migration carry that can
--   never be true of anything on disk, so @fromLocationInstancesDTO@
--   always rebuilds it as 'Nothing'.
data LocationInstancesDTO = LocationInstancesDTO
    { lisdNextId ∷ !Int
    , lisdById   ∷ !(HM.HashMap LocationInstanceId LocationInstanceDTO)
    } deriving (Show, Eq, Generic, Serialize)

toLocationInstancesDTO ∷ LocationInstances → LocationInstancesDTO
toLocationInstancesDTO l = LocationInstancesDTO
    { lisdNextId = lisNextId l
    , lisdById   = HM.map toLocationInstanceDTO (lisById l)
    }

fromLocationInstancesDTO ∷ LocationInstancesDTO → LocationInstances
fromLocationInstancesDTO d = LocationInstances
    { lisNextId        = lisdNextId d
    , lisById          = HM.map fromLocationInstanceDTO (lisdById d)
    , lisPendingLegacy = Nothing
    }

-- | The FROZEN @world-pages@ v7 location shape (#1230 through #916),
--   preserved verbatim: every current field except the encounter added in
--   v8. Historical instances migrate with @liEncounter = Nothing@; inventing
--   a roll during load would make the loader/content version choose gameplay.
data LocationInstanceDTOv4 = LocationInstanceDTOv4
    { lid4Id              ∷ !LocationInstanceId
    , lid4DefId           ∷ !Text
    , lid4Chunk           ∷ !ChunkCoord
    , lid4AnchorX         ∷ !Int
    , lid4AnchorY         ∷ !Int
    , lid4Bounds          ∷ !AbsBoundsDTO
    , lid4DisplayName     ∷ !Text
    , lid4Gloss           ∷ !(Maybe Text)
    , lid4Etymology       ∷ !(Maybe EtymologySourceDTO)
    , lid4Lifecycle       ∷ !LocationLifecycle
    , lid4ContentsSpawned ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

fromLocationInstanceDTOv4 ∷ LocationInstanceDTOv4 → LocationInstance
fromLocationInstanceDTOv4 d = LocationInstance
    { liId              = lid4Id d
    , liDefId           = lid4DefId d
    , liChunk           = lid4Chunk d
    , liAnchor          = (lid4AnchorX d, lid4AnchorY d)
    , liBounds          = fromAbsBoundsDTO (lid4Bounds d)
    , liDisplayName     = lid4DisplayName d
    , liGloss           = lid4Gloss d
    , liEtymology       = fromEtymologySourceDTO <$> lid4Etymology d
    , liLifecycle       = lid4Lifecycle d
    , liContentsSpawned = lid4ContentsSpawned d
    , liEncounter       = Nothing
    }

data LocationInstancesDTOv4 = LocationInstancesDTOv4
    { lisd4NextId ∷ !Int
    , lisd4ById   ∷ !(HM.HashMap LocationInstanceId LocationInstanceDTOv4)
    } deriving (Show, Eq, Generic, Serialize)

toLocationInstancesDTOv4 ∷ LocationInstances → LocationInstancesDTOv4
toLocationInstancesDTOv4 l = LocationInstancesDTOv4
    { lisd4NextId = lisNextId l
    , lisd4ById   = HM.map toV4 (lisById l)
    }
  where
    toV4 i = LocationInstanceDTOv4
        { lid4Id              = liId i
        , lid4DefId           = liDefId i
        , lid4Chunk           = liChunk i
        , lid4AnchorX         = fst (liAnchor i)
        , lid4AnchorY         = snd (liAnchor i)
        , lid4Bounds          = toAbsBoundsDTO (liBounds i)
        , lid4DisplayName     = liDisplayName i
        , lid4Gloss           = liGloss i
        , lid4Etymology       = toEtymologySourceDTO <$> liEtymology i
        , lid4Lifecycle       = liLifecycle i
        , lid4ContentsSpawned = liContentsSpawned i
        }

fromLocationInstancesDTOv4 ∷ LocationInstancesDTOv4 → LocationInstances
fromLocationInstancesDTOv4 d = LocationInstances
    { lisNextId        = lisd4NextId d
    , lisById          = HM.map fromLocationInstanceDTOv4 (lisd4ById d)
    , lisPendingLegacy = Nothing
    }

-- | The FROZEN pre-#1101 instance shape, preserved verbatim for
--   decode-only backward compatibility: everything the current DTO
--   carries except the gloss. Never edited — a further change freezes
--   the CURRENT shape as 'LocationInstanceDTOv2' instead (frozen-DTO
--   boundary rule).
data LocationInstanceDTOv1 = LocationInstanceDTOv1
    { lid1Id              ∷ !LocationInstanceId
    , lid1DefId           ∷ !Text
    , lid1Chunk           ∷ !ChunkCoord
    , lid1AnchorX         ∷ !Int
    , lid1AnchorY         ∷ !Int
    , lid1Bounds          ∷ !AbsBoundsDTO
    , lid1DiscoveryMargin ∷ !Int
    , lid1DisplayName     ∷ !Text
    , lid1Lifecycle       ∷ !LocationLifecycle
    , lid1ContentsSpawned ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

-- | A pre-#1101 instance keeps the name it was stored with, EXACTLY —
--   and its stored @discovery_margin@ is DROPPED (#1230): the live
--   record has no such field any more, and the wire shape above is
--   frozen, so the value is decoded off the wire and discarded rather
--   than the historical bytes being edited.
--
--   it was rendered once when the instance was placed and is never
--   re-derived (#1101 requirements 4 and 7) — and decodes with NO gloss.
--   A gloss is the English reading of a generated name; a stored label
--   has no such reading, and inventing one would attach a meaning to a
--   location that never had it.
fromLocationInstanceDTOv1 ∷ LocationInstanceDTOv1 → LocationInstance
fromLocationInstanceDTOv1 d = LocationInstance
    { liId              = lid1Id d
    , liDefId           = lid1DefId d
    , liChunk           = lid1Chunk d
    , liAnchor          = (lid1AnchorX d, lid1AnchorY d)
    , liBounds          = fromAbsBoundsDTO (lid1Bounds d)
    , liDisplayName     = lid1DisplayName d
    , liGloss           = Nothing
    , liEtymology       = Nothing
    , liLifecycle       = lid1Lifecycle d
    , liContentsSpawned = lid1ContentsSpawned d
    , liEncounter       = Nothing
    }

-- | The FROZEN pre-#1101 instance table. Structurally identical to
--   'LocationInstancesDTO' but over the frozen per-instance shape.
data LocationInstancesDTOv1 = LocationInstancesDTOv1
    { lisd1NextId ∷ !Int
    , lisd1ById   ∷ !(HM.HashMap LocationInstanceId LocationInstanceDTOv1)
    } deriving (Show, Eq, Generic, Serialize)

-- | Encoder for the frozen table — the round-trip partner every frozen
--   DTO version's tests build fixture bytes with (the same reason
--   'toWorldGenParamsDTOv1' exists).
toLocationInstancesDTOv1 ∷ LocationInstances → LocationInstancesDTOv1
toLocationInstancesDTOv1 l = LocationInstancesDTOv1
    { lisd1NextId = lisNextId l
    , lisd1ById   = HM.map toV1 (lisById l)
    }
  where
    toV1 i = LocationInstanceDTOv1
        { lid1Id              = liId i
        , lid1DefId           = liDefId i
        , lid1Chunk           = liChunk i
        , lid1AnchorX         = fst (liAnchor i)
        , lid1AnchorY         = snd (liAnchor i)
        , lid1Bounds          = toAbsBoundsDTO (liBounds i)
        , lid1DiscoveryMargin = historicalDiscoveryMargin
        , lid1DisplayName     = liDisplayName i
        , lid1Lifecycle       = liLifecycle i
        , lid1ContentsSpawned = liContentsSpawned i
        }

fromLocationInstancesDTOv1 ∷ LocationInstancesDTOv1 → LocationInstances
fromLocationInstancesDTOv1 d = LocationInstances
    { lisNextId        = lisd1NextId d
    , lisById          = HM.map fromLocationInstanceDTOv1 (lisd1ById d)
    , lisPendingLegacy = Nothing
    }

-- | The FROZEN pre-#1104 instance shape, preserved verbatim for
--   decode-only backward compatibility: everything the current DTO
--   carries except the etymology source. This is what @world-pages@ v4
--   (#1101) and v5 (#1102) both encoded — #1102 changed the PAGE's
--   river table, not the instance — so both versions share one instance
--   shape. Never edited; a further change freezes the CURRENT shape as
--   a v3 instead (frozen-DTO boundary rule).
data LocationInstanceDTOv2 = LocationInstanceDTOv2
    { lid2Id              ∷ !LocationInstanceId
    , lid2DefId           ∷ !Text
    , lid2Chunk           ∷ !ChunkCoord
    , lid2AnchorX         ∷ !Int
    , lid2AnchorY         ∷ !Int
    , lid2Bounds          ∷ !AbsBoundsDTO
    , lid2DiscoveryMargin ∷ !Int
    , lid2DisplayName     ∷ !Text
    , lid2Gloss           ∷ !(Maybe Text)
    , lid2Lifecycle       ∷ !LocationLifecycle
    , lid2ContentsSpawned ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

-- | A pre-#1104 instance keeps its stored name AND gloss exactly, its
--   stored @discovery_margin@ is dropped (#1230, exactly as in
--   'fromLocationInstanceDTOv1'), and
--   decodes with NO etymology source. The expression behind a name was
--   simply not recorded then; inventing one would attach a fabricated
--   derivation to a real location, which #1104 requirement 1 forbids as
--   explicitly as #1101 forbids inventing a gloss.
fromLocationInstanceDTOv2 ∷ LocationInstanceDTOv2 → LocationInstance
fromLocationInstanceDTOv2 d = LocationInstance
    { liId              = lid2Id d
    , liDefId           = lid2DefId d
    , liChunk           = lid2Chunk d
    , liAnchor          = (lid2AnchorX d, lid2AnchorY d)
    , liBounds          = fromAbsBoundsDTO (lid2Bounds d)
    , liDisplayName     = lid2DisplayName d
    , liGloss           = lid2Gloss d
    , liEtymology       = Nothing
    , liLifecycle       = lid2Lifecycle d
    , liContentsSpawned = lid2ContentsSpawned d
    , liEncounter       = Nothing
    }

-- | The FROZEN pre-#1104 instance table. Structurally identical to
--   'LocationInstancesDTO' but over the frozen per-instance shape.
data LocationInstancesDTOv2 = LocationInstancesDTOv2
    { lisd2NextId ∷ !Int
    , lisd2ById   ∷ !(HM.HashMap LocationInstanceId LocationInstanceDTOv2)
    } deriving (Show, Eq, Generic, Serialize)

-- | Encoder for the frozen table — the round-trip partner every frozen
--   DTO version's tests build fixture bytes with.
toLocationInstancesDTOv2 ∷ LocationInstances → LocationInstancesDTOv2
toLocationInstancesDTOv2 l = LocationInstancesDTOv2
    { lisd2NextId = lisNextId l
    , lisd2ById   = HM.map toV2 (lisById l)
    }
  where
    toV2 i = LocationInstanceDTOv2
        { lid2Id              = liId i
        , lid2DefId           = liDefId i
        , lid2Chunk           = liChunk i
        , lid2AnchorX         = fst (liAnchor i)
        , lid2AnchorY         = snd (liAnchor i)
        , lid2Bounds          = toAbsBoundsDTO (liBounds i)
        , lid2DiscoveryMargin = historicalDiscoveryMargin
        , lid2DisplayName     = liDisplayName i
        , lid2Gloss           = liGloss i
        , lid2Lifecycle       = liLifecycle i
        , lid2ContentsSpawned = liContentsSpawned i
        }

fromLocationInstancesDTOv2 ∷ LocationInstancesDTOv2 → LocationInstances
fromLocationInstancesDTOv2 d = LocationInstances
    { lisNextId        = lisd2NextId d
    , lisById          = HM.map fromLocationInstanceDTOv2 (lisd2ById d)
    , lisPendingLegacy = Nothing
    }

-- | The FROZEN pre-#1230 instance shape, preserved verbatim for
--   decode-only backward compatibility: everything the current DTO
--   carries PLUS the @discovery_margin@ the live record used to store
--   (#911) and lost when reveal became sight-based. This is what
--   @world-pages@ v6 (#1104) encoded. Never edited; a further change
--   freezes the CURRENT shape as a v4 instead (frozen-DTO boundary
--   rule).
data LocationInstanceDTOv3 = LocationInstanceDTOv3
    { lid3Id              ∷ !LocationInstanceId
    , lid3DefId           ∷ !Text
    , lid3Chunk           ∷ !ChunkCoord
    , lid3AnchorX         ∷ !Int
    , lid3AnchorY         ∷ !Int
    , lid3Bounds          ∷ !AbsBoundsDTO
    , lid3DiscoveryMargin ∷ !Int
    , lid3DisplayName     ∷ !Text
    , lid3Gloss           ∷ !(Maybe Text)
    , lid3Etymology       ∷ !(Maybe EtymologySourceDTO)
    , lid3Lifecycle       ∷ !LocationLifecycle
    , lid3ContentsSpawned ∷ !Bool
    } deriving (Show, Eq, Generic, Serialize)

-- | A pre-#1230 instance carries EVERYTHING across unchanged —
--   allocator id, definition id, chunk, anchor, bounds, display name,
--   gloss, etymology source, lifecycle and content-spawn flag — and
--   drops exactly one thing: its stored discovery margin, which has no
--   live counterpart any more (#1230 requirement 11). Reveal is
--   sight-based against 'liBounds', so the halo the margin described
--   describes nothing; it is decoded off the wire and discarded rather
--   than being remapped onto some other field.
fromLocationInstanceDTOv3 ∷ LocationInstanceDTOv3 → LocationInstance
fromLocationInstanceDTOv3 d = LocationInstance
    { liId              = lid3Id d
    , liDefId           = lid3DefId d
    , liChunk           = lid3Chunk d
    , liAnchor          = (lid3AnchorX d, lid3AnchorY d)
    , liBounds          = fromAbsBoundsDTO (lid3Bounds d)
    , liDisplayName     = lid3DisplayName d
    , liGloss           = lid3Gloss d
    , liEtymology       = fromEtymologySourceDTO <$> lid3Etymology d
    , liLifecycle       = lid3Lifecycle d
    , liContentsSpawned = lid3ContentsSpawned d
    , liEncounter       = Nothing
    }

-- | The FROZEN pre-#1230 instance table. Structurally identical to
--   'LocationInstancesDTO' but over the frozen per-instance shape.
data LocationInstancesDTOv3 = LocationInstancesDTOv3
    { lisd3NextId ∷ !Int
    , lisd3ById   ∷ !(HM.HashMap LocationInstanceId LocationInstanceDTOv3)
    } deriving (Show, Eq, Generic, Serialize)

-- | Encoder for the frozen table — the round-trip partner every frozen
--   DTO version's tests build fixture bytes with.
toLocationInstancesDTOv3 ∷ LocationInstances → LocationInstancesDTOv3
toLocationInstancesDTOv3 l = LocationInstancesDTOv3
    { lisd3NextId = lisNextId l
    , lisd3ById   = HM.map toV3 (lisById l)
    }
  where
    toV3 i = LocationInstanceDTOv3
        { lid3Id              = liId i
        , lid3DefId           = liDefId i
        , lid3Chunk           = liChunk i
        , lid3AnchorX         = fst (liAnchor i)
        , lid3AnchorY         = snd (liAnchor i)
        , lid3Bounds          = toAbsBoundsDTO (liBounds i)
        , lid3DiscoveryMargin = historicalDiscoveryMargin
        , lid3DisplayName     = liDisplayName i
        , lid3Gloss           = liGloss i
        , lid3Etymology       = toEtymologySourceDTO <$> liEtymology i
        , lid3Lifecycle       = liLifecycle i
        , lid3ContentsSpawned = liContentsSpawned i
        }

fromLocationInstancesDTOv3 ∷ LocationInstancesDTOv3 → LocationInstances
fromLocationInstancesDTOv3 d = LocationInstances
    { lisNextId        = lisd3NextId d
    , lisById          = HM.map fromLocationInstanceDTOv3 (lisd3ById d)
    , lisPendingLegacy = Nothing
    }

-- | The discovery margin every FROZEN instance encoder writes (#1230).
--
--   Those encoders exist only to build fixture bytes for the historical
--   wire shapes, and they are handed a LIVE 'LocationInstance', which no
--   longer records a margin at all. There is therefore nothing truthful
--   to copy: zero is written as the honest "no margin recorded", and it
--   is never read back — every @fromLocationInstanceDTOv{1,2,3}@ drops
--   the field. A fixture that needs to prove a NONZERO historical margin
--   survives its migration constructs the frozen DTO directly instead,
--   which is exactly what the v6→v7 migration test does.
historicalDiscoveryMargin ∷ Int
historicalDiscoveryMargin = 0

-- River names (#1102) ------------------------------------------------

-- | Frozen mirror of 'World.River.Naming.RiverName'. Small and stable
--   today, but it is a LIVE record on a live per-page table — a later
--   naming issue could give a river a second reading or a provenance of
--   its own — so it gets the same explicit conversion every other live
--   record on 'WorldGenParams' does rather than being embedded.
data RiverNameDTO = RiverNameDTO
    { rvdDisplayName ∷ !Text
    , rvdGloss       ∷ !(Maybe Text)
    , rvdEtymology   ∷ !(Maybe EtymologySourceDTO)
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of 'World.River.Naming.RiverNames'.
--   'World.Base.GeoFeatureId' is a leaf id newtype over 'Int', reused
--   as-is exactly like 'LocationInstanceId' / 'ChunkCoord'.
newtype RiverNamesDTO = RiverNamesDTO
    { rvdById ∷ HM.HashMap GeoFeatureId RiverNameDTO }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

toRiverNameDTO ∷ RiverName → RiverNameDTO
toRiverNameDTO n = RiverNameDTO
    { rvdDisplayName = rvnDisplayName n
    , rvdGloss       = rvnGloss n
    , rvdEtymology   = toEtymologySourceDTO <$> rvnEtymology n
    }

-- | Rebuild the live record. A stored name is carried across EXACTLY —
--   never re-rendered from the page's language, which is the whole
--   point of #708 principle 5 (a river named under one catalogue keeps
--   that name even after the catalogue grows).
fromRiverNameDTO ∷ RiverNameDTO → RiverName
fromRiverNameDTO d = RiverName
    { rvnDisplayName = rvdDisplayName d
    , rvnGloss       = rvdGloss d
    , rvnEtymology   = fromEtymologySourceDTO <$> rvdEtymology d
    }

toRiverNamesDTO ∷ RiverNames → RiverNamesDTO
toRiverNamesDTO = RiverNamesDTO . HM.map toRiverNameDTO . rvnById

fromRiverNamesDTO ∷ RiverNamesDTO → RiverNames
fromRiverNamesDTO = RiverNames . HM.map fromRiverNameDTO . rvdById

-- | The FROZEN pre-#1104 river-name shape (@world-pages@ v5), preserved
--   verbatim for decode-only backward compatibility: the stored name and
--   its gloss, no etymology source. Never edited; a further change
--   freezes the CURRENT shape as a v2 instead (frozen-DTO boundary rule).
data RiverNameDTOv1 = RiverNameDTOv1
    { rvd1DisplayName ∷ !Text
    , rvd1Gloss       ∷ !(Maybe Text)
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of the pre-#1104 per-page table.
newtype RiverNamesDTOv1 = RiverNamesDTOv1
    { rvd1ById ∷ HM.HashMap GeoFeatureId RiverNameDTOv1 }
    deriving stock (Generic)
    deriving newtype (Show, Eq, Serialize)

-- | A pre-#1104 river keeps its stored name and gloss EXACTLY and
--   decodes with no etymology source — the same honest absence a
--   pre-#1104 location instance decodes with.
fromRiverNameDTOv1 ∷ RiverNameDTOv1 → RiverName
fromRiverNameDTOv1 d = RiverName
    { rvnDisplayName = rvd1DisplayName d
    , rvnGloss       = rvd1Gloss d
    , rvnEtymology   = Nothing
    }

-- | Encoder for the frozen table — the round-trip partner a frozen-DTO
--   fixture is built with.
toRiverNamesDTOv1 ∷ RiverNames → RiverNamesDTOv1
toRiverNamesDTOv1 = RiverNamesDTOv1 . HM.map toV1 . rvnById
  where
    toV1 n = RiverNameDTOv1
        { rvd1DisplayName = rvnDisplayName n
        , rvd1Gloss       = rvnGloss n
        }

fromRiverNamesDTOv1 ∷ RiverNamesDTOv1 → RiverNames
fromRiverNamesDTOv1 = RiverNames . HM.map fromRiverNameDTOv1 . rvd1ById
