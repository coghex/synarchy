{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | The @"world-pages"@ owner (issue #760, save-overhaul B2; split out
--   of "World.Save.Component.Page" by #2135).
--
--   @"world-pages"@ (required, page-set authority) — per page: identity,
--   generation params, dates/clocks, map mode, and the page's OWN
--   remembered camera position. Owner: the world page. Boundary reason:
--   this is the spine every other page-scoped component's page set is
--   checked against; the world-generation seed lives in its gen params
--   (requirement 10). No dependencies — it is the root of the page
--   dependency graph.
--
--   Being that root is also why the two sibling owners
--   ("World.Save.Component.PageEdits" and
--   "World.Save.Component.PageActivity") import THIS module and never
--   the reverse: 'orderedPages' is the canonical (page-id ascending)
--   encode order every page-scoped component writes its slice list in,
--   so identical input produces identical bytes (requirement 10), and
--   stating it once here is what keeps the three from drifting. This
--   module imports neither sibling and neither the façade.
--
--   Requirement 4 — the on-disk contract is FROZEN, distinct from every
--   mutable runtime record; see "World.Save.Component.Page" for the
--   page-scoped statement of that rule and the full live-record →
--   frozen-DTO table. The live records this owner mirrors are:
--
--   - 'WorldGenParams'      → 'WorldGenParamsDTO' (with its nested live
--                             config/state records frozen recursively —
--                             see "World.Save.Component.WorldGen"; the
--                             pre-#1104 shape stays as
--                             'WorldGenParamsDTOv4', the pre-#1102 one as
--                             'WorldGenParamsDTOv3', the pre-#1101 one as
--                             'WorldGenParamsDTOv2' and the pre-#911 one
--                             as 'WorldGenParamsDTOv1')
--   - 'WorldIdentity'       → 'WorldIdentityDTO' (its optional
--                             'LanguageProvenance' frozen as
--                             'LanguageProvenanceDTO' and its optional
--                             #1104 etymology source as
--                             'EtymologySourceDTO'; the pre-#1104 shape
--                             stays as 'WorldIdentityDTOv2' and the
--                             pre-#1092 one as 'WorldIdentityDTOv1')
--
--   'ZoomMapMode' is reused as a payload-free append-only leaf enum, and
--   'GeneratedWorldId' as a durable opaque id leaf, exactly per the
--   frozen-DTO boundary rule stated in "World.Save.Component.Types".
module World.Save.Component.PageCore
    ( -- * The canonical page order every page-scoped encoder writes in
      orderedPages
      -- * Frozen world identity
    , WorldIdentityDTO(..)
    , WorldIdentityDTOv1(..)
    , WorldIdentityDTOv2(..)
    , LanguageProvenanceDTO(..)
    , toWorldIdentityDTO
    , toWorldIdentityDTOv2
      -- * The @"world-pages"@ wire shapes
    , PageCoreDTO(..)
    , WorldPagesDTO(..)
    , PageCoreDTOv1(..)
    , WorldPagesDTOv1(..)
    , PageCoreDTOv2(..)
    , WorldPagesDTOv2(..)
    , PageCoreDTOv3(..)
    , WorldPagesDTOv3(..)
    , PageCoreDTOv4(..)
    , WorldPagesDTOv4(..)
    , PageCoreDTOv5(..)
    , WorldPagesDTOv5(..)
    , PageCoreDTOv6(..)
    , WorldPagesDTOv6(..)
    , PageCoreDTOv7(..)
    , WorldPagesDTOv7(..)
    , PageCoreDTOv8(..)
    , WorldPagesDTOv8(..)
    , PageCoreDTOv9(..)
    , WorldPagesDTOv9(..)
      -- * The component
    , WorldPages(..)
    , worldPagesCodec
    , validatePages
    , basePageSnapshots
    , blankPageSnapshot
    , migrateWorldPagesV1
    , migrateWorldPagesV2
    , migrateWorldPagesV3
    , migrateWorldPagesV4
    , migrateWorldPagesV5
    , migrateWorldPagesV6
    , migrateWorldPagesV7
    , migrateWorldPagesV8
    , migrateWorldPagesV9
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Craft.Bills (emptyCraftBills)
import Unit.Transfer.Orders (emptyTransferOrders)
import Building.Knowledge (emptyContainerKnowledge)
import Power.Types (emptyPowerNodes)
import World.Save.Component.WorldGen
    ( WorldGenParamsDTO(..), toWorldGenParamsDTO, fromWorldGenParamsDTO
    , WorldGenParamsDTOv1(..), fromWorldGenParamsDTOv1
    , WorldGenParamsDTOv2(..), fromWorldGenParamsDTOv2
    , WorldGenParamsDTOv3(..), fromWorldGenParamsDTOv3
    , WorldGenParamsDTOv4(..), fromWorldGenParamsDTOv4
    , WorldGenParamsDTOv5(..), fromWorldGenParamsDTOv5
    , WorldGenParamsDTOv6(..), fromWorldGenParamsDTOv6
    , WorldGenParamsDTOv7(..), fromWorldGenParamsDTOv7
    , EtymologySourceDTO(..)
    , toEtymologySourceDTO, fromEtymologySourceDTO )
import Location.Instance
    ( locationInstanceAllocatorErrors, locationInstanceBoundsErrors
    , locationSignificantItemErrors )
import World.Generate.Types (WorldGenParams(..))
import World.Page.Types (WorldPageId, WorldIdentity(..))
import World.Page.GeneratedId (GeneratedWorldId, renderGeneratedWorldId)
import Language.Generated.Types
    ( LanguageProvenance(..), LangSeed(..), GeneratorVersion(..) )
import World.Render.Zoom.Types (ZoomMapMode(..))
import World.Edit.Types (emptyWorldEdits)
import World.Construct.Attempt (firstConstructAttemptId)
import World.Flora.Identity (firstPlantedFloraCursor)
import World.Spoil.Types (emptySpoilPiles)
import World.Flora.Harvest (emptyFloraHarvests, emptyPendingFloraHarvests)
import Item.Ground (emptyGroundItems)
import World.Save.Types (BuildingSnapshot(..), UnitSnapshot(..))
import World.Save.Snapshot (SessionSnapshot(..), PageSnapshot(..))
import World.Save.Component.Types

-- Canonical (page-id ascending) ordered list of a snapshot's pages.
orderedPages ∷ SessionSnapshot → [PageSnapshot]
orderedPages = L.sortOn pgsPageId . HM.elems . snapPages

-- Frozen leaf DTOs (requirement 4) -----------------------------------

-- | Frozen mirror of 'WorldIdentity' — the CURRENT (world-pages v10)
--   shape: the optional language provenance #1092 added, plus the
--   optional etymology source #1104 added. #1230 took the component to
--   v7 and #916 took it to v8 without touching the identity, so both
--   frozen page cores embed this same type.
--
--   The two are independently optional, exactly as they are on the live
--   record: provenance says WHICH language named the world, the source
--   says WHAT expression it rendered. A world can have the first without
--   the second (a caller that recorded a language but no expression),
--   never the second without the first — the source carries its own
--   provenance, so the pair can never disagree about the language.
data WorldIdentityDTO = WorldIdentityDTO
    { widName      ∷ !Text
    , widGloss     ∷ !(Maybe Text)
    , widLanguage  ∷ !(Maybe LanguageProvenanceDTO)
    , widEtymology ∷ !(Maybe EtymologySourceDTO)
    } deriving (Show, Eq, Generic, Serialize)

-- | Frozen mirror of 'LanguageProvenance' (#1092). Seed and version
--   live in ONE optional DTO, never as two independently-optional
--   fields — a decode can then never produce a seed without a version
--   (or the reverse), which would be an unreconstructible profile.
--   The primitives are the wire contract; the live newtypes are
--   reapplied on the way back in.
data LanguageProvenanceDTO = LanguageProvenanceDTO
    { lpdSeed    ∷ !Word64
    , lpdVersion ∷ !Int
    } deriving (Show, Eq, Generic, Serialize)

toLanguageProvenanceDTO ∷ LanguageProvenance → LanguageProvenanceDTO
toLanguageProvenanceDTO p = LanguageProvenanceDTO
    { lpdSeed    = langSeedWord (lpSeed p)
    , lpdVersion = generatorVersionInt (lpVersion p)
    }

fromLanguageProvenanceDTO ∷ LanguageProvenanceDTO → LanguageProvenance
fromLanguageProvenanceDTO d = LanguageProvenance
    { lpSeed    = LangSeed (lpdSeed d)
    , lpVersion = GeneratorVersion (lpdVersion d)
    }

toWorldIdentityDTO ∷ WorldIdentity → WorldIdentityDTO
toWorldIdentityDTO i = WorldIdentityDTO (wiName i) (wiGloss i)
    (toLanguageProvenanceDTO <$> wiLanguage i)
    (toEtymologySourceDTO <$> wiEtymology i)

fromWorldIdentityDTO ∷ WorldIdentityDTO → WorldIdentity
fromWorldIdentityDTO d = WorldIdentity (widName d) (widGloss d)
    (fromLanguageProvenanceDTO <$> widLanguage d)
    (fromEtymologySourceDTO <$> widEtymology d)

-- | The FROZEN pre-#1104 identity shape (@world-pages@ v3 through v5),
--   preserved verbatim for decode-only backward compatibility: name,
--   gloss, and #1092's language provenance, with no etymology source.
--   Never edited; a further identity schema change freezes the CURRENT
--   shape as 'WorldIdentityDTOv3' rather than touching this one
--   (frozen-DTO boundary rule).
data WorldIdentityDTOv2 = WorldIdentityDTOv2
    { wid2Name     ∷ !Text
    , wid2Gloss    ∷ !(Maybe Text)
    , wid2Language ∷ !(Maybe LanguageProvenanceDTO)
    } deriving (Show, Eq, Generic, Serialize)

-- | Encoder for the frozen shape — the round-trip partner a frozen-DTO
--   fixture is built with (the same reason 'toWorldGenParamsDTOv3'
--   exists). Kept exported with no consumer yet (#1119) because that
--   fixture seam is the whole point of a frozen shape's encoder, and
--   'toItemInstanceDTOv1' cites it as the precedent for its own.
toWorldIdentityDTOv2 ∷ WorldIdentity → WorldIdentityDTOv2
toWorldIdentityDTOv2 i = WorldIdentityDTOv2 (wiName i) (wiGloss i)
    (toLanguageProvenanceDTO <$> wiLanguage i)

-- | A pre-#1104 identity keeps its name, gloss, and language EXACTLY
--   and decodes with NO etymology source — the same honest absence
--   @fromWorldIdentityDTOv1@ produces for provenance. A world named
--   before the expression was recorded genuinely has none to recover,
--   and deriving one from the name would fabricate a meaning.
fromWorldIdentityDTOv2 ∷ WorldIdentityDTOv2 → WorldIdentity
fromWorldIdentityDTOv2 d = WorldIdentity (wid2Name d) (wid2Gloss d)
    (fromLanguageProvenanceDTO <$> wid2Language d) Nothing

-- | The FROZEN pre-#1092 identity shape, preserved verbatim for
--   decode-only backward compatibility: name and gloss, no language.
--   Referenced by the frozen 'PageCoreDTOv1'/'PageCoreDTOv2' page cores
--   and by "World.Save.Compat.SessionV90"'s v90 page save. Never
--   edited; a further identity schema change freezes the CURRENT
--   shape as 'WorldIdentityDTOv2' rather than touching either of them
--   (frozen-DTO boundary rule).
data WorldIdentityDTOv1 = WorldIdentityDTOv1
    { wid1Name  ∷ !Text
    , wid1Gloss ∷ !(Maybe Text)
    } deriving (Show, Eq, Generic, Serialize)

-- | Historical identities decode with provenance ABSENT — never
--   inferred (#1092 requirement 3, following #915's precedent). A
--   world named before provenance was recorded genuinely has no
--   recoverable language, and guessing one would attach a false
--   etymology to a real world. Name and gloss carry across exactly.
fromWorldIdentityDTOv1 ∷ WorldIdentityDTOv1 → WorldIdentity
fromWorldIdentityDTOv1 d =
    WorldIdentity (wid1Name d) (wid1Gloss d) Nothing Nothing

-- world-pages -------------------------------------------------------

-- | One page's identity / clock / camera core. All evolving records are
--   frozen DTOs; 'ZoomMapMode' is a payload-free append-only leaf enum.
--   This is the CURRENT (v10) wire shape — see 'PageCoreDTOv9' for the
--   frozen pre-#917 one, 'PageCoreDTOv8' for the
--   frozen pre-#2021 one, 'PageCoreDTOv7' for the
--   frozen pre-#916 one, 'PageCoreDTOv6' for the
--   frozen pre-#1230 one, 'PageCoreDTOv5' for the
--   pre-#1104 one, 'PageCoreDTOv4' for the pre-#1102 one,
--   'PageCoreDTOv3' for the pre-#1101 one, 'PageCoreDTOv2' for the
--   pre-#1092 one, and 'PageCoreDTOv1' for the pre-#911 one.
data PageCoreDTO = PageCoreDTO
    { pcPageId      ∷ !WorldPageId
    , pcGenParams   ∷ !WorldGenParamsDTO
    , pcCameraX     ∷ !Float
    , pcCameraY     ∷ !Float
    , pcTimeHour    ∷ !Int
    , pcTimeMinute  ∷ !Int
    , pcDateYear    ∷ !Int
    , pcDateMonth   ∷ !Int
    , pcDateDay     ∷ !Int
    , pcMapMode     ∷ !ZoomMapMode
    , pcIdentity    ∷ !(Maybe WorldIdentityDTO)
    , pcGeneratedId ∷ !(Maybe GeneratedWorldId)
      -- ^ #2021: this page's opaque generated-world id — the
      --   AUTHORITATIVE copy (the @"metadata"@ component carries a
      --   duplicate purely so a listing-depth read can obtain it
      --   without decoding this component).
      --
      --   Optional on the WIRE so that encoding is a total, faithful
      --   mirror of 'World.Save.Snapshot.pgsGeneratedId' — it never
      --   fabricates an id it was not given. That is NOT a licence for a
      --   v9 payload to omit one: @validatePages@ rejects an absent id
      --   in any payload whose own version carries the field
      --   ('wpIdsFromPayload'), so "a v9 save with no id" fails to
      --   decode while a MIGRATED pre-v9 payload legitimately arrives
      --   with 'Nothing' for load staging to fill.
      --
      --   The 'GeneratedWorldId' is reused as-is rather than mirrored by
      --   a frozen DTO, under the frozen-DTO boundary rule's carve-out
      --   for durable opaque id leaves: it is 128 bits with no internal
      --   structure a later change could reshape.
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTO = WorldPagesDTO { wpdPages ∷ [PageCoreDTO] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN v1 wire shape, preserved verbatim for decode-only
--   backward compatibility: identical to 'PageCoreDTO' except that its
--   gen params are 'WorldGenParamsDTOv1' (three chunk-keyed location
--   sets, no instance table — #911 replaced two of them) and its
--   identity is the pre-#1092 'WorldIdentityDTOv1'. Never edited; a
--   further schema change adds a newer type instead (frozen-DTO
--   boundary rule). "World.Save.Compat.SessionV90"'s B1 path builds
--   these too, since v90 bytes carry exactly the v1 gen params.
data PageCoreDTOv1 = PageCoreDTOv1
    { pc1PageId      ∷ !WorldPageId
    , pc1GenParams   ∷ !WorldGenParamsDTOv1
    , pc1CameraX     ∷ !Float
    , pc1CameraY     ∷ !Float
    , pc1TimeHour    ∷ !Int
    , pc1TimeMinute  ∷ !Int
    , pc1DateYear    ∷ !Int
    , pc1DateMonth   ∷ !Int
    , pc1DateDay     ∷ !Int
    , pc1MapMode     ∷ !ZoomMapMode
    , pc1Identity    ∷ !(Maybe WorldIdentityDTOv1)
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTOv1 = WorldPagesDTOv1 { wpd1Pages ∷ [PageCoreDTOv1] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN v2 wire shape (#911 through #1092), preserved verbatim
--   for decode-only backward compatibility: the #911 gen params, but
--   the pre-#1092 identity with no language provenance. Never edited.
--
--   Its gen params are 'WorldGenParamsDTOv2', the frozen pre-#1101
--   shape — #1101 was the "later gen-params change" this comment used
--   to anticipate, so the field was repointed off the current type onto
--   the frozen copy exactly as described, leaving these bytes unchanged.
data PageCoreDTOv2 = PageCoreDTOv2
    { pc2PageId      ∷ !WorldPageId
    , pc2GenParams   ∷ !WorldGenParamsDTOv2
    , pc2CameraX     ∷ !Float
    , pc2CameraY     ∷ !Float
    , pc2TimeHour    ∷ !Int
    , pc2TimeMinute  ∷ !Int
    , pc2DateYear    ∷ !Int
    , pc2DateMonth   ∷ !Int
    , pc2DateDay     ∷ !Int
    , pc2MapMode     ∷ !ZoomMapMode
    , pc2Identity    ∷ !(Maybe WorldIdentityDTOv1)
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTOv2 = WorldPagesDTOv2 { wpd2Pages ∷ [PageCoreDTOv2] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN v3 wire shape (#1092 through #1101), preserved verbatim
--   for decode-only backward compatibility: #1092's identity (name,
--   gloss, optional language provenance) over the frozen pre-#1101 gen
--   params, whose location instances carry no gloss of their own. Never
--   edited; a further schema change adds a newer type instead
--   (frozen-DTO boundary rule).
data PageCoreDTOv3 = PageCoreDTOv3
    { pc3PageId      ∷ !WorldPageId
    , pc3GenParams   ∷ !WorldGenParamsDTOv2
    , pc3CameraX     ∷ !Float
    , pc3CameraY     ∷ !Float
    , pc3TimeHour    ∷ !Int
    , pc3TimeMinute  ∷ !Int
    , pc3DateYear    ∷ !Int
    , pc3DateMonth   ∷ !Int
    , pc3DateDay     ∷ !Int
    , pc3MapMode     ∷ !ZoomMapMode
    , pc3Identity    ∷ !(Maybe WorldIdentityDTOv2)
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTOv3 = WorldPagesDTOv3 { wpd3Pages ∷ [PageCoreDTOv3] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN v4 wire shape (#1101 through #1102), preserved verbatim
--   for decode-only backward compatibility: #1092's identity over
--   #1101's gen params, whose location instances carry a gloss but
--   whose page carries no river-name table. Never edited; a further
--   schema change adds a newer type instead (frozen-DTO boundary rule).
data PageCoreDTOv4 = PageCoreDTOv4
    { pc4PageId      ∷ !WorldPageId
    , pc4GenParams   ∷ !WorldGenParamsDTOv3
    , pc4CameraX     ∷ !Float
    , pc4CameraY     ∷ !Float
    , pc4TimeHour    ∷ !Int
    , pc4TimeMinute  ∷ !Int
    , pc4DateYear    ∷ !Int
    , pc4DateMonth   ∷ !Int
    , pc4DateDay     ∷ !Int
    , pc4MapMode     ∷ !ZoomMapMode
    , pc4Identity    ∷ !(Maybe WorldIdentityDTOv2)
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTOv4 = WorldPagesDTOv4 { wpd4Pages ∷ [PageCoreDTOv4] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN v5 wire shape (#1102 through #1104), preserved verbatim
--   for decode-only backward compatibility: #1092's identity over
--   #1102's gen params, neither of which carries an etymology source.
--   Never edited; a further schema change adds a newer type instead
--   (frozen-DTO boundary rule).
data PageCoreDTOv5 = PageCoreDTOv5
    { pc5PageId      ∷ !WorldPageId
    , pc5GenParams   ∷ !WorldGenParamsDTOv4
    , pc5CameraX     ∷ !Float
    , pc5CameraY     ∷ !Float
    , pc5TimeHour    ∷ !Int
    , pc5TimeMinute  ∷ !Int
    , pc5DateYear    ∷ !Int
    , pc5DateMonth   ∷ !Int
    , pc5DateDay     ∷ !Int
    , pc5MapMode     ∷ !ZoomMapMode
    , pc5Identity    ∷ !(Maybe WorldIdentityDTOv2)
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTOv5 = WorldPagesDTOv5 { wpd5Pages ∷ [PageCoreDTOv5] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN v6 wire shape (#1104 through #1230), preserved verbatim
--   for decode-only backward compatibility: #1104's identity (carrying
--   its own etymology source) over #1104's gen params, whose location
--   instances still carry the @discovery_margin@ #1230 removed. Never edited; a further schema
--   change adds a newer type instead (frozen-DTO boundary rule).
data PageCoreDTOv6 = PageCoreDTOv6
    { pc6PageId      ∷ !WorldPageId
    , pc6GenParams   ∷ !WorldGenParamsDTOv5
    , pc6CameraX     ∷ !Float
    , pc6CameraY     ∷ !Float
    , pc6TimeHour    ∷ !Int
    , pc6TimeMinute  ∷ !Int
    , pc6DateYear    ∷ !Int
    , pc6DateMonth   ∷ !Int
    , pc6DateDay     ∷ !Int
    , pc6MapMode     ∷ !ZoomMapMode
    , pc6Identity    ∷ !(Maybe WorldIdentityDTO)
      -- ^ the CURRENT identity shape, not 'WorldIdentityDTOv2': #1104
      --   put an etymology source on the page identity in v6 and #1230
      --   changed nothing about it.
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTOv6 = WorldPagesDTOv6 { wpd6Pages ∷ [PageCoreDTOv6] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN v7 wire shape (#1230 through #916): the current page
--   identity over the frozen pre-encounter worldgen/location DTO.
data PageCoreDTOv7 = PageCoreDTOv7
    { pc7PageId      ∷ !WorldPageId
    , pc7GenParams   ∷ !WorldGenParamsDTOv6
    , pc7CameraX     ∷ !Float
    , pc7CameraY     ∷ !Float
    , pc7TimeHour    ∷ !Int
    , pc7TimeMinute  ∷ !Int
    , pc7DateYear    ∷ !Int
    , pc7DateMonth   ∷ !Int
    , pc7DateDay     ∷ !Int
    , pc7MapMode     ∷ !ZoomMapMode
    , pc7Identity    ∷ !(Maybe WorldIdentityDTO)
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTOv7 = WorldPagesDTOv7 { wpd7Pages ∷ [PageCoreDTOv7] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The FROZEN v8 wire shape (#916 through #2021): the current page
--   identity, but no generated-world id — v8 predates
--   'GeneratedWorldId' entirely, which is exactly why
--   'migrateWorldPagesV8' leaves it absent rather than inventing one.
--   Never edited; a further schema change adds a newer type instead
--   (frozen-DTO boundary rule).
--
--   Its gen params are 'WorldGenParamsDTOv7', the frozen pre-#917
--   shape — #917 was the "later schema change" master's own note here
--   anticipated, so the field was repointed off the current type onto
--   the frozen copy exactly as described, leaving these bytes
--   unchanged. A v8 payload's locations carry #916's encounter, with
--   its clearance-notice flag still nested inside it, and no
--   significant-item obligations.
data PageCoreDTOv8 = PageCoreDTOv8
    { pc8PageId      ∷ !WorldPageId
    , pc8GenParams   ∷ !WorldGenParamsDTOv7
    , pc8CameraX     ∷ !Float
    , pc8CameraY     ∷ !Float
    , pc8TimeHour    ∷ !Int
    , pc8TimeMinute  ∷ !Int
    , pc8DateYear    ∷ !Int
    , pc8DateMonth   ∷ !Int
    , pc8DateDay     ∷ !Int
    , pc8MapMode     ∷ !ZoomMapMode
    , pc8Identity    ∷ !(Maybe WorldIdentityDTO)
      -- ^ the CURRENT identity shape: neither #2021 nor #917 changed
      --   it. Its gen-params sibling above was repointed by #917, which
      --   is the repointing the frozen-DTO boundary rule prescribes and
      --   'PageCoreDTOv2' documents for its own.
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTOv8 = WorldPagesDTOv8 { wpd8Pages ∷ [PageCoreDTOv8] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The canonical decoded value of the @world-pages@ component, kept
--   separate from either wire DTO ("World.Save.Component.Types": the
--   canonical type a codec decodes INTO is the migration target). It is
--   the base 'PageSnapshot' map every other page-scoped component then
--   writes onto, plus the page ids in encoded order — the map alone
--   cannot answer the duplicate-page-id invariant, since a 'HM.HashMap'
--   silently collapses a duplicate key.
data WorldPages = WorldPages
    { wpPageIds ∷ ![WorldPageId]
    , wpBase    ∷ !(HM.HashMap WorldPageId PageSnapshot)
    , wpIdsFromPayload ∷ !Bool
      -- ^ #2021: whether the payload this value was decoded FROM carries
      --   generated-world ids at all — 'True' for v9 (and every later
      --   version that keeps the field), 'False' for every migrated
      --   pre-v9 payload.
      --
      --   This is the one fact @validatePages@ cannot recover from the
      --   decoded pages themselves, and it decides the meaning of an
      --   absent id: in a v9 payload it is corruption (the writer had an
      --   id and did not write it), while in a migrated v8 payload it is
      --   the correct value (the format had no such field), which load
      --   staging then fills with a fresh id. Carrying it here rather
      --   than inferring it is what lets ONE validator hold both rules
      --   without guessing which version it is looking at.
    } deriving (Show)

-- | Encoding always writes the current v10 shape; v9 payloads decode
--   through their own frozen DTO via 'migrateWorldPagesV9' (#917), v8
--   via 'migrateWorldPagesV8' (#2021), v7
--   via 'migrateWorldPagesV7' (#916), v6
--   via 'migrateWorldPagesV6' (#1230), v5
--   via 'migrateWorldPagesV5' (#1104), v4
--   via 'migrateWorldPagesV4' (#1102), v3 via 'migrateWorldPagesV3'
--   (#1101), v2 via 'migrateWorldPagesV2'
--   (#1092), and v1 via 'migrateWorldPagesV1' (#911). Issue #1093: this
--   used to be a hand-rolled 'ComponentCodec' because the shared helper
--   had no real multi-version dispatch — 'componentCodec' now expresses
--   it, with each accepted version declared exactly once.
worldPagesCodec ∷ ComponentCodec WorldPages
worldPagesCodec = componentCodec ComponentSpec
    { csComponent     = worldPagesComponentId
    , csVersion       = 10
    , csRequired      = True
    , csDeps          = []
    , csEncode        = \snap →
        WorldPagesDTO (map toPageCore (orderedPages snap))
    , csDecode        = basePageSnapshots
    , csOlderVersions = [ atVersion 9 migrateWorldPagesV9
                        , atVersion 8 migrateWorldPagesV8
                        , atVersion 7 migrateWorldPagesV7
                        , atVersion 6 migrateWorldPagesV6
                        , atVersion 5 migrateWorldPagesV5
                        , atVersion 4 migrateWorldPagesV4
                        , atVersion 3 migrateWorldPagesV3
                        , atVersion 2 migrateWorldPagesV2
                        , atVersion 1 migrateWorldPagesV1 ]
    , csValidate      = validatePages
    }
  where
    toPageCore p = PageCoreDTO
        { pcPageId     = pgsPageId p
        , pcGenParams  = toWorldGenParamsDTO (pgsGenParams p)
        , pcCameraX    = pgsCameraX p
        , pcCameraY    = pgsCameraY p
        , pcTimeHour   = pgsTimeHour p
        , pcTimeMinute = pgsTimeMinute p
        , pcDateYear   = pgsDateYear p
        , pcDateMonth  = pgsDateMonth p
        , pcDateDay    = pgsDateDay p
        , pcMapMode    = pgsMapMode p
        , pcIdentity   = toWorldIdentityDTO <$> pgsIdentity p
        -- #2021: written straight through, never fabricated. Every
        -- CAPTURED page carries one (the live ref is not optional), so
        -- this is 'Just' for every save this build writes; @validatePages@
        -- is what refuses a v9 payload that somehow says otherwise.
        , pcGeneratedId = pgsGeneratedId p
        }

-- | Component-local invariant (requirement 3): the page-set authority
--   must not itself carry a duplicate or empty page set. Hoisted to top
--   level so "World.Save.Compat.SessionV90"'s B1
--   migration path can run the SAME validator a modern envelope's
--   decode always does, rather than skip it entirely.
validatePages ∷ WorldPages → [ComponentError]
validatePages wp
    | null (wpPageIds wp) = [err "no world pages in save"]
    | otherwise =
        [ err ("duplicate page id " <> tshow pid)
        | (pid, n) ← HM.toList
                      (HM.fromListWith (+) [ (p, 1 ∷ Int) | p ← wpPageIds wp ])
        , n > 1 ]
        -- #2021: a payload whose own version carries generated-world ids
        -- must carry one for EVERY page. Gated on 'wpIdsFromPayload'
        -- because the identical shape is correct for a migrated pre-v9
        -- payload, where absence is the format's answer rather than a
        -- missing value — load staging mints a fresh id for those.
        ⧺ [ err ("page " <> tshow (pgsPageId p)
                 <> " carries no generated-world id")
          | wpIdsFromPayload wp
          , p ← HM.elems (wpBase wp)
          , isNothing (pgsGeneratedId p) ]
        -- #2021: two pages in one save naming the SAME generated
        -- foundation. Nothing in the engine can produce it (each page
        -- mints its own at creation and staging keeps them distinct),
        -- and downstream slices key durable per-world artifacts by this
        -- id, so a collision must be refused rather than silently
        -- collapsed the way a HashMap would.
        ⧺ [ err ("duplicate generated-world id "
                 <> renderGeneratedWorldId gid)
          | gid ← duplicates [ g | p ← HM.elems (wpBase wp)
                                 , Just g ← [pgsGeneratedId p] ] ]
        -- #911: the page-local location-instance allocator, mirroring
        -- @world-activity@'s own ground-item allocator check. #1668
        -- adds the table's GEOMETRY beside its ids: the save decode
        -- path is the one place an 'Location.Bounds.AbsBounds' is built
        -- from unrestricted wire 'Int's rather than downstream of the
        -- YAML loader's inverted-bounds gate, so an inverted stored
        -- footprint is rejected HERE -- in ValidatePhase, after every
        -- accepted version has migrated into this one canonical value
        -- -- rather than being published as spatial authority.
        ⧺ [ err ("page '" <> tshow (pgsPageId p) <> "': " <> msg)
          | p   ← HM.elems (wpBase wp)
          , let lis = wgpLocationInstances (pgsGenParams p)
          , msg ← locationInstanceAllocatorErrors lis
                    ⧺ locationInstanceBoundsErrors lis
                    ⧺ locationSignificantItemErrors lis
          ]
  where
    err = ComponentError worldPagesComponentId 10 ValidatePhase
    -- Each repeated value once, in ascending order, so the report is
    -- deterministic rather than a hash-map traversal order.
    duplicates xs = [ y | (y : _ : _) ← L.group (L.sort xs) ]

-- | Turn the decoded current v10 page cores into the base 'PageSnapshot' map every
--   other page-scoped component then writes onto (assembly). All entity/
--   activity/edit fields start empty and are overwritten by their own
--   REQUIRED components; a valid save leaves none of these placeholders.
basePageSnapshots ∷ WorldPagesDTO → WorldPages
basePageSnapshots (WorldPagesDTO ps) = WorldPages
    { wpPageIds = map pcPageId ps
    , wpBase    = HM.fromList [ (pcPageId p, toBase p) | p ← ps ]
    , wpIdsFromPayload = True
    }
  where
    toBase p = (blankPageSnapshot (pcPageId p)
                    (fromWorldGenParamsDTO (pcGenParams p)))
        { pgsCameraX    = pcCameraX p
        , pgsCameraY    = pcCameraY p
        , pgsTimeHour   = pcTimeHour p
        , pgsTimeMinute = pcTimeMinute p
        , pgsDateYear   = pcDateYear p
        , pgsDateMonth  = pcDateMonth p
        , pgsDateDay    = pcDateDay p
        , pgsMapMode    = pcMapMode p
        , pgsIdentity   = fromWorldIdentityDTO <$> pcIdentity p
        , pgsGeneratedId = pcGeneratedId p
        }

-- | The v8→v9 migration (#2021): every field a v8 page carries rides
--   across untouched, and its generated-world id is left ABSENT.
--
--   That absence is the whole point, not a gap. A v8 save predates
--   generated-world identity, so there is nothing in it an id could
--   honestly be recovered from — and deriving one from the seed, the
--   gen params, the page id or the display name is exactly the content
--   fingerprinting requirement 3 rejects (two worlds generated from the
--   same seed are DIFFERENT worlds and must receive DIFFERENT ids).
--   Nor could this function mint one: a migration is pure, and minting
--   needs real entropy.
--
--   So the id is filled in one step later, by transactional load
--   staging ("World.Load.Stage"), which mints a fresh one per page —
--   and never writes it back to the file it came from. Loading the same
--   unchanged v8 save twice therefore yields two different ids, which
--   is accepted behaviour (design decision D-21): the earlier one
--   simply belongs to a session nobody saved.
migrateWorldPagesV8 ∷ WorldPagesDTOv8 → WorldPages
migrateWorldPagesV8 (WorldPagesDTOv8 ps) = WorldPages
    { wpPageIds = map pc8PageId ps
    , wpBase    = HM.fromList [ (pc8PageId p, toBase p) | p ← ps ]
    , wpIdsFromPayload = False
    }
  where
    toBase p = (blankPageSnapshot (pc8PageId p)
                    (fromWorldGenParamsDTOv7 (pc8GenParams p)))
        { pgsCameraX    = pc8CameraX p
        , pgsCameraY    = pc8CameraY p
        , pgsTimeHour   = pc8TimeHour p
        , pgsTimeMinute = pc8TimeMinute p
        , pgsDateYear   = pc8DateYear p
        , pgsDateMonth  = pc8DateMonth p
        , pgsDateDay    = pc8DateDay p
        , pgsMapMode    = pc8MapMode p
        , pgsIdentity   = fromWorldIdentityDTO <$> pc8Identity p
        }

-- | The FROZEN v9 wire shape (#2021 through #917): #2021's page,
--   generated-world id and all, over the frozen pre-significant-contents
--   worldgen/location DTO ('WorldGenParamsDTOv7') — whose locations
--   carry #916's encounter, with its clearance-notice flag still nested
--   inside it, and no significant-item obligations. Never edited; a
--   further schema change adds a newer type instead (frozen-DTO
--   boundary rule).
data PageCoreDTOv9 = PageCoreDTOv9
    { pc9PageId      ∷ !WorldPageId
    , pc9GenParams   ∷ !WorldGenParamsDTOv7
    , pc9CameraX     ∷ !Float
    , pc9CameraY     ∷ !Float
    , pc9TimeHour    ∷ !Int
    , pc9TimeMinute  ∷ !Int
    , pc9DateYear    ∷ !Int
    , pc9DateMonth   ∷ !Int
    , pc9DateDay     ∷ !Int
    , pc9MapMode     ∷ !ZoomMapMode
    , pc9Identity    ∷ !(Maybe WorldIdentityDTO)
    , pc9GeneratedId ∷ !(Maybe GeneratedWorldId)
    } deriving (Show, Generic, Serialize)

newtype WorldPagesDTOv9 = WorldPagesDTOv9 { wpd9Pages ∷ [PageCoreDTOv9] }
    deriving stock (Generic)
    deriving newtype (Show, Serialize)

-- | The v9→v10 migration (#917): every historical placed location keeps
--   its exact stored identity, geometry, name, lifecycle, content flag
--   and encounter — including whether that encounter had already been
--   completed and whether its clearance notice had been spent, which is
--   lifted out of the encounter onto the instance where the
--   generalized latch now lives
--   ('World.Save.Component.WorldGen.fromLocationInstanceDTOv5'). So a
--   ruin defeated before it was ever seen still announces itself once
--   on sight, and one that already announced never announces again.
--
--   #2021's generated-world id rides across untouched, and
--   'wpIdsFromPayload' stays TRUE: a v9 payload DOES carry the field, so
--   an absent id in one is corruption exactly as it is in a v10 payload,
--   and @validatePages@ must keep saying so. That is the one thing this
--   migration must not borrow from 'migrateWorldPagesV8', which reports
--   'False' because v8 genuinely predates the field.
--
--   It gains NO significant-item obligations. Reading them off today's
--   YAML would hand a previously materialized world an item it never
--   spawned and nobody could take, permanently blocking a clearance the
--   pre-#917 build had already granted on the encounter alone — the
--   same reason 'migrateWorldPagesV7' refuses to roll an encounter, and
--   the same reason the v1 reconstruction discards both.
migrateWorldPagesV9 ∷ WorldPagesDTOv9 → WorldPages
migrateWorldPagesV9 (WorldPagesDTOv9 ps) = WorldPages
    { wpPageIds = map pc9PageId ps
    , wpBase    = HM.fromList [ (pc9PageId p, toBase p) | p ← ps ]
    , wpIdsFromPayload = True
    }
  where
    toBase p = (blankPageSnapshot (pc9PageId p)
                    (fromWorldGenParamsDTOv7 (pc9GenParams p)))
        { pgsCameraX    = pc9CameraX p
        , pgsCameraY    = pc9CameraY p
        , pgsTimeHour   = pc9TimeHour p
        , pgsTimeMinute = pc9TimeMinute p
        , pgsDateYear   = pc9DateYear p
        , pgsDateMonth  = pc9DateMonth p
        , pgsDateDay    = pc9DateDay p
        , pgsMapMode    = pc9MapMode p
        , pgsIdentity   = fromWorldIdentityDTO <$> pc9Identity p
        , pgsGeneratedId = pc9GeneratedId p
        }

-- | The v7→v8 migration (#916): every historical placed location keeps
--   its exact stored identity, geometry, name, lifecycle, and content flag,
--   and gains no encounter. Rolling an encounter while loading would let the
--   current content build reinterpret a previously materialized world.
migrateWorldPagesV7 ∷ WorldPagesDTOv7 → WorldPages
migrateWorldPagesV7 (WorldPagesDTOv7 ps) = WorldPages
    { wpPageIds = map pc7PageId ps
    , wpBase    = HM.fromList [ (pc7PageId p, toBase p) | p ← ps ]
    , wpIdsFromPayload = False
    }
  where
    toBase p = (blankPageSnapshot (pc7PageId p)
                    (fromWorldGenParamsDTOv6 (pc7GenParams p)))
        { pgsCameraX    = pc7CameraX p
        , pgsCameraY    = pc7CameraY p
        , pgsTimeHour   = pc7TimeHour p
        , pgsTimeMinute = pc7TimeMinute p
        , pgsDateYear   = pc7DateYear p
        , pgsDateMonth  = pc7DateMonth p
        , pgsDateDay    = pc7DateDay p
        , pgsMapMode    = pc7MapMode p
        , pgsIdentity   = fromWorldIdentityDTO <$> pc7Identity p
        }

-- | The v6 migration (#1230): decode the frozen v6 page cores into the
--   same base 'PageSnapshot' map. The ONLY difference is each location
--   instance's stored @discovery_margin@, which is DROPPED — the live
--   'Location.Instance.LocationInstance' has no such field any more,
--   because reveal became sight-based against the instance's own
--   bounds. Everything else about every instance rides across
--   untouched: its allocator, id, definition id, chunk, anchor, bounds,
--   display name, gloss, etymology source, lifecycle and
--   contents-spawned flag — as do the page's own identity, river names,
--   clocks, camera and map mode. A ruin a pre-#1230 save had already
--   discovered therefore stays discovered, and one it had not is
--   rediscovered by sight rather than by walking into a halo.
migrateWorldPagesV6 ∷ WorldPagesDTOv6 → WorldPages
migrateWorldPagesV6 (WorldPagesDTOv6 ps) = WorldPages
    { wpPageIds = map pc6PageId ps
    , wpBase    = HM.fromList [ (pc6PageId p, toBase p) | p ← ps ]
    , wpIdsFromPayload = False
    }
  where
    toBase p = (blankPageSnapshot (pc6PageId p)
                    (fromWorldGenParamsDTOv5 (pc6GenParams p)))
        { pgsCameraX    = pc6CameraX p
        , pgsCameraY    = pc6CameraY p
        , pgsTimeHour   = pc6TimeHour p
        , pgsTimeMinute = pc6TimeMinute p
        , pgsDateYear   = pc6DateYear p
        , pgsDateMonth  = pc6DateMonth p
        , pgsDateDay    = pc6DateDay p
        , pgsMapMode    = pc6MapMode p
        , pgsIdentity   = fromWorldIdentityDTO <$> pc6Identity p
        }

-- | The v5 migration (#1104): decode the frozen v5 page cores into the
--   same base 'PageSnapshot' map. The ONLY difference is the optional
--   etymology source, which comes back ABSENT on all three of the
--   things that can carry one — the page's own identity, each location
--   instance, and each river name. A save written before #1104 recorded
--   no expressions, and one is never reconstructed after the fact from
--   a stored name, gloss, entity type, id, or content definition
--   (#1104 requirement 1). Those names and glosses themselves, the
--   page's language provenance, the location instances with their
--   lifecycles, the river-name table with its ids, clocks, camera, and
--   map mode all ride across untouched — so a pre-#1104 save keeps
--   every name it had and simply reports its etymology as unavailable.
migrateWorldPagesV5 ∷ WorldPagesDTOv5 → WorldPages
migrateWorldPagesV5 (WorldPagesDTOv5 ps) = WorldPages
    { wpPageIds = map pc5PageId ps
    , wpBase    = HM.fromList [ (pc5PageId p, toBase p) | p ← ps ]
    , wpIdsFromPayload = False
    }
  where
    toBase p = (blankPageSnapshot (pc5PageId p)
                    (fromWorldGenParamsDTOv4 (pc5GenParams p)))
        { pgsCameraX    = pc5CameraX p
        , pgsCameraY    = pc5CameraY p
        , pgsTimeHour   = pc5TimeHour p
        , pgsTimeMinute = pc5TimeMinute p
        , pgsDateYear   = pc5DateYear p
        , pgsDateMonth  = pc5DateMonth p
        , pgsDateDay    = pc5DateDay p
        , pgsMapMode    = pc5MapMode p
        , pgsIdentity   = fromWorldIdentityDTOv2 <$> pc5Identity p
        }

-- | The v4 migration (#1102): decode the frozen v4 page cores into the
--   same base 'PageSnapshot' map. The ONLY difference is the per-page
--   river-name table, which comes back EMPTY
--   ('World.Save.Component.WorldGenHistory.fromWorldGenParamsDTOv3'): a save
--   written before #1102 named no rivers, and a name is never inferred
--   after the fact for a page whose language it was not rendered from
--   (#1102 requirements 5 and 6). Its rivers still carry ids, which are
--   derived from the timeline the page already stores, so the identity
--   half of the feature works on a pre-#1102 save with no migration at
--   all. Everything else — identity with its provenance, location
--   instances with their stored names and glosses, clocks, camera, map
--   mode — rides across untouched.
migrateWorldPagesV4 ∷ WorldPagesDTOv4 → WorldPages
migrateWorldPagesV4 (WorldPagesDTOv4 ps) = WorldPages
    { wpPageIds = map pc4PageId ps
    , wpBase    = HM.fromList [ (pc4PageId p, toBase p) | p ← ps ]
    , wpIdsFromPayload = False
    }
  where
    toBase p = (blankPageSnapshot (pc4PageId p)
                    (fromWorldGenParamsDTOv3 (pc4GenParams p)))
        { pgsCameraX    = pc4CameraX p
        , pgsCameraY    = pc4CameraY p
        , pgsTimeHour   = pc4TimeHour p
        , pgsTimeMinute = pc4TimeMinute p
        , pgsDateYear   = pc4DateYear p
        , pgsDateMonth  = pc4DateMonth p
        , pgsDateDay    = pc4DateDay p
        , pgsMapMode    = pc4MapMode p
        , pgsIdentity   = fromWorldIdentityDTOv2 <$> pc4Identity p
        }

-- | The v3 migration (#1101): decode the frozen v3 page cores into the
--   same base 'PageSnapshot' map. The ONLY difference is the per-page
--   LOCATION instances, whose stored display names carry across
--   EXACTLY — a location named before this landing keeps that name
--   forever (#1101 requirements 4 and 7), and is not renamed into the
--   world's language on upgrade — while each gains no gloss
--   ('World.Save.Component.WorldGenNaming.fromLocationInstanceDTOv1'). The
--   page's own identity, provenance included, rides across untouched:
--   #1101 changed no world-identity field.
migrateWorldPagesV3 ∷ WorldPagesDTOv3 → WorldPages
migrateWorldPagesV3 (WorldPagesDTOv3 ps) = WorldPages
    { wpPageIds = map pc3PageId ps
    , wpBase    = HM.fromList [ (pc3PageId p, toBase p) | p ← ps ]
    , wpIdsFromPayload = False
    }
  where
    toBase p = (blankPageSnapshot (pc3PageId p)
                    (fromWorldGenParamsDTOv2 (pc3GenParams p)))
        { pgsCameraX    = pc3CameraX p
        , pgsCameraY    = pc3CameraY p
        , pgsTimeHour   = pc3TimeHour p
        , pgsTimeMinute = pc3TimeMinute p
        , pgsDateYear   = pc3DateYear p
        , pgsDateMonth  = pc3DateMonth p
        , pgsDateDay    = pc3DateDay p
        , pgsMapMode    = pc3MapMode p
        , pgsIdentity   = fromWorldIdentityDTOv2 <$> pc3Identity p
        }

-- | The v2 migration (#1092): decode the frozen v2 page cores into
--   the same base 'PageSnapshot' map. The identity difference is the
--   headline — every v2 page's name and gloss carry across byte-exact
--   while its language provenance decodes ABSENT
--   (@fromWorldIdentityDTOv1@), never inferred from the world seed or
--   the name text. Its gen params are the frozen pre-#1101 shape, so
--   its location instances likewise keep their stored names and gain
--   no gloss. Clocks, camera, and map mode ride across untouched.
migrateWorldPagesV2 ∷ WorldPagesDTOv2 → WorldPages
migrateWorldPagesV2 (WorldPagesDTOv2 ps) = WorldPages
    { wpPageIds = map pc2PageId ps
    , wpBase    = HM.fromList [ (pc2PageId p, toBase p) | p ← ps ]
    , wpIdsFromPayload = False
    }
  where
    toBase p = (blankPageSnapshot (pc2PageId p)
                    (fromWorldGenParamsDTOv2 (pc2GenParams p)))
        { pgsCameraX    = pc2CameraX p
        , pgsCameraY    = pc2CameraY p
        , pgsTimeHour   = pc2TimeHour p
        , pgsTimeMinute = pc2TimeMinute p
        , pgsDateYear   = pc2DateYear p
        , pgsDateMonth  = pc2DateMonth p
        , pgsDateDay    = pc2DateDay p
        , pgsMapMode    = pc2MapMode p
        , pgsIdentity   = fromWorldIdentityDTOv1 <$> pc2Identity p
        }

-- | The v1 migration (#911): decode the frozen v1 page cores into the
--   same base 'PageSnapshot' map, with each page's gen params rebuilt by
--   'fromWorldGenParamsDTOv1' — which leaves the instance table empty and
--   the page's old per-chunk discovered / contents-spawned sets PENDING
--   on it. Turning those into instances needs each definition's
--   bounds / label — since #1230 there is no margin to resolve, reveal
--   being sight against those bounds — and no component decoder has the
--   location registry, so the load path resolves them
--   ('Location.Instance.resolveLegacyLocationInstances') at its
--   content-validation stage before publication.
--   @wgpLocationStamped@ rides across untouched — it stays a chunk
--   property (#424). Its identity decodes with #1092's language
--   provenance absent as well: a pre-#911 save predates provenance
--   entirely.
migrateWorldPagesV1 ∷ WorldPagesDTOv1 → WorldPages
migrateWorldPagesV1 (WorldPagesDTOv1 ps) = WorldPages
    { wpPageIds = map pc1PageId ps
    , wpBase    = HM.fromList [ (pc1PageId p, toBase p) | p ← ps ]
    , wpIdsFromPayload = False
    }
  where
    toBase p = (blankPageSnapshot (pc1PageId p)
                    (fromWorldGenParamsDTOv1 (pc1GenParams p)))
        { pgsCameraX    = pc1CameraX p
        , pgsCameraY    = pc1CameraY p
        , pgsTimeHour   = pc1TimeHour p
        , pgsTimeMinute = pc1TimeMinute p
        , pgsDateYear   = pc1DateYear p
        , pgsDateMonth  = pc1DateMonth p
        , pgsDateDay    = pc1DateDay p
        , pgsMapMode    = pc1MapMode p
        , pgsIdentity   = fromWorldIdentityDTOv1 <$> pc1Identity p
        }

-- | The zeroed base 'PageSnapshot' the v6, v5, v4, v3, v2, and v1 paths above
--   all build on, so they can never drift in which placeholder fields
--   they leave for the other components to fill. Each caller record-updates
--   the page-core scalars it decoded; everything left here is a
--   placeholder a REQUIRED component overwrites during assembly.
blankPageSnapshot ∷ WorldPageId → WorldGenParams → PageSnapshot
blankPageSnapshot pid params =
    PageSnapshot
        { pgsPageId       = pid
        , pgsGenParams    = params
        , pgsCameraX      = 0
        , pgsCameraY      = 0
        , pgsTimeHour     = 0
        , pgsTimeMinute   = 0
        , pgsDateYear     = 0
        , pgsDateMonth    = 0
        , pgsDateDay      = 0
        , pgsMapMode      = ZMDefault
        , pgsIdentity     = Nothing
        -- #2021: every legacy migration builds on this, and 'Nothing'
        -- is the honest answer for all of them — a pre-v9 payload
        -- carries no generated-world id, and load staging mints a fresh
        -- one rather than deriving one from the save's contents.
        , pgsGeneratedId  = Nothing
        , pgsEdits        = emptyWorldEdits
        , pgsMineDesignations      = HM.empty
        , pgsConstructDesignations = HM.empty
        , pgsConstructNextAttempt  = firstConstructAttemptId
        , pgsGroundItems  = emptyGroundItems
        , pgsSpoilPiles   = emptySpoilPiles
        , pgsBuildings    = BuildingSnapshot { bsnInstances = HM.empty, bsnNextId = 0 }
        , pgsUnits        = UnitSnapshot { usnInstances = HM.empty, usnNextId = 0 }
        , pgsUnitSimStates = HM.empty
        , pgsFloraHarvests = emptyFloraHarvests
        , pgsChopDesignations = HM.empty
        , pgsPendingChopMigration = HM.empty
        , pgsPendingFloraHarvests = emptyPendingFloraHarvests
        , pgsPlantedFloraCursor = firstPlantedFloraCursor
        , pgsCraftBills   = emptyCraftBills
        , pgsPowerNodes   = emptyPowerNodes
          -- #1087: the FIRST of the two defaults genuinely reached in a
          -- successful load. @"container-knowledge"@ is the first
          -- OPTIONAL gameplay component, so a save written before it
          -- existed carries no such payload at all and every page keeps
          -- this empty map — which is exactly right: every container in
          -- a pre-#1087 session is never-inspected, never known-empty,
          -- and never inferred from its live contents.
        , pgsContainerKnowledge = emptyContainerKnowledge
          -- #1246: the SECOND default genuinely reached in a successful
          -- load, for the same reason. @"transfer-orders"@ is the second
          -- OPTIONAL gameplay component, so a save written before it
          -- existed carries no such payload and every page keeps this
          -- empty queue — which is exactly right: no order could have
          -- been queued in a session that had nowhere to store one, and
          -- the allocator starts where a fresh page's does.
        , pgsTransferOrders = emptyTransferOrders
        , pgsTillDesignations = HM.empty
        , pgsCropPlots    = HM.empty
        , pgsPlantDesignations = HM.empty
        }
