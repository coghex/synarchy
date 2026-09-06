{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
-- | Naming a world's rivers in its own generated language (#1102, epic
--   #708). Rivers are the arc's second real surface area after placed
--   locations (#1101): they are numerous, spread across the whole map,
--   and permanent, so a head morpheme recurring across several river
--   names — and in the world's own name — is exactly the repetition
--   that makes a generated language legible.
--
--   /Where the names live./ NOT on 'World.Types.PersistentFeature'.
--   'World.Geology.Timeline.Types.GeoTimeline' is a positionally
--   serialized worldgen-OUTPUT schema, and a river's name has nothing
--   to do with terrain: putting it there would drag a labelling change
--   through the worldgen baselines. Instead a page carries a separate
--   'RiverNames' table beside its location instances
--   ('World.Generate.Types.wgpRiverNames'), keyed by the
--   'GeoFeatureId' the timeline already allocated. The durable identity
--   is therefore @(WorldPageId, GeoFeatureId)@ — feature ids restart at
--   zero for each timeline, so a river id is only ever meaningful
--   against the page it came from.
--
--   /Write-once./ #708 principle 5, which forbids re-deriving a
--   persisted name on load or migration however stable assignment
--   happens to be. A name is rendered when the page's table is BUILT,
--   at world init, and read thereafter.
--
--   What the rule guards against is worth stating precisely, because
--   the obvious answer is not the real one. Root assignment is NOT the
--   drift: 'Language.Generated.Root.assignLanguageRoots' places
--   concepts in the catalogue's append-only ordinal order (#1868), so
--   an addition leaves every existing concept's FREE root exactly as
--   it was. What a stable free root does not buy is a stable
--   RE-RENDERING, and at least two mechanisms behind these names read
--   the catalogue as it stands at the moment they run.
--   'riverModifierPool' is every concept the catalogue can express
--   attributively, and 'riverNameExpr' draws from it with
--   'Language.Generated.Hash.pickIndex' against that pool's CURRENT
--   length, so one eligible concept added anywhere re-points the draw
--   and changes the modifier, the gloss and the native name. And from
--   generator version 4 on, bound-form selection ranks the complete
--   current concept set ('Language.Generated.Bound.assignBoundForms',
--   #1096), so an addition can still move a selected bound form and
--   every rendered name that uses one. That describes the mechanisms
--   as built rather than promising anything of a future change to
--   them; the rule holds either way. A river named under one catalogue
--   keeps that name forever.
--
--   /No language, no invention./ Provenance is optional by design
--   (#1092 requirement 2): a custom-named world has no language, and a
--   world saved before provenance was recorded has none recoverable.
--   Both leave the table EMPTY — every river keeps its id and has no
--   name, and every consumer handles that. Absence is never papered
--   over by inventing a language.
--
--   Pure: no engine, world, Lua, IO, or wall-clock state.
module World.River.Naming
    ( -- * The per-page table
      RiverName(..)
    , RiverNames(..)
    , emptyRiverNames
    , lookupRiverName
    , riverNamesToList
      -- * Naming
    , riverHeadConcepts
    , riverHeadPool
    , riverModifierPool
    , buildRiverNames
    ) where

import UPrelude
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.List (sortOn)
import Data.Serialize (Serialize)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Language.Generated.Hash (draw, pickIndex)
import Language.Etymology.Source (EtymologySource(..))
import Language.Naming
    ( Namer(..), nameDrawSeed, namerProvenance, renderNamed )
import Language.Semantic.Types
    ( Catalogue, ConceptId(..), FormKind(..), NameExpr(..), conceptIds
    , formOf, lookupConcept )
import World.Base (GeoFeatureId(..))

-- * The per-page table ------------------------------------------------

-- | One river's stored name, rendered ONCE and read thereafter.
data RiverName = RiverName
    { rvnDisplayName ∷ !Text
      -- ^ native text in the page's own generated language
    , rvnGloss       ∷ !(Maybe Text)
      -- ^ the English reading of the SAME name expression, mirroring
      --   'World.Page.Types.wiGloss' and
      --   'Location.Instance.liGloss'. Always 'Just' for a stored
      --   entry — a river with nothing to name it simply has no entry
      --   at all — but the field keeps the shape those two already
      --   established, so a later feature that can store a name without
      --   a meaning has somewhere to say so.
    , rvnEtymology   ∷ !(Maybe EtymologySource)
      -- ^ what 'rvnDisplayName' was rendered FROM (#1104), the third
      --   copy of the same shape 'World.Page.Types.wiEtymology' and
      --   'Location.Instance.liEtymology' carry: the originating
      --   expression plus the provenance that rendered it. Written ONCE
      --   with the name; absent for every river named before #1104, and
      --   never inferred afterwards.
    } deriving (Show, Eq, Generic, NFData, Serialize)

-- | A page's river-name table. Sparse by construction: a page with no
--   language has an empty one, and a river with no entry is unnamed.
--
--   Keyed by 'GeoFeatureId' rather than by anything positional, so a
--   name survives save/load, chunk eviction, and any later change to
--   how rivers are enumerated. The keying is deliberately the ONLY
--   thing river-specific about this shape — a later issue naming lakes
--   or peaks reuses it without touching the timeline again.
newtype RiverNames = RiverNames { rvnById ∷ HM.HashMap GeoFeatureId RiverName }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)
    deriving newtype (Serialize)

emptyRiverNames ∷ RiverNames
emptyRiverNames = RiverNames HM.empty

lookupRiverName ∷ GeoFeatureId → RiverNames → Maybe RiverName
lookupRiverName fid = HM.lookup fid . rvnById

-- | Every stored name, ordered by feature id — a canonical total
--   order, never 'HM.HashMap' iteration order, so anything that
--   surfaces or reports names is deterministic call over call (the same
--   reason 'Location.Instance.instancesToList' sorts).
riverNamesToList ∷ RiverNames → [(GeoFeatureId, RiverName)]
riverNamesToList = sortOn fst . HM.toList . rvnById

-- * Naming -----------------------------------------------------------

-- | The kind tag mixed into every river's draw seed, so a river and a
--   location instance sharing a raw id never draw the same concepts.
riverNameKindTag ∷ Text
riverNameKindTag = "river"

-- | The concepts a river's HEAD may be drawn from, in a fixed order.
--
--   Deliberately small: heads are what a player sees RECUR across a
--   map's rivers and in the world's own name, and a pool the size of
--   the catalogue would make every river's head unique and the language
--   unreadable. These are the catalogue's watercourse-shaped place
--   concepts — #1102 added @RIVER@ itself for exactly this.
--
--   In code rather than in data because a river has no definition file
--   to author a scheme on, unlike a location's
--   'Location.Types.ldNaming'. Ordered explicitly (not sorted at use)
--   so the pool a name was drawn from is a stable, reviewable list.
riverHeadConcepts ∷ [ConceptId]
riverHeadConcepts = map ConceptId
    [ "RIVER", "FORD", "CROSSING", "BAY", "VALE", "HOLLOW" ]

-- | 'riverHeadConcepts' restricted to those a given catalogue can
--   actually render as a 'Modifier' head (i.e. that carry a singular
--   form). Defensive: the shipped catalogue authors all four forms for
--   every concept, so nothing is dropped in practice — but a name half
--   rendered is worse than a river with no name, so a head the
--   catalogue cannot express is skipped rather than emitted raw.
riverHeadPool ∷ Catalogue → [ConceptId]
riverHeadPool cat = filter (hasForm cat FormSingular) riverHeadConcepts

-- | The concepts a river's MODIFIER may be drawn from: every concept
--   the catalogue can express attributively, in the catalogue's own
--   stable ascending order.
--
--   Wide on purpose, and the mirror image of the head pool. A river has
--   no author to curate a modifier list for it, and any curated subset
--   would be an arbitrary line drawn through a vocabulary that already
--   reads well in this slot across every domain (ashen, iron, wolf,
--   sorrow). Breadth here plus a narrow head pool is what produces
--   \"many different rivers, visibly the same word for river\".
riverModifierPool ∷ Catalogue → [ConceptId]
riverModifierPool cat = filter (hasForm cat FormModifier) (conceptIds cat)

hasForm ∷ Catalogue → FormKind → ConceptId → Bool
hasForm cat kind cid = case lookupConcept cid cat of
    Nothing → False
    Just ce → maybe False (not . T.null) (formOf kind ce)

-- | The name expression one river gets: a 'Modifier' compound, the same
--   shape #1101 gives a location.
--
--   Deterministic from the river's own identity — its 'GeoFeatureId',
--   which the timeline allocated monotonically and which persists — plus
--   the language's seed and version. Never 'HM.HashMap' iteration
--   order, never mutable RNG state, never wall-clock: the same seed
--   regenerates the same river names in a fresh process.
--
--   'Nothing' when either pool is empty against this catalogue, which
--   leaves the river unnamed rather than named under half a scheme.
riverNameExpr ∷ Namer → GeoFeatureId → Maybe NameExpr
riverNameExpr nmr (GeoFeatureId rawId)
    | null heads ∨ null mods = Nothing
    | otherwise              = Just (Modifier (pick 0 mods) (pick 1 heads))
  where
    heads = riverHeadPool (nmrCatalogue nmr)
    mods  = riverModifierPool (nmrCatalogue nmr)
    base  = nameDrawSeed (nmrProfile nmr) riverNameKindTag rawId
    -- Total: the guard above rejects an empty pool before this runs.
    pick step pool = pool !! pickIndex (draw base step) (length pool)

-- | One river's stored name and gloss, or 'Nothing' when the page has
--   no language ('Nothing' namer) or the expression will not render.
--   Both renderings come from the SAME 'NameExpr', so the gloss always
--   explains the name beside it.
nameRiver ∷ Maybe Namer → GeoFeatureId → Maybe RiverName
nameRiver Nothing    _   = Nothing
nameRiver (Just nmr) fid = do
    expr           ← riverNameExpr nmr fid
    (native, glos) ← renderNamed nmr expr
    pure RiverName
        { rvnDisplayName = native
        , rvnGloss       = Just glos
        , rvnEtymology   = Just EtymologySource
            { esExpr = expr, esLanguage = namerProvenance nmr }
        }

-- | Name every river a page has, once. The ONLY writer of a river name.
--
--   Called at world init with the page's active river features; with no
--   namer (a page with no language) the table comes back empty and
--   every river stays unnamed with its id intact.
buildRiverNames ∷ Maybe Namer → [GeoFeatureId] → RiverNames
buildRiverNames namer fids = RiverNames $ HM.fromList
    [ (fid, nm) | fid ← fids, Just nm ← [nameRiver namer fid] ]
