{-# LANGUAGE Strict, DeriveGeneric #-}
module Location.Types
    ( LocationContent(..)
    , LocationNaming(..)
    , LocationDef(..)
    , LocationRegistry(..)
    , emptyLocationRegistry
    , registerLocation
    , lookupLocation
    , allLocations
    , locationIconTextureName
    , locationUnknownIconTextureName
    , locationUnknownIconPath
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.List (sortOn)
import Language.Semantic.Types (ConceptId)
import Location.Anchor (LocationAnchor)
import Location.Bounds (RelBounds(..))

-- | One piece of content a location places when it is stamped — a
--   building, unit, ground item, or loot-table roll, addressed by its
--   raw id string. The ids are NOT validated or resolved here: the
--   content-spawning pass (#90) resolves them at spawn time. `kind` is
--   held as 'Text' rather than a sum, but the VOCABULARY is closed
--   (#1708) at the YAML boundary by
--   'Engine.Asset.YamlLocations.validContentKinds', which is the one
--   place to extend when a new kind is added; a value outside it never
--   reaches a registered def.
data LocationContent = LocationContent
    { lconKind     ∷ !Text            -- ^ "building" | "unit" | "item"
                                      --   | "loot_table" — the closed
                                      --   #1708 vocabulary
    , lconId       ∷ !Text            -- ^ raw id string, resolved at spawn time
    , lconCount    ∷ !Int             -- ^ how many to place (defaults to 1;
                                      --   ignored by "loot_table", which
                                      --   uses 'lconRolls' instead)
    , lconPosition ∷ !(Maybe (Int, Int))
                                      -- ^ fixed (x, y) offset from the
                                      --   location anchor; 'Nothing' scatters
                                      --   randomly within the location's own
                                      --   declared bounds instead
    , lconFaction  ∷ !(Maybe Text)    -- ^ "unit" only: spawn-time faction tag
                                      --   (defaults to "hostile" when omitted)
    , lconRolls    ∷ !Int             -- ^ "loot_table" only: how many times
                                      --   to roll the table (defaults to 1)
    , lconCountRange ∷ !(Maybe (Int, Int))
                                      -- ^ "unit" only: inclusive uniform
                                      --   encounter count range. Distinct
                                      --   from the always-positive authored
                                      --   'lconCount' multiplicity because
                                      --   zero is a meaningful encounter
                                      --   outcome, not an empty spawn entry.
    , lconClearance ∷ !(Maybe Text)    -- ^ Encounter-only authored terminal
                                      --   policy. #916 supports explicit
                                      --   @death_only@; it is not a global
                                      --   default for future encounters.
    , lconSignificant ∷ !Bool         -- ^ #917: is this a GUARANTEED
                                      --   SIGNIFICANT item — one the
                                      --   location's clearance predicate
                                      --   waits on — rather than incidental
                                      --   salvage? Legal only on
                                      --   @kind: item@ (the YAML boundary
                                      --   rejects it anywhere else), so a
                                      --   @loot_table@ draw can never
                                      --   participate in clearance no matter
                                      --   what it rolls. Defaults to 'False':
                                      --   an entry is incidental unless it
                                      --   says otherwise.
    } deriving (Show, Eq, Generic)

-- | The authored naming scheme a definition's placed instances draw
--   their generated names from (#1101): two ordered, nonempty pools of
--   #709 concept ids. Every generated location name is one
--   @'Language.Semantic.Types.Modifier' modifier head@ expression, with
--   the modifier drawn from 'lnModifiers' and the head from 'lnHeads' —
--   which pools a location may draw on is DATA, never a hardcoded
--   mapping from 'ldType'.
--
--   Both pools are validated against the concept catalogue when the
--   definition loads ('Location.Naming.locationNamingErrors'): an empty
--   pool, an unknown concept id, or a concept missing the lexical form
--   its slot needs rejects the definition file outright rather than
--   silently degrading to 'ldLabel'.
data LocationNaming = LocationNaming
    { lnHeads     ∷ ![ConceptId]  -- ^ candidate head concepts (nonempty)
    , lnModifiers ∷ ![ConceptId]  -- ^ candidate modifier concepts (nonempty)
    } deriving (Show, Eq, Generic)

-- | A data-driven location definition (data/locations/*.yaml). Mirrors
--   the building/unit/item def pattern: the YAML shape is parsed in
--   'Engine.Asset.YamlLocations' and converted into this runtime type.
--
--   Field prefix is `ld` (location def).
data LocationDef = LocationDef
    { ldId       ∷ !Text              -- ^ unique string key
    , ldLabel    ∷ !Text              -- ^ display name
    , ldType     ∷ !Text              -- ^ tag: "ruin" / "camp" / "natural" / …
    , ldBuilder  ∷ !Text              -- ^ name of the Lua builder function
    , ldAnchor   ∷ ![LocationAnchor]  -- ^ terrain placement constraint tags
                                      --   (e.g. [AnchorMountain, AnchorFlat]);
                                      --   the generator (#89) filters on
                                      --   these. The CLOSED #801\/#1681
                                      --   vocabulary, so a def cannot carry
                                      --   a tag placement has no semantics
                                      --   for — from YAML or from Haskell.
                                      --   Not serialized: defs reload from
                                      --   YAML each boot and saves reference
                                      --   them by id only.
    , ldMaxCount ∷ !Int               -- ^ max instances the world-gen
                                      --   overlay (#89) places (per type).
    , ldMinSpacing ∷ !Int             -- ^ min chunk separation between two
                                      --   instances of this type (jittered
                                      --   distribution; #89).
    , ldContents ∷ ![LocationContent] -- ^ content to spawn (raw ids; #90)
    , ldBounds   ∷ !RelBounds         -- ^ authoritative footprint: an
                                      --   inclusive tile box relative to
                                      --   the anchor (#777). Drives the
                                      --   ruin geometry + content scatter
                                      --   in scripts/locations.lua — no
                                      --   independent Lua-side radius.
    , ldMapIcon  ∷ !(Maybe Text)      -- ^ the zoom-map annotation texture
                                      --   path for this location's TYPE
                                      --   (#781, singular since #1230).
                                      --   'Nothing' = this location places
                                      --   no map annotation at all. While
                                      --   the type is still unknown every
                                      --   annotated location instead draws
                                      --   the ONE shared
                                      --   'locationUnknownIconPath', which
                                      --   no definition declares.
    , ldNaming   ∷ !LocationNaming    -- ^ authored concept pools every
                                      --   generated instance name draws
                                      --   from (#1101). Required on
                                      --   every definition.
    } deriving (Show, Eq, Generic)

-- | Engine-wide registry of location defs loaded from data/locations/.
--   Stored as a list; 'allLocations' returns it sorted by id so the
--   readback ('locations.listDefs' / the debug overlay list) is
--   deterministic across runs and machines — file load order is
--   OS-dependent (engine.listFiles → listDirectory) and must not leak
--   into the visible ordering.
newtype LocationRegistry = LocationRegistry
    { lrDefs ∷ [LocationDef]
    } deriving (Show, Eq)

emptyLocationRegistry ∷ LocationRegistry
emptyLocationRegistry = LocationRegistry []

-- | Register (or replace) a def by id. A re-registered id keeps its
--   original position so reload doesn't reorder the list.
registerLocation ∷ LocationDef → LocationRegistry → LocationRegistry
registerLocation def (LocationRegistry defs)
    | any ((≡ ldId def) . ldId) defs =
        LocationRegistry (map (\d → if ldId d ≡ ldId def then def else d) defs)
    | otherwise = LocationRegistry (defs <> [def])

lookupLocation ∷ Text → LocationRegistry → Maybe LocationDef
lookupLocation lid (LocationRegistry defs) =
    case filter ((≡ lid) . ldId) defs of
        (d:_) → Just d
        []    → Nothing

-- | All defs, sorted by id (deterministic; see 'LocationRegistry').
allLocations ∷ LocationRegistry → [LocationDef]
allLocations = sortOn ldId . lrDefs

-- | Deterministic 'Engine.Asset.TextureNameRegistry.TextureNameRegistry' key
--   for one location definition's TYPE icon (#781; singular since
--   #1230) — shared between the YAML loader
--   ('Engine.Scripting.Lua.API.Locations', which registers under this
--   name) and the zoom-map renderer ('World.Render.Zoom.Icons', which
--   looks it up under the same name), mirroring the
--   @mat_tile_\<name\>@-style convention
--   'Engine.Asset.TextureNameRegistry' already documents.
--   The @loc_type_icon_@ prefix keeps this namespace DISJOINT from
--   'locationUnknownIconTextureName' by construction, which matters
--   because a definition id is unrestricted authored text: under a
--   shared @loc_icon_@ prefix, a definition with @id: unknown@
--   declaring a @map_icon@ would register under the shared marker's own
--   key and overwrite it — and every location's unknown state would
--   then draw that definition's type icon, leaking exactly the thing
--   the marker exists to hide. No id can produce the shared key here,
--   since every key this returns begins with @loc_type_icon_@ and the
--   shared key does not.
locationIconTextureName ∷ Text → Text
locationIconTextureName lid = "loc_type_icon_" <> lid

-- | Registry key for the ONE shared unknown-location icon (#1230). Not
--   derived from any definition id — every annotated location draws this
--   same marker while its type is unknown, which is the whole point:
--   the zoom map must not leak WHAT a location is before a unit has
--   seen it. Deliberately NOT of the form 'locationIconTextureName'
--   produces; see that function's note on why the two namespaces must
--   stay disjoint.
locationUnknownIconTextureName ∷ Text
locationUnknownIconTextureName = "loc_unknown_icon"

-- | The canonical on-disk path of that shared icon (#1230). Declared in
--   code rather than in any definition's YAML, and loaded once
--   independently of the definitions, for the same reason its registry
--   key is: it belongs to no location type.
locationUnknownIconPath ∷ FilePath
locationUnknownIconPath = "assets/textures/icons/location/location_unknown.png"
