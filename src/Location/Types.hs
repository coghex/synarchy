{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric #-}
module Location.Types
    ( LocationContent(..)
    , LocationDef(..)
    , LocationRegistry(..)
    , emptyLocationRegistry
    , registerLocation
    , lookupLocation
    , allLocations
    , locationIconTextureName
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.List (sortOn)
import Location.Bounds (RelBounds(..))

-- | One piece of content a location places when it is stamped — a
--   building, unit, ground item, loot-table roll, or nested structure,
--   addressed by its raw id string. The ids are NOT validated or
--   resolved here: the content-spawning pass (#90) resolves them at
--   spawn time. `kind` is a free string tag ("building" / "unit" /
--   "item" / "loot_table" / "structure") so new content kinds don't
--   force a schema change.
data LocationContent = LocationContent
    { lconKind     ∷ !Text            -- ^ "building" | "unit" | "item"
                                      --   | "loot_table" | "structure"
    , lconId       ∷ !Text            -- ^ raw id string, resolved at spawn time
    , lconCount    ∷ !Int             -- ^ how many to place (defaults to 1;
                                      --   ignored by "loot_table", which
                                      --   uses 'lconRolls' instead)
    , lconPosition ∷ !(Maybe (Int, Int))
                                      -- ^ fixed (x, y) offset from the
                                      --   location anchor; 'Nothing' scatters
                                      --   randomly within the structure
                                      --   footprint instead
    , lconFaction  ∷ !(Maybe Text)    -- ^ "unit" only: spawn-time faction tag
                                      --   (defaults to "hostile" when omitted)
    , lconRolls    ∷ !Int             -- ^ "loot_table" only: how many times
                                      --   to roll the table (defaults to 1)
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
    , ldAnchor   ∷ ![Text]            -- ^ terrain placement constraint tags
                                      --   (e.g. ["mountain","flat"]); the
                                      --   generator (#89) filters on these.
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
    , ldDiscoveryMargin ∷ !Int        -- ^ non-negative tile halo around
                                      --   'ldBounds' later discovery-
                                      --   trigger work (#780) expands by.
    , ldMapIcons ∷ !(Maybe (Text, Text))
                                      -- ^ zoom-map annotation texture
                                      --   paths, (undiscovered, discovered)
                                      --   (#781). 'Nothing' = this location
                                      --   places no map annotation at all.
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

-- | Deterministic 'Engine.Asset.YamlTextures.TextureNameRegistry' key
--   for one state of a location's map-icon pair (#781) — shared between
--   the YAML loader ('Engine.Scripting.Lua.API.Locations', which
--   registers under this name) and the zoom-map renderer
--   ('World.Render.Zoom.Icons', which looks it up under the same name),
--   mirroring the @mat_tile_\<name\>@-style convention
--   'Engine.Asset.YamlTextures' already documents.
locationIconTextureName ∷ Text → Bool → Text
locationIconTextureName lid isDiscovered =
    "loc_icon_" <> lid <> "_"
        <> (if isDiscovered then "discovered" else "undiscovered")
