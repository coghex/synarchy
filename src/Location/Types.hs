{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric #-}
module Location.Types
    ( LocationContent(..)
    , LocationDef(..)
    , LocationRegistry(..)
    , emptyLocationRegistry
    , registerLocation
    , lookupLocation
    , allLocations
    ) where

import UPrelude
import GHC.Generics (Generic)
import Data.List (sortOn)

-- | One piece of content a location places when it is stamped — a
--   building, unit, or ground item, addressed by its raw id string.
--   The ids are NOT validated or resolved here: the world-gen overlay
--   (#89) and the content-spawning pass (#90) resolve them at spawn
--   time. `kind` is a free string tag ("building" / "unit" / "item")
--   so new content kinds don't force a schema change.
data LocationContent = LocationContent
    { lconKind  ∷ !Text  -- ^ "building" | "unit" | "item"
    , lconId    ∷ !Text  -- ^ raw id string, resolved at spawn time
    , lconCount ∷ !Int   -- ^ how many to place (defaults to 1)
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
