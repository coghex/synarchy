{-# LANGUAGE Strict, DeriveGeneric #-}
module Engine.Asset.YamlLocations
    ( LocationYamlPosition(..)
    , LocationYamlContent(..)
    , LocationYamlBounds(..)
    , authoredLocationCoordinateLimit
    , LocationYamlNaming(..)
    , LocationYamlDef(..)
    , LocationYamlFile(..)
    , loadLocationYaml
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject, Value(..), Object)
import Data.Aeson.Types (parseEither, Parser)
import qualified Data.Aeson.Key as Key
import Engine.Core.Log (LoggerState)
import Engine.Asset.YamlList (loadYamlList)

-- | A fixed relative tile offset from a location's anchor (#90).
data LocationYamlPosition = LocationYamlPosition
    { lypX ∷ !Int
    , lypY ∷ !Int
    } deriving (Show, Eq, Generic)

instance FromJSON LocationYamlPosition where
    parseJSON = withObject "LocationYamlPosition" $ \v → LocationYamlPosition
        ⊚ v .:? "x" .!= 0
        ⊛ v .:? "y" .!= 0

-- | One `{kind, id, count, position, faction, rolls}` content entry.
--   `count` defaults to 1; `position`/`faction`/`rolls` are all
--   optional (#90) — see 'Location.Types.LocationContent'.
data LocationYamlContent = LocationYamlContent
    { lycKind     ∷ !Text
    , lycId       ∷ !Text
    , lycCount    ∷ !Int
    , lycPosition ∷ !(Maybe LocationYamlPosition)
    , lycFaction  ∷ !(Maybe Text)
    , lycRolls    ∷ !Int
    } deriving (Show, Eq, Generic)

instance FromJSON LocationYamlContent where
    parseJSON = withObject "LocationYamlContent" $ \v → LocationYamlContent
        ⊚ v .:  "kind"
        ⊛ v .:  "id"
        ⊛ v .:? "count"    .!= 1
        ⊛ v .:? "position"
        ⊛ v .:? "faction"
        ⊛ v .:? "rolls"    .!= 1

-- | The authoritative spatial contract (#777): an inclusive,
--   axis-aligned tile box relative to the location's anchor. Required
--   on every definition — see 'LocationYamlDef''s 'FromJSON' instance
--   for the inverted-bounds rejection (#777\/#1151) and the
--   authored-coordinate range rejection (#1796).
--
--   This instance itself accepts any four 'Int's on purpose: the
--   range rule is stated in the DEF parser, which is the only scope
--   where the location id is available to attribute a rejection to.
data LocationYamlBounds = LocationYamlBounds
    { lybMinX ∷ !Int
    , lybMinY ∷ !Int
    , lybMaxX ∷ !Int
    , lybMaxY ∷ !Int
    } deriving (Show, Eq, Generic)

instance FromJSON LocationYamlBounds where
    parseJSON = withObject "LocationYamlBounds" $ \v → LocationYamlBounds
        ⊚ v .: "min_x"
        ⊛ v .: "min_y"
        ⊛ v .: "max_x"
        ⊛ v .: "max_y"

-- | The inclusive domain an authored location bounds coordinate may
--   occupy (#1796): @[-2147483647, 2147483647]@, i.e. @±(2^31 - 1)@.
--   Enforced on all four of @bounds.min_x@, @bounds.min_y@,
--   @bounds.max_x@ and @bounds.max_y@ by 'LocationYamlDef''s 'FromJSON'
--   instance below, which REJECTS an out-of-domain value rather than
--   clamping or saturating it.
--
--   The limit is vastly larger than anything the generator produces.
--   'Location.Overlay.allCoords' ranges a placed chunk over
--   @[-worldSize\/2 .. worldSize\/2 - 1]@ and the anchor tile is that
--   chunk's centre, so the largest advertised world (1,024 chunks;
--   @scripts\/create_world\/settings_tab.lua@) reaches only about
--   ±8,184 tiles, and a 512-chunk world about ±4,088. An authored box
--   is allowed five more decimal orders of magnitude than that.
--
--   It is an authored-data SANITY boundary and nothing more. It is NOT
--   the proof that translating an authored box onto an arbitrary
--   'World.Chunk.Types.ChunkCoord' cannot overflow: chunk coordinates
--   are unrestricted 'Int's, no world-size normalization caps them, and
--   @ChunkCoord (2^59 - 1) 0@ alone already anchors at @maxBound - 7@.
--   That proof is 'Location.Instance.locationInstanceGeometry', which
--   computes every anchor and translated bound in 'Integer' and refuses
--   the placement before any instance exists if a component is not
--   representable. The two boundaries are complementary: this one keeps
--   authored data sane and attributable to a field; the checked
--   construction keeps the ENGINE from ever publishing a wrapped box.
authoredLocationCoordinateLimit ∷ Int
authoredLocationCoordinateLimit = 2147483647

-- | The authored naming scheme (#1101): the two concept-id pools a
--   definition's generated instance names draw on. Both keys are
--   required and both lists must be nonempty — see 'LocationYamlDef''s
--   'FromJSON' instance. The ids themselves are validated against the
--   concept catalogue by the API loader
--   ('Location.Naming.locationNamingErrors'), which is where the
--   catalogue is available; this module keeps its existing
--   zero-local-dependency shape.
data LocationYamlNaming = LocationYamlNaming
    { lynHeads     ∷ ![Text]
    , lynModifiers ∷ ![Text]
    } deriving (Show, Eq, Generic)

instance FromJSON LocationYamlNaming where
    parseJSON = withObject "LocationYamlNaming" $ \v → LocationYamlNaming
        ⊚ v .: "heads"
        ⊛ v .: "modifiers"

-- | True iff a fixed content offset falls inside a bounds box —
--   duplicated from 'Location.Bounds.rawContainsPoint' rather than
--   imported, so this module keeps its existing zero-local-dependency
--   shape (mirrors 'Engine.Asset.YamlItems' and its siblings).
relBoundsContains ∷ LocationYamlBounds → Int → Int → Bool
relBoundsContains b x y =
    x ≥ lybMinX b ∧ x ≤ lybMaxX b ∧ y ≥ lybMinY b ∧ y ≤ lybMaxY b

-- | The authoritative content-kind vocabulary (#1708): the four kinds
--   'scripts/locations.lua' can actually spawn. Closed here, at the
--   same entry point that already validates bounds, fixed content
--   positions, and anchor tags below, so an unrecognized kind fails
--   the whole file's load rather than reaching a stamp-time warning
--   after the definition is already registered.
--
--   @structure@ — a nested content entry naming another Lua builder —
--   is deliberately absent. A nested entry has no definition of its
--   own, so the builder received the OUTER def and translated its
--   whole 'lydBounds' box a second time around the shifted anchor:
--   every nonzero offset stamped geometry outside the box #777 made
--   authoritative for placement separation, discovery, portal
--   blocking, and map annotation. On tracked repository evidence no
--   definition ever authored it — no shipped data file, test, probe, or
--   commit in history does. Externally authored YAML is outside what
--   this tree can establish either way, so treat the removal as a
--   schema break, not a no-op.
--   Reintroducing nested content needs its own relative-bounds model,
--   not a re-listing here.
validContentKinds ∷ [Text]
validContentKinds = [ "unit", "item", "loot_table", "building" ]

-- | The authoritative anchor-tag vocabulary (#801): terrain/height
--   (flat/mountain/highland/lowland), ocean-distance
--   (coast/coastal/inland), and the #414 water-proximity opt-out
--   modifier (waterside — tolerates nearby water, no terrain
--   constraint of its own; see 'Location.Overlay.anchorOk'). Every tag
--   outside this set — a typo or an unimplemented climate/biome name —
--   is rejected below rather than silently matching every chunk.
--   Duplicated (not imported) in 'Location.Overlay.anchorOk' for the
--   same zero-local-dependency reason as 'relBoundsContains' above.
validAnchorTags ∷ [Text]
validAnchorTags =
    [ "flat", "mountain", "highland", "lowland"
    , "coast", "coastal", "inland"
    , "waterside"
    ]

-- | The YAML shape of a location definition. Converted to
--   'Location.Types.LocationDef' by the API loader.
data LocationYamlDef = LocationYamlDef
    { lydId         ∷ !Text
    , lydLabel      ∷ !Text
    , lydType       ∷ !Text
    , lydBuilder    ∷ !Text
    , lydAnchor     ∷ ![Text]
    , lydMaxCount   ∷ !Int   -- ^ max placements (#89); default 4
    , lydMinSpacing ∷ !Int   -- ^ min chunk separation (#89); default 4
    , lydContents   ∷ ![LocationYamlContent]
    , lydBounds     ∷ !LocationYamlBounds
    , lydMapIcon    ∷ !(Maybe Text)
                      -- ^ the zoom-map annotation texture path for this
                      --   location's TYPE (#781; singular @map_icon@
                      --   since #1230). 'Nothing' = no annotation. The
                      --   shared unknown marker is NOT declared here —
                      --   it belongs to no location type
                      --   ('Location.Types.locationUnknownIconPath').
    , lydNaming     ∷ !LocationYamlNaming
                      -- ^ required concept pools for generated instance
                      --   names (#1101).
    } deriving (Show, Eq, Generic)

-- | Fetch a required field as a raw 'Value' first (which never fails to
--   parse — any JSON/YAML value decodes as 'Value') so an absent key or a
--   value of the wrong shape can be reported with the location id
--   attached, rather than aeson's own key-not-found / type-mismatch
--   error, which has no way to know which location it's for (#777).
requireField ∷ FromJSON a ⇒ Text → Text → Object → Parser a
requireField lid fieldName v = do
    mRaw ← v .:? Key.fromText fieldName
    case mRaw of
        Nothing → fail (T.unpack ("location '" <> lid
            <> "': missing required field '" <> fieldName <> "'"))
        Just (raw ∷ Value) → case parseEither parseJSON raw of
            Left err → fail (T.unpack ("location '" <> lid <> "': invalid '"
                <> fieldName <> "' field (" <> T.pack err <> ")"))
            Right a  → pure a

-- | Parse the optional @map_icon: \<path\>@ field (#781, singular since
--   #1230). Absent entirely → 'Nothing' (this location places no
--   zoom-map annotation). Present → it must be a string path; anything
--   else (the old @map_icons@ object, a number, a list) fails with a
--   message naming the location and the offending field — the same
--   location-id-attributed contract 'requireField' already gives
--   'bounds'.
parseMapIcon ∷ Text → Object → Parser (Maybe Text)
parseMapIcon lid v = do
    mRaw ← v .:? "map_icon"
    case mRaw of
        Nothing → pure Nothing
        Just raw → case raw of
            String path → pure (Just path)
            _ → fail (T.unpack ("location '" <> lid
                <> "': 'map_icon' must be a texture path string"))

instance FromJSON LocationYamlDef where
    parseJSON = withObject "LocationYamlDef" $ \v → do
        lid      ← v .: "id"
        bounds   ← requireField lid "bounds" v
        mapIcon  ← parseMapIcon lid v
        naming   ← requireField lid "naming" v
        contents ← v .:? "contents" .!= []
        anchor   ← v .:? "anchor" .!= []
        -- Reject inverted bounds / an out-of-bounds fixed content
        -- position HERE, at the only entry point for this def's spatial
        -- contract, so a bad YAML fails the whole file's load with a
        -- message naming the def and the offending field rather than
        -- silently substituting geometry downstream (#777).
        --
        -- #1796: RANGE first, then ordering. An out-of-domain
        -- coordinate is rejected here rather than clamped, and each of
        -- the four fields is named separately so an author is told
        -- which one to fix. The comparison is a direct two-sided test
        -- against the domain: 'abs' would be wrong, because
        -- @abs (minBound ∷ Int)@ is 'minBound' again and would sail
        -- through any magnitude check.
        forM_ [ ("bounds.min_x" ∷ Text, lybMinX bounds)
              , ("bounds.min_y", lybMinY bounds)
              , ("bounds.max_x", lybMaxX bounds)
              , ("bounds.max_y", lybMaxY bounds) ] $ \(field, value) →
            when (value < negate authoredLocationCoordinateLimit
                    ∨ value > authoredLocationCoordinateLimit) $
                fail (T.unpack ("location '" <> lid <> "': " <> field
                    <> " (" <> tshow value
                    <> ") is outside the authored coordinate domain ["
                    <> tshow (negate authoredLocationCoordinateLimit)
                    <> ", " <> tshow authoredLocationCoordinateLimit
                    <> "]"))
        -- The two comparisons below are the ONE implementation of the
        -- "min ≤ max on both axes" rule (#1151): they are per-axis only
        -- so each failure can name its own offending field, and being
        -- strict '>' they accept the degenerate single-tile box
        -- (min == max) the inclusive contract allows. Nothing
        -- downstream re-checks the shape — 'Location.Bounds.RelBounds'
        -- is only ever built from a box that has passed here.
        when (lybMinX bounds > lybMaxX bounds) $
            fail (T.unpack ("location '" <> lid <> "': bounds.min_x ("
                <> tshow (lybMinX bounds) <> ") > bounds.max_x ("
                <> tshow (lybMaxX bounds) <> ")"))
        when (lybMinY bounds > lybMaxY bounds) $
            fail (T.unpack ("location '" <> lid <> "': bounds.min_y ("
                <> tshow (lybMinY bounds) <> ") > bounds.max_y ("
                <> tshow (lybMaxY bounds) <> ")"))
        forM_ contents $ \c → do
            -- The kind is checked BEFORE the position: an entry naming
            -- a kind nothing spawns has no meaningful footprint to
            -- contain, so reporting the unsupported kind is the
            -- actionable diagnostic (#1708).
            unless (lycKind c `elem` validContentKinds) $
                fail (T.unpack ("location '" <> lid
                    <> "': unsupported content kind '" <> lycKind c
                    <> "' (expected one of: "
                    <> T.intercalate ", " validContentKinds <> ")"))
            forM_ (lycPosition c) $ \p →
                unless (relBoundsContains bounds (lypX p) (lypY p)) $
                    fail (T.unpack ("location '" <> lid <> "': content '"
                        <> lycId c <> "' fixed position ("
                        <> tshow (lypX p) <> ","
                        <> tshow (lypY p)
                        <> ") lies outside declared bounds"))
        forM_ anchor $ \tag →
            unless (tag `elem` validAnchorTags) $
                fail (T.unpack ("location '" <> lid <> "': unsupported anchor tag '"
                    <> tag <> "' (expected one of: "
                    <> T.intercalate ", " validAnchorTags <> ")"))
        -- #1101: a pool that is present but empty would leave the
        -- definition with no concept to draw, which is authored data
        -- silently meaning "fall back to the label" — the one thing the
        -- fallback must not be able to say. Rejected here, at the same
        -- entry point as the spatial contract above.
        forM_ [ ("naming.heads" ∷ Text, lynHeads naming)
              , ("naming.modifiers", lynModifiers naming) ] $ \(field, pool) →
            when (null pool) $
                fail (T.unpack ("location '" <> lid <> "': " <> field
                    <> " must not be empty"))
        LocationYamlDef lid
            ⊚ v .:? "label"       .!= ""
            ⊛ v .:? "type"        .!= "natural"
            ⊛ v .:  "builder"
            ⊛ pure anchor
            ⊛ v .:? "max_count"   .!= 4
            ⊛ v .:? "min_spacing" .!= 4
            ⊛ pure contents
            ⊛ pure bounds
            ⊛ pure mapIcon
            ⊛ pure naming

newtype LocationYamlFile = LocationYamlFile
    { lyfLocations ∷ [LocationYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON LocationYamlFile where
    parseJSON = withObject "LocationYamlFile" $ \v → LocationYamlFile
        ⊚ v .: "locations"

loadLocationYaml ∷ LoggerState → FilePath → IO [LocationYamlDef]
loadLocationYaml logger =
    loadYamlList logger "location" "location definitions" lyfLocations
