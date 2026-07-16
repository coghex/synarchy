{-# LANGUAGE Strict, UnicodeSyntax, DeriveGeneric, OverloadedStrings #-}
module Engine.Asset.YamlLocations
    ( LocationYamlPosition(..)
    , LocationYamlContent(..)
    , LocationYamlBounds(..)
    , LocationYamlDef(..)
    , LocationYamlFile(..)
    , loadLocationYaml
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject, Value(..), Object)
import Data.Aeson.Types (parseEither, Parser)
import qualified Data.Aeson.Key as Key
import Engine.Core.Log (LoggerState, logDebug, logWarn, LogCategory(..))

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
--   for the inverted-bounds rejection.
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

-- | True iff a fixed content offset falls inside a bounds box —
--   duplicated from 'Location.Bounds.rawContainsPoint' rather than
--   imported, so this module keeps its existing zero-local-dependency
--   shape (mirrors 'Engine.Asset.YamlItems' and its siblings).
relBoundsContains ∷ LocationYamlBounds → Int → Int → Bool
relBoundsContains b x y =
    x ≥ lybMinX b ∧ x ≤ lybMaxX b ∧ y ≥ lybMinY b ∧ y ≤ lybMaxY b

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
    , lydDiscoveryMargin ∷ !Int
    , lydMapIcons   ∷ !(Maybe (Text, Text))
                      -- ^ (undiscovered, discovered) zoom-map annotation
                      --   texture paths (#781); 'Nothing' = no annotation.
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

-- | Parse the optional @map_icons: { undiscovered, discovered }@ block
--   (#781). Absent entirely → 'Nothing' (this location places no zoom-map
--   annotation). Present → BOTH sub-fields are required; a missing or
--   wrong-shaped sub-field, or a @map_icons@ value that isn't an object
--   at all, fails with a message naming the location and the offending
--   field — the same location-id-attributed contract 'requireField'
--   already gives 'bounds' / 'discovery_margin'.
parseMapIcons ∷ Text → Object → Parser (Maybe (Text, Text))
parseMapIcons lid v = do
    mRaw ← v .:? "map_icons"
    case mRaw of
        Nothing → pure Nothing
        Just raw → case raw of
            Object iconsObj → do
                undisc ← requireField lid "undiscovered" iconsObj
                disc   ← requireField lid "discovered" iconsObj
                pure (Just (undisc, disc))
            _ → fail (T.unpack ("location '" <> lid
                <> "': 'map_icons' must be an object with 'undiscovered' "
                <> "and 'discovered' fields"))

instance FromJSON LocationYamlDef where
    parseJSON = withObject "LocationYamlDef" $ \v → do
        lid      ← v .: "id"
        bounds   ← requireField lid "bounds" v
        margin   ← requireField lid "discovery_margin" v
        mapIcons ← parseMapIcons lid v
        contents ← v .:? "contents" .!= []
        anchor   ← v .:? "anchor" .!= []
        -- Reject inverted bounds / a negative margin / an out-of-bounds
        -- fixed content position HERE, at the only entry point for this
        -- def's spatial contract, so a bad YAML fails the whole file's
        -- load with a message naming the def and the offending field
        -- rather than silently substituting geometry downstream (#777).
        when (lybMinX bounds > lybMaxX bounds) $
            fail (T.unpack ("location '" <> lid <> "': bounds.min_x ("
                <> T.pack (show (lybMinX bounds)) <> ") > bounds.max_x ("
                <> T.pack (show (lybMaxX bounds)) <> ")"))
        when (lybMinY bounds > lybMaxY bounds) $
            fail (T.unpack ("location '" <> lid <> "': bounds.min_y ("
                <> T.pack (show (lybMinY bounds)) <> ") > bounds.max_y ("
                <> T.pack (show (lybMaxY bounds)) <> ")"))
        when (margin < 0) $
            fail (T.unpack ("location '" <> lid
                <> "': discovery_margin must be non-negative, got "
                <> T.pack (show margin)))
        forM_ contents $ \c → forM_ (lycPosition c) $ \p →
            unless (relBoundsContains bounds (lypX p) (lypY p)) $
                fail (T.unpack ("location '" <> lid <> "': content '"
                    <> lycId c <> "' fixed position ("
                    <> T.pack (show (lypX p)) <> ","
                    <> T.pack (show (lypY p))
                    <> ") lies outside declared bounds"))
        forM_ anchor $ \tag →
            unless (tag `elem` validAnchorTags) $
                fail (T.unpack ("location '" <> lid <> "': unsupported anchor tag '"
                    <> tag <> "' (expected one of: "
                    <> T.intercalate ", " validAnchorTags <> ")"))
        LocationYamlDef lid
            ⊚ v .:? "label"       .!= ""
            ⊛ v .:? "type"        .!= "natural"
            ⊛ v .:  "builder"
            ⊛ pure anchor
            ⊛ v .:? "max_count"   .!= 4
            ⊛ v .:? "min_spacing" .!= 4
            ⊛ pure contents
            ⊛ pure bounds
            ⊛ pure margin
            ⊛ pure mapIcons

newtype LocationYamlFile = LocationYamlFile
    { lyfLocations ∷ [LocationYamlDef]
    } deriving (Show, Eq, Generic)

instance FromJSON LocationYamlFile where
    parseJSON = withObject "LocationYamlFile" $ \v → LocationYamlFile
        ⊚ v .: "locations"

loadLocationYaml ∷ LoggerState → FilePath → IO [LocationYamlDef]
loadLocationYaml logger path = do
    result ← Yaml.decodeFileEither path
    case result of
        Left err → do
            logWarn logger CatAsset $ "Failed to parse location YAML "
                <> T.pack path <> ": " <> T.pack (show err)
            return []
        Right lf → do
            logDebug logger CatAsset $ "Loaded "
                <> T.pack (show (length (lyfLocations lf)))
                <> " location definitions from " <> T.pack path
            return (lyfLocations lf)
