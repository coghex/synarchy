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
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
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
    } deriving (Show, Eq, Generic)

instance FromJSON LocationYamlDef where
    parseJSON = withObject "LocationYamlDef" $ \v → do
        lid      ← v .: "id"
        bounds   ← v .: "bounds"
        margin   ← v .: "discovery_margin"
        contents ← v .:? "contents" .!= []
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
        LocationYamlDef lid
            ⊚ v .:? "label"       .!= ""
            ⊛ v .:? "type"        .!= "natural"
            ⊛ v .:  "builder"
            ⊛ v .:? "anchor"      .!= []
            ⊛ v .:? "max_count"   .!= 4
            ⊛ v .:? "min_spacing" .!= 4
            ⊛ pure contents
            ⊛ pure bounds
            ⊛ pure margin

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
