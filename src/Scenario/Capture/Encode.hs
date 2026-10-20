{-# LANGUAGE Strict #-}
-- | Writing a whole scenario as the document the decoder reads (#2714,
--   SCN-14 of epic #2698): the terrain, fluid and flora vocabulary run
--   backwards (the structure, building, location, unit and item entries are
--   their own owners' encoders), the document's top level, and the text.
--
--   A decode of what this writes is the scenario that was written: that is
--   what lets the capture service verify its candidate bytes by decoding
--   them and comparing, so an entry the document could not carry is found
--   before anything is published rather than after.
--
--   __Order is stable and meaningful.__ The top-level families come in the
--   decoder's order and an entry's own fields in the order a person writes
--   them (@id@, @tags@, the definition, the position, then the physical
--   values); every other key, which is a name the content chose (a stat, a
--   slot), is alphabetical. Lists keep the order the capture produced, which
--   is structural (never an allocation number or a hash-map order), and
--   nested item and content order is meaningful and kept.
module Scenario.Capture.Encode
    ( encodeScenario
    , scenarioValue
    , terrainEntryValue
    , fluidEntryValue
    , floraEntryValue
    ) where

import UPrelude
import Data.List (elemIndex)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.Scientific as Sci
import qualified Data.Yaml.Pretty as YP
import Scenario.Items.Encode (itemEntryFields, floatValue)
import Scenario.Placement.Encode
import Scenario.Types
import Scenario.Units.Encode (unitEntryValue)

-- | The document, as text. The version is the scenario's own (the current
--   format's, for a capture).
encodeScenario ∷ Scenario → BS.ByteString
encodeScenario = YP.encodePretty (YP.setConfCompare keyOrder YP.defConfig) ∘ scenarioValue

scenarioValue ∷ Scenario → A.Value
scenarioValue sc = A.object $ concat
    [ [ ("version", A.toJSON (scSourceVersion sc)) ]
    , [ ("map", A.object [ ("width", A.toJSON (mdWidth m)), ("height", A.toJSON (mdHeight m)) ])
      | Just m ← [scMap sc] ]
    , [ ("camera", A.object [ ("x", floatValue (fst (scCamera sc)))
                            , ("y", floatValue (snd (scCamera sc))) ]) ]
    , [ ("terrain", A.toJSON (map terrainEntryValue (scTerrain sc)))
      , ("fluids", A.toJSON (map fluidEntryValue (scFluids sc)))
      , ("flora", A.toJSON (map floraEntryValue (scFlora sc)))
      , ("structures", A.toJSON (map structureEntryValue (scStructures sc)))
      , ("buildings", A.toJSON (map buildingEntryValue (scBuildings sc)))
      , ("locations", A.toJSON (map locationEntryValue (scLocations sc)))
      , ("units", A.toJSON (map unitEntryValue (scUnits sc)))
      , ("ground_items", A.toJSON [ A.object (("x", floatValue (giX g)) : ("y", floatValue (giY g))
                                              : itemEntryFields (giItem g))
                                  | g ← scGroundItems sc ]) ] ]

regionPairs ∷ TileRegion → [(A.Key, A.Value)]
regionPairs = \case
    RegionRect x0 y0 x1 y1 →
        [ ("rect", A.object [ ("x0", A.toJSON x0), ("y0", A.toJSON y0)
                            , ("x1", A.toJSON x1), ("y1", A.toJSON y1) ]) ]
    RegionTiles ts → [ ("tiles", A.toJSON [ [x, y] | (x, y) ← ts ]) ]

terrainEntryValue ∷ TerrainPatch → A.Value
terrainEntryValue p = A.object $ concat
    [ idField (tpId p), tagsField (tpTags p)
    , regionPairs (tpRegion p)
    , [ ("material", A.toJSON (tpMaterial p)), ("surface_z", A.toJSON (tpSurfaceZ p)) ]
    , opt "slope" (tpSlope p) A.toJSON ]

fluidEntryValue ∷ FluidPatch → A.Value
fluidEntryValue p = A.object $ concat
    [ idField (fpId p), tagsField (fpTags p)
    , regionPairs (fpRegion p)
    , [ ("fluid", A.toJSON (fluidKindName (fpKind p)))
      , ("surface_z", eighthsValue (fpSurfaceEighths p)) ] ]

-- | An exact fluid plane as the decimal it is: @e@ eighths is @e · 125 / 1000@,
--   so the number is written exactly and read back to the same plane, never
--   through a float.
eighthsValue ∷ Int → A.Value
eighthsValue e = A.Number (Sci.scientific (toInteger e * 125) (-3))

floraEntryValue ∷ FloraEntry → A.Value
floraEntryValue f = A.object $ concat
    [ idField (feId f), tagsField (feTags f)
    , [ ("species", A.toJSON (feSpecies f)), ("x", A.toJSON (feX f)), ("y", A.toJSON (feY f)) ]
    , opt "z" (feZ f) A.toJSON
    , opt "age" (feAge f) floatValue
    , opt "health" (feHealth f) floatValue
    , opt "offset_u" (feOffsetU f) floatValue
    , opt "offset_v" (feOffsetV f) floatValue
    , opt "variant" (feVariant f) A.toJSON
    , opt "regrowth" (feRegrowth f) floatValue ]

-- | The order of the keys a person writes first; every other key follows,
--   alphabetically.
keyOrder ∷ Text → Text → Ordering
keyOrder a b = compare (rank a, a) (rank b, b)
  where
    rank k = maybe (length order) id (elemIndex k order)
    order =
        -- the document
        [ "version", "map", "camera", "terrain", "fluids", "flora", "structures"
        , "buildings", "locations", "units", "ground_items"
        -- an entry's identity, then what it is, then where, then its values
        , "id", "tags", "definition", "pack", "piece", "species", "material", "fluid"
        , "width", "height", "rect", "tiles", "x0", "y0", "x1", "y1", "x", "y", "z"
        , "surface_z", "slope" ]
