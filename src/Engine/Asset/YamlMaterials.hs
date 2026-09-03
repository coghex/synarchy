{-# LANGUAGE Strict, DeriveGeneric #-}
-- | Material definitions loaded from @data/materials/*.yaml@, and the
--   fold of those definitions into a 'World.Material.MaterialRegistry'.
module Engine.Asset.YamlMaterials
    ( -- * YAML types
      MaterialDef(..)
    , MaterialFile(..)
      -- * The @move_cost@ domain (#1734)
    , defaultMoveCost
    , validMoveCost
    , normalizeMaterialDef
      -- * Loading
    , loadMaterialYaml
    , loadMaterialYamlOutcome
    , loadMaterialDirectory
    , materialPropsFromDef
    , loadPopulatedMaterialRegistry
    ) where

import UPrelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Engine.Asset.YamlList (loadYamlListOutcome)
import Engine.Core.Log (LoggerState, logInfo, logWarn, LogCategory(..))
import World.Material
    (MaterialRegistry, MaterialProps(..), registerMaterial, emptyMaterialRegistry)

data MaterialDef = MaterialDef
    { mdId       ∷ Word8
    , mdName     ∷ Text
    , mdHardness ∷ Float
    , mdDensity  ∷ Float
    , mdAlbedo   ∷ Float
    , mdDrainage ∷ Float
      -- ^ Hydraulic drainage 0.0–1.0. See @World.Material.MaterialProps@
      --   for semantics. Defaults to 0.4 (neutral) when omitted in YAML.
    , mdPickSpeed   ∷ Float
      -- ^ Dig-rate multiplier with a pick (1.0 = baseline). See
      --   @World.Material.MaterialProps@.
    , mdShovelSpeed ∷ Float
      -- ^ Dig-rate multiplier with a shovel.
    , mdDigSpoil ∷ Maybe Text
      -- ^ Spoil material name produced when digging this material
      --   (e.g. granite → "heavy_gravel"). Absent = no spoil.
    , mdDigBulking ∷ Float
      -- ^ Spoil volume per excavated volume (default 1.0; broken
      --   hard rock bulks above 1).
    , mdDigChunk ∷ Maybe Text
      -- ^ Item def spawned by the chunk-yield accumulator
      --   (granite → "granite_chunk"). Absent = no chunk yields.
    , mdDigGems ∷ Bool
      -- ^ Gem region field applies while digging this material
      --   (default False).
    , mdMoveCost ∷ Float
      -- ^ Surface-traversal cost multiplier for unit pathing (default
      --   'defaultMoveCost'). See @World.Material.MaterialProps@
      --   (mpMoveCost) / #312. Constrained to 'validMoveCost'’s finite
      --   positive domain by 'normalizeMaterialDef', which every value
      --   reaching a registry passes through (#1734) — the raw decoded
      --   field here is the only place an out-of-domain value exists.
    , mdTile     ∷ Text
    , mdZoom     ∷ Text
    , mdBg       ∷ Text
    } deriving (Show, Eq, Generic)

instance FromJSON MaterialDef where
    parseJSON = withObject "MaterialDef" $ \v → MaterialDef
        ⊚ v .: "id"
        ⊛ v .: "name"
        ⊛ v .:? "hardness" .!= 0.5
        ⊛ v .:? "density"  .!= 2.5
        ⊛ v .:? "albedo"   .!= 0.5
        ⊛ v .:? "drainage" .!= 0.4
        ⊛ v .:? "pick_speed"   .!= 0.5
        ⊛ v .:? "shovel_speed" .!= 0.5
        ⊛ v .:? "dig_spoil"
        ⊛ v .:? "dig_bulking"  .!= 1.0
        ⊛ v .:? "dig_chunk"
        ⊛ v .:? "dig_gems"     .!= False
        ⊛ v .:? "move_cost"    .!= defaultMoveCost
        ⊛ v .: "tile"
        ⊛ v .: "zoom"
        ⊛ v .: "bg"

data MaterialFile = MaterialFile
    { mfMaterials ∷ [MaterialDef]
    } deriving (Show, Eq, Generic)

instance FromJSON MaterialFile where
    parseJSON = withObject "MaterialFile" $ \v → MaterialFile
        ⊚ v .: "materials"

-- * The @move_cost@ domain (#1734)

-- | The value an omitted @move_cost@ takes, and the value an authored
--   one outside 'validMoveCost'’s domain is replaced with: firm ground,
--   a 1.0 no-op on both route cost and traversal speed.
defaultMoveCost ∷ Float
defaultMoveCost = 1.0

-- | Is an authored @move_cost@ inside its documented domain — a FINITE,
--   strictly positive multiplier?
--
--   Both halves are load-bearing, because the planner and the mover read
--   the same field through different arithmetic
--   (@Unit.Pathing.Cost.materialFactor@ scales an A* edge weight;
--   @Unit.Thread.Movement.PathAdvance@ DIVIDES the per-tick step length
--   by it):
--
--     * Non-finite. @Data.Yaml@ decodes an oversized scalar such as
--       @move_cost: 1e999@ straight to @+Infinity@ for a 'Float' field.
--       The planner survives it — @clampStepCost@ folds a non-finite
--       edge weight to its finite ceiling, so the tile stays routable —
--       but the mover's uncapped @speed / Infinity@ is exactly @0@, and
--       nothing in @PathAdvance@ detects no progress, so a unit that
--       steps onto such ground never advances again.
--     * Zero or negative. @materialFactor@'s @max 0.1@ floor exists only
--       to stop a divide-by-zero, so @0@, a finite negative, and
--       @-Infinity@ all collapse to @0.1@ — ten times FASTER than bare
--       rock, and cheap enough that A* actively prefers that ground.
--       That is an incidental value, not a chosen default.
--
--   This mirrors 'Unit.Pathing.Config.finiteOr' (#815), which applies
--   the same policy at the sibling pathing-config boundary and for the
--   same reason: bare @max@\/@min@ bounds do not handle non-finite
--   inputs.
validMoveCost ∷ Float → Bool
validMoveCost x = not (isNaN x) ∧ not (isInfinite x) ∧ x > 0

-- | Bring one decoded definition inside the 'validMoveCost' domain,
--   substituting 'defaultMoveCost' for an out-of-domain @move_cost@ and
--   naming the file, the material id and name, the field, and the
--   offending value in a @CatAsset@ WARNING — invalid authoring is
--   surfaced, not silent (#1734).
--
--   Substitution rather than refusal is deliberate: it is per-FIELD, so
--   a valid sibling material in the same file — and every other field of
--   the offending material — keeps loading unchanged, which refusal
--   could not offer here ('Engine.Asset.YamlList.loadYamlList' turns any
--   decode failure into an empty result for the WHOLE file).
normalizeMaterialDef ∷ LoggerState → FilePath → MaterialDef → IO MaterialDef
normalizeMaterialDef logger path def
    | validMoveCost (mdMoveCost def) = pure def
    | otherwise = do
        logWarn logger CatAsset $ "Invalid move_cost in material YAML "
            <> T.pack path <> ": material id " <> tshow (mdId def)
            <> " (" <> mdName def <> ") field move_cost = "
            <> tshow (mdMoveCost def)
            <> " is outside the finite positive domain; using the default "
            <> tshow defaultMoveCost
        pure def { mdMoveCost = defaultMoveCost }

-- * YAML parsing

-- | Decode one material YAML file and bring every definition it yields
--   inside the documented field domains ('normalizeMaterialDef').
--
--   Normalizing HERE, rather than at either registration site, is what
--   makes the domain unbypassable (#1734 requirement 3): this is the one
--   decode boundary BOTH paths to a 'MaterialRegistry' go through —
--   'loadPopulatedMaterialRegistry' via 'loadMaterialDirectory', and
--   @engine.loadMaterialYaml@ ('Engine.Scripting.Lua.API.YamlTextures')
--   directly.
loadMaterialYaml ∷ LoggerState → FilePath → IO [MaterialDef]
loadMaterialYaml logger path =
    fromMaybe [] ⊚ loadMaterialYamlOutcome logger path

-- | 'loadMaterialYaml' with the decode OUTCOME kept (#2203):
--   'Nothing' is a parse failure, @Just xs@ a file that decoded
--   (possibly to an empty list). Normalization runs on the decoded
--   list exactly as it always has; a parse failure has nothing to
--   normalize. The startup loader needs the two apart; every other
--   caller reads 'loadMaterialYaml'.
loadMaterialYamlOutcome ∷ LoggerState → FilePath → IO (Maybe [MaterialDef])
loadMaterialYamlOutcome logger path =
    loadYamlListOutcome logger "material" "materials" mfMaterials path
        ⌦ traverse (mapM (normalizeMaterialDef logger path))

-- | Load and concatenate all @.yaml@\/@.yml@ files in a directory (non-recursive)
loadMaterialDirectory ∷ LoggerState → FilePath → IO [MaterialDef]
loadMaterialDirectory logger dir = do
    entries ← listDirectory dir
    let yamlFiles = filter isYaml entries
    logInfo logger CatAsset $ "Loading materials from "
        <> T.pack dir <> " ("
        <> tshow (length yamlFiles) <> " files)"
    mats ← concat ⊚ mapM (\f → loadMaterialYaml logger (dir </> f)) yamlFiles
    logInfo logger CatAsset $ "Total materials loaded: "
        <> tshow (length mats)
    return mats
  where
    isYaml f = takeExtension f ∈ [".yaml", ".yml"]

-- | The one 'MaterialDef' → 'MaterialProps' conversion, shared by both
--   registration paths so a material cannot enter a registry through one
--   of them carrying fields the other would have treated differently
--   (#1734 requirement 3). Field values are taken verbatim from the
--   definition; the @move_cost@ domain is established upstream, by the
--   'normalizeMaterialDef' pass every decoded definition goes through in
--   'loadMaterialYaml'.
materialPropsFromDef ∷ MaterialDef → MaterialProps
materialPropsFromDef def =
    MaterialProps (mdName def)
                  (mdHardness def)
                  (mdDensity def)
                  (mdAlbedo def)
                  (mdDrainage def)
                  (mdPickSpeed def)
                  (mdShovelSpeed def)
                  (mdDigSpoil def)
                  (mdDigBulking def)
                  (mdDigChunk def)
                  (mdDigGems def)
                  (mdMoveCost def)

-- | 'loadMaterialDirectory' plus the fold into a real
--   'World.Material.MaterialRegistry' — the exact population pass
--   'World.Thread.Command.Init' runs at "Step 0.5" of every
--   @world.init@, extracted here (issue #763) so
--   'Engine.Scripting.Lua.API.Save' can call the SAME logic on a
--   whole-session LOAD: the registry is otherwise populated only by
--   @world.init@, so a fresh headless boot that goes straight to
--   @engine.loadSave@ with no prior @world.init@ in the SAME process
--   would see an entirely empty registry (every id but air reporting
--   as "unknown") when validating a save's material references.
--   Idempotent, like the population pass it mirrors — safe to call
--   even when a live world has already populated the registry.
loadPopulatedMaterialRegistry ∷ LoggerState → FilePath → IO MaterialRegistry
loadPopulatedMaterialRegistry logger dir = do
    matDefs ← loadMaterialDirectory logger dir
    pure $ foldl' (\r def → registerMaterial (mdId def)
                                (materialPropsFromDef def) r)
                  emptyMaterialRegistry matDefs
