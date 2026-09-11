{-# LANGUAGE Strict #-}

-- | The stone a lava-water reaction leaves behind, as a MATERIAL (#2485,
--   FR-2 of epic #2480).
--
--   'Sim.Fluid.Reaction.solidProductFor' decides WHICH stone at the
--   moment of annihilation, from the contact's own reaction-time
--   reading; this module is the other half of that decision — turning
--   the chosen 'SolidProduct' into the 'World.Material.Id.MaterialId'
--   the terrain edit actually carries.
--
--   It is deliberately separate from the commit that uses it. The commit
--   runs on the world worker against a live page; resolving the name
--   here is a pure function of the registry, so the "a material the
--   registry does not know fails LOUDLY rather than silently omitting
--   the stone" rule is directly exercisable, and the commit has one
--   fallible step instead of one per event.
module World.Reaction.Stone
    ( stoneMaterialName
    , stoneMaterialFor
    ) where

import UPrelude
import Sim.Fluid.Reaction (SolidProduct(..))
import World.Material (MaterialRegistry, MaterialId, materialIdByName)

-- | The authored YAML material name for each product. Both live in
--   @data\/materials\/igneous_extrusive.yaml@ and both already have tile
--   and zoom art, so this slice adds no material and no asset.
stoneMaterialName ∷ SolidProduct → Text
stoneMaterialName SolidBasalt   = "basalt"
stoneMaterialName SolidObsidian = "obsidian"

-- | Resolve a product through the material REGISTRY, never a hardcoded
--   id: the ids are assigned at YAML load and a literal here would
--   silently name a different stone the moment the file is reordered.
--
--   A name the registry does not know is an error, not an absent stone.
--   Omitting the tile would leave the sim's consumed lava with no
--   product — volume destroyed and nothing to show for it — so the
--   caller reports this and commits NOTHING rather than committing the
--   fluid half alone.
stoneMaterialFor ∷ MaterialRegistry → SolidProduct → Either Text MaterialId
stoneMaterialFor registry product =
    case materialIdByName registry name of
        Just mid → Right mid
        Nothing  → Left $
            "solidification product material '" <> name
            <> "' is not in the material registry"
  where name = stoneMaterialName product
