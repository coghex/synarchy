{-# LANGUAGE Strict #-}
-- | The ONE projection from a unit's YAML animation metadata to the
--   facts its compiled atlas index is validated against, and the ONE
--   entry point that turns that into a selection (#1259 TEX-3, shared
--   by #1260 TEX-4).
--
--   Both consumers of a unit's compiled artifacts go through here: the
--   gameplay registration path
--   ('Engine.Scripting.Lua.API.Units.Yaml.registerUnitDefs') and, since
--   #1260, the @--preview units\/\<name\>@ viewer
--   ('Engine.Preview.Unit'). D-9 makes the preview a first-class
--   acceptance surface precisely so it catches the malformed-metadata
--   and sampling regressions gameplay would — which it can only do if
--   the two ask the SAME question of the SAME artifacts. A preview-side
--   copy of this projection would be free to disagree about which
--   animations are atlas-backed, and a viewer that renders art the game
--   refuses is worth nothing as a gate.
module Unit.Atlas.Yaml
    ( unitAnimFacts
    , resolveUnitAtlases
    , resolveUnitAtlasesIn
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Engine.Asset.YamlUnits (UnitYamlAnim(..))
import Unit.Atlas.Index (AtlasLoadError, YamlAnimFacts(..))
import Unit.Atlas.Load (loadUnitAtlasIndexIn)
import Unit.Atlas.Types (AtlasAnimation)
import Unit.Direction (parseDirectionName)

-- | What the unit YAML declares about each animation, in the shape
--   'Unit.Atlas.Index.planUnitAtlasStorage' checks an index against.
--
--   A direction key the engine does not recognise is DROPPED, so the
--   facts describe exactly the frames this build would compile against
--   rather than the ones the file happens to name. The compiler applies
--   the same rule, so an unparseable direction is a mismatch neither
--   side invents — and @tools\/pack_atlas.py@ rejects one outright, so
--   no shipped tree has any.
unitAnimFacts ∷ Map.Map Text UnitYamlAnim → Map.Map Text YamlAnimFacts
unitAnimFacts = fmap animFacts
  where
    animFacts ya = YamlAnimFacts
        { yafFps    = uyaFps ya
        , yafLoop   = uyaLoop ya
        , yafFlip   = uyaFlip ya
        , yafFrames = Map.fromList
            [ (dir, map T.unpack paths)
            | (dirKey, paths) ← Map.toList (uyaFrames ya)
            , Just dir ← [parseDirectionName dirKey] ]
        }

-- | Resolve the compiled atlas backing every animation a unit declares
--   (#1259; total since #1261).
--
--   The unit's compiled index is the whole answer, and on success it
--   covers EXACTLY the animations the YAML declares — there is no
--   second representation for an animation to be in. Everything else,
--   including the freshness of the compiled artifacts against this
--   unit's own source art, is 'Unit.Atlas.Load.loadUnitAtlasIndex'.
--
--   A 'Left' is NEVER a reason to draw the source frames instead: a
--   unit's animations either load from its compiled atlases or the unit
--   does not load at all, in both the game and the viewer.
resolveUnitAtlases
    ∷ Text
    → Map.Map Text UnitYamlAnim
    → IO (Either AtlasLoadError (HM.HashMap Text AtlasAnimation))
resolveUnitAtlases = resolveUnitAtlasesIn ""

-- | 'resolveUnitAtlases' against an explicit filesystem ROOT.
--
--   Production passes @""@ — every resource path is already relative to
--   the resource root the executable chdir'd into. A root is supplied
--   only to point the resolution at a fixture tree, which is what lets
--   a rejected-index case be exercised without writing into the shipped
--   asset tree. Same contract as
--   'Unit.Atlas.Load.loadUnitAtlasIndexIn', whose root this is.
resolveUnitAtlasesIn
    ∷ FilePath
    → Text
    → Map.Map Text UnitYamlAnim
    → IO (Either AtlasLoadError (HM.HashMap Text AtlasAnimation))
resolveUnitAtlasesIn root name yamlAnims =
    loadUnitAtlasIndexIn root name (unitAnimFacts yamlAnims)
