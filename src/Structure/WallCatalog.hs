{-# LANGUAGE Strict #-}
-- | The DIRECTIONAL-ASSET resolution contract for structure walls
--   (#1712): given the art a wall was PLACED with, which sprite and cap
--   facemap draw it once the camera has rotated its edge onto a
--   different screen position.
--
--   Why a registry at all. A placed piece
--   ('Structure.Types.StructurePieceData') stores exactly one texture
--   and one facemap palette id — the pair the builder chose — so the
--   renderer on its own has no way to reach the other three directions
--   of the same pack/variant. Guessing them from the filenames is not an
--   option (a pack's paths are arbitrary), so the pack's own YAML is the
--   authority: @scripts/structures.lua@ registers each family it loads
--   (default art and every variant) as the twenty paths it already read
--   out of @data/structure_packs/<pack>.yaml@, with the runtime handles
--   it already holds. This module indexes those registrations so a
--   stored path answers "and what is this wall's NW sprite?".
--
--   Consequences worth knowing:
--
--     * Registration is keyed by PATH, never by palette id, and the
--       catalogue is never cleared. A load replaces the palette
--       wholesale ('World.Load.Publish'), which can reassign ids; paths
--       are what survive, so a wall keeps rotating correctly across a
--       save\/load with no re-registration.
--     * A family is registered ALL OR NOTHING — four sprites and all
--       sixteen cap facemaps, or the registration is refused. A partial
--       family would silently rotate some of a wall's directions and not
--       others.
--     * 'rotatedWallArt' rotates the sprite and its cap facemap TOGETHER
--       or not at all, and only when BOTH stored assets are registered
--       under the wall's own authored edge. Art from outside any
--       registered pack (a hand-placed arbitrary path) is left exactly
--       as placed, which is also why 'FaceSouth' is the identity for
--       every input.
--     * Variant is preserved because it is the TEXTURE's family that
--       picks the rotated sprite: @damaged/wall_ne.png@ is a different
--       path in a different family from @wall_ne.png@. The facemap is
--       resolved through its OWN family for the same reason — a variant
--       that inherits the default masks (as @dungeon_1@'s @damaged@
--       does) shares those paths with the default family, and both
--       families answer identically for them.
module Structure.WallCatalog
    ( WallFamily(..)
    , StructureWallCatalog(..)
    , emptyStructureWallCatalog
    , WallArtEntry(..)
    , registerWallFamily
    , rotatedWallArt
    ) where

import UPrelude
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.HashMap.Strict as HM
import Engine.Asset.Handle (TextureHandle)
import Engine.Graphics.Camera (CameraFacing(..))
import Structure.Facing
    ( WallEdge(..), WallCaps(..), screenWallEdge, rotateWallCaps )

-- | One pack variant's complete directional wall art: a sprite per edge
--   and a cap facemap per (edge, cap-suffix). Paths, not handles — see
--   'swcHandles' for why the two are separate.
data WallFamily = WallFamily
    { wfTextures ∷ !(M.Map WallEdge Text)
    , wfFacemaps ∷ !(M.Map (WallEdge, WallCaps) Text)
    } deriving (Show, Eq)

-- | Every registered family plus the reverse indices a stored path is
--   looked up through. Populated from Lua at pack load
--   (@structure.registerWallFamily@) and read by the structure renderer;
--   never cleared, and re-registering a family it already holds is a
--   no-op that keeps the existing entry.
data StructureWallCatalog = StructureWallCatalog
    { swcFamilies ∷ !(IM.IntMap WallFamily)
      -- ^ Families by allocation index.
    , swcTexEdge  ∷ !(HM.HashMap Text (Int, WallEdge))
      -- ^ Sprite path → its family and the AUTHORED edge it draws.
    , swcFaceEdge ∷ !(HM.HashMap Text (Int, WallEdge, WallCaps))
      -- ^ Cap-facemap path → its family, authored edge and cap state.
      --   A path shared by several families (a variant inheriting the
      --   default masks) keeps its FIRST registration; every family
      --   holding it answers identically for it, so which one wins does
      --   not change the result.
    , swcHandles  ∷ !(HM.HashMap Text TextureHandle)
      -- ^ Runtime handle per registered path. The rotated art is a
      --   DIFFERENT path from the placed one, which the palette's
      --   id→handle table need not have resolved (nothing placed it), so
      --   the registration carries the handles Lua already loaded rather
      --   than relying on that table.
    , swcNextFamily ∷ !Int
    } deriving (Show, Eq)

emptyStructureWallCatalog ∷ StructureWallCatalog
emptyStructureWallCatalog = StructureWallCatalog IM.empty HM.empty HM.empty HM.empty 0

-- | One registered asset: a sprite (@Nothing@ caps) or one of an edge's
--   four cap facemaps.
data WallArtEntry = WallArtEntry
    { waeEdge   ∷ !WallEdge
    , waeCaps   ∷ !(Maybe WallCaps)
    , waePath   ∷ !Text
    , waeHandle ∷ !TextureHandle
    } deriving (Show, Eq)

-- | Register one complete directional family. Returns 'Nothing' — the
--   catalogue unchanged — when the entries do not cover all four edges
--   and all sixteen (edge, cap) facemaps, so a mis-registration is a
--   loud no-op rather than a half-rotating pack.
registerWallFamily ∷ [WallArtEntry] → StructureWallCatalog
                   → Maybe StructureWallCatalog
registerWallFamily entries cat
    | M.size textures ≢ 4 ∨ M.size facemaps ≢ 16 = Nothing
    | otherwise = Just cat
        { swcFamilies   = IM.insert fi (WallFamily textures facemaps) (swcFamilies cat)
        , swcTexEdge    = foldr (\e m → HM.insert (waePath e) (fi, waeEdge e) m)
                                (swcTexEdge cat) texEntries
        , swcFaceEdge   = foldr (\(e, c) m →
                                    HM.insertWith (\_new old → old)
                                                  (waePath e) (fi, waeEdge e, c) m)
                                (swcFaceEdge cat) faceEntries
        , swcHandles    = foldr (\e m → HM.insert (waePath e) (waeHandle e) m)
                                (swcHandles cat) entries
        , swcNextFamily = fi + 1
        }
  where
    fi          = swcNextFamily cat
    texEntries  = [ e | e ← entries, isNothing (waeCaps e) ]
    faceEntries = [ (e, c) | e ← entries, Just c ← [waeCaps e] ]
    textures    = M.fromList [ (waeEdge e, waePath e) | e ← texEntries ]
    facemaps    = M.fromList [ ((waeEdge e, c), waePath e) | (e, c) ← faceEntries ]

-- | The sprite and cap facemap a wall on AUTHORED edge @edge@, placed
--   with @texPath@\/@facePath@, is drawn with at @facing@.
--
--   'Nothing' means "draw exactly what was placed": either asset
--   unregistered, the two disagreeing about the wall's authored edge
--   (which is the only way a texture from one direction could be paired
--   with a facemap from another — refused rather than rendered), or a
--   target the family does not hold. At 'FaceSouth' the screen edge and
--   cap order are both the identity, so a registered pair resolves back
--   to its own two paths and the result is the placed art unchanged.
rotatedWallArt ∷ StructureWallCatalog → CameraFacing → WallEdge → Text → Text
               → Maybe (TextureHandle, TextureHandle)
rotatedWallArt cat facing edge texPath facePath = do
    (texFam, texEdge)          ← HM.lookup texPath  (swcTexEdge cat)
    (faceFam, faceEdge, caps)  ← HM.lookup facePath (swcFaceEdge cat)
    if texEdge ≢ edge ∨ faceEdge ≢ edge then Nothing else do
        famT ← IM.lookup texFam  (swcFamilies cat)
        famF ← IM.lookup faceFam (swcFamilies cat)
        let screen = screenWallEdge facing edge
        tPath ← M.lookup screen (wfTextures famT)
        fPath ← M.lookup (screen, rotateWallCaps facing edge caps) (wfFacemaps famF)
        (,) <$> HM.lookup tPath (swcHandles cat) <*> HM.lookup fPath (swcHandles cat)
