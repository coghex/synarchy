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
--     * A family's TABLE holds every path it renders, but only the paths
--       it OWNS claim it in the reverse index. A pack variant may override
--       any SUBSET of the wall art (@data/structure_packs/*.yaml@'s
--       @variants@) and INHERITS the default's paths for the rest — so a
--       variant overriding only @wall_ne@ shares @wall_nw.png@ with the
--       default family, and a piece placed with that shared path is
--       indistinguishable from a default wall in the stored data. Letting
--       the variant claim it would rotate a DEFAULT wall into the
--       VARIANT's art. Ownership is what the pack YAML actually states, so
--       the shared path stays the default family's while the variant still
--       resolves its own inherited art through its own table.
--     * Two families OWNING one path is contradictory pack data — nothing
--       in a placement can say which was meant — so the path is marked
--       AMBIGUOUS and any pair placed with it stops rotating rather than
--       picking a winner. BOTH halves stop: a piece whose sprite is
--       contested does not get rotated on the strength of its facemap's
--       uncontested claim, or the other way round. That makes the
--       catalogue independent of registration ORDER, which nothing
--       guarantees.
--     * 'rotatedWallArt' identifies ONE family for the placed pair and
--       takes BOTH rotated assets from it, so a wall can never be drawn
--       with a sprite from one variant and a mask from another. The
--       family is the one whose own art at this edge IS the pair that was
--       placed and which OWNS at least one of the two paths — ownership
--       is what separates a variant from the default it inherits from, and
--       requiring the pair to match is what stops a variant answering for
--       art it does not actually carry. No such family, or more than one,
--       means the piece is left exactly as placed. Art from outside any
--       registered pack (a hand-placed arbitrary path) is likewise left
--       alone, which is also why 'FaceSouth' is the identity for every
--       input.
--     * Variant is therefore preserved through BOTH halves:
--       @damaged/wall_ne.png@ identifies @dungeon_1@'s @damaged@ family,
--       and that family's own table then supplies the rotated sprite AND
--       the rotated cap facemap — including where it merely inherits the
--       default masks, which is what @damaged@ actually does.
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
    , swcTexOwner  ∷ !(HM.HashMap Text (Maybe Int))
      -- ^ Sprite path → the family that DECLARES it. 'Nothing' is a path
      --   two families both claim, which is contradictory pack data: it
      --   never rotates.
    , swcFaceOwner ∷ !(HM.HashMap Text (Maybe Int))
      -- ^ Cap-facemap path → the family that declares it, same ambiguity
      --   rule. A path a variant merely INHERITS makes no claim in
      --   either map, so it keeps resolving through the family that
      --   declared it.
    , swcHandles  ∷ !(HM.HashMap Text TextureHandle)
      -- ^ Runtime handle per registered path. The rotated art is a
      --   DIFFERENT path from the placed one, which the palette's
      --   id→handle table need not have resolved (nothing placed it), so
      --   the registration carries the handles Lua already loaded rather
      --   than relying on that table.
      --
      --   FIRST registration wins. @engine.loadTexture@ mints a fresh
      --   handle on every call and does not dedupe by path, so a variant
      --   re-loading a facemap it INHERITS hands over a second, duplicate
      --   handle for a path the default already registered — and a later
      --   overwrite would swap a live, uploaded handle for one whose
      --   upload may not have landed. A path's handle is a property of
      --   the path, so the first one is kept.
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
    , waeOwned  ∷ !Bool
      -- ^ Does this family DECLARE the path, or merely inherit it from
      --   the pack's default art? Only a declared path claims the family
      --   in the reverse index — see the module header.
    } deriving (Show, Eq)

-- | Register one complete directional family. Returns 'Nothing' — the
--   catalogue unchanged — when the entries do not cover all four edges
--   and all sixteen (edge, cap) facemaps, so a mis-registration is a
--   loud no-op rather than a half-rotating pack.
--
--   Registering a family the catalogue already holds is an idempotent
--   no-op, so a second call for the same pack variant cannot make that
--   variant's own paths look contradictory.
registerWallFamily ∷ [WallArtEntry] → StructureWallCatalog
                   → Maybe StructureWallCatalog
registerWallFamily entries cat
    | M.size textures ≢ 4 ∨ M.size facemaps ≢ 16 = Nothing
    | family `elem` IM.elems (swcFamilies cat)   = Just cat
    | otherwise = Just cat
        { swcFamilies   = IM.insert fi family (swcFamilies cat)
        , swcTexOwner   = foldr (\e → claim (waePath e) fi)
                                (swcTexOwner cat)
                                [ e | e ← texEntries, waeOwned e ]
        , swcFaceOwner  = foldr (\(e, _) → claim (waePath e) fi)
                                (swcFaceOwner cat)
                                [ (e, c) | (e, c) ← faceEntries, waeOwned e ]
        , swcHandles    = foldr (\e → HM.insertWith (\_new old → old)
                                                    (waePath e) (waeHandle e))
                                (swcHandles cat) entries
        , swcNextFamily = fi + 1
        }
  where
    fi          = swcNextFamily cat
    family      = WallFamily textures facemaps
    texEntries  = [ e | e ← entries, isNothing (waeCaps e) ]
    faceEntries = [ (e, c) | e ← entries, Just c ← [waeCaps e] ]
    textures    = M.fromList [ (waeEdge e, waePath e) | e ← texEntries ]
    facemaps    = M.fromList [ ((waeEdge e, c), waePath e) | (e, c) ← faceEntries ]
    -- A second, DIFFERENT owner for one path is contradictory pack data:
    -- mark it ambiguous instead of letting registration order decide.
    claim ∷ Text → Int → HM.HashMap Text (Maybe Int) → HM.HashMap Text (Maybe Int)
    claim path v = HM.insertWith merge path (Just v)
      where merge new old | old ≡ new = old
                          | otherwise = Nothing

-- | The sprite and cap facemap a wall on AUTHORED edge @edge@, placed
--   with @texPath@\/@facePath@, is drawn with at @facing@.
--
--   Each side is given as the placed PATH together with the runtime
--   handle that path already resolved to (the palette's own id→handle
--   entry, which is what the piece would otherwise be drawn with). A
--   resolved target that IS the placed path keeps that handle rather
--   than the catalogue's, so a rotation that changes nothing changes
--   nothing — including at 'FaceSouth', where the screen edge and cap
--   order are both the identity and the answer is always the placed pair.
--
--   'Nothing' means "draw exactly what was placed": no family carries
--   this exact pair at this edge, or several do and none is singled out
--   by ownership, or either path is ambiguously owned, or the resolved
--   family has no art for the target. A pair whose facemap belongs to a
--   different authored edge than its sprite matches no family at all, so
--   a texture from one direction can never be paired with a mask from
--   another.
rotatedWallArt ∷ StructureWallCatalog → CameraFacing → WallEdge
               → (Text, TextureHandle)   -- ^ placed sprite: path + its live handle
               → (Text, TextureHandle)   -- ^ placed cap facemap: path + its live handle
               → Maybe (TextureHandle, TextureHandle)
rotatedWallArt cat facing edge (texPath, texHandle) (facePath, faceHandle) =
  case matches of
    [(fam, caps)] → do
        let screen = screenWallEdge facing edge
        tPath ← M.lookup screen (wfTextures fam)
        fPath ← M.lookup (screen, rotateWallCaps facing edge caps) (wfFacemaps fam)
        (,) <$> resolve texPath texHandle tPath
            <*> resolve facePath faceHandle fPath
    _ → Nothing
  where
    resolve placedPath placedHandle target
        | target ≡ placedPath = Just placedHandle
        | otherwise           = HM.lookup target (swcHandles cat)
    -- The families this placement could have come from: their own art at
    -- @edge@ is exactly the pair that was placed. A variant that inherits
    -- both halves is indistinguishable from the default here, which is
    -- what ownership below settles.
    carriers =
        [ (i, fam, caps)
        | (i, fam) ← IM.toList (swcFamilies cat)
        , M.lookup edge (wfTextures fam) ≡ Just texPath
        , caps ← [ c | ((e, c), path) ← M.toList (wfFacemaps fam)
                     , e ≡ edge, path ≡ facePath ] ]
    -- ...narrowed to the ones that DECLARE one of the two paths rather
    -- than inheriting both. Each lookup has THREE outcomes, and ABSENT is
    -- not AMBIGUOUS: @Nothing@ means no family claims that path (the
    -- normal case for a path a variant merely inherits), so it makes no
    -- claim and leaves its companion to decide; @Just (Just i)@ claims
    -- family @i@; @Just Nothing@ is the marker 'registerWallFamily'
    -- installs when two families claim one path. That last one is
    -- contradictory pack data about a path this piece was actually placed
    -- with, so the whole PAIR stops rotating -- an unambiguous companion
    -- must not be allowed to pick the winner the contested half refuses
    -- to name.
    texOwner  = HM.lookup texPath  (swcTexOwner cat)
    faceOwner = HM.lookup facePath (swcFaceOwner cat)
    contested = any isNothing (catMaybes [texOwner, faceOwner])
    owners    = catMaybes [texOwner, faceOwner]
    matches
      | contested = []
      | otherwise = [ (fam, caps) | (i, fam, caps) ← carriers, Just i `elem` owners ]
