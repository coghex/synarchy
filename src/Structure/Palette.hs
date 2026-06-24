{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Texture PALETTE — the save-level translation between stable texture PATHS
--   and compact integer ids. Structure edits (and any future per-chunk object
--   that references a texture) store a `paletteId :: Int`, NOT the path string
--   (MB of duplication across many objects) and NOT the runtime `loadTexture`
--   handle (non-deterministic per session).
--
--   The palette is OWNED by us, so a paletteId is stable across save/load — no
--   remapping. The runtime handle non-determinism is absorbed at load by
--   resolving each path → `loadTexture` once into a small `paletteId → handle`
--   array (palette-sized, not object-sized).
--
--   Saved once per game (`sdTexPalette`); a live engine ref interns new paths
--   as they are first placed.
module Structure.Palette
    ( TexPalette(..)
    , emptyTexPalette
    , internPath
    , lookupPath
    , lookupId
    ) where

import UPrelude
import Data.Serialize (Serialize(..), Get)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM

-- | Bidirectional path↔id map + the next id to hand out. Both maps are kept
--   in memory; only the pairs are serialized (the inverse is rebuilt on load).
data TexPalette = TexPalette
    { tpPathToId ∷ !(HM.HashMap Text Int)
    , tpIdToPath ∷ !(HM.HashMap Int Text)
    , tpNextId   ∷ !Int
    } deriving (Show, Eq)

emptyTexPalette ∷ TexPalette
emptyTexPalette = TexPalette HM.empty HM.empty 0

-- | Serialize only nextId + the (path,id) pairs; rebuild both maps on load.
--   (Text + lists + tuples all have Serialize instances in scope — cf.
--   sdLuaModules :: HashMap Text Text.)
instance Serialize TexPalette where
    put tp = do
        S.put (tpNextId tp)
        S.put (HM.toList (tpPathToId tp))
    get = do
        n     ← S.get
        pairs ← S.get ∷ Get [(Text, Int)]
        let p2i = HM.fromList pairs
            i2p = HM.fromList [(i, p) | (p, i) ← pairs]
        return (TexPalette p2i i2p n)

-- | Return the id for a path, assigning a fresh one if it is new.
internPath ∷ Text → TexPalette → (Int, TexPalette)
internPath path tp = case HM.lookup path (tpPathToId tp) of
    Just i  → (i, tp)
    Nothing →
        let i = tpNextId tp
        in ( i
           , tp { tpPathToId = HM.insert path i (tpPathToId tp)
                , tpIdToPath = HM.insert i path (tpIdToPath tp)
                , tpNextId   = i + 1 } )

-- | id → path (for resolving to a runtime handle at render/load).
lookupPath ∷ Int → TexPalette → Maybe Text
lookupPath i tp = HM.lookup i (tpIdToPath tp)

-- | path → id (read-only; does not assign).
lookupId ∷ Text → TexPalette → Maybe Int
lookupId path tp = HM.lookup path (tpPathToId tp)
