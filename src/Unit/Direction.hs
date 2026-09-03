{-# LANGUAGE Strict, DeriveGeneric, DeriveAnyClass #-}
module Unit.Direction
    ( Direction(..)
    , dirIndex
    , indexToDir
    , allDirections
    , mirrorDir
    , parseDirectionName
    ) where

import UPrelude
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Serialize (Serialize)

-- | Eight compass directions, ordered clockwise from South.
--
--   APPEND-ONLY. The constructor order is load-bearing in two ways:
--   (1) `Enum`-derived `fromEnum`/`toEnum` drive the rotation arithmetic
--   in `dirIndex`/`indexToDir`, and (2) `Generic`-derived `Serialize` is
--   positional by constructor tag, so reordering or inserting a
--   constructor silently maps existing saved `usFacing` values to the
--   wrong direction.
--
--   TWO save components store this enum: `units`
--   (`UnitInstanceDTO.uidFacing`, declared in
--   `World.Save.Component.EntitySnapshots`) and `unit-sim`
--   (`UnitSimStateDTO.simFacing`, declared in
--   `World.Save.Component.EntitySimulation`). If the geometry ever
--   needs different
--   cardinality (16-way etc.), or the order otherwise has to change,
--   BOTH must be migrated; `currentSaveVersion` is a bookkeeping marker
--   and does not gate on-disk compatibility. For each component: raise
--   its `csVersion`, freeze the outgoing DTO, and register that frozen
--   type in `csOlderVersions` via `atVersion` with an explicit
--   migration — `componentCodec` derives `ccInputVers` from those
--   declarations, so the reader gains the new version while retaining
--   every version it already accepted.
--
--   Retaining a version means still DECODING it, so freezing the
--   OUTGOING shape is only half the job: EVERY version left in
--   `csOlderVersions` needs a wire type reaching a frozen COPY of the
--   constructor order that version was written with. Today's frozen
--   DTOs do not satisfy that — `UnitInstanceDTOv1.uid1Facing`, and
--   `UnitSimStateDTOv1.sim1Facing` which `unit-sim` v1 AND v2 both
--   decode through, still name this live type, so a reorder that froze
--   only the current shape would decode every retained legacy payload
--   against the new order anyway. `unitSimCodec`'s v1/v2 entries are
--   the shape to copy for version dispatch and explicit migration
--   only — no codec has needed a frozen enum yet, so they do not
--   demonstrate that half.
data Direction = DirS | DirSW | DirW | DirNW | DirN | DirNE | DirE | DirSE
    deriving (Show, Eq, Ord, Enum, Bounded, Generic, Serialize)

-- | Map a Direction to its clockwise index (S=0, SW=1, … SE=7)
dirIndex ∷ Direction → Int
dirIndex = fromEnum

-- | Inverse of dirIndex (mod 8)
indexToDir ∷ Int → Direction
indexToDir n = toEnum (n `mod` 8)

-- | All eight directions in clockwise order
allDirections ∷ [Direction]
allDirections = [minBound .. maxBound]

-- | The "western half" of the compass (SW, W, NW) can be obtained by
--   horizontally mirroring their eastern counterparts (SE, E, NE). This
--   lets bilaterally-symmetric animations ship 5 directional sprites
--   instead of 8 — the renderer flips UVs at draw time for the western
--   directions. Returns @Nothing@ for directions that are their own
--   canonical (S, N, NE, E, SE) — those use their own asset, no flip.
--
--   Used by `Unit.Render.resolveTexture` / `pickFrame` to fall back to
--   the mirror direction's sprite + a flipX flag when the requested
--   direction has no entry. Per-animation opt-in is governed by which
--   directions get authored — if an animation needs an asymmetric prop
--   (e.g. weapon in the right hand) the asset author simply provides
--   all 8 sprites and the mirror fallback never triggers.
mirrorDir ∷ Direction → Maybe Direction
mirrorDir DirSW = Just DirSE
mirrorDir DirW  = Just DirE
mirrorDir DirNW = Just DirNE
mirrorDir _     = Nothing

-- | Parse a direction written as text: the short compass abbreviation
--   (@"S"@, @"sw"@) or the long hyphenated name (@"south"@,
--   @"south-east"@), case-insensitively.
--
--   ONE table, because the same vocabulary is read from three places
--   that must never disagree about which spellings exist: a unit YAML's
--   @frames:@ direction keys
--   (@Engine.Scripting.Lua.API.Units.Yaml.parseDirKey@), an animation
--   asset tree's direction FOLDER names
--   (@Engine.Preview.Unit.parseDirectionDirName@), and — since #1260
--   routed the preview through the compiled index — the YAML→facts
--   projection the atlas selection is validated against
--   ("Unit.Atlas.Yaml"). A spelling one of those accepted and another
--   didn't would show up as an animation that loads in the game and
--   half-loads in the viewer.
parseDirectionName ∷ Text → Maybe Direction
parseDirectionName t = case T.toLower t of
    "s"          → Just DirS
    "sw"         → Just DirSW
    "w"          → Just DirW
    "nw"         → Just DirNW
    "n"          → Just DirN
    "ne"         → Just DirNE
    "e"          → Just DirE
    "se"         → Just DirSE
    "south"      → Just DirS
    "south-west" → Just DirSW
    "west"       → Just DirW
    "north-west" → Just DirNW
    "north"      → Just DirN
    "north-east" → Just DirNE
    "east"       → Just DirE
    "south-east" → Just DirSE
    _            → Nothing
