{-# LANGUAGE Strict #-}
-- | The structure TEARDOWN presentation lifecycle (#2491, BDA-14): how
--   clearing one placed piece captures its transient visual, how that
--   visual's frame is derived from the game clock, and when it expires.
--
--   Everything here is PURE and takes the clock, the catalogue, the
--   palette and the piece as explicit arguments — the same discipline
--   "Building.Destruction" is written under, and for the same reason:
--   the scanned render entry point emits nothing without a texture
--   system (the headless state), so the capture, timing and expiry
--   contracts are only assertable through functions that need no GPU.
--
--   The split of authority is the whole design.
--   @World.Thread.Command.Edit.Structure.handleWorldClearStructureCommand@
--   is the FUNCTIONAL boundary: it appends the @WeClearStructure@ edit
--   and deletes the resident overlay entry, immediately and
--   authoritatively. "Immediate" means AT WORLD-COMMAND APPLICATION, not
--   at @structure.clear@'s return — the Lua verb retracts staging and
--   queues the command, and the overlay stands until the world thread
--   processes it. What that handler captures first, through
--   'captureStructureDestruction', is a 'StructureDestructionEffect' —
--   render-only, owning nothing — that the effects pass draws through
--   the SAME 'Structure.Render.structurePieceQuadsResolved' geometry a
--   placed piece and a construction ghost use, and that the world tick
--   prunes once 'destructionEffectExpired'. No piece survives the
--   animation; the animation survives the piece.
--
--   Only a DECLARED clip is ever played. There is no fallback: an
--   appearance without one is cleared with no visual, and nothing
--   substitutes the static sprite, a fade, a reversed construction
--   sequence, or another appearance's frames.
--
--   __Nothing here owns gameplay state.__ An effect is not a structure
--   piece: it is in no chunk overlay and no staging cache, it is not
--   selectable, not a pathing or placement obstacle, not a wall
--   neighbour for cap or Wire shape resolution, and it is never written
--   to a save. Every structure query walks @lcStructures@ and the
--   staging cache alone, neither of which this touches.
module Structure.Destruction
    ( -- * The effect
      StructureDestructionEffect(..)
    , StructureDestructions
    , emptyStructureDestructions
    , destructionSlotKey
      -- * Capture
    , DestructionCapture(..)
    , captureStructureDestruction
    , insertDestructionEffect
      -- * Playback timing
    , destructionEffectDuration
    , destructionEffectElapsed
    , destructionEffectExpired
    , destructionEffectFrameIndex
      -- * Collection maintenance
    , anyDestructionExpired
    , pruneExpiredDestructionEffects
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Engine.Asset.Handle (TextureHandle)
import World.Page.Types (WorldPageId)
import Structure.ArtCatalog
    ( AppearanceKey(..), DestructionSequence(..), StructureArtCatalog
    , appearanceForTexturePath, resolveDestructionSequence )
import Structure.Palette (TexPalette, lookupPath)
import Structure.Types (StructurePieceData(..))

-- | One piece's teardown presentation: everything the effects pass
--   needs once the piece it was captured from is gone.
--
--   It is NOT a structure piece. It carries the removed piece's
--   IDENTITY — where it stood, what it was drawn as, which appearance
--   that is — and its own clock, and nothing else. Requirement 9's
--   consequence is visible in the type: there is no field a save
--   component could want, because the persistent authority
--   (@WeClearStructure@) has already said the piece is gone.
data StructureDestructionEffect = StructureDestructionEffect
    { sdePage     ∷ !WorldPageId
      -- ^ The cleared piece's page. An effect on a HIDDEN page is
      --   counted but never drawn, and expires on the same schedule
      --   either way ('pruneExpiredDestructionEffects' is driven by the
      --   world tick, not by a render pass).
    , sdeGX       ∷ !Int
    , sdeGY       ∷ !Int
      -- ^ The CANONICAL tile the clear was keyed at (#1175) — the same
      --   coordinates the @WeClearStructure@ edit carries, so the effect
      --   cannot draw at an alias the piece never occupied.
    , sdeSlotTag  ∷ !Word8
      -- ^ @fromEnum@ of the piece's 'Structure.Types.StructureSlot',
      --   the overlay's own key half.
    , sdeGridZ    ∷ !Int
    , sdeTexId    ∷ !Int
    , sdeFaceId   ∷ !Int
      -- ^ The removed piece's palette ids. Kept beside the paths
      --   because the runtime handle map is resolved from THEM, and a
      --   capture must not depend on that map already being populated
      --   (post-load reconciliation legitimately leaves it empty for a
      --   while).
    , sdeTexPath  ∷ !Text
      -- ^ The piece's static sprite path: the appearance identity, and
      --   what the wall-rotation catalogue is keyed on. Required — an
      --   effect cannot exist without one, because it is what resolved
      --   the clip.
    , sdeFacePath ∷ !(Maybe Text)
      -- ^ Its facemap path, when the palette still had one. 'Nothing'
      --   only skips the wall rotation, which is the same fallback a
      --   placed piece takes.
    , sdeTexHandle  ∷ !(Maybe TextureHandle)
    , sdeFaceHandle ∷ !(Maybe TextureHandle)
      -- ^ The handles as they stood AT CAPTURE, or 'Nothing' when the
      --   palette handle map had not resolved them yet. The render pass
      --   prefers the live map and falls back to these; either way the
      --   effect exists, keeps its clip and keeps its start time, and
      --   simply emits no quad until a handle is available.
    , sdePack     ∷ !Text
    , sdeAppearance ∷ !AppearanceKey
      -- ^ The authored appearance the piece was placed as. A WALL's
      --   DRAWN appearance rotates with the camera and is resolved per
      --   frame from this one; every other kind's is this one.
    , sdeFrameCount ∷ !Int
    , sdeFps        ∷ !Double
      -- ^ The captured clip's SHAPE, carried rather than looked up so
      --   timing and expiry are fixed at capture. Registration makes a
      --   wall family's four directions agree on both, which is what
      --   keeps the frame index — and therefore expiry — facing-blind.
    , sdeStartedAt  ∷ !Double
      -- ^ Game-clock seconds at the clear: frame zero. Read from the
      --   same monotonic, pause-frozen clock the rest of the session
      --   times against, so a paused effect stays frozen at its phase.
    } deriving (Show, Eq)

-- | One page's live teardown presentations, keyed exactly as the chunk
--   overlay keys a piece — @(gx, gy, slot tag)@ — so a slot holds at
--   most one, and re-placing and re-clearing the same slot mid-playback
--   replaces the effect rather than accumulating them.
type StructureDestructions =
    HM.HashMap (Int, Int, Word8) StructureDestructionEffect

emptyStructureDestructions ∷ StructureDestructions
emptyStructureDestructions = HM.empty

-- | The collection key of one effect.
destructionSlotKey ∷ StructureDestructionEffect → (Int, Int, Word8)
destructionSlotKey e = (sdeGX e, sdeGY e, sdeSlotTag e)

-- | What a clear's capture attempt produced.
--
--   Three outcomes, kept apart because they owe the caller different
--   things. Only 'CapturedEffect' produces a visual; only
--   'CaptureUndeclared' owes a diagnostic; 'CaptureSilent' owes nothing
--   at all, because a piece whose sprite belongs to no registered
--   appearance is not a pack's missing declaration — it is art the
--   catalogue was never told about.
data DestructionCapture
    = CaptureSilent
    | CaptureUndeclared !Text !AppearanceKey
    | CapturedEffect !StructureDestructionEffect
    deriving (Show, Eq)

-- | Capture the presentation of the piece @spd@ being cleared from
--   @(gx, gy, slotTag)@ on @page@ at game time @now@.
--
--   Pure, and deliberately dependent on nothing the render thread owns:
--   the catalogue and the palette are both read-only inputs here, and
--   the handle map is CONSULTED but never required (the review's
--   correction — a temporarily empty handle map must not lose the
--   effect, only delay its first drawn frame).
--
--   @now@ is frame zero. Nothing about the piece's own history is
--   captured or could be: a 'StructurePieceData' stores palette ids and
--   a z, so there is no placement time, no progress and no pack field to
--   inherit.
captureStructureDestruction
    ∷ StructureArtCatalog
    → TexPalette
    → HM.HashMap Int TextureHandle   -- ^ palette id → runtime handle
    → Double                         -- ^ game-clock seconds
    → WorldPageId
    → Int → Int → Word8              -- ^ canonical tile and slot tag
    → StructurePieceData
    → DestructionCapture
captureStructureDestruction cat palette handles now page gx gy slotTag spd =
    case lookupPath (spdTexId spd) palette of
        Nothing      → CaptureSilent
        Just texPath → case appearanceForTexturePath cat texPath of
            Nothing           → CaptureSilent
            Just (pack, ak) → case resolveDestructionSequence cat pack ak of
                Nothing → CaptureUndeclared pack ak
                Just ds → CapturedEffect StructureDestructionEffect
                    { sdePage       = page
                    , sdeGX         = gx
                    , sdeGY         = gy
                    , sdeSlotTag    = slotTag
                    , sdeGridZ      = spdGridZ spd
                    , sdeTexId      = spdTexId spd
                    , sdeFaceId     = spdFaceId spd
                    , sdeTexPath    = texPath
                    , sdeFacePath   = lookupPath (spdFaceId spd) palette
                    , sdeTexHandle  = HM.lookup (spdTexId spd) handles
                    , sdeFaceHandle = HM.lookup (spdFaceId spd) handles
                    , sdePack       = pack
                    , sdeAppearance = ak
                    , sdeFrameCount = V.length (dsFrames ds)
                    , sdeFps        = dsFps ds
                    , sdeStartedAt  = now
                    }

-- | Store one captured effect under its own slot key.
insertDestructionEffect ∷ StructureDestructionEffect → StructureDestructions
                        → StructureDestructions
insertDestructionEffect e = HM.insert (destructionSlotKey e) e

-- | The clip's full length in game seconds: @frameCount / fps@, so
--   every frame — the last included — gets its whole @1 / fps@
--   interval. The same rounding 'Building.Destruction.destructionDuration'
--   uses, deliberately.
destructionEffectDuration ∷ StructureDestructionEffect → Double
destructionEffectDuration e =
    fromIntegral (sdeFrameCount e) / sdeFps e

-- | Game seconds since frame zero, floored at 0 so a clock that has not
--   reached the start yet still reads as frame zero rather than
--   wrapping.
destructionEffectElapsed ∷ Double → StructureDestructionEffect → Double
destructionEffectElapsed now e = max 0 (now - sdeStartedAt e)

-- | True once the clip has run its full duration: from that instant the
--   effect emits no quad and is eligible for pruning.
--
--   Independent of every render state — page visibility, chunk
--   residency, texture system, handle map, z slice, depth alpha — which
--   is requirement 7. Cleanup cannot depend on the effect having been
--   drawn, because the states in which it is never drawn are exactly
--   the states in which it would otherwise leak.
destructionEffectExpired ∷ Double → StructureDestructionEffect → Bool
destructionEffectExpired now e =
    destructionEffectElapsed now e ≥ destructionEffectDuration e

-- | The semantic frame index at @now@: @floor (elapsed * fps)@, clamped
--   to the last frame while the effect is still inside its duration, and
--   'Nothing' at or past it.
--
--   Never wraps and never reverses — the clip plays once, forward. The
--   same index is selected whatever facing the camera is at, which is
--   what registration's equal-length, equal-fps rule for a wall family
--   guarantees.
destructionEffectFrameIndex ∷ Double → StructureDestructionEffect → Maybe Int
destructionEffectFrameIndex now e
    | destructionEffectExpired now e = Nothing
    | otherwise =
        let elapsed = destructionEffectElapsed now e
            raw     = floor (elapsed * sdeFps e) ∷ Int
        in Just (max 0 (min (sdeFrameCount e - 1) raw))

-- | Is anything in this collection out of time at @now@? Pure so the
--   world tick can decide whether a page write is needed at all before
--   performing one.
anyDestructionExpired ∷ Double → StructureDestructions → Bool
anyDestructionExpired now = any (destructionEffectExpired now) ∘ HM.elems

-- | Drop every effect whose clip has run out at @now@.
pruneExpiredDestructionEffects
    ∷ Double → StructureDestructions → StructureDestructions
pruneExpiredDestructionEffects now =
    HM.filter (not ∘ destructionEffectExpired now)
