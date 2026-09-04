{-# LANGUAGE Strict #-}
-- | The building destruction presentation lifecycle (#2091, BDA-3):
--   how a demolition captures its transient visual, how that visual's
--   frame is derived from the game clock, and when it expires.
--
--   Everything here is PURE and takes the clock, the facing, the
--   instance and the definition as explicit arguments. That is not a
--   style choice: the scanned render entry point emits nothing without
--   a texture system (the headless state), so the timing, facing and
--   expiry contracts are only assertable through functions that need no
--   GPU — the same reason 'Building.Visual' is pure.
--
--   The split of authority is the whole design. The @BuildingDestroy@
--   drain ('Building.Thread.Command') is the FUNCTIONAL boundary: it
--   deletes the live instance, clears its selection, forgets its
--   container knowledge and retires its power node, immediately. What
--   it captures first, through 'captureDestructionEffect', is a
--   'Building.Types.DestructionEffect' — render-only, owning nothing —
--   that 'Building.Render' draws from the SAME facing-aware geometry a
--   placed building uses and that the drain prunes once
--   'destructionExpired'. No building survives the animation; the
--   animation survives the building.
--
--   Only the DECLARED @destruction@ role is ever played. There is no
--   fallback: a definition without one is removed with no visual, and
--   nothing reverses construction frames, substitutes another
--   lifecycle's art, mirrors another facing, or infers a clip from a
--   @demolish@ directory on disk.
module Building.Destruction
    ( -- * Resolving the declared role
      resolveDestructionClip
    , captureDestructionEffect
      -- * Playback timing
    , destructionDuration
    , destructionElapsed
    , destructionExpired
    , destructionFrameIndex
    , destructionFrame
      -- * Collection maintenance
    , pruneExpiredDestructions
    , destructionsOnPages
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Engine.Asset.Handle (TextureHandle)
import Engine.Graphics.Camera (CameraFacing)
import World.Page.Types (WorldPageId)
import Building.Schema
import Building.Types
import Building.Visual (spriteAnchorOffset)

-- | Resolve a definition's @destruction@ role to a validated clip.
--
--   * 'Right Nothing' — the definition declares no destruction role.
--     Its demolition is silent: removed immediately, no visual.
--   * 'Right (Just clip)' — a playable clip.
--   * 'Left message' — the role is declared but INVALID: it names an
--     animation the definition does not declare, loops, has a
--     non-finite or non-positive fps, or has no frames. The message
--     names the building and the animation. The caller treats it as
--     "no visual" and reports it; it never corrects the declaration
--     (no clamping, no un-looping) and never blocks the demolition.
--
--   The LOOP rule is this role's alone: the YAML decoder accepts either
--   loop value for every animation (the built loop legitimately loops),
--   so it applies at the point the role is resolved.
--
--   The finite-positive fps rule became a GENERAL authoring-boundary
--   rule with #2347 — 'Engine.Asset.YamlBuildings' now refuses the whole
--   file for any animation declaring a non-finite or non-positive fps —
--   and the check below is deliberately RETAINED rather than folded
--   into it. 'BuildingDef' is a public constructor: a hand-built
--   definition, a Lua-registered one, and every test fixture reach this
--   resolver without passing the decoder at all, so dropping the check
--   here would move the guarantee from "always" to "only for definitions
--   that came from a YAML file".
resolveDestructionClip ∷ BuildingDef → Either Text (Maybe DestructionClip)
resolveDestructionClip def =
    case Map.lookup RoleDestruction (bdRoleAnims def) of
        Nothing → Right Nothing
        Just animName →
            let ctx = "building `" <> bdName def
                    <> "` destruction animation `" <> animName <> "`"
            in case HM.lookup animName (bdAnimations def) of
                Nothing → Left $
                    ctx <> " is not declared under `animations`"
                Just a
                    | banLoop a → Left $
                        ctx <> " declares `loop: true`; a destruction clip"
                            <> " plays once and must declare `loop: false`"
                    | isNaN (banFps a) ∨ isInfinite (banFps a) → Left $
                        ctx <> " declares a non-finite fps ("
                            <> tshow (banFps a)
                            <> "); a destruction clip needs a finite positive fps"
                    | banFps a ≤ 0 → Left $
                        ctx <> " declares fps " <> tshow (banFps a)
                            <> "; a destruction clip needs a finite positive fps"
                    | buildingAnimMaxFrames a ≤ 0 → Left $
                        ctx <> " declares no frames in any direction"
                    | otherwise → Right $ Just DestructionClip
                        { dcFps        = banFps a
                        , dcFrameCount = buildingAnimMaxFrames a
                        , dcFrames     = banFrames a
                        }

-- | Capture the presentation of @inst@ (identity @bid@, definition
--   @def@) being demolished at game time @now@ — everything the render
--   pass needs once the instance is gone. Same 'Either' shape as
--   'resolveDestructionClip', which is the only thing that can fail.
--
--   @now@ is frame zero, NOT 'biSpawnedAt': the clip plays identically
--   whatever the building's activity (Appearing, pre-delivery ghost,
--   Constructing or Built) was at the moment of destruction, and none
--   of the per-instance visual policies — ghost opacity, the selection
--   outline — carry over, because none of the state they read is
--   captured.
captureDestructionEffect
    ∷ Double → BuildingId → BuildingInstance → BuildingDef
    → Either Text (Maybe DestructionEffect)
captureDestructionEffect now bid inst def =
    fmap (fmap effectFrom) (resolveDestructionClip def)
  where
    effectFrom clip = DestructionEffect
        { deBuildingId   = bid
        , deDefName      = biDefName inst
        , dePage         = biPage inst
        , deAnchorX      = biAnchorX inst
        , deAnchorY      = biAnchorY inst
        , deGridZ        = biGridZ inst
        , deAnchorOffset = spriteAnchorOffset (Just def)
        , deClip         = clip
        , deStartedAt    = now
        }

-- | The clip's full length in game seconds: @frameCount / fps@, so
--   every frame — the last included — gets its whole @1 / fps@
--   interval. Facing-independent by construction ('dcFrameCount').
destructionDuration ∷ DestructionClip → Double
destructionDuration clip =
    fromIntegral (dcFrameCount clip) / realToFrac (dcFps clip)

-- | Game seconds since frame zero, floored at 0 so a clock that has
--   not reached the start yet (never in production, since the start IS
--   a clock reading) still reads as frame zero rather than wrapping.
destructionElapsed ∷ Double → DestructionEffect → Double
destructionElapsed now eff = max 0 (now - deStartedAt eff)

-- | True once the clip has run its full duration: from that instant
--   the effect emits no quad and is eligible for pruning. Independent
--   of any render state — page visibility, texture system, z slice,
--   depth alpha — so cleanup cannot depend on the effect having been
--   drawn.
destructionExpired ∷ Double → DestructionEffect → Bool
destructionExpired now eff =
    destructionElapsed now eff ≥ destructionDuration (deClip eff)

-- | The semantic frame index at @now@: @floor (elapsed * fps)@,
--   clamped to the last frame while the effect is still inside its
--   duration, and 'Nothing' at or past it. Never wraps — the clip is
--   non-looping by construction. The same index is selected from every
--   facing's own list, so a camera turn changes the handle and nothing
--   else.
destructionFrameIndex ∷ Double → DestructionEffect → Maybe Int
destructionFrameIndex now eff
    | destructionExpired now eff = Nothing
    | otherwise =
        let clip    = deClip eff
            elapsed = destructionElapsed now eff
            raw     = floor (elapsed * realToFrac (dcFps clip)) ∷ Int
        in Just (max 0 (min (dcFrameCount clip - 1) raw))

-- | The handle @facing@ shows at @now@: that facing's OWN declared
--   frame at 'destructionFrameIndex' — never a mirrored or substituted
--   direction, and never the static, construction, appearance or built
--   art. 'Nothing' once expired, or when this facing declares no
--   frames (a hand-built clip; the decoder refuses an empty direction).
--   A facing shorter than the clip length pins its last frame, which
--   is what keeps the index — and therefore expiry — facing-blind.
destructionFrame ∷ CameraFacing → Double → DestructionEffect
                 → Maybe TextureHandle
destructionFrame facing now eff =
    case destructionFrameIndex now eff of
        Nothing  → Nothing
        Just idx →
            let fs = facingAsset facing (dcFrames (deClip eff))
            in if V.null fs
               then Nothing
               else Just (fs V.! min idx (V.length fs - 1))

-- | Drop every effect whose clip has run out at @now@. Pure so the
--   drain can decide whether a manager write is needed at all before
--   performing one.
pruneExpiredDestructions
    ∷ Double → HM.HashMap BuildingId DestructionEffect
    → HM.HashMap BuildingId DestructionEffect
pruneExpiredDestructions now = HM.filter (not ∘ destructionExpired now)

-- | The effects belonging to any of the given pages — the visible set,
--   for rendering — mirroring 'buildingsOnPages'.
destructionsOnPages
    ∷ HS.HashSet WorldPageId
    → HM.HashMap BuildingId DestructionEffect
    → HM.HashMap BuildingId DestructionEffect
destructionsOnPages pages = HM.filter (\e → HS.member (dePage e) pages)
