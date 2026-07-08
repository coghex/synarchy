{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Pure world-decal render-record derivation (#606): turns a
--   'BloodDecal' plus the current game time into the data a world-space
--   quad needs — resolved texture reference, world placement, and an
--   aged tint/alpha — kept independent of Vulkan/GPU so the headless
--   debug surface ('Engine.Scripting.Lua.API.Blood') and the real
--   renderer ('World.Render.BloodQuads') share one definition instead of
--   two that could drift apart. See docs/blood_decals.md ("Aging").
module Blood.Render
    ( BloodRenderRecord(..)
    , decalTint
    , bloodRenderRecord
    , bloodRenderRecords
    ) where

import UPrelude
import Blood.Types
import World.Page.Types (WorldPageId)

-- | One decal's resolved render state at a given time. 'brrTexture' is
--   the stable 'BloodTextureId' — the renderer resolves it to a live
--   bindless slot, mirroring how every other world sprite keeps its
--   handle separate from the recyclable GPU slot.
data BloodRenderRecord = BloodRenderRecord
    { brrDecal    ∷ !BloodDecalId
    , brrTexture  ∷ !BloodTextureId
    , brrPage     ∷ !WorldPageId
    , brrX        ∷ !Float
    , brrY        ∷ !Float
    , brrSurfaceZ ∷ !Int
    , brrOffsetX  ∷ !Float
    , brrOffsetY  ∷ !Float
    , brrRotation ∷ !Float
    , brrScale    ∷ !Float
    , brrTintR    ∷ !Float
    , brrTintG    ∷ !Float
    , brrTintB    ∷ !Float
    , brrAlpha    ∷ !Float
    } deriving (Show, Eq)

-- | Fresh blood: wet, saturated dark red. Old blood: darker, desaturated
--   brown, and fainter — alpha fades toward a floor rather than to
--   nothing, so a very old mark reads as a faint dried stain instead of
--   popping out of existence (that's eviction's job, not aging's).
--   Never stored: aging is purely a function of @now@ and the decal's
--   own stored fields, so it never needs a texture rewrite (design
--   doc's "Aging"; issue #606 requirement 8).
decalTint ∷ Double → BloodDecal → (Float, Float, Float, Float)
decalTint now d =
    let wet = wetnessAt now d
        lerp a b t = a + (b - a) * t
        r = lerp dryR freshR wet
        g = lerp dryG freshG wet
        b = lerp dryB freshB wet
        alphaFloor = 0.35
        alpha = bdeOpacity d * lerp alphaFloor 1.0 wet
    in (r, g, b, alpha)
  where
    freshR = 0.55 ∷ Float; freshG = 0.02; freshB = 0.02
    dryR   = 0.22 ∷ Float; dryG   = 0.06; dryB   = 0.04

-- | One decal's render record, or 'Nothing' if its texture reference no
--   longer exists in the pool (issue #606 requirement 7: "omitted if
--   their texture reference has been evicted"). Defensive rather than
--   load-bearing — 'removeDecalsForTexture' already drops the decal
--   itself on eviction — but keeps the "no orphaned render record"
--   contract explicit rather than relying on that invariant forever.
bloodRenderRecord ∷ Double → BloodTexturePool → BloodDecal
                  → Maybe BloodRenderRecord
bloodRenderRecord now pool d = do
    _ ← lookupTexture (bdeTexture d) pool
    let (r, g, b, a) = decalTint now d
    pure BloodRenderRecord
        { brrDecal    = bdeId d
        , brrTexture  = bdeTexture d
        , brrPage     = bdePage d
        , brrX        = bdeX d
        , brrY        = bdeY d
        , brrSurfaceZ = bdeSurfaceZ d
        , brrOffsetX  = bdeOffsetX d
        , brrOffsetY  = bdeOffsetY d
        , brrRotation = bdeRotation d
        , brrScale    = bdeScale d
        , brrTintR    = r
        , brrTintG    = g
        , brrTintB    = b
        , brrAlpha    = a
        }

-- | Every renderable record on @page@, oldest decal first.
bloodRenderRecords ∷ Double → WorldPageId → BloodStore → [BloodRenderRecord]
bloodRenderRecords now page store =
    [ rec
    | d ← allDecals (bstDecals store)
    , bdePage d ≡ page
    , Just rec ← [bloodRenderRecord now (bstPool store) d]
    ]
