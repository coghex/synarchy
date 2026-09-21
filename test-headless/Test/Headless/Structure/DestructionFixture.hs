{-# LANGUAGE OverloadedStrings #-}
-- | The fixture pack #2491's teardown suite resolves against: the
--   construction suite's two packs, with ASYMMETRIC destruction clips
--   declared over exactly the same static art.
--
--   Sharing the static half is the point. The clips are keyed by the
--   appearance's own sprite path, the wall rotation is resolved through
--   "Test.Headless.Structure.ConstructionFixture"'s real
--   'fixtureWallCatalog', and a placed piece in an example is placed
--   with those same paths — so "which appearance did this effect come
--   from" is answered by the production lookup rather than by a
--   fixture-only shortcut.
--
--   Asymmetric in four senses, each of which an example depends on:
--
--     * every appearance's frames are its OWN paths, so "which clip did
--       this quad come from" is answerable from the handle alone;
--     * the clips run to DIFFERENT lengths across kinds (3, 5, 4, 6, 7,
--       2) and at DIFFERENT rates (12, 8, 10, 15, 5, 6 fps), so a frame
--       index or an expiry derived from the wrong appearance lands
--       somewhere visibly wrong rather than coincidentally agreeing;
--     * some appearances declare a clip and some do not — the default
--       CEILING declares none — so requirement 6's "captures nothing and
--       reports once" is a real state and not an empty catalogue;
--     * the @damaged@ variant declares a FLOOR clip of its own, so a
--       variant resolving the default's art is a visible failure.
--
--   The four DEFAULT wall directions deliberately run to the SAME
--   length at the SAME rate while carrying different paths. That is the
--   registration rule requirement 5 rests on: a camera turn changes
--   which direction's sprite is drawn, so only equal clips make one
--   elapsed time select the same stage — and expire at the same instant
--   — at all four facings.
module Test.Headless.Structure.DestructionFixture
    ( -- * The declarations
      wreckFrameCount
    , wreckFps
    , wreckFramePathsFor
    , wreckSequenceFor
    , wreckAppearances
      -- * The registrations
    , wreckVariantArt
    , wreckOverriddenNoClip
    , wreckRegistration
    , wreckWireRegistration
    , wreckCatalog
      -- * Building variations
    , withDestruction
    , wreckTexSizes
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Engine.Asset.Handle (TextureHandle)

import Structure.ArtCatalog
import Structure.Facing (WallEdge(..))
import Structure.Wire (WireShape(..), wireShapeName)

import Test.Headless.Structure.ConstructionFixture
    ( allAppearances, artAsset, damagedVariant, fixtureRegistration
    , fixtureTexSizes, fixtureWireRegistration, registerOrFail
    , staticPathFor )
import Test.Headless.Render.StructureGhostFixture (handleForPath)

-- | How many frames each appearance's teardown clip declares. 'Nothing'
--   is an appearance with no declaration at all — requirement 6's state.
wreckFrameCount ∷ AppearanceKey → Maybe Int
wreckFrameCount (AppearanceKey variant slot) = case (variant, slot) of
    (Nothing, ApFloor)         → Just 3
    (Nothing, ApPost)          → Just 5
    (Nothing, ApWall _)        → Just 4
    (Nothing, ApWire WireCross)      → Just 6
    (Nothing, ApWire WireStraightNS) → Just 7
    (Just v,  ApFloor) | v ≡ damagedVariant → Just 2
    _ → Nothing

-- | The rate each declared clip plays at. Distinct per kind so a
--   duration or a frame index taken from the wrong appearance is
--   visible rather than plausible; equal across a wall family, which is
--   what registration requires.
wreckFps ∷ AppearanceKey → Double
wreckFps (AppearanceKey variant slot) = case (variant, slot) of
    (Nothing, ApFloor)         → 12
    (Nothing, ApPost)          → 8
    (Nothing, ApWall _)        → 10
    (Nothing, ApWire WireCross)      → 15
    (Nothing, ApWire WireStraightNS) → 5
    (Just v,  ApFloor) | v ≡ damagedVariant → 6
    _ → 1

-- | The ordered frame paths of an appearance that declares a clip.
--   Deliberately a different suffix from the construction suite's
--   @_build_@, so no example can pass by resolving the wrong lifecycle.
wreckFramePathsFor ∷ AppearanceKey → [Text]
wreckFramePathsFor ak = case wreckFrameCount ak of
    Nothing → []
    Just n  → [ prefix <> "_break_" <> tshow i <> ".png" | i ← [0 .. n - 1] ]
  where
    AppearanceKey variant slot = ak
    prefix = maybe "" (\v → v <> "/") variant <> slotPath slot

slotPath ∷ AppearanceSlot → Text
slotPath s = case s of
    ApFloor   → "fx/floor"
    ApCeiling → "fx/ceiling"
    ApPost    → "fx/post"
    ApWall e  → "fx/wall_" <> edgeCode e
    ApWire w  → "fx/wire_" <> wireShapeName w

edgeCode ∷ WallEdge → Text
edgeCode e = case e of
    WallNE → "ne"; WallNW → "nw"; WallSE → "se"; WallSW → "sw"

wreckSequenceFor ∷ AppearanceKey → Maybe DestructionSequence
wreckSequenceFor ak = case wreckFramePathsFor ak of
    []     → Nothing
    frames → Just DestructionSequence
        { dsStatic = artAsset (staticPathFor ak)
        , dsFrames = V.fromList (map artAsset frames)
        , dsFps    = wreckFps ak }

-- | Every appearance this fixture declares a clip for, in a fixed order.
wreckAppearances ∷ [AppearanceKey]
wreckAppearances = [ ak | ak ← allAppearances, isJust (wreckFrameCount ak) ]

declaredWrecks ∷ [PieceKind] → [(AppearanceKey, DestructionSequence)]
declaredWrecks kinds =
    [ (ak, ds)
    | ak ← allAppearances
    , appearanceSlotKind (apSlot ak) `elem` kinds
    , Just ds ← [wreckSequenceFor ak] ]

-- * The registrations

-- | The @damaged@ variant's AUTHORED static appearances, as the
--   production loader sends them.
--
--   Three deliberately different states, because #2491's appearance
--   index has to answer for all three:
--
--     * @floor@ — overridden AND declaring its own teardown clip;
--     * @wall ne@ — overridden, declaring a CONSTRUCTION sequence and no
--       teardown one;
--     * @post@ — overridden and declaring NO lifecycle frames at all,
--       which is the state a variant is invisible in unless its static
--       art is registered ('wreckOverriddenNoClip').
--
--   The INHERITED case — a variant sharing the default's sprite — is
--   deliberately NOT here: it makes that sprite ambiguous for both
--   claimants, so it is built per-example rather than poisoning the
--   shared fixture.
wreckVariantArt ∷ [(AppearanceKey, ArtAsset)]
wreckVariantArt =
    [ (ak, artAsset (staticPathFor ak))
    | slot ← [ApFloor, ApPost, ApWall WallNE]
    , let ak = AppearanceKey (Just damagedVariant) slot ]

-- | The variant appearance that is overridden but declares no lifecycle
--   frames of any kind — requirement 6's report is owed for it, and was
--   unreachable before its static art was registered.
wreckOverriddenNoClip ∷ AppearanceKey
wreckOverriddenNoClip = AppearanceKey (Just damagedVariant) ApPost

-- | The piece pack's construction declaration, unchanged, plus its
--   teardown clips and its variant inventory.
wreckRegistration ∷ PackArtRegistration
wreckRegistration = fixtureRegistration
    { parDestruction = declaredWrecks [KFloor, KCeiling, KPost, KWall]
    , parVariants    = wreckVariantArt }

wreckWireRegistration ∷ PackArtRegistration
wreckWireRegistration = fixtureWireRegistration
    { parDestruction = declaredWrecks [KWire] }

-- | Both packs, registered through the production entry point — so a
--   fixture that violated the wall-family completeness rule fails here
--   rather than in an example.
wreckCatalog ∷ StructureArtCatalog
wreckCatalog =
    registerOrFail wreckWireRegistration
        (registerOrFail wreckRegistration emptyStructureArtCatalog)

-- | Pixel sizes for every image the teardown suite measures a quad
--   with: the construction fixture's, plus this fixture's own frames.
--
--   Keyed by HANDLE, and deliberately NOT the base tile size — a
--   producer that silently fell back to @baseTileW@\/@baseTileH@ would
--   emit a differently sized quad, so "an effect's frame is measured
--   exactly as the piece's sprite is" has something to fail on.
wreckTexSizes ∷ HM.HashMap TextureHandle (Int, Int)
wreckTexSizes = HM.union fixtureTexSizes $ HM.fromList
    [ (handleForPath path, fixtureFrameSize)
    | ak ← wreckAppearances, path ← wreckFramePathsFor ak ]

-- | The size 'fixtureTexSizes' reports for every construction image,
--   restated here so a teardown frame and the sprite it replaces are
--   measured identically. A difference in an example is then a real
--   defect rather than fixture noise.
fixtureFrameSize ∷ (Int, Int)
fixtureFrameSize = (192, 128)

-- | The same registration with ONE appearance's teardown clip replaced
--   — the mutation handle every refusal example turns.
withDestruction ∷ AppearanceKey → Maybe DestructionSequence
                → PackArtRegistration → PackArtRegistration
withDestruction ak mds reg = reg
    { parDestruction = [ e | e@(k, _) ← parDestruction reg, k ≢ ak ]
                         ⧺ maybe [] (\ds → [(ak, ds)]) mds }
