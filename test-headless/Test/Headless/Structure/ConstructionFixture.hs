{-# LANGUAGE OverloadedStrings #-}
-- | The fixture pack the construction-frame suite (#2488) resolves
--   against: a two-pack registration whose appearances declare
--   ASYMMETRIC construction sequences.
--
--   Asymmetric in three senses, each of which an example depends on:
--
--     * every appearance's frames are its OWN paths, so "which sequence
--       did this quad come from" is answerable from the handle alone;
--     * the sequences run to DIFFERENT lengths across kinds (3, 5, 2, 4,
--       6), so a frame index derived from the wrong appearance's count
--       lands on the wrong frame rather than coincidentally agreeing;
--     * within one pack, some appearances declare a sequence and some do
--       not — including one direction of the @damaged@ variant's wall
--       family — so requirement 8's "resolves none" is a real state and
--       not an empty catalogue.
--
--   The four DEFAULT wall directions deliberately run to the SAME length
--   while carrying different paths. That is the registration rule
--   requirement 5 rests on: a camera turn changes which direction's
--   sprite is drawn, so only equal lengths make the same progress select
--   the same stage at all four facings.
module Test.Headless.Structure.ConstructionFixture
    ( -- * Names
      fixturePack
    , fixtureWirePack
    , damagedVariant
      -- * The registration
    , fixtureRegistration
    , fixtureWireRegistration
    , fixtureCatalog
    , fixtureWallCatalog
    , fixtureTexSizes
      -- * Vocabulary
    , allWallEdges
    , allCaps
    , framedWireShapes
    , frameCount
    , framePathsFor
    , staticPathFor
      -- * Building variations
    , registerOrFail
    , withSequence
    , sequenceFor
    , canvas
    , asset
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Engine.Asset.Handle (TextureHandle)
import Structure.ArtCatalog
import Structure.Facing (WallCaps(..), WallEdge(..), wallCapsCode)
import Structure.WallCatalog
    ( StructureWallCatalog, WallArtEntry(..), emptyStructureWallCatalog
    , registerWallFamily )
import Structure.Wire (WireShape(..), allWireShapes, wireShapeName)

import Test.Headless.Render.StructureGhostFixture (handleForPath)

fixturePack, fixtureWirePack, damagedVariant ∷ Text
fixturePack     = "fixture_dungeon"
fixtureWirePack = "fixture_wire"
damagedVariant  = "damaged"

allWallEdges ∷ [WallEdge]
allWallEdges = [WallNE, WallNW, WallSE, WallSW]

allCaps ∷ [WallCaps]
allCaps = [ WallCaps l r | l ← [False, True], r ← [False, True] ]

-- | The two wire connections this pack builds through. Every other shape
--   has static art and no sequence, which is what makes "a wire run
--   never borrows another shape's frames" assertable.
framedWireShapes ∷ [WireShape]
framedWireShapes = [WireCross, WireStraightNS]

-- | The canvas every fixture image reports. One size for the whole pack,
--   because requirement 6's check is that a sequence's LAST frame
--   matches its own static sprite — a fixture with a second size would
--   pass that check by accident.
canvas ∷ (Int, Int)
canvas = (96, 64)

-- | How many frames each appearance declares. 'Nothing' is an appearance
--   with no declaration at all — requirement 8's state.
frameCount ∷ AppearanceKey → Maybe Int
frameCount (AppearanceKey variant slot) = case (variant, slot) of
    (Nothing, ApFloor)   → Just 3
    (Nothing, ApCeiling) → Just 5
    (Nothing, ApPost)    → Just 2
    (Nothing, ApWall _)  → Just 4
    (Nothing, ApWire w)
        | w `elem` framedWireShapes → Just 6
    (Just v, ApFloor)    | v ≡ damagedVariant → Just 2
    (Just v, ApWall WallNE) | v ≡ damagedVariant → Just 3
    _ → Nothing

-- | The STATIC sprite an appearance is built toward. A variant's own
--   override where it has one; the default's path otherwise.
staticPathFor ∷ AppearanceKey → Text
staticPathFor (AppearanceKey variant slot) =
    maybe "" (\v → v <> "/") variant <> slotPath slot <> ".png"

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

-- | The ordered frame paths of an appearance that declares a sequence.
framePathsFor ∷ AppearanceKey → [Text]
framePathsFor ak = case frameCount ak of
    Nothing → []
    Just n  → [ prefix <> "_build_" <> tshow i <> ".png" | i ← [0 .. n - 1] ]
  where
    AppearanceKey variant slot = ak
    prefix = maybe "" (\v → v <> "/") variant <> slotPath slot

sequenceFor ∷ AppearanceKey → Maybe ConstructionSequence
sequenceFor ak = case framePathsFor ak of
    []     → Nothing
    frames → Just ConstructionSequence
        { csStatic = artAsset (staticPathFor ak)
        , csFrames = V.fromList (map artAsset frames) }

artAsset ∷ Text → ArtAsset
artAsset p = ArtAsset p (handleForPath p)

asset ∷ Text → Text → PieceArt
asset tex face = PieceArt (artAsset tex) (artAsset face)

-- | Every appearance this fixture could declare, in a fixed order.
allAppearances ∷ [AppearanceKey]
allAppearances =
    [ AppearanceKey v s
    | v ← [Nothing, Just damagedVariant]
    , s ← [ApFloor, ApCeiling, ApPost]
            ⧺ map ApWall allWallEdges
            ⧺ map ApWire allWireShapes ]

-- * The registrations

-- | The piece pack: floor, ceiling, post and four wall edges, each with
--   its own facemap so an example can name which appearance a quad drew.
fixtureRegistration ∷ PackArtRegistration
fixtureRegistration = PackArtRegistration
    { parPack   = fixturePack
    , parKinds  = [ (k, True, Just (mkBuildCost 3.0 [("steel_plate", 1)]))
                  | k ← [KFloor, KCeiling, KPost, KWall] ]
    , parEntries =
        [ (AkFloor,   asset "fx/floor.png"   "fx/floorface.png")
        , (AkCeiling, asset "fx/ceiling.png" "fx/ceilingface.png")
        , (AkPost,    asset "fx/post.png"    "fx/postface.png")
        ]
        ⧺ [ ( AkWall e c
            , asset ("fx/wall_" <> edgeCode e <> ".png")
                    ("fx/wallface_" <> edgeCode e <> "_" <> wallCapsCode c
                       <> ".png") )
          | e ← allWallEdges, c ← allCaps ]
    , parFrames = declaredSequences [KFloor, KCeiling, KPost, KWall]
    , parSizes  = fixtureSizes
    }

-- | The wire pack: sixteen connection variants sharing one facemap,
--   exactly as @data\/structure_packs\/wire.yaml@ is shaped.
fixtureWireRegistration ∷ PackArtRegistration
fixtureWireRegistration = PackArtRegistration
    { parPack    = fixtureWirePack
    , parKinds   = [(KWire, True, Just (mkBuildCost 1.5 [("wiring", 1)]))]
    , parEntries = [ ( AkWire w
                     , asset ("fx/wire_" <> wireShapeName w <> ".png")
                             "fx/floorface.png" )
                   | w ← allWireShapes ]
    , parFrames  = declaredSequences [KWire]
    , parSizes   = fixtureSizes
    }

declaredSequences ∷ [PieceKind] → [(AppearanceKey, ConstructionSequence)]
declaredSequences kinds =
    [ (ak, cs)
    | ak ← allAppearances
    , appearanceSlotKind (apSlot ak) `elem` kinds
    , Just cs ← [sequenceFor ak] ]

-- | Every path any declared sequence measures, all at the one canvas.
fixtureSizes ∷ HM.HashMap Text (Int, Int)
fixtureSizes = HM.fromList
    [ (path, canvas)
    | ak ← allAppearances
    , path ← staticPathFor ak : framePathsFor ak ]

-- | Both packs, registered through the production entry point — so a
--   fixture that violated the all-or-nothing rule fails here rather than
--   in an example.
fixtureCatalog ∷ StructureArtCatalog
fixtureCatalog =
    registerOrFail fixtureWireRegistration
        (registerOrFail fixtureRegistration emptyStructureArtCatalog)

registerOrFail ∷ PackArtRegistration → StructureArtCatalog
               → StructureArtCatalog
registerOrFail reg cat = case registerPackArt reg cat of
    (cat', ArtRegistered)            → cat'
    (_,    ArtAlreadyRegistered)     → cat
    (_,    ArtRegistrationRefused f) →
        error ("fixture pack refused: " ⧺ show (artFaultMessage f))

-- | The same registration with ONE appearance's sequence replaced —
--   the mutation handle every refusal example turns.
withSequence ∷ AppearanceKey → Maybe ConstructionSequence
             → PackArtRegistration → PackArtRegistration
withSequence ak mcs reg = reg
    { parFrames = [ e | e@(k, _) ← parFrames reg, k ≢ ak ]
                    ⧺ maybe [] (\cs → [(ak, cs)]) mcs
    , parSizes  = maybe (parSizes reg) sizesFor mcs `HM.union` parSizes reg }
  where
    sizesFor cs = HM.fromList
        [ (aaPath a, canvas)
        | a ← csStatic cs : V.toList (csFrames cs) ]

-- | The wall family the same fixture declares, so a construction frame
--   rotates through #1712's real catalogue rather than a stand-in.
fixtureWallCatalog ∷ StructureWallCatalog
fixtureWallCatalog = fromMaybe (error "fixture wall family refused") $
    registerWallFamily entries emptyStructureWallCatalog
  where
    entries =
        [ WallArtEntry e Nothing (texOf e) (handleForPath (texOf e)) True
        | e ← allWallEdges ]
        ⧺ [ WallArtEntry e (Just c) (faceOf e c)
                         (handleForPath (faceOf e c)) True
          | e ← allWallEdges, c ← allCaps ]
    texOf e    = "fx/wall_" <> edgeCode e <> ".png"
    faceOf e c = "fx/wallface_" <> edgeCode e <> "_" <> wallCapsCode c
                   <> ".png"

-- | Pixel sizes for every fixture image, keyed by HANDLE — the map the
--   render pass measures a quad with.
--
--   Deliberately NOT the base tile size: a producer that silently fell
--   back to @baseTileW@\/@baseTileH@ would then emit a differently sized
--   quad, so "the frame is measured exactly as the static sprite is" has
--   something to fail on.
fixtureTexSizes ∷ HM.HashMap TextureHandle (Int, Int)
fixtureTexSizes = HM.fromList
    [ (handleForPath path, (192, 128))
    | path ← M.keys allPaths ]
  where
    allPaths = M.fromList
        [ (p, ())
        | ak ← allAppearances
        , p ← staticPathFor ak : framePathsFor ak ]
        `M.union` M.fromList
        [ (p, ())
        | (_, art) ← parEntries fixtureRegistration
                       ⧺ parEntries fixtureWireRegistration
        , p ← [aaPath (paTexture art), aaPath (paFacemap art)] ]
