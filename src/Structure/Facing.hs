{-# LANGUAGE Strict #-}
-- | The ONE facing-dependent mapping from a wall's AUTHORED world edge
--   (and a post's authored world corner) to the SCREEN edge/corner it
--   occupies at the current camera facing (#1712).
--
--   A tile's iso diamond has four vertices — in grid space
--   N(gx,gy) E(gx+1,gy) S(gx+1,gy+1) W(gx,gy+1) — and four edges between
--   them. The wall sprites are named for the SCREEN positions those
--   edges occupy at 'FaceSouth' (@wall_ne@ = the upper-right edge,
--   @wall_sw@ = the lower-left, ...), which is why the naming is the
--   identity there and a permutation everywhere else: rotating the
--   camera 90° moves each physical edge onto the next screen position
--   while the piece's stored slot — its PHYSICAL edge — does not move.
--
--   Both tables below are derived from 'World.Grid.applyFacing', which
--   is what actually places a vertex on screen
--   (@sx = (a-b)·halfW, sy = (a+b)·halfH@ with
--   @(a,b) = applyFacing facing gx gy@). Worked example, 'FaceWest':
--   @applyFacing FaceWest (gx,gy) = (gy,-gx)@, so relative to the tile
--   centre the four vertices land at N=left, E=top, S=right, W=bottom.
--   The upper-LEFT screen edge is therefore E→N, i.e. the world NE edge
--   — hence @NE ↦ NW@ — and the vertex at screen-left is world N, hence
--   @N ↦ W@. The remaining facings are that same permutation applied
--   twice ('FaceNorth') and three times ('FaceEast').
--
--   Everything the structure renderer decides per wall — which sprite
--   and cap facemap to draw, whether the #415 strip path applies,
--   which sort anchor and tie-break to use, where a post stands — reads
--   these two tables and nothing else. See "Structure.Render" and
--   'World.Render.Quads.structureFrontWallClear'.
module Structure.Facing
    ( WallEdge(..)
    , PostCorner(..)
    , WallCaps(..)
    , wallEdgeOfSlot
    , postCornerOfSlot
    , screenWallEdge
    , screenPostCorner
    , authoredPostCorner
    , screenWireNeighbors
    , screenWireShape
    , isScreenFrontEdge
    , wallEdgeEnds
    , wallEdgeCanvasSpan
    , rotateWallCaps
    , wallCapsCode
    , wallCapsFromCode
    ) where

import UPrelude
import qualified Data.Text as T
import Engine.Graphics.Camera (CameraFacing(..))
import Structure.Types (StructureSlot(..))
import Structure.Wire
    (WireNeighbors(..), WireShape, wireShapeFor, wireShapeNeighbors)

-- | One of the tile diamond's four edges. As an AUTHORED value this
--   names a physical world edge; as a SCREEN value it names the screen
--   position an edge occupies (and therefore which of the pack's four
--   wall sprites draws it). 'screenWallEdge' is the one conversion.
data WallEdge = WallNE | WallNW | WallSE | WallSW
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | One of the tile diamond's four vertices, same authored/screen
--   duality as 'WallEdge'. 'screenPostCorner' is the one conversion.
data PostCorner = CornerN | CornerE | CornerS | CornerW
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | A wall's two-ended cap state: whether a corner post carves a pillar
--   notch out of each end. The ends are the sprite's CANVAS-left and
--   CANVAS-right ones (@scripts/structures.lua@'s @WALL_ENDS@), which is
--   why rotating a wall onto a screen edge whose endpoint order is
--   reversed must swap them — see 'rotateWallCaps'.
data WallCaps = WallCaps
    { wcLeft  ∷ !Bool   -- ^ canvas-left end capped
    , wcRight ∷ !Bool   -- ^ canvas-right end capped
    } deriving (Show, Eq, Ord)

-- | The wall slots' edges. 'Nothing' for every non-wall slot.
wallEdgeOfSlot ∷ StructureSlot → Maybe WallEdge
wallEdgeOfSlot s = case s of
    SWallNE → Just WallNE
    SWallNW → Just WallNW
    SWallSE → Just WallSE
    SWallSW → Just WallSW
    _       → Nothing

-- | The post slots' corners. 'Nothing' for every non-post slot.
postCornerOfSlot ∷ StructureSlot → Maybe PostCorner
postCornerOfSlot s = case s of
    SPostN → Just CornerN
    SPostE → Just CornerE
    SPostS → Just CornerS
    SPostW → Just CornerW
    _      → Nothing

-- | Authored world edge → the screen edge it occupies at @facing@.
--   'FaceSouth' is the identity; the other three are the same 4-cycle
--   @NE → NW → SW → SE → NE@ applied once, twice and three times.
screenWallEdge ∷ CameraFacing → WallEdge → WallEdge
screenWallEdge FaceSouth e = e
screenWallEdge FaceWest  e = case e of
    WallNE → WallNW ; WallNW → WallSW ; WallSW → WallSE ; WallSE → WallNE
screenWallEdge FaceNorth e = case e of
    WallNE → WallSW ; WallNW → WallSE ; WallSW → WallNE ; WallSE → WallNW
screenWallEdge FaceEast  e = case e of
    WallNE → WallSE ; WallNW → WallNE ; WallSW → WallNW ; WallSE → WallSW

-- | Authored world corner → the screen corner it occupies at @facing@.
--   The vertex permutation that INDUCES 'screenWallEdge': 'FaceSouth' is
--   the identity, the others the 4-cycle @N → W → S → E → N@.
screenPostCorner ∷ CameraFacing → PostCorner → PostCorner
screenPostCorner FaceSouth c = c
screenPostCorner FaceWest  c = case c of
    CornerN → CornerW ; CornerW → CornerS ; CornerS → CornerE ; CornerE → CornerN
screenPostCorner FaceNorth c = case c of
    CornerN → CornerS ; CornerW → CornerE ; CornerS → CornerN ; CornerE → CornerW
screenPostCorner FaceEast  c = case c of
    CornerN → CornerE ; CornerW → CornerN ; CornerS → CornerW ; CornerE → CornerS

-- | Inverse of 'screenPostCorner': which PHYSICAL corner of the tile is
--   drawn at the given screen position. Used wherever a screen-space
--   fact (the sprite's canvas-left end, the screen-bottom vertex a front
--   wall's strips reach their deepest sort key at) has to be turned back
--   into the grid coordinate that produces that depth.
authoredPostCorner ∷ CameraFacing → PostCorner → PostCorner
authoredPostCorner FaceSouth c = c
authoredPostCorner FaceWest  c = screenPostCorner FaceEast  c
authoredPostCorner FaceNorth c = screenPostCorner FaceNorth c
authoredPostCorner FaceEast  c = screenPostCorner FaceWest  c

-- | World wire connections → the connections as the SCREEN shows them
--   at @facing@ (#2901): a world neighbour on side @d@ is drawn on side
--   @'screenSide' d@. The same permutation as 'screenPostCorner' — the
--   cardinal neighbours N\/E\/S\/W sit on the tile's N\/E\/S\/W
--   vertices' sides, so 'FaceWest' is @N → W → S → E → N@ and the others
--   its square and cube — and for the same reason: it is what
--   'World.Grid.applyFacing' does to a neighbour offset.
--
--   A wire sprite is a fixed picture whose spokes run to fixed screen
--   positions, named for the world side they occupy at 'FaceSouth'.
--   Choosing the sprite from the SCREEN connections is what keeps two
--   neighbouring tiles' spokes meeting at every facing. 'FaceSouth' is
--   the identity. The STORED shape stays world-relative; this is a
--   draw-time mapping only.
screenWireNeighbors ∷ CameraFacing → WireNeighbors → WireNeighbors
screenWireNeighbors facing (WireNeighbors n e s w) =
    WireNeighbors
        { wnNorth = at CornerN, wnEast = at CornerE
        , wnSouth = at CornerS, wnWest = at CornerW }
  where
    -- The screen side @c@ is lit when the world side that lands on it
    -- is connected.
    at c = case authoredPostCorner facing c of
        CornerN → n
        CornerE → e
        CornerS → s
        CornerW → w

-- | The wire variant DRAWN for a world shape at @facing@: the sprite
--   whose screen connections are the facing-rotated world ones. The one
--   conversion every wire draw path uses — placed pieces, previews,
--   designation ghosts, construction frames and destruction clips — the
--   wire counterpart of 'screenWallEdge'. Identity at 'FaceSouth'.
--
--   The four risers (#2516) are not horizontal connections and are
--   returned unchanged. They are declaration-only today (nothing places
--   one); when risers are placed they need the same screen mapping —
--   their darker camera-facing pair is a screen fact — and this is the
--   function to extend.
screenWireShape ∷ CameraFacing → WireShape → WireShape
screenWireShape facing shape =
    maybe shape (wireShapeFor ∘ screenWireNeighbors facing)
                (wireShapeNeighbors shape)

-- | Is this a SCREEN-front edge — the pair the #415 depth-strip path and
--   the #418 billboard lift both act on? Takes a screen edge, never an
--   authored one; compose with 'screenWallEdge'.
isScreenFrontEdge ∷ WallEdge → Bool
isScreenFrontEdge e = e ≡ WallSE ∨ e ≡ WallSW

-- | A wall sprite's two end corners, ordered {canvas-left, canvas-right}
--   — the order its @"<left><right>"@ cap-facemap suffix is keyed by.
--   N/S sit at canvas centre (x48), E at x96 and W at x0, so the
--   lower-x vertex is the "left" end. Mirrors
--   @scripts/structures.lua@'s @WALL_ENDS@; the two must agree.
wallEdgeEnds ∷ WallEdge → (PostCorner, PostCorner)
wallEdgeEnds e = case e of
    WallNE → (CornerN, CornerE)
    WallNW → (CornerW, CornerN)
    WallSE → (CornerS, CornerE)
    WallSW → (CornerW, CornerS)

-- | The canvas-x range (as sprite UV-x) an edge's art actually spans.
--   Each edge runs over HALF the 96px canvas: the N and S vertices sit
--   at the centre (u 0.5), W at u 0 and E at u 1.
wallEdgeCanvasSpan ∷ WallEdge → (Float, Float)
wallEdgeCanvasSpan e = case e of
    WallNW → (0.0, 0.5)
    WallSW → (0.0, 0.5)
    WallNE → (0.5, 1.0)
    WallSE → (0.5, 1.0)

-- | Re-encode a wall's cap suffix for the sprite it is drawn with at
--   @facing@. The caps record which PHYSICAL end carries a post, so the
--   physical assignment is preserved and only the canvas ORDER is
--   re-derived: the rendered sprite's canvas-left end shows whichever
--   physical corner is drawn at that screen position, and likewise for
--   its canvas-right end. Identity at 'FaceSouth'; a genuine swap
--   wherever the target screen edge reverses the endpoint order (e.g. a
--   world NW wall drawn as the SW sprite at 'FaceWest').
rotateWallCaps ∷ CameraFacing → WallEdge → WallCaps → WallCaps
rotateWallCaps facing authored caps =
    let (authL, authR) = wallEdgeEnds authored
        (scrL, scrR)   = wallEdgeEnds (screenWallEdge facing authored)
        -- Which physical corner is drawn at each end of the sprite.
        physOf         = authoredPostCorner facing
        capAt c
            | c ≡ authL = wcLeft caps
            | c ≡ authR = wcRight caps
            -- Unreachable: the screen edge is this edge's own image
            -- under the vertex permutation, so both of its endpoints map
            -- back to {authL, authR}. Pinned by the headless spec.
            | otherwise = False
    in WallCaps (capAt (physOf scrL)) (capAt (physOf scrR))

-- | The @"<left><right>"@ cap-facemap suffix a pack keys its four wall
--   facemap variants by.
wallCapsCode ∷ WallCaps → Text
wallCapsCode (WallCaps l r) = T.pack [bit l, bit r]
  where bit b = if b then '1' else '0'

wallCapsFromCode ∷ Text → Maybe WallCaps
wallCapsFromCode t = case T.unpack t of
    [l, r] → WallCaps <$> bit l <*> bit r
    _      → Nothing
  where
    bit '0' = Just False
    bit '1' = Just True
    bit _   = Nothing

