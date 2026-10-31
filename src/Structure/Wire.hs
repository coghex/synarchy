{-# LANGUAGE Strict #-}
-- | The wire autotile RULE (#359, exposed engine-side by #1842): which
--   of the "wire" pack's sixteen connection variants a tile draws, given
--   which of its four cardinal neighbours also carry wire.
--
--   Until #1842 this lived only in @scripts/wire.lua@'s @shapeFor@, which
--   is fine while the only consumer is the placer — but the render pass
--   has to answer the same question for a wire that is merely
--   DESIGNATED, and the world render thread cannot call into Lua. Rather
--   than grow a second implementation that would drift, the rule moves
--   here and @scripts/wire.lua@ calls it through @structure.wireShape@:
--   one function, one vocabulary, one set of sixteen names.
--
--   The names are the pack YAML's own connection keys
--   (@data/structure_packs/wire.yaml@), so 'wireShapeName' is what
--   indexes the art and must not be "tidied": a tee is named by its
--   MISSING side (@tee_n@ = connected E+S+W, the open side facing N),
--   which reads backwards until you notice it names the gap, and the
--   corner/tee vocabulary is the wall edges' (@ne@\/@nw@\/@se@\/@sw@).
module Structure.Wire
    ( WireShape(..)
    , WireNeighbors(..)
    , wireShapeFor
    , wireShapeName
    , wireShapeFromName
    , allWireShapes
    , allWireRisers
    , allWireArtShapes
    , isWireRiser
    , allWireNeighbors
    , wireShapeNeighbors
    ) where

import UPrelude

-- | One of the sixteen connection variants. Enumerated (not a
--   'Text') so a resolution cannot invent a name the pack has no art
--   for, and 'Bounded'\/'Enum' so a test can sweep the whole space.
data WireShape
    = WireIsolated
    | WireEndN | WireEndE | WireEndS | WireEndW
    | WireStraightNS | WireStraightEW
    | WireCornerNE | WireCornerNW | WireCornerSE | WireCornerSW
    | WireTeeN | WireTeeE | WireTeeS | WireTeeW
    | WireCross
      -- | The four RISERS (#2516): a cable climbing one tile side's wall
      --   face to the level above, named after that face. They are ART
      --   appearances only. 'wireShapeFor' never returns one, so the
      --   autotile never selects them; they are APPENDED so the sixteen
      --   autotile shapes keep their positions.
    | WireRiserN | WireRiserE | WireRiserS | WireRiserW
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Which cardinal neighbours of a tile carry wire, in the engine's own
--   slope-bit order (@World.Slope@: N = gy-1, E = gx+1, S = gy+1,
--   W = gx-1) — the order @scripts/wire.lua@'s @NEIGHBOR_OFFSETS@ walks.
data WireNeighbors = WireNeighbors
    { wnNorth ∷ !Bool
    , wnEast  ∷ !Bool
    , wnSouth ∷ !Bool
    , wnWest  ∷ !Bool
    } deriving (Show, Eq, Ord)

-- | The shape a tile with these neighbours draws. A transliteration of
--   @scripts/wire.lua@'s @shapeFor@, branch for branch, including its
--   count-first structure: the two-neighbour case is the only one where
--   WHICH pair matters, and the straights must be tested before the
--   corners.
wireShapeFor ∷ WireNeighbors → WireShape
wireShapeFor (WireNeighbors n e s w)
    | count ≡ 0 = WireIsolated
    | count ≡ 4 = WireCross
    | count ≡ 1 = if n then WireEndN
             else if e then WireEndE
             else if s then WireEndS
             else           WireEndW
    | count ≡ 3 = if not n then WireTeeN
             else if not e then WireTeeE
             else if not s then WireTeeS
             else               WireTeeW
    | n ∧ s     = WireStraightNS
    | e ∧ w     = WireStraightEW
    | n ∧ e     = WireCornerNE
    | n ∧ w     = WireCornerNW
    | s ∧ e     = WireCornerSE
    | otherwise = WireCornerSW
  where
    count = length (filter id [n, e, s, w]) ∷ Int

-- | The pack YAML's connection key for a shape. The ONE spelling — the
--   catalogue is keyed by it and the art is looked up by it.
wireShapeName ∷ WireShape → Text
wireShapeName s = case s of
    WireIsolated    → "isolated"
    WireEndN        → "end_n"
    WireEndE        → "end_e"
    WireEndS        → "end_s"
    WireEndW        → "end_w"
    WireStraightNS  → "straight_ns"
    WireStraightEW  → "straight_ew"
    WireCornerNE    → "corner_ne"
    WireCornerNW    → "corner_nw"
    WireCornerSE    → "corner_se"
    WireCornerSW    → "corner_sw"
    WireTeeN        → "tee_n"
    WireTeeE        → "tee_e"
    WireTeeS        → "tee_s"
    WireTeeW        → "tee_w"
    WireCross       → "cross"
    WireRiserN      → "riser_n"
    WireRiserE      → "riser_e"
    WireRiserS      → "riser_s"
    WireRiserW      → "riser_w"

-- | Parse a connection key. Exact, case-sensitively — the pack YAML's
--   keys are data, and a lenient parse would silently accept art the
--   placer would never ask for.
wireShapeFromName ∷ Text → Maybe WireShape
wireShapeFromName t =
    lookup t [ (wireShapeName s, s) | s ← allWireArtShapes ]

-- | The sixteen shapes the AUTOTILE can select — what 'wireShapeFor' can
--   return and what a four-neighbour sweep covers.
allWireShapes ∷ [WireShape]
allWireShapes = [minBound .. WireCross]

-- | The four riser appearances (#2516): art and declaration only.
allWireRisers ∷ [WireShape]
allWireRisers = [WireRiserN .. WireRiserW]

-- | Every appearance the pack must declare art for: the sixteen
--   autotile shapes and the four risers, each required with no fallback.
allWireArtShapes ∷ [WireShape]
allWireArtShapes = allWireShapes ⧺ allWireRisers

isWireRiser ∷ WireShape → Bool
isWireRiser = (≥ WireRiserN)

-- | The full sixteen-element neighbour space, so a sweep cannot miss a
--   combination.
allWireNeighbors ∷ [WireNeighbors]
allWireNeighbors =
    [ WireNeighbors n e s w
    | n ← [False, True], e ← [False, True]
    , s ← [False, True], w ← [False, True] ]

-- | The WORLD connections a shape stands for — the inverse of
--   'wireShapeFor' over the sixteen autotile shapes. 'Nothing' for the
--   four risers: a riser is not a horizontal connection, so it has no
--   neighbour set.
--
--   Derived by search over 'allWireNeighbors' rather than restated, so
--   the rule has ONE spelling and this cannot drift from it
--   ('wireShapeFor' is a bijection on the sixteen sets).
wireShapeNeighbors ∷ WireShape → Maybe WireNeighbors
wireShapeNeighbors s =
    case [ n | n ← allWireNeighbors, wireShapeFor n ≡ s ] of
        (n : _) → Just n
        []      → Nothing
