-- | The authored-fixture schema of the controlled hydraulic harness
--   (#2719, RVR-01).
--
--   A fixture is GEOMETRY plus a SCHEDULE, written in its own local tile
--   frame and independent of any solver. It names which 16×16 chunks
--   exist and whether each is resident-active or resident-inactive, the
--   exact terrain and fluid of every cell, the impermeable barriers, and
--   the timed operations (terrain edits, sources and sinks) the harness
--   applies between steps.
--
--   Three quantities are kept apart on purpose, because confusing them
--   is how a rounding or a double count slips into a comparison:
--
--   * 'Elevation' — a cell's terrain top on the exact plane, in eighths
--     of a z-level ('World.Fluid.Exact.fluidUnitsPerZ').
--   * 'Quantity' — the fluid units standing over that cell's own
--     terrain top. This is what conservation counts.
--   * 'Surface' — the absolute fluid surface, @elevation + quantity@,
--     derived and never authored. This is what profile comparisons read.
--
--   Whether a particular solver can represent a fixture is the solver
--   adapter's question, not this module's: the legacy adapter rejects
--   fractional terrain, for instance, while the schema itself allows it.
module RiverRuntime.Harness.Fixture
    ( -- * Units
      Elevation(..)
    , Quantity(..)
    , Surface(..)
    , surfaceOf
    , LogicalTime(..)
      -- * Frame
    , Tile(..)
    , LocalChunk(..)
    , tileChunk
    , tileIndex
    , chunkTiles
    , cardinalTiles
      -- * Schema
    , Residency(..)
    , FluidSpec(..)
    , CellSpec(..)
    , Barrier(..)
    , FluidPolicy(..)
    , Operation(..)
    , Scheduled(..)
    , Fixture(..)
    , fixtureTiles
    , initialCell
    , isBarrierTile
      -- * Validation
    , validateFixture
    ) where

import UPrelude
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import World.Chunk.Types (chunkSize)
import World.Fluid.Types (FluidType)

-- | Terrain top on the exact plane, in eighths of a z-level.
newtype Elevation = Elevation { unElevation ∷ Int }
    deriving (Show, Eq, Ord)

-- | Fluid units standing over a cell's own terrain top.
newtype Quantity = Quantity { unQuantity ∷ Int }
    deriving (Show, Eq, Ord)

-- | Absolute fluid surface on the exact plane, in eighths of a z-level.
newtype Surface = Surface { unSurface ∷ Int }
    deriving (Show, Eq, Ord)

-- | The absolute surface a quantity stands at over a terrain top.
surfaceOf ∷ Elevation → Quantity → Surface
surfaceOf (Elevation e) (Quantity q) = Surface (e + q)

-- | Logical time in microseconds. It labels steps; it is never read
--   from a clock, so a run's outcome cannot depend on how fast it ran.
newtype LogicalTime = LogicalTime { unLogicalTime ∷ Int }
    deriving (Show, Eq, Ord)

-- | A tile in the fixture's own frame. Placement onto a page (and any
--   seam wrapping that implies) happens only at the adapter boundary.
data Tile = Tile { tileX ∷ Int, tileY ∷ Int }
    deriving (Show, Eq, Ord)

-- | A chunk in the fixture's own frame.
data LocalChunk = LocalChunk Int Int
    deriving (Show, Eq, Ord)

tileChunk ∷ Tile → LocalChunk
tileChunk (Tile x y) = LocalChunk (x `div` chunkSize) (y `div` chunkSize)

-- | A tile's index inside its chunk: the row-major order every chunk
--   vector in the engine uses.
tileIndex ∷ Tile → Int
tileIndex (Tile x y) = (y `mod` chunkSize) * chunkSize + x `mod` chunkSize

-- | Every tile of a chunk, in 'tileIndex' order.
chunkTiles ∷ LocalChunk → [Tile]
chunkTiles (LocalChunk cx cy) =
    [ Tile (cx * chunkSize + lx) (cy * chunkSize + ly)
    | ly ← [0 .. chunkSize - 1], lx ← [0 .. chunkSize - 1] ]

-- | The four face neighbours of a tile, in a fixed order.
cardinalTiles ∷ Tile → [Tile]
cardinalTiles (Tile x y) =
    [Tile x (y - 1), Tile (x + 1) y, Tile x (y + 1), Tile (x - 1) y]

-- | Whether a declared chunk takes part in simulation from the start.
--   An absent chunk is simply not declared.
data Residency = ResidentActive | ResidentInactive
    deriving (Show, Eq)

data FluidSpec = FluidSpec
    { fsType     ∷ FluidType
    , fsQuantity ∷ Quantity  -- ^ strictly positive; a dry cell has no spec
    } deriving (Show, Eq)

data CellSpec = CellSpec
    { csTerrain ∷ Elevation
    , csFluid   ∷ Maybe FluidSpec
    } deriving (Show, Eq)

-- | An impermeable barrier: a set of cells that is CLOSED wherever its
--   scheduled terrain stands at or above the crest. A closed barrier
--   cell must hold no fluid, and no quantity may cross it: the harness
--   checks both against this geometry, never against a solver's report
--   of what it did. Raising a cell back to the crest closes it again.
data Barrier = Barrier
    { barrierName  ∷ Text
    , barrierCells ∷ [Tile]
    , barrierCrest ∷ Elevation
    } deriving (Show, Eq)

-- | What a terrain edit does to the fluid already standing in the cell.
data FluidPolicy
    = KeepQuantity
      -- ^ The quantity is preserved; its surface moves with the terrain.
    | DisplaceToSink
      -- ^ Whatever the cell holds when the edit applies is removed and
      --   accounted as a declared sink of exactly that amount.
    deriving (Show, Eq)

-- | One fixture operation. These are the harness's OWN semantics,
--   independent of production edit paths: no production edit
--   (which can lose or invent quantity, Appendix A of
--   @docs/designs/river_runtime_design.md@) is reused here.
data Operation
    = SetTerrain Tile Elevation FluidPolicy
      -- ^ Set a cell's terrain top.
    | AddFluid Tile FluidType Quantity
      -- ^ A declared source: add this many units of this fluid.
    | RemoveFluid Tile Quantity
      -- ^ A declared sink: remove exactly this many units.
    deriving (Show, Eq)

-- | An operation due at a logical time. Operations due at the same time
--   apply in declaration order, before the solver advances from it.
data Scheduled = Scheduled
    { schedAt ∷ LogicalTime
    , schedOp ∷ Operation
    } deriving (Show, Eq)

data Fixture = Fixture
    { fxName        ∷ Text
    , fxDescription ∷ Text
    , fxChunks      ∷ [(LocalChunk, Residency)]
    , fxBaseTerrain ∷ Elevation
      -- ^ terrain of every cell 'fxCells' does not name
    , fxCells       ∷ M.Map Tile CellSpec
    , fxBarriers    ∷ [Barrier]
    , fxSchedule    ∷ [Scheduled]
    , fxDuration    ∷ LogicalTime
      -- ^ the logical span a run covers; a run's step count is this
      --   divided by its step interval
    } deriving (Show, Eq)

-- | Every tile of every declared chunk, in ascending order.
fixtureTiles ∷ Fixture → [Tile]
fixtureTiles fx = S.toAscList (S.fromList (concatMap (chunkTiles . fst) (fxChunks fx)))

-- | A tile's declared initial state.
initialCell ∷ Fixture → Tile → CellSpec
initialCell fx tile =
    fromMaybe (CellSpec (fxBaseTerrain fx) Nothing) (M.lookup tile (fxCells fx))

isBarrierTile ∷ Fixture → Tile → Bool
isBarrierTile fx tile = any (elem tile . barrierCells) (fxBarriers fx)

-- | Every authoring error in a fixture, in a stable order. An empty
--   list is a valid fixture.
validateFixture ∷ Fixture → [Text]
validateFixture fx = concat
    [ [ "no chunks declared" | null (fxChunks fx) ]
    , [ "chunk declared twice: " <> tshow c
      | (c : _ : _) ← L.group (L.sort (map fst (fxChunks fx))) ]
    , [ "cell outside every declared chunk: " <> tshow t
      | t ← M.keys (fxCells fx), not (declared t) ]
    , [ "wet cell with a non-positive quantity: " <> tshow t
      | (t, CellSpec _ (Just (FluidSpec _ (Quantity q)))) ← M.toList (fxCells fx)
      , q ≤ 0 ]
    , [ "duration must be positive" | unLogicalTime (fxDuration fx) ≤ 0 ]
    , concatMap barrierErrors (fxBarriers fx)
    , concatMap scheduleErrors (fxSchedule fx)
    ]
  where
    chunkSet = S.fromList (map fst (fxChunks fx))
    declared t = S.member (tileChunk t) chunkSet
    wetSurfaces =
        [ unSurface (surfaceOf e q)
        | CellSpec e (Just (FluidSpec _ q)) ← M.elems (fxCells fx) ]
    highestSurface = if null wetSurfaces then Nothing else Just (maximum wetSurfaces)
    barrierErrors b = concat
        [ [ "barrier " <> barrierName b <> " has no cells" | null (barrierCells b) ]
        , [ "barrier " <> barrierName b <> " cell outside the fixture: " <> tshow t
          | t ← barrierCells b, not (declared t) ]
        , [ "barrier " <> barrierName b <> " crest "
              <> tshow (unElevation (barrierCrest b))
              <> " is not above the highest initial surface "
              <> tshow s
          | Just s ← [highestSurface], unElevation (barrierCrest b) ≤ s ]
        , [ "barrier " <> barrierName b <> " cell starts closed but wet: " <> tshow t
          | t ← barrierCells b
          , let CellSpec e fl = initialCell fx t
          , e ≥ barrierCrest b, isJust fl ]
        ]
    scheduleErrors (Scheduled (LogicalTime at) op) = concat
        [ [ "operation scheduled before time 0: " <> tshow op | at < 0 ]
        , [ "operation scheduled at or after the duration: " <> tshow op
          | at ≥ unLogicalTime (fxDuration fx) ]
        , [ "operation outside the fixture: " <> tshow op | not (declared (opTile op)) ]
        , [ "operation with a non-positive quantity: " <> tshow op
          | Just (Quantity q) ← [opQuantity op], q ≤ 0 ]
        ]
    opTile (SetTerrain t _ _) = t
    opTile (AddFluid t _ _)   = t
    opTile (RemoveFluid t _)  = t
    opQuantity (AddFluid _ _ q)  = Just q
    opQuantity (RemoveFluid _ q) = Just q
    opQuantity SetTerrain{}      = Nothing
