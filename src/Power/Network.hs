{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Power-grid connectivity + energy balance (#360): the core sim on top
--   of #358's placed nodes and #359's wire tiles. A "network" is a
--   connected component of wire tiles (4-dir cardinal adjacency, matching
--   scripts/wire.lua's autotile shape) plus whichever power nodes sit on
--   or orthogonally beside it — two nodes that don't share a wire path
--   are NOT on the same network even if their tiles happen to be
--   adjacent to each other directly. A node touching two otherwise-
--   disconnected wire stubs bridges them into ONE merged network
--   ('groupByComponent') rather than attaching to both independently —
--   a proper connected-components partition can't put one node in two
--   groups at once.
--
--   Connectivity and a network's generation/drain numbers are recomputed
--   fresh every call — nothing about network MEMBERSHIP is persisted,
--   only a battery's own accumulated charge ('pnStoredWh', Power.Types)
--   survives a save. Recomputing every tick is simpler than maintaining
--   incremental connectivity and cheap at the node/wire counts a
--   colony's grid is expected to reach; the issue's own "recompute only
--   when wire/nodes change" note is a future perf tuning knob, not a
--   correctness requirement — nothing here would need to change shape to
--   add that cache later.
--
--   'PowerNodes' is always the single source of truth for node data
--   (role, wattage, capacity, current charge) — every function here
--   takes it plus a separate tile-position lookup, rather than a list of
--   caller-constructed node copies that could drift out of sync with the
--   registry.
--
--   Consumer drain (#361 — "requires_power" workshops) isn't wired up
--   yet: 'tickPowerNodes' takes a @drain-by-node@ map as a parameter so
--   the balance math (charge, hold, brownout-under-load) is fully
--   exercised and tested now; the production call site passes an empty
--   map until #361 has real consumers to populate it from.
module Power.Network
    ( PowerNetworkStatus(..)
    , PowerNetworkSnapshot(..)
    , solarIntensity
    , wireComponents
    , computeSnapshots
    , tickPowerNodes
    , wireTilesOn
    , positionsOf
    ) where

import UPrelude
import Data.List (nub)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Building.Types (BuildingInstance(..), BuildingManager(..))
import Structure.Types (StructureSlot(..))
import World.Chunk.Types (LoadedChunk(..))
import World.Page.Types (WorldPageId)
import World.Tile.Types (WorldTileData(..))
import Power.Types

-- | Whether a network's demand was fully met this instant.
data PowerNetworkStatus = Powered | Brownout
    deriving (Show, Eq)

-- | A read-only view of one connected component's current numbers — the
--   Lua query surface (power.listNetworks / getNetworkForNode) reports
--   exactly this. Never persisted; recomputed live on every query.
data PowerNetworkSnapshot = PowerNetworkSnapshot
    { pnwNodeIds     ∷ ![PowerNodeId]  -- ^ every node on this network, oldest id first
    , pnwGenerationW ∷ !Float          -- ^ Σ source output this instant
    , pnwDrainW      ∷ !Float          -- ^ Σ registered consumer draw this instant
    , pnwStoredWh    ∷ !Float          -- ^ Σ storage node charge
    , pnwCapacityWh  ∷ !Float          -- ^ Σ storage node capacity
    , pnwStatus      ∷ !PowerNetworkStatus
    } deriving (Show, Eq)

-- | Solar output as a fraction of peak, from a raw sun angle
--   ('World.Time.Types.worldTimeToSunAngle': 0 = midnight, 0.25 = dawn,
--   0.5 = noon, 0.75 = dusk). Cosine of the sun's elevation — 1 at noon,
--   0 at dawn/dusk, clamped to 0 overnight — one of the two curve shapes
--   the issue leaves open, chosen for being smooth rather than a flat
--   day/night step.
solarIntensity ∷ Float → Float
solarIntensity sunAngle = max 0 (negate (cos (2 * pi * sunAngle)))

neighborsOf ∷ (Int, Int) → [(Int, Int)]
neighborsOf (x, y) = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

-- | Flood-fill connected components of a set of wire tiles (4-dir
--   cardinal adjacency).
wireComponents ∷ HS.HashSet (Int, Int) → [HS.HashSet (Int, Int)]
wireComponents tiles = go (HS.toList tiles) HS.empty []
  where
    go [] _ acc = acc
    go (t : ts) seen acc
        | HS.member t seen = go ts seen acc
        | otherwise =
            let comp = bfs (HS.singleton t) [t]
            in go ts (HS.union seen comp) (comp : acc)
    bfs visited [] = visited
    bfs visited (cur : rest) =
        let fresh = [ n | n ← neighborsOf cur
                         , HS.member n tiles
                         , not (HS.member n visited) ]
        in bfs (foldl' (flip HS.insert) visited fresh) (rest ++ fresh)

-- | Which wire components (by index into @comps@) a tile touches — the
--   tile itself (rare — a building could share a tile with a wire
--   overlay) or any of its 4 orthogonal neighbours. A tile that only
--   ever touches wire is attached to at most one component (BFS already
--   guarantees that); a NODE's tile can legitimately touch two or more
--   otherwise-disconnected wire stubs at once (e.g. a panel sitting
--   between two separate short runs) — that's the case 'mergedRoots'
--   below resolves.
touchedComponents ∷ HM.HashMap (Int, Int) Int → (Int, Int) → [Int]
touchedComponents tileToIdx tile =
    nub [ i | t ← tile : neighborsOf tile, Just i ← [HM.lookup t tileToIdx] ]

-- | Bare-bones union-find over component indices @[0 .. n-1]@: 'ufFind'
--   walks parent pointers to the representative; 'ufUnion' points one
--   root at the other. No path compression / union-by-rank — @n@ is the
--   wire-component count, expected small, so the naive walk is cheap.
newtype UnionFind = UnionFind (HM.HashMap Int Int)

ufNew ∷ Int → UnionFind
ufNew n = UnionFind (HM.fromList [ (i, i) | i ← [0 .. n - 1] ])

ufFind ∷ UnionFind → Int → Int
ufFind uf@(UnionFind m) i = case HM.lookup i m of
    Just p | p ≡ i     → i
           | otherwise → ufFind uf p
    Nothing            → i

ufUnion ∷ UnionFind → Int → Int → UnionFind
ufUnion uf@(UnionFind m) a b =
    let ra = ufFind uf a
        rb = ufFind uf b
    in if ra ≡ rb then uf else UnionFind (HM.insert ra rb m)

-- | Group a world's nodes by which (possibly node-merged) wire network
--   they attach to, looking each one up in @nodes@ (the single source of
--   truth for its current role/wattage/capacity/charge) by id. A node
--   touching two or more otherwise-disconnected wire components BRIDGES
--   them into one network via union-find, rather than being attached to
--   several components at once (a proper connected-components partition
--   can't put one vertex in two groups — a node bridging two stubs
--   physically joins them, it doesn't pick one arbitrarily or generate
--   into both). A node touching NO wire component at all — including two
--   nodes directly adjacent to each other with no wire tile between them
--   — attaches to nothing. Bare wire runs with no attached node produce
--   no group; a position with no matching node is silently skipped
--   (defensive, not expected in practice).
groupByComponent ∷ [HS.HashSet (Int, Int)] → PowerNodes
                 → HM.HashMap PowerNodeId (Int, Int) → [[PowerNode]]
groupByComponent comps nodes positions =
    let tileToIdx = HM.fromList [ (t, i) | (i, comp) ← zip [0 ..] comps
                                          , t ← HS.toList comp ]
        nodeList  = HM.toList positions
        uf0       = ufNew (length comps)
        -- Every node that touches 2+ components unions them together.
        mergedUf  = foldl' (\uf (_, tile) → case touchedComponents tileToIdx tile of
                        (i : is@(_ : _)) → foldl' (\u j → ufUnion u i j) uf is
                        _                → uf
                    ) uf0 nodeList
        rootFor tile = case touchedComponents tileToIdx tile of
            (i : _) → Just (ufFind mergedUf i)
            []      → Nothing
        byRoot = HM.fromListWith (++)
                   [ (root, [nid]) | (nid, tile) ← nodeList, Just root ← [rootFor tile] ]
    in [ grp | nids ← HM.elems byRoot
             , let grp = [ n | nid ← nids, Just n ← [lookupPowerNode nid nodes] ]
             , not (null grp) ]

-- | Distribute a charge (non-negative Wh) across batteries proportional
--   to each one's remaining headroom, so a fuller battery takes less.
--   Surplus beyond total capacity is curtailed (lost), like a full bank
--   spilling excess solar.
chargeBatteries ∷ Float → [PowerNode] → [PowerNode]
chargeBatteries wh batteries =
    let headrooms     = [ max 0 (pnCapacityWh b - pnStoredWh b) | b ← batteries ]
        totalHeadroom = sum headrooms
    in if totalHeadroom ≤ 0
       then batteries
       else [ b { pnStoredWh = pnStoredWh b + min hr (wh * (hr / totalHeadroom)) }
            | (b, hr) ← zip batteries headrooms ]
              -- clamp each share at its OWN headroom: when wh exceeds
              -- totalHeadroom every share would otherwise overshoot its
              -- battery's capacity by the same ratio — clamping fills
              -- each exactly full instead (the curtailed-surplus case).

-- | Distribute a discharge (non-negative Wh demand) across batteries
--   proportional to each one's current stored charge. When the demand
--   exceeds total stored, every battery's share exceeds what it holds
--   and all drain to exactly 0 (full depletion, not a favored survivor).
dischargeBatteries ∷ Float → [PowerNode] → [PowerNode]
dischargeBatteries wh batteries =
    let stores      = map pnStoredWh batteries
        totalStored = sum stores
    in if totalStored ≤ 0
       then batteries
       else [ b { pnStoredWh = max 0 (pnStoredWh b - wh * (s / totalStored)) }
            | (b, s) ← zip batteries stores ]

-- | One network's aggregate numbers plus its batteries' updated charge
--   after @dtHours@ of the given solar intensity vs. registered drain.
--   @dtHours = 0@ is a pure read — no mutation, current numbers only
--   (what 'computeSnapshots' uses for a live query between ticks).
tickGroup ∷ Float → HM.HashMap PowerNodeId Float → Float
          → [PowerNode] → ([PowerNode], PowerNetworkSnapshot)
tickGroup intensity drainByNode dtHours nodes =
    let generationW      = sum [ pnPeakWatts n * intensity
                                | n ← nodes, pnRole n ≡ PowerSource ]
        drainW           = sum [ HM.lookupDefault 0 (pnId n) drainByNode | n ← nodes ]
        batteries        = [ n | n ← nodes, pnRole n ≡ PowerStorage ]
        totalCap         = sum (map pnCapacityWh batteries)
        deltaWh          = (generationW - drainW) * dtHours
        batteries'       = if deltaWh ≥ 0
                            then chargeBatteries deltaWh batteries
                            else dischargeBatteries (negate deltaWh) batteries
        byId             = HM.fromList [ (pnId b, b) | b ← batteries' ]
        updated          = [ HM.lookupDefault n (pnId n) byId | n ← nodes ]
        totalStoredAfter = sum (map pnStoredWh batteries')
        -- Rate-based, not energy-based: browned out only when CURRENT
        -- demand exceeds CURRENT supply and there's no stored charge
        -- left to cover the gap. Independent of dtHours so a live query
        -- (computeSnapshots, dtHours = 0) reports the same status a
        -- tick would — "would this network keep the lights on right
        -- now", not "did energy actually move this call".
        status           = if generationW ≥ drainW ∨ totalStoredAfter > 1.0e-6
                            then Powered else Brownout
    in ( updated
       , PowerNetworkSnapshot (map pnId nodes) generationW drainW
                              totalStoredAfter totalCap status )

-- | Every connected network's current numbers, read-only (no charge
--   mutation) — what a Lua query reports at any instant between ticks.
computeSnapshots ∷ Float → HM.HashMap PowerNodeId Float → HS.HashSet (Int, Int)
                 → PowerNodes → HM.HashMap PowerNodeId (Int, Int)
                 → [PowerNetworkSnapshot]
computeSnapshots sunAngle drainByNode wireTiles nodes positions =
    [ snd (tickGroup (solarIntensity sunAngle) drainByNode 0 grp)
    | grp ← groupByComponent (wireComponents wireTiles) nodes positions ]

-- | Advance every network on a page by @dtGameSeconds@ of generation vs.
--   registered drain, folding each network's updated battery charge back
--   into the node registry. Nodes not attached to any wire network are
--   untouched. A no-op for @dtGameSeconds ≤ 0@ (paused / no time passed).
tickPowerNodes ∷ Float → HM.HashMap PowerNodeId Float → Float
              → HS.HashSet (Int, Int) → HM.HashMap PowerNodeId (Int, Int)
              → PowerNodes → PowerNodes
tickPowerNodes sunAngle drainByNode dtGameSeconds wireTiles positions nodes
    | dtGameSeconds ≤ 0 = nodes
    | otherwise =
        let intensity = solarIntensity sunAngle
            dtHours   = dtGameSeconds / 3600
            groups    = groupByComponent (wireComponents wireTiles) nodes positions
            updates   = concatMap (fst . tickGroup intensity drainByNode dtHours) groups
        in nodes { pnsNodes = foldl' (\m n → HM.insert (pnId n) n m)
                                     (pnsNodes nodes) updates }

wireSlotTag ∷ Word8
wireSlotTag = fromIntegral (fromEnum SWire)

-- | Every (gx, gy) carrying a wire piece across a page's LOADED chunks —
--   the shared glue between the world-thread tick ('World.Thread.Power')
--   and a live Lua query, so both see identically-defined connectivity.
--   A chunk that hasn't loaded contributes no wire, matching how
--   scripts/wire.lua itself only ever sees loaded-chunk state.
wireTilesOn ∷ WorldTileData → HS.HashSet (Int, Int)
wireTilesOn td = HS.fromList
    [ (gx, gy)
    | lc ← HM.elems (wtdChunks td)
    , ((gx, gy, slotTag), _) ← HM.toList (lcStructures lc)
    , slotTag ≡ wireSlotTag
    ]

-- | Every node's tile, resolved via the building it rides on and
--   restricted to one page. A node whose building isn't on this page is
--   dropped (shouldn't happen — a node only exists riding an
--   already-placed building on its own page — but cheap to filter
--   defensively rather than assume).
positionsOf ∷ WorldPageId → BuildingManager → PowerNodes
           → HM.HashMap PowerNodeId (Int, Int)
positionsOf pageId bm nodes = HM.fromList
    [ (pnId n, (biAnchorX bi, biAnchorY bi))
    | n ← allNodes nodes
    , Just bi ← [HM.lookup (pnBuilding n) (bmInstances bm)]
    , biPage bi ≡ pageId
    ]
