{-# LANGUAGE Strict #-}
-- | Per-unit line-of-sight. A unit's visible tile set is the
-- intersection of:
--   * a circular radius (perception × 'awareRangeTiles' tiles, scaled
--     by the page-local 'nightPerceptionFactor' — #1230),
--   * a 120° cone centered on the unit's facing direction,
--   * line-of-sight against terrain Z (a hill in the way blocks vision).
--
-- Used by the AI to discover water sources, to reveal placed locations
-- (#1230), and (in the future) to drive fog of war.
--
-- Every query below resolves terrain/clock/world-size against the
-- OWNING unit's own world page ('Unit.Types.uiPage'), never
-- 'World.Types.wmVisible's list order (#797) — each visible page has
-- its own 'wsTilesRef'\/'wsTimeRef'\/'wsGenParamsRef', so reading
-- "whichever page happens to be first" let a secondary page's units
-- see through the ACTIVE page's hills and dodge on the active page's
-- time of day. A unit (or defender\/attacker pair) whose own page can't
-- be resolved — missing, destroyed, not visible, or simply different
-- pages — gets a safe empty\/zero result rather than silently falling
-- back to an unrelated page.
--
-- #1230 split that resolution in two so location reveal and the public
-- query can share ONE sight definition without sharing one visibility
-- policy: 'visibleTilesOnPage' is the whole calculation over an
-- ALREADY-RESOLVED page, and 'unitVisibleTiles' is the public wrapper
-- that resolves the unit's page and keeps its 'wmVisible' gate. A
-- caller that already holds the unit's own 'WorldState' (the
-- per-loaded-page discovery tick, the per-loaded-page awareness query)
-- calls the core directly and therefore keeps working on a page that is
-- loaded but hidden.
module Unit.LineOfSight
    ( unitVisibleTiles
    , visibleTilesOnPage
    , unitAwareness
    , nightPerceptionFactor
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import Data.IORef (readIORef)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv, unitManagerRef)
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
import Unit.Direction (Direction(..))
import World.Types (WorldManager(..), WorldState(..), WorldGenParams(..),
                    WorldPageId)
import World.Time.Types (worldTimeToSunAngle)
import World.Chunk.Types (LoadedChunk(..), columnIndex, wrapChunkCoordU)
import World.Tile.Types (lookupChunk, WorldTileData(..))
import World.Generate.Coordinates (globalToChunk)
import World.Time.Local (localSunAngle)

-- | All tiles the unit can currently see. Empty list if the unit
--   doesn't exist or its own page isn't loaded/visible. Includes the
--   unit's own tile (always visible).
--
--   The 'wmVisible' gate is this PUBLIC query's own contract (#797) and
--   is deliberately preserved (#1230 requirement 6): a unit on a page
--   the player has hidden reports no vision here. Location reveal needs
--   the same sight on a loaded-but-hidden page, so it calls
--   'visibleTilesOnPage' with that page's own 'WorldState' instead —
--   the same calculation, without this wrapper's visibility policy.
unitVisibleTiles ∷ EngineEnv → UnitId → IO [(Int, Int)]
unitVisibleTiles env uid = do
    um ← readIORef (unitManagerRef env)
    case HM.lookup uid (umInstances um) of
        Nothing → return []
        Just inst → do
            wm ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
            if uiPage inst `notElem` wmVisible wm then return []
            else case lookup (uiPage inst) (wmWorlds wm) of
                Nothing → return []
                Just ws → visibleTilesOnPage ws inst

-- | THE sight calculation (#1230 requirement 5) — every tile @inst@ can
--   see, given the 'WorldState' of the page it is standing on. Both the
--   public 'unitVisibleTiles' query (and therefore Lua's
--   @unit.getVisibleTiles@) and location reveal / per-unit location
--   knowledge run through this one function, so they can never disagree
--   about radius, night, cone, or occlusion.
--
--   Terrain, clock and world size all come from the PASSED page's own
--   refs — never 'activeWorldSizeChunks', whose @wmVisible@ fallback
--   would compute a hidden page's 'localSunAngle' (and therefore its
--   night radius) from the world-default 128-chunk circumference
--   instead of that page's real one.
--
--   The radius is the perception range scaled by the page-local
--   'nightPerceptionFactor' (#1230 requirement 4): a tile that is
--   visible under an otherwise-identical daytime condition can be
--   beyond the night-scaled radius and therefore invisible. The
--   @max 1@ floor is unchanged, so a unit is never stone-blind — its
--   own tile and immediate ring stay visible at midnight.
visibleTilesOnPage ∷ WorldState → UnitInstance → IO [(Int, Int)]
visibleTilesOnPage ws inst = do
    wtd     ← readIORef (wsTilesRef ws)
    wt      ← readIORef (wsTimeRef ws)
    mParams ← readIORef (wsGenParamsRef ws)
    let worldSize = maybe fallbackWorldSizeChunks wgpWorldSize mParams
        ux = floor (uiGridX inst) ∷ Int
        uy = floor (uiGridY inst) ∷ Int
        uz = uiGridZ inst
        -- Eye Z is one tile above the unit's footing.
        eyeZ = fromIntegral (uz + 1) ∷ Double
        (fx, fy) = facingVector (uiFacing inst)
        perception = HM.lookupDefault 1.0 "perception" (uiStats inst)
        night = nightPerceptionFactor
                    (localSunAngle worldSize ux uy (worldTimeToSunAngle wt))
        radius = max 1 (floor (realToFrac perception * awareRangeTiles
                               * realToFrac night) ∷ Int)

        inFOV (dx, dy)
          | dx ≡ 0 ∧ dy ≡ 0 = True   -- own tile
          | otherwise =
              let dd = dx * dx + dy * dy
              in dd ≤ radius * radius ∧ inCone fx fy dx dy

        visible =
            [ (ux + dx, uy + dy)
            | dx ← [-radius .. radius]
            , dy ← [-radius .. radius]
            , inFOV (dx, dy)
            , (dx ≡ 0 ∧ dy ≡ 0)
              ∨ notBlocked worldSize wtd ux uy eyeZ (ux + dx) (uy + dy)
            ]
    return visible

-- | How aware is @defender@ of @attacker@ right now (0..1)? Used by
--   combat to gate the active DODGE — you can't slip a blow you never
--   saw coming. Three outcomes:
--
--     * 1.0  — attacker is within perception range, inside the defender's
--              facing cone, and terrain doesn't block the sightline:
--              fully aware, best chance to react.
--     * 0.4  — attacker is very close (≤ 'awareCloseRadius') but OUTSIDE
--              the cone: you sense a body in your face but can't fully
--              read the strike (peripheral / behind).
--     * 0.0  — terrain blocks LOS, the attacker is beyond perception
--              range, or it's outside the cone AND not close: a blind
--              spot. No dodge — this is what makes an ambush pounce land.
--
--   Perception widens the range; facing is the defender's last sim
--   heading (set by movement — a unit that walked up to its foe faces it).
--   Result is scaled by 'nightPerceptionFactor': a defender senses an
--   incoming blow less reliably in the dark (#315).
--
--   Zero (no dodge) when attacker and defender are on different world
--   pages, or when the defender's own page can't be resolved — missing,
--   destroyed, or registered but currently HIDDEN (removed from
--   wmVisible by world.hide while still present in wmWorlds) — a unit
--   never gains awareness from another page's geometry, clock, or
--   world-size (#797).
unitAwareness ∷ EngineEnv → UnitInstance → UnitInstance → IO Float
unitAwareness env defender attacker
    | uiPage defender ≠ uiPage attacker = pure 0.0
    | otherwise = do
        wm ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
        if uiPage defender `notElem` wmVisible wm then pure 0.0
        else case lookup (uiPage defender) (wmWorlds wm) of
            Nothing → pure 0.0
            Just ws → do
                let defX = uiGridX defender
                    defY = uiGridY defender
                    atkX = uiGridX attacker
                    atkY = uiGridY attacker
                    dxF  = realToFrac (atkX - defX) ∷ Double
                    dyF  = realToFrac (atkY - defY) ∷ Double
                    dist = sqrt (dxF * dxF + dyF * dyF)
                    perception = HM.lookupDefault 1.0 "perception" (uiStats defender)
                    perceptionRange = max 1.5 (realToFrac perception * awareRangeTiles)
                    (fvx, fvy) = facingVector (uiFacing defender)
                    dot   = dxF * fvx + dyF * fvy
                    lenSq = dxF * dxF + dyF * dyF
                    -- Same half-angle-60° cone test as the vision FOV (cos²60 = 0.25).
                    inFacingCone = dist < 1.0e-4 ∨ (dot ≥ 0 ∧ dot * dot ≥ 0.25 * lenSq)
                blocked ← losBlockedBetween env defender attacker
                wt ← readIORef (wsTimeRef ws)
                let sunAngle = worldTimeToSunAngle wt
                worldSize ← activeWorldSizeChunks env (uiPage defender)
                let localAngle = localSunAngle worldSize (floor defX) (floor defY) sunAngle
                pure $! nightPerceptionFactor localAngle *
                        if dist > perceptionRange ∨ blocked
                        then 0.0
                        else if inFacingCone           then 1.0
                        else if dist ≤ awareCloseRadius then awarePeripheral
                        else 0.0

-- | The given page's size (in chunks), for 'localSunAngle'. Falls back
--   to 'fallbackWorldSizeChunks' rather than failing — a defender's
--   local time of day just degrades to the world-default circumference
--   in that window.
--
--   Only 'unitAwareness' uses this, and only AFTER its own 'wmVisible'
--   check has already passed, so the fallback below is reachable here
--   solely for a page with no live gen params. 'visibleTilesOnPage'
--   deliberately does NOT route through it: it is handed the page's own
--   'WorldState' precisely so a hidden page reports its real size.
activeWorldSizeChunks ∷ EngineEnv → WorldPageId → IO Int
activeWorldSizeChunks env pageId = do
    wm ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    if pageId `notElem` wmVisible wm then pure fallbackWorldSizeChunks
    else case lookup pageId (wmWorlds wm) of
        Nothing → pure fallbackWorldSizeChunks
        Just ws → do
            mParams ← readIORef (wsGenParamsRef ws)
            pure (maybe fallbackWorldSizeChunks wgpWorldSize mParams)

-- | World size (chunks) assumed when a page's gen params aren't live
--   yet — the same default 'World.Render.Quads' uses in that window.
fallbackWorldSizeChunks ∷ Int
fallbackWorldSizeChunks = 128

-- | Multiplier on perception from time of day: 1.0 at noon, dipping to
--   'nightPerceptionFloor' at midnight, ramping smoothly through
--   dawn/dusk. Applied by 'unitAwareness' to its scalar 0..1 result and
--   (since #1230) by 'visibleTilesOnPage' to its binary radius — each
--   exactly ONCE, on independent quantities: the two paths never
--   compose, so nothing applies it twice. A cosine keyed to 'sunAngle' peaking at 0.5 (noon) and
--   troughing at 0.0\/1.0 (midnight) — the mapping 'WorldTime' documents
--   (@World.Time.Types@) — rather than reusing 'computeAmbientLight'
--   (@Engine.Loop.Frame@), whose own phase is tuned for the lighting
--   shader, not gameplay, and shouldn't be coupled to this.
nightPerceptionFactor ∷ Float → Float
nightPerceptionFactor sunAngle =
    let angle = (sunAngle - 0.5) * 2 * π
        height = cos angle ∷ Float
    in nightPerceptionFloor + (1.0 - nightPerceptionFloor) * (height + 1.0) / 2.0

-- | Awareness multiplier at deepest night (midnight). Reduced, not
--   eliminated — an ambush still lands more easily at night, but a
--   defender isn't stone-blind.
nightPerceptionFloor ∷ Float
nightPerceptionFloor = 0.5

-- | Terrain line-of-sight between two units (true ⇒ a hill/wall blocks
--   the sightline, the units are on different pages, or the defender's
--   page can't be resolved — missing, destroyed, or hidden — a
--   cross-page or unresolvable pair is treated as blocked, never as an
--   unearned clear sightline, #797). Falls back to "clear" only when NO
--   world page exists at all (the pre-boot / legacy no-page state),
--   matching 'notBlocked''s chunk-missing assumption — unchanged
--   single-world behavior. That empty-wmWorlds check must run BEFORE
--   the visibility check below: a genuinely page-less world also has an
--   empty wmVisible, and only the wmWorlds-empty case gets the "clear"
--   fallback — a page that's merely hidden (registered in wmWorlds,
--   removed from wmVisible by world.hide) must still come out blocked.
losBlockedBetween ∷ EngineEnv → UnitInstance → UnitInstance → IO Bool
losBlockedBetween env defender attacker
    | uiPage defender ≠ uiPage attacker = pure True
    | otherwise = do
        wm ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
        case wmWorlds wm of
            [] → pure False
            worlds
                | uiPage defender `notElem` wmVisible wm → pure True
                | otherwise → case lookup (uiPage defender) worlds of
                    Nothing → pure True
                    Just ws → do
                        wtd ← readIORef (wsTilesRef ws)
                        -- The page's own world size, for the canonical
                        -- chunk-key resolution 'tileTerrainZ' does: a
                        -- combat sightline near the seam must find the
                        -- same hill the visible-tile rasterization does,
                        -- or the two disagree about what blocks.
                        mParams ← readIORef (wsGenParamsRef ws)
                        let worldSize =
                                maybe fallbackWorldSizeChunks wgpWorldSize mParams
                            ux   = floor (uiGridX defender) ∷ Int
                            uy   = floor (uiGridY defender) ∷ Int
                            gx   = floor (uiGridX attacker) ∷ Int
                            gy   = floor (uiGridY attacker) ∷ Int
                            eyeZ = fromIntegral (uiGridZ defender + 1) ∷ Double
                        pure (not (notBlocked worldSize wtd ux uy eyeZ gx gy))

-- | Vision/awareness radius per point of perception (tiles).
awareRangeTiles ∷ Double
awareRangeTiles = 6.0

-- | Within this many tiles a defender senses an attacker even outside
--   its facing cone (but only at 'awarePeripheral' awareness).
awareCloseRadius ∷ Double
awareCloseRadius = 1.6

-- | Awareness of an attacker that's close but outside the facing cone.
awarePeripheral ∷ Float
awarePeripheral = 0.4

-- | Direction → unit-length screen-space heading. Y grows south, so
--   DirS is (0, +1).
facingVector ∷ Direction → (Double, Double)
facingVector DirN  = ( 0.0      , -1.0)
facingVector DirS  = ( 0.0      ,  1.0)
facingVector DirE  = ( 1.0      ,  0.0)
facingVector DirW  = (-1.0      ,  0.0)
facingVector DirNE = ( 0.7071068, -0.7071068)
facingVector DirNW = (-0.7071068, -0.7071068)
facingVector DirSE = ( 0.7071068,  0.7071068)
facingVector DirSW = (-0.7071068,  0.7071068)

-- | Is offset (dx, dy) inside the 120° cone centered on (fx, fy)?
--   Half-angle 60°, cos(60°) = 0.5. We compare dot² to 0.25 × |v|² to
--   avoid a sqrt; the dot ≥ 0 check rules out the back hemisphere
--   (where squaring would flip the sign).
inCone ∷ Double → Double → Int → Int → Bool
inCone fx fy dx dy =
    let dxF = fromIntegral dx
        dyF = fromIntegral dy
        dot = dxF * fx + dyF * fy
        lenSq = dxF * dxF + dyF * dyF
    in dot ≥ 0 ∧ dot * dot ≥ 0.25 * lenSq

-- | Cast a line from (ux, uy) at eyeZ to (gx, gy)'s surface; True if
--   no intermediate tile's terrain top rises above the line's height
--   at that horizontal step. Endpoints excluded — the source can't
--   block itself, and the target's own elevation IS the line's
--   terminus (not an occluder).
notBlocked ∷ Int → WorldTileData → Int → Int → Double → Int → Int → Bool
notBlocked worldSize wtd ux uy eyeZ gx gy =
    let targetZ = case tileTerrainZ worldSize wtd gx gy of
            Just z  → fromIntegral z
            Nothing → 0     -- chunk not loaded → assume flat (arena)
        steps = max (abs (gx - ux)) (abs (gy - uy))
        n     = fromIntegral steps ∷ Double
    in steps ≤ 1
       ∨ all (unblockedStep worldSize wtd ux uy gx gy eyeZ targetZ n)
             [1 .. steps - 1]

unblockedStep
    ∷ Int → WorldTileData → Int → Int → Int → Int → Double → Double → Double
    → Int → Bool
unblockedStep worldSize wtd ux uy gx gy eyeZ targetZ n i =
    let t = fromIntegral i / n ∷ Double
        interpX = fromIntegral ux + t * fromIntegral (gx - ux)
        interpY = fromIntegral uy + t * fromIntegral (gy - uy)
        tx = floor interpX
        ty = floor interpY
        lineZ = eyeZ + t * (targetZ - eyeZ)
        groundZ = case tileTerrainZ worldSize wtd tx ty of
            Just z  → fromIntegral z
            Nothing → 0
    in groundZ ≤ lineZ

-- | Terrain surface Z at a global tile, resolved against the CANONICAL
--   chunk key (#1230).
--
--   Chunks are STORED u-wrapped ('wrapChunkCoordU', see the tile-coordinate
--   frame contract in @CLAUDE.md@), and 'lookupChunk' is a plain hashmap
--   lookup that does no wrapping of its own. A raw 'globalToChunk' key
--   from an alias frame near the cylindrical seam therefore misses a
--   chunk that IS loaded, and the miss reads as \"chunk not loaded →
--   assume flat\" — which for occlusion means \"nothing blocks\". Sight
--   drives seam-aware location reveal since #1230, so a hill sitting on
--   the canonical seam tile would silently stop blocking and a ruin
--   behind it would be revealed through solid ground.
--
--   Canonicalizing here fixes every caller at once — the visible-tile
--   rasterization and the combat sightline both — which is the point:
--   two terrain lookups that disagreed about where a hill is would be
--   worse than either behaviour alone. Away from the seam, and in
--   arena / non-wrapping (@worldSize ≤ 0@) worlds, 'wrapChunkCoordU' is
--   the identity, so this changes nothing.
tileTerrainZ ∷ Int → WorldTileData → Int → Int → Maybe Int
tileTerrainZ worldSize wtd gx gy =
    let (coord, (lx, ly)) = globalToChunk gx gy
    in case lookupChunk (wrapChunkCoordU worldSize coord) wtd of
        Nothing → Nothing
        Just lc → Just (lcTerrainSurfaceMap lc VU.! columnIndex lx ly)
