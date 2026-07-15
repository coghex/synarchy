{-# LANGUAGE Strict, UnicodeSyntax, OverloadedStrings #-}
-- | Multi-world page-ownership regression for Unit.LineOfSight (#797).
--
--   Before the fix, unitVisibleTiles / unitAwareness / losBlockedBetween
--   all resolved terrain, clock, and world-size from wmVisible's HEAD
--   page instead of the querying unit's own uiPage. These specs build
--   TWO synthetic visible pages by hand — no engine boot, no worldgen,
--   mirroring Test.Headless.World.CursorInfo's pattern — with different
--   terrain, different clocks, and different world sizes, and assert
--   every query answers from the OWNING unit's page regardless of
--   wmVisible's list order.
module Test.Headless.Unit.LineOfSight (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (writeIORef)
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..))
import Engine.Core.State (EngineEnv(..))
import Unit.Direction (Direction(..))
import Unit.LineOfSight (unitVisibleTiles, unitAwareness, nightPerceptionFactor)
import Unit.Types
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..), chunkSize)
import World.Tile.Types (WorldTileData(..))
import World.Fluid.Types (emptyIceMap)
import World.Flora.Types (emptyFloraChunkData)
import Structure.Types (emptyChunkStructures)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.State.Types (WorldState(..), emptyWorldState, WorldManager(..))
import World.Page.Types (WorldPageId(..))
import World.Time.Types (WorldTime(..), worldTimeToSunAngle)
import World.Time.Local (localSunAngle)

-- ---- Terrain fixtures (mirrors Test.Headless.Unit.Pathing.AStar) ----

flatChunk ∷ Int → LoadedChunk
flatChunk z =
    let area = chunkSize * chunkSize
        v = VU.replicate area z
    in LoadedChunk
        { lcCoord = ChunkCoord 0 0, lcTiles = V.empty
        , lcSurfaceMap = v, lcTerrainSurfaceMap = v
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
        , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
        , lcMagma = Nothing, lcStructures = emptyChunkStructures
        }

-- A tall wall at lx = 8 (every ly), flat elsewhere — blocks a straight
-- sightline that crosses lx = 8, unlike flatChunk.
wallChunk ∷ Int → Int → LoadedChunk
wallChunk flatZ wallZ =
    let area = chunkSize * chunkSize
        v = VU.generate area $ \i →
                if i `mod` chunkSize ≡ 8 then wallZ else flatZ
    in LoadedChunk
        { lcCoord = ChunkCoord 0 0, lcTiles = V.empty
        , lcSurfaceMap = v, lcTerrainSurfaceMap = v
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
        , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
        , lcMagma = Nothing, lcStructures = emptyChunkStructures
        }

wtdWith ∷ LoadedChunk → WorldTileData
wtdWith lc = WorldTileData
    { wtdChunks = HM.singleton (ChunkCoord 0 0) lc, wtdMaxChunks = 1 }

-- ---- Minimal unit fixture: only the fields Unit.LineOfSight reads
--      (uiPage, uiGridX/Y/Z, uiFacing, uiStats) matter for these specs;
--      the rest are inert placeholders (mirrors Test.Headless.Unit.
--      Fall's minimal-BodyPart-def helper). ----

testUnit ∷ WorldPageId → Float → Float → Int → Direction → UnitInstance
testUnit page gx gy gz facing = UnitInstance
    { uiDefName = "test", uiName = "", uiPage = page
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = gx, uiGridY = gy, uiGridZ = gz
    , uiRealZ = fromIntegral gz, uiFacing = facing
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = [], uiEquipment = HM.empty
    , uiAccessories = [], uiFactionId = "player", uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing
    }

pageA, pageB, pageGone ∷ WorldPageId
pageA    = WorldPageId "los_test_a"
pageB    = WorldPageId "los_test_b"
pageGone = WorldPageId "los_test_missing"

-- Page A: open terrain, noon, worldSize 4. Page B: a wall at lx=8
-- splitting the (5,8)..(11,8) sightline, midnight, worldSize 256 —
-- everything a defender's OWN page must supply differs from the
-- other page's, so cross-contamination is observable in every check.
setupPages ∷ [WorldPageId] → IO WorldManager
setupPages visibleOrder = do
    wsA ← emptyWorldState
    writeIORef (wsTilesRef wsA) (wtdWith (flatChunk 5))
    writeIORef (wsTimeRef wsA) (WorldTime 12 0)   -- noon
    writeIORef (wsGenParamsRef wsA) (Just defaultWorldGenParams { wgpWorldSize = 4 })
    wsB ← emptyWorldState
    writeIORef (wsTilesRef wsB) (wtdWith (wallChunk 5 40))
    writeIORef (wsTimeRef wsB) (WorldTime 0 0)    -- midnight
    writeIORef (wsGenParamsRef wsB) (Just defaultWorldGenParams { wgpWorldSize = 256 })
    pure (WorldManager [(pageA, wsA), (pageB, wsB)] visibleOrder)

initEnv ∷ IO EngineEnv
initEnv = do
    EngineInitResult env ← initializeEngineHeadless
    pure env

putUnits ∷ EngineEnv → [(UnitId, UnitInstance)] → IO ()
putUnits env us = writeIORef (unitManagerRef env)
    (emptyUnitManager { umInstances = HM.fromList us })

closeTo ∷ Float → Float → Float → Bool
closeTo eps a b = abs (a - b) < eps

spec ∷ Spec
spec = beforeAll initEnv $ do

    describe "unitAwareness resolves terrain from the OWNING unit's page (#797)" $ do
        it "a unit on the open page sees clearly even when the walled page is wmVisible's head" $ \env → do
            wm ← setupPages [pageB, pageA]   -- pageB (walled) is HEAD
            writeIORef (worldManagerRef env) wm
            let defender = testUnit pageA 5 8 5 DirE
                attacker = testUnit pageA 11 8 5 DirE
            putUnits env [(UnitId 1, defender), (UnitId 2, attacker)]
            aw ← unitAwareness env defender attacker
            aw `shouldSatisfy` (> 0)   -- pageA has no wall: not blocked

        it "a unit on the walled page is blocked even when the open page is wmVisible's head" $ \env → do
            wm ← setupPages [pageA, pageB]   -- pageA (open) is HEAD
            writeIORef (worldManagerRef env) wm
            let defender = testUnit pageB 5 8 5 DirE
                attacker = testUnit pageB 11 8 5 DirE
            putUnits env [(UnitId 1, defender), (UnitId 2, attacker)]
            aw ← unitAwareness env defender attacker
            aw `shouldBe` 0   -- pageB's wall blocks the sightline

        it "reordering wmVisible changes neither unit's result" $ \env → do
            let defenderA = testUnit pageA 5 8 5 DirE
                attackerA = testUnit pageA 11 8 5 DirE
                defenderB = testUnit pageB 5 8 5 DirE
                attackerB = testUnit pageB 11 8 5 DirE

            wm1 ← setupPages [pageA, pageB]
            writeIORef (worldManagerRef env) wm1
            putUnits env [(UnitId 1, defenderA), (UnitId 2, attackerA)]
            awA1 ← unitAwareness env defenderA attackerA
            putUnits env [(UnitId 1, defenderB), (UnitId 2, attackerB)]
            awB1 ← unitAwareness env defenderB attackerB

            wm2 ← setupPages [pageB, pageA]
            writeIORef (worldManagerRef env) wm2
            putUnits env [(UnitId 1, defenderA), (UnitId 2, attackerA)]
            awA2 ← unitAwareness env defenderA attackerA
            putUnits env [(UnitId 1, defenderB), (UnitId 2, attackerB)]
            awB2 ← unitAwareness env defenderB attackerB

            awA1 `shouldBe` awA2
            awB1 `shouldBe` awB2

    describe "unitAwareness reads the defender's own page clock (#797)" $ do
        it "the same relative geometry reads noon on page A and midnight on page B" $ \env → do
            wm ← setupPages [pageA, pageB]
            writeIORef (worldManagerRef env) wm
            -- Defender == attacker position: dist 0 always clears range
            -- and cone, and trivially isn't terrain-blocked, isolating
            -- the CLOCK difference as the only variable.
            let defA = testUnit pageA 5 5 5 DirE
                atkA = testUnit pageA 5 5 5 DirE
                defB = testUnit pageB 5 5 5 DirE
                atkB = testUnit pageB 5 5 5 DirE
            putUnits env [(UnitId 1, defA), (UnitId 2, atkA)]
            awNoon ← unitAwareness env defA atkA
            putUnits env [(UnitId 1, defB), (UnitId 2, atkB)]
            awMidnight ← unitAwareness env defB atkB
            awNoon `shouldSatisfy` (> awMidnight)
            closeTo 1e-4 awNoon 1.0 `shouldBe` True
            closeTo 1e-4 awMidnight 0.5 `shouldBe` True

    describe "unitAwareness reads the defender's own page world-size (#797)" $ do
        it "identical clocks but different circumference change the night-perception factor" $ \env → do
            -- Both pages share the SAME time (so global sun angle is
            -- identical); only wgpWorldSize differs (4 vs 256), and
            -- gx /= gy so the longitude-local term is nonzero.
            wsA ← emptyWorldState
            writeIORef (wsTilesRef wsA) (wtdWith (flatChunk 5))
            writeIORef (wsTimeRef wsA) (WorldTime 3 0)
            writeIORef (wsGenParamsRef wsA) (Just defaultWorldGenParams { wgpWorldSize = 4 })
            wsB ← emptyWorldState
            writeIORef (wsTilesRef wsB) (wtdWith (flatChunk 5))
            writeIORef (wsTimeRef wsB) (WorldTime 3 0)
            writeIORef (wsGenParamsRef wsB) (Just defaultWorldGenParams { wgpWorldSize = 256 })
            writeIORef (worldManagerRef env)
                (WorldManager [(pageB, wsB), (pageA, wsA)] [pageB, pageA])
            let defA = testUnit pageA 12 4 5 DirE
                atkA = testUnit pageA 12 4 5 DirE
                defB = testUnit pageB 12 4 5 DirE
                atkB = testUnit pageB 12 4 5 DirE
            putUnits env [(UnitId 1, defA), (UnitId 2, atkA)]
            awA ← unitAwareness env defA atkA
            putUnits env [(UnitId 1, defB), (UnitId 2, atkB)]
            awB ← unitAwareness env defB atkB
            let sunAngle = worldTimeToSunAngle (WorldTime 3 0)
                expectA  = nightPerceptionFactor (localSunAngle 4   12 4 sunAngle)
                expectB  = nightPerceptionFactor (localSunAngle 256 12 4 sunAngle)
            closeTo 1e-4 awA expectA `shouldBe` True
            closeTo 1e-4 awB expectB `shouldBe` True
            awA `shouldNotBe` awB

    describe "cross-page and missing-page safety (#797)" $ do
        it "a defender and attacker on different pages get zero awareness" $ \env → do
            wm ← setupPages [pageA, pageB]
            writeIORef (worldManagerRef env) wm
            let defender = testUnit pageA 5 5 5 DirE
                attacker = testUnit pageB 5 5 5 DirE
            putUnits env [(UnitId 1, defender), (UnitId 2, attacker)]
            aw ← unitAwareness env defender attacker
            aw `shouldBe` 0

        it "a destroyed/missing owning page yields empty vision and zero awareness" $ \env → do
            wm ← setupPages [pageA, pageB]
            writeIORef (worldManagerRef env) wm
            let ghost = testUnit pageGone 5 5 5 DirE
                other = testUnit pageGone 5 5 5 DirE
            putUnits env [(UnitId 1, ghost), (UnitId 2, other)]
            tiles ← unitVisibleTiles env (UnitId 1)
            tiles `shouldBe` []
            aw ← unitAwareness env ghost other
            aw `shouldBe` 0

        it "a HIDDEN page (registered in wmWorlds, absent from wmVisible) yields empty vision and zero awareness" $ \env → do
            -- pageB keeps a full WorldState in wmWorlds (world.hide only
            -- ever drops a page from wmVisible, never from wmWorlds) but
            -- is NOT in the visible list — units on it must not get a
            -- free pass just because their page still exists.
            wm ← setupPages [pageA]
            writeIORef (worldManagerRef env) wm
            let defender = testUnit pageB 5 8 5 DirE
                attacker = testUnit pageB 11 8 5 DirE   -- same-page pair,
                                                          -- in range/cone
            putUnits env [(UnitId 1, defender), (UnitId 2, attacker)]
            tiles ← unitVisibleTiles env (UnitId 1)
            tiles `shouldBe` []
            aw ← unitAwareness env defender attacker
            aw `shouldBe` 0
