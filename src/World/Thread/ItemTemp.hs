{-# LANGUAGE Strict, UnicodeSyntax #-}
-- | Per-page item-temperature tick (#344).
--
--   Runs beside the world clock ('World.Thread.Time.tickWorldTime'), once
--   per visible page per world-thread iteration, in GAME-seconds — so item
--   cooling follows the same clock as flora regrowth and freezes with the
--   pause flag. Three item populations cool: ground items (each at its own
--   tile's ambient), items held by units on the page (at the holder's
--   tile), and items stored in / delivered to buildings on the page (at
--   the building's anchor tile).
--
--   Almost every item is UNTRACKED (iiTemp = Nothing → "at ambient"), so
--   each population is first scanned with the cheap 'hasTrackedTemp'
--   predicate and the rebuild-and-write is skipped entirely when nothing
--   inside is tracked — the steady-state cost of this tick is three
--   read-only scans. Unit / building managers are engine-global refs also
--   written from other threads; updates fold inside atomicModifyIORef'
--   (the Combat.Wounds.tickAllWounds pattern) so they merge with
--   concurrent writes instead of clobbering them.
module World.Thread.ItemTemp
    ( tickItemTemperatures
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Core.State (EngineEnv(..))
import Building.Types (BuildingInstance(..), BuildingManager(..))
import Item.Ground (GroundItem(..), GroundItems(..))
import Item.Temperature (coolItem, hasTrackedTemp)
import Item.Types (ItemManager)
import Unit.Types (UnitInstance(..), UnitManager(..))
import World.Types (WorldGenParams(..), WorldPageId, WorldState(..))
import World.Weather.Ambient (ambientTempAt)

-- | Relax every tracked item temperature on one page toward its tile's
--   ambient by @dtGame@ game-seconds. No-op when the page has no gen
--   params yet (nothing to read an ambient from).
tickItemTemperatures ∷ EngineEnv → WorldPageId → WorldState → Float → IO ()
tickItemTemperatures env pageId ws dtGame = do
    mParams ← readIORef (wsGenParamsRef ws)
    case mParams of
        Nothing → pure ()
        Just p → do
            im ← readIORef (itemManagerRef env)
            let ambientAt ∷ Int → Int → Float
                ambientAt = ambientTempAt (wgpSeed p) (wgpPlates p)
                                (wgpClimateState p) (wgpWorldSize p)
            tickGroundItems ws im ambientAt dtGame
            tickUnitItems env pageId im ambientAt dtGame
            tickBuildingItems env pageId im ambientAt dtGame

-- | Ground items — each cools at ITS OWN tile's ambient, so a hot item
--   dropped on an icy peak equilibrates to the peak, not the valley.
tickGroundItems ∷ WorldState → ItemManager → (Int → Int → Float) → Float
                → IO ()
tickGroundItems ws im ambientAt dtGame = do
    gis ← readIORef (wsGroundItemsRef ws)
    when (any (hasTrackedTemp . giInst) (gisItems gis)) $
        atomicModifyIORef' (wsGroundItemsRef ws) $ \gis' →
            (gis' { gisItems = HM.map coolGi (gisItems gis') }, ())
  where
    coolGi gi
        | hasTrackedTemp (giInst gi) =
            let amb = ambientAt (floor (giX gi)) (floor (giY gi))
            in gi { giInst = coolItem im amb dtGame (giInst gi) }
        | otherwise = gi

-- | Items a unit carries (inventory + equipment + accessories) breathe
--   the air their holder stands in.
tickUnitItems ∷ EngineEnv → WorldPageId → ItemManager
              → (Int → Int → Float) → Float → IO ()
tickUnitItems env pageId im ambientAt dtGame = do
    um ← readIORef (unitManagerRef env)
    when (any unitTracked (umInstances um)) $
        atomicModifyIORef' (unitManagerRef env) $ \um' →
            (um' { umInstances = HM.map coolUnit (umInstances um') }, ())
  where
    unitTracked inst =
        uiPage inst ≡ pageId
        ∧ (  any hasTrackedTemp (uiInventory inst)
           ∨ any hasTrackedTemp (uiEquipment inst)
           ∨ any hasTrackedTemp (uiAccessories inst) )
    coolUnit inst
        | unitTracked inst =
            let amb  = ambientAt (floor (uiGridX inst)) (floor (uiGridY inst))
                cool = coolItem im amb dtGame
            in inst { uiInventory   = map cool (uiInventory inst)
                    , uiEquipment   = HM.map cool (uiEquipment inst)
                    , uiAccessories = map cool (uiAccessories inst)
                    }
        | otherwise = inst

-- | Items stored in (or delivered to) a building sit at the building's
--   anchor tile — a hot item deposited into cargo keeps cooling there
--   instead of staying hot forever.
tickBuildingItems ∷ EngineEnv → WorldPageId → ItemManager
                  → (Int → Int → Float) → Float → IO ()
tickBuildingItems env pageId im ambientAt dtGame = do
    bm ← readIORef (buildingManagerRef env)
    when (any buildingTracked (bmInstances bm)) $
        atomicModifyIORef' (buildingManagerRef env) $ \bm' →
            (bm' { bmInstances = HM.map coolBuilding (bmInstances bm') }, ())
  where
    buildingTracked bi =
        biPage bi ≡ pageId
        ∧ (  any hasTrackedTemp (biStorage bi)
           ∨ any (any hasTrackedTemp) (biMaterialsDelivered bi) )
    coolBuilding bi
        | buildingTracked bi =
            let amb  = ambientAt (biAnchorX bi) (biAnchorY bi)
                cool = coolItem im amb dtGame
            in bi { biStorage            = map cool (biStorage bi)
                  , biMaterialsDelivered =
                        HM.map (map cool) (biMaterialsDelivered bi)
                  }
        | otherwise = bi
