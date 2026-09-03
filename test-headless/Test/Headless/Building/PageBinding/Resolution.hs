{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Page RESOLUTION for the "Build placement page binding" (#1602)
--   gate: the fixture discriminators the rest of the gate leans on,
--   what @world.pickTile@ reports, the single-resolution guarantee
--   'building.canPlaceAt' and @building.setGhost@ owe one call, and the
--   two distinct empty-visible rejection states.
--
--   These are fixture-CONSUMING fragments: the engine, the Lua backend
--   and the isolated resource root are the façade's
--   ("Test.Headless.Building.PageBinding"), and nothing here starts a
--   lifecycle of its own.
module Test.Headless.Building.PageBinding.Resolution
    ( fixtureSpec
    , pickBindingSpec
    , apiCoherenceSpec
    , emptyVisibleSpec
    ) where

import UPrelude
import Test.Hspec
import Data.IORef (readIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Building.Types (BuildingGhost(..))
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Building.PageBinding.Support
    ( aimAt, aliasOfA, canPlaceAt, clearStubs, evalDebug, ghostOf
    , insideLocA, insideLocB, occupiedA, occupiedB, onlyLoadedOnB
    , pageA, pageB, placeTile, portalName, resetHiddenOnly, resetNoWorlds
    , resetScene, selectionGen, shedName, sizeA, sizeB, terrainZA
    , terrainZB, tilesA, tilesB )
import World.Chunk.Types (ChunkCoord(..))
import World.Generate.Coordinates (canonicalTile)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..))
import World.Thread.Command.UI
    (handleWorldHideCommand, handleWorldShowCommand)
import World.Tile.Types (WorldTileData(..))

-- | The discriminators the rest of the module leans on really do
--   discriminate. Without this, a coherence assertion could pass
--   against a fixture where both pages happen to agree.
fixtureSpec ∷ SpecWith (EngineEnv, LuaBackendState)
fixtureSpec = describe "the two fixture pages really differ" $ do

    it "canonicalises page A's alias differently under each world size" $
        \_ → do
            let aliased = aliasOfA occupiedA
            uncurry (canonicalTile sizeA) aliased `shouldBe` occupiedA
            uncurry (canonicalTile sizeB) aliased `shouldNotBe` occupiedA

    it "loads a chunk on page B that page A does not have" $ \_ → do
        let cc = ChunkCoord 0 1
        HM.member cc (wtdChunks tilesA) `shouldBe` False
        HM.member cc (wtdChunks tilesB) `shouldBe` True

    it "gives the two pages different terrain elevations" $ \_ →
        terrainZA `shouldNotBe` terrainZB

pickBindingSpec ∷ SpecWith (EngineEnv, LuaBackendState)
pickBindingSpec =
  describe "world.pickTile reports the page it hit-tested (#1602 r1)" $ do

    it "returns the visible page id and its selection generation" $
        \(env, ls) → do
            _ ← resetScene env
            _ ← clearStubs ls
            gen ← selectionGen env
            (px, py) ← aimAt env placeTile terrainZA
            got ← evalDebug ls $ T.concat
                [ "local gx, gy, gz, page, g = world.pickTile("
                , tshow px, ", ", tshow py, "); "
                , "return tostring(gx) .. '|' .. tostring(gy) .. '|' "
                , "  .. tostring(page) .. '|' .. tostring(g)" ]
            got `shouldBe` T.concat
                [ tshow (fst placeTile), "|", tshow (snd placeTile)
                , "|", unWorldPageId pageA, "|", tshow gen ]

    it "reports a DIFFERENT generation after page selection moves, even \
       \back to the same page (A→B→A)" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        before' ← selectionGen env
        _ ← evalDebug ls "__pageSwitch('aba'); return 'switched'"
        mgr ← readIORef (worldManagerRef env)
        -- Same page id, different generation: a page-id comparison
        -- would see nothing at all here.
        wmVisible mgr `shouldBe` [pageA]
        wmSelectionGen mgr `shouldNotBe` before'

    it "does not move the generation when a show/hide changes nothing" $
        \(env, _) → do
            _ ← resetScene env
            logger ← readIORef (loggerRef env)
            let wsc = toWorldSimCapability env
            before' ← selectionGen env
            -- Page A is already visible; page B is already hidden.
            handleWorldShowCommand wsc logger pageA
            handleWorldHideCommand wsc logger pageB
            selectionGen env `shouldReturn` before'

apiCoherenceSpec ∷ SpecWith (EngineEnv, LuaBackendState)
apiCoherenceSpec = describe "one resolution answers the whole call" $ do

  describe "building.canPlaceAt (#1602 r3)" $ do

    it "filters occupancy to the VISIBLE page" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        -- Page A's own occupant blocks; page B's, at a different tile,
        -- must not.
        canPlaceAt ls shedName occupiedA Nothing
            `shouldReturn` "false|tile already occupied|false"
        canPlaceAt ls shedName occupiedB Nothing
            `shouldReturn` "true|nil|false"

    it "reads world size, occupancy and terrain from the SAME page" $
        \(env, ls) → do
            _ ← resetScene env
            _ ← clearStubs ls
            -- The alias canonicalises onto page A's occupied tile under
            -- page A's world size, and onto an unloaded chunk under page
            -- B's. One answer therefore names which page supplied the
            -- size, the occupancy filter AND the terrain.
            canPlaceAt ls shedName (aliasOfA occupiedA) Nothing
                `shouldReturn` "false|tile already occupied|false"

    it "reads terrain from the VISIBLE page, not a registered one" $
        \(env, ls) → do
            _ ← resetScene env
            _ ← clearStubs ls
            canPlaceAt ls shedName onlyLoadedOnB Nothing
                `shouldReturn` "false|chunk not loaded|false"

    it "reads placed locations from the VISIBLE page" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        canPlaceAt ls portalName insideLocA Nothing
            `shouldReturn` "false|inside a location's bounds|false"
        canPlaceAt ls portalName insideLocB Nothing
            `shouldReturn` "true|nil|false"

    it "accepts a binding that still holds" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        canPlaceAt ls shedName placeTile (Just (pageA, gen))
            `shouldReturn` "true|nil|false"

    it "refuses a binding naming a page that is no longer visible, even \
       \at the current generation" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        gen ← selectionGen env
        -- The generation half alone would accept this (it has not
        -- moved); the page half is what refuses it, so a supplied id is
        -- never taken and then quietly ignored.
        canPlaceAt ls shedName placeTile (Just (pageB, gen))
            `shouldReturn` "false|page binding stale|true"

  describe "building.setGhost (#1602 r9)" $ do

    it "canonicalises by the visible page's world size AND elevates from \
       \that same page's terrain" $ \(env, ls) → do
        _ ← resetScene env
        _ ← clearStubs ls
        let aliased = aliasOfA placeTile
        _ ← evalDebug ls $ T.concat
            [ "building.setGhost('", shedName, "', "
            , tshow (fst aliased), ", ", tshow (snd aliased)
            , ", true); return 'set'" ]
        ghost ← ghostOf env
        fmap (\g → (bgGridX g, bgGridY g, bgGridZ g)) ghost
            `shouldBe` Just (fst placeTile, snd placeTile, terrainZA)

emptyVisibleSpec ∷ SpecWith (EngineEnv, LuaBackendState)
emptyVisibleSpec =
  describe "empty-visible behaviour is unchanged (#1602 r10)" $ do

    it "canPlaceAt says 'no active world' with NO page registered" $
        \(env, ls) → do
            resetNoWorlds env
            _ ← clearStubs ls
            canPlaceAt ls shedName placeTile Nothing
                `shouldReturn` "false|no active world|false"

    it "canPlaceAt says 'no world loaded' with a page registered but \
       \none visible" $ \(env, ls) → do
        resetHiddenOnly env
        _ ← clearStubs ls
        -- The registered-but-hidden page is NOT silently used: its
        -- terrain would have made this placeable.
        canPlaceAt ls shedName placeTile Nothing
            `shouldReturn` "false|no world loaded|false"

    it "setGhost falls back to unwrapped coordinates and elevation 0" $
        \(env, ls) → do
            resetHiddenOnly env
            _ ← clearStubs ls
            let aliased = aliasOfA placeTile
            _ ← evalDebug ls $ T.concat
                [ "building.setGhost('", shedName, "', "
                , tshow (fst aliased), ", ", tshow (snd aliased)
                , ", true); return 'set'" ]
            ghost ← ghostOf env
            fmap (\g → (bgGridX g, bgGridY g, bgGridZ g)) ghost
                `shouldBe` Just (fst aliased, snd aliased, 0)
