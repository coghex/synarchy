{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
-- | Regression for #367: a tile selection honours an explicit z.
--
--   The Info-tool left-click resolves the tile under the cursor at the
--   active z-slice ('world.pickTile', which now returns that z) and
--   passes it to 'world.selectTile'. Below the surface that z is NOT the
--   column top, so dropping it — the old behaviour — silently selected
--   the surface tile instead of the clicked one (the reported bug).
--
--   'WorldSelectTileByCoord' now carries a @Maybe Int@: @Just z@ pins the
--   clicked tile; @Nothing@ keeps the surface-z default used by the
--   right-click context-menu "Info" path (which has no live pick). This
--   spec drives the real command handler against the shared 42/64/3
--   world and asserts both arms. It only touches that world's cursor
--   selection (not its tiles/edits), so sharing the read-only world is
--   safe.
module Test.Headless.World.SelectTileZ (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (readIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Unboxed as VU
import Engine.Core.State (EngineEnv(..))
import World.Tile.Types (WorldTileData(..))
import World.Chunk.Types (LoadedChunk(..), columnIndex)
import World.Generate.Coordinates (chunkToGlobal)
import World.Cursor.Types (CursorState(..))
import World.State.Types (WorldState(..))
import World.Page.Types (WorldPageId(..))
import World.Thread.Command.Cursor (handleWorldSelectTileByCoordCommand)
import Test.Headless.Harness (sharedWorld, getWorldTileData)

spec ∷ SpecWith EngineEnv
spec = describe "tile selection z (#367)" $
    it "Just z pins the clicked tile; Nothing falls back to surface z" $ \env → do
        ws     ← sharedWorld env 42 64 3
        -- The handler ignores the logger, but takes a real one.
        logger ← readIORef (loggerRef env)
        -- Reuse the shared world's page id (mirrors Harness.sharedWorld).
        let pid = WorldPageId "shared_42_64_3"
        tiles  ← getWorldTileData ws
        case HM.toList (wtdChunks tiles) of
            [] → expectationFailure "shared world has no loaded chunks"
            ((coord, lc) : _) → do
                let (gx, gy) = chunkToGlobal coord 0 0
                    surfZ    = lcSurfaceMap lc VU.! columnIndex 0 0
                    belowZ   = surfZ - 2   -- a distinct tile below the top

                -- Nothing → surface z (right-click context-menu path).
                handleWorldSelectTileByCoordCommand env logger pid gx gy Nothing
                sel0 ← worldSelectedTile <$> readIORef (wsCursorRef ws)
                sel0 `shouldBe` Just (gx, gy, surfZ)

                -- Just z → the clicked tile, even below the surface. The
                -- old code snapped this to surfZ — that is the #367 bug.
                handleWorldSelectTileByCoordCommand env logger pid gx gy (Just belowZ)
                sel1 ← worldSelectedTile <$> readIORef (wsCursorRef ws)
                sel1 `shouldBe` Just (gx, gy, belowZ)
