{-# LANGUAGE Strict #-}
-- | The page-level face of the finite-arena bounds (#2708): read a
--   page's OWN bounds and refuse an operation on a tile outside them.
--
--   Every authoritative mutation handler resolves its target page by id
--   from the world manager — visible or not — and asks THIS page, never
--   the active one, so an edit aimed at a hidden finite arena is judged
--   against that arena's rectangle even while another page is on screen.
module World.Arena.Page
    ( pageArenaBounds
    , tileAdmittedOnPage
    , whenTileAdmitted
    , outsideArenaText
    ) where

import UPrelude
import Data.IORef (readIORef)
import Engine.Core.Log (LogCategory(..), LoggerState, logWarn)
import Scenario.Bounds (TileBounds(..))
import World.Arena.Bounds (ArenaBounds, arenaAdmitsTile, arenaRect)
import World.Generate.Types (WorldGenParams(..))
import World.State.Types (WorldState(..))

-- | This page's bounds: 'Nothing' for an expandable arena, a generated
--   world, or a page that has no generation params yet.
pageArenaBounds ∷ WorldState → IO (Maybe ArenaBounds)
pageArenaBounds ws = (wgpArenaBounds =≪) <$> readIORef (wsGenParamsRef ws)

-- | May terrain exist at this engine tile of this page?
tileAdmittedOnPage ∷ WorldState → Int → Int → IO Bool
tileAdmittedOnPage ws gx gy = do
    mb ← pageArenaBounds ws
    pure (arenaAdmitsTile mb gx gy)

-- | Run the action when the tile is inside the page's bounds; otherwise
--   warn, naming the verb, the tile and the rectangle, and do NOTHING —
--   no edit appended, no object created, no terrain made. An expandable
--   page admits every tile, so the extra read is the whole cost there.
whenTileAdmitted ∷ LoggerState → Text → WorldState → Int → Int → IO () → IO ()
whenTileAdmitted logger verb ws gx gy action = do
    mb ← pageArenaBounds ws
    if arenaAdmitsTile mb gx gy
        then action
        else logWarn logger CatWorld $ verb <> " refused: " <> outsideArenaText mb gx gy

-- | "tile (x,y) is outside the finite arena X=a..b Y=c..d".
outsideArenaText ∷ Maybe ArenaBounds → Int → Int → Text
outsideArenaText mb gx gy = "tile (" <> tshow gx <> "," <> tshow gy
    <> ") is outside the finite arena" <> case mb of
        Nothing → ""
        Just ab →
            let TileBounds x0 y0 x1 y1 = arenaRect ab
            in " X=" <> tshow x0 <> ".." <> tshow x1
               <> " Y=" <> tshow y0 <> ".." <> tshow y1
