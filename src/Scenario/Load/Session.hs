{-# LANGUAGE Strict #-}
-- | The empty replacement shell a scenario load publishes (#2713, SCN-13
--   of epic #2698), and the question of whether the live session may be
--   replaced at all.
--
--   A scenario replaces the TESTING session (D-11). It never replaces an
--   ordinary gameplay session, so the first thing the service asks is
--   whether the live session is a testing one ('testingSessionVerdict').
--
--   The shell is one fresh arena page at the scenario's bounds and nothing
--   else — a session exactly like the one the developer arena opens with,
--   assembled as the 'World.Types.SaveData' the load transaction already
--   stages and publishes. Reusing that transaction is what brings the
--   barrier, the owner parking, the queue cutover, the allocator and
--   page-incarnation reset and the Lua reconciliation with it; the
--   scenario's CONTENT is installed into the published page afterwards
--   ("Scenario.Load.Construct").
module Scenario.Load.Session
    ( testingSessionVerdict
    , arenaPageId
    , ShellSpec(..)
    , defaultFlatShell
    , shellSaveData
    ) where

import UPrelude
import Data.IORef (readIORef)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Engine.Core.Capability.WorldSim (WorldSimCapability(..))
import Engine.Core.SessionEpoch (freshSessionGameTime)
import Engine.Graphics.Camera (CameraFacing(..))
import Item.Knowledge (emptyPortableKnowledge)
import Unit.Faction.RelationStore (emptyRelationCauseStore)
import Structure.Palette (emptyTexPalette)
import World.Arena.Bounds (ArenaBounds)
import World.Generate.Types
    (WorldGenParams(..), defaultWorldGenParams, isArenaParams)
import World.Page.GeneratedId (newGeneratedWorldId)
import World.Page.Types (WorldPageId(..))
import World.Save.Component.Page (blankPageSnapshot)
import World.Save.Snapshot
    (LiveCameraSnapshot(..), PageSnapshot(..), SessionSnapshot(..))
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotToSaveData)
import World.State.Types (WorldManager(..), WorldState(..))
import World.Time.Types
    (WorldDate(..), WorldTime(..), defaultWorldDate, defaultWorldTime)
import World.Types (SaveData)

-- | The page the developer arena lives on (@scripts/test_arena.lua@), and
--   so the page a scenario's arena replaces.
arenaPageId ∷ WorldPageId
arenaPageId = WorldPageId "test_arena"

-- | May the live session be replaced by a scenario? Yes when it holds no
--   page (a direct boot into a scenario) or only arena pages. Any other
--   page — a generated world, or a page whose generation parameters are
--   not yet known — is ordinary gameplay, and the whole-session
--   replacement would destroy it.
testingSessionVerdict ∷ WorldSimCapability → IO (Either Text ())
testingSessionVerdict wsc = do
    mgr ← readIORef (wsWorldManagerRef wsc)
    verdicts ← forM (wmWorlds mgr) $ \(pid, ws) → do
        params ← readIORef (wsGenParamsRef ws)
        pure (pid, maybe False isArenaParams params)
    pure $ case [ pid | (pid, False) ← verdicts ] of
        [] → Right ()
        bad → Left $ "the live session is not a testing session: it holds the \
                     \ordinary gameplay page(s) "
                     <> T.intercalate ", " (map unWorldPageId bad)
                     <> "; a scenario replaces only the developer arena"

-- | What the shell is built from.
data ShellSpec = ShellSpec
    { shBounds ∷ !(Maybe ArenaBounds)
      -- ^ the scenario's finite map, or 'Nothing' for the expandable arena
    , shCamera ∷ !(Float, Float)
      -- ^ the initial camera position (D-21): placement only, never bounds
    } deriving (Show, Eq)

-- | The default flat arena: expandable, camera at the origin. What
--   recovery publishes.
defaultFlatShell ∷ ShellSpec
defaultFlatShell = ShellSpec { shBounds = Nothing, shCamera = (0, 0) }

-- | The shell as the data the load transaction stages. The page's fields
--   are the ones a fresh developer arena starts with ('blankPageSnapshot''s
--   placeholders replaced by the live page defaults), its identity is a
--   freshly minted generated-world id, every allocator starts at its first
--   id, game time starts at the session epoch, and the session is paused
--   (a published load is always paused).
shellSaveData ∷ ShellSpec → IO SaveData
shellSaveData spec = do
    gid ← newGeneratedWorldId
    let params = defaultWorldGenParams
            { wgpSeed = 0
            , wgpWorldSize = 100000
            , wgpArenaBounds = shBounds spec }
        (cx, cy) = shCamera spec
        date = defaultWorldDate
        page = (blankPageSnapshot arenaPageId params)
            { pgsCameraX = cx, pgsCameraY = cy
            , pgsTimeHour = wtHour defaultWorldTime
            , pgsTimeMinute = wtMinute defaultWorldTime
            , pgsDateYear = wdYear date
            , pgsDateMonth = wdMonth date
            , pgsDateDay = wdDay date
            , pgsGeneratedId = Just gid }
        snap = SessionSnapshot
            { snapGameTime = freshSessionGameTime
            , snapTexPalette = emptyTexPalette
            , snapNextItemId = 1
            , snapNextBuildingId = 1
            , snapNextUnitId = 1
            , snapActivePage = arenaPageId
            , snapVisiblePages = [arenaPageId]
            , snapLiveCamera = LiveCameraSnapshot (Just arenaPageId) cx cy 0.5 FaceSouth
            , snapPages = HM.singleton arenaPageId page
            , snapItemTags = mempty
            , snapItemRefs = mempty
            , snapPortableKnowledge = emptyPortableKnowledge
            , snapRelationCauses = emptyRelationCauseStore }
    pure (snapshotToSaveData (SaveRequestMeta "scenario" "" False) snap)
