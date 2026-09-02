-- | Chop designation commits (#97, re-keyed by #1854, re-shaped by
--   #1856).
--
--   Chop no longer commits a tile RECTANGLE. Its gesture is a
--   screen-space press-drag and its selection oracle is
--   "World.Flora.HitTest", which picks trees by where they are DRAWN —
--   so what reaches this module is already an exact list of plant
--   identities and there is no geometry left to filter here.
--
--   What this module still owns is the authority half: re-checking the
--   unchanged Chop predicate against live world state (a tree can be
--   felled, or start regrowing, between the gesture and the drain),
--   resolving each plant's tile and surface z from the resident chunk,
--   and routing every write through 'World.Flora.Designation''s single
--   owning operation so the durable map and the loaded
--   'fiChopDesignated' mirrors can never drift.
--
--   Split out of "World.Thread.Command.Cursor" (issue #564).
module World.Thread.Command.Cursor.Chop
    ( handleWorldDesignateChopInstancesCommand
    , handleWorldEraseChopInstancesCommand
    , handleWorldCancelChopCommand
    , handleWorldSetChopDesignateTextureCommand
    ) where

import UPrelude
import qualified Data.HashMap.Strict as HM
import Data.IORef (readIORef, atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle)
import Engine.Core.Capability.WorldSim
    (WorldSimCapability(..), toWorldSimCapability)
import Engine.Core.State (EngineEnv)
import Engine.Core.Log (logDebug, LogCategory(..), LoggerState)
import qualified Data.Vector.Unboxed as VU
import World.Types
import World.Generate (chunkToGlobal)
import World.Chop.Types (chopDesignationTile)
import World.Flora.Designation
    (designateChopInstances, setChopDesignations
    , cancelChopAtTile, cancelChopForInstance)
import World.Thread.Command.Cursor.Common
    (recordDesignationOutcome, recordMissingWorldOutcome)

-- | Designate exactly the named plants.
--
--   The list is the gesture's own answer ("World.Flora.HitTest" already
--   applied the same predicate against the frame the player was looking
--   at); re-checking it here is the live-state guard, not a second
--   selection rule. Eligibility is unchanged from the two-click
--   rectangle it replaces: a species with a harvest block whose tags
--   carry @tag@, and an instance with no live regrowth timer. It
--   deliberately does NOT consult the forage API's growth-window
--   @harvestable@ signal — a designated tree stays choppable as a
--   sprout or standing dead.
--
--   Idempotent: designating an already-designated plant rewrites the
--   same entry (requirement 4's set/clear symmetry).
handleWorldDesignateChopInstancesCommand
    ∷ EngineEnv → LoggerState → WorldPageId → [FloraInstanceId] → Text
    → IO ()
handleWorldDesignateChopInstancesCommand env logger pageId iids tag = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Nothing → recordMissingWorldOutcome env "chop.designate" pageId 0 0
        Just worldState → do
            resident ← residentPlants env worldState
            harvests ← readIORef (wsFloraHarvestsRef worldState)
            cat ← readIORef (wsFloraCatalogRef (toWorldSimCapability env))
            let entries =
                    [ (iid, gx, gy, z)
                    | iid ← iids
                    , Just (inst, gx, gy, z) ← [HM.lookup iid resident]
                    , Just sp ← [lookupSpecies (fiSpecies inst) cat]
                    , Just fh ← [fsHarvest sp]
                    , tag `elem` fhTags fh
                    , HM.lookupDefault 0 iid harvests ≤ 0
                    ]
            designateChopInstances worldState entries
            logDebug logger CatWorld $
                "Chop designation: +" <> tshow (length entries)
                <> " trees of " <> tshow (length iids) <> " selected"
            recordDesignationOutcome env "chop.designate"
                "no selected plant is a choppable tree for the requested tag"
                0 0 (length iids) (length entries)

-- | Clear exactly the named plants' designations.
--
--   Filtered by what is DESIGNATED, never by add-eligibility: a tree
--   that stopped qualifying while its designation stood must still be
--   clearable by the gesture that erases it (D-12). Idempotent — an id
--   that is not designated contributes nothing.
handleWorldEraseChopInstancesCommand
    ∷ EngineEnv → LoggerState → WorldPageId → [FloraInstanceId] → IO ()
handleWorldEraseChopInstancesCommand env logger pageId iids = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Nothing → recordMissingWorldOutcome env "chop.erase" pageId 0 0
        Just worldState → do
            desigs ← readIORef (wsChopDesignationsRef worldState)
            let cleared = [ (iid, chopDesignationTile cd, Nothing)
                          | iid ← iids
                          , Just cd ← [HM.lookup iid desigs] ]
            setChopDesignations worldState cleared
            logDebug logger CatWorld $
                "Chop designation: -" <> tshow (length cleared)
                <> " trees of " <> tshow (length iids) <> " selected"
            recordDesignationOutcome env "chop.erase"
                "no selected plant carried a chop designation"
                0 0 (length iids) (length cleared)

-- | Every plant in a resident chunk, indexed by identity, with the tile
--   it stands on and the surface z a marker/nearest-scan reads.
--
--   One pass over the resident chunks builds the whole index, so a
--   drag-box commit resolving fifty trees does not walk the world fifty
--   times.
residentPlants
    ∷ EngineEnv → WorldState
    → IO (HM.HashMap FloraInstanceId (FloraInstance, Int, Int, Int))
residentPlants _env worldState = do
    tileData ← readIORef (wsTilesRef worldState)
    pure $ HM.fromList
        [ (fiInstanceId i, (i, gx, gy, z))
        | lc ← HM.elems (wtdChunks tileData)
        , i ← fcdInstances (lcFlora lc)
        , let lx = fromIntegral (fiTileX i)
              ly = fromIntegral (fiTileY i)
              (gx, gy) = chunkToGlobal (lcCoord lc) lx ly
              z = lcSurfaceMap lc VU.! columnIndex lx ly
        ]

handleWorldCancelChopCommand ∷ EngineEnv → LoggerState → WorldPageId
    → Int → Int → Maybe FloraInstanceId → IO ()
handleWorldCancelChopCommand env _logger pageId gx gy mIid = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState → case mIid of
            -- #1854: the felling acolyte cancels EXACTLY the plant it
            -- claimed, so a second designated tree on the same tile
            -- stays designated for whoever claims it next.
            Just iid → cancelChopForInstance worldState iid
            -- The AI's tile-granularity fallback, for a restored job
            -- that knows its tile but not which plant it had claimed.
            -- The PLAYER's erase gesture is exact-identity
            -- ('handleWorldEraseChopInstancesCommand') and never comes
            -- through here.
            Nothing → cancelChopAtTile worldState gx gy
        Nothing → pure ()

handleWorldSetChopDesignateTextureCommand ∷ EngineEnv → LoggerState
    → WorldPageId → TextureHandle → IO ()
handleWorldSetChopDesignateTextureCommand env _logger pageId tid = do
    mgr ← readIORef (wsWorldManagerRef (toWorldSimCapability env))
    case lookup pageId (wmWorlds mgr) of
        Just worldState →
            atomicModifyIORef' (wsCursorRef worldState) $ \cs →
                (cs { chopDesignTexture = Just tid }, ())
        Nothing → pure ()
