{-# LANGUAGE OverloadedStrings #-}
-- | "arena finite bounds" — units (#2708 requirement 4).
--
--   Two halves. The PURE half drives the real per-unit tick
--   ('tickUnit') over a snapshot built from the production generator,
--   from every edge and corner of a 20×10 map outward, in both hazard
--   policies, in small and enormous time steps, diagonally, across a
--   leap and a fall, and over a snapshot that has no chunks at all: a
--   unit must END every tick inside the rectangle.
--
--   The ENGINE half drives the real command handlers and the real
--   'tickAllMovement' against a finite arena page that is HIDDEN while
--   another page is on screen: direct positioning, orders, leaps and
--   spawning outside the map are refused with no effect, and the page's
--   own rectangle — not the visible page's — is what decides.
module Test.Headless.World.ArenaFiniteBounds.Movement (pureSpec, engineSpec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Engine.Core.Capability.UnitCombat (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.State (EngineEnv(..))
import GHC.Clock (getMonotonicTimeNSec)
import Test.Headless.Harness (sendWorldCommand, waitForWorldInit)
import Test.Headless.Unit.SimPageOwnership (baseSimState, minimalDef, mkUnit)
import qualified Data.Text as T
import Unit.Command.Types (SpawnProfile(..))
import Unit.Faction (Faction(..))
import Unit.Faction.Membership (resolveLegacyFaction)
import Unit.Pathing.Cost (defaultPathingConfig)
import Unit.Sim.Types
import Unit.Thread.Command.Lifecycle (handleUnitTeleportCommand)
import Unit.Thread.Command.Motion (handleUnitJumpCommand, handleUnitMoveToCommand)
import Unit.Thread.Command.Spawn (handleUnitSpawnCommand)
import Unit.Thread.Movement (tickAllMovement)
import Unit.Thread.Movement.Fall (startFall)
import Unit.Thread.Movement.Leap (startJump)
import Unit.Thread.Movement.PathAdvance (MoveWorld(..), snapshotOwnedWorldTiles, tickUnit)
import qualified Data.HashSet as HS
import Engine.Core.Capability.WorldSim (toWorldSimCapability)
import Unit.Thread.Movement.Types (defaultMoveStats)
import Unit.Types (UnitId(..), UnitInstance(..), UnitManager(..))
import World.Arena.Bounds
    (ArenaBounds, arenaAdmitsPosition, mkArenaBounds)
import World.Chunk.Admit (pageIncarnation)
import World.Constants (seaLevel)
import World.Generate.Arena (generateArenaChunksIn)
import World.Material (emptyMaterialRegistry)
import World.Tile.Types (WorldTileData(..), emptyWorldTileData)
import World.Types
    ( LoadedChunk(..), WorldCommand(..), WorldPageId(..), WorldState(..) )

bounds ∷ Int → Int → ArenaBounds
bounds w h = either (error . show) id (mkArenaBounds w h)

-- | The 20×10 map: X −10…9, Y −4…5.
ab2010 ∷ ArenaBounds
ab2010 = bounds 20 10

-- | A movement snapshot of the map, built from the production generator.
mapWorld ∷ ArenaBounds → MoveWorld
mapWorld ab = MoveWorld
    { mwTiles = Just WorldTileData
        { wtdChunks = HM.fromList [ (lcCoord c, c) | c ← generateArenaChunksIn (Just ab) 0 ]
        , wtdMaxChunks = 100
        , wtdBounds = Just ab }
    , mwOwnPage = True }

-- | A page whose bounds are known but whose terrain is not (yet) there.
emptyMapWorld ∷ ArenaBounds → MoveWorld
emptyMapWorld ab = MoveWorld
    { mwTiles = Just emptyWorldTileData { wtdBounds = Just ab }
    , mwOwnPage = True }

unitAt ∷ Float → Float → UnitSimState
unitAt x y = baseSimState
    { usRealX = x, usRealY = y, usGridZ = seaLevel
    , usRealZ = fromIntegral seaLevel }

headFor ∷ MoveHazardPolicy → Float → Float → Float → UnitSimState → UnitSimState
headFor hazard speed tx ty us = us
    { usTarget = Just (MoveTarget tx ty speed hazard), usState = Walking }

-- | Every state of a run, the start included.
run ∷ MoveWorld → Double → Int → UnitSimState → [UnitSimState]
run mw dt k s0 = map snd (take (k + 1) (iterate step (1000, s0)))
  where
    step (t, s) = let t' = t + dt
                  in (t', tickUnit defaultPathingConfig emptyMaterialRegistry t' dt
                                  mw defaultMoveStats s)

inMap ∷ ArenaBounds → UnitSimState → Bool
inMap ab s = arenaAdmitsPosition (Just ab) (usRealX s) (usRealY s)

-- | Where each unit starts and the outside point it is sent to: the
--   four edge midpoints and the four corners of the 20×10 map.
edgeRuns ∷ [(String, (Float, Float), (Float, Float))]
edgeRuns =
    [ ("east",  (9.5, 0.5),   (14.5, 0.5))
    , ("west",  (-9.5, 0.5),  (-14.5, 0.5))
    , ("north", (0.5, 5.5),   (0.5, 9.5))
    , ("south", (0.5, -3.5),  (0.5, -8.5))
    , ("north-east", (9.5, 5.5),   (13.5, 9.5))
    , ("north-west", (-9.5, 5.5),  (-13.5, 9.5))
    , ("south-east", (9.5, -3.5),  (13.5, -7.5))
    , ("south-west", (-9.5, -3.5), (-13.5, -7.5)) ]

pureSpec ∷ Spec
pureSpec = describe "a unit on the map" $ do

    it "never ends a tick outside, walking out from every edge and corner, in both hazard policies" $
        forM_ [FallPermitted, FallProhibited] $ \hazard →
            forM_ edgeRuns $ \(name, (sx, sy), (tx, ty)) → do
                let states = run (mapWorld ab2010) 0.05 160
                                 (headFor hazard 3 tx ty (unitAt sx sy))
                (name, hazard, filter (not . inMap ab2010) states)
                    `shouldBe` (name, hazard, [])

    it "is not carried out by an enormous time step" $
        forM_ [FallPermitted, FallProhibited] $ \hazard →
            forM_ edgeRuns $ \(name, (sx, sy), (tx, ty)) → do
                let states = run (mapWorld ab2010) 100 6
                                 (headFor hazard 50 tx ty (unitAt sx sy))
                (name, hazard, filter (not . inMap ab2010) states)
                    `shouldBe` (name, hazard, [])

    it "is not carried out by a diagonal step at the corner, fast and slow" $
        forM_ [0.016, 0.5, 5] $ \dt → forM_ [FallPermitted, FallProhibited] $ \hazard → do
            let states = run (mapWorld ab2010) dt 40
                             (headFor hazard 8 14 10 (unitAt 9.9 5.9))
            (dt, hazard, filter (not . inMap ab2010) states) `shouldBe` (dt, hazard, [])

    it "walks an ordinary order across the map and arrives, never leaving it" $ do
        let states = run (mapWorld ab2010) 0.05 400
                         (headFor FallPermitted 4 8.5 4.5 (unitAt (-9.5) (-3.5)))
            final = last states
        filter (not . inMap ab2010) states `shouldBe` []
        usTarget final `shouldBe` Nothing
        (usRealX final, usRealY final) `shouldBe` (8.5, 4.5)

    it "walks an order that ends at an edge tile and stops on it" $ do
        let states = run (mapWorld ab2010) 0.05 400
                         (headFor FallPermitted 4 9.5 5.5 (unitAt 0.5 0.5))
            final = last states
        filter (not . inMap ab2010) states `shouldBe` []
        (usRealX final, usRealY final) `shouldBe` (9.5, 5.5)

    it "stays inside when the terrain snapshot has no chunks yet — the outside is no fallback floor" $
        forM_ [FallPermitted, FallProhibited] $ \hazard → do
            let states = run (emptyMapWorld ab2010) 0.05 100
                             (headFor hazard 3 14.5 0.5 (unitAt 9.5 0.5))
            filter (not . inMap ab2010) states `shouldBe` []

    it "ends a tick idle at the nearest inside point with its request dropped when held in" $ do
        let once = last (run (mapWorld ab2010) 100 1
                             (headFor FallPermitted 50 14.5 0.5 (unitAt 9.5 0.5)))
        usTarget once `shouldBe` Nothing
        usState once `shouldBe` Idle
        usLocalPath once `shouldBe` []
        inMap ab2010 once `shouldBe` True
        usRealX once `shouldSatisfy` (\x → x ≥ 9.0 ∧ x < 10.0)

    it "is not carried out by a leap whose landing lies outside" $ do
        let leaping = startJump 1000 (unitAt 9.5 0.5) 12 0
            states = run (mapWorld ab2010) (1 / 30) 240 leaping
        filter (not . inMap ab2010) states `shouldBe` []

    it "is not carried out by a fall whose landing lies outside" $ do
        let falling = startFall 1000 (unitAt 9.5 0.5) ((12, 0), seaLevel - 3) seaLevel (1, 0)
            states = run (mapWorld ab2010) (1 / 30) 240 falling
        filter (not . inMap ab2010) states `shouldBe` []

    it "picks a unit that starts outside (a damaged save) back inside on its first tick" $
        forM_ [ (30.5, 0.5), (-30.5, 20.5), (0.5, -100.5), (1.0e9, 1.0e9) ] $ \(x, y) → do
            let states = run (mapWorld ab2010) 0.05 1 (unitAt x y)
            inMap ab2010 (last states) `shouldBe` True

    it "does not touch a unit on an expandable page: it walks past where a finite map would end" $ do
        let open = MoveWorld
                { mwTiles = Just WorldTileData
                    { wtdChunks = HM.fromList [ (lcCoord c, c) | c ← generateArenaChunksIn Nothing 0 ]
                    , wtdMaxChunks = 100, wtdBounds = Nothing }
                , mwOwnPage = True }
            states = run open 0.05 200 (headFor FallPermitted 3 20.5 0.5 (unitAt 0.5 0.5))
        -- the same walk the 20×10 map stops at tile 9
        usRealX (last states) `shouldBe` 20.5
        usTarget (last states) `shouldBe` Nothing

-- * The engine half

-- | A page id unique to the call AND to its role: two ids taken a few
--   nanoseconds apart can read the same clock tick, and a page id is a
--   reusable NAME — a second init under it would silently REPLACE the
--   first page.
arenaPage ∷ T.Text → IO WorldPageId
arenaPage role = do
    n ← getMonotonicTimeNSec
    pure (WorldPageId (T.pack ("finite_units_" <> T.unpack role <> "_" <> show n)))

-- | A finite 20×10 arena page that stays HIDDEN, and a second, EXPANDABLE
--   page that is shown — the visible one must not decide anything for
--   the hidden one's units.
withHiddenFinite ∷ EngineEnv → (WorldPageId → WorldState → IO α) → IO α
withHiddenFinite env act = do
    pid ← arenaPage "hidden"
    other ← arenaPage "open"
    sendWorldCommand env (WorldInitArena pid (Just ab2010))
    ws ← waitForWorldInit env pid 60
    sendWorldCommand env (WorldInitArena other Nothing)
    _ ← waitForWorldInit env other 60
    sendWorldCommand env (WorldInitArenaDone other)
    -- the unit manager knows the one definition the fixtures use
    atomicModifyIORef' (unitManagerRef env) $ \um →
        (um { umDefs = HM.insert "acolyte" minimalDef (umDefs um) }, ())
    r ← act pid ws
    sendWorldCommand env (WorldDestroy pid)
    sendWorldCommand env (WorldDestroy other)
    pure r

-- | Install a unit on the page (instance and sim state) at a position.
install ∷ EngineEnv → IORef UnitThreadState → WorldPageId → UnitId → Float → Float → IO ()
install env utsRef pid uid x y = do
    atomicModifyIORef' (unitManagerRef env) $ \um →
        ( um { umInstances = HM.insert uid
                 ((mkUnit pid []) { uiGridX = x, uiGridY = y }) (umInstances um) }, () )
    atomicModifyIORef' utsRef $ \uts →
        (uts { utsSimStates = HM.insert uid (unitAt x y) (utsSimStates uts) }, ())

simOf ∷ IORef UnitThreadState → UnitId → IO (Maybe UnitSimState)
simOf utsRef uid = HM.lookup uid . utsSimStates ⊚ readIORef utsRef

engineSpec ∷ SpecWith EngineEnv
engineSpec = describe "the command handlers and the movement tick, on a hidden finite page" $ do

    it "teleports a unit to any tile of the map, corners included, and refuses the first tile beyond" $ \env →
        withHiddenFinite env $ \pid _ → do
            utsRef ← newIORef emptyUnitThreadState
            let uid = UnitId 8201
            install env utsRef pid uid 0.5 0.5
            forM_ [ (9.5, 5.5), (-9.5, -3.5), (9.5, -3.5), (-9.5, 5.5), (0.5, 0.5) ] $ \(x, y) → do
                handleUnitTeleportCommand env utsRef uid x y Nothing
                fmap (\s → (usRealX s, usRealY s)) ⊚ simOf utsRef uid
                    `shouldReturn` Just (x, y)
            handleUnitTeleportCommand env utsRef uid 0.5 0.5 Nothing
            forM_ [ (10.0, 0.5), (10.5, 0.5), (-10.001, 0.5), (0.5, 6.0), (0.5, -4.001)
                  , (10.5, 6.5), (1.0e30, 0.5) ] $ \(x, y) → do
                handleUnitTeleportCommand env utsRef uid x y Nothing
                fmap (\s → (usRealX s, usRealY s)) ⊚ simOf utsRef uid
                    `shouldReturn` Just (0.5, 0.5)

    it "grounds an inside teleport on the page's own surface, not a fallback" $ \env →
        withHiddenFinite env $ \pid _ → do
            utsRef ← newIORef emptyUnitThreadState
            let uid = UnitId 8202
            install env utsRef pid uid 0.5 0.5
            handleUnitTeleportCommand env utsRef uid 3.5 2.5 Nothing
            fmap usGridZ ⊚ simOf utsRef uid `shouldReturn` Just seaLevel

    it "refuses an order to a point outside the map and accepts one inside" $ \env →
        withHiddenFinite env $ \pid _ → do
            utsRef ← newIORef emptyUnitThreadState
            let uid = UnitId 8203
            install env utsRef pid uid 0.5 0.5
            handleUnitMoveToCommand env utsRef uid 14.5 0.5 2 FallPermitted
            (usTarget =<<) ⊚ simOf utsRef uid `shouldReturn` Nothing
            handleUnitMoveToCommand env utsRef uid 9.5 5.5 2 FallProhibited
            (fmap (\t → (mtTargetX t, mtTargetY t)) . usTarget =<<) ⊚ simOf utsRef uid
                `shouldReturn` Just (9.5, 5.5)

    it "refuses a leap that would land outside and starts one that lands inside" $ \env →
        withHiddenFinite env $ \pid _ → do
            utsRef ← newIORef emptyUnitThreadState
            let uid = UnitId 8204
            install env utsRef pid uid 8.5 0.5
            handleUnitJumpCommand env utsRef uid 10 0
            fmap usState ⊚ simOf utsRef uid `shouldReturn` Just Idle
            handleUnitJumpCommand env utsRef uid 9 0
            fmap (isTransition . usState) ⊚ simOf utsRef uid `shouldReturn` Just True

    it "creates no unit when asked to spawn outside the map and one when inside" $ \env →
        withHiddenFinite env $ \pid ws → do
            utsRef ← pure (ucUtsRef (toUnitCombatCapability env))
            epoch ← pageIncarnation ws
            let faction = resolveLegacyFaction [] FactionPlayer
                spawn raw x y = handleUnitSpawnCommand env utsRef (UnitId raw)
                    "acolyte" x y seaLevel faction pid epoch SpawnStandard
                present raw = HM.member (UnitId raw) ∘ umInstances ⊚ readIORef (unitManagerRef env)
                simPresent raw = HM.member (UnitId raw) ∘ utsSimStates ⊚ readIORef utsRef
            spawn 8301 12.5 0.5
            spawn 8302 0.5 6.5
            spawn 8303 (-10.5) 0.5
            (,) ⊚ present 8301 ⊛ simPresent 8301 `shouldReturn` (False, False)
            (,) ⊚ present 8302 ⊛ simPresent 8302 `shouldReturn` (False, False)
            (,) ⊚ present 8303 ⊛ simPresent 8303 `shouldReturn` (False, False)
            spawn 8304 9.5 5.5
            present 8304 `shouldReturn` True

    it "holds a unit on the hidden page inside through the real movement tick" $ \env →
        withHiddenFinite env $ \pid _ → do
            -- the movement snapshot of the hidden page carries its rectangle
            snaps ← snapshotOwnedWorldTiles (toWorldSimCapability env) (HS.singleton pid)
            (wtdBounds <$> HM.lookup pid snaps) `shouldBe` Just (Just ab2010)
            utsRef ← newIORef emptyUnitThreadState
            let uid = UnitId 8205
            install env utsRef pid uid 9.5 0.5
            -- a target the handler would refuse, planted directly: the
            -- tick itself must still contain the unit
            atomicModifyIORef' utsRef $ \uts →
                ( uts { utsSimStates = HM.adjust (headFor FallPermitted 6 20.5 0.5)
                                         uid (utsSimStates uts) }, () )
            writeIORef (gameTimeRef env) 1000
            writeIORef (enginePausedRef env) False
            samples ← forM [1 .. 120 ∷ Int] $ \_ → do
                tickAllMovement 0.1 env utsRef
                simOf utsRef uid
            [ s | Just s ← samples, not (inMap ab2010 s) ] `shouldBe` []

isTransition ∷ UnitActivity → Bool
isTransition (TransitioningTo _) = True
isTransition _                   = False
