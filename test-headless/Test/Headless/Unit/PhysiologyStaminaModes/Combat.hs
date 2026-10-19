{-# LANGUAGE Strict, OverloadedStrings #-}
-- | Strikes, interleavings and the SD-20 engagement of the "physiology
--   stamina modes" group (#2834; design S8.3, S8.6): T-S8-1 to T-S8-4.
--
--   Every strike here is the REAL @Combat.Resolution.resolveAttack@ — the
--   production admission commit ('commitIfAdmitted'), 'Wear' and 'Damage',
--   over the real unit manager — with the stream pinned by a fixed
--   'StdGen', so an attempt that hits and one that misses are chosen by
--   scanning fixed seeds and reading what the commit did, never by luck.
--   The facade and the ticks are the shipped Lua
--   (@scripts\/physiology.lua@, @scripts\/unit_resources.lua@).
--
--   The interleavings are CONTROLLED rather than raced: the strike is
--   applied from inside the Lua rate calculation by a hook interposed on
--   @unit.getStat@ ("Test.Headless.Unit.StaminaCommit"'s technique), so
--   the window #2470 and S8-F2 lost is hit deterministically.
module Test.Headless.Unit.PhysiologyStaminaModes.Combat (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (modifyIORef', readIORef, writeIORef)
import qualified HsLua as Lua
import qualified System.Random as Random
import Combat.Resolution (resolveAttack)
import Combat.Resolution.Admission (StrikeCommit(..), commitIfAdmitted)
import Combat.Resolution.Stamina (staminaModeFor, staminaWorkFraction)
import qualified Combat.Resolution.Wear as Wear
import Combat.Types (AttackMode(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Unit.PhysiologyModes.Harness
import Unit.Physiology.Types
import Unit.Types
import World.Chunk.Types (ChunkCoord(..), LoadedChunk(..), chunkSize)
import World.Fluid.Types (emptyIceMap)
import World.Flora.Types (emptyFloraChunkData)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.State.Types (WorldManager(..), WorldState(..))
import World.Tile.Types (WorldTileData(..))
import World.Time.Types (WorldTime(..), preciseWorldTime)
import Structure.Types (emptyChunkStructures)

-----------------------------------------------------------------------
-- Fixtures
-----------------------------------------------------------------------

modOf ∷ Float → Float → StatModifier
modOf delta percent = StatModifier delta "spec" Nothing percent

-- | A nomad as the shipped definition declares it: stamina Nominal by
--   default, Simulated at the first admitted combat attempt, an innate
--   +50% @max_stamina@ (maximum 15 at endurance 1), no pool.
nomad ∷ Int → UnitSpec
nomad uid = (unitSpec uid "nomad_primitive" (Just "exempt"))
    { usModes = [("stamina", PhysioNominal)]
    , usEngagement = [("stamina", PhysioSimulated)]
    , usDropStats = ["max_stamina", "stamina"]
    , usModifiers = [("max_stamina", [modOf 0 0.5])] }

-- | A Simulated-stamina fighter of a species that does not declare the
--   trigger (the acolyte's shape, every other system Nominal).
fighter ∷ Int → UnitSpec
fighter uid = (unitSpec uid "acolyte" (Just "exempt"))
    { usModes = [("stamina", PhysioSimulated)]
    , usDropStats = ["max_stamina", "stamina"] }

-- | A Nominal-stamina unit that does not declare the trigger (the mule).
mule ∷ Int → UnitSpec
mule uid = (unitSpec uid "technomule" (Just "reduced"))
    { usModes = [("stamina", PhysioNominal)]
    , usDropStats = ["max_stamina", "stamina"] }

flatChunk ∷ LoadedChunk
flatChunk =
    let area = chunkSize * chunkSize
        v = VU.replicate area 5
    in LoadedChunk
        { lcCoord = ChunkCoord 0 0, lcTiles = V.empty
        , lcSurfaceMap = v, lcTerrainSurfaceMap = v
        , lcFluidMap = V.replicate area Nothing
        , lcIceMap = emptyIceMap, lcFlora = emptyFloraChunkData
        , lcSideDeco = VU.empty, lcWaterTableMap = VU.empty
        , lcMagma = Nothing, lcStructures = emptyChunkStructures }

-- | The harness scene with a flat page, so a strike can be resolved (the
--   dodge save reads the terrain) and nothing is refused for a reason these
--   specs are not about.
combatScene ∷ [UnitSpec] → (Scene → IO a) → IO a
combatScene specs act = withScene specs $ \sc@(env, _) → do
    wm ← readIORef (worldManagerRef env)
    forM_ (wmWorlds wm) $ \(_, ws) → do
        writeIORef (wsTilesRef ws)
            (WorldTileData (HM.singleton (ChunkCoord 0 0) flatChunk) 1 Nothing)
        writeIORef (wsTimeRef ws) (preciseWorldTime (WorldTime 12 0))
        writeIORef (wsGenParamsRef ws)
            (Just defaultWorldGenParams { wgpWorldSize = 4 })
    act sc

-- | Everything stored about stamina for one unit.
statOf ∷ EngineEnv → Int → Text → IO (Maybe Float)
statOf env uid k = HM.lookup k ∘ uiStats <$> readInstance env uid

-- | The unit manager, as the engine holds it right now.
snapshotUM ∷ EngineEnv → IO UnitManager
snapshotUM env = readIORef (unitManagerRef env)

-- | How one attempt ended.
data Outcome = Hit | Miss | Refused deriving (Show, Eq)

-- | One attempt through the REAL resolution, from a pinned stream. The unit
--   manager is restored afterwards when @keep@ is False, so a scan over
--   seeds changes nothing.
attempt ∷ EngineEnv → Bool → Int → Int → Int → AttackMode → IO Outcome
attempt env keep seed atk tgt mode = do
    um0 ← snapshotUM env
    before ← woundCount um0 tgt
    stance0 ← stanceOf um0 atk
    _ ← resolveAttack env (Random.mkStdGen seed) (fromIntegral atk)
            (fromIntegral tgt) mode 0 0
    um1 ← snapshotUM env
    after ← woundCount um1 tgt
    stance1 ← stanceOf um1 atk
    _ ← drainCommands env
    unless keep $ modifyIORef' (unitManagerRef env) (const um0)
    pure $ if after > before then Hit
           else if stance1 < stance0 then Miss
           else Refused
  where
    woundCount um u = pure $ maybe 0 (length ∘ uiWounds)
        (HM.lookup (UnitId (fromIntegral u)) (umInstances um))
    stanceOf um u = pure $ maybe 1 (HM.lookupDefault 1 "stance" ∘ uiStats)
        (HM.lookup (UnitId (fromIntegral u)) (umInstances um))

-- | The first seed in a fixed range that gives @want@, scanning on a
--   restored manager.
seedFor ∷ EngineEnv → Outcome → Int → Int → AttackMode → IO Int
seedFor env want atk tgt mode = go [1 .. 400]
  where
    go [] = do
        expectationFailure ("no seed in 1..400 gives " <> show want)
        pure 1
    go (s : rest) = do
        o ← attempt env False s atk tgt mode
        if o ≡ want then pure s else go rest

-- | An admitted attempt that hits (@True@) or misses (@False@), kept.
admitted ∷ EngineEnv → Bool → Int → Int → AttackMode → IO ()
admitted env hit atk tgt mode = do
    seed ← seedFor env (if hit then Hit else Miss) atk tgt mode
    o ← attempt env True seed atk tgt mode
    o `shouldBe` (if hit then Hit else Miss)

-- | Give the unit full stance again (what a stance recovery would do), so
--   a long run of strikes is never refused for stance.
refillStance ∷ EngineEnv → Int → IO ()
refillStance env uid = modifyInstance env uid $ \i →
    i { uiStats = HM.insert "stance" 1 (uiStats i) }

-- | Record every @unit.applyCombatEngagement@ outcome (@uid:outcome@) and
--   every warning about an engagement, while calling through.
installEngagementRecorder ∷ LuaBackendState → IO ()
installEngagementRecorder ls = do
    r ← luaText ls $ T.unlines
        [ "_G.__eng = {}; _G.__engWarn = 0"
        , "local real = unit.applyCombatEngagement"
        , "unit.applyCombatEngagement = function(uid)"
        , "  local o, c = real(uid)"
        , "  __eng[#__eng + 1] = tostring(uid) .. ':' .. tostring(o)"
        , "  return o, c"
        , "end"
        , "local warn = engine.logWarn"
        , "engine.logWarn = function(m)"
        , "  if tostring(m):find('combat engagement', 1, true) then"
        , "    __engWarn = __engWarn + 1 end"
        , "  if warn then return warn(m) end"
        , "end"
        , "return true" ]
    r `shouldBe` "true"

-- | The recorded engagement outcomes for one unit, in order.
engagementOutcomes ∷ LuaBackendState → Int → IO [Text]
engagementOutcomes ls uid = do
    out ← luaText ls "return table.concat(__eng, ',')"
    pure [ T.drop (T.length prefix) o
         | o ← T.splitOn "," out, prefix `T.isPrefixOf` o ]
  where prefix = tshow uid <> ":"

-- | One physiology pass (the shipped @unit_resources.update@).
pass ∷ LuaBackendState → IO ()
pass ls = runSeconds ls 0.1 0.1

-- | The mode stamina resolves to for a unit, by the engine's resolver.
modeOf' ∷ EngineEnv → Int → IO ResolvedMode
modeOf' env uid = do
    reg ← readIORef (physiologyRegistryRef env)
    um ← snapshotUM env
    inst ← readInstance env uid
    let up = maybe noUnitPhysiology udPhysiology
                 (HM.lookup (uiDefName inst) (umDefs um))
    pure (staminaModeFor reg up inst)

-- | The attacker's work fraction as 'Damage' reads it.
workFraction ∷ EngineEnv → Int → IO Float
workFraction env uid = do
    reg ← readIORef (physiologyRegistryRef env)
    um ← snapshotUM env
    inst ← readInstance env uid
    now ← readIORef (gameTimeRef env)
    let up = maybe noUnitPhysiology udPhysiology
                 (HM.lookup (uiDefName inst) (umDefs um))
    pure (staminaWorkFraction reg up now inst)

-- | The marker, the pinned override and the stored pool, for assertions.
engagementState ∷ EngineEnv → Int
                → IO (Maybe Float, Maybe PhysioMode, Maybe Float)
engagementState env uid = do
    i ← readInstance env uid
    pure ( HM.lookup "combat_engaged" (uiStats i)
         , HM.lookup "stamina" (uiPhysioOverrides i)
         , HM.lookup "stamina" (uiStats i) )

-----------------------------------------------------------------------
-- The group
-----------------------------------------------------------------------

spec ∷ Spec
spec = describe "combat: strikes, interleavings and engagement" $ do
    freeSwingSpec
    stanceSpec
    triggerSpec
    stateSpec
    interleavingSpec

-----------------------------------------------------------------------
-- T-S8-2: free swings (S8-F1)
-----------------------------------------------------------------------

freeSwingSpec ∷ Spec
freeSwingSpec = describe "T-S8-2: a Nominal receiver swings at full work" $ do
    forM_ [ ("a Nominal technomule", mule 1)
          , ("a nomad before its first engagement", nomad 1) ] $ \(label, spec1) →
        it (label <> " swings repeatedly at full work and creates no stamina key") $
            combatScene [spec1, fighter 2] $ \(env, _) → do
                -- a hit seed, fixed: the same stream every swing
                seed ← seedFor env Hit 1 2 Quick
                sevs ← forM [1 .. 6 ∷ Int] $ \i → do
                    -- the target is pristine and the attacker rested before each
                    modifyInstance env 2 $ \t → t { uiWounds = [] }
                    refillStance env 1
                    o ← attempt env True seed 1 2 (if even i then Heavy else Quick)
                    statOf env 1 "stamina" `shouldReturn` Nothing
                    workFraction env 1 `shouldReturn` 1.0
                    -- the severity of the wound it dealt
                    inst ← readInstance env 2
                    pure (o, [ woundSeverity w | w ← uiWounds inst ])
                -- every Quick swing deals what the first one did: the pool
                -- never wound it down to the 0.3 floor
                let quicks = [ s | (i, (Hit, s)) ← zip [1 ∷ Int ..] sevs, odd i ]
                length quicks `shouldSatisfy` (≥ 2)
                forM_ (drop 1 quicks) $ \s → Just s `shouldBe` listToMaybe quicks

    it "the old defect, for contrast: a Simulated fighter with an empty pool \
       \works at the 0.3 floor, so full work is the mode's doing" $
        combatScene [fighter 1] $ \(env, _) → do
            modifyInstance env 1 $ \i →
                i { uiStats = HM.insert "stamina" 0 (uiStats i) }
            workFraction env 1 `shouldReturn` 0.3

    it "a Simulated pool that is absent is read as full work and created by no strike" $
        combatScene [ fighter 1, fighter 2 ] $
            \(env, _) → do
                modifyInstance env 1 $ \i →
                    i { uiStats = HM.delete "stamina" (uiStats i) }
                workFraction env 1 `shouldReturn` 1.0
                _ ← attempt env True 1 1 2 Heavy
                statOf env 1 "stamina" `shouldReturn` Nothing

-----------------------------------------------------------------------
-- T-S8-3: stance is unowned and unchanged across stamina modes
-----------------------------------------------------------------------

stanceSpec ∷ Spec
stanceSpec = describe "T-S8-3: stance spending and recovery do not depend on the stamina mode" $
    it "a strike spends the same stance from a Nominal, a Simulated and a \
       \pre-engagement receiver, and the atomic recovery verb credits the same" $
        combatScene [mule 1, fighter 2, nomad 3, fighter 4] $ \(env, ls) → do
            forM_ [1, 2, 3] $ \atk → do
                refillStance env atk
                _ ← attempt env True 7 atk 4 Heavy
                statOf env atk "stance" `shouldReturn` Just 0.5
            -- recovery: the same relative credit, whatever the mode
            forM_ [1, 2, 3] $ \atk → do
                r ← luaText ls ("return tostring(unit.recoverStance(" <> tshow atk <> ", 0.25))")
                r `shouldNotBe` "nil"
                luaText ls ("return __f(unit.getStat(" <> tshow atk <> ", 'stance'))")
                    `shouldReturn` "0.75"
            -- and a stamina commit never touches it
            forM_ [2] $ \u → do
                _ ← luaText ls ("return unit.commitStamina(" <> tshow u <> ", -1) ~= nil")
                luaText ls ("return __f(unit.getStat(" <> tshow u <> ", 'stance'))")
                    `shouldReturn` "0.75"

-----------------------------------------------------------------------
-- T-S8-4: the trigger matrix
-----------------------------------------------------------------------

-- | The scene of a nomad (1) and an acolyte-like fighter (2) with the
--   recorder installed.
duel ∷ (Scene → IO a) → IO a
duel act = combatScene [nomad 1, fighter 2] $ \sc@(_, ls) → do
    installEngagementRecorder ls
    act sc

-- | Run passes until nothing is left to do.
settle ∷ LuaBackendState → IO ()
settle ls = replicateM_ 3 (pass ls)

triggerSpec ∷ Spec
triggerSpec = describe "T-S8-4: what engages, and the one application" $ do
    forM_ [ ("(a) a nomad's admitted attack hits", True, 1, 2, 1)
          , ("(a') a nomad's admitted attack misses", False, 1, 2, 1)
          , ("(b) an admitted attack hits a nomad", True, 2, 1, 1)
          , ("(b') an admitted attack misses a nomad", False, 2, 1, 1) ] $
        \(label, hit, atk, tgt, nomadId) →
            it (label <> ": marker 1, then one transition at the next pass") $
                duel $ \(env, ls) → do
                    engagementState env nomadId `shouldReturn` (Nothing, Nothing, Nothing)
                    admitted env hit atk tgt Quick
                    -- detection: the pending marker, no stamina, no mode change
                    engagementState env nomadId `shouldReturn` (Just 1, Nothing, Nothing)
                    modeOf' env nomadId `shouldReturn` ModeNominal
                    -- the other unit (a species with no declaration) is untouched
                    statOf env 2 "combat_engaged" `shouldReturn` Nothing
                    pass ls
                    (marker, ovr, pool) ← engagementState env nomadId
                    marker `shouldBe` Just 2
                    ovr `shouldBe` Just PhysioSimulated
                    pool `shouldBe` Just 15          -- the full effective maximum
                    modeOf' env nomadId `shouldReturn` ModeSimulated
                    engagementOutcomes ls nomadId `shouldReturn` ["transitioned"]
                    settle ls
                    engagementOutcomes ls nomadId `shouldReturn` ["transitioned"]

    it "(c) an attack refused at admission engages nobody" $
        duel $ \(env, ls) → do
            -- out of reach: refused before anything is written
            modifyInstance env 2 $ \i → i { uiGridX = 60 }
            o ← attempt env True 3 1 2 Quick
            o `shouldBe` Refused
            engagementState env 1 `shouldReturn` (Nothing, Nothing, Nothing)
            statOf env 2 "combat_engaged" `shouldReturn` Nothing
            settle ls
            engagementOutcomes ls 1 `shouldReturn` []

    it "(d) being in range, or the pairing alone, with no admitted attempt, engages nobody" $
        duel $ \(env, ls) → do
            settle ls
            engagementState env 1 `shouldReturn` (Nothing, Nothing, Nothing)
            engagementOutcomes ls 1 `shouldReturn` []

    it "(e) admitted attempts between non-declaring species engage nobody" $
        combatScene [fighter 1, fighter 2, mule 3] $ \(env, ls) → do
            installEngagementRecorder ls
            admitted env True 1 2 Quick
            admitted env False 2 1 Quick
            admitted env True 3 2 Quick
            forM_ [1, 2, 3] $ \u →
                statOf env u "combat_engaged" `shouldReturn` Nothing
            settle ls
            forM_ [1, 2, 3] $ \u → engagementOutcomes ls u `shouldReturn` []

    it "(f) five admitted attempts within one pass yield one detection and one application" $
        duel $ \(env, ls) → do
            forM_ [1 .. 5 ∷ Int] $ \_ → do
                refillStance env 1
                admitted env True 1 2 Quick
                statOf env 1 "combat_engaged" `shouldReturn` Just 1
            pass ls
            engagementOutcomes ls 1 `shouldReturn` ["transitioned"]
            statOf env 1 "combat_engaged" `shouldReturn` Just 2
            settle ls
            engagementOutcomes ls 1 `shouldReturn` ["transitioned"]

    it "(g) strikes admitted while pending are cost-free and full-work; the \
       \first strike after application is charged" $
        duel $ \(env, ls) → do
            admitted env True 1 2 Quick
            forM_ [1 .. 3 ∷ Int] $ \_ → do
                refillStance env 1
                admitted env True 1 2 Heavy
                statOf env 1 "stamina" `shouldReturn` Nothing
                workFraction env 1 `shouldReturn` 1.0
            pass ls
            statOf env 1 "stamina" `shouldReturn` Just 15
            refillStance env 1
            admitted env True 1 2 Quick
            -- 5% of the effective maximum 15
            Just st ← statOf env 1 "stamina"
            near 1e-4 14.25 (realToFrac st)

    it "(i) an explicit Nominal override before engagement latches the marker \
       \without a transition; clearing it then seeds Simulated" $
        duel $ \(env, ls) → do
            luaText ls "return tostring(__physiology.setMode(1, 'stamina', 'nominal'))"
                `shouldReturn` "true"
            admitted env True 1 2 Quick
            pass ls
            engagementState env 1 `shouldReturn` (Just 2, Just PhysioNominal, Nothing)
            modeOf' env 1 `shouldReturn` ModeNominal
            engagementOutcomes ls 1 `shouldReturn` ["latched"]
            -- clearing: the engagement default is now the effective mode
            luaText ls "return tostring(__physiology.clearMode(1, 'stamina'))"
                `shouldReturn` "true"
            engagementState env 1 `shouldReturn` (Just 2, Nothing, Just 15)
            modeOf' env 1 `shouldReturn` ModeSimulated

    it "(j) an explicit Nominal override after engagement wins" $
        duel $ \(env, ls) → do
            admitted env True 1 2 Quick
            pass ls
            statOf env 1 "stamina" `shouldReturn` Just 15
            luaText ls "return tostring(__physiology.setMode(1, 'stamina', 'nominal'))"
                `shouldReturn` "true"
            engagementState env 1 `shouldReturn` (Just 2, Just PhysioNominal, Nothing)
            modeOf' env 1 `shouldReturn` ModeNominal

    it "(k) clearPhysioMode after engagement stays Simulated, with no reseed and no hooks" $
        duel $ \(env, ls) → do
            admitted env True 1 2 Quick
            pass ls
            -- a partial pool the reseed would visibly refill
            modifyInstance env 1 $ \i →
                i { uiStats = HM.insert "stamina" 6 (uiStats i) }
            _ ← luaText ls $ T.unlines
                [ "_G.__hooks = 0"
                , "__physiology.onModeChanged('spec_k', 'stamina',"
                , "  function() __hooks = __hooks + 1 end)"
                , "return true" ]
            luaText ls "return tostring(__physiology.clearMode(1, 'stamina'))"
                `shouldReturn` "true"
            engagementState env 1 `shouldReturn` (Just 2, Nothing, Just 6)
            modeOf' env 1 `shouldReturn` ModeSimulated
            luaText ls "return __hooks" `shouldReturn` "0"

    it "(m) a second engagement after the marker is applied changes nothing" $
        duel $ \(env, ls) → do
            admitted env True 1 2 Quick
            pass ls
            before ← engagementState env 1
            refillStance env 2
            admitted env True 2 1 Quick
            (marker, ovr, _) ← engagementState env 1
            (marker, ovr) `shouldBe` (\(m, o, _) → (m, o)) before
            settle ls
            engagementOutcomes ls 1 `shouldReturn` ["transitioned"]

-----------------------------------------------------------------------
-- State cases: lethal strike, unrepresentable seed
-----------------------------------------------------------------------

stateSpec ∷ Spec
stateSpec = describe "T-S8-4: refusals keep the marker pending" $ do
    it "(l) a target killed by the triggering strike never transitions: \
       \marker 1 on the corpse, refused as dead, no repeated attempts" $
        combatScene [ fighter 1, nomad 2 ] $ \(env, ls) → do
            installEngagementRecorder ls
            -- the nomad's torso is vital
            modifyIORef' (unitManagerRef env) $ \um →
                um { umDefs = HM.adjust (\d → d { udBodyParts =
                        [ p { bpVital = True } | p ← udBodyParts d ] })
                        "nomad_primitive" (umDefs um) }
            -- the triggering admitted commit: the strike's own writes include
            -- a lethal wound on the vital part, in the SAME atomic update
            reg ← readIORef (physiologyRegistryRef env)
            now ← readIORef (gameTimeRef env)
            let lethal = Wound { woundPart = "torso", woundKind = "blunt"
                               , woundSeverity = 1.0, woundAt = 0
                               , woundBandage = 1.0, woundClot = 0.0
                               , woundHeal = 0.0, woundDressing = ""
                               , woundInfection = 0.0, woundClean = False
                               , woundInfectionType = "", woundNecrosis = 0.0 }
                apply _ _ um = Wear.spendStrikeCost reg now Heavy 1 $
                    um { umInstances = HM.adjust
                            (\t → t { uiWounds = lethal : uiWounds t })
                            (UnitId 2) (umInstances um) }
            commitIfAdmitted env (const 5) 0 Heavy 1 2 apply
                `shouldReturn` CommitApplied
            -- the pose mirror lags the lethal commit: still "standing"
            uiPose <$> readInstance env 2 `shouldReturn` "standing"
            statOf env 2 "combat_engaged" `shouldReturn` Just 1
            pass ls
            engagementOutcomes ls 2 `shouldReturn` ["refusedDead"]
            engagementState env 2 `shouldReturn` (Just 1, Nothing, Nothing)
            -- the unit thread now lands the kill; the corpse is never revisited
            modifyInstance env 2 $ \i → i { uiPose = "dead" }
            settle ls
            engagementOutcomes ls 2 `shouldReturn` ["refusedDead"]
            engagementState env 2 `shouldReturn` (Just 1, Nothing, Nothing)

    it "(n) a living nomad with a retained -100% stamina modifier stays pending \
       \and Nominal at full work, logs ONE diagnostic across retries, and \
       \transitions with a full seed once the modifier is gone" $
        duel $ \(env, ls) → do
            modifyInstance env 1 $ \i →
                i { uiModifiers = HM.insert "stamina" [modOf 0 (-1)]
                                    (uiModifiers i) }
            admitted env True 1 2 Quick
            forM_ [1 .. 5 ∷ Int] $ \_ → pass ls
            engagementOutcomes ls 1
                `shouldReturn` replicate 5 "seedUnrepresentable"
            luaText ls "return __engWarn" `shouldReturn` "1"
            engagementState env 1 `shouldReturn` (Just 1, Nothing, Nothing)
            modeOf' env 1 `shouldReturn` ModeNominal
            workFraction env 1 `shouldReturn` 1.0
            -- a pending strike is still free
            refillStance env 1
            admitted env True 1 2 Heavy
            statOf env 1 "stamina" `shouldReturn` Nothing
            -- the blocker is removed: the very next pass transitions, full seed
            modifyInstance env 1 $ \i →
                i { uiModifiers = HM.delete "stamina" (uiModifiers i) }
            pass ls
            engagementState env 1 `shouldReturn` (Just 2, Just PhysioSimulated, Just 15)
            last <$> engagementOutcomes ls 1 `shouldReturn` "transitioned"

    it "(n) the refusal diagnostic's debounce is session state: it is cleared \
       \by the session teardown and the load reset, so a reused unit id logs again" $
        duel $ \(env, ls) → do
            modifyInstance env 1 $ \i →
                i { uiModifiers = HM.insert "stamina" [modOf 0 (-1)]
                                    (uiModifiers i) }
            admitted env True 1 2 Quick
            forM_ [1 .. 3 ∷ Int] $ \_ → pass ls
            luaText ls "return __engWarn" `shouldReturn` "1"
            -- Exit to Menu: every registered clear runs (unit_resources, through
            -- the same function its load reset hook runs), then a new session
            _ ← luaText ls $ T.unlines
                [ "local t = require('scripts.lib.session_teardown')"
                , "t.runAll(); t.beginSession(); return true" ]
            pass ls
            luaText ls "return __engWarn" `shouldReturn` "2"
            -- and it is debounced again within the new session
            forM_ [1 .. 3 ∷ Int] $ \_ → pass ls
            luaText ls "return __engWarn" `shouldReturn` "2"

    it "T-S8-8: an engagement seed the solver could reach but the guards refuse \
       \(target 10, flat -10, percent -2) stays pending with no pinned override" $
        combatScene [ (nomad 1) { usModifiers = [] }, fighter 2 ] $ \(env, ls) → do
            installEngagementRecorder ls
            -- the unit's max is 10 (endurance 1, no +50%); flat -10, percent -2
            modifyInstance env 1 $ \i → i
                { uiModifiers = HM.insert "stamina" [modOf (-10) (-2)] (uiModifiers i) }
            admitted env True 1 2 Quick
            pass ls
            engagementOutcomes ls 1 `shouldReturn` ["seedUnrepresentable"]
            engagementState env 1 `shouldReturn` (Just 1, Nothing, Nothing)

-----------------------------------------------------------------------
-- T-S8-1: concurrency, controlled
-----------------------------------------------------------------------

-- | Expose an engine-side mutation to Lua as @__hook@, so an example can
--   fire it from INSIDE the script's own rate calculation.
installHook ∷ LuaBackendState → IO () → IO ()
installHook ls act = Lua.runWith (lbsLuaState ls) (installer ∷ Lua.LuaE Lua.Exception ())
  where
    installer = do
        Lua.pushHaskellFunction fire
        Lua.setglobal "__hook"
    fire ∷ Lua.LuaE Lua.Exception Lua.NumResults
    fire = Lua.liftIO act ≫ pure 0

-- | Wrap @unit.getStat@ so the FIRST read of @trigger@ for @uid@ AFTER a
--   read of @after@ runs @__hook@: the window the old read-modify-write lost,
--   made deterministic.
interposeAfter ∷ LuaBackendState → Int → Text → Text → IO ()
interposeAfter ls uid after trigger = do
    r ← luaText ls $ T.concat
        [ "_G.__fired = 0; local seen = false; local real = unit.getStat; "
        , "unit.getStat = function(id, name) "
        , "  if id == ", tshow uid, " then "
        , "    if name == '", after, "' then seen = true end; "
        , "    if seen and name == '", trigger, "' and __fired == 0 then "
        , "      __fired = 1; __hook(id); end; "
        , "  end; "
        , "  return real(id, name); "
        , "end; return true" ]
    r `shouldBe` "true"

interleavingSpec ∷ Spec
interleavingSpec = describe "T-S8-1: strikes interleaved with ticks, transitions and the shiver debit" $ do
    it "a strike landing inside the tick's rate calculation is never refunded: \
       \the pool is a serial order of the spend and the recovery" $
        combatScene [ fighter 1 ] $ \(env, ls) → do
            modifyInstance env 1 $ \i →
                i { uiStats = HM.insert "stamina" 6 (uiStats i) }
            installHook ls $ do
                reg ← readIORef (physiologyRegistryRef env)
                now ← readIORef (gameTimeRef env)
                modifyIORef' (unitManagerRef env) (spendHeavy reg now)
            interposeAfter ls 1 "stamina" "endurance"
            pass ls
            luaText ls "return __fired" `shouldReturn` "1"
            Just st ← statOf env 1 "stamina"
            -- 6.0 - 2.5 (heavy, 25% of 10) + 0.05 (idle regen 0.5 x 1 x 0.1)
            near 1e-4 3.55 (realToFrac st)

    it "ordering across ticks: strike, tick, strike, tick equals the serial sum" $
        combatScene [ fighter 1 ] $ \(env, ls) → do
            modifyInstance env 1 $ \i →
                i { uiStats = HM.insert "stamina" 10 (uiStats i) }
            reg ← readIORef (physiologyRegistryRef env)
            now ← readIORef (gameTimeRef env)
            modifyIORef' (unitManagerRef env) (spendHeavy reg now)
            pass ls
            modifyIORef' (unitManagerRef env) (spendHeavy reg now)
            pass ls
            Just st ← statOf env 1 "stamina"
            near 1e-4 (10 - 2.5 + 0.05 - 2.5 + 0.05) (realToFrac st)

    it "a strike before the switch to Nominal is scrubbed with the pool, and a \
       \strike after the switch to Simulated spends from the freshly seeded pool" $
        combatScene [ fighter 1 ] $ \(env, ls) → do
            reg ← readIORef (physiologyRegistryRef env)
            now ← readIORef (gameTimeRef env)
            modifyIORef' (unitManagerRef env) (spendHeavy reg now)
            luaText ls "return tostring(__physiology.setMode(1, 'stamina', 'nominal'))"
                `shouldReturn` "true"
            statOf env 1 "stamina" `shouldReturn` Nothing
            -- a strike at a Nominal receiver creates no key
            modifyIORef' (unitManagerRef env) (spendHeavy reg now)
            statOf env 1 "stamina" `shouldReturn` Nothing
            luaText ls "return tostring(__physiology.setMode(1, 'stamina', 'simulated'))"
                `shouldReturn` "true"
            statOf env 1 "stamina" `shouldReturn` Just 10
            modifyIORef' (unitManagerRef env) (spendHeavy reg now)
            statOf env 1 "stamina" `shouldReturn` Just 7.5

    it "the shiver debit lands as a relative commit: a strike inside the thermo \
       \tick is not refunded (S8-F2)" $
        combatScene [ (fighter 1) { usModes = [("stamina", PhysioSimulated), ("thermo", PhysioSimulated)] } ] $
            \(env, ls) → do
                climate ls (-10) 0.5
                -- a core already shivering cold
                modifyInstance env 1 $ \i → i
                    { uiStats = HM.insert "core_temp" 34 (HM.insert "stamina" 6 (uiStats i)) }
                installHook ls $ do
                    reg ← readIORef (physiologyRegistryRef env)
                    now ← readIORef (gameTimeRef env)
                    modifyIORef' (unitManagerRef env) (spendHeavy reg now)
                interposeAfter ls 1 "metabolism" "body_mass"
                _ ← luaText ls $ T.unlines
                    [ "local info = unit.getInfo(1)"
                    , "require('scripts.thermo').tick(1, info, 0.1)"
                    , "return true" ]
                luaText ls "return __fired" `shouldReturn` "1"
                Just st ← statOf env 1 "stamina"
                -- the strike (2.5) and the shiver debit are both kept; the old
                -- absolute write republished 6.0 minus the debit alone
                realToFrac st `shouldSatisfy` (< (6 - 2.5 + 1e-6 ∷ Double))

    it "a Nominal stamina receiver discards the shiver debit and creates no key" $
        combatScene [ (fighter 1) { usModes = [("stamina", PhysioNominal), ("thermo", PhysioSimulated)] } ] $
            \(env, ls) → do
                climate ls (-10) 0.5
                modifyInstance env 1 $ \i → i
                    { uiStats = HM.insert "core_temp" 34 (uiStats i) }
                forM_ [1 .. 10 ∷ Int] $ \_ → pass ls
                statOf env 1 "stamina" `shouldReturn` Nothing
  where
    spendHeavy = spendStrikeCostHeavy

-- | One heavy strike's charge on unit 1, through the production function.
spendStrikeCostHeavy ∷ PhysiologyRegistry → Double → UnitManager → UnitManager
spendStrikeCostHeavy reg now = Wear.spendStrikeCost reg now Heavy 1
