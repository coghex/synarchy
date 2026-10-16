{-# LANGUAGE Strict, OverloadedStrings #-}
-- | T-S8-5 of the "physiology stamina modes" group (#2834; design S8.3
--   "Nomad stamina tuning", S8.6): the two deterministic acceptance cases
--   for the nomad's stamina parameters. The comparison unit is the
--   STAMINA FRACTION (effective stamina over the effective maximum), and
--   both cases hold thermo Nominal for both units, so there is no shiver.
--
--   __B, the fixed-cadence budget.__ An endurance-1 acolyte and an engaged
--   nomad (maxima 10 and 15, no other modifiers), idle, no wounds, dt
--   0.1 s, both full. At every whole second each is charged by the REAL
--   'spendStrikeCost' — a Heavy at multiples of 4, a Quick otherwise —
--   AFTER that tick's physiology pass, and sampled after the charge, for
--   30 s. The nomad's fraction must stay at or above 0.1 at every sample
--   and at or above the acolyte's. Nothing about the transient (the
--   acolyte crossing 0.1 near 18 s, the nomad ending near 0.25) is
--   asserted: start-up clipping and the Heavy phase shift it.
--
--   __E, the bounded encounter.__ The shipped nomad (default loadout,
--   unarmed) and a standard-profile acolyte with its dagger, spawned
--   through the real handler on a flat arena from a pinned stat RNG, held
--   0.5 tiles apart, with the REAL attack resolution (admission, 'Wear',
--   'Damage') and the shipped @computeAttackCooldown@ rule and stance
--   >= 0.25 gate scheduling every attack, every fourth Heavy and the rest
--   Quick. The fixture states its two choices: a scheduled Heavy at
--   stance below 0.5 DOWNGRADES to Quick, as the shipped AI does
--   (@unit_ai_combat_attack.lua@) because admission refuses a Heavy there;
--   and the every-fourth counter advances per attack ISSUED, downgraded
--   or not. The run stops at either unit's first collapse or death, or at
--   60 s, sampling every 0.5 s. It is graded only when VALID: each unit
--   has at least one ADMITTED strike and the nomad's marker reached 2 by
--   @transitioned@; reaching the cap with either missing is a fixture
--   failure, never a pass. A valid failure of either case means the
--   parameters are retuned, not the bounds relaxed.
module Test.Headless.Unit.PhysiologyStaminaModes.Tuning
    ( spec
      -- * Shared with the persistence specs
    , withEncounter
    , encounterPage
    ) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort)
import System.FilePath ((</>), takeExtension)
import System.Directory (listDirectory)
import qualified System.Random as Random
import Combat.Resolution (resolveAttack)
import Combat.Resolution.Common (maxStaminaFor)
import Combat.Resolution.Wear (spendStrikeCost)
import Combat.Types (AttackMode(..))
import Engine.Core.Capability.UnitCombat
    (UnitCombatCapability(..), toUnitCombatCapability)
import Engine.Core.State (EngineEnv(..))
import Engine.Scripting.Lua.Types (LuaBackendState)
import Test.Headless.Unit.PhysiologyModes.Harness
import Test.Headless.Unit.TransferApi (evalDebug)
import Unit.Command.Types (SpawnProfile(..), UnitCommand(..))
import Unit.Faction (Faction(..))
import Unit.Faction.Membership (resolveLegacyFaction)
import Unit.Physiology.Types
import Unit.Stats (effectiveStat)
import Unit.Thread.Command.Spawn (handleUnitSpawnCommand)
import Unit.Types
import World.Chunk.Admit (pageIncarnation)
import World.Chunk.Types (ChunkCoord(..), ColumnTiles(..), LoadedChunk(..))
import World.Flora.Types (emptyFloraChunkData)
import World.Fluid.Types (emptyIceMap)
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Page.Types (WorldPageId(..))
import World.State.Types
    (WorldManager(..), WorldState(..), emptyWorldManager, emptyWorldState)
import World.Tile.Types (WorldTileData(..))
import Structure.Types (emptyChunkStructures)

spec ∷ Spec
spec = describe "T-S8-5: the nomad's stamina tuning" $ do
    budgetSpec
    encounterSpec

-----------------------------------------------------------------------
-- Shared readers
-----------------------------------------------------------------------

-- | The stamina fraction of a unit: its effective stamina over its
--   effective maximum, as every consumer reads it.
fractionOf ∷ EngineEnv → Int → IO Double
fractionOf env uid = do
    inst ← readInstance env uid
    now ← readIORef (gameTimeRef env)
    let mx = maxStaminaFor now inst
        eff = effectiveStat now (HM.lookupDefault 0 "stamina" (uiStats inst))
                  (HM.lookupDefault [] "stamina" (uiModifiers inst))
    pure (realToFrac (eff / mx))

modOf ∷ Float → Float → StatModifier
modOf delta percent = StatModifier delta "spec" Nothing percent

-----------------------------------------------------------------------
-- B: the fixed-cadence budget
-----------------------------------------------------------------------

-- | An endurance-1 acolyte (maximum 10) and an ENGAGED nomad (marker 2, a
--   pinned Simulated override, the +50% maximum modifier: maximum 15), both
--   idle, standing and healthy, every other system Nominal.
budgetUnits ∷ [UnitSpec]
budgetUnits =
    [ (unitSpec 1 "nomad_primitive" (Just "exempt"))
        { usModes = [("stamina", PhysioNominal)]
        , usEngagement = [("stamina", PhysioSimulated)]
        , usOverrides = [("stamina", PhysioSimulated)]
        , usDropStats = ["max_stamina", "stamina"]
        , usStats = [("combat_engaged", 2)]
        , usModifiers = [("max_stamina", [modOf 0 0.5])] }
    , (unitSpec 2 "acolyte" (Just "exempt"))
        { usModes = [("stamina", PhysioSimulated)]
        , usDropStats = ["max_stamina", "stamina"] } ]

budgetSpec ∷ Spec
budgetSpec = describe "B: a fixed-cadence budget" $
    it "the nomad's fraction stays at or above 0.1 and at or above the \
       \acolyte's at every sample over 30 s of Heavy at multiples of 4 s and \
       \Quick otherwise" $
        withScene budgetUnits $ \(env, ls) → do
            -- the maxima, and both pools full
            nomadMax ← luaNum ls "unit.getMaxStamina(1)"
            acolyteMax ← luaNum ls "unit.getMaxStamina(2)"
            (nomadMax, acolyteMax) `shouldBe` (15, 10)
            fractionOf env 1 `shouldReturn` 1
            fractionOf env 2 `shouldReturn` 1
            reg ← readIORef (physiologyRegistryRef env)
            samples ← forM [1 .. 300 ∷ Int] $ \step → do
                let t = fromIntegral step * 0.1 ∷ Double
                writeIORef (gameTimeRef env) t
                -- the acolyte's exhaustion death (kill_on_zero) raises a player
                -- alert that auto-pauses the engine; nothing here is a player,
                -- and a paused engine would stop the nomad's ticks too
                writeIORef (enginePausedRef env) False
                runSeconds ls 0.1 0.1
                if step `mod` 10 ≢ 0 then pure Nothing else do
                    let second = step `div` 10
                        mode = if second `mod` 4 ≡ 0 then Heavy else Quick
                    forM_ [1, 2] $ \uid →
                        modifyIORef' (unitManagerRef env)
                            (spendStrikeCost reg t mode (fromIntegral (uid ∷ Int)))
                    n ← fractionOf env 1
                    a ← fractionOf env 2
                    pure (Just (second, n, a))
            let sampled = [ s | Just s ← samples ]
            length sampled `shouldBe` 30
            -- neither the pool nor the death rules were skipped: the acolyte
            -- is genuinely spent by the end, the nomad is not
            case reverse sampled of
                ((_, n, a) : _) → n `shouldSatisfy` (> a)
                []              → expectationFailure "no samples"

-----------------------------------------------------------------------
-- E: the bounded encounter
-----------------------------------------------------------------------

encounterPage ∷ WorldPageId
encounterPage = WorldPageId "stamina_tuning_page"

terrainZ ∷ Int
terrainZ = 4

flatChunk ∷ LoadedChunk
flatChunk =
    let area = 16 * 16
        col = ColumnTiles
            { ctStartZ = terrainZ, ctMats = VU.singleton 1
            , ctSlopes = VU.singleton 0, ctVeg = VU.singleton 0 }
    in LoadedChunk
        { lcCoord = ChunkCoord 0 0, lcTiles = V.replicate area col
        , lcSurfaceMap = VU.replicate area terrainZ
        , lcTerrainSurfaceMap = VU.replicate area terrainZ
        , lcFluidMap = V.replicate area Nothing, lcIceMap = emptyIceMap
        , lcFlora = emptyFloraChunkData, lcSideDeco = VU.replicate area 0
        , lcWaterTableMap = VU.replicate area 0, lcMagma = Nothing
        , lcStructures = emptyChunkStructures }

-- | The shipped content a spawned acolyte and nomad depend on, in the
--   startup loader's order (factions before units).
contentDirs ∷ [(FilePath, Text)]
contentDirs =
    [ ("data/substances",  "engine.loadSubstanceYaml")
    , ("data/items",       "engine.loadItemYaml")
    , ("data/equipment",   "engine.loadEquipmentYaml")
    , ("data/materials",   "engine.loadMaterialYaml")
    , ("data/factions",    "engine.loadFactionYaml")
    , ("data/units",       "engine.loadUnitYaml")
    , ("data/loot_tables", "engine.loadLootTableYaml") ]

loadContent ∷ LuaBackendState → IO ()
loadContent ls = forM_ contentDirs $ \(dir, fn) → do
    files ← sort ∘ filter ((≡ ".yaml") ∘ takeExtension) <$> listDirectory dir
    forM_ files $ \f → void (evalDebug ls (fn <> "('" <> T.pack (dir </> f) <> "')"))

-- | The fixed generators the whole encounter draws from, documented: the
--   stat stream (spawn rolls, were any drawn), the treatment stream, and
--   the combat stream threaded through every resolution.
statSeed, treatSeed, combatSeed ∷ Int
statSeed = 2834
treatSeed = 28341
combatSeed = 283410

-- | One live page of flat ground and the shipped content, then the two
--   fighters spawned through the REAL handler: the nomad from its
--   definition with its default loadout, the acolyte from the standard
--   spawn profile with its dagger, 0.5 tiles apart.
withEncounter ∷ (Scene → IO a) → IO a
withEncounter act = withScene [] $ \sc@(env, ls) → do
    loadContent ls
    ws ← emptyWorldState
    writeIORef (wsTilesRef ws) WorldTileData
        { wtdChunks = HM.singleton (ChunkCoord 0 0) flatChunk, wtdMaxChunks = 1 }
    writeIORef (wsGenParamsRef ws)
        (Just defaultWorldGenParams { wgpWorldSize = 8 })
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(encounterPage, ws)], wmVisible = [encounterPage] }
    modifyIORef' (unitManagerRef env) $ \um → um { umInstances = HM.empty }
    writeIORef (gameTimeRef env) 0
    -- every random stream, pinned BEFORE spawning
    writeIORef (statRNGRef env) (Random.mkStdGen statSeed)
    writeIORef (treatRNGRef env) (Random.mkStdGen treatSeed)
    epoch ← pageIncarnation ws
    let spawn uid name gx = handleUnitSpawnCommand env
            (ucUtsRef (toUnitCombatCapability env)) (UnitId uid) name gx 10.0
            terrainZ (resolveLegacyFaction [] FactionPlayer) encounterPage epoch
            SpawnStandard
    spawn 1 "nomad_primitive" 10.0
    spawn 2 "acolyte" 10.5
    act sc

-- | One fighter's scheduling state.
data Fighter = Fighter
    { fUid      ∷ !Int
    , fTarget   ∷ !Int
    , fLastAt   ∷ !Double
    , fLastMode ∷ !Text
    , fIssued   ∷ !Int
    , fAdmitted ∷ !Int
    }

-- | How the encounter ended.
data Ending = Capped | Stopped !Text deriving (Show, Eq)

encounterSpec ∷ Spec
encounterSpec = describe "E: a bounded encounter" $
    it "an engaged nomad keeps its stamina fraction at or above 0.1 and never \
       \dies of exhaustion in an ordinary encounter against an armed acolyte" $
        withEncounter $ \(env, ls) → do
            -- both units: thermo Nominal, so there is no shiver at all
            forM_ [1, 2 ∷ Int] $ \uid →
                luaText ls ("return tostring(__physiology.setMode(" <> tshow uid
                            <> ", 'thermo', 'nominal'))") `shouldReturn` "true"
            _ ← luaText ls $ T.unlines
                [ "_G.__eng = {}"
                , "local real = unit.applyCombatEngagement"
                , "unit.applyCombatEngagement = function(uid)"
                , "  local o, c = real(uid)"
                , "  __eng[#__eng + 1] = tostring(uid) .. ':' .. tostring(o)"
                , "  return o, c"
                , "end"
                , "return true" ]
            -- the arena: facing each other, 0.5 tiles apart, held there (no AI
            -- runs in this engine, so nothing closes the distance)
            maxima ← forM [1, 2 ∷ Int] $ \uid →
                luaNum ls ("unit.getMaxStamina(" <> tshow uid <> ")")
            maxima `shouldBe` [15, 10]
            _ ← drainCommands env
            gen ← newIORef (Random.mkStdGen combatSeed)
            fighters ← newIORef
                [ Fighter 1 2 (-100) "quick" 0 0, Fighter 2 1 (-100) "quick" 0 0 ]
            samples ← newIORef ([] ∷ [(Double, Double)])
            ending ← newIORef Capped
            exhausted ← newIORef False
            let loop step
                  | step > 600 = pure ()
                  | otherwise = do
                      let t = fromIntegral step * 0.1 ∷ Double
                      writeIORef (gameTimeRef env) t
                      -- a death alert auto-pauses the engine, which would stop
                      -- every later tick: the run ends at the first collapse
                      -- or death, so a pause is only possible after it
                      writeIORef (enginePausedRef env) False
                      runSeconds ls 0.1 0.1
                      fs ← readIORef fighters
                      fs' ← forM fs $ \f → schedule env ls gen t f
                      writeIORef fighters fs'
                      when (step `mod` 5 ≡ 0) $ do
                          fr ← fractionOf env 1
                          modifyIORef' samples ((t, fr) :)
                      cmds ← drainCommands env
                      let downed = [ (u, "collapse") | UnitCollapse u ← cmds ]
                                ⧺ [ (u, "kill") | UnitKill u ← cmds ]
                      case downed of
                          [] → loop (step + 1)
                          ((UnitId u, what) : _) → do
                              writeIORef ending (Stopped (what <> " of unit "
                                                          <> tshow u <> " at "
                                                          <> tshow t))
                              fr ← fractionOf env 1
                              -- exhaustion kills only an empty pool
                              when (any (\(UnitId k, w) → k ≡ 1 ∧ w ≡ "kill") downed
                                    ∧ fr ≤ 0) $
                                  writeIORef exhausted True
            loop (1 ∷ Int)
            fsEnd ← readIORef fighters
            sampled ← reverse <$> readIORef samples
            how ← readIORef ending
            outcomes ← luaText ls "return table.concat(__eng, ',')"
            marker ← HM.lookup "combat_engaged" ∘ uiStats <$> readInstance env 1
            let admittedBy uid = sum [ fAdmitted f | f ← fsEnd, fUid f ≡ uid ]
                summary = "ending=" <> show how <> " admitted(nomad,acolyte)="
                    <> show (admittedBy 1, admittedBy 2) <> " marker="
                    <> show marker <> " engagement=" <> T.unpack outcomes
                    <> " samples=" <> show (length sampled)
            -- VALIDITY, before the oracle is read: each unit threw at least
            -- one admitted strike, and the nomad's marker reached 2 by
            -- `transitioned`. A capped run with either missing is a FIXTURE
            -- failure, not a pass.
            (summary, admittedBy 1 ≥ 1) `shouldBe` (summary, True)
            (summary, admittedBy 2 ≥ 1) `shouldBe` (summary, True)
            (summary, marker) `shouldBe` (summary, Just 2)
            (summary, "1:transitioned" `T.isInfixOf` outcomes) `shouldBe` (summary, True)
            -- the oracle, given validity
            forM_ sampled $ \(t, fr) →
                (summary, t, fr ≥ 0.1) `shouldBe` (summary, t, True)
            readIORef exhausted `shouldReturn` False

-- | One fighter's turn: attack when the shipped cooldown rule and the
--   stance gate allow, through the REAL resolution on the shared stream.
schedule ∷ EngineEnv → LuaBackendState → IORef Random.StdGen → Double
         → Fighter → IO Fighter
schedule env ls genRef t f = do
    cooldown ← luaNum ls
        ("require('scripts.unit_ai_combat').computeAttackCooldown("
         <> tshow (fUid f) <> ", '" <> fLastMode f <> "')")
    stance ← HM.lookupDefault 1.0 "stance" ∘ uiStats <$> readInstance env (fUid f)
    if t - fLastAt f ≥ cooldown ∧ stance ≥ 0.25
        then do
            let issued = fIssued f + 1
                scheduled = if issued `mod` 4 ≡ 0 then Heavy else Quick
                -- the shipped AI downgrades a Heavy it is not set for
                mode = if scheduled ≡ Heavy ∧ stance < 0.5 then Quick else scheduled
            g ← readIORef genRef
            g' ← resolveAttack env g (fromIntegral (fUid f))
                     (fromIntegral (fTarget f)) mode 0 0
            writeIORef genRef g'
            stance' ← HM.lookupDefault 1.0 "stance" ∘ uiStats
                          <$> readInstance env (fUid f)
            -- admitted iff the commit spent the stance
            let admittedNow = stance' < stance
            pure f { fLastAt = t
                   , fLastMode = if mode ≡ Heavy then "heavy" else "quick"
                   , fIssued = issued
                   , fAdmitted = fAdmitted f + (if admittedNow then 1 else 0) }
        else pure f
