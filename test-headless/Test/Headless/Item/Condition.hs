{-# LANGUAGE Strict #-}
-- | Condition is runtime wear state, not authored item data (#1421),
--   and a ground spawn's own arguments are held to their domains.
--
--   Four claims are gated here, each against the production code
--   rather than a restatement of it:
--
--   1. __Fresh items start at 100.__ Nothing rolls a starting condition
--      any more, so the loadout materialisers, the Lua inventory grant
--      and a craft all produce pristine instances — recursively
--      materialised container contents included. The type system takes
--      care of the rest: @idConditionSpec@ no longer exists, so no site
--      can roll a condition from a definition even if it wanted to, and
--      an item YAML that still authors @condition:@ is REFUSED rather
--      than quietly ignored. (The dig and forage yield paths build
--      their instances the same way but need a real terrain / flora
--      world to reach; @tools/craft_probe.py@ and
--      @tools/item_instance_probe.py@ cover them in a live engine.)
--
--   2. __Ground spawn is the one exception, and its arithmetic is
--      specific.__ @item.spawnGround@ is the salvage path, and its
--      condition is the difference of two independent draws. The
--      combination is checked against the DRAWS, never the resulting
--      range: @rand(80,100) − rand(0,20)@ and a flat @rand(60,100)@
--      share both bounds AND the mean of 80, so a range check or an
--      average cannot tell them apart — only the shape can, and the
--      shape is what the design asks for. An explicit base outside
--      condition's 0-100 domain is REFUSED rather than clamped
--      (#1790), because clamping the RESULT is what let a base of 120
--      guarantee the pristine condition this path does not offer.
--
--   3. __Condition is universal, and DISPLAY keys on its value.__ All
--      three Lua exporters push it for every item, and the two panels
--      that show a condition line show it only below 100, so a line
--      appearing means the item has actually taken wear.
--
--   4. __A spawn's GEOMETRY has a domain too, and it belongs to the
--      stored value.__ @item.spawnGround@,
--      @world.spawnLocationSignificantItem@ and @blood.spawn@ each
--      refuse a coordinate — and @blood.spawn@ every control it keeps
--      as a 'Float' — that is missing, unconvertible, NaN, infinite, or
--      finite in Lua but infinite once narrowed (#2336). Nothing
--      downstream raises on one: @floor@ of a NaN is 0, so the item or
--      decal resolves tile (0, 0) with NaN offsets and renders to a
--      discarded quad. A ground item's position then persists exactly,
--      so the load boundary drops an entry a pre-#2336 build already
--      let through — warning once, never refusing the load, and
--      leaving every reference to it dangling rather than inventing a
--      replacement.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Item.Condition"'@.
module Test.Headless.Item.Condition (spec) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Control.Monad (foldM)
import Data.Either (isLeft, isRight)
import Data.List (find, sort)
import Data.IORef
    (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Combat.Resolution.Damage (ResolvedStrike(..), resolveStrike)
import Craft.Types
    ( RecipeDef(..), RecipeIngredient(..), RecipeManager(..) )
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Asset.YamlItems (ItemYamlDef(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Equipment.Types (EquipmentClass(..), EquipmentSlot(..))
import Blood.Types (BloodDecal(..), BloodStore(..), allDecals)
import Building.Types (BuildingId(..))
import Engine.Core.Log
    ( LogBackend(..), LogCategory(..), LogConfig(..), LogEntry(..)
    , LogLevel(..), LoggerState, defaultLogConfig, initLogger )
import Engine.Graphics.Camera (CameraFacing(..))
import Item.Ground
    ( GroundItem(..), GroundItems(..), groundPositionIsFinite
    , sanitizeGroundItems, spawnGroundItem )
import Location.Instance
    ( LocationInstance(..), LocationInstanceId(..), LocationInstances(..)
    , LocationLifecycle(..), LocationSignificantItem(..)
    , emptyLocationInstances, firstLocationInstanceId
    , lookupLocationInstance )
import Location.Bounds (AbsBounds(..))
import Structure.Palette (emptyTexPalette)
import Unit.Transfer
    ( QueuedTransfer(..), TransferBatch(..), TransferEndpoint(..)
    , TransferItemRef(..), TransferState(..) )
import Unit.Transfer.Orders
    (TransferOrders, addTransferOrder, emptyTransferOrders)
import World.Chunk.Types (ChunkCoord(..))
import World.Generate.Types (WorldGenParams(..), defaultWorldGenParams)
import World.Load.Stage
    (stageSession, renderStageError, stagedGroundItemWarning)
import World.Load.Types (StagedPage(..), StagedSession(..))
import World.Save.Component.Page (blankPageSnapshot)
import World.Save.Snapshot
    (LiveCameraSnapshot(..), PageSnapshot(..), SessionSnapshot(..))
import World.Save.Integrity
    (IntegrityError(..), LuaRefEdge(..), luaReferenceErrors)
import World.Save.Payload (LoadReconcileContext(..))
import World.Save.Snapshot.Adapter (SaveRequestMeta(..), snapshotToSaveData)
import World.Save.Types (SaveData, UnitSnapshot(..), toUnitSnapshot)
import Engine.Scripting.Lua.API.Save.Integrity (knownEntitiesFromSaveData)
import Item.Roll
    ( GroundConditionBase, groundConditionBaseDomain
    , groundConditionBaseRange, groundConditionPenaltyRange
    , groundQualityFallbackRange, mkGroundConditionBase
    , rollGroundCondition, rollGroundQuality
    , rollItemSpec, salvageCondition )
import Item.Types
    ( ItemContentEntry(..), ItemDef(..), ItemInstance(..), ItemManager(..)
    , ItemWeapon(..) )
import Substance.Types (emptySubstanceManager)
import System.Random (StdGen, mkStdGen, randomR)
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Thread.Command.Spawn
    ( buildStartingAccessories, buildStartingEquipment
    , buildStartingInventory )
import Unit.Types
    ( UnitDef(..), UnitId(..), UnitInstance(..), UnitManager(..)
    , defaultNaturalResistance, emptyUnitManager )
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager )

-- * Item fixtures
--
-- Two of these three defs carried a @condition:@ block before #1421
-- ("worn_tool", "kit"); "ration" never did, so it is the def whose
-- items used to report NO condition to Lua at all.

page ∷ WorldPageId
page = WorldPageId "item_condition_page"

holderUid ∷ UnitId
holderUid = UnitId 1

bareDef ∷ Text → Text → ItemDef
bareDef name kind = ItemDef
    { idName = name, idDisplayName = name, idTexture = TextureHandle 0, idIconTexture = TextureHandle 0
    , idWeight = 0.5, idWeightSpec = Nothing, idBulk = 1.0
    , idStorage = Nothing, idKind = kind
    , idCategory = "Misc", idMake = "", idMaterial = ""
    , idQualitySpec = Nothing, idQualityTiers = []
    , idContainer = Nothing, idDefaultContents = []
    , idFood = Nothing, idWeapon = Nothing, idArmor = Nothing
    , idUnequippable = False, idBuffs = [], idInsulation = 0
    , idSourcePath = "test-fixture"
    }

-- | A tool with its own quality spec, and a kit that materialises two
--   of them as default contents so the recursive branch is exercised.
testItems ∷ ItemManager
testItems = ItemManager $ HM.fromList
    [ ("worn_tool", (bareDef "worn_tool" "tool")
                      { idQualitySpec = Just (50, 75) })
    , ("kit",       (bareDef "kit" "misc")
                      { idDefaultContents =
                          [ ItemContentEntry
                              { iceItem = "worn_tool", iceCount = 2
                              , iceFill = Nothing, iceContents = Nothing } ] })
    , ("ration",    bareDef "ration" "misc")
    , ("steel_bar", bareDef "steel_bar" "misc")
    ]

humanoid ∷ EquipmentClass
humanoid = EquipmentClass
    { ecName = "humanoid", ecSilhouetteTex = TextureHandle 0
    , ecSilhouetteW = 64, ecSilhouetteH = 64
    , ecSlots = [ EquipmentSlot { esId = "right_hand", esName = "Right Hand"
                                , esKind = "tool", esX = 0, esY = 0
                                , esW = 32, esH = 32 } ]
    }

barRecipe ∷ RecipeDef
barRecipe = RecipeDef
    { rdId = "shape_tool", rdName = "Shape Tool", rdStation = "forge"
    , rdInputs = [RecipeIngredient "steel_bar" 1]
    , rdFuel = Nothing, rdWork = 1
    , rdOutputs = [RecipeIngredient "worn_tool" 1]
    , rdKnowledge = Nothing, rdSkill = Nothing
    , rdRepairAxis = Nothing, rdOutputTemp = Nothing, rdPowerDraw = 0
    }

-- * Unit fixture

minimalUnitDef ∷ UnitDef
minimalUnitDef = UnitDef
    { udName = "acolyte", udNamePool = Nothing
    , udDisplayName = Just "Acolyte"
    , udTexture = TextureHandle 0, udPortrait = Nothing
    , udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing, udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts = []
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = []
    }

mkItem ∷ Text → Word64 → Float → ItemInstance
mkItem name iid cond = ItemInstance
    { iiDefName = name, iiCurrentFill = 0
    , iiQuality = 100, iiCondition = cond
    , iiWeight = 0.5, iiSharpness = 100
    , iiContents = [], iiInstanceId = iid
    , iiTemp = Nothing, iiBulk = Just 1, iiStorage = Nothing
    }

-- | The holder carries a worn tool loose, wears a pristine one in a
--   slot, and has a ration as an accessory — one item per Lua exporter.
holder ∷ UnitInstance
holder = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = page
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = 0, uiGridY = 0, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.empty, uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty
    , uiInventory = [ mkItem "ration" 10 100, mkItem "worn_tool" 11 74 ]
    , uiEquipment = HM.singleton "right_hand" (mkItem "worn_tool" 12 100)
    , uiAccessories = [ mkItem "ration" 13 100 ]
    , uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing
    , uiTrailState = Nothing
    }

-- | One visible page holding the item registry, the holder and a
--   recipe, so every verb below has something real to read.
resetScene ∷ EngineEnv → IO WorldState
resetScene env = do
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(page, ws)], wmVisible = [page] }
    writeIORef (itemManagerRef env) testItems
    writeIORef (recipeManagerRef env) (RecipeManager (HM.singleton "shape_tool" barRecipe))
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs      = HM.singleton "acolyte" minimalUnitDef
        , umInstances = HM.singleton holderUid holder
        }
    pure ws

-- * Lua plumbing

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

run ∷ LuaBackendState → Text → IO Text
run ls = executeDebugLua (lbsLuaState ls)

-- | Fail loudly on a Lua error instead of comparing against its text.
runOk ∷ LuaBackendState → Text → IO Text
runOk ls src = do
    r ← run ls src
    r `shouldNotSatisfy` isLuaError
    pure r

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-- | Debug-console return values come back JSON-encoded, so a Lua
--   string arrives quoted.
q ∷ Text → Text
q t = "\"" <> t <> "\""

-- | The debug console is single-line, so a snippet is joined with
--   spaces rather than newlines.
luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

-- * Draw helpers
--
-- The salvage draws are reproduced here from a known seed so the
-- production combination can be compared against the two draws that
-- fed it, rather than against the range they happen to land in.

drawUniform ∷ (Float, Float) → StdGen → (Float, StdGen)
drawUniform = randomR

-- | Run an IO roll against a fresh generator from @seed@, returning
--   both the value and the generator's final state.
withSeed ∷ Int → (IORef StdGen → IO α) → IO (α, StdGen)
withSeed seed act = do
    ref ← newIORef (mkStdGen seed)
    v ← act ref
    g ← readIORef ref
    pure (v, g)

-- | Fraction of @n@ salvage conditions landing inside [lo, hi].
sampleFraction ∷ Int → Maybe GroundConditionBase → (Float, Float) → IO Double
sampleFraction n mBase (lo, hi) = do
    ref ← newIORef (mkStdGen 4242)
    hits ← foldM (\acc _ → do
                v ← rollGroundCondition mBase ref
                pure (if v ≥ lo ∧ v ≤ hi then acc + 1 else acc))
             (0 ∷ Int) [1 .. n]
    pure (fromIntegral hits / fromIntegral n)

sampleAll ∷ Int → Maybe GroundConditionBase → IO [Float]
sampleAll n mBase = do
    ref ← newIORef (mkStdGen 99)
    mapM (\_ → rollGroundCondition mBase ref) [1 .. n]

-- * Ground-spawn observation (#1790)
--
-- Every surface a REFUSED @item.spawnGround@ must leave alone, sampled
-- together: the page's ground entries, its own id allocator, the
-- process-wide item-instance counter, and the shared stat RNG. The RNG
-- is read as the draw it WOULD next produce rather than by consuming
-- one, so observing costs nothing and the sample can be taken on both
-- sides of a call.

data GroundObservation = GroundObservation
    { obsGroundIds  ∷ [Int]
    , obsNextGid    ∷ Int
    , obsNextItemId ∷ Word64
    , obsNextDraw   ∷ Float
    } deriving (Eq, Show)

observeGround ∷ EngineEnv → WorldState → IO GroundObservation
observeGround env ws = do
    gis ← readIORef (wsGroundItemsRef ws)
    nid ← readIORef (nextItemInstanceIdRef env)
    g   ← readIORef (statRNGRef env)
    pure GroundObservation
        { obsGroundIds  = sort (HM.keys (gisItems gis))
        , obsNextGid    = gisNextId gis
        , obsNextItemId = nid
        , obsNextDraw   = fst (drawUniform (0, 1) g)
        }

-- | The out-of-domain explicit conditions the verb must refuse, as the
--   Lua expressions that produce them. @0/0@ and @±1/0@ are Lua 5.4
--   float division, so they arrive as genuine NaN and infinities
--   rather than as parse errors.
rejectedConditionExprs ∷ [(String, Text)]
rejectedConditionExprs =
    [ ("far above the domain",  "120")
    , ("just above 100",        "100.0001")
    , ("far below the domain",  "-5")
    , ("just below 0",          "-0.0001")
    , ("NaN",                   "0/0")
    , ("+Infinity",             "1/0")
    , ("-Infinity",             "-1/0")
    ]

-- * Non-finite spawn geometry (#2336)
--
-- The distinction every table below turns on: the domain belongs to
-- the value that is STORED, not to the number Lua handed over. A
-- @GroundItem@ and a @BloodDecal@ keep 'Float's, so @1e39@ — a
-- perfectly ordinary finite Lua number — is out of the domain because
-- narrowing it produces an infinity, and a check written against the
-- incoming 'Double' would wave it through.

-- | Which coordinate argument carries the poison. Every ground-spawn
--   example below runs both, so an x-only guard cannot satisfy the
--   contract.
data CoordSlot = SlotX | SlotY deriving (Eq, Show)

coordSlots ∷ [CoordSlot]
coordSlots = [SlotX, SlotY]

coordSlotName ∷ CoordSlot → String
coordSlotName SlotX = "x"
coordSlotName SlotY = "y"

-- | The coordinates both ground-spawn verbs must refuse, as the Lua
--   expressions that produce them.
--
--   @nil@ and a non-numeric string are what @Lua.tonumber@ itself
--   refuses — the "missing or unconvertible" half, which is unchanged
--   behaviour for the arity case and newly load-bearing for the rest.
--   @0/0@ and @±math.huge@ are genuine NaN and infinities under Lua
--   5.4 float division. @±1e39@ is the case only a post-narrowing check
--   catches.
rejectedCoordExprs ∷ [(String, Text)]
rejectedCoordExprs =
    [ ("missing",                        "nil")
    , ("unconvertible",                  "'not a number'")
    , ("NaN",                            "0/0")
    , ("+Infinity",                      "math.huge")
    , ("-Infinity",                      "-math.huge")
    , ("finite but overflowing a Float", "1e39")
    , ("finite but overflowing a Float, negative", "-1e39")
    ]

-- | @item.spawnGround@ with @expr@ in @slot@ and a good coordinate in
--   the other.
spawnGroundCall ∷ CoordSlot → Text → Text
spawnGroundCall SlotX expr = "item.spawnGround('ration', " <> expr <> ", 4)"
spawnGroundCall SlotY expr = "item.spawnGround('ration', 3, " <> expr <> ")"

-- | @world.spawnLocationSignificantItem@ against 'significantIid''s
--   one owed slot, likewise.
significantCall ∷ CoordSlot → Text → Text
significantCall SlotX expr =
    "world.spawnLocationSignificantItem(1, 1, " <> expr <> ", 4)"
significantCall SlotY expr =
    "world.spawnLocationSignificantItem(1, 1, 3, " <> expr <> ")"

-- * The significant-obligation fixture (#917, borrowed)

significantIid ∷ LocationInstanceId
significantIid = LocationInstanceId 1

significantSlot ∷ Int
significantSlot = 1

-- | A placed location owing exactly ONE unbound "ration", so
--   @world.spawnLocationSignificantItem@ has a real obligation to fill
--   and a refusal has a real binding to leave alone. Every other field
--   is at a value these examples never read.
owedInstance ∷ LocationInstance
owedInstance = LocationInstance
    { liId              = significantIid
    , liDefId           = "ruin_small"
    , liChunk           = ChunkCoord 0 0
    , liAnchor          = (0, 0)
    , liBounds          = AbsBounds 0 0 8 8
    , liDisplayName     = "Small Ruin"
    , liGloss           = Nothing
    , liEtymology       = Nothing
    , liLifecycle       = LifecycleDiscovered
    , liContentsSpawned = False
    , liEncounter       = Nothing
    , liSignificant     =
        [ LocationSignificantItem
            { lsiSlot        = significantSlot
            , lsiItemDefName = "ration"
            , lsiInstanceId  = Nothing
            , lsiTaken       = False } ]
    , liClearEventEmitted = False
    }

-- | 'resetScene' plus that obligation on the page's gen params.
resetSignificantScene ∷ EngineEnv → IO WorldState
resetSignificantScene env = do
    ws ← resetScene env
    writeIORef (wsGenParamsRef ws) $ Just defaultWorldGenParams
        { wgpLocationInstances = emptyLocationInstances
            { lisNextId = firstLocationInstanceId + 1
            , lisById   = HM.singleton significantIid owedInstance } }
    pure ws

-- | Everything a REFUSED significant spawn must leave alone: all four
--   ground-spawn observables, plus the obligation's own binding and
--   its taken latch.
data SignificantObservation = SignificantObservation
    { sigGround  ∷ GroundObservation
    , sigBinding ∷ Maybe (Maybe Word64, Bool)
    } deriving (Eq, Show)

observeSignificant ∷ EngineEnv → WorldState → IO SignificantObservation
observeSignificant env ws = do
    g ← observeGround env ws
    mParams ← readIORef (wsGenParamsRef ws)
    let binding = do
            p    ← mParams
            inst ← lookupLocationInstance significantIid
                       (wgpLocationInstances p)
            e    ← find ((≡ significantSlot) ∘ lsiSlot) (liSignificant inst)
            pure (lsiInstanceId e, lsiTaken e)
    pure (SignificantObservation g binding)

-- * The blood-decal fixture

-- | Every @blood.spawn@ control the decal keeps as a 'Float'. The two
--   positional coordinates are named here beside the five props so a
--   control added later is either in this list or visibly absent.
bloodGeometryFields ∷ [Text]
bloodGeometryFields =
    ["x", "y", "offsetX", "offsetY", "rotation", "scale", "opacity"]

-- | The values that are non-finite ONCE STORED — the sweep every one of
--   those fields gets.
bloodNonFiniteExprs ∷ [(String, Text)]
bloodNonFiniteExprs =
    [ ("NaN",                            "0/0")
    , ("+Infinity",                      "math.huge")
    , ("-Infinity",                      "-math.huge")
    , ("finite but overflowing a Float", "1e39")
    ]

-- | @blood.spawn@ with @expr@ in @field@ and everything else left at a
--   value the verb accepts.
bloodSpawnCall ∷ Text → Text → Text
bloodSpawnCall "x" expr = "blood.spawn(" <> expr <> ", 2, 'cut', 'minor')"
bloodSpawnCall "y" expr = "blood.spawn(1, " <> expr <> ", 'cut', 'minor')"
bloodSpawnCall field expr =
    "blood.spawn(1, 2, 'cut', 'minor', {" <> field <> " = " <> expr <> "})"

-- | Both of @blood.spawn@'s return values, folded into one string,
--   because the debug console reports only the first.
bloodResult ∷ LuaBackendState → Text → IO Text
bloodResult ls call = runOk ls $
    "local a, b = " <> call <> "; return tostring(a) .. '|' .. tostring(b)"

-- * The staged-load fixture (#2336 requirement 3)

stagedPageId ∷ WorldPageId
stagedPageId = WorldPageId "item_condition_staged"

-- | The three ground entries the forged save carries. Ids and instance
--   ids are distinct and hand-written so every assertion below can name
--   exactly one of them.
--
--   Only ONE is poisoned, which is what makes "exactly one warning"
--   measurable, and it sits BETWEEN two good entries so a filter that
--   truncated instead of filtering would be caught.
goodGidA, poisonedGid, goodGidB ∷ Int
goodGidA    = 0
poisonedGid = 1
goodGidB    = 2

goodIidA, poisonedIid, goodIidB ∷ Word64
goodIidA    = 900
poisonedIid = 901
goodIidB    = 902

-- | @gisNextId@ deliberately runs AHEAD of the highest live id: a
--   dropped entry must not rewind it, and an allocator equal to
--   @maximum + 1@ could not tell "preserved" from "recomputed".
savedGroundNextId ∷ Int
savedGroundNextId = 40

savedGroundItems ∷ GroundItems
savedGroundItems = GroundItems
    { gisNextId = savedGroundNextId
    , gisItems  = HM.fromList
        [ (goodGidA,    GroundItem (mkItem "ration" goodIidA 100) 3 4)
        , (poisonedGid, GroundItem (mkItem "worn_tool" poisonedIid 60)
                                   (0 / 0) 4)
        , (goodGidB,    GroundItem (mkItem "ration" goodIidB 100) 5 6)
        ] }

-- | One durable order naming BOTH a surviving ground item and the
--   dropped one, so the load-time dangling diagnostic reports exactly
--   the reference the sanitation invalidated. If staging built its
--   entity set from the DECODED map instead of the sanitized one,
--   neither would be reported.
savedOrders ∷ TransferOrders
savedOrders = case addTransferOrder (UnitId 11) batch emptyTransferOrders of
    Just (orders, _) → orders
    Nothing → error "fixture: addTransferOrder refused a fresh allocator"
  where
    batch = TransferBatch
        { tbSource      = EndpointUnit (UnitId 11)
        , tbDestination = EndpointBuilding (BuildingId 777)
        , tbEntries =
            [ QueuedTransfer
                { qtItem = TransferItemRef
                    { tirInstanceId = fromIntegral goodIidA
                    , tirDefName    = "ration" }
                , qtState = TransferQueued }
            , QueuedTransfer
                { qtItem = TransferItemRef
                    { tirInstanceId = fromIntegral poisonedIid
                    , tirDefName    = "worn_tool" }
                , qtState = TransferQueued }
            ] }

-- | The obligation the save carries already BOUND to the item staging
--   is about to drop. Nothing may rebind it, clear it, or latch it
--   taken: a dangling obligation is the honest record of what happened,
--   and inventing a replacement would hand the player a reward the
--   location never spawned.
boundInstance ∷ LocationInstance
boundInstance = owedInstance
    { liSignificant =
        [ LocationSignificantItem
            { lsiSlot        = significantSlot
            , lsiItemDefName = "worn_tool"
            , lsiInstanceId  = Just poisonedIid
            , lsiTaken       = False } ] }

-- | An ARENA page (the empty timeline at seed 0), so staging takes the
--   flat-chunk rebuild instead of generating a world — this is an
--   example about the sanitation, not about worldgen. Mirrors
--   "Test.Headless.World.GenConfigDomain"'s staging fixture.
stagedParams ∷ WorldGenParams
stagedParams = defaultWorldGenParams
    { wgpSeed = 0
    , wgpLocationInstances = emptyLocationInstances
        { lisNextId = firstLocationInstanceId + 1
        , lisById   = HM.singleton significantIid boundInstance } }

-- | The unit that OWNS the Lua @ground_item@ reference below.
--   'luaEdgeResolves' resolves that kind against the owning unit's page
--   only, so without a real unit on the page every such edge would fail
--   to resolve whatever the ground map said — and the example would
--   prove nothing.
ownerUid ∷ UnitId
ownerUid = UnitId 11

savedUnits ∷ UnitSnapshot
savedUnits = toUnitSnapshot stagedPageId emptyUnitManager
    { umInstances = HM.singleton ownerUid holder { uiPage = stagedPageId }
    , umNextId    = 12 }

-- | The forged save, built the way a real one is: a 'SessionSnapshot'
--   through 'snapshotToSaveData', which is the adapter staging's own
--   input comes from.
--
--   Staged as-is (no units, so the transfer-order example's references
--   dangle by construction); 'savedWithOwner' adds the unit the
--   reference-edge example needs and is never staged.
poisonedSave ∷ SaveData
poisonedSave = poisonedSaveWith (UnitSnapshot HM.empty 0)

savedWithOwner ∷ SaveData
savedWithOwner = poisonedSaveWith savedUnits

poisonedSaveWith ∷ UnitSnapshot → SaveData
poisonedSaveWith units = snapshotToSaveData
    (SaveRequestMeta "item_condition_slot" "2026-09-04T00:00:00.000000Z" False)
    SessionSnapshot
        { snapGameTime       = 0
        , snapTexPalette     = emptyTexPalette
        , snapNextItemId     = savedNextItemId
        , snapNextBuildingId = 1
        , snapNextUnitId     = 1
        , snapActivePage     = stagedPageId
        , snapVisiblePages   = [stagedPageId]
        , snapLiveCamera     = LiveCameraSnapshot
            { lcsOwnerPage = Just stagedPageId
            , lcsX = 0, lcsY = 0, lcsZoom = 1, lcsFacing = FaceSouth }
        , snapPages          = HM.singleton stagedPageId
            ((blankPageSnapshot stagedPageId stagedParams)
                { pgsGroundItems    = savedGroundItems
                , pgsTransferOrders = savedOrders
                , pgsUnits          = units })
        }

-- | The global item-instance cursor the save carries, above every
--   instance id in it, so "preserved" is a real assertion.
savedNextItemId ∷ Word64
savedNextItemId = 1000

-- | A logger whose entries are captured in emission order, so "exactly
--   one warning" can be asserted rather than "at least one".
capturingLogger ∷ IO (LoggerState, IO [LogEntry])
capturingLogger = do
    ref ← newIORef []
    logger ← initLogger defaultLogConfig
        { lcBackend = LogToCallback
            (\e → atomicModifyIORef' ref (\es → (e : es, ()))) }
    pure (logger, reverse ⊚ readIORef ref)

-- | One Lua reference edge of @kind@ naming @rid@, owned by
--   'ownerUid' on the staged page — the shape
--   @scripts\/unit_ai_reconcile.lua@'s repair jobs, pickup orders and
--   forage targets declare.
refEdge ∷ Text → Text → Int → LuaRefEdge
refEdge kind path rid = LuaRefEdge
    { lreComponent = "unit_ai"
    , lreKind      = kind
    , lreId        = rid
    , lreOwner     = Just (fromIntegral (unUnitId ownerUid))
    , lrePath      = path
    , lrePage      = Just (unWorldPageId stagedPageId)
    }

-- | Warnings whose message names @needle@ (mirrors
--   "Test.Headless.World.GenConfigDomain"'s helper).
warningsMentioning ∷ Text → [LogEntry] → [LogEntry]
warningsMentioning needle =
    filter (\e → leLevel e ≡ LevelWarn ∧ needle `T.isInfixOf` leMessage e)

-- | Stage 'poisonedSave' through the REAL entry point, handing back the
--   staged session and its one page beside everything the logger
--   emitted.
--
--   A 'StageError' fails the example outright, so every caller is also
--   asserting that a poisoned save still LOADS — requirement 3's
--   \"never refuses the load for it\".
stagePoisoned ∷ HasCallStack ⇒ EngineEnv
              → IO (StagedSession, WorldState, [LogEntry])
stagePoisoned env = do
    writeIORef (itemManagerRef env) testItems
    (logger, drain) ← capturingLogger
    matReg ← readIORef (materialRegistryRef env)
    staged ← stageSession env logger poisonedSave matReg ⌦ either
        (\e → expectationFailure (T.unpack (renderStageError e))
                ≫ error "unreachable")
        pure
    entries ← drain
    case find ((≡ stagedPageId) ∘ spPageId) (ssPages staged) of
        Nothing → expectationFailure "the staged page is missing"
                    ≫ error "unreachable"
        Just sp → pure (staged, spWorldState sp, entries)

-- * YAML fixtures

itemYaml ∷ [Text] → BS8.ByteString
itemYaml extra = BS8.pack ∘ T.unpack ∘ T.unlines $
    [ "name: probe_item"
    , "sprite: \"assets/textures/items/misc/probe.png\""
    , "bulk: 1.0"
    ] <> extra

decodeItem ∷ BS8.ByteString → Either String ItemYamlDef
decodeItem = either (Left ∘ show) Right ∘ Yaml.decodeEither'

-- * Spec

spec ∷ SpecWith EngineEnv
spec = describe "Item.Condition" $ do

    -- 1. Condition is no longer authorable at all.

    describe "the item YAML schema" $ do
        it "accepts a definition that authors no condition" $ \_ →
            decodeItem (itemYaml []) `shouldSatisfy` isRight

        it "REFUSES a definition that still authors a condition range, \
           \rather than ignoring the retired key" $ \_ → do
            let r = decodeItem (itemYaml [ "condition:"
                                         , "  min: 70"
                                         , "  max: 100" ])
            r `shouldSatisfy` isLeft
            either id (const "") r `shouldSatisfy`
                (("condition is no longer" `T.isInfixOf`) ∘ T.pack)

        it "REFUSES an explicitly null condition too — aeson reads that \
           \as absent, so only an explicit key lookup can see it" $ \_ →
            decodeItem (itemYaml ["condition: null"]) `shouldSatisfy` isLeft

        it "still accepts quality, which stays authored per definition" $ \_ →
            decodeItem (itemYaml [ "quality:", "  min: 50", "  max: 75" ])
                `shouldSatisfy` isRight

    -- 2. Every fresh creation path.

    describe "fresh items start at full condition" $ do

        it "starting_inventory materialises pristine instances, \
           \recursively materialised container contents included" $ \env → do
            _ ← resetScene env
            logger ← readIORef (loggerRef env)
            out ← buildStartingInventory env logger testItems
                    [("worn_tool", Nothing, 0), ("kit", Nothing, 1)]
            map iiDefName (map fst out) `shouldBe` ["worn_tool", "kit"]
            map iiCondition (map fst out) `shouldBe` [100, 100]
            -- The kit's two default tools are built by the same
            -- recursive call, so they are covered by the same rule.
            let contents = concatMap (iiContents ∘ fst) out
            map iiDefName contents `shouldBe` ["worn_tool", "worn_tool"]
            map iiCondition contents `shouldBe` [100, 100]
            -- Quality is untouched: it still rolls from the def's spec.
            all (\i → iiQuality i ≥ 50 ∧ iiQuality i ≤ 75) contents
                `shouldBe` True

        it "starting_equipment fills its slot with a pristine instance" $ \env → do
            _ ← resetScene env
            logger ← readIORef (loggerRef env)
            eq ← buildStartingEquipment env logger testItems (Just humanoid)
                    (HM.singleton "right_hand" "worn_tool")
            map iiCondition (HM.elems eq) `shouldBe` [100]

        it "starting_accessories are pristine" $ \env → do
            _ ← resetScene env
            logger ← readIORef (loggerRef env)
            accs ← buildStartingAccessories env logger testItems ["worn_tool"]
            map iiCondition accs `shouldBe` [100]

        it "unit.addItem grants a pristine instance" $ \env → do
            _ ← resetScene env
            ls ← newBareLuaBackend env
            _ ← runOk ls "unit.addItem(1, 'worn_tool'); return 'ok'"
            r ← runOk ls $ luaLines
                [ "local inv = unit.getInventory(1);"
                , "local last = inv[#inv];"
                , "return string.format('%s=%.1f', last.defName, last.condition)"
                ]
            r `shouldBe` q "worn_tool=100.0"

        it "a craft output is pristine" $ \env → do
            _ ← resetScene env
            ls ← newBareLuaBackend env
            _ ← runOk ls "unit.addItem(1, 'steel_bar'); return 'ok'"
            ok ← runOk ls "local ok = craft.execute(1, 'shape_tool'); return tostring(ok)"
            ok `shouldBe` q "true"
            r ← runOk ls $ luaLines
                [ "local inv = unit.getInventory(1);"
                , "local last = inv[#inv];"
                , "return string.format('%s=%.1f', last.defName, last.condition)"
                ]
            r `shouldBe` q "worn_tool=100.0"

    -- 3. The one exception, and its arithmetic.

    describe "ground salvage (item.spawnGround)" $ do

        it "combines the two draws exactly: base minus penalty" $ \_ → do
            salvageCondition 92 7 `shouldBe` 85
            salvageCondition 80 20 `shouldBe` 60
            salvageCondition 100 0 `shouldBe` 100

        it "clamps a combination below zero to zero" $ \_ → do
            salvageCondition 5 20 `shouldBe` 0
            salvageCondition 0 20 `shouldBe` 0

        it "draws base then penalty from the shared stat RNG and \
           \combines exactly those two values" $ \_ → do
            let g0            = mkStdGen 1234
                (base, g1)    = drawUniform groundConditionBaseRange g0
                (penalty, g2) = drawUniform groundConditionPenaltyRange g1
            (v, gEnd) ← withSeed 1234 (rollGroundCondition Nothing)
            v `shouldBe` salvageCondition base penalty
            -- Exactly two draws were consumed, so a later gameplay roll
            -- from the same shared generator is unaffected.
            fst (drawUniform (0, 1 ∷ Float) gEnd)
                `shouldBe` fst (drawUniform (0, 1 ∷ Float) g2)

        it "spends ONE draw — the penalty — when the caller names the \
           \base, and never suppresses that penalty" $ \_ → do
            let g0            = mkStdGen 777
                (penalty, g1) = drawUniform groundConditionPenaltyRange g0
            (v, gEnd) ← withSeed 777
                (rollGroundCondition (mkGroundConditionBase 80))
            v `shouldBe` salvageCondition 80 penalty
            fst (drawUniform (0, 1 ∷ Float) gEnd)
                `shouldBe` fst (drawUniform (0, 1 ∷ Float) g1)

        it "an explicit condition of 7 is a BASE, so it lands worn — \
           \never at 100" $ \_ → do
            vs ← sampleAll 200 (mkGroundConditionBase 7)
            all (\v → v ≥ 0 ∧ v ≤ 7) vs `shouldBe` True

        it "stays within [60, 100] with no explicit prop" $ \_ → do
            vs ← sampleAll 2000 Nothing
            all (\v → v ≥ 60 ∧ v ≤ 100) vs `shouldBe` True

        it "stays within [60, 80] for an explicit base of 80" $ \_ → do
            vs ← sampleAll 2000 (mkGroundConditionBase 80)
            all (\v → v ≥ 60 ∧ v ≤ 80) vs `shouldBe` True

        it "is TRIANGULAR, not the flat rand(60,100) that shares its \
           \bounds and its mean" $ \_ → do
            -- Difference of two equal-width uniforms peaks at 80:
            -- P(75 ≤ X ≤ 85) = 0.4375, against 0.25 if it were flat.
            f ← sampleFraction 8000 Nothing (75, 85)
            f `shouldSatisfy` (> 0.35)
            f `shouldSatisfy` (< 0.55)

        it "quality: an explicit prop replaces the roll and spends no \
           \draw" $ \_ → do
            let toolDef = testItems `defNamed` "worn_tool"
            (v, gEnd) ← withSeed 55 (rollGroundQuality toolDef (Just 42))
            v `shouldBe` 42
            fst (drawUniform (0, 1 ∷ Float) gEnd)
                `shouldBe` fst (drawUniform (0, 1 ∷ Float) (mkStdGen 55))

        it "quality: rolls the definition's own spec when it has one" $ \_ → do
            let toolDef = testItems `defNamed` "worn_tool"
            (v, _)   ← withSeed 91 (rollGroundQuality toolDef Nothing)
            (ref, _) ← withSeed 91 (rollItemSpec (Just (50, 75)))
            v `shouldBe` ref

        it "quality: falls back to 20-80 with the SAME truncated-normal \
           \semantics every other quality roll uses" $ \_ → do
            let plain = testItems `defNamed` "ration"
            groundQualityFallbackRange `shouldBe` (20, 80)
            (v, _)   ← withSeed 17 (rollGroundQuality plain Nothing)
            (ref, _) ← withSeed 17 (rollItemSpec (Just groundQualityFallbackRange))
            v `shouldBe` ref
            -- A uniform randomR (20,80) would be flat; this one
            -- concentrates around the midpoint (σ = range/4).
            ref2 ← newIORef (mkStdGen 8)
            vs ← mapM (\_ → rollGroundQuality plain Nothing ref2) [1 .. 2000 ∷ Int]
            all (\x → x ≥ 20 ∧ x ≤ 80) vs `shouldBe` True
            let mid = length (filter (\x → x ≥ 35 ∧ x ≤ 65) vs)
            (fromIntegral mid / 2000 ∷ Double) `shouldSatisfy` (> 0.6)

        it "spawns a worn item through the real verb, and a pristine \
           \one is unreachable through it" $ \env → do
            _ ← resetScene env
            ls ← newBareLuaBackend env
            r ← runOk ls $ luaLines
                [ "local worn = 0;"
                , "for i = 1, 40 do item.spawnGround('ration', 0, 0) end;"
                , "for _, g in ipairs(item.listGround()) do"
                , "  if g.condition < 60 or g.condition > 100 then return 'range' end;"
                , "  if g.condition < 100 then worn = worn + 1 end;"
                , "end;"
                , "return worn > 0 and 'worn' or 'all-pristine'"
                ]
            r `shouldBe` q "worn"

        -- The domain check on an explicit base (#1790).

        it "the domain check accepts every in-domain base, endpoints \
           \included, and rejects everything outside it — non-finite \
           \values included" $ \_ → do
            groundConditionBaseDomain `shouldBe` (0, 100)
            map (isJust ∘ mkGroundConditionBase)
                [0, 0.5, 7, 80, 99.9, 100]
                `shouldBe` replicate 6 True
            map (isJust ∘ mkGroundConditionBase)
                [ 100.0001, 120, 1e9, -0.0001, -5, -1e9
                , 0 / 0, 1 / 0, -1 / 0 ]
                `shouldBe` replicate 9 False

        forM_ rejectedConditionExprs $ \(label, expr) →
            it ("refuses an explicit condition " <> label <> " through \
                \the real verb, answering nil and mutating nothing") $
                \env → do
                    ws ← resetScene env
                    ls ← newBareLuaBackend env
                    writeIORef (statRNGRef env) (mkStdGen 20250827)
                    before ← observeGround env ws
                    r ← runOk ls $
                        "return tostring(item.spawnGround('ration', 3, \
                        \4, {condition = " <> expr <> "}))"
                    r `shouldBe` q "nil"
                    -- No ground entry, no advanced ground-item id, no
                    -- minted ItemInstance id, and the shared stat RNG
                    -- still about to produce the same draw: a refused
                    -- spawn cannot shift a later gameplay roll.
                    after ← observeGround env ws
                    after `shouldBe` before

        it "an ACCEPTED spawn moves all four of those observables, so \
           \the refusals above are not vacuous" $ \env → do
            ws ← resetScene env
            ls ← newBareLuaBackend env
            writeIORef (statRNGRef env) (mkStdGen 20250827)
            before ← observeGround env ws
            r ← runOk ls
                "return tostring(item.spawnGround('ration', 3, 4) ~= nil)"
            r `shouldBe` q "true"
            after ← observeGround env ws
            obsGroundIds after `shouldNotBe` obsGroundIds before
            obsNextGid after `shouldNotBe` obsNextGid before
            obsNextItemId after `shouldNotBe` obsNextItemId before
            obsNextDraw after `shouldNotBe` obsNextDraw before

        it "accepts both domain endpoints through the real verb" $ \env → do
            ws ← resetScene env
            ls ← newBareLuaBackend env
            r ← runOk ls $ luaLines
                [ "local a = item.spawnGround('ration', 0, 0, {condition = 0});"
                , "local b = item.spawnGround('ration', 1, 1, {condition = 100});"
                , "return string.format('%s,%s', tostring(a ~= nil),"
                , "  tostring(b ~= nil))"
                ]
            r `shouldBe` q "true,true"
            gis ← readIORef (wsGroundItemsRef ws)
            HM.size (gisItems gis) `shouldBe` 2

        it "cannot be made to guarantee 100 even at the MAXIMUM valid \
           \base, because the penalty draw still applies" $ \env → do
            ws ← resetScene env
            ls ← newBareLuaBackend env
            -- Naming the quality explicitly spends no draw, so the
            -- penalty is the FIRST draw off this generator and can be
            -- reproduced exactly. 100 is the largest base the domain
            -- admits, and every smaller one is further below 100 after
            -- the same non-negative penalty.
            let seed         = 31337
                (penalty, _) = drawUniform groundConditionPenaltyRange
                                           (mkStdGen seed)
            penalty `shouldSatisfy` (> 0)
            writeIORef (statRNGRef env) (mkStdGen seed)
            r ← runOk ls
                "return tostring(item.spawnGround('ration', 0, 0, \
                \{quality = 50, condition = 100}) ~= nil)"
            r `shouldBe` q "true"
            gis ← readIORef (wsGroundItemsRef ws)
            case HM.elems (gisItems gis) of
                [gi] → do
                    iiCondition (giInst gi)
                        `shouldBe` salvageCondition 100 penalty
                    iiCondition (giInst gi) `shouldSatisfy` (< 100)
                other → expectationFailure $
                    "expected exactly one ground item, got "
                    <> show (length other)

    -- 4. Universal exposure, value-based display.

    describe "condition is exposed to Lua for every item" $ do

        it "unit.getInventory reports a numeric condition even for a \
           \definition that declares no specs at all" $ \env → do
            _ ← resetScene env
            ls ← newBareLuaBackend env
            r ← runOk ls $ luaLines
                [ "local out = {};"
                , "for _, it in ipairs(unit.getInventory(1)) do"
                , "  out[#out+1] = string.format('%s:%s:%.1f', it.defName,"
                , "    type(it.condition), it.condition or -1)"
                , "end;"
                , "return table.concat(out, ',')"
                ]
            r `shouldBe` q "ration:number:100.0,worn_tool:number:74.0"

        it "equipment.getLoadout reports it too" $ \env → do
            _ ← resetScene env
            ls ← newBareLuaBackend env
            r ← runOk ls $ luaLines
                [ "local lo = equipment.getLoadout(1).right_hand;"
                , "return string.format('%s:%.1f', type(lo.condition), lo.condition)"
                ]
            r `shouldBe` q "number:100.0"

        it "pushItemInstance reports it too (equipment.getAccessories)" $ \env → do
            _ ← resetScene env
            ls ← newBareLuaBackend env
            r ← runOk ls $ luaLines
                [ "local a = equipment.getAccessories(1)[1];"
                , "return string.format('%s:%s:%.1f', a.defName,"
                , "  type(a.condition), a.condition)"
                ]
            r `shouldBe` q "ration:number:100.0"

    -- Runtime behaviour is untouched: the broken boundary is still
    -- exactly `<= 0`, and everything above it scales continuously.
    describe "the broken boundary in resolveStrike is unchanged" $ do

        it "treats condition 0 and below as broken, and anything above \
           \it as intact" $ \_ → do
            let intactEff = rsEff (strikeAt 0.0001)
                brokenEff = rsEff (strikeAt 0)
            rsEff (strikeAt 100) `shouldBe` intactEff
            brokenEff `shouldBe` intactEff * 0.15
            rsEff (strikeAt (-1)) `shouldBe` brokenEff
            rsSharpness (strikeAt 0) `shouldBe` rsSharpness (strikeAt 0.0001) * 4

        it "scales rsCondition from the instance's live value" $ \_ → do
            rsCondition (strikeAt 100) `shouldBe` 1
            rsCondition (strikeAt 50)  `shouldBe` 0.5
            rsCondition (strikeAt 0)   `shouldBe` 0

    describe "the panels display condition by VALUE, not presence" $ do

        it "the shared inventory/equipment hint hides a pristine \
           \condition, shows a worn one, and still shows zero" $ \env → do
            _ ← resetScene env
            ls ← newBareLuaBackend env
            r ← runOk ls $ luaLines
                [ "local m = require('scripts.unit_info_v2_items');"
                , "local function has(c)"
                , "  local h = m.buildItemHint({defName='x', weight=1, condition=c});"
                , "  return string.find(h, 'condition:', 1, true) ~= nil"
                , "end;"
                , "return tostring(has(100)) .. ',' .. tostring(has(74))"
                , "  .. ',' .. tostring(has(0))"
                ]
            r `shouldBe` q "false,true,true"

        it "the ground-item panel hides a pristine condition, shows a \
           \worn one, and marks a broken one broken" $ \env → do
            ws ← resetScene env
            gids ← mapM (\(iid, cond) →
                        atomicModifyIORef' (wsGroundItemsRef ws)
                            (spawnGroundItem (mkItem "ration" iid cond) 0 0))
                     [(20, 100), (21, 74), (22, 0)]
            ls ← newBareLuaBackend env
            _ ← runOk ls "require('scripts.item_info_panel'); return 'ok'"
            let readPanel gid = runOk ls $ luaLines
                    [ "local w = require('scripts.item_info_panel');"
                    , "local p = require('scripts.hud.info_panel');"
                    , "item.select(", T.pack (show gid), ");"
                    , "w.update(0);"
                    , "local t = p.tabText.iteminfo or '';"
                    , "return tostring(string.find(t, 'Condition:', 1, true) ~= nil)"
                    , "  .. ',' .. tostring(string.find(t, '(broken)', 1, true) ~= nil)"
                    ]
            case gids of
                [pristine, worn, broken] → do
                    readPanel pristine `shouldReturn` q "false,false"
                    readPanel worn     `shouldReturn` q "true,false"
                    readPanel broken   `shouldReturn` q "true,true"
                _ → expectationFailure "expected three ground items"

    -- 5. Non-finite spawn geometry (#2336), at the three live
    --    boundaries that could admit it and at the load boundary that
    --    could restore it.

    describe "non-finite spawn geometry" $ do

      describe "item.spawnGround coordinates" $ do

        forM_ coordSlots $ \slot →
          forM_ rejectedCoordExprs $ \(label, expr) →
            it ("refuses a " ⧺ coordSlotName slot ⧺ " that is " ⧺ label
                ⧺ ", answering nil and mutating nothing") $ \env → do
                ws ← resetScene env
                ls ← newBareLuaBackend env
                writeIORef (statRNGRef env) (mkStdGen 20260904)
                before ← observeGround env ws
                r ← runOk ls $
                    "return tostring(" <> spawnGroundCall slot expr <> ")"
                r `shouldBe` q "nil"
                -- The same four surfaces #1790's condition refusal
                -- leaves alone, for the same reason: a refused spawn
                -- must not shift a later gameplay roll.
                after ← observeGround env ws
                after `shouldBe` before

        it "still accepts a numeric STRING and every magnitude a Float \
           \can actually hold, so the refusals above are not vacuous" $
          \env → do
            ws ← resetScene env
            ls ← newBareLuaBackend env
            r ← runOk ls $ luaLines
                [ "local a = item.spawnGround('ration', '3.5', 4);"
                , "local b = item.spawnGround('ration', 1e38, -1e38);"
                , "return string.format('%s,%s', tostring(a ~= nil),"
                , "  tostring(b ~= nil))"
                ]
            r `shouldBe` q "true,true"
            gis ← readIORef (wsGroundItemsRef ws)
            sort (map giX (HM.elems (gisItems gis)))
                `shouldBe` [3.5, 1e38]

      describe "world.spawnLocationSignificantItem coordinates" $ do

        forM_ coordSlots $ \slot →
          forM_ rejectedCoordExprs $ \(label, expr) →
            it ("refuses a " ⧺ coordSlotName slot ⧺ " that is " ⧺ label
                ⧺ ", answering false and binding nothing") $ \env → do
                ws ← resetSignificantScene env
                ls ← newBareLuaBackend env
                writeIORef (statRNGRef env) (mkStdGen 20260904)
                before ← observeSignificant env ws
                r ← runOk ls $
                    "return tostring(" <> significantCall slot expr <> ")"
                r `shouldBe` q "false"
                after ← observeSignificant env ws
                after `shouldBe` before
                -- Spelled out as well as compared, so a fixture that
                -- somehow started bound could not make the equality
                -- above pass vacuously.
                sigBinding after `shouldBe` Just (Nothing, False)

        it "an ACCEPTED spawn fills the slot and moves those same \
           \observables" $ \env → do
            ws ← resetSignificantScene env
            ls ← newBareLuaBackend env
            before ← observeSignificant env ws
            sigBinding before `shouldBe` Just (Nothing, False)
            r ← runOk ls $
                "return tostring(" <> significantCall SlotX "3" <> ")"
            r `shouldBe` q "true"
            after ← observeSignificant env ws
            sigGround after `shouldNotBe` sigGround before
            case sigBinding after of
                Just (Just _, False) → pure ()
                other → expectationFailure
                    ("expected the slot bound and untaken, got "
                     ⧺ show other)

      describe "blood.spawn geometry" $ do

        forM_ bloodGeometryFields $ \field →
          forM_ bloodNonFiniteExprs $ \(label, expr) →
            it ("refuses a " ⧺ T.unpack field ⧺ " that is " ⧺ label
                ⧺ ", names it in the reason, and leaves the whole store \
                  \untouched") $ \env → do
                ws ← resetScene env
                ls ← newBareLuaBackend env
                before ← readIORef (wsBloodStoreRef ws)
                r ← bloodResult ls (bloodSpawnCall field expr)
                r `shouldSatisfy` T.isInfixOf "nil|blood.spawn: "
                -- The reason names the field that was actually
                -- poisoned, so a guard that checked the wrong one
                -- cannot pass.
                r `shouldSatisfy` T.isInfixOf (field <> " = ")
                readIORef (wsBloodStoreRef ws) `shouldReturn` before

        forM_ [ ("scale",   "zero",     "0")
              , ("scale",   "negative", "-1")
              , ("opacity", "below 0",  "-0.0001")
              , ("opacity", "above 1",  "2") ] $ \(field, label, expr) →
            it ("refuses a " ⧺ T.unpack field ⧺ " " ⧺ label
                ⧺ ", leaving the whole store untouched") $ \env → do
                ws ← resetScene env
                ls ← newBareLuaBackend env
                before ← readIORef (wsBloodStoreRef ws)
                r ← bloodResult ls (bloodSpawnCall field expr)
                r `shouldSatisfy`
                    T.isInfixOf ("nil|blood.spawn: " <> field <> " = ")
                readIORef (wsBloodStoreRef ws) `shouldReturn` before

        it "accepts a positive finite scale and BOTH opacity endpoints, \
           \storing each unchanged" $ \env → do
            ws ← resetScene env
            ls ← newBareLuaBackend env
            r ← runOk ls $ luaLines
                [ "local a = blood.spawn(1, 2, 'cut', 'minor',"
                , "  {scale = 0.5, opacity = 0});"
                , "local b = blood.spawn(3, 4, 'cut', 'moderate',"
                , "  {scale = 2, opacity = 1});"
                , "return string.format('%s,%s', tostring(a ~= nil),"
                , "  tostring(b ~= nil))"
                ]
            r `shouldBe` q "true,true"
            store ← readIORef (wsBloodStoreRef ws)
            -- The closed opacity interval, and the values arriving
            -- exactly as named: requirement 4's no-accepted-call-changes
            -- clause is what forbids clamping these instead.
            map (\d → (bdeScale d, bdeOpacity d))
                (allDecals (bstDecals store))
                `shouldBe` [(0.5, 0), (2, 1)]

        it "leaves an absent or unconvertible control on its documented \
           \default, exactly as before" $ \env → do
            ws ← resetScene env
            ls ← newBareLuaBackend env
            r ← runOk ls $ luaLines
                [ "local a = blood.spawn(1, 2, 'cut', 'minor',"
                , "  {scale = 'not a number', opacity = {}});"
                , "return tostring(a ~= nil)"
                ]
            r `shouldBe` q "true"
            store ← readIORef (wsBloodStoreRef ws)
            map (\d → (bdeScale d, bdeOpacity d))
                (allDecals (bstDecals store))
                `shouldBe` [(1, 1)]

      -- Requirement 3. Every example here stages through the real
      -- 'stageSession', which 'stagePoisoned' makes fail loudly on a
      -- 'StageError' — so each one is also the assertion that a
      -- poisoned save still LOADS.
      describe "a staged load" $ do

        -- The pure half first: 'stagePoisoned' below drives ONE
        -- poisoned entry through the real boundary, which cannot on its
        -- own distinguish a filter that checks both axes from one that
        -- checks only x.
        it "the filter rejects on EITHER axis, for NaN and both \
           \infinities, and keeps every finite position a Float holds" $
          \_ → do
            let at x y = GroundItem (mkItem "ration" 1 100) x y
                nonFinite = [0 / 0, 1 / 0, -1 / 0]
            map (\v → groundPositionIsFinite (at v 4)) nonFinite
                `shouldBe` replicate 3 False
            map (\v → groundPositionIsFinite (at 3 v)) nonFinite
                `shouldBe` replicate 3 False
            map (\(x, y) → groundPositionIsFinite (at x y))
                [(0, 0), (-1e38, 1e38), (3.5, -2.5)]
                `shouldBe` replicate 3 True

        it "sanitizeGroundItems drops exactly those entries, leaves \
           \every surviving id where it was, never rewinds the \
           \allocator, and reports in ascending id order" $ \_ → do
            let at x y = GroundItem (mkItem "ration" 1 100) x y
                gis = GroundItems
                    { gisNextId = 40
                    , gisItems  = HM.fromList
                        [ (5, at 1 2), (2, at (0 / 0) 2)
                        , (9, at 3 (1 / 0)), (7, at 4 5) ] }
                (kept, dropped) = sanitizeGroundItems gis
            sort (HM.keys (gisItems kept)) `shouldBe` [5, 7]
            gisNextId kept `shouldBe` 40
            map fst dropped `shouldBe` [2, 9]

        it "drops the entry stored at a non-finite position, keeps every \
           \other, preserves gisNextId and the global item cursor, and \
           \warns exactly once naming the page and the page-local id" $
          \env → do
            (staged, ws, entries) ← stagePoisoned env
            gis ← readIORef (wsGroundItemsRef ws)
            sort (HM.keys (gisItems gis)) `shouldBe` [goodGidA, goodGidB]
            sort (map (iiInstanceId ∘ giInst) (HM.elems (gisItems gis)))
                `shouldBe` [goodIidA, goodIidB]
            -- Dropping an entry retires an id; it never rewinds the
            -- allocator, and never touches the process-wide cursor.
            gisNextId gis `shouldBe` savedGroundNextId
            ssNextItemId staged `shouldBe` savedNextItemId

            let poisoned = HM.lookup poisonedGid (gisItems savedGroundItems)
            case (poisoned, warningsMentioning "ground item" entries) of
                (Just gi, [entry]) → do
                    leCategory entry `shouldBe` CatWorld
                    -- The facts the warning has to carry, spelled out
                    -- HERE rather than read back out of the renderer:
                    -- an expectation built only from
                    -- 'stagedGroundItemWarning' agrees with whatever
                    -- that function says, including a wrong id.
                    let msg = leMessage entry
                    msg `shouldSatisfy`
                        T.isInfixOf (unWorldPageId stagedPageId)
                    msg `shouldSatisfy`
                        T.isInfixOf ("ground item " <> tshow poisonedGid)
                    -- …and it names THAT entry, not a surviving sibling.
                    forM_ [goodGidA, goodGidB] $ \gid →
                        msg `shouldNotSatisfy`
                            T.isInfixOf ("ground item " <> tshow gid)
                    msg `shouldSatisfy` T.isInfixOf "worn_tool"
                    msg `shouldSatisfy` T.isInfixOf "NaN"
                    -- The whole line, once those facts are pinned, so
                    -- the rendering itself cannot drift silently.
                    msg `shouldBe`
                        stagedGroundItemWarning stagedPageId
                                                (poisonedGid, gi)
                (Nothing, _) → expectationFailure
                    "the fixture lost its poisoned entry"
                (_, other) → expectationFailure
                    ("expected exactly one sanitation warning, got "
                     ⧺ show (map leMessage other))

        it "resolves the durable-order diagnostic against the SANITIZED \
           \map — the dropped item's reference dangles, the surviving \
           \one does not — and keeps the order verbatim" $ \env → do
            (_, ws, entries) ← stagePoisoned env
            -- Retained, not pruned: a dangling order is tolerated
            -- gameplay, exactly as #1246 already has it.
            readIORef (wsTransferOrdersRef ws) `shouldReturn` savedOrders
            let diagnostics =
                    [ leMessage e
                    | e ← entries
                    , "transfer-order integrity diagnostic"
                          `T.isInfixOf` leMessage e ]
                mentions iid = any (T.isInfixOf (tshow iid)) diagnostics
            -- Had staging built its entity set from the DECODED map,
            -- the dropped item would still have resolved and nothing
            -- would be reported for it.
            mentions poisonedIid `shouldBe` True
            mentions goodIidA `shouldBe` False

        it "hands Lua a reconciliation context describing the RESTORED \
           \session, not the save as decoded: the dropped entry's \
           \ground id and item-instance id are both absent from it" $
          \env → do
            (staged, _, _) ← stagePoisoned env
            let rc = ssReconcile staged
            -- What @onSaveLoaded@ receives. A dropped id left in here
            -- tells @scripts/unit_ai_reconcile.lua@ that a repair job,
            -- pickup order or forage target still points at something.
            fmap sort (lookup (unWorldPageId stagedPageId)
                              (lrcGroundItemsByPage rc))
                `shouldBe` Just [goodGidA, goodGidB]
            sort (lrcItemInstances rc)
                `shouldBe` sort (map fromIntegral [goodIidA, goodIidB])

        it "reports a durable Lua ground_item or item_instance reference \
           \to the dropped entry as no longer resolving — tolerated and \
           \cleared, never a load failure — while a reference to a \
           \surviving entry still resolves" $ \_ → do
            -- The same 'KnownEntities' the reconciliation context is
            -- projected from, so this pins the shared derivation #1589
            -- exists to keep single rather than a second copy of it.
            let known = knownEntitiesFromSaveData savedWithOwner
                errors = luaReferenceErrors HM.empty known
                    [ refEdge "ground_item"   "dropped.ground"     poisonedGid
                    , refEdge "ground_item"   "surviving.ground"   goodGidA
                    , refEdge "item_instance" "dropped.instance"
                          (fromIntegral poisonedIid)
                    , refEdge "item_instance" "surviving.instance"
                          (fromIntegral goodIidA)
                    ]
            map iePath errors
                `shouldBe` ["dropped.ground", "dropped.instance"]
            forM_ errors $ \e →
                ieMessage e `shouldSatisfy`
                    T.isInfixOf "tolerated: cleared at reconcile time"

        it "leaves a significant obligation bound to the dropped item \
           \exactly as the save recorded it: dangling, never rebound, \
           \never latched taken" $ \env → do
            (_, ws, _) ← stagePoisoned env
            mParams ← readIORef (wsGenParamsRef ws)
            let entry = do
                    p    ← mParams
                    inst ← lookupLocationInstance significantIid
                               (wgpLocationInstances p)
                    find ((≡ significantSlot) ∘ lsiSlot) (liSignificant inst)
            (lsiInstanceId ⊚ entry) `shouldBe` Just (Just poisonedIid)
            (lsiTaken ⊚ entry) `shouldBe` Just False
            (lsiItemDefName ⊚ entry) `shouldBe` Just "worn_tool"

-- | A weapon so 'resolveStrike' has geometry to resolve; only the
--   instance's condition varies between the cases below.
probeWeapon ∷ ItemWeapon
probeWeapon = ItemWeapon
    { iwBladeLength = 20, iwBaseSharpness = 10
    , iwStabEff = 0.8, iwSlashEff = 0.5, iwBluntEff = 0.2
    , iwWeaponClass = "dagger", iwAttackCooldown = 1
    , iwLength = 30, iwCenterOfMass = 0.5
    }

-- | 'resolveStrike' for one instance condition.
strikeAt ∷ Float → ResolvedStrike
strikeAt cond =
    let inst = (mkItem "worn_tool" 1 cond) { iiWeight = 1 }
        idef = (testItems `defNamed` "worn_tool")
                 { idWeapon = Just probeWeapon }
    in resolveStrike emptySubstanceManager (Just (inst, idef, probeWeapon))
                     Nothing "stab" 70

-- | Look a fixture definition up, failing loudly rather than silently
--   testing a default.
defNamed ∷ ItemManager → Text → ItemDef
defNamed (ItemManager m) name =
    fromMaybe (error ("missing fixture def: " <> T.unpack name))
              (HM.lookup name m)
