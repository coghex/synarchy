{-# LANGUAGE OverloadedStrings #-}
-- | The ground↔inventory verbs resolve their page from the UNIT, not
--   from the active world (#1208).
--
--   Ground items are page-local and every unit records its own page, so
--   @item.pickupGround@, @unit.dropEquipmentToGround@,
--   @unit.dropItemToGround@ and @unit.dropItemById@ used to teleport an
--   exact 'ItemInstance' between worlds: they removed from / spawned
--   into whichever page happened to be ACTIVE while looking the unit up
--   globally. Ground-item ids are allocated per page ('gisNextId' lives
--   inside each 'GroundItems'), so a same-numbered gid on two pages is
--   the natural collision this whole spec is built around.
--
--   Driven through the REAL registered Lua API against REAL manager
--   refs — same bare-Lua-backend technique as
--   'Test.Headless.Unit.TransferApi', and for the same reason: the
--   defect lives in the verbs' page resolution, which a pure test
--   structurally cannot see. Pages are in-memory 'emptyWorldState's, so
--   two live worlds cost no worldgen.
--
--   The one exception is the concurrent-disappearance rollback, which
--   needs the unit to be gone at insert time. That is reached by
--   calling 'pickupGroundOnPage' — the exact core the Lua verb runs —
--   with a uid that does not exist, rather than by racing a thread.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "ground item page ownership"'@.
module Test.Headless.Item.GroundPageOwnership (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.List (sortOn)
import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.API.Items.Ground (pickupGroundOnPage)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Ground
    (GroundItem(..), GroundItems(..), spawnGroundItem)
import Item.Types (ItemInstance(..), emptyItemManager)
import Unit.Direction (Direction(..))
import Unit.Faction (Faction(..))
import Unit.Types
    ( BodyPart(..), UnitDef(..), UnitId(..), UnitInstance(..)
    , UnitManager(..), defaultNaturalResistance, emptyUnitManager )
import World.Cursor.Types (CursorState(..))
import World.Page.Types (WorldPageId(..))
import World.State.Types
    ( WorldManager(..), WorldState(..), emptyWorldState, emptyWorldManager )

-- * Fixture identities

-- | The ACTIVE page. Everything it holds is the bystander: nothing an
--   off-page unit does may touch it.
pageActive ∷ WorldPageId
pageActive = WorldPageId "ownership_active"

-- | A live but NOT active page — where the unit under test stands.
pageOwned ∷ WorldPageId
pageOwned = WorldPageId "ownership_owned"

-- | A page id with no entry in @wmWorlds@ at all.
pageGhost ∷ WorldPageId
pageGhost = WorldPageId "ownership_ghost"

-- | The unit under test, on 'pageOwned'.
travellerUid ∷ UnitId
travellerUid = UnitId 1

-- | A bystander on the ACTIVE page: its inventory must never move.
bystanderUid ∷ UnitId
bystanderUid = UnitId 2

-- | A unit whose page has no live world.
strandedUid ∷ UnitId
strandedUid = UnitId 3

-- | A uid that is never in @umInstances@ — the vanished unit the
--   rollback path exists for.
vanishedUid ∷ UnitId
vanishedUid = UnitId 999

travellerAt ∷ (Float, Float)
travellerAt = (7, 9)

-- * Fixtures

mkItem ∷ Text → Word64 → ItemInstance
mkItem name iid = ItemInstance
    { iiDefName     = name
    , iiCurrentFill = 0
    , iiQuality     = 100
    , iiCondition   = 100
    , iiWeight      = 2.5
    , iiSharpness   = 100
    , iiContents    = []
    , iiInstanceId  = iid
    , iiTemp        = Nothing
    , iiBulk        = Just 2.5
    , iiStorage     = Nothing
    }

-- | Mirrors 'Test.Headless.Unit.TransferApi.minimalDef': none of these
--   verbs read a def, so only enough to make a live unit.
minimalDef ∷ Text → UnitDef
minimalDef name = UnitDef
    { udName = name, udNamePool = Nothing, udDisplayName = Just name
    , udTexture = TextureHandle 0, udPortrait = Nothing
    , udDirSprites = Map.empty
    , udBaseWidth = 0, udMaxSpeed = 1.0, udRunThreshold = 0.6
    , udAnimations = HM.empty, udStateAnims = HM.empty, udEagerStats = False
    , udStatTemplates = HM.empty, udBodyTemplates = HM.empty
    , udSkillTemplates = HM.empty, udKnowledgeTemplates = HM.empty
    , udStartingInventory = []
    , udEquipmentClass = Nothing, udStartingEquipment = HM.empty
    , udStartingAccessories = []
    , udBodyParts =
        [ BodyPart
            { bpId = "torso", bpName = "torso", bpParent = Nothing
            , bpVital = False, bpAreaWeight = 1.0, bpTacticalValue = 0.5
            , bpBleedFactor = 1.0, bpHeightLow = 0, bpHeightHigh = 1
            , bpLayers = [], bpTargetable = True, bpDepth = 0.0
            , bpAffectsLocomotion = False, bpAffectsBalance = False } ]
    , udNaturalResistance = defaultNaturalResistance
    , udNaturalWeapon = Nothing, udModifiers = [] }

mkUnit ∷ WorldPageId → (Float, Float) → [ItemInstance]
       → HM.HashMap Text ItemInstance → UnitInstance
mkUnit page (gx, gy) inv equip = UnitInstance
    { uiDefName = "acolyte", uiName = "", uiPage = page
    , uiTexture = TextureHandle 0, uiDirSprites = Map.empty
    , uiBaseWidth = 0, uiGridX = gx, uiGridY = gy, uiGridZ = 0
    , uiRealZ = 0, uiFacing = DirS
    , uiCurrentAnim = "", uiAnimStart = 0, uiAnimReverse = False
    , uiActivity = "idle", uiPose = "standing", uiAnimStride = 1
    , uiStats = HM.singleton "carrying_capacity" 100
    , uiModifiers = HM.empty, uiSkills = HM.empty
    , uiKnowledge = HM.empty, uiInventory = inv, uiEquipment = equip
    , uiAccessories = [], uiFactionId = FactionPlayer, uiWounds = []
    , uiScars = [], uiImmuneResponse = 0, uiImmunities = HM.empty
    , uiBlood = 5.0, uiLastAttackerUid = Nothing, uiLastAttackerAt = 0
    , uiAnimOverride = "", uiFrozen = False, uiForceLoop = False
    , uiClimbDest = Nothing, uiTrailState = Nothing
    }

-- | The two live pages, active first, plus the three units.
--
--   @activeGround@ / @ownedGround@ are spawned in list order into
--   in-memory pages, so each page's own allocator hands out ids from 0
--   and a same-numbered gid on both pages is the DEFAULT, not something
--   the fixture has to contrive.
data Scene = Scene
    { scActive ∷ WorldState
    , scOwned  ∷ WorldState
    }

resetScene ∷ EngineEnv → [ItemInstance] → [ItemInstance]
           → [ItemInstance] → HM.HashMap Text ItemInstance
           → IO Scene
resetScene env activeGround ownedGround travellerInv travellerEquip = do
    wsA ← emptyWorldState
    wsO ← emptyWorldState
    forM_ activeGround $ \it →
        atomicModifyIORef' (wsGroundItemsRef wsA) $ spawnGroundItem it 1 1
    forM_ ownedGround $ \it →
        atomicModifyIORef' (wsGroundItemsRef wsO) $
            spawnGroundItem it (fst travellerAt) (snd travellerAt)
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds  = [(pageActive, wsA), (pageOwned, wsO)]
        , wmVisible = [pageActive] }
    writeIORef (itemManagerRef env) emptyItemManager
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.singleton "acolyte" (minimalDef "acolyte")
        , umInstances = HM.fromList
            [ (travellerUid, mkUnit pageOwned travellerAt
                                 travellerInv travellerEquip)
            , (bystanderUid, mkUnit pageActive (1, 1)
                                 [mkItem "bystander_pack" 900] HM.empty)
            -- Deliberately holding the SAME items the drop cases name,
            -- so the only thing that can make a drop fail for it is
            -- the missing page — not an empty slot or a missing def.
            , (strandedUid, mkUnit pageGhost (3, 3)
                                 [mkItem "steel_axe" 300]
                                 (HM.singleton "hand_right"
                                      (mkItem "steel_axe" 301))) ]
        }
    pure (Scene wsA wsO)

-- * Live-state readers

groundOf ∷ WorldState → IO GroundItems
groundOf ws = readIORef (wsGroundItemsRef ws)

-- | Every ground instance on a page, keyed by gid, with the position it
--   sits at — the shape assertions compare so a failure names the item
--   and the page rather than a diff of the whole record.
groundRows ∷ WorldState → IO [(Int, Text, Word64, (Float, Float))]
groundRows ws = do
    gis ← groundOf ws
    pure $ sortOn (\(g, _, _, _) → g)
        [ (gid, iiDefName (giInst gi), iiInstanceId (giInst gi)
          , (giX gi, giY gi))
        | (gid, gi) ← HM.toList (gisItems gis) ]

unitOf ∷ EngineEnv → UnitId → IO (Maybe UnitInstance)
unitOf env uid = do
    um ← readIORef (unitManagerRef env)
    pure $ HM.lookup uid (umInstances um)

invOf ∷ EngineEnv → UnitId → IO [(Word64, Text)]
invOf env uid = do
    mu ← unitOf env uid
    pure $ case mu of
        Nothing → []
        Just u  → [(iiInstanceId i, iiDefName i) | i ← uiInventory u]

equipOf ∷ EngineEnv → UnitId → IO [(Text, Word64)]
equipOf env uid = do
    mu ← unitOf env uid
    pure $ case mu of
        Nothing → []
        Just u  → sortOn fst [ (slot, iiInstanceId i)
                             | (slot, i) ← HM.toList (uiEquipment u) ]

cursorSelection ∷ WorldState → IO (Maybe Int)
cursorSelection ws = selectedGroundItem <$> readIORef (wsCursorRef ws)

selectGround ∷ WorldState → Maybe Int → IO ()
selectGround ws sel =
    atomicModifyIORef' (wsCursorRef ws) $ \cs →
        (cs { selectedGroundItem = sel }, ())

-- * Lua plumbing

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalDebug ∷ LuaBackendState → Text → IO Text
evalDebug ls = executeDebugLua (lbsLuaState ls)

-- * The spec

spec ∷ SpecWith EngineEnv
spec = describe "ground item page ownership" $ do

    describe "item.pickupGround" $ do

        it "picks from the unit's OWN page when both pages share the gid" $
            \env → do
                ls ← newBareLuaBackend env
                scene ← resetScene env [mkItem "steel_bar" 100]
                                      [mkItem "copper_bar" 200] [] HM.empty
                beforeActive ← groundRows (scActive scene)
                beforeActive `shouldBe` [(0, "steel_bar", 100, (1, 1))]
                r ← evalDebug ls "return item.pickupGround(1, 0)"
                r `shouldBe` "true"
                -- The traveller got its OWN page's instance …
                invOf env travellerUid
                    `shouldReturn` [(200, "copper_bar")]
                -- … its page's ground is now empty …
                groundRows (scOwned scene) `shouldReturn` []
                -- … and the active page's same-numbered item is exactly
                -- as it was.
                groundRows (scActive scene) `shouldReturn` beforeActive
                groundOf (scActive scene) `shouldReturn` GroundItems
                    { gisNextId = 1
                    , gisItems = HM.singleton 0
                        (GroundItem (mkItem "steel_bar" 100) 1 1) }

        it "fails with no mutation when only the ACTIVE page has the gid" $
            \env → do
                ls ← newBareLuaBackend env
                scene ← resetScene env [mkItem "steel_bar" 100] [] []
                                      HM.empty
                before ← groundOf (scActive scene)
                r ← evalDebug ls "return item.pickupGround(1, 0)"
                r `shouldBe` "false"
                groundOf (scActive scene) `shouldReturn` before
                groundRows (scOwned scene) `shouldReturn` []
                invOf env travellerUid `shouldReturn` []

        it "fails with no mutation when the unit's page has no live world" $
            \env → do
                ls ← newBareLuaBackend env
                scene ← resetScene env [mkItem "steel_bar" 100]
                                      [mkItem "copper_bar" 200] [] HM.empty
                beforeA ← groundOf (scActive scene)
                beforeO ← groundOf (scOwned scene)
                -- Both live pages hold a selection of the gid being
                -- asked for, so a stray deselect anywhere shows up.
                selectGround (scActive scene) (Just 0)
                selectGround (scOwned scene) (Just 0)
                r ← evalDebug ls "return item.pickupGround(3, 0)"
                r `shouldBe` "false"
                groundOf (scActive scene) `shouldReturn` beforeA
                groundOf (scOwned scene) `shouldReturn` beforeO
                cursorSelection (scActive scene) `shouldReturn` Just 0
                cursorSelection (scOwned scene) `shouldReturn` Just 0
                invOf env strandedUid `shouldReturn` [(300, "steel_axe")]
                equipOf env strandedUid
                    `shouldReturn` [("hand_right", 301)]

        it "fails with no mutation when the unit does not exist" $
            \env → do
                ls ← newBareLuaBackend env
                scene ← resetScene env [mkItem "steel_bar" 100]
                                      [mkItem "copper_bar" 200] [] HM.empty
                beforeA ← groundOf (scActive scene)
                beforeO ← groundOf (scOwned scene)
                r ← evalDebug ls "return item.pickupGround(999, 0)"
                r `shouldBe` "false"
                groundOf (scActive scene) `shouldReturn` beforeA
                groundOf (scOwned scene) `shouldReturn` beforeO

        it "deselects in the OWNING page's cursor only" $ \env → do
            ls ← newBareLuaBackend env
            scene ← resetScene env [mkItem "steel_bar" 100]
                                  [mkItem "copper_bar" 200] [] HM.empty
            selectGround (scActive scene) (Just 0)
            selectGround (scOwned scene) (Just 0)
            r ← evalDebug ls "return item.pickupGround(1, 0)"
            r `shouldBe` "true"
            cursorSelection (scOwned scene) `shouldReturn` Nothing
            cursorSelection (scActive scene) `shouldReturn` Just 0

        it "leaves the cursor untouched when nothing was removed" $
            \env → do
                ls ← newBareLuaBackend env
                scene ← resetScene env [mkItem "steel_bar" 100]
                                      [mkItem "copper_bar" 200] [] HM.empty
                selectGround (scOwned scene) (Just 0)
                r ← evalDebug ls "return item.pickupGround(1, 7)"
                r `shouldBe` "false"
                cursorSelection (scOwned scene) `shouldReturn` Just 0

        it "restores a vanished unit's item to the page it came from" $
            \env → do
                _ ← newBareLuaBackend env
                scene ← resetScene env [mkItem "steel_bar" 100]
                                      [mkItem "copper_bar" 200] [] HM.empty
                beforeActive ← groundOf (scActive scene)
                selectGround (scOwned scene) (Just 0)
                -- The exact core item.pickupGround runs, with a uid the
                -- manager does not hold: remove succeeds, insert fails.
                ok ← pickupGroundOnPage env (scOwned scene) vanishedUid 0
                ok `shouldBe` False
                -- Restored to the OWNING page, at its old position,
                -- under a fresh id — the same instance, not a copy.
                groundRows (scOwned scene) `shouldReturn`
                    [(1, "copper_bar", 200, travellerAt)]
                -- The active page never saw any of it.
                groundOf (scActive scene) `shouldReturn` beforeActive
                -- The old gid is stale after a rollback, so it goes.
                cursorSelection (scOwned scene) `shouldReturn` Nothing

    describe "the three drop verbs" $ do

        -- Each verb's call is parameterised by uid so the live-page and
        -- dead-page scenarios below run the SAME call against the same
        -- holdings, differing only in which unit — and so which page —
        -- it names.
        -- Each row also carries what the SOURCE unit must be left
        -- holding, so both sides of the move are pinned: the verb has
        -- to take the instance out of exactly one place and put it in
        -- exactly one other. The traveller starts with steel_axe 300
        -- loose and steel_axe 301 in hand_right — same def in both, so
        -- a verb that reached for the wrong one would look right on
        -- the ground and wrong here.
        let dropCases ∷ [( Text, Text → Text, Word64
                         , [(Word64, Text)], [(Text, Word64)] )]
            dropCases =
                [ ( "unit.dropEquipmentToGround"
                  , \u → "return unit.dropEquipmentToGround("
                             <> u <> ", 'hand_right')"
                  , 301, [(300, "steel_axe")], [] )
                , ( "unit.dropItemToGround"
                  , \u → "return unit.dropItemToGround(" <> u
                             <> ", 'steel_axe')"
                  , 300, [], [("hand_right", 301)] )
                , ( "unit.dropItemById"
                  , \u → "return unit.dropItemById(" <> u <> ", 300)"
                  , 300, [], [("hand_right", 301)] )
                ]

        forM_ dropCases $ \(label, call, droppedIid, restInv, restEquip) →
            it (T.unpack label <> " lands on the unit's own page only") $
                \env → do
                    ls ← newBareLuaBackend env
                    scene ← resetScene env [mkItem "steel_bar" 100] []
                                [mkItem "steel_axe" 300]
                                (HM.singleton "hand_right"
                                     (mkItem "steel_axe" 301))
                    beforeActive ← groundOf (scActive scene)
                    r ← evalDebug ls (call "1")
                    r `shouldBe` "true"
                    -- The exact instance landed on the OWNING page, at
                    -- the coordinates the unit's own frame gave it.
                    groundRows (scOwned scene) `shouldReturn`
                        [(0, "steel_axe", droppedIid, travellerAt)]
                    -- …and left the unit: the source side of the move,
                    -- with the unit's other same-def instance still in
                    -- place.
                    invOf env travellerUid `shouldReturn` restInv
                    equipOf env travellerUid `shouldReturn` restEquip
                    -- The active page is untouched, id allocator and all.
                    groundOf (scActive scene) `shouldReturn` beforeActive
                    -- So is the bystander standing on it.
                    invOf env bystanderUid
                        `shouldReturn` [(900, "bystander_pack")]

        forM_ dropCases $ \(label, call, _, _, _) →
            it (T.unpack label
                    <> " fails with no mutation off a live page") $
                \env → do
                    ls ← newBareLuaBackend env
                    scene ← resetScene env [mkItem "steel_bar" 100] [] []
                                HM.empty
                    beforeA ← groundOf (scActive scene)
                    beforeO ← groundOf (scOwned scene)
                    selectGround (scActive scene) (Just 0)
                    selectGround (scOwned scene) (Just 0)
                    -- Same call, same holdings, aimed at the unit whose
                    -- page has no live world.
                    r ← evalDebug ls (call "3")
                    r `shouldBe` "false"
                    groundOf (scActive scene) `shouldReturn` beforeA
                    groundOf (scOwned scene) `shouldReturn` beforeO
                    cursorSelection (scActive scene) `shouldReturn` Just 0
                    cursorSelection (scOwned scene) `shouldReturn` Just 0
                    invOf env strandedUid
                        `shouldReturn` [(300, "steel_axe")]
                    equipOf env strandedUid
                        `shouldReturn` [("hand_right", 301)]

    describe "same-page behavior is unchanged" $ do

        it "round-trips the exact instance on a non-active page" $
            \env → do
                ls ← newBareLuaBackend env
                scene ← resetScene env [] [mkItem "quartz_lens" 400] []
                                      HM.empty
                picked ← evalDebug ls "return item.pickupGround(1, 0)"
                picked `shouldBe` "true"
                invOf env travellerUid `shouldReturn` [(400, "quartz_lens")]
                dropped ← evalDebug ls "return unit.dropItemById(1, 400)"
                dropped `shouldBe` "true"
                invOf env travellerUid `shouldReturn` []
                gis ← groundOf (scOwned scene)
                map giInst (HM.elems (gisItems gis))
                    `shouldBe` [mkItem "quartz_lens" 400]

        it "round-trips on the ACTIVE page for a unit standing on it" $
            \env → do
                ls ← newBareLuaBackend env
                _ ← resetScene env [mkItem "quartz_lens" 401] [] [] HM.empty
                -- The bystander lives on the active page: this is the
                -- entire shipped AI/UI surface, and it must not shift.
                picked ← evalDebug ls "return item.pickupGround(2, 0)"
                picked `shouldBe` "true"
                invOf env bystanderUid `shouldReturn`
                    [(900, "bystander_pack"), (401, "quartz_lens")]
                dropped ← evalDebug ls "return unit.dropItemById(2, 401)"
                dropped `shouldBe` "true"
                invOf env bystanderUid `shouldReturn`
                    [(900, "bystander_pack")]

        it "still refuses a gid that exists on no live page" $ \env → do
            ls ← newBareLuaBackend env
            _ ← resetScene env [] [] [] HM.empty
            r ← evalDebug ls "return item.pickupGround(1, 0)"
            r `shouldBe` "false"
