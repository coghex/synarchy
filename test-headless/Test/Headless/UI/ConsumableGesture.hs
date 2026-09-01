-- | The player's Drink gesture (#1580): the only shipped path into
--   @scripts/consumable.lua@, reached through the selected unit's
--   inventory-row context menu.
--
--   Registered under a describe beginning "Player coffee drink gesture",
--   which is the gate the issue names.
--
--   What this fixture has to be, and why:
--
--   * It drives the REAL production modules —
--     'scripts.unit_info_v2_context_menu', 'scripts.consumable_gestures'
--     and 'scripts.consumable' — against REAL manager refs, and builds
--     its rows through the SAME 'scripts.ui.item_list' @groupItems@ path
--     the live panel uses. A stubbed consumable would prove only that a
--     callback ran; the whole claim under test is that a player click
--     reaches the effect arithmetic.
--
--   * Its two coffee pots are stack-equivalent in every field the
--     widget keys on — equal fill, equal quality, equal base weight,
--     nothing nested — and differ ONLY in instance id and tracked
--     temperature, which the key deliberately excludes (#1268). That is
--     what makes them ONE merged row holding two genuinely different
--     drinks, which is the situation the submenu exists for.
--
--   * @coffee_pot@ carries a real quality spec, because
--     'Engine.Scripting.Lua.API.Units.Inventory' pushes @quality@ only
--     when the def declares one and @scripts/consumable.lua@ then falls
--     back to @quality_mid@ (50), at which the mood delta is identically
--     zero. Hydration, caffeine and mood all start with headroom for the
--     same reason: a clamp must not be able to hide the movement.
--
--   * Every STALE case is produced by mutating live engine state between
--     capturing the menu and firing its callback — never by editing the
--     captured row, which would prove nothing about revalidation.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Player coffee drink gesture"'@.
module Test.Headless.UI.ConsumableGesture (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (newIORef, writeIORef, atomicModifyIORef')
import Engine.Asset.Handle (TextureHandle(..))
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Config (vcUIScale)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Equipment.Types
    (EquipmentClass(..), EquipmentClassManager(..), EquipmentSlot(..))
import Item.Types
    (ItemContainer(..), ItemDef(..), ItemInstance(..), ItemManager(..))
import Test.Headless.Harness (withHeadlessEngine)
import Test.Headless.Unit.TransferApi (minimalDef, mkItem, mkUnit)
import UI.Types (emptyUIPageManager)
import Unit.Faction (Faction(..))
import Unit.Types
    (UnitDef(..), UnitId(..), UnitInstance(..), UnitManager(..)
    , emptyUnitManager)
import World.Page.Types (WorldPageId(..))
import World.State.Types (WorldManager(..), emptyWorldState, emptyWorldManager)

-- * Fixture ids

-- | uid 1: the player's acolyte. Holds the merged hot/cold pair, a
--   separate EMPTY pot, an unregistered rope, and an EQUIPPED pot.
drinkerUid ∷ UnitId
drinkerUid = UnitId 1

-- | uid 2: a second player acolyte holding two pots whose temperatures
--   ROUND to the same degree — the ordinal-disambiguation case.
twinUid ∷ UnitId
twinUid = UnitId 2

-- | uid 3: wildlife. Selectable, never commandable — selection is not
--   authorization.
wolfUid ∷ UnitId
wolfUid = UnitId 3

-- | uid 4: a player acolyte that is WALKING, so @unit.drink@ would be
--   dropped by the engine's Idle gate.
walkerUid ∷ UnitId
walkerUid = UnitId 4

fixturePage ∷ WorldPageId
fixturePage = WorldPageId "coffee_gesture_page"

-- * Item fixtures

-- | Both pots are 1 L containers so the sip's clamp is the sip's own
--   0.25 L and the remaining fill is exact.
coffeeDef ∷ ItemDef
coffeeDef = ItemDef
    { idName = "coffee_pot", idDisplayName = "Coffee Pot"
    , idTexture = TextureHandle 0, idIconTexture = TextureHandle 0, idWeight = 0.4, idWeightSpec = Nothing
    , idBulk = 1.6, idStorage = Nothing, idKind = "misc"
    , idCategory = "Supplies", idMake = "", idMaterial = ""
    -- Present ONLY so `quality` reaches Lua at all — without a spec the
    -- row carries none, consumable.lua falls back to quality_mid, and
    -- the mood delta becomes structurally zero.
    , idQualitySpec = Just (30, 90), idQualityTiers = []
    , idContainer = Just (ItemContainer { icCapacity = 1.0
                                        , icHolds = "coffee"
                                        , icFillWeight = 1.0
                                        , icDefaultFill = 1.0 })
    , idDefaultContents = [], idFood = Nothing, idWeapon = Nothing
    , idArmor = Nothing, idUnequippable = False, idBuffs = []
    , idInsulation = 0, idSourcePath = "test-fixture"
    }

ropeDef ∷ ItemDef
ropeDef = coffeeDef
    { idName = "rope", idDisplayName = "Rope", idWeight = 2.0
    , idCategory = "Misc", idQualitySpec = Nothing, idContainer = Nothing }

fixtureItems ∷ ItemManager
fixtureItems = ItemManager $ HM.fromList
    [ ("coffee_pot", coffeeDef), ("rope", ropeDef) ]

-- | A pot at a given fill and tracked temperature. Quality is pinned
--   equal across every pot so it is never what separates two of them —
--   only the id and the temperature are.
pot ∷ Word64 → Float → Maybe Float → ItemInstance
pot iid fill temp = (mkItem "coffee_pot" iid 0.4)
    { iiCurrentFill = fill, iiQuality = 90, iiTemp = temp }

-- | The equipped pot: full and hot, so if the equipped ROW ever offered
--   a Drink it would be an executable one, and the omission under test
--   is a real refusal rather than an artefact of an empty container.
equippedPot ∷ ItemInstance
equippedPot = pot 320 1.0 (Just 80)

-- | 301 hot / 302 cold: stack-equivalent, so the widget merges them.
--   303 is empty, which DOES change the key, so it is its own row —
--   which is precisely how the empty exclusion presents on screen.
drinkerInventory ∷ [ItemInstance]
drinkerInventory =
    [ pot 301 1.0 (Just 80)
    , pot 302 1.0 (Just 26)
    , pot 303 0.0 (Just 80)
    , mkItem "rope" 310 2.0
    ]

-- | 42.4 °C and 41.6 °C both PRESENT as "42°C", so the two entries
--   collide on label and must still be individually selectable.
twinInventory ∷ [ItemInstance]
twinInventory = [ pot 601 1.0 (Just 42.4), pot 602 1.0 (Just 41.6) ]

-- * Unit fixtures

-- | Hydration well below its maximum, caffeine well below 1 and mood
--   mid-range: every one of the three has room to move in the direction
--   the effect pushes it.
withStats ∷ UnitInstance → UnitInstance
withStats u = u { uiStats = HM.union
    (HM.fromList [ ("hydration", 10), ("max_hydration", 100)
                 , ("caffeine", 0.1), ("mood", 0.5) ])
    (uiStats u) }

onPage ∷ UnitInstance → UnitInstance
onPage u = u { uiPage = fixturePage }

acolyteDef ∷ UnitDef
acolyteDef = (minimalDef "acolyte" "Acolyte")
    { udEquipmentClass = Just "humanoid" }

-- | One slot, accepting the pot's own kind, so the equipped pot is
--   emitted by the real inventory collector's class-ordered walk.
humanoidClass ∷ EquipmentClass
humanoidClass = EquipmentClass
    { ecName = "humanoid", ecSilhouetteTex = TextureHandle 0
    , ecSilhouetteW = 64, ecSilhouetteH = 64
    , ecSlots = [ EquipmentSlot { esId = "back", esName = "Back"
                                , esKind = "misc", esX = 0, esY = 0
                                , esW = 32, esH = 32 } ]
    }

resetWorld ∷ EngineEnv → IO ()
resetWorld env = do
    ws ← emptyWorldState
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(fixturePage, ws)], wmVisible = [fixturePage] }
    writeIORef (itemManagerRef env) fixtureItems
    writeIORef (equipmentClassManagerRef env)
        (EquipmentClassManager (HM.singleton "humanoid" humanoidClass))
    writeIORef (unitManagerRef env) emptyUnitManager
        { umDefs = HM.fromList
            [ ("acolyte", acolyteDef), ("wolf", minimalDef "wolf" "Wolf") ]
        , umInstances = HM.fromList
            [ (drinkerUid, withStats (onPage
                  (mkUnit "acolyte" FactionPlayer (10, 10) 100
                          drinkerInventory []))
                      { uiEquipment = HM.singleton "back" equippedPot })
            , (twinUid, withStats (onPage
                  (mkUnit "acolyte" FactionPlayer (12, 12) 100
                          twinInventory [])))
            , (wolfUid, withStats (onPage
                  (mkUnit "wolf" FactionWildlife (11, 11) 100
                          [pot 401 1.0 (Just 80)] [])))
            , (walkerUid, withStats (onPage
                  (mkUnit "acolyte" FactionPlayer (13, 13) 100
                          [pot 501 1.0 (Just 80)] []))
                      { uiActivity = "walking" })
            ]
        }

-- | Edit ONE live unit — the only way a stale case may be produced.
mutateUnit ∷ EngineEnv → UnitId → (UnitInstance → UnitInstance) → IO ()
mutateUnit env uid f = atomicModifyIORef' (unitManagerRef env) $ \um →
    (um { umInstances = HM.adjust f uid (umInstances um) }, ())

dropInstance ∷ Word64 → UnitInstance → UnitInstance
dropInstance iid u = u
    { uiInventory = filter ((≢ iid) . iiInstanceId) (uiInventory u) }

emptyInstance ∷ Word64 → UnitInstance → UnitInstance
emptyInstance iid u = u { uiInventory = map drain (uiInventory u) }
  where drain i | iiInstanceId i ≡ iid = i { iiCurrentFill = 0 }
                | otherwise            = i

setActivity ∷ Text → UnitInstance → UnitInstance
setActivity a u = u { uiActivity = a }

-- * Lua plumbing

withSharedFixture ∷ ((EngineEnv, LuaBackendState) → IO ()) → IO ()
withSharedFixture action = withHeadlessEngine $ \env → do
    ls ← newBareLuaBackend env
    action (env, ls)

resetFixture ∷ EngineEnv → LuaBackendState → IO ()
resetFixture env ls = do
    writeIORef (uiManagerRef env) emptyUIPageManager
    atomicModifyIORef' (videoConfigRef env) $ \c → (c { vcUIScale = 1.0 }, ())
    resetWorld env
    cleared ← evalOk ls
        "for k, _ in pairs(package.loaded) do package.loaded[k] = nil end; return true"
    cleared `shouldBe` "true"
    _ ← evalOk ls sceneLua
    pure ()

newBareLuaBackend ∷ EngineEnv → IO LuaBackendState
newBareLuaBackend env = do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                                (assetPoolRef env) (nextObjectIdRef env)
                                (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    pure ls

evalOk ∷ LuaBackendState → Text → IO Text
evalOk ls src = do
    r ← executeDebugLua (lbsLuaState ls) src
    r `shouldNotSatisfy` isLuaError
    pure r

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

-- | Preloads the unit-info singleton the context-menu module binds at
--   load time, captures whatever menu a right-click opens, and installs
--   the row-building and readback helpers every case drives.
--
--   @__menu@ groups through the widget's own @groupItems@, exactly as
--   the live panel does; @__equippedMenu@ goes through the REAL
--   inventory collector instead, because @unit.getInventory@ reports the
--   loose inventory alone and can never produce an equipped row.
sceneLua ∷ Text
sceneLua = luaLines
    -- Undo any drain stub a previous case installed (#1744). This runs
    -- on EVERY reset, so a case that fails part-way cannot leave the
    -- engine's real primitives replaced for the next one.
    [ "if _G.__origDrain then"
    , "  unit.modifyItemFillById = _G.__origDrain; _G.__origDrain = nil end;"
    , "if _G.__origDrink then"
    , "  unit.drink = _G.__origDrink; _G.__origDrink = nil end;"
    , "package.loaded['scripts.unit_info_v2'] ="
    , "  { activeUid = 1, equipSlots = {}, accessoryRows = {} };"
    , "package.loaded['scripts.unit_info_v2_inventory'] ="
    , "  { invalidate = function() end };"
    , "local cm = require('scripts.ui.context_menu');"
    , "_G.__captured = nil;"
    , "cm.show = function(items) _G.__captured = items end;"
    , "local function openOn(uid, rows, pick)"
    , "  local row; for _, r in ipairs(rows) do"
    , "    if pick(r) then row = r end end;"
    , "  if not row then return nil end;"
    , "  package.loaded['scripts.unit_info_v2'].activeUid = uid;"
    , "  _G.__captured = nil;"
    , "  require('scripts.unit_info_v2_context_menu');"
    , "  package.loaded['scripts.unit_info_v2']"
    , "    .handleInvItemRightClick(row);"
    , "  return _G.__captured end;"
    -- A LOOSE row, keyed by defName and fill so the merged pair and the
    -- separate empty pot are individually addressable.
    , "_G.__menu = function(uid, defName, fill)"
    , "  local il = require('scripts.ui.item_list');"
    , "  local rows = il.groupItems(unit.getInventory(uid) or {});"
    , "  return openOn(uid, rows, function(r)"
    , "    return r.defName == defName"
    , "       and math.abs((r.currentFill or 0) - fill) < 1e-6 end) end;"
    -- The EQUIPPED row, through the production collector plus the
    -- widget's own separateEquipped grouping (what the live panel asks
    -- for), so the row is tagged exactly as it is on screen.
    , "_G.__equippedMenu = function(uid, defName)"
    , "  local il = require('scripts.ui.item_list');"
    , "  local data = require('scripts.unit_info_v2_inventory_data');"
    , "  local raw = data.collectInventoryAndEquipment(uid);"
    , "  local rows = il.groupItems(raw, { separateEquipped = true });"
    , "  return openOn(uid, rows, function(r)"
    , "    return r.defName == defName and r.equipped == true end) end;"
    , "_G.__labels = function(items)"
    , "  local out = {}; for i, e in ipairs(items or {}) do out[i] = e.label end;"
    , "  return table.concat(out, '|') end;"
    , "_G.__sub = function(items, label)"
    , "  for _, e in ipairs(items or {}) do"
    , "    if e.label == label then return _G.__labels(e.submenu) end end;"
    , "  return '' end;"
    -- Fire ONE submenu entry by its exact player-visible label.
    , "_G.__fire = function(items, label, sublabel)"
    , "  for _, e in ipairs(items or {}) do"
    , "    if e.label == label then"
    , "      for _, s in ipairs(e.submenu or {}) do"
    , "        if s.label == sublabel and s.callback then"
    , "          s.callback(); return true end end end end;"
    , "  return false end;"
    -- Readback: the three stats, and one instance's fill (-1 = gone).
    , "_G.__stats = function(uid)"
    , "  return string.format('%.4f|%.4f|%.4f',"
    , "    unit.getStat(uid, 'hydration') or -1,"
    , "    unit.getStat(uid, 'caffeine')  or -1,"
    , "    unit.getStat(uid, 'mood')      or -1) end;"
    , "_G.__fill = function(uid, iid)"
    , "  for _, it in ipairs(unit.getInventory(uid) or {}) do"
    , "    if it.instanceId == iid then"
    , "      return string.format('%.4f', it.currentFill or 0) end end;"
    , "  return '-1' end;"
    -- #1744: answer the authoritative drain with `result` — nil (the
    -- engine found no such unit or no such loose instance) or a SIGNED
    -- applied delta, negative for a real drain. Records the arguments
    -- it was asked for, and counts unit.drink so "no animation queued"
    -- is asserted against a number. The originals are restored by the
    -- next resetFixture, above.
    , "_G.__stubDrain = function(result)"
    , "  _G.__origDrain = _G.__origDrain or unit.modifyItemFillById;"
    , "  _G.__origDrink = _G.__origDrink or unit.drink;"
    , "  _G.__drainArgs = 'none'; _G.__drains = 0; _G.__drinks = 0;"
    , "  unit.modifyItemFillById = function(uid, iid, delta)"
    , "    _G.__drains = _G.__drains + 1;"
    , "    _G.__drainArgs = string.format('%d|%d|%.4f', uid, iid, delta);"
    , "    return result end;"
    , "  unit.drink = function() _G.__drinks = _G.__drinks + 1 end end;"
    , "return true"
    ]

-- | The untouched starting readings, so "no mutation" is asserted
--   against a value rather than against an absence.
pristineStats ∷ Text
pristineStats = "\"10.0000|0.1000|0.5000\""

-- * Spec

spec ∷ Spec
spec = aroundAll withSharedFixture $
  describe "Player coffee drink gesture (#1580)" $ do

    describe "exact-instance selection on a merged row" $ do

        it "offers a Drink submenu naming each represented instance by \
           \its own temperature, with the row still merged \
           \(requirements 2 and 3)" $ \(env, ls) → do
            resetFixture env ls
            labels ← evalOk ls
                "return _G.__labels(_G.__menu(1, 'coffee_pot', 1.0))"
            labels `shouldSatisfy` T.isInfixOf "Drink"
            -- Two members, so the row really did merge, and each entry
            -- names its OWN effective temperature rather than the
            -- group's summary or the representative's value.
            sub ← evalOk ls
                "return _G.__sub(_G.__menu(1, 'coffee_pot', 1.0), 'Drink')"
            sub `shouldBe` "\"80°C|26°C\""

        it "invoking the NON-representative entry drinks that exact \
           \instance: only its fill moves, and the caffeine gain is the \
           \one its temperature implies (requirements 6 and 7)" $
           \(env, ls) → do
            resetFixture env ls
            fired ← evalOk ls
                "return _G.__fire(_G.__menu(1, 'coffee_pot', 1.0), \
                \'Drink', '26°C')"
            fired `shouldBe` "true"
            -- 302 is the SECOND member; the representative is 301.
            chosen ← evalOk ls "return _G.__fill(1, 302)"
            chosen `shouldBe` "\"0.7500\""
            untouched ← evalOk ls "return _G.__fill(1, 301)"
            untouched `shouldBe` "\"1.0000\""
            -- hydration 10 + 0.25*11*0.9         = 12.4750
            -- caffeine  0.1 + 0.25*1*0.9*warmth  = 0.1720 at 26°C, where
            --           warmth = 0.3 + 0.7*(26-25)/(60-25) = 0.32
            -- mood      0.5 + 0.25*0.3*(90-50)/50 = 0.5600
            stats ← evalOk ls "return _G.__stats(1)"
            stats `shouldBe` "\"12.4750|0.1720|0.5600\""

        it "the SAME gesture on the hot instance yields the larger \
           \caffeine gain, so the number above is temperature-derived \
           \and not a constant" $ \(env, ls) → do
            resetFixture env ls
            fired ← evalOk ls
                "return _G.__fire(_G.__menu(1, 'coffee_pot', 1.0), \
                \'Drink', '80°C')"
            fired `shouldBe` "true"
            -- 80°C is at/above hot_temp, so warmth is 1.0:
            --   0.1 + 0.25*1*0.9*1.0 = 0.3250
            stats ← evalOk ls "return _G.__stats(1)"
            stats `shouldBe` "\"12.4750|0.3250|0.5600\""
            hot ← evalOk ls "return _G.__fill(1, 301)"
            hot `shouldBe` "\"0.7500\""
            cold ← evalOk ls "return _G.__fill(1, 302)"
            cold `shouldBe` "\"1.0000\""

        it "entries that would read identically gain a stable ordinal, \
           \and the ordinal picks the instance it names (requirement 3)" $
           \(env, ls) → do
            resetFixture env ls
            -- 42.4 and 41.6 both present as 42°C.
            sub ← evalOk ls
                "return _G.__sub(_G.__menu(2, 'coffee_pot', 1.0), 'Drink')"
            sub `shouldBe` "\"42°C (1)|42°C (2)\""
            fired ← evalOk ls
                "return _G.__fire(_G.__menu(2, 'coffee_pot', 1.0), \
                \'Drink', '42°C (2)')"
            fired `shouldBe` "true"
            second ← evalOk ls "return _G.__fill(2, 602)"
            second `shouldBe` "\"0.7500\""
            first ← evalOk ls "return _G.__fill(2, 601)"
            first `shouldBe` "\"1.0000\""

    describe "exclusions — no executable drink exists (requirement 4)" $ do

        let noDrinkableEntry ∷ LuaBackendState → Text → Int → Word64 → IO ()
            noDrinkableEntry ls menuExpr uid iid = do
                labels ← evalOk ls ("return _G.__labels(" <> menuExpr <> ")")
                labels `shouldNotSatisfy` T.isInfixOf "Drink"
                sub ← evalOk ls
                    ("return _G.__sub(" <> menuExpr <> ", 'Drink')")
                sub `shouldBe` "\"\""
                -- Requirement 4 is met by an absent entry OR a disabled
                -- one, so what actually matters is that nothing here can
                -- mutate: no fill moved and no stat moved.
                fill ← evalOk ls
                    ("return _G.__fill(" <> tshow uid <> ", " <> tshow iid <> ")")
                fill `shouldBe` "\"1.0000\""
                stats ← evalOk ls ("return _G.__stats(" <> tshow uid <> ")")
                stats `shouldBe` pristineStats

        it "an EMPTY pot's own row offers no drink" $ \(env, ls) → do
            resetFixture env ls
            labels ← evalOk ls
                "return _G.__labels(_G.__menu(1, 'coffee_pot', 0.0))"
            labels `shouldNotSatisfy` T.isInfixOf "Drink"
            -- Empty means empty: it stays at zero, and nothing else was
            -- reached for in its place.
            drained ← evalOk ls "return _G.__fill(1, 303)"
            drained `shouldBe` "\"0.0000\""
            stats ← evalOk ls "return _G.__stats(1)"
            stats `shouldBe` pristineStats

        it "an EQUIPPED pot's row offers no drink — and it is its own \
           \single-instance row, never a member of the merged one" $
           \(env, ls) → do
            resetFixture env ls
            -- The equipped row exists at all (so the case is real).
            equipped ← evalOk ls
                "return _G.__labels(_G.__equippedMenu(1, 'coffee_pot'))"
            equipped `shouldNotSatisfy` T.isInfixOf "Drink"
            equipped `shouldSatisfy` T.isInfixOf "Unequip"
            stats ← evalOk ls "return _G.__stats(1)"
            stats `shouldBe` pristineStats

        it "an UNREGISTERED def offers no drink" $ \(env, ls) → do
            resetFixture env ls
            noDrinkableEntry ls "_G.__menu(1, 'rope', 0.0)" 1 301

        it "a NON-PLAYER-COMMANDABLE unit offers no drink, however it \
           \came to be the panel's active unit" $ \(env, ls) → do
            resetFixture env ls
            noDrinkableEntry ls "_G.__menu(3, 'coffee_pot', 1.0)" 3 401

        it "a NON-IDLE unit offers no drink, because the engine would \
           \drop the queued UnitDrink and the stats would move with no \
           \animation" $ \(env, ls) → do
            resetFixture env ls
            noDrinkableEntry ls "_G.__menu(4, 'coffee_pot', 1.0)" 4 501

    describe "stale-menu invocation revalidates live state \
             \(requirement 5)" $ do

        it "refuses when the chosen instance has DISAPPEARED, and never \
           \falls back to another pot of the same def" $ \(env, ls) → do
            resetFixture env ls
            held ← evalOk ls
                "_G.__held = _G.__menu(1, 'coffee_pot', 1.0); \
                \return _G.__sub(_G.__held, 'Drink')"
            held `shouldBe` "\"80°C|26°C\""
            mutateUnit env drinkerUid (dropInstance 302)
            fired ← evalOk ls "return _G.__fire(_G.__held, 'Drink', '26°C')"
            fired `shouldBe` "true"     -- the callback ran…
            gone ← evalOk ls "return _G.__fill(1, 302)"
            gone `shouldBe` "\"-1\""
            -- …and refused: the sibling pot is untouched and no stat moved.
            sibling ← evalOk ls "return _G.__fill(1, 301)"
            sibling `shouldBe` "\"1.0000\""
            stats ← evalOk ls "return _G.__stats(1)"
            stats `shouldBe` pristineStats

        it "refuses when the chosen instance has been EMPTIED since the \
           \menu opened" $ \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls "_G.__held = _G.__menu(1, 'coffee_pot', 1.0); return true"
            mutateUnit env drinkerUid (emptyInstance 302)
            fired ← evalOk ls "return _G.__fire(_G.__held, 'Drink', '26°C')"
            fired `shouldBe` "true"
            emptied ← evalOk ls "return _G.__fill(1, 302)"
            emptied `shouldBe` "\"0.0000\""
            sibling ← evalOk ls "return _G.__fill(1, 301)"
            sibling `shouldBe` "\"1.0000\""
            stats ← evalOk ls "return _G.__stats(1)"
            stats `shouldBe` pristineStats

        it "refuses when the unit became BUSY after the menu opened" $
           \(env, ls) → do
            resetFixture env ls
            _ ← evalOk ls "_G.__held = _G.__menu(1, 'coffee_pot', 1.0); return true"
            mutateUnit env drinkerUid (setActivity "walking")
            fired ← evalOk ls "return _G.__fire(_G.__held, 'Drink', '26°C')"
            fired `shouldBe` "true"
            chosen ← evalOk ls "return _G.__fill(1, 302)"
            chosen `shouldBe` "\"1.0000\""
            stats ← evalOk ls "return _G.__stats(1)"
            stats `shouldBe` pristineStats

    describe "the engine's own drain decides the sip (#1744)" $ do

        it "refuses when the authoritative drain answers nil, committing \
           \no stat and queueing no animation" $ \(env, ls) → do
            resetFixture env ls
            -- Every eligibility check PASSES here — 302 is loose, of a
            -- registered def, and full. Only the drain itself refuses,
            -- which is the unit-destroyed-between-snapshot-and-mutation
            -- window the ordering fix exists for.
            r ← evalOk ls
                "_G.__stubDrain(nil); \
                \local c = require('scripts.consumable'); \
                \local s, why = c.drinkInstance(1, 302); \
                \return tostring(s) .. '|' .. tostring(why) .. '|' \
                \    .. _G.__drinks .. '|' .. _G.__drainArgs"
            -- Failure return, no unit.drink, and the drain was asked for
            -- the FULL requested sip as a negative delta.
            r `shouldBe` "\"nil|drain failed|0|1|302|-0.2500\""
            stats ← evalOk ls "return _G.__stats(1)"
            stats `shouldBe` pristineStats

        it "the same refusal reaches the player through the shipped \
           \click path, so the gesture's no-fill-no-stat promise covers \
           \the drain and not only the eligibility check" $
           \(env, ls) → do
            resetFixture env ls
            fired ← evalOk ls
                "_G.__stubDrain(nil); \
                \return tostring(_G.__fire(_G.__menu(1, 'coffee_pot', 1.0), \
                \  'Drink', '26°C')) .. '|' .. _G.__drains \
                \  .. '|' .. _G.__drinks"
            -- The callback ran and did reach the drain — it just got
            -- nothing back, so nothing was credited.
            fired `shouldBe` "\"true|1|0\""
            stats ← evalOk ls "return _G.__stats(1)"
            stats `shouldBe` pristineStats

        it "a SHORT drain re-derives every effect from the magnitude the \
           \engine returned, never from the snapshot's requested sip" $
           \(env, ls) → do
            resetFixture env ls
            -- -0.10 against a requested -0.25: a clamped partial drain,
            -- signed the way adjustFillById reports one. Every stat has
            -- headroom, so a clamp cannot hide which amount was used.
            r ← evalOk ls
                "_G.__stubDrain(-0.10); \
                \local c = require('scripts.consumable'); \
                \local s = c.drinkInstance(1, 302); \
                \return string.format('%.4f|%.4f|%.4f|%.4f|%d|%s', \
                \  s.sip, s.hydration, s.caffeine, s.mood, \
                \  _G.__drinks, _G.__drainArgs)"
            -- sip       = 0.10 (the returned magnitude, not 0.25)
            -- hydration = 0.10*11*0.9         = 0.9900
            -- caffeine  = 0.10*1*0.9*0.32     = 0.0288 at 26°C
            -- mood      = 0.10*0.3*(90-50)/50 = 0.0240
            r `shouldBe`
                "\"0.1000|0.9900|0.0288|0.0240|1|1|302|-0.2500\""
            -- And the committed stats moved by exactly those amounts:
            -- 10 + 0.99, 0.1 + 0.0288, 0.5 + 0.024. Had the stale 0.25
            -- survived anywhere these would read 12.4750|0.1720|0.5600.
            stats ← evalOk ls "return _G.__stats(1)"
            stats `shouldBe` "\"10.9900|0.1288|0.5240\""

        it "an unexpected fill INCREASE is not consumption: a positive \
           \applied delta refuses rather than crediting its magnitude" $
           \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls
                "_G.__stubDrain(0.10); \
                \local c = require('scripts.consumable'); \
                \local s, why = c.drinkInstance(1, 302); \
                \return tostring(s) .. '|' .. tostring(why) \
                \    .. '|' .. _G.__drinks"
            r `shouldBe` "\"nil|nothing drained|0\""
            stats ← evalOk ls "return _G.__stats(1)"
            stats `shouldBe` pristineStats

        it "the legacy defName entry point gets the same behaviour from \
           \the one shared body (requirement 5)" $ \(env, ls) → do
            resetFixture env ls
            refused ← evalOk ls
                "_G.__stubDrain(nil); \
                \local c = require('scripts.consumable'); \
                \local s, why = c.drink(1, 'coffee_pot'); \
                \return tostring(s) .. '|' .. tostring(why) .. '|' \
                \    .. _G.__drinks .. '|' .. _G.__drainArgs"
            -- 301 is the first non-empty pot, so the legacy selection
            -- policy is unchanged — only the ordering it feeds is.
            refused `shouldBe` "\"nil|drain failed|0|1|301|-0.2500\""
            stats ← evalOk ls "return _G.__stats(1)"
            stats `shouldBe` pristineStats
            short ← evalOk ls
                "_G.__stubDrain(-0.10); \
                \local c = require('scripts.consumable'); \
                \local s = c.drink(1, 'coffee_pot'); \
                \return string.format('%.4f|%.4f', s.sip, s.caffeine)"
            -- 301 is at 80°C, so warmth is 1.0: 0.10*1*0.9*1.0.
            short `shouldBe` "\"0.1000|0.0900\""

    describe "the legacy mechanism call is untouched (requirement 8)" $

        it "consumable.drink(uid, defName) still selects the FIRST \
           \non-empty instance by defName, which is the probe's contract" $
           \(env, ls) → do
            resetFixture env ls
            r ← evalOk ls
                "local c = require('scripts.consumable'); \
                \local s = c.drink(1, 'coffee_pot'); \
                \return s and string.format('%.4f', s.caffeine) or 'nil'"
            -- The hot pot (301) is first, so the legacy path warms to 1.0.
            r `shouldBe` "\"0.2250\""
            first ← evalOk ls "return _G.__fill(1, 301)"
            first `shouldBe` "\"0.7500\""
            rest ← evalOk ls "return _G.__fill(1, 302)"
            rest `shouldBe` "\"1.0000\""
