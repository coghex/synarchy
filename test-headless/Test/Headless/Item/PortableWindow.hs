-- | The @portableItem@ container-window level (#2527, epic #1231
--   PLC-17): a PORTABLE container's own window, addressed by the
--   crate's item-instance id and drawn entirely from PLC-7's remembered
--   knowledge.
--
--   Driven end to end against a LIVE headless engine with the real
--   @scripts/hud.lua@ booted: real ground items, a real item registry,
--   a real portable-knowledge map written through the registered
--   observe verbs and drained by a real world tick, the real
--   ground-item context menu, the real window manager and the real
--   shared item-list widget. Nothing about the knowledge layer or the
--   window is stubbed — the only fixture seam is @item.hitTestAt@,
--   which answers which ground id the player right-clicked, because a
--   pixel hit test has no meaning without a rendered frame.
--
--   Filed under the @Nested item contents@ describe so #1238's own gate
--   covers it:
--   @cabal test synarchy-test-headless
--   --test-options='--match "Nested item contents"'@.
module Test.Headless.Item.PortableWindow (spec) where

import UPrelude
import Test.Hspec
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.IORef (newIORef, readIORef, writeIORef)
import Engine.Core.Init (EngineInitResult(..))
import Engine.Core.State
import Engine.Core.Thread (ThreadControl(..))
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Item.Knowledge
import Item.Ground (GroundItem(..), GroundItems(..))
import Item.Types (ItemInstance(..))
import Test.Headless.Harness.Isolation (withIsolatedResourceRoot)
import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)
import Test.Headless.Item.PortableKnowledge.Fixture
import UI.Types (emptyUIPageManager)
import Unit.Faction (Faction(..))
import Unit.Types (UnitId(..), UnitManager(..), emptyUnitManager)
import World.Page.Types (WorldPageId(..))
import World.State.Types
import World.Thread (worldTickWith)

-- * The scene

-- | @scripts/hud.lua@ resolves its world page by this name, and
--   @item.listGround@ answers about the ACTIVE page, so the crate lives
--   on exactly one page that is both.
hudPage ∷ WorldPageId
hudPage = WorldPageId "main_world"

-- | The ids this module adds to the shared fixture's. @kitBId@ is a
--   SECOND @first_aid_kit@ inside the same crate, so "two same-def
--   nested containers answer with their own contents" is a claim with
--   something to be wrong about.
kitBId, bandageBId, plainCrateId ∷ Word64
kitBId       = 61
bandageBId   = 62
plainCrateId = 63   -- ^ a crate whose record is never observed

-- | Ground ids. Deliberately not equal to the instance ids: a level
--   opened with a ground id instead of the instance id would resolve a
--   different crate, and these being distinct is what makes that
--   visible.
crateGid, plainGid, barGid ∷ Int
crateGid = 0
plainGid = 1
barGid   = 2

-- | The observed crate: two same-def kits, each holding its own
--   bandage.
stockedCrate ∷ ItemInstance
stockedCrate = crate { iiContents = [kit, kitB] }

kitB ∷ ItemInstance
kitB = kit { iiInstanceId = kitBId
           , iiContents   = [bandage { iiInstanceId = bandageBId }] }

-- | A second crate, identical in every way but identity, so a level can
--   be opened on one while the other is what changes.
plainCrate ∷ ItemInstance
plainCrate = crate { iiInstanceId = plainCrateId, iiContents = [] }

observeTime, laterTime ∷ Double
observeTime = 500
laterTime   = 1000

data Bindings = Bindings
    { bnEnv ∷ EngineEnv
    , bnLua ∷ LuaBackendState
    }

withBindings ∷ (Bindings → IO α) → IO α
withBindings act = withIsolatedResourceRoot $ do
    EngineInitResult env ← initializeEngineHeadlessQuiet
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    act (Bindings env ls)

-- | Reinstall the whole scene and a fresh Lua module graph, so no
--   example can inherit another's open window, knowledge record or
--   monkey-patched verb.
--
--   @ground@ is what the page holds; @holder@ is the unit (if any) that
--   carries the crate instead.
resetScene ∷ Bindings → [(Int, ItemInstance)] → Maybe (Faction, ItemInstance)
           → IO ()
resetScene b ground holder = do
    let env = bnEnv b
    writeIORef (uiManagerRef env) emptyUIPageManager
    writeIORef (itemManagerRef env) testItems
    writeIORef (gameTimeRef env) observeTime
    ws ← emptyWorldState
    writeIORef (wsGroundItemsRef ws) (groundAt ground)
    writeIORef (worldManagerRef env) emptyWorldManager
        { wmWorlds = [(hudPage, ws)], wmVisible = [hudPage] }
    writeIORef (unitManagerRef env) emptyUnitManager
        { umInstances = case holder of
            Nothing → HM.empty
            Just (f, held) → HM.singleton (UnitId 1)
                (mkUnitOfFaction f hudPage [held] HM.empty [])
        , umNextId = 2 }
    _ ← luaOk b "for k, _ in pairs(package.loaded) do \
                 \package.loaded[k] = nil end; return true"
    _ ← luaOk b sceneLua
    pure ()

-- | Ground items at explicitly chosen ids (the fixture's own helper
--   numbers them positionally).
groundAt ∷ [(Int, ItemInstance)] → GroundItems
groundAt entries = GroundItems
    { gisNextId = 1 + maximum (0 : map fst entries)
    , gisItems  = HM.fromList [ (gid, GroundItem inst 1 1)
                              | (gid, inst) ← entries ] }

-- | One world-thread tick, draining the queue the observe verbs write
--   to. Every assertion about a persisted record runs after one of
--   these, and so does every assertion that a record is ABSENT — so
--   "absent" means no command existed rather than that none had run.
drainWorld ∷ Bindings → IO ()
drainWorld b = do
    lastRef ← newIORef 0
    _ ← worldTickWith (pure 0) (bnEnv b) lastRef
    pure ()

knowledgeOf ∷ Bindings → IO PortableKnowledge
knowledgeOf b = wmPortableKnowledge <$> readIORef (worldManagerRef (bnEnv b))

-- * Lua plumbing

-- | The scene's Lua half: the real HUD booted with synthetic font and
--   texture handles, the window manager it owns, and the three capture
--   helpers every gesture below goes through.
--
--   Two seams, both deliberate and both narrow:
--
--     * @item.hitTestAt@ answers which ground id the player
--       right-clicked. A pixel hit test means nothing without a
--       rendered frame, and everything the menu does with that id —
--       resolving it to a row, reading @hasStorage@, resolving the
--       instance id, opening the level — stays real.
--     * @item.observeContainerContents@ is WRAPPED, not replaced: the
--       shim counts calls and forwards to the registered verb, which is
--       what lets "exactly one write" be proven by a count rather than
--       inferred from two equal timestamps.
sceneLua ∷ Text
sceneLua = T.concat
    [ "_G.__observes = 0; _G.__hit = nil; "
    , "local origObserve = item.observeContainerContents; "
    , "item.observeContainerContents = function(iid) "
    , "  _G.__observes = _G.__observes + 1; return origObserve(iid) end; "
    , "item.hitTestAt = function() return _G.__hit end; "
    , "local hud = require('scripts.hud'); "
    , "hud.init(1, 2, 1920, 1080); hud.createUI(); "
    , "_G.__hud = hud; "
    , "_G.__cip = require('scripts.cargo_inventory_panel'); "
    -- The ground-item menu, built by the real production module and
    -- captured instead of shown.
    , "_G.__groundMenu = function(gid) "
    , "  local cm = require('scripts.ui.context_menu'); "
    , "  local cim = require('scripts.init_context_menu_item'); "
    , "  _G.__hit = gid; "
    , "  local captured, orig = nil, cm.show; "
    , "  cm.show = function(items) captured = items end; "
    , "  cim.tryItemMenu(100, 100); "
    , "  cm.show = orig; _G.__hit = nil; "
    , "  return captured or {} end; "
    , "_G.__groundLabels = function(gid) "
    , "  local out = {}; "
    , "  for i, e in ipairs(_G.__groundMenu(gid)) do out[i] = e.label end; "
    , "  return table.concat(out, '|') end; "
    , "_G.__fireGround = function(gid, label) "
    , "  for _, e in ipairs(_G.__groundMenu(gid)) do "
    , "    if e.label == label and e.callback then e.callback(); "
    , "      return true end end; "
    , "  return false end; "
    -- A ROW's menu inside an open level, through the real widget
    -- dispatcher and the real row-menu builder.
    , "_G.__rowMenu = function(lvl, rowIdx) "
    , "  local il = require('scripts.ui.item_list'); "
    , "  local cm = require('scripts.ui.context_menu'); "
    , "  local level = _G.__cip.getLevel(lvl); if not level then return {} end; "
    , "  local row = il.getRows(level.listId)[rowIdx]; "
    , "  if not row then return {} end; "
    , "  local captured, orig = nil, cm.show; "
    , "  cm.show = function(items) captured = items end; "
    , "  il.handleCallback('onItemListRightClick', row.hitId); "
    , "  cm.show = orig; "
    , "  return captured or {} end; "
    , "_G.__rowLabels = function(lvl, rowIdx) "
    , "  local out = {}; "
    , "  for i, e in ipairs(_G.__rowMenu(lvl, rowIdx)) do out[i] = e.label end; "
    , "  return table.concat(out, '|') end; "
    , "_G.__fireRow = function(lvl, rowIdx, label) "
    , "  for _, e in ipairs(_G.__rowMenu(lvl, rowIdx)) do "
    , "    if e.label == label and e.callback then e.callback(); "
    , "      return true end end; "
    , "  return false end; "
    -- The row a level renders, by def name, so an assertion never
    -- depends on the grouping's hashmap enumeration order.
    , "_G.__rowAt = function(lvl, defName) "
    , "  local il = require('scripts.ui.item_list'); "
    , "  local level = _G.__cip.getLevel(lvl); if not level then return 0 end; "
    , "  for i, r in ipairs(il.getRows(level.listId)) do "
    , "    if r.item and r.item.defName == defName then return i end end; "
    , "  return 0 end; "
    , "_G.__rows = function(lvl) "
    , "  local il = require('scripts.ui.item_list'); "
    , "  local level = _G.__cip.getLevel(lvl); if not level then return 'nil' end; "
    , "  local parts = {}; "
    -- A rendered row is { hitId, item, index }: `item` is the group's
    -- representative, which is where every data field lives.
    , "  for _, r in ipairs(il.getRows(level.listId)) do "
    , "    local g = r.item or {}; "
    , "    parts[#parts+1] = string.format('%s:%d:%d', tostring(g.defName), "
    , "      g.count or 0, g.instanceId or -1) end; "
    , "  table.sort(parts); return table.concat(parts, ',') end; "
    -- The whole stack as `kind@instanceId/path` steps, which is exactly
    -- the identity a resize must round-trip.
    , "_G.__stack = function() "
    , "  local d = _G.__cip.dump(); local out = {}; "
    , "  for i, l in ipairs(d.levels) do "
    , "    out[i] = string.format('%s@%s/%s', tostring(l.kind), "
    , "      tostring(l.instanceId), table.concat(l.path or {}, '.')) end; "
    , "  return table.concat(out, '>') end; "
    -- The window-driven observation count starts from zero at the
    -- moment the gesture under test begins, so a scenario that had to
    -- SET UP a record through the same verb does not charge its own
    -- setup to the window.
    , "_G.__resetObserves = function() _G.__observes = 0; return true end; "
    , "_G.__field = function(lvl, key) "
    , "  local l = _G.__cip.dump().levels[lvl]; "
    , "  if not l then return 'nil' end; return tostring(l[key]) end; "
    , "return true" ]

-- | Evaluate a chunk, failing the example on a Lua error. The debug
--   console JSON-encodes its result, so a string comes back quoted.
luaOk ∷ Bindings → Text → IO Text
luaOk b src = do
    r ← executeDebugLua (lbsLuaState (bnLua b)) src
    r `shouldNotSatisfy` isLuaError
    pure (T.strip r)

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-- | An expression's value as unquoted text.
luaStr ∷ Bindings → Text → IO Text
luaStr b expr = T.filter (≢ '"') <$> luaOk b ("return " <> expr)

luaBool ∷ Bindings → Text → IO Bool
luaBool b expr = (≡ "true") <$> luaStr b expr

luaInt ∷ Bindings → Text → IO Int
luaInt b expr = do
    t ← luaStr b expr
    case reads (T.unpack t) of
        [(n, _)] → pure n
        _        → do expectationFailure ("not a number: " ⧺ T.unpack t)
                      pure (-1)

-- | Open the ground crate's portable level through the REAL context
--   menu, and confirm the entry was there to fire.
openGroundContents ∷ Bindings → Int → IO ()
openGroundContents b gid = do
    fired ← luaBool b ("__fireGround(" <> tshow gid <> ", 'Contents')")
    fired `shouldBe` True

-- | Take the contents observation the way PLC-7's verb does, and let
--   the world thread merge it.
observeContents ∷ Bindings → Word64 → IO ()
observeContents b iid = do
    ok ← luaBool b ("item.observeContainerContents(" <> tshow iid <> ")")
    ok `shouldBe` True
    drainWorld b

-- | Zero the window-driven observation counter. Every scenario that
--   had to establish a record through the same verb calls this before
--   the gesture under test, so setup is never charged to the window.
resetObserves ∷ Bindings → IO ()
resetObserves b = do
    ok ← luaBool b "__resetObserves()"
    ok `shouldBe` True

observeWeight ∷ Bindings → Word64 → IO ()
observeWeight b iid = do
    ok ← luaBool b ("item.observeContainerWeight(" <> tshow iid <> ")")
    ok `shouldBe` True
    drainWorld b

-- * Spec

spec ∷ Spec
spec = around withBindings $
  describe "Nested item contents" $
    describe "a portable container's own level (#2527)" $ do

    -- The scene every state case starts from: the crate and a plain
    -- non-storage bar on the ground, nothing observed yet.
    let onGround b = resetScene b [ (crateGid, stockedCrate)
                                  , (plainGid, plainCrate)
                                  , (barGid,   loose) ] Nothing

    describe "the ground-item Contents entry" $ do

        it "is offered for an item whose DEFINITION declares storage, \
           \and for no other ground item — not for an ordinary item, \
           \and not on the strength of a container kind alone" $ \b → do
            onGround b
            crateLabels ← luaStr b ("__groundLabels(" <> tshow crateGid <> ")")
            barLabels   ← luaStr b ("__groundLabels(" <> tshow barGid <> ")")
            crateLabels `shouldBe` "Info|Contents"
            -- The bar's def declares no storage: Info and nothing else.
            barLabels `shouldBe` "Info"

        it "is offered with NO unit selected, and opens a level with no \
           \unit and no building in its identity" $ \b → do
            onGround b
            selected ← luaInt b "#(unit.getSelected() or {})"
            selected `shouldBe` 0
            openGroundContents b crateGid
            luaStr b "__stack()" `shouldReturn`
                ("portableItem@" <> tshow crateId <> "/")
            luaStr b "__field(1, 'uid')" `shouldReturn` "nil"
            luaStr b "__field(1, 'bid')" `shouldReturn` "nil"

        it "opens the level for the crate's own INSTANCE id, never the \
           \page-local ground id it was hit-tested with" $ \b → do
            onGround b
            openGroundContents b crateGid
            -- The two differ by construction (crateGid is 0), so a
            -- level addressed by the ground id could not resolve at all.
            luaInt b "__cip.getLevel(1).src.instanceId"
                `shouldReturn` fromIntegral crateId

        it "writes NO observation in any of the four states — opening a \
           \remembered level is a pure read" $ \b → do
            onGround b
            -- never-inspected
            openGroundContents b crateGid
            -- weight-only
            observeWeight b crateId
            openGroundContents b crateGid
            -- known-contents
            observeContents b crateId
            resetObserves b
            openGroundContents b crateGid
            before ← knowledgeOf b
            luaInt b "_G.__observes" `shouldReturn` 0
            drainWorld b
            after ← knowledgeOf b
            after `shouldBe` before

    describe "the four knowledge states" $ do

        it "never-inspected: no weight, no age, and an empty text that \
           \says nobody has inspected it — never an empty list" $ \b → do
            onGround b
            openGroundContents b crateGid
            luaStr b "__field(1, 'knowledgeState')" `shouldReturn` "unknown"
            -- The LIVE capacity is still reported: the crate is right
            -- there on the floor and the player can see how big it is.
            luaStr b "__field(1, 'subtitle')"
                `shouldReturn` "Weight: unknown - holds up to 60.00 kg"
            luaStr b "__field(1, 'ageText')" `shouldReturn` "nil"
            luaStr b "__field(1, 'emptyText')"
                `shouldReturn` "Contents unknown (never inspected)"
            luaInt b "__field(1, 'rowCount')" `shouldReturn` 0

        it "weight-only: the remembered whole mass and an age taken \
           \from the WEIGHING, with the contents still unknown" $ \b → do
            onGround b
            observeWeight b crateId
            writeIORef (gameTimeRef (bnEnv b)) laterTime
            openGroundContents b crateGid
            luaStr b "__field(1, 'knowledgeState')"
                `shouldReturn` "weight-only"
            -- The crate's whole recursive mass, not its contents' — the
            -- two kits and their bandages ride in it.
            luaStr b "__field(1, 'subtitle')"
                `shouldReturn` "Weight: 14.60 kg - holds up to 60.00 kg"
            luaStr b "__field(1, 'ageText')" `shouldReturn` "as of 8m 20s ago"
            luaStr b "__field(1, 'emptyText')"
                `shouldReturn` "Contents unknown (never opened)"
            luaInt b "__field(1, 'rowCount')" `shouldReturn` 0

        it "known-empty: an OBSERVED-empty crate renders as empty, \
           \with an age — which is a different screen from \
           \never-inspected" $ \b → do
            resetScene b [(crateGid, crateEmpty)] Nothing
            observeContents b crateId
            writeIORef (gameTimeRef (bnEnv b)) laterTime
            openGroundContents b crateGid
            luaStr b "__field(1, 'knowledgeState')" `shouldReturn` "empty"
            luaStr b "__field(1, 'emptyText')" `shouldReturn` "(empty)"
            luaStr b "__field(1, 'ageText')" `shouldReturn` "as of 8m 20s ago"
            luaInt b "__field(1, 'rowCount')" `shouldReturn` 0

        it "known-contents: the remembered rows, grouped, with the \
           \contents age and no empty text at all" $ \b → do
            onGround b
            observeContents b crateId
            writeIORef (gameTimeRef (bnEnv b)) laterTime
            openGroundContents b crateGid
            luaStr b "__field(1, 'knowledgeState')" `shouldReturn` "known"
            luaStr b "__field(1, 'emptyText')" `shouldReturn` "nil"
            luaStr b "__field(1, 'ageText')" `shouldReturn` "as of 8m 20s ago"
            -- Two same-def kits group into ONE row of two, carrying a
            -- representative instance id.
            luaInt b "__field(1, 'rowCount')" `shouldReturn` 1
            rows ← luaStr b "__rows(1)"
            T.isPrefixOf "first_aid_kit:2:" rows `shouldBe` True

        it "a later WEIGHING does not make the remembered contents read \
           \as fresher: the contents age keeps deriving from the open" $
           \b → do
            onGround b
            observeContents b crateId
            writeIORef (gameTimeRef (bnEnv b)) laterTime
            observeWeight b crateId
            openGroundContents b crateGid
            -- The record is now weight-stamped at `laterTime` and
            -- contents-stamped at `observeTime`; the line must age from
            -- the older one.
            luaStr b "__field(1, 'knowledgeState')" `shouldReturn` "known"
            luaStr b "__field(1, 'ageText')" `shouldReturn` "as of 8m 20s ago"

        it "reports no capacity at all for a crate that can no longer \
           \be located, rather than a fabricated zero" $ \b → do
            onGround b
            observeContents b crateId
            -- The crate leaves the world entirely; its MEMORY does not.
            ws ← headWorldState b
            writeIORef (wsGroundItemsRef ws) (groundAt [(barGid, loose)])
            fired ← luaBool b
                ("__cip.openLevel({kind='portableItem', instanceId="
                 <> tshow crateId <> ", path={}}, 100, 100, 0)")
            fired `shouldBe` True
            luaStr b "__field(1, 'subtitle')" `shouldReturn` "Weight: 14.60 kg"

    describe "descending the remembered snapshot" $ do

        it "descends a nested kit by exact instance identity, and two \
           \SAME-DEFINITION kits answer with their own contents rather \
           \than each other's" $ \b → do
            onGround b
            observeContents b crateId
            openGroundContents b crateGid
            -- The base level groups both kits into one row, so each is
            -- opened by naming its own instance directly.
            okA ← luaBool b
                ("__cip.openLevel({kind='portableItem', instanceId="
                 <> tshow crateId <> ", path={" <> tshow kitId
                 <> "}}, 100, 100, 1)")
            okA `shouldBe` True
            luaStr b "__rows(2)" `shouldReturn`
                ("bandage:1:" <> tshow bandageId)
            okB ← luaBool b
                ("__cip.openLevel({kind='portableItem', instanceId="
                 <> tshow crateId <> ", path={" <> tshow kitBId
                 <> "}}, 100, 100, 1)")
            okB `shouldBe` True
            luaStr b "__rows(2)" `shouldReturn`
                ("bandage:1:" <> tshow bandageBId)

        it "opens a nested level through the ROW's own Contents entry, \
           \keeping the root crate's identity and extending the path" $
           \b → do
            onGround b
            observeContents b crateId
            openGroundContents b crateGid
            idx ← luaInt b "__rowAt(1, 'first_aid_kit')"
            idx `shouldSatisfy` (> 0)
            fired ← luaBool b
                ("__fireRow(1, " <> tshow idx <> ", 'Contents')")
            fired `shouldBe` True
            stack ← luaStr b "__stack()"
            -- Both levels are the SAME crate's record; only the path
            -- grows, because a nested container has no record of its own.
            T.isPrefixOf ("portableItem@" <> tshow crateId <> "/>portableItem@"
                          <> tshow crateId <> "/") stack `shouldBe` True

        it "a nested level whose path stops resolving closes, and takes \
           \every deeper level with it — forgetting the ROOT record \
           \invalidates the whole descent" $ \b → do
            onGround b
            observeContents b crateId
            openGroundContents b crateGid
            deep ← luaBool b
                ("__cip.openLevel({kind='portableItem', instanceId="
                 <> tshow crateId <> ", path={" <> tshow kitId
                 <> "}}, 100, 100, 1)")
            deep `shouldBe` True
            luaInt b "__cip.depth()" `shouldReturn` 2
            forgot ← luaBool b
                ("item.forgetContainerKnowledge(" <> tshow crateId <> ")")
            forgot `shouldBe` True
            drainWorld b
            _ ← luaOk b "__cip.update(0.016); return true"
            -- The BASE level survives: a forgotten crate is
            -- never-inspected again, which is a legitimate thing to be
            -- looking at. The nested level is not.
            luaInt b "__cip.depth()" `shouldReturn` 1
            luaStr b "__field(1, 'knowledgeState')" `shouldReturn` "unknown"

        it "never performs a live read: mutating the real crate after \
           \the observation changes neither the base rows nor a nested \
           \level's" $ \b → do
            onGround b
            observeContents b crateId
            openGroundContents b crateGid
            baseBefore ← luaStr b "__rows(1)"
            deep ← luaBool b
                ("__cip.openLevel({kind='portableItem', instanceId="
                 <> tshow crateId <> ", path={" <> tshow kitId
                 <> "}}, 100, 100, 1)")
            deep `shouldBe` True
            nestedBefore ← luaStr b "__rows(2)"
            -- Empty the live crate outright and move the clock on.
            ws ← headWorldState b
            writeIORef (wsGroundItemsRef ws)
                (groundAt [(crateGid, crate { iiContents = [] })])
            writeIORef (gameTimeRef (bnEnv b)) laterTime
            _ ← luaOk b "__cip.update(0.016); return true"
            luaInt b "__cip.depth()" `shouldReturn` 2
            luaStr b "__rows(1)" `shouldReturn` baseBefore
            luaStr b "__rows(2)" `shouldReturn` nestedBefore

    describe "the level is render-only (D-5)" $

        it "offers no transfer or Retrieve gesture on a portable row — \
           \only the inspection entry a container row earns" $ \b → do
            onGround b
            observeContents b crateId
            openGroundContents b crateGid
            idx ← luaInt b "__rowAt(1, 'first_aid_kit')"
            luaStr b ("__rowLabels(1, " <> tshow idx <> ")")
                `shouldReturn` "Contents"

    describe "the resize snapshot/restore pass" $

        it "round-trips a nested portable level's kind, instance id and \
           \path through a real framebuffer resize, and writes no \
           \observation doing it" $ \b → do
            onGround b
            observeContents b crateId
            openGroundContents b crateGid
            deep ← luaBool b
                ("__cip.openLevel({kind='portableItem', instanceId="
                 <> tshow crateId <> ", path={" <> tshow kitId
                 <> "}}, 100, 100, 1)")
            deep `shouldBe` True
            before ← luaStr b "__stack()"
            resetObserves b
            _ ← luaOk b "__hud.onFramebufferResize(1600, 900); return true"
            luaStr b "__stack()" `shouldReturn` before
            luaInt b "_G.__observes" `shouldReturn` 0

    describe "D-26: opening a carried container's live level" $ do

        let carried f b = resetScene b [(barGid, loose)]
                                       (Just (f, stockedCrate))

        it "records exactly ONE contents observation through the \
           \unit-info Contents gesture, and none on reopen through a \
           \layout rebuild" $ \b → do
            carried FactionPlayer b
            opened ← luaBool b
                ("require('scripts.item_contents_panel').openFor(1, \
                 \'supply_crate', 100, 100, " <> tshow crateId <> ")")
            opened `shouldBe` True
            luaInt b "_G.__observes" `shouldReturn` 1
            drainWorld b
            k ← knowledgeOf b
            portableState crateId k `shouldBe` KnownContents
            -- A resize destroys and rebuilds every level; that is not
            -- the player opening anything.
            _ ← luaOk b "__hud.onFramebufferResize(1600, 900); return true"
            luaInt b "_G.__observes" `shouldReturn` 1
            -- Nor is a per-tick refresh, a scroll or a tab change.
            _ ← luaOk b "__cip.update(0.016); \
                        \__cip.onPaneTabChange(__cip.getLevel(1), \
                        \  __cip.getLevel(1), 'Misc'); \
                        \__cip.refreshLevel(__cip.getLevel(1)); return true"
            luaInt b "_G.__observes" `shouldReturn` 1

        it "records one observation through the OTHER entry route too — \
           \descending from the unit endpoint level into its carried \
           \crate" $ \b → do
            carried FactionPlayer b
            opened ← luaBool b "__cip.openFor('unit', 1, 100, 100)"
            opened `shouldBe` True
            luaInt b "_G.__observes" `shouldReturn` 0
            idx ← luaInt b "__rowAt(1, 'supply_crate')"
            idx `shouldSatisfy` (> 0)
            fired ← luaBool b ("__fireRow(1, " <> tshow idx <> ", 'Contents')")
            fired `shouldBe` True
            luaInt b "_G.__observes" `shouldReturn` 1
            drainWorld b
            k ← knowledgeOf b
            portableState crateId k `shouldBe` KnownContents

        it "a DESCENT inside the carried crate writes nothing more: a \
           \container nested in an observed one gets no record of its \
           \own" $ \b → do
            carried FactionPlayer b
            opened ← luaBool b
                ("require('scripts.item_contents_panel').openFor(1, \
                 \'supply_crate', 100, 100, " <> tshow crateId <> ")")
            opened `shouldBe` True
            idx ← luaInt b "__rowAt(1, 'first_aid_kit')"
            idx `shouldSatisfy` (> 0)
            fired ← luaBool b ("__fireRow(1, " <> tshow idx <> ", 'Contents')")
            fired `shouldBe` True
            luaInt b "__cip.depth()" `shouldReturn` 2
            luaInt b "_G.__observes" `shouldReturn` 1
            drainWorld b
            k ← knowledgeOf b
            portableState kitId k `shouldBe` NeverInspected

        it "a NON-commandable unit's container is never observed by \
           \looking at it" $ \b → do
            carried FactionHostile b
            opened ← luaBool b
                ("require('scripts.item_contents_panel').openFor(1, \
                 \'supply_crate', 100, 100, " <> tshow crateId <> ")")
            -- The level still renders — a live read of a container an
            -- enemy carries is not a knowledge question — but nothing
            -- is written.
            opened `shouldBe` True
            luaInt b "_G.__observes" `shouldReturn` 0
            drainWorld b
            k ← knowledgeOf b
            portableState crateId k `shouldBe` NeverInspected

        it "a FAILED open writes nothing: the level is refused before \
           \any observation is taken" $ \b → do
            carried FactionPlayer b
            -- The unit does not hold instance 9001.
            opened ← luaBool b
                ("require('scripts.item_contents_panel').openFor(1, \
                 \'supply_crate', 100, 100, " <> tshow unlocatableId <> ")")
            opened `shouldBe` False
            luaInt b "__cip.depth()" `shouldReturn` 0
            luaInt b "_G.__observes" `shouldReturn` 0
            drainWorld b
            k ← knowledgeOf b
            portableState crateId k `shouldBe` NeverInspected

        it "keeps the omitted-instance-id fallback rendering, and \
           \writes nothing for it — no particular crate was named" $
           \b → do
            carried FactionPlayer b
            opened ← luaBool b
                "require('scripts.item_contents_panel').openFor(1, \
                \'supply_crate', 100, 100)"
            opened `shouldBe` True
            luaInt b "__cip.depth()" `shouldReturn` 1
            luaInt b "_G.__observes" `shouldReturn` 0
            drainWorld b
            k ← knowledgeOf b
            portableState crateId k `shouldBe` NeverInspected

-- | The one page this module's scene installs.
headWorldState ∷ Bindings → IO WorldState
headWorldState b = do
    mgr ← readIORef (worldManagerRef (bnEnv b))
    case wmWorlds mgr of
        ((_, ws) : _) → pure ws
        []            → fail "scene has no page"
