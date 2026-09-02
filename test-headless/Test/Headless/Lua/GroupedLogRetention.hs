-- | The "grouped log history is bounded" gate (#2189).
--
--   @scripts\/combat_log.lua@ and @scripts\/injury_log_panel.lua@ capped only
--   their flat "All" ring (@ALL_RING_CAP = 200@). Every event was ALSO pushed
--   into an uncapped per-group list, and the group list itself
--   (@combatLog.battles@ \/ @injuryLog.unitLogs@) was append-only with no
--   removal path: a bare-Lua harness injecting 1,000 encounters separated
--   beyond the rejoin window retained 200 flat events but 1,000 group tabs and
--   1,000 grouped events. So a long session grew without bound, every ingest
--   scanned a growing list twice (lookup + name disambiguation), and every
--   render measured a tab per group ever seen.
--
--   Both panels now bound the grouped history to 64 groups of 200 events, and
--   this gate pins the whole contract for BOTH of them from one parameterised
--   fixture:
--
--     * the group count stops at 64 and the newest groups are the survivors;
--     * eviction is deterministic -- outside the rejoin window first, then
--       earliest @lastEventAt@, then smallest id (which is oldest creation
--       order, because the id allocator only ever counts up and an evicted id
--       is never reissued) -- including the case where every retained group is
--       still INSIDE the window;
--     * each group keeps its newest 200 events, by IDENTITY and in the panels'
--       newest-first order, not merely by count;
--     * total grouped retention cannot exceed 64 x 200;
--     * rejoin behaviour is unchanged for a group that is still retained, and
--       an evicted group cannot be rejoined even from inside the window;
--     * eviction leaves the tab and scroll state valid -- the active tab falls
--       back to @"all"@ with the content scroll zeroed, the tab-strip offset
--       is clamped, the panel is marked dirty so the strip is actually
--       rebuilt, and no transient handle of an evicted group survives that
--       rebuild; and
--     * neither the eviction nor the renderer's guarded scrollbar sync
--       behaves as a user action: no tab-click or user-scroll effect, and
--       every @onContentScroll@ invocation happens under @syncingScrollbar@
--       and leaves the scroll state alone.
--
--   The flat ring's own contract (200, newest-first, oldest dropped) and both
--   @unitEntries@ functions are re-pinned here too, since the point of the
--   change is that bounding the GROUPS must not touch them.
--
--   Technique: the shared headless engine plus one bare Lua VM, exactly as
--   "Test.Headless.Load.ReplacementTeardown" drives these same two modules.
--   The REAL panels run -- ingestion through their own @update@ drain, and for
--   the tab-strip cases a real @bootstrap@ + @show@ render on a real page tree
--   with synthetic texture\/font handles. Only the four engine reads they need
--   are stubbed: @engine.gameTime@, @unit.getInfo@ and the panel's own
--   @combat.drainEvents@ \/ @injury.drainEvents@.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "Lua.GroupedLogRetention"'@.
module Test.Headless.Lua.GroupedLogRetention (spec) where

import UPrelude
import Test.Hspec
import Data.IORef (newIORef, writeIORef, atomicModifyIORef')
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Engine.Core.State (EngineEnv(..))
import Engine.Core.Thread (ThreadControl(..))
import Engine.Graphics.Config (vcUIScale)
import Engine.Scripting.Lua.API (registerLuaAPI)
import Engine.Scripting.Lua.Thread (createLuaBackendState)
import Engine.Scripting.Lua.Thread.Console (executeDebugLua)
import Engine.Scripting.Lua.Types (LuaBackendState(..))
import Test.Headless.Harness (withHeadlessEngine)
import UI.Types

-----------------------------------------------------------
-- Fixture
-----------------------------------------------------------

luaLines ∷ [Text] → Text
luaLines = T.intercalate " "

withSharedFixture ∷ ((EngineEnv, LuaBackendState) → IO ()) → IO ()
withSharedFixture action = withHeadlessEngine $ \env → do
    ls ← createLuaBackendState (luaToEngineQueue env) (luaQueue env)
                               (assetPoolRef env) (nextObjectIdRef env)
                               (inputStateRef env) (loggerRef env)
    stateRef ← newIORef ThreadRunning
    registerLuaAPI (lbsLuaState ls) env ls stateRef
    action (env, ls)

isLuaError ∷ Text → Bool
isLuaError t = "error:" `T.isPrefixOf` t ∨ "syntax error:" `T.isPrefixOf` t

-- | Run one chunk, failing the example on a Lua error and stripping the
--   quotes 'executeDebugLua' puts around a string result. Every fixture here
--   returns a @'|'@-joined string, so one 'shouldBe' reads as a row.
eval ∷ LuaBackendState → Text → IO Text
eval ls src = do
    r ← executeDebugLua (lbsLuaState ls) src
    when (isLuaError r) $ expectationFailure ("Lua error: " ⧺ T.unpack r)
    pure $ if T.length r ≥ 2 ∧ T.head r ≡ '"' ∧ T.last r ≡ '"'
        then T.dropEnd 1 (T.drop 1 r)
        else r

-- | A clean slate per example: an empty page manager, uiscale 1 (so the tab
--   geometry is deterministic), and every module reloaded, which is what makes
--   each panel's @package.loaded@ singleton a fresh table.
resetPanel ∷ EngineEnv → LuaBackendState → Panel → IO ()
resetPanel env ls p = do
    writeIORef (uiManagerRef env) emptyUIPageManager
    atomicModifyIORef' (videoConfigRef env) $ \c → (c { vcUIScale = 1.0 }, ())
    cleared ← eval ls
        "for k, _ in pairs(package.loaded) do package.loaded[k] = nil end; \
        \return 'ok'"
    cleared `shouldBe` "ok"
    booted ← eval ls (bootLua p)
    booted `shouldBe` "ok"

-----------------------------------------------------------
-- The two panels, and the one fixture that drives either
-----------------------------------------------------------

-- | Everything the shared fixture needs to speak to one panel. @pEventFn@ and
--   @pVictimFn@ define encounter @k@: distinct participants per @k@, so a new
--   @k@ always opens a new group and never rejoins another one.
data Panel = Panel
    { pLabel    ∷ String
    , pModule   ∷ Text
    , pGroups   ∷ Text
    , pNextId   ∷ Text
    , pDrain    ∷ Text
    , pEventFn  ∷ Text
    , pVictimFn ∷ Text
    }

combatPanel ∷ Panel
combatPanel = Panel
    { pLabel   = "combat_log"
    , pModule  = "scripts.combat_log"
    , pGroups  = "battles"
    , pNextId  = "nextBattleId"
    , pDrain   = "combat.drainEvents"
    , pEventFn = "function EV(k, ts, seq) return { kind = 'miss', \
                 \attacker = 1000 + 2 * k, target = 1001 + 2 * k, \
                 \ts = ts, seq = seq, payload = {} } end"
    , pVictimFn = "function VICTIM(k) return 1001 + 2 * k end"
    }

injuryPanel ∷ Panel
injuryPanel = Panel
    { pLabel   = "injury_log_panel"
    , pModule  = "scripts.injury_log_panel"
    , pGroups  = "unitLogs"
    , pNextId  = "nextLogId"
    , pDrain   = "injury.drainEvents"
    , pEventFn = "function EV(k, ts, seq) return { kind = 'fall', \
                 \target = 1000 + k, ts = ts, seq = seq, payload = {} } end"
    , pVictimFn = "function VICTIM(k) return 1000 + k end"
    }

-- | The stubs and the driver. @feed1@ advances @engine.gameTime@ in step with
--   the event's own @ts@: the two are INDEPENDENT sources in production
--   (@lastEventAt@ is written from @ev.ts@, while the rejoin check and the
--   eviction preference read @engine.gameTime()@), so a fixed clock would
--   silently collapse every "outside the window" case into the all-inside
--   branch. @feed@ leaves the clock where the case put it.
bootLua ∷ Panel → Text
bootLua p = luaLines
    [ "_G.NOW = 0;"
    , "engine.gameTime = function() return _G.NOW end;"
    , "unit.getInfo = function(uid) return { defName = 'probe_unit',"
    , "  displayName = 'U' .. tostring(uid) } end;"
    , "_G.DRAIN = {};"
    , pDrain p <> " = function() local d = _G.DRAIN; _G.DRAIN = {}; return d end;"
    , "M = require('" <> pModule p <> "');"
    , "GROUPS, NEXTID = '" <> pGroups p <> "', '" <> pNextId p <> "';"
    , pEventFn p <> ";"
    , pVictimFn p <> ";"
    , "function G() return M[GROUPS] end;"
    , "function NEXT() return M[NEXTID] end;"
    , "function feed(evs) _G.DRAIN = evs; M.update(0.1) end;"
    , "function feed1(k, ts, seq) _G.NOW = ts; feed({ EV(k, ts, seq) }) end;"
    , "function ids() local t = {};"
    , "  for i, g in ipairs(G()) do t[i] = g.id end; return t end;"
    , "function hasId(id) for _, g in ipairs(G()) do"
    , "  if g.id == id then return true end end; return false end;"
    , "function byId(id) for _, g in ipairs(G()) do"
    , "  if g.id == id then return g end end; return nil end;"
    , "function grouped() local n = 0;"
    , "  for _, g in ipairs(G()) do n = n + #g.events end; return n end;"
    , "function count(t) local n = 0;"
    , "  for _ in pairs(t) do n = n + 1 end; return n end;"
    , "return 'ok'"
    ]

-----------------------------------------------------------
-- Spec
-----------------------------------------------------------

spec ∷ Spec
spec = aroundAll withSharedFixture $
  describe "grouped log history is bounded (#2189)" $ do

    mapM_ panelSpec [combatPanel, injuryPanel]

    describe "the unitEntries consumers" $
        it "unit_log reaches both panels only through unitEntries, never \
           \through a grouped list" $ \_ → do
            src ← TIO.readFile "scripts/unit_log.lua"
            T.isInfixOf "package.loaded[\"scripts.combat_log\"]" src
                `shouldBe` True
            T.isInfixOf "package.loaded[\"scripts.injury_log_panel\"]" src
                `shouldBe` True
            T.isInfixOf "m.unitEntries(uid)" src `shouldBe` True
            -- The grouped lists this issue bounds have no consumer outside
            -- their own modules, which is why bounding them is invisible here.
            T.isInfixOf "battles" src  `shouldBe` False
            T.isInfixOf "unitLogs" src `shouldBe` False

panelSpec ∷ Panel → SpecWith (EngineEnv, LuaBackendState)
panelSpec p = describe (pLabel p) $ do

    let run env ls src expected = do
            resetPanel env ls p
            eval ls src ≫= (`shouldBe` expected)

    it "retains at most 64 groups across 640 encounters separated beyond \
       \the rejoin window, keeping the newest" $ \(env, ls) →
        -- Each encounter is 1000s past the last, so every one of them is a
        -- new group AND every retained group is outside the 120s window: the
        -- eviction preference's "stale" branch, 576 times over.
        run env ls (luaLines
            [ "for k = 1, 640 do feed1(k, k * 1000, k) end;"
            , "return #G() .. '|' .. grouped() .. '|' .. NEXT() .. '|'"
            , "  .. ids()[1] .. '|' .. ids()[#G()] .. '|' .. #M.allEvents"
            , "  .. '|' .. M.allEvents[1].seq"
            ])
            -- 64 groups of one event each; ids 577..640 survive; the flat
            -- ring is untouched at 200, newest first.
            "64|64|641|577|640|200|640"

    it "evicts by earliest lastEventAt with a smallest-id tie-break when \
       \every retained group is still inside the rejoin window" $ \(env, ls) →
        -- The clock is pinned at 170 while the groups' own timestamps run
        -- 101..164, so `now - lastEventAt` never exceeds 120 and the
        -- "outside the window" preferred set stays EMPTY throughout.
        run env ls (luaLines
            [ "_G.NOW = 170;"
            -- Groups 1 and 2 tie at lastEventAt 101: the tie-break decides
            -- which goes first, and it must be the smaller id.
            , "feed({ EV(1, 101, 1), EV(2, 101, 2) });"
            , "for k = 3, 64 do feed({ EV(k, 100 + k, k) }) end;"
            , "local full = #G();"
            , "local allInside = true;"
            , "for _, g in ipairs(G()) do"
            , "  if (170 - g.lastEventAt) > 120 then allInside = false end end;"
            , "feed({ EV(65, 165, 65) }); local a = ids()[1];"
            , "feed({ EV(66, 166, 66) }); local b = ids()[1];"
            , "feed({ EV(67, 167, 67) }); local c = ids()[1];"
            , "return full .. '|' .. tostring(allInside) .. '|' .. a .. '|'"
            , "  .. b .. '|' .. c .. '|' .. #G() .. '|'"
            , "  .. tostring(hasId(65) and hasId(66) and hasId(67))"
            ])
            -- id 1 goes before id 2 on the tie, then id 2, then id 3 (101,
            -- 101, 103): the survivor list starts at 2, then 3, then 4.
            "64|true|2|3|4|64|true"

    it "evicts by recency, not list position, once a retained group has \
       \rejoined" $ \(env, ls) →
        -- The discriminating case: group 1 stays FIRST in the list forever
        -- (a rejoin updates lastEventAt in place, it never reorders), so an
        -- eviction that just took the head of the list would drop the most
        -- recently active group instead of the least.
        run env ls (luaLines
            [ "_G.NOW = 170;"
            , "for k = 1, 64 do feed({ EV(k, 100 + k, k) }) end;"
            , "feed({ EV(1, 168, 200) });"
            , "local head = ids()[1]; local rejoined = #byId(1).events;"
            , "feed({ EV(65, 169, 65) });"
            , "return head .. '|' .. rejoined .. '|' .. tostring(hasId(1))"
            , "  .. '|' .. tostring(not hasId(2)) .. '|' .. ids()[1] .. '|'"
            , "  .. #G()"
            ])
            -- Group 1 heads the list and survives; group 2, now the earliest
            -- at lastEventAt 102, is the one that goes.
            "1|2|true|true|1|64"

    it "prefers a group outside the rejoin window, and evicts the earliest \
       \one within that set" $ \(env, ls) →
        run env ls (luaLines
            [ "_G.NOW = 170;"
            , "for k = 1, 64 do feed({ EV(k, 100 + k, k) }) end;"
            -- Jump the clock past every retained group's window, then record
            -- who the policy has to pick before admitting one more.
            , "_G.NOW = 100000;"
            , "local stale = 0;"
            , "for _, g in ipairs(G()) do"
            , "  if (100000 - g.lastEventAt) > 120 then stale = stale + 1 end end;"
            , "local pickId, pickLast;"
            , "for _, g in ipairs(G()) do"
            , "  if pickLast == nil or g.lastEventAt < pickLast"
            , "     or (g.lastEventAt == pickLast and g.id < pickId) then"
            , "    pickLast, pickId = g.lastEventAt, g.id end end;"
            , "feed({ EV(65, 100001, 65) });"
            , "return stale .. '|' .. pickId .. '|' .. tostring(not hasId(pickId))"
            , "  .. '|' .. tostring(hasId(2)) .. '|' .. #G()"
            ])
            "64|1|true|true|64"

    it "keeps exactly the newest 200 events of a group, by identity and in \
       \newest-first order, across 2,000 events" $ \(env, ls) →
        -- One encounter fed 2,000 times, the clock one second per event so
        -- every one stays inside its own group's rejoin window and rejoins.
        run env ls (luaLines
            [ "for j = 1, 2000 do feed1(1, j, j) end;"
            , "local g = G()[1];"
            , "local descending = true;"
            , "for i = 1, #g.events - 1 do"
            , "  if g.events[i].seq ~= g.events[i + 1].seq + 1 then"
            , "    descending = false end end;"
            , "return #G() .. '|' .. #g.events .. '|' .. g.events[1].seq .. '|'"
            , "  .. g.events[#g.events].seq .. '|' .. tostring(descending)"
            , "  .. '|' .. #M.allEvents .. '|' .. M.allEvents[1].seq .. '|'"
            , "  .. M.allEvents[#M.allEvents].seq"
            ])
            -- seq 2000 down to 1801, contiguous: adding event 201 dropped the
            -- OLDEST, not an arbitrary one. The flat ring holds the same
            -- window under its own untouched cap.
            "1|200|2000|1801|true|200|2000|1801"

    it "cannot retain more than 64 x 200 grouped events" $ \(env, ls) →
        run env ls (luaLines
            [ "for k = 1, 70 do local base = (k - 1) * 100000;"
            , "  for j = 1, 250 do feed1(k, base + j, j) end end;"
            , "return #G() .. '|' .. grouped() .. '|'"
            , "  .. tostring(grouped() <= 64 * 200)"
            ])
            "64|12800|true"

    it "reuses a retained group inside the rejoin window, but opens a new \
       \group for an evicted one even while it is still inside" $ \(env, ls) →
        run env ls (luaLines
            [ "_G.NOW = 170;"
            , "for k = 1, 64 do feed({ EV(k, 100 + k, k) }) end;"
            -- (1) a RETAINED group inside the window still absorbs the event:
            -- no new group, no id burned.
            , "feed({ EV(64, 165, 100) });"
            , "local rejoined = #G() == 64 and #byId(64).events == 2"
            , "  and NEXT() == 65;"
            -- (2) one more encounter evicts the earliest, which is group 1.
            , "feed({ EV(65, 166, 101) });"
            -- (3) group 1's participants are STILL inside the window"
            -- (170 - 101 = 69 <= 120), but its group is gone, so this opens a
            -- fresh one rather than resurrecting it.
            , "local fresh = NEXT();"
            , "feed({ EV(1, 167, 102) });"
            , "return tostring(rejoined) .. '|' .. tostring(not hasId(1)) .. '|'"
            , "  .. tostring(hasId(fresh)) .. '|' .. #byId(fresh).events .. '|'"
            , "  .. (170 - 101) .. '|' .. #G()"
            ])
            "true|true|true|1|69|64"

    it "leaves unitEntries and the flat ring untouched when a group inside \
       \their horizon is evicted" $ \(env, ls) →
        run env ls (luaLines
            [ "for k = 1, 640 do feed1(k, k * 1000, k) end;"
            -- Encounter 500 is inside the 200-event allEvents horizon
            -- (441..640) but OUTSIDE the 64 retained groups (577..640), so
            -- this is exactly the case where group eviction could have been
            -- visible to a consumer -- and must not be.
            , "local e500 = M.unitEntries(VICTIM(500));"
            , "local e300 = M.unitEntries(VICTIM(300));"
            , "return tostring(hasId(500)) .. '|' .. #e500 .. '|' .. e500[1].ts"
            , "  .. '|' .. #e300 .. '|' .. #M.allEvents .. '|'"
            , "  .. M.allEvents[1].seq .. '|' .. M.allEvents[200].seq"
            ])
            -- Encounter 300 has fallen off the flat ring, which is the
            -- pre-existing horizon this issue does not move.
            "false|1|500000|0|200|640|441"

    it "selects all, zeroes the content scroll, clamps the tab-strip offset \
       \and marks the panel dirty when the ACTIVE group is evicted" $ \(env, ls) →
        -- Read with the panel HIDDEN, so this pins the state eviction itself
        -- leaves. Selecting "all" also arms justifyBottom, so a render would
        -- legitimately pull contentScroll back to the foot of the log -- that
        -- chat-style auto-follow is existing behaviour, not this issue's.
        run env ls (luaLines
            [ "M.bootstrap(1, 2, 3, 1280, 720);"
            , "_G.NOW = 170;"
            , "for k = 1, 64 do feed({ EV(k, 100 + k, k) }) end;"
            , "M.activeTabId   = 1;"   -- the group the next admission evicts
            , "M.contentScroll = 5;"
            -- 63 is the largest offset 64 whole tabs can carry; after one
            -- eviction the list is momentarily 63 long, so a correct clamp
            -- pulls both down to 62 and leaves them there.
            , "M.scrollOffset  = 63; M.tabMaxScroll = 63;"
            , "M.dirty = false;"
            , "feed({ EV(65, 165, 65) });"
            , "return tostring(M.activeTabId) .. '|' .. M.contentScroll .. '|'"
            , "  .. M.scrollOffset .. '|' .. M.tabMaxScroll .. '|'"
            , "  .. tostring(M.dirty) .. '|' .. tostring(not hasId(1)) .. '|'"
            , "  .. #G() .. '|' .. tostring(M.scrollOffset <= #G() - 1)"
            ])
            "all|0|62|62|true|true|64|true"

    it "marks the panel dirty even when the evicted group is not the active \
       \tab, which is the only way the stale tab gets rebuilt away" $ \(env, ls) →
        -- processEvent's own tail sets dirty only for the ACTIVE tab, so with
        -- a surviving group selected the flag can only have come from the
        -- eviction. Panel hidden, so nothing consumes the flag first.
        run env ls (luaLines
            [ "M.bootstrap(1, 2, 3, 1280, 720);"
            , "_G.NOW = 170;"
            , "for k = 1, 64 do feed({ EV(k, 100 + k, k) }) end;"
            , "M.activeTabId = 64; M.dirty = false;"
            , "feed({ EV(65, 165, 65) });"
            , "return tostring(M.dirty) .. '|' .. tostring(M.activeTabId) .. '|'"
            , "  .. tostring(not hasId(1)) .. '|' .. tostring(hasId(64)) .. '|'"
            , "  .. #G()"
            ])
            "true|64|true|true|64"

    it "rebuilds the open tab strip so no handle of an evicted group \
       \survives, without any user-originated tab or scroll effect" $ \(env, ls) →
        -- The panel is really open here: bootstrap + show build a real page
        -- tree, so tabClickBoxes genuinely maps the evicted group before the
        -- eviction (asserted as a precondition, or the "no stale handle"
        -- result below would be vacuous).
        run env ls (luaLines
            [ "M.bootstrap(1, 2, 3, 1280, 720);"
            , "_G.NOW = 170;"
            , "for k = 1, 64 do feed({ EV(k, 100 + k, k) }) end;"
            , "M.show();"
            , "assert(M.isVisible(), 'fixture: the panel must be open');"
            -- A SURVIVING group owns the active tab, so the render that
            -- clears the strip can only be the one eviction asked for: no
            -- group flips quiescent at this clock either.
            , "M.activeTabId = 64; M.dirty = false;"
            , "local boxes = count(M.tabClickBoxes); local sawEvicted = false;"
            , "for _, id in pairs(M.tabClickBoxes) do"
            , "  if id == 1 then sawEvicted = true end end;"
            -- Spies: every user-originated route on the module, every button
            -- onClick closure built from here on, and a record of each
            -- onContentScroll call -- the renderer's own scrollbar sync
            -- invokes it, and that invocation must change nothing.
            , "_G.FIRED, _G.GUARDED, _G.LEAKED = 0, 0, 0;"
            , "for _, k in ipairs({ 'onTabClick', 'onScrollPrev', 'onScrollNext' }) do"
            , "  local f = M[k];"
            , "  M[k] = function(...) _G.FIRED = _G.FIRED + 1; return f(...) end end;"
            , "local oc = M.onContentScroll;"
            , "M.onContentScroll = function(off)"
            , "  local guarded, before = M.syncingScrollbar, M.contentScroll;"
            , "  local r = oc(off);"
            , "  if guarded and M.contentScroll == before then"
            , "    _G.GUARDED = _G.GUARDED + 1"
            , "  else _G.LEAKED = _G.LEAKED + 1 end;"
            , "  return r end;"
            , "local btn = require('scripts.ui.button'); local origNew = btn.new;"
            , "btn.new = function(params) local cb = params and params.onClick;"
            , "  if cb then params.onClick = function(...)"
            , "    _G.FIRED = _G.FIRED + 1; return cb(...) end end;"
            , "  return origNew(params) end;"
            , "feed({ EV(65, 165, 65) });"
            , "local stale = 0;"
            , "for _, id in pairs(M.tabClickBoxes) do"
            , "  if id ~= 'all' and not hasId(id) then stale = stale + 1 end end;"
            , "return tostring(sawEvicted) .. '|' .. tostring(boxes > 1) .. '|'"
            , "  .. stale .. '|' .. _G.FIRED .. '|' .. _G.LEAKED .. '|'"
            , "  .. tostring(_G.GUARDED > 0) .. '|' .. tostring(not hasId(1))"
            , "  .. '|' .. #G()"
            ])
            -- The strip did map the evicted group, the rebuild left nothing
            -- pointing at a departed one, no player route fired, and the one
            -- guarded onContentScroll the sync makes leaked nothing.
            "true|true|0|0|0|true|true|64"
