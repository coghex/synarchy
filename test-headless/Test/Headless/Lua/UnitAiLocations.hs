{-# LANGUAGE TypeApplications #-}
-- | The "unit location knowledge" gate (#915): @scripts/
--   unit_ai_locations.lua@'s per-unit location memory — identity dedup,
--   page-scoped nearest lookup, targeted forget, empty-state, the
--   persisted wire form, awareness ingest, and the load-time scrub —
--   plus the @lua.unit_ai@ v3→v4 schema evolution that carries it. The
--   component's DECLARED version has moved on since (v5 as of #1291),
--   so the version case below tracks the current one while still
--   pinning that every historical input version, v4 included, decodes.
--
--   Same standalone-Lua-VM pattern as "Test.Headless.Lua.SaveModules":
--   each 'it' runs one self-contained chunk via 'Lua.dostring' in a
--   fresh interpreter, asserting inside Lua via @assert()@/@error()@,
--   with a non-OK 'Lua.Status' surfaced as an hspec failure carrying the
--   Lua message. @scripts/unit_ai_locations.lua@ deliberately requires
--   nothing at module scope precisely so it is reachable this way; the
--   engine globals it uses (@world@, @unit@, @engine@) are stubbed per
--   test, which is also how the "unit elsewhere learns nothing" and
--   "dangling memory is dropped" scenarios are staged without an engine.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "unit location knowledge"'@.
module Test.Headless.Lua.UnitAiLocations (spec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | A minimal @engine@ global: the only thing these modules reach
--   outside a real boot (logWarn from the load-time scrub).
engineStub ∷ Text
engineStub =
    "engine = { logWarn = function(...) end, logInfo = function(...) end }"

runsOk ∷ Text → Expectation
runsOk chunkText = do
    result ← Lua.run @Lua.Exception $ do
        Lua.openlibs
        _ ← Lua.dostring (TE.encodeUtf8 engineStub)
        status ← Lua.dostring (TE.encodeUtf8 chunkText)
        case status of
            Lua.OK → return Nothing
            _ → do
                err ← Lua.tostring (-1)
                return (Just (maybe "<no message>" TE.decodeUtf8Lenient err))
    case result of
        Nothing  → pure ()
        Just msg → expectationFailure (T.unpack msg)

lns ∷ [Text] → Text
lns = T.intercalate "\n"

-- | Load the module under test and give the chunk an empty state table.
prelude ∷ Text
prelude = lns
    [ "local L = require('scripts.unit_ai_locations')"
    , "local s = {}"
    ]

spec ∷ Spec
spec = describe "unit location knowledge" $ do

    describe "identity dedup (NOT distance dedup)" $ do
        it "records a new location and reports it as newly learned" $
            runsOk $ lns
                [ prelude
                , "assert(L.addKnownLocation(s, 'main', 3, 8, 8) == true)"
                , "assert(#s.knownLocations == 1)"
                , "assert(s.knownLocations[1].page == 'main')"
                , "assert(s.knownLocations[1].id == 3)"
                , "assert(s.knownLocations[1].x == 8)"
                , "assert(s.knownLocations[1].y == 8)"
                ]

        it "re-recording the SAME (page, id) is not new knowledge and \
           \does not duplicate the entry" $
            runsOk $ lns
                [ prelude
                , "assert(L.addKnownLocation(s, 'main', 3, 8, 8) == true)"
                , "assert(L.addKnownLocation(s, 'main', 3, 8, 8) == false)"
                -- Even re-approached from a different tile, the anchor
                -- is the location's, so the identity is unchanged.
                , "assert(L.addKnownLocation(s, 'main', 3, 99, 99) == false)"
                , "assert(#s.knownLocations == 1)"
                ]

        it "keeps two DISTINCT instances that share an anchor tile — the \
           \water rule (same place within N tiles) must never apply here" $
            runsOk $ lns
                [ prelude
                -- Identical coordinates, different instance ids. Under
                -- knownWaterSources' distance dedup these would collapse
                -- into one; two locations are never the same location.
                , "assert(L.addKnownLocation(s, 'main', 1, 8, 8) == true)"
                , "assert(L.addKnownLocation(s, 'main', 2, 8, 8) == true)"
                , "assert(L.addKnownLocation(s, 'main', 3, 9, 8) == true)"
                , "assert(#s.knownLocations == 3)"
                ]

        it "the SAME instance number on two pages is two distinct \
           \memories — instance ids are allocated per page" $
            runsOk $ lns
                [ prelude
                , "assert(L.addKnownLocation(s, 'main', 1, 8, 8) == true)"
                , "assert(L.addKnownLocation(s, 'other', 1, 8, 8) == true)"
                , "assert(#s.knownLocations == 2)"
                , "assert(L.knowsLocation(s, 'main', 1))"
                , "assert(L.knowsLocation(s, 'other', 1))"
                , "assert(not L.knowsLocation(s, 'third', 1))"
                ]

        it "refuses a malformed identity rather than storing one" $
            runsOk $ lns
                [ prelude
                , "assert(L.addKnownLocation(s, nil, 1, 0, 0) == false)"
                , "assert(L.addKnownLocation(s, '', 1, 0, 0) == false)"
                , "assert(L.addKnownLocation(s, 'main', nil, 0, 0) == false)"
                , "assert(L.addKnownLocation(s, 'main', 1.5, 0, 0) == false)"
                , "assert(s.knownLocations == nil or #s.knownLocations == 0)"
                ]

    describe "nearest lookup" $ do
        it "picks the nearest memory ON THE GIVEN PAGE by its anchor" $
            runsOk $ lns
                [ prelude
                , "L.addKnownLocation(s, 'main', 1, 100, 0)"
                , "L.addKnownLocation(s, 'main', 2, 10, 0)"
                , "L.addKnownLocation(s, 'main', 3, 50, 0)"
                , "local n = L.nearestKnownLocation(s, 'main', 0, 0)"
                , "assert(n ~= nil and n.id == 2)"
                ]

        it "never returns a memory from another page, even when it is \
           \numerically closer" $
            runsOk $ lns
                [ prelude
                , "L.addKnownLocation(s, 'other', 1, 1, 1)"
                , "L.addKnownLocation(s, 'main', 2, 90, 90)"
                , "local n = L.nearestKnownLocation(s, 'main', 0, 0)"
                , "assert(n ~= nil and n.id == 2 and n.page == 'main')"
                , "assert(L.nearestKnownLocation(s, 'none', 0, 0) == nil)"
                ]

    describe "forget" $ do
        it "removes exactly the named memory and leaves its siblings" $
            runsOk $ lns
                [ prelude
                , "L.addKnownLocation(s, 'main', 1, 0, 0)"
                , "L.addKnownLocation(s, 'main', 2, 0, 0)"
                , "L.addKnownLocation(s, 'other', 2, 0, 0)"
                , "assert(L.forgetKnownLocation(s, 'main', 2) == true)"
                , "assert(#s.knownLocations == 2)"
                , "assert(L.knowsLocation(s, 'main', 1))"
                , "assert(L.knowsLocation(s, 'other', 2))"
                , "assert(not L.knowsLocation(s, 'main', 2))"
                ]

        it "forgetting something never known is a no-op, not an error" $
            runsOk $ lns
                [ prelude
                , "assert(L.forgetKnownLocation(s, 'main', 1) == false)"
                , "L.addKnownLocation(s, 'main', 1, 0, 0)"
                , "assert(L.forgetKnownLocation(s, 'main', 9) == false)"
                , "assert(#s.knownLocations == 1)"
                ]

    describe "empty state" $
        it "a unit that has never seen a location reports no knowledge \
           \and no nearest, without a knownLocations table existing" $
            runsOk $ lns
                [ prelude
                , "assert(L.hasKnownLocation(s) == false)"
                , "assert(L.nearestKnownLocation(s, 'main', 0, 0) == nil)"
                , "assert(L.knowsLocation(s, 'main', 1) == false)"
                , "assert(s.knownLocations == nil)"
                , "L.addKnownLocation(s, 'main', 1, 0, 0)"
                , "assert(L.hasKnownLocation(s) == true)"
                , "L.forgetKnownLocation(s, 'main', 1)"
                , "assert(L.hasKnownLocation(s) == false)"
                ]

    describe "acquisition from world.getLocationAwareness" $ do
        it "records a memory for the reported unit and NOT for a unit \
           \the engine did not report" $
            runsOk $ lns
                [ "local L = require('scripts.unit_ai_locations')"
                , "local aiState = {}"
                , "local function ensure(uid)"
                , "  aiState[uid] = aiState[uid] or {}; return aiState[uid] end"
                -- Unit 1 can see the location; unit 2 cannot, so the
                -- engine's shared SIGHT predicate (#1230) never names it.
                , "world = { getLocationAwareness = function() return {"
                , "  { uid = 1, page = 'main', instance_id = 3, gx = 8, gy = 9 }"
                , "} end }"
                , "assert(L.ingestAwareness(ensure) == 1)"
                , "assert(L.knowsLocation(aiState[1], 'main', 3))"
                , "assert(aiState[2] == nil)"
                -- Standing still re-reports the same row every tick; the
                -- identity dedup makes that a no-op.
                , "assert(L.ingestAwareness(ensure) == 0)"
                , "assert(#aiState[1].knownLocations == 1)"
                ]

        it "records the anchor the engine reported, so the nearest \
           \lookup needs no engine round-trip" $
            runsOk $ lns
                [ "local L = require('scripts.unit_ai_locations')"
                , "local aiState = {}"
                , "local function ensure(uid)"
                , "  aiState[uid] = aiState[uid] or {}; return aiState[uid] end"
                , "world = { getLocationAwareness = function() return {"
                , "  { uid = 1, page = 'main', instance_id = 3, gx = 8, gy = 9 }"
                , "} end }"
                , "L.ingestAwareness(ensure)"
                , "local n = L.nearestKnownLocation(aiState[1], 'main', 0, 0)"
                , "assert(n.x == 8 and n.y == 9)"
                ]

        it "is inert when the engine query is unavailable (bare Lua \
           \backend / preview boot) rather than erroring" $
            runsOk $ lns
                [ "local L = require('scripts.unit_ai_locations')"
                , "world = nil"
                , "assert(L.ingestAwareness(function() error('unreachable') end)"
                , "       == 0)"
                , "world = {}"
                , "assert(L.ingestAwareness(function() error('unreachable') end)"
                , "       == 0)"
                ]

    describe "persisted wire form" $ do
        it "tags each entry as a typed location_instance reference, \
           \carrying its page, and round-trips back to the live shape" $
            runsOk $ lns
                [ prelude
                , "L.addKnownLocation(s, 'main', 3, 8, 9)"
                , "local wire = L.wrapForSave(s.knownLocations)"
                , "assert(wire[1].__ref == 'location_instance')"
                , "assert(wire[1].page == 'main' and wire[1].id == 3)"
                , "assert(wire[1].x == 8 and wire[1].y == 9)"
                -- The LIVE shape never grows __ref -- only the bytes on
                -- disk change (the #764 contract).
                , "assert(s.knownLocations[1].__ref == nil)"
                , "local back = L.unwrapFromSave(wire)"
                , "assert(back[1].__ref == nil)"
                , "assert(back[1].page == 'main' and back[1].id == 3)"
                , "assert(back[1].x == 8 and back[1].y == 9)"
                , "assert(L.wrapForSave(nil) == nil)"
                , "assert(L.unwrapFromSave(nil) == nil)"
                ]

        it "is reported to the integrity graph as one location_instance \
           \edge per memory, each naming its own page and field path" $
            runsOk $ lns
                [ "local refs = require('scripts.unit_ai_save_refs')"
                , "local L = require('scripts.unit_ai_locations')"
                , "local s = {}"
                , "L.addKnownLocation(s, 'main', 3, 8, 9)"
                , "L.addKnownLocation(s, 'other', 3, 1, 1)"
                , "local edges = refs.references({ [7] = s })"
                , "local found = {}"
                , "for _, e in ipairs(edges) do"
                , "  if e.kind == 'location_instance' then"
                , "    found[e.path] = e end end"
                , "local a = found['unit[7].knownLocations[1]']"
                , "local b = found['unit[7].knownLocations[2]']"
                , "assert(a ~= nil and a.id == 3 and a.page == 'main')"
                , "assert(a.owner == 7)"
                , "assert(b ~= nil and b.id == 3 and b.page == 'other')"
                ]

        it "rejects a malformed persisted memory at validate() time, \
           \before it can reach live aiState" $
            runsOk $ lns
                [ "local refs = require('scripts.unit_ai_save_refs')"
                , "local function errsFor(entry)"
                , "  local errs = {}"
                , "  refs.validateRefTags(7, { __owner = { __ref = 'unit',"
                , "    id = 7 }, knownLocations = { entry } }, errs)"
                , "  return errs end"
                , "local ok = { __ref = 'location_instance', page = 'main',"
                , "             id = 3, x = 1, y = 2 }"
                , "assert(#errsFor(ok) == 0)"
                -- untagged / wrong kind / bad id / missing page / bad coords
                , "assert(#errsFor({ page = 'main', id = 3, x = 1, y = 2 }) > 0)"
                , "assert(#errsFor({ __ref = 'unit', page = 'main', id = 3,"
                , "                  x = 1, y = 2 }) > 0)"
                , "assert(#errsFor({ __ref = 'location_instance',"
                , "                  page = 'main', id = 0, x = 1, y = 2 }) > 0)"
                , "assert(#errsFor({ __ref = 'location_instance', id = 3,"
                , "                  x = 1, y = 2 }) > 0)"
                , "assert(#errsFor({ __ref = 'location_instance',"
                , "                  page = 'main', id = 3, x = 'a',"
                , "                  y = 2 }) > 0)"
                ]

    describe "lua.unit_ai schema evolution (v3 → v4)" $ do
        -- Capture the spec table the component registers, without an
        -- engine or a real save_modules registry behind it.
        -- The stub must cover BOTH registrations @register@ performs:
        -- the persistent component whose spec these cases inspect, and
        -- (since #1329) the @unit_ai_claims@ reset hook that clears the
        -- family's transient claim registries on load. A stub missing
        -- the second raises rather than reporting a spec problem.
        let registered = lns
                [ "local captured"
                , "package.loaded['scripts.lib.save_modules'] ="
                , "  { register = function(id, spec) captured = spec end,"
                , "    registerResetHook = function() end }"
                , "local unitAi = {}"
                , "package.loaded['scripts.unit_ai'] = unitAi"
                , "local aiState = {}"
                , "require('scripts.unit_ai_save').register(unitAi, aiState)"
                , "assert(captured ~= nil)"
                ]

        it "declares the current version (v6 since #1216) while still \
           \accepting every historical input version" $
            runsOk $ lns
                [ registered
                , "assert(captured.version == 6)"
                , "local accepted = {}"
                , "for _, v in ipairs(captured.inputVersions) do"
                , "  accepted[v] = true end"
                -- Every historical version decodes, this gate's own v4
                -- included: #915's memory rides payloads that predate
                -- #1291's stall accounting (v5) and #1216's position
                -- hold (v6) alike.
                , "for v = 1, 6 do assert(accepted[v], 'v' .. v"
                , "  .. ' must still decode') end"
                ]

        it "depends on world-pages, which owns the instance table its \
           \location references resolve against" $
            runsOk $ lns
                [ registered
                , "local deps = {}"
                , "for _, d in ipairs(captured.deps) do deps[d] = true end"
                , "assert(deps['world-pages'])"
                ]

        it "decodes a v1/v2/v3 payload with the memory ABSENT — never \
           \inferred from player-wide discovery" $
            runsOk $ lns
                [ registered
                -- A v3 payload: already-wrapped fields plus __owner, and
                -- no knownLocations, because the field did not exist.
                , "local v3 = { [7] = { __owner = { __ref = 'unit', id = 7 },"
                , "                     currentAction = 'idle' } }"
                , "local out3 = captured.decode(3, v3)"
                , "assert(out3[7].knownLocations == nil)"
                -- v1 is a bare payload migrated straight through.
                , "local v1 = { [7] = { currentAction = 'idle' } }"
                , "local out1 = captured.decode(1, v1)"
                , "assert(out1[7].knownLocations == nil)"
                , "assert(out1[7].__owner.__ref == 'unit')"
                , "local v2 = { [7] = { currentAction = 'idle' } }"
                , "local out2 = captured.decode(2, v2)"
                , "assert(out2[7].knownLocations == nil)"
                ]

        it "accepts and preserves a current-version (v4) payload's \
           \memories through decode + validate — including ones whose \
           \targets no longer exist, so a dangling memory can never \
           \block a load" $
            -- validate() deliberately checks SHAPE, never existence: the
            -- #761 tolerated-dangling-reference contract says a target
            -- that legitimately vanished before the save boundary must
            -- stay representable. It is reported as a non-blocking
            -- diagnostic (World.Save.Integrity) and scrubbed at
            -- reconcile time instead — see "load-time reconciliation".
            runsOk $ lns
                [ registered
                , "local v4 = { [7] = { __owner = { __ref = 'unit', id = 7 },"
                , "  knownLocations = {"
                , "    { __ref = 'location_instance', page = 'main', id = 3,"
                , "      x = 8, y = 9 },"
                , "    { __ref = 'location_instance', page = 'other', id = 3,"
                , "      x = 1, y = 2 } } } }"
                , "local out = captured.decode(4, v4)"
                , "assert(#out[7].knownLocations == 2)"
                , "assert(captured.validate(out) == nil)"
                ]

    describe "load-time reconciliation" $ do
        it "drops only a memory absent from the restored session, keeps \
           \every sibling, and never fails the load" $
            runsOk $ lns
                [ "local L = require('scripts.unit_ai_locations')"
                , "local s = {}"
                , "L.addKnownLocation(s, 'main', 1, 0, 0)"
                , "L.addKnownLocation(s, 'main', 9, 5, 5)"   -- gone
                , "L.addKnownLocation(s, 'other', 1, 7, 7)"
                -- The restored session: page 'main' kept instance 1,
                -- page 'other' kept instance 1. Instance 9 is gone.
                , "local live = { main = { [1] = true },"
                , "               other = { [1] = true } }"
                , "world = { getLocationInstance = function(id, page)"
                , "  local p = live[page]"
                , "  if p and p[id] then return { instance_id = id } end"
                , "  return nil end }"
                , "assert(L.scrubStaleKnownLocations(7, s) == 1)"
                , "assert(#s.knownLocations == 2)"
                , "assert(L.knowsLocation(s, 'main', 1))"
                , "assert(L.knowsLocation(s, 'other', 1))"
                , "assert(not L.knowsLocation(s, 'main', 9))"
                ]

        it "an instance that resolves on ANOTHER page is still dropped — \
           \a per-page id must not alias across worlds" $
            runsOk $ lns
                [ "local L = require('scripts.unit_ai_locations')"
                , "local s = {}"
                , "L.addKnownLocation(s, 'other', 4, 0, 0)"
                , "world = { getLocationInstance = function(id, page)"
                , "  if page == 'main' and id == 4 then return {} end"
                , "  return nil end }"
                , "assert(L.scrubStaleKnownLocations(7, s) == 1)"
                , "assert(#s.knownLocations == 0)"
                ]

        it "leaves memories untouched when the query is unavailable, \
           \rather than dropping everything" $
            runsOk $ lns
                [ "local L = require('scripts.unit_ai_locations')"
                , "local s = {}"
                , "L.addKnownLocation(s, 'main', 1, 0, 0)"
                , "world = {}"
                , "assert(L.scrubStaleKnownLocations(7, s) == 0)"
                , "assert(#s.knownLocations == 1)"
                ]
