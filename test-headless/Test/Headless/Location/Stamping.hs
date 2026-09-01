{-# LANGUAGE TypeApplications #-}
-- | The "location stamp completion" gate (#1719): a location's durable
--   stamped marker records that its geometry actually MATERIALIZED, not
--   that its builder was called.
--
--   Three layers are pinned here, because a regression in any one of
--   them is invisible from the other two:
--
--   * the two shipped builders in @scripts/locations.lua@ aggregate the
--     booleans @scripts/structures.lua@ already returns, and do so
--     without short-circuiting — every piece the builder would issue is
--     still issued, in the same order, after an earlier failure;
--   * @buildAt@ (and therefore @locations.stamp@) returns
--     @(ok, failedPlacementCount)@, where @ok@ is true only when the def
--     resolved, the builder ran, AND nothing it attempted failed. An
--     unknown id or unknown builder stays false with a count of zero;
--   * @scripts/location_stamper.lua@ writes @world.markLocationStamped@
--     only on that success, warns once per unsuccessful attempt with the
--     location id, page, anchor tile and failure count, and leaves the
--     chunk unmarked so the existing every-load dispatch retries it —
--     while content spawning keeps its independent one-time gate and
--     runs on every dispatch regardless of geometry outcome (#90).
--
--   Same standalone-Lua-VM pattern as "Test.Headless.Lua.UnitAiLocations":
--   each 'it' runs one self-contained chunk via 'Lua.dostring' in a fresh
--   interpreter, asserting inside Lua via @assert()@, with a non-OK
--   'Lua.Status' surfaced as an hspec failure carrying the Lua message.
--   @scripts/structures.lua@ is replaced through @package.loaded@ by a
--   recording stub, which is what lets a placement failure be injected at
--   a chosen call index without an engine, a page, or a loaded chunk.
--
--   Run just this gate: @cabal test synarchy-test-headless
--   --test-options='--match "location stamp"'@.
module Test.Headless.Location.Stamping (spec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

runsOk ∷ Text → Expectation
runsOk chunkText = do
    result ← Lua.run @Lua.Exception $ do
        Lua.openlibs
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

-- | Stubs for every engine global the two modules under test reach, plus
--   a recording replacement for @scripts.structures@. @failIf(index,
--   slot)@ decides which placement attempts report failure; @calls@
--   records every attempt in issue order, so a chunk can compare the
--   whole sequence between a clean run and a failing one.
--
--   @world.hasSpawnedLocationContents@ answers true (already spawned):
--   that both proves @locations.spawnContents@ was entered and stops it
--   before the content dispatch, which is a separate concern here.
harness ∷ Text
harness = lns
    [ "local rec = { warns = {}, infos = {}, marks = 0, spawnCalls = 0,"
    , "              stamped = false, markArgs = nil, watermarkPages = {} }"
    , "local defs = {}"
    , "engine = {"
    , "  logWarn = function(m) rec.warns[#rec.warns + 1] = m end,"
    , "  logInfo = function(m) rec.infos[#rec.infos + 1] = m end,"
    , "  listLocationDefs = function() return defs end,"
    , "}"
    , "world = {"
    , "  getTerrainAt = function() return nil, 0 end,"
    , "  setCell = function() end,"
    , "  setSlope = function() end,"
    , "  hasStampedLocation = function() return rec.stamped end,"
    , "  markLocationStamped = function(gx, gy, page, fromTok, toTok)"
    , "    rec.marks = rec.marks + 1"
    , "    rec.markArgs = { gx, gy, page, fromTok, toTok }"
    , "    rec.stamped = true"
    , "  end,"
    , "  hasSpawnedLocationContents = function()"
    , "    rec.spawnCalls = rec.spawnCalls + 1"
    , "    return true"
    , "  end,"
    , "  markLocationContentsSpawned = function() end,"
    , "}"
    , "local calls = {}"
    , "local failIf = nil"
    -- The engine `structure` global the stamper reads its commit window
    -- from (#2051). Only ACCEPTED placements take a token, so a stub
    -- placement that reports failure must not advance the watermark —
    -- mirroring structure.place, which validates before it stages.
    , "local watermark = 0"
    , "structure = {"
    , "  stageWatermark = function(page)"
    , "    rec.watermarkPages[#rec.watermarkPages + 1] = page"
    , "    return watermark"
    , "  end,"
    , "}"
    , "local function record(slot, gx, gy)"
    , "  calls[#calls + 1] = string.format('%s@%d,%d', slot, gx, gy)"
    , "  if failIf and failIf(#calls, slot) then return false end"
    , "  watermark = watermark + 1"
    , "  return true"
    , "end"
    , "package.loaded['scripts.structures'] = {"
    , "  floor   = function(gx, gy) return record('floor', gx, gy) end,"
    , "  ceiling = function(gx, gy) return record('ceiling', gx, gy) end,"
    , "  post    = function(gx, gy, c) return record('post_' .. c, gx, gy) end,"
    , "  wall    = function(gx, gy, e) return record('wall_' .. e, gx, gy) end,"
    , "}"
    , "local function seq() return table.concat(calls, ' ') end"
    , "local function resetCalls() calls = {} end"
    , "local function countKind(prefix)"
    , "  local n = 0"
    , "  for _, c in ipairs(calls) do"
    , "    if c:sub(1, #prefix) == prefix then n = n + 1 end"
    , "  end"
    , "  return n"
    , "end"
    -- A 5x5 def, the shipped ruin_small footprint: 25 floors + 4 corner
    -- posts + 20 perimeter walls = 49 attempts for the intact builder.
    , "local function def(id, builder)"
    , "  return { id = id, label = id, builder = builder, contents = {},"
    , "           bounds = { min_x = -2, min_y = -2, max_x = 2, max_y = 2 } }"
    , "end"
    , "local INTACT = 49"
    ]

spec ∷ Spec
spec = describe "location stamp completion" $ do

    describe "builder placement-result aggregation" $ do
        it "room_small reports zero failures and every attempt when all \
           \placements succeed" $
            runsOk $ lns
                [ harness
                , "defs = { def('ruin', 'room_small') }"
                , "local L = require('scripts.locations')"
                , "local ok, failed = L.stamp('ruin', 0, 0, 'page1')"
                , "assert(ok == true, 'expected a complete stamp')"
                , "assert(failed == 0, 'expected 0 failures, got ' .. tostring(failed))"
                , "assert(#calls == INTACT, 'expected ' .. INTACT .."
                , "       ' placements, got ' .. #calls)"
                ]

        it "room_small counts a failed placement and still issues every \
           \later piece in the same order" $
            runsOk $ lns
                [ harness
                , "defs = { def('ruin', 'room_small') }"
                , "local L = require('scripts.locations')"
                , "assert(L.stamp('ruin', 0, 0, 'page1') == true)"
                , "local baseline = seq()"
                , "resetCalls()"
                -- The FIRST floor fails: everything the builder issues
                -- afterwards must still be issued.
                , "failIf = function(i) return i == 1 end"
                , "local ok, failed = L.stamp('ruin', 0, 0, 'page1')"
                , "assert(ok == false, 'a failed placement must not report success')"
                , "assert(failed == 1, 'expected 1 failure, got ' .. tostring(failed))"
                , "assert(seq() == baseline,"
                , "       'the placement sequence changed after a failure')"
                ]

        it "room_small counts failures across floors, posts and walls" $
            runsOk $ lns
                [ harness
                , "defs = { def('ruin', 'room_small') }"
                , "local L = require('scripts.locations')"
                -- 1..25 floors, 26..29 posts, 30..49 walls.
                , "failIf = function(i) return i == 1 or i == 27 or i == 49 end"
                , "local ok, failed = L.stamp('ruin', 0, 0, 'page1')"
                , "assert(ok == false)"
                , "assert(failed == 3, 'expected 3 failures, got ' .. tostring(failed))"
                , "assert(#calls == INTACT, 'expected ' .. INTACT .."
                , "       ' placements, got ' .. #calls)"
                ]

        it "room_small_damaged reports zero failures on a clean stamp — a \
           \piece the collapse omits is not an attempt" $
            runsOk $ lns
                [ harness
                , "defs = { def('ruin', 'room_small_damaged') }"
                , "local L = require('scripts.locations')"
                , "local ok, failed = L.stamp('ruin', 0, 0, 'page1')"
                , "assert(ok == true, 'a fully collapsed-as-designed ruin is complete')"
                , "assert(failed == 0, 'omitted pieces must not count as failures, got '"
                , "       .. tostring(failed))"
                -- Every floor is kept; the post and wall counts are short
                -- of the intact builder's precisely because of the omissions.
                , "assert(countKind('floor') == 25, 'all 25 floors are kept')"
                , "assert(countKind('post_') == 3, 'exactly one corner post collapses')"
                , "assert(countKind('wall_') < 20, 'the perimeter must be breached')"
                , "assert(#calls < INTACT)"
                ]

        it "room_small_damaged counts a failed placement and still issues \
           \every later piece in the same order" $
            runsOk $ lns
                [ harness
                , "defs = { def('ruin', 'room_small_damaged') }"
                , "local L = require('scripts.locations')"
                , "assert(L.stamp('ruin', 4, 7, 'page1') == true)"
                , "local baseline = seq()"
                , "resetCalls()"
                , "failIf = function(i) return i == 1 or i == 26 end"
                , "local ok, failed = L.stamp('ruin', 4, 7, 'page1')"
                , "assert(ok == false)"
                , "assert(failed == 2, 'expected 2 failures, got ' .. tostring(failed))"
                , "assert(seq() == baseline,"
                , "       'the placement sequence changed after a failure')"
                ]

        it "an unknown id and an unknown builder stay false with a count \
           \of zero — nothing was attempted" $
            runsOk $ lns
                [ harness
                , "defs = { def('ruin', 'no_such_builder') }"
                , "local L = require('scripts.locations')"
                , "local ok, failed = L.stamp('nope', 0, 0, 'page1')"
                , "assert(ok == false and failed == 0)"
                , "local ok2, failed2 = L.stamp('ruin', 0, 0, 'page1')"
                , "assert(ok2 == false and failed2 == 0)"
                , "assert(#calls == 0, 'neither path may attempt a placement')"
                , "assert(#rec.warns == 2, 'each path warns for itself')"
                ]

    describe "the stamper's marker gate" $ do
        it "a failed stamp leaves the chunk unmarked, still spawns \
           \contents, and warns exactly once with the full context" $
            runsOk $ lns
                [ harness
                , "defs = { def('ruin', 'room_small') }"
                , "local stamper = require('scripts.location_stamper')"
                , "failIf = function(i) return i == 3 end"
                , "stamper.onStampLocation('page1', 'ruin', 8, 8)"
                , "assert(rec.marks == 0, 'a partial stamp must not be marked')"
                , "assert(rec.stamped == false)"
                , "assert(rec.spawnCalls == 1, 'content spawning is not gated on geometry')"
                , "assert(#rec.warns == 1, 'expected exactly one aggregate warning, got '"
                , "       .. #rec.warns)"
                , "local w = rec.warns[1]"
                , "assert(w:find('ruin', 1, true), 'warning names the location id')"
                , "assert(w:find('page1', 1, true), 'warning names the page')"
                , "assert(w:find('8,8', 1, true), 'warning names the anchor tile')"
                , "assert(w:find('1 placement', 1, true), 'warning names the count')"
                ]

        it "the next dispatch retries the unmarked chunk, marks it exactly \
           \once on success, and a third dispatch is a geometry no-op" $
            runsOk $ lns
                [ harness
                , "defs = { def('ruin', 'room_small') }"
                , "local stamper = require('scripts.location_stamper')"
                , "failIf = function(i) return i == 3 end"
                , "stamper.onStampLocation('page1', 'ruin', 8, 8)"
                , "assert(rec.marks == 0)"
                -- Retry: the chunk is still unmarked, so the every-load
                -- dispatch re-attempts it, and this time it completes.
                , "resetCalls()"
                , "failIf = nil"
                , "stamper.onStampLocation('page1', 'ruin', 8, 8)"
                , "assert(#calls == INTACT, 'the retry re-issues the whole builder')"
                , "assert(rec.marks == 1, 'a completed stamp marks exactly once')"
                , "assert(rec.markArgs[1] == 8 and rec.markArgs[2] == 8)"
                , "assert(rec.markArgs[3] == 'page1')"
                , "assert(rec.spawnCalls == 2)"
                -- Already stamped: #424's skip still holds.
                , "resetCalls()"
                , "stamper.onStampLocation('page1', 'ruin', 8, 8)"
                , "assert(#calls == 0, 'an already-stamped dispatch re-ran the builder')"
                , "assert(rec.marks == 1, 'an already-stamped dispatch marked again')"
                , "assert(rec.spawnCalls == 3, 'content spawning still runs every dispatch')"
                , "assert(#rec.warns == 1, 'only the one failed attempt warned')"
                ]

        it "carries the commit window bracketing exactly this \
           \invocation's accepted placements (#2051)" $
            runsOk $ lns
                [ harness
                , "defs = { def('ruin', 'room_small') }"
                , "local stamper = require('scripts.location_stamper')"
                -- A first, FAILING attempt: its 48 accepted placements
                -- advance the page watermark, so the retry's window must
                -- start above them rather than at zero.
                , "failIf = function(i) return i == 3 end"
                , "stamper.onStampLocation('page1', 'ruin', 8, 8)"
                , "assert(rec.marks == 0)"
                , "resetCalls()"
                , "failIf = nil"
                , "stamper.onStampLocation('page1', 'ruin', 8, 8)"
                , "assert(rec.marks == 1)"
                , "local fromTok, toTok = rec.markArgs[4], rec.markArgs[5]"
                , "assert(fromTok == INTACT - 1, 'window starts after the failed '"
                , "       .. 'attempt\\'s accepted placements, got ' .. tostring(fromTok))"
                , "assert(toTok - fromTok == INTACT, 'window spans this '"
                , "       .. 'invocation\\'s placements, got ' .. tostring(toTok - fromTok))"
                -- Both reads name the page being stamped, never the
                -- active one: a hidden secondary page has its own counter.
                , "for _, p in ipairs(rec.watermarkPages) do"
                , "  assert(p == 'page1', 'watermark read on the wrong page')"
                , "end"
                , "assert(#rec.watermarkPages == 4, 'one read either side of '"
                , "       .. 'each of the two builder runs')"
                ]

        it "every failed retry warns again — the warning is per attempt, \
           \not once for the location's lifetime" $
            runsOk $ lns
                [ harness
                , "defs = { def('ruin', 'room_small') }"
                , "local stamper = require('scripts.location_stamper')"
                -- Fail by SLOT, not call index: the same piece fails on
                -- every attempt, which is what a genuinely unreachable
                -- chunk or an unresolvable page looks like.
                , "failIf = function(_, slot) return slot == 'post_n' end"
                , "stamper.onStampLocation('page1', 'ruin', 8, 8)"
                , "stamper.onStampLocation('page1', 'ruin', 8, 8)"
                , "assert(rec.marks == 0, 'neither attempt completed, so neither marks')"
                , "assert(#rec.warns == 2, 'expected one warning per attempt, got '"
                , "       .. #rec.warns)"
                ]

        it "a clean first dispatch marks once and warns not at all" $
            runsOk $ lns
                [ harness
                , "defs = { def('ruin', 'room_small') }"
                , "local stamper = require('scripts.location_stamper')"
                , "stamper.onStampLocation('page1', 'ruin', 8, 8)"
                , "assert(#calls == INTACT)"
                , "assert(rec.marks == 1)"
                , "assert(#rec.warns == 0, 'a complete stamp is silent')"
                , "assert(rec.spawnCalls == 1)"
                ]

        it "an unknown id is not summarised a second time by the stamper, \
           \and never marks the chunk" $
            runsOk $ lns
                [ harness
                , "defs = {}"
                , "local stamper = require('scripts.location_stamper')"
                , "stamper.onStampLocation('page1', 'ghost', 8, 8)"
                , "assert(rec.marks == 0, 'an unresolvable location must not be marked')"
                -- Exactly the one warning locations.stamp's unknown-id
                -- path already emits; the stamper adds none on top.
                , "assert(#rec.warns == 1, 'expected no aggregate warning on top of the '"
                , "       .. 'existing unknown-id one, got ' .. #rec.warns)"
                , "for _, w in ipairs(rec.warns) do"
                , "  assert(not w:find('placement', 1, true),"
                , "         'the stamper must not summarise a path that attempted nothing')"
                , "end"
                ]
