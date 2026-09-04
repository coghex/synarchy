-- | Per-entity component application and the restored-entity context
--   it filters against -- one of the four owners
--   'Test.Headless.Lua.SaveModules' composes (issue #2047).
--
--   Two groups: @applyEntityRows@' own filtering and drop diagnostics
--   (issue #900), and the per-component isolation of the restored
--   context each @apply@ is handed (issue #1279).
--
--   Its domain fixtures live here with it: 'perEntitySpec', the shape
--   both production per-entity callbacks have, and 'captureWarnings'.
module Test.Headless.Lua.SaveModules.EntityApplication (spec) where

import UPrelude
import Test.Hspec
import qualified Data.Text as T

import Test.Headless.Lua.SaveModules.Support (lns, runsOk)

-- | Issue #900: a per-entity component whose @apply@ is nothing but an
--   'applyEntityRows' call against @live@, the shape both production
--   per-entity callbacks have. Module-level since issue #1279's isolation
--   block needs the same fixture as the #900 filtering block.
perEntitySpec ∷ Text → Text → Text
perEntitySpec name live = T.concat
    [ "{ version=1, inputVersions={1}, required=true, "
    , "scope='global', deps={},"
    , "  snapshot=function()"
    , "    local out = {}"
    , "    for k, v in pairs(", live, ") do out[k] = v end"
    , "    return out end,"
    , "  decode=function(v,d) return d end,"
    , "  validate=function() return nil end,"
    , "  apply=function(data, entities)"
    , "    require('scripts.lib.save_modules').applyEntityRows("
    , live, ", data, entities,"
    , "      { kind='unit', component='", name, "' })"
    , "  end }"
    ]

-- | Collect @engine.logWarn@ text into a Lua-local @warnings@ array, so a
--   chunk can assert on the absent-owner drop diagnostic.
captureWarnings ∷ [Text]
captureWarnings =
    [ "local warnings = {}"
    , "engine.logWarn = function(msg)"
    , "  warnings[#warnings + 1] = tostring(msg) end"
    ]

spec ∷ Spec
spec = do
    describe "per-entity component application (issue #900)" $ do
        it "applies each row against the restored session's own entity \
           \set: a row whose owner is absent is dropped with a diagnostic \
           \while its siblings apply normally" $ runsOk $ lns $
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            ] ⧺ captureWarnings ⧺
            [ "local live = {}"
            , "saveModules.register('pe_drop', "
              <> perEntitySpec "pe_drop" "live" <> ")"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='pe_drop', version=1,"
            , "      payload=codec.encode({ [7]={tag='seven'},"
            , "                             [9]={tag='nine'} }) } },"
            , "  1, false, { unit = { [7]=true }, building = {} })"
            , "assert(prep.ok, 'an absent owner is tolerated-dangling, "
              <> "never a load failure: ' .. tostring(prep.errors and prep.errors[1]))"
            , "saveModules.applyAll()"
            , "assert(live[7] ~= nil and live[7].tag == 'seven',"
            , "  'a row whose unit is in the restored session must apply')"
            , "assert(live[9] == nil,"
            , "  'a row whose unit is absent must be dropped, not applied')"
            , "assert(#warnings == 1, 'the drop must emit exactly one "
              <> "diagnostic, got ' .. #warnings)"
            , "assert(warnings[1]:find('9', 1, true) ~= nil,"
            , "  'the diagnostic must name the dropped entity: ' .. warnings[1])"
            ]

        it "keeps structurally malformed rows FAIL-FAST even when their \
           \owner is absent from the restored session -- only unresolved \
           \OWNERSHIP is tolerated-and-dropped, never malformed data" $
            runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local live = {}"
            , "saveModules.register('pe_strict', { version=1, "
              <> "inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function(d)"
            , "    local errs = {}"
            , "    for uid, row in pairs(d) do"
            , "      if type(row) ~= 'table' then"
            , "        errs[#errs+1] = 'row ' .. tostring(uid)"
            , "          .. ' is not a table' end"
            , "    end"
            , "    if #errs > 0 then return errs end"
            , "    return nil end,"
            , "  apply=function(data, entities)"
            , "    saveModules.applyEntityRows(live, data, entities,"
            , "      { kind='unit', component='pe_strict' })"
            , "  end })"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='pe_strict', version=1,"
            , "      payload=codec.encode({ [4]='not_a_table' }) } },"
            , "  1, false, { unit = {}, building = {} })"
            , "assert(not prep.ok, 'a malformed row must fail the whole "
              <> "load even though its owner is absent')"
            ]

        it "leaves the module holding EXACTLY the payload's applicable \
           \rows: a live row with no row of its own in the payload does \
           \not survive, even when its id exists in the restored session \
           \(ids are session-global allocators that restart per session, \
           \so a merge would let the new session's unit 7 inherit the old \
           \session's unit 7 state)" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local live = { [7] = { tag='OLD SESSION' } }"
            , "saveModules.register('pe_exact', "
              <> perEntitySpec "pe_exact" "live" <> ")"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='pe_exact', version=1,"
            , "      payload=codec.encode({ [9]={tag='nine'} }) } },"
            , "  1, false, { unit = { [7]=true, [9]=true }, building = {} })"
            , "assert(prep.ok)"
            , "saveModules.applyAll()"
            , "assert(live[7] == nil, 'a pre-load row absent from the "
              <> "payload must NOT survive, even though unit 7 exists in "
              <> "the restored session')"
            , "assert(live[9] ~= nil and live[9].tag == 'nine')"
            ]

        it "rolls a failed apply back VERBATIM, with no ownership \
           \filtering -- the rollback capture is the OLD session's \
           \snapshot, whose owners are by definition absent from the \
           \RESTORED context, so filtering it would erase exactly the \
           \state the rollback exists to restore" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local live = { [42] = { tag='preload' } }"
            , "saveModules.register('pe_a_rollback', "
              <> perEntitySpec "pe_a_rollback" "live" <> ")"
            , "-- Sorts after pe_a_rollback, so it applies second."
            , "saveModules.register('pe_z_boom', { version=1, "
              <> "inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function() error('synthetic apply failure') end })"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='pe_a_rollback', version=1,"
            , "      payload=codec.encode({ [9]={tag='nine'} }) },"
            , "    { id='pe_z_boom', version=1, payload=codec.encode({}) } },"
            , "  1, false, { unit = { [9]=true }, building = {} })"
            , "assert(prep.ok)"
            , "local ok = pcall(saveModules.applyAll)"
            , "assert(not ok, 'the failing component must abort the apply')"
            , "assert(live[42] ~= nil and live[42].tag == 'preload',"
            , "  'rollback must restore the pre-load row verbatim even "
              <> "though unit 42 is absent from the restored session')"
            , "assert(live[9] == nil, 'the rolled-back component must not "
              <> "retain the new session rows')"
            ]

        it "resolves per-page nested references through the OWNING unit's \
           \page, so equal per-page ids on two different pages neither \
           \alias nor contaminate one another (rows themselves stay keyed \
           \by the session-global unit id and are never re-keyed per page)" $
            runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local live = {}"
            , "saveModules.register('pe_pages', { version=1, "
              <> "inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function(data, entities)"
            , "    saveModules.applyEntityRows(live, data, entities,"
            , "      { kind='unit', component='pe_pages' })"
            , "    -- A billId is a PER-PAGE allocator: it only means"
            , "    -- anything relative to its owning unit's page."
            , "    for uid, row in pairs(live) do"
            , "      row.billPage = entities and entities.unitPage"
            , "        and entities.unitPage[uid] or nil"
            , "    end"
            , "  end })"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='pe_pages', version=1,"
            , "      payload=codec.encode({ [3]={billId=1}, [4]={billId=1} }) } },"
            , "  1, false, { unit = { [3]=true, [4]=true }, building = {},"
            , "              unitPage = { [3]='alpha', [4]='beta' } })"
            , "assert(prep.ok)"
            , "saveModules.applyAll()"
            , "assert(live[3].billId == 1 and live[4].billId == 1,"
            , "  'both pages legitimately carry bill id 1')"
            , "assert(live[3].billPage == 'alpha', 'unit 3 resolves to its "
              <> "own page, got ' .. tostring(live[3].billPage))"
            , "assert(live[4].billPage == 'beta', 'unit 4 resolves to its "
              <> "own page, got ' .. tostring(live[4].billPage))"
            ]

        it "treats a CONTEXTLESS applyAll as 'apply every row' rather than \
           \'the restored session is empty', so every pre-#900 caller \
           \keeps working unchanged" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local live = {}"
            , "saveModules.register('pe_nocontext', "
              <> perEntitySpec "pe_nocontext" "live" <> ")"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='pe_nocontext', version=1,"
            , "      payload=codec.encode({ [7]={tag='seven'},"
            , "                             [9]={tag='nine'} }) } })"
            , "assert(prep.ok)"
            , "saveModules.applyAll()"
            , "assert(live[7] ~= nil and live[9] ~= nil,"
            , "  'no context must mean no ownership filtering')"
            ]

    -- Issue #1279: #900 requirement 1 promised apply() "a read-only
    -- restored-entity context" and shipped one shared MUTABLE table, so an
    -- earlier component could delete or rewrite entries a later one still
    -- filters against -- making apply ORDER load-correctness-relevant and
    -- contradicting Haskell's authoritative KnownEntities. The production
    -- callbacks only read it, so this was latent; nothing enforced it.
    --
    -- The mechanism is an independent per-component COPY, so these cases
    -- assert the observable guarantee (isolation) rather than a raise:
    -- a component's mutations land on its own copy and reach nobody.
    describe "restored-entity context isolation (issue #1279)" $ do
        -- Sorts before the observer, so it applies FIRST -- the ordering
        -- this whole block exists to make irrelevant. The `entities == nil`
        -- guard is the ROLLBACK pass, contextless by design (#900).
        let mutatorSpec body = T.concat
                [ "{ version=1, inputVersions={1}, required=true, "
                , "scope='global', deps={},"
                , "  snapshot=function() return {} end,"
                , "  decode=function(v,d) return d end,"
                , "  validate=function() return nil end,"
                , "  apply=function(data, entities)"
                , "    if entities == nil then return end "
                , body, " end }"
                ]

        it "keeps a LATER component's view of membership and owner pages \
           \exactly as Haskell supplied it after an earlier component \
           \deletes, inserts, rewrites an owner page, rebinds an outer \
           \field and rawsets -- and applyEntityRows still retains the \
           \present-owner row while dropping the genuinely absent-owner one" $
            runsOk $ lns $
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            ] ⧺ captureWarnings ⧺
            [ "local live = {}"
            , "local seen = {}"
            -- Every write here SUCCEEDS -- on this component's own copy.
            -- The point is that none of them is observable downstream.
            , "saveModules.register('iso_a_mutator', "
              <> mutatorSpec (T.concat
                  [ "entities.unit[7] = nil;"
                  , "entities.unit[9] = true;"
                  , "entities.unitPage[7] = 'HIJACKED';"
                  , "entities.building[1] = true;"
                  , "rawset(entities, 'unit', { [999]=true })"
                  ]) <> ")"
            , "saveModules.register('iso_z_observer', { version=1, "
              <> "inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function(data, entities)"
            , "    seen.unit7 = entities.unit[7]"
            , "    seen.unit9 = entities.unit[9]"
            , "    seen.unit999 = entities.unit[999]"
            , "    seen.page7 = entities.unitPage[7]"
            , "    seen.building1 = entities.building[1]"
            , "    saveModules.applyEntityRows(live, data, entities,"
            , "      { kind='unit', component='iso_z_observer' })"
            , "  end })"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='iso_a_mutator', version=1, payload=codec.encode({}) },"
            , "    { id='iso_z_observer', version=1,"
            , "      payload=codec.encode({ [7]={tag='seven'},"
            , "                             [9]={tag='nine'} }) } },"
            , "  1, false, { unit = { [7]=true }, building = {},"
            , "              unitPage = { [7]='alpha' } })"
            , "assert(prep.ok, tostring(prep.errors and prep.errors[1]))"
            , "saveModules.applyAll()"
            , "assert(seen.unit7 == true, 'a deleted membership entry must "
              <> "still be present downstream, got ' .. tostring(seen.unit7))"
            , "assert(seen.unit9 == nil, 'an inserted membership entry must "
              <> "not reach downstream, got ' .. tostring(seen.unit9))"
            , "assert(seen.unit999 == nil, 'a rawset table must not reach "
              <> "downstream, got ' .. tostring(seen.unit999))"
            , "assert(seen.page7 == 'alpha', 'a rewritten owner page must "
              <> "not reach downstream, got ' .. tostring(seen.page7))"
            , "assert(seen.building1 == nil, 'a different kind is isolated "
              <> "too, got ' .. tostring(seen.building1))"
            , "assert(live[7] ~= nil and live[7].tag == 'seven',"
            , "  'the present-owner row must still be retained')"
            , "assert(live[9] == nil, 'the absent-owner row must still be "
              <> "dropped -- an earlier insert must not have rescued it')"
            , "assert(#warnings == 1, 'exactly one drop diagnostic, got '"
            , "  .. #warnings)"
            ]

        it "hands every component a table that shares NOTHING -- neither \
           \with Haskell's own table nor with any sibling's copy -- so \
           \there is no shared source left for a mutation to reach \
           \through, by any route" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local source = { unit = { [7]=true }, building = {},"
            , "                 unitPage = { [7]='alpha' } }"
            , "local got = {}"
            , "saveModules.register('idn_a', "
              <> mutatorSpec "got[#got + 1] = entities" <> ")"
            , "saveModules.register('idn_b', "
              <> mutatorSpec "got[#got + 1] = entities" <> ")"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='idn_a', version=1, payload=codec.encode({}) },"
            , "    { id='idn_b', version=1, payload=codec.encode({}) } },"
            , "  1, false, source)"
            , "assert(prep.ok)"
            , "saveModules.applyAll()"
            , "assert(#got == 2, 'both components must have applied')"
            , "assert(got[1] ~= got[2], 'each component needs its OWN copy')"
            , "assert(got[1] ~= source and got[2] ~= source,"
            , "  'no component may receive Haskell own table')"
            , "assert(got[1].unit ~= got[2].unit"
            , "  and got[1].unit ~= source.unit,"
            , "  'the NESTED maps must be copied too, not shared')"
            , "assert(got[1].unitPage ~= got[2].unitPage"
            , "  and got[1].unitPage ~= source.unitPage,"
            , "  'every nested map, not just the ones named unit')"
            , "-- The values themselves are still exactly Haskell answer."
            , "assert(got[2].unit[7] == true and got[2].unitPage[7] == 'alpha')"
            , "assert(source.unit[7] == true and source.unitPage[7] == 'alpha',"
            , "  'the source itself must come through untouched')"
            ]

        it "is unaffected by an earlier component reaching AROUND its own \
           \context to mutate the module's pending source directly, since \
           \the encoded snapshot every context is rebuilt from is taken -- \
           \and the raw source dropped -- before the first apply runs" $
            runsOk $ lns $
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            ] ⧺ captureWarnings ⧺
            [ "local live = {}"
            , "local seen = {}"
            -- _pendingEntities is a public field on the module table, so
            -- this is a route that needs no debug library and no reference
            -- captured from the value the component was handed.
            , "saveModules.register('src_a_mutator', "
              <> mutatorSpec (T.concat
                  [ "local sm = require('scripts.lib.save_modules');"
                  , "seen.pendingCleared = (sm._pendingEntities == nil);"
                  , "if sm._pendingEntities ~= nil then"
                  , "  sm._pendingEntities.unit[7] = nil;"
                  , "  sm._pendingEntities.unitPage[7] = 'HIJACKED' end;"
                  , "sm._pendingEntities = { unit = { [999]=true },"
                  , "  building = {}, unitPage = {} }"
                  ]) <> ")"
            , "saveModules.register('src_z_observer', { version=1, "
              <> "inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function(data, entities)"
            , "    seen.unit7 = entities.unit[7]"
            , "    seen.unit999 = entities.unit[999]"
            , "    seen.page7 = entities.unitPage[7]"
            , "    saveModules.applyEntityRows(live, data, entities,"
            , "      { kind='unit', component='src_z_observer' })"
            , "  end })"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='src_a_mutator', version=1, payload=codec.encode({}) },"
            , "    { id='src_z_observer', version=1,"
            , "      payload=codec.encode({ [7]={tag='seven'},"
            , "                             [9]={tag='nine'} }) } },"
            , "  1, false, { unit = { [7]=true }, building = {},"
            , "              unitPage = { [7]='alpha' } })"
            , "assert(prep.ok, tostring(prep.errors and prep.errors[1]))"
            , "saveModules.applyAll()"
            , "assert(seen.pendingCleared, 'the raw source must already be "
              <> "gone by the time any component runs')"
            , "assert(seen.unit7 == true, 'the later component must still "
              <> "see unit 7 present, got ' .. tostring(seen.unit7))"
            , "assert(seen.unit999 == nil, 'a wholesale replacement of the "
              <> "pending source must not reach it either')"
            , "assert(seen.page7 == 'alpha', 'the later component must see "
              <> "Haskell owner page, got ' .. tostring(seen.page7))"
            , "assert(live[7] ~= nil and live[7].tag == 'seven',"
            , "  'the present-owner row must still be retained')"
            , "assert(live[9] == nil, 'the absent-owner row must still be "
              <> "dropped')"
            ]

        it "keeps no sibling context alive to be found: an earlier \
           \component sweeping the applyAll frame with debug.getlocal \
           \reaches only its own table and an immutable encoded string, \
           \never a later component's context" $ runsOk $ lns $
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            ] ⧺ captureWarnings ⧺
            [ "local live = {}"
            , "local seen = { mutatedTables = 0, strings = 0 }"
            -- Walk every local of every active frame above this one,
            -- exactly as the reachability concern describes, and hijack
            -- anything shaped like an entity context. `mine` is this
            -- component's OWN context, which it is of course allowed to
            -- reach; any OTHER one would be a sibling's. Deliberately
            -- narrow: `prepared` and the rollback captures also live in
            -- that frame, and they are legitimately shared per-component
            -- payloads -- this issue is about the entity CONTEXT.
            , "saveModules.register('dbg_a_mutator', "
              <> mutatorSpec (T.concat
                  [ "local mine = entities;"
                  , "for level = 2, 12 do"
                  , "  local i = 1;"
                  , "  while true do"
                  , "    local ok, name, value = pcall(debug.getlocal, level, i);"
                  , "    if not ok or name == nil then break end;"
                  , "    if type(value) == 'table' and value ~= mine"
                  , "      and (rawget(value, 'unit') ~= nil"
                  , "           or rawget(value, 'unitPage') ~= nil) then"
                  , "      seen.mutatedTables = seen.mutatedTables + 1;"
                  , "      pcall(function()"
                  , "        if value.unit ~= nil then value.unit[7] = nil end;"
                  , "        if value.unitPage ~= nil then"
                  , "          value.unitPage[7] = 'HIJACKED' end"
                  , "      end) end;"
                  , "    if type(value) == 'string' then"
                  , "      seen.strings = seen.strings + 1 end;"
                  , "    i = i + 1 end end"
                  ]) <> ")"
            , "saveModules.register('dbg_z_observer', { version=1, "
              <> "inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function(data, entities)"
            , "    seen.unit7 = entities.unit[7]"
            , "    seen.page7 = entities.unitPage[7]"
            , "    saveModules.applyEntityRows(live, data, entities,"
            , "      { kind='unit', component='dbg_z_observer' })"
            , "  end })"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='dbg_a_mutator', version=1, payload=codec.encode({}) },"
            , "    { id='dbg_z_observer', version=1,"
            , "      payload=codec.encode({ [7]={tag='seven'},"
            , "                             [9]={tag='nine'} }) } },"
            , "  1, false, { unit = { [7]=true }, building = {},"
            , "              unitPage = { [7]='alpha' } })"
            , "assert(prep.ok, tostring(prep.errors and prep.errors[1]))"
            , "saveModules.applyAll()"
            , "assert(seen.unit7 == true, 'the later component must still "
              <> "see unit 7 present, got ' .. tostring(seen.unit7))"
            , "assert(seen.page7 == 'alpha', 'the later component must see "
              <> "Haskell owner page, got ' .. tostring(seen.page7))"
            , "assert(live[7] ~= nil and live[7].tag == 'seven',"
            , "  'the present-owner row must still be retained')"
            , "assert(live[9] == nil, 'the absent-owner row must still be "
              <> "dropped')"
            , "assert(seen.mutatedTables == 0, 'no sibling context may be "
              <> "reachable at all, found ' .. seen.mutatedTables)"
            -- The sweep has to have actually run, or this proves nothing.
            , "assert(seen.strings > 0, 'the sweep must have reached the "
              <> "applyAll frame -- the encoded context is a local string "
              <> "there; found none, so the walk never got that far')"
            ]

        it "is an ORDINARY table under every standard idiom -- next, pairs \
           \and getmetatable included -- so a component using raw iteration \
           \cannot silently conclude the restored session is empty" $
            runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local seen = {}"
            -- `next` is a primitive: it consults no metatable at all, so a
            -- proxy shell would report an EMPTY context here.
            , "saveModules.register('iter_probe', "
              <> mutatorSpec (T.concat
                  [ "seen.nextOuter = (next(entities) ~= nil);"
                  , "seen.nextUnit = (next(entities.unit) ~= nil);"
                  , "seen.rawUnit7 = rawget(entities.unit, 7);"
                  , "seen.meta = getmetatable(entities);"
                  , "seen.outer = 0;"
                  , "for _ in pairs(entities) do"
                  , "  seen.outer = seen.outer + 1 end;"
                  , "seen.units = {};"
                  , "for uid in next, entities.unit do"
                  , "  seen.units[#seen.units + 1] = uid end"
                  ]) <> ")"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='iter_probe', version=1, payload=codec.encode({}) } },"
            , "  1, false, { unit = { [7]=true }, building = {},"
            , "              unitPage = { [7]='alpha' } })"
            , "assert(prep.ok)"
            , "saveModules.applyAll()"
            , "assert(seen.nextOuter, 'next(entities) must see the context')"
            , "assert(seen.nextUnit, 'next(entities.unit) must see the "
              <> "restored membership -- a metatable-only view would not')"
            , "assert(seen.rawUnit7 == true, 'rawget must find real storage')"
            , "assert(seen.meta == nil, 'a plain table carries no metatable, "
              <> "so there is nothing for debug.getmetatable to reach into')"
            , "assert(seen.outer == 3, 'pairs() must yield every kind "
              <> "Haskell supplied, got ' .. tostring(seen.outer))"
            , "assert(#seen.units == 1 and seen.units[1] == 7,"
            , "  'raw iteration over a nested map must yield its membership')"
            ]

        -- The encoded snapshot rides data_codec, whose DEFAULT limits are
        -- PAYLOAD limits: MAX_TABLE_ENTRIES is 200,000 per table, and
        -- KnownEntities has no matching bound -- the context names every
        -- entity in the session while a component's payload carries only
        -- the rows it saved, so a session can exceed the cap with a tiny
        -- payload. This snapshot never reaches disk, so it passes
        -- UNBOUNDED; the same session must load, on the SAME immutable
        -- path as any other (no second, weaker mechanism for big worlds).
        it "applies a session whose entity set is past the codec's default \
           \per-table cap on the ordinary immutable path, keeping both the \
           \real membership and sibling isolation" $
            runsOk $ lns $
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            ] ⧺ captureWarnings ⧺
            [ "local live = {}"
            , "local seen = {}"
            , "local units = {}"
            , "for i = 1, codec.MAX_TABLE_ENTRIES + 1 do units[i] = true end"
            , "saveModules.register('big_a_mutator', "
              <> mutatorSpec (T.concat
                  [ "entities.unit[7] = nil;"
                  , "entities.unitPage[7] = 'HIJACKED'"
                  ]) <> ")"
            , "saveModules.register('big_z_observer', { version=1, "
              <> "inputVersions={1}, required=true, scope='global', deps={},"
            , "  snapshot=function() return {} end,"
            , "  decode=function(v,d) return d end,"
            , "  validate=function() return nil end,"
            , "  apply=function(data, entities)"
            , "    seen.unit7 = entities.unit[7]"
            , "    seen.page7 = entities.unitPage[7]"
            , "    seen.absent = entities.unit[999999]"
            , "    saveModules.applyEntityRows(live, data, entities,"
            , "      { kind='unit', component='big_z_observer' })"
            , "  end })"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='big_a_mutator', version=1, payload=codec.encode({}) },"
            , "    { id='big_z_observer', version=1,"
            , "      payload=codec.encode({ [7]={tag='seven'},"
            , "                             [999999]={tag='gone'} }) } },"
            , "  1, false, { unit = units, building = {},"
            , "              unitPage = { [7]='alpha' } })"
            , "assert(prep.ok, tostring(prep.errors and prep.errors[1]))"
            , "saveModules.applyAll()"
            , "assert(seen.unit7 == true, 'the later component must still "
              <> "see unit 7 present, got ' .. tostring(seen.unit7))"
            , "assert(seen.page7 == 'alpha', 'an earlier mutation must "
              <> "still not reach it, got ' .. tostring(seen.page7))"
            , "assert(seen.absent == nil, 'the oversized set is still the "
              <> "REAL membership, not an everything-passes stand-in')"
            , "assert(live[7] ~= nil and live[7].tag == 'seven',"
            , "  'the present-owner row must still be retained')"
            , "assert(live[999999] == nil, 'the absent-owner row must still "
              <> "be dropped -- filtering is not skipped in this path')"
            -- The DEFAULT limits must be untouched by the override: this
            -- same context is still refused for anything disk-bound.
            , "assert(codec.encode({ unit = units }) == nil,"
            , "  'the payload default cap must still reject this context "
              <> "-- the allowance is per call, not a global relaxation')"
            , "assert(#warnings == 1, 'the dropped row must still report "
              <> "its diagnostic, got ' .. #warnings)"
            ]

        it "leaves the two documented compatibility semantics untouched: a \
           \nil context is still 'apply every row' and an EMPTY per-kind \
           \set still filters" $ runsOk $ lns
            [ "local saveModules = require('scripts.lib.save_modules')"
            , "local codec = require('scripts.lib.data_codec')"
            , "local liveNil, liveEmpty = {}, {}"
            , "saveModules.register('compat_nil', "
              <> perEntitySpec "compat_nil" "liveNil" <> ")"
            , "local prep = saveModules.prepareLoad("
            , "  { { id='compat_nil', version=1,"
            , "      payload=codec.encode({ [7]={tag='seven'} }) } },"
            , "  1, false, nil)"
            , "assert(prep.ok)"
            , "saveModules.applyAll()"
            , "assert(liveNil[7] ~= nil, 'a nil context must not filter')"
            , "saveModules.register('compat_empty', "
              <> perEntitySpec "compat_empty" "liveEmpty" <> ")"
            , "prep = saveModules.prepareLoad("
            , "  { { id='compat_nil', version=1, payload=codec.encode({}) },"
            , "    { id='compat_empty', version=1,"
            , "      payload=codec.encode({ [7]={tag='seven'} }) } },"
            , "  2, false, { unit = {}, building = {} })"
            , "assert(prep.ok)"
            , "saveModules.applyAll()"
            , "assert(liveEmpty[7] == nil, 'an empty per-kind set is a real "
              <> "answer and must still filter')"
            ]
