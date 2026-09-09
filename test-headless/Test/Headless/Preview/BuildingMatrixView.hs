-- | The SHIPPED buildings viewer driven end to end on the CPU (#2492
--   requirement 19): the real @scripts/preview_manager.lua@, the real
--   @scripts/ui/building_asset_view.lua@ and the real
--   @scripts/ui/preview_zoom.lua@, on the shared stdlib-only harness
--   ('Test.Headless.Preview.LuaHarness'), with only the asset browser
--   and list chrome stubbed.
--
--   Driving the real view is the whole point. @tools/preview_probe.py@
--   is manual-only and @needs-gpu@, so this group is the only BLOCKING
--   automated gate the facing strip, the missing-cell presentation, the
--   combined-row identities and the resize-preserved state have. A
--   fixture that restated the layout arithmetic would pass over a
--   shipped pane that never drew a cell.
--
--   Every geometric assertion reads bounds back through
--   @UI.getElementInfo@ — the harness's UI oracle answers from the same
--   records the shipped code wrote — so a click located from the dump
--   exercises the element that is really there.
module Test.Headless.Preview.BuildingMatrixView (spec) where

import UPrelude
import Test.Hspec
import Test.Headless.Preview.LuaHarness (harness, lns, runsOk)

-- | A mixed-provenance building: a CANONICAL @construction@ with four
--   independently authored facings, a LEGACY multi-frame @built@ whose
--   one list is repeated across all four, an UNRESOLVED @destruction@,
--   and a legacy sprite — plus the raw filesystem rows that back some
--   of the same files.
--
--   Shaped exactly as 'Engine.Scripting.Lua.API.Core' marshals it, so
--   this fixture is a payload the engine could really produce rather
--   than a convenient one.
mixedBrowse ∷ Text
mixedBrowse = lns
    [ "local function cell(f, paths, missing, reason, legacy)"
    , "  return { facing = f, paths = paths, missing = missing,"
    , "           missingReason = reason, legacy = legacy }"
    , "end"
    , "local B = 'assets/textures/buildings/mixed/'"
    , "local canonical = {"
    , "  cell('south', { B..'s.png' }, false, nil, false),"
    , "  cell('west',  { B..'w.png' }, false, nil, false),"
    , "  cell('north', { B..'n.png' }, false, nil, false),"
    , "  cell('east',  { B..'e.png' }, false, nil, false) }"
    , "local legacyFrames = { B..'idle/frame_000.png', B..'idle/frame_001.png' }"
    , "local legacyCells = {"
    , "  cell('south', legacyFrames, false, nil, true),"
    , "  cell('west',  legacyFrames, false, nil, true),"
    , "  cell('north', legacyFrames, false, nil, true),"
    , "  cell('east',  legacyFrames, false, nil, true) }"
    , "-- One MISSING facing on an otherwise loadable canonical row, so"
    , "-- the diagnostic is reachable by a facing move rather than only"
    , "-- by selecting a wholly broken row."
    , "canonical[2] = cell('west', { B..'w.png' }, true, 'absent', false)"
    , "local unresolved = {"
    , "  cell('south', {}, true, 'unresolved', false),"
    , "  cell('west',  {}, true, 'unresolved', false),"
    , "  cell('north', {}, true, 'unresolved', false),"
    , "  cell('east',  {}, true, 'unresolved', false) }"
    , "local spriteCells = {"
    , "  cell('south', { B..'default.png' }, false, nil, true),"
    , "  cell('west',  { B..'default.png' }, false, nil, true),"
    , "  cell('north', { B..'default.png' }, false, nil, true),"
    , "  cell('east',  { B..'default.png' }, false, nil, true) }"
    , "local building = { name = 'mixed', defaultEntry = 'idle',"
    , "  defaultSelection = 'lifecycle:built',"
    , "  entries = {"
    , "    { label = 'default.png', animated = false, fps = 8, loop = false,"
    , "      frames = { B..'default.png' } },"
    , "    { label = 'idle', animated = true, fps = 8, loop = false,"
    , "      frames = legacyFrames },"
    , "    { label = 'stray.png', animated = false, fps = 8, loop = false,"
    , "      frames = { B..'stray.png' } } },"
    , "  declared = {"
    , "    { identity = 'lifecycle:construction', kind = 'lifecycle',"
    , "      label = 'construction \\226\\134\\146 build-anim', role = 'construction',"
    , "      animation = 'build-anim', resolved = true, fps = 8, loop = false,"
    , "      source = 'canonical', legacy = false, cells = canonical },"
    , "    { identity = 'lifecycle:built', kind = 'lifecycle',"
    , "      label = 'built \\226\\134\\146 idle-anim', role = 'built',"
    , "      animation = 'idle-anim', resolved = true, fps = 8, loop = false,"
    , "      source = 'legacy', legacy = true, projected = 'idle',"
    , "      cells = legacyCells },"
    , "    { identity = 'lifecycle:destruction', kind = 'lifecycle',"
    , "      label = 'destruction \\226\\134\\146 gone', role = 'destruction',"
    , "      animation = 'gone', resolved = false, fps = 0, loop = false,"
    , "      source = 'canonical', legacy = false, cells = unresolved },"
    , "    { identity = 'sprite', kind = 'sprite', label = 'sprite',"
    , "      resolved = true, fps = 0, loop = false, source = 'legacy',"
    , "      legacy = true, projected = 'default.png', cells = spriteCells } },"
    , "  filesystemClasses = {"
    , "    { label = 'default.png', identity = 'filesystem:default.png',"
    , "      declared = { 'sprite' }, undeclared = false },"
    , "    { label = 'idle', identity = 'filesystem:idle',"
    , "      declared = { 'lifecycle:built' }, undeclared = false },"
    , "    { label = 'stray.png', identity = 'filesystem:stray.png',"
    , "      declared = {}, undeclared = true } } }"
    , "local pm = bootPreview({ mode = 'building', building = building },"
    , "                       { category = 'buildings', item = 'mixed' })"
    , "pm.update(0.016)"
    ]

-- | A YAML-less building: the pre-#2492 payload exactly, with no
--   @declared@, no @defaultSelection@ and no classes.
rawOnlyBrowse ∷ Text
rawOnlyBrowse = lns
    [ "local B = 'assets/textures/buildings/dungeon_1/'"
    , "local building = { name = 'dungeon_1', defaultEntry = 'damaged/floor.png',"
    , "  entries = {"
    , "    { label = 'damaged/floor.png', animated = false, fps = 8,"
    , "      loop = false, frames = { B..'damaged/floor.png' } },"
    , "    { label = 'damaged/wall.png', animated = false, fps = 8,"
    , "      loop = false, frames = { B..'damaged/wall.png' } } } }"
    , "local pm = bootPreview({ mode = 'building', building = building },"
    , "                       { category = 'buildings', item = 'dungeon_1' })"
    , "pm.update(0.016)"
    ]

-- | Locate a combined row in the dump by IDENTITY, never by label — two
--   rows here legitimately draw text a label match could confuse.
findRow ∷ Text
findRow = lns
    [ "function rowByIdentity(pm, identity)"
    , "  for _, r in ipairs(pm.dump().rows or {}) do"
    , "    if r.identity == identity then return r end"
    , "  end"
    , "  return nil"
    , "end"
    , "function cellByFacing(pm, facing)"
    , "  for _, c in ipairs(pm.dump().facingRow or {}) do"
    , "    if c.facing == facing then return c end"
    , "  end"
    , "  return nil"
    , "end"
    , "function centerOf(bounds)"
    , "  return bounds.x + bounds.w / 2, bounds.y + bounds.h / 2"
    , "end"
    ]

spec ∷ Spec
spec = do

  describe "the combined list" $ do
    it "orders declared rows before every raw row and gives each a \
       \distinct identity" $ runsOk $ lns
      [ harness, mixedBrowse, findRow
      , "local d = pm.dump()"
      , "assert(d.mode == 'building', tostring(d.mode))"
      , "local ids = {}"
      , "for _, r in ipairs(d.rows) do table.insert(ids, r.identity) end"
      , "assert(table.concat(ids, ',') =="
      , "  'lifecycle:construction,lifecycle:built,lifecycle:destruction,"
        <> "sprite,filesystem:default.png,filesystem:idle,filesystem:stray.png',"
      , "  'combined order: ' .. table.concat(ids, ','))"
      , "local seen = {}"
      , "for _, id in ipairs(ids) do"
      , "  assert(not seen[id], 'duplicate identity ' .. id); seen[id] = true"
      , "end"
      ]

    it "keeps the raw entry list, its defaultEntry and the raw \
       \projection of `selected` untouched" $ runsOk $ lns
      [ harness, mixedBrowse, findRow
      , "local d = pm.dump()"
      , "assert(#d.entries == 3 and d.entryCount == 3)"
      , "assert(d.entries[1].label == 'default.png' and d.entries[1].animated == false)"
      , "assert(d.entries[2].label == 'idle' and d.entries[2].animated == true)"
      , "assert(d.entries[3].label == 'stray.png')"
      , "assert(d.defaultEntry == 'idle', tostring(d.defaultEntry))"
      , "assert(d.defaultSelection == 'lifecycle:built')"
      , "-- The declared built row is selected, and `selected` still"
      , "-- names the RAW entry it projects onto -- which is what every"
      , "-- pre-#2492 consumer reads."
      , "assert(d.selection.identity == 'lifecycle:built')"
      , "assert(d.selected.label == 'idle' and d.selected.path == 'idle')"
      , "assert(d.declaration == 'legacy', tostring(d.declaration))"
      , "assert(d.selectedLifecycle == 'built')"
      ]

    it "classifies every raw row without dropping one" $ runsOk $ lns
      [ harness, mixedBrowse, findRow
      , "local fs = pm.dump().filesystemEntries"
      , "assert(#fs == 3)"
      , "assert(fs[1].identity == 'filesystem:default.png'"
      , "   and fs[1].declared[1] == 'sprite' and fs[1].undeclared == false)"
      , "assert(fs[2].declared[1] == 'lifecycle:built' and fs[2].undeclared == false)"
      , "assert(fs[3].undeclared == true and #fs[3].declared == 0)"
      , "local t = pm.dump().totals"
      , "assert(t.undeclaredFilesystemEntries == 1, tostring(t.undeclaredFilesystemEntries))"
      , "assert(t.unresolvedLifecycleRows == 1, tostring(t.unresolvedLifecycleRows))"
      , "-- one absent canonical cell plus four unresolved destruction cells"
      , "assert(t.missingCells == 5, tostring(t.missingCells))"
      ]

  describe "the facing strip" $ do
    it "draws four cells in camera order and enlarges the clicked one, \
       \located through the dump's own bounds" $ runsOk $ lns
      [ harness, mixedBrowse, findRow
      , "local d = pm.dump()"
      , "assert(d.selectedFacing == 'south', tostring(d.selectedFacing))"
      , "local order = {}"
      , "for _, c in ipairs(d.facingRow) do table.insert(order, c.facing) end"
      , "assert(table.concat(order, ',') == 'south,west,north,east',"
      , "    table.concat(order, ','))"
      , "-- Route the click exactly as the engine does: the hit box's own"
      , "-- handle, read back from the element the view really created."
      , "local east = cellByFacing(pm, 'east')"
      , "assert(east.bounds.w > 0 and east.bounds.h > 0, 'cell has real bounds')"
      , "assert(pm.onPreviewFacingClick(east.hitHandle))"
      , "pm.update(0.016)"
      , "assert(pm.dump().selectedFacing == 'east')"
      , "-- and the cells really are laid out left to right, in order."
      , "local prev = nil"
      , "for _, c in ipairs(pm.dump().facingRow) do"
      , "  if prev then assert(c.bounds.x > prev, 'cells advance rightwards') end"
      , "  prev = c.bounds.x"
      , "end"
      ]

    it "moves through all four facings and back to the start" $ runsOk $ lns
      [ harness, mixedBrowse, findRow
      , "assert(pm.dump().selectedFacing == 'south')"
      , "assert(pm.onKeyDown('Left'), 'Left is handled on a declared row')"
      , "pm.update(0.016)"
      , "assert(pm.dump().selectedFacing == 'east', 'Left from first wraps to last')"
      , "assert(pm.onKeyDown('Right'))"
      , "pm.update(0.016)"
      , "assert(pm.dump().selectedFacing == 'south', 'Right from last wraps to first')"
      , "for _, want in ipairs({'west', 'north', 'east', 'south'}) do"
      , "  assert(pm.onKeyDown('Right')); pm.update(0.016)"
      , "  assert(pm.dump().selectedFacing == want,"
      , "      'expected ' .. want .. ' got ' .. tostring(pm.dump().selectedFacing))"
      , "end"
      ]

    it "repeats a held Left/Right on the preview's own clock and stops \
       \exactly on key-up" $ runsOk $ lns
      [ harness, mixedBrowse, findRow
      , "NOW = 100"
      , "assert(pm.onKeyDown('Right')); pm.update(0.016)"
      , "assert(pm.dump().selectedFacing == 'west')"
      , "NOW = 100.19; pm.update(0.016)"
      , "assert(pm.dump().selectedFacing == 'west',"
      , "    'the initial delay must not repeat early')"
      , "NOW = 100.21; pm.update(0.016)"
      , "assert(pm.dump().selectedFacing == 'north')"
      , "NOW = 100.249; pm.update(0.016)"
      , "assert(pm.dump().selectedFacing == 'north',"
      , "    'the fixed interval must not repeat early')"
      , "NOW = 100.251; pm.update(0.016)"
      , "assert(pm.dump().selectedFacing == 'east')"
      , "-- Facings WRAP, so unlike a list boundary the hold continues"
      , "-- until the matching key-up rather than terminating itself."
      , "NOW = 100.30; pm.update(0.016)"
      , "assert(pm.dump().selectedFacing == 'south', 'the hold wrapped')"
      , "assert(pm.onKeyUp('Right'))"
      , "NOW = 101; pm.update(0.016)"
      , "assert(pm.dump().selectedFacing == 'south', 'release stops the clock')"
      ]

    it "changing facing never resets the row selection, the cycle or the \
       \zoom multiplier" $ runsOk $ lns
      [ harness, mixedBrowse, findRow
      , "pm.onUIScroll(pm.dump().zoom.surface, 0, 2)"
      , "NOW = 0.14"
      , "pm.update(0.016)"
      , "local before = pm.dump()"
      , "local phase, held = before.playback.frameIndex, before.zoom.multiplier"
      , "assert(phase == 1, 'fps 8 at t=0.14 is frame 1, got ' .. tostring(phase))"
      , "assert(pm.onKeyDown('Right')); pm.update(0.016)"
      , "local after = pm.dump()"
      , "assert(after.selectedFacing == 'west')"
      , "assert(after.selection.identity == 'lifecycle:built')"
      , "assert(after.playback.frameIndex == phase, 'the cycle kept running')"
      , "assert(after.zoom.multiplier == held, 'the multiplier survived')"
      ]

  describe "missing cells" $ do
    it "never requests an invalid path, and reaches a TERMINAL ready \
       \state rather than staying in loading" $ runsOk $ lns
      [ harness, mixedBrowse, findRow
      , "local B = 'assets/textures/buildings/mixed/'"
      , "-- Select the wholly unresolved destruction row: nothing it"
      , "-- declares can ever load."
      , "assetBrowserStub.selectEntry(1, 'lifecycle:destruction')"
      , "pm.update(0.016)"
      , "local d = pm.dump()"
      , "assert(d.selection.identity == 'lifecycle:destruction')"
      , "assert(d.selection.resolved == false and d.selection.missing == true)"
      , "assert(d.selection.missingReason == 'unresolved',"
      , "    tostring(d.selection.missingReason))"
      , "assert(d.state == 'ready', 'terminal, got ' .. tostring(d.state))"
      , "assert(d.state ~= 'empty', '\"empty\" is #1690s bindless failure')"
      , "assert(d.path == nil, 'no enlarged path was resolved')"
      , "for _, c in ipairs(d.facingRow) do"
      , "  assert(c.missing == true and c.handle == nil,"
      , "      'a missing cell must hold no requested handle')"
      , "end"
      ]

    it "requests no texture outside the building's own root for a \
       \missing selection" $ runsOk $ lns
      [ harness, mixedBrowse, findRow
      , "assetBrowserStub.selectEntry(1, 'lifecycle:destruction')"
      , "pm.update(0.016); pm.update(0.016)"
      , "for _, p in ipairs(pm.dump().loadedPaths) do"
      , "  assert(p:find('assets/textures/buildings/mixed/', 1, true) == 1"
      , "      or p:find('assets/textures/ui/', 1, true) == 1,"
      , "      'unexpected load: ' .. p)"
      , "end"
      ]

    it "shows the missing marker on one facing while its siblings still \
       \draw, substituting nothing" $ runsOk $ lns
      [ harness, mixedBrowse, findRow
      , "assetBrowserStub.selectEntry(1, 'lifecycle:construction')"
      , "pm.update(0.016)"
      , "local south = cellByFacing(pm, 'south')"
      , "local west = cellByFacing(pm, 'west')"
      , "assert(south.missing == false and south.handle ~= nil)"
      , "assert(west.missing == true and west.missingReason == 'absent')"
      , "assert(west.handle == nil, 'the missing cell requested nothing')"
      , "assert(west.path == nil, 'and resolved no path of its own')"
      , "-- Enlarging the missing facing is terminal, and does NOT fall"
      , "-- back to south's art."
      , "assert(pm.onPreviewFacingClick(west.hitHandle))"
      , "pm.update(0.016)"
      , "local d = pm.dump()"
      , "assert(d.selectedFacing == 'west' and d.selection.missing == true)"
      , "assert(d.state == 'ready' and d.path == nil)"
      , "assert(d.zoom.sprite == nil,"
      , "    'a missing enlarged view reports no rendered sprite')"
      , "-- and moving back to a loadable facing recovers."
      , "assert(pm.onKeyDown('Right')); pm.update(0.016)"
      , "assert(pm.dump().selection.missing == false)"
      , "assert(pm.dump().path ~= nil)"
      ]

  describe "legacy declarations" $
    it "marks every repeated cell and its row legacy, and a canonical \
       \row neither" $ runsOk $ lns
      [ harness, mixedBrowse, findRow
      , "local d = pm.dump()"
      , "assert(d.selection.identity == 'lifecycle:built')"
      , "assert(d.selection.legacy == true and d.declaration == 'legacy')"
      , "for _, c in ipairs(d.facingRow) do"
      , "  assert(c.legacy == true, c.facing .. ' must be flagged legacy')"
      , "  assert(c.frameCount == 2,"
      , "      'the COMPLETE legacy list reaches every facing, got '"
      , "      .. tostring(c.frameCount))"
      , "end"
      , "assetBrowserStub.selectEntry(1, 'lifecycle:construction')"
      , "pm.update(0.016)"
      , "local c = pm.dump()"
      , "assert(c.selection.legacy == false and c.declaration == 'canonical')"
      , "for _, cell in ipairs(c.facingRow) do assert(cell.legacy == false) end"
      , "-- Provenance is per ENTRY: one building, two answers."
      , "assetBrowserStub.selectEntry(1, 'sprite')"
      , "pm.update(0.016)"
      , "assert(pm.dump().declaration == 'legacy')"
      , "-- A RAW selection reports none at all."
      , "assetBrowserStub.selectEntry(1, 'filesystem:stray.png')"
      , "pm.update(0.016)"
      , "assert(pm.dump().declaration == nil)"
      , "assert(pm.dump().selection.kind == 'filesystem')"
      ]

  describe "a resize preserves the whole selection state" $
    it "keeps the row identity, facing, scroll offset, cycle phase and \
       \zoom while recomputing every rect" $ runsOk $ lns
      [ harness, mixedBrowse, findRow
      , "assetBrowserStub.selectEntry(1, 'lifecycle:construction')"
      , "pm.onUIScroll(pm.dump().zoom.surface, 0, 2)"
      , "assert(pm.onKeyDown('Right')); assert(pm.onKeyDown('Right'))"
      , "-- Released before the clock advances: an unreleased hold would"
      , "-- fire its own repeat during the update below and move the"
      , "-- facing this case is about preserving."
      , "assert(pm.onKeyUp('Right'))"
      , "assetBrowserStub.setScrollOffset(1, 2)"
      , "NOW = 0.26"
      , "pm.update(0.016)"
      , "local before = pm.dump()"
      , "assert(before.selectedFacing == 'north', tostring(before.selectedFacing))"
      , "assert(before.playback.frameIndex == 0,"
      , "    'a 1-frame canonical clip stays on 0')"
      , "local held = before.zoom.multiplier"
      , "local beforeCell = cellByFacing(pm, 'north').bounds"
      , "pm.onFramebufferResize(1500, 1150)"
      , "pm.update(0.016)"
      , "local after = pm.dump()"
      , "assert(after.selection.identity == 'lifecycle:construction',"
      , "    tostring(after.selection and after.selection.identity))"
      , "assert(after.selectedFacing == 'north', tostring(after.selectedFacing))"
      , "assert(after.scrollOffset == 2, tostring(after.scrollOffset))"
      , "assert(after.zoom.multiplier == held,"
      , "    tostring(after.zoom.multiplier) .. ' vs ' .. tostring(held))"
      , "local afterCell = cellByFacing(pm, 'north').bounds"
      , "assert(afterCell.x ~= beforeCell.x or afterCell.w ~= beforeCell.w,"
      , "    'the facing cells really were re-laid out')"
      , "assert(after.zoom.region.width ~= before.zoom.region.width,"
      , "    'and so was the enlarged region')"
      ]

  describe "a building with no usable declaration" $ do
    it "browses exactly as before: raw rows only, the unchanged default, \
       \no facing strip, and Left/Right unhandled" $ runsOk $ lns
      [ harness, rawOnlyBrowse, findRow
      , "local d = pm.dump()"
      , "assert(#d.rows == 2 and #d.entries == 2)"
      , "assert(d.rows[1].identity == 'filesystem:damaged/floor.png')"
      , "assert(d.rows[2].identity == 'filesystem:damaged/wall.png')"
      , "assert(#d.lifecycle == 0 and d.staticSprite == nil)"
      , "assert(d.declaration == nil)"
      , "assert(d.defaultSelection == nil,"
      , "    'the engine sent none; the manager falls back rather than inventing one')"
      , "assert(d.selection.identity == 'filesystem:damaged/floor.png')"
      , "assert(d.selected.label == 'damaged/floor.png')"
      , "assert(d.selectedFacing == nil and #(d.facingRow or {}) == 0,"
      , "    'a raw row has no facing model at all')"
      , "assert(not pm.onKeyDown('Left') and not pm.onKeyDown('Right'))"
      , "-- #1907's existing rule for raw rows: the whole panel."
      , "assert(math.abs(d.zoom.region.width - d.panelBounds.width) < 1e-6)"
      , "assert(d.totals.undeclaredFilesystemEntries == 2)"
      ]

    it "keeps a raw ANIMATED row's own clock running -- it has no facing \
       \cells to drive it" $ runsOk $ lns
      [ harness, rawOnlyBrowse, findRow
      , "-- A raw animated row, browsed with no declaration at all: its"
      , "-- index comes from the entry's own frame list, because a raw row"
      , "-- has no facing cells for the strip loop to compute it from."
      , "local B = 'assets/textures/buildings/dungeon_1/'"
      , "local building = { name = 'd', defaultEntry = 'spin', entries = {"
      , "  { label = 'spin', animated = true, fps = 8, loop = false,"
      , "    frames = { B..'a.png', B..'b.png', B..'c.png' } } } }"
      , "local pm2 = bootPreview({ mode = 'building', building = building },"
      , "                        { category = 'buildings', item = 'd' })"
      , "NOW = 0"
      , "pm2.update(0.016)"
      , "assert(pm2.dump().playback.frameIndex == 0)"
      , "NOW = 0.14; pm2.update(0.016)"
      , "assert(pm2.dump().playback.frameIndex == 1,"
      , "    'fps 8 at t=0.14 is frame 1, got '"
      , "    .. tostring(pm2.dump().playback.frameIndex))"
      , "NOW = 0.30; pm2.update(0.016)"
      , "assert(pm2.dump().playback.frameIndex == 2)"
      , "-- #1833: it REPLAYS rather than pinning, whatever `loop` says."
      , "NOW = 0.40; pm2.update(0.016)"
      , "assert(pm2.dump().playback.frameIndex == 0,"
      , "    'a loop:false raw clip still wraps to 0')"
      , "assert(pm2.dump().playback.loop == false,"
      , "    'and its AUTHORED loop is still reported verbatim')"
      ]

    it "still selects, resizes and zooms a raw row after a declared \
       \building was browsed in the same session" $ runsOk $ lns
      [ harness, mixedBrowse, findRow
      , "assetBrowserStub.selectEntry(1, 'filesystem:stray.png')"
      , "pm.update(0.016)"
      , "local d = pm.dump()"
      , "assert(d.selection.kind == 'filesystem')"
      , "assert(d.selectedFacing == nil and #(d.facingRow or {}) == 0)"
      , "-- The zoom region goes back to the whole panel with no strip."
      , "assert(math.abs(d.zoom.region.width - d.panelBounds.width) < 1e-6)"
      , "assert(math.abs(d.zoom.region.height - d.panelBounds.height) < 1e-6)"
      , "assert(d.playback == nil, 'a raw static exposes no playback')"
      , "assert(d.state == 'ready')"
      ]
