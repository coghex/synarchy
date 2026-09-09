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
    -- Four DISTINCT paths per facing, so an enlarged view that failed
    -- to re-point at the newly selected facing's art would keep showing
    -- the previous texture and be caught rather than looking identical.
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
    , "-- A declared row whose paths point OUTSIDE the building's folder."
    , "-- They are real strings, so a view that requested a missing"
    , "-- cell's path anyway would load another tree's texture and break"
    , "-- trimmed loading -- which an all-empty fixture could not show."
    , "local outside = {"
    , "  cell('south', { 'assets/textures/buildings/other/s.png' }, true,"
    , "       'outside_root', false),"
    , "  cell('west',  { 'assets/textures/buildings/other/w.png' }, true,"
    , "       'outside_root', false),"
    , "  cell('north', { 'assets/textures/buildings/other/n.png' }, true,"
    , "       'outside_root', false),"
    , "  cell('east',  { 'assets/textures/buildings/other/e.png' }, true,"
    , "       'outside_root', false) }"
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
    , "    { identity = 'lifecycle:appearance', kind = 'lifecycle',"
    , "      label = 'appearance \226\134\146 out-anim', role = 'appearance',"
    , "      animation = 'out-anim', resolved = true, fps = 8, loop = false,"
    , "      source = 'canonical', legacy = false, cells = outside },"
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

-- | A building whose initial selection is WHOLLY diagnostic: the
--   declared @built@ row is unresolved, so a fresh session requests no
--   building texture at all before the first wheel event.
--
--   Requirement 13 makes this reachable on purpose — a declared @built@
--   row stays selectable as a diagnostic — and #1907 still requires the
--   preview region to own a scroll-capturing surface. The two together
--   are the case a viewer that borrowed its surface handle from the
--   first frame it happened to load could not serve.
diagnosticOnlyBrowse ∷ Text
diagnosticOnlyBrowse = lns
    [ "local function cell(f)"
    , "  return { facing = f, paths = {}, missing = true,"
    , "           missingReason = 'unresolved', legacy = false }"
    , "end"
    , "local cells = { cell('south'), cell('west'), cell('north'), cell('east') }"
    , "local B = 'assets/textures/buildings/broken/'"
    , "local building = { name = 'broken', defaultEntry = 'default.png',"
    , "  defaultSelection = 'lifecycle:built',"
    , "  entries = {"
    , "    { label = 'default.png', animated = false, fps = 8, loop = false,"
    , "      frames = { B..'default.png' } } },"
    , "  declared = {"
    , "    { identity = 'lifecycle:built', kind = 'lifecycle',"
    , "      label = 'built \226\134\146 gone', role = 'built',"
    , "      animation = 'gone', resolved = false, fps = 0, loop = false,"
    , "      source = 'canonical', legacy = false, cells = cells } },"
    , "  filesystemClasses = {"
    , "    { label = 'default.png', identity = 'filesystem:default.png',"
    , "      declared = {}, undeclared = true } } }"
    , "local pm = bootPreview({ mode = 'building', building = building },"
    , "                       { category = 'buildings', item = 'broken' })"
    , "pm.update(0.016)"
    ]

-- | The four invalid-cell kinds @mixedBrowse@ does not carry, on the
--   initial @built@ row so every one is reachable by a facing move.
--
--   Requirement 19 names absent, directory, symlink, unsupported
--   extension and special-file cells explicitly, and it names them for
--   the REAL-view gate: the Haskell classifier decides those verdicts,
--   but only here do they have to survive marshalling and reach the
--   presentation as distinct, never-requested, non-loading diagnostics.
invalidCellsBrowse ∷ Text
invalidCellsBrowse = lns
    [ "local function cell(f, path, reason)"
    , "  return { facing = f, paths = { path }, missing = true,"
    , "           missingReason = reason, legacy = false }"
    , "end"
    , "local B = 'assets/textures/buildings/invalid/'"
    , "local cells = {"
    , "  cell('south', B..'a_dir.png',  'directory'),"
    , "  cell('west',  B..'a_link.png', 'symlink'),"
    , "  cell('north', B..'a.txt',      'unsupported_extension'),"
    , "  cell('east',  B..'a_fifo.png', 'special') }"
    , "local building = { name = 'invalid', defaultEntry = 'default.png',"
    , "  defaultSelection = 'lifecycle:built',"
    , "  entries = {"
    , "    { label = 'default.png', animated = false, fps = 8, loop = false,"
    , "      frames = { B..'default.png' } } },"
    , "  declared = {"
    , "    { identity = 'lifecycle:built', kind = 'lifecycle',"
    , "      label = 'built \226\134\146 broken', role = 'built',"
    , "      animation = 'broken', resolved = true, fps = 8, loop = false,"
    , "      source = 'canonical', legacy = false, cells = cells } },"
    , "  filesystemClasses = {"
    , "    { label = 'default.png', identity = 'filesystem:default.png',"
    , "      declared = {}, undeclared = true } } }"
    , "local pm = bootPreview({ mode = 'building', building = building },"
    , "                       { category = 'buildings', item = 'invalid' })"
    , "pm.update(0.016)"
    ]

-- | A building whose YAML exists but is MALFORMED (or matches no
--   definition, or names an unknown lifecycle key): the engine yields
--   no usable matrix, so it marshals an EMPTY @declared@ list and no
--   @defaultSelection@ — distinct on the wire from the YAML-less
--   payload below, and required to behave identically.
malformedYamlBrowse ∷ Text
malformedYamlBrowse = lns
    [ "local B = 'assets/textures/buildings/malformed/'"
    , "local building = { name = 'malformed', defaultEntry = 'idle',"
    , "  declared = {}, filesystemClasses = {"
    , "    { label = 'idle', identity = 'filesystem:idle',"
    , "      declared = {}, undeclared = true },"
    , "    { label = 'default.png', identity = 'filesystem:default.png',"
    , "      declared = {}, undeclared = true } },"
    , "  entries = {"
    , "    { label = 'idle', animated = true, fps = 8, loop = false,"
    , "      frames = { B..'idle/frame_000.png', B..'idle/frame_001.png' } },"
    , "    { label = 'default.png', animated = false, fps = 8, loop = false,"
    , "      frames = { B..'default.png' } } } }"
    , "local pm = bootPreview({ mode = 'building', building = building },"
    , "                       { category = 'buildings', item = 'malformed' })"
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
    -- The UI oracle answers from the records the shipped code wrote, so
    -- these read back a real write rather than restating an expectation.
    , "function elemOf(handle)"
    , "  assert(handle, 'the dump reported no element handle')"
    , "  local e = elements[handle]"
    , "  assert(e, 'element ' .. tostring(handle) .. ' does not exist')"
    , "  return e"
    , "end"
    , "function shown(handle) return elemOf(handle).visible == true end"
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
      , "  'lifecycle:construction,lifecycle:appearance,lifecycle:built,"
        <> "lifecycle:destruction,sprite,filesystem:default.png,"
        <> "filesystem:idle,filesystem:stray.png',"
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
      , "-- one absent canonical cell, four out-of-root and four unresolved"
      , "assert(t.missingCells == 9, tostring(t.missingCells))"
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
      , "-- #1907 requirement 3: the enlarged sprite must not overlap the"
      , "-- strip, so a DECLARED row's zoom region is the sub-rect ABOVE"
      , "-- it -- shorter than the panel, and clear of every cell."
      , "local d2 = pm.dump()"
      , "assert(d2.zoom.region.height < d2.panelBounds.height,"
      , "    'the declared region excludes the strip: '"
      , "    .. tostring(d2.zoom.region.height) .. ' vs panel '"
      , "    .. tostring(d2.panelBounds.height))"
      , "local regionBottom = d2.zoom.region.y + d2.zoom.region.height"
      , "for _, c in ipairs(d2.facingRow) do"
      , "  assert(c.bounds.y >= regionBottom,"
      , "      c.facing .. ' cell overlaps the enlarged region')"
      , "end"
      , "assertContained('building enlarged', d2.zoom.sprite, d2.zoom.region)"
      , "assertCentered('building enlarged', d2.zoom.sprite, d2.zoom.region)"
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

    it "carries the enlarged facing across a row change, and clears it \
       \for a row with no facing model" $ runsOk $ lns
      [ harness, mixedBrowse, findRow
      , "assert(pm.onKeyDown('Right')); assert(pm.onKeyUp('Right'))"
      , "assert(pm.onKeyDown('Right')); assert(pm.onKeyUp('Right'))"
      , "pm.update(0.016)"
      , "assert(pm.dump().selectedFacing == 'north',"
      , "    tostring(pm.dump().selectedFacing))"
      , "-- Another DECLARED row keeps the reviewer on the same view,"
      , "-- which is the whole point of comparing two roles side by side."
      , "assetBrowserStub.selectEntry(1, 'lifecycle:construction')"
      , "pm.update(0.016)"
      , "assert(pm.dump().selection.identity == 'lifecycle:construction')"
      , "assert(pm.dump().selectedFacing == 'north',"
      , "    'a row change must not snap back to south, got '"
      , "    .. tostring(pm.dump().selectedFacing))"
      , "-- A RAW row has no facing model, so it clears the facing"
      , "-- entirely rather than reporting a stale one."
      , "assetBrowserStub.selectEntry(1, 'filesystem:idle')"
      , "pm.update(0.016)"
      , "assert(pm.dump().selectedFacing == nil,"
      , "    tostring(pm.dump().selectedFacing))"
      , "-- Coming back therefore starts from south again: a raw row has"
      , "-- no facing to carry, and inventing a remembered one for a row"
      , "-- class that has no facing model would be state nothing asks"
      , "-- for."
      , "assetBrowserStub.selectEntry(1, 'lifecycle:built')"
      , "pm.update(0.016)"
      , "assert(pm.dump().selectedFacing == 'south',"
      , "    tostring(pm.dump().selectedFacing))"
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
      , "-- The appearance row declares real paths under another"
      , "-- building's folder, all flagged outside_root. Selecting it"
      , "-- must load nothing new at all."
      , "assetBrowserStub.selectEntry(1, 'lifecycle:appearance')"
      , "pm.update(0.016); pm.update(0.016)"
      , "local d = pm.dump()"
      , "assert(d.selection.missing == true"
      , "   and d.selection.missingReason == 'outside_root')"
      , "assert(d.state == 'ready' and d.path == nil)"
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

  describe "a wholly diagnostic session" $ do
    it "still owns a scroll-capturing zoom surface, having requested no \
       \building texture at all" $ runsOk $ lns
      [ harness, diagnosticOnlyBrowse, findRow
      , "local d = pm.dump()"
      , "assert(d.selection.identity == 'lifecycle:built')"
      , "assert(d.selection.missing == true and d.selection.resolved == false)"
      , "assert(d.state == 'ready')"
      , "-- The premise: nothing under the building's own root was ever"
      , "-- requested, so a surface borrowed from the first loaded frame"
      , "-- would not exist. Only list chrome is allowed to be in flight."
      , "for _, p in ipairs(d.loadedPaths) do"
      , "  assert(p:find('assets/textures/ui/', 1, true) == 1,"
      , "      'a diagnostic session loaded a building texture: ' .. p)"
      , "end"
      , "assert(d.zoom.surface ~= nil,"
      , "    'the preview region must own a capturing surface (#1907)')"
      , "assert(elements[d.zoom.surface].scrollCapture == true,"
      , "    'and it must actually capture scroll')"
      , "assert(elements[d.zoom.surface].clickable == false"
      , "   and elements[d.zoom.surface].pointerBlocking == false,"
      , "    'capture ONLY: #743 keeps the three policies independent')"
      , "-- And the wheel really reaches it, rather than leaking to the"
      , "-- gameplay/z-slice broadcasts for the whole session."
      , "assert(pm.onUIScroll(d.zoom.surface, 0, 2))"
      , "pm.update(0.016)"
      , "assert(pm.dump().zoom.multiplier < 1,"
      , "    tostring(pm.dump().zoom.multiplier))"
      ]

    it "reports the diagnostic state on the ROW, so a row located from \
       \`rows` alone says whether its art is there" $ runsOk $ lns
      [ harness, mixedBrowse, findRow
      , "-- A row whose animation RESOLVED but whose west cell is absent"
      , "-- is a real authoring fault. `resolved` alone cannot say so."
      , "local construction = rowByIdentity(pm, 'lifecycle:construction')"
      , "assert(construction.resolved == true, 'the reference resolved')"
      , "assert(construction.missing == true,"
      , "    'yet one of its cells is not there')"
      , "assert(construction.missingReason == 'absent',"
      , "    tostring(construction.missingReason))"
      , "assert(construction.missingCells == 1,"
      , "    tostring(construction.missingCells))"
      , "-- A wholly unresolved row, and a healthy one, are both distinct"
      , "-- from it through the same fields."
      , "local gone = rowByIdentity(pm, 'lifecycle:destruction')"
      , "assert(gone.resolved == false and gone.missing == true"
      , "   and gone.missingReason == 'unresolved' and gone.missingCells == 4)"
      , "local built = rowByIdentity(pm, 'lifecycle:built')"
      , "assert(built.resolved == true and built.missing == false"
      , "   and built.missingReason == nil and built.missingCells == 0,"
      , "    'a healthy row reports no diagnostic at all')"
      , "-- The sprite row too, and a raw row instead reports whether it"
      , "-- backs any declaration."
      , "assert(rowByIdentity(pm, 'sprite').missing == false)"
      , "assert(rowByIdentity(pm, 'filesystem:stray.png').undeclared == true)"
      , "assert(rowByIdentity(pm, 'filesystem:idle').undeclared == false)"
      , "-- and `lifecycle` agrees with `rows`, rather than the two"
      , "-- surfaces disagreeing about the same row."
      , "for _, e in ipairs(pm.dump().lifecycle) do"
      , "  local r = rowByIdentity(pm, e.identity)"
      , "  assert(r.missing == e.missing and r.missingCells == e.missingCells,"
      , "      e.identity .. ': rows and lifecycle disagree')"
      , "end"
      ]

  describe "what is actually drawn" $ do
    it "re-points the enlarged sprite at the newly selected facing's own \
       \texture, and hides the missing marker while it does" $ runsOk $ lns
      [ harness, mixedBrowse, findRow
      , "-- The canonical row declares four DISTINCT paths, so a view"
      , "-- that changed only its dump would keep the same texture on"
      , "-- screen and fail here."
      , "assetBrowserStub.selectEntry(1, 'lifecycle:construction')"
      , "pm.update(0.016)"
      , "local d = pm.dump()"
      , "assert(d.selectedFacing == 'south')"
      , "local sprite = elemOf(d.spriteElement)"
      , "assert(sprite.visible == true, 'the enlarged sprite is drawn')"
      , "assert(shown(d.missingElement) == false,"
      , "    'and the missing marker is not')"
      , "local southTex = sprite.tex"
      , "assert(southTex, 'the enlarged sprite really holds a texture')"
      , "-- north is the other loadable canonical facing."
      , "local north = cellByFacing(pm, 'north')"
      , "assert(pm.onPreviewFacingClick(north.hitHandle))"
      , "pm.update(0.016)"
      , "local after = elemOf(pm.dump().spriteElement)"
      , "assert(after.tex ~= southTex,"
      , "    'the enlarged view must show the NEW facings own art')"
      , "assert(after.tex == elemOf(north.spriteElement).tex,"
      , "    'and specifically that cells own texture')"
      , "assert(after.visible == true and shown(pm.dump().missingElement) == false)"
      ]

    it "draws the missing marker and hides the sprite, in the cell and \
       \in the enlarged region alike" $ runsOk $ lns
      [ harness, mixedBrowse, findRow
      , "assetBrowserStub.selectEntry(1, 'lifecycle:construction')"
      , "pm.update(0.016)"
      , "local west = cellByFacing(pm, 'west')"
      , "assert(west.missing == true)"
      , "-- The CELL: marker visible, sprite hidden, and no handle."
      , "assert(shown(west.missingElement), 'the cell marker is drawn')"
      , "assert(elemOf(west.missingElement).text ~= nil"
      , "   and #elemOf(west.missingElement).text > 0,"
      , "    'and it carries a visible mark, not an empty string')"
      , "assert(shown(west.spriteElement) == false,"
      , "    'the cell sprite must be hidden, not left showing stale art')"
      , "assert(west.handle == nil)"
      , "-- A loadable sibling is the control: the marker is not simply"
      , "-- visible everywhere."
      , "local south = cellByFacing(pm, 'south')"
      , "assert(shown(south.missingElement) == false"
      , "   and shown(south.spriteElement) == true)"
      , "-- The ENLARGED region, once that facing is selected."
      , "assert(pm.onPreviewFacingClick(west.hitHandle))"
      , "pm.update(0.016)"
      , "local d = pm.dump()"
      , "assert(shown(d.missingElement), 'the enlarged marker is drawn')"
      , "assert(shown(d.spriteElement) == false,"
      , "    'and the enlarged sprite is hidden')"
      , "assert(d.state == 'ready' and d.path == nil)"
      ]

    it "marks a legacy row visibly -- a `*` on every cell caption and \
       \the enlarged legacy flag -- and a canonical row not at all" $
      runsOk $ lns
      [ harness, mixedBrowse, findRow
      , "-- The legacy built row: four repeated views must never read as"
      , "-- four authored ones (#2492 requirement 6)."
      , "assert(pm.dump().selection.identity == 'lifecycle:built')"
      , "assert(shown(pm.dump().legacyElement),"
      , "    'the enlarged legacy flag is drawn')"
      , "assert(elemOf(pm.dump().legacyElement).text == 'legacy',"
      , "    tostring(elemOf(pm.dump().legacyElement).text))"
      , "for _, c in ipairs(pm.dump().facingRow) do"
      , "  local caption = elemOf(c.labelElement).text"
      , "  assert(caption:sub(-1) == '*',"
      , "      c.facing .. ' caption must carry the legacy mark, got '"
      , "      .. tostring(caption))"
      , "end"
      , "-- The canonical row is the control, through the same reads."
      , "assetBrowserStub.selectEntry(1, 'lifecycle:construction')"
      , "pm.update(0.016)"
      , "assert(shown(pm.dump().legacyElement) == false,"
      , "    'a canonical row draws no legacy flag')"
      , "for _, c in ipairs(pm.dump().facingRow) do"
      , "  local caption = elemOf(c.labelElement).text"
      , "  assert(caption:sub(-1) ~= '*',"
      , "      c.facing .. ' caption must NOT be marked legacy: ' .. caption)"
      , "  assert(#caption > 0, 'and must still name its facing')"
      , "end"
      ]

    it "presents every invalid-cell kind as its own distinct, \
       \never-requested diagnostic" $ runsOk $ lns
      [ harness, invalidCellsBrowse, findRow
      , "local want = { south = 'directory', west = 'symlink',"
      , "               north = 'unsupported_extension', east = 'special' }"
      , "local d = pm.dump()"
      , "assert(d.selection.identity == 'lifecycle:built')"
      , "assert(d.state == 'ready', tostring(d.state))"
      , "for _, c in ipairs(d.facingRow) do"
      , "  assert(c.missing == true, c.facing)"
      , "  assert(c.missingReason == want[c.facing],"
      , "      c.facing .. ': ' .. tostring(c.missingReason)"
      , "      .. ' want ' .. tostring(want[c.facing]))"
      , "  assert(c.handle == nil, c.facing .. ' requested its bad path')"
      , "  assert(c.path == nil, c.facing .. ' resolved a path anyway')"
      , "  assert(shown(c.missingElement), c.facing .. ' drew no marker')"
      , "  assert(shown(c.spriteElement) == false,"
      , "      c.facing .. ' left its sprite showing')"
      , "end"
      , "assert(d.totals.missingCells == 4)"
      , "-- Nothing under the building's own root was ever requested."
      , "for _, p in ipairs(d.loadedPaths) do"
      , "  assert(p:find('assets/textures/ui/', 1, true) == 1,"
      , "      'a diagnostic row loaded: ' .. p)"
      , "end"
      , "-- Every one stays terminal as it is enlarged in turn."
      , "for _, facing in ipairs({'west', 'north', 'east'}) do"
      , "  local cell = cellByFacing(pm, facing)"
      , "  assert(pm.onPreviewFacingClick(cell.hitHandle))"
      , "  pm.update(0.016)"
      , "  local e = pm.dump()"
      , "  assert(e.selectedFacing == facing)"
      , "  assert(e.selection.missingReason == want[facing],"
      , "      facing .. ': ' .. tostring(e.selection.missingReason))"
      , "  assert(e.state == 'ready' and e.path == nil)"
      , "  assert(shown(e.missingElement) and shown(e.spriteElement) == false)"
      , "end"
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
      , "-- The legacy `built` row: TWO frames at 8 fps, so the replay"
      , "-- cycle is 0.25 s and the frame index really moves. A"
      , "-- single-frame clip would sit on 0 whatever the clock did, and"
      , "-- could not tell a preserved phase from a restarted one."
      , "NOW = 0"
      , "assetBrowserStub.selectEntry(1, 'lifecycle:built')"
      , "pm.onUIScroll(pm.dump().zoom.surface, 0, 2)"
      , "assert(pm.onKeyDown('Right')); assert(pm.onKeyDown('Right'))"
      , "-- Released before the clock advances: an unreleased hold would"
      , "-- fire its own repeat during the update below and move the"
      , "-- facing this case is about preserving."
      , "assert(pm.onKeyUp('Right'))"
      , "assetBrowserStub.setScrollOffset(1, 2)"
      , "NOW = 0.14"
      , "pm.update(0.016)"
      , "local before = pm.dump()"
      , "assert(before.selectedFacing == 'north', tostring(before.selectedFacing))"
      , "assert(before.playback.frameIndex == 1,"
      , "    'fps 8 at t=0.14 is frame 1, got '"
      , "    .. tostring(before.playback.frameIndex))"
      , "local held = before.zoom.multiplier"
      , "local beforeCell = cellByFacing(pm, 'north').bounds"
      , "pm.onFramebufferResize(1500, 1150)"
      , "pm.update(0.016)"
      , "local after = pm.dump()"
      , "assert(after.selection.identity == 'lifecycle:built',"
      , "    tostring(after.selection and after.selection.identity))"
      , "assert(after.selectedFacing == 'north', tostring(after.selectedFacing))"
      , "assert(after.scrollOffset == 2, tostring(after.scrollOffset))"
      , "assert(after.zoom.multiplier == held,"
      , "    tostring(after.zoom.multiplier) .. ' vs ' .. tostring(held))"
      , "assert(after.playback.frameIndex == 1, 'the frame did not jump')"
      , "-- The load-bearing half: the CLOCK itself survived, not just"
      , "-- the frame that happened to be showing. A resize that"
      , "-- restarted rowStart at 0.14 would read frame 1 again at 0.30"
      , "-- (elapsed 0.16); the original clock reads frame 0 (elapsed"
      , "-- 0.30, wrapped)."
      , "NOW = 0.30"
      , "pm.update(0.016)"
      , "assert(pm.dump().playback.frameIndex == 0,"
      , "    'the cycle continued on its ORIGINAL clock, got '"
      , "    .. tostring(pm.dump().playback.frameIndex))"
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

    it "treats a MALFORMED or unmatched YAML exactly like a missing one \
       \-- an empty declaration is not a partial one" $ runsOk $ lns
      [ harness, malformedYamlBrowse, findRow
      , "-- The engine yields no usable matrix, so it marshals an EMPTY"
      , "-- `declared` and no `defaultSelection` -- distinct on the wire"
      , "-- from the YAML-less payload, and required to behave the same."
      , "local d = pm.dump()"
      , "assert(#d.rows == 2 and #d.entries == 2)"
      , "assert(#d.lifecycle == 0 and d.staticSprite == nil)"
      , "assert(d.declaration == nil)"
      , "assert(d.selection.identity == 'filesystem:idle',"
      , "    'the unchanged raw defaultEntry ladder still decides')"
      , "assert(d.selected.label == 'idle' and d.defaultEntry == 'idle')"
      , "assert(d.selectedFacing == nil and #(d.facingRow or {}) == 0)"
      , "assert(not pm.onKeyDown('Left') and not pm.onKeyDown('Right'))"
      , "assert(d.totals.undeclaredFilesystemEntries == 2)"
      , "-- The raw browser keeps its existing playback, too."
      , "assert(d.playback ~= nil and d.playback.frameCount == 2)"
      , "NOW = 0.14; pm.update(0.016)"
      , "assert(pm.dump().playback.frameIndex == 1)"
      , "assert(math.abs(d.zoom.region.width - d.panelBounds.width) < 1e-6)"
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
