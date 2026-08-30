-- | Centered bounded zoom for the @--preview@ asset panes (#1907).
--
--   CPU-only, and deliberately drives the REAL shipped Lua rather than a
--   Haskell restatement of its arithmetic: @scripts/ui/preview_zoom.lua@,
--   @scripts/ui/unit_animation_view.lua@,
--   @scripts/ui/building_asset_view.lua@ and
--   @scripts/preview_manager.lua@ all run in a stdlib-only @HsLua@
--   interpreter — no engine, no GPU, no UI backend — with @engine@/@UI@
--   and the two modules that are NOT under test (@scripts.ui.list@,
--   @scripts.ui.asset_browser@) stubbed through @package.loaded@. That
--   is the pattern "Test.Headless.Lua.AssetFailure" already uses to
--   drive @preview_manager.lua@ headlessly.
--
--   Why the real modules matter here: @tools/preview_probe.py@ is
--   manual-only and @needs-gpu@ (@tools/ci_probes.py@), so this group is
--   the only BLOCKING automated gate zoom has. A test that reimplemented
--   the fit would pass while the shipped pane was wrong.
--
--   The stub browser fires @onSelect@ exactly where the real one does
--   (@selectEntry@ fires it, @selectEntrySilently@ does not), because
--   that timing IS the object-identity reset rule: a resize restores
--   silently and therefore preserves the multiplier for free.
module Test.Headless.Preview.Zoom (spec) where

import UPrelude
import Test.Hspec
import qualified HsLua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Run one self-contained Lua chunk in a fresh stdlib-only
--   interpreter, with the repo root as CWD (as every @cabal test@ run
--   has). The chunk signals failure through Lua's own @assert()@.
runsOk ∷ Text → Expectation
runsOk chunkText = do
    result ← Lua.run $ do
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

-- | A UI backend that records what the shipped code actually wrote:
--   every element's position, size, visibility, alpha, and each of #743's
--   three INDEPENDENT input policies, so a test can prove the zoom
--   surface opted into scroll capture ONLY.
--
--   @UI.getElementInfo@ answers from the same records, which is what
--   makes the dump's reported sprite bounds a read-back of a real write
--   rather than a restatement of the module's own arithmetic.
uiStub ∷ Text
uiStub = lns
    [ "elements = {}"
    , "local nextElem = 1"
    , "UI = {"
    , "  newPage = function() return 1 end,"
    , "  showPage = function() end,"
    , "  deletePage = function() end,"
    , "  newSprite = function(name, w, h, tex, r, g, b, a)"
    , "      local id = nextElem; nextElem = nextElem + 1"
    , "      elements[id] = { name = name, width = w, height = h, x = 0, y = 0,"
    , "                       tex = tex, alpha = a, visible = true,"
    , "                       scrollCapture = false, clickable = false,"
    , "                       pointerBlocking = false }"
    , "      return id"
    , "  end,"
    , "  newText = function(name)"
    , "      local id = nextElem; nextElem = nextElem + 1"
    , "      elements[id] = { name = name, width = 0, height = 0, x = 0, y = 0,"
    , "                       visible = true }"
    , "      return id"
    , "  end,"
    , "  addToPage = function(_p, id, x, y)"
    , "      local e = elements[id]; if e then e.x = x; e.y = y end end,"
    , "  addChild = function(_p, id, x, y)"
    , "      local e = elements[id]; if e then e.x = x; e.y = y end end,"
    , "  setSize = function(id, w, h)"
    , "      local e = elements[id]; if e then e.width = w; e.height = h end end,"
    , "  setPosition = function(id, x, y)"
    , "      local e = elements[id]; if e then e.x = x; e.y = y end end,"
    , "  setVisible = function(id, v)"
    , "      local e = elements[id]; if e then e.visible = v end end,"
    , "  setZIndex = function() end,"
    , "  setSpriteTexture = function(id, t)"
    , "      local e = elements[id]; if e then e.tex = t end end,"
    , "  setSpriteFrame = function(id, t)"
    , "      local e = elements[id]; if e then e.tex = t end end,"
    , "  setScrollCapture = function(id, v)"
    , "      local e = elements[id]; if e then e.scrollCapture = v end end,"
    , "  setClickable = function(id, v)"
    , "      local e = elements[id]; if e then e.clickable = v end end,"
    , "  setPointerBlocking = function(id, v)"
    , "      local e = elements[id]; if e then e.pointerBlocking = v end end,"
    , "  setOnClick = function() end,"
    , "  deleteElement = function(id) elements[id] = nil end,"
    , "  getElementInfo = function(id)"
    , "      local e = elements[id]"
    , "      if not e then return nil end"
    , "      return { x = e.x, y = e.y, width = e.width, height = e.height }"
    , "  end,"
    , "}"
    ]

-- | An @engine@ global with no live state behind it. @loadTexture@
--   hands back a distinct handle per request and remembers the latest,
--   so a test can deliver the asynchronous upload completion by hand;
--   @TEXTURE_SIZES@ lets a test give the atlas SHEET a size that is not
--   its cell's, which is how the cell-vs-sheet fit is proved.
engineStub ∷ Text
engineStub = lns
    [ "TEXTURE_SIZES = {}"
    , "local sizeOf = {}"
    , "engine = {"
    , "  logInfo = function() end, logWarn = function() end,"
    , "  logDebug = function() end, logError = function() end,"
    , "  setTextureFilter = function() end,"
    , "  loadFont = function() return 100 end,"
    , "  loadTexture = function(path)"
    , "      LOAD_COUNT = (LOAD_COUNT or 0) + 1"
    , "      local id = 1000 + LOAD_COUNT"
    , "      sizeOf[id] = TEXTURE_SIZES[path] or { width = 64, height = 32 }"
    , "      LAST_HANDLE, LAST_PATH = id, path"
    , "      return id"
    , "  end,"
    , "  getTextureSize = function(h) return sizeOf[h] end,"
    , "  getFramebufferSize = function() return FB_W or 1000, FB_H or 800 end,"
    , "  getPreviewBrowse = function() return BROWSE end,"
    , "  getPreviewTarget = function() return TARGET end,"
    , "  realTime = function() return NOW or 0 end,"
    , "}"
    ]

-- | The two modules NOT under test. The browser stub reproduces the one
--   behavior the reset rule depends on and nothing else: @selectEntry@
--   fires @onSelect@ (a genuine selection), @selectEntrySilently@ does
--   not (a resize restore).
browserStub ∷ Text
browserStub = lns
    [ "local browsers = {}"
    , "local nextBrowser = 1"
    , "assetBrowserStub = {"
    , "  init = function() end,"
    , "  new = function(params)"
    , "      local id = nextBrowser; nextBrowser = nextBrowser + 1"
    , "      browsers[id] = { params = params, selected = nil, scroll = 0 }"
    , "      return id"
    , "  end,"
    , "  getPanelBounds = function(id)"
    , "      local b = browsers[id]"
    , "      if not b then return nil end"
    , "      local p = b.params"
    , "      return { x = p.x + 300, y = p.y,"
    , "               width = p.width - 300, height = p.height }"
    , "  end,"
    , "  selectEntry = function(id, path)"
    , "      local b = browsers[id]"
    , "      if not b then return end"
    , "      local target = path"
    , "      if not target then"
    , "          local first = (b.params.entries or {})[1]"
    , "          target = first and first.path or nil"
    , "      end"
    , "      b.selected = target"
    , "      if b.params.onSelect and target then"
    , "          b.params.onSelect(target, target, 1)"
    , "      end"
    , "  end,"
    , "  selectEntrySilently = function(id, path)"
    , "      local b = browsers[id]; if b then b.selected = path end end,"
    , "  destroy = function(id) browsers[id] = nil end,"
    , "  getSelectedPath = function(id)"
    , "      local b = browsers[id]; return b and b.selected or nil end,"
    , "  getSelectedLabel = function(id)"
    , "      local b = browsers[id]; return b and b.selected or nil end,"
    , "  getScrollOffset = function(id)"
    , "      local b = browsers[id]; return b and b.scroll or 0 end,"
    , "  setScrollOffset = function(id, o)"
    , "      local b = browsers[id]; if b then b.scroll = o end end,"
    , "  dump = function() return {} end,"
    , "  onScroll = function()"
    , "      LIST_SCROLLS = (LIST_SCROLLS or 0) + 1; return true end,"
    , "}"
    , "package.loaded['scripts.ui.scale'] = { get = function() return 1 end }"
    , "package.loaded['scripts.ui.list'] = { init = function() end,"
    , "    getChromeTexture = function() return 900 end }"
    , "package.loaded['scripts.ui.asset_browser'] = assetBrowserStub"
    ]

-- | The REAL modules under test, plus the boot the engine performs:
--   @previewManager.init@ then the font's own @onAssetLoaded@, which is
--   what actually builds the page.
harness ∷ Text
harness = lns
    [ uiStub
    , engineStub
    , browserStub
    , "package.loaded['scripts.ui.preview_zoom'] ="
    , "    dofile('scripts/ui/preview_zoom.lua')"
    , "package.loaded['scripts.ui.unit_animation_view'] ="
    , "    dofile('scripts/ui/unit_animation_view.lua')"
    , "package.loaded['scripts.ui.building_asset_view'] ="
    , "    dofile('scripts/ui/building_asset_view.lua')"
    , "pz = package.loaded['scripts.ui.preview_zoom']"
    , "function bootPreview(browse, target, fbw, fbh)"
    , "    BROWSE, TARGET = browse, target"
    , "    FB_W, FB_H = fbw or 1000, fbh or 800"
    , "    dofile('scripts/preview_manager.lua')"
    , "    local pm = package.loaded['scripts.preview_manager']"
    , "    pm.init(1)"
    , "    pm.onAssetLoaded('font', 100, 'assets/fonts/arcade.ttf')"
    , "    return pm"
    , "end"
    , "-- The engine's asynchronous upload completion, delivered by hand."
    , "function resolveTexture(pm)"
    , "    if LAST_HANDLE then pm.onAssetLoaded('texture', LAST_HANDLE, LAST_PATH) end"
    , "end"
    , "function approx(a, b, tol)"
    , "    return math.abs(a - b) <= (tol or 1e-9)"
    , "end"
    , "-- Requirement 3, asserted the same way for every pane."
    , "function assertContained(what, rect, region)"
    , "    assert(rect and region, what .. ': missing geometry')"
    , "    assert(rect.w > 0 and rect.h > 0,"
    , "        what .. ': non-positive extent ' .. tostring(rect.w)"
    , "        .. 'x' .. tostring(rect.h))"
    , "    assert(rect.w == rect.w and rect.h == rect.h,"
    , "        what .. ': non-finite extent')"
    , "    assert(rect.x >= region.x - 1e-6 and rect.y >= region.y - 1e-6,"
    , "        what .. ': escapes the region origin')"
    , "    assert(rect.x + rect.w <= region.x + region.width + 1e-6,"
    , "        what .. ': overflows the region width')"
    , "    assert(rect.y + rect.h <= region.y + region.height + 1e-6,"
    , "        what .. ': overflows the region height')"
    , "end"
    , "function assertCentered(what, rect, region)"
    , "    assert(approx(rect.x + rect.w / 2, region.x + region.width / 2, 1e-6),"
    , "        what .. ': not horizontally centered')"
    , "    assert(approx(rect.y + rect.h / 2, region.y + region.height / 2, 1e-6),"
    , "        what .. ': not vertically centered')"
    , "end"
    ]

-- | A bare simple-category browse payload (icons): two textures, no
--   @item@ in the target, so each texture is its own preview object.
bareList ∷ Text
bareList = lns
    [ "local browse = { mode = 'list', entries = {"
    , "    { label = 'a.png', path = 'assets/textures/icons/a.png' },"
    , "    { label = 'b.png', path = 'assets/textures/icons/b.png' } } }"
    , "local pm = bootPreview(browse, { category = 'icons' })"
    , "resolveTexture(pm)"
    ]

-- | One atlas-backed unit whose SHEET size is deliberately unlike its
--   cell size, so a fit taken from the wrong one is visible.
unitBrowse ∷ Text
unitBrowse = lns
    [ "local function frame()"
    , "    return { path = 'assets/textures/units/acolyte/atlas/idle.png',"
    , "             u0 = 0, v0 = 0, u1 = 0.25, v1 = 0.5,"
    , "             width = 32, height = 48 }"
    , "end"
    , "local function dirEntry(name, mirrored)"
    , "    return { direction = name, source = name, mirrored = mirrored,"
    , "             frames = { frame(), frame() } }"
    , "end"
    , "local unit = { name = 'acolyte', defaultAnim = 'idle', animations = {"
    , "  { name = 'idle', fps = 8, loop = true, flip = true,"
    , "    atlas = 'assets/textures/units/acolyte/atlas/idle.png',"
    , "    thumb = { path = 'assets/textures/units/acolyte/atlas/idle.png',"
    , "              u0 = 0, v0 = 0, u1 = 0.25, v1 = 0.5 },"
    , "    directions = { dirEntry('south', false), dirEntry('west', true) } },"
    , "  { name = 'walk', fps = 8, loop = true, flip = true,"
    , "    atlas = 'assets/textures/units/acolyte/atlas/walk.png',"
    , "    thumb = { path = 'assets/textures/units/acolyte/atlas/walk.png',"
    , "              u0 = 0, v0 = 0, u1 = 0.25, v1 = 0.5 },"
    , "    directions = { dirEntry('south', false) } },"
    , "} }"
    , "-- The compiled SHEET, which is not the cell: a fit taken from"
    , "-- engine.getTextureSize instead of the index's own cell geometry"
    , "-- would size the sprite to 256x96's aspect ratio, not 32x48's."
    , "TEXTURE_SIZES['assets/textures/units/acolyte/atlas/idle.png'] ="
    , "    { width = 256, height = 96 }"
    , "TEXTURE_SIZES['assets/textures/units/acolyte/atlas/walk.png'] ="
    , "    { width = 256, height = 96 }"
    , "local pm = bootPreview({ mode = 'unit', unit = unit },"
    , "                       { category = 'units', item = 'acolyte' })"
    , "pm.update(0.016)"
    ]

buildingBrowse ∷ Text
buildingBrowse = lns
    [ "local building = { name = 'acolyte_portal', defaultEntry = 'idle',"
    , "  entries = {"
    , "  { label = 'idle', animated = true, fps = 8, loop = false,"
    , "    frames = {"
    , "      'assets/textures/buildings/acolyte_portal/idle/frame_000.png',"
    , "      'assets/textures/buildings/acolyte_portal/idle/frame_001.png' } },"
    , "  { label = 'default.png', animated = false,"
    , "    frames = { 'assets/textures/buildings/acolyte_portal/default.png' } },"
    , "} }"
    , "local pm = bootPreview({ mode = 'building', building = building },"
    , "                       { category = 'buildings', item = 'acolyte_portal' })"
    , "pm.update(0.016)"
    ]

spec ∷ Spec
spec = do
  describe "limits and the fit-to-region arithmetic" $ do
    it "1 is the initial and maximum multiplier and 1/8 the minimum" $ runsOk $ lns
      [ harness
      , "assert(pz.MAX == 1, 'MAX is 1')"
      , "assert(pz.MIN == 1 / 8, 'MIN is one eighth')"
      , "assert(pz.clamp(nil) == pz.MAX, 'an absent multiplier reads as the initial 1')"
      , "assert(pz.clamp(4) == pz.MAX and pz.clamp(0) == pz.MIN, 'clamped both ways')"
      ]

    it "multiplier 1 equals fit-to-region, and 1/8 is exactly one eighth \
       \of those fitted dimensions, for landscape, portrait and square \
       \assets alike" $ runsOk $ lns
      [ harness
      , "local box = { x = 100, y = 50, width = 400, height = 200 }"
      , "local cases = { { 100, 100, 'square' }, { 400, 100, 'landscape' },"
      , "                { 100, 400, 'portrait' } }"
      , "for _, c in ipairs(cases) do"
      , "    local w, h, label = c[1], c[2], c[3]"
      , "    local full = pz.fitRect(box, w, h, pz.MAX)"
      , "    -- The fit itself: aspect preserved, and touching at least"
      , "    -- one axis of the region (as large as the ratio permits)."
      , "    assert(approx(full.width / full.height, w / h, 1e-9),"
      , "        label .. ': aspect ratio changed')"
      , "    assert(approx(full.width, box.width, 1e-9)"
      , "        or approx(full.height, box.height, 1e-9),"
      , "        label .. ': multiplier 1 must FILL the region on one axis')"
      , "    local eighth = pz.fitRect(box, w, h, pz.MIN)"
      , "    assert(approx(eighth.width, full.width / 8, 1e-9),"
      , "        label .. ': width is not one eighth of the fit')"
      , "    assert(approx(eighth.height, full.height / 8, 1e-9),"
      , "        label .. ': height is not one eighth of the fit')"
      , "    for _, r in ipairs({ full, eighth }) do"
      , "        local rect = { x = r.x, y = r.y, w = r.width, h = r.height }"
      , "        assertContained(label, rect, box)"
      , "        assertCentered(label, rect, box)"
      , "    end"
      , "end"
      ]

    it "a fractional delta and a differently sized one both change the \
       \multiplier monotonically, and splitting one delta into two \
       \totals the same as the whole (magnitude, not sign)" $ runsOk $ lns
      [ harness
      , "assert(pz.step(0.5, 1) < 0.5, 'dy > 0 shrinks')"
      , "assert(pz.step(0.5, -1) > 0.5, 'dy < 0 enlarges')"
      , "assert(pz.step(0.5, 0.25) < 0.5 and pz.step(0.5, 0.25) > pz.step(0.5, 1),"
      , "    'a fractional delta moves less than a whole one, not the same')"
      , "assert(pz.step(0.5, 2) < pz.step(0.5, 1),"
      , "    'a larger delta shrinks further -- magnitude is not reduced to a sign')"
      , "local split = pz.step(pz.step(0.5, 0.3), 0.2)"
      , "assert(approx(split, pz.step(0.5, 0.5), 1e-12),"
      , "    'an OS that splits one notch into several deltas must total the same')"
      , "-- Monotone across a whole sweep, in both directions."
      , "local m = pz.MAX"
      , "for _ = 1, 40 do"
      , "    local next = pz.step(m, 0.4)"
      , "    assert(next <= m, 'shrinking is monotone')"
      , "    m = next"
      , "end"
      , "assert(m == pz.MIN, 'clamps EXACTLY at the minimum, got ' .. tostring(m))"
      , "for _ = 1, 40 do"
      , "    local next = pz.step(m, -0.4)"
      , "    assert(next >= m, 'enlarging is monotone')"
      , "    m = next"
      , "end"
      , "assert(m == pz.MAX, 'clamps EXACTLY at the maximum, got ' .. tostring(m))"
      ]

    it "further input at either limit is stable: the multiplier does not \
       \drift, overshoot, or wrap" $ runsOk $ lns
      [ harness
      , "local low = pz.MIN"
      , "for _ = 1, 25 do low = pz.step(low, 3) end"
      , "assert(low == pz.MIN, 'still exactly MIN, got ' .. tostring(low))"
      , "local high = pz.MAX"
      , "for _ = 1, 25 do high = pz.step(high, -3) end"
      , "assert(high == pz.MAX, 'still exactly MAX, got ' .. tostring(high))"
      , "-- ...and a mid-range multiplier still moves, so the two above"
      , "-- prove saturation rather than an inert step function."
      , "assert(pz.step(0.5, 3) < 0.5 and pz.step(0.5, -3) > 0.5)"
      ]

    it "invalid or degenerate geometry yields no rect at all, rather than \
       \a non-finite, negative or inverted one" $ runsOk $ lns
      [ harness
      , "local box = { x = 0, y = 0, width = 100, height = 100 }"
      , "local nan = 0 / 0"
      , "local inf = math.huge"
      , "local bad = {"
      , "  { nil, 10, 10, 'no box' },"
      , "  { { x = 0, y = 0, width = 0, height = 10 }, 10, 10, 'zero-width box' },"
      , "  { { x = 0, y = 0, width = 10, height = -5 }, 10, 10, 'negative-height box' },"
      , "  { { x = nan, y = 0, width = 10, height = 10 }, 10, 10, 'NaN origin' },"
      , "  { { x = 0, y = 0, width = inf, height = 10 }, 10, 10, 'infinite box' },"
      , "  { box, 0, 10, 'zero source width' },"
      , "  { box, 10, -1, 'negative source height' },"
      , "  { box, nan, 10, 'NaN source width' },"
      , "  { box, inf, 10, 'infinite source width' },"
      , "}"
      , "for _, c in ipairs(bad) do"
      , "    assert(pz.fitRect(c[1], c[2], c[3], 1) == nil,"
      , "        c[4] .. ' must yield no rect')"
      , "end"
      , "-- ...and a valid but extreme case still yields a sane rect."
      , "local tiny = pz.fitRect({ x = 0, y = 0, width = 1, height = 1 },"
      , "                        4096, 4096, pz.MIN)"
      , "assert(tiny and tiny.width > 0 and tiny.height > 0"
      , "    and tiny.width == tiny.width, 'a 1x1 region still fits')"
      , "assertContained('tiny region', "
      , "    { x = tiny.x, y = tiny.y, w = tiny.width, h = tiny.height },"
      , "    { x = 0, y = 0, width = 1, height = 1 })"
      ]

  describe "wheel input over the preview region" $ do
    it "owns a scroll-CAPTURING surface over the region, and opts into \
       \nothing else -- #743's three policies stay independent, so \
       \list-row and direction-cell clicks are untouched" $ runsOk $ lns
      [ harness
      , bareList
      , "local d = pm.dump()"
      , "local surface = d.zoom.surface"
      , "assert(surface, 'the preview region owns a zoom surface')"
      , "local e = elements[surface]"
      , "assert(e.scrollCapture == true, 'it captures scroll')"
      , "assert(e.clickable == false, 'it is NOT clickable')"
      , "assert(e.pointerBlocking == false, 'it does NOT block the pointer')"
      , "assert(e.alpha == 0.0, 'it is invisible')"
      , "assert(e.visible == true, 'but effectively visible, or it would not hit-test')"
      , "assert(approx(e.x, d.zoom.region.x) and approx(e.y, d.zoom.region.y)"
      , "    and approx(e.width, d.zoom.region.width)"
      , "    and approx(e.height, d.zoom.region.height),"
      , "    'the surface covers exactly the reported zoom region')"
      ]

    it "shrinks to the lower limit and enlarges back to the fitted size, \
       \staying centered and contained at every step" $ runsOk $ lns
      [ harness
      , bareList
      , "local d = pm.dump()"
      , "local surface = d.zoom.surface"
      , "local fitted = d.zoom.sprite"
      , "assert(d.zoom.multiplier == pz.MAX, 'a new session starts at 1')"
      , "assertCentered('fitted', fitted, d.zoom.region)"
      , "for _ = 1, 40 do"
      , "    pm.onUIScroll(surface, 0, 1)"
      , "    local s = pm.dump()"
      , "    assertContained('shrinking', s.zoom.sprite, s.zoom.region)"
      , "    assertCentered('shrinking', s.zoom.sprite, s.zoom.region)"
      , "end"
      , "local low = pm.dump()"
      , "assert(low.zoom.multiplier == pz.MIN, 'reached the floor')"
      , "assert(approx(low.zoom.sprite.w, fitted.w / 8, 1e-6),"
      , "    'one eighth of the fitted width, got ' .. tostring(low.zoom.sprite.w))"
      , "assert(approx(low.zoom.sprite.h, fitted.h / 8, 1e-6),"
      , "    'one eighth of the fitted height')"
      , "for _ = 1, 40 do"
      , "    pm.onUIScroll(surface, 0, -1)"
      , "    local s = pm.dump()"
      , "    assertContained('enlarging', s.zoom.sprite, s.zoom.region)"
      , "end"
      , "local high = pm.dump()"
      , "assert(high.zoom.multiplier == pz.MAX, 'back to the ceiling')"
      , "assert(approx(high.zoom.sprite.w, fitted.w, 1e-6)"
      , "    and approx(high.zoom.sprite.h, fitted.h, 1e-6),"
      , "    'and back to exactly the fitted size, never past it')"
      ]

    it "matches the gameplay wheel convention: the direction that zooms \
       \the camera IN enlarges the preview" $ runsOk $ lns
      [ harness
      , bareList
      , "local surface = pm.dump().zoom.surface"
      , "-- Engine.Loop.Camera: dy < 0 zooms IN (camZoom is the viewport"
      , "-- half-height, so smaller is closer), dy > 0 zooms out."
      , "pm.onUIScroll(surface, 0, 2)"
      , "local shrunk = pm.dump().zoom.multiplier"
      , "assert(shrunk < pz.MAX, 'dy > 0 must SHRINK, got ' .. tostring(shrunk))"
      , "pm.onUIScroll(surface, 0, -1)"
      , "assert(pm.dump().zoom.multiplier > shrunk,"
      , "    'dy < 0 must ENLARGE')"
      ]

    it "behaves identically for plain and Shift-modified wheel input" $ runsOk $ lns
      [ harness
      , bareList
      , "local surface = pm.dump().zoom.surface"
      , "pm.onUIScroll(surface, 0, 1, false)"
      , "local plain = pm.dump().zoom.multiplier"
      , "pm.onUIScroll(surface, 0, -1, false)"
      , "pm.onUIScroll(surface, 0, 1, true)"
      , "local shifted = pm.dump().zoom.multiplier"
      , "assert(plain == shifted,"
      , "    'Shift must not change the result: ' .. tostring(plain)"
      , "    .. ' vs ' .. tostring(shifted))"
      ]

    it "keeps list and pane input spatially unambiguous, at the limits \
       \too: a list element scrolls the list and never the zoom, and the \
       \zoom surface never moves the list offset" $ runsOk $ lns
      [ harness
      , bareList
      , "local surface = pm.dump().zoom.surface"
      , "LIST_SCROLLS = 0"
      , "-- An element handle that is not the zoom surface is the list's."
      , "pm.onUIScroll(surface + 999, 0, -3)"
      , "assert(LIST_SCROLLS == 1, 'the list received it')"
      , "assert(pm.dump().zoom.multiplier == pz.MAX,"
      , "    'a list scroll must never change the zoom')"
      , "-- Saturate the zoom, then keep scrolling the pane: the surplus"
      , "-- must be consumed here, not fall through to the list."
      , "for _ = 1, 40 do pm.onUIScroll(surface, 0, 1) end"
      , "assert(pm.dump().zoom.multiplier == pz.MIN)"
      , "LIST_SCROLLS = 0"
      , "for _ = 1, 5 do pm.onUIScroll(surface, 0, 1) end"
      , "assert(LIST_SCROLLS == 0,"
      , "    'input at the zoom limit must not leak into the list')"
      , "assert(pm.dump().zoom.multiplier == pz.MIN, 'and changes nothing')"
      ]

    it "reaches focused-item mode, which has no browser at all" $ runsOk $ lns
      [ harness
      , "local pm = bootPreview({ mode = 'item', entry = {"
      , "    label = 'skill/climbing.png',"
      , "    path = 'assets/textures/icons/skill/climbing.png' } },"
      , "    { category = 'icons', item = 'skill/climbing.png' })"
      , "resolveTexture(pm)"
      , "local d = pm.dump()"
      , "assert(d.mode == 'item' and d.state == 'ready', tostring(d.mode))"
      , "assert(d.zoom.surface, 'focused item mode owns a zoom surface too')"
      , "local fitted = d.zoom.sprite"
      , "pm.onUIScroll(d.zoom.surface, 0, 3)"
      , "local s = pm.dump()"
      , "assert(s.zoom.multiplier < pz.MAX, 'the guard is scoped to the "
        <> "list-forwarding branch, so item mode really zooms')"
      , "assert(s.zoom.sprite.w < fitted.w)"
      , "assertContained('focused item', s.zoom.sprite, s.zoom.region)"
      , "assertCentered('focused item', s.zoom.sprite, s.zoom.region)"
      ]

    -- A texture upload is asynchronous. If the capturing surface only
    -- appeared once onAssetLoaded landed, the whole load would be a
    -- window in which a wheel over the preview pane never reached
    -- onUIScroll at all -- routeScroll would find no capturing element
    -- and the event would leak to the gameplay/z-slice broadcasts.
    it "zooms while the first texture is still loading, in list mode -- \
       \the surface is primed from the REQUEST, not the completion"
      $ runsOk $ lns
      [ harness
      , "local browse = { mode = 'list', entries = {"
      , "    { label = 'a.png', path = 'assets/textures/icons/a.png' } } }"
      , "local pm = bootPreview(browse, { category = 'icons' })"
      , "-- Deliberately NO resolveTexture: the upload is still in flight."
      , "assert(pm.dump().state == 'loading', 'precondition: still loading')"
      , "local d = pm.dump()"
      , "assert(d.zoom.surface, 'the capturing surface exists during the load')"
      , "assert(elements[d.zoom.surface].scrollCapture == true,"
      , "    'and it really captures scroll')"
      , "pm.onUIScroll(d.zoom.surface, 0, 2)"
      , "local held = pm.dump().zoom.multiplier"
      , "assert(held < pz.MAX,"
      , "    'a wheel during the load must zoom, got ' .. tostring(held))"
      , "-- ...and the multiplier it set is what the texture is fitted at"
      , "-- once the upload finally lands."
      , "resolveTexture(pm)"
      , "local after = pm.dump()"
      , "assert(after.state == 'ready', 'the load still completes normally')"
      , "assert(after.zoom.multiplier == held, 'the multiplier survives')"
      , "local fitted = pz.fitRect(after.zoom.region, 64, 32, held)"
      , "assert(approx(after.zoom.sprite.w, fitted.width, 1e-6),"
      , "    'the arriving texture is fitted AT that multiplier, got '"
      , "    .. tostring(after.zoom.sprite.w) .. ' want '"
      , "    .. tostring(fitted.width))"
      , "assertContained('loaded at held zoom', after.zoom.sprite,"
      , "    after.zoom.region)"
      ]

    it "zooms while the first texture is still loading, in focused-item \
       \mode too -- and still loads nothing extra for the surface"
      $ runsOk $ lns
      [ harness
      , "local pm = bootPreview({ mode = 'item', entry = {"
      , "    label = 'skill/climbing.png',"
      , "    path = 'assets/textures/icons/skill/climbing.png' } },"
      , "    { category = 'icons', item = 'skill/climbing.png' })"
      , "assert(pm.dump().state == 'loading', 'precondition: still loading')"
      , "local d = pm.dump()"
      , "assert(d.zoom.surface, 'the capturing surface exists during the load')"
      , "assert(#d.loadedPaths == 1,"
      , "    'and priming it loaded nothing extra, got '"
      , "    .. tostring(#d.loadedPaths))"
      , "assert(elements[d.zoom.surface].tex == LAST_HANDLE,"
      , "    'it borrowed the in-flight request own handle')"
      , "pm.onUIScroll(d.zoom.surface, 0, 2)"
      , "assert(pm.dump().zoom.multiplier < pz.MAX,"
      , "    'a wheel during the load must zoom in item mode too')"
      ]

    -- The borrowed handle can be exactly the one that dies. The surface
    -- must outlive it: deleting the element would take wheel capture
    -- down with it, and #1690 makes "empty" terminal, so zoom would be
    -- unrecoverable for the rest of the session.
    it "keeps zooming when the very request the surface borrowed fails, \
       \and re-points the surface at the next live handle" $ runsOk $ lns
      [ harness
      , "local browse = { mode = 'list', entries = {"
      , "    { label = 'a.png', path = 'assets/textures/icons/a.png' },"
      , "    { label = 'b.png', path = 'assets/textures/icons/b.png' } } }"
      , "local pm = bootPreview(browse, { category = 'icons' })"
      , "local surface = pm.dump().zoom.surface"
      , "local dead = LAST_HANDLE"
      , "assert(elements[surface].tex == dead, 'the surface borrowed it')"
      , "pm.onAssetFailed('texture', dead,"
      , "    'assets/textures/icons/a.png', 'no bindless slot')"
      , "assert(elements[surface] ~= nil,"
      , "    'the surface element survives the failure')"
      , "pm.onUIScroll(surface, 0, 2)"
      , "assert(pm.dump().zoom.multiplier < pz.MAX,"
      , "    'and the wheel still zooms after it')"
      , "-- The next selection re-points the SAME surface at a live"
      , "-- handle rather than leaving a dead one bound."
      , "assetBrowserStub.selectEntry(1, 'assets/textures/icons/b.png')"
      , "resolveTexture(pm)"
      , "assert(pm.dump().zoom.surface == surface,"
      , "    'still the same surface element')"
      , "assert(elements[surface].tex ~= dead,"
      , "    'and it no longer holds the dead handle')"
      ]

    it "loads no texture of its own for the surface -- focused-item mode \
       \has no chrome allowance, so the surface reuses a handle the \
       \session already asked for" $ runsOk $ lns
      [ harness
      , "local pm = bootPreview({ mode = 'item', entry = {"
      , "    label = 'skill/climbing.png',"
      , "    path = 'assets/textures/icons/skill/climbing.png' } },"
      , "    { category = 'icons', item = 'skill/climbing.png' })"
      , "resolveTexture(pm)"
      , "local d = pm.dump()"
      , "assert(#d.loadedPaths == 1,"
      , "    'exactly one texture request, got ' .. tostring(#d.loadedPaths))"
      , "assert(d.loadedPaths[1] == 'assets/textures/icons/skill/climbing.png')"
      , "assert(elements[d.zoom.surface].tex == LAST_HANDLE,"
      , "    'the surface reuses the item texture handle, not a new chrome load')"
      ]

    -- The nastiest ordering, and the one the three ownership tests in
    -- onAssetFailed all miss: the request that CREATED the surface is
    -- abandoned (a new selection supersedes it) and only THEN fails. It
    -- is no longer pendingHandle, and it never reached viewHandles or
    -- textureCache because it never resolved -- so a release placed
    -- after those tests never runs, and adoptZoomSurfaceTexture refuses
    -- to replace a non-nil record, stranding the surface on a dead
    -- texture for the rest of the session.
    it "rebinds the surface when the request it borrowed is abandoned \
       \first and fails afterwards" $ runsOk $ lns
      [ harness
      , "local browse = { mode = 'list', entries = {"
      , "    { label = 'a.png', path = 'assets/textures/icons/a.png' },"
      , "    { label = 'b.png', path = 'assets/textures/icons/b.png' } } }"
      , "local pm = bootPreview(browse, { category = 'icons' })"
      , "local surface = pm.dump().zoom.surface"
      , "local abandoned = LAST_HANDLE"
      , "assert(elements[surface].tex == abandoned,"
      , "    'the surface borrowed the first request')"
      , "-- The user selects B while A is STILL in flight, so A never"
      , "-- resolves: it reaches neither viewHandles nor textureCache."
      , "assetBrowserStub.selectEntry(1, 'assets/textures/icons/b.png')"
      , "local live = LAST_HANDLE"
      , "assert(live ~= abandoned, 'B really is a second request')"
      , "-- ...and only NOW does the abandoned request fail."
      , "pm.onAssetFailed('texture', abandoned,"
      , "    'assets/textures/icons/a.png', 'no bindless slot')"
      , "assert(elements[surface] ~= nil, 'the surface element survives')"
      , "assert(elements[surface].tex ~= abandoned,"
      , "    'and is no longer bound to the dead handle')"
      , "assert(elements[surface].tex == live,"
      , "    'it rebound to the in-flight request, got '"
      , "    .. tostring(elements[surface].tex))"
      , "-- B completing must still be able to settle the view, and the"
      , "-- wheel must still zoom throughout."
      , "resolveTexture(pm)"
      , "assert(pm.dump().state == 'ready', 'B still resolves normally')"
      , "assert(pm.dump().zoom.surface == surface, 'same surface element')"
      , "pm.onUIScroll(surface, 0, 2)"
      , "assert(pm.dump().zoom.multiplier < pz.MAX,"
      , "    'and the wheel still zooms after the stale failure')"
      ]

    -- With nothing live to rebind to, the record is released rather than
    -- left pointing at a dead handle, so the NEXT request adopts and
    -- re-points this same element instead of building a second one.
    it "releases the borrowed handle even when no live one is available \
       \yet, and the next request re-points the same surface"
      $ runsOk $ lns
      [ harness
      , "local browse = { mode = 'list', entries = {"
      , "    { label = 'a.png', path = 'assets/textures/icons/a.png' },"
      , "    { label = 'b.png', path = 'assets/textures/icons/b.png' } } }"
      , "local pm = bootPreview(browse, { category = 'icons' })"
      , "local surface = pm.dump().zoom.surface"
      , "local dead = LAST_HANDLE"
      , "-- The one and only request fails while it is still the pending"
      , "-- one, so nothing live exists to rebind to at that moment."
      , "pm.onAssetFailed('texture', dead,"
      , "    'assets/textures/icons/a.png', 'no bindless slot')"
      , "assert(elements[surface] ~= nil, 'the surface element survives')"
      , "assetBrowserStub.selectEntry(1, 'assets/textures/icons/b.png')"
      , "assert(pm.dump().zoom.surface == surface,"
      , "    'the next request re-points the SAME element, never a second')"
      , "assert(elements[surface].tex == LAST_HANDLE,"
      , "    'and it now holds the new live handle, got '"
      , "    .. tostring(elements[surface].tex))"
      , "pm.onUIScroll(surface, 0, 2)"
      , "assert(pm.dump().zoom.multiplier < pz.MAX, 'zoom still works')"
      ]

  describe "zoom follows preview-object identity" $ do
    it "a different texture in a BARE simple-category browser is a \
       \different preview object and resets the multiplier" $ runsOk $ lns
      [ harness
      , bareList
      , "local surface = pm.dump().zoom.surface"
      , "pm.onUIScroll(surface, 0, 2)"
      , "assert(pm.dump().zoom.multiplier < pz.MAX)"
      , "assetBrowserStub.selectEntry(1, 'assets/textures/icons/b.png')"
      , "resolveTexture(pm)"
      , "assert(pm.dump().zoom.multiplier == pz.MAX,"
      , "    'a different bare-category texture resets to 1, got '"
      , "    .. tostring(pm.dump().zoom.multiplier))"
      ]

    it "a flora stage or structure piece is another view of the SAME \
       \grouped object and preserves the multiplier -- even though it \
       \is the identical list mode a bare category uses" $ runsOk $ lns
      [ harness
      , "local browse = { mode = 'list', entries = {"
      , "    { label = 'stage0.png',"
      , "      path = 'assets/textures/world/flora/oak/stage0.png' },"
      , "    { label = 'stage1.png',"
      , "      path = 'assets/textures/world/flora/oak/stage1.png' } } }"
      , "-- The ONLY thing separating this from the bare-category case is"
      , "-- engine.getPreviewTarget()'s `item`; the browse mode is the same."
      , "local pm = bootPreview(browse, { category = 'flora', item = 'oak' })"
      , "resolveTexture(pm)"
      , "assert(pm.dump().mode == 'list', 'grouped items reuse the list browser')"
      , "pm.onUIScroll(pm.dump().zoom.surface, 0, 2)"
      , "local held = pm.dump().zoom.multiplier"
      , "assert(held < pz.MAX)"
      , "assetBrowserStub.selectEntry(1,"
      , "    'assets/textures/world/flora/oak/stage1.png')"
      , "resolveTexture(pm)"
      , "assert(pm.dump().zoom.multiplier == held,"
      , "    'another stage of the same item preserves zoom, got '"
      , "    .. tostring(pm.dump().zoom.multiplier))"
      ]

    it "a resize preserves the multiplier while recomputing the fitted \
       \size from the new region (list mode)" $ runsOk $ lns
      [ harness
      , bareList
      , "pm.onUIScroll(pm.dump().zoom.surface, 0, 2)"
      , "local held = pm.dump().zoom.multiplier"
      , "local before = pm.dump().zoom"
      , "pm.onFramebufferResize(1600, 1200)"
      , "local after = pm.dump().zoom"
      , "assert(after.multiplier == held, 'the multiplier survives a resize')"
      , "assert(after.region.width ~= before.region.width,"
      , "    'but the region really did change')"
      , "assert(after.sprite.w > before.sprite.w,"
      , "    'so the fitted size it multiplies was recomputed')"
      , "assertContained('after resize', after.sprite, after.region)"
      , "assertCentered('after resize', after.sprite, after.region)"
      , "assert(approx(elements[after.surface].width, after.region.width),"
      , "    'and the surface followed the new region')"
      ]

  describe "unit animation viewer" $ do
    it "zooms over the ENLARGED sub-rect, never the whole panel, and \
       \sizes the frame from the atlas CELL rather than the sheet" $ runsOk $ lns
      [ harness
      , unitBrowse
      , "local d = pm.dump()"
      , "assert(d.mode == 'unit', tostring(d.mode))"
      , "local region = d.zoom.region"
      , "assert(region.height < d.panelBounds.height,"
      , "    'the zoom region excludes the direction strip: '"
      , "    .. tostring(region.height) .. ' vs panel '"
      , "    .. tostring(d.panelBounds.height))"
      , "-- 32x48 is the index's own cell; 256x96 is the compiled sheet."
      , "local wantCell = pz.fitRect(region, 32, 48, pz.MAX)"
      , "local wantSheet = pz.fitRect(region, 256, 96, pz.MAX)"
      , "assert(approx(d.zoom.sprite.w, wantCell.width, 1e-6)"
      , "    and approx(d.zoom.sprite.h, wantCell.height, 1e-6),"
      , "    'the fit must come from the cell, got ' .. tostring(d.zoom.sprite.w)"
      , "    .. 'x' .. tostring(d.zoom.sprite.h) .. ' want '"
      , "    .. tostring(wantCell.width) .. 'x' .. tostring(wantCell.height))"
      , "assert(not approx(wantCell.width, wantSheet.width, 1e-6),"
      , "    'the two fits really do differ, so that assertion means something')"
      , "assertContained('unit enlarged', d.zoom.sprite, region)"
      , "assertCentered('unit enlarged', d.zoom.sprite, region)"
      ]

    it "preserves the multiplier across an animation change, a direction \
       \change, playback and a resize -- a unit is ONE preview object" $ runsOk $ lns
      [ harness
      , unitBrowse
      , "local d = pm.dump()"
      , "pm.onUIScroll(d.zoom.surface, 0, 2)"
      , "pm.update(0.016)"
      , "local held = pm.dump().zoom.multiplier"
      , "assert(held < pz.MAX)"
      , "assetBrowserStub.selectEntry(1, 'walk')"
      , "pm.update(0.016)"
      , "assert(pm.dump().zoom.multiplier == held, 'animation change preserves')"
      , "assetBrowserStub.selectEntry(1, 'idle')"
      , "pm.update(0.016)"
      , "local current = pm.dump().playback.direction"
      , "local other = nil"
      , "for _, dd in ipairs(pm.dump().playback.directions) do"
      , "    if dd.direction ~= current then other = dd end"
      , "end"
      , "assert(other, 'this animation has a second direction')"
      , "pm.onPreviewDirectionClick(other.handle)"
      , "pm.update(0.016)"
      , "assert(pm.dump().playback.direction == other.direction,"
      , "    'the direction really changed')"
      , "assert(pm.dump().zoom.multiplier == held, 'direction change preserves')"
      , "NOW = 5"
      , "pm.update(0.016)"
      , "assert(pm.dump().zoom.multiplier == held, 'playback preserves')"
      , "local beforeRegion = pm.dump().zoom.region"
      , "pm.onFramebufferResize(1500, 1150)"
      , "pm.update(0.016)"
      , "local after = pm.dump()"
      , "assert(after.zoom.multiplier == held, 'resize preserves')"
      , "assert(after.zoom.region.height ~= beforeRegion.height,"
      , "    'while the region was recomputed')"
      , "assertContained('unit after resize', after.zoom.sprite, after.zoom.region)"
      ]

    it "leaves the direction-row cells at their existing fixed sizing" $ runsOk $ lns
      [ harness
      , unitBrowse
      , "local d = pm.dump()"
      , "local before = {}"
      , "for _, dd in ipairs(d.playback.directions) do"
      , "    before[dd.direction] = dd.bounds"
      , "end"
      , "for _ = 1, 20 do pm.onUIScroll(d.zoom.surface, 0, 1) end"
      , "pm.update(0.016)"
      , "assert(pm.dump().zoom.multiplier == pz.MIN, 'the enlarged view did zoom')"
      , "for _, dd in ipairs(pm.dump().playback.directions) do"
      , "    assert(dd.bounds.w == before[dd.direction].w"
      , "        and dd.bounds.h == before[dd.direction].h,"
      , "        'cell ' .. dd.direction .. ' must not zoom')"
      , "end"
      ]

  describe "buildings viewer" $ do
    it "zooms the panel sprite and preserves the multiplier across an \
       \entry change -- a building is ONE preview object" $ runsOk $ lns
      [ harness
      , buildingBrowse
      , "local d = pm.dump()"
      , "assert(d.mode == 'building', tostring(d.mode))"
      , "assert(approx(d.zoom.region.width, d.panelBounds.width),"
      , "    'the buildings viewer zooms over the whole panel')"
      , "local fitted = d.zoom.sprite"
      , "pm.onUIScroll(d.zoom.surface, 0, 2)"
      , "pm.update(0.016)"
      , "local held = pm.dump().zoom.multiplier"
      , "assert(held < pz.MAX and pm.dump().zoom.sprite.w < fitted.w)"
      , "assertContained('building', pm.dump().zoom.sprite, pm.dump().zoom.region)"
      , "assertCentered('building', pm.dump().zoom.sprite, pm.dump().zoom.region)"
      , "assetBrowserStub.selectEntry(1, 'default.png')"
      , "pm.update(0.016)"
      , "local after = pm.dump()"
      , "assert(after.playback == nil, 'a static entry exposes no playback')"
      , "assert(after.zoom.multiplier == held,"
      , "    'a static/animated entry change preserves zoom')"
      , "assert(after.zoom.sprite, 'and a static entry still reports its bounds')"
      ]

    it "preserves the multiplier across playback and a resize" $ runsOk $ lns
      [ harness
      , buildingBrowse
      , "pm.onUIScroll(pm.dump().zoom.surface, 0, 2)"
      , "pm.update(0.016)"
      , "local held = pm.dump().zoom.multiplier"
      , "local frameBefore = pm.dump().playback.frameIndex"
      , "-- fps 8 over two frames: 5.125s lands on frame 1, so the clip"
      , "-- genuinely advances rather than landing back on frame 0."
      , "NOW = 5.125"
      , "pm.update(0.016)"
      , "assert(pm.dump().playback.frameIndex ~= frameBefore,"
      , "    'the clip really did advance, so this proves something')"
      , "assert(pm.dump().zoom.multiplier == held, 'playback preserves')"
      , "local before = pm.dump().zoom"
      , "pm.onFramebufferResize(1500, 1150)"
      , "pm.update(0.016)"
      , "local after = pm.dump()"
      , "assert(after.zoom.multiplier == held, 'resize preserves')"
      , "assert(after.zoom.region.width ~= before.region.width,"
      , "    'while the region was recomputed')"
      , "assertContained('building after resize', after.zoom.sprite,"
      , "    after.zoom.region)"
      ]

  describe "session lifecycle" $
    it "zoom is session state: a new session starts at 1, and a torn-down \
       \one leaves nothing behind" $ runsOk $ lns
      [ harness
      , bareList
      , "pm.onUIScroll(pm.dump().zoom.surface, 0, 3)"
      , "local surface = pm.dump().zoom.surface"
      , "assert(pm.dump().zoom.multiplier < pz.MAX)"
      , "pm.shutdown()"
      , "assert(elements[surface] == nil, 'the surface element was deleted')"
      , "local after = pm.dump()"
      , "assert(after.zoom.multiplier == pz.MAX,"
      , "    'zoom is not carried into the next session, got '"
      , "    .. tostring(after.zoom.multiplier))"
      , "assert(after.zoom.surface == nil, 'and no stale surface handle')"
      ]
