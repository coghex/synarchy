-- | The stdlib-only Lua harness the @--preview@ pane specs share
--   (#1907, #2492).
--
--   CPU-only, and deliberately drives the REAL shipped Lua rather than a
--   Haskell restatement of its arithmetic:
--   @scripts/ui/preview_zoom.lua@,
--   @scripts/ui/unit_animation_view.lua@,
--   @scripts/ui/building_asset_view.lua@ and
--   @scripts/preview_manager.lua@ all run in a stdlib-only @HsLua@
--   interpreter — no engine, no GPU, no UI backend — with @engine@/@UI@
--   and the two modules that are NOT under test (@scripts.ui.list@,
--   @scripts.ui.asset_browser@) stubbed through @package.loaded@. That
--   is the pattern "Test.Headless.Lua.AssetFailure" already uses to
--   drive @preview_manager.lua@ headlessly.
--
--   Why the real modules matter: @tools/preview_probe.py@ is manual-only
--   and @needs-gpu@ (@tools/ci_probes.py@), so these groups are the only
--   BLOCKING automated gate the preview panes have. A test that
--   reimplemented the fit, the facing strip or the missing-cell
--   presentation would pass while the shipped pane was wrong.
--
--   The stub browser fires @onSelect@ exactly where the real one does
--   (@selectEntry@ fires it, @selectEntrySilently@ does not), because
--   that timing IS the object-identity reset rule: a resize restores
--   silently and therefore preserves the multiplier for free.
--
--   Lives in its own module because two specs need it verbatim: a
--   second copy would be free to drift, and a harness that differs
--   between two groups makes their results incomparable.
module Test.Headless.Preview.LuaHarness
  ( runsOk
  , lns
  , uiStub
  , engineStub
  , browserStub
  , harness
  ) where

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
    -- A faithful-enough stand-in for the real browser's dump: ONE row
    -- per entry, carrying the item's own `key` (its value, which is what
    -- selection speaks in) beside the drawn `label`, plus deterministic
    -- bounds. The key is the field a consumer needs to resolve a dumped
    -- row back to its data when two rows legitimately draw the same
    -- text, which is exactly what the buildings viewer's combined list
    -- does since #2492. Row GEOMETRY here is synthetic — the real
    -- browser is not under test in these groups; every bound this
    -- harness proves anything about (facing cells, the enlarged sprite)
    -- is read back from UI.getElementInfo on a real element.
    , "  dump = function(id)"
    , "      local b = browsers[id]"
    , "      if not b then return {} end"
    , "      local out = {}"
    , "      for i, e in ipairs(b.params.entries or {}) do"
    , "          out[i] = { key = e.path, label = e.label,"
    , "                     value = (e.path == b.selected),"
    , "                     bounds = { x = b.params.x, y = b.params.y + i * 32,"
    , "                                w = 200, h = 32 } }"
    , "      end"
    , "      return out"
    , "  end,"
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

