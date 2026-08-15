-- Offline regression harness for issue #618's requirement 3, retargeted by
-- #1157 onto the ONE production helper it now guards:
-- `textWrap.truncateToWidth` in scripts/ui/text_wrap.lua.
--
-- Lua strings are byte arrays -- string.sub cuts by byte, not codepoint --
-- so a pixel-width binary search over raw byte offsets can land its cut
-- inside a multi-byte UTF-8 sequence (e.g. right after the 0xC3 lead byte
-- of "e-acute"), producing a candidate that ends in a dangling lead byte
-- with no continuation byte. #618 fixed that by snapping every candidate
-- cut point to a full character boundary via scripts/ui/utf8_safe.lua.
--
-- #618 fixed five private copies of that search (scripts/popup.lua,
-- event_log.lua, unit_info_v2_inventory.lua, item_contents_panel.lua and
-- cargo_inventory_panel.lua) the same way, and this harness used to
-- REIMPLEMENT the popup/event-log copy in order to test the shape. There
-- is now exactly one implementation -- #1088 merged the three inventory
-- panels into scripts/ui/item_list.lua, #1107 moved the body into
-- scripts/ui/text_wrap.lua, and #1157 retired the popup's and the event
-- log's last two private copies -- so Part 2 below calls that production
-- function directly instead. A reimplementation could only ever have
-- tested itself.
--
-- Part 1 still unit-tests utf8_safe.snapToCharBoundary directly against
-- known multi-byte sequences, since that primitive is what the boundary
-- safety rests on.
--
-- Run from the repo root: luajit tools/test_utf8_safe_truncate.lua
package.path = "./?.lua;" .. package.path
local utf8Safe = require("scripts.ui.utf8_safe")

local failures = 0
local function assert_eq(a, b, msg)
    if a ~= b then
        failures = failures + 1
        print("FAIL: " .. msg .. " (got " .. tostring(a) .. ", want " .. tostring(b) .. ")")
    else
        print("ok: " .. msg)
    end
end

local function assert_true(cond, msg)
    if not cond then
        failures = failures + 1
        print("FAIL: " .. msg)
    else
        print("ok: " .. msg)
    end
end

-- Byte-level UTF-8 validity check: walks the string, and for any
-- multi-byte lead byte, requires the full run of continuation bytes to
-- be present. Returns false at exactly the "dangling lead byte" shape
-- this issue's bug produces (a lead byte with a missing/short
-- continuation run, whether from stream truncation or a bad string.sub
-- cut point).
local function isValidUtf8(s)
    local i, n = 1, #s
    while i <= n do
        local b = s:byte(i)
        local extra
        if b < 0x80 then extra = 0
        elseif b >= 0xC0 and b < 0xE0 then extra = 1
        elseif b >= 0xE0 and b < 0xF0 then extra = 2
        elseif b >= 0xF0 and b < 0xF8 then extra = 3
        else return false end
        for k = 1, extra do
            local cb = s:byte(i + k)
            if not cb or cb < 0x80 or cb > 0xBF then return false end
        end
        i = i + extra + 1
    end
    return true
end

-----------------------------------------------------------
-- Part 1: snapToCharBoundary unit tests
-----------------------------------------------------------

-- "cafe" -- pure ASCII, every byte offset is already a character
-- boundary, so snapping must be a no-op at every length.
local ascii = "cafe"
for n = 0, #ascii do
    assert_eq(utf8Safe.snapToCharBoundary(ascii, n), n,
        "ASCII snap is a no-op at n=" .. n)
end

-- "caf\xC3\xA9" == "café": the trailing 2-byte sequence (0xC3 0xA9) is
-- "é". Cutting at n=4 (right after the 0xC3 lead byte) is exactly the
-- dangling-lead-byte shape from the issue; it must snap back to n=3.
local cafe_accented = "caf\195\169"
assert_eq(#cafe_accented, 5, "café is 5 bytes (3 ASCII + 2-byte é)")
assert_eq(utf8Safe.snapToCharBoundary(cafe_accented, 3), 3, "snap at a real boundary (before é) is a no-op")
assert_eq(utf8Safe.snapToCharBoundary(cafe_accented, 4), 3, "snap mid-é (after lead byte) backs off to before é")
assert_eq(utf8Safe.snapToCharBoundary(cafe_accented, 5), 5, "snap at the full string (after é completes) is a no-op")

-- A 3-byte sequence (e.g. a CJK character, 0xE4 0xB8 0xAD == "中") and a
-- 4-byte sequence (an emoji, 0xF0 0x9F 0x8E 0x89 == "🎉"), each preceded
-- by one ASCII byte so there's a real boundary to snap back to.
local cjk = "x\228\184\173"        -- "x中"
assert_eq(#cjk, 4, "x + 3-byte CJK char is 4 bytes")
assert_eq(utf8Safe.snapToCharBoundary(cjk, 2), 1, "snap 1 byte into a 3-byte char backs off fully")
assert_eq(utf8Safe.snapToCharBoundary(cjk, 3), 1, "snap 2 bytes into a 3-byte char backs off fully")
assert_eq(utf8Safe.snapToCharBoundary(cjk, 4), 4, "snap at the full char is a no-op")

local emoji = "y\240\159\142\137" -- "y🎉"
assert_eq(#emoji, 5, "y + 4-byte emoji is 5 bytes")
for n = 2, 4 do
    assert_eq(utf8Safe.snapToCharBoundary(emoji, n), 1,
        "snap partway into a 4-byte char backs off fully at n=" .. n)
end
assert_eq(utf8Safe.snapToCharBoundary(emoji, 5), 5, "snap at the full emoji is a no-op")

-- Never goes negative / below 0 even when the very first character is
-- multi-byte with no ASCII prefix to fall back on.
local leadOnly = "\195\169" -- "é" alone
assert_eq(utf8Safe.snapToCharBoundary(leadOnly, 1), 0, "snap mid-first-char with no prefix floors at 0")
assert_eq(utf8Safe.snapToCharBoundary(leadOnly, 0), 0, "snap at 0 is a no-op")

-----------------------------------------------------------
-- Part 2: the REAL shared helper (scripts/ui/text_wrap.lua), driven
-- through a stubbed engine.getTextWidth. Byte length stands in for pixel
-- width: monotonic in prefix length, which is all the binary search's
-- correctness depends on. Every measured argument is recorded, so the
-- boundary check covers each intermediate candidate rather than only the
-- returned string.
-----------------------------------------------------------

local measured = {}
_G.engine = _G.engine or {}
engine.getTextWidth = function(_, s, _)
    measured[#measured + 1] = s
    return #s
end

local textWrap = require("scripts.ui.text_wrap")
local ELLIPSIS = ".."   -- the one form, chosen by #1157

local function truncate(text, maxWidthPx)
    return textWrap.truncateToWidth(text, 1, 10, maxWidthPx)
end

-- Mixed ASCII + accented + CJK + emoji, long enough to truncate at many
-- different byte budgets.
local mixed = "cafe caf\195\169 x\228\184\173 y\240\159\142\137 more plain text after"

local sawShorterThanFull = false
local sawEllipsisOnly = false
for maxW = 0, #mixed + 4 do
    measured = {}
    local result = truncate(mixed, maxW)
    if #result < #mixed then sawShorterThanFull = true end
    if result == ELLIPSIS then sawEllipsisOnly = true end
    if not isValidUtf8(result) then
        failures = failures + 1
        print(string.format("FAIL: final result invalid UTF-8 at maxW=%d: %q", maxW, result))
    end
    for _, candidate in ipairs(measured) do
        if not isValidUtf8(candidate) then
            failures = failures + 1
            print(string.format("FAIL: measured candidate invalid UTF-8 at maxW=%d: %q", maxW, candidate))
        end
    end
    -- The result must actually fit the budget it was given (an empty
    -- result is the defined answer when even the ellipsis does not).
    if #result > maxW then
        failures = failures + 1
        print(string.format("FAIL: result wider than maxW=%d: %q", maxW, result))
    end
end
assert_true(sawShorterThanFull, "the sweep actually exercised truncation (not just the already-fits branch)")
assert_true(sawEllipsisOnly, "the sweep reached the width where only the ellipsis fits")
print("ok: every measured candidate + final result across the maxWidthPx sweep is valid UTF-8")
print("ok: no result across the sweep exceeds its own maxWidthPx")

-- The bug #618 fixed is real: the same binary search WITHOUT the boundary
-- snap produces invalid UTF-8 somewhere in the same sweep. This is the
-- only reimplementation left, and it exists to fail, not to stand in for
-- production code.
local function truncateUnsnapped(text, maxWidthPx)
    if #text <= maxWidthPx then return text end
    local lo, hi = 0, #text
    while lo < hi do
        local mid = math.floor((lo + hi + 1) / 2)
        local candidate = string.sub(text, 1, mid) .. ELLIPSIS
        if #candidate <= maxWidthPx then lo = mid else hi = mid - 1 end
    end
    return string.sub(text, 1, lo) .. ELLIPSIS
end

local naiveBroke = false
for maxW = 4, #mixed + 4 do
    if not isValidUtf8(truncateUnsnapped(mixed, maxW)) then naiveBroke = true end
end
assert_true(naiveBroke, "the unsnapped algorithm does produce invalid UTF-8 somewhere in the sweep")

-- Pure-ASCII text is byte-identical between the production helper and the
-- unsnapped shape at every width, since snapping is a no-op for ASCII:
-- boundary safety costs nothing for well-formed single-byte text.
local asciiText = "the quick brown fox jumps over the lazy dog"
for maxW = 4, #asciiText + 4 do
    assert_eq(truncate(asciiText, maxW), truncateUnsnapped(asciiText, maxW),
        "ASCII output matches the unsnapped shape at maxW=" .. maxW)
end

-- The defensive contract #1157 settled, at the production helper's own
-- boundaries. ".." is 2 wide under this stub.
assert_eq(truncate(nil, 100), nil, "nil text returns nil")
assert_eq(truncate("", 100), "", "empty text returns empty")
assert_eq(truncate("abcdef", 0), "", "a zero budget drops the field")
assert_eq(truncate("abcdef", -50), "", "a negative budget drops the field")
assert_eq(textWrap.truncateToWidth("abcdef", 1, 10, nil), "abcdef",
    "a nil budget is no bound at all")
assert_eq(truncate("abcdef", 1), "", "a budget too narrow for the ellipsis drops the field")
assert_eq(truncate("abcdef", 2), ELLIPSIS, "a budget fitting only the ellipsis returns it alone")
assert_eq(truncate("abcdef", 3), "a" .. ELLIPSIS, "one character plus the ellipsis")
assert_eq(truncate("abc", 3), "abc", "text that already fits is returned unchanged")

if failures == 0 then
    print("\nALL PASS")
    os.exit(0)
else
    print("\n" .. failures .. " FAILURE(S)")
    os.exit(1)
end
