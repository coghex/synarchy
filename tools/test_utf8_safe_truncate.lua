-- Offline regression harness for issue #618's requirement 3: the five
-- `truncateToWidth` ellipsis helpers (scripts/popup.lua, event_log.lua,
-- unit_info_v2_inventory.lua, item_contents_panel.lua,
-- cargo_inventory_panel.lua) binary-search a pixel-width cut point over
-- RAW BYTE offsets. Lua strings are byte arrays -- string.sub cuts by
-- byte, not codepoint -- so an unguarded search can land its cut point
-- inside a multi-byte UTF-8 sequence (e.g. right after the 0xC3 lead
-- byte of "é"), producing a candidate that ends in a dangling lead byte
-- with no continuation byte. All five helpers were fixed the same way:
-- snap every candidate cut point to a full character boundary via the
-- shared scripts/ui/utf8_safe.lua module before building or measuring a
-- substring. Per the issue's own acceptance text, testing that shared
-- helper (used identically by all five call sites) covers all of them.
--
-- This harness (a) unit-tests the real utf8_safe.snapToCharBoundary
-- directly against known multi-byte sequences, and (b) reimplements the
-- exact binary-search shape used by popup.lua/event_log.lua (the other
-- three files use the same snap primitive with a different loop
-- invariant -- see CLAUDE.md's requirement-3 note) against the REAL
-- utf8Safe module, asserting every candidate string constructed during
-- the search -- not just the final result -- decodes as valid UTF-8, and
-- that pure-ASCII output is byte-identical to the pre-fix (unsnapped)
-- algorithm, proving no behavior change for well-formed non-multi-byte
-- text (requirement 4).
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
-- Part 2: the real production binary-search shape (popup.lua /
-- event_log.lua's algorithm, reimplemented here verbatim except for a
-- stubbed getWidth) against the REAL utf8Safe module.
-----------------------------------------------------------

local function truncateToWidthSafe(text, getWidth, maxWidthPx, measured)
    if getWidth(text) <= maxWidthPx then return text end
    local lo, hi = 1, #text
    while lo < hi do
        local mid = math.floor((lo + hi + 1) / 2)
        local cut = utf8Safe.snapToCharBoundary(text, mid)
        local candidate = string.sub(text, 1, cut) .. "..."
        if measured then measured[#measured + 1] = candidate end
        if getWidth(candidate) <= maxWidthPx then lo = mid else hi = mid - 1 end
    end
    local final = string.sub(text, 1, utf8Safe.snapToCharBoundary(text, lo)) .. "..."
    if measured then measured[#measured + 1] = final end
    return final
end

-- Pre-fix shape (no snapping) -- used only as a reference to show the
-- bug is real and to prove the fix is behavior-preserving for ASCII.
local function truncateToWidthNaive(text, getWidth, maxWidthPx)
    if getWidth(text) <= maxWidthPx then return text end
    local lo, hi = 1, #text
    while lo < hi do
        local mid = math.floor((lo + hi + 1) / 2)
        local candidate = string.sub(text, 1, mid) .. "..."
        if getWidth(candidate) <= maxWidthPx then lo = mid else hi = mid - 1 end
    end
    return string.sub(text, 1, lo) .. "..."
end

-- Byte length stands in for pixel width: monotonic in prefix length,
-- which is all the binary search's correctness depends on.
local function byteWidth(s) return #s end

-- Mixed ASCII + accented + CJK + emoji, long enough to truncate at many
-- different byte budgets.
local mixed = "cafe caf\195\169 x\228\184\173 y\240\159\142\137 more plain text after"

local sawShorterThanFull = false
for maxW = 4, #mixed + 4 do
    local measured = {}
    local result = truncateToWidthSafe(mixed, byteWidth, maxW, measured)
    if #result < #mixed then sawShorterThanFull = true end
    if not isValidUtf8(result) then
        failures = failures + 1
        print(string.format("FAIL: final result invalid UTF-8 at maxW=%d: %q", maxW, result))
    end
    for _, candidate in ipairs(measured) do
        if not isValidUtf8(candidate) then
            failures = failures + 1
            print(string.format("FAIL: mid-search candidate invalid UTF-8 at maxW=%d: %q", maxW, candidate))
        end
    end
end
assert_true(sawShorterThanFull, "the sweep actually exercised truncation (not just the already-fits branch)")
print("ok: every candidate + final result across the maxWidthPx sweep is valid UTF-8")

-- Prove the bug is real: the naive (unsnapped) version produces invalid
-- UTF-8 somewhere in the same sweep -- this is what requirement 3 fixes.
local naiveBroke = false
for maxW = 4, #mixed + 4 do
    local result = truncateToWidthNaive(mixed, byteWidth, maxW)
    if not isValidUtf8(result) then naiveBroke = true end
end
assert_true(naiveBroke, "the pre-fix (unsnapped) algorithm does produce invalid UTF-8 somewhere in the sweep")

-- Requirement 4: pure-ASCII text is byte-identical between the safe and
-- naive algorithms at every width, since snapping is a no-op for ASCII.
local asciiText = "the quick brown fox jumps over the lazy dog"
for maxW = 4, #asciiText + 4 do
    local safe = truncateToWidthSafe(asciiText, byteWidth, maxW)
    local naive = truncateToWidthNaive(asciiText, byteWidth, maxW)
    assert_eq(safe, naive, "ASCII output unchanged at maxW=" .. maxW)
end

if failures == 0 then
    print("\nALL PASS")
    os.exit(0)
else
    print("\n" .. failures .. " FAILURE(S)")
    os.exit(1)
end
