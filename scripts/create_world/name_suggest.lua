-- World-name suggestion state for the Create World screen (#1106).
--
-- The dice button beside World Name used to concatenate three fixed
-- English-ish word lists with math.random. It now asks the engine for a
-- name in the world seed's OWN generated language
-- (world.suggestName -> Language.Suggest), which produces a native name
-- AND its English gloss from one semantic expression, plus the #1092
-- provenance to record if the player keeps it.
--
-- This module owns the STATE that decision needs and nothing else -- no
-- widgets, no layout, no UI globals -- so the whole contract is
-- testable without booting a UI. settings_tab.lua wires the dice
-- button, the seed control, and typing to the four verbs below.
--
-- Everything lives on the caller's `pending` table (createWorldMenu.
-- pending), which outlives a responsive rebuild. That is what preserves
-- the suggestion/manual distinction, the gloss, the provenance, and the
-- reroll ordinal across a resize -- the randbox snapshot only preserves
-- text, cursor, and focus.
--
-- The central distinction: a name is either a live SUGGESTION (it came
-- from a language, so it has a gloss and provenance) or MANUAL (the
-- player typed it, so #708 principle 7 says it means nothing and gets
-- neither). `nameSuggested` is the only authority on which -- never a
-- comparison of the text against the last suggestion, because retyping
-- a rendered name by hand still makes it manual.
local nameSuggest = {}

-- A seed change re-suggests, and the fresh suggestion must be visibly
-- different from the one it replaces. Adjacent ordinals within one
-- language differ by construction (Language.Suggest), but ordinal 0 of
-- a DIFFERENT language is an independent draw that could in principle
-- land on the same rendered pair, so the sequence is advanced until it
-- doesn't. A handful of tries is far past the point of reachability.
local MAX_RESEED_TRIES = 8

-----------------------------------------------------------
-- Engine call
-----------------------------------------------------------

-- Returns suggestion|nil, errorMessage. world.suggestName itself
-- reports a failure as (nil, message); the pcall additionally contains
-- a hard error (no world table at all in a stripped profile) so a dice
-- press can never take down the menu.
local function fetch(seedNum, ordinal)
    local ok, sug, err = pcall(world.suggestName, seedNum, ordinal)
    if not ok then
        return nil, tostring(sug)
    end
    if type(sug) ~= "table" or type(sug.name) ~= "string" then
        return nil, err or "world.suggestName returned no suggestion"
    end
    return sug
end

local function apply(pending, sug, seedNum, nextOrdinal)
    pending.worldName          = sug.name
    -- The text this suggestion actually rendered, kept beside the name
    -- so `reconcile` can tell whether the field still holds it. Not a
    -- substitute for `nameSuggested` -- see reconcile for why both.
    pending.nameSuggestedText  = sug.name
    pending.nameGloss          = sug.gloss
    pending.nameLanguageSeed   = sug.language and sug.language.seed or nil
    pending.nameLanguageVersion = sug.language and sug.language.version or nil
    pending.nameSuggested      = true
    pending.nameOrdinal        = nextOrdinal
    pending.nameSeedNum        = seedNum
end

-----------------------------------------------------------
-- Verbs
-----------------------------------------------------------

-- The dice button: the next suggestion in this seed's sequence.
--
-- On failure the current name and metadata are left EXACTLY as they
-- were and the reason is reported. There is deliberately no fallback
-- generator: a name with no language behind it is what this issue
-- removed, and silently producing one again would be worse than
-- leaving the field alone.
function nameSuggest.suggest(pending, seedNum)
    local ordinal = pending.nameOrdinal or 0
    local sug, err = fetch(seedNum, ordinal)
    if not sug then
        engine.logWarn("Create World: could not suggest a world name: "
            .. tostring(err))
        return nil, err
    end
    apply(pending, sug, seedNum, ordinal + 1)
    return sug.name
end

-- The world seed changed (or was set for the first time).
--
-- Returns the new name when one was produced, nil otherwise. A MANUAL
-- name is never touched -- requirement 3 is explicit that changing the
-- seed leaves a name the player typed alone -- and neither is anything
-- else when the seed's numeric value did not actually change, which is
-- what keeps a rebuild's programmatic restore from being mistaken for
-- an edit (the restored seed text re-fires the same value).
function nameSuggest.reseed(pending, seedNum)
    local changed = pending.nameSeedNum ~= seedNum
    pending.nameSeedNum = seedNum
    if not changed then return nil end

    -- The sequence restarts on EVERY genuine seed change, whether or not
    -- the current name is still a suggestion. The ordinal indexes one
    -- language's sequence, so carrying it into a new language would drop
    -- the next dice press partway into a language the player has never
    -- heard a word of -- the opposite of "changing the seed gives a
    -- fresh suggestion". A manual name is still left untouched below.
    pending.nameOrdinal = 0
    if not pending.nameSuggested then return nil end

    local previousName  = pending.worldName
    local previousGloss = pending.nameGloss
    for _ = 1, MAX_RESEED_TRIES do
        local name = nameSuggest.suggest(pending, seedNum)
        if not name then return nil end
        if name ~= previousName or pending.nameGloss ~= previousGloss then
            return name
        end
    end
    return pending.worldName
end

-- The player typed, deleted, or otherwise mutated the name text.
--
-- Fires on the mutation itself, not on unfocus, so the gloss and
-- provenance are gone the moment the name stops being the language's
-- and becomes the player's. Retyping the identical rendered text counts
-- -- the distinction is authorship, not spelling.
--
-- The reroll ordinal deliberately survives: pressing the dice after
-- typing continues the sequence rather than replaying it.
function nameSuggest.clear(pending)
    pending.nameSuggested       = false
    pending.nameSuggestedText   = nil
    pending.nameGloss           = nil
    pending.nameLanguageSeed    = nil
    pending.nameLanguageVersion = nil
end

-- Forget everything, including the sequence position (Defaults).
function nameSuggest.reset(pending)
    nameSuggest.clear(pending)
    pending.nameOrdinal = nil
    pending.nameSeedNum = nil
end

-- Whether the World Name field accepts one character.
--
-- The admissible set is the GENERATOR's own
-- (`Language.Generated.Orthography.outputInventory`, via
-- `world.generatedNameCharacters`), not a character class written out
-- here: a suggested name carries extended-Latin letters (#1100) plus
-- the possessive apostrophe and a hyphen-joining language's separator,
-- and requirement 4's "type over the suggestion" is impossible if the
-- field rejects the letters it was just filled with. Asking the engine
-- also means the two cannot drift apart when the repertoire moves.
--
-- Resolved once and memoized -- this runs per keystroke. If the query
-- is unavailable the field falls back to plain ASCII letters and the
-- two marks, which is strictly better than accepting nothing.
local nameChars = nil

local function resolveNameChars()
    if nameChars then return nameChars end
    nameChars = {}
    local ok, repertoire = pcall(world.generatedNameCharacters)
    if ok and type(repertoire) == "string" and repertoire ~= "" then
        for _, cp in utf8.codes(repertoire) do
            nameChars[utf8.char(cp)] = true
        end
    else
        for c in ("abcdefghijklmnopqrstuvwxyz"
               .. "ABCDEFGHIJKLMNOPQRSTUVWXYZ-'"):gmatch(".") do
            nameChars[c] = true
        end
    end
    return nameChars
end

function nameSuggest.isNameChar(char)
    return resolveNameChars()[char] == true
end

-- Reconcile the recorded suggestion against the text the control now
-- holds, whoever put it there. Returns whether the name is still a
-- suggestion afterwards.
--
-- `clear` covers the player; this covers the other direction. A
-- PROGRAMMATIC set can put text in the field that a different
-- suggestion produced -- a resize teardown unfocuses an unsubmitted
-- seed edit, which re-suggests, and the rebuild's restoreAll then puts
-- the pre-teardown name back over the new suggestion's gloss and
-- provenance. Pairing a name with another expression's meaning would
-- persist an etymology that name never had, so the pairing is checked
-- rather than assumed.
--
-- This does NOT replace `clear` as the manual-edit rule: retyping the
-- suggested text by hand still clears, because the first keystroke of
-- that retyping already did.
function nameSuggest.reconcile(pending, text)
    pending.worldName = text
    if pending.nameSuggested and text ~= pending.nameSuggestedText then
        nameSuggest.clear(pending)
    end
    return pending.nameSuggested == true
end

-- The complete (seed, name, meaning) tuple, for a caller that destroys
-- and recreates the controls around it (a responsive rebuild). The
-- widgets' own snapshot carries text, cursor, and focus; this carries
-- the meaning, the language, and the sequence position, which no widget
-- knows about.
--
-- The COMMITTED seed is part of the tuple because the language is
-- derived from it: restoring a name without the seed it was suggested
-- for would leave the two describing different languages. The
-- distinction that matters is committed versus in-progress -- the raw
-- text a Seed control holds mid-edit belongs to the widget snapshot,
-- and `pending.seed` only catches up when the player submits it.
function nameSuggest.snapshot(pending)
    return {
        seed            = pending.seed,
        worldName       = pending.worldName,
        suggestedText   = pending.nameSuggestedText,
        gloss           = pending.nameGloss,
        languageSeed    = pending.nameLanguageSeed,
        languageVersion = pending.nameLanguageVersion,
        suggested       = pending.nameSuggested,
        ordinal         = pending.nameOrdinal,
        seedNum         = pending.nameSeedNum,
    }
end

function nameSuggest.restore(pending, snap)
    if not snap then return end
    -- Tearing the controls down unfocuses them, so an unsubmitted Seed
    -- edit SUBMITS on the way out and commits a seed the player never
    -- accepted. Putting the committed seed back with the name is what
    -- keeps the pair honest -- and leaves the edit still pending in the
    -- widget, so submitting it later re-suggests exactly as it would
    -- have without the rebuild.
    pending.seed                = snap.seed
    pending.worldName           = snap.worldName
    pending.nameSuggestedText   = snap.suggestedText
    pending.nameGloss           = snap.gloss
    pending.nameLanguageSeed    = snap.languageSeed
    pending.nameLanguageVersion = snap.languageVersion
    pending.nameSuggested       = snap.suggested
    pending.nameOrdinal         = snap.ordinal
    pending.nameSeedNum         = snap.seedNum
end

-----------------------------------------------------------
-- Queries
-----------------------------------------------------------

function nameSuggest.isSuggested(pending)
    return pending.nameSuggested == true
end

-- The gloss to display beside the name, or nil when there is none to
-- display. A manual name has no meaning to show.
function nameSuggest.gloss(pending)
    if not nameSuggest.isSuggested(pending) then return nil end
    local g = pending.nameGloss
    if type(g) ~= "string" or g == "" then return nil end
    return g
end

-- The trailing world.init identity arguments: gloss, language seed
-- (decimal STRING -- a language seed is unsigned 64-bit and Lua carries
-- neither an exact integer nor an exact double for the top of that
-- range), and generator version. All nil for a manual name, which is
-- what makes provenance recorded for suggested names ONLY.
function nameSuggest.identity(pending)
    if not nameSuggest.isSuggested(pending) then return nil, nil, nil end
    return pending.nameGloss,
           pending.nameLanguageSeed,
           pending.nameLanguageVersion
end

return nameSuggest
