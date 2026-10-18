# Gameplay tags from Lua: the `gameplayTags` reference

Status: #2706 (SCN-06 of epic #2698). This is the script-author reference
for the `gameplayTags` namespace. The engine-side contract (owners,
lifetimes, persistence, query semantics) is
[gameplay_tags.md](gameplay_tags.md); this page does not repeat it, and
where the two meet, that page is the authority.

Gameplay tags are optional labels on units, buildings, locations, plants,
physical items and terrain cells. A tag means nothing to the engine. Your
script decides what `meeting_point` or `defender` means. Tags do not
follow an object anywhere, and they have nothing to do with faction tags:
`faction` (#2551) decides who fights whom, and `gameplayTags` never touches
it.

The executable examples are
[`scripts/gameplay_tag_examples.lua`](../scripts/gameplay_tag_examples.lua).
The Hspec group `Lua gameplay tags` runs every one of them against the
production API, so that file, not a copy on this page, is what is proven.

## The verbs

| Verb | Answers |
|---|---|
| `gameplayTags.add(target, tags)` | `true` |
| `gameplayTags.remove(target, tags)` | `true` |
| `gameplayTags.replace(target, tags)` | `true` |
| `gameplayTags.clear(target)` | `true` |
| `gameplayTags.tagsOf(target)` | a list of tag strings |
| `gameplayTags.has(target, tag)` | a boolean |
| `gameplayTags.query(expr [, options])` | a list of targets |
| `gameplayTags.count(expr [, options])` | an integer |
| `gameplayTags.exists(expr [, options])` | a boolean |

Every verb answers a refusal as `nil, message`, where `message` is
`"gameplayTags.<verb>: <reason>"`. No verb raises a Lua error for a bad
request or a missing target, so you can tell a refusal apart from an empty
list, `0` or `false` without `pcall`:

```lua
local ok, err = gameplayTags.add(target, "defender")
if not ok then log(err) end
```

A refusal changes nothing. A malformed request is refused before anything
is mutated or evaluated, and the namespace and the Lua stack are ready for
the next call.

## Targets

A target is a table with a `kind` and exactly that kind's fields. Any
other field is refused.

| Kind | Fields | Identity |
|---|---|---|
| `unit` | `id` | the unit id |
| `building` | `id` | the building id |
| `location` | `id`, `page` | the placed location's instance id on that page |
| `item` | `id` | the physical item's instance id, wherever it is |
| `plant` | `id`, `page` | a real flora occurrence (a wild plant or a planted row crop) |
| `plot` | `x`, `y`, `page` | a groundcover crop plot, by its tile |
| `tile` | `x`, `y`, `z`, `page` | one terrain cell |

- **Categories.** Every kind is its own category except `plot`, which
  belongs to the `plant` category beside the occurrences. A plot, the
  plants standing on a tile and the cells under them are all different
  targets, and unit 1, building 1, item 1 and location 1 are four.
- **Items.** An item is addressed the same way whether it is carried,
  nested inside another item at any depth, equipped, worn, stored in a
  building, delivered to a building, or lying on the ground. Moving it
  does not change its identity or its tags. Two items of the same
  definition are two targets.
- **Pages.** `page` is the page id string the world verbs use. If you
  leave it out, the target is on the active page, exactly as
  `world.getTerrainAt(gx, gy [, pageId])` defaults. Units, buildings and
  items take no `page`, because their ids are not per page.
- **Coordinates.** `x`, `y` and `z` are global tile coordinates. A
  horizontal seam alias of a tile names the same cell or plot, and
  results always carry the canonical coordinates.
- **Returned targets.** A target that a query returns is a fresh table in
  the same encoding, with `page` always filled in for page-scoped kinds.
  You can pass it straight back to any verb, whatever page is active by
  then. Editing a returned table changes nothing in the engine.

### Integers

Ids and coordinates must be numbers, and they are never converted from
strings: `"12"` is refused.

- An integer-subtype Lua number is used exactly, at its full 64-bit value.
  This is how flora occurrence ids and item ids above 2^53 cross unchanged.
  Keep them as integers in your scripts (for example, never divide them).
- A float is accepted only when it is finite, a whole number, and at most
  2^53 in magnitude. `12.0` is unit 12; `1.5`, `0/0`, `math.huge` and
  `2.0^60` are refused.
- Ranges: unit and building ids are `0 .. 4294967295`; item ids are
  `1 .. 2^63 - 1`; a plant id must be a real occurrence id; `x`, `y` and
  `z` are 32-bit (`-2^31 .. 2^31 - 1`).

## Tags

A tag is any nonempty, valid UTF-8 string. Spelling carries no meaning.
`tags` may be one string or a list of strings, and a repeated tag counts
once. The returned tag list from `tagsOf` is sorted by code point.

| Verb | Effect |
|---|---|
| `add` | Adds the tags. Adding a tag the target already has does nothing. |
| `remove` | Removes the tags. Removing a tag the target does not have does nothing. |
| `replace` | Makes the list the target's complete set. `replace(t, {})` clears. |
| `clear` | Removes every tag from the target. |

A tag shared by many targets stays on the others when one target loses
it. A target with no tags has no entry at all.

## What a target must be: loaded and existing

Every verb works on objects that exist and are loaded now:

- Units, buildings, locations and items must exist.
- A tile must be a stored, non-air cell of a loaded chunk. Underground
  cells count once their chunk is loaded. Air, depths outside a column's
  stored range, and cells of chunks that are not loaded are refused.
- A plant must stand in a loaded chunk, and a plot must exist on a loaded
  chunk.

Nothing is generated or loaded to satisfy a request. A refused request
never creates an assignment.

**Absence is not destruction.** When a chunk unloads, its cells' and
plants' tags are kept but are invisible: the verbs refuse those targets
and queries do not return them. When the chunk loads again they are back.
So a target missing from a query result may only be unloaded. Tags are
lost only when the object really goes: a unit or building destroyed, an
item consumed or destroyed, a cell dug away (the cell it exposes keeps
its own tags and inherits none), a plant removed, a plot harvested.

Tags never move by themselves. If your meeting point is dug away, or you
want a new one, your script moves the tag (see `moveMeetingPoint` in the
examples).

## Completion

When an edit verb returns `true`, the edit has been applied. The next
read sees it, from the same script chunk, and so does the next ordinary
save.

- **Units, buildings, locations and items** are edited inside the call,
  with one atomic step on the owner.
- **Tiles, plants and plots** are edited by the world thread, the only
  writer of the terrain they live in. The verb queues the edit and waits
  for the world thread's answer. When the world thread applies it, it
  checks again that the page is the same incarnation the verb resolved.
  The verb reads that incarnation before it derives anything from the
  page, such as canonical coordinates. If the page was destroyed or
  re-initialised at any point after that read, or a load replaced the
  session, the edit is refused (`nil, message`) and never reaches the
  replacement.

Scripts never need to poll or wait.

**Saves and loads.** While a save or load is capturing the session,
every edit verb is refused immediately with `nil, message`. That window
runs from the moment the transaction begins until its snapshot is taken,
and it includes the Lua save and load callbacks that run inside it. An
edit there would race the snapshot, and a tile or plant edit would wait
on a world thread that is parked for the capture. Reads and queries are
not refused. Once the snapshot is taken, edits are accepted again. Lua
is the only thread that begins a save or load, so none can begin while
an edit verb is running.

## Queries

```lua
gameplayTags.query(expr [, { categories = ..., page = ... }])
```

- `categories` is one category name or a nonempty list of them: `unit`,
  `building`, `location`, `plant`, `item`, `tile`. Omitted means all six.
- `page` is the page to search. Omitted means the active page. A unit,
  building or item is on the page it stands on, is stored on, or is
  carried on now.
- The universe is every live object of the selected categories on that
  page, tagged or not. A tile query ranges over every loaded non-air
  cell.

### Expressions

An expression is a table. Each node takes exactly one form:

| Form | Matches |
|---|---|
| `{ tag = "t" }` | targets carrying `t` |
| `{ all = tags, any = tags, none = tags }` | targets with every `all` tag, at least one `any` tag, and no `none` tag; any nonempty subset of the three keys |
| `{ universe = true }` | the whole selected universe |
| `{ union = { e1, e2, ... } }` | in any operand (one or more operands) |
| `{ intersection = { e1, e2, ... } }` | in every operand (one or more operands) |
| `{ difference = { a, b } }` | in `a` but not in `b` (exactly two operands) |
| `{ complement = e }` | in the selected universe but not in `e` |

- An empty group in `all`, `any` or `none` imposes no restriction, so
  `{ all = {} }` matches the whole universe.
- A tag no target carries is simply an empty set.
- A complement is taken against the selected universe, so untagged
  objects appear in negative results.
- A tag held by a target outside the universe, such as an unloaded cell,
  never appears in any result.
- The whole expression is evaluated natively in one consistent view.
  There is no string syntax, no intermediate list, no truncation and no
  cache.

Decoding limits, each refused with a diagnostic and never by silently
changing the expression:

- **Depth.** At most 64 nested levels.
- **Size.** At most 65 536 visited elements. Each node, and each entry of
  an `all`/`any`/`none` list, counts as it is reached, before repeats are
  merged. A table shared between operands counts once per use. Decoding
  stops at the bound, so an oversized list is never fully read.
- **Cycles.** A table that contains itself, directly or through its
  operand list, is refused. Table identity is compared along the current
  path, so sharing one subexpression between two operands is fine.

Arguments are read raw. No metamethod (`__index`, `__len`, `__pairs`, …)
runs while a request is decoded.

### Result modes and order

| Mode | Empty result | Otherwise |
|---|---|---|
| `query` | `{}` | every match, complete and without duplicates |
| `count` | `0` | the number of matches |
| `exists` | `false` | `true` |

`count` and `exists` take the size of the native result and push no
target, so they are the cheap way to ask "how many" or "any". All three
modes use the same expression semantics.

A `query` list is ordered by category (`unit`, `building`, `location`,
`plant`, `item`, `tile`), then within a category by:

- units, buildings and items: id;
- locations: page, then id;
- plants: page, with occurrences (by id) before plots (by `x`, `y`);
- tiles: page, `x`, `y`, `z`.

## Examples

All in [`scripts/gameplay_tag_examples.lua`](../scripts/gameplay_tag_examples.lua):

- `markMeetingPoint(pageId, x, y, z)` tags one terrain cell
  `meeting_point` and finds every meeting point on the page.
- `rangedDefenders(pageId)` selects units that are `defender` and
  `ranged`, minus the `reserve`, with `intersection` and `difference`.
- `rangedDefendersShorthand(pageId)` makes the same selection with
  `{ all = { "defender", "ranged" }, none = { "reserve" } }`.
- `nonDefenders(pageId)` uses `complement` to select every unit that is
  not a defender, including units with no tags at all.
- `rangedDefenderCount(pageId)` gives the same selection's `count` and
  `exists`.
- `moveMeetingPoint(from, to)` is a script relocating a tag: tags never
  move by themselves.

## Console use

The verbs are ordinary registered verbs, so the debug console and the
headless socket console call them like any other. The in-game console
exposes the namespace too.
`tools/gameplay_tag_persistence_probe.py` drives them over the socket
console through save, fresh-process load and autosave.

## Gates

- `cabal test synarchy-test-headless --test-options='--match "Lua gameplay tags"'`
  covers every verb and target form, identities and seam aliases, the
  query algebra, count and exists against a counting marshalling seam,
  residency, destruction, stale queued work, every refusal, detached
  results, faction isolation, and the examples file.
- `python3 tools/run_probes.py --only gameplay_tag_persistence_probe`
  covers script-made assignments across fresh processes, the arena and
  the autosave path, and refusal while a chunk is absent.
- `python3 tools/lua_registration_audit.py` certifies the registrar and
  checks the examples file's verb names.
