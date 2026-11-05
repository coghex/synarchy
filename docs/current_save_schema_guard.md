# Current-schema save guard

`tools/current_save_schema_audit.py` (#2916, PSP-2 of #2914) enforces the
pre-release save rule from
[prerelease_save_policy_design.md](designs/prerelease_save_policy_design.md)
(D-1, D-5, P-2): **a persisted shape change advances the affected carrier's
on-disk version, and that carrier then accepts only its current version.**
It never asks whether an old decoder could still be safe.

The guard is not wired into CI or `make ci` yet; PSP-5 (#2919) switches the
mandatory gates over to it and retires the strict enum audit with the other
historical machinery.

## Running it

```bash
python3 tools/current_save_schema_audit.py --self-test        # synthetic suite
python3 tools/current_save_schema_audit.py                    # record is fresh
python3 tools/current_save_schema_audit.py --base-ref origin/master
python3 tools/current_save_schema_audit.py --base-ref origin/master --json
python3 tools/current_save_schema_audit.py --update-record    # regenerate
```

* `--update-record` rewrites `docs/save_compat/current_schema.json` from
  source and nothing else. The record is a projection of the candidate; it
  is never the comparison input, so regenerating or deleting it cannot hide a
  same-version change. Commit it with the code (a tool reads it).
* `--base-ref REF` compares the candidate with the **merge-base** of `REF` and
  `HEAD`, both computed from source, so a target branch that moved on is not
  read as this change reverting it. The resolved base and candidate SHAs are
  printed (and in `--json`). A comparison of a commit with itself, with no
  uncommitted changes, fails, as does a base that cannot be resolved or read.
  The first run that introduces the record is still a real comparison.
* Exit 0 passes; 1 is a failed rule or missing evidence. Each failure names
  the carrier (and, for a shape change, the declarations that moved).

## What counts as a carrier

| Carrier | Version source | Shape |
|---|---|---|
| Haskell component | `csVersion`, `csOlderVersions` | records, enums, newtypes, aliases reachable from its `*DTO*` roots, plus hand-written `Serialize` instances on reached types |
| `metadata` | `metadataComponentVersion`, `metadataComponentInputVersions` | `SaveMetadata` and its closure |
| `lua.<id>` | `version`, `inputVersions` in `saveMods.register` | the shared `scripts/lib/data_codec.lua` (a Lua payload has no declared schema) |
| `framing` | `currentEnvelopeVersion` | `envelope_framing_fingerprint` |
| `session`, `lua-state` | retired | read only through the `b1-session` and `b2-lua-state` legacy routes |

A carrier's *legacy shape* is what only its still-accepted older versions
reach (the frozen DTOs its `csOlderVersions` migrations name, the
`MetadataV<n>` mirrors). Editing one while it is reachable fails until the
older version is removed; the current version need not move if the current
shape did not.

## The rules

1. A shape change, **including an enum append**, needs a strictly higher
   version for every carrier whose shape it touches. A shared declaration
   affects every carrier that reaches it.
2. Any carrier whose version rose is affected, whether or not the guard saw a
   shape change.
3. An affected carrier accepts exactly its current version.
4. Every enabled legacy route that decodes through an affected carrier must be
   disabled. A route is enabled while `tryLegacyEnvelopeFallbacks` or
   `tryLegacyMetadataFallbacks` still names its decoder; a metadata change
   affects both routes, a change to a type the frozen `SessionV90` DTOs reach
   affects `b1-session`, and `b2-lua-state` also decodes every *required*
   Haskell component of its era, so a version change in any of them affects
   it. A dispatcher may only call the decoders `LEGACY_ROUTES` names; any
   other call leaves the route's state unknown and fails.
5. A new carrier is covered and accepts only its current version, with no
   bump. A removed carrier must leave the record.
6. Unchanged carriers owe nothing. Removing registrations does not require
   deleting the old source or writing a migration or fixture.

Anything the guard cannot classify fails and names the boundary: a registered
codec with no definition, a codec with no resolvable DTO, a duplicate Lua
module id, a `saveMods.register` site that is not a quoted literal id plus
a table, a non-literal `inputVersions` entry, a framing decoder that does not
reject exactly `version ≢ expectedVersion` (or a caller that passes another
version), a type name declared in two modules (list it in
`AMBIGUOUS_TYPE_NAMES` with why a union is acceptable), a type that both
derives and hand-writes `Serialize`, a missing legacy dispatcher.

## What it cannot see

Source inspection has limits, and a green run is not proof the wire is
unchanged:

* **Custom serializers.** An instance is hashed as the text of its block.
  Helpers it calls elsewhere, and a change in meaning with identical text, are
  invisible.
* **Lua payloads.** Only the shared data codec is mechanical. A new `aiState`
  field in any `scripts/unit_ai_*.lua`, or any other Lua table shape, is not
  detected.
* **Positional values.** Two same-typed positional fields swapped, or a unit
  or encoding change on an unchanged type, leave no source diff.
* **Name resolution.** Types resolve by bare name; types from outside `src/`
  and `app/` are not followed.
* Comment, layout, strictness-bang and import-only edits are ignored on
  purpose.

An encoding or meaning change outside mechanical detection still needs a
version bump, current-only acceptance, explicit review evidence and a test
that distinguishes the old meaning from the new one.

## Maintaining it

The synthetic suite lives in `tools/current_save_schema_selftest.py` over the
tree in `tools/current_save_schema_fixture.py`; extend them with any new rule.
The model reuses `save_compat_audit_components` (component specs, Lua call
sites), `save_compat_audit_fingerprint` (framing fingerprint, metadata input
versions) and the enum audit's scanner; run `tools/test_save_compat_audit.py
--without-reproducibility` and `tools/enum_append_only_audit.py --self-test`
after touching any of them.
