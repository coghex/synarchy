# Current-schema save guard (PSP-2, #2916)

Before release, a persisted shape change advances every affected component,
metadata, Lua or framing version, and each affected carrier then accepts only
its current version with its affected exact-shape legacy routes disabled. No
historical-decoder compatibility analysis, new migration or historical fixture
is required. Design: `docs/designs/prerelease_save_policy_design.md` (P-2,
PSP-2, D-5, D-9). The guard is not wired into CI, `make ci` or any default
until PSP-5 (#2919).

## Commands

```bash
cabal build all
cabal build synarchy-test-headless
cabal test synarchy-test-headless --test-options='--match "current save schema"'
python3 tools/current_save_schema_audit.py --self-test
python3 tools/current_save_schema_audit.py --base-ref origin/master
python3 tools/current_save_schema_audit.py --update-record
```

`--update-record` rewrites `docs/save_compat/current_schema.json` from the
compiled evidence. It does not replace the independent `--base-ref` check:
regenerating, editing or deleting the record cannot bless same-version drift,
because the comparison reads the record committed at the base revision.

## Where each fact comes from

Nothing is parsed from source text.

| Fact | Evidence |
|---|---|
| Haskell component shapes | `componentCodec` demands a `WireSchema` instance for its DTO type `d` and keeps the compiled `WireRoot` on the codec (`ccWire`) and registry entry (`rcWire`) before the registry erases `d`. The walk is GHC `Generic`/`Typeable`: constructors in order, record selectors, newtypes, instantiated field types, GHC-resolved aliases. |
| Component versions and acceptance | `rcVersion`/`rcInputVers` from the live `saveComponentRegistry`, and each version PROBED through `rcPrepare` (the entry point `assembleSnapshot` runs). A disagreement fails extraction. |
| Metadata | `metadataWireRoot` (the type `encodeSessionSnapshot` encodes), `metadataComponentVersion`, and a probe of `decodeSaveEnvelopeMetadata` at every version. |
| Framing | `currentEnvelopeVersion`, `envelopeMagic`, `headerSize`, an fnv1a64 probe, and the hex of a fixed encoded envelope. The header bytes are hand written, so they are declared and probed, not derived. The cereal manifest is walked. |
| Lua modules | The production `scripts/init_loader.lua` runs under a recording `engine.loadScript`; every recorded script is loaded and its real `init` run; the evaluated `saveModules.registry` supplies id, version, `inputVersions` and `required`; each version is probed through `saveModules.prepareLoad`. A script that cannot load or initialise fails the run. `scripts/lib/data_codec.lua` is the unversioned wire owner every Lua carrier shares, recorded by a probe of its encoding. |
| Legacy routes | B1 (`session`) and B2 (`lua-state`) are probed through `decodeSaveEnvelopeMetadata`, `decodeSessionEnvelopeClassified` and the direct `decodeLegacy*`/`decodeB2*` entry points with correctly shaped envelopes. A route is enabled if any entry point recognises the shape. Ownership is conservative: B1 is owned by metadata and every Haskell carrier; B2 by metadata, `lua-state` and every required Haskell carrier. |

## Serialization boundaries

A `Generic` instance cannot tell whether a `Serialize` instance uses cereal's
generic encoding. Every reachable type is one of:

- **generic** (the default instance): its real `S.encode` is compared with
  cereal's own `GSerializePut` encoding on distinguishing sample values, and
  must round-trip. A hand-written encoder that disagrees fails extraction
  with `unclassified encoder`. This is evidence on samples, not a proof.
- **primitive / library**: cereal's base-type and container encoders, declared
  by explicit instances in `World.Save.Wire`.
- **newtype**: a newtype-derived instance, checked against its field's
  encoding.
- **custom**: a hand-written encoder declared in `World.Save.Wire.Custom` with
  an owner, a description of the bytes and the wire types it writes. Declared
  and tested (round trip on samples where samples exist), **not derived**. A
  type that is reachable but not declared, or declared but unreachable, fails.

A type with no `Generic` instance and no explicit instance is a compile error
at the `componentCodec` call site, so a new boundary cannot be added silently.

## Stated limits

- Custom encoders, Lua payloads (dynamic tables with no declared schema) and
  the framing header are covered by declaration and probes, not by a derived
  shape. An encoding or meaning change outside detection still requires every
  affected version to advance, current-only acceptance, review evidence and a
  distinguishing test. A refreshed record or self-generated expected output is
  not that evidence.
- A declaration reachable only through a still-accepted older decoder or an
  enabled legacy route is **not recorded** (for example `ToolMode`, which is on
  the wire only through B1). D-5/D-7 accept this residual; the unchanged strict
  `tools/enum_append_only_audit.py` still guards such enums until PSP-5
  retires it.
- Routine comparison never builds the base. The base record's freshness rests
  on the merge-time check PSP-5 enforces on every merged commit; this tool
  validates its structure and reports its blob identity.

## Rules

Per carrier: a shape change needs a strictly higher real version; an affected
carrier (shape changed or version higher) accepts exactly its current version
and every enabled legacy route it owns must be off; a version decrease fails; a
new carrier is covered and accepts only its current version. Unchanged carriers
need no purge, so removing one older registration alone is fine. A change to
the shared Lua codec probe or the framing probe affects every Lua carrier or
the framing.

## Base, candidate and bootstrap

Routine comparison reads the validated record committed at the merge-base of
`--base-ref` and `HEAD`, compiles only the candidate (always building first and
refusing a binary older than the newest source), and requires the committed
record to equal the compiled evidence exactly. A base that contains any guard
artifact must carry a valid record: a missing or malformed one fails and never
enables the bootstrap. Only a base containing none of the guard's artifacts
(first introduction) is extracted as well, by laying the candidate's additive
hooks onto an export of it; the adapter is refused if it would remove any base
line outside the declared hook rewrite. The run output states the resolved
commits, the base record's blob and validation provenance, the candidate's
tracked-edit identity and the extractor and toolchain identity. The record
itself carries none of that, so off-wire and comment-only edits leave it
byte-identical.
