# Current-schema save guard (PSP-5, #2919)

Before the first public release, a persisted shape change advances every
affected component, metadata, Lua or framing version, and each affected carrier then accepts only
its current version with its affected exact-shape legacy routes disabled. No
historical-decoder compatibility analysis, new migration or historical fixture
is required. Design: `docs/designs/prerelease_save_policy_design.md` (P-2,
PSP-2, D-5, D-9). The guard is activated for every code revision in CI
and the local gate; it is not path-selected.

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
compiled evidence. The record is code-gated and cannot take the docs-only
fast path. Updating it does not replace the independent `--base-ref` check:
regenerating, editing or deleting the record cannot bless same-version drift,
because the comparison reads the record committed at the base revision.

## Where each fact comes from

Nothing is parsed from source text.

| Fact | Evidence |
|---|---|
| Haskell component shapes | `componentCodec` demands a `WireSchema` instance for its DTO type `d` and keeps the compiled `WireRoot` on the codec (`ccWire`) and registry entry (`rcWire`) before the registry erases `d`. The walk is GHC `Generic`/`Typeable`: constructors in order, record selectors, newtypes, instantiated field types, GHC-resolved aliases. |
| Component versions and acceptance | `rcVersion`/`rcInputVers` from the live `saveComponentRegistry`, and each version PROBED through `rcPrepare` (the entry point `assembleSnapshot` runs). A disagreement fails extraction. |
| Metadata | `metadataWireRoot` (the type `encodeSessionSnapshot` encodes), `metadataComponentVersion`, and a probe at every version through all four public entry points (`decodeSaveEnvelopeMetadata`, `decodeSaveEnvelopeMetadataClassified`, `decodeSessionEnvelope`, `decodeSessionEnvelopeClassified`). They must agree, and the result must equal the advertised list. |
| Framing | `currentEnvelopeVersion` (also its advertised list), `envelopeMagic`, a header size measured from a real encoded envelope, an fnv1a64 probe, and the hex of a fixed encoded envelope, with acceptance probed through the same four entry points. The header bytes are hand written, so they are declared and probed, not derived. The cereal manifest is walked. |
| Lua modules | The production `scripts/init_loader.lua` runs under a recording `engine.loadScript`; every recorded script is loaded and its real `init` run; the evaluated `saveModules.registry` supplies id, version, `inputVersions` and `required`; each version is probed through `saveModules.prepareLoad`. A script that cannot load or initialise fails the run. `scripts/lib/data_codec.lua` is the unversioned wire owner every Lua carrier shares, recorded by a probe of its encoding. |
| Legacy routes | B1 (`session`) and B2 (`lua-state`) entry points are probed with correctly shaped envelopes. Any affected carrier must refuse its older shape at the envelope boundary, and the current record stores the route status so a re-enabled route is detected. |

## Serialization boundaries

A `Generic` instance cannot tell whether a `Serialize` instance uses cereal's
generic encoding. Every reachable type is one of:

- **generic** (the default instance): its real `S.encode` is compared with
  cereal's own `GSerializePut` encoding on distinguishing sample values, and
  must round-trip. A hand-written encoder that disagrees fails extraction
  with `unclassified encoder`. Samples give any two fields of one constructor
  different values in some variant (field `i` takes its second sample in the
  variants whose bit is set in `i + 1`, so even the only field of a constructor
  gets one), so a serializer that swaps two equal-typed
  fields on write and read is caught; a field type with only one constructible
  sample cannot be told from an equal-typed neighbour. This is evidence on
  samples, not a proof. A
  checked classification (generic, newtype, custom) with no constructible sample
  fails with `unchecked boundary`; once the nesting budget is spent, sampling
  falls back to the simplest constructible value, so a deeply wrapped type is
  still compared.
- **primitive / library**: base-type and container encoders, declared by
  explicit instances in `World.Save.Wire`. Several are this repository's own
  orphans (`Text`, `HashMap`, `Vector`, `HashSet`); the rest are cereal's. Every
  reachable one has a layout example in the `current save schema` group whose
  bytes are specified by hand, and the reachable set must equal the table
  exactly, so a swap of the serializer behind any of them is visible.
- **newtype**: a newtype-derived instance, checked against its field's
  encoding; a prefix-writing encoder that still round-trips fails.
- **custom**: a hand-written encoder declared in `World.Save.Wire.Custom` with
  an owner, a description of the bytes and the wire types it writes. Declared
  and tested, **not derived**: every declaration needs at least two
  distinguishing sample values (round trip), and an owned example in the
  `current save schema` group pins its layout against bytes specified by hand
  in the test from the declared layout (not produced by the encoder under
  test). A type that is reachable but not declared, or declared but
  unreachable, fails.

Lua payloads are dynamic tables; the hand-written framing bytes are explicit
owned boundaries rather than a fabricated type description. Their behavioral
probes and independent current tests are part of the declared evidence.

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
  enabled legacy route is **not recorded**. For a changed carrier, disable its
  affected old dispatches and legacy routes and pin refusal in current
  behavioral tests. Unchanged carriers need no blanket purge.
- Routine comparison never builds the base. CI checks every candidate and every
  merged code revision, so the base record was itself validated when it
  entered the protected history; this tool validates its structure and reports
  its blob identity.

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
then asking cabal's own dry run to confirm nothing is left to build), and requires the committed
record to equal the compiled evidence exactly. A base that contains any guard
artifact must carry a valid record: a missing or malformed one fails and never
enables the bootstrap. Only a base containing none of the guard's artifacts
(first introduction) is extracted as well, by laying the candidate's additive
hooks onto an export of it; the adapter is refused if it would remove any base
line outside the declared hook rewrite. The run output states the resolved
commits, the base record's blob and validation provenance, the candidate's
tracked-edit identity and the extractor and toolchain identity. The record
itself carries none of that (nor the number of scripts the Lua loader
requested, which is run provenance), so off-wire and comment-only edits leave
it byte-identical.
