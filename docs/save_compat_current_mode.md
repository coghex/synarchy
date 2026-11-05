# Current save-compatibility audit

PSP-3 adds an opt-in audit for saves written by the current schema. The
existing no-mode audit continues to check the historical corpus and remains
the default until PSP-5 activates the current-only gate.

Run the current audit after building the production codec:

```bash
cabal build all
python3 tools/save_compat_audit.py --mode current
python3 tools/test_save_compat_audit.py --mode current
```

The current mode reads `tools/save_compat_current/record.json` and only the
fixtures named there. It does not load `docs/save_compat/manifest.json`, scan
`test-headless/data/save-compat/`, or run historical migrations and
reproducibility checks. The record is outside the historical corpus's
discovery paths so the unchanged default audit remains unaffected.

For every selected fixture, the audit checks nonempty bytes and session
summary, SHA-256 checksums, framing against `currentEnvelopeVersion`, and an
independently recorded component descriptor table. It compares that table
with live Haskell and Lua registrations, requiring every required component
and allowing registered optional components. The compiled
`exe:synarchy-save-codec` decodes the bytes, verifies the real descriptors,
and produces a temporary summary which must match the committed regression
summary. The audit never regenerates expected summaries.

Current fixtures and summaries live under `tools/save_compat_current/`. To
replace one, decode and re-encode a real session through the compiled codec,
record how it was produced, calculate its checksums, and hand-maintain the
expected descriptors independently. A version or framing change makes the
fixture stale and requires replacement; historical bytes must not be
relabelled as current. PSP-3 does not boot the engine, run a migration probe,
change CI defaults, or remove historical audit machinery.

The focused current self-test has 7 members. The pre-existing selector is
unchanged: its no-argument, `--without-reproducibility`, and
`--only-reproducibility` forms select 104, 102, and 2 members both before and
after PSP-3. Current tests stay separate until PSP-5 activates them.
