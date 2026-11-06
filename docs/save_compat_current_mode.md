# Current save-schema and codec checks

PSP-5 activates the current-schema guard and the current-fixture codec audit.
There is no historical mode. Pre-release save-shape changes advance every
affected carrier version and disable that carrier's old accepted dispatches
and affected exact-shape legacy routes.

Run the focused checks after building the production codec:

```bash
cabal build all
python3 tools/current_save_schema_audit.py --self-test
python3 tools/current_save_schema_audit.py --base-ref origin/master
python3 tools/test_save_compat_audit.py
python3 tools/save_compat_audit.py
```

The compiled schema guard reads the validated record committed at the base,
compiles only the candidate, and checks the candidate record against fresh
compiled evidence. The codec audit checks the committed current fixture and
its independent descriptor and summary expectations. Neither command
reconstructs historical fixtures or proves arbitrary custom serializers,
Lua payload meanings, or the handwritten framing bytes; those boundaries
have explicit ownership and behavioral tests. See
[`current_save_schema_guard.md`](current_save_schema_guard.md) and
[`persistence_contract.md`](persistence_contract.md) §5 for the full policy
and public-release checklist.
