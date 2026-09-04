"""The flake self-test's case owners and their shared fixtures (#2087).

`tools/test_probe_flake.py` stays the aggregate command CI and
`tools/ci-local.sh` invoke; this package holds the test bodies it
composes, split along the boundary the gate's two responsibilities
actually have:

  `harness_*`     the generic contracts -- descriptor and event-stream
                  validation, eligibility and reconciliation, ports,
                  leases and concurrency, artifacts, result rendering,
                  census integration, the `run_one` interface, and the
                  composition of this package itself;
  `migration_*`   one module per key in `probe_flake.PROTOCOL_PROBES`,
                  each owning that probe's standalone/protocol
                  compatibility contract and nothing else.

That is the point of the split: adding or changing one probe's
migration contract touches `migration_<key>.py` alone -- not the
harness modules, and not another probe's module.

`support` is what two or more owners share: the synthetic protocol
probe and its throwaway tree, the shared assertion and skip helpers,
and the two migration drivers (`migration_descriptor`, and the
`batch_contract` the ten batch-migrated probes have in common). A
leading underscore is not used to mark it: the facade's
`NON_OWNER_MODULES` names it, and the facade's composition check fails
if the modules on disk and the registered owners ever disagree in
either direction.

Importing this package runs no test and imports no case owner -- the
facade imports the owners itself, so `--only migration:role` can be
served without paying for the other twenty-one, and nothing here can
register a group by side effect.
"""
