"""The probe-runner process suite's case owners and shared fixtures (#2130).

`tools/test_run_probes.py` stays the aggregate command CI and
`tools/ci-local.sh` invoke; this package holds the test bodies it
composes, divided along the production owners #2074 split the runner
into:

  `registry`      timeout-override and exclusive-resource declarations,
                  exact and substring selection, and port spans;
  `resources`     the one-time executable preflight, inherited and
                  foreign holds, and the reader/writer resource ledger;
  `lifecycle`     one probe's launch, teardown, liveness and reap;
  `scheduler`     aggregate exits, conflict scheduling, retries, and a
                  real Ctrl-C to a real runner;
  `diagnostics`   the durable progress and failure record protocols;
  `readme`        #2035's registry-count audit, placed in the aggregate,
                  and the proof that its failure fails this gate.

`support` is what two or more of them share. A leading underscore is not
used to mark it: the facade's `NON_OWNER_MODULES` names it, and the
facade's composition check fails if the modules on disk and the
registered families ever disagree in either direction.

Importing this package runs no test and imports no case owner -- the
facade imports the six families itself, so `--family lifecycle` can be
served without paying for the other five, and nothing here can register
a group by side effect.
"""
