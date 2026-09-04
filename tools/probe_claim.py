#!/usr/bin/env python3
"""One probe, one claimant: the atomic per-probe claim (#1434).

Many `/deflake` agents run at once, one probe each. Two of them
measuring the SAME probe is an hour of duplicated engine time and two
conflicting census records, and nothing in the harness prevented it:
`tools/run_probes.py` coordinates only the probes inside one runner
process, and `tools/probe_flake.py`'s port leases and live-invocation
registry coordinate host-global PORTS and a concurrency figure, not
probe identity.

    python3 tools/probe_claim.py --probe role --runs 10 --result /tmp/r.json
    python3 tools/probe_claim.py --status

This file is the COMMAND and nothing else: it parses the arguments,
invokes an owner, renders text or JSON, and returns a status. It
implements no claim-file codec, no lease decision, no retention and no
census orchestration, and it re-exports none of its owners' names, so a
tool or test that reaches for `acquire`, `LEASE_SECONDS`,
`repository_claim_root` or `retain_measurement` imports the owner that
defines it and assigning to it changes the state the implementation
actually reads (#2148).

The three owners, and the one direction they depend in:

  `probe_claim_storage`        the namespace, the claim-file codec,
                               durable writes and the sidecar lock —
                               the filesystem leaf, which decides
                               nothing about ownership
  `probe_claim_lease`          who holds a claim and for how long:
                               key and lease validation, acquisition,
                               renewal, takeover, token-safe release,
                               the renewer, and the status query
  `probe_claim_orchestration`  the claimed measurement: result-path
                               validation, the acquisition audit, the
                               pre-run reassertion, retention,
                               serialized ingestion, and the outcome

Storage imports neither of the others; the lease owner imports storage
and the probe registry; orchestration imports both plus the census and
the measurement harness; this file imports all three. The invariant
documentation for each of those contracts lives with the owner that
ENFORCES it — what a claim file is and why it is the lock in storage,
why expiry is one-way and why every instant is sampled inside the lock
in the lease owner, and the eight-step claimed-measurement sequence in
orchestration.

Exit codes:
  0  the measurement ran and was ingested (whatever rate it observed)
  2  rejected before anything was claimed or created
  3  ALREADY CLAIMED: another agent holds this probe; nothing was
     created and nothing was recorded
  4  harness error: the measurement's protocol stream could not be
     trusted. The non-accepted attempt is still ingested
  5  the census could not durably record this run. BEFORE the probe
     ran that is a claim audit failure: the acquisition was not
     recordable, so the measurement was refused and the claim released
     with nothing created. AFTER it ran the measurement really
     happened, so it is retained on disk instead — the diagnostic names
     the file and the `--record` command that ingests it, and
     re-running the probe is never the recovery
  6  no clear, leasable port in the whole range
  7  the claim was lost. Before the probe started, the probe was NOT
     run, because starting a measurement this run no longer owns is the
     duplicated work the claim exists to prevent. After it ran, another
     agent may have been measuring the probe too, so NOTHING is
     ingested — an unattributable measurement is not a measurement —
     while the artifacts and the retained result document are kept
"""
from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import probe_census  # noqa: E402
import probe_claim_lease as lease  # noqa: E402
import probe_claim_orchestration as orchestration  # noqa: E402
import probe_claim_storage as storage  # noqa: E402
import probe_flake  # noqa: E402

def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--probe", default=None,
                    help="exactly one registered, probe-result/v1 probe key")
    ap.add_argument("--runs", type=int, default=None,
                    help="how many times to run it (positive)")
    ap.add_argument("--status", action="store_true",
                    help="report every claim in this repository and exit")
    ap.add_argument("--result", default=None,
                    help="write the probe-flake-result/v1 document here, as "
                         "soon as the measurement completes and before it is "
                         "ingested, so an ingestion failure cannot cost the "
                         "run")
    ap.add_argument("--artifact-root", default=None,
                    help="override probe_flake's artifact root")
    ap.add_argument("--rts-caps", type=int, default=probe_flake.DEFAULT_RTS_CAPS,
                    help=f"RTS capabilities for every engine "
                         f"(default {probe_flake.DEFAULT_RTS_CAPS})")
    ap.add_argument("--lease-seconds", type=float, default=lease.LEASE_SECONDS,
                    help=f"claim lease before renewal (default "
                         f"{lease.LEASE_SECONDS:.0f}, minimum "
                         f"{orchestration.MIN_ORCHESTRATION_LEASE_SECONDS:.0f}"
                         f"); the renewer refreshes it while the probe runs")
    ap.add_argument("--json", action="store_true",
                    help="machine-readable outcome on stdout")
    args = ap.parse_args(argv)

    try:
        if args.status:
            rows = lease.status_rows(storage.repository_claim_root())
            if args.json:
                print(json.dumps({"claims": rows}, indent=2, sort_keys=True))
            elif not rows:
                print("no probe is claimed in this repository")
            else:
                for row in rows:
                    print(f"  {row['probe']:<32} {row.get('state')} "
                          f"{row.get('owner') or ''}".rstrip())
            return orchestration.EXIT_OK
        if not args.probe or args.runs is None:
            ap.error("--probe and --runs are required unless --status is given")

        def announce(index: int, total: int, port: int) -> None:
            print(f"[{index}/{total}] {args.probe} on port {port} ...",
                  file=sys.stderr, flush=True)

        outcome = orchestration.run_claimed_measurement(
            args.probe, args.runs,
            artifact_root=Path(args.artifact_root) if args.artifact_root else None,
            rts_caps=args.rts_caps, lease_seconds=args.lease_seconds,
            result_path=args.result, announce=announce)
    except probe_flake.Rejection as error:
        print(f"probe_claim: {error}", file=sys.stderr)
        return orchestration.EXIT_REJECTED
    except storage.ClaimError as error:
        print(f"probe_claim: {error}", file=sys.stderr)
        return orchestration.EXIT_REJECTED
    except orchestration.ClaimAuditFailed as error:
        print(f"probe_claim: {error}", file=sys.stderr)
        return orchestration.EXIT_CLAIM_AUDIT
    except orchestration.ClaimLostDuringRun as error:
        print(f"probe_claim: {error}", file=sys.stderr)
        return orchestration.EXIT_CLAIM_LOST
    except orchestration.ResultIngestionFailed as error:
        print(f"probe_claim: {error}", file=sys.stderr)
        return orchestration.EXIT_CLAIM_AUDIT
    except probe_flake.PortExhausted as error:
        print(f"probe_claim: {error}", file=sys.stderr)
        return orchestration.EXIT_NO_PORT
    except (probe_census.CensusError,
            probe_census.DocsWorktreeMissing) as error:
        print(f"probe_claim: {error}", file=sys.stderr)
        return orchestration.EXIT_CLAIM_AUDIT

    if outcome.denied is not None:
        print(f"probe_claim: {outcome.detail}", file=sys.stderr)
        if args.json:
            print(json.dumps(outcome.to_document(), indent=2, sort_keys=True))
        return outcome.exit_code

    if args.json:
        print(json.dumps(outcome.to_document(), indent=2, sort_keys=True))
    else:
        print(probe_flake.render(outcome.measurement))
        print(f"\nclaim {outcome.claim.token} recorded in {outcome.census_path}")
        if outcome.result_path is not None:
            print(f"wrote {probe_flake.RESULT_SCHEMA} to "
                  f"{outcome.result_path}")
    if outcome.result_problem:
        print(f"probe_claim: WARNING — {outcome.result_problem}",
              file=sys.stderr)
    if outcome.claim_lost:
        print(f"probe_claim: WARNING — the claim was lost during the "
              f"measurement ({outcome.claim_lost})", file=sys.stderr)
    return outcome.exit_code


if __name__ == "__main__":
    sys.exit(main())
