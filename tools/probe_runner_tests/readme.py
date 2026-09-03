#!/usr/bin/env python3
"""The README registry-count audit, and its composition (#2035, #2130).

Two groups. The first is not defined here: it is #2035's own
`test_the_readme_states_no_registry_total`, read from that module's
`TEST_GROUPS` declaration so the audit keeps exactly one owner and this
family merely places it. The second proves the composition -- a
deliberately violating document, driven through the extracted module's
own `use_readme`, must make the aggregate gate exit non-zero, with the
shipped file as the passing control.

The composition driver runs the facade's REAL `main` in a fresh
interpreter through the facade's REAL `compose`, so the aggregate order
and its checks are the ones under test; only the groups OTHER than the
audit are replaced, which is what keeps the proof well under a second.
Requiring the audit to be present in what `compose` returned is what
stops the driver from passing vacuously if the README group were ever
dropped from the aggregate.
"""
from __future__ import annotations

import subprocess
import sys
import tempfile
import textwrap
from pathlib import Path

from .support import TOOLS_DIR

import test_readme_registry_count  # noqa: E402
from selftestlib import expect  # noqa: E402


# --------------------------------------------------------------------------
# The composed README audit decides this gate's verdict too (#2035)
# --------------------------------------------------------------------------
# The README registry-count audit has its own owner
# (`tools/test_readme_registry_count.py`), while the unconditional gate CI
# and `tools/ci-local.sh` invoke is `tools/test_run_probes.py`. So "the
# aggregate keeps failing when the audit fails" is a claim about
# composition, and a claim with no proven failing case is not a claim
# (#704, #1128, #1309).
#
# The driver below runs the facade's REAL `main()` in a fresh interpreter,
# through the facade's REAL `compose()` -- so the family roster check, the
# fragment reconstruction and the run-order checks all execute, and the
# order under test is the shipped one rather than a list written here.
# What it then replaces is every group in that composed sequence EXCEPT
# the audit, which is what keeps the proof well under a second. Replacing
# by identity, after requiring the audit to BE in what `compose` returned,
# is what stops this from passing vacuously if the README group were ever
# dropped from the aggregate: the driver exits nonzero on its own, and the
# control assertion below reads that as a failure.

COMPOSITION_DRIVER_SRC = textwrap.dedent("""\
    import sys
    from pathlib import Path

    tools, readme = sys.argv[1], sys.argv[2]
    sys.path.insert(0, tools)

    import test_readme_registry_count as audit
    import test_run_probes

    # "-" leaves the shipped file in place: that run is the passing control.
    if readme != "-":
        audit.use_readme(Path(readme))

    kept = audit.test_the_readme_states_no_registry_total
    real_compose = test_run_probes.compose

    def only_the_audit(family=None):
        groups = real_compose(family)
        if kept not in groups:
            raise SystemExit(
                "the composed run does not include the README audit")

        def skipped(*args, **kwargs):
            return None

        return [group if group is kept else skipped for group in groups]

    test_run_probes.compose = only_the_audit
    raise SystemExit(test_run_probes.main([]))
""")


def composed_gate(readme: str) -> tuple[int, str]:
    """Run the facade's `main()` with only the composed audit live."""
    with tempfile.TemporaryDirectory() as tmp:
        driver = Path(tmp) / "drive_composition.py"
        driver.write_text(COMPOSITION_DRIVER_SRC, encoding="utf-8")
        done = subprocess.run([sys.executable, str(driver), TOOLS_DIR, readme],
                              capture_output=True, text=True, timeout=180)
    return done.returncode, done.stdout + done.stderr


def test_a_failing_readme_audit_fails_this_gate() -> None:
    print("\n-- a failing README registry-count audit fails this gate")

    # The control: the shipped README, reached through the real composition.
    status, out = composed_gate("-")
    expect(status == 0,
           f"the shipped README passes the composed gate "
           f"(exit {status})\n{out[-800:]}")
    expect("run_probes section states no registry total" in out,
           f"and the composed audit really ran (got {out[-400:]!r})")

    # The proof: a document the audit rejects, through the same composition.
    with tempfile.TemporaryDirectory() as tmp:
        violating = Path(tmp) / "README.md"
        violating.write_text(
            test_readme_registry_count.VIOLATING_DOCUMENT, encoding="utf-8")
        status, out = composed_gate(str(violating))
    expect(status != 0,
           "a failing README audit makes this gate exit non-zero "
           f"(exit {status})\n{out[-800:]}")
    expect("claims no registry total" in out,
           f"naming the audit's own failure (got {out[-400:]!r})")
    expect("test(s) failed:" in out,
           f"through this gate's own failure accounting (got {out[-400:]!r})")


#: This family's complete ordered inventory. The audit itself is #2035's
#: group, read from that module's own declaration rather than named
#: again here, so it keeps exactly one owner; the composition proof is
#: this module's.
TESTS = test_readme_registry_count.TEST_GROUPS + (
    test_a_failing_readme_audit_fails_this_gate,
)
