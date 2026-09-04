#!/usr/bin/env python3
"""Port allocation, leasing and concurrency accounting (#2087).

The largest harness family, and the one whose fixtures spawn child
harnesses: reserved spans, whole-span leasing, cross-`TMPDIR` lease-root
resolution, concurrent leasing and the peak-concurrency ledger.
"""
from __future__ import annotations

import json
import os
import shutil
import socket
import stat
import subprocess
import sys
import tempfile
import textwrap
import threading
import time
from pathlib import Path

from .support import probe_flake, probe_runner_lifecycle, probe_runner_registry
from .support import SyntheticTree, run_synthetic, TOOLS_DIR, hand_to_third_party, expect_raises, expect

def test_ports() -> None:
    print("\n-- ports --")
    with SyntheticTree() as tree:
        expect_raises(probe_flake.Rejection,
                      lambda: probe_flake.PortLease.try_acquire(
                          probe_flake.FORBIDDEN_PORT),
                      "port 8008 is always forbidden", "always forbidden")

        saved = (probe_flake.PORT_MIN, probe_flake.PORT_MAX)
        try:
            # A three-port range makes increment, wraparound and
            # exhaustion all directly observable.
            probe_flake.PORT_MIN, probe_flake.PORT_MAX = 8009, 8011
            lease_a, cursor = probe_flake.acquire_port(8009)
            expect(lease_a.port == 8009 and cursor == 8010,
                   "the first lease takes the range floor and advances the cursor")
            lease_b, cursor = probe_flake.acquire_port(cursor)
            expect(lease_b.port == 8010 and cursor == 8011,
                   "the cursor increments per run")
            lease_c, cursor = probe_flake.acquire_port(cursor)
            expect(lease_c.port == 8011 and cursor == 8009,
                   "the cursor wraps back to the range floor")
            expect_raises(probe_flake.PortExhausted,
                          lambda: probe_flake.acquire_port(cursor),
                          "a fully leased range is exhausted, cleanly",
                          "complete scan")
            # A concurrent harness (a second lease attempt on a held
            # port) is refused by the atomic O_EXCL create.
            expect(probe_flake.PortLease.try_acquire(8010) is None,
                   "a port another harness holds cannot be leased twice")
            lease_b.release()
            regained, _ = probe_flake.acquire_port(8010)
            expect(regained.port == 8010,
                   "a released port becomes leasable again")
            regained.release()
            lease_a.release()
            lease_c.release()

            # 8008 is skipped even if the range is edited to include it.
            probe_flake.PORT_MIN, probe_flake.PORT_MAX = 8008, 8009
            lease, _ = probe_flake.acquire_port(8008)
            expect(lease.port == 8009,
                   "8008 is skipped even when the range would contain it")
            lease.release()
        finally:
            probe_flake.PORT_MIN, probe_flake.PORT_MAX = saved

        # A leftover lease FILE is not a lease: the lock is, and a dead
        # owner holds none. No age heuristic, nothing to unlink.
        leftover = probe_flake._lease_path(8009)
        leftover.write_text(json.dumps({"pid": _dead_pid(), "port": 8009}),
                            encoding="utf-8")
        lease = probe_flake.PortLease.try_acquire(8009)
        expect(lease is not None,
               "a leftover lease file from a dead harness is immediately "
               "acquirable")
        if lease:
            lease.release()

        # A held lease is never stolen, however old its file looks.
        held = probe_flake.PortLease.try_acquire(8009)
        os.utime(leftover, (0, 0))
        expect(probe_flake.PortLease.try_acquire(8009) is None,
               "a held lease is never treated as stale, whatever its mtime")
        if held:
            held.release()
        expect(leftover.exists(),
               "releasing a lease leaves its diagnostic file in place — "
               "unlinking it is what would reintroduce the recovery race")


def _held_ports(base: int, width: int) -> list[int]:
    """Which of `base .. base + width - 1` cannot be leased right now.

    `flock` conflicts between open file DESCRIPTIONS, not processes, so
    this answers correctly about a lease this same process is holding —
    which is what makes it a usable probe for "the span is held".
    Anything it could take is given straight back.
    """
    held: list[int] = []
    for port in range(base, base + width):
        lease = probe_flake.PortLease.try_acquire(port)
        if lease is None:
            held.append(port)
        else:
            lease.release()
    return held


def test_port_spans() -> None:
    print("\n-- multi-port spans (#1571) --")
    with SyntheticTree() as tree:
        saved = (probe_flake.PORT_MIN, probe_flake.PORT_MAX)
        try:
            # A five-port range makes every boundary directly observable.
            probe_flake.PORT_MIN, probe_flake.PORT_MAX = 8009, 8013

            expect_raises(probe_flake.Rejection,
                          lambda: probe_flake.acquire_span(8009, 0),
                          "a zero-width span is rejected", "positive")
            expect_raises(probe_flake.PortExhausted,
                          lambda: probe_flake.acquire_span(8009, 6),
                          "a span wider than the whole range is exhaustion, "
                          "not a partial lease", "does not fit")

            # -- the complete span is leased, and the cursor advances by it.
            leases, cursor = probe_flake.acquire_span(8009, 2)
            expect([lease.port for lease in leases] == [8009, 8010],
                   f"a two-port span leases BOTH ports, contiguously "
                   f"(got {[lease.port for lease in leases]})")
            expect(cursor == 8011,
                   f"and the cursor advances by the full span (got {cursor})")
            expect(_held_ports(8009, 2) == [8009, 8010],
                   "both members are really held while the span is out")

            # -- a span overlapping a held one is refused, base or member.
            #    8009 is held (a base clash) and 8010 is held (a SECONDARY
            #    clash for base 8009), so the next two-port span must start
            #    at 8011.
            second, cursor2 = probe_flake.acquire_span(8009, 2)
            expect([lease.port for lease in second] == [8011, 8012],
                   f"the next span skips every base whose span overlaps a "
                   f"held one (got {[lease.port for lease in second]})")
            for lease in second:
                lease.release()

            # -- partial acquisition is released, not kept. Base 8011 is
            #    free but 8012 is not, so the attempt at 8011 must give
            #    8011 back before moving on -- otherwise the free port
            #    would be stranded by a span that never started.
            blocker = probe_flake.PortLease.try_acquire(8012)
            expect(blocker is not None, "the fixture can hold 8012")
            expect_raises(probe_flake.PortExhausted,
                          lambda: probe_flake.acquire_span(8011, 2),
                          "no two-port span survives 8009/8010/8012 being held",
                          "complete scan")
            expect(_held_ports(8011, 1) == [],
                   "and the partially acquired 8011 was given back")
            if blocker:
                blocker.release()

            # -- an OCCUPIED secondary is refused the same way a leased one
            #    is: the lease is available but something outside this
            #    harness is listening there.
            for lease in leases:
                lease.release()
            occupied = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            occupied.bind(("127.0.0.1", 8010))
            occupied.listen(4)
            try:
                got, _ = probe_flake.acquire_span(8009, 2)
                expect([lease.port for lease in got] == [8011, 8012],
                       f"a span whose SECOND port is occupied is skipped "
                       f"(got {[lease.port for lease in got]})")
                expect(_held_ports(8009, 1) == [],
                       "and the base it partially acquired was released")
                for lease in got:
                    lease.release()
            finally:
                occupied.close()

            # -- a span never wraps the range end. 8013 is PORT_MAX, so a
            #    two-port span cannot start there; it wraps the SCAN to
            #    8009 instead of leasing 8013+8014.
            wrapped, _ = probe_flake.acquire_span(8013, 2)
            expect([lease.port for lease in wrapped] == [8009, 8010],
                   f"a base whose span would pass PORT_MAX is skipped, and "
                   f"the scan wraps to a base that fits "
                   f"(got {[lease.port for lease in wrapped]})")
            for lease in wrapped:
                lease.release()

            # -- span 1 is exactly the old single-port behaviour.
            one, cursor3 = probe_flake.acquire_span(8009, 1)
            expect([lease.port for lease in one] == [8009] and cursor3 == 8010,
                   f"span 1 leases one port and advances by one "
                   f"(got {[lease.port for lease in one]}, cursor {cursor3})")
            for lease in one:
                lease.release()
        finally:
            probe_flake.PORT_MIN, probe_flake.PORT_MAX = saved


def test_measure_leases_the_probes_whole_declared_span() -> None:
    print("\n-- measure leases the DECLARED span, and only lets go after the "
          "reap --")
    with SyntheticTree() as tree:
        saved_spans = probe_runner_registry.PROBE_PORT_SPANS
        real_run_one = probe_runner_lifecycle.run_one
        seen: dict[str, object] = {}

        def spy(script, port, timeout, groups, **kwargs):
            # Inside `run_one` the probe is live, so this is the window
            # the span has to cover. `run_one` reaps the probe's whole
            # process group before it returns, so a lease still held
            # here and released after cannot hand a port to anyone while
            # an engine still owns it.
            seen["base"] = port
            seen["held"] = _held_ports(port, 3)
            return real_run_one(script, port, timeout, groups, **kwargs)

        probe_runner_registry.PROBE_PORT_SPANS = {"synthetic": 2}
        probe_runner_lifecycle.run_one = spy
        try:
            measurement = run_synthetic(tree, "pass", runs=1)
        finally:
            probe_runner_lifecycle.run_one = real_run_one
            probe_runner_registry.PROBE_PORT_SPANS = saved_spans

        base = seen.get("base")
        expect(isinstance(base, int) and base is not None,
               f"the probe was launched with a base port (got {base!r})")
        expect(seen.get("held") == [base, base + 1],
               f"its declared TWO-port span was held for the whole run, and "
               f"nothing beyond it (got {seen.get('held')}, base {base})")
        expect(measurement.runs and measurement.runs[0].port == base,
               f"the run records the BASE of the span it was given "
               f"(got {[r.port for r in measurement.runs]}, base {base})")
        expect(_held_ports(base, 2) == [],
               "and every member is released once the measurement is over")


def test_concurrent_leasing() -> None:
    print("\n-- concurrent lease acquisition --")
    with SyntheticTree() as tree:
        port = 8042
        path = probe_flake._lease_path(port)
        # A leftover file from an abnormally terminated harness: the
        # exact state in which two racing harnesses used to be able to
        # both "recover" it and both end up on this port.
        path.write_text(json.dumps({"pid": _dead_pid(), "port": port}),
                        encoding="utf-8")

        racers = 12
        ready = threading.Barrier(racers)
        winners: list[object] = []
        lock = threading.Lock()

        def race() -> None:
            ready.wait(timeout=30)
            lease = probe_flake.PortLease.try_acquire(port)
            if lease is not None:
                with lock:
                    winners.append(lease)

        threads = [threading.Thread(target=race) for _ in range(racers)]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join(timeout=60)
        expect(len(winners) == 1,
               f"exactly one of {racers} racers recovers and holds the port "
               f"(got {len(winners)})")
        for lease in winners:
            lease.release()

        # Cross-PROCESS exclusion, and recovery when the holder is
        # killed outright — which is what an `flock` gives for free and
        # a create-then-delete protocol has to guess at.
        holder_src = tree.root / "holder.py"
        holder_src.write_text(textwrap.dedent(f'''\
            import sys, time
            sys.path.insert(0, {TOOLS_DIR!r})
            import probe_flake
            from pathlib import Path
            probe_flake.LEASE_ROOT = Path({str(probe_flake.LEASE_ROOT)!r})
            lease = probe_flake.PortLease.try_acquire({port})
            print("held" if lease else "missed", flush=True)
            time.sleep(300)
        '''), encoding="utf-8")
        holder = subprocess.Popen([sys.executable, str(holder_src)],
                                  stdout=subprocess.PIPE, text=True)
        try:
            first = holder.stdout.readline().strip()
            expect(first == "held",
                   f"another process acquires the port first (said {first!r})")
            expect(probe_flake.PortLease.try_acquire(port) is None,
                   "a lease held by another PROCESS blocks this one")
        finally:
            holder.kill()
            holder.wait(timeout=30)
        recovered = None
        for _ in range(50):
            recovered = probe_flake.PortLease.try_acquire(port)
            if recovered is not None:
                break
            time.sleep(0.1)
        expect(recovered is not None,
               "killing the holder outright releases the lease with no "
               "staleness heuristic at all")
        if recovered:
            recovered.release()


# Source for a child harness that publishes a live registration and
# holds it until killed — the only way to exercise "another PROCESS
# owns this entry" and, after a SIGKILL, "its owner is gone" without
# consulting a pid.
REGISTRY_HOLDER_SRC = """\
import sys, time
sys.path.insert(0, {tools!r})
import probe_flake
from pathlib import Path
probe_flake.LEASE_ROOT = Path({root!r})
with probe_flake.LiveRegistry() as registry:
    print(registry.path, flush=True)
    time.sleep(120)
"""


# A port deliberately OUTSIDE 8009-8999, so the cross-TMPDIR test below
# contends in the REAL machine-wide lease namespace — which is the only
# place the defect it covers can be observed — without ever touching a
# port a real harness might be measuring on.
TMPDIR_TEST_PORT = 65000

# Source for a child harness that resolves LEASE_ROOT ITSELF. Rebinding
# it here would defeat the point: what is under test is whether two
# invocations under different TMPDIRs agree on the namespace.
HOLDER_SRC = """\
import sys, time
sys.path.insert(0, {tools!r})
import probe_flake
lease = probe_flake.PortLease.try_acquire({port})
print(('held ' if lease else 'missed ') + str(probe_flake.LEASE_ROOT),
      flush=True)
if lease:
    time.sleep(120)
"""


def test_lease_root_is_tmpdir_independent() -> None:
    print("\n-- lease namespace vs TMPDIR --")
    saved = os.environ.get("TMPDIR")
    try:
        first = probe_flake._machine_wide_scratch()
        os.environ["TMPDIR"] = tempfile.mkdtemp(prefix="probe-flake-tmpa-")
        tempfile.tempdir = None
        second = probe_flake._machine_wide_scratch()
        moved_artifacts = probe_flake.default_artifact_root()
        os.environ["TMPDIR"] = tempfile.mkdtemp(prefix="probe-flake-tmpb-")
        tempfile.tempdir = None
        third = probe_flake._machine_wide_scratch()
        expect(first == second == third,
               f"the lease root is the same under any TMPDIR "
               f"({first}, {second}, {third})")
        expect(first == Path("/tmp"),
               f"the lease root is anchored at the fixed shared /tmp ({first})")
        expect(moved_artifacts != probe_flake.default_artifact_root(),
               "the ARTIFACT root does follow TMPDIR — only the lease "
               "namespace is pinned")
    finally:
        tempfile.tempdir = None
        if saved is None:
            os.environ.pop("TMPDIR", None)
        else:
            os.environ["TMPDIR"] = saved

    # Cross-USER, too: a TCP port is host-global, so a uid in the path
    # would let two accounts lease "the same" port through different
    # files. A second account cannot be created from a self-test, so
    # the properties that make sharing WORK are asserted instead.
    root = probe_flake._machine_wide_scratch()
    expect(str(os.getuid()) not in root.name,
           f"the lease root carries no uid, so every account resolves the "
           f"same one ({root})")
    expect(root == Path("/tmp"),
           f"the lease root is /tmp ITSELF ({root})")
    # `stat`, not `lstat`: /tmp IS a symlink to /private/tmp on macOS,
    # and what has to be root-owned and sticky is the directory it names.
    info = root.stat()
    expect(info.st_uid == 0 and stat.S_IMODE(info.st_mode) & stat.S_ISVTX,
           f"which is root-owned and sticky here, so no unprivileged "
           f"account owns the namespace (uid {info.st_uid}, mode "
           f"{stat.S_IMODE(info.st_mode):04o})")
    expect(probe_flake._check_shared_dir(root) == root,
           "and the real /tmp passes every namespace check, symlinked or not")
    expect(probe_flake._lease_path(8009).parent == root,
           "lease files are FLAT in it — a harness-created subdirectory "
           "would be owned by whoever made it, and a directory's owner may "
           "unlink entries in it whatever the sticky bit says")
    with SyntheticTree() as shared:
        lease = probe_flake.PortLease.try_acquire(8009)
        try:
            file_mode = stat.S_IMODE(lease.path.lstat().st_mode)
            expect(file_mode & 0o066 == 0o066,
                   f"a lease file is readable and writable by other users, "
                   f"because a lock nobody else can open coordinates nobody "
                   f"else (mode {file_mode:04o})")
        finally:
            lease.release()
        with probe_flake.LiveRegistry() as registry:
            reg_mode = stat.S_IMODE(registry.path.lstat().st_mode)
            expect(reg_mode & 0o066 == 0o066,
                   f"so is a live-invocation registration (mode {reg_mode:04o})")
        # A symlinked root is FOLLOWED — `/tmp` is one on macOS — and
        # then judged on what it landed on, so a link into a directory
        # that fails any namespace check is still refused.
        elsewhere = shared.root / "elsewhere"
        elsewhere.mkdir()
        elsewhere.chmod(0o777)
        hostile = shared.root / "hostile"
        hostile.symlink_to(elsewhere)
        expect_raises(probe_flake.Rejection,
                      lambda: probe_flake._check_shared_dir(hostile),
                      "a symlink into a non-sticky directory is refused",
                      "is not sticky")
        elsewhere.chmod(0o1777)
        expect(probe_flake._check_shared_dir(hostile) == hostile,
               "while a symlink to a sound one is accepted, which is exactly "
               "how /tmp is reached on macOS")
        # A NON-STICKY directory is refused rather than repaired: in a
        # directory without the sticky bit any local user may unlink any
        # entry, so a held lease means nothing. Nothing here chmods a
        # shared directory — quietly widening someone else's permissions
        # would be a worse answer than stopping.
        loose = shared.root / "loose-leases"
        loose.mkdir()
        loose.chmod(0o777)
        expect_raises(probe_flake.Rejection,
                      lambda: probe_flake._check_shared_dir(loose),
                      "a non-sticky scratch directory is refused",
                      "is not sticky")
        expect(stat.S_IMODE(loose.lstat().st_mode) == 0o777,
               "and is left exactly as it was found")

        # THE PATHNAME-REPLACEMENT HOLE. A sticky directory still lets
        # its OWNER unlink anyone's entry, so a namespace owned by
        # another unprivileged account is refused outright — that
        # account could remove a held lease pathname and recreate it,
        # leaving two harnesses holding locks on different inodes for
        # one port. `uid` is a parameter precisely so this is testable:
        # a second local account cannot be created from a self-test.
        sticky = shared.root / "someone-elses-leases"
        sticky.mkdir()
        sticky.chmod(0o1777)
        expect(probe_flake._check_shared_dir(sticky, uid=os.getuid()) == sticky,
               "a sticky directory this user owns is accepted")
        asker = hand_to_third_party(sticky)
        expect_raises(probe_flake.Rejection,
                      lambda: probe_flake._check_shared_dir(
                          sticky, uid=asker),
                      "a sticky directory owned by ANOTHER unprivileged user "
                      "is refused, because its owner could replace a held "
                      "lease pathname",
                      "neither root nor this user")
        # Root-owned is the real case, and is accepted for any user.
        expect(probe_flake._check_shared_dir(
                   Path("/tmp"), uid=os.getuid() + 1) == Path("/tmp"),
               "a root-owned sticky directory is accepted whoever is running")

        # THE SYMLINK OVERWRITE. The lease directory is world-writable,
        # so a local user can plant a symlink at an unused port's lease
        # name pointing at a file a harness user can write. Following it
        # would fchmod, truncate and overwrite that target.
        victim = shared.root / "victim.txt"
        victim.write_text("precious", encoding="utf-8")
        victim.chmod(0o600)
        planted = probe_flake._lease_path(8100)
        planted.symlink_to(victim)
        expect(probe_flake.PortLease.try_acquire(8100) is None,
               "a symlinked lease name makes the port unavailable, never "
               "an overwrite")
        expect(victim.read_text(encoding="utf-8") == "precious"
               and stat.S_IMODE(victim.lstat().st_mode) == 0o600,
               f"the symlink's target is untouched — not truncated, not "
               f"chmodded (mode "
               f"{stat.S_IMODE(victim.lstat().st_mode):04o}, "
               f"{victim.read_text(encoding='utf-8')!r})")
        planted.unlink()

        # A planted HARD link is the same attack without a symlink.
        hardlinked = probe_flake._lease_path(8101)
        os.link(victim, hardlinked)
        expect(probe_flake.PortLease.try_acquire(8101) is None,
               "a hard-linked lease name makes the port unavailable too")
        expect(victim.read_text(encoding="utf-8") == "precious",
               "and its target survives as well")
        hardlinked.unlink()

        # So is a non-regular file (a fifo stands in for any of them).
        fifo = probe_flake._lease_path(8102)
        os.mkfifo(fifo)
        expect(probe_flake.PortLease.try_acquire(8102) is None,
               "a non-regular lease entry makes the port unavailable")
        fifo.unlink()

        # And a planted symlink in the registry is never counted live
        # nor followed.
        decoy = (probe_flake.LEASE_ROOT /
                 f"{probe_flake.SHARED_PREFIX}-live-1-decoy.json")
        decoy.symlink_to(victim)
        expect(probe_flake._registration_is_live(decoy) is False,
               "a symlinked registration is never counted as a live harness")
        expect(victim.exists() and victim.read_text(encoding="utf-8")
               == "precious",
               "and its target is neither read as a registration nor removed")
        decoy.unlink()

        # And the hole itself, demonstrated rather than argued: in a
        # directory whose OWNER is not root, unlinking a held lease's
        # pathname and recreating it leaves TWO harnesses holding locks
        # on different inodes for one port. This is what the ownership
        # check above refuses to operate in.
        owned = shared.root / "owner-can-replace"
        owned.mkdir()
        owned.chmod(0o1777)
        saved_root = probe_flake.LEASE_ROOT
        probe_flake.LEASE_ROOT = owned
        try:
            first = probe_flake.PortLease.try_acquire(8009)
            expect(first is not None, "a lease is held in the owned directory")
            expect(probe_flake.PortLease.try_acquire(8009) is None,
                   "and blocks a second acquire while the pathname stands")
            # The directory's owner may unlink it regardless of sticky.
            probe_flake._lease_path(8009).unlink()
            second = probe_flake.PortLease.try_acquire(8009)
            expect(first is not None and second is not None,
                   "but once the pathname is replaced BOTH harnesses hold a "
                   "lease for port 8009 — the hole a user-owned namespace "
                   "leaves open, and the reason /tmp itself is used")
            for lease in (first, second):
                if lease:
                    lease.release()
        finally:
            probe_flake.LEASE_ROOT = saved_root
        asker = hand_to_third_party(owned)
        expect_raises(probe_flake.Rejection,
                      lambda: probe_flake._check_shared_dir(
                          owned, uid=asker),
                      "so such a directory is refused for anyone who does not "
                      "own it", "neither root nor this user")

        # A regular file where the directory belongs is refused outright.
        notadir = shared.root / "not-a-dir"
        notadir.write_text("", encoding="utf-8")
        expect_raises(probe_flake.Rejection,
                      lambda: probe_flake._check_shared_dir(notadir),
                      "a non-directory scratch path is refused",
                      "is not a directory")

    # The regression itself, cross-PROCESS and cross-TMPDIR: two
    # harnesses whose TMPDIRs differ must still contend for one port.
    scratch = Path(tempfile.mkdtemp(prefix="probe-flake-xtmp-"))
    try:
        holder = scratch / "holder.py"
        holder.write_text(
            HOLDER_SRC.format(tools=TOOLS_DIR, port=TMPDIR_TEST_PORT),
            encoding="utf-8")
        (scratch / "a").mkdir()
        (scratch / "b").mkdir()
        env_a = {**os.environ, "TMPDIR": str(scratch / "a")}
        env_b = {**os.environ, "TMPDIR": str(scratch / "b")}
        first_proc = subprocess.Popen([sys.executable, str(holder)],
                                      stdout=subprocess.PIPE, text=True,
                                      env=env_a)
        try:
            said = first_proc.stdout.readline().strip()
            expect(said.startswith("held "),
                   f"the first harness (TMPDIR A) holds the port ({said!r})")
            done = subprocess.run([sys.executable, str(holder)],
                                  capture_output=True, text=True,
                                  env=env_b, timeout=60)
            other = (done.stdout.strip().splitlines() or [""])[0]
            expect(other.startswith("missed "),
                   f"a harness under a DIFFERENT TMPDIR is blocked by it "
                   f"({other!r})")
            expect(" " in said and " " in other
                   and said.split(" ", 1)[1] == other.split(" ", 1)[1],
                   f"both resolved the SAME lease namespace "
                   f"({said!r} vs {other!r})")
        finally:
            first_proc.kill()
            first_proc.wait(timeout=30)
    finally:
        shutil.rmtree(scratch, ignore_errors=True)


def _dead_pid() -> int:
    """A pid that is certainly not running: spawn one and reap it."""
    proc = subprocess.Popen([sys.executable, "-c", "pass"])
    proc.wait()
    return proc.pid


def test_concurrency_accounting() -> None:
    print("\n-- concurrency accounting --")
    with SyntheticTree() as tree:
        with probe_flake.LiveRegistry() as solo:
            expect(solo.sample() == 1,
                   "a solo harness observes a concurrency of 1 (itself)")
            with probe_flake.LiveRegistry() as other:
                expect(solo.sample() == 2,
                       "a second live invocation is counted by the first")
                expect(other.sample() == 2,
                       "and by the second")
            expect(solo.peak == 2,
                   "the recorded concurrency is the PEAK, not the current count")
            expect(solo.sample() == 1,
                   "a departed invocation stops being counted")

        # A concurrent startup must never lose a live registration: a
        # harness publishing its own entry while others scan the same
        # directory used to be readable as an empty, "corrupt" file and
        # be unlinked, erasing a still-running harness from every later
        # sample.
        harnesses = 8
        ready = threading.Barrier(harnesses)
        observed: list[int] = []
        errors: list[str] = []
        lock = threading.Lock()

        def start_one() -> None:
            try:
                with probe_flake.LiveRegistry() as registry:
                    ready.wait(timeout=30)
                    for _ in range(20):
                        registry.sample()
                    with lock:
                        observed.append(registry.peak)
                        if not registry.path.exists():
                            errors.append(f"{registry.path} was unlinked while "
                                          f"its owner was still live")
            except Exception as error:  # noqa: BLE001
                with lock:
                    errors.append(f"{type(error).__name__}: {error}")

        threads = [threading.Thread(target=start_one) for _ in range(harnesses)]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join(timeout=60)
        expect(not errors, f"concurrent startups keep every registration "
                           f"({errors[:3]})")
        expect(len(observed) == harnesses and all(v == harnesses
                                                  for v in observed),
               f"every concurrent harness sees all {harnesses} of them "
               f"(observed {sorted(observed)})")
        leftovers = list(probe_flake.LEASE_ROOT.glob(
            probe_flake._registration_glob()))
        expect(leftovers == [],
               f"every departed harness cleans its registration up ({leftovers})")

        live_dir = probe_flake.LEASE_ROOT
        PREFIX = probe_flake.SHARED_PREFIX

        # THE PID-REUSE CASE. An abandoned registration naming a pid the
        # operating system has since handed to an unrelated live
        # process — modelled exactly, and most sharply, by naming THIS
        # process's own pid. A pid-and-age test reads it as a second
        # live harness forever and inflates every later measurement;
        # the lock test sees an unheld file and reaps it.
        recycled = live_dir / f"{PREFIX}-live-{os.getpid()}-recycled.json"
        recycled.write_text(
            json.dumps({"pid": os.getpid(), "started": 0.0}), encoding="utf-8")
        os.utime(recycled, (0, 0))
        with probe_flake.LiveRegistry() as registry:
            expect(registry.sample() == 1,
                   "a registration naming a REUSED (live) pid is not counted")
        expect(not recycled.exists(),
               "a registration naming a reused pid is reaped, not trusted")

        # The same, with a pid that is merely gone, and with a corrupt
        # entry: neither needs an age heuristic any more.
        stale = live_dir / f"{PREFIX}-live-999999-deadbeef.json"
        stale.write_text(json.dumps({"pid": _dead_pid(), "started": 0.0}),
                         encoding="utf-8")
        garbage = live_dir / f"{PREFIX}-live-garbage.json"
        garbage.write_text("not json", encoding="utf-8")
        fresh = live_dir / f"{PREFIX}-live-77777-justwritten.json"
        fresh.write_text("", encoding="utf-8")          # current mtime
        with probe_flake.LiveRegistry() as registry:
            expect(registry.sample() == 1,
                   "abandoned registrations are not counted whatever they say")
        expect(not stale.exists() and not garbage.exists()
               and not fresh.exists(),
               "an unheld registration is reaped immediately — being recent "
               "no longer protects it, because a live one is always locked")

        # And the converse: a registration a LIVE process holds is
        # counted even though this process could never verify its pid,
        # and killing that process outright makes it reapable with no
        # staleness window at all.
        holder_src = tree.root / "registry_holder.py"
        holder_src.write_text(
            REGISTRY_HOLDER_SRC.format(tools=TOOLS_DIR,
                                       root=str(probe_flake.LEASE_ROOT)),
            encoding="utf-8")
        holder = subprocess.Popen([sys.executable, str(holder_src)],
                                  stdout=subprocess.PIPE, text=True)
        held_path = None
        try:
            held_path = Path(holder.stdout.readline().strip())
            expect(held_path.exists(),
                   f"another process published its registration ({held_path})")
            with probe_flake.LiveRegistry() as registry:
                expect(registry.sample() == 2,
                       "a registration held by a live PROCESS is counted")
            expect(held_path.exists(),
                   "and is never reaped while its owner runs")
        finally:
            holder.kill()
            holder.wait(timeout=30)
        counted = None
        for _ in range(50):
            with probe_flake.LiveRegistry() as registry:
                counted = registry.sample()
            if counted == 1:
                break
            time.sleep(0.1)
        expect(counted == 1,
               "killing the owner outright drops its registration with no "
               "staleness heuristic at all")
        expect(held_path is not None and not held_path.exists(),
               "and the abandoned file is reaped")

        m = run_synthetic(tree, "pass", runs=1)
        expect(m.peak_concurrency >= 1,
               "a measurement records its own peak observed concurrency")


TESTS = (
    test_ports,
    test_port_spans,
    test_measure_leases_the_probes_whole_declared_span,
    test_concurrent_leasing,
    test_lease_root_is_tmpdir_independent,
    test_concurrency_accounting,
)
