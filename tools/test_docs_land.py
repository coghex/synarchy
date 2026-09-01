#!/usr/bin/env python3
"""Regression tests for tools/docs_land.sh (issue #1252).

The helper's whole reason to exist is that it "commits ONLY the paths you
name" while the docs worktree is deliberately holding OTHER unfinished
documents, and it pushes straight to master under admin bypass with no PR
review. Before #1252 it staged the named paths and then ran a bare
`git commit`, which records the entire index -- so an unrelated file
someone had already staged rode along, and in the boundary case (named
paths unchanged) the pushed commit contained nothing BUT that unrelated
file.

Every case here runs the real script against a hermetic, offline sandbox:
a local bare repository as `origin`, a main worktree on `master` and a
second worktree on `docs-wip`, because the script resolves both worktrees
BY BRANCH and unconditionally fetches and pushes. The sandbox overrides
HOME and the git config environment so a developer's global git settings
cannot make these pass or fail for unrelated reasons.

Usage:
  python3 tools/test_docs_land.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

import selftestlib
from selftestlib import FAILURES, expect

REPO_ROOT = Path(__file__).resolve().parent.parent
SCRIPT = REPO_ROOT / "tools" / "docs_land.sh"


# A 20-line body so an upstream edit to the LAST line and a local edit to
# the FIRST line merge cleanly. The force-override case needs a file that
# is dirty here and also changed upstream (to trip the risk predictor)
# without the autostash replay then conflicting for unrelated reasons.
B_SEED = "".join(f"b line {i}\n" for i in range(1, 21))


class Sandbox:
    """A throwaway repo trio: bare origin + master worktree + docs-wip."""

    def __init__(self, tmp: str) -> None:
        # realpath because git records resolved worktree paths and macOS
        # hands out /var/folders symlinks for temp directories.
        self.root = Path(os.path.realpath(tmp))
        self.origin = self.root / "origin.git"
        self.main = self.root / "main"
        self.docs = self.root / "docs"

        home = self.root / "home"
        home.mkdir()
        gitconfig = self.root / "gitconfig"
        gitconfig.write_text("", encoding="utf-8")

        # Drop every inherited GIT_* variable: GIT_DIR or GIT_WORK_TREE
        # leaking in from the caller would point the sandbox at the real
        # repository.
        env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        env.update({
            "HOME": str(home),
            "XDG_CONFIG_HOME": str(home / ".config"),
            "GIT_CONFIG_GLOBAL": str(gitconfig),
            "GIT_CONFIG_SYSTEM": os.devnull,
            "GIT_AUTHOR_NAME": "Docs Land Test",
            "GIT_AUTHOR_EMAIL": "docs-land@example.invalid",
            "GIT_COMMITTER_NAME": "Docs Land Test",
            "GIT_COMMITTER_EMAIL": "docs-land@example.invalid",
            "GIT_EDITOR": "true",
            "GIT_TERMINAL_PROMPT": "0",
            "LC_ALL": "C",
        })
        self.env = env
        self._build()

    # --- plumbing -----------------------------------------------------
    def git(self, *args: str, cwd: Path | None = None) -> str:
        done = subprocess.run(
            ["git", *args], cwd=str(cwd or self.main), env=self.env,
            capture_output=True, text=True)
        if done.returncode != 0:
            raise RuntimeError(
                f"sandbox git {' '.join(args)} failed:\n{done.stderr}")
        return done.stdout

    def _build(self) -> None:
        subprocess.run(["git", "init", "--bare", "-q", str(self.origin)],
                       env=self.env, check=True, capture_output=True)
        # Name the default branch explicitly: init.defaultBranch is `main`
        # on current git, and the script only knows `master`.
        self.git("symbolic-ref", "HEAD", "refs/heads/master", cwd=self.origin)

        self.main.mkdir()
        self.git("init", "-q")
        self.git("symbolic-ref", "HEAD", "refs/heads/master")
        self.git("config", "user.name", "Docs Land Test")
        self.git("config", "user.email", "docs-land@example.invalid")
        self.git("config", "commit.gpgsign", "false")

        (self.main / "docs").mkdir()
        self.write(self.main, "docs/a.md", "a seed\n")
        self.write(self.main, "docs/b.md", B_SEED)
        self.write(self.main, "docs/del.md", "del seed\n")
        self.write(self.main, "docs/keep.md", "keep seed\n")
        self.git("add", "-A")
        self.git("commit", "-q", "-m", "seed")
        self.git("remote", "add", "origin", str(self.origin))
        self.git("push", "-q", "-u", "origin", "master")
        self.git("worktree", "add", "-q", str(self.docs), "-b", "docs-wip",
                 "origin/master")

    # --- fixtures -----------------------------------------------------
    @staticmethod
    def write(wt: Path, rel: str, text: str) -> None:
        path = wt / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")

    def stage_unrelated_b(self, body: str = "b wip\n" + B_SEED) -> str:
        """Leave an unfinished, STAGED docs/b.md in the docs worktree."""
        self.write(self.docs, "docs/b.md", body)
        self.git("add", "docs/b.md", cwd=self.docs)
        return body

    def move_master_upstream(self, rel: str = "docs/upstream.md") -> None:
        """Advance origin/master via a path that is NOT dirty in docs/."""
        self.write(self.main, rel, "upstream\n")
        self.git("add", rel)
        self.git("commit", "-q", "-m", "upstream change")
        self.git("push", "-q", "origin", "master")

    # --- observations -------------------------------------------------
    def run_script(self, *args: str) -> subprocess.CompletedProcess:
        """Invoke the real script from the PRIMARY checkout, as documented."""
        return subprocess.run(
            ["bash", str(SCRIPT), *args], cwd=str(self.main), env=self.env,
            capture_output=True, text=True)

    def head(self, ref: str = "HEAD") -> str:
        return self.git("rev-parse", ref, cwd=self.docs).strip()

    def commit_files(self, ref: str = "HEAD") -> set[str]:
        out = self.git("diff-tree", "--no-commit-id", "--name-only", "-r",
                       ref, cwd=self.docs)
        return {line for line in out.splitlines() if line}

    def porcelain(self, wt: Path) -> dict[str, str]:
        out = self.git("status", "--porcelain", cwd=wt)
        return {line[3:]: line[:2] for line in out.splitlines() if line}

    def blob(self, ref: str, rel: str) -> str | None:
        try:
            return self.git("show", f"{ref}:{rel}", cwd=self.docs)
        except RuntimeError:
            return None

    def tracked(self, ref: str) -> set[str]:
        out = self.git("ls-tree", "-r", "--name-only", ref, cwd=self.docs)
        return {line for line in out.splitlines() if line}

    def index_state(self, wt: Path) -> str:
        return self.git("ls-files", "-s", cwd=wt)


def sandbox() -> tempfile.TemporaryDirectory:
    return tempfile.TemporaryDirectory(prefix="docs-land-test-")


# ---------------------------------------------------------------------
# Requirement 1 + 3: named-path isolation, master unmoved
# ---------------------------------------------------------------------
def test_unrelated_staged_file_is_not_landed() -> None:
    print("\n[#1252] unrelated STAGED file + named changed file, master unmoved")
    with sandbox() as tmp:
        sb = Sandbox(tmp)
        sb.write(sb.docs, "docs/a.md", "a landed\n")
        b_body = sb.stage_unrelated_b()
        # Unrelated UNSTAGED work too: requirement 3 covers both.
        sb.write(sb.docs, "docs/keep.md", "keep wip\n")

        done = sb.run_script("-m", "Land A", "docs/a.md")
        expect(done.returncode == 0,
               f"script succeeded (rc={done.returncode})\n{done.stderr}")

        # The primary guarantee, asserted directly on the commit itself.
        expect(sb.commit_files() == {"docs/a.md"},
               f"commit contains exactly the named path ({sb.commit_files()})")
        expect(sb.blob("origin/master", "docs/a.md") == "a landed\n",
               "the named path's new content reached origin/master")
        expect(sb.blob("origin/master", "docs/b.md") == B_SEED,
               "the unrelated staged file was NOT published")
        expect(sb.blob("origin/master", "docs/keep.md") == "keep seed\n",
               "the unrelated unstaged file was NOT published")

        # Requirement 3, unmoved-master case: each side keeps its exact
        # prior index disposition -- staged stays staged, dirty stays dirty.
        status = sb.porcelain(sb.docs)
        expect(status.get("docs/b.md") == "M ",
               f"unrelated staged work is still STAGED ({status})")
        expect(status.get("docs/keep.md") == " M",
               f"unrelated unstaged work is still UNSTAGED ({status})")
        expect((sb.docs / "docs/b.md").read_text(encoding="utf-8") == b_body,
               "unrelated staged work kept its worktree content")
        expect((sb.docs / "docs/keep.md").read_text(encoding="utf-8")
               == "keep wip\n",
               "unrelated unstaged work kept its worktree content")

        # Requirement 5: push verification and clean-primary fast-forward.
        expect("landed: docs-wip == origin/master" in done.stdout,
               "rev-list push verification still runs")
        expect("primary checkout fast-forwarded" in done.stdout,
               "clean primary checkout is still fast-forwarded")
        expect("master has not moved" in done.stdout,
               "the no-rebase fast path was taken")


# ---------------------------------------------------------------------
# Requirement 2: named paths unchanged -> no NEW commit, nothing published
# ---------------------------------------------------------------------
def test_unchanged_named_path_publishes_nothing() -> None:
    print("\n[#1252] named path unchanged + unrelated staged -> no new commit")
    with sandbox() as tmp:
        sb = Sandbox(tmp)
        b_body = sb.stage_unrelated_b()
        before = sb.head()
        before_upstream = sb.head("origin/master")

        done = sb.run_script("-m", "Land A", "docs/a.md")
        expect(done.returncode == 0,
               f"script succeeded (rc={done.returncode})\n{done.stderr}")
        expect("nothing staged from the named paths" in done.stdout,
               "the no-op was detected by an explicit scoped query")
        expect(sb.head() == before, "no commit was created")
        expect(sb.head("origin/master") == before_upstream,
               "origin/master did not move")
        expect(sb.blob("origin/master", "docs/b.md") == B_SEED,
               "the unrelated staged file was NOT published on its own")
        expect(sb.porcelain(sb.docs).get("docs/b.md") == "M ",
               f"unrelated work is still STAGED ({sb.porcelain(sb.docs)})")
        expect((sb.docs / "docs/b.md").read_text(encoding="utf-8") == b_body,
               "unrelated work kept its worktree content")


def test_unchanged_named_path_still_completes_an_interrupted_landing() -> None:
    print("\n[#1252] no-op is not an early exit: a prior local commit still lands")
    with sandbox() as tmp:
        sb = Sandbox(tmp)
        # Simulate a run interrupted after its commit but before its push.
        sb.write(sb.docs, "docs/a.md", "a landed\n")
        sb.git("add", "docs/a.md", cwd=sb.docs)
        sb.git("commit", "-q", "-m", "Land A", cwd=sb.docs)
        pending = sb.head()
        sb.stage_unrelated_b()

        done = sb.run_script("-m", "Land A", "docs/a.md")
        expect(done.returncode == 0,
               f"script succeeded (rc={done.returncode})\n{done.stderr}")
        expect("nothing staged from the named paths" in done.stdout,
               "the named paths were correctly seen as unchanged")
        expect(sb.head() == pending, "no second commit was created")
        expect(sb.head("origin/master") == pending,
               "the pre-existing landing commit was pushed and verified")
        expect(sb.blob("origin/master", "docs/b.md") == B_SEED,
               "the unrelated staged file was still NOT published")


# ---------------------------------------------------------------------
# Requirement 4: additions, modifications, deletions, multiple arguments
# ---------------------------------------------------------------------
def test_add_modify_delete_across_multiple_paths() -> None:
    print("\n[#1252] one invocation naming an addition, a modification, a deletion")
    with sandbox() as tmp:
        sb = Sandbox(tmp)
        sb.write(sb.docs, "docs/a.md", "a modified\n")
        sb.write(sb.docs, "docs/new.md", "brand new\n")
        (sb.docs / "docs/del.md").unlink()
        sb.stage_unrelated_b()

        done = sb.run_script("-m", "Land three", "docs/a.md", "docs/del.md",
                             "docs/new.md")
        expect(done.returncode == 0,
               f"script succeeded (rc={done.returncode})\n{done.stderr}")
        expect(sb.commit_files() == {"docs/a.md", "docs/del.md", "docs/new.md"},
               f"commit contains exactly the three named paths "
               f"({sb.commit_files()})")

        published = sb.tracked("origin/master")
        expect("docs/new.md" in published, "the addition landed")
        expect("docs/del.md" not in published, "the deletion landed")
        expect(sb.blob("origin/master", "docs/a.md") == "a modified\n",
               "the modification landed")
        expect(sb.blob("origin/master", "docs/b.md") == B_SEED,
               "the unrelated staged file was NOT published")
        expect(sb.porcelain(sb.docs).get("docs/b.md") == "M ",
               f"unrelated work is still STAGED ({sb.porcelain(sb.docs)})")


# ---------------------------------------------------------------------
# Requirement 3 + 5: isolation holds on the rebase path too
# ---------------------------------------------------------------------
def test_isolation_holds_when_the_rebase_path_runs() -> None:
    print("\n[#1252] master moved -> rebase path, isolation still holds")
    with sandbox() as tmp:
        sb = Sandbox(tmp)
        # Move master via a path that is NOT dirty here, so the pre-flight
        # risk predictor does not exit 3 before the rebase is exercised.
        sb.move_master_upstream()
        sb.write(sb.docs, "docs/a.md", "a landed\n")
        b_body = sb.stage_unrelated_b()

        done = sb.run_script("-m", "Land A", "docs/a.md")
        expect(done.returncode == 0,
               f"script succeeded (rc={done.returncode})\n{done.stderr}")
        expect("master moved; rebasing" in done.stdout,
               "the rebase path actually ran")
        expect(sb.commit_files() == {"docs/a.md"},
               f"the landing commit contains only the named path "
               f"({sb.commit_files()})")

        published = sb.tracked("origin/master")
        expect("docs/upstream.md" in published, "the upstream commit survived")
        expect(sb.blob("origin/master", "docs/a.md") == "a landed\n",
               "the named path landed on top of the moved master")
        expect(sb.blob("origin/master", "docs/b.md") == B_SEED,
               "the unrelated file was NOT published")
        # Amended requirement 3: on the rebase path only SURVIVAL is
        # required. git's autostash replays with `git stash apply` (never
        # --index), so staged entries legitimately come back unstaged.
        expect((sb.docs / "docs/b.md").read_text(encoding="utf-8") == b_body,
               "unrelated work survived the autostash uncommitted")
        expect("docs/b.md" in sb.porcelain(sb.docs),
               "unrelated work is still dirty, not silently committed")


# ---------------------------------------------------------------------
# Requirement 5: the pre-flight conflict predictor and its -f override
# ---------------------------------------------------------------------
def test_risk_predictor_and_force_override() -> None:
    print("\n[#1252] risk predictor still gates, and -f still overrides")
    with sandbox() as tmp:
        sb = Sandbox(tmp)
        # docs/b.md dirty here AND changed on master: exactly the
        # autostash-conflict shape the predictor exists to catch.
        sb.write(sb.main, "docs/b.md", B_SEED.replace("b line 20", "b upstream 20"))
        sb.git("add", "docs/b.md")
        sb.git("commit", "-q", "-m", "upstream touches b")
        sb.git("push", "-q", "origin", "master")

        sb.write(sb.docs, "docs/a.md", "a landed\n")
        b_body = "b local 1\n" + "".join(
            f"b line {i}\n" for i in range(2, 21))
        sb.write(sb.docs, "docs/b.md", b_body)
        sb.git("add", "docs/b.md", cwd=sb.docs)
        before = sb.head()
        before_upstream = sb.head("origin/master")

        blocked = sb.run_script("-m", "Land A", "docs/a.md")
        expect(blocked.returncode == 3,
               f"the predictor exits 3 (rc={blocked.returncode})")
        expect("dirty here AND changed on master" in blocked.stderr,
               "the warning names the risk")
        expect(sb.head() == before, "nothing was committed while blocked")
        expect(sb.head("origin/master") == before_upstream,
               "nothing was pushed while blocked")

        forced = sb.run_script("-f", "-m", "Land A", "docs/a.md")
        expect(forced.returncode == 0,
               f"-f proceeds (rc={forced.returncode})\n{forced.stderr}")
        expect(sb.commit_files() == {"docs/a.md"},
               f"the forced landing commit contains only the named path "
               f"({sb.commit_files()})")
        expect(sb.blob("origin/master", "docs/b.md")
               == B_SEED.replace("b line 20", "b upstream 20"),
               "the unrelated file's local edit was NOT published")
        expect((sb.docs / "docs/b.md").read_text(encoding="utf-8")
               == b_body.replace("b line 20", "b upstream 20"),
               "the unrelated local edit survived the autostash replay")


# ---------------------------------------------------------------------
# Requirement 5: dry run touches nothing
# ---------------------------------------------------------------------
def test_dry_run_changes_nothing() -> None:
    print("\n[#1252] dry run changes neither HEAD, index, worktree nor remote")
    with sandbox() as tmp:
        sb = Sandbox(tmp)
        sb.write(sb.docs, "docs/a.md", "a landed\n")
        b_body = sb.stage_unrelated_b()

        before = {
            "head": sb.head(),
            "upstream": sb.head("origin/master"),
            "index": sb.index_state(sb.docs),
            "status": sb.porcelain(sb.docs),
            "a": (sb.docs / "docs/a.md").read_text(encoding="utf-8"),
            "b": (sb.docs / "docs/b.md").read_text(encoding="utf-8"),
        }

        done = sb.run_script("-n", "-m", "Land A", "docs/a.md")
        expect(done.returncode == 0,
               f"dry run succeeded (rc={done.returncode})\n{done.stderr}")
        expect("would run: git commit" in done.stdout,
               "the dry run reported the commit it would make")

        expect(sb.head() == before["head"], "HEAD unchanged")
        expect(sb.head("origin/master") == before["upstream"],
               "origin/master unchanged")
        expect(sb.index_state(sb.docs) == before["index"], "index unchanged")
        expect(sb.porcelain(sb.docs) == before["status"],
               "working-tree status unchanged")
        expect((sb.docs / "docs/a.md").read_text(encoding="utf-8")
               == before["a"], "named file unchanged")
        expect((sb.docs / "docs/b.md").read_text(encoding="utf-8") == b_body,
               "unrelated file unchanged")


# ---------------------------------------------------------------------
# Requirement 6: macOS Bash 3.2 compatibility
# ---------------------------------------------------------------------
# `bash -n` is a SYNTAX parse under whichever bash is on PATH -- bash 5.x on
# CI and on any Homebrew Mac -- where every bash-4-only construct below parses
# cleanly. So the version constraint needs its own explicit check.
BASH4_ONLY = [
    (r"\bmapfile\b", "mapfile"),
    (r"\breadarray\b", "readarray"),
    (r"\bcoproc\b", "coproc"),
    (r"\b(?:declare|typeset|local)\s+(?:-[a-zA-Z]+\s+)*-[a-zA-Z]*A",
     "associative array declaration (declare -A)"),
    (r"\$\{[^}]*\^\^", "${var^^} case conversion"),
    (r"\$\{[^}]*,,", "${var,,} case conversion"),
    (r"&>>", "&>> append redirection"),
    (r"\[\[\s+-v\s", "[[ -v ]] (bash 4.2)"),
]


def code_lines() -> list[tuple[int, str]]:
    """The script's executable lines, with whole-line comments dropped.

    The script's own header documents the 3.2 constraint by NAMING the
    constructs it avoids ("no mapfile, ..."), so scanning raw text would
    fail on the very comment that records the rule.
    """
    return [(i, line)
            for i, line in enumerate(SCRIPT.read_text(encoding="utf-8")
                                     .splitlines(), 1)
            if not line.lstrip().startswith("#")]


def test_bash_32_compatible() -> None:
    print("\n[#1252] the script stays parseable and free of bash-4-only syntax")
    parsed = subprocess.run(["bash", "-n", str(SCRIPT)],
                            capture_output=True, text=True)
    expect(parsed.returncode == 0,
           f"bash -n parses the script\n{parsed.stderr}")
    lines = code_lines()
    expect(bool(lines), "the script has executable lines to scan")
    for pattern, label in BASH4_ONLY:
        hits = [f"line {i}" for i, line in lines if re.search(pattern, line)]
        expect(not hits, f"no {label} ({', '.join(hits) or 'absent'})")


def main() -> int:
    selftestlib.parse_verbose()
    if not SCRIPT.is_file():
        print(f"error: {SCRIPT} not found")
        return 1
    test_unrelated_staged_file_is_not_landed()
    test_unchanged_named_path_publishes_nothing()
    test_unchanged_named_path_still_completes_an_interrupted_landing()
    test_add_modify_delete_across_multiple_paths()
    test_isolation_holds_when_the_rebase_path_runs()
    test_risk_predictor_and_force_override()
    test_dry_run_changes_nothing()
    test_bash_32_compatible()
    if FAILURES:
        print(f"\n{len(FAILURES)} test(s) failed:")
        for failure in FAILURES:
            print(f"  {failure}")
        return selftestlib.concluded(1)
    return selftestlib.concluded(0, "\nAll docs_land tests passed")


if __name__ == "__main__":
    raise SystemExit(main())
