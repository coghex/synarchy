#!/usr/bin/env python3
"""Guard (#2648): no `test-headless/` module imports the production
`Engine.Core.Init.initializeEngineHeadless`.

Engine contracts §Headless fixture logging (#1925) requires every
`test-headless` engine to boot through `Test.Headless.Harness.Log`. The
production initializer hard-wires `LogToHandle stdout`, and the backend
can only be steered BEFORE initialization, so a fixture booting it
directly ignores `SYNARCHY_TEST_LOG=quiet`, cannot be redirected with
`SYNARCHY_TEST_LOG=stderr`, and writes every enabled initialization-time
category into the test runner's stdout. The contract said "never" and
nothing enforced it: #2121 landed the Chop authority fixture on the
production initializer after #2143 had migrated the whole suite. This
audit is what would have caught it.

WHAT IS DETECTED: an import declaration of exactly `Engine.Core.Init`
that brings `initializeEngineHeadless` into scope, qualified or not:

  * an import list naming it -- `import Engine.Core.Init
    (initializeEngineHeadless, EngineInitResult(..))`, over one line or
    several;
  * an UNRESTRICTED import -- `import Engine.Core.Init`,
    `import qualified Engine.Core.Init as I`, or the ImportQualifiedPost
    spelling -- because it exports every name, this one included;
  * a `hiding` import whose list does not hide it.

Never flagged: an import naming only other exports -- `EngineInitResult`,
`resolveConfigPath`, `migrateLegacyConfig` and the rest, which specs
across the suite legitimately use -- or the harness's own
`initializeEngineHeadlessWith`, and any mention inside a comment or a
string literal. An `Engine.Core.Init` import in a shape the classifier
does not model is a failure, not a pass.

WHAT IS READ: the source GHC compiles in THIS checkout's configured
build, not the file on disk (#2648 owner amendments). Cabal decides the
suite's real compiler arguments inside its own build code, per build way
and with the component's own `-tmp` output directory, writes them to a
temporary response file and deletes it; `build-info.json` records only a
generic approximation. So Setup.hs captures them at the build itself
(BuildSupport/GhcCapture.hs): for the duration of each package build the
configured `ghc` runs through BuildSupport/ghc-capture-wrapper.sh, which
records every command verbatim -- raw arguments and a byte copy of each
response file -- and runs the real compiler with it unchanged, RTS
options included. When the build succeeds, each unit it compiled gets a
fresh slot (`<dist>/build/ghc-capture/units/<unit id>`) whose `meta`
binds it to the package configuration (`setup-config`) and the compiler
program's bytes. A failed build publishes nothing.

The captured compiler itself reads each command back
(`read_invocations`): GHC's own `expandResponse` expands the response
files and GHC's own flag parser (`parseDynamicFlagsCmdLine`) identifies
the source arguments and the `--make` mode, which are dropped; RTS
sections are passed on. Each distinct build way is scanned. The replays
force-include the component's own generated `cabal_macros.h` and carry
every define, include directory, extension and package flag of the real
build, so GHC decides whether CPP runs and cpp expands directives,
splices, macros and every header it reads, wherever it lives, exactly as
the build did. Nothing about CPP or Cabal's component settings is
reconstructed here.

The imports are then read from that output with
`unicode_operator_audit.py`'s comment/string lexer (`haskell_code_only`,
MultilineStrings literals included) and `lua_strict_decode_audit.py`'s
layout-aware splitter (`haskell_import_declarations`), which follow
GHC's whitespace and eight-column tab stops. Masked comment tabs stay
tabs, and a line-1 `#!` line is blanked, as GHC skips it. A reported
line is mapped back through the `LINE` pragma and cpp line markers to
the module's own source line; header text maps to the module line that
included it. When a module suppresses the markers (`-optP-P`), the
report takes the one source line with the same text, or says the line is
the preprocessed text's.

RECORDING (`--record --builddir B -- <the build's own cabal args>`)
binds the capture to the configuration it came from. cabal-install
3.16.1.0 keeps building with an edited *imported* project file's old
settings and calls the build up to date, so neither Cabal's verdict nor
a post-build hash of the files proves which configuration was built.
The recorder therefore moves Cabal's project-configuration cache aside
and re-runs the build's own arguments as a dry run, which makes the
pinned cabal re-read every configuration input now; only an `Up to date`
answer is accepted. Cabal names what it read in its provenance messages
(`parse_provenance`: the files it was affected by, and each import it
fetched); those files, the absent `.local`/`.freeze` companions of each
root project file and the global config (`cabal path --config-file`) are
hashed. An edited import followed by a cached build is refused, and the
re-read makes the next build apply it. The selected build directory's
own plan locates the suite, and the capture slot must match the current
`setup-config` and compiler bytes.

FRESHNESS IS CONTENT IDENTITY: the gate (`configured_settings`) never
plans. It verifies the record's schema field by field, then that the
capture's content (`capture_fingerprint`: the same commands, however
many rebuilds ran them), `setup-config`, the header, the compiler
program's bytes -- those its configured path reaches now, so a
retargeted symlink counts -- and `--info`, every configuration input (a file
appearing counts) and the global config cabal names are all unchanged.
A byte-identical restored cache verifies whatever its mtimes. Source
files are deliberately not inputs: an edited module is what the gate is
for, and it is scanned as it now reads.

NO SETTINGS, NO VERDICT: a missing capture, slot or record; a record
missing or mistyping any field; anything recorded that changed; a build
not up to date with its own arguments once Cabal re-reads the
configuration; a cabal-install other than 3.16.1.0 or provenance output
in any shape it does not print; a build directory whose plan does not
hold the suite; a header that is not the component's own; a compiler
that is missing, differs from the `tested-with` pin or changed: each
stops the gate (exit 2) with the cause. A module `ghc -E` cannot
preprocess fails it (exit 1).

THE CERTIFIED ENVIRONMENT is the configured build that just ran: Linux
in CI's `test-and-audits`, and the developer's native configuration
under `tools/ci-local.sh`. Other platforms, flag settings, installed
tools and dependency versions are not predicted: a branch that only
another environment would take is that environment's build to check.

The self-test (`--self-test`, run in `static-audits`) needs no build and
no cabal: it drives the tracked wrapper with a fake compiler, decodes
fixture captures through GHC's own reader and parser (the `ghc`
library), parses provenance fixtures, verifies synthetic records, and
runs every lexical and CPP fixture through `ghc` on PATH or
`SYNARCHY_AUDIT_GHC` at the `tested-with` version. `--cabal-regression`
(run after the project build, in `test-and-audits` and
tools/ci-local.sh) builds a tiny Custom package through the repository's
own hook and wrapper with the pinned cabal-install and Setup Cabal,
offline, and records and scans it (`cabal_regression`). Source formats other than plain Haskell (`.lhs`,
`.hsig`, Cabal's `.hsc`/`.x`/`.y`), custom preprocessors (`-F -pgmF`) and
quasiquote contents are out of scope.

The single exemption is `Test.Headless.Harness.Log`, the boundary that
owns the backend choice (requirement 4). It is matched by exact path, so
a sibling module or a same-named file elsewhere is scanned as usual.

Usage:
  python3 tools/headless_init_import_audit.py --record --builddir dist-newstyle \\
      -- build synarchy-test-headless -v0     # right after the suite build
  python3 tools/headless_init_import_audit.py --builddir dist-newstyle  # gate
  python3 tools/headless_init_import_audit.py --cabal-regression
  python3 tools/headless_init_import_audit.py --self-test  # fixture suite
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from unicode_operator_audit import haskell_code_only  # type: ignore  # noqa: E402
from lua_strict_decode_audit import (  # type: ignore  # noqa: E402
    haskell_import_declarations)

SCOPED_TREE = "test-headless"
INIT_MODULE = "Engine.Core.Init"
BANNED_IDENTIFIER = "initializeEngineHeadless"
HARNESS_MODULE = "Test.Headless.Harness.Log"
CABAL_FILE = "synarchy.cabal"
HEADLESS_SUITE = "synarchy-test-headless"
GHC_ENV = "SYNARCHY_AUDIT_GHC"
CABAL_ENV = "SYNARCHY_AUDIT_CABAL"
PREPROCESS_TIMEOUT_SECONDS = 120

# Repo-relative path -> the reason it is exempt. Whole-file and exact.
EXEMPTIONS: dict[str, str] = {
    "test-headless/Test/Headless/Harness/Log.hs":
        "the harness is the one place a fixture's log backend is chosen, "
        "and wraps Engine.Core.Init's initializers (#1925)",
}


class AuditError(Exception):
    """A setup failure the gate cannot certify past: no compiler, the
    wrong compiler, or a configuration it cannot read."""


# `INIT_MODULE` as a whole module path, not the prefix of a longer one.
_MENTIONS_INIT_MODULE = re.compile(
    r"(?<![\w'.])" + re.escape(INIT_MODULE) + r"(?![\w'.])")

# Haskell 2010 report SS5.3 plus `safe` and GHC2024's
# ImportQualifiedPost: `import [safe] [qualified] Engine.Core.Init
# [qualified] [as Alias] [hiding] [( ... )]`. A `{-# SOURCE #-}` pragma
# or a package-import string is already blanked by the lexer.
_INIT_IMPORT = re.compile(
    r"\Aimport\s+(?:safe\s+)?(?:qualified\s+)?"
    + re.escape(INIT_MODULE) + r"(?![\w'.])"
    r"(?:\s+qualified(?![\w']))?"
    r"(?:\s+as\s+\w[\w']*(?:\.\w[\w']*)*)?"
    r"(?P<rest>[\s\S]*)\Z")

_HIDING = re.compile(r"\Ahiding(?![\w'])\s*(?P<list>[\s\S]*)\Z")

_BANNED_TOKEN = re.compile(
    r"(?<![\w'.])" + re.escape(BANNED_IDENTIFIER) + r"(?![\w'])")


@dataclass(frozen=True)
class Violation:
    path: str
    line: int
    reason: str

    def __str__(self) -> str:
        return f"{self.path}:{self.line}: {self.reason}"


def _line_of(text: str, pos: int) -> int:
    return text.count("\n", 0, pos) + 1


def _import_list(text: str) -> str | None:
    """The contents of a parenthesised import list that is the whole of
    `text`, or None when `text` is not exactly one balanced list."""
    if not text.startswith("("):
        return None
    depth = 0
    for i, char in enumerate(text):
        if char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
            if depth == 0:
                return text[1:i] if not text[i + 1:].strip() else None
    return None


def _classify(decl: str) -> str | None:
    """Why this `INIT_MODULE` import exposes the banned name, or None if
    it does not. Raises ValueError for a shape it does not model."""
    match = _INIT_IMPORT.match(decl)
    if match is None:
        raise ValueError("unrecognised import shape")
    rest = match.group("rest").strip()
    if not rest:
        return (f"unrestricted import of {INIT_MODULE} exposes "
                f"{BANNED_IDENTIFIER}")
    hiding = _HIDING.match(rest)
    names = _import_list(hiding.group("list") if hiding else rest)
    if names is None:
        raise ValueError("unterminated or trailing text after import list")
    named = _BANNED_TOKEN.search(names) is not None
    if hiding:
        return None if named else (
            f"hiding import of {INIT_MODULE} still exposes "
            f"{BANNED_IDENTIFIER}")
    return (f"imports {INIT_MODULE}.{BANNED_IDENTIFIER}"
            if named else None)




# ---------------------------------------------------------------------
# The configured build's settings
# ---------------------------------------------------------------------

_TESTED_WITH_GHC = re.compile(r"(?im)^tested-with\s*:.*?GHC\s*==\s*([0-9.]+)")
_PACKAGE_NAME = re.compile(r"(?im)^name\s*:\s*(\S+)\s*$")
HEADLESS_COMPONENT = f"test:{HEADLESS_SUITE}"
DEFAULT_BUILDDIR = "dist-newstyle"
# The cabal-install whose configuration-provenance messages the recorder
# reads (#2648 owner amendment 5912650157); .github/ci/Dockerfile pins it.
CABAL_PIN = "3.16.1.0"
STAMP_NAME = "headless-import-audit.json"
STAMP_SCHEMA = 2
# Where BuildSupport/GhcCapture.hs keeps each unit's last successful
# compiler commands, under the package's `<dist>/build`.
CAPTURE_UNITS = Path("ghc-capture") / "units"
# The compiler mode Cabal builds a component in; any other leftover
# flag in a captured command is a shape this gate does not model.
MAKE_MODE = "--make"


@dataclass(frozen=True)
class BuildSettings:
    """One way Cabal compiled the headless suite in this checkout: the
    compiler it ran and the exact arguments it ran it with, minus the
    source arguments GHC's own parser identifies, plus the header those
    arguments force-include."""
    ghc: str
    args: tuple[str, ...]
    header: Path
    description: str


def _load_json(path: Path, what: str) -> dict:
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except FileNotFoundError:
        raise AuditError(
            f"{what} {path} does not exist: build the headless suite first "
            f"(cabal build {HEADLESS_SUITE}), with `build-info: True` for "
            f"package synarchy in cabal.project") from None
    except (OSError, ValueError) as error:
        raise AuditError(f"{what} {path} is unreadable: {error}") from None
    if not isinstance(document, dict):
        raise AuditError(f"{what} {path} is not a JSON object")
    return document


def _compiler_version(ghc: str) -> str:
    try:
        return subprocess.run(
            [ghc, "--numeric-version"], capture_output=True, text=True,
            timeout=PREPROCESS_TIMEOUT_SECONDS, check=True).stdout.strip()
    except (OSError, subprocess.SubprocessError) as error:
        raise AuditError(f"{ghc} --numeric-version failed: {error}") from None


def _tested_with(root: Path) -> str:
    try:
        pin = _TESTED_WITH_GHC.search(
            (root / CABAL_FILE).read_text(encoding="utf-8"))
    except OSError as error:
        raise AuditError(f"{CABAL_FILE} is unreadable: {error}") from None
    if pin is None:
        raise AuditError(f"{CABAL_FILE} pins no `tested-with: GHC ==` version")
    return pin.group(1)


def _digest(path: Path, algorithm: str = "sha256") -> str | None:
    """The file's digest, or None when it does not exist."""
    try:
        return hashlib.new(algorithm, path.read_bytes()).hexdigest()
    except FileNotFoundError:
        return None
    except OSError as error:
        raise AuditError(f"{path} is unreadable: {error}") from None


def _text_digest(text: str) -> str:
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


# ---------------------------------------------------------------------
# cabal-install 3.16.1.0: the pinned tool and its provenance messages
# ---------------------------------------------------------------------

def cabal_command() -> str:
    """The cabal the build ran: `SYNARCHY_AUDIT_CABAL`, or `cabal` on
    PATH. Its provenance messages are read by their exact 3.16.1.0
    wording, so any other version is refused rather than guessed at."""
    requested = os.environ.get(CABAL_ENV) or "cabal"
    cabal = shutil.which(requested)
    if cabal is None:
        raise AuditError(f"{requested!r} is not an executable; set {CABAL_ENV}")
    try:
        version = subprocess.run([cabal, "--numeric-version"],
                                 capture_output=True, text=True, timeout=60,
                                 check=True).stdout.strip()
    except (OSError, subprocess.SubprocessError) as error:
        raise AuditError(f"{cabal} --numeric-version failed: {error}") from None
    if version != CABAL_PIN:
        raise AuditError(
            f"{cabal} is cabal-install {version}; the recorder reads "
            f"cabal-install {CABAL_PIN}'s configuration-provenance messages "
            f"and supports no other version (set {CABAL_ENV})")
    return cabal


_AFFECTED = "Configuration is affected by "
_AT_ROOT = re.compile(r"\A(?P<body>.*?) ?at '(?P<root>[^']*)'\.\Z")
_FETCHED = "fetching import: "
_IMPORTED_BY = re.compile(r"\A\s*imported by: (?P<path>.+)\Z")


def parse_provenance(output: str, root: Path) -> tuple[list[str], list[str]]:
    """`(root files, imported files)` that cabal-install 3.16.1.0 read in
    one `--verbose=debug+nowrap` run that re-parsed the project.

    The root files come from its `Configuration is affected by ...`
    message (cabal-install ProjectPlanning.hs `informAboutConfigFiles`),
    whose one- and two-file shapes print each file's *root project*, so
    `cabal.project and cabal.project` means one file was an import; the
    imports come from `fetching import: <path>`, logged for each import
    as it is read (ProjectConfig/Legacy.hs). Every path is relative to
    the project root the message names, which must be `root`. Any other
    shape, several messages, a quoted (untrimmed) path, a URL import or
    an inconsistent pair is an `AuditError`."""
    lines = output.splitlines()
    starts = [i for i, line in enumerate(lines) if line.startswith(_AFFECTED)]
    if len(starts) != 1:
        raise AuditError(
            f"cabal-install {CABAL_PIN} printed {len(starts)} `{_AFFECTED}"
            f"...` messages, not one; the configuration it read cannot be "
            f"identified")
    imports = [line[len(_FETCHED):] for line in lines
               if line.startswith(_FETCHED)]
    first = lines[starts[0]][len(_AFFECTED):]
    listed: list[str] = []
    listed_imports: set[str] = set()
    if first == "the following files:":
        tail = lines[starts[0] + 1:]
        end = next((i for i, line in enumerate(tail)
                    if line.startswith("at '")), None)
        if end is None:
            raise AuditError(f"cabal-install's file list has no `at '<root>'.`")
        project_root = _AT_ROOT.match(tail[end])
        for line in tail[:end]:
            imported = _IMPORTED_BY.match(line)
            if line.startswith("- "):
                listed.append(line[2:])
            elif imported and listed:
                listed_imports.add(listed[-1])
            else:
                raise AuditError(
                    f"unexpected line in cabal-install's file list: {line!r}")
        if len(listed) < 3:
            raise AuditError("cabal-install's file list names fewer than "
                             "three files")
        roots = [p for p in listed if p not in listed_imports]
        if set(imports) != listed_imports:
            raise AuditError(
                f"cabal-install fetched imports {sorted(imports)} but its file "
                f"list marks {sorted(listed_imports)} as imported")
    else:
        project_root = _AT_ROOT.match(first)
        body = project_root.group("body") if project_root else ""
        names = body.split(" and ") if body else []
        if len(names) == 1:
            if imports:
                raise AuditError("cabal-install names one file but fetched "
                                 f"imports {imports}")
            roots = names
        elif len(names) == 2 and names[0] != names[1]:
            if imports:
                raise AuditError("cabal-install names two root files but "
                                 f"fetched imports {imports}")
            roots = names
        elif len(names) == 2 and len(imports) == 1:
            roots = names[:1]
        else:
            raise AuditError(
                f"unrecognised cabal-install {CABAL_PIN} configuration "
                f"message: {lines[starts[0]]!r}")
    if project_root is None:
        raise AuditError(f"unrecognised end of cabal-install's configuration "
                         f"message")
    if Path(project_root.group("root")).resolve() != root.resolve():
        raise AuditError(f"cabal-install read the project at "
                         f"{project_root.group('root')}, not {root}")
    for path in [*roots, *imports]:
        if path != path.strip() or path.startswith("'") or "://" in path:
            raise AuditError(
                f"configuration input {path!r} is not a plain local file "
                f"(quoted, untrimmed or a URL); it cannot be tracked")
    return roots, imports


def configuration_closure(root: Path, roots: list[str], imports: list[str],
                          global_config: str) -> dict[str, str | None]:
    """Every configuration input as `{absolute path: sha256 or None}`:
    the files cabal read, the `.local`/`.freeze` companions of each root
    project file it would read if they appeared (tracked as absent), and
    the global config `cabal path --config-file` names (absent allowed)."""
    closure: dict[str, str | None] = {}
    for rel in [*roots, *imports]:
        path = (root / rel).resolve()
        digest = _digest(path)
        if digest is None:
            raise AuditError(f"configuration input {path} vanished")
        closure[str(path)] = digest
    for rel in roots:
        if rel.endswith((".local", ".freeze")):
            continue
        for suffix in (".local", ".freeze"):
            path = (root / (rel + suffix)).resolve()
            closure.setdefault(str(path), _digest(path))
    path = Path(global_config)
    closure[str(path)] = _digest(path)
    return closure


def _cabal_path(cabal: str, *queries: str) -> subprocess.CompletedProcess:
    """`cabal path <queries>` from an empty scratch directory: the global
    settings it reports do not depend on the project, and run inside the
    checkout it would refresh the project's configuration cache."""
    with tempfile.TemporaryDirectory() as scratch:
        return subprocess.run([cabal, "path", *queries], cwd=scratch,
                              capture_output=True, text=True, timeout=120)


def _global_config(cabal: str, root: Path) -> str:
    del root  # the global config does not depend on the project
    try:
        result = _cabal_path(cabal, "--config-file")
    except (OSError, subprocess.SubprocessError) as error:
        raise AuditError(f"cabal path --config-file could not run: {error}"
                         ) from None
    path = result.stdout.strip()
    if result.returncode != 0 or not path or "\n" in path:
        raise AuditError("cabal path --config-file named no single file: "
                         + " ".join((result.stdout + result.stderr).split()))
    return path


# ---------------------------------------------------------------------
# The captured compiler commands
# ---------------------------------------------------------------------

def split_rts(args: list[str]) -> tuple[list[str], list[str]]:
    """`(program arguments, RTS arguments)` by the rule a GHC-compiled
    program's runtime applies to its command line: `+RTS` opens a
    section and `-RTS` closes it, and `--RTS` ends RTS processing (it is
    consumed; everything after it is a program argument)."""
    program: list[str] = []
    rts: list[str] = []
    inside = False
    for index, arg in enumerate(args):
        if arg == "--RTS":
            rts.append(arg)
            program.extend(args[index + 1:])
            break
        if arg == "+RTS":
            inside = True
            rts.append(arg)
        elif arg == "-RTS" and inside:
            inside = False
            rts.append(arg)
        elif inside:
            rts.append(arg)
        else:
            program.append(arg)
    return program, rts


# The captured compiler reads each record with GHC's own response-file
# expansion and flag parser: the leftovers `parseDynamicFlagsCmdLine`
# returns are the command's source arguments and mode. Positions ride in
# each argument's location so equal strings stay distinct.
_CAPTURE_READER = """\
import Control.Exception (evaluate)
import Control.Monad.IO.Class (liftIO)
import GHC (getSessionDynFlags, runGhc)
import GHC.Data.FastString (fsLit, unpackFS)
import GHC.Driver.Session (parseDynamicFlagsCmdLine)
import GHC.ResponseFile (expandResponse)
import GHC.Types.SrcLoc
  (GenLocated (L), SrcSpan (UnhelpfulSpan), UnhelpfulSpanReason (UnhelpfulOther),
   mkGeneralSrcSpan)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  libdir : jobs <- getArgs
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let one (input, output) = do
          raw <- liftIO (readUtf8 input)
          args <- liftIO (expandResponse (splitNul raw))
          (_, left, _) <- parseDynamicFlagsCmdLine dflags
            [L (mkGeneralSrcSpan (fsLit (show i))) a | (i, a) <- zip [0 :: Int ..] args]
          let spans = [s | L s _ <- left]
              index (UnhelpfulSpan (UnhelpfulOther fs)) = unpackFS fs
              index _ = "?"
          liftIO $ withFile output WriteMode $ \\h -> do
            hSetEncoding h utf8
            hPutStr h (unwords (map index spans) ++ "\\n")
            mapM_ (\\a -> hPutStr h (a ++ "\\0")) args
    mapM_ one (pairs jobs)
  where
    pairs (a : b : rest) = (a, b) : pairs rest
    pairs _ = []
    splitNul s = case break (== '\\0') s of
      (a, _ : rest) -> a : splitNul rest
      (a, []) -> [a | not (null a)]
    readUtf8 path = do
      h <- openFile path ReadMode
      hSetEncoding h utf8
      text <- hGetContents h
      _ <- evaluate (length text)
      hClose h
      pure text
"""


@dataclass(frozen=True)
class Invocation:
    """One captured compiler command, read back."""
    record: str                 # the record directory's name
    args: tuple[str, ...]       # program arguments, response files expanded
    leftovers: tuple[int, ...]  # indices GHC's parser left unconsumed
    rts: tuple[str, ...]        # RTS sections, as passed


def read_invocations(ghc: str, slot: Path, cwd: Path) -> list[Invocation]:
    """Every record in a capture slot, decoded by `ghc` itself."""
    records = sorted(p for p in slot.iterdir() if p.name.startswith("inv."))
    if not records:
        raise AuditError(f"capture slot {slot} holds no compiler command")
    with tempfile.TemporaryDirectory() as tmp:
        jobs: list[str] = []
        rts_of: dict[str, list[str]] = {}
        for record in records:
            try:
                raw = (record / "argv").read_bytes().decode("utf-8")
            except (OSError, UnicodeDecodeError) as error:
                raise AuditError(f"{record}/argv is unreadable: {error}"
                                 ) from None
            fields = raw.split("\0")
            if fields[-1] != "":
                raise AuditError(f"{record}/argv is truncated")
            program, rts = split_rts(fields[:-1])
            rts_of[record.name] = rts
            staged: list[str] = []
            for index, arg in zip(_program_positions(fields[:-1]), program):
                if arg.startswith("@"):
                    copy = record / f"rsp.{index}"
                    if not copy.is_file():
                        raise AuditError(f"{copy} is missing: the response "
                                         f"file was not captured")
                    arg = "@" + str(copy)
                staged.append(arg)
            source = Path(tmp) / f"{record.name}.in"
            source.write_text("".join(a + "\0" for a in staged),
                              encoding="utf-8")
            jobs += [str(source), str(Path(tmp) / f"{record.name}.out")]
        reader = Path(tmp) / "CaptureReader.hs"
        reader.write_text(_CAPTURE_READER, encoding="utf-8")
        libdir = subprocess.run([ghc, "--print-libdir"], capture_output=True,
                                text=True, timeout=120).stdout.strip()
        command = [ghc, "-v0", "-package-env", "-", "-package", "ghc", "-e",
                   ":main " + " ".join(json.dumps(a) for a in [libdir, *jobs]),
                   str(reader)]
        try:
            result = subprocess.run(command, cwd=cwd, capture_output=True,
                                    text=True, timeout=600)
        except (OSError, subprocess.SubprocessError) as error:
            raise AuditError(f"{ghc} could not read the captured commands: "
                             f"{error}") from None
        if result.returncode != 0:
            raise AuditError(f"{ghc} could not read the captured commands: "
                             + " ".join(result.stderr.split())[:600])
        invocations = []
        for record in records:
            text = (Path(tmp) / f"{record.name}.out").read_text(
                encoding="utf-8")
            head, _, body = text.partition("\n")
            args = body.split("\0")[:-1]
            try:
                left = tuple(int(i) for i in head.split())
            except ValueError:
                raise AuditError(f"GHC's parser returned an unlocated "
                                 f"leftover for {record}") from None
            invocations.append(Invocation(record.name, tuple(args), left,
                                          tuple(rts_of[record.name])))
    return invocations


def _program_positions(args: list[str]) -> list[int]:
    """Indices of the program (non-RTS) arguments in `args`, in order;
    the wrapper numbers response-file copies by these positions."""
    program, _ = split_rts(args)
    positions: list[int] = []
    rts_open, done = False, False
    for index, arg in enumerate(args):
        if done:
            positions.append(index)
        elif arg == "--RTS":
            done = True
        elif arg == "+RTS":
            rts_open = True
        elif arg == "-RTS" and rts_open:
            rts_open = False
        elif not rts_open:
            positions.append(index)
    assert len(positions) == len(program), (positions, program)
    return positions


def replay_of(invocation: Invocation, unit_id: str) -> tuple[str, ...] | None:
    """The arguments to preprocess with, for a `--make` build of
    `unit_id`: the command minus the leftovers GHC's parser identified
    (its source arguments and mode), with its RTS sections kept. None
    for a command that is not such a build (linking a library, asking a
    version). Raises for a leftover flag other than `--make`."""
    args = invocation.args
    left = set(invocation.leftovers)
    modes = [args[i] for i in left if args[i].startswith("-")]
    if MAKE_MODE not in modes:
        return None
    if any(mode != MAKE_MODE for mode in modes):
        raise AuditError(f"captured command {invocation.record} has leftover "
                         f"flags {sorted(modes)}: a mode this gate does not "
                         f"model")
    unit = [args[i + 1] for i in range(len(args) - 1)
            if args[i] == "-this-unit-id" and i not in left]
    if unit != [unit_id]:
        return None
    return tuple(a for i, a in enumerate(args) if i not in left) + \
        invocation.rts


def _dedupe_key(replay: tuple[str, ...]) -> tuple[str, ...]:
    """A replay minus what only selects GHC's output (`-no-link`, `-o
    <file>`): the replay's own `-E -o` overrides both, so two commands
    differing only there preprocess identically."""
    key, skip = [], False
    for arg in replay:
        if skip:
            skip = False
        elif arg == "-o":
            skip = True
        elif arg != "-no-link":
            key.append(arg)
    return tuple(key)


def distinct_replays(invocations: list[Invocation], unit_id: str
                     ) -> list[tuple[str, ...]]:
    """One replay per distinct preprocessing configuration of the unit
    (compile and link commands of a way collapse; ways stay apart)."""
    seen: dict[tuple[str, ...], tuple[str, ...]] = {}
    for invocation in invocations:
        replay = replay_of(invocation, unit_id)
        if replay is not None:
            seen.setdefault(_dedupe_key(replay), replay)
    return list(seen.values())


def read_meta(slot: Path) -> dict[str, str]:
    """A capture slot's `meta`, every required key present."""
    try:
        text = (slot / "meta").read_text(encoding="utf-8")
    except OSError as error:
        raise AuditError(f"{slot}/meta is unreadable: {error}") from None
    meta = dict(line.split("\t", 1) for line in text.splitlines()
                if "\t" in line)
    missing = [key for key in _META_KEYS if not meta.get(key)]
    if missing:
        raise AuditError(f"{slot}/meta lacks {', '.join(missing)}")
    return meta


_META_KEYS = ("session", "package-root", "ghc", "ghc-canonical", "ghc-md5",
              "setup-config-md5", "cabal-library")


def capture_fingerprint(slot: Path) -> dict:
    """What a capture slot says, independent of which build wrote it:
    each record's command with every `@file` replaced by the bytes the
    wrapper copied (Cabal's temporary response-file names differ between
    runs), as a sorted list of digests, plus the slot's `meta` without
    its session. A later build that ran the same commands under the same
    configuration and compiler (`cabal test` rebuilds, for one) leaves it
    unchanged; any other command, a record added or lost, or another
    configuration or compiler changes it."""
    if not slot.is_dir():
        raise AuditError(
            f"{slot} does not exist: no successful build of the suite ran "
            f"through BuildSupport/GhcCapture.hs; rebuild the suite")
    digests = []
    for record in sorted(p for p in slot.iterdir() if p.name.startswith("inv.")):
        try:
            fields = (record / "argv").read_bytes().split(b"\0")[:-1]
            canonical = b""
            for index, field in enumerate(fields):
                if field.startswith(b"@"):
                    field = b"@" + (record / f"rsp.{index}").read_bytes()
                canonical += field + b"\0\0"
        except OSError as error:
            raise AuditError(f"capture record {record} is unreadable: {error}"
                             ) from None
        digests.append(hashlib.sha256(canonical).hexdigest())
    meta = read_meta(slot)
    return {"records": sorted(digests),
            **{key: meta[key] for key in _META_KEYS if key != "session"}}


# ---------------------------------------------------------------------
# Locating the selected build
# ---------------------------------------------------------------------

@dataclass(frozen=True)
class SelectedBuild:
    builddir: Path          # resolved
    dist_dir: Path          # the package's dist dir inside it
    unit_id: str            # the headless suite's unit
    info_path: Path         # its build-info.json
    slot: Path              # its capture slot
    stamp: Path


def select_build(root: Path, builddir: str) -> SelectedBuild:
    """The headless suite's build in `builddir` (relative to `root`), as
    that build directory's own plan names it."""
    selected = (root / builddir).resolve()
    plan = _load_json(selected / "cache" / "plan.json", "Cabal's build plan")
    try:
        package = _PACKAGE_NAME.search(
            (root / CABAL_FILE).read_text(encoding="utf-8")).group(1)
    except (OSError, AttributeError):
        raise AuditError(f"{CABAL_FILE} names no package") from None
    units = [u for u in plan.get("install-plan", [])
             if isinstance(u, dict) and u.get("pkg-name") == package
             and u.get("style") == "local"]
    unit = next((u for u in units if u.get("component-name")
                 == HEADLESS_COMPONENT), None) or next(
                     (u for u in units if u.get("component-name") is None), None)
    if unit is None or not unit.get("build-info") or not unit.get("dist-dir"):
        raise AuditError(
            f"{selected}/cache/plan.json has no local {package} unit with a "
            f"build-info path; build the suite in that build directory")
    dist_dir = Path(unit["dist-dir"]).resolve()
    if selected not in dist_dir.parents:
        raise AuditError(f"{selected}'s plan puts the package in {dist_dir}, "
                         f"outside the selected build directory")
    info_path = Path(unit["build-info"])
    if not info_path.exists():
        raise AuditError(
            f"{info_path} does not exist. Cabal writes it only when it "
            f"builds: force the suite to rebuild (remove "
            f"{info_path.parent / 'cache'}, then cabal build "
            f"{HEADLESS_SUITE}), with `build-info: True` for package "
            f"synarchy in cabal.project")
    info = _load_json(info_path, "Cabal's build information")
    component = next((c for c in info.get("components", [])
                      if isinstance(c, dict)
                      and c.get("name") == HEADLESS_COMPONENT), None)
    if component is None or not isinstance(component.get("unit-id"), str):
        raise AuditError(
            f"{info_path} has no {HEADLESS_COMPONENT} component: the last "
            f"build did not build the headless suite")
    if Path(component.get("src-dir", "")).resolve() != root.resolve():
        raise AuditError(f"{info_path} describes a build of "
                         f"{component.get('src-dir')}, not {root}")
    unit_id = component["unit-id"]
    return SelectedBuild(selected, dist_dir, unit_id, info_path,
                         dist_dir / "build" / CAPTURE_UNITS / unit_id,
                         dist_dir / STAMP_NAME)


def compiler_program(path: str) -> tuple[str, Path]:
    """`(the executable path runs, the file it reaches now)`. A configured
    compiler path can be a symlink (ghcup's always is); its identity is
    the bytes the path reaches when it runs, resolved afresh each time,
    never a target resolved earlier."""
    ghc = shutil.which(path)
    if ghc is None:
        raise AuditError(f"the compiler Cabal built with, {path!r}, is not an "
                         f"executable")
    return ghc, Path(os.path.realpath(ghc))


def captured_compiler(meta: dict[str, str]) -> tuple[str, Path]:
    """The capture slot's compiler, provided its configured path still
    reaches the bytes the build started with (`ghc-md5`); else an
    `AuditError`, whether the file was overwritten or the path now
    reaches another file."""
    ghc, reached = compiler_program(meta["ghc"])
    if _digest(reached, "md5") != meta["ghc-md5"]:
        raise AuditError(
            f"the compiler {meta['ghc']} now reaches {reached}, whose bytes "
            f"differ from the program the suite was built with (then "
            f"{meta['ghc-canonical']}); rebuild the suite and record again")
    return ghc, reached


def _checked_compiler(root: Path, path: str) -> str:
    ghc = shutil.which(path)
    if ghc is None:
        raise AuditError(f"the compiler Cabal built with, {path!r}, is not an "
                         f"executable")
    version, pin = _compiler_version(ghc), _tested_with(root)
    if version != pin:
        raise AuditError(f"the headless suite was built with GHC {version}, "
                         f"but {CABAL_FILE} pins GHC {pin}")
    return ghc


def _compiler_info(ghc: str) -> str:
    try:
        return subprocess.run([ghc, "--info"], capture_output=True, text=True,
                              timeout=120, check=True).stdout
    except (OSError, subprocess.SubprocessError) as error:
        raise AuditError(f"{ghc} --info failed: {error}") from None


def _own_header(root: Path, args: tuple[str, ...], unit_id: str,
                where: str) -> Path:
    header = next(
        (Path(args[i + 1][len("-optP"):]) for i in range(len(args) - 1)
         if args[i] == "-optP-include" and args[i + 1].startswith("-optP")),
        None)
    if header is None or header.parts[-3:] != (HEADLESS_SUITE, "autogen",
                                                "cabal_macros.h"):
        raise AuditError(
            f"{where} does not force-include {HEADLESS_COMPONENT}'s own "
            f"autogen/cabal_macros.h (found {header})")
    header = header if header.is_absolute() else root / header
    try:
        text = header.read_text(encoding="utf-8")
    except OSError as error:
        raise AuditError(
            f"the suite's generated {header} is unreadable: "
            f"{error.strerror}; re-run cabal build {HEADLESS_SUITE}") from None
    if f'#define CURRENT_COMPONENT_ID "{unit_id}"' not in text:
        raise AuditError(f"{header} does not belong to {unit_id}; re-run "
                         f"cabal build {HEADLESS_SUITE}")
    return header


# ---------------------------------------------------------------------
# --record
# ---------------------------------------------------------------------

def _builddir_in(args: list[str]) -> str | None:
    """The `--builddir` the cabal arguments select, if any."""
    found = None
    for index, arg in enumerate(args):
        if arg.startswith("--builddir="):
            found = arg.split("=", 1)[1]
        elif arg == "--builddir" and index + 1 < len(args):
            found = args[index + 1]
    return found


def _replan(cabal: str, root: Path, builddir: Path, args: list[str]) -> str:
    """Run the build's own cabal arguments as a dry run, after moving
    Cabal's project-configuration cache aside so it re-reads every
    configuration file now (cabal-install 3.16.1.0 re-reads an edited
    *imported* file only when a root file changes, and otherwise keeps
    building with the old settings). Cabal writes a fresh cache either
    way; the old one is restored only if cabal itself fails."""
    cache = builddir / "cache" / "config"
    moved = cache.with_name("config.audit-replaced")
    if cache.exists():
        os.replace(cache, moved)
    try:
        result = subprocess.run(
            [cabal, *args, "--dry-run", "--verbose=debug+nowrap"], cwd=root,
            capture_output=True, text=True, timeout=1800)
    except (OSError, subprocess.SubprocessError) as error:
        if moved.exists():
            os.replace(moved, cache)
        raise AuditError(f"cabal could not run: {error}") from None
    if result.returncode != 0:
        if moved.exists():
            if cache.exists():
                moved.unlink()
            else:
                os.replace(moved, cache)
        raise AuditError("cabal " + " ".join(args) + " --dry-run failed: "
                         + " ".join((result.stdout + result.stderr).split()
                                    )[-600:])
    if moved.exists():
        if cache.exists():
            moved.unlink()
        else:
            os.replace(moved, cache)
    return result.stdout + result.stderr


def record_settings(root: Path, builddir: str, cabal_args: list[str]) -> Path:
    """`--record`: bind the headless suite's captured compiler commands
    to the configuration they were built from, in `builddir`, using the
    build's own cabal arguments. See the module docstring."""
    if not cabal_args or cabal_args[0] != "build":
        raise AuditError("pass the build's own cabal arguments after `--`, "
                         "starting with `build`")
    if "--dry-run" in cabal_args:
        raise AuditError("the build's arguments cannot include --dry-run")
    named = _builddir_in(cabal_args)
    if (Path(root / (named or DEFAULT_BUILDDIR)).resolve()
            != (root / builddir).resolve()):
        raise AuditError(
            f"the cabal arguments build in {named or DEFAULT_BUILDDIR}, not "
            f"the selected {builddir}")
    cabal = cabal_command()
    output = _replan(cabal, root, (root / builddir).resolve(), cabal_args)
    if not any(line == "Up to date" for line in output.splitlines()):
        status = [line.strip() for line in output.splitlines()
                  if line.startswith(" - ")]
        raise AuditError(
            "the selected build is not up to date with these arguments "
            "once Cabal re-reads the checkout's configuration ("
            + "; ".join(status) + "). Cabal has now re-read it: rebuild with "
            f"the same arguments (cabal {' '.join(cabal_args)}) and record "
            f"again")
    roots, imports = parse_provenance(output, root)
    global_config = _global_config(cabal, root)
    closure = configuration_closure(root, roots, imports, global_config)
    build = select_build(root, builddir)
    if not build.slot.is_dir():
        raise AuditError(
            f"{build.slot} does not exist: no successful build of "
            f"{build.unit_id} in {build.builddir} ran through "
            f"BuildSupport/GhcCapture.hs; rebuild the suite")
    meta = read_meta(build.slot)
    if Path(meta["package-root"]).resolve() != root.resolve():
        raise AuditError(f"{build.slot} was captured in "
                         f"{meta['package-root']}, not {root}")
    setup_config = build.dist_dir / "setup-config"
    if _digest(setup_config, "md5") != meta["setup-config-md5"]:
        raise AuditError(
            f"{build.slot} was captured under another package configuration "
            f"than {setup_config}: the suite has not been built since Cabal "
            f"reconfigured; rebuild it and record again")
    _, ghc_real = captured_compiler(meta)
    ghc = _checked_compiler(root, meta["ghc"])
    invocations = read_invocations(ghc, build.slot, root)
    replays = distinct_replays(invocations, build.unit_id)
    if not replays:
        raise AuditError(f"{build.slot} holds no `--make` build of "
                         f"{build.unit_id}")
    headers = {_own_header(root, replay, build.unit_id,
                           f"captured command for {build.unit_id}")
               for replay in replays}
    if len(headers) != 1:
        raise AuditError(f"the captured ways force-include different headers "
                         f"{sorted(map(str, headers))}")
    header = headers.pop()
    document = {
        "schema": STAMP_SCHEMA,
        "checkout": str(root.resolve()),
        "builddir": str(build.builddir),
        "cabal": CABAL_PIN,
        "cabal-args": list(cabal_args),
        "unit-id": build.unit_id,
        "session": meta["session"],
        "capture": capture_fingerprint(build.slot),
        "replays": [list(r) for r in replays],
        "header": {"path": str(header), "sha256": _digest(header)},
        "setup-config": _digest(setup_config),
        "compiler": {"path": ghc, "canonical": str(ghc_real),
                     "sha256": _digest(ghc_real),
                     "info-sha256": _text_digest(_compiler_info(ghc))},
        "configuration": closure,
        "global-config": global_config,
    }
    build.stamp.write_text(json.dumps(document, indent=1) + "\n",
                           encoding="utf-8")
    return build.stamp


# ---------------------------------------------------------------------
# The gate's settings: the record, verified by content
# ---------------------------------------------------------------------

def _require(document: dict, key: str, kind: type, where: Path):
    value = document.get(key)
    if not isinstance(value, kind) or (kind in (str, list, dict) and not value):
        raise AuditError(f"{where} lacks a valid `{key}` ({kind.__name__}); "
                         f"record again")
    return value


def _string_map(value: dict, key: str, where: Path, nullable: bool = False
                ) -> dict[str, str | None]:
    for path, digest in value.items():
        if not isinstance(path, str) or not (
                isinstance(digest, str) or (nullable and digest is None)):
            raise AuditError(f"{where}'s `{key}` is malformed; record again")
    return value


def configured_settings(root: Path = REPO_ROOT,
                        builddir: str = DEFAULT_BUILDDIR
                        ) -> list[BuildSettings]:
    """The replays `--record` bound for `builddir`, after verifying by
    CONTENT that nothing they depend on changed: the capture itself, the
    package configuration, the header, the compiler program and its
    settings, and every configuration input (a file appearing counts).
    Source files are not inputs, so an injected import reaches the
    import diagnosis. Anything missing, malformed or different is an
    `AuditError` naming it."""
    build = select_build(root, builddir)
    stamp = build.stamp
    if not stamp.exists():
        raise AuditError(
            f"{stamp} does not exist: record the build's settings (python3 "
            f"tools/headless_init_import_audit.py --record --builddir "
            f"{builddir} -- <the build's cabal arguments>)")
    document = _load_json(stamp, "The recorded build settings")
    if document.get("schema") != STAMP_SCHEMA:
        raise AuditError(f"{stamp} has schema {document.get('schema')!r}, "
                         f"not {STAMP_SCHEMA}; record again")
    for key, kind in (("checkout", str), ("builddir", str), ("cabal", str),
                      ("cabal-args", list), ("unit-id", str), ("session", str),
                      ("capture", dict), ("replays", list), ("header", dict),
                      ("setup-config", str), ("compiler", dict),
                      ("configuration", dict), ("global-config", str)):
        _require(document, key, kind, stamp)
    if document["checkout"] != str(root.resolve()):
        raise AuditError(f"{stamp} records {document['checkout']}, not {root}")
    if document["builddir"] != str(build.builddir):
        raise AuditError(f"{stamp} records build directory "
                         f"{document['builddir']}, not {build.builddir}")
    if document["unit-id"] != build.unit_id:
        raise AuditError(f"{stamp} records {document['unit-id']}, not "
                         f"{build.unit_id}")
    replays = document["replays"]
    if not all(isinstance(r, list) and r and all(isinstance(a, str) for a in r)
               for r in replays):
        raise AuditError(f"{stamp}'s `replays` are malformed; record again")
    header = document["header"]
    compiler = document["compiler"]
    for part, keys in ((header, ("path", "sha256")),
                       (compiler, ("path", "canonical", "sha256",
                                   "info-sha256"))):
        if not all(isinstance(part.get(k), str) and part.get(k) for k in keys):
            raise AuditError(f"{stamp}'s header/compiler record is "
                             f"incomplete; record again")
    capture = document["capture"]
    configuration = _string_map(document["configuration"], "configuration",
                                stamp, nullable=True)
    changed: list[str] = []
    try:
        current_capture = capture_fingerprint(build.slot)
    except AuditError as error:
        current_capture = {"unreadable": str(error)}
    if current_capture != capture:
        changed.append(f"the captured commands in {build.slot} (the suite "
                       f"was rebuilt with other commands, configuration or "
                       f"compiler since recording)")
    if _digest(build.dist_dir / "setup-config") != document["setup-config"]:
        changed.append(f"{build.dist_dir / 'setup-config'}")
    if _digest(Path(header["path"])) != header["sha256"]:
        changed.append(f"the generated header {header['path']}")
    try:
        _, reached = compiler_program(compiler["path"])
        reached_digest = _digest(reached)
    except AuditError:
        reached, reached_digest = compiler["path"], None
    if reached_digest != compiler["sha256"]:
        changed.append(f"the compiler program {compiler['path']} (it now "
                       f"reaches {reached}; recorded {compiler['canonical']})")
    for path, digest in configuration.items():
        if _digest(Path(path)) != digest:
            changed.append(f"configuration input {path}")
    if changed:
        raise AuditError(
            "changed since the settings were recorded: " + "; ".join(changed)
            + f". Rebuild the suite (cabal {' '.join(document['cabal-args'])}) "
            f"and record again")
    ghc = _checked_compiler(root, compiler["path"])
    if _text_digest(_compiler_info(ghc)) != compiler["info-sha256"]:
        raise AuditError(f"{ghc} --info changed since the settings were "
                         f"recorded; rebuild the suite and record again")
    cabal = cabal_command()
    if _global_config(cabal, root) != document["global-config"]:
        raise AuditError("cabal now reads another global config than the one "
                         f"recorded ({document['global-config']})")
    for replay in replays:
        if _own_header(root, tuple(replay), build.unit_id,
                       f"{stamp}'s replay") != Path(header["path"]):
            raise AuditError(f"{stamp}'s replays and header disagree")
    description = (f"{HEADLESS_COMPONENT} as built in {build.builddir} "
                   f"({len(replays)} way{'s' if len(replays) != 1 else ''})")
    return [BuildSettings(ghc, tuple(r), Path(header["path"]), description)
            for r in replays]


def ghc_command(repo_root: Path = REPO_ROOT) -> str:
    """The GHC the self-test preprocesses its fixtures with: `ghc` on
    PATH, or `SYNARCHY_AUDIT_GHC`, at the `tested-with` version. The
    configured scan uses the compiler Cabal built with instead."""
    requested = os.environ.get(GHC_ENV) or "ghc"
    ghc = shutil.which(requested)
    if ghc is None:
        raise AuditError(
            f"{requested!r} is not an executable on PATH. This gate "
            f"preprocesses modules with the real GHC; install the pinned "
            f"toolchain (the version in {CABAL_FILE}'s tested-with) or set "
            f"{GHC_ENV}.")
    version, pin = _compiler_version(ghc), _tested_with(repo_root)
    if version != pin:
        raise AuditError(
            f"{ghc} is GHC {version}, but {CABAL_FILE} pins GHC {pin}; "
            f"preprocess with the pinned compiler (set {GHC_ENV} to its "
            f"path).")
    return ghc


# ---------------------------------------------------------------------
# Preprocessing and the source map
# ---------------------------------------------------------------------

# GHC's own `{-# LINE n "file" #-}` and cpp's `# n "file" flags`.
_LINE_MARKER = re.compile(
    r'\A(?:\{-#\s*LINE\s+(\d+)\s+"((?:[^"\\]|\\.)*)"\s*#-\}'
    r'|#\s*(?:line\s+)?(\d+)\s+"((?:[^"\\]|\\.)*)"[^\n]*)\Z')

@dataclass(frozen=True)
class Preprocessed:
    text: str                                # markers blanked
    origins: tuple[tuple[str, int], ...]     # per output line
    cpp_ran: bool
    # Per output line, the module line whose `#include` brought it in
    # (0 for the module's own lines).
    includers: tuple[int, ...] = ()


def map_output(output: str, source: str, verbatim: bool = False) -> Preprocessed:
    """GHC's `-E` output with its line markers blanked, each remaining
    line tagged with the `(file, line)` it came from. A line-1 `#!` line
    of the module itself is blanked too, as GHC's lexer skips it.

    `verbatim` says the output is GHC's untouched copy of the source
    behind its one `LINE` pragma, so no later line is a marker, however
    much a comment or string line looks like `# 7 "x.hs"`."""
    lines = output.split("\n")
    origins: list[tuple[str, int]] = []
    includers: list[int] = []
    current, number, cpp_ran, includer = source, 1, False, 0
    pending: list[int] = []     # header lines awaiting cpp's return marker
    for index, line in enumerate(lines):
        marker = _LINE_MARKER.match(line) if index == 0 or not verbatim else None
        if marker:
            cpp_ran = cpp_ran or marker.group(3) is not None
            target = (marker.group(2) if marker.group(2) is not None
                      else marker.group(4)).replace('\\"', '"')
            if current == source and target != source:
                # Entering a header from the module: the module line cpp
                # had reached, until its return marker says exactly.
                includer = number
            elif target == source and current != source:
                # Back in the module at line N: the `#include` was N - 1.
                for pending_index in pending:
                    includers[pending_index] = int(
                        marker.group(1) or marker.group(3)) - 1
                pending, includer = [], 0
            number = int(marker.group(1) or marker.group(3))
            current = target
            origins.append(("", 0))
            includers.append(0)
            lines[index] = " " * len(line)
            continue
        origins.append((current, number))
        includers.append(includer if current != source else 0)
        if current != source:
            pending.append(len(includers) - 1)
        if (current, number) == (source, 1) and line.startswith("#!"):
            lines[index] = " " * len(line)
        number += 1
    return Preprocessed("\n".join(lines), tuple(origins), cpp_ran,
                        tuple(includers))


def preprocess(settings: BuildSettings, root: Path, rel_path: str,
               work: Path) -> str:
    """`ghc -E` of one module with the configured arguments. Raises
    `PreprocessError` with GHC's message when it fails."""
    handle, name = tempfile.mkstemp(suffix=".hspp", dir=work)
    os.close(handle)
    output = Path(name)
    try:
        result = subprocess.run(
            [settings.ghc, "-E", "-v0", *settings.args, "-o", str(output),
             rel_path],
            cwd=root, capture_output=True, text=True,
            timeout=PREPROCESS_TIMEOUT_SECONDS)
        if result.returncode != 0:
            raise PreprocessError(
                "ghc -E failed: " + " ".join(result.stderr.split())[:600])
        return output.read_text(encoding="utf-8")
    except (OSError, subprocess.SubprocessError) as error:
        raise PreprocessError(f"ghc -E could not run: {error}") from None
    finally:
        output.unlink(missing_ok=True)


class PreprocessError(Exception):
    """GHC could not preprocess one module; the module cannot compile."""


def scan_preprocessed(pre: Preprocessed, rel_path: str
                      ) -> list[tuple[Violation, str]]:
    """Every banned import in GHC's preprocessed text of one module,
    each with the text of the output line it starts on."""
    # Blank what the lexer masked, keeping a tab a tab: GHC advances a
    # comment's tab to the next tab stop, and a space would shift every
    # later column on the line out of layout. MultilineStrings literals
    # are masked whether or not the module enables the extension: no
    # string literal can precede an import except a package qualifier,
    # so masking more can hide a false match but never a real import.
    code_text = "".join(
        ("\t" if original == "\t" else " ") if masked == "\0" else masked
        for masked, original in zip(
            haskell_code_only(pre.text, multiline_strings=True), pre.text))
    violations: list[tuple[Violation, str]] = []
    for start, _end, decl in haskell_import_declarations(code_text):
        if not _MENTIONS_INIT_MODULE.search(decl):
            continue
        try:
            reason = _classify(decl.strip())
        except ValueError as error:
            reason = (f"cannot classify this {INIT_MODULE} import ({error}); "
                      f"teach tools/headless_init_import_audit.py the shape "
                      f"rather than letting the file go unchecked:\n    "
                      + " ".join(decl.split()))
        if reason is None:
            continue
        index = _line_of(pre.text, start) - 1
        origin, line = pre.origins[index]
        if origin != rel_path:
            # Text from an `#include`: report the module's including line.
            own = [n for o, n in pre.origins[:index] if o == rel_path]
            reason += f" (from {origin}:{line})"
            line = (pre.includers[index] if pre.includers
                    and pre.includers[index] else own[-1] if own else 1)
        violations.append((Violation(rel_path, line, reason),
                           pre.text.split("\n")[index]))
    return violations


def check_module(settings: BuildSettings, root: Path, rel_path: str,
                 work: Path) -> list[Violation]:
    """Every banned import GHC sees in one module when it is compiled
    with the configured settings."""
    source = (root / rel_path).read_text(encoding="utf-8")
    verbatim = f'{{-# LINE 1 "{rel_path}" #-}}\n' + source
    try:
        output = preprocess(settings, root, rel_path, work)
    except PreprocessError as error:
        return [Violation(rel_path, 1, f"{error}; a module GHC cannot "
                          f"preprocess cannot be certified")]
    pre = map_output(output, rel_path, verbatim=output == verbatim)
    markerless = output != verbatim and not pre.cpp_ran
    violations = []
    for violation, text in scan_preprocessed(pre, rel_path):
        line, reason = violation.line, violation.reason
        if markerless:
            # Preprocessed, yet no cpp marker maps it back (`-optP-P`):
            # take the one source line with the same text, or say the
            # line is the preprocessed text's.
            same = [n for n, source_line in enumerate(source.split("\n"), 1)
                    if source_line.strip() == text.strip()]
            if len(same) == 1:
                line = same[0]
                reason += (" (cpp line markers were suppressed; located by "
                           "its text)")
            else:
                reason += (" (cpp line markers were suppressed, so the line "
                           "is the preprocessed text's)")
        violations.append(Violation(rel_path, line, reason))
    return sorted(set(violations), key=lambda v: (v.line, v.reason))


def _modules(root: Path) -> list[str]:
    tree = root / SCOPED_TREE
    paths = sorted({*tree.glob("**/*.hs"), *tree.glob("**/*.hs-boot")})
    return [rel for rel in (p.relative_to(root).as_posix() for p in paths)
            if rel not in EXEMPTIONS]


def scan_tree(root: Path, settings: BuildSettings) -> list[Violation]:
    """Every banned import in `root`'s `test-headless/` as GHC sees it
    under `settings`."""
    with tempfile.TemporaryDirectory() as work, \
            ThreadPoolExecutor(max_workers=min(8, os.cpu_count() or 2)) as pool:
        results = pool.map(
            lambda rel: check_module(settings, root, rel, Path(work)),
            _modules(root))
        return [violation for result in results for violation in result]


def scan_configured(root: Path, ways: list[BuildSettings]) -> list[Violation]:
    """Every banned import in `root`'s `test-headless/` under each way
    the suite was built, reported once."""
    found: dict[Violation, None] = {}
    for settings in ways:
        for violation in scan_tree(root, settings):
            found.setdefault(violation, None)
    return sorted(found, key=lambda v: (v.path, v.line, v.reason))


def find_violations(text: str, settings: BuildSettings,
                    rel_path: str = "Fixture.hs") -> list[Violation]:
    """One module's source checked under `settings`."""
    with tempfile.TemporaryDirectory() as tmp, \
            tempfile.TemporaryDirectory() as work:
        root = Path(tmp)
        (root / rel_path).write_text(text, encoding="utf-8")
        return check_module(settings, root, rel_path, Path(work))


# ---------------------------------------------------------------------
# Self-test
# ---------------------------------------------------------------------

_HEAD = "module M where\n"
_BANNED = "import Engine.Core.Init (initializeEngineHeadless)\n"
_ALLOWED = "import Engine.Core.Init (EngineInitResult(..))\n"
# Clean as plain Haskell; the banned import once CPP runs, because cpp
# honours a directive inside a Haskell comment and renames the allowed
# name. So a fixture ending in this reports its import line exactly when
# GHC preprocesses it.
_CPP_PROBE = ("{-\n#define EngineInitResult initializeEngineHeadless\n-}\n"
              + _ALLOWED)


_CPP = "{-# LANGUAGE CPP #-}\n"
_NO_MARKERS = _CPP + "{-# OPTIONS_GHC -optP-P #-}\n"
_ML = "{-# LANGUAGE MultilineStrings #-}\n"


def _branch(condition: str, taken: str, other: str = _ALLOWED) -> str:
    return f"#{condition}\n{taken}#else\n{other}#endif\n"


def _probe(header: str) -> tuple[str, list[int]]:
    """`header` + a module ending in `_CPP_PROBE`, with the probe's
    import line."""
    source = header + _HEAD + _CPP_PROBE
    return source, [source.count("\n")]


# `(label, source, lines that must be reported in order)`, each checked
# under the default configuration.
DETECTED_FIXTURES: list[tuple[str, str, list[int]]] = [
    ("the #2121 shape: an explicit list naming it",
     _HEAD + "import Engine.Core.Init (initializeEngineHeadless, "
     "EngineInitResult(..))\n", [2]),
    ("the name alone", _HEAD + _BANNED, [2]),
    ("a multiline list naming it on a continuation line",
     _HEAD + "import Engine.Core.Init\n"
     "  ( EngineInitResult(..)\n"
     "  , initializeEngineHeadless\n"
     "  )\n", [2]),
    ("a qualified import whose list names it",
     _HEAD + "import qualified Engine.Core.Init as I "
     "(initializeEngineHeadless)\n", [2]),
    ("an unrestricted import", _HEAD + "import Engine.Core.Init\n", [2]),
    ("an unrestricted qualified import",
     _HEAD + "import qualified Engine.Core.Init as I\n", [2]),
    ("an unrestricted qualified import with no alias",
     _HEAD + "import qualified Engine.Core.Init\n", [2]),
    ("an unrestricted ImportQualifiedPost import",
     _HEAD + "import Engine.Core.Init qualified as I\n", [2]),
    ("a multiline unrestricted qualified import",
     _HEAD + "import qualified\n  Engine.Core.Init\n    as I\n", [2]),
    ("a hiding import that hides something else",
     _HEAD + "import Engine.Core.Init hiding (initializeEngine)\n", [2]),
    ("a SOURCE pragma and a package import are blanked, not shields",
     _HEAD + "import {-# SOURCE #-} \"synarchy\" Engine.Core.Init\n", [2]),
    ("an indented top-level layout",
     _HEAD + "  " + _BANNED, [2]),
    ("a real import after a commented-out one is still found",
     _HEAD + "-- " + _ALLOWED + _BANNED, [3]),
    ("every offending import is reported",
     _HEAD + _BANNED + "import Data.IORef (newIORef)\n"
     "import qualified Engine.Core.Init as I\n", [2, 4]),
    ("an unmodelled shape fails rather than passing",
     _HEAD + "import Engine.Core.Init (EngineInitResult(..)) junk\n", [2]),
    ("an unterminated list fails rather than passing",
     _HEAD + "import Engine.Core.Init (EngineInitResult(..)\n", [2]),
    ("the first import on the `module ... where` line itself",
     "module M where import Engine.Core.Init (initializeEngineHeadless); "
     "fixture = initializeEngineHeadless\n", [1]),
    ("an unrestricted import on the `where` line, layout continuing below",
     "module M (spec) where import Engine.Core.Init\n"
     "                      import Data.IORef (newIORef)\n", [1]),
    ("an import at the start of a line indented with a non-breaking space",
     _HEAD + " " + _BANNED, [2]),
    ("a non-breaking space after an explicit-layout `;`",
     "module M where { import Data.IORef (newIORef); "
     "import Engine.Core.Init (initializeEngineHeadless) }\n", [1]),
    ("a block comment before the import on its own line",
     _HEAD + "{- note -} " + _BANNED, [2]),
    ("a list continued on lines indented with non-breaking spaces",
     _HEAD + "import Engine.Core.Init\n"
     "  ( EngineInitResult(..)\n"
     "  , initializeEngineHeadless )\n", [2]),
    ("a tab-containing comment before a banned import",
     _HEAD + "{-\t-} " + _BANNED
     + "           fixture = initializeEngineHeadless\n", [2]),
    ("a shebang holding a quote cannot swallow the imports",
     "#!/usr/bin/env runghc \"x\n" + _HEAD + _BANNED, [3]),
    ("a shebang holding `{-` cannot swallow the imports",
     "#!/bin/sh {-\n" + _HEAD + _BANNED, [3]),
    # CPP, as GHC runs it: every spelling below turns it on, and the
    # probe's import becomes the banned one.
    ("LANGUAGE CPP", *_probe("{-# LANGUAGE CPP #-}\n")),
    ("CPP in a LANGUAGE list",
     *_probe("{-# LANGUAGE OverloadedStrings, CPP #-}\n")),
    ("CPP in a LANGUAGE list spread over lines",
     *_probe("{-# LANGUAGE OverloadedStrings,\n             CPP #-}\n")),
    ("a lower-case pragma keyword", *_probe("{-# language CPP #-}\n")),
    ("no spaces inside the braces", *_probe("{-#LANGUAGE CPP#-}\n")),
    ("the name alone on its own line", *_probe("{-# LANGUAGE\nCPP\n#-}\n")),
    ("`CPP{- note -}`: a nested comment directly after the name (#2648 "
     "review)", *_probe("{-# LANGUAGE CPP{- note -} #-}\n")),
    ("a comment before the name inside the pragma",
     *_probe("{-# LANGUAGE {- a -} CPP #-}\n")),
    ("a line comment after the name inside the pragma",
     *_probe("{-# LANGUAGE CPP -- note\n #-}\n")),
    ("a pragma spaced with non-breaking spaces",
     *_probe("{-# LANGUAGE CPP #-}\n")),
    ("OPTIONS_GHC -XCPP", *_probe("{-# OPTIONS_GHC -Wall -XCPP #-}\n")),
    ("OPTIONS_GHC -cpp", *_probe("{-# OPTIONS_GHC -cpp #-}\n")),
    ("the deprecated OPTIONS pragma", *_probe("{-# OPTIONS -cpp #-}\n")),
    ("a quoted OPTIONS_GHC argument, as GHC's toArgs reads it (#2648 "
     "review)", *_probe("{-# OPTIONS_GHC \"-XCPP\" #-}\n")),
    ("a quoted argument followed by another",
     *_probe("{-# OPTIONS_GHC \"-XCPP\" -Wall #-}\n")),
    ("OPTIONS_GHC's bracketed list form",
     *_probe("{-# OPTIONS_GHC [\"-Wall\", \"-XCPP\"] #-}\n")),
    ("a numeric escape GHC decodes to -XCPP",
     *_probe("{-# OPTIONS_GHC \"-X\\67PP\" #-}\n")),
    ("a `\\&` escape GHC decodes to -XCPP",
     *_probe("{-# OPTIONS_GHC \"-XC\\&PP\" #-}\n")),
    ("after a line-1 shebang, which GHC skips",
     *_probe("#!/usr/bin/env runghc\n{-# LANGUAGE CPP #-}\n")),
    ("after an unrecognised pragma, which does not end the header",
     *_probe("{-# FOO bar #-}\n{-# LANGUAGE CPP #-}\n")),
    ("after comments and blank lines",
     *_probe("-- c\n\n{- b -}\n{-# LANGUAGE CPP #-}\n")),
    ("a later pragma switching CPP back on after NoCPP",
     *_probe("{-# LANGUAGE NoCPP #-}\n{-# OPTIONS_GHC -cpp #-}\n")),
    ("a module with no header line",
     "{-# LANGUAGE CPP #-}\n" + _CPP_PROBE + "main :: IO ()\nmain = pure ()\n",
     [5]),
    ("a line splice rebuilds the banned import with no directive (#2648 "
     "review)",
     "{-# LANGUAGE CPP #-}\n" + _HEAD + "import Engine.Core.\\\n"
     "Init (initializeEngineHeadless)\n"
     "fixture = initializeEngineHeadless\n", [3]),
    ("a splice later on the line still reports the line it starts on",
     "{-# LANGUAGE CPP #-}\n" + _HEAD + _ALLOWED
     + "import Engine.Core.Init (Engine\\\nInitResult(..), initialize\\\n"
     "EngineHeadless)\nx = 1\n", [4]),
    ("a /**/ comment pastes the module name together",
     "{-# LANGUAGE CPP #-}\n" + _HEAD
     + "import Engine.Core./**/Init (initializeEngineHeadless)\n", [3]),
    ("a macro defined in the file builds the module name",
     "{-# LANGUAGE CPP #-}\n" + _HEAD + "#define BOOT Engine.Core.Init\n"
     "import BOOT (initializeEngineHeadless)\n", [4]),
    ("a branch GHC takes on every supported host",
     "{-# LANGUAGE CPP #-}\n" + _HEAD
     + "#if defined(darwin_HOST_OS) || defined(linux_HOST_OS)\n" + _BANNED
     + "#endif\n", [4]),
    ("a module ghc -E cannot preprocess fails the gate: an undefined "
     "function-like macro in #if",
     "{-# LANGUAGE CPP #-}\n" + _HEAD + "#if MIN_VERSION_notadep(2,0,0)\n"
     + _ALLOWED + "#endif\n", [1]),
    ("a marker-suppressed banned import",
     _NO_MARKERS + _HEAD + _BANNED, [4]),
    # The configured header's dependency, package and tool macros
    # (#2648 review rounds 4-6): GHC takes the branch the build takes.
    ("`#ifdef VERSION_hspec`: hspec is a dependency, so Cabal defines it",
     _CPP + _HEAD + _branch("ifdef VERSION_hspec", _BANNED), [4]),
    ("`defined(MIN_VERSION_hspec)` is true under Cabal too",
     _CPP + _HEAD + _branch("if defined(MIN_VERSION_hspec)", _BANNED), [4]),
    ("a dependency with a dash in its name", _CPP + _HEAD
     + _branch("ifdef VERSION_cryptohash_sha256", _BANNED), [4]),
    ("base's version comes from the pinned GHC, exactly",
     _CPP + _HEAD + _branch("if MIN_VERSION_base(4,0,0)", _BANNED), [4]),
    ("the package's own version is exact",
     _CPP + _HEAD + _branch("if MIN_VERSION_synarchy(0,1,0)", _BANNED), [4]),
    ("`CURRENT_COMPONENT_ID` is defined, as Cabal defines it",
     _CPP + _HEAD + _branch("ifdef CURRENT_COMPONENT_ID", _BANNED), [4]),
    ("a dependency version comparison takes the header's real value",
     _CPP + _HEAD + _branch("if MIN_VERSION_hspec(2,0,0)", _BANNED), [4]),
    ("a host-tool macro the configured header defines", _CPP + _HEAD
     + _branch("ifdef TOOL_VERSION_ghc", _BANNED), [4]),
    ("a host-tool name split by a line splice (#2648 review round 5)",
     _CPP + _HEAD + _branch("if defined(TOOL_VERSION_\\\nghc)", _BANNED),
     [5]),
    ("`#ifdef` on a spliced host-tool name",
     _CPP + _HEAD + _branch("ifdef TOOL_VERSION_\\\nghc", _BANNED), [5]),
    ("a splice with trailing whitespace, as GCC and clang both accept",
     _CPP + _HEAD + _branch("ifdef TOOL_VERSION_\\ \nghc", _BANNED), [5]),
    ("a spliced MIN_TOOL_VERSION comparison", _CPP + _HEAD
     + _branch("if MIN_TOOL_\\\nVERSION_ghc(9,0,0)", _BANNED), [5]),
    ("a directive continued through a multiline C comment (#2648 review "
     "round 6)", _CPP + _HEAD + _branch(
         "if defined(/* note\n */ TOOL_VERSION_ghc)", _BANNED), [5]),
    ("a host-tool branch among many text mentions it expands in", _CPP
     + _HEAD + "-- TOOL_VERSION_ghc\n" * 25
     + _branch("ifdef TOOL_VERSION_ghc", _BANNED), [29]),
    # MultilineStrings (#2648 review round 4): real imports around the
    # literals are still read.
    ("a banned import before a multiline string with an embedded quote",
     _ML + _HEAD + _BANNED + 'message = """\n  A double quote: "\n  """\n',
     [3]),
    ("a banned import after a `\"\"\"` inside a comment and a line comment",
     _ML + _HEAD + '{- """ -}\n-- """\n' + _BANNED, [5]),
    ("a banned package-qualified import under MultilineStrings",
     _ML + _HEAD
     + 'import "synarchy" Engine.Core.Init (initializeEngineHeadless)\n', [3]),
    ("a comment line that looks like a cpp marker does not move the report",
     _HEAD + '{-\n# 7 "elsewhere.hs"\n-}\n' + _BANNED, [5]),
] + [
    (f"the `where`-line import after U+{ord(space):04X} -- GHC skips every "
     f"Unicode space separator, form feed and vertical tab (#2648 review)",
     f"module M where{space}import Engine.Core.Init "
     f"(initializeEngineHeadless); fixture = initializeEngineHeadless\n", [1])
    for space in (" ", "\f", "\v", " ", "　")
]

CLEAN_FIXTURES: list[tuple[str, str]] = [
    ("EngineInitResult-only, the shape across the suite", _HEAD + _ALLOWED),
    ("other exports over several lines",
     _HEAD + "import Engine.Core.Init\n"
     "  ( resolveConfigPath, migrateLegacyConfig\n"
     "  , LegacyNeutralityCheck(..) )\n"),
    ("the harness's own seam, a longer name",
     _HEAD + "import Engine.Core.Init (initializeEngineHeadlessWith, "
     "EngineInitResult(..))\n"),
    ("a qualified import with a clean list",
     _HEAD + "import qualified Engine.Core.Init as I (EngineInitResult(..))\n"),
    ("a hiding import that hides it",
     _HEAD + "import Engine.Core.Init hiding (initializeEngineHeadless)\n"),
    ("the harness's quiet entry point from its own module",
     _HEAD + "import Test.Headless.Harness.Log (initializeEngineHeadlessQuiet)\n"
     "f = initializeEngineHeadlessQuiet\n"),
    ("a line comment quoting the banned import", _HEAD + "-- " + _BANNED),
    ("a block comment quoting it over several lines",
     _HEAD + "{- import Engine.Core.Init\n     (initializeEngineHeadless) -}\n"),
    ("a haddock naming the qualified function",
     _HEAD + "-- | Never 'Engine.Core.Init.initializeEngineHeadless'.\n"
     + _ALLOWED),
    ("a string literal quoting it",
     _HEAD + "msg = \"import Engine.Core.Init (initializeEngineHeadless)\"\n"),
    ("a longer module path is a different module",
     _HEAD + "import Engine.Core.Init.Extra (initializeEngineHeadless)\n"),
    ("a different module exporting the same name",
     _HEAD + "import Other.Init (initializeEngineHeadless)\n"),
    ("the `where` keyword is matched whole", _HEAD + _ALLOWED + "nowhere = 0\n"),
    ("an allowed list continued on non-breaking-space-indented lines",
     _HEAD + "import Engine.Core.Init\n  ( EngineInitResult(..) )\n"),
    ("a tab-indented continuation sits at GHC's column 8, past an "
     "indented import's column 2",
     "module M where\n  import Engine.Core.Init\n\t(EngineInitResult(..))\n"
     "  x = 1\n"),
    ("a hiding list on a non-breaking-space-indented line",
     _HEAD + "import Engine.Core.Init hiding\n (initializeEngineHeadless)\n"),
    ("a tab inside a comment before the import keeps GHC's column 11 "
     "(#2648 review)",
     _HEAD + "{-\t-} " + _ALLOWED + "           fixture = EngineInitResult\n"),
    ("tabs in a multi-line comment before the import",
     _HEAD + "{- a\n\t\tb -}\t " + _ALLOWED + " " * 25
     + "fixture = EngineInitResult\n"),
    ("a shebang holding a quote hides nothing from a clean module",
     "#!/usr/bin/env runghc \"x\n" + _HEAD + _ALLOWED),
    # Text that looks like CPP but that GHC does not preprocess: each
    # ends in the probe, which only CPP would turn banned.
    ("quoted CPP text in a non-CPP module's comment (#2648 review)",
     _HEAD + _CPP_PROBE),
    ("a CPP pragma quoted in a string literal (#2648 review)",
     _HEAD + _CPP_PROBE + "message = \"{-# LANGUAGE CPP #-}\"\n"),
    ("a CPP pragma nested inside a block comment (#2648 review)",
     "{- {-# LANGUAGE CPP #-} -}\n" + _HEAD + _CPP_PROBE),
    ("a CPP pragma quoted in a haddock line comment",
     "-- | Enable with {-# LANGUAGE CPP #-}\n" + _HEAD + _CPP_PROBE),
    ("OPTIONS_GHC -XCPP quoted in a string literal",
     _HEAD + _CPP_PROBE + "flags = \"{-# OPTIONS_GHC -XCPP #-}\"\n"),
    ("a nested comment before the keyword: GHC no longer reads LANGUAGE",
     "{-# {- note -} LANGUAGE CPP #-}\n" + _HEAD + _CPP_PROBE),
    ("a CPP pragma after `module` is misplaced, and GHC ignores it",
     _HEAD + "{-# LANGUAGE CPP #-}\n" + _CPP_PROBE),
    ("CPP only inside a comment within LANGUAGE",
     "{-# LANGUAGE GADTs {- CPP -} #-}\n" + _HEAD + _CPP_PROBE),
    ("CPP only inside a line comment within LANGUAGE",
     "{-# LANGUAGE GADTs -- CPP\n #-}\n" + _HEAD + _CPP_PROBE),
    ("a later NoCPP in the same pragma wins",
     "{-# LANGUAGE CPP, NoCPP #-}\n" + _HEAD + _CPP_PROBE),
    ("-XNoCPP after -XCPP in OPTIONS_GHC",
     "{-# OPTIONS_GHC -XCPP -XNoCPP #-}\n" + _HEAD + _CPP_PROBE),
    ("a quoted literal glued to a word is one different argument",
     "{-# OPTIONS_GHC -Wall\"-XCPP\" #-}\n" + _HEAD + _CPP_PROBE),
    ("OPTIONS_GHC flags that merely contain `cpp` or `CPP`",
     "{-# OPTIONS_GHC -Wall -optP-DCPP -optP-cpp #-}\n" + _HEAD + _CPP_PROBE),
    ("a WARNING pragma mentioning CPP",
     _HEAD + "{-# WARNING fixture \"needs -XCPP and CPP\" #-}\n"
     "fixture = ()\n" + _CPP_PROBE.replace(_ALLOWED, "")),
    # CPP modules GHC does preprocess, whose imports stay allowed.
    ("a CPP module importing only allowed names", "{-# LANGUAGE CPP #-}\n"
     + _HEAD + _ALLOWED),
    ("a CPP module whose directives rewrite nothing banned",
     "{-# LANGUAGE CPP #-}\n" + _HEAD + "#define BOOT initializeEngineHeadless\n"
     + _ALLOWED),
    ("a CPP branch GHC skips on every supported host",
     "{-# LANGUAGE CPP #-}\n" + _HEAD
     + "#if defined(mingw32_HOST_OS)\n" + _BANNED + "#endif\n" + _ALLOWED),
    ("a CPP module quoting the banned import in a comment and a string",
     "{-# LANGUAGE CPP #-}\n" + _HEAD + _ALLOWED + "-- " + _BANNED
     + "msg = \"import Engine.Core.Init (initializeEngineHeadless)\"\n"),
    ("cpp's line marker for a long skipped branch sits inside the import "
     "and is not a line of code",
     "{-# LANGUAGE CPP #-}\n" + _HEAD + "import Engine.Core.Init\n"
     "#ifdef NOT_DEFINED\n" + "  -- skipped\n" * 12 + "#else\n"
     "  (EngineInitResult(..))\n#endif\nx = 1\n"),
    ("a marker-suppressing CPP module whose branches are all allowed",
     _NO_MARKERS + _HEAD + _branch("ifdef DARWIN", _ALLOWED)),
    ("`#ifndef VERSION_hspec`: Cabal defines it, so the banned branch is "
     "skipped", _CPP + _HEAD + _branch("ifndef VERSION_hspec", _BANNED)),
    ("`#ifdef` on a package that is not a dependency",
     _CPP + _HEAD + _branch("ifdef VERSION_yaml_light", _BANNED)),
    ("base's exact version picks the allowed branch", _CPP + _HEAD
     + _branch("if MIN_VERSION_base(4,0,0)", _ALLOWED, _BANNED)),
    ("the package's exact version picks the allowed branch", _CPP + _HEAD
     + _branch("if MIN_VERSION_synarchy(0,1,0)", _ALLOWED, _BANNED)),
    ("a dependency version macro used as a value, outside a conditional",
     _CPP + _HEAD + _ALLOWED + "v :: String\nv = VERSION_hspec\n"),
    ("a host-tool macro named in a non-CPP module's comment",
     _HEAD + _ALLOWED + "-- #ifdef TOOL_VERSION_alex\n"),
    ("a host-tool name in a CPP module's Haskell comment, not a directive",
     _CPP + _HEAD + _ALLOWED + "-- TOOL_VERSION_ghc is Cabal's\n"),
    ("a host-tool name inside a comment on a directive",
     _CPP + _HEAD + _branch("ifdef DARWIN /* not TOOL_VERSION_ghc */",
                            _ALLOWED)),
    ("a host tool the configured header does not define", _CPP + _HEAD
     + _branch("ifdef TOOL_VERSION_notatool", _BANNED)),
    ("a dependency version comparison the header's value rejects",
     _CPP + _HEAD + _branch("if MIN_VERSION_hspec(99,0,0)", _BANNED)),
    ("a host-tool name only inside a quoted #define value (#2648 review "
     "round 6)", _CPP + _HEAD + _ALLOWED + '#define MESSAGE "TOOL_VERSION_ghc"\n'
     "message :: String\nmessage = MESSAGE\n"),
    ("a directive-looking line inside a C comment", _CPP + _HEAD
     + "/*\n#ifdef TOOL_VERSION_ghc\n*/\n" + _ALLOWED),
    ("an ordinary condition continued through a multiline comment",
     _CPP + _HEAD + _branch("if defined(/* note\n */ DARWIN)", _ALLOWED)),
    ("a commented macro parameter with no paste", _CPP + _HEAD + _ALLOWED
     + "#define D(a /* note */) (a)\n"),
    ("a host-tool name in Haskell text of a markerless module",
     _NO_MARKERS + _HEAD + _ALLOWED + "-- TOOL_VERSION_ghc is Cabal's\n"),
    ("an ordinary spliced directive", _CPP + _HEAD
     + _branch("if defined(DAR\\\nWIN)", _ALLOWED, _ALLOWED)),
    ("an object-like macro pasting ordinary pieces", _CPP + _HEAD + _ALLOWED
     + "#define GREETING hel/**/lo\n"),
    ("a function-like macro with no paste", _CPP + _HEAD
     + "#define D(x) defined(x)\n" + _branch("if D(NOT_A_MACRO)", _ALLOWED)),
    ("a function-like macro whose comment joins no parameter", _CPP + _HEAD
     + _ALLOWED + "#define F(x) (x) /* note */\n"),
    ("the review's multiline string: an embedded quote, then import text "
     "(#2648 review round 4)",
     _ML + "module M (message) where\nimport Prelude (Char)\n"
     "message :: [Char]\nmessage = \"\"\"\n  A double quote: \"\n"
     "  import Engine.Core.Init (initializeEngineHeadless)\n  \"\"\"\n"),
    ("an escaped delimiter does not close the multiline string",
     _ML + _HEAD + _ALLOWED + 'message :: String\nmessage = """a\\"""\n'
     "  import Engine.Core.Init (initializeEngineHeadless)\n  \"\"\"\n"),
    ("a quote run at the close, comment openers and a marker-looking line "
     "inside", _ML + _HEAD + _ALLOWED + 'message :: String\nmessage = """\n'
     '  "" -- {- not comments\n# 1 "fake.hs"\n'
     "import Engine.Core.Init (initializeEngineHeadless)\n"
     '  tail quote\\""""\n'),
    ("a gap inside a multiline string", _ML + _HEAD + _ALLOWED
     + 'message :: String\nmessage = """gap\\   \\\n'
     "import Engine.Core.Init (initializeEngineHeadless)\n\"\"\"\n"),
    ("a multiline string in a CPP module", _ML + _CPP + _HEAD + _ALLOWED
     + 'message :: String\nmessage = """\n  a " quote\n'
     "  import Engine.Core.Init (initializeEngineHeadless)\n  \"\"\"\n"),
    ("a module without the extension: three quotes are two strings",
     _HEAD + _ALLOWED + "join :: String -> String -> String\n"
     "join a b = a <> b\n" 'joined :: String\njoined = join """x"\n'),
    ("a CPP module's tab-containing comment keeps GHC's columns",
     "{-# LANGUAGE CPP #-}\n" + _HEAD + "{-\t-} " + _ALLOWED
     + "           fixture = EngineInitResult\n"),
]

# `(label, source, substrings the single report must contain)`
REASON_FIXTURES: list[tuple[str, str, tuple[str, ...]]] = [
    ("a markerless report says how it was located",
     _NO_MARKERS + _HEAD + _BANNED, ("located by its text",)),
    ("a preprocessing failure quotes GHC and refuses certification",
     _CPP + _HEAD + "#if MIN_VERSION_notadep(1,0,0)\n#endif\n",
     ("ghc -E failed", "cannot be certified")),
]

# `(label, source)`: GCC's traditional cpp pastes these into the host-tool
# name the header defines and takes the banned branch; clang rejects the
# construct itself. Either way the module is reported, on different lines.
REPORTED_FIXTURES: list[tuple[str, str]] = [
    ("a macro body GCC pastes into a host-tool name", _CPP + _HEAD
     + "#define T defined(TOOL_VERSION_/**/ghc)\n" + _branch("if T", _BANNED)),
    ("a function-like macro that pastes its parameters",
     _CPP + _HEAD + "#define D(a,b) defined(a/**/b)\n"
     + _branch("if D(TOOL_VERS,ION_ghc)", _BANNED)),
    ("a commented macro signature that GCC pastes (#2648 review round 6)",
     _CPP + _HEAD + "#define D(a /* note */) defined(a/**/_ghc)\n"
     + _branch("if D(TOOL_VERSION)", _BANNED)),
    ("a comment between `#` and `define` (#2648 review round 6)",
     _CPP + _HEAD + "#/**/define D(a,b) defined(a/**/b)\n"
     + _branch("if D(TOOL_VERSION,_ghc)", _BANNED)),
]

# The configured header the fixtures run under, shaped as Cabal writes
# cabal_macros.h: dependency, package, host-tool and component macros.
def _fixture_header(dependencies: dict[str, str], tools: dict[str, str]) -> str:
    def version_macros(prefix: str, name: str, version: str) -> str:
        a, b, c = ([int(x) for x in version.split(".")] + [0, 0, 0])[:3]
        macro = name.replace("-", "_")
        return (f"#ifndef {prefix}VERSION_{macro}\n"
                f"#define {prefix}VERSION_{macro} \"{version}\"\n#endif\n"
                f"#ifndef MIN_{prefix}VERSION_{macro}\n"
                f"#define MIN_{prefix}VERSION_{macro}(major1,major2,minor) (\\\n"
                f"  (major1) <  {a} || \\\n"
                f"  (major1) == {a} && (major2) <  {b} || \\\n"
                f"  (major1) == {a} && (major2) == {b} && (minor) <= {c})\n"
                f"#endif\n")
    return ("/* DO NOT EDIT: This file is automatically generated by Cabal */\n"
            + "".join(version_macros("", n, v) for n, v in dependencies.items())
            + "".join(version_macros("TOOL_", n, v) for n, v in tools.items())
            + f'#ifndef CURRENT_COMPONENT_ID\n#define CURRENT_COMPONENT_ID '
              f'"synarchy-0.1.0.0-inplace-{HEADLESS_SUITE}"\n#endif\n'
              f'#ifndef CURRENT_PACKAGE_VERSION\n#define CURRENT_PACKAGE_VERSION '
              f'"0.1.0.0"\n#endif\n')


_DEPENDENCIES = {"base": "4.21.0.0", "hspec": "2.11.17", "synarchy": "0.1.0.0",
                 "cryptohash-sha256": "0.11.102.1"}
FIXTURE_HEADER = _fixture_header(_DEPENDENCIES, {"ghc": "9.12.2"})
NO_HSPEC_HEADER = _fixture_header(
    {k: v for k, v in _DEPENDENCIES.items() if k != "hspec"}, {"ghc": "9.12.2"})
# The headless suite's language arguments, as its build-info records them.
_FIXTURE_ARGS = ("-XGHC2024", "-XDefaultSignatures", "-XDuplicateRecordFields",
                 "-XMagicHash", "-XNoMonomorphismRestriction",
                 "-XNoImplicitPrelude", "-XNumDecimals", "-XOverloadedStrings",
                 "-XPatternSynonyms", "-XQuantifiedConstraints",
                 "-XRecordWildCards", "-XTypeFamilyDependencies",
                 "-XUnicodeSyntax", "-XViewPatterns", "-XQuasiQuotes",
                 "-hide-all-packages")


def fixture_settings(ghc: str, work: Path, extra: tuple[str, ...] = (),
                     header_text: str = FIXTURE_HEADER) -> BuildSettings:
    """Settings as `configured_settings` returns them, with a fixture
    header in place of a build's."""
    handle, name = tempfile.mkstemp(suffix="-cabal_macros.h", dir=work)
    os.close(handle)
    header = Path(name)
    header.write_text(header_text, encoding="utf-8")
    return BuildSettings(ghc, _FIXTURE_ARGS + extra + (
        "-optP-include", f"-optP{header}"), header, "fixture")


_DARWIN = ("-optP-DDARWIN",)

# `(label, extra configured arguments, header text, module source at
# test-headless/Test/M.hs, lines reported or None for "reported", files)`
TREE_FIXTURES: list[tuple[str, tuple[str, ...], str, str, list[int] | None,
                          dict[str, str]]] = [
    ("scan_tree reports the `where`-line import", (), FIXTURE_HEADER,
     "module M where import Engine.Core.Init (initializeEngineHeadless); "
     "fixture = initializeEngineHeadless\n", [1], {}),
    ("scan_tree reports it after a non-breaking space", (), FIXTURE_HEADER,
     "module M where " + _BANNED, [1], {}),
    ("scan_tree reports it after a form feed", (), FIXTURE_HEADER,
     "module M where\f" + _BANNED, [1], {}),
    ("scan_tree certifies an allowed list on non-breaking-space lines", (),
     FIXTURE_HEADER,
     _HEAD + "import Engine.Core.Init\n  ( EngineInitResult(..) )\n",
     [], {}),
    ("scan_tree certifies an import after a tab-containing comment", (),
     FIXTURE_HEADER,
     _HEAD + "{-\t-} " + _ALLOWED + "           fixture = EngineInitResult\n",
     [], {}),
    ("scan_tree ignores a string-quoted CPP pragma", (), FIXTURE_HEADER,
     _HEAD + _CPP_PROBE + "message = \"{-# LANGUAGE CPP #-}\"\n", [], {}),
    ("scan_tree ignores a CPP pragma nested in a block comment", (),
     FIXTURE_HEADER, "{- {-# LANGUAGE CPP #-} -}\n" + _HEAD + _CPP_PROBE,
     [], {}),
    ("scan_tree ignores a misplaced CPP pragma", (), FIXTURE_HEADER,
     _HEAD + "{-# LANGUAGE CPP #-}\n" + _CPP_PROBE, [], {}),
    ("scan_tree reads `LANGUAGE CPP{- note -}` as GHC does", (),
     FIXTURE_HEADER, "{-# LANGUAGE CPP{- note -} #-}\n" + _HEAD
     + "#define BOOT Engine.Core.Init\nimport BOOT (initializeEngineHeadless)\n"
     "fixture = initializeEngineHeadless\n", [4], {}),
    ("scan_tree reads a quoted `\"-XCPP\"` option", (), FIXTURE_HEADER,
     "{-# OPTIONS_GHC \"-XCPP\" #-}\n" + _HEAD
     + "#define BOOT Engine.Core.Init\nimport BOOT (initializeEngineHeadless)\n"
     "fixture = initializeEngineHeadless\n", [4], {}),
    ("scan_tree joins a line-spliced import", (), FIXTURE_HEADER,
     "{-# LANGUAGE CPP #-}\n" + _HEAD + "import Engine.Core.\\\n"
     "Init (initializeEngineHeadless)\nfixture = initializeEngineHeadless\n",
     [3], {}),
    ("scan_tree reports an import behind a quoting shebang", (),
     FIXTURE_HEADER, "#!/usr/bin/env runghc \"x\n" + _HEAD + _BANNED, [3], {}),
    # The configured arguments decide, for this host's build only.
    ("a cpp-option in the configured arguments", _DARWIN, FIXTURE_HEADER,
     _CPP + _HEAD + _branch("ifdef DARWIN", _BANNED), [4], {}),
    ("the same module where the configured arguments lack it", (),
     FIXTURE_HEADER, _CPP + _HEAD + _branch("ifdef DARWIN", _BANNED), [], {}),
    ("CPP enabled by the configured arguments", ("-XCPP",), FIXTURE_HEADER,
     _HEAD + _CPP_PROBE, [5], {}),
    ("a module's NoCPP turns configured CPP off", ("-XCPP",), FIXTURE_HEADER,
     "{-# LANGUAGE NoCPP #-}\n" + _HEAD + _CPP_PROBE, [], {}),
    ("-optP-P in the configured arguments", ("-optP-P",) + _DARWIN,
     FIXTURE_HEADER, _CPP + _HEAD + _branch("ifdef DARWIN", _BANNED), [4], {}),
    ("a marker-suppressing module under a configured cpp-option", _DARWIN,
     FIXTURE_HEADER, _NO_MARKERS + _HEAD + _branch("ifdef DARWIN", _BANNED),
     [5], {}),
    # The configured header decides.
    ("VERSION_hspec from the configured header", (), FIXTURE_HEADER,
     _CPP + _HEAD + _branch("ifdef VERSION_hspec", _BANNED), [4], {}),
    ("a configured header without that dependency", (), NO_HSPEC_HEADER,
     _CPP + _HEAD + _branch("ifdef VERSION_hspec", _BANNED), [], {}),
    ("a dependency version comparison takes the header's value", (),
     FIXTURE_HEADER, _CPP + _HEAD
     + _branch("if MIN_VERSION_hspec(2,0,0)", _BANNED), [4], {}),
    ("scan_tree certifies the review's multiline string", (), FIXTURE_HEADER,
     _ML + "module M (message) where\nimport Prelude (Char)\n"
     "message :: [Char]\nmessage = \"\"\"\n  A double quote: \"\n"
     "  import Engine.Core.Init (initializeEngineHeadless)\n  \"\"\"\n",
     [], {}),
    ("scan_tree reports a banned import before a multiline string", (),
     FIXTURE_HEADER, _ML + _HEAD + _BANNED
     + 'message :: String\nmessage = """\n  a " quote\n  """\n', [3], {}),
    # Headers: whatever cpp reads with the configured arguments.
    ("a header selects the banned import under the header's tool macro, "
     "markers suppressed (#2648 review round 5)", (), FIXTURE_HEADER,
     _NO_MARKERS + _HEAD + '#include "tool.h"\n', None,
     {"test-headless/Test/tool.h": _branch("ifdef TOOL_VERSION_ghc", _BANNED)}),
    ("a nested include's spliced tool test, markers suppressed", (),
     FIXTURE_HEADER, _NO_MARKERS + _HEAD + '#include "outer.h"\n', None,
     {"test-headless/Test/outer.h": '#include "inner.h"\n',
      "test-headless/Test/inner.h": "#ifdef TOOL_VERSION_gh\\\nc\n" + _BANNED
      + "#endif\n"}),
    ("a header included only under a configured cpp-option", _DARWIN,
     FIXTURE_HEADER, _CPP + _HEAD + '#ifdef DARWIN\n#include "tool.h"\n#endif\n',
     [4], {"test-headless/Test/tool.h": _branch("ifdef TOOL_VERSION_ghc",
                                                 _BANNED)}),
    ("the same header where the configured arguments lack the option", (),
     FIXTURE_HEADER, _CPP + _HEAD + '#ifdef DARWIN\n#include "tool.h"\n#endif\n'
     + _ALLOWED, [], {"test-headless/Test/tool.h": _branch(
         "ifdef TOOL_VERSION_ghc", _BANNED)}),
    ("a clean header in a markerless module", (), FIXTURE_HEADER,
     _NO_MARKERS + _HEAD + '#include "ok.h"\n', [],
     {"test-headless/Test/ok.h": _branch("ifdef DARWIN", _ALLOWED)}),
    ("a header in an include directory outside the tree (#2648 review "
     "round 6)", ("-I../ext",), FIXTURE_HEADER,
     _CPP + _HEAD + '#include "tool.h"\n', [3],
     {"../ext/tool.h": _branch("ifdef TOOL_VERSION_ghc", _BANNED)}),
    ("a clean header in an include directory outside the tree",
     ("-I../ext",), FIXTURE_HEADER, _CPP + _HEAD + '#include "ok.h"\n', [],
     {"../ext/ok.h": _branch("ifdef DARWIN", _ALLOWED)}),
    ("a header reached through a symlinked include directory",
     ("-Ilinked",), FIXTURE_HEADER, _NO_MARKERS + _HEAD + '#include "tool.h"\n',
     None, {"../ext/tool.h": _branch("ifdef TOOL_VERSION_ghc", _BANNED),
            "linked": "@symlink:../ext"}),
    ("a header in a directory whose path has a space", ("-I../ext dir",),
     FIXTURE_HEADER, _CPP + _HEAD + '#include "t.h"\n', [3],
     {"../ext dir/t.h": _branch("ifdef TOOL_VERSION_ghc", _BANNED)}),
    ("a header testing a tool the configured host lacks", (), FIXTURE_HEADER,
     _CPP + _HEAD + '#include "tool.h"\n', [],
     {"test-headless/Test/tool.h": _branch("ifdef TOOL_VERSION_happy",
                                           _BANNED)}),
    ("scan_tree reports a directive continued through a multiline comment "
     "(#2648 review round 6)", (), FIXTURE_HEADER, _CPP + _HEAD
     + _branch("if defined(/* note\n */ TOOL_VERSION_ghc)", _BANNED), [5], {}),
    ("scan_tree certifies a quoted host-tool name (#2648 review round 6)",
     (), FIXTURE_HEADER, _CPP + _HEAD + _ALLOWED
     + '#define MESSAGE "TOOL_VERSION_ghc"\nmessage :: String\n'
     "message = MESSAGE\n", [], {}),
    ("Haskell text naming a tool macro in a markerless module's header", (),
     FIXTURE_HEADER, _NO_MARKERS + _HEAD + '#include "text.h"\n', [],
     {"test-headless/Test/text.h": _ALLOWED + "-- TOOL_VERSION_ghc\n"}),
    ("an #include is reported at the including line", (), FIXTURE_HEADER,
     _CPP + _HEAD + '#include "banned.h"\n', [3],
     {"test-headless/Test/banned.h": _BANNED}),
]

_BANNED_SOURCE = _HEAD + _BANNED

# `(relative path, expected to be reported)` for the exemption's exactness.
EXEMPTION_FIXTURES: list[tuple[str, bool]] = [
    ("test-headless/Test/Headless/Harness/Log.hs", False),
    ("test-headless/Test/Headless/Harness/LogExtra.hs", True),
    ("test-headless/Test/Headless/Harness.hs", True),
    ("test-headless/Test/Headless/Other/Harness/Log.hs", True),
    ("test-headless/Test/Headless/World/Chop/Authority.hs", True),
    ("test-headless/Test/Headless/Harness/Log.hs-boot", True),
]

# `(label, ghc -E output, source path, expected origins, cpp ran)`
MAP_FIXTURES: list[tuple[str, str, str, list[tuple[str, int]], bool]] = [
    ("GHC's LINE pragma alone: a module CPP did not touch",
     '{-# LINE 1 "M.hs" #-}\nmodule M where\nx = 1\n', "M.hs",
     [("", 0), ("M.hs", 1), ("M.hs", 2), ("M.hs", 3)], False),
    ("cpp markers, flags and an included header",
     '{-# LINE 1 "M.hs" #-}\n# 1 "M.hs"\n# 1 "<built-in>" 1\n'
     '# 3 "M.hs" 2\nimport A\n# 1 "h.h" 1\nimport B\n# 5 "M.hs" 2\nx\n',
     "M.hs",
     [("", 0), ("", 0), ("", 0), ("", 0), ("M.hs", 3), ("", 0),
      ("h.h", 1), ("", 0), ("M.hs", 5), ("M.hs", 6)], True),
]


def _run_tree(settings: BuildSettings, files: dict[str, str]
              ) -> list[Violation]:
    """Scan a fixture tree at `<tmp>/repo`. A `../` path lands beside it,
    outside the scanned root, and a `@symlink:<target>` value makes a
    symlink."""
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp) / "repo"
        root.mkdir()
        for rel, content in files.items():
            (root / rel).parent.mkdir(parents=True, exist_ok=True)
            if content.startswith("@symlink:"):
                (root / rel).symlink_to(content[len("@symlink:"):])
            else:
                (root / rel).write_text(content, encoding="utf-8")
        return scan_tree(root, settings)


# ---- the capture: wrapper, decoding, ways, provenance, record binding

WRAPPER = REPO_ROOT / "BuildSupport" / "ghc-capture-wrapper.sh"

# The fake "real compiler": records the argv it received, NUL-separated,
# and exits with $FAKE_EXIT.
_FAKE_COMPILER = """#!/bin/sh
: > "$FAKE_ARGV"
for a in "$@"; do printf '%s\\0' "$a" >> "$FAKE_ARGV"; done
exit "${FAKE_EXIT:-0}"
"""

_AWKWARD_ARGS = ["-optP-DX=a b", "it's", 'say "hi"', "back\\slash", "",
                 "-v0", "+RTS", "-A64m", "-RTS", "tab\there"]


def _wrapper_failures(tmp: Path) -> list[str]:
    """The tracked wrapper passes every command through unchanged and
    publishes its record only on success; any record it cannot write
    fails the compile."""
    failures: list[str] = []
    fake = tmp / "fake-ghc"
    fake.write_text(_FAKE_COMPILER, encoding="utf-8")
    fake.chmod(0o755)
    rsp = tmp / "ghc.rsp"
    rsp.write_bytes(b"-package-env=-\n--make\nMain\\ Module.hs\n")

    def run(case: str, args: list[str], exit_code: int = 0,
            capture: Path | None = None) -> tuple[int, Path]:
        session = capture or tmp / f"session-{case}"
        if capture is None:
            session.mkdir()
        received = tmp / f"received-{case}"
        env = {**os.environ, "SYNARCHY_CAPTURE_REAL_GHC": str(fake),
               "SYNARCHY_CAPTURE_DIR": str(session),
               "FAKE_ARGV": str(received), "FAKE_EXIT": str(exit_code)}
        result = subprocess.run(["sh", str(WRAPPER), *args], env=env,
                                capture_output=True, timeout=60)
        return result.returncode, session

    args = [*_AWKWARD_ARGS, "@" + str(rsp)]
    code, session = run("pass", args)
    published = sorted(session.glob("inv.*"))
    expected = b"".join(a.encode() + b"\0" for a in args)
    if code != 0:
        failures.append(f"WRAPPER pass-through exited {code}")
    if (tmp / "received-pass").read_bytes() != expected:
        failures.append("WRAPPER the compiler did not receive the exact argv")
    if len(published) != 1 or (published[0] / "argv").read_bytes() != expected:
        failures.append("WRAPPER the published argv differs from the command")
    elif (published[0] / f"rsp.{len(args) - 1}").read_bytes() != rsp.read_bytes():
        failures.append("WRAPPER the response file copy differs")
    code, session = run("fail", ["-c", "x.hs"], exit_code=3)
    if code != 3 or list(session.glob("inv.*")):
        failures.append(f"WRAPPER a failing compile exited {code} or "
                        f"published a record")
    code, _ = run("nodir", ["-c"], capture=tmp / "no-such" / "dir")
    if code != 70 or (tmp / "received-nodir").exists():
        failures.append(f"WRAPPER an unwritable record exited {code} or ran "
                        f"the compiler")
    code, session = run("norsp", ["@" + str(tmp / "missing.rsp")])
    if code != 70 or list(session.glob("inv.*")):
        failures.append(f"WRAPPER a missing response file exited {code}")
    return failures


# (label, raw args, expected (program, rts))
SPLIT_RTS_FIXTURES = [
    ("no RTS", ["-O", "a"], (["-O", "a"], [])),
    ("closed section", ["-O", "+RTS", "-A64m", "-RTS", "a"],
     (["-O", "a"], ["+RTS", "-A64m", "-RTS"])),
    ("open to the end", ["-O", "+RTS", "-A64m", "-N"],
     (["-O"], ["+RTS", "-A64m", "-N"])),
    ("--RTS ends RTS processing", ["+RTS", "-A1m", "--RTS", "+RTS", "x"],
     (["+RTS", "x"], ["+RTS", "-A1m", "--RTS"])),
    ("-RTS outside a section is a program argument", ["-RTS", "a"],
     (["-RTS", "a"], [])),
]


def _write_record(slot: Path, name: str, args: list[str],
                  responses: dict[int, str] | None = None) -> None:
    record = slot / name
    record.mkdir(parents=True)
    (record / "argv").write_bytes(b"".join(a.encode() + b"\0" for a in args))
    for index, text in (responses or {}).items():
        (record / f"rsp.{index}").write_text(text, encoding="utf-8")


def _escape_rsp(args: list[str]) -> str:
    """Cabal 3.16.1.0's `escapeResponseFileArg`, one argument per line."""
    return "".join("".join("\\" + c if c in "\\'\"" or c.isspace() else c
                           for c in a) + "\n" for a in args)


_UNIT = "synarchy-0.1.0.0-inplace-synarchy-test-headless"
_COMPILE = ["-package-env=-", "--make", "-no-link", "-v0", "-optP-DX=a b",
            "-this-unit-id", _UNIT, "-main-is", "Main", "-o", "Main",
            "test-headless/Main.hs", "Main", "Engine.Core.Init"]


def _decode_failures(ghc: str, tmp: Path) -> list[str]:
    """Response files are expanded, and source arguments identified, by
    the compiler's own GHC code; positions keep equal strings apart."""
    failures: list[str] = []
    slot = tmp / "decode-slot"
    _write_record(slot, "inv.a", ["@/tmp/ghc0.rsp", "+RTS", "-A64m", "-RTS"],
                  {0: _escape_rsp(_COMPILE)})
    _write_record(slot, "inv.b", ["-package-env=-", "--numeric-version"])
    try:
        read = {i.record: i for i in read_invocations(ghc, slot, tmp)}
    except AuditError as error:
        return [f"DECODE {error}"]
    compile_ = read.get("inv.a")
    if compile_ is None or list(compile_.args) != _COMPILE:
        failures.append(f"DECODE the response file decoded to "
                        f"{compile_ and compile_.args}")
    elif sorted(compile_.args[i] for i in compile_.leftovers) != sorted(
            ["--make", "test-headless/Main.hs", "Main", "Engine.Core.Init"]):
        failures.append(f"DECODE leftovers {[compile_.args[i] for i in compile_.leftovers]}")
    else:
        replay = replay_of(compile_, _UNIT)
        if replay is None or "-main-is" not in replay or \
                replay[replay.index("-main-is") + 1] != "Main" or \
                "Engine.Core.Init" in replay or "--make" in replay or \
                replay[-3:] != ("+RTS", "-A64m", "-RTS"):
            failures.append(f"DECODE replay {replay}")
    if "inv.b" not in read or replay_of(read["inv.b"], _UNIT) is not None:
        failures.append("DECODE a version query was taken for a build")
    return failures


def _ways_failures() -> list[str]:
    """Compile and link commands of one way collapse; ways stay apart;
    other units and non-builds are ignored; unmodelled modes refuse."""
    def inv(name: str, args: list[str], left: list[str]) -> Invocation:
        return Invocation(name, tuple(args),
                          tuple(i for i, a in enumerate(args) if a in left), ())
    base = ["-this-unit-id", _UNIT, "-O", "--make"]
    compile_ = inv("c", [*base, "-no-link", "Main.hs"], ["--make", "Main.hs"])
    link = inv("l", [*base, "-o", "exe", "Main.hs"], ["--make", "Main.hs"])
    prof = inv("p", [*base, "-prof", "-optP-DPROF", "Main.hs"],
               ["--make", "Main.hs"])
    other = inv("o", ["-this-unit-id", "lib", "--make", "Lib"], ["--make", "Lib"])
    failures = []
    replays = distinct_replays([compile_, link, prof, other], _UNIT)
    if len(replays) != 2 or not any("-optP-DPROF" in r for r in replays):
        failures.append(f"WAYS {replays}")
    try:
        distinct_replays([inv("x", [*base, "-c", "Main.hs"],
                              ["--make", "-c", "Main.hs"])], _UNIT)
        failures.append("WAYS an unmodelled mode was accepted")
    except AuditError:
        pass
    return failures


_ROOT = "/checkout"
# (label, output, (roots, imports) or an error substring)
PROVENANCE_FIXTURES = [
    ("one file", f"Configuration is affected by cabal.project at '{_ROOT}'.",
     (["cabal.project"], [])),
    ("two roots",
     f"Configuration is affected by cabal.project and cabal.project.local "
     f"at '{_ROOT}'.", (["cabal.project", "cabal.project.local"], [])),
    ("one import (printed as its root twice)",
     f"fetching import: conf/native.project\nConfiguration is affected by "
     f"cabal.project and cabal.project at '{_ROOT}'.",
     (["cabal.project"], ["conf/native.project"])),
    ("the verbose list with nested imports",
     "fetching import: conf/a.project\nfetching import: conf/b.project\n"
     "Configuration is affected by the following files:\n- cabal.project\n"
     "- conf/a.project\nimported by: cabal.project\n- conf/b.project\n"
     "imported by: conf/a.project\nimported by: cabal.project\n"
     f"at '{_ROOT}'.",
     (["cabal.project"], ["conf/a.project", "conf/b.project"])),
    ("no message", "Up to date", "printed 0"),
    ("two messages", f"Configuration is affected by x at '{_ROOT}'.\n"
     f"Configuration is affected by x at '{_ROOT}'.", "printed 2"),
    ("a fetched import the list does not mark",
     "fetching import: conf/a.project\nConfiguration is affected by the "
     "following files:\n- cabal.project\n- cabal.project.local\n"
     f"- conf/a.project\nat '{_ROOT}'.", "marks"),
    ("an import the two-file shape cannot hold",
     "fetching import: a\nfetching import: b\nConfiguration is affected by "
     f"cabal.project and cabal.project at '{_ROOT}'.", "unrecognised"),
    ("two roots yet an import",
     f"fetching import: a\nConfiguration is affected by cabal.project and "
     f"cabal.project.local at '{_ROOT}'.", "fetched imports"),
    ("another project root",
     "Configuration is affected by cabal.project at '/elsewhere'.", "not "),
    ("a URL import", "fetching import: https://example.org/x.project\n"
     f"Configuration is affected by cabal.project and cabal.project at "
     f"'{_ROOT}'.", "URL"),
    ("a quoted, untrimmed path",
     f"Configuration is affected by ' cabal.project' at '{_ROOT}'.",
     "untrimmed"),
    ("a wrapped message",
     f"Configuration is affected by cabal.project at\n'{_ROOT}'.",
     "unrecognised"),
]


def _provenance_failures() -> list[str]:
    failures = []
    for label, output, expected in PROVENANCE_FIXTURES:
        try:
            got = parse_provenance(output, Path(_ROOT))
            if isinstance(expected, str) or got != expected:
                failures.append(f"PROVENANCE {label}: {got}")
        except AuditError as error:
            if not isinstance(expected, str) or expected not in str(error):
                failures.append(f"PROVENANCE {label}: {error}")
    return failures


_FAKE_CABAL = """#!/bin/sh
case "$1" in
  --numeric-version) echo "${FAKE_CABAL_VERSION:-3.16.1.0}" ;;
  path) echo "$FAKE_GLOBAL_CONFIG" ;;
  *) echo "fake cabal: $*" >&2; exit 9 ;;
esac
"""


def _stamped_checkout(root: Path, ghc: str) -> dict:
    """A synthetic built checkout with a capture slot and the stamp
    `--record` would write for it, for `configured_settings` to verify."""
    (root / CABAL_FILE).write_text(
        f"name: synarchy\nversion: 0.1.0.0\n"
        f"tested-with: GHC =={_compiler_version(ghc)}\n", encoding="utf-8")
    (root / "cabal.project").write_text("packages: .\n", encoding="utf-8")
    (root / SCOPED_TREE).mkdir()
    builddir = root / "dist-alt"
    dist = builddir / "build" / "synarchy-0.1.0.0"
    autogen = dist / "build" / HEADLESS_SUITE / "autogen"
    autogen.mkdir(parents=True)
    header = autogen / "cabal_macros.h"
    header.write_text(FIXTURE_HEADER, encoding="utf-8")
    (dist / "setup-config").write_text("configured\n", encoding="utf-8")
    info = dist / "build-info.json"
    info.write_text(json.dumps({"components": [{
        "name": HEADLESS_COMPONENT, "unit-id": _UNIT,
        "src-dir": str(root) + "/"}]}), encoding="utf-8")
    (builddir / "cache").mkdir(parents=True)
    (builddir / "cache" / "plan.json").write_text(json.dumps({
        "install-plan": [{"pkg-name": "synarchy", "style": "local",
                          "component-name": None, "dist-dir": str(dist),
                          "build-info": str(info)}]}), encoding="utf-8")
    slot = dist / "build" / CAPTURE_UNITS / _UNIT
    _write_record(slot, "inv.a", ["--make", "Main.hs"])
    (slot / "meta").write_text(
        "".join(f"{key}\t{key}-value\n" for key in _META_KEYS), encoding="utf-8")
    # The configured compiler: a symlink to a forwarding program, as a
    # ghcup or `--with-compiler` path can be.
    programs = root / "compilers"
    programs.mkdir()
    compiler_program_file = programs / "ghc-built"
    compiler_program_file.write_text(_forwarding_compiler(ghc, "built"),
                                     encoding="utf-8")
    compiler_program_file.chmod(0o755)
    configured = programs / "ghc"
    configured.symlink_to(compiler_program_file)
    global_config = root / "global-config"
    global_config.write_text("-- global\n", encoding="utf-8")
    replay = [*_FIXTURE_ARGS, "-optP-include",
              f"-optP{header.relative_to(root).as_posix()}", "-optP-DRECORDED"]
    stamp = dist / STAMP_NAME
    document = {
        "schema": STAMP_SCHEMA, "checkout": str(root.resolve()),
        "builddir": str(builddir.resolve()), "cabal": CABAL_PIN,
        "cabal-args": ["build", HEADLESS_SUITE, "--builddir=dist-alt"],
        "unit-id": _UNIT, "session": "1",
        "capture": capture_fingerprint(slot),
        "replays": [replay],
        "header": {"path": str(header), "sha256": _digest(header)},
        "setup-config": _digest(dist / "setup-config"),
        "compiler": {"path": str(configured),
                     "canonical": str(compiler_program_file),
                     "sha256": _digest(compiler_program_file),
                     "info-sha256": _text_digest(_compiler_info(ghc))},
        "configuration": {
            str((root / "cabal.project").resolve()):
                _digest(root / "cabal.project"),
            str((root / "cabal.project.local").resolve()): None,
            str(global_config): _digest(global_config)},
        "global-config": str(global_config)}
    stamp.write_text(json.dumps(document), encoding="utf-8")
    return {"stamp": stamp, "document": document, "slot": slot,
            "header": header, "dist": dist, "compiler": compiler_program_file,
            "configured": configured, "ghc": ghc,
            "global": global_config}


def _forwarding_compiler(ghc: str, tag: str) -> str:
    """A program that runs `ghc` unchanged; `tag` only varies its bytes."""
    return f'#!/bin/sh\n# forwarding compiler: {tag}\nexec "{ghc}" "$@"\n'


def _retarget(tag: str | None):
    """Point the configured compiler symlink at another program: a
    different forwarder (`tag`), or a byte-identical copy (None)."""
    def tamper(root: Path, pieces: dict) -> None:
        target = root / "compilers" / f"ghc-{tag or 'copy'}"
        target.write_bytes(pieces["compiler"].read_bytes() if tag is None
                           else _forwarding_compiler(pieces["ghc"], tag).encode())
        target.chmod(0o755)
        pieces["configured"].unlink()
        pieces["configured"].symlink_to(target)
    return tamper


def _drop(key: str):
    def tamper(root: Path, pieces: dict) -> None:
        document = dict(pieces["document"])
        document.pop(key)
        pieces["stamp"].write_text(json.dumps(document), encoding="utf-8")
    return tamper


def _edit_stamp(**changes):
    def tamper(root: Path, pieces: dict) -> None:
        document = {**pieces["document"], **changes}
        pieces["stamp"].write_text(json.dumps(document), encoding="utf-8")
    return tamper


def _append(key: str, text: str = "changed\n"):
    def tamper(root: Path, pieces: dict) -> None:
        with Path(pieces[key]).open("a", encoding="utf-8") as handle:
            handle.write(text)
    return tamper


def _write(rel: str, text: str = "-- appeared\n"):
    def tamper(root: Path, pieces: dict) -> None:
        (root / rel).parent.mkdir(parents=True, exist_ok=True)
        (root / rel).write_text(text, encoding="utf-8")
    return tamper


# (label, tamper, AuditError substring, or "" when it must still load)
VERIFY_FIXTURES = [
    ("valid", lambda root, pieces: None, ""),
    ("a source edit only", _write(f"{SCOPED_TREE}/M.hs", _HEAD + _BANNED), ""),
    *[(f"no `{key}`", _drop(key), f"`{key}`" if key != "schema" else "schema")
      for key in ("schema", "checkout", "builddir", "cabal", "cabal-args",
                  "unit-id", "session", "capture", "replays", "header",
                  "setup-config", "compiler", "configuration",
                  "global-config")],
    ("empty replays", _edit_stamp(replays=[]), "`replays`"),
    ("a replay that is not a list of strings", _edit_stamp(replays=[[1]]),
     "malformed"),
    ("an incomplete compiler record", _edit_stamp(compiler={"path": "ghc"}),
     "incomplete"),
    ("an incomplete header record", _edit_stamp(header={"path": "x"}),
     "incomplete"),
    ("a malformed configuration map", _edit_stamp(configuration={"x": 1}),
     "malformed"),
    ("another checkout", _edit_stamp(checkout="/elsewhere"), "records /elsewhere"),
    ("another build directory", _edit_stamp(builddir="/elsewhere"),
     "build directory"),
    ("another unit", _edit_stamp(**{"unit-id": "other"}), "records other"),
    ("an old schema", _edit_stamp(schema=1), "schema 1"),
    ("a rebuild that ran the same commands (cabal test)", lambda root, pieces: (
        os.replace(pieces["slot"] / "inv.a", pieces["slot"] / "inv.z"),
        (pieces["slot"] / "meta").write_text(
            (pieces["slot"] / "meta").read_text(encoding="utf-8").replace(
                "session\tsession-value", "session\t99"), encoding="utf-8")),
     ""),
    ("a rebuild that ran another command", lambda root, pieces: (
        pieces["slot"] / "inv.a" / "argv").write_bytes(b"--make\0-O2\0"),
     "captured commands"),
    ("a capture under another package configuration", lambda root, pieces: (
        pieces["slot"] / "meta").write_text(
            (pieces["slot"] / "meta").read_text(encoding="utf-8").replace(
                "setup-config-md5-value", "other"), encoding="utf-8"),
     "captured commands"),
    ("a capture slot lost", lambda root, pieces: shutil.rmtree(pieces["slot"]),
     "captured commands"),
    ("a new capture record", lambda root, pieces: _write_record(
        pieces["slot"], "inv.b", ["--make"]), "captured commands"),
    ("a changed setup-config", lambda root, pieces: (
        pieces["dist"] / "setup-config").write_text("re\n", encoding="utf-8"),
     "setup-config"),
    ("a changed header", _append("header", "#define LATER 1\n"),
     "generated header"),
    ("same-version compiler, changed program bytes", _append("compiler"),
     "compiler program"),
    ("the configured compiler symlink retargeted to another same-version "
     "program (#2648 review 10)", _retarget("other"), "compiler program"),
    ("the configured compiler symlink retargeted to a byte-identical copy",
     _retarget(None), ""),
    ("changed compiler settings (--info)",
     lambda root, pieces: _edit_stamp(compiler={
         **pieces["document"]["compiler"], "info-sha256": "0" * 64})(
             root, pieces), "--info changed"),
    ("a changed project file", _write("cabal.project", "packages: ./x\n"),
     "configuration input"),
    ("a project .local appearing", _write("cabal.project.local"),
     "configuration input"),
    ("a changed global config", _append("global"), "configuration input"),
]


def _verify_failures(ghc: str, tmp: Path) -> list[str]:
    failures: list[str] = []
    fake_cabal = tmp / "fake-cabal"
    fake_cabal.write_text(_FAKE_CABAL, encoding="utf-8")
    fake_cabal.chmod(0o755)
    saved = {k: os.environ.get(k) for k in (CABAL_ENV, "FAKE_GLOBAL_CONFIG",
                                            "FAKE_CABAL_VERSION")}
    os.environ[CABAL_ENV] = str(fake_cabal)
    try:
        for label, tamper, needle in VERIFY_FIXTURES:
            root = tmp / ("verify-" + re.sub(r"\W+", "-", label))
            root.mkdir()
            pieces = _stamped_checkout(root, ghc)
            os.environ["FAKE_GLOBAL_CONFIG"] = str(pieces["global"])
            tamper(root, pieces)
            try:
                ways = configured_settings(root, "dist-alt")
                if needle:
                    failures.append(f"VERIFY {label}: loaded")
                elif ways[0].args[-1] != "-optP-DRECORDED":
                    failures.append(f"VERIFY {label}: args {ways[0].args}")
            except AuditError as error:
                if not needle or needle not in str(error):
                    failures.append(f"VERIFY {label}: {error}")
        # The global config cabal now resolves must be the recorded one.
        root = tmp / "verify-global-path"
        root.mkdir()
        pieces = _stamped_checkout(root, ghc)
        os.environ["FAKE_GLOBAL_CONFIG"] = str(root / "another-config")
        try:
            configured_settings(root, "dist-alt")
            failures.append("VERIFY another global config path: loaded")
        except AuditError as error:
            if "another global config" not in str(error):
                failures.append(f"VERIFY another global config path: {error}")
        # The default build directory is not the one recorded.
        try:
            configured_settings(root, DEFAULT_BUILDDIR)
            failures.append("VERIFY the unselected build directory: loaded")
        except AuditError as error:
            if "does not exist" not in str(error):
                failures.append(f"VERIFY the unselected build directory: {error}")
        # The record boundary: the slot's compiler must still be reached,
        # byte for byte, through its configured path.
        for label, tamper, needle in (
                ("the capture's compiler, unchanged", None, ""),
                ("the capture's compiler overwritten", _append("compiler"),
                 "now reaches"),
                ("the capture's compiler symlink retargeted (#2648 review 10)",
                 _retarget("other"), "now reaches"),
                ("the capture's compiler symlink retargeted to a copy",
                 _retarget(None), "")):
            case = tmp / ("capture-" + re.sub(r"\W+", "-", label))
            case.mkdir()
            pieces = _stamped_checkout(case, ghc)
            meta = {"ghc": str(pieces["configured"]),
                    "ghc-canonical": str(pieces["compiler"]),
                    "ghc-md5": _digest(pieces["compiler"], "md5")}
            if tamper:
                tamper(case, pieces)
            try:
                captured_compiler(meta)
                if needle:
                    failures.append(f"CAPTURED COMPILER {label}: accepted")
            except AuditError as error:
                if not needle or needle not in str(error):
                    failures.append(f"CAPTURED COMPILER {label}: {error}")
        # Recording refuses what it cannot bind before running cabal.
        for label, args, builddir, version, needle in (
                ("no build command", ["test", HEADLESS_SUITE], "dist-alt",
                 CABAL_PIN, "starting with `build`"),
                ("a dry run", ["build", "--dry-run"], DEFAULT_BUILDDIR,
                 CABAL_PIN, "--dry-run"),
                ("another build directory", ["build", "--builddir=dist-x"],
                 "dist-alt", CABAL_PIN, "not the selected"),
                ("the default build directory, unselected", ["build"],
                 "dist-alt", CABAL_PIN, "not the selected"),
                ("another cabal-install", ["build", "--builddir=dist-alt"],
                 "dist-alt", "3.18.0.0", "supports no other version")):
            os.environ["FAKE_CABAL_VERSION"] = version
            try:
                record_settings(root, builddir, args)
                failures.append(f"RECORD {label}: recorded")
            except AuditError as error:
                if needle not in str(error):
                    failures.append(f"RECORD {label}: {error}")
    finally:
        for key, value in saved.items():
            if value is None:
                os.environ.pop(key, None)
            else:
                os.environ[key] = value
    return failures


CAPTURE_CASES = (4 + 4 + len(SPLIT_RTS_FIXTURES) + 3 + 2
                 + len(PROVENANCE_FIXTURES) + len(VERIFY_FIXTURES) + 2 + 5)


def capture_failures(ghc: str) -> list[str]:
    failures: list[str] = []
    for label, args, expected in SPLIT_RTS_FIXTURES:
        got = split_rts(args)
        if got != expected:
            failures.append(f"RTS {label}: {got}")
    with tempfile.TemporaryDirectory() as tmp:
        failures += _wrapper_failures(Path(tmp))
        failures += _decode_failures(ghc, Path(tmp))
        failures += _verify_failures(ghc, Path(tmp))
    failures += _ways_failures()
    failures += _provenance_failures()
    return failures


# ---- --cabal-regression: the real hook, cabal-install and Setup 3.16.1.0

CABAL_REGRESSION_STEPS = (
    "the local order (build all, headless, graphical) records and scans "
    "clean, and still does after cabal test",
    "a header in the suite's own -tmp output directory selects the banned "
    "import (#2648 review 9), and its clean control stays clean",
    "a source-only injection after recording is diagnosed without a "
    "rebuild",
    "two successfully built directories are each certified by explicit "
    "selection, and a record with the other build's arguments is refused "
    "(#2648 review 9)",
    "an edited nested import fails the scan as stale",
    "after that edit an up-to-date build cannot be recorded: Cabal kept "
    "the old settings, and recording refuses (#2648 review 9)",
    "the rebuild then applies the edit and the record certifies it",
    "a changed global config fails the scan as stale",
    "a record missing its replays fails the gate (#2648 review 9)",
    "a same-version compiler program with changed bytes fails the gate "
    "(#2648 review 9), and restoring its bytes passes",
    "RTS options and a profiling build's own options reach the replay",
    "a byte-identical cache restored beneath re-stamped configuration "
    "files verifies and records",
    "a failed build publishes nothing and cannot be recorded",
    "a configured compiler symlink retargeted to another same-version "
    "program fails both the scan and a re-record without a rebuild, and "
    "restoring its target restores detection (#2648 review 10)",
)

_TINY_CABAL = """cabal-version: 3.0
name: synarchy
version: 0.1.0.0
build-type: Custom
tested-with: GHC =={version}
extra-source-files: BuildSupport/GhcCapture.hs
                    BuildSupport/ghc-capture-wrapper.sh
custom-setup
  setup-depends: base, Cabal =={cabal}, directory, filepath, process
library
  exposed-modules: Lib
  hs-source-dirs: src
  build-depends: base
  default-language: GHC2024
test-suite synarchy-test-headless
  type: exitcode-stdio-1.0
  main-is: Main.hs
  hs-source-dirs: test-headless
  other-modules: Engine.Core.Init
  build-depends: base, synarchy
  default-language: GHC2024
  ghc-prof-options: -optP-DPROF_WAY
test-suite synarchy-test-graphical
  type: exitcode-stdio-1.0
  main-is: Main.hs
  hs-source-dirs: graphical
  build-depends: base
  default-language: GHC2024
"""

_TINY_INIT = """module Engine.Core.Init (initializeEngineHeadless, EngineInitResult(..)) where
data EngineInitResult = EngineInitResult
initializeEngineHeadless :: IO EngineInitResult
initializeEngineHeadless = pure EngineInitResult
"""

_TINY_MAIN = """{-# LANGUAGE CPP #-}
module Main where
#include "actual.h"
#if defined(REVIEW_INCLUDE) || defined(REVIEW_BUILDDIR) || defined(FRESH_CHANGED) || defined(REVIEW_COMPILER) || defined(PROF_WAY)
import Engine.Core.Init (initializeEngineHeadless)
#else
import Engine.Core.Init (EngineInitResult(..))
#endif
main :: IO ()
main = pure ()
"""
_MAIN_BANNED_LINE = 5


def _cabal_paths(cabal: str) -> tuple[str, str]:
    """The store and package index the project build itself uses, as
    `cabal path` reports them, so the fixture's Setup is built offline
    on the same Cabal library."""
    result = _cabal_path(cabal, "--store-dir", "--remote-repo-cache")
    values = dict(line.split(": ", 1) for line in result.stdout.splitlines()
                  if ": " in line)
    store, cache = values.get("store-dir"), values.get("remote-repo-cache")
    if result.returncode != 0 or not store or not cache:
        raise AuditError("cabal path named no store and index: "
                         + " ".join((result.stdout + result.stderr).split()))
    return store, cache


def cabal_regression() -> tuple[list[str], int]:
    """Record and scan a tiny Custom package shaped like this one, built
    through the repository's own BuildSupport/GhcCapture.hs and wrapper
    by the pinned cabal-install and Setup Cabal, offline. Run after the
    project build (test-and-audits, tools/ci-local.sh), whose store holds
    that Cabal. Returns failures and the case count."""
    failures: list[str] = []
    ghc = ghc_command(REPO_ROOT)
    version = _compiler_version(ghc)
    cabal = cabal_command()
    store, cache = _cabal_paths(cabal)
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp) / "pkg"
        for sub in ("src", "graphical", "BuildSupport",
                    f"{SCOPED_TREE}/Engine/Core", "conf"):
            (root / sub).mkdir(parents=True)
        (root / CABAL_FILE).write_text(
            _TINY_CABAL.format(version=version, cabal=CABAL_PIN),
            encoding="utf-8")
        shutil.copy(REPO_ROOT / "BuildSupport" / "GhcCapture.hs",
                    root / "BuildSupport")
        shutil.copy(WRAPPER, root / "BuildSupport")
        (root / "Setup.hs").write_text(
            "import Distribution.Simple\nimport BuildSupport.GhcCapture "
            "(withGhcCapture)\nmain :: IO ()\nmain = defaultMainWithHooks "
            "(withGhcCapture simpleUserHooks)\n", encoding="utf-8")
        (root / "src" / "Lib.hs").write_text("module Lib where\n",
                                             encoding="utf-8")
        (root / "graphical" / "Main.hs").write_text(
            "module Main where\nmain :: IO ()\nmain = pure ()\n",
            encoding="utf-8")
        (root / SCOPED_TREE / "Engine/Core/Init.hs").write_text(
            _TINY_INIT, encoding="utf-8")
        main = root / SCOPED_TREE / "Main.hs"
        main.write_text(_TINY_MAIN, encoding="utf-8")
        (root / SCOPED_TREE / "actual.h").write_text("/* clean */\n",
                                                     encoding="utf-8")
        project = root / "cabal.project"
        project.write_text("packages: .\nimport: conf/native.project\n"
                           "package synarchy\n  build-info: True\n",
                           encoding="utf-8")
        native = root / "conf" / "native.project"
        native.write_text("import: nested.project\n", encoding="utf-8")
        nested = root / "conf" / "nested.project"
        nested.write_text("package synarchy\n  ghc-options: -optP-DFRESH_CLEAN\n",
                          encoding="utf-8")
        cabal_dir = Path(tmp) / "cabal-dir"
        cabal_dir.mkdir()
        global_config = cabal_dir / "config"
        global_config.write_text(
            "repository hackage.haskell.org\n  url: http://hackage.haskell.org/\n"
            f"remote-repo-cache: {cache}\nstore-dir: {store}\n",
            encoding="utf-8")
        env = {**os.environ, "CABAL_DIR": str(cabal_dir), CABAL_ENV: cabal}
        saved = {key: os.environ.get(key) for key in ("CABAL_DIR", CABAL_ENV)}
        os.environ.update({"CABAL_DIR": str(cabal_dir), CABAL_ENV: cabal})
        header = main.parent / "actual.h"

        def build(*args: str, expect_ok: bool = True) -> bool:
            result = subprocess.run([cabal, *args, "-v0", "--offline"],
                                    cwd=root, env=env, capture_output=True,
                                    text=True, timeout=900)
            if (result.returncode == 0) != expect_ok:
                failures.append(f"CABAL `cabal {' '.join(args)}` exited "
                                f"{result.returncode}: " + " ".join(
                                    (result.stdout + result.stderr).split())[-400:])
                return False
            return True

        def outcome(action):
            try:
                return action()
            except AuditError as error:
                return error

        def record(builddir: str, *args: str):
            return outcome(lambda: record_settings(
                root, builddir, ["build", *args, "--offline"]))

        def scan(builddir: str = DEFAULT_BUILDDIR):
            ways = outcome(lambda: configured_settings(root, builddir))
            if isinstance(ways, AuditError):
                return ways
            return {f"{v.path}:{v.line}" for v in scan_configured(root, ways)}

        banned = {f"{SCOPED_TREE}/Main.hs:{_MAIN_BANNED_LINE}"}

        def rebuild_and_record(*args: str):
            """Build, then record; when Cabal had kept an edited import's
            old settings, the record's re-read made it notice, so build
            and record once more."""
            build("build", HEADLESS_SUITE, *args)
            recorded = record(DEFAULT_BUILDDIR, HEADLESS_SUITE, *args)
            if isinstance(recorded, AuditError):
                build("build", HEADLESS_SUITE, *args)
                recorded = record(DEFAULT_BUILDDIR, HEADLESS_SUITE, *args)
            return recorded

        def expect_recorded(step: int, got) -> None:
            if not isinstance(got, Path):
                failures.append(f"CABAL {CABAL_REGRESSION_STEPS[step]}: "
                                f"recording failed: {got}")

        def expect(step: int, got, wanted) -> None:
            ok = (isinstance(got, AuditError) and isinstance(wanted, str)
                  and wanted in str(got)) or got == wanted
            if not ok:
                failures.append(f"CABAL {CABAL_REGRESSION_STEPS[step]}: {got}")

        try:
            suite = HEADLESS_SUITE
            # 0. The local order.
            if not (build("build", "all") and build("build", suite)
                    and build("build", "synarchy-test-graphical")):
                return failures, len(CABAL_REGRESSION_STEPS)
            expect_recorded(0, record(DEFAULT_BUILDDIR, suite))
            expect(0, scan(), set())
            build("test", suite)
            expect(0, scan(), set())
            # 1. The suite's own -tmp directory comes before the build dir.
            dist = select_build(root, DEFAULT_BUILDDIR).dist_dir
            tmp_dir = dist / "build" / suite / f"{suite}-tmp"
            header.unlink()
            (dist / "build" / "actual.h").write_text("/* clean */\n",
                                                     encoding="utf-8")
            (tmp_dir / "actual.h").write_text("#define REVIEW_INCLUDE 1\n",
                                              encoding="utf-8")
            expect(1, scan(), banned)
            (tmp_dir / "actual.h").write_text("/* clean */\n", encoding="utf-8")
            expect(1, scan(), set())
            header.write_text("/* clean */\n", encoding="utf-8")
            # 2. A source-only injection, no rebuild.
            clean_main = main.read_text(encoding="utf-8")
            main.write_text(clean_main.replace(
                "module Main where\n", "module Main where\nimport "
                "Engine.Core.Init (initializeEngineHeadless)\n"), encoding="utf-8")
            expect(2, scan(), {f"{SCOPED_TREE}/Main.hs:3"})
            main.write_text(clean_main, encoding="utf-8")
            # 3. A second build directory with its own options.
            alt = ("--builddir=dist-alt", "--ghc-options=-optP-DREVIEW_BUILDDIR")
            if build("build", suite, *alt):
                (Path(select_build(root, "dist-alt").dist_dir) / "build"
                 / suite / f"{suite}-tmp" / "actual.h").write_text(
                     "/* clean */\n", encoding="utf-8")
                expect_recorded(3, record("dist-alt", suite, *alt))
                expect(3, scan("dist-alt"), banned)
                expect(3, scan(DEFAULT_BUILDDIR), set())
                expect(3, record("dist-alt", suite, "--builddir=dist-alt"),
                       "not up to date")
            # 4-6. Cabal keeps building with an edited import's old settings.
            nested.write_text(
                "package synarchy\n  ghc-options: -optP-DFRESH_CHANGED\n",
                encoding="utf-8")
            expect(4, scan(), "configuration input")
            build("build", suite)
            expect(5, record(DEFAULT_BUILDDIR, suite), "not up to date")
            expect(5, scan(), "configuration input")
            if build("build", suite):
                expect_recorded(6, record(DEFAULT_BUILDDIR, suite))
                expect(6, scan(), banned)
            nested.write_text(
                "package synarchy\n  ghc-options: -optP-DFRESH_CLEAN\n",
                encoding="utf-8")
            rebuild_and_record()
            # 7. The global config is an input.
            saved_global = global_config.read_bytes()
            global_config.write_bytes(saved_global + b"-- edited\n")
            expect(7, scan(), "configuration input")
            global_config.write_bytes(saved_global)
            expect(7, scan(), set())
            # 8. An incomplete record.
            stamp = select_build(root, DEFAULT_BUILDDIR).stamp
            complete = stamp.read_bytes()
            document = json.loads(complete)
            document.pop("replays")
            stamp.write_text(json.dumps(document), encoding="utf-8")
            expect(8, scan(), "`replays`")
            stamp.write_bytes(complete)
            # 9. A same-version compiler whose bytes change.
            shim_dir = Path(tmp) / "shim"
            shim_dir.mkdir()
            shim = shim_dir / "ghc"
            real = shutil.which(ghc)
            shim.write_text(f'#!/bin/sh\nexec "{real}" -optP-DREVIEW_COMPILER '
                            f'"$@"\n', encoding="utf-8")
            shim.chmod(0o755)
            pkg = shutil.which("ghc-pkg-" + version, path=str(Path(
                os.path.realpath(real)).parent)) or shutil.which("ghc-pkg")
            (shim_dir / "ghc-pkg").symlink_to(pkg)
            if build("build", suite, f"--with-compiler={shim}"):
                expect_recorded(9, record(DEFAULT_BUILDDIR, suite,
                                          f"--with-compiler={shim}"))
                expect(9, scan(), banned)
                with_define = shim.read_bytes()
                shim.write_text(f'#!/bin/sh\nexec "{real}" "$@"\n',
                                encoding="utf-8")
                expect(9, scan(), "compiler program")
                shim.write_bytes(with_define)
                expect(9, scan(), banned)
            # 13. A configured compiler symlink retargeted to another
            # same-version program; the original target is left untouched.
            positive = shim_dir / "ghc-positive"
            positive.write_text(f'#!/bin/sh\nexec "{real}" '
                                f'-optP-DREVIEW_COMPILER "$@"\n',
                                encoding="utf-8")
            clean = shim_dir / "ghc-clean"
            clean.write_text(f'#!/bin/sh\nexec "{real}" "$@"\n',
                             encoding="utf-8")
            positive.chmod(0o755)
            clean.chmod(0o755)
            link = shim_dir / "ghc-link"
            link.symlink_to(positive)
            linked = f"--with-compiler={link}"
            if build("build", suite, linked):
                expect_recorded(13, record(DEFAULT_BUILDDIR, suite, linked))
                expect(13, scan(), banned)
                link.unlink()
                link.symlink_to(clean)
                expect(13, subprocess.run(
                    [str(link), "--numeric-version"], capture_output=True,
                    text=True).stdout.strip(), version)
                expect(13, scan(), "compiler program")
                expect(13, record(DEFAULT_BUILDDIR, suite, linked),
                       "now reaches")
                expect(13, scan(), "compiler program")
                link.unlink()
                link.symlink_to(positive)
                expect(13, scan(), banned)
                expect_recorded(13, record(DEFAULT_BUILDDIR, suite, linked))
                expect(13, scan(), banned)
            # 10. RTS options and the profiling way's own options.
            project.write_text(project.read_text(encoding="utf-8")
                               + "  ghc-options: +RTS -A16m -RTS\n",
                               encoding="utf-8")
            if build("build", suite, "--enable-profiling"):
                expect_recorded(10, record(DEFAULT_BUILDDIR, suite,
                                             "--enable-profiling"))
                ways = outcome(lambda: configured_settings(root))
                ok = (not isinstance(ways, AuditError) and len(ways) == 1
                      and "-prof" in ways[0].args
                      and "-optP-DPROF_WAY" in ways[0].args
                      and ways[0].args[-3:] == ("+RTS", "-A16m", "-RTS"))
                expect(10, ok, True)
                expect(10, scan(), banned)
            project.write_text("packages: .\nimport: conf/native.project\n"
                               "package synarchy\n  build-info: True\n",
                               encoding="utf-8")
            rebuild_and_record()
            # 11. The review's cache restore.
            archived = Path(tmp) / "archived"
            shutil.copytree(root / DEFAULT_BUILDDIR, archived)
            time.sleep(1.1)
            for path in (project, native, nested, root / CABAL_FILE):
                os.utime(path, None)
            shutil.rmtree(root / DEFAULT_BUILDDIR)
            shutil.copytree(archived, root / DEFAULT_BUILDDIR)
            expect(11, scan(), set())
            build("build", suite)
            expect_recorded(11, record(DEFAULT_BUILDDIR, suite))
            expect(11, scan(), set())
            # 12. A failed build.
            slot = select_build(root, DEFAULT_BUILDDIR).slot
            before = sorted(p.name for p in slot.iterdir())
            main.write_text(clean_main + "broken =\n", encoding="utf-8")
            build("build", suite, expect_ok=False)
            expect(12, sorted(p.name for p in slot.iterdir()), before)
            expect(12, record(DEFAULT_BUILDDIR, suite), "not up to date")
            main.write_text(clean_main, encoding="utf-8")
        except (AuditError, OSError, ValueError, KeyError) as error:
            # A later step can depend on what an earlier, failed step
            # should have produced: stop there, reported, not raised.
            failures.append(f"CABAL stopped: {type(error).__name__}: {error}")
        finally:
            for key, value in saved.items():
                if value is None:
                    os.environ.pop(key, None)
                else:
                    os.environ[key] = value
    return failures, len(CABAL_REGRESSION_STEPS)


def run_cabal_regression() -> int:
    failures, total = cabal_regression()
    if failures:
        print(f"headless_init_import_audit cabal regression: {len(failures)} "
              f"failure(s) over {total} steps:")
        for failure in failures:
            print(f"  {failure}")
        return 1
    print(f"headless_init_import_audit cabal regression: all {total} steps "
          f"passed (cabal-install and Setup Cabal {CABAL_PIN}).")
    return 0


_RECORD_COMMAND = ("python3 tools/headless_init_import_audit.py --record "
                   "--builddir dist-newstyle -- build synarchy-test-headless "
                   "-v0")
_GATE_COMMAND = ("python3 tools/headless_init_import_audit.py --builddir "
                 "dist-newstyle")
_REGRESSION_COMMAND = ("python3 tools/headless_init_import_audit.py "
                       "--cabal-regression")
# What must run, in this order, in each entry point (#2648 review round
# 8): the builds, `--record`, the gate, and only then the tests.
GATE_ORDER = {
    "tools/ci-local.sh": (
        "cabal build all -v0", "cabal build synarchy-test-headless -v0",
        "cabal build synarchy-test-graphical -v0", _RECORD_COMMAND,
        _GATE_COMMAND, _REGRESSION_COMMAND, "cabal test synarchy-test-headless"),
    ".github/workflows/ci.yml": (
        "cabal build all -v0", "cabal build synarchy-test-headless -v0",
        "cabal build synarchy-test-graphical -v0", _RECORD_COMMAND,
        _GATE_COMMAND, _REGRESSION_COMMAND, "cabal test synarchy-test-headless"),
}


def gate_order_failures(name: str, text: str) -> list[str]:
    """Where entry point `name` (its text) runs the configured gate out
    of order: each command in GATE_ORDER[name] must first run after the
    first run of the one before it. For the workflow, only the
    `test-and-audits` job counts, and a step's `run: ` prefix is not
    part of its command. Commands match exactly, except that the test
    run may carry arguments and a leading environment assignment.
    Comment lines never count."""
    lines = text.splitlines()
    if name.endswith(".yml"):
        start = next((i for i, line in enumerate(lines)
                      if line.rstrip() == "  test-and-audits:"), None)
        if start is None:
            return [f"{name}: no test-and-audits job"]
        end = next((i for i in range(start + 1, len(lines))
                    if re.match(r"  [\w-]+:\s*$", lines[i])), len(lines))
        lines = lines[start:end]
    commands = [re.sub(r"^run:\s*", "", line.strip()) for line in lines
                if line.strip() and not line.strip().startswith("#")]

    def first(wanted: str) -> int | None:
        return next((i for i, command in enumerate(commands)
                     if command == wanted
                     or (wanted.startswith("cabal test")
                         and re.match(r"(\w+=\S*\s+)*" + re.escape(wanted)
                                      + r"(\s|$)", command))), None)
    order = GATE_ORDER[name]
    for earlier, later in zip(order, order[1:]):
        if first(earlier) is None or first(later) is None \
                or first(later) <= first(earlier):
            return [f"{name}: `{later}` must first run after `{earlier}`"]
    return []


def _move_line(text: str, command: str, before: str | None) -> str:
    """`text` with its first line running `command` moved to just before
    the first line containing `before`, or dropped when `before` is None."""
    lines = text.splitlines(keepends=True)
    index = next(i for i, line in enumerate(lines) if line.strip() == command)
    moved = lines.pop(index)
    if before is not None:
        target = next(i for i, line in enumerate(lines) if before in line)
        lines.insert(target, moved)
    return "".join(lines)


def _after_tests(text: str) -> str:
    """`text` with the gate moved to just after the headless test run."""
    lines = text.splitlines(keepends=True)
    index = next(i for i, line in enumerate(lines)
                 if line.strip() == _GATE_COMMAND)
    moved = lines.pop(index)
    target = next(i for i, line in enumerate(lines)
                  if "cabal test synarchy-test-headless -v0" in line)
    lines.insert(target + 1, moved)
    return "".join(lines)


# Reorderings of each entry point that the order check must refuse.
ORDER_MUTATIONS = (
    ("gate after the tests", _after_tests),
    ("no record", lambda text: _move_line(text, _RECORD_COMMAND, None)),
    ("record before the suite build", lambda text: _move_line(
        text, _RECORD_COMMAND, "cabal build synarchy-test-headless -v0")),
)


def self_test() -> int:
    failures: list[str] = []
    ghc = ghc_command(REPO_ROOT)
    failures += capture_failures(ghc)
    for name in GATE_ORDER:
        text = (REPO_ROOT / name).read_text(encoding="utf-8")
        failures += [f"ORDER {f}" for f in gate_order_failures(name, text)]
        for label, mutate in ORDER_MUTATIONS:
            if not gate_order_failures(name, mutate(text)):
                failures.append(f"ORDER {name}: {label} was accepted")
    for label, output, source, origins, cpp_ran in MAP_FIXTURES:
        pre = map_output(output, source)
        if list(pre.origins) != origins or pre.cpp_ran != cpp_ran:
            failures.append(f"MAP {label}: got {pre.origins}, {pre.cpp_ran}")
    # The self-test's own compiler: its absence and a mismatched version
    # both stop.
    saved = os.environ.get(GHC_ENV)
    with tempfile.TemporaryDirectory() as tmp:
        fake = Path(tmp) / "ghc-9.10.1"
        fake.write_text("#!/bin/sh\necho 9.10.1\n", encoding="utf-8")
        fake.chmod(0o755)
        for label, value, needle in (
                ("a missing compiler", str(Path(tmp) / "no-ghc"), "not an "),
                ("a mismatched compiler", str(fake), "pins GHC")):
            os.environ[GHC_ENV] = value
            try:
                ghc_command(REPO_ROOT)
                failures.append(f"COMPILER {label}: no AuditError")
            except AuditError as error:
                if needle not in str(error):
                    failures.append(f"COMPILER {label}: {error}")
    if saved is None:
        os.environ.pop(GHC_ENV, None)
    else:
        os.environ[GHC_ENV] = saved
    # Compiler-backed: every fixture through the real ghc -E, under the
    # fixture configured settings.
    with tempfile.TemporaryDirectory() as work, \
            ThreadPoolExecutor(max_workers=min(8, os.cpu_count() or 2)) as pool:
        settings = fixture_settings(ghc, Path(work))
        detected = list(pool.map(lambda f: find_violations(f[1], settings),
                                 DETECTED_FIXTURES))
        clean = list(pool.map(lambda f: find_violations(f[1], settings),
                              CLEAN_FIXTURES))
        reasons = list(pool.map(lambda f: find_violations(f[1], settings),
                                REASON_FIXTURES))
        reported = list(pool.map(lambda f: find_violations(f[1], settings),
                                 REPORTED_FIXTURES))
        trees = list(pool.map(
            lambda f: _run_tree(
                fixture_settings(ghc, Path(work), f[1], f[2]),
                {f"{SCOPED_TREE}/Test/M.hs": f[3], **f[5]}),
            TREE_FIXTURES))
        exempted = {v.path for v in _run_tree(
            settings, {rel: _BANNED_SOURCE for rel, _ in EXEMPTION_FIXTURES})}
    for (label, _, lines), got in zip(DETECTED_FIXTURES, detected):
        if [v.line for v in got] != lines:
            failures.append(f"DETECT {label}: expected lines {lines}, got "
                            f"{[str(v) for v in got]}")
    for (label, _, needles), got in zip(REASON_FIXTURES, reasons):
        if len(got) != 1 or not all(n in got[0].reason for n in needles):
            failures.append(f"REASON {label}: {[str(v) for v in got]}")
    for (label, _), got in zip(REPORTED_FIXTURES, reported):
        if not got:
            failures.append(f"REPORTED {label}: certified clean")
    for (label, _), got in zip(CLEAN_FIXTURES, clean):
        if got:
            failures.append(f"CLEAN {label}: unexpected {[str(v) for v in got]}")
    for (label, _, _, _, lines, _), got in zip(TREE_FIXTURES, trees):
        if (not got) if lines is None else [v.line for v in got] != lines:
            failures.append(f"TREE {label}: expected lines {lines}, got "
                            f"{[str(v) for v in got]}")
    for rel, expected in EXEMPTION_FIXTURES:
        if (rel in exempted) != expected:
            failures.append(f"EXEMPTION {rel}: expected reported={expected}, "
                            f"got {rel in exempted}")
    for rel, reason in EXEMPTIONS.items():
        if not reason.strip():
            failures.append(f"EXEMPTION {rel} carries no reason")
    total = (CAPTURE_CASES + len(MAP_FIXTURES) + 2
             + len(GATE_ORDER) * (1 + len(ORDER_MUTATIONS))
             + len(DETECTED_FIXTURES) + len(CLEAN_FIXTURES)
             + len(REASON_FIXTURES) + len(REPORTED_FIXTURES)
             + len(TREE_FIXTURES) + len(EXEMPTION_FIXTURES))
    if failures:
        print(f"headless_init_import_audit self-test: {len(failures)} of "
              f"{total} case(s) FAILED:")
        for failure in failures:
            print(f"  {failure}")
        return 1
    print(f"headless_init_import_audit self-test: all {total} cases passed "
          f"({ghc}).")
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--self-test", action="store_true",
                      help="run the build-independent fixture suite")
    mode.add_argument("--cabal-regression", action="store_true",
                      help="record and scan a tiny package with real Cabal "
                           "(after the project build)")
    mode.add_argument("--record", action="store_true",
                      help="bind the build's captured commands; pass the "
                           "build's own cabal arguments after `--`")
    parser.add_argument("--builddir", default=DEFAULT_BUILDDIR,
                        help="the build directory the suite was built in")
    parser.add_argument("cabal_args", nargs=argparse.REMAINDER,
                        help="with --record: `-- build ...`, the build's "
                             "own cabal arguments")
    args = parser.parse_args(argv)
    cabal_args = args.cabal_args[1:] if args.cabal_args[:1] == ["--"] \
        else args.cabal_args
    if cabal_args and not args.record:
        parser.error("cabal arguments are only taken with --record")
    try:
        if args.self_test:
            return self_test()
        if args.cabal_regression:
            return run_cabal_regression()
        if args.record:
            stamp = record_settings(REPO_ROOT, args.builddir, cabal_args)
            print(f"Recorded the headless suite's build settings in {stamp}.")
            return 0
        missing = [rel for rel in EXEMPTIONS if not (REPO_ROOT / rel).is_file()]
        if missing:
            print(f"Stale exemption(s), no such file: {', '.join(missing)}")
            return 1
        ways = configured_settings(REPO_ROOT, args.builddir)
        violations = scan_configured(REPO_ROOT, ways)
    except AuditError as error:
        print(f"headless_init_import_audit: {error}")
        return 2
    if violations:
        print(f"{len(violations)} {SCOPED_TREE}/ import(s) of the production "
              f"{INIT_MODULE}.{BANNED_IDENTIFIER}, or module(s) GHC could "
              f"not preprocess:")
        for violation in violations:
            print(f"  {violation}")
        print(f"\nIt hard-wires the engine log to stdout, so SYNARCHY_TEST_LOG "
              f"cannot steer it. Boot through {HARNESS_MODULE} "
              f"(initializeEngineHeadlessQuiet, or "
              f"initializeEngineHeadlessLogging for a spec that wants the "
              f"entries); see engine contracts §Headless fixture logging.")
        return 1
    print(f"No {SCOPED_TREE}/ module imports {INIT_MODULE}."
          f"{BANNED_IDENTIFIER} outside {HARNESS_MODULE} "
          f"({len(_modules(REPO_ROOT))} modules preprocessed as "
          f"{ways[0].description}).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
