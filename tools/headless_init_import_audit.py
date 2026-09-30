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
build, not the file on disk (#2648 owner amendment). The gate runs right
after `cabal build synarchy-test-headless` and preprocesses every module
with `ghc -E`, using exactly the arguments Cabal gave GHC for that
component, read from the `build-info.json` Cabal writes because
cabal.project sets `build-info: True` (`configured_settings`). Those
arguments force-include the component's own generated `cabal_macros.h`
(real dependency, package, host-tool and component macros) and carry the
native `cpp-options`, extensions, include directories and `-package-id`s.
So GHC decides whether CPP runs, and cpp expands directives, splices,
comment pasting, configured macros and every header it reads, wherever
that header lives, exactly as the build does. Nothing about CPP or Cabal
is reconstructed here.

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

NO SETTINGS, NO VERDICT: a missing or unreadable plan, `build-info.json`
or generated header; build information older than a configuration input
(`synarchy.cabal`, `cabal.project*`), for another checkout, or lacking
the headless component; a header that is not that component's own; a
compiler that is missing, reports another version, or differs from the
`tested-with` pin: each stops the gate (exit 2) with the cause. A module
`ghc -E` cannot preprocess fails it (exit 1).

THE CERTIFIED ENVIRONMENT is the configured build that just ran: Linux
in CI's `test-and-audits`, and the developer's native configuration
under `tools/ci-local.sh`. Other platforms, flag settings, installed
tools and dependency versions are not predicted: a branch that only
another environment would take is that environment's build to check.

The self-test (`--self-test`, run in `static-audits`) needs no build: it
drives the same code with fixture settings and a fixture
`cabal_macros.h`, through `ghc` on PATH or `SYNARCHY_AUDIT_GHC` at the
`tested-with` version. Source formats other than plain Haskell (`.lhs`,
`.hsig`, Cabal's `.hsc`/`.x`/`.y`), custom preprocessors (`-F -pgmF`) and
quasiquote contents are out of scope.

The single exemption is `Test.Headless.Harness.Log`, the boundary that
owns the backend choice (requirement 4). It is matched by exact path, so
a sibling module or a same-named file elsewhere is scanned as usual.

Usage:
  python3 tools/headless_init_import_audit.py              # the gate (after
                                                           # the suite build)
  python3 tools/headless_init_import_audit.py --self-test  # fixture suite
"""
from __future__ import annotations

import argparse
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
PLAN_JSON = Path("dist-newstyle") / "cache" / "plan.json"
# What decides the configuration: build settings older than any of these
# describe a build of different inputs.
CONFIGURATION_INPUTS = (CABAL_FILE, "cabal.project", "cabal.project.local",
                        "cabal.project.freeze")


@dataclass(frozen=True)
class BuildSettings:
    """How Cabal compiled the headless suite in this checkout: the
    compiler it ran, that component's exact GHC arguments (which
    force-include its generated `cabal_macros.h`), and the header."""
    ghc: str
    args: tuple[str, ...]
    header: Path
    description: str


def _load_json(path: Path, what: str) -> dict:
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except FileNotFoundError:
        raise AuditError(
            f"{what} {path} does not exist: build the headless suite first "
            f"(cabal build {HEADLESS_SUITE}), with `build-info: True` for "
            f"package synarchy in cabal.project") from None
    except (OSError, ValueError) as error:
        raise AuditError(f"{what} {path} is unreadable: {error}") from None


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


def configured_settings(root: Path = REPO_ROOT) -> BuildSettings:
    """The headless suite's settings from the build that just ran here,
    each checked rather than trusted:

      * `dist-newstyle/cache/plan.json` names the package's unit and the
        `build-info.json` Cabal writes for it (`build-info: True`);
      * that file must be newer than every configuration input, describe
        this checkout, carry the `test:synarchy-test-headless` component,
        and name the plan's compiler;
      * the compiler must exist, report that version, and match the
        `tested-with` pin;
      * the component's arguments must force-include its OWN generated
        `cabal_macros.h`, which must be readable and name the component.

    Anything missing, stale, mismatched or unreadable is an `AuditError`
    naming the cause: no settings, no verdict."""
    plan_path = root / PLAN_JSON
    plan = _load_json(plan_path, "Cabal's build plan")
    try:
        package = _PACKAGE_NAME.search(
            (root / CABAL_FILE).read_text(encoding="utf-8")).group(1)
    except (OSError, AttributeError):
        raise AuditError(f"{CABAL_FILE} names no package") from None
    units = [u for u in plan.get("install-plan", [])
             if u.get("pkg-name") == package and u.get("style") == "local"]
    unit = next((u for u in units if u.get("component-name")
                 == HEADLESS_COMPONENT), None) or next(
                     (u for u in units if u.get("component-name") is None), None)
    if unit is None or not unit.get("build-info"):
        raise AuditError(
            f"{plan_path} has no local {package} unit with a build-info "
            f"path; re-run cabal build {HEADLESS_SUITE}")
    info_path = Path(unit["build-info"])
    info = _load_json(info_path, "Cabal's build information")
    newest = max(((root / name).stat().st_mtime, name)
                 for name in CONFIGURATION_INPUTS if (root / name).exists())
    if info_path.stat().st_mtime < newest[0]:
        raise AuditError(
            f"{info_path} is stale: {newest[1]} changed after the last "
            f"build; re-run cabal build {HEADLESS_SUITE}")
    component = next((c for c in info.get("components", [])
                      if c.get("name") == HEADLESS_COMPONENT), None)
    if component is None:
        raise AuditError(
            f"{info_path} has no {HEADLESS_COMPONENT} component: the last "
            f"build did not build the headless suite (cabal build "
            f"{HEADLESS_SUITE})")
    if Path(component.get("src-dir", "")).resolve() != root.resolve():
        raise AuditError(
            f"{info_path} describes a build of {component.get('src-dir')}, "
            f"not {root}")
    compiler = info.get("compiler", {})
    compiler_id = compiler.get("compiler-id")
    if compiler_id != plan.get("compiler-id"):
        raise AuditError(
            f"{info_path} names compiler {compiler_id}, but {plan_path} "
            f"names {plan.get('compiler-id')}; re-run cabal build "
            f"{HEADLESS_SUITE}")
    ghc = shutil.which(compiler.get("path") or "")
    if ghc is None:
        raise AuditError(
            f"the compiler Cabal built with, {compiler.get('path')!r}, is "
            f"not an executable")
    version = _compiler_version(ghc)
    if f"ghc-{version}" != compiler_id:
        raise AuditError(f"{ghc} reports GHC {version}, not {compiler_id}")
    pin = _tested_with(root)
    if version != pin:
        raise AuditError(
            f"the headless suite was built with GHC {version}, but "
            f"{CABAL_FILE} pins GHC {pin}")
    args = tuple(component.get("compiler-args", []))
    header = next(
        (Path(args[i + 1][len("-optP"):]) for i in range(len(args) - 1)
         if args[i] == "-optP-include" and args[i + 1].startswith("-optP")),
        None)
    if header is None or header.parts[-3:] != (HEADLESS_SUITE, "autogen",
                                                "cabal_macros.h"):
        raise AuditError(
            f"{info_path}'s {HEADLESS_COMPONENT} arguments do not "
            f"force-include its own autogen/cabal_macros.h (found "
            f"{header})")
    header = header if header.is_absolute() else root / header
    try:
        text = header.read_text(encoding="utf-8")
    except OSError as error:
        raise AuditError(
            f"the suite's generated {header} is unreadable: "
            f"{error.strerror}; re-run cabal build {HEADLESS_SUITE}") from None
    identity = f'#define CURRENT_COMPONENT_ID "{component.get("unit-id")}"'
    if identity not in text:
        raise AuditError(
            f"{header} does not belong to {component.get('unit-id')}; "
            f"re-run cabal build {HEADLESS_SUITE}")
    return BuildSettings(
        ghc, args, header,
        f"{HEADLESS_COMPONENT} as built by {compiler_id} "
        f"({plan.get('os')}/{plan.get('arch')}, flags {unit.get('flags')})")


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


def _settings_checkout(root: Path, ghc: str, version: str) -> dict:
    """A synthetic built checkout for `configured_settings`: its cabal
    file, plan, build-info and generated header. Returns the pieces so a
    case can break one."""
    (root / CABAL_FILE).write_text(
        f"name: synarchy\nversion: 0.1.0.0\ntested-with: GHC =={version}\n",
        encoding="utf-8")
    build = root / "dist-newstyle" / "build" / "synarchy-0.1.0.0"
    autogen = build / "build" / HEADLESS_SUITE / "autogen"
    autogen.mkdir(parents=True)
    header = autogen / "cabal_macros.h"
    header.write_text(FIXTURE_HEADER, encoding="utf-8")
    info_path = build / "build-info.json"
    plan = {"compiler-id": f"ghc-{version}", "os": "fixture-os",
            "arch": "fixture-arch", "install-plan": [
                {"pkg-name": "synarchy", "style": "local",
                 "component-name": None, "flags": {},
                 "build-info": str(info_path)}]}
    info = {"compiler": {"compiler-id": f"ghc-{version}", "path": ghc},
            "components": [{
                "name": HEADLESS_COMPONENT, "src-dir": str(root) + "/",
                "unit-id": f"synarchy-0.1.0.0-inplace-{HEADLESS_SUITE}",
                "compiler-args": list(_FIXTURE_ARGS) + [
                    "-optP-include",
                    f"-optP{header.relative_to(root).as_posix()}"]}]}
    return {"plan": plan, "info": info, "info_path": info_path,
            "header": header}


def _write_checkout(root: Path, pieces: dict) -> None:
    (root / PLAN_JSON).parent.mkdir(parents=True, exist_ok=True)
    (root / PLAN_JSON).write_text(json.dumps(pieces["plan"]), encoding="utf-8")
    pieces["info_path"].write_text(json.dumps(pieces["info"]),
                                   encoding="utf-8")


def _break(case: str, root: Path, pieces: dict, fake_ghc: str) -> None:
    """Break one piece of a synthetic checkout, as case `case` names."""
    info, plan = pieces["info"], pieces["plan"]
    component = info["components"][0]
    if case == "no plan":
        (root / PLAN_JSON).unlink()
    elif case == "unreadable plan":
        (root / PLAN_JSON).write_text("{", encoding="utf-8")
    elif case == "no package unit":
        plan["install-plan"] = []
    elif case == "no build-info":
        pieces["info_path"].unlink()
    elif case == "stale build-info":
        os.utime(root / CABAL_FILE, (time.time() + 60, time.time() + 60))
    elif case == "no headless component":
        component["name"] = "test:synarchy-test-graphical"
    elif case == "another checkout":
        component["src-dir"] = "/nonexistent/checkout/"
    elif case == "compiler mismatch":
        info["compiler"]["compiler-id"] = "ghc-9.10.1"
    elif case == "missing compiler":
        info["compiler"]["path"] = str(root / "no-ghc")
    elif case == "wrong compiler version":
        info["compiler"]["path"] = fake_ghc
    elif case == "pin mismatch":
        text = (root / CABAL_FILE).read_text(encoding="utf-8")
        (root / CABAL_FILE).write_text(
            re.sub(r"GHC ==\S+", "GHC ==9.10.1", text), encoding="utf-8")
        os.utime(root / CABAL_FILE, (time.time() - 60, time.time() - 60))
    elif case == "no header argument":
        component["compiler-args"] = list(_FIXTURE_ARGS)
    elif case == "another component's header":
        component["compiler-args"][-1] = component["compiler-args"][-1].replace(
            HEADLESS_SUITE, "synarchy-test-graphical")
    elif case == "missing header":
        pieces["header"].unlink()
    elif case == "unreadable header":
        pieces["header"].unlink()
        pieces["header"].mkdir()
    elif case == "foreign header":
        pieces["header"].write_text(
            FIXTURE_HEADER.replace(HEADLESS_SUITE, "synarchy-test-graphical"),
            encoding="utf-8")
    if case not in ("no plan", "unreadable plan", "no build-info"):
        _write_checkout(root, pieces)
        if case != "stale build-info":
            os.utime(pieces["info_path"], (time.time() + 1, time.time() + 1))


# `(case, substring the AuditError names)`
SETTINGS_ERROR_FIXTURES: list[tuple[str, str]] = [
    ("no plan", "does not exist"),
    ("unreadable plan", "unreadable"),
    ("no package unit", "no local synarchy unit"),
    ("no build-info", "does not exist"),
    ("stale build-info", "is stale"),
    ("no headless component", f"no {HEADLESS_COMPONENT} component"),
    ("another checkout", "describes a build of"),
    ("compiler mismatch", "names compiler ghc-9.10.1"),
    ("missing compiler", "not an executable"),
    ("wrong compiler version", "reports GHC 9.10.1"),
    ("pin mismatch", "pins GHC 9.10.1"),
    ("no header argument", "do not force-include"),
    ("another component's header", "do not force-include"),
    ("missing header", "unreadable"),
    ("unreadable header", "unreadable"),
    ("foreign header", "does not belong"),
]


def self_test() -> int:
    failures: list[str] = []
    ghc = ghc_command(REPO_ROOT)
    version = _compiler_version(ghc)
    # The configured settings: a valid checkout loads, every broken one
    # stops with its cause.
    with tempfile.TemporaryDirectory() as tmp:
        fake = Path(tmp) / "ghc-9.10.1"
        fake.write_text("#!/bin/sh\necho 9.10.1\n", encoding="utf-8")
        fake.chmod(0o755)
        for case, needle in [("valid", "")] + SETTINGS_ERROR_FIXTURES:
            root = Path(tmp) / case.replace(" ", "-").replace("'", "")
            root.mkdir()
            pieces = _settings_checkout(root, ghc, version)
            _write_checkout(root, pieces)
            os.utime(pieces["info_path"], (time.time() + 1, time.time() + 1))
            _break(case, root, pieces, str(fake))
            try:
                loaded = configured_settings(root)
                if needle:
                    failures.append(f"SETTINGS {case}: loaded, expected "
                                    f"an error naming {needle!r}")
                elif loaded.header.resolve() != pieces["header"].resolve() \
                        or loaded.args[-2:] != tuple(
                            pieces["info"]["components"][0]["compiler-args"][-2:]):
                    failures.append(f"SETTINGS valid: {loaded}")
            except AuditError as error:
                if not needle or needle not in str(error):
                    failures.append(f"SETTINGS {case}: {error}")
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
    total = (1 + len(SETTINGS_ERROR_FIXTURES) + len(MAP_FIXTURES) + 2
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
    parser.add_argument("--self-test", action="store_true",
                        help="run the fixture suite instead of the gate")
    args = parser.parse_args(argv)
    try:
        if args.self_test:
            return self_test()
        missing = [rel for rel in EXEMPTIONS if not (REPO_ROOT / rel).is_file()]
        if missing:
            print(f"Stale exemption(s), no such file: {', '.join(missing)}")
            return 1
        settings = configured_settings(REPO_ROOT)
        violations = scan_tree(REPO_ROOT, settings)
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
          f"{settings.description}).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
