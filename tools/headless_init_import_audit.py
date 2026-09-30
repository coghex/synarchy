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

WHAT IS READ: the source GHC compiles, not the file on disk. Every
module goes through the real compiler's preprocessing step, `ghc -E`,
so GHC alone decides whether CPP runs -- a real `LANGUAGE CPP` or
`OPTIONS_GHC -XCPP` pragma, in whatever syntax GHC accepts, or the
suite's own extensions -- and the preprocessor expands directives,
backslash-newline splices, `/**/` comment pasting and configured macros
exactly as the build does. A pragma quoted in a string or comment, or
one GHC ignores as misplaced, changes nothing. The imports are then read
from that output with `unicode_operator_audit.py`'s comment/string lexer
(`haskell_code_only`) and `lua_strict_decode_audit.py`'s layout-aware
splitter (`haskell_import_declarations`), which follow GHC's whitespace
and eight-column tab stops. The lexer also masks GHC MultilineStrings
literals, whatever quotes, comment openers or marker-like lines they
hold. Masked comment tabs stay tabs, and a line-1 `#!` line is blanked,
as GHC skips it. A reported line is mapped back through the `LINE`
pragma and cpp line markers to the module's own source line; text from
an `#include` maps to the including line. When a module suppresses the
markers (`-optP-P`), the report takes the one source line with the same
text, or says the line is the preprocessed text's.

THE CONFIGURATION is the headless suite's own, read from
`synarchy.cabal`: the `test-suite synarchy-test-headless` stanza and
the `common` stanzas it imports supply `default-language`,
`default-extensions` (as `-X` flags), `cpp-options` (as `-optP`),
`include-dirs` (as `-I`), and the `ghc-options` that reach the
preprocessor (`-optP`, `-D`, `-U`, `-I`, `-X`, `-cpp`). Every value of
the `flag(...)` and `os(linux|darwin)` conditions they sit under is
enumerated, and EVERY module is preprocessed under every distinct
configuration: nothing is inferred from the output, since `-optP-P`
hides the markers that would show CPP ran. So a macro from
`cpp-options`, such as the darwin-only `-DDARWIN`, is checked on a Linux
runner too. A condition, field or preprocessor this reader does not
know, such as `-pgmP`, `-F` or `arch(...)`, stops the gate with the
reason rather than being skipped.

CABAL'S MACROS (`_cabal_macro_flags`) are rebuilt from the same stanzas,
without a configured build. Every `build-depends` package gets
`VERSION_<p>` defined, as in Cabal's `cabal_macros.h`, so `#ifdef` and
`defined(...)` read exactly as the build reads them; non-dependencies
stay undefined. Values are exact for `base` (the pinned GHC's, via
`-hide-all-packages -package base`), for the package itself, and for
`CURRENT_PACKAGE_VERSION`/`CURRENT_COMPONENT_ID`. Any other dependency's
`MIN_VERSION_<p>` needs the build plan's version, so an `#if` comparing
it fails the gate with a message naming the macro. A preprocessor
directive, in the module or a header it includes, naming Cabal's
host-tool macros (`TOOL_VERSION_*`, `MIN_TOOL_VERSION_*`) fails it too:
those exist only for programs installed on the build host.

THE COMPILER is `ghc` on PATH, or the executable named by
`SYNARCHY_AUDIT_GHC`. Its `--numeric-version` must equal
`synarchy.cabal`'s `tested-with: GHC ==` pin (9.12.2, the CI image's
GHC_VERSION). A missing or mismatched compiler, an unreadable
configuration, or a module `ghc -E` cannot preprocess fails the gate
with GHC's own message; nothing is certified silently.

LIMITS, all absent from `test-headless/` today:
  * GHC's own platform macros (`darwin_HOST_OS`, `x86_64_HOST_ARCH`
    and the like) are the running host's. They cannot be overridden,
    because GHC passes them after every user flag. CI checks the Linux
    branches; a local `make ci` checks the macOS ones.
  * Dependency version comparisons and host-tool macros fail closed, as
    above, rather than being evaluated.
  * The host-tool check reads the headers cpp's markers name, so it
    does not see into an `#include` of a module that also suppresses the
    markers.
Source formats other than plain Haskell (`.lhs`, `.hsig`, Cabal's
`.hsc`/`.x`/`.y`), custom preprocessors (`-F -pgmF`) and quasiquote
contents are out of scope.

The single exemption is `Test.Headless.Harness.Log`, the boundary that
owns the backend choice (requirement 4). It is matched by exact path, so
a sibling module or a same-named file elsewhere is scanned as usual.

Usage:
  python3 tools/headless_init_import_audit.py              # the gate
  python3 tools/headless_init_import_audit.py --self-test  # fixture suite
"""
from __future__ import annotations

import argparse
import itertools
import os
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
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
# The platforms the suite is built on (CLAUDE.md: macOS and Linux).
SUPPORTED_OS = ("linux", "darwin")
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
# The headless suite's preprocessing configuration, from synarchy.cabal
# ---------------------------------------------------------------------

@dataclass(frozen=True)
class Configuration:
    """One preprocessing configuration: the cabal-derived `flags`, and
    `macros`, the flags that rebuild Cabal's `cabal_macros.h`
    environment for the suite's dependencies (`_cabal_macro_flags`)."""
    label: str
    flags: tuple[str, ...]
    macros: tuple[str, ...] = ()

    @property
    def command(self) -> tuple[str, ...]:
        return self.flags + self.macros

_STANZA = re.compile(
    r"(?i)(common|test-suite|library|executable|benchmark|foreign-library"
    r"|flag|source-repository|custom-setup)(?:\s+(\S+))?\s*\Z")
_FIELD = re.compile(r"(?P<name>[A-Za-z][A-Za-z0-9-]*)\s*:(?P<value>.*)\Z")
_CONDITION = re.compile(r"(?P<negated>!?)\s*(?P<kind>flag|os)\s*\(\s*"
                        r"(?P<name>[A-Za-z0-9_-]+)\s*\)\Z")
# `ghc-options` that reach the preprocessor, and those that replace it.
_PREPROCESSOR_OPTION = re.compile(r"-(?:optP|D|U|I|X)\S*\Z|-cpp\Z")
_CUSTOM_PREPROCESSOR = re.compile(r"-(?:pgmP|pgmF|optF|pgmL)\S*\Z|-F\Z")
_TESTED_WITH_GHC = re.compile(r"(?im)^tested-with\s*:.*?GHC\s*==\s*([0-9.]+)")
_PACKAGE_FIELD = re.compile(r"(?im)^(name|version)\s*:\s*(\S+)\s*$")
_DEPENDENCY = re.compile(r"\s*([A-Za-z0-9][A-Za-z0-9-]*)")
# A Cabal host-tool macro. Which ones Cabal defines depends on the
# programs installed on the build host, so no static environment can
# reproduce `#ifdef TOOL_VERSION_alex`; a directive naming one fails.
_TOOL_MACRO = re.compile(r"(?<![\w'])(?:MIN_)?TOOL_VERSION_\w+")
_DIRECTIVE_LINE = re.compile(r"(?m)^[^\S\n]*#(?:[^\n]*\\\n)*[^\n]*")


def _cabal_stanzas(text: str) -> dict[tuple[str, str], list[str]]:
    """Top-level stanzas as `(kind, name)` -> their indented lines."""
    stanzas: dict[tuple[str, str], list[str]] = {}
    current: list[str] | None = None
    for line in text.splitlines():
        if not line.strip() or line.lstrip().startswith("--"):
            continue
        if not line[0].isspace():
            header = _STANZA.match(line.strip())
            current = (stanzas.setdefault(
                (header.group(1).lower(), (header.group(2) or "").lower()), [])
                if header else None)
        elif current is not None:
            current.append(line)
    return stanzas


def _stanza_fields(lines: list[str], where: str
                   ) -> list[tuple[tuple[str, ...], str, str]]:
    """`(conditions, field, value)` for every field in one stanza, a
    field's continuation lines joined to its value. A condition is a
    `flag(x)`/`os(x)` test, prefixed `!` when it holds under `else`."""
    fields: list[tuple[tuple[str, ...], str, str]] = []
    blocks: list[tuple[int, str]] = []     # open `if`/`else` blocks
    last_if: dict[int, str] = {}
    field: list | None = None               # [indent, conds, name, parts]
    for line in lines:
        if line[:len(line) - len(line.lstrip())].count("\t"):
            raise AuditError(f"{where}: a tab indents {line.strip()!r}")
        indent = len(line) - len(line.lstrip())
        content = line.strip()
        if field is not None and indent > field[0]:
            field[3].append(content)
            continue
        if field is not None:
            fields.append((field[1], field[2], " ".join(field[3])))
            field = None
        while blocks and indent <= blocks[-1][0]:
            blocks.pop()
        conditions = tuple(condition for _, condition in blocks)
        if content.startswith("if ") or content == "else":
            if content == "else":
                if indent not in last_if:
                    raise AuditError(f"{where}: `else` without an `if`")
                test = last_if.pop(indent)
                condition = test[1:] if test.startswith("!") else "!" + test
            else:
                test = content[3:].strip()
                match = _CONDITION.match(test)
                if match is None:
                    raise AuditError(
                        f"{where}: cannot evaluate the condition {test!r}; "
                        f"only flag(...) and os(...) are modelled")
                condition = (match.group("negated")
                             + f"{match.group('kind')}({match.group('name')})")
                last_if[indent] = condition
            blocks.append((indent, condition))
            continue
        match = _FIELD.match(content)
        if match is None or "{" in content:
            raise AuditError(f"{where}: cannot read {content!r}")
        field = [indent, conditions, match.group("name").lower(),
                 [match.group("value").strip()]]
    if field is not None:
        fields.append((field[1], field[2], " ".join(field[3])))
    return fields


def _holds(condition: str, assignment: dict[str, object]) -> bool:
    negated = condition.startswith("!")
    kind, name = condition.lstrip("!")[:-1].split("(")
    name = "darwin" if name.lower() == "osx" else name.lower()
    value = (assignment["os"] == name if kind == "os"
             else assignment[f"flag:{name}"])
    return bool(value) != negated


def _field_flags(name: str, value: str, where: str) -> list[str]:
    """The `ghc -E` flags one cabal field contributes."""
    try:
        tokens = shlex.split(value.replace(",", " "))
    except ValueError as error:
        raise AuditError(f"{where}: cannot split {name}: {error}") from None
    if name == "default-language":
        return [f"-X{token}" for token in tokens]
    if name in ("default-extensions", "extensions"):
        return [f"-X{token}" for token in tokens]
    if name == "cpp-options":
        return [f"-optP{token}" for token in tokens]
    if name == "include-dirs":
        return [f"-I{token}" for token in tokens]
    if name == "ghc-options":
        custom = [token for token in tokens if _CUSTOM_PREPROCESSOR.match(token)]
        if custom:
            raise AuditError(
                f"{where}: ghc-options {' '.join(custom)} runs a custom "
                f"preprocessor this gate does not model")
        return [token for token in tokens if _PREPROCESSOR_OPTION.match(token)]
    return []


def _dependency_names(value: str) -> list[str]:
    """The package names in one `build-depends` value."""
    return [match.group(1) for entry in value.split(",")
            if (match := _DEPENDENCY.match(entry)) and entry.strip()]


def _min_version(version: str) -> str:
    """Cabal's `MIN_VERSION_<p>(major1,major2,minor)` body for `version`."""
    a, b, c = ([int(part) for part in version.split(".")] + [0, 0, 0])[:3]
    return (f"((major1) < {a} || (major1) == {a} && (major2) < {b} || "
            f"(major1) == {a} && (major2) == {b} && (minor) <= {c})")


def _cabal_macro_flags(package: str, version: str,
                       dependencies: list[str]) -> tuple[str, ...]:
    """The `ghc -E` flags that rebuild the headless suite's Cabal macro
    environment, exact wherever the value is knowable without a build
    plan and unevaluable where it is not.

    Cabal defines `VERSION_<p>` and `MIN_VERSION_<p>` for exactly the
    component's direct dependencies (`-` spelled `_`), plus
    `CURRENT_PACKAGE_VERSION` and `CURRENT_COMPONENT_ID`. Here:

      * GHC's own package macros are switched off with
        `-hide-all-packages`, then `-package base` restores base's --
        exact, since the pinned GHC's base cannot be replaced;
      * the package itself, a dependency of its own test suite, gets its
        exact `version:` values;
      * every other dependency gets `VERSION_<p>` DEFINED, so `#ifdef`
        and `defined(...)` read exactly as Cabal would, but
        `MIN_VERSION_<p>` expands to a call of an undefined function-like
        macro, so an `#if` that compares its version fails with cpp's
        error instead of silently taking a branch;
      * `CURRENT_*` follow Cabal's in-place naming for a local package.
    """
    flags = ["-hide-all-packages"]
    for dependency in dict.fromkeys(dependencies):
        macro = dependency.replace("-", "_")
        if dependency == "base":
            flags += ["-package", "base"]
        elif dependency == package:
            flags += [f'-optP-DVERSION_{macro}="{version}"',
                      f"-optP-DMIN_VERSION_{macro}(major1,major2,minor)="
                      f"{_min_version(version)}"]
        else:
            flags += [f'-optP-DVERSION_{macro}="unknown"',
                      f"-optP-DMIN_VERSION_{macro}(major1,major2,minor)="
                      f"CABAL_VERSION_OF_{macro}_IS_UNKNOWN_TO_THIS_GATE("]
    flags += [f'-optP-DCURRENT_PACKAGE_VERSION="{version}"',
              f'-optP-DCURRENT_COMPONENT_ID='
              f'"{package}-{version}-inplace-{HEADLESS_SUITE}"']
    return tuple(flags)


def headless_configurations(repo_root: Path) -> list[Configuration]:
    """Every distinct preprocessing configuration of the headless suite,
    one per assignment of the `flag`/`os` conditions its fields sit
    under, labelled by the flags that assignment turns on."""
    cabal = repo_root / CABAL_FILE
    if not cabal.is_file():
        raise AuditError(f"{CABAL_FILE} not found under {repo_root}")
    text = cabal.read_text(encoding="utf-8")
    package_fields = {m.group(1).lower(): m.group(2)
                      for m in _PACKAGE_FIELD.finditer(text)}
    package = package_fields.get("name", "")
    version = package_fields.get("version", "0")
    stanzas = _cabal_stanzas(text)
    suite = ("test-suite", HEADLESS_SUITE)
    if suite not in stanzas:
        raise AuditError(f"{CABAL_FILE} has no `test-suite {HEADLESS_SUITE}`")
    fields: list[tuple[tuple[str, ...], str, str]] = []
    pending, seen = [suite], set()
    while pending:
        key = pending.pop(0)
        if key in seen:
            continue
        seen.add(key)
        if key not in stanzas:
            raise AuditError(f"{CABAL_FILE}: `import: {key[1]}` names no "
                             f"common stanza")
        where = f"{CABAL_FILE} {key[0]} {key[1]}"
        for conditions, name, value in _stanza_fields(stanzas[key], where):
            if name == "import":
                if conditions:
                    raise AuditError(f"{where}: a conditional import")
                pending.extend(("common", common.strip().lower())
                               for common in value.split(",") if common.strip())
            else:
                fields.append((conditions, name, value))
    flags = sorted({c.lstrip("!")[5:-1].lower() for conds, _, _ in fields
                    for c in conds if c.lstrip("!").startswith("flag(")})
    configurations: dict[tuple[tuple[str, ...], tuple[str, ...]], str] = {}
    for os_name, *values in itertools.product(
            SUPPORTED_OS, *([False, True] for _ in flags)):
        assignment: dict[str, object] = {"os": os_name}
        assignment.update({f"flag:{flag}": value
                           for flag, value in zip(flags, values)})
        result: list[str] = []
        dependencies: list[str] = []
        for conditions, name, value in fields:
            if all(_holds(c, assignment) for c in conditions):
                result.extend(_field_flags(name, value, CABAL_FILE))
                if name == "build-depends":
                    dependencies.extend(_dependency_names(value))
        label = "+".join([os_name] + [f for f, v in zip(flags, values) if v])
        configurations.setdefault(
            (tuple(result), _cabal_macro_flags(package, version, dependencies)),
            label)
    return [Configuration(label, flags, macros)
            for (flags, macros), label in configurations.items()]


# ---------------------------------------------------------------------
# The compiler
# ---------------------------------------------------------------------

def ghc_command(repo_root: Path = REPO_ROOT) -> str:
    """The GHC this gate preprocesses with, checked against the pin."""
    requested = os.environ.get(GHC_ENV) or "ghc"
    ghc = shutil.which(requested)
    if ghc is None:
        raise AuditError(
            f"{requested!r} is not an executable on PATH. This gate "
            f"preprocesses every module with the real GHC; install the "
            f"pinned toolchain (ghcup install ghc, the version in "
            f"{CABAL_FILE}'s tested-with) or set {GHC_ENV}.")
    pin = _TESTED_WITH_GHC.search(
        (repo_root / CABAL_FILE).read_text(encoding="utf-8"))
    if pin is None:
        raise AuditError(f"{CABAL_FILE} pins no `tested-with: GHC ==` version")
    try:
        version = subprocess.run(
            [ghc, "--numeric-version"], capture_output=True, text=True,
            timeout=PREPROCESS_TIMEOUT_SECONDS, check=True).stdout.strip()
    except (OSError, subprocess.SubprocessError) as error:
        raise AuditError(f"{ghc} --numeric-version failed: {error}") from None
    if version != pin.group(1):
        raise AuditError(
            f"{ghc} is GHC {version}, but {CABAL_FILE} pins GHC "
            f"{pin.group(1)}; preprocess with the pinned compiler (set "
            f"{GHC_ENV} to its path).")
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
    files: frozenset[str] = frozenset()      # every file a marker names


def map_output(output: str, source: str, verbatim: bool = False) -> Preprocessed:
    """GHC's `-E` output with its line markers blanked, each remaining
    line tagged with the `(file, line)` it came from. A line-1 `#!` line
    of the module itself is blanked too, as GHC's lexer skips it.

    `verbatim` says the output is GHC's untouched copy of the source
    behind its one `LINE` pragma, so no later line is a marker, however
    much a comment or string line looks like `# 7 "x.hs"`."""
    lines = output.split("\n")
    origins: list[tuple[str, int]] = []
    current, number, cpp_ran = source, 1, False
    files: set[str] = set()
    for index, line in enumerate(lines):
        marker = _LINE_MARKER.match(line) if index == 0 or not verbatim else None
        if marker:
            cpp_ran = cpp_ran or marker.group(3) is not None
            number = int(marker.group(1) or marker.group(3))
            current = (marker.group(2) if marker.group(2) is not None
                       else marker.group(4)).replace('\\"', '"')
            files.add(current)
            origins.append(("", 0))
            lines[index] = " " * len(line)
            continue
        origins.append((current, number))
        if (current, number) == (source, 1) and line.startswith("#!"):
            lines[index] = " " * len(line)
        number += 1
    return Preprocessed("\n".join(lines), tuple(origins), cpp_ran,
                        frozenset(files))


def preprocess(ghc: str, root: Path, rel_path: str,
               configuration: Configuration, work: Path) -> str:
    """`ghc -E` of one module under one configuration. Raises
    `PreprocessError` with GHC's message when it fails."""
    handle, name = tempfile.mkstemp(suffix=".hspp", dir=work)
    os.close(handle)
    output = Path(name)
    try:
        result = subprocess.run(
            [ghc, "-E", "-v0", *configuration.command, "-o", str(output),
             rel_path],
            cwd=root, capture_output=True, text=True,
            timeout=PREPROCESS_TIMEOUT_SECONDS)
    except (OSError, subprocess.SubprocessError) as error:
        raise PreprocessError(f"ghc -E could not run: {error}") from None
    if result.returncode != 0:
        message = " ".join(result.stderr.split())[:600]
        raise PreprocessError(
            f"ghc -E failed under {configuration.label}: {message}")
    try:
        return output.read_text(encoding="utf-8")
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
            line = own[-1] if own else 1
        violations.append((Violation(rel_path, line, reason),
                           pre.text.split("\n")[index]))
    return violations


_UNKNOWN_VERSION = re.compile(r"CABAL_VERSION_OF_(\w+)_IS_UNKNOWN_TO_THIS_GATE")


def _directive_tool_macros(text: str) -> set[str]:
    """The Cabal host-tool macros named on `text`'s preprocessor lines."""
    return {name for line in _DIRECTIVE_LINE.findall(text)
            for name in _TOOL_MACRO.findall(line)}


def check_module(ghc: str, root: Path, rel_path: str,
                 configurations: tuple[Configuration, ...] | list[Configuration],
                 work: Path) -> list[Violation]:
    """Every banned import GHC would see in one module, under EVERY
    distinct configuration. Nothing about CPP is inferred from the
    output -- `-optP-P` removes the line markers that would show it ran
    -- so each pass is run and scanned."""
    source = (root / rel_path).read_text(encoding="utf-8")
    verbatim = f'{{-# LINE 1 "{rel_path}" #-}}\n' + source
    outputs: list[tuple[Configuration, str]] = []
    for configuration in configurations:
        try:
            outputs.append((configuration, preprocess(
                ghc, root, rel_path, configuration, work)))
        except PreprocessError as error:
            unknown = sorted(
                {name for name in _UNKNOWN_VERSION.findall(
                    " ".join(configuration.macros))
                 if any(f"MIN_VERSION_{name}" in line
                        for line in _DIRECTIVE_LINE.findall(source))})
            hint = (f"; its preprocessor conditionals compare "
                    f"{', '.join('MIN_VERSION_' + n for n in unknown)}, a "
                    f"dependency version only a configured Cabal build "
                    f"knows, so this gate cannot choose the branch"
                    if unknown else "")
            return [Violation(rel_path, 1, f"{error}{hint}; a module GHC "
                              f"cannot preprocess cannot be certified")]
    found: dict[tuple[int, str], list[str]] = {}
    included: set[str] = set()
    for configuration, output in outputs:
        pre = map_output(output, rel_path, verbatim=output == verbatim)
        included.update(name for name in pre.files
                        if name != rel_path and not name.startswith("<"))
        markerless = output != verbatim and not pre.cpp_ran
        for violation, text in scan_preprocessed(pre, rel_path):
            line, reason = violation.line, violation.reason
            if markerless:
                # Preprocessed, yet no cpp marker maps it back
                # (`-optP-P`): take the one source line with the same
                # text, or say the line is the preprocessed text's.
                same = [n for n, source_line in
                        enumerate(source.split("\n"), 1)
                        if source_line.strip() == text.strip()]
                if len(same) == 1:
                    line = same[0]
                    reason += (" (cpp line markers were suppressed; located "
                               "by its text)")
                else:
                    reason += (" (cpp line markers were suppressed, so the "
                               "line is the preprocessed text's)")
            found.setdefault((line, reason), []).append(configuration.label)
    if any(output != verbatim for _, output in outputs):
        texts = [source] + [
            (root / name).read_text(encoding="utf-8", errors="replace")
            for name in sorted(included)
            if (root / name).resolve().is_relative_to(root.resolve())
            and (root / name).is_file()]
        tools = sorted(set().union(*map(_directive_tool_macros, texts)))
        if tools:
            found.setdefault((1, (
                f"its preprocessor directives name Cabal's host-tool "
                f"macro(s) {', '.join(tools)}, which Cabal defines only for "
                f"programs installed on the build host, so this gate cannot "
                f"certify the branch it takes")), []).extend(
                    c.label for c, _ in outputs)
    labels = [configuration.label for configuration, _ in outputs]
    return [Violation(rel_path, line,
                      reason if seen == labels
                      else f"{reason} (under {', '.join(seen)})")
            for (line, reason), seen in sorted(found.items())]


def _modules(root: Path) -> list[str]:
    tree = root / SCOPED_TREE
    paths = sorted({*tree.glob("**/*.hs"), *tree.glob("**/*.hs-boot")})
    return [rel for rel in (p.relative_to(root).as_posix() for p in paths)
            if rel not in EXEMPTIONS]


def scan_tree(root: Path, ghc: str | None = None) -> list[Violation]:
    """Every banned import in `root`'s `test-headless/`, preprocessed
    under `root`'s headless suite configuration."""
    ghc = ghc or ghc_command(root)
    configurations = headless_configurations(root)
    modules = _modules(root)
    with tempfile.TemporaryDirectory() as work, \
            ThreadPoolExecutor(max_workers=min(8, os.cpu_count() or 2)) as pool:
        results = pool.map(
            lambda rel: check_module(ghc, root, rel, configurations, Path(work)),
            modules)
        return [violation for result in results for violation in result]


def find_violations(text: str, ghc: str,
                    rel_path: str = "Fixture.hs") -> list[Violation]:
    """One module's source checked under the real headless suite's
    configurations, as `scan_tree` checks it."""
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        (root / rel_path).write_text(text, encoding="utf-8")
        return check_module(ghc, root, rel_path, _suite_configurations(), root)


_SUITE_CONFIGURATIONS: list[Configuration] = []


def _suite_configurations() -> list[Configuration]:
    if not _SUITE_CONFIGURATIONS:
        _SUITE_CONFIGURATIONS.extend(headless_configurations(REPO_ROOT))
    return _SUITE_CONFIGURATIONS


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
    ("a module ghc -E cannot preprocess fails the gate: cpp's error on a "
     "version macro only Cabal defines",
     "{-# LANGUAGE CPP #-}\n" + _HEAD + "#if MIN_VERSION_hspec(2,0,0)\n"
     + _ALLOWED + "#endif\n", [1]),
    # Every configuration is preprocessed: `-P` drops the markers that
    # would show CPP ran (#2648 review round 4).
    ("a marker-suppressing CPP module takes the banned branch under the "
     "darwin configurations", _NO_MARKERS + _HEAD
     + _branch("ifdef DARWIN", _BANNED), [5]),
    ("the same under the dev configurations",
     _NO_MARKERS + _HEAD + _branch("ifdef DEVELOPMENT", _BANNED), [5]),
    ("a marker-suppressed banned import found in every configuration",
     _NO_MARKERS + _HEAD + _BANNED, [4]),
    # Cabal's dependency macros (#2648 review round 4): definedness is
    # Cabal's, a dependency version comparison fails closed.
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
    ("a dependency version comparison fails closed",
     _CPP + _HEAD + _branch("if MIN_VERSION_hspec(2,0,0)", _BANNED), [1]),
    ("a host-tool macro fails closed", _CPP + _HEAD
     + _branch("ifdef TOOL_VERSION_alex", _ALLOWED), [1]),
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
    ("the version comparison names its macro and why it cannot be chosen",
     _CPP + _HEAD + _branch("if MIN_VERSION_hspec(2,0,0)", _BANNED),
     ("MIN_VERSION_hspec", "configured Cabal build")),
    ("the host-tool refusal names its macro", _CPP + _HEAD
     + _branch("ifdef TOOL_VERSION_alex", _ALLOWED), ("TOOL_VERSION_alex",)),
    ("a markerless report says how it was located",
     _NO_MARKERS + _HEAD + _branch("ifdef DARWIN", _BANNED),
     ("located by its text", "under darwin, darwin+dev")),
]

_FIXTURE_CABAL = f"test-suite {HEADLESS_SUITE}\n    default-language: GHC2024\n"
_DEV_FLAG = "flag dev\n    default: False\n    manual: True\n\n"

# `(label, synarchy.cabal text, module source, lines reported, extra
# files)`, each module scanned alone in its own tree at
# test-headless/Test/M.hs.
TREE_FIXTURES: list[tuple[str, str, str, list[int], dict[str, str]]] = [
    ("scan_tree reports the `where`-line import (#2648 review)",
     _FIXTURE_CABAL, "module M where import Engine.Core.Init "
     "(initializeEngineHeadless); fixture = initializeEngineHeadless\n",
     [1], {}),
    ("scan_tree reports it after a non-breaking space", _FIXTURE_CABAL,
     "module M where " + _BANNED, [1], {}),
    ("scan_tree reports it after a form feed", _FIXTURE_CABAL,
     "module M where\f" + _BANNED, [1], {}),
    ("scan_tree certifies an allowed list on non-breaking-space lines",
     _FIXTURE_CABAL,
     _HEAD + "import Engine.Core.Init\n  ( EngineInitResult(..) )\n",
     [], {}),
    ("scan_tree certifies an import after a tab-containing comment "
     "(#2648 review)", _FIXTURE_CABAL,
     _HEAD + "{-\t-} " + _ALLOWED + "           fixture = EngineInitResult\n",
     [], {}),
    ("scan_tree ignores a string-quoted CPP pragma", _FIXTURE_CABAL,
     _HEAD + _CPP_PROBE + "message = \"{-# LANGUAGE CPP #-}\"\n", [], {}),
    ("scan_tree ignores a CPP pragma nested in a block comment",
     _FIXTURE_CABAL, "{- {-# LANGUAGE CPP #-} -}\n" + _HEAD + _CPP_PROBE,
     [], {}),
    ("scan_tree ignores a misplaced CPP pragma", _FIXTURE_CABAL,
     _HEAD + "{-# LANGUAGE CPP #-}\n" + _CPP_PROBE, [], {}),
    ("scan_tree reads `LANGUAGE CPP{- note -}` as GHC does (#2648 review)",
     _FIXTURE_CABAL, "{-# LANGUAGE CPP{- note -} #-}\n" + _HEAD
     + "#define BOOT Engine.Core.Init\nimport BOOT (initializeEngineHeadless)\n"
     "fixture = initializeEngineHeadless\n", [4], {}),
    ("scan_tree reads a quoted `\"-XCPP\"` option (#2648 review)",
     _FIXTURE_CABAL, "{-# OPTIONS_GHC \"-XCPP\" #-}\n" + _HEAD
     + "#define BOOT Engine.Core.Init\nimport BOOT (initializeEngineHeadless)\n"
     "fixture = initializeEngineHeadless\n", [4], {}),
    ("scan_tree joins a line-spliced import (#2648 review)", _FIXTURE_CABAL,
     "{-# LANGUAGE CPP #-}\n" + _HEAD + "import Engine.Core.\\\n"
     "Init (initializeEngineHeadless)\nfixture = initializeEngineHeadless\n",
     [3], {}),
    ("scan_tree certifies a clean CPP module", _FIXTURE_CABAL,
     "{-# LANGUAGE CPP #-}\n" + _HEAD + "#ifdef DARWIN\n" + _ALLOWED
     + "#endif\n", [], {}),
    ("scan_tree reports an import behind a quoting shebang", _FIXTURE_CABAL,
     "#!/usr/bin/env runghc \"x\n" + _HEAD + _BANNED, [3], {}),
    ("a macro configured in cpp-options under a flag (#2648 review)",
     _DEV_FLAG + _FIXTURE_CABAL
     + "    if flag(dev)\n        cpp-options: -DBOOT=Engine.Core.Init\n",
     "{-# LANGUAGE CPP #-}\n" + _HEAD + "import BOOT (initializeEngineHeadless)\n",
     [3], {}),
    ("a darwin-only configured macro is checked on any host",
     _FIXTURE_CABAL + "    if os(darwin)\n        cpp-options: -DDARWIN\n",
     "{-# LANGUAGE CPP #-}\n" + _HEAD + "#ifdef DARWIN\n" + _BANNED
     + "#endif\n", [4], {}),
    ("a macro from a common stanza's ghc-options -optP",
     f"common policy\n    ghc-options: -O2 \"-optP-DBOOT=Engine.Core.Init\"\n\n"
     f"test-suite {HEADLESS_SUITE}\n    import: policy\n",
     "{-# LANGUAGE CPP #-}\n" + _HEAD + "import BOOT (initializeEngineHeadless)\n",
     [3], {}),
    ("CPP from the suite's default-extensions preprocesses every module",
     _FIXTURE_CABAL + "    default-extensions: OverloadedStrings, CPP\n",
     _HEAD + _CPP_PROBE, [5], {}),
    ("-cpp in the suite's ghc-options", _FIXTURE_CABAL
     + "    ghc-options: -Wall -cpp\n", _HEAD + _CPP_PROBE, [5], {}),
    ("a module's NoCPP turns the suite's CPP off",
     _FIXTURE_CABAL + "    default-extensions: CPP\n",
     "{-# LANGUAGE NoCPP #-}\n" + _HEAD + _CPP_PROBE, [], {}),
    ("cpp-options alone does not enable CPP",
     _FIXTURE_CABAL + "    cpp-options: -DDARWIN\n", _HEAD + _CPP_PROBE, [], {}),
    ("a commented-out cabal line changes nothing",
     _FIXTURE_CABAL + "    -- default-extensions: CPP\n",
     _HEAD + _CPP_PROBE, [], {}),
    ("another component's CPP does not reach the suite",
     "library\n    default-extensions: CPP\n\n" + _FIXTURE_CABAL,
     _HEAD + _CPP_PROBE, [], {}),
    ("scan_tree checks every configuration of a marker-suppressing module "
     "(#2648 review round 4)",
     _FIXTURE_CABAL + "    if os(darwin)\n        cpp-options: -DDARWIN\n",
     _NO_MARKERS + _HEAD + _branch("ifdef DARWIN", _BANNED), [5], {}),
    ("-optP-P from the suite's own ghc-options",
     _FIXTURE_CABAL + "    ghc-options: -optP-P\n"
     "    if os(darwin)\n        cpp-options: -DDARWIN\n",
     _CPP + _HEAD + _branch("ifdef DARWIN", _BANNED), [4], {}),
    ("scan_tree defines VERSION_hspec for a suite that depends on hspec "
     "(#2648 review round 4)",
     _FIXTURE_CABAL + "    build-depends: base >=4 && <5, hspec\n",
     _CPP + _HEAD + _branch("ifdef VERSION_hspec", _BANNED), [4], {}),
    ("and leaves it undefined for a suite that does not",
     _FIXTURE_CABAL + "    build-depends: base >=4 && <5\n",
     _CPP + _HEAD + _branch("ifdef VERSION_hspec", _BANNED), [], {}),
    ("a dependency added under a flag is defined in that configuration",
     _DEV_FLAG + _FIXTURE_CABAL + "    build-depends: base\n"
     "    if flag(dev)\n        build-depends: hspec\n",
     _CPP + _HEAD + _branch("ifdef VERSION_hspec", _BANNED), [4], {}),
    ("scan_tree fails closed on a dependency version comparison",
     _FIXTURE_CABAL + "    build-depends: base, hspec\n",
     _CPP + _HEAD + _branch("if MIN_VERSION_hspec(2,0,0)", _BANNED), [1], {}),
    ("scan_tree certifies the review's multiline string",
     _FIXTURE_CABAL, _ML + "module M (message) where\nimport Prelude (Char)\n"
     "message :: [Char]\nmessage = \"\"\"\n  A double quote: \"\n"
     "  import Engine.Core.Init (initializeEngineHeadless)\n  \"\"\"\n",
     [], {}),
    ("scan_tree reports a banned import before a multiline string",
     _FIXTURE_CABAL, _ML + _HEAD + _BANNED
     + 'message :: String\nmessage = """\n  a " quote\n  """\n', [3], {}),
    ("a host-tool macro in an included header fails closed",
     _FIXTURE_CABAL, _CPP + _HEAD + '#include "tool.h"\n' + _ALLOWED, [1],
     {"test-headless/Test/tool.h": "#ifdef TOOL_VERSION_happy\n#endif\n"}),
    ("an #include is reported at the including line",
     _FIXTURE_CABAL, "{-# LANGUAGE CPP #-}\n" + _HEAD
     + "#include \"banned.h\"\n", [3],
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

# `(label, synarchy.cabal text or None, substring the AuditError names)`
CONFIG_ERROR_FIXTURES: list[tuple[str, str | None, str]] = [
    ("no synarchy.cabal", None, "not found"),
    ("no headless suite", "library\n    default-language: GHC2024\n",
     HEADLESS_SUITE),
    ("a condition it cannot evaluate",
     _FIXTURE_CABAL + "    if arch(x86_64)\n        cpp-options: -DX\n",
     "arch(x86_64)"),
    ("a custom preprocessor", _FIXTURE_CABAL
     + "    ghc-options: -F -pgmF rewrite\n", "custom preprocessor"),
    ("an import of a missing common stanza",
     _FIXTURE_CABAL + "    import: nowhere\n", "nowhere"),
]

# The real synarchy.cabal, spelled out: a changed configuration fails
# here until this table is reread and updated.
_LANG = ("-XGHC2024", "-XDefaultSignatures", "-XDuplicateRecordFields",
         "-XMagicHash", "-XNoMonomorphismRestriction", "-XNoImplicitPrelude",
         "-XNumDecimals", "-XOverloadedStrings", "-XPatternSynonyms",
         "-XQuantifiedConstraints", "-XRecordWildCards",
         "-XTypeFamilyDependencies", "-XUnicodeSyntax", "-XViewPatterns",
         "-XQuasiQuotes")
_DARWIN = ("-optP-DDARWIN", "-optP-Wno-nonportable-include-path")
EXPECTED_HEADLESS_CONFIGURATIONS = [
    ("linux", _LANG),
    ("linux+dev", _LANG + ("-optP-DDEVELOPMENT",)),
    ("darwin", _LANG + _DARWIN),
    ("darwin+dev", _LANG + ("-optP-DDEVELOPMENT",) + _DARWIN),
]
# The `VERSION_<p>` names Cabal's generated cabal_macros.h defines for
# the suite (read from a configured build), which every configuration's
# macro environment must define too, and nothing else.
EXPECTED_DEPENDENCY_MACROS = frozenset(
    "GLFW_b JuicyPixels aeson async base base_unicode_symbols bytestring "
    "cereal containers cryptohash_sha256 deepseq directory filepath hslua "
    "hspec linear mtl network process random scientific stm synarchy text "
    "unix unordered_containers vector vulkan yaml zlib".split())

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


def _run_tree(ghc: str, cabal: str | None, files: dict[str, str]
              ) -> list[Violation]:
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        for rel, content in files.items():
            (root / rel).parent.mkdir(parents=True, exist_ok=True)
            (root / rel).write_text(content, encoding="utf-8")
        if cabal is not None:
            (root / CABAL_FILE).write_text(cabal, encoding="utf-8")
        return scan_tree(root, ghc)


def self_test() -> int:
    failures: list[str] = []
    # Compiler-free: the configuration reader and the source map.
    derived = headless_configurations(REPO_ROOT)
    if [(c.label, c.flags) for c in derived] != EXPECTED_HEADLESS_CONFIGURATIONS:
        failures.append(
            f"CONFIG {CABAL_FILE}: derived "
            f"{[(c.label, c.flags) for c in derived]}; reread the headless "
            f"suite's stanzas and update EXPECTED_HEADLESS_CONFIGURATIONS")
    for configuration in derived:
        defined = {m.group(1) for flag in configuration.macros
                   if (m := re.match(r"-optP-DVERSION_(\w+)=", flag))}
        defined |= {"base"} if "base" in configuration.macros else set()
        if defined != EXPECTED_DEPENDENCY_MACROS:
            failures.append(
                f"CONFIG {configuration.label}: VERSION_ macros "
                f"{sorted(defined ^ EXPECTED_DEPENDENCY_MACROS)} differ from "
                f"Cabal's; update EXPECTED_DEPENDENCY_MACROS")
    for label, cabal, needle in CONFIG_ERROR_FIXTURES:
        with tempfile.TemporaryDirectory() as tmp:
            if cabal is not None:
                (Path(tmp) / CABAL_FILE).write_text(cabal, encoding="utf-8")
            try:
                headless_configurations(Path(tmp))
                failures.append(f"CONFIG-ERROR {label}: no AuditError")
            except AuditError as error:
                if needle not in str(error):
                    failures.append(f"CONFIG-ERROR {label}: {error}")
    for label, output, source, origins, cpp_ran in MAP_FIXTURES:
        pre = map_output(output, source)
        if list(pre.origins) != origins or pre.cpp_ran != cpp_ran:
            failures.append(f"MAP {label}: got {pre.origins}, {pre.cpp_ran}")
    # The compiler: its absence and a mismatched version both stop.
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
    # Compiler-backed: every fixture through the real ghc -E.
    ghc = ghc_command(REPO_ROOT)
    with ThreadPoolExecutor(max_workers=min(8, os.cpu_count() or 2)) as pool:
        detected = list(pool.map(lambda f: find_violations(f[1], ghc),
                                 DETECTED_FIXTURES))
        clean = list(pool.map(lambda f: find_violations(f[1], ghc),
                              CLEAN_FIXTURES))
        reasons = list(pool.map(lambda f: find_violations(f[1], ghc),
                                REASON_FIXTURES))
        trees = list(pool.map(
            lambda f: _run_tree(ghc, f[1],
                                {f"{SCOPED_TREE}/Test/M.hs": f[2], **f[4]}),
            TREE_FIXTURES))
    for (label, _, lines), got in zip(DETECTED_FIXTURES, detected):
        if [v.line for v in got] != lines:
            failures.append(f"DETECT {label}: expected lines {lines}, got "
                            f"{[str(v) for v in got]}")
    for (label, _, needles), got in zip(REASON_FIXTURES, reasons):
        if len(got) != 1 or not all(n in got[0].reason for n in needles):
            failures.append(f"REASON {label}: {[str(v) for v in got]}")
    for (label, _), got in zip(CLEAN_FIXTURES, clean):
        if got:
            failures.append(f"CLEAN {label}: unexpected {[str(v) for v in got]}")
    for (label, _, _, lines, _), got in zip(TREE_FIXTURES, trees):
        if [v.line for v in got] != lines:
            failures.append(f"TREE {label}: expected lines {lines}, got "
                            f"{[str(v) for v in got]}")
    reported = {v.path for v in _run_tree(
        ghc, _FIXTURE_CABAL,
        {rel: _BANNED_SOURCE for rel, _ in EXEMPTION_FIXTURES})}
    for rel, expected in EXEMPTION_FIXTURES:
        if (rel in reported) != expected:
            failures.append(f"EXEMPTION {rel}: expected reported={expected}, "
                            f"got {rel in reported}")
    for rel, reason in EXEMPTIONS.items():
        if not reason.strip():
            failures.append(f"EXEMPTION {rel} carries no reason")
    total = (1 + len(CONFIG_ERROR_FIXTURES) + len(MAP_FIXTURES) + 2
             + len(DETECTED_FIXTURES) + len(CLEAN_FIXTURES)
             + len(REASON_FIXTURES)
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
        ghc = ghc_command(REPO_ROOT)
        configurations = headless_configurations(REPO_ROOT)
        violations = scan_tree(REPO_ROOT, ghc)
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
          f"({len(_modules(REPO_ROOT))} modules preprocessed by {ghc} "
          f"under {', '.join(c.label for c in configurations)}).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
