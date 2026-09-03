#!/usr/bin/env python3
"""Guard: every QUALIFIED haddock link `'Module.function'` in a `src/` or
`app/` comment names a function the module actually exports (#2292,
HLR-1 of docs/haddock_link_resolution_design.md).

A haddock link renders as navigable only when the named module exports
the named symbol; otherwise it renders as plain text and sends a reader
to a module that hides the name. Module splits and the deliberate export
narrowing of #1083/#1154/#1156 left many such references behind, and
nothing caught them: `synarchy.cabal` passes `-haddock` to GHC, which
validates comment SYNTAX only -- link TARGETS are resolved by the
`haddock` tool, which no gate runs.

The detection rule
------------------
A qualified link `'M.f'` inside a Haskell comment is DEAD when all of:

* `M` is a module under `src/` or `app/`;
* `M` has an explicit export list (a module without one exports
  everything, so its links always resolve);
* that list neither names `f` nor carries a `module X` re-export or a
  `T(..)` subordinate group that supplies it; and
* `f` is a real top-level definition somewhere under `src/` or `app/` --
  a `name ∷` signature at column 0, or a record field of a `data`/
  `newtype` declaration.

A module linking its OWN unexported name is included: `src/World/Save/
Storage.hs` links `'World.Save.Storage.publishValidated'`, which its own
export list omits, and that is a real dead link.

The last clause is what keeps Lua binding names out. `'UI.setVisible'`
looks like a Haskell reference only because `src/UI.hs` exists;
`setVisible` is a Lua verb with no Haskell definition anywhere, so the
link is not a candidate.

What is deliberately NOT reported
---------------------------------
The SINGLE-QUOTE delimiter is itself the scoping rule D-1 asked for, and
it is the whole mechanism behind four of the exclusions: `@M.f@` code
spans (the accepted spelling for a name that is real but not exported,
established by PR #1407), backtick-quoted `` `M.f` `` prose (which
`src/Unit/Pathing/Cost.hs` writes for the very name
`src/Engine/Asset/YamlMaterials.hs` links), module links `"M.N"`, and
unqualified `'f'` links -- none of them is a qualified `'M.f'`, so none
is ever matched. Nothing masks a code span first: inside `@…@` Haddock
still resolves a quoted identifier, so a `'M.f'` written there is a real
link and belongs in the report, and a mask keyed on a stray second `@`
would instead swallow whatever sits between them.

Also never reported: modules outside this repository, modules without an
explicit export list, names supplied through a re-export, record fields
reached through `Type(..)`, and any text inside a string literal, a
character literal or a quasiquote. `test/`, `test-headless/`, `cbits/`
and Lua are outside the scanned trees entirely. Those are DETECTION
rules, not discretionary exceptions -- the only allowance for a genuine
failure is the generated baseline below.

The baseline ratchet
--------------------
Owner decision D-3 lands the guard before the cleanup, so this tool
ships with a checked-in baseline of every dead link present at its own
commit. A run exits zero only when the live findings match that baseline
exactly: a link not in it fails as NEW, and an entry no longer found
fails as STALE, so the file can only shrink. `--update-baseline`
regenerates it; the file is generated, never hand-edited, exactly like
`docs/save_compat/enum_baseline.json`.

The baseline is TEMPORARY, not a permanent exemption list. HLR-2 through
HLR-4 drain it in bounded comment-only sweeps, and HLR-5 deletes both
the file and `--update-baseline`, after which zero dead links is the
permanent state.

Comment awareness is not this module's own lexer: it reuses
`tools/unicode_operator_audit.py`'s scanner through the
`haskell_comment_spans` it exposes for that purpose, so nested block
comments, string literals and atomically-skipped char literals behave
identically on both sides. Quasiquotes are masked HERE rather than
there, because the shared scanner has no quasiquote state and the
unicode audit handles its one quasiquoting file by a separate,
file-scoped step.

Usage:
    python3 tools/haddock_link_audit.py
    python3 tools/haddock_link_audit.py --update-baseline
"""
from __future__ import annotations

import argparse
import json
import re
import sys
from collections import Counter
from dataclasses import dataclass
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from unicode_operator_audit import (  # type: ignore  # noqa: E402
    haskell_code_only, haskell_comment_spans)

#: The generated ratchet. Deleted by HLR-5 together with
#: `--update-baseline`; see the module docstring.
BASELINE_PATH = REPO_ROOT / "tools" / "haddock_link_baseline.json"

#: The two trees a link may live in AND a definition may come from. The
#: test suites, `cbits/` and Lua are outside both by design.
SCANNED_TREES = ("src", "app")

# A qualified haddock link: `'Data.Some.Module.functionName'`. The module
# part is one or more capitalised components; the symbol is a
# lower-case-initial identifier, so a link to a CONSTRUCTOR (`'M.Ctor'`)
# is not a candidate -- D-1 scopes this arc to functions.
LINK_RE = re.compile(
    r"'([A-Z][A-Za-z0-9_']*(?:\.[A-Z][A-Za-z0-9_']*)*)"
    r"\.([a-z_][A-Za-z0-9_']*)'")

# A quasiquote opener. GHC lexes `[name|` as one with QuasiQuotes on --
# which synarchy.cabal enables globally -- so this matches exactly what
# the compiler treats as quasiquoted, list-comprehension lookalikes
# included.
QUASIQUOTE_OPEN_RE = re.compile(r"\[[a-z_][A-Za-z0-9_']*\|")

MODULE_HEADER_RE = re.compile(
    r"^module\s+([A-Z][A-Za-z0-9_'.]*)", re.MULTILINE)

# A top-level signature, matched against a declaration BLOCK that starts
# at column 0. `\s*` spans newlines, so the continuation-line form
#
#     publishValidated
#         ∷ (FilePath → IO ()) → …
#
# -- which is how src/World/Save/Storage.hs declares the one same-module
# case the initial baseline must contain -- is recognised exactly like
# the same-line form. Both `∷` and `::` are accepted: the tree is
# UnicodeSyntax throughout, so a detector written against ASCII `::`
# alone would find no definitions at all and report a silent clean run.
SIGNATURE_RE = re.compile(
    r"\A([a-z_][A-Za-z0-9_']*(?:\s*,\s*[a-z_][A-Za-z0-9_']*)*)\s*(?:∷|::)",
    re.DOTALL)

DATA_HEADER_RE = re.compile(r"\A(?:data|newtype)\s+([A-Z][A-Za-z0-9_']*)")
RECORD_FIELD_RE = re.compile(
    r"([a-z_][A-Za-z0-9_']*(?:\s*,\s*[a-z_][A-Za-z0-9_']*)*)\s*(?:∷|::)")


@dataclass(frozen=True)
class Finding:
    """One dead link occurrence."""
    path: str
    line: int
    module: str
    symbol: str
    defined_in: tuple[str, ...]

    @property
    def link(self) -> str:
        return f"'{self.module}.{self.symbol}'"

    def __str__(self) -> str:
        # `defined_in` is never empty: a symbol with no definition
        # anywhere is not a candidate in the first place, which is what
        # keeps Lua verbs out. Naming the real owner is what makes the
        # fix mechanical -- eight of the baselined links name a module
        # that never held the function.
        return (f"{self.path}:{self.line}: {self.link} — {self.module} "
                f"does not export {self.symbol} "
                f"(defined in {', '.join(self.defined_in)})")


@dataclass
class ModuleFacts:
    """What one module's source says about its exports and definitions."""
    name: str
    path: str
    has_export_list: bool
    exported_names: set[str]
    #: `T` for each `T(..)`, so its constructors and fields resolve.
    exported_open_types: set[str]
    #: Modules named by a `module X` re-export in the export list.
    reexported_modules: set[str]
    #: Every top-level function this module defines.
    defined: set[str]
    #: `T -> {field, …}` for each record declared here.
    record_fields: dict[str, set[str]]


def mask_spans(text: str, spans: list[tuple[int, int]]) -> str:
    """Blank `[start, end)` spans to NULs, one character for one, so
    every other offset -- and so every line number -- stays valid."""
    out = list(text)
    for start, end in spans:
        for i in range(start, end):
            if out[i] != "\n":
                out[i] = "\x00"
    return "".join(out)


def quasiquote_spans(text: str) -> list[tuple[int, int]]:
    """The `[name| … |]` spans of `text`.

    Two passes, because the opener and the body need different views.
    The OPENER is located only inside a genuine code span, so a comment
    that merely contains `[frag|`-shaped text can never be mistaken for
    a real quasiquote boundary. The CLOSER is then found in the RAW
    text, because a `--` or `{-` inside the quasiquoted body opens a
    phantom comment state in the shared scanner (it has no quasiquote
    state), which would hide the `|]` from a code-span-only search."""
    code_only = haskell_code_only(text)
    spans: list[tuple[int, int]] = []
    search_from = 0
    while True:
        match = QUASIQUOTE_OPEN_RE.search(code_only, search_from)
        if match is None:
            return spans
        close = text.find("|]", match.end())
        if close == -1:
            # An unterminated opener is a list comprehension or a stray
            # bracket, not a quasiquote. Skip past it rather than
            # masking the rest of the file.
            search_from = match.end()
            continue
        spans.append((match.start(), close + 2))
        search_from = close + 2


def comment_text_spans(text: str) -> list[tuple[int, int]]:
    """The comment spans of `text`, with quasiquotes masked first."""
    return haskell_comment_spans(mask_spans(text, quasiquote_spans(text)))


def _sanitize(text: str) -> str:
    """Turn `haskell_code_only`'s comment/string placeholder NULs back
    into whitespace.

    Positions matter while the file is being CUT into export lists and
    declaration blocks, which is why the masker preserves them one for
    one. Once a fragment has been cut, its NULs are just an interleaved
    comment, and a regex looking for whitespace must see whitespace there --
    the export entry `( publishStagedSession\n  -- * heading\n , …` is
    one entry whose name is followed by NULs, not a name that happens to
    end in them."""
    return text.replace("\x00", " ")


def _top_level_blocks(code: str) -> list[str]:
    """Every declaration block of `code` -- a column-0 line plus the
    indented lines that continue it."""
    blocks: list[str] = []
    current: list[str] | None = None
    for line in code.splitlines():
        # A column-0 NUL is a comment starting there, and a comment never
        # starts a declaration -- so it continues the block it sits in
        # rather than cutting a signature in half.
        if line[:1].strip("\x00").strip():
            if current is not None:
                blocks.append("\n".join(current))
            current = [line]
        elif current is not None:
            if not _sanitize(line).strip():
                blocks.append("\n".join(current))
                current = None
            else:
                current.append(line)
    if current is not None:
        blocks.append("\n".join(current))
    return blocks


def _split_names(names: str) -> list[str]:
    return [part.strip() for part in names.split(",") if part.strip()]


def _export_list(code: str) -> tuple[bool, str]:
    """`(has an explicit export list, its text)` for module source
    `code` (comments and strings already blanked)."""
    header = MODULE_HEADER_RE.search(code)
    if header is None:
        return False, ""
    i, n = header.end(), len(code)
    while i < n and code[i] in " \t\n\r\x00":
        i += 1
    if i >= n or code[i] != "(":
        return False, ""
    depth, start = 0, i
    while i < n:
        if code[i] == "(":
            depth += 1
        elif code[i] == ")":
            depth -= 1
            if depth == 0:
                return True, code[start + 1:i]
        i += 1
    return False, ""


def _split_export_entries(body: str) -> list[str]:
    """`body` split on its TOP-LEVEL commas, so `T(..)`'s own commas and
    an operator export's parentheses stay with their entry."""
    entries, depth, current = [], 0, []
    for char in body:
        if char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
        if char == "," and depth == 0:
            entries.append("".join(current))
            current = []
        else:
            current.append(char)
    entries.append("".join(current))
    return [entry.strip() for entry in entries if entry.strip()]


def parse_module(rel_path: str, text: str) -> ModuleFacts | None:
    """Everything the audit needs to know about one Haskell source."""
    code = haskell_code_only(text)
    header = MODULE_HEADER_RE.search(code)
    if header is None:
        return None
    name = header.group(1)

    has_list, body = _export_list(code)
    exported: set[str] = set()
    open_types: set[str] = set()
    reexports: set[str] = set()
    if has_list:
        for entry in _split_export_entries(_sanitize(body)):
            if entry.startswith("module "):
                reexports.add(entry[len("module "):].strip())
                continue
            # `type Foo`, `pattern Bar`, `data Baz(..)`: the namespace
            # keyword is not part of the exported name.
            entry = re.sub(r"\A(?:type|pattern|data)\s+", "", entry)
            subordinates = ""
            paren = entry.find("(")
            if paren != -1 and not entry.startswith("("):
                subordinates = entry[paren + 1:].rstrip().rstrip(")")
                entry = entry[:paren]
            head = entry.strip().strip("()").strip()
            if head:
                exported.add(head)
                # A qualified re-export (`GLFW.setWindowSize`) exports
                # the name under its own last component.
                exported.add(head.rsplit(".", 1)[-1])
            if subordinates.strip() == "..":
                open_types.add(head)
            else:
                for sub in _split_names(subordinates):
                    exported.add(sub.strip("()").strip())

    defined: set[str] = set()
    record_fields: dict[str, set[str]] = {}
    for block in map(_sanitize, _top_level_blocks(code)):
        signature = SIGNATURE_RE.match(block)
        if signature is not None:
            defined.update(_split_names(signature.group(1)))
            continue
        data = DATA_HEADER_RE.match(block)
        if data is not None:
            fields = record_fields.setdefault(data.group(1), set())
            for group in re.findall(r"\{(.*?)\}", block, re.DOTALL):
                for field_match in RECORD_FIELD_RE.finditer(group):
                    fields.update(_split_names(field_match.group(1)))
    return ModuleFacts(name, rel_path, has_list, exported, open_types,
                       reexports, defined, record_fields)


def _haskell_sources(repo_root: Path) -> list[Path]:
    files: list[Path] = []
    for tree in SCANNED_TREES:
        files.extend(sorted((repo_root / tree).glob("**/*.hs")))
    return files


@dataclass(frozen=True)
class Index:
    """The whole tree, as the detection rule needs to see it."""
    #: Module name -> what its source says.
    modules: dict[str, ModuleFacts]
    #: Symbol -> the modules that define it.
    definitions: dict[str, set[str]]
    #: Type name -> its record fields, pooled across the tree. A module
    #: routinely exports `T(..)` for a `T` DECLARED somewhere else
    #: (`Unit.Types` exports `UnitManager(..)`, declared in
    #: `Unit.Types.Manager`), so resolving that group against the
    #: exporting module's own declarations alone reports its fields as
    #: dead when they are exported perfectly well.
    record_fields: dict[str, set[str]]


def build_index(repo_root: Path) -> Index:
    modules: dict[str, ModuleFacts] = {}
    for path in _haskell_sources(repo_root):
        rel = path.relative_to(repo_root).as_posix()
        facts = parse_module(rel, path.read_text(encoding="utf-8"))
        if facts is not None:
            modules[facts.name] = facts
    definitions: dict[str, set[str]] = {}
    record_fields: dict[str, set[str]] = {}
    for facts in modules.values():
        for symbol in facts.defined:
            definitions.setdefault(symbol, set()).add(facts.name)
        for type_name, fields in facts.record_fields.items():
            record_fields.setdefault(type_name, set()).update(fields)
            for field in fields:
                definitions.setdefault(field, set()).add(facts.name)
    return Index(modules, definitions, record_fields)


def exports_symbol(index: Index, module: str, symbol: str,
                   seen: frozenset[str] = frozenset()) -> bool:
    """Does `module` export `symbol`?

    A module outside this tree, or one with no explicit export list, is
    treated as supplying anything asked of it -- unknowable and
    everything-exported respectively both mean the link may resolve, and
    this guard never reports a link it cannot prove dead."""
    if module in seen:
        return False
    modules = index.modules
    facts = modules.get(module)
    if facts is None or not facts.has_export_list:
        return True
    if symbol in facts.exported_names:
        return True
    for type_name in facts.exported_open_types:
        if symbol in index.record_fields.get(type_name, ()):
            return True
    for reexported in facts.reexported_modules:
        if reexported == module:
            # `module M` inside M's OWN export list re-exports every
            # name in scope there -- each local definition, record field
            # and constructor, plus everything imported unqualified. So
            # it exports whatever is asked of it. Engine.Core.State is
            # the tree's canonical example (`module Engine.Core.State,
            # module Engine.Core.Lifecycle`), and every link into it
            # resolves.
            return True
        if exports_symbol(index, reexported, symbol, seen | {module}):
            return True
    return False


def find_findings(rel_path: str, text: str, index: Index) -> list[Finding]:
    """Every dead qualified link in one source file."""
    findings: list[Finding] = []
    for start, end in comment_text_spans(text):
        comment = text[start:end]
        for match in LINK_RE.finditer(comment):
            module, symbol = match.group(1), match.group(2)
            facts = index.modules.get(module)
            if facts is None or not facts.has_export_list:
                continue
            if exports_symbol(index, module, symbol):
                continue
            defining = index.definitions.get(symbol)
            if not defining:
                continue
            line = text.count("\n", 0, start + match.start()) + 1
            findings.append(Finding(rel_path, line, module, symbol,
                                    tuple(sorted(defining))))
    return findings


def scan_tree(repo_root: Path) -> list[Finding]:
    index = build_index(repo_root)
    findings: list[Finding] = []
    for path in _haskell_sources(repo_root):
        rel = path.relative_to(repo_root).as_posix()
        findings.extend(find_findings(
            rel, path.read_text(encoding="utf-8"), index))
    return findings


def baseline_key(finding: Finding) -> tuple[str, str]:
    """What the baseline records: the file and the link, never the line.

    A comment edit that moves a still-dead link must not churn the
    baseline; only fixing the link may."""
    return (finding.path, finding.link)


def load_baseline(path: Path) -> Counter[tuple[str, str]]:
    if not path.exists():
        return Counter()
    entries = json.loads(path.read_text(encoding="utf-8"))["entries"]
    return Counter((entry["path"], entry["link"]) for entry in entries)


def write_baseline(path: Path, findings: list[Finding]) -> None:
    """Regenerate the baseline. Duplicate occurrences are preserved as
    repeated entries, so a file that links the same dead name twice must
    fix both before its count drops."""
    entries = sorted((baseline_key(finding) for finding in findings))
    document = {
        "_comment": (
            "Generated by tools/haddock_link_audit.py --update-baseline; "
            "never hand-edited. A TEMPORARY ratchet over the dead links "
            "present when the audit landed (#2292), not a permanent "
            "exemption list: it may only shrink, and HLR-5 deletes it "
            "along with --update-baseline."),
        "entries": [{"path": path_, "link": link} for path_, link in entries],
    }
    path.write_text(json.dumps(document, indent=2, ensure_ascii=False) + "\n",
                    encoding="utf-8")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--update-baseline", action="store_true",
        help="regenerate the checked-in baseline from the current tree")
    parser.add_argument(
        "--repo-root", type=Path, default=REPO_ROOT,
        help=argparse.SUPPRESS)
    parser.add_argument(
        "--baseline", type=Path, default=None, help=argparse.SUPPRESS)
    args = parser.parse_args(argv)
    baseline_path = args.baseline if args.baseline is not None else BASELINE_PATH

    findings = scan_tree(args.repo_root)
    if args.update_baseline:
        write_baseline(baseline_path, findings)
        print(f"Wrote {len(findings)} baselined dead haddock link(s) to "
              f"{baseline_path}.")
        return 0

    baseline = load_baseline(baseline_path)
    live = Counter(baseline_key(finding) for finding in findings)
    new_keys = live - baseline
    stale_keys = baseline - live

    if new_keys:
        remaining = Counter(new_keys)
        print(f"{sum(new_keys.values())} dead haddock link(s) not in the "
              f"baseline:")
        for finding in findings:
            key = baseline_key(finding)
            if remaining[key] > 0:
                remaining[key] -= 1
                print(f"  {finding}")
    if stale_keys:
        print(f"{sum(stale_keys.values())} baseline entry/entries no longer "
              f"found — shrink the baseline in the same change:")
        for (path_, link), count in sorted(stale_keys.items()):
            print(f"  {path_}: {link}" + (f" (x{count})" if count > 1 else ""))
    if new_keys or stale_keys:
        print("\nFix a dead link by demoting it to a code span (@M.f@) or by "
              "pointing it at an exported entry point; never widen an export "
              "list (docs/haddock_link_resolution_design.md D-2). Then run "
              "`python3 tools/haddock_link_audit.py --update-baseline`.")
        return 1

    print(f"No new dead qualified haddock links "
          f"({sum(baseline.values())} still baselined).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
