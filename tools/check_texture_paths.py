#!/usr/bin/env python3
"""Verify every `assets/textures/...` reference in data/, scripts/, src/, app/,
config/ resolves on disk — covering BOTH file literals (…/foo.png) AND
directory-style base paths (e.g. boxTextures.load("assets/textures/ui/box"),
texDir, addTextureDir(...)). A missed reference after a texture move renders
magenta in-engine rather than erroring, so this is the guard for issue #428.

- A reference ending in an image extension must resolve to a file.
- Any other `assets/textures/...` reference (a directory base, possibly a
  concatenation prefix) must resolve to a directory.

Exit non-zero and list the offenders if anything is missing. Bare-name icon
references (resolved at runtime via the icon index) are not paths and are out
of scope here.

Comment awareness (issue #1705)
-------------------------------
A reference that occurs only inside a comment is documentation, not a runtime
path: `src/Engine/Preview/Discovery.hs`'s Haddock counterexample deliberately
names `assets/textures/iconsEvil/x.png`, which no loader can consume. Raw-line
scanning could not tell prose from code, so this check ran red and was never
wired into a gate.

Each scanned file is therefore lexed just far enough to separate comment
regions from executable content, per language: `--`/`{- -}` for `.hs`,
`--`/`--[[ ]]` (with long-bracket levels) for `.lua`, `#` for `.yaml`/`.yml`,
and nothing for `.json`, which has no comment syntax. Skipping is decided
PER REFERENCE, at the match's own start offset, so executable content on a
line that also carries a trailing comment is still checked, and a comment
introducer inside a string literal starts no comment.

The lexer fails loudly rather than narrowing silently: an unterminated string,
block comment or long bracket, and an unreadable or non-UTF-8 input file, are
each an error naming the offending location (exit 2) — never a quietly smaller
set of scanned references. The same reason drives the reported comment-skip
count: a policy change that starts swallowing executable references moves a
visible number rather than passing in silence.

The lexers recognise comment and string boundaries only. They are deliberately
NOT parsers for their languages (issue #1705 rules that out as
disproportionate); anything they cannot resolve is an error, not a guess.

Usage:
  python3 tools/check_texture_paths.py
  python3 tools/check_texture_paths.py --root <tree>   # for the self-test
Exit codes: 0 = every reference resolves, 1 = a reference is missing,
2 = an input could not be read or lexed.
"""
from __future__ import annotations

import argparse
import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SCAN_DIRS = ["data", "scripts", "src", "app", "config"]
EXTS = (".yaml", ".yml", ".lua", ".hs", ".json")
IMG_EXT = (".png", ".jpg", ".jpeg")
PAT = re.compile(r'assets/textures/[A-Za-z0-9_./-]+')

EXIT_OK = 0
EXIT_MISSING = 1
EXIT_INPUT = 2


class LexError(Exception):
    """An input shape the lexer cannot resolve, located at `line` (1-based)."""

    def __init__(self, line: int, message: str) -> None:
        super().__init__(message)
        self.line = line
        self.message = message


def _masks(lines: list[str]) -> list[bytearray]:
    """One byte per character of each line; 1 marks a comment character."""
    return [bytearray(len(line)) for line in lines]


def _mark(mask: bytearray, start: int, end: int) -> None:
    for k in range(start, end):
        mask[k] = 1


# --------------------------------------------------------------------------
# Haskell
# --------------------------------------------------------------------------

# The symbol characters a `--` run may extend into. `-->` and `<--` are
# operators, not comments, so a dash run is a comment opener only when it is
# maximal and is not adjacent to another symbol character.
HS_SYMBOL = frozenset("!#$%&*+./<=>?@\\^|-~:")
HS_IDENT = frozenset(
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'")
# A char literal is exactly one character or one escape sequence. Anything
# else spelled with apostrophes -- a prime-suffixed identifier, `don't` inside
# a comment, 'quoted' prose -- is not one, and must not open a literal.
HS_CHAR_LIT = re.compile(r"'(?:\\.[0-9a-zA-Z]*|[^'\\])'")


def _hs_line_comment(line: str, i: int) -> bool:
    if not line.startswith("--", i):
        return False
    if i > 0 and line[i - 1] in HS_SYMBOL:
        return False
    j = i
    while j < len(line) and line[j] == "-":
        j += 1
    return j >= len(line) or line[j] not in HS_SYMBOL


def scan_haskell(lines: list[str]) -> list[bytearray]:
    masks = _masks(lines)
    state = "code"
    depth = 0
    opened = 0
    for ln, line in enumerate(lines):
        mask = masks[ln]
        length = len(line)
        i = 0
        while i < length:
            char = line[i]
            if state == "code":
                if char == "{" and line.startswith("{-", i):
                    state, depth, opened = "block", 1, ln + 1
                    _mark(mask, i, i + 2)
                    i += 2
                elif char == "-" and _hs_line_comment(line, i):
                    _mark(mask, i, length)
                    i = length
                elif char == '"':
                    state, opened = "string", ln + 1
                    i += 1
                elif char == "'":
                    if i > 0 and line[i - 1] in HS_IDENT:
                        i += 1
                    else:
                        match = HS_CHAR_LIT.match(line, i)
                        i = match.end() if match else i + 1
                else:
                    i += 1
            elif state == "block":
                if line.startswith("{-", i):
                    depth += 1
                    _mark(mask, i, i + 2)
                    i += 2
                elif line.startswith("-}", i):
                    depth -= 1
                    _mark(mask, i, i + 2)
                    i += 2
                    if depth == 0:
                        state = "code"
                else:
                    mask[i] = 1
                    i += 1
            elif state == "string":
                if char == "\\":
                    # A backslash followed by nothing but whitespace opens a
                    # string gap, which is how this tree spells a multi-line
                    # message literal.
                    if line[i + 1:].strip() == "":
                        state = "gap"
                        i = length
                    else:
                        i += 2
                elif char == '"':
                    state = "code"
                    i += 1
                else:
                    i += 1
            elif state == "gap":
                if char.isspace():
                    i += 1
                elif char == "\\":
                    state = "string"
                    i += 1
                else:
                    raise LexError(
                        ln + 1,
                        "string gap must resume with a backslash before any "
                        "other character")
            else:  # pragma: no cover - defensive
                raise AssertionError(state)
        if state == "string":
            raise LexError(opened, "unterminated string literal")
    if state == "block":
        raise LexError(opened, "unterminated block comment")
    if state == "gap":
        raise LexError(opened, "unterminated string literal")
    return masks


# --------------------------------------------------------------------------
# Lua
# --------------------------------------------------------------------------

LUA_LONG_OPEN = re.compile(r"\[(=*)\[")


def scan_lua(lines: list[str]) -> list[bytearray]:
    masks = _masks(lines)
    state = "code"
    level = 0
    quote = ""
    opened = 0
    for ln, line in enumerate(lines):
        mask = masks[ln]
        length = len(line)
        continued = False
        i = 0
        while i < length:
            char = line[i]
            if state == "code":
                if line.startswith("--", i):
                    match = LUA_LONG_OPEN.match(line, i + 2)
                    if match:
                        state, level, opened = "longcomment", len(match.group(1)), ln + 1
                        _mark(mask, i, match.end())
                        i = match.end()
                    else:
                        _mark(mask, i, length)
                        i = length
                elif char == "[":
                    match = LUA_LONG_OPEN.match(line, i)
                    if match:
                        state, level, opened = "longstring", len(match.group(1)), ln + 1
                        i = match.end()
                    else:
                        i += 1
                elif char in "\"'":
                    state, quote, opened = "string", char, ln + 1
                    i += 1
                else:
                    i += 1
            elif state == "string":
                if char == "\\":
                    if i + 1 >= length:
                        continued = True   # escaped newline continues the string
                        i = length
                    else:
                        i += 2
                elif char == quote:
                    state = "code"
                    i += 1
                else:
                    i += 1
            elif state in ("longstring", "longcomment"):
                closer = "]" + "=" * level + "]"
                at = line.find(closer, i)
                end = length if at < 0 else at + len(closer)
                if state == "longcomment":
                    _mark(mask, i, end)
                if at >= 0:
                    state = "code"
                i = end
            else:  # pragma: no cover - defensive
                raise AssertionError(state)
        if state == "string" and not continued:
            raise LexError(opened, "unterminated string literal")
    if state == "string":
        raise LexError(opened, "unterminated string literal")
    if state == "longstring":
        raise LexError(opened, "unterminated long string")
    if state == "longcomment":
        raise LexError(opened, "unterminated long comment")
    return masks


# --------------------------------------------------------------------------
# YAML
# --------------------------------------------------------------------------

# `key: |`, `key: >-`, `- |+2` … open a literal block whose indented body is
# content, not code: a `#` inside one is a character, never a comment.
YAML_BLOCK_SCALAR = re.compile(r"(?:^|[\s:])[|>][+-]?[0-9]*[+-]?\s*$")


def _yaml_scalar_start(line: str, i: int) -> bool:
    """True when the quote at `i` begins a node rather than sitting inside a
    plain scalar (`don't`, `6" wide`), where YAML treats it as a character.

    A node may carry tag and anchor properties before its scalar
    (`!!str "x"`, `&id "x"`, `key: !tag &id "x"`), each a whole
    whitespace-delimited token, so those are stepped over rather than read as
    plain-scalar text. A token merely CONTAINING `!` or `&` is not one:
    `note: hello !world "x"` really is a plain scalar, and stepping past
    `hello` would open a quoted scalar that is not there.
    """
    j = i - 1
    while True:
        while j >= 0 and line[j] in " \t":
            j -= 1
        if j < 0:
            return True
        if line[j] in ":-,[{?":
            return True
        start = j
        while start >= 0 and line[start] not in " \t":
            start -= 1
        if line[start + 1] not in "!&":
            return False
        j = start          # a tag or anchor property: keep looking behind it


def scan_yaml(lines: list[str]) -> list[bytearray]:
    masks = _masks(lines)
    state = "code"
    opened = 0
    block_parent = 0
    for ln, line in enumerate(lines):
        mask = masks[ln]
        length = len(line)
        if state == "block":
            if not line.strip():
                continue
            if len(line) - len(line.lstrip()) > block_parent:
                continue          # literal body: all content, no comments
            state = "code"        # dedented out of the block; lex this line
        i = 0
        while i < length:
            char = line[i]
            if state == "code":
                if char == "#" and (i == 0 or line[i - 1] in " \t"):
                    _mark(mask, i, length)
                    i = length
                elif char in "\"'" and _yaml_scalar_start(line, i):
                    state = "dq" if char == '"' else "sq"
                    opened = ln + 1
                    i += 1
                else:
                    i += 1
            elif state == "dq":
                if char == "\\":
                    i += 2
                elif char == '"':
                    state = "code"
                    i += 1
                else:
                    i += 1
            elif state == "sq":
                if char == "'":
                    if line.startswith("''", i):
                        i += 2
                    else:
                        state = "code"
                        i += 1
                else:
                    i += 1
            else:  # pragma: no cover - defensive
                raise AssertionError(state)
        # A quoted scalar may legally span lines, so an unclosed one simply
        # continues; only reaching EOF inside one is an error.
        if state == "code":
            code = "".join(c for k, c in enumerate(line) if not mask[k])
            if YAML_BLOCK_SCALAR.search(code.rstrip()):
                state = "block"
                block_parent = len(line) - len(line.lstrip())
    if state in ("dq", "sq"):
        raise LexError(opened, "unterminated quoted scalar")
    return masks


# --------------------------------------------------------------------------
# JSON
# --------------------------------------------------------------------------

def scan_json(lines: list[str]) -> list[bytearray]:
    """JSON has no comment syntax, so nothing is ever skipped here. The pass
    still runs, because an unterminated string is a malformed input this check
    must refuse rather than scan past."""
    masks = _masks(lines)
    state = "code"
    opened = 0
    for ln, line in enumerate(lines):
        length = len(line)
        i = 0
        while i < length:
            char = line[i]
            if state == "code":
                if char == '"':
                    state, opened = "string", ln + 1
                    i += 1
                else:
                    i += 1
            else:
                if char == "\\":
                    i += 2
                elif char == '"':
                    state = "code"
                    i += 1
                else:
                    i += 1
        if state == "string":
            raise LexError(opened, "unterminated string literal")
    return masks


SCANNERS = {
    ".hs": scan_haskell,
    ".lua": scan_lua,
    ".yaml": scan_yaml,
    ".yml": scan_yaml,
    ".json": scan_json,
}


def resolves(ref: str, root: str = ROOT) -> bool:
    ap = os.path.join(root, ref)
    if ref.lower().endswith(IMG_EXT):
        return os.path.isfile(ap)
    return os.path.isdir(ap)          # directory / concat-prefix base


def collect(root: str) -> tuple[dict[str, list[str]], int, list[str]]:
    """Return (ref -> locations, comment-skipped occurrences, input errors)."""
    refs: dict[str, list[str]] = {}
    skipped = 0
    errors: list[str] = []
    for scan_dir in SCAN_DIRS:
        for dirpath, dirnames, filenames in os.walk(os.path.join(root, scan_dir)):
            dirnames.sort()
            for name in sorted(filenames):
                if not name.endswith(EXTS):
                    continue
                path = os.path.join(dirpath, name)
                rel = os.path.relpath(path, root)
                try:
                    with open(path, encoding="utf-8") as handle:
                        lines = handle.read().splitlines()
                except (OSError, UnicodeDecodeError) as exc:
                    errors.append(f"{rel}: unreadable ({exc})")
                    continue
                try:
                    masks = SCANNERS[os.path.splitext(name)[1]](lines)
                except LexError as exc:
                    errors.append(f"{rel}:{exc.line}: {exc.message}")
                    continue
                for index, line in enumerate(lines, 1):
                    mask = masks[index - 1]
                    for match in PAT.finditer(line):
                        if mask[match.start()]:
                            skipped += 1
                            continue
                        ref = match.group(0).rstrip("/")
                        refs.setdefault(ref, []).append(f"{rel}:{index}")
    return refs, skipped, errors


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--root", default=ROOT,
                        help="tree to scan (default: the repository root)")
    args = parser.parse_args(argv)
    root = os.path.abspath(args.root)

    refs, skipped, errors = collect(root)
    if errors:
        print(f"INPUT ERRORS ({len(errors)}):", file=sys.stderr)
        for error in errors:
            print(f"  {error}", file=sys.stderr)
        print("refusing to report on a partial scan", file=sys.stderr)
        return EXIT_INPUT

    files = {p for p in refs if p.lower().endswith(IMG_EXT)}
    dirs = {p for p in refs if p not in files}
    missing = {p: refs[p] for p in refs if not resolves(p, root)}
    print(f"scanned {len(refs)} unique texture references "
          f"({len(files)} file, {len(dirs)} directory/base); "
          f"skipped {skipped} reference(s) inside comments")
    if not missing:
        print("OK — all referenced texture paths exist")
        return EXIT_OK
    print(f"\nMISSING ({len(missing)}):")
    for p in sorted(missing):
        kind = "file" if p.lower().endswith(IMG_EXT) else "dir "
        print(f"  [{kind}] {p}")
        for loc in missing[p][:4]:
            print(f"        <- {loc}")
    return EXIT_MISSING


if __name__ == "__main__":
    sys.exit(main())
