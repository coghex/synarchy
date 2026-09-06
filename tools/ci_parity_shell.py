#!/usr/bin/env python3
"""Shell lexing and `python3 tools/*.py` invocation discovery (issue #1355).

The leaf of the CI/`make ci` parity audit (`tools/ci_parity_audit.py`).
This module knows what a shell command IS; it knows nothing about which
files are compared, which workflow job is audited, which invocations are
exempt, or what the audit then does with what it finds. Keeping it that
way is deliberate: the lexer is the part whose correctness the whole
audit rests on, and it is easiest to reason about with no policy in it.

What it owns
------------
Balanced delimiter reading, quote and escape handling, command
substitution, shell-comment removal, command-boundary recognition,
interpreter recognition, `tools/*.py` script recognition, and normalized
invocation extraction.

Failing loudly rather than silently
-----------------------------------
The real hazard for an extractor like this is a shape it does not
recognise: a missed invocation reads as parity that is not there. So it
refuses rather than shrugs. A `python` interpreter appearing anywhere
other than as the head of a command (`xargs python3 ...`), a
`tools/*.py` script executed directly instead of through an interpreter,
and an unterminated quote or `$(`/`${` are each an `AuditError` naming
the offending text -- never a quietly smaller comparison.

`AuditError` is defined HERE, once, and imported by every other parity
module that raises it. Two classes of the same name would slip straight
past `tools/ci_parity_audit.py`'s handlers, which turn it into a
one-line diagnostic and exit 1 instead of a traceback.

This module is a library: it has no command line of its own. Run the
audit through `python3 tools/ci_parity_audit.py`.
"""
from __future__ import annotations

import re
import shlex


_INTERPRETER_RE = re.compile(r"python[0-9]*(?:\.[0-9]+)*")
_TOOLS_SCRIPT_RE = re.compile(r"tools/[A-Za-z0-9_.\-/]+\.py")

# Unquoted characters that end one command and begin another (or end it
# outright, in the case of a redirection target).
_COMMAND_BREAKS = set("|&;\n(){}<>")


class AuditError(Exception):
    """A parse the audit refuses to guess its way past."""


def _read_delimited(text: str, start: int, open_ch: str, close_ch: str,
                    where: str) -> tuple[str, int]:
    """Read a balanced `open_ch`/`close_ch` body whose opener is consumed.

    Returns the body and the index just past the matching closer. Quoting
    inside the body is respected so a delimiter in a string does not
    unbalance the scan.
    """
    depth = 1
    i = start
    n = len(text)
    quote: str | None = None
    while i < n:
        ch = text[i]
        if quote == "'":
            if ch == "'":
                quote = None
            i += 1
            continue
        if ch == "\\" and i + 1 < n:
            i += 2
            continue
        if quote == '"':
            if ch == '"':
                quote = None
            i += 1
            continue
        if ch == "'":
            quote = "'"
            i += 1
            continue
        if ch == '"':
            quote = '"'
            i += 1
            continue
        if ch == open_ch:
            depth += 1
        elif ch == close_ch:
            depth -= 1
            if depth == 0:
                return text[start:i], i + 1
        i += 1
    raise AuditError(
        f"{where}: unterminated {open_ch}...{close_ch} starting at offset "
        f"{start - 1}; the audit will not guess where the command ends.")


def split_shell_commands(text: str, where: str) -> list[str]:
    """Split shell text into individual command segments.

    Quote-, comment- and substitution-aware: shell comments are dropped,
    `$(...)` bodies are lifted out and split in their own right (so a
    command hidden inside a substitution is still seen), and `${...}` is
    consumed whole so a `${{ ... }}` workflow expression cannot look like
    a brace group.
    """
    segments: list[str] = []
    current: list[str] = []
    quote: str | None = None
    i = 0
    n = len(text)
    while i < n:
        ch = text[i]
        if quote == "'":
            current.append(ch)
            if ch == "'":
                quote = None
            i += 1
            continue
        if ch == "\\" and i + 1 < n:
            # Kept verbatim (line continuations included): shlex re-reads it.
            current.append(ch)
            current.append(text[i + 1])
            i += 2
            continue
        if ch == "$" and text.startswith("$(", i):
            inner, i = _read_delimited(text, i + 2, "(", ")", where)
            segments.extend(split_shell_commands(inner, where))
            current.append(" ")
            continue
        if ch == "$" and text.startswith("${", i):
            body, i = _read_delimited(text, i + 2, "{", "}", where)
            current.append("${" + body + "}")
            continue
        if quote == '"':
            current.append(ch)
            if ch == '"':
                quote = None
            i += 1
            continue
        if ch == '"':
            quote = '"'
            current.append(ch)
            i += 1
            continue
        if ch == "'":
            quote = "'"
            current.append(ch)
            i += 1
            continue
        if ch == "#" and (not current or current[-1].isspace()):
            newline = text.find("\n", i)
            i = n if newline == -1 else newline
            continue
        if ch in _COMMAND_BREAKS:
            segments.append("".join(current))
            current = []
            i += 1
            continue
        current.append(ch)
        i += 1
    if quote is not None:
        raise AuditError(
            f"{where}: unterminated {quote} quote; the audit will not guess "
            "where the command ends.")
    segments.append("".join(current))
    return segments


def _is_interpreter(token: str) -> bool:
    """True for `python`, `python3`, `python3.12`, and path-qualified forms.

    Path-qualified so `/usr/bin/python3 tools/x.py` is recognised as an
    invocation rather than quietly ignored; it then compares unequal to a
    bare `python3 tools/x.py`, which is the honest answer — the two files
    would be running the check under different interpreters.
    """
    return _INTERPRETER_RE.fullmatch(token.rsplit("/", 1)[-1]) is not None


def _tools_script(tokens: list[str]) -> str | None:
    for token in tokens:
        if _TOOLS_SCRIPT_RE.fullmatch(token):
            return token
    return None


def extract_invocations(text: str, where: str) -> list[str]:
    """Every `python3 tools/*.py ...` command in `text`, normalized.

    Normalization is `shlex` tokenization rejoined by single spaces, so
    quoting differences between the two files compare equal while
    arguments still compare exactly.
    """
    invocations: list[str] = []
    for segment in split_shell_commands(text, where):
        stripped = segment.strip()
        if not stripped:
            continue
        try:
            tokens = shlex.split(stripped)
        except ValueError as error:
            raise AuditError(
                f"{where}: could not tokenize shell command {stripped!r} "
                f"({error}).") from error
        if not tokens:
            continue
        head = tokens[0]
        if _is_interpreter(head):
            if _tools_script(tokens) is None:
                # `python3 -m pip install ...` and friends: environment
                # preparation, not part of the audited gate set.
                continue
            invocations.append(" ".join(tokens))
            continue
        if any(_is_interpreter(token) for token in tokens):
            raise AuditError(
                f"{where}: a Python interpreter appears somewhere other than "
                f"the head of a command, in {stripped!r}. This audit compares "
                "`python3 tools/*.py` commands; rewrite the step as a plain "
                "invocation, or teach this audit the new shape deliberately.")
        if _TOOLS_SCRIPT_RE.fullmatch(head):
            raise AuditError(
                f"{where}: {head} is executed directly rather than through a "
                f"Python interpreter, in {stripped!r}. This audit compares "
                f"`python3 tools/*.py` commands; invoke it as "
                f"`python3 {head}`.")
    return invocations



_ENV_ASSIGNMENT_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_]*=")


def _is_cabal(token: str) -> bool:
    """True for `cabal` and for a path-qualified `.../bin/cabal`.

    Path-qualified so the workflow's pinned `/usr/local/.ghcup/bin/cabal`
    is recognised as the same program rather than quietly ignored. A
    token carrying `=` is a value, not a program: `CABAL_DIR=/usr/local/
    cabal` ends in `cabal` and names no command, so excluding it is what
    keeps the not-at-the-head rule below from firing on an `echo`.
    """
    return "=" not in token and token.rsplit("/", 1)[-1] == "cabal"


def extract_cabal_commands(text: str, where: str) -> list[list[str]]:
    """Every direct `cabal ...` command in `text`, as token lists.

    Leading `VAR=value` assignments are stripped, so
    `SYNARCHY_FULL_TESTS=1 cabal test ...` is the same command as
    `cabal test ...` for the caller's purposes; the tokens returned start
    at `cabal` itself.

    Fail-loud in the same shape as `extract_invocations`: a `cabal` that
    is not the head of its command once assignments are stripped -- `env
    cabal build`, `xargs cabal build` -- is an error naming the offending
    text, never a quietly smaller result. A quoted mention (`step "cabal
    module audit"`) is one token that is not `cabal`, so it is text and
    matches nothing.
    """
    commands: list[list[str]] = []
    for segment in split_shell_commands(text, where):
        stripped = segment.strip()
        if not stripped:
            continue
        try:
            tokens = shlex.split(stripped)
        except ValueError as error:
            raise AuditError(
                f"{where}: could not tokenize shell command {stripped!r} "
                f"({error}).") from error
        while tokens and _ENV_ASSIGNMENT_RE.match(tokens[0]):
            tokens = tokens[1:]
        if not tokens:
            continue
        if _is_cabal(tokens[0]):
            commands.append(tokens)
            continue
        if any(_is_cabal(token) for token in tokens):
            raise AuditError(
                f"{where}: `cabal` appears somewhere other than the head of "
                f"a command, in {stripped!r}. This audit reads the verbosity "
                "of direct Cabal commands; rewrite the step as a plain "
                "invocation, or teach this audit the new shape deliberately.")
    return commands


def cabal_subcommand(tokens: list[str]) -> str | None:
    """A Cabal command's subcommand, or None when it has none.

    The first token after `cabal` that is not an option, so
    `cabal build all -v0` is `build` while `cabal --version` is None.
    """
    for token in tokens[1:]:
        if not token.startswith("-"):
            return token
    return None
