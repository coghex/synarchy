"""Lua scanner for tools/physiology_access_audit.py (#2830).

A deliberately small, closed scanner: a tokenizer, a block-structure pass
and a handful of recognised key forms. It is NOT a Lua parser, and the
audit documents (physiology_access_audit.py, "Supported forms and limits")
exactly what it understands. Anything it cannot resolve is reported as an
unresolved dynamic key rather than guessed at.

What it finds, per file or embedded chunk:

  * RAW ACCESS: ``unit.getStat``, ``unit.getStatBase``, ``unit.setStat``
    and the ``get`` of any alias of ``scripts.unit_stats`` (including the
    inline ``require("scripts.unit_stats").get``), each with the key
    argument resolved to the literal registry names it can carry.
  * MUTATING MODE VERBS: any mention of ``unit.setPhysioMode`` or
    ``unit.clearPhysioMode``.
  * GENERIC HELPERS: a function that hands one of its own parameters (or
    an enclosing function's parameter, which is how a closure such as
    ``fracColorFn`` reaches the key) to a raw access. The call sites of
    such a helper that pass a literal registry key are then reported at the
    call site, because that is where the registry key actually enters.
"""
from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterator, Optional

RAW_UNIT_VERBS = {"getStat": "read", "getStatBase": "read", "setStat": "write"}
RAW_STATS_VERBS = {"get": "read"}
STATS_MODULE = "scripts.unit_stats"
MUTATING_MODE_VERBS = ("setPhysioMode", "clearPhysioMode")


# ---------------------------------------------------------------- tokens

@dataclass(frozen=True)
class Tok:
    kind: str    # "name" | "str" | "num" | "sym"
    text: str
    line: int    # 1-based, relative to the scanned text


def _long_bracket(src: str, i: int) -> Optional[tuple[int, int]]:
    """If src[i:] opens a Lua long bracket, return (level, length of the
    opener); else None."""
    if src[i] != "[":
        return None
    j = i + 1
    while j < len(src) and src[j] == "=":
        j += 1
    if j < len(src) and src[j] == "[":
        return j - i - 1, j - i + 1
    return None


def tokenize(src: str) -> list[Tok]:
    toks: list[Tok] = []
    i, n, line = 0, len(src), 1
    while i < n:
        c = src[i]
        if c == "\n":
            line += 1
            i += 1
        elif c in " \t\r":
            i += 1
        elif src.startswith("--", i):
            lb = _long_bracket(src, i + 2)
            if lb:
                level, olen = lb
                close = "]" + "=" * level + "]"
                end = src.find(close, i + 2 + olen)
                end = n if end < 0 else end + len(close)
                line += src.count("\n", i, end)
                i = end
            else:
                end = src.find("\n", i)
                i = n if end < 0 else end
        elif c in "\"'":
            j, buf = i + 1, []
            while j < n and src[j] != c and src[j] != "\n":
                if src[j] == "\\" and j + 1 < n:
                    buf.append(src[j + 1])
                    j += 2
                else:
                    buf.append(src[j])
                    j += 1
            toks.append(Tok("str", "".join(buf), line))
            i = j + 1
        elif c == "[" and _long_bracket(src, i):
            level, olen = _long_bracket(src, i)  # type: ignore[misc]
            close = "]" + "=" * level + "]"
            end = src.find(close, i + olen)
            stop = n if end < 0 else end
            toks.append(Tok("str", src[i + olen:stop], line))
            line += src.count("\n", i, stop + len(close))
            i = n if end < 0 else end + len(close)
        elif c.isalpha() or c == "_":
            j = i + 1
            while j < n and (src[j].isalnum() or src[j] == "_"):
                j += 1
            toks.append(Tok("name", src[i:j], line))
            i = j
        elif c.isdigit():
            j = i + 1
            while j < n and (src[j].isalnum() or src[j] in "._"):
                j += 1
            toks.append(Tok("num", src[i:j], line))
            i = j
        else:
            for sym in ("...", "..", "==", "~=", "<=", ">=", "::", "//"):
                if src.startswith(sym, i):
                    toks.append(Tok("sym", sym, line))
                    i += len(sym)
                    break
            else:
                toks.append(Tok("sym", c, line))
                i += 1
    return toks


# ---------------------------------------------------------------- blocks

@dataclass
class Frame:
    kind: str                      # function | if | loop | do | repeat
    line: int
    name: Optional[str] = None     # functions: the name it is bound to
    params: tuple[str, ...] = ()
    header: tuple[int, int] = (0, 0)   # loops: token range of the header


def _match(toks: list[Tok], i: int) -> int:
    """Index of the bracket closing the one at toks[i]."""
    opener = toks[i].text
    closer = {"(": ")", "[": "]", "{": "}"}[opener]
    depth = 0
    for j in range(i, len(toks)):
        t = toks[j]
        if t.kind == "sym" and t.text == opener:
            depth += 1
        elif t.kind == "sym" and t.text == closer:
            depth -= 1
            if depth == 0:
                return j
    return len(toks) - 1


def split_args(toks: list[Tok], open_idx: int) -> tuple[list[list[Tok]], int]:
    """The depth-0 comma-separated argument token lists of the call whose
    '(' is toks[open_idx], and the index of its ')'."""
    close = _match(toks, open_idx)
    args: list[list[Tok]] = []
    cur: list[Tok] = []
    depth = 0
    for t in toks[open_idx + 1:close]:
        if t.kind == "sym" and t.text in "([{":
            depth += 1
        elif t.kind == "sym" and t.text in ")]}":
            depth -= 1
        if depth == 0 and t.kind == "sym" and t.text == ",":
            args.append(cur)
            cur = []
        else:
            cur.append(t)
    if cur or args:
        args.append(cur)
    return args, close


def _function_head(toks: list[Tok], i: int) -> tuple[Optional[str], tuple[str, ...], bool]:
    """For the `function` token at i: (bound name, params, is_method)."""
    j = i + 1
    name: Optional[str] = None
    is_method = False
    while j < len(toks) and toks[j].kind == "name":
        name = toks[j].text
        if j + 1 < len(toks) and toks[j + 1].text in (".", ":"):
            is_method = toks[j + 1].text == ":"
            j += 2
        else:
            j += 1
            break
    if name is None:
        # `NAME = function (` / `a.NAME = function (` / `NAME = function(`
        k = i - 1
        if k >= 1 and toks[k].text == "=" and toks[k - 1].kind == "name":
            name = toks[k - 1].text
    params: list[str] = []
    if j < len(toks) and toks[j].text == "(":
        close = _match(toks, j)
        params = [t.text for t in toks[j + 1:close] if t.kind == "name"]
    return name, tuple(params), is_method


@dataclass
class Event:
    kind: str            # raw | mode-verb | call
    idx: int             # token index of the verb / callee name
    frames: tuple[Frame, ...]
    verb: str = ""       # raw: "unit.getStat" ...
    access: str = ""     # raw: "read" | "write"
    callee: str = ""     # call: the callee's bare name


def block_events(toks: list[Tok], aliases: set[str]) -> list[Event]:
    """One pass over the tokens, recording each raw access, mode-verb
    mention and call together with the blocks enclosing it."""
    events: list[Event] = []
    stack: list[Frame] = []
    pending_do_owner = 0     # loop headers whose `do` is not a new block
    for i, t in enumerate(toks):
        if t.kind == "name":
            tx = t.text
            if tx == "function":
                name, params, _ = _function_head(toks, i)
                stack.append(Frame("function", t.line, name, params))
            elif tx == "if":
                stack.append(Frame("if", t.line))
            elif tx in ("for", "while"):
                j = i + 1
                depth = 0
                while j < len(toks):
                    u = toks[j]
                    if u.kind == "sym" and u.text in "([{":
                        depth += 1
                    elif u.kind == "sym" and u.text in ")]}":
                        depth -= 1
                    elif depth == 0 and u.kind == "name" and u.text == "do":
                        break
                    j += 1
                stack.append(Frame("loop", t.line, header=(i, j)))
                pending_do_owner += 1
                # skip scanning the header for blocks: nothing opens there
            elif tx == "do":
                if pending_do_owner and stack and stack[-1].kind == "loop" \
                        and i == stack[-1].header[1]:
                    pending_do_owner -= 1
                else:
                    stack.append(Frame("do", t.line))
            elif tx == "repeat":
                stack.append(Frame("repeat", t.line))
            elif tx in ("end", "until"):
                if stack:
                    stack.pop()
            # ---- accesses
            nxt = toks[i + 1] if i + 1 < len(toks) else None
            nn = toks[i + 2] if i + 2 < len(toks) else None
            n3 = toks[i + 3] if i + 3 < len(toks) else None
            if nxt and nxt.text == "." and nn and nn.kind == "name":
                if tx == "unit" and nn.text in RAW_UNIT_VERBS \
                        and n3 and n3.text == "(" and not _after_dot(toks, i):
                    events.append(Event("raw", i + 2, tuple(stack),
                                        f"unit.{nn.text}",
                                        RAW_UNIT_VERBS[nn.text]))
                elif tx in aliases and nn.text in RAW_STATS_VERBS \
                        and n3 and n3.text == "(" and not _after_dot(toks, i):
                    events.append(Event("raw", i + 2, tuple(stack),
                                        f"{tx}.{nn.text}",
                                        RAW_STATS_VERBS[nn.text]))
                elif tx == "unit" and nn.text in MUTATING_MODE_VERBS:
                    events.append(Event("mode-verb", i + 2, tuple(stack),
                                        f"unit.{nn.text}"))
            elif tx == "require" and nxt and nxt.text == "(" and nn \
                    and nn.kind == "str" and nn.text == STATS_MODULE:
                close = _match(toks, i + 1)
                if close + 3 < len(toks) and toks[close + 1].text == "." \
                        and toks[close + 2].text in RAW_STATS_VERBS \
                        and toks[close + 3].text == "(":
                    events.append(Event("raw", close + 2, tuple(stack),
                                        f"require({STATS_MODULE}).{toks[close + 2].text}",
                                        RAW_STATS_VERBS[toks[close + 2].text]))
            if nxt and nxt.text == "(" and tx not in _KEYWORDS:
                events.append(Event("call", i, tuple(stack), callee=tx))
    return events


def _after_dot(toks: list[Tok], i: int) -> bool:
    """True when toks[i] is itself a member (`x.unit.getStat`)."""
    return i > 0 and toks[i - 1].text in (".", ":")


_KEYWORDS = {"if", "while", "for", "function", "return", "and", "or", "not",
             "in", "until", "then", "do", "else", "elseif", "local"}


def stats_aliases(toks: list[Tok]) -> set[str]:
    """Every name bound to ``require("scripts.unit_stats")`` in the text."""
    out: set[str] = set()
    for i in range(len(toks) - 5):
        if toks[i].text == "local" and toks[i + 1].kind == "name" \
                and toks[i + 2].text == "=" and toks[i + 3].text == "require" \
                and toks[i + 4].text == "(" and toks[i + 5].kind == "str" \
                and toks[i + 5].text == STATS_MODULE:
            out.add(toks[i + 1].text)
    return out


# ------------------------------------------------------------ key forms

def local_tables(toks: list[Tok]) -> dict[str, tuple[int, int]]:
    """`local NAME = { ... }` anywhere: NAME -> token range of the body."""
    out: dict[str, tuple[int, int]] = {}
    for i in range(len(toks) - 3):
        if toks[i].text == "local" and toks[i + 1].kind == "name" \
                and toks[i + 2].text == "=" and toks[i + 3].text == "{":
            out[toks[i + 1].text] = (i + 3, _match(toks, i + 3))
    return out


def local_strings(toks: list[Tok]) -> dict[str, str]:
    """`local NAME = "literal"` anywhere."""
    out: dict[str, str] = {}
    for i in range(len(toks) - 3):
        if toks[i].text == "local" and toks[i + 1].kind == "name" \
                and toks[i + 2].text == "=" and toks[i + 3].kind == "str" \
                and (i + 4 >= len(toks) or toks[i + 4].text not in (".", "..")):
            out[toks[i + 1].text] = toks[i + 3].text
    return out


def _table_elements(toks: list[Tok], rng: tuple[int, int]) -> list[str]:
    """String literals that are direct list elements of the constructor."""
    out, depth = [], 0
    for j in range(rng[0], rng[1] + 1):
        t = toks[j]
        if t.kind == "sym" and t.text in "([{":
            depth += 1
        elif t.kind == "sym" and t.text in ")]}":
            depth -= 1
        elif depth == 1 and t.kind == "str" \
                and toks[j - 1].text in ("{", ",") \
                and toks[j + 1].text in ("}", ","):
            out.append(t.text)
    return out


def _table_fields(toks: list[Tok], rng: tuple[int, int], field_name: str) -> list[str]:
    """`field = "lit"` anywhere inside the constructor."""
    out = []
    for j in range(rng[0], rng[1]):
        if toks[j].kind == "name" and toks[j].text == field_name \
                and toks[j + 1].text == "=" and toks[j + 2].kind == "str":
            out.append(toks[j + 2].text)
    return out


@dataclass
class Resolution:
    kind: str                       # literal | helper | unresolved
    keys: tuple[str, ...] = ()
    helper: Optional[Frame] = None
    param_index: int = 0
    text: str = ""


def _loop_binding(toks: list[Tok], frame: Frame, tables, name: str):
    """If `frame` is `for ..., VALUE in ipairs|pairs(EXPR) do` binding
    `name` as VALUE, return EXPR's table range (inline or a local table)."""
    a, b = frame.header
    head = toks[a:b]
    if not head or head[0].text != "for":
        return None
    try:
        in_at = next(k for k, t in enumerate(head) if t.text == "in")
    except StopIteration:
        return None
    vars_ = [t.text for t in head[1:in_at] if t.kind == "name"]
    if not vars_ or vars_[-1] != name:
        return None
    rest = head[in_at + 1:]
    if len(rest) >= 3 and rest[0].text in ("ipairs", "pairs") and rest[1].text == "(":
        inner = rest[2:-1] if rest[-1].text == ")" else rest[2:]
        if inner and inner[0].text == "{":
            return (a + in_at + 1 + 2, a + in_at + 1 + 2 + _match(inner, 0))
        if len(inner) == 1 and inner[0].kind == "name" and inner[0].text in tables:
            return tables[inner[0].text]
    return None


def resolve_key(toks: list[Tok], arg: list[Tok], frames: tuple[Frame, ...],
                tables, strings) -> Resolution:
    """Resolve a key argument's token list. Supported forms: a string
    literal; a loop variable over a literal list; `v.field` for a loop
    variable over a table of records; a file-local string constant; a
    parameter of the enclosing function or of an enclosing closure."""
    text = " ".join(t.text for t in arg)
    if len(arg) == 1 and arg[0].kind == "str":
        return Resolution("literal", (arg[0].text,))
    if len(arg) == 1 and arg[0].kind == "name":
        name = arg[0].text
        for fr in reversed(frames):
            if fr.kind == "loop":
                rng = _loop_binding(toks, fr, tables, name)
                if rng is not None:
                    return Resolution("literal",
                                      tuple(_table_elements(toks, rng)), text=text)
            if fr.kind == "function" and name in fr.params:
                return Resolution("helper", helper=fr,
                                  param_index=fr.params.index(name), text=text)
        if name in strings:
            return Resolution("literal", (strings[name],), text=text)
        return Resolution("unresolved", text=text)
    if len(arg) == 3 and arg[0].kind == "name" and arg[1].text == "." \
            and arg[2].kind == "name":
        var, fld = arg[0].text, arg[2].text
        for fr in reversed(frames):
            if fr.kind == "loop":
                rng = _loop_binding(toks, fr, tables, var)
                if rng is not None:
                    return Resolution("literal",
                                      tuple(_table_fields(toks, rng, fld)),
                                      text=text)
        return Resolution("unresolved", text=text)
    return Resolution("unresolved", text=text)


# ----------------------------------------------------------------- scan

@dataclass
class Access:
    path: str
    line: int
    verb: str            # unit.getStat | stats.get | ... | helper:NAME
    access: str          # read | write
    key: str
    function: str        # nearest named enclosing function, or "<chunk>"
    via: str = "literal"       # literal | loop | helper-call
    fallback: bool = False     # the read is followed by `or <default>`
    scope: str = ""            # embedded chunks: the Python function holding it
    straight: bool = False     # the chunk has no block structure at all
    sig: str = ""              # the chunk's text, whitespace-normalized


@dataclass
class Unresolved:
    path: str
    line: int
    verb: str
    access: str
    text: str
    function: str


@dataclass
class ModeVerb:
    path: str
    line: int
    verb: str


@dataclass
class HelperDef:
    path: str
    line: int
    name: str
    verb: str
    access: str
    param_index: int


@dataclass
class ChunkScan:
    path: str
    accesses: list[Access] = field(default_factory=list)
    unresolved: list[Unresolved] = field(default_factory=list)
    mode_verbs: list[ModeVerb] = field(default_factory=list)
    helpers: list[HelperDef] = field(default_factory=list)
    toks: list[Tok] = field(default_factory=list)
    events: list[Event] = field(default_factory=list)
    scope: str = ""
    straight: bool = False
    sig: str = ""


def _enclosing_name(frames: tuple[Frame, ...]) -> str:
    for fr in reversed(frames):
        if fr.kind == "function" and fr.name:
            return fr.name
    return "<chunk>"


BLOCK_WORDS = {"function", "for", "while", "repeat", "if", "do"}


def scan_chunk(path: str, src: str, registry_keys: frozenset[str],
               line_base: int = 0, scope: str = "") -> ChunkScan:
    """Scan one Lua text. `line_base` is added to token lines (embedded
    chunks report the line of the Python string that holds them, and the
    Python function `scope` that holds it)."""
    toks = tokenize(src)
    straight = not any(t.kind == "name" and t.text in BLOCK_WORDS for t in toks)
    sig = " ".join(src.split())
    scan = ChunkScan(path, toks=toks, scope=scope, straight=straight, sig=sig)
    aliases = stats_aliases(toks)
    events = block_events(toks, aliases)
    scan.events = events
    tables, strings = local_tables(toks), local_strings(toks)
    for ev in events:
        if ev.kind == "mode-verb":
            scan.mode_verbs.append(
                ModeVerb(path, toks[ev.idx].line + line_base, ev.verb))
        elif ev.kind == "raw":
            open_idx = ev.idx + 1
            args, close = split_args(toks, open_idx)
            line = toks[ev.idx].line + line_base
            fn = _enclosing_name(ev.frames)
            if len(args) < 2:
                scan.unresolved.append(Unresolved(path, line, ev.verb, ev.access,
                                                  "<no key argument>", fn))
                continue
            res = resolve_key(toks, args[1], ev.frames, tables, strings)
            fallback = close + 1 < len(toks) and toks[close + 1].text == "or"
            if res.kind == "literal":
                via = "literal" if len(args[1]) == 1 and args[1][0].kind == "str" \
                    else "loop"
                for k in res.keys:
                    scan.accesses.append(Access(path, line, ev.verb, ev.access,
                                                k, fn, via, fallback,
                                                scope, straight, sig))
            elif res.kind == "helper" and res.helper is not None:
                scan.helpers.append(HelperDef(
                    path, res.helper.line + line_base, res.helper.name or "?",
                    ev.verb, ev.access, res.param_index))
            else:
                scan.unresolved.append(Unresolved(path, line, ev.verb, ev.access,
                                                  res.text, fn))
    return scan


def helper_call_sites(scan: ChunkScan, helpers: dict[str, list[HelperDef]],
                      line_base: int = 0) -> Iterator[Access]:
    """Calls in `scan` to a known helper that pass a literal in the
    helper's key parameter, as accesses made THROUGH that helper."""
    toks = scan.toks
    for ev in scan.events:
        if ev.kind != "call" or ev.callee not in helpers:
            continue
        # a definition (`function name(` / `local function name(`), not a call
        k = ev.idx - 1
        while k >= 1 and toks[k].text in (".", ":") and toks[k - 1].kind == "name":
            k -= 2
        if k >= 0 and toks[k].text == "function":
            continue
        args, _ = split_args(toks, ev.idx + 1)
        for h in helpers[ev.callee]:
            if h.param_index < len(args):
                a = args[h.param_index]
                if len(a) == 1 and a[0].kind == "str":
                    yield Access(scan.path, toks[ev.idx].line + line_base,
                                 f"helper:{ev.callee}", h.access, a[0].text,
                                 _enclosing_name(ev.frames), "helper-call",
                                 False, scan.scope, scan.straight, scan.sig)
