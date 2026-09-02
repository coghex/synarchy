"""Haskell publication discovery for the bare-name icon gate (#1740, split
by #2142 requirement 8).

The ONE owner of the Haskell side: comment and string cleaning, whole
`src/` and `app/` discovery of every file that names the Lua `icon`
field, enumerated literal publication sites, forwarding-allowlist
accounting, and the rejection of any publication site that is neither
enumerated nor allowlisted.

The four-way distinction is load-bearing (requirement 9): a field name
inside a comment is NOT a site, a string literal IS code, an enumerated
site yields a reference, and an allowlisted site forwards an already-
extracted value. `clean_haskell` blanks comments in place so every scan
after it sees code only — never replace it with raw substring matching.

Forwarding-allowlist HITS are recorded in the `allow_hits` dict the audit
owner passes in and validated for staleness there, after BOTH this
extractor and the Lua one have run — this module never judges an entry
stale on its own.

Consumes only the shared leaf (`bare_name_icon_asset_core`).
"""
from __future__ import annotations

import re
from pathlib import Path

from bare_name_icon_asset_core import CheckError, LineMap, Reference, blank_span


#: Haskell's symbol characters. A run of dashes only starts a comment when it
#: is not part of a longer operator (`<--`, `-->`), which is GHC's own rule.
SYMBOL_CHARS = set("!#$%&*+./<=>?@\\^|-~:")


def clean_haskell(text: str, label: str) -> str:
    """Blank Haskell comments; keep string literals verbatim.

    Same contract as `clean_lua`: the result is the same length as the input,
    so a `"icon"` occurrence that survives is one that really is code. Without
    this a Haddock paragraph mentioning the field would read as a reference
    site, which is the mistake #1705 had to correct in the texture-path
    checker.
    """
    out = []
    i, n, line = 0, len(text), 1
    depth = 0
    while i < n:
        ch = text[i]
        if depth:
            if text.startswith("{-", i):
                depth += 1
                out.append("  ")
                i += 2
                continue
            if text.startswith("-}", i):
                depth -= 1
                out.append("  ")
                i += 2
                continue
            out.append("\n" if ch == "\n" else " ")
            line += ch == "\n"
            i += 1
            continue
        if ch == "\n":
            out.append("\n")
            line += 1
            i += 1
            continue
        if text.startswith("{-", i):
            depth = 1
            out.append("  ")
            i += 2
            continue
        if ch == "-" and text.startswith("--", i):
            j = i
            while j < n and text[j] == "-":
                j += 1
            if (text[j:j + 1] not in SYMBOL_CHARS
                    and text[i - 1:i] not in SYMBOL_CHARS):
                end = text.find("\n", i)
                if end < 0:
                    end = n
                out.append(blank_span(text[i:end]))
                i = end
                continue
            out.append(text[i:j])
            i = j
            continue
        if ch == "'":
            # A character literal, so `'"'` cannot open a phantom string. A
            # trailing identifier tick (`x'`) simply fails to match and is
            # emitted as-is.
            match = re.match(r"'(?:\\.|[^\\'])'", text[i:])
            if match:
                out.append(match.group(0))
                i += match.end()
                continue
            out.append(ch)
            i += 1
            continue
        if ch == '"':
            j = i + 1
            while j < n and text[j] != '"':
                if text[j] == "\\":
                    j += 2
                    continue
                if text[j] == "\n":
                    raise CheckError(
                        f"{label}:{line}: unterminated string literal")
                j += 1
            if j >= n:
                raise CheckError(f"{label}:{line}: unterminated string literal")
            out.append(text[i:j + 1])
            i = j + 1
            continue
        out.append(ch)
        i += 1
    if depth:
        raise CheckError(f"{label}: unterminated block comment")
    return "".join(out)


def extract_haskell(root: Path, config: dict, allow_hits: dict) -> list:
    """Bare names the ENGINE publishes into the same Lua icon field.

    Two Haskell literals reach `buildIconIndex` without passing through any
    Lua map: the immunity row's icon, pushed directly, and the default a
    `data/infections/*.yaml` entry gets when it declares no `icon:`. A third
    site forwards an already-extracted YAML value.

    Scope is every `.hs` under the configured roots that CONTAINS the field
    name, so a new publication site anywhere in the tree fails loudly rather
    than joining the index unchecked.
    """
    field = config.get("haskell_field", "icon")
    needle = f'"{field}"'
    candidates = []
    for relative_root in config.get("haskell_roots", []):
        directory = root / relative_root
        if not directory.is_dir():
            continue
        for path in sorted(directory.rglob("*.hs")):
            if needle in path.read_text(encoding="utf-8"):
                candidates.append(path)

    sites = config.get("haskell_sites", [])
    allowlist = config.get("haskell_forwarding_allowlist", [])
    declared = {entry["file"] for entry in sites} | {e["file"] for e in allowlist}
    seen = {str(path.relative_to(root)) for path in candidates}
    for missing in sorted(declared - seen):
        raise CheckError(
            f"{missing}: declared as a Haskell {needle} reference site, but the "
            f"file does not exist or no longer names that field; the extractor "
            f"refuses rather than silently narrowing coverage")

    references = []
    site_hits = {(entry["file"], entry["name"]): 0 for entry in sites}
    for path in candidates:
        label = str(path.relative_to(root))
        cleaned = clean_haskell(path.read_text(encoding="utf-8"), label)
        lines = LineMap(cleaned)
        consumed = []
        for entry in sites:
            if entry["file"] != label:
                continue
            for match in re.finditer(entry["pattern"], cleaned):
                references.append(
                    Reference(match.group("name"), label,
                              lines.line_of(match.start()),
                              f"{label} {entry['name']}"))
                consumed.append((match.start(), match.end()))
                site_hits[(label, entry["name"])] += 1
        for entry in allowlist:
            if entry["file"] != label:
                continue
            key = (entry["file"], entry["pattern"])
            for match in re.finditer(entry["pattern"], cleaned):
                consumed.append((match.start(), match.end()))
                allow_hits[key] = allow_hits.get(key, 0) + 1
        for match in re.finditer(re.escape(needle), cleaned):
            if any(start <= match.start() and match.end() <= end
                   for start, end in consumed):
                continue
            raise CheckError(
                f"{label}:{lines.line_of(match.start())}: a Lua {needle} field "
                f"is published here, but this is neither an enumerated Haskell "
                f"reference site nor a reason-carrying forwarding allowlist "
                f"entry. Every engine-published icon basename reaches the same "
                f"global index; enumerate it or allowlist it.")

    for (file_name, site_name), hits in sorted(site_hits.items()):
        if hits == 0:
            raise CheckError(
                f"{file_name}: Haskell reference site {site_name!r} matched "
                f"nothing; the extractor refuses rather than silently "
                f"narrowing coverage")
    return references
