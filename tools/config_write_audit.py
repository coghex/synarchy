#!/usr/bin/env python3
"""Structural guard (#2202): every `config/` write goes through
`Engine.Core.ConfigWrite`, and nothing else performs raw file I/O on a
config path.

Before #2202 five families persisted local configuration by calling
`Data.Yaml.encodeFile`, `Data.ByteString.writeFile` or
`System.Directory.copyFile` straight onto the target. libyaml emits onto
a directly opened file, so an interrupted emit left a TRUNCATED
`*.local.yaml` that the next boot decoded as malformed and the next save
overwrote -- the player's settings lost once, with only a log line.

WHY THIS IS NOT AN `rg` ONE-LINER. The issue's own acceptance command
was `rg 'encodeFile|writeFile' src app | rg 'config/'`, and it returned
NO MATCHES on the defective snapshot: the raw write and the `config/`
literal sit on different lines, and three of the six writers never name
a `config/` path at all (the path is a parameter). A text filter over
single lines cannot see this class of defect, so the guard is structural
instead -- it reasons about MODULES.

Two rules, each with a checked-in constant so a change to the set is a
reviewed edit rather than a silent one:

  1. CONFIG_PERSISTENCE_MODULES -- the modules that persist local
     configuration. None of them may contain a raw write, copy or rename
     call, and every one of them must import `Engine.Core.ConfigWrite`.
     That is what pins requirement 2 ("no `Yaml.encodeFile`,
     `BS.writeFile`, or `writeFile` on a `config/` path remains") for
     exactly the writers whose paths are parameters.

  2. Completeness -- any OTHER Haskell file under `src/` or `app/` that
     names a `config/...` string literal must contain no raw write
     either, unless it is exempt. That is what catches a NEW module that
     starts writing config directly instead of joining rule 1.

Plus one positive check, so the helper cannot be hollowed out while both
negative rules stay green: `Engine.Core.ConfigWrite` must itself name
the durable primitives it is built from (`writeBytesDurably`,
`syncDirectory`, `claimUniquePath`) and the publishing `renameFile`.

WHAT COUNTS AS A RAW WRITE: a maximal identifier LEXEME equal to one of
RAW_WRITE_NAMES, optionally preceded by a module qualifier
(`BS.writeFile`, `Yaml.encodeFile`, `Data.ByteString.writeFile`). The
qualifier is not resolved: inside the scoped module sets any function of
these names IS the concern, and a same-named local binding there would
be indistinguishable to a reader too. Comment and string-literal
awareness comes from `tools/unicode_operator_audit.py`'s lexer via its
public `haskell_code_only`, so a haddock that merely NAMES `encodeFile`
(this repository's do) is never a hit -- which is also why the module
under rule 1 can go on documenting what it replaced.

EXEMPT_RAW_IO names the files that legitimately perform raw file I/O and
would otherwise trip rule 2 or the helper's own rule 1 exclusion; each
carries its reason.

Usage:
  python3 tools/config_write_audit.py              # audit the tree
  python3 tools/config_write_audit.py --self-test  # fixtures only
Exit codes: 0 = clean, 1 = a violation (or, under --self-test, a fixture
behaved wrongly).
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from unicode_operator_audit import haskell_code_only  # noqa: E402

REPO_ROOT = Path(__file__).resolve().parent.parent

# The modules that persist local configuration. Each must route its
# writes through the helper and must contain no raw write of its own.
CONFIG_PERSISTENCE_MODULES = {
    "src/Engine/Asset/YamlNotifications.hs",
    "src/Engine/Core/Init.hs",
    "src/Engine/Graphics/Config.hs",
    "src/Engine/Input/Bindings.hs",
    "src/Engine/Save/Config.hs",
}

# The one module allowed to perform the raw write, because it IS the
# helper.
HELPER_MODULE = "src/Engine/Core/ConfigWrite.hs"

# The primitives the helper is built from. Losing any of them would mean
# the "atomic replace" it promises is no longer what it does.
HELPER_REQUIRED_NAMES = (
    "writeBytesDurably",
    "syncDirectory",
    "claimUniquePath",
    "renameFile",
    "removeFile",
)

IMPORTED_HELPER = "Engine.Core.ConfigWrite"

# Removals are here too (#2202 review round 1): a config family whose
# "no overrides left" state is the ABSENCE of the file publishes that
# state by unlinking, which is a directory-entry change exactly like the
# publish rename and needs the same directory sync before it is reported
# as saved.
RAW_WRITE_NAMES = (
    "encodeFile",
    "encodeFileWith",
    "writeFile",
    "copyFile",
    "copyFileWithMetadata",
    "renameFile",
    "removeFile",
    "removePathForcibly",
    "removeDirectoryRecursive",
)

# Files that legitimately perform raw file I/O and are not config
# writers. Rule 2 would otherwise flag one only if it also named a
# `config/` literal, but listing them keeps the reason on the record.
EXEMPT_RAW_IO = {
    "src/Engine/Scripting/Lua/API/Screenshot.hs":
        "screenshots are not configuration (#2202 out of scope); the "
        "writer is a whole-file PNG dump with no read-back contract",
    "src/World/Save/Storage.hs":
        "the save transaction's own durable publication, whose rotation "
        "and envelope a config file has no counterpart for",
    "src/World/Save/Storage/Durable.hs":
        "the shared durable primitives this guard's helper is built from",
    "src/World/ZoomMap/Artifact.hs":
        "the zoom-map artifact store's own publish, outside config/",
    "src/World/GeneratedLibrary/Registry.hs":
        "the generated-world library's registry publish, outside config/",
}

_QUALIFIED = r"(?:[A-Z][A-Za-z0-9_']*\.)*"
RAW_WRITE_RE = re.compile(
    r"(?<![A-Za-z0-9_'.])" + _QUALIFIED
    + r"(?:" + "|".join(RAW_WRITE_NAMES) + r")(?![A-Za-z0-9_'])"
)
CONFIG_LITERAL_RE = re.compile(r'"config/')
IMPORT_HELPER_RE = re.compile(
    r"^\s*import\s+(?:qualified\s+)?" + re.escape(IMPORTED_HELPER)
    + r"(?![A-Za-z0-9_'.])", re.MULTILINE)


def raw_write_hits(source: str) -> list[tuple[int, str]]:
    """Every raw write/copy/rename lexeme in genuine code, as
    `(line, text)`."""
    code = haskell_code_only(source)
    hits = []
    for match in RAW_WRITE_RE.finditer(code):
        line = code.count("\n", 0, match.start()) + 1
        hits.append((line, match.group(0)))
    return hits


def strip_import_declarations(code: str) -> str:
    """`code` with every import declaration blanked, line count preserved.

    The helper's positive check is that its BODY still calls the durable
    primitives. Without this, deleting the `syncDirectory` call while
    leaving the import list alone would keep the check green -- and an
    unused import is a `-Werror=unused-imports` failure, so the two
    edits always travel together in real code and the audit must see
    past the import."""
    out = []
    in_import = False
    for line in code.split("\n"):
        if re.match(r"^import(?![A-Za-z0-9_'])", line):
            in_import = True
        elif in_import and not (line[:1].isspace() and line.strip()):
            in_import = False
        out.append("" if in_import else line)
    return "\n".join(out)


def names_config_literal(source: str) -> bool:
    """Does this file name a `config/...` path? Read from the RAW source
    on purpose: the literal lives inside a string, which the code-only
    lexer blanks."""
    return CONFIG_LITERAL_RE.search(source) is not None


def audit_tree(root: Path) -> list[str]:
    problems: list[str] = []
    seen_persistence: set[str] = set()

    helper_path = root / HELPER_MODULE
    if not helper_path.is_file():
        return [f"{HELPER_MODULE} is missing: every config/ write is "
                f"supposed to go through it."]
    helper_source = helper_path.read_text(encoding="utf-8")
    helper_code = strip_import_declarations(haskell_code_only(helper_source))
    for name in HELPER_REQUIRED_NAMES:
        if not re.search(r"(?<![A-Za-z0-9_'])" + name + r"(?![A-Za-z0-9_'])",
                         helper_code):
            problems.append(
                f"{HELPER_MODULE}: no call to '{name}' outside its import "
                f"list. The helper's "
                f"whole contract is write-to-temporary, fsync, atomic "
                f"rename, fsync the directory; losing that primitive "
                f"means it no longer performs an atomic replace.")

    for path in sorted(root.glob("src/**/*.hs")) + sorted(
            root.glob("app/**/*.hs")):
        rel = path.relative_to(root).as_posix()
        if rel == HELPER_MODULE:
            continue
        source = path.read_text(encoding="utf-8")
        hits = raw_write_hits(source)

        if rel in CONFIG_PERSISTENCE_MODULES:
            seen_persistence.add(rel)
            for line, text in hits:
                problems.append(
                    f"{rel}:{line}: raw '{text}' in a config-persistence "
                    f"module. Route the write through "
                    f"{IMPORTED_HELPER}, so an interrupted write cannot "
                    f"leave a truncated config file behind.")
            if not IMPORT_HELPER_RE.search(haskell_code_only(source)):
                problems.append(
                    f"{rel}: listed in CONFIG_PERSISTENCE_MODULES but does "
                    f"not import {IMPORTED_HELPER}. Either it no longer "
                    f"persists configuration (remove it from the constant, "
                    f"with the reason) or its write has escaped the "
                    f"helper.")
            continue

        if rel in EXEMPT_RAW_IO:
            continue

        if hits and names_config_literal(source):
            line, text = hits[0]
            problems.append(
                f"{rel}:{line}: raw '{text}' in a file that names a "
                f"'config/' path. A new config writer joins "
                f"CONFIG_PERSISTENCE_MODULES and routes its write through "
                f"{IMPORTED_HELPER}; a file that is not a config writer "
                f"belongs in EXEMPT_RAW_IO with its reason.")

    for missing in sorted(CONFIG_PERSISTENCE_MODULES - seen_persistence):
        problems.append(
            f"{missing}: named in CONFIG_PERSISTENCE_MODULES but not found "
            f"under src/. Update the constant to match the tree.")

    return problems


# --- Self-test -------------------------------------------------------

HELPER_FIXTURE = """\
module Engine.Core.ConfigWrite (writeConfigBytes) where
import World.Save.Storage.Durable
  (claimUniquePath, syncDirectory, writeBytesDurably)
import System.Directory (removeFile, renameFile)
writeConfigBytes path bytes = do
    tmp <- claimUniquePath "d" "t"
    _ <- writeBytesDurably tmp bytes
    renameFile tmp path
    syncDirectory "d"
removeConfigFile path = do
    removeFile path
    syncDirectory "d"
"""

CLEAN_WRITER = """\
module Engine.Graphics.Config (saveVideoConfig) where
import Engine.Core.ConfigWrite (writeConfigYaml)
-- Historical note: this used to call Yaml.encodeFile directly.
saveVideoConfig path cfg = writeConfigYaml path cfg
"""


def _plant(root: Path, rel: str, source: str) -> None:
    path = root / rel
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(source, encoding="utf-8")


def _clean_tree(root: Path) -> None:
    _plant(root, HELPER_MODULE, HELPER_FIXTURE)
    _plant(root, "src/Engine/Graphics/Config.hs", CLEAN_WRITER)
    for rel in sorted(CONFIG_PERSISTENCE_MODULES
                      - {"src/Engine/Graphics/Config.hs"}):
        _plant(root, rel,
               "module M where\nimport Engine.Core.ConfigWrite (writeConfigYaml)\n"
               "f = writeConfigYaml\n")


def self_test() -> int:
    import tempfile

    failures: list[str] = []

    def check(label: str, mutate, expect_clean: bool, needle: str = "") -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            _clean_tree(root)
            mutate(root)
            problems = audit_tree(root)
            if expect_clean and problems:
                failures.append(f"{label}: expected clean, got {problems}")
            elif not expect_clean:
                if not problems:
                    failures.append(f"{label}: expected a violation, got none")
                elif needle and not any(needle in p for p in problems):
                    failures.append(
                        f"{label}: expected a problem mentioning {needle!r}, "
                        f"got {problems}")

    check("a compliant tree", lambda root: None, expect_clean=True)

    check("a haddock that merely NAMES encodeFile",
          lambda root: _plant(root, "src/Engine/Input/Bindings.hs",
                              "module M where\n"
                              "import Engine.Core.ConfigWrite (writeConfigYaml)\n"
                              "-- | Replaces the old Yaml.encodeFile call.\n"
                              "f = writeConfigYaml\n"),
          expect_clean=True)

    check("a raw encodeFile in a config-persistence module",
          lambda root: _plant(root, "src/Engine/Input/Bindings.hs",
                              "module M where\n"
                              "import Engine.Core.ConfigWrite (writeConfigYaml)\n"
                              "f path x = Yaml.encodeFile path x\n"),
          expect_clean=False, needle="encodeFile")

    check("a raw BS.writeFile in a config-persistence module",
          lambda root: _plant(root, "src/Engine/Core/Init.hs",
                              "module M where\n"
                              "import Engine.Core.ConfigWrite (writeConfigBytes)\n"
                              "f p b = BS.writeFile p b\n"),
          expect_clean=False, needle="writeFile")

    check("a raw removeFile in a config-persistence module",
          lambda root: _plant(root, "src/Engine/Save/Config.hs",
                              "module M where\n"
                              "import Engine.Core.ConfigWrite (removeConfigFile)\n"
                              "f p = removeFile p\n"),
          expect_clean=False, needle="removeFile")

    check("a helper that unlinks without syncing", lambda root: _plant(
              root, HELPER_MODULE,
              HELPER_FIXTURE.replace("import System.Directory (removeFile, renameFile)\n",
                                     "import System.Directory (renameFile)\n")
                            .replace("removeConfigFile path = do\n"
                                     "    removeFile path\n"
                                     "    syncDirectory \"d\"\n", "")),
          expect_clean=False, needle="removeFile")

    check("a raw copyFile in a config-persistence module",
          lambda root: _plant(root, "src/Engine/Core/Init.hs",
                              "module M where\n"
                              "import Engine.Core.ConfigWrite (copyConfigFile)\n"
                              "f a b = copyFile a b\n"),
          expect_clean=False, needle="copyFile")

    check("a config-persistence module that stopped importing the helper",
          lambda root: _plant(root, "src/Engine/Save/Config.hs",
                              "module M where\nf = id\n"),
          expect_clean=False, needle="does not import")

    check("a NEW module writing a config/ path directly",
          lambda root: _plant(root, "src/Engine/Other.hs",
                              'module M where\n'
                              'f = writeFile "config/other.local.yaml" ""\n'),
          expect_clean=False, needle="names a 'config/' path")

    check("a new module naming config/ but performing no raw write",
          lambda root: _plant(root, "src/Engine/Other.hs",
                              'module M where\np = "config/other.local.yaml"\n'),
          expect_clean=True)

    check("an exempt raw-I/O file that names config/",
          lambda root: _plant(root,
                              "src/Engine/Scripting/Lua/API/Screenshot.hs",
                              'module M where\n'
                              'f = BSL.writeFile "config/x" ""\n'),
          expect_clean=True)

    check("a helper that no longer syncs the directory",
          lambda root: _plant(root, HELPER_MODULE,
                              HELPER_FIXTURE.replace(
                                  '    syncDirectory "d"\n', "")),
          expect_clean=False, needle="syncDirectory")

    check("a helper that no longer renames",
          lambda root: _plant(root, HELPER_MODULE,
                              HELPER_FIXTURE
                              .replace("import System.Directory (renameFile)\n", "")
                              .replace("    renameFile tmp path\n", "")),
          expect_clean=False, needle="renameFile")

    check("a deleted helper",
          lambda root: (root / HELPER_MODULE).unlink(),
          expect_clean=False, needle="is missing")

    check("a config-persistence module that vanished from the tree",
          lambda root: (root / "src/Engine/Save/Config.hs").unlink(),
          expect_clean=False, needle="not found under src/")

    for failure in failures:
        print(f"SELF-TEST FAILURE: {failure}")
    if failures:
        return 1
    print("config_write_audit self-test: all fixtures behaved as expected")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--self-test", action="store_true",
                        help="run the fixture suite instead of the tree")
    args = parser.parse_args()
    if args.self_test:
        return self_test()

    problems = audit_tree(REPO_ROOT)
    if problems:
        print("config write audit FAILED:")
        for problem in problems:
            print(f"  {problem}")
        return 1
    print(f"config write audit: {len(CONFIG_PERSISTENCE_MODULES)} config "
          f"writers all route through {IMPORTED_HELPER}; no raw config write "
          f"remains in src/ or app/")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
