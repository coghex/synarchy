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
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SCAN_DIRS = ["data", "scripts", "src", "app", "config"]
EXTS = (".yaml", ".yml", ".lua", ".hs", ".json")
IMG_EXT = (".png", ".jpg", ".jpeg")
PAT = re.compile(r'assets/textures/[A-Za-z0-9_./-]+')

def resolves(ref):
    ap = os.path.join(ROOT, ref)
    if ref.lower().endswith(IMG_EXT):
        return os.path.isfile(ap)
    return os.path.isdir(ap)          # directory / concat-prefix base

def main():
    refs = {}            # normalized ref -> list of "file:line"
    for d in SCAN_DIRS:
        for dp, _, files in os.walk(os.path.join(ROOT, d)):
            for f in files:
                if not f.endswith(EXTS):
                    continue
                fp = os.path.join(dp, f)
                try:
                    lines = open(fp, encoding="utf-8", errors="ignore").read().splitlines()
                except Exception:
                    continue
                for i, line in enumerate(lines, 1):
                    for m in PAT.findall(line):
                        ref = m.rstrip("/")
                        refs.setdefault(ref, []).append(f"{os.path.relpath(fp, ROOT)}:{i}")

    files = {p for p in refs if p.lower().endswith(IMG_EXT)}
    dirs  = {p for p in refs if p not in files}
    missing = {p: refs[p] for p in refs if not resolves(p)}
    print(f"scanned {len(refs)} unique texture references "
          f"({len(files)} file, {len(dirs)} directory/base)")
    if not missing:
        print("OK — all referenced texture paths exist")
        return 0
    print(f"\nMISSING ({len(missing)}):")
    for p in sorted(missing):
        kind = "file" if p.lower().endswith(IMG_EXT) else "dir "
        print(f"  [{kind}] {p}")
        for loc in missing[p][:4]:
            print(f"        <- {loc}")
    return 1

if __name__ == "__main__":
    sys.exit(main())
