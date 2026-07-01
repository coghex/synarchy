#!/usr/bin/env python3
"""Verify every `assets/textures/...` path literal referenced in data/, scripts/,
and src/ resolves to a real file. A missed reference after a texture move renders
magenta in-engine rather than erroring, so this guards the texture-path contract.

Exit non-zero (and list the offenders) if any referenced path is missing.
Bare-name icon references (resolved at runtime via the icon index) are not full
paths and are intentionally out of scope here.
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SCAN_DIRS = ["data", "scripts", "src", "app", "config"]
EXTS = (".yaml", ".yml", ".lua", ".hs", ".json")
PAT = re.compile(r'assets/textures/[A-Za-z0-9_./-]+\.(?:png|jpg|jpeg)')

def main():
    refs = {}            # path -> list of "file:line"
    for d in SCAN_DIRS:
        base = os.path.join(ROOT, d)
        for dp, _, files in os.walk(base):
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
                        refs.setdefault(m, []).append(f"{os.path.relpath(fp, ROOT)}:{i}")

    missing = {p: locs for p, locs in refs.items()
               if not os.path.isfile(os.path.join(ROOT, p))}
    print(f"scanned {len(refs)} unique texture-path references")
    if not missing:
        print("OK — all referenced texture paths exist")
        return 0
    print(f"\nMISSING ({len(missing)}):")
    for p in sorted(missing):
        print(f"  {p}")
        for loc in missing[p][:4]:
            print(f"      <- {loc}")
    return 1

if __name__ == "__main__":
    sys.exit(main())
