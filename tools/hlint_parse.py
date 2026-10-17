#!/usr/bin/env python3
"""CI's initial HLint contract: parse errors fail; style hints do not.

Inspect JSON because --only matches complete hint names, whereas parse-error
names include the diagnostic. Tool/configuration failures also fail closed.
"""
import json
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]


def check_result(result, profile):
    if result.returncode not in (0, 1):
        print(f"HLint {profile}: tool failed ({result.returncode})", file=sys.stderr)
        print(result.stderr or result.stdout, file=sys.stderr)
        return False
    try:
        hints = json.loads(result.stdout)
        if not isinstance(hints, list) or any(
                not isinstance(h, dict) or not isinstance(h.get("hint"), str)
                or h.get("severity") not in ("Ignore", "Suggestion", "Warning", "Error")
                for h in hints):
            raise ValueError("unexpected HLint JSON schema")
        if result.returncode == 1 and not hints:
            raise ValueError("HLint failed without diagnostic hints")
    except (ValueError, TypeError) as exc:
        print(f"HLint {profile}: invalid result: {exc}", file=sys.stderr)
        print(result.stderr or result.stdout, file=sys.stderr)
        return False
    # Preserve preprocessing warnings rather than silently losing stderr.
    if result.stderr:
        print(result.stderr, file=sys.stderr, end="")
    errors = [h for h in hints if h["hint"].startswith("Parse error")]
    for hint in errors:
        print(f"{hint.get('file', '?')}:{hint.get('startLine', '?')}: "
              f"{hint['hint']}\n{hint.get('from', '')}", file=sys.stderr)
    print(f"HLint {profile}: {len(errors)} parse errors; "
          f"{len(hints) - len(errors)} advisory hints (not enforced)")
    return not errors


def main():
    ok = True
    for profile, flags in (("production", []),
                           ("development", ["--cpp-define=DEVELOPMENT"])):
        result = subprocess.run(
            ["bash", str(ROOT / "tools/hlint.sh"), "--json", *flags],
            cwd=ROOT, capture_output=True, text=True, check=False)
        ok = check_result(result, profile) and ok
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
