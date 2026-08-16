"""Reporting helpers for synarchy tool output.

Provides a tiny, dependency-free renderer used to turn structured probe
results into readable console bullet lists.
"""


def render_bullets(rows):
    """Render an iterable of (label, value) pairs as indented bullet lines."""
    lines = []
    for label, value in rows:
        lines.append(f"  - {label}: {value}")
    return "\n".join(lines)


def summarize(status, detail=""):
    """Return a one-line summary with a leading marker for the status."""
    marker = "OK" if status else "FAIL"
    return f"[{marker}] {detail}" if detail else f"[{marker}]"


if __name__ == "__main__":
    out = render_bullets([("name", "probe"), ("score", 42)])
    assert "name: probe" in out
    assert summarize(True) == "[OK]"
    assert summarize(False, "boom") == "[FAIL] boom"
    print("format_report self-test passed")
