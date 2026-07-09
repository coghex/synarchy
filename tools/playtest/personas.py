"""Persona interface for the playtest harness (#647) — the contract C2
(#649) builds against.

A persona is a small structured blob handed to H1:

    name:        str  — short identifier (also the trace's label)
    temperament: str  — one/two sentences of who this player is
    goal:        str  — what they're trying to accomplish this session
    tendencies:  [str] — behavioral leanings the player role-plays

Optional: prose (str) — extra freeform flavor appended to the prompt.

Files are YAML (or JSON — a JSON file is valid YAML) in this package's
personas/ directory; C2 later generates conforming files elsewhere and
passes them by path. H1 ships three hardcoded placeholders so it runs
standalone before C2 lands.
"""
from __future__ import annotations

import json
import os

PERSONA_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "personas")
REQUIRED_FIELDS = ("name", "temperament", "goal", "tendencies")


def _load_file(path: str) -> dict:
    with open(path, encoding="utf-8") as f:
        text = f.read()
    try:
        import yaml  # PyYAML — present in the dev env; JSON fallback below
        data = yaml.safe_load(text)
    except ImportError:
        data = json.loads(text)
    if not isinstance(data, dict):
        raise ValueError(f"{path}: persona must be a mapping")
    missing = [k for k in REQUIRED_FIELDS if not data.get(k)]
    if missing:
        raise ValueError(f"{path}: persona missing fields: {', '.join(missing)}")
    if not isinstance(data["tendencies"], list):
        raise ValueError(f"{path}: tendencies must be a list of strings")
    return data


def list_personas() -> list[str]:
    names = []
    for fn in sorted(os.listdir(PERSONA_DIR)):
        if fn.endswith((".yaml", ".yml", ".json")):
            names.append(os.path.splitext(fn)[0])
    return names


def load_persona(name_or_path: str) -> dict:
    """Load by bundled name ('curious_carl') or by file path (the C2
    hand-off shape)."""
    if os.path.sep in name_or_path or os.path.isfile(name_or_path):
        return _load_file(name_or_path)
    for ext in (".yaml", ".yml", ".json"):
        candidate = os.path.join(PERSONA_DIR, name_or_path + ext)
        if os.path.isfile(candidate):
            return _load_file(candidate)
    raise FileNotFoundError(
        f"no persona named {name_or_path!r}; bundled: {', '.join(list_personas())}")
