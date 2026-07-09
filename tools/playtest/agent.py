"""Player agents for the playtest harness (#647).

PlayerAgent is the naive LLM player. THE CARDINAL RULE lives here: its
inputs are persona + goal + manual + the current screenshot + its own
rolling memory of recent turns — and nothing else. No oracle data
(widget dumps, event logs, engine state) ever enters prompt assembly;
the constructor doesn't even accept a parameter it could arrive
through. The prompt casts it as a confused new player narrating their
experience and taking notes — explicitly not a bug-hunter.

ScriptedAgent is a deterministic no-LLM stand-in for --smoke and
--selftest runs (loop/trace/replay plumbing without an API key).
"""
from __future__ import annotations

import base64
import json
import re

from engine import ACTION_KINDS

DEFAULT_MODEL = "claude-sonnet-5"  # cheap + naive is the point (#641 design call)
DEFAULT_EFFORT = "low"
DEFAULT_MAX_TOKENS = 1024

# Structured-output schema for the per-turn decision. The action mirrors
# the harness vocabulary (translate_action in engine.py).
TURN_SCHEMA = {
    "type": "object",
    "properties": {
        "observation": {"type": "string",
                        "description": "What you think you are looking at."},
        "action": {
            "type": "object",
            "properties": {
                "do": {"type": "string", "enum": list(ACTION_KINDS)},
                "x": {"type": "number"},
                "y": {"type": "number"},
                "x1": {"type": "number"},
                "y1": {"type": "number"},
                "x2": {"type": "number"},
                "y2": {"type": "number"},
                "dx": {"type": "number"},
                "dy": {"type": "number"},
                "button": {"type": "string"},
                "mods": {"type": "array", "items": {"type": "string"}},
                "name": {"type": "string"},
                "text": {"type": "string"},
                "reason": {"type": "string"},
            },
            "required": ["do"],
            "additionalProperties": False,
        },
        "expectation": {"type": "string",
                        "description": "What you expect this action to do."},
        "note": {"type": "string",
                 "description": "Confusion, friction, hesitation, or surprise."},
    },
    "required": ["observation", "action", "expectation", "note"],
    "additionalProperties": False,
}

SYSTEM_TEMPLATE = """\
You are role-playing a NEW PLAYER trying a video game for the first \
time, narrating your experience out loud and taking notes as you go. \
You are not a tester, reviewer, or bug-hunter — you are just a person \
playing a game, and your notes capture whatever you genuinely feel: \
confusion, hesitation, surprise, small victories. Report every \
trip-up honestly; never speculate about code or intent.

WHO YOU ARE
Name: {name}
Temperament: {temperament}
Tendencies:
{tendencies}
{prose}
YOUR GOAL THIS SESSION
{goal}

THE ONLY GUIDE YOU WERE GIVEN (a one-page quickstart)
---
{manual}
---

HOW YOU PLAY
Each turn you see one screenshot of the game ({width}x{height} \
pixels; x grows right, y grows down; the game is PAUSED while you \
look). You choose exactly ONE action, in screenshot pixel \
coordinates — aim by eye at what you see; there are no coordinate \
aids, so being slightly off is normal:
- click: {{"do":"click","x":N,"y":N}} (optional "button":"left|right|middle", "mods":["shift"])
- drag: {{"do":"drag","x1":N,"y1":N,"x2":N,"y2":N}}
- scroll: {{"do":"scroll","dy":N}} (negative = away/up; optional "x","y" to aim first)
- key: {{"do":"key","name":"Space"}} (names like the manual uses: W A S D, Q, E, L, Enter, Escape, Backspace, Tab, Up/Down/Left/Right, Home)
- hold: {{"do":"hold","name":"W"}} (hold a key for a moment, e.g. to pan)
- type: {{"do":"type","text":"..."}} (types into a focused text box)
- wait: {{"do":"wait"}} (just watch time pass)
- done: {{"do":"done","reason":"..."}} (ONLY if you believe you reached your goal)

After your action the game runs for a moment, then you get the next \
screenshot. Respond with a single JSON object: {{"observation": ..., \
"action": ..., "expectation": ..., "note": ...}}. "expectation" is \
what you believe the action will do; "note" is your honest running \
commentary as this player.
"""


def build_system_prompt(persona: dict, manual: str, fb_size) -> str:
    """The complete player context. Everything the player will ever
    know comes through here + the screenshot + its own memory —
    keeping this function free of oracle inputs IS the naive-purity
    guarantee, so keep its signature that way."""
    tendencies = "\n".join(f"- {t}" for t in persona.get("tendencies", []))
    prose = persona.get("prose") or ""
    if prose:
        prose = prose.strip() + "\n"
    return SYSTEM_TEMPLATE.format(
        name=persona["name"], temperament=persona["temperament"].strip(),
        tendencies=tendencies, prose=prose, goal=persona["goal"].strip(),
        manual=manual.strip(), width=fb_size[0], height=fb_size[1])


def _lenient_parse(text: str) -> dict:
    try:
        return json.loads(text)
    except (ValueError, TypeError):
        m = re.search(r"\{.*\}", text or "", re.DOTALL)
        if m:
            return json.loads(m.group(0))
        raise


def normalize_turn(data: dict) -> dict:
    """Coerce a model reply into the turn shape, downgrading anything
    unusable to a recorded 'wait' (a confused reply is data, not a
    crash)."""
    out = {
        "observation": str(data.get("observation") or ""),
        "action": data.get("action") if isinstance(data.get("action"), dict) else {},
        "expectation": str(data.get("expectation") or ""),
        "note": str(data.get("note") or ""),
    }
    if out["action"].get("do") not in ACTION_KINDS:
        out["note"] = (out["note"] + " [harness: unparseable action, treated as wait]").strip()
        out["action"] = {"do": "wait"}
    return out


class PlayerAgent:
    """The naive LLM player. decide() sees the screenshot + rolling
    memory only."""

    def __init__(self, persona: dict, manual: str, model: str = DEFAULT_MODEL,
                 effort: str = DEFAULT_EFFORT, max_tokens: int = DEFAULT_MAX_TOKENS):
        try:
            import anthropic
        except ImportError as e:
            raise SystemExit(
                "the player agent needs the Anthropic SDK: pip install anthropic\n"
                "(scripted/--smoke/--selftest/--replay runs don't)") from e
        self._anthropic = anthropic
        self.client = anthropic.Anthropic()  # ANTHROPIC_API_KEY / ant auth profile
        self.persona = persona
        self.manual = manual
        self.model = model
        self.effort = effort
        self.max_tokens = max_tokens
        self.needs_llm = True
        self._structured = True  # drop to lenient parsing if the model rejects it

    def decide(self, screenshot_path: str, fb_size, memory_lines: list[str],
               turn: int) -> dict:
        system = build_system_prompt(self.persona, self.manual, fb_size)
        with open(screenshot_path, "rb") as f:
            image_b64 = base64.standard_b64encode(f.read()).decode()
        memory = "\n".join(memory_lines) if memory_lines else "(first turn — nothing yet)"
        content = [
            {"type": "image",
             "source": {"type": "base64", "media_type": "image/png", "data": image_b64}},
            {"type": "text",
             "text": f"Turn {turn}. Your notes from recent turns:\n{memory}\n\n"
                     "Here is what you see now. One JSON object, one action."},
        ]
        kwargs = dict(
            model=self.model,
            max_tokens=self.max_tokens,
            thinking={"type": "disabled"},  # cheap + snappy; naive is the point
            system=[{"type": "text", "text": system,
                     "cache_control": {"type": "ephemeral"}}],
            messages=[{"role": "user", "content": content}],
        )
        output_config = {"effort": self.effort}
        if self._structured:
            output_config["format"] = {"type": "json_schema", "schema": TURN_SCHEMA}
        try:
            response = self.client.messages.create(output_config=output_config, **kwargs)
        except self._anthropic.BadRequestError:
            if not self._structured:
                raise
            # Model/config without structured-output support: fall back
            # to lenient parsing for the rest of the session.
            self._structured = False
            output_config.pop("format", None)
            response = self.client.messages.create(output_config=output_config, **kwargs)
        text = next((b.text for b in response.content if b.type == "text"), "")
        try:
            data = _lenient_parse(text)
        except (ValueError, TypeError):
            data = {"observation": "", "action": {"do": "wait"},
                    "expectation": "", "note": "[harness: reply was not JSON]"}
        result = normalize_turn(data)
        result["raw"] = text
        result["usage"] = {
            "input_tokens": response.usage.input_tokens,
            "output_tokens": response.usage.output_tokens,
            "cache_read_input_tokens": getattr(response.usage,
                                               "cache_read_input_tokens", None),
        }
        return result


class ScriptedAgent:
    """Deterministic agent for --smoke/--selftest: cycles a fixed,
    harmless action list (nothing that quits or commits anything)."""

    DEFAULT_SCRIPT = [
        {"do": "wait"},
        {"do": "scroll", "dy": -1},
        {"do": "hold", "name": "W"},   # exercises the pre/post split
        {"do": "key", "name": "Space"},
    ]

    def __init__(self, script: list[dict] | None = None):
        self.script = script or self.DEFAULT_SCRIPT
        self.needs_llm = False
        self._i = 0

    def decide(self, screenshot_path: str, fb_size, memory_lines: list[str],
               turn: int) -> dict:
        action = self.script[self._i % len(self.script)]
        self._i += 1
        return {
            "observation": f"[scripted turn {turn}]",
            "action": dict(action),
            "expectation": "[scripted]",
            "note": "",
            "raw": "",
            "usage": None,
        }


class RepeatAgent(ScriptedAgent):
    """Selftest helper: repeats one action forever (exercises the
    stuck-loop detector)."""

    def __init__(self):
        super().__init__([{"do": "wait"}])
