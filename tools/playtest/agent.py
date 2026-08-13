"""Player agents for the playtest harness (#647).

PlayerAgent is the naive LLM player. THE CARDINAL RULE lives here: its
inputs are persona + goal + manual + the current screenshot + its own
rolling memory of recent turns — and nothing else. No oracle data
(widget dumps, event logs, engine state) ever enters prompt assembly;
the constructor doesn't even accept a parameter it could arrive
through. The prompt casts it as a confused new player narrating their
experience and taking notes — explicitly not a bug-hunter.

ScriptedAgent is a deterministic no-LLM stand-in for --smoke and
--selftest runs (loop/trace/replay plumbing without a Codex login).
"""
from __future__ import annotations

import json
import os
import re
import shutil
import subprocess
import tempfile

from engine import ACTION_KINDS

# These are deliberately pins, not user-selectable defaults. Every naive-player
# turn must use the same cheap, quick Codex configuration so an ordinary H1 run
# cannot silently fall back to a costly provider/model. The critic and optional
# persona-flavor generator are separate workflows with their own model choices.
PLAYER_BACKEND = "codex-cli"
PLAYER_MODEL = "gpt-5.6-luna"
PLAYER_EFFORT = "medium"
DEFAULT_DECISION_TIMEOUT = 120.0

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
                "x": {"type": ["number", "null"]},
                "y": {"type": ["number", "null"]},
                "x1": {"type": ["number", "null"]},
                "y1": {"type": ["number", "null"]},
                "x2": {"type": ["number", "null"]},
                "y2": {"type": ["number", "null"]},
                "dx": {"type": ["number", "null"]},
                "dy": {"type": ["number", "null"]},
                "button": {"type": ["string", "null"]},
                "mods": {"type": ["array", "null"],
                         "items": {"type": "string"}},
                "name": {"type": ["string", "null"]},
                "text": {"type": ["string", "null"]},
                "reason": {"type": ["string", "null"]},
            },
            # Codex/OpenAI strict structured output requires every property to
            # be required; fields irrelevant to the chosen action are null.
            "required": ["do", "x", "y", "x1", "y1", "x2", "y2",
                         "dx", "dy", "button", "mods", "name", "text",
                         "reason"],
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
    raw_action = data.get("action")
    action = ({k: v for k, v in raw_action.items() if v is not None}
              if isinstance(raw_action, dict) else {})
    out = {
        "observation": str(data.get("observation") or ""),
        # Codex's strict schema requires nullable placeholders; discard them
        # before trace/memory/translation so the historical compact action
        # vocabulary stays unchanged.
        "action": action,
        "expectation": str(data.get("expectation") or ""),
        "note": str(data.get("note") or ""),
    }
    if out["action"].get("do") not in ACTION_KINDS:
        out["note"] = (out["note"] + " [harness: unparseable action, treated as wait]").strip()
        out["action"] = {"do": "wait"}
    return out


def _build_codex_command(codex_bin: str, screenshot_path: str, workspace: str,
                         schema_path: str, output_path: str) -> list[str]:
    """Build the pinned, oracle-blind Codex invocation for one turn.

    The isolated empty cwd and disabled tools are part of the purity boundary:
    the player can reason over the attached screenshot and prompt, but cannot
    inspect the game repository or acquire outside information.
    """
    return [
        codex_bin, "exec",
        "--model", PLAYER_MODEL,
        "--config", f'model_reasoning_effort="{PLAYER_EFFORT}"',
        "--config", 'model_provider="openai"',
        "--config", 'approval_policy="never"',
        "--config", 'web_search="disabled"',
        "--config", "agents.enabled=false",
        "--disable", "shell_tool",
        "--disable", "multi_agent",
        "--disable", "plugins",
        "--disable", "remote_plugin",
        "--disable", "skill_search",
        "--disable", "image_generation",
        "--disable", "view_image",
        "--sandbox", "read-only",
        "--cd", workspace,
        "--skip-git-repo-check",
        "--ignore-user-config",
        "--ignore-rules",
        "--ephemeral",
        "--strict-config",
        "--image", os.path.abspath(screenshot_path),
        "--output-schema", schema_path,
        "--output-last-message", output_path,
        "--json",
        "--color", "never",
        "-",
    ]


def _parse_codex_usage(events_jsonl: str) -> dict | None:
    """Extract the last turn-completed usage event from `codex exec --json`."""
    usage = None
    for line in events_jsonl.splitlines():
        try:
            event = json.loads(line)
        except (TypeError, ValueError):
            continue
        if not isinstance(event, dict):
            continue
        candidate = event.get("usage")
        if event.get("type") == "turn.completed" and isinstance(candidate, dict):
            usage = candidate
    if usage is None:
        return None
    result = {
        "input_tokens": usage.get("input_tokens"),
        "output_tokens": usage.get("output_tokens"),
        "cache_read_input_tokens": usage.get(
            "cached_input_tokens", usage.get("cache_read_input_tokens")),
    }
    if "reasoning_output_tokens" in usage:
        result["reasoning_output_tokens"] = usage["reasoning_output_tokens"]
    return result


class PlayerAgent:
    """The naive LLM player. decide() sees the screenshot + rolling
    memory only. Its provider/model/effort are intentionally fixed."""

    def __init__(self, persona: dict, manual: str,
                 decision_timeout: float = DEFAULT_DECISION_TIMEOUT):
        codex_bin = shutil.which("codex")
        if codex_bin is None:
            raise SystemExit(
                "the player agent needs the Codex CLI on PATH and logged in\n"
                "(check with: codex login status; scripted/--smoke/--selftest/"
                "--replay runs don't)")
        if decision_timeout <= 0:
            raise ValueError("decision_timeout must be positive")
        self.codex_bin = codex_bin
        self.persona = persona
        self.manual = manual
        self.model = PLAYER_MODEL
        self.effort = PLAYER_EFFORT
        self.decision_timeout = decision_timeout
        self.needs_llm = True

    def decide(self, screenshot_path: str, fb_size, memory_lines: list[str],
               turn: int) -> dict:
        system = build_system_prompt(self.persona, self.manual, fb_size)
        memory = "\n".join(memory_lines) if memory_lines else "(first turn — nothing yet)"
        prompt = (
            system
            + f"\nTurn {turn}. Your notes from recent turns:\n{memory}\n\n"
            + "The attached image is what you see now. Do not inspect files, "
              "search, or use tools; the prompt, your notes, and this screenshot "
              "are everything you know. Return one JSON object with one action."
        )

        # A fresh empty cwd on every turn prevents Codex's general-purpose agent
        # substrate from seeing the game checkout. Session continuity comes only
        # from the rolling player memory explicitly included above.
        with tempfile.TemporaryDirectory(prefix="synarchy_playtest_codex_") as scratch:
            workspace = os.path.join(scratch, "workspace")
            os.mkdir(workspace)
            schema_path = os.path.join(scratch, "turn.schema.json")
            output_path = os.path.join(scratch, "turn.json")
            with open(schema_path, "w", encoding="utf-8") as f:
                json.dump(TURN_SCHEMA, f)
            command = _build_codex_command(
                self.codex_bin, screenshot_path, workspace, schema_path, output_path)
            try:
                response = subprocess.run(
                    command, input=prompt, text=True, capture_output=True,
                    timeout=self.decision_timeout, check=False)
            except subprocess.TimeoutExpired as e:
                raise RuntimeError(
                    f"Codex player decision timed out after "
                    f"{self.decision_timeout:g}s") from e
            if response.returncode != 0:
                detail = (response.stderr or response.stdout or "no diagnostic").strip()
                raise RuntimeError(
                    f"Codex player decision failed (exit {response.returncode}): "
                    f"{detail[-2000:]}")
            try:
                with open(output_path, encoding="utf-8") as f:
                    text = f.read().strip()
            except OSError as e:
                raise RuntimeError("Codex player returned no final response file") from e

        try:
            data = _lenient_parse(text)
        except (ValueError, TypeError):
            data = {"observation": "", "action": {"do": "wait"},
                    "expectation": "", "note": "[harness: reply was not JSON]"}
        result = normalize_turn(data)
        result["raw"] = text
        result["usage"] = _parse_codex_usage(response.stdout)
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
