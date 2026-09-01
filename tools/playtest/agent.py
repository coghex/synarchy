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

from engine import (ACTION_KINDS, SCROLL_DY_MAX, SCROLL_DY_MIN,
                    SCROLL_DY_NOTCH)

# These are fixed, audited profiles rather than arbitrary model overrides.  A
# run selects one complete profile, so it cannot silently drift to a costly
# model or a different effort level. The critic and optional persona-flavor
# generator are separate workflows with their own model choices.
PLAYER_PROFILES = {
    "codex-luna": {
        "backend": "codex-cli",
        "model": "gpt-5.6-luna",
        "effort": "medium",
        "binary": "codex",
    },
    "claude-sonnet": {
        "backend": "claude-cli",
        "model": "claude-sonnet-5",
        "effort": "medium",
        "binary": "claude",
    },
}
DEFAULT_PLAYER_PROFILE = "codex-luna"
DEFAULT_DECISION_TIMEOUT = 90.0
CLAUDE_SCREENSHOT_READ_RULE = "Read(./screenshot.png)"


# The player-facing wheel sentence (#1980), rendered from the SAME
# constants translate_action enforces so the published contract and the
# enforced one cannot drift. The polarity wording is the CAMERA's, taken
# from Engine.Loop.Camera's own sign convention: a player who reads
# "negative = away/up" reasons about the gesture and gets the opposite of
# what the game does. Keep the canonical clause "negative dy zooms in"
# intact and unique — run.py --selftest parses this rendered text and
# checks it against the checked-in Haskell.
SCROLL_MULTI_NOTCH = -4.0
SCROLL_ACTION_LINE = (
    '- scroll: {"do":"scroll","dy":N} — the mouse wheel, measured in '
    f'notches: one notch is {SCROLL_DY_NOTCH:g}, and dy must be between '
    f'{SCROLL_DY_MIN:g} and {SCROLL_DY_MAX:g} (fractions are fine). '
    'Negative dy zooms in, closer to the ground; positive dy zooms out. '
    f'Ask for several notches at once (e.g. "dy":{SCROLL_MULTI_NOTCH:g}) '
    'to travel further in one go — the wheel moves further per notch the '
    'further out you already are. Optional "x","y" to aim the pointer '
    'first.'
)
SCROLL_SCHEMA_DESCRIPTION = (
    f'Vertical wheel delta in notches: one notch is {SCROLL_DY_NOTCH:g}, '
    f'between {SCROLL_DY_MIN:g} and {SCROLL_DY_MAX:g} inclusive. Negative '
    'dy zooms in, closer to the ground; positive dy zooms out. A value '
    'outside that range is clamped to the nearest bound and the turn '
    'records it.'
)


class DecisionTimeout(RuntimeError):
    """The bounded provider call used all time available for this turn."""

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
                # The bound is PUBLISHED here and ENFORCED in
                # translate_action, not declared as minimum/maximum
                # keywords: a scripted agent and a lenient provider
                # fallback both reach the translation boundary without
                # any schema having validated them, so the schema
                # keyword would be the weaker half of a guarantee the
                # boundary has to make anyway.
                "dy": {"type": ["number", "null"],
                       "description": SCROLL_SCHEMA_DESCRIPTION},
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
{scroll_action}
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
        manual=manual.strip(), width=fb_size[0], height=fb_size[1],
        scroll_action=SCROLL_ACTION_LINE)


# The two ways a reply can be unusable before its action is even read.
# They stay distinct in the trace: one is text that is not JSON at all,
# the other is JSON of the wrong top-level type.
NOT_JSON_NOTE = "[harness: reply was not JSON]"
NON_OBJECT_NOTE = ("[harness: reply was valid JSON but not an object, "
                   "treated as wait]")


def _lenient_parse(text: str) -> object:
    try:
        return json.loads(text)
    except (ValueError, TypeError):
        m = re.search(r"\{.*\}", text or "", re.DOTALL)
        if m:
            return json.loads(m.group(0))
        raise


def normalize_turn(data: object) -> dict:
    """Coerce a model reply into the turn shape, downgrading anything
    unusable to a recorded 'wait' (a confused reply is data, not a
    crash).

    ``data`` is whatever the reply parsed to, which is not necessarily a
    mapping: valid JSON of any other shape (a list, a scalar, ``null``)
    is a confused turn too, and gets its own note so the trace says which
    failure the model actually produced rather than crashing the session.
    """
    if not isinstance(data, dict):
        return {"observation": "", "action": {"do": "wait"},
                "expectation": "", "note": NON_OBJECT_NOTE}
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
                         schema_path: str, output_path: str,
                         profile: dict | None = None) -> list[str]:
    """Build the pinned, oracle-blind Codex invocation for one turn.

    The isolated empty cwd and disabled tools are part of the purity boundary:
    the player can reason over the attached screenshot and prompt, but cannot
    inspect the game repository or acquire outside information.
    """
    profile = profile or PLAYER_PROFILES["codex-luna"]
    return [
        codex_bin, "exec",
        "--model", profile["model"],
        "--config", f'model_reasoning_effort="{profile["effort"]}"',
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


def _build_claude_command(claude_bin: str, workspace: str,
                          system_prompt: str, profile: dict | None = None
                          ) -> list[str]:
    """Build an OAuth-compatible, oracle-blind Claude Code invocation.

    Safe mode disables local customizations while the fresh cwd contains only
    the copied screenshot. Read is the sole tool because Claude Code has no
    image-attachment flag in print mode; its permission rule names that one
    relative path exactly, so a guessed absolute repository path is denied.
    """
    profile = profile or PLAYER_PROFILES["claude-sonnet"]
    return [
        claude_bin, "-p",
        "--safe-mode",
        "--disable-slash-commands",
        "--no-session-persistence",
        "--prompt-suggestions", "false",
        "--model", profile["model"],
        "--effort", profile["effort"],
        "--tools", "Read",
        "--allowedTools", CLAUDE_SCREENSHOT_READ_RULE,
        "--permission-mode", "dontAsk",
        "--strict-mcp-config",
        "--mcp-config", '{"mcpServers":{}}',
        "--system-prompt", system_prompt,
        "--json-schema", json.dumps(TURN_SCHEMA, separators=(",", ":")),
        "--output-format", "json",
        ("Use Read to inspect screenshot.png, then return one structured "
         "turn decision. Do not inspect any other path."),
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


def _parse_claude_result(output: str) -> tuple[dict | None, dict | None, str]:
    """Return (structured turn, normalized usage, fallback text) from
    Claude Code JSON.

    The turn is only ever a mapping or ``None`` — the same discipline the
    outer payload and ``structured_output`` already get. A ``result`` that
    parses as valid JSON of some other shape is not a turn, so it is not
    returned as one.

    Whenever ``structured_output`` was unusable the raw ``result`` text is
    handed back separately, independent of what it parsed to. The caller
    needs it for two things the accepted turn value cannot carry: telling
    malformed fallback text apart from valid non-object JSON, and keeping
    the model's own words in the trace even when they parsed to something
    falsy.

    Claude Code may make small internal helper-model calls. ``modelUsage`` is
    therefore the conservative source: all models, cached/created input, and
    output are counted. This intentionally measures actual CLI consumption,
    not only the named Sonnet response.
    """
    try:
        payload = json.loads(output)
    except (TypeError, ValueError):
        return None, None, ""
    if not isinstance(payload, dict):
        return None, None, ""
    structured = payload.get("structured_output")
    fallback = ""
    if not isinstance(structured, dict):
        result_text = payload.get("result")
        fallback = result_text if isinstance(result_text, str) else ""
        try:
            parsed = _lenient_parse(fallback)
        except (TypeError, ValueError):
            parsed = None
        structured = parsed if isinstance(parsed, dict) else None
    input_tokens = 0
    output_tokens = 0
    cache_read = 0
    cache_creation = 0
    models = payload.get("modelUsage")
    if isinstance(models, dict) and models:
        for usage in models.values():
            if not isinstance(usage, dict):
                continue
            input_tokens += int(usage.get("inputTokens") or 0)
            output_tokens += int(usage.get("outputTokens") or 0)
            cache_read += int(usage.get("cacheReadInputTokens") or 0)
            cache_creation += int(usage.get("cacheCreationInputTokens") or 0)
        # Claude reports cached and newly-cached prompt tokens separately from
        # inputTokens. Both are input consumption and belong in the total.
        input_tokens += cache_read + cache_creation
    else:
        usage = payload.get("usage")
        if isinstance(usage, dict):
            cache_read = int(usage.get("cache_read_input_tokens") or 0)
            cache_creation = int(usage.get("cache_creation_input_tokens") or 0)
            input_tokens = (int(usage.get("input_tokens") or 0)
                            + cache_read + cache_creation)
            output_tokens = int(usage.get("output_tokens") or 0)
        else:
            return structured, None, fallback
    return structured, {
        "input_tokens": input_tokens,
        "output_tokens": output_tokens,
        "cache_read_input_tokens": cache_read,
        "cache_creation_input_tokens": cache_creation,
    }, fallback


class PlayerAgent:
    """The naive LLM player. decide() sees the screenshot + rolling
    memory only. Provider/model/effort come from one fixed profile."""

    def __init__(self, persona: dict, manual: str,
                 player_profile: str = DEFAULT_PLAYER_PROFILE,
                 decision_timeout: float = DEFAULT_DECISION_TIMEOUT):
        if player_profile not in PLAYER_PROFILES:
            raise ValueError(f"unknown player profile {player_profile!r}")
        profile = dict(PLAYER_PROFILES[player_profile])
        provider_bin = shutil.which(profile["binary"])
        if provider_bin is None:
            auth_check = ("codex login status" if profile["backend"] == "codex-cli"
                          else "claude auth status")
            raise SystemExit(
                f"the {player_profile} player needs {profile['binary']} on PATH "
                f"and logged in\n(check with: {auth_check}; scripted/--smoke/"
                "--selftest/--replay runs don't)")
        if decision_timeout <= 0:
            raise ValueError("decision_timeout must be positive")
        self.provider_bin = provider_bin
        self.player_profile = player_profile
        self.backend = profile["backend"]
        self.persona = persona
        self.manual = manual
        self.model = profile["model"]
        self.effort = profile["effort"]
        self.profile = profile
        self.decision_timeout = decision_timeout
        self.needs_llm = True

    def decide(self, screenshot_path: str, fb_size, memory_lines: list[str],
               turn: int, timeout_seconds: float | None = None) -> dict:
        system = build_system_prompt(self.persona, self.manual, fb_size)
        memory = "\n".join(memory_lines) if memory_lines else "(first turn — nothing yet)"
        prompt = (
            system
            + f"\nTurn {turn}. Your notes from recent turns:\n{memory}\n\n"
            + "The attached image is what you see now. Do not inspect files, "
              "search, or use tools; the prompt, your notes, and this screenshot "
              "are everything you know. Return one JSON object with one action."
        )

        timeout = self.decision_timeout
        if timeout_seconds is not None:
            timeout = min(timeout, timeout_seconds)
        if timeout <= 0:
            raise DecisionTimeout("no session time remains for a player decision")

        # A fresh empty cwd on every turn prevents either general-purpose agent
        # substrate from seeing the game checkout. Session continuity comes only
        # from the rolling player memory explicitly included above.
        prefix = f"synarchy_playtest_{self.backend.replace('-cli', '')}_"
        with tempfile.TemporaryDirectory(prefix=prefix) as scratch:
            workspace = os.path.join(scratch, "workspace")
            os.mkdir(workspace)
            if self.backend == "codex-cli":
                schema_path = os.path.join(scratch, "turn.schema.json")
                output_path = os.path.join(scratch, "turn.json")
                with open(schema_path, "w", encoding="utf-8") as f:
                    json.dump(TURN_SCHEMA, f)
                command = _build_codex_command(
                    self.provider_bin, screenshot_path, workspace, schema_path,
                    output_path, self.profile)
                run_input = prompt
            else:
                shutil.copyfile(screenshot_path,
                                os.path.join(workspace, "screenshot.png"))
                command = _build_claude_command(
                    self.provider_bin, workspace, system, self.profile)
                command[-1] = (
                    f"Turn {turn}. Your notes from recent turns:\n{memory}\n\n"
                    + command[-1])
                run_input = None
            try:
                response = subprocess.run(
                    command, input=run_input, text=True, capture_output=True,
                    timeout=timeout, check=False, cwd=workspace)
            except subprocess.TimeoutExpired as e:
                raise DecisionTimeout(
                    f"{self.backend} player decision timed out after "
                    f"{timeout:g}s") from e
            if response.returncode != 0:
                detail = (response.stderr or response.stdout or "no diagnostic").strip()
                raise RuntimeError(
                    f"{self.backend} player decision failed "
                    f"(exit {response.returncode}): "
                    f"{detail[-2000:]}")

            if self.backend == "codex-cli":
                try:
                    with open(output_path, encoding="utf-8") as f:
                        text = f.read().strip()
                except OSError as e:
                    raise RuntimeError(
                        "Codex player returned no final response file") from e
                data = None
                usage = _parse_codex_usage(response.stdout)
            else:
                data, usage, fallback = _parse_claude_result(response.stdout)
                # Keep the model's own fallback text rather than a
                # re-serialization of it: an unusable parse still has to
                # reach the trace, and a falsy one has no serialization.
                text = (json.dumps(data, separators=(",", ":"))
                        if isinstance(data, dict) else fallback)

        if data is None:
            try:
                data = _lenient_parse(text)
            except (ValueError, TypeError):
                data = {"observation": "", "action": {"do": "wait"},
                        "expectation": "", "note": NOT_JSON_NOTE}
        # normalize_turn, not this branch, owns the wrong-top-level-type
        # case: _lenient_parse succeeding says nothing about the shape.
        result = normalize_turn(data)
        result["raw"] = text
        result["usage"] = usage
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
               turn: int, timeout_seconds: float | None = None) -> dict:
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
