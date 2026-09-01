#!/usr/bin/env python3
"""Self-test component: player prompt, provider, response, action,
usage and budget behavior (#2040).

Owns the oracle-blind prompt assembly, the fixed provider profiles
and their CLI invocations, response normalization and its unusable
replies, the action-translation contract, the decision timeout, and
usage accounting/compaction.

Offline: every provider call is a monkeypatched subprocess, so no
window, build, network access, login or model call happens."""
from __future__ import annotations

import inspect
import json
import math
import os
import re
import subprocess
import sys
import tempfile

_HERE = os.path.dirname(os.path.abspath(__file__))
if _HERE not in sys.path:
    sys.path.insert(0, _HERE)

from engine import ActionError, FakeEngine, translate_action  # noqa: E402
from personas import load_persona  # noqa: E402
from session import run_session  # noqa: E402
from trace import SessionTrace, load_replay, load_turns  # noqa: E402
import agent as agent_mod  # noqa: E402
import engine as engine_mod  # noqa: E402
import launch as launch_mod  # noqa: E402

NAME = "player"


def run(check) -> None:
    """Run every player/provider/usage check into `check`."""
    with tempfile.TemporaryDirectory() as tmp:
        # 6. persona + prompt assembly stays oracle-blind by shape:
        # build_system_prompt takes persona/manual/fb only
        params = list(inspect.signature(agent_mod.build_system_prompt).parameters)
        check("prompt assembly accepts no oracle inputs",
              params == ["persona", "manual", "fb_size"], str(params))
        p = load_persona("curious_carl")
        prompt = agent_mod.build_system_prompt(p, "MANUAL", (1280, 720))
        check("prompt contains persona + manual + size",
              "curious_carl" in prompt and "MANUAL" in prompt
              and "1280x720" in prompt)

        # The naive player can select exactly one of two audited profiles. The
        # model and effort inside each profile remain hard pins, and both run
        # from an empty cwd without repository or network access.
        player_params = list(inspect.signature(agent_mod.PlayerAgent).parameters)
        check("naive player accepts only a complete profile selection",
              player_params == ["persona", "manual", "player_profile",
                                "decision_timeout"],
              str(player_params))
        check("approved player profiles pin both medium-effort models",
              agent_mod.PLAYER_PROFILES == {
                  "codex-luna": {
                      "backend": "codex-cli", "model": "gpt-5.6-luna",
                      "effort": "medium", "binary": "codex"},
                  "claude-sonnet": {
                      "backend": "claude-cli", "model": "claude-sonnet-5",
                      "effort": "medium", "binary": "claude"},
              })
        codex_cmd = agent_mod._build_codex_command(
            "/usr/bin/codex", "frame.png", os.path.join(tmp, "empty"),
            os.path.join(tmp, "turn.schema.json"), os.path.join(tmp, "turn.json"))
        check("Codex profile invokes gpt-5.6-luna medium",
              codex_cmd[:2] == ["/usr/bin/codex", "exec"]
              and "gpt-5.6-luna" in codex_cmd
              and 'model_reasoning_effort="medium"' in codex_cmd)
        check("Codex player cannot inspect the repo or acquire oracle data",
              "--ignore-user-config" in codex_cmd
              and "--ignore-rules" in codex_cmd
              and "--ephemeral" in codex_cmd
              and 'web_search="disabled"' in codex_cmd
              and all(feature in codex_cmd for feature in
                      ("shell_tool", "multi_agent", "plugins", "skill_search")))
        claude_cmd = agent_mod._build_claude_command(
            "/usr/bin/claude", os.path.join(tmp, "empty"), "SYSTEM")
        check("Claude profile invokes claude-sonnet-5 medium in safe mode",
              claude_cmd[:2] == ["/usr/bin/claude", "-p"]
              and "claude-sonnet-5" in claude_cmd
              and claude_cmd[claude_cmd.index("--effort") + 1] == "medium"
              and "--safe-mode" in claude_cmd
              and "--no-session-persistence" in claude_cmd)
        check("Claude player can read only the isolated screenshot",
              claude_cmd[claude_cmd.index("--tools") + 1] == "Read"
              and claude_cmd[claude_cmd.index("--allowedTools") + 1]
              == "Read(./screenshot.png)"
              and "--strict-mcp-config" in claude_cmd
              and "--disable-slash-commands" in claude_cmd)
        allowed_tool_values = [
            claude_cmd[i + 1] for i, value in enumerate(claude_cmd[:-1])
            if value == "--allowedTools"]
        check("Claude permission allowlist excludes every alternate read path",
              allowed_tool_values == ["Read(./screenshot.png)"]
              and "Read" not in allowed_tool_values
              and not any("**" in rule or "../" in rule
                          for rule in allowed_tool_values),
              str(allowed_tool_values))
        action_schema = agent_mod.TURN_SCHEMA["properties"]["action"]
        check("Codex strict action schema requires every declared field",
              set(action_schema["required"]) == set(action_schema["properties"])
              and all("null" in spec["type"] for name, spec in
                      action_schema["properties"].items() if name != "do"))
        normalized_nulls = agent_mod.normalize_turn({
            "observation": "",
            "action": {"do": "wait", "x": None, "name": None},
            "expectation": "", "note": ""})
        check("strict-schema null placeholders stay out of trace actions",
              normalized_nulls["action"] == {"do": "wait"},
              str(normalized_nulls["action"]))

        # --- the wheel contract (#1980) ---------------------------------
        # Two independent halves, both offline: the contract the player is
        # HANDED must agree with the engine's own sign convention, and the
        # contract the harness ENFORCES must not forward a delta the
        # published range excludes.
        camera_hs = os.path.join(launch_mod.REPO_ROOT, "src", "Engine",
                                 "Loop", "Camera.hs")
        camera_src = ""
        try:
            with open(camera_hs, encoding="utf-8") as f:
                camera_src = f.read()
        except OSError as e:
            check("Engine.Loop.Camera is readable for the polarity check",
                  False, str(e))
        # Derive which dy sign moves the camera toward the ground from the
        # checked-in Haskell rather than restating a Python constant. The
        # impulse is `zoomScrollScale * zoom * dy` with camZoom the viewport
        # half-height, and zoomMin is annotated as the CLOSEST zoom — so a
        # negative dy zooms in exactly when the scale is positive. Each
        # premise is matched explicitly, so a change to the formula, the
        # scale's sign, or which bound is closest fails this check loudly
        # instead of being derived past.
        impulse_ok = bool(re.search(
            r"scrollZoomImpulse\s+zoom\s+dy\s*=\s*zoomScrollScale\s*\*"
            r"\s*zoom\s*\*\s*dy", camera_src))
        scale_m = re.search(r"^zoomScrollScale\s*=\s*(-?[\d.]+)",
                            camera_src, re.M)
        min_m = re.search(r"^zoomMin\s*=\s*(-?[\d.]+)\s*--\s*closest zoom",
                          camera_src, re.M)
        max_m = re.search(r"^zoomMax\s*=\s*(-?[\d.]+)", camera_src, re.M)
        camera_premises = bool(impulse_ok and scale_m and min_m and max_m)
        check("Engine.Loop.Camera still states the premises the playtest "
              "wheel polarity is derived from",
              camera_premises,
              f"impulse={impulse_ok} scale={bool(scale_m)} "
              f"min={bool(min_m)} max={bool(max_m)}")
        engine_zoom_in_sign = None
        if camera_premises:
            scale = float(scale_m.group(1))
            zmin, zmax = float(min_m.group(1)), float(max_m.group(1))
            # zoom is a positive half-height, so sign(impulse) = sign(scale)
            # * sign(dy); a negative impulse walks camZoom toward the
            # smaller bound, which the source annotates as the closest one.
            if scale > 0 and zmin < zmax:
                engine_zoom_in_sign = -1
            elif scale < 0 and zmin < zmax:
                engine_zoom_in_sign = 1
        prompt = agent_mod.build_system_prompt(
            {"name": "n", "temperament": "t", "tendencies": ["x"],
             "goal": "g"}, "manual", (1280, 720))
        stated = re.findall(r"(?i)\b(negative|positive) dy zooms in", prompt)
        check("the player contract states the wheel polarity exactly once",
              len(stated) == 1, str(stated))
        stated_sign = ({"negative": -1, "positive": 1}[stated[0].lower()]
                       if len(stated) == 1 else None)
        check("the player contract's zoom-in sign matches "
              "Engine.Loop.Camera's own convention",
              stated_sign is not None
              and stated_sign == engine_zoom_in_sign,
              f"contract={stated_sign} engine={engine_zoom_in_sign}")
        stated_range = re.search(
            r"dy must be between (-?[\d.]+) and (-?[\d.]+)", prompt)
        check("the player contract publishes the enforced dy range",
              bool(stated_range)
              and float(stated_range.group(1)) == engine_mod.SCROLL_DY_MIN
              and float(stated_range.group(2)) == engine_mod.SCROLL_DY_MAX,
              stated_range.group(0) if stated_range else "absent")
        check("the player contract names one ordinary wheel notch",
              f"one notch is {engine_mod.SCROLL_DY_NOTCH:g}" in prompt)
        schema_dy = agent_mod.TURN_SCHEMA["properties"]["action"][
            "properties"]["dy"]
        schema_range = re.search(r"between (-?[\d.]+) and (-?[\d.]+)",
                                 schema_dy.get("description", ""))
        check("the structured schema publishes the same dy range",
              bool(schema_range)
              and float(schema_range.group(1)) == engine_mod.SCROLL_DY_MIN
              and float(schema_range.group(2)) == engine_mod.SCROLL_DY_MAX,
              schema_dy.get("description", "")[:60])

        def scroll_calls(act):
            collected: list[str] = []
            calls, post = translate_action(act, (1280, 720),
                                           notes=collected)
            return calls, post, collected

        def scroll_dy_of(calls):
            vals = [float(m.group(1)) for m in
                    (re.search(
                        r"input\.scroll\([^,]+,\s*"
                        r"(-?[\d.]+(?:[eE][-+]?\d+)?)\)", c)
                     for c in calls) if m]
            return vals

        notch, _, notch_notes = scroll_calls(
            {"do": "scroll", "dy": -engine_mod.SCROLL_DY_NOTCH})
        check("one ordinary notch is forwarded verbatim, unremarked",
              scroll_dy_of(notch) == [-engine_mod.SCROLL_DY_NOTCH]
              and notch_notes == [] and len(notch) == 1,
              str(notch))
        multi, _, multi_notes = scroll_calls({"do": "scroll", "dy": -4})
        check("a bounded multi-notch correction is one unremarked call",
              scroll_dy_of(multi) == [-4.0] and multi_notes == []
              and len(multi) == 1, str(multi))
        for edge in (engine_mod.SCROLL_DY_MIN, engine_mod.SCROLL_DY_MAX):
            edge_calls, _, edge_notes = scroll_calls(
                {"do": "scroll", "dy": edge})
            check(f"dy at the inclusive range edge {edge:g} is not clamped",
                  scroll_dy_of(edge_calls) == [edge] and edge_notes == [],
                  str(edge_calls) + str(edge_notes))
        # Requirement 4's headline is deliberately written against
        # translate_action's historical two-argument call, so it states a
        # property of the translation boundary itself rather than of this
        # revision's note plumbing: the same call forwarded dy=600 to the
        # engine verbatim before this contract existed.
        legacy_big, _ = translate_action({"do": "scroll", "dy": 600},
                                         (1280, 720))
        check("an oversized dy never reaches the engine verbatim",
              scroll_dy_of(legacy_big) == [engine_mod.SCROLL_DY_MAX]
              and len(legacy_big) == 1, str(legacy_big))
        big, _, big_notes = scroll_calls({"do": "scroll", "dy": 600})
        check("the turn records that the oversized dy was clamped, with "
              "both the requested and the effective value",
              len(big_notes) == 1 and "clamped" in big_notes[0]
              and "600" in big_notes[0]
              and f"{engine_mod.SCROLL_DY_MAX:g}" in big_notes[0],
              str(big_notes))
        for bad in (float("nan"), float("inf"), float("-inf")):
            rejected = None
            try:
                scroll_calls({"do": "scroll", "dy": bad})
            except ActionError as e:
                rejected = str(e)
            check(f"a non-finite dy ({bad}) is rejected, not forwarded",
                  rejected is not None and "rejected" in rejected,
                  str(rejected))
        # An in-range fraction must survive serialization: at any fixed
        # decimal width a small real gesture becomes a literal 0.0, and a
        # value just inside a bound rounds onto it while the turn records
        # no clamp — both of them the accepted no-op this contract exists
        # to stop.
        for fraction in (0.00001, -0.00001, 9.99999, -9.99999, 0.25):
            fcalls, _, fnotes = scroll_calls({"do": "scroll", "dy": fraction})
            check(f"an in-range fraction {fraction!r} is serialized "
                  "losslessly and unremarked",
                  scroll_dy_of(fcalls) == [fraction] and fnotes == [],
                  str(fcalls) + str(fnotes))
        # The translation boundary is the one that has to hold, so it
        # types dy itself instead of trusting the schema: a numeric string
        # and a bool are exactly what a lenient provider fallback and a
        # scripted agent produce, and float() would have accepted both.
        for bogus in ("5", "-1", True, False, [], {}, complex(1, 0)):
            typed = None
            try:
                scroll_calls({"do": "scroll", "dy": bogus})
            except ActionError as e:
                typed = str(e)
            check(f"a non-numeric dy ({bogus!r}) is rejected, not coerced",
                  typed is not None and "rejected" in typed, str(typed))
        # A float just outside a bound must be recorded as the value it
        # actually was. A fixed significant-figure format collapses it
        # onto the bound, producing a note that reads "dy 10 ... clamped
        # to 10" and loses what caused the clamp.
        for near in (10.0000001, -10.0000001,
                     math.nextafter(engine_mod.SCROLL_DY_MAX, math.inf),
                     math.nextafter(engine_mod.SCROLL_DY_MIN, -math.inf)):
            ncalls, _, nnotes = scroll_calls({"do": "scroll", "dy": near})
            bound = (engine_mod.SCROLL_DY_MAX if near > 0
                     else engine_mod.SCROLL_DY_MIN)
            check(f"a float just outside the bound ({near!r}) clamps and "
                  "records the value that caused it",
                  scroll_dy_of(ncalls) == [bound] and len(nnotes) == 1
                  and repr(near) in nnotes[0]
                  and float(re.search(r"scroll dy (\S+) is outside",
                                      nnotes[0]).group(1)) == near,
                  str(ncalls) + str(nnotes))
        # A schema-valid integer can be arbitrary precision and sit
        # entirely outside float range. It is still FINITE, so the
        # contract clamps it; converting first would raise OverflowError
        # and reject it instead.
        for huge, bound in ((10 ** 400, engine_mod.SCROLL_DY_MAX),
                            (-(10 ** 400), engine_mod.SCROLL_DY_MIN)):
            hcalls2, _, hnotes2 = scroll_calls({"do": "scroll", "dy": huge})
            check("an integer too large to be a float is clamped, not "
                  f"rejected (sign {'+' if huge > 0 else '-'})",
                  scroll_dy_of(hcalls2) == [bound] and len(hcalls2) == 1
                  and len(hnotes2) == 1 and "clamped" in hnotes2[0]
                  and f"({len(str(huge))} digits)" in hnotes2[0],
                  str(hcalls2) + str(hnotes2))
        # A clamp note must describe a scroll the engine actually
        # received. When a companion field fails to translate the turn
        # injects nothing, so claiming a clamp would put a false entry in
        # the trace and in the player's own memory.
        for companion in ({"dx": "invalid"}, {"x": "a", "y": 1}):
            bad_notes: list[str] = []
            raised = None
            try:
                translate_action({"do": "scroll", "dy": 600, **companion},
                                 (1280, 720), notes=bad_notes)
            except Exception as e:
                raised = e
            check("a scroll that fails to translate records no clamp "
                  f"note ({sorted(companion)})",
                  raised is not None and bad_notes == [],
                  f"{type(raised).__name__ if raised else None} {bad_notes}")
        absent, _, absent_notes = scroll_calls({"do": "scroll", "dx": 2})
        check("an absent dy still defaults to a zero vertical delta",
              scroll_dy_of(absent) == [0.0] and absent_notes == [],
              str(absent))
        aimed, _, _ = scroll_calls(
            {"do": "scroll", "dy": -2, "x": 640, "y": 360})
        check("cursor-aimed scrolling still pre-moves, then scrolls once",
              len(aimed) == 2 and aimed[0].startswith("return input.moveMouse")
              and "input.scroll" in aimed[1]
              and scroll_dy_of(aimed) == [-2.0], str(aimed))
        for horizontal in (3, 600, -7.5):
            hcalls, _, hnotes = scroll_calls(
                {"do": "scroll", "dx": horizontal, "dy": 0})
            check(f"horizontal dx {horizontal:g} keeps its verbatim "
                  "forwarding",
                  len(hcalls) == 1
                  and f"input.scroll({float(horizontal):.1f}," in hcalls[0]
                  and hnotes == [], str(hcalls))
        check("every scroll action generates exactly one input.scroll call",
              all(sum("input.scroll" in c for c in cs) == 1
                  for cs in (notch, multi, big, legacy_big, aimed)))

        # The clamp reaches the trace through the real recording path:
        # requested action retained, clamp recorded in the note, and only
        # the bounded call in injected/replay.
        cdir = os.path.join(tmp, "scroll-clamp")
        ctrace2 = SessionTrace(cdir, {"mode": "selftest-scroll-clamp"})
        run_session(FakeEngine(),
                    agent_mod.ScriptedAgent([{"do": "scroll", "dy": 600}]),
                    ctrace2, turns=1, dt=0.0, max_seconds=None,
                    memory_turns=4, stuck_k=99, settle=0.0)
        ctrace2.finish("turn_budget_exhausted")
        cturn = load_turns(cdir)[0]
        creplay = load_replay(cdir)[0]
        check("the clamped turn retains the action the player requested",
              cturn["player"]["action"] == {"do": "scroll", "dy": 600},
              str(cturn["player"]["action"]))
        check("the clamped turn's note says a clamp happened",
              "clamped" in cturn["player"]["note"],
              cturn["player"]["note"])
        check("only the bounded call lands in injected and replay data",
              cturn["injected"] == creplay["pre"]
              and len(cturn["injected"]) == 1
              and scroll_dy_of(cturn["injected"]) == [
                  engine_mod.SCROLL_DY_MAX]
              and "600" not in cturn["injected"][0],
              str(cturn["injected"]))
        usage = agent_mod._parse_codex_usage(
            '{"type":"thread.started"}\n'
            '{"type":"turn.completed","usage":{"input_tokens":123,'
            '"cached_input_tokens":45,"output_tokens":67}}\n')
        check("Codex JSONL token usage maps into the existing trace shape",
              usage == {"input_tokens": 123, "output_tokens": 67,
                        "cache_read_input_tokens": 45}, str(usage))
        (claude_turn, claude_usage,
         claude_fallback) = agent_mod._parse_claude_result(json.dumps({
            "structured_output": {
                "observation": "menu", "action": {"do": "wait"},
                "expectation": "", "note": ""},
            "modelUsage": {
                "claude-sonnet-5": {
                    "inputTokens": 2, "outputTokens": 52,
                    "cacheReadInputTokens": 1085,
                    "cacheCreationInputTokens": 0},
                "claude-haiku-4-5": {
                    "inputTokens": 897, "outputTokens": 12,
                    "cacheReadInputTokens": 0,
                    "cacheCreationInputTokens": 0},
            },
        }))
        check("Claude usage includes cached input and helper-model calls",
              claude_turn["action"] == {"do": "wait"}
              and claude_usage == {
                  "input_tokens": 1984, "output_tokens": 64,
                  "cache_read_input_tokens": 1085,
                  "cache_creation_input_tokens": 0}, str(claude_usage))
        check("a usable structured_output needs no fallback text",
              claude_fallback == "", repr(claude_fallback))

        # #1874: a player reply that parses as valid JSON but is not an
        # object is a confused turn, not a crash. Driven through
        # PlayerAgent.decide with only the provider process faked, so the
        # whole production path runs — including _parse_claude_result's
        # own fallback handoff — and the checks assert the observable
        # turn rather than a helper's return value.
        shot = os.path.join(tmp, "reply_shape_frame.png")
        with open(shot, "wb") as f:
            f.write(b"\x89PNG\r\n\x1a\n")
        codex_usage_stdout = (
            '{"type":"thread.started"}\n'
            '{"type":"turn.completed","usage":{"input_tokens":123,'
            '"cached_input_tokens":45,"output_tokens":67}}\n')
        codex_expected_usage = {"input_tokens": 123, "output_tokens": 67,
                                "cache_read_input_tokens": 45}
        claude_model_usage = {
            "claude-sonnet-5": {
                "inputTokens": 2, "outputTokens": 52,
                "cacheReadInputTokens": 1085, "cacheCreationInputTokens": 0}}
        claude_expected_usage = {
            "input_tokens": 1087, "output_tokens": 52,
            "cache_read_input_tokens": 1085, "cache_creation_input_tokens": 0}

        def decide_with_reply(backend, stdout="", file_text=None):
            """One real decide() turn against a faked provider process."""
            profile_name = ("codex-luna" if backend == "codex-cli"
                            else "claude-sonnet")
            player = object.__new__(agent_mod.PlayerAgent)
            player.provider_bin = "/nonexistent/provider"
            player.player_profile = profile_name
            player.backend = backend
            player.persona = p
            player.manual = "MANUAL"
            player.profile = dict(agent_mod.PLAYER_PROFILES[profile_name])
            player.model = player.profile["model"]
            player.effort = player.profile["effort"]
            player.decision_timeout = 30.0
            player.needs_llm = True

            def fake_run(command, **kwargs):
                if file_text is not None:
                    out_path = command[
                        command.index("--output-last-message") + 1]
                    with open(out_path, "w", encoding="utf-8") as handle:
                        handle.write(file_text)
                return subprocess.CompletedProcess(command, 0, stdout, "")

            saved_run = agent_mod.subprocess.run
            agent_mod.subprocess.run = fake_run
            try:
                return player.decide(shot, (1280, 720), [], 1)
            finally:
                agent_mod.subprocess.run = saved_run

        def check_unusable_reply(label, turn, raw, usage):
            check(label,
                  turn["action"] == {"do": "wait"}
                  and turn["observation"] == ""
                  and turn["expectation"] == ""
                  and turn["note"] == agent_mod.NON_OBJECT_NOTE
                  and turn["note"] != agent_mod.NOT_JSON_NOTE
                  and turn["raw"] == raw
                  and turn["usage"] == usage, str(turn))

        # Falsy non-object through the Codex path, which reaches
        # normalize_turn outside decide's parse-exception handler.
        check_unusable_reply(
            "a falsy non-object Codex reply is a recorded wait, not a crash",
            decide_with_reply("codex-cli", stdout=codex_usage_stdout,
                              file_text="[]"),
            "[]", codex_expected_usage)
        check_unusable_reply(
            "a scalar Codex reply is a recorded wait, not a crash",
            decide_with_reply("codex-cli", stdout=codex_usage_stdout,
                              file_text='"wait"'),
            '"wait"', codex_expected_usage)

        # Claude: a non-mapping structured_output falls back to `result`,
        # which is itself valid non-object JSON. Before #1874 this left
        # _parse_claude_result returning a list and crashed normalize_turn.
        check_unusable_reply(
            "a non-object Claude fallback is a recorded wait, not a crash",
            decide_with_reply("claude-cli", stdout=json.dumps({
                "structured_output": [],
                "result": "[1, 2]",
                "modelUsage": claude_model_usage})),
            "[1, 2]", claude_expected_usage)
        # A `null` fallback: valid JSON of the wrong type, which before
        # #1874 was misreported as malformed JSON with the text erased.
        check_unusable_reply(
            "a null Claude fallback keeps its text and its own note",
            decide_with_reply("claude-cli", stdout=json.dumps({
                "result": "null", "modelUsage": claude_model_usage})),
            "null", claude_expected_usage)

        # The narrow guard: text that is not JSON at all still gets the
        # existing malformed-JSON wait, and a well-formed object still
        # becomes its own turn with usage intact.
        not_json_turn = decide_with_reply(
            "codex-cli", stdout=codex_usage_stdout, file_text="sorry, no idea")
        check("unparseable reply text keeps the malformed-JSON wait",
              not_json_turn["action"] == {"do": "wait"}
              and not_json_turn["note"] == agent_mod.NOT_JSON_NOTE
              and not_json_turn["raw"] == "sorry, no idea"
              and not_json_turn["usage"] == codex_expected_usage,
              str(not_json_turn))
        good_turn = decide_with_reply("claude-cli", stdout=json.dumps({
            "structured_output": {
                "observation": "menu", "action": {"do": "scroll", "dy": -1},
                "expectation": "list moves", "note": "trying"},
            "modelUsage": claude_model_usage}))
        check("a well-formed structured reply still becomes its own turn",
              good_turn["action"] == {"do": "scroll", "dy": -1}
              and good_turn["observation"] == "menu"
              and good_turn["note"] == "trying"
              and good_turn["usage"] == claude_expected_usage,
              str(good_turn))
        non_object_fallback = agent_mod._parse_claude_result(
            json.dumps({"result": "[]"}))
        check("_parse_claude_result never hands back a non-mapping turn",
              non_object_fallback[0] is None
              and non_object_fallback[2] == "[]", str(non_object_fallback))

