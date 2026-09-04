#!/usr/bin/env python3
"""The `run_one` interface the harness shares with the probe runner (#2087).
"""
from __future__ import annotations

import os
import textwrap

from .support import probe_protocol, probe_runner_lifecycle
from .support import SyntheticTree, TOOLS_DIR, expect

def test_run_one_defaults() -> None:
    print("\n-- run_one's extended interface --")
    expect(probe_runner_lifecycle.probe_protocol_env() == {},
           "no protocol wiring produces no environment override")
    env = probe_runner_lifecycle.probe_protocol_env(
        event_path="/e", artifact_dir="/a", engine_log_dir="/l", rts_caps=4)
    expect(env == {probe_protocol.ENV_EVENTS: "/e",
                   probe_protocol.ENV_ARTIFACT_DIR: "/a",
                   probe_protocol.ENV_ENGINE_LOG_DIR: "/l",
                   probe_protocol.ENV_RTS_CAPS: "4"},
           "every protocol parameter reaches the child through the environment")
    # An operator's stale export must not push an ordinary run_probes.py
    # run into protocol mode.
    stale = "/tmp/should-be-ignored.jsonl"
    saved = os.environ.get(probe_protocol.ENV_EVENTS)
    os.environ[probe_protocol.ENV_EVENTS] = stale
    try:
        with SyntheticTree() as tree:
            script = tree.root / "tools" / "echoenv_probe.py"
            script.write_text(textwrap.dedent(f'''\
                import os, sys
                sys.path.insert(0, {TOOLS_DIR!r})
                import probe_protocol
                print(repr(os.environ.get(probe_protocol.ENV_EVENTS)))
            '''), encoding="utf-8")
            _ok, _t, _e, out = probe_runner_lifecycle.run_one("echoenv_probe.py", None, 60.0)
            expect(out.strip() == "None",
                   f"an inherited SYNARCHY_PROBE_EVENTS is stripped from an "
                   f"ordinary run (got {out.strip()!r})")
            _ok, _t, _e, out = probe_runner_lifecycle.run_one(
                "echoenv_probe.py", None, 60.0, event_path="/tmp/wanted.jsonl")
            expect(out.strip() == "'/tmp/wanted.jsonl'",
                   f"the harness's own event path wins (got {out.strip()!r})")
    finally:
        if saved is None:
            os.environ.pop(probe_protocol.ENV_EVENTS, None)
        else:
            os.environ[probe_protocol.ENV_EVENTS] = saved

    import inspect
    signature = inspect.signature(probe_runner_lifecycle.run_one)
    positional = [n for n, p in signature.parameters.items()
                  if p.kind is inspect.Parameter.POSITIONAL_OR_KEYWORD]
    expect(positional == ["script", "port", "timeout", "groups"],
           "run_one's positional interface is unchanged for existing callers")
    expect(all(signature.parameters[n].default is None
               for n in ("event_path", "artifact_dir", "engine_log_dir",
                         "rts_caps")),
           "every new parameter is keyword-only with a default")


TESTS = (
    test_run_one_defaults,
)
