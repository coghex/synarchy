"""Model transport for the playtest critic (#648, extracted by
#2069): the one multimodal, oracle-armed adjudication call.

Owns the Anthropic invocation and nothing else — the client, the
structured-output request against `critic_contract.FINDINGS_SCHEMA`,
base64 image attachment of the batch's frames (a negative turn key is
that turn's own post-step frame, #775), and the model/effort/token
settings. The SDK is imported lazily in `Critic.__init__`, so
`--selftest` never needs it installed.

Consumes `critic_contract` only. It does not import orchestration, and
it holds no test double: the deterministic fake critics live in
`critic_selftest.py`, so a production run never imports test support.
"""
from __future__ import annotations

import base64
import json
import os
import sys

_HERE = os.path.dirname(os.path.abspath(__file__))
if _HERE not in sys.path:
    sys.path.insert(0, _HERE)

from critic_contract import (DEFAULT_EFFORT, DEFAULT_MAX_TOKENS,  # noqa: E402
                             DEFAULT_MODEL, FINDINGS_SCHEMA, SYSTEM_PROMPT)


class Critic:
    """One multimodal, oracle-armed adjudication call."""

    def __init__(self, model: str = DEFAULT_MODEL, effort: str = DEFAULT_EFFORT,
                 max_tokens: int = DEFAULT_MAX_TOKENS):
        try:
            import anthropic
        except ImportError as e:
            raise SystemExit(
                "the critic needs the Anthropic SDK: pip install anthropic\n"
                "(--selftest doesn't)") from e
        self._anthropic = anthropic
        self.client = anthropic.Anthropic()
        self.model = model
        self.effort = effort
        self.max_tokens = max_tokens

    def adjudicate(self, digest: str, manual: str,
                   frames: list[tuple[int, str]],
                   ask: str | None = None) -> dict:
        content = []
        for n, path in frames:
            with open(path, "rb") as f:
                data = base64.standard_b64encode(f.read()).decode()
            # a negative n is that same turn's OWN post-step frame
            # (#775), not a different turn — see critic_signals.plan_batches
            label = (f"[screenshot of turn {n}]" if n > 0
                     else f"[screenshot of turn {-n}, after that turn's step]")
            content.append({"type": "text", "text": label})
            content.append({"type": "image",
                            "source": {"type": "base64",
                                       "media_type": "image/png", "data": data}})
        content.append({"type": "text", "text":
                        "THE PLAYER MANUAL (the intended mental model)\n---\n"
                        + manual + "\n---\n\n" + digest
                        + "\n\n" + (ask or "Produce the findings JSON now.")})
        response = self.client.messages.create(
            model=self.model,
            max_tokens=self.max_tokens,
            thinking={"type": "adaptive"},
            output_config={"effort": self.effort,
                           "format": {"type": "json_schema",
                                      "schema": FINDINGS_SCHEMA}},
            system=[{"type": "text", "text": SYSTEM_PROMPT,
                     "cache_control": {"type": "ephemeral"}}],
            messages=[{"role": "user", "content": content}],
        )
        text = next((b.text for b in response.content if b.type == "text"), "")
        return json.loads(text)
