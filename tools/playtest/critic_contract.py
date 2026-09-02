"""The critic contract (#648, extracted by #2069): everything the
critic's inputs and outputs are DEFINED by, with no behavior of its own.

  * the closed enums a finding is validated against — categories,
    severities, verdicts, confidences;
  * the structured-output findings schema handed to the model;
  * the system prompt (the joins, the evidence discipline, the output
    contract as the model sees it);
  * the default model, effort, token and screenshot-budget settings the
    `critic.py` command line exposes.

A leaf: nothing here imports another playtest module, so the analysis,
transport and validation owners can all consume it without a cycle.
Changing a category, a prompt rule or a schema field is a behavior
change for every consumer at once — see `critic_evidence.py` for what
is mechanically enforced against these enums.
"""
from __future__ import annotations


DEFAULT_MODEL = "claude-opus-5"   # strong + multimodal; runs once per session
DEFAULT_EFFORT = "high"
DEFAULT_MAX_TOKENS = 16000
DEFAULT_MAX_FRAMES = 8


CATEGORIES = ("soft-lock", "missing-feedback", "phantom-affordance",
              "misleading-affordance", "discoverability", "doc-mismatch",
              "crash", "other")
SEVERITIES = ("blocker", "major", "minor", "polish")
VERDICTS = ("defect", "intended")
CONFIDENCES = ("high", "medium", "low")


FINDINGS_SCHEMA = {
    "type": "object",
    "properties": {
        "summary": {"type": "string",
                    "description": "3-6 sentence session-level summary."},
        "findings": {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {
                    "title": {"type": "string"},
                    "category": {"type": "string", "enum": list(CATEGORIES)},
                    "severity": {"type": "string", "enum": list(SEVERITIES)},
                    "verdict": {"type": "string", "enum": list(VERDICTS)},
                    "confidence": {"type": "string", "enum": list(CONFIDENCES)},
                    "evidence": {
                        "type": "object",
                        "properties": {
                            "turns": {"type": "array", "items": {"type": "integer"}},
                            "candidate_ids": {"type": "array",
                                              "items": {"type": "string"}},
                            "player_quote": {"type": "string"},
                            "oracle": {"type": "string",
                                       "description": "The specific oracle record(s) grounding this."},
                        },
                        "required": ["turns", "candidate_ids", "player_quote",
                                     "oracle"],
                        "additionalProperties": False,
                    },
                    "root_cause_hypothesis": {"type": "string"},
                },
                "required": ["title", "category", "severity", "verdict",
                             "confidence", "evidence", "root_cause_hypothesis"],
                "additionalProperties": False,
            },
        },
    },
    "required": ["summary", "findings"],
    "additionalProperties": False,
}


SYSTEM_PROMPT = """\
You are the critic in a naive-player UX playtest harness. A cheap, \
deliberately naive LLM "player" played the game seeing only \
screenshots and narrated its experience; you are the sharp analyst \
with the ground truth it never had. Your job: walk EVERY friction \
point, decide what is a real defect versus the player just being \
naive, and write it up so a maintainer can act on it.

You receive:
- the minimal player manual the player was given (the INTENDED mental \
model — a mismatch between it and observed behavior is a doc-mismatch \
or a manual gap);
- a per-turn session digest: the player's observation / action / \
EXPECTATION / note, the exact injected inputs, and the ORACLE the \
player never saw (widget dump with bounds+labels, event-log rows plus \
any gaps in them, action-outcome records where available, pause/menu \
state), plus \
harness-computed signals and joins;
- screenshots of the friction turns (you must actually look at them \
to judge visual clarity and whether feedback was visible);
- a list of FRICTION CANDIDATES with ids (C1, C2, ...). You MUST \
adjudicate every candidate id in exactly the findings that cover it — \
none may be left uncovered.

Categories: soft-lock, missing-feedback, phantom-affordance, \
misleading-affordance, discoverability, doc-mismatch, crash, other.
Severities: blocker, major, minor, polish.
Verdicts: "defect" (grounded in oracle evidence) or "intended" \
(working as designed; the player was naive — say WHY, citing the \
oracle, e.g. "feedback WAS shown: the event log has 'Notes saved' and \
the frame changed; the player missed it". A missed-but-present cue \
may still deserve a minor discoverability finding — judge it).

The canonical joins (use them, don't parrot them):
- outcome rejected/noop/deadclick + no user-facing event + no visible \
frame change => missing-feedback / silent failure (defect).
- deadclick where the player treated a spot as interactive + no \
widget at that point => phantom-affordance. If a widget IS there but \
the player couldn't find/use it => discoverability.
- a stuck loop (same action, no change, repeatedly) => strong \
missing-feedback signal.
- the player followed the manual and still got stuck => doc-mismatch \
or manual gap.
- player expectation != oracle reality on a SUCCESSFUL action => \
misleading affordance/label.
- an engine crash in the trace => crash (blocker).

Evidence discipline — this is what makes the report trustworthy, and \
it is MECHANICALLY VERIFIED after your reply:
- every finding cites specific turn numbers (including each covered \
candidate's own turn) and the oracle record that grounds it;
- player_quote must be copied VERBATIM from the player's recorded \
words (their note/observation/expectation) for the cited turns — \
never paraphrase inside the quote field. It may be empty ONLY when \
the trace recorded no player words at all for those turns (e.g. a \
crash before the player ever spoke);
- the oracle field must restate the candidate's RECORDED data: \
include at least one verbatim atom from the digest — the outcome \
value and its reason, an event text, or the clicked widget's label — \
for each covered candidate. Naming a harness join tag does NOT count; \
free prose that references nothing recorded is rejected as \
unverifiable. When a candidate has NO outcome/event/widget records, \
state the absence by quoting the digest's oracle-line fragments \
verbatim (e.g. "events=[]", "outcomes=[]", "visual_change=False") \
— never assert facts the record doesn't contain;
- NO ungrounded findings: a hunch without oracle backing is either \
dropped or explicitly confidence=low with the gap named;
- when outcome records are absent (older traces), reason from events \
+ frames + widgets and lower your confidence accordingly.
- merge duplicates: one finding may cover several candidates (e.g. a \
click and its stuck repeats).

Output exactly the JSON schema you were given. Findings should be \
few and sharp, not exhaustive prose.
"""
