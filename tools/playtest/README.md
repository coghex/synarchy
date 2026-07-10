# Naive-player UX playtest harness (H1, #647 / epic #641)

A Python harness that lets a **naive LLM player** play the real game —
perceiving only screenshots, acting only through injected input — and
records everything into a replayable **session trace** for the critic
(H2, #648) to analyze. H1 records; it never analyzes or judges.

> ⚠️ **This launches a windowed game instance that takes over your
> screen and steals focus** (F1 screenshots and F2 input need a real
> render pipeline; GPU-less `--headless` cannot host a playtest). Run
> it while away from the machine or on a second display/machine. This
> is the one sanctioned exception to the repo's never-launch-graphical
> rule: the graphical instance *is* the system under test. Offscreen
> unattended mode is P1 (#650).

## Usage

```bash
# Full session with the naive LLM player (needs ANTHROPIC_API_KEY or an
# `ant auth login` profile, and `pip install anthropic`)
python3 tools/playtest/run.py
python3 tools/playtest/run.py --persona impatient_imogen --turns 60 --dt 3

# Tiny scripted session — exercises the loop/trace/replay plumbing on a
# real windowed instance with no LLM call
python3 tools/playtest/run.py --smoke

# Re-inject a recorded session's inputs against a fresh instance (no LLM)
python3 tools/playtest/run.py --replay tools/playtest/sessions/<dir>

# Offline harness check: no window, no engine build, no API. This is the
# runnable gate for the harness code itself.
python3 tools/playtest/run.py --selftest
```

Defaults: port **9308** (never the GUI's 8008), 40 turns, `dt` 2.0 s,
model `claude-sonnet-5` at low effort with thinking disabled (cheap and
naive is the point — a #641 design decision), stuck detection after 3
identical no-change turns. `--help` lists everything.

## The lockstep loop

Per turn: **pause → screenshot (F1 `debug.captureScreenshot`) → the
player decides from pixels alone → inject its action (F2 `input.*`) →
record the oracle snapshot → unpause for a wall-clock `dt` → re-pause.**
Wall-clock stepping is a deliberate simplicity tradeoff: `--replay`
replays every recorded turn — pre-step inputs before the `dt` step,
post-step inputs (held-key release) after it, empty turns included —
so the **input sequence and pacing** are faithful, but turns are not
guaranteed bit-identical (deterministic tick-stepping is the noted
escape hatch, not built).

## The cardinal rule: the player is oracle-blind

The player agent receives **only**: its persona + goal, the minimal
player manual (C1, `docs/player_manual.md`; stubbed if absent), the
current screenshot, and a short rolling memory of its own recent
turns. It never sees widget dumps, event logs, or engine state —
surfacing ground truth would destroy the naive-perception premise.
This is enforced structurally: `agent.build_system_prompt(persona,
manual, fb_size)` has no parameter oracle data could arrive through
(the selftest asserts the signature), and `PlayerAgent.decide()` takes
the screenshot path + memory only. The **critic** (H2) reads the
oracle from the trace instead.

The prompt casts the model as a *confused new player narrating their
experience and taking notes* — explicitly not a QA tester. Per turn it
returns `observation` / `action` / `expectation` / `note`; the
expectation-vs-oracle diff is H2's gold.

## Action vocabulary & coordinates

Actions are in **screenshot pixel space** — exactly the framebuffer
space F1 reports and F2 accepts, so a click at screenshot pixel (x, y)
lands on the thing drawn there. No coordinate grids or widget-label
overlays are composited onto the screenshot (considered and rejected:
misclicks are wanted naive-behavior signal). The harness only clamps
coordinates into the frame.

| Agent action | Injected as |
|---|---|
| `{"do":"click","x","y"[,"button","mods"]}` | `input.click` |
| `{"do":"drag","x1","y1","x2","y2"[,"button"]}` | `input.mouseDown` → `moveMouse` (midpoint, end) → `input.mouseUp` |
| `{"do":"scroll","dy"[,"dx","x","y"]}` | optional `input.moveMouse` + `input.scroll` |
| `{"do":"key","name"[,"mods"]}` | `input.key` |
| `{"do":"hold","name"}` | `input.keyDown` before the step, `input.keyUp` after it (camera pan rides the unpaused `dt`) |
| `{"do":"type","text"}` | `input.type` |
| `{"do":"wait"}` | nothing — watch time pass |
| `{"do":"done","reason"}` | nothing — player claims the goal; session ends |

## Personas (the C2 contract)

A persona is a small structured YAML/JSON blob (see
`personas.py` for the schema): `name`, `temperament`, `goal`,
`tendencies[]`, optional `prose`. Three hardcoded placeholders ship in
`personas/` (`curious_carl`, `impatient_imogen`, `methodical_mara`) so
H1 runs standalone; C2 (#649) generates conforming files and passes
them by path (`--persona path/to/file.yaml`). `--goal` overrides the
persona's goal for one session.

## Session trace format (what H2 consumes)

One directory per session (default `tools/playtest/sessions/<ts>_<persona>/`,
gitignored):

- `meta.json` — persona, goal, player model + settings, `dt`, budgets,
  harness version, timestamps, `stop_reason`
  (`goal_reached_claimed` / `turn_budget_exhausted` /
  `time_budget_exhausted` / `stuck_loop` / `engine_crash` /
  `interrupted`), crash detail + engine log tail when applicable.
- `turns.jsonl` — per turn: screenshot path, the player's structured
  output (observation/action/expectation/note + raw + token usage),
  the exact injected `input.*` calls and their acks, and the **oracle
  snapshot** (`ui.dumpWidgets`, `engine.getEventLog` delta, current
  menu, pause state), flagged `player_invisible: true`.
- `replay.jsonl` — one line **per turn** (no-input turns included, so
  replay pacing is faithful): `{"turn": N, "pre": [lua...], "post":
  [lua...]}` — `pre` is injected before the sim step, `post` after it
  (a held key's `keyUp` rides `post`).
- `frames/turn_NNNN.png` — the F1 captures.
- `engine.log` — engine output, copied at session end (an engine crash
  mid-session is a **finding**: the partial trace + logs are retained
  and `stop_reason` is `engine_crash`).

Notes on trace contents:
- **World seed:** `world.getSeed()` (added with this harness) is
  polled in every oracle snapshot; the first non-null value — the seed
  the player actually got, typed or randomized — is promoted into
  `meta.world_seed`. `--replay` **pins that seed**: until the replayed
  world exists, it forces the recorded seed into the create-world
  form state the Generate button reads (the same field the seed box's
  onChange writes, so typed-seed sessions replay identically too), so
  randomized-seed sessions rebuild the same world. `replay_seed_match`
  is recorded as a verification backstop, with a warning if the replay
  diverged before world creation.
- **F4 outcomes** (#646): each turn's oracle snapshot includes
  `action_outcomes` — the result of draining `debug.drainActionOutcomes()`
  since the last turn (a destructive read, like `combat.drainEvents`; no
  "_seen" index needed). `meta.f4_outcomes_total` is a running count
  across the whole session, for a quick session-level glance without
  walking every turn's oracle.
- **Scenario-jump** (pre-set mid-game state) is explicitly out of
  scope for H1 — cold-boot only. The trace/runner leave room for it
  (a future mode would only add setup calls before turn 1 and a
  `meta.scenario` field).

## Stop conditions

Turn budget (`--turns`), wall-clock budget (`--max-seconds`), the
player claiming its goal (`done`), or a **stuck loop**: the same action
with byte-identical frames `--stuck-k` times in a row. A
repeat-with-no-change loop is itself a strong missing-feedback signal —
it is recorded on the turn (`stuck: true`) before the session ends.

## The critic (H2, #648)

`critic.py` is the analysis half: it consumes a session trace
**offline** (never drives the game) and emits `report.md` +
`findings.json` into the trace directory.

```bash
python3 tools/playtest/critic.py tools/playtest/sessions/<dir>
python3 tools/playtest/critic.py <dir> --model claude-opus-4-8 --effort high
python3 tools/playtest/critic.py --selftest   # offline, no API key
python3 tools/playtest/critic.py --eval       # REAL model vs the canned
                                              # planted-issue trace (needs a key)
```

Mechanism: a deterministic pre-analysis derives per-turn signals and
the canonical cross-source joins (action-outcome `rejected`/`noop`/
`deadclick` + no event + no frame change ⇒ silent-failure candidate;
click-hit-no-widget ⇒ phantom-affordance; player-claims-nothing-
happened while the oracle shows feedback ⇒ feedback-was-shown; stuck
loops; crash) and enumerates **friction candidates** with stable ids.
Adjudication (default `claude-opus-4-8`, high effort — cost is
per-session, not per-turn) is **batched** so that every candidate's
own screenshot is actually shown in the call that judges it:
`--max-frames` is a per-call budget, and a tight budget means more
calls, never an unseen candidate frame. The critic judges against the
player manual as the intended mental model. Validation enforces the
enums and **evidence-disciplined coverage**: a finding only counts for
a candidate if it cites real trace turns including that candidate's
turn, an oracle record, the player's own words when the friction came
from a note, and only frames its call was actually shown (a bounded
repair pass re-adjudicates anything stripped, then honest warnings).
Findings attach only screenshots their call saw; `findings.json`
embeds the full candidate list and a per-call audit
(`adjudication_calls`: which candidates, which frames) so nothing is
silently dropped or overstated.

Testing: `canned_trace.py` builds a synthetic trace with planted
issues — a genuine silent failure (outcome rejected, no event, no
visual change), a working-as-designed case the player merely missed
(event fired + frame changed), and a phantom affordance. `--selftest`
(offline) asserts the joins land on the planted turns and runs the
whole pipeline against a deterministic fake critic; `--eval` runs the
REAL model against the same trace and asserts the planted silent
failure comes back a missing-feedback **defect** and the missed-
feedback case comes back **intended**/minor-discoverability — that is
the acceptance run for a key-holder. The F4-shaped `outcomes` records
in the fixture prove the (still-unshipped, #646) outcome-tap path;
live traces without them lower the critic's grounding confidence
rather than breaking it.

Cross-session aggregation (same spot tripping N personas) is a
deliberate follow-up, not built here — the single-session path stays
clean and additive.

## Testing

- `python3 tools/playtest/run.py --selftest` — offline, CI-safe check
  of the loop, trace write, replay, stuck detection, and the
  oracle-blind prompt shape (FakeEngine + scripted agent; no window,
  no build, no API key).
- `python3 tools/playtest/run.py --smoke` — few-turn scripted session
  against a real **windowed** instance (human-run; verifies F1/F2/F3
  wiring end to end).
- A real LLM session is the acceptance run; needs GPU + focus and an
  API key.
- `python3 tools/playtest/critic.py --selftest` — offline critic
  pipeline check (canned trace, fake critic, no key);
  `--eval` is the real-model acceptance run against the planted trace.
