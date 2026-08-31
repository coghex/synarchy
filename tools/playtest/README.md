# Naive-player UX playtest harness (H1, #647 / epic #641)

A Python harness that lets a **naive LLM player** play the real game —
perceiving only screenshots, acting only through injected input — and
records everything into a replayable **session trace** for the critic
(H2, #648) to analyze. H1 records; it never analyzes or judges.

> ⚠️ **By default this launches a windowed game instance that takes
> over your screen and steals focus** (F1 screenshots and F2 input
> need a real render pipeline; GPU-less `--headless` cannot host a
> playtest). Run it while away from the machine, on a second
> display/machine — or pass **`--render-mode offscreen`** (P1, #650):
> the same full render + input pipeline drawing to offscreen images,
> no window, no focus steal, safe to run unattended and in parallel
> on distinct ports. The windowed default is the one sanctioned
> exception to the repo's never-launch-graphical rule: the graphical
> instance *is* the system under test.

## Usage

```bash
# Full session with the default Luna player (needs an existing Codex login)
python3 tools/playtest/run.py
python3 tools/playtest/run.py --persona impatient_imogen --dt 3

# Sonnet 5 medium through an existing Claude Code subscription login
python3 tools/playtest/run.py --player claude-sonnet

# Same session, but unattended: windowless offscreen render (#650) —
# no focus steal, and several sessions can run in parallel on
# distinct --port values
python3 tools/playtest/run.py --render-mode offscreen

# Tiny scripted session — exercises the loop/trace/replay plumbing on a
# real windowed instance with no LLM call
python3 tools/playtest/run.py --smoke

# Re-inject a recorded session's inputs against a fresh instance (no LLM)
python3 tools/playtest/run.py --replay tools/playtest/sessions/<dir>

# Offline harness check: no window, no engine build, no API. This is the
# runnable gate for the harness code itself.
python3 tools/playtest/run.py --selftest
```

Defaults: port **9308** (never the GUI's 8008), **12 turns**, 600 seconds
wall-clock, `dt` 2.0 s, a 90-second decision timeout, a **200K**
input-plus-output player-token ceiling, stuck detection after 3 identical
no-change turns, and a 1800-second `--setup-timeout` watchdog for
everything *before* the session starts (see
[Budgets and the player-ready boundary](#budgets-and-the-player-ready-boundary)). `--player` selects one complete audited medium-effort profile:
`codex-luna` (the default, `gpt-5.6-luna`) or `claude-sonnet`
(`claude-sonnet-5`). Arbitrary provider/model/effort strings are not accepted.

After every decision the console shows compact `K`/`M`/`G` values for tokens
used that turn, cumulative player tokens, and remaining session budget. The
harness reserves the observed average cost before starting another turn that
would likely cross the ceiling, and stops if usage is unavailable. One response
can still cross the remaining ceiling because the CLIs report usage only after
it completes. Neither noninteractive CLI exposes a trustworthy whole-plan
remaining-token count, so account remaining is shown as unavailable, not
guessed.

## Budgets and the player-ready boundary

A cold-boot playtest spends real time on things that are not play: on a
fresh worktree `cabal` compiles the whole game, the process then has to
start, and the loading screen has to assemble a main menu. **None of it
is charged to the player.** A run is two halves separated by one
observable boundary (#1539).

**Player-ready** — the boundary — is reached only when all of these hold
at once:

1. the engine process is running and its debug console is reachable
   (it printed `READY`);
2. startup boot has **finished** and the main-menu surface itself is
   initialized (`ui_manager.startupBootDone` and
   `moduleReady.mainMenu`, both set by `finishStartupBoot`), with
   `currentMenu` naming a menu and `ui.registry.dumpWidgets()`
   non-empty; and
3. a screenshot that could really be handed to the player succeeds and
   reports a positive framebuffer size.

Condition 2 is what keeps the **loading screen** out: `currentMenu` is
initialized to `"main"` at module load, long before any UI exists, and
the startup loading screen is shown without changing it — and it carries
visible labels, so `dumpWidgets()` is non-empty there too. A menu name
plus arbitrary visible widgets would hand the player a progress bar as
its first frame.

Elapsed time and fixed sleeps never satisfy it — the old
`time.sleep(3.0)` after boot is gone. Both console reads are ordinary
harness-side **oracle** reads: they are recorded for the critic and
never surfaced to the player, so the oracle-blind contract is unchanged
(it forbids *showing* the player oracle data, not reading it). The
probe's screenshot is written to `setup_ready.png` — a **setup
artifact**, deliberately not `frames/turn_0001.png`: it is never a turn
observation, never shown to the player, and produces no `turns.jsonl`
or `replay.jsonl` record.

**When each budget begins:**

| Budget | Begins |
|---|---|
| `--max-seconds` wall clock | at the player-ready boundary |
| `--turns`, stuck detection (`--stuck-k`) | at the boundary |
| `--decision-timeout`, `--max-player-tokens` + its projected reserve | at the boundary — no player-provider call happens before it |
| lockstep `--dt` advancement | at the boundary |
| `--setup-timeout` | at process start; it **ends** at the boundary |

A setup longer than `--max-seconds` therefore leaves the complete
configured session budget available; the two never overlap.

**Setup watchdogs are not session budgets.** `--setup-timeout` (default
**1800 s**, generous enough for a full cold-worktree build) exists only
so a wedged compiler or engine cannot hang forever. It is independent of
`--max-seconds` and is **not** `probelib`'s 180-second probe `READY`
default — that default, which counted compilation against itself, is what
killed the run this section exists because of, and it remains unchanged
for the ~85 behavior probes that call `probelib.boot` directly. Tripping
the setup watchdog is a setup failure, never `time_budget_exhausted`.

**How setup failures appear.** The pre-ready sequence runs in three named
phases, and a failure names the one it happened in:

| Phase | `stop_reason` | `setup_failure.kind` |
|---|---|---|
| build / preparation | `setup_build_failed` | `build_failed`, `build_timeout` |
| engine process + debug console | `setup_engine_failed` | `engine_exited`, `console_timeout`, `refused_port` |
| rendered-UI readiness | `setup_render_failed` | `render_engine_exited`, `render_timeout` |

Such a run records **zero turns**, writes no `replay.jsonl`, makes no
player-provider call and consumes no player tokens, leaves `loaded_at`
and `session_started_at` null, and exits non-zero. `meta.setup_failure`
carries `{phase, kind, detail, timed_out, stop_reason}`, and
`meta.setup_log_tail` / `meta.engine_log_tail` carry the retained output.
The pre-ready teardown reaps the **whole spawned process group** (the
engine is spawned into its own session) and waits for the listener port
to actually release, so a failed attempt cannot leave an orphan holding
the port and make the next one fail as an unrelated "exited before READY"
(#1190/#1323).

**Which timestamps measure setup versus play** — all four are unix epoch
floats (`time.time()`); budget *enforcement* stays on a monotonic clock:

| Field | Meaning |
|---|---|
| `setup_started_at` | setup began (trace creation) |
| `loaded_at` | the player-ready boundary; **null** if never reached |
| `session_started_at` | the session loop began; **null** if no session ran |
| `ended_at` | the session finished |
| `started_at` | retained, unchanged: the same instant as `setup_started_at` |

Setup duration is `loaded_at - setup_started_at`; the actual player
session is `ended_at - session_started_at`. Traces written before #1539
carry only `started_at`/`ended_at`, and every reader (the usage ledger,
the critic, pre-analysis, `--replay`) still accepts them.

## The lockstep loop

Per turn: **pause → screenshot (F1 `debug.captureScreenshot`) → the
player decides from pixels alone → record the routing oracle → inject
its action (F2 `input.*`) → record the pre-step oracle context →
unpause for a wall-clock `dt` → re-pause → record the post-step oracle
evidence.** Splitting the oracle capture around the step (#775)
matters: the widgets/menu/pause state a click actually acted on has to
be read BEFORE the step (a step can change the UI underneath it), while
the event-log progress, F4 action outcomes, and visible-change
comparison the step itself produces have to be read AFTER it — otherwise they get drained onto the FOLLOWING
turn's pre-step read instead (or, on the session's last turn, lost
outright, since there is no following turn to (mis)capture them). Both
halves are recorded together as one turn's `oracle`. Wall-clock
stepping is a deliberate simplicity tradeoff: `--replay`
replays every recorded turn — pre-step inputs before the `dt` step,
post-step inputs (held-key release) after it, empty turns included —
so the **input sequence and pacing** are faithful, but turns are not
guaranteed bit-identical (deterministic tick-stepping is the noted
escape hatch, not built). The trace records only the phases that
actually executed (#698): a turn that ends the session (`done`, stuck
detection) or is interrupted anywhere between the first injected call
and the post phase records exactly the acknowledged calls (a
multi-call action keeps its successful prefix) and a `step_phase`
distinguishing a step that never began from one that began but was
interrupted from one that fully completed (#728) — replaying a
never-began turn injects no step and no unexecuted call, while an
interrupted turn still replays the step itself (just not its
never-run post calls), since the interruption happened after the
engine had genuinely already advanced.

## The cardinal rule: the player is oracle-blind

The player agent receives **only**: its persona + goal, the minimal
player manual (C1, `docs/player_manual.md`; stubbed if absent), the
current screenshot, and a short rolling memory of its own recent
turns. It never sees widget dumps, event logs, or engine state —
surfacing ground truth would destroy the naive-perception premise.
This is enforced structurally: `agent.build_system_prompt(persona,
manual, fb_size)` has no parameter oracle data could arrive through
(the selftest asserts the signature), and `PlayerAgent.decide()` takes
the screenshot path + memory only. Each decision runs in a fresh empty
directory. Codex uses ephemeral `codex exec` with its information-acquiring
tools disabled. Claude Code uses safe mode, no persisted session, no MCP or
skills, and only `Read`; the directory contains only a copy of the screenshot.
Neither profile can inspect the repository or acquire other context. The
**critic** (H2) reads the oracle from the trace instead.

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
| `{"do":"scroll","dy"[,"dx","x","y"]}` | optional `input.moveMouse` + one `input.scroll` |
| `{"do":"key","name"[,"mods"]}` | `input.key` |
| `{"do":"hold","name"}` | `input.keyDown` before the step, `input.keyUp` after it (camera pan rides the unpaused `dt`) |
| `{"do":"type","text"}` | `input.type` |
| `{"do":"wait"}` | nothing — watch time pass |
| `{"do":"done","reason"}` | nothing — player claims the goal; session ends |

`dy` is the one action parameter with a published range, because it is
the one whose sign the player cannot infer from the gesture (#1980). It
is measured in **wheel notches** — one notch is `1` — and its polarity is
the *camera's*: **negative `dy` zooms in, toward the ground**, positive
zooms out. That is `Engine.Loop.Camera`'s own convention
(`scrollZoomImpulse zoom dy = zoomScrollScale * zoom * dy` over a
half-height `camZoom` whose `zoomMin` is the closest zoom), and
`--selftest` re-derives it from that checked-in source and compares it
with the rendered player prompt, so the two cannot drift.

The range is `[-10, 10]` inclusive, fractions included. Because the
impulse multiplies by the *current* zoom, one delta is far more violent
from the whole-world view than from near the ground, so the bound is what
keeps a single turn to a short multi-notch correction. It is enforced in
`translate_action`, not in the structured schema alone, since a scripted
agent and a lenient provider fallback both reach that boundary without a
schema having validated them:

- a finite `dy` outside the range is **clamped** to the nearest bound —
  the turn keeps the action the player requested, its note records the
  requested and effective values, and only the bounded call reaches
  `injected`/`replay.jsonl`;
- a non-finite `dy` is **rejected** — there is no nearest bound to clamp
  it to, so the turn injects no scroll call and its note says so.

Either way the turn still crosses exactly one observable action boundary.
`dx` keeps its historical verbatim forwarding: the camera premise and the
notch vocabulary are about `dy` alone.

## Personas

A persona is a small structured YAML/JSON blob (see
`personas.py` for the schema): `name`, `temperament`, `goal`,
`tendencies[]`, optional `prose`. Three hardcoded placeholders ship in
`personas/` (`curious_carl`, `impatient_imogen`, `methodical_mara`) so
H1 runs standalone; generated personas (below) are passed by path
(`--persona path/to/file.yaml`). `--goal` overrides the persona's goal
for one session.

### Persona generation (C2, #649)

`personas.py` also *generates* personas: `generate_persona(seed)` is a
pure function of a seed — it samples one value per behavioral axis
(experience / patience / reads_guidance / play_style / persistence,
plus the session `goal`) from **`personas/axes.yaml`** and assembles
the H1 fields from each value's tendency/blurb data. Same seed →
identical spec, so the H1 trace's recorded persona regenerates exactly
on replay. Adding persona variety is data-editing in `axes.yaml`, not
code. Generated specs carry extra provenance fields (`seed`, `axes`,
`sampling`) that H1 ignores.

```bash
python3 tools/playtest/personas.py --seed 42            # preview one
python3 tools/playtest/personas.py --seed 42 --count 5  # seeds 42..46
python3 tools/playtest/personas.py --coverage --count 12  # balanced spread
python3 tools/playtest/personas.py --seed 42 --out DIR  # write files
python3 tools/playtest/personas.py --seed 42 --llm      # LLM-flavored blurb
python3 tools/playtest/personas.py --selftest           # offline check
```

Two sampling modes: **seeded-random** (default — cheap, varied,
reproducible per seed) and **`--coverage`** (a balanced
Latin-hypercube-style spread across the axis space, reproducible from
`(seed, count)`, so a campaign deliberately spans combinations instead
of clustering). The default blurb is a deterministic template;
**`--llm`** rewrites the name + blurb with a cheap model
(`claude-haiku-4-5` by default, `--model` to change — needs an
Anthropic key). LLM prose is **frozen into the spec at generation
time** — files, the H1 trace, and replay always reuse the stored text,
never regenerate — so the prose can't drift between runs while the
axes/goal/tendencies stay seed-deterministic regardless.

## Session trace format (what H2 consumes)

One directory per session (default `tools/playtest/sessions/<ts>_<persona>/`,
gitignored):

- `meta.json` — persona, goal, player model + settings, `dt`, budgets,
  harness version, the four lifecycle timestamps
  (`setup_started_at` / `loaded_at` / `session_started_at` / `ended_at`,
  plus the retained `started_at`), `stop_reason`
  (`goal_reached_claimed` / `turn_budget_exhausted` /
  `time_budget_exhausted` / `decision_timeout` /
  `token_budget_reserved` / `token_budget_exhausted` /
  `usage_unavailable` / `stuck_loop` / `engine_crash` / `interrupted`,
  or one of the three pre-ready SETUP outcomes `setup_build_failed` /
  `setup_engine_failed` / `setup_render_failed`, #1539),
  cumulative `usage_totals` (input + output), crash detail + engine log tail
  when applicable, and `setup_failure` + `setup_log_tail` on a pre-ready
  failure.
- `turns.jsonl` — per turn: screenshot path, the player's structured
  output (observation/action/expectation/note + raw + token usage),
  the exact injected `input.*` calls and their acks (**executed calls
  only**, post-step acks retained; `post_injected` counts the trailing
  post-step entries and `step_phase` is `"not_started"` /
  `"interrupted"` / `"completed"` — whether the unpause-dt-repause sim
  step never began, began but was interrupted before repause
  confirmed, or fully completed, #698/#728), and the **oracle** record
  (`ui.dumpWidgets`, `engine.getEventLog` delta, current menu, pause
  state), flagged `player_invisible: true`. Since #775, the oracle is
  assembled from three reads: `routing_widgets` (#1750) is a
  `ui.dumpWidgets` read taken BEFORE the turn's first input call, so
  the offline click join correlates against the record set the real
  pointer router resolved the click against — a callback that opens,
  closes or replaces a modal, or that creates/destroys elements, can no
  longer rewrite it; `widgets`/`current_menu`/`paused`/
  `world_seed` are the PRE-step affordance context (the state the
  player actually acted on, read once after inject+settle) and keep
  that later sampling point, because the digest and the seed promotion
  consume them with exactly that meaning;
  `event_log_new`/`event_log_gaps`/`action_outcomes` are the union of
  that same pre-step read (whatever the action produced synchronously,
  while still paused) and — when the turn's sim step actually ran — a
  SECOND read taken right after it (whatever the unpaused `dt` interval
  itself produced), both credited to this turn's action rather than
  the next turn's pre-step read. `event_log_new` is driven by the
  store-assigned `sequence` every `engine.getEventLog()` row carries
  (#1714) — a positive integer taken consecutively from 1 in mutation
  commit order — so byte-identical rows are distinguishable and each
  retained mutation is reported exactly once. The ring is bounded, so
  a mutation can be committed and then evicted (or superseded by a
  coalesced replacement) before the oracle reads it; every such
  sequence is reported in `event_log_gaps` as a maximal missing
  interval `{"first_sequence", "last_sequence", "missing_count"}`.
  A gap object asserts ABSENCE only and never a cause — eviction, a
  coalesce, and a load-publish reset are indistinguishable from the
  snapshot. The intervals run up to the store's own
  high-water mark, taken from the SAME engine-side snapshot as the rows
  (`engine.getEventLogProgress()` — one verb, one `readTVarIO`, so no
  emitter can commit between the two halves), not up to the newest
  surviving row: a load publish empties the ring without resetting the
  counter, so the rows alone would report a whole discarded interval as
  no change at all.
  The first observation is an explicit baseline (the whole current log,
  no gap, adopting the high-water mark as its cursor), and the cursor
  never moves backwards. The read is strict end to end: a reply that is
  not a `{rows, highest}` table, a row without a usable `sequence`, and
  a missing or unusable high-water mark are each hard errors — never a
  fall back to matching rows by value, and never an empty observation
  that would leave the cursor untouched and erase the turn's evidence. `visual_change` (bool) and
  `post_screenshot` (path, or null) are this turn's own before/after
  comparison and post-step frame — populated only when a step ran, so
  never for a `done`/stuck terminal turn, but always for an ordinary
  turn INCLUDING the session's last one (previously lost outright, for
  want of a following turn to capture it on). A null `post_screenshot`
  on a `"completed"` turn means the step ran but its frame could not be
  taken (#1752): the turn's own drained events and outcomes are merged
  before the screenshot is attempted, so a screenshot crash states the
  missing frame rather than silently dropping evidence the runner had
  already consumed.
- `replay.jsonl` — one line **per turn** (no-input turns included, so
  replay pacing is faithful): `{"turn": N, "pre": [lua...], "post":
  [lua...], "step_phase": "not_started"|"interrupted"|"completed"}` —
  `pre` is injected before the sim step, `post` after it (a held key's
  `keyUp` rides `post`); only calls that actually ran are recorded. A
  `"not_started"` turn (done/stuck, or interrupted before a successful
  unpause) replays without a step or post calls. An `"interrupted"`
  turn (unpause succeeded live but the step didn't finish cleanly)
  still replays one full unpause-dt-repause step — the interruption
  itself isn't reproduced — but never replays post calls, since those
  never ran live either; only `"completed"` replays them too (#698,
  superseded by #728's tri-state field — a legacy trace's missing or
  boolean `stepped` field loads with the historical mapping:
  missing/`true` → `"completed"`, `false` → `"not_started"`).
- `frames/turn_NNNN.png` — the F1 captures, plus `turn_NNNN_post.png`
  (#775) for every turn whose sim step actually ran — that turn's own
  post-step frame, never a following turn's.
- `engine.log` — engine output, copied at session end (an engine crash
  mid-session is a **finding**: the partial trace + logs are retained
  and `stop_reason` is `engine_crash`).
- `setup.log` — build/preparation output (#1539), written live during the
  pre-ready `build` phase at cabal's normal verbosity. Unlike the engine
  log it survives a failure that happens *before the executable ever
  starts*, which used to leave the trace with nothing but an unexplained
  zero-byte `engine.log`.
- `setup_ready.png` — the player-ready probe's screenshot (#1539). A setup
  artifact, not under `frames/` and never numbered as a turn.
- `inspection-plan.json` — deterministic, LLM-free selection of bookends and
  turns implicated by notes, bad outcomes, stuck detection, or a crash. It is
  an inspection queue, not a verdict; listed images still need direct review.

Notes on trace contents:
- **World seed:** `world.getSeed()` (added with this harness) is
  polled in every turn's pre-step oracle context; the first non-null
  value — the seed the player actually got, typed or randomized — is
  promoted into `meta.world_seed`. `--replay` **pins that seed**: until
  the replayed world exists, it forces the recorded seed into the
  create-world form state the Generate button reads (the same field
  the seed box's onChange writes, so typed-seed sessions replay
  identically too), so randomized-seed sessions rebuild the same
  world. `replay_seed_match` is recorded as a verification backstop,
  with a warning if the replay diverged before world creation.
- **F4 outcomes** (#646): each turn's oracle includes `action_outcomes`
  — the union of draining `debug.drainActionOutcomes()` once after
  inject+settle and, when the turn stepped, again right after (#775) —
  a destructive read, like `combat.drainEvents`; no "_seen" index
  needed, since each drain's records are disjoint from the next.
  `meta.f4_outcomes_total` is a running count across the whole
  session, for a quick session-level glance without walking every
  turn's oracle.
- **Scenario-jump** (pre-set mid-game state) is explicitly out of
  scope for H1 — cold-boot only. The trace/runner leave room for it
  (a future mode would only add setup calls before turn 1 and a
  `meta.scenario` field).

## Stop conditions

Every reason below is a **player-session** outcome and can only be reached
after the [player-ready boundary](#budgets-and-the-player-ready-boundary).
A failure before it stops the run with one of the three SETUP reasons
instead (`setup_build_failed` / `setup_engine_failed` /
`setup_render_failed`) and zero turns.

Turn budget (`--turns`), wall-clock budget (`--max-seconds`), decision timeout,
player-token budget (`--max-player-tokens`), missing provider usage, the player
claiming its goal (`done`), or a **stuck loop**: the same action with
byte-identical frames `--stuck-k` times in a row. A
repeat-with-no-change loop is itself a strong missing-feedback signal —
it is recorded on the turn (`stuck: true`) before the session ends.

Completed model sessions also rebuild a local, unversioned Markdown ledger at
the shared Git directory's `codex-test/playtest-usage.md`. It scans durable
`codex-test/artifacts/` traces and records date, run, player, turns, stop reason,
compact token use, and budget. `--usage-log` selects another output destination;
artifact discovery remains anchored to the shared Git directory, so a custom
destination does not discard historical rows.

## The critic (H2, #648)

`critic.py` is the analysis half: it consumes a session trace
**offline** (never drives the game) and emits `report.md` +
`findings.json` into the trace directory.

```bash
python3 tools/playtest/critic.py tools/playtest/sessions/<dir>
python3 tools/playtest/critic.py <dir> --model claude-opus-5 --effort high
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

For a default or explicit LEFT click, that click-to-control join is
routing-aware (#1750): it reproduces `UI.InputOwnership.routePointer`
from engine-owned facts the dump now carries per record —
`inScope` (the modal-scope decision, so empty exclusive-modal space
never correlates a HUD control below it), effective `pointerBlocking`
(so a callback-less blocking panel suppresses every lower control
instead of the join falling through to one the router could not
reach), and `leftClickTarget`/`leftClickAffordance` (which separate an
ACTIVE left target from a right-click-only blocker and from #783's
shown-but-disabled affordance, the one case that still correlates to
itself). Right/middle clicks and drags keep the older
`(paintKey, paintOrder)` topmost-eligible join, and so does any trace
recorded before those fields existed.
Adjudication (default `claude-opus-5`, high effort — cost is
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
It also enforces **one verdict per candidate** (#1873): findings are
indexed back by candidate identity, agreeing duplicates publish with a
warning, and opposite verdicts on one candidate are withdrawn from
both claimants — candidate-scoped, so a finding consolidating several
candidates keeps its uncontested ones — which makes that candidate
uncovered and sends it through the same bounded repair pass. A
conflict the repair round repeats ends as an honest warning naming the
candidate and the competing verdicts, never as two published verdicts.
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
the acceptance run for a key-holder. The fixture's oracle snapshots
carry live-shaped F4 records under `action_outcomes` (#646, shipped —
see the per-turn field above), so the same rejected-outcome candidate
assertion protects the real critic read path; a live trace whose
`action_outcomes` is empty lowers the critic's grounding confidence
rather than breaking it.

Cross-session aggregation (same spot tripping N personas) is a
deliberate follow-up, not built here — the single-session path stays
clean and additive.

## Testing

- `python3 tools/playtest/run.py --selftest` — offline, CI-safe check
  of the loop, trace write, replay, stuck detection, trace phase
  fidelity (#698: terminal/stuck/interrupted turns record and replay
  without an invented step or post call), the setup/session split
  (#1539: a setup longer than `--max-seconds` still hands the session
  its whole budget, no player call precedes the boundary, readiness
  needs positive rendered/UI evidence rather than elapsed time, each
  pre-ready phase's failure records zero turns and phase-specific
  diagnostics with null boundary stamps, lifecycle metadata is
  chronological, the group reap and port wait really happen, and
  pre-#1539 traces still load), and the oracle-blind prompt
  shape plus both pinned medium-effort provider invocations, normalized usage,
  and projected token reserve (FakeEngine + scripted agent; no window, no
  build, no model call).
- `python3 tools/playtest/run.py --smoke` — few-turn scripted session
  against a real instance (windowed by default; add
  `--render-mode offscreen` for the windowless #650 substrate —
  verifies F1/F2/F3 wiring end to end either way).
- A real LLM session is the acceptance run; needs a GPU and the selected CLI's
  existing login (plus focus only in windowed mode).
- `python3 tools/playtest/critic.py --selftest` — offline critic
  pipeline check (canned trace, fake critic, no key);
  `--eval` is the real-model acceptance run against the planted trace.
- `python3 tools/playtest/personas.py --selftest` — offline generator
  check (C2): same-seed determinism, H1 schema conformance (through
  `load_persona` + real prompt assembly), axis coverage over many
  seeds, coverage-mode balance/reproducibility, and the LLM-blurb
  freeze contract via an injected fake completer (no key). A real
  `--llm` run is the key-holder acceptance for the flavor path.
