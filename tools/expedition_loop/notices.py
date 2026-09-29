#!/usr/bin/env python3
"""Player-notification evidence retained across the whole session (#2640).

The confrontation leg's exactly-once claims — one discovery event, one
aggression notice, one clearance notice for the occupied ruin — are
claims about a BOUNDED stream (aggression is once per EPISODE; see
`encounter.reward`). `Engine.PlayerEvent.Emit.pushBounded`
keeps at most `eventStoreCap` rows, retires a row's sequence when an
identical repeat coalesces onto the tail, and a load publish empties the
ring outright. Counting matches in one final `engine.getEventLog()`
snapshot therefore proves nothing about rows that were committed and
then pushed out, and a count of one could be the survivor of two.

`EventLedger` is the instrument that closes that gap. It polls
`engine.getEventLogProgress()` — the rows AND the store's own committed
high-water mark, taken from one snapshot (#1714) — and:

  * retains every row it has ever seen, keyed by its `sequence`, so an
    observation made early in the session still counts after the ring
    has moved on;
  * deduplicates by sequence, so polling the same row twice is one row;
  * reports every committed sequence it never saw as a MISSING interval.

A missing interval is not automatically a loss. Within one process the
ring only ever LOSES rows through front eviction, and eviction happens
only once the ring is full; below capacity the only other thing that
retires a sequence is a coalesced replacement, and that replacement row
is itself retained here with its `count` bumped. So an interval is
`unexplained` — possibly a lost row — exactly when it was observed with
the ring at capacity. The exactly-once checks require no unexplained
interval, rather than hoping the poll was fast enough.

A library like the rest of the package: it asserts nothing and knows no
stage. Owners poll it while they drive the session and ask it questions
when they check.
"""
from __future__ import annotations

from probelib import send, send_json

#: `Engine.PlayerEvent.eventStoreCap`. Below this many retained rows no
#: front eviction can have happened in this process.
EVENT_STORE_CAP = 1000


class EventLedger:
    """Every event-log row the session has shown this probe, by sequence."""

    def __init__(self) -> None:
        self.rows: dict[int, dict] = {}
        self.cursor: int | None = None
        #: Missing intervals, each (first, last, ring_size_when_seen).
        self.missing: list[tuple[int, int, int]] = []
        self.polls = 0

    def poll(self, port: int) -> None:
        snap = send_json(port, "return engine.getEventLogProgress()")
        if not isinstance(snap, dict):
            return
        rows = snap.get("rows")
        rows = rows if isinstance(rows, list) else []
        highest = snap.get("highest")
        if not isinstance(highest, int) or isinstance(highest, bool):
            return
        self.polls += 1
        present = set()
        for row in rows:
            seq = row.get("sequence") if isinstance(row, dict) else None
            if isinstance(seq, int) and not isinstance(seq, bool):
                present.add(seq)
                self.rows[seq] = row
        if self.cursor is not None:
            expected = self.cursor + 1
            for seq in sorted(s for s in present if s > self.cursor):
                if seq > expected:
                    self.missing.append((expected, seq - 1, len(rows)))
                expected = seq + 1
            if highest >= expected:
                self.missing.append((expected, highest, len(rows)))
        self.cursor = max([highest] + list(present)
                          + ([self.cursor] if self.cursor is not None else []))

    def unexplained(self) -> list[tuple[int, int, int]]:
        """Missing intervals seen with the ring full — the only ones a
        front eviction could account for, so the only ones that could
        hide a row this probe needed."""
        return [m for m in self.missing if m[2] >= EVENT_STORE_CAP]

    def matching(self, pred) -> list[dict]:
        """Every retained row satisfying `pred`, oldest first."""
        return [self.rows[s] for s in sorted(self.rows) if pred(self.rows[s])]

    def emissions(self, pred) -> int:
        """A lower bound on how many times a matching notice was EMITTED.

        Every committed emit takes its own sequence — a plain append and
        a coalesced replacement alike — so each distinct sequence seen
        is one emit. A coalesced replacement also carries the running
        `count`, which still reveals an earlier emit whose own row was
        retired before any poll saw it. Per coalescing key the larger of
        the two is taken; the answer is exactly 1 only for a single row
        with count 1."""
        seqs: dict[tuple, int] = {}
        counts: dict[tuple, int] = {}
        for row in self.matching(pred):
            key = (row.get("category"), row.get("text"), row.get("uid"),
                   row.get("page"))
            seqs[key] = seqs.get(key, 0) + 1
            counts[key] = max(counts.get(key, 0), int(row.get("count") or 1))
        return sum(max(seqs[k], counts[k]) for k in seqs)


# --------------------------------------------------------------------------
# Tutorial latch ORDER, recorded at the one write boundary it passes
# --------------------------------------------------------------------------
#: Wraps `tutorial_progress.completeObjectives` — the single surface
#: `scripts/tutorial_eval.lua`'s evaluation pass publishes one batch of
#: latches through — so every newly latched id is recorded with the
#: number of the evaluation pass that latched it. Observation only: the
#: wrapper returns exactly what the original returns and changes no
#: argument, and the record lives on the probe's own keys rather than in
#: anything the component snapshots. Installed once per engine, before
#: anything in its world can latch.
#:
#: It answers two questions:
#:   * ORDER, in engine A. The four trip objectives can latch as early as
#:     the zero-occupant leg and several can latch in ONE pass (the #2640
#:     review correction), so a probe poll a second apart could neither
#:     see the order nor tell "same pass" from "one poll late".
#:   * PROVENANCE, in engine B. The save component's apply() writes the
#:     completed set directly and never calls this surface, while any
#:     evaluator recomputation must. A latch that is present after the
#:     load but absent from the fresh engine's record was RESTORED. (The
#:     evaluator cannot simply be held back to prove it: a headless boot
#:     already runs `scripts/init_loader.lua`, which loads it ticking.)
LATCH_RECORDER_LUA = (
    "local TP = require('scripts.tutorial_progress'); "
    "if TP._probeLatchWrapper ~= nil "
    "   and TP.completeObjectives == TP._probeLatchWrapper then "
    "  return 'already' end; "
    "TP._probeLatchLog = {}; TP._probeLatchPass = 0; "
    "local original = TP.completeObjectives; "
    "TP._probeLatchWrapper = function(ids) "
    "  TP._probeLatchPass = TP._probeLatchPass + 1; "
    "  local newly = original(ids); "
    "  for _, id in ipairs(newly or {}) do "
    "    TP._probeLatchLog[#TP._probeLatchLog + 1] = "
    "      id .. '@' .. TP._probeLatchPass end; "
    "  return newly end; "
    "TP.completeObjectives = TP._probeLatchWrapper; "
    "return 'installed'"
)


def install_latch_recorder(port: int) -> str:
    return send(port, LATCH_RECORDER_LUA, timeout=15.0)


def latch_passes(port: int) -> dict[str, int]:
    """Objective id -> the evaluation pass that first latched it."""
    raw = send(port, "local TP = require('scripts.tutorial_progress'); "
                     "return table.concat(TP._probeLatchLog or {}, ',')",
               timeout=15.0).strip().strip('"')
    out: dict[str, int] = {}
    for part in raw.split(","):
        ident, _, n = part.partition("@")
        if ident and n.isdigit():
            out.setdefault(ident, int(n))
    return out
