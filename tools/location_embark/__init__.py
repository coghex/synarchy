"""Phase owners behind `tools/location_embark_probe.py` (#2164).

The probe is still one executable, one registration and one aggregate
result: what moved here is WHO owns each contract it checks, so the
integrated embark-to-discovery chain no longer lives in a single
1,500-line file.

  * `constants` — the facts every owner shares: the world page the
    fixture is generated on, the two save slots that carry state from
    one session to the next, and the portal and ruin identifiers the
    checks are written against.
  * `invocation` — everything that is not a scenario assertion: the
    aggregate failure ledger and the engine-log context a failing check
    quotes, the single artifact directory this invocation owns and its
    isolated resource root (#1569), the release that removes the whole
    tree again, the request-specific save publication both slots go
    through (#1746), and the `SessionContext` the facade threads from
    one session owner to the next.
  * `support` — the engine reads and real-input gestures more than one
    session needs: locations, sight, the event log, screenshot
    luminance, the deterministic coordinate searches, and the real
    click-select / right-click move-order path (#1770).
  * `fixture` — phase 0: the fallback-seed search, each candidate
    generated and discarded with its own headless process, and the
    durable `SAVE_BASE` every later session loads.
  * `session_ghost` — session (a): the shared unknown markers before any
    portal exists, ghost validity, and both branches of the
    remote-settlement modal.
  * `session_discovery` — session (b): local placement, the portal
    roster, discovery driven by a real move order, the no-duplicate
    re-entry check, the lifecycle-icon comparisons, and `SAVE_LOCAL`.
  * `session_reload` — session (c): the fresh process that loads
    `SAVE_LOCAL` and proves the location count, the discovery state and
    the restored icons survived.

No module here parses a command line, allocates an artifact root, opens
a port, registers a probe, or has a runnable entry point:
`location_embark_probe.run_probe` owns the ordered process lifecycle —
phase 0's own seed retries, then one boot and one shutdown per session —
and hands each owner the port it opened.
"""
