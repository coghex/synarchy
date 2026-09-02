"""Click correlation for the playtest critic (#783/#749/#1750,
extracted by #2069): which control, if any, a recorded click landed on.

This is the oracle-side reproduction of the engine's own pointer
routing, and the F3 join that tells a phantom affordance (the click hit
NO widget) from a discoverability problem (a widget WAS there). It owns:

  * `widget_at` — the topmost eligible control containing a point, on
    `interactiveBounds` when the record carries it (#749) and content
    `bounds` only for records that lack it; explicitly non-hittable and
    degenerate bounds are rejected; hidden and passive (`control=False`)
    records are never the correlated control; a shown-but-disabled
    affordance stays correlatable (#783);
  * the legacy `(paintKey, paintOrder)` winner for old traces, drags
    and non-left clicks, and the route-aware pass (#1750) reproducing
    `UI.InputOwnership.routePointer`'s modal scope, pointer occlusion,
    active-target and disabled-affordance precedence for left clicks;
  * `routing_aware_records` — the explicit, presence-based detection of
    the routing facts that decides which of those two joins applies;
  * `is_left_click` — the action classification the route-aware
    contract covers.

A leaf: pure functions over dump records, importing nothing from the
rest of the harness. `critic_signals.build_signals` is the production
caller; `scripts/ui/registry.lua` is the producer of the records, and
`Test.Headless.UI.InteractiveBounds` reads THIS file to prove the join
prefers `interactiveBounds`.
"""
from __future__ import annotations


def widget_at(widgets, x, y, route_aware=False):
    """The topmost eligible control whose bounds contain (x, y) — the F3
    join used to tell phantom-affordance from discoverability (#783).

    Eligible means `control` is not explicitly False — passive context
    geometry (F3's label/panel records) is marked `control=False` and
    can never satisfy this join, even when it covers the click, so a
    dead click behind only a passive record still reads as "hit no
    widget". A record with no `control` key (older traces predating
    this field, or a hand-built fixture) defaults eligible for
    backward compatibility. `visible` is treated the same way — an
    explicit False excludes, a missing key defaults visible. A
    shown-but-disabled control (`enabled: False`) is NOT excluded here;
    disabled-ness explains why a click didn't activate anything, it
    doesn't remove the control from correlation.

    Containment is tested against `interactiveBounds` when the record
    carries it (#749 — the effective, clip-intersected pointer/hover/
    scroll/release rect, which for a migrated box-backed control is its
    expanded visual border, not the content-only `bounds`), so a click
    on a control's visible border correlates to that control exactly as
    the real UI router (UI.Manager.Query.isPointInElement) would resolve
    it. A record with no `interactiveBounds` (widget-module dumps
    without a live handle, older traces/fixtures, or a fully clipped
    element) falls back to `bounds` — the pre-#749 behavior.

    Among eligible matches, the one with the highest `(paintKey,
    paintOrder)` pair (compared lexicographically) wins. `paintKey` is
    the page-band + accumulated-zIndex ordering
    UI.Manager.Query.topHitBy resolves overlapping hits with — but it
    is NOT a total order on its own: ordinary siblings sharing a band
    and zIndex (the common case — most elements never set an explicit
    zIndex) tie on it. `paintOrder` is the element's position in the
    engine's own paint traversal (UI.Manager.Query.elementPaintOrder),
    exactly the tiebreak topHitBy itself applies at equal keys
    ("later-painted wins" — see its haddock). Comparing the pair
    reproduces topHitBy's exact selection, so an overlapping click
    (even a tied one) resolves to the control the real UI input router
    would pick, independent of the dump list's own (Lua-table-derived)
    order. A missing `paintKey`/`paintOrder` defaults to 0.

    `route_aware` (#1750) switches the join to reproduce the real
    left-click router (`UI.InputOwnership.routePointer`) instead of
    "topmost eligible record". The caller turns it on only for a
    default/left `click` action over a record set that actually
    carries the routing facts (see `routing_aware_records`); drag,
    right/middle clicks, and legacy traces keep the behavior described
    above, unchanged. The routing precedence is:

      0. Records the engine reports as out of pointer scope
         (`inScope is False` — a page below the modal boundary) are
         dropped outright. They are unreachable, so they can neither
         correlate nor occlude.
      1. Among the remaining records containing the point, find the
         topmost EFFECTIVE pointer-blocking surface
         (`pointerBlocking is True`) by the same `(paintKey,
         paintOrder)` ranking, which is exactly what
         `UI.Manager.Query.topHitBy` picks under
         `elementBlocksPointer`.
      2. If that surface is an active left-click target
         (`leftClickTarget is True`), correlate it — the click fires
         its callback.
      3. Otherwise, if it is a shown, explicitly pointer-blocking
         DISABLED left affordance (`leftClickAffordance is True`,
         `leftClickTarget` not True), correlate that disabled control
         itself. It blocks and it is visibly a control, so #783's
         "the control is there but disabled" reading still holds.
      4. Otherwise the blocker is passive (a callback-less occlusion
         surface, or a right-click-only control): correlate NOTHING,
         and never fall through to a lower control the router could
         not have reached.
      5. Only when no pointer-blocking surface covers the point at all
         does the disabled-affordance fallback apply: the topmost
         shown disabled left affordance, preserving #783 for a lone
         `setClickable(false)` control (which is NOT pointer-blocking
         unless it opted in explicitly).

    Note what case 5 deliberately does not cover: an in-scope, shown,
    non-blocking record that is neither an active target nor a
    disabled left affordance (an enabled control carrying only a
    right-click callback, say) correlates to nothing, matching
    `routePointer`'s `RouteMiss`. Records marked `control: False` stay
    passive context — they can OCCLUDE as blockers (that is the whole
    point of admitting the callback-less blocker) but can never be the
    correlated control. A record with no routing fields at all in an
    otherwise route-aware set is one the engine has no live element
    for, so it neither blocks nor correlates.
    """
    if not isinstance(widgets, list):
        return None
    best = None
    best_key = None
    # #1750 route-aware pass: collected here, resolved by the routing
    # precedence below. `blocker` is the topmost effective
    # pointer-blocking surface at the point; `affordance` is the
    # topmost shown disabled left-control affordance, used only as
    # case 5's no-blocker fallback.
    blocker = None
    blocker_key = None
    affordance = None
    affordance_key = None
    for w in widgets:
        if not isinstance(w, dict):
            continue
        if w.get("visible") is False:
            continue
        # A `control: False` record is passive context: never a
        # correlation target. In the route-aware pass it is still read
        # for OCCLUSION (a callback-less pointer blocker is exactly
        # such a record), so the eligibility test moves to the point
        # where a winner is chosen rather than gating the scan.
        if not route_aware and w.get("control") is False:
            continue
        # Out of pointer scope (below the modal boundary) — the router
        # cannot see it, so it neither correlates nor occludes.
        if route_aware and w.get("inScope") is False:
            continue
        # #749: prefer the effective interactive bounds (the rect a real
        # hit resolves against). A value of False is the engine's DISTINCT
        # "non-hittable" marker (fully clipped / collapsed control) — skip
        # it entirely rather than falling back to content bounds, which
        # the real router could never hit. A dict with a non-positive
        # extent is likewise degenerate and non-hittable. Only a MISSING
        # key (None — an older trace / hand-built fixture that predates
        # this field) falls back to content `bounds`.
        ib = w.get("interactiveBounds")
        if ib is False:
            continue
        if isinstance(ib, dict):
            if ib.get("w", 0) <= 0 or ib.get("h", 0) <= 0:
                continue
            b = ib
        else:
            b = w.get("bounds")
        if not isinstance(b, dict):
            continue
        try:
            hit = (b["x"] <= x <= b["x"] + b["w"]
                   and b["y"] <= y <= b["y"] + b["h"])
        except (KeyError, TypeError):
            continue
        if not hit:
            continue
        key = (w.get("paintKey", 0), w.get("paintOrder", 0))
        if route_aware:
            if w.get("pointerBlocking") is True:
                if blocker is None or key >= blocker_key:
                    blocker, blocker_key = w, key
            if (w.get("control") is not False
                    and w.get("leftClickAffordance") is True
                    and w.get("leftClickTarget") is not True):
                if affordance is None or key >= affordance_key:
                    affordance, affordance_key = w, key
            continue
        if best is None or key >= best_key:
            best, best_key = w, key
    if route_aware:
        if blocker is not None:
            # An active left target activates; an explicitly blocking
            # disabled left affordance correlates to itself; anything
            # else consumed the click without a left-control meaning,
            # so nothing correlates and nothing lower is reachable.
            if blocker.get("control") is not False:
                if blocker.get("leftClickTarget") is True:
                    return blocker
                if blocker.get("leftClickAffordance") is True:
                    return blocker
            return None
        return affordance
    return best


def routing_aware_records(widgets) -> bool:
    """True when this record set carries #1750's routing facts, so the
    click join may use `widget_at(..., route_aware=True)`.

    Detected EXPLICITLY, by the presence of the fields themselves, and
    never inferred from their values. On a pre-#1750 trace every
    record lacks `pointerBlocking`, so reading absent as False would
    make the routing precedence conclude "no blocking surface" and
    drop straight to the disabled-affordance fallback — silently
    correlating a disabled control over an enabled one on every legacy
    trace. `inScope` is the marker because the engine emits it for
    every element it has a live record for (`pushElementInfoTable`),
    and both `ui.dumpWidgets` passes carry it through."""
    if not isinstance(widgets, list):
        return False
    return any(isinstance(w, dict) and "inScope" in w for w in widgets)


def is_left_click(action: dict) -> bool:
    """True for a `click` action the #1750 routing contract covers:
    button omitted (or otherwise falsy — `translate_action` normalizes
    that to "left") or naming left.

    Matched case-insensitively because the engine itself case-folds
    (`Engine.Input.Inject.resolveButton` lowercases before matching),
    so "Left" really does inject a left click. No other normalization
    is applied: the engine does not strip whitespace either, and a
    value it would reject outright must not be treated as a left click
    here. Anything that does not resolve to left — "right", "middle",
    a typo, a non-string — falls through to the legacy join, so an
    unexpected token can never cause a false routing suppression.
    """
    if action.get("do") != "click":
        return False
    button = action.get("button")
    if not button:
        return True
    return isinstance(button, str) and button.lower() == "left"
