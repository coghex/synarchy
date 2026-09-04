#!/usr/bin/env python3
"""Event-stream validation, the trusted prefix and forbidden markers (#2087).

The second of the three parsers this suite mutation-covers, plus the two
rules about what a migrated probe may write to its own stdout.
"""
from __future__ import annotations


from .support import probe_protocol
from .support import expect_raises, skip, expect
from .support import synthetic_descriptor as _descriptor
from .support import event_line as _line

def test_event_stream() -> None:
    print("\n-- event stream --")
    d = _descriptor()

    stream = (_line(event="check", id="alpha", outcome="PASS") +
              _line(event="check", id="beta", outcome="FAIL",
                    detail={"observed": 3}) +
              _line(event="check", id="gamma", outcome="PASS"))
    events, outcomes = probe_protocol.parse_event_stream(stream, d)
    expect(outcomes == {"alpha": "PASS", "beta": "FAIL", "gamma": "PASS"},
           "a complete in-order stream reconciles every declared check")
    expect(any(isinstance(e, probe_protocol.CheckEvent) and
               e.detail == {"observed": 3} for e in events),
           "dynamic runtime values ride in detail beside a stable identifier")

    partial = _line(event="check", id="alpha", outcome="PASS")
    _events, outcomes = probe_protocol.parse_event_stream(partial, d)
    expect(outcomes == {"alpha": "PASS", "beta": "MISSING", "gamma": "MISSING"},
           "checks a stopped run never emitted reconcile to MISSING")
    _events, outcomes = probe_protocol.parse_event_stream("", d)
    expect(set(outcomes.values()) == {"MISSING"},
           "an empty stream leaves every declared check MISSING")

    diagnostics = (_line(event="diagnostic", level="INFO", message="note") +
                   _line(event="check", id="alpha", outcome="PASS") +
                   _line(event="diagnostic", level="WARN", message="careful") +
                   _line(event="diagnostic", level="SKIP", message="skipped"))
    events, outcomes = probe_protocol.parse_event_stream(diagnostics, d)
    expect(outcomes["alpha"] == "PASS" and outcomes["beta"] == "MISSING",
           "INFO/WARN/SKIP diagnostics carry no check outcome of their own")
    expect(sum(isinstance(e, probe_protocol.DiagnosticEvent) for e in events) == 3,
           "all three supported diagnostic levels parse")

    def bad(stream: str):
        return lambda: probe_protocol.parse_event_stream(stream, d)

    expect_raises(probe_protocol.ProtocolError,
                  bad('{"event": "check", "id": "alpha"'),
                  "a truncated final line is a protocol error", "truncated")
    expect_raises(probe_protocol.ProtocolError, bad("not json\n"),
                  "a malformed line is a protocol error", "malformed JSON")
    expect_raises(probe_protocol.ProtocolError, bad("[1, 2]\n"),
                  "a non-object event line is a protocol error",
                  "not a JSON object")
    expect_raises(probe_protocol.ProtocolError,
                  bad(_line(event="check", id="delta", outcome="PASS")),
                  "an undeclared check identifier is a protocol error",
                  "is not declared")
    expect_raises(probe_protocol.ProtocolError,
                  bad(_line(event="check", id="alpha", outcome="PASS") +
                      _line(event="check", id="alpha", outcome="PASS")),
                  "a duplicate check event is a protocol error", "duplicate")
    expect_raises(probe_protocol.ProtocolError,
                  bad(_line(event="check", id="beta", outcome="PASS")),
                  "a check arriving before its declared predecessor is an error",
                  "arrived before the declared check")
    expect_raises(probe_protocol.ProtocolError,
                  bad(_line(event="check", id="alpha", outcome="PASS") +
                      _line(event="check", id="gamma", outcome="PASS") +
                      _line(event="check", id="beta", outcome="PASS")),
                  "a mid-sequence skip then backfill is out of order",
                  "arrived before the declared check")
    expect_raises(probe_protocol.ProtocolError,
                  bad(_line(event="check", id="alpha", outcome="MISSING")),
                  "MISSING is never a reportable check outcome", "expected one of")
    expect_raises(probe_protocol.ProtocolError,
                  bad(_line(event="check", id="alpha", outcome="pass")),
                  "a lowercase outcome is a protocol error", "expected one of")
    expect_raises(probe_protocol.ProtocolError,
                  bad(_line(event="diagnostic", level="DEBUG", message="x")),
                  "an unsupported diagnostic level is a protocol error",
                  "is not one of")
    expect_raises(probe_protocol.ProtocolError,
                  bad(_line(event="diagnostic", level="INFO")),
                  "a diagnostic with no message is a protocol error",
                  "no string `message`")
    expect_raises(probe_protocol.ProtocolError,
                  bad(_line(event="progress", id="alpha")),
                  "an unknown event kind is unclassifiable", "unclassifiable")
    # A non-string `id` must be a PROTOCOL error, not a crash: an
    # unhashable one (`[]`, `{}`) would otherwise raise TypeError out of
    # the dictionary membership test, escape every handler, and
    # traceback the harness instead of producing a harness error.
    for value in ([], {}, 5, None, True, ["alpha"], {"id": "alpha"}):
        expect_raises(probe_protocol.ProtocolError,
                      bad(_line(event="check", id=value, outcome="PASS")),
                      f"check id {value!r} is a protocol error, not a crash",
                      "must be a string")
    # And nothing a probe can put on a line may leak a non-ProtocolError.
    hostile = [
        _line(event="check", id=[], outcome="PASS"),
        _line(event="check", id={}, outcome=[]),
        _line(event="diagnostic", level=[], message={}),
        _line(event=[], id="alpha"),
        _line(event="check", id="alpha", outcome="PASS", detail=[[]]),
        '{"event": {"nested": {"deep": [1, 2]}}}\n',
        '{"event": "check", "id": "\\ud800", "outcome": "PASS"}\n',
    ]
    for line in hostile:
        try:
            probe_protocol.parse_event_stream(line, d)
            leaked = "accepted"
        except probe_protocol.ProtocolError:
            leaked = None
        except Exception as error:  # noqa: BLE001
            leaked = f"{type(error).__name__}: {error}"
        expect(leaked is None,
               f"hostile event line {line.strip()[:48]!r} is a ProtocolError "
               f"({leaked})")
    # A present `detail` must be an OBJECT. Every falsey non-object is
    # its own case: a truthiness fallback would coerce each to `{}` and
    # let a malformed event be counted as a pass.
    for value in ("not-an-object", [], "", 0, False, None, 1, [1, 2]):
        expect_raises(probe_protocol.ProtocolError,
                      bad(_line(event="check", id="alpha", outcome="PASS",
                                detail=value)),
                      f"check detail {value!r} is a protocol error",
                      "must be an object")
        expect_raises(probe_protocol.ProtocolError,
                      bad(_line(event="diagnostic", level="INFO",
                                message="m", detail=value)),
                      f"diagnostic detail {value!r} is a protocol error",
                      "must be an object")
    # An ABSENT key is the only thing that means "no detail".
    _events, outcomes = probe_protocol.parse_event_stream(
        _line(event="check", id="alpha", outcome="PASS"), d)
    expect(outcomes["alpha"] == "PASS",
           "an absent detail key is accepted as no detail")
    events, _outcomes = probe_protocol.parse_event_stream(
        _line(event="check", id="alpha", outcome="PASS", detail={}), d)
    expect(events[0].detail == {},
           "an explicitly empty detail object is accepted")


def test_trusted_prefix() -> None:
    print("\n-- trusted prefix of a broken stream --")
    d = _descriptor()
    good = _line(event="check", id="alpha", outcome="PASS")
    for name, tail in (("malformed", "not json\n"),
                       ("truncated", '{"event": "che'),
                       ("duplicate", _line(event="check", id="alpha",
                                           outcome="PASS")),
                       ("unknown id", _line(event="check", id="delta",
                                            outcome="PASS")),
                       ("bad level", _line(event="diagnostic", level="DEBUG",
                                           message="x"))):
        events, outcomes, error = probe_protocol.scan_event_stream(good + tail, d)
        expect(error is not None,
               f"a {name} tail is still an error")
        expect(outcomes == {"alpha": "PASS", "beta": "MISSING",
                            "gamma": "MISSING"},
               f"the valid prefix before a {name} tail is preserved "
               f"(got {outcomes})")
        expect(len(events) == 1,
               f"only the trusted prefix's events survive a {name} tail")
    # A clean stream scans with no error at all.
    _events, outcomes, error = probe_protocol.scan_event_stream(good, d)
    expect(error is None and outcomes["alpha"] == "PASS",
           "a clean stream scans without an error")
    # An out-of-order FIRST line has no valid prefix to keep.
    _events, outcomes, error = probe_protocol.scan_event_stream(
        _line(event="check", id="gamma", outcome="PASS"), d)
    expect(error is not None and set(outcomes.values()) == {"MISSING"},
           "a fault on the first line leaves nothing salvageable")


def test_forbidden_markers() -> None:
    print("\n-- forbidden stdout markers --")
    caught = ["[PASS] a check", "[FAIL] a check", "[pass] lowercase",
              "  [INFO] indented", "\t[WARN] tabbed", "[SKIP] skipped",
              "[diag] a legacy diagnostic", "[whatever]", "[UNKNOWN] form"]
    for line in caught:
        expect(probe_protocol.forbidden_marker_lines(line) == [line],
               f"marker line {line!r} is detected")
    allowed = ['[1, 2, 3]', '{"a": [1]}', 'result: [PASS] mid-line',
               '[]', '[3] numeric', 'plain text', '[a,b] not word-like',
               '[PASS]x no separator', '["a"]']
    for line in allowed:
        expect(probe_protocol.forbidden_marker_lines(line) == [],
               f"non-marker line {line!r} is left alone")
    multi = "ok\n[PASS] one\nmore\n[FAIL] two\n"
    expect(probe_protocol.forbidden_marker_lines(multi) ==
           ["[PASS] one", "[FAIL] two"],
           "every marker line in a multi-line capture is reported")


TESTS = (
    test_event_stream,
    test_trusted_prefix,
    test_forbidden_markers,
)
