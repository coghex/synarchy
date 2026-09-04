#!/usr/bin/env python3
"""Descriptor validation, from both sides (#2087).

Every rule `probe_protocol.parse_descriptor` enforces is exercised with a
document that must be accepted and a minimally mutated one that must be
rejected, because a validator that only ever sees valid input proves
nothing.
"""
from __future__ import annotations

import json

from .support import probe_protocol
from .support import expect_raises, expect

GOOD_DESCRIPTOR = {
    "protocol": "probe-result/v1",
    "probe": "synthetic",
    "checks": [{"id": "alpha", "label": "the first"},
               {"id": "beta", "label": "the second"}],
}


def test_descriptor() -> None:
    print("\n-- descriptor --")
    d = probe_protocol.parse_descriptor(json.dumps(GOOD_DESCRIPTOR),
                                        expected_probe="synthetic")
    expect(d.probe == "synthetic" and d.ids == ("alpha", "beta"),
           "a well-formed descriptor parses with its declared order intact")
    expect(json.loads(d.to_json()) == GOOD_DESCRIPTOR,
           "a descriptor round-trips through to_json unchanged")

    # --- mutations, one rule at a time ---
    def mutate(**changes):
        doc = json.loads(json.dumps(GOOD_DESCRIPTOR))
        doc.update(changes)
        return json.dumps(doc)

    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor("not json"),
                  "a non-JSON descriptor is a protocol error", "valid JSON")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor("[]"),
                  "a non-object descriptor is a protocol error", "JSON object")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor(
                      mutate(protocol="probe-result/v2")),
                  "an unsupported protocol version is rejected", "supports only")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor(
                      json.dumps({k: v for k, v in GOOD_DESCRIPTOR.items()
                                  if k != "protocol"})),
                  "a descriptor with no protocol version is rejected",
                  "supports only")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor(
                      json.dumps(GOOD_DESCRIPTOR), expected_probe="other"),
                  "a descriptor for the wrong probe key is rejected",
                  "was requested")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor(mutate(probe="")),
                  "a descriptor with an empty probe key is rejected",
                  "non-empty string")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor(mutate(checks=[])),
                  "a descriptor declaring no checks is rejected",
                  "declares no checks")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor(mutate(checks={})),
                  "a non-list `checks` is rejected", "must be a list")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor(mutate(checks=[
                      {"id": "alpha", "label": "one"},
                      {"id": "alpha", "label": "two"}])),
                  "duplicate check identifiers are rejected", "duplicate")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor(mutate(checks=[
                      {"id": "alpha"}])),
                  "a check with no label is rejected", "`id` and `label`")
    expect_raises(probe_protocol.ProtocolError,
                  lambda: probe_protocol.parse_descriptor(mutate(checks=[
                      {"id": "alpha", "label": ""}])),
                  "a check with an empty label is rejected", "no label")

    # Stable identifiers may not carry runtime values. A DIGIT is the
    # only way one can get in, so the protocol's own prohibited
    # examples must be refused rather than merely discouraged.
    unstable = ("role_miner_60", "unit_4711", "unit4711", "alpha_1", "beta2",
                "Alpha", "unit-4711", "alpha 1", "9alpha", "alpha.beta", "",
                "_alpha", "alpha_", "alpha__beta")
    for bad in unstable:
        expect_raises(probe_protocol.ProtocolError,
                      lambda bad=bad: probe_protocol.build_descriptor(
                          "synthetic", [(bad, "label")]),
                      f"identifier {bad!r} is refused as unstable",
                      "stable check identifier")
    ok = probe_protocol.build_descriptor(
        "synthetic", [("alpha", "l"), ("phase_two", "l"), ("a_b_c", "l")])
    expect(ok.ids == ("alpha", "phase_two", "a_b_c"),
           "lowercase words joined by single underscores are accepted")


TESTS = (
    test_descriptor,
)
