"""Plant suitability failures and unavailable fixtures are distinct."""
from __future__ import annotations

import argparse
from unittest.mock import patch

from . import support
import plant_probe as probe

PROBE = "plant"


def test_plant_launch():
    support.isolated_launch_contract(PROBE, fields=dict(seed=42, size=64, plates=3),
        logs=["plant_engine.log"], default_log="/tmp/plant_probe_engine.log")


def test_plant_soil_failure():
    def exercise(rep):
        rows = [dict(name=name, category=category, score=0.0,
                     factors=[dict(factor=f, fit=1.0) for f in probe.FACTOR_NAMES])
                for name, category in (("tomato_plant", "row_crop"), ("wheat", "groundcover_crop"))]
        with patch.object(probe, "bootstrap"), \
             patch.object(probe, "send", return_value="ok"), \
             patch.object(probe, "select_positive_fixture", return_value=((1, 2, 3, "wheat", rows), [])), \
             patch.object(probe, "suitability_row", return_value=rows[1]), \
             patch.object(probe, "set_material_and_wait", side_effect=support.StopBeforeEngine), \
             patch.object(probe, "quit_engine") as quit_owned:
            try:
                probe._exercise(8911, object(), argparse.Namespace(seed=42, size=64, plates=3), True, rep)
            except support.StopBeforeEngine:
                pass
            support.expect(quit_owned.call_count == 1, "plant assertion path cleans up its engine")
    outcomes = support.captured_outcomes(probe, exercise)
    support.expect(outcomes == dict(preferred_soil_positive="FAIL", crop_suitability_inventory="PASS", factor_breakdown="PASS"),
                   "plant reports a zero preferred-soil score without inventing later checks")


def test_plant_missing_fixture():
    def exercise(rep):
        with patch.object(probe, "bootstrap"), patch.object(probe, "send"), \
             patch.object(probe, "select_positive_fixture", return_value=(None, [])), \
             patch.object(probe, "quit_engine") as quit_owned:
            rc = probe._exercise(8911, object(), argparse.Namespace(seed=42, size=64, plates=3), True, rep)
            support.expect(rc == 1 and quit_owned.call_count == 1, "missing plant fixture preserves failure and cleanup")
    support.expect(support.captured_outcomes(probe, exercise) == {},
                   "missing plant fixture leaves every assertion missing")


TESTS = (test_plant_launch, test_plant_soil_failure, test_plant_missing_fixture)
