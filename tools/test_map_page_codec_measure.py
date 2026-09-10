#!/usr/bin/env python3
"""Engine-free arithmetic, fixture, and failed-candidate tests for #2303."""
import copy
from fractions import Fraction
import json
from pathlib import Path
import unittest

import map_page_codec_model as model


class CorpusTests(unittest.TestCase):
    def setUp(self):
        self.worlds = json.loads((Path(__file__).parent / "map_codec/corpus.json").read_text())

    def test_tracked_corpus_covers_all_categories(self):
        model.validate_corpus(self.worlds)

    def test_bad_coordinates_and_inputs_are_refused(self):
        for field, value in (("u", -1), ("v", 999), ("level", 10),
                             ("u", 0.5), ("id", "../escape")):
            with self.subTest(field=field, value=value):
                worlds = copy.deepcopy(self.worlds)
                worlds[0]["pages"][0][field] = value
                with self.assertRaises(ValueError):
                    model.validate_corpus(worlds)

    def test_duplicate_page_and_missing_categories_are_refused(self):
        worlds = copy.deepcopy(self.worlds)
        worlds[0]["pages"].append(worlds[0]["pages"][0])
        with self.assertRaises(ValueError):
            model.validate_corpus(worlds)
        for world in self.worlds:
            for page in world["pages"]:
                page["categories"] = ["parity"]
        with self.assertRaises(ValueError):
            model.validate_corpus(self.worlds)

    def test_declaring_lava_does_not_prove_lava(self):
        observation = dict(ocean_source_tiles=10, lava_source_tiles=0,
                           ice_source_tiles=0, material_ids=[1], dry_material_ids=[1],
                           transparent_pixels=0, longitude_seam=False,
                           latitude_edge=False, parity_round_trip=True)
        self.assertEqual(model.verify_observation({"categories": ["ocean", "lava", "parity"]}, observation), ["lava"])

    def test_inventory_hand_examples(self):
        self.assertEqual(model.level_shape(64, 0), (2, 4))
        self.assertEqual(model.level_shape(136, 0), (5, 9))
        self.assertEqual(model.level_shape(136, 2), (2, 3))
        self.assertEqual(model.level_shape(8192, 7), (2, 4))
        for size, level in ((7, 0), (9, 0), (64, 1), (136, -1)):
            with self.assertRaises(ValueError):
                model.level_shape(size, level)


class ArithmeticTests(unittest.TestCase):
    def test_ratios_preserve_zero_and_expansion(self):
        self.assertEqual(model.ratio(0, 4), Fraction(0))
        self.assertEqual(model.ratio(3, 4), Fraction(3, 4))
        self.assertEqual(model.ratio(8, 4), Fraction(2))
        self.assertIsNone(model.ratio(0, 0))
        with self.assertRaises(ValueError):
            model.ratio(-1)

    def test_byte_lru_pins_hit_denominator_and_recency(self):
        # A,B,A promotes A; C therefore evicts B. Final B misses.
        result = model.lru("ABACAB", {"A": 3, "B": 2, "C": 2}, 5)
        self.assertEqual(result, dict(requests=6, hits=2, misses=4,
                                     hit_rate=1/3, peak_resident_bytes=5,
                                     distinct_working_set_bytes=7, distinct_pages=3))

    def test_over_quota_set_evicts_instead_of_growing(self):
        result = model.lru("ABCABC", dict(A=4, B=4, C=4), 8)
        self.assertEqual(result["hits"], 0)
        self.assertEqual(result["peak_resident_bytes"], 8)
        self.assertEqual(result["distinct_working_set_bytes"], 12)

    def test_oversized_page_bypasses_without_flushing_others(self):
        result = model.lru("AXA", dict(A=2, X=10), 3)
        self.assertEqual(result["hits"], 1)
        self.assertEqual(result["peak_resident_bytes"], 2)
        self.assertEqual(model.lru("AA", {"A": 1}, 0)["hits"], 0)
        self.assertIsNone(model.lru([], {}, 0)["hit_rate"])

    def test_real_differences_and_rejection_are_distinct(self):
        pristine = bytes(range(16))
        changed = bytearray(pristine)
        for offset in (0, 1, 7):
            changed[offset] ^= 1
        self.assertEqual(model.damage(pristine, bytes(changed)),
                         {"different_bytes": 3, "different_pixels": 2})
        self.assertEqual(model.damage(pristine, None),
                         {"different_bytes": None, "different_pixels": None})
        self.assertEqual(model.corruption_offsets(11), [0, 5, 10])
        with self.assertRaises(ValueError):
            model.damage(pristine, b"short")

    def test_failed_or_absent_candidate_stays_in_comparison(self):
        rows = [dict(codec="raw", deterministic=True, corruption_detected=True, round_trip=True),
                dict(codec="png", deterministic=False, corruption_detected=True, round_trip=True)]
        self.assertTrue(model.compare_checks(rows)["raw"]["passed"])
        self.assertFalse(model.compare_checks(rows)["png"]["passed"])
        rows[1].update(deterministic=True, corruption_detected=False)
        self.assertFalse(model.compare_checks(rows)["png"]["passed"])
        self.assertEqual(model.compare_checks(rows[:1])["png"], {"pages": 0, "passed": False})

    def test_camera_requests_are_bounded_and_reproducible(self):
        for kind in ("home-expeditions", "frontier", "distant-inspection"):
            requests = model.camera_trace(kind)
            self.assertEqual(requests, model.camera_trace(kind))
            self.assertGreater(len(requests), 1000)
            for level, u, v in set(requests):
                nu, nv = model.level_shape(8192, level)
                self.assertTrue(0 <= u < nu and 0 <= v < nv)


if __name__ == "__main__":
    unittest.main()
