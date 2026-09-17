#!/usr/bin/env python3
import unittest
from chunk_memory_measure import canonical, traversal, rss_bytes, require_json, require_region, require_samples, require_unit_id


class MeasurementContract(unittest.TestCase):
    def test_counter_units(self):
        self.assertEqual(rss_bytes(123, 'ru_maxrss', 'darwin'), 123)
        self.assertEqual(rss_bytes(123, 'ru_maxrss', 'linux'), 123*1024)
        for host in ('darwin','linux'):
            self.assertEqual(rss_bytes(123, 'ps-rss-kib', host), 123*1024)
        with self.assertRaises(ValueError):
            rss_bytes(123, 'unknown')

    def test_distinct_canonical_traversal(self):
        self.assertEqual(canonical(4,0,8), (0,4))
        for size in (64,256):
            coords = {cc for _,_,region in traversal(size) for cc in region}
            self.assertGreaterEqual(len(coords), 1000)
            self.assertTrue(all(-size//2 <= x-y < size//2 for x,y in coords))

    def test_console_failure_is_not_a_sample(self):
        for raw in ('', 'error: missing verb', '> banner\n> 0\n> '):
            with self.assertRaises(RuntimeError):
                require_json(raw)
        self.assertEqual(require_json('{"resident":12}'), {'resident':12})

    def test_missing_or_incomplete_workload_fails(self):
        for result in (None, {}, [], [{'loaded':False}], [None]):
            with self.assertRaises(RuntimeError):
                require_region(result, [(0,0)])
        require_region([{'loaded':True}], [(0,0)])

    def test_missing_phase_or_zero_sample_fails(self):
        for samples in ([], [{'phase':'generation','rssBytes':0}],
                        [{'phase':'boot','rssBytes':123}]):
            with self.assertRaises(RuntimeError):
                require_samples(samples, ['generation'])
        require_samples([{'phase':'generation','rssBytes':123}], ['generation'])

    def test_lua_numeric_unit_ids_and_rejections(self):
        for value in (8, 8.0):
            self.assertEqual(require_unit_id(value), 8)
        for value in (-1, 0, True, None, '8', 1.5, float('inf'), float('nan')):
            with self.assertRaises(RuntimeError):
                require_unit_id(value)


if __name__ == '__main__':
    unittest.main()
