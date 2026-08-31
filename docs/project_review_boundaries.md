# Project review sweep cursor

Machine-owned state for the `project-review` workflow: each repository's
exclusive older PR boundary, the units completed batches reviewed, the direct
history endpoint, and the units a user explicitly excluded. PR selection always
starts at the latest merge and stops before its boundary; a clean batch records
reviewed coverage exactly as a finding-bearing batch does.

Written by `project_review_cursor.py`. Edit it through that helper rather than
by hand: the payload below is parsed strictly, and an edit it cannot read stops
the next sweep instead of being ignored.

<!-- project-review:cursor:v2 -->

```json
{
  "repositories": {
    "coghex/synarchy": {
      "direct": {
        "endpoint": null,
        "reviewed": []
      },
      "excluded": {
        "commits": [],
        "prs": []
      },
      "pr": {
        "endpoint": {
          "merged_at": "2026-08-19T17:39:39Z",
          "number": 1423
        },
        "reviewed": [
          1411,
          1423,
          1834,
          1835,
          1836,
          1838,
          1839,
          1840,
          1841,
          1843,
          1847,
          1851,
          1852,
          1859,
          1860,
          1861,
          1862,
          1863,
          1864,
          1865,
          1866,
          1867,
          1870,
          1872,
          1877,
          1878,
          1879,
          1880,
          1881,
          1883,
          1885,
          1886,
          1887,
          1888,
          1889,
          1891,
          1893,
          1894,
          1895,
          1897,
          1898,
          1899,
          1900,
          1901,
          1902,
          1903,
          1904,
          1905,
          1906,
          1908,
          1923,
          1936,
          1942,
          1943,
          1951,
          1962,
          1964,
          1968,
          1970,
          1971,
          1972,
          1973,
          1974,
          1975,
          1976,
          1977,
          1979,
          1981,
          1984,
          1985,
          1986,
          1987,
          1988,
          1989
        ]
      }
    }
  },
  "version": 2
}
```
