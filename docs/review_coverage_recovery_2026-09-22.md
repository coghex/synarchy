# Review coverage recovery — 2026-09-22

Recovery for `coghex/synarchy`, confirmed by the owner and migrated on 2026-09-22. The ledger now contains all 1,126 expected legacy PR rows; all 76 report mappings and 490 cursor evidence links were verified through a fresh helper read. No new review, commit, or publication was made by migration. Direct-history evidence remains documented but was not imported by the PR migration.

## What survived

- No `docs/project_review/ledger.md` was found in Synarchy worktrees, branches, or reflog-reachable history. The workspace search found modern ledgers only in the unrelated hetoimasia repository.
- The current `docs/project_review_boundaries.md` contains 490 explicitly reviewed PRs. Its committed version contains 478; the uncommitted twelve-PR batch is preserved in this proposal. Historical cursor versions contain no direct-history progress.
- There are 76 PR reports and four direct-history reports. One direct report explicitly describes broad triage rather than completed granular reviews.
- The complete GitHub inventory collected on this session contains 1,268 merged PRs across 13 contiguous pages: twelve pages of 100 and a final page of 68.
- The proposed union preserves **1,126 distinct merged PRs (88.8%)**: all 490 cursor entries plus 636 additional report-backed entries. All proposed numbers exist in the merged inventory. The remaining **142 PRs have no coverage established by this proposal**; this does not prove they were never reviewed.
- Historical reports and the original cursor remain unchanged. The owner confirmed the mappings below, and they were passed verbatim to `migrate --confirm`.

## Why confirmation is needed

The installed helper returned exit 3, `status: flagged`, for all 76 PR reports because their opening wording is outside its recognized sentence templates. It wrote nothing. The project-review skill says: **“A flagged migration stops for the operator”** and **“Do not guess which candidates were reviewed and do not continue past a flag.”**

Skill: `/Users/vincentcoghlan/.codex/plugins/cache/kanban/kanban/1.60.0/skills/project-review/SKILL.md`.

The mappings below are prepared for one explicit confirmation. Raw helper candidates include incidental references and sometimes only range endpoints; they must not be imported wholesale. Each entry includes its exact helper refusal reason, source paragraph, and proposed reviewed PRs. The companion JSON preserves the complete inventory, exact confirmations, and SHA-256 hashes of every PR report and the cursor for revalidation before migration.

## Range and identity reconciliation

- `1018-991`: the twelve-PR merge-order interval includes #990.
- `1035-1020` excludes later-merged #1034; `1048-1034` includes it and excludes #1035.
- `1063-1049`: #1061 merged after #1063, but line 64 explicitly describes it as reviewed; the proposed twelve include it.
- `1165-1128`: the merge-order interval includes #1166.
- `989-939`: #940 and #941 belong to the preceding report and lie outside this merge interval.
- `1423-1297`: its stated 58-PR inclusive range includes the explicitly reviewed late merge #1411, and does not import prior-range references #1271 or #1296.
- `432-412`: the report explicitly reviews documentation commit `07015dbb` and checks its result at line 7. GitHub confirms that commit is PR #429 (`07015dbb876a825cc5a1dc3e5b16dfeab3628185`), so the proposal includes it despite the report calling it direct.
- Incidental issue numbers, later fixes, previous batches, exclusive stops, and excluded concerns are omitted from each report mapping. Existing cursor coverage is preserved independently.
- The 18 PRs #1211 through #1248 have no explicit coverage in the located reports or cursor; the broad older-history statement does not supply an exact enumeration. They remain uncovered rather than being guessed into the ledger.

## Direct-history evidence retained for recovery

Three reports document consecutive twelve-commit batches, totaling 36 commits. Their ranges and next pointers agree with Git first-parent ancestry. The source cursor has no direct endpoint or reviewed list, so standard PR migration will not recover these automatically. No direct-mode selection or record command was run. This evidence is retained here so a later direct-history recovery does not silently restart at the beginning.

| Report | Completed batch | Next pointer |
| --- | --- | --- |
| `docs/project_review_direct_84f6fa2-a3f4481.md` | `84f6fa27` through `a3f4481d` | `ea2c03dd` |
| `docs/project_review_direct_ea2c03d-16daead.md` | `ea2c03dd` through `16daead6` | `f753927e` |
| `docs/project_review_direct_f753927-9590ae8.md` | `f753927e` through `9590ae80` | `36d97497561c27779b262d8b92f0354997da4a77` |

The broad `docs/project_review_direct_84f6fa2-1122797.md` inventory explicitly disclaims granular coverage; its 191-commit range is not proposed as reviewed.

## Proposed confirmations for every flagged report

### docs/project_review_1018-991.md

**Helper candidates:** #991, #1018.

**Helper reason:** docs/project_review_1018-991.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the twelve merged PRs from #1018 through #991 for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #991, #1018; confirm the reviewed ones with --confirm 'docs/project_review_1018-991.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the twelve merged PRs from #1018 through #991 for later one-at-a-time disposition.

**Proposed reviewed PRs (12):** #1018, #1017, #1015, #1004, #1003, #1002, #1001, #995, #994, #993, #991, #990.

**Basis:** Proposed expansion of the stated merge-order interval using the complete 1,268-PR GitHub inventory; requires confirmation because the report does not enumerate every member.

**Exact confirmation:** `docs/project_review_1018-991.md=1018,1017,1015,1004,1003,1002,1001,995,994,993,991,990`

### docs/project_review_1035-1020.md

**Helper candidates:** #1020, #1035.

**Helper reason:** docs/project_review_1035-1020.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs, #1035 through #1020 in merge order, for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #1020, #1035; confirm the reviewed ones with --confirm 'docs/project_review_1035-1020.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs, #1035 through #1020 in merge order, for later one-at-a-time disposition.

**Proposed reviewed PRs (12):** #1035, #1033, #1032, #1030, #1029, #1028, #1027, #1026, #1025, #1024, #1023, #1020.

**Basis:** Proposed expansion of the stated merge-order interval using the complete 1,268-PR GitHub inventory; requires confirmation because the report does not enumerate every member.

**Exact confirmation:** `docs/project_review_1035-1020.md=1035,1033,1032,1030,1029,1028,1027,1026,1025,1024,1023,1020`

### docs/project_review_1048-1034.md

**Helper candidates:** #1034, #1048.

**Helper reason:** docs/project_review_1048-1034.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of merged PRs #1048 through #1034, plus the two direct first-parent documentation commits in the same merge window, for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #1034, #1048; confirm the reviewed ones with --confirm 'docs/project_review_1048-1034.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of merged PRs #1048 through #1034, plus the two direct first-parent documentation commits in the same merge window, for later one-at-a-time disposition.

**Proposed reviewed PRs (12):** #1048, #1047, #1046, #1045, #1044, #1043, #1042, #1041, #1039, #1038, #1037, #1034.

**Basis:** Proposed expansion of the stated merge-order interval using the complete 1,268-PR GitHub inventory; requires confirmation because the report does not enumerate every member.

**Exact confirmation:** `docs/project_review_1048-1034.md=1048,1047,1046,1045,1044,1043,1042,1041,1039,1038,1037,1034`

### docs/project_review_1063-1049.md

**Helper candidates:** #1049, #1063.

**Helper reason:** docs/project_review_1063-1049.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of merged PRs #1063 through #1049 for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #1049, #1063; confirm the reviewed ones with --confirm 'docs/project_review_1063-1049.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of merged PRs #1063 through #1049 for later one-at-a-time disposition.

**Proposed reviewed PRs (12):** #1063, #1062, #1061, #1060, #1056, #1055, #1054, #1053, #1052, #1051, #1050, #1049.

**Basis:** Merged PRs in the stated numeric range; includes #1061, explicitly described as reviewed in this report at line 64 despite merging after #1063.

**Exact confirmation:** `docs/project_review_1063-1049.md=1063,1062,1061,1060,1056,1055,1054,1053,1052,1051,1050,1049`

### docs/project_review_1076-1064.md

**Helper candidates:** #1064, #1076.

**Helper reason:** docs/project_review_1076-1064.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of merged PRs #1076 through #1064 for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #1064, #1076; confirm the reviewed ones with --confirm 'docs/project_review_1076-1064.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of merged PRs #1076 through #1064 for later one-at-a-time disposition.

**Proposed reviewed PRs (12):** #1076, #1075, #1074, #1073, #1071, #1070, #1069, #1068, #1067, #1066, #1065, #1064.

**Basis:** Proposed expansion of the stated merge-order interval using the complete 1,268-PR GitHub inventory; requires confirmation because the report does not enumerate every member.

**Exact confirmation:** `docs/project_review_1076-1064.md=1076,1075,1074,1073,1071,1070,1069,1068,1067,1066,1065,1064`

### docs/project_review_1127-1079.md

**Helper candidates:** #1079, #1127.

**Helper reason:** docs/project_review_1127-1079.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of merged PRs #1127 through #1079 for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #1079, #1127; confirm the reviewed ones with --confirm 'docs/project_review_1127-1079.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of merged PRs #1127 through #1079 for later one-at-a-time disposition.

**Proposed reviewed PRs (12):** #1127, #1126, #1125, #1124, #1123, #1122, #1121, #1120, #1089, #1082, #1080, #1079.

**Basis:** Proposed expansion of the stated merge-order interval using the complete 1,268-PR GitHub inventory; requires confirmation because the report does not enumerate every member.

**Exact confirmation:** `docs/project_review_1127-1079.md=1127,1126,1125,1124,1123,1122,1121,1120,1089,1082,1080,1079`

### docs/project_review_1165-1128.md

**Helper candidates:** #1128, #1165.

**Helper reason:** docs/project_review_1165-1128.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of merged PRs #1165 through #1128 for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #1128, #1165; confirm the reviewed ones with --confirm 'docs/project_review_1165-1128.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of merged PRs #1165 through #1128 for later one-at-a-time disposition.

**Proposed reviewed PRs (12):** #1166, #1165, #1164, #1163, #1143, #1142, #1141, #1140, #1134, #1130, #1129, #1128.

**Basis:** Proposed expansion of the stated merge-order interval using the complete 1,268-PR GitHub inventory; requires confirmation because the report does not enumerate every member.

**Exact confirmation:** `docs/project_review_1165-1128.md=1166,1165,1164,1163,1143,1142,1141,1140,1134,1130,1129,1128`

### docs/project_review_1182-1167.md

**Helper candidates:** #1167, #1182.

**Helper reason:** docs/project_review_1182-1167.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of merged PRs #1182 through #1167 for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #1167, #1182; confirm the reviewed ones with --confirm 'docs/project_review_1182-1167.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of merged PRs #1182 through #1167 for later one-at-a-time disposition.

**Proposed reviewed PRs (12):** #1182, #1180, #1179, #1178, #1174, #1173, #1172, #1171, #1170, #1169, #1168, #1167.

**Basis:** Proposed expansion of the stated merge-order interval using the complete 1,268-PR GitHub inventory; requires confirmation because the report does not enumerate every member.

**Exact confirmation:** `docs/project_review_1182-1167.md=1182,1180,1179,1178,1174,1173,1172,1171,1170,1169,1168,1167`

### docs/project_review_1210-1183.md

**Helper candidates:** #1183, #1210.

**Helper reason:** docs/project_review_1210-1183.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of merged PRs #1210 through #1183 for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #1183, #1210; confirm the reviewed ones with --confirm 'docs/project_review_1210-1183.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of merged PRs #1210 through #1183 for later one-at-a-time disposition.

**Proposed reviewed PRs (12):** #1210, #1202, #1201, #1199, #1198, #1194, #1193, #1188, #1186, #1185, #1184, #1183.

**Basis:** Proposed expansion of the stated merge-order interval using the complete 1,268-PR GitHub inventory; requires confirmation because the report does not enumerate every member.

**Exact confirmation:** `docs/project_review_1210-1183.md=1210,1202,1201,1199,1198,1194,1193,1188,1186,1185,1184,1183`

### docs/project_review_1296.md

**Helper candidates:** #1271, #1296.

**Helper reason:** docs/project_review_1296.md: its opening paragraph carries a sentence this helper does not read: 'This entry records focused evidence from the senior review of merged PRs #1296 through #1271, including the direct first-parent audio-design commit in that landing interval, for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #1271, #1296; confirm the reviewed ones with --confirm 'docs/project_review_1296.md=<list>'.

**Source opening:** This entry records focused evidence from the senior review of merged PRs #1296 through #1271, including the direct first-parent audio-design commit in that landing interval, for later one-at-a-time disposition. The separately reported `transfer_order_probe.py` concern was deliberately excluded at the user's request because its tracker issues were already being drafted.

**Proposed reviewed PRs (12):** #1296, #1295, #1294, #1293, #1292, #1290, #1289, #1288, #1287, #1285, #1284, #1271.

**Basis:** Proposed expansion of the stated merge-order interval using the complete 1,268-PR GitHub inventory; requires confirmation because the report does not enumerate every member.

**Exact confirmation:** `docs/project_review_1296.md=1296,1295,1294,1293,1292,1290,1289,1288,1287,1285,1284,1271`

### docs/project_review_1423-1297.md

**Helper candidates:** #1271, #1296, #1297, #1411, #1423.

**Helper reason:** docs/project_review_1423-1297.md: its opening paragraph carries a sentence this helper does not read: 'This entry records the senior review of merged PRs #1423 through #1297 — 58 PRs across five batches, merged 2026-08-13 through 2026-08-19 — and moves the review cursor: \x00 was the previous newest entry, ranges below it were skipped as deliberately clear, and everything from #1297 up through #1423 is covered here.', so no row was written from it. Its candidate pull requests are #1271, #1296, #1297, #1411, #1423; confirm the reviewed ones with --confirm 'docs/project_review_1423-1297.md=<list>'.

**Source opening:** This entry records the senior review of merged PRs #1423 through #1297 — 58 PRs across five batches, merged 2026-08-13 through 2026-08-19 — and moves the review cursor: `project_review_1296.md` (PRs #1296–#1271) was the previous newest entry, ranges below it were skipped as deliberately clear, and everything from #1297 up through #1423 is covered here. **PR #1411 (merged 2026-08-19T18:04Z, after #1423) is included in this sweep despite its lower number** — a future sweep resuming from the merge-date position of #1423 must not re-review it. Direct first-parent master commits in the window were docs-lane report dispositions plus one reviewed direct test-file push (`d1d5f33e`, removing a `test_findings_report_audit.py` assertion that measured processing-lane backlog rather than lexer behavior — a justified un-wedge of a master-wide red, verified correct).

**Proposed reviewed PRs (58):** #1423, #1422, #1411, #1409, #1407, #1406, #1405, #1404, #1403, #1393, #1392, #1391, #1390, #1389, #1387, #1373, #1371, #1353, #1352, #1351, #1350, #1349, #1348, #1347, #1346, #1345, #1344, #1343, #1340, #1339, #1338, #1336, #1335, #1334, #1333, #1332, #1328, #1327, #1317, #1316, #1315, #1314, #1313, #1312, #1310, #1309, #1308, #1307, #1306, #1305, #1304, #1303, #1302, #1301, #1300, #1299, #1298, #1297.

**Basis:** 58 merged PRs in the explicitly inclusive numeric range; includes the report's explicit late-merged exception #1411.

**Exact confirmation:** `docs/project_review_1423-1297.md=1423,1422,1411,1409,1407,1406,1405,1404,1403,1393,1392,1391,1390,1389,1387,1373,1371,1353,1352,1351,1350,1349,1348,1347,1346,1345,1344,1343,1340,1339,1338,1336,1335,1334,1333,1332,1328,1327,1317,1316,1315,1314,1313,1312,1310,1309,1308,1307,1306,1305,1304,1303,1302,1301,1300,1299,1298,1297`

### docs/project_review_1455-1424.md

**Helper candidates:** #1411, #1423, #1424, #1442, #1443, #1445, #1446, #1448, #1449, #1450, #1451, #1452, #1453, #1454, #1455.

**Helper reason:** docs/project_review_1455-1424.md: its opening paragraph carries a sentence this helper does not read: 'This entry records the senior review of the final 13 merged PRs after the previous sweep: #1455, #1453, #1454, #1452, #1451, #1450, #1449, #1448, #1446, #1445, #1443, #1442, and #1424, merged on 2026-08-19 through 2026-08-20.', so no row was written from it. Its candidate pull requests are #1411, #1423, #1424, #1442, #1443, #1445, #1446, #1448, #1449, #1450, #1451, #1452, #1453, #1454, #1455; confirm the reviewed ones with --confirm 'docs/project_review_1455-1424.md=<list>'.

**Source opening:** This entry records the senior review of the final 13 merged PRs after the previous sweep: #1455, #1453, #1454, #1452, #1451, #1450, #1449, #1448, #1446, #1445, #1443, #1442, and #1424, merged on 2026-08-19 through 2026-08-20. The first-parent interval also contains direct commit `16f04f07`, which only records the existing project-review checkpoint and cleared review. PR #1411 is deliberately excluded because the earlier sweep already reviewed that late-merged PR. This batch stops at the exclusive checkpoint before PR #1423; no older history was reopened.

**Proposed reviewed PRs (13):** #1455, #1454, #1453, #1452, #1451, #1450, #1449, #1448, #1446, #1445, #1443, #1442, #1424.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_1455-1424.md=1455,1454,1453,1452,1451,1450,1449,1448,1446,1445,1443,1442,1424`

### docs/project_review_1547-1535.md

**Helper candidates:** #1535, #1536, #1537, #1538, #1540, #1541, #1542, #1543, #1544, #1545, #1546, #1547, #1570, #1630.

**Helper reason:** docs/project_review_1547-1535.md: its opening paragraph carries a sentence this helper does not read: 'This entry records the senior review of the next twelve merged PRs in merge-time order — #1547, #1546, #1536, #1545, #1544, #1543, #1542, #1541, #1540, #1538, #1537, and #1535 — plus direct first-parent commit \x00 in the same landing interval.', so no row was written from it. Its candidate pull requests are #1535, #1536, #1537, #1538, #1540, #1541, #1542, #1543, #1544, #1545, #1546, #1547, #1570, #1630; confirm the reviewed ones with --confirm 'docs/project_review_1547-1535.md=<list>'.

**Source opening:** This entry records the senior review of the next twelve merged PRs in merge-time order — #1547, #1546, #1536, #1545, #1544, #1543, #1542, #1541, #1540, #1538, #1537, and #1535 — plus direct first-parent commit `1657e834` (`docs: land pending design and findings updates`) in the same landing interval. The implementation, test-oracle, export-narrowing, CI, and playtest contracts remain coherent in the current tree and their focused checks passed. The direct commit's already-recorded shared-Cabal probe race was subsequently fixed by issue #1570 / PR #1630, so it is not a current finding. PR #1545's raw measurement corpus still reproduces exactly, but the tracked report draws a stronger failure-rate conclusion than its eight-attempt cells can establish; that current documentation defect is preserved below for later one-at-a-time disposition.

**Proposed reviewed PRs (12):** #1547, #1546, #1545, #1544, #1543, #1542, #1541, #1540, #1538, #1537, #1536, #1535.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_1547-1535.md=1547,1546,1545,1544,1543,1542,1541,1540,1538,1537,1536,1535`

### docs/project_review_1630-1614.md

**Helper candidates:** #1614, #1615, #1619, #1622, #1623, #1625, #1626, #1627, #1628, #1629, #1630, #1632.

**Helper reason:** docs/project_review_1630-1614.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge-time order — #1630, #1632, #1629, #1628, #1627, #1626, #1623, #1625, #1622, #1619, #1615, and #1614 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #1614, #1615, #1619, #1622, #1623, #1625, #1626, #1627, #1628, #1629, #1630, #1632; confirm the reviewed ones with --confirm 'docs/project_review_1630-1614.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge-time order — #1630, #1632, #1629, #1628, #1627, #1626, #1623, #1625, #1622, #1619, #1615, and #1614 — for later one-at-a-time disposition. The same landing interval contains one direct first-parent commit, `04a6f55d` (`docs: add the CI stdout and logging hygiene findings report`), which is documentation-only and remains coherent with the report-processing lane.

**Proposed reviewed PRs (12):** #1632, #1630, #1629, #1628, #1627, #1626, #1625, #1623, #1622, #1619, #1615, #1614.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_1630-1614.md=1632,1630,1629,1628,1627,1626,1625,1623,1622,1619,1615,1614`

### docs/project_review_1642-1631.md

**Helper candidates:** #1624, #1631, #1633, #1634, #1635, #1636, #1637, #1638, #1639, #1640, #1641, #1642.

**Helper reason:** docs/project_review_1642-1631.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge-time order — #1642, #1641, #1640, #1639, #1638, #1637, #1636, #1635, #1633, #1624, #1634, and #1631 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #1624, #1631, #1633, #1634, #1635, #1636, #1637, #1638, #1639, #1640, #1641, #1642; confirm the reviewed ones with --confirm 'docs/project_review_1642-1631.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge-time order — #1642, #1641, #1640, #1639, #1638, #1637, #1636, #1635, #1633, #1624, #1634, and #1631 — for later one-at-a-time disposition. The linked issues, PR descriptions, commit messages, merged diffs, and current descendants were inspected newest-first. There were no direct first-parent commits in the same landing interval.

**Proposed reviewed PRs (12):** #1642, #1641, #1640, #1639, #1638, #1637, #1636, #1635, #1634, #1633, #1631, #1624.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_1642-1631.md=1642,1641,1640,1639,1638,1637,1636,1635,1634,1633,1631,1624`

### docs/project_review_1655-1643.md

**Helper candidates:** #1643, #1644, #1645, #1646, #1647, #1648, #1649, #1650, #1651, #1653, #1654, #1655.

**Helper reason:** docs/project_review_1655-1643.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge-time order — #1655, #1654, #1653, #1651, #1650, #1649, #1648, #1647, #1646, #1645, #1644, and #1643 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #1643, #1644, #1645, #1646, #1647, #1648, #1649, #1650, #1651, #1653, #1654, #1655; confirm the reviewed ones with --confirm 'docs/project_review_1655-1643.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge-time order — #1655, #1654, #1653, #1651, #1650, #1649, #1648, #1647, #1646, #1645, #1644, and #1643 — for later one-at-a-time disposition. There were no direct first-parent commits in the same landing interval.

**Proposed reviewed PRs (12):** #1655, #1654, #1653, #1651, #1650, #1649, #1648, #1647, #1646, #1645, #1644, #1643.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_1655-1643.md=1655,1654,1653,1651,1650,1649,1648,1647,1646,1645,1644,1643`

### docs/project_review_167-80.md

**Helper candidates:** #77, #80, #83, #109, #112, #113, #121, #155, #162, #164, #165, #167.

**Helper reason:** docs/project_review_167-80.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in first-parent order — #167, #165, #164, #162, #155, #121, #113, #112, #109, #77, #83, and #80 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #77, #80, #83, #109, #112, #113, #121, #155, #162, #164, #165, #167; confirm the reviewed ones with --confirm 'docs/project_review_167-80.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in first-parent order — #167, #165, #164, #162, #155, #121, #113, #112, #109, #77, #83, and #80 — for later one-at-a-time disposition. The window contains exactly those twelve PR merges and no unrelated direct commits.

**Proposed reviewed PRs (12):** #167, #165, #164, #162, #155, #121, #113, #112, #109, #83, #80, #77.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_167-80.md=167,165,164,162,155,121,113,112,109,83,80,77`

### docs/project_review_1684-1656.md

**Helper candidates:** #1652, #1656, #1657, #1658, #1662, #1663, #1664, #1665, #1677, #1678, #1683, #1684.

**Helper reason:** docs/project_review_1684-1656.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the twelve most recently merged PRs in merge-time order — #1684, #1683, #1678, #1677, #1665, #1664, #1663, #1652, #1662, #1658, #1657, and #1656 — plus the 23 direct first-parent commits in the same landing interval, for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #1652, #1656, #1657, #1658, #1662, #1663, #1664, #1665, #1677, #1678, #1683, #1684; confirm the reviewed ones with --confirm 'docs/project_review_1684-1656.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the twelve most recently merged PRs in merge-time order — #1684, #1683, #1678, #1677, #1665, #1664, #1663, #1652, #1662, #1658, #1657, and #1656 — plus the 23 direct first-parent commits in the same landing interval, for later one-at-a-time disposition.

**Proposed reviewed PRs (12):** #1684, #1683, #1678, #1677, #1665, #1664, #1663, #1662, #1658, #1657, #1656, #1652.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_1684-1656.md=1684,1683,1678,1677,1665,1664,1663,1662,1658,1657,1656,1652`

### docs/project_review_1878-1859.md

**Helper candidates:** #1859, #1860, #1861, #1862, #1863, #1865, #1866, #1867, #1870, #1872, #1877, #1878.

**Helper reason:** docs/project_review_1878-1859.md: its opening paragraph carries a sentence this helper does not read: 'This report records the senior review of the next twelve uncovered merged pull requests in merge-time order — #1878, #1877, #1870, #1872, #1867, #1866, #1865, #1863, #1862, #1861, #1860, and #1859.', so no row was written from it. Its candidate pull requests are #1859, #1860, #1861, #1862, #1863, #1865, #1866, #1867, #1870, #1872, #1877, #1878; confirm the reviewed ones with --confirm 'docs/project_review_1878-1859.md=<list>'.

**Source opening:** This report records the senior review of the next twelve uncovered merged pull requests in merge-time order — #1878, #1877, #1870, #1872, #1867, #1866, #1865, #1863, #1862, #1861, #1860, and #1859. The review read each pull request, its linked specification, merged diff and commits, then traced the surviving behavior at current HEAD. No direct first-parent commit landed in the same interval. Eleven selected pull requests produced no current concern; PR #1862's farm-probe yield trail still has one unsampled transition that contradicts its stated “picked up, carried or eaten all still count” contract, preserved below. No concern was explicitly excluded from this batch.

**Proposed reviewed PRs (12):** #1878, #1877, #1872, #1870, #1867, #1866, #1865, #1863, #1862, #1861, #1860, #1859.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_1878-1859.md=1878,1877,1872,1870,1867,1866,1865,1863,1862,1861,1860,1859`

### docs/project_review_1981-1968.md

**Helper candidates:** #1900, #1968, #1970, #1971, #1972, #1973, #1974, #1975, #1976, #1977, #1979, #1981.

**Helper reason:** docs/project_review_1981-1968.md: its opening paragraph carries a sentence this helper does not read: 'This report records the senior review of the next twelve uncovered merged pull requests in merge order — #1981, #1979, #1977, #1976, #1975, #1974, #1973, #1900, #1972, #1971, #1970, and #1968 — plus direct first-parent commit in the same landing interval.', so no row was written from it. Its candidate pull requests are #1900, #1968, #1970, #1971, #1972, #1973, #1974, #1975, #1976, #1977, #1979, #1981; confirm the reviewed ones with --confirm 'docs/project_review_1981-1968.md=<list>'.

**Source opening:** This report records the senior review of the next twelve uncovered merged pull requests in merge order — #1981, #1979, #1977, #1976, #1975, #1974, #1973, #1900, #1972, #1971, #1970, and #1968 — plus direct first-parent commit `4960d4d9` in the same landing interval. The review read each pull request, its linked specification, merged diff and commits, then traced the surviving behavior at current HEAD. One new current documentation-contract mistake is retained below for later one-at-a-time disposition. The other eleven pull requests and the direct documentation commit produced no separate current concern, and no concern was explicitly excluded from this batch.

**Proposed reviewed PRs (12):** #1981, #1979, #1977, #1976, #1975, #1974, #1973, #1972, #1971, #1970, #1968, #1900.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_1981-1968.md=1981,1979,1977,1976,1975,1974,1973,1972,1971,1970,1968,1900`

### docs/project_review_1986-1908.md

**Helper candidates:** #1905, #1908, #1923, #1936, #1942, #1943, #1951, #1962, #1964, #1984, #1985, #1986.

**Helper reason:** docs/project_review_1986-1908.md: its opening paragraph carries a sentence this helper does not read: 'This report records the senior review of the next twelve uncovered merged pull requests in merge order — #1986, #1985, #1984, #1964, #1962, #1951, #1943, #1942, #1936, #1923, #1905, and #1908 — plus direct first-parent commit in the same landing interval.', so no row was written from it. Its candidate pull requests are #1905, #1908, #1923, #1936, #1942, #1943, #1951, #1962, #1964, #1984, #1985, #1986; confirm the reviewed ones with --confirm 'docs/project_review_1986-1908.md=<list>'.

**Source opening:** This report records the senior review of the next twelve uncovered merged pull requests in merge order — #1986, #1985, #1984, #1964, #1962, #1951, #1943, #1942, #1936, #1923, #1905, and #1908 — plus direct first-parent commit `4960d4d9` in the same landing interval. The review read each pull request, its linked specification where one existed, merged diff and commits, then traced the surviving behavior at current HEAD. One new current audit-enforcement mistake is retained below for later one-at-a-time disposition. The other eleven pull requests and the direct documentation commit produced no separate current concern, and no concern was explicitly excluded from this batch.

**Proposed reviewed PRs (12):** #1986, #1985, #1984, #1964, #1962, #1951, #1943, #1942, #1936, #1923, #1908, #1905.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_1986-1908.md=1986,1985,1984,1964,1962,1951,1943,1942,1936,1923,1908,1905`

### docs/project_review_1987-1893.md

**Helper candidates:** #1893, #1894, #1895, #1897, #1898, #1899, #1901, #1902, #1903, #1904, #1906, #1979, #1987.

**Helper reason:** docs/project_review_1987-1893.md: its opening paragraph carries a sentence this helper does not read: 'This report records the senior review of the next twelve uncovered merged pull requests in merge order — #1987, #1906, #1904, #1903, #1902, #1901, #1899, #1898, #1897, #1895, #1894, and #1893 — plus direct first-parent commits,, and in the same landing interval.', so no row was written from it. Its candidate pull requests are #1893, #1894, #1895, #1897, #1898, #1899, #1901, #1902, #1903, #1904, #1906, #1979, #1987; confirm the reviewed ones with --confirm 'docs/project_review_1987-1893.md=<list>'.

**Source opening:** This report records the senior review of the next twelve uncovered merged pull requests in merge order — #1987, #1906, #1904, #1903, #1902, #1901, #1899, #1898, #1897, #1895, #1894, and #1893 — plus direct first-parent commits `99d73d07`, `0dd0cdc8`, and `4960d4d9` in the same landing interval. The review read each pull request, its linked specification where one existed, merged diff and commits, then traced the surviving behavior at current HEAD. The direct census reconciliation exposed one post-merge protocol-status drift from already-covered PR #1979; it is retained below because it is current, reproducible, and still blocks the authoritative census validator. The twelve selected pull requests and the other two direct documentation commits produced no separate current concern, and no concern was explicitly excluded from this batch.

**Proposed reviewed PRs (12):** #1987, #1906, #1904, #1903, #1902, #1901, #1899, #1898, #1897, #1895, #1894, #1893.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_1987-1893.md=1987,1906,1904,1903,1902,1901,1899,1898,1897,1895,1894,1893`

### docs/project_review_1989-1834.md

**Helper candidates:** #1834, #1835, #1836, #1838, #1839, #1840, #1841, #1843, #1847, #1851, #1852, #1989.

**Helper reason:** docs/project_review_1989-1834.md: its opening paragraph carries a sentence this helper does not read: 'This report records the senior review of the next twelve uncovered merged pull requests in merge order — #1989, #1838, #1852, #1851, #1847, #1843, #1841, #1835, #1840, #1836, #1839, and #1834 — plus direct first-parent commits,,,,,,, and in the same landing interval.', so no row was written from it. Its candidate pull requests are #1834, #1835, #1836, #1838, #1839, #1840, #1841, #1843, #1847, #1851, #1852, #1989; confirm the reviewed ones with --confirm 'docs/project_review_1989-1834.md=<list>'.

**Source opening:** This report records the senior review of the next twelve uncovered merged pull requests in merge order — #1989, #1838, #1852, #1851, #1847, #1843, #1841, #1835, #1840, #1836, #1839, and #1834 — plus direct first-parent commits `4960d4d9`, `0dd0cdc8`, `99d73d07`, `83fddc35`, `91444631`, `19af28ea`, `1f591b9d`, and `dc470999` in the same landing interval. The review read each pull request, its linked specification where one existed, merged diff and commits, then traced the surviving behavior at current HEAD. The first three direct commits were mentioned in the preceding project-review report but were re-audited here because report reconciliation does not itself establish direct-commit cursor coverage. PR #1989 produced the one current concern below. The other eleven selected pull requests and all eight direct documentation commits produced no separate current concern, and no concern was explicitly excluded from this batch.

**Proposed reviewed PRs (12):** #1989, #1852, #1851, #1847, #1843, #1841, #1840, #1839, #1838, #1836, #1835, #1834.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_1989-1834.md=1989,1852,1851,1847,1843,1841,1840,1839,1838,1836,1835,1834`

### docs/project_review_199-166.md

**Helper candidates:** #163, #166, #168, #169, #170, #171, #174, #179, #181, #187, #192, #199.

**Helper reason:** docs/project_review_199-166.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in first-parent order — #199, #179, #192, #187, #171, #181, #174, #169, #170, #163, #168, and #166 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #163, #166, #168, #169, #170, #171, #174, #179, #181, #187, #192, #199; confirm the reviewed ones with --confirm 'docs/project_review_199-166.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in first-parent order — #199, #179, #192, #187, #171, #181, #174, #169, #170, #163, #168, and #166 — for later one-at-a-time disposition. The window contains exactly those twelve PR merges and no unrelated direct commits.

**Proposed reviewed PRs (12):** #199, #192, #187, #181, #179, #174, #171, #170, #169, #168, #166, #163.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_199-166.md=199,192,187,181,179,174,171,170,169,168,166,163`

### docs/project_review_2000-1827.md

**Helper candidates:** #1827, #1828, #1829, #1830, #1831, #1832, #1991, #1992, #1993, #1998, #1999, #2000.

**Helper reason:** docs/project_review_2000-1827.md: its opening paragraph carries a sentence this helper does not read: 'This report records the senior review of the next twelve uncovered merged pull requests in merge order — #2000, #1999, #1998, #1993, #1992, #1991, #1832, #1831, #1830, #1828, #1829, and #1827 — plus direct first-parent commits,,,,,,, and in the same landing interval.', so no row was written from it. Its candidate pull requests are #1827, #1828, #1829, #1830, #1831, #1832, #1991, #1992, #1993, #1998, #1999, #2000; confirm the reviewed ones with --confirm 'docs/project_review_2000-1827.md=<list>'.

**Source opening:** This report records the senior review of the next twelve uncovered merged pull requests in merge order — #2000, #1999, #1998, #1993, #1992, #1991, #1832, #1831, #1830, #1828, #1829, and #1827 — plus direct first-parent commits `4960d4d9`, `0dd0cdc8`, `99d73d07`, `83fddc35`, `91444631`, `19af28ea`, `1f591b9d`, and `dc470999` in the same landing interval. The review read each pull request, its linked specification, merged diff and commits, then traced the surviving behavior at current HEAD. PR #1993 produced the one current concern below. The other eleven selected pull requests and all eight direct documentation commits produced no separate current concern, and no concern was explicitly excluded from this batch.

**Proposed reviewed PRs (12):** #2000, #1999, #1998, #1993, #1992, #1991, #1832, #1831, #1830, #1829, #1828, #1827.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_2000-1827.md=2000,1999,1998,1993,1992,1991,1832,1831,1830,1829,1828,1827`

### docs/project_review_2002-1783.md

**Helper candidates:** #1783, #1784, #1792, #1794, #1795, #1797, #1798, #1799, #1800, #1801, #1802, #2002.

**Helper reason:** docs/project_review_2002-1783.md: its opening paragraph carries a sentence this helper does not read: 'This report records the senior review of the next twelve uncovered merged pull requests in merge-date order — #2002, #1802, #1801, #1800, #1799, #1798, #1797, #1795, #1794, #1792, #1784, and #1783 — plus direct first-parent documentation commits,,,,,,,, and in the same landing interval.', so no row was written from it. Its candidate pull requests are #1783, #1784, #1792, #1794, #1795, #1797, #1798, #1799, #1800, #1801, #1802, #2002; confirm the reviewed ones with --confirm 'docs/project_review_2002-1783.md=<list>'.

**Source opening:** This report records the senior review of the next twelve uncovered merged pull requests in merge-date order — #2002, #1802, #1801, #1800, #1799, #1798, #1797, #1795, #1794, #1792, #1784, and #1783 — plus direct first-parent documentation commits `dc470999`, `1f591b9d`, `19af28ea`, `91444631`, `83fddc35`, `99d73d07`, `0dd0cdc8`, `4960d4d9`, and `87ae3951` in the same landing interval. The review read each pull request, its linked specification where one existed, merged diff and commits, then traced the surviving behavior at current HEAD. The direct documentation commits retain their intended design, findings-report, probe-census, and project-review cursor roles; the known `text_encoding` census drift remains captured in `docs/project_review_1987-1893.md` and is not duplicated here. The other eleven selected pull requests produced no separate current concern, and no concern was explicitly excluded from this batch.

**Proposed reviewed PRs (12):** #2002, #1802, #1801, #1800, #1799, #1798, #1797, #1795, #1794, #1792, #1784, #1783.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_2002-1783.md=2002,1802,1801,1800,1799,1798,1797,1795,1794,1792,1784,1783`

### docs/project_review_2003-1754.md

**Helper candidates:** #1754, #1755, #1756, #1764, #1773, #1774, #1775, #1776, #1777, #1778, #1779, #1783, #2002, #2003.

**Helper reason:** docs/project_review_2003-1754.md: its opening paragraph carries a sentence this helper does not read: 'This report records the senior review of the next twelve uncovered merged pull requests in merge-date order — #2003, #1779, #1775, #1777, #1774, #1778, #1776, #1773, #1764, #1756, #1755, and #1754.', so no row was written from it. Its candidate pull requests are #1754, #1755, #1756, #1764, #1773, #1774, #1775, #1776, #1777, #1778, #1779, #1783, #2002, #2003; confirm the reviewed ones with --confirm 'docs/project_review_2003-1754.md=<list>'.

**Source opening:** This report records the senior review of the next twelve uncovered merged pull requests in merge-date order — #2003, #1779, #1775, #1777, #1774, #1778, #1776, #1773, #1764, #1756, #1755, and #1754. The review read each pull request, its linked specification, merged diff and commits, then traced the surviving behavior at current HEAD. The first-parent landing interval also contains direct documentation commits `3cf352c` and `ae68014`, audited here, plus nine documentation commits already individually audited in the overlapping #2002–#1783 batch (`dc470999`, `1f591b9d`, `19af28ea`, `91444631`, `83fddc35`, `99d73d07`, `0dd0cdc8`, `4960d4d9`, and `87ae3951`). Their current descendants retain their intended design, findings-disposition, probe-census, and review-record roles; the findings-report audit passes and the audio concept consolidation left no surviving reference to the deleted file.

**Proposed reviewed PRs (12):** #2003, #1779, #1778, #1777, #1776, #1775, #1774, #1773, #1764, #1756, #1755, #1754.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_2003-1754.md=2003,1779,1778,1777,1776,1775,1774,1773,1764,1756,1755,1754`

### docs/project_review_2004-1710.md

**Helper candidates:** #1700, #1710, #1725, #1726, #1727, #1728, #1741, #1742, #1749, #1751, #1753, #2004.

**Helper reason:** docs/project_review_2004-1710.md: its opening paragraph carries a sentence this helper does not read: 'This report records the senior review of the next twelve uncovered merged pull requests in merge-date order — #2004, #1753, #1751, #1749, #1742, #1741, #1727, #1728, #1726, #1725, #1700, and #1710.', so no row was written from it. Its candidate pull requests are #1700, #1710, #1725, #1726, #1727, #1728, #1741, #1742, #1749, #1751, #1753, #2004; confirm the reviewed ones with --confirm 'docs/project_review_2004-1710.md=<list>'.

**Source opening:** This report records the senior review of the next twelve uncovered merged pull requests in merge-date order — #2004, #1753, #1751, #1749, #1742, #1741, #1727, #1728, #1726, #1725, #1700, and #1710. The review read each pull request, its linked specification where one existed, merged diff and commits, then traced the surviving behavior at current HEAD. The first-parent landing interval also contains 32 direct documentation commits, audited here: `87ae3951`, `4960d4d9`, `0dd0cdc8`, `99d73d07`, `83fddc35`, `91444631`, `19af28ea`, `1f591b9d`, `dc470999`, `ae680144`, `3cf352c2`, `80821ee7`, `2174eacc`, `0f167e87`, `b8598932`, `abe25c1c`, `74c7c975`, `fa8248d6`, `6afb9bba`, `506f150a`, `f647359e`, `05ff148e`, `f9cb3fcd`, `1c1c9a61`, `95d7cee7`, `1f8fb793`, `10af3154`, `4fa04422`, `91ff70e2`, `1a92447f`, `339c7a41`, and `0a4cc084`. Their current descendants retain their intended design, findings-disposition, probe-census, bug-record, and project-review roles; none introduces production behavior.

**Proposed reviewed PRs (12):** #2004, #1753, #1751, #1749, #1742, #1741, #1728, #1727, #1726, #1725, #1710, #1700.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_2004-1710.md=2004,1753,1751,1749,1742,1741,1728,1727,1726,1725,1710,1700`

### docs/project_review_2007-1456.md

**Helper candidates:** #1455, #1456, #1457, #1458, #1459, #1460, #1461, #1462, #1463, #1464, #1465, #1466, #1467, #1468, #1469, #1470, #1472, #1473, #1477, #1478, #1480, #1489, #1491, #1495, #1496, #1497, #1498, #1499, #1500, #1501, #1502, #1503, #1504, #1505, #1506, #1507, #1508, #1509, #1510, #1511, #1512, #1513, #1514, #1515, #1516, #1517, #1518, #1519, #1520, #1521, #1522, #1523, #1526, #1527, #1528, #1529, #1530, #1531, #1532, #1533, #1534, #1548, #1549, #1550, #1551, #1552, #1553, #1554, #1555, #1556, #1557, #1558, #1559, #1560, #1561, #1562, #1563, #1564, #1565, #1566, #1567, #1568, #1574, #1601, #1606, #1984, #2007.

**Helper reason:** docs/project_review_2007-1456.md: its opening paragraph carries a sentence this helper does not read: 'This report records the senior review of the 85 merged pull requests above #1455 that had no durable prior-review evidence: #2007, #1606, #1601, #1574, #1568, #1567, #1566, #1565, #1564, #1563, #1562, #1561, #1560, #1559, #1558, #1550, #1557, #1556, #1555, #1554, #1553, #1552, #1551, #1549, #1548, #1534, #1532, #1533, #1531, #1530, #1529, #1528, #1527, #1526, #1523, #1521, #1522, #1520, #1519, #1518, #1517, #1516, #1515, #1514, #1513, #1511, #1512, #1509, #1510, #1508, #1507, #1506, #1505, #1504, #1503, #1502, #1501, #1500, #1499, #1498, #1497, #1496, #1495, #1480, #1478, #1477, #1491, #1473, #1489, #1472, #1470, #1469, #1468, #1467, #1466, #1465, #1464, #1463, #1462, #1461, #1460, #1459, #1458, #1457, and #1456, in merge-time order.', so no row was written from it. Its candidate pull requests are #1455, #1456, #1457, #1458, #1459, #1460, #1461, #1462, #1463, #1464, #1465, #1466, #1467, #1468, #1469, #1470, #1472, #1473, #1477, #1478, #1480, #1489, #1491, #1495, #1496, #1497, #1498, #1499, #1500, #1501, #1502, #1503, #1504, #1505, #1506, #1507, #1508, #1509, #1510, #1511, #1512, #1513, #1514, #1515, #1516, #1517, #1518, #1519, #1520, #1521, #1522, #1523, #1526, #1527, #1528, #1529, #1530, #1531, #1532, #1533, #1534, #1548, #1549, #1550, #1551, #1552, #1553, #1554, #1555, #1556, #1557, #1558, #1559, #1560, #1561, #1562, #1563, #1564, #1565, #1566, #1567, #1568, #1574, #1601, #1606, #1984, #2007; confirm the reviewed ones with --confirm 'docs/project_review_2007-1456.md=<list>'.

**Source opening:** This report records the senior review of the 85 merged pull requests above #1455 that had no durable prior-review evidence: #2007, #1606, #1601, #1574, #1568, #1567, #1566, #1565, #1564, #1563, #1562, #1561, #1560, #1559, #1558, #1550, #1557, #1556, #1555, #1554, #1553, #1552, #1551, #1549, #1548, #1534, #1532, #1533, #1531, #1530, #1529, #1528, #1527, #1526, #1523, #1521, #1522, #1520, #1519, #1518, #1517, #1516, #1515, #1514, #1513, #1511, #1512, #1509, #1510, #1508, #1507, #1506, #1505, #1504, #1503, #1502, #1501, #1500, #1499, #1498, #1497, #1496, #1495, #1480, #1478, #1477, #1491, #1473, #1489, #1472, #1470, #1469, #1468, #1467, #1466, #1465, #1464, #1463, #1462, #1461, #1460, #1459, #1458, #1457, and #1456, in merge-time order. Thirty other selector hits in the same numeric range were excluded because `docs/project_review_1642-1631.md`, `docs/project_review_1630-1614.md`, and `docs/project_review_1547-1535.md` explicitly record their prior review. The review read each selected pull request, its linked specification when present, its commits and merged diff, then traced the surviving behavior at current HEAD. It also classified all 162 direct first-parent commits in the landing interval `570607805a^..9f947f6870`; every one changes only `CLAUDE.md` or `docs/`, and none produced a separate current concern. PR #1568's consolidation invariant has since been violated by PR #1984, producing the one current concern below. The other 84 selected pull requests produced no separate current concern, and no concern was explicitly excluded from this batch.

**Proposed reviewed PRs (85):** #2007, #1606, #1601, #1574, #1568, #1567, #1566, #1565, #1564, #1563, #1562, #1561, #1560, #1559, #1558, #1557, #1556, #1555, #1554, #1553, #1552, #1551, #1550, #1549, #1548, #1534, #1533, #1532, #1531, #1530, #1529, #1528, #1527, #1526, #1523, #1522, #1521, #1520, #1519, #1518, #1517, #1516, #1515, #1514, #1513, #1512, #1511, #1510, #1509, #1508, #1507, #1506, #1505, #1504, #1503, #1502, #1501, #1500, #1499, #1498, #1497, #1496, #1495, #1491, #1489, #1480, #1478, #1477, #1473, #1472, #1470, #1469, #1468, #1467, #1466, #1465, #1464, #1463, #1462, #1461, #1460, #1459, #1458, #1457, #1456.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_2007-1456.md=2007,1606,1601,1574,1568,1567,1566,1565,1564,1563,1562,1561,1560,1559,1558,1557,1556,1555,1554,1553,1552,1551,1550,1549,1548,1534,1533,1532,1531,1530,1529,1528,1527,1526,1523,1522,1521,1520,1519,1518,1517,1516,1515,1514,1513,1512,1511,1510,1509,1508,1507,1506,1505,1504,1503,1502,1501,1500,1499,1498,1497,1496,1495,1491,1489,1480,1478,1477,1473,1472,1470,1469,1468,1467,1466,1465,1464,1463,1462,1461,1460,1459,1458,1457,1456`

### docs/project_review_211-200.md

**Helper candidates:** #188, #200, #201, #202, #203, #204, #205, #206, #207, #209, #210, #211.

**Helper reason:** docs/project_review_211-200.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in first-parent order — #211, #210, #209, #188, #207, #206, #205, #204, #203, #202, #201, and #200 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #188, #200, #201, #202, #203, #204, #205, #206, #207, #209, #210, #211; confirm the reviewed ones with --confirm 'docs/project_review_211-200.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in first-parent order — #211, #210, #209, #188, #207, #206, #205, #204, #203, #202, #201, and #200 — for later one-at-a-time disposition. The window contains exactly those twelve PR merges and no unrelated direct commits.

**Proposed reviewed PRs (12):** #211, #210, #209, #207, #206, #205, #204, #203, #202, #201, #200, #188.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_211-200.md=211,210,209,207,206,205,204,203,202,201,200,188`

### docs/project_review_237-208.md

**Helper candidates:** #208, #212, #226, #227, #228, #230, #231, #232, #233, #234, #235, #237.

**Helper reason:** docs/project_review_237-208.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in first-parent order — #237, #232, #235, #233, #234, #231, #228, #230, #227, #226, #212, and #208 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #208, #212, #226, #227, #228, #230, #231, #232, #233, #234, #235, #237; confirm the reviewed ones with --confirm 'docs/project_review_237-208.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in first-parent order — #237, #232, #235, #233, #234, #231, #228, #230, #227, #226, #212, and #208 — for later one-at-a-time disposition. The window contains exactly those twelve PR merges and no unrelated direct commits.

**Proposed reviewed PRs (12):** #237, #235, #234, #233, #232, #231, #230, #228, #227, #226, #212, #208.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_237-208.md=237,235,234,233,232,231,230,228,227,226,212,208`

### docs/project_review_2429-2413.md

**Helper candidates:** #2413, #2418, #2419, #2420, #2421, #2422, #2423, #2424, #2426, #2427, #2428, #2429.

**Helper reason:** docs/project_review_2429-2413.md: its opening paragraph carries a sentence this helper does not read: 'Reviewed against current source at: PRs #2429, #2428, #2427, #2426, #2424, #2423, #2422, #2421, #2418, #2420, #2419 and #2413, in merge order, plus direct documentation commit within their landing interval.', so no row was written from it. Its candidate pull requests are #2413, #2418, #2419, #2420, #2421, #2422, #2423, #2424, #2426, #2427, #2428, #2429; confirm the reviewed ones with --confirm 'docs/project_review_2429-2413.md=<list>'.

**Source opening:** Reviewed `coghex/synarchy` against current source at `e89e61bb112318765e44728fbbf31b86f3426499`: PRs #2429, #2428, #2427, #2426, #2424, #2423, #2422, #2421, #2418, #2420, #2419 and #2413, in merge order, plus direct documentation commit `88a952ab6` within their landing interval. This report preserves a current probe defect encountered while checking #2428's reported validation failure; the comment-only PR did not introduce it. No production code or tracker artifact was changed.

**Proposed reviewed PRs (12):** #2429, #2428, #2427, #2426, #2424, #2423, #2422, #2421, #2420, #2419, #2418, #2413.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_2429-2413.md=2429,2428,2427,2426,2424,2423,2422,2421,2420,2419,2418,2413`

### docs/project_review_2441-2425.md

**Helper candidates:** #2425, #2430, #2431, #2432, #2433, #2434, #2435, #2436, #2437, #2438, #2439, #2441.

**Helper reason:** docs/project_review_2441-2425.md: its opening paragraph carries a sentence this helper does not read: 'Reviewed twelve merged PRs in newest-first merge order: #2441, #2439, #2438, #2437, #2436, #2435, #2434, #2433, #2432, #2431, #2430, and #2425.', so no row was written from it. Its candidate pull requests are #2425, #2430, #2431, #2432, #2433, #2434, #2435, #2436, #2437, #2438, #2439, #2441; confirm the reviewed ones with --confirm 'docs/project_review_2441-2425.md=<list>'.

**Source opening:** Reviewed twelve merged PRs in newest-first merge order: #2441, #2439, #2438, #2437, #2436, #2435, #2434, #2433, #2432, #2431, #2430, and #2425. Review covered their linked issues, descriptions, commit messages, merged patches, and current code and consumers. Each GitHub diff matched its actual first-parent merge diff. The interval `6732510d8^..b615db242` contains exactly those twelve PR merges and no direct commits. No concern was excluded. Current failure-path verification was completed on 2026-09-07 at `e89e61bb1`; the only change since the freshly built `3e158738b` executable is unrelated capability-inventory documentation.

**Proposed reviewed PRs (12):** #2441, #2439, #2438, #2437, #2436, #2435, #2434, #2433, #2432, #2431, #2430, #2425.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_2441-2425.md=2441,2439,2438,2437,2436,2435,2434,2433,2432,2431,2430,2425`

### docs/project_review_2462-2405.md

**Helper candidates:** #2405, #2406, #2407, #2408, #2409, #2410, #2411, #2412, #2414, #2416, #2417, #2462.

**Helper reason:** docs/project_review_2462-2405.md: its opening paragraph carries a sentence this helper does not read: 'Reviewed PRs #2462, #2417, #2416, #2412, #2414, #2411, #2410, #2409, #2408, #2407, #2406, and #2405, including their linked specifications, commits, merged changes, and surviving code at.', so no row was written from it. Its candidate pull requests are #2405, #2406, #2407, #2408, #2409, #2410, #2411, #2412, #2414, #2416, #2417, #2462; confirm the reviewed ones with --confirm 'docs/project_review_2462-2405.md=<list>'.

**Source opening:** Reviewed `coghex/synarchy` PRs #2462, #2417, #2416, #2412, #2414, #2411, #2410, #2409, #2408, #2407, #2406, and #2405, including their linked specifications, commits, merged changes, and surviving code at `60c7168f2303299acbe1a4f0ec331e89fd0d4ebf`. The intervening direct documentation commits `e89e61bb112318765e44728fbbf31b86f3426499` and `7680c9ef6e73822c4e9bca1fa2016b90cf0f82b9` were also reviewed; other direct commits in the interval were covered in preceding batches. No implementation or tracker changes were made. GPU-dependent probe changes were assessed from their real control paths and focused offline tests, not claimed as newly verified rendered runs.

**Proposed reviewed PRs (12):** #2462, #2417, #2416, #2414, #2412, #2411, #2410, #2409, #2408, #2407, #2406, #2405.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_2462-2405.md=2462,2417,2416,2414,2412,2411,2410,2409,2408,2407,2406,2405`

### docs/project_review_2466-2453.md

**Helper candidates:** #2453, #2454, #2455, #2456, #2457, #2458, #2459, #2460, #2461, #2463, #2465, #2466, #2467.

**Helper reason:** docs/project_review_2466-2453.md: its opening paragraph carries a sentence this helper does not read: 'Reviewed twelve merged PRs in newest-first merge order: #2466, #2465, #2463, #2461, #2460, #2459, #2458, #2457, #2456, #2455, #2454, and #2453.', so no row was written from it. Its candidate pull requests are #2453, #2454, #2455, #2456, #2457, #2458, #2459, #2460, #2461, #2463, #2465, #2466, #2467; confirm the reviewed ones with --confirm 'docs/project_review_2466-2453.md=<list>'.

**Source opening:** Reviewed twelve merged PRs in newest-first merge order: #2466, #2465, #2463, #2461, #2460, #2459, #2458, #2457, #2456, #2455, #2454, and #2453. Each review covered the linked issue, PR description, commit messages, merged patch, and surviving code and consumers. All twelve GitHub diffs matched their actual first-parent merge diffs. The interval `145cee8cb^..903dd0e72` contains exactly those twelve PR merges and no direct commits. No concern was explicitly excluded. Current behavior was verified on 2026-09-07 at `3e158738b` (PR #2467 landed during this review and is not counted as reviewed).

**Proposed reviewed PRs (12):** #2466, #2465, #2463, #2461, #2460, #2459, #2458, #2457, #2456, #2455, #2454, #2453.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_2466-2453.md=2466,2465,2463,2461,2460,2459,2458,2457,2456,2455,2454,2453`

### docs/project_review_2475-2357.md

**Helper candidates:** #2357, #2358, #2359, #2360, #2361, #2362, #2363, #2364, #2365, #2469, #2472, #2475.

**Helper reason:** docs/project_review_2475-2357.md: its opening paragraph carries a sentence this helper does not read: 'Reviewed PRs #2475, #2469, #2472, #2365, #2364, #2363, #2359, #2362, #2360, #2361, #2358, and #2357 against their linked specifications, commits, first-parent landed patches, and surviving code at.', so no row was written from it. Its candidate pull requests are #2357, #2358, #2359, #2360, #2361, #2362, #2363, #2364, #2365, #2469, #2472, #2475; confirm the reviewed ones with --confirm 'docs/project_review_2475-2357.md=<list>'.

**Source opening:** Reviewed `coghex/synarchy` PRs #2475, #2469, #2472, #2365, #2364, #2363, #2359, #2362, #2360, #2361, #2358, and #2357 against their linked specifications, commits, first-parent landed patches, and surviving code at `167e3e88f093eaa2b9919d482cf2339b2b60b9b7`. Also reviewed the direct documentation publication at that commit; other direct commits within this interval were covered in previous batches. Existing findings in `docs/bugs.md` are not duplicated here. No implementation or tracker changes were made. GPU-dependent behavior was traced and checked with focused offline tests, not claimed as newly rendered evidence.

**Proposed reviewed PRs (12):** #2475, #2472, #2469, #2365, #2364, #2363, #2362, #2361, #2360, #2359, #2358, #2357.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_2475-2357.md=2475,2472,2469,2365,2364,2363,2362,2361,2360,2359,2358,2357`

### docs/project_review_249-236.md

**Helper candidates:** #229, #236, #238, #239, #240, #241, #242, #243, #245, #246, #247, #249.

**Helper reason:** docs/project_review_249-236.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #249, #247, #245, #241, #246, #243, #239, #240, #242, #238, #229, and #236 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #229, #236, #238, #239, #240, #241, #242, #243, #245, #246, #247, #249; confirm the reviewed ones with --confirm 'docs/project_review_249-236.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #249, #247, #245, #241, #246, #243, #239, #240, #242, #238, #229, and #236 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no unrelated direct commits.

**Proposed reviewed PRs (12):** #249, #247, #246, #245, #243, #242, #241, #240, #239, #238, #236, #229.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_249-236.md=249,247,246,245,243,242,241,240,239,238,236,229`

### docs/project_review_2493-2262.md

**Helper candidates:** #2254, #2262, #2268, #2270, #2271, #2279, #2281, #2287, #2289, #2295, #2296, #2377, #2493, #2532.

**Helper reason:** docs/project_review_2493-2262.md: its opening paragraph carries a sentence this helper does not read: 'Reviewed PRs #2493, #2296, #2295, #2289, #2287, #2281, #2279, #2254, #2271, #2270, #2268, and #2262 against their linked issues, commit messages, landed first-parent patches, and current descendants.', so no row was written from it. Its candidate pull requests are #2254, #2262, #2268, #2270, #2271, #2279, #2281, #2287, #2289, #2295, #2296, #2377, #2493, #2532; confirm the reviewed ones with --confirm 'docs/project_review_2493-2262.md=<list>'.

**Source opening:** Reviewed `coghex/synarchy` PRs #2493, #2296, #2295, #2289, #2287, #2281, #2279, #2254, #2271, #2270, #2268, and #2262 against their linked issues, commit messages, landed first-parent patches, and current descendants. Review began at `cfd30002dde1901f91ad7db251a07f1e01889330`; the subsequent #2532 landing is reserved for the next batch. The older landing interval contains PR merges only. Direct publication `53e448ac9` was also reviewed (build-wait policy, existing audit documentation, and previously captured findings/cursor publication). The excluded #2377 concern remains excluded. No implementation, tracker, or publication changes were made by this review.

**Proposed reviewed PRs (12):** #2493, #2296, #2295, #2289, #2287, #2281, #2279, #2271, #2270, #2268, #2262, #2254.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_2493-2262.md=2493,2296,2295,2289,2287,2281,2279,2271,2270,2268,2262,2254`

### docs/project_review_2494-2313.md

**Helper candidates:** #2309, #2312, #2313, #2318, #2319, #2320, #2321, #2322, #2331, #2340, #2341, #2377, #2494.

**Helper reason:** docs/project_review_2494-2313.md: its opening paragraph carries a sentence this helper does not read: 'Reviewed PRs #2494, #2341, #2340, #2331, #2309, #2312, #2322, #2321, #2320, #2319, #2318, and #2313 against their linked specifications, commits, first-parent landed patches, and surviving code.', so no row was written from it. Its candidate pull requests are #2309, #2312, #2313, #2318, #2319, #2320, #2321, #2322, #2331, #2340, #2341, #2377, #2494; confirm the reviewed ones with --confirm 'docs/project_review_2494-2313.md=<list>'.

**Source opening:** Reviewed `coghex/synarchy` PRs #2494, #2341, #2340, #2331, #2309, #2312, #2322, #2321, #2320, #2319, #2318, and #2313 against their linked specifications, commits, first-parent landed patches, and surviving code. Verification completed at `cfd30002dde1901f91ad7db251a07f1e01889330`; the two newest landings since initial inspection changed documentation and comments only. The older landing interval contains PR merges only; intervening direct publications through #2494 were covered in preceding batches. Newer landings are selected in the next batch. The previously excluded #2377 concern remains excluded. No implementation or tracker changes were made.

**Proposed reviewed PRs (12):** #2494, #2341, #2340, #2331, #2322, #2321, #2320, #2319, #2318, #2313, #2312, #2309.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_2494-2313.md=2494,2341,2340,2331,2322,2321,2320,2319,2318,2313,2312,2309`

### docs/project_review_2508-2239.md

**Helper candidates:** #2239, #2242, #2245, #2246, #2247, #2248, #2249, #2250, #2252, #2253, #2255, #2377, #2508, #2537.

**Helper reason:** docs/project_review_2508-2239.md: its opening paragraph carries a sentence this helper does not read: 'Reviewed PRs #2508, #2255, #2253, #2252, #2250, #2249, #2248, #2246, #2247, #2245, #2242, and #2239 against their linked specifications, commit messages, landed patches, and current callers.', so no row was written from it. Its candidate pull requests are #2239, #2242, #2245, #2246, #2247, #2248, #2249, #2250, #2252, #2253, #2255, #2377, #2508, #2537; confirm the reviewed ones with --confirm 'docs/project_review_2508-2239.md=<list>'.

**Source opening:** Reviewed `coghex/synarchy` PRs #2508, #2255, #2253, #2252, #2250, #2249, #2248, #2246, #2247, #2245, #2242, and #2239 against their linked specifications, commit messages, landed patches, and current callers. Their GitHub patches match their first-parent landing diffs after stripping diff metadata. The older landing interval contains only PR merges; #2508's new landing was included separately. Verification used `57a6a4385`, with the finding rechecked at `7b2833f79`; the intervening #2537 addition does not touch its failure path. The previously excluded #2377 concern stays excluded. No implementation or tracker was changed.

**Proposed reviewed PRs (12):** #2508, #2255, #2253, #2252, #2250, #2249, #2248, #2247, #2246, #2245, #2242, #2239.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_2508-2239.md=2508,2255,2253,2252,2250,2249,2248,2247,2246,2245,2242,2239`

### docs/project_review_2532-2256.md

**Helper candidates:** #2256, #2257, #2258, #2259, #2260, #2261, #2263, #2264, #2265, #2266, #2267, #2377, #2532.

**Helper reason:** docs/project_review_2532-2256.md: its opening paragraph carries a sentence this helper does not read: 'Reviewed PRs #2532, #2267, #2266, #2265, #2264, #2263, #2261, #2260, #2259, #2258, #2257, and #2256 against their linked specifications, commit messages, landed patches, and current code.', so no row was written from it. Its candidate pull requests are #2256, #2257, #2258, #2259, #2260, #2261, #2263, #2264, #2265, #2266, #2267, #2377, #2532; confirm the reviewed ones with --confirm 'docs/project_review_2532-2256.md=<list>'.

**Source opening:** Reviewed `coghex/synarchy` PRs #2532, #2267, #2266, #2265, #2264, #2263, #2261, #2260, #2259, #2258, #2257, and #2256 against their linked specifications, commit messages, landed patches, and current code. The older landing interval contains only PR merges; the new #2532 landing was also included. Verification used `28bd42c0140698efd28e9bd8433073583307b8d1`; subsequent merges are reserved for the next batch. The previously excluded #2377 concern remains excluded. No implementation or tracker was changed.

**Proposed reviewed PRs (12):** #2532, #2267, #2266, #2265, #2264, #2263, #2261, #2260, #2259, #2258, #2257, #2256.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_2532-2256.md=2532,2267,2266,2265,2264,2263,2261,2260,2259,2258,2257,2256`

### docs/project_review_2537-2196.md

**Helper candidates:** #2133, #2181, #2190, #2196, #2197, #2200, #2207, #2222, #2235, #2237, #2238, #2377, #2537.

**Helper reason:** docs/project_review_2537-2196.md: its opening paragraph carries a sentence this helper does not read: 'Reviewed PRs #2537, #2238, #2237, #2235, #2190, #2222, #2207, #2200, #2197, #2181, #2133, and #2196 against their specifications, commit messages, landed patches, and current consumers.', so no row was written from it. Its candidate pull requests are #2133, #2181, #2190, #2196, #2197, #2200, #2207, #2222, #2235, #2237, #2238, #2377, #2537; confirm the reviewed ones with --confirm 'docs/project_review_2537-2196.md=<list>'.

**Source opening:** Reviewed `coghex/synarchy` PRs #2537, #2238, #2237, #2235, #2190, #2222, #2207, #2200, #2197, #2181, #2133, and #2196 against their specifications, commit messages, landed patches, and current consumers. Their GitHub patches agree with their first-parent landing diffs after normalizing diff metadata. Also reviewed direct documentation commits `ea2ad316ddac9d23788111a362d8df7b421022d4` and `f4f2bb699d6c4012a771f35baafdf4a2a8dedbb3` in the older landing interval. Verification used the package rebuilt at `7b2833f79`; the three failure paths remain unchanged at `92948204f`. The previously excluded #2377 concern remains excluded. No implementation or tracker was changed.

**Proposed reviewed PRs (12):** #2537, #2238, #2237, #2235, #2222, #2207, #2200, #2197, #2196, #2190, #2181, #2133.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_2537-2196.md=2537,2238,2237,2235,2222,2207,2200,2197,2196,2190,2181,2133`

### docs/project_review_2542-2144.md

**Helper candidates:** #2121, #2125, #2144, #2146, #2152, #2153, #2154, #2158, #2178, #2191, #2377, #2540, #2542.

**Helper reason:** docs/project_review_2542-2144.md: its opening paragraph carries a sentence this helper does not read: 'Reviewed PRs #2542, #2540, #2191, #2153, #2178, #2158, #2154, #2125, #2121, #2152, #2146, and #2144 against their specifications, commit messages, landed patches, and current consumers.', so no row was written from it. Its candidate pull requests are #2121, #2125, #2144, #2146, #2152, #2153, #2154, #2158, #2178, #2191, #2377, #2540, #2542; confirm the reviewed ones with --confirm 'docs/project_review_2542-2144.md=<list>'.

**Source opening:** Reviewed `coghex/synarchy` PRs #2542, #2540, #2191, #2153, #2178, #2158, #2154, #2125, #2121, #2152, #2146, and #2144 against their specifications, commit messages, landed patches, and current consumers. Checked the first-parent landing intervals; there were no additional direct commits to review. Large truncated GitHub patch output was recovered from the first-parent landing diff. Verification used the package rebuilt at `92948204f`; the finding paths are unchanged at `12d747399`. The previously excluded #2377 concern remains excluded. No implementation or tracker was changed.

**Proposed reviewed PRs (12):** #2542, #2540, #2191, #2178, #2158, #2154, #2153, #2152, #2146, #2144, #2125, #2121.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_2542-2144.md=2542,2540,2191,2178,2158,2154,2153,2152,2146,2144,2125,2121`

### docs/project_review_2543-2119.md

**Helper candidates:** #1423, #2119, #2120, #2122, #2123, #2127, #2132, #2134, #2136, #2137, #2139, #2143, #2377, #2543.

**Helper reason:** docs/project_review_2543-2119.md: its opening paragraph carries a sentence this helper does not read: 'Completed the paused 17th batch in: PRs #2543, #2143, #2139, #2137, #2136, #2134, #2127, #2132, #2123, #2122, #2120, and #2119.', so no row was written from it. Its candidate pull requests are #1423, #2119, #2120, #2122, #2123, #2127, #2132, #2134, #2136, #2137, #2139, #2143, #2377, #2543; confirm the reviewed ones with --confirm 'docs/project_review_2543-2119.md=<list>'.

**Source opening:** Completed the paused 17th batch in `coghex/synarchy`: PRs #2543, #2143, #2139, #2137, #2136, #2134, #2127, #2132, #2123, #2122, #2120, and #2119. These are the exact twelve selected before the pause, not every PR in the numeric interval. Reviewed their linked requirements where present, individual commit messages, landed first-parent patches, and current implementations and consumers. The landing intervals contained no additional direct commits requiring review. The previously excluded #2377 concern was not revisited. This batch ends here; the older stop remains PR #1423.

**Proposed reviewed PRs (12):** #2543, #2143, #2139, #2137, #2136, #2134, #2132, #2127, #2123, #2122, #2120, #2119.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_2543-2119.md=2543,2143,2139,2137,2136,2134,2132,2127,2123,2122,2120,2119`

### docs/project_review_262-248.md

**Helper candidates:** #244, #248, #250, #251, #252, #253, #255, #256, #257, #258, #259, #262.

**Helper reason:** docs/project_review_262-248.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #262, #259, #258, #257, #256, #255, #252, #253, #251, #250, #244, and #248 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #244, #248, #250, #251, #252, #253, #255, #256, #257, #258, #259, #262; confirm the reviewed ones with --confirm 'docs/project_review_262-248.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #262, #259, #258, #257, #256, #255, #252, #253, #251, #250, #244, and #248 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no unrelated direct commits; #251 is represented by its squash commit.

**Proposed reviewed PRs (12):** #262, #259, #258, #257, #256, #255, #253, #252, #251, #250, #248, #244.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_262-248.md=262,259,258,257,256,255,253,252,251,250,248,244`

### docs/project_review_279-261.md

**Helper candidates:** #260, #261, #263, #266, #267, #268, #269, #270, #271, #272, #278, #279.

**Helper reason:** docs/project_review_279-261.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #279, #278, #272, #271, #260, #269, #268, #270, #266, #267, #263, and #261 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #260, #261, #263, #266, #267, #268, #269, #270, #271, #272, #278, #279; confirm the reviewed ones with --confirm 'docs/project_review_279-261.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #279, #278, #272, #271, #260, #269, #268, #270, #266, #267, #263, and #261 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no direct commits.

**Proposed reviewed PRs (12):** #279, #278, #272, #271, #270, #269, #268, #267, #266, #263, #261, #260.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_279-261.md=279,278,272,271,270,269,268,267,266,263,261,260`

### docs/project_review_292-281.md

**Helper candidates:** #280, #281, #282, #283, #284, #285, #287, #288, #289, #290, #291, #292.

**Helper reason:** docs/project_review_292-281.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #292, #291, #290, #288, #287, #289, #284, #283, #285, #282, #280, and #281 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #280, #281, #282, #283, #284, #285, #287, #288, #289, #290, #291, #292; confirm the reviewed ones with --confirm 'docs/project_review_292-281.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #292, #291, #290, #288, #287, #289, #284, #283, #285, #282, #280, and #281 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no direct commits.

**Proposed reviewed PRs (12):** #292, #291, #290, #289, #288, #287, #285, #284, #283, #282, #281, #280.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_292-281.md=292,291,290,289,288,287,285,284,283,282,281,280`

### docs/project_review_32-14.md

**Helper candidates:** #14, #16, #17, #32, #33.

**Helper reason:** docs/project_review_32-14.md: its opening paragraph carries a sentence this helper does not read: "These entries record focused evidence from the senior review of the repository's terminal merged-PR window in first-parent order: #32, #17, #16, and #14.", so no row was written from it. Its candidate pull requests are #14, #16, #17, #32, #33; confirm the reviewed ones with --confirm 'docs/project_review_32-14.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the repository's terminal merged-PR window in first-parent order: #32, #17, #16, and #14. There are no other merged PRs before #33. Because early development was mostly landed as bare commits, the exact interval contains 625 first-parent commits: 23 PR-owned commits (PR #14 alone was rebased as 18 commits) plus 602 direct commits between PR #33's parent and PR #14's base. The direct commits were inventoried with the PRs rather than silently omitted.

**Proposed reviewed PRs (4):** #32, #17, #16, #14.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_32-14.md=32,17,16,14`

### docs/project_review_341-296.md

**Helper candidates:** #293, #294, #295, #296, #310, #311, #314, #318, #320, #322, #339, #341.

**Helper reason:** docs/project_review_341-296.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #341, #339, #320, #310, #318, #322, #314, #311, #293, #294, #295, and #296 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #293, #294, #295, #296, #310, #311, #314, #318, #320, #322, #339, #341; confirm the reviewed ones with --confirm 'docs/project_review_341-296.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #341, #339, #320, #310, #318, #322, #314, #311, #293, #294, #295, and #296 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no direct commits.

**Proposed reviewed PRs (12):** #341, #339, #322, #320, #318, #314, #311, #310, #296, #295, #294, #293.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_341-296.md=341,339,322,320,318,314,311,310,296,295,294,293`

### docs/project_review_398-348.md

**Helper candidates:** #338, #340, #348, #354, #355, #356, #362, #363, #364, #366, #368, #398.

**Helper reason:** docs/project_review_398-348.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #398, #368, #364, #366, #362, #355, #363, #356, #354, #340, #338, and #348 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #338, #340, #348, #354, #355, #356, #362, #363, #364, #366, #368, #398; confirm the reviewed ones with --confirm 'docs/project_review_398-348.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #398, #368, #364, #366, #362, #355, #363, #356, #354, #340, #338, and #348 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no direct commits.

**Proposed reviewed PRs (12):** #398, #368, #366, #364, #363, #362, #356, #355, #354, #348, #340, #338.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_398-348.md=398,368,366,364,363,362,356,355,354,348,340,338`

### docs/project_review_411-399.md

**Helper candidates:** #399, #400, #401, #402, #404, #405, #406, #407, #408, #409, #410, #411.

**Helper reason:** docs/project_review_411-399.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #411, #410, #409, #401, #408, #407, #404, #405, #406, #402, #400, and #399 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #399, #400, #401, #402, #404, #405, #406, #407, #408, #409, #410, #411; confirm the reviewed ones with --confirm 'docs/project_review_411-399.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #411, #410, #409, #401, #408, #407, #404, #405, #406, #402, #400, and #399 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no direct commits.

**Proposed reviewed PRs (12):** #411, #410, #409, #408, #407, #406, #405, #404, #402, #401, #400, #399.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_411-399.md=411,410,409,408,407,406,405,404,402,401,400,399`

### docs/project_review_432-412.md

**Helper candidates:** #412, #413, #416, #417, #419, #420, #425, #429, #430, #431, #432, #442, #444.

**Helper reason:** docs/project_review_432-412.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #432, #442, #444, #431, #430, #425, #420, #419, #417, #416, #413, and #412 — plus direct first-parent documentation commit \x00, for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #412, #413, #416, #417, #419, #420, #425, #429, #430, #431, #432, #442, #444; confirm the reviewed ones with --confirm 'docs/project_review_432-412.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #432, #442, #444, #431, #430, #425, #420, #419, #417, #416, #413, and #412 — plus direct first-parent documentation commit `07015dbb` (#429), for later one-at-a-time disposition.

**Proposed reviewed PRs (13):** #444, #442, #432, #431, #430, #429, #425, #420, #419, #417, #416, #413, #412.

**Basis:** The twelve explicit PRs plus #429: the report says it reviewed commit 07015dbb and checks its documentation at line 7; GitHub confirms this exact commit is merged PR #429, despite the report calling it a direct commit.

**Exact confirmation:** `docs/project_review_432-412.md=444,442,432,431,430,429,425,420,419,417,416,413,412`

### docs/project_review_459-450.md

**Helper candidates:** #449, #450, #451, #452, #453, #454, #455, #456, #457, #458, #459, #460.

**Helper reason:** docs/project_review_459-450.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #459, #460, #458, #457, #456, #455, #454, #449, #453, #452, #451, and #450 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #449, #450, #451, #452, #453, #454, #455, #456, #457, #458, #459, #460; confirm the reviewed ones with --confirm 'docs/project_review_459-450.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #459, #460, #458, #457, #456, #455, #454, #449, #453, #452, #451, and #450 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no direct commits.

**Proposed reviewed PRs (12):** #460, #459, #458, #457, #456, #455, #454, #453, #452, #451, #450, #449.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_459-450.md=460,459,458,457,456,455,454,453,452,451,450,449`

### docs/project_review_474-461.md

**Helper candidates:** #461, #462, #463, #464, #465, #467, #468, #469, #470, #472, #473, #474.

**Helper reason:** docs/project_review_474-461.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #474, #473, #472, #470, #469, #468, #467, #465, #464, #463, #462, and #461 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #461, #462, #463, #464, #465, #467, #468, #469, #470, #472, #473, #474; confirm the reviewed ones with --confirm 'docs/project_review_474-461.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #474, #473, #472, #470, #469, #468, #467, #465, #464, #463, #462, and #461 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no direct commits.

**Proposed reviewed PRs (12):** #474, #473, #472, #470, #469, #468, #467, #465, #464, #463, #462, #461.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_474-461.md=474,473,472,470,469,468,467,465,464,463,462,461`

### docs/project_review_491-475.md

**Helper candidates:** #475, #476, #477, #480, #481, #482, #484, #486, #487, #488, #490, #491.

**Helper reason:** docs/project_review_491-475.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #491, #488, #490, #487, #486, #484, #482, #481, #480, #477, #476, and #475 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #475, #476, #477, #480, #481, #482, #484, #486, #487, #488, #490, #491; confirm the reviewed ones with --confirm 'docs/project_review_491-475.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #491, #488, #490, #487, #486, #484, #482, #481, #480, #477, #476, and #475 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no direct commits.

**Proposed reviewed PRs (12):** #491, #490, #488, #487, #486, #484, #482, #481, #480, #477, #476, #475.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_491-475.md=491,490,488,487,486,484,482,481,480,477,476,475`

### docs/project_review_504-492.md

**Helper candidates:** #492, #493, #494, #495, #496, #497, #498, #499, #501, #502, #503, #504.

**Helper reason:** docs/project_review_504-492.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #504, #503, #502, #501, #499, #498, #497, #496, #495, #494, #493, and #492 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #492, #493, #494, #495, #496, #497, #498, #499, #501, #502, #503, #504; confirm the reviewed ones with --confirm 'docs/project_review_504-492.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #504, #503, #502, #501, #499, #498, #497, #496, #495, #494, #493, and #492 — for later one-at-a-time disposition. The first-parent window also contains two direct commits between #494 and #493: `db44dfc9` (notification settings) and `d9ce2f82` (unit texture assets).

**Proposed reviewed PRs (12):** #504, #503, #502, #501, #499, #498, #497, #496, #495, #494, #493, #492.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_504-492.md=504,503,502,501,499,498,497,496,495,494,493,492`

### docs/project_review_516-505.md

**Helper candidates:** #505, #506, #507, #508, #509, #510, #511, #512, #513, #514, #515, #516.

**Helper reason:** docs/project_review_516-505.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #516, #515, #514, #513, #512, #511, #510, #509, #508, #507, #506, and #505 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #505, #506, #507, #508, #509, #510, #511, #512, #513, #514, #515, #516; confirm the reviewed ones with --confirm 'docs/project_review_516-505.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #516, #515, #514, #513, #512, #511, #510, #509, #508, #507, #506, and #505 — for later one-at-a-time disposition. The first-parent window contains no direct commits between those merges.

**Proposed reviewed PRs (12):** #516, #515, #514, #513, #512, #511, #510, #509, #508, #507, #506, #505.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_516-505.md=516,515,514,513,512,511,510,509,508,507,506,505`

### docs/project_review_534-518.md

**Helper candidates:** #518, #519, #520, #521, #522, #523, #524, #525, #528, #532, #533, #534.

**Helper reason:** docs/project_review_534-518.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #534, #533, #528, #532, #525, #524, #523, #522, #521, #520, #519, and #518 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #518, #519, #520, #521, #522, #523, #524, #525, #528, #532, #533, #534; confirm the reviewed ones with --confirm 'docs/project_review_534-518.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #534, #533, #528, #532, #525, #524, #523, #522, #521, #520, #519, and #518 — for later one-at-a-time disposition. The same first-parent window also contains direct commits `73f5a546` (`notify timeout`), `418a58d8` (`updating markdown files`), and `4c1d800c` (`mac os cabal hook`).

**Proposed reviewed PRs (12):** #534, #533, #532, #528, #525, #524, #523, #522, #521, #520, #519, #518.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_534-518.md=534,533,532,528,525,524,523,522,521,520,519,518`

### docs/project_review_609-535.md

**Helper candidates:** #535, #536, #594, #595, #597, #598, #600, #601, #602, #605, #608, #609.

**Helper reason:** docs/project_review_609-535.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #609, #608, #605, #602, #601, #600, #598, #597, #595, #594, #536, and #535 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #535, #536, #594, #595, #597, #598, #600, #601, #602, #605, #608, #609; confirm the reviewed ones with --confirm 'docs/project_review_609-535.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #609, #608, #605, #602, #601, #600, #598, #597, #595, #594, #536, and #535 — for later one-at-a-time disposition. The same first-parent window also contains direct commits `b09c1518` (`CI updates`) and `eaee85c3` (`quick comment`).

**Proposed reviewed PRs (12):** #609, #608, #605, #602, #601, #600, #598, #597, #595, #594, #536, #535.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_609-535.md=609,608,605,602,601,600,598,597,595,594,536,535`

### docs/project_review_627-614.md

**Helper candidates:** #614, #615, #616, #617, #619, #620, #621, #623, #624, #625, #626, #627.

**Helper reason:** docs/project_review_627-614.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #627, #626, #625, #624, #623, #621, #620, #619, #617, #616, #615, and #614 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #614, #615, #616, #617, #619, #620, #621, #623, #624, #625, #626, #627; confirm the reviewed ones with --confirm 'docs/project_review_627-614.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #627, #626, #625, #624, #623, #621, #620, #619, #617, #616, #615, and #614 — for later one-at-a-time disposition. The same first-parent window also contains direct CI-comment commit `e0e55105`.

**Proposed reviewed PRs (12):** #627, #626, #625, #624, #623, #621, #620, #619, #617, #616, #615, #614.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_627-614.md=627,626,625,624,623,621,620,619,617,616,615,614`

### docs/project_review_654-628.md

**Helper candidates:** #628, #629, #630, #631, #633, #634, #637, #639, #640, #651, #653, #654.

**Helper reason:** docs/project_review_654-628.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #654, #653, #640, #639, #651, #637, #634, #633, #631, #630, #629, and #628 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #628, #629, #630, #631, #633, #634, #637, #639, #640, #651, #653, #654; confirm the reviewed ones with --confirm 'docs/project_review_654-628.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #654, #653, #640, #639, #651, #637, #634, #633, #631, #630, #629, and #628 — for later one-at-a-time disposition. The same first-parent window also contains ten direct CI commits (`4233b2c5`, `d82718fc`, `86292c15`, `7f1e679b`, `b5108075`, `c2458a76`, `0fdd9187`, `1f3b7cda`, `bceade0d`, and `b9ef2643`) from the temporary fixed-image/ghcup repair sequence.

**Proposed reviewed PRs (12):** #654, #653, #651, #640, #639, #637, #634, #633, #631, #630, #629, #628.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_654-628.md=654,653,651,640,639,637,634,633,631,630,629,628`

### docs/project_review_668-655.md

**Helper candidates:** #655, #656, #657, #658, #659, #660, #661, #662, #663, #666, #667, #668.

**Helper reason:** docs/project_review_668-655.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #668, #667, #666, #663, #662, #661, #660, #659, #658, #657, #656, and #655 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #655, #656, #657, #658, #659, #660, #661, #662, #663, #666, #667, #668; confirm the reviewed ones with --confirm 'docs/project_review_668-655.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #668, #667, #666, #663, #662, #661, #660, #659, #658, #657, #656, and #655 — for later one-at-a-time disposition. The first-parent window contains no direct non-PR commits.

**Proposed reviewed PRs (12):** #668, #667, #666, #663, #662, #661, #660, #659, #658, #657, #656, #655.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_668-655.md=668,667,666,663,662,661,660,659,658,657,656,655`

### docs/project_review_681-669.md

**Helper candidates:** #669, #670, #672, #673, #674, #675, #676, #677, #678, #679, #680, #681.

**Helper reason:** docs/project_review_681-669.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #681, #680, #679, #678, #677, #676, #675, #674, #673, #672, #670, and #669 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #669, #670, #672, #673, #674, #675, #676, #677, #678, #679, #680, #681; confirm the reviewed ones with --confirm 'docs/project_review_681-669.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #681, #680, #679, #678, #677, #676, #675, #674, #673, #672, #670, and #669 — for later one-at-a-time disposition. The first-parent window contains no direct non-PR commits.

**Proposed reviewed PRs (12):** #681, #680, #679, #678, #677, #676, #675, #674, #673, #672, #670, #669.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_681-669.md=681,680,679,678,677,676,675,674,673,672,670,669`

### docs/project_review_693-682.md

**Helper candidates:** #682, #683, #684, #685, #686, #687, #688, #689, #690, #691, #692, #693.

**Helper reason:** docs/project_review_693-682.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #693, #692, #691, #690, #689, #688, #687, #686, #685, #684, #683, and #682 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #682, #683, #684, #685, #686, #687, #688, #689, #690, #691, #692, #693; confirm the reviewed ones with --confirm 'docs/project_review_693-682.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #693, #692, #691, #690, #689, #688, #687, #686, #685, #684, #683, and #682 — for later one-at-a-time disposition. The first-parent window contains no direct non-PR commits.

**Proposed reviewed PRs (12):** #693, #692, #691, #690, #689, #688, #687, #686, #685, #684, #683, #682.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_693-682.md=693,692,691,690,689,688,687,686,685,684,683,682`

### docs/project_review_71-33.md

**Helper candidates:** #27, #33, #34, #40, #41, #47, #50, #51, #52, #57, #62, #63, #71.

**Helper reason:** docs/project_review_71-33.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in first-parent order — #71, #63, #50, #62, #57, #52, #51, #47, #41, #40, #34, and #33 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #27, #33, #34, #40, #41, #47, #50, #51, #52, #57, #62, #63, #71; confirm the reviewed ones with --confirm 'docs/project_review_71-33.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in first-parent order — #71, #63, #50, #62, #57, #52, #51, #47, #41, #40, #34, and #33 — for later one-at-a-time disposition. The same interval also contains the direct commits `a6eb9c3` (issue #27 river-mouth classification), `9c3a61a` (repository guidance and documentation), and `96cccbf` (world-view structural-texture rebinding), which were reviewed with the PRs rather than omitted between windows.

**Proposed reviewed PRs (12):** #71, #63, #62, #57, #52, #51, #50, #47, #41, #40, #34, #33.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_71-33.md=71,63,62,57,52,51,50,47,41,40,34,33`

### docs/project_review_715-694.md

**Helper candidates:** #694, #695, #696, #701, #702, #703, #704, #705, #711, #712, #714, #715.

**Helper reason:** docs/project_review_715-694.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #715, #714, #712, #711, #704, #705, #701, #703, #702, #696, #695, and #694 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #694, #695, #696, #701, #702, #703, #704, #705, #711, #712, #714, #715; confirm the reviewed ones with --confirm 'docs/project_review_715-694.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #715, #714, #712, #711, #704, #705, #701, #703, #702, #696, #695, and #694 — for later one-at-a-time disposition. The same first-parent window contains one direct non-PR commit, `aa18b24b` (`Document July 2026 project assessment`); it added an explicitly archived history document and introduced no separate current concern.

**Proposed reviewed PRs (12):** #715, #714, #712, #711, #705, #704, #703, #702, #701, #696, #695, #694.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_715-694.md=715,714,712,711,705,704,703,702,701,696,695,694`

### docs/project_review_739-716.md

**Helper candidates:** #716, #718, #719, #720, #731, #732, #733, #734, #735, #736, #738, #739.

**Helper reason:** docs/project_review_739-716.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #739, #738, #736, #735, #732, #734, #733, #731, #719, #720, #718, and #716 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #716, #718, #719, #720, #731, #732, #733, #734, #735, #736, #738, #739; confirm the reviewed ones with --confirm 'docs/project_review_739-716.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #739, #738, #736, #735, #732, #734, #733, #731, #719, #720, #718, and #716 — for later one-at-a-time disposition. The first-parent window contains no direct non-PR commits.

**Proposed reviewed PRs (12):** #739, #738, #736, #735, #734, #733, #732, #731, #720, #719, #718, #716.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_739-716.md=739,738,736,735,734,733,732,731,720,719,718,716`

### docs/project_review_792-740.md

**Helper candidates:** #737, #740, #751, #752, #753, #754, #755, #765, #769, #770, #791, #792.

**Helper reason:** docs/project_review_792-740.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #792, #791, #769, #770, #765, #755, #754, #753, #752, #751, #737, and #740 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #737, #740, #751, #752, #753, #754, #755, #765, #769, #770, #791, #792; confirm the reviewed ones with --confirm 'docs/project_review_792-740.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #792, #791, #769, #770, #765, #755, #754, #753, #752, #751, #737, and #740 — for later one-at-a-time disposition. The first-parent window contains no direct non-PR commits.

**Proposed reviewed PRs (12):** #792, #791, #770, #769, #765, #755, #754, #753, #752, #751, #740, #737.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_792-740.md=792,791,770,769,765,755,754,753,752,751,740,737`

### docs/project_review_823-789.md

**Helper candidates:** #789, #803, #804, #808, #809, #810, #817, #818, #819, #820, #821, #823.

**Helper reason:** docs/project_review_823-789.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #823, #821, #820, #819, #810, #818, #817, #809, #808, #804, #803, and #789 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #789, #803, #804, #808, #809, #810, #817, #818, #819, #820, #821, #823; confirm the reviewed ones with --confirm 'docs/project_review_823-789.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #823, #821, #820, #819, #810, #818, #817, #809, #808, #804, #803, and #789 — for later one-at-a-time disposition. The first-parent window contains no direct non-PR commits.

**Proposed reviewed PRs (12):** #823, #821, #820, #819, #818, #817, #810, #809, #808, #804, #803, #789.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_823-789.md=823,821,820,819,818,817,810,809,808,804,803,789`

### docs/project_review_835-822.md

**Helper candidates:** #822, #824, #825, #826, #827, #828, #829, #830, #831, #832, #833, #835.

**Helper reason:** docs/project_review_835-822.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #835, #832, #833, #831, #830, #829, #828, #827, #826, #824, #825, and #822 — for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #822, #824, #825, #826, #827, #828, #829, #830, #831, #832, #833, #835; confirm the reviewed ones with --confirm 'docs/project_review_835-822.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #835, #832, #833, #831, #830, #829, #828, #827, #826, #824, #825, and #822 — for later one-at-a-time disposition. The first-parent window contains no direct non-PR commits.

**Proposed reviewed PRs (12):** #835, #833, #832, #831, #830, #829, #828, #827, #826, #825, #824, #822.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_835-822.md=835,833,832,831,830,829,828,827,826,825,824,822`

### docs/project_review_847-834.md

**Helper candidates:** #834, #836, #837, #838, #839, #840, #841, #842, #843, #845, #846, #847.

**Helper reason:** docs/project_review_847-834.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #847, #846, #845, #840, #843, #842, #841, #838, #839, #837, #836, and #834 — plus direct first-parent commit in the same window, for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #834, #836, #837, #838, #839, #840, #841, #842, #843, #845, #846, #847; confirm the reviewed ones with --confirm 'docs/project_review_847-834.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #847, #846, #845, #840, #843, #842, #841, #838, #839, #837, #836, and #834 — plus direct first-parent commit `e067378a` in the same window, for later one-at-a-time disposition.

**Proposed reviewed PRs (12):** #847, #846, #845, #843, #842, #841, #840, #839, #838, #837, #836, #834.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_847-834.md=847,846,845,843,842,841,840,839,838,837,836,834`

### docs/project_review_859-848.md

**Helper candidates:** #848, #849, #850, #851, #852, #853, #854, #855, #856, #857, #858, #859.

**Helper reason:** docs/project_review_859-848.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #859, #858, #856, #857, #855, #854, #852, #853, #851, #850, #849, and #848 — plus the two direct first-parent commits \x00 in the same window, for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #848, #849, #850, #851, #852, #853, #854, #855, #856, #857, #858, #859; confirm the reviewed ones with --confirm 'docs/project_review_859-848.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #859, #858, #856, #857, #855, #854, #852, #853, #851, #850, #849, and #848 — plus the two direct first-parent commits (`aa85eb6b` and `4f4675134`) in the same window, for later one-at-a-time disposition.

**Proposed reviewed PRs (12):** #859, #858, #857, #856, #855, #854, #853, #852, #851, #850, #849, #848.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_859-848.md=859,858,857,856,855,854,853,852,851,850,849,848`

### docs/project_review_873-860.md

**Helper candidates:** #860, #861, #862, #863, #866, #867, #868, #869, #870, #871, #872, #873.

**Helper reason:** docs/project_review_873-860.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #873, #872, #871, #870, #869, #867, #868, #866, #863, #862, #861, and #860 — plus the three direct first-parent commits \x00 in the same window, for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #860, #861, #862, #863, #866, #867, #868, #869, #870, #871, #872, #873; confirm the reviewed ones with --confirm 'docs/project_review_873-860.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #873, #872, #871, #870, #869, #867, #868, #866, #863, #862, #861, and #860 — plus the three direct first-parent commits (`16038c9d`, `034fd733`, and `7fdb6a84`) in the same window, for later one-at-a-time disposition.

**Proposed reviewed PRs (12):** #873, #872, #871, #870, #869, #868, #867, #866, #863, #862, #861, #860.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_873-860.md=873,872,871,870,869,868,867,866,863,862,861,860`

### docs/project_review_909-874.md

**Helper candidates:** #874, #875, #879, #880, #881, #902, #903, #904, #905, #906, #908, #909.

**Helper reason:** docs/project_review_909-874.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #909, #908, #906, #905, #904, #903, #902, #881, #880, #879, #875, and #874 — plus the two direct first-parent commits \x00 in the same window, for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #874, #875, #879, #880, #881, #902, #903, #904, #905, #906, #908, #909; confirm the reviewed ones with --confirm 'docs/project_review_909-874.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #909, #908, #906, #905, #904, #903, #902, #881, #880, #879, #875, and #874 — plus the two direct first-parent commits (`cb147c8a` and `7400009c`) in the same window, for later one-at-a-time disposition.

**Proposed reviewed PRs (12):** #909, #908, #906, #905, #904, #903, #902, #881, #880, #879, #875, #874.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_909-874.md=909,908,906,905,904,903,902,881,880,879,875,874`

### docs/project_review_938-910.md

**Helper candidates:** #910, #924, #926, #927, #928, #929, #930, #935, #937, #938, #940, #941.

**Helper reason:** docs/project_review_938-910.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #938, #941, #940, #937, #929, #935, #930, #928, #926, #927, #924, and #910 — plus the five direct first-parent documentation commits in the same window, for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #910, #924, #926, #927, #928, #929, #930, #935, #937, #938, #940, #941; confirm the reviewed ones with --confirm 'docs/project_review_938-910.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #938, #941, #940, #937, #929, #935, #930, #928, #926, #927, #924, and #910 — plus the five direct first-parent documentation commits in the same window, for later one-at-a-time disposition.

**Proposed reviewed PRs (12):** #941, #940, #938, #937, #935, #930, #929, #928, #927, #926, #924, #910.

**Basis:** Explicit reviewed enumeration in the opening sentence; later incidental, excluded, issue, or prior-batch references are omitted.

**Exact confirmation:** `docs/project_review_938-910.md=941,940,938,937,935,930,929,928,927,926,924,910`

### docs/project_review_989-939.md

**Helper candidates:** #939, #989.

**Helper reason:** docs/project_review_989-939.md: its opening paragraph carries a sentence this helper does not read: 'These entries record focused evidence from the senior review of the next twelve merged PRs, #989 through #939 in merge order, plus the direct first-parent commits \x00 and \x00 in the same window, for later one-at-a-time disposition.', so no row was written from it. Its candidate pull requests are #939, #989; confirm the reviewed ones with --confirm 'docs/project_review_989-939.md=<list>'.

**Source opening:** These entries record focused evidence from the senior review of the next twelve merged PRs, #989 through #939 in merge order, plus the direct first-parent commits `3eed2906` (portable-loot design) and `64306746` (code-health findings) in the same window, for later one-at-a-time disposition.

**Proposed reviewed PRs (12):** #989, #988, #987, #986, #966, #963, #962, #961, #955, #954, #953, #939.

**Basis:** Proposed expansion of the stated merge-order interval using the complete 1,268-PR GitHub inventory; requires confirmation because the report does not enumerate every member.

**Exact confirmation:** `docs/project_review_989-939.md=989,988,987,986,966,963,962,961,955,954,953,939`

## PRs left without established coverage

These 142 entries are not implicitly marked reviewed. New landings after the inventory are also outside this reconciliation.

| PR | Merged at (UTC) | Title |
| --- | --- | --- |
| #2676 | 2026-09-21T13:57:41Z | Ignore treatment claims held by dead or collapsed medics (#2642) |
| #2675 | 2026-09-21T11:56:00Z | Add validated faction-tag definitions and legacy mappings (#2506) |
| #2674 | 2026-09-21T04:46:52Z | Add immediate structure teardown with transient destruction playback (#2491) |
| #2673 | 2026-09-21T02:11:53Z | Show portable containers in the container window (#2527) |
| #2672 | 2026-09-20T20:29:47Z | Make eighth-z fluid state exact, conserved, and durable (#2520) |
| #2671 | 2026-09-20T20:03:54Z | Add an identity-preserving ground-item move operation (#2486) |
| #2670 | 2026-09-20T15:07:13Z | Credit autonomous canteen drinking from the drain the engine actually applied (#2631) |
| #2669 | 2026-09-20T14:26:10Z | Decide starvation lean-floor death within a Float rounding tolerance |
| #2668 | 2026-09-20T01:33:20Z | Rebuild blood volume once bleeding is fully stabilized |
| #2667 | 2026-09-19T13:25:26Z | Commit a typed dropdown edit on focus loss instead of discarding it |
| #2666 | 2026-09-18T20:08:53Z | Rank only medics that can discover the patient they are ranked for |
| #2665 | 2026-09-18T19:45:05Z | Reject case-insensitive aliases of reserved generated-library file names |
| #2664 | 2026-09-18T17:37:30Z | Carry the resource tick's sub-binary32 remainder instead of dropping it |
| #2663 | 2026-09-18T14:12:58Z | Count only able-bodied workers toward building construction progress and recruitment |
| #2662 | 2026-09-18T00:52:59Z | Fetch antibiotics per medicine and bound the futile-cure loop |
| #2661 | 2026-09-18T00:29:23Z | Connect power networks across the cylindrical seam |
| #2660 | 2026-09-17T21:50:26Z | Give treatment a unique wound identity so same-time wounds are not mutated together |
| #2659 | 2026-09-17T21:22:36Z | Verify retained payload bytes before reusing a generated-library entry as unchanged |
| #2658 | 2026-09-17T19:11:54Z | Report failed saveLoaded teardown hooks in the load's reconciliation outcome |
| #2657 | 2026-09-17T18:50:15Z | Refuse pose transitions out of the terminal Dead pose at command execution |
| #2635 | 2026-09-17T18:02:24Z | Sync the list widget's scrollbar when its items are replaced |
| #2632 | 2026-09-17T17:28:15Z | Dismiss open dropdowns on Escape regardless of how many were ever created |
| #2626 | 2026-09-17T16:31:15Z | [CRS-2] Measure detailed-chunk memory and process high-water |
| #2624 | 2026-09-17T15:56:58Z | feat: spawn pending container shells from location content entries (#2505) |
| #2623 | 2026-09-14T18:03:45Z | feat: realize a loot profile deterministically into a container (#2502) |
| #2622 | 2026-09-14T16:29:25Z | Destroy units and ground items caught at a solidifying cell (#2490) |
| #2621 | 2026-09-13T16:15:52Z | [foraging] Preserve the edible target when harvesting a shared flora tile |
| #2620 | 2026-09-13T04:47:18Z | fix: bound and release the source-drink phase lock (#2545) |
| #2619 | 2026-09-13T02:40:49Z | docs: drop the obsolete arena save-test prohibition (#2569) |
| #2618 | 2026-09-13T01:12:04Z | farming: recheck proximity when resuming harvest-yield collection (#2550) |
| #2617 | 2026-09-12T21:23:02Z | survival: use the frame-based fat floor for organ failure (#2556) |
| #2616 | 2026-09-12T18:16:43Z | craft: plan ingredient sourcing per cycle, not per claim (#2524) |
| #2615 | 2026-09-12T17:56:08Z | survival: score water actions off the emptiest canteen, not the first (#2546) |
| #2614 | 2026-09-12T16:02:32Z | preview: resynchronize the audio pane on every catalog reload (#2611) |
| #2613 | 2026-09-12T15:29:23Z | mine: select the nearest WORKABLE designation (#2538) |
| #2612 | 2026-09-12T15:07:27Z | Add approved charred saguaro art |
| #2610 | 2026-09-12T13:25:12Z | chop: select the nearest CLAIMABLE designated tree (#2536) |
| #2609 | 2026-09-12T05:14:12Z | survival: restrict hydration recovery to actual source drinking (#2541) |
| #2608 | 2026-09-12T04:14:33Z | farm: select the nearest CLAIMABLE designation (#2534) |
| #2607 | 2026-09-12T00:42:25Z | Add approved saguaro juvenile living/dead pair |
| #2606 | 2026-09-11T23:49:16Z | Add engine audio and an interactive preview player |
| #2605 | 2026-09-11T23:14:35Z | Solidify the lava-water reaction product into durable stone through the world edit log |
| #2604 | 2026-09-11T03:31:48Z | Run the persistence-contract probe through the prebuilt decoder (#2274) |
| #2602 | 2026-09-11T01:26:59Z | Render flat fluid tops and give one-z drops a side face (#2517) |
| #2603 | 2026-09-11T01:03:17Z | Publish the canonical flora visual-state and fallback contract (#2530) |
| #2601 | 2026-09-11T00:06:37Z | Release ground-repair jobs when the worker no longer owns the target (#2531) |
| #2600 | 2026-09-10T23:31:42Z | Preserve combat stamina costs during physiology updates and evaluate exhaustion from committed values |
| #2599 | 2026-09-10T23:05:49Z | Retire page-owned units and buildings on single-page destroy and same-id re-init (#2476) |
| #2598 | 2026-09-10T16:46:39Z | Generate and approve eighth-level fluid masks (#2525) |
| #2597 | 2026-09-10T15:56:28Z | Honor station queue order when workers choose a bill (#2523) |
| #2595 | 2026-09-10T15:30:30Z | Measure world-map page codecs and bounded disk-cache tradeoffs |
| #2594 | 2026-09-10T14:35:05Z | Carry residual elapsed time across path waypoints (#2473) |
| #2593 | 2026-09-10T13:02:40Z | Fence in-flight fluid writebacks with the page's incarnation epoch |
| #2592 | 2026-09-09T22:29:01Z | Decide review-gate staleness by replaying the approved head (#2591) |
| #2589 | 2026-09-09T18:47:50Z | Measure Lua-to-Haskell calls with runtime-local telemetry |
| #2590 | 2026-09-09T18:21:33Z | Make the building preview inspect every direction and lifecycle role (#2492) |
| #2588 | 2026-09-09T14:55:29Z | Add the pure faction identity and relation policy model (#2500) |
| #2587 | 2026-09-09T14:29:01Z | Add the approved lantern item and sprite |
| #2586 | 2026-09-09T11:57:00Z | Render structure construction from authored progress frames (#2488) |
| #2585 | 2026-09-09T04:33:51Z | Persist player knowledge of portable containers (#2512) |
| #2584 | 2026-09-09T00:31:26Z | Load loot-profile definitions (#2499) |
| #2583 | 2026-09-08T20:54:03Z | Enforce capacity-safe, acyclic nested ownership moves |
| #2582 | 2026-09-08T19:43:13Z | Generate deterministic spatial map-pyramid pages from world-generation parameters |
| #2581 | 2026-09-08T17:30:45Z | Pilot a declarative registration contract on the UI namespace |
| #2579 | 2026-09-08T14:43:30Z | Resolve unlike-fluid contact by annihilation in every active-sim transfer path |
| #2578 | 2026-09-08T14:01:14Z | Decode save fixtures through a prebuilt executable instead of a cabal repl of the test suite |
| #2580 | 2026-09-08T13:23:44Z | Retain sub-minute calendar progress across world ticks so the clock advances at default speed |
| #2577 | 2026-09-08T06:19:32Z | Apply stance recovery atomically against the current stored value |
| #2576 | 2026-09-08T06:01:51Z | Fix instant-built seeding comment: SeedWhenBuilt, not SeedAtSpawn |
| #2575 | 2026-09-08T05:46:24Z | Fix circadian.getCircadianUrge doc comment call shape |
| #2574 | 2026-09-08T05:30:36Z | Preserve approved synthesized menu sound references |
| #2572 | 2026-09-08T05:22:32Z | Migrate ten manual probes to the flake protocol |
| #2573 | 2026-09-08T05:06:45Z | Reduce normal startup delay with budgeted queue draining |
| #2571 | 2026-09-08T04:47:21Z | docs: fix component-owner layering statement in Save.Component.Types |
| #2570 | 2026-09-08T04:30:34Z | Fix immediate-pause observation in orphan-prune probe |
| #2568 | 2026-09-08T04:14:34Z | docs: rewrite the Tier 3 damage derivation to the rotational swing and six-factor delivery |
| #2567 | 2026-09-08T03:59:27Z | docs: fix loadVegetationYamlFn call-site comment cardinality claim |
| #2566 | 2026-09-08T03:43:07Z | ci: retire the deleted fluid facade's three surviving references |
| #2565 | 2026-09-08T03:25:31Z | Relabel computeAmbientLight's curve to its own input convention |
| #2118 | 2026-09-01T20:04:21Z | Split the atomic probe-claim gate along lease, census, and orchestration owners (#2100) |
| #2117 | 2026-09-01T19:38:24Z | Split the playtest runner's self-test along its module owners (#2040) |
| #2109 | 2026-09-01T18:21:36Z | Make structure drag planning authoritative and self-clearing (#1844) |
| #2115 | 2026-09-01T11:15:23Z | Extract the unified-transfer probe's stage owners behind its single-session façade |
| #2114 | 2026-09-01T10:45:22Z | Make woundEffSeverity the only spelling of effective severity in the wound tick |
| #2113 | 2026-09-01T10:22:24Z | Route gameplay unit atlases through the player-selected sampler |
| #2112 | 2026-09-01T09:59:01Z | Split the generated-language headless spec along generator-version contracts |
| #2111 | 2026-09-01T09:32:00Z | Split the aggregate probe runner along registry, diagnostics, lifecycle, and scheduling owners |
| #2110 | 2026-09-01T09:07:51Z | Pin the encounter roll's id dependence, chunk independence and mapping |
| #2108 | 2026-09-01T08:43:09Z | Give every flora instance stable identity and exact mutable state (#1854) |
| #2107 | 2026-09-01T08:13:13Z | Extrude one texel around every atlas cell so linear sampling cannot bleed across frames |
| #2106 | 2026-09-01T07:47:55Z | Carry signed 32-bit cylinder coordinates in every world vertex |
| #2105 | 2026-09-01T07:22:20Z | Delete the twelve subsumed repeated-projection examples and correct the seven inventory claims they back |
| #2104 | 2026-09-01T06:58:04Z | Pin UI textures to nearest instead of following the player's filter setting |
| #2103 | 2026-09-01T06:33:45Z | Keep input_check.py's diagnostic sequence alive after a missed fixture click |
| #2102 | 2026-09-01T06:11:29Z | Establish transient unit-AI runtime defaults before a migrated row goes live |
| #2101 | 2026-09-01T05:47:55Z | Require a crossed presentation boundary before acknowledging sticky tutorial rows |
| #2096 | 2026-09-01T05:23:29Z | Migrate blood impact probe to probe-result/v1 |
| #2099 | 2026-09-01T04:59:42Z | Make the capability-writer audit fail closed on projection bindings it cannot read |
| #2086 | 2026-09-01T04:36:07Z | Gate the durable location-stamp marker on world-thread commit, not on queuing |
| #2082 | 2026-09-01T04:09:07Z | Exchange and activate runtime fluid across the cylindrical U seam |
| #2084 | 2026-09-01T03:41:07Z | Refuse an unsafe map image plan before allocating or uploading it (#2020) |
| #2083 | 2026-09-01T03:17:45Z | Gate Mode B transfer orders on the carrier's registered AI actions |
| #2022 | 2026-09-01T02:15:06Z | Centralize chunk demand behind one canonical chunk key and request owner |
| #2081 | 2026-09-01T01:48:14Z | Cap lateral fluid equalization at the source's remaining volume |
| #2079 | 2026-09-01T01:21:38Z | Aggregate active startup YAML logging once per registry family |
| #2077 | 2026-09-01T01:03:01Z | Add keyboard navigation to preview browser |
| #2066 | 2026-09-01T00:43:38Z | playtest: camera-relative zoom semantics and a bounded wheel delta for the scroll action |
| #2045 | 2026-09-01T00:27:39Z | Declare repair recipes instantaneous instead of advertising work the repair path never spends |
| #2039 | 2026-09-01T00:11:23Z | Gate the coastal-parallel threshold on the run length it names, in both river gates |
| #2038 | 2026-08-31T23:47:22Z | Bound breakthrough search scratch to its radius |
| #2032 | 2026-08-31T23:20:54Z | Migrate five targeted probes to probe-result/v1 |
| #2029 | 2026-08-31T22:58:45Z | Bound the debug console's retained scrollback and its layout measurement |
| #2028 | 2026-08-31T22:35:31Z | Make red raspberry fruiting art visibly ripe |
| #2025 | 2026-08-31T22:12:58Z | Skip location sight rasterization during clearance-only ticks |
| #2023 | 2026-08-31T21:39:47Z | Add Machine Shop construction progress art |
| #2018 | 2026-08-31T21:14:45Z | Instrument World.Render scene assembly telemetry (#1921) |
| #2016 | 2026-08-31T20:33:13Z | Render crop Plant as a light-green flat tilled surface |
| #2015 | 2026-08-31T20:10:47Z | Keep a failing location/portal probe's failed check in the retained output |
| #2014 | 2026-08-31T19:47:41Z | Add common cattail wetland flora |
| #2013 | 2026-08-31T19:28:42Z | Measure fjord and glacial coast forms |
| #2012 | 2026-08-31T19:11:53Z | Gate every Lua call site against the engine's real registration set (#1996) |
| #2011 | 2026-08-31T18:42:22Z | Migrate lua_strict_msg probe to probe-result/v1 |
| #2010 | 2026-08-31T18:25:47Z | Prepare a directly-invoked probe's engine outside the READY deadline (#1913) |
| #2009 | 2026-08-31T18:02:22Z | Couple scene-text cache entries to their scene nodes' lifetimes |
| #1248 | 2026-08-12T13:25:43Z | Generalize the container window to any endpoint kind |
| #1245 | 2026-08-12T05:09:44Z | Make the transactional-load probe deterministically test mutual exclusion |
| #1244 | 2026-08-12T04:27:59Z | Cut the two live UI truncators at code-point boundaries |
| #1243 | 2026-08-12T02:59:49Z | Repair the Lua save API smoke test against the current asynchronous contracts |
| #1241 | 2026-08-12T02:37:22Z | Re-derive worn-accessory buffs when an accessory is unequipped |
| #1242 | 2026-08-12T01:57:20Z | Retire a power building's node when the building is destroyed |
| #1240 | 2026-08-12T01:35:21Z | Judge spawn-time loadout shedding against effective carrying capacity |
| #1236 | 2026-08-11T22:03:00Z | Resolve ground pickup and unit drops on the unit's owning page |
| #1235 | 2026-08-11T21:31:36Z | Require power.placeNode's supplying unit to belong to the destination page |
| #1228 | 2026-08-11T17:26:58Z | Replace the transposable positional runs in the two 14-parameter quad producers |
| #1227 | 2026-08-11T16:31:50Z | Keep persistent wire networks electrically connected across chunk eviction |
| #1226 | 2026-08-11T14:45:23Z | Report a distinct terminal disposition when post-load reconciliation callbacks fail |
| #1225 | 2026-08-11T03:06:54Z | State who owns the findings report's status fields, and audit that they agree |
| #1224 | 2026-08-11T02:45:22Z | Narrow the src/UI export lists, delete the dead submitBuffer, and drop the UI.Focus TextBuffer re-export |
| #1223 | 2026-08-11T02:24:37Z | Drain and refill the exact canteen instance the AI selected |
| #1215 | 2026-08-10T20:35:20Z | Surface Lua load rollback failures instead of reporting a cleanly aborted load |
| #1214 | 2026-08-10T19:56:59Z | Fix the invalid regex escape sequence in action_outcome_coverage.py's docstring |
| #1211 | 2026-08-10T18:42:37Z | Keep a selectable generation on disk when saving over a corrupt-authoritative recovery slot |

## Completed migration and remaining direct-history recovery

Source hashes were unchanged. Each `proposed_confirmations` entry was passed verbatim as a separate `--confirm path=numbers` argument to the installed ledger helper’s `migrate` command. All 1,126 expected PR rows and every report link passed verification. Imported rows are labeled `legacy`, which preserves historical coverage without inventing a modern verification SHA or clean/findings verdict. Under the installed scheduling rules, never-reviewed PRs are selected before legacy re-reviews. No push or docs landing is authorized by this recovery step.
