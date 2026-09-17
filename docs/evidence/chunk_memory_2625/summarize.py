#!/usr/bin/env python3
"""Recompute the retained matrix tables; no engine launch or mutation."""
import csv
import hashlib
import json
from pathlib import Path
import statistics
import sys

root = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).resolve().parent
cells = [f'w{s}-{r}-{n}' for s in (64, 256)
         for r in ('production', 'small-nursery') for n in (1, 2, 3)]
cells += ['w64-production-1-offscreen', 'w64-small-nursery-1-offscreen']
records = []
for cell in cells:
    path = root / cell / 'measurement.json'
    d = json.loads(path.read_text())
    assert d['status'] == 'complete' and d['exitCode'] == 0, cell
    assert d['seed'] == 42 and d['plates'] == 3 and d['verifiedDistinctChunks'] >= 1000, cell
    assert not d['forcedGC'] and not d['samplerErrors'], cell
    expected = [] if '-production-' in cell else ['+RTS', '-A8M', '-RTS']
    assert d['rtsOverride'] == expected, cell
    if d['mode'] == 'offscreen':
        assert len(d['colony']['unitIds']) == 5 and d['colony']['requestedBaseChunks'] == 100, cell
    phases = ['generation', 'traversal', 'loading'] + (['gameplay'] if d['mode'] == 'offscreen' else [])
    for phase in phases:
        samples = [s['rssBytes'] for s in d['samples'] if s['phase'] == phase]
        assert samples and max(samples) == d['phasePeaksBytes'][phase] and max(samples) > 0, (cell, phase)
    for obs in d['observations']:
        v = obs['value']
        assert v['capabilities'] == 16 and v['wordBytes'] == 8 and v['model'] == 'logical-unshared-v1', cell
        assert sum(p['resident'] for p in v['pages']) == v['resident'], cell
        assert sum(p['logicalEstimatedBytes']['total'] for p in v['pages']) == v['logicalEstimatedBytes'], cell
        for page in v['pages']:
            b = page['logicalEstimatedBytes']
            assert sum(b[k] for k in ('columns','derivedMaps','overlays','containers')) == b['total'], cell
    records.append((cell, d, hashlib.sha256(path.read_bytes()).hexdigest()))
assert {d['revision'] for _, d, _ in records} == {'7650862c6b9f48102e5ae17e036615e678cec9f3', 'b279123fb684795d1e8d6a48a5ea439e2fd6a057'}
assert len({d['binarySha256'] for _, d, _ in records}) == 1
print('Validated',len(records),'complete fresh processes; no excluded complete cells.')
print('Revisions:', sorted({d['revision'] for _, d, _ in records}))
print('Binary SHA256:',records[0][1]['binarySha256'])
print('\nPer-process sampled phase peaks, GiB (not exact transition maxima):')
writer = csv.writer(sys.stdout)
writer.writerow(['cell','generation','traversal','gameplay','save','loading','verifiedChunks','worldPeakMiB','simPeakMiB','residentPeak'])
for cell,d,_ in records:
    obs = [o['value'] for o in d['observations']]
    writer.writerow([cell] + [f"{d['phasePeaksBytes'][p]/2**30:.4f}" if p in d['phasePeaksBytes'] else '' for p in ('generation','traversal','gameplay','save','loading')]
        + [d['verifiedDistinctChunks'],f"{max(o['logicalEstimatedBytes'] for o in obs)/2**20:.4f}",
           f"{max(o['simulation']['logicalEstimatedBytes'] for o in obs)/2**20:.4f}",max(o['peakResident'] for o in obs)])
print('\nPeak median [min,max] GiB for headless repetitions:')
for size in (64,256):
    for rts in ('production','small-nursery'):
        ds=[d for cell,d,_ in records if cell.startswith(f'w{size}-{rts}-') and d['mode']=='headless']
        print(size,rts, '; '.join(f"{p} {statistics.median(a):.4f} [{min(a):.4f},{max(a):.4f}]" for p in ('generation','traversal','loading')
              for a in [[d['phasePeaksBytes'][p]/2**30 for d in ds]]))
print('\nConsole memory-query round-trip milliseconds (includes transport idle boundary):')
for cell,d,_ in records:
    q=[c['elapsedSeconds']*1000 for c in d['commands'] if c['lua']=='return world.getChunkMemory()']
    gaps=[b['timeSeconds']-a['timeSeconds'] for a,b in zip(d['samples'],d['samples'][1:])]
    print(cell,'n',len(q),'median',round(statistics.median(q),3),'max',round(max(q),3),'RSS n',len(d['samples']),'max gap seconds',round(max(gaps),3))
print('\nFinal traversal field groups and individual chunk estimate range, bytes:')
for cell,d,_ in records:
    v=next(o['value'] for o in d['observations'] if o['label']=='traversal-complete')
    print(cell, json.dumps(v['pages'],sort_keys=True))
print('\nSource measurement.json SHA256:')
for cell,_,digest in records:
    print(digest,cell+'/measurement.json')

