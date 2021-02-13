#!/usr/bin/env python3

import pitches as p
import tabs as t

# Let's find the most even scale among these

def computeDifferenceVectors(scale):
    diffs = []
    for s in scale:
        diff = tuple(sorted([p.normalizePitch(x-s) for x in scale]))
        diffs.extend(diff)
    return diffs

def countRepeats(list):
    r = {}
    for l in list:
        if l in r:
            r[l] += 1
        else:
            r[l] = 1
    return r

for sid in range(9):
    for mid in range(3):
        s = p.getScaleName(sid)
        scale = p.getScalePitches(s,mid,t.tunings[8][0])[:-1] #remove the 15ma
        diffs = computeDifferenceVectors(scale)
        reps = countRepeats(diffs)
        repcounts = reps.values()
        repc = countRepeats(repcounts)
