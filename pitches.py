#!/usr/bin/env python3
"""
  This module contains helper funtions to convert between classical and
  quintadecimal pitches.
"""

def normalizePitch(pitch):
    x = pitch % 24
    if (x < 0):
        x += 24
    return x

# Pitch   0  =%=   C  =  J, Kb, Y#
# Pitch   1  =%=  C#  =  K, J#
# Pitch   2  =%=   D  =  Lb, K#
# Pitch   3  =%=  D#  =  L, Mb
# Pitch   4  =%=   E  =  M, L#
# Pitch   5  =%=   F  =  Nb, M#
# Pitch   6  =%=  F#  =  N
# Pitch   7  =%=   G  =  Ob, N#
# Pitch   8  =%=  G#  =  O, Pb
# Pitch   9  =%=   A  =  P, O#
# Pitch  10  =%=  A#  =  Qb, P#
# Pitch  11  =%=   B  =  Q
# Pitch  12  =%=   C  =  Rb, Q#
# Pitch  13  =%=  C#  =  R
# Pitch  14  =%=   D  =  Sb, R#
# Pitch  15  =%=  D#  =  S, Tb
# Pitch  16  =%=   E  =  T, S#
# Pitch  17  =%=   F  =  Ub, T#
# Pitch  18  =%=  F#  =  U
# Pitch  19  =%=   G  =  Vb, U#
# Pitch  20  =%=  G#  =  V, Xb
# Pitch  21  =%=   A  =  X, V#
# Pitch  22  =%=  A#  =  Yb, X#
# Pitch  23  =%=   B  =  Y, Jb

def getPitchNameCandidates(pitch):
    x = normalizePitch(pitch)
    candidate_list = [
        ["J", "Kb", "Y#"], #C
        ["K", "J#"],       
        ["Lb", "K#"],
        ["L", "Mb"],
        ["M", "L#"],
        ["Nb", "M#"],
        ["N"],
        ["Ob", "N#"],
        ["O", "Pb"],
        ["P", "O#"],
        ["Qb", "P#"],
        ["Q"],
        ["Rb", "Q#"], #C
        ["R"],
        ["Sb", "R#"],
        ["S", "Tb"],
        ["T", "S#"],
        ["Ub", "T#"],
        ["U"],
        ["Vb", "U#"],
        ["V", "Xb"],
        ["X", "V#"],
        ["Yb", "X#"],
        ["Y", "Jb"]
    ]
    return candidate_list[x]
    
def scorePartialChoice(choice):
    nbr_sharps = len([1 for x in choice if x.endswith("#")])
    nbr_flats = len([1 for x in choice if x.endswith("b")])
    unique_pitches = len(frozenset((x[0] for x in choice)))
    return 49*nbr_sharps + 50*nbr_flats + 5000*(len(choice)-unique_pitches)
    
def getPitchNames(pitches):
    p0 = sorted(frozenset(map(normalizePitch, pitches)))
    choices = [getPitchNameCandidates(x) for x in p0]
    # get trivial upper bound
    defaultChoice = tuple((x[0] for x in choices))
    defaultScore = scorePartialChoice(defaultChoice)
    # run depth first search with bound
    def dfs(partialChoice, upperBound, boundChoice):
        if (len(partialChoice) == len(choices)):
            return scorePartialChoice(partialChoice),partialChoice
        for candidate in choices[len(partialChoice)]:
            nextChoice = partialChoice + (candidate,)
            nextScore = scorePartialChoice(nextChoice)
            if nextScore < upperBound:
                upperBound, boundChoice = dfs(nextChoice,upperBound,boundChoice)
        return upperBound, boundChoice
            
    _,finalChoice = dfs(tuple(),defaultScore,defaultChoice)
    return [finalChoice[p0.index(normalizePitch(x))] for x in pitches]
