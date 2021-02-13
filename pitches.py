#!/usr/bin/env python3
"""
  This module contains helper funtions to convert between classical and
  quintadecimal pitches.
"""

def normalizePitch(pitch,modulo=24):
    return pitch % modulo # unlike C, this has the same sign as the denumerator modulo

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
    
def getPitch(name):
    x = name.strip()
    if len(x) == 1:
        pitchnames = {'J':  0, 'K':  1, 'L':  3, 'M':  4, 'N':  6, 
                      'O':  8, 'P':  9, 'Q': 11, 'R': 13, 'S': 15, 
                      'T': 16, 'U': 18, 'V': 20, 'X': 21, 'Y': 23,
                      'C':  0, 'D':  2, 'E':  4, 'F':  5, 'G':  7,
                      'A':  9, 'B': 11, 'H': 11, #germans
                      'Z':  0 #mispronounced C or J
                      }
        return pitchnames[x.upper()]
    offset = 0
    for m in x[1:]:
        if m == "#":
            offset += 1
        elif m == "b" or m == "B":
            offset -= 1
        elif m == "'":
            offset += 12
        elif m == ",":
            offset -= 12
        elif m == '"':
            offset += 24
        elif m == ";":
            offset -= 24
    return (getPitch(x[0])+offset)
    
def getClassicalPitchNameCandidates(pitch):
    x = normalizePitch(pitch,12)
    candidate_list = [
        ["C","B#"],
        ["C#","Db"],
        ["D"],
        ["D#","Eb"],
        ["E","Fb"],
        ["F","E#"],
        ["F#","Gb"],
        ["G"],
        ["G#","Ab"],
        ["A"],
        ["A#","Bb"],
        ["B","Cb"]
    ]
    return candidate_list[x]
    
def scorePartialChoice(choice):
    nbr_sharps = len([1 for x in choice if x.endswith("#")])
    nbr_flats = len([1 for x in choice if x.endswith("b")])
    unique_pitches = len(frozenset((x[0] for x in choice)))
    return 49*nbr_sharps + 50*nbr_flats + 5000*(len(choice)-unique_pitches)
    
def getPitchNames(pitches, candidates=getPitchNameCandidates):
    """
      takes a list of pitches and returns a list of pitch class names
    """
    p0 = sorted(frozenset(map(normalizePitch, pitches)))
    choices = [candidates(x) for x in p0]
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

def getClassicalPitchNames(pitches):
    """
      takes a list of pitches and returns a list of classical pitch class names
    """
    return getPitchNames([normalizePitch(x,12) for x in pitches],getClassicalPitchNameCandidates)
    
def getPitchNames2(pitches, candidates=getPitchNameCandidates):
    """
      takes a list of pitches and returns a list of pitch class names with indication
      of octave relative to C0 =%= J0
    """
    octave_indicator = {0:"",1:"'",2:'"',3:"\"'",4:'""',5:"\"\"'",6:"\"\"\"",
                         -1:",",-2:';',-3:";,",-4:';;',-5:";;,",-6:";;;"}
    names = getPitchNames(pitches,candidates)
    diffs = [(p-np)//12 for p,np in zip(pitches,map(getPitch,names))]
    return [p+octave_indicator[x] for p,x in zip(names,diffs)]

def getClassicalPitchNames2(pitches):
    """
      takes a list of pitches and returns a list of pitch class names with indication
      of octave relative to C0 =%= J0
    """
    octave_indicator = {0:"",1:"'",2:"''",3:"'''",4:"''''",5:"'''''",6:"''''''",
                         -1:",",-2:',,',-3:",,,",-4:',,,,',-5:",,,,,",-6:",,,,,,"}
    names = getClassicalPitchNames(pitches)
    diffs = [(p-np)//12 for p,np in zip(pitches,map(getPitch,names))]
    return [p+octave_indicator[x] for p,x in zip(names,diffs)]

def getScaleName(nbr):
    roman = {"I":0,"II":1,"III":2,"IV":3,"V":4,"VI":5,"VII":6,"VIII":7,"IX":8}
    romanI = dict(((y,x) for x,y in roman.items()))
    return romanI[int(nbr)]

def getScalePitches(scaletype, mode, root):
    roman_names = {"I":0,"II":1,"III":2,"IV":3,"V":4,"VI":5,"VII":6,"VIII":7,"IX":8}
    if type(scaletype) == str:
        t = roman_names[scaletype]
    else:
        t = int(scaletype)
    library = [[[0, 2, 3, 5, 7, 8, 9, 11, 13, 14, 15, 17, 19, 21, 23, 24], 
                [0, 1, 3, 5, 6, 7, 9, 11, 13, 15, 16, 18, 19, 21, 23, 24], 
                [0, 1, 2, 4, 6, 8, 10, 11, 13, 14, 16, 18, 19, 20, 22, 24]], 
               [[0, 1, 3, 5, 7, 9, 10, 11, 13, 15, 16, 17, 19, 21, 22, 24], 
                [0, 2, 4, 5, 6, 8, 10, 11, 13, 14, 16, 18, 20, 22, 23, 24], 
                [0, 1, 3, 5, 6, 8, 9, 11, 13, 15, 17, 18, 19, 21, 23, 24]], 
               [[0, 1, 3, 4, 5, 7, 9, 11, 13, 14, 16, 17, 19, 21, 23, 24], 
                [0, 1, 2, 4, 6, 8, 10, 11, 13, 14, 16, 18, 20, 21, 22, 24], 
                [0, 2, 3, 5, 7, 9, 10, 11, 13, 14, 15, 17, 19, 21, 23, 24]], 
               [[0, 2, 3, 5, 6, 7, 9, 11, 13, 14, 15, 17, 19, 21, 23, 24], 
                [0, 1, 3, 5, 7, 8, 9, 11, 13, 15, 17, 18, 20, 21, 23, 24], 
                [0, 1, 2, 4, 6, 8, 10, 11, 13, 14, 16, 17, 18, 20, 22, 24]], 
               [[0, 1, 3, 5, 7, 8, 10, 11, 13, 15, 17, 19, 20, 21, 23, 24], 
                [0, 1, 3, 5, 7, 9, 10, 11, 13, 14, 15, 17, 19, 21, 22, 24], 
                [0, 2, 3, 4, 6, 8, 10, 11, 13, 14, 16, 18, 20, 22, 23, 24]], 
               [[0, 1, 3, 5, 7, 9, 10, 11, 13, 15, 17, 18, 19, 21, 22, 24], 
                [0, 2, 4, 6, 7, 8, 10, 11, 13, 14, 16, 18, 20, 22, 23, 24], 
                [0, 1, 3, 4, 6, 7, 9, 11, 13, 15, 16, 17, 19, 21, 23, 24]], 
               [[0, 1, 3, 5, 6, 7, 9, 11, 13, 14, 15, 17, 19, 21, 23, 24], 
                [0, 1, 3, 5, 7, 8, 9, 11, 13, 15, 17, 18, 19, 21, 23, 24], 
                [0, 1, 3, 5, 7, 9, 10, 11, 13, 15, 16, 17, 19, 21, 23, 24]], 
                [[0, 1, 3, 5, 7, 8, 9, 11, 13, 14, 15, 17, 19, 21, 23, 24], 
                [0, 1, 3, 5, 6, 7, 9, 11, 13, 15, 16, 17, 19, 21, 23, 24], 
                [0, 1, 3, 5, 7, 9, 10, 11, 13, 15, 17, 18, 19, 21, 23, 24]], 
               [[0, 1, 3, 4, 5, 7, 9, 11, 13, 14, 15, 17, 19, 21, 23, 24], 
                [0, 1, 3, 5, 7, 9, 10, 11, 13, 15, 17, 19, 20, 21, 23, 24], 
                [0, 1, 3, 5, 7, 9, 10, 11, 13, 14, 15, 17, 19, 21, 23, 24]]]
    if type(root) == str:
        r = getPitch(root)
    else:
        r = root
    return [x+r for x in library[t][int(mode)]]