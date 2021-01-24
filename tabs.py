#!/usr/bin/env python3

import pitches as p

unplayedString = "" # we use this to indicate that there is a string which is left alone :)

tunings = {4:[-8   + i*5 for i in range(4)], #4-string bass
           5:[-13  + i*5 for i in range(5)], #5-string bass
           6:[-5   + i*5 for i in range(6)], # my 6-string tuning
           7:[-5   + i*5 for i in range(7)], # my 7-string tuning
           8:[-10  + i*5 for i in range(8)], # my 8-string tuning
           'git':[4 + i for i in [0,5,10,15,19,24]] #standard guitar tuning
          }
          
def transposeAndMap(listoflists, fn=lambda x: x,nothing=""):
    """
      takes a list of lists and returns its transpose
      after doing a point-wise mapping:
      
      listoflists = [[1,2],[3,4]] 
      fn = str       yields
      [["1","3"],["2","4"]]
    """
    innerdim = max((map(len,listoflists)))
    output = [[] for i in range(innerdim)]
    for x in listoflists:
        for i,c in enumerate(x):
            output[i].append(fn(c))
        for i in range(len(x),innerdim):
            output[i].append(nothing)
    return output

def formatColumn(content,width=3,c="-"):
    content = str(content)
    l = len(content)
    if l > width:
        return content[:l]
    return content + c*(width-l)
    
def fillLeft(s,width):
    s = str(s)
    if len(s) < width:
        return " "*(width-len(s)) + s
    return s

def printTab(columnwise,textleft=[],formatter=formatColumn):
    """
       prints a tab gives as list of columns; where the first column
       corresponds to the lowest row in the printout
       (because this is the way you read tabs, bottom up; 
        root note first, then other chord notes).
       
       >>> printTab([[6,8],[3],["",8]])
       8-----8--
       6--3-----
    """
    rowwise = list(reversed(transposeAndMap(columnwise,formatter,formatter(""))))
    textleft = list(textleft)
    if len(textleft) < len(rowwise):
        textleft += [""] * (len(rowwise)-len(textleft))
    elif len(textleft) > len(rowwise):
        rowwise += [[""]] * (len(textleft)-len(rowwise))
    maxwidth = max((len(x) for x in textleft))
    for t,r in zip(textleft,rowwise):
        print(fillLeft(t,maxwidth),"".join(r),sep="")
        
def fillColumnsFromBottom(columnwise, filler=""):
    maxheight = max((len(x) for x in columnwise))
    return [[filler]*(maxheight-len(x)) + x for  x in columnwise]

def reverseColumns(columnwise):
    return [list(reversed(x)) for x in columnwise]

def getPitchFretCandidates(pitch, tuning=tunings[8], hifret=24):
    candidates = []
    lt = len(tuning)
    for nbr, basepitch in enumerate(tuning):
        if basepitch <= pitch <= basepitch + hifret:
            candidates.append([unplayedString]*(nbr) + [pitch-basepitch]+ [unplayedString]*(lt-nbr-1))
    return candidates
    
def canCombineFrettings(listoffrettings):
    used_frets = [frozenset((i for i in range(len(x)) if x[i] != unplayedString)) for x in listoffrettings]
    sum_used_frets = sum((len(x) for x in used_frets))
    return len(frozenset().union(*used_frets)) == sum_used_frets
   
def combineFrettings(frettings):
    current = []
    for f in frettings:
        if len(current) < len(f):
            current += [unplayedString] * (len(f)-len(current))
        for i,x in enumerate(f):
            if x == unplayedString:
                continue
            current[i] = x
    return current
   
def getChordFretCandidates(pitches, tuning=tunings[8],hifret=24):
    candidates = [[]]
    c0 = [getPitchFretCandidates(x) for x in pitches]
    for c in c0:
        candidates = [combineFrettings([x,y]) for x in candidates for y in c if canCombineFrettings([x,y])]
    return candidates

def getFretSpan(fretting):
    nbrs = [x for x in fretting if x != unplayedString and x != 0]
    if len(nbrs) == 0:
        return None
    return min(nbrs),max(nbrs)
    
def getStringSpan(fretting):
    nbrs = [i for i,x in enumerate(fretting) if x != unplayedString and x != 0]
    if len(nbrs) == 0:
        return None
    return min(nbrs),max(nbrs)

def getOpenStrings(fretting):
    nbrs = [i for i,x in enumerate(fretting) if x == 0]
    return nbrs

def getPlayedStrings(fretting):
    nbrs = [i for i,x in enumerate(fretting) if x != unplayedString]
    return nbrs

openStringPenalty = 0
neckMovePenalty = 100
stringChangePenalty = 75
openMutePenalty = 250
mutedSkippedStringPenalty = 30
multipleSkippedStringsPenalty = 10 #for each additionally skipped string
spanWidthPenalty = 10
reverseFrettingPenalty = 40
spanBiggerThan4Penalty = 120
spanBiggerThan5Penalty = 1000
fretHigherThan16Penalty = 50

def scoreSuccessiveFrettings(prevfrets,currfrets):
    penalty = 0
    # penalty for moving around on the neck
    prevSpan = getFretSpan(prevfrets)
    currSpan = getFretSpan(currfrets)
    
    if not ((prevSpan == None) or (currSpan == None)):
        penalty += neckMovePenalty * abs(prevSpan[0]-currSpan[0])
    
    # penalty for changing strings
    prevSpan = getStringSpan(prevfrets)
    currSpan = getStringSpan(currfrets)
    
    if not ((prevSpan == None) or (currSpan == None)):
        penalty += stringChangePenalty * (abs(prevSpan[0]-currSpan[0])+abs(prevSpan[1]-currSpan[1]))
    
    # penalty for having to mute open strings that ring out
    penalty += openMutePenalty * len(frozenset(getOpenStrings(prevfrets)).difference(getPlayedStrings(currfrets)))
    
    played = getPlayedStrings(currfrets)
    
    if len(played) > 0:
        gaps = 0
        multilinegaps = 0
        lastPlayed = False
        for i in range(min(played),max(played)):
            if i in played:
                lastPlayed = True
            else:
                if lastPlayed:
                    gaps += 1
                else:
                    multilinegaps += 1
                lastPlayed = False
        penalty += mutedSkippedStringPenalty*gaps + multipleSkippedStringsPenalty*multilinegaps
    
    fret_inversions = 0
    for f0,f1 in zip(currfrets[:-1],currfrets[1:]):
        if f0 == unplayedString or f1 == unplayedString:
            continue
        if f1 < f0:
            fret_inversions += 1
    penalty += fret_inversions * reverseFrettingPenalty
    
    fretSpan = getFretSpan(currfrets)
    if fretSpan != None:
        fs = fretSpan[1] - fretSpan[0]
        penalty += fs*spanWidthPenalty
        if fs > 4:
            penalty += spanBiggerThan5Penalty
        elif fs > 3:
            penalty += spanBiggerThan4Penalty
    
    penalty += len([1 for x in currfrets if x != unplayedString and x > 16])*fretHigherThan16Penalty
    
    # penalty for the awkwardness of the current fretting
    penalty += openStringPenalty * len([1 for x in currfrets if x == 0])
    return penalty

def scoreFretting(fretting):
    return scoreSuccessiveFrettings([],fretting)