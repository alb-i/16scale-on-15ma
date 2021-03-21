#!/usr/bin/env python3

import pitches as p

from math import ceil
import itertools as it

# from timeit import timeit

unplayedString = "" # we use this to indicate that there is a string which is left alone :)

tunings = {4:tuple([-8   + i*5 for i in range(4)]), #4-string bass
           5:tuple([-13  + i*5 for i in range(5)]), #5-string bass
           6:tuple([-5   + i*5 for i in range(6)]), # my 6-string tuning
           7:tuple([-8   + i*5 for i in range(7)]), # my 7-string tuning
           '8.1':tuple([-10  + i*5 for i in range(8)]), # my old 8-string tuning
           8:tuple([-8-7] + [-8   + i*5 for i in range(7)]), # my new, more evil 8-string tuning
           'git':tuple([4 + i for i in [0,5,10,15,19,24]]) #standard guitar tuning
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
        
def printMultilineTab(colPerLine,columnwise,textleft=[],formatter=formatColumn,sep=""):
    nbr = ceil(len(columnwise) / colPerLine)
    for i in range(nbr):
        if (i > 0):
            print(sep)
        printTab(columnwise[i*colPerLine:(i+1)*colPerLine],textleft,formatter)
        
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

favoriteFretNbr = 5
belowFavoriteFretPenalty = 6
aboveFavoriteFretPenalty = 4
openStringPenalty = 35 # 100 if you despice open string, 50 if you prefer no open strings; 15, 30, 20 also are reasonable numbers, even 0
neckMovePenalty = 20
stringChangePenalty = 75
stringChangeGamma = 1.4
openMutePenalty = 250
mutedSkippedStringPenalty = 100
multipleSkippedStringsPenalty = 10 #for each additionally skipped string
spanWidthPenalty = 10
reverseFrettingPenalty = 40
spanBiggerThan4Penalty = 120
spanBiggerThan5Penalty = 1000

penalizeUpperFretsAfter = 13
fretUpperFretsPenalty = 5


penalizeFretsHigherThan = 16
fretHigherThanXXPenalty = 50


def scoreSuccessiveFrettings(prevfrets,currfrets,cache={}):
    cacheEntry = (tuple(prevfrets),tuple(currfrets))
    # this cache gives ~15x speedup
    # >>> timeit(lambda: scoreSuccessiveFrettings([5,"",5],[6,8],cache={}),number=1000000)
    # 11.593538855000006
    # >>> timeit(lambda: scoreSuccessiveFrettings([5,"",5],[6,8]),number=1000000)
    # 0.7480272119999967
    if cacheEntry in cache:
        return cache[cacheEntry]
    
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
        penalty += stringChangePenalty * (abs(prevSpan[0]-currSpan[0])**stringChangeGamma+abs(prevSpan[1]-currSpan[1])**stringChangeGamma)
    
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
    fretswithoutgaps = [x for x in currfrets if x != unplayedString]
    for f0,f1 in zip(fretswithoutgaps[:-1],fretswithoutgaps[1:]):
        #if f0 == unplayedString or f1 == unplayedString:
        #    continue
        if f1 < f0:
            fret_inversions += 1
    penalty += fret_inversions * reverseFrettingPenalty
    
    below_favorite_fret_sum = 0
    above_favorite_fret_sum = 0
    for f in currfrets:
        if f == unplayedString:
            continue
        if f < favoriteFretNbr and f != 0:
            below_favorite_fret_sum += (favoriteFretNbr - f)
        elif f > favoriteFretNbr:
            above_favorite_fret_sum += (f - favoriteFretNbr)
    
    penalty += below_favorite_fret_sum*belowFavoriteFretPenalty
    penalty += above_favorite_fret_sum*aboveFavoriteFretPenalty
    
    fretSpan = getFretSpan(currfrets)
    if fretSpan != None:
        fs = fretSpan[1] - fretSpan[0]
        penalty += fs*spanWidthPenalty
        if fs > 4:
            penalty += spanBiggerThan5Penalty
        elif fs > 3:
            penalty += spanBiggerThan4Penalty
    
    penalty += len([1 for x in currfrets if x != unplayedString and x > penalizeFretsHigherThan])*fretHigherThanXXPenalty
    penalty += len([1 for x in currfrets if x != unplayedString and x > penalizeUpperFretsAfter])*fretUpperFretsPenalty
    
    # penalty for the awkwardness of the current fretting
    penalty += openStringPenalty * len([1 for x in currfrets if x == 0])
    
    cache[cacheEntry] = penalty
    return penalty

def scoreFretting(fretting):
    return scoreSuccessiveFrettings([],fretting)
    
def isImpossibleFretting(fretting):
    fingered = [x for x in fretting if x != unplayedString and x > 0]
    if not fingered:
        return False
    if max(fingered)-min(fingered) > 5: # +3 is normal, +4 works usually on guitars, +5 sometimes on guitars, +6 frets maybe at 24th fret 
        return True
    return False

def getChordFretCandidates(pitches, tuning=tunings[8],hifret=24,cache={}):
    """
        Takes pitches and generates a list of possible frettings achieving these pitches.
        The resulting list is sorted according to scoreFretting(..).
    """
    cacheEntry = (tuple(pitches),tuple(tuning),hifret) 
    # this cache gives ~100x speedup
    # >>> timeit.timeit(lambda: getChordFretCandidates([6,8]),number=10000)
    # 0.009678113999996185
    # >>> timeit.timeit(lambda: getChordFretCandidates([6,8],cache={}),number=10000)
    # 1.1319672120000064
    if cacheEntry in cache:
        return cache[cacheEntry]
    candidates = [[]]
    c0 = [getPitchFretCandidates(x,tuning=tuning,hifret=hifret) for x in pitches]
    for c in c0:
        candidates = [combineFrettings([x,y]) for x in candidates 
                                              for y in c if canCombineFrettings([x,y])]
        candidates = [x for x in candidates if not isImpossibleFretting(x)]
    # sort the candidates and combine the cache: our cache entries are 3-tuples, the scoreSuccessiveFrettings
    # cache entries are 2-tuples.
    candidates.sort(key=lambda x: scoreSuccessiveFrettings([],x,cache))
    if candidates == []:
        candidates = [[]] #regard unplayable stuff as a fancy rest
    cache[cacheEntry] = candidates
    return candidates

def scoreFrettingSequence(seq, cache={}):
    if len(seq) == 0:
        return 0
    scoresum = scoreSuccessiveFrettings([],seq[0],cache)
    for p,c in zip(seq[:-1],seq[1:]):
        scoresum += scoreSuccessiveFrettings(p,c,cache)
    return scoresum

def generateFrettingSequence(pitches, tuning=tunings[8], hifret=24, windowsize=5, cache={}):
    """
        takes a list of pitch-lists and turns them into a list of frettings
        that optimizes its score. Dictionaries found in the pitch-lists
        are considered to contain on-the-fly options.
        
        {'frets':["",6,8]} - fixes the fretting at the given point
        {'best':k} - only consider the k best fretting options
        {'open':'no'} - only consider fretting options that have no open strings
        {'open':'yes'} - only consider fretting options with at least one open string
        {'above':k} - only consider fretting options that use open frets or frets above (including) k
        {'below':k} - only consider fretting options that use frets below (including) k
        {'+strings':[a,b,c]} - only allow the strings a,b,c to be used
        {'-strings':[a,b,c]} - forbid the strings a,b,c to be used
    """
    # separate pitches from options
    options = [[x for x in p if type(x) == dict] for p in pitches]
    pitches = [[x for x in p if type(x) != dict] for p in pitches]
    candidates = [getChordFretCandidates(p, tuning, hifret,cache) for p in pitches]
    
    def combineOptions(options):
        opts = {}
        for x in options:
            opts = dict(list(opts.items()) + list(x.items()))
        return opts
    
    options = [combineOptions(x) for x in options]
    
    for i,opts in enumerate(options):
        if "frets" in opts:
            candidates[i] = [opts["frets"]]
        else:
            if "best" in opts:
                candidates[i] = candidates[i][:opts["best"]]
            if "open" in opts:
                if opts["open"] in ["no","NO","No","nO",False,0]:
                    candidates[i] = [x for x in candidates[i] if len([1 for y in x if y == 0]) == 0]
                else:
                    candidates[i] = [x for x in candidates[i] if len([1 for y in x if y == 0]) != 0]
            if "above" in opts:
                candidates[i] = [x for x in candidates[i] if [1 for y in x if y != unplayedString and y != 0 and y < opts["above"]] == [] ]
            if "below" in opts:
                candidates[i] = [x for x in candidates[i] if [1 for y in x if y != unplayedString and  y > opts["below"]] == []]
            if "+strings" in opts:
                candidates[i] = [x for x in candidates[i] if frozenset([len(tuning)-y for y in opts["+strings"]]).issuperset(getPlayedStrings(x))]
            if "-strings" in opts:
                candidates[i] = [x for x in candidates[i] if frozenset([len(tuning)-y for y in opts["-strings"]]).isdisjoint(getPlayedStrings(x))]
        if candidates[i] == []:
            candidates[i] = [[]]
    
    def dfs(currentBest, currentBound, otherPrefix,prefixChoice, choiceCandidates):
        if choiceCandidates == []:
            return prefixChoice
        for c in choiceCandidates[0]:
            partialScore = scoreFrettingSequence(otherPrefix + prefixChoice+[c],cache)
            if partialScore < currentBound:
                nextBest = dfs(currentBest,currentBound,otherPrefix, prefixChoice+[c],choiceCandidates[1:])
                if nextBest != currentBest:
                    currentBest = nextBest
                    currentBound = scoreFrettingSequence(otherPrefix + nextBest,cache)
        return currentBest
        
    
    bestcandidate = [c[0] for c in candidates]
    
    for i in range(len(pitches)):
        if i > 0:
            prev = bestcandidate[i-1]
        else:
            prev = []
        N_i = min(i+windowsize,len(bestcandidate))
        currentChunk = bestcandidate[i:N_i] # current best candidate solution
        
        scoreBound = scoreFrettingSequence([prev]+currentChunk,cache) # upper bound for best score
        chunkCandidates = candidates[i:N_i]
        bestChunk = dfs(currentChunk,scoreBound,[prev],[],chunkCandidates)
        bestcandidate[i:N_i] = bestChunk
        
    
    return bestcandidate
    
def addBarLinesAt(columnwise, where=None, what="|--"):
    if where == None:
        where = it.count(0,8)
    output = []
    insert = iter(where)
    next_insert = next(insert)
    columns = max((len(x) for x in columnwise))
    for i,c in enumerate(columnwise):
        if i == next_insert:
            if type(what) == list:
                output.append(what)
            else:
                output.append([what]*columns)
            next_insert = next(insert)
        output.append(c)
    return output
