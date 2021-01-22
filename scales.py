#!/usr/bin/env python3

from datetime import datetime

import pitches

form_feed = False

def new_page():
    if form_feed:
        print("",end="\f")
        return
    print()
    print("-"*67) # courier new 10pt = 75 (78) chars per line; 53 (57) lines per page; so it prints on both A4 and letter
    print()
    
    

print("%66s"%(datetime.now().strftime("%b %d, %Y")))
print()
print("16-Tone Scale Generator and Analyzer Report")
print("===========================================")
print()
print("Generates 16-elementary Scales that span two octaves, with")
print("additional properties. Here, the 16th element of the scale")
print("is the repeated root note transposed up by two octaves.")
print("This corresponds to the eight element of the prototypical")
print("C major scale: C D E F G A B *--> C <--*")
print()

romannbr = {0:"I",1:"II",2:"III",3:"IV",4:"V",5:"VI",6:"VII",7:"VIII",8:"IX",9:"X",
10:"XI"}

note_names = ["c","c#","d","d#","e","f","f#","g","g#","a","a#","b"]
note_names2 = [" c","c#"," d","d#"," e"," f","f#"," g","g#"," a","a#"," b"]

# base note for the chord diagram
base_note = note_names.index("c")

# its 15 steps out of 24 semi tone steps
A = "FFh"
B = "FhF"
C = "hFF"

alphabet = [A,B,C]

D = "hhF"
E = "hFh"
F = "Fhh"

alphabet2 = [D,E,F]

major = "FFhFFFh"

major_rotations = [major[i:] + major[:i] for i in range(len(major))]

candidates0 = [((x,y,a,b),q) for x in alphabet
                          for y in alphabet
                          for a in alphabet
                          for b in alphabet
                          for q in alphabet2]

def combineLetters(x):
    (a,b,c,d),q = x
    return [(a,b,c,d,q),(a,b,c,q,d),(a,b,q,c,d),(a,q,b,c,d),(q,a,b,c,d)]

def flat_map(f, xs): 
    return (y for ys in xs for y in f(ys))

print("We require that each consecutive 3 step intervals have at least one")
print("and at most 2 semi-tone steps; all 15 intervals must add up to 24")
print("semi-tone steps. This should somehow evenly distribute the small")
print("steps.")
print()


candidates = list(flat_map(combineLetters, candidates0))

print("Candidate count:",len(candidates))



def containsHHH(candidate):
    test = "".join(candidate * 2)
    return "hhh" in test

print()
print("Filtering out candidates that have three consecutive semi-tone\nsteps.")
print()
candidates = [x for x in candidates if not containsHHH(x)]

print("Candidate count:",len(candidates))

                        
# every letter must occur either once or twice
def countOccurences(word, alphabet):
    return [len([1 for x in word if x == a]) for a in alphabet]

def correctOccurences(word, alphabet):
    count = countOccurences(word,alphabet)
    return min(count) == 1 and max(count) == 2

if 0:
    print()
    print("Require that every 3-step sequence is used either once or twice.")
    print()
    candidates = [x for x in candidates if correctOccurences(x,alphabet)]
    print("Candidate count:",len(candidates))

def containsMajor(candidate):
    test = "".join(candidate * 2)
    return major in test

if 0: # not needed when filtering all diatonic scales
    print()
    print("Filtering out all candidates that contain a consecutive major scale.")    
    print()
    candidates = [x for x in candidates if not containsMajor(x)]

    print("Candidate count:",len(candidates))

def containsDiatonicScale(candidate):
    test = "".join(candidate * 2)
    return any((m in test for m in major_rotations))

if 0: # not needed as we filter out diatonic sub-scales with additional steps below
    print()
    print("Filtering out all candidates that contain any of the 7 diatonic\nmodes.")
    print()
    candidates = [x for x in candidates if not containsDiatonicScale(x)]

    print("Candidate count:",len(candidates))



def getSteps(candidate):
    scale = "".join(candidate)
    offset = 0
    steps = [0]
    for x in scale:
        if x == "F":
            offset += 2
        else:
            offset += 1
        steps.append(offset)
    return steps
    
print()
print("Filtering out all candidates that also contain the octave of the\nroot.")
print()
candidates = [x for x in candidates if not 12 in getSteps(x)]

print("Candidate count:",len(candidates))


def getOctaves(candidate):
    steps = getSteps(candidate)[:-1] # remove the 24
    steps8a = [x % 12 for x in steps]
    steps8a = [("+" if steps8a.count(x) > 1 else ".") for x in steps8a ]
    return "".join(steps8a)
    
def getOctaveDeficitSteps(candidate):
    steps = getSteps(candidate)[:-1] # remove the 24
    steps8a = [x % 12 for x in steps]
    return  [x for x in steps if steps8a.count(x%12) == 1 ]
    
def countOctaveDeficits(candidate):
    return len([1 for x in getOctaves(candidate) if x == "."])

print()
print("Filtering out all candidates with more than three steps that do not")
print("have their octave in the scale.")
print()
candidates = [x for x in candidates if countOctaveDeficits(x) <= 3] 

print("Candidate count:",len(candidates))


def hasConsecutiveOctaveDeficit(candidate):
    test = getOctaves(candidate) * 2
    return ".." in test
    
print()
print("Filtering out all candidates where two consecutive steps do not")
print("have their octave in the scale.")
print()
candidates = [x for x in candidates if not hasConsecutiveOctaveDeficit(x)] 

print("Candidate count:",len(candidates))

def hasDiatonicSubscale(candidate):
    steps = frozenset(getSteps(candidate * 2))
    for s in major_rotations:
        steps2 = getSteps(s)
        for x0 in steps:
            if x0 >= 24:
                continue
            if steps.issuperset([x0+x for x in steps2]):
                return True
    return False


print()
print("Filtering out all candidates that have a diatonic modal scale as")
print("a sub-scale with possible additional steps inserted between them.")
print()
candidates = [x for x in candidates if not hasDiatonicSubscale(x)] 

print("Candidate count:",len(candidates))

def structureInfo(candidate):
    return " %d%d%d%d%d%d"%(len([1 for x in candidate if x == A]),
    len([1 for x in candidate if x == B]),
    len([1 for x in candidate if x == C]),
    len([1 for x in candidate if x == D]),
    len([1 for x in candidate if x == E]),
    len([1 for x in candidate if x == F]))


def structureInfoRot(candidate):
    data = "".join(candidate)
    data = data + data[:2]
    a = 0
    b = 0
    c = 0
    d = 0
    e = 0
    f = 0
    q = 0
    for i in range(len(data)-2):
        x = data[i:i+3]
        if x == A:
            a += 1
        elif x == B:
            b += 1
        elif x == C:
            c += 1
        elif x == D:
            d += 1
        elif x == E:
            e += 1
        elif x == F:
            f += 1
        elif x == "FFF":
            q += 1
    return "  %d%d%d%d%d%d%d"%(q,a,b,c,d,e,f)


def backConversion(candidate):
    d = {A:"A",B:"B",C:"C",D:"D",E:"E",F:"F"}
    return list(map(d.get,candidate))
    
candidate_strings = [(structureInfoRot(x),list(map(lambda x: 1 if x == "." else 2, getOctaves(x))),"".join(x),backConversion(x),x) for x in candidates]
candidate_strings.sort()

candidate_strings = [(a,b) for x,y,z,a,b in candidate_strings]

def getNormalizedBaseScale(candidate):
    scale = "".join(candidate)
    rotations = [scale[i:]+scale[:i] for i in range(len(scale))]
    rotations.sort()
    return rotations[0]

types = [getNormalizedBaseScale(y) for (x,y) in candidate_strings]

candidates = [b for a,b in candidate_strings]

interesting_scales = []

new_page()


print()
print("Generated scales with the desired properties, up to rotation")
print("============================================================")
print()
print("#\t ID\t Code\t Steps        \t Has 8a?           xABCDEF")
cnt = 0
for i,(x,y) in enumerate(candidate_strings):
    snorm = getNormalizedBaseScale(y)
    t = types.index(snorm)
    if (t != i):
        continue
    print(romannbr[cnt],"\t",i,"\t","".join(x),"\t","".join(y),getOctaves(y),structureInfoRot(y))
    interesting_scales.append((cnt,i,"".join(x),"".join(y)))
    cnt += 1
scaletypes = [x for a,b,c,x in interesting_scales]

def getName(x):
    return note_names2[(x+base_note)%12].upper()
    
    
def checkChord(steps, root, chord):
    return all(((root + x)%24 in steps for x in chord))
    
def checkStepsForChord(steps, chord):
    return [" X" if checkChord(steps,x,chord) else " ." for x in steps]

def octaveFree(steps):
    return [" -" if checkChord(steps,x,[12]) else " *" for x in steps]

modelist = []
modelist2 = []

def nextMode(candidate0):
    candidate = "".join(candidate0)
    position = getOctaves(candidate)[1:].index(".") + 1
    return candidate[position:]+candidate[:position]


for (cnt,i,name,s0) in interesting_scales:
    
    for modenbr in [0,1,2]:
        s = s0
        for i in range(modenbr):
            s = nextMode(s)
        steps0 = getSteps(s)
        
        new_page()
        
        print("  "+romannbr[cnt],"-",modenbr,"\t","     ","\t",s)
        print("="*41)
        steps = steps0[:-1]
        intervals = [("8",12),("3-",3),("5",7),
        ("7",10),("3",4),("maj7",11),
        ("6-",8),("6",9),("4",5),("b5",6)]
        print("Intervals:")
        for iname,istep in intervals:
            hasStep = ["+" if ((x+istep)%24) in steps else "." for x in steps]
            print("\t\t"+iname+"\t","".join(hasStep))
            if istep != 12:
                istep += 12
                iname = "8+"+iname
                hasStep = ["+" if ((x+istep)%24) in steps else "." for x in steps]
                print("\t\t"+iname+"\t","".join(hasStep))
            #print()
        steps = steps0
        print()
        #new_page()
        #print("  "+romannbr[cnt],"-",modenbr,"\t",name,"\t",s)
        #print("="*41)
        
        print("Root-Notes:"+" "*5," ".join(map(getName,steps)))
        print("           "+" "*5," ".join(octaveFree(steps)))
        hexadecimal_roots = list(map(lambda x:"%2s"%x,
            pitches.getPitchNames([x + base_note for x in steps])))
        for i in range(15):
            if s[i] == "F":
                hexadecimal_roots[i] = hexadecimal_roots[i] + " "
            else:
                hexadecimal_roots[i] = hexadecimal_roots[i] + "`"
        hexadecimal_roots_prnt = "".join(hexadecimal_roots).replace("` ","``")
        print("  Chord [#roots]" ,hexadecimal_roots_prnt)
        chords = [
                ("5",[7]),
                ("5+8",[7,12]),
                ("4",[5]),
                ("3m",[3,7]),
                ("3+",[4,7]),
                ("maj7",[4,7,11]),
                ("dom7",[4,7,10]),
                ("min7",[3,7,10]),
                ("b7",[3,6]),
                ("maj7-5",[4,11]),
                ("dom7-5",[4,10]),
                ("min7-5",[3,10]),
                ("8+5",[12,12+7]),
                ("8+4",[12,12+5]),
                ("8+3m",[12,12+3,12+7]),
                ("8+3+",[12,12+4,12+7]),
                ("8+maj7",[12,12+4,12+7,12+11]),
                ("8+dom7",[12,12+4,12+7,12+10]),
                ("8+min7",[12,12+3,12+7,12+10]),
                ("8+b7",[12,12+3,12+6]),
                ("8+maj7-5",[12,12+4,12+11]),
                ("8+dom7-5",[12,12+4,12+10]),
                ("8+min7-5",[12,12+3,12+10]),
                ]
        
        for name_,chord in chords:
            name0 = " "*(10-len(name_))+name_ + " "
            print(name0," ".join(["[%2s"%str(len([x for x in checkStepsForChord(steps[:-1],chord) if x.strip() != "."]))+"]"]+checkStepsForChord(steps,chord)))
    
    oct_deficit = getOctaveDeficitSteps(s0)
    oct_deficit2 = oct_deficit[1:]+oct_deficit[:1]
    oct_deficit3 = oct_deficit[2:]+oct_deficit[:2]
            
    for r in range(12):
        modelist.append(("-".join(map(lambda x :getName(x),[x+r for x in oct_deficit])),cnt,0))
        modelist.append(("-".join(map(lambda x :getName(x),[x+r for x in oct_deficit2])),cnt,1))
        modelist.append(("-".join(map(lambda x :getName(x),[x+r for x in oct_deficit3])),cnt,2))
    
    for i in range(3):
        s = s0
        for x in range(i):
            s = nextMode(s)
        steps = getSteps(s)
        oct_deficit = getOctaveDeficitSteps(s)
        for r in range(24):
            rel_pitches = [r+x for x in oct_deficit + steps]
            canonical_names = pitches.getPitchNames(rel_pitches)
            deficit_names = ["%2s"%x for x in canonical_names[:len(oct_deficit)]]
            modelist2.append(("-".join(deficit_names), cnt, i))
            
        

# reset base note
base_note = 0
        
for (cnt,i,name,s0) in interesting_scales:
    if cnt % 5 == 0:
        new_page()
        print("Chromatic Mode Representants:")
        print()

    oct_deficit = getOctaveDeficitSteps(s0)
    oct_deficit2 = oct_deficit[1:]+oct_deficit[:1]
    oct_deficit3 = oct_deficit[2:]+oct_deficit[:2]
    print("%12s"%(romannbr[cnt]+" - 0:"),"|".join(["%8s"%"-".join(map(lambda x :getName(x),[x+r+12-oct_deficit[0] for x in oct_deficit])) for r in range(6)]))
    print("%12s"%(""),"|".join(["%8s"%"-".join(map(lambda x :getName(x),[x+r+12-oct_deficit[0] for x in oct_deficit])) for r in range(6,12)]))
    print()
    print("%12s"%(romannbr[cnt]+" - 1:"),"|".join(["%8s"%"-".join(map(lambda x :getName(x),[x+r+12-oct_deficit2[0] for x in oct_deficit2])) for r in range(6)]))
    print("%12s"%(""),"|".join(["%8s"%"-".join(map(lambda x :getName(x),[x+r+12-oct_deficit2[0] for x in oct_deficit2])) for r in range(6,12)]))
    print()
    print("%12s"%(romannbr[cnt]+" - 2:"),"|".join(["%8s"%"-".join(map(lambda x :getName(x),[x+r+12-oct_deficit3[0] for x in oct_deficit3])) for r in range(6)]))
    print("%12s"%(""),"|".join(["%8s"%"-".join(map(lambda x :getName(x),[x+r+12-oct_deficit3[0] for x in oct_deficit3])) for r in range(6,12)]))
    print()

modelist.sort()
for nbr,(a,b,c) in enumerate(modelist):
    if (nbr % 48) == 0:
        new_page()
        print("Chromatic Representant to Type and Mode Conversion Chart:")
        print()

    print("%3d"%(nbr+1),"%12s"%a,"is of form",romannbr[b],"-",c)
    
last_type = None

for nbr,(a,b,c) in enumerate(modelist2):
    if nbr % (9*24) == 0:
        new_page()
        print("Hexdecimal Mode Representants:",end="")
    t = romannbr[b]+" - "+str(c)
    if last_type != t:
        print("\n\n%12s"%t,end=" ")
        last_type = t
    elif nbr % 6 == 0:
        print("\n%12s"%"",end=" ")
        
    print("%8s"%a,end=" " if nbr%6 == 5 else "|")

modelist2.sort()
for nbr,(a,b,c) in enumerate(modelist2):
    if (nbr % 48) == 0:
        new_page()
        print("Hexdecimal Representant to Type and Mode Conversion Chart:")
        print()

    print("%3d"%(nbr+1),"%12s"%a,"is of form",romannbr[b],"-",c)
