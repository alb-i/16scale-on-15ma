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

print("Quintadecimal vs. Classical Pitch Conversion Chart")
print("==================================================")
print()

def mapUp(x):
    flat = [y for y in x if y.lower().endswith("b")] + [""]
    sharp = [y for y in x if y.lower().endswith("#")] + [""]
    norm = [y for y in x if not (y in flat or y in sharp)] + [""]
    return [sharp[0],norm[0],flat[0]]

qdecscale = [mapUp(pitches.getPitchNameCandidates(i)) for i in range(25)]
octascale = [mapUp(pitches.getClassicalPitchNameCandidates(i)) for i in range(25)]

def pivotListOfLists(l):
    innerdim = len(l[0])
    output = [[] for i in range(innerdim)]
    for x in l:
        for i in range(innerdim):
            output[i].append(x[i])
    return output

def printScale(pitchNames,name,titleat=1):
    pitchLines = pivotListOfLists(pitchNames)
    for i,l in enumerate(pitchLines):
        if i == titleat:
            title = name
        else:
            title = " "*(len(name))
        data = [(x+"  ")[:2] for x in l]
        print(title,"".join(data).replace(" ","-"),sep="")
    
print("               ","  "*pitches.getPitch("P"),"vv",sep="")
print("               ","| "*25,sep="")
printScale(qdecscale, "Quintadecimal  ")

print("               ","| "*25,sep="")

printScale(octascale,"    Classical  ")
print("               ","| "*25,sep="")

print("               ","  "*pitches.getPitch("P"),"^^",sep="")
print("               ","  "*pitches.getPitch("P"),"concert pitch (440Hz)",sep="")
print()

printScale([[x[1]] for x in qdecscale], "               ")
print("               ","| "*25,sep="")
printScale([[x[1]] for x in octascale],"               ")

print()
printScale([[x] for x in pitches.getPitchNames([i*5 for i in range(25)])],
                     "  Circle       ",0)
print("  of           ","| "*25,sep="")
printScale([[x] for x in pitches.getClassicalPitchNames([i*5 for i in range(25)])],
                     "  Fourths      ",0)
                     

print()
printScale([[x] for x in pitches.getPitchNames([i*7 for i in range(25)])],
                     "  Circle       ",0)
print("  of           ","| "*25,sep="")
printScale([[x] for x in pitches.getClassicalPitchNames([i*7 for i in range(25)])],
                     "  Fifths       ",0)
                     
print()
printScale([[x] for x in pitches.getPitchNames([i*11 for i in range(25)])],
                     "  Circle       ",0)
print("  of Major     ","| "*25,sep="")
printScale([[x] for x in pitches.getClassicalPitchNames([i*11 for i in range(25)])],
                     "  Sevenths     ",0)
                     
print()
printScale([[x] for x in pitches.getPitchNames([i*13 for i in range(25)])],
                     "  Circle       ",0)
print("  of           ","| "*25,sep="")
printScale([[x] for x in pitches.getClassicalPitchNames([i*13 for i in range(25)])],
                     "  Minor Ninths ",0)
                    
print()
printScale([[x] for x in pitches.getPitchNames([i*17 for i in range(25)])],
                     "  Circle       ",0)
print("  of           ","| "*25,sep="")
printScale([[x] for x in pitches.getClassicalPitchNames([i*17 for i in range(25)])],
                     "  Elevenths    ",0)
                     
print()
printScale([[x] for x in pitches.getPitchNames([i*19 for i in range(25)])],
                     "  Circle       ",0)
print("  of           ","| "*25,sep="")
printScale([[x] for x in pitches.getClassicalPitchNames([i*19 for i in range(25)])],
                     "  Twelfths     ",0)