#!/usr/bin/env python3

import sys,os

def disablePrint():
    sys.stdout = open(os.devnull, 'w')

def enablePrint():
    sys.stdout = sys.__stdout__

disablePrint()
import scales
enablePrint()

pitches = [scales.getSteps(x) for x in scales.scaletypes]

possible_values = {}

for p in pitches:
    for i,x in enumerate(p):
        l0 = possible_values.get(i,[])
        l0.append(x)
        possible_values[i] = l0
        
pitch0 = ord("J")

pitch_names = "JKLMNOPQRSTUVXY"

pitch_values = []

for i in range(15):
    pitch_name = pitch_names[i]
    values = sorted(set(possible_values[i]))
    c = len(values)
    if c == 1:
        print(pitch_name," = always",scales.getName(values[0]))
        pitch_values.append(values[0])
    elif c == 3:
        print(pitch_name," = middle",scales.getName(values[1]))
        pitch_values.append(values[1])
    elif c == 2:
        if i < 8:
            print(pitch_name," =  lower",scales.getName(values[0]))
            pitch_values.append(values[0])
        else:
            print(pitch_name," =  upper",scales.getName(values[1]))
            pitch_values.append(values[1])
            
pitch_values.append(24)

def getXNames(pitch):
    x = (pitch + 240) % 24
    candidates = []
    if x in pitch_values:
        candidates.append(pitch_names[pitch_values.index(x)])
    if (x+1)%24 in pitch_values:
        candidates.append(pitch_names[pitch_values.index((x+1)%24)]+"b")
    if (x+23)%24 in pitch_values:
        candidates.append(pitch_names[pitch_values.index((x+23)%24)]+"#")
    return candidates

print()

for i in range(15):
    stepsize = pitch_values[i+1]-pitch_values[i]
    if stepsize == 1:
        print(pitch_names[i], "=%=", scales.getName(pitch_values[i]), " (1/2) step, = ", 
        "%12s"% ", ".join(getXNames(pitch_values[i])),
        " +/-8va =",
        "%12s"% ", ".join(getXNames(pitch_values[i]+12)), )
    else:
        print(pitch_names[i],"=%=", scales.getName(pitch_values[i]), "  full step, = ", 
        "%12s"% ", ".join(getXNames(pitch_values[i])),
                " +/-8va =",
        "%12s"% ", ".join(getXNames(pitch_values[i]+12)) )

print()

for i in range(24):
    print("Pitch ","%2d"%i," =%= ", scales.getName(i), " = ",
    ", ".join(getXNames(i)))