#!/usr/bin/env python3

import pitches as p

import sys,os

def disablePrint():
    sys.stdout = open(os.devnull, 'w')

def enablePrint():
    sys.stdout = sys.__stdout__

disablePrint()
import scales as s
enablePrint()
