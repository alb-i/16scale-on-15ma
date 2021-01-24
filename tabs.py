#!/usr/bin/env python3

import pitches as p

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
    rowwise = transposeAndMap(columnwise,formatter,formatter(""))
    if len(textleft) < len(rowwise):
        textleft += [""] * (len(rowwise)-len(textleft))
    elif len(textleft) > len(rowwise):
        rowwise += [[""]] * (len(textleft)-len(rowwise))
    maxwidth = max((len(x) for x in textleft))
    for t,r in zip(textleft,rowwise):
        print(fillLeft(t,maxwidth),"".join(r),sep="")
        
def fillColumnsFromTop(columnwise, filler=""):
    maxheight = max((len(x) for x in columnwise))
    return [[filler]*(maxheight-len(x)) + x for  x in columnwise]

def reverseColumns(columnwise):
    return [list(reversed(x)) for x in columnwise]