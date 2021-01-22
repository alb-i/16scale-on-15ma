#!/usr/bin/env python3

import sys

tabsize = 8
pagelines = 54
statuslines = 3
pagebreakmarker = "-"*67

leftmargin = 2
rightmargin = 66

printablepagelines = pagelines-statuslines

lines = map(lambda x: x.rstrip().expandtabs(tabsize), sys.stdin.readlines())

sections = []
current_section = []

for l in lines:
    if (l.strip() == pagebreakmarker):
        sections.append(current_section)
        current_section = []
        continue
    if (l.strip() == ""):
        current_section.append("")
    else:
        current_section.append(l)
    
sections.append(current_section)

def remove_blank_surroundings(section):
    while len(section) > 0 and (section[0] == ""):
        section = section[1:]
    while len(section) > 0 and (section[-1] == ""):
        section = section[:-1]
    return section
    
sections = map(remove_blank_surroundings, sections)

sections = filter(lambda x : len(x) > 0, sections)

pages = []

for section in sections:
    while len(section) > printablepagelines:
        pages.append(section[:printablepagelines])
        section = section[printablepagelines:]
    pages.append(section)
    
def augment_page(page):
    return page + [""]*(printablepagelines - len(page))
    
pages = list(map(augment_page,pages))

headerline = ""
if len(pages) > 0:
    if len(pages[0]) > 0:
        headerline = pages[0][0]
        pages[0] = pages[0][1:]+[""]

nbr_pages = sum((1 for p in pages))

def formatP(nbr,x,center=""):
    if (nbr % 2) == 0:
        p0 = (" "*leftmargin) + x
    else:
        p0 = (" "*(rightmargin-len(x))) + x
    center_start = (2*leftmargin + (rightmargin-leftmargin) - len(center))//2
    center_end = center_start + len(center)
    if len(p0) < center_end:
        p0 = p0+ " "*(center_end-len(p0))
    return p0[:center_start] + center + p0[center_end:]


for nbr_,page in enumerate(pages):
    nbr = nbr_ + 1
    print(headerline)
    for l in page:
        print(l)
    print(formatP(nbr,"- "+str(nbr)+" -",             "I.Albrecht: 8va-Interval-Maximal 16-tone Scales"))
    print(formatP(nbr," of " + str(nbr_pages) + " ","on 15ma w/o Diatonic Modes and Root-8va."),end="" if (nbr == nbr_pages) else "\n")
