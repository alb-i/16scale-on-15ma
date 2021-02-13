#!/usr/bin/env python3

import tabs as t

""" This module may be used to create apple scripts that control
    GP7 note entering """
    
def getScriptHeader():
    return "\n".join(map(lambda x: x.strip(), """
        activate application "Guitar Pro 7"
        delay 0.1

        set keycode_right to 124
        set keycode_left to 123
        set keycode_up to 126
        set keycode_down to 125
        set keycode_enter to 36

        set keydelay to 0.025
        """.splitlines()))+"\n"
        
def selectCurrentChord():
    return "\n".join(map(lambda x: x.strip(), """
    tell application "System Events" to tell process "Guitar Pro 7"
	    key code keycode_up using {shift down}
    end tell
    delay keydelay
    """.splitlines()))+"\n"
    
def togglePalmMute():
    return "\n".join(map(lambda x: x.strip(), """
    tell application "System Events" to tell process "Guitar Pro 7"
	    keystroke "p"
    end tell
    delay keydelay
    """.splitlines()))+"\n"

def nextNote():
    return "\n".join(map(lambda x: x.strip(), """
    tell application "System Events" to tell process "Guitar Pro 7"
	    key code keycode_right
    end tell
    delay keydelay
    """.splitlines()))+"\n"

def stringUp():
    return "\n".join(map(lambda x: x.strip(), """
    tell application "System Events" to tell process "Guitar Pro 7"
	    key code keycode_up
    end tell
    delay keydelay
    """.splitlines()))+"\n"

def stringDown():
    return "\n".join(map(lambda x: x.strip(), """
    tell application "System Events" to tell process "Guitar Pro 7"
	    key code keycode_down
    end tell
    delay keydelay
    """.splitlines()))+"\n"

def putKeystrokes(keys):
    return "\n".join(map(lambda x: x.strip(), """
    tell application "System Events" to tell process "Guitar Pro 7"
	    keystroke "KEYSTROKES"
    end tell
    delay keydelay
    """.splitlines())).replace("KEYSTROKES",keys)+"\n"

def setAltText(text):
    return putKeystrokes("t") + putKeystrokes(text) + "\n".join(map(lambda x: x.strip(), """
    tell application "System Events" to tell process "Guitar Pro 7"
	    key code keycode_enter
    end tell
    delay keydelay
    """.splitlines()))+"\n"

def enterTabScript(columnwise,auxinfo={},title=""):
    current_string = 0
    collate = [getScriptHeader()]
    
    if title:
        collate.append(setAltText(title))
    
    for i,c in enumerate(columnwise):
        if i in auxinfo:
            aux = auxinfo[i]
        else:
            aux = {}
        is_a_rest = True
        for j,f in enumerate(c):
            if f == t.unplayedString:
                continue
            if (current_string != j):
                if current_string > j:
                    for k in range(j,current_string):
                        collate.append(stringDown())
                else:
                    for k in range(current_string,j):
                        collate.append(stringUp())
                current_string = j
            is_a_rest = False
            collate.append(putKeystrokes(str(f)))
        if is_a_rest:
            collate.append(putKeystrokes("r"))
            
        if "pm" in aux:
            collate.append(selectCurrentChord())
            collate.append(putKeystrokes("p"))
        collate.append(nextNote())
    return "\n".join(collate)
