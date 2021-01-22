# 16scale-on-15ma

* `scales.py` is a script that generates 16-notes per two-octaves scales with certain properties.
* `format_printable.py` formats the output such that it can be printed typewriter
style.

Currently, I'm looking for the right pitch of the individual scale notes
`JKLMNOPQRSTUVXY` such that each mode is easily represented by these pitches
and `#` and `b`'s. We let `J` be every second `C`, because `GHIJ` and `GABC` should go analogously. And we do not use the letter `W`, because it does not
fit the meter when speaking. Also, if you go a step further after `Y` you
would get to `Z`, which then is also `C`.

* I used `find_base_pitches.py` in order to assign the other 14 new note names
in a sensible way:

```
J =%=  C  (1/2) step, =     J, Kb, Y#  +/-8va =       Rb, Q#
K =%= C#   full step, =         K, J#  +/-8va =            R
L =%= D#  (1/2) step, =         L, Mb  +/-8va =        S, Tb
M =%=  E   full step, =         M, L#  +/-8va =        T, S#
N =%= F#   full step, =             N  +/-8va =            U
O =%= G#  (1/2) step, =         O, Pb  +/-8va =        V, Xb
P =%=  A   full step, =         P, O#  +/-8va =        X, V#
Q =%=  B   full step, =             Q  +/-8va =        Y, Jb
R =%= C#   full step, =             R  +/-8va =        K, J#
S =%= D#  (1/2) step, =         S, Tb  +/-8va =        L, Mb
T =%=  E   full step, =         T, S#  +/-8va =        M, L#
U =%= F#   full step, =             U  +/-8va =            N
V =%= G#  (1/2) step, =         V, Xb  +/-8va =        O, Pb
X =%=  A   full step, =         X, V#  +/-8va =        P, O#
Y =%=  B  (1/2) step, =         Y, Jb  +/-8va =            Q
```

First, we have pairs of named pitches that are an octave apart:
`KR`, `LS`, `MT`, `NU`, `OV`, `PX`, and `QY`; and this is as
many octave-pairs that we might hope for when using 15 pitches
on 2 octaves. Thus `J` is the only pitch where the corresponding
octave is not a named pitch, but rather `R#` or `Qb`.

Next, there is an easy rule to remember where the semi-tone steps
are between the named pitches: Beginning with `J`, you start with a half-step,
then there is 1 full step, again a half-step, then 2 full steps,
then a half-step followed by 3 full steps, then again a
half-step, followed by 2 full steps, another half-step,
then 1 full step, and the final half step. Note that it does not
matter whether you go up or down from `J`.