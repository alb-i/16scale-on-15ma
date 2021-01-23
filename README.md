# 16scale-on-15ma

* `scales.py` is a script that generates 16-notes per two-octaves scales with certain properties.
* `format_printable.py` formats the output such that it can be printed typewriter
style.

## Quintadecimal Pitch Names


In order to represent the right pitches of the individual quintadecimal base pitches
`JKLMNOPQRSTUVXY` such that each new scale mode is easily represented by these base pitches
together with `#`'s and `b`'s. 
We let `J` be every second `C`, because `GHIJ` and `GABC` should go analogously. 
And we do not use the letter `W`, because it does not
fit the meter when speaking. Observe that if you go a step further after `Y` you
would get to `Z`, which then is also `C`.

* I used `find_base_pitches.py` in order to assign the other 14 new base pitches
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

So, the last thing we need to define is which `C`'s are considered
to be `J`s. I use the following convention: the (low) `C` string on my
8-string guitar is also a `J` string. Thus my guitar is tuned
`R#`, `U#`, `J`, `M#`, `P#`, `S`, `V`, `K` in all perfect 4th steps. 
If I'm not mistaken, then this guitar is tuned down quite a bit, 
so the `M` should correspond to the standard (low, and high) `E` string 
on a guitar. Which means that the standard 5-string bass `B` is
indeed a `Q`, and so the standard bass `E` is a `T`, one octave below
the guitars `M`. 

* The standard guitar tuning is `M`, `P`, `R#`, `U#`, `Y`, `M`,
from low to high; 
* the standard 5-string bass tuning is `Q`, `T`, `X`, `K#`, `N#`;
* the concert pitch `A` (usually set to 440Hz) then refers to `P` (just like the word pitch). 


The full chromatic quintadecimal scale can be enumerated like this:
```
Classical:     C, C#,  D, D#, E,  F, F#,  G, G#, A, A#, B,  C, C#,  D, D#, E,  F, F#,  G, G#, A, A#, B, C  
Quintadecimal: J,  K, K#,  L, M, M#,  N, N#,  O, P, P#, Q, Q#,  R, R#,  S, T, T#,  U, U#,  V, X, X#, Y, J
Quintadecimal: J,  K, Lb,  L, M, Nb,  N, Ob,  O, P, Qb, Q, Rb,  R, Sb,  S, T, Ub,  U, Vb,  V, X, Yb, Y, J
Classical:     C, Db,  D, Eb, E,  F, Gb,  G, Ab, A, Bb, B,  C, Db,  D, Eb, E,  F, Gb,  G, Ab, A, Bb, B, C
```

We refer to the regular old `CDEFGABC`-scale as either the classical or the octaval scale.

## Scale Tables

For more in detail information, look into [scales.pdf](scales.pdf).
