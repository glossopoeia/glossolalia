#lang glossolalia

Sounds
------

@onset = p, t, k, v, r, g
@short = a, e, i, o, u
@long  = aa, ee, ii, oo, uu, ie, ia, io, iu, ei, ea, eo, eu, ai, ae, ao, au, oi, oe, oa, ou, ui, ue, ua, uo

Syllables
---------

$one         = @short
$two         = @long
$three : 50% = @onset @short
$four  : 30% = @onset @long

Rules
-----

@long never-starts-word

Configuration
-------------

Seed    = 9001
Count   = 100
Longest = 6
