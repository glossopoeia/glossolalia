#lang glossolalia

Sounds
------

@onset = p, t, k, v, r, g
@short = a, e, i, o, u
@long  = aa, ee, ii, oo, uu, ie, ia, io, iu, ei, ea, eo,
         eu, ai, ae, ao, au, oi, oe, oa, ou, ui, ue, ua, uo

Syllables
---------

$vowel        = @short | @long
$simple : 75% = @onset @short | @onset @long

Rules
-----

@long never-starts-word

Configuration
-------------

Seed    = 9001
Count   = 100
Longest = 6
