#lang trinity

Categories
----------

@onset = p, t, k, v, r, g
@short = a, e, i, o, u
@long  = aa, ee, ii, oo, uu, ie, ia, io, iu, ei, ea, eo, eu, ai, ae, ao, au, oi, oe, oa, ou, ui, ue, ua, uo

Structures
----------

$one   = @short
$two   = @long
$three = @onset @short = 50%
$four  = @onset @long = 30%

Rules
-----

@long never-starts-word

Generate
--------

Seed    = 9001
Count   = 100
Longest = 6
