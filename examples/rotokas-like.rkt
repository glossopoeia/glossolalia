#lang glossolalia

Sounds
------

-- I included all possible diphthongs in the same sound group as
-- the long vowels. This helped make the language look more like
-- the text samples of Rotokas I was able to find.

@onset = p, t, k, v, r, g
@short = a, e, i, o, u
@long  = aa, ee, ii, oo, uu, ie, ia, io, iu, ei, ea, eo,
         eu, ai, ae, ao, au, oi, oe, oa, ou, ui, ue, ua, uo

Syllables
---------

-- We highly prefer to have a consonant in our syllables, so that we
-- don't get absurdly long strings of vowels. However, I have seen
-- Rotokas words with up to four vowels in a row.

$vowel        = @short | @long
$simple : 75% = @onset @short | @onset @long

Rules
-----

-- I honestly don't think Rotokas needs any rules.

Configuration
-------------

Seed    = 9001
Count   = 100
Longest = 6
