#lang glossolalia

Sounds
------

@onset   = k, g, s, z, t, d, n, h, b, m, r, w
@palatal = y
@short   = a, e, i, o, u
@long    = ā, ē, ī, ō, ū
@coda    = n, k

Syllables
---------

$vowels        = @short | @long
$simple  : 50% = @onset @short | @onset @long
$coda          = @onset @short @coda | @onset @long @coda
$palatal : 25% = @palatal @short | @onset @palatal @short = 25% | @onset @palatal @short @coda |
                 @palatal @long  | @onset @palatal @long  = 25% | @onset @palatal @long @coda

Rules
-----

w only-followed-by a, ā
@palatal only-followed-by a, u, o, ā, ō, ū

k never-ends-word
k becomes p before p
k becomes s before s
k becomes t before t
s becomes sh before i, ī

Configuration
-------------
