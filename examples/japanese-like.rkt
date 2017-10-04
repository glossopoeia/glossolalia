#lang glossolalia

Sounds
------

@onset   = k, g, s, z, t, d, n, h, b, m, r, w
@palatal = y
@short   = a, e, i, o, u
@long    = ā, ē, ī, ō, ū
@coda    = n, k = 30%

Syllables
---------

$vowels        = @short | @long
$simple  : 50% = @onset @short | @onset @long
$coda          = @onset @short @coda | @onset @long @coda
$palatal : 25% = @palatal @short | @onset @palatal @short = 25%
               | @onset @palatal @short @coda | @palatal @long 
               | @onset @palatal @long  = 25% | @onset @palatal @long @coda

Rules
-----

w only-followed-by a, ā
@palatal only-followed-by a, u, o, ā, ō, ū
k never-ends-word

-- Remember, all transformers are applied to each word from top to bottom in an *accumulative* fashion.

-- These three rules show our 'hack' for getting geminate consonants.
k becomes p before p
k becomes s before s
k becomes t before t

-- These rules make the romanization more familiar for English speakers.
s becomes sh before i, ī, y
t becomes ch before i, ī, y
z becomes j  before i, ī, y
y becomes  after sh, ch, j

Configuration
-------------

Seed = 67