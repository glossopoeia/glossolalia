#lang glossolalia

Sounds
------

-- This is my favorite example because it showcases a lot of
-- the cool things Glossolalia can do. Perhaps the biggest thing
-- to note is the 'k' hack: in the @coda group, we include a
-- psuedo-sound 'k', which later gets converted into geminate consonants
-- in the transformation rules.

@onset   = k, g, s, z, t, d, n, h, b, m, r, w
@palatal = y
@short   = a, e, i, o, u
@long    = ā, ē, ī, ō, ū
@coda    = n, k = 30%

Syllables
---------

-- We use some percentages to cut down on the number of syllables with a
-- coda or a palatal.

$vowels        = @short | @long
$simple  : 50% = @onset @short | @onset @long
$coda          = @onset @short @coda | @onset @long @coda
$palatal : 25% = @palatal @short
               | @onset @palatal @short = 25%
               | @onset @palatal @short @coda
               | @palatal @long 
               | @onset @palatal @long  = 25%
               | @onset @palatal @long @coda

Rules
-----

w only-followed-by a, ā
@palatal only-followed-by a, u, o, ā, ō, ū
k never-ends-word

-- Remember, all transformers are applied to each word from top to
-- bottom in an *accumulative* fashion.

-- These three rules show our 'hack' for getting geminate consonants.
-- This relies on the fact that a 'k' sound in the onset never occurs
-- before anything but a palatal or vowel.
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