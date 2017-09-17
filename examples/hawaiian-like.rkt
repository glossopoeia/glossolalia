#lang trinity


Categories
----------

-- Note that this analysis of Hawaiian treats (short and long) vowels and dipthongs as separate phonemes.
-- This doesn't have a lot of bearing on the generation, but it does make construction of the syllable
-- structure section much easier (don't have to deal with dipthongs).
-- We could technically have put all short, long and dipthong vowels into the same category and shortened
-- our list of syllable structures to only two, but this way is clearer for me and potentially more extensible.
@onset = m, n, p, k, ', h, w, l
@vowel = a, e, i, o, u
@long  = ā, ē, ī, ō, ū
@dipth = iu, ou, au, eu, oi, ei, ai, ao, ae, ōu, ēi, āu, āi, āo, āe


Structures
----------

1. @onset @vowel
2. @vowel
3. @onset @long
4. @long
5. @onset @dipth
6. @dipth


Rules
-----

-- From the Wikipedia page for Hawaiian, we see that only *wū* never occurs in Hawaiian,
-- and *wu* only in loan words. Let's filter them both with some rules.

w never-followed-by ū
w never-followed-by u


Frequencies
-----------

-- Hawaiian, like other languages, favors certain letters in certain positions over others.
-- For this simple generator, we'll leave it alone for now.


Generate
--------

Seed    = 12345
Count   = 100
Longest = 5