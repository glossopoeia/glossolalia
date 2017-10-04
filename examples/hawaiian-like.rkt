#lang glossolalia


Sounds
------

-- Note that this analysis of Hawaiian treats (short and long) vowels and
-- dipthongs as separate phonemes. There are perhaps better ways to do this.
-- We could technically have put all short, long and dipthong vowels into
-- the same category and shortened our list of sound groups to only two,
-- but this way we can make rules differentiating between short and long
-- vowels without having to mention the sounds individually.
@onset = m, n, p, k, ', h, w, l
@short = a, e, i, o, u
@long  = ā, ē, ī, ō, ū
@dipth = iu, ou, au, eu, ou, ei, ai, ao, ae, ōu, ēi, āu, āi, āo, āe


Syllables
---------

-- Adding these percentages here keeps down on the number of syllables in
-- the average word that have no consonant. Otherwise we can end up with a
-- lot of words having 3 to 4 times as many vowels as consonants.

$vowel        = @short | @long | @dipth = 20%
$simple : 75% = @onset @short | @onset @long | @onset @dipth = 20%


Rules
-----

-- From the Wikipedia page for Hawaiian, we see that *wū* never occurs in Hawaiian,
-- and *wu* only in loan words. Let's filter them both with some rules.

w never-followed-by ū
w never-followed-by u


Configuration
-------------

Seed    = 12345
Count   = 100
Longest = 5
