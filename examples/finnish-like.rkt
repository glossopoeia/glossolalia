#lang glossolalia

Sounds
------

@onset = m, n, p, t, k, f, s, h, w, l, j, r
@short = a, e, i, o, u, y, ä = 5%, ö = 5%
@long  = aa, ee, ii, oo, uu, yy, ää = 5%, öö = 5%
@coda  = m, n, ng, p, t, k, f, s, h, l, r

Syllables
---------

$vowel  : 35% = @short | @long = 10%
$simple : 40% = @onset @short | @onset @long = 15%
$full         = @onset @short @coda | @onset @long @coda = 20%

Rules
-----

y, ä, ö, yy, ää, öö never-in-same-word-as a, o, u, aa, oo, uu
p, t, k never-followed-by m, n
p, k never-followed-by w, j
p never-followed-by t, k
n, t, s, l, r never-ends-word
l, r never-followed-by l, r
m, n, ng never-followed-by m, n, ng, l, r
@long, @short never-doubled
@short, @long never-followed-by @long
aa never-followed-by a

Configuration
-------------
