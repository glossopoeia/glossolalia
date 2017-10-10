#lang glossolalia

Sounds
------

@init = k = 50%, d, t = 7%, p = 7%, m, n, l, r
@nuc = a, e, i, o, u
@fin = k, t, th, m, n, g, z, s


Syllables
---------

$init = @init @nuc @fin
$no-init = @nuc @fin

Rules
-----

k never-starts-word
$no-init only-followed-by $init


Configuration
-------------

Seed = 123456
Count = 20
Longest = 4
Shortest = 2
