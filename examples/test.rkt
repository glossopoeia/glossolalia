#lang trinity

Categories
----------

@init = k = 50%, d, t = 7%, p = 7%, m, n, l, r
@nuc = a, e, i, o, u
@fin = k, t, th, m, n, g, z, s


Structures
----------

1. @init @nuc @fin
2. @nuc @fin


Rules
-----

k never-starts-word
-- k never-followed-by g
-- s never-followed-by z
-- a, o never-in-same-word-as i, u

-- LIST OF RULES : ARITIES
-- never-followed-by : 2
-- never-preceded-by : 2
-- never-in-same-word-as : 2
-- always-followed-by : 2
-- always-preceded-by : 2
-- never-starts-word : 1
-- never-ends-word : 1
-- only-starts-word : 1
-- only-ends-word : 1


Generate
--------

Seed = 123456
Count = 20
Longest = 4
Shortest = 2
