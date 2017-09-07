#lang trinity/parser-only

Categories
----------

@init = k, d, t, p, m, n, l, r
@nuc = a, e, i, o, u
@fin = k, t, th, m, n, g, z, s


Structures
----------

1. @init @nuc @fin
2. @nuc @fin


Rules
-----

k never-followed-by g
s never-followed-by @init
a, o never-in-same-word-as i, u

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


Frequencies
-----------

-- frequencies are independent among groups
--     so 'k' technically has 2 frequencies: one for @init, and one for @fin
-- added percentage total must be <100
-- if all elements in the group specify a percentage, the percentage total must be =100
-- elements not mentioned will have the remaining percentage divided among them, even if it's zero

@init:
    k = 12%
    p, t = 7%

Generate
--------

Seed = 123456
Count = 5000
