#lang glossolalia

Sounds
------

@onset-one = s, sch, p, t, k, b, d, g, f, v, l, r, m, n, z, h
@onset-two = sp, st, sk, sl, schp, scht, schk, schl, schr, schm, schn, pl, pr, pf, bl, br, tr, dr, kl, kr, kn,
             gl, gr, gn, fr, fl
@onset-tri = spl, spr, str, skr, skl, schpl, schpr, schtr, schkr, schkl

@vowel = a, e, i, o, u, y, ä, ü, ö

@coda-one = r, l, m, n, ng, s, sch, p, t, k, b, d, g, f, v, ch
@coda-two = rl, rm, rn, lm, ln, lng, rs, rp, rt, rk, rb, rd, rg, rf, rch, ls, lsch, lp, lt, lk, lb, ld, lf, lch,
            mz, msch, mp, mf, nz, nsch, nt, nd, nf, ngz, ngk, sp, st, sk, ps, pt, pf, ts, tsch, ks, kt, fs, chs, cht
@coda-tri = rls, lms, lns, lps, mst, nst, pst, ngkt, ngst
@coda-qua = lmst, lnst, ngkst

Syllables
---------

$vowel          = @vowel
$simple         = @onset-one @vowel | @onset-two @vowel | @onset-tri @vowel = 10%
$full-one       = @onset-one @vowel @coda-one | @onset-one @vowel @coda-two | @onset-one @vowel @coda-tri = 10% | @onset-one @vowel @coda-qua = 5%
$full-two : 10% = @onset-two @vowel @coda-one | @onset-two @vowel @coda-two | @onset-two @vowel @coda-tri = 10% | @onset-two @vowel @coda-qua = 5%
$full-tri : 5%  = @onset-tri @vowel @coda-one | @onset-tri @vowel @coda-two | @onset-tri @vowel @coda-tri = 10% | @onset-tri @vowel @coda-qua = 5%

Rules
-----

@vowel never-doubled
@coda-qua, @coda-tri never-followed-by @onset-tri

Configuration
-------------

Seed = 999
