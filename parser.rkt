#lang brag

t-file : t-categories t-syllables t-rules t-generate

t-categories : /"Sounds" t-category+
t-category   : GROUP-NAME /"=" t-cat-sound (/"," t-cat-sound)*
t-cat-sound  : SOUND-NAME [/"=" PERCENTAGE]

t-syllables     : /"Syllables" (t-syllable | t-syllable-perc)+
t-syllable      : SYLLABLE-NAME /"=" t-syll-variant (/"|" t-syll-variant)*
t-syllable-perc : SYLLABLE-NAME /":" PERCENTAGE /"=" t-syll-variant (/"|" t-syll-variant)*
@t-syll-variant : t-syll-con | t-syll-con-perc
t-syll-con      : GROUP-NAME+
t-syll-con-perc : GROUP-NAME+ /"=" PERCENTAGE

t-rules        : /"Rules" t-rule*
@t-rule        : t-unary-rule
               | t-binary-rule
               | t-ternary-rule
               | t-unary-srule
               | t-binary-srule
t-unary-rule   : t-rule-args UNARY-RULE-NAME
t-binary-rule  : t-rule-args BINARY-RULE-NAME t-rule-args
t-ternary-rule : t-rule-args TERNARY-RULE-NAME SOUND-NAME TERNARY-INDIRECT t-rule-args
t-unary-srule  : t-srule-args UNARY-RULE-NAME
t-binary-srule : t-srule-args BINARY-RULE-NAME t-srule-args
t-rule-args    : t-rule-arg (/"," t-rule-arg)*
@t-rule-arg    : SOUND-NAME | GROUP-NAME
t-srule-args   : SYLLABLE-NAME (/"," SYLLABLE-NAME)*

t-generate     : /"Configuration" t-config-item*
@t-config-item : t-seed | t-count | t-shortest | t-longest | t-mode
t-seed         : /"Seed" /"=" INTEGER
t-count        : /"Count" /"=" INTEGER
t-shortest     : /"Shortest" /"=" INTEGER
t-longest      : /"Longest" /"=" INTEGER
t-mode         : /"Mode" /"=" INTEGER