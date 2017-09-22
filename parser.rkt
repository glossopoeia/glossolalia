#lang brag

t-file : /t-star-newl t-categories t-structures t-rules t-generate

t-categories : /"Categories" /t-plus-newl ([t-category] /t-star-newl)+
t-category   : GROUP-NAME /"=" t-cat-sound (/"," t-cat-sound)* /NEWLINE
t-cat-sound  : SOUND-NAME [/"=" PERCENTAGE]

t-structures     : /"Structures" /t-plus-newl ((t-structure | t-structure-perc) /t-star-newl)+
t-structure      : /INTEGER /"." GROUP-NAME+ /NEWLINE
t-structure-perc : /INTEGER /"." GROUP-NAME+ /"=" PERCENTAGE /NEWLINE

t-rules       : /"Rules" /t-plus-newl (t-rule /t-star-newl)*
@t-rule       : t-unary-rule
              | t-binary-rule
t-unary-rule  : t-rule-args UNARY-RULE-NAME /NEWLINE
t-binary-rule : t-rule-args BINARY-RULE-NAME t-rule-args /NEWLINE
t-rule-args   : t-rule-arg (/"," t-rule-arg)*
@t-rule-arg   : SOUND-NAME | GROUP-NAME

t-generate     : /"Generate" /t-plus-newl (t-config-item /t-star-newl)*
@t-config-item : t-seed | t-count | t-shortest | t-longest | t-mode
t-seed         : /"Seed" /"=" INTEGER /NEWLINE
t-count        : /"Count" /"=" INTEGER /NEWLINE
t-shortest     : /"Shortest" /"=" INTEGER /NEWLINE
t-longest      : /"Longest" /"=" INTEGER /NEWLINE
t-mode         : /"Mode" /"=" INTEGER /NEWLINE

t-plus-newl : NEWLINE+
t-star-newl : NEWLINE*