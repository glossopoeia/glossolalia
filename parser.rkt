#lang brag

t-file : /t-star-newl t-categories t-structures t-rules t-frequencies t-generate

t-categories : /"Categories" /t-plus-newl ([t-category] /t-star-newl)+
t-category   : GROUP-NAME /"=" SOUND-NAME (/"," SOUND-NAME)* /NEWLINE

t-structures : /"Structures" /t-plus-newl (t-structure /t-star-newl)+
t-structure  : /INTEGER /"." GROUP-NAME+ /NEWLINE

t-rules       : /"Rules" /t-plus-newl (t-rule /t-star-newl)*
@t-rule       : t-unary-rule
              | t-binary-rule
t-unary-rule  : t-rule-args UNARY-RULE-NAME /NEWLINE
t-binary-rule : t-rule-args BINARY-RULE-NAME t-rule-args /NEWLINE
t-rule-args   : t-rule-arg (/"," t-rule-arg)*
@t-rule-arg   : SOUND-NAME

t-frequencies    : /"Frequencies" /t-plus-newl (t-group-frequency /t-star-newl)*
t-group-frequency : GROUP-NAME /":" /t-plus-newl (t-sound-frequency /t-star-newl)+
t-sound-frequency : t-sound-names /"=" PERCENTAGE
@t-sound-names    : SOUND-NAME (/"," SOUND-NAME)*

t-generate : /"Generate" /t-plus-newl t-seed /t-star-newl t-count /t-star-newl t-longest /t-star-newl
@t-seed    : /"Seed" /"=" INTEGER /NEWLINE
@t-count   : /"Count" /"=" INTEGER
@t-longest : /"Longest" /"=" INTEGER

t-plus-newl : NEWLINE+
t-star-newl : NEWLINE*