#lang brag

t-file : /(NEWLINE*) t-categories t-structures t-rules t-frequencies t-generate

t-categories : /"Categories" /(NEWLINE+) (t-category /(NEWLINE*))*
t-category   : GROUP-NAME /"=" SOUND-NAME (/"," SOUND-NAME)* /NEWLINE

t-structures : /"Structures" /(NEWLINE+) (t-structure /(NEWLINE*))*
t-structure  : /INTEGER /"." GROUP-NAME+ /NEWLINE

t-rules        : /"Rules" /(NEWLINE+) (t-rule /(NEWLINE*))*
t-rule         : t-unary-rule
               | t-binary-rule
@t-unary-rule  : t-rule-args UNARY-RULE-NAME /NEWLINE
@t-binary-rule : t-rule-args BINARY-RULE-NAME t-rule-args /NEWLINE
t-rule-args    : t-rule-arg (/"," t-rule-arg)*
@t-rule-arg    : SOUND-NAME | GROUP-NAME

t-frequencies       : /"Frequencies" /(NEWLINE+) (t-group-frequencies /(NEWLINE*))*
t-group-frequencies : GROUP-NAME /":" /(NEWLINE+) (t-sound-frequency /(NEWLINE*))+
t-sound-frequency   : t-sound-names /"=" PERCENTAGE
@t-sound-names      : SOUND-NAME (/"," SOUND-NAME)*

t-generate : /"Generate" /(NEWLINE+) t-seed /(NEWLINE*) t-count /(NEWLINE*)
t-seed     : /"Seed" /"=" INTEGER /NEWLINE
t-count    : /"Count" /"=" INTEGER
