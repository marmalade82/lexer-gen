# Design #

The goal of this codebase is to take a specification file and generate the code for simple lexical analysis. The specification should have the following features to control the behavior of the generated lexer:

- [ ] Lexing should follow the principle of maximum munch
- [ ] Lexing rules should have assignable priorities for breaking ties if necessary
- [ ] The generated tokens should allow queries concerning their line and column numbers
- [ ] The ability to specify tokens that will be discarded from the stream automatically (convenience feature so parsers don't need to do it).
- [ ] The ability to specify custom Error tokens with custom error messages
- [ ] The ability to specify synchronization tokens that mark the end of custom error tokens (which are often generated from very broad rules), so that normal lexing can continue on the rest of the input.

## Grammar

The following grammar for the codebase is suitable for LL(1) parsing.

```
[ ] Program                 ->              NormalTokens ErrorTokens DefaultTokens
[ ] NormalTokens            ->              "%%_normal_%%" NormalSpecs
[ ] ErrorTokens             ->              "%%_error_%%" ErrorSpecs
                            |               ""  
                                                // User could choose not to provide any error specs
[ ] DefaultTokens           ->              "%%_default_%% DefaultSpecs  
                                                // Allow user to customize any default settings
[ ]                         |               ""
[ ] NormalSpecs             ->              NormalSpec NormalSpecs
[ ]                         |               ""
[ ] NormalSpec              ->              TokenName Regex ";"
[ ] Regex                   ->                  // Any random sequence of characters nested in parentheses.
[ ] ErrorSpecs              ->              ErrorSpec ErrorSpecs
[ ]                         |               ""
[ ] ErrorSpec               ->              TokenName ErrorMessage OptionalSync ";"
[ ] TokenName               ->                  // Letters, numbers, underscores, hyphens only
[ ] ErrorMesssage           ->                  // Any random sequence of characters contained between two double-quotes
[ ] OptionalSync            ->              TokenName
[ ]                         |               ""
[ ] DefaultSpecs            ->              DefaultError
[ ]                         |               ""
[ ] DefaultError            ->              "$Default" ErrorMessage OptionalSync ";"
```

## Building the LL(1) Parsing Table

### Terminal tokens:

```
- normalHeader      -> nh
- errorHeader       -> eh
- defaultHeader     -> dh
- ;                 -> ;
- tokenName         -> n
- errorMessage      -> em
- regex             -> r
- default           -> d
```

### First Sets:

```
First(nh)               = { nh }
First(eh)               = { eh }
First(dh)               = { dh }
First(;)                = { ; }
First(n)                = { n }
First(em)               = { em }
First(r)                = { r }
First(d)                = { d }
First(DefaultError)     = { d }
First(DefaultSpecs)     = First(DefaultError) union { "" } = { d, ""}
First(OptionalSync)     = First(n) union { "" } = { n, "" }
First(ErrorSpec)        = { n }
First(ErrorSpecs)       = First(ErrorSpec) union { "" } = { n, "" }
First(NormalSpec)       = { n }
First(NormalSpecs)      = First(NormalSpec) union { "" } = { n, "" }
First(DefaultTokens)    = { dh, "" }
First(ErrorTokens)      = { eh, "" }
First(NormalTokens)     = { nh }
First(Program)          = First(NormalTokens) = { nh }
```

### Follow Sets Analysis of Grammar:

```
Let F(t) = Follow(t) and FirstE(t) = First(t) - { "" }

Program                 ->              NormalTokens ErrorTokens DefaultTokens
    F(Program) subset F(DefaultTokens)
    { $ } subset F(Program)
    F(Program) subset F(ErrorTokens)
    F(Program) subset F(NormalTokens)
    FirstE(ErrorTokens) subset F(NormalTokens)
    FirstE(DefaultTokens) subset F(NormalTokens)    // because ErrorTokens can disappear
    FirstE(DefaultTokens) subset F(ErrorTokens)
NormalTokens            ->              "%%_normal_%%" NormalSpecs
    F(NormalTokens) subset F(NormalSpecs)
    F(NormalTokens) subset F(nh)
    FirstE(NormalSpecs) subset F(nh)
ErrorTokens             ->              "%%_error_%%" ErrorSpecs
                        |               ""  
    F(ErrorTokens) subset F(ErrorSpecs)
    F(ErrorTokens) subset F(eh)
    FirstE(ErrorSpecs) subset F(eh)
DefaultTokens           ->              "%%_default_%% DefaultSpecs  
                        |               ""
    F(DefaultTokens) subset F(DefaultSpecs)
    F(DefaultTokens) subset F(dh)
    FirstE(DefaultSpecs) subset F(dh)
NormalSpecs             ->              NormalSpec NormalSpecs
                        |               ""
    F(NormalSpecs) = F(NormalSpecs)
    F(NormalSpecs) subset F(NormalSpec)
    FirstE(NormalSpecs) subset F(NormalSpec)
NormalSpec              ->              TokenName Regex ";"
    F(NormalSpec) subset F(;)
    { ; } subset F(Regex)
    { r } subset F(n)
ErrorSpecs              ->              ErrorSpec ErrorSpecs
                        |               ""
    F(ErrorSpecs) = F(ErrorSpecs)
    F(ErrorSpecs) subset F(ErrorSpec)
    FirstE(ErrorSpecs) subset F(ErrorSpec)
ErrorSpec               ->              TokenName ErrorMessage OptionalSync ";"
    F(ErrorSpec) subset F(;)
    { ; } subset F(OptionalSync)
    FirstE(OptionalSync) subset F(ErrorMessage)
    { em } subset F(TokenName)
OptionalSync            ->              TokenName
                        |               ""
    F(OptionalSync) subset F(TokenName)
DefaultSpecs            ->              DefaultError
                        |               ""
    F(DefaultSpecs) subset F(DefaultError)
DefaultError            ->              "$Default" ErrorMessage OptionalSync ";"
    F(DefaultError) subset F(;)
    { ; } subset F(OptionalSync)
    FirstE(OptionalSync) subset F(ErrorMessage)
    { em } subset F(d)
```

### Follow Sets:

```
F(DefaultError) contains F(DefaultSpecs)                    = { $ }
F(DefaultSpecs) contains F(DefaultTokens)                   = { $ } 
F(DefaultTokens) contains F(Program)                        = { $ }
F(Program)                                                  = { $ }
F(OptionalSync) contains { ; }                              = { ; }
F(ErrorSpec) contains F(ErrorSpecs)                         = { dh, $ }
                      FirstE(ErrorSpecs)       union { n }  = { dh, $, n }
F(ErrorSpecs) contains F(ErrorTokens)                       = { dh, $ }
F(ErrorTokens) contains FirstE(DefaultTokens)               = { dh }
                        F(Program)             union { $ }  = { dh, $ }
F(NormalSpec) contains F(NormalSpecs),                      = { eh, $ }
                        FirstE(NormalSpecs)    union { n }  = { eh, $, n }             
F(NormalSpecs) contains F(NormalTokens)                     = { eh, dh, $ }
F(NormalTokens) contains FirstE(ErrorTokens)                = { eh }
                         FirstE(DefaultTokens) union { dh } = { eh, dh }
                         F(Program)            union { $ }  = { eh, dh, $ }
```


### LL1 Parsing Table:

|                   |   $               |  n                        |     ;      |   d                       |    dh            |   eh          |   nh
|-------            |------             |-------                    |--------    |----------                 | --------         | -----------   | -----
| Program           |                   |                           |            |                           |                  |               | NormalTokens, ErrorTokens, DefaultTokens
| NormalTokens      |                   |                           |            |                           |                  |               | nh NormalSpecs
| ErrorTokens       | ""                |                           |            |                           |  ""              | eh ErrorSpecs |
| DefaultTokens     | ""                |                           |            |                           | dh DefaultSpecs  |
| NormalSpecs       | ""                | NormalSpec, NormalSpecs   |            |                           |  ""              |  ""
| NormalSpec        |                   | n, r, ";"                 |
| ErrorSpecs        | ""                | ErrorSpec, ErrorSpecs     |            |                           |  ""
| ErrorSpec         |                   | n,em,OptionalSync,";"     |
| DefaultSpecs      | ""                |                           |            | DefaultError
| DefaultError      |                   |                           |            | d,em,OptionalSync,";"
| OptionalSync      |                   |  n                        |     ""