# Design #

The goal of this codebase is to take a specification file and generate the code for simple lexical analysis. The specification should have the following features to control the behavior of the generated lexer:

- [ ] Lexing should follow the principle of maximum munch
- [ ] Lexing rules should have assignable priorities for breaking ties if necessary
- [ ] The generated tokens should allow queries concerning their line and column numbers
- [ ] The ability to specify tokens that will be discarded from the stream automatically (convenience feature so parsers don't need to do it).
- [ ] The ability to specify custom Error tokens with custom error messages
- [ ] The ability to specify synchronization tokens that mark the end of custom error tokens (which are often generated from very broad rules), so that normal lexing can continue on the rest of the input.

## Grammar

The following grammar for the codebase was designed from the beginning to be suitable for LL(1) parsing:

```
[ ] Program                 ->              NormalTokens ErrorTokens DefaultTokens
[ ] NormalTokens            ->              "%%_normal_%%" NormalSpecs
[ ] ErrorTokens             ->              "%%_error_%%" ErrorSpecs
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