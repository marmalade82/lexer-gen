# README #

A lexer generator library that takes a lexer specification and returns a generated lexer program in JavaScript

Built with PureScript and Prettier

## Features

- Regex-based lexing
- Specification of error lexemes, with error recovery
- Custom error messages on error lexemes
- Basic error detection
    - Checking that token specs are declared before use
    - Checking that token specs are not used ambiguously
- Library uses LL1 parsing internally

## Basic Usage ##

An example lexer specification is as follows:

```
%%_normal_%%
let (let);
const (const);
var (var);
id ([\w_]+);
terminator (;);

%%_error_%%
var "Using `var` is forbidden. Use `let` or `const` instead" terminator;

%%_default_%%
$Default "Unrecognized token";
```

## Major Dependencies

- PureScript
- Prettier

## Build/Deploy Instructions

Pre-requisites:

- Node >= 10.18.1
- NPM >= 6.13.4

To run the tests, use `spago test` from the root directory.
To generate the production build, run `npm run build` from the `/packages` directory, and a bundle will be generated in `/packages/dist`

## Contact

Questions/comments can be sent to <hchen7913@gmail.com>