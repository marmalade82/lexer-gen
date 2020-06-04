// This file exports the lexer as a function that is combined with Prettier.

var prettier = require("prettier/standalone");
var babel = require("prettier/parser-babel")
var Compiler = require("./purescript-build.js");

function compile(program) {
    var output = Compiler.exec(program)
    output.lexer = prettier.format(output.lexer, {
        parser: "babel",
        plugins: [babel]
    })
    return output;
}

exports.compile = compile;