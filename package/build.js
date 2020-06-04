var CP = require("child_process");
var path = require("path");
var cwd = path.resolve(__dirname, "..")

;(() => {
    CP.spawn("spago.cmd", ["bundle-module", "--main", "Compiler", "--to", path.resolve(__dirname, "purescript-build.js")], {
        cwd: cwd,
    })
})();