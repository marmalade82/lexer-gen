const path = require('path');

module.exports = {
  entry: './purescript-build.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'lexer-gen.js'
  }
};