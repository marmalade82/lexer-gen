export function lex(input) {
let str = input;
const tokens = [];
const errors = [];
const line = 0;
const column = 0;
while(inputRemains(str)){
let maxMunch = doMaxMunch(str, line, column);
str = str.slice(maxMunch.lexeme.length);
publish(maxMunch, tokens, errors)
line = newLine(maxMunch, line);
column = newColumn(maxMunch, column);
}
return {
errors: errors,
tokens: tokens
};
}
export default lex;