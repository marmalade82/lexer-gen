import Lexer from "./emptySpec";

const MyInput = 
`I am a hippopatamus `;

describe("Lexing input with an empty lexer", () => {
    test("empty input succeeds", () => {
        const input = "";
        const result = Lexer.lex(input);

        expect(result.tokens.length).toEqual(0);
        expect(result.errors.length).toEqual(0);
    })

    test("existing input fails", () => {
        const result = Lexer.lex(MyInput)
        expect(result.tokens.length).toEqual(0);
        expect(result.errors.length).toEqual(1)
    })
})