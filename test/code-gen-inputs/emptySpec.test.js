import lex from "./emptySpec";

const MyInput = `I am a hippopatamus `;

describe("Lexing input with an empty lexer", () => {
  test("yup", () => {
    expect(true).toEqual(true);
  });
  test("empty input succeeds", () => {
    const input = "";
    const result = lex(input);

    expect(result.tokens.length).toEqual(0);
    expect(result.errors.length).toEqual(0);
  });

  test("existing input fails", () => {
    const result = lex(MyInput);
    expect(result.tokens.length).toEqual(0);
    expect(result.errors.length).toEqual(1);
  });
});
