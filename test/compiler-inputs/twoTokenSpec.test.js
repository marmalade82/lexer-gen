import lex from "./twoTokenSpec";

describe("Lexing input with a two-token lexer", () => {
  test("empty input succeeds", () => {
    const input = "";
    const result = lex(input);

    expect(result.tokens.length).toEqual(0);
    expect(result.errors.length).toEqual(0);
  });

  test("existing input fails after recognizing two tokens", () => {
    const MyInput = `Iam a hippopatamus `;
    const result = lex(MyInput);
    expect(result.tokens.length).toEqual(2);
    expect(result.errors.length).toEqual(1);
  });

  test("existing input fails after recognizing four tokens", () => {
    const MyInput = `IamamI a hippopatamus `;
    const result = lex(MyInput);
    expect(result.tokens.length).toEqual(4);
    expect(result.errors.length).toEqual(1);
  });

  test("existing input succeeds after recognizing everything", () => {
    const MyInput = `IIIam`;
    const result = lex(MyInput);
    expect(result.tokens.length).toEqual(4);
    expect(result.errors.length).toEqual(0);
  });
});
