import lex from "./oneTokenSpec";

describe("Lexing input with a one-token lexer", () => {
  test("empty input succeeds", () => {
    const input = "";
    const result = lex(input);

    expect(result.tokens.length).toEqual(0);
    expect(result.errors.length).toEqual(0);
  });

  test("existing input fails after recognizing one token", () => {
    const MyInput = `I am a hippopatamus `;
    const result = lex(MyInput);
    expect(result.tokens.length).toEqual(1);
    expect(result.errors.length).toEqual(1);
  });

  test("existing input fails after recognizing three tokens", () => {
    const MyInput = `III am a hippopatamus `;
    const result = lex(MyInput);
    expect(result.tokens.length).toEqual(3);
    expect(result.errors.length).toEqual(1);
  });
});
