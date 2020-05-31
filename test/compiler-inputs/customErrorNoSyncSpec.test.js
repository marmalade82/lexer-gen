import lex from "./customErrorNoSyncSpec";

describe("Lexing input with a custom error sync on ' '", () => {
  test("empty input succeeds", () => {
    const input = "";
    const result = lex(input);

    expect(result.tokens.length).toEqual(0);
    expect(result.errors.length).toEqual(0);
  });

  test("errors have custom error message", () => {
    const MyInput = `I`;
    const result = lex(MyInput);
    expect(result.tokens.length).toEqual(0);
    expect(result.errors.length).toEqual(1);
    expect(result.errors[0].includes("Error 1")).toEqual(true);
  });
});
