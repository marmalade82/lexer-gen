import lex from "./defaultErrorNoSyncSpec";

describe("Lexing input with a default error sync on ' '", () => {
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
    expect(result.errors[0].includes("I am an error")).toEqual(true);
  });
});
