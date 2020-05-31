import lex from "./defaultErrorSyncSpec";

describe("Lexing input with a default error sync on ' '", () => {
  test("empty input succeeds", () => {
    const input = "";
    const result = lex(input);

    expect(result.tokens.length).toEqual(0);
    expect(result.errors.length).toEqual(0);
  });

  test("errors sync on spaces", () => {
    const MyInput = `I, am, a`;
    const result = lex(MyInput);
    expect(result.tokens.length).toEqual(0);
    expect(result.errors.length).toEqual(1); //default error matches EVERYTHING, regardless of syncing
  });

  test("errors have custom error message", () => {
    const MyInput = `I`;
    const result = lex(MyInput);
    expect(result.tokens.length).toEqual(0);
    expect(result.errors.length).toEqual(1);
    expect(result.errors[0].includes("I am an error")).toEqual(true);
  });
});
