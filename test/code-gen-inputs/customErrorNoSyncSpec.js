export function lex(input) {
  let str = input;
  const tokens = [];
  const errors = [];
  let line = 0;
  let column = 0;
  while (inputRemains(str)) {
    let maxMunch = doMaxMunch(str, line, column);
    str = str.slice(maxMunch.lexeme.length);
    publish(maxMunch, tokens, errors);
    line = newLine(maxMunch, line);
    column = newColumn(maxMunch, column);
  }
  return {
    errors: errors,
    tokens: tokens,
  };
}
export default lex;

/*
 * This function calculates the new line position by looking at the lexeme
 * and counting the number of newlines in it
 * */
function newLine(munch, oldLine) {
  let count = 0;
  for (let i = 0; i < munch.lexeme.length; i++) {
    if (munch.lexeme[i] === "\n") {
      count++;
    }
    return oldLine + count;
  }
}
/*
 * This function calculates the new column position by looking at the lexeme
 * and finding the number of characters AFTER the last newline
 * */
function newColumn(munch, oldCol) {
  let index = munch.lexeme.length;
  let foundNewline = false;
  while (index > 0 && !foundNewline) {
    index--;
    if (munch.lexeme[index] === "\n") {
      foundNewline = true;
      break;
    }
  }
  if (foundNewline) {
    return munch.lexeme.length - index - 1;
  } else {
    return oldCol + munch.lexeme.length;
  }
}
function inputRemains(str) {
  return str.length > 0;
}
/*
 * This function discards characters from the input string until
 * the regex matches the syncing regex. Lexing should restart from
 * there
 * */
function discardUntil(str, syncMatcher) {
  let search = str;
  const discarded = [];
  while (!syncMatcher(search) && search.length > 0) {
    discarded.push(search[0]);
    search = search.slice(1);
  }
  return {
    synced: search,
    discarded: discarded.join(""),
  };
}

/*
 * This function runs through all the declared tokens and tries all of them to
 * find the one with maximum munch. If two or more have the same length, the one
 * that was declared latest in the lexer-gen file takes priority. Once the maximum
 * munch is identified, it returns an object containing the token type, the lexeme,
 * the column number, and the line number
 * */
function doMaxMunch(str, line, column) {
  let munch = Object.values(matchers).reduce(
    function match(acc, matcher) {
      const result = matcher(str);
      if (result !== null) {
        acc.type = result.type;
        acc.lexeme = result.lexeme;
      }
      return acc;
    },
    {
      lexeme: "",
      type: undefined,
      column: column,
      line: line,
    }
  );
  if (munch.type === undefined) {
    munch = Object.values(errors).reduce(
      function match(acc, error) {
        const result = error(str);
        if (result !== null) {
          acc.type = result.type;
          acc.lexeme = result.lexeme;
        }
        return acc;
      },
      {
        lexeme: "",
        type: undefined,
        column: column,
        line: line,
      }
    );
    if (munch.type === undefined) {
      throw new Error(
        "Lexing got stuck! No matchers or errors succeeded! Unexpected"
      );
    }
  }

  return munch;
}
/*
 * This function determines whether a given munch is an error munch or not
 * */
function isError(munch) {
  return lookupError(munch.type.toString()) !== null;
}
function publish(munch, tokens, errors) {
  if (isError(munch)) {
    publishError(munch, errors);
  } else {
    publishToken(munch, tokens);
  }
  function publishError(munch, errors) {
    errors.push(
      `line ${munch.line}, column ${munch.column}: ${lookupError(munch.type)}`
    );
  }
  function publishToken(munch, tokens) {
    tokens.push(munch);
  }
}
export const _default = "_default";
export const me = "me";
export const space = "space";
export const verb = "verb";
function makeMatcher(tokenName, regex) {
  return function matcher(input) {
    const result = input.match(regex);
    if (result && result.length > 0) {
      return {
        lexeme: result[0],
        type: tokenName,
      };
    } else {
      return null;
    }
  };
}
const matchers = {
  space: makeMatcher(space, new RegExp("^ ")),
};
function makeError(name, regex, sync) {
  const initialMatcher = makeMatcher(name, regex);
  return function matcher(input) {
    const initialResult = initialMatcher(input);
    const syncMatcher = matchers[sync];
    if (!syncMatcher || initialResult === null) {
      return initialResult;
    } else {
      const afterMatchInput = input.slice(initialResult.lexeme.length);
      const { discarded, synced } = discardUntil(afterMatchInput, syncMatcher);
      initialResult.originalLexeme = initialResult.lexeme;
      initialResult.lexeme = initialResult.lexeme + discarded;
      return initialResult;
    }
  };
}
function lookupError(type) {
  const lookup = {
    verb: "Error 2",
    me: "Error 1",
    _default: "No match for any token",
  };
  if (lookup[type] !== undefined) {
    return lookup[type];
  } else {
    return null;
  }
}
const errors = {
  _default: makeError(_default, new RegExp(".*"), undefined),
  me: makeError(me, new RegExp("^I"), undefined),
  verb: makeError(verb, new RegExp("^am"), undefined),
};
