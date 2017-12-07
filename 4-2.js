const passphrases = require("./4-2");

function isValid(passphrase) {
  const words = passphrase.split(" ").map(word =>
    word
      .split("")
      .sort()
      .join("")
  );
  return words.length === new Set(words).size;
}

const count = passphrases.reduce((acc, passphrase) => (isValid(passphrase) ? acc + 1 : acc), 0);

console.log(count);
