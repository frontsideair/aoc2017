const banks = "14 0 15 12 11 11 3 5 1 6 8 4 9 1 8 4".split(" ").map(n => Number(n));
// const banks = [0, 2, 7, 0]
let cycles = 0;
let configs = new Set();

function incWrap(n, len) {
  return (n + 1) % len;
}

while (!configs.has(banks.join(" "))) {
  configs.add(banks.join(" "));
  cycles++;
  let [bank, bankIndex] = banks.reduce(
    ([max, maxIndex], bank, bankIndex) => (max >= bank ? [max, maxIndex] : [bank, bankIndex]),
    [0, -1]
  );
  banks[bankIndex] = 0;

  while (bank !== 0) {
    bankIndex = incWrap(bankIndex, banks.length);
    banks[bankIndex]++;
    bank--;
  }
}

console.log(cycles);
