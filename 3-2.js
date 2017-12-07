const { num, numberToSquare, area, side } = require("./3-1");

// 147  142  133  122   59
// 304    5    4    2   57
// 330   10    1    1   54
// 351   11   23   25   26
// 362  747  806--->   ...

const memo = [
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
];

function sumOfSurroundings(memo, y, x) {
  return (
    memo[y - 1][x + 1] +
    memo[y - 1][x] +
    memo[y - 1][x - 1] +
    memo[y][x - 1] +
    memo[y + 1][x - 1] +
    memo[y + 1][x] +
    memo[y + 1][x + 1] +
    memo[y][x + 1]
  );
}

function run(n) {
  let y = 5;
  let x = 6;
  let stride = 1;

  for (let i = 0; i < n; i++) {
    // go up
    for (let i = 0; i < stride; i++) {
      y = y - 1;
      memo[y][x] = sumOfSurroundings(memo, y, x);
    }
    // increase stride
    stride = stride + 1;
    // go left
    for (let i = 0; i < stride; i++) {
      x = x - 1;
      memo[y][x] = sumOfSurroundings(memo, y, x);
    }
    // go down
    for (let i = 0; i < stride; i++) {
      y = y + 1;
      memo[y][x] = sumOfSurroundings(memo, y, x);
    }
    // increase stride
    stride = stride + 1;
    // go right
    for (let i = 0; i < stride; i++) {
      x = x + 1;
      memo[y][x] = sumOfSurroundings(memo, y, x);
    }
  }
}

run(4);
console.log(memo);
