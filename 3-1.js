const num = 368078;

// 17  16  15  14  13
// 18   5   4   3  12
// 19   6   1   2  11
// 20   7   8   9  10
// 21  22  23---> ...

// [1] (1 el) -> 1st square
// [2..9] (8 el) -> 2nd square
// [10..25] (16 el) -> 3rd square
// [26..49] (24 el) -> 4th square
// [50..81] (32 el) -> 5th square
// rightmost element -> corner element

// 1024 -> 31 steps

function side(square) {
  return (square - 1) * 2 + 1;
}

function area(square) {
  return side(square) ** 2;
}

function numberToSquare(num) {
  let square = 1;
  while (area(square) < num) {
    square++;
  }
  return square;
}

function corners(square) {
  const stride = side(square) - 1;
  const last = area(square);
  return [last - stride * 3, last - stride * 2, last - stride, last];
}

function distanceFromClosestCorner(num, corners) {
  return Math.min(...corners.map(corner => distanceFrom(num, corner)));
}

function distanceFrom(a, b) {
  return Math.abs(a - b);
}

function distanceFromCenter(square) {
  return (side(square) - 1) / 2;
}

function distance(num) {
  const square = numberToSquare(num);
  const cs = corners(square);
  const s = side(square);
  return distanceFromCenter(square) + (s - 1) / 2 - distanceFromClosestCorner(num, cs);
}

// console.log(
//   numberToSquare(1),
//   numberToSquare(49),
//   numberToSquare(50),
//   numberToSquare(65),
//   numberToSquare(81),
//   numberToSquare(82)
// );

// console.log(distanceFromClosestCorner(20, corners(numberToSquare(20))));

console.log(distance(num));

module.exports = {
  num,
  numberToSquare,
  area,
  side
};
