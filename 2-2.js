const spreadsheet = require("./2-1");

const checksums = spreadsheet
  .map(row => {
    return row.reduce(
      ({ min, max }, cell) => ({ min: Math.min(cell, min), max: Math.max(cell, max) }),
      { min: Infinity, max: 0 }
    );
  })
  .map(({ min, max }) => max - min);

const checksum = checksums.reduce((acc, checksum) => acc + checksum);

console.log(checksum);

// TODO: solution lost
