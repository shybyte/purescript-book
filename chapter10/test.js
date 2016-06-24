// 10.9 Representing Constrained Types - 2.Exercise
// Try using the functions defined in the purescript-arrays package, calling from JavaScript

const array = require('Data.Array');

console.log(array.range(5)(10));
console.log(array.filter(x => x>5)([1,2,5,6,10]));
