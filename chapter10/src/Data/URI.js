"use strict";
// module Data.URI


// 10.10 Using JavaScript Code From PureScript
exports.encodeURIComponent = encodeURIComponent;


// 10.11 Wrapping JavaScript Values
exports.unsafeHead = function(arr) {
  if (arr.length) {
    return arr[0];
  } else {
    throw new Error('unsafeHead: empty array');
  }
};



// 10.12 Defining Foreign Types
exports.head = function(arr) {
  return arr[0];
};

exports.isUndefined = function(value) {
  return value === undefined;
};
