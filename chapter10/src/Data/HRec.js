"use strict";

// module Data.HRec

exports.empty = {};

exports.insert = function(key, value, rec) {
    var copy = {};
    for (var k in rec) {
        if (rec.hasOwnProperty(k)) {
            copy[k] = rec[k];
        }
    }
    copy[key] = value;
    return copy;
};

exports.mapHRec = function(f, rec) {
    var mapped = {};
    for (var k in rec) {
        if (rec.hasOwnProperty(k)) {
            mapped[k] = f(rec[k]);
        }
    }
    return mapped;
};

exports.foldHRec = function(f, r, rec) {
    var acc = r;
    for (var k in rec) {
        if (rec.hasOwnProperty(k)) {
            acc = f(acc, k, rec[k]);
        }
    }
    return acc;
};

exports.union = function(rec1, rec2) {
    var copy = {};
    for (var k in rec1) {
        if (rec1.hasOwnProperty(k)) {
            copy[k] = rec1[k];
        }
    }
    for (var k in rec2) {
        if (rec2.hasOwnProperty(k)) {
            copy[k] = rec2[k];
        }
    }
    return copy;
};
