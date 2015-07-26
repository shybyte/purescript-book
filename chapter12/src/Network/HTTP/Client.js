"use strict";

// module Network.HTTP.Client

exports.getImpl = function(opts, more, done) {
    return function() {
        require('http').request(opts, function(res) {
            res.setEncoding('utf8');
            res.on('data', function(s) {
                more(s)();
            });
            res.on('end', function() {
                done();
            });
        }).end();
    };
};
