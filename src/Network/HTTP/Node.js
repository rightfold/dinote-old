'use strict';

var Data_Maybe = require('../Data.Maybe');

exports.exhaustStream = function(stream) {
    return function(success, error) {
        var clean = function() {
            stream.removeListener('end', onEnd);
            stream.removeListener('error', onError);
            stream.removeListener('data', onData);
        };
        var result = Buffer.alloc(0);
        var onEnd = function() {
            clean();
            success(result);
        };
        var onError = function(err) {
            clean();
            error(err);
        };
        var onData = function(buffer) {
            result = Buffer.concat([result, buffer]);
        };
        stream.on('end', onEnd);
        stream.on('error', onError);
        stream.on('data', onData);
    };
};
