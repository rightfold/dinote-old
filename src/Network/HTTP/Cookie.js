'use strict';

var cookie = require('cookie');

exports.parse = function(s) {
    return cookie.parse(s);
};

exports.serialize = function(k) {
    return function(v) {
        return cookie.serialize(k, v);
    };
};
