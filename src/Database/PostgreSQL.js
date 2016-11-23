'use strict';

var pg = require('pg');

exports.newPool = function(config) {
    return function(onSuccess, onError) {
        onSuccess(new pg.Pool(config));
    };
};

exports.withConnection = function(pool) {
    return function(body) {
        return function(onSuccess, onError) {
            pool.connect(function(err, client, done) {
                if (err !== null) {
                    onError(err);
                    return;
                }
                body(client)(function(r) {
                    done();
                    onSuccess(r);
                }, function(e) {
                    done();
                    onError(e);
                });
            });
        };
    };
};
