'use strict';

var stormpath = require('stormpath');

exports.newAPIKey = function(id) {
    return function(secret) {
        return new stormpath.ApiKey(id, secret);
    };
};

exports.newClient = function(apiKey) {
    return function(onSuccess, onError) {
        var client;
        try {
            client = new stormpath.Client({apiKey: apiKey});
        } catch (e) {
            onError(e);
            return;
        }
        onSuccess(client);
    };
};

exports.getApplication = function(client) {
    return function(href) {
        return function(onSuccess, onError) {
            client.getApplication(href, function(err, application) {
                if (err !== null) {
                    onError(err);
                    return;
                }
                onSuccess(application);
            });
        };
    };
};
