"use strict";

/**
 * Tone detector helper. I would put that in a separate file
 * but exercism currently allows submitting only one file.
 * @type {Object}
 */
var ToneDetector = {
    /**
     * A set of detector functions for each tone type
     * @type {Object}
     */
    detectors: {
        silence: function(s) {
            return s === '';
        },
        question: function(s) {
            return s.length && s[s.length -1] === '?';
        },
        yell: function(s) {
            // if the string contains at least one letter and it is uppercased
            return s.toUpperCase() === s && s.toLowerCase() !== s;
        }
    },

    /**
     * Order of detector application
     * @type {Array}
     */
    detectorOrder: [
        'silence',
        'yell',     // cause a forceful question is still yelling
        'question'
    ],

    /**
     * For a given phrase returns its tone
     * @param  {String} phrase Phrase in normalized form (e.g. trimmed)
     * @return {String} tone
     */
    getTone: function(phrase) {
        var checkList = this.detectorOrder,
            detectors = this.detectors,
            result = 'other';

        checkList.every(function(toneType) {
            if(detectors[toneType](phrase)) {
                result = toneType;
                return false;
            }

            return true;
        });

        return result;
    }
};

var Bob = function() {};
/**
 * @class Bob Our friend
 */
Bob.prototype = {
    constructor: Bob,

    /**
     * Bob's very limited vocabulary
     * @static
     * @type {Object}
     */
    Replies: {
        question:   'Sure.',
        yell:       'Woah, chill out!',
        silence:    'Fine. Be that way!',
        other:      'Whatever.'
    },

    /**
     * Say something to Bob and see what he says
     * @param  {String} phrase Your phrase that is a string or converts to one
     * @return {String}        Bob's reply
     */
    hey: function(phrase) {
        phrase = phrase && phrase.toString().trim();

        var tone = ToneDetector.getTone(phrase);

        return this.Replies[tone];
    }
};

module.exports = Bob;
