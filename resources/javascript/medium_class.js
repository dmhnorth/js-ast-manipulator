define([
    'dojo/_base/declare',
    'indium/control/_BaseController',
    'f',
    'fsp/view/ContactView'
], function (declare, _BaseController, thing, ContactView) {
    return declare('fsp.control.ContactController', [_BaseController], {
        contactView: null,
        constructor: function () {
            this.contactView = new ContactView();
            this.stuff();
        },
        /*
         * @returns {Object}
         * @param {String} fish
         */
        stuff: function (a, b, c) {
            console.log(b, c);
        },
        dave: function () {
            var s;
            var f;
            return 'g';
        },
        action: function () {
            return { 'content': this.contactView };
        }
    });
});
