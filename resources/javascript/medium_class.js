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
        stuff: function (a, b, c) {
            console.log(b, c);
        },
        action: function () {
            return { 'content': this.contactView };
        }
    });
});
