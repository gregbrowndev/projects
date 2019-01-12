const bootstrap = require('bootstrap');
window.jQuery = require('jquery');
window.$ = global.jQuery;

module.exports = {
  greeting
};

function greeting (name) {
  return `Hello ${name}!`;
}

