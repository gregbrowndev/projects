const bootstrap = require('bootstrap');
window.jQuery = require('jquery');
window.$ = global.jQuery;

const lightbox = require('ekko-lightbox');

// Lightbox Init
$(document).on('click', '[data-toggle="lightbox"]', function(event) {
  event.preventDefault();
  $(this).ekkoLightbox();
});

module.exports = {
  greeting
};

function greeting (name) {
  return `Hello ${name}!`;
}

