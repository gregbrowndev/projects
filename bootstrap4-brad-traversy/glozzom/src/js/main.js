const bootstrap = require('bootstrap');
const lightbox = require('ekko-lightbox');
const slickCarousel = require('slick-carousel');

// Add jQuery to globals
window.jQuery = require('jquery');
window.$ = global.jQuery;

// Lightbox Init
$(document).on('click', '[data-toggle="lightbox"]', function (event) {
  event.preventDefault();
  $(this).ekkoLightbox();
});

module.exports = {
  greeting
};

function greeting(name) {
  return `Hello ${name}!`;
}

