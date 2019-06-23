// Add jQuery to globals
window.jQuery = require('jquery');
window.$ = global.jQuery;

const bootstrap = require('bootstrap');
const lightbox = require('ekko-lightbox');
const slickCarousel = require('slick-carousel');

// Lightbox Init
$(document).on('click', '[data-toggle="lightbox"]', function (event) {
  event.preventDefault();
  $(this).ekkoLightbox();
});

// Initialise slick slider
// NOTE - this is used in about.html and likely should be inline in the page?
$('.slider').slick({
  infinite: true,
  slidesToScroll: 1,
  slidesToShow: 1
});

module.exports = {
  greeting
};

function greeting(name) {
  return `Hello ${name}!`;
}

