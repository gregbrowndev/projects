// Side Navigation
var backdrop = document.querySelector('.backdrop');
var toggleButton = document.querySelector('.toggle-button');
var sideNav = document.querySelector('.mobile-nav');
var ctaButton = document.querySelector('.main-nav__item--cta');

toggleButton.addEventListener('click', openSideNav);
backdrop.addEventListener('click', closeSideNav);

ctaButton.addEventListener('animationstart', function(event) {
  console.log('Animation started', event);
});
ctaButton.addEventListener('animationend', function(event) {
  console.log('Animation ended', event);
});
ctaButton.addEventListener('animationiteration', function(event) {
  console.log('Animation iteration', event);
});

function openSideNav() {
  openBackdrop();
  sideNav.style.display = 'block';
  setTimeout(function () {
    sideNav.classList.add('open');
  }, 10);
}

function closeSideNav() {
  closeBackdrop();
  sideNav.classList.remove('open');
  setTimeout(function () {
    sideNav.style.display = 'none';
  }, 200);
}

function openBackdrop() {
  backdrop.style.display = 'block';
  setTimeout(function () {
    backdrop.classList.add('open');
  }, 10);
}

function closeBackdrop() {
  backdrop.classList.remove('open');
  setTimeout(function () {
    backdrop.style.display = 'none';
  }, 200);
}