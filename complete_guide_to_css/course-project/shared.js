// Side Navigation
var backdrop = document.querySelector('.backdrop');
var toggleButton = document.querySelector('.toggle-button');
var sideNav = document.querySelector('.mobile-nav');

toggleButton.addEventListener('click', openSideNav);
backdrop.addEventListener('click', closeSideNav);

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