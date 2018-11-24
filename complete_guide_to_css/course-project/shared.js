// Side Navigation
var backdrop = document.querySelector('.backdrop');
var toggleButton = document.querySelector('.toggle-button');
var sideNav = document.querySelector('.mobile-nav');

toggleButton.addEventListener('click', openSideNav);
backdrop.addEventListener('click', closeSideNav);

function openSideNav() {
  openBackdrop();
  sideNav.classList.add('open');
}

function closeSideNav() {
  sideNav.classList.remove('open');
  closeBackdrop();
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