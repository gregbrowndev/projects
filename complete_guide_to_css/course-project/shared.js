// Side Navigation
var backdrop = document.querySelector('.backdrop');
var toggleButton = document.querySelector('.toggle-button');
var sideNav = document.querySelector('.mobile-nav');

toggleButton.addEventListener('click', function() {
    // sideNav.style.display = 'block';
    // backdrop.style.display = 'block';
    sideNav.classList.add('open');
    backdrop.classList.add('open');
});
backdrop.addEventListener('click', function(){
    // sideNav.style.display = 'none';
    // backdrop.style.display = 'none';
    sideNav.classList.remove('open');
    backdrop.classList.remove('open');
});
