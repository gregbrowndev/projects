// Modal
var backdrop = document.querySelector('.backdrop');
var modal = document.querySelector('.modal');
var selectPlanButtons = document.querySelectorAll('.plan button');

for (var i = 0; i < selectPlanButtons.length; ++i) {
    selectPlanButtons[i].addEventListener('click', function() {
        // modal.style.display = 'block';
        // backdrop.style.display = 'block';
        // modal.className = 'open'; // This would override all the classes on modal
        modal.classList.add('open');
        backdrop.classList.add('open');
    });
}

var modalCancelButton = modal.querySelector('.modal__actions .modal__action--negative');

modalCancelButton.addEventListener('click', closeModal);
backdrop.addEventListener('click', closeModal);

function closeModal(){
    // modal.style.display = 'none';
    // backdrop.style.display = 'none';
    modal.classList.remove('open');
    backdrop.classList.remove('open');
}

// Side Navigation
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
