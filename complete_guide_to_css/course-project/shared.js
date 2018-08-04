var backdrop = document.querySelector('.backdrop');
var modal = document.querySelector('.modal');
var selectPlanButtons = document.querySelectorAll('.plan button');

for (var i = 0; i < selectPlanButtons.length; ++i) {
    selectPlanButtons[i].addEventListener('click', function() {
        modal.style.display = 'block';
        backdrop.style.display = 'block';
    });
}

var modalCancelButton = modal.querySelector('.modal__actions .modal__action--negative');

modalCancelButton.addEventListener('click', closeModal);
backdrop.addEventListener('click', closeModal);

function closeModal(){
    modal.style.display = 'none';
    backdrop.style.display = 'none';
}