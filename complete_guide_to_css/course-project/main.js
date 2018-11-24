// Modal
var backdrop = document.querySelector('.backdrop');
var modal = document.querySelector('.modal');
var selectPlanButtons = document.querySelectorAll('.plan button');

for (var i = 0; i < selectPlanButtons.length; ++i) {
  selectPlanButtons[i].addEventListener('click', openModel);
}

var modalCancelButton = modal.querySelector('.modal__actions .modal__action--negative');

modalCancelButton.addEventListener('click', closeModal);
backdrop.addEventListener('click', closeModal);

function openModel() {
  openBackdrop();
  modal.style.display = 'block';
  setTimeout(function () {
    modal.classList.add('open');
  }, 10);
}

function closeModal() {
  modal.classList.remove('open');
  setTimeout(function () {
    modal.style.display = 'none';
  }, 200);
  closeBackdrop();
}