const infile = document.querySelector('#fileElem'),
      filename = document.querySelector('#filename'),
      dropArea = document.querySelector('#drop-area'),
      input = document.querySelector('#input');

function previewFile(file) {
    if (file) {
        filename.innerHTML = file.name;
        let reader = new FileReader();
        reader.readAsText(file);
        reader.onloadend = function() {
            //DEV syntax colour...
            input.value = reader.result;
        }
    }
}

function changeHandler(e) {
    previewFile(e.target.files[0]);
}
infile.addEventListener('change', changeHandler);

function clickHandler() {
    infile.value = null;
}
infile.addEventListener('click', clickHandler);

function preventDefaults(e) {
    e.preventDefault(); // otherwise browser opens the file.
    e.stopPropagation();
}
dropArea.addEventListener('dragenter', preventDefaults, false);
dropArea.addEventListener('dragleave', preventDefaults, false);
dropArea.addEventListener('dragover', preventDefaults, false);

function handleDrop(e) {
    previewFile(e.dataTransfer.files[0]);
    preventDefaults(e);
}
dropArea.addEventListener('drop', handleDrop, false);

