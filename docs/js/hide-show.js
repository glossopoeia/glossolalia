
function tutorial_button(lang) {
    var btnId = 'btn-' + lang;
    var codeId = lang + '-ish';

    var btns = document.getElementsByTagName("button");
    for (var i = 0; i < btns.length; i++) {
        btns[i].classList.remove("button-current");
    }
    var blocks = document.getElementsByTagName("code");
    for (var i = 0; i < blocks.length; i++) {
        if (blocks[i].className == "block") {
            blocks[i].style.display = "none";
        }
    }

    document.getElementById(btnId).classList.add("button-current");
    document.getElementById(codeId).style.display = "block";
}