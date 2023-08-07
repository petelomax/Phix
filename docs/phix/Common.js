function CopyToClipboard(clipname) {
    if (window.clipboardData) {
        window.clipboardData.setData('text', document.getElementById(clipname).innerText);
    }
}
function ExternalLink(linkname) {
    if (window.clipboardData) {
        let elem = document.getElementById(linkname),
            link = elem.title;
        if (!link) { link = elem.innerText; elem.title = link; }
        window.clipboardData.setData('text', link);
        elem.innerText = "External Link Copied to Clipboard"
    }
}
function SourceLink(linktext) {
    if (window.clipboardData) {
        window.clipboardData.setData('text', 'http://msdn.microsoft.com/en-us/library/'+linktext);
        document.getElementById("source").innerText = "External Link Copied to Clipboard"
    }
}
function setleft(img) {
    var elem = document.getElementById("leftNav");
    if (elem.style.display == "none") {
        img.style.left = "13px";
    } else {
        img.style.left = "292px";
    }
}
function setStart(img) {
    img.style.position = "absolute";
    img.style.top = "80px";
    img.style.zIndex = 100;
    setleft(img);
}
function setStart2(img) {
    img.style.position = "absolute";
    img.style.top = "360px";
    img.style.zIndex = 100;
    setleft(img);
}
function setStartT(img) {
    img.style.top = -10;
    img.style.zIndex = 100;
}
function changeImage(img) {
    var elem = document.getElementById("leftNav");
    if (elem.style.display == "none") {
        elem.style.display = "";
        img.src = img.src.replace("widen", "close");
//      img.style.left = "292px";
    } else {
        elem.style.display = "none";
        img.src = img.src.replace("close", "widen");
//      img.style.left = "13px";
    }
    setleft(img);
}
function changeImageT(img) {
    var elem = document.getElementById("Technicalia");
    if (elem.style.display == "none") {
        elem.style.display = "";
        img.src = img.src.replace("open", "close");
    } else {
        elem.style.display = "none";
        img.src = img.src.replace("close", "open");
    }
}
function opentech() {
    let elem = document.getElementById("Technicalia");
    if (elem.style.display == "none") {
        elem.style.display = "";
        let img = document.getElementById("Timg");
        img.src = img.src.replace("open", "close");
    }
}
/* Used by the Hide/Show button beside syntax diagrams, to toggle the */
function hideorshow(btn, obj) {
    var x = document.getElementById(obj),
        b = document.getElementById(btn);
    if (x.style.display != 'none') {
        x.style.display = 'none';
        b.innerHTML = 'show';
    } else {
        x.style.display = '';
        b.innerHTML = 'hide';
    }
    return false;
}

