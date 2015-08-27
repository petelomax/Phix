function CopyToClipboard(clipname) {
    if (window.clipboardData) {
        window.clipboardData.setData('text', document.getElementById(clipname).innerText);
    }
}
function ExternalLink(linkname,linktext) {
    if (window.clipboardData) {
        window.clipboardData.setData('text', linktext);
        document.getElementById(linkname).innerText = "External Link Copied to Clipboard"
    }
}
function SourceLink(linktext) {
    if (window.clipboardData) {
        window.clipboardData.setData('text', 'http://msdn.microsoft.com/en-us/library/'+linktext);
        document.getElementById("source").innerText = "External Link Copied to Clipboard"
    }
}

function setStart(img) {
    img.style.position = "absolute";
    img.style.top = "80px";
    img.style.zIndex = 100;
}
function setStart2(img) {
    img.style.position = "absolute";
    img.style.top = "360px";
    img.style.zIndex = 100;
}
function setStartT(img) {
    img.style.top = -10;
    img.style.zIndex = 100;
}
function changeImage(img) {
    var elem = document.getElementById("leftNav");
    if (elem.style.display=="none")
    {
        elem.style.display = "block";
        img.src = img.src.replace("widen", "close");
        img.style.left = "292px";
    }
    else
    {
        elem.style.display = "none";
        img.src = img.src.replace("close", "widen");
        img.style.left = "13px";
    }
}
function changeImageT(img) {
    var elem = document.getElementById("Technicalia");
    if (elem.style.display=="none")
    {
        elem.style.display = "block";
        img.src = img.src.replace("open", "close");
    }
    else
    {
        elem.style.display = "none";
        img.src = img.src.replace("close", "open");
    }
}

