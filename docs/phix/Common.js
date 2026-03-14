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
    /* setStart for main phix.htm page */
    img.style.position = "absolute";
    img.style.top = "360px";
    img.style.zIndex = 100;
    setleft(img);
}
//function setStartT(img) {
//  img.style.top = +10;
//  img.style.zIndex = 100;
//}
/* Used by the Hide/Show button beside syntax diagrams, to toggle them */
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
function initAccordion() {
    function handlePanelClick(event) {
        let expandel = accordionElem.querySelector(".active");
        if (expandel && expandel !== event.currentTarget) {
            expandel.classList.remove("active");
        }
        event.currentTarget.classList.toggle("active");
    }
    let accordionElem = document.getElementById("accordion");
    if (accordionElem) {
        let allPanels = accordionElem.querySelectorAll(".panel");
        for (var i = 0, len = allPanels.length; i < len; i++) {
            allPanels[i].addEventListener("click", handlePanelClick);
        }
    }
}
function changeImage(img) {
    /* toggle leftNav display */
    var elem = document.getElementById("leftNav");
    if (elem.style.display == "none") {
        elem.style.display = "";
        img.src = img.src.replace("widen", "close");
    } else {
        elem.style.display = "none";
        img.src = img.src.replace("close", "widen");
    }
    setleft(img);
}

const TOPEN = "images/sprites/tech.open.png",
     TCLOSE = "images/sprites/tech.close.png",
    TEXPSHR = "Expand/Shrink";

function changeImageT(bOpenOnly) {
    /* toggle technicalia display */
    let elem = document.getElementById("Technicalia");
    if (elem) {
        let timg = document.getElementById("Timg"),
           table = elem.parentNode;
        if (elem.className === "hiddenRow") {
            elem.className = "";
            timg.src = TCLOSE;
            table.classList.remove("tableHideBottomBorder");
        } else if (bOpenOnly!=true) { // nb *NOT* (!bOpenOnly)
            elem.className = "hiddenRow";
            timg.src = TOPEN;
            table.classList.add("tableHideBottomBorder");
        }
    }
}
function opentech() {
    changeImageT(true);
}
function opentechsometimes() {
    let url = window.location.href,
        frag = url.split('#')[1];
    if (frag === "bresenham" ||
        frag === "fakescroll" ||
        frag === "fsum" ||
        frag === "psum" ||
        frag === "GTK4" ||
        frag === "All" ||
        frag === "Alle" ||
        frag === "hFake" ||
        frag === "hImages") {
      changeImageT(true);
    }
}
function loaded() {
    let timg = document.getElementById("Timg");
    if (timg) {
        timg.onclick = changeImageT;
        timg.setAttribute("title", TEXPSHR);
        timg.setAttribute("aria-label", TEXPSHR);
        timg.src = TOPEN;
        // force reflow
        timg.style.bottom = "1px";
        timg.offsetHeight; // trigger layout
        timg.style.bottom = "0";
    }
}
document.addEventListener("DOMContentLoaded", loaded);

