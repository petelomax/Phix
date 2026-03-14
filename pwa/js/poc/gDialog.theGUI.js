"use strict";

let $zCounter = 100;
let $windows = [];
let NULL = 0;

function $get_focus() {
    for (let i=0; i<$windows.length; i += 1) {
        let w = $windows[i];
        if (w.classList.contains("active")) { return w; }
    }
    return NULL;
}
function $focusWindow(win) {
    let w = $get_focus();
    if (w) { w.classList.remove("active"); }
    win.classList.add("active");
    win.style.zIndex = ++$zCounter;
}

//gDialog(gdx child, [parent=NULL,][string [title="",] attributes="",[dword_seq args={},]] bool bEsc=true) 
//function gDialog(parent, title, options) {
function gDialog(parent, title, x, y, w, h, draw_cross=true) {

  function element(tag, classname) {
    let elem = document.createElement(tag);
    if (classname) { elem.className = classname; }
    return elem;
  }
  function svgns_element(tag) {
    return document.createElementNS("http://www.w3.org/2000/svg",tag);
  }

  /* Detect host OS */
  function detectPlatform() {
    let ua = navigator.userAgent,
       win = /Win/i.test(ua),
       lnx = /Linux/i.test(ua) ||
               /X11/i.test(ua) ||
               /Mac/i.test(ua);
    return (!win && lnx)? "linux"
                        : "windows";
  }
  // Default to actual platform
  let currentPlatform = detectPlatform(),
//let currentPlatform = "linux", // (do this/similar when developing/tweaking)
      win = element("div","gwindow " + currentPlatform);
//win.style.display = "none"; // hidden by default (already in theGUI4.css / .gwindow{} )
  win.parent = parent;
//  if (w) {
    win.style.top = y + "px";
    win.style.left = x + "px";
    win.style.width = w + "px";
    win.style.height = h + "px";
//  }   
  win.style.zIndex = ++$zCounter;
  win.addEventListener("mousedown", function(){ $focusWindow(win); });

  // hmm.. (now marked as "creation only" in the docs...)
//  if (currentPlatform !== "headerless") {
    let titlebar = element("div","gtitlebar"),
        titletxt = element("div","gtitle"),
        controls = element("div","gcontrols");
    titletxt.innerHTML = title;

    function makeButton(cls, action) {
      var btn = element("div","gbtn " + cls);
      if (action) {
        btn.onclick = action;
      } else {
        btn.style.display === "none"; // mnz
      }
      return btn;
    }

//  function minimize() {
//    var content = win.querySelector(".gcontent");
//    if (content) content.style.display = "none";
//    win.dataset.minimized = "true";
//  }

    function toggle_rst() { // aka maximise/restore
      // aside: occasionally rst "resets" in the browser developer tools:
      //        closing and re-opening dev tools window seems to fix it...
      // double-clicking the titlebar (works even when rstbtn hidden)
      var rstBtn = win.querySelector(".gbtn.rst");
      if (!rstBtn || !rstBtn.classList.contains("disabled")) {
        if (win.dataset.maximized === "true") {
          win.style.top = win.dataset.prevTop;
          win.style.left = win.dataset.prevLeft;
          win.style.width = win.dataset.prevWidth;
          win.style.height = win.dataset.prevHeight;
          win.dataset.maximized = "false";
          if (rstBtn) rstBtn.classList.remove("restore");
        } else {
          win.dataset.prevTop = win.style.top;
          win.dataset.prevLeft = win.style.left;
          win.dataset.prevWidth = win.style.width;
          win.dataset.prevHeight = win.style.height;
          win.style.top = "0";
          win.style.left = "0";
          win.style.width = "100%";
          win.style.height = "100%";
          win.dataset.maximized = "true";
          if (rstBtn) rstBtn.classList.add("restore");
        }
      }
    }

    function closeWindow(e) {
      if (!e.currentTarget.classList.contains("disabled")) {
        win.remove();
        $windows = $windows.filter(function(w){ return w !== win; });
      }
    }

//  controls.appendChild(makeButton("mnz", minimize));
    controls.appendChild(makeButton("mnz"));
    controls.appendChild(makeButton("rst", toggle_rst));
    controls.appendChild(makeButton("clx", closeWindow));

    const path = "M102 30q14 21-25 39l42 47h-13L8 11q71-2 86 13l-6 7Q68 18 29 19l39 "+
                 "41q32-7 27-22l7-8m-90 87h13-13l94-107h14L65 72q34-8-8 45H46q44-50 "+
                 "12-38l-33 38m84-39-21 28q-4 6 0 10-19 3-12-9l21-27q5-6 2-10 15-2 10 8",
          icon = element("div"),
          svg = svgns_element("svg"),
          spath = svgns_element("path");
    svg.setAttribute("viewBox", "-74 -64 256 256");
    svg.style.width = "30px";
    svg.style.height = "30px";
    spath.setAttribute("d", path);
    spath.setAttribute("fill", "green");
    svg.appendChild(spath);
    icon.setAttribute("class", "icon"); // (technically unused)
    icon.style.width = "30px";
    icon.style.height = "30px";
    icon.appendChild(svg);

    titlebar.appendChild(icon);
    titlebar.appendChild(titletxt);
    titlebar.appendChild(controls);
    win.appendChild(titlebar);

    let offsetX, offsetY, dragging = false; // (captured closure variables)
    function mousemove(e) {
      if (dragging) {
        win.style.left = (e.clientX - offsetX) + "px";
        win.style.top = (e.clientY - offsetY) + "px";
      }
    }
    function mouseup() {
      dragging = false;
      document.onmousemove = null;
    }
    function mousedown(e) {
      if (!e.target.classList.contains("gbtn")) {
        dragging = true;
        offsetX = e.clientX - win.offsetLeft;
        offsetY = e.clientY - win.offsetTop;
        document.onmousemove = mousemove;
        document.onmouseup = mouseup;
      }
    }
    titlebar.addEventListener("mousedown", mousedown);
    titlebar.ondblclick = toggle_rst;
//  } else {
  if (currentPlatform === "headerless") {
    win.style.resize = "none";
    titlebar.style.display = "none";
  }

  function drawCross(canvas) {
    let ctx = canvas.getContext("2d"),
          r = canvas.getBoundingClientRect(),
          w = r.width,
          h = r.height;
    canvas.width = w;
    canvas.height = h;
    ctx.clearRect(0, 0, w, h);
    ctx.strokeStyle = "black";
    ctx.beginPath();
    ctx.moveTo(0, 0);
    ctx.lineTo(w, h);
    ctx.moveTo(w, 0);
    ctx.lineTo(0, h);
    ctx.stroke();
  }
  function resize_observer(entries) {
    let canvas = entries[0].target.querySelector("canvas");
    drawCross(canvas);
//  for (var i = 0; i < entries.length; i += 1) {
//    // DEV to be replaced with appliction's REDRAW handler:
//    drawCross(entries[i].target.querySelector("canvas"));
//  }
  }
  let content = element("div","gcontent"),
      canvas = element("canvas");
//content.style.width = "calc(100% - 2px)";
  content.style.height = "calc(100% - 32px)";
  content.appendChild(canvas);
  if (draw_cross) {
    let observer = new ResizeObserver(resize_observer);
    observer.observe(content);
  }
  win.appendChild(content);

  document.body.appendChild(win);
  $windows.push(win);

  function switchPlatform(newPlatform) {
    currentPlatform = newPlatform; // (not sure whether that helps at all, btw)
    // Update all open windows
    function setplat(win) {
      win.classList.remove("windows", "linux", "headerless");
      win.classList.add(newPlatform);
      // also reset button icons if needed
      const rstBtn = win.querySelector(".gbtn.rst");
      if (rstBtn) { 
        if (win.dataset.maximized === "true") {
          rstBtn.classList.add("restore");
        } else {
          rstBtn.classList.remove("restore");
        }
      }
      // and the header
      const header = win.querySelector(".gtitlebar");
      if (header) {
        const v = (newPlatform === "headerless");
        header.style.display = v ? "none" : "flex";
        win.style.resize = v ? "none" : "both";
      }
    }
    $windows.forEach(setplat);
  }

  // Listen for key presses
  function keydown(e) {
    let key = e.key.toUpperCase();
    if (key === "L") { switchPlatform("linux"); }
    if (key === "W") { switchPlatform("windows"); }
    if (key === "H") { switchPlatform("headerless"); }
    function toggle(win,query) {
      var btn = win.querySelector(query);
      if (btn) {
        if (btn.style.display === "none") {
          btn.style.display = "flex";
        } else if (btn.classList.contains("disabled")) {
          btn.style.display = "none";
          btn.classList.remove("disabled");
        } else {
          btn.classList.add("disabled");
        }
      }     
    }
//  function togglemnz(win) { toggle(win,".gbtn.mnz"); }
//  function togglerst(win) { toggle(win,".gbtn.rst"); }
//  function toggleclx(win) { toggle(win,".gbtn.clx"); }
//  if (key === "M") { $windows.forEach(togglemnz); }
//  if (key === "R") { $windows.forEach(togglerst); }
//  if (key === "X") { $windows.forEach(toggleclx); }
    if (key === "M") { toggle(win,".gbtn.mnz"); }
    if (key === "R") { toggle(win,".gbtn.rst"); }
    if (key === "X") { toggle(win,".gbtn.clx"); }
    if (key === "ESCAPE") { 
        let w = $get_focus();
        if (w) {
            let p = w.parent;
            w.style.display = "none";
            if (p) { $focusWindow(p); }
        }
    }
  }
  if ($windows.length === 1) { // (we may as well only do this the once)
    document.addEventListener("keydown", keydown);
  }
//  win.addEventListener("keydown", keydown);
  return win;
}

function gShow(win) {
  win.style.display = "flex";
  $focusWindow(win);
}

function gMainLoop(win) {
  if (win) { gShow(win); }
  // idle loop placeholder (/rely on the browser event loop)
}

