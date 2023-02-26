"use strict";
//
// xpGUI.js
// ========
//
//  Emulates xpGUI.e in the browser, so (with a little help from p2js) we
//  can develop/test on the desktop and publish straight to the web.
//  Naturally it is and will always be a subset of the Phix language...
//
requires(JS);

function gVersion() { return "xpGUI.js version 0.1 (32 bits)"; }

function gUseGTK() { /* do nothing */ }

//let $storeAttr = {};  // element-specific attribute handlers/setters
let $storeAttr; // element-specific attribute handlers/setters

function $pxFloat(s) { return (s === "auto") ? 0 : parseFloat(s.replace("px", "")); }
function $eHeight(elem) { return $pxFloat(getComputedStyle(elem).height); }
function $eWidth(elem) { return $pxFloat(getComputedStyle(elem).width); }

function $to_bool(val) {
    // convert YES/NO to true/false (as a common requirement)
    if (typeof(val) !== "string") { return val; }
    val = val.toUpperCase();
    if (val === "YES" || val === "ON" || val === "1") { return true; }
    if (val === "NO" || val === "OFF" || val === "0") { return false; }
    crash("uh?");
}

let $ctrlKey  = false, /// or event.ctrlKey, for IupGetInt(NULL,"CONTROLKEY")
    $shiftKey = false; /// or event.shiftKey, for IupGetInt(NULL,"SHIFTKEY")

//function IupOpen() {
function $gInit() {
    // invoke using if(typeof($storeAttr) === "undefined") { $gInit(); }, as needed
//try first:
    // invoke using if(!$storeAttr) { $gInit(); }, as needed
    $storeAttr = {};
    //
    // DEV/SUG: [not yet implemented!]
    // Note that pwa/p2js collates elems/names from all the store_attrs()
    //           calls in this (pGUI.js) to determine which attributes 
    //           it should permit(/or warn about) on the desktop (!!!).
    //           [done whenever file modified!=last analysed date&time]
    //

    // Handle the case of "save as" running locally: it will dump a 
    // complete copy of the generated dialog without any handlers, so 
    // all you would see is a blank and unresponsive dialog overlay.
    // You can of course avoid this message by manually deleting the
    // offending dialog(s) from the end of the saved html file.
    let d = Array.from($docBody.querySelectorAll(".dialog"));
    if (d.length) {
        puts(1,`Warning: "Save as" dialog(s) deleted`);
//      crash("placeholder"); // (this may need to be delayed...)
        for (let i=0; i<d.length; i += 1) {
            let di = d[i];
            $docBody.removeChild(di);
        }
    }
    
    function shiftkey_cb(event) {
        $ctrlKey = event.ctrlKey; // for IupGetInt()
        $shiftKey = event.shiftKey; // for IupGetInt()
        return true;
    }
    $docBody.addEventListener("keydown",shiftkey_cb);
    $docBody.addEventListener("keyup",shiftkey_cb);

    function store_attrs(elems, names, fn) {
        if (!Array.isArray(elems) ||
            !Array.isArray(names) ||
            typeof(fn) !== "function") {
            crash("invalid elems/names/fn");
        }
        for (let e = 0; e < elems.length; e += 1) {
            let elem = elems[e];
            if (typeof(elem) !== "string") {
                crash("invalid elem");
            }
            if (!$storeAttr.hasOwnProperty(elem)) {
                $storeAttr[elem] = {};
            }
            for (let n = 0; n < names.length; n += 1) {
                let name = names[n];
                if (typeof(name) !== "string") {
                    crash("invalid name");
                }
                $storeAttr[elem][name] = fn;
            }
        }
    }

    function intint(val) {
        if (typeof(val) === "string") {
            // convert eg "225x75" to [225,75]
            //  (ie a js Array of length 2)
            let x = val.indexOf('x'), y;
            if (x !== -1) {
                y = Number(val.slice(x+1));
                x = Number(val.slice(0,x));
                if (Number.isInteger(x) &&
                    Number.isInteger(y)) {
                    val = [x,y];
                }
            }
        }
        if (!Array.isArray(val) ||
            val.length !== 2) {
            crash("invalid intint value");
        }
        return val;
    }

    function intints(val, w, h, name) {
        // version of intint() for dialog [raster]size, supporting eg "QUARTERxEIGHTH"
        if (typeof(val) === "string") {
            // convert eg "225x75" to [225,75]
            //  (ie a js Array of length 2)
            let x = val.indexOf('x'), y;
            if (x !== -1) {
                y = val.slice(x+1);
                x = val.slice(0,x);
                function fulleighth(s,x,f) {
                    let n = Number(s);
                    if (Number.isInteger(n)) {
                        if (name === "SIZE") {
                            n *= f;
                        }
                        return floor(n);
                    }
                    if (s === "FULL") { s = x; }
                    if (s === "HALF") { s = floor(x/2); }
                    if (s === "THIRD") { s = floor(x/3); }
                    if (s === "QUARTER") { s = floor(x/4); }
                    if (s === "EIGHTH") { s = floor(x/8); }
                    return s;
                }
                x = fulleighth(x,w,6/4);
                y = fulleighth(y,h,15/8);
                if (Number.isInteger(x) &&
                    Number.isInteger(y)) {
                    val = [x,y];
                }
            }
        }
        if (!Array.isArray(val) ||
            val.length !== 2) {
            crash("invalid intint value");
        }
        return val;
    }

    function set_style(ih, sname, val) {
        if (val.indexOf('x') === -1) {
            // treat eg "200" as "200x"
            val = val + "x";
        }
        let [w,h] = intint(val);
        w += "px";
        h += "px";
        ih.style[sname+"Top"] = h;
        ih.style[sname+"Left"] = w;
        ih.style[sname+"Right"] = w;
        ih.style[sname+"Bottom"] = h;
    }

    function set_font(ih,val) {
        let face = val, fallback,
            comma = val.indexOf(',');
        if (comma !== -1) {
            face = face.slice(0,comma);
            let stylesize = val.slice(comma+1),
                space = stylesize.indexOf(' ');
            while (space !== -1) {
                if (space) {
                    let stylename = val.slice(0,space);
                    // Bold, Italic, Underline and Strikeout
                    if (stylename === "Bold") {
                        ih.style.fontWeight = "bold";
                    } else if (stylename === "Italic") {
                        ih.style.fontStyle = "italic";
                    } else if (stylename === "Underline") {
                        ih.style.textDecoration = "underline";
                    } else if (stylename === "Strikeout") {
                        ih.style.textDecoration = "line-through";
//text-decoration: underline;
//text-decoration: line-through;
//font-style: italic;
//font-weight: bold;
                    } else {
                        crash("FONT="+val+"??\n");
                    }
                }
                stylesize = stylesize.slice(space+1);
                space = stylesize.indexOf(' ');
            }
            stylesize = parseInt(stylesize);
            if (!stylesize) {
                crash("FONT="+val+"??\n");
            } else if (stylesize < 0) {
                ih.style.fontSize = -stylesize + "px";
            } else {
                ih.style.fontSize = stylesize + "pt";
            }
        }
        if (face === "Tahoma" ||
            face === "Verdana" ||
            face === "Arial") {
            fallback = "sans_serif";
//          fallback = "serif";
        } else if (face === "Courier") {
            fallback = "monospace";
        } else {
//          crash("FONT="+val+"??\n");
            puts(1,"FONT="+val+"??\n"); // placeholder
        }
        ih.style.fontFamily = `"` + face + `", ` + fallback;
//font-family: "Tahoma", sans-serif;
//font-size: 12px;
//  "Times, Bold 18"
//  "Arial, 24" (no style)
//  "Courier New, Italic Underline -30" (size in pixels)
    }

    function rastersize(ih, name, val) {
        // applies to dialog, label, text, canvas, ...
        assert(name === "SIZE" || name === "RASTERSIZE");
        let cn = ih.classList[0];
        if ((val === null) || (val === NULL)) {
            if (cn === "canvas") {
                ih = IupGetDialog(ih);
                cn = "dialog";
            }
            if (cn === "dialog") {
//DEV still not sure about this...
//9/5/22:
//              ih.minWidth = 260;
                if (!ih.minWidth) { ih.minWidth = 260; }
//              ih.minimumWidth = 0;
//              ih.minHeight = 160;
                if (!ih.minHeight) { ih.minHeight = 160; }
//              ih.minimumHeight = 0;
//put back for Sierpinski_curve 5/4/22:
//              ih.minimumWidth = 0;
                ih.minimumWidth = ih.minWidth;
//              ih.minimumHeight = 0;
                ih.minimumHeight = ih.minHeight;
            }
        } else {
            let w, h;
            if (cn === "dialog") {
                [w, h] = intints(val,window.innerWidth,window.innerHeight,name);
            } else {
                if (cn === "hbox" && val.indexOf('x') === -1) {
                    // treat eg "200" as "200x"
                    val = val + "x";
                } else if (cn === "vbox" && val.indexOf('x') === -1) {
                    // treat eg "200" as "x200"
                    val = "x" + val;
                }
                [w, h] = intint(val);
                if (name === "SIZE") {
                    // assume CHARSIZE of 6x15
                    w *= 6/4;
                    h *= 15/8;
                }
            }
            if (cn === "canvas") {
                if (w) { ih.width = w; }
                if (h) { ih.height = h; }
            } else {
//DEV still not sure about this...
//              if (w) { ih.style.width = w + "px"; }
                ih.style.width = w ? w + "px" : "";
//              if (h) { ih.style.height = h-10 + "px"; }
                ih.style.height = h ? h-10 + "px" :"";
            }
        }
    }
    store_attrs(["button","canvas","dialog","frame","hbox","label","text","vbox","tabcontainer","progress"], ["RASTERSIZE","SIZE"], rastersize);

    function set_dialog(ih, name, val) {
        assert(ih.classList[0] === "dialog");
        if (name === "TITLE") {
            const hdr = ih.querySelector(".dialog-handle");
            hdr.innerHTML = val;
        } else if (name === "MINSIZE") {
            let [w, h] = intint(val);
            h += 10;
//          if (ih.style.width < w)  { ih.style.width = w + "px"; }
//          if (ih.style.height < h) { ih.style.height = h + "px"; }
            ih.minWidth = w;
            ih.minimumWidth = w;
            ih.minHeight = h;
            ih.minimumHeight = h;
//9/5/22:
            if (ih.isConnected) {
//7/5/22:
                if (!ih.style.width || ih.style.width < w) {
                    ih.style.width = w ? w + "px" : "";
                }
                if (!ih.style.height || ih.style.height < h) {
                    ih.style.height = h ? h-10 + "px" :"";
                }
            }
        } else if (name === "MAXSIZE") {
            let [w, h] = intint(val);
//          ih.maxWidth = w;
            ih.maximumWidth = w;
//          ih.minHeight = h;
            ih.maximumHeight = h;
//7/5/22:
            if (!ih.style.width || ih.style.width > w) {
                ih.style.width = w ? w + "px" : "";
            }
            if (!ih.style.height || ih.style.height > h) {
                ih.style.height = h ? h-10 + "px" :"";
            }
//      } else if (name === "MARGIN" || name === "GAP") {
        } else if (name === "MARGIN") {
//DEV tryme:
//          set_style(ih,"margin",val);
            let [w, h] = intint(val);
            if (h) {
                ih.style.marginTop = h;
                ih.style.marginBottom = h;
            }
            if (w) {
                ih.style.marginLeft = w;
                ih.style.marginRight = w;
            }
//DEV...?? (in general and above)
        } else if (name === "GAP") {
            let g = Number(val);
            ih.style.margin = g;
        } else if (name === "RESIZE") {
            val = $to_bool(val);
//          ,["YES"])
//          let maxbtn = ih.getElementsByClassName("Maximise")[0],
            let maxbtn = ih.querySelector(".Maximise"),
//              resizers = ih.getElementsByClassName("dialog-resizers")[0],
                resizers = ih.querySelector(".dialog-resizers"),
                display_style = val ? "block" : "none";
            maxbtn.style.display = display_style;
            resizers.style.display = display_style;
//      } else if (name === "SIZE") {
//          puts(1,"IupDialog(SIZE,"+val+")??\n"); // placeholder
        } else if (name === "OPACITY") {
            ih.style.opacity = Number(val)/255;
        } else if (name === "PLACEMENT") {
//          val = $to_bool(val,["MAXIMIZED"]) [maybe not]
            if (val==="MAXIMIZED") {
//              $maxWindow(ih);
                if (!ih.classList.contains("window-maximized")) {
                    setTimeout($maxWindow, 100, ih);
                }
//          } else if (val === "MINIMIZED") {   // not supported..
            } else if (val === "NORMAL") {  // not supported..
                if (ih.classList.contains("window-maximized")) {
                    setTimeout($maxWindow, 100, ih);
                }
            } else {
                crash("IupStoreAttribute(dialog,\"" + name + "\",\"" + val + "\") not yet implemented");
            }
        } else if (name === "VISIBLE") {
//          puts(1,"IupDialog(VISIBLE,"+val+")??\n"); // placeholder
            val = $to_bool(val);
            let display_style = val ? "block" : "none";
            ih.style.display = display_style;
        } else if (name === "FULLSCREEN") {
//          puts(1,"IupDialog(FULLSCREEN,"+val+")??\n"); // placeholder
            val = $to_bool(val);
            let header = ih.querySelector(".dialog-header"),
                resizers = ih.querySelector(".dialog-resizers");
            if (val) {
                header.style.display = "none";
                resizers.style.display = "none";
                set_dialog(ih, "PLACEMENT", "MAXIMIZED");
                document.documentElement.requestFullscreen();
            } else if (document.fullscreenElement) {
                document.exitFullscreen();
                header.style.display = "flex";
                resizers.style.display = "block";
//              set_dialog(ih, "PLACEMENT", "NORMAL");
                if (ih.classList.contains("window-maximized")) {
                    $maxWindow(ih);
                }
            }
        } else if (name === "SHRINK") {
//          puts(1,"IupDialog(SHRINK,"+val+")??\n"); // placeholder
            ih.minWidth = 260;
            ih.minimumWidth = 0;
            ih.minHeight = 160;
            ih.minimumHeight = 0;
        } else if (name === "DIALOGFRAME") {
            // Set the common decorations for modal dialogs. This means RESIZE=NO, MINBOX=NO and MAXBOX=NO. 
            // In Windows, if the PARENTDIALOG is defined then the MENUBOX is also removed, but the Close button remains. 
            set_dialog(ih, "RESIZE", false);
//          puts(1,"IupDialog(DIALOGFRAME,"+val+")??\n"); // placeholder
        } else if (name === "CHILDOFFSET") {
// ignore for now (demo\rosetta\Animation.exw)
//          puts(1,"IupDialog(CHILDOFFSET,"+val+")??\n"); // placeholder
        } else if (name === "MINBOX") {
            puts(1,"IupDialog(MINBOX,"+val+")??\n"); // placeholder
        } else if (name === "MAXBOX") {
            puts(1,"IupDialog(MAXBOX,"+val+")??\n"); // placeholder
        } else {
            crash("IupStoreAttribute(dialog,\"" + name + "\",\"" + val + "\") not yet implemented");
// neither yet tried:
//          crash(`IupStoreAttribute(dialog,"` + name + `","` + val + `\) not yet implemented`);
//          crash(`IupStoreAttribute(dialog,"%s","%s") not yet implemented`,["sequence",name,val]);
        }
    }
    store_attrs(["dialog"], ["TITLE","MINSIZE","MAXSIZE","MARGIN","FULLSCREEN",
                             "GAP","RESIZE","OPACITY","DIALOGFRAME","CHILDOFFSET",
                             "PLACEMENT","VISIBLE","SHRINK","MINBOX","MAXBOX"], set_dialog); // (common-up when appropriate)
//  store_values("MINSIZE",[["%d"],"x",["%d"]])
//--    dlg = IupDialog(lbl,`TITLE="Hello", SIZE=150x40, MINSIZE=225x75`)
//      dlg = IupDialog(lbl,`TITLE="Hello", MINSIZE=225x75`)
//--    dlg = IupDialog(lbl,`TITLE="Hello", MINSIZE=225x75, PLACEMENT=MAXIMIZED`) -- (more like it)
//--    dlg = IupDialog(lbl,`TITLE="Hello", MINSIZE=225x75, FULLSCREEN=YES`) (be like F11 in the browser...)

    function expand(ih,val) {
//Value: "YES" (both directions), "HORIZONTAL", "VERTICAL", "HORIZONTALFREE", "VERTICALFREE" or "NO".
//Default: "NO". For containers the default is "YES".
//Affects: All elements, except menus. 
        if (val === "YES" || val === "BOTH") {
            ih.classList.add("expandv");
            ih.classList.add("expandh");
        } else if (val === "HORIZONTAL") {
            ih.classList.remove("expandv");
            ih.classList.add("expandh");
        } else if (val === "VERTICAL") {
            ih.classList.add("expandv");
            ih.classList.remove("expandh");
        } else if (val === "NO") {
            ih.classList.remove("expandv");
            ih.classList.remove("expandh");
        } else {
            crash("IupStoreAttribute(IupLabel,\"EXPAND\", \"" + val + "\"??)");
        }
    }

    function align(ih,val,elem) {
//ALIGNMENT  (non inheritable) horizontal and vertical alignment. 
//              Possible values: "ALEFT", "ACENTER" and "ARIGHT", combined to "ATOP", "ACENTER" and "ABOTTOM". 
//              Default: "ALEFT:ACENTER". 
//??            Partial values are also accepted, like "ARIGHT" or ":ATOP", the other value will be used from the current alignment. 
        ih.style.display = "flex";
        let k = val.indexOf(':');
        if (k !== -1) {
            let vtb = val.slice(k+1);
                val = val.slice(0,k);
            if (vtb === "ATOP" ||
                vtb === "ALEFT") {
                ih.style.alignItems = "flex-start";
//              ih.classList.add("expandv");
//              ih.classList.add("expandh");
            } else if (vtb === "ACENTER") {
                ih.style.alignItems = "center";
//              ih.classList.remove("expandv");
//              ih.classList.add("expandh");
            } else if (vtb === "ABOTTOM" ||
                       vtb === "ARIGHT") {
                ih.style.alignItems = "flex-end";
//              ih.classList.add("expandv");
//              ih.classList.remove("expandh");
            } else {
                crash("IupStoreAttribute(" + elem + ",\"ALIGNMENT\", \":" + vtb + "\"??)");
            }
//          puts(1,"IupStoreAttribute(IupLabel,\"ALIGNMENT\", \":" + vtb + "\" not yet implemented)");
        }
        if (val.length) {
//          ih.style.display = "grid";
            if (val === "ALEFT") {
//              ih.style.placeItems = "start";  // (top left)
                ih.style.justifyContent = "flex-start";
            } else if (val === "ACENTER") {
//              ih.style.placeItems = "center"; // (mid/mid)
                ih.style.justifyContent = "center";
            } else if (val === "ARIGHT") {
//              ih.style.placeItems = "end";    // (btm right)
                ih.style.justifyContent = "flex-end";
            } else {
                crash("IupStoreAttribute(" + elem + ",\"ALIGNMENT\", \"" + val + "\"??)");
            }
        }
    }   
    
    function set_frame(ih, name, val) {
        assert(ih.classList[0] === "frame");
//DEV spotted in passing: some of these are not reachable aka not set in store_attrs below...
        if (name === "EXPAND") {
            expand(ih,val);
//      } else if (name === "ALIGNMENT") {
//          align(ih,val,"IupList");
        } else if (name === "PADDING") {
            set_style(ih,"padding",val);
        } else if (name === "MARGIN") {
//DEV tryme:
            set_style(ih,"margin",val);
//          let [w, h] = intint(val);
//          if (h) {
//              ih.style.marginTop = h;
//              ih.style.marginBottom = h;
//          }
//          if (w) {
//              ih.style.marginLeft = w;
//              ih.style.marginRight = w;
//          }
        } else if (name === "ACTIVE") {
            val = $to_bool(val);
            ih.disabled = !val;
            puts(1,"?!IupFrame(ACTIVE"+val+")??\n"); // placeholder
        } else if (name === "TITLE") {
            ih.childNodes[0].innerText = val;
        } else if (name === "VISIBLE") {
            puts(1,"IupFrame(VISIBLE"+val+")??\n"); // placeholder
        } else if (name === "FGCOLOR") {
//DEV tryme:
//          set_style(ih,"margin",val);
            puts(1,"IupFrame(FGCOLOR"+val+")??\n"); // placeholder
        } else if (name === "NAME") {
            ih["NAME"] = val;
            puts(1,"?!IupFrame(NAME,"+val+")??\n"); // placeholder
        } else {
            crash("IupStoreAttribute(IupFrame,\"" + name + "\") not yet implemented\n");
        }
    }
    store_attrs(["frame"], ["PADDING","MARGIN","TITLE","VISIBLE","FGCOLOR","NAME"], set_frame);

//Ihandle lbl = IupLabel("World","EXPAND=YES, ALIGNMENT=ACENTER"),
    function set_label(ih, name, val) {
        assert(ih.classList[0] === "label");
        if (name === "TITLE") {
            if (typeof(val) === "string") {
                const sp = new RegExp("[ ]","g"),
                      lf = new RegExp("\\n","g");
/*                   
    const am = new RegExp("&","g"),
          lt = new RegExp("[<]","g"),
          gt = new RegExp("[>]","g"),
          sp = new RegExp("[ ]","g"),
          lf = new RegExp("\\n","g");
    text = text.replace(am,"&amp;")
               .replace(lt,"&lt;")
               .replace(gt,"&gt;")
               .replace(sp,"&ensp;")
               .replace(lf,"<br>");
*/
                val = val.replace(sp,"&ensp;")
                         .replace(lf,"<br>");
            }
//          ih.innerHTML = val;
//          ih.innerText = val;
            ih.innerHTML = "<nobr>"+val+"</nobr>";
        } else if (name === "EXPAND") {
            expand(ih,val);
        } else if (name === "ALIGNMENT") {
            align(ih,val,"IupLabel");
        } else if (name === "PADDING") {
            set_style(ih,"padding",val);
//          let [w, h] = intint(val);
//          ih.style.paddingTop = h + "px";
//          ih.style.paddingBottom = h + "px";
//          ih.style.paddingLeft = w + "px";
//          ih.style.paddingRight = w + "px";
        } else if (name === "ACTIVE") {
            val = $to_bool(val);
            ih.disabled = !val;
            puts(1,"?!IupLabel(ACTIVE,"+val+")??\n"); // placeholder
        } else if (name === "VISIBLE") {
            puts(1,"IupLabel(VISIBLE,"+val+")??\n"); // placeholder
        } else if (name === "FGCOLOR") {
            if (val.indexOf(" ")) {
                // eg "255 0 128" ==> "#FF0080".
                val = val.split(" ").map(Number);   // ==> [255,0,128]
//.map
//              val = (+val[0]<<16)+(+val[1]<<8)+(+val[2]);
//              val = "#"+((1<<24)+val).toString(16).slice(1);
//              cal = sprintf("#%06x",colour)
                val = sprintf("#%02x%02x%02x",val);  // ==> #FF0080 (doh)
/*
// pretty cool, eg "Red" -> #ff0000, "PeachPuff" -> #ffdab9, "PaleGoldenRod" -> #eee8aa, 
//  plus "RGB(123,234,142)" -> #7bea8e and "HSL(284,6%,49%)" -> #807584. Handy!
//function standardize_color(str){
//  var ctx = document.createElement("canvas").getContext('2d');
//  ctx.fillStyle = str;
//  return ctx.fillStyle;
//}
*/
            }
            if (val.indexOf("#") !== -1) {
//              ih.style.background=val;
                ih.style.color=val;
            } else {
                puts(1,"IupLabel(FGCOLOR,"+val+")??\n"); // placeholder??
            }
        } else if (name === "FONT" || name === "FONTFACE") {
            set_font(ih,val);
//          puts(1,"IupLabel(FONT,"+val+")??\n"); // placeholder
        } else if (name === "FONTSTYLE") {
            puts(1,"IupLabel(FONTSTYLE,"+val+")??\n"); // placeholder
        } else if (name === "SEPARATOR") {
//          val = $to_bool(val);
            if (val === "HORIZONTAL") {
                ih.classList.add("labelhsep");
            } else {
                ih.classList.remove("labelhsep");
            }
//DEV seems to work for IupLabel(NULL,"SEPARATOR=HORIZONTAL") anyway...
//          puts(1,"?IupLabel(SEPARATOR,"+val+")??\n"); // placeholder
        } else if (name === "NAME") {
            ih["NAME"] = val;
            puts(1,"?!IupLabel(NAME,"+val+")??\n"); // placeholder
        } else if (name === "TIP") {
            puts(1,"IupLabel(TIP,"+val+")\n"); // placeholder
        } else if (name === "TEXTWRAP") {
            puts(1,"IupLabel(TEXTWRAP,"+val+")\n"); // placeholder
        } else {
            crash("IupStoreAttribute(IupLabel,\"" + name + "\") not yet implemented\n");
        }
    }
    store_attrs(["label"], ["TITLE","EXPAND","ALIGNMENT","MARGIN","PADDING","ACTIVE","VISIBLE",
                            "FGCOLOR","FONT","FONTFACE","FONTSTYLE","SEPARATOR","NAME","TIP",
                            "TEXTWRAP"], set_label);
//  store_values("EXPAND",[["YES","NO","HORIZONTAL","HORIZONTALFREE","VERTICAL","VERTICALFREE"]])
//  store_values("ALIGNMENT",[["ALEFT","ACENTER","ARIGHT"],":",["ATOP","ACENTER","ABOTTOM"]])

    function set_list(ih, name, val) {
        assert(ih.classList[0] === "list");
        if (name === "CANFOCUS") {
            puts(1,"IupList(CANFOCUS,"+val+")??\n"); // placeholder
        } else if (name === "EXPAND") {
            expand(ih,val);
//      } else if (name === "ALIGNMENT") {
//          align(ih,val,"IupList");
        } else if (name === "PADDING") {
            set_style(ih,"padding",val);
        } else if (name === "ACTIVE") {
            val = $to_bool(val);
            ih.disabled = !val;
            puts(1,"?!IupList(ACTIVE,"+val+")??\n"); // placeholder
        } else if (name === "DROPDOWN") {
            puts(1,"IupList(DROPDOWN,"+val+")??\n"); // placeholder
            val = $to_bool(val);
            ih.DROPDOWN = val;
        } else if (name === "APPENDITEM") {
            puts(1,"IupList(APPENDITEM,"+val+")??\n"); // placeholder
        } else if (name === "REMOVEITEM") {
            puts(1,"IupList(REMOVEITEM,"+val+")??\n"); // placeholder
        } else if (name === "TIP") {
            puts(1,"IupList(TIP,"+val+")??\n"); // placeholder
        } else if (name === "VALUE") {
            puts(1,"IupList(VALUE,"+val+")??\n"); // placeholder
        } else if (name === "VISIBLE") {
            puts(1,"IupList(VISIBLE,"+val+")??\n"); // placeholder
        } else if (name === "VISIBLELINES") {
            puts(1,"IupList(VISIBLELINES,"+val+")??\n"); // placeholder
        } else if (name === "VISIBLECOLUMNS") {
            puts(1,"IupList(VISIBLECOLUMNS,"+val+")??\n"); // placeholder
        } else if (name === "VISIBLEITEMS") {
            puts(1,"IupList(VISIBLEITEMS,"+val+")??\n"); // placeholder
        } else if (name === "SHOWDROPDOWN") {
            puts(1,"IupList(SHOWDROPDOWN,"+val+")??\n"); // placeholder
        } else if (name === "<list_id>") {
//          puts(1,"IupList(<list_id>,"+val+")??\n"); // placeholder
            [/*integer*/ name, /*string*/ val] = val;
            //... (DEV)
            let n = ih.childElementCount;
            if (name>n) {
                let childoption = document.createElement("option"),
                    t = document.createTextNode(val);
                childoption.setAttribute("value", name.toString());
                childoption.appendChild(t);
                ih.appendChild(childoption);
                n += 1;
            } else {
                let childoption = ih.children[n-1],
                    t = childoption.childNodes[0];
                t.textContent = val;
//              crash("IupList replace???"); // placeholder
            }
            if (!ih.DROPDOWN) { ih.size = n; }
        } else if (name === "NAME") {
            puts(1,"IupList(NAME,"+val+")??\n"); // placeholder
        } else if (name === "EDITBOX") {
            puts(1,"IupList(EDITBOX,"+val+")??\n"); // placeholder
        } else {
            crash("IupStoreAttribute(IupList,\"" + name + "\") not yet implemented\n");
        }
    }
//DEV must do better...
//maybe: return ((parseInt(str, 10).toString() == str) && str.indexOf('-') === -1);

    // note: "1", "2", etc are handled specially, see IupSetStrAttribute() and IupSetAttributeId().
    store_attrs(["list"], ["<list_id>","CANFOCUS","DROPDOWN","EXPAND","APPENDITEM","REMOVEITEM",
                           "TIP","VALUE","VISIBLE","VISIBLELINES","VISIBLECOLUMNS","VISIBLEITEMS",
                           "SHOWDROPDOWN","NAME","EDITBOX"], set_list);

    function set_vbox(ih, name, val) {
        let cn = ih.classList[0];
        assert(cn === "vbox") || (cn === "hbox");
//DEV common up??
        if (name === "MARGIN") {
//          set_style(ih,"margin",val);
            let [w, h] = intint(val);
            if (w) {
                ih.style.marginLeft = w + "px";
                ih.style.marginRight = w + "px";
            }
            if (h) {
                ih.style.marginTop = h + "px";
                ih.style.marginBottom = h + "px";
            }
        } else if (name === "NMARGIN") {
//          set_style(ih,"margin",val);
            puts(1,"IupH/Vbox(NMARGIN," + val + ")\n"); // placeholder
        } else if (name === "TABTITLE") {
            ih.TABTITLE = val;
        } else if (name === "ALIGNMENT") {
//21/10/21:
//          align(ih,val,"IupH/Vbox");
            align(ih,":"+val,"IupH/Vbox");
        } else if (name === "PADDING") {
            set_style(ih,"padding",val);
//          puts(1,"IupH/Vbox(PADDING,"+val+")\n"); // placeholder
        } else if (name === "NORMALIZESIZE") {
            puts(1,"IupH/Vbox(NORMALIZESIZE," + val + ")\n"); // placeholder
//          $normalize(ih.children,val);
        } else if (name === "GAP") {
//for now do nothing...
//          puts(1,"IupH/Vbox(GAP,"+val+")\n"); // placeholder
//      } else if (name === "SIZE") {
//          puts(1,"IupH/Vbox(SIZE," + val + ")\n"); // placeholder
        } else if (name === "LINEBREAK") {
            puts(1,"IupH/Vbox(LINEBREAK," + val + ")\n"); // placeholder
        } else if (name === "FONT") {
            set_font(ih,val);
//      } else if (name === "NORMALSIZE") {
//          puts(1,"IupH/Vbox(LINEBREAK," + val + ")\n"); // placeholder
        } else if (name === "BGCOLOR") {
            puts(1,"IupH/Vbox(BGCOLOR,"+val+")\n"); // placeholder
        } else {
            crash("IupStoreAttribute(IupVbox,\"" + name + "\") not yet implemented");
        }
    }
    store_attrs(["vbox","hbox"], ["MARGIN","NMARGIN","TABTITLE","ALIGNMENT","PADDING",
                                  "NORMALIZESIZE","GAP","LINEBREAK","FONT","BGCOLOR"], set_vbox);

    function set_text(ih, name, val) {
        assert(ih.classList[0] === "text");
        if (name === "VALUE") {
//          ih.innerHTML = val;
//          ih.value = val;
//          const lf = new RegExp("\\n","g");
//          val = val.replace(lf,"<br>");
//          ih.innerHTML = val;
            ih.value = val;
//          ih.innerText = val;
//          ih.textContent = val;
/*
    const am = new RegExp("&","g"),
          lt = new RegExp("[<]","g"),
          gt = new RegExp("[>]","g"),
          sp = new RegExp("[ ]","g"),
          lf = new RegExp("\\n","g");
    text = text.replace(am,"&amp;")
               .replace(lt,"&lt;")
               .replace(gt,"&gt;")
               .replace(sp,"&ensp;")
               .replace(lf,"<br>");
*/
//          ih.innerText = val;
        } else if (name === "EXPAND") {
            expand(ih,val);
        } else if (name === "MASK") {
//          puts(1,"IupText(MASK,"+val+")??\n"); // placeholder
            ih.type = "number";
//          ih.type = "text";
            ih.pattern = val;
        } else if (name === "PADDING") {
            set_style(ih,"margin",val);
        } else if (name === "SPIN") {
//          puts(1,"IupText(SPIN,"+val+")??\n"); // placeholder
//          val = $to_bool(val); // (maybe??)
            ih.setAttribute("type", "number");
//          ih.setAttribute("type", "number");
        } else if (name === "SPINMIN") {
//          puts(1,"IupText(SPINMIN,"+val+")??\n"); // placeholder
            ih.setAttribute("min", val);
        } else if (name === "SPINMAX") {
//          puts(1,"IupText(SPINMAX,"+val+")??\n"); // placeholder
            ih.setAttribute("max", val);
        } else if (name === "ACTIVE") {
            val = $to_bool(val);
            ih.disabled = !val;
//          puts(1,"?!IupText(ACTIVE,"+val+")??\n"); // placeholder -- worked fine, 21/10/21.
        } else if (name === "FILTER") {
//[DEV] not really what I was hoping for...
//          if (val === "NUMBER") {
//              ih.pattern = `^\d*$`;
//          } else {
                puts(1,"IupText(FILTER," + val + ")??\n"); // placeholder
//          }
        } else if (name === "CUEBANNER") {
            puts(1,"IupText(CUEBANNER," + val + ")??\n"); // placeholder
        } else if (name === "NAME") {
            ih["NAME"] = val;
            puts(1,"?!IupText(NAME," + val + ")??\n"); // placeholder
        } else if (name === "TIP") {
            puts(1,"IupText(TIP," + val + ")??\n"); // placeholder
        } else if (name === "MULTILINE") {
            puts(1,"IupText(MULTILINE," + val + ")??\n"); // placeholder
        } else if (name === "FORMATTING") {
            puts(1,"IupText(FORMATTING," + val + ")??\n"); // placeholder
        } else if (name === "APPEND") {
            puts(1,"IupText(APPEND," + val + ")??\n"); // placeholder
        } else if (name === "INSERT") {
            puts(1,"IupText(INSERT," + val + ")??\n"); // placeholder
        } else if (name === "WORDWRAP") {
            puts(1,"IupText(WORDWRAP," + val + ")??\n"); // placeholder
        } else if (name === "SCROLLBAR") {
            puts(1,"IupText(SCROLLBAR," + val + ")??\n"); // placeholder
        } else if (name === "READONLY") {
            puts(1,"IupText(READONLY," + val + ")??\n"); // placeholder
        } else if (name === "BGCOLOR") {
            puts(1,"IupText(BGCOLOR," + val + ")??\n"); // placeholder
        } else if (name === "VISIBLECOLUMNS") {
            puts(1,"IupText(VISIBLECOLUMNS," + val + ")??\n"); // placeholder
        } else if (name === "SELECTION") {
            if (val === "ALL") {
                ih.select();
            } else {
                puts(1,"IupText(SELECTION," + val + ")??\n"); // placeholder
            }
        } else {
            crash("IupStoreAttribute(IupText,\"" + name + "," + val + "\") not yet implemented\n");
        }
    }
    store_attrs(["text"], ["VALUE","EXPAND","MASK","PADDING","SPIN","SPINMIN","SPINMAX","ACTIVE","FILTER",
                           "CUEBANNER","NAME","TIP","MULTILINE","FORMATTING","APPEND","INSERT","WORDWRAP",
                           "SCROLLBAR","SELECTION","READONLY","BGCOLOR","VISIBLECOLUMNS"], set_text);

    function set_toggle(ih, name, val) {
        assert(ih.classList[0] === "toggle");
        if (name === "VALUE") {
//          ih.innerHTML = val;
            val = $to_bool(val);
//21/10/21:
//          ih.checked = val;
            ih.children[1].checked = val;
        } else if (name === "RIGHTBUTTON") {
//          puts(1,"IupToggle(RIGHTBUTTON,"+val+")??\n"); // placeholder
            val = $to_bool(val);
            ih.style.flexDirection = val?"row":"row-reverse";
        } else if (name === "PADDING") {
            set_style(ih,"padding",val);
//          puts(1,"IupToggle(PADDING,"+val+")??\n"); // placeholder
//      } else if (name === "EXPAND") {
//          expand(ih,val);
//      } else if (name === "MASK") {
//          puts(1,"IupToggle(MASK,"+val+")??\n"); // placeholder
//      } else if (name === "SPIN") {
//          puts(1,"IupToggle(SPIN,"+val+")??\n"); // placeholder
//      } else if (name === "SPINMIN") {
//          puts(1,"IupToggle(SPINMIN,"+val+")??\n"); // placeholder
        } else if (name === "TIP") {
            puts(1,"IupToggle(TIP,"+val+")??\n"); // placeholder
        } else if (name === "CANFOCUS") {
            puts(1,"IupToggle(CANFOCUS,"+val+")??\n"); // placeholder
        } else if (name === "NOTE") {
            puts(1,"IupToggle(NOTE,"+val+")??\n"); // placeholder
        } else {
            crash("IupStoreAttribute(IupToggle,\"" + name + "," + val + "\") not yet implemented\n");
        }
    }
    store_attrs(["toggle"], ["CANFOCUS","NOTE","PADDING","RIGHTBUTTON","VALUE","TIP"], set_toggle);

    function linebreak(ih, name, val) {
        // applies to dialog, label, text
        if (name !== "LINEBREAK") { crash("LINEBREAK expected!"); }
        let cn = ih.classList[0];
        puts(1,"linebreak("+cn+","+val+")??\n"); // placeholder
//      ih[LINEBREAK] = val;
//      ih.LINEBREAK = val;
    }

    function set_button(ih, name, val) {
        assert(ih.classList[0] === "button");
        if (name === "ACTIVE") {
            val = $to_bool(val);
            ih.disabled = !val;
//          puts(1,"?!IupButton(ACTIVE,"+val+")??\n"); // placeholder
        } else if (name === "GAP") {
//DEV should be hbox/vbox only???
//          puts(1,"IupButton(GAP,"+val+")??\n"); // placeholder
            crash("IupButton(GAP,"+val+")??\n");
        } else if (name === "EXPAND") {
            puts(1,"IupButton(EXPAND,"+val+")??\n"); // placeholder
//          expand(ih,val);
        } else if (name === "PADDING") {
            set_style(ih,"padding",val);
//          puts(1,"IupButton(PADDING,"+val+")??\n"); // placeholder
        } else if (name === "RUNNING") {
//DEV I think this is a hack for demo\rosetta\Morpion_solitaire.exw ...
            val = $to_bool(val);
            ih[name] = val;
        } else if (name === "TIP") {
            puts(1,"IupButton(TIP,"+val+")??\n"); // placeholder
        } else if (name === "IMAGE") {
            puts(1,"IupButton(IMAGE,"+val+")??\n"); // placeholder
        } else if (name === "IMPRESS") {
            puts(1,"IupButton(IMPRESS,"+val+")??\n"); // placeholder
        } else if (name === "BGCOLOR") {
            puts(1,"IupButton(BGCOLOR,"+val+")??\n"); // placeholder
        } else if (name === "TITLE") {
            ih.innerHTML = "<nobr>" + val + "</nobr>";
        } else {
            crash("IupStoreAttribute(IupButton,\"" + name + "," + val + "\") not yet implemented\n");
        }
    }
    store_attrs(["button"], ["ACTIVE","GAP","EXPAND","PADDING","RUNNING","TIP","IMAGE","IMPRESS","BGCOLOR",
                             "TITLE"], set_button);
    store_attrs(["button"], ["LINEBREAK"], linebreak);
//ToDo: (found while surfin)
//button.setAttribute("disabled", "true");
//button.removeAttribute("disabled");
//[attribute]   [target]        Selects all elements with a target attribute            (PL: eg [inactive] ??)
//[attribute=value]     [target=_blank] Selects all elements with target="_blank"       (PL: eg [type=button] ??)

    function set_canvas(ih, name, val) {
        assert(ih.classList[0] === "canvas");
        if (name === "BORDER") {
            puts(1,"IupCanvas(BORDER,"+val+")??\n"); // placeholder
        } else if (name === "BUFFER") {
            // (ignored, as now documented)
//      } else if (name === "DATA") {
//          puts(1,"IupCanvas(DATA,"+val+")??\n"); // placeholder
        } else if (name === "EXPAND") {
            puts(1,"IupCanvas(EXPAND,"+val+")??\n"); // placeholder
        } else if (name === "BGCOLOR" ||
                   name === "BARMODE" ||
//                 name === "DATA" ||
                   name === "DRID" ||
                   name === "GRIDCOLOR" ||
                   name === "GTITLE" ||
                   name === "MARKSTYLE" ||
                   name === "MODE" ||
                   name === "XNAME" ||
                   name === "YNAME" ||
                   name === "XRID" ||
                   name === "YRID" ||
                   name === "XTICKFMT" ||
                   name === "YTICKFMT") {
            ih[name] = val;
        } else if (name === "GRID" ||
                   name === "LEGENDBOX") {
            val = $to_bool(val);
            ih[name] = val;
        } else if (name === "TITLESTYLE" ||
                   name === "XANGLE" ||
                   name === "XCROSSORIGIN" ||
                   name === "XMARGIN" ||
                   name === "XMAX" ||
                   name === "XMIN" ||
                   name === "XTICK" ||
                   name === "XYSHIFT" ||
                   name === "YANGLE" ||
                   name === "YCROSSORIGIN" ||
                   name === "YMARGIN" ||
                   name === "YMAX" ||
                   name === "YMIN" ||
                   name === "YTICK" ||
                   name === "YXSHIFT") {
            ih[name] = Number(val);
        } else if (name === "DRAWCOLOR") {
            // IupBackgroundBox really, as cdCanvasSetForeground(ctx, colour):
//  if (integer(colour)) { colour = sprintf("#%06x",colour); }
            ih.fillStyle = val;
            ih.strokeStyle = val;
        } else if (name === "DRAWSTYLE") {
            puts(1,"IupCanvas(DRAWSTYLE,"+val+")??\n"); // placeholder
        } else if (name === "DRAWFONT") {
            puts(1,"IupCanvas(DRAWFONT,"+val+")??\n"); // placeholder
        } else if (name === "DRAWTEXTORIENTATION") {
            puts(1,"IupCanvas(DRAWTEXTORIENTATION,"+val+")??\n"); // placeholder
        } else if (name === "DATA") {
            ih.DATA = Number(val);
        } else if (name === "FONT") {
            puts(1,"IupCanvas(FONT,"+val+")??\n"); // placeholder
        } else if (name === "SCROLLBAR") {
            puts(1,"IupCanvas(SCROLLBAR,"+val+")??\n"); // placeholder
// gah, null at this point...
//          ih.parentNode.style.overflow = "scroll";
        } else if (name === "DX") {
            puts(1,"IupCanvas(DX,"+val+")??\n"); // placeholder
        } else if (name === "DY") {
            puts(1,"IupCanvas(DY,"+val+")??\n"); // placeholder
        } else {
            crash("IupStoreAttribute(IupCanvas,\"" + name + "\") not yet implemented\n");
        }
    }
    store_attrs(["canvas"], ["BORDER","BUFFER","DATA","EXPAND","BGCOLOR","BARMODE","DRID","GRID","GRIDCOLOR",
                             "GTITLE","MARKSTYLE","MODE","TITLESTYLE","XNAME","YNAME",
                             "DRAWCOLOR","DRAWSTYLE","DRAWFONT","DRAWTEXTORIENTATION",
                             "XANGLE","XCROSSORIGIN","XMARGIN","XMAX","XMIN","XRID","XTICK","XTICKFMT","XYSHIFT",
                             "YANGLE","YCROSSORIGIN","YMARGIN","YMAX","YMIN","YRID","YTICK","YTICKFMT","YXSHIFT",
                             "FONT","SCROLLBAR","DX","DY","LEGENDBOX"], set_canvas);

    function set_datepick(ih, name, val) {
        assert(ih.classList[0] === "datepick");
        if (name === "ACTIVE") {
//          val = $to_bool(val);
//          ih.disabled = !val;
            puts(1,"IupDatePick(ACTIVE,"+val+")??\n"); // placeholder
        } else if (name === "MONTHSHORTNAMES") {
            puts(1,"IupDatePick(MONTHSHORTNAMES,"+val+")??\n"); // placeholder
        } else if (name === "ZEROPRECED") {
            puts(1,"IupDatePick(ZEROPRECED,"+val+")??\n"); // placeholder
        } else if (name === "VALUE") {
            const fs = new RegExp("/","g");
            val = val.replace(fs,"-");
            ih.value = val;
        } else {
            crash("IupStoreAttribute(IupDatePick,\"" + name + "," + val + "\") not yet implemented\n");
        }
    }
    store_attrs(["datepick"], ["ACTIVE","MONTHSHORTNAMES","ZEROPRECED","VALUE"], set_datepick);

    function set_menuitem(ih, name, val) {
        assert(ih.classList[0] === "menuitem");
        if (name === "ACTIVE") {
//          val = $to_bool(val);
//          ih.disabled = !val;
            puts(1,"IupMenuItem(ACTIVE,"+val+")??\n"); // placeholder
        } else if (name === "VALUE") {
//          val = $to_bool(val);
//          ih.disabled = !val;
            puts(1,"IupMenuItem(VALUE,"+val+")??\n"); // placeholder
        } else if (name === "AUTOTOGGLE") {
//          val = $to_bool(val);
//          ih.disabled = !val;
            puts(1,"IupMenuItem(AUTOTOGGLE,"+val+")??\n"); // placeholder
        } else {
            crash("IupStoreAttribute(IupMenuItem,\"" + name + "\"," + val + ") not yet implemented\n");
        }
    }
    store_attrs(["menuitem"], ["ACTIVE","VALUE","AUTOTOGGLE"], set_menuitem);

    function set_multibox(ih, name, val) {
        assert(ih.classList[0] === "multibox");
//      if (name === "ACTIVE") {
//          val = $to_bool(val);
//          ih.disabled = !val;
//          puts(1,"?!IupMultiBox(ACTIVE,"+val+")??\n"); // placeholder
//      } else if (name === "GAP") {
//          puts(1,"IupMultiBox(GAP,"+val+")??\n"); // placeholder
//      } else if (name === "EXPAND") {
//          expand(ih,val);
//      } else if (name === "PADDING") {
//          set_style(ih,"padding",val);
//          puts(1,"IupMultiBox(PADDING,"+val+")??\n"); // placeholder
        if (name === "MARGIN") {
            puts(1,"IupMultiBox(MARGIN,"+val+")??\n"); // placeholder
        } else if (name === "GAPHORIZ") {
            puts(1,"IupMultiBox(GAPHORIZ,"+val+")??\n"); // placeholder
        } else if (name === "GAPVERT") {
            puts(1,"IupMultiBox(GAPVERT,"+val+")??\n"); // placeholder
        } else if (name === "NMARGIN") {
//          set_style(ih,"margin",val);
            puts(1,"IupMultiBox(NMARGIN," + val + ")\n"); // placeholder
        } else {
            crash("IupStoreAttribute(IupMultiBox,\"" + name + "\"," + val + ") not yet implemented\n");
        }
    }
    store_attrs(["multibox"], ["MARGIN","NMARGIN","GAPHORIZ","GAPVERT"], set_multibox);

    function set_progress(ih, name, val) {
        assert(ih.classList[0] === "progress");
//      if (name === "ACTIVE") {
//          val = $to_bool(val);
//          ih.disabled = !val;
//          puts(1,"?!IupProgressBar(ACTIVE,"+val+")??\n"); // placeholder
//      } else if (name === "EXPAND") {
//          expand(ih,val);
        if (name === "ORIENTATION") {
            puts(1,"IupProgressBar(ORIENTATION,"+val+")??\n"); // placeholder
//          if (val === "HORIZONTAL") {
//              drag.className = "drag-y"
//              ih.style.flexDirection = "column";
//          } else {
//              assert(val === "VERTICAL");
//              drag.className = "drag-x"
//              ih.style.flexDirection = "row";
//          }
        } else if (name === "MIN") {
            puts(1,"IupProgressBar(MIN,"+val+")??\n"); // placeholder
            ih.max = val;
        } else if (name === "MAX") {
            puts(1,"IupProgressBar(MAX,"+val+")??\n"); // placeholder
            ih.min = val;
        } else if (name === "VALUE") {
//          puts(1,"IupProgressBar(MAX,"+val+")??\n"); // placeholder
            ih.value = val;
        } else if (name === "EXPAND") {
            puts(1,"IupProgressBar(EXPAND,"+val+")??\n"); // placeholder
        } else {
            crash("IupStoreAttribute(IupProgressBar,\"" + name + "\"," + val + ") not yet implemented\n");
        }
    }
    store_attrs(["progress"], ["ORIENTATION","MIN","MAX","VALUE","EXPAND"], set_progress);

    function set_split(ih, name, val) {
        assert(ih.classList[0] === "split");
//      if (name === "ACTIVE") {
//          val = $to_bool(val);
//          ih.disabled = !val;
//          puts(1,"?!IupMultiBox(ACTIVE,"+val+")??\n"); // placeholder
//      } else if (name === "GAP") {
//          puts(1,"IupMultiBox(GAP,"+val+")??\n"); // placeholder
//      } else if (name === "EXPAND") {
//          expand(ih,val);
        if (name === "ORIENTATION") {
//          puts(1,"IupSplit(ORIENTATION,"+val+")??\n"); // placeholder
            let drag = ih.children[1];
            if (val === "HORIZONTAL") {
                drag.className = "drag-y"
                ih.style.flexDirection = "column";
            } else {
                assert(val === "VERTICAL");
                drag.className = "drag-x"
                ih.style.flexDirection = "row";
            }
        } else if (name === "MINMAX") {
            puts(1,"IupSplit(MINMAX,"+val+")??\n"); // placeholder
        } else {
            crash("IupStoreAttribute(IupSplit,\"" + name + "\"," + val + ") not yet implemented\n");
        }
    }
    store_attrs(["split"], ["ORIENTATION","MINMAX"], set_split);

    function set_tabcontainer(ih, name, val) {
        assert(ih.classList[0] === "tabcontainer");
//      if (name === "ACTIVE") {
//          val = $to_bool(val);
//          ih.disabled = !val;
//          puts(1,"?!IupMultiBox(ACTIVE,"+val+")??\n"); // placeholder
//      } else if (name === "GAP") {
//          puts(1,"IupMultiBox(GAP,"+val+")??\n"); // placeholder
//      } else if (name === "EXPAND") {
//          expand(ih,val);
//      } else if (name === "PADDING") {
//          set_style(ih,"padding",val);
//          puts(1,"IupMultiBox(PADDING,"+val+")??\n"); // placeholder
        if (name === "TABTYPE") {
            puts(1,"IupTabs(TABTYPE,"+val+")??\n"); // placeholder
        } else {
            crash("IupStoreAttribute(IupTabs,\"" + name + "\"," + val + ") not yet implemented\n");
        }
    }
    store_attrs(["tabcontainer"], ["TABTYPE"], set_tabcontainer);

    function set_slider(ih, name, val) {
        assert(ih.classList[0] === "slider");
        if (name === "ORIENTATION") {
//          puts(1,"IupValuator(ORIENTATION,"+val+")??\n"); // placeholder
            if (val === "VERTICAL") {
                ih.style.transform = "rotate(90deg)";
            } else if (val === "HORIZONTAL") {
                ih.style.transform = "";
            } else {
                crash("uh?");
            }
        } else if (name === "MAX") {
//          puts(1,"IupValuator(MAX,"+val+")??\n"); // placeholder
            ih.setAttribute("max", val);
        } else if (name === "MIN") {
//          puts(1,"IupValuator(MIN,"+val+")??\n"); // placeholder
            ih.setAttribute("min", val);
        } else if (name === "VALUE") {
//          puts(1,"IupValuator(VALUE,"+val+")??\n"); // placeholder
            ih.setAttribute("value", val);
        } else if (name === "STEP") {
//          puts(1,"IupValuator(STEP,"+val+")??\n"); // placeholder
            ih.setAttribute("step", val);
        } else if (name === "PAGESTEP") {
            puts(1,"IupValuator(PAGESTEP,"+val+")??\n"); // placeholder
//          ih.setAttribute("step", val);
        } else if (name === "EXPAND") {
//          puts(1,"IupValuator(EXPAND,"+val+")??\n"); // placeholder
            ih.style.flexGrow = 1;
        } else if (name === "TIP") {
            puts(1,"IupValuator(TIP,"+val+")??\n"); // placeholder
//          ih.setAttribute("step", val);
        } else if (name === "CANFOCUS") {
// just ignore for now...
//          puts(1,"IupValuator(CANFOCUS,"+val+")??\n"); // placeholder
        } else {
            crash("IupStoreAttribute(IupValuator,\"" + name + "\"," + val + ") not yet implemented\n");
        }
    }
    store_attrs(["slider"], ["ORIENTATION","MAX","MIN","VALUE","STEP","PAGESTEP","EXPAND","TIP",
                             "CANFOCUS"], set_slider);

    function set_clipboard(ih, name, val) {
        assert(ih.classList[0] === "clipboard");
        if (name === "TEXT") {
//          puts(1,"IupClipboard(TEXT,"+val+")??\n"); // placeholder
            ih.value = val;
            document.body.appendChild(ih);
            ih.focus();
            ih.select();
            document.execCommand("copy");
            document.body.removeChild(ih);
//          window.Clipboard.prototype.writeText(val);
        } else {
            crash("IupStoreAttribute(IupClipboard,\"" + name + "\"," + val + ") not yet implemented\n");
        }
    }
    store_attrs(["clipboard"], ["TEXT"], set_clipboard);

}

//function IupTreeGetUserId(tree, id) {
function gTreeGetUserId(tree, id) {
//DEV/DOC this is effectively a null-op on the web browser.
//  let leaf = tree.querySelector(`[data-tree-id="` + id + `"]`);
//  let /*integer*/ userid = leaf.getAttribute("data-tree-id");
//  if (userid!==id) { crash("userid error?"); }
//  return userid;
    return id;
}

//p2js [DEV/temp]
//function IupTreeAddNodes(/*Ihandle*/ tree, /*sequence*/ tree_nodes, /*integer*/ id=-1) {
//function IupTreeAddNodes(tree, tree_nodes, id=-1) {
function gTreeAddNodes(tree, tree_nodes, id=-1) {

    function createTVelem(tagname, classname, innerhtml) {
        let res = document.createElement(tagname);
        res.classList.add(classname);
        if (innerhtml) { res.innerHTML = innerhtml; }
//      if (innerhtml) { crash("oh, it *is* used!"); }
        return res;
    }
    if (tree==="createTVelem") { return createTVelem; } // for gTreeView

//p2js [DEV/temp]
//  function iupTreeSetNodeAttributes(leaf, /*sequence*/ attrs) {
//  function iupTreeSetNodeAttributes(leaf, attrs) {
    function xpg_TreeSetNodeAttributes(leaf, attrs) {
        // (internal routine, leaf should be an li)
        for (let i=1; i<=length(attrs); i += 2) {
            let /*string*/ name = attrs[i];
//          if (!find(name,["sequence","COLOR","STATE","TITLE","TITLEFONT","TOGGLEVALUE","TOGGLEVISIBLE",
//                            "USERDATA","IMAGE","IMAGEEXPANDED","MARKED"])) {
            if (!find(name,["sequence","STATE","USERDATA"])) {
                crash("invalid tree attribute")
            }
            let /*object*/ v = attrs[i+1];
            if (string(v)) {
//              IupSetAttributeId(tree, name, id, v);
                if (name==="STATE") {
                    if (v==="COLLAPSED") {
                        leaf.classList.add("closed");
                    } else if (v==="EXPANDED") {
                        leaf.classList.remove("closed");
                    } else {
                        crash("?9/0");
                    }
                } else {
                    crash("?9/0");
                }
            } else if (name==="USERDATA" && integer(v)) {
//              IupTreeSetUserId(tree, id, v); // (maybe?)
                leaf.setAttribute("data-tree-id",v);
            } else {
                crash("?9/0");
            }
        }
    }

//p2js [DEV/temp]
//  function iupTreeAddNodesRec(/*Ihandle*/ tree, /*sequence*/ tree_nodes, /*bool*/ bRoot=true) {
//  function iupTreeAddNodesRec(tree, tree_nodes, bRoot=true) {
    function xpg_TreeAddNodesRec(tree, tree_nodes, bRoot=true) {
        //
        // internal routine, the guts of gTreeAddNodes, less the initial clear
        // Here, tree is the immediate parent that we're adding the [subtree] tree_nodes to.
        // In the root case, tree is the ul, otherwise tree is an li and the ul we need to
        // add the new leaf (li) to is the (first) child ul of that, create one if needed.
        // Every node in the tree is basically an li/span/a triplet, with the latter two
        // being the first two children of the li.
        //
        let label = string(tree_nodes)?tree_nodes:tree_nodes[1],
            leaf = createTVelem("li", "leaf"),
            parentList = tree;
        leaf.appendChild(createTVelem("span","treeToggle"));
        leaf.appendChild(createTVelem("a", "leafLabel", label));
        if (!bRoot) {
            let list = tree.querySelector("ul");
            if (!list) {
                list = tree.appendChild(createTVelem("ul", "subtree"));
            }
            parentList = list;
        }
        parentList.appendChild(leaf);
        if (!string(tree_nodes)) {
//          let children = ["sequence"],
            let children,
//              lench = length(children),
                lench = 0,
//              lentn = length(tree_nodes);
                lentn = Math.min(length(tree_nodes),3);
            if (!atom(tree_nodes[lentn])) {
                children = tree_nodes[lentn];
                lench = length(children);
                leaf.classList.add(lench?"showExpander":"closed");
                leaf.classList.add("hasChildren");
            }
            for (let i=1; i<=lench; i += 1) {
                xpg_TreeAddNodesRec(leaf, children[i], false);
            }
            if (lentn===3) {
                xpg_TreeSetNodeAttributes(leaf, tree_nodes[2]);
            }
        }
    }

    function xpg_DeleteTreeChildren(node) {
        while (node.hasChildNodes()) {  
            node.removeChild(node.firstChild);
        }
    }

    if (id===-1) {
//      IupSetAttributeId(tree,"DELNODE",0,"ALL");  // (maybe...)
        xpg_DeleteTreeChildren(tree);
        xpg_TreeAddNodesRec(tree, tree_nodes, true);
    } else {
        // tree_nodes is actually just children
        let children = tree_nodes,
            lench = length(children);
        tree = tree.querySelector(`[data-tree-id="` + id + `"]`);
        let list = tree.querySelector("ul");
        if (list) { xpg_DeleteTreeChildren(list); }
        if (!lench) {
            tree.classList.remove("showExpander");
            tree.classList.add("closed");
        } else {
            for (let i=1; i<=lench; i += 1) {
                xpg_TreeAddNodesRec(tree, children[i], false);
            }
        }
    }
}

//p2js [DEV/temp]
//function IupTreeView(/*sequence*/ tree_nodes, /*atom*/ branchopen_cb=null, /*string*/ attributes="", /*sequence*/ args=[]) {
//function IupTreeView(tree_nodes, branchopen_cb=null, attributes="", args=[]) {
function gTreeView(tree_nodes, branchopen_cb=null, attributes="", args=[]) {
//
// Creates a tree from a recursive [callback] data structure.
//
// tree_nodes is as per gTreeAddNodes()
// branchopen_cb can be used for deferred loading, or be null if tree_nodes is complete.
// you can also set branchopen_cb later, via IupSetCallback(tree,"BRANCHOPEN_CB",Icallback("branchopen_cb")) [DEV]
//
    const createTVelem = gTreeAddNodes("createTVelem");
    let /*Ihandle*/ tree = createTVelem("ul", "tree");
//  let /*Ihandle*/ tree = gTreeAddNodes.createTVelem("ul", "tree"); // does not work...
    tree.setAttribute("EXPAND", "YES");

    function xpg_tree_clickHandler(event) {
        let leaf = event.target, // an a.leafLabel or span.treeToggle
            tcl = leaf.classList,
            pcl = leaf.parentNode.classList; // always an li.leaf
        if (tcl.contains("leafLabel")) {
            // was selectTreeLeaf()
            if (!pcl.contains("selected")) {
                let prev = tree.querySelector(".selected");
                if (prev) { prev.classList.remove("selected"); }
                pcl.add("selected");
                let select_cb = tree.select_cb;
                if (select_cb) {
                    select_cb(leaf.innerText, ", ye ha");
                }
            }
        } else if (tcl.contains("treeToggle")) {
            let it = leaf.nextSibling.innerText; // the a.leafLabel
            if (pcl.contains("closed")) {
                pcl.remove("closed");
                let open_cb = tree.open_cb;
                if (open_cb) {
                    open_cb(it, ", wowee"); //DEV (testing relic??)
                }
                let branchopen_cb = tree.BRANCHOPEN_CB;
                if (branchopen_cb) {
                    let id = leaf.parentNode.getAttribute("data-tree-id");
                    if (id) {
                        branchopen_cb(tree,id);
                    }
                }
            } else {
                pcl.add("closed");
                let close_cb = tree.close_cb;
                if (close_cb) {
                    close_cb(it, ", so there"); //DEV (testing relic??)
                }
            }
        }
    }
//  tree.addEventListener("click", xpg_tree_clickHandler);
    tree.onclick = xpg_tree_clickHandler;

/*
    tree.addEventListener("keydown", e => {
                const {code} = e;
                if (code === "ArrowUp" || code === "ArrowDown") {
                    this.navigate(code === "ArrowUp" ? "backward" : "forward");
                    e.preventDefault();
                }
            });
    navigate(direction = "forward") {
      const e = this.active();
      if (e) {
        const list = [...this.parent.querySelectorAll("a, summary")];
        const index = list.indexOf(e);
        const candidates = direction === "forward" ? list.slice(index + 1) : list.slice(0, index).reverse();
        for (const m of candidates) {
          if (m.getBoundingClientRect().height) {
            return this.select(m);
          }
        }
      }
    }
*/

    if (branchopen_cb) {
//DEV we (also) need to support this, btw:
//      IupSetCallback(tree, "BRANCHOPEN_CB",  branchopen_cb);
//      tree.branchopen_cb = branchopen_cb;
        tree["BRANCHOPEN_CB"] = branchopen_cb;
    }
    gTreeAddNodes(tree, tree_nodes);
//DEV gSetAttributes(tree,attributes,args); // maybe, MARKMULTIPLE
    return tree;
}

//DEV/SUG pwa/p2js() could perhaps suppress a trailing gMainLoop()....
//function gMainLoop() { crash("if platform()!=JS missing"); }
function gMainLoop() { return false; }


