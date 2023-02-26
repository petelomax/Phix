"use strict";
//
// pGUI.js
// =======
//
//  Emulates IUP in the browser, so (with a little help from p2js) we
//  can develop/test on the desktop and publish straight to the web.
//  Naturally it is, and will always be a subset of the Phix language
//  for obvious security reasons if nothing else, and if you have ever
//  glanced sideways at the pGUI docs there are obviously far too many
//  elements/containers/attributes, that this has to mimic one-by-one,
//  for this to ever get close to completion. Hopefully however this
//  will still allow us to do some interesting and useful things, and 
//  pretty much everywhere.
//
requires(JS);

const CD_AMBER = "#FFBF00",
      CD_BASE_CENTER = 1, // (=== CD_SOUTH)
      CD_BASE_LEFT = 7, // (=== CD_SOUTH_WEST)
      CD_BASE_RIGHT = 6, // (=== CD_SOUTH_EAST)
      CD_BLACK = "#000000",
      CD_BLUE = "#0000FF",
      CD_BOLD = 1,
      CD_BOLD_ITALIC = 3,
      CD_BOX = 4,
      CD_CENTER = 8,
      CD_CIRCLE = 2,
      CD_CLOSED_LINES = 2,
      CD_CONTINUOUS = 0,
      CD_CUSTOM = 5,
      CD_CYAN = "cyan",
      CD_DARK_BLUE = "#000080",
      CD_DARK_CYAN = "#008080",
      CD_DARK_GRAY = "#808080",
      CD_DARK_GREY = "#808080",
      CD_DARK_GREEN = "green",
      CD_DARK_MAGENTA = "#800080",
      CD_DARK_RED = "#800000",
      CD_DARK_YELLOW = "#EBEB00",
      CD_DASHED = 1,
      CD_DOTTED = 2,
      CD_DASH_DOT = 3,
      CD_DASH_DOT_DOT = 4,
      CD_DBUFFER = 2,
      CD_DEG2RAD = PI/180,
      CD_DIAMOND = 5,
      CD_EAST = 2,
      CD_EVENODD = 0,
      CD_FILL = 0,
      CD_GL = "CD_GL",
      CD_GRAY = "#C0C0C0",
      CD_GREY = "#C0C0C0",
      CD_GREEN = "#3cb44b",
      CD_HATCH = 1,
      CD_HOLLOW = 4,
      CD_HOLLOW_BOX = 7,
      CD_HOLLOW_CIRCLE = 6,
      CD_HOLLOW_DIAMOND = 8,
      CD_INDIGO = "#4B0082",
      CD_ITALIC = 2,
//    CD_IUP = 1,
      CD_IUP = "CD_IUP",
      CD_LIGHT_BLUE = "#4363d8",
      CD_LIGHT_GRAY = "#E4E4E4",
      CD_LIGHT_GREY = "#E4E4E4",
      CD_LIGHT_GREEN = "#00FF00",
      CD_LIGHT_PARCHMENT = "#FAF8EF",
      CD_MAGENTA = "#f032e6",
      CD_NAVY = "#000080",
      CD_NORTH = 0,
      CD_NORTH_EAST = 4,
      CD_NORTH_WEST = 5,
      CD_OLIVE = "#808000",
      CD_OPEN_LINES = 1,
      CD_ORANGE = "#FF8C00",
//    CD_PARCHMENT = "parchment",
      CD_PARCHMENT = "#FFFFE0",         IUP_PARCHMENT = CD_PARCHMENT,
      CD_PATTERN = 3,
      CD_PLAIN = 0,
      CD_PLUS = 0,
      CD_PURPLE = "#911eb4",
      CD_QUERY = -1,
      CD_RAD2DEG = 180/PI,
      CD_RED = "#FF0000",
      CD_SILVER = "#C0C0C0",  // (=== CD_GREY)
      CD_SOLID = 0,
      CD_SOUTH = 1,
      CD_SOUTH_EAST = 6,
      CD_SOUTH_WEST = 7,
      CD_STAR = 1,
      CD_STIPPLE = 2,
      CD_STRIKEOUT = 8,
      CD_UNDERLINE = 4,
      CD_VIOLET = "#EE82EE",
      CD_WEST = 3,
      CD_WHITE  = "#FFFFFF",
      CD_WINDING = 1,
      CD_X = 3,
      CD_YELLOW = "#FFFF00",
//DEV move to builtins/opengl.js... (along with much other code)
      GL_ARRAY_BUFFER = 0x8892,
      GL_INFO_LOG_LENGTH = 0x8B84,
      GL_SHADER_SOURCE_LENGTH = 0x8B88,
      GL_CLAMP = 0x2900,
      GL_CLAMP_TO_BORDER = 0x812D,
      GL_CLAMP_TO_EDGE = 0x812F,
      GL_COLOR_BUFFER_BIT = 0x00004000,
      GL_COMPILE_STATUS = 0x8B81, 
      GL_CULL_FACE = 0x0B44,
      GL_DEPTH_BUFFER_BIT = 0x00000100,
      GL_DEPTH_TEST = 0x0B71,
//    GL_DEPTH_TEST = 0,
      GL_FLOAT = 0x1406,
      GL_FRAGMENT_SHADER = 0x8B30,
      GL_LINEAR = 0x2601,
      GL_LINK_STATUS = 0x8B82,
      GL_MODELVIEW = 0x1700,
      GL_NO_ERROR = 0,
      GL_NEAREST = 0x2600,
      GL_PROJECTION = 0x1701,
      GL_REPEAT = 0x2901,
      GL_RGB = 0x1907,
      GL_RGBA = 0x1908,
      GL_STATIC_DRAW = 0x88E4,
      GL_TEXTURE_2D = 0x0DE1,
      GL_TEXTURE_MAG_FILTER = 0x2800,
      GL_TEXTURE_MIN_FILTER = 0x2801,
      GL_TEXTURE_WRAP_S = 0x2802,
      GL_TEXTURE_WRAP_T = 0x2803,
      GL_TRIANGLES = 4,
      GL_UNSIGNED_BYTE = 0x1401,
      GL_VERTEX_SHADER = 0x8B31,
      IUP_BUTTON1 = 0X31, /* '1' */
      IUP_BUTTON2 = 0X32, /* '2' */
      IUP_BUTTON3 = 0X33, /* '3' */
      IUP_CENTER = 0xFFFF, /* 65535 */
      IUP_CONTINUE  = -4,   
      IUP_CLOSE    = -3,
      IUP_DEFAULT = -2,
      IUP_IGNORE = -1,
      IUP_MASK_UINT = "/d+",
      IUP_MASK_INT = "[+/-]?/d+",
      IUP_MOUSEPOS = 0xFFFC, /* 65532 */
      K_BS = 0X08,
      K_CR = 0X0D,
      K_DEL = 0XFFFF,
      K_DOWN = 0xFF54,
      K_END = 0xFF57,
      K_ESC = 0X1B,
//    K_ESC = 0xFF1B,   (untried...)
      K_F1 = 0xFFBE,
      K_F2 = 0xFFBF,
      K_F3 = 0xFFC0,
      K_F4 = 0xFFC1,
      K_F5 = 0xFFC2,
      K_F6 = 0xFFC3,
      K_F7 = 0xFFC4,
      K_F8 = 0xFFC5,
      K_F9 = 0xFFC6,
      K_F10 = 0xFFC7,
      K_F11 = 0xFFC8,
      K_F12 = 0xFFC9,
      K_HOME = 0xFF50,
      K_INS = 0xFF63,
      K_LEFT = 0xFF51,
      K_MIDDLE = 0xFF0B,
      K_PGDN = 0xFF56,
      K_PGUP = 0xFF55,
      K_RIGHT = 0xFF53,
      K_SP = 0X20,
      K_TAB = 0X09,
      K_UP = 0xFF52,
      K_a = 0X61,       // 'a' / 97
      K_b = 0X62,       // 'b' / 98
      K_c = 0X63,       // 'c' / 99
      K_d = 0X64,       // 'd' / 100
      K_e = 0X65,       // 'e' / 101
      K_f = 0X66,       // 'f' / 102
      K_g = 0X67,       // 'g' / 103
      K_h = 0X68,       // 'h' / 104
      K_i = 0X69,       // 'i' / 105
      K_j = 0X6A,       // 'j' / 106
      K_p = 0X70,       // 'p' / 112
      K_r = 0X72,       // 'r' / 114
      K_s = 0X73,       // 's' / 115
      K_cA = 1,
      K_cC = 3,
      K_cD = 4;

//DEV/SUG split into/manage both handlers and valid/supported values?
let $storeAttr = {};    // element-specific attribute handlers/setters

function $pxFloat(s) { return (s === "auto") ? 0 : parseFloat(s.replace("px", "")); }
function $eHeight(elem) { return $pxFloat(getComputedStyle(elem).height); }
function $eWidth(elem) { return $pxFloat(getComputedStyle(elem).width); }

// ACTIVE, EXPAND+, FLOATING+, SCROLLBAR+, VISIBLE, FLATSCROLLBAR?, SHOWARROWS, SHOWFLOATING (OK, from now on only ones I have used...)
// UTF8MODE?, CANFOCUS, FLAT, BORDER, TOGGLE, VALUE+, REDRAWALL, UNDERLINE, ELLIPSIS, WORDWRAP, AUTOHIDE, DROPDOWN, DROPEXPAND,
// EDITBOX, MULTIPLE, SHOWDROPDOWN, SHOWIMAGE, SORT?, AUTOTOGGLE, DASHED, FORMATTING, MULTILINE, OVERWRITE, READONLY, SPIN,
// SPINWRAP, SPINAUTO, RADIO, RIGHTBUTTON, VALUE? (toggle), 3STATE, CHECKRIGHT, SHOWTICKS, EXPANDCHILDREN, FLOATING, SHRINK,
// TOPMOST, SUNKEN, SHOWCLOSE, TABVISIBLEn, ...
//read-only: HASFOCUS, HIGHLIGHTED, PRESSED, RADIO, HIGHLIGHT, MODAL, ACTIVEWINDOW, 
//SUG: function $to_int(val, strs=["YES","NO"], vals=[true,false]) {
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

function IupOpen() {

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

function IupSetGlobal(name, v) {
    if (name === "UTF8MODE") {
        // do nothing... (it is already/permanently enabled in JavaScript)
    } else if (name === "SINGLEINSTANCE") {
        puts(1,"IupSetGlobal(" + name + "," + v + ")...\n");
    } else if (name === "INPUTCALLBACKS") {
        puts(1,"IupSetGlobal(" + name + "," + v + ")...\n");
    } else {
        crash("IupSetGlobal(" + name + "," + v + ")...");
    }
}
const IupSetGlobalInt = IupSetGlobal;

function IupGetGlobal(/*string*/ name) {
    if (name === "DLGBGCOLOR") {
        return "240 240 240";
    }
    crash("IupGetGlobal(" + name + ")...");
}

function IupGetGlobalInt(/*string*/ name) {
    if (name === "CONTROLKEY") { return $ctrlKey; }
    if (name === "SHIFTKEY") { return $shiftKey; }
    if (name === "UTF8MODE") { return true; }
    crash("IupGetGlobalInt(%s) not supported",["sequence",name]);
}

function IupGetGlobalIntInt(/*string*/ name) {
    if (name === "SCREENSIZE") { return ["sequence",window.innerWidth,window.innerHeight]; }
    crash("IupGetGlobalIntInt(%s) not supported",["sequence",name]);
}

function IupSetGlobalFunction(name, v) {
    if (name === "IDLE_ACTION") {
        function cb() {
            let res = v();
            if (res === IUP_DEFAULT) {
                window.requestIdleCallback(cb); 
            }
        }
        window.requestIdleCallback(cb);
    } else {
        crash("IupSetGlobalFunction(" + name + ")...");
    }
}

function $timer(/*string*/cmd, ih, name, v) { // nb: mapped to ("create",func,msecs,active)
    // timer handling, used by IupTimer(), IupSetInt(), and IupGetInt()
    // (bundled like this mainly just so that the next enum can be local)
    const TIMER=0, FUNC=1, MSECS=2, ACTIVE=3, ID=4, CB=5
    if (cmd === "create") {
        function action_cb(timer) {
            if (!timer[ACTIVE]) { 
                if (timer[ID]) {
                    // shouldn't really happen... not very often anyway
                    clearInterval(timer[ID]);
                    timer[ID] = 0;
                }
            } else {
                let fn = timer[FUNC],
                    res = fn(timer);
    //          if (res === IUPCLOSE) { uh?? [now documented as not supported] }
            }
        }
        //function IupTimer(/*cbfunc*/ func=NULL, /*integer*/ msecs=0, /*boolean*/ active=true) {
        //  return $timer("create",func,msecs,active);
        let func = ih, msecs = name, active = v; // map args to more sensible names
        ih = ["timer",func,msecs,active,0,action_cb];
        if (active) {
            ih[ID] = setInterval(action_cb,msecs,ih);
        }
        return ih;

    } else if (cmd === "is") {
        return Array.isArray(ih) && ih[TIMER] === "timer";
    } else if (cmd === "set") {
        if (ih[ID]) {
            clearInterval(ih[ID]);
            ih[ID] = 0;
        }
        if (name === "RUN") {
            ih[ACTIVE] = v;
        } else if (name === "TIME") {
            ih[MSECS] = v;
        } else {
            crash("timer??");
        }
        if (ih[ACTIVE]) {
            ih[ID] = setInterval(ih[CB],ih[MSECS],ih);
        }
    } else if (cmd === "get") {
        if (name === "TIME") { return ih[MSECS]; }
        if (name === "RUN") { return ih[ACTIVE]; }
        crash("timer??")
    }
}   

//(Ihandle ih, string name, nullable_string val, sequence args={}) 
function IupSetStrAttribute(ih, name, v, args = []) {
    if ($timer("is",ih)) {
        if (args.length) {
            v = sprintf(v, args);
        }
        v = $to_bool(v);
        $timer("set",ih,name,v);
        return;
    }
//  let t = ih.className;
    let t = ih.classList[0];
//  let t = $class_name(ih);
    if (!t || !$storeAttr.hasOwnProperty(t)) {
        crash("invalid type");
    }
    if (typeof(name) === "string") {
        if (args.length) {
            v = sprintf(v, args);
        }
        let bOK = $storeAttr[t].hasOwnProperty(name);
        if ((!bOK) && (t === "list")) {
            // map "1", "2", etc. to ("<list_id>",[n,v]):
//DEV define almost like this in p2js.js and exclude from transpilation, for use in [s]printf()?
//          let n = to_integer(name);
            let l = name.length, n = 0;
            for (let i = 0; i < l; i += 1) {
                let d = name.codePointAt(i) - 0X30; // (ie ch-'0')
                if ((d > 9) || d < (i === 0)) { // (not leading 0)
                    n = 0;
                    break;
                }
                n = n*10 + d;
            }
            if (n > 0) {
                bOK = true;
                v = [n,v]; // (set_list() does matching desequence)
                name = "<list_id>";
            }
        }
        if (bOK) {
            let fn = $storeAttr[t][name];
            if (typeof(fn) !== "function") {
                crash("invalid fn");
            }
            fn(ih, name, v);
            return;
        }
    }
    crash("invalid attr name (%s for %s)",["sequence",name,t]);
}

const IupStoreAttribute = IupSetStrAttribute;
const IupSetAttribute = IupSetStrAttribute;

function IupSetAttributeId(ih, name, id, v) {
    let t = ih.classList[0];
    if (t === "list" && name === "") {
        name = "<list_id>"
        let fn = $storeAttr[t][name];
        v = [id,v]; // (set_list() does matching desequence)
        fn(ih, name, v);
        return;
    }
    printf(1,"IupSetAttributeId(%s,\"%s\",%d,%V)??\n",["sequence",t,name,id,v]);
//  ih[name] = v;
}

function IupSetInt(ih, name, v) {
    if (name!="GRIDCOLOR") {
        let tv = typeof(v);
        if (tv !== "number" && 
            tv !== "boolean" && 
            tv !== "function") {
            crash("IupSetInt("+name+","+tv+")??");
        }
    }
    if ($timer("is", ih)) {
        $timer("set", ih, name, v);
    } else if (sequence(ih)) {
        for (let i=length(ih); i >= 1; i -= 1) {
            IupSetAttribute(ih[i], name, v);
        }
    } else {
//      IupSetStrAttribute(ih,name,v?"YES":"NO");
        IupSetStrAttribute(ih, name, v);
    }
}

function IupGetAttribute(ih, name, dflt) {
//  if (string(ih) && ih === "clipboard") {
//      if (name !== "TEXT") { crash("uh?"); }
//      puts(1,"IupGetAttribute(clipboard,`TEXT`)==>``??\n");
////        let text = window.clientInformation.clipboard.readText(); // ugh, a /Promise/...
////        let text = window.clientInformation.clipboard.read();
////        return text;
//      return "";
//  }
    let t = ih.classList[0];
    if (name === "VALUE") {
        if (t === "text" ||
            t === "datepick") { // nb eg 2021-11-05 (for docs)
            return ih.value;
        } else if (t === "list") {
            let children = ih.childNodes,
                l = children.length;
            for (let i=0; i<l; i += 1) {
                if (children[i].selected) {
                    return children[i].text;
                }
            }
            return "";
        }
    } else if (name === "TITLE") {
        if (t === "button" ||
            t === "toggle" ||
            t === "label" ||
            t === "menuitem") {
            return ih.innerText;
        } else if (t === "frame") {
            return ih.childNodes[0].innerText;
        } else if (t === "hbox" ||
//      } else if (t === "hbox" ) {
                   t === "canvas") {
            return IupGetAttribute(ih.parentNode,name);
        }
//  } else if (t === "dialog") {
    } else if (name === "GTITLE" ||
               name === "XNAME" ||
               name === "YNAME" ||
               name === "MODE" ||
               name === "MARKSTYLE" ||
               name === "LEGENDBOX") {
        if (t === "canvas" ) {
            if (!ih[name]) { return dflt; }
            return ih[name];
        }
    } else if (name === "RASTERSIZE") {
//      if (name === "RASTERSIZE") {
//          return ["sequence", $eWidth(ih), $eHeight(ih)];
        return sprintf("%dx%d",["sequence",$eWidth(ih), $eHeight(ih)]);
//      }
    } else if (name === "TEXT") {
        if (t === "clipboard" ) {
//          puts(1,"IupClipboard(TEXT,"+val+")??\n"); // placeholder
//          ih.value = "paste";
            ih.value = "1";
            document.body.appendChild(ih);
            ih.style.display = "block";
            ih.select();
            ih.focus();
//DEV does not work:
//          document.execCommand("copy");
//                  if (document.execCommand !== undefined && document.execCommand("copy") !== true) {
//                      throw new Error(`Unsupported "copy" clipboard command`);
//                  }
//                  if (document.execCommand !== undefined && document.execCommand("paste", null, null)) {
//                      return clip.value;
            let res = document.execCommand("paste",null,null);
            document.body.removeChild(ih);
            return ih.value;
// "illegal invocation"...
//          return window.Clipboard.prototype.readText();
        }
    }
    if (ih.hasOwnProperty(name)) { // (DBUFFER etc)
        return ih[name];
//  } else if (t === "canvas" && dflt &&
//             (name === "XTICTFMT" ||
//              name === "YTICTFMT") {
    } else if (dflt) {
        return dflt
    }
    crash("IupGetAttribute(%s,%s) not supported",["sequence",t,name]);
}   

function IupSetAttributePtr(ih, name, v) {
    ih[name] = v;
}

// 28/4/22 (may have jumped the gun.. [while updating the docs, rather than actually using])
//let IupGetAttributePtr = IupGetAttribute; // NO!!
function IupGetAttributePtr(ih, name) {
    return ih[name];
}

function IupGetAttributeId(ih, name, id) {
    let t = ih.classList[0];
    if (t === "list") {
        if (name === "") {
            let n = ih.childElementCount;
            if (id<=n) {
                let childoption = ih.children[n-1],
                    t = childoption.childNodes[0];
                return t.textContent;
            }
            crash("uh?");
//          return "";
        }
    }
    crash("IupGetAttributeId(%s,%s,%d) not supported",["sequence",t,name,id]); // placeholder
}   

function IupGetInt(ih, name, dflt=0) {
    if (ih === 0) {
        return IupGetGlobalInt(name);
//      if (name === "CONTROLKEY") { return $ctrlKey; }
//      if (name === "SHIFTKEY") { return $shiftKey; }
//      crash("IupGetInt(NULL,%s) not supported",["sequence",name]);
    } else if ($timer("is",ih)) {
        return $timer("get",ih,name);
    }
    let t = ih.classList[0];
    if (t === "button") {
        if (name === "ACTIVE") {
            return !ih.disabled;
//DEV...
        } else if (name === "RUNNING") {
            return ih[name];
        }
    } else if (t === "text") {
        if (name === "VALUE") {
            if (ih.value === "") { return 0; }
            return parseInt(ih.value);
        }
    } else if (t === "label") {
        if (name === "TITLE") {
            if (ih.innerText === "") { return 0; }
            return parseInt(ih.innerText);
        }
    } else if (t === "toggle") {
        if (name === "VALUE") {
//          val = $to_bool(val);
//DEV to_bool not in scope...? (is innerText actually so???) [IupToggle is ~1% implemented, if that]
//          let val = ih.innerText;
//          if (string(val)) { val = (val.toUpperCase()==="YES"); }
//8/4/22 (for Spinning Rod Animation)
//          return ih.innerText.toUpperCase() === "YES";
            return ih.childNodes[1].checked;
        }
    } else if (t === "canvas") {
        if (name === "DATA" ||
            name === "DRID" ||
            name === "GRID" ||
            name === "GRIDCOLOR" ||
            name === "LEGENDBOX" ||
            name === "POSX" ||
            name === "POSY" ||
            name === "TITLESTYLE" ||
            name === "XANGLE" ||
            name === "XCROSSORIGIN" ||
            name === "XMARGIN" ||
            name === "XMAX" ||
            name === "XMIN" ||
            name === "XRID" ||
            name === "XTICK" ||
            name === "XTICKFMT" ||
            name === "XYSHIFT" ||
            name === "YANGLE" ||
            name === "YCROSSORIGIN" ||
            name === "YMARGIN" ||
            name === "YMAX" ||
            name === "YMIN" ||
            name === "YRID" ||
            name === "YTICK" ||
            name === "YTICKFMT" ||
            name === "YXSHIFT") {
            return ih[name] || dflt;
        }
    } else if (t === "list") {
        if (name === "VALUE") {
            let children = ih.childNodes,
                l = children.length;
            for (let i=0; i<l; i += 1) {
                if (children[i].selected) {
                    return i+1;
                }
            }
            return 0;
        } else if (name === "COUNT") {
            return ih.childElementCount;
        }
    } else if (t === "dialog") {
        if (name === "VISIBLE") {
//          return !ih.hidden;
            return ih.isConnected;
        }
    } else if (t === "slider") {
        if (name === "VALUE") {
            return ih.valueAsNumber;
        }
    }
    crash("IupGetInt(%s,%s) not supported",["sequence",t,name]);
}   

function IupToggleInt(/*Ihandle*/ ih, /*string*/ name) {
    IupSetInt(ih,name,!IupGetInt(ih,name))
}

function IupGetIntInt(ih, name) {
//  if (typeof(name) !== "string" ...??
    let t = ih.classList[0];
    if (name == "RASTERSIZE") {
        if (t === "canvas" ||
            t === "dialog") {
//erm...
//          const rect = ih.getBoundingClientRect(),    // (nb: recalc in DOM)
//                w = rect.width,
//                h = rect.height;
//          crash("RASTERSIZE expected!");
//          return ["sequence",w,h];
//          return ["sequence",ih.width,ih.height];
            return ["sequence",ih.clientWidth,ih.clientHeight];
//dialog:
//          if (w) { ih.style.width = w + "px"; }
//          if (h) { ih.style.height = h + "px"; }
//canvas:
//          if (w) { ih.width = w; }
//          if (h) { ih.height = h; }
        }
        if (t === "button" ||
            t === "label") {
//DEV all 0...
//          let cse = getComputedStyle(ih);
//          return ["sequence",$pxFloat(cse.width),$pxFloat(cse.height)];
            let cse = ih.getBoundingClientRect();
            return ["sequence",cse.width,cse.height];
        }
    } else if (t === "canvas") {
        if (name === "DRAWSIZE") {
            return ["sequence",ih.width,ih.height];
        }
    } else if (t === "dialog") {
        if (name === "SCREENPOSITION") {
            return ["sequence",ih.offsetLeft,ih.offsetTop];
        }
    }
//  if (!t || !$storeAttr.hasOwnProperty(t)) {
//      crash("invalid type");
//  }
/*
    let val = ih[name];
//aw bollocks, it's a *lot* more involved than this...
//  if (!val || $storeAttr[t].hasOwnProperty(name)) {
//      val = $storeAttr[t][name];
//  }
    if (typeof(val) === "string") {
        // convert eg "225x75" to ["sequence",225,75]
        let x = val.indexOf('x');
        if (x) {
            let y = Number(val.slice(x+1));
                x = Number(val.slice(0,x));
            if (Number.isInteger(x) &&
                Number.isInteger(y)) {
                val = ["sequence",x,y];
            }
        }
    }
    if (!sequence(val) || length(val)!==2) {
        crash("invalid IupGetIntInt value");
//or maybe just:
//      val = ["sequence",0,0];
    }
    return val;
*/
    crash("IupGetIntInt(%s,%s) not supported",["sequence",t,name]);
}

function IupGetDouble(ih, name, dflt=0) {
    let t = ih?ih.classList[0]:"NULL";
    if (ih === NULL) {
        if (name === "SCREENDPI") {
            return window.devicePixelRatio * 96;
        }
    } else if (t === "canvas") {
        if (name === "XTICK" ||
//          name === "DATA" ||
//          name === "DRID" ||
            name === "XANGLE" ||
//          name === "XCROSSORIGIN" ||
            name === "XMARGIN" ||
            name === "XMAX" ||
            name === "XMIN" ||
            name === "XYSHIFT" ||
            name === "YANGLE" ||
//          name === "YCROSSORIGIN" ||
            name === "YMARGIN" ||
            name === "YMAX" ||
            name === "YMIN" ||
            name === "YTICK" ||
            name === "YXSHIFT") {
            return ih[name] || dflt;
        }
    } else if (t === "slider") {
        if (name === "VALUE") {
            return Number(ih.value);
        }
    }
    crash("IupGetDouble(%s,%s) not supported",["sequence",t,name]);
}

function IupSetDouble(ih, name, v) {
    let t = ih?ih.classList[0]:"NULL";
//  if (t === "slider") {
    if (t === "progress") {
        if (name === "VALUE") {
            ih.value = v;
            return;
        }
    } else if (t === "canvas") {
        if (name === "XTICK" ||
            name === "YTICK") {
            ih[name] = v;
            return;
        }
    }
    crash("IupSetDouble(%s,%s,%V) not supported",["sequence",t,name,v]);
}

// translated from iup_attrib.c:
function IupSetAttributes(ih, attributes, args = []) {
    if (attributes && typeof(attributes) === "string" && attributes.length>0) {
        let i = 0;
        let token = "";
        const IUPLEX_TK_END   = 0,
              IUPLEX_TK_SET   = 1,
              IUPLEX_TK_COMMA = 2,
              IUPLEX_TK_NAME  = 3;

        function iAttribCapture(delims) {
            token = "";
            while (i < attributes.length) {
                let c = attributes[i];
                i += 1;
                if (delims.indexOf(c) !== -1) {
                    return;
                }
                token += c;
            }
            i += 1;
        }

        function iAttribToken() {
            while (i < attributes.length) {
                let c = attributes[i];
                i += 1;
                switch (c) {
                case 0:
                    return IUPLEX_TK_END;

                case '#':
                case '%':
                    // Skip comment
//                  while (i < attributes.length && attributes[i++] !== '\n') {}
                    while (i < attributes.length) {
                        c = attributes[i];
                        i += 1;
                        if (c === '\n') { break; }
                    }
                    break;

                case ' ':
//              case '\t':
//              case 0X9:
                case '\n':
                case '\r':
                case '\f':
//              case '\v':
                    break;

                case '=':
                    return IUPLEX_TK_SET;

                case ',':
                    return IUPLEX_TK_COMMA;

//              case '\"':                                  // string
                case '"':                                   // string
                    iAttribCapture("\"");
                    return IUPLEX_TK_NAME;

                default:
                    if (c > " ") {                          // identifier
                        i -= 1;                             // unget first character of env_buffer
//                      iAttribCapture("=, \t\n\r\f\v");    // get env_buffer until delimiter
//                      iAttribCapture("=, \t\n\r\f");      // get env_buffer until delimiter
                        iAttribCapture("=, \n\r\f");        // get env_buffer until delimiter
                        i -= 1;                             // unget delimiter
                        return IUPLEX_TK_NAME;
                    }
                } // (end switch)
            } // (end while)
            return IUPLEX_TK_END;
        }

        let name = null;
        let val = null;
        let get_name = true; // (else get_value)
        let bEnd = false;
        if (args.length>0) {
            attributes = sprintf(attributes,args)
        }

//      for (;;) {
        while (true) {
            switch (iAttribToken()) {
            case IUPLEX_TK_END:         // same as IUPLEX_TK_COMMA
                bEnd = 1;
            case IUPLEX_TK_COMMA:
                if (name) {
                    if (sequence(ih)) {
                        for (let i=length(ih); i >= 1; i -= 1) {
                            IupStoreAttribute(ih[i], name, val);
                        }
                    } else {
                        IupStoreAttribute(ih, name, val);
                    }
//                  let s = "IupStoreAttribute(ih, \"" + name + "\", \"" + val + "\")<br>";
//                  document.body.insertAdjacentHTML("beforeend", s);
//                  $docBody.insertAdjacentHTML("beforeend", s);
                }
                if (bEnd) { return; }
                name = null;
                val = null;
                get_name = true;
                break;

            case IUPLEX_TK_SET:
                get_name = false;
                break;

            case IUPLEX_TK_NAME:
                if (get_name) {
                    name = token;
                } else {
                    val = token;
                }
                break;
            } // (end switch)
        }
//  } else {
    } else if (attributes === null || typeof(attributes) !== "string") {
        crash("attributes not string");
    }   
}

function IupSetAttributeHandle(/*Ihandln*/ ih, /*string*/ name, /*Ihandln*/ ih_named) {
    // NB currently only handles dialog/MENU..
    if (name === "IMAGE") {
//      inputDataUrl.value = dataUrl;
//      imagePreview.src = dataUrl;
//      ih.value = ih_named;
//      ih.src = ih_named;
//      imagePreview.style.display = "block";
//      ih.style.display = "block";
//  imagePreview.style.maxWidth = `${inputWidth.value}px`;
//      ih.style.maxWidth = `32px`;
        let img = new Image();
        img.src = ih_named;
        ih.appendChild(img);
        return;
    }
    let t = ih?ih.classList[0]:"NULL",
        n = ih_named?ih_named.classList[0]:"NULL";
    if (t === "dialog" && name === "MENU" && n === "submenu") {
        // ih_named is a ul.submenu...
        const mh = document.createElement("div"),
              mb = document.createElement("div"),
             nav = document.createElement("nav");
//<div id="mobbtn">&#9776;</div>
//<nav id="menu"><ul class="menubar">
//      mh.setAttribute("class", "menuheader");
        mh.className = "menuheader";
//      mh.id = "menuheader";
        mb.id = "mobbtn";
        mb.innerHTML = "&#9776;";
        nav.id = "menu";
        nav.appendChild(ih_named);
        mh.appendChild(mb);
        mh.appendChild(nav);
        if (ih_named.className !== "submenu") { crash("uh?"); }
//      ih_named.setAttribute("class", "menubar");
        ih_named.className = "menubar";
        let l = ih_named.children.length;
        for (let i = 0; i < l; i += 1) {
            let ci = ih_named.children[i],
                cn = ci.className;
            // quick sanity check:
            assert(cn === "nestmenu" || cn === "menuitem" );
            ci.className = "topmenu";
        }
        let dlgbod = ih.querySelector(".dialog-body");
        dlgbod.insertAdjacentElement("afterbegin", mh);
        // Open sub-menu on click/hover
        nav.mstack = [];
        function close_to(mitem,clicked) {
            //
            // For an explanation, run demo/pGUI/submenu.exw
            // (on the desktop or in the browser!) and open
            // Edit/Create/Triangle. They should be "sticky"
            // and remain as you move the mouse around over 
            // the rest of the window/document. Sneak back 
            // onto File or Edit/Copy and several menus are 
            // hidden in one go, and one new menu opened.
            //
            // Called by click(topmenu),
            //           enter(topmenu/nestmenu/menuitem),
            //           click(doc).
            //
            let mstack = nav.mstack,
                wasmlen = mstack.length,
                p = mitem?.parentNode.parentNode;
            while (mstack.length &&
                   mstack[mstack.length-1]!==p) {
                mstack.pop().classList.remove("open");
            }
            if (mitem!==null && (clicked || wasmlen)) {
                mitem.classList.toggle("open");
                if (mitem.classList.contains("open")) {
                    mstack.push(mitem);
                }
            }
        }
        function li_click(event,mitem) {
            // (m is an element from ml)
            //event.preventDefault(); // avoids default right click menu (no help...)
            event.stopPropagation(); // important!
            close_to(mitem,true);
        }
        let ml = ih_named.querySelectorAll(".topmenu");
        for (let i = 0; i < ml.length; i += 1) {
            let m = ml[i];
            m.addEventListener("click", (event) => li_click(event,m), false);
            m.addEventListener("mouseenter", () => { close_to(m,false); }, false);
        }
        let nm = ih_named.querySelectorAll(".nestmenu, .menuitem");
        for (let i = 0; i < nm.length; i += 1) {
            let m = nm[i];
            m.addEventListener("mouseenter", () => { close_to(m,false); }, false);
        }
        function hide_menu() {
            close_to(null,false);
            // alt-keys (mush === Menu Underline SHow, topmenu only)
            const mush = ih_named.querySelectorAll(".mush");
            mush.forEach((item) => { item.classList.remove("mush"); });
        }
        document.addEventListener("click", hide_menu);
        document.addEventListener("contextmenu", hide_menu);

//      let menu = ih_named.getElementById("menu");
//      menu.addEventListener("contextmenu", (event)=> {event.preventDefault(); event.stopPropagation(); return false; });
        nav.addEventListener("contextmenu", (event)=> {event.preventDefault(); event.stopPropagation(); return false; });

        function window_resize() {
            hide_menu();
            //DEV/SUG calc the 640 from sum of toplevel sizes...
            if (window.innerWidth < 140) {
                nav.classList.add("mobile");
            } else {
                nav.classList.remove("mobile");
            }
        }   
        window_resize();
        ih.addEventListener("resize", window_resize);

//      let me = ih_named.querySelectorAll(".topmenu > span");

        function listenKeys(event) {
            var key = event.keyCode;
            switch (key) {
//            case 16: // Shift
//            case 17: // Ctrl
              case 18: // Alt (with shift) or AltGr
                        const malt = document.querySelectorAll(".malt");
                        // (mush === Menu Underline SHow, topmenu only)
                        malt.forEach((item) => { item.classList.toggle("mush"); });
//                      me[0].focus();
                        ih_named.querySelector(".topmenu > span").focus();
                        event.preventDefault();
                        break;
              case 27: // Escape
                        hide_menu();
                        break;
//            case 37: // left
//            case 38: // up
//            case 39: // right
//            case 40: // down
            }
//console.log(key);
//http://keycode.info/
        }
//window.onkeydown = listenKeys;
        ih.addEventListener("keydown", listenKeys)

        // TOGGLE SLIDE MOBILE MENU
//      let mobbtn = ih_named.querySelector("#mobbtn");
//      ==> mb
        function mobbtn_click() {
            let mobile = ih.querySelector(".mobile");
            if (mb.classList.contains("active")) {
                mb.classList.remove("active");
                mb.innerHTML = "&#9776;";
                if (mobile) {
                    mobile.style.right = "-220px";
                }
                mb.style.right = "0";
            } else {
                mb.classList.add("active");
                mb.innerHTML = "&#9587;";
                if (mobile) {
                    mobile.style.right = "0";
                }
                mb.style.right = "220px";
            } 
        }
        mb.addEventListener("click", mobbtn_click);

//DEV is this not the same as/should be in doc_click??
//      let content = document.querySelector(".content");
//$(".content").on("click", function() { 
        function content_click() {
            if (mb.classList.contains("active")) {
                mb.classList.remove("active");
                mb.innerHTML = "&#9776;";
                let mobile = document.querySelector(".mobile");
                if (mobile) {
                    mobile.style.right = "-220px";
                }
                mb.style.right = "0";
            } 
        }
//      content.addEventListener("click", content_click);
        window.addEventListener("click", content_click);
//      let h = $eHeight(ih);
        let h = $pxFloat(ih.style.height);
        ih.style.height = (h+21) + "px";
    } else if (t === "NULL" && name === "PARENTDIALOG") {
        // (not something we have to worry about in a browser...)
    } else {
        puts(1,"IupSetAttributeHandle(" + t +",\"" + name + "\"," + n +")??\n");
    }
}

function iup_XkeyBase(c) { return c; }

function iup_isdouble(/*atom*/ pStatus) {
// double click
//  return peek(pStatus+5)='D'
    puts(1,"iup_isdouble\n");
    return false;
}

function iup_isprint(/*atom*/ c) {
    return c>31 && c<127;   // K_SP..K_tidle
}

function iupKeyCodeToName(/*atom*/ ch) {
//  atom pKeyName = c_func(xiupKeyCodeToName,{ch})
//  return peek_string(pKeyName)
    if (ch === K_F5) {
        return "F5";
    }
    crash("iupKeyCodeToName(" + ch + ")\n");
}

function IupSetCallback(ih, name, func) {
    if (sequence(ih)) {
        let l = length(ih);
        for (let i=1; i<=l; i += 1) {
            let /*Ihandle*/ ihi = ih[i];
            IupSetCallback(ihi,name,func);
        }
    } else {
        let cn = ih.classList[0];
        ih[name] = func;
        function mapkey(event) {
            // (common code for KEY_CB and text/ACTION)
            let key = event.key;
//          $shiftKey = event.shiftKey; // for IupGetInt()
//puts(1,"shiftKey:"+shiftKey+"\n");
            if (string(key) && key.length === 1) {
                key = key.codePointAt(0);
                if (event.ctrlKey) {
                    if (key>=65 && key<=90) { // A..Z
                        key -= 64;
                    } else if (key>=97 && key<=122) { // a..z
                        key -= 96;
                    }
                }
            } else if (key == "Backspace") {
                key = K_BS;
            } else if (key == "Delete") {
                key = K_DEL;
            } else if (key == "Enter") {
                key = K_CR;
            } else if (key == "Shift") {
                return true;
            } else if (key == "F1") {
                key = K_F1;
            } else if (key == "F2") {
                key = K_F2;
            } else if (key == "F3") {
                key = K_F3;
            } else if (key == "F4") {
                key = K_F4;
            } else if (key == "F5") {
                key = K_F5;
            } else if (key == "F6") {
                key = K_F6;
            } else if (key == "F7") {
                key = K_F7;
            } else if (key == "F8") {
                key = K_F8;
            } else if (key == "F9") {
                key = K_F9;
            } else if (key == "F10") {
                key = K_F10;
            } else if (key == "F11") {
                key = K_F11;
            } else if (key == "F12") {
                key = K_F12;
            } else if (key == "PageDown") {
                key = K_PGDN;
            } else if (key == "PageUp") {
                key = K_PGUP;
            } else if (key == "ArrowUp") {
                key = K_UP;
            } else if (key == "ArrowDown") {
                key = K_DOWN;
            } else if (key == "ArrowLeft") {
                key = K_LEFT;
            } else if (key == "ArrowRight") {
                key = K_RIGHT;
            } else {    
                console.log("key is " + key);
            }
            return key;
        }
        if (name==="KEY_CB" || name==="K_ANY") {
            function key_cb(event) {
//puts(1,"key_cb("+event.keyCode+")\n");
                let tgt = event.currentTarget,
                    key = mapkey(event);
//19/10/21:
//              let res = func(tgt,key);
                let res = func(ih,key);
                // IUP_DEFAULT and IUP_IGNORE 
                // should prevent propagation,
                // am not going to bother with
                // IUP_CLOSE handling here:
//7/5/22:
//              if (res !== IUP_DEFAULT) {
                if (res !== IUP_DEFAULT &&
                    res !== IUP_CONTINUE) {
                    event.preventDefault();
                }
                return (res === IUP_CONTINUE);
            }
//          ih.onkeydown = key_cb;
//          ih.addEventListener("keydown",key_cb);
//          document.addEventListener("keydown",key_cb);
            $docBody.addEventListener("keydown",key_cb);
//          ih.tabIndex="-1";   // important!
        } else if (name=="MOTION_CB") {
            function motion_cb(event) {
//puts(1,"motion_cb("+event.clientX+","+event.clientY+")\n");
                let ctrl = event.currentTarget,
                    parent = IupGetDialog(ctrl),
                    x = event.clientX - ctrl.offsetLeft - parent.offsetLeft,
                    y = event.clientY - ctrl.offsetTop - parent.offsetTop;
                func(ctrl,x,y,NULL);
            }
            ih.addEventListener("mousemove",motion_cb);
        } else if (name=="BUTTON_CB") {
            function button_cb(event) {
                const buttons = [IUP_BUTTON1,IUP_BUTTON2,IUP_BUTTON3];
                let code = buttons[event.button],
                    ctrl = event.currentTarget,
                    parent = IupGetDialog(ctrl),
                    x = event.clientX - ctrl.offsetLeft - parent.offsetLeft,
                    y = event.clientY - ctrl.offsetTop - parent.offsetTop,
//et = event.type,
//                  pressed = 0; // (not pressed)
                    pressed = (event.type==="mousedown");
                func(ctrl,code,pressed,x,y,NULL);
            }
//          ih.addEventListener("click",button_cb);
            ih.addEventListener("mousedown",button_cb);
            ih.addEventListener("mouseup",button_cb);
//          ih.addEventListener("contextmenu",button_cb);
        } else if (name=="VALUECHANGED_CB") {
            function change_cb(event) {
                func(event.currentTarget);
            }
//21/10/21: (for guessthenumber)
            if (cn === "datepick" ||
                cn === "list" ||
                cn === "toggle") {
                ih.addEventListener("change",change_cb);
            } else if (cn === "slider") {
                ih.addEventListener("input",change_cb);
            } else {
                ih.addEventListener("keyup",change_cb);
            }
        } else if (name=="ACTION") {
            if (cn === "button" ||
                cn === "menuitem") {
                // action with just an ih parameter
                function action_cb(event) {
                    let ih = event.currentTarget,
                        res = func(ih);
                    if (res === IUP_CLOSE) {
                        let dialog = IupGetDialog(ih);
                        IupHide(dialog);
                    }
                }
//              ih.addEventListener("click", action_cb);
                ih.onclick = action_cb;
            } else if (cn === "text") {
                // action with ih, c, pNewValue parameters [not that we could ever actually provide the latter??
                //                                          - maybe change pGUI.e/docs to provide it as a string?
                //                                          - can we even can get a "cancellable-post-key" here?]
                function action_cb(event) {
                    let ih = event.currentTarget,
//                       c = event.keyCode,
//                       c = event.key.charCodeAt(0),
                         c = mapkey(event),
//                     res = func(ih);
                       res = func(ih,c);
//                     res = func(ih,c,pNewValue);
                    if (res === IUP_CLOSE) {
                        let dialog = IupGetDialog(ih);
                        IupHide(dialog);
                    } else if (res == IUP_IGNORE) {
                        event.preventDefault();
                        event.stopPropagation();
                    } else if (res && res !== IUP_DEFAULT && res !== c) {
//                      event.keyCode = res; // not alowed
//                      event.charCode = res;
                        //DEV no idea what to do here...
                        printf(1,"text/action_cb(%d) ==> %d???\n",["sequence",c,res]);
                    }
                }
//              ih.addEventListener("click", action_cb);
                ih.addEventListener("keydown", action_cb);
//              ih.onclick = action_cb;
            } else if (cn === "canvas") {
                ih.ACTION = func;
            } else {
                puts(1,"IupSetCallback(" + cn + "," + name + ")??\n"); // placeholder
            }
        } else if (name!=="MAP_CB" &&
                   name!=="UNMAP_CB" &&
//                 name!=="ACTION" &&
                   name!=="RESIZE_CB") {
            puts(1,"IupSetCallback(" + cn + "," + name + ")??\n"); // placeholder
        }
    }
}

function IupSetCallbacks(/*Ihandles*/ ih, /*sequence*/ namefuncpairs) {
    let l = length(namefuncpairs);
    assert(even(l));
    for (let i = 1; i <= l; i+=2) {
//      IupSetCallback(ih,$subse(namefuncpairs,i),$subse(namefuncpairs,i+1));
        IupSetCallback(ih,namefuncpairs[i],namefuncpairs[i+1]);
    }
}

function $paranormalise(action, func, attributes, args) {
// (see pGUI.e or the help docs for more details)
// ([DOC] named parameters are not [generally] permitted in pwa/p2js, except perhaps for eg timedelta()/as individually hand-coded in p2js.e)
    if (action === NULL) { action = null; }
    if (func === NULL) { func = null; }
    if ((action !== null) && (typeof(action) !== "string")) {
        if (typeof(action) === "function") {
            if (attributes && Array.isArray(attributes) && attributes.length) {
                args = attributes;
                attributes = func;
            } else if (typeof(func) === "string") {
                attributes = func;
            }
            func = action;
            action = null;
        } else {
            if (Array.isArray(func)) {
                args = func;
            }
            attributes = action;
            func = null;
            action = null;
        }
    } else if (func === null) {
        if ((typeof(attributes) !== "string" || attributes.length === 0) && (action !== null)) {
            attributes = action;
            action = null;
        }
    } else if (Array.isArray(func)) {
        attributes = action;
        args = func;
        action = null;
        func = null;
    }
    return [action,func,attributes,args];
}

//tests:
//  1   IupText()
//  2   IupText(action,func) [NB func is /not/ optional in this case]
//  3   IupText(func)
//  4   IupText(attributes[,args])
//  5   IupText(func,attributes[,args])
//  6   IupText(action,func,attributes[,args])
/*
function sqeq(a, b) {   // nested array match test
    if (!Array.isArray(a) || !Array.isArray(b) || a.length !== b.length) { crash("?9/0"); }
    for (let i = 0; i < a.length; i += 1) {
        if (Array.isArray(a[i])) {
            sqeq(a[i],b[i]);
        } else if (a[i] !== a[i]) {
            crash("?9/0");
        }
    }
}
function tcb() { return 1; }
//sqeq($paranormalise(),[null,null,"",[]]);                     // 1
//sqeq($paranormalise(null,tcb),[null,tcb,"",[]]);              // 2
//sqeq($paranormalise("act",tcb),["act",tcb,"",[]]);            // 2
//sqeq($paranormalise(tcb),[null,tcb,"",[]]);                   // 3
//sqeq($paranormalise("x"),[null,null,"x",[]]);                 // 4
//sqeq($paranormalise("x",[1]),[null,null,"x",[1]]);            // 4
//sqeq($paranormalise(tcb,"x"),[null,tcb,"x",[]]);              // 5
//sqeq($paranormalise(tcb,"x",[1]),[null,tcb,"x",[1]]);         // 5
//sqeq($paranormalise(null,tcb,"x"),[null,tcb,"x",[]]);         // 6
//sqeq($paranormalise(null,tcb,"x",[1]),[null,tcb,"x",[1]]);    // 6
// same, but with all four args:
sqeq($paranormalise(null,null,"",[]),[null,null,"",[]]);    // 1
sqeq($paranormalise(null,tcb,"",[]),[null,tcb,"",[]]);      // 2
sqeq($paranormalise("act",tcb,"",[]),["act",tcb,"",[]]);    // 2
sqeq($paranormalise(tcb,null,"",[]),[null,tcb,"",[]]);      // 3
sqeq($paranormalise("x",null,"",[]),[null,null,"x",[]]);    // 4
sqeq($paranormalise("x",[1],"",[]),[null,null,"x",[1]]);    // 4
sqeq($paranormalise(tcb,"x","",[]),[null,tcb,"x",[]]);      // 5
sqeq($paranormalise(tcb,"x",[1],[]),[null,tcb,"x",[1]]);    // 5
sqeq($paranormalise(null,tcb,"x",[]),[null,tcb,"x",[]]);    // 6
sqeq($paranormalise(null,tcb,"x",[1]),[null,tcb,"x",[1]]);  // 6

*/

function $topZindex(dlg) {
    // make dlg the topmost dialog [also done before removal]
    const dlgs = document.getElementsByClassName("dialog"),
          maxz = dlgs.length,
          thisz = dlg.style.zIndex;
    if (thisz !== maxz) {
        // eg 321 ==click2==> ([loop] 221==>) 231 [last]
        if (thisz) {
            for (let i = 0; i < dlgs.length; i += 1) {
                const di = dlgs[i], thatz = di.style.zIndex;
                if (thatz > thisz) { di.style.zIndex = thatz - 1; }
            }
        }
        dlg.style.zIndex = maxz;
    }
}

function IupClipboard() {
//  return "clipboard";
//string text = IupGetAttribute(clipboard,"TEXT")
    const ih = document.createElement("textarea");
    ih.setAttribute("class", "clipboard");
    ih.setAttribute("readonly", "");
//  ih.style = { position: "absolute", left: "-9999px" };
//              this.$clipboard.setAttribute("style", "display:none;position:fixed;left:-99em;top:-99em;");
    ih.setAttribute("style", "display:none;position:fixed;left:-99em;top:-99em;");
    return ih;
}

function IupCloseOnEscape(/* Ihandle */ dlg) {
    function hide_dlg(event) {
//      let keyCode = window.event ? window.event.keyCode : e.which;
//      if (keyCode === 27) {
        if (event.key === "Escape") {
            IupHide(dlg);
        }
    }
//  document.onkeydown = hide_dlg;
//  dlg.onkeydown = hide_dlg;
//  dlg.addEventListener("keydown",hide_dlg);
//  document.addEventListener("keydown",hide_dlg);
    $docBody.addEventListener("keydown",hide_dlg);
//  dlg.tabIndex="-1";
}

function IupDialog(child, attributes = "", args = [], bEsc = true) {
// as close as possible to pGUI.e, see phix.chm

    function create_svg(classname, viewbox, path) {
        // (private helper function)
        const svgns = "http://www.w3.org/2000/svg",
                svg = document.createElementNS(svgns, "svg"),
                pth = document.createElementNS(svgns, "path");
        svg.setAttribute("class", classname);
        svg.setAttribute("viewBox", viewbox);
        pth.setAttribute("d", path);
        svg.appendChild(pth);
        return svg;
    }

    function header_button(htitle, path, viewbox = "0 0 30 30", classname = "minmax") {
        // (private helper function)
        const div = document.createElement("div"),
              svg = create_svg(classname, viewbox, path);
        div.classList.add(htitle);
        div.title = htitle;
        svg.classList.add(htitle);
        div.appendChild(svg);
        return div;
    }

    //
    // First create the (dialog) window with a draggable header.
    // =========================================================
    //
    const dialog = document.createElement("div"),
          headiv = document.createElement("div"),
          header = document.createElement("div"),
          dlgbod = document.createElement("div");
    headiv.classList.add("dialog-header");
    header.classList.add("dialog-handle");
//div.setAttribute('class', 'note');
//  header.innerHTML = "<b>" + htitle + "</b>";
    header.innerHTML = "<b><i>untitled</i></b>";
    headiv.appendChild(header_button("Application", "M28 0h-24c-2.2 0-4 1.8-4 4v24c0 2.2 "+
                                     "1.8 4 4 4h24c2.2 0 4-1.8 4-4v-24c0-2.2-1.8-4-4-4zM14 "+
                                     "24.828l-7.414-7.414 2.828-2.828 4.586 4.586 9.586-9.586 "+
                                     "2.828 2.828-12.414 12.414z", "-12 -8 48 48", "icon"));
// <svg class="icon icon-checkbox-checked"><use xlink:href="#icon-checkbox-checked"></use></svg>
// <symbol id="icon-checkbox-checked" viewBox="0 0 32 32">
// <path d="M28 0h-24c-2.2 0-4 1.8-4 4v24c0 2.2 1.8 4 4 4h24c2.2 0 4-1.8 4-4v-24c0-2.2-1.8-4-4-4zM14 24.828l-7.414-7.414 2.828-2.828 4.586 4.586 9.586-9.586 2.828 2.828-12.414 12.414z"></path>
// </symbol>
//.icon {
//  display: inline-block;
//  width: 1em;
//  height: 1em;
//  stroke-width: 0;
//  stroke: currentColor;
//  fill: currentColor;
//}
    headiv.appendChild(header);
    headiv.appendChild(header_button("Maximise", "M10,10 L10,20 L20,20 L20,10 Z"));
    headiv.appendChild(header_button("Close", "M10,10 L20,20 M20,10 L10,20"));
    dlgbod.classList.add("dialog-body");
//  dlgbod.innerHTML = "<p>Move</p><p>this</p><p>div</p>";
//  dlgbod.innerHTML = "Move<br>this<br>div<br>";
    dlgbod.appendChild(child);
    dialog.className = "dialog";
//26/5 (see if this helps with boids...)
//  dialog.setAttribute("data-sizing","intrinsic");
    dialog.appendChild(headiv);
    dialog.appendChild(dlgbod);

    function mouseDown(event) {
        event.preventDefault();  // prevent selection start (browser action)

        const rect = header.getBoundingClientRect(),
            shiftX = event.clientX - rect.left,
            shiftY = event.clientY - rect.top;

        function doMove(event) {
            const Lft = event.clientX - shiftX,
                  Top = event.clientY - shiftY,
                  Rht = window.innerWidth - dialog.offsetWidth + 1,
                  Btm = window.innerHeight - dialog.offsetHeight + 1;

            function clamp(num, min, max) {
                return (num <= min ? min : (num >= max ? max : num));
            }

            function above(num, low) {
                return (num <= low ? low : num);
            }

            dialog.style.top = clamp(Top, -1, above(Btm, -1)) + "px";
            dialog.style.left = clamp(Lft, -1, above(Rht, -1)) + "px";
        }

        function stopMove() {
            document.documentElement.removeEventListener("mousemove", doMove);
            document.documentElement.removeEventListener("mouseup", stopMove);
        }

        document.documentElement.addEventListener("mousemove", doMove);
        document.documentElement.addEventListener("mouseup", stopMove);
    }

    const maxbtn = dialog.getElementsByClassName("Maximise")[0],
          clsbtn = dialog.getElementsByClassName("Close")[0],
          maxsvg = maxbtn.children[0];

    function maxWindow() {
        $topZindex(dialog);
        if (dialog.classList.toggle("window-maximized")) {
            dialog.wastop = dialog.style.top;
            dialog.wasleft = dialog.style.left;
            dialog.waswidth = dialog.style.width;
            dialog.washeight = dialog.style.height;
            dialog.style.top = "-1px";
            dialog.style.left = "-1px";
            dialog.style.width = window.innerWidth + "px";
            dialog.style.height = window.innerHeight + "px";
            maxbtn.title = "Restore";
        } else {
            dialog.style.top = dialog.wastop;
            dialog.style.left = dialog.wasleft;
            dialog.style.width = dialog.waswidth;
            dialog.style.height = dialog.washeight;
            maxbtn.title = "Maximise";
        }
        maxsvg.classList.toggle("restore");
    }

    header.onmousedown = mouseDown;
    maxbtn.onclick = maxWindow;
    header.ondblclick = maxWindow;
//  header.ondragstart = function ds() { return false; }
//  clsbtn.onclick = function hd() { IupHide(dialog); }
//  dialog.onmousedown = function md() { $topZindex(dialog); }
    function ds() { return false; }
    function hd() { IupHide(dialog); }
    function md() { $topZindex(dialog); }
    header.ondragstart = ds;
    clsbtn.onclick = hd;
    dialog.onmousedown = md;

    //
    // Next, allow re-sizing, aka dragging corners and edges.
    // ======================================================
    //
    function initSize(event) {
        event.preventDefault();
//      const resizer = this,  // (one of 8)
        const resizer = event.currentTarget,  // (one of 8)
              originalTop = dialog.offsetTop,
              originalLeft = dialog.offsetLeft,
              originalWidth = dialog.clientWidth,
              originalHeight = dialog.clientHeight,
              originalPageX = event.pageX,
              originalPageY = event.pageY;

        function doSize(event) {
            //
            // Note: use of "resize: both;" displays the handle *AND* effectively
            //       disables this routine on the bottom right resizer. This must
            //       handle the other seven resizers anyway, and hence instead we
            //       shun resize:both and draw our own (br) resize handle.
            //
            // Also: We deliberately allow dragging/resizing to {-1,-1}, effectively
            //       overlapping our and the browser's top 1-pixel borders, whereas
            //       limiting it to {0,0} left it looking too fat (2px). Note that
            //       the left/right/bottom borders can & do in fact end up hidden.
            //       A bug during developent allowed drag to {-2,-2}, and it looked
            //       barely any different, in other words we cannot, and would not
            //       actually want to get rid of the top border completely anyway.
            //
            let xd = event.pageX - originalPageX,
                yd = event.pageY - originalPageY,
                 w = originalWidth + resizer.xm*xd,
                 h = originalHeight + resizer.ym*yd;

            if (w >= dialog.minimumWidth) {
                if (resizer.xm === -1) {
                    // move left edge
                    xd += originalLeft;
                    if (xd < -1) {
                        w += xd + 1;
                        xd = -1;
                    }
                    dialog.style.left = xd + "px";
                } else {
                    // move right edge
                    if (originalLeft + w > window.innerWidth) {
                        w = window.innerWidth - originalLeft;
                    }
                }
                dialog.style.width = w + "px";
            }

            if (h >= dialog.minimumHeight) {
                if (resizer.ym === -1) {
                    // move top edge
                    yd += originalTop;
                    if (yd < -1) {
                        h += yd + 1;
                        yd = -1;
                    }
                    dialog.style.top = yd + "px";
                } else {
                    // move btm edge
                    if (originalTop + h > window.innerHeight) {
                        h = window.innerHeight - originalTop;
                    }
                }
                dialog.style.height = h + "px";
            }
        }

        function stopSize() {
            document.documentElement.removeEventListener("mousemove", doSize, false);
            document.documentElement.removeEventListener("mouseup", stopSize, false);
        }

        document.documentElement.addEventListener("mousemove", doSize, false);
        document.documentElement.addEventListener("mouseup", stopSize, false);
        $topZindex(dialog);
    }

    // add eight resizers, and all their handling, to the dialog window
    const rd = ["tl", "top", "tr", "left", "right", "bl", "bottom", "br"],
          xms = [-1,    0,    +1,    -1,      +1,    -1,      0,     +1],
          yms = [-1,   -1,    -1,     0,       0,    +1,     +1,     +1],
          rc = ["nw",  "n",  "ne",   "e",    "e",   "ne",    "n",   "nw"],
          rdiv = document.createElement("div");

//  rd.forEach((id, idx) => {
    function addSizer(id, idx) {
        const xm = xms[idx],
              ym = yms[idx],
              r = document.createElement("div");
        r.className = "resizer-" + id;  // (no longer strictly necessary)
        r.xm = xm;                      // for use in doSize()
        r.ym = ym;                      //      """
        r.style.width = xm === 0 ? "100%" : "5px";  // (edge | corner)
        r.style.height = ym === 0 ? "100%" : "5px"; //      """
        r.style.background = "transparent";
        r.style.position = "absolute";
        r.style.cursor = rc[idx] + "-resize";
        // ensure corner resizers are atop side resizers:
        r.style.zIndex = (xm === 0 || ym === 0) ? 1 : 10;
        if (ym === -1) { r.style.top = 0; } else { r.style.bottom = 0; }
        if (xm === -1) { r.style.left = 0; } else { r.style.right = 0; }
        r.addEventListener("mousedown", initSize, false);
        rdiv.appendChild(r);
//    });
    }
    rd.forEach(addSizer);

    rdiv.className = "dialog-resizers"; // (not strictly necessary)

    const drag_handle = create_svg("resize","0 0 7 7","M0,6 L6,0 M4,6 L6,4");
    rdiv.appendChild(drag_handle);
    dialog.appendChild(rdiv);

    IupSetAttributes(dialog, attributes, args);

    if (bEsc) { IupCloseOnEscape(dialog); }

//DEV: IupShow/IupPopup??
/*
    // Then, almost last, make it top dog, and add it to the DOM.
    dialog.style.zIndex = document.getElementsByClassName("dialog").length;
//  document.body.appendChild(dialog);
    $docBody.appendChild(dialog);

    // Originally, dragging a window behaved rather differently before and
    // after resizing the window, hence this...
    const rect = dialog.getBoundingClientRect(),    // (nb: recalc in DOM)
             w = rect.width,
             h = rect.height;
    dialog.style.width = w + "px";
    dialog.style.height = h + "px";
    // It may be possible to allow some squishing, not entirely sure...
    dialog.minimumWidth = w;                        // for use in doSize()
    dialog.minimumHeight = h;                       //      """
*/
//temp:
//let s = sprintf("minsize: {%d,%d}\n",[w,h]);
//let s = sprintf("%v\n",[intint("225x75")]);   // (NB: I had to move it to test it)
//puts(2,s);
//document.body.insertAdjacentHTML("afterbegin", s);
//$docBody.insertAdjacentHTML("afterbegin", s);
//minsize: {208,202}
//minsize: {142,202}

    return dialog;
} // (IupDialog() ends...)

function IupMap(ih) {
    let action = ih["MAP_CB"];
    if (action) { 
        action(ih); 
        ih["MAP_CB"] = null;
    }
    let children = ih.childNodes,
        l = children.length;
    for (let i=0; i<l; i += 1) { IupMap(children[i]); }
}

function IupRedraw(ih) {
//  let action = ih["REDRAW_CB"];
    let action = ih["ACTION"];
    if (action) { 
        action(ih); 
//      ih["MAP_CB"] = null;
    }
    let children = ih.childNodes,
        l = children.length;
    for (let i=0; i<l; i += 1) { IupRedraw(children[i]); }
}

function IupShow(ih) {
    IupMap(ih);
    // Make it top dog, and add it to the DOM.
    ih.style.zIndex = document.getElementsByClassName("dialog").length;
    $docBody.appendChild(ih);

    // Originally, dragging a window behaved rather differently before and
    // after resizing the window, hence this...
    const rect = ih.getBoundingClientRect(),    // (nb: recalc in DOM)
             w = rect.width,
//           w = floor(rect.width),
             h = rect.height;
//           h = floor(rect.height);
    ih.style.width = w + "px";
    ih.style.height = h + "px";
    // It may be possible to allow some squishing, not entirely sure...
    ih.minimumWidth = w;                        // for use in doSize()
    ih.minimumHeight = h;                       //      """
//  ih.onresize(ih,w,h);
//--DEV REDRAW_CB? ACTION? (triggers violation too)
//  const event = new Event('resize');
//  ih.dispatchEvent(event);
//  window.setTimeout(ih.dispatchEvent,10,event);
//  window.setTimeout(function() { ih.dispatchEvent(event); }, 100);
    IupRedraw(ih);
}

//TEMP:
let IupShowXY = IupShow;

function IupHide(ih) {
    const dialogs = document.getElementsByClassName("dialog");
    if (dialogs.length === 1) {
// See IupMainLoop:
//      if (terminator) { terminator(); }
// @grant        window.close
        window.close();  // close the whole page [DEV/erm?]
//      open(location, '_self').close();
//      window.top.close();  // close the whole page [DEV/erm?]
//      window.open('about:blank', '_self').close();
//      window.open('', '_self').close();
//      function closeWin() { 
//          window.top.close();
//      }
//      setTimeout(function(){ closeWin() }, 1000);
//      $docBody.innerHTML = "";
//      puts(1,"\n\n\n\n");
//      puts(1,"                                                 Oh dear.\n\n");
//      puts(1,"                      With all of their usual infinite lack of wisdom,\n\n");
//      puts(1,"              the spineless retards that so-called `develop` this stuff,\n\n");
//      puts(1,"          have blocked window.close() for cretinous `security reasons`.\n\n");
//      puts(1,"      Just like http:mydir and file:mydir being a stupid `CORS violation`.\n\n");
//      puts(1,"  Like it would be `far too difficult` to ever make anyone's life any easier.\n\n");
//      puts(1,"Try using an older and therefore less bug-ridden browser version, maybe.\n\n");
//      puts(1,"PS: Any resemblance of the above to a dog's turd is entirely coincidental.\n\n");
    } else {
        $topZindex(ih); // (specifically, fixup the rest)
        $docBody.removeChild(ih);
    }
//  ih.style.display = 'none';
}

function IupText(action=null, func=null, attributes = "", args = []) {
    const ih = document.createElement("input");
    ih.setAttribute('class', 'text');
    [action,func,attributes,args] = $paranormalise(action,func,attributes,args);
    if (func) {
        if (action === "VALUECHANGED_CB") {
            printf(1,"warning: IupText(VALUECHANGED_CB) not yet supported\n");
        } else {
            if (action !== null && action !== "ACTION") { crash("?9/0"); }
//          if (!action) { action = "ACTION"; }
            function action_cb(event) {
                puts(1,"action_cb");    // placeholder...
//              let c = ??,
//                  pNewValue = ??,
//                  res = func(/*Ihandle*/ ih, /*integer*/ c, /*atom*/ pNewValue);
//              if (res===IUP_IGNORE) {
//              }
            }
            ih.addEventListener('keyup', action_cb);
        }
    }
    IupSetAttributes(ih, attributes, args);
    return ih;
}       

function IupToggle(title=null, action=null, func=null, attributes = "", args = []) {
    const ih = document.createElement("div");
    ih.setAttribute('class', 'toggle');
    if (title) {
//      ih.innerHTML = title;
        ih.innerHTML = "<nobr>" + title + "</nobr>";
//DEV:
//      IupSetAtribute(ih,"TITLE",title);
    }
    [action,func,attributes,args] = $paranormalise(action,func,attributes,args);
    if (func) {
        if (action === "VALUECHANGED_CB") {
            printf(1,"warning: IupToggle(VALUECHANGED_CB) not yet supported\n");
        } else {
            if (action !== null && action !== "ACTION") { crash("?9/0"); }
//          if (!action) { action = "ACTION"; }
            function action_cb(event) {
                puts(1,"action_cb");    // placeholder...
//              let c = ??,
//                  pNewValue = ??,
//                  res = func(/*Ihandle*/ ih, /*integer*/ c, /*atom*/ pNewValue);
//              if (res===IUP_IGNORE) {
//              }
            }
            ih.addEventListener('keyup', action_cb);
        }
    }
    IupSetAttributes(ih, attributes, args);
    return ih;
}       

//IupFlatLabel?? (mnemonics not supported...???)
function IupLabel(title=null, attributes = "", args = []) {
    const ih = document.createElement("div");
    ih.setAttribute('class', 'label');
    if (title) {
//      ih.innerHTML = title;
        ih.innerHTML = "<nobr>" + title + "</nobr>";
//DEV:
//      IupSetAtribute(ih,"TITLE",title);
    }
    IupSetAttributes(ih, attributes, args);
//  ih.className = "label";
    return ih;
}

//function IupFlatLabel(title = null, attributes = "", args = []) {
//  return IupLabel(title, attributes, args);
//}
const IupFlatLabel = IupLabel;

function IupHbox(children, attributes = "", args = []) {
//  if (!Array.isArray(children)) {
    if (!sequence(children)) {
//      crash('vbox children must be an array!');
        crash('hbox children must be a sequence!');
    }
    const ih = document.createElement("div");
    ih.setAttribute('class', 'hbox');
//  ih.className = "hbox";
//9/4/21:
//  for (let i = 0; i < children.length; i += 1) {
    let l = length(children);
    for (let i = 1; i <= l; i += 1) {
        let ci = children[i];
// ?    if (ci.className === "fill") {
//          IupSetAttribute(ci,"EXPAND","HORIZONTAL");
//      }
        ih.appendChild(ci);
    }
    IupSetAttributes(ih, attributes, args);
    return ih;
}

function IupVbox(children, attributes = "", args = []) {
//  if (!Array.isArray(children)) {
    if (!sequence(children)) {
//      crash('vbox children must be an array!');
        crash('vbox children must be a sequence!');
    }
    const ih = document.createElement("div");
    ih.setAttribute('class', 'vbox');
//  ih.className = "vbox";
    let l = length(children);
    for (let i = 1; i <= l; i += 1) {
        let ci = children[i];
// ?    if (ci.className === "fill") {
//          IupSetAttribute(ci,"EXPAND","VERTICAL");
//      }
        ih.appendChild(ci);
    }
    IupSetAttributes(ih, attributes, args);
    return ih;
}

//DOC: [RASTER]SIZE must use wxh format, one of which will be ignored... or... must be 0.
//      Any attributes must be set before the element is inserted into the hierarchy (not normally an issue).
function IupFill(attributes = "", args = []) {
    const ih = document.createElement("div");
    ih.setAttribute('class', 'fill');
    IupSetAttributes(ih, attributes, args);
    return ih;
}

function IupGetDialog(ih) {
    let parent = ih.offsetParent;
    while (parent === null || parent.classList[0] !== "dialog") { // nb not NULL!
        ih = ih.parentNode;
        if (ih.classList[0] === "dialog") {
            parent = ih;
            break;
        }
    }
//  assert(parent.classList[0] === "dialog");
    return parent;
}

//IupButton(nullable_string title=NULL,[[nullable_string action=NULL,] cbfunc func=NULL,] string attributes="", sequence args={}) 
function IupButton(title = null, action = null, func = null, attributes = "", args = []) {
    const ih = document.createElement("button");
    ih.setAttribute('class', 'button');
    [action,func,attributes,args] = $paranormalise(action,func,attributes,args);
    if (title) {
//      ih.innerHTML = title;
//      title = mnemonics(ih,title);
        ih.innerHTML = "<nobr>" + title + "</nobr>";
//DEV
//      IupSetAttribute(ih,"TITLE",title);
    }
    if (func) {
        if (action !== null && action !== "ACTION") { crash("?9/0"); }
//      IupSetCallback(ih, action, func)
//      btn.addEventListener('click', toggleHide);
        function click(event) {
            let res = func(event.currentTarget);
            if (res === IUP_CLOSE) {
                let dialog = IupGetDialog(ih);
                IupHide(dialog);
            }
        }
        ih.addEventListener('click', click);
    }
    IupSetAttributes(ih, attributes, args);
    return ih;
}

const IupFlatButton = IupButton;

function IupTable(columns, data, visible=10, attributes="", args=[]) {
    let table = document.createElement(`table`),
        container = document.createElement(`div`);  // wraps table+resizers
    table.style.height = visible*23+27 + 'px';
    container.appendChild(table);
    container.classList = 'container';

    function pxFloat(s) { return parseFloat(s.replace("px", "")); }
    function pxWidth(elem) { return pxFloat(elem.style.width); }
    function eHeight(elem) { return pxFloat(getComputedStyle(elem).height); }
    //function eWidth(elem) { return pxFloat(getComputedStyle(elem).width); }

//DEV now don't be silly... (if we need pGUI.js then make sure tagset.js is in the list...)
//  function my_tagset(n) {
//      // (in preference to forcing pwa/p2js to include pwa/builtins/tagset.js...)
////        let tags = repeat(0,n);
//      let tags = [];
//      for (let i = 1; i <= n; i += 1) { 
////        for (let i = 0; i < n; i += 1) { 
//        tags[i] = i;
//      }
//      return tags;
//  }

    let nColumns = length(columns),
        nRows = length(data[1]),
//      tags = my_tagset(nRows),
        tags = tagset(nRows),
        sort_col = 1,
        sort_dir = 1,
        table_widths = [],
        table_aligns = [],
        iObserver,
        iObservedNodes = [],
        created = 0,
        updated = 0;

    function coldata(i,c) {
        let ti = tags[i],
            l2 = length(data[2]);
        if (c<=l2 && sequence(data[2][c])) {
            return data[2][c][ti];
        }
        return data[1][ti][c];
    }

    function update_item(node) {
        let id = Number(node.id),
            tds = Array.from(node.querySelectorAll('td'));
        if (tds.length===1) {
            for (let i=1; i<=nColumns; i += 1) {
                let td = document.createElement('td');
                node.appendChild(td);
                tds[i] = td;
            }
        }
        for (let c=1; c<=nColumns; c += 1) {
            let td = tds[c-1];
            td.textContent = coldata(id,c);
            td.style.width = table_widths[c];
            td.style.maxWidth = table_widths[c];
            td.style.textAlign = table_aligns[c];
        }
    }

    function background_refresh() {
        // Note this can take 15s to update 5000 records, the point
        //      is to make more "reasonable" scrolling silky-smooth,
        //      (without it, every page down jumps quite jarringly)
        //      and for the first display to be near-instantaneous,
        //      at least, that is, post-buildDummyData() timings.
        let last = Math.min(updated+100, nRows);
//p2js:
//      for (; updated<last; updated += 1) {
        for (let i=updated; i<last; i += 1) {
            updated = i;
            update_item(iObservedNodes[updated]);
        }
        // let UI update / do next block in 0.01s:
        if (updated<nRows) { setTimeout(background_refresh, 10); }
    }

    function refresh_observer() {
        // works fine, and perfectly fast enough on 10,000 nodes.
        if (created===nRows) {
            iObserver.disconnect();
            for (let i=0; i<nRows; i += 1) {
                let node = iObservedNodes[i];
//              iObserver.unobserve(node);
                iObserver.observe(node);
            }
            updated = 0;
            setTimeout(background_refresh,10);
        } else {
            setTimeout(refresh_observer,100); // (try again in 0.1s)
        }
    }

    function xSorter(i, j) {
//$share:
//      if (i==="sequence") { return -1; }
//      if (j==="sequence") { return +1; }
        if (typeof(i) !== "number") { return -1; }
        if (typeof(j) !== "number") { return +1; }

//maybe:
//      let cmp = compare(data[1][i][sort_col],
//                        data[1][j][sort_col])
        let ti = data[1][i][sort_col],
            tj = data[1][j][sort_col],
            cmp = ti < tj ? -1 
                : ti > tj ? +1 
                          :  0;
        return cmp*sort_dir;
    }

    function sortTable(event) {
        let th = event.currentTarget,
            ad = th.getAttribute('data-sorted'),
            ths = th.parentNode.querySelectorAll('th');
        for (let i=0; i<ths.length; i += 1) {
            let ti = ths[i];
            ti.removeAttribute('data-sorted');
        }
        ad = ad==='A' ? 'D' : 'A';
        sort_col = Number(th.id);
        sort_dir = ad==='A' ? +1 : -1;
        tags.sort(xSorter);
        th.setAttribute('data-sorted', ad);
        refresh_observer();
    }

    function set_intersection_observer() {
        function iob_handler(entries, iObserver) {
//p2js:
/*
            entries.forEach(entry => {
                if (entry.isIntersecting) {
                    update_item(entry.target);
                }
            });
*/
            let l = entries.length;
            for (let i=0; i < l; i += 1) {
                let entry = entries[i];
                if (entry.isIntersecting) {
                    update_item(entry.target);
                }
            }
        }
        iObserver = new IntersectionObserver(iob_handler, {});
    }

    function trHide(trOld) {
        if (trOld) { trOld.classList.remove('trActive'); }
        return false;
    }

//  let shift, ctrl;

    function trSelect(trOld, trNew, bPageKey=0) {
// maybe not for pageup/down:...
        if (trNew==null) { return true; }   // pass keybd input up
        trHide(trOld);
        trNew.classList.add('trActive');
        let tbody = trNew.parentNode,
            tableHeight = eHeight(tbody);
        if (bPageKey) {
            if (typeof(bPageKey)==="string") { return true; } // ditto
            tbody.scrollTop += bPageKey*tableHeight+bPageKey;
            return false;
        }
        // scrolling magic
        let rowTop = trNew.offsetTop - tbody.scrollTop - tbody.offsetTop,
            rowBtm = rowTop + eHeight(trNew);
        if (rowTop < 0) {
            tbody.scrollTop += rowTop;                  // scroll up
        } else if (rowBtm > tableHeight) {
            tbody.scrollTop += rowBtm - tableHeight;    // scroll down
        }
        return false;
    }

    function trPage(trSel, direction) {
        let n = Math.floor(eHeight(trSel.parentNode)/eHeight(trSel));
        for (let i=1; i<=n; i+=1) {
            let trNxt = (direction===-1) ? trSel.previousElementSibling
                                         : trSel.nextElementSibling;
            if (!trNxt) { break; }
            trSel = trNxt;
        }
        return trSel;
    }

    function trClick(event) {
      let trNew = event.currentTarget,
          trOld = trNew.parentNode.querySelector('.trActive');
      trSelect(trOld,trNew);
    }

    function table_keyhandler(event) {
        let table = event.currentTarget,
            tbody = table.querySelector("tbody"),
            trSel = tbody.querySelector('.trActive');
        if (trSel!=null) {
//          shift = event.shiftKey;
//          ctrl = event.ctrlKey;
            switch(event.key)
            {
                case 'ArrowDown':   return trSelect(trSel,trSel.nextElementSibling);
                case 'ArrowUp':     return trSelect(trSel,trSel.previousElementSibling);
                case 'PageDown':    return trSelect(trSel,trPage(trSel,+1),+1);
                case 'PageUp':      return trSelect(trSel,trPage(trSel,-1),-1);
                case 'Home':        return trSelect(trSel,trSel.parentNode.firstElementChild,"Home");
                case 'End':         return trSelect(trSel,trSel.parentNode.lastElementChild,"End");
                case 'Escape':      return trHide(trSel);
            }
        }
    }

    function populate_table() {
        let tbody, bRefresh = false;
        if (created===0) {
            let thead = document.createElement('thead'),
                headr = document.createElement('tr');
            tbody = document.createElement('tbody');
            tbody.setAttribute('tabindex', "0");
            thead.appendChild(headr);
            table.appendChild(thead);
            table.appendChild(tbody);
            for (let i=1; i<=nColumns; i += 1) {
                let th = document.createElement('th'),
                    ci = columns[i];
                if (Array.isArray(ci)) {
                    let wydth = (columns[i][2]-11) + 'px',
                        align = columns[i][3].toLowerCase().slice(1);
                    th.style.width = wydth;
                    th.style.maxWidth = wydth;
                    th.style.textAlign = align;
                    table_widths[i] = wydth;
                    table_aligns[i] = align;
                    ci = ci[1];
                }
                th.textContent = ci;
                th.id = i;
                th.classList.add('sortable');
                th.onclick = sortTable;
                headr.appendChild(th);
            }   
            let th = document.createElement('th');
            th.classList.add('thlast');
            headr.appendChild(th);
            bRefresh = true;
        } else {
            tbody = table.querySelector("tbody");
        }
//      for (let i=1; i<=nRows; i++) { // (now in timslices)
        let last = Math.min(created+100-0, nRows);
//p2js:
//      for (; created<last; created += 1) {
        for (let i=created; i<=last; i += 1) {
            created = i;
            let tr = document.createElement('tr');
            for (let c=1; c<=nColumns; c += 1) {
                tr.appendChild(document.createElement('td'));
            }
            tr.id = created+1;
            tr.onclick = trClick;
            tbody.appendChild(tr);
            iObserver.observe(tr);
            iObservedNodes.push(tr);
        }
        if (created<nRows) { setTimeout(populate_table, 10); }
        if (bRefresh) { refresh_observer(); }
    }

    function createResizeDivs() {
        let dragActive = false, startX, 
            bodyLeft = document.body.getBoundingClientRect().left,
            resizer,                    // as clicked on
            resizeLeft,                 //      ""       initial left (pos)
            left_th, right_th,          //      "" twixt
            leftWidth, rightWidth,      // initial widths
            resizer_array = [],
            table_th = table.querySelectorAll("th"),
            leftPos = 0,
            colIndex;

        function rd_mouseDown(event) {
            dragActive = true;
            startX = event.pageX;
            resizer = event.currentTarget;
            colIndex = resizer_array.indexOf(resizer);
            left_th = table_th[colIndex];
            right_th = table_th[colIndex+1];
            leftWidth = pxWidth(left_th);
            rightWidth = pxWidth(right_th);
            resizeLeft = resizer.getBoundingClientRect().left - bodyLeft;
        }

        function mouseMove(event) {
            if (dragActive) {
                let movement = event.pageX - startX,
                    new_left = leftWidth + movement,
                    new_right = rightWidth - movement;

                if (new_left > 40 &&
                    new_right > 40) {
                    let leftpx = new_left + "px",
                        rightpx = new_right + "px";
                    left_th.style.width = leftpx;
                    left_th.style.maxWidth = leftpx;
                    right_th.style.width = rightpx;
                    right_th.style.maxWidth = rightpx;
                    table_widths[colIndex+1] = leftpx;
                    table_widths[colIndex+2] = rightpx;
                    resizer.style.left = resizeLeft + movement + "px";
                    refresh_observer();
                }
            }
        }

        function mouseUp(){
            dragActive = false;
        }

        for (let i=1; i<table_th.length-1; i += 1) {
            let resizer = document.createElement("div");
            resizer.className = "column_resizer";
            leftPos += pxWidth(table_th[i-1])+11;
            resizer.style.left = (leftPos-3)+"px";
            resizer.addEventListener("mousedown", rd_mouseDown);
            container.appendChild(resizer);
            resizer_array.push(resizer);
        }
        table.addEventListener("mousemove", mouseMove);
        document.addEventListener("mouseup", mouseUp);
    }

    set_intersection_observer();
    populate_table();
    createResizeDivs(container,table);
    table.onkeydown = table_keyhandler;

    return container;
}

function IupTreeGetUserId(tree, id) {
//DEV/DOC this is effectively a null-op on the web browser.
//  let leaf = tree.querySelector('[data-tree-id="' + id + '"]');
//  let /*integer*/ userid = leaf.getAttribute('data-tree-id');
//  if (userid!==id) { crash("userid error?"); }
//  return userid;
    return id;
}

//p2js [DEV/temp]
//function IupTreeAddNodes(/*Ihandle*/ tree, /*sequence*/ tree_nodes, /*integer*/ id=-1) {
function IupTreeAddNodes(tree, tree_nodes, id=-1) {

    function createTVelem(tagname, classname, innerhtml) {
        let res = document.createElement(tagname);
        res.classList.add(classname);
        if (innerhtml) { res.innerHTML = innerhtml; }
        return res;
    }
    if (tree==="createTVelem") { return createTVelem; } // for IupTreeView

//p2js [DEV/temp]
//  function iupTreeSetNodeAttributes(leaf, /*sequence*/ attrs) {
    function iupTreeSetNodeAttributes(leaf, attrs) {
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
                        leaf.classList.add('closed');
                    } else if (v==="EXPANDED") {
                        leaf.classList.remove('closed');
                    } else {
                        crash("?9/0");
                    }
                } else {
                    crash("?9/0");
                }
            } else if (name==="USERDATA" && integer(v)) {
//              IupTreeSetUserId(tree, id, v); // (maybe?)
                leaf.setAttribute('data-tree-id',v);
            } else {
                crash("?9/0");
            }
        }
    }

//p2js [DEV/temp]
//  function iupTreeAddNodesRec(/*Ihandle*/ tree, /*sequence*/ tree_nodes, /*bool*/ bRoot=true) {
    function iupTreeAddNodesRec(tree, tree_nodes, bRoot=true) {
        //
        // internal routine, the guts of IupTreeAddNodes, less the initial clear
        // Here, tree is the immediate parent that we're adding the [subtree] tree_nodes to.
        // In the root case, tree is the ul, otherwise tree is an li and the ul we need to
        // add the new leaf (li) to is the (first) child ul of that, create one if needed.
        // Every node in the tree is basically an li/span/a triplet, with the latter two
        // being the first two children of the li.
        //
        let label = string(tree_nodes)?tree_nodes:tree_nodes[1],
            leaf = createTVelem('li', 'leaf'),
            parentList = tree;
        leaf.appendChild(createTVelem('span','treeToggle'));
        leaf.appendChild(createTVelem('a', 'leafLabel', label));
        if (!bRoot) {
            let list = tree.querySelector('ul');
            if (!list) {
                list = tree.appendChild(createTVelem('ul', 'subtree'));
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
                leaf.classList.add(lench?'showExpander':'closed');
                leaf.classList.add('hasChildren');
            }
            for (let i=1; i<=lench; i += 1) {
                iupTreeAddNodesRec(leaf, children[i], false);
            }
            if (lentn===3) {
                iupTreeSetNodeAttributes(leaf, tree_nodes[2]);
            }
        }
    }

    function deleteChildren(node) {
        while (node.hasChildNodes()) {  
            node.removeChild(node.firstChild);
        }
    }

    if (id===-1) {
//      IupSetAttributeId(tree,"DELNODE",0,"ALL");  // (maybe...)
        deleteChildren(tree);
        iupTreeAddNodesRec(tree, tree_nodes, true);
    } else {
        // tree_nodes is actually just children
        let children = tree_nodes,
            lench = length(children);
        tree = tree.querySelector('[data-tree-id="' + id + '"]');
        let list = tree.querySelector('ul');
        if (list) { deleteChildren(list); }
        if (!lench) {
            tree.classList.remove('showExpander');
            tree.classList.add('closed');
        } else {
            for (let i=1; i<=lench; i += 1) {
                iupTreeAddNodesRec(tree, children[i], false);
            }
        }
    }
}

//p2js [DEV/temp]
//function IupTreeView(/*sequence*/ tree_nodes, /*atom*/ branchopen_cb=null, /*string*/ attributes="", /*sequence*/ args=[]) {
function IupTreeView(tree_nodes, branchopen_cb=null, attributes="", args=[]) {
//
// Creates an IupFlatTree from a recursive [callback] data structure.
//
// tree_nodes is as per IupTreeAddNodes()
// branchopen_cb can be used for deferred loading, or be null if tree_nodes is complete.
// you can also set branchopen_cb later, via IupSetCallback(tree,"BRANCHOPEN_CB",Icallback("branchopen_cb")) [DEV]
//
    const createTVelem = IupTreeAddNodes("createTVelem");
    let /*Ihandle*/ tree = createTVelem('ul', 'tree');
//  let /*Ihandle*/ tree = IupTreeAddNodes.createTVelem('ul', 'tree'); // does not work...

    function clickHandler(event) {
        let leaf = event.target, // an a.leafLabel or span.treeToggle
            tcl = leaf.classList,
            pcl = leaf.parentNode.classList; // always an li.leaf
        if (tcl.contains('leafLabel')) {
            // was selectTreeLeaf()
            if (!pcl.contains('selected')) {
                let prev = tree.querySelector('.selected');
                if (prev) { prev.classList.remove('selected'); }
                pcl.add('selected');
                let select_cb = tree.select_cb;
                if (select_cb) {
                    select_cb(leaf.innerText, ', ye ha');
                }
            }
        } else if (tcl.contains('treeToggle')) {
            let it = leaf.nextSibling.innerText; // the a.leafLabel
            if (pcl.contains('closed')) {
                pcl.remove('closed');
                let open_cb = tree.open_cb;
                if (open_cb) {
                    open_cb(it, ', wowee');
                }
                let branchopen_cb = tree.BRANCHOPEN_CB;
                if (branchopen_cb) {
                    let id = leaf.parentNode.getAttribute('data-tree-id');
                    if (id) {
                        branchopen_cb(tree,id);
                    }
                }
            } else {
                pcl.add('closed');
                let close_cb = tree.close_cb;
                if (close_cb) {
                    close_cb(it, ', so there');
                }
            }
        }
    }
    tree.addEventListener('click', clickHandler);

/*
    tree.addEventListener('keydown', e => {
                const {code} = e;
                if (code === 'ArrowUp' || code === 'ArrowDown') {
                    this.navigate(code === 'ArrowUp' ? 'backward' : 'forward');
                    e.preventDefault();
                }
            });
    navigate(direction = 'forward') {
      const e = this.active();
      if (e) {
        const list = [...this.parent.querySelectorAll('a, summary')];
        const index = list.indexOf(e);
        const candidates = direction === 'forward' ? list.slice(index + 1) : list.slice(0, index).reverse();
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
    IupTreeAddNodes(tree, tree_nodes);
//DEV IupSetAttributes(tree,attributes,args); // maybe, MARKMULTIPLE
    return tree;
}

function Icallback(func) {
    // dummy function, exists solely for p2js to convert Icallback("func") [phix] <==> Icallback(func); [js]
    // in other words, p2js recognises the Icallback() and simply removes/adds the quotes, whereas if it
    // instead did Icallback("func") [phix] ==> func; [js] then it would likely end up as a one-way trip.
    // (js ==> phix may help the transpiler development, but is otherwise likely to be fairly useless.)
    return func;
}

// three more, for similar reasons to Icallback():
function IupMainLoop() {
    crash("ERROR: IupMainLoop() [and any following shudown code] should be inside `if platform()!=JS then`");
}
//function IupMainLoop(fn) { terminator = fn; }

function IupClose() {}

function IupDestroy(/* Ihandlns */ dlg) {
//  Ah: IupMainLoop() is a null-op in pGUI.js so it ends up invoking following code immediately...
//  What I need to do is (maybe) something like this:
//  IupMainLoop(()==> {
//  IupDestroy()...
//  });
//  (let's just make it an error to have "end procedure" etc after IupMainLoop(), perhaps?...)
//  (maybe better, force this (in .exw files):
//  if platform()!=JAVASCRIPT then
//      IupMainLoop()
//      dlg = IupDestroy(dlg)
//      IupClose()
//  end if
//  (and if platform()=WINDOWS and someothertest then ... else                                  )
//  (                          ^illegal                   ^error, platform()=JAVASCRIPT missing )
//
//  DEV: if (Array.isArray(dlg)) {
//  or do we just want to let js gc deal with it?? [Eh: no good for IupNormaliser!]
//X dlg.parentNode.removeChild(dlg);
//  return null;  //DEV would be good for ih, fatal for non-if-not-JS-wrapped (main)dlg=IupDestroy(dlg)...
    return dlg;
}

function IupCanvas(action = null, func = null, attributes = "", args = []) {
    let ih = document.createElement("canvas");
    ih.setAttribute('class', 'canvas');
    ih["EXPAND"] = "YES";
    [action,func,attributes,args] = $paranormalise(action,func,attributes,args);
    //DEV/temp...
//  canvas.onload = func; // nope...
//  canvas.onloadstart = func;
//  canvas.onresize = func;
    if (attributes) {
        IupSetAttributes(ih, attributes, args);
    }
//  if (func) { func(ih); }
    if (func) {
        function func_closure() { func(ih); }
        window.requestAnimationFrame(func_closure);
    }
    return ih;
}
let IupGLCanvas = IupCanvas;


function cdCanvasActivate(ctx) {
// (moved to cdCanvasClear...)
//  ctx.fillStyle = "white";
    ctx.fillRect(0,0,ctx.canvas.clientWidth,ctx.canvas.clientHeight);
}

function cdCanvasPixel(ctx,x,y,colour) {
/// let fs = ctx.fillStyle;
//  ctx.fillStyle = colour;
//  ctx.fillRect(x,ctx.canvas.clientHeight-y,1,1);
//  ctx.fillStyle = fs;
//  ctx.strokeStyle = colour;
    if (integer(colour)) { colour = sprintf("#%06x",colour); }
    ctx.strokeStyle = colour;
    ctx.strokeRect(x,ctx.canvas.clientHeight-y,1,0);
}

function cdCanvasClear(ctx) {
//  ctx.fillStyle = "white";
    ctx.fillRect(0,0,ctx.canvas.clientWidth,ctx.canvas.clientHeight);
}

function cdCanvasFlush() {}

function cdCreateCanvas(context, ih) {
    if (context===CD_DBUFFER ||
        context===CD_IUP) {
        if (typeof(ih.getContext) === "function") {
            let ctx = ih.getContext("2d");
            ctx.fillStyle = "white";
            return ctx;
        }
        return ih;
//  } else if (context===CD_GL) {
//      crash("CD_GL not supported by JavaScript");
    }
    crash("cdCreateCanvas(" + context + ",...) not supported");
}

function cdCanvasSetForeground(canvas, colour) {
    if (integer(colour)) { colour = sprintf("#%06x",colour); }
    canvas.strokeStyle = colour;
}

function cdCanvasSetBackground(canvas, colour) {
    if (integer(colour)) { colour = sprintf("#%06x",colour); }
    canvas.fillStyle = colour;
}

function cdCanvasBegin(ctx, mode) {
    ctx.beginPath();
    ctx["MODE"] = mode;
}

function cdCanvasVertex(ctx, x, y) {
    let h = ctx.canvas.clientHeight;
    ctx.lineTo(x, h-y);
}

function cdCanvasEnd(ctx) {
    let mode = ctx["MODE"];
    if (mode === CD_FILL) {
        let fs = ctx.fillStyle;
        ctx.fillStyle = ctx.strokeStyle;
        ctx.fill();
        ctx.fillStyle = fs;
    } else {
        crash("unsupported mode");
    }
}


function cdCanvasLine(ctx, x1, y1, x2, y2) {
    let h = ctx.canvas.clientHeight;
    ctx.beginPath();
    ctx.moveTo(x1, h-y1);
    ctx.lineTo(x2, h-y2);
    ctx.stroke();
}

function cdCanvasRect(/*cdCanvas*/ ctx, /*atom*/ xmin, xmax, ymin, ymax) {
//DEV see cdCanvasBox...
//  let h = ctx.canvas.clientHeight;
    let h = ctx.canvas? ctx.canvas.clientHeight : ctx.clientHeight;
    ymax = ymin-ymax;
    ymin = h-ymin;
//  ctx.strokeRect(xmin,h-ymin,xmax,h-ymax);
//  ctx.strokeRect(xmin+1,ymin+1,xmax-xmin-1,ymax-2);
    ctx.strokeRect(xmin,ymin,xmax-xmin,ymax);
//  ctx.strokeRect(xmin,h-ymin,xmax-3,h-ymax-3);
//  puts(1,"cdCanvasRect() not yet supported..\n"); // placeholder
}

//cdCanvasSector(cdCanvas canvas, atom xc, yc, w, h, angle1, angle2) 
function cdCanvasSector(ctx, xc, yc, w, h, angle1, angle2) {
    let ch = ctx.canvas.clientHeight;
    let fs = ctx.fillStyle;
    ctx.fillStyle = ctx.strokeStyle;
    ctx.beginPath();
    ctx.arc(xc,ch-yc,w/2,angle1*Math.PI/180,angle2*Math.PI/180);
    ctx.fill();
    ctx.fillStyle = fs;
}

function IupTimer(/*cbfunc*/ func=NULL, /*integer*/ msecs=0, /*boolean*/ active=true) {
    let timer;
    const /*TIMER=0, FUNC=1, MSECS=2,*/ ACTIVE=3, ID=4
    function action_cb() {
        if (!timer[ACTIVE]) { 
            window.clearInterval(timer[ID]);
        } else {
            let res = func(timer); // (it has to be able to IupSetAttribute(ih,"RUN")...)
//DEV: IupSetAttribute has to check for Array[0] of "timer" and name of RUN/TIME(/WID?).
//          if (res === IUPCLOSE) { uh?? }
        }
    }
    let id = window.setInterval(action_cb,msecs);
    timer = ["timer",func,msecs,active,id];
    return timer;
}

function IupUpdate(ih) {
//IupRedraw
//  let action = ih["ACTION"]
//  if (action) { action(ih); }
    function redraw() { IupRedraw(ih); }
    setTimeout(redraw, 100);
}

function IupGLMakeCurrent() {
}

function IupMessage(title=NULL, msg=NULL, args=[], bWrap=true) {
    if (args.length) {
        msg = sprintf(msg, args);
    }
    if (title) { msg = title + "\n\n" + msg;}
    alert(msg);
}

/*
<!DOCTYPE html>
<html>
<body>
<p>A script on this page starts this clock:</p>
<p id="demo"></p>
<p id="demo2"></p>
<script>
var myVar = setInterval(myTimer, 1000);
function myTimer() {
  var d = new Date();
  var t = d.toLocaleTimeString();
  document.getElementById("demo").innerHTML = t;
}
var myVar2 = setInterval(myTimer2, 2000);
function myTimer2() {
  var d = new Date();
  var t = d.toLocaleTimeString();
  document.getElementById("demo2").innerHTML = t;
}
</script>
</body>
</html>
*/



// (cbfunc/Ihandles/Ihandlns/cdCanvas/imImage/cdContext?)
// IupRawStringPtr?/IupSetCallback[s]/IupGetAttribute/IupGetInt/IupS/GetGlobal/IupVersion?
// Iup[GL]Canvas/IupDraw*/IupDatePick/IupCalendar/IupImage/IupLink(example)/IupList(example)/IupMenu/
// IupProgressBar/IupGauge/IupText/IupToggle/IupTree/IupValuator/IupMatrix?/IupPlot?
// Iupappend?/IupReparent?/IupFrame/IupMultiBox/IupNormaliser?/IupRadio/IupSbox/IupSplit/IupTabs/IupUser?
// IupClipboard/IupHelp?/IupTimer
// cdCanvasClear/Arc/Begin/Vertex/PathSet/End/Box/Line  glBegin...? (gears)
// cdCreateCanvas?

/*
// for IupMenu, perhaps...
function popupMenu() {
  const menuItems = ['add', 'this'];

  const popup = document.createElement('div');
  const popupList = document.createElement('ul')
  popup.className = 'popup-menu';
  popup.appendChild(popupList)
  document.body.appendChild(popup)

  function create_item(item) {
    const li = document.createElement('li');
    li.textContent = item;
    popupList.appendChild(li);
  }
  menuItems.forEach(create_item);

  function context_listener(event) {
    event.preventDefault();
    popup.style.display = 'block';
    let dW = document.documentElement.clientWidth,
        dH = document.documentElement.clientHeight,
        pW = popup.offsetWidth,
        pH = popup.offsetHeight,
        pX = Math.min(event.pageX,dW-pW-3),
        pY = Math.min(event.pageY,dH-pH-3);
    popup.style.left = pX + 'px';
    popup.style.top = pY + 'px';
  }
  document.addEventListener('contextmenu', context_listener);

  function click_hide(event) {
    const classes = event.target.classList;
    if (!classes.contains('popup-menu-list')) {
      popup.style.display = 'none';
    }
  }
  document.addEventListener('click', click_hide);
}

window.onload = popupMenu;

===
but, we probably want to attach different popups to different things anyway...:
function addContextMenu(elem) {
    elem.addEventListener("contextmenu", function(e) {
        e.preventDefault(); // avoids default right click menu
        e.stopPropagation();
        toggleMenuOn();
        positionMenu(e);
    });
}

// to attach a context menu to all divs, you can do this:
let divs = document.querySelectorAll(".div");

//divs.forEach(function(d) { addContextMenu(d); });
divs.forEach(addContextMenu);
*/
