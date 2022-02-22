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

const CD_BASE_CENTER = 10,
      CD_BASE_LEFT = 9,
      CD_BASE_RIGHT = 11,
      CD_BLACK = "#000000",
//    CD_BLUE = "blue",
      CD_BLUE = "#0000FF",
      CD_BOLD = 1,
      CD_BOLD_ITALIC = 3,
      CD_CENTER = 8,
      CD_CLOSED_LINES = 2,
      CD_CONTINUOUS = 0,
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
      CD_CUSTOM = 5,
      CD_DBUFFER = 2,
      CD_DEG2RAD = PI/180,
      CD_EAST = 2,
      CD_EVENODD = 0,
      CD_FILL = 0,
      CD_GL = "CD_GL",
      CD_GRAY = "#C0C0C0",
      CD_GREY = "#C0C0C0",
      CD_GREEN = "#3cb44b",
      CD_HATCH = 1,
      CD_HOLLOW = 4,
      CD_INDIGO = "#4B0082",
      CD_ITALIC = 2,
//    CD_IUP = 1,
      CD_IUP = "CD_IUP",
      CD_LIGHT_BLUE = "#4363d8",
      CD_LIGHT_GRAY = "#E4E4E4",
      CD_LIGHT_GREY = "#E4E4E4",
      CD_LIGHT_GREEN = "#00FF00",
      CD_MAGENTA = "#f032e6",
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
      CD_PURPLE = "#911eb4",
      CD_RAD2DEG = 180/PI,
      CD_RED = "#FF0000",
      CD_SILVER = "#C0C0C0",  // (=== CD_GREY)
      CD_SOLID = 0,
      CD_SOUTH = 1,
      CD_SOUTH_EAST = 6,
      CD_SOUTH_WEST = 7,
      CD_STIPPLE = 2,
      CD_STRIKEOUT = 8,
      CD_UNDERLINE = 4,
      CD_VIOLET = "#EE82EE",
      CD_WEST = 3,
      CD_WHITE  = "#FFFFFF",
      CD_WINDING = 1,
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
      K_DOWN = 40,
      K_END = 0,
      K_ESC = 0X1B,
      K_F1 = 112,
      K_F2 = 113,
      K_F3 = 114,
      K_F4 = 115,
      K_F5 = 116,
      K_F6 = 117,
      K_F7 = 118,
      K_F8 = 119,
      K_F9 = 120,
      K_F10 = 121,
      K_F11 = 122,
      K_F12 = 123,
      K_HOME = 0,
      K_INS = 0,
      K_LEFT = 37,
      K_MIDDLE = 0,
      K_PGDN = 34,
      K_PGUP = 33,
      K_RIGHT = 39,
      K_SP = 0X20,
      K_TAB = 0,
      K_UP = 38,
      K_h = 0X68,       // 'h' / 104
      K_i = 0X69,       // 'i' / 105
      K_j = 0X6A,       // 'j' / 106
      K_p = 0X70,       // 'p' / 112
      K_r = 0X72,       // 'r' / 114
      K_s = 0X73,       // 's' / 115
//DEV..
      K_cA = 0,
      K_cD = 0;

//DEV/SUG split into/manage both handlers and valid/supported values?
let $storeAttr = {};    // element-specific attribute handlers/setters

//DEV rename/doc...
//function $class_name(ih, check) {
//  if (!ih) { crash("invalid handle"); }
//  let cn = ih.classList[0];
//  if (typeof(check) === "string") {
//      if (check !== cn) {
//          crash("wrong class (not " + check + ")");
//      }
//  } else {
//      if (!Array.isArray(check)) { crash("uh??"); }
//      if (check.indexOf(cn) === -1) {
//          crash("wrong class (" + cn + " not in " + check + ")");
//      }
//  }
//}

//function $pxFloat(s) { return parseFloat(s.replace("px", "")); }
function $pxFloat(s) {
    if (s === "auto") { return 0; }
    return parseFloat(s.replace("px", ""));
}
//function $pxWidth(elem) { return $pxFloat(elem.style.width); }
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


let $shiftKey = false; /// or event.shiftKey, for IupGetInt(NULL,"SHIFTKEY")

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

    function intint(val,w,h) {
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

    function intints(val,w,h,name) {
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

    function class_name(ih, check) {
        if (!ih) { crash("invalid handle"); }
        let cn = ih.classList[0];
        if (typeof(check) === "string") {
            if (check !== cn) {
                crash("wrong class (not " + check + ")");
            }
        } else {
//          if (!Array.isArray(check)) { crash("uh??"); }
            assert(Array.isArray(check));
            if (check.indexOf(cn) === -1) {
                crash("wrong class (" + cn + " not in " + check + ")");
            }
        }
    }

//  function class_name(ih, check) {
//      if (!ih) { crash("invalid handle"); }
////        let cn = ih.className;
//      let cn = ih.classList[0];
////        let sp = cn.indexOf(' ');
//////hmmm, can we be sure it stays first??
////        if (sp !== -1) { cn = cn.slice(0,cn); }
////        if (check && check !== cn) {
//      if (typeof(check)==="string") {
//          if (check !== cn) {
////            if (!ih.classList.contains(check)) {    // [untried]
//              crash("wrong class (not " + check + ")");
//          }
//      } else if (Array.isArray(check)) {
//          if (check.indexOf(cn)===-1) {
//              crash("wrong class (" + cn + " not in " + check + ")");
//          }
//      } else {
//          crash("class_name(" + check + ")???");
//      }
////        return cn
//  }

    function set_style(ih,sname,val) {
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

    function rastersize(ih, name, val) {
        // applies to dialog, label, text, canvas, ...
//      if (name !== "SIZE" && name !== "RASTERSIZE") { crash("[RASTER]SIZE expected!"); }
        assert(name === "SIZE" || name === "RASTERSIZE");
        let cn = ih.classList[0];
        if ((val === null) || (val === NULL)) {
            if (cn === "canvas") {
                ih = IupGetDialog(ih);
                cn = "dialog";
            }
            if (cn === "dialog") {
                ih.minWidth = 260;
                ih.minimumWidth = 0;
                ih.minHeight = 160;
                ih.minimumHeight = 0;
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

/*
    function crastersize(ih, name, val) {
        // applies to canvas
        if (name !== "RASTERSIZE") { crash("RASTERSIZE expected!"); }
        if ((val === null) || (val === NULL)) {
            let dlg = IupGetDialog(ih);
            dlg.minWidth = 260;
            dlg.minimumWidth = 260;
            dlg.minHeight = 160;
            dlg.minimumHeight = 160;
//          ih.width = 0;
//          ih.height = 0;
//          ih.width = "100%";
//          ih.height = "100%";
        } else {
            let [w, h] = intint(val);
//DEV + "px"??
//NO: this is actually right, the trouble is the containing dialog does not expand to hold this...
            if (w) { ih.width = w; }
//          if (w) { ih.style.width = w; }
//          if (w) { ih.style.width = w + "px"; }
            if (h) { ih.height = h; }
//          if (h) { ih.style.height = h; }
//          if (h) { ih.style.height = h + "px"; }
        }
    }
    store_attrs(["canvas"], ["RASTERSIZE"], crastersize);
*/
//  store_attrs(["canvas"], ["RASTERSIZE","SIZE"], rastersize);

    function set_dialog(ih, name, val) {
//      if (ih.className !== "dialog") {
//          crash("wrong class (not IupDialog)");
//      }
        class_name(ih, "dialog");
        if (name === "TITLE") {
//          const hdrs = ih.getElementsByClassName("dialog-handle");
            const hdr = ih.querySelector(".dialog-handle");
//          if (hdrs.length !== 1) {
//              crash("cannot get (lone) header handle");
//          }
//          hdrs[0].innerHTML = "<b>" + val + "</b>";
//          hdrs[0].innerHTML = val;
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
        } else if (name === "MAXSIZE") {
            let [w, h] = intint(val);
//          ih.maxWidth = w;
            ih.maximumWidth = w;
//          ih.minHeight = h;
            ih.maximumHeight = h;
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
            if (vtb === "ATOP") {
                ih.style.alignItems = "flex-start";
//              ih.classList.add("expandv");
//              ih.classList.add("expandh");
            } else if (vtb === "ACENTER") {
                ih.style.alignItems = "center";
//              ih.classList.remove("expandv");
//              ih.classList.add("expandh");
            } else if (vtb === "ABOTTOM") {
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
        class_name(ih,"frame");
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
        } else {
            crash("IupStoreAttribute(IupFrame,\"" + name + "\") not yet implemented\n");
        }
    }
    store_attrs(["frame"], ["PADDING","MARGIN","TITLE","VISIBLE","FGCOLOR"], set_frame);

//Ihandle lbl = IupLabel("World","EXPAND=YES, ALIGNMENT=ACENTER"),
    function set_label(ih, name, val) {
        class_name(ih,"label");
        if (name === "TITLE") {
//          ih.innerHTML = val;
            const sp = new RegExp("[ ]","g"),
                  lf = new RegExp("\\n","g");
            val = val.replace(sp,"&ensp;")
                     .replace(lf,"<br>");
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
        } else if (name === "FONT" || name === "FONTFACE") {
//DEV       set_font(ih,val);
//          puts(1,"IupLabel(FONT,"+val+")??\n"); // placeholder
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
//              fallback = "serif";
            } else if (face === "Courier") {
                fallback = "monospace";
            } else {
//              crash("FONT="+val+"??\n");
                puts(1,"FONT="+val+"??\n"); // placeholder
            }
            ih.style.fontFamily = `"` + face + `", ` + fallback;
//font-family: "Tahoma", sans-serif;
//font-size: 12px;
//  "Times, Bold 18"
//  "Arial, 24" (no style)
//  "Courier New, Italic Underline -30" (size in pixels)
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
        } else {
            crash("IupStoreAttribute(IupLabel,\"" + name + "\") not yet implemented\n");
        }
    }
    store_attrs(["label"], ["TITLE","EXPAND","ALIGNMENT","MARGIN","PADDING","ACTIVE","VISIBLE",
                            "FONT","FONTFACE","FONTSTYLE","SEPARATOR"], set_label);
//  store_values("EXPAND",[["YES","NO","HORIZONTAL","HORIZONTALFREE","VERTICAL","VERTICALFREE"]])
//  store_values("ALIGNMENT",[["ALEFT","ACENTER","ARIGHT"],":",["ATOP","ACENTER","ABOTTOM"]])

    function set_list(ih, name, val) {
        class_name(ih,"list");
        if (name === "EXPAND") {
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
    store_attrs(["list"], ["<list_id>","DROPDOWN","EXPAND","APPENDITEM","REMOVEITEM","TIP",
                           "VALUE","VISIBLE","VISIBLELINES","VISIBLECOLUMNS","VISIBLEITEMS",
                           "SHOWDROPDOWN","NAME","EDITBOX"], set_list);

    function set_vbox(ih, name, val) {
        class_name(ih,["vbox","hbox"]);
//DEV common up??
        if (name === "MARGIN") {
//          set_style(ih,"margin",val);
            let [w, h] = intint(val);
            if (w) {
                ih.style.marginLeft = w + 'px';
                ih.style.marginRight = w + 'px';
            }
            if (h) {
                ih.style.marginTop = h + 'px';
                ih.style.marginBottom = h + 'px';
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
        } else {
            crash("IupStoreAttribute(IupVbox,\"" + name + "\") not yet implemented");
        }
    }
    store_attrs(["vbox","hbox"], ["MARGIN","NMARGIN","TABTITLE","ALIGNMENT","PADDING","NORMALIZESIZE","GAP","LINEBREAK"], set_vbox);

    function set_text(ih, name, val) {
        class_name(ih,"text");
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
            ih.setAttribute('type', 'number');
//          ih.setAttribute('type', 'number');
        } else if (name === "SPINMIN") {
//          puts(1,"IupText(SPINMIN,"+val+")??\n"); // placeholder
            ih.setAttribute('min', val);
        } else if (name === "SPINMAX") {
//          puts(1,"IupText(SPINMAX,"+val+")??\n"); // placeholder
            ih.setAttribute('max', val);
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
            ih['NAME'] = val;
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
        } else {
            crash("IupStoreAttribute(IupText,\"" + name + "," + val + "\") not yet implemented\n");
        }
    }
    store_attrs(["text"], ["VALUE","EXPAND","MASK","PADDING","SPIN","SPINMIN","SPINMAX","ACTIVE","FILTER",
                           "CUEBANNER","NAME","TIP","MULTILINE","FORMATTING","APPEND","INSERT","WORDWRAP",
                           "SCROLLBAR","READONLY","BGCOLOR","VISIBLECOLUMNS"], set_text);

    function set_toggle(ih, name, val) {
        class_name(ih,"toggle");
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
        } else {
            crash("IupStoreAttribute(IupToggle,\"" + name + "," + val + "\") not yet implemented\n");
        }
    }
    store_attrs(["toggle"], ["PADDING","RIGHTBUTTON","VALUE","TIP"], set_toggle);

    function linebreak(ih, name, val) {
        // applies to dialog, label, text
        if (name !== "LINEBREAK") { crash("LINEBREAK expected!"); }
        let cn = ih.classList[0];
        puts(1,"linebreak("+cn+","+val+")??\n"); // placeholder
//      ih[LINEBREAK] = val;
//      ih.LINEBREAK = val;
    }

    function set_button(ih, name, val) {
        class_name(ih,"button");
        if (name === "ACTIVE") {
            val = $to_bool(val);
            ih.disabled = !val;
//          puts(1,"?!IupButton(ACTIVE,"+val+")??\n"); // placeholder
        } else if (name === "GAP") {
            puts(1,"IupButton(GAP,"+val+")??\n"); // placeholder
        } else if (name === "EXPAND") {
            puts(1,"IupButton(EXPAND,"+val+")??\n"); // placeholder
//          expand(ih,val);
        } else if (name === "PADDING") {
            set_style(ih,"padding",val);
//          puts(1,"IupButton(PADDING,"+val+")??\n"); // placeholder
        } else if (name === "RUNNING") {
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
        } else {
            crash("IupStoreAttribute(IupButton,\"" + name + "," + val + "\") not yet implemented\n");
        }
    }
    store_attrs(["button"], ["ACTIVE","GAP","EXPAND","PADDING","RUNNING","TIP","IMAGE","IMPRESS","BGCOLOR"], set_button);
    store_attrs(["button"], ["LINEBREAK"], linebreak);
//ToDo: (found while surfin)
//button.setAttribute("disabled", "true");
//button.removeAttribute("disabled");
//[attribute]   [target]        Selects all elements with a target attribute            (PL: eg [inactive] ??)
//[attribute=value]     [target=_blank] Selects all elements with target="_blank"       (PL: eg [type=button] ??)

    function set_canvas(ih, name, val) {
        class_name(ih,"canvas");
        if (name === "BUFFER") {
            puts(1,"IupCanvas(BUFFER,"+val+")??\n"); // placeholder
        } else if (name === "BORDER") {
            puts(1,"IupCanvas(BORDER,"+val+")??\n"); // placeholder
//      } else if (name === "DATA") {
//          puts(1,"IupCanvas(DATA,"+val+")??\n"); // placeholder
        } else if (name === "EXPAND") {
            puts(1,"IupCanvas(EXPAND,"+val+")??\n"); // placeholder
        } else if (name === "BGCOLOR" ||
                   name === "BARMODE" ||
//                 name === "DATA" ||
                   name === "DRID" ||
                   name === "GTITLE" ||
                   name === "XNAME" ||
                   name === "YNAME" ||
                   name === "XRID" ||
                   name === "YRID" ||
                   name === "XTICKFMT" ||
                   name === "YTICKFMT") {
            ih[name] = val;
        } else if (name === "GRID") {
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
    store_attrs(["canvas"], ["BORDER","BUFFER","DATA","EXPAND","BGCOLOR","BARMODE","DRID","GRID",
                             "GTITLE","TITLESTYLE","XNAME","YNAME",
                             "DRAWCOLOR","DRAWSTYLE","DRAWFONT","DRAWTEXTORIENTATION",
                             "XANGLE","XCROSSORIGIN","XMARGIN","XMAX","XMIN","XRID","XTICK","XTICKFMT","XYSHIFT",
                             "YANGLE","YCROSSORIGIN","YMARGIN","YMAX","YMIN","YRID","YTICK","YTICKFMT","YXSHIFT",
                             "FONT","SCROLLBAR","DX","DY"], set_canvas);

    function set_datepick(ih, name, val) {
        class_name(ih,"datepick");
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
        class_name(ih,"menuitem");
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
        class_name(ih,"multibox");
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
        class_name(ih,"progress");
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
//              ih.style.flexDirection = 'column';
//          } else {
//              assert(val === "VERTICAL");
//              drag.className = "drag-x"
//              ih.style.flexDirection = 'row';
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
        class_name(ih,"split");
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
                ih.style.flexDirection = 'column';
            } else {
                assert(val === "VERTICAL");
                drag.className = "drag-x"
                ih.style.flexDirection = 'row';
            }
        } else if (name === "MINMAX") {
            puts(1,"IupSplit(MINMAX,"+val+")??\n"); // placeholder
        } else {
            crash("IupStoreAttribute(IupSplit,\"" + name + "\"," + val + ") not yet implemented\n");
        }
    }
    store_attrs(["split"], ["ORIENTATION","MINMAX"], set_split);

    function set_tabcontainer(ih, name, val) {
        class_name(ih,"tabcontainer");
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
        class_name(ih,"slider");
        if (name === "ORIENTATION") {
            puts(1,"IupValuator(ORIENTATION,"+val+")??\n"); // placeholder
            if (val === "VERTICAL") {
                ih.style.transform = "rotate(90deg)";
            } else if (val === "HORIZONTAL") {
                ih.style.transform = "";
            } else {
                crash("uh?");
            }
        } else if (name === "MAX") {
//          puts(1,"IupValuator(MAX,"+val+")??\n"); // placeholder
            ih.setAttribute('max', val);
        } else if (name === "MIN") {
//          puts(1,"IupValuator(MIN,"+val+")??\n"); // placeholder
            ih.setAttribute('min', val);
        } else if (name === "VALUE") {
//          puts(1,"IupValuator(VALUE,"+val+")??\n"); // placeholder
            ih.setAttribute('value', val);
        } else if (name === "STEP") {
//          puts(1,"IupValuator(STEP,"+val+")??\n"); // placeholder
            ih.setAttribute('step', val);
        } else if (name === "PAGESTEP") {
            puts(1,"IupValuator(PAGESTEP,"+val+")??\n"); // placeholder
//          ih.setAttribute('step', val);
        } else if (name === "EXPAND") {
            puts(1,"IupValuator(EXPAND,"+val+")??\n"); // placeholder
//          ih.setAttribute('step', val);
        } else if (name === "TIP") {
            puts(1,"IupValuator(TIP,"+val+")??\n"); // placeholder
//          ih.setAttribute('step', val);
        } else if (name === "CANFOCUS") {
            puts(1,"IupValuator(CANFOCUS,"+val+")??\n"); // placeholder
        } else {
            crash("IupStoreAttribute(IupValuator,\"" + name + "\"," + val + ") not yet implemented\n");
        }
    }
    store_attrs(["slider"], ["ORIENTATION","MAX","MIN","VALUE","STEP","PAGESTEP","EXPAND","TIP",
                             "CANFOCUS"], set_slider);

    function set_clipboard(ih, name, val) {
        class_name(ih,"clipboard");
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
        // do nothing... (it is already/permanently enabled in JvaScript)
    } else if (name === "SINGLEINSTANCE") {
        puts(1,"IupSetGlobal(" + name + "," + v + ")...\n");
    } else if (name === "INPUTCALLBACKS") {
        puts(1,"IupSetGlobal(" + name + "," + v + ")...\n");
    } else {
        crash("IupSetGlobal(" + name + "," + v + ")...");
    }
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
    const TIMER=0, FUNC=1, MSECS=2, ACTIVE=3, RUNNING=4, ID=5, CB=6
    if (cmd === "create") {

        //function IupTimer(/*cbfunc*/ func=NULL, /*integer*/ msecs=0, /*boolean*/ active=true) {
        //  return $timer("create",func,msecs,active);
        let func = ih, msecs = name, active = v;    // map args to more sensible names
        // (aside: ACTIVE is what we want it to be, RUNNING is what it actually is)
        function action_cb(timer) {
            if (!timer[ACTIVE]) { 
//              console.log("IupTimer clearInterval " + timer[ID]);
                timer[RUNNING] = false;
                clearInterval(timer[ID]);
            } else {
                let fn = timer[FUNC],
                    res = fn(timer);
    //          if (res === IUPCLOSE) { uh?? }
            }
        }
        let timer = ["timer",func,msecs,active,true,0,action_cb];
        timer[ID] = setInterval(action_cb,msecs,timer);
//      console.log("IupTimer setInterval " + timer[ID]);
        return timer;

    } else if (cmd === "is") {
        return Array.isArray(ih) && ih[TIMER] === "timer";
    } else if (cmd === "set") {
        if (name === "RUN") {
            if (v && !ih[RUNNING]) {
                ih[RUNNING] = true;
                ih[ID] = setInterval(ih[CB],ih[MSECS],ih);
            }
//          console.log("IupSetInt active:"+v);
            ih[ACTIVE] = v;
        } else if (name === "TIME") {
            ih[MSECS] = v;
            if (ih[RUNNING]) {
                ih[ID] = setInterval(ih[CB],ih[MSECS],ih);
            }
        } else {
            crash("timer??");
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
    let tv = typeof(v);
    if (tv !== "number" && tv !== "boolean" && tv !== "function") { crash("??"); }
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

function IupSetAttributePtr(ih, name, v) {
    ih[name] = v;
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
//      } else if (t === "hbox" ||
        } else if (t === "hbox" ) {
//                 t === "canvas") {
            return IupGetAttribute(ih.parentNode,name);
        }
//  } else if (t === "dialog") {
    } else if (name === "GTITLE" ||
               name === "XNAME" ||
               name === "YNAME") {
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
//          ih.value = 'paste';
            ih.value = "1";
            document.body.appendChild(ih);
            ih.style.display = "block";
            ih.select();
            ih.focus();
//DEV does not work:
//          document.execCommand("copy");
//                  if (document.execCommand !== undefined && document.execCommand("copy") !== true) {
//                      throw new Error("Unsupported 'copy' clipboard command");
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
let IupGetAttributePtr = IupGetAttribute;

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
        if (name === "SHIFTKEY") {
            return $shiftKey;
        }
        crash("IupGetInt(NULL,%s) not supported",["sequence",name]);
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
    } else if (t === "toggle") {
        if (name === "VALUE") {
//          val = $to_bool(val);
//DEV to_bool not in scope...? (is innerText actually so???) [IupToggle is ~1% implemented, if that]
//          let val = ih.innerText;
//          if (string(val)) { val = (val.toUpperCase()==="YES"); }
            return ih.innerText.toUpperCase() === "YES";
        }
    } else if (t === "canvas") {
        if (name === "DATA" ||
            name === "DRID" ||
            name === "GRID" ||
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
        }
    }
    crash("IupGetInt(%s,%s) not supported",["sequence",t,name]);
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
    let t = ih?ih.classList[0]:"NULL",
        n = ih_named?ih_named.classList[0]:"NULL";
    if (t === "dialog" && name === "MENU" && n === "submenu") {
        // ih_named is a ul.submenu...
        const mh = document.createElement("div"),
              mb = document.createElement("div"),
             nav = document.createElement("nav");
//<div id="mobbtn">&#9776;</div>
//<nav id="menu"><ul class="menubar">
//      mh.setAttribute('class', 'menuheader');
        mh.className = 'menuheader';
//      mh.id = 'menuheader';
        mb.id = 'mobbtn';
        mb.innerHTML = '&#9776;';
        nav.id = 'menu';
        nav.appendChild(ih_named);
        mh.appendChild(mb);
        mh.appendChild(nav);
        if (ih_named.className !== 'submenu') { crash("uh?"); }
//      ih_named.setAttribute('class', 'menubar');
        ih_named.className = 'menubar';
        let l = ih_named.children.length;
        for (let i = 0; i < l; i += 1) {
            let ci = ih_named.children[i],
                cn = ci.className;
            // quick sanity check:
            assert(cn === 'nestmenu' || cn === 'menuitem' );
            ci.className = 'topmenu';
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
                mstack.pop().classList.remove('open');
            }
            if (mitem!==null && (clicked || wasmlen)) {
                mitem.classList.toggle('open');
                if (mitem.classList.contains('open')) {
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
        let ml = ih_named.querySelectorAll('.topmenu');
        for (let i = 0; i < ml.length; i += 1) {
            let m = ml[i];
            m.addEventListener("click", (event) => li_click(event,m), false);
            m.addEventListener("mouseenter", () => { close_to(m,false); }, false);
        }
        let nm = ih_named.querySelectorAll('.nestmenu, .menuitem');
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

//      let menu = ih_named.getElementById('menu');
//      menu.addEventListener("contextmenu", (event)=> {event.preventDefault(); event.stopPropagation(); return false; });
        nav.addEventListener("contextmenu", (event)=> {event.preventDefault(); event.stopPropagation(); return false; });

        function window_resize() {
            hide_menu();
            //DEV/SUG calc the 640 from sum of toplevel sizes...
            if (window.innerWidth < 140) {
                nav.classList.add('mobile');
            } else {
                nav.classList.remove('mobile');
            }
        }   
        window_resize();
        ih.addEventListener("resize", window_resize);

//      let me = ih_named.querySelectorAll('.topmenu > span');

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
                        ih_named.querySelector('.topmenu > span').focus();
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
//      let mobbtn = ih_named.querySelector('#mobbtn');
//      ==> mb
        function mobbtn_click() {
            let mobile = ih.querySelector('.mobile');
            if (mb.classList.contains('active')) {
                mb.classList.remove('active');
                mb.innerHTML = "&#9776;";
                if (mobile) {
                    mobile.style.right = "-220px";
                }
                mb.style.right = "0";
            } else {
                mb.classList.add('active');
                mb.innerHTML = "&#9587;";
                if (mobile) {
                    mobile.style.right = "0";
                }
                mb.style.right = "220px";
            } 
        }
        mb.addEventListener("click", mobbtn_click);

//DEV is this not the same as/should be in doc_click??
//      let content = document.querySelector('.content');
//$('.content').on('click', function() { 
        function content_click() {
            if (mb.classList.contains('active')) {
                mb.classList.remove('active');
                mb.innerHTML = "&#9776;";
                let mobile = document.querySelector('.mobile');
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
                event.preventDefault();
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
                cn === "list") {
                ih.addEventListener("change",change_cb);
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
//              ih.addEventListener('click', action_cb);
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
//              ih.addEventListener('click', action_cb);
                ih.addEventListener('keydown', action_cb);
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
    ih.setAttribute('class', 'clipboard');
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
        if (event.key === 'Escape') {
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

//9/11/21 pretty sure I should be using css to resize...
//21/8/21:
function $resize_children(ih, w, h) {
//function $resize_children(ih, w, h, nest=0) {
    // recursively, honouring EXPAND (etc) and invoking any RESIZE_CB/REDRAW_CB
//  let cn = ih.classList[0];
    let cn = ih.classList[0] || ih.localName;
    if (cn !== "dialog-header" &&   // (there can be no ["ACTION"] attached
        cn !== "dialog-resizers") { //  to either, so simplify debugging..)
//if (nest===0) {
//printf(1,"resize_children(%s,%d,%d,%d)\n",["sequence",cn,w,h,nest]);
//}

        let bSizeChanged = false,
             bDoChildren = false,
//                prev_w = ih.width,
//                prev_h = ih.height;
                  prev_w,
                  prev_h;

        if (cn === "canvas") {
//          if (ih.width !== w || ih.height !== h) {
            prev_w = ih.width;
            prev_h = ih.height;
            if (prev_w !== w || prev_h !== h) {
                let ctx = ih.getContext("2d") || ih.getContext("webgl"),
                    // save, since they get trashed(!!):
                    fs = ctx.fillStyle,
                    ss = ctx.strokeStyle,
                    ta = ctx.textAlign,
                    tb = ctx.textBaseline;
    
                ih.width = w;
                ih.height = h;
//no diff:
//ctx.canvas.width  = w;
//ctx.canvas.height = h;
                ctx.fillStyle = fs;
                ctx.strokeStyle = ss;
                ctx.textAlign = ta;
                ctx.textBaseline = tb;
//              if (l) { crash("uh?"); }
                bSizeChanged = true;
            }
        } else if (cn === "container" ||
                   cn === "table") {
//          if (ih.width !== w || ih.height !== h) {
//          if (prev_w !== w || prev_h !== h) {
//              ih.width = w;
//              ih.height = h;
//              bSizeChanged = true;
//              l = 1; // (the table only)
//          }
/*
//no good: need to split between, or something...
// and, erm, which children are expandable? and what ratio should they get?
// append to affectedchildren if EXPAND in "YES" (both directions), "HORIZONTAL", 
//      "VERTICAL", ?"HORIZONTALFREE", "VERTICALFREE" [but not "NO" or undefined].
        } else if (cn === "vbox" ||
                   cn === "hbox") {
// not even sure this is a good idea...
//          if (ih.width !== w || ih.height !== h) {
            if (prev_w !== w || prev_h !== h) {
                ih.width = w;
                ih.height = h;
                bSizeChanged = true;    
//              bDoChildren = true;
            }
*/
//      } else {
//      } else if (cn === "menuheader") {
//          h -= 21;
        } else {
            prev_w = ih.offsetWidth,
//          prev_w = ih.offsetWidth-ih.offsetLeft,
//          prev_w = ih.clientWidth,
            prev_h = ih.offsetHeight;
//          prev_h = ih.offsetHeight-ih.offsetTop;
//          prev_h = ih.clientHeight;
//          w -= ih.offsetLeft;
            let nw = w-ih.offsetLeft,
                nh = h-(ih.offsetTop-45);
//          h += ih.offsetTop;
//          h += ih.offsetTop-45;   // well, it appears to work 21/10/21!! (on drag, that is)
//          if (prev_w !== w || prev_h !== h) {
            if (prev_w !== nw || prev_h !== nh) {
//              ih.offsetWidth = w;
//              ih.clientWidth = w;
//              ih.style.width = w + "px";
//              ih.offsetHeight = h;
//              ih.clientHeight = h;
//              ih.style.height = h + "px";
                bSizeChanged = true;    
//bollocks, see function expand(ih,val) {...
                let expand = ih.EXPAND;
                if (expand && expand !== "NO") {
//              if ((expand && expand !== "NO") ||
//                  (cn === "dialog-body")){
                    bDoChildren = true;
//X             w_children = repeat(0,l);
//X             h_children = repeat(0,l);
//X             if (expand === "YES") {
//
//DEV will probably get rid of this once demo/pGUI/sample.exw is done.
//          puts(1,"resize_children("+cn+")??\n"); // placeholder
                }
            }
        }

        if (bSizeChanged) {
            let resize_cb = ih.RESIZE_CB,
                action_cb = ih.ACTION;

            if (resize_cb) {
                resize_cb(ih,w,h);
            }
            if (action_cb && cn === "canvas") { 
                action_cb(ih); 
            }

//28/12/21:
//          if (bDoChildren) {
            if (bDoChildren || cn === "dialog" || cn === "dialog-body") {
//          if (false) {
                // for eg {button,fill,button,fill,button}, we want {0,1,0,2,0} and v_count of 2.
                // each non-zero gets (2-1+1)/2, (2-2+1)/1 of the remaining prev_w-w, iyswim.
                let w_children = [],    w_count = 0,    // width candidates
                    h_children = [],    h_count = 0,    // height candidates
                      children = ih.childNodes,
                  l = children.length;

                for (let i=0; i<l; i += 1) {
// what about w,h???
//crash("wtf?"); // (test!)
//              $resize_children(children[i]);
                    let ci = children[i],
                        expand = ci.EXPAND,
                        cin = ci.classList[0] || ci.localName;
//28/12/21:
                    if (cin === "dialog-body") {
                        expand = "YES";
                    } else if (!expand) {
                        let ccv = ci.classList.contains("expandv"),
                            cch = ci.classList.contains("expandh");
                        expand = ccv ? (cch ? "YES" : "HORIZONTAL")
                                     : (cch ? "VERTICAL" : "NO");
                    }
//fill
//DEV more complex: If User size is not defined, then when inside an IupHbox or IupGridBox EXPAND is HORIZONTAL, 
//                  when inside a IupVbox EXPAND is VERTICAL.If User size is defined then EXPAND is NO. 
//                  if (cn === "menuheader") {
//                      h -= 21;
//                  } else {
                        if (cin === "fill") {
                            if (cn === "hbox") {
                                expand = "HORIZONTAL";
                            } else if (cn === "vbox") {
                                expand = "VERTICAL";
                            }
                        }
                        if (expand && expand !== "NO") {
//Value: "YES" (both directions), "HORIZONTAL", "VERTICAL", "HORIZONTALFREE", "VERTICALFREE" or "NO".
//Default: "NO". For containers the default is "YES".
                            if (expand === "YES" ||
                                expand === "HORIZONTAL") {
                                w_count += 1;
                                w_children[i] = w_count;
                            }
                            if (expand === "YES" ||
                                expand === "VERTICAL") {
                                h_count += 1;
                                h_children[i] = h_count;
                            }
                        }
//                  }
                }
                if (w_count || h_count) {
//                  let rem_w = prev_w-w,
//                      rem_h = prev_h-h,
                    let rem_w = w-prev_w,
                        rem_h = h-prev_h,
                        wcrem = w_count,
                        hcrem = h_count;
                    for (let i=0; i<l; i += 1) {
                        let w = w_children[i],
                            h = h_children[i];
                        if (w || h) {
                            let ci = children[i],
//                              cw = ci.width,
//                              cw = ci.clientWidth,
                                cw = ci.offsetWidth,
//                              ch = ci.height;
//                              ch = ci.clientHeight;
                                ch = ci.offsetHeight;
                            if (w) {
                                let dw = ((w_count-w+1)/wcrem)*rem_w;
                                cw += dw;
                                rem_w -= dw;
                                wcrem -= 1;
                            }
                            if (h) {
                                let dh = ((h_count-h+1)/hcrem)*rem_h;
                                ch += dh;
                                rem_h -= dh;
                                hcrem -= 1;
                            }
                            $resize_children(ci,cw,ch);
//                          $resize_children(ci,cw,ch,nest+1);
                        }
                    }
                    if ((w_count !== 0 && (rem_w !== 0 || wcrem !== 0)) ||
                        (h_count !== 0 && (rem_h !== 0 || hcrem !== 0))) {
                        crash("uh?");
                    }
                }
            }
        }
    }
//  console.log("dialog.resize " + w + " " + h);
}

function $maxWindow(dialog) {
//  const maxbtn = dialog.getElementsByClassName("Maximise")[0],
    const maxbtn = dialog.querySelector(".Maximise");
    if (maxbtn.style.display !== "none") {
        const maxsvg = maxbtn.children[0],
//            dlgbod = dialog.getElementsByClassName("dialog-body")[0],
              dlgbod = dialog.querySelector(".dialog-body"),
              child = dlgbod.children[0];
        $topZindex(dialog);
        if (dialog.classList.toggle("window-maximized")) {
            dialog.wastop = dialog.style.top;
            dialog.wasleft = dialog.style.left;
            dialog.waswidth = dialog.style.width;
            dialog.washeight = dialog.style.height;
            dialog.style.top = "-1px";
            dialog.style.left = "-1px";
//          dialog.style.width = (dialog.maximumWidth || window.innerWidth) + "px";
            dialog.style.width = (dialog.maximumWidth || (window.innerWidth-4)) + "px";
//          dialog.style.height = (dialog.maximumHeight || window.innerHeight) + "px";
            dialog.style.height = (dialog.maximumHeight || (window.innerHeight-34)) + "px";
            maxbtn.title = "Restore";
        } else {
            dialog.style.top = dialog.wastop;
            dialog.style.left = dialog.wasleft;
            dialog.style.width = dialog.waswidth;
            dialog.style.height = dialog.washeight;
            maxbtn.title = "Maximise";
        }
        maxsvg.classList.toggle("restore");
//      $resize_children(child,$eWidth(dialog),$eHeight(dialog));
//              dialog.style.width = w + "px";
//              dialog.style.height = h + "px";
//              $resize_children(child,w-4,h-34);

//      $resize_children(child,$eWidth(dialog)-4,$eHeight(dialog)-34);
//      $resize_children(child,$eWidth(dialog)-4,$eHeight(dialog)-34-45);
//      $resize_children(child,$eWidth(dlgbod),$eHeight(dlgbod));
//getBoundingClientRect
        $resize_children(child,dlgbod.clientWidth-2,dlgbod.clientHeight-2);
//      $resize_children(child,dlgbod.clientWidth-5,dlgbod.clientHeight-5);
//      $resize_children(child,dialog.clientWidth-4,dialog.clientHeight-34);
//      const rect = dialog.getBoundingClientRect(),    // (nb: recalc in DOM)
//      const rect = dlgbod.getBoundingClientRect(),    // (nb: recalc in DOM)
//getComputedStyle
//               w = rect.width,
//               h = rect.height;
//      $resize_children(child,w-2,h-2);
    }
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
          dlgbod = document.createElement("div"),
          maxbtn = header_button("Maximise", "M10,10 L10,20 L20,20 L20,10 Z"),
          maxsvg = maxbtn.children[0],
          clsbtn = header_button("Close", "M10,10 L20,20 M20,10 L10,20");
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
    headiv.appendChild(maxbtn);
    headiv.appendChild(clsbtn);
    dlgbod.classList.add("dialog-body");
//  dlgbod.innerHTML = "<p>Move</p><p>this</p><p>div</p>";
//  dlgbod.innerHTML = "Move<br>this<br>div<br>";
    dlgbod.appendChild(child);
    dialog.className = "dialog";
    dialog.setAttribute('EXPAND', 'YES');
//26/5 (see if this helps with boids...)
//  dialog.setAttribute("data-sizing","intrinsic");
    dialog.appendChild(headiv);
    dialog.appendChild(dlgbod);

    function mouseDown(event) {
        event.preventDefault();  // prevent selection start (browser action)

        const rect = dialog.getBoundingClientRect(),
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

    function maxWindow() { $maxWindow(dialog); }

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
//              bResize = false;

            if (w >= dialog.maximumWidth) {
                w = dialog.maximumWidth;
            } else if (w >= dialog.minimumWidth) {
                if (resizer.xm === -1) {
                    // move left edge
                    xd += originalLeft;
                    if (xd < -1) {
                        w += xd + 1;
                        xd = -1;
                    }
                    dialog.style.left = xd + "px";
                } else if (originalLeft + w > window.innerWidth) {
                    // move right edge
                    w = window.innerWidth - originalLeft;
                }
//              dialog.style.width = w + "px";
//              bResize = true;
//              bResize = (w!=dialog.clientWidth);
//              if (w!=dialog.clientWidth) { bResize = true; }
            } else {
                w = dialog.minimumWidth;
            }

            if (h >= dialog.maximumHeight) {
                h = dialog.maximumHeight;
            } else if (h >= dialog.minimumHeight) {
                if (resizer.ym === -1) {
                    // move top edge
                    yd += originalTop;
                    if (yd < -1) {
                        h += yd + 1;
                        yd = -1;
                    }
                    dialog.style.top = yd + "px";
                } else if (originalTop + h > window.innerHeight) {
                    // move btm edge
                    h = window.innerHeight - originalTop;
                }
//              dialog.style.height = h + "px";
//              bResize = true;
//              bResize = (h!=dialog.clientHeight);
//              if (h!=dialog.clientHeight) { bResize = true; }
            } else {
                h = dialog.minimumHeight;
            }
//          if (bResize) {
            if (w !== dialog.clientWidth ||
                h !== dialog.clientHeight) {
                dialog.style.width = w + "px";
                dialog.style.height = h + "px";
                if (dialog.querySelector('.menuheader')) { h -= 21; }
                $resize_children(child,w-4,h-34);
//              $resize_children(dialog.children[1],w-4,h-34);
//              $resize_children(child,w-4,h-34-45);
            }
        }

//ditto $docBody??
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
        r.onmousedown = initSize;
//      r.addEventListener("mousedown", initSize, false);
        rdiv.appendChild(r);
//    });
    }
    rd.forEach(addSizer);

    rdiv.className = "dialog-resizers"; // (so they can be hidden/disabled)

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

//21/8/21: (bust)
/*
    dialog.onresize = function() {
//      ih.width = this.innerWidth;
//  c.height = this.innerHeight;
        console.log("dialog_resize?");
    }
*/

    return dialog;
} // (IupDialog() ends...)

function IupMap(ih) {
    let cn = ih.className;
    if (cn !== "dialog-header" &&   // (there can be no ["MAP_CB"] attached
        cn !== "dialog-resizers") { //  to either, so simplify debugging..)
        let map_cb = ih["MAP_CB"];
//28/12/21: (decided against)
//          resize_cb = ih.RESIZE_CB;
        if (map_cb) { 
            map_cb(ih); 
            ih["MAP_CB"] = null;
        }
//      if (resize_cb) {
//>>
//          resize_cb(ih,w,h);
//      }
        let children = ih.childNodes,
            l = children.length;
        for (let i=0; i<l; i += 1) { IupMap(children[i]); }
    }
}

function IupRedraw(ih) {
    let cn = ih.className;
    if (cn !== "dialog-header" &&   // (there can be no ["ACTION"] attached
        cn !== "dialog-resizers") { //  to either, so simplify debugging..)
//      let action = ih["REDRAW_CB"];
        let action = ih["ACTION"];
        if (action && cn === "canvas") { 
            action(ih); 
//          ih["MAP_CB"] = null;
        }
        let children = ih.childNodes,
            l = children.length;
        for (let i=0; i<l; i += 1) { IupRedraw(children[i]); }
    }
}
let IupRefresh = IupRedraw;
let IupRefreshChildren = IupRedraw;

function IupShow(ih, x = IUP_CENTER, y = IUP_CENTER) {
//  IupMap(ih);
    // Make it top dog, and add it to the DOM.
    ih.style.zIndex = document.getElementsByClassName("dialog").length;
// 25/9/21: (for "save as" handling)
//  $docBody.appendChild(ih);
    $docBody.insertAdjacentElement("afterbegin", ih);
    IupMap(ih);
//$resize_children(ih,ih.clientWidth-2,ih.clientHeight-2);

    // Originally, dragging a window behaved rather differently before and
    // after resizing the window, hence this...
    const rect = ih.getBoundingClientRect();    // (nb: recalc in DOM)
    let w = rect.width,
//      w = floor(rect.width),
        h = rect.height;
//      h = floor(rect.height);
    ih.style.width = w + "px";
    ih.style.height = h + "px";
    if (x && y) {
        if (x === IUP_CENTER) { x = (window.innerWidth-w)/2; }
        if (y === IUP_CENTER) { y = (window.innerHeight-h)/2; }
        ih.style.left = x + "px";
        ih.style.top = y + "px";
    }
//28/12/21: (total bust...)
//  $resize_children(ih, w, h);
//  const r2 = ih.getBoundingClientRect();
//  $resize_children(ih, r2.width, r2.height);
//  IupUpdate(ih);
//  const dlgbod = ih.querySelector(".dialog-body");
//  $resize_children(ih,dlgbod.clientWidth-2,dlgbod.clientHeight-2);
//  $resize_children(ih,ih.clientWidth-2,ih.clientHeight-2);
//          if (w !== dialog.clientWidth ||
//              h !== dialog.clientHeight) {
//              dialog.style.width = w + "px";
//              dialog.style.height = h + "px";
//              if (dialog.querySelector('.menuheader')) { h -= 21; }
//              $resize_children(child,w-4,h-34);
//
//  let resize_cb = ih.RESIZE_CB;
//  if (resize_cb) {
//      resize_cb(ih,w,h);
//  }
    // It may be possible to allow some squishing, not entirely sure...
//26/9/21:
    if (ih.minWidth) { w = ih.minWidth; }
    if (ih.minHeight) { h = ih.minHeight; }
    ih.minimumWidth = w;                        // for use in doSize()
    ih.minimumHeight = h;                       //      """
//  ih.onresize(ih,w,h);
//--DEV REDRAW_CB? ACTION? (triggers violation too)
//  const event = new Event('resize');
//  ih.dispatchEvent(event);
//  window.setTimeout(ih.dispatchEvent,10,event);
//  window.setTimeout(function() { ih.dispatchEvent(event); }, 100);
    IupRedraw(ih);
//  ih.focus();
//  const r2 = ih.getBoundingClientRect();
//  $resize_children(ih, r2.width, r2.height);
// 31/21/21: bingo!!
    $maxWindow(ih);
    $maxWindow(ih);
}
let IupShowXY = IupShow;

function IupSetFocus(ih) {
    ih.focus();
}

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

function IupGetChild(/*Ihandle*/ ih, /*integer*/ pos) {
    return ih.children[pos];
}

function IupGetChildCount(/*Ihandle*/ ih) {
    return ih.childElementCount;
}

function IupGetClassName(/*Ihandle*/ ih) {
    return ih.classList[0];
}

function IupGetDialog(ih) {
//  while (true) {
//      ih = ih.parentNode;
    let parent = ih.offsetParent;
//  while (parent === null) { // nb not NULL!
    while (parent === null || parent.classList[0] !== "dialog") { // nb not NULL!
        ih = ih.parentNode;
        if (ih.classList[0] === "dialog") {
            parent = ih;
            break;
        }
    }
//      if (ih.!="dialog") { crash("uh?"); }
//  $class_name(parent, "dialog");
    assert(parent.classList[0] === "dialog");
    return parent;
}

function IupGetDialogChild(/*Ihandle*/ ih, /*string*/ name) {
    function get_child(ih, name) {
        let count = ih.childElementCount;
        for (let pos = 0; pos < count; pos += 1) {
            let cp = ih.children[pos];
            if (cp['NAME'] === name) { return cp; }
            cp = get_child(cp,name);
            if (cp) { return cp; }      
        }
        return NULL;
    }
    if (ih.classList[0] !== "dialog") {
        ih = IupGetDialog(ih);
    }
    return get_child(ih,name);
}

function IupGetFocus() {
    return document.activeElement;
}

function IupGetParent(ih) {
    return ih.parentNode || NULL;
}

function IupGetBrother(ih, bPrev=false) {
    let parent = ih.parentNode,
//      siblings = parent.children,
        siblings = [...parent.children],
        pos = siblings.indexOf(ih),
        brother = siblings[bPrev?pos-1:pos+1];
    return brother;
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
        IupSetCallback(ih, "ACTION", func);
//      btn.addEventListener('click', toggleHide);
    }
    IupSetAttributes(ih, attributes, args);
    return ih;
}

const IupFlatButton = IupButton;

function IupDatePick(action = null, func = null, attributes = "", args = []) {
    const ih = document.createElement("input");
    ih.setAttribute('class', 'datepick');
    ih.setAttribute('type', 'date');
    [action,func,attributes,args] = $paranormalise(action,func,attributes,args);
    if (func) {
        if (action !== null && action !== "VALUECHANGED_CB") { crash("?9/0"); }
        IupSetCallback(ih, "VALUECHANGED_CB", func);
    }
    IupSetStrAttribute(ih,"VALUE","%4d-%02d-%02d",date());
    IupSetAttributes(ih, attributes, args);
    return ih;
}

//DOC: [RASTER]SIZE must use wxh format, one of which will be ignored... or... must be 0.
//      Any attributes must be set before the element is inserted into the hierarchy (not normally an issue).
function IupFill(attributes = "", args = []) {
    const ih = document.createElement("div");
    ih.setAttribute('class', 'fill');
//DEV more complex: If User size is not defined, then when inside an IupHbox or IupGridBox EXPAND is HORIZONTAL, 
//                  when inside a IupVbox EXPAND is VERTICAL.If User size is defined then EXPAND is NO. 
//  ih.setAttribute('EXPAND', 'YES');
    IupSetAttributes(ih, attributes, args);
    return ih;
}

function IupFlush() { }

function IupFrame(child = NULL, attributes = "", args = []) {
    const ih = document.createElement("fieldset"),
          legend = document.createElement("legend");
    ih.setAttribute('class', 'frame');
//  ih.setAttribute('EXPAND', 'YES');
    ih['EXPAND'] = 'YES';
    ih.appendChild(legend);
    if (child) {
        ih.appendChild(child);
    }
    IupSetAttributes(ih, attributes, args);
    return ih;
}

function IupHbox(children, attributes = "", args = []) {
//  if (!Array.isArray(children)) {
    if (!sequence(children)) {
//      crash('vbox children must be an array!');
        crash('hbox children must be a sequence!');
    }
    const ih = document.createElement("div");
    ih.setAttribute('class', 'hbox');
//  ih.setAttribute('EXPAND', 'YES');
    ih['EXPAND'] = 'YES';
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
//  ih.setAttribute('EXPAND', 'YES');
    ih['EXPAND'] = 'YES';
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

//IupFlatLabel?? (mnemonics not supported...???)
function IupLabel(title=null, attributes = "", args = []) {
    const ih = document.createElement("div");
    ih.setAttribute('class', 'label');
//  ih['EXPAND'] = 'YES';
    if (title) {
//      ih.innerHTML = title;
        if (title.indexOf('\n') === -1) {
            title = "<nobr>" + title + "</nobr>";
        } else {
            const lf = new RegExp("\\n","g");
            title = title.replace(lf,"<br>");
        }
        ih.innerHTML = title;
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

function IupList(action = null, func = null, attributes = "", args = []) {
    const ih = document.createElement("select");
    ih.setAttribute('class', 'list');
    [action,func,attributes,args] = $paranormalise(action,func,attributes,args);
    if (func) {
        if (!action) {
            action = "ACTION";
        } else if (action !== "VALUECHANGED_CB" &&
                   action !== "ACTION") {
            crash("?9/0");
        }
        IupSetCallback(ih, action, func);
    }
    IupSetAttributes(ih, attributes, args);
    return ih;
}

/*
  <div id="menuheader">
    <div id="mobbtn">&#9776;</div>
    <nav id="menu">
      <ul class="menubar">                                          -- (Menu)
        <li class="topmenu">                                        -- (SubMenu)
          <span tabindex="0"><span class="malt">F</span>ile</span>
          <ul class="submenu">                                      -- (Menu)
//         <li class="menuitem"><span><i></i>New</span></li>        -- (MenuItem)
//         <li class="separator"></li>                              -- (Separator)
           <li class="nestmenu">                                    -- (SubMenu)
            <span><i></i>Paste</span>
            <ul class="submenu">                                    -- (Menu)
//           <li class="menuitem"><span><i></i>Normal</span></li>
//           <li class="nestmenu">                                  -- (SubMenu)
//            <span><i></i>Circle</span>
//            <ul class="submenu">                                  -- (Menu)
//             <li class="separator"></li>
//            </ul>
//           </li>
            </ul>
           </li>
          </ul>
        </li>
        <li class="topmenu"><span tabindex="0"><span class="malt">H</span>elp</span></li>
      </ul>
    </nav>
  </div>    
*/

function $menutitle(ih,title) {
    if (title) {
        let adx = title.indexOf('&');
        if (adx !== -1) {
            // replace eg '&File' with '<span class="malt">F</span>ile'
            title = title.slice(0,adx) + '<span class="malt">' + title.slice(adx+1,adx+2) + '</span>' + title.slice(adx+2);
//      } else {
//          title = "<i></i>" + title;
        }
//  } else {
//      title = "<i></i>"
    }
//  ih.innerHTML = title;
    ih.innerHTML = "<i></i>" + title;
}

function IupMenu(/*Ihandles*/ children=[], /*sequence*/ attributes="", /*dword_seq*/ args=[]) {
// Note: the class of a menu is "submenu", maybe replaced with "menubar" later, whereas
//       the class of a submenu is "nestmenu", maybe replaced with "topmenu" later...
//atom pChildren = iup_ptr_array(children)
//Ihandle ih = c_func(xIupMenuv, {pChildren})
//  free(pChildren)
//DEV NO, this needs to be done at the IupSetAttributeHandle(dlg,"MENU",menu) stage:
/*
    const mh = document.createElement("div"),
          mb = document.createElement("div"),
         nav = document.createElement("nav"),
          ul = document.createElement("ul");
//<div id="mobbtn">&#9776;</div>
//<nav id="menu"><ul class="menubar">
//  ih.setAttribute('class', 'menuheader');
    ih.id = 'menuheader';
    mb.id = 'mobbtn';
    mb.innerHTML = '&#9776;';
    nav.id = 'menu';
//  ul.setAttribute('class', 'menubar');
    ul.className = 'menubar';
           <li class="nestmenu">
            <span><i></i>Create</span>
            <ul class="submenu">

           <li class="nestmenu">
            <span><i></i>Paste</span>
            <ul class="submenu">
//           <li class="menuitem"><span><i></i>Normal</span></li>
//           <li class="menuitem"><span><i></i>html-stripped</span></li>
            </ul>
           </li>
              <ul class="submenu">
               <li class="menuitem"><span><i></i>Normal</span></li>
               <li class="menuitem"><span><i></i>Dotted</span></li>
               <li class="separator"></li>
               <li class="menuitem"><span><i></i>Filled</span></li>
              </ul>

*/
//  const ih = document.createElement("li"),
//        sp = document.createElement("span");
    const ih = document.createElement("ul");
    ih.className = 'submenu';   // (maybe replaced with 'topmenu' later, along with a tabindex of 0)
    if (children.length) {
        let l = length(children);
        for (let i = 1; i <= l; i += 1) {
            let ci = children[i];
//DEV NO, this needs to be done at the IupSetAttributeHandle(dlg,"MENU",menu) stage:
/*
            cs = ci.children[0];
        ci.className = 'topmenu';
        cs.tabIndex = 0;
*/
            ih.appendChild(ci);
        }
    }
//  nav.appendChild(ul);
//  ih.appendChild(mb);
//  ih.appendChild(nav);
    IupSetAttributes(ih, attributes, args);
//  if length(attributes) then
//      IupSetAttributes(ih, attributes, args)
//  end if
    return ih;
}

function IupMenuItem(/*nullable_string*/ title=NULL, /*object*/ action=NULL, func=NULL, /*sequence*/ attributes="", args=[]) {
//<li class="menuitem"><span><i></i>Normal</span></li>
    [action,func,attributes,args] = $paranormalise(action,func,attributes,args);
    const ih = document.createElement("li"),
          sp = document.createElement("span");
//        ii = document.createElement("i");
//  ih.setAttribute('class', 'menuitem');
    ih.className = 'menuitem';
//  sp.appendChild(ii);
    $menutitle(sp,title);
    ih.appendChild(sp);
//  Ihandle ih = c_func(xIupItem, {title, action})
//  
//  if (func !== NULL) {
//  if (func !== null) {
    if (func) {
//      if (action === NULL) {
        if (!action) {
            action = "ACTION";
        }
        IupSetCallback(ih, action, func);
    }
//  if length(attributes) then
//      IupSetAttributes(ih, attributes, args)
//  end if
    IupSetAttributes(ih, attributes, args);
    return ih;
}
let IupItem = IupMenuItem;

function IupSeparator() {
//  Ihandle ih = c_func(xIupSeparator, {})
//<li class="separator"></li>
    const ih = document.createElement("li");
    ih.className = 'separator';
    return ih;
}

function IupSubmenu(/*nullable_string*/ title=NULL, /*Ihandln*/ menu=NULL, /*string*/ attributes="", /*sequence*/ args=[]) {
    const ih = document.createElement("li"),
          sp = document.createElement("span");
//        ul = document.createElement("ul");

    ih.className = 'nestmenu';   // (maybe replaced with 'topmenu' later)
    $menutitle(sp,title);
    ih.appendChild(sp);
    ih.appendChild(menu);
//  ih.appendChild(ul);
//           <li class="nestmenu">
//            <span><i></i>Triangle</span>
//            <ul class="submenu">
//             <li class="menuitem"><span><i></i>Equilateral</span></li>
//             <li class="menuitem"><span><i></i>Isoceles</span></li>
//             <li class="menuitem"><span><i></i>Scalenus</span></li>
//            </ul>
//           </li>

//      <li class="topmenu">
//        <span tabindex="0"><span class="malt">E</span>dit</span>
//        <ul class="submenu">
//         <li class="menuitem"><span><i></i>Copy</span></li>
//         <li class="nestmenu">
//          <span><i></i>Paste</span>
//          <ul class="submenu">
//           <li class="menuitem"><span><i></i>Normal</span></li>
//           <li class="menuitem"><span><i></i>html-stripped</span></li>
//          </ul>
//         </li>

//  Ihandle ih = c_func(xIupSubmenu, {title, menu})
//  if length(attributes) then
//      IupSetAttributes(ih, attributes, args)
//  end if
    IupSetAttributes(ih, attributes, args);
    return ih;
}
let IupSubMenu = IupSubmenu;

//DEV re-do the snapshots in the manual...
function IupMultiBox(children, attributes = "", args = []) {
    if (!sequence(children)) {
//      children = IupGetChildren(children);
        crash('multibox children must be a sequence!');
    }
//DEV (and elsewhere)
//  assert(sequence(children),
    const ih = document.createElement("div");
    ih.setAttribute('class', 'multibox');
    ih['EXPAND'] = 'YES';
//  ih.className = "multibox";
    let l = length(children);
    for (let i = 1; i <= l; i += 1) {
        let ci = children[i];
        ih.appendChild(ci);
    }
    IupSetAttributes(ih, attributes, args);
    return ih;
}

function IupProgressBar(attributes = "", args = []) {
    const ih = document.createElement("progress");
    ih.setAttribute('class', 'progress');
    ih.setAttribute('min', 0);
    ih.setAttribute('max', 1);
    IupSetAttributes(ih, attributes, args);
    return ih;
}


//DEV IupSplit.
// see file:///E:/downloads/misc/js/vanillajs/vanillajs-resizable-panes/dist/index.html
//C:\Program Files (x86)\Phix\demo\rosetta\hexapawn.exw:538           left = IupSplit(board_frame,moves_frame,"ORIENTATION=HORIZONTAL,MINMAX=100:900"),
//C:\Program Files (x86)\Phix\demo\rosetta\hexapawn.exw:540           full = IupSplit(left,right,"ORIENTATION=VERTICAL,MINMAX=100:900")
function IupSplit(/*Ihandln*/ child1=NULL, child2=NULL, /*string*/ attributes="", /*sequence*/ args=[]) {
//
// Creates something like this:
//
//  <div class="split">
//    (child1)
//    <div class="drag-y"></div>    -- (or drag-x iff HORIZONTAL/VERTICAL)
//    (child2)
//  </div>
//
    let ih = document.createElement(`div`),
      drag = document.createElement(`div`);
    ih.style.display = 'flex';
    ih.style.flexDirection = 'column';  // ('row' if HORIZONTAL)
    ih.EXPAND = "YES"
    ih.className = "split";
    drag.className = "drag-y";          // ('drag-x' if HORIZONTAL)
//DEV??
//  ih.style.height = '100vh';
    child1.style.flex = '0 0 50%';
    child2.style.flex = '1';
    ih.appendChild(child1);
    ih.appendChild(drag);
    ih.appendChild(child2);

    let startX = 0,
        startY = 0,
        dragElement;
//   _leftPane = document.querySelector(".box-1");
//var _rightPane = document.querySelector(".box-2");
//var _container = document.querySelector(".main-holder");  // the div split...
//var _topPane = document.querySelector(".top-pane");

//InitDragDrop();

//console.log(document)

//function InitDragDrop(){
//  drag.onmousedown = OnMouseDown;
//  drag.onmouseup = OnMouseUp;
//}

    function OnMouseMoveX(e){
        if (e == null)
            var e = window.event; 
//      _leftPane.style.flex = '0 0' + (e.clientX/(_container.clientWidth/100)) + '%';
        child1.style.flex = '0 0' + (e.clientX/(ih.clientWidth/100)) + '%';
    }

    function OnMouseMoveY(e){
        if (e == null)
            var e = window.event; 
//      _topPane.style.flex = '0 0' + (e.clientY/(_container.clientHeight/100)) + '%';
        child1.style.flex = '0 0' + (e.clientY/(ih.clientHeight/100)) + '%';
    }

    function ff() { return false; }

    function OnMouseDown(e){
//DEV??
        if (e == null) {
            e = window.event; 
        }
    
//      var target = e.target != null ? e.target : e.srcElement;
        let target = e.target;
    
        if ((e.button == 1 && window.event != null || e.button == 0) && target.className == 'drag-x') {
            startX = e.clientX;
            dragElement = target;
            document.onmousemove = OnMouseMoveX;
            document.body.focus();
            document.onselectstart = ff;
            target.ondragstart = ff;
            return false;
        } else if ((e.button == 1 && window.event != null || e.button == 0) && target.className == 'drag-y') {
            startY = e.clientY;
            dragElement = target;
            document.onmousemove = OnMouseMoveY;
            document.body.focus();
            document.onselectstart = ff;
            target.ondragstart = ff;
            return false;
        }
    }

    function OnMouseUp(e){
        if (dragElement != null){
            document.onmousemove = null;
            document.onselectstart = null;
            dragElement.ondragstart = null;
            dragElement = null;
        }
    }

    drag.onmousedown = OnMouseDown;
//  drag.onmouseup = OnMouseUp;
    document.onmouseup = OnMouseUp;

    IupSetAttributes(ih, attributes, args);
    return ih;
}


//DEV erm, hello, where are IupTableClick_cb(), IupTableEnterItem_cb(), IupTableResize_cb(),
//                          IupTableGetSelected(), IupTableGetData(), IupTableSetData()???
function IupTable(columns, data, visible=10, attributes="", args=[]) {
    let table = document.createElement(`table`),
        container = document.createElement(`div`);  // wraps table+resizers
    table.style.height = visible*23+27 + 'px';
//  table.classList = 'table';
    container.appendChild(table);
    container.classList = 'container';
    container.style.width = '100%';
    container.style.height = '100%';

    let nColumns = length(columns),
        nRows = length(data[1]),
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
        if (c<=l2) {
            let d2c = data[2][c];
            if (string(d2c)) {
                return sprintf(d2c,data[1][ti][c]);
            } else if (sequence(d2c)) {
                return d2c[ti];
            } else if (typeof(d2c) === "function") {
                return d2c(data[1],ti,c);
            }
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
            tableHeight = $eHeight(tbody);
        if (bPageKey) {
            if (typeof(bPageKey)==="string") { return true; } // ditto
            tbody.scrollTop += bPageKey*tableHeight+bPageKey;
            return false;
        }
        // scrolling magic
        let rowTop = trNew.offsetTop - tbody.scrollTop - tbody.offsetTop,
            rowBtm = rowTop + $eHeight(trNew);
        if (rowTop < 0) {
            tbody.scrollTop += rowTop;                  // scroll up
        } else if (rowBtm > tableHeight) {
            tbody.scrollTop += rowBtm - tableHeight;    // scroll down
        }
        return false;
    }

    function trPage(trSel, direction) {
        let n = Math.floor($eHeight(trSel.parentNode)/$eHeight(trSel));
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
//          tbody.classList = 'tbody';
//23/8/21 (when making KEY_CB work... put back if focus goes wrong)
//          tbody.setAttribute('tabindex', "0");
            tbody.setAttribute('tabindex', "-1");
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
            leftWidth = $eWidth(left_th);
            rightWidth = $eWidth(right_th);
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
            leftPos += $eWidth(table_th[i-1])+11;
            resizer.style.left = (leftPos-3)+"px";
//          resizer.addEventListener("mousedown", rd_mouseDown);
            resizer.onmousedown = rd_mouseDown;
            container.appendChild(resizer);
            resizer_array.push(resizer);
        }
//      table.addEventListener("mousemove", mouseMove);
        table.onmousemove = mouseMove;
//      document.addEventListener("mouseup", mouseUp);
        $docBody.addEventListener("mouseup", mouseUp);
    }

    set_intersection_observer();
    populate_table();
    createResizeDivs(container,table);
    table.onkeydown = table_keyhandler;
//  table.addEventListener("keydown", table_keyhandler);

    return container;
}

function IupTableGetSelected(/*Ihandle*/ table) {
//  crash("testme");
    puts(1,"IupTableGetSelected\n");
//  let tr = table.parentNode.querySelector('.trActive'),
    let tr = table.querySelector('.trActive'),
        idx = tr.id;
//      idx = tr.id || 0;
//  integer idx = IupGetInt(table,"FOCUSCELL") -- (line only)
//  if idx!=0 then
//      integer dsidx = IupGetInt(table,"DSIDX")
//      idx = table_tagsets[dsidx][idx]
//  end if
    return idx;
}

function IupTableClearSelected(/*Ihandle*/ table) {
//  crash("testme");
    puts(1,"IupTableClearSelected\n");
//  let tr = table.parentNode.querySelector('.trActive'),
    let tr = table.querySelector('.trActive');
    if (tr) {
        tr.classList.remove('trActive');
    }
}

function IupTableClick_cb(/*Ihandle*/ table, /*integer*/ l, c/*, atom pStatus*/) {
// default CLICK_CB for IupTable (callable directly, when overridden)
    puts(1,"IupTableClick_cb\n"); // placeholder
/*
    integer numcol = IupGetInt(table,"NUMCOL")
    if l=0 and c>0 and c<=numcol then
        dsidx = IupGetInt(table,"DSIDX")
        sequence data = table_datasets[dsidx],
                 cols = table_sortcols[dsidx]
        integer sel = IupTableGetSelected(table),
                  k = find(c,cols)
        integer sortcol = iff(length(cols)?cols[1]:0)
        if k=1 then
            table_sortdirs[dsidx][1] *= -1
        else
            if k then
                table_sortcols[dsidx][k..k] = {}
                table_sortdirs[dsidx][k..k] = {}
            end if
            table_sortcols[dsidx] = prepend(table_sortcols[dsidx],c)
            table_sortdirs[dsidx] = prepend(table_sortdirs[dsidx],1)
        end if
        if sortcol!=0 and sortcol!=c then
            IupSetAttributeId(table,"SORTSIGN",sortcol,"NO")
        end if
        integer sortdir = iff(IupGetAttributeId(table,"SORTSIGN",c)="DOWN"?-1:1)
        IupSetAttributeId(table,"SORTSIGN",c,iff(sortdir=-1?"UP":"DOWN"))
        table_tagsets[dsidx] = custom_sort(routine_id("by_column"),table_tagsets[dsidx])
        IupSetAttribute(table,"REDRAW","ALL")
        -- restore selection - it stays off-screen, but user can use up/down
        --  to force it into the viewport, should they be inclined to do so.
        sel = find(sel,table_tagsets[dsidx])
        {} = IupTableEnterItem_cb(table, sel, 1)
    end if
*/
    return IUP_DEFAULT;
}

function IupTabs(/*Ihandles*/ children = [], /*string*/ attributes="", /*sequence*/ args = []) {
//
// Creates something like this:
//
//   <div class="tabcontainer">
//    <ul class="tabs">
//      <li class="tab active"> (children[1].TABTITLE) </li>
//      <li class="tab"       > (children[2].TABTITLE) </li>
//    </ul>
//    <div class="panels">
//      <div class="panel active"> (children[1]) </div>
//      <div class="panel"       > (children[2]) </div>
//    </div>
//   </div>
//
//maybe?:
//  if (!Array.isArray(children)) {
//      children = ["sequence",children];
//  }
//or: (though children.length crashing is probably just as good)
//  assert(Array.isArray(children),"IupTabs children must be a sequence!");
    const ih = document.createElement("div"),
        tabs = document.createElement("ul"),
      panels = document.createElement("div");
    ih.setAttribute('class', 'tabcontainer');
    tabs.setAttribute('class', 'tabs');
    panels.setAttribute('class', 'panels');
//  ih.setAttribute('EXPAND', 'YES');
    ih['EXPAND'] = 'YES';
//  ih.className = "tabs";

    function tabclick(event) {
        let tgt = event.target;
        if (tgt.classList.contains('tab')) {
            let tabul = tgt.parentNode,                 // the <ul tabs> (lone)
                tabch = [...tabul.children],            // the <li tab>s (Array)
                 pdiv = tabul.parentNode.children[1],   // the <div panel>s
                  act = tabul.querySelector('.active'),
                  adx = tabch.indexOf(act),
                  apt = pdiv.children[adx],
                  tdx = tabch.indexOf(tgt),
                  pnl = pdiv.children[tdx];
            act.classList.remove('active');
            apt.classList.remove('active');
            tgt.classList.add('active');
            pnl.classList.add('active');
        }
    }

    let l = children.length;
    for (let i = 1; i < l; i += 1) {
        let ci = children[i],
            li = document.createElement("li"),
            pi = document.createElement("div");
        li.innerText = ci.TABTITLE;
        li.setAttribute('class', 'tab');
        pi.setAttribute('class', 'panel');
        if (i === 1) {
            li.classList.add('active');
            pi.classList.add('active');
        }
        li.addEventListener('click', tabclick);
        pi.appendChild(ci);
        tabs.appendChild(li);
        panels.appendChild(pi);
    }
    ih.appendChild(tabs);
    ih.appendChild(panels);
    IupSetAttributes(ih, attributes, args);
    return ih;
}       

function IupText(action=null, func=null, attributes = "", args = []) {
    const ih = document.createElement("input");
    ih.setAttribute('class', 'text');
    [action,func,attributes,args] = $paranormalise(action,func,attributes,args);
    if (func) {
        if (!action) { action = "ACTION"; }
        IupSetCallback(ih,action,func);
//          if (action === "VALUECHANGED_CB") {
//  //          printf(1,"warning: IupText(VALUECHANGED_CB) not yet supported\n");
//              IupSetCallback(ih,action,func);
//          } else {
//              if (action !== null && action !== "ACTION") { crash("?9/0"); }
//  //          if (!action) { action = "ACTION"; }
//              function action_cb(event) {
//                  puts(1,"action_cb");    // placeholder...
//  //              let c = ??,
//  //                  pNewValue = ??,
//  //                  res = func(/*Ihandle*/ ih, /*integer*/ c, /*atom*/ pNewValue);
//  //              if (res===IUP_IGNORE) {
//  //              }
//              }
//              ih.onkeyup = action_cb;
//  //          ih.addEventListener('keyup', action_cb);
//          }
    }
    IupSetAttributes(ih, attributes, args);
    return ih;
}       

//DEV document this, MULTILINE not supported.
//(might also warrant a class of "multiline")
function IupMultiLine(action=null, func=null, attributes = "", args = []) {
//  let ih = IupText(action, func, attributes, args);
//  IupSetAttribute(ih, "MULTILINE", "YES")
    const ih = document.createElement("textarea");
    ih.setAttribute('class', 'text');
    [action,func,attributes,args] = $paranormalise(action,func,attributes,args);
    if (func) {
        if (!action) { action = "ACTION"; }
        IupSetCallback(ih,action,func);
    }
    IupSetAttributes(ih, attributes, args);
    return ih;
}       

let $radio_id = 1;

function IupRadio(/*Ihandln*/ child=NULL, /*string*/ attributes="", /*sequence*/ args=[]) {
    const ih = document.createElement("div"),
        name = "radio"+$radio_id;
    $radio_id += 1;
    function radiate(ih) {
        let cn = ih.classList[0];
        if (cn === "toggle") {
            let input = ih.children[1];
            assert(input.type === "checkbox");
            input.type = "radio";
            input.name = name;
        } else {
            let l = ih.children.length;
            for (let i = 0; i < l; i += 1) {
                radiate(ih.children[i]);
            }
        }
    }
    radiate(child);
    ih.appendChild(child);
    IupSetAttributes(ih, attributes, args);
    return ih;
}       

let $toggle_id = 1;

function IupToggle(title=null, action=null, func=null, attributes = "", args = []) {
//<label accesskey="s" for="strict-mode"><input type="checkbox" id="strict-mode" /> <u>S</u>trict Mode</label accesskey="x">
//  const ih = document.createElement("div");
    const ih = document.createElement("label"),
          cb = document.createElement("input"),
          span = document.createElement("span"),
          tid = "toggle"+$toggle_id;
    $toggle_id += 1;
    cb.type = "checkbox";
    cb.id = tid;
    ih.for = tid;   
    ih.setAttribute('class', 'toggle');
    if (title) {
//      ih.innerHTML = title;
        let adx = title.indexOf('&');
        if (adx !== -1) {
            crash("not yet done...");
//          let key = title[adx];
//          title = title.slice(0,adx) + "<u>" + key + "</u>" + title.slice(adx+1);
//          lbl.accesskey = key.toLowerCase();
        }
        span.innerHTML = "<nobr>" + title + "</nobr>";
//DEV:
//      IupSetAtribute(ih,"TITLE",title);
    }
//  ih.addChild
    ih.appendChild(span);
    ih.appendChild(cb);
    ih.style.display = "flex";
    ih.style.flexDirection = "row-reverse";
    [action,func,attributes,args] = $paranormalise(action,func,attributes,args);
    if (func) {
//      if (!action) { action = "ACTION"; }
//      IupSetCallback(ih,action,func);
        if (action === "VALUECHANGED_CB") {
            printf(1,"warning: IupToggle(VALUECHANGED_CB) not yet supported\n");
        } else {
//DEV
//          IupSetCallback(ih,action,func);
            if (action !== null && action !== "ACTION") { crash("?9/0"); }
//          if (!action) { action = "ACTION"; }
//DEV/SUG:  IupSetCallback(ih,"TOGGLEACTION",func); (it has/needs an extra parameter...)
            function action_cb(event) {
//              puts(1,"action_cb");    // placeholder...
                let tgl = event.currentTarget;
//              func(tgl,tgl.children[1].checked);
                func(tgl.parentNode,tgl.checked);
//              let c = ??,
//                  pNewValue = ??,
//                  res = func(/*Ihandle*/ ih, /*integer*/ c, /*atom*/ pNewValue);
//              if (res===IUP_IGNORE) {
//              }
            }
//          ih.addEventListener('keyup', action_cb);
//          ih.addEventListener('change', action_cb);
            cb.addEventListener('change', action_cb);
//          ih.onkeyup = action_cb;
        }
    }
    IupSetAttributes(ih, attributes, args);
    return ih;
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
    tree.setAttribute('EXPAND', 'YES');

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
                    open_cb(it, ', wowee'); //DEV (testing relic??)
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
                    close_cb(it, ', so there'); //DEV (testing relic??)
                }
            }
        }
    }
//  tree.addEventListener('click', clickHandler);
    tree.onclick = clickHandler;

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

//<input type="range" min="1" max="10" value="3" step="0.1" class="slider" id="lengthRange">
//IupValuator
function IupValuator(/*nullable_string*/ orientation=NULL, action=NULL, /*cbfunc*/ func=NULL, /*string*/ attributes="", /*sequence*/ args = []) {
    const ih = document.createElement("input");
    ih.setAttribute('class', 'slider');
    ih.setAttribute('type', 'range');
    ih.setAttribute('min', 0);
    ih.setAttribute('max', 1);
    [action,func,attributes,args] = $paranormalise(action,func,attributes,args);
    if (orientation) {
        IupSetAttribute(ih,"ORIENTATION",orientation);
    }
    if (func) {
        if (!action) {
            action = "ACTION";
        } else if (action !== "VALUECHANGED_CB" &&
                   action !== "ACTION") {
            crash("?9/0");
        }
        IupSetCallback(ih, action, func);
//      btn.addEventListener('click', toggleHide);
    }
    IupSetAttributes(ih, attributes, args);
    return ih;
}
let IupFlatVal = IupValuator;

function Icallback(func) {
    // dummy function, exists solely for p2js to convert Icallback("func") [phix] <==> Icallback(func); [js]
    // in other words, p2js recognises the Icallback() and simply removes/adds the quotes, whereas if it
    // instead did Icallback("func") [phix] ==> func; [js] then it would likely end up as a one-way trip.
    // (js ==> phix may help the transpiler development, but is otherwise likely to be fairly useless.)
    return func;
}

// three more, for similar reasons to Icallback():
function IupMainLoop() {
    crash("ERROR: IupMainLoop() and more importantly any following IupDestroy()/IupClose() must be in `if platform()!=JS then`");
}
//function IupMainLoop(fn) { terminator = fn; }

function IupClose() {}

function IupDestroy(/* Ihandlns */ dlg) {
//  NB: IupMainLoop() is a null-op in pGUI.js so it ends up invoking following code immediately...
//  instead, do (say) this:
//
//      if platform()!=JS then
//          IupMainLoop()
//          dlg = IupDestroy(dlg)
//          IupClose()
//      end if
//
//  Note however that eg IupDestroy(IupNormaliser(...)) should [one day] be perfectly valid.
//
//27/9/21 try it anyways...(NULL is probably better but neither has really been properly tested)
//  return null;  //DEV would be good for ih, fatal for non-if-not-JS-wrapped (main)dlg=IupDestroy(dlg)...
    return NULL;  //DEV would be good for ih, fatal for non-if-not-JS-wrapped (main)dlg=IupDestroy(dlg)...
//  return dlg;
}

function IupCanvas(action = null, func = null, attributes = "", args = []) {
    let ih = document.createElement("canvas");
    ih.setAttribute('class', 'canvas');
//  ih.setAttribute('EXPAND', 'YES');
    ih['EXPAND'] = 'YES';
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
        ih.ACTION = func;
        function func_closure() { func(ih); }
        window.requestAnimationFrame(func_closure);
    }
//DEV bust:
/*
    ih.onresize = function() {
//      ih.width = this.innerWidth;
//  c.height = this.innerHeight;
        console.log("canvas_resize?");
    }
*/
    return ih;
}
let IupGLCanvas = IupCanvas;

function rgb(/*atom*/ red, green, blue, alpha=0) {
    let colour = sprintf("#%02x%02x%02x",["sequence",red,green,blue]);
//DEV (untried, check online first)
//  let colour = sprintf("#%02%02x%02x%02x",["sequence",alpha,red,green,blue]);
    return colour;
}
let cdEncodeColor = rgb;
let cdEncodeColorAlpha = rgb;

//function alpha(/*atom*/ color) {
//  color = and_bits(color,0xFF000000);
//  color = floor(color/0x1000000);
//  return 255-color;
//}
//let cdDecodeAlpha = alpha;

//function red(/*atom*/ color) {
//  return floor(and_bits(color,0xFF0000)/0x10000);
//}
//
//function green(/*atom*/ color) {
//  return floor(and_bits(color,0xFF00)/0x100);
//}
//
//function blue(/*atom*/ color) {
//  return remainder(color,0x100);
//}

function to_rgb(/*atom*/ colour) {
    if (string(colour)) {
        colour = colour.toUpperCase();
        let l = colour.length, n = 0;
        if (colour[0] !== '#') { crash("colour must start with #"); }
        for (let i = 1; i < l; i += 1) {
            let d = colour.codePointAt(i);
            d -= (d <= 0X39) ? 0X30 : 0X41-10; // (ie ch-('0'|'A'-10))
            if ((d < 0) || d > 15) { crash("colour not hex digits"); }
            n = n*16 + d;
        }
        colour = n;
    }
    let red = and_bits(colour,0xFF0000)/0x10000,
      green = and_bits(colour,0xFF00)/0x100,
       blue = and_bits(colour,0xFF),
      alpha = and_bits(colour,0xFF000000)/0x1000000;
//  return ["sequence",red(colour),green(colour),blue(colour),alpha(colour)];
    return ["sequence",red,green,blue,255-alpha];
}
let cdDecodeColor = to_rgb;
let cdDecodeColorAlpha = to_rgb;

function cdDecodeAlpha(/*atom*/ colour) {
    let rgba = to_rgb(colour),
        alpha = rgba[4];
    return alpha;
}

//function cdEncodeAlpha(/*atom*/ colour, /*integer*/ alpha) {
////DEV probably not...
//  colour = and_bits(colour,0x00FFFFFF) + and_bits(alpha,0xFF)*0x1000000;
// much more likely, but as above, check online first...
//  let [,r,g,b] = to_rgb(colour);
//  let colour = sprintf("#%02%02x%02x%02x",["sequence",alpha,red,green,blue]);
//  return colour;
//}

function hsv_to_rgb(/*atom*/ h, s, v, a=0) {
    let /*integer*/ i = floor(h*6);
    let /*atom*/ f = h*6-i, 
                 p = v*(1-s), 
                 q = v*(1-s*f), 
                 t = v*(1-s*(1-f)), 
                 r, g, b;
    switch (i) {
        case 0: case 6: [r,g,b] = [v,t,p];  break;
                case 1: [r,g,b] = [q,v,p];  break;
                case 2: [r,g,b] = [p,v,t];  break;
                case 3: [r,g,b] = [p,q,v];  break;
                case 4: [r,g,b] = [t,p,v];  break;
                case 5: [r,g,b] = [v,p,q];
    }
    return rgb(r*255,g*255,b*255,a);
}

//cdEncodeColor() returns a codified triple (r,g,b) in an integer such as 0x00RRGGBB, where RR are the red components, GG are the green ones and BB are the blue ones. 
//cdEncodeColorAlpha() returns a codified quadriple (r,g,b,a) in an atom such as 0xAARRGGBB, where AA are the alpha components, and as above.

//function cdEncodeColorAlpha(/*atom*/ red, green, blue, alpha) {
////    return alpha<<24 + red<<16 + green<<8 + blue;
//  return and_bits(alpha,#FF)*0x1000000 +
//         and_bits(red,  #FF)*0x10000 +
//         and_bits(green,#FF)*0x100 +
//         and_bits(blue ,#FF);
//}
//
//function cdEncodeColor(/*atom*/ red, green, blue) {
//  return cdEncodeColorAlpha(red, green, blue, 0)
//}

function cdCreateCanvas(context, ih) {
    if (context===CD_DBUFFER ||
        context===CD_IUP) {
        if (typeof(ih.getContext) === "function") {
//          let ctx = ih.getContext("2d");
            let ctx = ih.getContext("2d") || ih.getContext("webgl");
//if (ctx) {
            ctx.fillStyle = "white";
            ctx.backGround = "#ffffff"; // (custom attribute)
            return ctx;
//}
        }
//      ih.style.textAlign = "start";
//      ih.textAlign = "start";
//      ih.textBaseline = "alphabetic";
        return ih;
    } else if (context===CD_GL) {
//      if ($gl) { return $gl; }
////        crash("CD_GL not supported by JavaScript");
//      crash("cdCreateCanvas(CD_GL) requires that IupGLMakeCurrent() has been called");
        // (see for example demo/rosetta/animate_pendulum2.exw)
        crash(`needs "cdCreateCanvas(CD_GL)" ==> "if JS then cdCreateCanvas(CD_IUP,canvas) else ..."`);
    }
    crash("cdCreateCanvas(" + context + ",...) not supported");
}

function cdCanvasActivate(ctx) {
    // nuthin needs doin here...
}

function cdCanvasArc(ctx, xc, yc, w, h, angle1, angle2) {
//  let ch = ctx.canvas.clientHeight;
    let ch = ctx.canvas.clientHeight - yc;
    if (xc > 0 && ch > 0 && w >= 2 && h >= 2) {
        ctx.beginPath();
//  ctx.arc(xc,ch-yc,w/2,angle1*Math.PI/180,angle2*Math.PI/180);
//  ctx.ellipse(xc,ch-yc,w/2,h/2,0,angle1*Math.PI/180,angle2*Math.PI/180);
        ctx.ellipse(xc,ch,w/2,h/2,0,angle1*Math.PI/180,angle2*Math.PI/180);
        ctx.stroke();
    }
}

//cdCanvasSector(cdCanvas canvas, atom xc, yc, w, h, angle1, angle2) 
function cdCanvasSector(ctx, xc, yc, w, h, angle1, angle2, sector=true) {
    let ch = ctx.canvas.clientHeight,
        fs = ctx.fillStyle;
    ctx.fillStyle = ctx.strokeStyle;
    ctx.beginPath();
//void ctx.arc(x, y, radius, startAngle, endAngle [, counterclockwise]);
//void ctx.ellipse(x, y, radiusX, radiusY, rotation, startAngle, endAngle [, counterclockwise]);
//  ctx.arc(xc,ch-yc,w/2,angle1*Math.PI/180,angle2*Math.PI/180);
    ctx.ellipse(xc,ch-yc,w/2,h/2,0,(360-angle2)*Math.PI/180,(360-angle1)*Math.PI/180);
    if (sector) { ctx.lineTo(xc,ch-yc); } // else chord
    ctx.fill();
    ctx.fillStyle = fs;
}

function cdCanvasChord(ctx, xc, yc, w, h, angle1, angle2) {
    cdCanvasSector(ctx, xc, yc, w, h, angle1, angle2, false);
}

function cdCanvasBox(/*cdCanvas*/ ctx, /*atom*/ xmin, xmax, ymin, ymax) {
//  puts(1,"cdCanvasBox("+xmin+","+xmax+","+ymin+","+ymax+")\n");
//DEV Probably still not quite right. Need to get my head around pixel co-ord mappings properly.
//    (write that demo/PGUI/cdCanvasBox.exw demo)
    let x = xmin,
     ccch = ctx.canvas.clientHeight,
//      y = ccch-ymin,
        y = ccch-ymax-1,
//      h = ymin-ymax-1,
        h = ymax-ymin+1,
//  ymin = h-ymin;
        w = xmax-xmin+1;
//  ctx.strokeRect(xmin,h-ymin,xmax,h-ymax);
//  ctx.fillRect(xmin+1,ymin+1,xmax-xmin-1,ymax-2);
    ctx.fillRect(x,y,w,h);
}

function cdCanvasBegin(ctx, mode) {
    ctx.beginPath();
    ctx["MODE"] = mode;
}

function cdCanvasCircle(/*cdCanvas*/ ctx, /*atom*/ x, y, r, /*boolean*/ filled=false) {
    if (filled) {
        cdCanvasSector(ctx,x,y,r,r,0,360);
    } else {
        cdCanvasArc(ctx,x,y,r,r,0,360);
    }
}

function cdCanvasClear(ctx) {
//  ctx.fillStyle = "white";
//3/10/21: (bust)
//  if (ctx.canvas) { ctx = ctx.canvas; }
    let fs = ctx.fillStyle,
        bs = ctx.backGround;
    if (bs) { ctx.fillStyle = bs; }
//if (ctx.canvas) {
    ctx.fillRect(0,0,ctx.canvas.clientWidth,ctx.canvas.clientHeight);
//  ctx.fillRect(0,0,ctx.clientWidth,ctx.clientHeight);
//  ctx.canvas.fillRect(0,0,ctx.canvas.clientWidth,ctx.canvas.clientHeight);
//}
//  ctx.fillRect(0,0,ctx.clientWidth,ctx.clientHeight);
    ctx.fillStyle = fs;
}

function cdCanvasEnd(ctx) {
    let mode = ctx["MODE"];
    if (mode === CD_FILL) {
        let fs = ctx.fillStyle;
        ctx.fillStyle = ctx.strokeStyle;
        ctx.fill();
        ctx.fillStyle = fs;
    } else if (mode === CD_OPEN_LINES) {
        ctx.stroke();
    } else if (mode === CD_CLOSED_LINES) {
        ctx.closePath();
        ctx.stroke();
    } else {
        crash("unsupported mode");
    }
}

function cdCanvasFlush() {}

function cdCanvasFont(/*cdCanvas*/ ctx, /*nullable_string*/ font, /*integer*/ style, /*integer*/ size) {
//CD_PLAIN (0), CD_BOLD (1), CD_ITALIC (2), CD_UNDERLINE (4) and CD_STRIKEOUT (8).
//pGUI also provides the constant CD_BOLD_ITALIC (3) for convenience.
//ctx.font = 'bold 48px serif';
//ctx.font = '50px serif';
    if (font === "Courier") {
        font = "courier";
    } else if (font === "Times") {
        font = "serif";
    } else if (font === "Helvetica" ||
               font === "Calibri") {
        font = "sans-serif";
    } else {
        crash("cdCanvasFont(font="+font+") not supported"); // placeholder??
    }       
    if (style === CD_PLAIN) {
        style = "";
    } else if (style === CD_BOLD) {
        style = "bold ";
    } else if (style === CD_ITALIC) {
        style = "italic ";
    } else if (style === CD_BOLD_ITALIC) {
        style = "bold italic ";
    } else {
        crash("cdCanvasFont(style="+style+") not supported"); // placeholder
    }
    if (size>=0) {
//      crash("cdCanvasFont(size="+size+") not supported (must use -ve pixels)");
        size = size/12 + "em ";
    } else {
        size = -size + "px ";
    }
//  size = -size;
//  font = style + size + "px " + font;
    font = style + size + font; // eg "bold 12px sans-serif"
    ctx.font = font;
//  puts(1,"cdCanvasFont() not yet supported..\n"); // placeholder
}

function cdCanvasGetSize(ih) {
//  let w = ih.canvas.width,
//      h = ih.canvas.height;
    if (ih.canvas) { ih = ih.canvas; }
    let w = ih.width,
        h = ih.height;
    // (conversion to mm tbd...)
    return ["sequence", w, h];
}

//DEV we may want to retrieve a(lpha) as well, see recent mods to pGUI.e
function cdCanvasGetImageRGB(/*cdCanvas*/ ctx, /*atom*/ x, y, w, h) {
    let /*integer*/ l = w*h,
//  let /*integer*/ l = (w-x)*(h-y),
        /*atom*/ r = repeat(0,l),
                 g = repeat(0,l),
                 b = repeat(0,l),
           imgdata = ctx.getImageData(x, y, w, h),
                dl = imgdata.data.length,
               dtx = 0;
    for (let i = 1; i <= l; i += 1) {
        r[i] = imgdata.data[dtx];
        g[i] = imgdata.data[dtx+1];
        b[i] = imgdata.data[dtx+2];
        dtx += 4;
    }
//getImageData()??
//createImageData()??
//putImageData()??
//--24/10/21..
//  iup_init_cd()
//--?pR
//--?{xcdCanvasGetImageRGB,canvas,x,y,w,h}
//  c_proc(xcdCanvasGetImageRGB, {canvas, pR, pG, pB, x, y, w, h})
//--?pR
//  sequence r = peek({pR, w*h}),
//           g = peek({pG, w*h}),
//           b = peek({pB, w*h})
//  free({pR,pG,pB})
    return ["sequence",r,g,b];
}

/*
//function cdCanvasPixel(ctx,x,y,colour) {
//  if (integer(colour)) { colour = sprintf("#%06x",colour&0xFFFFFF); }
//  ctx.strokeStyle = colour;
//  ctx.strokeRect(x,ctx.canvas.clientHeight-y,1,0);
//}

global procedure cdCanvasPutImageRectRGB(cdCanvas canvas, atom iw, ih, sequence rgb3, 
                                         atom x=0, y=0, w=0, h=0, xmin=0, xmax=0, ymin=0, ymax=0)
    integer l = length(rgb3[1])
    if length(rgb3)!=3
    or length(rgb3[2])!=l
    or length(rgb3[3])!=l then
        ?9/0
    end if
    atom pR = allocate(l),
         pG = allocate(l),
         pB = allocate(l)
    poke(pR, rgb3[1])
    poke(pG, rgb3[2])
    poke(pB, rgb3[3])
    c_proc(xcdCanvasPutImageRectRGB, {canvas, iw, ih, pR, pG, pB, x, y, w, h, xmin, xmax, ymin, ymax})
    free({pR,pG,pB})
end procedure

global procedure cdCanvasPutImageRectRGBA(cdCanvas canvas, atom iw, atom ih, sequence rgba, 
                                          atom x=0, y=0, w=0, h=0, xmin=0, xmax=0, ymin=0, ymax=0)
    integer l = length(rgba[1])
    if length(rgba)!=4
    or length(rgba[2])!=l
    or length(rgba[3])!=l
    or length(rgba[4])!=l then
        ?9/0
    end if
    atom pR = allocate(l),
         pG = allocate(l),
         pB = allocate(l),
         pA = allocate(l)
    poke(pR, rgba[1])
    poke(pG, rgba[2])
    poke(pB, rgba[3])
    poke(pA, rgba[4])
    c_proc(xcdCanvasPutImageRectRGBA, {canvas, iw, ih, pR, pG, pB, pA, x, y, w, h, xmin, xmax, ymin, ymax})
    free({pR,pG,pB,pA})
end procedure
global procedure cdCanvasPutImageRectMap(cdCanvas canvas, atom iw, ih, sequence index, colors,
                                                          atom x, y, w, h, 
                                                          atom xmin, xmax, ymin, ymax)
    atom pColors = allocate(4*256+length(index)),
         pIndex = pColors+4*256
    poke4(pColors, colors)
    poke(pIndex, index)
    c_proc(xcdCanvasPutImageRectMap, {canvas, iw, ih, pIndex, pColors, x, y, w, h, xmin, xmax, ymin, ymax})
    free(pColors)
end procedure
*/

function cdCanvasLine(ctx, x1, y1, x2, y2) {
    let h = ctx.canvas.clientHeight;
    ctx.beginPath();
    ctx.moveTo(x1, h-y1);
    ctx.lineTo(x2, h-y2);
    ctx.stroke();
}

function cdCanvasPixel(ctx,x,y,colour) {
/// let fs = ctx.fillStyle;
//  ctx.fillStyle = colour;
//  ctx.fillRect(x,ctx.canvas.clientHeight-y,1,1);
//  ctx.fillStyle = fs;
//  ctx.strokeStyle = colour;
    if (integer(colour)) { colour = sprintf("#%06x",colour&0xFFFFFF); }
    ctx.strokeStyle = colour;
    ctx.strokeRect(x,ctx.canvas.clientHeight-y,1,0);
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

function cdCanvasRoundedBox(/*cdCanvas*/ canvas, /*atom*/ xmin, xmax, ymin, ymax, w, h) {
    // first draw the filled rectangle with straight-clipped corners (aka an octagon)
    cdCanvasBegin(canvas,CD_FILL);
    cdCanvasVertex(canvas,xmin+w,ymin);
    cdCanvasVertex(canvas,xmax-w,ymin);
    cdCanvasVertex(canvas,xmax,ymin+h);
    cdCanvasVertex(canvas,xmax,ymax-h);
    cdCanvasVertex(canvas,xmax-w,ymax);
    cdCanvasVertex(canvas,xmin+w,ymax);
    cdCanvasVertex(canvas,xmin,ymax-h);
    cdCanvasVertex(canvas,xmin,ymin+h);
    cdCanvasEnd(canvas);
    // then round/fill in the corners using cdCanvasSector
//  cdCanvasSector(cdCanvas canvas, atom xc, yc, w, h, angle1, angle2) 
//  cdCanvasSetForeground(cddbuffer, CD_RED)
    cdCanvasSector(canvas,xmin+w,ymin+h,w*2,h*2,180,270);   // btm left
    cdCanvasSector(canvas,xmax-w,ymin+h,w*2,h*2,270,0);     // btm right
    cdCanvasSector(canvas,xmin+w,ymax-h,w*2,h*2,90,180);    // top left
    cdCanvasSector(canvas,xmax-w,ymax-h,w*2,h*2,0,90);      // top right
}

function cdCanvasRoundedRect(/*cdCanvas*/ canvas, /*atom*/ xmin, xmax, ymin, ymax, w, h) {
    // first draw four edges, not-quite-meeting
    cdCanvasLine(canvas,xmin+w,ymin,xmax-w,ymin);
    cdCanvasLine(canvas,xmax,ymin+h,xmax,ymax-h);
    cdCanvasLine(canvas,xmax-w,ymax,xmin+w,ymax);
    cdCanvasLine(canvas,xmin,ymax-h,xmin,ymin+h);
    // then round/connect the corners using cdCanvasArc
//  cdCanvasArc(cdCanvas canvas, atom xc, yc, w, h, a1, a2) 
//  cdCanvasSetForeground(cddbuffer, CD_RED)
    cdCanvasArc(canvas,xmin+w,ymin+h,w*2,h*2,180,270);
    cdCanvasArc(canvas,xmax-w,ymin+h,w*2,h*2,270,0);
    cdCanvasArc(canvas,xmin+w,ymax-h,w*2,h*2,90,180);
    cdCanvasArc(canvas,xmax-w,ymax-h,w*2,h*2,0,90);
}

function cdCanvasSetAttribute(/*cdCanvas*/ ctx, /*string*/ name, /*nullable_string*/ val, /*sequence*/ args=[]) {
    if (name=="SIZE") {
        if (val!=="%dx%d %g") { crash(`"%dx%d %g" expected`); }
        let [,w,h] = args;
        $resize_children(ctx.canvas,w,h);
//C:\Program Files (x86)\Phix\demo\pGUI\simple_paint.exw:785       cdCanvasSetAttribute(rgb_canvas, "RESOLUTION", "%g", {res})
//C:\Program Files (x86)\Phix\demo\pGUI\simple_paint.exw:859           cdCanvasSetAttribute(cd_canvas, "IMGINTERP", "NEAREST")  -- affects only drivers that have this attribute
//C:\Program Files (x86)\Phix\demo\pGUI\simple_play.exw:766   cdCanvasSetAttribute(canvas, "ROTATE", NULL)
    } else {
        puts(1,"cdCanvasSetAttribute("+name+")??\n"); // placeholder
    }
}

function cdCanvasSetBackground(ctx, colour) {
    if (integer(colour)) { colour = sprintf("#%06x",colour); }
//  ctx.fillStyle = colour;
    ctx.backGround = colour; // (custom attribute)
}

function cdCanvasSetForeground(ctx, colour) {
    if (integer(colour)) { colour = sprintf("#%06x",colour); }
    ctx.fillStyle = colour;
    ctx.strokeStyle = colour;
}

function cdCanvasSetFillMode(ctx, mode) {
    if (mode === CD_WINDING) {
        // default, do nothing for now
//  } else if (mode === CD_EVENODD) {
        // not even sure this is possible...
    } else {
        crash("uh?");
    }
}

function cdCanvasSetInteriorStyle(/*cdCanvas*/ ctx, /*integer*/ style) {
/*
Configures or retrieves the current style for the area filling primitives: 
    CD_SOLID, CD_HOLLOW, CD_HATCH, CD_STIPPLE or CD_PATTERN.
Note that only CD_HATCH and CD_STIPPLE are affected by the backopacity.
Default value: CD_SOLID.
If a stipple or a pattern were not defined, when they are selected the state of the attribute is not changed. 
When the style CD_HOLLOW is defined, functions cdCanvasBox() and cdCanvasSector() 
behave as their equivalent cdCanvasRect() and cdCanvasArc()+cdCanvasLine()s,
and the polygons with style CD_FILL behave like CD_CLOSED_LINES. 
*/
//  ctx.??? = style;
    if (style===CD_SOLID) {
        // nowt yet, but may have to undo other things this does...
    } else {
        puts(1,"cdCanvasSetInteriorStyle(" + style + ",...) not supported\n");
    }
}

//function cdCanvasGetInteriorStyle(/*cdCanvas*/ canvas) {
////    let /*integer*/ style = canvas.???;
//  return style;
//}

function cdCanvasSetLineStyle(/*cdCanvas*/ ctx, /*integer*/ style) {
// CD_CONTINUOUS = 0,
// CD_DASHED = 1,
// CD_DOTTED = 2,
// CD_DASH_DOT = 3,
// CD_DASH_DOT_DOT = 4,
// CD_CUSTOM = 5,
//  ctx.?lineXXX = style;
//  ctx.lineDashOffset = style*5;
//  let lds = ctx.getLineDash();
//  let lds = [[],[15,5],[5,5],[],[]][style];
//  ctx.setLineDash(lds);
//  ctx.setLineDash([[],[24,8],[4,4],[12,8,4,8],[12,4,4,4,4,4]][style]);
    if (style !== CD_CUSTOM) {
        ctx.setLineDash([[],[8,8],[4,4],[12,8,4,8],[12,4,4,4,4,4]][style]);
    }
//  puts(1,"cdCanvasSetLineStyle() not yet supported\n"); // placeholder
}

function cdCanvasSetLineWidth(/*cdCanvas*/ ctx, /*atom*/ width) {
    ctx.lineWidth = width;
//  puts(1,"cdCanvasSetLineWidth() not yet supported\n"); // placeholder
}

function cdCanvasText(/*cdCanvas*/ ctx, /*atom*/ x, y, /*string*/ text) {
    let h = ctx.canvas.clientHeight,
        angle = ctx.textOrientation || 0;
    if (!string(text)) { text = String.fromCodePoint(...text.slice(1)); }
    if (angle) {
        ctx.save();
        ctx.textAlign="center";
        ctx.textBaseline="middle";
        ctx.translate(x,h-y);
        ctx.rotate((Math.PI/180)*angle);
        ctx.fillText(text,0,0);
        ctx.restore();
//3/10/21: (bust, went with glCanvasSpecialText() instead)
//  } else if (typeof(ctx.fillText) !== "function") {
//      if (typeof(ctx.getContext) !== "function") { ctx = ctx.canvas; }
//      let textCtx = ctx.getContext("2d");
//      if (!textCtx) { textCtx = ctx.getContext("webgl"); }
////        let textCtx = ctx.canvas.getContext("2d") || ctx.canvas.getContext("webgl");
//      textCtx.fillText(text, x, h-y);
    } else {
        ctx.fillText(text, x, h-y);
    }
//  puts(1,"cdCanvasText() not yet supported..\n"); // placeholder
}

function glCanvasSpecialText(/*Ihandle*/ cd_canvas, /*integer*/ w, h, fontsize, /*string*/ text) {
    let textCtx = document.createElement("canvas").getContext("2d");
    textCtx.canvas.width = w;
    textCtx.canvas.height = h;
//  textCtx.font = Math.floor(fontsize*1.15) + "px monospace";
    textCtx.font = Math.floor(fontsize*1.25) + "px courier";
    textCtx.textAlign = "center";
    textCtx.textBaseline = "middle";
    textCtx.fillStyle = "black";
    textCtx.clearRect(0, 0, w, h);
//  textCtx.direction = "ltr";
//  textCtx.direction = "rtl";
    textCtx.fillText(text, w / 2, h / 2);
//  textCtx.rotate(285);
//  textCtx.setTransform(1,2,3,4,5,6);
    return textCtx.canvas;
}

function cdCanvasSetTextAlignment(/*cdCanvas*/ ctx, /*integer*/ alignment) {
//DEV (oh, could get messy... see diagram in phix.htm)
//Defines the vertical and horizontal alignment of a text as: CD_NORTH,
//CD_SOUTH, CD_EAST, CD_WEST, CD_NORTH_EAST, CD_NORTH_WEST, CD_SOUTH_EAST, 
//CD_SOUTH_WEST, CD_CENTER, CD_BASE_LEFT, CD_BASE_CENTER, or CD_BASE_RIGHT. 
//  CD_NORTH = 0,
//  CD_SOUTH = 1,
//  CD_EAST = 2,
//  CD_WEST = 3,
//  CD_NORTH_EAST = 4,
//  CD_NORTH_WEST = 5,
//  CD_SOUTH_EAST = 6,
//  CD_SOUTH_WEST = 7,
//  CD_CENTER = 8,
//  CD_BASE_LEFT = 9,
//  CD_BASE_CENTER = 10,
//  CD_BASE_RIGHT = 11,
//Returns the previous value. 
//Default value: CD_BASE_LEFT. -- (ok!)
//  let ta = ctx.textAlign || ctx.style.textAlign,
    let ta, tb;
    if (alignment === CD_CENTER) {
        ta = "center";
        tb = "middle";
    } else if (alignment === CD_NORTH) {
        ta = "center";
        tb = "top";
    } else if (alignment === CD_SOUTH) {
        ta = "center";
        tb = "bottom";
    } else if (alignment === CD_WEST) {
        ta = "left";
        tb = "middle";
    } else if (alignment === CD_EAST) {
        ta = "right";
        tb = "middle";
    } else {
        crash("cdCanvasTextAlignment("+alignment+") not yet supported\n"); // placeholder
    }
    ctx.textAlign = ta;
    ctx.textBaseline = tb;
}

function cdCanvasGetTextAlignment(canvas) {
    let ta = ctx.textAlign,
        tb = ctx.textBaseline,
        /*integer*/ alignment = -1;
//ctx.textAlign = "left" || "right" || "center" || "start" || "end";
//ctx.textBaseline = "top" || "hanging" || "middle" || "alphabetic" || "ideographic" || "bottom";
    if (ta === "center") {
        if (tb === "middle") {
            alignment = CD_CENTER;
        } else if (tb === "top") {
            alignment = CD_NORTH;
        } else if (tb === "bottom") {
            alignment = CD_SOUTH;
        }
    } else if (ta === "start") {
        if (tb === "alphabetic") {
            alignment = CD_BASE_LEFT;
        }
    } else if (ta === "left") {
        if (tb === "middle") {
            alignment = CD_WEST;
        }
    } else if (ta === "right") {
        if (tb === "middle") {
            alignment = CD_EAST;
        }
    }
    if (alignment===-1) {
        crash("cdCanvasGetTextAlignment("+ta+","+tb+") not yet supported\n"); // placeholder
    }
    return alignment;
}

function cdCanvasGetTextSize(/*cdCanvas*/ ctx, /*string*/ text) {
    let r = ctx.measureText(text);
/*
actualBoundingBoxAscent: 7
actualBoundingBoxDescent: 0
actualBoundingBoxLeft: 1
actualBoundingBoxRight: 10
fontBoundingBoxAscent: 9
fontBoundingBoxDescent: 2
width: 9.4384765625
r.actualBoundingBoxRight
*/
    return ["sequence",r.actualBoundingBoxRight,r.fontBoundingBoxAscent+r.fontBoundingBoxDescent];
}

function cdCanvasSetTextOrientation(/*cdCanvas*/ ctx, /*atom*/ angle) {
    // (applies to cdCanvasText() above only)
    ctx.textOrientation = -angle;
}

function cdCanvasGetTextOrientation(/*cdCanvas*/ ctx) {
    // (applies to cdCanvasText() above only)
    let prev = ctx.textOrientation || 0;
    return -prev;
}

function cdCanvasVectorText(/*cdCanvas*/ ctx, /*atom*/ x, y, /*string*/ text) {
    puts(1,"cdCanvasVectorText...\n"); // placeholder
}
let cdCanvasMultiLineVectorText = cdCanvasVectorText;

function cdCanvasVectorTextDirection(/*cdCanvas*/ canvas, /*integer*/ x1, y1, x2, y2) {
    puts(1,"cdCanvasVectorTextDirection...\n"); // placeholder
}

function cdCanvasVectorTextSize(/*cdCanvas*/ ctx, /*atom*/ w, h, /*string*/ text) {
    puts(1,"cdCanvasVectorTextSize...\n"); // placeholder
}

function cdCanvasVertex(ctx, x, y) {
//DEV see cdCanvasBox?
    let h = ctx.canvas.clientHeight;
//DEV should the first one be a moveTo?
    ctx.lineTo(x, h-y);
}

function cdKillCanvas() {}

function IupTimer(/*cbfunc*/ func=NULL, /*integer*/ msecs=0, /*boolean*/ active=true) {
    return $timer("create",func,msecs,active);
}

function IupUpdate(ih) {
//IupRedraw
//  let action = ih["ACTION"]
//  if (action) { action(ih); }
    function redraw() { IupRedraw(ih); }
    setTimeout(redraw, 100);
}

let $gl;

function IupGLMakeCurrent(canvas) {
    if (canvas === NULL) {
        // enabling call from opengl.js
//1/10/21... (on a whim)
//      $gl = NULL;
        $gl = true;
//  } else if ($gl === NULL) { // ie not "undefined"
    } else if ($gl) { // ie not "undefined"
        const names = ['webgl', 'experimental-webgl', 'webkit-3d', 'moz-webgl'];
        for (let i = 0; i < names.length; ++i) {
//          try {
            $gl = canvas.getContext(names[i]);
//          }
            if ($gl) break;
        }
        if (!$gl) { crash("unable to create webgl context"); }
    }
}

//DEV these should probably be moved to opengl.js:
function glAttachShader(/*integer*/ program, shader) {
    $gl.attachShader(program,shader);
}

function glBindBuffer(/*integer*/ target, buffer) {
    $gl.bindBuffer(target,buffer);
}

function glBindTexture(/*integer*/ target, /*atom*/ texture) {
    $gl.bindTexture(target,texture);
}

function glBufferData(/*integer*/ target, size, /*atom*/ pData, /*integer*/ usage) {
//  $gl.bufferData(target, size, pData, usage);
    $gl.bufferData(target, pData, usage);
}

function glClear(/*integer*/ mask) {
    $gl.clear(mask);
}

function glClearColor(/*atom*/ r, g, b, a) {
    $gl.clearColor(r,g,b,a);
}

function glCompileShader(/*integer*/ shader) {
    $gl.compileShader(shader);
}

function glCreateBuffer() {
    return $gl.createBuffer();
}

function glCreateProgram() {
    return $gl.createProgram();
}

function glCreateShader(/*integer*/ shaderType) {
    return $gl.createShader(shaderType);
}

function glCreateTexture() {
    return $gl.createTexture();
}

function glDeleteProgram(/*integer*/ program) {
    $gl.deleteProgram(program);
    return NULL;
}

function glDeleteShader(/*integer*/ shader) {
    $gl.deleteShader(shader);
    return NULL;
}

function glDrawArrays(/*integer*/ mode, first, count) {
    $gl.drawArrays(mode, first, count);
}

function glEnable(/*integer*/ what) {
    $gl.enable(what);
}

function glEnableVertexAttribArray(/*integer*/ index) {
    $gl.enableVertexAttribArray(index);
}

function glFloat32Array(/*sequence*/ data) {
    let /*integer*/ size = length(data)*4;
    let /*atom*/ pData = new Float32Array(data.slice(1));
    return ["sequence",size,pData];
}

function glInt32Array(/*sequence*/ data) {
    let /*integer*/ size = length(data)*4;
    let /*atom*/ pData = new Int32Array(data.slice(1));
    return ["sequence",size,pData];
}


function glFlush() {
    $gl.flush();
}

function glGetAttribLocation(/*integer*/ program, /*string*/ name) {
    return $gl.getAttribLocation(program,name);
}

function glGetError() {
    return $gl.getError();
}

function glGetProgramInfoLog(/*integer*/ program) {
    return $gl.getProgramInfoLog(program);
}

function glGetProgramParameter(/*integer*/ program, pname, dflt=0) {
    if (pname === GL_INFO_LOG_LENGTH) {
        crash("GL_INFO_LOG_LENGTH not supported");
    }
    return $gl.getProgramParameter(program, pname);
}

function glGetShaderInfoLog(/*integer*/ shader) {
    return $gl.getShaderInfoLog(shader);
}

function glGetShaderParameter(/*integer*/ shader, pname, dflt=0) {
    if (pname === GL_INFO_LOG_LENGTH) {
        crash("GL_INFO_LOG_LENGTH not supported");
    }
    if (pname === GL_SHADER_SOURCE_LENGTH) {
        crash("GL_SHADER_SOURCE_LENGTH not supported");
    }
    return $gl.getShaderParameter(shader, pname);
}

function glGetUniformLocation(/*integer*/ prog, /*string*/ name) {
    return $gl.getUniformLocation(prog,name);
}

function glLinkProgram(/*integer*/ prog) {
    $gl.linkProgram(prog);
}

function glShaderSource(/*integer*/ shader, /*string*/ source) {
    $gl.shaderSource(shader,source);
}

function glTexImage2Dc(/*integer*/ target, level, internalformat, width, height, border, fmt, datatype, /*cdCanvas*/ canvas) {
    $gl.texImage2D(target, level, internalformat, fmt, datatype, canvas);
}

function glTexParameteri(/*integer*/ target, pname, param) {
    $gl.texParameteri(target, pname, param);
}

function glUniform1f(/*integer*/ location, /*atom*/ v0) {
    $gl.uniform1f(location, v0);
}

function glUniform1i(/*integer*/ location, /*integer*/ v0) {
    $gl.uniform1i(location, v0);
}

//function glUniformMatrix4fv(/*integer*/ location, count, transpose, /*atom*/ pData) {
function glUniformMatrix4fv(/*integer*/ location, transpose, /*atom*/ pData) {
//DEV check... (count?)
//  gl.uniformMatrix4fv(matrixLocation, false, matrix);
    $gl.uniformMatrix4fv(location, transpose, pData);
}

function glUseProgram(/*integer*/ prog) {
    $gl.useProgram(prog);
}

function glVertexAttribPointer(/*integer*/ index, size, datatype, normalized, stride, /*atom*/ pVertices) {
    $gl.vertexAttribPointer(index,size, datatype, normalized, stride, pVertices);
}

function glViewport(/*integer*/ x, y, w, h) {
    $gl.viewport(x,y,w,h);
}

function IupDrawBegin(/*Ihandle ih*/) {
}
let IupDrawEnd = IupDrawBegin;
let IupDrawGetSize = cdCanvasGetSize;
//let IupDrawLine = cdCanvasLine;
//let IupDrawArc = cdCanvasArc;
//let IupDrawGetTextSize = cdCanvasGetTextSize;

function IupDrawArc(/*Ihandle*/ ih, /*integer*/ x1, y1, x2, y2, /*atom*/ a1, a2) {
    let ctx = ih.getContext("2d") || ih.getContext("webgl");
    cdCanvasArc(ctx,x1,y1,x2,y2,a1,a2);
}

function IupDrawGetTextSize(/*Ihandle*/ ih, /*string*/ text) {
    let ctx = ih.getContext("2d") || ih.getContext("webgl");
    return cdCanvasGetTextSize(ctx, text);
}

function IupDrawLine(/*Ihandle*/ ih, /*integer*/ x1, y1, x2, y2) {
    let ctx = ih.getContext("2d") || ih.getContext("webgl");
    cdCanvasLine(ctx,x1,y1,x2,y2);
}

//cdCanvasRect(/*cdCanvas*/ ctx, /*atom*/ xmin, xmax, ymin, ymax) {
function IupDrawRectangle(/*Ihandle*/ ih, /*integer*/ x1, y1, x2, y2) {
    let ctx = ih.getContext("2d") || ih.getContext("webgl");
//  cdCanvasRect(ih,x1,x2,y1,y2);
    cdCanvasRect(ctx,x1,x2,y1,y2);
}

function IupDrawText(/*Ihandle*/ ih, /*string*/ text, /*integer*/ x, y, w=0, h=0) {
//  cdCanvasText(ih, x, y, text);
    let ctx = ih.getContext("2d") || ih.getContext("webgl");
    cdCanvasText(ctx, x, y, text);
}

function IupMessage(msg_title=NULL, msg=NULL, args=[], bWrap=true) {
    if (args.length) {
        msg = sprintf(msg, args);
    }
    if (msg_title) { msg = msg_title + "\n\n" + msg;}
    alert(msg);
}

/* (return to this when(if) I get wifi back!)
integer res = IupAlarm(string title, string msg, string b1, nullable_string b2=NULL, nullable_string b3=NULL) 
function IupAlarm(title, msg, b1, b2, b3) {
    alert(title + "\n\n" + msg);
}
*/

function IupGLCanvasOpen() {
    // (does nothing)
}

/*
glUniform1f (needs proper doc in opengl.e first...)
glRotatef
glCreateShader
GL_VERTEX_SHADER, GL_FRAGMENT_SHADER (docs and syntax colour)
glShaderSource
glCompileShader
glCreateProgram
glAttachShader
glLinkProgram
glUseProgram
glGetUniformLocation
*/

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
// Iup[GL]Canvas/IupDraw*/IupImage/IupLink(example)/IupList(example)/IupMenu/
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
//  document.addEventListener('contextmenu', context_listener);
  $docBody.addEventListener('contextmenu', context_listener);

  function click_hide(event) {
    const classes = event.target.classList;
    if (!classes.contains('popup-menu-list')) {
      popup.style.display = 'none';
    }
  }
//  document.addEventListener('click', click_hide);
  $docBody.addEventListener('click', click_hide);
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

for "CURSORPOS", [GLOBAL]MOTION_CB:
document.addEventListener('mousemove', function(e){
  var position = { x: e.clientX, y: e.clientY }
}

*/

