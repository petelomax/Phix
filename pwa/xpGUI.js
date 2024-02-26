"use strict";
//
// xpGUI.js
// ========
//
//  Emulates xpGUI.e in the browser, so (with a little help from p2js) we
//  can develop/test on the desktop and publish straight to the web.
//
requires(JS);

const VK_BS = 0X08,
      VK_CR = 0X0D,
      VK_DEL = 0XFFFF,
      VK_DOWN = 0xFF54,
//    VK_END = 0xFF57,
      VK_ESC = 0X1B,
      VK_F1 = 0xF1,
      VK_F2 = 0xF2,
      VK_F3 = 0xF3,
      VK_F4 = 0xF4,
      VK_F5 = 0xF5,
      VK_F6 = 0xF6,
      VK_F7 = 0xF7,
      VK_F8 = 0xF8,
      VK_F9 = 0xF9,
      VK_F10 = 0xFA,
      VK_F11 = 0xFB,
      VK_F12 = 0xFC,
//    VK_HOME = 0xFF50,
//    VK_INS = 0xFF63,
      VK_LEFT = 0xFF51,
//    VK_MIDDLE = 0xFF0B,
      VK_PGDN = 0xFF56,
      VK_PGUP = 0xFF55,
      VK_RIGHT = 0xFF53,
//    VK_SP = 0X20,
//    VK_TAB = 0X09,
      VK_UP = 0xFF52,
      XPG_CONTINUE = -4,    
      XPG_DEFAULT = -3,
      XPG_IGNORE = -2,
      XPG_CLOSE = -1,

      XPG_CURRENT       = 0xFFF0,   // 0b0000   -- 65520
      XPG_LEFT          = 0xFFF1,   // 0b0001   -- 65521
      XPG_RIGHT         = 0xFFF2,   // 0b0010   -- 65522
      XPG_CENTER        = 0xFFF3,   // 0b0011   -- 65523
      XPG_MOUSEPOS      = 0xFFF4,   // 0b0100   -- 65524
      XPG_LEFTPARENT    = 0xFFF5,   // 0b0101   -- 65525
      XPG_RIGHTPARENT   = 0xFFF6,   // 0b0110   -- 65526
      XPG_CENTERPARENT  = 0xFFF7,   // 0b0111   -- 65527
      XPG_TOP           = XPG_LEFT,
      XPG_TOPPARENT     = XPG_LEFTPARENT,
      XPG_BOTTOM        = XPG_RIGHT,
      XPG_BOTTOMPARENT  = XPG_RIGHTPARENT,

      XPG_BLACK             = "#000000",
      XPG_NAVY              = "#000080",
      XPG_BLUE              = "#0000FF",
      XPG_LIGHT_BLUE        = "#4363D8",
      XPG_TEAL              = "#008080",
      XPG_DARK_CYAN         = "#00C0C0",
      XPG_CYAN              = "#00FFFF",
      XPG_DARK_GREEN        = "#008000",
      XPG_GREEN             = "#3CB44B",
      XPG_LIGHT_GREEN       = "#00FF00",
      XPG_OLIVE             = "#808000",
      XPG_ORANGE            = "#FF8C00",
      XPG_AMBER             = "#FFBF00",
      XPG_DARK_YELLOW       = "#EBEB00",
      XPG_YELLOW            = "#FFFF00",
      XPG_INDIGO            = "#4B0082",
      XPG_PURPLE            = "#911EB4",
      XPG_DARK_PURPLE       = "#800080",
      XPG_MAGENTA           = "#FF00FF",
      XPG_DARK_VIOLET       = "#F032E6",
      XPG_VIOLET            = "#EE82EE",
      XPG_DARK_RED          = "#800000",
      XPG_RED               = "#FF0000",
      XPG_SLATE             = "#404040",
      XPG_DARK_GRAY         = "#808080",    XPG_DARK_GREY = XPG_DARK_GRAY,
      XPG_GRAY              = "#C0C0C0",    XPG_GREY = XPG_GRAY, 
                                            XPG_SILVER = XPG_GRAY,
      XPG_LIGHT_GRAY        = "#E4E4E4",    XPG_LIGHT_GREY = XPG_LIGHT_GRAY,
      XPG_PARCHMENT         = "#FFFFE0",
      XPG_LIGHT_PARCHMENT   = "#FAF8EF",
      XPG_WHITE             = "#FFFFFF",
    
      XPG_CONTINUOUS = 0,
      XPG_DASHED = 1,
      XPG_DOTTED = 2,
      XPG_DASH_DOT = 3,
      XPG_DASH_DOT_DOT = 4,
    
      XPG_NORMAL        = 0x0,
      XPG_BOLD          = 0x1,
      XPG_ITALIC        = 0x2,
      XPG_BOLDITALIC    = 0x3,

      XPG_FILLED = 0b001, // (nb same as true[/false])
      XPG_CHORD  = 0b010,
      XPG_SECTOR = 0b100,

              // WENS (note absence of both 0b11xx and 0bxx11)
      XPG_C  = 0b0000,  XPG_CENTRE    = XPG_C,
      XPG_NW = 0b1010,  XPG_NORTHWEST = XPG_NW,
      XPG_W  = 0b1000,  XPG_WEST      = XPG_W,
      XPG_SW = 0b1001,  XPG_SOUTHWEST = XPG_SW,
      XPG_N  = 0b0010,  XPG_NORTH     = XPG_N,
      XPG_S  = 0b0001,  XPG_SOUTH     = XPG_S,
      XPG_NE = 0b0110,  XPG_NORTHEAST = XPG_NE,
      XPG_E  = 0b0100,  XPG_EAST      = XPG_E,
      XPG_SE = 0b0101,  XPG_SOUTHEAST = XPG_SE,
    
      XPG_GTK    = 1,
      XPG_WINAPI = 2,
      XPG_JS     = 3;


//let $storeAttr = {};  // element-specific attribute handlers/setters
let $storeAttr; // element-specific attribute handlers/setters
//DEV/SUG: should we have a $fetchAttr as well?? (to help keep related code near each other)

// This performs a maximise/restore to sort sizes out, it would also 
//  invoke it on the preceding initial creation: skip the first two.
let $redraw = true;

function $pxFloat(s) { return (s === "auto") ? 0 : parseFloat(s.replace("px", "")); }
function $eHeight(elem) { return $pxFloat(getComputedStyle(elem).height); }
function $eWidth(elem) { return $pxFloat(getComputedStyle(elem).width); }

function $to_bool(val) {
    // convert YES/NO to true/false (as a common requirement)
    if (typeof(val) !== "string") { return val; }
    val = val.toUpperCase();
    if (val === "YES" || val === "ON" || val === "1" || val === "TRUE") { return true; }
    if (val === "NO" || val === "OFF" || val === "0" || val === "FALSE") { return false; }
    crash("uh?");
}

let $ctrlKey  = false, /// or event.ctrlKey, for IupGetInt(NULL,"CONTROLKEY")
    $shiftKey = false; /// or event.shiftKey, for IupGetInt(NULL,"SHIFTKEY")

function rgba(/*atom*/ red, green, blue, alpha=0) {
//function rgba(/*atom*/ red, green, blue, alpha=0xFF) {
//  if (alpha!=0xFF) {
    if (alpha!=0) {
//      return sprintf("#%02%02x%02x%02x",["sequence",0xFF-(alpha&0xFF),red&0xFF,green&0xFF,blue&0xFF]);
        return sprintf("#%02%02x%02x%02x",["sequence",alpha&0xFF,red&0xFF,green&0xFF,blue&0xFF]);
    }
    return sprintf("#%02x%02x%02x",["sequence",red&0xFF,green&0xFF,blue&0xFF]);
//tryme (performancewise, if/when Julia set works in xpGUI... [no measurable gain in pGUI.js...])
//  let res = "#";
//  if (alpha!=0) { res += (alpha&0xFF).toString(16).padStart(2,"0"); }
//  res += (  red&0xFF).toString(16).padStart(2,"0")
//       + (green&0xFF).toString(16).padStart(2,"0")
//       + ( blue&0xFF).toString(16).padStart(2,"0");
//  return res;
// works the same, but above was 10s, this 13s (Julia set)...
//  let colour = (red&0xFF)*0x10000+(green&0xFF)*0x100+(blue&0xFF);
//  return colour;
}

function to_rgba(/*atom*/ colour) {
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
//  let alpha = and_bits(colour,0xFF000000)/0x1000000,
//        red = and_bits(colour,0xFF0000)/0x10000,
//      green = and_bits(colour,0xFF00)/0x100,
//       blue = and_bits(colour,0xFF);
    let alpha = (colour & 0xFF000000)/0x1000000,
          red = (colour & 0xFF0000)/0x10000,
        green = (colour & 0xFF00)/0x100,
         blue = (colour & 0xFF);
//  return ["sequence",red(colour),green(colour),blue(colour),alpha(colour)];
//  return ["sequence",red,green,blue,255-alpha];
    return ["sequence",red,green,blue,alpha];
}

function hsv_to_rgba(/*atom*/ h, s, v, a=0) {
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
//  return rgba(r*255,g*255,b*255,a);
    return rgba(r*255,g*255,b*255,a*255);
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

function $gInit() {
    // invoke using if (!$storeAttr) { $gInit(); }, as needed
    $storeAttr = {};

    // Handle the case of "save as" running locally: it will dump a 
    // complete copy of the generated dialog without any handlers, so 
    // all you would see is a blank and unresponsive dialog overlay.
    // You can of course avoid this message by manually deleting the
    // offending dialog(s) from the end of the saved html file.
    let d = Array.from($docBody.querySelectorAll(".dialog"));
    if (d.length) {
        puts(1,`Warning: "Save as" dialog(s) deleted`);
//      crash("placeholder"); // (this may need to be delayed...)
        for (let i = 0; i < d.length; i += 1) {
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
            // convert eg "225x75" to ["sequence",225,75] 
            //  (ie a js Array of length 2)
            // likewise "{225,75}"
            let x = val.indexOf('x'), y;
            if (x === -1) {
                if (equal($subse(val,1),0X7B)       // '{'
                &&  equal($subse(val,-1),0X7D)) {   // '}'
                    val = $subss(val,2,-2);
                    x = val.indexOf(',');
                }
            }
            if (x !== -1) {
                y = Number(val.slice(x+1));
                x = Number(val.slice(0,x));
                if (Number.isInteger(x) &&
                    Number.isInteger(y)) {
                    val = ["sequence",x,y];
                }
            }
        }
        if (!Array.isArray(val) ||
//          val.length !== 2) {
            val.length !== 3) {
            crash("invalid intint value");
        }
        return val;
    }

    function intints(val, w, h, name) {
        // version of intint() for dialog [raster]size, supporting eg "QUARTERxEIGHTH"
        if (typeof(val) === "string") {
            // convert eg "225x75" to ["sequence",225,75]
            //  (ie a js Array of length 2)
            let x = val.indexOf('x'), y;
            if (x !== -1) {
                y = val.slice(x+1);
                x = val.slice(0,x);
                function fulleighth(s,x,f,name) {
                    let n = Number(s);
                    if (Number.isInteger(n)) {
//                      if (name === "SIZE") {
//                          n *= f;
//                      }
                        return floor(n);
                    }
                    if (s === "FULL") { s = x; }
                    if (s === "HALF") { s = floor(x/2); }
                    if (s === "THIRD") { s = floor(x/3); }
                    if (s === "QUARTER") { s = floor(x/4); }
                    if (s === "EIGHTH") { s = floor(x/8); }
                    return s;
                }
                x = fulleighth(x,w,6/4,name);
                y = fulleighth(y,h,15/8,name);
                if (Number.isInteger(x) &&
                    Number.isInteger(y)) {
                    val = ["sequence",x,y];
                }
            }
        }
        if (!Array.isArray(val) ||
            val.length !== 3) {
            crash("invalid intint value");
        }
        return val;
    }

    function set_active(id, sname, val) {
        val = $to_bool(val);
        id.disabled = !val;
        let children = id.childNodes,
        l = children.length;
        for (let i = 0; i < l; i += 1) {
            set_active(children[i],sname,val);
        }
    }
    store_attrs(["button","canvas","datepick","dialog","drop","frame","hbox","label",
//               "list","menuitem","text","vbox","tabcontainer","progress"], ["ACTIVE"], set_active);
                 "list","spin","text","vbox","tabcontainer","progress"], ["ACTIVE"], set_active);

    function set_style(id, sname, val) {
//DEV 1..4 values, need not be string...
        if (val.indexOf('x') === -1) {
            // treat eg "200" as "200x"
            val = val + "x";
        }
        let [, w, h] = intint(val);
        w += "px";
        h += "px";
        id.style[sname+"Top"] = h;
        id.style[sname+"Left"] = w;
        id.style[sname+"Right"] = w;
        id.style[sname+"Bottom"] = h;
    }

    function set_font(id,val) {
        let face = val, //fallback,
            comma = val.indexOf(',');
        if (comma !== -1) {
            face = face.slice(0,comma);
            let stylesize = val.slice(comma+1),
                space = stylesize.indexOf(' ');
            while (space !== -1) {
                if (space) {
                    let stylename = stylesize.slice(0,space).toLowerCase();
                    // Bold, Italic, Underline and Strikeout
                    if (stylename === "bold") {
                        id.style.fontWeight = "bold";
                    } else if (stylename === "italic") {
                        id.style.fontStyle = "italic";
                    } else if (stylename === "underline") {
                        id.style.textDecoration = "underline";
                    } else if (stylename === "strikeout") {
                        id.style.textDecoration = "line-through";
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
                id.style.fontSize = -stylesize + "px";
            } else {
                id.style.fontSize = stylesize + "pt";
            }
        }
//      if (face === "Tahoma" ||
//          face === "Verdana" ||
//          face === "Arial") {
//          fallback = "sans_serif";
////            fallback = "serif";
//      } else 
//      if (face === "Courier") {
//          fallback = "monospace";
//      } else {
////            crash("FONT="+val+"??\n");
//          puts(1,"FONT="+val+"??\n"); // placeholder
//      }
//      id.style.fontFamily = `"` + face + `", ` + fallback;
        id.style.fontFamily = `"` + face + `"`;
//font-family: "Tahoma", sans-serif;
//font-size: 12px;
//  "Times, Bold 18"
//  "Arial, 24" (no style)
//  "Courier New, Italic Underline -30" (size in pixels)
    }

    function rastersize(id, name, val) {
        // applies to dialog, label, text, canvas, ...
        assert(name === "SIZE" || name === "RASTERSIZE");
        let cn = id.classList[0];
        if ((val === null) || (val === NULL)) {
            if (cn === "canvas" ||
                cn === "graph" ||
                cn === "list") {
                id = gGetDialog(id);
                cn = "dialog";
            }
            if (cn === "dialog") {
//DEV still not sure about this...
//9/5/22:
//              id.minWidth = 260;
                if (!id.minWidth) { id.minWidth = 260; }
//              id.minimumWidth = 0;
//              id.minHeight = 160;
                if (!id.minHeight) { id.minHeight = 160; }
//              id.minimumHeight = 0;
//put back for Sierpinski_curve 5/4/22:
//              id.minimumWidth = 0;
                id.minimumWidth = id.minWidth;
//              id.minimumHeight = 0;
                id.minimumHeight = id.minHeight;
            }
        } else {
            let w, h;
//          if (sequence(val)) {
            if (Array.isArray(val) && val[0] === "sequence") {
                w = val[1];
                h = val[2];
            } else if (cn === "dialog") {
                [, w, h] = intints(val,window.innerWidth,window.innerHeight,name);
            } else {
                if (cn === "hbox" && val.indexOf('x') === -1) {
                    // treat eg "200" as "200x"
                    val = val + "x";
                } else if (cn === "vbox" && val.indexOf('x') === -1) {
                    // treat eg "200" as "x200"
                    val = "x" + val;
                }
                [, w, h] = intint(val);
//              if (name === "SIZE") {
//                  // assume CHARSIZE of 6x15
//                  w *= 6/4;
//                  h *= 15/8;
//              }
            }
            if (cn === "canvas" ||
                cn === "graph" ||
                cn === "list") {
                if (w) { id.width = w; }
                if (h) { id.height = h; }
            } else {
//DEV still not sure about this...
//              if (w) { id.style.width = w + "px"; }
//              id.style.width = w ? w-2 + "px" : "";
                id.style.width = w ? w + "px" : "";
//              if (h) { id.style.height = h-10 + "px"; }
//              id.style.height = h ? h-10 + "px" :"";
//              id.style.height = h ? h-2 + "px" :"";
                id.style.height = h ? h + "px" :"";
            }
        }
    }
    store_attrs(["button","canvas","graph","list","dialog","frame","hbox","label","text","vbox","tabcontainer","progress",
                 "slider","spin"], ["RASTERSIZE","SIZE"], rastersize);

    function set_tip(id, name, val) {
        id.title = val;
    }
    store_attrs(["button","canvas","graph","list","dialog","frame","hbox","label","spin","text","vbox","tabcontainer","progress"], ["TIP"], set_tip);

    function expand(id,val) {
//Value: "YES" (both directions), "HORIZONTAL", "VERTICAL", "HORIZONTALFREE", "VERTICALFREE" or "NO".
//Default: "NO". For containers the default is "YES".
//Affects: All elements, except menus. 
        if (val === "YES" || val === "BOTH") {
            id.classList.add("expandv");
            id.classList.add("expandh");
        } else if (val === "HORIZONTAL") {
            id.classList.remove("expandv");
            id.classList.add("expandh");
        } else if (val === "VERTICAL") {
            id.classList.add("expandv");
            id.classList.remove("expandh");
        } else if (val === "NO") {
            id.classList.remove("expandv");
            id.classList.remove("expandh");
        } else {
            crash("gSetAttribute(id,\"EXPAND\", \"" + val + "\"??)");
        }
    }

//DEV begone?:
    function align(id,val,elem) {
//ALIGNMENT  (non inheritable) horizontal and vertical alignment. 
//              Possible values: "ALEFT", "ACENTER" and "ARIGHT", combined to "ATOP", "ACENTER" and "ABOTTOM". 
//              Default: "ALEFT:ACENTER". 
//??            Partial values are also accepted, like "ARIGHT" or ":ATOP", the other value will be used from the current alignment. 
        id.style.display = "flex";
        let k = val.indexOf(':');
        if (k !== -1) {
            let vtb = val.slice(k+1);
                val = val.slice(0,k);
            if (vtb === "ATOP" ||
                vtb === "ALEFT") {
                id.style.alignItems = "flex-start";
//              id.classList.add("expandv");
//              id.classList.add("expandh");
            } else if (vtb === "ACENTER") {
                id.style.alignItems = "center";
//              id.classList.remove("expandv");
//              id.classList.add("expandh");
            } else if (vtb === "ABOTTOM" ||
                       vtb === "ARIGHT") {
                id.style.alignItems = "flex-end";
//              id.classList.add("expandv");
//              id.classList.remove("expandh");
            } else {
                crash("gSetAttribute(" + elem + ",\"ALIGNMENT\", \":" + vtb + "\"??)");
            }
//          puts(1,"gSetAttribute(gLabel,\"ALIGNMENT\", \":" + vtb + "\" not yet implemented)");
        }
        if (val.length) {
//          id.style.display = "grid";
            if (val === "ALEFT") {
//              id.style.placeItems = "start";  // (top left)
                id.style.justifyContent = "flex-start";
            } else if (val === "ACENTER") {
//              id.style.placeItems = "center"; // (mid/mid)
                id.style.justifyContent = "center";
            } else if (val === "ARIGHT") {
//              id.style.placeItems = "end";    // (btm right)
                id.style.justifyContent = "flex-end";
            } else {
                crash("gSetAttribute(" + elem + ",\"ALIGNMENT\", \"" + val + "\"??)");
            }
        }
    }   

    function set_dialog(id, name, val) {
        assert(id.classList[0] === "dialog");
        if (name === "TITLE") {
            const hdr = id.querySelector(".dialog-handle");
            hdr.innerHTML = val;
        } else if (name === "MINSIZE") {
            let [, w, h] = intint(val);
            h += 10;
//          if (id.style.width < w)  { id.style.width = w + "px"; }
//          if (id.style.height < h) { id.style.height = h + "px"; }
            id.minWidth = w;
            id.minimumWidth = w;
            id.minHeight = h;
            id.minimumHeight = h;
//9/5/22:
            if (id.isConnected) {
//7/5/22:
                if (!id.style.width || id.style.width < w) {
                    id.style.width = w ? w + "px" : "";
                }
                if (!id.style.height || id.style.height < h) {
                    id.style.height = h ? h-10 + "px" :"";
                }
            }
        } else if (name === "MAXSIZE") {
            let [, w, h] = intint(val);
//          id.maxWidth = w;
            id.maximumWidth = w;
//          id.minHeight = h;
            id.maximumHeight = h;
//7/12/23:
            if (id.isConnected) {
//7/5/22:
                if (!id.style.width || id.style.width > w) {
                    id.style.width = w ? w + "px" : "";
                }
                if (!id.style.height || id.style.height > h) {
                    id.style.height = h ? h-10 + "px" :"";
                }
            }
//      } else if (name === "MARGIN" || name === "GAP") {
        } else if (name === "MARGIN") {
//DEV tryme:
//          set_style(id,"margin",val);
            let [, w, h] = intint(val);
            if (h) {
                id.style.marginTop = h;
                id.style.marginBottom = h;
            }
            if (w) {
                id.style.marginLeft = w;
                id.style.marginRight = w;
            }
//DEV...?? (in general and above)
        } else if (name === "GAP") {
            let g = Number(val);
            id.style.margin = g;
        } else if (name === "RESIZE") {
            val = $to_bool(val);
//          ,["YES"])
//          let maxbtn = id.getElementsByClassName("Maximise")[0],
            let maxbtn = id.querySelector(".Maximise"),
//              resizers = id.getElementsByClassName("dialog-resizers")[0],
                resizers = id.querySelector(".dialog-resizers"),
                display_style = val ? "block" : "none";
            maxbtn.style.display = display_style;
            resizers.style.display = display_style;
//      } else if (name === "SIZE") {
//          puts(1,"gDialog(SIZE,"+val+")??\n"); // placeholder
        } else if (name === "OPACITY") {
            id.style.opacity = Number(val)/255;
        } else if (name === "PLACEMENT") {
//          val = $to_bool(val,["MAXIMIZED"]) [maybe not]
            if (val==="MAXIMIZED") {
//              $maxWindow(id);
                if (!id.classList.contains("window-maximized")) {
                    setTimeout($maxWindow, 100, id);
                }
//          } else if (val === "MINIMIZED") {   // not supported..
            } else if (val === "NORMAL") {  // not supported..
                if (id.classList.contains("window-maximized")) {
                    setTimeout($maxWindow, 100, id);
                }
            } else {
                crash("gSetAttribute(dialog,\"" + name + "\",\"" + val + "\") not yet implemented");
            }
        } else if (name === "VISIBLE") {
//          puts(1,"gDialog(VISIBLE,"+val+")??\n"); // placeholder
            val = $to_bool(val);
            let display_style = val ? "block" : "none";
            id.style.display = display_style;
        } else if (name === "FULLSCREEN") {
//          puts(1,"gDialog(FULLSCREEN,"+val+")??\n"); // placeholder
            val = $to_bool(val);
            let header = id.querySelector(".dialog-header"),
                resizers = id.querySelector(".dialog-resizers");
            if (val) {
                header.style.display = "none";
                resizers.style.display = "none";
                set_dialog(id, "PLACEMENT", "MAXIMIZED");
                document.documentElement.requestFullscreen();
            } else if (document.fullscreenElement) {
                document.exitFullscreen();
                header.style.display = "flex";
                resizers.style.display = "block";
//              set_dialog(id, "PLACEMENT", "NORMAL");
                if (id.classList.contains("window-maximized")) {
                    $maxWindow(id);
                }
            }
        } else if (name === "SHRINK") {
//          puts(1,"gDialog(SHRINK,"+val+")??\n"); // placeholder
            id.minWidth = 260;
            id.minimumWidth = 0;
            id.minHeight = 160;
            id.minimumHeight = 0;
        } else if (name === "DIALOGFRAME") {
            // Set the common decorations for modal dialogs. This means RESIZE=NO, MINBOX=NO and MAXBOX=NO. 
            // In Windows, if the PARENTDIALOG is defined then the MENUBOX is also removed, but the Close button remains. 
            set_dialog(id, "RESIZE", false);
//          puts(1,"gDialog(DIALOGFRAME,"+val+")??\n"); // placeholder
        } else if (name === "CHILDOFFSET") {
// ignore for now (demo\rosetta\Animation.exw)
//          puts(1,"gDialog(CHILDOFFSET,"+val+")??\n"); // placeholder
        } else if (name === "MINBOX") {
            puts(1,"gDialog(MINBOX,"+val+")??\n"); // placeholder
        } else if (name === "MAXBOX") {
            puts(1,"gDialog(MAXBOX,"+val+")??\n"); // placeholder
        } else {
            crash("gSetAttribute(dialog,\"" + name + "\",\"" + val + "\") not yet implemented");
// neither yet tried:
//          crash(`gSetAttribute(dialog,"` + name + `","` + val + `\) not yet implemented`);
//          crash(`gSetAttribute(dialog,"%s","%s") not yet implemented`,["sequence",name,val]);
        }
    }
    store_attrs(["dialog"], ["TITLE","MINSIZE","MAXSIZE","MARGIN","FULLSCREEN",
                             "GAP","RESIZE","OPACITY","DIALOGFRAME","CHILDOFFSET",
                             "PLACEMENT","VISIBLE","SHRINK","MINBOX","MAXBOX"], set_dialog); // (common-up when appropriate)
//  store_values("MINSIZE",[["%d"],"x",["%d"]])
//--    dlg = gDialog(lbl,`TITLE="Hello", SIZE=150x40, MINSIZE=225x75`)
//      dlg = gDialog(lbl,`TITLE="Hello", MINSIZE=225x75`)
//--    dlg = gDialog(lbl,`TITLE="Hello", MINSIZE=225x75, PLACEMENT=MAXIMIZED`) -- (more like it)
//--    dlg = gDialog(lbl,`TITLE="Hello", MINSIZE=225x75, FULLSCREEN=YES`) (be like F11 in the browser...)

//DEV gDropDown...
    function set_drop(id, name, val) {
        assert(id.classList[0] === "drop");
        if (name === "CANFOCUS") {
            puts(1,"gDropDown(CANFOCUS,"+val+")??\n"); // placeholder
        } else if (name === "EXPAND") {
            expand(id,val);
//      } else if (name === "ALIGNMENT") {
//          align(id,val,"IupList");
        } else if (name === "PADDING") {
//          puts(1,"gList(PADDING,"+val+")??\n"); // erm, now banned? [or rather utilised by gH/Vbox?]
            set_style(id,"padding",val);
//      } else if (name === "DROPDOWN") {
//          puts(1,"IupList(DROPDOWN,"+val+")??\n"); // placeholder
//          val = $to_bool(val);
//          id.DROPDOWN = val;
//      } else if (name === "APPENDITEM") {
//          puts(1,"IupList(APPENDITEM,"+val+")??\n"); // placeholder
//      } else if (name === "REMOVEITEM") {
//          puts(1,"IupList(REMOVEITEM,"+val+")??\n"); // placeholder
        } else if (name === "TIP") {
            puts(1,"gDropDown(TIP,"+val+")??\n"); // placeholder
        } else if (name === "VALUE") {
            puts(1,"gDropDown(VALUE,"+val+")??\n"); // placeholder
        } else if (name === "VISIBLE") {
            puts(1,"gDropDown(VISIBLE,"+val+")??\n"); // placeholder
//      } else if (name === "VISIBLELINES") {
//          puts(1,"gDropDown(VISIBLELINES,"+val+")??\n"); // placeholder
//      } else if (name === "VISIBLECOLUMNS") {
//          puts(1,"IupList(VISIBLECOLUMNS,"+val+")??\n"); // placeholder
//      } else if (name === "VISIBLEITEMS") {
//          puts(1,"IupList(VISIBLEITEMS,"+val+")??\n"); // placeholder
//      } else if (name === "SHOWDROPDOWN") {
//          puts(1,"IupList(SHOWDROPDOWN,"+val+")??\n"); // placeholder
//      } else if (name === "NAME") {
//          puts(1,"IupList(NAME,"+val+")??\n"); // placeholder
//      } else if (name === "EDITBOX") {
//          puts(1,"IupList(EDITBOX,"+val+")??\n"); // placeholder
        } else if (name === "VALINT") {
//          puts(1,"gDropDown(VALINT,"+val+")??\n"); // placeholder
            id.selectedIndex = val-1;
/*
    } else if (name === "VALINT" ||
               name === "VALSTR" ) {
        if (t === "list") {
            let children = id.childNodes,
                l = children.length;
            for (let i = 0; i < l; i += 1) {
                if (children[i].selected) {
                    if (name === "VALINT") {
                        return i+1;
                    }
                    return children[i].text;
                }
            }
            if (name === "VALINT") {
                return 0;
            }
            return "";
        }
*/
        } else {
            crash("gSetAttribute(gDropDown,\"" + name + "\") not yet implemented\n");
        }
    }
    // note: "OPTIONS" is handled specially, see gSetAttribute().
//"DROPDOWN","APPENDITEM","REMOVEITEM","VISIBLELINES","VISIBLECOLUMNS","VISIBLEITEMS","SHOWDROPDOWN","NAME","EDITBOX"
    store_attrs(["drop"], ["CANFOCUS","EXPAND","VALINT","TIP","VALUE","VISIBLE"], set_drop);

    function set_frame(id, name, val) {
        assert(id.classList[0] === "frame");
//DEV spotted in passing: some of these are not reachable aka not set in store_attrs below...
        if (name === "EXPAND") {
            expand(id,val);
//      } else if (name === "ALIGNMENT") {
//          align(id,val,"IupList");
        } else if (name === "PADDING") {
            set_style(id,"padding",val);
        } else if (name === "MARGIN") {
//DEV tryme:
            set_style(id,"margin",val);
//          let [, w, h] = intint(val);
//          if (h) {
//              id.style.marginTop = h;
//              id.style.marginBottom = h;
//          }
//          if (w) {
//              id.style.marginLeft = w;
//              id.style.marginRight = w;
//          }
        } else if (name === "TITLE") {
            id.childNodes[0].innerText = val;
        } else if (name === "VISIBLE") {
            puts(1,"IupFrame(VISIBLE"+val+")??\n"); // placeholder
        } else if (name === "FGCOLOR") {
//DEV tryme:
//          set_style(id,"margin",val);
            puts(1,"IupFrame(FGCOLOR"+val+")??\n"); // placeholder
        } else if (name === "NAME") {
            id["NAME"] = val;
            puts(1,"?!IupFrame(NAME,"+val+")??\n"); // placeholder
        } else {
            crash("gSetAttribute(IupFrame,\"" + name + "\") not yet implemented\n");
        }
    }
    store_attrs(["frame"], ["PADDING","MARGIN","TITLE","VISIBLE","FGCOLOR","NAME"], set_frame);

//gdx lbl = gLabel("World","EXPAND=YES, ALIGNMENT=ACENTER"),
    function set_label(id, name, val) {
        assert(id.classList[0] === "label");
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
//          id.innerHTML = val;
//          id.innerText = val;
            id.innerHTML = "<nobr>"+val+"</nobr>";
        } else if (name === "EXPAND") {
            expand(id,val);
        } else if (name === "ALIGNMENT") {
            align(id,val,"gLabel");
        } else if (name === "PADDING") {
            set_style(id,"padding",val);
//          let [, w, h] = intint(val);
//          id.style.paddingTop = h + "px";
//          id.style.paddingBottom = h + "px";
//          id.style.paddingLeft = w + "px";
//          id.style.paddingRight = w + "px";
        } else if (name === "VISIBLE") {
            puts(1,"gLabel(VISIBLE,"+val+")??\n"); // placeholder
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
//  var ctx = document.createElement("canvas").getContext("2d");
//  ctx.fillStyle = str;
//  return ctx.fillStyle;
//}
*/
            }
            if (val.indexOf("#") !== -1) {
//              id.style.background = val;
                id.style.color = val;
            } else {
                puts(1,"gLabel(FGCOLOR,"+val+")??\n"); // placeholder??
            }
        } else if (name === "FONT" || name === "FONTFACE") {
            set_font(id,val);
        } else if (name === "FONTSTYLE") {
            puts(1,"gLabel(FONTSTYLE,"+val+")??\n"); // placeholder
        } else if (name === "SEPARATOR") {
//          val = $to_bool(val);
            if (val === "HORIZONTAL") {
                id.classList.add("labelhsep");
            } else {
                id.classList.remove("labelhsep");
            }
//DEV seems to work for gLabel(NULL,"SEPARATOR=HORIZONTAL") anyway...
//          puts(1,"?gLabel(SEPARATOR,"+val+")??\n"); // placeholder
        } else if (name === "NAME") {
            id["NAME"] = val;
            puts(1,"?!gLabel(NAME,"+val+")??\n"); // placeholder
        } else if (name === "TIP") {
            puts(1,"gLabel(TIP,"+val+")\n"); // placeholder
        } else if (name === "TEXTWRAP") {
            puts(1,"gLabel(TEXTWRAP,"+val+")\n"); // placeholder
        } else {
            crash("gSetAttribute(gLabel,\"" + name + "\") not yet implemented\n");
        }
    }
    store_attrs(["label"], ["TITLE","EXPAND","ALIGNMENT","MARGIN","PADDING","VISIBLE",
                            "FGCOLOR","FONT","FONTFACE","FONTSTYLE","SEPARATOR","NAME","TIP",
                            "TEXTWRAP"], set_label);
//  store_values("EXPAND",[["YES","NO","HORIZONTAL","HORIZONTALFREE","VERTICAL","VERTICALFREE"]])
//  store_values("ALIGNMENT",[["ALEFT","ACENTER","ARIGHT"],":",["ATOP","ACENTER","ABOTTOM"]])

    function set_list(id, name, val) {
        assert(id.classList[0] === "list");
        if (name === "CANFOCUS") {
            puts(1,"IupList(CANFOCUS,"+val+")??\n"); // placeholder
        } else if (name === "EXPAND") {
            expand(id,val);
//      } else if (name === "ALIGNMENT") {
//          align(id,val,"IupList");
        } else if (name === "PADDING") {
//          puts(1,"gList(PADDING,"+val+")??\n"); // erm, now banned? [or rather utilised by gH/Vbox?]
            set_style(id,"padding",val);
//      } else if (name === "DROPDOWN") {
//          puts(1,"IupList(DROPDOWN,"+val+")??\n"); // placeholder
//          val = $to_bool(val);
//          id.DROPDOWN = val;
//      } else if (name === "APPENDITEM") {
//          puts(1,"IupList(APPENDITEM,"+val+")??\n"); // placeholder
//      } else if (name === "REMOVEITEM") {
//          puts(1,"IupList(REMOVEITEM,"+val+")??\n"); // placeholder
        } else if (name === "TIP") {
            puts(1,"IupList(TIP,"+val+")??\n"); // placeholder
        } else if (name === "VALUE") {
            puts(1,"IupList(VALUE,"+val+")??\n"); // placeholder
        } else if (name === "VISIBLE") {
//          puts(1,"IupList(VISIBLE,"+val+")??\n"); // placeholder
//      } else if (name === "VISIBLELINES") {
//          puts(1,"IupList(VISIBLELINES,"+val+")??\n"); // placeholder
//      } else if (name === "VISIBLECOLUMNS") {
//          puts(1,"IupList(VISIBLECOLUMNS,"+val+")??\n"); // placeholder
//      } else if (name === "VISIBLEITEMS") {
//          puts(1,"IupList(VISIBLEITEMS,"+val+")??\n"); // placeholder
//      } else if (name === "SHOWDROPDOWN") {
//          puts(1,"IupList(SHOWDROPDOWN,"+val+")??\n"); // placeholder
//      } else if (name === "NAME") {
//          puts(1,"IupList(NAME,"+val+")??\n"); // placeholder
//      } else if (name === "EDITBOX") {
//          puts(1,"IupList(EDITBOX,"+val+")??\n"); // placeholder
//      } else if (name === "VALINT") {
//          puts(1,"IupList(VALINT,"+val+")??\n"); // placeholder
/*
    } else if (name === "VALINT" ||
               name === "VALSTR" ) {
        if (t === "list") {
            let children = id.childNodes,
                l = children.length;
            for (let i = 0; i < l; i += 1) {
                if (children[i].selected) {
                    if (name === "VALINT") {
                        return i+1;
                    }
                    return children[i].text;
                }
            }
            if (name === "VALINT") {
                return 0;
            }
            return "";
        }
*/
        } else {
            crash("gSetAttribute(IupList,\"" + name + "\") not yet implemented\n");
        }
    }
//DEV must do better...
//maybe: return ((parseInt(str, 10).toString() == str) && str.indexOf('-') === -1);

//  store_attrs(["list"], ["CANFOCUS","DROPDOWN","EXPAND","APPENDITEM","REMOVEITEM","VALINT",
//                         "TIP","VALUE","VISIBLE","VISIBLELINES","VISIBLECOLUMNS","VISIBLEITEMS",
//                         "SHOWDROPDOWN","NAME","EDITBOX"], set_list);
    store_attrs(["list"], ["CANFOCUS","EXPAND","TIP","VALUE","VISIBLE"], set_list);

    function set_box(id, name, val) {
        let cn = id.classList[0];
        assert((cn === "vbox") || (cn === "hbox"));
//DEV common up??
        if (name === "MARGIN") {
//          set_style(id,"margin",val);
            let [, w, h] = intint(val);
            if (w) {
                id.style.marginLeft = w + "px";
                id.style.marginRight = w + "px";
            }
            if (h) {
                id.style.marginTop = h + "px";
                id.style.marginBottom = h + "px";
            }
//      } else if (name === "NMARGIN") {
//          set_style(id,"margin",val);
//          puts(1,"IupH/Vbox(NMARGIN," + val + ")\n"); // placeholder
        } else if (name === "TABTITLE") {
            id.TABTITLE = val;
//      } else if (name === "ALIGNMENT") {
//21/10/21:
//          align(id,val,"IupH/Vbox");
//          align(id,":"+val,"IupH/Vbox");
        } else if (name === "PADDING") {
            set_style(id,"padding",val);
//          puts(1,"IupH/Vbox(PADDING,"+val+")\n"); // placeholder
        } else if (name === "NORMALIZESIZE") {
            puts(1,"gH/Vbox(NORMALIZESIZE," + val + ")\n"); // placeholder
//          $normalize(id.children,val);
        } else if (name === "GAP") {
//for now do nothing...
//          puts(1,"IupH/Vbox(GAP,"+val+")\n"); // placeholder
//      } else if (name === "SIZE") {
//          puts(1,"IupH/Vbox(SIZE," + val + ")\n"); // placeholder
//      } else if (name === "LINEBREAK") {
//          puts(1,"IupH/Vbox(LINEBREAK," + val + ")\n"); // placeholder
        } else if (name === "SPACE") {
            if (val === "LEFT") {
                id.style.justifyContent = "flex-end";
            } else {
                puts(1,"gH/Vbox(SPACE," + val + ")\n"); // placeholder
            }
        } else if (name === "FONT") {
            set_font(id,val);
//      } else if (name === "NORMALSIZE") {
//          puts(1,"gH/Vbox(LINEBREAK," + val + ")\n"); // placeholder
        } else if (name === "BGCOLOR") {
            puts(1,"gH/Vbox(BGCOLOR,"+val+")\n"); // placeholder
        } else {
            crash("gSetAttribute(gVbox,\"" + name + "\") not yet implemented");
        }
    }
    store_attrs(["vbox","hbox"], ["MARGIN","TABTITLE","PADDING",
//                                "ALIGNMENT","NMARGIN","NORMALIZESIZE","LINEBREAK","FONT","BGCOLOR"], set_box);
                                  "GAP","SPACE","FONT","BGCOLOR"], set_box);

    function set_text(id, name, val) {
        assert(id.classList[0] === "text");
        if (name === "VALUE") {
//          id.innerHTML = val;
//          id.value = val;
//          const lf = new RegExp("\\n","g");
//          val = val.replace(lf,"<br>");
//          id.innerHTML = val;
            id.value = val;
//          id.innerText = val;
//          id.textContent = val;
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
//          id.innerText = val;
        } else if (name === "EXPAND") {
            expand(id,val);
//      } else if (name === "MASK") {
////            puts(1,"IupText(MASK,"+val+")??\n"); // placeholder
//          id.type = "number";
////            id.type = "text";
//          id.pattern = val;
        } else if (name === "PADDING") {
//DEV... (sip)
            set_style(id,"margin",val);
//      } else if (name === "SPIN") {
////            puts(1,"IupText(SPIN,"+val+")??\n"); // placeholder
////            val = $to_bool(val); // (maybe??)
//          id.setAttribute("type", "number");
////            id.setAttribute("type", "number");
//      } else if (name === "SPINMIN") {
////            puts(1,"IupText(SPINMIN,"+val+")??\n"); // placeholder
//          id.setAttribute("min", val);
//      } else if (name === "SPINMAX") {
////            puts(1,"IupText(SPINMAX,"+val+")??\n"); // placeholder
//          id.setAttribute("max", val);
//      } else if (name === "FILTER") {
////[DEV] not really what I was hoping for...
////            if (val === "NUMBER") {
////                id.pattern = `^\d*$`;
////            } else {
//              puts(1,"IupText(FILTER," + val + ")??\n"); // placeholder
////            }
//      } else if (name === "CUEBANNER") {
//          puts(1,"IupText(CUEBANNER," + val + ")??\n"); // placeholder
//      } else if (name === "NAME") {
//          id["NAME"] = val;
//          puts(1,"?!IupText(NAME," + val + ")??\n"); // placeholder
        } else if (name === "TIP") {
            puts(1,"gText(TIP," + val + ")??\n"); // placeholder
//      } else if (name === "MULTILINE") {
//          puts(1,"IupText(MULTILINE," + val + ")??\n"); // placeholder
//      } else if (name === "FORMATTING") {
//          puts(1,"IupText(FORMATTING," + val + ")??\n"); // placeholder
//      } else if (name === "APPEND") {
//          puts(1,"IupText(APPEND," + val + ")??\n"); // placeholder
//      } else if (name === "INSERT") {
//          puts(1,"IupText(INSERT," + val + ")??\n"); // placeholder
//      } else if (name === "WORDWRAP") {
//          puts(1,"IupText(WORDWRAP," + val + ")??\n"); // placeholder
//      } else if (name === "SCROLLBAR") {
//          puts(1,"IupText(SCROLLBAR," + val + ")??\n"); // placeholder
//      } else if (name === "READONLY") {
//          puts(1,"IupText(READONLY," + val + ")??\n"); // placeholder
        } else if (name === "BGCOLOR") {
            puts(1,"gText(BGCOLOR," + val + ")??\n"); // placeholder
//      } else if (name === "VISIBLECOLUMNS") {
//          puts(1,"IupText(VISIBLECOLUMNS," + val + ")??\n"); // placeholder
//      } else if (name === "SELECTION") {
//          if (val === "ALL") {
//              id.select();
//          } else {
//              puts(1,"IupText(SELECTION," + val + ")??\n"); // placeholder
//          }
        } else {
            crash("gSetAttribute(gText,\"" + name + "," + val + "\") not yet implemented\n");
        }
    }
//  store_attrs(["text"], ["VALUE","EXPAND","MASK","PADDING","SPIN","SPINMIN","SPINMAX","FILTER",
//                         "CUEBANNER","NAME","TIP","MULTILINE","FORMATTING","APPEND","INSERT","WORDWRAP",
//                         "SCROLLBAR","SELECTION","READONLY","BGCOLOR","VISIBLECOLUMNS"], set_text);
    store_attrs(["text"], ["VALUE","EXPAND","PADDING","TIP","BGCOLOR"], set_text);

    function set_toggle(id, name, val) {
        assert(id.classList[0] === "toggle");
        if (name === "VALUE") {
//          id.innerHTML = val;
            val = $to_bool(val);
//21/10/21:
//          id.checked = val;
            id.children[1].checked = val;
            if (val && id.RGX) {
                let ids = id.RGX.RADIO_GROUP,
                    idl = length(ids);
                for (let i = 1; i <= idl; i += 1) {
                    let ri = ids[i],
                        cb = ri.children[1];
                    if (cb.checked && ri!=id) { 
                        cb.checked = false;
                    }
                }
            }
//      } else if (name === "RIGHTBUTTON") {
////            puts(1,"IupToggle(RIGHTBUTTON,"+val+")??\n"); // placeholder
//          val = $to_bool(val);
//          id.style.flexDirection = val?"row":"row-reverse";
        } else if (name === "PADDING") {
            set_style(id,"padding",val);
//          puts(1,"IupToggle(PADDING,"+val+")??\n"); // placeholder
//      } else if (name === "EXPAND") {
//          expand(id,val);
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
//DEV...
        } else if (name === "FONT") {
            set_font(id,val);
        } else {
            crash("gSetAttribute(IupToggle,\"" + name + "," + val + "\") not yet implemented\n");
        }
    }
//  store_attrs(["toggle"], ["CANFOCUS","NOTE","PADDING","RIGHTBUTTON","VALUE","TIP","FONT"], set_toggle);
    store_attrs(["toggle"], ["CANFOCUS","NOTE","PADDING","VALUE","TIP","FONT"], set_toggle);

//  function linebreak(id, name, val) {
//      // applies to dialog, label, text
//      if (name !== "LINEBREAK") { crash("LINEBREAK expected!"); }
//      let cn = id.classList[0];
//      puts(1,"linebreak("+cn+","+val+")??\n"); // placeholder
////        id[LINEBREAK] = val;
////        id.LINEBREAK = val;
//  }

    function set_button(id, name, val) {
        assert(id.classList[0] === "button");
//      if (name === "GAP") {
////DEV should be hbox/vbox only???
////            puts(1,"gButton(GAP,"+val+")??\n"); // placeholder
//          crash("gButton(GAP,"+val+")??\n");
//      } else 
        if (name === "EXPAND") {
            puts(1,"gButton(EXPAND,"+val+")??\n"); // placeholder
//          expand(id,val);
        } else if (name === "PADDING") {
            set_style(id,"padding",val);
//          puts(1,"gButton(PADDING,"+val+")??\n"); // placeholder
//      } else if (name === "RUNNING") {
////DEV I think this is a hack for demo\rosetta\Morpion_solitaire.exw ... [rework to USER_DATA]
//          val = $to_bool(val);
//          id[name] = val;
//      } else if (name === "TIP") {
//          puts(1,"gButton(TIP,"+val+")??\n"); // placeholder
        } else if (name === "IMAGE") {
            puts(1,"gButton(IMAGE,"+val+")??\n"); // placeholder
//  elsif name="IMAGE" then
//--DEV erm, not quite true: image *can* be changed, but not the presence/absence of an image....
//      assert(not bMapped,"image cannot be changed after mapping")
//--        ?{"xpg_set_button_attribute",id,name,v,ctrl_xtra[id]}
//      if string(v) then
//--            v = gImage_from_XPM(v,"BUTTON")
//          v = gImage_from_XPM(v,XPG_BTN_BG)
//--DEV this didn't work the other place I tried it... also (sip) [3]???
//      elsif backend=XPG_WINAPI then
//          v = xpg_WinAPI_replace_bgclr(v[3],XPG_BTN_BG)
//      end if
//      assert(sequence(v) and v[1]="gImage")
//      ctrl_xtra[id] = v
//      return true

//      } else if (name === "IMPRESS") {
//          puts(1,"gButton(IMPRESS,"+val+")??\n"); // placeholder
//      } else if (name === "BGCOLOR") {
//          puts(1,"gButton(BGCOLOR,"+val+")??\n"); // placeholder
        } else if (name === "TITLE") {
            id.innerHTML = "<nobr>" + val + "</nobr>";
        } else if (name === "USER_DATA") {
            id.USER_DATA = val;
        } else {
            crash("gSetAttribute(gButton,\"" + name + "," + val + "\") not yet implemented\n");
        }
    }
//  store_attrs(["button"], ["GAP","EXPAND","PADDING","IMAGE",
    store_attrs(["button"], ["EXPAND","PADDING","IMAGE",
//  "IMPRESS","BGCOLOR","RUNNING",
                             "TITLE","USER_DATA"], set_button);
//  store_attrs(["button"], ["LINEBREAK"], linebreak);
//ToDo: (found while surfin)
//button.setAttribute("disabled", "true");
//button.removeAttribute("disabled");
//[attribute]   [target]        Selects all elements with a target attribute            (PL: eg [inactive] ??)
//[attribute=value]     [target=_blank] Selects all elements with target="_blank"       (PL: eg [type=button] ??)

    function set_canvas(id, name, v) {
        let t = id.classList[0]
//      assert(t === "canvas");
//indexOf?
//      assert(t === "canvas" || t === "list");
        assert(t === "canvas" || t === "list" || t === "graph" || t === "drag-x" || t == "drag-y");
        if (name === "FONT") {
//          set_font(id,val);
//  function set_font(id,val) {
            let face = v, //fallback,
                style = "",
                stylesize = "",
                comma = v.indexOf(',');
            if (comma !== -1) {
                face = face.slice(0,comma);
                stylesize = v.slice(comma+1);
                let space = stylesize.indexOf(' ');
                while (space !== -1) {
//                  if (space) {
                    let stylename = stylesize.slice(0,space).toLowerCase();
                        // Bold, Italic, Underline and Strikeout
//                      if (stylename === "bold") {
//                          id.style.fontWeight = "bold";
//                      } else if (stylename === "italic") {
//                          id.style.fontStyle = "italic";
//                      } else if (stylename === "underline") {
//                          id.style.textDecoration = "underline";
//                      } else 
                    if (stylename === "strikeout") {
                        stylename = "line-through";
//text-decoration: underline;
//text-decoration: line-through;
//font-style: italic;
//font-weight: bold;
//                      } else {
//                          crash("FONT="+val+"??\n");
                    }
//                  if (length(style)) { style += " "; }
                    style += stylename + " ";
//                  }
                    stylesize = stylesize.slice(space+1);
                    space = stylesize.indexOf(' ');
                }
                stylesize = parseInt(stylesize);
                if (!stylesize) {
                    crash("FONT="+v+"??\n");
                } else if (stylesize < 0) {
//                  id.style.fontSize = -stylesize + "px";
                    stylesize = -stylesize + "px ";
                } else {
//                  id.style.fontSize = stylesize + "pt";
                    stylesize = stylesize + "pt ";
                }
//              face = style + stylesize + face; // eg "bold 12px sans-serif"
            }
            if (face === "Courier") {
                face = "courier";
            } else if (face === "Times") {
                face = "serif";
            } else if (face === "Helvetica" ||
                       face === "Calibri") {
                face = "sans-serif";
            } else {
                crash("gSetAttribute(canvas,`FONT`,"+face+") not supported"); // placeholder??
            }       
            face = style + stylesize + face; // eg "bold 12px sans-serif"
//          font = style + size + font; // eg "bold 12px sans-serif"
//          ctx.font = font;
//          ctx.font = face;
//          id.font = face;
            id.ctx.font = face;
//function cdCanvasFont(/*cdCanvas*/ ctx, /*nullable_string*/ font, /*integer*/ style, /*integer*/ size) {
////CD_PLAIN (0), CD_BOLD (1), CD_ITALIC (2), CD_UNDERLINE (4) and CD_STRIKEOUT (8).
////pGUI also provides the constant CD_BOLD_ITALIC (3) for convenience.
////ctx.font = "bold 48px serif";
////ctx.font = "50px serif";
//  if (font === "Courier") {
//      font = "courier";
//  } else if (font === "Times") {
//      font = "serif";
//  } else if (font === "Helvetica" ||
//             font === "Calibri") {
//      font = "sans-serif";
//  } else {
//      crash("cdCanvasFont(font="+font+") not supported"); // placeholder??
//  }       
//  if (style === CD_PLAIN) {
//      style = "";
//  } else if (style === CD_BOLD) {
//      style = "bold ";
//  } else if (style === CD_ITALIC) {
//      style = "italic ";
//  } else if (style === CD_BOLD_ITALIC) {
//      style = "bold italic ";
//  } else {
//      crash("cdCanvasFont(style="+style+") not supported"); // placeholder
//  }
//  if (size>=0) {
////        crash("cdCanvasFont(size="+size+") not supported (must use -ve pixels)");
//      size = size/12 + "em ";
//  } else {
//      size = -size + "px ";
//  }
////    size = -size;
////    font = style + size + "px " + font;
//  font = style + size + font; // eg "bold 12px sans-serif"
//  ctx.font = font;
////    puts(1,"cdCanvasFont() not yet supported..\n"); // placeholder
//}

//          puts(1,"gCanvas(SROLLABLE,"+val+")??\n"); // placeholder
//      } else if (name === "SCROLLINFO") {
//          puts(1,"gCanvas(SCROLLINFO,"+val+")??\n"); // placeholder
//      if (name === "BORDER") {
//          puts(1,"gCanvas(BORDER,"+val+")??\n"); // placeholder
//      } else if (name === "BUFFER") {
//          // (ignored, as now documented)
//      } else if (name === "DATA") {
//          puts(1,"gCanvas(DATA,"+val+")??\n"); // placeholder
//      } else if (name === "EXPAND") {
//          puts(1,"gCanvas(EXPAND,"+val+")??\n"); // placeholder
//      } else if (name === "DRAWCOLOR") {
//          // IupBackgroundBox really, as cdCanvasSetForeground(ctx, colour):
////    if (integer(colour)) { colour = sprintf("#%06x",colour); }
//          id.fillStyle = val;
//          id.strokeStyle = val;
//      } else if (name === "DRAWSTYLE") {
//          puts(1,"gCanvas(DRAWSTYLE,"+val+")??\n"); // placeholder
//      } else if (name === "DRAWFONT") {
//          puts(1,"gCanvas(DRAWFONT,"+val+")??\n"); // placeholder
//      } else if (name === "DRAWTEXTORIENTATION") {
//          puts(1,"gCanvas(DRAWTEXTORIENTATION,"+val+")??\n"); // placeholder
//      } else if (name === "DATA") {
//          id.DATA = Number(val);
//      } else if (name === "FONT") {
//          puts(1,"gCanvas(FONT,"+val+")??\n"); // placeholder
//      } else if (name === "SCROLLBAR") {
//          puts(1,"gCanvas(SCROLLBAR,"+val+")??\n"); // placeholder
//// gah, null at this point...
////            id.parentNode.style.overflow = "scroll";
//      } else if (name === "DX") {
//          puts(1,"gCanvas(DX,"+val+")??\n"); // placeholder
//      } else if (name === "DY") {
//          puts(1,"gCanvas(DY,"+val+")??\n"); // placeholder
        } else if (name === "BGCLR" ||
                   name === "BGCOLOR" ||
                   name === "BGCOLOUR") {
            if (integer(v)) { v = sprintf("#%06x",v); }
            id.style.backgroundColor = v;
        } else if (name === "FGCLR" ||
                   name === "FGCOLOR" ||
                   name === "FGCOLOUR") {
            if (integer(v)) { v = sprintf("#%06x",v); }
            let ctx = id.ctx;
            ctx.fillStyle = v;
            ctx.strokeStyle = v;
        } else if (name === "LINESTYLE" ) {
            let ctx = id.ctx;
            ctx.LINESTYLE = v;
//          if (v !== CD_CUSTOM) {
                v = [[],[8,8],[4,4],[12,8,4,8],[12,4,4,4,4,4]][v];
//          }
            ctx.setLineDash(v);
//?         if (v!==XPG_CONTINUOUS) { ctx.lineWidth = 1; }
        } else if (name === "LINEWIDTH" ) {
            let ctx = id.ctx;
            ctx.lineWidth = v;
//?         if (width!==1) { ctx.setLineDash([]); }
        } else {
            crash("gSetAttribute(gCanvas,\"" + name + "\") not yet implemented\n");
        }
    }
//"BGCOLOR","DRID","TITLESTYLE","BORDER","BUFFER","DATA",
//"DRAWCOLOR","DRAWSTYLE","DRAWFONT","DRAWTEXTORIENTATION",
//"SCROLLBAR","DX","DY","EXPAND","SCROLLINFO"
    store_attrs(["canvas","graph","list","drag-x","drag-y",], ["FONT","BGCLR","BGCOLOR","BGCOLOUR","FGCLR","FGCOLOR","FGCOLOUR",
                                            "LINESTYLE","LINEWIDTH"], set_canvas);

    function set_graph(id, name, val) {
        assert(id.classList[0] === "graph");
        if (name === "GRID" ||
            name === "XACROSS" ||
            name === "YACROSS" ||
            name === "LEGENDBOX") {
            val = $to_bool(val);
            id[name] = val;
        } else if (name === "BARMODE" ||
                   name === "GRIDCOLOR" ||
                   name === "GTITLE" ||
                   name === "MARKSTYLE" ||
                   name === "MODE" ||
                   name === "XNAME" ||
                   name === "YNAME" ||
//                 name === "XRID" ||
//                 name === "YRID" ||
                   name === "XTICKFMT" ||
                   name === "YTICKFMT") {
            id[name] = val;
        } else if (name === "XANGLE" ||
                   name === "YANGLE" ||
                   name === "XMARGIN" ||
                   name === "YMARGIN" ||
                   name === "XMAX" ||
                   name === "YMAX" ||
                   name === "XMIN" ||
                   name === "YMIN" ||
                   name === "XTICK" ||
                   name === "YTICK" ||
                   name === "XYSHIFT" ||
                   name === "YXSHIFT" ||
                   name === "MARKSIZE") {
            id[name] = Number(val);
        } else {
            crash("gSetAttribute(graph,\"" + name + "\")\n");
        }
    }
    store_attrs(["graph"], ["BARMODE","GRID","GRIDCOLOR","GTITLE","LEGENDBOX","MARKSTYLE","MARKSIZE","MODE",
                            "XANGLE","XACROSS","XMARGIN","XMAX","XMIN","XRID","XTICK","XTICKFMT","XYSHIFT",
                            "YANGLE","YACROSS","YMARGIN","YMAX","YMIN","YRID","YTICK","YTICKFMT","YXSHIFT",
                            "XNAME","YNAME"], set_graph);
/*
    "BORDER","BUFFER","DATA","EXPAND","BGCOLOR",
    "TITLESTYLE",
    "DRAWCOLOR","DRAWSTYLE","DRAWFONT","DRAWTEXTORIENTATION",
    "XANGLE","XCROSSORIGIN","XMARGIN","XMAX","XMIN","XRID","XTICK","XTICKFMT","XYSHIFT",
    "YANGLE","YCROSSORIGIN","YMARGIN","YMAX","YMIN","YRID","YTICK","YTICKFMT","YXSHIFT",
    "FONT","SCROLLBAR","DX","DY"]
*/

//  function set_list(id, name, val) {
//      assert(id.classList[0] === "list");
///!*
//      if (name === "GRID" ||
//          name === "LEGENDBOX") {
//          val = $to_bool(val);
//          id[name] = val;
//      } else if (name === "BARMODE" ||
//                 name === "GRIDCOLOR" ||
//                 name === "GTITLE" ||
//                 name === "MARKSTYLE" ||
//                 name === "MODE" ||
//                 name === "XNAME" ||
//                 name === "YNAME" ||
//                 name === "XTICKFMT" ||
//                 name === "YTICKFMT") {
//          id[name] = val;
//      } else if (name === "XANGLE" ||
//                 name === "YANGLE" ||
//                 name === "XACROSS" ||
//                 name === "YACROSS" ||
//                 name === "XMARGIN" ||
//                 name === "YMARGIN" ||
//                 name === "XMAX" ||
//                 name === "YMAX" ||
//                 name === "XMIN" ||
//                 name === "YMIN" ||
//                 name === "XTICK" ||
//                 name === "YTICK" ||
//                 name === "XYSHIFT" ||
//                 name === "YXSHIFT" ||
//                 name === "MARKSIZE") {
//          id[name] = Number(val);
//      } else {
//*!/
//          crash("gSetAttribute(list,\"" + name + "\")\n");
////        }
//  }
//  store_attrs(["list"], ["DEV",
//                          "XNAME","YNAME"], set_list);

    function set_datepick(id, name, val) {
        assert(id.classList[0] === "datepick");
        if (name === "MONTHSHORTNAMES") {
            puts(1,"IupDatePick(MONTHSHORTNAMES,"+val+")??\n"); // placeholder
        } else if (name === "ZEROPRECED") {
            puts(1,"IupDatePick(ZEROPRECED,"+val+")??\n"); // placeholder
        } else if (name === "VALUE") {
            const fs = new RegExp("/","g");
            val = val.replace(fs,"-");
            id.value = val;
        } else {
            crash("gSetAttribute(IupDatePick,\"" + name + "," + val + "\") not yet implemented\n");
        }
    }
    store_attrs(["datepick"], ["MONTHSHORTNAMES","ZEROPRECED","VALUE"], set_datepick);

//  function set_menuitem(id, name, val) {
//      assert(id.classList[0] === "menuitem");
//      if (name === "VALUE") {
////            val = $to_bool(val);
////            id.disabled = !val;
//          puts(1,"IupMenuItem(VALUE,"+val+")??\n"); // placeholder
//      } else if (name === "AUTOTOGGLE") {
////            val = $to_bool(val);
////            id.disabled = !val;
//          puts(1,"IupMenuItem(AUTOTOGGLE,"+val+")??\n"); // placeholder
//      } else {
//          crash("gSetAttribute(IupMenuItem,\"" + name + "\"," + val + ") not yet implemented\n");
//      }
//  }
//  store_attrs(["menuitem"], ["VALUE","AUTOTOGGLE"], set_menuitem);

    function set_progress(id, name, val) {
        assert(id.classList[0] === "progress");
//      if (name === "EXPAND") {
//          expand(id,val);
        if (name === "ORIENTATION") {
            puts(1,"IupProgressBar(ORIENTATION,"+val+")??\n"); // placeholder
//          if (val === "HORIZONTAL") {
//              drag.className = "drag-y"
//              id.style.flexDirection = "column";
//          } else {
//              assert(val === "VERTICAL");
//              drag.className = "drag-x"
//              id.style.flexDirection = "row";
//          }
        } else if (name === "MIN") {
            puts(1,"IupProgressBar(MIN,"+val+")??\n"); // placeholder
            id.max = val;
        } else if (name === "MAX") {
            puts(1,"IupProgressBar(MAX,"+val+")??\n"); // placeholder
            id.min = val;
        } else if (name === "VALUE") {
//          puts(1,"IupProgressBar(MAX,"+val+")??\n"); // placeholder
            id.value = val;
        } else if (name === "EXPAND") {
            puts(1,"IupProgressBar(EXPAND,"+val+")??\n"); // placeholder
        } else {
            crash("gSetAttribute(IupProgressBar,\"" + name + "\"," + val + ") not yet implemented\n");
        }
    }
    store_attrs(["progress"], ["ORIENTATION","MIN","MAX","VALUE","EXPAND"], set_progress);

    function set_spin(id, name, val) {
        assert(id.classList[0] === "spin");
        if (name === "MARGIN") {
            set_style(id,"margin",val);
        } else if (name === "RANGE") {
            if (string(val)) { val = intint(val); }
            id.setAttribute("min", val[1]);
            id.setAttribute("max", val[2]);
        } else if (name === "STEP") {
            id.setAttribute("step", val);
        } else if (name === "VALUE") {
            id.setAttribute("value", val);
            id.LASTVALUE = val;
        } else if (name === "WRAP") {
            if (string(val)) { val = $to_bool(val); }
            let wrap = val ? 1 : 0;
            id.WRAP = wrap;
        } else {
            crash("gSetAttribute(gSpin,\"" + name + "\"," + val + ") not yet implemented\n");
        }
    }
    store_attrs(["spin"], ["MARGIN","RANGE","STEP","VALUE","WRAP"], set_spin);

    function set_split(id, name, val) {
        assert(id.classList[0] === "split");
//      if (name === "GAP") {
//          puts(1,"IupMultiBox(GAP,"+val+")??\n"); // placeholder
//      } else if (name === "EXPAND") {
//          expand(id,val);
        if (name === "ORIENTATION") {
//          puts(1,"IupSplit(ORIENTATION,"+val+")??\n"); // placeholder
            let drag = id.children[1];
            if (val === "HORIZONTAL") {
                drag.className = "drag-y"
                id.style.flexDirection = "column";
            } else {
                assert(val === "VERTICAL");
                drag.className = "drag-x"
                id.style.flexDirection = "row";
            }
//      } else if (name === "MINMAX") {
//          puts(1,"IupSplit(MINMAX,"+val+")??\n"); // placeholder
        } else if (name === "FRAC") {
            puts(1,"gSplit(FRAC,"+val+")??\n"); // placeholder
        } else {
            crash("gSetAttribute(gSplit,\"" + name + "\"," + val + ") not yet implemented\n");
        }
    }
//  store_attrs(["split"], ["ORIENTATION","MINMAX"], set_split);
    store_attrs(["split"], ["ORIENTATION","FRAC"], set_split);

    function set_tabcontainer(id, name, val) {
        assert(id.classList[0] === "tabcontainer");
//      if (name === "GAP") {
//          puts(1,"IupMultiBox(GAP,"+val+")??\n"); // placeholder
//      } else if (name === "EXPAND") {
//          expand(id,val);
//      } else if (name === "PADDING") {
//          set_style(id,"padding",val);
//          puts(1,"IupMultiBox(PADDING,"+val+")??\n"); // placeholder
        if (name === "TABTYPE") {
            puts(1,"IupTabs(TABTYPE,"+val+")??\n"); // placeholder
        } else {
            crash("gSetAttribute(IupTabs,\"" + name + "\"," + val + ") not yet implemented\n");
        }
    }
    store_attrs(["tabcontainer"], ["TABTYPE"], set_tabcontainer);

    function set_slider(id, name, val) {
        assert(id.classList[0] === "slider");
        if (name === "ORIENTATION") {
//          puts(1,"gSlider(ORIENTATION,"+val+")??\n"); // placeholder
            if (val === "VERTICAL") {
                id.style.transform = "rotate(90deg)";
            } else if (val === "HORIZONTAL") {
                id.style.transform = "";
            } else {
                crash("uh?");
            }
        } else if (name === "MAX") {
//          puts(1,"gSlider(MAX,"+val+")??\n"); // placeholder
            id.setAttribute("max", val);
        } else if (name === "MIN") {
//          puts(1,"gSlider(MIN,"+val+")??\n"); // placeholder
            id.setAttribute("min", val);
        } else if (name === "VALUE") {
//          puts(1,"gSlider(VALUE,"+val+")??\n"); // placeholder
            id.setAttribute("value", val);
        } else if (name === "STEP") {
//          puts(1,"gSlider(STEP,"+val+")??\n"); // placeholder
            id.setAttribute("step", val);
        } else if (name === "PAGESTEP") {
            puts(1,"gSlider(PAGESTEP,"+val+")??\n"); // placeholder
//          id.setAttribute("step", val);
        } else if (name === "EXPAND") {
            puts(1,"gSlider(EXPAND,"+val+")??\n"); // placeholder
//22/1/24 (spotted in passing... flexBasis??)
//          id.style.flexGrow = 1;
            id.style.flexGrow = val;
        } else if (name === "TIP") {
            puts(1,"gSlider(TIP,"+val+")??\n"); // placeholder
//          id.setAttribute("step", val);
        } else if (name === "CANFOCUS") {
// just ignore for now...
//          puts(1,"gSlider(CANFOCUS,"+val+")??\n"); // placeholder
        } else {
            crash("gSetAttribute(gSlider,\"" + name + "\"," + val + ") not yet implemented\n");
        }
    }
    store_attrs(["slider"], ["ORIENTATION","MAX","MIN","VALUE","STEP","PAGESTEP","EXPAND","TIP",
                             "CANFOCUS"], set_slider);

    function set_clipboard(id, name, val) {
        assert(id.classList[0] === "clipboard");
        if (name === "TEXT") {
//          puts(1,"IupClipboard(TEXT,"+val+")??\n"); // placeholder
            id.value = val;
            document.body.appendChild(id);
            id.focus();
            id.select();
            document.execCommand("copy");
            document.body.removeChild(id);
//          window.Clipboard.prototype.writeText(val);
        } else {
            crash("gSetAttribute(IupClipboard,\"" + name + "\"," + val + ") not yet implemented\n");
        }
    }
    store_attrs(["clipboard"], ["TEXT"], set_clipboard);

} // $gInit() ends

/*global*/ function gGetAlignName(/*integer*/ d) {
//  if d=-1 then return "-1" end if -- erm, no
    if (equal(d,XPG_C)) { return "XPG_C"; }
    let /*string*/ res = "XPG_";
    let bc$seq = ["sequence",["sequence",XPG_N,0X4E],["sequence",XPG_S,0X53],["sequence",XPG_E,0X45],["sequence",XPG_W,0X57]]; 
    for (let bc$idx = 1; bc$idx <= 4; bc$idx += 1) { let bc = $subse(bc$seq,bc$idx);
        if (and_bits(d,$subse(bc,1))) { res = $conCat(res, $subse(bc,2), false); }
    }
    return res;
}


function gSetGlobal(name, v) {
//  if (name === "UTF8MODE") {
//      // do nothing... (it is already/permanently enabled in JavaScript)
//  } else if (name === "SINGLEINSTANCE") {
//      puts(1,"gSetGlobal(" + name + "," + v + ")...\n");
//  } else if (name === "INPUTCALLBACKS") {
//      puts(1,"gSetGlobal(" + name + "," + v + ")...\n");
//  } else {
//      crash("gSetGlobal(" + name + "," + v + ")...");
        puts(1,"gSetGlobal(" + name + "," + v + ")...\n");
//  }
}
//const gSetGlobalInt = gSetGlobal;

function gGetGlobalInt(/*string*/ name) {
    if (name === "CONTROLKEY") { return $ctrlKey; }
    if (name === "SHIFTKEY") { return $shiftKey; }
    if (name === "UTF8MODE") { return true; }
    crash("gGetGlobalInt(%s) not supported",["sequence",name]);
}

function gGetGlobalIntInt(/*string*/ name) {
    if (name === "SCREENSIZE") { return ["sequence",window.innerWidth,window.innerHeight]; }
    crash("gGetGlobalIntInt(%s) not supported",["sequence",name]);
}

function gVersion(/*integer*/ bBack=false) {
    if (bBack) { return (bBack==-1) ? XPG_JS : "JS"; }
    // note the user agent and friends can all be spoofed anyway...
    let ua = navigator.userAgent,
        browser = navigator.appName,
        browver = " " + parseInt(navigator.appVersion,10);
    if (ua.indexOf("OPR/")!=-1 ||
        ua.indexOf("Opera")!=-1) { browser = "Opera"; } else
    if (ua.indexOf("MSIE")!=-1)  { browser = "IE"; } else 
    if (ua.indexOf("Chrome")!=-1) { browser = "Chrome"; } else 
    if (ua.indexOf("Safari")!=-1) { browser = "Safari"; } else 
    if (ua.indexOf("Firefox")!=-1) { browser = "Firefox"; }
    else {
        let nameOffset = ua.lastIndexOf(' ')+1,
             verOffset = ua.lastIndexOf('/'), no_ver = true;
        if (nameOffset < verOffset) {
            let bsub = ua.substring(nameOffset,verOffset);
            if (bsub.toLowerCase()!==bsub.toUpperCase()) {
                browser = bsub;
                no_ver = false;
            }
        }
        if (no_ver) { browver = ""; }
    }
    let plat = navigator.platform + " using " + browser + browver;
    return "xpGUI.js version 0.1 (32 bits) on " + plat;
/*
    let plat = navigator.platform,
          ua = navigator.userAgent, browser = ua,
        isIE = /!*@cc_on!@*!/false || !!document.documentMode,
      isEdge = !isIE && !!window.StyleMedia;
    if (!!window.opera || ua.indexOf(' OPR/') >= 0) { browser = "Opera"; } else
    if (typeof InstallTrigger !== 'undefined')      { browser = 'Firefox'; } else
    if (ua.indexOf("Chrome") != -1 && !isEdge)      { browser = 'Chrome'; } else
    if (ua.indexOf("Safari") != -1 && !isEdge)      { browser = 'Safari'; } else
//  if (ua.indexOf("Firefox") != -1 )               { browser = 'Firefox'; } else
    if ((ua.indexOf("MSIE") != -1 ) || //IF IE > 10
       (!!document.documentMode == true ))          { browser = 'IE'; } else 
    if (isEdge)                                     { browser = 'Edge'; }
    return "xpGUI.js version 0.1 (32 bits) on " + plat + " using " + browser;
--*/
/*
from https://stackoverflow.com/questions/9847580/how-to-detect-safari-chrome-ie-firefox-and-opera-browsers
getBrowser = () => {
    const userAgent = navigator.userAgent;
    let browser = "unkown";
    // Detect browser name
    browser = (/ucbrowser/i).test(userAgent) ? 'UCBrowser' : browser;
    browser = (/edg/i).test(userAgent) ? 'Edge' : browser;
    browser = (/googlebot/i).test(userAgent) ? 'GoogleBot' : browser;
    browser = (/chromium/i).test(userAgent) ? 'Chromium' : browser;
    browser = (/firefox|fxios/i).test(userAgent) && !(/seamonkey/i).test(userAgent) ? 'Firefox' : browser;
    browser = (/; msie|trident/i).test(userAgent) && !(/ucbrowser/i).test(userAgent) ? 'IE' : browser;
    browser = (/chrome|crios/i).test(userAgent) && !(/opr|opera|chromium|edg|ucbrowser|googlebot/i).test(userAgent) ? 'Chrome' : browser;;
    browser = (/safari/i).test(userAgent) && !(/chromium|edg|ucbrowser|chrome|crios|opr|opera|fxios|firefox/i).test(userAgent) ? 'Safari' : browser;
    browser = (/opr|opera/i).test(userAgent) ? 'Opera' : browser;

    // detect browser version
    switch (browser) {
        case 'UCBrowser': return `${browser}/${browserVersion(userAgent,/(ucbrowser)\/([\d\.]+)/i)}`;
        case 'Edge': return `${browser}/${browserVersion(userAgent,/(edge|edga|edgios|edg)\/([\d\.]+)/i)}`;
        case 'GoogleBot': return `${browser}/${browserVersion(userAgent,/(googlebot)\/([\d\.]+)/i)}`;
        case 'Chromium': return `${browser}/${browserVersion(userAgent,/(chromium)\/([\d\.]+)/i)}`;
        case 'Firefox': return `${browser}/${browserVersion(userAgent,/(firefox|fxios)\/([\d\.]+)/i)}`;
        case 'Chrome': return `${browser}/${browserVersion(userAgent,/(chrome|crios)\/([\d\.]+)/i)}`;
        case 'Safari': return `${browser}/${browserVersion(userAgent,/(safari)\/([\d\.]+)/i)}`;
        case 'Opera': return `${browser}/${browserVersion(userAgent,/(opera|opr)\/([\d\.]+)/i)}`;
        case 'IE': const version = browserVersion(userAgent,/(trident)\/([\d\.]+)/i);
            // IE version is mapped using trident version 
            // IE/8.0 = Trident/4.0, IE/9.0 = Trident/5.0
            return version ? `${browser}/${parseFloat(version) + 4.0}` : `${browser}/7.0`;
        default: return `unknown/0.0.0.0`;
    }
}

browserVersion = (userAgent,regex) => {
    return userAgent.match(regex) ? userAgent.match(regex)[2] : null;
}

console.log(getBrowser());
AND/OR:
var BrowserType;
(function (BrowserType) {
    BrowserType["OPERA"] = "Opera";
    BrowserType["OPERA2"] = "OPR";
    BrowserType["EDGE"] = "Edg";
    BrowserType["CHROME"] = "Chrome";
    BrowserType["SAFARI"] = "Safari";
    BrowserType["FIREFOX"] = "Firefox";
    BrowserType["UNKNOWN"] = "unknown";
})(BrowserType || (BrowserType = {}));
const detectBrowser = () => {
    return Object.values(BrowserType).find((browser) => navigator.userAgent.indexOf(browser) != -1);
};
console.log(detectBrowser());
AND/OR:
https://github.com/darcyclarke/Detect.js/blob/master/detect.js
AND/OR:
Engine                          Must contain    Must not contain
Firefox                         Firefox/xyz     Seamonkey/xyz
Seamonkey                       Seamonkey/xyz   
Chrome                          Chrome/xyz      Chromium/xyz or Edg.*!/xyz
Chromium                        Chromium/xyz    
Safari                          Safari/xyz      Chrome/xyz or Chromium/xyz
Opera 15+ (Blink-based engine)  OPR/xyz 
Opera 12- (Presto-based engine) Opera/xyz       
AND/OR:
navigator.saysWho = (() => {
  const { userAgent } = navigator
  let match = userAgent.match(/(opera|chrome|safari|firefox|msie|trident(?=\/))\/?\s*(\d+)/i) || []
  let temp

  if (/trident/i.test(match[1])) {
    temp = /\brv[ :]+(\d+)/g.exec(userAgent) || []

    return `IE ${temp[1] || ''}`
  }

  if (match[1] === 'Chrome') {
    temp = userAgent.match(/\b(OPR|Edge)\/(\d+)/)

    if (temp !== null) {
      return temp.slice(1).join(' ').replace('OPR', 'Opera')
    }

    temp = userAgent.match(/\b(Edg)\/(\d+)/)

    if (temp !== null) {
      return temp.slice(1).join(' ').replace('Edg', 'Edge (Chromium)')
    }
  }

  match = match[2] ? [ match[1], match[2] ] : [ navigator.appName, navigator.appVersion, '-?' ]
  temp = userAgent.match(/version\/(\d+)/i)

  if (temp !== null) {
    match.splice(1, 1, temp[1])
  }

  return match.join(' ')
})()

console.log(navigator.saysWho) // outputs: `Chrome 89`
--*/
}

function gGetGlobal(/*string*/ name) {
    if (name === "VERSION") {
        return gVersion();
    } else if (name === "DLGBGCOLOR") {
//      return "240 240 240";
        return ["sequence",0xF0,0xF0,0xF0];
    } else if (name === "SCREENSIZE") {
//      return window.innerWidth + "x" + window.innerHeight;
//      return ["sequence",window.innerWidth,window.innerHeight];
        return gGetGlobalIntInt(name);
    } else if (name === "CONTROLKEY" ||
               name === "SHIFTKEY" ||
               name === "UTF8MODE") {
        return gGetGlobalInt(name);
    }
    crash("gGetGlobal(" + name + ")...");
}

function gSetGlobalFunction(name, v) {
    if (name === "IDLE_ACTION") {
        function cb() {
            let res = v();
//? !=XPG_IGNORE ?
            if (res === XPG_DEFAULT) {
                window.requestIdleCallback(cb); 
            }
        }
        window.requestIdleCallback(cb);
    } else {
        crash("gSetGlobalFunction(" + name + ")...");
    }
}

function $timer(/*string*/cmd, id, name, v) { // nb: mapped to ("create",func,msecs,active)
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
        let func = id, msecs = name, active = v; // map args to more sensible names
        id = ["timer",func,msecs,active,0,action_cb];
        if (active) {
            id[ID] = setInterval(action_cb,msecs,id);
        }
        return id;

    } else if (cmd === "is") {
        return Array.isArray(id) && id[TIMER] === "timer";
    } else {
        if (name === "RUNNING" || name === "ACTIVE") { name = "RUN"; }
        if (name === "MSECS" || name === "MSEC") { name = "TIME"; }
        if (cmd === "set") {
            if (id[ID]) {
                clearInterval(id[ID]);
                id[ID] = 0;
            }
            if (name === "RUN") {
                id[ACTIVE] = v;
            } else if (name === "TIME") {
                id[MSECS] = v;
            } else {
                crash("timer??");
            }
            if (id[ACTIVE]) {
                id[ID] = setInterval(id[CB],id[MSECS],id);
            }
        } else if (cmd === "get") {
            if (name === "RUN") { return id[ACTIVE]; }
            if (name === "TIME") { return id[MSECS]; }
            crash("timer??")
        }
    }
}   

//(gdx id, string name, nullable_string v, sequence args={}) 
function gSetAttribute(id, name, v, args = []) {
    if ($timer("is",id)) {
        if (args.length) {
            v = sprintf(v, args);
        }
        v = $to_bool(v);
        $timer("set", id, name, v);
        return;
    }
    if (sequence(id)) {
//22/1/24 (sip/untested/for no good reason...)
//      for (let i = length(id); i >= 1; i -= 1) {
        let lid = length(id);
        for (let i = 1; i < lid; i += 1) {
            gSetAttribute(id[i], name, v);
        }
        return;
    }
//  let t = id.className;
    let t = id.classList[0];
//  let t = $class_name(id);
    if (!t || !$storeAttr.hasOwnProperty(t)) {
        crash("invalid type");
    }
    if (typeof(name) === "string") {
        if (args.length) {
            v = sprintf(v, args);
        }
        if (t === "drop" && name === "OPTIONS") {
//      } else if (name === "<list_id>") {
//          puts(1,"IupList(<list_id>,"+val+")??\n"); // placeholder
//          [/*integer*/ name, /*string*/ val] = val;
            //... (DEV)
            let n = id.childElementCount;
            if (n) {
                // placeholder
                crash("delete prior options not yet implemented");
            }
            n = length(v);
//          if (name>n) {
            for (let i = 1; i <= n; i += 1) {
                let vi = v[i],
                    childoption = document.createElement("option"),
                    t = document.createTextNode(vi);
//              childoption.setAttribute("value", name.toString());
                childoption.appendChild(t);
                id.appendChild(childoption);
//              n += 1;
            } 
            id.selectedIndex = "-1";
/*
            else {
                let childoption = id.children[n-1],
                    t = childoption.childNodes[0];
                t.textContent = val;
//              crash("IupList replace???"); // placeholder
            }
*/
//DEV??
//          if (!id.DROPDOWN) { id.size = n; }
//          id.size = n; (NO!!)
            return;
        }
/*
        let bOK = $storeAttr[t].hasOwnProperty(name);
//DEV..
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
*/
        if ($storeAttr[t].hasOwnProperty(name)) {
            let fn = $storeAttr[t][name];
            if (typeof(fn) !== "function") {
                crash("invalid fn");
            }
            fn(id, name, v);
            return;
        }
    }
    crash("invalid attr name (%s for %s)",["sequence",name,t]);
}
const gSetInt = gSetAttribute;
const gSetDouble = gSetAttribute;

//const gSetAttribute = IupSetStrAttribute;
//const gSetAttribute = IupSetStrAttribute;

//function IupSetAttributeId(id, name, idx, v) {
//  let t = id.classList[0];
//  if (t === "list" && name === "") {
//      name = "<list_id>"
//      let fn = $storeAttr[t][name];
//      v = [idx,v]; // (set_list() does matching desequence)
//      fn(id, name, v);
//      return;
//  }
//  printf(1,"IupSetAttributeId(%s,\"%s\",%d,%V)??\n",["sequence",t,name,id,v]);
////    id[name] = v;
//}

//DEV gSetInt === gSetAttribute...
//DEV kill when GRIDCOLOR/timer all done and tested...
function IupSetInt(id, name, v) {
    if (name!="GRIDCOLOR") {
        let tv = typeof(v);
        if (tv !== "number" && 
            tv !== "boolean" && 
            tv !== "function") {
            crash("gSetInt("+name+","+tv+")??");
        }
    }
    if ($timer("is", id)) {
        $timer("set", id, name, v);
    } else if (sequence(id)) {
        for (let i = length(id); i >= 1; i -= 1) {
            gSetAttribute(id[i], name, v);
        }
    } else {
        gSetAttribute(id, name, v);
    }
}

function gGetAttribute(id, name, dflt) {
//  if (string(id) && id === "clipboard") {
//      if (name !== "TEXT") { crash("uh?"); }
//      puts(1,"gGetAttribute(clipboard,`TEXT`)==>``??\n");
////        let text = window.clientInformation.clipboard.readText(); // ugh, a /Promise/...
////        let text = window.clientInformation.clipboard.read();
////        return text;
//      return "";
//  }
    if ($timer("is",id)) {
        return $timer("get",id,name);
    }
    let t = id.classList[0];
    if (name === "VALUE") {
        if (t === "text"
        ||  t === "datepick"  // nb eg 2021-11-05 (for docs)
        ||  t === "spin") {
            return id.value;
        } else if (t === "slider") {
            return Number(id.value);
        }
    } else if (name === "VALINT" ||
               name === "VALSTR" ) {
        if (t === "drop") {
            let children = id.childNodes,
                l = children.length,
                vi = (name === "VALINT");
            for (let i = 0; i < l; i += 1) {
                if (children[i].selected) {
//                  if (name === "VALINT") {
//                      return i+1;
//                  }
                    return vi ? i+1 : children[i].text;
                }
            }
//          if (name === "VALINT") {
//              return 0;
//          }
//          return "";
            return vi ? 0 : "";
        }
    } else if ((name === "OPTIONS") && (t === "drop")) {
        let children = id.childNodes,
                   l = children.length,
                 res = repeat(0,l);
        for (let i = 0; i < l; i += 1) {
            res[i+1] = children[i].text;
        }
        return res;
    } else if (name === "TITLE") {
        if (t === "button" ||
            t === "toggle" ||
            t === "label" ) {
// "menu"??
            return id.innerText;
        } else if (t === "frame") {
            return id.childNodes[0].innerText;
//DEV??
        } else if (t === "hbox" ||
//      } else if (t === "hbox" ) {
                   t === "canvas" ||
                   t === "graph" ||
                   t === "list") {
            return gGetAttribute(id.parentNode,name);
        }
    } else if (name === "RASTERSIZE" || name === "SIZE") {
//      if (name === "RASTERSIZE") {
//          return ["sequence", $eWidth(id), $eHeight(id)];
        let w = id.width, 
            h = id.height;
        if (!w) { w = $eWidth(id); }
        if (!h) { h = $eHeight(id); }
        return ["sequence", w, h];
//      return sprintf("%dx%d",["sequence",$eWidth(id), $eHeight(id)]);
//      }
    } else if (name === "BGCLR" || 
               name === "BGCOLOR" ||
               name === "BGCOLOUR") {
//DEV or maybe drag-x/y -> canvas above?? 
        if (t === "canvas" ||
            t === "graph" ||
            t === "drag-x" ||
            t === "drag-y") {
            return id.style.backgroundColor;
        }
    } else if (name === "FGCLR" || 
               name === "FGCOLOR" ||
               name === "FGCOLOUR") {
        if (t === "canvas" ||
            t === "graph" ||
            t === "drag-x" ||
            t === "drag-y") {
            return id.ctx.strokeStyle;
        }
    } else if (name === "LINESTYLE" ) {
        if (t === "canvas" ||
            t === "drag-x" ||
            t === "drag-y") {
            return id.ctx.LINESTYLE || XPG_CONTINUOUS;
        }
    } else if (name === "LINEWIDTH" ) {
        if (t === "canvas" ||
            t === "drag-x" ||
            t === "drag-y") {
            return id.ctx.lineWidth;
        }
    } else if (t === "graph" || t === "list") {
//      if (name === "GTITLE" ||
//          name === "XNAME" ||
//          name === "YNAME" ||
//          name === "MODE" ||
//          name === "MARKSTYLE" ||
//          name === "LEGENDBOX") {
//          if (!id[name]) { return dflt; }
//          return id[name];
            let res = id[name];
            if (typeof(res) === 'undefined') { res = dflt; }
            return res;
//      }
    } else if (name === "TEXT") {
        if (t === "clipboard" ) {
//          puts(1,"IupClipboard(TEXT,"+val+")??\n"); // placeholder
//          id.value = "paste";
            id.value = "1";
            document.body.appendChild(id);
            id.style.display = "block";
            id.select();
            id.focus();
//DEV does not work:
//          document.execCommand("copy");
//                  if (document.execCommand !== undefined && document.execCommand("copy") !== true) {
//                      throw new Error(`Unsupported "copy" clipboard command`);
//                  }
//                  if (document.execCommand !== undefined && document.execCommand("paste", null, null)) {
//                      return clip.value;
            let res = document.execCommand("paste",null,null);
            document.body.removeChild(id);
            return id.value;
// "illegal invocation"...
//          return window.Clipboard.prototype.readText();
        }
    }
//DEV??
    if (id.hasOwnProperty(name)) { // (DBUFFER etc)
        return id[name];
//  } else if (t === "canvas" && dflt &&
//             (name === "XTICTFMT" ||
//              name === "YTICTFMT") {
    } else if (dflt) {
        return dflt;
    } else if (name === "USER_DATA") {
        return NULL;
    }
    crash("gGetAttribute(%s,%s) not supported",["sequence",t,name]);
}   

//DEV gGetInt:
//  if name="EXPAND" then
//      return and_bits(ctrl_flags[id],CF_EXPANDH+CF_EXPANDV)
//  end if

function gGetInt(id, name, dflt) {
    if (name === "EXPAND") {
        let res = 0;
        if (id.classList.contains("expandh")) { res += 1; }
        if (id.classList.contains("expandv")) { res += 2; }
        return res;
    }
    return gGetAttribute(id, name, dflt);
}
const gGetIntInt = gGetAttribute;
const gGetDouble = gGetAttribute;

//function IupSetAttributePtr(id, name, v) {
//  id[name] = v;
//}

// 28/4/22 (may have jumped the gun.. [while updating the docs, rather than actually using])
//let IupGetAttributePtr = IupGetAttribute; // NO!!
//function IupGetAttributePtr(id, name) {
//  return id[name];
//}

//function IupGetAttributeId(id, name, id) {
//  let t = id.classList[0];
//  if (t === "list") {
//      if (name === "") {
//          let n = id.childElementCount;
//          if (id<=n) {
//              let childoption = id.children[n-1],
//                  t = childoption.childNodes[0];
//              return t.textContent;
//          }
//          crash("uh?");
////            return "";
//      }
//  }
//  crash("IupGetAttributeId(%s,%s,%d) not supported",["sequence",t,name,id]); // placeholder
//} 

//DEV... gGetInt === gGetAttribute....
function IupGetInt(id, name, dflt=0) {
//  if (id === 0) {
//      return IupGetGlobalInt(name);
////        if (name === "CONTROLKEY") { return $ctrlKey; }
////        if (name === "SHIFTKEY") { return $shiftKey; }
////        crash("IupGetInt(NULL,%s) not supported",["sequence",name]);
//  } else 
    if ($timer("is",id)) {
        return $timer("get",id,name);
    }
    let t = id.classList[0];
    if (t === "button") {
        if (name === "ACTIVE") {
            return !id.disabled;
//DEV...
        } else if (name === "RUNNING") {
            return id[name];
        }
    } else if (t === "text") {
        if (name === "VALUE") {
            if (id.value === "") { return 0; }
            return parseInt(id.value);
        }
    } else if (t === "label") {
        if (name === "TITLE") {
            if (id.innerText === "") { return 0; }
            return parseInt(id.innerText);
        }
    } else if (t === "toggle") {
        if (name === "VALUE") {
//          val = $to_bool(val);
//DEV to_bool not in scope...? (is innerText actually so???) [IupToggle is ~1% implemented, if that]
//          let val = id.innerText;
//          if (string(val)) { val = (val.toUpperCase()==="YES"); }
//8/4/22 (for Spinning Rod Animation)
//          return id.innerText.toUpperCase() === "YES";
            return id.childNodes[1].checked;
        }
//  } else if (t === "canvas") {
//      if (name === "DATA" ||
//          name === "DRID" ||
//          name === "GRID" ||
//          name === "GRIDCOLOR" ||
//          name === "LEGENDBOX" ||
//          name === "POSX" ||
//          name === "POSY" ||
//          name === "TITLESTYLE" ||
//          name === "XANGLE" ||
//          name === "XCROSSORIGIN" ||
//          name === "XMARGIN" ||
//          name === "XMAX" ||
//          name === "XMIN" ||
//          name === "XRID" ||
//          name === "XTICK" ||
//          name === "XTICKFMT" ||
//          name === "XYSHIFT" ||
//          name === "YANGLE" ||
//          name === "YCROSSORIGIN" ||
//          name === "YMARGIN" ||
//          name === "YMAX" ||
//          name === "YMIN" ||
//          name === "YRID" ||
//          name === "YTICK" ||
//          name === "YTICKFMT" ||
//          name === "YXSHIFT") {
//          return id[name] || dflt;
//      }
    } else if (t === "list") {
        if (name === "VALUE") {
            let children = id.childNodes,
                l = children.length;
            for (let i=0; i<l; i += 1) {
                if (children[i].selected) {
                    return i+1;
                }
            }
            return 0;
        } else if (name === "COUNT") {
            return id.childElementCount;
        }
    } else if (t === "dialog") {
        if (name === "VISIBLE") {
//          return !id.hidden;
//22/1/24 (spotted in passing, untested)
//          return id.isConnected;
            return id.isConnected && id.style.display !== "none";
        }
    } else if (t === "slider") {
        if (name === "VALUE") {
            return id.valueAsNumber;
        }
    }
    crash("IupGetInt(%s,%s) not supported",["sequence",t,name]);
}   

function gToggleInt(/*gdx*/ id, /*string*/ name) {
    gSetInt(id,name,!gGetInt(id,name))
}

function IupGetIntInt(id, name) {
//  if (typeof(name) !== "string" ...??
    let t = id.classList[0];
    if (name == "RASTERSIZE") {
        if (t === "canvas" ||
            t === "graph" ||
            t === "list" ||
            t === "dialog") {
//erm...
//          const rect = id.getBoundingClientRect(),    // (nb: recalc in DOM)
//                w = rect.width,
//                h = rect.height;
//          crash("RASTERSIZE expected!");
//          return ["sequence",w,h];
//          return ["sequence",id.width,id.height];
            return ["sequence",id.clientWidth,id.clientHeight];
//dialog:
//          if (w) { id.style.width = w + "px"; }
//          if (h) { id.style.height = h + "px"; }
//canvas:
//          if (w) { id.width = w; }
//          if (h) { id.height = h; }
        }
        if (t === "button" ||
            t === "label") {
//DEV all 0...
//          let cse = getComputedStyle(id);
//          return ["sequence",$pxFloat(cse.width),$pxFloat(cse.height)];
            let brect = id.getBoundingClientRect();
            return ["sequence",brect.width,brect.height];
        }
    } else if (t === "canvas" ||
               t === "graph" ||
               t === "list") {
        if (name === "DRAWSIZE") {
            return ["sequence",id.width,id.height];
        }
    } else if (t === "dialog") {
        if (name === "SCREENPOSITION") {
            return ["sequence",id.offsetLeft,id.offsetTop];
        }
    }
//  if (!t || !$storeAttr.hasOwnProperty(t)) {
//      crash("invalid type");
//  }
/*
    let val = id[name];
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

//DEV gGetDouble === gGetAttribute...
//function IupGetDouble(id, name, dflt=0) {
////    let t = id?id.classList[0]:"NULL";
//  let t = id.classList[0];
////DEV we're not allowing this anymore...
////    if (id === NULL) {
////        if (name === "SCREENDPI") {
////            return window.devicePixelRatio * 96;
////        }
////    } else 
////    if (t === "canvas") {
////        if (name === "XTICK" ||
//////          name === "DATA" ||
//////          name === "DRID" ||
////            name === "XANGLE" ||
//////          name === "XCROSSORIGIN" ||
////            name === "XMARGIN" ||
////            name === "XMAX" ||
////            name === "XMIN" ||
////            name === "XYSHIFT" ||
////            name === "YANGLE" ||
//////          name === "YCROSSORIGIN" ||
////            name === "YMARGIN" ||
////            name === "YMAX" ||
////            name === "YMIN" ||
////            name === "YTICK" ||
////            name === "YXSHIFT") {
////            return id[name] || dflt;
////        }
////    } else 
//  if (t === "slider") {
//      if (name === "VALUE") {
//          return Number(id.value);
//      }
//  }
//  crash("IupGetDouble(%s,%s) not supported",["sequence",t,name]);
//}
//
////DEV gSetDouble = gSetAttribute;
//function IupSetDouble(id, name, v) {
////    let t = id?id.classList[0]:"NULL";
//  let t = id.classList[0];
////    if (t === "slider") {
//  if (t === "progress") {
//      if (name === "VALUE") {
//          id.value = v;
//          return;
//      }
////    } else if (t === "canvas") {
////        if (name === "XTICK" ||
////            name === "YTICK") {
////            id[name] = v;
////            return;
////        }
//  }
//  crash("IupSetDouble(%s,%s,%V) not supported",["sequence",t,name,v]);
//}

// translated from iup_attrib.c:
function gSetAttributes(id, attributes, args = []) {
    if (attributes && typeof(attributes) === "string" && attributes.length>0) {
        if (equal($subse(attributes,1),0X3D)) { // '='
            if (attributes==="==") {
                for (let i=1, i$lim=length(args); i<=i$lim; i+=1) {
                    let [,/*string*/ n,/*object*/ v] = $subse(args,i);
                    gSetAttribute(id,n,v);
                }
                return;
            }
            let /*sequence*/ names = split($subss(attributes,2,-1),0X2C);
            assert(equal(length(names),length(args)),"names/args must be same length");
            for (let i=1, i$lim=length(names); i<=i$lim; i+=1) {
                gSetAttribute(id,$subse(names,i),$subse(args,i));
            }
            return;
        }
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
            attributes = sprintf(attributes,args);
        }

//      for (;;) {
        while (true) {
            switch (iAttribToken()) {
            case IUPLEX_TK_END:         // same as IUPLEX_TK_COMMA
                bEnd = 1;
            case IUPLEX_TK_COMMA:
                if (name) {
                    gSetAttribute(id, name, val);
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

//      function IupSetAttributeHandle(/*gdx*/ id, /*string*/ name, /*gdx*/ id_named) {
//          // NB currently only handles dialog/MENU..
//          if (name === "IMAGE") {
//      //      inputDataUrl.value = dataUrl;
//      //      imagePreview.src = dataUrl;
//      //      id.value = id_named;
//      //      id.src = id_named;
//      //      imagePreview.style.display = "block";
//      //      id.style.display = "block";
//      //  imagePreview.style.maxWidth = `${inputWidth.value}px`;
//      //      id.style.maxWidth = `32px`;
//              let img = new Image();
//              img.src = id_named;
//              id.appendChild(img);
//              return;
//          }
//          let t = id?id.classList[0]:"NULL",
//              n = id_named?id_named.classList[0]:"NULL";
//          if (t === "dialog" && name === "MENU" && n === "submenu") {
//              // id_named is a ul.submenu...
//              const mh = document.createElement("div"),
//                    mb = document.createElement("div"),
//                   nav = document.createElement("nav");
//      //<div id="mobbtn">&#9776;</div>
//      //<nav id="menu"><ul class="menubar">
//      //      mh.setAttribute("class", "menuheader");
//              mh.className = "menuheader";
//      //      mh.id = "menuheader";
//              mb.id = "mobbtn";
//              mb.innerHTML = "&#9776;";
//              nav.id = "menu";
//              nav.appendChild(id_named);
//              mh.appendChild(mb);
//              mh.appendChild(nav);
//              if (id_named.className !== "submenu") { crash("uh?"); }
//      //      id_named.setAttribute("class", "menubar");
//              id_named.className = "menubar";
//              let l = id_named.children.length;
//              for (let i = 0; i < l; i += 1) {
//                  let ci = id_named.children[i],
//                      cn = ci.className;
//                  // quick sanity check:
//                  assert(cn === "nestmenu" || cn === "menuitem" );
//                  ci.className = "topmenu";
//              }
//              let dlgbod = id.querySelector(".dialog-body");
//              dlgbod.insertAdjacentElement("afterbegin", mh);
//              // Open sub-menu on click/hover
//              nav.mstack = [];
//              function close_to(mitem,clicked) {
//                  //
//                  // For an explanation, run demo/pGUI/submenu.exw
//                  // (on the desktop or in the browser!) and open
//                  // Edit/Create/Triangle. They should be "sticky"
//                  // and remain as you move the mouse around over 
//                  // the rest of the window/document. Sneak back 
//                  // onto File or Edit/Copy and several menus are 
//                  // hidden in one go, and one new menu opened.
//                  //
//                  // Called by click(topmenu),
//                  //           enter(topmenu/nestmenu/menuitem),
//                  //           click(doc).
//                  //
//                  let mstack = nav.mstack,
//                      wasmlen = mstack.length,
//                      p = mitem?.parentNode.parentNode;
//                  while (mstack.length &&
//                         mstack[mstack.length-1]!==p) {
//                      mstack.pop().classList.remove("open");
//                  }
//                  if (mitem!==null && (clicked || wasmlen)) {
//                      mitem.classList.toggle("open");
//                      if (mitem.classList.contains("open")) {
//                          mstack.push(mitem);
//                      }
//                  }
//              }
//              function li_click(event,mitem) {
//                  // (m is an element from ml)
//                  //event.preventDefault(); // avoids default right click menu (no help...)
//                  event.stopPropagation(); // important!
//                  close_to(mitem,true);
//              }
//              let ml = id_named.querySelectorAll(".topmenu");
//              for (let i = 0; i < ml.length; i += 1) {
//                  let m = ml[i];
//                  m.addEventListener("click", (event) => li_click(event,m), false);
//                  m.addEventListener("mouseenter", () => { close_to(m,false); }, false);
//              }
//              let nm = id_named.querySelectorAll(".nestmenu, .menuitem");
//              for (let i = 0; i < nm.length; i += 1) {
//                  let m = nm[i];
//                  m.addEventListener("mouseenter", () => { close_to(m,false); }, false);
//              }
//              function hide_menu() {
//                  close_to(null,false);
//                  // alt-keys (mush === Menu Underline SHow, topmenu only)
//                  const mush = id_named.querySelectorAll(".mush");
//                  mush.forEach((item) => { item.classList.remove("mush"); });
//              }
//              document.addEventListener("click", hide_menu);
//              document.addEventListener("contextmenu", hide_menu);
//
//      //      let menu = id_named.getElementById("menu");
//      //      menu.addEventListener("contextmenu", (event)=> {event.preventDefault(); event.stopPropagation(); return false; });
//              nav.addEventListener("contextmenu", (event)=> {event.preventDefault(); event.stopPropagation(); return false; });
//
//              function window_resize() {
//                  hide_menu();
//                  //DEV/SUG calc the 640 from sum of toplevel sizes...
//                  if (window.innerWidth < 140) {
//                      nav.classList.add("mobile");
//                  } else {
//                      nav.classList.remove("mobile");
//                  }
//              }   
//              window_resize();
//              id.addEventListener("resize", window_resize);
//
//      //      let me = id_named.querySelectorAll(".topmenu > span");
//
//              function listenKeys(event) {
//                  var key = event.keyCode;
//                  switch (key) {
//      //            case 16: // Shift
//      //            case 17: // Ctrl
//                    case 18: // Alt (with shift) or AltGr
//                              const malt = document.querySelectorAll(".malt");
//                              // (mush === Menu Underline SHow, topmenu only)
//                              malt.forEach((item) => { item.classList.toggle("mush"); });
//      //                      me[0].focus();
//                              id_named.querySelector(".topmenu > span").focus();
//                              event.preventDefault();
//                              break;
//                    case 27: // Escape
//                              hide_menu();
//                              break;
//      //            case 37: // left
//      //            case 38: // up
//      //            case 39: // right
//      //            case 40: // down
//                  }
//      //console.log(key);
//      //http://keycode.info/
//              }
//      //window.onkeydown = listenKeys;
//              id.addEventListener("keydown", listenKeys)
//
//              // TOGGLE SLIDE MOBILE MENU
//      //      let mobbtn = id_named.querySelector("#mobbtn");
//      //      ==> mb
//              function mobbtn_click() {
//                  let mobile = id.querySelector(".mobile");
//                  if (mb.classList.contains("active")) {
//                      mb.classList.remove("active");
//                      mb.innerHTML = "&#9776;";
//                      if (mobile) {
//                          mobile.style.right = "-220px";
//                      }
//                      mb.style.right = "0";
//                  } else {
//                      mb.classList.add("active");
//                      mb.innerHTML = "&#9587;";
//                      if (mobile) {
//                          mobile.style.right = "0";
//                      }
//                      mb.style.right = "220px";
//                  } 
//              }
//              mb.addEventListener("click", mobbtn_click);
//
//      //DEV is this not the same as/should be in doc_click??
//      //      let content = document.querySelector(".content");
//      //$(".content").on("click", function() { 
//              function content_click() {
//                  if (mb.classList.contains("active")) {
//                      mb.classList.remove("active");
//                      mb.innerHTML = "&#9776;";
//                      let mobile = document.querySelector(".mobile");
//                      if (mobile) {
//                          mobile.style.right = "-220px";
//                      }
//                      mb.style.right = "0";
//                  } 
//              }
//      //      content.addEventListener("click", content_click);
//              window.addEventListener("click", content_click);
//      //      let h = $eHeight(id);
//              let h = $pxFloat(id.style.height);
//              id.style.height = (h+21) + "px";
//          } else if (t === "NULL" && name === "PARENTDIALOG") {
//              // (not something we have to worry about in a browser...)
//          } else {
//              puts(1,"IupSetAttributeHandle(" + t +",\"" + name + "\"," + n +")??\n");
//          }
//      }

//function iup_XkeyBase(c) { return c; }

//function iup_isdouble(/*atom*/ pStatus) {
//// double click
////    return peek(pStatus+5)='D'
//  puts(1,"iup_isdouble\n");
//  return false;
//}

//function iup_isprint(/*atom*/ c) {
//  return c>31 && c<127;   // K_SP..K_tidle
//}

//function iupKeyCodeToName(/*atom*/ ch) {
////    atom pKeyName = c_func(xiupKeyCodeToName,{ch})
////    return peek_string(pKeyName)
//  if (ch === K_F5) {
//      return "F5";
//  }
//  crash("iupKeyCodeToName(" + ch + ")\n");
//}

function gSetHandler(id, name, func) {
    if (sequence(id)) {
        let l = length(id);
        for (let i=1; i<=l; i += 1) {
            let /*gdx*/ idi = id[i];
            gSetHandler(idi,name,func);
        }
    } else if (!string(name)) {
//      assert(handler==NULL)
//      assert(even(length(name)))
        let l = length(name);
        for (let i=1; i<=l; i += 2) {
            let /*string*/ ni = name[i];
            let /*integer*/ f = name[i+1];
            gSetHandler(id,ni,f);
        }
    } else {
        let cn = id.classList[0];
        id[name] = func;
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
            } else if (key === "Backspace") {
                key = VK_BS;
            } else if (key === "Delete") {
                key = VK_DEL;
            } else if (key === "Enter") {
                key = VK_CR;
            } else if (key === "Shift") {
                return true;
            } else if (key === "F1") {
                key = VK_F1;
            } else if (key === "F2") {
                key = VK_F2;
            } else if (key === "F3") {
                key = VK_F3;
            } else if (key === "F4") {
                key = VK_F4;
            } else if (key === "F5") {
                key = VK_F5;
            } else if (key === "F6") {
                key = VK_F6;
            } else if (key === "F7") {
                key = VK_F7;
            } else if (key === "F8") {
                key = VK_F8;
            } else if (key === "F9") {
                key = VK_F9;
            } else if (key === "F10") {
                key = VK_F10;
            } else if (key === "F11") {
                key = VK_F11;
            } else if (key === "F12") {
                key = VK_F12;
            } else if (key === "PageDown") {
                key = VK_PGDN;
            } else if (key === "PageUp") {
                key = VK_PGUP;
            } else if (key === "ArrowUp") {
                key = VK_UP;
            } else if (key === "ArrowDown") {
                key = VK_DOWN;
            } else if (key === "ArrowLeft") {
                key = VK_LEFT;
            } else if (key === "ArrowRight") {
                key = VK_RIGHT;
            } else {    
                console.log("key is " + key);
            }
            return key;
        }
//      if (name==="KEY_CB" || name==="K_ANY") {
        if (name === "KEY") {
            function key_cb(event) {
//puts(1,"key_cb("+event.keyCode+")\n");
                let tgt = event.currentTarget,
                    key = mapkey(event);
//19/10/21:
//              let res = func(tgt,key);
                let res = func(id,key);
                // XPG_DEFAULT and XPG_IGNORE 
                // should prevent propagation,
                // am not going to bother with
                // XPG_CLOSE handling here:
//              if (res !== XPG_DEFAULT &&
//                  res !== XPG_CONTINUE) {
                if (res === XPG_IGNORE ||
                   (res === XPG_DEFAULT && key !== VK_ESC && key !== VK_F5)) {
                    event.preventDefault();
                }
//      if res=XPG_IGNORE
//      or (res=XPG_DEFAULT and not find(key,{VK_ESC,VK_F5})) then

                return (res === XPG_CONTINUE);
            }
//          id.onkeydown = key_cb;
//          id.addEventListener("keydown",key_cb);
//          document.addEventListener("keydown",key_cb);
            $docBody.addEventListener("keydown",key_cb);
//          id.tabIndex="-1";   // important!
        } else if (name === "MOTION_CB") {
            function motion_cb(event) {
//puts(1,"motion_cb("+event.clientX+","+event.clientY+")\n");
                let ctrl = event.currentTarget,
                    parent = gGetDialog(ctrl),
                    x = event.clientX - ctrl.offsetLeft - parent.offsetLeft,
                    y = event.clientY - ctrl.offsetTop - parent.offsetTop;
                func(ctrl,x,y,NULL);
            }
            id.addEventListener("mousemove",motion_cb);
        } else if (name === "BUTTON_CB") {
//      } else if (name === "CLICK") {
            function button_cb(event) {
                const buttons = [IUP_BUTTON1,IUP_BUTTON2,IUP_BUTTON3];
                let code = buttons[event.button],
                    ctrl = event.currentTarget,
                    parent = gGetDialog(ctrl),
                    x = event.clientX - ctrl.offsetLeft - parent.offsetLeft,
                    y = event.clientY - ctrl.offsetTop - parent.offsetTop,
//et = event.type,
//                  pressed = 0; // (not pressed)
                    pressed = (event.type === "mousedown");
//DEV (sip)
//                  click = ctrl.CLICK;
//              if (CLICK) {
//                  status = repeat(false,5);
//                  status[1] = "LMRXY"[?event.button];
//                  status[2] = "SDR"[?pressed];
//                  status[3] = ctrl?
//                  status[4] = shift?
//                  status[5] = alt?
//                  click(ctrl,status[,x,y]);
//              }
//with this still here:
//          id.ACTION = func;

                func(ctrl,code,pressed,x,y,NULL);
            }
//          id.addEventListener("click",button_cb);
            id.addEventListener("mousedown",button_cb);
            id.addEventListener("mouseup",button_cb);
//          id.addEventListener("contextmenu",button_cb);
        } else if (name === "VALUECHANGED") {
            function change_cb(event) {
                func(event.currentTarget);
            }
            if (cn === "datepick" ||
                cn === "list" ) {
                id.addEventListener("change",change_cb);
            } else if (cn === "slider") {
                id.addEventListener("input",change_cb);
            } else {
                id.addEventListener("keyup",change_cb);
            }
        } else if (name === "VALUE_CHANGED") {
            function check_changed_cb(event) {
                let tgl = event.currentTarget,
                    id = tgl.parentNode,
                    val = tgl.checked;
                if (val && id.RGX) {
                    let ids = id.RGX.RADIO_GROUP,
                        idl = length(ids);
                    for (let i = 1; i <= idl; i += 1) {
                        let ri = ids[i],
                            cb = ri.children[1];
                        if (cb.checked && ri!=id) {
                            cb.checked = false;
                        }
                    }
                }
                func(id,val);
            }
            if (cn === "toggle") {
                let cb = id.children[1];
                cb.addEventListener("change",check_changed_cb);
            } else if (cn === "spin") {
                id.VALUE_CHANGED = func;
            }
        } else if (name === "CHANGED") {
            function change_cb(event) {
                func(event.currentTarget);
            }
            if (cn === "drop") {
                id.addEventListener("change",change_cb);
            }
        } else if (name === "CLICK") {
            if (cn === "button" ||
//DEV menuitems should now have "HANDLER"... (is "menuitem" still in use at all??)
                cn === "menuitem") {
                // action with just an id parameter
                function click(event) {
                    let id = event.currentTarget,
                        res = func(id);
                    if (res === XPG_CLOSE) {
                        let dialog = gGetDialog(id);
                        gHide(dialog);
                    }
                }
//              id.addEventListener("click", click);
                id.onclick = click;
            } else if (cn === "text") {
//DEV...
                // action with id, c, pNewValue parameters [not that we could ever actually provide the latter??
                //                                          - maybe change pGUI.e/docs to provide it as a string?
                //                                          - can we even can get a "cancellable-post-key" here?]
                function action_cb(event) {
                    let id = event.currentTarget,
//                       c = event.keyCode,
//                       c = event.key.charCodeAt(0),
                         c = mapkey(event),
//                     res = func(id);
                       res = func(id,c);
//                     res = func(id,c,pNewValue);
                    if (res === XPG_CLOSE) {
                        let dialog = gGetDialog(id);
                        gHide(dialog);
                    } else if (res == XPG_IGNORE) {
                        event.preventDefault();
                        event.stopPropagation();
                    } else if (res && res !== XPG_DEFAULT && res !== c) {
//                      event.keyCode = res; // not alowed
//                      event.charCode = res;
                        //DEV no idea what to do here...
                        printf(1,"text/action_cb(%d) ==> %d???\n",["sequence",c,res]);
                    }
                }
//              id.addEventListener("click", action_cb);
                id.addEventListener("keydown", action_cb);
//              id.onclick = action_cb;
            } else {
                puts(1,"gSetHandler(" + cn + "," + name + ")??\n"); // placeholder
            }
        } else if (name === "REDRAW" && (cn === "canvas" || cn === "graph" || cn === "list")) {
            id.REDRAW = func;
        } else if ((name === "DRID" || name === "XRID" || name === "YRID") && cn === "graph") {
            id[name] = func;
        } else if ((name === "DATA") && cn === "list") {
            id[name] = func;
        } else //if (//name!=="MAP_CB" &&
//                 name!=="UNMAP_CB" &&
//                 name!=="ACTION" &&
//                 name!=="RESIZE_CB") 
                                       {
            puts(1,"gSetHandler(" + cn + "," + name + ")??\n"); // placeholder
        }
    }
}

//function gSetHandlers(/*gdx*/ id, /*sequence*/ namefuncpairs) {
//  let l = length(namefuncpairs);
//  assert(even(l));
//  for (let i = 1; i <= l; i+=2) {
////        gSetHandler(id,$subse(namefuncpairs,i),$subse(namefuncpairs,i+1));
//      gSetHandler(id,namefuncpairs[i],namefuncpairs[i+1]);
//  }
//}
const gSetHandlers = gSetHandler;

//DEV gGetHandler, gGetInheritedHandler
function gGetHandler(/*gdx*/ id, /*string*/ name, /*integer*/ dflt=0) {
    return id[name] || dflt;
}

function $paranormalise(action, func, attributes, args) {
    crash("wrong un");
}

// gdx replaced with integer, all assert() and rtn() checks deleted:

/*include C:\Program Files (x86)\Phix\builtins\ptypes.e*/
///*global*/ function atom_string(/*object*/ o) {
//  return (string(o) || (integer(o) && compare(o,NULL)>=0)) || ((atom(o) && compare(o,NULL)>=0) && (equal(o,floor(o))));
//}
//
///*global*/ function rid_string(/*object*/ o) {
//  return string(o) || (integer(o) && compare(o,15)>0);
//}
//
/*global*/ function nullable_string(/*object*/ o) {
    return string(o) || (equal(o,NULL));
}
///*global*/ function boolean(/*object*/ o) {
//  return integer(o) && ((equal(o,true)) || (equal(o,false)));
//}
/*local*/ function dword_seq(/*object*/ s) { // (technically qword_seq on 64-bit)
    return sequence(s) && !string(s);
}

/*local*/ function $paranormalise_traa(/*object*/ title, click, /*sequence*/ attributes, /*dword_seq*/ args) {
// used by gButton([nullable_string title=NULL,] [rtn click=NULL,] string attributes="", sequence args={})
// and gLink(), ??gMenuItem??(), gToggle(), and gValuator(). (See the docs for the full details)
    if (!nullable_string(title)) {
        // assume title omitted (and at least one other parameter passed)
        if (atom(title)) {  // (and, of course, title is not NULL here)
            // assume gButton(click,[attributes[,args]])                -- (intended)
            //                ^title ^click      ^attributes            -- (actual)
            if (length(attributes)) {  // (args really?)
                args = attributes;                                      // (verified dword_seq now)
                attributes = click;                                     // (verified string below)
            } else if (string(click)) {
                attributes = click;                                     // (verified string below)
            }
            click = title;                                              // (verified rtn below)
            title = NULL;
        } else {
            // assume gButton(attributes[,args])                        -- (intended)
            //                ^title      ^click                        -- (actual)
            if (sequence(click)) {  // (args really?)
                args = click;                                           // (verified dword_seq now)
            }
            attributes = title;                                         // (verified string below)
            click = NULL;
            title = NULL;
        }
    } else if (string(click)) {
        // assume gButton(title,attributes[,args]))                     -- (intended)
        //                      ^click      ^attributes                 -- (actual)
        if (!string(attributes)) {     // (args really?)
            args = attributes;                                          // (verified dword_seq now)
        }
        attributes = click;                                             // (verified string below)
        click = NULL;
    } else if (sequence(click)) { // (and not string)
        // assume gButton(attributes,args)                              -- (intended)
        //                ^title     ^click                             -- (actual)
        attributes = title;                                             // (verified string below)
        args = click;                                                   // (verified dword_seq now)
        title = NULL;
        click = NULL;
//  else assume 3 or 4 parameters were passed (title,click,attributes[,args])
    }
    if (equal(click,NULL) && 
        equal(attributes,"") && 
        string(title) && 
        find(0X3D,title)) {     // 0x3D === '='
        attributes = title;
        title = "";
    }
    return ["sequence",title,click,attributes,args];
}

function $paranormalise_qraa(/*object*/ q, rid, /*sequence*/ attributes, /*dword_seq*/ args) {
// used by gTreeView([sequence q={},] [rtn rid=NULL,] string attributes="", sequence args={})
// (where q is tree_nodes and rid is branchopen)  (See the docs for the full details)
    if (!dword_seq(q)) {
        // assume q omitted (and at least one other parameter passed)
        if (atom(q)) {  // assume q is really rid
            // assume gTreeView(rid,[attributes[,args]])                -- (intended)
            //                  ^q   ^rid        ^attributes            -- (actual)
            if (length(attributes)) {  // (args really?)
                args = attributes;                                      // (verified qword_seq now)
                attributes = rid;                                       // (verified string below)
            } else if (string(rid)) { // (attributes really?)
                attributes = rid;                                       // (verified string below)
            }
            rid = q;                                                    // (verified rtn below)
            q = ["sequence"];
        } else {
            // assume gTreeView(attributes[,args])                      -- (intended)
            //                  ^q         ^rid                         -- (actual)
            if (sequence(rid)) {  // (args really?)
                args = rid;                                             // (verified dword_seq now)
            }
            attributes = q;                                             // (verified string below)
            rid = NULL;
            q = ["sequence"];
        }
    } else if (string(rid)) {
        // assume gTreeView(q,attributes[,args]))                       -- (intended)
        //                    ^rid        ^attributes                   -- (actual)
        if (!string(attributes)) {     // (args really?)
            args = attributes;                                          // (verified dword_seq now)
        }
        attributes = rid;                                               // (verified string below)
        rid = NULL;
    } else if (sequence(rid)) { // (and not string)
        // assume gTreeView(attributes,args)                            -- (intended)
        //                  ^ in q     ^ in rid                         -- (actual)
        attributes = q;                                                 // (verified string below)
        args = rid;                                                     // (verified dword_seq now)
        q = ["sequence"];
        rid = NULL;
//  else assume 3 or 4 parameters were passed (q,rid,attributes[,args])
    }
    return ["sequence",q,rid,attributes,args];
}

function $paranormalise_raa(/*object*/ rid, /*sequence*/ attributes, /*dword_seq*/ args) {
// used by gText([rtn rid=NULL,] string attributes="", sequence args={}) and gCanvas(), gDatePick(), 
//  gDetachBox(), gList(), gTable(), and gSpinBox(). (See the docs for the full details)
    if (dword_seq(attributes)) {
        // assume gText(attributes,args)                                    -- (intended)
        //              ^ in rid   ^ in attributes                          -- (actual)
        args = attributes;                                                  // (verified dword_seq now)
        attributes = rid;                                                   // (verified string below)
        rid = NULL;
    } else if (string(rid)) {
        // assume gText(attributes)                                         -- (intended)
        //              ^ in rid                                            -- (actual)
        attributes = rid;                                                   // (verified string below)      
        rid = NULL;
    }
    return ["sequence",rid,attributes,args];
}

function $paranormalise_taa(/*nullable_string*/ title, /*sequence*/ attributes, /*dword_seq*/ args) {
// used by gLabel([nullable_string title=NULL,] string attributes="", sequence args={}) (only)
    if (dword_seq(attributes)) {
        // assume gLable(attributes,args)                                   -- (intended)
        //               ^ title    ^ attributes                            -- (actual)
        args = attributes;                                                  // (verified dword_seq now)
        attributes = title;                                                 // (verified string below)
        title = NULL;
    } else if (string(title) && find(0X3D,title)) {     // 0x3D === '='
        // assume gText(attributes)                                         -- (intended)
        //              ^ title                                             -- (actual)
        attributes = title;                                                 // (verified string below)      
        title = NULL;
    }
    return ["sequence",title,attributes,args];
}

function $paranormalise_ptaab(/*object*/ parent, title, attr, args, /*bool*/ bEsc) {
// used by gDialog(gdx child, [parent=NULL,] [string [title="",] attr=""[, sequence args={}]], bool bEsc=true)
    if (!integer(parent)) {
        // assume parent omitted, and at least one other parameter passed,
        // and in fact the first **must** then be the (string) attr/title.
        if (find(0X3D,parent)) {    // assume parent is attr (and not title), 0x3D === '='
            if (bool(title)) {
                // assume gDialog(child,attr,bEsc)                  -- (intended)
                //                parent^    ^title                 -- (actual)
                bEsc = title;
            } else {
                // assume gDialog(child,attr,args[,bEsc])           -- (intended)
                //                parent^  title^  ^attr            -- (actual)
                if (bool(attr)) {
                    bEsc = attr;                                    // (verified bool now)
                }
                if (dword_seq(title)) {
                    args = title;                                   // (verified dword_seq below)
                }
            }
            title = "";
            attr = parent;                                          // (verified string below)
        } else { // assume parent is really title
            if (string(title)) {    // attr
                if (dword_seq(attr)) {
                    if (bool(args)) {
                        bEsc = args;
                    }
                    args = attr;
                } else if (bool(attr)) {
                    bEsc = attr;
                }
                attr = title;
            } else {
                bEsc = title;
            }
            title = parent;
        }
        parent = NULL;
    } else if (bool(title)) {
        // assume gDialog(child,parent,bEsc):                   -- (intended)
        //                             ^title                   -- (actual)
        bEsc = title;                                           // (verified bool now)
        title = "";
    } else if (find(0X3D,title)) {      // 0x3D === '='
        // assume gDialog(child,parent,attr[,args][,bEsc]):     -- (intended)
        //                        title^     ^attr  ^args       -- (actual)
        if (bool(attr)) {
            bEsc = attr;
        } else if (dword_seq(attr)) {
            if (bool(args)) {
                bEsc = args;
            }
            args = attr;
        }
        attr = title;
        title = "";
    } else if (bool(attr)) {
        // assume gDialog(child,parent,title,bEsc):     -- (intended)
        //                                   ^attr      -- (actual)
        bEsc = attr;                                            // (verified bool now)
        attr = "";
    } else if (bool(args)) {
        // assume gDialog(child,parent,title,attr,bEsc):        -- (intended)
        //                                        ^args         -- (actual)
        bEsc = args;                                            // (verified bool now)
        args = ["sequence"];
    }
    return ["sequence",parent,title,attr,args,bEsc];
}

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

let $next_id = 1;

function $ocument_createElement(tag) {
    const id = document.createElement(tag);
    id.ID = $next_id++;
    return id;
}

function gClipboard() {
//  return "clipboard";
//string text = gGetAttribute(clipboard,"TEXT")
    if (!$storeAttr) { $gInit(); }
//  const id = document.createElement("textarea");
    const id = $ocument_createElement("textarea");
    id.setAttribute("class", "clipboard");
    id.setAttribute("readonly", "");
//  id.style = { position: "absolute", left: "-9999px" };
//              this.$clipboard.setAttribute("style", "display:none;position:fixed;left:-99em;top:-99em;");
    id.setAttribute("style", "display:none;position:fixed;left:-99em;top:-99em;");
    return id;
}

//DEV we probably still need this for gSetAttribute(dlg,"CLOSE_ON_ESCAPE")...
function $xpg_CloseOnEscape(/* gdx */ dlg) {
    function hide_dlg(event) {
//      let keyCode = window.event ? window.event.keyCode : e.which;
//      if (keyCode === 27) {
        if (event.key === "Escape") {
//      if (event.key === "Escape" &&
//          event.target === dlg) {
//          event.preventDefault();
//          event.stopPropagation(); // important!
//          event.cancelBubble = true;
//DEV wrong one!!
            gHide(dlg);
        }
    }
//  document.onkeydown = hide_dlg;
//  dlg.onkeydown = hide_dlg;
//  dlg.setAttribute("tabindex", 0);
    dlg.setAttribute("tabindex",-1);
    dlg.addEventListener("keydown",hide_dlg);
//  document.addEventListener("keydown",hide_dlg);
//  $docBody.addEventListener("keydown",hide_dlg);
//  dlg.tabIndex="-1";
}

// from https://stackoverflow.com/questions/48044951/canvas-state-lost-after-changing-size
/*
function save(ctx){
    let props = ['strokeStyle', 'fillStyle', 'globalAlpha', 'lineWidth', 
    'lineCap', 'lineJoin', 'miterLimit', 'lineDashOffset', 'shadowOffsetX',
    'shadowOffsetY', 'shadowBlur', 'shadowColor', 'globalCompositeOperation', 
    'font', 'textAlign', 'textBaseline', 'direction', 'imageSmoothingEnabled'];
    let state = {}
    for(let prop of props){
      state[prop] = ctx[prop];
    }
    return state;
}

function restore(ctx, state){
    for(let prop in state){
      ctx[prop] = state[prop];
    }
}

//function resize(ctx, width, height){
//  let state = save(ctx);
//  ctx.canvas.width = width || canvas.width;
//  ctx.canvas.height = height || canvas.height;
//  restore(ctx, state);
//}
*/
/*
let canvas = document.querySelector('canvas')
  , ctx = canvas.getContext('2d')
  , stack = []

function save(ctx){
  let state = {}
  for(let property in ctx){
    if(property == 'canvas')
      continue
    if(typeof ctx[property] == 'function')
      continue
    state[property] = ctx[property]
  }
  stack.push(state)
}

function restore(ctx){
  let state = stack.pop() || {}
  for(let property in state){
    ctx[property] = state[property]
  }
}

//function resize(ctx, width, height){
//  save(ctx)
//  ctx.canvas.width = width || canvas.width;
//  ctx.canvas.height = height || canvas.height;
//  restore(ctx)
//}


//////////////    EXAMPLE    ////////////////


let index = 0
  , words = ["Our", "Words", "Are", "Dynamic"];

(function change(){
  let font_size = ~~(Math.random() * 150 + 16)
  let word = words[index]
  index = (index + 1) % words.length
  
  ctx.font = font_size+"px serif"
  ctx.textBaseline = "hanging"
  ctx.fillStyle = "white"
  resize(ctx, ctx.measureText(word).width, font_size)
  ctx.fillText(word, 0, 0)
  
  setTimeout(change, 750)
})()
*/

function $set_canvas_size(id,w,h) {
    let ctx = id.getContext("2d") || id.getContext("webgl"),
        // save, since they get trashed(!!):
        fs = ctx.fillStyle,
        ss = ctx.strokeStyle,
        ta = ctx.textAlign,
        tb = ctx.textBaseline,
        cf = ctx.font;
    
//console.log("canvas.width: "+id.width+" => "+w);
//console.log("canvas.height: "+id.height+" => "+h);
    id.width = w;
    id.height = h;
//no diff:
//ctx.canvas.width  = w;
//ctx.canvas.height = h;
    ctx.fillStyle = fs;
    ctx.strokeStyle = ss;
    ctx.textAlign = ta;
    ctx.textBaseline = tb;
    ctx.font = cf;
}

const use_css = true;
//9/11/21 pretty sure I should be using css to resize...  15/12/23: rubbish on a canvas!
//21/8/21:
function $resize_children(id, w, h) {
//  if (use_css) { return; }
//function $resize_children(id, w, h, nest=0) {
    // recursively, honouring EXPAND (etc) and invoking any RESIZE_CB/REDRAW_CB
//  let cn = id.classList[0];
    let cn = id.classList[0] || id.localName;
    if (cn !== "dialog-header" &&   // (there can be no ["ACTION"] attached
        cn !== "dialog-resizers") { //  to either, so simplify debugging..)
//if (nest===0) {
//printf(1,"resize_children(%s,%d,%d,%d)\n",["sequence",cn,w,h,nest]);
//}

        let bSizeChanged = false,
             bDoChildren = false,
//                prev_w = id.width,
//                prev_h = id.height;
                  prev_w,
                  prev_h;

        if (cn === "canvas" ||
            cn === "drag-x" ||
            cn === "drag-y" ||
            cn === "graph" ||
//DEV no help...
//          cn === "table" ||
            cn === "list") {
//          if (id.width !== w || id.height !== h) {
//SUG/ from StackOverflow:
//let cheight = parseInt(canvas.getAttribute("height"));
//let cwidth = parseInt(canvas.getAttribute("width"));
            prev_w = id.width;
            prev_h = id.height;
            if (prev_w !== w || prev_h !== h) {
                $set_canvas_size(id,w,h);
//              if (l) { crash("uh?"); }
                bSizeChanged = true;
            }
        } else if (cn === "split") {
            let [c1,splitter,c2] = id.childNodes;
//DEV this currently goes a bit mental if you resize below the minsize... (Shirley, this should not be invoked with nonsense w/h)
//DEV not entirely sure if this is where we should be applying an id.FRAC!=-1, but it probably is...
            if (id.ORIENTATION === "VERTICAL") {
                let h1 = c1.height,
                    h2 = c2.height;
                if (!h1) { h1 = $eHeight(c1); }
                if (!h2) { h2 = $eHeight(c2); }
                h1 = round((h1/(h1+h2))*(h-3));
                h2 = h-3-h1;
                $resize_children(c1,w,h1)
                $resize_children(splitter,w,3)
                $resize_children(c2,w,h2)
            } else {
                let w1 = c1.width,
                    w2 = c2.width;
                if (!w1) { w1 = $eWidth(c1); }
                if (!w2) { w2 = $eWidth(c2); }
                w1 = round((w1/(w1+w2))*(w-3));
                w2 = w-3-w1;
                $resize_children(c1,w1,h)
                $resize_children(splitter,3,h)
                $resize_children(c2,w2,h)
            }
        } else if (cn === "container" ||
                   cn === "table") {
//          if (id.width !== w || id.height !== h) {
//          if (prev_w !== w || prev_h !== h) {
//              id.width = w;
//              id.height = h;
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
//          if (id.width !== w || id.height !== h) {
            if (prev_w !== w || prev_h !== h) {
                id.width = w;
                id.height = h;
                bSizeChanged = true;    
//              bDoChildren = true;
            }
*/
//      } else {
//      } else if (cn === "menuheader") {
//          h -= 21;
//6/5/22:
//      } else {
        } else if (!use_css) {
            prev_w = id.offsetWidth,
//          prev_w = id.offsetWidth-($pxFloat(id.style.marginLeft)+$pxFloat(id.style.marginRight)),
//id.style.marginBottom
//marginBottom: "5px"
//marginLeft: "10px"
//marginRight: "10px"
//marginTop: "5px
//          prev_w = id.offsetWidth-id.offsetLeft,
//          prev_w = id.clientWidth,
            prev_h = id.offsetHeight;
//          prev_h = id.offsetHeight-($pxFloat(id.style.marginTop)+$pxFloat(id.style.marginBottom));
//          prev_h = id.offsetHeight-id.offsetTop;
//          prev_h = id.clientHeight;
//          w -= id.offsetLeft;
            let nw = w-id.offsetLeft,
                nh = h-(id.offsetTop-45);
//          h += id.offsetTop;
//          h += id.offsetTop-45;   // well, it appears to work 21/10/21!! (on drag, that is)
//          if (prev_w !== w || prev_h !== h) {
            if (prev_w !== nw || prev_h !== nh) {
//              id.offsetWidth = w;
//              id.clientWidth = w;
//              id.style.width = w + "px";
//              id.offsetHeight = h;
//              id.clientHeight = h;
//              id.style.height = h + "px";
                bSizeChanged = true;    
//bollocks, see function expand(id,val) {...
                let expand = id.EXPAND;
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
            let resize_cb = id.RESIZE_CB,
//DEV redraw...
//              action_cb = id.ACTION;
                redraw = id.REDRAW;

            if (resize_cb) {
                resize_cb(id,w,h);
            }
//          if (action_cb && cn === "canvas") { 
//              action_cb(id); 
//DEV no help...
            if (redraw && $redraw && (cn === "canvas" || cn === "graph" || cn === "list" || cn === "drag-x" || cn === "drag-y")) { 
//          if (redraw && $redraw && (cn === "canvas" || cn === "graph" || cn === "list" || cn === "table")) { 
                let bg = id.style.backgroundColor;
                if (redraw.length === 1) {
                    redraw(id); 
                } else if (redraw.length === 3) {
                    redraw(id,w,h);
                } else {
                    crash("uh??");
                }
                id.style.backgroundColor = bg; // (restore, as per docs)
            }

//28/12/21:
//          if (bDoChildren) {
            if (bDoChildren || cn === "dialog" || cn === "dialog-body") {
//          if (false) {
                // for eg {button,fill,button,fill,button}, we want {0,1,0,2,0} and v_count of 2.
                // each non-zero gets (2-1+1)/2, (2-2+1)/1 of the remaining prev_w-w, iyswim.
                let w_children = [],    w_count = 0,    // width candidates
                    h_children = [],    h_count = 0,    // height candidates
                      children = id.childNodes,
                  l = children.length;

                for (let i = 0; i < l; i += 1) {
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
//                              cw = ci.offsetWidth-($pxFloat(ci.style.marginLeft)+$pxFloat(ci.style.marginRight)),
//                              ch = ci.height;
                                ch = ci.clientHeight;
//                              ch = ci.offsetHeight-($pxFloat(id.style.marginTop)+$pxFloat(id.style.marginBottom));
//                          if (ci.style.marginLeft   !== "") { cw -= $pxFloat(ci.style.marginLeft); }
//                          if (ci.style.marginRight  !== "") { cw -= $pxFloat(ci.style.marginRight); }
//                          if (ci.style.marginTop    !== "") { ch -= $pxFloat(ci.style.marginTop); }
//                          if (ci.style.marginBottom !== "") { ch -= $pxFloat(ci.style.marginBottom); }
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
            dialog.style.height = (dialog.maximumHeight || window.innerHeight) + "px";
//          dialog.style.height = (dialog.maximumHeight || (window.innerHeight-34)) + "px";
//console.log("dialog.width: "+dialog.waswidth+" => "+dialog.style.width);
//console.log("dialog.height: "+dialog.washeight+" => "+dialog.style.height);
            maxbtn.title = "Restore";
        } else {
            if ($pxFloat(dialog.washeight)<dialog.minimumHeight) { dialog.washeight = dialog.minimumHeight + "px"; }
            if ($pxFloat(dialog.waswidth) <dialog.minimumWidth ) { dialog.waswidth  = dialog.minimumWidth  + "px"; }
//console.log("dialog.width: "+dialog.style.width+" => "+dialog.waswidth);
//console.log("dialog.height: "+dialog.style.height+" => "+dialog.washeight);
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
//(removed 27/5/22...)
//      $resize_children(child,dlgbod.clientWidth-2,dlgbod.clientHeight-2);
//20/1/24:
        $resize_children(child,dlgbod.clientWidth,dlgbod.clientHeight);
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

//function xpg_set_menu(/*gdx*/ id, /*string*/ name, /*gdx*/ id_named) {
function $xpg_set_menu(/*gdx*/ id, /*gdx*/ id_named) {
//  let t = id?id.classList[0]:"NULL",
//      n = id_named?id_named.classList[0]:"NULL";
//  if (t === "dialog" && name === "MENU" && n === "submenu") {
        // id_named is a ul.submenu...
        const mh = document.createElement("div"),
//      const mh = $ocument_createElement("div"),
              mb = document.createElement("div"),
             nav = document.createElement("nav");
        mh.className = "menuheader";
        mb.id = "mobbtn";
        mb.innerHTML = "&#9776;";
        nav.id = "menu";
        nav.appendChild(id_named);
        mh.appendChild(mb);
        mh.appendChild(nav);
        if (id_named.className !== "submenu") { crash("uh?"); }
        id_named.className = "menubar";
        let l = id_named.children.length;
        for (let i = 0; i < l; i += 1) {
            let ci = id_named.children[i],
                cn = ci.className;
            // quick sanity check:
            assert(cn === "nestmenu" || cn === "menuitem" );
            ci.className = "topmenu";
        }
        let dlgbod = id.querySelector(".dialog-body");
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
        let ml = id_named.querySelectorAll(".topmenu");
        for (let i = 0; i < ml.length; i += 1) {
            let m = ml[i];
            m.addEventListener("click", (event) => li_click(event,m), false);
            m.addEventListener("mouseenter", () => { close_to(m,false); }, false);
        }
        let nm = id_named.querySelectorAll(".nestmenu, .menuitem");
        for (let i = 0; i < nm.length; i += 1) {
            let m = nm[i];
            m.addEventListener("mouseenter", () => { close_to(m,false); }, false);
        }
        function hide_menu() {
            close_to(null,false);
            // alt-keys (mush === Menu Underline SHow, topmenu only)
            const mush = id_named.querySelectorAll(".mush");
            mush.forEach((item) => { item.classList.remove("mush"); });
        }
        document.addEventListener("click", hide_menu);
        document.addEventListener("contextmenu", hide_menu);

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
        id.addEventListener("resize", window_resize);

//      let me = id_named.querySelectorAll(".topmenu > span");

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
                        id_named.querySelector(".topmenu > span").focus();
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
        id.addEventListener("keydown", listenKeys)

        // TOGGLE SLIDE MOBILE MENU
//      let mobbtn = id_named.querySelector("#mobbtn");
//      ==> mb
        function mobbtn_click() {
            let mobile = id.querySelector(".mobile");
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
//      let h = $eHeight(id);
        let h = $pxFloat(id.style.height);
        id.style.height = (h+21) + "px";
//  } else if (t === "NULL" && name === "PARENTDIALOG") {
//      // (not something we have to worry about in a browser...)
//  } else {
//      puts(1,"IupSetAttributeHandle(" + t +",\"" + name + "\"," + n +")??\n");
//  }
}

function gDialog(child, parent = NULL, title = "", attributes = "", args = [], bEsc = true) {
// as close as possible to xpGUI.e, see phix.chm
    if (!$storeAttr) { $gInit(); }
    [,parent,title,attributes,args,bEsc] = $paranormalise_ptaab(parent,title,attributes,args,bEsc);

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
//  const dialog = document.createElement("div"),
    const dialog = $ocument_createElement("div"),
          headiv = document.createElement("div"),
          header = document.createElement("div"),
          dlgbod = document.createElement("div"),
          maxbtn = header_button("Maximise", "M10,10 L10,20 L20,20 L20,10 Z"),
          maxsvg = maxbtn.children[0],
          clsbtn = header_button("Close", "M10,10 L20,20 M20,10 L10,20");
    headiv.classList.add("dialog-header");
    header.classList.add("dialog-handle");
//div.setAttribute("class", "note");
//  header.innerHTML = "<b>" + htitle + "</b>";
    header.innerHTML = "<b><i>untitled</i></b>";
    headiv.appendChild(header_button("Application", "M3,15l8,6L26,0L12,27L3,15z", "-12 -8 48 48", "icon"));
//  headiv.appendChild(header_button("Application", "M0,13L10,5L26,0L12,27L0,13z", "-12 -8 48 48", "icon"));
//  headiv.appendChild(header_button("Application", "M 0 13 L 10 5 L 26 0 L 12 27 L 0 13 z", "-12 -8 48 48", "icon"));
//  headiv.appendChild(header_button("Application", "M0,13l6-0c2,1,4,3,6,5C16,11,21,5,26,0h4 "+
//                                   "C23,8.5,17,18,12,27C9,22,5,17,0,13L0,13z", "-12 -8 48 48", "icon"));
//  headiv.appendChild(header_button("Application", "M28 0h-24c-2.2 0-4 1.8-4 4v24c0 2.2 "+
//                                   "1.8 4 4 4h24c2.2 0 4-1.8 4-4v-24c0-2.2-1.8-4-4-4zM14 "+
//                                   "24.828l-7.414-7.414 2.828-2.828 4.586 4.586 9.586-9.586 "+
//                                   "2.828 2.828-12.414 12.414z", "-12 -8 48 48", "icon"));
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
    dialog.setAttribute("EXPAND", "YES");
//26/5 (see if this helps with boids...)
//  dialog.setAttribute("data-sizing","intrinsic");
    dialog.appendChild(headiv);
    dialog.appendChild(dlgbod);
//DEV... (actually this is probably right)
    let menu = child.MENU;
    if (menu) {
        $xpg_set_menu(dialog,menu);
    }       

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

            function clamp(num, low, high) {
                return (num <= low) ? low : ((num >= high) ? high : num);
            }

            function above(num, low) {
                return (num <= low) ? low : num;
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
//  clsbtn.onclick = function hd() { gHide(dialog); }
//  dialog.onmousedown = function md() { $topZindex(dialog); }
    function ds() { return false; }
    function hd() { gHide(dialog); }
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
                if (dialog.querySelector(".menuheader")) { h -= 21; }
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

    if (length(title)) {
        gSetAttribute(dialog,"TITLE",title);
    }
    gSetAttributes(dialog, attributes, args);

    if (bEsc) { $xpg_CloseOnEscape(dialog); }

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
//      id.width = this.innerWidth;
//  c.height = this.innerHeight;
        console.log("dialog_resize?");
    }
*/

    return dialog;
} // (gDialog() ends...)

function gMap(id) {
/*
    let cn = id.className;
    if (cn !== "dialog-header" &&   // (there can be no ["MAP_CB"] attached
        cn !== "dialog-resizers") { //  to either, so simplify debugging..)
        let map_cb = id["MAP_CB"];
//28/12/21: (decided against)
//          resize_cb = id.RESIZE_CB;
        if (map_cb) { 
            map_cb(id); 
            id["MAP_CB"] = null;
        }
//      if (resize_cb) {
//>>
//          resize_cb(id,w,h);
//      }
        let children = id.childNodes,
            l = children.length;
        for (let i = 0; i < l; i += 1) { gMap(children[i]); }
    }
*/
}

function gRedraw(id) {
    if (sequence(id)) {
        let len = length(id);
        for (let i = 1; i <= len; i += 1) {
            gRedraw(id[i]);
        }
    } else {
        let cn = id.className;
        if (cn !== "dialog-header" &&   // (there can be no ["REDRAW"] attached
            cn !== "dialog-resizers") { //  to either, so simplify debugging..)
            let redraw = id["REDRAW"];
            if (redraw && $redraw && (cn === "canvas" || cn === "graph" || cn === "list")) { 
                redraw(id); 
            }
            let children = id.childNodes,
                l = children.length;
            for (let i=0; i<l; i += 1) { gRedraw(children[i]); }
        }
    }
}
//const IupRefresh = gRedraw;
//const IupRefreshChildren = gRedraw;

function gShow(id, x = XPG_CURRENT, y = XPG_CURRENT) {
    // Make it top dog, and add it to the DOM.
    id.style.zIndex = document.getElementsByClassName("dialog").length;
// 25/9/21: (for "save as" handling)
//  $docBody.appendChild(id);
    $docBody.insertAdjacentElement("afterbegin", id);

    function xpg_placement(/*integer*/ xy, os, ol, is, il) {
        // xy is the x or y of gShow(), which can be an absolute value such as 100 (returned as is)
        // or one of the XPG_XXX - which obviously need the outer and inner starts and lengths.
        let /*integer*/ res = is;
        if (xy < XPG_CURRENT) {
            res = xy;
        } else if (xy > XPG_CURRENT) {
            switch (xy & 0b0011) {
                case 0b11: res = os+floor((ol-il)/2);   break; // centre
                case 0b10: res = os+ol-il;              break; // right/btm
                case 0b01: res = os;                    break; // left/top
                case 0b00: res = is-(is+il<=ol?0:il);   break; // mousepos [DEV untested]
            }
        }
        return res;
    }

//global procedure gShow(gdx id, integer x=XPG_CURRENT, y=XPG_CURRENT)
    let pid = gGetParent(id), sx = 0, sy = 0,
        [, sw, sh] = gGetGlobalIntInt("SCREENSIZE")
//  -- catch the handle of the first window displayed (nb arwen saves first created)
//  if PrimaryWindowID=UNDEFINED then
//      assert(pid=0,"first window displayed must be parent-less")
//      PrimaryWindowID = id
//  end if
    const XPG_PARENT = 0x0004;  // 0b0100
    if (pid) {
        let prect = pid.getBoundingClientRect();    // (nb: recalc in DOM)
//      if (and_bits(x,XPG_PARENT)) { sx = prect.offsetLeft; sw = prect.width; }
//      if (and_bits(x,XPG_PARENT)) { sx = prect.left; sw = prect.width; }
        if (x & XPG_PARENT) { sx = prect.left; sw = prect.width; }
//      if (and_bits(y,XPG_PARENT)) { sy = prect.offsetTop; sh = prect.height; }
//      if (and_bits(y,XPG_PARENT)) { sy = prect.top; sh = prect.height; }
        if (y & XPG_PARENT) { sy = prect.top; sh = prect.height; }
    }
    if (!id.PREVIOUSLYSHOWN) {
        if (x === XPG_CURRENT) { x = XPG_CENTER; }
        if (y === XPG_CURRENT) { y = XPG_CENTER; }
        id.PREVIOUSLYSHOWN = true;
    }
//  sequence gwr = xpg_get_window_rect(id)
//  integer {wx,wy,ww,wh} = gwr
    let wx = id.offsetLeft,
        wy = id.offsetTop,
//      ww = id.width,
        ww = id.offsetWidth,
//      wh = id.height;
        wh = id.offsetHeight;
    if ((x === XPG_MOUSEPOS) || (y === XPG_MOUSEPOS)) {
//      assert(x==y,"gShow: XPG_MOUSEPOS is both-only")
//      ?9/0 -- (placeholder)
        crash("placeholder");
//      {wx,wy} = gGetGlobalIntInt("MOUSEPOS") -- (nee CURSORPOS)
    }
    x = xpg_placement(x,sx,sw,wx,ww);
    y = xpg_placement(y,sy,sh,wy,wh);
//--?{"win/rect",{h,gwr},{x},wx,ww,{y},wy,wh}
//  xpg_move_window(id,wx,wy)
//--?"moved"
//--DEV setAttribute(h,"VISIBLE",true)...

//function IupShow(id, x = XPG_CURRENT, y = XPG_CURRENT) {
//  // Make it top dog, and add it to the DOM.
//  id.style.zIndex = document.getElementsByClassName("dialog").length;
//// 25/9/21: (for "save as" handling)
////    $docBody.appendChild(id);
//  $docBody.insertAdjacentElement("afterbegin", id);
////    gMap(id);
////$resize_children(id,id.clientWidth-2,id.clientHeight-2);

    // Originally, dragging a window behaved rather differently before and
    // after resizing the window, hence this...
    const rect = id.getBoundingClientRect();    // (nb: recalc in DOM)
//  let w = rect.width,
    let w = round(rect.width),
//      w = floor(rect.width),
//1/9/23 for gTable.exw:
//      h = rect.height;
        h = round(rect.height);
//      h = rect.height - 3;
//      h = floor(rect.height);
    id.style.width = w + "px";
    id.style.height = h + "px";
//  if (x && y) {
//      if (x === IUP_CENTER) { x = (window.innerWidth-w)/2; }
//      if (y === IUP_CENTER) { y = (window.innerHeight-h)/2; }
        id.style.left = x + "px";
        id.style.top = y + "px";
//  }
//28/12/21: (total bust...)
//  $resize_children(id, w, h);
//  const r2 = id.getBoundingClientRect();
//  $resize_children(id, r2.width, r2.height);
//  IupUpdate(id);
//  const dlgbod = id.querySelector(".dialog-body");
//  $resize_children(id,dlgbod.clientWidth-2,dlgbod.clientHeight-2);
//  $resize_children(id,id.clientWidth-2,id.clientHeight-2);
//          if (w !== dialog.clientWidth ||
//              h !== dialog.clientHeight) {
//              dialog.style.width = w + "px";
//              dialog.style.height = h + "px";
//              if (dialog.querySelector(".menuheader")) { h -= 21; }
//              $resize_children(child,w-4,h-34);
//
//  let resize_cb = id.RESIZE_CB;
//  if (resize_cb) {
//      resize_cb(id,w,h);
//  }
    // It may be possible to allow some squishing, not entirely sure...
//26/9/21:
    if (id.minWidth) { w = id.minWidth; }
    if (id.minHeight) { h = id.minHeight; }
//DEV probably wrong 5/4/22...
    id.minimumWidth = w;                        // for use in doSize()
    id.minimumHeight = h;                       //      """
//  id.onresize(id,w,h);
//--DEV REDRAW_CB? ACTION? (triggers violation too)
//  const event = new Event("resize");
//  id.dispatchEvent(event);
//  window.setTimeout(id.dispatchEvent,10,event);
//  window.setTimeout(function() { id.dispatchEvent(event); }, 100);
//  gRedraw(id);
//  id.focus();
//  const r2 = id.getBoundingClientRect();
//  $resize_children(id, r2.width, r2.height);
// 31/21/21: bingo!!
/* SUG... (total failure)
const config = { attributes: true, childList: true, subtree: true };
const callback = function(mutationList, observer) {
    for(const mutation of mutationList) {
        if (mutation.type === "childList") {
            console.log("A child node has been added or removed.");
        }
        else if (mutation.type === "attributes") {
            console.log("The " + mutation.attributeName + " attribute was modified.");
        }
    }
};
const observer = new MutationObserver(callback);
observer.observe(id, config);
*/
//console.log("maxmax");
    $redraw = false;
    $maxWindow(id);
    $redraw = true;
    $maxWindow(id);
//console.log("mmout");
/*
observer.disconnect();
*/
    id.focus();
}
//const IupShowXY = IupShow;
//IupPopup?

function gSetFocus(id) {
    id.focus();
}

function gHide(id) {
    const dialogs = document.getElementsByClassName("dialog");
    if (dialogs.length === 1) {
        window.close();  // close the whole page, if allowed
    } else {
        $topZindex(id); // (specifically, fixup the rest)
        $docBody.removeChild(id);
    }
}

function gGetChild(/*gdx*/ id, /*integer*/ pos) {
    return id.children[pos-1];
}

function gGetChildCount(/*gdx*/ id) {
    return id.childElementCount;
}

//DEV CLASSNAME, should make it match...
//function IupGetClassName(/*gdx*/ id) {
//  return id.classList[0];
//}

function gGetDialog(id) {
    let parent = id.offsetParent;
    while (parent === null || parent.classList[0] !== "dialog") { // nb not NULL!
        id = id.parentNode;
        if (id.classList[0] === "dialog") {
            parent = id;
            break;
        }
    }
//  assert(parent.classList[0] === "dialog");
    return parent;
}

//function gGetDialogChild(/*gdx*/ id, /*string*/ name) {
//  function get_child(id, name) {
//      let count = id.childElementCount;
//      for (let pos = 0; pos < count; pos += 1) {
//          let cp = id.children[pos];
//          if (cp["NAME"] === name) { return cp; }
//          cp = get_child(cp,name);
//          if (cp) { return cp; }      
//      }
//      return NULL;
//  }
//  if (id.classList[0] !== "dialog") {
//      id = gGetDialog(id);
//  }
//  return get_child(id,name);
//}

function gGetFocus() {
    return document.activeElement;
}

function gGetParent(id) {
    let res = id.parentNode;
    if (!res || res.className === "") { res = NULL; }
    return res
}

function gGetBrother(id, bPrev=false) {
    let parent = id.parentNode,
//      siblings = parent.children,
        siblings = [...parent.children],
        pos = siblings.indexOf(id),
        brother = siblings[bPrev?pos-1:pos+1];
    return brother;
}

//function IupNextField(id) {
//  let next = IupGetBrother(id);
//  IupSetFocus(next);
//  return next;
//}
//function IupPreviousField(id) {
//  let prev = IupGetBrother(id,true);
//  IupsetFocus(prev);
//  return prev;
//}

//gButton([nullable_string title=NULL,][rtn click=NULL,] string attributes="", sequence args={}) 
function gButton(title = null, click = null, func = null, attributes = "", args = []) {
    if (!$storeAttr) { $gInit(); }
//  const id = document.createElement("button");
    const id = $ocument_createElement("button");
    id.setAttribute("class", "button");
    [,title,click,attributes,args] = $paranormalise_traa(title,click,attributes,args);
    if (title) {
        gSetAttribute(id,"TITLE",title);
    }
    if (click) {
        gSetHandler(id, "CLICK", click);
    }
    gSetAttributes(id, attributes, args);
    return id;
}

//function gDatePick(action = null, func = null, attributes = "", args = []) {
//DEV
//function gDatePick(value_changed = null, attributes = "", args = []) {
function gDatePick(attributes = "", args = []) {
    if (!$storeAttr) { $gInit(); }
//  const id = document.createElement("input");
    const id = $ocument_createElement("input");
    id.setAttribute("class", "datepick");
    id.setAttribute("type", "date");
/*
    [,value_changed,attributes,args] = $paranormalise_raa(value_changed,attributes,args);
    if (value_changed) {
//      if (action !== null && action !== "VALUECHANGED_CB") { crash("?9/0"); }
        gSetHandler(id, "VALUECHANGED", value_changed);
    }
*/
    gSetAttribute(id,"VALUE","%4d-%02d-%02d",date());
    gSetAttributes(id, attributes, args);
    return id;
}

function gDropDown(/*object*/ options, changed=NULL, /*sequence*/ attributes="", args=[]) {
    if (!$storeAttr) { $gInit(); }
    [,options,changed,attributes,args] = $paranormalise_qraa(options,changed,attributes,args);
    const id = $ocument_createElement("select");
    id.setAttribute("class", "drop");
//? id.DROPDOWN = true;
    if (options) {
        gSetAttribute(id,"OPTIONS",options);
    }
    if (changed) {
        gSetHandler(id,"CHANGED",changed);
    }
    gSetAttributes(id,attributes,args);
//  ctrl_xtra[id] = options
    return id;
}

//DOC: [RASTER]SIZE must use wxh format, one of which will be ignored... or... must be 0.
//      Any attributes must be set before the element is inserted into the hierarchy (not normally an issue).
//function IupFill(attributes = "", args = []) {
//  const id = document.createElement("div");
//  id.setAttribute("class", "fill");
////DEV more complex: If User size is not defined, then when inside an IupHbox or IupGridBox EXPAND is HORIZONTAL, 
////                    when inside a IupVbox EXPAND is VERTICAL.If User size is defined then EXPAND is NO. 
////    id.setAttribute("EXPAND", "YES");
//  gSetAttributes(id, attributes, args);
//  return id;
//}

//function IupFlush() { }

//function gFrame(child = NULL, attributes = "", args = []) {
function gFrame(/*gdx*/ child, /*object*/ title = NULL, /*sequence*/ attributes = "", args = []) {
    if (!$storeAttr) { $gInit(); }
    [,title,attributes,args] = $paranormalise_taa(title,attributes,args);
    const id = $ocument_createElement("fieldset"),
          legend = document.createElement("legend");
    id.setAttribute("class", "frame");
    legend.setAttribute("padding", "0px 5px 0px 5px");
    if (title) {
        legend.innerText = title;
    }
//  id.setAttribute("EXPAND", "YES");
    id["EXPAND"] = "YES";
    id.appendChild(legend);
    if (child) {
        id.appendChild(child);
    }
    gSetAttributes(id, attributes, args);
    return id;
}

function gHbox(children, attributes = "", args = []) {
    if (!$storeAttr) { $gInit(); }
//  if (!Array.isArray(children)) {
    if (!sequence(children)) {
//      crash("vbox children must be an array!");
        crash("hbox children must be a sequence!");
    }
    const id = $ocument_createElement("div");
    id.setAttribute("class", "hbox");
//  id.setAttribute("EXPAND", "YES");
    id["EXPAND"] = "YES";
//  id.className = "hbox";
//9/4/21:
//  for (let i = 0; i < children.length; i += 1) {
    let l = length(children);
    for (let i = 1; i <= l; i += 1) {
        let ci = children[i];
// ?    if (ci.className === "fill") {
//          gSetAttribute(ci,"EXPAND","HORIZONTAL");
//      }
        id.appendChild(ci);
    }
    gSetAttributes(id, attributes, args);
    return id;
}

function gVbox(children, attributes = "", args = []) {
    if (!$storeAttr) { $gInit(); }
//  if (!Array.isArray(children)) {
    if (!sequence(children)) {
//      crash("vbox children must be an array!");
        crash("vbox children must be a sequence!");
    }
    const id = $ocument_createElement("div");
    id.setAttribute("class", "vbox");
//  id.setAttribute("EXPAND", "YES");
    id["EXPAND"] = "YES";
//  id.className = "vbox";
    let l = length(children);
    for (let i = 1; i <= l; i += 1) {
        let ci = children[i];

// ?    if (ci.className === "fill") {
//          gSetAttribute(ci,"EXPAND","VERTICAL");
//      }
        if (i === 1 && ci.className === "submenu") {
//          $xpg_set_menu(??,ci);
//          $xpg_set_menu(id,ci);
            id.MENU = ci
        } else {
            id.appendChild(ci);
        }
    }
    gSetAttributes(id, attributes, args);
    return id;
}

function gImageRGBA(/*integer*/ width, height, /*sequence*/ pixels) {
    let id = document.createElement("canvas");
    id.width = width;
    id.height = height;
    let ctx = id.getContext("2d"),
        imgData = ctx.createImageData(width, height),
        len = length(pixels);
    for (let i = 0; i < len; i += 1) {
        imgData.data[i] = pixels[i+1];
    }
    ctx.putImageData(imgData,0,0);
//  return id;
//or, maybe:
    let res = id.toDataURL();
    return res;
}

function gImageRGB(/*integer*/ width, height, /*sequence*/ pixels) {
    let id = document.createElement("canvas");
    id.width = width;
    id.height = height;
    let ctx = id.getContext("2d"),
        imgData = ctx.createImageData(width, height),
        len = length(pixels),
        k = 0;
    for (let i = 0; i < len; i += 3) {
        imgData.data[k+0] = pixels[i+1];
        imgData.data[k+1] = pixels[i+2];
        imgData.data[k+2] = pixels[i+3];
        imgData.data[k+3] = 255; // (alpha)
        k += 4
    }
    ctx.putImageData(imgData,0,0);
    let res = id.toDataURL();
    return res;
}

function gImage(/*integer*/ width, height, /*sequence*/ pixels, palette) {
    let id = document.createElement("canvas");
    id.width = width;
    id.height = height;
    let ctx = id.getContext("2d"),
        imgData = ctx.createImageData(width, height),
        len = length(pixels),
        k = 0;
//DEV defer this until actually used, or better yet optionally allow a palette.
    if (!palette || length(palette) === 0) {
        palette = [[  0,  0,  0], // ( 0 black)
                   [128,  0,  0], // ( 1 dark red)
                   [  0,128,  0], // ( 2 dark green) 
                   [128,128,  0], // ( 3 dark yellow)
                   [  0,  0,128], // ( 4 dark blue)
                   [128,  0,128], // ( 5 dark magenta) 
                   [  0,128,128], // ( 6 dark cyan) 
                   [192,192,192], // ( 7 gray)
                   [128,128,128], // ( 8 dark gray)
                   [255,  0,  0], // ( 9 red)    
                   [  0,255,  0], // (10 green)
                   [255,255,  0], // (11 yellow)
                   [  0,  0,255], // (12 blue)
                   [255,  0,255], // (13 magenta)
                   [  0,255,255], // (14 cyan)
                   [255,255,255]] // (15 white)
//    XPG_BLACK         = 0x000000,
//    XPG_DARK_RED      = 0x800000,
//    XPG_DARK_GREEN    = 0x008000,
//    XPG_DARK_YELLOW   = 0x808000,     -- aka XPG_OLIVE
//    XPG_DARK_BLUE     = 0x000080,     -- aka XPG_NAVY
//    XPG_DARK_MAGENTA  = 0x800080,
//    XPG_DARK_CYAN     = 0x008080,
//    XPG_GRAY          = 0xC0C0C0,     -- aka XPG_GREY, XPG_SILVER
//    XPG_DARK_GRAY     = 0x808080,     -- aka XPG_DARK_GREY
//    XPG_RED           = 0xFF0000,
//    XPG_GREEN         = 0x00FF00,     -- aka XPG_LIGHT_GREEN
//    XPG_YELLOW        = 0xFFFF00,
//    XPG_BLUE          = 0x0000FF,
//    XPG_MAGENTA       = 0xFF00FF,
//    XPG_CYAN          = 0x00FFFF,
//    XPG_WHITE         = 0xFFFFFF,
    } else {
        let plen = length(palette);
        for (let i = 0; i < plen; i += 1) {
            if (!sequence(palette[i])) {
                palette[i] = to_rgba(palette[i]);
            }
        }
    }   

//  if (length(palette) === 0) { palette = ["sequence",XPG_BLACK, etc.]; }
    for (let i = 0; i < len; i += 1) {
//      let pi = pixels[i+1],
        let /*integer*/ pi = $subse(pixels,i+1),
//          p3 = $gImageDefaultColours[pi];
//          p3 = to_rgba(palette[pi]);
            /* sequence */ p3 = palette[pi];
        for (let j = 0; j < 3; j += 1) {
            imgData.data[k] = p3[j];
            k += 1;
        }   
        imgData.data[k] = 255; // (alpha)
        k += 1;
    }
    ctx.putImageData(imgData,0,0);
    let res = id.toDataURL();
    return res;
}

function gLabel(title=null, attributes = "", args = []) {
    if (!$storeAttr) { $gInit(); }
    [,title,attributes,args] = $paranormalise_taa(title,attributes,args);
    const id = $ocument_createElement("div");
    id.setAttribute("class", "label");
//  id["EXPAND"] = "YES";
    if (title) {
        gSetAttribute(id,"TITLE",title);
    }
    gSetAttributes(id, attributes, args);
    return id;
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

function $get_menu_with_id(menu, id) {
//  let res = menu.querySelector("#" + id);
//  let res = menu.querySelector("[menuid=mid" + id + "]");
    let mid = "mid" + menu.id + "_" + id;
    let res = menu.querySelector("[menuid=" + mid + "]");
//  if (menu.ID !== id) {
////        let clen = 
//
//      return null;
//  }
    return res;
}

function $xpg_mnemonicalize_menu_title(text) {
    if (text) {
        let adx = text.indexOf('&');
        if (adx !== -1) {
            // replace eg "&File" with "<span class="malt">F</span>ile"
            text = text.slice(0,adx) + `<span class="malt">` + text.slice(adx+1,adx+2) + `</span>` + text.slice(adx+2);
        }
    }
    return "<i></i>" + text;
}

function gMenuSetAttribute(/*gdx*/ menu, /*integer*/ id, /*string*/ name, /*object*/ v) {
    // DEV convert menu, id into handle...
//  id = $get_menu_with_id(menu,id);
//  id = menu.querySelector(".??");
//  let mi = menu.getElementById(id);
    let mi = $get_menu_with_id(menu,id);
    if (name === "TITLE") {
//      mi.innerHTML = $xpg_mnemonicalize_menu_title(v);
        mi.children[0].innerHTML = $xpg_mnemonicalize_menu_title(v);
    } else if (name === "ACTIVE") {
        val = $to_bool(val);
        mi.disabled = !val;
    } else if (name === "CHECKED") {
        puts(1,"gMenuSetAttribute(CHECKED)\n");
    }
}

function gMenuGetAttribute(/*gdx*/ menu, /*integer*/ id, /*string*/ name) {
    let mi = $get_menu_with_id(menu,id);
    if (name === "TITLE") {
//      return mi.innerText;
        return mi.children[0].innerText;
    } else if (name === "ACTIVE") {
        return !mi.disabled;
    } else if (name === "CHECKED") {
        puts(1,"gMenuGetAttribute(CHECKED)\n");
        return false;
    }
}

let $next_menu_id = 1;

//function gMenu(/*gdx*/ children=[], /*sequence*/ attributes="", /*dword_seq*/ args=[]) {
function gMenu(/*sequence*/ children, /*rtn*/ handler, /*bool*/ bRadio=false) {
// Note: the class of a menu is "submenu", maybe replaced with "menubar" later, whereas
//       the class of a submenu is "nestmenu", maybe replaced with "topmenu" later...
//atom pChildren = iup_ptr_array(children)
//gdx id = c_func(xIupMenuv, {pChildren})
//  free(pChildren)
//DEV NO, this needs to be done at the IupSetAttributeHandle(dlg,"MENU",menu) stage:
/*
    const mh = document.createElement("div"),
          mb = document.createElement("div"),
         nav = document.createElement("nav"),
          ul = document.createElement("ul");
//<div id="mobbtn">&#9776;</div>
//<nav id="menu"><ul class="menubar">
//  id.setAttribute("class", "menuheader");
    id.id = "menuheader";
    mb.id = "mobbtn";
    mb.innerHTML = "&#9776;";
    nav.id = "menu";
//  ul.setAttribute("class", "menubar");
    ul.className = "menubar";
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
//  const id = document.createElement("li"),
//        sp = document.createElement("span");
    if (!$storeAttr) { $gInit(); }
    const id = $ocument_createElement("ul");
    id.className = "submenu";   // (maybe replaced with "topmenu" later, along with a tabindex of 0)
    id.id = $next_menu_id;
    $next_menu_id += 1;

//  function xpg_add_menu(/*integer*/ pmid, /*atom*/ menu, /*sequence*/ children, /*bool*/ bRadio) {
    function xpg_add_menu(/*atom*/ menu, id, /*sequence*/ children, /*bool*/ bRadio) {
        let /*string*/ text;
        let /*integer*/ mdx;
        let /*sequence*/ rdi = ["sequence"];
        let /*integer*/ rdimin = 0;
        let /*atom*/ rdimh;
        let /*integer*/ lc = length(children);
        for (let i = 1; i <= lc; i += 1) {
            let /*object*/ c = $subse(children,i);
            if (string(c)) {
                assert((equal(c,"|")) || (equal(c,"-")));
                c = $subse(c,1);
            }
            if (atom(c)) {
                assert((equal(c,0X7C)) || (equal(c,0X2D))); // c='|' or c='-'
                let sep = document.createElement("li");
                sep.className = "separator";
                menu.appendChild(sep);
                if (length(rdi)) { bRadio = false; }
//              if (rdi.length) { bRadio = false; }
            } else if ((equal(length(c),2)) && integer($subse(c,2))) {
                // eg {"Cut",CUT=$}
                [,text,mdx] = c;
                let /*bool*/ bCheck = equal($subse(text,1),0X3F);   // text[1]=='?'
                if (bCheck) { text = $subss(text,2,-1); }
                const mi = document.createElement("li"),
                      sp = document.createElement("span"),
                    html = $xpg_mnemonicalize_menu_title(text);
                mi.className = "menuitem";
                sp.innerHTML = html;
                mi.appendChild(sp);
//              if (mdx) { mi.id = mdx; }
//              if (mdx) { mi.menuid = mdx; }
//              if (mdx) { mi.setAttribute("menuid",mdx); }
//              if (mdx) { mi.setAttribute("menuid","mid" + mdx); }
                if (mdx) { 
//                  let mid = "mid" + menu.id + "_" + mdx;
                    let mid = "mid" + id + "_" + mdx;
                    mi.setAttribute("menuid",mid);
                }
                menu.appendChild(mi);
                if (bCheck) {
                    if (bRadio) {
//                      rdi = $conCat(rdi, mdx, false); // (full set stored on each below)
                        rdi = $conCat(rdi, mi, false); // (full set stored on each below)
                        if ((rdimin === 0) || mdx < rdimin) {
                            rdimin = mdx;
                            rdimh = mi;
                        }
//DEV?? (lets worry about this later!)
//                      else
//                          setd({pmid,mdx},0,WIN_MENU_CHECKED)
                    }
                }
            } else {
                text = $subse(c,1);
                mdx = ((length(c) === 3) ? $subse(c,2) : 0);
                let /*sequence*/ grandkids = $subse(c,-1);
                let /*bool*/ bSubRadio = equal($subse(text,1),0X3F); // text[1]='?'
                if (bSubRadio) { text = $subss(text,2,-1); }
                let nestmenu = document.createElement("li"),
                    nm_span = document.createElement("span"),
                    submenu = document.createElement("ul"),
//                  sm_span = document.createElement("span"),
                    html = $xpg_mnemonicalize_menu_title(text);
                nestmenu.className = "nestmenu";  // (maybe replaced with "topmenu" later)
                submenu.className = "submenu";
                nm_span.innerHTML = html;
                nestmenu.appendChild(nm_span);
                nestmenu.appendChild(submenu);
//              if (mdx) { nestmenu.menuid = mdx; }
//              if (mdx) { nestmenu.setAttribute("menuid",mdx); }
//              if (mdx) { nestmenu.setAttribute("menuid","mid" + mdx); }
                if (mdx) { 
//                  let mid = "mid" + menu.id + "_" + mdx;
                    let mid = "mid" + id + "_" + mdx;
                    nestmenu.setAttribute("menuid",mid);
                }
//DEV (untried...)
//              if (mdx) { submenu.menuid = mdx; }

//              xpg_add_menu(pmid,submenu,grandkids,bSubRadio)
                xpg_add_menu(submenu,id,grandkids,bSubRadio);
                menu.appendChild(nestmenu);

                if (length(rdi)) { bRadio = false; }
//              if (rdi.length) { bRadio = false; }
            }
        }
// (lets worry about this later!)
        let /*integer*/ lr = length(rdi);
        if (lr) {
    //      rdi = sort(rdi)
    //      integer f = rdi[1], l = rdi[$]
    //      assert((l-f)=(length(rdi)-1))
    //      bOK = c_func(xCheckMenuRadioItem,{menu,f,l,f,MF_BYCOMMAND})
//DEV (checked)
//          rdimh.?? = true;
            // and stash the full set against every entry
//          for mdx in rdi do
//              setd({pmid,mdx},{rdi,menu},WIN_MENU_CHECKED)
            for (let i=1; i <= lr; i += 1) {
                let /*atom*/ m = $subse(rdi,i);
                m.RADIOGROUP = rdi;
            }
        }
    }
    xpg_add_menu(id,id.id,children,bRadio);

//  if (children.length) {
//      let l = length(children);
//      for (let i = 1; i <= l; i += 1) {
//          let ci = children[i];
////DEV NO, this needs to be done at the IupSetAttributeHandle(dlg,"MENU",menu) stage:
///*
//Each entry of the children structure is: 
// * {string menu_item, integer id} for a "leaf" menu entry, or 
// * any of {"|","-",'|','-'} for a separator (which are all treated identically), or 
// * {string submenu, [integer id,] sequence children}
//
//          cs = ci.children[0];
//      ci.className = "topmenu";
//      cs.tabIndex = 0;
//*/
//crash("menus!")
////            id.appendChild(ci);
//      }
//  }
//  nav.appendChild(ul);
//  id.appendChild(mb);
//  id.appendChild(nav);
//  gSetAttributes(id, attributes, args);
//  if length(attributes) then
//      gSetAttributes(id, attributes, args)
//  end if
    return id;
}

function gPopupMenu(/*gdx*/ menu, /*integer*/ x=XPG_MOUSEPOS, y=XPG_MOUSEPOS) {
    if (!$storeAttr) { $gInit(); }
    crash("gPopupMenu");
/*
    integer ct = ctrl_types[menu]
    assert(ct=MENU)
    bool bMap = and_bits(ctrl_flags[menu],CF_MAPPED)=0
    if bMap then gMap(menu) end if
    atom handle = ctrl_handles[menu]
    if x=XPG_MOUSEPOS
    or y=XPG_MOUSEPOS then
        assert(x==y,"gPopupMenu: XPG_MOUSEPOS is both-only")
        {x,y} = gGetGlobal("MOUSEPOS")
    end if
    if backend=GTK then
        if bGTK3 then
            c_proc(gtk_menu_popup_at_pointer,{handle,NULL})
        else
            atom cb = call_back(gtk_menu_position_func),
               data = x*#8000+y,
             cevntt = c_func(gtk_get_current_event_time,{})
            c_proc(gtk_menu_popup,{handle,NULL,NULL,cb,data,0,cevntt})
        end if
    elsif backend=WinAPI then
        atom hwnd = any_active_window()
        tracked_menu = menu
        integer res = c_func(xTrackPopupMenuEx,{handle,TPM_RETURNCMD,x,y,hwnd,NULL})
        tracked_menu = 0
        if res then
            {} = xpg_menu_common(menu,res,false)
        end if
    else
        ?9/0 -- (unknown backend)
    end if
*/
}

//function gMenuItem(/*nullable_string*/ title=NULL, /*object*/ func=NULL, /*sequence*/ attributes="", args=[]) {
////<li class="menuitem"><span><i></i>Normal</span></li>
//  [,title,func,attributes,args] = $paranormalise_traa(title,func,attributes,args);
//  const id = document.createElement("li"),
//        sp = document.createElement("span");
//  id.className = "menuitem";
//  sp.innerHTML = $xpg_mnemonicalize_menu_title(title);
//  id.appendChild(sp);
//  if (func) {
//      gSetHandler(id,"CLICK",func);
//  }
//  gSetAttributes(id, attributes, args);
//  return id;
//}

//function gSeparator() {
////<li class="separator"></li>
//  const id = document.createElement("li");
//  id.className = "separator";
//  return id;
//}

//function gSubmenu(/*nullable_string*/ title=NULL, /*gdx*/ menu=NULL, /*string*/ attributes="", /*sequence*/ args=[]) {
//  const id = document.createElement("li"),
//        sp = document.createElement("span");
//
//  id.className = "nestmenu";   // (maybe replaced with "topmenu" later)
//  sp.innerHTML = $xpg_mnemonicalize_menu_title(title);
//  id.appendChild(sp);
//  id.appendChild(menu);
////    id.appendChild(ul);
////             <li class="nestmenu">
////              <span><i></i>Triangle</span>
////              <ul class="submenu">
////               <li class="menuitem"><span><i></i>Equilateral</span></li>
////               <li class="menuitem"><span><i></i>Isoceles</span></li>
////               <li class="menuitem"><span><i></i>Scalenus</span></li>
////              </ul>
////             </li>
//
////        <li class="topmenu">
////          <span tabindex="0"><span class="malt">E</span>dit</span>
////          <ul class="submenu">
////           <li class="menuitem"><span><i></i>Copy</span></li>
////           <li class="nestmenu">
////            <span><i></i>Paste</span>
////            <ul class="submenu">
////             <li class="menuitem"><span><i></i>Normal</span></li>
////             <li class="menuitem"><span><i></i>html-stripped</span></li>
////            </ul>
////           </li>
//
//  gSetAttributes(id, attributes, args);
//  return id;
//}

function gProgressBar(attributes = "", args = []) {
    if (!$storeAttr) { $gInit(); }
    const id = $ocument_createElement("progress");
    id.setAttribute("class", "progress");
    id.setAttribute("min", 0);
    id.setAttribute("max", 1);
    gSetAttributes(id, attributes, args);
    return id;
}

function gSpin(/*[rtn]*/ value_changed=NULL, /*string*/ attributes="", /*dword_seq*/ args=[]) {
    if (!$storeAttr) { $gInit(); }
    [,value_changed,attributes,args] = $paranormalise_raa(value_changed,attributes,args);
    let id = $ocument_createElement("input");
    id.className = "spin";
    id.setAttribute("type", "number");
    id.setAttribute("min", 0);
    id.setAttribute("max", 100);
    id.setAttribute("step", 1);
    if (value_changed) { id.VALUE_CHANGED = value_changed; }
    function changed(e) {
        let ctrl = e.currentTarget,
            wrap = ctrl.WRAP,
               n = parseInt(ctrl.value),
            minv = parseInt(ctrl.min),
            maxv = parseInt(ctrl.max);
        if (e.type === "keyup") {
            if (wrap) {
                if(e.keyCode === 38 || e.code === 'ArrowUp') {
                    e.preventDefault();
                    ctrl.value = n===maxv ? minv : n+1;
                } else 
                if(e.keyCode === 40 || e.code === 'ArrowDown') {
                    e.preventDefault();
                    ctrl.value = n===minv ? maxv : n-1;
                }    
            }
        } else { // (e.type === "click")
            if (wrap && n === ctrl.LASTVALUE) {
                if (n === maxv) {
                    n = minv; ctrl.value = n;
                } else if (n === minv) {
                    n = maxv; ctrl.value = n;
                }
            }
            ctrl.LASTVALUE = n;
            let value_changed = ctrl.VALUE_CHANGED;
            if (value_changed) {
                value_changed(ctrl);
            }
        }
    }
    id.addEventListener("click", changed);
    gSetAttributes(id, attributes, args);
    // (while WRAP is creation-only, mainly for WinAPI...)
    if (gGetAttribute(id,"WRAP",false)) {
        id.addEventListener("keyup", changed);
    }
    return id;
}

//DEV IupSplit.
// see file:///E:/downloads/misc/js/vanillajs/vanillajs-resizable-panes/dist/index.html
//C:\Program Files (x86)\Phix\demo\rosetta\hexapawn.exw:538           left = IupSplit(board_frame,moves_frame,"ORIENTATION=HORIZONTAL,MINMAX=100:900"),
//C:\Program Files (x86)\Phix\demo\rosetta\hexapawn.exw:540           full = IupSplit(left,right,"ORIENTATION=VERTICAL,MINMAX=100:900")
function gSplit(/*gdx*/ child1=NULL, child2=NULL, /*string*/ orientation="VERTICAL") {
//
// Creates something like this:
//
//  <div class="split">
//    (child1)
//    <div class="drag-y"></div>    -- (or drag-x iff HORIZONTAL/VERTICAL)
//    (child2)
//  </div>
//
    if (!$storeAttr) { $gInit(); }

    let id = $ocument_createElement("div"),
//    drag = document.createElement("div");
      drag = document.createElement("canvas");
    if (typeof(drag.getContext) === "function") {
//      let ctx = drag.getContext("2d") || drag.getContext("webgl");
//      ctx.fillStyle = "blue";
//      drag.style.backgroundColor = "lightgray";
        drag.ctx = drag.getContext("2d") || drag.getContext("webgl");
    }
    id.className = "split";
    id.style.display = "flex";
    if (orientation === "VERTICAL") {
        id.style.flexDirection = "column";
        drag.className = "drag-y";
//      drag.className = "canvas";
    } else {
        assert(orientation === "HORIZONTAL");
        id.style.flexDirection = "row";
        drag.className = "drag-x";
//      drag.className = "canvas";
    }
    id.ORIENTATION = orientation;
    id.EXPAND = "YES"
//DEV I think I want no border and padding: 2px; background: lightgray;
    function redraw(/*gdx*/ canvas) {
        let /*string*/ orientation = gGetAttribute(gGetParent(canvas),"ORIENTATION");
//      let /*string*/ orientation = gGetAttribute(id,"ORIENTATION");
//      let dclass = canvas.className;
        let /*integer*/ [,w,h] = gGetAttribute(canvas,"SIZE");
        if (orientation === "HORIZONTAL") {
            gCanvasLine(canvas,1,0,1,h-1,XPG_DOTTED,1,XPG_BLACK);
        } else {
            gCanvasLine(canvas,0,1,w-1,1,XPG_DOTTED,1,XPG_BLACK);
        }
    }
    function redraw_closure() { redraw(drag); }
    window.requestAnimationFrame(redraw_closure);
    drag.REDRAW = redraw;
//DEV??
//  id.style.height = "100vh";
//  child1.style.flex = "0 0 50%";
//  child2.style.flex = "1";
    id.appendChild(child1);
    id.appendChild(drag);
    id.appendChild(child2);
/*
cursor: col-resize;
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
//      _leftPane.style.flex = "0 0" + (e.clientX/(_container.clientWidth/100)) + "%";
        child1.style.flex = "0 0" + (e.clientX/(id.clientWidth/100)) + "%";
    }

    function OnMouseMoveY(e){
        if (e == null)
            var e = window.event; 
//      _topPane.style.flex = "0 0" + (e.clientY/(_container.clientHeight/100)) + "%";
        child1.style.flex = "0 0" + (e.clientY/(id.clientHeight/100)) + "%";
    }

    function ff() { return false; }

    function OnMouseDown(e){
//DEV??
        if (e == null) {
            e = window.event; 
        }
    
//      var target = e.target != null ? e.target : e.srcElement;
        let target = e.target;
    
        if ((e.button == 1 && window.event != null || e.button == 0) && target.className == "drag-x") {
            startX = e.clientX;
            dragElement = target;
            document.onmousemove = OnMouseMoveX;
            document.body.focus();
            document.onselectstart = ff;
            target.ondragstart = ff;
            return false;
        } else if ((e.button == 1 && window.event != null || e.button == 0) && target.className == "drag-y") {
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

//  gSetAttributes(id, attributes, args);
*/
/*
<div class="panes-container">
    <div class="left-pane" id="left-pane">
        <p>Left pane</p>
        <ul>
            <li><a href="#">Item 1</a></li>
            <li><a href="#">Item 2</a></li>
            <li><a href="#">Item 3</a></li>
        </ul>
    </div>
    <div class="panes-separator" id="panes-separator"></div>
    <div class="right-pane" id="right-pane">
        <p>Right pane</p>
      <p><a href="https://github.com/lingtalfi/simpledrag">https://github.com/lingtalfi/simpledrag</a></p>
        <p>
            Lorem ipsum dolor sit amet, consectetur adipisicing elit. A accusantium at cum cupiditate dolorum, eius eum
            eveniet facilis illum maiores molestiae necessitatibus optio possimus sequi sunt, vel voluptate. Asperiores,
            voluptate!
        </p>
    </div>
</div>
*/
/*
    let id = document.createElement("div"),
      drag = document.createElement("div");
//  id.className = "split";
    id.className = "panes-container";
    drag.className = "panes-separator";

    id.appendChild(child1);
    id.appendChild(drag);
    id.appendChild(child2);

//  var leftPane = document.getElementById("left-pane");
//  var rightPane = document.getElementById("right-pane");
//  var paneSep = document.getElementById("panes-separator");

    let leftPane = child1,
        rightPane = child2,
        paneSep = drag;

    function ondrag(el, pageX, startX, pageY, startY, fix) {

        fix.skipX = true;

        if (pageX < id.innerWidth * leftLimit / 100) {
            pageX = id.innerWidth * leftLimit / 100;
            fix.pageX = pageX;
        }
        if (pageX > id.innerWidth * rightLimit / 100) {
            pageX = id.innerWidth * rightLimit / 100;
            fix.pageX = pageX;
        }

        var cur = pageX / id.innerWidth * 100;
        if (cur < 0) {
            cur = 0;
        }
        if (cur > id.innerWidth) {
            cur = id.innerWidth;
        }


        var right = (100-cur-2);
        leftPane.style.width = cur + '%';
        rightPane.style.width = right + '%';

    }

    // The script below constrains the target to move horizontally between a left and a right virtual boundaries.
    // - the left limit is positioned at 10% of the screen width
    // - the right limit is positioned at 90% of the screen width
    var leftLimit = 10;
    var rightLimit = 90;


    function sdrag(sepPage, onDrag, onStop, direction) {

        var startX = 0;
        var startY = 0;
//      var el = this;
        var el = sepPage;
        var dragging = false;

        function move(e) {

            var fix = {};
            onDrag && onDrag(el, e.pageX, startX, e.pageY, startY, fix);
            if ('vertical' !== direction) {
                var pageX = ('pageX' in fix) ? fix.pageX : e.pageX;
                if ('startX' in fix) {
                    startX = fix.startX;
                }
                if (false === ('skipX' in fix)) {
                    el.style.left = (pageX - startX) + 'px';
                }
            }
            if ('horizontal' !== direction) {
                var pageY = ('pageY' in fix) ? fix.pageY : e.pageY;
                if ('startY' in fix) {
                    startY = fix.startY;
                }
                if (false === ('skipY' in fix)) {
                    el.style.top = (pageY - startY) + 'px';
                }
            }
        }

        function startDragging(e) {
            if (e.currentTarget instanceof HTMLElement || e.currentTarget instanceof SVGElement) {
                dragging = true;
                var left = el.style.left ? parseInt(el.style.left) : 0;
                var top = el.style.top ? parseInt(el.style.top) : 0;
                startX = e.pageX - left;
                startY = e.pageY - top;
                id.addEventListener('mousemove', move);
            }
            else {
                throw new Error("Your target must be an html element");
            }
        }

//      this.addEventListener('mousedown', startDragging);
        sepPage.addEventListener('mousedown', startDragging);
//      window.addEventListener('mouseup', function (e) {
        id.addEventListener('mouseup', function (e) {
            if (true === dragging) {
                dragging = false;
//              window.removeEventListener('mousemove', move);
                id.removeEventListener('mousemove', move);
                onStop && onStop(el, e.pageX, startX, e.pageY, startY);
            }
        });
    }

//  paneSep.sdrag(ondrag, null, 'horizontal');
    orientation = orientation.toLowerCase();
//  sdrag(paneSep, ondrag, null, 'horizontal');
    sdrag(paneSep, ondrag, null, orientation);
*/
    return id;
}


//DEV erm, hello, where are IupTableClick_cb(), IupTableEnterItem_cb(), IupTableResize_cb(),
//                          IupTableGetSelected(), IupTableGetData(), IupTableSetData()???
function gTable(columns, data, rows=10, attributes="", args=[]) {
    if (!$storeAttr) { $gInit(); }
    [,rows,attributes,args] = $paranormalise_raa(rows,attributes,args)//,bCheckRid:=false)
    if (rows === 0) { rows = 10; }
    let table = $ocument_createElement("table"),
        container = document.createElement("div");  // wraps table+resizers
//DEV this is fine, but the dialog is 3px too high...
    table.style.height = rows*23+27 + "px";
//  table.classList = "table";
    container.appendChild(table);
    container.classList = "container";
    container.style.width = "100%";
    container.style.height = "100%";

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
            tds = Array.from(node.querySelectorAll("td"));
        if (tds.length===1) {
            for (let i=1; i<=nColumns; i += 1) {
                let td = document.createElement("td");
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
            ad = th.getAttribute("data-sorted"),
            ths = th.parentNode.querySelectorAll("th");
        for (let i=0; i<ths.length; i += 1) {
            let ti = ths[i];
            ti.removeAttribute("data-sorted");
        }
        ad = (ad === 'A') ? 'D' : 'A';
        sort_col = Number(th.id);
        sort_dir = (ad === 'A') ? +1 : -1;
        tags.sort(xSorter);
        th.setAttribute("data-sorted", ad);
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
        if (trOld) { trOld.classList.remove("trActive"); }
        return false;
    }

//  let shift, ctrl;

    function trSelect(trOld, trNew, bPageKey=0) {
// maybe not for pageup/down:...
        if (trNew==null) { return true; }   // pass keybd input up
        trHide(trOld);
        trNew.classList.add("trActive");
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
          trOld = trNew.parentNode.querySelector(".trActive");
      trSelect(trOld,trNew);
    }

    function table_keyhandler(event) {
        let table = event.currentTarget,
            tbody = table.querySelector("tbody"),
            trSel = tbody.querySelector(".trActive");
        if (trSel!=null) {
//          shift = event.shiftKey;
//          ctrl = event.ctrlKey;
            switch(event.key)
            {
                case "ArrowDown":   return trSelect(trSel,trSel.nextElementSibling);
                case "ArrowUp":     return trSelect(trSel,trSel.previousElementSibling);
                case "PageDown":    return trSelect(trSel,trPage(trSel,+1),+1);
                case "PageUp":      return trSelect(trSel,trPage(trSel,-1),-1);
                case "Home":        return trSelect(trSel,trSel.parentNode.firstElementChild,"Home");
                case "End":         return trSelect(trSel,trSel.parentNode.lastElementChild,"End");
                case "Escape":      return trHide(trSel);
            }
        }
    }

    function populate_table() {
        let tbody, bRefresh = false;
        if (created === 0) {
            let thead = document.createElement("thead"),
                headr = document.createElement("tr");
            tbody = document.createElement("tbody");
//          tbody.classList = "tbody";
//23/8/21 (when making KEY_CB work... put back if focus goes wrong)
//          tbody.setAttribute("tabindex", "0");
            tbody.setAttribute("tabindex", "-1");
            thead.appendChild(headr);
            table.appendChild(thead);
            table.appendChild(tbody);
            for (let i=1; i<=nColumns; i += 1) {
                let th = document.createElement("th"),
                    ci = columns[i];
                if (Array.isArray(ci)) {
                    let width = (columns[i][2]-11) + "px",
                        align = columns[i][3].toLowerCase().slice(1);
                    th.style.width = width;
                    th.style.maxWidth = width;
                    th.style.textAlign = align;
                    table_widths[i] = width;
                    table_aligns[i] = align;
                    ci = ci[1];
                }
                th.textContent = ci;
                th.id = i;
                th.classList.add("sortable");
                th.onclick = sortTable;
                headr.appendChild(th);
            }   
            let th = document.createElement("th");
            th.classList.add("thlast");
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
            let tr = document.createElement("tr");
            for (let c=1; c<=nColumns; c += 1) {
                tr.appendChild(document.createElement("td"));
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

//DEV the column resizers work (at first) if you (manually) do this:
/*
  background: beige;
  opacity: 0.3;
  left: 126px;
*/
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

function gTableGetSelected(/*gdx*/ table) {
//  crash("testme");
    puts(1,"gTableGetSelected\n");
//  let tr = table.parentNode.querySelector(".trActive"),
    let tr = table.querySelector(".trActive"),
        idx = tr.id;
//      idx = tr.id || 0;
//  integer idx = IupGetInt(table,"FOCUSCELL") -- (line only)
//  if idx!=0 then
//      integer dsidx = IupGetInt(table,"DSIDX")
//      idx = table_tagsets[dsidx][idx]
//  end if
    return idx;
}

function gTableClearSelected(/*gdx*/ table) {
//  crash("testme");
    puts(1,"gTableClearSelected\n");
//  let tr = table.parentNode.querySelector(".trActive"),
    let tr = table.querySelector(".trActive");
    if (tr) {
        tr.classList.remove("trActive");
    }
}

//function IupTableClick_cb(/*gdx*/ table, /*integer*/ l, c/*, atom pStatus*/) {
//// default CLICK_CB for IupTable (callable directly, when overridden)
//  puts(1,"IupTableClick_cb\n"); // placeholder
///*
//  integer numcol = IupGetInt(table,"NUMCOL")
//  if l=0 and c>0 and c<=numcol then
//      dsidx = IupGetInt(table,"DSIDX")
//      sequence data = table_datasets[dsidx],
//               cols = table_sortcols[dsidx]
//      integer sel = IupTableGetSelected(table),
//                k = find(c,cols)
//      integer sortcol = iff(length(cols)?cols[1]:0)
//      if k=1 then
//          table_sortdirs[dsidx][1] *= -1
//      else
//          if k then
//              table_sortcols[dsidx][k..k] = {}
//              table_sortdirs[dsidx][k..k] = {}
//          end if
//          table_sortcols[dsidx] = prepend(table_sortcols[dsidx],c)
//          table_sortdirs[dsidx] = prepend(table_sortdirs[dsidx],1)
//      end if
//      if sortcol!=0 and sortcol!=c then
//          IupSetAttributeId(table,"SORTSIGN",sortcol,"NO")
//      end if
//      integer sortdir = iff(gGetAttributeId(table,"SORTSIGN",c)="DOWN"?-1:1)
//      IupSetAttributeId(table,"SORTSIGN",c,iff(sortdir=-1?"UP":"DOWN"))
//      table_tagsets[dsidx] = custom_sort(routine_id("by_column"),table_tagsets[dsidx])
//      IupSetAttribute(table,"REDRAW","ALL")
//      -- restore selection - it stays off-screen, but user can use up/down
//      --  to force it into the viewport, should they be inclined to do so.
//      sel = find(sel,table_tagsets[dsidx])
//      {} = IupTableEnterItem_cb(table, sel, 1)
//  end if
//*/
//  return XPG_DEFAULT;
//}

function gTabs(/*gdx*/ children = [], /*string*/ attributes="", /*sequence*/ args = []) {
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
    if (!$storeAttr) { $gInit(); }
    const id = $ocument_createElement("div"),
        tabs = document.createElement("ul"),
      panels = document.createElement("div");
    id.setAttribute("class", "tabcontainer");
    tabs.setAttribute("class", "tabs");
    panels.setAttribute("class", "panels");
//  id.setAttribute("EXPAND", "YES");
    id["EXPAND"] = "YES";
//  id.className = "tabs";

    function tabclick(event) {
        let tgt = event.target;
        if (tgt.classList.contains("tab")) {
            let tabul = tgt.parentNode,                 // the <ul tabs> (lone)
                tabch = [...tabul.children],            // the <li tab>s (Array)
                 pdiv = tabul.parentNode.children[1],   // the <div panel>s
                  act = tabul.querySelector(".active"),
                  adx = tabch.indexOf(act),
                  apt = pdiv.children[adx],
                  tdx = tabch.indexOf(tgt),
                  pnl = pdiv.children[tdx];
            act.classList.remove("active");
            apt.classList.remove("active");
            tgt.classList.add("active");
            pnl.classList.add("active");
        }
    }

//DEV:
//Each element of children can therefore be a gdx, title, {gdx,title[,image]}, or {title[,image]}, where title is a string. 
    let l = children.length;
    for (let i = 1; i < l; i += 1) {
        let ci = children[i],
            li = document.createElement("li"),
            pi = document.createElement("div");
        if (string(ci)) {
            li.innerText = ci;
        } else if (sequence(ci)) {
            let txtIdx = 1;
            if (!string(ci[1])) {
                pi.appendChild(ci[1]);
                txtIdx = 2;
            }
            li.innerText = ci[txtIdx];
            if (length(ci)>txtIdx) {
                let img = ci[txtIdx+1];
//DEV now what??
            }
        } else {
            li.innerText = ci.TABTITLE;
            pi.appendChild(ci);
        }               
        li.setAttribute("class", "tab");
        pi.setAttribute("class", "panel");
        if (i === 1) {
            li.classList.add("active");
            pi.classList.add("active");
        }
        li.addEventListener("click", tabclick);
        tabs.appendChild(li);
        panels.appendChild(pi);
    }
    id.appendChild(tabs);
    id.appendChild(panels);
    gSetAttributes(id, attributes, args);
    return id;
}       

function gText(value_changed=null, attributes = "", args = []) {
    if (!$storeAttr) { $gInit(); }
    const id = $ocument_createElement("input");
    id.setAttribute("class", "text");
    [,value_changed,attributes,args] = $paranormalise_raa(value_changed,attributes,args);
    if (value_changed) {
        gSetHandler(id,"VALUECHANGED",value_changed);
//          if (action === "VALUECHANGED") {
//  //          printf(1,"warning: IupText(VALUECHANGED) not yet supported\n");
//              gSetHandler(id,action,func);
//          } else {
//              if (action !== null && action !== "ACTION") { crash("?9/0"); }
//  //          if (!action) { action = "ACTION"; }
//              function action_cb(event) {
//                  puts(1,"action_cb");    // placeholder...
//  //              let c = ??,
//  //                  pNewValue = ??,
//  //                  res = func(/*gdx*/ id, /*integer*/ c, /*atom*/ pNewValue);
//  //              if (res===XPG_IGNORE) {
//  //              }
//              }
//              id.onkeyup = action_cb;
//  //          id.addEventListener("keyup", action_cb);
//          }
    }
    gSetAttributes(id, attributes, args);
    return id;
}       

//DEV document this, MULTILINE not supported.
//(might also warrant a class of "multiline")
//function IupMultiLine(action=null, func=null, attributes = "", args = []) {
////    let id = IupText(action, func, attributes, args);
////    gSetAttribute(id, "MULTILINE", "YES")
//  const id = document.createElement("textarea");
//  id.setAttribute("class", "text");
//  [action,func,attributes,args] = $paranormalise(action,func,attributes,args);
//  if (func) {
//      if (!action) { action = "ACTION"; }
//      IupSetCallback(id,action,func);
//  }
//  gSetAttributes(id, attributes, args);
//  return id;
//}     

//  let $radio_id = 1;
//
//  //--DEV: RADIO on a h/vbox...
//  function IupRadio(/*gdx*/ child=NULL, /*string*/ attributes="", /*sequence*/ args=[]) {
//      const id = document.createElement("div"),
//          name = "radio"+$radio_id;
//      $radio_id += 1;
//      id.setAttribute("class", "radio");
//      function radiate(id) {
//          let cn = id.classList[0];
//          if (cn === "toggle") {
//              let input = id.children[1];
//              assert(input.type === "checkbox");
//              input.type = "radio";
//              input.name = name;
//          } else {
//              let l = id.children.length;
//              for (let i = 0; i < l; i += 1) {
//                  radiate(id.children[i]);
//              }
//          }
//      }
//      radiate(child);
//      id.appendChild(child);
//      gSetAttributes(id, attributes, args);
//      return id;
//  }       


//let $radio_groups = ["sequence"];

function gRadio(/*gdx*/ ids) {
    let rgx = ids[1];
    rgx.RADIO_GROUP = ids;
//  let rdx = length($radio_groups);
    let idl = length(ids);
//  function radio_click() {
//      // nb ids is part of the closure here...
//      
//  }
    for (let i = 1; i <= idl; i += 1) {
        let id = ids[i],
            cb = id.children[1];
//      id.RDX = rdx;
        id.RGX = rgx;
        cb.type = 'radio';
//DEV
//      id.onclick = radio_click;
    }
//  $radio_groups = append($radio_groups,{NULL,ids})
}

function gRadioItem(/*gdx*/ id) {
    let rgx = id.RGX;
    if (!rgx) { return -1; }
    let ids = rgx.RAGIO_GROUP,
        idl = length(ids);
    for (let i = 1; i <= idl; i += 1) {
        let id = ids[i],
            cb = id.children[1];
        if (cb.checked) { return id; }
    }
//  assert(ctrl_types[id]=CHECKBOX)
//  integer rdx = ctrl_xtra[id]
//  -- (of course this will typecheck should you store it in a gdx:)
//  if rdx=0 then return -1 end if
//  for id in radio_groups[rdx][2] do
//      if gGetAttribute(id,"VALUE") then return id end if
//  end for
//  return 0 -- (whereas "" and this are fine being stored in a gdx)
    return 0;
}


let $toggle_id = 1;

function gCheckbox(/*nullable_string*/ title=null, /*rtn*/ value_changed=null, /*string*/ attributes = "", /*sequence*/ args = []) {
//<label accesskey="s" for="strict-mode"><input type="checkbox" id="strict-mode" /> <u>S</u>trict Mode</label accesskey="x">
    if (!$storeAttr) { $gInit(); }
    [,title,value_changed,attributes,args] = $paranormalise_traa(title,value_changed,attributes,args);
//  const id = document.createElement("div");
    const id = $ocument_createElement("label"),
          cb = document.createElement("input"),
          span = document.createElement("span"),
          tid = "toggle"+$toggle_id;
    $toggle_id += 1;
    cb.type = "checkbox";
    cb.id = tid;
    id.for = tid;   
    id.setAttribute("class", "toggle");
    if (title) {
//      id.innerHTML = title;
        let adx = title.indexOf('&');
        if (adx !== -1) {
            crash("not yet done...");
//          let key = title[adx];
//          title = title.slice(0,adx) + "<u>" + key + "</u>" + title.slice(adx+1);
//          lbl.accesskey = key.toLowerCase();
        }
        span.innerHTML = "<nobr>" + title + "</nobr>";
//DEV:
//      gSetAttribute(id,"TITLE",title);
    }
    id.appendChild(span);
    id.appendChild(cb);
    id.style.display = "flex";
    id.style.flexDirection = "row-reverse";
    if (value_changed) {
        // (nb: cb is id.children[1]...)
        gSetHandler(id,"VALUE_CHANGED",value_changed);
    }
    gSetAttributes(id, attributes, args);
    return id;
}       
//const IupFlatToggle = IupToggle;

//function IupTreeGetUserId(tree, id) {
//function gTreeGetUserId(tree, id) {
function gTreeGetUserId(treenode) {
//DEV/DOC this is effectively a null-op on the web browser. [BLUFF!]
    let [,tree,id] = treenode;
//  let leaf = tree.querySelector(`[data-tree-id="` + id + `"]`);
//  let /*integer*/ userid = Number(leaf.getAttribute("data-tree-id"));
//  if (userid!==id) { crash("userid error?"); }
//  return userid;
    return id;
//  return treenode;
}

//p2js [DEV/temp]
//function IupTreeAddNodes(/*gdx*/ tree, /*sequence*/ tree_nodes, /*integer*/ id=-1) {
//function IupTreeAddNodes(tree, tree_nodes, id=-1) {
function gTreeAddNodes(tree, tree_nodes, id=-1) {

    function createTVelem(tagname, classname, innerhtml) {
        let res = document.createElement(tagname);
        res.classList.add(classname);
        if (innerhtml) { res.innerHTML = innerhtml; }
//      if (innerhtml) { crash("oh, it *is* used!"); }
        return res;
    }
    if (tree === "createTVelem") { return createTVelem; } // for gTreeView

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
                if (name === "STATE") {
                    if (v === "COLLAPSED") {
                        leaf.classList.add("closed");
                    } else if (v === "EXPANDED") {
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
//  function iupTreeAddNodesRec(/*gdx*/ tree, /*sequence*/ tree_nodes, /*bool*/ bRoot=true) {
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
            for (let i = 1; i <= lench; i += 1) {
                xpg_TreeAddNodesRec(leaf, children[i], false);
            }
            if (lentn === 3) {
                xpg_TreeSetNodeAttributes(leaf, tree_nodes[2]);
            }
        }
    }

    function xpg_tree_delete_children(node) {
        if (sequence(node)) { node = node[1]; }
        while (node.hasChildNodes()) {  
            node.removeChild(node.firstChild);
        }
    }

//  if (id===-1) {
    if (!sequence(tree)) {
//      IupSetAttributeId(tree,"DELNODE",0,"ALL");  // (maybe...)
        xpg_tree_delete_children(tree);
        xpg_TreeAddNodesRec(tree, tree_nodes, true);
    } else {
        // tree_nodes is actually just children
        let children = tree_nodes,
            lench = length(children),
            tree12 = tree, // (debug aid)
            id = tree[2];
        tree = tree[1];
//      tree = tree.querySelector(`[data-tree-id="` + id + `"]`);
        let list = tree.querySelector("ul");
        if (list) { xpg_tree_delete_children(list); }
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
//function IupTreeView(/*sequence*/ tree_nodes, /*atom*/ branchopen=null, /*string*/ attributes="", /*sequence*/ args=[]) {
//function IupTreeView(tree_nodes, branchopen=null, attributes="", args=[]) {
function gTreeView(tree_nodes = [], branchopen = null, attributes = "", args = []) {
//
// Creates a tree from a recursive [callback] data structure.
//
// tree_nodes is as per gTreeAddNodes()
// branchopen can be used for deferred loading, or be null if tree_nodes is complete.
// you can also set branchopen later, via gSetHandler(tree,"BRANCHOPEN",branchopen)
//
    if (!$storeAttr) { $gInit(); }
    [,tree_nodes,branchopen,attributes,args] = $paranormalise_qraa(tree_nodes,branchopen,attributes,args);
    const createTVelem = gTreeAddNodes("createTVelem");
    let /*gdx*/ tree = createTVelem("ul", "tree");
//  let /*gdx*/ tree = gTreeAddNodes.createTVelem("ul", "tree"); // does not work...
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
                let branchopen = tree.BRANCHOPEN;
                if (branchopen) {
                    let id = Number(leaf.parentNode.getAttribute("data-tree-id"));
                    if (id) {
//                      branchopen(tree,id);
//                      branchopen(["sequence",tree,id]);
                        branchopen(["sequence",leaf.parentNode,id]);
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

    if (branchopen) {
//DEV we (also) need to support this, btw:
//      gSetHandler(tree, "BRANCHOPEN",  branchopen);
//      tree.branchopen = branchopen;
        tree["BRANCHOPEN"] = branchopen;
    }
    gTreeAddNodes(tree, tree_nodes);
//DEV gSetAttributes(tree,attributes,args); // maybe, MARKMULTIPLE
    return tree;
}

//<input type="range" min="1" max="10" value="3" step="0.1" class="slider" id="lengthRange">
//IupValuator
function gSlider(/*nullable_string*/ orientation=NULL, /*rtn*/ value_changed=NULL, /*string*/ attributes="", /*sequence*/ args = []) {
    if (!$storeAttr) { $gInit(); }
    [,orientation,value_changed,attributes,args] = $paranormalise_traa(orientation,value_changed,attributes,args);
    const id = $ocument_createElement("input");
    id.setAttribute("class", "slider");
    id.setAttribute("type", "range");
    id.setAttribute("min", 0);
    id.setAttribute("max", 100);
    id.setAttribute("value", 0);
    id.setAttribute("step", "any");
    if (orientation) {
        gSetAttribute(id,"ORIENTATION",orientation);
    }
    if (value_changed) {
//      gSetHandler(id, "VALUE_CHANGED", value_changed);
        gSetHandler(id, "VALUECHANGED", value_changed);
//      btn.addEventListener("click", toggleHide);
/*
solveSlider.addEventListener("input", (e) => {
  solveMilliseconds = parseInt(e.target.value, 10);
  solveTimeContainer.innerText = e.target.value;
});
*/
    }
    gSetAttributes(id, attributes, args);
    return id;
}

function gMainLoop() { }

function gDestroy(/* gdx */ dlg) {
//  NB: IupMainLoop() is a null-op in xpGUI.js so it ends up invoking following code immediately...
//  instead, do (say) this:
//
//      if platform()!=JS then
//          gMainLoop()
//          dlg = gDestroy(dlg)
//      end if
//
//  Note however that eg gDestroy(IupNormaliser(...)) should [one day] be perfectly valid.
//
//27/9/21 try it anyways...(NULL is probably better but neither has really been properly tested)
//  return null;  //DEV would be good for id, fatal for non-if-not-JS-wrapped (main)dlg=gDestroy(dlg)...
    return NULL;  //DEV would be good for id, fatal for non-if-not-JS-wrapped (main)dlg=gDestroy(dlg)...
//  return dlg;
}

function gCanvas(redraw = null, attributes = "", args = []) {
    if (!$storeAttr) { $gInit(); }
    let id = $ocument_createElement("canvas");
//id.width = 0;
//id.height = 0;
    id.setAttribute("class", "canvas");
    id["EXPAND"] = "YES";
    [,redraw,attributes,args] = $paranormalise_raa(redraw,attributes,args);
    if (attributes) {
        gSetAttributes(id, attributes, args);
    }
    if (redraw) {
        id.REDRAW = redraw;
        function redraw_closure() {
            let [,w,h] = gGetAttribute(id,"SIZE");
            redraw(id,w,h);
        }
        window.requestAnimationFrame(redraw_closure);
/* bust:
        function resize_handler(event) {
            let x = event;
        }
        id.addEventListener("resize",resize_handler);   
*/
    }
    if (typeof(id.getContext) === "function") {
//      let ctx = id.getContext("2d");
        let ctx = id.getContext("2d") || id.getContext("webgl");
//if (ctx) {
        ctx.fillStyle = "white";
//      ctx.backGround = "#ffffff"; // (custom attribute)
        id.style.backgroundColor = "#FFFFFF";
        id.ctx = ctx;               //        ""
//      return ctx;
//}
    }
    return id;
}
// a scrollable canvas:
//  <!DOCTYPE html>
//  <html>
//   <head>
//    <title>Canvas scroll bar</title>
//   </head>
//   <body>
//    <h2 style="color:black"><i>Canvas scroll bar</i></h2>
//    <style>
//      html, body {
//          width:100%;
//          height:100%;
//          margin:0;
//          overflow:hidden
//          }
//      #parent {
//          width:400px;
//          height:400px;
//          border:1px solid #000;
//          overflow:auto;
//      }
//      .canvas {
//          width:auto;
//          height:200%;
//      }
//    </style>
//    <div id="parent">
//      <canvas id="canvas" class="canvas" width="2500" height="3500"></canvas>
//    </div>
//    <script>
//          var canvas = document.getElementById("canvas");
//          var context = canvas.getContext('2d');
//          context.lineWidth = 1;
//          context.strokeStyle = 'pink';
//          for (let i = 10; i < canvas.height; i += 100) {
//              context.moveTo(i, 0);
//              context.lineTo(0, i);
//          }
//          context.stroke();
//    </script>
//   </body>
//  </html>

//DEV we might still need this...
//function cdCreateCanvas(context, id) {
//  if (context===CD_DBUFFER ||
//      context===CD_IUP) {
//      if (typeof(id.getContext) === "function") {
////            let ctx = id.getContext("2d");
//          let ctx = id.getContext("2d") || id.getContext("webgl");
////if (ctx) {
//          ctx.fillStyle = "white";
//          ctx.backGround = "#ffffff"; // (custom attribute)
//          return ctx;
////}
//      }
////        id.style.textAlign = "start";
////        id.textAlign = "start";
////        id.textBaseline = "alphabetic";
//      return id;
//  } else if (context===CD_GL) {
////        if ($gl) { return $gl; }
//////      crash("CD_GL not supported by JavaScript");
////        crash("cdCreateCanvas(CD_GL) requires that IupGLMakeCurrent() has been called");
//      // (see for example demo/rosetta/animate_pendulum2.exw)
//      crash(`needs "cdCreateCanvas(CD_GL)" ==> "if JS then cdCreateCanvas(CD_IUP,canvas) else ..."`);
//  }
//  crash("cdCreateCanvas(" + context + ",...) not supported");
//}

//function cdCanvasActivate(ctx) {
//  // nuthin needs doin here...
//}

//DEV use       XPG_FILLED = 0b001, -- (nb same as true[/false])
//              XPG_CHORD  = 0b010,
//              XPG_SECTOR = 0b100

//function cdCanvasSetAttribute(/*cdCanvas*/ ctx, /*string*/ name, /*nullable_string*/ val, /*sequence*/ args=[]) {
//  if (name=="SIZE") {
//      if (val!=="%dx%d %g") { crash(`"%dx%d %g" expected`); }
//      let [,w,h] = args;
//      $resize_children(ctx.canvas,w,h);
////C:\Program Files (x86)\Phix\demo\pGUI\simple_paint.exw:785     cdCanvasSetAttribute(rgb_canvas, "RESOLUTION", "%g", {res})
////C:\Program Files (x86)\Phix\demo\pGUI\simple_paint.exw:859         cdCanvasSetAttribute(cd_canvas, "IMGINTERP", "NEAREST")  -- affects only drivers that have this attribute
////C:\Program Files (x86)\Phix\demo\pGUI\simple_play.exw:766   cdCanvasSetAttribute(canvas, "ROTATE", NULL)
//  } else {
//      puts(1,"cdCanvasSetAttribute("+name+")??\n"); // placeholder
//  }
//}

//function gCanvasSetBackground(ctx, colour) {
//function gCanvasSetBackground(canvas, colour) {
//  if (integer(colour)) { colour = sprintf("#%06x",colour); }
////    ctx.fillStyle = colour;
////    ctx.backGround = colour; // (custom attribute)
////    canvas.ctx.backGround = colour; // (custom attributez)
//  canvas.style.backgroundColor = colour;
//}
//
////function gCanvasGetBackground(ctx) {
//function gCanvasGetBackground(canvas) {
////    return ctx.backGround;
////    return canvas.ctx.backGround;
//  return canvas.style.backgroundColor;
//}
//
//function gCanvasSetForeground(ctx, colour) {
//function gCanvasSetForeground(canvas, colour) {
//  if (integer(colour)) { colour = sprintf("#%06x",colour); }
//  let ctx = canvas.ctx;
//  ctx.fillStyle = colour;
//  ctx.strokeStyle = colour;
//}
//
////function gCanvasGetForeground(ctx) {
//function gCanvasGetForeground(canvas) {
////    return ctx.strokeStyle;
//  return canvas.ctx.strokeStyle;
//}
//
//function cdCanvasSetFillMode(ctx, mode) {
//  if (mode === CD_WINDING) {
//      // default, do nothing for now
////    } else if (mode === CD_EVENODD) {
//      // not even sure this is possible...
//  } else {
//      crash("uh?");
//  }
//}
//
//function cdCanvasSetInteriorStyle(/*cdCanvas*/ ctx, /*integer*/ style) {
///*
//Configures or retrieves the current style for the area filling primitives: 
//  CD_SOLID, CD_HOLLOW, CD_HATCH, CD_STIPPLE or CD_PATTERN.
//Note that only CD_HATCH and CD_STIPPLE are affected by the backopacity.
//Default value: CD_SOLID.
//If a stipple or a pattern were not defined, when they are selected the state of the attribute is not changed. 
//When the style CD_HOLLOW is defined, functions cdCanvasBox() and cdCanvasSector() 
//behave as their equivalent cdCanvasRect() and gCanvasArc()+cdCanvasLine()s,
//and the polygons with style CD_FILL behave like CD_CLOSED_LINES. 
//*/
////    ctx.??? = style;
//  if (style===CD_SOLID) {
//      // nowt yet, but may have to undo other things this does...
//  } else {
//      puts(1,"cdCanvasSetInteriorStyle(" + style + ",...) not supported\n");
//  }
//}

//function cdCanvasGetInteriorStyle(/*cdCanvas*/ canvas) {
////    let /*integer*/ style = canvas.???;
//  return style;
//}

//function gCanvasSetLineStyle(/*cdCanvas*/ ctx, /*integer*/ style) {
//function gCanvasSetLineStyle(/*cdCanvas*/ canvas, /*integer*/ style) {
//  let ctx = canvas.ctx;
////      XPG_CONTINUOUS = 0,
////      XPG_DASHED = 1,
////      XPG_DOTTED = 2,
////      XPG_DASH_DOT = 3,
////      XPG_DASH_DOT_DOT = 4,
////    ctx.?lineXXX = style;
////    ctx.lineDashOffset = style*5;
////    let lds = ctx.getLineDash();
////    let lds = [[],[15,5],[5,5],[],[]][style];
////    ctx.setLineDash(lds);
////    ctx.setLineDash([[],[24,8],[4,4],[12,8,4,8],[12,4,4,4,4,4]][style]);
////    if (style !== CD_CUSTOM) {
//  ctx.setLineDash([[],[8,8],[4,4],[12,8,4,8],[12,4,4,4,4,4]][style]);
//  ctx.LINESTYLE = style;
//  if (style!==XPG_CONTINUOUS) { ctx.lineWidth = 1; }
////    }
////    puts(1,"cdCanvasSetLineStyle() not yet supported\n"); // placeholder
//}
//
////function gCanvasGetLineStyle(/*cdCanvas*/ ctx) {
//function gCanvasGetLineStyle(/*cdCanvas*/ canvas) {
////    return ctx.LINESTYLE || XPG_CONTINUOUS;
//  return canvas.ctx.LINESTYLE || XPG_CONTINUOUS;
//}
//
//function gCanvasSetLineWidth(/*cdCanvas*/ ctx, /*atom*/ width) {
//function gCanvasSetLineWidth(/*cdCanvas*/ canvas, /*atom*/ width) {
//  let ctx = canvas.ctx;
//  ctx.lineWidth = width;
//  if (width!==1) { ctx.setLineDash([]); }
////    puts(1,"cdCanvasSetLineWidth() not yet supported\n"); // placeholder
//}
//
////function gCanvasGetLineWidth(/*cdCanvas*/ ctx) {
//function gCanvasGetLineWidth(/*cdCanvas*/ canvas) {
////    return ctx.lineWidth;
//  return canvas.ctx.lineWidth;
//}

//let gai = true;

//function gCanvasArc(ctx, xc, yc, w, h, angle1, angle2, flags) {
//function gCanvasArc(canvas, xc, yc, w, h, angle1, angle2, flags) {
function gCanvasArc(/*gdx*/ canvas, /*atom*/ xc, yc, w, h, angle1, angle2, /*integer*/ flags=0, style=-1, width=-1, colour=-1) {
//flags can be XPG_FILLED with XPG_CHORD or XPG_SECTOR
//  if (gai) { gai = false; puts(1,"gCanvasArc incomplete (colour)...\n") }
    let ctx = canvas.ctx;
//  let ch = ctx.canvas.clientHeight;
//  let ch = ctx.canvas.clientHeight - yc;
    let ch = yc;
    if (xc > 0 && ch > 0 && w >= 2 && h >= 2) {
        let pstyle, pwidth, pss, pfs;
//      if (style!=-1) { pstyle = gCanvasGetLineStyle(canvas); gCanvasSetLineStyle(canvas,style); }
        if (style!=-1) { pstyle = gGetInt(canvas,"LINESTYLE"); gSetInt(canvas,"LINESTYLE",style); }
//      if (width!=-1) { pwidth = gCanvasGetLineWidth(canvas); gCanvasSetLineWidth(canvas,width); }
        if (width!=-1) { pwidth = gGetInt(canvas,"LINEWIDTH"); gSetInt(canvas,"LINEWIDTH",width); }
        if (colour!=-1) { 
//          gCanvasSetForeground(canvas, colour);
            if (integer(colour)) { colour = sprintf("#%06x",colour); }
            pss = ctx.strokeStyle,
            pfs = ctx.fillStyle,
            ctx.fillStyle = colour;
            ctx.strokeStyle = colour;
        }
        let /*atom*/ a1r = angle1*Math.PI/180,
                     a2r = angle2*Math.PI/180,
         // The arc starts at the point (xc+(w/2)*cos(angle1),
         //                              yc+(h/2)*sin(angle1))
         // and ditto ends/angle2 (but WinAPI goes counter-clockwise):
         sx = xc+(w/2)*Math.cos(a2r),
         sy = yc+(h/2)*Math.sin(a2r),
         ex = xc+(w/2)*Math.cos(a1r),
         ey = yc+(h/2)*Math.sin(a1r);
//printf(1,"sx:%f, sy:%f, ex:%f, ey:%f\n",["sequence",sx,sy,ex,ey]);
        ctx.beginPath();
//  ctx.arc(xc,ch-yc,w/2,angle1*Math.PI/180,angle2*Math.PI/180);
//  ctx.ellipse(xc,ch-yc,w/2,h/2,0,angle1*Math.PI/180,angle2*Math.PI/180);
        ctx.ellipse(xc,ch,w/2,h/2,0,a1r,a2r);
        if (flags & XPG_SECTOR) {
//          ctx.lineTo(sx,sy,xc,yc)
            ctx.lineTo(xc,yc)
//          ctx.lineTo(sx,sy)
            ctx.lineTo(ex,ey)
        } else if (flags & XPG_CHORD) {
//          ctx.lineTo(sx,sy)
            ctx.lineTo(ex,ey)
        }   
        if (flags & XPG_FILLED) {
//      if (false) {
            ctx.fill();
        } else {
            ctx.stroke();
        }
//      if (style!=-1) { gCanvasSetLineStyle(canvas,pstyle); }
        if (style!=-1) { gSetInt(canvas,"LINESTYLE",pstyle); }
//      if (width!=-1) { gCanvasSetLineWidth(canvas,pwidth); }
        if (width!=-1) { gSetInt(canvas,"LINEWIDTH",pwidth); }
        if (colour!=-1) { 
            ctx.strokeStyle = pss;
            ctx.fillStyle = pfs;
        }
    }
}

////cdCanvasSector(cdCanvas canvas, atom xc, yc, w, h, angle1, angle2) 
//function gCanvasSector(ctx, xc, yc, w, h, angle1, angle2, sector=true) {
//  let ch = ctx.canvas.clientHeight,
//      fs = ctx.fillStyle;
//  ctx.fillStyle = ctx.strokeStyle;
//  ctx.beginPath();
////void ctx.arc(x, y, radius, startAngle, endAngle [, counterclockwise]);
////void ctx.ellipse(x, y, radiusX, radiusY, rotation, startAngle, endAngle [, counterclockwise]);
////    ctx.arc(xc,ch-yc,w/2,angle1*Math.PI/180,angle2*Math.PI/180);
//  ctx.ellipse(xc,ch-yc,w/2,h/2,0,(360-angle2)*Math.PI/180,(360-angle1)*Math.PI/180);
//  if (sector) { ctx.lineTo(xc,ch-yc); } // else chord
//  ctx.fill();
//  ctx.fillStyle = fs;
//}
//
//function gCanvasChord(ctx, xc, yc, w, h, angle1, angle2) {
//  cdCanvasSector(ctx, xc, yc, w, h, angle1, angle2, false);
//}

//function gCanvasCircle(/*cdCanvas*/ canvas, /*atom*/ x, y, r, /*boolean*/ filled=false) {
function gCanvasCircle(/*gdx*/ canvas, /*atom*/ xc, yc, radius, /*boolean*/ filled=false, /*integer*/ style=-1, width=-1, /*atom*/ colour=-1) {
    let /*atom*/ diameter = radius*2;
    gCanvasArc(canvas,xc,yc,diameter,diameter,0,360,filled,style,width,colour);
}
////    if (filled) {
////        cdCanvasSector(ctx,x,y,r,r,0,360);
////    } else {
//      gCanvasArc(canvas,x,y,r,r,0,360,filled);
////    }
//}

//function cdCanvasBegin(ctx, mode) {
//  ctx.beginPath();
//  ctx["MODE"] = mode;
//}
//
//function cdCanvasClear(ctx) {
////    ctx.fillStyle = "white";
////3/10/21: (bust)
////    if (ctx.canvas) { ctx = ctx.canvas; }
//  let fs = ctx.fillStyle,
//      bs = ctx.backGround;
//  if (bs) { ctx.fillStyle = bs; }
////if (ctx.canvas) {
////    ctx.fillRect(0,0,ctx.canvas.clientWidth,ctx.canvas.clientHeight);
//  ctx.fillRect(0,0,ctx.canvas.width,ctx.canvas.height);
////    ctx.fillRect(0,0,ctx.clientWidth,ctx.clientHeight);
////    ctx.canvas.fillRect(0,0,ctx.canvas.clientWidth,ctx.canvas.clientHeight);
////}
////    ctx.fillRect(0,0,ctx.clientWidth,ctx.clientHeight);
//  ctx.fillStyle = fs;
//}
//
//function cdCanvasEnd(ctx) {
//  let mode = ctx["MODE"];
//  if (mode === CD_FILL) {
//      let fs = ctx.fillStyle;
//      ctx.fillStyle = ctx.strokeStyle;
//      ctx.fill();
//      ctx.fillStyle = fs;
//  } else if (mode === CD_OPEN_LINES) {
//      ctx.stroke();
//  } else if (mode === CD_CLOSED_LINES) {
//      ctx.closePath();
//      ctx.stroke();
//  } else {
//      crash("unsupported mode");
//  }
//}

//function cdCanvasFlush() {}

//DEV we mighht still need this, via attributes
//function cdCanvasFont(/*cdCanvas*/ ctx, /*nullable_string*/ font, /*integer*/ style, /*integer*/ size) {
////CD_PLAIN (0), CD_BOLD (1), CD_ITALIC (2), CD_UNDERLINE (4) and CD_STRIKEOUT (8).
////pGUI also provides the constant CD_BOLD_ITALIC (3) for convenience.
////ctx.font = "bold 48px serif";
////ctx.font = "50px serif";
//  if (font === "Courier") {
//      font = "courier";
//  } else if (font === "Times") {
//      font = "serif";
//  } else if (font === "Helvetica" ||
//             font === "Calibri") {
//      font = "sans-serif";
//  } else {
//      crash("cdCanvasFont(font="+font+") not supported"); // placeholder??
//  }       
//  if (style === CD_PLAIN) {
//      style = "";
//  } else if (style === CD_BOLD) {
//      style = "bold ";
//  } else if (style === CD_ITALIC) {
//      style = "italic ";
//  } else if (style === CD_BOLD_ITALIC) {
//      style = "bold italic ";
//  } else {
//      crash("cdCanvasFont(style="+style+") not supported"); // placeholder
//  }
//  if (size>=0) {
////        crash("cdCanvasFont(size="+size+") not supported (must use -ve pixels)");
//      size = size/12 + "em ";
//  } else {
//      size = -size + "px ";
//  }
////    size = -size;
////    font = style + size + "px " + font;
//  font = style + size + font; // eg "bold 12px sans-serif"
//  ctx.font = font;
////    puts(1,"cdCanvasFont() not yet supported..\n"); // placeholder
//}

//function cdCanvasGetSize(id) {
////    let w = id.canvas.width,
////        h = id.canvas.height;
//  if (id.canvas) { id = id.canvas; }
//  let w = id.width,
//      h = id.height;
//  // (conversion to mm tbd...)
//  return ["sequence", w, h];
//}

//DEV we may want to retrieve a(lpha) as well, see recent mods to pGUI.e
//function cdCanvasGetImageRGB(/*cdCanvas*/ ctx, /*atom*/ x, y, w, h) {
//  let /*integer*/ l = w*h,
////    let /*integer*/ l = (w-x)*(h-y),
//      /*atom*/ r = repeat(0,l),
//               g = repeat(0,l),
//               b = repeat(0,l),
//         imgdata = ctx.getImageData(x, y, w, h),
//              dl = imgdata.data.length,
//             dtx = 0;
//  for (let i = 1; i <= l; i += 1) {
//      r[i] = imgdata.data[dtx];
//      g[i] = imgdata.data[dtx+1];
//      b[i] = imgdata.data[dtx+2];
//      dtx += 4;
//  }
////getImageData()??
////createImageData()??
////putImageData()??
////--24/10/21..
////    iup_init_cd()
////--?pR
////--?{xcdCanvasGetImageRGB,canvas,x,y,w,h}
////    c_proc(xcdCanvasGetImageRGB, {canvas, pR, pG, pB, x, y, w, h})
////--?pR
////    sequence r = peek({pR, w*h}),
////             g = peek({pG, w*h}),
////             b = peek({pB, w*h})
////    free({pR,pG,pB})
//  return ["sequence",r,g,b];
//}

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

global procedure cdCanvasPutImageRectRGBA(cdCanvas canvas, atom iw, ih, sequence rgba, 
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

//function gCanvasLine(ctx, x1, y1, x2, y2, style=-1, width=-1) {
function gCanvasLine(canvas, x1, y1, x2, y2, style=-1, width=-1, colour=-1) {
//DEV
//  if (atom(ctx)) {
    let pstyle, pwidth, pss, pfs;
//  if (style!=-1) { pstyle = gCanvasGetLineStyle(canvas); gCanvasSetLineStyle(canvas,style); }
    if (style!=-1) { pstyle = gGetInt(canvas,"LINESTYLE"); gSetInt(canvas,"LINESTYLE",style); }
//  if (width!=-1) { pwidth = gCanvasGetLineWidth(canvas); gCanvasSetLineWidth(canvas,width); }
    if (width!=-1) { pwidth = gGetInt(canvas,"LINEWIDTH"); gSetInt(canvas,"LINEWIDTH",width); }
    let ctx = canvas.ctx;
    if (colour!=-1) { 
//      gCanvasSetForeground(canvas, colour);
        if (integer(colour)) { colour = sprintf("#%06x",colour); }
        pss = ctx.strokeStyle;
        pfs = ctx.fillStyle;
        ctx.fillStyle = colour;
        ctx.strokeStyle = colour;
    }
//function gCanvasSetLineStyle(/*cdCanvas*/ ctx, /*integer*/ style) {
//    XPG_CONTINUOUS = 0,
//    XPG_DASHED = 1,
//    XPG_DOTTED = 2,
//    XPG_DASH_DOT = 3,
//    XPG_DASH_DOT_DOT = 4,
//  if (style!==XPG_CONTINUOUS) { ctx.lineWidth = 1; }
//function gCanvasGetLineStyle(/*cdCanvas*/ ctx) {
//function gCanvasSetLineWidth(/*cdCanvas*/ ctx, /*atom*/ width) {
//function gCanvasGetLineWidth(/*cdCanvas*/ ctx) {
//  let h = ctx.canvas.clientHeight;
    ctx.beginPath();
//  ctx.moveTo(x1, h-y1);
    ctx.moveTo(x1, y1);
//  ctx.lineTo(x2, h-y2);
    ctx.lineTo(x2, y2);
    ctx.stroke();
//  if (style!=-1) { gCanvasSetLineStyle(canvas,pstyle); }
    if (style!=-1) { gSetInt(canvas,"LINESTYLE",pstyle); }
//  if (width!=-1) { gCanvasSetLineWidth(canvas,pwidth); }
    if (width!=-1) { gSetInt(canvas,"LINEWIDTH",pwidth); }
    if (colour!=-1) { 
        ctx.fillStyle = pfs;
        ctx.strokeStyle = pss;
    }
}

//function cdCanvasMark(ctx, x, y) {
//  let msize = ctx.MarkSize || 10;
////    cdCanvasBox(ctx, x, y, x+msize, y+msize);
//  cdCanvasBox(ctx, x, x+msize, y, y+msize);
///*
//                      elsif dms="PLUS" then
//                          cdCanvasLine(cd_canvas,x2,y2-3,x2,y2+3)
//                          cdCanvasLine(cd_canvas,x2-3,y2,x2+3,y2)
//                      else --default/x
//                          cdCanvasLine(cd_canvas,x2-3,y2-3,x2+3,y2+3)
//                          cdCanvasLine(cd_canvas,x2-3,y2+3,x2+3,y2-3)
//*/
//}
//
//function cdCanvasMarkSize(ctx, mark_size) {
//  let prev = ctx.MarkSize || 10;
//  if (mark_size !== CD_QUERY) {
//      ctx.MarkSize = mark_size;
//  }
//  return prev;
//}
//
//function cdCanvasMarkType(ctx, mark_type) {
//  let prev = ctx.MarkType || CD_BOX;
//  if (mark_type !== CD_QUERY) {
//      ctx.MarkType = mark_type;
//  }
//  return prev;
//}

function gCanvasCubicBezier(/*gdx*/ canvas, /*atom*/ x1, y1, xc1, yc1, xc2, yc2, x2, y2, colour=-1) {
    let ctx = canvas.ctx,
        pss = ctx.strokeStyle,
        pfs = ctx.fillStyle;
    if (colour!=-1) { 
//      gCanvasSetForeground(canvas, colour);
        if (integer(colour)) { colour = sprintf("#%06x",colour); }
        ctx.fillStyle = colour;
        ctx.strokeStyle = colour;
    }
    ctx.beginPath();
    ctx.moveTo(x1, y1);
    ctx.bezierCurveTo(xc1, xc2, yc1, yc2, x2, y2);
//  ctx.quadraticCurveTo(cx, cy, x2, y2); // (maybe..)
    ctx.stroke();
    
    ctx.strokeStyle = pss;
    ctx.fillStyle = pfs;
}

function gCanvasQuadBezier(/*gdx*/ canvas, /*atom*/ x1, y1, cx, cy, x2, y2, /*integer*/ colour=-1) {
    gCanvasCubicBezier(canvas,x1,y1,cx,cy,cx,cy,x2,y2,colour);
}

function gRotatePolygon(/*sequence*/ poly, /*object*/ angle) {
    let /*atom*/ cx, cy;
    if (sequence(angle)) {
        [,angle,cx,cy] = angle;
    } else {
        [,cx,cy] = poly[1];
    }
    if (angle !== 0) {
        let /*integer*/ lp = length(poly);
        let /*sequence*/ res = repeat(0,lp);
        let /*atom*/ r = angle*XPG_DEG2RAD,
                     s = sin(r),
                     c = cos(r)
        for (let i = 1; i <= lp; i += 1) {
            let /*sequence*/ pi = poly[i];
            let /*integer*/ lpi = length(pi);
            if (lpi === 0) {
                res[i] = ["sequence"];
            } else {
                let /*sequence*/ ri = repeat(0,lpi);
                for (let j = 1; j <= lpi-1; j += 2) {
                    let /*integer*/ k = j+1;
                    let /*atom*/ x = pi[j] - cx,
                                 y = pi[k] - cy;
                    ri[j] = x*c - y*s + cx;
                    ri[k] = x*s + y*c + cy;
                }
                res[i] = ri;
            }
        }
        return res;
    }
    return poly;
}

function gCanvasPolygon(/*gdx*/ canvas, /*sequence*/ poly, /*bool*/ bFilled=true, /*integer*/ colour=-1, style=-1, width=-1) {
    let ctx = canvas.ctx,
        pstyle, pwidth, pss, pfs;
//  if (style!=-1) { pstyle = gCanvasGetLineStyle(canvas); gCanvasSetLineStyle(canvas,style); }
    if (style!=-1) { pstyle = gGetInt(canvas,"LINESTYLE"); gSetInt(canvas,"LINESTYLE",style); }
//  if (width!=-1) { pwidth = gCanvasGetLineWidth(canvas); gCanvasSetLineWidth(canvas,width); }
    if (width!=-1) { pwidth = gGetInt(canvas,"LINEWIDTH"); gSetInt(canvas,"LINEWIDTH",width); }
    if (colour!=-1) { 
//      gCanvasSetForeground(canvas, colour);
        if (integer(colour)) { colour = sprintf("#%06x",colour); }
        pss = ctx.strokeStyle,
        pfs = ctx.fillStyle,
        ctx.fillStyle = colour;
        ctx.strokeStyle = colour;
    }
    ctx.beginPath();
    let l = length(poly), start = 1;
    for (let i = 1; i <= l; i += 1) {
        let pi = poly[i],
           lpi = length(pi);
        if (lpi === 0) { // (new loop)
            start = i+1;
        } else {
            let j = (i===l || length(poly[i+1]) === 0) ? start : i+1;
            let iX = pi[1],
                iY = pi[2],
                aX = (lpi > 2) ? pi[3] : 0,
                aY = (lpi > 2) ? pi[4] : 0,
                bX = (lpi > 4) ? pi[5] : aX,
                bY = (lpi > 4) ? pi[6] : aY,
                [,jX,jY] = poly[j];
            if (i === start) {
                ctx.closePath();
                ctx.moveTo(iX,iY);
            }
            if (lpi === 2) {    // straight line
                ctx.lineTo(jX, jY);
            } else {
                ctx.bezierCurveTo(aX, aY, bX, bY, jX, jY);
            }
        }
    }
    if (bFilled) {
        ctx.fill('evenodd');
    } else {
        ctx.stroke();
    }
    ctx.strokeStyle = pss;
    ctx.fillStyle = pfs;
//  if (style!=-1) { gCanvasSetLineStyle(canvas,pstyle); }
    if (style!=-1) { gSetInt(canvas,"LINESTYLE",pstyle); }
//  if (width!=-1) { gCanvasSetLineWidth(canvas,pwidth); }
    if (width!=-1) { gSetInt(canvas,"LINEWIDTH",pwidth); }
    if (colour!=-1) {
        ctx.fillStyle = pfs;
        ctx.strokeStyle = pss;
    }
}

function gCanvasPixel(ctx,x,y,colour) {
//DEV
//  if (atom(ctx)) {
    if (integer(colour)) { colour = sprintf("#%06x",colour&0xFFFFFF); }
//24/7/22 put back for gaussian primes... (much sharper)
//      (no performance difference to brownian tree, death star...)
    let fs = ctx.fillStyle;
    ctx.fillStyle = colour;
//  ctx.fillRect(x,ctx.canvas.clientHeight-y,1,1);
    ctx.fillRect(x,y,1,1);
    ctx.fillStyle = fs;

//  ctx.strokeStyle = colour;
//  ctx.strokeRect(x,ctx.canvas.clientHeight-y,1,0);
}

function gCanvasRect(/*gdx*/ canvas, /*atom*/ xmin, xmax, ymin, ymax, /*bool*/ bFill=false, /*integer*/ rc=0, style=-1, width=-1, /*atom*/ colour=-1) {
//  puts(1,"gCanvasRect incomplete...\n");  --DEV style/width not yet...
//  printf(1,"gCanvasRect(%d,%d,%d,%d)\n",["sequence", xmin, xmax, ymin, ymax]);
//DEV see cdCanvasBox...
//  let h = ctx.canvas.clientHeight;
//  let h = ctx.canvas? ctx.canvas.clientHeight : ctx.clientHeight;
//  let h = canvas.canvas? canvas.canvas.clientHeight : canvas.clientHeight;
//  ymax = ymin-ymax;
//  ymin = h-ymin;
//  ctx.strokeRect(xmin,h-ymin,xmax,h-ymax);
//  ctx.strokeRect(xmin+1,ymin+1,xmax-xmin-1,ymax-2);
//  ctx.strokeRect(xmin,ymin,xmax-xmin,ymax);
    let ctx = canvas.ctx,
        pss = ctx.strokeStyle,
        pfs = ctx.fillStyle;
    if (colour!=-1) { 
//      gCanvasSetForeground(canvas, colour);
        if (integer(colour)) { colour = sprintf("#%06x",colour); }
        ctx.fillStyle = colour;
        ctx.strokeStyle = colour;
    }
    if (rc) {
        rc /= 2;
        const r90 = 90*Math.PI/180,
              r180 = 180*Math.PI/180,
              r270 = 270*Math.PI/180;
        ctx.beginPath();
        ctx.moveTo(xmin+rc,ymin);
        ctx.lineTo(xmax-rc,ymin);                       // top
        ctx.ellipse(xmax-rc,ymin+rc,rc,rc,0,r270,0);    // top right cnr
        ctx.lineTo(xmax,ymin+rc);                       // right
        ctx.ellipse(xmax-rc,ymax-rc,rc,rc,0,   0,r90);  // btm right cnr
        ctx.lineTo(xmin+rc,ymax);                       // btm (r->l)
        ctx.ellipse(xmin+rc,ymax-rc,rc,rc,0, r90,r180); // btm left cnr
        ctx.lineTo(xmin,ymin+rc);                       // left 
        ctx.ellipse(xmin+rc,ymin+rc,rc,rc,0,r180,r270); // top left cnr
        if (bFill) {
            ctx.fill();
        } else {
            ctx.stroke();
        }
    } else {
        xmax = xmax-xmin;
        ymax = ymax-ymin;
        if (bFill) {
            ctx.fillRect(xmin,ymin,xmax,ymax);
        } else {
            ctx.strokeRect(xmin,ymin,xmax,ymax);
        }
    }
    ctx.strokeStyle = pss;
    ctx.fillStyle = pfs;
}

//function cdCanvasBox(/*cdCanvas*/ ctx, /*atom*/ xmin, xmax, ymin, ymax) {
////    puts(1,"cdCanvasBox("+xmin+","+xmax+","+ymin+","+ymax+")\n");
////DEV Probably still not quite right. Need to get my head around pixel co-ord mappings properly.
////      (write that demo/PGUI/cdCanvasBox.exw demo)
//  let x = xmin,
//   ccch = ctx.canvas.clientHeight,
////        y = ccch-ymin,
//      y = ccch-ymax-1,
////        h = ymin-ymax-1,
//      h = ymax-ymin+1,
////    ymin = h-ymin;
//      w = xmax-xmin+1;
////    ctx.strokeRect(xmin,h-ymin,xmax,h-ymax);
////    ctx.fillRect(xmin+1,ymin+1,xmax-xmin-1,ymax-2);
//  ctx.fillRect(x,y,w,h);
//}

//function cdCanvasRoundedBox(/*cdCanvas*/ canvas, /*atom*/ xmin, xmax, ymin, ymax, w, h) {
//  // first draw the filled rectangle with straight-clipped corners (aka an octagon)
//  cdCanvasBegin(canvas,CD_FILL);
//  cdCanvasVertex(canvas,xmin+w,ymin);
//  cdCanvasVertex(canvas,xmax-w,ymin);
//  cdCanvasVertex(canvas,xmax,ymin+h);
//  cdCanvasVertex(canvas,xmax,ymax-h);
//  cdCanvasVertex(canvas,xmax-w,ymax);
//  cdCanvasVertex(canvas,xmin+w,ymax);
//  cdCanvasVertex(canvas,xmin,ymax-h);
//  cdCanvasVertex(canvas,xmin,ymin+h);
//  cdCanvasEnd(canvas);
//  // then round/fill in the corners using cdCanvasSector
////    cdCanvasSector(cdCanvas canvas, atom xc, yc, w, h, angle1, angle2) 
////    cdCanvasSetForeground(cddbuffer, CD_RED)
//  cdCanvasSector(canvas,xmin+w,ymin+h,w*2,h*2,180,270);   // btm left
//  cdCanvasSector(canvas,xmax-w,ymin+h,w*2,h*2,270,0);     // btm right
//  cdCanvasSector(canvas,xmin+w,ymax-h,w*2,h*2,90,180);    // top left
//  cdCanvasSector(canvas,xmax-w,ymax-h,w*2,h*2,0,90);      // top right
//}

//function cdCanvasRoundedRect(/*cdCanvas*/ canvas, /*atom*/ xmin, xmax, ymin, ymax, w, h) {
//  // first draw four edges, not-quite-meeting
//  cdCanvasLine(canvas,xmin+w,ymin,xmax-w,ymin);
//  cdCanvasLine(canvas,xmax,ymin+h,xmax,ymax-h);
//  cdCanvasLine(canvas,xmax-w,ymax,xmin+w,ymax);
//  cdCanvasLine(canvas,xmin,ymax-h,xmin,ymin+h);
//  // then round/connect the corners using cdCanvasArc
////    cdCanvasArc(cdCanvas canvas, atom xc, yc, w, h, a1, a2) 
////    cdCanvasSetForeground(cddbuffer, CD_RED)
//  cdCanvasArc(canvas,xmin+w,ymin+h,w*2,h*2,180,270);
//  cdCanvasArc(canvas,xmax-w,ymin+h,w*2,h*2,270,0);
//  cdCanvasArc(canvas,xmin+w,ymax-h,w*2,h*2,90,180);
//  cdCanvasArc(canvas,xmax-w,ymax-h,w*2,h*2,0,90);
//}

function gGetTextExtent(/*gdx*/ id, /*sequence*/ text, /*bool*/ bSumHeights = true) {
    if (string(text)) { text = ["sequence",text]; }

//  const am = new RegExp("&","g"),
//            lt = new RegExp("[<]","g"),
//            gt = new RegExp("[>]","g"),
    const lt = length(text),
          sp = new RegExp("[ ]","g");
//            lf = new RegExp("\\n","g");
//      text = text.replace(am,"&amp;")
//                 .replace(lt,"&lt;")
//                 .replace(gt,"&gt;")
//                 .replace(sp,"&ensp;")
    let w = 0, h = 0;
    for (let i = 1; i <= lt; i += 1) {
        let ti = text[i].replace(sp,"_"),
//          tm = id.measureText(ti),
            tm = id.ctx.measureText(ti),
/*
actualBoundingBoxAscent: 7
actualBoundingBoxDescent: 0
actualBoundingBoxLeft: 1
actualBoundingBoxRight: 10
fontBoundingBoxAscent: 9
fontBoundingBoxDescent: 2
width: 9.4384765625
tm.actualBoundingBoxRight
*/
            wi = tm.actualBoundingBoxLeft + tm.actualBoundingBoxRight,
            hi = tm.fontBoundingBoxAscent + tm.fontBoundingBoxDescent;
//  return ["sequence",tm.actualBoundingBoxRight,tm.fontBoundingBoxAscent+tm.fontBoundingBoxDescent];
        w = max(w,wi);
        if (bSumHeights) {
            h += hi;
        } else {
            h = hi;
        }
    }
    return ["sequence",w,h];
}

//let gti = true;

//function gCanvasText(/*cdCanvas*/ ctx, /*atom*/ x, y, /*string*/ text, /*integer*/ align, /*atom*/ angle, colour=-1) {
//gCanvasText(gdx canvas, atom x, y, string text, integer align=XPG_E, object angle=0, atom colour=-1, style=-1)
//       gCanvasText(gdx canvas, atom x, y, string text, integer align=XPG_E, object angle=0, colour=-1, integer style=-1)
function gCanvasText(/*gdx*/ canvas, /*atom*/ x, y, /*string*/ text, /*integer*/ align=XPG_E, /*object*/ angle=0, /*atom*/ colour=-1, style=-1) {
//  if (gti) { gti = false; puts(1,"gCanvasText incomplete (align/angle)...\n") }
    let ctx = canvas.ctx, pcolour, oldfont;
//  fs, td;
//  let h = ctx.canvas.clientHeight
//      angle = ctx.textOrientation || 0;
//  if (!string(text)) { text = String.fromCodePoint(...text.slice(1)); }
    assert(string(text));
    if (colour !== -1) {
//      pcolour = gCanvasGetForeground(canvas);
        pcolour = gGetInt(canvas, "FGCLR");
//      gCanvasSetForeground(canvas, colour);
        gSetInt(canvas, "FGCLR", colour);
    }
    if (style !== -1) {
        oldfont = ctx.font; // (restored later)
        let newfont = oldfont, space;
        while ((space = newfont.indexOf(' ')) !== -1) {
            if (space) { // (just skip any leading spaces)
                // discard first word[s] of bold|italic
                let fw = newfont.slice(0,space).toLowerCase();
                if (fw !== "bold" && fw !== "italic") { break; }
            }
            newfont = newfont.slice(space+1);
        }
        // XPG_NORMAL=0, XPG_BOLD=1, XPG_ITALIC=2, XPG_BOLDITALIC=3
        newfont = ["","bold ","italic ","bold italic "][style] + newfont;
        ctx.font = newfont;
    }
    let rot_pt = XPG_NW;
    if (sequence(angle)) {
        [,rot_pt,angle] = angle
        if (rot_pt === XPG_CENTER) {rot_pt = XPG_C; } // (as per docs)
    }
    if (align === XPG_CENTER) {align = XPG_C; } // (as per docs)
    if (angle) {
//      if (align!=rot_pt) --??
        // adjust x,y to rot_pt
        let [,w,h] = gGetTextExtent(canvas,text),
            a = [0,1,-1][align>>2],
            r = [0,-1,1][rot_pt>>2];
        x += ((r-a)/2)*w;
/*
let xx = x + ((r-a)/2)*w;
        if (align & XPG_W) {
            if (rot_pt & XPG_W) {
                x += w;
            } else if (!(rot_pt & XPG_E)) {
                x += w/2;
            }
        } else if (align & XPG_E) {
            if (rot_pt & XPG_E) {
                x -= w;
            } else if (!(rot_pt & XPG_W)) {
                x -= w/2;   
            }
        } else { // align = XPG_C[+N|S]
            if (rot_pt & XPG_E) {
                x -= w/2;
            } else if (rot_pt & XPG_W) {
                x += w/2;   
            }
        }
*/
        a = [0,1,-1][align&3],
        r = [0,-1,1][rot_pt&3];
        y += ((r-a)/2)*h;
/*
let yy = y + ((r-a)/2)*h;
        if (align & XPG_N) {
            if (rot_pt & XPG_S) {
                y += h;
            } else if (!(rot_pt & XPG_N)) {
                y += h/2;
            }
        } else if (align & XPG_S) {
            if (rot_pt & XPG_N) {
                y -= h;
            } else if (!(rot_pt & XPG_S)) {
                y -= h/2;   
            }
        } else { // align = XPG_C[+W|E]
            if (rot_pt & XPG_N) {
                y -= h/2;
            } else if (rot_pt & XPG_S) {
                y += h/2;   
            }
        }
gCanvasLine(graph,x-4,y-4,x+4,y+4,-1,-1,XPG_GREEN);
gCanvasLine(graph,x-4,y+4,x+4,y-4,-1,-1,XPG_GREEN);
gCanvasLine(graph,xx-2,yy-2,xx+2,yy+2,-1,-1,XPG_WHITE);
gCanvasLine(graph,xx-2,yy+2,xx+2,yy-2,-1,-1,XPG_WHITE);
*/
        ctx.save();
//      ctx.textAlign="center";
//      ctx.textAlign="left";
//      ctx.textBaseline="middle";
//      ctx.textBaseline="top";
        // (as per docs, rot_pt is logical inverse of align)
        let tA = ["center","right","left"][rot_pt >> 2],
            tB = ["middle","bottom","top"][rot_pt & 3];
        ctx.textAlign = tA;
        ctx.textBaseline = tB;
//      ctx.textAlign = ["center","right","left"][rot_pt >> 2];
//      ctx.textBaseline = ["middle","bottom","top"][rot_pt & 3];
//      ctx.translate(x,h-y);
        ctx.translate(x,y);
        ctx.rotate((Math.PI/180)*angle);
//      ctx.textAlign = tA;
//      ctx.textBaseline = tB;
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
        let tA = ["center","left","right"][align >> 2],
            tB = ["middle","top","bottom"][align & 3];
        ctx.textAlign = tA;
        ctx.textBaseline = tB;
        ctx.fillText(text, x, y);
    }
//  if (colour !== -1) { gCanvasSetForeground(canvas, pcolour); }
    if (colour !== -1) { gSetInt(canvas, "FGCLR", pcolour); }
    if (style !== -1) { ctx.font = oldfont; }
}

//function glCanvasSpecialText(/*gdx*/ cd_canvas, /*integer*/ w, h, fontsize, /*string*/ text) {
//  let textCtx = document.createElement("canvas").getContext("2d");
//  textCtx.canvas.width = w;
//  textCtx.canvas.height = h;
////    textCtx.font = Math.floor(fontsize*1.15) + "px monospace";
//  textCtx.font = Math.floor(fontsize*1.25) + "px courier";
//  textCtx.textAlign = "center";
//  textCtx.textBaseline = "middle";
//  textCtx.fillStyle = "black";
//  textCtx.clearRect(0, 0, w, h);
////    textCtx.direction = "ltr";
////    textCtx.direction = "rtl";
//  textCtx.fillText(text, w / 2, h / 2);
////    textCtx.rotate(285);
////    textCtx.setTransform(1,2,3,4,5,6);
//  return textCtx.canvas;
//}

//function cdCanvasSetTextAlignment(/*cdCanvas*/ ctx, /*integer*/ alignment) {
////DEV (oh, could get messy... see diagram in phix.htm)
////Defines the vertical and horizontal alignment of a text as: CD_NORTH,
////CD_SOUTH, CD_EAST, CD_WEST, CD_NORTH_EAST, CD_NORTH_WEST, CD_SOUTH_EAST, 
////CD_SOUTH_WEST, CD_CENTER, CD_BASE_LEFT, CD_BASE_CENTER, or CD_BASE_RIGHT. 
////    CD_NORTH = 0,
////    CD_SOUTH = 1,
////    CD_EAST = 2,
////    CD_WEST = 3,
////    CD_NORTH_EAST = 4,
////    CD_NORTH_WEST = 5,
////    CD_SOUTH_EAST = 6,
////    CD_SOUTH_WEST = 7,
////    CD_CENTER = 8,
////    CD_BASE_LEFT = 7, (was 9)
////    CD_BASE_CENTER = 1,  (was 10)
////    CD_BASE_RIGHT = 6, (was 11)
////Returns the previous value. 
////Default value: CD_BASE_LEFT. -- (ok!)
////    let ta = ctx.textAlign || ctx.style.textAlign,
//  let ta, tb;
//  if (alignment === CD_CENTER) {
//      ta = "center";
//      tb = "middle";
//  } else if (alignment === CD_NORTH) {
//      ta = "center";
//      tb = "top";
//  } else if (alignment === CD_NORTH_EAST) {
//      ta = "right";
//      tb = "top";
//  } else if (alignment === CD_NORTH_WEST) {
//      ta = "left";
//      tb = "top";
//  } else if (alignment === CD_SOUTH) {
//      ta = "center";
//      tb = "bottom";
//  } else if (alignment === CD_SOUTH_EAST) {
//      ta = "right";
//      tb = "bottom";
//  } else if (alignment === CD_SOUTH_WEST) {
//      ta = "left";
//      tb = "bottom";
//  } else if (alignment === CD_WEST) {
//      ta = "left";
//      tb = "middle";
//  } else if (alignment === CD_EAST) {
//      ta = "right";
//      tb = "middle";
//  } else {
//      crash("cdCanvasTextAlignment("+alignment+") not yet supported\n"); // placeholder
//  }
//  ctx.textAlign = ta;
//  ctx.textBaseline = tb;
//}

//function cdCanvasGetTextAlignment(canvas) {
//  let ta = ctx.textAlign,
//      tb = ctx.textBaseline,
//      /*integer*/ alignment = -1;
////ctx.textAlign = "left" || "right" || "center" || "start" || "end";
////ctx.textBaseline = "top" || "hanging" || "middle" || "alphabetic" || "ideographic" || "bottom";
//  if (ta === "center") {
//      if (tb === "middle") {
//          alignment = CD_CENTER;
//      } else if (tb === "top") {
//          alignment = CD_NORTH;
//      } else if (tb === "bottom") {
//          alignment = CD_SOUTH;
//      }
//  } else if (ta === "start") {
//      if (tb === "alphabetic") {
//          alignment = CD_BASE_LEFT;
//      }
//  } else if (ta === "left") {
//      if (tb === "middle") {
//          alignment = CD_WEST;
//      } else if (tb === "top") {
//          alignment = CD_NORTH_WEST;
//      } else if (tb === "bottom") {
//          alignment = CD_SOUTH_WEST;
//      }
//  } else if (ta === "right") {
//      if (tb === "middle") {
//          alignment = CD_EAST;
//      } else if (tb === "top") {
//          alignment = CD_NORTH_EAST;
//      } else if (tb === "bottom") {
//          alignment = CD_SOUTH_EAST;
//      }
//  }
//  if (alignment===-1) {
//      crash("cdCanvasGetTextAlignment("+ta+","+tb+") not yet supported\n"); // placeholder
//  }
//  return alignment;
//}

//function cdCanvasGetTextSize(/*cdCanvas*/ ctx, /*string*/ text) {
////    const am = new RegExp("&","g"),
////              lt = new RegExp("[<]","g"),
////              gt = new RegExp("[>]","g"),
//  const sp = new RegExp("[ ]","g");
////              lf = new RegExp("\\n","g");
////        text = text.replace(am,"&amp;")
////                   .replace(lt,"&lt;")
////                   .replace(gt,"&gt;")
////                   .replace(sp,"&ensp;")
//  text = text.replace(sp,"_");
//  let tm = ctx.measureText(text),
///*
//actualBoundingBoxAscent: 7
//actualBoundingBoxDescent: 0
//actualBoundingBoxLeft: 1
//actualBoundingBoxRight: 10
//fontBoundingBoxAscent: 9
//fontBoundingBoxDescent: 2
//width: 9.4384765625
//tm.actualBoundingBoxRight
//*/
//      w = tm.actualBoundingBoxLeft + tm.actualBoundingBoxRight,
//      h = tm.fontBoundingBoxAscent + tm.fontBoundingBoxDescent;
////    return ["sequence",tm.actualBoundingBoxRight,tm.fontBoundingBoxAscent+tm.fontBoundingBoxDescent];
//  return ["sequence",w,h];
//}

//function cdCanvasSetTextOrientation(/*cdCanvas*/ ctx, /*atom*/ angle) {
//  // (applies to cdCanvasText() above only)
//  ctx.textOrientation = -angle;
//}
//
//function cdCanvasGetTextOrientation(/*cdCanvas*/ ctx) {
//  // (applies to cdCanvasText() above only)
//  let prev = ctx.textOrientation || 0;
//  return -prev;
//}

//function cdCanvasVectorText(/*cdCanvas*/ ctx, /*atom*/ x, y, /*string*/ text) {
//  puts(1,"cdCanvasVectorText...\n"); // placeholder
//}
//let cdCanvasMultiLineVectorText = cdCanvasVectorText;
//
//function cdCanvasVectorTextDirection(/*cdCanvas*/ canvas, /*integer*/ x1, y1, x2, y2) {
//  puts(1,"cdCanvasVectorTextDirection...\n"); // placeholder
//}
//
//function cdCanvasVectorTextSize(/*cdCanvas*/ ctx, /*atom*/ w, h, /*string*/ text) {
//  puts(1,"cdCanvasVectorTextSize...\n"); // placeholder
//}

//function cdCanvasVertex(ctx, x, y) {
////DEV see cdCanvasBox?
//  let h = ctx.canvas.clientHeight;
////DEV should the first one be a moveTo?
//  ctx.lineTo(x, h-y);
//}

//function cdKillCanvas() {}

function gGraph(/*rtn*/ drid, /*string*/ attributes="", /*sequence*/ args=["sequence"]) {

    function xpg_graph_default_DROP(/*integer*/ rid) { return rid; }

    function redraw_graph(/*gdx*/ graph) {
        let /*rtn*/ drid = gGetHandler(graph,"DRID");
//      let /*sequence*/ sig = get_routine_info(drid,false),
//                  datasets = iff(sig={0,0,"F"}?drid(),
//                             iff(sig={1,1,"FO"}?drid(graph):9/0)),

        // aside: unlike desktop/Phix, no problem with if drid() has no parameters
        let /*sequence*/ datasets = drid(graph);

        // aside: unlike desktop/Phix, attributes are stored directly on graph,
        //        so I could just use eg graph.GRID or graph["GRID"], but I may
        //        as well use gGetAttributes() and friends to force myself to
        //        implement them all correctly/test on day one.
        let /* integer*/ grid = gGetInt(graph,"GRID"), 
                   gridcolour = gGetInt(graph,"GRIDCOLOR"),
              [,width, hight] = gGetAttribute(graph,"SIZE"),
                         dsdx = 1,
                         drop = gGetHandler(graph,"DROP",xpg_graph_default_DROP),
//       xCanvasSetForeground = drop(gCanvasSetForeground),
//       xCanvasSetBackground = drop(gCanvasSetBackground),
                  xCanvasRect = drop(gCanvasRect),
                  xCanvasLine = drop(gCanvasLine),
                  xCanvasText = drop(gCanvasText);
// JavaScript no likey:
         // /now/ it feels safe to shadow the globals:
//       gCanvasSetForeground = xCanvasSetForeground,
//       gCanvasSetBackground = xCanvasSetBackground,
//                gCanvasRect = xCanvasRect,
//                gCanvasLine = xCanvasLine,
//                gCanvasText = xCanvasText;

        // nb: "XTICK" etc may be/get set in the above drid() call.
        let /*atom*/ xtick = gGetDouble(graph,"XTICK"), 
                      xmin = gGetDouble(graph,"XMIN"), 
                      xmax = gGetDouble(graph,"XMAX"), 
                   xmargin = gGetDouble(graph,"XMARGIN",10), 
                   xyshift = gGetDouble(graph,"XYSHIFT"), 
                    xangle = gGetDouble(graph,"XANGLE"), 
                     ytick = gGetDouble(graph,"YTICK"), 
                      ymin = gGetDouble(graph,"YMIN"), 
                      ymax = gGetDouble(graph,"YMAX"), 
                   ymargin = gGetDouble(graph,"YMARGIN",10), 
                   yxshift = gGetDouble(graph,"YXSHIFT"), 
                    yangle = gGetDouble(graph,"YANGLE"),
//                   bgclr = gCanvasGetBackground(graph);
                     bgclr = gGetInt(graph,"BGCLR");

        let /*string*/ title = gGetAttribute(graph,"GTITLE",""), 
                       xname = gGetAttribute(graph,"XNAME",""), 
                       yname = gGetAttribute(graph,"YNAME",""), 
                       xfmt = gGetAttribute(graph,"XTICKFMT","%g"), 
                       yfmt = gGetAttribute(graph,"YTICKFMT","%g"), 
                       mode = gGetAttribute(graph,"MODE",""), 
                       barmode = gGetAttribute(graph,"BARMODE",""), 
                       markstyle = gGetAttribute(graph,"MARKSTYLE","");
//                     fontface = "Helvetica";

        let /*integer*/ marksize = gGetInt(graph,"MARKSIZE"),
                          legend = 0, // (idx to datasets)
                          lx, ly, // (XPG_EAST of the first legend text)
                         xacross = gGetInt(graph,"XACROSS"),
                         yacross = gGetInt(graph,"YACROSS"),
                            xrid = gGetHandler(graph,"XRID"),
                            yrid = gGetHandler(graph,"YRID"),
                            post = gGetHandler(graph,"POST")

//      let /*integer*/ fontstyle = CD_PLAIN, 
//                      fontsize = 9, 
//                      bgclr = IupGetAttributePtr(graph,"BGCOLOR"), 
//                      titlestyle = IupGetInt(graph,"TITLESTYLE",CD_PLAIN), 
//      let /*cdCanvas*/ cd_canvas = IupGetAttributePtr(graph,"CD_CANVAS");

        while (true) {
            let /*sequence*/ ds = $subse(datasets,dsdx), 
                             s = $subse(ds,1);
            if (!string(s)) { break; }
            assert(equal(s,"NAMES"));
            legend = dsdx;
            dsdx += 1;
        }

        // draw title and axis names
//      xCanvasSetForeground(graph,XPG_BLACK);
        gSetInt(graph,"FGCLR",XPG_BLACK);
        if (title!=="") {
            xCanvasText(graph,width/2+4,0,title,XPG_SOUTH);
        }
        if (yname!=="") {
            xCanvasText(graph,34,hight/2,yname,XPG_SOUTH,90);
        }
        if (xname!=="") {
            xCanvasText(graph,width/2,hight-4,xname,XPG_NORTH);
        }
//printf(1,"xacross:%d, yacross:%d\n",["sequence",xacross,yacross])

        // draw the x/y-axis labels and vertical gridlines
        xacross = ((xacross) ? round((0-xmin)/xtick)+1 : 1);
        yacross = ((yacross) ? round((0-ymin)/ytick)+1 : 1);
//printf(1,"xacross:%d, yacross:%d\n",["sequence",xacross,yacross])

        let /*integer*/ vb = barmode === "VERTICAL", 
                        hb = barmode === "HORIZONTAL", 
                        nx = max(round((xmax-xmin)/xtick)+vb,1), 
                        ny = max(round((ymax-ymin)/ytick)+hb,1);
        if (barmode!=="") {
            assert(vb || hb,"invalid BARMODE");
            assert((mode==="") || (mode==="BAR"),"invalid MODE for BARMODE");
            mode = "BAR";
        } else if (mode==="BAR") {
            vb = true;
        }
        if (((markstyle!=="") && (mode!=="MARK")) && (mode!=="MARKLINE")) {
            assert(mode==="","invalid MODE for MARKSTYLE");
            mode = "MARK";
        }
        let /*atom*/ dx = ((width-60)-xmargin)/nx, 
                     dy = ((hight-60)-ymargin)/ny, 
                     vx = 30+xmargin, 
                     vy = 30+ymargin, 
                     x = xmin, 
                     y = ymin;
        for (let i = 1, i$lim = (nx+1)-vb; i <= i$lim; i += 1) { // the vertical lines
            if ((grid && !vb) || (i === xacross)) {
//              xCanvasSetForeground(graph,((i === xacross) ? XPG_BLACK : gridcolour));
                gSetInt(graph,"FGCLR",((i === xacross) ? XPG_BLACK : gridcolour));
//              gCanvasLine(graph,vx,30+ymargin,vx,hight-30);
                xCanvasLine(graph,vx,hight-(30+ymargin),vx,28);
            }
//          xCanvasSetForeground(graph,XPG_BLACK);
            gSetInt(graph,"FGCLR",XPG_BLACK);
            let /*integer*/ align = (xangle === 0) ? XPG_SOUTH :
                                    (xangle === 90) ? XPG_EAST :
//                                  (xangle === -90) ? XPG_WEST;
                                  /*(xangle === -90) ?*/ XPG_WEST;
            let /*string*/ xtext = xrid ? xrid(x) : sprintf(xfmt,x);
            let /*atom*/ tx = vx+dx*vb/2+1,
                         ty = hight-(((25+ymargin)+xyshift)+(yacross-1)*dy);
//printf(1,"ty: %d, ymargin:%d, xyshift:%d, yacross:%d, dy:%d\n",["sequence",ty,ymargin,xyshift,yacross,dy]);
//          xCanvasText(graph,vx+(dx*vb)/2,ty,xtext);
//          xCanvasLine(graph,tx-5,ty-5,tx+5,ty+5,-1,-1,XPG_RED)
//          xCanvasLine(graph,tx-5,ty+5,tx+5,ty-5,-1,-1,XPG_RED)

            xCanvasText(graph,tx,ty,xtext,align,["sequence",XPG_W,xangle]);
//          xCanvasText(graph,tx,ty,xtext,XPG_E,["sequence",XPG_W,xangle]);
//          xCanvasText(graph,tx,ty,xtext,XPG_E,["sequence",XPG_W,xangle]);
//that's the effect we want:
//          xCanvasText(graph,tx,ty,xtext,XPG_W,["sequence",XPG_W,xangle]);
//          xCanvasText(graph,tx,ty,xtext,XPG_W,45);
//          xCanvasText(graph,vx+dx*vb/2+1,hight-ty,xtext,align);
            vx += dx;
            x += xtick;
        }

        for (let i = 1, i$lim = (ny+1)-hb; i <= i$lim; i += 1) { // the horizontal lines
            if ((grid && !hb) || (i===yacross)) {
//              xCanvasSetForeground(graph,((i===yacross) ? XPG_BLACK : gridcolour));
                gSetInt(graph,"FGCLR",((i===yacross) ? XPG_BLACK : gridcolour));
//              xCanvasLine(graph,31+xmargin,vy,width-30,vy);
                xCanvasLine(graph,31+xmargin,hight-vy,width-30,hight-vy);
            }
//          xCanvasSetForeground(graph,XPG_BLACK);
            gSetInt(graph,"FGCLR",XPG_BLACK);
            let /*integer*/ align = (yangle === 0) ? XPG_WEST :
                                    (yangle === 90) ? XPG_NORTH :
                                  /*(yangle === -90) ?*/ XPG_SOUTH;
            let /*string*/ ytext = ((yrid) ? yrid(y) : sprintf(yfmt,y));
            let /*atom*/ tx = ((25+xmargin)+yxshift)+(xacross-1)*dx,
                         ty = hight-((vy+dy*hb/2)+2);
            xCanvasText(graph,tx,ty,ytext,align,yangle)
            vy += dy;
            y += ytick;
        }
        let /*integer*/ lh; // (legend text height, per line)
//DEV does this not want to be drawn last??
        if (legend) {
            let /*sequence*/ legendnames = $subse($subse(datasets,legend),2);
            [, lw, lh] = gGetTextExtent(graph,legendnames,false)
            let /*string*/ legendpos = gGetAttribute(graph,"LEGENDPOS","TOPRIGHT");
            if (legendpos === "XY") {
                [, lx, ly] = gGetIntInt(graph,"LEGENDXY"); // (untested)
            } else {
                if (equal($subse(legendpos,1),0X54)) { // 'T'
                    assert(equal($subss(legendpos,1,3),"TOP"));
                    legendpos = $subss(legendpos,4,-1);
                    ly = 10;
                } else {
                    assert(equal($subss(legendpos,1,6),"BOTTOM"));
                    legendpos = $subss(legendpos,7,-1);
//                  ly = (hight-50)-ll*lh;
                    ly = (hight-50)-lh;
                }
                if (legendpos === "LEFT") {
                    lx = (30+xmargin)+lw;
                } else if (legendpos === "CENTER") {
                    lx = floor(((xmargin+width)+lw)/2);
                } else {
                    assert(legendpos === "RIGHT");
                    lx = width-30;
                }
            }
            if (gGetInt(graph,"LEGENDBOX")) {
//              xCanvasSetForeground(graph,XPG_BLACK)
                gSetInt(graph,"FGCLR",XPG_BLACK)
                let /*integer*/ lxl = (lx-lw)-25, lxr = lx+10, 
//                              lyt = hight-(ly+15), lyb = hight-((ly+ll*lh)+25);
                                lyt = ly+15, lyb = ly+lh+25;
                xCanvasRect(graph,lxl,lxr,lyt,lyb,true);
            }
        }
        // and finally draw/plot the points!
        let /*atom*/ w = dx/xtick, 
                     h = dy/ytick;
        vx = (30+xmargin)+(xacross-1)*dx;
        vy = (30+ymargin)+(yacross-1)*dy;
        let /*integer*/ lm1 = dsdx-1;
        for (let d = dsdx, d$lim = length(datasets); d <= d$lim; d+=1) {
            let /*sequence*/ dd = $subse(datasets,d), [,px,py] = dd;
            let /*integer*/ ldd = length(dd), 
                             mm = mode==="MARK",
                            dsz = marksize;
            let /*string*/ dms = markstyle, 
                           dmm = mode;
            if (ldd>=4) {
                mm = true;
                dms = $subse(dd,4);
                if (ldd>=5) {
                    let /*object*/ dd5 = $subse(dd,5);
                    if (string(dd5)) {
                        assert(equal(dd5,"MARKLINE"));
                        dmm = "MARKLINE";
                        if (ldd>=6) {
                            dsz = $subse(dd,6)
                        }
                    } else {
                        dsz = dd5;
                    }
                }
            }
            let /*atom*/ fgclr = (ldd>=3) ? $subse(dd,3) : XPG_BLACK;
//          xCanvasSetForeground(graph,fgclr);
            gSetInt(graph,"FGCLR",fgclr);
//          xCanvasSetBackground(graph,fgclr);
            gSetInt(graph,"BGCLR",fgclr);
            if (length(px)) {
                let /*atom*/ x1 = (30+xmargin)+($subse(px,1)-xmin)*w, 
                             y1 = (30+ymargin)+($subse(py,1)-ymin)*h;
                for (let i = 2-((vb || hb) || mm), i$lim = length(px); i <= i$lim; i += 1) {
                    let /*atom*/ x2 = ((30+xmargin)+($subse(px,i)-xmin)*w)+(dx*vb)/2, 
                                 y2 = ((30+ymargin)+($subse(py,i)-ymin)*h)+(dy*hb)/2;
                    if (mode === "BAR") {
                        if (vb) {
                            xCanvasRect(graph,(x2-dx/2)+1,(x2+dx/2)-1,hight-vy,hight-y2,true);
                        } else if (hb) {
                            xCanvasRect(graph,vx,x2,hight-(y2-dy/2+1),hight-(y2+dy/2-1),true);
                        }
                    } else {
                        if (mm) {
                            // (from IupPlot:) mark style of the current dataset. 
                            //        Can be: "HOLLOW_CIRCLE", "PLUS", "X", [DONE]
                            //          "STAR", "CIRCLE", "BOX", "DIAMOND",
                            //          "HOLLOW_BOX", "HOLLOW_DIAMOND". Default "X". 
                            //        (rest to be implemented as and when needed)
                            if (dms === "HOLLOW_CIRCLE") {
//  --                          xCanvasCircle(graph,x2,y2,8)
                                xCanvasCircle(graph,x2,hight-y2,2*(dsz+1));
                            } else if (dms === "PLUS") {
//                              xCanvasLine(graph,x2,y2-3,x2,y2+3)
                                xCanvasLine(graph,x2,hight-(y2-dsz),x2,hight-(y2+dsz+1));
//                              xCanvasLine(graph,x2-3,y2,x2+3,y2)
                                xCanvasLine(graph,x2-dsz+1,hight-y2,x2+dsz,hight-y2);
                            } else if (dms === "X") {
//--                            xCanvasLine(graph,x2-3,y2-3,x2+3,y2+3)
                                xCanvasLine(graph,x2-dsz,hight-(y2-dsz),x2+dsz,hight-(y2+dsz))
//--                            xCanvasLine(graph,x2-3,y2+3,x2+3,y2-3)
                                xCanvasLine(graph,x2-dsz,hight-(y2+dsz),x2+dsz,hight-(y2-dsz))
                            } else if (dms="DOT") {
                                xCanvasPixel(graph,x2,hight-y2)
                            } else {
                                crash("unknown MARKSTYLE (%s)",["sequence",dms]);
                            }
                        }
                        if (!mm || ((dmm === "MARKLINE") && (i>=2))) {
//                          xCanvasLine(graph,x1,y1,x2,y2);
                            xCanvasLine(graph,x1,hight-y1,x2,hight-y2);
                        }
                    }
                    x1 = x2;
                    y1 = y2;
                }
            }
            if (legend) {
                let /*integer*/ lX = lx-20, 
//                              lY = (hight-ly)-25;
                                lY = ly-25;
                if (mode==="BAR") { // (untested)
                    xCanvasRect(graph,lX,lX+10,lY-5,lY+5,true);
//--                xCanvasRect(graph,lX,lX+10,hight-(lY-5),hight-(lY+5),true)
                } else {
                    if (mm) {
                        if (dms === "HOLLOW_CIRCLE") {
                            xCanvasCircle(graph,lX+15,lY,8);
                        } else if (dms==="PLUS") {
                            xCanvasLine(graph,lX+15,lY-5,lX+15,lY-5);
                            xCanvasLine(graph,lX+10,lY,lX+20,lY);
                        } else if (dms==="PLUS") {
                            xCanvasLine(graph,lX+10,lY+5,lX+20,lY-5);
                            xCanvasLine(graph,lX+10,lY-5,lX+20,lY+5);
                        } else if (dms==="PLUS") {
                            xCanvasPixel(graph,lX+15,lY);
                        }
                    }
                    if (!mm || (dmm==="MARKLINE")) {
                        xCanvasLine(graph,lX+5,lY,lX+25,lY);
                    }
                }
//              xCanvasSetForeground(graph,XPG_BLACK);
                gSetInt(graph,"FGCLR",XPG_BLACK);
                xCanvasText(graph,lX,lY,$subse($subse($subse(datasets,legend),2),d-lm1));
//--DEV erm...
                ly += lh;
//              ly -= lh;
            }
        }
//      xCanvasSetBackground(graph,bgclr);
        gSetInt(graph,"BGCLR",bgclr);
        if (post) { post(graph); }
        return XPG_DEFAULT;
    }

    let /*gdx*/ graph = gCanvas(redraw_graph);
//DEV check this is OK for xpGUI.css:
    graph.setAttribute("class", "graph");
    gSetHandler(graph,"DRID",drid)
    gSetInt(graph,"GRID",true);         // (show the grid by default)
    gSetInt(graph,"LEGENDBOX",true);    // (ditto box around legend)
    gSetAttribute(graph,"GRIDCOLOR",XPG_GREY);
    gSetDouble(graph,"XTICK",1);
    gSetDouble(graph,"YTICK",1);
    gSetInt(graph,"XMARGIN",10);
    gSetInt(graph,"YMARGIN",10);
    gSetAttribute(graph,"GTITLE","");
    gSetAttribute(graph,"XNAME","");
    gSetAttribute(graph,"YNAME","");
    gSetAttribute(graph,"XTICKFMT","%g");
    gSetAttribute(graph,"YTICKFMT","%g");
    gSetAttribute(graph,"MODE","");
    gSetAttribute(graph,"BARMODE","");
    gSetAttribute(graph,"MARKSTYLE","");
    gSetInt(graph,"MARKSIZE",3);
    // these are needed to replace/match the repeat(0,..) within xpGUI.e:
    gSetInt(graph,"XYSHIFT",0);
    gSetInt(graph,"YXSHIFT",0);
    gSetInt(graph,"XANGLE",0);
    gSetInt(graph,"YANGLE",0);
    gSetInt(graph,"XACROSS",0);
    gSetInt(graph,"YACROSS",0);
//  store_attrs(["graph"], ["","","","XMAX","XMIN","","","","",
//                          "","","","YMAX","YMIN","","","","",
//                          "XNAME","YNAME"], set_graph);
/*
//local constant graph_attrs = {{    GX_GRID:=$,'B',"GRID"          },
//                            { GX_GRIDCOLOR:=$,'I',"GRIDCOLOR"     },
//                            { GX_LEGENDBOX:=$,'B',"LEGENDBOX"     },
                              {     GX_XTICK:=$,'N',"XTICK"         },
                              {     GX_YTICK:=$,'N',"YTICK"         },
                              {      GX_XMIN:=$,'N',"XMIN"          },
                              {      GX_XMAX:=$,'N',"XMAX"          },
                              {      GX_YMIN:=$,'N',"YMIN"          },
                              {      GX_YMAX:=$,'N',"YMAX"          },
                              {   GX_XMARGIN:=$,'N',"XMARGIN"       },
                              {   GX_YMARGIN:=$,'N',"YMARGIN"       },
                              {   GX_XYSHIFT:=$,'N',"XYSHIFT"       },
                              {   GX_YXSHIFT:=$,'N',"YXSHIFT"       },
//                            {    GX_XANGLE:=$,'N',"XANGLE"        },
//                            {    GX_YANGLE:=$,'N',"YANGLE"        },
//                            {    GX_GTITLE:=$,'S',"GTITLE"        },
//                            {     GX_XNAME:=$,'S',"XNAME"         },
//                            {     GX_YNAME:=$,'S',"YNAME"         },
                              {  GX_XTICKFMT:=$,'S',"XTICKFMT"      },
                              {  GX_YTICKFMT:=$,'S',"YTICKFMT"      },
//                            {      GX_MODE:=$,'S',"MODE"          },
//                            {   GX_BARMODE:=$,'S',"BARMODE"       },
//                            { GX_MARKSTYLE:=$,'S',"MARKSTYLE"     },
                              {  GX_MARKSIZE:=$,'I',"MARKSIZE"      },
                              {   GX_XACROSS:=$,'B',"XACROSS"       },
                              {   GX_YACROSS:=$,'B',"YACROSS"       },
--                            {      GX_XRID:=$,'I',"XRID"          },
--                            {      GX_YRID:=$,'I',"YRID"          },
                              { GX_LEGENDPOS:=$,'S',"LEGENDPOS"     },
                              {  GX_LEGENDXY:=$,'P',"LEGENDXY"      }},
*/

    gSetAttributes(graph,attributes,args);
    return graph;
}

//function IupList(action = null, func = null, attributes = "", args = []) {
function gList(/*object*/ data, /*object*/ selected=NULL, /*sequence*/ attributes="", /*dword_seq*/ args=[]) {

//local constant lSigs = {{1,1,"FI"},{2,2,"FOI"}}
//local enum                lFI,       lFOI

    function xpg_list_default_DROP(/*integer*/ rid) { return rid; }

    function redraw_list(/*gdx*/ list) {
//      object lstattr = ctrl_xtra[list][CX_GTL_ATTRS]
//      let object data = lstattr[LX_DATA]
        let data = list.DATA;
        let /*integer*/ [,width, height] = gGetAttribute(list, "SIZE"),
                        n_rows, maxp, lh = $subse(gGetTextExtent(list,"X"),2),
                        drop = gGetHandler(list,"DROP",xpg_list_default_DROP),
                        xCanvasText = drop(gCanvasText),
                        datafn = (typeof(data) === 'function');
//      if (integer(data)) {
        if (datafn) {
//          let /*sequence*/ sig = $subse(get_routine_info(data,false),3);
            let /*sequence*/ sig = get_routine_info(data,false);
            maxp = $subse(sig,1);
            if (maxp === 1) {
                n_rows = data(0);
            } else if (maxp === 2) {
                n_rows = data(list,0);
            } else {
                crash("unrecognised sig");
            }
        } else {
            n_rows = length(data);
        }
        let /*sequence*/ si;
        let /*integer*/ nr = min(n_rows,ceil(height/lh)-1), vy = 9;
        for (let r = 1; r <= nr; r += 1) {
            if (datafn) {
                if (maxp === 1) {
                    si = data(r);
                } else if (maxp === 2) {
                    si = data(list,r);
                }
            } else {
                si = data[r];
            }
            if (string(si)) {
                xCanvasText(list,2,vy,si);
            } else {
                let /*atom*/ vx = 2, lsi = length(si);
//              for sii in si do
                for (let sdx = 1; sdx <= lsi; sdx += 1) {
                    let sii = $subse(si,sdx);
                    let /*atom*/ sic = -1, sis = -1
                    if (!string(sii)) {
//                      if not sequence(sii) then
//                          crash("fragments must be string or {string,colour[,style}}")
//                      end if
                        if (length(sii) === 2) {
                            [,sii, sic] = sii;
                        } else if (length(sii) === 3) {
                            [,sii, sic, sis] = sii;
//                      } else {
//                          crash("unrecognised fragment[length]:%v",{sii})
                        }
                    }
                    assert(string(sii));
                    xCanvasText(list,vx,vy,sii,XPG_E,0,sic,sis);
                    vx += $subse(gGetTextExtent(list,sii),1);
                }
            }
            vy += lh;
        }
    }

//--    {rows,attributes,args} = paranormalise_raa(rows,attributes,args,bCheckRid:=false)
    [,selected,attributes,args] = $paranormalise_raa(selected,attributes,args);
    let /*gdx*/ list = gCanvas(redraw_list);
//DEV check this is OK for xpGUI.css:
    list.setAttribute("class", "list");
//  ctrl_xtra[list][CX_CANVAS_TYPE] = LIST
//  object l_attr = repeat(0,LX_LEN)
//  l_attr[LX_DATA] = data
    list.DATA = data;
//  ctrl_xtra[list][CX_GTL_ATTRS] = l_attr
//  l_attr = 0 -- (kill refcount)
//  xpg_register_handler(CANVAS,"SELECTED",{{2,2,"POI"},{2,2,"POP"},{1,1,"PI"},{1,1,"PP"}})
    if (selected !== NULL) {
        gSetHandler(list,"SELECTED",selected)
    }
    if (length(attributes)) {
        gSetAttributes(list,attributes,args)
    }
    return list
}

function gTimer(/*cbfunc*/ action=NULL, /*integer*/ msecs=0, /*boolean*/ active=true) {
    if (!$storeAttr) { $gInit(); }
    return $timer("create",action,msecs,active);
}

/*global*/ 
function gQuit(/*gdx*/ /*id*/anon1) {
    // standard "Close"/"Quit" button shorthand
    return XPG_CLOSE;
}

/*local*/ let /*gdx*/ $mbid = NULL, $lbl;
//procedure gMsgBox(gdx parent,string title, msg, sequence args={}, bool bWrap=true)
/*global*/ 
function gMsgBox(/*gdx*/ parent=NULL, /*string*/ title="", msg="", /*sequence*/ args=["sequence"]) {
    if (length(args)) { msg = sprintf(msg,args); }
//DEV this may be an IUP-only thing.... quite how to resize for wordwrap remains an open question...
//  if string(msg) and find('\n',msg) and bWrap then
//      -- make each paragraph a single line, improves wordwrap
//      -- (note: this may be a windows only thing, not yet tested on lnx)
//      msg = substitute(msg,"\n\n","\r\r")
//      msg = substitute(msg,"\n"," ")
//      msg = substitute(msg,"  "," ")
//      msg = substitute(msg,"\r\r","\n\n")
//  end if
    if ($mbid===NULL) {
//      lbl = gLabel(msg,"MARGIN=10x10")
        $lbl = gLabel(msg);
//      if not find(gQuit,internal_rtns) then internal_rtns &= gQuit end if
        let /*gdx*/ ok = gHbox(["sequence",gButton("OK",gQuit)],"SPACE=LEFT"),  // DEV not aligning...
                    child = gVbox(["sequence",$lbl,ok],"MARGIN=10x10,GAP=10");
//DEV/SUG a min width of 230, or possibly based on text_extent(title)...
        $mbid = gDialog(child,parent,title);
    } else {
        //DEV reparent??
        gSetAttribute($mbid,"TITLE",title);
        gSetAttribute($lbl,"TITLE",msg);
//      gSetAttribute($mbid,"SIZE",NULL) -- DEV oops
        gRedraw($mbid,0x7);             // DEV not resizing...
    }
//DEV modal??
//  ?{"gMsgBox",parent,title,msg,bWrap}
    gShow($mbid,XPG_CENTERPARENT,XPG_CENTERPARENT); // DEV not repositioning...
}

//function IupUpdate(id) {
////IupRedraw
////    let action = id["ACTION"]
////    if (action) { action(id); }
//  function redraw() { gRedraw(id); }
//  setTimeout(redraw, 100);
//// no better:
////    window.requestAnimationFrame(redraw);
//}

let $gl;

function IupGLMakeCurrent(canvas) {
    if (canvas === NULL) {
        // enabling call from opengl.js
//1/10/21... (on a whim)
//      $gl = NULL;
        $gl = true;
//  } else if ($gl === NULL) { // ie not "undefined"
    } else if ($gl) { // ie not "undefined"
        const names = ["webgl", "experimental-webgl", "webkit-3d", "moz-webgl"];
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

//function IupDrawBegin(/*gdx id*/) {
//}
//const IupDrawEnd = IupDrawBegin;
//const IupDrawGetSize = cdCanvasGetSize;

//function IupDrawArc(/*gdx*/ id, /*integer*/ x1, y1, x2, y2, /*atom*/ a1, a2) {
//  let ctx = id.getContext("2d") || id.getContext("webgl");
//  cdCanvasArc(ctx,x1,y1,x2,y2,a1,a2);
//}

//function IupDrawGetTextSize(/*gdx*/ id, /*string*/ text) {
//  let ctx = id.getContext("2d") || id.getContext("webgl");
//  return cdCanvasGetTextSize(ctx, text);
//}

//function IupDrawLine(/*gdx*/ id, /*integer*/ x1, y1, x2, y2) {
//  let ctx = id.getContext("2d") || id.getContext("webgl");
//  cdCanvasLine(ctx,x1,y1,x2,y2);
//}

////cdCanvasRect(/*cdCanvas*/ ctx, /*atom*/ xmin, xmax, ymin, ymax) {
//function IupDrawRectangle(/*gdx*/ id, /*integer*/ x1, y1, x2, y2) {
//  let ctx = id.getContext("2d") || id.getContext("webgl");
////    cdCanvasRect(id,x1,x2,y1,y2);
//  cdCanvasRect(ctx,x1,x2,y1,y2);
//}

//function IupDrawText(/*gdx*/ id, /*string*/ text, /*integer*/ x, y, w=0, h=0) {
////    cdCanvasText(id, x, y, text);
//  let ctx = id.getContext("2d") || id.getContext("webgl");
//  cdCanvasText(ctx, x, y, text);
//}

//function IupMessage(msg_title=NULL, msg=NULL, args=[], bWrap=true) {
//  if (args.length) {
//      msg = sprintf(msg, args);
//  }
//  if (msg_title) { msg = msg_title + "\n\n" + msg;}
//  alert(msg);
//}

// (return to this when(if) I get wifi back!)
//integer res = IupAlarm(string title, string msg, string b1, nullable_string b2=NULL, nullable_string b3=NULL) 
//function IupAlarm(title, msg, b1, b2, b3) {
//  alert(title + "\n\n" + msg);
//}

//function IupGLCanvasOpen() {
//  // (does nothing)
//}

function gImage_from_XPM(/*sequence*/ xpm) {
//  if not bInit then xpg_Init() end if
//  if string(xpm) then xpm = split(xpm,'\n') end if
    if (string(xpm)) { xpm = split(xpm,0XA); }
//  object img
//  if backend=GTK then
//      img = xpg_xpm_callback(xpm)
//  elsif backend=WinAPI then
//      img = xpg_winAPI_create_DIB_from_xpm(xpm,xpg_xpm_callback)
//  else
//      ?9/0 -- (unknown backend)
//  end if
//  gdc res = {"gImage",img}
//  return res
//var XPM = {
    //
    // Only method of this scope for now: create()
    // Takes multiple parameters... here is the list:
    //   - "<width> <height> <number of colors> <characters used per color>"
    //   - "<character used> c <color>" <- for each color used
    //   - Drawing of the XPM.
    //
    // @see here for the inspiration: http://fr.wikipedia.org/wiki/X_PixMap
    //
//  create: function() {
        // Split the first argument in an array
//      var firstArgument = arguments[0].split(' '),
        let firstArgument = xpm[1].split(' '),

        // Get the width, height, number of colors and number of characters
        // per color with the first argument.
            width = firstArgument[0],
            height = firstArgument[1],
            nbColors = parseInt(firstArgument[2], 10),
            nbCharactersPerColor = parseInt(firstArgument[3], 10),
            step = width * nbCharactersPerColor,

        // Get the end of the argument array (all the characters of the image)
//          imgCharacters = [].slice.call(arguments, nbColors + 1),
            imgCharacters = [].slice.call(xpm, nbColors + 2),

        // Create a canvas
            canvas = document.createElement("canvas"),
            ctx = canvas.getContext("2d"),

        // Declare some variables for later use
            map = [],
            characters,
            color,
            i,
            j,
            k,
            l

        // Define the width and height of the canvas
        canvas.width = width;
        canvas.height = height;

        // Add every mapping of characters and colors to the `map` array
        for (i = 0; i < nbColors; ++i) {
//          characters = arguments[1 + i].substring(0, nbCharactersPerColor)
            characters = xpm[2 + i].substring(0, nbCharactersPerColor)
//          color = arguments[1 + i].slice(-7)
            color = xpm[2 + i].slice(-7)
            map.push({characters: characters, color: color})
        }
        let imgdata = ctx.getImageData(0,0,width,height);

        // For every line of the ASCII art
        for (i = 0; i < height; ++i) {

            // For every string of a line
            for (j = 0; j < step; j += nbCharactersPerColor) {

                // Loop to get the correct mapping
                for (k = 0, l = map.length; k < l; ++k) {

                    // If it maps, draw!
                    if (map[k].characters === imgCharacters[i].substr(j, nbCharactersPerColor)) {
//                      ctx.fillStyle = map[k].color
                        let c = map[k].color;
//closePath
//if (c === " c None") { c = "white"; }
//                      if (c === " c None") { 
//                          // set alpha on that pixel to 0
//                          // [DEV only tested on nbCharactersPerColor==1]
//assert(nbCharactersPerColor === 1,"test this better");
////                            imgdata[i*width+j+3] = 0;
////??                      imgdata[i*step+j*nbCharactersPerColor+3] = 0;
//                      } else {
                        if (c !== " c None") { 
//if (c !== " c None") {
//printf(1,"{%d,%d}:[%d]%v\n",["sequence",i,j,k,c]);
//}
                            ctx.fillStyle = c;
//                      ctx.fillStyle = "red";
                            ctx.fillRect((j * nbCharactersPerColor), i, 1, 1)
                        }
                        // Break out of the loop since we hit the nail already
                        break
                    }
                }
            }
        }

        // Return the canvas DOM element already drawn
        return canvas
//      return ctx;
//  }
//}
}

function gImageDraw(/*gdc*/ src, tgt, /*atom*/ x=0, y=0) {
    tgt.ctx.drawImage(src, x, y);
}

function gUseGTK() { /* do nothing */ }

////DEV move to builtins/opengl.js... (along with much other code)
//    GL_ARRAY_BUFFER = 0x8892,
//    GL_INFO_LOG_LENGTH = 0x8B84,
//    GL_SHADER_SOURCE_LENGTH = 0x8B88,
//    GL_CLAMP = 0x2900,
//    GL_CLAMP_TO_BORDER = 0x812D,
//    GL_CLAMP_TO_EDGE = 0x812F,
//    GL_COLOR_BUFFER_BIT = 0x00004000,
//    GL_COMPILE_STATUS = 0x8B81, 
//    GL_CULL_FACE = 0x0B44,
//    GL_DEPTH_BUFFER_BIT = 0x00000100,
//    GL_DEPTH_TEST = 0x0B71,
////      GL_DEPTH_TEST = 0,
//    GL_FLOAT = 0x1406,
//    GL_FRAGMENT_SHADER = 0x8B30,
//    GL_LINEAR = 0x2601,
//    GL_LINK_STATUS = 0x8B82,
//    GL_MODELVIEW = 0x1700,
//    GL_NO_ERROR = 0,
//    GL_NEAREST = 0x2600,
//    GL_PROJECTION = 0x1701,
//    GL_REPEAT = 0x2901,
//    GL_RGB = 0x1907,
//    GL_RGBA = 0x1908,
//    GL_STATIC_DRAW = 0x88E4,
//    GL_TEXTURE_2D = 0x0DE1,
//    GL_TEXTURE_MAG_FILTER = 0x2800,
//    GL_TEXTURE_MIN_FILTER = 0x2801,
//    GL_TEXTURE_WRAP_S = 0x2802,
//    GL_TEXTURE_WRAP_T = 0x2803,
//    GL_TRIANGLES = 4,
//    GL_UNSIGNED_BYTE = 0x1401,
//    GL_VERTEX_SHADER = 0x8B31,


