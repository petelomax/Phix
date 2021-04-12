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
const IUP_DEFAULT = -2;

//DEV/SUG split into/manage both handlers and valid/supported values?
let $storeAttr = {};    // element-specific attribute handlers/setters

function IupOpen() {

    //
    // DEV/SUG: [not yet implemented!]
    // Note that pwa/p2js collates elems/names from all the store_attrs()
    //           calls in this (pGUI.js) to determine which attributes 
    //           it should permit(/or warn about) on the desktop (!!!).
    //           [done whenever file modified!=last analysed date&time]
    //
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
            for (let n = 0; n < names.length; n += 1) {
                let name = names[n];
                if (typeof(name) !== "string") {
                    crash("invalid name");
                }
                if (!$storeAttr.hasOwnProperty(elem)) {
                    $storeAttr[elem] = {};
                }
                $storeAttr[elem][name] = fn;
            }
        }
    }

    function intint(val) {
        if (typeof(val) === "string") {
            // convert eg "225x75" to [225,75]
            //  (ie a js Array of length 2)
            let x = val.indexOf('x');
            if (x) {
                let y = Number(val.slice(x+1));
                    x = Number(val.slice(0,x));
                if (Number.isInteger(x) &&
                    Number.isInteger(y)) {
                    val = [x,y];
//maybe: (IupGetIntInt() anyway)
//                  val = ["sequence",x,y];
                }
            }
        }
        if (!Array.isArray(val) ||
            val.length !== 2) {
//maybe:
//      if (!sequence(val) || length(val)!==2) {
            crash("invalid intint value");
        }
        return val;
    }

    function class_name(ih, check) {
        if (!ih) { crash("invalid handle"); }
//      let cn = ih.className;
        let cn = ih.classList[0];
//      let sp = cn.indexOf(' ');
////hmmm, can we be sure it stays first??
//      if (sp !== -1) { cn = cn.slice(0,cn); }
//      if (check && check !== cn) {
        if (check !== cn) {
//      if (!ih.classList.contains(check)) {    // [untried]
            crash("wrong class (not " + check + ")");
        }
//      return cn
    }

    function rastersize(ih, name, val) {
        // applies to dialog, label
        if (name !== "RASTERSIZE") { crash("RASTERSIZE expected!"); }
        if (val!==null) {
            let [w, h] = intint(val);
            if (w) { ih.style.width = w + "px"; }
            if (h) { ih.style.height = h + "px"; }
        }
    }

    function set_dialog(ih, name, val) {
//      if (ih.className !== "dialog") {
//          crash("wrong class (not IupDialog)");
//      }
        class_name(ih, "dialog");
        if (name === "TITLE") {
            const hdrs = ih.getElementsByClassName("dialog-handle");
            if (hdrs.length !== 1) {
                crash("cannot get (lone) header handle");
            }
//          hdrs[0].innerHTML = "<b>" + val + "</b>";
            hdrs[0].innerHTML = val;
        } else if (name === "MINSIZE") {
            let [w, h] = intint(val);
            if (ih.style.width < w)  { ih.style.width = w + "px"; }
            if (ih.style.height < h) { ih.style.height = h + "px"; }
            ih.minimumWidth = w;
            ih.minimumHeight = h;
//      } else if (name === "MARGIN" || name === "GAP") {
        } else if (name === "MARGIN") {
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
        } else {
            crash("IupStoreAttribute(IupDialog,\"" + name + "\") not yet implemented");
        }
    }
    store_attrs(["dialog"], ["TITLE","MINSIZE","MARGIN","GAP"], set_dialog);    // (common-up when appropriate)
    store_attrs(["dialog"], ["RASTERSIZE"], rastersize);                        // (... like this)
//  store_attrs(["dialog","label"], ["RASTERSIZE"], rastersize);                // (...  or this)
//  store_values("MINSIZE",[["%d"],"x",["%d"]])
//--    dlg = IupDialog(lbl,`TITLE="Hello", SIZE=150x40, MINSIZE=225x75`)
//      dlg = IupDialog(lbl,`TITLE="Hello", MINSIZE=225x75`)
//--    dlg = IupDialog(lbl,`TITLE="Hello", MINSIZE=225x75, PLACEMENT=MAXIMIZED`) -- (more like it)
//--    dlg = IupDialog(lbl,`TITLE="Hello", MINSIZE=225x75, FULLSCREEN=YES`) (be like F11 in the browser...)

    function crastersize(ih, name, val) {
        // applies to canvas
        if (name !== "RASTERSIZE") { crash("RASTERSIZE expected!"); }
        if (val!==null) {
            let [w, h] = intint(val);
            if (w) { ih.width = w; }
            if (h) { ih.height = h; }
//      } else {
//          ih.width = 0;
//          ih.height = 0;
//          ih.width = "100%";
//          ih.height = "100%";
        }
    }
    store_attrs(["canvas"], ["RASTERSIZE"], crastersize);


//Ihandle lbl = IupLabel("World","EXPAND=YES, ALIGNMENT=ACENTER"),
    function set_label(ih, name, val) {
        class_name(ih,"label");
        if (name === "TITLE") {
            ih.innerHTML = val;
        } else if (name === "EXPAND") {
//Value: "YES" (both directions), "HORIZONTAL", "VERTICAL", "HORIZONTALFREE", "VERTICALFREE" or "NO".
//Default: "NO". For containers the default is "YES".
//Affects: All elements, except menus. 
            if (val === "YES") {
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
        } else if (name === "ALIGNMENT") {
//ALIGNMENT  (non inheritable) horizontal and vertical alignment. 
//              Possible values: "ALEFT", "ACENTER" and "ARIGHT", combined to "ATOP", "ACENTER" and "ABOTTOM". 
//              Default: "ALEFT:ACENTER". 
//??            Partial values are also accepted, like "ARIGHT" or ":ATOP", the other value will be used from the current alignment. 
            ih.style.display = "flex";
            let k = val.indexOf(':');
            if (k !== -1) {
                let val = val.slice(0,k),
                    vtb = val.slice(k+1);
                if (vtb === "ATOP") {
                    ih.style.alignItems = "flex-start";
//                  ih.classList.add("expandv");
//                  ih.classList.add("expandh");
                } else if (vtb === "ACENTER") {
                    ih.style.alignItems = "center";
//                  ih.classList.remove("expandv");
//                  ih.classList.add("expandh");
                } else if (vtb === "ABOTTOM") {
                    ih.style.alignItems = "flex-end";
//                  ih.classList.add("expandv");
//                  ih.classList.remove("expandh");
                } else {
                    crash("IupStoreAttribute(IupLabel,\"ALIGNMENT\", \":" + vtb + "\"??)");
                }
//              puts(1,"IupStoreAttribute(IupLabel,\"ALIGNMENT\", \":" + vtb + "\" not yet implemented)");
            }
            if (val.length) {
//              ih.style.display = "grid";
                if (val === "ALEFT") {
//                  ih.style.placeItems = "start";  // (top left)
                    ih.style.justifyContent = "flex-start";
                } else if (val === "ACENTER") {
//                  ih.style.placeItems = "center"; // (mid/mid)
                    ih.style.justifyContent = "center";
                } else if (val === "ARIGHT") {
//                  ih.style.placeItems = "end";    // (btm right)
                    ih.style.justifyContent = "flex-end";
                } else {
                    crash("IupStoreAttribute(IupLabel,\"ALIGNMENT\", \"" + val + "\"??)");
                }
            }
        } else {
            crash("IupStoreAttribute(IupLabel,\"" + name + "\") not yet implemented");
        }
    }
    store_attrs(["label"], ["TITLE","EXPAND","ALIGNMENT","MARGIN"], set_label);
    store_attrs(["label"], ["RASTERSIZE"], rastersize);
//  store_values("EXPAND",[["YES","NO","HORIZONTAL","HORIZONTALFREE","VERTICAL","VERTICALFREE"]])
//  store_values("ALIGNMENT",[["ALEFT","ACENTER","ARIGHT"],":",["ATOP","ACENTER","ABOTTOM"]])
    function set_vbox(ih, name, val) {
        class_name(ih,"vbox");
//DEV common up??
        if (name === "MARGIN") {
            let [w, h] = intint(val);
            if (w) {
                ih.style.marginLeft = w + 'px';
                ih.style.marginRight = w + 'px';
            }
            if (h) {
                ih.style.marginTop = h + 'px';
                ih.style.marginBottom = h + 'px';
            }
        } else if (name === "TABTITLE") {
            ih.TABTIITLE = val;
        } else {
            crash("IupStoreAttribute(IupVbox,\"" + name + "\") not yet implemented");
        }
    }
    store_attrs(["vbox"], ["MARGIN","TABTITLE"], set_vbox);

//ToDo: (found while surfin)
//button.setAttribute("disabled", "true");
//button.removeAttribute("disabled");
//[attribute]   [target]        Selects all elements with a target attribute            (PL: eg [inactive] ??)
//[attribute=value]     [target=_blank] Selects all elements with target="_blank"       (PL: eg [type=button] ??)
}

//(Ihandle ih, string name, nullable_string val, sequence args={}) 
function IupSetStrAttribute(ih, name, v, args = []) {
//  let t = ih.className;
    let t = ih.classList[0];
//  let t = class_name(ih);
    if (!t || !$storeAttr.hasOwnProperty(t)) {
        crash("invalid type");
    }
    if (typeof(name) !== "string" || !$storeAttr[t].hasOwnProperty(name)) {
        crash("invalid attr name (%s for %s)",["sequence",name,t]);
    }
    let fn = $storeAttr[t][name];
    if (typeof(fn) !== "function") {
        crash("invalid fn");
    }
    if (args.length) {
        v = sprintf(v, args);
    }
    fn(ih, name, v);
}

const IupStoreAttribute = IupSetStrAttribute;
const IupSetAttribute = IupSetStrAttribute;

function IupSetInt(ih, name, v) {
//  if (typeof(v) !== "number") crash("?");
    IupSetStrAttribute(ih,name,v?"YES":"NO");
}

/* (untried)
function IupGetIntInt(ih, name) {
//  if (typeof(name) !== "string" ...??
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
}

*/

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
                case '\t':
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
                        iAttribCapture("=, \t\n\r\f");      // get env_buffer until delimiter
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
                    IupStoreAttribute(ih, name, val);
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

function IupSetCallback(ih, name, func) {
    ih[name] = func;
}

function $paranormalise(action, func, attributes, args) {
// (see pGUI.e or the help docs for more details)
// ([DOC] named parameters are not [generally] permitted in pwa/p2js, except perhaps for eg timedelta()/as individually hand-coded in p2js.e)
    if (action !== null && typeof(action) !== "string") {
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
        if ((typeof(attributes) !== "string" || attributes.length === 0) && action !== null) {
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

function IupCloseOnEscape(/* Ihandle */ dlg) {
    function hide_dlg(event) {
//      let keyCode = window.event ? window.event.keyCode : e.which;
//      if (keyCode === 27) {
        if (event.key === 'Escape') {
            IupHide(dlg);
        }
    }
//  document.onkeydown = hide_dlg;
    dlg.onkeydown = hide_dlg;
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

function IupShow(ih) {
    // Make it top dog, and add it to the DOM.
    ih.style.zIndex = document.getElementsByClassName("dialog").length;
    $docBody.appendChild(ih);

    // Originally, dragging a window behaved rather differently before and
    // after resizing the window, hence this...
    const rect = ih.getBoundingClientRect(),    // (nb: recalc in DOM)
             w = rect.width,
             h = rect.height;
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
    for (let i = 1, l = length(children); i <= l; i += 1) {
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
    for (let i = 1, l = length(children); i <= l; i += 1) {
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
        ih.addEventListener('click', func);
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
        for (; updated<last; updated += 1) {
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
            ths[i].removeAttribute('data-sorted');
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
            entries.forEach(entry => {
                if (entry.isIntersecting) {
                    update_item(entry.target);
                }
            });
        }
        iObserver = new IntersectionObserver(iob_handler, {});
    }

    function trHide(trOld) {
        if (trOld) trOld.classList.remove('trActive');
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
            if (typeof(bPageKey)==="string") return true; // ditto
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
        for (; created<last; created += 1) {
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
        if (created<nRows) setTimeout(populate_table, 10);
        if (bRefresh) refresh_observer();
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

        function mouseDown(event) {
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

        for (let i=1; i<table_th.length-1; i++) {
            let resizer = document.createElement("div");
            resizer.className = "column_resizer";
            leftPos += pxWidth(table_th[i-1])+11;
            resizer.style.left = (leftPos-3)+"px";
            resizer.addEventListener("mousedown", mouseDown);
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

function IupTreeAddNodes(/*Ihandle*/ tree, /*sequence*/ tree_nodes, /*integer*/ id=-1) {

    function createTVelem(tagname, classname, innerhtml) {
        let res = document.createElement(tagname);
        res.classList.add(classname);
        if (innerhtml) { res.innerHTML = innerhtml; }
        return res;
    }
    if (tree==="createTVelem") { return createTVelem; } // for IupTreeView

    function iupTreeSetNodeAttributes(leaf, /*sequence*/ attrs) {
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

    function iupTreeAddNodesRec(/*Ihandle*/ tree, /*sequence*/ tree_nodes, /*bool*/ bRoot=true) {
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

function IupTreeView(/*sequence*/ tree_nodes, /*atom*/ branchopen_cb=null, /*string*/ attributes="", /*sequence*/ args=[]) {
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

    tree.addEventListener('click', function(event) {
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
    });

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
