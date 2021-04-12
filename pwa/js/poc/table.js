'use strict';

//DEV temp:
let counter = document.querySelector('.details'),
    button = document.querySelector('button');
//  button.onclick = refresh_observer;
//  counter.innerHTML = sprintf('%d lines (%3.2fs)',[length(data[1]),(end-start)/1000]);


const columns = ["sequence",
                 ["sequence","Chq#",80,"ARIGHT"],
                 ["sequence","some very long text Date",120,"ACENTER"],
                 ["sequence","Amount",210,"ARIGHT"],
                 ["sequence","Status",70,"ACENTER"],
                 ["sequence","Bank",105,"ALEFT"]];

//const columns = ["Chq#","Date","Amount","Status","Bank"] -- (fine too) [DEV untested]

//const MAX = 50000; // ~0.75s to build, 0s to display
const MAX = 5000-0; // ~0.2s to build
//const MAX = 500-0-0; // ~0.035s to build

function buildDummyData() {
    // data is length 2: {raw/sortable,null/display columns}
    let data = repeat(0,MAX);
//oh fuck... (cow semantics...)
//  data = ["sequence",data,["sequence",0,data,data]];
//  data = ["sequence",data,["sequence",0,repeat(0,MAX),repeat(0,MAX)]];
    data = ["sequence",[...data],["sequence",0,[...data],[...data]]];
    for (let i=1; i<=MAX; i += 1) {
        let dt = ["sequence",2018+rand(2),rand(12),rand(28)],
            cheque = i,
            date = sprintf("%02d/%02d/%04d",reverse(dt)),
            amount = rand(999999)/100,
            status = (rand(5)==1?"R":""),
            bank = sprintf("%5d", rand(22)+10000);
        data[1][i] = ["sequence",cheque,dt,amount,status,bank];
//      -- initially overwritten with date/amt_fmt() below,
//      -- however not when ' ' is subsequently keyed:
        if (i==3) { date += '(this needs verifying)'; }
        data[2][2][i] = date;
        data[2][3][i] = sprintf("%.2f",amount);
    }
    return data;
}

let start = new Date().valueOf();
let data = buildDummyData();
let end = new Date().valueOf();
counter.innerHTML = sprintf('%d lines (%3.2fs)',[length(data[1]),(end-start)/1000]);
let table = IupTable(columns,data);
//DEV temp:
document.body.insertBefore(table, document.body.firstChild);

/*
let selectedElements = new Array();
let allowMultiple = true;

//DEV use classList... (directly)
function trim(str) {
    while (str.substr(0,1)==' ') str = str.substr(1);
    while (str.substr(str.length-1,1)==' ') str = str.substr(0,str.length-1);
    return str;
}
function hasClass(elm,findclass) {
    if (!elm) return null;
    return (' '+elm.className+' ').indexOf(' '+findclass+' ')+1;
}
function changeClass(elm,oldclass,newclass) {
    if (!elm) return null;
    let c = elm.className.split(' ');
    for (let i=0;i<c.length;i++) {
        c[i] = trim(c[i]);
        if (c[i]==oldclass || c[i]==newclass || c[i]=='') c.splice(i,1);
    }
    c.push(newclass);
    elm.className = trim(c.join(' '));
}
function elementIndex(elm) {
    for (let i=0;i<this.elements.length;i++) {
        if (this.elements[i]==elm) return i;
    }
    return -1;
}

function selected(elm) {
    return hasClass(elm,'selrow');
}
function select(elm) {
    changeClass(elm,'','selrow');
    selectedElements.push(elm);
}
function deselect(elm) {
    changeClass(elm,'selrow','');
    for (let i=0;i<selectedElements.length;i++) {
        if (selectedElements[i]==elm) selectedElements.splice(i,1);
    }
}
//DEV use the tags...
function selectRange(elm1) {
    if (selectedElements.length===0) {
        select(elm1);
        return false;
    }
    let elm0 = selectedElements[selectedElements.length-1];
    let d = (elementIndex(elm0) < elementIndex(elm1));
    let elm = elm0;
    if (selected(elm1)) {if (selected(elm0)) deselect(elm0);}
    else {if (!selected(elm0)) select(elm0);}
    do {
        elm = (d)? elm.nextSibling : elm.previousSibling;
        if (selected(elm)) deselect(elm);
        else select(elm);
    } while (elm!=elm1);
    return true;
}
function cleanselect() {
    for (let i=0;i<this.elements.length;i++) {
        if (selected(this.elements[i])) deselect(this.elements[i]);
    }
    selectedElements = new Array();
}
function getEventElement(e) {
    if (!e) e = window.event;
    return (e.target)? e.target : e.srcElement;
}
function callBodyClick(e) {
    let elm = getEventElement(e);
    let st = SortedTable.getSortedTable(elm);
    if (!st) return false;
    let elm = st.findParent(elm,'tr');
    if (e.shiftKey && allowMultiple) {
        selectRange(elm);
    } else if (selected(elm)) {
        deselect(elm);
    } else if (!e.ctrlKey || !allowMultiple) {
        cleanselect();
    } else {
        select(elm);
    }
    return false;
}
*/

/*
let styleNode = document.createElement('style');
document.head.appendChild(styleNode);
let s = document.getElementsByTagName('style');
*/

/*
function performTask(items, numToProcess, processItem) {
    let pos = 0;
    // This is run once for every numToProcess items.
    function iteration() {
        // Calculate last position.
        let j = Math.min(pos + numToProcess, items.length);
        // Start at current position and loop to last position.
        for (let i = pos; i < j; i++) {
            processItem(items, i);
        }
        // Increment current position.
        pos += numToProcess;
        // Only continue if there are more items to process.
        if (pos < items.length)
            setTimeout(iteration, 10); // Wait 10 ms to let the UI update.
    }
    iteration();
}

performTask(
    // A set of items.
    ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o'],
    // Process two items every iteration.
    2,
    // Function that will do stuff to the items. Called once for every item. Gets
    // the array with items and the index of the current item (to prevent copying
    // values around which is unnecessary.)
    function (items, index) {
        // Do stuff with items[index]
        // This could also be inline in iteration for better performance.
    });
*/

/*
const observer = new IntersectionObserver(handle, {});
let observedNodes = new Set();
function refresh_observer() {
    // works fine, and perfectly fast enough on 10,000 nodes.
    observer.disconnect();
    for (let node of observedNodes) {
//      observer.unobserve(node);
        observer.observe(node);
    }
}
*/

/*
let options = {
  root: document.querySelector('#scrollArea'),
  rootMargin: '0px',
  threshold: 1.0    // 100% (0, the default, is better!)
}
let observer = new IntersectionObserver(callback, options);
*/
/*
class IntersectionObserverManager {
    constructor(observer) {
        this._observer = observer;
        this._observedNodes = new Set();
    }
    observe(node) {
        this._observedNodes.add(node);
        this._observer.observe(node);
    }
    unobserve(node) {
        this._observedNodes.remove(node);
        this._observer.unobserve(node);
    }
    disconnect() {
        this._observedNodes.clear();
        this._observer.disconnect();
    }
    refresh() {
        for (let node of this._observedNodes) {
            this._observer.unobserve(node);
            this._observer.observe(node);
        }
    }
}
*/
//const observer = new IntersectionObserver(handle, {
//  threshold: [0, 1],
//  rootMargin: '-100px 0px'
//});
/*
//function getRandom() {
//  return parseInt(Math.random() * 10000000)
//}
let count = 1
function add_some_rows() {
  const num = 10000
  let contentArea = document.querySelector('#contentArea');
  for (; count <= num; count++) {
//  $("#contentArea").append(`<tr class="tr"><td>${count}</td><td></td><td>${getRandom()}</td><td>${getRandom()}</td></tr>`)
//  let node = document.createElement(`<tr class="tr"><td>${count}</td><td></td><td>${getRandom()}</td><td>${getRandom()}</td></tr>`);
    let node = document.createElement(`tr`);
//  node.classList.add("tr");
    node.id = count;
//  set_inner(node); // [DEV/SUG]
//DEV try setting the height... (in css?!)
//if (count<100) {
    let td = document.createElement(`td`);
    td.textContent = count;
    node.appendChild(td);
//  node.innerHTML = `<td>${count}</td><td></td><td>${getRandom()}</td><td>${getRandom()}</td>`;
//}
    contentArea.appendChild(node);
    observer.observe(node);
//if (count>1) {
    observedNodes.add(node);
//}
//     <img class="img" src="https://timgsa.baidu.com/timg?v=${count}&image&quality=80&size=b9999_10000&sec=1598534746227&di=01ee68f8e9595365fd187780ced5d6fd&imgtype=0&src=http%3A%2F%2Ft7.baidu.com%2Fit%2Fu%3D3204887199%2C3790688592%26fm%3D79%26app%3D86%26f%3DJPEG%3Fw%3D4610%26h%3D2968"/>
  }
}
add_some_rows();
//  $("#count").html(`共${count}条数据`)
document.querySelector("#count").innerHTML = `${count-1} lines`;
//let tr = document.querySelectorAll('.tr');
////  $('.tr').each((index, element) => {
//tr.forEach((element, index) => {
//  observer.observe(element);
//  });
*/

//function checkTime(i) {
//  if (i < 10) {
//  i = "0" + i;
//  }
//  return i;
//}
//
//function getTime() {
//  let today = new Date(),
//        h = today.getHours(),
//        m = today.getMinutes(),
//        s = today.getSeconds();
//  // add a zero in front of numbers<10
//  m = checkTime(m);
//  s = checkTime(s);
//  return h + ":" + m + ":" + s;
////  document.getElementById('time').innerHTML = h + ":" + m + ":" + s;
//}

/*
function handle (entries, observer) {
  entries.forEach(entry => {
//  const target = entry.target
    if (entry.isIntersecting) {
//    if($(target).find('img').length) return
//    if (target.querySelectorAll('img').length) return
//    const img = document.createElement('img')
//    img.src = `https://timgsa.baidu.com/timg?v=${getRandom()}&image&quality=80&size=b9999_10000&sec=1598534746227&di=01ee68f8e9595365fd187780ced5d6fd&imgtype=0&src=http%3A%2F%2Ft7.baidu.com%2Fit%2Fu%3D3204887199%2C3790688592%26fm%3D79%26app%3D86%26f%3DJPEG%3Fw%3D4610%26h%3D2968`
//    target.querySelectorAll('td')[1].appendChild(img);
//    target.querySelectorAll('td')[1].innerHTML = getTime();
      let dt = new Date();
//    entry.target.querySelectorAll('td')[1].innerHTML = dt.toLocaleTimeString();
      let node = entry.target;
      node.innerHTML = `<td>${node.id}</td><td>${dt.toLocaleTimeString()}</td><td>${getRandom()}</td><td>${getRandom()}</td>`;

//  } else {
//    let img = target.querySelector('img');
//    if (img) img.parentNode.removeChild(img);
    }
  });
}
*/

