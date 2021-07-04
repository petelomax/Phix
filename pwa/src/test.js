//
// pwa/src/test.js
// ===============
//
//  Purpose: Verify nothing has been accidentally broken.
//           Output shd match tout.exw, and itself bar whitespace.
//
//  This is not a working program, there is no guarantee 
//  that some of these working examples will not someday
//  be sacrificed for some as yet unknown greater good.
//
const a = 1, b, c, d;
a = $conCat(b, c);
a = $conCat(1+2, 3+4);
a = 1+2+3+4;
a = $conCat(1===2, 3===4);
a = 1===$conCat(2,3)===4;
let [,/*string*/ s,/*integer*/ i] = s[i];
let /*sequence*/ s123 = ["sequence",1,2,3]; 
function multitext_valuechanged_cb(/*Ihandle*/ /*multitext*/anon1) {
//DEV update syntax colouring... (on a timer?)
    return IUP_DEFAULT;
}
if (a || b || c || d) { }
if ((a || b) && (c || d)) { }
if ((a && b) || (c && d)) { }
let /*sequence*/ res = ["sequence"], start = ["sequence"];
res = $conCat(res, start);
res = $conCat(res, start);

