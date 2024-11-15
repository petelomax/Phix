"use strict";
// auto-generated by pwa/p2js - part of Phix, see http://phix.x10.mx
//
// to_str.e
//
// Phix implementation of to_string (auto-include)
//

/*global*/ function to_string(/*object*/ data_in, /*integer*/ string_quote=0, /*integer*/ embed_string_quote=0X22) {
    if (string(data_in)) {
        if (string_quote===0) {
            return data_in;
        }
        data_in = match_replace(`\\`,data_in,`\\\\`);
        data_in = match_replace("\r",data_in,`\\r`);
        data_in = match_replace("\n",data_in,`\\n`);
//p2js (tough... I previously recommended using sprint() or printf(%v) instead in the docs anyway)
//      data_in = match_replace("\t", data_in, `\t`)
        data_in = match_replace(["sequence",string_quote],data_in,$conCat(`\\`, string_quote));
        return $conCat($conCat(string_quote, data_in), string_quote);
    }
    if (atom(data_in)) {
        if (integer(data_in)) {
            return sprintf("%d",data_in);
        }
        data_in = trim_tail(sprintf("%.15f",data_in),0X30);
        if (equal($subse(data_in,-1),0X2E)) {
            data_in = remove(data_in,length(data_in));
        }
        return data_in;
    }
    let /*sequence*/ data_out = "{";
    for (let i=1, i$lim=length(data_in); i<=i$lim; i+=1) {
        data_out = $conCat(data_out, to_string($subse(data_in,i),embed_string_quote), false);
        if (!equal(i,length(data_in))) {
            data_out = $conCat(data_out, ", ", false);
        }
    }
    data_out = $conCat(data_out, 0X7D, false);
    return data_out;
}
