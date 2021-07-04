function sha256(str) {
    let ls = length(str);
    function stringFillArray(str, arr, i = 0) {
        let bString = string(str);
        while(i < ls) {
            arr[i] = bString?str.charCodeAt(i):str[i+1];
            i += 1;
        } 
    }
    const H = [0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19],
          K = [0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5, 
               0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 
               0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 
               0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 
               0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 
               0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 
               0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3, 
               0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2];
    function ch(x, y, z) { return (x & y) ^ (~x & z); }
    function maj(x, y, z) { return (x & y) ^ (x & z) ^ (y & z); }
    function sigma0(x) { return (((x >>>  2) | (x << 30)) ^ ((x >>> 13) | (x << 19)) ^ ((x >>> 22) | (x << 10))); }
    function sigma1(x) { return (((x >>>  6) | (x << 26)) ^ ((x >>> 11) | (x << 21)) ^ ((x >>> 25) | (x <<  7))); }
    function omega0(x) { return (((x >>>  7) | (x << 25)) ^ ((x >>> 18) | (x << 14)) ^ (x >>>  3)); }
    function omega1(x) { return (((x >>> 17) | (x << 15)) ^ ((x >>> 19) | (x << 13)) ^ (x >>> 10)); }
    const buf32 = new Array(64),
          hTemp = new Int32Array(8),
          totals = new Int32Array(2),
          hashed = new Uint32Array(H),
          o1 = omega0, o2 = omega1, s1 = sigma0, s2 = sigma1, t = hTemp, b = buf32; // Aliases
    function packChunk(i) { return o2(b[i - 2]) + b[i - 7] + o1(b[i - 15]) + b[i - 16]; }
    function hashVals(i = 0) {
        while (i < 64) {
            totals[0] = t[7] + s2(t[4]) + ch(t[4], t[5], t[6]) + K[i] + b[i++];
            totals[1] = s1(t[0]) + maj(t[0], t[1], t[2]);
            t[7] = t[6];
            t[6] = t[5];
            t[5] = t[4];
            t[4] = t[3] + totals[0];
            t[3] = t[2];
            t[2] = t[1];
            t[1] = t[0];
            t[0] = totals[0] + totals[1];
        }
    }
    function sumVals(i = 0) { while (i < 8) { hashed[i] = t[i] + hashed[i++] } }
    const stringBuf = new ArrayBuffer(((ls / 64 | 0) + 1) * 64),
          stringView = new DataView(stringBuf),
          bytes = new Uint8Array(stringBuf),
          words = new Int32Array(stringBuf);
    stringFillArray(str, bytes);
    bytes[ls] = 0x80;
    stringView.setUint32(bytes.length - 4, ls * 8);
    let i = 0, j;
    while (i < words.length) {
        j = 0;
        while (j < 16) { buf32[j] = stringView.getInt32((i + (j++)) * 4); }
        while (j < 64) { buf32[j] = packChunk(j++); }
        hTemp.set(hashed);
        hashVals();
        sumVals();
        i += 16;
    }
    i = 0;
    j = 1;
//  while (i < 8) { result[i] = hashed[i++].toString(16).padStart(8, "0") }
//  return result.join("");
    let result = ["sequence"];
    while (i < 8) { 
        let hi = hashed[i++]
        for (let k = 3; k >= 0; k -= 1) {
            let byte = hi & 0xff;
            result[j+k] = byte
            hi = (hi-byte) / 0x100;
        }
        j += 4;
    }
    return result;
}

