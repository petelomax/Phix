--with profile_time
--
-- bigint.e
-- ========
--
--  **ABANDONWARE** - totally outclassed by mpfr.e, before the first release.
--
--  An experimental cut-down version of bigatom.e, for integers only, with an eye
--  to (vastly) increasing performance. First, rather than base 10, hold numbers
--  in base #FFFF/#FFFFFFFF, so that a*b fits in e/rax, and hopefully most of the
--  maths can be done in #ilASM{}, in situ in sequences or allocated memory using
--  simple (e|r)si/di scans, and [almost?] no float-boxing or reference counting.
--  Secondly, keep an eye on any gains and back-port anything we can back to the
--  original bigatom library: if said gains either never get over 10% or drop to 
--  less than 10%, then we may as well bin this effort.
--
--  (incomplete) Some ideas for an integer-only module, much faster than bigatom.e
--  To do: (not yet attempted) test for performance against bigatom (and accuracy)
--         I am expecting there to be a fair amount of low-level assembly in here,
--         hammering straight down the vast bulk of a sequence of int, perhaps, or 
--         maybe just sticking to the usual sort of pre-allocated memory.
--
-- SUG: bi_new, like ba_new, and maybe bi_new_bin, which accepts a binary string? [DONE]
--
-- First up: A copy of bignum, which I rejected in favour of bigatom, but now see some merit (for ints).
--  (ERM, belay that thought. If this is still digit-by-digit then maybe not...)
--
--  1) bigints *DO NOT* have a base. They merely hold a number as a sequence of 
--      machine-word-sized integers. bi_new can however accept the standard
--      prefixes "0b/0o/0x/0(2)..0(36)" when passed a string.
--
--  2) Uses base 65536 (#10000), ie "digits" are 0..#FFFF. This is partly a compromise:
--      ideally all intermediates would remain integer, for maximum performance, but.
--      the only logical choice would then be base 256 (#100), which turns out slower.
--      [DEV try a "constant usebase256 = true" test]
--      OTOH, the intermediates /will/ all fit in eax when restorting to #ilASM.
--      In fact, the core reasoining behind an "all integer" approach is that it is
--      possible to ensure sufficient space in hll, then do all the actual maths in
--      #ilASM, with simple esi/edi scans rather than hll subscripts & refcounting.
--     64-bit could use base 4294967296 (#100000000), try that as well. [DEV]
--     64-bit uses base 4294967296 (#100000000), and is therefore twice as fast. [I wish]
--
--  3) Negative numbers, from first principles.
--      Since I know hex numbers quite well, lets construct some -ve base 16 nos.
--          -#00 = #00, ie  0, 0 =   0+ 0
--          -#01 = #FF, ie -1,15 = -16+15
--          -#02 = #FE, ie -1,14 = -16+14
--          -#0E = #F2, ie -1, 2 = -16+ 2
--          -#0F = #F1, ie -1, 1 = -16+ 1
--          -#10 = #F0, ie -1, 0 = -16+ 0
--          -#11 = #EF, ie -2,15 = -32+15
--      The top digit is signed, the rest are unsigned.
--      NAH: use {sign,digits} as per bignum, for the tried-and-tested code.
--
--  4) bi_sprintf
--      The traditional method is /10 to peel off least significant digits.
--      However, I think we can improve on that. 
--      My first idea was to print (say) 8 digits at a time, but of course
--      there are /10 buried deep inside printf. Anyway, it already felt 
--      like that would lead to significantly fewer divides. Next I thought
--      why not binary-split, recursively, and why use sprintf at all? So:
--      Make/keep a table of ten repeatedly squared, and how many bigint
--      blocks that requires:
--          [1] 10  1
--          [2] 100 1
--          [3] 10,000 1 (still < 65536)
--          [4] 100,000,000 2
--          [5] 10,000,000,000,000,000 ? ...
--      Find the first entry (extend as needed) with >=half the blocks of 
--      the number we wish to print. You can then split the task in half,
--      and use a recursive sub-function to print them, stitch together,
--      and strip leading zeroes, unless zero fill is required anyway.
--      Obviously the resursion stops at [1], as we have the final digit.
--      That should drastically cut the total number of divides required [test me]
--      Requires bi_div to be fully operational first, so do /10 for now.
--

--
-- structure of a bigint
--
enum SG_NOVALUE = -2,
     SG_MINUS   = -1,
     SG_ZERO    =  0,
     SG_PLUS    = +1

-- a bigint is always a sequence of length 2
enum SIGN,      -- one of those SG_xxx
     DIGITS     -- positive integer in base 65536,  (erm,WORD)
                -- most significant digit first,
                -- eg 65536 is held as {1,0}
                --    65538 is held as {1,2}
                -- leading zeroes shd be stripped

--constant WORD = iff(machine_bits()=32?#FFFF:#FFFFFFFF)
--DEV borken on 64-bit...
--constant WORD = iff(machine_bits()=32?#10000:#100000000)
--constant WORD = #10000
--?{"WORD",WORD} -- {"WORD,0}...
--atom WORD
--  if machine_bits()=32 then
--      WORD=#10000
--  else
----        WORD=#100000000
--      WORD=#10000*#10000
--  end if
--?{"WORD",WORD}
-- (at last, found something that actually works...)
atom WORD = #10000
    if machine_bits()=64 then WORD *= WORD end if


without type_check
without trace

type t_sign(object s)
    return integer(s) and (s=SG_NOVALUE or s=SG_MINUS or s=SG_ZERO or s=SG_PLUS)
end type

type t_digits(object x)
    if atom(x) or string(x) then
        return false
    end if
--  for i=1 to length(x) do
--      object xi = x[i]
----X   if not integer(xi) or xi<0 or xi>#FFFF then
--      if not integer(xi) or xi<0 or xi>=WORD then
--          return false
--      end if
--  end for
    if length(x)>1 and x[1]=0 then
        return false
    end if
    return true
end type

-- NO_VALUE: not a real number.
-- yielded when a result is undefined, indeterminate, complex, ...
-- eg.  division by zero, logarithm of zero or negative, following a negative ...
constant NO_VALUE = {SG_NOVALUE, {}}

bool allow_NOVALUE = false

global procedure bi_allow_novalue(bool flag)
    allow_NOVALUE = flag
end procedure

global type bigint(object x)
    if sequence(x)
    and not string(x)
    and length(x)=DIGITS
    and t_digits(x[DIGITS])
    and t_sign(x[SIGN])
    and (allow_NOVALUE or x!=NO_VALUE) then
        return true
    end if
    return false
end type

type base(object b)
    return integer(b) and b>=2 and b<=36
end type
with trace

--global constant BI_ZERO = {SG_ZERO, {}}  -- BI_ZERO = normalize({0,0,{0}})[DEV??]
global constant BI_ZERO = {SG_ZERO, {0}}  -- BI_ZERO = ba_new(0)
global constant BI_ONE  = {SG_PLUS, {1}}  -- BI_ONE = ba_new(1)
global constant BI_TWO  = {SG_PLUS, {2}}  -- BI_TWO = ba_new(2)

--global type bigint(object n)
--  if atom(n) 
--  or string(n)
--  or length(n)!=DIGITS then 
--      return false
--  end if
--  object ni = n[1]
----    if not integer(ni) or ni<-1 or ni>#FFFF then
--  if not integer(ni) or ni<-#8000 or ni>#7FFF then ??
--      return false
--  end if
--  for i=2 to length(n) do
--      ni = n[i]
----X   if not integer(ni) or ni<0 or ni>#FFFF then
--      if not integer(ni) or ni<0 or ni>=WORD then
--          return false
--      end if
--  end for
--  return true
--end type

function strip_lz(sequence n)
--
-- strips leading zeros (helpful/necessary for bi_compare, at least?)
--
    integer ln = length(n)
    if ln>1 and n[1]=0 then
        for i=2 to ln do
            if n[i]!=0 or i=ln then
                n = n[i..$]
                exit
            end if
        end for
    end if
    return n
end function

--DEV is this a better approach? [we need both base 65536[?] and string (leading) zero-strippers anyway]
function strip_zeros(string s)
    integer zdx = 1, ls = length(s)
    while ls>zdx and s[zdx]='0' do
        zdx += 1
    end while
    if zdx>1 then
        s = s[zdx..$]
    end if
    return s
end function

function bi_normalize(sequence n)
--
-- Standardizes a bigint.
--
    n[DIGITS] = strip_lz(n[DIGITS])
--erm or {}??
    if n[DIGITS]={0} then
        n[SIGN] = SG_ZERO 
    end if
--      
--  if n[SIGN]<SG_MINUS or n[SIGN]>SG_PLUS then
--      return NO_VALUE
--  end if
--
--  sequence mantissa = n[DIGITS]
--  integer first = 1,
--          last = length(mantissa)
--  while last and mantissa[last]=0 do
--      last -= 1
--  end while
--  while first<=last and mantissa[first]=0 do
--      first += 1
--  end while
--
--  if first>last or not n[SIGN] then
--      n = BA_ZERO
--  else
--      n[DIGITS]  = mantissa[first..last]
--X     n[EXPONENT] -= first-1
--  end if
--
    return n
end function


global function convert_base(string s, base inb, outb)
--
-- Convert string s to a different base, eg
--          convert_base("65535",10,16) => "FFFF"
-- NB: s must not be negative or contain any fractional part.
--
--DEV see also bn_baseconv...
-- Note this may be a tad slow: it performs string-maths - converting a 100-digit
--  input string is likely to perform some 10,000 individual mod and floor_div ops
--  - which should not be a problem as long as it is kept out of tight inner loops.
--  It is intended for use in bi_new (only) and therefore we are not expecting any
--  1,000-digit strings, let alone any 1,000,000-digit ones...
--DEV/SUG:
--  [If really needed, it may help to convert s to inb^m blocks, where m is the
--   maximum safe power of inb eg/ie a base 10 number of "1234567890", assuming 
--   m=9, then first convert to (the integers) {1,234567890}, for the same-as-is 
--   overhead for the first digit, but then fewer-per-output-digit mod/div. (???)]
--   [perhaps, I should say for completeness, skipping that when length(s)<m/?2??]
--
--  On the plus side it can theoretically handle 800 million+ digits with perfect
--  accuracy, far more on 64 bit, if you have enough RAM and patience that is ;-)
--
    string res = "" 
    while length(s) do
        integer q = 0, r = 0, digit
        for i=1 to length(s) do
--          digit = upper(s[i]) -- (already taken care of)
            digit = s[i]
            digit -= iff(digit>='A'?'A'-10:'0')
            if digit<0 or digit>=inb then ?9/0 end if
            q = q*inb+digit
            r = mod(q,outb)
            q = floor(q/outb)
            q += iff(q>=10?'A'-10:'0')
            s[i] = q
            q = r
--          {s[i],q} = {
        end for
        r += iff(r>=10?'A'-10:'0')
        res &= r    -- (reverse order)
        while length(s) and s[1]='0' do
            s = s[2..$]
        end while
    end while
    return reverse(res)
end function

--?convert_base("65535",10,16)      --> FFFF
--?convert_base("FFFF",16,10)       --> 65535
--?convert_base("65535",10,3)       --> 10022220020
--?convert_base("10022220020",3,10) --> 65535
--?convert_base("FFFF",16,8)        --> 177777
--?convert_base("177777",8,16)      --> FFFF
--?convert_base("7",8,10)           --> 7
--?convert_base("7",10,2)           --> 111
--?convert_base("111",2,10)         --> 7
--independently verified:
--?convert_base("EC851A69B8ACD843164E10CFF70CF9E86DC2FEE3CF6F374B43C854E3342A2F1AC3E30C741CC41E679DF6D07CE6FA3A66083EC9B8C8BF3AF05D8BDBB0AA6CB3EF"&
--              "8C5BAA2A5E531BA9E28592F99E0FE4F95169A6C63F635D0197E325C5EC76219B907E4EBDCD401FB1986E4E3CA661FF73E7E2B8FD9988E753B7042B2BBCA76679",16,10)
--?convert_base("1660899461379861685353688491843017402046137536931563604625752175601309049219539763248397828080182770002"&
--              "9602706087374780329179786968451649489474169926767424688162265865426713125047095658790838544704431992304"&
--              "0838072975636163137212887824248575510341104029461758594855159174329892125993844566497176102668262139513",10,16)

function from_base(string s, base b)
--
-- Convert the string representation in base b, s, to a normal integer.
--            eg from_base("1234",10) ==> 1234 (integer)
--  (Actually, of course, the numbers/results do not have a base and
--   are neither binary nor decimal, only their string [and physical 
--   hardware] representations can posess that particular quality!)
--
-- Typically we expect the input (here) to be "0000".."FFFF", 16 or
--  the equivalent in binary, or the "e" part of eg "1e250",10, as 
--  used within this module. Nowt near 15/19 digits-worth, though.
--
integer res = 0, c
    for i=1 to length(s) do
--      c = upper(s[i]) -- (already taken care of)
        c = s[i]
--      if c<='9' then
--          c -= '0'
--      else
--          c -= 'A'-10
--      end if
        c -= iff(c<='9'?'0':'A'-10)
        if c<0 or c>=b then ?9/0 end if
        res = res*b+c
    end for
    return res
end function

global function bi_new(object n)
--
-- n can be integer, atom, human-readable string, or bigint
-- Convert the parameter n into a bigint. Note that atoms should be limited
-- to +/-9007199254740992 (power(2,53), #20_0000_0000_0000) on 32 bit, and
-- to +/-18446744073709551616 (power(2,64)) on 64-bit, since those are the
-- limits that a 64/80 bit floating point value can represent exactly (if
-- the binary/hex representation does not end in lots of zeros, that is).
-- Obviously n should not contain any fractional part (atom or string).
-- Should n already be a bigint, it is returned completely unharmed.
--
-- Examples
--
--      bigint n = bi_new(1)
--             n = bi_new("1,000,000,000,000,000")
--             n = bi_new("9,007,199,254,740,992")
--             n = bi_new("#20000000000000")
--             n = bi_new("0(16)20000000000000")
--             n = bi_new("0b0101")
--             n = bi_new("1e250")
--
--  Note: if you have a binary string, eg x"FFFF", use bi_new_bin() instead.
--  Commas(',') and underscores ('_') are ignored, decimal points are fatal 
--  except in base 10 when there is an "e" part that cancels them out, eg
--  "1.75e2"(=="175"), and negative exponents are always fatal (including 
--  eg "1000e-3", != "1" here).
--  Bases 2 to 36 are supported, by prefixing the string in all the same manners 
--  that standard integer arguments can be specified in different bases, eg '#'.
--  Exponents can only be specified on base 10 integers, and as mentioned >=0.
--
sequence res
bool neg = false

    if atom(n) then

        neg = n<0
        if neg then n = -n end if
        res = {}
        while 1 do
--          integer low = mod(n,#10000)
            integer low = mod(n,WORD)
            res = prepend(res,low)
--          n = floor(n/#10000)
            n = floor(n/WORD)
            if n=0 then exit end if
        end while
        res = {iff(neg?SG_MINUS:SG_PLUS),res}

    elsif string(n) then

        neg = (n[1]='-')
        if neg then n = n[2..$] end if
        -- allow eg "1.75e2", as long as that 2>=length("75")
--      integer dot = find('.',n)
--      if dot then ?9/0 end if
        if n="" then ?9/0 end if -- (or return BI_ZERO or NO_VALUE?)
        --
        -- First check for a base (2..36 allowed)
        --
        base b = 10
        if n[1]='#' then
            -- eg "#05"
            b = 16
            n = n[2..$]
        elsif length(n)>=2 and n[1]='0' then
            integer k=find(n[2],"boxt(") -- binary/octal/hex/octal/explicit
            if k then
                if k=5 then
                    -- eg "0(3)102202001"
                    integer l = find(')',n)
                    if l<4 or l>5 then ?9/0 end if
                    b = n[l-1]-'0'
                    if l=5 then
                        b += (n[3]-'0')*10
                    end if
                    if b<2 or b>36 then ?9/0 end if
                    n = n[l+1..$]
                else
                    -- eg "0b0101" or "0x05"
                    b = {2,8,16,8}[k]
                    n = n[3..$]
                end if
            end if
        end if
        integer dot = find('.',n)
        if dot then
            if b!=10 then ?9/0 end if
            n[dot..dot] = ""
        end if
        if n="" then ?9/0 end if -- (just "#" or "0b" etc - die die die!)
        --
        -- Quickly check they are now all valid digits
        --
        n = upper(n)
        for i=length(n) to 1 by -1 do
            integer c = n[i]
            if b=10 and (c='e' or c='E') then
                if n[i+1]='-' then ?9/0 end if  -- illegal!
                integer z = from_base(n[i+1..$],10)
                if dot then
                    z -= (i-dot)
                    if z<0 then ?9/0 end if
                    dot = 0
                end if
                n[i..$] = repeatch('0',z)
            elsif c=',' or c='_' then
                n[i..i] = ""
            else
                c -= iff(c<='9'?'0':'A'-10)
                if c<0 or c>b then ?9/0 end if
            end if
        end for
        if dot then ?9/0 end if
        --
        -- Convert to base 16 unless 2 or already 16, so that we can chop
        --  out individual blocks of bits/nibbles that much more easily.
        --  (be not thou tempted by octal, for it hath wrong boundaries)
        --
        integer d, pad
if b=2 or b=16 then
        --
        -- old/original code: I thought I had a really clever idea, but
        -- for decimal strings the convert_base proved quiet appallingly
        -- slow... Anyway, it still seems like the right thing to do for
        -- binary/hex strings, not that they are very common, or that I
        -- have actually bothered to time thi against that below...
        --
        if b=2 then
--          d = machine_bits()/2    -- 2/4 bytes is 16/32 bits/digits
            d = 16                  -- 2 bytes is 16 bits/digits
        else
--          if b!=16 then
--              n = convert_base(n,b,16)
--              b = 16
--          end if
--          d = machine_word()      -- 2/4 bytes is 4/8 digits
            d = 4                   -- 2 bytes is 4 digits
        end if
        --
        -- Pad to a multiple of 2/4 bytes, 16/32 bits:
        --
        pad = mod(length(n),d)
        if pad!=0 then
            n = repeatch('0',d-pad) & n
        end if
        --
        -- Then convert blocks of 16/32 bits:
        --
        res = {}
        for i=1 to length(n) by d do
            res &= from_base(n[i..i+d-1],b)
        end for
--      res = reverse(res)  -- NO!
else
        res = {0}
        for i=1 to length(n) do
            integer carry = n[i]    -- (already upper)
--          carry -= iff(carry<='9'?'0':'A'-1)
            carry -= iff(carry<='9'?'0':'A'-10)
            for j=length(res) to 1 by -1 do
                atom digit = res[j]*b+carry
                carry = 0
--              if digit>65535 then
                if digit>=WORD then
--                  carry = floor(digit/65536)
                    carry = floor(digit/WORD)
--                  digit = remainder(digit, 65536)
                    digit = remainder(digit, WORD)
                end if
--              if digit<0 or digit>#FFFF then ?9/0 end if
                if digit<0 or digit>=WORD then ?9/0 end if
                res[j] = digit
            end for
            if carry then
--              if carry<0 or carry>#FFFF then ?9/0 end if
                if carry<0 or carry>=WORD then ?9/0 end if
                res = prepend(res, carry)
            end if
        end for
end if
        res = {iff(neg?SG_MINUS:SG_PLUS),res}
    else
        res = n
    end if
    if not bigint(res) then ?9/0 end if
    return bi_normalize(res)
end function


--for i=1 to 100 do
--  bigint z = bi_new("12345679012345679012345679012345679012345679012345679012345679012345679e0")
--end for

--DEV methinks byte order may well be wrong, eg
-- ?bi_sprint10(bi_new_bin(x"0000FEFF")) -> 65534
-- ?bi_sprint10(bi_new_bin(x"01000200")) -> 65538
-- compare/contrast with:
-- atom mem = allocate(4); poke4(mem,65538); 
--  ?peek(mem) -> 2; ?peek({mem,4}) -> {2,0,1,0}
--I shd dig out some concrete examples from my crypto stuff.
-- (aka WARNING: byte order is not yet cast in stone...)

global function bi_new_bin(string n, bool bPad=false, integer padch=#80)
--
-- n should be a binary string, eg x"FFFFFFFF", or perhaps the 
--  output from something like sha_512() or a similar peek().
-- It must be a multiple of 4 or 8 bytes, unless bPad is true.
-- When bPad is true, leading 0/-1 bytes are inserted as needed.
-- NB: If n is unsigned with the high bit set, it'll pad wrong,
--     unless for that one specific case you also set padch 0.
--
-- Example: bi_new_bin(x"FFFF") === bi_new("#FFFF")
--
atom pN
    if n="" then ?9/0 end if -- (or maybe return {0})
    integer padding = mod(length(n),machine_word())
--  integer padding = mod(length(n),4)
    if padding!=0 then
--?{"padding",padding}
        if not bPad then ?9/0 end if
--DEV syntax colouring in Edix (!=Edita, where it ain't right either)):
--      integer ch = iff(and_bits(n[1],#80)?'xFF':'\0')
--      integer ch = iff(and_bits(n[1],#80)?'\xFF':'\0')
        if padch=#80 then
            padch = iff(and_bits(n[1],#80)?'\xFF':'\0')
        end if
        n = repeatch(padch,machine_word()-padding) & n
    end if
    #ilASM{ -- pN := raw_address(n)
        [32]
            mov eax,[n]
            shl eax,2
            mov [pN],eax
        [64]
            mov rax,[n]
            shl rax,2
            mov [pN],rax
        }
    integer b = machine_word()/2,   -- (ie {#0000..#FFFF}'s on 32 bit)
            blocks = length(n)/b
--NO, we need to treat as big-endian... (??)
--  sequence res = {SG_PLUS,peekNS({pN,blocks},b,false)}
--/!*
    sequence bytes = peek({pN,length(n)})
--?bytes
    sequence res = {SG_PLUS,repeat(0,blocks)}
--  for i=1 to blocks by b do
    integer bdx = 1
    for i=1 to blocks do
--      res[DIGITS][i] = bytes_to_int(bytes[i..i+b-1],false)
        integer d = 0
        for j=1 to b do
            d = d*#100 + bytes[bdx]
            bdx += 1
        end for
--      res[DIGITS][i] = bytes[i*2-1]*#100+bytes[i*2]
        res[DIGITS][i] = d
--      res[DIGITS][i] = bytes_to_int(reverse(bytes[i..i+b-1]),false)
    end for
--*!/
    return bi_normalize(res)
end function

global function bi_bin(bigint n)
-- reverse of bi_new_bin, return a binary string from n
    integer b = machine_word()/2
    string res = "",
           word = repeat(' ',b)
    for i=1 to length(n[DIGITS]) do
        integer d = n[DIGITS][i]
        for j=1 to b do
            word[-j] = and_bits(d,#FF)
            d = floor(d/#100)
        end for
        res &= word
    end for
    return res
end function

--/*
--erm...
--function add_digit(sequence a, integer carry)
--  integer digit
--  --
--  -- add shorter integer, eg (9)909 + 101 ==> (9)010 carry 1,
--  -- and then propagate any carry down remaining a as rqd.
--  --  (the -ve index is simply going from right to left)
--  --
--  for i=-1 to -length(a) by -1 do
--      if carry=0 then exit end if
--      digit = a[i]+carry
----X   carry = digit>65535
--      carry = digit>=WORD
----X   if carry then digit -= 65536 end if
--      if carry then digit -= WORD end if
--      a[i] = digit
--  end for
--  -- lastly extend with any carry (always===1, or none)
--  if carry then a = 1&a end if
--  return a
--end function

--function digits_shift(sequence a, integer carry, outbase)
--end function

--function bi_baseconv(string s, base frombase)--, integer tobase=#10000)
function bi_baseconv(string s, base frombase)--, integer tobase=WORD)
--integer carry
--atom digit        -- may (temporarily) be #00000000 to #FFFFFFFF, I think...

    sequence res = {0}
    for i=1 to length(s) do
        integer carry = upper(s[i])
        carry -= iff(carry<='9'?'0':'A'-1)
        for j=lenth(res) to 1 by -1 do
            atom digit = res[j]*frombase+carry
            carry = 0
--          if digit>65535 then
            if digit>=WORD then
--              carry = floor(digit/65536)
                carry = floor(digit/WORD)
--              digit = remainder(digit, 65536)
                digit = remainder(digit, WORD)
            end if
--          if digit<0 or digit>#FFFF then ?9/0 end if
            if digit<0 or digit>=WORD then ?9/0 end if
            res[j] = digit
        end for
        if carry then
--          if carry<0 or carry>#FFFF then ?9/0 end if
            if carry<0 or carry>=WORD then ?9/0 end if
            res = prepend(resa, carry)
        end if
    end for
    return res
end function
--*/

global function bi_compare(object a,b)
--
-- as per compare, returns: -1 (a<b) | 0 (a=b) | +1 (a>b)
-- a, b can be integer, atom, human-readable string, or bigint.
--
    if not bigint(a) then a = bi_new(a) end if
    if not bigint(b) then b = bi_new(b) end if

    integer cmp = compare(a[SIGN], b[SIGN])
    if cmp=0 then
        cmp = compare(length(a[DIGITS]), length(b[DIGITS]))
        if cmp=0 then
--          cmp = compare(reverse(a[DIGITS]), reverse(b[DIGITS]))   --NO!
            cmp = compare(a[DIGITS], b[DIGITS])
        end if
--      if a[SIGN]=SG_MINUS and b[SIGN]=SG_MINUS then
        if a[SIGN]=SG_MINUS then -- (no need to test both)
            cmp = -cmp
        end if
    end if
    return cmp
end function

global function bi_equal(object a,b)
    return bi_compare(a,b)==0
end function

global function bi_neg(object n)
--
-- negate (unary minus) argument, return: a bigint
-- n can be integer, atom, human-readable string, or bigint.
--
    if not bigint(n) then n = bi_new(n) end if
--DEV erm, SG_NOVALUE??
    n[SIGN] *= -1
    return n
end function

global function bi_sign(object n)
--
-- as per sign() but for bigint. Returns -1 (n<0) / 0 (n=0) / +1 (n>0)
-- n can be integer, atom, human-readable string, or bigint.
--

    return bi_compare(n,BI_ZERO)
end function

global function bi_abs(object n)
--
-- as per abs() but for bigint. Returns a bigint.
-- n can be integer, atom, human-readable string, or bigint.
--
    if not bigint(n) then n = bi_new(n) end if
    if n[SIGN]=SG_MINUS then
        n[SIGN] = SG_PLUS
    end if
    return n
end function

function digits_add(sequence a, b, bool bRetCarry=false, integer icarry=0)
--
-- adds two sequences of digits
-- returns a sequence of digits, or in the special case for digits_multiply(),
--         which is the only thing that sets bRetCarry, a sequence of length
--         two [{carry, digits}, see digits_multiply() for more details].
-- internal routine: a, b typically derived from bigint[DIGITS].
--
    -- first, swap if needed to make length(a)>=length(b)
    if length(b)>length(a) then
        {a,b} = {b,a}
    end if
    integer mlb = -length(b), carry = 0, digit
    --
    -- add shorter integer, eg (9)909 + 101 ==> (9)010 carry 1,
    -- and then propagate any carry down remaining a as rqd.
    --  (the -ve index is simply going from right to left)
    --
    for i=-1 to -length(a) by -1 do
        digit = a[i]+carry
        if i<mlb then
            if carry=0 then exit end if
        else
            digit += b[i]
        end if
--      carry = digit>65535
        carry = digit>=WORD
--      if carry then digit -= 65536 end if
        if carry then digit -= WORD end if
        a[i] = digit
    end for
    -- lastly extend with any carry (always===1, or none)
    if carry then a = 1&a end if
    -- for digits_multiply(), only:
    if bRetCarry then return {carry+icarry,a} end if
    return a
end function

function complement_digits(sequence n)
-- internal routine: as per 2's complement, but base 65536 (?).
--  n = sq_sub(#FFFF,n)
    n = sq_sub(WORD-1,n)
    n = digits_add(n,{1})
    return n
end function

function digits_sub(sequence a, b)
--
-- subtracts two sequences of digits
-- returns a sequence of two elements: {carry, result}
-- if carry is not zero it indicates that the result is negative.
-- internal routine: a, b typically derived from bigint[DIGITS].
--
    -- pad a when shorter than b
    integer rqz = length(b)-length(a)
    if rqz>0 then
        a = repeat(0,rqz)&a
    end if
    -- subtract with carry to length(b), then
    -- propagate carry down any remaining a
    integer mlb = -length(b), carry = 0, digit
    for i=-1 to -length(a) by -1 do
        digit = a[i]-carry
        if i<mlb then
            if carry=0 then exit end if
        else
            digit -= b[i]
        end if
        carry = digit<0
        if carry then
--          digit += 65536
            digit += WORD
        end if
        a[i] = digit
    end for
    if carry then a = complement_digits(a) end if
    return {carry,a}
end function

function digits_multiply(sequence a, sequence b)
--
-- multiplies two sequences of digits
-- returns a sequence of digits
-- internal routine: a, b typically derived from bigint[DIGITS].
--
integer lena = length(a), lenb = length(b)
sequence partial, result = {}
atom digit      -- may (temporarily) be #00000000 to #FFFFFFFF, I think...
integer carry

    -- swap if needed to make length(a)>=length(b)
    if lena<lenb then
        {a, lena, b, lenb} = {b, lenb, a, lena}
    end if

    for i=lenb to 1 by -1 do
        carry = 0
--DEV/SUG (spotted in passing) if b[i]!=0? (might need a bi_normalise in ba_multiply?)
        partial = sq_mul(a,b[i]) & repeat(0, lenb-i)
        for j=lena to 1 by -1 do
            digit = partial[j]+carry
            carry = 0
--          if digit>65535 then
            if digit>=WORD then
--              carry = floor(digit/65536)
                carry = floor(digit/WORD)
--              digit = remainder(digit, 65536)
                digit = remainder(digit, WORD)
            end if
--          if digit<0 or digit>#FFFF then ?9/0 end if
            if digit<0 or digit>=WORD then ?9/0 end if
            partial[j] = digit
        end for
        if carry then
--          if carry<0 or carry>#FFFF then ?9/0 end if
            if carry<0 or carry>=WORD then ?9/0 end if
            partial = prepend(partial, carry)
        end if
        {carry,result} = digits_add(result, partial, true, carry)
    end for

    return result
end function

global function bi_add(object a, b)
--
-- add a and b using the 'longhand' method
-- Returns: a bigint
-- a, b can be integer, atom, human-readable string, or bigint.
--
    if not bigint(a) then a = bi_new(a) end if
    if not bigint(b) then b = bi_new(b) end if

    integer signA = a[SIGN],
            signB = b[SIGN]
    if signA=SG_ZERO then
        return b
    elsif signB=SG_ZERO then
        return a
    elsif signA=SG_NOVALUE or signB=SG_NOVALUE then
        return NO_VALUE
    end if

    integer sgn = SG_PLUS
    sequence res
    if signA=signB then
        res = digits_add(a[DIGITS], b[DIGITS])
        sgn = signA
    else
        integer carry
        if signA=SG_MINUS then
            {carry,res} = digits_sub(b[DIGITS], a[DIGITS])
        else -- signB=SG_MINUS
            {carry,res} = digits_sub(a[DIGITS], b[DIGITS])
        end if
        if carry then sgn = SG_MINUS end if
    end if
    res = bi_normalize({sgn, res})
    return res
end function

global function bi_sub(object a, b)
--
-- subtract b from a using the 'longhand' method
-- Returns: a bigint
-- a, b can be integer, atom, human-readable string, or bigint.
--
    if not bigint(a) then a = bi_new(a) end if
    if not bigint(b) then b = bi_new(b) end if

    integer signA = a[SIGN],
            signB = b[SIGN]
    if signA=SG_ZERO then
        b[SIGN] = -signB
        return b
    elsif signB=SG_ZERO then
        return a
    elsif signA=SG_NOVALUE
       or signB=SG_NOVALUE then
        return NO_VALUE
    end if

    if signA=SG_MINUS 
    or signB=SG_MINUS then
        b[SIGN] = -signB
        return bi_add(a, b)
    end if

    {integer carry,sequence res} = digits_sub(a[DIGITS], b[DIGITS])
    integer sgn = iff(carry?SG_MINUS:SG_PLUS)
    return bi_normalize({sgn, res})
end function

global function bi_mul(object a, b)
--
-- multiply arguments using the 'longhand' method
-- Returns: a bigint
-- a, b can be integer, atom, human-readable string, or bigint.
--
    if not bigint(a) then a = bi_new(a) end if
    if not bigint(b) then b = bi_new(b) end if

    integer signA = a[SIGN],
            signB = b[SIGN],
            signR = signA*signB
    sequence res
    if signA=SG_ZERO
    or signB=SG_ZERO then
        res = BI_ZERO
    elsif signA=SG_NOVALUE
       or signB=SG_NOVALUE then
        res = NO_VALUE
    else
        sequence digsA = a[DIGITS],
                 digsB = b[DIGITS]
        if digsA={1} then       -- a = +/-1
            res = digsB
        elsif digsB={1} then    -- b = +/-1
            res = digsA
        else
            res = digits_multiply(digsA, digsB)
        end if
        res = {signR,res}
    end if
    return res
end function

--
-- Returns the result of integer division of two numbers.
--
-- Supports Atoms, bigatoms and representation of numbers in a string
--
-- Returns {Quotient, Remainder}
---
--/* GULP: on one test this was just over *400* times slower than next!
global function bi_divX(object a, b)
    if not bigint(a) then a = bi_new(a) end if
    if not bigint(b) then b = bi_new(b) end if

    integer signA = a[SIGN],
            signB = b[SIGN]
    if signB=SG_ZERO 
    or signB=SG_NOVALUE 
    or signA=SG_NOVALUE then
        return NO_VALUE
    elsif signA=SG_ZERO then
        return BI_ZERO
    end if

    integer signR = signA*signB
    sequence res, rmdr
    if equal({1}, b[DIGITS]) then   -- +/-1
        res = {signR, a[DIGITS]}
        rmdr = BI_ZERO
    else
        a[SIGN] = SG_PLUS
        b[SIGN] = SG_PLUS

        sequence quotient = {}, 
                 partial = {SG_PLUS,{}}, -- +0
                 digsA = a[DIGITS]

        for i=1 to length(digsA) do
            partial[DIGITS] &= digsA[i]
            quotient &= 0
            partial = bi_normalize(partial)
--DEV this is probably ok for base 10, not so much for base 65536...
            while bi_compare(partial, b)!=SG_MINUS do
                quotient[$] += 1
                partial = bi_sub(partial, b)
            end while
            partial[SIGN] = SG_PLUS -- if it has zeroed
        end for
        res = bi_normalize({signR, quotient})
        rmdr = bi_normalize(partial)
    end if

    return {res,rmdr}
end function
--*/

--erm, borken...
--/*
integer maxdiv = 1000

--  global function bi_div(object dividend, divisor)
--  ?{"bi_div",dividend,divisor}
--  maxdiv-=1
--  if maxdiv<=0 then ?9/0 end if
--  --
--  -- divide dividend by divisor using the 'longhand' method
--  -- Returns: two-element sequence containing quotient and remainder,
--  --          as in {quotient,remainder} == dividend/divisor
--  -- divident, divisor can be integer, atom, human-readable string, or bigint.
--  --
--      if not bigint(dividend) then dividend = bi_new(dividend) end if
--      if not bigint(divisor)  then divisor  = bi_new(divisor)  end if
--
--      integer signA = dividend[SIGN],
--              signB = divisor[SIGN]
--      if signB=SG_ZERO 
--      or signB=SG_NOVALUE 
--      or signA=SG_NOVALUE then
--          return {NO_VALUE,NO_VALUE}
--      elsif signA=SG_ZERO then
--          return {BI_ZERO,BI_ZERO}
--      end if
--
--      integer signR = signA*signB
--      sequence res
--      if equal({1}, divisor[DIGITS]) then -- division by +/-1
--          res = {{signR, dividend[DIGITS]},BI_ZERO}
--      else
--          integer divisorlen = length(divisor[DIGITS]),
--                  dividendlen = length(dividend[DIGITS])
--
--          if dividendlen<divisorlen then
--              return {BI_ZERO, dividend}
--          end if
--
--          dividend[SIGN] = SG_PLUS
--          divisor[SIGN] = SG_PLUS
--
--          bigint trialSub, curdividend
--          sequence curdivdigits, Q = {}, gap, bucket
--          integer trialQ, next, direction, trialmatch, divisorFirst, offset
--
--          if divisorlen>1 then
--              curdivdigits = dividend[DIGITS][1..divisorlen-1]
--              next = divisorlen
--          else
--              curdivdigits = {}
--              next = 1
--          end if
--
--          divisorFirst = divisor[DIGITS][1]
--
--          while next<=dividendlen do   -- get quotient digits
--              curdivdigits &= dividend[DIGITS][next]
--              curdividend = bi_normalize({SG_PLUS,curdivdigits})
--              next += 1
--              if length(curdivdigits)=1 then
--                  trialQ = floor(curdivdigits[1]/divisorFirst)
--              else
--  --              atom c1c2 = curdivdigits[1]*65536+curdivdigits[2] -- (max #FFFFFFFFF)
--                  atom c1c2 = curdivdigits[1]*WORD+curdivdigits[2] -- (max #FFFFFFFFF)
--                  trialQ = floor(c1c2/divisorFirst)
--  --              if trialQ>=65536 then
--                  if trialQ>=WORD then
--  --                  trialQ = 65535
--                      trialQ = WORD-1
--                  end if
--              end if
--              trialSub = bi_mul(divisor, trialQ)
--              gap = bi_abs(bi_sub(trialSub, curdividend))
--              if bi_compare(gap, divisor)>0 then
--                  bucket = bi_div(gap, divisor)
--              else
--                  bucket = {BI_ZERO,BI_ZERO}
--              end if
--              if length(bucket[1][DIGITS])=1 then
--                  offset = bucket[1][DIGITS][1]
--              else
--  --              offset = 65535
--                  offset = WORD-1 -- (???)
--              end if
--
--              direction = +1      -- up
--
--              while offset>1 do   -- zero in on the current quotient
--                  while offset>trialQ do
--                      offset = floor(offset/2)
--                  end while
--
--                  if direction=+1 then    -- up
--                      trialQ += offset
--                  else
--                      trialQ = abs(trialQ-offset)
--                  end if
--
--                  trialSub = bi_mul(trialQ, divisor)
--                  trialmatch = bi_compare(trialSub, curdividend)
--                  if trialmatch=direction then
--                      offset = floor(offset/2)
--                      direction = direction*(-1)
--                  elsif trialmatch=0 then
--                      exit
--                  end if
--              end while
--
--              -- final adjustments to current quotient
--              while bi_compare(trialSub, curdividend)<0 do
--                  trialQ += 1
--                  trialSub = bi_mul(trialQ, divisor)
--              end while
--              while bi_compare(trialSub, curdividend)>0 do
--                  trialQ -= 1
--                  trialSub = bi_mul(trialQ, divisor)
--              end while
--
--  --          if trialQ<0 or trialQ>#FFFF then ?9/0 end if
--              if trialQ<0 or trialQ>=WORD then ?9/0 end if
--              Q = Q & trialQ
--              curdividend = bi_abs(bi_sub(curdividend,trialSub))
--              curdivdigits = curdividend[DIGITS]
--          end while
--
--          curdividend[SIGN] = signA
--          res = {bi_normalize({signR,Q}), bi_normalize(curdividend)}
--      end if
--      return res
--  end function
--*/

--DEV rename as bi_divrem()
global function bi_div3(object dividend, divisor, bool bFloor=false)
--
-- divide dividend by divisor using a 'bitshift' method.
--
-- Returns: two-element sequence containing {quotient,remainder},
--          such that quotient*divisor+remainder == dividend,
--          obviously with the highest possible quotient, given
--          that remainder >= 0 (that is for bi_abs()'d dividend 
--          and divisor, with signs adjusted last of all).
--          Note that this matches bi_rmdr() rather than bi_mod().
--
-- divident, divisor can be integer, atom, human-readable string, or bigint.
--
    if not bigint(dividend) then dividend = bi_new(dividend) end if
    if not bigint(divisor)  then divisor  = bi_new(divisor)  end if

    integer signA = dividend[SIGN],
            signB = divisor[SIGN],
            signR = signA*signB
    sequence res
    if signB=SG_ZERO 
    or signB=SG_NOVALUE 
    or signA=SG_NOVALUE then
        res = {NO_VALUE,NO_VALUE}
    elsif signA=SG_ZERO then
        res = {BI_ZERO,BI_ZERO}
    elsif equal({1}, divisor[DIGITS]) then -- division by +/-1
        res = {{signR, dividend[DIGITS]},BI_ZERO}
    else
        dividend[SIGN] = SG_PLUS
        divisor[SIGN] = SG_PLUS
        --
        -- divisor is doubled until >= dividend, saving each 
        -- doubling in a work array. We then climb back down 
        -- that array finding the largest sum*divisor that is
        -- still <= dividend. Based on my own bi_sqrt() method, 
        -- and again I would not be surprised if there is a much
        -- better way, but at least there are very few divides.
        --
        sequence dblbits = {},  -- 1,2,4,8,16,etc.
                 doubles = {}   -- divisor*""
        bigint dblbitt = BI_ONE
        while bi_compare(divisor,dividend)<=0 do
            dblbits = append(dblbits,dblbitt)
            doubles = append(doubles,divisor)
            dblbitt = bi_add(dblbitt,dblbitt)
            divisor = bi_add(divisor,divisor)
        end while
        res = BI_ZERO
        bigint rmdr = dividend
        dblbitt = BI_ZERO
        for d=length(doubles) to 1 by -1 do
            bigint trial = bi_add(dblbitt,doubles[d]),
                   diff = bi_sub(dividend,trial)
            integer c = bi_compare(diff,BI_ZERO)
            if c>=0 then
                dblbitt = trial
                res = bi_add(res,dblbits[d])
                rmdr = diff
                if c=0 then exit end if
            end if
        end for

--or: 
        if res[SIGN]!=SG_ZERO then
            res[SIGN] = signR
        end if
--      res[SIGN] = signR
--      rmdr[SIGN] = signA
        if rmdr[SIGN]!=SG_ZERO then
            rmdr[SIGN] = signA
            -- adjustment for bi_mod():
            -- [DEV horrid way to force floor(a/b)...]
--          if bFloor and rmdr!=BI_ZERO then
            if bFloor then
                res = bi_add(res,{signR,{1}})
            end if
        end if
--      res = bi_normalize(res)
--DEV unnecessary??
--      rmdr = bi_normalize(rmdr)
        -- adjustment for bi_mod(): [DEV horrid way to force floor(a/b)...]
--      if bFloor and rmdr!=BI_ZERO then
--          res = bi_add(res,{signR,{1}})
--      end if
        res = {res,rmdr}
    end if
    return res
end function

global function bi_rmdr(object a, object b)
--
-- as per remainder(a,b), but for/returns bigint
-- a,b can be integer, atom, human-readable string, or bigint
--
    if not bigint(a) then a = bi_new(a) end if
    if not bigint(b) then b = bi_new(b) end if

    if a[SIGN]=SG_NOVALUE
    or b[SIGN]=SG_NOVALUE
    or b[SIGN]=SG_ZERO then
        return NO_VALUE
    end if

--DEV test for speed... (without the above!)
--DEV see also the ideas in bi_mod().
    bigint res
    {?,res} = bi_div3(a,b)
--/*
    bigint res = BI_ZERO
    integer cmp = bi_compare(bi_abs(a), bi_abs(b))
    if cmp>0 then
--DEV erm {?,res} = bi_div3(a,b)
        {res,?} = bi_div3(a,b)
--      {res,?} = bi_div(a,b)
        res = bi_mul(b,res)
        res = bi_sub(a,res)
    elsif cmp<0 then
        res = a
    end if
--*/
    return res
end function

global function bi_remainder(object a, object b)
-- (simple alias)
    return bi_rmdr(a, b)
end function

global function bi_mod(object a, b)
--
-- as per mod(a,b): same as remainder when operands have the same sign, as the 
--  vast majority do, otherwise (different signs) round away from zero whereas
--  remainder rounds towards zero. To illustrate:
--
--          bi_rmdr(-9,-4) = -1, bi_mod(-9,-4) = -1
--          bi_rmdr(-9, 4) = -1, bi_mod(-9, 4) =  3
--          bi_rmdr( 9,-4) =  1, bi_mod( 9,-4) = -3
--          bi_rmdr( 9, 4) =  1, bi_mod( 9, 4) =  1
--
-- a,b can be integer, atom, human-readable string, or bigint
--
    if not bigint(a) then a = bi_new(a) end if
    if not bigint(b) then b = bi_new(b) end if

--DEV SG_NOVALUE/SG_ZERO as above??

--DEV for say bi_mod(n,2), or when b is any power of b <= #8000, then just
--                                  return mod(a[DIGITS][$],b[DIGITS][1])
--Also for any length(bi[DIGITS])==1, it may be very much faster to run a
-- simple mod() down length(a[DIGITS]). Oh, and b==1 => 0.

--??(untested)
--  if bi_sign(a)=bi_sign(b) then
    if a[SIGN]=b[SIGN] then
        return bi_rmdr(a,b)
    end if
--  return ba_sub(a,ba_multiply(b,ba_floor(ba_divide(a,b))))
    bigint res
--DEV erm {?,res} = bi_div3(a,b)
    {res,?} = bi_div3(a,b,true)
--  {res,?} = bi_div(a,b)
    res = bi_mul(b,res)
    res = bi_sub(a,res)
--  return x - y * floor(x / y)
    return res
end function

--/*
--Hmm, see bi_mod_exp (for ideas)... [DONE, should really time these...]
function ipower(bigint a, integer exponent)
-- raise a bigint to an integer power

--  if exponent=0 then return BI_ONE end if --not as used (yet...)

    sequence res = a
    for i=2 to exponent do
        res = bi_mul(res,a)
    end for

    return res
end function
--*/

function ipower(bigint a, integer exponent)
-- internal routine: raise a bigint to an integer power
    bigint res = BI_ONE
    while exponent!=0 do
        if mod(exponent,2)=1 then -- odd
            res = bi_mul(res,a)
        end if
        a = bi_mul(a,a)
        exponent = floor(exponent/2)
    end while
    return res
end function

--global function bi_power(object n, integer exponent)
--global function bi_power(object n, exponent)
global function bi_power(object n, exponent,bool track=false)
--
-- Returns n raised to the power of exponent.
-- n can be integer, atom, human-readable string, or bigint
--
-- aside:
-- obviously, exponent other than integer is pointless, since anything 
-- raised to a billion or more will require more than a billion digits,
-- exceeding available ram, at least for any n>=2.
--
sequence res

--  if exponent<0 then
    if bi_compare(exponent,0)<0 then
        return NO_VALUE
--  elsif exponent=0 then
    elsif bi_compare(exponent,0)=0 then
        return BI_ONE
    end if

    if not bigint(n) then n = bi_new(n) end if

    if n[SIGN]=SG_ZERO then
        res = BI_ZERO
    elsif equal({1}, n[DIGITS]) then
        res = n
--      if mod(exponent,2)=0 then
        if bi_mod(exponent,2)=0 then
            res[SIGN] = SG_PLUS
        end if
    else
--DEV it is probably [now] faster just to ipower the lot(inline)?
if 0 then
        sequence factrs = prime_factors(exponent,true)
        res = n
        for i=1 to length(factrs) do
            res = ipower(res, factrs[i])
        end for
else
--  function ipower(bigint a, integer exponent)
-- internal routine: raise a bigint to an integer power
--      bigint 
        res = BI_ONE
--      while bi_compare(exponent,0)!=0 do
        while true do
            if bi_mod(exponent,2)=BI_ONE then -- odd
                res = bi_mul(res,n)
            end if
--DEV gotta be a better way!!
            {exponent} = bi_div3(exponent,2)
            if bi_compare(exponent,0)=0 then exit end if
if track then ?exponent end if
            n = bi_mul(n,n)
        end while
--  return res
--end function
end if

--ie (untested/timed):
--      res = ipower(n,exponent)

--or (inlined, untested):
--      res = BI_ONE
--      while exponent!=0 do
--          if mod(exponent,2)=1 then -- odd
--              res = bi_mul(res,n)
--          end if
--          n = bi_mul(n,n)
--          exponent = floor(exponent/2)
--      end while
    end if
    return res
end function

global function bi_shl(object a, integer n)
-- returns a << n, in C parlance
    for i=1 to n do
        a = bi_add(a,a)
    end for
    return a
end function

global function bi_gcd(object a, b)
--
-- calculate the greatest common divisor of a and b
-- a,b can be integer, atom, human-readable string, or bigint
--
    a = bi_abs(a)
    b = bi_abs(b)
    while bi_compare(b,BI_ZERO)!=0 do
        {a,b} = {b,bi_rmdr(a,b)}
    end while
    return a
end function

global function bi_lcm(object m, n)
--
-- calculate the lowest common multiple of a and b
-- a,b can be integer, atom, human-readable string, or bigint
--
    {m,?} = bi_div3(m,bi_gcd(m,n))
    return bi_mul(m,n)
end function

global function bi_mod_exp(object b, exponent, modulus)
--
--DEV return to this once we have a better bi_mod...
-- returns bi_mod(bi_power(b,exponent),modulus), aka 
--  (b^exponent)%modulus, but //much// faster, eg
--  b=123456789, exponent=2000, modulus=12 takes this
--  ~0s but bi_mod(bi_power(..)) about 48s, and if you 
--  increase the exponent to 4000 the longhand method 
--  will simply run out of memory, whereas this still 
--  finishes near-instantly. Note the longhand method
--  fares a little bit better on bigatom that bigint, 
--  presumably because it limits the precision.
--
-- arguments may be integer, atom, human-readable string, or bigint
--
    if bi_equal(modulus,1) then return BI_ZERO end if
    bigint res = BI_ONE
    b = bi_mod(b,modulus)
    while bi_compare(exponent,0)!=0 do
        if bi_mod(exponent,2)=BI_ONE then -- odd
            res = bi_mod(bi_mul(res,b),modulus)
        end if
        b = bi_mod(bi_mul(b,b),modulus)
        {exponent,?} = bi_div3(exponent,2)
    end while
    return res
end function

global function bi_sqrt(object n)
--
-- returns {root,rmdr} such that n=root*root+rmdr, where root is the
--                          largest integer with rmdr still >= zero.
--
-- n may be integer, atom, human-readable string, or bigint.
--
    if not bigint(n) then n = bi_new(n) end if
    if n[SIGN]=SG_ZERO then
        return {BI_ZERO,BI_ZERO}
    elsif bi_compare(n,BI_ONE)=0 then
        return {BI_ONE,BI_ZERO}
    elsif n[SIGN]<0 then
        return NO_VALUE -- imaginary root (ie sqrt(-x)*i)
    end if

    --
    -- Calculate the highest possible bit, then just try each bit in
    -- turn to see what fits, saving the remainder whenever it does.
    -- It would not surprise me in the slightest to be told there is
    -- a much faster way, but at least there is very little division.
    -- I was quite staggered that my first blind stab at bitn worked
    -- flawlessly first time; it might be somewhat improve-able.
    -- See also bi_div3, which (afterwards) was based on this method.
    --
    integer bit = length(n[DIGITS])*8 -- (each digit actually holds 16)
    bigint bitn = {SG_PLUS,power(2,mod(bit,16))&repeat(0,floor(bit/16))},
           res = BI_ZERO,
           rmdr = n
    while bit>=0 do
        bigint trial = bi_add(res,bitn),
               trialsq = bi_mul(trial,trial),
               diff = bi_sub(n,trialsq)
        integer c = bi_compare(diff,BI_ZERO)
        if c>=0 then
            res = trial
            rmdr = diff
            if c=0 then exit end if
        end if
        -- fast divide by 2:
        if bitn[DIGITS][1]=1 then
            if bit=0 then exit end if
            bitn[DIGITS][1..2] = {#8000}
        else
            bitn[DIGITS][1] /= 2
        end if
        bit -= 1
    end while
    return {res,rmdr}
end function

integer finit = 0
sequence fcache

global function bi_factorial(integer n)
--
-- Standard iterative factorial function, with memoisation.
-- eg            n : 0 1 2 3 4  5   6   7    8
--  ba_factorial(n): 1 1 2 6 24 120 720 5040 40320 
--
-- Note that n must be integer (it indexes fcache, for one thing).
--
atom res = 1
    if n>0 then
        if not finit then
            fcache = {BI_ONE}
            finit = 1
        end if
        for i=length(fcache)+1 to n do
            fcache = append(fcache,bi_mul(fcache[$],i))
        end for
        res = fcache[n]
    end if 
    return res
end function


--/* needs bi_sqrt... (floor thing, probably returning {root,rmdr})
function bi_factor(object n)
-- eg ba_factor(1000) -> {{2,3},{5,3}}, ie power(2,3)*power(5,3) == 8*125 == 1000.
--  (note that each res[i] is {bigint|integer,integer})
    if bi_compare(n,BI_ZERO)=0 then return {} end if
    sequence pf = {}
    integer e = 0
    while bi_mod(n,2)=BI_ZERO do
        {n,?} = bi_div3(n,2)
        e += 1
    end while
    if e>0 then
        pf = {{2,e}}
    end if
    bigint s = bi_sqrt(n),
           d = bi_new(3)
    while bi_compare(n,BI_ONE)>0 do
        if bi_compare(d,s)>0 then
            d = bi_new(n)
        end if
        e = 0
        while true do
--          bigint r = bi_mod(n,d)
            bigint {q,r} = bi_div3(n,d)
            if r!=BI_ZERO then exit end if
--          {n,?} = ba_div(n,d)
            n = q
            e += 1
        end while
        if e>0 then
            pf = append(pf,{d,e})
            {s,?} = bi_sqrt(n)
        end if
        d = bi_add(d,2)
    end while
    return pf
end function

global function bi_factors(object n, bool include1=false)
-- returns a list of all integer factors of n
--  if include1 is 0 (the default), result does not contain either 1 or n
--  if include1 is 1, and n>1, the result contains 1 and n
--  if include1 is -1, and n>1, the result contains 1 but not n
sequence lfactors = {}, hfactors = {}
bigint hfactor,
       {lim,?} = ba_sqrt(n)
integer p = 2

--  if n<1 or n!=floor(n) then ?9/0 end if  --DEV crash("first argument to factors() must be a positive integer",{},2)
--  n = ba_floor(n)
    if bi_compare(n,1)<0 then ?9/0 end if
--  if ba_compare(n,ba_floor(n))!=0 then ?9/0 end if
    
    if bi_compare(n,1)!=0 and include1 then
        lfactors = {1}
        if include1=1 then
            hfactors = {n}
        end if
    end if
--  while p<=lim do
    while bi_compare(p,lim)<=0 do
        bigint{q,r} = bi_div3(n,p)
        if r=BA_ZERO then
            lfactors = append(lfactors,p)
            hfactor = q --ba_idivide(n,p)
            if bi_compare(hfactor,p)=0 then exit end if
            hfactors = prepend(hfactors,hfactor)
        end if
        p += 1
    end while 
    return lfactors & hfactors
end function
--*/

--/*
--bi_sign
--bi_add
--bi_sub
--bi_mul
--bi_div (as ba_idiv...)
--bi_rmdr
--bi_mod
--bi_round??trunc??ceil??floor??
--bi_abs
--bi_neg
--bi_sqrt??     -- erm, yes...
--bi_root??     -- (or...)
--bi_power
--bi_mod_exp
--bi_gcd
--bi_lcm
--bi_rand
--bi_prime_factors??
--*/

-- DEV try/time me: we could just print in hex and convert_base...
-- jeez, over 5 times slower... (actually faster on small ints though...)
global function bi_sprint_viahex(object n, base b=10)
    if not bigint(n) then n = bi_new(n) end if
    {t_sign sgn, n} = n
    string res = sprintf("%x",n[1])
--  for i=length(n)-1 to 1 by -1 do
    for i=2 to length(n)do
        res &= sprintf("%04x",n[i])
    end for
    if b!=16 then
        res = convert_base(res,16,b)
    end if
    if sgn=SG_MINUS then
        res = "-"&res
    end if
    return res
end function

--DEV: AND: bool e_fmt: (replace (>3?) trailing zeroes with eNNN)
global function bi_sprint(object n, base b=10)--, ??bool zero_fill/string fmt??
--? if b=16 then return bi_sprint_viahex(n,b) end if
    if not bigint(n) then n = bi_new(n) end if
    string res = "" 
    {t_sign sgn, n} = n
    while length(n) do
        integer q = 0, r = 0, digit
--      for i=length(n) to 1 by -1 do
        for i=1 to length(n) do
            digit = n[i]
--          q = q*65536+digit
            q = q*WORD+digit
--if 0 then
--          r = mod(q,10000)
--          q = floor(q/10000)
--          n[i] = q
--          q = r
----tryme: (seems fine)
--else
            {q,n[i]} = {mod(q,10000),floor(q/10000)}
--end if
        end for
--      res = sprintf("%04d%s",{r,res})
        res = sprintf("%04d%s",{q,res})
--?     n = bi_normalise(n)
--      n = strip_lz(n)     -- NO! (we have to go all the way here)
                            -- (or while length(n) and n!={0} do??)
        while length(n) and n[$]=0 do
            n = n[1..$-1]
        end while
    end while
    if res="" then
        res = "0"
    else
--      while length(res)>1 and res[1]='0' do
--          res = res[2..$]
--      end while
        res = strip_zeros(res)
        if b!=10 then
            res = convert_base(res,10,b)
        end if
    end if
    if sgn=SG_MINUS then
        res = "-"&res
    end if
    return res
end function

function add_commas(string s, integer comma)
-- internal helper routine for bi_sprint, eg "1234" -> "1,234".
    if not find(comma,",_") then ?9/0 end if
    integer edx = length(s)
    if edx>3 then
        -- As per substitute(), move each digit at most twice:
        sequence chunks = {}
        while edx>3 do
            chunks = prepend(chunks,comma&s[edx-2..edx])
            edx -= 3
        end while
        chunks = prepend(chunks,s[1..edx])
        s = join(chunks,"")
    end if
    return s
end function
--  (there is probably a faster way to do this, perhaps like substitute().)
--  integer commapos = length(s)+1
--  while commapos>4 do
--      commapos -= 3
--      s = s[1..commapos-1]&comma&s[commapos..$]
--  end while

sequence squares = {}   -- eg squares[10] is 10, 100, 10000, 100000000, etc.

function pt10(bigint n, base b, integer qdx)
-- internal routine: the resursive inner loop of bi_sprint10()
    string res = ""
    if qdx=1 then
        if length(n[DIGITS])!=1 then ?9/0 end if
        integer digit = n[DIGITS][1]
        if digit<0 or digit>36 then ?9/0 end if
        digit += iff(digit<='9'?'0':'A'+1)
        res &= digit
    else
        qdx -= 1
        bigint {q,r} = bi_div3(n,squares[b][qdx])
        res = pt10(q,b,qdx) & pt10(r,b,qdx)
    end if      
    return res
end function    

global constant PT_SCIENTIFIC = #00001, -- eg 10000 -> 1e4
                PT_SHOWCOMMAS = #00002, -- eg 10000 -> 10,000
                PT_UNDERSCORE = #2+#04  -- eg 10000 -> 10_000
-- note that for PT_SCIENTIFIC+PT_SHOWCOMMAS, 10000 -> 10e3,
--      ie the "e" part is always capped to a multiple of 3.
-- also note that PT_SCIENTIFIC only applies to base 10, and 
-- PT_SHOWCOMMAS and PT_UNDERSCORE only to bases 10 and 16.
-- obviously, PT_UNDERSCORE is "PT_SHOWCOMMAS but use a '_'".

global function bi_sprint10(object n, base b=10, integer options=0)
    if not bigint(n) then n = bi_new(n) end if
    string res = "" 
    integer sgn = n[SIGN],
            comma = iff(and_bits(options,PT_UNDERSCORE)=PT_UNDERSCORE?'_':',')

    if b=2 or b=16 then
        --
        -- We can print binary and hexadecimal pretty fast. In truth the original
        -- idea was to create a hex string for any base, and then convert_base();
        -- when that proved much slower than predicted, and I developed the much
        -- better strategy below, I still saw no reason to ditch this.
        --
        n = n[DIGITS]
        if b=2 then
            res = sprintf("%b",n[1])
            for i=2 to length(n)do
                res &= sprintf("%016b",n[i])
            end for
        else
            res = sprintf("%x",n[1])
            for i=2 to length(n)do
                if and_bits(options,PT_SHOWCOMMAS) then
                    -- (nb this post-dates the convert_base call below)
                    res &= comma
                end if
                res &= sprintf("%04x",n[i])
            end for
--          if b!=16 then
--              res = convert_base(res,16,b)
--          end if
        end if
    else
        --
        -- The traditional method is /10 to peel off least significant digits.
        -- However, I think we can improve on that:
        --
        -- Simple divide-and-conquer strategy: suppose we wish to print 12345678, 
        -- then / 10,000 gives quotient 1234, remainder 5678 ==> print & stitch.
        -- Obviously, you (recursively) print 1234 by dividing by 100, and print
        -- 12 by diving by 10 and in that way get down to each individual digit.
        -- And obviously it must zero-fill everything, and only strip at the end.
        --
        -- The idea is that after the split, the top half does not need any more
        -- divides to extract the digits of the bottom half, hence less divides
        -- overall, at least that is when length(n[DIGITS]) is greater than one.
        --
        -- First thing to do, obviously, is find the power of 10 to split it on,
        -- but when recursing you already know what that will be at each stage.
        -- The only point to note is that squares[10] does *not* contain 1000, 
        -- since you cannot d&c that, at least not as easily as 100 and 10,000.
        --
--LIES, ALL LIES... (it was better on one test, but 10x slower on 1e500)
        -- Turns out perhaps 25 times faster than a divmod(10) all the way down 
        -- the n[DIGITS] array to obtain each and every individual decimal digit.
        -- I suppose that for ultimate performance you might want to replace the
        -- recursive method with a simpler iterative todo-stack-based solution.
        --
        n[SIGN] = SG_PLUS
        integer qdx = b-length(squares)
        if qdx>0 then squares &= repeat({},qdx) end if
        qdx = 1
        while true do
            if qdx>length(squares[b]) then
                bigint square = iff(qdx=1?bi_new(b):bi_power(squares[b][qdx-1],2))
                squares[b] = append(squares[b],square)
            end if
            if bi_compare(squares[b][qdx],n)>0 then exit end if
            qdx += 1
        end while
        res = pt10(n,b,qdx)
        if qdx>1 then res = strip_zeros(res) end if
        if and_bits(options,PT_SCIENTIFIC) then
            -- convert eg "100000" to "1e5"
            integer tz = 0
            for i=length(res) to 1 by -1 do
                if res[i]!='0' then
                    if tz>3 then
                        if and_bits(options,PT_SHOWCOMMAS) then
                            -- convert eg "12340000" to "12,340e3"
                            --  (the mod3 is partly to simplify matters, 
                            --   and partly that "12,34e4" looks wrong)
                            integer m3 = mod(tz,3)
                            res = add_commas(res[1..i+m3],comma)
                            tz -= m3 -- (tz should now be a multiple of 3)
                        else
                            -- convert eg "12340000" to "1234e4"
                            res = res[1..i]
                        end if
                        res &= sprintf("e%d",tz)
                    end if
                    exit
                end if
                tz += 1
            end for
        elsif and_bits(options,PT_SHOWCOMMAS) then
            -- convert eg "12340000" to "12,340,000"
            res = add_commas(res,comma)
        end if
    end if
    if sgn=SG_MINUS then
        res = "-"&res
    end if
    return res
end function

global function bi_rand(object low, high)
-- generate a random integer between low and high (inclusive)
-- low and high can be passed in as integer/string/bigint
--  low = bi_sub(low,1)
    bigint hz = bi_sub(high,low)    -- convert range to 0..hz
    string hs = bi_sprint(hz)       -- get length
    integer l = length(hs)
    string rs = repeat('9',l)
    while 1 do
        -- generate "000..." .. "999..." in blocks of up to 9
        for p=1 to length(rs) by 9 do
            integer cl = min(l-p+1,9)
            string fmt = sprintf("%%0%dd",cl) -- "%01d".."%09d"
            string chunk = sprintf(fmt,rand(power(10,cl))-1)
            rs[p..p+cl-1] = chunk
        end for
        if length(rs)!=length(hs) then ?9/0 end if -- sanity
        if rs<=hs then exit end if
    end while
--?{"rs",rs,hs}
    return bi_add(bi_new(rs),low)
end function

global function bi_bitlength(object n)
    if not bigint(n) then n = bi_new(n) end if
    integer nb = 0,
            w1 = n[DIGITS][1]
    while w1>0 do
        nb += 1
        w1 = floor(w1/2)
    end while
    nb += (length(n[DIGITS])-1)*(machine_bits()/2)
    return nb
end function

--/*
--some tests nicked from Go: (all passed/DEV move to p -test(tNNbigint.e?))
constant bitLenTests = {{"-1", 1},
                        {"0", 0},
                        {"1", 1},
                        {"2", 2},
                        {"4", 3},
                        {"0xabc", 12},
                        {"0x8000", 16},
                        {"0x80000000", 32},
                        {"0x800000000000", 48},
                        {"0x8000000000000000", 64},
                        {"0x80000000000000000000", 80},
                        {"-0x4000000000000000000000", 87}}

for i=1 to length(bitLenTests) do
    {string s, integer e} = bitLenTests[i]
    bigint x = bi_new(s)
    integer nb = bi_bitlength(x)
    if nb!=e then
        crash("bigint line 1912: #%d (%s) got %d want %d",{i,s,nb,e})
    end if
end for
--*/

global function bi_int(object n)
--
-- Returns n as an integer within the standard range, ie -1,073,741,824 to +1,073,741,823 
-- on 32 bit, and -4,611,686,018,427,387,904 to +4,611,686,018,427,387,903 on 64 bit.
-- Throws an exception if n (integer/atom/string/bigint) cannot be held in an integer.
--
    integer res
    if integer(n) then
        res = n
    else
        if not bigint(n) then n = bi_new(n) end if
        if bi_bitlength(n)>=machine_bits() then
            throw("bi_int() argument out of range")
        end if
        res = n[DIGITS][1]
        for i=2 to length(n[DIGITS]) do     -- (max one iteration, one would expect)
            res = res*WORD+n[DIGITS][i]
        end for
    end if
    return res
end function

--DEV use true/false:
constant YES = 1,
         NO = 0

--power(2,53/64):
constant mcINTMAX = 9007199254740992,   -- largest value Phix atom can represent exactly (32 bit)
         mcDEFAULTBASE = 10,            -- sets the number base on start-up
         mcADDITION = 1,                -- determines which type of carry operation
         mcSUBTRACTION = -1,            -- to correct for in fixcarries()

         mcLEFTBIG = 1  -- "   "    "
--       mcRIGHTBIG = -1 -- "   "   "

--power(2,26/32)?:
global constant gcMAXBASE = floor(sqrt(mcINTMAX/2)) -- max safe number base

integer  mBASE    -- the current working number base, used by these routines
atom  mSAFEMAX    -- the maximum value allowable at any stage of fundamental operations

global integer gCURBASE -- always equals mBASE but is not used by any
                        -- routines in this file; maintained to allow
                        -- the user to check the current working base
                        -- without endangering the integrity of the
                        -- (internally important) variable mBASE

mBASE = mcDEFAULTBASE
gCURBASE = mBASE
mSAFEMAX = mcINTMAX-mBASE*mBASE

sequence mTWO  -- handy in several places to have a
               -- bignum version of 2 predefined
if mcDEFAULTBASE>2 then
    mTWO = {2}
else
    mTWO = {1,0}
end if

------------------------ DEFINED TYPES
global type bignum(object n)
    return sequence(n)
end type

global type numbase(object n)
    return integer(n) and n>1 and n<=gcMAXBASE
end type

global function bn_set_base(numbase newbase)
-- sets the working number base for all operations; restricts the base to
-- the range 2 through gcMAXBASE with gcMAXBASE defined as the maximum base
-- that can be used without causing fundamental operations to produce
-- numbers greater than the maximum exact value Euphoria can handle.
-- maintains the module variables mBASE, mSAFEMAX and the global
-- gCURBASE, along with the convenience variable mTWO.
-- RETURNS: the previous number base
integer oldbase

    if newbase>gcMAXBASE or newbase<2 then  -- leave base as it is
        oldbase = mBASE
        gCURBASE = mBASE
    else
        oldbase = mBASE
        mBASE = newbase
        gCURBASE = mBASE
        mSAFEMAX = mcINTMAX-mBASE*mBASE
--      set_mTWO()
        if mBASE>2 then
            mTWO = {2}
        else    -- is binary, only legal base in which 2 is not '2'
            mTWO = {1,0}
        end if
    end if

    return oldbase
end function

--DEV might fare better stripping leading zeroes?
function even_len(bignum s1, bignum s2)
-- makes two numbers equal in digit length by padding the left side of one
-- with zeroes
-- RETURNS: a two element sequence whose elements are the input numbers
--          in the same order they were passed to the function
integer diff

    diff = length(s1)-length(s2)
    if diff>0 then
        s2 = repeat(0,diff) & s2
    else
        s1 = repeat(0, diff*(-1)) & s1
    end if

    return {s1,s2}
end function


global function trimzero(bignum n)
-- removes leading zeros from number
-- RETURNS: the number after zeros have been stripped
integer len, start
    len = length(n)
    if len=0 then
        return 0
    end if

    start = len

    for x=1 to len by 1 do
        if n[x]!=0 then
            start = x
            exit
        end if
    end for

    return n[start..len]
end function

global function bn_is_zero(bignum n)
-- determines whether the input number is zero
-- RETURNS: 1 if the number is zero, 0 if not
integer len

    len = length(n)
    if n[len]=0 then
        for x=(len-1) to 1 by -1 do
            if n[x]!=0 then
                return NO
            end if
        end for
    else
        return NO
    end if


    return YES
end function

--/*
function bn_is_one(bignum n)
-- determines whether the input number is one
-- RETURNS: 1 if the number is one, 0 if not
integer len

    len = length(n)
    if n[len]=1 then
        for x=(len-1) to 1 by -1 do
            if n[x]!=0 then
                return NO
            end if
        end for
    else
        return NO
    end if

    return YES
end function

function bn_is_even(bignum n)
-- determines if the input number is even
-- RETURNS: 1 if the number is even, 0 if not
integer retval, len

    len = length(n)
    if len<1 then
        retval = YES
    else
        if and_bits(n[len],1)=0 then   -- if number is not odd
            retval = YES
        else
            retval = NO
        end if
    end if

    return retval
end function
--*/

global function bn_compare(bignum n1,bignum n2)
-- compares two numbers
-- RETURNS: -1 if n1 < n2
--           0 if n1 = n2
--           1 if n1 > n2
bignum bucket, even1, even2
integer compval

    bucket = even_len(n1,n2)
    even1 = bucket[1]
    even2 = bucket[2]
    compval = compare(even1,even2)

    return compval
end function

function fixcarries(bignum n, atom fixtype)
-- adjusts the numbers passed in so that no 'digit' is greater
-- than the maximum allowable digit value for the current number base,
-- or less than zero (as in mcSUBTRACTION)
-- RETURNS: the adjusted number
bignum result
integer redo, len
atom curdigit, numcarries, numbases
    result = n
    redo = 1
    if fixtype=mcADDITION then
        while redo=YES do
            result = 0 & result
            len = length(result)
            redo = NO
            for x=len to 2 by -1 do
                curdigit = result[x]
                if curdigit>(mBASE-1) then -- carry the excess over
                    numcarries = floor(curdigit/mBASE)
                    if numcarries>mSAFEMAX then -- keep the carryover from
                        numcarries = mSAFEMAX     -- exceeding the max atom
                        redo = YES                -- value
                    end if
                    result[x] = curdigit-mBASE*(numcarries)
                    result[x-1] = result[x-1]+numcarries
                end if
            end for
        end while

        redo = NO
        while result[1]>(mBASE-1) or redo=1 do -- fix most significant
            result = 0 & result                         -- digit
            numcarries = floor(result[2]/mBASE)
            if numcarries>mSAFEMAX then
                numcarries = mSAFEMAX
                redo = YES
            end if
            result[2] = result[2]-mBASE*(numcarries)
            result[1] = result[1]+numcarries
        end while

    else -- fix for mcSUBTRACTION, essentially adjusting for the borrows
     -- that would have been made if subtracting longhand
        while redo=YES do
            result = 0 & result
            len = length(result)
            redo = NO
            for x=len to 2 by -1 do
                curdigit = result[x]
                if curdigit<0 then  -- bring in the next digit's excess
                    numbases = curdigit/(-mBASE)
                    if numbases!=1 then
                        numcarries = floor(numbases)+1
                    else
                        numcarries = 1
                    end if
                    if numcarries>mSAFEMAX then -- keep the carryover from
                        numcarries = mSAFEMAX     -- exceeding the max atom
                        redo = YES                -- value
                    end if
                    result[x] = curdigit+mBASE*(numcarries)
                    result[x-1] = result[x-1]-numcarries
                end if
            end for
        end while
    end if

    return trimzero(result)
end function


global function bn_add(bignum n1, bignum n2)
-- add two numbers
-- RETURNS: the sum of the numbers
bignum res, bucket

    bucket = even_len(n1,n2)
--  res = bucket[1]+bucket[2]
    res = sq_add(bucket[1],bucket[2])
    res = fixcarries(res, mcADDITION)

    return res
end function

global function bn_diff(bignum n1, bignum n2)
-- calculate the POSITIVE difference between two numbers
-- RETURNS: the difference
bignum diff, bucket

    if bn_compare(n1,n2)=mcLEFTBIG then
        bucket = even_len(n1,n2)
--      diff = bucket[1]-bucket[2]
        diff = sq_sub(bucket[1],bucket[2])
    else
        bucket = even_len(n1,n2)
--      diff = bucket[2]-bucket[1]
        diff = sq_sub(bucket[2],bucket[1])
    end if

    diff = fixcarries(diff, mcSUBTRACTION)

    return diff
end function

global function bn_dec(bignum n)
-- subtract one from the number
-- RETURNS: the decremented number
integer len

    len = length(n)
    n[len] = n[len]-1
    if n[len]<0 then
        n = fixcarries(n, mcSUBTRACTION)
    end if
    return n
end function

global function bn_mult(bignum n1, bignum n2)
-- multiplies two numbers by brute force method
-- RETURNS: the product of the numbers
bignum product, curLine
integer len

    len = length(n1)
    product = {0}
    for x=len to 1 by -1 do
--      curLine = n2*n1[x] & repeat(0,len-x)
        curLine = sq_mul(n2,n1[x]) & repeat(0,len-x)
        product = bn_add(product,curLine)
        if x=1 then
            product = fixcarries(product, mcADDITION)
        end if
    end for

    return product
end function

global function bn_curto_atom(bignum innum)
-- convert a number in bignum form to an atom, base 10, using Horner's
-- method for evaluating polynomials
-- Note: does not check to see if the number is actually within
-- the limits of exact representation
-- RETURNS: base 10 representation of the number
integer fromLen
atom outnum

    fromLen = length(innum)
    outnum = innum[1]
    for curdigit=2 to fromLen by 1 do
        outnum = outnum*mBASE+innum[curdigit]
    end for

    return outnum
end function

global function bn_atomto_other(atom innum, numbase tobase)
-- convert an atom (base 10) number to another base and represent it
-- in long integer form (bignum type)
-- RETURNS: the number in long integer form as a 'tobase' number
bignum outnum
integer redo
atom numcarries

    if tobase>gcMAXBASE then
        outnum = {-1}
    elsif innum<tobase then
        outnum = {innum}
    else
        outnum = {innum}
        redo = YES
        while redo=YES do
            redo = NO
            outnum = 0 & outnum
            for x=length(outnum) to 2 by -1 do
                numcarries = floor(outnum[x]/tobase)
                outnum[x] = outnum[x]-tobase*numcarries
                outnum[x-1] = outnum[x-1]+numcarries
            end for
            if outnum[1]>=tobase then
                redo = YES
            end if
        end while
    end if

    return outnum
end function

global function bn_baseconv(bignum innum, numbase frombase, numbase tobase)
-- convert a number from current base to the specified new number base;
-- uses Horner's method for evaluating polynomials, which seems to be
-- significantly faster than base conversion by iterative dividing
-- RETURNS: the original number in the new number base
bignum outnum, frombase_newform
integer fromLen
atom oldbase

    oldbase = bn_set_base(tobase)
    fromLen = length(innum)
    -- from here on, all work is done in the target number base, treating the
    -- original number as a polynomial evaluated at x = frombase.
    -- For instance: to convert 4560000000 to base 8 (octal):
    -- 4560000000 in base 10(bignum) = {4,5,6,0,0,0,0,0,0,0}
    -- represent as polynomial:{4}*{1,0}^{1,0} + {5}*{1,0}^{9} + {6}*{1,0}^{8}
    -- convert parts to octal:{4}*{1,2}^{1,2} + {5}*{1,2}^{1,1} + {6}*{1,2}^{1,0}
    -- evaluate the polynomial (in octal) and you have the octal result:
    -- {4,1,7,6,3,0,1,2,0,0,0}
    frombase_newform = bn_atomto_other(frombase,tobase)

    outnum = bn_atomto_other(innum[1], tobase)
    for curdigit=2 to fromLen by 1 do
        outnum = bn_mult(outnum, frombase_newform)
        outnum = bn_add(outnum, bn_atomto_other(innum[curdigit], tobase))
    end for

    oldbase = bn_set_base(oldbase)
    return outnum
end function

--/*
--Also, make this suitable for 32 and 64 bit...
Supposing you have a fast 32-bit multiplication and division the result can be computed 4 digits at a time 
by implementing a bigint division/modulo 10000 and then using (s)printf for output of digit groups.

This approach is also trivial to extend to higher (or even variable) precision...

#include <stdio.h>

typedef unsigned long bigint[4];

function print_bigint(bigint src)
--  unsigned long int x[8];   -- expanded version (16 bit per element)
--  int result[12];           -- 4 digits per element
sequence result = {}, x = {}
bool done = false             -- did we finish?
--  int i = 0;                -- digit group counter

--  /* expand to 16-bit per element */
--  x[0] = src[0] & 65535;
--  x[1] = src[0] >> 16;
--  x[2] = src[1] & 65535;
--  x[3] = src[1] >> 16;
--  x[4] = src[2] & 65535;
--  x[5] = src[2] >> 16;
--  x[6] = src[3] & 65535;
--  x[7] = src[3] >> 16;
    for i=1 to length(src) do
--      x &= {and_bits(src[i],#FFFF),floor(src[i]/#10000)}
        x &= {and_bits(src[i],WORD-1),floor(src[i]/WORD)}
    end for

    while not done do
        done = true
        unsigned long carry = 0;
        for j=length(x) to 1 by -1 do
            unsigned long d = (carry << 16) + x[j]
::WORD??::
            x[j] = d / 10000;
            carry = d - x[j] * 10000;
            if (x[j]) done = false end if
        end for
        i += 1
        result &= carry
    end while

--DEV should printf support %i??? [google it]
    string res = sprintf("%d", result[i])
    while i>1 do
        i -= 1
        res &= sprintf("%04d", result[i])
    end while
    return res
end function

int main(int argc, const char *argv[])
{
    bigint tests[] = { { 0, 0, 0, 0 },
                       { 0xFFFFFFFFUL, 0, 0, 0 },
                       { 0, 1, 0, 0 },
                       { 0x12345678UL, 0x90abcdefUL, 0xfedcba90UL, 0x8765421UL } };
    {
        int i;
        for (i=0; i<4; i++)
        {
            print_bigint(tests[i]);
            printf("\n");
        }
    }
    return 0;
}

--*/

--maybe::
--/*
A slow but simple approach is to just printing digits from most significant to least significant using subtraction. 
Basically you need a function for checking if x >= y and another for computing x -= y when that is the case. 
Then you can start counting how many times you can subtract 10^38 (and this will be most significant digit), 
then how many times you can subtract 10^37 ... down to how many times you can subtract 1.

The following is a full implementation of this approach:

#include <stdio.h>

typedef unsigned ui128[4];

int ge128(ui128 a, ui128 b)
{
    int i = 3;
    while (i >= 0 && a[i] == b[i])
        --i;
    return i < 0 ? 1 : a[i] >= b[i];
}

void sub128(ui128 a, ui128 b)
{
    int i = 0;
    int borrow = 0;
    while (i < 4)
    {
        int next_borrow = (borrow && a[i] <= b[i]) || (!borrow && a[i] < b[i]);
        a[i] -= b[i] + borrow;
        borrow = next_borrow;
        i += 1;
    }
}

ui128 deci128[] = {{1u,0u,0u,0u},
                   {10u,0u,0u,0u},
                   {100u,0u,0u,0u},
                   {1000u,0u,0u,0u},
                   {10000u,0u,0u,0u},
                   {100000u,0u,0u,0u},
                   {1000000u,0u,0u,0u},
                   {10000000u,0u,0u,0u},
                   {100000000u,0u,0u,0u},
                   {1000000000u,0u,0u,0u},
                   {1410065408u,2u,0u,0u},
                   {1215752192u,23u,0u,0u},
                   {3567587328u,232u,0u,0u},
                   {1316134912u,2328u,0u,0u},
                   {276447232u,23283u,0u,0u},
                   {2764472320u,232830u,0u,0u},
                   {1874919424u,2328306u,0u,0u},
                   {1569325056u,23283064u,0u,0u},
                   {2808348672u,232830643u,0u,0u},
                   {2313682944u,2328306436u,0u,0u},
                   {1661992960u,1808227885u,5u,0u},
                   {3735027712u,902409669u,54u,0u},
                   {2990538752u,434162106u,542u,0u},
                   {4135583744u,46653770u,5421u,0u},
                   {2701131776u,466537709u,54210u,0u},
                   {1241513984u,370409800u,542101u,0u},
                   {3825205248u,3704098002u,5421010u,0u},
                   {3892314112u,2681241660u,54210108u,0u},
                   {268435456u,1042612833u,542101086u,0u},
                   {2684354560u,1836193738u,1126043566u,1u},
                   {1073741824u,1182068202u,2670501072u,12u},
                   {2147483648u,3230747430u,935206946u,126u},
                   {0u,2242703233u,762134875u,1262u},
                   {0u,952195850u,3326381459u,12621u},
                   {0u,932023908u,3199043520u,126217u},
                   {0u,730304488u,1925664130u,1262177u},
                   {0u,3008077584u,2076772117u,12621774u},
                   {0u,16004768u,3587851993u,126217744u},
                   {0u,160047680u,1518781562u,1262177448u}};

void print128(ui128 x)
{
    int i = 38;
    int z = 0;
    while (i >= 0)
    {
        int c = 0;
        while (ge128(x, deci128[i]))
        {
            c++; sub128(x, deci128[i]);
        }
        if (i==0 || z || c > 0)
        {
            z = 1; putchar('0' + c);
        }
        --i;
    }
}

int main(int argc, const char *argv[])
{
    ui128 test = { 0x12345678, 0x90abcdef, 0xfedcba90, 0x8765421 };
    print128(test);
    return 0;
}

--*/

--another one:
--/*
#include <string.h>
#include <stdio.h>

typedef unsigned long uint32;

/* N[0] - contains least significant bits, N[3] - most significant */
char* Bin128ToDec(const uint32 N[4])
{
  // log10(x) = log2(x) / log2(10) ~= log2(x) / 3.322
  static char s[128 / 3 + 1 + 1];
  uint32 n[4];
  char* p = s;
  int i;

  memset(s, '0', sizeof(s) - 1);
  s[sizeof(s) - 1] = '\0';

  memcpy(n, N, sizeof(n));

  for (i = 0; i < 128; i++)
  {
    int j, carry;

    carry = (n[3] >= 0x80000000);
    // Shift n[] left, doubling it
    n[3] = ((n[3] << 1) & 0xFFFFFFFF) + (n[2] >= 0x80000000);
    n[2] = ((n[2] << 1) & 0xFFFFFFFF) + (n[1] >= 0x80000000);
    n[1] = ((n[1] << 1) & 0xFFFFFFFF) + (n[0] >= 0x80000000);
    n[0] = ((n[0] << 1) & 0xFFFFFFFF);

    // Add s[] to itself in decimal, doubling it
    for (j = sizeof(s) - 2; j >= 0; j--)
    {
      s[j] += s[j] - '0' + carry;

      carry = (s[j] > '9');

      if (carry)
      {
        s[j] -= 10;
      }
    }
  }

  while ((p[0] == '0') && (p < &s[sizeof(s) - 2]))
  {
    p++;
  }

  return p;
}

int main(void)
{
  static const uint32 testData[][4] =
  {
    { 0, 0, 0, 0 },
    { 1048576, 0, 0, 0 },
    { 0xFFFFFFFF, 0, 0, 0 },
    { 0, 1, 0, 0 },
    { 0x12345678, 0x90abcdef, 0xfedcba90, 0x8765421 }
  };
  printf("%s\n", Bin128ToDec(testData[0]));
  printf("%s\n", Bin128ToDec(testData[1]));
  printf("%s\n", Bin128ToDec(testData[2]));
  printf("%s\n", Bin128ToDec(testData[3]));
  printf("%s\n", Bin128ToDec(testData[4]));
  return 0;
}
Output:

0
1048576
4294967295
4294967296
11248221411398543556294285637029484152
--*/

-- and another:
--/*
64-bit:
#include <stdio.h>

unsigned int a [] = { 0x12345678, 0x12345678, 0x12345678, 0x12345678 };

/* 24197857161011715162171839636988778104 */

int
main ()
{
  unsigned long long d, r;

  do
    {
      r = a [0];

      d = r / 10;
      r = ((r - d * 10) << 32) + a [1];
      a [0] = d;

      d = r / 10;
      r = ((r - d * 10) << 32) + a [2];
      a [1] = d;

      d = r / 10;
      r = ((r - d * 10) << 32) + a [3];
      a [2] = d;

      d = r / 10;
      r = r - d * 10;
      a [3] = d;

      printf ("%d\n", (unsigned int) r);
    }
  while (a[0] || a[1] || a[2] || a[3]);

  return 0;
}

32-bit:
#include <stdio.h>

unsigned short a [] = { 
  0x0876, 0x5421,
  0xfedc, 0xba90,
  0x90ab, 0xcdef,
  0x1234, 0x5678
};

int
main ()
{
  unsigned int d, r;

  do
    {
      r = a [0];

      d = r / 10;
      r = ((r - d * 10) << 16) + a [1];
      a [0] = d;

      d = r / 10;
      r = ((r - d * 10) << 16) + a [2];
      a [1] = d;

      d = r / 10;
      r = ((r - d * 10) << 16) + a [3];
      a [2] = d;

      d = r / 10;
      r = ((r - d * 10) << 16) + a [4];
      a [3] = d;

      d = r / 10;
      r = ((r - d * 10) << 16) + a [5];
      a [4] = d;

      d = r / 10;
      r = ((r - d * 10) << 16) + a [6];
      a [5] = d;

      d = r / 10;
      r = ((r - d * 10) << 16) + a [7];
      a [6] = d;

      d = r / 10;
      r = r - d * 10;
      a [7] = d;

      printf ("%d\n", r);
    }
  while (a[0] || a[1] || a[2] || a[3] || a [4] || a [5] || a[6] || a[7]);


  return 0;
}
--*/

--Yet another:
--/*
Function HexToDecimal(ByVal sHex As String) As String

    Dim dec() As Long
    ReDim dec(0 To 0) As Long

    Dim lCharLoop As Long
    For lCharLoop = 1 To Len(sHex)

        Dim char As String * 1
        char = Mid$(sHex, lCharLoop, 1)

        Dim carry As Long
        carry = Val("&h" & char)

        Dim i As Long
        For i = 0 To UBound(dec)
            Dim lVal As Long
            lVal = dec(i) * 16 + carry
            dec(i) = lVal Mod 10
            carry = lVal \ 10
        Next i

        While (carry > 0)
            ReDim Preserve dec(0 To UBound(dec) + 1) As Long
            dec(UBound(dec)) = carry Mod 10
            carry = carry \ 10
        Wend
    Next

    For lCharLoop = UBound(dec) To LBound(dec) Step -1
        Dim sDecimal As String
        sDecimal = sDecimal & Chr$(48 + dec(lCharLoop))

    Next

    HexToDecimal = sDecimal

End Function

Private Sub TestHexToDecimal()

    Debug.Assert HexToDecimal("F") = "15"
    Debug.Assert HexToDecimal("4") = CStr(Val("&H4"))
    Debug.Assert HexToDecimal("10") = CStr(Val("&H10"))
    Debug.Assert HexToDecimal("20") = CStr(Val("&H20"))
    Debug.Assert HexToDecimal("30") = CStr(Val("&H30"))
    Debug.Assert HexToDecimal("40") = CStr(Val("&H40"))
    Debug.Assert HexToDecimal("44") = CStr(Val("&H44"))
    Debug.Assert HexToDecimal("FF") = "255"
    Debug.Assert HexToDecimal("FFF") = "4095"
    Debug.Assert HexToDecimal("443") = CStr(Val("&H443"))
    Debug.Assert HexToDecimal("443C1") = "279489"
    Debug.Assert HexToDecimal("443C1CE20DFD592FB374D829B894BBE5") = "90699627342249584016268008583970733029"

    Debug.Assert HexToDecimal("EC851A69B8ACD843164E10CFF70CF9E86DC2FEE3CF6F374B43C854E3342A2F1AC3E30" & _
    "C741CC41E679DF6D07CE6FA3A66083EC9B8C8BF3AF05D8BDBB0AA6CB3EF8C5BAA2A5" & _
    "E531BA9E28592F99E0FE4F95169A6C63F635D0197E325C5EC76219B907E4EBDCD401FB1" & _
    "986E4E3CA661FF73E7E2B8FD9988E753B7042B2BBCA76679") = _
    "1660899461379861685353688491843017402046137536931563604625752175601309049219" & _
    "5397632483978280801827700029602706087374780329179786968451649489474169926767" & _
    "4246881622658654267131250470956587908385447044319923040838072975636163137212" & _
    "8878242485755103411040294617585948551591743298921259938445664971761026682621" & _
    "39513"

End Sub

--PL verified/done:
How to convert a "big" Hex number (in string format):

EC851A69B8ACD843164E10CFF70CF9E86DC2FEE3CF6F374B43C854E3342A2F1AC3E30C741CC41E679DF6D07CE6FA3A66083EC9B8C8BF3AF05D8BDBB0AA6CB3EF8C5BAA2A5E531BA9E28592F99E0FE4F95169A6C63F635D0197E325C5EC76219B907E4EBDCD401FB1986E4E3CA661FF73E7E2B8FD9988E753B7042B2BBCA76679

to a decimal number (in string format):

166089946137986168535368849184301740204613753693156360462575217560130904921953976324839782808018277000296027060873747803291797869684516494894741699267674246881622658654267131250470956587908385447044319923040838072975636163137212887824248575510341104029461758594855159174329892125993844566497176102668262139513

without using BigInteger Class (as my application should support machines without .NET Framework 4)?

--*/

--and another
--/*
       var s = "843370923007003347112437570992242323";
        var result = new List<byte>();
        result.Add( 0 );
        foreach ( char c in s )
        {
            int val = (int)( c - '0' );
            for ( int i = 0 ; i < result.Count ; i++ )
            {
                int digit = result[i] * 10 + val;
                result[i] = (byte)( digit & 0x0F );
                val = digit >> 4;
            }
            if ( val != 0 )
                result.Add( (byte)val );
        }

        var hex = "";
        foreach ( byte b in result )
            hex = "0123456789ABCDEF"[ b ] + hex;

--*/

--and another:
--/*
function h2d(s) {

    function add(x, y) {
        var c = 0, r = [];
        var x = x.split('').map(Number);
        var y = y.split('').map(Number);
        while(x.length || y.length) {
            var s = (x.pop() || 0) + (y.pop() || 0) + c;
            r.unshift(s < 10 ? s : s - 10); 
            c = s < 10 ? 0 : 1;
        }
        if(c) r.unshift(c);
        return r.join('');
    }

    var dec = '0';
    s.split('').forEach(function(chr) {
        var n = parseInt(chr, 16);
        for(var t = 8; t; t >>= 1) {
            dec = add(dec, dec);
            if(n & t) dec = add(dec, '1');
        }
    });
    return dec;
}
Test:

t = 'dfae267ab6e87c62b10b476e0d70b06f8378802d21f34e7'
console.log(h2d(t)) 
prints

342789023478234789127089427304981273408912349586345899239

var x = str2bigInt("5061756c205768697465",16,1,1);
var s = bigInt2str(x, 10);
$('#output').text(s);
Correctly converts 0x5061756c205768697465 to the expected result of 379587113978081151906917.
--*/

--and another
--/*
//Usage: string bigIntString = bigIntVar.ToQuickString()
public static String ToQuickString(this BigInteger source)
{
    powersOfTen = new List<BigInteger>();

    powersOfTen.Add(1);

    for (BigInteger i = 10; i < source; i *= i)
    {
        powersOfTen.Add(i);
    }

    return BuildString(source, powersOfTen.Count - 1).ToString().TrimStart('0');
}

private static List<BigInteger> powersOfTen;

private static string BuildString(BigInteger n, int m)
{
    if (m == 0)
        return n.ToString();

    BigInteger remainder;
    BigInteger quotient = BigInteger.DivRem(n, powersOfTen[m], out remainder);

    return BuildString(quotient, m - 1) + BuildString(remainder, m - 1);
}

--*/

--one more:
--/*
.js
/**
 * A function for converting hex <-> dec w/o loss of precision.
 *
 * The problem is that parseInt("0x12345...") isn't precise enough to convert
 * 64-bit integers correctly.
 *
 * Internally, this uses arrays to encode decimal digits starting with the least
 * significant:
 * 8 = [8]
 * 16 = [6, 1]
 * 1024 = [4, 2, 0, 1]
 *
 * Source: http://www.danvk.org/hex2dec.html
 */

// Adds two arrays for the given base (10 or 16), returning the result.
// This turns out to be the only "primitive" operation we need.
function add(x, y, base) {
  var z = [];
  var n = Math.max(x.length, y.length);
  var carry = 0;
  var i = 0;
  while (i < n || carry) {
    var xi = i < x.length ? x[i] : 0;
    var yi = i < y.length ? y[i] : 0;
    var zi = carry + xi + yi;
    z.push(zi % base);
    carry = Math.floor(zi / base);
    i++;
  }
  return z;
}

// Returns a*x, where x is an array of decimal digits and a is an ordinary
// JavaScript number. base is the number base of the array x.
function multiplyByNumber(num, x, base) {
  if (num < 0) return null;
  if (num == 0) return [];

  var result = [];
  var power = x;
  while (true) {
    if (num & 1) {
      result = add(result, power, base);
    }
    num = num >> 1;
    if (num === 0) break;
    power = add(power, power, base);
  }

  return result;
}

function parseToDigitsArray(str, base) {
  var digits = str.split('');
  var ary = [];
  for (var i = digits.length - 1; i >= 0; i--) {
    var n = parseInt(digits[i], base);
    if (isNaN(n)) return null;
    ary.push(n);
  }
  return ary;
}

function convertBase(str, fromBase, toBase) {
  var digits = parseToDigitsArray(str, fromBase);
  if (digits === null) return null;

  var outArray = [];
  var power = [1];
  for (var i = 0; i < digits.length; i++) {
    // invariant: at this point, fromBase^i = power
    if (digits[i]) {
      outArray = add(outArray, multiplyByNumber(digits[i], power, toBase), toBase);
    }
    power = multiplyByNumber(fromBase, power, toBase);
  }

  var out = '';
  for (var i = outArray.length - 1; i >= 0; i--) {
    out += outArray[i].toString(toBase);
  }
  return out;
}

function decToHex(decStr, opts) {
  var hidePrefix = opts && opts.prefix === false;
  var hex = convertBase(decStr, 10, 16);
  return hex ? (hidePrefix ? hex : '0x' + hex) : null;
}

function hexToDec(hexStr) {
  if (hexStr.substring(0, 2) === '0x') hexStr = hexStr.substring(2);
  hexStr = hexStr.toLowerCase();
  return convertBase(hexStr, 16, 10);
}

module.exports = {
  hexToDec: hexToDec,
  decToHex: decToHex
};
.css
* {
    margin: 0;
    padding: 0;
    font-size: 100%;
    vertical-align: baseline;
    outline: none;
    box-sizing: border-box;
    font-family: 'VT323', monospace;
    border-radius: none;
    border-collapse: collapse;
}

input {
  margin: 2px;
  padding: 5px;
  width: 700px;
}

hr {
  margin: 10px 5px;
}
.html
Hex: <input type="text" id="hex_input">
<div id="results1"></div>
<hr>
Dec: <input type="text" id="dec_input">
<div id="results2"></div>
--*/

--/*
The code that kicked this off:
sequence primes = {2,3,5,7}
atom sieved = 10
 
procedure add_block()
integer N = min((sieved-1)*sieved,400000)
sequence sieve = repeat(1,N)    -- sieve[i] is really i+sieved
    for i=2 to length(primes) do -- (evens filtered on output)
        atom p = primes[i], p2 = p*p
        if p2>sieved+N then exit end if
        if p2<sieved+1 then
            p2 += ceil((sieved+1-p2)/p)*p
        end if
        p2 -= sieved
        if and_bits(p2,1)=0 then p2 += p end if
--      if sieve[p2] then           -- dang!
            for k=p2 to N by p*2 do
                sieve[k] = 0
            end for
--      end if
    end for
    for i=1 to N by 2 do
        if sieve[i] then
            primes &= i+sieved
        end if
    end for
    sieved += N
end procedure

--Uses primes and add_block from [[Extensible_prime_generator#Phix]]
--==cheat/bigatom==
--Cheating slightly, using base 10 logs with limited precision, but nice and fast.<br>

atom t0 = time()
constant bool cheat_mode = false
constant integer lim = iff(cheat_mode?1000000   -- log10
                                     :10000)    -- bigatom
while length(primes)<1000000 do add_block() end while
--while length(primes)<lim do add_block() end while
--?primes[lim]
--?primes[10000]    -- 104729
--?primes[100000]   -- 1299709
--?primes[1000000]  -- 15485863

--?elapsed(time()-t0)       -- 0.4s

include bigatom.e

sequence tests = tagset(10,0)
for i=2 to 6 do tests &= power(10,i) end for
bigatom P = BA_ONE
--bigatom p = BA_ZERO
atom p = 0
integer pi = 1
for i=1 to length(tests) do
    integer ti = tests[i]
    for pi=pi to ti do
        if cheat_mode then
            p += log10(primes[pi])
        else
            P = ba_mul(P,primes[pi])
        end if
    end for
    if ti<=10 then
        if cheat_mode then
            printf(1,"Primorial(%d) = %d\n",{ti,power(10,p)})
        else
            printf(1,"Primorial(%d) = %s\n",{ti,ba_sprint(P)})
        end if
    else
        if cheat_mode then
            printf(1,"Primorial(%d) has %d digits\n",{ti,floor(p)+1})
        else
            printf(1,"Primorial(%d) has %d digits (%s)\n",{ti,length(ba_sprint(P)),elapsed(time()-t0)})
        end if
    end if
end for
?elapsed(time()-t0)

puts(1, "Done!\n") 

{} = wait_key()
without warning
abort(0)

--cheat_mode:
"started"
Primorial(0) = 1
Primorial(1) = 2
Primorial(2) = 6
Primorial(3) = 30
Primorial(4) = 210
Primorial(5) = 2310
Primorial(6) = 30030
Primorial(7) = 510510
Primorial(8) = 9699690
Primorial(9) = 223092870
Primorial(10) = 6469693230
Primorial(100) has 220 digits
Primorial(1000) has 3393 digits
Primorial(10000) has 45337 digits
Primorial(100000) has 563921 digits
Primorial(1000000) has 6722809 digits
"0.5s"
</pre>
PS add_block() takes about 0.4 of that 0.5s

bigatom:
"started"
Primorial(0) = 1
Primorial(1) = 2
Primorial(2) = 6
Primorial(3) = 30
Primorial(4) = 210
Primorial(5) = 2310
Primorial(6) = 30030
Primorial(7) = 510510
Primorial(8) = 9699690
Primorial(9) = 223092870
Primorial(10) = 6469693230
Primorial(100) has 220 digits (0.1s)
Primorial(1000) has 3393 digits (1.2s)
Primorial(10000) has 45337 digits (3 minutes and 03s)

(no diff on 64 bit)

Primorial(5) = 2310
Primorial(6) = 30030
Primorial(7) = 510510
Primorial(8) = 9699690
Primorial(9) = 223092870
Primorial(10) has 10 digits
Primorial(100) has 220 digits
Primorial(1000) has 3393 digits
Primorial(10000) has 45337 digits
Primorial(100000) has 563921 digits
Primorial(1000000) has 6722809 digits

--Uses primes and add_block from [[Extensible_prime_generator#Phix]]
--==bigint version==
--{{trans|FreeBASIC}}
--Already knowing that bigatom is not exactly fast, I tried copynig the FreeBASIC approach.<br>
--Keep an eye on/out for builtins/bigint.e, which may one day improve on this (inline assembly etc).

atom t0 = time()

constant lim = 10000-0
while length(primes)<lim do add_block() end while
--?primes[1000000]  -- 15485863
--?primes[100000]   -- ??15485863

integer primorial = 1
 
for n=0 to 9 do
    if n!=0 then
        primorial *= primes[n]
    end if
    printf(1," primorial(%d) = %d\n",{n,primorial})
end for

--printf(1,"%d\n",power(2,53))  -- 9007199254740992
--printf(1,"%d\n",power(2,63))  -- 9223372036854776028
--{} = wait_key()
--9223372036854776028
--1000000000_000000000
  


--' could use GMP, but why not make are own big integer routine
--constant Base = 10        -- needs lim of 564000, 22 mins
--constant Base = 100       -- needs lim of 282000, 10 mins
--constant Base = 1000      -- needs lim of 188000, 7 mins
--constant Base = 1024      -- needs lim of 188000, 7 mins
--constant Base = 10000     -- needs lim of 141000, 39 mins
--constant Base = #10000    -- needs lim of 141000, 43 mins
--constant Base = 100000    -- needs lim of 113000, 39 mins 
--constant Base = 1000000   -- needs lim of 94000, 34 mins 
--constant Base = 10000000  -- needs lim of 81000, 29 mins 
--constant Base = 100000000 -- needs lim of 71000, 25 mins (64 bit only*)
--constant Base = 1000000000    -- needs lim of 63000, 2 mins (64 bit only*)
constant Base = iff(machine_bits()=32?1000:1000000000)
--(*it will run and seem correct on 32-bit, but cannot be accurate)
--9007199254740992
--10000000_0000000
--1073741824 
--10000_00000
--15485863
constant integer digits_per = length(sprint(Base))-1
--constant lim = 100000 -- ceil(563921/digits_per) [better, replace with append]
--constant ??lim = iff(machine_bits()=32?200000:100000)

?{Base,digits_per,lim}

-- start at the back, number grows to the right (like little endian)
sequence bigint = {primorial}
integer low = 9,
        high = 10
atom result

--include bigatom.e
--bigatom check = ba_new(primorial)
--constant string dpfmt = sprintf("%%0%dd",digits_per)
 
atom tc = time()+1

printf(1,"\n")
for pow=0 to length(sprint(lim))-2 do
    if pow>0 then
        low = high
        high = high * 10
    end if
    for n=low+1 to high do
        integer carry = 0,
                pn = primes[n]
        for i=1 to length(bigint) do
            result = pn*bigint[i]+carry
            carry = floor(result/Base)
            bigint[i] = result-carry*Base
        end for
        while carry <> 0 do
            if time()>tc then
                printf(1,"%d\r",length(bigint)*digits_per)
                tc = time()+1
            end if
            result = carry
            carry = floor(carry/Base)
            bigint &= result-carry*Base
        end while
--check = ba_mul(check,pn)
--string bis = sprint(bigint[$])
--for i=2 to length(bigint) do
--  bis &= sprintf(dpfmt,bigint[i])
--end for
--string bc = ba_sprint(check)
--  ?bis
--  ?bc
--if bis!=bc then
--  ?9/0
--end if
--{} = wait_key()
    end for
    integer l = length(sprint(bigint[$])) + (length(bigint)-1)*digits_per
    printf(1," primorial(%d) has %d digits (%s)\n",{high,l,elapsed(time()-t0)})
end for
 
 
puts(1, "Done!\n") 

{} = wait_key()
without warning
abort(0)

FINSL(32 BIT)
"started"
 primorial(0) = 1
 primorial(1) = 2
 primorial(2) = 6
 primorial(3) = 30
 primorial(4) = 210
 primorial(5) = 2310
 primorial(6) = 30030
 primorial(7) = 510510
 primorial(8) = 9699690
 primorial(9) = 223092870
{1000,3,100000,187973.6667}

 primorial(10) has 10 digits (0.4s)
 primorial(100) has 220 digits (0.4s)
 primorial(1000) has 3393 digits (0.4s)
 primorial(10000) has 45337 digits (4.0s)
 primorial(100000) has 563921 digits (7 minutes and 57s)
Done!

final (64 bit)
"started"
 primorial(0) = 1
 primorial(1) = 2
 primorial(2) = 6
 primorial(3) = 30
 primorial(4) = 210
 primorial(5) = 2310
 primorial(6) = 30030
 primorial(7) = 510510
 primorial(8) = 9699690
 primorial(9) = 223092870

 primorial(10) has 10 digits (0.1s)
 primorial(100) has 220 digits (0.1s)
 primorial(1000) has 3393 digits (0.1s)
 primorial(10000) has 45337 digits (1.5s)
 primorial(100000) has 563921 digits (2 minutes and 52s)
Done!



"started"
 primorial(0) = 1
 primorial(1) = 2
 primorial(2) = 6
 primorial(3) = 30
 primorial(4) = 210
 primorial(5) = 2310
 primorial(6) = 30030
 primorial(7) = 510510
 primorial(8) = 9699690
 primorial(9) = 223092870

 primorial(10) has 10 digits (0.4s)
 primorial(100) has 220 digits (0.4s)
 primorial(1000) has 3393 digits (0.5s)
 primorial(10000) has 45337 digits (12.1s)
 primorial(100000) has 563921 digits (24 minutes and 23s)
Done!

with pn, while/integer carry, base 1000000000 (and another test running)
 primorial(100000) has 563921 digits (24 minutes and 59s)
Done!

"started"
 primorial(10) has 10 digits
 primorial(100) has 235 digits
 primorial(1000) has 3301 digits
 primorial(10000) has 43583 digits

>>
{10,1,564000,563921}
 primorial(10000) has 45337 digits (10.5s)
 primorial(100000) has 563921 digits (22 minutes and 43s)

{100,2,282000,281960.5}
 primorial(10000) has 45337 digits (5.4s)
 primorial(100000) has 563921 digits (10 minutes and 44s)

{1000,3}
 primorial(10000) has 45337 digits (3.8s)
 primorial(100000) has 563921 digits (7 minutes and 40s)

wrong answers, speed test:
{1024,3,282000,187973.6667}
 primorial(10000) has 45182 digits (3.8s)
 primorial(100000) has 561991 digits (7 minutes and 52s)

{10000,4}
 primorial(10000) has 45337 digits (3.0s)
 primorial(100000) has 563921 digits (39 minutes and 45s)

wrong answers, speed test:
{65536,4,282000,140980.25}
 primorial(10000) has 37653 digits (12.5s)
 primorial(100000) has 468326 digits (43 minutes and 24s)

{100000,5}
 primorial(10000) has 45337 digits (14.9s)
 primorial(100000) has 563921 digits (39 minutes and 53s)

{1000000,6,282000,93986.83333}
 primorial(10000) has 45337 digits (16.6s)
 primorial(100000) has 563921 digits (34 minutes and 05s)

{10000000,7,282000,80560.14286}
 primorial(10000) has 45337 digits (14.3s)
 primorial(100000) has 563921 digits (29 minutes and 29s)

{100000000,8,282000,70490.125}
 primorial(10000) has 45337 digits (12.6s)
 primorial(100000) has 563921 digits (25 minutes and 29s)

{1000000000,9,171000,62657.88889}
 primorial(10000) has 45337 digits (11.1s)
 primorial(100000) has 563921 digits (22 minutes and 32s)

64 bit:
 primorial(0) = 1
 primorial(1) = 2
 primorial(2) = 6
 primorial(3) = 30
 primorial(4) = 210
 primorial(5) = 2310
 primorial(6) = 30030
 primorial(7) = 510510
 primorial(8) = 9699690
 primorial(9) = 223092870
{1000000000,9,171000,62657.88889}

 primorial(10) has 10 digits (0.6s)
 primorial(100) has 220 digits (0.6s)
 primorial(1000) has 3393 digits (0.6s)
 primorial(10000) has 45337 digits (2.0s)
 primorial(100000) has 563921 digits (2 minutes and 44s)
 primorial(1000000) has 6722809 digits (5 hours 29 minutes and 4s)
--(10^6 takes far too long on 32 bit)

<<
"started"
 primorial(0) = 1
 primorial(1) = 2
 primorial(2) = 6
 primorial(3) = 30
 primorial(4) = 210
 primorial(5) = 2310
 primorial(6) = 30030
 primorial(7) = 510510
 primorial(8) = 9699690
 primorial(9) = 223092870

 primorial(10) has 10 digits (0.3s)
 primorial(100) has 220 digits (0.3s)
 primorial(1000) has 3393 digits (0.3s)
 primorial(10000) has 45337 digits (14.7s)
--wrong! (oh, this is freebasic... probably before I added the while loop)
 primorial(100000) has 495334 digits (2 hours, 13 minutes and 35s)
[Primorial(100000) has 563921 digits, is what it should be!]
Done!



puts(1, "Done!\n") 

{} = wait_key()
without warning
abort(0)
--*/

-- from http://rosettacode.org/wiki/Long_multiplication#Phix
--/*
--Translation of: Euphoria
--Simple longhand multiplication. To keep things as simple as possible, this does not handle negative numbers.
--If bcd1 is a number split into digits 0..9, bcd9 is a number split into "digits" 000,000,000..999,999,999, which fit in an integer.
--They are held lsb-style mainly so that trimming a trailing 0 does not alter their value.
--PL: string(int) to bcd9 should be quite trivial...

constant base = 1_000_000_000
 
function bcd9_mult(sequence a, sequence b)
sequence c
integer j
atom ci
    c = repeat(0,length(a)+length(b))
    for i=1 to length(a) do
        j = i+length(b)-1
        c[i..j] = sq_add(c[i..j],sq_mul(a[i],b))
    end for
 
    for i=1 to length(c) do
        ci = c[i]
        if ci>base then
            c[i+1] += floor(ci/base) -- carry
            c[i] = remainder(ci,base)
        end if
    end for
 
    if c[$]=0 then
        c = c[1..$-1]
    end if
    return c
end function
 
function atom_to_bcd9(atom a)
sequence s = {}
    while a>0 do
        s = append(s,remainder(a,base))
        a = floor(a/base)
    end while
    return s
end function
 
function bcd9_to_str(sequence a)
string s = sprintf("%d",a[$])
    for i=length(a)-1 to 1 by -1 do
        s &= sprintf("%09d",a[i])
    end for
    -- (might want to trim leading 0s here)
    return s
end function
 
sequence a, b, c
 
a = atom_to_bcd9(power(2,32))
printf(1,"a is %s\n",{bcd9_to_str(a)})
 
b = bcd9_mult(a,a)  -- power(2,64)
printf(1,"a*a is %s\n",{bcd9_to_str(b)})
 
c = bcd9_mult(b,b)  -- power(2,128)
printf(1,"a*a*a*a is %s\n",{bcd9_to_str(c)})
Output:
a is 4294967296
a*a is 18446744073709551616
a*a*a*a is 340282366920938463488374607488768211456
--*/

-- for http://rosettacode.org/wiki/Generalised_floating_point_addition#Phix
-- (Note for use here I would rip out all the fraction handling/throw errors,
--  but some of the ideas behind the e handling all seem to be fairly sound,
--  and note that, for example, 1.75e2 is a perfectly good and valid integer.)
--/*
>>>
=={{header|Phix}}==
===bigatom===           (for [speed] comparison only...)
<lang Phix>include bigatom.e
{} = ba_scale(200) 

string s = "12345679",
       t = "123456790"
integer e = 63
for n = -7 to 21 do
    bigatom bd = ba_new(sprintf("%se%d",{s,e})),
            e1 = ba_new(sprintf("1e%d",e)),
            res = ba_add(ba_mul(bd,81),e1)
    printf(1,"%2d : %s\n",{n,ba_sprintf("%.eB",res)})
    s = t & s
    e -= 9
end for</lang>
{{out|Output (trimmed)}}
<pre>
-7 : 1e72
...
21 : 1e72
</pre>
===bcd/string arithmetic===
May yet form the basis of something useful in builtins. (same output)
<lang Phix>
--function bcd_normalise(string a)
----
---- eg "12.34e-3" == "0.01234" ==> "0","01234"
----                 and/or "1e2" ==> "100",""
----
--  integer d = find('.',a),
--          e = find('e',a)
--  string f = ""
--  if e then
--      integer {{z}} = scanf(a[e+1..$],"%d")
--      a = a[1..e-1]
--      if d then
--          -- eg 12.34e-3 == 1234e-5
--          a[d..d] = ""
--          z -= length(a)-d+1
--      end if
--      if z>=0 then
--          -- eg 1e2 ==> "100",""
--          a &= repeat('0',z)
--      elsif z<-length(a) then
--          -- eg 1234e-5 == 0.01234 ==> "0","01234"
--          {a,f} = {"0",repeat('0',-z-length(a))&a}
--      else
--          -- eg 1234e-3 == 1.234 ==> "1","234"
--          {a,f} = {a[1..z-1],a[z..$]}
--      end if
--  elsif d then
--      -- eg "12.34" ==> "12","34"
--      {a,f} = {a[1..d-1],a[d+1..$]}
--  end if
--  return {a,f}
--end function
--
--function digit(integer d1, d2, carry)
---- common code
--  integer digit = d1+d2-'0'+carry
--  carry = digit>'9'
--  if carry then digit -= 10 end if
--  return {digit,carry}
--end function
--
--function bcd_add(string a,b)
--  string af,bf    -- fractional parts
--  {a,af} = bcd_normalise(a)
--  {b,bf} = bcd_normalise(b)
--  if length(b)>length(a) then {a,b} = {b,a} end if
--  if length(bf)>length(af) then {af,bf} = {bf,af} end if
--  integer carry = 0
--  -- 1) add shorter fraction, eg .1234 + .01 ==> .1334 carry 0
--  --    (ie we don't touch the "34", and the '.' is implicit)
--  for i=length(bf) to 1 by -1 do
--      {af[i],carry} = digit(af[i],bf[i],carry)
--  end for
--  af = trim_tail(af,'0')
--  -- 2) add shorter integer, eg (9)909 + 101 ==> (9)010 carry 1
--  --    (these -ve indexes are simply going from right to left)
--  for i=-1 to -length(b) by -1 do
--      {a[i],carry} = digit(a[i],b[i],carry)
--  end for
--  -- 3) propagate carry into the longer integer a if possible
--  for i=-length(b)-1 to -length(a) by -1 do
--      if carry=0 then exit end if
--      {a[i],carry} = digit(a[i],'0',carry)
--  end for
--  -- 4) otherwise prepend it (cannot be >1)
--  if carry then a = "1"&a end if
--  if length(af) then a &= "."&af end if
--  return a
--end function
--
--string s = "12345679",
--     t = "123456790"
--integer e = 63
--for n = -7 to 21 do
--  string bd = sprintf("%se%d",{s,e}),
--         e1 = sprintf("1e%d",e),
--         res = bd
--  for i=1 to 4 do
--      res = bcd_add(bcd_add(res,res),res)
--  end for
--  res = bcd_add(res,e1)
--  if not find('.',res) then
--      string z = trim_tail(res,'0')
--      if z!=res then
--          res = sprintf("%se%d",{z,length(res)-length(z)})
--      end if
--  end if
--  printf(1,"%2d : %s\n",{n,res})
--  s = t & s
--  e -= 9
--end for
--</lang>

--*!/
--?{12.34e-3,1.234e-2,123.4e-4,1234e-5} -- (same, 0.01234)
--?1e-3+1.234
--?1e-3+12.34e-3
--?1e-3+12.34e3
--?bcd_add("1e-3","1.234")
--?bcd_add("1e-3","12.34e-3")
--?bcd_add("1e-3","12.34e3")
--{} = wait_key()
--*/

--/*
?convert_base("65535",10,16)        --> FFFF
?convert_base("FFFF",16,10)         --> 65535
?convert_base("65535",10,3)         --> 10022220020
?convert_base("10022220020",3,10)   --> 65535
?convert_base("FFFF",16,8)          --> 177777
?convert_base("177777",8,16)        --> FFFF
?convert_base("7",8,10)             --> FFFF
?convert_base("7",10,2)             --> 111
?convert_base("111",2,10)           --> 7
?convert_base("EC851A69B8ACD843164E10CFF70CF9E86DC2FEE3CF6F374B43C854E3342A2F1AC3E30C741CC41E679DF6D07CE6FA3A66083EC9B8C8BF3AF05D8BDBB0AA6CB3EF8C5BAA2A5E531BA9E28592F99E0FE4F95169A6C63F635D0197E325C5EC76219B907E4EBDCD401FB1986E4E3CA661FF73E7E2B8FD9988E753B7042B2BBCA76679",16,10)
?convert_base("166089946137986168535368849184301740204613753693156360462575217560130904921953976324839782808018277000296027060873747803291797869684516494894741699267674246881622658654267131250470956587908385447044319923040838072975636163137212887824248575510341104029461758594855159174329892125993844566497176102668262139513",10,16)
--*/

--/*
include builtins\bigint.e
bigint n = bi_new(1),
       o = bi_new(1)
atom ni = 1
for i=1 to 64 do
    n = bi_mul(n,2)
    o = bi_add(o,o)
    ?bi_sprint10(n)
    ?bi_sprint10(o)
--  ?bi_sprint_viahex(o)
    if i<=53 then
        ni += ni
        ?sprintf("%d",ni)
    end if
end for
?"===="
--?n
bigint n0 = n
bigint r
--trace(1)
for i=64 to 1 by -1 do
--if i=16 then trace(1) end if
--  {n,r} = bi_divX(n,2)
    {n,r} = bi_div3(n,2)
    if r!=BI_ZERO then ?9/0 end if
    ?bi_sprint10(n)
    if i<=53 then
        ni = ni/2
        ?sprintf("%d",ni)
    end if
end for
?"-----"
--*/

--/*
include builtins\bigint.e
--bigint n
--n = bi_new("1e500")
--constant lim = 100-0-00-0
--atom t0 = time()
--string r
--for i=1 to lim do
--  r = bi_sprint(n)
--end for
--atom t1 = time()
--for i=1 to lim do
----    r = bi_sprint_viahex(n)
--  r = bi_sprint10(n)
--end for
--atom t2 = time()
--?{t1-t0, t2-t1}

--for i=1 to 10 do
--  ?bi_sprint(bi_rand("11","15"))
--end for

--bigint b = bi_new("123456789")
bigint b = bi_new("123612365126358125381538125381523812538125381625381253825")
--bigint b = bi_new(9)
--
,{sq,rem} = bi_sqrt(b)
?bi_sprint(b)
?bi_sprint(bi_add(bi_mul(sq,sq),rem))
?bi_sprint(bi_add(bi_power(sq,2),rem))
?bi_sprint(bi_sub(bi_power(sq,2),bi_neg(rem)))

--sq = bi_new(4)
--sq = bi_neg(sq)
--b = bi_neg(b)
----?bi_sprint(sq)
----trace(1)
--bigint {q2,r2} = bi_div3(b,sq)
----bigint {q2,r2} = bi_div3(b,bi_neg(sq))
--?"=="
--?bi_sprint(q2)
--?bi_sprint(r2)
--?"==="
--?bi_sprint(bi_rmdr(b,sq))
--?bi_sprint(bi_mod(b,sq))

b = bi_sub(b,rem)
?bi_sprint(b)
?bi_sprint(bi_mul(sq,sq))

?bi_sprint(sq)
bigint {q2,r2} = bi_div3(b,sq)
--bigint {q2,r2} = bi_div(b,sq) -- DEV still loops...
?bi_sprint(q2)
?bi_sprint(r2)

--?bi_new(+65538)
--?bi_new(-65538)
-- is stored as {+1,{+1,+2}, and -65538 is stored as {-1,{+1,+2}}.

--  and note that, for example, 1.75e2 is a perfectly good and valid integer.)
--b = bi_new("1.75e2")
--?b
--?bi_sprint(b)

--no good (xor edx,edx):
function my_mod(integer x,y)
--x = abs(x)
--y = abs(y)
    #ilASM{
        mov eax,[x]
        mov ecx,[y]
        mov esi,eax
--      xor edx,edx
--      mov edx,eax
--      sar edx,31
        cdq             -- (cqo on 64-bit)
        idiv ecx
--      mul eax,edx [illegal instruction]
--      mov edx,eax
--      mov eax,edx
--      mov eax,ecx
--      mul edx
        mul ecx
        sub esi,eax
        mov [x], esi
          }
    return x
----    if equal(sign(x), sign(y)) then
----        return remainder(x,y)
----    end if
--  return x - y * floor(x / y)
end function

--/!*
procedure squint(integer a, b)
    bigint {q,r} = bi_div3(a,b)
--  bigint {q,r} = bi_div3(a,b,true),
--  bigint {q,r} = bi_div(a,b),
--          rab  = bi_rmdr(a,b),
--          mab  = bi_mod(a,b)
    integer rab  = remainder(a,b),
--          mab  = mod(a,b)
            mab  = my_mod(a,b)
    printf(1,"div3(%2d,%2d) = %2s rem %2s; rmdr(%2d,%2d) = %2s, mod(%2d,%2d) = %2s\n",
             {a,b,bi_sprint(q),bi_sprint(r),
              a,b,bi_sprint(rab),
              a,b,bi_sprint(mab)})
end procedure

squint(-9,-4)
squint(-9,+4)
squint(+9,-4)
squint(+9,+4)
--*!/

--div3(-9,-4) =  2 rem -1; rmdr(-9,-4) = -1, mod(-9,-4) = -1
--div3(-9, 4) = -2 rem -1; rmdr(-9, 4) = -1, mod(-9, 4) = -1*  (-9+12?)
--div3( 9,-4) = -2 rem  1; rmdr( 9,-4) =  1, mod( 9,-4) =  1*  ( 9-12?)
--div3( 9, 4) =  2 rem  1; rmdr( 9, 4) =  1, mod( 9, 4) =  1
--real:
--div3(-9,-4) =  2 rem -1; rmdr(-9,-4) = -1, mod(-9,-4) = -1
--div3(-9, 4) = -2 rem -1; rmdr(-9, 4) = -1, mod(-9, 4) =  3
--div3( 9,-4) = -2 rem  1; rmdr( 9,-4) =  1, mod( 9,-4) = -3
--div3( 9, 4) =  2 rem  1; rmdr( 9, 4) =  1, mod( 9, 4) =  1
--bi_div:
--div3(-9,-4) =  2 rem  1; rmdr(-9,-4) = -1, mod(-9,-4) = -1
--div3(-9, 4) = -2 rem  1; rmdr(-9, 4) = -1, mod(-9, 4) = -1
--div3( 9,-4) = -2 rem  1; rmdr( 9,-4) =  1, mod( 9,-4) =  1
--div3( 9, 4) =  2 rem  1; rmdr( 9, 4) =  1, mod( 9, 4) =  1
--bi_div,fixed..
--div3(-9,-4) =  2 rem -1; rmdr(-9,-4) = -1, mod(-9,-4) = -1
--div3(-9, 4) = -2 rem -1; rmdr(-9, 4) = -1, mod(-9, 4) = -1
--div3( 9,-4) = -2 rem  1; rmdr( 9,-4) =  1, mod( 9,-4) =  1
--div3( 9, 4) =  2 rem  1; rmdr( 9, 4) =  1, mod( 9, 4) =  1
--div3 with floor:
--div3(-9,-4) =  1 rem -1; rmdr(-9,-4) = -1, mod(-9,-4) = -1
--div3(-9, 4) = -3 rem -1; rmdr(-9, 4) = -1, mod(-9, 4) = -1
--div3( 9,-4) = -1 rem  1; rmdr( 9,-4) =  1, mod( 9,-4) =  1
--div3( 9, 4) =  3 rem  1; rmdr( 9, 4) =  1, mod( 9, 4) =  1
--div3 with floor/signR:
--div3(-9,-4) =  3 rem -1; rmdr(-9,-4) = -1, mod(-9,-4) = -1
--div3(-9, 4) = -3 rem -1; rmdr(-9, 4) = -1, mod(-9, 4) =  3
--div3( 9,-4) = -3 rem  1; rmdr( 9,-4) =  1, mod( 9,-4) = -3
--div3( 9, 4) =  3 rem  1; rmdr( 9, 4) =  1, mod( 9, 4) =  1
--finally:
--div3(-9,-4) =  2 rem -1; rmdr(-9,-4) = -1, mod(-9,-4) = -1
--div3(-9, 4) = -2 rem -1; rmdr(-9, 4) = -1, mod(-9, 4) =  3
--div3( 9,-4) = -2 rem  1; rmdr( 9,-4) =  1, mod( 9,-4) = -3
--div3( 9, 4) =  2 rem  1; rmdr( 9, 4) =  1, mod( 9, 4) =  1

--my_mod as just return x-y*floor(x/y) [good!]:
--div3(-9,-4) =  2 rem -1; rmdr(-9,-4) = -1, mod(-9,-4) = -1
--div3(-9, 4) = -2 rem -1; rmdr(-9, 4) = -1, mod(-9, 4) =  3
--div3( 9,-4) = -2 rem  1; rmdr( 9,-4) =  1, mod( 9,-4) = -3
--div3( 9, 4) =  2 rem  1; rmdr( 9, 4) =  1, mod( 9, 4) =  1


--my_mod():
--div3(-9,-4) =  2 rem -1; rmdr(-9,-4) = -1, mod(-9,-4) =  3
--div3(-9, 4) = -2 rem -1; rmdr(-9, 4) = -1, mod(-9, 4) =  3
--div3( 9,-4) = -2 rem  1; rmdr( 9,-4) =  1, mod( 9,-4) =  1
--div3( 9, 4) =  2 rem  1; rmdr( 9, 4) =  1, mod( 9, 4) =  1



--trace(1)
--?bi_sprint10(bi_new("1000"))
--?bi_sprint10(q2)
--constant lim = 1000-00
--atom t0 = time()
--string rs
--for i=1 to lim do
--  rs = bi_sprint(q2)
--end for
--atom t1 = time()
--for i=1 to lim do
----    rs = bi_sprint_viahex(n)
--  rs = bi_sprint10(n)
--end for
--atom t2 = time()
--?{t1-t0, t2-t1}
--
--?3.781/0.141

--  Printing 12340000 with PT_SHOWCOMMAS ==> 12,340,000
--                and with PT_SCIENTIFIC ==> 1234e4
--                but with     both      ==> 12,340e3
?bi_sprint10(12340000)
?bi_sprint10(12340000,10,PT_SHOWCOMMAS)
?bi_sprint10(12340000,10,PT_SCIENTIFIC)
?bi_sprint10(12340000,10,PT_SCIENTIFIC+PT_SHOWCOMMAS)
?bi_sprint10(0)

--b = bi_new_bin(x"12345678123456781234567812345678")
--b = bi_new_bin(x"87645321876453218764532187645321")
--b = bi_new_bin(x"78563412785634127856341278563412")
--b = bi_new_bin(x"56781234567812345678123456781234")
--b = bi_new_bin(x"34127856341278563412785634127856")       -- bingo!
--b = bi_new("24197857161011715162171839636988778104")
--b = bi_new_bin(x"0000FEFF")
b = bi_new_bin(x"01000200")
?bi_sprint10(b,10)
--?bi_sprint10(b,16)
--unsigned int a [] = { 0x12345678, 0x12345678, 0x12345678, 0x12345678 };

--with profile_time

--include bigatom.e
--atom t0 = time()
--{} = ba_scale(200) 
--
--for zz=1 to 1000 do
--string s = "12345679",
--     t = "123456790"
--integer e = 63
--for nn = -7 to 0 do
--  bigatom bd = ba_new(sprintf("%se%d",{s,e})),
--          e1 = ba_new(sprintf("1e%d",e)),
--          res = ba_add(ba_mul(bd,81),e1)
--if zz=1000 then
--  printf(1,"%2d : %s\n",{nn,ba_sprintf("%.eB",res)})
--end if
--  s = t & s
--  e -= 9
--end for
--end for
--?elapsed(time()-t0)   -- 0.4s

--include bigint.e
--atom t0 = time()
--
--for zz=1 to 1000 do
--string s = "12345679",
--     t = "123456790"
--integer e = 63
--for nn = -7 to 0 do
----    bigint bd = bi_new(sprintf("%se%d",{s,e}))
--  string bis = sprintf("%se%d",{s,e})
--  bigint bd = bi_new(bis)
--  bigint e1 = bi_new(sprintf("1e%d",e))
--  bigint res = bi_add(bi_mul(bd,81),e1)
--if zz=1000 then
----?bis
----"12345679012345679012345679012345679012345679012345679012345679012345679"
----?bd
----?{nn,e}
--  printf(1,"%2d : %s\n",{nn,bi_sprint10(res,10,PT_SCIENTIFIC)})
--end if
--  s = t & s
--  e -= 9
--end for
--end for
----?elapsed(time()-t0)     -- 5.7s!! (OH DEAR!)
--?elapsed(time()-t0)       -- 0.5s (.. ugh, not exactly a saving!)

--printf(1,"%g\n",10000)
--b = bi_new(10000)
--?bi_sprint10(b,10,PT_SCIENTIFIC)

--
--{"res","77151769820433191243225629121"}
--{"rmdr","142349062381465529672634003849"}
--b = bi_new("77151769820433191243225629121")
--?bi_sprint(bi_add(bi_mul(b,b),"142349062381465529672634003849"))
--oops:"295492930267339538908528371917096838881893779591989605"
--     123612365126358125381538125381523812538125381625381253825
--{"res","77151769820433191243225629120"}
--{"rmdr","296652602022331912159085262090"}

--offs:
--{"res","77151769820433191243225629121"}
--{"rmdr","142349062381465529672634003849"}
--"5952395586425105774570954399331718284989282412978877236490"
--"5952395586425105774570954399331718284989282412978877236490"
--"done"

--bigint a = bi_new(power(2,32))
--printf(1,"a is %s\n",{bi_sprint(a)})
-- 
--bigint b = bi_mul(a,a)    -- power(2,64)
--printf(1,"a*a is %s\n",{bi_sprint(b)})
-- 
--bigint c = bi_mul(b,b)    -- power(2,128)
--printf(1,"a*a*a*a is %s\n",{bi_sprint(c)})
--Output:
--a is 4294967296
--a*a is 18446744073709551616
--a*a*a*a is 340282366920938463488374607488768211456
--yup:
--a is 4294967296
--a*a is 18446744073709551616
--a*a*a*a is 340282366920938463463374607431768211456
--"done"

--?bi_gcd(21,14)    -- 7
--?bi_lcm(21,14)    -- 42

--constant lim = 100-0-00
--atom t0 = time()
--for i=1 to lim do
--  {} = bi_divX(n0,2)
--end for
--atom t1 = time()
--for i=1 to lim do
--  {} = bi_div(n0,2)
--end for
--atom t2 = time()
--?{t1-t0, t2-t1}
--?6.016/0.015


--DEV re-test once we have a better bi_mod, and with 
--                      those variations on bi_power...
--integer bb = 123456789,--56789,
--      exponent = 2000,
--      modulus = 12
--bigint res, res2
--atom t0 = time()
--res = bi_mod(bi_power(bb,exponent),modulus)
--atom t1 = time()
--res2 = bi_mod_exp(bb, exponent, modulus)
--atom t2 = time()
--?{"bi_mod",res}
--?{"bi_mod_exp",res2}
--?{t1-t0,t2-t1}

--include bigatom.e
--
--bigatom res, res2
----exponent = 1234
--atom t0 = time()
--res = ba_mod(ba_power(bb,exponent),modulus)
--atom t1 = time()
--res2 = ba_mod_exp(bb, exponent, modulus)
--atom t2 = time()
--?{"ba_mod",res}
--?{"ba_mod_exp",res2}
--?{t1-t0,t2-t1}

--{1,0,{9}}
--{1,0,{9}}
--{8.5,0.016}



--global function bi_mod_exp(object b, exponent, modulus)
--
-- b/exponent/modulus can be integer/string/bigatom.
-- returns bi_mod(bi_power(b,exponent),modulus), aka 
-- (b^exponent)%modulus, but //much// faster, eg
--  b=123456789, exponent=1234, modulus=12 takes this
--  ~0s but ba_mod(ba_power(..)) about 8.6s, and if you
--  increase the exponent to 12345 you will simply give
--  up on the longhand method (or run out of memory),
--  whereas this still finishes near-instantly.


--/*
bignum bn = {2}

for i=1 to 5 do
    bn = bn_dec(bn)
    ?{bn,bn_curto_atom(bn)}
end for
bn = bn_mult(bn,{2})
?{bn,bn_curto_atom(bn)}
bignum {q,r} = bn_div(bn,{2})
?{bn,q,r,bn_curto_atom(r),bn_curto_atom(r)}
--?{bn,bn_div(bn,{3})}
--*/

--*/

--SIMULA: (well, it is simple... just implement it and test for speed.)
--/*
class bignum;
begin
 
    boolean procedure tiszero(t); text t;
        tiszero := t = "0";
 
    text procedure tshl(t); text t;
        tshl :- if tiszero(t) then t else t & "0";
 
    text procedure tshr(t); text t;
        tshr :- if t.length = 1 then "0" else t.sub(1, t.length - 1);
 
    integer procedure tsign(t); text t;
        tsign := if tiszero(t) then 0
            else if t.sub(1, 1) = "-" then -1
            else 1;
 
    text procedure tabs(t); text t;
        tabs :- if tsign(t) < 0 then t.sub(2, t.length - 1) else t;
 
    text procedure tnegate(t); text t;
        tnegate :- if tsign(t) <= 0 then tabs(t) else ("-" & t);
 
    text procedure treverse(t); text t;
    begin
        integer i, j;
        i := 1; j := t.length;
        while i < j do
        begin character c1, c2;
            t.setpos(i); c1 := t.getchar;
            t.setpos(j); c2 := t.getchar;
            t.setpos(i); t.putchar(c2);
            t.setpos(j); t.putchar(c1);
            i := i + 1;
            j := j - 1;
        end;
        treverse :- t;
    end treverse;
 
    integer procedure tcmpunsigned(a, b); text a, b;
    begin
        integer alen, blen, result;
        alen := a.length; blen := b.length;
        if alen < blen then
            result := -1
        else if alen > blen then
            result := 1
        else begin
            integer cmp, i; boolean done;
            a.setpos(1);
            b.setpos(1);
            i := 1;
            while i <= alen and not done do
            begin
                i := i + 1;
                cmp := rank(a.getchar) - rank(b.getchar);
                if not (cmp = 0) then
                    done := true;
            end;
            result := cmp;
        end;
        tcmpunsigned := result;
    end tcmpunsigned;
 
    integer procedure tcmp(a, b); text a, b;
    begin
        boolean aneg, bneg;
        aneg := tsign(a) < 0; bneg := tsign(b) < 0;
        if aneg and bneg then
           tcmp := -tcmpunsigned(tabs(a), tabs(b))
        else if not aneg and bneg then
           tcmp := 1
        else if aneg and not bneg then
           tcmp := -1
        else
           tcmp := tcmpunsigned(a, b);
    end tcmp;
 
    text procedure taddunsigned(a, b); text a, b;
    begin
        integer carry, i, j;
        text bf;
        i := a.length;
        j := b.length;
        bf :- blanks(max(i, j) + 1);
        while i >= 1 or j >= 1 do begin
            integer x, y, z;
            if i >= 1 then begin
                a.setpos(i); i := i - 1; x := rank(a.getchar) - rank('0');
            end;
            if j >= 1 then begin
                b.setpos(j); j := j - 1; y := rank(b.getchar) - rank('0');
            end;
            z := x + y + carry;
            if z < 10 then
            begin bf.putchar(char(z + rank('0'))); carry := 0;
            end else
            begin bf.putchar(char(mod(z, 10) + rank('0'))); carry := 1;
            end;
        end;
        if carry > 0 then
            bf.putchar(char(carry + rank('0')));
        bf :- treverse(bf.strip);
        taddunsigned :- bf;
    end taddunsigned;
 
    text procedure tadd(a, b); text a, b;
    begin
        boolean aneg, bneg;
        aneg := tsign(a) < 0; bneg := tsign(b) < 0;
        if not aneg and bneg then          ! (+7)+(-5) =  (7-5) =   2 ;
            tadd :- tsubunsigned(a, tabs(b))
        else if aneg and not bneg then     ! (-7)+(+5) =  (5-7) =  -2 ;
            tadd :- tsubunsigned(b, tabs(a))
        else if aneg and bneg then         ! (-7)+(-5) = -(7+5) = -12 ;
            tadd :- tnegate(taddunsigned(tabs(a), tabs(b)))
        else                               ! (+7)+(+5) =  (7+5) =  12 ;
            tadd :- taddunsigned(a, b);
    end tadd;
 
    text procedure tsubunsigned(a, b); text a, b;
    begin
        integer i, j, carry;
        i := a.length; j := b.length;
        if i < j or i = j and a < b then
            tsubunsigned :- tnegate(tsubunsigned(b, a)) else
        begin
            text bf;
            bf :- blanks(max(i, j) + 1);
            while i >= 1 or j >= 1 do
            begin
                integer x, y, z;
                if i >= 1 then
                begin a.setpos(i); i := i - 1;
                    x := rank(a.getchar) - rank('0');
                end;
                if j >= 1 then
                begin b.setpos(j); j := j - 1;
                    y := rank(b.getchar) - rank('0');
                end;
                z := x - y - carry;
                if z >= 0 then
                begin
                    bf.putchar(char(rank('0') + z));
                    carry := 0;
                end else
                begin
                    bf.putchar(char(rank('0') + mod(10 + z, 10)));
                    carry := 1; ! (z / 10);
                end;
            end;
            bf :- bf.strip;
            bf :- treverse(bf);
            bf.setpos(1);
            while bf.length > 1 and then bf.getchar = '0' do
            begin
                bf :- bf.sub(2, bf.length - 1);
                bf.setpos(1);
            end;
            tsubunsigned :- bf;
        end;
    end tsubunsigned;
 
    text procedure tsub(a, b); text a, b;
    begin
        boolean aneg, bneg;
        aneg := tsign(a) < 0; bneg := tsign(b) < 0;
        if aneg and bneg then              ! (-7)-(-5) = -(7-5) =  -2 ;
            tsub :- tnegate(tsubunsigned(tabs(a), tabs(b)))
        else if not aneg and bneg then     ! (+7)-(-5) =  (7+5) =  12 ;
            tsub :- taddunsigned(a, tabs(b))
        else if aneg and not bneg then     ! (-7)-(+5) = -(7+5) = -12 ;
            tsub :- tnegate(taddunsigned(tabs(a), b))
        else                               ! (+7)-(+5) =  (7-5) =   2 ;
            tsub :- tsubunsigned(a, b);
    end tsub;
 
    text procedure tmulunsigned(a, b); text a, b;
    begin
        integer alen, blen;
        alen := a.length; blen := b.length;
        if alen < blen then
            tmulunsigned :- tmulunsigned(b, a)
        else begin
            text product; integer j;
            product :- "0";
            for j := 1 step 1 until blen do begin
                text part; integer i, y, carry;
                b.setpos(j); y := rank(b.getchar) - rank('0');
                part :- blanks(alen + blen + 1); part.setpos(1);
                for i := alen step -1 until 1 do begin
                    integer x, z;
                    a.setpos(i); x := rank(a.getchar) - rank('0');
                    z := x * y + carry;
                    if z < 10 then begin
                        part.putchar(char(rank('0') + z));
                        carry := 0;
                    end else begin
                        part.putchar(char(rank('0') + mod(z, 10)));
                        carry := z // 10;
                    end;
                end;
                if carry > 0 then
                    part.putchar(char(rank('0') + carry));
                part :- part.sub(1, part.pos - 1);
                part :- treverse(part);
                part.setpos(1);
                while part.length > 1 and then part.getchar = '0' do
                begin
                    part :- part.sub(2, part.length - 1);
                    part.setpos(1);
                end;
                product :- taddunsigned(tshl(product), part);
            end;
            tmulunsigned :- product;
        end;
    end tmulunsigned;
 
    text procedure tmul(a, b); text a, b;
    begin
        boolean aneg, bneg;
        aneg := tsign(a) < 0; bneg := tsign(b) < 0;
        if aneg and bneg then              ! (-7)*(-5) =  (7*5) =>  35 ;
           tmul :- tmulunsigned(tabs(a), tabs(b))
        else if not aneg and bneg then     ! (+7)*(-5) = -(7*5) => -35 ;
           tmul :- tnegate(tmulunsigned(a, tabs(b)))
        else if aneg and not bneg then     ! (-7)*(+5) = -(7*5) => -35 ;
           tmul :- tnegate(tmulunsigned(tabs(a), b))
        else                               ! (+7)*(+5) =  (7*5) =>  35 ;
           tmul :- tmulunsigned(a, b);
    end tmul;
 
    class divmod(div,mod); text div,mod;;
 
    ref(divmod) procedure tdivmodunsigned(a, b); text a, b;
    begin
        integer cc;
        ref(divmod) result;
        if tiszero(b) then
            error("division by zero");
        cc := tcmpunsigned(a, b);
        if cc < 0 then
            result :- new divmod("0", a)
        else if cc = 0 then
            result :- new divmod("1", "0")
        else begin
            integer alen, blen, aidx;
            text q, r;
            alen := a.length; blen := b.length;
            q :- blanks(alen); q.setpos(1);
            r :- blanks(alen); r.setpos(1);
            r := a.sub(1, blen - 1); r.setpos(blen);
            for aidx := blen step 1 until alen do
            begin
                integer count; boolean done;
                if tiszero(r.strip) then
                    r.setpos(1);
                a.setpos(aidx); r.putchar(a.getchar);
                while not done do
                begin
                    text diff;
                    diff :- tsubunsigned(r.strip, b);
                    if tsign(diff) < 0 then
                        done := true
                    else begin
                        r := diff; r.setpos(diff.length + 1);
                        count := count + 1;
                    end;
                end;
                if (not (count = 0)) or (not (q.pos = 1)) then
                    q.putchar(char(count + rank('0')));
            end;
            result :- new divmod(q.strip, r.strip);
        end;
        tdivmodunsigned :- result;
    end tdivmodunsigned;
 
    ref(divmod) procedure tdivmod(a, b); text a, b;
    begin
        boolean aneg, bneg; ref(divmod) result;
        aneg := tsign(a) < 0; bneg := tsign(b) < 0;
        if aneg and bneg then
            begin
                result :- tdivmod(tabs(a), tabs(b));
                result.mod :- tnegate(result.mod);
            end
        else if not aneg and bneg then
            begin
                result :- tdivmod(a, tabs(b));
                result.div :- tnegate(result.div);
            end
        else if aneg and not bneg then
            begin
                result :- tdivmod(tabs(a), b);
                result.div :- tnegate(result.div);
                result.mod :- tnegate(result.mod);
            end
        else
            result :- tdivmodunsigned(a, b);
        tdivmod :- result;
    end tdivmod;
 
    text procedure tdiv(a, b); text a, b;
        tdiv :- tdivmod(a, b).div;
 
    text procedure tmod(a, b); text a, b;
        tmod :- tdivmod(a, b).mod;
 
end bignum;
--*/
