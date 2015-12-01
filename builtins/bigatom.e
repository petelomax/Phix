-- -------------------------------------------------------------------------------------------
--    Copyright (C) 2014 Carlos Gómez Andreu (cargoan)
--
--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-- -------------------------------------------------------------------------------------------
--
-- bigatom.e
--    by Carlos J. Gómez Andreu (cargoan)       translated to english by google (petelomax)
--
--    My floating point numbers in base 10.
--
--    The function b_a_log() and related bigfixedmath.e (Lucius L. Hilley III) are adapted
--    from the Euphoria Archive.
--    More accurate than the internal of euphoria and it went better with bigatoms.
--
--    The function b_a_exp() adapted from the BC manpages.
--
--    The function euler() on the algorithm I have no idea, taken from an example in Modula-2
--    a Modula-old Oberon compiler that has a looooong time, used in OS / 2 called XDS.
--    The original algorithm was written in Algol by Serge Batalov, rewritten in Modula-2
--    and amended by Eugene Nalimov and Pavel Zemtsov.
--    Adapted Euphoria for me (unlimited decimals).
--    
-- -------------------------------------------------------------------------------------------
--
-- A bigatom is a sequence of three elements, the sign, the exponent and the mantissa.
-- The sign is 1 if the number is positive, -1 if negative or 0 if zero.
--
-- The value of a bigatom is evaluated in the form:
--      value = sign * mantissa * 10^exponent
-- The decimal point is implied after the first digit of the mantissa.
--
-- The same value can have different representations.
-- eg. the number -23.456 would be represented as follows:
--    { -1, 1, {2, 3, 4, 5, 6} } = -1 * 2.3456 * 10^1
--    or as:
--    { -1, 3, {0, 0, 2, 3, 4, 5, 6} } = -1 * 0.023456 * 10^3
--    or even:
--    { -1, 1, {2, 3, 4, 5, 6, 0, 0} } = -1 * 2.345600 * 10^1
--    etc.
--
-- Always reduced to the shortest form and storing only digits
-- significant number; eliminating zeros to the right and left
-- first nonzero digit and adjusting the exponent accordingly.
--
-- The normalize() function is responsible for this task.
--
-- -------------------------------------------------------------------------------------------

--namespace bigatom

--without warning &= { override }
without type_check

-- -------------------------------------------------------------------------------------------

-- maximum number of digits of an atom (for binary conversion => decimal)
--ifdef BITS64 then
--   constant ATOM_DIGS = 19     -- 18 to 20 or so (to taste each)
--elsedef
--   constant ATOM_DIGS = 15     -- ??? (Not sure if more in 32 bits)
--end ifdef
integer ATOM_DIGS
    if machine_bits()=64 then
        ATOM_DIGS = 19
    else
        ATOM_DIGS = 15
    end if


-- structure of a bigatom
--global 
enum SIGN, EXPONENT, DIGITS
--
-- valid signs
enum SG_NOVALUE = -2,
     SG_MINUS, -- -1,
     SG_ZERO,  --  0,
     SG_PLUS   -- +1

type t_sign(object s)
    return integer(s) and (s=SG_NOVALUE or s=SG_MINUS or s=SG_ZERO or s=SG_PLUS)
end type

constant tdigits = {0,1,2,3,4,5,6,7,8,9}

type t_digits(object x)
    if sequence(x) then
        for i=1 to length(x) do
            if not find(x[i], tdigits) then
                return 0
            end if
        end for
        return 1
    end if
    return 0
end type

global type bigatom(object x)
    if sequence(x)
    and length(x)=DIGITS
    and t_digits(x[DIGITS])
    and t_sign(x[SIGN])
    and integer(x[EXPONENT]) then
        return 1
    end if
    return 0
end type

-- -------------------------------------------------------------------------------------------


-- scale
integer SCALE = 25    -- number of decimal places (positions or digits)
integer SC_MODE = 1   -- 0 positions are not 0 are digits

-- NO_VALUE: not a real number.
-- back when a result is undefined, indeterminate, complex, ...
-- eg.  division by zero, logarithm of zero or negative, following a negative ...
constant NO_VALUE = {SG_NOVALUE, 0, {}}

-- definition of zero
global 
constant BA_ZERO = {SG_ZERO, -1, {}}  -- BA_ZERO = normalize({0,0,{0}})
-- useful in some places
global 
constant BA_ONE  = {SG_PLUS, 0, {1}}  -- BA_ONE  = ba_new(1)

-- characters allowed in the entry
constant SPACE     = ' '    -- space character
constant UNDERLINE = '_'    -- underscore character

-- sign characters
constant SPLUS     = '+'    -- positive sign character
constant SMINUS    = '-'    -- negative character
constant SZERO     = SPACE  -- sign character zero

integer DFCHAR = SPACE  -- decimal filling character (format 'c')


-- -------------------------------------------------------------------------------------------


--
-- Sets the number of decimal places and the way (position or digits)
--    decs: (atom) the number of decimal places
--          (Sequence) if null, returns the current values
--                     if not, the first element is the number of decimal places
--                     and the second, if any, is the way
--
--    mode: if zero, establishes that positions from the decimal point
--          greater than zero, digits (if 0 < n < 1 does not have leading zeros) this allows
--                             handling very small numbers without having to use a very large scale
--          less than zero, maintained the previous mode
--
--    Returns a sequence with the previous values: {scale, mode}
--
global function scale(object decs = -1, integer mode = -1)
sequence prev = {SCALE, SC_MODE}

    if sequence(decs) then
        if length(decs) then
            if length(decs)>1 then
                mode = not (not decs[2])
            end if
            decs = decs[1]
        else
            decs = -1
            mode = -1
        end if
    end if

    if decs>=0 then
        SCALE = floor(decs)
    end if
    if mode>=0 then
        SC_MODE = not (not mode)
    end if

    return prev
end function
--


--
-- Returns the scale of a bigatom
-- (the number of decimals)
--
global function scale_of(bigatom N)
integer decs = length(N[DIGITS])-N[EXPONENT]-1
    return decs*(decs>0)
end function
--


--
-- Returns a bigatom equivalent to an integer or atom or, better yet, string.
--
-- Allows use of separators (underlines) in the string, if that aids readability, eg:
--
--    s = ba_new(1.2345)                -- s = {1,1,{1,2,3,4,5}}
--    s = ba_new("1_234_567.891_123")   -- s = {1,6,{1,2,3,4,5,6,7,8,9,1,1,2,3}}
--    s = ba_new("-.2_345e-1_3")        -- s = {-1,-14,{2,3,4,5}}
--
-- If you recall, binary floating point numbers /cannot/ hold the value 0.1 precisely, because 
-- a mix of 1/2, 1/4, 1/8, 1/16 etc gets close but never spot-on. In most cases the last digit 
-- rounds properly (deep inside printf) but a) there is no guarantee of that and b) expanding 
-- it to "0.0999999999999999" and then rounding back down to "0.1" is obviously a measurable 
-- performance overhead. Similar effects do not occur when converting strings, hence it may in
-- fact be better (faster and more accurate) to invoke ba_new("0.1") rather than ba_new(0.1).
-- Further, with an atom, you are inherently limited to 15 (on 32-bit, 19 on 64-bit) digits 
-- of precision, whereas a string can be as long (or as short) as you want it to be, and since
-- conversion occurs character-by-character there is no loss of accuracy whatsoever.
--
-- Returns NO_VALUE if the string is invalid, empty, or not /completely/ recognisable as a 
--  number, as in all of the following examples:
--      "", "-.2_3	 45e-1 _3", "1 234 567.891_123", "e-231.2345 hola",
--      "- 231.2345hola", "- 231.2345e-23hola", "__ -231.2345 e-12e++..--Eholae"
--
--with trace
global function ba_new(object n)
integer exponent, len, ndigits
sequence snum
sequence big
--integer c
object c
integer pos = 0, sflag = 0

    if bigatom(n) then
        return n
    elsif atom(n) then
        if integer(n) then
            return int_to_bigatom(n)
        end if

--PL I assume this is/was meant to be eu:log not b_a_log...
        ndigits = -floor(-log(abs(n))/log(10))  -- enough
        if ndigits>=0 then
            if ndigits>ATOM_DIGS then
                ndigits = ATOM_DIGS
            end if
            n = sprintf(sprintf("%%+.%df", ATOM_DIGS-ndigits), n)
            len = find('.', n)
            if not len then
                len = length(n)-1
            end if
            if len>ATOM_DIGS then
                n = int_value(n[1..ATOM_DIGS+1])
                n = sprintf("%+.f", n)
                n &= repeat('0', len-length(n)+1)
            end if
        else
            snum = sprintf("%.e", n)
            exponent = find('e', snum)
            if exponent then
                exponent = int_value(snum[exponent+1..$])
            end if
            n = sprintf(sprintf("%%+.%df", ATOM_DIGS-exponent), n)
        end if
    end if

    big = NO_VALUE  -- {SG_NOVALUE, 0, {}}  (NB: exponent is zero (PL))

    -- eliminates valid separators (underline)
    -- any invalid character causes NO_VALUE to be returned
    for i=1 to length(n) do
        c = n[i]
        if not integer(c) then return NO_VALUE end if
        if find(c, "+-") then
            if sflag then
                if i<length(n) then return NO_VALUE end if
                exit
            end if
            pos += 1
            n[pos] = c
            sflag = 1
        elsif find(c, ".0123456789") then
            pos += 1
            n[pos] = c
        elsif find(c, "eE") then
            pos += 1
            n[pos] = 'e'
            sflag = 0
        elsif c!=UNDERLINE then
            return NO_VALUE
        end if
    end for
    if pos=0 or find(n[pos],"+-.e") then return NO_VALUE end if
    n = n[1..pos]

    -- floating point
    pos = find('e', n)
    if pos then
--    big = ba_new(n[1..pos-1])
--    big[EXPONENT] += int_value(n[pos+1..$])
--    return normalize(big)
        -- PL
        big[EXPONENT] = int_value(n[pos+1..$])
        n = n[1..pos-1]
    end if

    -- fixed point
--  if length(n) then
    if n[1]=SMINUS then
        big[SIGN] = SG_MINUS
        n = n[2..$]
    else
        big[SIGN] = SG_PLUS
        if n[1]=SPLUS then
            n = n[2..$]
        end if
    end if

    pos = find('.',n)
    if pos then
        n[pos..pos] = ""
        big[EXPONENT] += pos-2
    else
        big[EXPONENT] += length(n)-1
    end if
    -- (PL ensure it is a dword-sequence, not a "string" of '\0'..'\t'!)
    snum = repeat(0,length(n))
    for i=1 to length(n) do
        snum[i] = n[i]-'0'
    end for
    big[DIGITS] = snum
--  end if

    return normalize(big)
end function
--


--
-- Compares two bigatoms
-- (NO_VALUE is smaller than any number)
--
global function ba_compare(bigatom A, bigatom B)
integer cmp = compare(A, B)

    if A[SIGN]=SG_MINUS and B[SIGN]=SG_MINUS then
        cmp = -cmp
    end if

    return cmp
end function
--


-- -------------------------------------------------------------------------------------------


--
-- ############################
-- ###   OUTPUT FUNCTIONS   ###
-- ############################
--

--
-- Returns the complete string representation of a bigatom
--
global function ba_sprint(bigatom N)
sequence str = to_string(N)
    if str[1]=SZERO then
        str = str[2..$]
    end if
    return str
end function


--
-- Outputs the complete representation of a bigatom to file
-- (1 = STDOUT, 2 = STDERR)
global procedure ba_print(integer file, bigatom N)
    puts(file, ba_sprint(N))
end procedure


--
-- Returns a bigatom as a formatted string representation
-- Note: You need a major overhaul, it is made ??on the fly but is functional.
--       It would be nice to integrate with other types, replace and pass all bigatoms
--       to printf or sprintf.  Actually this function only reads the format string and
--       calls to_string() that is executed by the conversion of the number and it fits
--       the size, the filling sets.
--
-- The format string consists of three parts: the header (the text goes ahead
-- number), the format itself and the tail (the text to be put back).   All optional.
--
-- If the chain is zero, the number with all digits will be represented.  Otherwise, the format
-- is delimited with the two characters '%' and 'B'.  The front and back of the format
-- are added before and behind respectively.  If it contains a format% B, the string is placed
-- before the number.
--
-- It is similar to printf(), the differences are:
--   - The size refers only to the integer part, not the total. If there is a front size
--     character that is not a digit between 1 and 9 or the sign, taken as filler character
--     to complete the whole part.
--
--   - If there is a zero behind the point, the sign will be if the number is not zero, but in
--     the representation.
--
--   - If the format ends in 'a' is filled with zeros decimal places unoccupied.
--     If ending in 'c' spaces are used but if it is preceded by a character other than
--     a digit, it is used to fill the decimal places.
--
--     'a' and 'c' is mainly used to produce encolumnadas outputs.
--     'c' significantly increases readability by putting only the necessary decimals.  If I dont know
--      specify a size 'c' is ignored.  Not 'a'.

--
--
-- fmt = '%' ['+'] [fchar] [size] [['.'] ['0'] [decs] ['a' | [char] 'c']  ['e'|'E'] ] 'B'
--
--          [+] always put sign (*)
--
--      [fchar] symbol (excluding any digit from 1 to 9, if the 0)
--
--       [size] minimum size of the integer portion of the number (if it can not be ignored)
--
--          [.] point, it separates only the parts of the format string
--
--          [0] put sign when the number is less than the limits of representation but
--              not zero in the current scale, resulting zeros with sign
--
--       [desc] "maximum" number of decimals to show (if negative, all decimal)
--
--  [a|[char]c] 'a': filled with zeros decimal places unoccupied.
--              'c': use the character that precedes it (if not a digit), or a space as
--                   fill character for the decimal part.  If you have not specified a
--                   size 'c' is ignored.
--  ['E' | 'e'] exponential format, write the number in exponential format, with 'e' or 'E'
--              the use of 'a' is redundant and 'c' is ignored, always filled with zeros.
--
-- When the fill character is '0' is reserved for the top position sign.
--
-- (*) The sign of zero (space) is only displayed when the pad character is '0'
--     the sign is placed in front of and not behind as filler in other cases.
--
global function ba_sprintf(sequence fmt, bigatom N)
sequence header, trailer
integer fpos
sequence ifmt, dfmt
integer decs, dp
integer c, sg = 0, size = 0, fchar = SPACE
integer zsign = 0, all = 0, exponent, expfmt = 0
sequence str
integer len

    if not length(fmt) then
        return ba_sprint(N)
    end if

    fpos = find('%', fmt)
    if fpos then
        header = fmt[1..fpos-1]
        fmt  = fmt[fpos+1..$]
        fpos = find('B', fmt)
        if fpos then
            trailer = fmt[fpos+1..$]
            fmt  = fmt[1..fpos-1]
        else
            --DEV bad format?
            trailer = fmt
            fmt  = ""
        end if
    else
        header = fmt
        fmt  = ""
        trailer = ""
    end if

    decs = -1
    dp = find('.', fmt)
    if dp then
        ifmt = fmt[1..dp-1]
        dfmt = fmt[dp+1..$]
        decs = 0
    else
        ifmt = fmt
        dfmt = ""
    end if

    while length(ifmt) do
        c = ifmt[1]
        if not sg and c=SPLUS then
            sg = 1
        elsif c<'1' or c>'9' then
            if fchar=SPACE then
                fchar = c
            end if
        else
            size = int_value(ifmt)
            exit
        end if
        ifmt = ifmt[2..$]
    end while

    if length(dfmt) then
        c = dfmt[$]
        if c='a' then
            all = 1
        elsif c='c' then
            c = dfmt[$-1]
            if size then
                all = 2
                if c<'0' or c>'9' then
                    DFCHAR = c
                    dfmt = dfmt[1..$-1]
                else
                    DFCHAR = SPACE
                end if
            end if
        elsif c='e' or c='E' then
            -- exponential format
            expfmt = 1+(c='E')
            exponent = N[EXPONENT]
            N[EXPONENT] = 0
            all = 1
        end if
        if all then
            dfmt = dfmt[1..$-1]
        end if

        while length(dfmt) do
            c = dfmt[1]
            if not zsign and c='0' then
                zsign = 1
            else
                decs = int_value(dfmt)
                exit
            end if
            dfmt = dfmt[2..$]
        end while
    end if

    if decs<0 then
        decs = scale_of(N)
    end if

    str = to_string(N, sg, decs, zsign, all)

    -- removes the sign of zero if the filling is not zero
    if str[1]=SZERO and fchar!='0' then
        if size then
            str[1] = fchar
        else
            str = str[2..$]
        end if
    elsif str[1]='<' and size and all then  -- NO_VALUE
        if all>1 then   -- 'c'
            str = str & repeat(DFCHAR, decs+1)
        else            -- 'a' (using filler integer part)
            str = str & repeat(fchar, decs+1)
        end if
    end if

    -- if no decimal point replaced by filling
    len = find('.', str)
    if len and str[len+1]=DFCHAR then
        str[len] = DFCHAR
    end if

    if len then
        decs = length(str)-len
        len -= 1
    else
        len = length(str)
        decs = 0
    end if

    -- filling integer part
    len = size-len-decs-(decs>0)
    if len>0 then
        if fchar='0' and str[1]!='<' then
            str = str[1] & repeat(fchar, len) & str[2..$]
        elsif str[1]='<' then
            str = repeat(DFCHAR, len) & str
        else
            str = repeat(fchar, len) & str
        end if
    end if
    if expfmt>1 then
        str &= 'E' & sprintf("%+d", exponent)
    elsif expfmt then
        str &= 'e' & sprintf("%+d", exponent)
    end if

    return header & str & trailer
end function


--
-- Outputs the representation format of a bigatom to file
-- (1 = STDOUT, 2 = STDERR)
--
global procedure ba_printf(integer file, sequence fmt, bigatom N)
    puts(file, ba_sprintf(fmt, N))
end procedure
--

-- -------------------------------------------------------------------------------------------


-- #################################
-- ###   ARITHMETIC OPERATIONS   ###
-- #################################


--
-- Returns a bigatom with the result of the sum of two numbers.
--
-- Supports Atoms, bigatoms and representation of numbers in a string
--
global function ba_add(object A, object B)
integer signA, signB
integer sign
integer exponent
sequence res

    if not bigatom(A) then
        A = ba_new(A)
    end if
    if not bigatom(B) then
        B = ba_new(B)
    end if

    signA = A[SIGN]
    signB = B[SIGN]
    if signA=SG_ZERO then
        return B
    elsif signB=SG_ZERO then
        return A
    elsif signA=SG_NOVALUE or signB=SG_NOVALUE then
        return NO_VALUE
    end if

    sign = SG_PLUS
    if signA=signB then
        sign = signA
    elsif signA=SG_MINUS then
        A[SIGN] = SG_PLUS
        return ba_sub(B, A)
    else -- signB = SG_MINUS
        B[SIGN] = SG_PLUS
        return ba_sub(A, B)
    end if

    {A, B} = align(A, B)

    exponent = A[EXPONENT]
    res = digits_add(A[DIGITS], B[DIGITS])
    if res[1] then
        exponent += 1
    end if
    res = {sign, exponent, res[2]}

    return normalize(res)
end function


--
-- Returns a bigatom with the result of subtraction of two numbers.
--
-- Supports Atoms, bigatoms and representation of numbers in a string
--
global function ba_sub(object A, object B)
integer signA, signB
sequence res
integer sign

    if not bigatom(A) then
        A = ba_new(A)
    end if
    if not bigatom(B) then
        B = ba_new(B)
    end if

    signA = A[SIGN]
    signB = B[SIGN]
    if signA=SG_ZERO then
        B[SIGN] = -signB
        return B
    elsif signB=SG_ZERO then
        return A
    elsif signA=SG_NOVALUE or B[SIGN]=SG_NOVALUE then
        return NO_VALUE
    end if

    if signA=SG_MINUS or signB=SG_MINUS then
        B[SIGN] = -signB
        return ba_add(A, B)
    end if

    {A, B} = align(A, B)

    res = digits_sub(A[DIGITS], B[DIGITS])
    sign = SG_PLUS
    if res[1] then
        sign = SG_MINUS
    end if

    return normalize({sign, A[EXPONENT], res[2]})
end function
--


--
-- Returns a bigatom with the result of multiplying two numbers.
--
-- Supports Atoms, bigatoms and representation of numbers in a string
--
global function ba_multiply(object A, object B, integer bRound = 0)
integer signA, signB
integer signR
sequence res, digsA, digsB
integer expA, expB, expR
integer ndecs, len

    if not bigatom(A) then
        A = ba_new(A)
    end if
    if not bigatom(B) then
        B = ba_new(B)
    end if

    signA = A[SIGN]
    signB = B[SIGN]
    if signA=SG_ZERO or signB=SG_ZERO then
        return BA_ZERO
    elsif signA=SG_NOVALUE or signB=SG_NOVALUE then
        return NO_VALUE
    end if

    signR = signA*signB
    digsA = A[DIGITS]
    digsB = B[DIGITS]
    expA = A[EXPONENT]
    expB = B[EXPONENT]
    expR = expA+expB
    if equal({1}, digsA) then      -- power of 10
        res = {signR, expR, digsB}
    elsif equal({1}, digsB) then    -- power of 10
        res = {signR, expR, digsA}
    else
        res = digits_multiply(digsA, digsB)
        res = {signR, expR+(res[1]>0), res[2]}
    end if

    -- decimal limit
    if SC_MODE and res[EXPONENT]<0 then
        ndecs = SCALE+1
    else
        ndecs = res[EXPONENT]+SCALE+2
    end if
    if ndecs>0 then
        if bRound then
            res = round_digits(res, ndecs)
        else
            len = length(res[DIGITS])
            if len>ndecs then
                res[DIGITS] = remove(res[DIGITS], ndecs, len)
            end if
            res = normalize(res)
        end if
    else
        res = BA_ZERO
    end if

    return res
end function
--


--
-- Returns the result of integer division of two numbers.
--
-- Supports Atoms, bigatoms and representation of numbers in a string
--
-- The numbers are truncated before making division
--
global function ba_idivide(object A, object B)
integer signA, signB
sequence res, digsA
sequence quotient = {}, partial = {SG_PLUS,-1,{}}   -- +0
integer signR, expR = -1

    if not bigatom(A) then
        A = ba_new(A)
    end if
    if not bigatom(B) then
        B = ba_new(B)
    end if

    {A, B} = {ba_trunc(A), ba_trunc(B)}

    signA = A[SIGN]
    signB = B[SIGN]
    if signB=SG_ZERO 
    or signB=SG_NOVALUE 
    or signA=SG_NOVALUE then
        return NO_VALUE
    elsif signA=SG_ZERO then
        return BA_ZERO
    end if

    signR = A[SIGN]*B[SIGN]
    if equal({1}, B[DIGITS]) then   -- power of 10
        res = {signR, A[EXPONENT]-B[EXPONENT], A[DIGITS]}
    else
        A[SIGN] = SG_PLUS
        B[SIGN] = SG_PLUS

        A = expand(A)
        digsA = A[DIGITS]
        for i=1 to length(digsA) do
            partial[DIGITS] &= digsA[i]
            partial[EXPONENT] += 1
            quotient &= 0
            expR += 1
            partial = normalize(partial)
            while compare(partial, B)!=SG_MINUS do
                quotient[$] += 1
                partial = ba_sub(partial, B)
            end while
            partial[SIGN] = SG_PLUS -- if it has zeroed
            partial = expand(partial)
        end for
        res = normalize({signR, expR, quotient})
    end if

    return ba_floor(res)
end function
--


--
-- Returns a bigatom with the result of the division of two numbers.
--
-- Supports Atoms, bigatoms and representation of numbers in a string
--
global function ba_divide(object A, object B, integer bRound = 0)
integer signA, signB
sequence res
integer decsB, ndecs
integer expA, expB
integer mult, len

    if not bigatom(A) then
        A = ba_new(A)
    end if
    if not bigatom(B) then
        B = ba_new(B)
    end if

    signA = A[SIGN]
    signB = B[SIGN]
    if signB=SG_ZERO 
    or signB=SG_NOVALUE
    or signA=SG_NOVALUE then
        return NO_VALUE
    elsif signA=SG_ZERO then
        return BA_ZERO
    end if

    expA = A[EXPONENT]
    expB = B[EXPONENT]
    if equal({1}, B[DIGITS]) then
        A[EXPONENT] -= expB
        A[SIGN] *= signB
        res = A
    else
        ndecs = scale_of(A)
        decsB = scale_of(B)
        if ndecs<decsB then
            ndecs = decsB
        end if

        mult = SCALE+1  -- an extra one to rounding
        if SC_MODE then
            if expA>expB then
                mult += expA-expB
            else
                mult += expB-expA
            end if
        end if
        A[EXPONENT] += ndecs+mult
        B[EXPONENT] += ndecs

        res = ba_idivide(A, B)
        res[EXPONENT] -= mult
    end if

    if SC_MODE and res[EXPONENT]<0 then
        ndecs = SCALE+1
    else
        ndecs = res[EXPONENT]+SCALE+2
    end if
    if ndecs>0 then
        if bRound then
            res = round_digits(res, ndecs)
        else
            len = length(res[DIGITS])
            if len>ndecs then
                res[DIGITS] = remove(res[DIGITS], ndecs, len)
--DEV/sug:
--              res[DIGITS] = res[DIGITS][1..ndecs-1]
            end if
            res = normalize(res)
        end if
    else
        res = BA_ZERO
    end if

    return res
end function
--


-- -------------------------------------------------------------------------------------------


-- #############################################
-- ###   REMAINS, ROUNDINGS, MISCELLANEOUS   ###
-- #############################################

--
-- Returns a bigatom with the rest of the division of two numbers.
--
-- Supports Atoms, bigatoms or strings that represent numbers
--
global function ba_remainder(object A, object B)
sequence res = BA_ZERO
integer cmp

    if not bigatom(A) then
        A = ba_new(A)
    end if
    if not bigatom(B) then
        B = ba_new(B)
    end if

    if A[SIGN]=SG_NOVALUE
    or B[SIGN]=SG_NOVALUE
    or B[SIGN]=SG_ZERO then
        return NO_VALUE
    end if

    cmp = compare(ba_abs(A), ba_abs(B))
    if cmp>0 then
        res = ba_trunc(ba_divide(A, B))
        res = ba_multiply(res, B)
        res = ba_sub(A, res)
    elsif cmp<0 then
        res = A
    end if

    return res
end function
--


--
-- my rounding function adapted to bigatom
-- equal rounds negative numbers positive
--    -0.5 --> -1 instead of -0.5 --> 0
--
global function ba_round(object N, atom precision = 1, integer mode = 0)
sequence res

    if not bigatom(N) then
        N = ba_new(N)
    end if

    if N[SIGN]=SG_NOVALUE then
        return NO_VALUE
    end if

    if not precision then
        precision = 1
    elsif precision<0 then
        precision = -1/precision
    end if

    if mode>0 then
        -- n = ceil(n * precision) / precision
        res = ba_ceil(ba_multiply(N, precision))
    elsif mode<0 then
        -- n = floor(n * precision) / precision
        res = ba_floor(ba_multiply(N, precision))
    else -- mode = 0
        -- n = sign * floor(sign * a * precision + 0.5) / precision
        res = ba_multiply(N, N[SIGN])
        res = ba_multiply(res, precision)
        res = ba_add(res, {SG_PLUS, -1, {5}})   -- 0.5
        res = ba_multiply(ba_floor(res), N[SIGN])
    end if
    res = ba_divide(res, precision, 1)

    return res
end function
--


--
-- returns the absolute value of a bigatom
--
global function ba_abs(bigatom N)
    if N[SIGN]=SG_MINUS then
        N[SIGN] = SG_PLUS
    end if

    return N
end function
--


--
-- returns the integer part of a bigatom
--
global function ba_trunc(bigatom N)
integer start, stop
    if N[SIGN]=SG_NOVALUE then
        return NO_VALUE
    elsif N[EXPONENT]<0 then
        return BA_ZERO
    end if

    start = N[EXPONENT]+2
    stop = length(N[DIGITS])
--PL 19/11/15
--  if stop>start then
    if stop>=start then
        N[DIGITS] = remove(N[DIGITS],start,stop)
    end if

    return normalize(N)
end function
--


--
-- returns the fractional part of a bigatom
--
global function ba_frac(bigatom N)
    if N[SIGN]=SG_NOVALUE then
        return NO_VALUE
    end if

    if N[EXPONENT]<0 then
        return N
    end if
    N[DIGITS] = remove(N[DIGITS], 1, N[EXPONENT]+1)
    N[EXPONENT] = -1

    return normalize(N)
end function
--


--
-- rounded to integer equal to or less immediately
--
global function ba_floor(bigatom N)
sequence I

    if N[SIGN]=SG_NOVALUE then
        return NO_VALUE
    end if

    I = ba_trunc(N)
    if N[SIGN]=SG_MINUS and compare(N, I) then
        I = ba_sub(I, BA_ONE)
    end if

    return I
end function
--


--
-- rounded to the immediate integer equal to or greater
-- (ceil(x) = -floor(-x) )
global function ba_ceil(bigatom N)

    if N[SIGN]=SG_NOVALUE then
        return NO_VALUE
    end if

    N[SIGN] = -N[SIGN]
    N = ba_floor(N)
    N[SIGN] = -N[SIGN]

    return N
end function
--


-- -------------------------------------------------------------------------------------------


-- #################################################
-- ###   EXPONENTIAL AND LOGARITHMIC FUNCTIONS   ###
-- #################################################


--
-- adapted from bigfixedmath.e (Lucius L. Hilley III) Euphoria Archive
-- (more precise than the internal of euphoria and it went better with bigatom)
-- 
constant LIMIT   = 1.1
--
global function b_a_log(atom x)
integer inc
atom res, limit, curr, prev

    if x<=0 then        -- < 0 is not a real, complex (= log(x)+pi*i)
--    return eu:log(x)  -- = 0 is undefined (nan)
        return log(x)   -- = 0 is undefined (nan)
    end if

    if x>LIMIT then
        res = 0
        limit = b_a_log(LIMIT)
        while x>LIMIT do
            x   /= LIMIT
            res += limit
        end while
        res += b_a_log(x)

        return res
    end if

    prev = x
    x   = x-1
    res = x
    curr = x
    inc = 1
    while res!=prev do
        prev    = res
        curr *= x
        inc  += 1
        res  -= curr/inc
        curr *= x
        inc  += 1
        res  += curr/inc
    end while

    return res
end function
--


-- --------------------------------------------------------
global function logb(atom x, atom base = 10)
    if base>0 then
        base = -base
    end if
    base = -floor(base)

--   return log(x) / log(base)
    return b_a_log(x)/b_a_log(base)
end function
--
--------------------------------------------------------------------------------

--
-- calculating the number of Euler (e) and their powers
-- e = 2.718281828459045235360287471352662497757247093699959574
--       9669676277240766303535475945713821785251664274274663919...
-- adapted from the library bc
--
-- override if it has included math.e
--
--override function exp(atom x)
global function b_a_exp(atom x)
integer neg = 0, mult = 0
integer inc
atom xpow, curr
atom fact, res

    if x<0 then
        neg = 1
        x    = -x
    end if

    while x>1 do
        mult += 1
        x     /= 2
    end while

    inc = 2
    xpow = x
    curr = x
    fact = 1
    res = 1+x
    while curr do
        xpow *= x
        fact *= inc
        curr    = xpow/fact
        res  += curr
        inc  += 1
    end while

    while mult do
        res  *= res
        mult -= 1
    end while

    if neg then
        res = 1/res
    end if

    return res
end function
--


--
-- square root
--
--override function sqrt(atom x)
global function b_a_sqrt(atom x)
atom res, res1 = 0

    if x < 0 then
--      return eu:sqrt(x)   -- is not a real, imaginary (= sqrt(-x)i)
        return sqrt(x)      -- is not a real, imaginary (= sqrt(-x)i)
    elsif x = 0 then
        return 0
    elsif x = 1 then
        return 1
    end if

    if x < 1 then
        res = 1
    else
--      res1 = -floor(-eu:log(x) * 0.5)
        res1 = -floor(-log(x) * 0.5)
        res  = power(10, res1)
    end if

    while res != res1 do
        res1 = res
        res  = x / res
        res += res1
        res *= 0.5
    end while

    return res
end function

-- -------------------------------------------------------------------------------------------
--
-- -------------------------------------------------------------------------------------------



--
constant BLIMIT = {1,0,{1,1}}   -- 1.1
--
-- Return the natural logarithm bigatom
--
-- Supports Atoms, bigatoms and representation of numbers in a string
--
-- adapted from bigfixedmath.e (Lucius L. Hilley III) euphoria file
-- (the one that has best taken with bigatoms)
--
global function ba_log(object x, integer bRound = 0)
sequence sc = scale(SCALE+4, 0)
--sequence limit, ln_limit
sequence ln_limit
sequence res, curr, prev, inc

integer start,stop

    if not bigatom(x) then
        x = ba_new(x)
    end if

    if x[SIGN]<SG_PLUS then
        return NO_VALUE
    end if

    if compare(BA_ONE, x)=SG_PLUS then
        res = ba_log(ba_divide(BA_ONE, x))
        res[SIGN] = -res[SIGN]
    elsif compare(x, BLIMIT)=SG_PLUS then
        res    = BA_ZERO
        ln_limit = ba_log(BLIMIT)
        while compare(x, BLIMIT)=SG_PLUS do
            x    = ba_divide(x, BLIMIT)
            res = ba_add(res, ln_limit)
        end while
        res = ba_add(res, ba_log(x))
    else
      -- ln(x) = x - x^2/2 + x^3/3 - x^4/4 + x^5/5 - ...
        prev = x
        x    = ba_sub(x, BA_ONE)
        curr = x
        res  = x
        inc  = {1, 0, {2}}  -- 2
        while compare(prev, res) do
            prev = res
            curr = ba_multiply(curr, x)
            res  = ba_sub(res, ba_divide(curr, inc))
            inc  = ba_add(inc, BA_ONE)
            curr = ba_multiply(curr, x)
            res  = ba_add(res, ba_divide(curr, inc))
            inc  = ba_add(inc, BA_ONE)
        end while
    end if

    {} = scale(sc)  -- (restore original settings)

    if bRound then
        res = round_digits(res, res[EXPONENT]+2+SCALE)
    else
        start = res[EXPONENT]+2+SCALE
        stop = length(res[DIGITS])
        if stop>start then
            res[DIGITS] = remove(res[DIGITS],start,stop)
        end if
    end if

    return normalize(res)
end function
--


--
-- Returns a bigatom with the power of e (e^x)
-- e = 2.71828182845904523536028747135266249775724709369995
--       95749669676277240766303535475945713821785251664274...
-- ba_exp(1) = e
--
-- Supports Atoms, bigatoms and representation of numbers in a string
--
-- adapted from the library bc
--
global function ba_exp(object x, integer bRound = 0)
integer neg = 0, mult = 0
--sequence sc, limit, inc = {1, 0, {2}}
sequence sc, inc = {1, 0, {2}}
sequence xpow, curr, res, fact

integer start,stop

    if not bigatom(x) then
        x = ba_new(x)
    end if

    if equal(x, BA_ONE) then
        return euler(SCALE, 1)  -- much faster
    end if

    if x[SIGN]=SG_MINUS then
        neg = 1
        x[SIGN] = -x[SIGN]
    end if

    sc = scale(SCALE+2, 0)

    while compare(x, BA_ONE)=SG_PLUS do
        mult += 1
        x = ba_divide(x, {1, 0, {2}})
    end while

    curr = x
    xpow = x
    res = ba_add(x, BA_ONE)
    fact = BA_ONE
    while curr[SIGN] do
        xpow = ba_multiply(xpow, x)
        fact = ba_multiply(fact, inc)
        curr = ba_divide(xpow, fact)
        res  = ba_add(res, curr)
        inc  = ba_add(inc, BA_ONE)
    end while

    while mult do
        res = ba_multiply(res, res)
        mult -= 1
    end while
    if neg then
        res = ba_divide(BA_ONE, res)
    end if

    {} = scale(sc)  -- (restore original settings)

    if bRound then
        res = round_digits(res, res[EXPONENT]+2+SCALE)
    else
        start = res[EXPONENT]+2+SCALE
        stop = length(res[DIGITS])
        if stop>start then
            res[DIGITS] = remove(res[DIGITS],start,stop)
        end if
    end if

    return normalize(res)
end function
--


--
-- Returns a bigatom with the power of x (x^exp)
--
-- Supports Atoms, bigatoms and representation of numbers in a string
--
global function ba_power(object x, object exponent, integer bRound = 0)
sequence res

    if not bigatom(x) then
        if atom(x) and atom(exponent) then
            if floor(exponent)=exponent then
                res = intf_power(ba_new(x), exponent)
            elsif exponent>0 then
                res = ba_exp(ba_multiply(ba_log(x), exponent), bRound)
            elsif exponent then
                res = ba_divide(BA_ONE, ba_exp(ba_multiply(ba_log(x), -exponent), bRound))
            else
                res = BA_ONE
            end if

            return res
        end if
        x = ba_new(x)
    end if

    if atom(exponent) and floor(exponent)=exponent then
        return intf_power(x, exponent)
    end if

    if not bigatom(exponent) then
        exponent = ba_new(exponent)
    end if

    if equal(ba_floor(exponent), exponent) then
        res = intf_power(x, bigatom_to_atom(exponent))
    elsif exponent[SIGN]=SG_PLUS then
        res = ba_exp(ba_multiply(ba_log(x), exponent), bRound)
    elsif exponent[SIGN]=SG_MINUS then
        exponent[SIGN] = -exponent[SIGN]
        res = ba_divide(BA_ONE, ba_exp(ba_multiply(ba_log(x), exponent)), bRound)
    else
        res = BA_ONE
    end if

    return res
end function
--


--
-- Returns a bigatom with the square root of x
--
-- Supports Atoms, bigatoms and representation of numbers in a string
--
global function ba_sqrt(object x, integer bRound = 0)
sequence res, res1 = BA_ZERO
integer cmp
integer start,stop

    if not bigatom(x) then
        x = ba_new(x)
    end if

    if x[SIGN]<0 then
        -- is not a real, imaginary (= sqrt(-x)*i)
        return NO_VALUE
    elsif x[SIGN]=SG_ZERO then
        return BA_ZERO
    end if

    cmp = compare(x, BA_ONE)
    if not cmp then
        return BA_ONE
    elsif cmp<1 then
        -- if 0 < x < 1, initially 1
        res = BA_ONE
    else
        -- if x > 1 start in 10^(exp/2) = 10^(exp*0.5)
        res1 = ba_floor(ba_multiply(x[EXPONENT]+1, {1,-1,{5}}))
        res  = ba_power({1,1,{1}}, res1)
    end if

    while compare(res, res1) do
        res1 = res
        res  = ba_divide(x, res)
        res  = ba_add(res, res1)
        res  = ba_multiply(res, {1,-1,{5}}) -- 0.5
    end while

    if bRound then
        res = round_digits(res, res[EXPONENT]+2+SCALE)
    else
        start = res[EXPONENT]+2+SCALE
        stop = length(res[DIGITS])
        if stop>start then
            res[DIGITS] = remove(res[DIGITS],start, stop)
        end if
    end if

    return normalize(res)
end function
--


--
-- Returns a bigatom with root n x (x^(1/n))
--
-- Supports Atoms, bigatoms and representation of numbers in a string
--
global function ba_root(object x, object exponent, integer bRound = 0)
sequence res
sequence sc = scale(SCALE+1)

integer start,stop

    if not bigatom(x) then
        if atom(x) and atom(exponent) then
            if x<0 or not exponent then
                res =  NO_VALUE
            elsif exponent>0 then
                if exponent=2 then
                    res = ba_sqrt(x)  -- very common and much faster
                else
                    res = ba_power(x, 1/exponent)
                end if
            else
                res = ba_divide(BA_ONE, ba_power(x, 1/ -exponent), bRound)
            end if

            {} = scale(sc)  -- (restore original settings)

            start = res[EXPONENT]+2+SCALE
            stop = length(res[DIGITS])
            if stop>start then
                res[DIGITS] = remove(res[DIGITS],start,stop)
            end if

            return normalize(res)
        end if
        x = ba_new(x)
    end if

    if not bigatom(exponent) then
        if atom(exponent) and exponent=2 then
            res = ba_sqrt(x)

            {} = scale(sc)  -- (restore original settings)

            if bRound then
                res = round_digits(res, res[EXPONENT]+2+SCALE)
            else
                start = res[EXPONENT]+2+SCALE
                stop = length(res[DIGITS])
                if stop>start then
                    res[DIGITS] = remove(res[DIGITS],start,stop)
                end if
                res = normalize(res)
            end if

            return res
        end if
        exponent = ba_new(exponent)
    end if

    if x[SIGN]!=SG_PLUS
    or exponent[SIGN]=SG_NOVALUE
    or not exponent[SIGN] then
        res = NO_VALUE
    elsif exponent[SIGN]=SG_PLUS then
        res = ba_power(x, ba_divide(BA_ONE, exponent))
    else
        exponent[SIGN] = -exponent[SIGN]
        res = ba_divide(BA_ONE, ba_power(x, ba_divide(BA_ONE, exponent)))
    end if

    {} = scale(sc)  -- (restore original settings)

    if bRound then
        res = round_digits(res, res[EXPONENT]+2+SCALE)
    else
        start = res[EXPONENT]+2+SCALE
        stop = length(res[DIGITS])
        if stop>start then
            res[DIGITS] = remove(res[DIGITS],start,stop)
        end if
    end if

    return normalize(res)
end function
--


-- with large numbers shortcut
-- log10(x * y) = log10(x) + log10(y)
-- 123456789 = 1.23456789 * 10^8 = {1, 8, {1,2,3,4,5,6,7,8,9}}
-- log10(123456789) = 8 + 1 + log(0.123456789)
-- if x or y is a power of ten, its logarithm is the exponent and
-- just add it to the logarithm of the other number with exponent 0
-- that's not a big number and calculates faster ...
-- ... the logarithm of a small number and a sum
-- I'm surprised not to have fallen before, so simple and so effective.
--
global function ba_log10(object x, integer bRound = 0)
integer exponent
sequence res

integer start, stop

    if not bigatom(x) then
        x = ba_new(x)
    end if

    exponent = x[EXPONENT]+1
    x[EXPONENT] = -1
    res = ba_logb(x, 10, bRound)
    res = ba_add(res, exponent)          -- the sum ignores the scale
    if bRound then
        res = round_digits(res, res[EXPONENT]+2+SCALE)
    else
        start = res[EXPONENT]+2+SCALE
        stop = length(res[DIGITS])
        if stop>start then
            res[DIGITS] = remove(res[DIGITS],start,stop)
        end if
    end if

    return normalize(res)
end function
--


--/*
global function ba_log10_2(object x, integer bRound = 0)
integer exponent
sequence res
    if not bigatom(x) then
        x = ba_new(x)
    end if

    exponent = x[EXPONENT] 
    x[EXPONENT] = 0
    res = ba_logb(x, 10, round)

    return ba_add(res, exponent)
end function
--
--*/

--
-- Returns a bigatom with logarithm b of x
--
-- Supports Atoms, bigatoms and representation of numbers in a string
--
global function ba_logb(object x, object base = 10, integer bRound = 0)
    if not bigatom(x) then
        x = ba_new(x)
    end if
    if not bigatom(base) then
        base = ba_new(base)
    end if
    base[SIGN] = SG_PLUS
    base = ba_ceil(base)

    return ba_divide(ba_log(x), ba_log(base), bRound)
end function
--


-- -------------------------------------------------------------------------------------------




-- #####################################################
-- ###   BIGATOM CONVERSION AND HANDLING FUNCTIONS   ###
-- #####################################################


--
-- Returns a string representation of the formatted number.
--
-- sign:  if nonzero the sign also shows positive numbers.
--        if zero the sign is only displayed in negative numbers.
--
-- decs:  is the number of decimal places displayed (rounded to the last decimal).
--        If negative number shown with all decimals.
--        If not, is filled with zeros or other characters until the total number of
--        decimal requested, or trimmed and rounded to the last decimal.
--
-- zsign: sign put to zero when the number is less than the limits of representation
--        but its value is not zero in the current scale.
--
-- all:   indicates whether to show all the numbers in decimal. Not shown if it is zero.
--        If one, adds the decimal point and zeros up to the number of decimal places, 
--        and if greater than one the DFCHAR character is used instead of zeros.
--
function to_string(sequence N, integer sign = 0, integer decs = -1, integer zsign = 0, integer all = 0)
integer sg, exponent
sequence digits, dcopy
integer decsN
integer last
integer dp

    if N[SIGN]=SG_NOVALUE then
        return "<no_value>"
    end if

    N = expand(N)

    {sg, exponent, digits} = N
    
    -- set the number of decimal requested
    decsN = length(digits)-exponent-1
    if decs<0 then
        decs = decsN
    end if

    last = exponent+1+decs
    if decs<decsN then
        if digits[last+1]>=5 then
         -- rounds the last decimal
            digits = digits_add(digits[1..last], {1})
            exponent += digits[1]
            digits = digits[2]
        else
            digits = digits[1..last]
        end if
    elsif all or decsN then
        if all=1 then
            digits &= repeat(0, decs-decsN)
        elsif all then
            digits &= repeat(DFCHAR-'0', decs-decsN)
        end if
    end if

    if compare(digits, repeat(0, length(digits)-exponent)) then
        zsign = 0
    end if

--  digits += '0'
--  digits = sq_add(digits,'0')
    dcopy = digits
    digits = repeat(' ',length(digits))     -- make string
    for i=1 to length(digits) do
        digits[i] = dcopy[i]+'0'
    end for

    -- put the decimal point
    dp = exponent+2
    if dp<=length(digits) then
        digits = insert(digits, '.', dp)
    end if

    -- put sign
    if sg=SG_MINUS then
        digits = prepend(digits, SMINUS)
    elsif sg and (sign or zsign) then
        digits = prepend(digits, SPLUS)
    else   --  if sign then
        digits = prepend(digits, SZERO)
    end if

    return digits
end function
--


--
-- Returns an atom with the value of a bigatom
--
--/*
global function bigatom_to_atom(bigatom N)
atom val = 0
integer sg = N[SIGN]
sequence digits = N[DIGITS]
atom div = 10

    if length(digits) then
        val = digits[1]
        for i=2 to length(digits) do
            val += digits[i]/div
            div *= 10
        end for
        val *= sg*power(10, N[EXPONENT])
    end if
    return val
end function
--*/
global function bigatom_to_atom(bigatom N)
atom val = 0
integer sign = N[SIGN]
integer exponent = N[EXPONENT]
sequence digits = N[DIGITS]
atom div = 10

    if length(digits) then
        val = digits[1]
        for i=2 to length(digits) do
            if exponent>1 then
                val = val*10+digits[i]
                exponent -= 1
            else
                for j=i to length(digits) do
                    val += digits[j]/div
                    div *= 10
                end for
                exit
            end if
        end for
        val *= sign*power(10, exponent)
    end if
    return val
end function
--


--
-- Returns a bigatom with the value of an integer (integer)
--
function int_to_bigatom(integer i)
integer sign = SG_PLUS
sequence s
    if i<0 then
        sign = SG_MINUS
    end if
    s = sprintf("%+d", i)
    s[1] = -1   -- (force expansion to dword-sequence)
    s = s[2..$]
    s = sq_sub(s,'0')

    return normalize({sign, length(s)-1, s})
end function
--


--
-- Returns an atom with the integer value of a string containing the
-- representation of a number.
--
-- The conversion stops to find the first invalid character.
-- Allows the use of spaces and underscores as separators groups.
-- If the string contains a number with decimal value returns
-- the integer part, point or comma stop the conversion.
--
--global 
function int_value(sequence str)
integer digit
integer sign
atom ival = 0

    if not length(str) then
        return 0
    end if

    sign = str[1]
    sign = (sign=SPLUS)-(sign=SMINUS)
    if sign then
        str = str[2..$]
    else
        sign = 1
    end if

    for i=1 to length(str) do
        if not integer(str[i]) then
            exit
        end if
        digit = str[i]
        if digit>='0' and digit<='9' then
            ival = ival*10+(digit-'0')
        elsif digit!=UNDERLINE and digit!=SPACE then
            exit
        end if
    end for

    return sign*ival
end function
--


--
-- Standardizes a bigatom.
-- Reduces your bigatom to a shorter form, eliminating superfluous zeroes and adjusting the exponent.
--
function normalize(sequence N)
sequence mantissa
integer first, last

    if N[SIGN]<SG_MINUS or N[SIGN]>SG_PLUS then
        return NO_VALUE
    end if

    mantissa = N[DIGITS]
    first = 1
    last = length(mantissa)
    while last and mantissa[last]=0 do
        last -= 1
    end while
    while first<=last and mantissa[first]=0 do
        first += 1
    end while

    if first>last or not N[SIGN] then
        N = BA_ZERO
    else
        N[DIGITS]  = mantissa[first..last]
        N[EXPONENT] -= first-1
    end if

    return N
end function
--


--
-- expand a bigatom with all its digits
--
function expand(sequence N)
sequence digits = N[DIGITS]
integer exponent = N[EXPONENT]
integer len = length(digits)-1

    if exponent<0 then
        digits = repeat(0, -exponent) & digits
        N[EXPONENT] = 0
    elsif exponent>len then
        digits &= repeat(0, exponent-len)
    end if
    N[DIGITS] = digits

    return N
end function
--


--
-- returns both numbers with sequences of aligned and matched digits
-- in size by adding zeros as both right and left
-- the digits of the two numbers.
-- Equating the exponents and the same number of digits in both mantissas.
--
function align(sequence A, sequence B)
integer expA, expB, offset
sequence digsA, digsB
integer last

    {?, expA, digsA} = A
    {?, expB, digsB} = B

    -- put zeros to the left
    offset = expA-expB
    if offset>0 then
        digsB = repeat(0, offset) & digsB
        expB += offset
    else
        digsA = repeat(0, -offset) & digsA
        expA -= offset
    end if

    -- put zeros to the right
    offset = length(digsA)-length(digsB)
    if offset>0 then
        digsB &= repeat(0, offset)
    else
        digsA &= repeat(0, -offset)
    end if

    -- delete extra zeros
    last = length(digsA)
    while last and digsA[last]=0 and digsB[last]=0 do
        last -= 1
    end while
    A[DIGITS] = digsA[1..last]
    B[DIGITS] = digsB[1..last]
    A[EXPONENT] = expA
    B[EXPONENT] = expB

    return {A, B}
end function
--


--
-- adds two sequences of digits
-- returns a sequence of two elements: {carry, result}
-- if carry is not zero it indicates the length of the result is increased by 1.
--
function digits_add(sequence a, sequence b)
integer len = length(a), lenb = length(b)
sequence result
integer carry = 0, digit

    if len<lenb then
        a = repeat(0, lenb-len) & a
        len = lenb
    elsif lenb<len then
        b = repeat(0, len-lenb) & b
    end if

    result = sq_add(a,b)

    for i=len to 1 by -1 do
        digit = result[i]+carry
        carry = digit>9
        if carry then
            digit -= 10
        end if
        result[i] = digit
    end for
    if carry then
        result = prepend(result, carry)
    end if

    return {carry, result}
end function
--


--
-- subtracts two sequences of digits
-- returns a sequence of two elements: {negative, result}
-- if negative is not zero it indicates that the result is negative.
--
function digits_sub(sequence a, sequence b)
integer len = length(a), lenb = length(b)
sequence result
integer digit, neg = 0

    if len<lenb then
        a = repeat(0, lenb-len) & a
        len = lenb
    elsif lenb<len then
        b = repeat(0, len-lenb) & b
    end if

    result = sq_sub(a,b)

    for i=len to 1 by -1 do
        digit = result[i]-neg
        neg = digit<0
        if neg then
            digit += 10
        end if
        result[i] = digit
    end for
    -- if negative complement is 10
    if neg then
        result = sq_sub(9,result)      -- supplement 9
        result = digits_add(result, {1}) -- plus 1
        result = result[2]
    end if

    return {neg, result}
end function
--


--
-- multiplies two sequences of digits
-- returns a sequence of two elements: {carry, result}
-- if carry is not zero it indicates the result length is increased by 1.
--
function digits_multiply(sequence a, sequence b)
integer lena = length(a), lenb = length(b)
sequence partial, result = {}
integer digit, carry

    if lena<lenb then
        {a, lena, b, lenb} = {b, lenb, a, lena}
    end if

    for i=lenb to 1 by -1 do
        carry = 0
        partial = sq_mul(a,b[i]) & repeat(0, lenb-i)
        for j=lena to 1 by -1 do
            digit = partial[j]+carry
            carry = 0
            if digit>9 then
                carry = floor(digit/10)
                digit = remainder(digit, 10)
            end if
            partial[j] = digit
        end for
        if carry then
            partial = prepend(partial, carry)
        end if
        result  = digits_add(result, partial)
        carry  += result[1]
        result  = result[2]
    end for

    return {carry, result}
end function
--


--
-- rounds the dignum digit of the mantissa of a bigatom
-- returns a number rounded bigatom with a digit less.
--
function round_digits(sequence N, integer dignum)
sequence res = N[DIGITS]

    if dignum<1 or dignum>length(res) then
        return N
    end if

    res = digits_add(res[1..dignum], {5})
    N[EXPONENT] += res[1]
    N[DIGITS] = res[2][1..$-1]

    return normalize(N)
end function
--


--
-- raise bigatom to an integer power (one to one)
--
function ipower(sequence A, integer exponent)
sequence res
    if exponent=0 then
        return BA_ONE
    else
        res = A
        for i=2 to exponent do
            res = ba_multiply(A, res)
        end for
    end if

    return res
end function
--


--
-- raise bigatom to an integer power (down into prime factors)
-- sure there are better ways to do it, but at least gives the result
-- exact integer exponents
--
function intf_power(sequence A, integer exponent)
sequence res
sequence factrs

    if exponent=0 then
        return  BA_ONE -- for convenience 0^0 = 1 (the neutral term multiplication)
    end if

    if A[SIGN]=SG_ZERO then
        if exponent<0 then
            res = NO_VALUE
        else
            res = BA_ZERO
        end if
    elsif exponent<0 then
        res = ba_divide(BA_ONE, intf_power(A, -exponent))
        if res[SIGN] then
            res[SIGN] = A[SIGN]
        end if
    elsif equal({1}, A[DIGITS]) then
        res = A
        res[EXPONENT] *= exponent
    else
        factrs = get_factors(exponent)
        res = A
        for i=1 to length(factrs) do
            res = ipower(res, factrs[i])
        end for
    end if

    return res
end function
--


--
-- decompose an integer into its prime factors
--
function get_factors(integer n)
sequence flist = {}
integer factor = 3

    while not remainder(n, 2) do
        flist &= 2
        n /= 2
    end while

    while n>=factor do
        while remainder(n, factor) do
            factor += 2
        end while
        flist &= factor
        n /= factor
    end while
    if not length(flist) then
        flist = {n}
    end if

    return flist
end function
--


-- -----------------------------------------------
-- adapted literally an example of XDS,
-- Modula and Oberon compiler for OS/2
-- -----------------------------------------------
--
-- Calculate the number of Euler (e) with arbitrary precision
-- proven up to one million decimals, which looked on the internet and only a diff
-- the last decimal differed, although it takes a bit different ... lol will smoke ...
-- according to the clear machine.
--
--    decs:   e number of decimal places (>=0)
--    output: if zero returns a string if
--            other than zero, a bigatom
--
--  Algorithm originally written in Algol by Serge Batalov
--  Rewritten in Modula-2 and modified by Eugene Nalimov and Pavel Zemtsov
--  Adapted Euphoria by Carlos J. Gómez Andreu (cargoan)
--
--  (I finally understood something and I put names to variables)?
-- 

--ifdef BITS64 then
--   constant N  = 12,                 -- exponent base (multiple of 4)
--          BASE = 1_000_000_000_000   -- base (10^N)
--elsedef
--   constant N = 8,                 -- (32bit)?  (down to 4 if type error)
--          BASE = 100_000_000   -- base (10^N)
--end ifdef

--DEV:
--integer N,BASE
integer N
atom BASE
    if machine_bits()=64 then
        N    = 12                   -- exponent base (multiple of 4)
        BASE = 1_000_000_000_000    -- base (10^N)
    else
--      N    = 8                    -- (32bit)?  (down to 4 if type error)
        N    = 4                    -- (32bit)?  (down to 4 if type error)
--      BASE = 100_000_000          -- base (10^N)
        BASE = 10_000               -- base (10^N)
    end if

--constant BASE = power(10, N)   -- wrong atom (that is slow)
--
global function euler(integer decs, integer output = 0)
integer d
integer size
sequence bdigits
sequence carries
sequence res
integer c = 1, bdigit = 0
integer carry = 0
sequence sdigit
integer dsize

    if decs<0 then
        decs = 0
    end if
    d = decs
    if remainder(decs, N) then     -- decimal multiple of N
        d += N-remainder(decs, N)
    end if

    size = floor(d/N)+N/4
    bdigits = repeat(0, size)    -- "digits" in base 10^N
    carries = repeat(0, size)    -- carries
    res = {}

    if d>0 then
        carries[1] = BASE
        for i=1 to size do
            while 1 do
                c += 1
                for j=i to size do
                    bdigit = bdigit*BASE+carries[j]
                    carries[j]  = floor(bdigit/c)
                    bdigits[j] += carries[j]
                    bdigit -= c*carries[j]
                end for
                if carries[i]<c then
                    exit
                end if
                bdigit = 0
            end while
            bdigit = carries[i]
        end for

        for i=size to 1 by -1 do
            bdigit = bdigits[i]+carry
            carry   = floor(bdigit/BASE)
            bdigits[i] = bdigit-carry*BASE
        end for
-- -----------------------------------------------------------
--  here x is an array of decimal 'e' in base 10^N
--  each "digit" (element) are N digits in base 10
-- -----------------------------------------------------------
      -- convert base 10 and save
        for i=1 to size do
            sdigit = repeat(0, N)
            bdigit = bdigits[i]
            dsize = N
            while dsize do
                sdigit[dsize] = remainder(bdigit, 10)
                bdigit = floor(bdigit/10)
                dsize -= 1
            end while
            res &= sdigit
        end for
    end if

    if output then
        res = {1, 0, 2 & res[1..decs]}
    elsif decs then
--      res = "  2." & res[1..decs]+'0'
        bdigits = res
        res = "  2."
        for i=1 to decs do
            res &= bdigits[i]+'0'
        end for
    else
        res = "2"
    end if

    return res
end function
--

-- -------------------------------------------------------------------------------------------
-- end bigatom.e
-- -------------------------------------------------------------------------------------------


/*
--scale(20)

sequence s = euler(100000)
for i = 1 to length(s)-2 by 50 do
    puts(1, s[i..i+49]&10)
end for

*/

atom t0, t1
integer n = 2, decs = 100
bigatom ba
sequence s
integer f
integer count

--ifdef TEST then
if 0 then
    puts(1, "\nDid you know...\n\n")
    {} = scale(decs)
    ba = ba_sqrt(n)
--?ba
    printf(1, "The square root of %d with %d decimals is:\n\t%s\n\n",
          {n, decs, ba_sprintf("%.100aB", ba)})
--0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
    ba = ba_root(n, 3)
    ba_printf(1, "And the cube root is:\n\t%.100aB\n\n", ba)
    ba = ba_log(n)
    ba_printf(1, "Its natural logarithm is:\n\t%.100aB\n\n", ba)
    ba = ba_logb(n, 10)
    ba_printf(1, "And its logarithm is:\n\t%.100aB\n\n", ba)
    decs = 843
    {} = scale(decs)
    ba = ba_exp("1")
    printf(1, "And that 'e' with %d decimals is:\n\t%s\n", {decs, ba_sprint(ba)})
--end ifdef

--ifdef TEST2 then
    {} = scale(100, 1)
    t0 = time()
    -- exp = -1 and cut
    ba_printf(1,"\nba_log10('9999999999999999'):\n\t%B\n", ba_log10("9999999999999999"))
    t1 = time()
    printf(1, "in: %.3f sec.\n\n", t1-t0)
    -- exp = 0 
--commented out above...
--   ba_printf(1,"ba_log10_2('9999999999999999'):\n\t%B\n", ba_log10_2("9999999999999999"))
--   printf(1, "in: %.3f sec.\n", time() - t1) 
end if
--end ifdef


--ifdef TEST3 then
if 0 then
    -- 10,000 decimals took 0.296s,
    -- 20,000 decimals took 1.061s,
    -- 50,000 decimals took 6.053s,
    -- 100,000 decimals took 22.932s,
    -- 200,000 decimals took 88.780s,
    -- I gave up waiting for 1 million, but the above suggests it would be at least 10mins...
    -- I checked the 200,000 digits and as above the last was wrong, so it looks like you
    --  may need to ask for n+1 and throw one away, or have the routine in here do that 
    --  for you automatically, although it seems fine (truncated not rounded) up to 100.
    decs = 1_000_000
--  decs = 1_000_000
    t0 = time()
    s = euler(decs)
    printf(1, "'e' with %d decimals in: %.3f sec.\n", {decs, time()-t0})
    f = open("e-1millon.txt", "w")
    count = 0
    for i=1 to length(s) do
        puts(f, s[i])
        count += 1
        if count=80 then
            puts(f,'\n')
            count = 0
        end if
    end for
    puts(f, 10)
    close(f)
end if
--end ifdef

