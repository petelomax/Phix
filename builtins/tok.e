global procedure tokinit()
--  initFilePathSet()

    allpfiles = {}

    line = 1
    col = 0
    ltl = -1
    tokline = 0
    tokcol = 0
    
    mapEndToMinusOne = 0

end procedure

global function getIncludeLine()
string res
    tokcol = col+1
    for i=col+1 to ltl do
        if text[i]='\n' then
            res = text[col..i]
            col = i--+1
            tokcol = i
--          line += 1
            return res
        end if
    end for
end function

global integer Tok9
global atom TokN

global procedure getCh()
-- this code inlined where possible.
-- called by main/include to initialise.
    col += 1
    if col>ltl then
        eof_processing() -- (sets Ch to -1 and clears activepaths)
        return
    end if
    if Ch='\n' then
        line += 1
    end if
    Ch = text[col]
end procedure

global function ChNext(integer ch)
-- used for named parameters, to check whether a Ch of ':' is part of a ":=".
    return text[col+1]=ch
end function

integer ch2
integer cp1, cp2, cp3

--with trace
procedure SkipBlockComment()
-- Skip nested /* block comments */
-- Note that "*/" inside a string is interpreted as end of comment, 
--  (since it is technically text not code, and for example we must
--   treat '/* if x="*/" then' as '" then'), though a "/*" and "*/" 
--   pair (of strings/within a string) behave as expected.
-- The opening /* has already been processed; if we cannot find a 
-- matching */ before EOF then display an error.
integer oline, ocol
--trace(1)
    ocol = col
    oline = line
-- if line!=0 then ?9/0 end if  -- we must maintain this!
    while 1 do
        col += 1
        if col>=ltl then exit end if -- Triggers error
        if Ch='\n' then
            line += 1
        end if
        Ch = text[col]
        if Ch='*' then
            cp1 = col+1
            ch2 = text[cp1]
            if ch2='/' then
                col += 2
                Ch = text[col]
                return
            end if
        elsif Ch='/' then
            cp1 = col+1
            ch2 = text[cp1]
            if ch2='*' then
                col = cp1
                SkipBlockComment()
                col -= 1
            end if
        end if
    end while
    tokline = oline
    tokcol = ocol
    Abort("missing closing block comment")
end procedure

global constant
    EOL     = 1,    -- End of line
    SPACE   = 2,    -- Spaces & tabs
    SYMBOL  = 3,    -- General symbols !&*+,./<>=?
    HEXDEC  = 4,    -- Hexadecimal (#) mark
--  ILASM   = 5,    -- #ilasm statement
--  TYPEIS = 6,     -- #type_is construct
    BRACES  = 5,    -- ()[]{}
    ELLIPSE = 6,    -- '..'
    SQUOTE  = 7,    -- Single quotation mark
    DQUOTE  = 8,    -- Double quotation mark
    BKTICK  = 9,    -- Back tick (string with no escape characters)
    ILLEGAL = 10,   -- illegal character
    FLOAT   = 11,   -- float, eg 1.0 or 1e4
    DIGIT   = 12,   -- 0..9
--  USCORE  = 12,   -- _
    LETTER  = 13,   -- A..Z,a..z
    HEXSTR  = 14,   -- Hexadecimal Byte String
    DIGIBAD = -1    -- partials, eg "-."; "3.0e"

--string charset, identset, baseset, whiteacl   -- identset now in ptree.e
string charset, baseset
--, whiteacl
    charset = repeat(ILLEGAL,256)
    charset['\n'] = EOL
    charset['\r'] = EOL
    charset['\t'] = SPACE
    charset[' ']  = SPACE
    charset['!']  = SYMBOL
    charset['\"'] = DQUOTE
    charset['`']  = BKTICK
    charset['#']  = HEXDEC
    charset['&']  = SYMBOL
    charset['\''] = SQUOTE
    charset['('..')'] = BRACES  -- () only
    charset['*'..'/'] = SYMBOL  -- *+,-./
    charset['0'..'9'] = DIGIT
    charset[':'..'?'] = SYMBOL  -- :;<=>?
    charset['A'..'Z'] = LETTER
    charset[#80] = LETTER   -- more unicode
    charset[#88] = LETTER   -- more unicode
    charset[#94] = LETTER   -- for rosettacode/unicode (as ptok.e is not stored in utf8)
    charset[#9A] = LETTER   -- for rosettacode/unicode
    charset[#A3] = LETTER   -- for rosettacode/unicode
    charset[#BB] = LETTER   -- for rosettacode/unicode
    charset[#CE] = LETTER   -- for rosettacode/unicode
    charset[#CF] = LETTER
    charset[#E2] = LETTER
--  charset['_'] = ILLEGAL  -- Specifically checked for after 1st character of LETTER
--  charset['_'] = USCORE
--12/11/15:
    charset['_'] = LETTER
    charset['['] = BRACES
    charset[']'] = BRACES
    charset['a'..'z'] = LETTER
    charset['{'] = BRACES
    charset['}'] = BRACES
if ORAC then
    charset['~']  = SYMBOL
end if

global procedure SpecialHandling(integer char, integer chartype)
    charset[char] = chartype
end procedure
-- now in pttree.e:
--  identset = repeat(' ',256)  -- characters valid in an identifier
--  -- convert to "string of nulls" (repeat(0,256) makes a dword-sequence)
--  for i=1 to 256 do identset[i] = 0 end for
--  identset['0'..'9'] = 1
--  identset['A'..'Z'] = 1
--  identset['_'] = 1
--  identset['a'..'z'] = 1

--  whiteacl = repeat(' ',256)  -- whitespace and comment lead-ins
--  -- convert to "string of nulls" (repeat(0,256) makes a sequence)
--  for i=1 to 256 do whiteacl[i] = 0 end for
--  whiteacl[' '] = 1
--  whiteacl['\t'] = 1
--  whiteacl['\r'] = 1
--  whiteacl['\n'] = 1
--  whiteacl['-'] = 2
--  whiteacl['/'] = 3

    baseset = repeat(255,256)
    for i=0 to 9 do
        baseset['0'+i] = i
    end for
    for i=10 to 35 do
        baseset['A'+i-10] = i
        baseset['a'+i-10] = i
    end for

--DEV constants can be compiled better??
-- Subscripting a literal constant can be optimised better by the compiler, 
--  since it has a fixed base address (as opposed to a calculate-once constant).
-- The gain is small, maybe 3%[???], so don't worry about using the hll code below when experimenting.
--constant charset = {13,13,13,13,13,13,13,13, 1, 2,13,13, 2,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13, 1,
--                   3, 4, 5,13,13, 3, 6, 7, 7, 3, 3, 3, 3, 3, 3, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 3, 3, 3, 3, 3, 3,13,
--                  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10, 7,13, 7,13,13,13,
--                  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10, 7,13, 7,13,13,13,
--                  13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,
--                  13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,
--                  13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,
--                  13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13},
--      identset = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,    -- 0..9
--                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,0,    -- A..Z,_
--                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,    -- a..z
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
--      baseset = {255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  0,     -- '0'
--                   1,  2,  3,  4,  5,  6,  7,  8,  9,255,255,255,255,255,255,255,     -- '1'..'9'
--                  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,     -- 'A'..'Z'
--                  26, 27, 28, 29, 30, 31, 32, 33, 34, 35,255,255,255,255,255,255,
--                  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,     -- 'a'..'z'
--                  26, 27, 28, 29, 30, 31, 32, 33, 34, 35,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255}
--global constant whiteacl = {
--                  0,0,0,0,0,0,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,    -- \t, \n, \r, ' '
--                  0,0,0,0,0,0,0,0,0,0,0,0,2,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    -- '-', '/'
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}


--with trace
global procedure skipSpacesAndComments()
--global procedure skipSpaces()
--NB: idx out of bounds here (Ch=-1) should be fixed by testing
--    for that before the call, rather than here.
--    if Ch is 0, it should be fixed in loadFile [?].
--integer k
--integer t -- now global chartype
--  while whiteacl[Ch] do   -- whitespace and comment lead-ins
    while 1 do
--      k = whiteacl[Ch]
        chartype = charset[Ch]
        if chartype>SYMBOL then exit end if
--      if not k then exit end if
--      if k=1 then         -- Ch in " \t\r\n"
--      if Ch<=' ' then
        if chartype<=SPACE then
            col += 1
            if col>ltl then
                eof_processing() -- (sets Ch to -1 and clears activepaths)
                return
            end if
            if Ch='\n' then
                line += 1
            end if
            Ch = text[col]
        elsif Ch='-' then   -- check for comment
            cp1 = col+1
            if text[cp1]!='-' then exit end if
            cp2 = col+2
            Ch = text[cp2]
            cp3 = col+3
            -- check for "--/*" case:
            if Ch='/' and text[cp3]='*' then
                col = cp3
                SkipBlockComment()
            else
--26/7/17:
--/*
                integer ipstart = 0
                if Ch='#' 
                and length(text)>col+16
                and text[col..col+15]="--#include_paths"
                and find(text[col+16]," \t") then
                                    -- 12345678901234567
                    cp2 += 15 -- (ie col+17)
                    ipstart = cp2
                end if
--*/
--19/05/2010:
--              col = cp3
                col = cp2
                while Ch!='\n' do
                    col += 1
                    Ch = text[col]
                end while
--/*
                if ipstart!=0 then
                    string ip = text[ipstart..col-1]
                    ipstart = match("--",ip)
                    if ipstart then
                        ip = ip[1..ipstart-1]
                    end if
                    ip = get_proper_path(trim(ip),"")
--?{"#include_paths",ip}
                    addPath(ip)
                end if
--*/
                line += 1
                while Ch<=' ' do
                    col += 1
                    if col>ltl then
                        eof_processing()    -- (sets Ch to -1 and clears activepaths)
                        return
                    end if
                    Ch = text[col]
--19/05/2010:
                    if Ch='\n' then
                        line += 1
                    end if
                end while
            end if
        else
            if Ch!='/' then exit end if
            cp1 = col+1
            if text[cp1]!='*' then exit end if
            col = cp1
            SkipBlockComment()
        end if
    end while
end procedure

global procedure skipHashBangLine()
-- called from Compile() in pmain.e when 1st char is '#'
--19/03/2010 (allow #ilasm at very start)
    if ltl>1 and text[2]='i' then return end if
    getCh()
    if Ch!='!' then Expected("#!") end if
    while Ch!='\n' do
        col += 1
-- added 18/9, removed immediately as we should always have a '\n', I think.
--      if col>ltl then
--          Ch = ' '
--          exit
--      end if
        Ch = text[col]
    end while
    line += 1
    skipSpacesAndComments()
end procedure

--with trace
global function allWhiteToTokcol()
integer c
    if Ch>0 then
        for i=tokcol-1 to 1 by -1 do
            c = text[i]
            if c='\n' then return 1 end if
            if charset[c]>SPACE then return 0 end if
        end for
    end if
    return 1
end function

global function isFLOAT(atom N)
--if useFLOAT then --(DEV/temp)
    if machine_bits()=32 then   -- (runtime)
        if X64=0 then           -- (target)
            -- 32 bit compiler -> 32 bit executable
            return not integer(N)
        else
            -- 32 bit compiler -> 64 bit executable (partial range coverage)
--          return N!=floor(N) or N<-#80000000 or N>+#80000000
            return N!=floor(N) or N<-#FFFFFFFF or N>+#FFFFFFFF
--7/7/17: (no help)
--                             or N<MININT     or N>MAXINT
        end if
    else -- machine_bits()=64   -- (runtime)
        if X64=1 then           -- (target)
            -- 64 bit compiler -> 64 bit executable
            return not integer(N)
        else
            -- 64 bit compiler -> 32 bit executable
            return N!=floor(N) or N<-#40000000 or N>+#3FFFFFFF
        end if
    end if
--else
--  return not integer(N)
--end if
end function

procedure setFLOAT()
--
-- from the manual:
--      When using a 32-bit compiler to create a 64-bit executable, be aware that the integer range is
--      redefined as +/-#FFFF_FFFF rather than -#4000_0000_0000_0000 to #3FFF_FFFF_FFFF_FFFF. See ptok.e/
--      setFLOAT() for all the nitty-gritty details.
-- details:
--      Technically, when using a 32-bit compiler to create a 64-bit executable, using 64-bit atoms with
--      53 bits of precision, a limit of +/-#20_0000_0000_0000 would apply (/significant complications
--      could be introduced by using a pair of atoms if we really wanted to go mad) but since the 32-bit
--      compiler can create a 64-bit compiler without problems, there seems little point trying harder.
--      (Besides, the harder you try, the more issues you'll get from still being on a 32 bit run-time.)
--      In practice (see above) we settle for a limit of +/-#FFFF_FFFF, to keep things reasonably simple,
--      because 32 bit versions of things like and_bits() really are limited to 32 bits, and not 53 bits.
--      Programs which use (very large) integer values/constants in the uncovered ranges are likely to be
--      incorrectly cross-compiled. Obviously these are only limits during compilation, not run-time, and
--      the source code of Phix itself does not use any such values, nor do any of the supplied demos.
--      (For obvious reasons the compiler itself should only ever contain completely 32-bit safe code.)
--      The absolute minimum range that we MUST cope with perfectly is (signed) #80000000 (ie -2147483648) 
--      through to (unsigned) #FFFFFFFF (ie + 4294967295), especially <-#40000000|>+#3FFFFFFF, otherwise
--      code that works perfectly on 32-bit may well exhibit cryptic mishaps on 64-bit. I am not overly
--      concerned with 64-bit code failing when compiled to a 32-bit executable, for obvious reasons.
--
--      The sources of the compiler have been modified to use isFLOAT() instead of integer(), but there
--      will inevitably be a few places that I have missed, eg builtins\timestamp.ew currently contains:
--          --DEV temp/32/64 bit issues:
--          --  GENERIC_READ =  #80000000
--              GENERIC_READ =  #8000
--              GENERIC_READ *= #10000
--      which is a symptom of such that ought to be properly investigated and fixed, just not right now.
--
--      Longer term, I expect to add fatal error messages when encountering any tricky cross-compilation 
--      issues; inevitably it will not be long before some 64-bit-only code exists, that makes no sense 
--      to attempt creation of a 32-bit executable from, and I plan to err on the side of caution/blunt 
--      refusal to entertain cross-compilation (32<->64) whenever there is any doubt that it will work.
--
-- This particular routine is concerned with correctly tagging tokens found in the source code; the above
--  isFLOAT() is factored out to apply the same logic elsewhere, eg/ie as part of constant propagation.
--
    if isFLOAT(TokN) then
        toktype = FLOAT
    end if
end procedure

--with trace
procedure completeFloat()
-- TokN contains the integer (mantissa) part of the float.
-- next char should be one of .eE
-- return the float value.
-- Note that "1.0" is effectively treated as "1".
atom dec
--integer ndp -- number of decimal places
integer exponent
integer esigned
--  ndp=0
atom fraction
    if Ch='.' then
        toktype = DIGIBAD
--      dec = 10
        dec = 1
        fraction = 0
        while 1 do
            col += 1
            Ch = text[col]
--          if charset[Ch]!=DIGIT then exit end if
--26/6/10...
            if Ch!='_' then
                if Ch<'0' or Ch>'9' then exit end if
--              TokN = TokN*10 + Ch-'0'
--27/10/15:
--              TokN += (Ch-'0') / dec
                fraction = fraction*10+(Ch-'0')
                dec *= 10
                toktype = DIGIT
--              ndp += 1
            end if
        end while
        TokN += fraction/dec
    end if
    if toktype=DIGIBAD then Abort("illegal") end if
    exponent = 0
    if Ch='e' or Ch='E' then
        toktype = DIGIBAD
        esigned = 0
        while 1 do
            col += 1
            Ch = text[col]
            --DEV this should not really be in the loop, eg "2.4e--"
--          if charset[Ch]!=DIGIT then
            if Ch<'0' or Ch>'9' then
                if Ch!='_' then
                    if toktype!=DIGIBAD then exit end if -- ie first time round only
                    if Ch='-' then
                        esigned = 1
                    elsif Ch!='+' then
                        exit
                    end if
                end if
            else
                exponent = exponent*10 + Ch-'0'
                toktype = DIGIT
            end if
        end while
        if esigned then
            exponent = -exponent
        end if
--  end if
    if toktype=DIGIBAD then Abort("illegal") end if
--  exponent-=ndp
--  if exponent then
        if exponent>308 then
            -- rare case: avoid power() overflow
            TokN *= power(10, 308)
            if exponent>1000 then
                exponent = 1000 
            end if
            for i=1 to exponent-308 do
                TokN *= 10
            end for
        elsif exponent<0 then
            TokN /= power(10,-exponent)
        else
            TokN *= power(10, exponent)
        end if
    end if

end procedure

-- Technical note:  These values achieve near immortality; for example, suppose
-- ==============   you want \g to represent BEL (7), but, of course you cannot
--  (aka gotcha)    put '\g' yet, so instead put 'g' as a placeholder, you will 
--                  find it stays 'g' forever. Obviously pretty easy to fix and
--                  for the same reasons once fixed will stay fixed forever ;-).
--                  (eg/ie use \x07 for one round of self-hosting the compiler)

constant escchar = "nrtb\"\'\\0eE#xuU",
                --  1234 5 6 78901234
         escbyte = "\n\r\t\b\"\'\\\0\e\E"
                --   1 2 3 4 5 6 7 8 9 0

constant eschash = 11,
         eschex  = 12

constant AFaf = "ABCDEFabcdef"
constant bases = {8,16,2,10}    -- NB: oxbd order
integer base
        base = -1
integer bCh
        bCh = -1
integer prevCh
        prevCh = -1

procedure getByteWiseOctal()
-- specialised bytewise octal handling
-- eg 0ob377377377377 is the same as #FFFFFFFF
    col += 1
    Ch = text[col]
    -- groups of 3:
    while 1 do
        -- first char 0..3
        bCh = baseset[Ch]
        if bCh>3 then exit end if
        toktype = DIGIBAD
        TokN = TokN*4 + bCh
        col += 1
        Ch = text[col]
        -- second char 0..7
        bCh = baseset[Ch]
        if bCh>7 then exit end if
        TokN = TokN*8 + bCh
        col += 1
        Ch = text[col]
        -- third char 0..7
        bCh = baseset[Ch]
        if bCh>7 then exit end if
        TokN = TokN*8 + bCh
        col += 1
        Ch = text[col]
        toktype = DIGIT
    end while
    if toktype=DIGIBAD then Abort("bytewise octal must be in strict {377} format") end if
end procedure

--with trace
procedure loadBase()
    if base=5 then      -- 0(nn) case
        base = 0
        while 1 do
            col += 1
            Ch = text[col]
            if Ch<'0' or Ch>'9' then exit end if
            base = base*10 + Ch-'0'
        end while
--      if not find(base,bases) then
        if base<2 or base>36 then
            -- TIP:
            -- If you want to allow eg base 64 literals as eg 0(64)9QJZB3FX (terminating space?)
            --  Then 1) fill in 17..64 in baseset and/or use a basesetBig with 'a'..'f' not
            --  the same as 'A'..'F'. 2) add the base to bases. 3) think of a letter (but not a-f 
            --  please) to use as eg 0y9QJZB3FX and change find(Ch,"oxbd(") to find(Ch,"oxbdy(") 
            --  or if no such letter is needed, change to say find(Ch,"oxbdo("). 4) Apply to all
            --  four cases: 0(64)NNN, 0yNNN, #(64)NNN, and #yNNN, and test thoroughly!
            Abort("unsupported number base")
        end if
        if Ch!=')' then
            tokcol = col
            Abort("')' expected")
        end if
--      col += 1
--      Ch = thisline[col]?? 1/11/09
    else
        base = bases[base]
    end if
    col += 1
    Ch = text[col]
    toktype = DIGIBAD
    if base=8 and Ch='b' then
        getByteWiseOctal()
    else
        while 1 do
            bCh = baseset[Ch]
            if bCh>base then
--              if toktype!=DIGIT then exit end if
--              if base!=10 or Ch!='_' then exit end if     -- allow eg 1_000_000 to mean 1000000
                if Ch!='_' then exit end if     -- allow eg 1_000_000 to mean 1000000 (any base)
            else
                TokN = TokN*base + bCh
                toktype = DIGIT
            end if
            col += 1
            Ch = text[col]
        end while
        if toktype=DIGIBAD then Abort("missing digits") end if
    end if
    setFLOAT()
end procedure

--with trace
procedure MultiLineString(integer termch)
-- termch should be '`' for backtick handling 
--               or '\"' for triplequote handling
integer trimlength
--trace(1)
--DEV (spotted in passing) shouldn't this be doing some:
--                  if Ch='\n' then
--                      line += 1
--                  end if
    TokStr = ""
    toktype = DQUOTE
    trimlength = 0
    if charset[Ch]=EOL then
        col += 1
        while col<=ltl do
-- Added 4/7/13:
            if Ch='\n' then
                line += 1
            end if
            Ch = text[col]
            if charset[Ch]!=EOL then exit end if
            col += 1        
        end while
        while Ch='_' do
            trimlength += 1
            col += 1
            if col>ltl then exit end if
            Ch = text[col]
        end while
    end if
    while 1 do
        if Ch=termch then
            if Ch='`' then exit end if
            if col<ltl-2
            and text[col+1]='\"'
            and text[col+2]='\"' then
                col += 2
                exit
            end if
        end if
        col += 1
        if col>ltl then
            Abort("missing end quote")
        end if
        if Ch!='\r' then
            TokStr = append(TokStr,Ch)
            if Ch='\n' then
                for i=1 to trimlength do
                    Ch = text[col]
                    if charset[Ch]!=SPACE then exit end if
                    col += 1
--                  if col>ltl then exit end if
                    if col>ltl then
                        Abort("missing end quote")
                    end if
                end for
--Added 4/7/13:
                line += 1
            end if
        end if
        Ch = text[col]
    end while
    col += 1
    Ch = text[col]
--          if Ch='`' then
--              col += 1
--              Ch = text[col]
--              return
--          end if
--          TokStr = append(TokStr,Ch)
--          col += 1
--          Ch = text[col]
--          if col>=ltl or Ch='\n' or Ch='\r' then
--              Abort("missing end quote")
--          end if
--      end while
end procedure

--with trace
constant HBSize = {2,4,8}
procedure HexadecimalByteString()
--
-- Returns the equivalent of a DQUOTE, which pmain.e processes further.
-- It does however check for invalid characters and converts 'a'..f' to 'A'..'F'.
--
    toktype = HEXSTR
    TokN = HBSize[find(Ch,"xuU")]
    TokStr = ""
    col += 1
    while 1 do
        col += 1
        Ch = text[col]
        if col>=ltl or Ch='\n' or Ch='\r' then
            tokcol = col
            Abort("missing end quote")
        elsif Ch='\"' then
            col += 1
            Ch = text[col]
            return
        elsif Ch='\t'
           or Ch=' ' then
            Ch = ' '
        elsif Ch>='a'
          and Ch<='f' then
            Ch -= #20
        elsif Ch>'F'
           or Ch<'0'
           or (Ch>'9' and Ch<'A') then
            tokcol = col
            Abort("hex digit expected")
        end if
        TokStr = append(TokStr,Ch)
    end while
end procedure

integer preprocactive = 0

forward procedure preprocess()

include builtins\utfconv.e

--without trace
--global procedure getToken()
--global procedure getToken(bool parse_dot_as_symbol=false)
-- A parse_dot_as_symbol of true simply means the next token cannot legally be a
--  floating point number. It may or may not be a valid dot-subscript.
--  A default of true would probably be more sensible (long-term)
--  [DEV ==> bool float_valid=false] (strip defaults, then kill)

global procedure getToken(bool float_valid=false)
--
-- A float_valid of true fairly obviously means that a float is valid; by default
--  any '.' are returned as a separate SYMBOL (for ORAC, ignored if that is 0).
--  I would expect the odd get_token(false) when simply being explicit, and most
--  certainly that or () after },),],",',`,and $, otherwise I would only expect a
--  getToken(true) after most of {,(,[,=,>,+,-,*,/,&,?,..,and,or,xor,and not (but 
--  not all, esp format directives, routine definitions, multi-assign lhs, enums, 
--  and #ilASM, #istype, #isinit, and #isginfo).
--
--integer savecol
--integer signed, toklen

    while 1 do
        toktype = charset[Ch]                   -- NB: idx out of bounds here (Ch=-1) 
                                                --      should be fixed by testing for
                                                --      that before the getToken() call,
                                                --      rather than here.
                                                --      if Ch is 0, it should be fixed 
                                                --      in loadFile [?].

        if toktype>SYMBOL then exit end if
        if toktype<=SPACE then
            col += 1
            if col>ltl then
                toktype = EOL
                eof_processing()    -- (sets Ch to -1 and clears activepaths)
                return
            end if
            if Ch='\n' then
                line += 1
            end if
            Ch = text[col]
        elsif Ch='-' then   -- check for comment
            cp1 = col+1
            if text[cp1]!='-' then exit end if
            cp2 = col+2
            Ch = text[cp2]
            cp3 = col+3
            -- check for "--/*" case:
            if Ch='/' and text[cp3]='*' then
                col = cp3
                SkipBlockComment()
            elsif Ch='*' and text[cp3]='/' then
                tokcol = col
                tokline = line
                Abort("unexpected end block comment")
            else
--26/7/17:
--/*
                integer ipstart = 0
                if Ch='#' 
                and length(text)>col+16
                and text[col..col+15]="--#include_paths"
                and find(text[col+16]," \t") then
                    cp2 += 15 -- (ie col+17)
                    ipstart = cp2
                end if
--*/
                col = cp2
                while Ch!='\n' do
                    col += 1
                    Ch = text[col]
                end while
--/*
                if ipstart!=0 then
                    string ip = text[ipstart..col-1]
                    ipstart = match("--",ip)
                    if ipstart then
                        ip = ip[1..ipstart-1]
                    end if
                    ip = get_proper_path(trim(ip),"")
--?{"#include_paths",ip}
                    addPath(ip)
                end if
--*/
                while Ch<=' ' do
                    if Ch='\n' then
                        line += 1
                    end if
                    col += 1
                    if col>ltl then
                        toktype = EOL
                        eof_processing()    -- (sets Ch to -1 and clears activepaths)
                        return
                    end if
                    Ch = text[col]
                end while
            end if
        else
            if Ch!='/' then exit end if
            cp1 = col+1
            if text[cp1]!='*' then exit end if
            col = cp1
            SkipBlockComment()
        end if
    end while

    tokline = line
    tokcol = col
    tokno = 0
    if toktype=LETTER then
        if find(Ch,"xuU") and text[col+1]='\"' then
            HexadecimalByteString()
            return
        end if
        tt_search()
        if ttidx=T_end then
            if mapEndToMinusOne>0 and not ORAC then
                mapEndToMinusOne = T_end
                toktype = DIGIT
                TokN = -1
            end if
        elsif ttidx=T_ifdef and preprocactive=0 then
            preprocactive = 1
--          call_proc(r_preprocess,{})
            preprocess()
--          getToken()
            getToken(float_valid)
            preprocactive = 0
        end if
        return
    end if  -- toktype = LETTER
    prevCh = Ch
    col += 1
    Ch = text[col]
    if toktype=SYMBOL then
        -- !&*+,-./:;<=>?
        -- note that most compound symbols such as "!=" 
        -- are handled in the parser as two tokens, or
        -- by testing the global Ch for (say) '='.
        if prevCh='.' then
            if Ch='.' then
                col += 1
                Ch = text[col]
                toktype = ELLIPSE
                return
--          elsif charset[Ch]=DIGIT then -- ".4" is a number
--DEV (use fromsubss or parse_dot_as_symbol?)
--          elsif Ch>='0' and Ch<='9' then -- ".4" is a number
--          elsif (not ORAC) and Ch>='0' and Ch<='9' then -- ".4" is a number
--DEV this may want to be (not ORAC or float_valid)  [spotted in passing, 27/3/17]
            elsif (ORAC and float_valid)
              and Ch>='0' and Ch<='9' then -- ".4" is a number
                col -= 1
                Ch = '.'
                TokN = 0
                completeFloat()     -- will set toktype to DIGIT
                setFLOAT()
                return
            end if
        end if
        toktype = prevCh
        return
    elsif toktype=BRACES then
        -- []{}()
        toktype = prevCh
        return
    elsif toktype=DIGIT then
        -- 0..9
        -- note that eg "1.0" is treated as the integer "1"
        --
        TokN = prevCh-'0'

        if TokN=0 then  -- check for 0x/o/b/d formats
-- 't' added as octal to match Open Euphoria 13/12/2010
--          base = find(Ch,"oxbd(")
--          base = find(Ch,"toxbd(")
            base = find(lower(Ch),"toxbd(")
            if base then
                if base>1 then
                    base -= 1
                end if
                loadBase()
                return
            end if
        end if

        while 1 do
            if Ch<'0' or Ch>'9' then
                if Ch!='_' then exit end if     -- allow eg 1_000_000 to mean 1000000
            else
                TokN = TokN*10 + Ch-'0'
            end if
            col += 1
            Ch = text[col]
        end while
        cp1 = col+1
--DEV (ORAC/fromsubss)
--      if Ch='.' then
        if Ch='.' and (not ORAC or float_valid) then
            ch2 = text[cp1]
        else
            if Ch='\'' and text[cp1]=TokN and text[col+2]='\'' then
                col += 3
                Ch = text[col]
                return
            end if
            ch2 = '.'
        end if
--      if (Ch='.' and thisline[cp1]!='.')      -- fraction but not ellipse
        if ch2!='.'                             -- fraction but not ellipse
        or (Ch='e' or Ch='E') then              -- exponent ahead
            completeFloat()
        end if
        setFLOAT()
        return
    elsif toktype=DQUOTE then
        TokStr = ""
        while 1 do
            if Ch='\"' then
                col += 1
                Ch = text[col]
                if Ch='\"' and length(TokStr)=0 then
                    col += 1
                    Ch = text[col]
                    MultiLineString('\"')
                end if
                return
            elsif Ch='\\' then
                col += 1
                Ch = text[col]
                Ch = find(Ch,escchar)
                if Ch=0 then
                    tokcol = col
                    Abort("unrecognised escape character")
                elsif Ch=eschash or Ch=eschex then -- (ie \# or \x)
                    -- inline hex byte (max 2 digits):
                    col += 1
                    Ch = text[col]
                    Ch = baseset[Ch]
                    if Ch>16 then
                        tokcol = col
                        Abort("hex digit expected")
                    end if
                    col += 1
                    bCh = text[col]
                    bCh = baseset[bCh]
--replaced 11/6/2013:
--                  if bCh>16 then
--                      tokcol = col
--                      Abort("hex digit expected")
--                  end if
--                  Ch = Ch*16+bCh
                    if bCh<=16 then
                        Ch = Ch*16+bCh
                    else
                        col -= 1
                    end if

--DEV \b should be backspace... (removed 11/6/2013)
--              elsif Ch=10 then    -- (ie \b)
--                  -- inline binary byte
--                  col += 1
--                  Ch = text[col]
--                  savecol = col
--                  if Ch<'0' or Ch>'1' then
--                      tokcol = col
--                      Abort("binary digit expected")
--                  end if
--                  TokN = Ch-'0'
--                  while 1 do
--                      col += 1
--                      Ch = text[col]
--                      if not find(Ch,"01") then exit end if
--                      TokN = TokN*2 + Ch-'0'
--                  end while
--                  if TokN>255 then
--                      tokcol = savecol
--                      Abort("inline binary byte may not exceed 255")
--                  end if
----?? this may help? (untried)
----                    Ch = and_bits(TokN,#FF)
--                  Ch = TokN
--                  col -= 1
                elsif Ch=13 or Ch=14 then -- (ie \u or \U)
if 1 then   -- new code (2/7/16)
--DOC: unicode characters are converted to their utf-8 equivalents. 
--  Note that invalid characters (>#10FFFF or #D800..#DFFF) are converted to "\#EF\#BF\#BD".
--  Don't expect console displays (?my_unicode_string) or debug/trace screens to be pretty.
--  When using pGUI, do not forget to add an IupSetGlobal("UTF8MODE","YES") at the start.
                    integer nchars = (Ch-12)*4  -- (4 or 8 digits)
                    Ch = 0
                    for i=1 to nchars do
                        col += 1
                        bCh = text[col]
                        bCh = baseset[bCh]
                        if bCh>16 then
                            tokcol = col
                            Abort("hex digit expected")
                        end if
                        Ch = Ch*16+bCh
                    end for
                    string utf8 = utf32_to_utf8({Ch})
                    Ch = utf8[$]
                    TokStr &= utf8[1..-2]
else -- old code
--DEV... (of course we could just return a dword-sequence here, which would need a new type
--        instead of DQUOTE and pmain.e to T_Dseq it instead if T_string it (tiny job), but
--        much more significantly heavy testing of puts() etc...)
                    tokcol = col
                    Abort("Sorry, Phix does not [yet] support 2 or 4 byte unicode characters in 8-bit strings.")
end if
                else
                    Ch = escbyte[Ch]
                end if
            elsif Ch='\t' then
                tokcol = col
                Abort(`tab character found in string - use spaces or \t instead`)
            end if
            TokStr = append(TokStr,Ch)
            col += 1
            Ch = text[col]
            if col>=ltl or Ch='\n' or Ch='\r' then
                tokcol = col
                Abort("missing end quote")
            end if
        end while
    elsif toktype=BKTICK then -- string with no escape characters (nb tabs treated as-is!)
        MultiLineString('`')
        return
    elsif toktype=HEXDEC then
        -- #
        if Ch='i' then
            -- #ilASM{inline assembly},
            -- #isType{var_id,var_type}, or
            -- #isInit{var_id,0/1}.
--          toktype = ILASM
            return
--      elsif Ch='t' then   --#type_is construct
--          toktype = TYPEIS
--          return
        end if
--      base = find(Ch,"ooox(") -- Note: #b0101 and #d99 notations not supported (treated as 721153 & 3481)
--      if base then
--          loadBase()
--          return
--      end if
        toktype = DIGIBAD -- ensure followed by >=1 hex digit
        TokN = 0
--      toklen = 0
--      signed = 0
--      if find(Ch,"-+") then
--          signed = (Ch='-')
--          col += 1
--          Ch = text[col]
--      end if
--DEV for X64=1, this wants to optionally return {dword,dword} (toktype=DIGIT64?)
        while 1 do
--          if Ch=-1 then exit end if
--          if charset[Ch]!=DIGIT then
--          if Ch<'0' or Ch>'9' then
            if Ch<'0' then exit end if
            if Ch>'9' then
                if Ch!='_' then
                    prevCh = find(Ch,AFaf) 
                    if not prevCh then exit end if
                    if prevCh>6 then
                        prevCh += 3 -- (7..12, aka a..f, --> 10..15)
                    else
                        prevCh += 9 -- (1..6, aka A..F, --> 10..15)
                    end if
                    TokN = TokN*16 + prevCh
--                  toklen += 1
                end if
            else
                TokN = TokN*16 + Ch-'0'
--              toklen += 1
            end if
            toktype = DIGIT
            setFLOAT()
            col += 1
            Ch = text[col]
        end while
        if toktype=DIGIBAD then Abort("illegal") end if
-- removed 24/09/2013:
--      if signed then
--          if not find(toklen,{2,4,8}) then
----                if 64-bit then  --DEV more work/testing rqd
----                    if toklen!=16 then
----                        Abort("Signed hexadecimal literals must be length 2, 4, 8, or 16")
----                    end if
----                else
--                  Abort("Signed hexadecimal literals must be length 2, 4, or 8")
----                end if
--          end if
--          if toklen=2 then
--              if and_bits(TokN,#80) then TokN = or_bits(#FFFFFF00,TokN) end if
--          elsif toklen=4 then
--              if and_bits(TokN,#8000) then TokN = or_bits(#FFFF0000,TokN) end if
----            elsif toklen=8 then
----            elsif toklen=16 then
--          end if
--          TokN = and_bits(-1,TokN)
--      end if
        setFLOAT()
        return
    elsif toktype=SQUOTE then
        if Ch='\\' then
            col += 1
            Ch = text[col]
            Ch = find(Ch,escchar)
            if not Ch or Ch>eschex then
                tokcol = col
                Abort("unrecognised escape character")
            elsif Ch=eschash or Ch=eschex then -- (ie \# or \x)
                -- inline hex byte:
                col += 1
                Ch = text[col]
                Ch = baseset[Ch]
                if Ch>16 then Abort("hex digit expected") end if
                col += 1
                bCh = text[col]
                bCh = baseset[bCh]
                if bCh>16 then Abort("hex digit expected") end if
                Ch = Ch*16+bCh
            else
                Ch = escbyte[Ch]
            end if
        elsif Ch='\t' then
            tokcol = col
            Abort(`tab character - use space or \t instead`)
        end if
        Tok9 = Ch   -- DEV use Ch! [DOH, we cannot as Ch is NEXT char!!!]
        col += 1
        Ch = text[col]
        if col>ltl or Ch!='\'' then
            tokcol = col
            Abort("missing end quote")
        end if
--      getCh()
        col += 1
        Ch = text[col]
        return
    elsif toktype=ILLEGAL and mapEndToMinusOne then
        if prevCh='$' then
            if mapEndToMinusOne=-1      -- from DoConstant
            or mapEndToMinusOne=-2 then -- from DoEnum
                mapEndToMinusOne = '$'
            end if
            if T_end='$' then ?9/0 end if   -- sanity check (T_end was 224, and '$'=36 last time I checked)
            toktype = DIGIT
            TokN = -1
            return
        elsif prevCh='%' 
          and mapEndToMinusOne=-3 then  -- from preprocess()
            -- (preprocess() scans the whole file, which may contain #ilasm
            --  constructs. Allows preprocess() to quietly skip eg %isVar)
            toktype = DIGIT
            TokN = -1
            return
        end if
--23/7/16:
    elsif prevCh=#1A then   -- Ctrl Z
        eof_processing()    -- (sets Ch to -1 and clears activepaths)
        return
    else
--DEV:::
--puts(1,"Illegal character (eof assumed):\n")
--?{toktype,Ch,col,line}
--Ch=-1
--DEV:
--  if toktype=LETTER then
----        charset['\\']=ILLEGAL -- reset following include processing
----        skipSpaces()
--  end if
--  if toktype=ILLEGAL then
            Abort("illegal character")
----            illcheck()
--      end if
--  end if              
    end if
end procedure

--global procedure Semi()
---- Match a Semicolon
--  if toktype=';' then getToken() end if
--end procedure

--global procedure MatchChar(integer x)
--global procedure MatchChar(integer x, bool parse_dot_as_symbol=false)
-- A parse_dot_as_symbol of true simply means the next token cannot legally be a
--  floating point number. It may or may not be a valid dot-subscript.

global procedure MatchChar(integer x, bool float_valid=false)
--
-- Match a Specific Input Char, typically a symbol eg ')'
-- NB: This is only ever called when you already know the currrent token will match
--     It also makes the code a bit more self-documenting
--
-- By default float_valid is false ('.' treated as a separate SYMBOL) [for ORAC]
-- Typically I would expect:
--      MatchChar('=',true)     -- (ditto +-*/&)
--      MatchChar('{',true)     -- (except lhs of multi-assign)
--      MatchChar('(',true)     -- (on calls not definitions)
--      MatchChar(',',true)     --  "",""
--      MatchChar('[',true)
--      MatchChar('.',false)    -- (for ORAC)
--      MatchChar(')'[,false])
--      MatchChar('}'[,false])
--
--  if not equal(Ch,x) then Expected(`'`&x&`'`) end if
    if not equal(toktype,x) then Expected(`'`&x&`'`) end if
--  getToken()
    getToken(float_valid)
end procedure

--global procedure MatchString(integer T_ident)
--global procedure MatchString(integer T_ident, bool parse_dot_as_symbol=false)
-- A parse_dot_as_symbol of true simply means the next token cannot legally be a
--  floating point number. It may or may not be a valid dot-subscript.

global procedure MatchString(integer T_ident, bool float_valid=false)
--
-- Match a Specific Input String
-- NB: This is only ever called when you already know the currrent token will match,
--     or when it is mandatory, eg as per 'then' in 'if then else end if' 
--     It also makes the code a bit more self-documenting
--
-- By default float_valid is false ('.' treated as a separate SYMBOL) [for ORAC]
-- Typically I would expect:
--  MatchString(T_xxx,true) on T_and, T_or, T_xor, T_if, T_while, T_to, T_return,
--  T_switch, and T_case, with MatchString(T_xxx[,false]) on all other T_xxx and
--  some T_if, T_while, and T_return as appropriate.
--
    if toktype!=LETTER 
    or ttidx!=T_ident then
        Expected('"'&getname(T_ident,-2)&'"')
    end if
    getToken(float_valid)
end procedure

--with trace
-- routines for preprocess():
procedure KillString(integer T_ident)
    for i=tokcol to col-1 do
        text[i] = ' '
    end for
    if T_ident!=ttidx then
        allpfiles[fileno] = text
    end if
    MatchString(T_ident)
end procedure

procedure KillChar(integer x)
    text[tokcol] = ' '
    if toktype!=x then
        allpfiles[fileno] = text
    end if
    MatchChar(x)
end procedure

procedure KillToken()
    for i=tokcol to col-1 do
        text[i] = ' '
    end for
    getToken()
end procedure

procedure Aborpp(sequence msg)
--  if probable_logic_error then show_ple() end if      --DEV??
    allpfiles[fileno] = text
    Abort(msg)
end procedure

--with trace
procedure process_one_ifdef()
integer state = 0   -- 0 = normal start, looking for a branch to keep
                    -- 1 = suppress remaining elsifdef/elsedef/end ifdef
integer negate, nest, flag, thisflag, andor
integer wastokcol, wascol
sequence name

--trace(1)
    andor = 0
    while Ch>0 do
        if ttidx=T_ifdef
        or ttidx=T_elsifdef then
            KillString(ttidx)
            while Ch>0 do   -- process any and/or in the tests
                negate = 0
                if toktype='!' then
                    negate = 1
                    KillChar('!')
                elsif toktype=LETTER and ttidx=T_not then
                    negate = 1
                    KillString(T_not)
                end if
                if toktype!=LETTER then
                    Aborpp("a name is expected here")
                end if
                if ttidx=T_WIN32
                or ttidx=T_WINDOWS then
--                  thisflag = 1    -- DEV (PE==1 && X64==0)
                    thisflag = PE
                elsif ttidx=T_LINUX
--                 or ttidx=T_FREEBSD
--                 or ttidx=T_SUNOS
--                 or ttidx=T_OPENBSD
--                 or ttidx=T_OSX
                   or ttidx=T_UNIX then
--                  thisflag = 0    -- DEV (PE==0?)
                    thisflag = not PE
                elsif ttidx=T_WIN32_GUI
                   or ttidx=T_WIN32_CONSOLE then
                    thisflag = OptConsole
                    if thisflag=-1 then
                        --DEV until I put in an opcode for setting this depending on
                        --    whether p.exw or pw.exe (or some renamed copy of either)
                        --    is running, you must first explicitly specify one of
                        --    with/without console/gui before you can use the above.
                        --OR, we can call pemit.e/readAllHeaders()/getmzpe(#DC, WORD)
                        --    and "thisflag = (res=CUI)" (where CUI=3).
                        Aborpp("with/without console/gui must be explicitly specified first")
--                  else OptConsole is 0=gui, 1=console...
                    end if
                    if ttidx=T_WIN32_GUI then
                        thisflag = not thisflag
                    end if
                elsif ttidx=T_SAFE
                   or ttidx=T_DATA_EXECUTE
                   or ttidx=T_UCSTYPE_DEBUG
                   or ttidx=T_EU4_1
                   or ttidx=T_OSX
                   or ttidx=T_FREEBSD
                   or ttidx=T_SUNOS
                   or ttidx=T_OPENBSD
                   or ttidx=T_CRASH then
                    thisflag = 0
                elsif ttidx=T_BITS32 then
--                  thisflag = machine_bits()=32
                    thisflag = not X64
                elsif ttidx=T_BITS64 then
                    thisflag = X64
                elsif ttidx=T_PHIX then
                    thisflag = 1
                else
                    Aborpp("unrecognised")
                end if
                if negate then
                    thisflag = not thisflag
                end if
                KillToken()
                if andor=0 then -- neither encountered yet
                    flag = thisflag
                    if toktype!=LETTER then exit end if
                    if ttidx=T_and then
                        andor = 1
                    elsif ttidx=T_or then
                        andor = 2
                    else
                        exit
                    end if
                else
                    if toktype!=LETTER then exit end if
                    if andor=1 then -- we've had an and already
                        flag = flag and thisflag
                        if ttidx!=T_and then exit end if
                    elsif andor=2 then  -- we've had an or already
                        flag = flag or thisflag
                        if ttidx!=T_or then exit end if
                    else
                        ?9/0
                    end if
                end if
                KillToken() -- discard the T_and/T_or
            end while
            KillString(T_then)
        elsif ttidx=T_elsedef then
            flag = 1
            KillString(T_elsedef)
        else
            Aborpp("ifdef/elsifdef/elsedef expected\n") --???
        end if
        if flag and state=0 then
            -- keep this branch
            while Ch>0 do
                if toktype=LETTER then
                    if ttidx=T_end then
                        wastokcol = tokcol
                        wascol = col
                        MatchString(T_end)
                        if ttidx=T_ifdef then
                            for i=wastokcol to wascol-1 do
                                text[i] = ' '
                            end for
                            KillToken()
                            return      -- ALL DONE!
                        end if
                    elsif ttidx=T_elsifdef
                       or ttidx=T_elsedef then
                        exit
                    elsif ttidx=T_ifdef then
                        process_one_ifdef()
                    elsif ttidx=T_include then
                        -- avoid any '\' illegal char errors
                        name = getIncludeLine()
                    end if
                end if
                getToken()
            end while
            state = 1
        else
            -- wipe this branch (and any nested ifdefs)
            nest = 0
            while Ch>0 do
                if toktype=LETTER then
                    if ttidx=T_end then
                        KillString(T_end)
                        if ttidx=T_ifdef then
                            if nest=0 then exit end if
                            nest -= 1
                        end if
                    elsif ttidx=T_elsifdef
                       or ttidx=T_elsedef then
                        if nest=0 and state=0 then exit end if
                    elsif ttidx=T_ifdef then
                        nest += 1
                    elsif ttidx=T_include then
                        -- avoid any '\' illegal char errors
                        wastokcol = tokcol
                        name = getIncludeLine()
                        tokcol = wastokcol
                    end if
                end if
                KillToken()
            end while
            if ttidx=T_ifdef then
                KillToken()
                return      -- ALL DONE!
            end if
        end if
    end while
    Aborpp(`"end ifdef" expected`)
end procedure

procedure preprocess() -- called from getToken()
--
-- Converts eg "ifdef WIN32 then puts(1,"WIN32") elsedef puts(1,"NOT WIN32") end ifdef"
--          to "                 puts(1,"WIN32")                                      "
--
-- Linebreaks and comments are preserved. The result is stored in allpfiles, with the
--  original still in allfiles. Obviously further parsing occurs on the allpfiles
--  version but any errors/warnings/listings work off the allfiles original.
-- 
-- NB this is the **BARE MINIMUM NECESSARY** to support RDS Eu4 std/ files...
-- 
-- Specifically:
-- ============
--  WIN32/WINDOWS are assumed to be TRUE/DEFINED,
--  LINUX/FREEBSD/SUNOS/OPENBSD/OSX/UNIX are assumed FALSE/UNDEFINED [DEV?]
--  WIN32_GUI/WIN32_CONSOLE are dependent on with/without console/gui,
--      and /NOT/ on whether p.exe or pw.exe is running (**NB**). [DEV]
--  SAFE/DATA_EXECUTE/UCSTYPE_DEBUG/EU4_1/CRASH are assumed FALSE/UNDEFINED.
--  everything else is assumed FALSE/UNDEFINED.
--  
-- I am certainly no fan of ifdef. The popular argument that eg:
--
--      if platform()=WIN32 then
--          puts(1,"This is win32\n")
--      else
--          puts(1,"This is NOT win32\n")
--      end if
--
--  is somehow not as good as:
--
--      ifdef WIN32 then
--          puts(1,"This is win32\n")
--      elsedef
--          puts(1,"This is NOT win32\n")
--      end ifdef
--
--  is just nonsense. Try "-d"'ing the above snippets and you'll see.
--  It is of course far better to improve the compiler/language at a 
--  fundamental level than paper over cracks or add ugly warts!
--
--  Worse:
--      ifdef WIN32 then
--          include win32version.e
--      elsedef
--          include nonwin32version.e
--      end ifdef
--  is just weak-minded. If you are going to be cross-platform, then 
--  includes should be cross-platform! If anyone wants to use those
--  files, they are forced to type out those five lines, not just one.
--  Also, if anyone fixes a bug in one of those files, they are much
--  less likely to apply the equivalent fix in the other...
--
-- My other argument against ifdef is that things end up being defined 
--  in batch files (ie another language), config files (not a language)
--  or worse on the command line, generally unavailable at run-time and
--  nothing short of a blatent incitement to make packaging blunders.
--
-- Anyway, that's my rant for the day.
--
-- Actually, one case has cropped up which /is/ useful:
--
--      ifdef PHIX then
--          ...
--      elsedef
--          ...
--      end ifdef
--
--  which works out quite well (with absolutely no changes to OE).
--
string name
integer wasline, wascol
integer wasmapEndToMinusOne
--12/11/15: (either this or use a flag to switch off eof_processing)
sequence was_activepaths = activepaths

--trace(1)
    wascol = col
    wasline = line
    wasmapEndToMinusOne = mapEndToMinusOne
--DEV should this be inside the loop??
    mapEndToMinusOne = -3 -- treat '$' and '%' as normal symbols
    
    -- get the clone over and done with:
    text[col-1] = ' '

--?"preprocess!\n"
    while Ch!=-1 do -- keep going until end of file
--trace(1)
        process_one_ifdef()
        -- and check remainder of file...
        while Ch>0 do
--          getToken()
            if toktype=LETTER then
                if ttidx=T_end then
                    MatchString(T_end)
                    if ttidx=T_ifdef then Aborpp("no matching ifdef") end if
                elsif ttidx=T_elsifdef
                   or ttidx=T_elsedef then Aborpp("no matching ifdef")
                elsif ttidx=T_ifdef then
                    exit
                elsif ttidx=T_include then
                    name = getIncludeLine()
                end if
            end if
            getToken()
        end while
    end while       

    allpfiles[fileno] = text

    mapEndToMinusOne = wasmapEndToMinusOne
    col = wascol
    line = wasline
    Ch = ' '
--12/11/15:
    activepaths = was_activepaths

end procedure
--r_preprocess = routine_id("preprocess")


