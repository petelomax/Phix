--
-- utfconv.e
-- =========
--
-- Simple unicode conversion routines, originally written for edix.
--  Based heavily on the work by Shian Lee, but reworked to use Phix string types, and
--  extended to incorporate four new UTF-16 routines (two of which are trivial wrappers).
--
-- The primary purpose of this is to generate a one-element-per-character medium for edix to
--  navigate/manipulate, hence further testing is advised before adoption for any other use.
--  (That said, if edix starts messing things up, I guess I'll hear about it pretty soon...)
--
-- BOMs (Byte Order Marks, aka "ZERO WIDTH NO-BREAK SPACE" prefix characters) should really
--  be handled by the calling routines, at the read/write file level, rather than here. 
--  Leaving them in would almost certainly make edix mis-locate subsequent characters, and,
--  incidentally, edix has no handling for the "Word Joiner" character, U+2060, either.
--
--(copy and paste these definitions as needed)
--constant
--  UTF8    = "\#EF\#BB\#BF",
--  UTF16BE = "\#FE\#FF",
--  UTF16LE = "\#FF\#FE",
--  UTF32BE = "\#00\#00\#FE\#FF",
--  UTF32LE = "\#FF\#FE\#00\#00",
--
--  -- I recommend issuing error if you find these:
--  UTF7      = "\#2B\#2F\#76", -- (38|39|2B|2F|38 2D)
--  UTF1      = "\#F7\#64\#4C",
--  UTFEBCDIC = "\#DD\#73\#66\#73",
--  SCSU      = "\#0E\#FE\#FF",
--  BOCU1     = "\#FB\#EE\#28",
--  GB18030   = "\#84\#31\#95\#33"
--
-- Likewise the UTF-16 routines have no comprehension of any difference between UTF-16LE
--  and UTF-16BE: that is down to how the calling application reads/writes or peeks/pokes.
--  Quite clearly by the time you pass a value/character/unicode point to these routines, 
--  it should be, well, a value, in the proper and expected endian-ness of the machine 
--  the program is running on, rather than (say) a sequence of (optionally) byte-swapped 
--  elements, which could only ever make everything far harder than it needs to be. 
--
-- Usage (UTF-8)
-- =============
--
--  dword_sequence utf32 = utf8_to_utf32(string utf8, integer fail_flag=0)
--
--  string utf8 = utf32_to_utf8(dword_sequence utf32, integer fail_flag=0)
--
-- Note that pure ASCII strings (all characters in the range 0..127) are returned unaltered
--  by utf8_to_utf32, with the intention that calls to utf32_to_utf8 can be avoided.
--
-- If fail_flag!=0 the routines yield -1 rather than embed "\#EF\#BF\#BD"/#FFFD.
--  Obviously the lack of an equivalent fail_flag on the following utf16 routines means
--  that any required validation may need to be done via the above (utf8) routines.
--
-- Usage (UTF-16)
-- ==============
--
--  dword_sequence utf32 = utf16_to_utf32(dword_sequence utf16)
--
--  dword_sequence utf16 = utf32_to_utf16(dword_sequence utf32)
--
--  dword_sequence utf16 = utf8_to_utf16(string utf8)
--
--  string utf8 = utf16_to_utf8(dword_sequence utf16)
--

--
-- The following "quick ref for UTF-8" is taken from https://tools.ietf.org/html/rfc3629
--  which you may want to read, along with https://en.wikipedia.org/wiki/UTF-8
--
--     Char number range |  UTF-8 octet sequence
--       (hexadecimal)   |        (binary)
--     ------------------+--------------------------------------
--     00000000-0000007F | 0xxxxxxx
--     00000080-000007FF | 110xxxxx 10xxxxxx
--     00000800-0000FFFF | 1110xxxx 10xxxxxx 10xxxxxx
--     00010000-0010FFFF | 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
--
--  A single head octet (byte) is followed by 0..3 tail bytes.
--  Tail bytes are always of the form 0b10xxxxxx.
--  4 byte encoding is limited to #10FFFF (not #1FFFFF).
--  #D800..#DFFF are invalid/used for UTF-16 surrogate pairs*.
--  Connect the x's of all bytes to get the Unicode codepoint.
--  Using more bits than necessary (long form) is deemed invalid.
--
-- [*] #D800..#DFFF are deemed invalid in all forms of UTF.
--
-- Douglas Crockford rewrites this a little clearer:
--  -Binary- Hex Thru  Range Thru   Continuation    Total
--                                     Bytes    Data Bits
--  0xxxxxxx  00  7F      00 7F          0          7 (7+6*0)
--  10xxxxxx  80  BF <---   continuation   --->     -    6
--  110xxxxx  C0  DF      80 7FF         1         11 (5+6*1)
--  1110xxxx  E0  EF     800 FFFF        2         16 (4+6*2)
--  11110xxx  F0  F7   10000 1FFFFF      3         21 (3+6*3)
--  111110xx  F8  FB  200000 3FFFFFF     4         26 (2+6*4)
--  111111xx  FC  FF 4000000 FFFFFFFF    5         32 (2+6*5)
--
--  (Note these routines respect/implement a limit of #10FFFF, 
--   making the last two lines of that table irrelevant.)
--
--!/**/include builtins\VM\pMath.e  -- (not strictly necessary)

constant INVALID_UTF8 = #FFFD

global function utf8_to_utf32(string utf8, integer fail_flag=0)
--
-- convert a utf8 string to utf32 (one element per character)
--  note result may be string for pure-ascii input (all chars 0..#7F).
--  conversely it /will/ return a dword_sequence when any character is
--  #80 or above, even if said character is still below #FF.
-- by default, invalid/long form characters embed #FFFD in the result.
--
--sequence utf32 = ""
sequence utf32 = repeat(' ',0)
integer  utf8len, i, headb, bytes, c

    utf8len = length(utf8)
    i = 1
    while i<=utf8len do
        -- headb = first byte of utf-8 character:
        headb = utf8[i]

        -- calculate length of utf-8 character in bytes (1..4):
        if    headb<0           then bytes = 0  -- (utf-8 starts at #0)
        elsif headb<=0b01111111 then bytes = 1  -- 0b_0xxx_xxxx
        elsif headb<=0b10111111 then bytes = 0  -- (it's a tail byte)
        elsif headb<=0b11011111 then bytes = 2  -- 0b_110x_xxxx
        elsif headb<=0b11101111 then bytes = 3  -- 0b_1110_xxxx
        elsif headb<=0b11110100 then bytes = 4  -- 0b_1111_0xzz
        else                         bytes = 0  -- (utf-8 ends at #10FFFF)
        end if
        if i+bytes-1>utf8len then
            bytes = 0                           -- end of string too soon
        end if

        -- 2..4 bytes encoding (tail range: 0b_1000_0000..0b_1011_1111);
        for j=1 to bytes-1 do                   -- tail bytes are valid?
            c = utf8[i+j]
            if c<#80 or c>#BF then
                bytes = 0                       -- invalid tail byte
                exit
            end if
        end for

        -- 1 byte encoding (head range: 0b_0000_0000..0b_0111_1111):
        if bytes=1 then
            c = headb                               -- UTF-8 = ASCII

        -- 2 bytes encoding (head range: 0b_1100_0000..0b_1101_1111):
        elsif bytes=2 then
            c = and_bits(headb, #1F)*#40+   -- 0b110[7..11] headb
                and_bits(utf8[i+1], #3F)            -- 0b10[1..6] tail
            if c>#7FF then ?9/0 end if              -- sanity check
            if c<#80 then                           -- long form?
                c = INVALID_UTF8
            end if

        -- 3 bytes encoding (head range: 0b_1110_0000..0b_1110_1111):
        elsif bytes=3 then
            c = and_bits(headb, #0F)*#1000+ -- 0b1110[13..16] head
                and_bits(utf8[i+1], #3F)*#40+   -- 0b10[7..12] tail
                and_bits(utf8[i+2], #3F)            -- 0b10[1..6] tail
            if c>#FFFF then ?9/0 end if             -- sanity check
            if c<#800                               -- long form?
            or (c>=#D800 and c<=#DFFF) then         -- utf-16 incompatible
                c = INVALID_UTF8
            end if

        -- 4 bytes encoding (head range: 0b_1111_0000..0b_1111_0111):
        elsif bytes=4 then
            c = and_bits(headb, #07)*#040000+   -- 0b11110[19..21] head
                and_bits(utf8[i+1], #3F)*#1000+ -- 0b10[13..18] tail
                and_bits(utf8[i+2], #3F)*#0040+ -- 0b10[7..12] tail
                and_bits(utf8[i+3], #3F)            -- 0b10[1..6] tail
            if c<#10000                             -- long form?
            or c>#10FFFF then
                c = INVALID_UTF8                    -- utf-8 ends at #10FFFF
            end if

        -- bytes = 0; (or>4-ish) current byte is not encoded correctly:
        else
            c = INVALID_UTF8
            i += 1                      -- check if the next byte is head
        end if

        if fail_flag=-1 and (bytes=0 or c=INVALID_UTF8) then return -1 end if

        if c<=#FF and bytes=2 and string(utf32) then
            -- force dword sequence (specifically for c in #80..#FF)
            utf32 &= -1
            utf32[$] = c
        else
            utf32 &= c                  -- add code point to string
        end if
        i += bytes                      -- move to next utf-8 character
    end while

    return utf32
end function


constant INVALID_UNICODE = "\#EF\#BF\#BD"

global function utf32_to_utf8(sequence utf32, integer fail_flag=0)
--
-- convert a utf32 sequence (one element per character) to a utf8 string.
-- if utf32 is already string (presumably pure-ascii) it is returned unaltered.
-- by default, invalid unicode points embed "\#EF\#BF\#BD" in the result.
--
    if string(utf32) then return utf32 end if               -- just makes life simpler!

    string utf8 = repeat(' ',0)

    for i=1 to length(utf32) do
        integer u = utf32[i]                                    -- u = Unicode codepoint
        object chr

        if u<0
        or u>#10FFFF
        or (u>=#D800 and u<#DFFF) then
            -- out of utf-8 standard range (#0..#10FFFF) or invalid:
            chr = INVALID_UNICODE

        elsif u<=#7F then
            -- 1 byte encoding (head/only byte: 0b_0xxx_xxxx):
            chr = u                                             -- UTF-8 = ASCII

        elsif u<=#7FF then
            -- 2 bytes encoding (head byte: 0b_110x_xxxx):
            chr = or_bits(floor(u/#40), 0b11000000) &           -- 0b110_7..11 head
                  or_bits(and_bits(u, #3F), 0b10000000)         -- 0b10_1..6 tail

        elsif u<=#FFFF then
            -- 3 bytes encoding (head byte: 0b_1110_xxxx):
            if u>=#D800 and u<=#DFFF then ?9/0 end if
            chr = or_bits(floor(u/#1000), 0b11100000) &         -- 0b1110_13..16 head
                  or_bits(and_bits(floor(u/#40), #3F), #80) &   -- 0b10_7..12 tail
                  or_bits(and_bits(u, #3F), #80)                -- 0b10_1..6 tail

        else -- u<=#10FFFF then (already tested above)
            -- 4 bytes encoding (head byte: 0b_1111_0xxx):
            chr = or_bits(floor(u/#40000), 0b11110000) &        -- 0b11110_19..21 head
                  or_bits(and_bits(floor(u/#1000), #3F), #80) & -- 0b10_13..18 tail
                  or_bits(and_bits(floor(u/#40), #3F), #80) &   -- 0b10_7..12 tail
                  or_bits(and_bits(u, #3F), #80)                -- 0b10_1..6 tail

        end if

        if fail_flag=-1 and chr=INVALID_UNICODE then return -1 end if

        utf8 &= chr
    end for

    return utf8
end function

--
-- The official rfc for UTF-16 can be found at http://www.ietf.org/rfc/rfc2781.txt
--  which you may want to read, along with https://en.wikipedia.org/wiki/UTF-16
--
-- To match the above "quick ref for UTF-8", I present a UTF-16 version:
--
--     Char number range |  UTF-16 16-bit integer sequence
--       (hexadecimal)   |           (binary)
--     ------------------+--------------------------------------
--     00000000-0000FFFF | xxxxxxxx xxxxxxxx
--     00010000-0010FFFF | 0b110110yy yyyyyyyy, 0b110111zz zzzzzzzz
--
--  Note: Values between 0xD800 and 0xDFFF are specifically reserved for
--        use with UTF-16, and don't have any characters assigned to them.
--  The leading/high surrogate is a value in the range #D800 to #DBFF.
--  The trailing/low surrogate is a value in the range #DC00 to #DFFF.
--  It is deemed invalid if any leading/high surrogate is not immediately 
--  followed by a trailing/low surrogate, and likewise if any trailing/low 
--  surrogate is not immediately preceded by a leading/high surrogate.
--  Connect the y's and z's and add #10000 to get the Unicode codepoint.
--  Conversely subtract #10000 from the range 00010000-0010FFFF, to get a
--  20-bit value 00000000-000FFFFF, then put 10 bits in each surrogate.
--
-- Is this a better way of saying it?
--  Unpaired surrogates are invalid in UTFs. These include any value 
--  in the range #D800 to #DBFF not followed by a value 
--  in the range #DC00 to #DFFF, or any value 
--  in the range #DC00 to #DFFF not preceded by a value 
--  in the range #D800 to #DBFF.
--

global function utf16_to_utf32(sequence utf16)
--
-- The parameter utf16 is usually a dword_sequence result from peek2u(), or
--  something from file, or (for testing) a result of utf32_to_utf16().
--  If the latter returns a string, it is suitable for direct poke2(), and
--  should not be passed to this routine, as doing so achieves very little.
--  Any UTF-16LE/UTF-16BE handling differences should have already happened.
-- Invalid input triggers a fatal error (string inputs are however fine).
-- The result is always suitable for passing to utf32_to_utf8(); there is 
--  no special "pure ascii"/string case to consider here.
--
sequence utf32 = {}
integer i, ch, ch2
--  if string(utf16) then ?9/0 end if   -- (just not particularly helpful)
    i = 1
    while i<=length(utf16) do
        ch = utf16[i]
        if ch<0
        or ch>#FFFF then
--          ?9/0
            utf32 = append(utf32,#FFFD)
        elsif ch>=#D800 and ch<=#DBFF then
            i += 1
            ch2 = utf16[i]
            if ch2>=#DC00 and ch2<=#DFFF then
                utf32 = append(utf32,#00010000+and_bits(ch,#3FF)*#400+and_bits(ch2,#3FF))
            else
--              ?9/0                        -- missing trailing/low surrogate
                utf32 = append(utf32,#FFFD)
            end if
        elsif ch>=#DC00 and ch<=#DFFF then
--          ?9/0                            -- missing leading/high surrogate
            utf32 = append(utf32,#FFFD)
        else -- ch<=#FFFF then
            utf32 = append(utf32,ch)
        end if
        i += 1
    end while
    return utf32
end function

global function utf32_to_utf16(sequence utf32)
--
-- utf32 is usually a dword_sequence result from utf8_to_utf32().
--  If that routine returns a string, it is suitable for direct poke2(), and
--  should not be passed to this routine, as doing so achieves very little.
-- Invalid input triggers a fatal error (string inputs are however fine).
-- The result is really only suitable for poke2() or writing to file.
-- Any distinction between UTF-16LE and UTF-16BE is down to use of result.
--
sequence utf16 = {}
integer ch
--  if string(utf16) then ?9/0 end if   -- (just not particularly helpful)
    for i=1 to length(utf32) do
        ch = utf32[i]
        if ch<0
        or (ch>=#D800 and ch<#DFFF)
        or ch>#10FFFF then
--          ?9/0
            utf16 = append(utf16,#FFFD)
        elsif ch<#FFFF then
            utf16 = append(utf16,ch)
        else
            ch -= #10000
            utf16 = append(utf16,#D800+floor(ch/#400))
            utf16 = append(utf16,#DC00+and_bits(ch,#3FF))
        end if
    end for
    return utf16
end function


global function utf8_to_utf16(string utf8)
    return utf32_to_utf16(utf8_to_utf32(utf8))
end function

global function utf16_to_utf8(sequence utf16)
    return utf32_to_utf8(utf16_to_utf32(utf16))
end function

--/* --maybe: (cld/shd extensively test via utf32 up to #10FFFF (see below)
-- Here is a little function to report an UTF-8 string length.
-- It works also with ASCII strings so it could replace length(). [nah...]

function ulength(sequence s)
integer res = 0,
          i = 1,
          lg = length(s)
    if lg<2 then return length(s) end if
    while i<=lg do
        integer si = s[i]
        if    and_bits(si,#80)=#00 then i += 1
        elsif and_bits(si,#E0)=#C0 then i += 2
        elsif and_bits(si,#F0)=#E0 then i += 3
        elsif and_bits(si,#F8)=#F0 then i += 4
        else                            i += 1
        end if
        res += 1
    end while
    return res
end function

-- And here is un UTF-8 compliant head() function. It works also with ASCII strings.
function uhead(sequence s, integer n)
sequence res
integer i = 1,
          lg = length(s),
          ul = 0
    if lg<2 then return head(s, n) end if
    while i<=lg do
        integer si = s[i]
        if    and_bits(si,#80)=#00 then i += 1
        elsif and_bits(si,#E0)=#C0 then i += 2
        elsif and_bits(si,#F0)=#E0 then i += 3
        elsif and_bits(si,#F8)=#F0 then i += 4
        else                            i += 1
        end if
        ul += 1
        if ul=n then
            s = s[1..i-1]
            exit
        end if
    end while
    return s
end function

From: https://rosettacode.org/wiki/Idiomatically_determine_all_the_characters_that_can_be_used_for_symbols#Phix
(something like this, perhaps... not that we need system_exec(), you understand, just the "to #10FFFF" part)
function run(string ident)
    integer fn = open("test.exw","w")
    printf(fn,"object %s",ident)
    close(fn)
    return system_exec("p -batch test.exw")
end function
 
function check(integer lo, hi)
    string ok1 = "", ok2 = ""
    integer ng1 = 0, ng2 = 0
    for ch=lo to hi do
        printf(1,"%d/%d...\r",{ch,hi})
        if find(ch,"\t\r\n \0\x1A;") then
            ng1 += 1
            ng2 += 1
        else
            string c = sprintf("%c",ch)
            if run(c)==0 then ok1 &= c else ng1 += 1 end if
            if run("_"&c)==0 then ok2 &= c else ng2 += 1 end if
        end if
    end for
    return {{ng1,length(ok1),ok1},
            {ng2,length(ok2),ok2}}
end function
 
sequence r = check(0,127)
printf(1,"ansi characters:\n===============\n")
printf(1,"1st character: %d bad, %d OK %s\n",r[1])
printf(1,"2nd..nth char: %d bad, %d OK %s\n\n",r[2])
r = check(128,255)
integer ok8 = 0, ng8 = 0
for i=#80 to #10FFFF do
    if i<#D800 or i>#DFFF then
        printf(1,"#%x/#10FFFF...\r",i)
        string utf8 = utf32_to_utf8({i})
        bool ok = true
        if not find(utf8[1],r[1][3]) then
            ok = false
        else
            for j=2 to length(utf8) do
                if not find(utf8[j],r[2][3]) then
                    ok = false
                    exit
                end if
            end for
        end if
        if ok then
            ok8 += 1
        else
            ng8 += 1
        end if
    end if
end for
printf(1,"utf8 characters:   \n===============\n")
printf(1,"bad:%,d, good:%,d\n",{ng8,ok8})
Output:
ansi characters:
===============
1st character: 75 bad, 53 OK ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz
2nd..nth char: 65 bad, 63 OK 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz

utf8 characters:
===============
bad:0, good:1,111,936
--*/
