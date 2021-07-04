--
-- Pete's Pretty Print. (Phix compatible)
-- ===================
-- Author Pete Lomax May 2003
-- thanks to Juergen Luethje for help and suggestions.
--           Revised August 2003
--           Revised October 2003
--           Revised September 2004
--           Revised June 2010
--
--   04/06/10: Added pp_Q22 option
-- Version 2.0.4
--   27/03/09: Updated for auto-include, string handling.
--   12/12/04: added escape char handling to strings (see escBytes)
-- Version 2.0.3
--   07/09/04: pp_IntFmt applies to 32-bit integers not just 31-bit
--      (#80000000..#7FFFFFFF not just #C0000000..#3FFFFFFF)
--Ahem: -#FFFFFFFF..#FFFFFFFF as requested by JL.
--
-- Version 2.0.1
--   05/06/04: float format default changed to %g (was %3.2g)
--
--!/**/without debug -- keep ex.err clean (remove to debug!)
--!/**/with debug
--  NB: the "without debug" in pdiag.e overshadows the one here; you may
--      need to "with debug" here to get a listing, or use -nodiag.
--without trace -- (covered by without debug)

--
-- Introduction.
-- ============
--
--  Prints any object to screen, file, or returns a string.
--  Long sequences are automatically line-wrapped and optionally 
--  displayed in a neat (vertically aligned) nested format.
--
--  Default handling.
--  ================
--  All these settings can be changed:
--   Display to screen (ie file 1, aka stdout).
--   Lines are wrapped at 78 characters.
--   Display is paused every 23 lines.
--   Strings are printed as double-quoted ascii, eg "abc".
--<   Values in the range #20 to #7F are treated as ascii.
--    Values in the range #20 to #7D are treated as ascii.
--   Integers are printed using a "%d" format.
--   Floating point numbers are printed using a "%g" format.
--   Date handling is turned off by default.
--   Sequences are enclosed in curly braces '{' and '}'.
--
--  The default behaviour can be changed permanently using ppOpt,
--  or temporarily using ppEx(object,<option-list>).
--
--  Interface.
--  =========    
--   pp(object)     Output using the current settings.
--                  This is the way this routine is normally used.
--
--   x=ppf(object)  Return a string representation, (with embedded \n if 
--                  needed), rather than display to screen or write to file.
--                  Note there is no trailing \n on the result
--
--   ppOpt(options) Permanently (or until next ppOpt call) change
--                  selected formatting options (see below).
--
--   ppEx(object,options) Temporarily apply specified options while
--                  printing object. Defaults are reset before exit.
--
--   x=ppExf(object,options) Return a string representation using the
--                  specified temporary formatting options.
--
--  Formatting options.
--  ==================
--   Example:
--
--      ppOpt({pp_IntFmt,"%d.00 US$"})
--      ppOpt({pp_FltFmt,"%.2f US$"})
--
--      x=ppf(1) -- returns "1.00 US$"
--
--   Notes:
--    When using this routine to format values which may or may not have a
--    fractional element, both the integer and float formats should be set.
--    The following one line can be used instead of the above:
--
--      x=ppExf(1,{{pp_IntFmt,"%d.00 US$",pp_FltFmt,"%.2f US$"})
--
--   The options are specified as a pair-sequence, selected from:
--
--         Odd element:    Even element:
--          pp_File         0:no output (used by ppf())
--                          1:display to Screen (the default),
--                          n:an open file handle
--          pp_Nest         nest level, eg:
--                          0: output is {1, {2, {3,3}, 2}, 1}
--                          1: output is {1,
--                                        {2, {3,3}, 2},
--                                        1}
--                          See below for more examples.
--          pp_Maxlen       Split lines at column, default 78
--          pp_Indent       Auto-indent amount, default 0
--          pp_Pause        Pause every n lines, default=23.
--                          Specify 0 for no pause
--          pp_StrFmt       0: print strings as eg "abc" (default)
--                         -1:  ditto, but without quotes.
--                         -2:  as 0, but chars number-only like +1                 [DEPRECATED: use pp_IntCh,false instead]
--                         -3:  as -1, ""                                           [ "" (and pp_StrFmt,-1)]
--                          1: as number only, eg {97, 98, 99}                      [DEPRECATED: use pp_StrFmt,3,pp_IntCh,false instead]
--                          3: as number&text, eg {97a, 98b, 99c}
--          pp_IntCh        show ints as eg 65'A', default true (false==just 65)
--          pp_IntFmt       Integer format, default "%d"
--                          NB: applies to pp_StrFmt 1 as well.
--          pp_FltFmt       Float format, default "%.10g"
--                          Both pp_IntFmt and pp_FltFmt can contain extra text,
--                          eg "#%08x (integer)", "%04.1 km", or "%.2f US$"
--          pp_Ascii        Min/max ascii character, default {' ',#7D}
--                          If sequences are passed, they define ranges.
--                          Eg, for ISO 8859-1, use {{#20,#A0},{#7E,#FF}},
--                          since characters 0..31 and 127..159 are system codes.
--                          (Note it is {start1,start2,...},{end1,end2,...})
--          pp_Date         "": no special date handling (Default)
--                          "%04d-%02d-%02d": eg 2003-12-31
--                          "%02d/%02d/%04d": eg 31/12/2003
--                          Sequences of 3 integers, in the form {day,month,year} or
--                          {year,month,day}, with 1<=day<=31, 1<=month<=12, and
--                          1900<=year<=3000 are recognised as dates.
--          pp_Brkt         "{}" (default) outputs '{' at the start of a (nested)
--                          sequence and '}' at the end. Must be two characters.
--          pp_Q22          Show quotes as #22 (default 0, ie \")
--
--   eg ppOpt({pp_StrFmt,0,pp_Nest,2})
--
-- More Examples of pp_Nest:
-- ========================
--
-- obj={1,{2,{3,3},2},1}
-- ppOpt({pp_Nest,0})
-- pp(obj) gives: 
-- 
-- {1, {2, {3,3}, 2}, 1}
--
-- ppEx(obj,{pp_Nest,1}) gives:
--
-- {1,
--  {2, {3,3}, 2},
--  1}
--
-- ppEx(obj,{pp_Nest,2}) gives:
--
-- {1,
--  {2,
--   {3,3},
--   2},
--  1}
--
-- nest may also be a sequence of {indent,nest_level}.
--  In the above examples, note how the 1, {, and 1 display in the 
--  same column. If an indent level is specified, spaces are inserted 
--  in the "{1" to maintain alignment.
--
-- ppEx(obj,{pp_Nest,{2,2}}) gives:
--
-- { 1,
--   { 2,
--     { 3,3},
--     2},
--   1}
--
-- ppEx(obj,{pp_Nest,{3,2}}) gives:
--
-- {  1,
--    {  2,
--       {  3,3},
--       2},
--    1}
--
-- Naturally, some experimentation may be required to find the 
-- indent/nest combination which works best with your data.
--
-- Also, specifying a nest of -1 suppresses the trailing \n
--
-- Programming notes
-- =================
--
--    no spaces in innermost sequences:
--          if atom(cl[i]) and (i=length(cl) or atom(cl[i+1])) then
--              -- as req by Juergen Luethje
--              sep=","
--          else
--              sep=", "
--          end if
--
-- If you are interested in how it works, (obviously you need to at 
-- least skim the rest), the key to understanding is in the recursion:
--
--  function prnf(object cl, integer col, integer indent, integer prnt, integer nestlvl)
--          len+=prnf(cl[i],col+len,col+nindent,[0|prnt],nestlvl-1)
--
--  Briefly, cl is the object being printed,
--  col & indent are column and indent with a few subtle twists ;-),
--  prnt=0 allows the routine to find out long it would be if printed, so it can make
--   an informed decision whether it fits or if it needs to break the line,
--  and nestlvl is the nest level, decremented as we recurse.
--
--  Otherwise it is pretty trivial - there is more glue round puts() 
--  and to set up the parameters than anything else.
--
--DEV wrong one for newEmit
--!/**/include builtins\pprntf.e -- (not strictly necessary, but saves on opCallOnce/fwd calls/onDeclaration)

integer ppp_Maxlen = 78     -- break lines longer than this
integer ppp_Indent = 0      -- auto-indent all lines this much
object  ppp_Nest = 0        -- nest level
integer ppp_Pause = 0       -- pause display after this many lines, 0=never pause
integer ppp_StrFmt = 0      -- 0=text as strings, -1 without quotes,
                            -- 3 as numbers [each as per pp_IntCh]
--3/4/21
--integer ppp_IntCh = true  -- ints as eg 65'A' (false=just 65, (-1 and ' '..'~')=just 'A')
integer ppp_IntCh = false   -- ints as eg 65 (true=65'A', (-1 with ' '..'~')=just 'A')

integer ppp_Init =  0

object  ppp_Ascii --={#20,#7D}  -- low ascii [ranges]
sequence ppp_IntFmt --="%d"     -- integer display format
sequence ppp_FltFmt --="%.10g"  -- float display format
sequence ppp_Date  --=""        -- date handling
sequence ppp_Br --="{}"         -- Bracket characters
integer ppp_Q22 = 0     -- Show quotes as #22 (default 0)

sequence ascii

integer ppp_File = 1        -- default: display to Screen

--!/**/  string ppp_result --/*
sequence ppp_result         -- print result if ppp_File is 0
--!*/

--/* Not required for Phix (see psym.e routine syminit)
global constant pp_File     = 1,
                pp_Maxlen   = 2,
                pp_Indent   = 11,
                pp_Pause    = 3,
                pp_StrFmt   = 4,
                pp_IntFmt   = 5,
                pp_FltFmt   = 6,
                pp_Nest     = 7,
                pp_Ascii    = 8,
                pp_Date     = 9,
                pp_Brkt     = 10,
                pp_Q22      = 12,
                pp_IntCh    = 13
--*/
--DEV temp:
--constant pp_IntCh  = 13

--DEV:
--!/**/ string pline --/*
sequence pline          -- output line, as built by sput()
--!*/
integer  plen  = 0      -- used part of pline; rest is garbage
integer  sline = 0      -- counter for screen line
integer  nindent        -- indent increment when nesting

procedure spurge()
--
-- display/concatenate the output built so far
--
    if ppp_File then
        puts(ppp_File,pline[1..plen])
    else
        ppp_result &= pline[1..plen]
    end if
    plen = 0
end procedure

--integer dieonceonly
--      dieonceonly=1

--/*
constant M_WAIT_KEY = 26
--*/

procedure sput(object txt)
--
-- bundle "puts(1," calls together for output one line at a time.
--
    integer p = plen+1
    if atom(txt) then
        plen += 1
    else
--if dieonceonly then
------/**/if not string(txt) then ?9/0 end if    --DEV
--dieonceonly=0
--end if
        plen += length(txt)
    end if
    if plen>length(pline) then
        pline &= repeat(' ',plen-length(pline))
    end if
    pline[p..plen] = txt
    if pline[plen]='\n' then
        spurge()
        if ppp_File=1 and ppp_Pause then
            sline += 1+(plen>ppp_Maxlen)
            if sline>=ppp_Pause then
--/**/          if wait_key() then end if                   --/*    -- Phix
                if machine_func(M_WAIT_KEY,0) then end if   --      -- RDS --*/
                sline = 0
            end if
        end if
    end if
end procedure

sequence escBytes,escChars
--constant escBytes = "\t\n\r\\\"\'",
--       escChars = "tnr\\\"\'"

sequence constants
--constant constants = {-1.295837195871e307,"NOVALUE"}

--constant maxgraphic = iff(platform()=LINUX?#7E:255)

function graphic(object cl)
    if integer(cl)
    and cl>=1
--  and cl>=' '
    and cl<=255
--DEV fixme:
--  and cl<=iff(platform()=LINUX?#7E:255)
--  and cl<=maxgraphic
--30/1/18:
--  and ((platform()=LINUX and cl<=#7E) or
--       (platform()=WINDOWS and cl<=255))
--  and cl<=#7E
--  and cl<='}'
    and ascii[cl] then
        return 1
    end if
    return 0
end function

without warning -- suppress short-circuit warning

integer cl1q = 0        -- cl[1] was '"'
integer cllq = 0        -- cl[-1] was '"'

function prnf(object cl, integer col, integer indent, integer prnt, integer nestlvl)
    integer len, iplus1, k, ch
    integer aschar, asbacktick
    sequence sep,txt
    object chint
    if sequence(cl) then
--23/11/19: (I just never want to see {"this",{9,10}} as {"this","\t\n"} ever again, thank you very much!)
--      if ppp_StrFmt<=0 then
        if string(cl) and ppp_StrFmt<=0 then

--put back 26/4/2020:
--/**/      aschar = string(cl)             --/* -- Phix
            aschar = (length(cl)>0)         --*/ -- RDS Eu:
            asbacktick = true

            for i=1 to length(cl) do
                chint = cl[i]
-- 15/3/2010:
----                if ppp_StrFmt=0 then
--              if chint<' ' then   -- "\r\n\t" or escape chars "&'
----                    if chint<' ' or find(chint,escBytes) then   -- "\r\n\t" or escape chars "&'
--                  if not find(chint,escBytes) then    -- "\r\n\t" or escape chars "&'
--                      aschar = 0
--                      exit
--                  end if
--                  aschar += 1
--              end if
--              if find(chint,escBytes) then    -- "\t\n\r\\\"\'"
                k = find(chint,escBytes)        -- "\t\n\r\\\"\'"
                if k then
                    aschar += 1
                    if k<3 then asbacktick = false end if
                    if ppp_Q22
                    and chint=#22 then  -- ie '"'
                        -- \"blah ==> #22&"blah
                        -- blah\"blah ==> blah"&#22&"blah
                        -- blah\" ==> blah"&#22
                        if i=1 or i=length(cl) then
                            aschar += 4
                        else
                            aschar += 5
                        end if
                    end if
                elsif chint<' ' then
                    aschar = 0
                    exit
                elsif not graphic(chint) then
                    aschar = 0
                    exit
                elsif chint='`' then
                    asbacktick = false
                end if
            end for
            if aschar then
--              len = length(cl)+(aschar-1) -- allow for escape chars
                len = length(cl)
                if not asbacktick then
                    len += (aschar-1) -- allow for escape chars
                end if
                if prnt then
                    if aschar>1     --replace escape chars
                    and not asbacktick then
                        for i=length(cl) to 1 by -1 do
                            ch = cl[i]
                            if find(ch,escBytes) then -- escape chars
                                if ppp_Q22
                                and ch=#22 then -- ie '"'
                                    -- \"blah ==> #22&"blah
                                    -- blah\"blah ==> blah"&#22&"blah
                                    -- blah\" ==> blah"&#22
                                    if i=1 then
--                                      cl = "#22&\""&cl[2..length(cl)]
                                        cl = `#22&"`&cl[2..length(cl)]
--                                      if ppp_StrFmt!=-1
--                                      and ppp_StrFmt!=-3 then
                                        if ppp_StrFmt!=-1 then
                                            cl1q = 1
                                        end if
                                    elsif i=length(cl) then
--                                      cl = cl[1..i-1]&"\"&#22"
                                        cl = cl[1..i-1]&`"&#22`
--                                      if ppp_StrFmt!=-1
--                                      and ppp_StrFmt!=-3 then
                                        if ppp_StrFmt!=-1 then
                                            cllq = 1
                                        end if
                                    else
--                                      cl = cl[1..i-1]&"\"&#22&\""&cl[i+1..length(cl)]
                                        cl = cl[1..i-1]&`"&#22&"`&cl[i+1..length(cl)]
                                    end if
--                              else
--22/6/19 (-3 is now deprecated, though was not entirely sure what this was meant to be doing anyways...)
--                              elsif ppp_StrFmt!=-3 then
                                else
                                    cl[i] = '\\'
                                    ch = escChars[find(ch,escBytes)]
                                    cl = cl[1..i]&ch&cl[i+1..length(cl)]
                                end if
                            end if
                        end for
                    end if
--22/6/19:
--                  if ppp_StrFmt=-1
--                  or ppp_StrFmt=-3 then
                    if ppp_StrFmt=-1 then
                        sput(cl)
                    elsif asbacktick then
                        sput('`'&cl&'`')
                    else
                        if ppp_Q22 and cl1q then
                            cl1q = 0
                            if cllq then
                                sput(cl)
                                cllq = 0
                            else
                                sput(cl&'"')
                            end if
                        elsif ppp_Q22 and cllq then
                            cllq = 0
                            sput('"'&cl)
                        else
                            sput('"'&cl&'"')
                        end if
                    end if
                end if
--              if ppp_StrFmt!=-1
--              and ppp_StrFmt!=-3 then
                if ppp_StrFmt!=-1 then
                    len += 2
                end if
                return len
            end if
        end if
        -- date handling added for George Walters 12/10/2003
        if length(ppp_Date) and length(cl)=3
        and integer(cl[1]) and integer(cl[2]) and integer(cl[3])
        and ((cl[1]>=1 and cl[1]<=31 and cl[3]>=1900 and cl[3]<=3000) or
             (cl[3]>=1 and cl[3]<=31 and cl[1]>=1900 and cl[1]<=3000))
        and (cl[2]>=1 and cl[2]<=12) then
            txt = sprintf(ppp_Date,cl)
            if prnt then sput('"'&txt&'"') end if
            return length(txt)+2
        end if
        len = nindent
        if prnt then
            txt = repeat(' ',nindent)
            txt[1] = ppp_Br[1]
            sput(txt)
        end if
        if nestlvl<0 then nestlvl = 0 end if
        sep = ""
        for i=1 to length(cl) do
            if col+len>nindent*(ppp_Nest[2]+1-nestlvl) then
                if (i>1 and nestlvl>0)
                or ppp_Maxlen<=prnf(cl[i],col+len,col+nindent,0,nestlvl-1)
                               +col+len+length(sep)+2+ppp_Indent then
                    if not prnt then return ppp_Maxlen+1 end if -- force linebreak
                    if length(sep) then
                        sput(",\n")
                        sput(repeat(' ',indent+nindent+ppp_Indent))
                    end if
                    len = nindent
                    col = indent
                    sep = ""
                end if
            end if
            if prnt and length(sep) then sput(sep) end if
            len += length(sep)
            len += prnf(cl[i],col+len,col+nindent,prnt,nestlvl-1)
            iplus1 = i+1
            if atom(cl[i]) and (i=length(cl) or atom(cl[iplus1])) then
                -- as req by Juergen Luethje
                sep = ","
            else
                sep = ", "
            end if
        end for
        if prnt then sput(ppp_Br[2]) end if
        return len+1
    end if
    if integer(cl)
    or (cl>=-#FFFFFFFF and cl<=#FFFFFFFF and cl=floor(cl)) then
        if graphic(cl)
        and ppp_IntCh
        and (not find(cl,escBytes) or cl>=' ') then
--22/6/19:
--          if ppp_StrFmt=1
--          or ppp_StrFmt=-2
--          or ppp_StrFmt=-3 then
--              txt = sprintf(ppp_IntFmt,cl)
--30/1/18:
--          elsif find(cl,escBytes) then
            if find(cl,escBytes) then
                -- (just `"'\`, aka "\"\'\\", not "\r\n\t\e\0":)
--              txt = sprintf("%d'\\%s'",{cl,cl})
                txt = sprintf(`%d'\%s'`,{cl,cl})
            elsif ppp_IntCh=-1 and cl<='~' then
                txt = sprintf(`'%s'`,{cl})
            else
                txt = sprintf("%d'%s'",{cl,cl})
            end if
--3/12/18:
--      elsif cl<0 then
        elsif cl<0 and ppp_IntFmt[1]!='%' then
            -- we may want eg -$14, not $-14
            txt = '-'&sprintf(ppp_IntFmt,-cl)
        else
            txt = sprintf(ppp_IntFmt,cl)
        end if
    else
        k = find(cl,constants)
        if k then
            k += 1
            txt = constants[k]
--25/11/18:
--      elsif cl<0 then
--      elsif cl<0 and ppp_FltFmt[2]='.' then
        elsif cl<0 and ppp_FltFmt[1]!='%' then
            -- we may want eg -$14.99, not $-14.99
            txt = '-'&sprintf(ppp_FltFmt,-cl)
        else
            txt = sprintf(ppp_FltFmt,cl)
        end if
    end if
    if prnt then sput(txt) end if
    return length(txt)
end function
with warning

--procedure pp_fatal(sequence msg)
--  puts(1,"ppp.e: pp_fatal: "&msg&"\nPress d for diagnostics...")
--  if find(getc(0),"dD") then ?9/0 end if
----    if find(getc(0),"dD") then crash("?9/0") end if
--  abort(1)
--end procedure
--bool warned = false
--procedure warn(string msg)
procedure pp_fatal(string msg, integer level)
--  puts(1,msg)
--  warned = true
--18/12/20:
--/*
    crash_message(msg)
    #ilASM{ 
            [32]
                mov ecx,[level] -- no of frames to pop to obtain an era
            [64]
                mov rcx,[level] -- no of frames to pop to obtain an era
            []
--              mov al,2        -- (we don't rightly care, /0'll do)
                mov al,68       -- e68crash
                jmp :!fatalN    -- fatalN(level,errcode,ep1,ep2)
          }
--  ?9/0
--*/
    crash(msg,nFrames:=level)
--  crash(msg,{},level) -- pwa/p2js: JavaScript does not support named parameters... [but p2js now does!]
end procedure

procedure setAscii()
    ascii = repeat(0,255)
    if length(ppp_Ascii)!=2 then pp_fatal("length(ascii) must be 2",4) end if
    object {minasc,maxasc} = ppp_Ascii
    if not sequence(minasc) then minasc = {minasc} end if
    if not sequence(maxasc) then maxasc = {maxasc} end if
    if length(minasc)!=length(maxasc) then pp_fatal("length(minasc)!=length(maxasc)",4) end if
    if find(0,minasc) or find(0,maxasc) then pp_fatal("find(0,minasc) or find(0,maxasc)",4) end if
    for i=1 to length(minasc) do
        ascii[minasc[i]..maxasc[i]] = 1
    end for
    for i=1 to length(escBytes) do
        ascii[escBytes[i]] = 1
    end for
end procedure

procedure pp_Init()
    ppp_Nest = 0
--simplified 24/11/16:
--  ppp_Ascii = {#20,#FF}
--DEV/SUG
--  if platform()=LINUX then
--      ppp_Ascii = {{#20,#A0},{#7E,#FF}}
--      ppp_Ascii = {#20,#7F}
        ppp_Ascii = {#20,#7D}
--  end if
    ppp_IntFmt = "%d"
    ppp_FltFmt = "%.10g"
    ppp_Date = ""
    ppp_Br = "{}"
--  escBytes = "\t\n\r\\\"\'\e\E"
--  escBytes = {'\t','\n','\r','\\','\"','\'','\e','\E'}
    escBytes = {9,'\n','\r','\\','\"','\'','\e','\E'}
    escChars = "tnr\\\"\'eE"
--DEV you should have to explicitly load this sort of thing with a ppOpt() call...
    constants = {-1.295837195871e307,"NOVALUE"}
    setAscii()
    ppp_Init = 1
end procedure

function setOpt(sequence options)
    integer f, ip1, flvl = 4
    object tmp
    if not ppp_Init then pp_Init() end if
    if and_bits(1,length(options)) then pp_fatal("length(options) not even",flvl) end if
    options = deep_copy(options)
    for i=1 to length(options) by 2 do
        f = options[i]
        ip1 = i+1
        tmp = options[ip1]
        if f=pp_File then
            options[ip1] = ppp_File
            ppp_File = tmp
        elsif f=pp_Maxlen then
            options[ip1] = ppp_Maxlen
            if tmp=0 then tmp = 78 end if
            ppp_Maxlen = tmp
        elsif f=pp_Indent then
            options[ip1] = ppp_Indent
            ppp_Indent = tmp
        elsif f=pp_Pause then
            options[ip1] = ppp_Pause
            ppp_Pause = tmp
        elsif f=pp_StrFmt then
            options[ip1] = ppp_StrFmt
            ppp_StrFmt = tmp
            if tmp=-2 then
                pp_fatal("pp_StrFmt,-2 deprecated: use pp_IntCh,false instead",flvl)
            elsif tmp=-3 then
                pp_fatal("pp_StrFmt,-3 deprecated: use pp_StrFmt,-1,pp_IntCh,false instead",flvl)
            elsif tmp=1 then
                pp_fatal("pp_StrFmt,1 behaves as 3: use pp_StrFmt,3,pp_IntCh,false instead",flvl)
            end if
--             -2:  as 0, but chars number-only like +1                 [DEPRECATED: use pp_IntCh,false instead]
--             -3:  as -1, ""                                           [ "" (and pp_StrFmt,-1)]
--              1: as number only, eg {97, 98, 99}                      [DEPRECATED: use pp_StrFmt,3,pp_IntCh,false instead]
        elsif f=pp_IntFmt then
            options[ip1] = ppp_IntFmt
            ppp_IntFmt = tmp
        elsif f=pp_IntCh then
            options[ip1] = ppp_IntCh
            ppp_IntCh = tmp
        elsif f=pp_FltFmt then
            options[ip1] = ppp_FltFmt
            ppp_FltFmt = tmp
        elsif f=pp_Nest then
            options[ip1] = ppp_Nest
            ppp_Nest = tmp
        elsif f=pp_Ascii then
            options[ip1] = ppp_Ascii
            ppp_Ascii = tmp
            setAscii()
        elsif f=pp_Date then
            options[ip1] = ppp_Date
            ppp_Date = tmp
        elsif f=pp_Brkt then
            options[ip1] = ppp_Br
            ppp_Br = tmp
        elsif f=pp_Q22 then
            options[ip1] = ppp_Q22
            ppp_Q22 = tmp
        end if
    end for
    return options
end function

global procedure ppOpt(sequence options)
-- Permanently (or until next ppOpt call) change selected formatting options.
-- options is a pair-sequence, selected from:
--   Odd element:      Even element:
--      pp_File         0:no output (used by ppf())
--                      1:display to Screen (the default),
--                      n=an open file handle
--      pp_Nest         nest level, eg:
--                      0: output is {1, {2, {3,3}, 2}, 1}
--                      1: output is {1,
--                                    {2, {3,3}, 2},
--                                    1}
--      pp_Maxlen       split lines at column, default 78
--      pp_Indent       Auto-indent amount, default 0
--      pp_Pause        pause every n lines, default=23.
--                      specify 0 for no pause
--      pp_StrFmt       0: print strings as eg "abc" (default)
--                     -1:  ditto, but without quotes.
--                     -2:  as 0, but chars number-only like +1                 [DEPRECATED: use pp_IntCh,false instead]
--                     -3:  as -1, ""                                           [ "" (and pp_StrFmt,-1)]
--                      1: as number only, eg {97, 98, 99}                      [DEPRECATED: use pp_StrFmt,3,pp_IntCh,false instead]
--                      3: as number&text, eg {97a, 98b, 99c}
--      pp_IntCh        show ints as eg 65'A', default true (false==just 65)
--      pp_IntFmt       integer format, default "%d"
--                      NB: applies to pp_StrFmt 1 as well.
--      pp_FltFmt       float format, default "%g"
--                      Both pp_IntFmt and pp_FltFmt can contain extra text,
--                      eg "#%08x (integer)", "%04.1 km", or "%.2f US$"
--
--      pp_Ascii        min/max ascii character, default {' ',#7D}
--                      if sequences are passed, they define ranges.
--                      Eg, for ISO 8859-1, use {{#20,#A0},{#7E,#FF}},
--                      since characters 0..31 and 127..159 are system codes.
--
--      pp_Date         "": no special date handling (Default)
--                      "%04d-%02d-%02d": eg 2003-12-31
--                      "%02d/%02d/%04d": eg 31/12/2003
--                      Sequences of 3 integers, in the form {day,month,year} or
--                      {year,month,day}, with 1<=day<=31, 1<=month<=12, and
--                      1900<=year<=3000 are recognised as dates.
--
--      pp_Brkt         "{}" (default) outputs '{' at the start of a (nested)
--                      sequence and '}' at the end. Must be two characters.
--
--      pp_Q22          Show quotes as #22 (default 0, ie \")
--
--   eg ppOpt({pp_StrFmt,0,pp_Nest,2})
--
    options = setOpt(options)
end procedure

-- internal, so that pp() and ppEx() have same nesting/fatal error level:
procedure pp_Ex(object o, sequence options)
-- pretty print object with selected options (as per ppOpt)
-- The previous pretty_print options are restored on exit.
    options = setOpt(options)
    pline = ""
    ppp_result = ""
    if not sequence(ppp_Nest) then
        ppp_Nest = {1,ppp_Nest}
    end if
    nindent = ppp_Nest[1]
    if prnf(o,0,0,1,ppp_Nest[2]) then end if
    if ppp_Nest[2]>=0 then
        sput("\n")
    elsif plen then
        spurge()
    end if
    options = setOpt(options) -- restore
end procedure

global procedure ppEx(object o, sequence options)
-- pretty print object with selected options (as per ppOpt)
-- The previous pretty_print options are restored on exit.
    pp_Ex(o,options)
end procedure

global procedure pp(object o, sequence options={})
-- pretty print
-- For options see ppEx, ppOpt, and ppExf
    pp_Ex(o,options)
end procedure

global function ppExf(object o, sequence options)
-- return object pretty printed, with options (as per ppOpt)
-- The previous pretty_print options are restored on exit.
    for i=1 to length(options) by 2 do
        if options[i]=pp_Nest then
            ppEx(o,options&{pp_File,0})
            return ppp_result
        end if
    end for
    ppEx(o,options&{pp_File,0,pp_Nest,-1})
    return ppp_result
end function

global function ppf(object o, sequence options={})
-- return object pretty printed (with embedded \n), no trailing \n
    return ppExf(o,options)
end function

--sequence s 
--s={{{"blue", "red"}, {"cyan", "white"}},
--   {{"blue", "red"}, {"cyan", "white"}}}
--
---- s={1,{2,{3,3},2},1}
--ppEx(s,{pp_Nest,0})
--puts(1, "---------------------------\n")
--ppEx(s,{pp_Nest,1})
--puts(1, "---------------------------\n")
--ppEx(s,{pp_Nest,2})
--puts(1, "---------------------------\n")
--ppEx(s,{pp_Nest,{2,2}})
--puts(1, "---------------------------\n")
--ppEx(s,{pp_Nest,3})
--if getc(0) then end if
--s={{1, 2, 3, 4}, {1, 2, 4, 3}, {1, 3, 2, 4}, {1, 3, 4, 2}, {1, 4, 2, 3},
-- {1, {4, 3}, 2}, {2, 1, 3, 4}, {2, 1, 4, 3}, {2, 3, 1, 4}, {2, 3, 4, 1},
-- {{2, 4, 1, 3}, {2, 4, 3, 1}}, {3, 1, 2, 4}, {3, 1, 4, 2}, {3, 2, 1, 4},
-- {3, 2, 4, 1}, {3, 4, 1, 2}, {3, 4, 2, 1}, {4, 1, 2, 3}, {4, 1, 3, 2},
-- {4, 2, 1, 3}, {4, 2, 3, 1}, {4, 3, 1, 2}, {4, 3, 2, 1}}
--ppEx(s,{pp_Nest,0})
--puts(1, "---------------------------\n")
--ppEx(s,{pp_Nest,1})
--pp("Hello World\n")
--if getc(0) then end if
