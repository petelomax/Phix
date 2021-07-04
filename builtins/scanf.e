--
-- builtins/scanf.e
-- ================
--
--  (Also implements to_number())
--  scanf(s,fmt) attempts to find sequence params for which sprintf(fmt,params) could have produced s.
--
--  May return more than one set, for example 
--          res = scanf("one two three","%s %s") - res is {{"one","two three"},{"one two","three"}}
--
--  Note that scanf relies heavily on literal separators, especially spaces. It is illegal to specify back-to-back
--  strings, integers, or atoms with format strings such as "%s%s", "%d%d", "%s%d", etc. The one exception is that
--  a string can immediately follow a number, an example of which is "4th" and "%d%s". Theoretically it might be
--  possible to write a scanf that yields {{1,23},{12,3}} from scanf("123","%d%d") but I for one cannot think of
--  a single practical use, and a scanf(x,"%s%s") that returns {{"",x},..{x,""}} is also of dubious value.
--  Likewise the ability to get {"hello",12} from "hello12", which is hard, as opposed to from "hello 12", which 
--  is trivial, is deemed completely unnecessary.
--
--  Any width/precision/justify/zerofill details are for the most part quietly ignored: you may get the same results
--  from %d/%d/%d as %02d/%02d/%04d, but obviously the latter might make the intent clearer, re-use something that 
--  actually needs those qualifiers, or just be brain on autopilot: any error or warning here would hinder rather 
--  than help. If scanf is about to return several possibilities, it tests the results of sprintf and trims the 
--  result set down to those with an exact character-by-character match, as long as that does not trim the result 
--  set down to zero, and, as in the example above, may trim nothing.
--
--  Internally scanf only cares for d/f/s formats; (x/o/b)/(e/g)/(c) are treated as respective aliases, up to but 
--  not including the afore-mentioned final printf trimming stage that is. For more details regarding format strings 
--  refer to printf. All characters not part of a %-group are treated as literal. There is no way to suggest, let 
--  alone force, that say scanf("#FF","%d") should fail but scanf("255","%d") should succeed, and anything like 
--  scanf("#FF","#%x") simply does not work (sorry), but scanf("#FF","%x") is perfectly fine, apart from the fact 
--  that sprintf("%x",#FF) produces "FF" not "#FF". [DEV both scanf("#FF","#%x") and scanf("FF","%x") fail...]
--
--  Failure is indicated by {}. Otherwise each element of the results has as many elements as there were format
--  specifications in the format string, in the same order as specfied. A perfect unique and unambiguous result
--  is indicated by length(res)=1.
--
--  The parse_date_string() function of timedate is a much better way to process date and time strings.
--
--  (programming note: %s is all the wildcard-matching we can handle; ? and * are treated as literals.)
--
--!/**/without debug -- (keep ex.err clean)
--
-- The real gruntwork here is recognising numbers (if you think this is complicated, google "scanf.c").
--  Much of get_number() and completeFloat() was copied from ptok.e, should really unify I spose, but
--  ptok.e relies heavily on there being a \n at the end of every line, which this cannot, really.
-- The clever bit, if you can call it that, is strings simply expand between literal matches.
--

-- (actual values have no special meaning, except for one test that assumes NONE<LITERAL<REST.
--  the values -1,0,1,2,3, for example, could just as easily have been used instead)
constant NONE    = 0,
         LITERAL = 1,   -- (note we actually store a string, rather than the 1)
--       INTEGER = 2,   -- ( ie %d )
         ATOM    = 4,   -- ( ie %f )
         STRING  = 8,   -- ( ie %s )
         DECIMAL = 10,
         BINARY  = 12,
         HEXADEC = 16,
         OCTAL   = 18

function parse_fmt(string fmt)
--
-- internal: converts eg "  Vendor: %s Model: %s Rev: %4d"
--                    to {"  Vendor: ",STRING," Model: ",STRING," Rev: ",INTEGER}
--                    =~ {LITERAL,     STRING,LITERAL,   STRING,LITERAL, INTEGER}
-- invalid formats cause a fatal runtime error
--
integer fmtdx = 1, 
        litstart = 1
integer scan_ch, 
        ftyp, 
        last = NONE
sequence res = {}

    if length(fmt)=0 then crash("length(fmt) is 0") end if
    while fmtdx<=length(fmt) do
        scan_ch = fmt[fmtdx]
        if scan_ch='%' then
            if litstart<fmtdx then
                if last=LITERAL then
                    res[$] &= fmt[litstart..fmtdx-1]
                else
                    res = append(res,fmt[litstart..fmtdx-1])
                end if
                last = LITERAL
            end if
            fmtdx += 1
            if fmtdx>length(fmt) then crash("bad format") end if
            scan_ch = fmt[fmtdx]
            -- skip any maxwidth/precision/zerofill/justify etc:
            while (scan_ch>='0' and scan_ch<='9') or find(scan_ch,"+-.,")!=0 do
                fmtdx += 1
                if fmtdx>length(fmt) then crash("bad format") end if
                scan_ch = fmt[fmtdx]
            end while
            switch lower(scan_ch) do
                case 's','c':           ftyp = STRING
--              case 'd','x','o','b':   ftyp = INTEGER
                case 'd':               ftyp = DECIMAL
                case 'b':               ftyp = BINARY
                case 'o':               ftyp = OCTAL
                case 't':               ftyp = OCTAL
                case 'x':               ftyp = HEXADEC
                case 'f','g','e':       ftyp = ATOM
                case '%':               ftyp = LITERAL
                default:                crash("bad format")
            end switch
            if ftyp=LITERAL then
                if last=LITERAL then
                    res[$] &= '%'
                else
                    res = append(res,"%")
                end if
            else
                if ftyp=STRING then
                    if last=STRING then crash("bad format") end if
                else
                    if last>LITERAL then crash("bad format") end if
                end if
                res = append(res,ftyp)
            end if
            last = ftyp
            litstart = fmtdx+1
        end if
        fmtdx += 1
    end while
    if litstart<fmtdx then
        if last=LITERAL then
            res[$] &= fmt[litstart..$]
        else
--31/10/15:
--          res &= fmt[litstart..$]
            res = append(res,fmt[litstart..$])
        end if
    end if
    return res  
end function

--constant bases = {8,16,2,10}  -- NB: oxbd order

string baseset
sequence bases
integer binit = 0

procedure initb()
    baseset = repeat(255,256)
    for i=0 to 9 do
        baseset['0'+i] = i
    end for
--  for i=10 to 15 do
    for i=10 to 35 do
        baseset['A'+i-10] = i
        baseset['a'+i-10] = i
    end for
    bases = {8,16,2,10}  -- NB: oxbd order
    binit = 1
end procedure

integer scan_ch

--NB code from ptok.e relies on there being a \n at the end.

function completeFloat(string s, integer sidx, atom N, integer msign)
integer tokvalid
atom dec
integer exponent
integer esigned
atom fraction

    if scan_ch='.' then
        tokvalid = 0
--      dec = 10
        dec = 1
        fraction = 0
        while 1 do
--          sidx += 1
            if sidx>length(s) then exit end if
            scan_ch = s[sidx]
            if scan_ch!='_' then
                if scan_ch<'0' or scan_ch>'9' then exit end if
--27/10/15
--              N += (scan_ch-'0') / dec
                fraction = fraction*10+(scan_ch-'0')
                dec *= 10
                tokvalid = 1
            end if
            sidx += 1
        end while
        if tokvalid=0 then return {} end if
        N += fraction/dec
    else
        sidx -= 1
    end if
    exponent = 0
    if scan_ch='e' or scan_ch='E' then
        tokvalid = 0
        esigned = 0
        while 1 do
            sidx += 1
            if sidx>length(s) then exit end if
            scan_ch = s[sidx]
            if scan_ch<'0' or scan_ch>'9' then
                if scan_ch!='_' then
                    if tokvalid=1 then exit end if -- ie first time round only
                    if scan_ch='-' then
                        esigned = 1
                    elsif scan_ch!='+' then
                        exit
                    end if
                end if
            else
                exponent = exponent*10 + scan_ch-'0'
                tokvalid = 1
            end if
--          sidx += 1
        end while
        if tokvalid=0 then return {} end if
        if esigned then
            exponent = -exponent
        end if
        if exponent<0 then
            if exponent<-308 then
                -- rare case: avoid power() overflow
                N /= power(10, 308)
                if exponent<-1000 then
                    exponent = -1000 
                end if
                for i=1 to -exponent-308 do
                    N /= 10
                end for
            else
                N /= power(10,-exponent)
            end if
        else
            if exponent>308 then
                -- rare case: avoid power() overflow
                N *= power(10, 308)
                if exponent>1000 then
                    exponent = 1000 
                end if
                for i=1 to exponent-308 do
                    N *= 10
                end for
            else
                N *= power(10, exponent)
            end if
        end if
    end if
    return {N*msign,sidx}
end function

function get_number(string s, integer sidx, inbase=10)
integer scan_ch2
atom N
integer msign, base = 0, tokvalid = 1

--  sidx += 1
    if sidx>length(s) then return {} end if
    scan_ch = s[sidx]
    msign = 1
    if scan_ch='-' then
        sidx += 1
        if sidx>length(s) then return {} end if
        scan_ch = s[sidx]
        msign = -1
    elsif scan_ch='+' then
        sidx += 1
        if sidx>length(s) then return {} end if
        scan_ch = s[sidx]
    end if
--  if  scan_ch>='0' 
--  and scan_ch<='9' then
    N = baseset[scan_ch]
    if N<inbase then
        sidx += 1
        if sidx>length(s) then return {N*msign,sidx} end if
        scan_ch = s[sidx]
        if N=0 then -- check for 0x/o/b/d formats
--          base = find(scan_ch,"toxbd(")
            base = find(lower(scan_ch),iff(inbase>10?"toxxx(":"toxbd("))
            if base then
                if base>1 then
                    base -= 1
                end if
                if base=5 then      -- 0(nn) case
                    base = 0
                    while 1 do
                        sidx += 1
                        if sidx>length(s) then return {} end if
                        scan_ch = s[sidx]
                        if scan_ch<'0' or scan_ch>'9' then exit end if
                        base = base*10 + scan_ch-'0'
                    end while
                    if base<2 or base>16 or scan_ch!=')' then
                        return {}
                    end if
                else
                    base = bases[base]
                end if
                sidx += 1
                if sidx>length(s) then return {} end if
                scan_ch = s[sidx]
                tokvalid = 0
                if base=8 and scan_ch='b' then  -- getByteWiseOctal()
                    sidx += 1
                    if sidx>length(s) then return {} end if
                    scan_ch = s[sidx]
                    -- groups of 3:
                    while 1 do
                        -- first char 0..3
                        scan_ch = baseset[scan_ch]
                        if scan_ch>3 then exit end if
                        tokvalid = 0
                        N = N*4 + scan_ch
                        sidx += 1
                        if sidx>length(s) then return {} end if
                        scan_ch = s[sidx]
                        -- second char 0..7
                        scan_ch = baseset[scan_ch]
                        if scan_ch>7 then return {} end if
                        N = N*8 + scan_ch
                        sidx += 1
                        if sidx>length(s) then return {} end if
                        scan_ch = s[sidx]
                        -- third char 0..7
                        scan_ch = baseset[scan_ch]
                        if scan_ch>7 then return {} end if
                        N = N*8 + scan_ch
                        tokvalid = 1
                        sidx += 1
                        if sidx>length(s) then exit end if
                        scan_ch = s[sidx]
                    end while
                    if tokvalid=0 then return {} end if
                    return {N*msign,sidx}
                end if
--              else
--                  while 1 do
--                      if scan_ch!='_' then    -- allow eg 1_000_000 to mean 1000000 (any base)
--                          scan_ch = baseset[scan_ch]
--                          if scan_ch>=base then exit end if   
--                          N = N*base + scan_ch
--                          tokvalid = 1
--                      end if
--                      sidx += 1
--                      if sidx>length(s) then exit end if
--                      scan_ch = s[sidx]
--                  end while
--              end if
--              if tokvalid=0 then return {} end if
--              return {N*msign,sidx}
            end if
        end if
        if base=0 then base=inbase end if

        while 1 do
--          if scan_ch<'0' or scan_ch>'9' then
--              if scan_ch!='_' then exit end if    -- allow eg 1_000_000 to mean 1000000
--          else
--              N = N*10 + scan_ch-'0'
--          end if
            if scan_ch!='_' then    -- allow eg 1_000_000 to mean 1000000 (any base)
--31/7/19:
                if scan_ch='.' then exit end if
                scan_ch2 = baseset[scan_ch]
                if scan_ch2>=base then exit end if  
                N = N*base + scan_ch2
                tokvalid = 1
            end if
            sidx += 1
            if sidx>length(s) then exit end if
            scan_ch = s[sidx]
        end while
        if sidx<length(s) then
            sidx = sidx+1
            scan_ch2 = s[sidx]
            if scan_ch!='.' then
                -- allow eg 65'A' to be the same as 65:
--DEV could probably do with some more bounds checking here (spotted in passing)
                if scan_ch='\'' and s[sidx]=N and s[sidx+2]='\'' then
                    sidx += 3
                    return {N*msign,sidx}
                end if
                scan_ch2 = '.'
            end if
            if scan_ch2!='.'                            -- fraction but not ellipse
            or (scan_ch='e' or scan_ch='E') then                -- exponent ahead
                return completeFloat(s,sidx,N,msign)
            end if
            sidx -= 1
        elsif tokvalid=0 then   -- eg "0b" or "0(16)", ie no actual digits
            return {}
        end if
        return {N*msign,sidx}
    elsif scan_ch='.' then
        sidx += 1
        if sidx>length(s) then return {} end if
        scan_ch = s[sidx]
        if scan_ch>='0' and scan_ch<='9' then -- ".4" is a number
--          sidx -= 1
            scan_ch = '.'
            return completeFloat(s,sidx,0,msign)
        end if
        return {}
    elsif scan_ch='#' then
        tokvalid = 0 -- ensure followed by >=1 hex digit
        N = 0
        while 1 do
            sidx += 1
            if sidx>length(s) then exit end if
            scan_ch = s[sidx]
            if scan_ch!='_' then
                scan_ch = baseset[scan_ch]
                if scan_ch>16 then exit end if
                N = N*16 + scan_ch
                tokvalid = 1
            end if
        end while
        if tokvalid=0 then return {} end if
        return {N*msign,sidx}
    end if
    return {}
end function

global function to_number(string s, object failure={}, integer inbase=10)
    atom N
    integer sidx = 1
    if not binit then initb() end if
    if length(s)>=2 and s[1..2]="0(" then
        {inbase,sidx} = get_number(s,3,10)
        if s[sidx]!=')' then return failure end if
        sidx += 1
    end if
    sequence r = get_number(s,sidx,inbase)
    if length(r) then
        {N, sidx} = r
        if sidx>length(s) then
            return N
        end if
    end if
--  return {}   -- failure?
    return failure
end function


function scanff(sequence res, string s, integer sidx, sequence fmts, integer fidx)
object ffi, tries
integer start
sequence resset = {}
atom N
--integer goodres
    if fidx<=length(fmts) then
        if not binit then initb() end if
        ffi = fmts[fidx]
        if string(ffi) then -- LITERAL
            for i=1 to length(ffi) do
                if sidx>length(s) or s[sidx]!=ffi[i] then return {} end if
                sidx += 1
            end for
            res = scanff(res,s,sidx,fmts,fidx+1)
        elsif ffi=STRING then
            if fidx=length(fmts) then
                res = append(res,s[sidx..$])
                return {res}
            end if
            fidx += 1
            ffi = fmts[fidx]
            fidx += 1
            if not string(ffi) then ?9/0 end if     -- should never happen
                                -- (should have been spotted in parse_fmt)
            start = sidx
            res = append(res,0)     -- (placeholder, overwritten/discarded)
            resset = {}
            while 1 do                              -- backtracking loop
                sidx = match(ffi,s,sidx)
                if sidx=0 then exit end if
                res[$] = s[start..sidx-1]
--              tries = scanff(res,s,sidx+length(ffi),fmts,fidx)
                tries = scanff(deep_copy(res),s,sidx+length(ffi),fmts,fidx)
                if length(tries) then
                    resset &= tries
                end if
--              tries = 0
                sidx += 1
            end while
            res = resset
        else
--       ATOM    = 4,   -- ( ie %f )
--       DECIMAL = 10,
--       BINARY  = 12,
--       HEXADEC = 16,
--       OCTAL   = 18
            integer inbase = {10,10,2,16,8}[find(ffi,{ATOM,DECIMAL,BINARY,HEXADEC,OCTAL})]
            tries = get_number(s,sidx,inbase)
            if length(tries)=0 then return {} end if
            {N, sidx} = tries
--          if ffi=INTEGER then
            if ffi>=DECIMAL then
--              if not integer(N) then return {} end if
                if not integer(N) and N!=floor(N) then return {} end if
            end if
            res = append(res,N)
            res = scanff(res,s,sidx,fmts,fidx+1)
        end if
    else
        if sidx<=length(s) then return {} end if
        res = {res}
    end if
--finally moved 22/7/19: (upon also spotting that fmts is no good for the sprintf() call anyways)
--DEV/DOH: this should almost certainly be in scanf itself! [ie no need for "and sidx=1 and fidx=1"] (spotted in passing)
--  if length(res)>1 and sidx=1 and fidx=1 then
--      -- filter multiple results to exact matches
--      goodres = 0
--      for i=1 to length(res) do
--          if sprintf(fmts,res[i])==s then
--              goodres += 1
--              res[goodres] = res[i]
--          end if
--      end for
--      if goodres!=0 then
--          res = res[1..goodres]
--      end if
--  end if
    return res  
end function

global function scanf(string s, string fmt)
--  return scanff({},s,1,parse_fmt(fmt),1)
--p2js:
--  sequence res = scanff({},s,1,parse_fmt(fmt),1)
    sequence res = {}
    res = scanff(res,s,1,parse_fmt(fmt),1)
    if length(res)>1 then
        -- filter multiple results to exact matches
        integer goodres = 0
        for i=1 to length(res) do
            if sprintf(fmt,res[i])==s then
                goodres += 1
                res[goodres] = res[i]
            end if
        end for
        if goodres!=0 then
            res = res[1..goodres]
        end if
    end if
    return res
end function

