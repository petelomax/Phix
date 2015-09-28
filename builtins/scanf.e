--
-- builtins/scanf.e
-- ================
--
-- the real gruntwork here is recognising numbers.
-- the clever bit, if you can call it that, is strings simply expand between literal matches.

constant NONE = 0,
         LITERAL = 1,
         INTEGER = 2,
         ATOM = 4,
         STRING = 8

function parse_fmt(string fmt)
--
-- internal: converts eg "  Vendor: %s Model: %s Rev: %4d"
--                    to {"  Vendor: ",STRING," Model: ",STRING," Rev: ",INTEGER}
--                    ie {LITERAL,     STRING,LITERAL,   STRING,LITERAL, INTEGER}
-- invalid formats cause a fatal runtime error
--
integer fmtdx = 1, 
        litstart = 1
integer ch, 
        ftyp, 
        last = NONE
sequence res = {}

    if length(fmt)=0 then crash("length(fmt) is 0") end if
    while fmtdx<=length(fmt) do
        ch = fmt[fmtdx]
        if ch='%' then
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
            ch = fmt[fmtdx]
            -- skip any maxwidth/precision/zerofill/justify etc:
            while (ch>='0' and ch<='9') or find(ch,"+-.,")!=0 do
                fmtdx += 1
                if fmtdx>length(fmt) then crash("bad format") end if
                ch = fmt[fmtdx]
            end while
            switch lower(ch) do
                case 's','c':           ftyp = STRING
                case 'd','x','o','b':   ftyp = INTEGER
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
            res &= fmt[litstart..$]
        end if
    end if
    return res  
end function

constant bases = {8,16,2,10}    -- NB: oxbd order

string baseset
    baseset = repeat(255,256)
    for i=0 to 9 do
        baseset['0'+i] = i
    end for
    for i=10 to 15 do
        baseset['A'+i-10] = i
        baseset['a'+i-10] = i
    end for

integer ch

--NB code from ptok.e relies on there being a \n at the end.

function completeFloat(string s, integer sidx, atom N, integer msign)
integer tokvalid
atom dec
integer exponent
integer esigned

    if ch='.' then
        tokvalid = 0
        dec = 10
        while 1 do
--          sidx += 1
            if sidx>length(s) then exit end if
            ch = s[sidx]
            if ch!='_' then
                if ch<'0' or ch>'9' then exit end if
                N += (ch-'0') / dec
                dec *= 10
                tokvalid = 1
            end if
            sidx += 1
        end while
        if tokvalid=0 then return {} end if
    else
        sidx -= 1
    end if
    exponent = 0
    if ch='e' or ch='E' then
        tokvalid = 0
        esigned = 0
        while 1 do
            sidx += 1
            if sidx>length(s) then exit end if
            ch = s[sidx]
            if ch<'0' or ch>'9' then
                if ch!='_' then
                    if tokvalid=1 then exit end if -- ie first time round only
                    if ch='-' then
                        esigned = 1
                    elsif ch!='+' then
                        exit
                    end if
                end if
            else
                exponent = exponent*10 + ch-'0'
                tokvalid = 1
            end if
--          sidx += 1
        end while
        if tokvalid=0 then return {} end if
        if esigned then
            exponent = -exponent
        end if
        if exponent>308 then
            -- rare case: avoid power() overflow
            N *= power(10, 308)
            if exponent>1000 then
                exponent = 1000 
            end if
            for i=1 to exponent-308 do
                N *= 10
            end for
        elsif exponent<0 then
            N /= power(10,-exponent)
        else
            N *= power(10, exponent)
        end if
    end if
    return {N*msign,sidx}
end function

function get_number(string s, integer sidx)
--integer ch
integer ch2
atom N
integer msign, base, tokvalid

--  sidx += 1
    if sidx>length(s) then return {} end if
    ch = s[sidx]
    msign = 1
    if ch='-' then
        sidx += 1
        if sidx>length(s) then return {} end if
        ch = s[sidx]
        msign = -1
    elsif ch='+' then
        sidx += 1
        if sidx>length(s) then return {} end if
        ch = s[sidx]
    end if
    if  ch>='0' 
    and ch<='9' then
        N = ch-'0'
        sidx += 1
        if sidx>length(s) then return {N*msign,sidx} end if
        ch = s[sidx]
        if N=0 then -- check for 0x/o/b/d formats
            base = find(ch,"toxbd(")
            if base then
                if base>1 then
                    base -= 1
                end if
                if base=5 then      -- 0(nn) case
                    base = 0
                    while 1 do
                        sidx += 1
                        if sidx>length(s) then return {} end if
                        ch = s[sidx]
                        if ch<'0' or ch>'9' then exit end if
                        base = base*10 + ch-'0'
                    end while
                    if base<2 or base>16 or ch!=')' then
                        return {}
                    end if
                else
                    base = bases[base]
                end if
                sidx += 1
                if sidx>length(s) then return {} end if
                ch = s[sidx]
                tokvalid = 0
                if base=8 and ch='b' then   -- getByteWiseOctal()
                    sidx += 1
                    if sidx>length(s) then return {} end if
                    ch = s[sidx]
                    -- groups of 3:
                    while 1 do
                        -- first char 0..3
                        ch = baseset[ch]
                        if ch>3 then exit end if
                        tokvalid = 0
                        N = N*4 + ch
                        sidx += 1
                        if sidx>length(s) then return {} end if
                        ch = s[sidx]
                        -- second char 0..7
                        ch = baseset[ch]
                        if ch>7 then return {} end if
                        N = N*8 + ch
                        sidx += 1
                        if sidx>length(s) then return {} end if
                        ch = s[sidx]
                        -- third char 0..7
                        ch = baseset[ch]
                        if ch>7 then return {} end if
                        N = N*8 + ch
                        tokvalid = 1
                        sidx += 1
                        if sidx>length(s) then exit end if
                        ch = s[sidx]
                    end while
                else
                    while 1 do
                        if ch!='_' then     -- allow eg 1_000_000 to mean 1000000 (any base)
                            ch = baseset[ch]
                            if ch>=base then exit end if    
                            N = N*base + ch
                            tokvalid = 1
                        end if
                        sidx += 1
                        if sidx>length(s) then exit end if
                        ch = s[sidx]
                    end while
                end if
                if tokvalid=0 then return {} end if
                return {N*msign,sidx}
            end if
        end if

        while 1 do
            if ch<'0' or ch>'9' then
                if ch!='_' then exit end if     -- allow eg 1_000_000 to mean 1000000
            else
                N = N*10 + ch-'0'
            end if
            sidx += 1
            if sidx>length(s) then exit end if
            ch = s[sidx]
        end while
        if sidx<length(s) then
            sidx = sidx+1
            ch2 = s[sidx]
            if ch!='.' then
                -- allow eg 65'A' to be the same as 65:
                if ch='\'' and s[sidx]=N and s[sidx+2]='\'' then
                    sidx += 3
                    return {N*msign,sidx}
                end if
                ch2 = '.'
            end if
            if ch2!='.'                             -- fraction but not ellipse
            or (ch='e' or ch='E') then              -- exponent ahead
                return completeFloat(s,sidx,N,msign)
            end if
            sidx -= 1
        end if
        return {N*msign,sidx}
    elsif ch='.' then
        sidx += 1
        if sidx>length(s) then return {} end if
        ch = s[sidx]
        if ch>='0' and ch<='9' then -- ".4" is a number
--          sidx -= 1
            ch = '.'
            return completeFloat(s,sidx,0,msign)
        end if
        return {}
    elsif ch='#' then
        tokvalid = 0 -- ensure followed by >=1 hex digit
        N = 0
        while 1 do
            sidx += 1
            if sidx>length(s) then exit end if
            ch = s[sidx]
            if ch!='_' then
                ch = baseset[ch]
                if ch>16 then exit end if
                N = N*16 + ch
                tokvalid = 1
            end if
        end while
        if tokvalid=0 then return {} end if
        return {N*msign,sidx}
    end if
    return {}
end function


function scanff(sequence res, string s, integer sidx, sequence fmts, integer fidx)
object ffi, try
integer start
sequence resset = {}
atom N
integer goodres
    if fidx<=length(fmts) then
        ffi = fmts[fidx]
        if string(ffi) then -- LITERAL
            for i=1 to length(ffi) do
                if s[sidx]!=ffi[i] then return {} end if
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
                try = scanff(res,s,sidx+length(ffi),fmts,fidx)
                if length(try) then
                    resset &= try
                end if
                sidx += 1
            end while
            res = resset
        else
            try = get_number(s,sidx)
            if length(try)=0 then return {} end if
            {N, sidx} = try
            if ffi=INTEGER then
                if not integer(N) then return {} end if
            end if
            res = append(res,N)
            res = scanff(res,s,sidx,fmts,fidx+1)
        end if
    else
        if sidx<=length(s) then return {} end if
        res = {res}
    end if
    if length(res)>1 and sidx=1 and fidx=1 then
        -- filter multiple results to exact matches
        goodres = 0
        for i=1 to length(res) do
            if sprintf(fmts,res[i])=s then
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

global function scanf(string s, string fmt)
    return scanff({},s,1,parse_fmt(fmt),1)
end function

