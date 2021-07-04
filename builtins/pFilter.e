--
-- builtins\pFilter.e
-- ==================
--
--  Phix implementation of the filter() function (an autoinclude)
--
--  See docs for details, and contrast with Euphoria's std/sequence.e/filter().
--
--  There is no get_routine_info() in JavaScript, so this was hand-translated for p2js.js
--
include builtins/ptypes.e               -- type rid_string
include builtins/get_routine_info.e     -- get_routine_info()

global function filter(sequence s, rid_string rs, object userdata = {}, string rangetype = "")
--
-- Select only those elements from a sequence that pass a specified test.
--
    object si
    sequence res = iff(string(s)?"":{})
    if string(rs) then
        -- built-in handling
        integer inout = find(rs,{"in","out"})
        if inout!=0 then
            inout = (inout=1) -- in: true, out: false
            if rangetype="" then
                -- set handling
                for i=1 to length(s) do
                    si = s[i]
                    integer f = find(si,userdata)
                    if (f!=0)==inout then
                        res = append(res,si)
                    end if
                end for             
            else
                -- inclusive/exclusive lo/hi range handling
                integer rt = find(rangetype,{"[]","(]","[)","()"})-1
                if rt=-1 then
                    crash("invalid rangetype")
                end if
                -- rt is now 0..3, aka 0b00..0b11:  -- exclsve, inclsive
                integer xl =  and_bits(rt,0b01),    -- 0 for [,  1 for (
                        xh = -and_bits(rt,0b02)/2   -- 0 for ], -1 for )
                if not sequence(userdata)
                or length(userdata)!=2 then
                    crash("userdata must be a sequence of length 2 for in/out handling")
                end if
                object {lo,hi} = userdata
                for i=1 to length(s) do
                    si = s[i]
                    integer lc = compare(si,lo),
                            hc = compare(si,hi)
                    if ((lc>=xl) and (hc<=xh))==inout then
                        res = append(res,si)
                    end if
                end for
            end if
        else
            if rangetype!="" then crash("invalid rangetype") end if
            integer ct = find(rs,{"<", "<=","=", "!=",">=",">" })
            if ct=0 then
                    ct = find(rs,{"lt","le","eq","ne","gt","ge"})
                if ct=0 then --       maybe "=="    
                    if rs!="==" then crash("unrecognised comparison operator") end if
                    ct = 3
                end if
            end if
            integer ne = (ct=4)
            ct -= (ct>=4)
            sequence ok = {{-1},{-1,0},{0},{0,1},{1}}[ct]
            for i=1 to length(s) do
                si = s[i]
                integer c = compare(si,userdata)
                if (find(c,ok)!=0)!=ne then
                    res = append(res,si)
                end if
            end for
        end if      
    else
        -- user-defined function handling
        if rangetype!="" then crash("invalid rangetype") end if
        integer fn = rs,
                {maxp,minp} = get_routine_info(fn)
        if maxp<1 or minp>3 then
            crash("filter routine must accept 1..3 parameters")
        elsif maxp=1 and userdata!={} then
            crash("filter routine must accept 2..3 parameters")
        end if
        for i=1 to length(s) do
            si = s[i]
            bool bAdd
            if userdata={} then
                if (maxp=1 or minp<=1) then
                    bAdd = fn(si)
                elsif minp=2 then
                    bAdd = fn(si,i)
                elsif minp=3 then
                    bAdd = fn(si,i,s)
                end if
            elsif minp=3 then
                bAdd = fn(si,i,userdata)
            else
                bAdd = fn(si,userdata)
            end if
            if bAdd then
                res = append(res,si)
            end if
        end for
    end if
    return res
end function

