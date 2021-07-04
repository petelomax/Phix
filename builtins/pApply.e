--
-- builtins\pApply.e
-- =================
--
include builtins\get_routine_info.e

enum PROC, FUNC

function apply_(object s, integer fn, pf, object userdata = {})
    -- apply fn as PROC or FUNC to all elements of sequence s
    integer {maxp,minp} = get_routine_info(fn)
    sequence res = {}
    if atom(s) then
        integer l = length(userdata),
                n = (l>0)
        if s=true then
            --
            -- userdata specifies multiple arguments:
            --  if userdata[i] is an atom, it is passed to every call.
            --  if userdata[i] is length(1), then userdata[i][1] "".
            --  otherwise length(userdata[i]) must match all other 
            --            non-atom/length(1) elements of it (==n).
            --  fn is then invoked n times (can be 0), and always
            --                with exactly length(userdata) args.
            --
            sequence args = deep_copy(userdata), -- (sets atoms and length)
                     multi = repeat(false,l)
            for i=1 to l do
                object ui = userdata[i]
                if sequence(ui) then
                    integer m = length(ui)
                    if m = 1 then
                        args[i] = ui[1]
                    elsif n!=1 and m!=n then
                        crash("invalid lengths")
                    else
                        n = m
                        multi[i] = true
                    end if
                end if
            end for

            if maxp<l or minp>l then
                crash("supplied function must accept %d parameters",{l})
            end if

            if pf=FUNC then res = repeat(0,n) end if
            for i=1 to n do
                for j=1 to l do
                    if multi[j] then
                        args[j] = userdata[j][i]
                    end if
                end for
                if pf=PROC then
                    call_proc(fn,args)
                else
                    res[i] = call_func(fn,args)
                end if
            end for
        elsif s=false then
            if pf=FUNC then res = repeat(0,l) end if
            for i=1 to l do
                object ui = userdata[i]
                if atom(ui) 
                or string(ui) then
                    ui = {ui}
                end if 

                integer lui = length(ui)
                if maxp<lui or minp>lui then
                    crash("supplied function must accept 1..%d parameters",{lui})
                end if

                if pf=PROC then
                    call_proc(fn,ui)
                else
                    res[i] = call_func(fn,ui)
                end if
            end for
        else
            ?9/0 -- what is (atom) s then, if not true/false?
        end if
    else
        -- s is a list of single args
        integer ls = length(s)
        if pf=FUNC then res = repeat(0,ls) end if
        if userdata={} and (maxp=1 or minp<=1) then
            if maxp=0 or minp>1 then
                crash("supplied function must accept 1 parameter")
            end if
            for i=1 to ls do
                if pf=PROC then
                    fn(s[i])
                else
                    res[i] = fn(s[i])
                end if
            end for
        else
            if maxp=0 or minp>2 then
                crash("supplied function must accept 1..2 parameters")
            end if
            for i=1 to ls do
                if pf=PROC then
                    fn(s[i],userdata)
                else
                    res[i] = fn(s[i],userdata)
                end if
            end for
        end if
    end if
    return res
end function

global function apply(object s, integer fn, object userdata = {})
    return apply_(s,fn,FUNC,userdata)
end function

global procedure papply(object s, integer fn, object userdata = {})
    {} = apply_(s,fn,PROC,userdata)
end procedure

