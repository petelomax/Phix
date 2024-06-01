--
-- builtins/closures.e
-- ===================
--
-- implements lambda(), define_lambda(), set_captures(), and call_lambda(). [see docs]
--
-- TODO: test under p2js...
--
sequence clrid, captures, sole, clean
integer cl_init = 0, cl_freelist = 0, clfrid

global type lambda(object l)
    bool res = cl_init and sequence(l) and length(l)=1 and integer(l[1])
    if res then
        integer ldx = l[1]
        res = ldx>0 and ldx<=length(clrid) and sequence(captures[ldx])
              and ((not clean[ldx]) or still_has_delete_routine(l,clfrid))
    end if
    return res
end type

local procedure cl_free(lambda clid)
    integer ci = clid[1]
    captures[ci] = 0
    clrid[ci] = cl_freelist
    cl_freelist = ci
end procedure

global function define_lambda(object rid, sequence partial_args, bool bSoleOwner=false, bCleanup=true)
    -- returns an {int}, optionally with cleanup, meaningful only to call_lambda().
    if not cl_init then {cl_init,clrid,captures,sole,clean,clfrid} = {1,{},{},{},{},cl_free} end if
    if not integer(rid) then
        integer fi = rid[1]
        partial_args = deep_copy(captures[fi])&partial_args
        rid = clrid[fi]
    else    
        partial_args = deep_copy(partial_args)
    end if
    integer rdx = cl_freelist
    if rdx then
        cl_freelist = clrid[cl_freelist]
        clrid[rdx] = rid
        captures[rdx] = partial_args
        sole[rdx] = bSoleOwner
        clean[rdx] = false
    else
        clrid &= rid
        rdx = length(clrid)
        captures = append(captures,partial_args)
        sole &= bSoleOwner
        clean &= false
    end if
    lambda res = {rdx}
    if bCleanup and platform()!=JS then
        clean[rdx] = bCleanup
        res = delete_routine(res,cl_free)
    end if
    return res
end function

global procedure set_captures(sequence lambdas, object caps)
    -- overwrite partial_args for one or more prior define_lambda()s.
    -- see demo/rosetta/Variadic_fixed-point_combinator.exw for an example.
    -- sets exactly the same caps on all lambdas (else use multiple calls)
    for l in lambdas do
        if integer(l) then
            assert(lambda(lambdas))
            captures[l] = deep_copy(caps)
        else
            set_captures(l,caps)
        end if
    end for
end procedure

-- maybe:
--global procedure set_capture_s(sequence lambdas, caps)
--  assert(length(lambdas)==length(caps))
--  for i,l in lambdas do set_captures(l,caps[i]) end for
--end procedure
--global procedure set_capture_pairs(sequence lc)
--  for l in lc do set_captures(l[1],l[2]) end for
--end procedure

local constant three = 3 -- set 1 to debug
--local constant three = 1

local function get_args(integer rid, fi, bSole, object args)
    -- minimises refcounts that using a local var would spanner
    sequence res
    if bSole then -- not recursion!
        res = captures[fi]
        captures[fi] = length(res) -- (kill refcounts and track length)
    else
        res = deep_copy(captures[fi])
    end if
    res &= args
    -- replicate the call_func() errors but land them in user-code:
    {integer maxp, integer minp, string sig} = get_routine_info(rid)
    if platform()!=JS then -- (only maxp checkable for that)
        assert(sig[1]='F',"closure must be a function",nFrames:=three)
        assert(length(res)>=minp,"insufficient parameters",nFrames:=three)
    end if
    assert(length(res)<=maxp,"too many parameters",nFrames:=three)
    return res
end function

local constant two = 2 -- set 1 to debug
--local constant two = 1

global function call_lambda(object f, args={})
    if integer(f) then return call_func(f,iff(atom(args)?{args}:args)) end if
    assert(lambda(f),"not a valid lambda")
    integer fi = f[1], rid = clrid[fi], l=0, bSole = sole[fi]
    object res = call_func(rid,get_args(rid,fi,bSole,args))
    if not sequence(res) then
        if not bSole then return res end if
    else
        l = length(res) -- (else 0 aka error, when bSole=true)
    end if
    bool bLenOK = l>=1+bSole and l<=2
    assert(bLenOK,"lambda results must be atom or {[captures,]res}",nFrames:=two)
    if l=1 then return res[1] end if
    bLenOK = length(res[1])==iff(sole[fi]?captures[fi]:length(captures[fi]))
    assert(bLenOK,"invalid capture return length",nFrames:=two)
    captures[fi] = res[1]
    return res[2]
end function

--?? (erm... didn't really help at the time...)
--global procedure dump_lambdas()
--  for i,ci in captures do
--      if ci!=0 then
--          integer rid = clrid[i]
--          printf(1,"%d: %d, rtn_info:%v, captures:%v\n",{i,rid,get_routine_info(rid),ci})
--      end if
--  end for
--end procedure


--/* tests:
function f(integer a, b, c)
--function f(integer a, b, c=77)
    return {{a},a+b+c}
end function    

function f2(integer a, b, c)
    return {{a,b},a+b+c}
end function    

object f1 = define_lambda(f,{1})
--object f12 = define_lambda(f2,{1,2})
object f_2 = define_lambda(f2,{1})
object f12 = define_lambda(f_2,{2})
--?f1
--?f12 -- {1}!!!
--?f12
--trace(1)
?call_lambda(f1,{1,2}) -- 4
--?call_lambda(f1,1) -- 4 -- insufficient params... [unless we have c=77]
--?call_lambda(f1,{1,2,3}) -- 4 -- too many params...
?call_lambda(f1,{2,3}) -- 6
?call_lambda(f12,3) -- 6
?call_lambda(f12,4) -- 7
--?call_lambda(f12,4)
--*/

--?call_lambda(integer,{{1,2,3}})   -- 0 (cos it isn't)
--?call_func(integer,{{1,2,3}})     -- 0      ""
--?call_lambda(integer,{1})         -- 1 (cos args[1] is)
--?call_func(integer,{1})           -- 1      ""
--?call_lambda(integer,1)           -- 1      ""
--?call_func(integer,1)             -- 1      ""
--?call_lambda(length,{{1,2,3}})    -- 3
--?call_func(length,{{1,2,3}})      -- 3
--?call_lambda(power,{2,3})         -- 8
--?call_func(power,{2,3})           -- 8

