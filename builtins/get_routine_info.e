--
-- builtins\get_routine_info.e (Phix compatible 0.8.2)
-- ===========================
--
include builtins/VM/pStack.e    -- :%opGetST

-- These must match pglobals.e:
--  (include pglobals.e is not on as it introduces a bunch of compiler-
--   related global variables that do not belong in application code.)
--SUG: since this is *not* an autoinclude, we /could/ make these global...

constant S_Name  = 1,    -- const/var/rtn name (now a ttidx number or -1)
         S_NTyp  = 2,   -- Const/GVar/TVar/Nspc/Type/Func/Proc
         S_sig   = 7,   -- routine signature, eg {'F',T_integer} (nb S_sig must be = S_vtype)
         S_ParmN = 9,   -- minimum no of parameters (max is length(S_sig)-1)
         S_Type  = 6,   -- [S_NTyp] settings
--       S_Func  = 7,
         S_Proc  = 8,
--       T_integer  =  1,
--       T_atom     =  3,
--       T_string   =  8,
--       T_sequence = 12,
         T_object   = 15

-- hack: force [the entire] symtab name population...
function rtnid(string name) return routine_id(name) end function
integer grid = 0

global function get_routine_info(integer rid, bool bName=true)
--global function get_arg_count(integer rid, bMin = false)
--
-- returns {maxp, minp, sig[, name]}, where
--  maxp is the maximum number of arguments the specified routine accepts,
--  minp is the minmum            """				"""
--  sig is the signature, in (roottype) string form, eg "FISO".
--
    if rid=-9 or grid=-9 then
        -- compiler special (don't stomp on symtab!)
        -- (as used in structs.e fetch/store_field.)
        grid = -9
        return {0,0,"XX"}
    end if
    if bName and grid=0 then grid = rtnid("get_routine_info") end if
    object symtab
    enter_cs()
    #ilASM{
        [32]
            lea edi,[symtab]
        [64]
            lea rdi,[symtab]
        []
            call :%opGetST      -- [e/rdi]=symtab (ie our local:=the real symtab)
          }
    object sr = symtab[rid]
--  string name = sr[S_Name]
    object name = sr[S_Name]    -- 22/12/20 class methods have a [symtab] name of -1, on purpose
                                --          (their proper names are kept in builtins/structs.e,
                                --           and are passed in as text anyway, so you don't need
                                --           multiple conflicting entries messing up the symtab,
                                --           or ever allow a "do()" that should be an "s.do()".)
    integer ntype = sr[S_NTyp],
            minp = sr[S_ParmN]
    if ntype<S_Type or ntype>S_Proc then ?9/0 end if
    sr = sr[S_sig]
    integer sigl = length(sr),
            maxp = sigl-1
    --
    -- Avoid shared refcounts of any kind, and 
    -- get root types/make it a readable string:
    --
    sequence res = repeat(' ',sigl)
    for i=1 to sigl do
        integer si = sr[i]
        if i>1 then
            while si>T_object do
                si = symtab[si][S_sig][2]   -- (sig shd be {'T',type})
            end while
            si = "INSPO"[find(si,{1,3,8,12,15})]
        end if
        res[i] = si
    end for
    res = iff(bName?{maxp,minp,res,name}:{maxp,minp,res})
    sr = 0
    symtab = 0
    leave_cs()
    return res
end function

