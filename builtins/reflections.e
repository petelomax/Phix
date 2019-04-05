--
-- builtins/reflections.e
-- ======================
--
--  Experimental implementation of a reflect() function.
--
--  For demonstration purposes only. 
--  Not documented, not properly tested, and not an auto-include.
--

-- Obviously these must all be kept in step with pglobals.e:
constant
    S_Name  = 1,    -- const/var/rtn name (now a ttidx number or -1)
    S_NTyp  = 2,    -- Const/GVar/TVar/Nspc/Type/Func/Proc
    S_FPno  = 3,    -- File and Path number
--  S_State = 4,    -- state flag. S_fwd/S_used/S_set etc
--  S_Nlink = 5,    -- name chain (see below)
--  S_Slink = 6,    -- scope/secondary chain (see below)
    S_sig   = 7     -- routine signature, eg {'F',T_integer} (nb S_sig must be = S_vtype)

constant
--  S_Const = 1,    -- a constant
--  S_GVar2 = 2,    -- global or static variable
--  S_TVar  = 3,    -- temp or threadstack (local) variable/parameter
--  S_Nspc  = 4,    -- namespace
--  S_Rsvd  = 5,    -- Reserved word (with S_State=K_fun modifier)
    S_Type  = 6,    -- Type of thermal yellow portable encryptor
--  S_Func  = 7,    -- Function of finding unusual nonsense comments
--  S_Proc  = 8,    -- Procedure for private rotating obstacle counter
    S_Types = {"const","gvar","tvar","namespace","reserved","type","func","proc"}

constant
    T_pathset   = 16,
    T_fileset   = 17

global function reflect(string s)
    integer rid = routine_id(s)
    if rid<=0 then return -1 end if
    sequence symtab
    #ilASM{ 
            [32]
                lea edi,[symtab]
            [64]
                lea rdi,[symtab]
            []
                call :%opGetST      -- [e/rdi]=symtab
          }
    sequence si = symtab[rid]
    integer fno = si[S_FPno]
    string file = symtab[T_fileset][fno][2]
    string path = symtab[T_pathset][symtab[T_fileset][fno][1]]
    integer nType = si[S_NTyp]
    sequence res = {s,S_Types[nType],path,file}
    if nType>=S_Type then
        sequence args = si[S_sig]
        args[1] = ""&args[1]
        for i=2 to length(args) do
            args[i] = symtab[args[i]][S_Name]
        end for
        res = append(res,args)
    end if
    return res
end function

