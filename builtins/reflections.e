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
res = append(res,si[S_Name])
    return res
end function

function _debug_info()
-- use throw to convert a return address and routine number 
-- from the call stack into a proper line number, etc.
-- (private, not called direct/from outside this file)
integer rtn
atom ret_addr
 
    #ilASM{
        [32]
            mov edx,[ebp+20]    -- prev_ebp
            mov eax,[edx+28]    -- return address
            mov edx,[edx+20]    -- prev_ebp
            lea edi,[ret_addr]
            call :%pStoreMint
            mov eax,[edx+8]     -- calling routine no
            mov [rtn],eax
        [64]
            mov rdx,[rbp+40]    -- prev_ebp
            mov rax,[rdx+56]    -- return address
            mov rdx,[rdx+40]    -- prev_ebp
            lea rdi,[ret_addr]
            call :%pStoreMint
            mov rax,[rdx+16]    -- calling routine no
            mov [rtn],rax
        []
          }
    try
        throw({1,ret_addr-1,-1,rtn,-1,-1,-1})
    catch e
        return e
    end try
end function

-- NOTE: following five routines must all use the exact same nesting level.

global function debug_info()
    return _debug_info()
end function

global function debug_line()
    return _debug_info()[E_LINE]
end function

global function debug_rtn()
    return _debug_info()[E_NAME]
end function

global function debug_file()
    return _debug_info()[E_FILE]
end function

global function debug_path()
    return _debug_info()[E_PATH]
end function

