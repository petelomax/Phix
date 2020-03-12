--
-- pincpathN.e
-- ===========
--
-- Phix implementation of include_paths.
--
-- Note this file is 100% incompatible with RDS Eu, for obvious reasons.
--  The Phix compiler includes this file automatically when needed; in
--  normal circumstances you should *NOT* manually include this file.
--  If for any reason you really need to, then I recommend using
--      --/**/include pincpaths.e as incpaths
--  (with the "--/**/" causing RDS Eu to ignore the line)
--
--without debug

-- As per psym.e/pglobals.e:
constant S_FPno  = 3,    -- File and Path number
         T_pathset = 16,
         T_fileset = 17

global function include_paths(integer convert=0)
--
-- returns a list of include paths, eg
--      {`C:\Program Files (x86)\Phix\builtins\`,
--       `C:\Program Files (x86)\Phix\builtins\VM\`,
--       `C:\Program Files (x86)\Phix\`}
-- Note that paths are normally absolute/fully qualified (as shown) but there is no
--  guarantee they will not (one day) be relative to some core/original directory.
-- If a program is compiled and installed on a different machine, then obviously these
--  paths refer to the machine on which the program was compiled and may not exist on 
--  the machine where the compiled program was installed/is currently running on.
--

object symtab
sequence filepaths      -- result variable

    if convert then end if  -- parameter not used

    -- get copy of symtab. NB read only! may contain nuts! (unassigned vars)
    enter_cs()
    #ilASM{ 
        [32]
            lea edi,[symtab]
        [64]
            lea rdi,[symtab]
        []
            call :%opGetST      -- [e/rdi]=symtab
          }
    filepaths = symtab[T_pathset]
    symtab = 0
    leave_cs()
    return filepaths
end function

global function include_files()
--
-- returns a list of included files, eg
--      {{3,`test.exw`},
--       {1,`pincpathN.e`},
--       {2,`pprntfN.e`}}
-- The first element of each entry is an index to the include_paths() result.
--
object symtab
sequence fileset        -- result variable

    -- get copy of symtab. NB read only! may contain nuts! (unassigned vars)
    enter_cs()
    #ilASM{ 
        [32]
            lea edi,[symtab]
        [64]
            lea rdi,[symtab]
        []
            call :%opGetST      -- [e/rdi]=symtab
          }
    fileset = symtab[T_fileset]
    symtab = 0
    leave_cs()
    return fileset
end function

global function include_path(sequence d = "builtins")
-- d can have several segements, eg {"builtins","VM"} or `builtins\VM`.
-- when d is "" (or {}) it returns the "root directory"
    string res = ""
    sequence s = include_paths()
    if string(d) then d = split_path(d) end if
    integer ld = length(d)
    if ld=0 then
--      res = s[3]
        sequence fileset = include_files()
        integer fdx = fileset[1][1]
        res = s[fdx]
    else
        for p=1 to length(s) do
            sequence sp = split_path(s[p])
            if sp[-ld..$]=d then
                res = s[p]
                exit
            end if
        end for
    end if
    return res
end function

global function include_file(integer depth=1)
--
-- returns an index to the include_files() result (1==main file).
--
object symtab
integer rtn,    -- from stack
        fn      -- result variable

    if depth<=0 then ?9/0 end if -- quick sanity check
    #ilASM{
        -- obviously this will agi stall, depth+1 times, 
        --  so don't be calling it from within a loop.
        [32]
            mov eax,ebp
            mov ecx,[depth]
         @@:
            mov eax,[eax+20]    -- prev_ebp
            sub ecx,1
            jnz @b
            lea edi,[symtab]
            mov eax,[eax+8]     -- rtn
            mov [rtn],eax
        [64]
            mov rax,rbp
            mov rcx,[depth]
         @@:
            mov rax,[rax+40]    -- prev_ebp
            sub rcx,1
            jnz @b
            lea rdi,[symtab]
            mov rax,[rax+16]    -- rtn
            mov [rtn],rax
        []
            call :%opGetST      -- [e/rdi]=symtab
          }
    fn = symtab[rtn][S_FPno]
    return fn
end function

--?include_paths()
--?include_files()
--?include_path("")
--?include_file()

