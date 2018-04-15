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
without debug

-- As per psym.e/pglobals.e:
constant T_pathset = 16

global function include_paths(integer convert=0)
--
-- returns a list of include paths, eg
--      {"C:\\Program Files (x86)\\Phix\\builtins\\",
--       "C:\\Program Files (x86)\\Phix\\builtins\\VM\\",
--       "C:\\Program Files (x86)\\Phix\\"}
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
    #ilASM{ lea edi,[symtab]
            call :%opGetST }    -- [edi]=symtab
    filepaths = symtab[T_pathset]
    symtab = 0
    leave_cs()
    return filepaths
end function

