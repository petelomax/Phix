--
-- builtins\VM\prtnidN.e (Phix compatible 0.6.3)
-- =====================
--
-- Phix implementation of routine_id.
--
-- Ideally, calls to routine_id should be placed immediately after the routine, eg:
--
--       function someFunc()
--       end function
--       [constant] r_someFunc=routine_id("someFunc")
--
-- In such cases (fixed literal string matching an already defined routine) the 
--  value is resolved at compile-time, which means this routine is not loaded or 
--  called and that there are no scope issues (see below).
--
-- Note this file is 100% incompatible with RDS Eu, for obvious reasons.
--  The Phix compiler includes this file automatically when needed; in
--  normal circumstances you should *NOT* manually include this file.
--  If for any reason you really need to, then I recommend using
--      --/**/include prtnid.e as prtnid
--  (with the "--/**/" causing RDS Eu to ignore the line)
--
--without debug
include builtins/VM/pStack.e    -- :%opGetST


-- These must match pglobals.e:
--  (include pglobals.e is not on as it introduces a bunch of compiler-
--   related global variables that do not belong in application code.)
constant S_Name  = 1,   -- const/var/rtn name
         S_NTyp  = 2,   -- Const/GVar/TVar/Nspc/Type/Func/Proc
         S_FPno  = 3,   -- File and Path number
         S_State = 4,   -- state flag. S_fwd/S_used/S_set
--       S_Nlink = 5,   -- name chain (see below)
         S_Slink = 6,   -- scope/secondary chain (see below)
-- constants and variables [S_NTyp<=S_TVar]
         S_vtype = 7,   -- variable type or namespace fileno
         S_GVar2 = 2    -- global or static variable

constant
--       S_Nspc = 4, S_Type = 6,                -- [S_NTyp] settings
         K_gbl  = #1000,                        -- a true "global" (in S_State)
         T_maintls = 21,
--       T_callstk = 20,
         T_nslink = 18

function prev_gvar(sequence symtab, integer idx)
    while true do
        idx -= 1
        if idx=0 or symtab[idx][S_NTyp]=S_GVar2 then exit end if
    end while
    return idx
end function

--DEV 
--global function routine_id_p(sequence s, integer parentscope)
--end function
--
--global function routine_id(sequence s)
--  return routine_id_p(s,0)
--end function

--global function routine_id(sequence s)
function get_id(string s, bool bVar=false)
--
-- returns a routine number (actually an index into symtab), or
--  -1 if the routine cannot be found (anywhere/in specified namespace)
--removed, 18/8/19:
--  -2 if multiple conflicting instances exist (no namespace specified)
--  -3 if the specified namespace does not exist
-- (-2/-3 may break some legacy code that explicitly tests for -1 rather than <0.)
--
-- Note: When called at run-time, this does not fully honor nested scope, eg:
--
--          include win32lib.ew as w32
--          w32:getOpenFileName(...)
--
--  works fine, even though getOpenFileName is in w32file.ew rather than win32lib.ew. 
--  See psym2.e, sequence priorities. The compiler can even manage to resolve say
--  routine_id("w32:getOpenFileName") properly at compile-time (as long as it occurs
--  after the above include statement). However that table of priorites and all the 
--  logic to go with it is /not/ available here[*], so it *will* fail with something 
--  like routine_id("w32"&':'&"getOpenFileName"), although it would work if you had
--  an include w32file.ew as w32f (ie where it really is) and used "w32f" not "w32".
--  [*] It probably is feasible to save priorities in say symtab[26] (similar to eg 
--  T_fileset, create a new constant T_priorities=26), if needed, however as that
--  table is modified as compilation proceeds it may not be quite right at eof for
--  the point in the code where routine_id was called (cmiiw). (Obviously the example
--  of literal string&char&string may start working if the tokeniser is ever enhanced 
--  to treat such as a single fixed string, but it would still fail for vars.)
--  Likewise there may be a problem with routine_id("xx"&"yy") when xxyy() would,
--  because there is more than one global xxyy(), be resolved utilising priorities.
--  AFAIK, *no legacy code whatsoever* is affected *in any way* by such issues.
--
--  The routine_id code is also utterly indifferent to forward references, hence in:
--
--              rEight = routine_id("r8")
--              procedure r8()
--              end procedure
--
--  the (one-pass) compiler is unable to resolve rEight at compile-time, but it is
--  fixed up just fine at run-time, albeit things being a bit slower and larger.
--   <aside>
--      If all routine_ids are defined after their target, this file is not linked 
--      in. Also this is incompatible or at least different to RDS, and some legacy 
--      code may have been deliberately written to leave rEight as -1. Likewise the
--      above may mislead should rEight resolve to an existing (global) r8 routine 
--      rather than the one immediately following. This also means that a library 
--      component that utilises forward routine-ids may cease to operate correctly 
--      under certain outside influences that would not affect it were it written 
--      the right way round. You have been warned.
--  </aside>
--
--
--  Notes: A namespace is a local, so if you specify say "w32:" then there *must*
--          be an "include ... as w32" in the *same* source file as the statement
--          that is invoking routine_id, or an include of a file that contains a
--          "namespace w32" directive at or near the start, which has exactly the
--          same (local) effect.
--         If you specify a namespace, the routine *must* be a global, even if it
--          is one of those main.e doing "include main.e as main" things, which
--          may be wierd and disconcerting, but have always been perfectly valid.
--

object symtab,          -- yup, we (routine_id) is symtab[188], give or take...
       si,              -- copy of symtab[i], speedwise
       si_name          -- copy of symtab[i][S_name], speedwise/thread-sfaety
integer rtn,            -- routine number of callee, from callstack
        cFno,           -- calling fileno. Any namespace *must* be in this file.
        tFno,           -- target fileno, or 0 if no namespace specified
        siFno,          -- copy of si[S_FPno], speedwise
        clink,          -- for skipping down namespace/routine chains
        state,          -- copy of si[S_State]
        isGlobal,       -- K_gbl or 0, from si[S_State].
        res             -- the one and only global, or -2
sequence name_space     -- eg "fred" when "fred:thing" is passed as parameter.

    -- get copy of symtab. NB read only! may contain nuts! (unassigned vars)
    enter_cs()
--  si = 1  -- callstack not rqd
    #ilASM{
        [32]
            lea edi,[symtab]
--DEV not used:
--          lea esi,[si]        -- flag/crashmsg addr
--          xor ecx,ecx         -- mov ecx,0    (unused)
            call :%opGetST      -- [edi]=symtab (ie our local:=the real symtab)
            mov edi,[ebp+20]    -- prev_ebp
--DEV (16/03/2013) if we add parentscope to routine_id then we must add locals, see pemit.e
--          mov edi,[edi+20]    -- prev_ebp
--2/3/21
            mov edi,[edi+20]    -- prev_ebp
            mov edi,[edi+8]     -- calling routine no
            mov [rtn],edi
        [64]
            lea rdi,[symtab]
--DEV not used:
--          lea rsi,[si]        -- flag/crashmsg addr
--          xor rcx,rcx         -- mov ecx,0    (unused)
            call :%opGetST      -- [rdi]=symtab (ie our local:=the real symtab)
            mov rdi,[rbp+40]    -- prev_ebp
--2/3/21
            mov rdi,[rdi+40]    -- prev_ebp
            mov rdi,[rdi+16]    -- calling routine no
            mov [rtn],rdi
        []
          }

    cFno = symtab[rtn][S_FPno]      -- fileno of callee (whether routine or toplevel)
--?{rtn,cFno}
    tFno = find(':',s)
    res = -1
    if tFno then
        name_space = s[1..tFno-1]
        s = s[tFno+1..length(s)]
        tFno = 0
        clink = symtab[T_nslink]        -- follow the (special) namespace chain, speedwise
        while clink do
            si = symtab[clink]
            siFno = si[S_FPno]
            if siFno=cFno then              -- local namespace in callee file...
                si_name = si[S_Name]
                if si_name=name_space then
                    tFno = si[S_vtype]      -- ...pointing at target fileno
                    exit
                end if
            end if
            clink = si[S_Slink]
        end while
        if tFno=0 then                  -- no such namespace
            res = -3
        end if
    end if

    if res!=-3 then
        if bVar then
            clink = prev_gvar(symtab, length(symtab)+1)
        else
            clink = symtab[T_maintls][S_Slink]      -- follow the (special) routines chain, speedwise
        end if
        while clink do
            si = symtab[clink]
            si_name = si[S_Name]
            if si_name=s then
                state = si[S_State]
                isGlobal = and_bits(state,K_gbl)
                siFno = si[S_FPno]
                if tFno=0 then -- no namespace
                    if siFno=cFno then          -- callee file wins
                        res = clink
                        exit
                    end if
                    if isGlobal then
                        res = clink
                        while 1 do  -- scan for duplicates or one in callee file
                            clink = si[S_Slink]
                            if not clink then exit end if
                            si = symtab[clink]
                            si_name = si[S_Name]
                            if si_name=s then
                                siFno = si[S_FPno]
                                if siFno=cFno then  -- callee file wins
                                    res = clink
                                    exit
                                end if
                                if and_bits(si[S_State],K_gbl) then
                                    res = -2    -- more than one global (and no namespace)
                                end if          -- carry on scan for callee file wins cases
                            end if
                        end while
--                      return res  -- the one and only global not in callee file, or -2
                        exit
                    end if
                elsif siFno=tFno and isGlobal then
--                  return clink
                    res = clink
                    exit
                end if
            end if
            if bVar then
                clink = prev_gvar(symtab, clink)
            else
                clink = si[S_Slink]
            end if
        end while
    end if
--  if res<0 then res = 0 end if    -- added 17/8/19 (and docs updated)
    if res<0 then res = -1 end if   -- 18/8/19 (thought twice about it)
    si_name = 0
    si = 0
    symtab = 0
    leave_cs()
--  return -1
    return res
end function

global function routine_id(string s)
    return get_id(s)
end function

global function var_id(string s)
    return get_id(s,true)
end function
