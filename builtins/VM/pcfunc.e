--
-- pcfunc.e (Phix compatible 0.7.0)
--
--  Phix implementations of define_c_func, define_c_proc, define_c_var,
--                          open_dll, call_back, c_func, and c_proc.
--
--  WARNING: Fragile code ahead! Mistakes in this code may completely
--           spanner diagnostics and tracing. You have been warned.
--           (If p -test, p edix, p test\terror, and p test\trace
--            all seem fine, then you can breathe easy once again.)
--
--/*
    ?9/0    This file is wholly incompatible with RDS Eu/OpenEuphoria.
--*/
--
--  In Phix, you need not "include dll.e" before using define_c_func, 
--  etc, nor do you actually need to "include pcfunc.e"; the compiler
--  is smart enough to find and load this file for you automagically.
--
--  Moved from dll.e for the benefit of pmach.e
--
--  In Phix and RDS Eu you can (if you want) still code eg:
--      include dll.e as dll
--      i1 = dll:define_c_func(a,s1,s2,i2)
--  (obviously you need an explicit include if you want a namespace)
--    (and "include dll.e" is recommended over "include pcfunc.e")
--
--  a) The Phix version of dll.e contains the line
--      "--/**/ include pcfunc.e	-- (Phix compatible)"
--      Obviously, RDS Eu ignores that line/treats it as a comment,
--      whereas Phix includes this file. Also, unlike RDS Eu, in
--      Phix namespaces apply equally to sub-includes.
--
--  b) RDS Eu will of course run the versions of the routines in
--      dll.e (inside "--/*" "--*/" pairs), whereas Phix will of 
--      course execute the ones below.
--
--  c) In Phix, calls to machine_func(M_DEFINE_C) are not supposed to
--      occur, however if an application includes an old version (say 
--      RDS Eu 2.4) of dll.e, then pmach.e maps it here. Now, if we
--      left this in dll.e, and pmach.e (naievely) mapped M_DEFINE_C
--      to dll:define_c_func(), it would result in an infinite loop. 
--      Hence I moved them, but kept it as compatible as possible.
--
--  d) The fixed constants from dll.e have been "moved" to psym.e, to
--      allow them to be used before dll.e has been "auto-included".
--
--
-- NOTE:
-- =====
--  This has been carefully engineered to match RDS Eu, whether that 
--  is for the best or not. For example:
--
--          function f(atom a)
--              ?a
--              return 1
--          end function
--          constant cb_f = call_back(routine_id("f")),
--                   cp_f = define_c_proc({},cb_f,{C_INT})
--          c_proc(cp_f,{-1})
--
--  Displays 4294967295 (=#FFFFFFFF), not -1. Commented out code 
--   exists below to change this behaviour, if needed.              [DEV did I change that?][11/9/16 YES!! - DEV/DOC!]
--  See also test/t42cback.exw for further notes.
--
--
-- WARNING: [DEV add this to the docs]
-- =======
--  Phix allows eg c_func(WriteConsole,{stdout,"hello\n",6,pBytes,0}) 
--  whereas in RDS/OpenEu you must allocate_string/free. However, a  
--  few C functions modify such strings/data directly, eg
--              c_func(ReadFile,{handle,strbuf,...})
--  and in that case */THE REFCOUNT OF STRBUF IS COMPLETELY IGNORED/*.
--
--  Obviously (legacy) code which performs allocate/c_func/peek/free 
--  does exactly what you might expect, but in eg:
--
--      sequence table, a, strbuf
--          table = {0}
--          a = repeat(' ',8192)
--          table[1] = a
--          ...
--          strbuf = table[1]
--          c_func(ReadFile,{handle,strbuf,...})
--
--  then strbuf, a, and table[1] are */ALL/* modified in the one hit.
--    (pfileio.e does that sort of thing quite deliberately, btw)
--
--  There is, in fact, no (sensible) way to change this behaviour, 
--  aside from forcing the use of allocate/free.
--
--DEV I have now dropped support for this (legacy code would have been using allocate_string anyway):
--  Also, wherever you can pass a string you can pass a dword-sequence
--  equivalent ("fred" vs {'f','r','e','d'}). Should you pass such a
--  buffer to ReadFile, then (see argstring below) c_func will quietly 
--  convert it to an 8-bit string, then (let the C code) modify that, 
--  before just completely discarding the modified 8-bit version.
--
--  Naturally this is all a warning for "new style" code and even then 
--  only for a very select few external C functions; to avoid any such
--  effects simply allocate and peek/free after the call, in the same 
--  way that legacy code would/had to.
--

--!/**/without debug
--!/**/with debug

include builtins\VM\pHeap.e     -- :%pStoreFlt etc
include builtins\VM\pStack.e    -- :%opFrame etc
--include builtins\VM\pDiagN.e  -- e02atdb0 (DEV/now in pUnassigned)
--include builtins\VM\pprntfN.e
--include builtins\VM\pUnassigned.e -- :%pRTErn (DEV/togo)
include builtins\VM\pUnassigned.e   -- opCallOnceYeNot etc
--include builtins\VM\pcallfunc.e

#ilASM{ jmp !opCallOnceYeNot }

constant e16cbchop      = 16    -- call_backs cannot have optional parameters
constant e72iri         = 72    -- invalid routine_id
constant e73atodmbs     = 73    -- argument to open_dll must be string
constant e74dcfpe       = 74    -- define_c_func/proc parameter error
constant e75cbrpmaba    = 75    -- call back routine parameters must all be atoms
--constant e81ipicfp        = 81    -- insufficient parameters in call_func/proc()
constant e84cbpmbropr   = 84    -- call_back parameter must be routine_id or {'+',routine_id}
constant e88atcfpmbaos  = 88    -- arguments to c_func/proc must be atoms or strings
--constant e89tmpicfp   = 89    -- too many parameters in call_func/proc()
constant e116rrnp       = 116   -- routine requires %d parameters, not %d
constant e117rdnrav     = 117   -- routine does not return a value
constant e118rrav       = 118   -- routine returns a value

procedure fatalN(integer level, integer errcode, integer ep1=0, integer ep2=0)
-- level is the number of frames to pop to obtain an era (must be >=2).
-- we report errors on (eg) the c_func call, not in c_func below, so
-- obviously c_func itself calls fatalN(2..), whereas if c_func calls
-- toString, that must then call fatalN(3..), and when open_dll calls
-- OpenOneDLL, which calls toString which finally calls this, it must 
-- do so with call fatalN(4..). There are no fatalN(1..) calls since
-- this is local and that would report an error in pcfunc.e itself,
-- which is the very thing the level parameter is supposed to avoid!
    #ilASM{
        [32]
            mov ecx,[level]
            mov eax,[errcode]
            mov edi,[ep1]
            mov esi,[ep2]
          @@:
            mov edx,[ebp+16]    -- era
--??        mov edx,[ebp+12]    -- called from address (or just use :!fatalN...)
            mov ebp,[ebp+20]    -- (nb no local vars after this!)
            sub ecx,1
            jg @b
            sub edx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
            int3
        [64]
            mov rcx,[level]
            mov rax,[errcode]
            mov rdi,[ep1]
            mov rsi,[ep2]
          @@:
            mov rdx,[rbp+32]    -- era
--??        mov rdx,[rbp+24]    -- called from address
            mov rbp,[rbp+40]    -- (nb no local vars after this!)
            sub rcx,1
            jg @b
            sub rdx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
            int3
        []
          }
    ?9/0
end procedure

function toString(sequence name, integer errcode, integer level)
-- Explicitly convert a dword-sequence to an 8-bit string
-- errcode is one of the constants defined above (e73..e88)
string res
integer nlen
object ch
    nlen = length(name)
    res = repeat(' ',nlen)
    for i=1 to nlen do
        ch = name[i]
        if atom(ch) then
            ch = and_bits(ch,#FF)
            res[i] = ch
        else
            fatalN(level,errcode)
        end if
    end for
    return res
end function

function OpenOneDLL(sequence filename)
atom res
    if not string(filename) then
        --DEV add to test/terror: open_dll({'u','s','e','r',"32.dll"}) -- e73atodmbs
        filename = toString(filename,e73atodmbs,4)
    end if
    #ilASM{
        [PE32]
            mov eax,[filename]
            push ebx    --(=0) (for fild qword)
            shl eax,2
            push eax                            -- lpLibFileName
            call "kernel32.dll","LoadLibraryA"
            push eax
            lea edi,[res]
            fild qword[esp]
            add esp,8
            call :%pStoreFlt                    -- ([edi]:=ST0)
        [PE64]
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whatever alignment we started with
            mov rcx,[filename]
            sub rsp,8*5         -- minimum 4 param shadow space, and align
            shl rcx,2                           -- lpLibFileName
            call "kernel32.dll","LoadLibraryA"
            mov [rsp],rax
            lea rdi,[res]
            fild qword[rsp]
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
            call :%pStoreFlt    -- ([rdi]:=ST0)
        [ELF32]
            mov eax,[filename]
--          push 1              -- flags (RTLD_LAZY)
            push 0x00101        -- flags (RTLD_GLOBAL|RTLD_LAZY)
            shl eax,2           -- ref->raw
            push eax            -- library name
            call "libdl.so.2", "dlopen"
            add esp,8
            lea edi,[res]
            call :%pStoreMint   -- [edi]:=eax, as float if rqd
        [ELF64]
            mov rax,[filename]
--          push 1              -- flags (RTLD_LAZY)
            push 0x00101        -- flags (RTLD_GLOBAL|RTLD_LAZY)
            shl rax,2           -- ref->raw
            push rax            -- library name
            call "libdl.so.2", "dlopen"
            add rsp,16
            lea rdi,[res]
            call :%pStoreMint   -- [rdi]:=rax, as float if rqd
        []
          }
    return res
end function

global function open_dll(sequence filename)
atom res = 0
sequence fi
    if length(filename)>0 and atom(filename[1]) then
        res = OpenOneDLL(filename)
    else
        -- A list of filenames: try each one in turn
        for idx=1 to length(filename) do
            fi = filename[idx]
            res = OpenOneDLL(fi)
            if res!=0 then exit end if
        end for
    end if
    return res
end function

global function get_proc_address(atom lib, string name)
--
-- Low-level wrapper of kernel32/GetProcAddress and libdl/dlsym.
-- used by define_c_func/define_c_proc/define_c_var and for
-- runtime interpretation of inline assembly
-- Applications would not normally use this directly.
--
atom addr
    #ilASM{
        [32]
            mov eax,[lib]
            mov edx,[name]
            call :%pLoadMint
            shl edx,2
        [PE32]
            push edx                            -- lpProcName
            push eax                            -- hModule
            call "kernel32.dll","GetProcAddress"
        [ELF32]
            push edx                            -- symbol
            push eax                            -- handle
            call "libdl.so.2", "dlsym"
            add esp,8
        [32]
--          push ebx    --(=0, for fild qword)
--          push eax
--          lea edi,[addr]
--          fild qword[esp]
--          add esp,8
--          call :%pStoreFlt                    -- ([edi]:=ST0)
--DEV tryme:
            lea edi,[addr]
            call :%pStoreMint
        [PE64]
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whatever alignment we started with

            mov rax,[lib]
            mov r15,h4
            sub rsp,8*5                         -- minimum 4 param shadow space, and align
            mov rdx,[name]
            call :%pLoadMint
            shl rdx,2                           -- lpProcName
            mov rcx,rax                         -- hModule
            call "kernel32.dll","GetProcAddress"
            lea rdi,[addr]
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
            call :%pStoreMint
        [ELF64]
            mov rdx,[name]
            mov rax,[lib]
            shl rdx,2
            call :%pLoadMint
            push rdx                            -- symbol
            push rax                            -- handle
            call "libdl.so.2", "dlsym"
            add rsp,16
            lea rdi,[addr]
            call :%pStoreMint
        []
          }
    return addr
end function

procedure check(object o, integer level)
-- (if any more types are added here, ensure c_func copes with the new return type,
--  also see that routine for some possible future types we might one day need.)
--/* not thread safe??
    if not find(o, {C_CHAR,C_UCHAR,
                    C_SHORT,C_USHORT,
                    C_INT,C_UINT,
                -- (C_LONG    = C_INT,
                --  C_ULONG   = C_UINT,
                --  C_POINTER = C_ULONG,)
--                  C_FLOAT,C_DOUBLE,
                    C_FLOAT,C_DOUBLE}) then
--                  E_INTEGER,E_ATOM,
--                  E_SEQUENCE,E_OBJECT}) then
--*/
    if  o!=C_CHAR
    and o!=C_UCHAR
    and o!=C_SHORT
    and o!=C_USHORT
    and o!=C_INT
    and o!=C_UINT
    and o!=C_FLOAT
    and o!=C_DOUBLE then
        fatalN(level,e74dcfpe)
    end if
end procedure

--  table = append(table,{name,addr,args,return_type,convention})
constant T_name         = 1,  -- (kept for ref/debugging purposes only)
         T_address      = 2,
         T_args         = 3,
         T_return_type  = 4,
         T_convention   = 5

--DEV (posted as a challenge)
--if T_name!=1 then ?9/0 end if -- suppress warning (no code is generated)
--if T_name then end if -- same problem

constant STDCALL = 1,
         CDECL   = 2 -- T_convention values

--DEV not thread safe! (critical section should be fine). Also, a cleanup (or delete) routine might be nice...
sequence table      -- all defined c_func/procs {{name,addr,args,return_type,convention}}

sequence previd,    -- table of all previous call_backs created (id)
         prevcb     -- result for each "" (the memory address)

integer tinit
        tinit = 0

integer tcs         -- (added 25/11/16)
--DEV...
integer tmax = 0    -- length(table), but thread-safe.
--(integer fdmax = 0    -- length(fdtbl), but thread-safe.)

--procedure resett()
--  table = {}
--  previd = {}
--  prevcb = {}
--end procedure

procedure c_cleanup()
    enter_cs(tcs)
    if tinit then
        for i=1 to length(prevcb) do
            free(prevcb[i])
        end for
--      resett()
        tinit = 0
    end if
    leave_cs(tcs)
end procedure

procedure Tinit()
--  resett()
    -- Note: this /should/ be thread safe, because pThread.e calls it!
    tinit = 1
    tcs = init_cs()
    table = {}
    tmax = 0
    previd = {}
    prevcb = {}
--DEV tryme (and get rid of :%SetCCleanup) [erm, will not work because not "final"...]
--  table = delete_routine(table,routine_id("c_cleanup"))
--!/*
    --
    -- We need to invoke c_cleanup() in the MCHK branch of p.exw, 
    --  when interpreting [not compiling] and it is about to call
    --  check_heap(), to prevent the above three tables from being 
    --  reported as memory leaks. MCHK is 0 in all shipped verions.
    --
    #ilASM{
        jmp :setcleanup
        --
        -- These nops are here because we are storing ::CCleanup/4,
        -- so a "shr 2; call" may land up to 3 bytes early(!!)
        --  (and no, an align feature is not in the plan)
        --
        nop
        nop
        nop
    ::CCleanup
      [32]
        mov edx,routine_id(c_cleanup)       -- mov edx,imm32 (sets K_ridt)
        mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[c_cleanup][S_Ltot])
        call :%opFrame
        mov dword[ebp+16],:cleanupret       -- return address
        jmp $_il                            -- jmp code:c_cleanup
      [64]
        mov rdx,routine_id(c_cleanup)       -- mov rdx,imm32 (sets K_ridt)
        mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[c_cleanup][S_Ltot])
        call :%opFrame
        mov qword[rbp+32],:cleanupret       -- return address
        jmp $_il                            -- jmp code:c_cleanup
      []
      ::cleanupret
        ret
        
    ::setcleanup
    [32]
        call :%pGetSymPtr           -- (mov esi,[ds+8])
        mov edx,[esi+21*4]          -- edx:=symtab[T_EBP=22]
        test edx,edx
        jz @f
            -- interpreted
            mov eax,:CCleanup
            shl eax,2
            call :%SetCCleanup      -- see pStack.e
      @@:
    [64]
        call :%pGetSymPtr           -- (mov rsi,[ds+8])
        mov rdx,[rsi+21*8]          -- rdx:=symtab[T_EBP=22]
        test rdx,rdx
        jz @f
            -- interpreted
            mov rax,:CCleanup
            shl rax,2
            call :%SetCCleanup      -- see pStack.e
  @@:
    []
          }
--!*/
end procedure
--if not tinit then Tinit() end if  -- (not necessary)

global function define_c_func(object lib, object fname, sequence args, atom return_type)
--
-- Define the characteristics of either:
--  * a C function in a dll or .so file, or
--  * a machine-code routine at a given address.
--
--  lib may be an atom, the address of the library containing the function, normally
--      from open_dll(), in which case fname must be a sequence, eg:
--
--          constant kernel32 = open_dll("kernel32"),
--                   xBeep = define_c_func(kernel32,"Beep",{C_INT,C_INT},C_INT)
--          ...
--              if c_func(xBeep,{250,500}) then end if  -- 250Hz for 500ms
--
--  lib may instead be {} or "", in which case fname must be an atom, ie a machine 
--      code routine, either from inline assembly (recommended), or memory from 
--      allocate() that has had x86 binary poked into it (the legacy way), eg:
--
--          -- The recommended way:
--          integer jraddr
--          #ilASM {jmp :fin
--                  ::justret
--                      -- (params at eg [esp+4]/[rsp+8])
--                      ret -- (or ret nn)
--                  ::fin
--                      mov [jraddr],:justret   -- (assumes justret<1GB)
--                 }
--          constant ret1 = define_c_proc({},jraddr,{})
--          ...
--          -- The legacy way:
--          constant mem1 = allocate(1)
--              poke(mem1,#C3)  -- ret
--          constant ret2 = define_c_proc({},mem1,{})
--          ...
--              c_proc(ret1,{})
--              c_proc(ret2,{})
--
--      (of course c_proc/func can be used on both library and machine routines)
--
--  name may be prefixed by '+' to use the CDECL calling convention:
--      CDECL is the default/only calling convention on a Linux system.
--      STDCALL is the default in a Windows system.
--          c_func({},{'+',<atom memory_address>},...)
--      defines a machine code routine to be called using CDECL,
--          c_func(<atom lib>,"+FunctionName",...)
--      or
--          c_func(<atom lib>,{'+',"FunctionName"},...) [NOT RDS Eu compatible]
--      define a dll/.so routine to be called using CDECL.
--
--      The CDECL convention requires the callee to clean up the stack, and is 
--      therefore more suited to variable numbers of arguments, whereas with 
--      STDCALL the routine cleans up the stack before returning.
--
--  args should be a sequence of C_CHAR..E_OBJECT (as per check() above),
--
--  return_type should be one of C_CHAR..E_OBJECT, or 0 for define_c_proc.
--
--  The file test\t42cback.exw is (or should be) the most comprehensive set
--  or examples, but there are many others, such as demo/winwire, or even 
--  just the Beep/DeleteFile in p.exw which may be easier to start with.
--
--  Returns a positive integer (aka routine_id, an index into the tables
--  above), which should be used as the first argument to c_func (or c_proc),
--  or -1 is returned if the routine could not be located.
--
integer nlen
object name
integer convention
atom addr
integer res
integer level = 2+(return_type=0)

    convention = STDCALL
    if platform()!=WIN32 then
        convention = CDECL
    end if
    name = fname
    if sequence(fname) then
        -- check for a '+' prefix:
        nlen = length(fname)
        if nlen=0 then fatalN(level,e74dcfpe) end if
        if fname[1]='+' then
            convention = CDECL
            if nlen<=1 then fatalN(level,e74dcfpe) end if
            if atom(lib) then
                if nlen=2 and sequence(name[2]) then
                    -- eg {'+',"fred"} (RDS Eu incompatible)
                    name = name[2]
                else
                    -- "+fred" format                   
                    name = name[2..-1]
                end if
            else
                -- CDECL m/c must always be {'+',<atom addr>}
                if nlen!=2 then fatalN(level,e74dcfpe) end if
                name = name[2]
                if not atom(name) then fatalN(level,e74dcfpe) end if
            end if
        end if
    end if
--  addr = 0
    if sequence(lib) then
        if length(lib)
        or not atom(name) then
            fatalN(level,e74dcfpe)
        end if
        addr = name
    else -- atom(lib)
        if not sequence(name) then
            fatalN(level,e74dcfpe)
        elsif not string(name) then
            name = toString(name,e74dcfpe,3) --DEV better messsage
        end if
        addr = get_proc_address(lib,name)
        if addr=NULL then return -1 end if
    end if

    --
    -- Validate the args and return type
    --
    for i=1 to length(args) do
        check(args[i],2+(return_type=0))
    end for
    if return_type then
        check(return_type,2)
    end if

    if not tinit then Tinit() end if
--DEV locking...
    enter_cs(tcs)
    table = append(table,{name,addr,args,return_type,convention})
    tmax = length(table)
    res = tmax
    leave_cs(tcs)
    return res
end function

global function define_c_proc(object lib, object name, sequence args)
--
-- Define the characteristics of either:
--  * a C function with a VOID return type / ignored return value, or
--  * a machine-code routine at a given address.
--
    return define_c_func(lib, name, args, 0)
end function

global function define_c_var(atom lib, sequence name)
--
-- Get the address of a public C variable defined in a dll or .so file.
--
atom addr
    if not string(name) then
        name = toString(name,e74dcfpe,3)
    end if
    addr = get_proc_address(lib,name)
-- we may want this?:
--  if addr=0 then return -1 end if
    return addr
end function

-- keep these matching pglobals.e!
constant S_NTyp     = 2,
         S_sig      = 7,    -- routine signature, eg {'F',T_integer} (nb S_sig must be = S_vtype)
--       S_Parm1    = 8,    -- first parameter. (idx to symtab, then follow S_Slink)
         S_ParmN    = 9,    -- min no of parameters. (max is length(S_sig)-1))
--       S_Ltot     = 10,   -- total no of parameters + locals (for stack frame allocation)
--       S_il       = 11,   -- intermediate code (also backpatch list)
--       S_Tidx     = 9,
         S_Type     = 6,
         S_Func     = 7,
--       S_Proc     = 8,
         T_atom     = 3,
         T_object   = 15,
         T_const1   = 26,
         DEBUG      = 0


--/*
procedure :%cbhandler(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    #ilASM{
            :%cbhandler
            -----------
        [32]
                pushad

                -- [esp] is saved edi (from the pushad)
                -- [esp+4] is saved esi
                -- [esp+8] is saved ebp
                -- [esp+12] is saved esp
                -- [esp+16] is saved ebx
                -- [esp+20] is saved edx    -- (now used for prevebp)
                -- [esp+24] is saved ecx
                -- [esp+28] is saved eax    -- (save of symtab[routineno], then result)
                -- [esp+32] is return address into stdcb clone (ret nnn instruction)
                -- [esp+36] is routine no   -- (also used to save result addr)
                -- [esp+40] another return address [into C code, probably]
                -- [esp+44] params

                -- (same for both STDCALL and CDECL, cmiiw)

                xor ebx,ebx
                -- restore ebp (from last call()/c_func()/c_proc())
                xor edx,edx             -- edx:=0
                call :%pSetSaveEBP      -- (eax<-pTCB.SaveEBP<-edx, all regs trashed)
                test eax,eax
                jz @f
--15/9/16:
--mov edx,ebp
                    mov ebp,eax
--push eax
--call :%pSetSaveEBP
--pop eax
              @@:
                mov [esp+20],eax        -- prevebp
                mov edx,[esp+36]        -- rtnid
                call :%pGetSymPtr       -- (mov esi,[ds+8])
                mov esi,[esi+edx*4-4]   -- esi:=symtab[rtnid]
                mov [esp+28],esi        -- save symtab[rtnid] (in eax after popad)
                mov edi,[ebx+esi*4+32]  -- edi:=esi[S_ParmN=9]
                push edi                -- [1] push edi (no of params [min==max])
                mov ecx,[ebx+esi*4+36]  -- ecx:=esi[S_Ltot=10]
                --
                -- Create a frame:
                --
                --  ecx ==> number of params+locals
                --  edx ==> routineNo to call
                --
                call :%opFrame
                --
                -- Set params, converting any big 32-bit values to floats
                --
                mov edi,ebp             -- (addr first param)
                pop ecx                 -- [1] pop ecx (no of params)
                test ecx,ecx
                jz :zeroparams
                    lea esi,[esp+44]    -- params (on stack)
                ::paramloop
                    lodsd               --  eax:=[esi], esi+=4
                    cmp eax,h4
                    jb :paramstosd      -- (0..#3FFFFFFF)
--DEV this is viable, treating #FFFFFFFF as -1, but disagrees with RDS Eu...
                    cmp eax,#C0000000
                    jae :paramstosd     -- (#C0000000..#FFFFFFFF)
--DEV this loads it unsigned (agreeing with RDS Eu)....
--                  push ebx            -- (=0)
--                  push eax
--                  fild qword[esp]
--                  add esp,8
--DEV ... whereas this loads it signed (disagreeing with RDS Eu).
                    fild dword[esi-4]
                    call :%pStoreFlt    -- ([edi]:=ST0)
                    jmp :nextparam
                ::paramstosd
                    mov [edi],eax
                ::nextparam
                    sub edi,4
                    sub ecx,1
                    jnz :paramloop
            ::zeroparams
                mov esi,[esp+28]        -- restore symtab[rtnid]
                mov dword[ebp+16],:retaddr
                jmp dword[ebx+esi*4+40] -- execute first opcode (S_il=11)
            ::retaddr
                mov edx,[esp+20]        -- prevebp
--15/9/16:
--test edx,edx
--jz @f
                push eax
                call :%pSetSaveEBP      -- (eax<-pTCB.SaveEBP<-edx, all regs trashed)
                pop eax
--@@:
                -- result is in eax, but >31bit stored as a float
                cmp eax,h4 --DEV :%pLoadMint
                jl :retint
                    cmp byte[ebx+eax*4-1],0x12          -- (ebx==0)
                    je @f
                        mov esi,[esp+28]                -- restore symtab[rtnid]
                        mov al,80                       -- e80cbrna(esi)
                        mov edx,[ebx+esi*4+40]          -- first opcode (S_il=11)
                        mov esi,[esp+36]                -- routine no
                        jmp :!iDiag
                        int3
                  @@:
                    sub esp,8
                    fld qword[ebx+eax*4]
                    fistp qword[esp]
                    sub dword[ebx+eax*4-8],1
                    jnz @f
                        mov edx,eax
                        push dword[esp+40]              -- era
                        call :%pDealloc0
                  @@:
                    pop eax
                    add esp,4
            ::retint
                mov [esp+28],eax        -- keep eax, but
                popad                   -- restore all other registers
                ret 4                   -- (the dword pushed by template code)

            -- end of cbhandler (32 bit)

            [64]
                -- rax is routine number
                -- [rsp] is return address into stdcb clone (ret nnn instruction)
                -- [rsp+8] another return address [into C code, probably]
                -- PE64 only, not ELF64:
                -- [rsp+16] params: shadow space: rcx/[rsp+16]
                --                                rdx/[rsp+24]
                --                                r8 /[rsp+32]
                --                                r9 /[rsp+40]
                -- (same for both STDCALL and CDECL, cmiiw)

--[PE64]
--15/2/16:
                -- rax/rcx/rdx/r8/r9/r10/r11 are damaged, as are xmm0..5 and st0..7
                -- rbx/rbp/rdi/rsi/r12/r13/r14/r15 are preserved (as are xmm6..15)
                push rbx
                push rbp
                push rdi
                push rsi
                push r12
                push r13
                push r14
                push r15
                xor rbx,rbx
            [PE64]
                mov [rsp+16+64],rcx
                mov [rsp+24+64],rdx
                mov [rsp+32+64],r8
                mov [rsp+40+64],r9
            [64]

                push rax                -- routine number
                xor rdx,rdx             -- rdx:=0
                call :%pSetSaveEBP      -- (rax<-pTCB.SaveEBP<-rdx, all regs trashed)
                call :%pGetSymPtr       -- (mov rsi,[ds+8])
                pop rdx                 -- routine number
                test rax,rax
                jz @f
                    mov rbp,rax
              @@:
                push rax                --[0] saved ebp

                mov rsi,[rsi+rdx*8-8]   -- rsi:=symtab[rtnid]
                push rsi                --[1] save symtab[rtnid]
                mov rdi,[rbx+rsi*4+64]  -- rdi:=rsi[S_ParmN=9]
                mov rcx,[rbx+rsi*4+72]  -- rcx:=rsi[S_Ltot=10]
                push rdi                --[2] push rdi (no of params [min==max])

                call :%opFrame          -- (with rcx==number of params+locals, rdx==routine no)

                --
                -- Set params, converting any big 32-bit values to floats
                --
                mov rdi,rbp             -- (addr first param)
                pop rcx                 --[2] pop rcx (no of params)
                mov r15,h4
                test rcx,rcx
                jz :zeroparams
                    lea rsi,[rsp+32+64] -- params (on stack)
                ::paramloop
                    lodsq               --  rax:=[rsi], rsi+=8
                    cmp rax,r15
                    jb :paramstosd      -- (0..#3FFFFFFF)
--DEV this is viable, treating #FFFFFFFF as -1, but disagrees with RDS Eu...
--                  cmp eax,#C0000000
--                  jae :paramstosd     -- (#C0000000..#FFFFFFFF)
--DEV this loads it unsigned (agreeing with RDS Eu)....
--pop al    -- DEV done to here...
----                push ebx            -- (=0)
--                  push rax
--                  fild qword[rsp]
--                  add rsp,8
--DEV ... whereas this loads it signed (disagreeing with RDS Eu).       [DEV seems like I'm going this route...]
                    fild qword[rsi-8]
                    call :%pStoreFlt                    -- ([rdi]:=ST0)
                    jmp :nextparam
                ::paramstosd
                    mov [rdi],rax
                ::nextparam
                    sub rdi,8
                    sub rcx,1
                    jnz :paramloop
            ::zeroparams
                pop rsi                 --[1] symtab[rtnid]
                push rdx                --[1] routine No
                mov qword[rbp+32],:retaddr64
                jmp dword[rbx+rsi*4+80] -- execute first opcode (S_il=11)
            ::retaddr64
                pop rsi                 --[1] routine No
                pop rdx                 --[0] saved ebp
                push rax
                call :%pSetSaveEBP      -- (rax<-pTCB.SaveEBP<-rdx, all regs trashed)
                pop rax             

                -- result is in rax, but >63bit stored as a float
                mov r15,h4
                cmp rax,r15
                jl :retint
                    sub rsp,8
                    cmp byte[rbx+rax*4-1],0x12          -- (ebx==0)
                    je @f
                        mov al,80                       -- e80cbrna
                        mov rcx,rsi                     -- routine no
                        call :%pGetSymPtr               -- (mov rsi,[ds+8])
                        mov rsi,[rsi+rcx*8-8]           -- rsi:=symtab[rtnid]
                        mov rdx,[rbx+rsi*4+80]          -- first opcode (S_il=11)
                        mov rsi,rcx
                        jmp :!iDiag
                        int3
                  @@:
                    fld tbyte[rbx+rax*4]
                    fistp qword[rsp]
                    sub qword[rbx+rax*4-16],1
                    jnz @f
                        mov rdx,rax
                        push qword[rsp+24]
                        call :%pDealloc0
                  @@:
                    pop rax
            ::retint
                pop r15
                pop r14
                pop r13
                pop r12
                pop rsi
                pop rdi
                pop rbp
                pop rbx
                ret

            -- end of cbhandler (64-bit)

            []
        }

global function call_back(object id)
--
-- Get a (32-bit) machine address for calling a routine
--
-- id should be a result from routine_id, or on Windows the same as part
--  of a {'+',id} pair, if the routine uses CDECL calling convention.
--
-- Note that all parameters must be declared as atoms, and will receive an      --DEV check/test this...
--  unsigned 32-bit value. Use "integer" instead of "atom" at your own risk;
--  eg -1, might, at best, appear as 4,294,967,295 (#FFFFFFFF), at worst it 
--  could be (especially if type checking/debug is turned off) a raw address
--  or bit-shifted Phix reference, which will undoubtedly trigger a machine
--  exception. If you find any kind of bug when using integer parameters on 
--  a call_back, the "fix" will be "use atom".
--
-- Repeated invocations of this routine do not "leak" memory
--  (unlike the RDS Eu version of this routine).
--
-- Technical point:
--  You should always "call" a call_back, rather than "jmp" to one, since
--  the code below expects a return address at the top of the stack, before 
--  any parameters. That may sound obvious, and is mentioned only in order
--  to specifically say that you cannot optimise "push push call ret" to
--  to "push push jmp", although "popx push push pushx jmp" might be ok.
--
integer k, siNTyp, sigi, noofparams
sequence symtab, sig
object si
atom r
integer convention

    k = 0
    if not tinit then
        Tinit()
    else
        k = find(id,previd)
    end if
    if k=0 then
        convention = STDCALL
        if platform()!=WIN32 then
            convention = CDECL
        end if
        if sequence(id) then
            if length(id)!=2
            or id[1]!='+'
            or sequence(id[2]) then
                fatalN(2,e84cbpmbropr) -- call_back parameter must be routine_id or {'+',routine_id}
            end if
            convention = CDECL
            id = id[2]
        end if
--      si = 1  -- callstack not rqd
        #ilASM{
            [32]
                lea edi,[symtab]
                call :%opGetST  -- [edi]:=symtab (see pStack.e)
            [64]
                lea rdi,[symtab]
                call :%opGetST  -- [rdi]:=symtab (see pStack.e)
            []
              }
        if not integer(id)
        or id<=T_const1             -- (no real routines that far down there mate!)
        or id>length(symtab) then   -- (nor any "" "" after the end of the symtab!)
            fatalN(2,e72iri,id)
        end if
        si = symtab[id]
        if atom(si) then
            fatalN(2,e72iri,id)
        end if
        siNTyp = si[S_NTyp]
        if siNTyp!=S_Func
        and siNTyp!=S_Type then
            fatalN(2,e72iri,id)
        end if
        sig = si[S_sig]
        for i=2 to length(sig) do
            sigi = sig[i]
            if sigi>T_atom then
                while sigi>T_object do
                    -- follow the type chain to get the builtin type, eg
                    --  type hour(integer h) return h>=0 and h<=23 end type
                    --  type evening(hour h) return h>=18 and h<=23 end type
                    -- then this loop does evening->hour->integer.
                    if DEBUG then
                        if symtab[sigi][S_NTyp]!=S_Type then ?9/0 end if
                    end if
                    sigi = symtab[sigi][S_sig][2]
                end while
--30/04/2011 (for iup)
--              if sigi>T_atom then
                if and_bits(sigi,T_atom)=0 then
                    fatalN(2,e75cbrpmaba) -- call back routine parameters must all be atoms
                end if
            end if
        end for
        -- length(sig)-1 is the max, ParmN the min. Verify max==min.
        -- In theory this could be a warning, with call_back assuming 
        --  the max, but safer as an error. Besides, it is not exactly 
        --  difficult to make fixed-var thin wrapper(s), ie/eg:
        --      function varfunc(a=1,b=2)... end function
        --      --constant cb = call_back(routine_id("varfunc")) -- error!
        --      function fixed0() return varfunc() end function
        --      function fixed1(a) return varfunc(a) end function
        --      function fixed2(a,b) return varfunc(a,b) end function
        --      constant cb0 = call_back(routine_id("fixed0"))
        --      constant cb1 = call_back(routine_id("fixed1"))
        --      constant cb2 = call_back(routine_id("fixed2"))
        -- (Though normally there would be only one callback required.)
        noofparams = si[S_ParmN]
        if noofparams!=length(sig)-1 then
            fatalN(2,e16cbchop) -- call_backs cannot have optional parameters
        end if
--SUG: should we check the return type for non-atom as well? (warning)

--DEV Needs DEP handling. [test]
        r = allocate(16)    -- STDCALL needs 13 bytes, CDECL 11 (round up to 4 dwords):
                            --  push 0x05060708     ; 68 08070605 (routine number)
                            --  call #030000000     ; E8 03000000
                            --  ret 0x9090          ; C2 9090   (or C3 nop nop for CDECL)
                            --                      (total of 13 (or 11) bytes)
        #ilASM{
            [32]
                mov eax,[r]
                call :%pLoadMint
                mov edi,eax
--                      push 84281096     ;#00458B4D: 150 08070605  -- push routine no
--                      call #00458C1D    ;#00458B52: 350 C6000000  -- call :%cbhandler
--                      ret 1234          ;#00458B57: 302 D204      -- ret [imm32]
                mov eax,:%cbhandler
                mov ecx,[id]
                sub eax,10
                mov byte[edi],0o150     -- push (#68)
                mov [edi+1],ecx         --      imm32 (routine no)
                sub eax,edi
                mov byte[edi+5],0o350   -- call (#E8)
                mov [edi+6],eax         --      off32 (:%cbhandler)
                mov ecx,[noofparams]
                cmp [convention],STDCALL
                -- sug: setcc -> C2/C3? (and always store cx anyway)
                jne :cdecl
                    shl ecx,2
                    mov byte[edi+10],0o302  -- ret (#C2)
                    mov word[edi+11],cx
                    jmp @f
              ::cdecl
                    mov byte[edi+10],0o303  -- ret (#C3)
              @@:
            [64]
                mov rax,[r]
                call :%pLoadMint
                mov rdi,rax
            [PE64]
                -- Grant execute access (DEV would be better to allocate a single large block)
                --                       (perhaps something like arwen/Quick_Allocations.ew)
                mov rcx,rsp -- put 2 copies of rsp onto the stack...
                push rsp
                push rcx
                or rsp,8    -- [rsp] is now 1st or 2nd copy:
                            -- if on entry rsp was xxx8: both copies remain on the stack
                            -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                            -- obviously rsp is now xxx8, whatever alignment we started with
                sub rsp,8*5         -- minimum 4 param shadow space, flOldProtect, and align
                lea r9,[rsp+32]                     -- lpflOldProtect
                mov r8,#40                          -- flNewProtect (PAGE_EXECUTE_READWRITE)
                mov rdx,16                          -- dwSize
                mov rcx,rdi                         -- lpAddress
                call "kernel32.dll","VirtualProtect"    -- (preserves rdi,rsi)
--              add rsp,8*5
--              pop rsp
                mov rsp,[rsp+8*5]   -- equivalent to the add/pop
            [64]
--                      mov eax,84281096  ;#0063390A: 270 08070605               uv 01 00  1 184      
--                      call #00633915    ;#0063390F: 350 01000000               v  00 00  1 184      
--                      ret               ;#00633914: 303                        np 00 00  2 185      
                mov rax,:%cbhandler
                mov rcx,[id]
                sub rax,10
                mov byte[rdi],0o270     -- mov eax, (#B8)
                mov dword[rdi+1],ecx    --          imm32 (routine no)
                sub rax,rdi
                mov byte[rdi+5],0o350   -- call (#E8)
--DEV... (we might want to mov ecx,imm64; mov eax,id; call ecx; ret, and/or try jmp ecx...)
--       (maybe: mov ecx,:%cbhandler; mov eax,id; call ecx - but that might not work for dlls)
--              mov dword[rdi+11],rax   --      off32 (:%cbhandler)
--              mov dword[rdi+11],eax   --      off32 (:%cbhandler)
                mov dword[rdi+6],eax    --      off32 (:%cbhandler)
--              mov byte[rdi+5],0o303   -- ret (#C3)
                mov byte[rdi+10],0o303  -- ret (#C3)
--          [ELF64]
--              pop al  -- for certain, the above "mov [rsp+16+64],rcx" etc is wrong... (as marked) [DEV]
            []
        }
--DEV locking (added 25/11/16)
        enter_cs(tcs)
        previd = append(previd,id)
        prevcb = append(prevcb,r)
        leave_cs(tcs)
    else
        r = prevcb[k]
    end if
    return r
end function

global procedure call(atom addr)
integer prev_ebp4 -- (stored /4)
    #ilASM{
                e_all                                       -- set "all side-effects"
            [32]
                -- first, save ebp in case of a callback:
                mov edx,ebp
                call :%pSetSaveEBP          -- (eax<-pTCB.SaveEBP<-edx, all regs trashed)
                shr eax,2
                mov [prev_ebp4],eax
--DEV (pTrace.e)
--              call %opClrDbg                              -- clear debug screen if needed
                mov eax,[addr]
                call :%pLoadMint
                call eax
                xor ebx,ebx
                mov edx,[prev_ebp4]
                shl edx,2
                call :%pSetSaveEBP          -- (eax<-pTCB.SaveEBP<-edx, all regs trashed)

            [64]
                mov rdx,rbp
                call :%pSetSaveEBP          -- (rax<-pTCB.SaveEBP<-rdx, all regs trashed)
                shr rax,2
                mov [prev_ebp4],rax
--DEV (pTrace.e)
--              call :%opClrDbg                             -- clear debug screen if needed
                mov rax,[addr]
                call :%pLoadMint
            [PE64]
                mov rcx,rsp -- put 2 copies of rsp onto the stack...
                push rsp
                push rcx
                or rsp,8    -- [rsp] is now 1st or 2nd copy:
                            -- if on entry rsp was xxx8: both copies remain on the stack
                            -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                            -- obviously rsp is now xxx8, whatever alignment we started with
                sub rsp,8*5
            [64]
                call rax
                xor rbx,rbx
            [PE64]
--              add rsp,8*5
--              pop rsp
                mov rsp,[rsp+8*5]   -- equivalent to the add/pop
            [64]
                mov rdx,[prev_ebp4]
                shl rdx,2
                call :%pSetSaveEBP          -- (rax<-pTCB.SaveEBP<-rdx, all regs trashed)

            []
        }
end procedure


constant FUNC = 1, PROC = 0

function c_common(integer rid, sequence args, integer flag)
-- common code for c_func and c_proc (validate and process args)
--  flag is FUNC or PROC accordingly.
sequence argdefs
integer argdefi
integer convention
integer la, lad
object argi
integer return_type
object name -- for debugging only
sequence tr -- table[rid], ie {name,addr,args,return_type,convention}
atom addr
--DEV/temp:
string memory_corruption = "pcfunc.e: memory corruption at #"
string pGtcb4eq = ", pGtcb*4=#"
string diffis = ", diff="
integer esp4

    #ilASM{ e_all               -- set "all side-effects"
--DEV
--          call %opClrDbg      -- clear debug screen if needed
        [32]
            mov eax,esp
            shr eax,2
            mov [esp4],eax
        [64]
            mov rax,rsp
            shr rax,2
            mov [esp4],rax
          }

--DEV (temp, getting e36loaaind on lnx, presumably "table" has been clobbered)
    #ilASM{
        [ELF32]
            mov eax,[table]
            cmp byte[ebx+eax*4-1],#80
            je @f
                lea esi,[ebx+eax*4-4]
                push esi
                mov edi,[memory_corruption] -- "pcfunc.e:memory corruption at #"
                call :%puts1
                mov edx,[esp]
                push 0                      -- no cr
                call :%puthex32
                mov edi,[pGtcb4eq]          -- ", pGtcb*4=#"
                call :%puts1
                call :%pGetpGtcb
                mov edx,eax
                shl edx,2
                push edx
                push 0
                call :%puthex32
                mov edi,[diffis]            -- ", diff="
                call :%puts1
                pop ecx
                mov edx,[esp]
                push 1
                sub edx,ecx
                call :%puthex32
--              pop esi
--DEV/SUG (in VM\puts1.e)
--              jmp :%terminate
                int3
          @@:
        []
          }
--  if tinit=0 or rid<1 or rid>length(table) then
    if tinit=0 or rid<1 or rid>tmax then
        fatalN(3,e72iri,rid)
    end if
--DEV locking?? (maybe something like pfileio?) [untried, refcounts anyway...]
    enter_cs(tcs)
    tr = table[rid]
--  #ilASM{
--      [32]
--          mov edx,[rid]
--          shl edx,2
--        @@:
--          mov edi,[table]
--          mov esi,[edi*4+edx-4]       -- esi:=table[rid]
--          cmp edi,[table]
--          jne @b
--          add dword[ebx+esi*4-8],1    -- incref
--          mov [tr],esi
--      [64]
--          mov rdx,[rid]
--          shl rdx,3
--        @@:
--          mov rdi,[table]
--          mov rsi,[rdi*4+rdx-8]       -- rsi:=table[rid]
--          cmp rdi,[table]
--          jne @b
--          add qword[rbx+rsi*4-16],1   -- incref
--          mov [tr],rsi
--        }
    name = tr[T_name]
    addr = tr[T_address]
    argdefs = tr[T_args]
    return_type = tr[T_return_type]
    convention = tr[T_convention]
    tr = {}
    leave_cs(tcs)

    --20/8/15: (ensure shadow space and align)
    if machine_bits()=64 then
        --DEV actually, this should be more like pHeap.e/pGetMem... (nah, this shd be fine...)
        la = length(args)
        if la<5 then
            args &= repeat(0,5-la)
            argdefs &= repeat(#01000004,5-la)
        elsif remainder(la,2)!=1 then
            args &= 0
            argdefs &= #01000004    -- (C_INT)
        end if
        #ilASM{
            [PE64]
                mov rcx,rsp -- put 2 copies of rsp onto the stack...
                push rsp
                push rcx
                or rsp,8    -- [rsp] is now 1st or 2nd copy:
                            -- if on entry rsp was xxx8: both copies remain on the stack
                            -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                            -- obviously rsp is now xxx8, whatever alignment we started with
--          sub rsp,8*5                             -- minimum 4 param shadow space, and align/somewhere to save rax
--          mov [rsp+32],rax                        -- save rax (required length)
--          call "kernel32.dll","GetProcessHeap"
--          mov r8,[rsp+32]                         -- dwBytes (p3)
--          mov rdx,rbx                             -- dwFlags (p2,0)
--          mov rcx,rax                             -- hHeap (p1)
--          call "kernel32.dll","HeapAlloc"
--          add rsp,8*5
--          pop rsp
--          mov rsp,[rsp+8*5]   -- equivalent to the add/pop

--          [ELF64]
--              pop al
            []              
              }
    end if

    la = length(args)
    lad = length(argdefs)
    if la!=lad then
        -- e116rrnp: routine requires %d parameters, not %d
        fatalN(3,e116rrnp,lad,la)
    end if
    if flag=FUNC then
        if return_type=0 then fatalN(3,e117rdnrav) end if
    else -- flag=PROC
        if return_type!=0 then fatalN(3,e118rrav) end if
    end if

    for i=la to 1 by -1 do
        argi = args[i]
        argdefi = argdefs[i]
        if integer(argi) then
            if find(argdefi,{
                             #01000001,     -- C_CHAR
                             #02000001,     -- C_UCHAR
                             #01000002,     -- C_SHORT (a 16 bit signed integer)
                             #02000002,     -- C_USHORT (a 16 bit signed integer)
                             #01000004,     -- C_INT
                             #02000004      -- C_UINT == C_ULONG, C_POINTER, C_PTR
                            }) then
                #ilASM{
                        [32]
                            mov edx,[argi]
                            push edx
                        [64]
--                          mov rdx,[argi]  --DEV everywhere
--                          push rdx
                            push [argi]
                        []
                    }
--DEV todo...
            elsif argdefi=#03000008                                 -- C_DOUBLE
               or (argdefi=#03000004 and machine_bits()=64) then    -- C_FLOAT
--          elsif argdefi=#03000008 then                            -- C_DOUBLE
                #ilASM{
                        [32]
                            sub esp,8
                            fild dword[argi]
                            fstp qword[esp]
                        [64]
                            sub rsp,8
                            fild qword[argi]
                            fstp qword[rsp]
                        []
                    }
                -- (technically this should probably be done just before the "call rax" in c_func/proc,
                --  but as we won't damage them (ie xmm0..xmm3/7) before that, this should be fine.)
                if machine_bits()=64 then
                    if i<=4 then
                        if i=1 then
                            #ilASM{
                                    [64]
                                        movsd xmm0,qword[rsp]
                                    []
                                  }
                        elsif i=2 then
                            #ilASM{
                                    [64]
                                        movsd xmm1,qword[rsp]
                                    []
                                  }
                        elsif i=3 then
                            #ilASM{
                                    [64]
                                        movsd xmm2,qword[rsp]
                                    []
                                  }
                        elsif i=4 then
                            #ilASM{
                                    [64]
                                        movsd xmm3,qword[rsp]
                                    []
                                  }
                        else
                            ?9/0
                        end if
                        #ilASM{
                                [64]
                                    add rsp,8
                                []
                              }
--DEV elsif??
                    elsif platform()=LINUX and i<=8 then
                        if i=5 then
                            #ilASM{
                                    [64]
                                        movsd xmm4,qword[rsp]
                                    []
                                }
                        elsif i=6 then
                            #ilASM{
                                    [64]
                                        movsd xmm5,qword[rsp]
                                    []
                                }
                        elsif i=7 then
                            #ilASM{
                                    [64]
                                        movsd xmm6,qword[rsp]
                                    []
                                }
                        elsif i=8 then
                            #ilASM{
                                    [64]
                                        movsd xmm7,qword[rsp]
                                    []
                                }
                        else
                            ?9/0
                        end if
                        #ilASM{
                                [64]
                                    add rsp,8
                                []
                              }
                    elsif argdefi=#03000004 then
                        #ilASM{
                                [64]
                                    fld qword[rsp]
-- removed 25/11/16:
--                                  add rsp,4
                                    fstp dword[rsp]
                                []
                              }
                    end if
                end if
            elsif argdefi=#03000004 then -- (and machine_bits()=32) -- C_FLOAT
--DEV todo:
--if machine_bits()=32 then
                #ilASM{
                        [32]
                            sub esp,4
                            fild dword[argi]
                            fstp dword[esp]
--                      [64]
--                          sub rsp,4
--                          fild qword[argi]
--                          fstp dword[rsp]
                        []
                    }
--DEV todo:
--/*
else
                #ilASM{
                        [64]
                            sub rsp,8
                            fild qword[argi]
                            fstp dword[rsp]
                        []
                    }
                -- (technically this should probably be done just before the "call rax" in c_func/proc,
                --  but as we won't damage them (ie xmm0..xmm3/7) before that, this should be fine.)
                if machine_bits()=64 then
                    if i<=4 then
                        if i=1 then
                            #ilASM{
                                    [64]
                                        movsd xmm0,qword[rsp]
                                    []
                                  }
                        elsif i=2 then
                            #ilASM{
                                    [64]
                                        movsd xmm1,qword[rsp]
                                    []
                                  }
                        elsif i=3 then
                            #ilASM{
                                    [64]
                                        movsd xmm2,qword[rsp]
                                    []
                                  }
                        elsif i=4 then
                            #ilASM{
                                    [64]
                                        movsd xmm3,qword[rsp]
                                    []
                                  }
                        else
                            ?9/0
                        end if
                        #ilASM{
                                [64]
                                    add rsp,8
                                []
                              }
                    elsif platform()=LINUX and i<=8 then
                        if i=5 then
                            #ilASM{
                                    [64]
                                        movsd xmm4,qword[rsp]
                                    []
                                }
                        elsif i=6 then
                            #ilASM{
                                    [64]
                                        movsd xmm5,qword[rsp]
                                    []
                                }
                        elsif i=7 then
                            #ilASM{
                                    [64]
                                        movsd xmm6,qword[rsp]
                                    []
                                }
                        elsif i=8 then
                            #ilASM{
                                    [64]
                                        movsd xmm7,qword[rsp]
                                    []
                                }
                        else
                            ?9/0
                        end if
                        #ilASM{
                                [64]
                                    add rsp,8
                                []
                              }
                    end if
end if
--*/
            else
                ?9/0
            end if
        elsif atom(argi) then
            if find(argdefi,{
                             #01000004,     -- C_INT
                             #02000004      -- C_UINT == C_ULONG, C_POINTER, C_PTR
                            }) then
                #ilASM{
                        [32]
--                          mov edx,[argi]      --DEV :%pLoadMint
--                          sub esp,8
--                          fld qword[ebx+edx*4]
--                          fistp qword[esp]
--                          pop edx
--                          add esp,4
--                          push edx
                            mov eax,[argi]
                            call :%pLoadMint
                            push eax
                        [64]
--                          mov rdx,[argi]
--                          sub rsp,8
--                          fld tbyte[rbx+rdx*4]
--                          fistp qword[rsp]
                            mov rax,[argi]
                            call :%pLoadMint
                            push rax
                        []
                    }
--DEV as above
            elsif argdefi=#03000008     -- C_DOUBLE
               or (argdefi=#03000004 and machine_bits()=64) then
                #ilASM{
                        [32]
                            mov edx,[argi]
                            sub esp,8
                            fld qword[ebx+edx*4]
                            fstp qword[esp]
                        [64]
                            mov rdx,[argi]
                            sub rsp,8
                            fld tbyte[rbx+rdx*4]
                            fstp qword[rsp]
                        []
                    }
                if machine_bits()=64 then
                    if i<=4 then
                        if i=1 then
                            #ilASM{
                                    [64]
                                        movsd xmm0,qword[rsp]
                                    []
                                }
                        elsif i=2 then
                            #ilASM{
                                    [64]
                                        movsd xmm1,qword[rsp]
                                    []
                                }
                        elsif i=3 then
                            #ilASM{
                                    [64]
                                        movsd xmm2,qword[rsp]
                                    []
                                }
                        elsif i=4 then
                            #ilASM{
                                    [64]
                                        movsd xmm3,qword[rsp]
                                    []
                                }
                        else
                            ?9/0
                        end if
                        #ilASM{
                                [64]
                                    add rsp,8
                                []
                              }
                    elsif platform()=LINUX and i<=8 then
                        if i=5 then
                            #ilASM{
                                    [64]
                                        movsd xmm4,qword[rsp]
                                    []
                                }
                        elsif i=6 then
                            #ilASM{
                                    [64]
                                        movsd xmm5,qword[rsp]
                                    []
                                }
                        elsif i=7 then
                            #ilASM{
                                    [64]
                                        movsd xmm6,qword[rsp]
                                    []
                                }
                        elsif i=8 then
                            #ilASM{
                                    [64]
                                        movsd xmm7,qword[rsp]
                                    []
                                }
                        else
                            ?9/0
                        end if
                        #ilASM{
                                [64]
                                    add rsp,8
                                []
                              }
                    elsif argdefi=#03000004 then
                        #ilASM{
                                [64]
                                    fld qword[rsp]
-- removed 25/11/16:
--                                  add rsp,4
                                    fstp dword[rsp]
                                []
                              }
                    end if
                end if -- machine_bits()=64
            elsif argdefi=#03000004 then -- (and machine_bits()=32) -- C_FLOAT
                #ilASM{
                        [32]
                            mov edx,[argi]
                            sub esp,4
                            fld qword[ebx+edx*4]
                            fstp dword[esp]
--                          [64]
--                              mov rdx,[argi]
--                              sub rsp,8
--                              fld tbyte[rbx+rdx*4]
--                              fstp qword[rsp]
                        []
                    }
            else
                ?9/0
            end if
        elsif string(argi) then
--      elsif string(argi) and argdefi!=C_WIDEPTR then
            if DEBUG then
                if not find(argdefi,{
                                     #02000004      -- C_UINT == C_ULONG, C_POINTER, C_PTR
                                    }) then ?9/0 end if
            end if
            -- NB recount-agnostic; see WARNING above.
            #ilASM{
                    [32]
                        mov edx,[argi]
                        shl edx,2       -- (convert to raw address)
                        push edx
                    [64]
                        mov rdx,[argi]
                        shl rdx,2       -- (convert to raw address)
                        push rdx
                    []
                }
        else
            if DEBUG then
                if not find(argdefi,{
                                     #02000004      -- C_UINT == C_ULONG, C_POINTER, C_PTR
--                                   #12000004      -- C_WIDEPTR [DEV]
                                    }) then ?9/0 end if
            end if
            fatalN(3,e88atcfpmbaos,flag)
        end if
    end for
--DEV/SUG??
--  enter_cs(tcs)
--  name = 0
--  argdefs = 0 -- {}
--  leave_cs(tcs)
    return {return_type,addr,esp4}
end function

--DEV clutching at straws... (not thread safe) [did not help]
--integer static_ebp4 = 0

global function c_func(integer rid, sequence args={})
integer return_type
atom addr
object res
integer esp4        -- Note: esp4 has fairly obvious use: save/restore value of e/rsp,
                    --       easier&safer than "add e/rsp,length(args)*machine_word()",
integer prev_ebp4   --       whereas this is subtler: maintain the e/rbp for call_backs
                    --       and in fact has nothing to do with save/restore of e/rbp,
                    --       at least not in any direct sense.

    {return_type,addr,esp4} = c_common(rid,args,FUNC)

--(DEV: delete once all types are handled)
    if not find(return_type,{
--                           C_CHAR,        -- #01000001
                             C_UCHAR,       -- #02000001
                             C_SHORT,       -- #01000002
                             C_USHORT,      -- #02000002
                             C_INT,         -- #01000004    -- == C_LONG
                             C_UINT,        -- #02000004    -- == C_ULONG, C_POINTER, C_PTR
                             C_FLOAT,       -- #03000004
                             C_DOUBLE,      -- #03000008
--                               E_INTEGER,     -- #06000004    \
--                               E_ATOM,        -- #07000004     \ (Should probably be reserved
--                               E_SEQUENCE,    -- #08000004     /  for RDS Eu-built dlls)
--                               E_OBJECT,      -- #09000004    /
-- possibly:
--C:\Program Files\Phix\psym.e:1304 --  initialConstant("P_REF",          #04000004)??
--                               P_INTEGER,     -- #04010004    \
--                               P_ATOM,        -- #04030004     \ (ie reserved for Phix-built
--                               P_STRING,      -- #04040004      > dlls, if we ever get round
--                               P_SEQUENCE,    -- #040C0004     /  to creating them that is!)
--                               P_OBJECT,      -- #040F0004    /
--                           ""}) then      -- (dummy entry)
                             $}) then       -- (dummy entry)
--      printf(1,"c_func: return type %08x not yet coded/tested\n",return_type)
        ?9/0    -- return type not yet coded/tested!
--DEV e15?
    end if

    --
    -- Call the routine and convert result (in eax/ST0) to a Phix ref:
    --
    #ilASM{
            [32]
                mov edx,ebp
                call :%pSetSaveEBP      -- (eax<-pTCB.SaveEBP<-edx, all regs trashed)
                shr eax,2
                mov [prev_ebp4],eax

                mov eax,[addr]
                call :%pLoadMint

--no help:
--[ELF32]
--shr ebp,2
--mov [static_ebp4],ebp
--xor ebp,ebp
----push eax
----xor eax,eax
--xor ecx,ecx
--xor edx,edx
--xor esi,esi
--xor edi,edi
--[32]
                call eax
--[ELF32]
--mov ebp,[static_ebp4]
--shl ebp,2
--[32]

                mov ecx,[esp4]
                shl ecx,2
                mov edx,[return_type]
                mov esp,ecx

                cmp edx,0x01000004  -- (C_INT [== C_LONG])
                jne @f
                    cmp eax,h4
                    jb :intres          -- (0..#3FFFFFFF)
                    cmp eax,#C0000000
                    jae :intres         -- (#C0000000..#FFFFFFFF)
                        push eax
                        fild dword[esp]
                        pop eax -- (discard)
                        jmp :cstore
            @@:
                cmp edx,0x02000004  -- (C_UINT [== C_ULONG, C_POINTER, C_PTR])
                jne @f
                    cmp eax,h4
                    jb :intres          -- (0..#3FFFFFFF)
--                  cmp eax,#C0000000
--                  jae :intres         -- (#C0000000..#FFFFFFFF)
                    push ebx            -- (0)
                    push eax
                    fild qword[esp]
                    add esp,8
                    jmp :cstore
            @@:
                cmp edx,0x03000004  -- (C_FLOAT)
                je :cstore
                cmp edx,0x03000008  -- (C_DOUBLE)
                jne @f
            ::cstore
                    lea edi,[res]
                    call :%pStoreFlt                    -- ([edi]:=ST0)
                    jmp :done
            @@:
                cmp edx,0x01000002  -- (C_SHORT)
                jne @f
                    cwde                                    -- (ax->eax) [in range -32768..32767]
                    jmp :intres
            @@:
                cmp edx,0x02000001  -- (C_UCHAR)
                jne @f
                    and eax,0xFF
                    jmp :intres
            @@:
                cmp edx,0x02000002  -- (C_USHORT)
                jne @f
                    and eax,0x0000FFFF
                    jmp :intres
            @@:
--              cmp edx,0x01000001  -- (C_CHAR)
--              jne @f
--                  cbw                                     -- (al->ax)
--                  cwde                                    -- (ax->eax) [in range -128..127]
--                  jmp :intres
--          @@:
--              cmp edx,0x02000001  -- (C_UCHAR)
--              jne @f
--                  and eax,0xFF
--                  jmp :intres
--          @@:
                int3
                call :%e02atdb0
--DEV e15 instead

            ::intres                                        -- intres:
                mov [res],eax
            ::done                                          -- done:
                mov edx,[prev_ebp4]
                shl edx,2
                call :%pSetSaveEBP      -- (eax<-pTCB.SaveEBP<-edx, all regs trashed)

            [64]
                mov rdx,rbp
                call :%pSetSaveEBP      -- (rax<-pTCB.SaveEBP<-rdx, all regs trashed)
                shr rax,2
                mov [prev_ebp4],rax

                mov rax,[addr]
                call :%pLoadMint
--[PE64]    
                -- (xmm0..3 are set in c_common; one or more of
                --  these may actually be garbage, but no matter.)
                mov rcx,[rsp]
                mov rdx,[rsp+8]
                mov r8,[rsp+16]
                mov r9,[rsp+24]
--[ELF64]?
                call rax

                mov rcx,[esp4]
                shl rcx,2
                mov rdx,[return_type]
                mov rsp,rcx

                cmp rdx,0x01000004  -- (C_INT [== C_LONG])
                jne @f
--                  cmp rax,h4
--                  mov r15,h4
                    cmp rax,r15
                    jb :intres          -- (0..#3FFFFFFFFFFFFFFF)
--                  mov r14,#C000000000000000
                    mov r14,#C0000000 --0000 0000
                    shl r14,32
                    cmp rax,r14
                    jae :intres         -- (#C0000000..#FFFFFFFF)
                        push rax
                        fild qword[rsp]
                        pop rax -- (discard)
                        jmp :cstore
            @@:
                cmp rdx,0x02000004  -- (C_UINT [== C_ULONG, C_POINTER, C_PTR])
                jne @f
--                  cmp rax,h4
--                  mov r15,h4
                    cmp rax,r15
                    jb :intres          -- (0..#3FFFFFFFFFFFFFFF)
--                  cmp eax,#C000000000000000
--                  jae :intres         -- (#C0000000..#FFFFFFFF)
--                  push rax
--                  fild qword[rsp]
--                  add rsp,8
                    -- to load unsigned, right shift rax by 1, save odd bit in rcx, then *2+[0|1]
                    mov rcx,rbx
                    shr rax,1
                    rcl rcx,1
                    push rax
                    push rcx
                    fild qword[rsp]
                    fild qword[rsp+8]
                    add rsp,16
                    fadd st0,st0
                    faddp
                    jmp :cstore
            @@:
                cmp rdx,0x03000004  -- (C_FLOAT)
                je :cstorexmm0
                cmp rdx,0x03000008  -- (C_DOUBLE)
                jne @f
            ::cstorexmm0
-- 14/2/16: (certainly C_DOUBLE, not necessarily C_FLOAT?) [25/2, I think it's the same]
                    sub rsp,8
                    movsd qword[rsp],xmm0
                    fld qword[rsp]
                    add rsp,8
-- (14/2/16 ends)
            ::cstore
                    lea rdi,[res]
                    call :%pStoreFlt                    -- ([rdi]:=ST0)
                    jmp :done
            @@:
                cmp rdx,0x01000002  -- (C_SHORT)
                jne @f
                    cwde                                    -- (ax->eax) [in range -32768..32767]
--                  and rax,0xFFFF
                    cdqe                                    -- (nb does not list right in FDBG)
                    jmp :intres                             -- ( - but it seems fine in x64dbg)
            @@:
                cmp rdx,0x02000001  -- (C_UCHAR)
                jne @f
                    and rax,0x00FF
                    jmp :intres
            @@:
                cmp rdx,0x02000002  -- (C_USHORT)
                jne @f
                    and rax,0x0000FFFF
                    jmp :intres
            @@:
--              cmp rdx,0x01000001  -- (C_CHAR)
--              jne @f
--                  cbw                                     -- (al->ax)
--                  cwde                                    -- (ax->eax) [in range -128..127]
--                  cdqe                                    -- (eax->rax)
--                  jmp :intres
--          @@:
--              cmp rdx,0x02000001  -- (C_UCHAR)
--              jne @f
--                  and rax,0xFF
--                  jmp :intres
--          @@:
                call :%e02atdb0
--DEV e15 instead

            ::intres                                        -- intres:
                mov [res],rax
            ::done                                          -- done:
                mov rdx,[prev_ebp4]
                shl rdx,2
                call :%pSetSaveEBP      -- (rax<-pTCB.SaveEBP<-rdx, all regs trashed)

            []
        }
--DEV overkill??
--  enter_cs(tcs)
--  addr = 0
--  leave_cs(tcs)
    return res
end function

global procedure c_proc(integer rid, sequence args={})
integer return_type
atom addr
integer esp4        -- Note: esp4 has fairly obvious use: save/restore value of e/rsp,
                    --       easier&safer than "add e/rsp,length(args)*machine_word()",
integer prev_ebp4   --       whereas this is subtler: maintain the e/rbp for call_backs
                    --       and in fact has nothing to do with save/restore of e/rbp,
                    --       at least not in any direct sense.

    {return_type,addr,esp4} = c_common(rid,args,PROC)

    -- (return_type has already been tested for 0 in c_common)

    #ilASM{
        [32]
            mov edx,ebp
            call :%pSetSaveEBP      -- (eax<-pTCB.SaveEBP<-edx, all regs trashed)
            shr eax,2
            mov [prev_ebp4],eax

            mov eax,[addr]
            call :%pLoadMint

            call eax

            mov ecx,[esp4]
            xor ebx,ebx     -- (Phix likes it zero!)
            shl ecx,2
            mov edx,[prev_ebp4]
            mov esp,ecx
            shl edx,2
            call :%pSetSaveEBP      -- (eax<-pTCB.SaveEBP<-edx, all regs trashed)

        [64]
            mov rdx,rbp
            call :%pSetSaveEBP      -- (rax<-pTCB.SaveEBP<-rdx, all regs trashed)
            shr rax,2
            mov [prev_ebp4],rax

            mov rax,[addr]
            call :%pLoadMint
--push rbp
--[PE64]
            -- (xmm0..3 are set in c_common; one or more of
            --  these may actually be garbage, but no matter.)
            mov rcx,[rsp]
            mov rdx,[rsp+8]
            mov r8,[rsp+16]
            mov r9,[rsp+24]
--[ELF64]
            call rax
--pop rbp
            mov rcx,[esp4]
            xor rbx,rbx     -- (Phix likes it zero!)
            shl rcx,2
            mov rdx,[prev_ebp4]
            mov rsp,rcx
            shl rdx,2
            call :%pSetSaveEBP      -- (rax<-pTCB.SaveEBP<-rdx, all regs trashed)

        []
          }
--DEV overkill??
--  enter_cs(tcs)
--  addr = 0
--  leave_cs(tcs)
end procedure


