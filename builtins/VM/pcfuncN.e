--
-- pcfuncN.e (Phix compatible 0.6.3)
--
--  Phix implementations of define_c_func, define_c_proc, define_c_var,
--                          open_dll, call_back, c_func, and c_proc.
--
--  WARNING: Fragile code ahead! Mistakes in this code may completely
--           spanner diagnostics and tracing. You have been warned.
--           (If p -test, p edita, p test\terror, and p test\trace
--            all seem fine, then you can breathe easy once again.)
--
--/*
    ?9/0    This file is wholly incompatible with RDS/OpenEuphoria.
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
--   exists below to change this behaviour, if needed.              [DEV did I change that?]
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
--include platform.e
include builtins\VM\pUnassigned.e   -- :%pRTErn (DEV/temp)

constant ASALPHANUM = 1,    --(specifically the chars we expect in a dll entry point; 1-9/A-Z/a-z/_)
         ASANY = 0      --(ie assume any and all atoms are ok)
                        --(all atoms are and'ed with #FF, in both cases, btw) 

constant e16cbchop      = 16    -- call_backs cannot have optional parameters
constant e72iri         = 72    -- invalid routine_id
constant e73atodmbs     = 73    -- argument to open_dll must be string
constant e74dcfpe       = 74    -- define_c_func/proc parameter error
constant e75cbrpmaba    = 75    -- call back routine parameters must all be atoms
constant e81ipicfp      = 81    -- insufficient parameters in call_func/proc()
constant e84cbpmbropr   = 84    -- call_back parameter must be routine_id or {'+',routine_id}
constant e88atcfpmbaos  = 88    -- arguments to c_func/proc must be atoms or strings
constant e89tmpicfp     = 89    -- too many parameters in call_func/proc()
constant e116rrnp       = 116   -- routine requires %d parameters, not %d
constant e117rdnrav     = 117   -- routine does not return a value
constant e118rrav       = 118   -- routine returns a value

--DEV remove, as per pfileioN.e
--/*
procedure fatal(integer errcode, integer ep1=0)
    #ilASM{
        [32]
            mov eax,[errcode]
            mov edi,[ep1]
            xor esi,esi     -- ep2 unused
            call :%pRTErn   -- fatal error (see pdiagN.e)
        [64]
            mov rax,[errcode]
            mov rdi,[ep1]
            xor rsi,rsi     -- ep2 unused
            call :%pRTErn   -- fatal error (see pdiagN.e)
        []
          }
    ?9/0
end procedure
--*/

-- new version, to replace the above...
procedure fatalN(integer level, integer errcode, integer ep1=0, integer ep2=0)
-- level is the number of frames to pop to obtain an era (must be >=2).
-- we report errors on (eg) the c_func call, not in c_func below, so
-- obviously c_func itself calls fatalN(2..), whereas if c_func calls
-- toString, that must then call fatalN(3..), and when open_dll calls
-- OpenOneDLL, which calls toString which finally calls this, it must 
-- do so with call fatalN(4..). There are no fatalN(1..) calls since
-- this is local and that would report an error in pcfuncN.e itself,
-- which is the very thing the level parameter is supposed to avoid!
    #ilASM{
        [32]
            mov ecx,[level]
            mov eax,[errcode]
            mov edi,[ep1]
            mov edi,[ep2]
          @@:
            mov edx,[ebp+16]    -- era
            mov ebp,[ebp+20]    -- (nb no local vars after this!)
            sub ecx,1
            jg @b
            xor esi,esi         -- ep2 unused
            sub edx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
            int3
        [64]
            mov rcx,[level]
            mov rax,[errcode]
            mov rdi,[ep1]
            mov rsi,[ep1]
          @@:
            mov rdx,[rbp+32]    -- era
            mov rbp,[rbp+40]    -- (nb no local vars after this!)
            sub rcx,1
            jg @b
            xor rsi,rsi         -- ep2 unused
            sub rdx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
            int3
        []
          }
    ?9/0
end procedure

function toString(sequence name, integer asAlphanum, integer errcode, integer level)
-- Explicitly convert a dword-sequence to an 8-bit string
-- asAlphanum is ASALPHANUM or ASANY
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
            if asAlphanum=ASALPHANUM then
                if ch<'1'
                or (ch>'9' and ch<'A')
                or (ch>'Z' and ch<'a' and ch!='_')
                or ch>'z' then
                    -- add any special foreign chars (ŠŒŸšœÿÀÁÂÃÄÅÆÇ etc) 
                    --  here as and when needed (if ever).
--                  if not find(ch,"") then
                    ch = {}
--                  end if
                end if
            end if
        end if
        if not atom(ch) then
--          fatal(errcode)
            fatalN(level,errcode)
        end if
    end for
    return res
end function

function OpenOneDLL(sequence filename)
atom res
    if not string(filename) then
        --DEV add to test/terror: open_dll({'u','s','e','r',"32.dll"}) -- e73atodmbs
        filename = toString(filename,ASALPHANUM,e73atodmbs,4)
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
            pop al
        [ELF64]
            pop al
        []
          }
    return res
end function

global function open_dll(sequence filename)
--function fopen_dll(sequence filename)
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

procedure check(object o, integer level)
-- (if any more types are added here, ensure c_func copes with the new return type,
--  also see that routine for some possible future types we might one day need.)
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

procedure Tinit()
    table = {}
    previd = {}
    prevcb = {}
    tinit = 1
end procedure
if not tinit then Tinit() end if

global function define_c_func(object lib, object fname, sequence args, atom return_type)
--function fdefine_c_func(object lib, object fname, sequence args, atom return_type)
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
--      if nlen=0 then fatal(e74dcfpe) end if
        if nlen=0 then fatalN(level,e74dcfpe) end if
        if fname[1]='+' then
            convention = CDECL
--          if nlen<=1 then fatal(e74dcfpe) end if
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
--              if nlen!=2 then fatal(e74dcfpe) end if
                if nlen!=2 then fatalN(level,e74dcfpe) end if
                name = name[2]
--              if not atom(name) then fatal(e74dcfpe) end if
                if not atom(name) then fatalN(level,e74dcfpe) end if
            end if
        end if
    end if
--  addr = 0
    if sequence(lib) then
        if length(lib)
        or not atom(name) then
--          fatal(e74dcfpe)
            fatalN(level,e74dcfpe)
        end if
        addr = name
    else -- atom(lib)
        if not sequence(name) then
--          fatal(e74dcfpe)
            fatalN(level,e74dcfpe)
        elsif not string(name) then
            name = toString(name,ASALPHANUM,e74dcfpe,3) --DEV better messsage
        end if
        #ilASM{
            [32]
--              lea edi,[addr]
                mov eax,[lib]
                mov edx,[name]
                --
                --  If lib is stored as an atom (presumably because it is > 31 bits),
                --  convert it to 32bits in eax. Likewise convert the string ref to
                --  a raw address of the string, by shifting it left two bits.
                --
                cmp eax,h4  --DEV :%pLoadMint
                jl @f
                    sub esp,8
                    fld qword[ebx+eax*4]
                    fistp qword[esp]
                    pop eax
                    add esp,4
              @@:
                shl edx,2
            [PE32]
                push edx                            -- lpProcName
                push eax                            -- hModule
                call "kernel32.dll","GetProcAddress"
            [ELF32]
                pop al
            [32]
                push ebx    --(=0, for fild qword)
                push eax
                lea edi,[addr]
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

                mov rcx,[lib]
                mov r15,h4
                sub rsp,8*5                         -- minimum 4 param shadow space, and align
                mov rdx,[name]
                --
                --  If lib is stored as an atom (presumably because it is > 63 bits),
                --  convert it to 64bits in rax. Likewise convert the string ref to
                --  a raw address of the string, by shifting it left two bits.
                --
                cmp rcx,r15
                jl @f
                    fld tbyte[rbx+rcx*4]
                    fistp qword[rsp]
                    mov rcx,[rsp]
              @@:
                shl rdx,2                           -- lpProcName
                -- (rcx already set)                -- hModule
                call "kernel32.dll","GetProcAddress"
                mov [rsp],rax
                lea rdi,[addr]
                fild qword[rsp]
--              add rsp,8*5
--              pop rsp
                mov rsp,[rsp+8*5]   -- equivalent to the add/pop
                call :%pStoreFlt                    -- ([edi]:=ST0)
            [ELF32]
                pop al
            []
              }
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

--DEV locking...
--  enter_cs()
    if not tinit then Tinit() end if
    table = append(table,{name,addr,args,return_type,convention})
    res = length(table)
--  leave_cs()
    return res
end function

global function define_c_proc(object lib, object name, sequence args)
--<to go>
--
-- Define the characteristics of either:
--  * a C function with a VOID return type / ignored return value, or
--  * a machine-code routine at a given address.
--
    return define_c_func(lib, name, args, 0)
end function

global function define_c_var(atom lib, sequence name)
--function fdefine_c_var(atom lib, sequence name)
--
-- Get the address of a public C variable defined in a dll or .so file.
--
atom addr
    if not string(name) then
        name = toString(name,ASALPHANUM,e74dcfpe,3)
    end if
    #ilASM{
        [32]
--          lea edi,[addr]
            mov eax,[lib]
            mov edx,[name]
            cmp eax,h4 --DEV :%pLoadMint
            jl @f
                sub esp,8
                fld qword[ebx+eax*4]
                fistp qword[esp]
                pop eax
                add esp,4
          @@:
            shl edx,2
        [PE32]
            push edx
            push eax
            call "kernel32.dll","GetProcAddress"
        [ELF32]
            pop al
        [32]
            push ebx    --(=0, for fild qword)
            push eax
            lea edi,[addr]
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
            mov rcx,[lib]
            mov r15,h4
            sub rsp,8*5                         -- minimum 4 param shadow space, and align
            mov rdx,[name]
            cmp rcx,r15
            jl @f
                fld tbyte[rbx+rcx*4]
                fistp qword[rsp]
                mov rcx,[rsp]
          @@:
            shl rdx,2                           -- lpProcName
            -- (rcx already set)                -- hModule
            call "kernel32.dll","GetProcAddress"
            mov [rsp],rax
            lea rdi,[addr]
            fild qword[rsp]
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
            call :%pStoreFlt                    -- ([edi]:=ST0)
        [ELF64]
            pop al
        []
          }
-- we may want this?:
--  if addr=0 then return -1 end if
    return addr
end function

--  ;DEV better:
--  ;   save symtab[i][S_il],
--  ;   addr threadstack[-symtab[i-1][S_Tidx]]
--  ;   [ebpidx] (as current)
--  ;   save/set opFrame stuff: addr first(eax), N(ecx), edi (routineNo)
--  ;   

-- keep these matching pglobals.e!
constant S_NTyp     = 2,
         S_sig      = 7,    -- routine signature, eg {'F',T_integer} (nb S_sig must be = S_vtype)
--       S_Parm1    = 8,    -- first parameter. (idx to symtab, then follow S_Slink)
         S_ParmN    = 9,    -- min no of parameters. (max is length(S_sig)-1))
         S_Ltot     = 10,   -- total no of parameters + locals (for stack frame allocation)
         S_il       = 11,   -- intermediate code (also backpatch list)
--       S_Tidx     = 9,
         S_Type     = 6,
         S_Func     = 7,
         S_Proc     = 8,
         T_atom     = 3,
         T_object   = 15,
         T_const1   = 26,
         DEBUG      = 0


----DEV not thread safe (locking rqd). Also, a cleanup (or delete) routine might be nice...
--sequence previd,  -- table of all previous call_backs created (id)
--       prevcb     -- result for each ""
--integer pinit
--      pinit = 0
--
--procedure Pinit()
--  previd = {}
--  prevcb = {}
--  pinit = 1
--end procedure
--if not pinit then Pinit() end if

    --DEV not thread safe!! (done)
    -- save ebp (/4 to avoid any 31-bit integer issues).
    -- set by every call()/c_func()/c_proc(), which each have was_ebp_xx,
    --  and used in cbhandler (below) /reset to 0 before resuming Phix code.
    -- (ebp is the current frame/callstack, in case you didn't know.)
    ----DEV use TLS: (done)
--integer ebp_save = 0  -- stored /4 to avoid any 31-bit integer issues
--  --
--  #ilASM{ jmp :fin
--  [32]
--    :%save_ebp
--      mov edx,ebp
--      mov eax,[ebp_save]
--      shr edx,2
--    :%restore_ebp
--      mov [ebp_save],edx
--  [64]
--    :%save_rbp
--      mov rdx,rbp
--      mov rax,[ebp_save]
--      shr rdx,2
--    :%restore_rbp
--      mov [ebp_save],rdx
--  []
--      ret
--    ::fin
--}
-- calling convention:
-- integer local_ebp
--  #ilASM{ call :%save_ebp
--          mov [local_ebp],eax
--  ... <some ilASM which might invoke a callback>
--          mov edx,[local_ebp]
--          call :%restore_ebp
--  }
-- These "guards" are used in c_func/c_proc/call, and therefore you need
--  the same if any ilASM is attempting anything similar.
-- You can put hll code between the save and restore, though it is
--  probably best to have such guards cover smallish blocks of code.
-- Note that while you /could/ just inline this where it is called from
--  in this source (pcfunc.e), the whole point is to have just the one 
--  place that needs modifying (eg to use TlsSetValue & TlsGetValue),
--  that is, on top of whatever mods cbhandler needs.
--

global function call_back(object id)
--function fcall_back(object id)
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
--integer d1234
sequence symtab, sig
object si
atom r
integer convention

    k = 0
--  if not pinit then
    if not tinit then
--      Pinit()
        Tinit()
    else
        k = find(id,previd)
    end if
    if k=0 then
--DEV locking
        previd = append(previd,id)
        convention = STDCALL
        if platform()!=WIN32 then
            convention = CDECL
        end if
        if sequence(id) then
            if length(id)!=2
            or id[1]!='+'
            or sequence(id[2]) then
--              fatal(e84cbpmbropr) -- call_back parameter must be routine_id or {'+',routine_id}
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
        or id<=T_const1             -- (no real routines that far down there mate)
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
--                  fatal(e75cbrpmaba) -- call back routine parameters must all be atoms
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


--DEV Needs DEP handling.
--DEV also, we could try the floor(adr/4) trick

--      r = allocate(20)    -- STDCALL needs 18 bytes, CDECL 16 (round up to 5 dwords):
        r = allocate(16)    -- STDCALL needs 13 bytes, CDECL 11 (round up to 4 dwords):
--                          --  push 0x01020304     ; 68 04030201 [DEV no longer rqd]   -- DEV symtab pointer
                            --  push 0x05060708     ; 68 08070605 (routine number)
                            --  call #030000000     ; E8 03000000
                            --  ret 0x9090          ; C2 9090   (or C3 nop nop for CDECL)
--                          --                      (total of 18 (or 16) bytes)
                            --                      (total of 13 (or 11) bytes)

        #ilASM{
            [32]
                mov edi,[r]
                call :tpop              -- (===push :template; jmp tpop)
--      ::template  -- template code, copied to [r] and then patched.
--DEV ebp save no longer rqd. (see save_ebp etc, quite a pain to remove this, though)
--              push #01020304      -- (saved ebp)  [erm, saved symtab ptr]
                push #05060708      -- (routine number)
                call :tpop          -- patched to call cbhandler (relative jump)
                ret 1234            -- ret imm16      (patched to C3 (&nop,nop) for CDECL,
                                    --                  and the imm16 patched for STDCALL.)

--          ::cbhandler
            -----------
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
--X             -- [esp+40] is saved ebp [DEV no longer rqd/used]
--X             -- [esp+44] another return address [into C code, probably]
--X             -- [esp+48] params
                -- [esp+40] another return address [into C code, probably]
                -- [esp+44] params

                -- (same for both STDCALL and CDECL, cmiiw)

--              mov edx,[esp+36]        -- rtnid
--              mov ecx,[esp+36]        -- rtnid
                xor ebx,ebx
                -- restore ebp (from last call()/c_func()/c_proc())
                --  DEV this is not thread safe! (tls rqd?) (reserve say gvar[1]?)
--              mov ecx,[ebp_save]
--              shl ecx,2
--              jz @f
--                  mov ebp,ecx
--            @@:
--                              push ebp                    -- see note[2] below
                xor edx,edx                 -- edx:=0
--mov esi,2
                call :%pSetSaveEBP
                test eax,eax
                jz @f
                    mov ebp,eax
              @@:
--              push eax
                mov [esp+20],eax
--              mov edx,[esp+40]        -- rtnid
                mov edx,[esp+36]        -- rtnid

--              mov esi,[ebp+24]        -- symtab
--14/8/15
--              mov esi,[ds+8]          -- symtab
                call :%pGetSymPtr
--27/2/15:
--^             mov esi,[ebp+24]        -- vsb_root
--^             mov esi,[esi+8]         -- symtabptr
                mov esi,[esi+edx*4-4]   -- esi:=symtab[rtnid]
                mov [esp+28],esi        -- save symtab[rtnid] (in eax after popad)
--              mov [esp+32],esi        -- save symtab[rtnid] (in eax after popad)
                mov edi,[ebx+esi*4+32]  -- edi:=esi[S_ParmN=9]
                push edi                -- [1] push edi (no of params [min==max])
                mov ecx,[ebx+esi*4+36]  -- ecx:=esi[S_Ltot=10]
                --
                -- Create a frame:
                --
                --  ecx ==> number of params+locals
--              --  esi ==> "called from" addr -- opFrame sets this from [esp]
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
--                  lea esi,[esp+48]    -- params (on stack)
                    lea esi,[esp+44]    -- params (on stack)
--                  lea esi,[esp+48]    -- params (on stack)
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

--                  push edx
--DEV (to go) [temporary compatibility kludge, while pwy.exe is 26/08/2012]
--                  mov edx,edi
--                  call %opMovbi       --  call StoreFlt ([edi]:=ST0)
                    call :%pStoreFlt                    -- ([edi]:=ST0)
--                  pop edx
                    jmp :nextparam
                ::paramstosd
                    mov [edi],eax
                ::nextparam
                    sub edi,4
                    sub ecx,1
                    jnz :paramloop
            ::zeroparams
--              mov eax,[ebp_save]
                mov esi,[esp+28]        -- restore symtab[rtnid]
--              mov esi,[esp+32]        -- restore symtab[rtnid]
--              mov [ebp_save],ebx      -- (0)  ; important!
--              mov [esp+28],eax        -- was_ebp_save
                mov dword[ebp+16],:retaddr
                jmp dword[ebx+esi*4+40] -- execute first opcode (S_il=11)
            ::retaddr
--              pop edx
                mov edx,[esp+20]
--              mov ecx,eax
push eax
--mov esi,-2
                call :%pSetSaveEBP
--                              pop ebp                     -- see note[2] below
--              mov eax,ecx
pop eax

                -- result is in eax, but >31bit stored as a float
                cmp eax,h4 --DEV :%pLoadMint
                jl :retint
                    sub esp,8
                    cmp byte[ebx+eax*4-1],0x12          -- (ebx==0)
                    je @f
                        call :%e02atdb0                 -- (DEV better: call back returned non-atom?)
                  @@:
                    fld qword[ebx+eax*4]
                    fistp qword[esp]
                    dec dword[ebx+eax*4-8]
                    jnz @f
                        mov edx,eax
                        push dword[esp+40]              -- era
                        call :%pDealloc0
                  @@:
                    pop eax
                    add esp,4
            ::retint
--              mov edx,[esp+28]        -- was_ebp_save
                mov [esp+28],eax        -- keep eax, but
--              mov [ebp_save],edx      -- restore (important!)
                popad                   -- restore all other registers
--              ret 8                   -- (the two dwords pushed by template code)
                ret 4                   -- (the dword pushed by template code)

            -- end of cbhandler

            ::tpop
                pop esi                 -- (start of template code)
                cmp edi,h4              -- (allocated memory) --DEV :%pLoadMint
                jl @f
                    -- allocated addrs > #3FFFFFFF get stored as 64-bit float:
                    sub esp,8
                    fld qword[ebx+edi*4]
                    fistp qword[esp]
                    pop edi
                    add esp,4
            @@:
--              lea edx,[edi+1]         -- (addr of that #01020304)
                mov edx,edi             -- (save)
--              lea edx,[edi+11]        -- (addr of offset)
                mov ecx,5               -- (5 dwords=20 bytes)
                rep movsd
--              mov ecx,[edx]           -- (to check #01020304)
--DEV should no longer be necessary...
--              mov [edx],ebp
--              mov [d1234],ecx
--              mov eax,[edx+10]        -- (original jump offset)
                mov eax,3               -- (original jump offset)
                sub esi,edi
                mov ecx,[id]
                add eax,esi
--              mov [edx+6],ecx
                mov [edx+1],ecx
--              mov [edx+11],eax        -- (updated jump offset)
                mov [edx+6],eax         -- (updated jump offset)
--              mov [edx],eax           -- (updated jump offset)
                mov ecx,[noofparams]
                cmp [convention],STDCALL
                jne :cdecl
                    shl ecx,2
--                  mov word[edx+16],cx
                    mov word[edx+11],cx
                    jmp @f
              ::cdecl
--                  mov byte[edx+15],#C3
                    mov byte[edx+10],#C3
              @@:
            [64]
                mov rdi,[r]
                call :tpop              -- (===push :template; jmp tpop)
--      ::template  -- template code, copied to [r] and then patched.
--DEV ebp save no longer rqd. (see save_ebp etc, quite a pain to remove this, though)
--              push #01020304          -- (saved ebp)  [erm, saved symtab ptr]
--              push #05060708          -- (routine number)
--              mov edx,#05060708       -- (routine number) [yes, use a 32-bit instruction]     (#BA imm32)
                mov eax,#05060708       -- (routine number) [yes, 32-bit ins, 0-fills rax]      (#B8 imm32)
                call :cbhandler64       -- patched to call cbhandler (relative jump)            (#E8 rel32)
--              ret 1234                -- ret imm16      (patched to C3 (&nop,nop) for CDECL,  (#C2 #04D2)
--                                      --                  and the imm16 patched for STDCALL.) (=13bytes=)
                ret                     -- ret                                                  (#C3)
--                                      --                                                      (=11bytes=)
--              nop
--              nop
--              nop                     -- round it up to 16 bytes

            ::cbhandler64
            -----------
                -- rax is routine number
                -- [rsp] is return address into stdcb clone (ret nnn instruction)
                -- [rsp+8] another return address [into C code, probably]
                -- [rsp+16] params: shadow space: rcx/[rsp+16]
                --                                rdx/[rsp+24]
                --                                r8 /[rsp+32]
                --                                r9 /[rsp+40]
                -- (same for both STDCALL and CDECL, cmiiw)

--[PE64]
                xor rbx,rbx
                mov [rsp+16],rcx
                mov [rsp+24],rdx
                mov [rsp+32],r8
                mov [rsp+40],r9
--14/8/15:
--              mov rsi,[ds+8]          -- symtab (raw)
                call :%pGetSymPtr
--27/2/15:
--^             mov rsi,[rbp+48]        -- vsb_root
--^             mov rsi,[rsi+16]        -- symtabptr
-- DEV this is not thread safe! (tls rqd?)
--              mov rcx,[ebp_save]
--              mov rdx,rax
--              shl rcx,2
--              jz @f
--                  mov rbp,rcx
--            @@:
--                              push ebp                    -- see note[2] below
                mov rcx,rax
                xor rdx,rdx                 -- edx:=0
                call :%pSetSaveEBP
                test rax,rax
                jz @f
                    mov rbp,rax
              @@:
                push rax
                mov rdx,rcx

--              mov rsi,[rsi+rdx*8-8]   -- rsi:=symtab[rtnid]
                mov rsi,[rsi+rcx*8-8]   -- rsi:=symtab[rtnid]
--[ELF64]
                push rsi                --[1] save symtab[rtnid]
--              mov rdi,[rbx+rsi*8+64]  -- rdi:=rsi[S_ParmN=9]
                mov rdi,[rbx+rsi*4+64]  -- rdi:=rsi[S_ParmN=9]
                mov rcx,[rbx+rsi*4+72]  -- rcx:=rsi[S_Ltot=10]
                push rdi                --[2] push edi (no of params [min==max])

                call :%opFrame          -- (with rcx==number of params+locals, rdx==routine no)

                --
                -- Set params, converting any big 32-bit values to floats
                --
                mov rdi,rbp             -- (addr first param)
                pop rcx                 --[2] pop ecx (no of params)
                mov r15,h4
                test rcx,rcx
                jz :zeroparams
--31/12/14:
--                  lea rsi,[rsp+16]    -- params (on stack)
--                  lea rsi,[rsp+24]    -- params (on stack)
                    lea rsi,[rsp+32]    -- params (on stack)
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

--                  push edx
--DEV (to go) [temporary compatibility kludge, while pwy.exe is 26/08/2012]
--                  mov edx,edi
--                  call %opMovbi       --  call StoreFlt ([edi]:=ST0)
                    call :%pStoreFlt                    -- ([edi]:=ST0)
--                  pop edx
                    jmp :nextparam
                ::paramstosd
                    mov [rdi],rax
                ::nextparam
                    sub rdi,8
                    sub rcx,1
                    jnz :paramloop
            ::zeroparams
--              mov rax,[ebp_save]
--              mov rsi,[rsp+?28]       -- restore symtab[rtnid]
                pop rsi                 --[1]
--              mov [ebp_save],rbx      -- (0)  ; important!
--              push rax
                mov qword[rbp+32],:retaddr64
                jmp dword[rbx+rsi*4+80] -- execute first opcode (S_il=11)
            ::retaddr64
                mov rcx,rax
                pop rdx
                call :%pSetSaveEBP
--                              pop ebp                     -- see note[2] below
                mov rax,rcx

                -- result is in rax, but >63bit stored as a float
--              cmp rax,h4
                mov r15,h4
                cmp rax,r15
                jl :retint
                    sub rsp,8
                    cmp byte[rbx+rax*4-1],0x12          -- (ebx==0)
                    je @f
                        call :%e02atdb0                 -- (DEV better: call back returned non-atom?)
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
--                  add esp,4
            ::retint
--              mov edx,[esp+28]        -- was_ebp_save
--              pop rdx
--              mov [esp+28],eax        -- keep eax, but
--              mov [ebp_save],rdx      -- restore (important!)
--              popad                   -- restore all other registers
--              ret 8                   -- (the two dwords pushed by template code)
                ret

            -- end of cbhandler

            ::tpop
                pop rsi                 -- (start of template code)
--              cmp rdi,h4              -- (allocated memory)
                mov r15,h4
                cmp rdi,r15             -- (allocated memory) --DEV :%pLoadMint
                jl @f
                    -- allocated addrs > #3FFFFFFF_FFFFFFFF get stored as 64-bit float:
                    sub rsp,8
                    fld tbyte[rbx+rdi*4]
                    fistp qword[esp]
                    pop rdi
            @@:
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
                mov rdx,rdi             -- (save)
--              lea edx,[edi+11]        -- (addr of offset)
--              mov ecx,5               -- (5 dwords=20 bytes)
                mov rcx,2               -- (2 qwords=16 bytes)
                rep movsq
--              mov ecx,[edx]           -- (to check #01020304)
--DEV should no longer be necessary...
--              mov [edx],ebp
--              mov [d1234],ecx
--              mov eax,[edx+10]        -- (original jump offset)
--              mov rax,3               -- (original jump offset)
                mov rax,1               -- (original jump offset)
                sub rsi,rdi
                mov rcx,[id]
                add rax,rsi
                mov dword[rdx+1],ecx    -- id (as a 32-bit value)
--              mov [edx+11],eax        -- (updated jump offset)
--              mov [edx],eax           -- (updated jump offset)
--              mov dword[rdx+11],eax   -- (updated jump offset,"")
                mov dword[rdx+6],eax    -- (updated jump offset,"")
                mov rcx,[noofparams]
--removed 19/8/15:
--              cmp [convention],STDCALL
--              jne :cdecl
--                  --DEV may need +1 for even no of params/min of 5...
--                  -- (also not sure about STDCALL/CDECL vs 64-bit)
----                    shl ecx,2
--                  shl rcx,3
----                    mov word[rdx+16],cx
--                  mov word[rdx+11],cx
----DEV 19/8/15:
----                    jmp @f
--            ::cdecl
--                  mov byte[rdx+10],#C3
--            @@:
            [ELF64]
                pop al  -- for certain, the above "mov [rsp+16],rcx" etc is wrong...
            []
        }
--      previd = append(previd,id)  -- done above
        prevcb = append(prevcb,r)
    else
        r = prevcb[k]
    end if
    return r
end function

global procedure call(atom addr)
--procedure fcall(atom addr)
integer local_ebp4 -- (stored /4)
    #ilASM{
            [32]
                e_all                                       -- set "all side-effects"
                -- first, save ebp in case of a callback:
--              call :%save_ebp
--              mov [local_ebp],eax
                mov edx,ebp
--mov esi,3
                call :%pSetSaveEBP
                shr eax,2
                mov [local_ebp4],eax
--DEV (pTrace.e)
--              call %opClrDbg                              -- clear debug screen if needed
--              int3
                mov eax,[addr]
                xor ebx,ebx
                cmp eax,h4  --DEV :%pLoadMint
                jl @f
                    sub esp,8
                    fld qword[ebx+eax*4]
                    fistp qword[esp]
                    pop eax
                    add esp,4
            @@:
                call eax
                xor ebx,ebx
--              mov edx,[local_ebp]
--              call :%restore_ebp
                mov edx,[local_ebp4]
                shl edx,2
--mov esi,-3
                call :%pSetSaveEBP

            [64]
                e_all                                       -- set "all side-effects"
--              call :%save_rbp
--              mov [local_ebp],rax
                mov rdx,rbp
                call :%pSetSaveEBP
                shr rax,2
                mov [local_ebp4],rax

--DEV (pTrace.e)
--              call :%opClrDbg                             -- clear debug screen if needed
--              int3
                mov rax,[addr]
--              xor rbx,rbx
--              cmp rax,h4
                mov r15,h4
                cmp rax,r15
                jl @f
                    sub rsp,8
                    fld tbyte[rbx+rax*4]
                    fistp qword[rsp]
                    pop rax
            @@:
--DEV is this just PE64? (it may not really matter)
                sub rsp,8*5
                call rax
                xor rbx,rbx
                add rsp,8*5
--              mov rdx,[local_ebp]
--              call :%restore_rbp
                mov rdx,[local_ebp4]
                shl rdx,2
                call :%pSetSaveEBP

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
--, ch
object argi
string argstring
integer return_type
object name -- for debugging only
sequence tr -- table[rid], ie {name,addr,args,return_type,convention}
atom addr
sequence cstrings -- keeps refcounst>0, of any temps we have to make

    if string(args) then
        args = {args}
    end if
    if tinit=0 or rid<1 or rid>length(table) then
--      fatal(e72iri,rid)
        fatalN(3,e72iri,rid)
    end if
    tr = table[rid]
    name = tr[T_name]
    addr = tr[T_address]
    argdefs = tr[T_args]
    return_type = tr[T_return_type]
    convention = tr[T_convention]

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

            [ELF64]
                pop al
            []              
              }
    end if

    la = length(args)
    lad = length(argdefs)
    if la!=lad then
        -- e116rrnp: routine requires %d parameters, not %d
        fatalN(3,e116rrnp,lad,la)
--/*
        -- e116rrnp4: routine [%s] requires %d parameters, not %d
        #ilASM{
                [32]
                    mov al,116
                    mov edi,[lad]   -- ep1
                    mov esi,[la]    -- ep2
--                  mov edx,[name]  -- ep3  --DEV (we haven't got an ep3, yet)
                    call :%pRTErn   -- fatal error
--DEV (this is :%pRTErn:, see also pfileioN.e for ebp/calledfrom etc)
--      pop edx -- era
--      sub edx,1
--      jmp :!iDiag
--      int3
                [64]
                    mov al,116
                    mov rdi,[lad]   -- ep1
                    mov rsi,[la]    -- ep2
--                  mov rdx,[name]  -- ep3  --DEV (we haven't got an ep3, yet)
                    call :%pRTErn   -- fatal error
                []
            }
--*/
    end if
    if flag=FUNC then
        if return_type=0 then fatalN(3,e117rdnrav) end if
    else -- flag=PROC
        if return_type!=0 then fatalN(3,e118rrav) end if
    end if
    #ilASM{ e_all               -- set "all side-effects"
--DEV
--              call %opClrDbg      -- clear debug screen if needed
--              int3
        }
    cstrings = {}
--DEV 64bit: do we want an initial push rbx for the even no of params case?
--  --> let's do the whole pHeap.e/pGetMem thing... (Erm, see 20/8/15 mods above)

--  if string(args) then
--      -- hmmm, well just push the string chars then...
--      for i=la to 1 by -1 do
--          ch = args[i]
--          argdefi = argdefs[i]
--          if DEBUG then
--              if not find(argdefi,{
--                    -- (These should be fine, but not much point adding them when
--                    --    we've nothing to say whether they work or not...)
----                                         #01000001,     -- C_CHAR
----                                         #01000004,     -- C_INT
----                                         #02000004      -- C_UINT == C_ULONG, C_POINTER, C_PTR
--                                  }) then ?9/0 end if
--          end if
--          #ilASM{
--                  [32]
--                      mov edx,[ch]
--                      push edx
--                  [64]
--                      mov rdx,[ch]
--                      push rdx
--                  []
--              }
--      end for
--  else
        for i=la to 1 by -1 do
            argi = args[i]
            argdefi = argdefs[i]
            if integer(argi) then
                if DEBUG then
                    if not find(argdefi,{
                                         #01000001,     -- C_CHAR
                                         #01000004,     -- C_INT
                                         #02000004      -- C_UINT == C_ULONG, C_POINTER, C_PTR
                                        }) then ?9/0 end if
                end if
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
            elsif atom(argi) then
--DEV switch would be better?
                if find(argdefi,{
                                 #01000004,     -- C_INT
                                 #02000004      -- C_UINT == C_ULONG, C_POINTER, C_PTR
                                }) then
                    #ilASM{
                            [32]
                                mov edx,[argi]      --DEV :%pLoadMint
                                sub esp,8
                                fld qword[ebx+edx*4]
                                fistp qword[esp]
                                pop edx
                                add esp,4
                                push edx
                            [64]
                                mov rdx,[argi]
                                sub rsp,8
                                fld tbyte[rbx+rdx*4]
                                fistp qword[rsp]
                            []
                        }
                elsif find(argdefi,{
                                    #03000004       -- C_FLOAT
                                   }) then
                    #ilASM{
                            [32]
                                mov edx,[argi]
                                sub esp,4
                                fld qword[ebx+edx*4]
                                fstp dword[esp]
                            [64]
--erm... (no self-respecting 64-bit code should be using 32-bit float params...)
                                mov rdx,[argi]
                                sub rsp,8
                                fld tbyte[rbx+rdx*4]
                                fstp qword[rsp]
                            []
                        }
                elsif find(argdefi,{
                                    #03000008       -- C_DOUBLE
                                   }) then
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
                else
                    ?9/0
                end if
            elsif string(argi) then
--              elsif string(argi) and argdefi!=C_WIDEPTR then
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
--                                       #12000004      -- C_WIDEPTR [DEV]
                                        }) then ?9/0 end if
                end if
                -- convert to a string and push that (cstrings ensures these temporaries are
                --  /not/ freed, and the memory reused, until /after/ c_func|c_proc return)
                argstring = toString(argi,ASANY,e88atcfpmbaos,4)
                #ilASM{
                        [32]
                            mov edx,[argstring]
                            shl edx,2   -- (convert to raw address)
                            push edx
                        [64]
                            mov rdx,[argstring]
                            shl rdx,2   -- (convert to raw address)
                            push rdx
                        []
                    }
                cstrings = append(cstrings,argstring)   -- (keep refcounts>0, for a while)
            end if
        end for
--  end if
    return {return_type,addr,cstrings}
end function


global function c_func(integer rid, sequence args={})
--function fc_func(integer rid, sequence args={})
integer return_type
object r
--DEV /4 trick:
integer c_esp_lo = 0    -- save/restore esp (just in case)  [dev it is dword-aligned!]
integer c_esp_hi = 0
integer local_ebp4
atom addr
sequence cstrings -- Keeps refcounts>0, of any temps we had to make, over the call.
                  -- Freed automatically, after we get back from the call_eax, /not/
                  -- on exit from, or mid-way through, calling the c_common routine.

    #ilASM{
            [32]
                mov eax,esp
                mov ecx,esp
                shr eax,16
                and ecx,#0000FFFF
                mov [c_esp_hi],eax
                mov [c_esp_lo],ecx
            [64]
--DEV better yet: use the push rsp twice, or rsp,8 trick... (nah...) {YES, we need to do this!!}
                mov rax,rsp
                mov rcx,rsp
                shr rax,32
                and rcx,#FFFFFFFF
                mov [c_esp_hi],rax
                mov [c_esp_lo],rcx
            []
        }
    {return_type,addr,cstrings} = c_common(rid,args,FUNC)
--  {return_type,addr,cstrings,esp4} = c_common(rid,args,FUNC)

--(DEV: delete once all types are handled)
    if not find(return_type,{
--                               C_CHAR,        -- #01000001
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
--                               ""}) then      -- (dummy entry)
                                 $}) then       -- (dummy entry)
--          printf(1,"c_func: return type %08x not yet coded/tested\n",return_type)
        ?9/0    -- return type not yet coded/tested!
--DEV e15?

    end if

    --
    -- Call the routine and convert result (in eax/ST0) to a Phix ref:
    --
    #ilASM{
            [32]
--              call :%save_ebp
--              mov [local_ebp],eax
                mov edx,ebp
--mov esi,4
                call :%pSetSaveEBP
                shr eax,2
                mov [local_ebp4],eax

                mov eax,[addr]
                cmp eax,h4  --DEV :%pLoadMint
                jl @f
                    -- addr>#3FFFFFFF stored as float:
                    sub esp,8
                    fld qword[ebx+eax*4]
                    fistp qword[esp]
                    pop eax
                    add esp,4
            @@:

                call eax

                mov edx,[c_esp_hi]
                mov ecx,[c_esp_lo]
                shl edx,16
                xor ebx,ebx         -- (Phix likes it zero!)
                add ecx,edx
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
            ::cstore                                        --    cstore:
                    lea edi,[r]
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
                call :%e02atdb0
--DEV e15 instead

            ::intres                                        -- intres:
                mov [r],eax
            ::done                                          -- done:
--              mov edx,[local_ebp]
--              call :%restore_ebp
                mov edx,[local_ebp4]
                shl edx,2
--mov esi,-4
                call :%pSetSaveEBP

            [64]
--              call :%save_rbp
--              mov [local_ebp],rax
                mov rdx,rbp
                call :%pSetSaveEBP
                shr rax,2
                mov [local_ebp4],rax

                mov rax,[addr]
--              cmp rax,h4
                mov r15,h4
                cmp rax,r15
                jl @f
                    -- addr>#3FFFFFFFFFFFFFFF stored as float:
                    sub rsp,8
                    fld tbyte[rbx+rax*4]
                    fistp qword[rsp]
                    pop rax
            @@:
--[PE64]    
                mov rcx,[rsp]
                mov rdx,[rsp+8]
                mov r8,[rsp+16]
                mov r9,[rsp+24]
--[ELF64]?
                call rax

                mov rdx,[c_esp_hi]
                mov rcx,[c_esp_lo]
                shl rdx,32
                xor rbx,rbx         -- (Phix likes it zero!)
                add rcx,rdx
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
                        fild qword[esp]
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
                je :cstore
                cmp rdx,0x03000008  -- (C_DOUBLE)
                jne @f
            ::cstore                                        --    cstore:
                    lea rdi,[r]
                    call :%pStoreFlt                    -- ([rdi]:=ST0)
                    jmp :done
            @@:
                cmp rdx,0x01000002  -- (C_SHORT)
                jne @f
                    cwde                                    -- (ax->eax) [in range -32768..32767]
--                  and rax,0xFFFF
                    cdqe                                    -- (nb does not list right in FDBG)
                    jmp :intres
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
--                  cdqe
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
                mov [r],rax
            ::done                                          -- done:
--DEV beginning to think this is drivel... (as per c_proc)
-- (erm, just badly named??)
--              mov rdx,[local_ebp]
--              call :%restore_rbp
                mov rdx,[local_ebp4]
                shl rdx,2
                call :%pSetSaveEBP

            []
        }
    return r
end function

--    opCFuncZeroParams:
--
--  mov ecx,[ebpidx]
--  mov [ebpsave+ecx],ebp   ; in case of callback
--      call eax
--      mov edi,FltWrk
--      xor ebx,ebx
--      pop esi                 ;[2] dcftbl entry addr
--   ;  [esi] contains a 32-bit function address
--   ;  [esi+4] contains the number of parameters
--   ;  [esi+8] contains the return type
--   ;  [esi+12] contains the calling convention
--
--  ;;DEV::
--  ;   cmp dword[esi+8],0x03000008     ; C_DOUBLE?
--  ;   mov cl,[esi+11]     0x01 signed, 0x02 unsigned, 0x03 float  
--  ;;DEV???
--  ;   cmp [ecode],0
--  ;   je @f
--  ;       pop eax
--  ;       ret
--  ;  @@:
--      mov ecx,[esi+8]
--      cmp cl,4
--      je opCFuncDword
--  ;   cmp cl,8
--      cmp ecx,0x03000008
--      je opCFuncDouble
--      cmp cl,2
--      je opCFuncWord
--      cmp cl,1
--      jne e15ucfrt            ; unknown cfunc return type
--      and eax,0xFF
--    opCFuncWord:  
--      and eax,0x0000FFFF
--    opCFuncDword: 
--      rol ecx,8
--      mov dword[edi],eax
--  ;   cmp byte[esi+11],0x02
--      cmp cl,1
--      jne opCFunsigned
--  ;   initialConstant("C_CHAR",       #01000001)
--  ;   initialConstant("C_UCHAR",      #02000001)
--  ;   initialConstant("C_SHORT",      #01000002)
--  ;   initialConstant("C_USHORT",     #02000002)
--  ;   initialConstant("C_INT",        #01000004)
--  ;   initialConstant("C_UINT",       #02000004)
--  ;   initialConstant("C_FLOAT",      #03000004)
--  ;   initialConstant("C_DOUBLE",     #03000008)  -- result in st0
--  ;   initialConstant("E_INTEGER",    #06000004)
--  ;   initialConstant("E_ATOM",       #07000004)
--  ;   initialConstant("E_SEQUENCE",   #08000004)
--  ;   initialConstant("E_OBJECT",     #09000004)
--
--  ;   jge opCFunsigned
--      fild dword[edi] ;;DEV was qword...
--  ;   mov [FltWrk64],0
--  ;   fild qword[FltWrk]
--  ;  @@:
--    opCFuncDouble:
--      pop edx
--      push freeStrings
--      jmp StoreFlt
--
--    opCFunsigned:
--  ;   jg opCFloat
--  ;   jg @b
--      cmp cl,2    
--      jne opCFuncElse
--      mov dword[edi+4],ebx
--      fild qword[edi]
--      jmp opCFuncDouble
--
--    opCFuncElse:
--      cmp cl,4
--      jne @f
--      ; P_REF handling
--  ;   mov edi,edx
--      mov eax,edx
--      pop edi
--      cmp eax,h4
--      jl opCFuncStore
--      inc dword[ebx+eax*4-8]
--      jmp opCFuncStore
--      
--    @@:
--      cmp cl,8
--      jne e15ucfrtR8          ; unknown c_func return type
--      ; E_SEQUENCE handling
--  ;   shl eax,3
--  ;               NO VALUE: 10111111 11111111 11111111 11111111   (undefined object)
--  ;               ATOM-DBL: 101ppppp pppppppp pppppppp pppppppp   (29-bit pointer)
--  ;               SEQUENCE: 100ppppp pppppppp pppppppp pppppppp   (29-bit pointer)
--  ;struct s1 {                       /* a sequence header block */
--  ;       object_ptr base;               /* pointer to (non-existent) 0th element */
--  ;       long length;                   /* number of elements */
--  ;       long ref;                      /* reference count */
--  ;       long postfill;                 /* number of post-fill objects */
--  ;}; /* total 16 bytes */
--  ;struct d {                        /* a double precision number */
--  ;       double dbl;                    /* double precision value */
--  ;       long ref;                      /* reference count */
--  ;}; /* total 12 bytes */
--    opCFuncEseq:
--  ;   push edx                ; result addr
--      mov edx,[ebx+eax*8+4]   ; length (RDS Eu style)
--      mov esi,[ebx+eax*8]     ; base (RDS Eu style)
--      <mov edx,era>
--      call AllocSeq           ; damages eax only
--      test edx,edx
--      jz opCFuncEseqZL
--      mov ecx,edx
--      lea edi,[ebx+eax*4]
--    opCFuncEseqLoop:
--      mov edx,[esi+4]
--      add esi,4 
--      cmp edx,0x80000000
--      jb opCFuncEseqInt
--      cmp edx,0xA0000000
--      jb opCFuncEseqSubseq
--      ; this is a float then:
--      fld qword[ebx+edx*8]
--      pushad
--      call AllocFlt
--      mov [esp+20],eax    ; (edx:=eax, the newly allocated ref)
--      popad
--      jmp opCFuncEseqInt
--    opCFuncEseqSubseq:
--      pushad
--      mov eax,edx
--      mov edx,edi         ; actually gets stored twice, not a problem,
--      mov [edi],ebx       ; clear, just in case
--      call opCFuncEseq
--      mov [esp+20],eax    ; (edx:=eax, the newly allocated ref)
--      popad
--    opCFuncEseqInt:
--      mov [edi],edx
--      add edi,4
--      dec ecx
--      jnz opCFuncEseqLoop
--   opCFuncEseqZL:
--      pop edi
--   opCFuncStore:
--      mov edx,[edi]
--      mov [edi],eax
--      cmp edx,h4
--      jle @f
--          dec dword[ebx+edx*4-8]
--          jz deallocX
--    @@:
--      ret
--

global procedure c_proc(integer rid, sequence args={})
--procedure fc_proc(integer rid, sequence args={})
integer return_type
integer c_esp_lo = 0    --DEV esp is dword-aligned!
integer c_esp_hi = 0
integer local_ebp4
atom addr
sequence cstrings -- Keeps refcounts>0, of any temps we had to make, over the call.
                  -- Freed automatically, after we get back from the call_eax, /not/
                  -- on exit from, or mid-way through, calling the c_common routine.

    #ilASM{
            [32]
                mov eax,esp
                mov ecx,esp
                shr eax,16
                and ecx,#0000FFFF
                mov [c_esp_hi],eax
                mov [c_esp_lo],ecx
            [64]
                mov rax,rsp
                mov rcx,rsp
                shr rax,32
--              and ecx,#0000FFFF
                and rcx,#FFFFFFFF
                mov [c_esp_hi],rax
                mov [c_esp_lo],rcx
--push rbp
            []
        }
    {return_type,addr,cstrings} = c_common(rid,args,PROC)
--  {return_type,addr,cstrings,esp4} = c_common(rid,args,PROC)
    -- (return_type has already been tested for 0 in c_common)
    #ilASM{
            [32]
--              call :%save_ebp         --DEV this is stupid!!
--              mov [local_ebp],eax
                mov edx,ebp
--mov esi,5
                call :%pSetSaveEBP
                shr eax,2
                mov [local_ebp4],eax

                mov eax,[addr]
                cmp eax,h4  --DEV :%pLoadMint
                jl @f
                    -- addr>#3FFFFFFF get stored as float:
                    sub esp,8
                    fld qword[ebx+eax*4]
                    fistp qword[esp]
                    pop eax
                    add esp,4
            @@:
                call eax
                mov edx,[c_esp_hi]
                mov ecx,[c_esp_lo]
                shl edx,16
                xor ebx,ebx     -- (Phix likes it zero!)
                add ecx,edx
                mov esp,ecx
--              mov edx,[local_ebp]
--              call :%restore_ebp
                mov edx,[local_ebp4]
                shl edx,2
--mov esi,-5
                call :%pSetSaveEBP

            [64]
--              call :%save_rbp
--              mov [local_ebp],rax
                mov rdx,rbp
                call :%pSetSaveEBP
                shr rax,2
                mov [local_ebp4],rax

                mov rax,[addr]
--              cmp rax,h4
                mov r15,h4
                cmp rax,r15
                jl @f
                    -- addr>#3FFFFFFFFFFFFFFF get stored as float:
                    sub rsp,8
                    fld tbyte[rbx+rax*4]
                    fistp qword[rsp]
                    pop rax
            @@:
--push rbp
--[PE64]
                mov rcx,[rsp]
                mov rdx,[rsp+8]
                mov r8,[rsp+16]
                mov r9,[rsp+24]
--[ELF64]
                call rax
--pop rbp
--DEV... we cannot restore rsp or rbp from a corrupt rbp!!! (that's not what it is!)
--!/*
                mov rdx,[c_esp_hi]
                mov rcx,[c_esp_lo]
                shl rdx,32
                xor rbx,rbx     -- (Phix likes it zero!)
                add rcx,rdx
                mov rsp,rcx
--              mov rdx,[local_ebp]
--              call :%restore_rbp
                mov rdx,[local_ebp4]
                shl rdx,2
                call :%pSetSaveEBP

--!*/
--pop al
            []
        }
end procedure


--Searching for: p_ref
-- Files scanned 4751, Directories scanned 365, Lines 2444672
--C:\Program Files\Phix\e0.exw:338 o=c_func(define_c_func({},call_back(routine_id("h0")),{},P_REF),{}) -- -77
--C:\Program Files\Phix\psym.e:1304 --  initialConstant("P_REF",          #04000004)
--C:\Program Files\Phix\psym.e:1310 --   P_REF, when Phix gets a -dll command line option that is..)
--C:\Program Files\Phix\tmp.txt:62 C\Program Files\Phix\e0.exw338 o=c_func(define_c_func({},call_back(routine_id("h0")),{},P_REF),{}) -- -77
--C:\Program Files\Phix\tmp.txt:77 C\Program Files\Phix\test\t42cback.exw55 o=c_func(define_c_func({},call_back(routine_id("h0")),{},P_REF),{}) -- -77
--C:\Program Files\Phix\test\t42cback.exw:11 --  the new P_REF setting
--C:\Program Files\Phix\test\t42cback.exw:59 --o=c_func(define_c_func({},call_back(routine_id("h0")),{},P_REF),{}) -- -77
--C:\Program Files\Phix\builtins\dll.e:39 --     P_REF      = #04000004,    -- NB: Phix uses this instead of above four.
--C:\Program Files\Phix\bench\dll.e:39 --   P_REF    = #04000004,    -- NB: Phix uses this instead of above four.
--C:\Program Files\Phix\asm\p.asm:13358   mov [esp+20],eax    ; preserve (in edx) for P_REF handling
--C:\Program Files\Phix\asm\p.asm:13961   ; P_REF handling
--

function call_common(integer rid, sequence params, integer isProc)
-- common code for call_proc/call_func (validate and process args)
-- isProc is 0 from call_func, 1 from call_proc.
sequence symtab
object si               -- copy of symtab[i], speedwise
integer sNtyp,
        minparams,
        maxparams,
        noofparams,
        nooflocals,     -- (ie params+localvars)
        si_il

--integer tidx
object res

    -- get copy of symtab. NB read only! may contain nuts! (unassigned vars)
--  si = 1  -- callstack not rqd
    #ilASM{
        [32]
            lea edi,[symtab]
            call :%opGetST                      -- [edi]:=symtab (see pStack.e)
        [64]
            lea rdi,[symtab]
            call :%opGetST                      -- [rdi]:=symtab (see pStack.e)
        []
          }
    if rid<T_const1
    or rid>length(symtab) then
        fatalN(3,e72iri,rid)
    end if

    si = symtab[rid]

    sNtyp = si[S_NTyp]
    if sNtyp<S_Type
    or sNtyp>S_Proc
    or (sNtyp=S_Proc)!=isProc then
        fatalN(3,e72iri,rid)
    end if

    minparams = si[S_ParmN]
    maxparams = length(si[S_sig])-1
    noofparams = length(params)
    if noofparams<minparams then fatalN(3,e81ipicfp) end if -- insufficient parameters in call_func/proc()
    if noofparams>maxparams then fatalN(3,e89tmpicfp) end if -- too many parameters in call_func/proc()
    nooflocals = si[S_Ltot]         -- (total no of params + local vars + temps)
    if string(params) then
        params &= -1    -- (force conversion to dword-sequence)
        --  Of course call_proc(N,"fred") is probably an error,
        --  where the programmer actually meant to use {"fred"},
        --  ((assuming enough optional params to get this far))
        --  but call_proc(N,repeat(65,5)) must work the same as
        --  call_proc(N,{65,65,65,65,65}) even though it is in
        --  fact effectively the same as call_proc(N,"AAAAA"),
        --  since repeat(ch,N) creates a string, as it should.
    end if
--DEV 26/02/2012 (we want something similar on si, maybe object sicopy?)
--put back, 21/9/14 (keep ex.err simpler):
--  symtab = {} -- 1/10/14: spannered self-host on newEmit=0, so I made the same un-change here for now [DEV, re-test when newEmit=1 self host works!]
-- added 21/9/14:
    si_il = si[S_il]
    si = 0
    #ilASM{ e_all                                   -- set "all side_effects"
        [32]
            mov ecx,[nooflocals]                    -- (si[S_Ltot])
            mov edx,[rid]
            -- 04/12/2011: load before the opFrame (as that modifies ebp) and push*3
--          mov eax,[si]
            mov eax,[si_il]                         -- (si[S_il])
            mov edi,[noofparams]
            mov esi,[params]
            push eax                                -- [1] (popped into esi)
            push edi                                -- [2] (popped into ecx)
            push esi                                -- [3] 
            call :%opFrame
            pop esi                                 -- [3] (params)
            mov edi,ebp                             -- address of first parameter
            pop ecx                                 -- [2] (noofparams)
            shl esi,2                               -- params(raw)
            test ecx,ecx
            jz :zeroparams

         ::paramloop
                lodsd                               -- mov eax,[esi]; esi+=4
                mov [edi],eax
                sub edi,4
                cmp eax,h4
                jl @f
                    add dword[ebx+eax*4-8],1        -- increment refcount.
              @@:
                dec ecx
                jnz :paramloop

         ::zeroparams

--          pop esi                                 -- [1] ([si, ie symtab[rid]])
--          pop esi                                 -- [1] ([si_il, ie symtab[rid][S_il]])
            mov dword[ebp+16],:retaddr
--          jmp dword[ebx+esi*4+40]                 -- execute first opcode (S_il=11)
--          jmp esi                                 -- execute first opcode
            ret                                     -- [1] (== jmp symtab[rid][S_il])
         ::retaddr
            cmp [isProc],0
            jz :isFunc
                xor eax,eax
         ::isFunc
            mov [res],eax                           -- (assumes [res] is still h4 here)
        [64]
            mov rcx,[nooflocals]
            mov rdx,[rid]
            -- 04/12/2011: load before the opFrame (as that modifies ebp) and push*3
--          mov rax,[si]
            mov rax,[si_il]
            mov rdi,[noofparams]
            mov rsi,[params]
            push rax                                -- [1] (popped into rsi)
            push rdi                                -- [2] (popped into rcx)
            push rsi                                -- [3] 
            call :%opFrame
            pop rsi                                 -- [3] (params)
            mov rdi,rbp                             -- address of first parameter
            pop rcx                                 -- [2] (noofparams)
            shl rsi,2                               -- params(raw)
            test rcx,rcx
            jz :zeroparams
            mov r15,h4

         ::paramloop
                lodsq                               -- mov rax,[rsi]; rsi+=8
                mov [rdi],rax
--              sub rdi,4
                sub rdi,8
--              cmp rax,h4
                cmp rax,r15
                jl @f
                    add qword[rbx+rax*4-16],1       -- increment refcount.
              @@:
                sub ecx,1
                jnz :paramloop

         ::zeroparams

--          pop esi                                 -- [1] ([si, ie symtab[rid]])
--          pop esi                                 -- [1] ([si_il, ie symtab[rid][S_il]])
            mov qword[rbp+32],:retaddr
--          jmp dword[ebx+esi*4+40]                 -- execute first opcode (S_il=11)
--          jmp esi                                 -- execute first opcode
            ret                                     -- [1] (== jmp symtab[rid][S_il])
         ::retaddr
            cmp [isProc],0
            jz :isFunc
                xor rax,rax
         ::isFunc
            mov [res],rax                           -- (assumes [res] is still h4 here)
        []
    }
    return res
end function

global function call_func(integer rid, sequence params)
    return call_common(rid,params,0)
end function

global procedure call_proc(integer rid, sequence params)
    if call_common(rid,params,1)!=0 then ?9/0 end if
end procedure

--DEV tryme: (once pcfuncN.e is in the optable, and remove c_cleanup)
-- The next two routines are used by p.exw when interpreting:
-- In p.exe test.exw, the three tables mentioned below are populated and being used by p.exe,
--  but when it decides to execute the code it has just generated for test.exw, it must save
--  the current state (the previous content could still be used via the optable) so that if/
--  when test.exw extends them we can restore them back to their initial state when control
--  returns to p.exe (specially for the -test case). But the real issue here is that we have
--  called :%pNewGtcbChain (see pHeap.e) and are about to call :%pRestoreGtcbChain: what we
--  must do is treat the previous state like something from the global constant pool, which
--  is achieved (in a subtle way) by relying on reference counts. Note the order carefully:
--
--      call :%opGcft
--      mov [prev3],eax
--      call :%pNewGtcbChain        -- <no longer safe to deallocate prev3>
--      ...
--      mov eax,[prev3]
--      call :%opRcft               -- <can safely deallocate any cloned copies>
--      call :%pRestoreGtcbChain    -- <now it is safe to deallocate prev3>
--
-- (Alternatively we could create the prev3 container after pNewGtcbChain, and then zero/h4
--  prev3 before invoking opRcft, which would deliberately /not/ incref it, but that would
--  be a really tiny gain, at the cost of serious head-scratching should it go wrong.)
--

--function fget_pcfunc_tables()     -- (can only be invoked via the optable)
--  if not tinit then Tinit() end if
--  return {prevcb,previd,table}    -- (length-3 container on the old/current heap, pls)
--end function
--
--procedure frestore_pcfunc_tables(sequence prev3)  -- ("")
--  if tinit then -- (should always be true)
--      for i=length(prev3[1])+1 to length(prevcb) do
--          free(prevcb[i]) -- (recent additions only)
--      end for
--  end if
--  {prevcb,previd,table} = prev3 -- (should free any cloned/extended copies)
--end procedure

global procedure c_cleanup()
    if tinit then
        for i=1 to length(prevcb) do
            free(prevcb[i])
        end for
        Tinit()
    end if
end procedure

#ilASM{ jmp :fin

--/*
--global function open_dll(sequence filename)
--/*
procedure :%opOpenDLL(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opOpenDLL
---------------
        --
        --  This is the "glue" needed to allow open_dll() to be put in the optable.
        --  Sure, I could rewrite it as pure #ilASM, but it was much easier to
        --  write (and test) as a (global) hll routine; plus writing something 
        --  like x=s[i] in assembler gets real tedious real fast, trust me.
        --
        [32]
            -- calling convention
            --  lea edi,[res]       -- result location
            --  mov eax,[filename]  -- (opUnassigned)
            --  call :%opOpenDLL    -- [edi]:=open_dll(eax)
            cmp eax,h4
            jl @f
                add dword[ebx+eax*4-8],1
          @@:
            push edi                            --[1] addr res
            push eax                            --[2] filename
--          mov edx,routine_id(fopen_dll)       -- mov edx,imm32 (sets K_ridt)
            mov edx,routine_id(open_dll)        -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[fopen_dll][S_Ltot])
            call :%opFrame
            mov edx,[esp+8]
            pop dword[ebp]                      --[2] filename
            mov dword[ebp+16],:opendllret       -- return address
            mov dword[ebp+12],edx               -- called from address
            jmp $_il                            -- jmp code:fopen_dll
          ::opendllret                          -- (also used for define_c_func etc)
            pop edi                             --[1] addr res
            mov edx,[edi]
            mov [edi],eax
            cmp edx,h4
            jle @f
                sub dword[ebx+edx*4-8],1
                jz :%pDealloc
        [64]
            -- calling convention
            --  lea rdi,[res]       -- result location
            --  mov rax,[filename]  -- (opUnassigned)
            --  call :%opOpenDLL    -- [rdi]:=open_dll(rax)
            mov r15,h4
            cmp rax,r15
            jl @f
                add qword[rbx+rax*4-16],1
          @@:
            push rdi                            --[1] addr res
            push rax                            --[2] filename
--          mov rdx,routine_id(fopen_dll)       -- mov rdx,imm32 (sets K_ridt)
            mov rdx,routine_id(open_dll)        -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[fopen_dll][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+16]
            pop qword[rbp]                      --[2] filename
            mov qword[rbp+32],:opendllret       -- return address
            mov qword[rbp+24],rdx               -- called from address
            jmp $_il                            -- jmp code:fopen_dll
          ::opendllret                          -- (also used for define_c_func etc)
            pop rdi                             --[1] addr res
            mov rdx,[rdi]
            mov [rdi],rax
            cmp rdx,r15
            jle @f
                sub qword[rbx+rdx*4-16],1
                jz :%pDealloc
        []
          @@:
            ret

--global function define_c_func(object lib, object fname, sequence args, atom return_type)
--global function define_c_proc(object lib, object name, sequence args)
--  return define_c_func(lib, name, args, 0)
--end function
--/*
procedure :%opDcfunc(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opDcfunc
--------------
        [32]
            -- calling convention
            --  lea edi,[res]       -- result location
            --  mov eax,[lib]       -- (opUnassigned)
            --  mov ecx,[fname]     -- (opUnassigned)
            --  mov esi,[args]      -- (opUnassigned)
            --  mov edx,[rtyp]      -- (opUnassigned) [0 for define_c_proc]
            --  call :%opDcfunc     -- [edi]:=define_c_func(eax,ecx,esi,edx)
            cmp eax,h4
            jl @f
                add dword[ebx+eax*4-8],1
          @@:
            cmp ecx,h4
            jl @f
                add dword[ebx+ecx*4-8],1
          @@:
            cmp esi,h4
            jl @f
                add dword[ebx+esi*4-8],1
          @@:
            cmp edx,h4
            jl @f
                add dword[ebx+edx*4-8],1
          @@:
            push edi                            --[1] addr res
            push edx                            --[2] return_type
            push esi                            --[3] args
            push ecx                            --[4] fname
            push eax                            --[5] lib
--          mov edx,routine_id(fdefine_c_func)  -- mov edx,imm32 (sets K_ridt)
            mov edx,routine_id(define_c_func)   -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[fdefine_c_func][S_Ltot])
            call :%opFrame
            mov edx,[esp+20]
            pop dword[ebp]                      --[5] lib
            pop dword[ebp-4]                    --[4] fname
            pop dword[ebp-8]                    --[3] args
            pop dword[ebp-12]                   --[2] return_type
--          mov dword[ebp+16],:dcfret           -- return address
            mov dword[ebp+16],:opendllret       -- return address
            mov dword[ebp+12],edx               -- called from address
            jmp $_il                            -- jmp code:fdefine_c_func
--        ::dcfret  
--          pop edi                             --[1] addr res
--          mov edx,[edi]
--          mov [edi],eax
--          cmp edx,h4
--          jle @f
--              sub dword[ebx+edx*4-8],1
--              jz :%pDealloc
        [64]
            -- calling convention
            --  lea rdi,[res]       -- result location
            --  mov rax,[lib]       -- (opUnassigned)
            --  mov rcx,[fname]     -- (opUnassigned)
            --  mov rsi,[args]      -- (opUnassigned)
            --  mov rdx,[rtyp]      -- (opUnassigned) [0 for define_c_proc]
            --  call :%opDcfunc     -- [rdi]:=define_c_func(rax,rcx,rsi,rdx)
            mov r15,h4
            cmp rax,r15
            jl @f
                add qword[rbx+rax*4-16],1
          @@:
            cmp rcx,r15
            jl @f
                add qword[rbx+rcx*4-16],1
          @@:
            cmp rsi,r15
            jl @f
                add qword[rbx+rsi*4-16],1
          @@:
            cmp rdx,r15
            jl @f
                add qword[rbx+rdx*4-16],1
          @@:
            push rdi                            --[1] addr res
            push rdx                            --[2] return_type
            push rsi                            --[3] args
            push rcx                            --[4] fname
            push rax                            --[5] lib
--          mov rdx,routine_id(fdefine_c_func)  -- mov edx,imm32 (sets K_ridt)
            mov rdx,routine_id(define_c_func)   -- mov edx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov ecx,imm32 (=symtab[fdefine_c_func][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+40]
            pop qword[rbp]                      --[5] lib
            pop qword[rbp-8]                    --[4] fname
            pop qword[rbp-16]                   --[3] args
            pop qword[rbp-24]                   --[2] return_type
--          mov qword[rbp+32],:dcfret           -- return address
            mov qword[rbp+32],:opendllret       -- return address
            mov qword[ebp+24],rdx               -- called from address
            jmp $_il                            -- jmp code:fdefine_c_func
        []
--        @@:
--          ret

--global function define_c_var(atom lib, sequence name)
--DEV untested
--/*
procedure :%opDcvar(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opDcvar
-------------
        [32]
            -- calling convention
            --  lea edi,[res]       -- result location
            --  mov eax,[lib]       -- (opUnassigned)
            --  mov ecx,[name]      -- (opUnassigned)
            --  call :%opDcvar      -- [edi]:=define_c_var(eax,ecx)
            cmp eax,h4
            jl @f
                add dword[ebx+eax*4-8],1
          @@:
            cmp ecx,h4
            jl @f
                add dword[ebx+ecx*4-8],1
          @@:
            push edi                            --[1] addr res
            push ecx                            --[2] name
            push eax                            --[3] lib
--          mov edx,routine_id(fdefine_c_var)   -- mov edx,imm32 (sets K_ridt)
            mov edx,routine_id(define_c_var)    -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[fdefine_c_var][S_Ltot])
            call :%opFrame
            mov edx,[esp+12]
            pop dword[ebp]                      --[3] lib
            pop dword[ebp-4]                    --[2] name
            mov dword[ebp+16],:opendllret       -- return address
            mov dword[ebp+12],edx               -- called from address
            jmp $_il                            -- jmp code:fdefine_c_func
        [64]
            -- calling convention
            --  lea rdi,[res]       -- result location
            --  mov rax,[lib]       -- (opUnassigned)
            --  mov rcx,[name]      -- (opUnassigned)
            --  call :%opDcvar      -- [rdi]:=define_c_var(rax,rcx)
            mov r15,h4
            cmp rax,r15
            jl @f
                add qword[rbx+rax*4-16],1
          @@:
            cmp rcx,r15
            jl @f
                add qword[rbx+rcx*4-16],1
          @@:
            push rdi                            --[1] addr res
            push rcx                            --[2] name
            push rax                            --[3] lib
--          mov rdx,routine_id(fdefine_c_var)   -- mov edx,imm32 (sets K_ridt)
            mov rdx,routine_id(define_c_var)    -- mov edx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov ecx,imm32 (=symtab[fdefine_c_var][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+24]
            pop qword[rbp]                      --[3] lib
            pop qword[rbp-8]                    --[2] name
            mov qword[rbp+32],:opendllret       -- return address
            mov qword[ebp+24],rdx               -- called from address
            jmp $_il                            -- jmp code:fdefine_c_func
        []


--global function call_back(object id)
--/*
procedure :%opCallback(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opCallback
----------------
        [32]
            -- calling convention
            --  lea edi,[res]       -- result location
            --  mov eax,[id]        -- (opUnassigned)
            --  call :%opCallback   -- [edi]:=call_back(eax)
            cmp eax,h4
            jl @f
                add dword[ebx+eax*4-8],1
          @@:
            push edi                            --[1] addr res
            push eax                            --[2] id
--          mov edx,routine_id(fcall_back)      -- mov edx,imm32 (sets K_ridt)
            mov edx,routine_id(call_back)       -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[fcall_back][S_Ltot])
            call :%opFrame
            mov edx,[esp+8]
            pop dword[ebp]                      --[2] id
            mov dword[ebp+16],:opendllret       -- return address
            mov dword[ebp+12],edx               -- called from address
            jmp $_il                            -- jmp code:fcall_back
        [64]
            -- calling convention
            --  lea rdi,[res]       -- result location
            --  mov rax,[id]        -- (opUnassigned)
            --  call :%opCallback   -- [rdi]:=call_back(rax)
            mov r15,h4
            cmp rax,r15
            jl @f
                add qword[rbx+rax*4-16],1
          @@:
            push rdi                            --[1] addr res
            push rax                            --[2] id
--          mov rdx,routine_id(fcall_back)      -- mov edx,imm32 (sets K_ridt)
            mov rdx,routine_id(call_back)       -- mov edx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov ecx,imm32 (=symtab[fcall_back][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+16]
            pop qword[rbp]                      --[2] id
            mov qword[rbp+32],:opendllret       -- return address
            mov qword[ebp+24],rdx               -- called from address
            jmp $_il                            -- jmp code:fcall_back
        []


--global procedure call(atom addr)
--/*
procedure :%opCallA(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opCallA
-------------
        [32]
            -- calling convention
            --  mov eax,[addr]      -- (opUnassigned)
            --  call :%opCallA      -- call(eax)
--          call :%pLoadMint
            cmp eax,h4
            jl @f
                add dword[ebx+eax*4-8],1
          @@:
            push eax                            --[1] addr
--          mov edx,routine_id(fcall)           -- mov edx,imm32 (sets K_ridt)
            mov edx,routine_id(call)            -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[fcall][S_Ltot])
            call :%opFrame
            mov edx,[esp+4]
            pop dword[ebp]                      --[2] addr
            mov dword[ebp+16],:callret          -- return address
            mov dword[ebp+12],edx               -- called from address
            jmp $_il                            -- jmp code:fcall_back
          ::callret
            ret
        [64]
            -- calling convention
            --  mov rax,[addr]      -- (opUnassigned)
            --  call :%opCallA      -- call(rax)
            mov r15,h4
            cmp rax,r15
            jl @f
                add qword[rbx+rax*4-16],1
          @@:
            push rax                            --[1] addr
--          mov rdx,routine_id(fcall)           -- mov edx,imm32 (sets K_ridt)
            mov rdx,routine_id(call)            -- mov edx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov ecx,imm32 (=symtab[fcall][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+8]
            pop qword[rbp]                      --[1] addr
            mov qword[rbp+32],:callret          -- return address
            mov qword[ebp+24],rdx               -- called from address
            jmp $_il                            -- jmp code:fcall_back
          ::callret
            ret
        []

--global function c_func(integer rid, sequence args={})
--/*
procedure :%opCfunc(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opCfunc
-------------
        [32]
            -- calling convention
            --  lea edi,[res]       -- result location
            --  mov eax,[rid]       -- (opUnassigned)
            --  mov esi,[args]      -- (opUnassigned)
            --  call :%opCfunc      -- [edi]:=c_func(eax,esi)
            cmp eax,h4
            jl @f
                add dword[ebx+eax*4-8],1
          @@:
            cmp esi,h4
            jl @f
                add dword[ebx+esi*4-8],1
          @@:
            push edi                            --[1] addr res
            push esi                            --[2] args
            push eax                            --[3] rid
--          mov edx,routine_id(fc_func)         -- mov edx,imm32 (sets K_ridt)
            mov edx,routine_id(c_func)          -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[fc_func][S_Ltot])
            call :%opFrame
            mov edx,[esp+12]
            pop dword[ebp]                      --[3] rid
            pop dword[ebp-4]                    --[2] args
            mov dword[ebp+16],:opendllret       -- return address
            mov dword[ebp+12],edx               -- called from address
            jmp $_il                            -- jmp code:fc_func
        [64]
            -- calling convention
            --  lea rdi,[res]       -- result location
            --  mov rax,[rid]       -- (opUnassigned)
            --  mov rsi,[args]      -- (opUnassigned)
            --  call :%opCfunc      -- [rdi]:=c_func(rax,rsi)
            mov r15,h4
            cmp rax,r15
            jl @f
                add qword[rbx+rax*4-16],1
          @@:
            cmp rsi,r15
            jl @f
                add qword[rbx+rsi*4-16],1
          @@:
            push rdi                            --[1] addr res
            push rsi                            --[2] args
            push rax                            --[3] rid
--          mov rdx,routine_id(fc_func)         -- mov edx,imm32 (sets K_ridt)
            mov rdx,routine_id(c_func)          -- mov edx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov ecx,imm32 (=symtab[fc_func][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+24]
            pop qword[rbp]                      --[3] rid
            pop qword[rbp-8]                    --[2] args
            mov qword[rbp+32],:opendllret       -- return address
            mov qword[ebp+24],rdx               -- called from address
            jmp $_il                            -- jmp code:fc_func
        []

--global procedure c_proc(integer rid, sequence args={})
--/*
procedure :%opCproc(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opCproc
-------------
        [32]
            -- calling convention
            --  mov eax,[rid]       -- (opUnassigned)
            --  mov esi,[args]      -- (opUnassigned)
            --  call :%opCproc      -- c_proc(eax,esi)
            cmp eax,h4
            jl @f
                add dword[ebx+eax*4-8],1
          @@:
            cmp esi,h4
            jl @f
                add dword[ebx+esi*4-8],1
          @@:
            push esi                            --[1] args
            push eax                            --[2] rid
--          mov edx,routine_id(fc_proc)         -- mov edx,imm32 (sets K_ridt)
            mov edx,routine_id(c_proc)          -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[fc_proc][S_Ltot])
            call :%opFrame
            mov edx,[esp+8]
            pop dword[ebp]                      --[2] rid
            pop dword[ebp-4]                    --[1] args
            mov dword[ebp+16],:cprocret         -- return address
            mov dword[ebp+12],edx               -- called from address
            jmp $_il                            -- jmp code:fc_func
        [64]
            -- calling convention
            --  mov rax,[rid]       -- (opUnassigned)
            --  mov rsi,[args]      -- (opUnassigned)
            --  call :%opCproc      -- c_proc(rax,rsi)
            mov r15,h4
            cmp rax,r15
            jl @f
                add qword[rbx+rax*4-16],1
          @@:
            cmp rsi,r15
            jl @f
                add qword[rbx+rsi*4-16],1
          @@:
            push rsi                            --[1] args
            push rax                            --[2] rid
--          mov rdx,routine_id(fc_proc)         -- mov edx,imm32 (sets K_ridt)
            mov rdx,routine_id(c_proc)          -- mov edx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov ecx,imm32 (=symtab[fc_proc][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+16]
            pop qword[rbp]                      --[2] rid
            pop qword[rbp-8]                    --[1] args
            mov qword[rbp+32],:cprocret         -- return address
            mov qword[ebp+24],rdx               -- called from address
            jmp $_il                            -- jmp code:fc_proc
        []
          ::cprocret
            ret

--global function call_func(integer rid, sequence params)
--  return call_common(rid,params,0)
--end function
--/*
procedure :%opCallFunc(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opCallFunc
----------------
        [32]
            -- calling convention
            --  lea edi,[res]       -- result location
            --  mov eax,[rid]       -- (opUnassigned)
            --  mov esi,[args]      -- (opUnassigned)
            --  call :%opCallFunc   -- [edi]:=call_func(eax,esi)
            cmp eax,h4
            jl @f
                add dword[ebx+eax*4-8],1
          @@:
            cmp esi,h4
            jl @f
                add dword[ebx+esi*4-8],1
          @@:
            push edi                            --[1] addr res
            push esi                            --[2] args
            push eax                            --[3] rid
            mov edx,routine_id(call_common)     -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[call_common][S_Ltot])
            call :%opFrame
            mov edx,[esp+12]
            pop dword[ebp]                      --[3] rid
            pop dword[ebp-4]                    --[2] args
            mov dword[ebp+16],:opendllret       -- return address
            mov dword[ebp+12],edx               -- called from address
            jmp $_il                            -- jmp code:call_common
        [64]
            -- calling convention
            --  lea rdi,[res]       -- result location
            --  mov rax,[rid]       -- (opUnassigned)
            --  mov rsi,[args]      -- (opUnassigned)
            --  call :%opCallFunc   -- [rdi]:=call_func(rax,rsi)
            mov r15,h4
            cmp rax,r15
            jl @f
                add qword[rbx+rax*4-16],1
          @@:
            cmp rsi,r15
            jl @f
                add qword[rbx+rsi*4-16],1
          @@:
            push rdi                            --[1] addr res
            push rsi                            --[2] args
            push rax                            --[3] rid
            mov rdx,routine_id(call_common)     -- mov edx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov ecx,imm32 (=symtab[call_common][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+24]
            pop qword[rbp]                      --[3] rid
            pop qword[rbp-8]                    --[2] args
            mov qword[rbp+32],:opendllret       -- return address
            mov qword[ebp+24],rdx               -- called from address
            jmp $_il                            -- jmp code:call_common
        []

--global procedure call_proc(integer rid, sequence params)
--  if call_common(rid,params,1)!=0 then ?9/0 end if
--end procedure
--/*
procedure :%opCallProc(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opCallProc
----------------
        [32]
            -- calling convention
            --  mov eax,[rid]       -- (opUnassigned)
            --  mov esi,[args]      -- (opUnassigned)
            --  call :%opCallProc   -- call_proc(eax,esi)
            cmp eax,h4
            jl @f
                add dword[ebx+eax*4-8],1
          @@:
            cmp esi,h4
            jl @f
                add dword[ebx+esi*4-8],1
          @@:
            push esi                            --[1] args
            push eax                            --[2] rid
            mov edx,routine_id(call_common)     -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[call_common][S_Ltot])
            call :%opFrame
            mov edx,[esp+8]
            pop dword[ebp]                      --[2] rid
            pop dword[ebp-4]                    --[1] args
            mov dword[ebp+16],:callprocret      -- return address
            mov dword[ebp+12],edx               -- called from address
            jmp $_il                            -- jmp code:fc_func
          ::callprocret
            test eax,eax
            jnz :%e02atdb0
            ret
        [64]
            -- calling convention
            --  mov rax,[rid]       -- (opUnassigned)
            --  mov rsi,[args]      -- (opUnassigned)
            --  call :%opCallProc   -- call_proc(rax,rsi)
            mov r15,h4
            cmp rax,r15
            jl @f
                add qword[rbx+rax*4-16],1
          @@:
            cmp rsi,r15
            jl @f
                add qword[rbx+rsi*4-16],1
          @@:
            push rsi                            --[1] args
            push rax                            --[2] rid
            mov rdx,routine_id(call_common)     -- mov edx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov ecx,imm32 (=symtab[call_common][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+16]
            pop qword[rbp]                      --[2] rid
            pop qword[rbp-8]                    --[1] args
            mov qword[rbp+32],:callprocret      -- return address
            mov qword[ebp+24],rdx               -- called from address
            jmp $_il                            -- jmp code:fc_proc
          ::callprocret
            test rax,rax
            jnz :%e02atdb0
            ret
        []

--function fget_pcfunc_tables()
--/*
procedure :%opGpct(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opGpct
------------
        [32]
            -- calling convention
            --  lea edi,[res]       -- result location
            --  call :%opGpct       -- [edi]:=prev3
            push edi                                --[1] addr res
            mov edx,routine_id(fget_pcfunc_tables)  -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                          -- mov ecx,imm32 (=symtab[fget_pcfunc_tables][S_Ltot])
            call :%opFrame
            mov edx,[esp+4]
            mov dword[ebp+16],:opendllret           -- return address
            mov dword[ebp+12],edx                   -- called from address
            jmp $_il                                -- jmp code:fget_pcfunc_tables
        [64]
            -- calling convention
            --  lea rdi,[res]       -- result location
            --  call :%opGpct       -- [rdi]:=prev3
            push rdi                                --[1] addr res
            mov rdx,routine_id(fget_pcfunc_tables)  -- mov edx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                          -- mov ecx,imm32 (=symtab[fget_pcfunc_tables][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+8]
            mov qword[rbp+32],:opendllret           -- return address
            mov qword[ebp+24],rdx                   -- called from address
            jmp $_il                                -- jmp code:fget_pcfunc_tables
        []

--procedure frestore_pcfunc_tables(sequence prev3)
--opRpct
--/*
procedure :%opRpct(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opRpct
------------
        [32]
            -- calling convention
            --  mov eax,[prev3]     -- (opUnassigned)
            --  call :%opRpct       -- call_proc(eax)
            cmp eax,h4
            jl @f
                add dword[ebx+eax*4-8],1
          @@:
            push eax                                    --[1] prev3
            mov edx,routine_id(frestore_pcfunc_tables)  -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                              -- mov ecx,imm32 (=symtab[frestore_pcfunc_tables][S_Ltot])
            call :%opFrame  
            mov edx,[esp+8]
            pop dword[ebp]                              --[1] prev3
            mov dword[ebp+16],:rpctret                  -- return address
            mov dword[ebp+12],edx                       -- called from address
            jmp $_il                                    -- jmp code:frestore_pcfunc_tables
        [64]
            -- calling convention
            --  mov rax,[rid]       -- (opUnassigned)
            --  call :%opRpct       -- call_proc(rax)
            mov r15,h4
            cmp rax,r15
            jl @f
                add qword[rbx+rax*4-16],1
          @@:
            push rax                                    --[1] prev3
            mov rdx,routine_id(frestore_pcfunc_tables)  -- mov edx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                              -- mov ecx,imm32 (=symtab[frestore_pcfunc_tables][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+16]
            pop qword[rbp]                              --[1] prev
            mov qword[rbp+32],:rpctret                  -- return address
            mov qword[ebp+24],rdx                       -- called from address
            jmp $_il                                    -- jmp code:frestore_pcfunc_tables
        []
          ::rpctret
            ret
--*/

        --
        -- These nops are here because we are storing ::Cleanup/4,
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
        mov rdx,routine_id(c_cleanup)       -- mov edx,imm32 (sets K_ridt)
        mov rcx,$_Ltot                      -- mov ecx,imm32 (=symtab[c_cleanup][S_Ltot])
        call :%opFrame
        mov qword[rbp+32],:cleanupret       -- return address
        jmp $_il                            -- jmp code:c_cleanup
      []
      ::cleanupret
        ret
        
    ::fin
--DEV we should be able to (/will probably need to) get rid of this if we implement that get3/restore3 handling...
--14/8/15:
--      mov esi,[ds+8]              -- esi:=raw addr of symtab[1]
        call :%pGetSymPtr
    [32]
        mov edx,[esi+84]            -- edx:=symtab[T_EBP=22]
        test edx,edx
        jz @f
            -- interpreted
            mov eax,:CCleanup
            shl eax,2
            call :%SetCCleanup      -- see pStack.e
--          mov [cNext],eax
      @@:
    [64]
        mov rdx,[rsi+21*8]          -- rdx:=symtab[T_EBP=22]
        test rdx,rdx
        jz @f
            -- interpreted
            mov rax,:CCleanup
            shl rax,2
            call :%SetCCleanup      -- see pStack.e
--          mov [cNext],rax
      @@:
    []
      }

