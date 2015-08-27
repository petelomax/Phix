--
-- pcfunc.e (Phix compatible 0.6.3)
--
--  Phix implementations of define_c_func, define_c_proc, define_c_var,
--                          call_back, c_func, and c_proc.
--
--  WARNING: Fragile code ahead! Mistakes in this code may completely
--           spanner diagnostics and tracing. You have been warned.
--           (If p -test, p edita, p test\terror, and p test\trace
--            all seem fine, then you can breathe easy once again.)
--
--/*
    ?9/0    This file is incompatible with RDS/OpenEu.
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
--   exists below to change this behaviour, if needed.
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
--  effects simply allocate() and peek/free after the call, in the same 
--  way that legacy code would/had to.
--

--/**/without debug
--!/**/with debug

include pprntf.e

include platform.e

constant ASALPHANUM=1,  --(specifically the chars we expect in a dll entry point; 1-9/A-Z/a-z/_)
         ASANY=0        --(ie assume any and all atoms are ok)
                        --(all atoms are and'ed with #FF, in both cases, btw) 

constant e16cbchop      = 16    -- call_backs cannot have optional parameters
constant e72iri         = 72    -- invalid routine_id
constant e73atodmbs     = 73    -- argument to open_dll must be string
constant e74dcfpe       = 74    -- define_c_func/proc parameter error
constant e75cbrpmaba    = 75    -- call back routine parameters must all be atoms
constant e81ipicfp      = 81    -- insufficient parameters in call_func/proc()
constant e84cbpmbropr   = 84    -- call_back parameter must be routine_id or {'+',routine_id}
constant e88atcfpmbaos  = 88    -- arguments to c_func/proc must be atoms or strings
constant e93tmpicfp     = 93    -- too many parameters in call_func/proc()
constant e117rdnrav     = 117   -- routine does not return a value
constant e118rrav       = 118   -- routine returns a value

procedure fatal(integer errcode, integer ep1=0)
    #ilASM{ mov eax,[errcode]
            mov edi,[ep1]
            xor esi,esi     -- ep2 unused
            call %opRTErn } -- fatal error
    ?9/0
end procedure

function toString(sequence name, integer asAlphanum, integer errcode)
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
            fatal(errcode)
        end if
    end for
    return res
end function

function OpenOneDLL(sequence filename)
atom res
    if not string(filename) then
        filename = toString(filename,ASALPHANUM,e73atodmbs)
    end if
    #ilASM{ mov eax,[filename]
            push ebx    --(=0) (for fild qword)
            shl eax,2
            push eax                            -- lpLibFileName
            call "kernel32.dll","LoadLibraryA"
            push eax
            lea edi,[res]
            fild qword[esp]
            add esp,8
--DEV (to go) [temporary compatibility kludge, while pwy.exe is 26/08/2012]
            mov edx,edi
            call %opMovbi }                 -- call StoreFlt ([edi]:=ST0)
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

procedure check(object o)
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
        fatal(e74dcfpe)
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

--DEV not thread safe! (critical section should be fine)
sequence table

integer tinit
        tinit = 0

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
--      code routine, typically memory from allocate() that has had x86 binary 
--      poked into it, eg:
--
--          constant mem1 = allocate(1)
--              poke(mem1,#C3)  -- ret
--          constant ret = define_c_proc({},mem1,{})
--          ...
--              c_proc(ret,{})
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

    convention = STDCALL
    if platform()!=WIN32 then
        convention = CDECL
    end if
    name = fname
    if sequence(fname) then
        -- check for a '+' prefix:
        nlen = length(fname)
        if nlen=0 then fatal(e74dcfpe) end if
        if fname[1]='+' then
            convention = CDECL
            if nlen<=1 then fatal(e74dcfpe) end if
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
                if nlen!=2 then fatal(e74dcfpe) end if
                name = name[2]
                if not atom(name) then fatal(e74dcfpe) end if
            end if
        end if
    end if
--  addr = 0
    if sequence(lib) then
        if length(lib) 
        or not atom(name) then
            fatal(e74dcfpe)
        end if
        addr = name
    else -- atom(lib)
        if not sequence(name) then
            fatal(e74dcfpe)
        elsif not string(name) then
            name = toString(name,ASALPHANUM,e74dcfpe)
        end if
        --
        -- invoke GetProcAddress,lib,name
        --
        --  If lib is stored as an atom (presumably because it is > 31 bits),
        --  convert it to 32bits in eax. Likewise convert the string ref to
        --  a raw address of the string, by shifting it left two bits.
        --
        #ilASM{ lea edi,[addr]
                mov eax,[lib]
                mov edx,[name]
                cmp eax,h4
                jl @f
                    sub esp,8
                    fld qword[ebx+eax*4]
                    fistp qword[esp]
                    pop eax
                    add esp,4
              @@:
                shl edx,2
--              call %opGetProcA }  -- [edi] = GetProcAddress,eax,edx
--!/* --DEV (untried) (one below as well)
                push edx
                push eax
                call "kernel32.dll","GetProcAddress"
                push ebx    --(=0, for fild qword)
                push eax
                lea edi,[addr]
                fild qword[esp]
                add esp,8
--DEV (to go) [temporary compatibility kludge, while pwy.exe is 26/08/2012]
                mov edx,edi
                call %opMovbi }                 -- call StoreFlt ([edi]:=ST0, as int/flt)
--!*/
                --
                -- Aside: opGetProcA is simply:
                --              invoke GetProcAddress,eax,edx
                --              mov [FltWrk],eax
                --              mov [FltWrk64],ebx (==0)
                --              fild qword[FltWrk]
                --              jmp StoreFlt
                --      There is no way to invoke GetProcAddress directly,  [DEV there is now!]
                --      at least not without deciphering RVA entries... ,
                --      nor can we use GetProcAddress to locate the address
                --      of GetProcAddress, if we ain't got it yet...
                --
                --      StoreFlt just stores ST0 in [edi] as int/float.
                --
        if addr=NULL then return -1 end if
    end if

    --
    -- Validate the args and return type
    --
    for i=1 to length(args) do
        check(args[i])
    end for
    if return_type then
        check(return_type)
    end if

--DEV locking...
-- EnterCriticalSection()
    if not tinit then
        table = {}
        tinit = 1
    end if
    table = append(table,{name,addr,args,return_type,convention})
    res = length(table)
-- LeaveCriticalSection()
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
            name = toString(name,ASALPHANUM,e74dcfpe)
        end if
        #ilASM{ lea edi,[addr]
                mov eax,[lib]
                mov edx,[name]
                cmp eax,h4
                jl @f
                    sub esp,8
                    fld qword[ebx+eax*4]
                    fistp qword[esp]
                    pop eax
                    add esp,4
              @@:
                shl edx,2
--              call %opGetProcA }  -- [edi] = GetProcAddress,eax,edx
--DEV (see above for opGetProcA replacement)
                push edx
                push eax
                call "kernel32.dll","GetProcAddress"
                push ebx    --(=0, for fild qword)
                push eax
                lea edi,[addr]
                fild qword[esp]
                add esp,8
--DEV (to go) [temporary compatibility kludge, while pwy.exe is 26/08/2012]
                mov edx,edi
                call %opMovbi }                 -- call StoreFlt ([edi]:=ST0, as int/flt)
-- we may want this?:
--      if addr=0 then return -1 end if
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
--       S_Tidx     = 9,
         S_Type     = 6,
         S_Func     = 7,
         S_Proc     = 8,
         T_atom     = 3,
         T_object   = 15,
         T_const1   = 26,
         DEBUG      = 0


--DEV not thread safe (locking rqd)
sequence previd,    -- table of all previous call_backs created (id)
         prevcb     -- result for each ""
integer pinit
        pinit = 0

procedure Pinit()
        previd = {}
        prevcb = {}
        pinit = 1
end procedure
if not pinit then Pinit() end if

--DEV not thread safe!!
-- save ebp (in two 16-bit chunks to avoid any 31-bit integer issues).      [DOH, ebp is dword-aligned!] [DEV]
-- set by every call()/c_func()/c_proc(), which each have was_ebp_xx,
--  and used in cbhandler (below) to reset before resuming Phix code.
-- (ebp is the current frame/callstack, in case you didn't know.)
--integer ebp_loword = 0
--integer ebp_hiword = 0
--integer ebp_valid = 0

-- ebp_valid (obviously) controls whether lo/hiword have meaning and/or
--  ebp needs to be restored in case external C/asm/? code munged it.
-- set to 1 by call(), c_func(), c_proc();
-- tested/reset to 0 by cbhandler;
-- saved/restored by cbhandler, call(), c_func(), and c_proc().

----DEV use TLS:
integer ebp_save = 0    -- stored /4 to avoid any 31-bit integer issues
--
#ilASM{ jmp :fin
      :%save_ebp
        mov edx,ebp
        mov eax,[ebp_save]
        shr edx,2
      :%restore_ebp
        mov [ebp_save],edx
        ret
      ::fin
}
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
--
-- Get a (32-bit) machine address for calling a routine
--
-- id should be a result from routine_id, or on Windows the same as part
--  of a {'+',id} pair, if the routine uses CDECL calling convention.
--
-- Note that all parameters must be declared as atoms, and will recieve an
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
--  You must always "call" a call_back, rather than "jmp" to one, because
--  the code below expects a return address before the parameters, except
--  perhaps (ie untested) in the zero-parameter case. The "ret imm16" in
--  the template code could also be a cause for concern (if jmp is used).
--
integer k, siNTyp, sigi, noofparams
--integer d1234
sequence symtab, sig
object si
atom r
integer convention

    k = 0
    if not pinit then
        Pinit()
    else
        k = find(id,previd)
    end if
    if k=0 then
        previd = append(previd,id)
        convention = STDCALL
        if platform()!=WIN32 then
            convention = CDECL
        end if
        if sequence(id) then
            if length(id)!=2
            or id[1]!='+'
            or sequence(id[2]) then
                fatal(e84cbpmbropr) -- call_back parameter must be routine_id or {'+',routine_id}
            end if
            convention = CDECL
            id = id[2]
        end if
        si = 1  -- callstack not rqd
        #ilASM{ lea edi,[symtab]
                lea esi,[si]
--              xor ecx,ecx
                call %opGetST   -- [edi] := symtab
        }
        if not integer(id) 
        or id<=T_const1             -- (no real routines that far down there mate)
        or id>length(symtab) then   -- (nor any "" "" after the end of the symtab!)
            fatal(e72iri,id)
        end if
        si = symtab[id]
        if atom(si) then
            fatal(e72iri,id)
        end if
        siNTyp = si[S_NTyp]
        if siNTyp!=S_Func
        and siNTyp!=S_Type then
            fatal(e72iri,id)
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
                    fatal(e75cbrpmaba) -- call back routine parameters must all be atoms
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
            fatal(e16cbchop) -- call_backs cannot have optional parameters
        end if
--SUG: should we check the return type for non-atom as well? (warning)


--DEV Needs DEP handling. Also, possibly smart to allocate in 8180=409*20 byte blocks.
--                              (and do some extra testing with 20, 40, 60 etc sizes)
--DEV also, we could try the floor(adr/4) trick

        r = allocate(20)    -- STDCALL needs 18 bytes, CDECL 16 (round up to 5 dwords):
--      r = allocate(16)    -- STDCALL needs 13 bytes, CDECL 11 (round up to 4 dwords):
                            --  push 0x01020304     ; 68 04030201 [DEV no longer rqd]   -- DEV symtab pointer
                            --  push 0x05060708     ; 68 08070605 (routine number)
                            --  call #030000000     ; E8 03000000
                            --  ret 0x9090          ; C2 9090   (or C3 nop nop for CDECL)
                            --                      (total of 18 (or 16) bytes)

        #ilASM{ mov edi,[r]
                call :tpop              -- (===push :template; jmp tpop)
--      ::template  -- template code, copied to [r] and then patched.
--DEV ebp save no longer rqd. (see save_ebp etc, quite a pain to remove this, though)
                push #01020304      -- (saved ebp)  [erm, saved symtab ptr]
                push #05060708      -- (routine number)
                call :tpop          -- patched to call cbhandler (relative jump)
                ret 1234            -- ret imm16      (patched to C3 (&nop,nop) for CDECL,
                                    --                  and the imm16 patched for STDCALL.)

--          ::cbhandler
            -----------
                pushad
    
                -- [esp] is saved edi
                -- [esp+4] is saved esi
                -- [esp+8] is saved ebp
                -- [esp+12] is saved esp
                -- [esp+16] is saved ebx
                -- [esp+20] is saved edx
                -- [esp+24] is saved ecx
                -- [esp+28] is saved eax    -- (save of symtab[routineno], then was_ebp_valid, and then result)
                -- [esp+32] is return address into stdcb clone (ret nnn instruction)
                -- [esp+36] is routine no   -- (also used to save result addr)
                -- [esp+40] is saved ebp [DEV no longer rqd/used]
                -- [esp+44] another return address [into C code, probably]
                -- [esp+48] params
    
                -- (same for both STDCALL and CDECL, cmiiw)

                mov edx,[esp+36]        -- rtnid
                xor ebx,ebx
                -- restore ebp (from last call()/c_func()/c_proc())
--                  DEV this is not thread safe! (tls rqd?)
--              cmp dword[ebp_valid],0
--              je @f
--                  mov ecx,[ebp_hiword]
--                  mov ebp,[ebp_loword]
--                  shl ecx,16
--                  or ebp,ecx
                mov ecx,[ebp_save]
                shl ecx,2
                jz @f
                    mov ebp,ecx
              @@:
                mov esi,[ebp+24]        -- symtab
                mov esi,[esi+edx*4-4]   -- esi:=symtab[rtnid]
                mov [esp+28],esi        -- save symtab[rtnid]
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
                call %opFrame           -- call makeFrame
                --
                -- Set params, converting any big 32-bit values to floats
                --
                mov edi,ebp             -- (addr first param)
                pop ecx                 -- [1] pop ecx (no of params)
                test ecx,ecx
                jz :zeroparams
                    lea esi,[esp+48]    -- params (on stack)
                ::paramloop
                    lodsd               --  eax:=[esi], esi+=4
                    cmp eax,h4
                    jb :paramstosd      -- (0..#3FFFFFFF)
--DEV this is viable, treating #FFFFFFFF as -1, but disagrees with RDS Eu...
--                  cmp eax,#C0000000
--                  jae :paramstosd     -- (#C0000000..#FFFFFFFF)
--DEV this loads it unsigned (agreeing with RDS Eu)....
                    push ebx            -- (=0)
                    push eax
                    fild qword[esp]
                    add esp,8
--DEV ... whereas this loads it signed (disagreeing with RDS Eu).
--                  fild dword[esi-4]
    
--                  push edx
--DEV (to go) [temporary compatibility kludge, while pwy.exe is 26/08/2012]
                    mov edx,edi
                    call %opMovbi       --  call StoreFlt ([edi]:=ST0)
--                  pop edx
                    jmp :nextparam
                ::paramstosd
                    mov [edi],eax
                ::nextparam
                    sub edi,4
                    sub ecx,1
                    jnz :paramloop
            ::zeroparams
--              mov eax,[ebp_valid]
                mov eax,[ebp_save]
                mov esi,[esp+28]        -- restore symtab[rtnid]
--              mov [ebp_valid],ebx     -- (0)  ; important!
                mov [ebp_save],ebx      -- (0)  ; important!
                mov [esp+28],eax        -- was_ebp_save
                mov dword[ebp+16],:retaddr
                jmp dword[ebx+esi*4+40] -- execute first opcode (S_il=11)
            ::retaddr
                -- result is in eax, but >31bit stored as a float
                cmp eax,h4
                jl :retint
                    sub esp,8
                    cmp byte[ebx+eax*4-1],0x12          -- (ebx==0)
                    je @f
                    call %opDivf2       --    call e02atdb0  (DEV better: call back returned non-atom?)
                  @@:
                    fld qword[ebx+eax*4]
                    fistp qword[esp]
                    dec dword[ebx+eax*4-8]
                    jnz @f
                        mov edx,eax
                        call %opMovsi   --    call dealloc
                  @@:
                    pop eax
                    add esp,4
            ::retint
--              mov edx,[esp+28]        -- was_ebp_valid
                mov edx,[esp+28]        -- was_ebp_save
                mov [esp+28],eax        -- keep eax, but
--              mov [ebp_valid],edx     -- restore (important!)
                mov [ebp_save],edx      -- restore (important!)
                popad                   -- restore all other registers
                ret 8                   -- (the two dwords pushed by template code)
    
            -- end of cbhandler
    
            ::tpop
                pop esi                 -- (start of template code)
                cmp edi,h4              -- (allocated memory)
                jl @f
                    -- allocated addrs > #3FFFFFFF get stored as 64-bit float:
                    sub esp,8
                    fld qword[ebx+edi*4]
                    fistp qword[esp]
                    pop edi
                    add esp,4
            @@:
                lea edx,[edi+1]         -- (addr of that #01020304)
--              lea edx,[edi+11]        -- (addr of offset)
                mov ecx,5               -- (5 dwords=20 bytes)
                rep movsd
--              mov ecx,[edx]           -- (to check #01020304)
--DEV should no longer be necessary...
                mov [edx],ebp
--              mov [d1234],ecx
--              mov eax,[edx+10]        -- (original jump offset)
                mov eax,3               -- (original jump offset)
                sub esi,edi
                add eax,esi
                mov [edx+10],eax        -- (updated jump offset)
--              mov [edx],eax           -- (updated jump offset)
        }
        if DEBUG then
--          if d1234!=#01020304 then ?9/0 end if
            if peek4s(r+6)!=#05060708 then ?9/0 end if
            if peek(r+15)!=#C2 then ?9/0 end if
        end if
        poke4(r+6,id)
        if convention=STDCALL then      -- fixup that ret imm16
            k = noofparams*4
            poke(r+16,and_bits(k,#FF))
            poke(r+17,floor(k/#100))
--DEV (tryme)
--          poke2(r+16,and_bits(k,#FFFF))
--or just:  poke2(r+16,k)
        else -- convention=CDECL
            poke(r+15,{#C3,#90,#90})        -- ret imm16 ==> plain ret (and nop,nop)
        end if

--      previd = append(previd,id)  -- done above
        prevcb = append(prevcb,r)
    else
        r = prevcb[k]
    end if
    return r
end function

global procedure call(atom addr)
--DEV would this benefit from c_esp[hi/lo] handling as per c_func/c_proc?
--integer was_ebp_lo = ebp_loword
--integer was_ebp_hi = ebp_hiword
--integer was_ebp_valid = ebp_valid
integer local_ebp
        #ilASM{ 
                e_all                                       -- set "all side-effects"
                -- first, save ebp in case of a callback:
--              mov eax,ebp
--              mov ecx,ebp
--              shr eax,16
--              and ecx,#0000FFFF
--              mov [ebp_hiword],eax
--              mov [ebp_loword],ecx
--              mov [ebp_valid],1
                call :%save_ebp
                mov [local_ebp],eax
                call %opClrDbg                              -- clear debug screen if needed
                mov eax,[addr]
                xor ebx,ebx
                cmp eax,h4
                jl @f
                    sub esp,8
                    fld qword[ebx+eax*4]
                    fistp qword[esp]
                    pop eax
                    add esp,4
            @@:
                call eax
                -- restore ebp
--              mov eax,[ebp_hiword]
--              mov ecx,[ebp_loword]
--              shl eax,16
--              xor ebx,ebx
--              add eax,ecx
--              mov ebp,eax
                mov edx,[local_ebp]
                call :%restore_ebp
        }
--      ebp_loword = was_ebp_lo
--      ebp_hiword = was_ebp_hi
--      ebp_valid = was_ebp_valid
end procedure


constant FUNC=1, PROC=0

function c_common(integer rid, sequence args, integer flag)
-- common code for c_func and c_proc (validate and process args)
--  flag is FUNC or PROC accordingly.
sequence argdefs
integer argdefi
integer convention
integer la, lad, ch
object argi
string argstring
integer return_type
object name -- for debugging only
sequence tr -- table[rid], ie {name,addr,args,return_type,convention}
atom addr
sequence cstrings -- keeps refcounst>0, of any temps we have to make

        if tinit=0 or rid<1 or rid>length(table) then
            fatal(e72iri,rid)
        end if
        tr = table[rid]
        name = tr[T_name]
        addr = tr[T_address]
        argdefs = tr[T_args]
        return_type = tr[T_return_type]
        convention = tr[T_convention]
        la = length(args)
        lad = length(argdefs)
        if la!=lad then
            -- e116rrnp4: routine [%s] requires %d parameters, not %d
            #ilASM{ mov al,116
                    mov edi,[lad]   -- ep1
                    mov esi,[la]    -- ep2
--                  mov edx,[name]  -- ep3  --DEV (we haven't got an ep3, yet)
                    call %opRTErn   -- fatal error
            }
        end if
        if flag=FUNC then
            if return_type=0 then fatal(e117rdnrav) end if
        else -- flag=PROC
            if return_type!=0 then fatal(e118rrav) end if
        end if
        #ilASM{ e_all               -- set "all side-effects"
                call %opClrDbg      -- clear debug screen if needed
        }
        cstrings = {}
        if string(args) then
            -- hmmm, well just push the string chars then...
            for i=la to 1 by -1 do
                ch = args[i]
                argdefi = argdefs[i]
                if DEBUG then
                    if not find(argdefi,{
                      -- (These should be fine, but not much point adding them when
                      --    we've nothing to say whether they work or not...)
--                                       #01000001,     -- C_CHAR
--                                       #01000004,     -- C_INT
--                                       #02000004      -- C_UINT == C_ULONG, C_POINTER, C_PTR
                                        }) then ?9/0 end if
                end if
                #ilASM{ mov edx,[ch]
                        push edx
                }
            end for
        else
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
                    #ilASM{ mov edx,[argi]
                            push edx
                    }
                elsif atom(argi) then
                    if find(argdefi,{
                                     #01000004,     -- C_INT
                                     #02000004      -- C_UINT == C_ULONG, C_POINTER, C_PTR
                                    }) then
                        #ilASM{ mov edx,[argi]
                                sub esp,8
                                fld qword[ebx+edx*4]
                                fistp qword[esp]
                                pop edx
                                add esp,4
                                push edx
                        }
                    elsif find(argdefi,{
                                        #03000004       -- C_FLOAT
                                        }) then
                        #ilASM{ mov edx,[argi]
                                sub esp,4
                                fld qword[ebx+edx*4]
                                fstp dword[esp]
                        }
                    elsif find(argdefi,{
                                        #03000008       -- C_DOUBLE
                                        }) then
                        #ilASM{ mov edx,[argi]
                                sub esp,8
                                fld qword[ebx+edx*4]
                                fstp qword[esp]
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
                    #ilASM{ mov edx,[argi]
                            shl edx,2       -- (convert to raw address)
                            push edx
                    }
                else
                    if DEBUG then
                        if not find(argdefi,{
                                             #02000004      -- C_UINT == C_ULONG, C_POINTER, C_PTR
--                                           #12000004      -- C_WIDEPTR [DEV]
                                            }) then ?9/0 end if
                    end if
                    -- convert to a string and push that (cstrings ensures these temporaries are
                    --  /not/ freed, and the memory reused, until /after/ c_func|c_proc return)
                    argstring = toString(argi,ASANY,e88atcfpmbaos)
                    #ilASM{ mov edx,[argstring]
                            shl edx,2   -- (convert to raw address)
                            push edx
                    }
                    cstrings = append(cstrings,argstring)   -- (keep refcounts>0, for a while)
                end if
            end for
        end if
        return {return_type,addr,cstrings}
end function


global function c_func(integer rid, sequence args={})
integer return_type
object r
integer c_esp_lo = 0    -- save/restore esp (just in case)  [dev it is dword-aligned!]
integer c_esp_hi = 0
--integer was_ebp_lo = ebp_loword
--integer was_ebp_hi = ebp_hiword
--integer was_ebp_valid = ebp_valid
integer local_ebp
atom addr
sequence cstrings -- Keeps refcounts>0, of any temps we had to make, over the call.
                  -- Freed automatically, after we get back from the call_eax, /not/
                  -- on exit from, or mid-way through, calling the c_common routine.
                
        #ilASM{ mov eax,esp
                mov ecx,esp
                shr eax,16
                and ecx,#0000FFFF
                mov [c_esp_hi],eax
                mov [c_esp_lo],ecx
        }
        {return_type,addr,cstrings} = c_common(rid,args,FUNC)

--(DEV: delete once all types are handled)
if DEBUG then -- speed!
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
            printf(1,"c_func: return type %08x not yet coded/tested\n",return_type)
            ?9/0    -- return type not yet coded/tested!
--DEV e15?

        end if
end if

        --
        -- Call the routine and convert result (in eax/ST0) to a Phix ref:
        --
        #ilASM{ 
--              mov eax,ebp
--              mov ecx,ebp
--              shr eax,16
--              and ecx,#0000FFFF
--              mov [ebp_hiword],eax
--              mov [ebp_loword],ecx
--              mov [ebp_valid],1
                call :%save_ebp
                mov [local_ebp],eax
                mov eax,[addr]
                cmp eax,h4
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
--DEV (to go) [temporary compatibility kludge, while pwy.exe is 26/08/2012]
                    lea edx,[r]
                    lea edi,[r]
                    call %opMovbi       -- call StoreFlt
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
                call %opDivf2   -- call e02atdb0
--DEV e15 instead
    
            ::intres                                        -- intres:
                mov [r],eax
            ::done                                          -- done:
--              mov eax,[ebp_hiword]
--              mov ecx,[ebp_loword]
--              shl eax,16
--              xor ebx,ebx
--              add eax,ecx
--              mov ebp,eax
                mov edx,[local_ebp]
                call :%restore_ebp
        }
--      ebp_loword = was_ebp_lo
--      ebp_hiword = was_ebp_hi
--      ebp_valid = was_ebp_valid
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
integer return_type
integer c_esp_lo = 0    --DEV esp is dword-aligned!
integer c_esp_hi = 0
--integer was_ebp_lo = ebp_loword
--integer was_ebp_hi = ebp_hiword
--integer was_ebp_valid = ebp_valid
integer local_ebp
atom addr
sequence cstrings -- Keeps refcounts>0, of any temps we had to make, over the call.
                  -- Freed automatically, after we get back from the call_eax, /not/
                  -- on exit from, or mid-way through, calling the c_common routine.

        #ilASM{ mov eax,esp
                mov ecx,esp
                shr eax,16
                and ecx,#0000FFFF
                mov [c_esp_hi],eax
                mov [c_esp_lo],ecx
        }
        {return_type,addr,cstrings} = c_common(rid,args,PROC)
        -- (return_type has already been tested for 0 in c_common)
        #ilASM{ 
--              mov eax,ebp
--              mov ecx,ebp
--              shr eax,16
--              and ecx,#0000FFFF
--              mov [ebp_hiword],eax
--              mov [ebp_loword],ecx
--              mov [ebp_valid],1
                call :%save_ebp
                mov [local_ebp],eax
                mov eax,[addr]
                cmp eax,h4
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
--              mov eax,[ebp_hiword]
--              mov ecx,[ebp_loword]
--              shl eax,16
--              xor ebx,ebx
--              add eax,ecx
--              mov ebp,eax
                mov edx,[local_ebp]
                call :%restore_ebp
        }
--      ebp_loword = was_ebp_lo
--      ebp_hiword = was_ebp_hi
--      ebp_valid = was_ebp_valid
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
integer minparams,
        maxparams,
        noofparams,
        nooflocals,     -- (ie params+localvars)
        sNtyp

--integer tidx
object res

        -- get copy of symtab. NB read only! may contain nuts! (unassigned vars)
        si = 1  -- callstack not rqd
        #ilASM{ lea edi,[symtab]                    -- p1   symtab addr
                lea esi,[si]                        -- p2   flag/crashmsg addr
--              xor_ecx_ecx                         --      (unused)
                call %opGetST                       -- [edi]:=symtab
        }
        if rid<T_const1
        or rid>length(symtab) then
            fatal(e72iri,rid)
        end if

        si = symtab[rid]

        sNtyp = si[S_NTyp]
        if sNtyp<S_Type
        or sNtyp>S_Proc
        or (sNtyp=S_Proc)!=isProc then
            fatal(e72iri,rid)
        end if

        minparams = si[S_ParmN]
        maxparams = length(si[S_sig])-1
        noofparams = length(params)
        if noofparams<minparams then fatal(e81ipicfp) end if -- insufficient parameters in call_func/proc()
        if noofparams>maxparams then fatal(e93tmpicfp) end if -- too many parameters in call_func/proc()
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
--symtab = {} -- 1/10/14: spanners self-host! [DEV]
        #ilASM{ e_all                                   -- set "all side_effects"
                mov ecx,[nooflocals]
                mov edx,[rid]
                -- 04/12/2011: load before the opFrame (as that modifies ebp) and push*3
                mov eax,[si]
                mov edi,[noofparams]
                mov esi,[params]
                push eax                                -- [1] (popped into esi)
                push edi                                -- [2] (popped into ecx)
                push esi                                -- [3] 
                call %opFrame                           -- call makeFrame
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
    
                pop esi                                 -- [1] ([si, ie symtab[rid]])
                mov dword[ebp+16],:retaddr
                jmp dword[ebx+esi*4+40]                 -- execute first opcode (S_il=11)
             ::retaddr
                cmp [isProc],0
                jz :isFunc
                    xor eax,eax
             ::isFunc
                mov [res],eax                           -- (assumes [res] is still h4 here)
        }
        return res
end function

global function call_func(integer rid, sequence params)
    return call_common(rid,params,0)
end function

global procedure call_proc(integer rid, sequence params)
    if call_common(rid,params,1)!=0 then ?9/0 end if
end procedure

