--
-- pcallfunc.e (Phix compatible 0.6.3)
--
--  Phix implementation of dynamic calls: call_func and call_proc.
--
--DEV??
--  WARNING: Fragile code ahead! Mistakes in this code may completely
--           spanner diagnostics and tracing. You have been warned.
--           (If p -test, p edita, p test\terror, and p test\trace
--            all seem fine, then you can breathe easy once again.)
--
--/*
    ?9/0    This file is wholly incompatible with RDS/OpenEuphoria.
--*/

--!/**/without debug
--!/**/with debug

--include builtins\VM\pHeap.e   -- :%pStoreFlt etc
include builtins\VM\pStack.e    -- :%opGetST, :%opFrame etc
--include builtins\VM\pUnassigned.e -- :%pRTErn (DEV/temp)

--constant e16cbchop        = 16    -- call_backs cannot have optional parameters
constant e71cfppe   = 71    -- call_func/proc parameter error
constant e72iri     = 72    -- invalid routine_id
--constant e73atodmbs   = 73    -- argument to open_dll must be string
--constant e74dcfpe     = 74    -- define_c_func/proc parameter error
--constant e75cbrpmaba  = 75    -- call back routine parameters must all be atoms
constant e81ipicfp  = 81    -- insufficient parameters in call_func/proc()
--constant e84cbpmbropr = 84    -- call_back parameter must be routine_id or {'+',routine_id}
--constant e88atcfpmbaos    = 88    -- arguments to c_func/proc must be atoms or strings
constant e89tmpicfp = 89    -- too many parameters in call_func/proc()
--constant e116rrnp     = 116   -- routine requires %d parameters, not %d
constant e117rdnrav = 117   -- routine does not return a value
constant e118rrav   = 118   -- routine returns a value

--DEV??
procedure fatalN(integer level, integer errcode, integer ep1=0, integer ep2=0)
-- level is the number of frames to pop to obtain an era (must be >=2).
-- we report errors on (eg) the c_func call, not in c_func below, so
-- obviously c_func itself calls fatalN(2..), whereas if c_func calls
-- toString, that must then call fatalN(3..), and when open_dll calls
-- OpenOneDLL, which calls toString which finally calls this, it must 
-- do so with call fatalN(4..). There are no fatalN(1..) calls since
-- this is local and that would report errors in pcallfunc.e itself,
-- which is the very thing the level parameter is supposed to avoid!
--printf(1,"fatalN(%d,%d)\n",{level,errcode})
    #ilASM{
        -- calling convention
        --  lea ecx,[level]     -- no of frames to pop to obtain an era (>=2)
        --  mov eax,[errcode]   -- (opUnassigned)
        --  mov edi,[ep1]       -- (opUnassigned) [optional]
        --  mov esi,[ep2]       -- (opUnassigned) [optional]
        --  jmp :%opFatalN      -- fatalN(level,errcode,ep1,ep2)
        [32]
            mov ecx,[level]
            mov eax,[errcode]
            mov edi,[ep1]
            mov esi,[ep2]
          @@:
            mov edx,[ebp+12]    -- called from address
            mov ebp,[ebp+20]    -- (nb no local vars after this!)
            sub ecx,1
            jg @b
            xor esi,esi         -- ep2 unused
            sub edx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
--          jmp :!fatalN        -- fatal error (see pdiagN.e)
            int3
        [64]
            mov rcx,[level]
            mov rax,[errcode]
            mov rdi,[ep1]
            mov rsi,[ep1]
          @@:
            mov rdx,[rbp+24]    -- called from address
            mov rbp,[rbp+40]    -- (nb no local vars after this!)
            sub rcx,1
            jg @b
            xor rsi,rsi         -- ep2 unused
            sub rdx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
--          jmp :!fatalN        -- fatal error (see pdiagN.e)
            int3
        []
          }
    ?9/0
end procedure

-- keep these matching pglobals.e!
constant S_NTyp     = 2,
         S_sig      = 7,    -- routine signature, eg {'F',T_integer} (nb S_sig must be = S_vtype)
--       S_Parm1    = 8,    -- first parameter. (idx to symtab, then follow S_Slink)
         S_ParmN    = 9,    -- min no of parameters. (max is length(S_sig)-1))
         S_Ltot     = 10,   -- total no of parameters + locals (for stack frame allocation)
         S_il       = 11,   -- intermediate code (also backpatch list)
--       S_Tidx     = 9,
         S_Type     = 6,
--       S_Func     = 7,
         S_Proc     = 8,
--       T_atom     = 3,
--       T_object   = 15,
         T_const1   = 26
--       DEBUG      = 0


--constant FUNC = 1, PROC = 0

--1/6/17:
--function call_common(integer rid, sequence params, integer isProc)
function call_common(object rid, object params, integer isProc)
-- common code for call_proc/call_func (validate and process args)
-- isProc is 0 from call_func, 1 from call_proc.
--sequence symtab
object symtab
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
enter_cs()
    #ilASM{
        [32]
            lea edi,[symtab]
            call :%opGetST                      -- [edi]:=symtab (see pStack.e)
        [64]
            lea rdi,[symtab]
            call :%opGetST                      -- [rdi]:=symtab (see pStack.e)
        []
          }
--1/6/17:
--  if rid<T_const1
    if not integer(rid)
    or rid<T_const1
    or rid>length(symtab) then
--      fatalN(3,e72iri,rid)
        si = 0
    else
        si = symtab[rid]
    end if
leave_cs()
    if si=0 then
        fatalN(2,e72iri,rid)
    end if

--1/6/17:
    if not sequence(params) then
--(1/6/17) also applied elsewhere [now that we've (eg) killed off call_func() and invoke call_common() directly from opCallFunc]
--      fatalN(3,e71cfppe,rid)
        fatalN(2,e71cfppe,rid)
    end if

    sNtyp = si[S_NTyp]
    if sNtyp<S_Type
    or sNtyp>S_Proc then
--      fatalN(3,e72iri,rid)
        fatalN(2,e72iri,rid)
    elsif (sNtyp=S_Proc)!=isProc then
        if sNtyp=S_Proc then
--          fatalN(3,e117rdnrav,rid)
            fatalN(2,e117rdnrav,rid)
        else
--          fatalN(3,e118rrav,rid)
            fatalN(2,e118rrav,rid)
        end if
    end if

    minparams = si[S_ParmN]
    noofparams = length(params)
    nooflocals = si[S_Ltot]         -- (total no of params + local vars + temps)
--DEV 26/02/2012 (we want something similar on si, maybe object sicopy?)
--put back, 21/9/14 (keep ex.err simpler):
--  symtab = {} -- 1/10/14: spannered self-host on newEmit=0, so I made the same un-change here for now [DEV, re-test when newEmit=1 self host works!]
-- added 21/9/14:
    si_il = si[S_il]

    enter_cs()
    maxparams = length(si[S_sig])-1
    si = 0
    symtab = 0
    leave_cs()

--  if noofparams<minparams then fatalN(3,e81ipicfp) end if -- insufficient parameters in call_func/proc()
    if noofparams<minparams then fatalN(2,e81ipicfp) end if -- insufficient parameters in call_func/proc()
--  if noofparams>maxparams then fatalN(3,e89tmpicfp) end if -- too many parameters in call_func/proc()
    if noofparams>maxparams then fatalN(2,e89tmpicfp) end if -- too many parameters in call_func/proc()
--  if noofparams>maxparams then fatalN(1,e89tmpicfp) end if -- too many parameters in call_func/proc()

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
push dword[ebp+12]
            call :%opFrame
pop dword[ebp+12]
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
                sub ecx,1
                jnz :paramloop

         ::zeroparams

--          pop esi                                 -- [1] ([si, ie symtab[rid]])
--          pop esi                                 -- [1] ([si_il, ie symtab[rid][S_il]])
--EXCEPT
--X         mov dword[ebp+16],:retaddr
            mov dword[ebp+28],:cc_retaddr
--          mov dword[ebp+28],:!cc_ret
--          jmp dword[ebx+esi*4+40]                 -- execute first opcode (S_il=11)
--          jmp esi                                 -- execute first opcode
            ret                                     -- [1] (== jmp symtab[rid][S_il])
         ::cc_retaddr
--       :!cc_ret
            cmp [isProc],0
            jz :isFunc
                xor eax,eax
         ::isFunc
            mov [res],eax                           -- (assumes [res] is still h4 here)
--DEV (temp, verify it is a valid ebp, or 0:)
--mov eax,[ebp+24]              -- ebp_root
--cmp dword[eax+16],#40565342   -- vsb_magic="@VSB"?
--je @f
--  int3
--@@:

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
                sub rcx,1
                jnz :paramloop

         ::zeroparams

--          pop esi                                 -- [1] ([si, ie symtab[rid]])
--          pop esi                                 -- [1] ([si_il, ie symtab[rid][S_il]])
--EXCEPT
--X         mov qword[rbp+32],:retaddr
            mov qword[rbp+56],:cc_retaddr
--          mov qword[rbp+56],:!cc_ret
--          jmp dword[ebx+esi*4+40]                 -- execute first opcode (S_il=11)
--          jmp esi                                 -- execute first opcode
            ret                                     -- [1] (== jmp symtab[rid][S_il])
         ::cc_retaddr
--       :!cc_ret
            cmp [isProc],0
            jz :isFunc
                xor rax,rax
         ::isFunc
            mov [res],rax                           -- (assumes [res] is still h4 here)
        []
    }
    return res
end function

--DEV to go, see below:
--global function call_func(integer rid, sequence params)
--  return call_common(rid,params,0)
--end function
--
--global procedure call_proc(integer rid, sequence params)
--  if call_common(rid,params,1)!=0 then ?9/0 end if
--end procedure
--
--global function call_funcN(integer rid, sequence params)
--  return call_common(rid,params,0)
--end function
--
--global procedure call_procN(integer rid, sequence params)
--  if call_common(rid,params,1)!=0 then ?9/0 end if
--end procedure

#ilASM{ jmp :!opCallOnceYeNot
--#ilASM{ jmp :fin

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
--29/10/17...
--mov edx,[esp]
--mov dword[ebp+12],edx
            push edi                            --[1] addr res
            push esi                            --[2] args
            push eax                            --[3] rid
            mov edx,routine_id(call_common)     -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[call_common][S_Ltot])
            call :%opFrame
            mov edx,[esp+12]
            pop dword[ebp]                      --[3] rid
            pop dword[ebp-4]                    --[2] args
            mov dword[ebp-8],ebx                -- isProc:=0
--EXCEPT
--X         mov dword[ebp+16],:callfuncret      -- return address
--          mov dword[ebp+28],:callfuncret      -- return address
            mov dword[ebp+28],:!cf_ret          -- return address
            mov dword[ebp+12],edx               -- called from address
            jmp $_il                            -- jmp code:call_common
--        ::callfuncret
          :!cf_ret
            pop edi                             --[1] addr res
            mov edx,[edi]
            mov [edi],eax
            cmp edx,h4
            jle @f
                sub dword[ebx+edx*4-8],1
                jz :%pDealloc
          @@:
            ret
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
            mov rdx,routine_id(call_common)     -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[call_common][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+24]
            pop qword[rbp]                      --[3] rid
            pop qword[rbp-8]                    --[2] args
            mov qword[rbp-16],rbx               -- isProc:=0
--EXCEPT
--X         mov qword[rbp+32],:callfuncret      -- return address
--          mov qword[rbp+56],:callfuncret      -- return address
            mov qword[rbp+56],:!cf_ret          -- return address
            mov qword[rbp+24],rdx               -- called from address
            jmp $_il                            -- jmp code:call_common
--        ::callfuncret
          :!cf_ret
            pop rdi                             --[1] addr res
            mov rdx,[rdi]
            mov [rdi],rax
            cmp rdx,r15
            jle @f
                sub qword[rbx+rdx*4-16],1
                jz :%pDealloc
          @@:
            ret
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
            -- If, instead of call, this is invoked via push 0; jmp :%opCallProc,
            --  the impossible return is discarded and control passes to opRetf.
            --  This allows pTask.e to keep the system stack balanced.
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
            mov edx,[esp+8]                     -- return address
            pop dword[ebp]                      --[2] rid
            pop dword[ebp-4]                    --[1] args
--          mov eax,:callprocret                -- return address
            pop eax
            test edx,edx
            jnz @f
                mov edx,[ebp+20]                -- prev_ebp
--              add esp,4
                mov eax,:%opRetf
                mov edx,[edx+12]                -- called from address
          @@:
            mov dword[ebp-8],1                  -- isProc:=1
--EXCEPT
--X         mov dword[ebp+16],eax               -- return address
            mov dword[ebp+28],eax               -- return address
            mov dword[ebp+12],edx               -- called from address
            jmp $_il                            -- jmp code:call_common
--        ::callprocret
--          test eax,eax
--          jnz :%e02atdb0
--          ret
        [64]
            -- calling convention
            --  mov rax,[rid]       -- (opUnassigned)
            --  mov rsi,[args]      -- (opUnassigned)
            --  call :%opCallProc   -- call_proc(rax,rsi)
            -- If instead of call this is invoked via push 0; jmp :%opCallProc,
            --  the impossible return is discarded and control passes to opRetf.
            --  This allows pTask.e to keep the system stack balanced.
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
            mov rdx,routine_id(call_common)     -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[call_common][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+16]                    -- return address
            pop qword[rbp]                      --[2] rid
            pop qword[rbp-8]                    --[1] args
            mov rax,:callprocret                -- return address
            test rdx,rdx
            jnz @f
                mov rdx,[rbp+40]                -- prev_ebp
                add rsp,8
                mov rax,:%opRetf
                mov rdx,[rdx+24]                -- called from address
          @@:
            mov qword[rbp-16],1                 -- isProc:=1
--EXCEPT
--X         mov qword[rbp+32],rax               -- return address
            mov qword[rbp+56],rax               -- return address
            mov qword[rbp+24],rdx               -- called from address
            jmp $_il                            -- jmp code:call_common
          ::callprocret
            test rax,rax
            jnz :%e02atdb0
            ret
        []

--  ::fin
      }


