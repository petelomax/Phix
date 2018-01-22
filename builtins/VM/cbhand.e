--
-- cbhand.e (Phix compatible 0.7.2)
--

--  Phix implementation of %cbhandler as used by pcfunc.e and made separate 
--  from that so that !cb_ret could be put into the optable (for pDiagN.e).
--
--/*
    ?9/0    This file is wholly incompatible with RDS Eu/OpenEuphoria.
    ~?|/$   Commenting this out to remove compile error will not help,
    ~/|$?   but making it "--|**|include builtins\VM\cbhand.e" might.
--*/ ---    (DEV using | instead of / above because of bug in synclr)

--!/**/without debug
--!/**/with debug

include builtins\VM\pHeap.e         -- :%pStoreFlt etc
include builtins\VM\pStack.e        -- :%opFrame etc
include builtins\VM\pUnassigned.e   -- opCallOnceYeNot etc

--DEV temp:
--include builtins\VM\puts1.e
--string ns = "calling newstack\n"
--string gp = "calling GetPool\n"
--string fp = "calling FreePool\n"

--/*
procedure :%cbhandler(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    #ilASM{ jmp !opCallOnceYeNot
            
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
-- 20/1/2018: (no help)
--call :%pGetTCB
--test esi,esi
--jnz @f
--  mov edi,[ns]
--  call :%puts1
--  call :%pNewStack
--  -- and force newcttcb
--  mov edi,[gp]
--  call :%puts1
--  mov ecx,1
--  call :%pGetPool
--  push eax
--  push edx
--  mov edi,[fp]
--  call :%puts1
----    mov ecx,edx
--  pop ecx
--  pop eax
--  call :%pFreePool
--@@:
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
--DEV (not supported in pilasm.e) [it is now!]
--              mov dword[ebp+16],:!cb_ret
                mov dword[ebp+28],:!cb_ret
--              mov eax,:!cb_ret
--EXCEPT
--              mov dword[ebp+16],eax
--              mov dword[ebp+28],eax
                jmp dword[ebx+esi*4+40] -- execute first opcode (S_il=11)
            :!cb_ret
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
            [ELF64]
                -- first 6 parameters are passed in rdi/rsi/rdx/rcx/r8/r9 (or xmm0..7).
                -- DEV limit lnx to 6 params in callbacks...
                -- maybe: mov rbx,[rsp+???]; push rbx
                --        mov rbx,[rsp+???]; push rbx (as many lines as needed)
                --              (would ??? stay the same on each line?)
                -- or: bump rdi in ::nextparam after 6?? [lea r9(say),[rsp+48+16?]..cmp rdi,r9; addeq rdi,32?]
                push r9
                push r8
                push rcx
                push rdx
                push rsi
                push rdi

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
            [PE64]
                    lea rsi,[rsp+32+64] -- params (on stack)
            [ELF64]
                    lea rsi,[rsp+16]
            [64]
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
--DEV as above
--              mov qword[rbp+32],:!cb_ret
                mov qword[rbp+56],:!cb_ret
--              mov rax,:!cb_ret
--EXCEPT
--              mov qword[rbp+32],rax
--              mov qword[rbp+56],rax
                jmp dword[rbx+rsi*4+80] -- execute first opcode (S_il=11)
            :!cb_ret
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
--DEV (this may be unneccessary, now that cbhandler is in the optable)
                        call :%pGetSymPtr               -- (mov rsi,[ds+8])
                        mov rsi,[rsi+rcx*8-8]           -- rsi:=symtab[rtnid]
                        mov rdx,[rbx+rsi*4+80]          -- first opcode (S_il=11)
                        mov rsi,rcx
                        jmp :!iDiag
                        int3
                  @@:
--DEV :%pLoadMint?
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
            [ELF64]
                add rsp,48
            [64]
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

