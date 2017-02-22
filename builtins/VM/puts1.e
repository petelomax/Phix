--
-- puts1.e
-- =======
--
--  Low-level simplistic console i/o
--
--  These routines exist so that we can output some simple console messages before all of
--  pHeap.e/pStack.e/pprntfN.e/psprintN.e/pfileioN.e/pMath.e/pSubseN.e/pSubssN.e/pJcc.e
--  pRmdr.e/pRepeN.e/pRepsN.e/pApnd.e etc are working, or if they get broken in any way.
--
--  See ..\puts1h.e for hll wrapper routines/hints on invoking these from inline assembly.
--  This file was made an autoinclude purely because it was an easy one to start off with.
--  Note that unlike this, ..\puts1h.e requires an explicit include statement before use.
--  Also note the original version of this file was deliberately split into puts1/puts1h
--  so that this can be put into the optable (:%labels only) and the latter left out.
--
--      in here         in puts1h.e
--      =======         ===========
--      :%puts1         puts1(s)        equivalent to     puts(1,s)
--                                                        puts(1,raw(edi|rdi)[1..$])
--      :%puts1ediesi                                     puts(1,*edi[1..esi])  -- (32-bit char*)
--      :%puts1rdirsi                                     puts(1,*rdi[1..rsi])  -- (64-bit char*)
--      :%puthex32a     puthex32(a)     equivalent to     printf(1,"%08x\n",{a})    -- (atom/int)
--      :%puthex32                                        printf(1,"%08x\n",{edx|and_bits(rdx,#FFFFFFFF)})  -- 32-bit reg
--      :%puthex64                                        printf(1,"%016x\n",{edx:eax|rdx})
--      :%putsint       putsint(i)      equivalent to     printf(1,"%d\n",{i})
--                                                        printf(1,"%d\n",{edx|and_bits(rdx,#FFFFFFFF)})
--      :%getc0         getc0()         equivalent to     {} = getc(0), aka if getc(0) then end if
--
--  These routines are used by the likes of pFEH.e and pdiagN.e and therefore form part of
--  the runtime VM. That means they are always automatically available and further that an
--  explicit "include builtins\VM\puts1.e" may be quietly ignored when interpreting - any
--  changes may need a "p -cp" round before coming into effect. It may be easier to make a
--  copy of this that you can edit/test rather than edit/compile(/compile)/test, though I
--  would urge you to carefully rename all the global labels in the test copy.
--
--  This file was chosen for use in the first steps to getting the optable to work, and
--  also a key factor in deciding to use :!opCallOnceYeNot instead of :%opRetf/::fin.
--
with debug

--/*
This will not work on RDS Eu/OpenEuphoria!!
--*/

--include builtins\VM\pStack.e
include builtins\VM\pUnassigned.e   -- :!opCallOnceYeNot

--#ilASM{ jmp :%opRetf
--#ilASM{ jmp :fin
#ilASM{ jmp :!opCallOnceYeNot

--/*
procedure :%puts1(:%)
end procedure -- (for Edita/CtrlQ)
--*/
      :%puts1
-------------
        [32]
            -- edi loaded
            xor ebx,ebx
            mov esi,[ebx+edi*4-12]          -- length
            shl edi,2                       -- ref->raw

--/*
procedure :%puts1ediesi(:%)
end procedure -- (for Edita/CtrlQ)
--*/
      :%puts1ediesi     -- (edi=raw text, esi=length)
-------------------
        [PE32]
--added 14/11/16:
            call "kernel32.dll","AllocConsole"
            push -11                        -- nStdHandle (p1) (-11=STD_OUTPUT_HANDLE)
            call "kernel32.dll","GetStdHandle"
            push ebx                        -- lpOverlapped (NULL)
            push esp                        -- lpNumberOfBytesWritten
            push esi                        -- nNumberOfBytesToWrite
            push edi                        -- lpBuffer
            push eax                        -- hFile,
            call "kernel32.dll","WriteFile"
        [ELF32]
            mov eax,4               -- sys_write(ebx=int fd,ecx=char *buf,edx=int len)
            mov edx,esi             -- length (p3)
            mov ecx,edi             -- raw(hw) (p2)
            mov ebx,1               -- stdout (p1)
            int 0x80
            xor ebx,ebx             -- (common requirement after int 0x80)

        [64]
            -- rdi loaded
            xor rbx,rbx
            mov rsi,[rbx+rdi*4-24]          -- length
            shl rdi,2

--/*
procedure :%puts1rdirsi(:%)
end procedure -- (for Edita/CtrlQ)
--*/
      :%puts1rdirsi     -- (rdi=raw text, rsi=length)
-------------------
        [PE64]
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whatever alignment we started with
            sub rsp,8*7                     -- 5 params and space for target of r9, plus align
            mov rcx,-11                     -- DWORD nStdHandle (p1) (--11=STD_OUTPUT_HANDLE)
--added 14/1/16:
            call "kernel32.dll","AllocConsole"
            call "kernel32.dll","GetStdHandle"
            mov qword[rsp+4*8],rbx          -- LPOVERLAPPED lpOverlapped (p5) (rbx=NULL)
            lea r9,[rsp+5*8]                -- LPDWORD lpNumberOfBytesWritten (p4)
            mov r8,rsi                      -- DWORD nNumberOfBytesToWrite (p3)
            mov rdx,rdi                     -- LPCVOID lpBuffer (p2)
            mov rcx,rax                     -- HANDLE hFile (p1)
            call "kernel32.dll","WriteFile"
--          add rsp,8*7
--          pop rsp     -- restore, equivalent to rsp += (either #08 or #10)
            mov rsp,[rsp+8*7]   -- equivalent to add/pop
        [ELF64]
            mov rdx,rsi             -- length(hw)
            mov rsi,rdi             -- raw(hw)
            mov rdi,1               -- stdout
            mov rax,1               -- sys_write(rdi=unsigned int fd,rsi=const char *buf,rdx=size_t count)
            syscall
        []
            ret

--/*
procedure :%puthex32a(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%puthex32a
---------------
        [32]
            -- edx loaded (with an integer or phix atom)
            xor ebx,ebx
            sub esp,16      -- build "00000000\r\n" on the stack
            cmp edx,h4
            jl @f
                fld qword[ebx+edx*4]
                fistp qword[esp]
                mov edx,[esp]
          @@:
            mov edi,esp
            jmp @f
        [64]
            -- rdx loaded (with an integer or phix atom)
            xor rbx,rbx
            mov r15,h4
            sub rsp,16      -- ditto (8 digits), also keeps stack (mis)aligned
            cmp rdx,r15
            jl @f
--DEV %pLoadMint
                fld tbyte[rbx+rdx*4]
                fistp qword[rsp]
                mov rdx,[rsp]
          @@:
            mov rdi,rsp
            jmp @f
        []

--/*
procedure :%puthex32(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%puthex32
--------------
        [32]
            -- edx loaded (with an absolute 32bit value)
            sub esp,16      -- build "00000000\r\n" on the stack
          @@:
            mov edi,esp
        [64]
            -- rdx loaded (but only the edx part gets printed)
            sub rsp,16      -- ditto (8 digits), also keeps stack (mis)aligned
          @@:
            mov rdi,rsp
        []
            mov ecx,8
          ::loop8
            xor eax,eax
            shld eax,edx,4  -- peel off top nibble
            shl edx,4       -- discard ""/get next nibble into place
            add al,0x37     -- + 'A'-9
            cmp al,0x41     -- cmp 'A'
            jge @f
                sub al,7    -- map '7'..'@' to '0'..'9' (or something like that)
          @@:
            stosb
            sub ecx,1
            jnz :loop8
            mov word[edi],0x0D0A
        [32]
            mov edi,esp
            mov esi,10
            cmp dword[esp+20],0 -- putcr
            jne @f
                mov esi,8
          @@:
            call :%puts1ediesi
            add esp,16
            ret 4
        [64]
            mov rdi,rsp
            mov rsi,10
            cmp qword[rsp+24],0
            jne @f
                mov rsi,8
          @@:
            call :%puts1rdirsi
            add rsp,16
            ret 8
        []

--/*
procedure :%puthex64(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%puthex64
--------------
        [32]    
            -- edx:eax loaded (hi-dword in edx, lo-dword in eax)
            sub esp,32
            mov edi,esp
            mov esi,16
          ::loop16edxeax
            xor ecx,ecx
            shld ecx,edx,4  -- peel off top nibble
            shld edx,eax,4  -- peel off top nibble
            shl eax,4       -- discard ""/get next nibble into place
            add cl,0x37     -- + 'A'-9
            cmp cl,0x41     -- cmp 'A'
            jge @f
                sub cl,7    -- map '7'..'@' to '0'..'9' (or something like that)
          @@:
            mov [edi],cl
            add edi,1
            sub esi,1
            jnz :loop16edxeax
            mov word[edi],0x0D0A
            mov edi,esp
            mov esi,18
            cmp dword[esp+36],0
            jne @f
                mov esi,16
          @@:
            call :%puts1ediesi
            add esp,32
            ret 4

        [64]
            -- rdx loaded   -- (or, to show the lo-dword of rdx, aka edx, use :%puthex32)
            sub rsp,32
            mov rdi,rsp
            mov rcx,16
          ::loop16
            xor rax,rax
            shld rax,rdx,4  -- peel off top nibble
            shl rdx,4       -- discard ""/get next nibble into place
            add al,0x37     -- + 'A'-9
            cmp al,0x41     -- cmp 'A'
            jge @f
                sub al,7    -- map '7'..'@' to '0'..'9' (or something like that)
          @@:
            stosb
            sub rcx,1
            jnz :loop16
            mov word[rdi],0x0D0A
            mov rdi,rsp
            mov rsi,18
--          cmp qword[rsp+24],0
            cmp qword[rsp+40],0
            jne @f
                mov rsi,16
          @@:
            call :%puts1rdirsi
            add rsp,32
            ret 8
        []

--/*
procedure :%putsint(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%putsint
-------------
        [32]
            -- eax loaded, putscr on stack
            sub esp,16      -- build "[-]nnn\r\n" on the stack
            mov edi,esp
            cmp eax,0
            jge @f
                neg eax
                mov byte[edi],'-'
                add edi,1
          @@:
        [64]
            -- eax loaded, putscr on stack
            sub rsp,16      -- build "[-]nnn\r\n" on the stack
            mov rdi,rsp
            cmp eax,0
            jge @f
                neg eax
                mov byte[rdi],'-'
                add rdi,1
          @@:
        []
            mov ecx,1000000000  -- repeatedly divided by 10 until it is 1, as we get each digit.
            xor edx,edx         -- edx:=0
            xor esi,esi         -- clear esi, the 'some digits have already been printed' flag.
          @@:
            div ecx             -- eax:=eax/ecx, (so eax=top digit) edx:=remainder
            push edx            -- save remainder
            cmp ecx,1           -- always print the last digit
            je :display_digit
            test esi,esi        -- or if any have already been printed
            jnz :display_digit
--          test al,al          -- or if this digit is not zero (ie eax)
            test eax,eax        -- or if this digit is not zero (ie eax)
            jz :digit_skip
--          not esi             -- set flag indicating a digit has been printed
            or esi,1            -- set flag indicating a digit has been printed
          ::display_digit
            add al,0x30         -- +'0'
            stosb
          ::digit_skip
            mov eax,ecx         -- all divisions must occur in eax
            xor edx,edx         -- zero edx
            mov ecx,10
            div ecx             -- eax:=eax/10
            mov ecx,eax         -- save the new, smaller, divisor
            pop eax             -- restore the saved remainder back to eax
            test ecx,ecx        -- all done?
            jnz @b
        [32]
            mov eax,[esp+20]    -- putcr
            mov ecx,edi
            test eax,eax
            jz @f
                add ecx,2
                mov word[edi],0x0D0A
          @@:
            sub ecx,esp
            mov edi,esp
            mov esi,ecx -- (improve me, once working)
            call :%puts1ediesi
            add esp,16
            ret 4
        [64]
            mov rax,[rsp+24]    -- putcr
            mov rcx,rdi
            test rax,rax
            jz @f
                add rcx,2
                mov word[rdi],0x0D0A
          @@:
            sub rcx,rsp
            mov rdi,rsp
            mov rsi,rcx
            call :%puts1rdirsi
--          add rsp,24
            add rsp,16
            ret 8
        []

--/*
procedure :%getc0(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%getc0
-----------
        [PE32]
            push -10                        -- nStdHandle (p1) (-10=STD_INPUT_HANDLE)
            call "kernel32.dll","GetStdHandle"
-- 23/4/15:
            push eax                        -- save
            push 1                          -- fdwMode (1=ENABLE_PROCESSED_INPUT)
            push eax                        -- hConsole
            call "kernel32.dll","SetConsoleMode"
            pop eax                         -- restore

            push ebx            -- reserve space for buffer (1 byte realy)
            mov esi,esp
            push ebx            -- reserve space for NumberOfBytesRead
            mov edi,esp
            push ebx                        -- lpOverlapped
            push edi                        -- lpNumberOfBytesRead
            push dword 1                    -- nNumberOfBytesToRead (1)
            push esi                        -- lbBuffer
            push eax                        -- hFile
            call "kernel32.dll","ReadFile"
            add esp,8
        [ELF32]
            push ebx                -- char *buf (1 byte)
            mov eax,3               -- sys_read(ebx=int fd,ecx=char *buf,edx=int len)
            mov edx,1               -- length (p3)
            lea ecx,[esp+4]         -- buf (p2)
            mov ebx,0               -- stdin (p1)
            int 0x80
            xor ebx,ebx             -- (common requirement after int 0x80)
            add esp,4
        [PE64]
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whatever alignment we started with
            sub rsp,8*7                     -- 5 params plus buffer and bytesread (no align rqd)
            mov rcx,-10                     -- DWORD nStdHandle (p1) (-10=STD_INPUT_HANDLE)
            call "kernel32.dll","GetStdHandle"

-- 23/4/15:
            mov [rsp+5*8],rax               -- save
            mov rdx,1                       -- fdwMode (1=ENABLE_PROCESSED_INPUT)
            mov rcx,rax                     -- hConsole
            call "kernel32.dll","SetConsoleMode"
            mov rax,[rsp+5*8]               -- restore

            mov [rsp+5*8],rbx               -- lbBuffer (zero the high bits)
            mov qword[rsp+4*8],rbx          -- LPOVERLAPPED lpOverlapped (p5) (rbx=NULL)
            lea r9,[rsp+6*8]                -- LPDWORD lpNumberOfBytesRead (p4)
            mov r8,1                        -- nNumberOfBytesToRead (p3) (1)
            lea rdx,[rsp+5*8]               -- lbBuffer (p2)
            mov rcx,rax                     -- hFile (p1)
            call "kernel32.dll","ReadFile"
--          add rsp,8*7
--          pop rsp     -- restore, equivalent to rsp += (either #08 or #10)
            mov rsp,[rsp+8*7]   -- equivalent to add/pop
        [ELF64]
            push rbx                -- char *buf (1 byte)
            mov rdx,10              -- length (p3)
            mov rsi,rsp             -- buf (p2)
            mov rdi,0               -- stdin
            mov rax,0               -- sys_read(rdi=unsigned int fd,rsi=char *buf,rdx=size_t count)
            syscall
            add rsp,8
        []
            ret
--::fin
    }


