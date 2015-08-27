--
-- pmalloc.e
--
--  Test file. See how we might implement auto-included ilasm stuff.
--
--  AutoAsm() added to psym.e
--  Needed I_malloc (ttidx of "%opMallocX"), T_malloc (symtab index),
--  T_Asm supplements T_Bin (T_Bin<T_Asm; 1..T_Asm are (virtual) opcodes,
--  T_Bin..T_Ainc are auto_inc (all T_Bin..T_asm are auto-inc), needed to
--  clear fwd flags in ilASM()/global label definition. 
--  No progress yet on a new VMep.
--
-- %opRTErn (pdiag4), StoreFlt (pheap), test in e07, opInit mods, [32/64]

--without debug
include VM\pstack.e

atom hHeap  -- (let this get h4)

--#ilASM{ jmp %opRetf
#ilASM{ jmp :%opRetf
--#ilASM{ jmp :fin

      :%opMallocX   -- (obviously, must match the glabel in psym.e)
        --
        -- size in eax (integer)
--      -- result in eax [DEV atom... (opMalloc in pilx86.e (oh, AND pmain.e!) to do that for us!)]
        -- target addr (atom) in edi
        --
        -- [edi] := malloc(eax)
        --
        cmp eax,h4
        jge :e37atambpi
        test eax,eax
        jg @f
      ::e37atambpi
            -- e37atambpi: "argument to allocate must be positive integer"
            mov al,37
            xor edi,edi                         -- ep1 unused
            xor esi,esi                         -- ep2 unused
            jmp %opRTErn                        -- fatal error
      @@:
        mov edx,[hHeap]
        push edi        -- save (target addr)
        cmp edx,h4
        jl :hHeapSet
            je :initM -- (preserves eax, sets edx, resumes at hHeapSet)
            sub esp,8
            fld qword[ebx+edx*4]
            fistp qword[esp]
            pop edx
            add esp,4
      ::hHeapSet
        push eax                                    -- dwBytes
        push ebx                                    -- dwFlags (0)
        push edx                                    -- hHeap
        call "kernel32.dll","HeapAlloc"
        -- (note that if NULL is returned, GetLastError is no help [see MSDN])
        pop edi         -- restore (target addr)
        push ebx
        push eax
        fild qword[esp]
        add esp,8
        jmp %opMovbi    -- call StoreFlt ([edi]:=ST0)

      :%opMfreeX    -- (obviously, must match the glabel in psym.e)
        cmp eax,h4
        jl @f
            je :e97he   -- unassigned (DEV opUnassigned shd have been called?)
            sub esp,8
            fld qword[ebx+eax*4]
            fistp qword[esp]
            pop eax
            add esp,4
      @@:
        mov edx,[hHeap]
        cmp edx,h4
        jl :hHeapSet2
--          je :initM -- erm:
            je :e97he -- clearly %opMallocX has never been called...
            sub esp,8
            fld qword[ebx+edx*4]
            fistp qword[esp]
            pop edx
            add esp,4
      ::hHeapSet2
        push eax                                    -- lpMem
        push ebx                                    -- dwFlags
        push edx                                    -- hHeap
        call "kernel32.dll","HeapFree"
        test eax,eax
        jnz @f
      ::e97he
            -- e97he: heap error [%s]
            call "kernel32","GetLastError"
            mov edi,eax -- ep1
            mov al,97
            xor esi,esi -- ep2 unused
            jmp %opRTErn
      @@:
        ret

      ::initM
        push eax    -- save
        call "kernel32.dll","GetProcessHeap"
        test eax,eax
        jnz @f
            -- (might need HeapCreate?)
            add esp,4   -- discard that saved edi (target addr)
            jmp e97he
      @@:
        mov [hHeap],eax
        mov edx,eax
        cmp eax,h4
        jl @f
            fild dword[hHeap]
            push edx
            lea edi,[hHeap]
            call %opMovbi   -- call StoreFlt ([edi]:=ST0)
            pop edx
      @@:
        pop eax     -- restore
        jmp :hHeapSet

--  ::fin }
   }
