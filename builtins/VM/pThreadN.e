--
-- pThreadN.e
-- ==========
--
--  Phix implementation of threads
--

--now defined in psym.e:
--global constant CREATE_SUSPENDED = #00000004
--global constant INFINITE       = #FFFFFFFF  -- Infinite timeout
--global constant WAIT_ABANDONED = #00000080
--global constant WAIT_TIMEOUT   = #00000102
--global constant WAIT_OBJECT_0  = #00000000
--global constant WAIT_FAILED    = #FFFFFFFF
--global constant STILL_ACTIVE   = 259

integer init = 0
--DEV these should probably all be replaced with #ilASM...
atom kernel32,
    xCreateEvent,
    xSetEvent,
    xResetEvent,
    xWaitForSingleObject,
--  xCreateThread,
    xGetExitCodeThread,
    xSuspendThread,
    xResumeThread,
    xCloseHandle,
    xGetLastError

sequence suspended,     -- list of suspended thread-ids
         suspendcs      -- corresponding critical sections
integer susp_cs         -- thread-safe access to ""

procedure t_init()
--puts(1,"pThreadN.e not linux\n")
    if platform()=WINDOWS then
        --DEV we might be able to get rid of these...
        kernel32 = open_dll("kernel32.dll")

        xCreateEvent = define_c_func(kernel32,"CreateEventA",
            {C_PTR,     --  LPSECURITY_ATTRIBUTES  lpEventAttributes, // address of security attributes  
             C_INT,     --  BOOL  bManualReset, // flag for manual-reset event 
             C_INT,     --  BOOL  bInitialState, // flag for initial state 
             C_PTR},    --  LPCTSTR  lpName     // address of event-object name  
            C_PTR)      -- HANDLE

        xSetEvent = define_c_func(kernel32,"SetEvent",
            {C_PTR},    --  HANDLE  hEvent      // handle of event object 
            C_INT)      -- BOOL

        xResetEvent = define_c_func(kernel32,"ResetEvent",
            {C_PTR},    --  HANDLE  hEvent      // handle of event object 
            C_INT)      -- BOOL

        xWaitForSingleObject = define_c_func(kernel32, "WaitForSingleObject",
            {C_PTR,     --  HANDLE hObject, // handle of object to wait for
             C_LONG},   --  DWORD dwTimeout // time-out interval in milliseconds
            C_LONG)     -- DWORD -- WAIT_ABANDONED, WAIT_OBJECT_0, WAIT_TIMEOUT, or WAIT_FAILED

--      xCreateThread = define_c_func(kernel32,"CreateThread",
--          {C_PTR,     --  LPSECURITY_ATTRIBUTES  lpThreadAttributes,  // address of thread security attributes  
--           C_LONG,    --  DWORD  dwStackSize, // initial thread stack size, in bytes 
--           C_PTR,     --  LPTHREAD_START_ROUTINE  lpStartAddress,     // address of thread function 
--           C_PTR,     --  LPVOID  lpParameter,        // argument for new thread 
--           C_LONG,    --  DWORD  dwCreationFlags,     // creation flags 
--           C_PTR},    --  LPDWORD  lpThreadId         // address of returned thread identifier 
--          C_PTR)      -- HANDLE

        xGetExitCodeThread = define_c_func(kernel32,"GetExitCodeThread",
            {C_PTR,     --  _In_     HANDLE hThread
             C_PTR},    --  _Out_  LPDWORD lpExitCode
            C_INT)      -- BOOL        

        xSuspendThread = define_c_func(kernel32,"SuspendThread",
            {C_PTR},    --  _In_     HANDLE hThread
            C_INT)      -- DWORD       
               
        xResumeThread = define_c_func(kernel32,"ResumeThread",
            {C_PTR},    --  _In_     HANDLE hThread
            C_INT)      -- DWORD       

        xCloseHandle = define_c_func(kernel32,"CloseHandle",
            {C_PTR},    --  HANDLE  hObject     // handle of object to close  
            C_INT)      -- BOOL

        xGetLastError = define_c_func(kernel32,"GetLastError",
            {},
            C_INT)      -- DWORD

    else -- LINUX
        suspended = {}
        suspendcs = {}
        susp_cs = init_cs()
    end if

    init = 1
end procedure

type bool(integer flag)
    return (flag=0 or flag=1)
end type

procedure start_thread(sequence s)
-- internal routine, see create_thread()
integer rid
sequence params
integer cs
    if length(s)!=3 then ?9/0 end if
    {rid,params,cs} = s
    if cs!=0 then
        -- linux implementation of CREATE_SUSPENDED
        enter_cs(cs)
        leave_cs(cs)
        delete_cs(cs)
    end if
    call_proc(rid,params)
end procedure

global function create_thread(integer rid, sequence params, integer flags=0)
--
-- Start a new thread by invoking rid (a procedure) with specified parameters
--
-- The optional flags parameter may be:
--  0 (the default)         -- thread runs immediately after creation
--  CREATE_SUSPENDED (4)    -- thread is created in a suspended state, and
--                          --  does not run until resume_thread() called. 
-- 
atom hThread
integer cs = 0
--  if not init then t_init() end if    -- (not needed)
    if not init then t_init() end if
    if platform()=LINUX then
        if flags=CREATE_SUSPENDED then
            cs = init_cs()
            enter_cs(cs)
        end if
    end if
    params = {rid,params,cs}    -- (btw, this gets freed at the end of start_thread() above)
    #ilASM{
        [PE32]
            mov eax,[params]
            mov ecx,[flags]
            mov [params],ebx                    -- mov [params],0   (without any ref counting)
            jmp :createthread

          ::threadproc
            call :%pNewStack                    -- (still has a dummy T_maintls, btw)
            --
            -- NB: we now have NO ACCESS to /ANY/ params or local variables.
            --  (except for a copy of params as passed to kernel32:CreateThread)
            --
            add esp,4                           -- discard return address (into knl32)
            mov edx,routine_id(start_thread)    -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[start_thread][S_Ltot])
            call :%opFrame
            pop dword[ebp]                      --[1] {rid,params}
--EXCEPT
--X         mov dword[ebp+16],:threadret        -- return address
            mov dword[ebp+28],:threadret        -- return address
            jmp $_il                            -- jmp code:start_thread

         ::threadret
            call :%pFreeStack
            push ebx
            call "kernel32.dll","ExitThread"    -- ExitThread(0)

          ::createthread
            push ebx                            -- dwThreadId (result, discarded)
            mov esi,esp

            push esi                            -- lpThreadId
            push ecx                            -- dwCreationFlags (aka flags parameter)
            push eax                            --[1] lpParameter
            push :threadproc                    -- lpStartAddress
            push ebx                            -- dwStackSize (0)
            push ebx                            -- lpThreadAttributes (NULL)
            call "kernel32.dll","CreateThread"
            mov [esp],ebx                       -- overwrite/discard dwThreadId
            push eax                            -- (unsigned extend)
            fild qword[esp]
            add esp,8
            lea edi,[hThread]
            call :%pStoreFlt                    -- ([edi]:=st0, as 31 bit int if possible)
        [ELF32]
--DEV there is a list of non-thread-safe functions at https://www.unmanarc.com/v1/2013/08/cc-p-thread-code-safety-avoiding-race-conditions/
--  (one that struck me was localtime, used in pdir.e)
            mov eax,[params]
            jmp :createthread

          ::threadproc
            call :%pNewStack                    -- (still has a dummy T_maintls, btw)
            --
            -- NB: we now have NO ACCESS to /ANY/ params or local variables.
            --  (except for a copy of params as passed to pthread_create)
            --
            mov edx,routine_id(start_thread)    -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[start_thread][S_Ltot])
            call :%opFrame
            mov eax,[esp+4]                     --[1] {rid,params,cs}
            mov [ebp],eax
--EXCEPT
--X         mov dword[ebp+16],:threadret        -- return address
            mov dword[ebp+28],:threadret        -- return address
            jmp $_il                            -- jmp code:start_thread

         ::threadret
            call :%pFreeStack
            xor ebx,ebx 
--          mov eax,1                           -- sys_exit
--          int 0x80 
            push ebx
            call "libpthread.so.0","pthread_exit"

          ::createthread
            mov [params],ebx                    -- mov [params],0   (without any ref counting)
            push ebx                            -- space for thread_id
            mov esi, esp
            push eax                            -- *arg [params]
            push :threadproc                    -- start_routine
            push ebx                            -- *attr [NULL]
            push esi                            -- *thread
            call "libpthread.so.0","pthread_create"
            add esp,16
            xor ebx,ebx
            test eax,eax
            jz @f
                int3
          @@:
            pop eax
            lea edi,[hThread]
            call :%pStoreMint                   -- ([edi]:=eax, as float if rqd)

        [PE64]
            mov rax,[params]
            mov rcx,[flags]
            mov [params],rbx                    --  mov [params],0  (without any ref counting)
            jmp :createthread

          ::threadproc
            mov [rsp],rcx
--          push rcx            
            call :%pNewStack                    -- (still has a dummy T_maintls, btw)
            --
            -- NB: we now have NO ACCESS to /ANY/ params or local variables.
            --  (except for a copy of lpParameter as passed to kernel32:CreateThread)
            --
--          add rsp,8                           -- discard return address (into knl32)
            mov rdx,routine_id(start_thread)    -- mov edx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov ecx,imm32 (=symtab[start_thread][S_Ltot])
            call :%opFrame
            pop qword[rbp]                      --[1] {rid,params}
--EXCEPT
--X         mov qword[rbp+32],:threadret        -- return address
            mov qword[rbp+56],:threadret        -- return address
            jmp $_il                            -- jmp code:start_thread

         ::threadret
            call :%pFreeStack
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whatever alignment we started with
            sub rsp,8*5                         -- minimum 4 param shadow space, and align

            mov rcx,rbx                         -- dwExitCode
            call "kernel32.dll","ExitThread"    -- ExitThread(0)
--          mov rsp,[rsp+8*5]

          ::createthread
            mov rdx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rdx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whatever alignment we started with
            sub rsp,8*7                         -- 6 params and align/dwThreadId

            mov [rsp+48],rbx                    -- dwThreadId (result, discarded)
            lea rsi,[rsp+48]
            mov [rsp+40],rsi                    -- lpThreadId
            mov [rsp+32],rcx                    -- dwCreationFlags (aka flags parameter)
            mov r9,rax                          --[1] lpParameter
            mov r8,:threadproc                  -- lpStartAddress
            mov rdx,rbx                         -- dwStackSize (0)
            mov rcx,rbx                         -- lpThreadAttributes (NULL)
            call "kernel32.dll","CreateThread"
            mov rsp,[rsp+8*7]

            push rax
            fild qword[rsp]
            add rsp,8
            lea rdi,[hThread]
            call :%pStoreFlt                    -- ([rdi]:=st0, as 63 bit int if possible)
        [ELF64]
            mov rcx,[params]
            jmp :createthread

          ::threadproc
            call :%pNewStack                    -- (still has a dummy T_maintls, btw)
            --
            -- NB: we now have NO ACCESS to /ANY/ params or local variables.
            --  (except for a copy of params as passed to pthread_create)
            --
            mov rdx,routine_id(start_thread)    -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[start_thread][S_Ltot])
            call :%opFrame
            mov rax,[rsp+8]                     --[1] {rid,params,cs}
            mov [rbp],rax
--EXCEPT
--X         mov qword[rbp+32],:threadret        -- return address
            mov qword[rbp+56],:threadret        -- return address
            jmp $_il                            -- jmp code:start_thread

         ::threadret
            call :%pFreeStack
            xor rdi,rdi 
            call "libpthread.so.0","pthread_exit"

          ::createthread
            mov [params],rbx                    -- mov [params],0   (without any ref counting)
--int pthread_create(pthread_t *thread, const pthread_attr_t *attr,
--                        void *(*start_routine) (void *), void *arg);
--  xor rsi, rsi                    -- (arg2)
--  push rax 
--  mov rdi, rsp                    -- (arg1)
--  mov edx, _thread_routine_       -- (arg3)
--  mov rcx, r12                    -- (arg4)
--  call [_pthread_create_] 
            push rbx                            -- space for thread_id
--          <rcx already set>                   -- *arg [params]
            mov rdx, :threadproc                -- start_routine
            mov rsi,rbx                         -- *attr [NULL]
            mov rdi,rsp                         -- *thread
            call "libpthread.so.0","pthread_create"
            xor rbx,rbx
            test rax,rax
            jz @f
                int3
          @@:
            pop rax
            lea rdi,[hThread]
            call :%pStoreMint                   -- ([rdi]:=rax, as float if rqd)
        []
           }
    if platform()=LINUX then
        if flags=CREATE_SUSPENDED then
            -- add entries for resume_thread
            enter_cs(susp_cs)
            suspended = append(suspended,hThread)
            suspendcs = append(suspendcs,cs)
            leave_cs(susp_cs)
        end if
    end if
    return hThread
end function

global procedure suspend_thread(atom hThread)
atom dwError
    if not init then t_init() end if
    if platform()=WINDOWS then
        if c_func(xSuspendThread,{hThread})=-1 then
            dwError = c_func(xGetLastError,{})
            ?9/0
        end if
    else -- LINUX
        ?9/0 -- not supported
    end if
end procedure

global procedure resume_thread(atom hThread)
atom dwError
    if not init then t_init() end if
    if platform()=WINDOWS then
        if c_func(xResumeThread,{hThread})=-1 then
            dwError = c_func(xGetLastError,{})
            ?9/0
        end if
    else
        enter_cs(susp_cs)
        integer k = find(hThread,suspended)
        if k=0 then ?9/0 end if
        leave_cs(suspendcs[k])  -- release start_thread()
        suspended[k..k] = {}    --  (no further use)
        suspendcs[k..k] = {}    -- [delete_cs()'d in start_thread()]
        leave_cs(susp_cs)
    end if
end procedure

global procedure wait_thread(object hThread)
atom dwError
    if sequence(hThread) then
        for i=1 to length(hThread) do
            wait_thread(hThread[i])
        end for
    else
        if not init then t_init() end if
        if platform()=WINDOWS then
            dwError = c_func(xWaitForSingleObject,{hThread,INFINITE})
            if dwError!=WAIT_OBJECT_0 then  -- [WAIT_FAILED?]
                dwError = c_func(xGetLastError,{})
                ?9/0
            end if
            if not c_func(xCloseHandle,{hThread}) then
                ?9/0
            end if
        else
            #ilASM{
                [ELF32]
                    mov eax,[hThread]
                    call :%pLoadMint
                    push ebx                            -- *retval [NULL]
                    push eax                            -- thread
                    call "libpthread.so.0","pthread_join"
                    add esp,8
                    test eax,eax
                    jz @f
                        int3
                  @@:
                [ELF64]
                    mov rax,[hThread]
                    call :%pLoadMint
                    mov rsi,rbx                         -- *retval [NULL]
                    mov rdi,rax                         -- thread
                    call "libpthread.so.0","pthread_join"
                    test rax,rax
                    jz @f
                        int3
                  @@:
                []
                  }
        end if
    end if
end procedure

global procedure exit_thread(integer ecode)
    if ecode=STILL_ACTIVE then ?9/0 end if
    #ilASM{
        [PE32]
            push dword[ecode]                   -- dwExitCode
            call :%pFreeStack
            call "kernel32.dll","ExitThread"
        [ELF32]
            xor ebx,ebx 
--          mov eax,1                           -- sys_exit
--          int 0x80 
            push ebx
            call "libpthread.so.0","pthread_exit"
        [PE64]
            push qword[ecode]                   -- dwExitCode
            call :%pFreeStack
            pop rcx                 
            sub rsp,8*5                         -- minimum 4 param shadow space, and align
            call "kernel32.dll","ExitThread"    -- ExitThread(0)
        [ELF64]
--          xor rbx,rbx 
--          mov rax,60                          -- sys_exit
--          syscall
            xor rdi,rdi
            call "libpthread.so.0","pthread_exit"
        []
           }
end procedure

global function get_thread_id()
atom res
    #ilASM{
                call :%pGetTCB
                -- (aside: while pGetTCB is documented/intended to set esi/rsi, it
                --         also leaves a thread_id in eax/rax, just what we want.)
            [32]
                lea edi,[res]
            [64]
                lea rdi,[res]
            []
                call :%pStoreMint
          }
    return res
end function

constant W = machine_word()

global function get_thread_exitcode(atom hThread)
atom pExitCode, dwExitCode
    if not init then t_init() end if
    if platform()=WINDOWS then
--      pExitCode = allocate(4)
        pExitCode = allocate(W)
        pokeN(pExitCode,0,W)
        bool res = c_func(xGetExitCodeThread,{hThread,pExitCode})
        if res=0 then
            dwExitCode = c_func(xGetLastError,{}) --[only WAIT_FAILED]
            ?9/0
        end if
--      dwExitCode = peek4u(pExitCode)
        dwExitCode = peekNS(pExitCode,W,0)
        free(pExitCode)
    else
        #ilASM{
            [ELF32]
                mov eax,[hThread]
                call :%pLoadMint
                push ebx
--              mov ecx,esp
--              push 3                      -- options (WUNTRACED|WNOHANG)
--              push ecx                    -- int *wstatus
--              push eax                    -- pid
--              call "libc.so.6","waitpid"
--              add esp,12
--              push ecx
--              push eax
                push esp                            -- *retval
                push eax                            -- thread
                call "libpthread.so.0","pthread_join"
                add esp,8
                test eax,eax
                jz @f
                    int3
              @@:
                pop eax
                lea edi,[dwExitCode]
                call :%pStoreMint
            [ELF64]
                mov rax,[hThread]
                call :%pLoadMint
                push rbx
                mov rsi,rsp                         -- *retval
                mov rdi,rax                         -- thread
                call "libpthread.so.0","pthread_join"
                test rax,rax
                jz @f
                    int3
              @@:
                pop rax
                lea rdi,[dwExitCode]
                call :%pStoreMint
            []
              }
            if and_bits(dwExitCode,0o177)=0 then
                return floor(dwExitCode/#80)
            else
                return 0
            end if
    end if
    return dwExitCode
end function

global function thread_safe_string(string s)
-- create a thread-safe version of the passed string.
-- result is only thread safe when it is stored in a 
-- variable that only one thread uses and refcounts.
--1/5/21:
--/*
    if length(s)=0 then
        s = repeat(' ',0)
    else
        s[1] = s[1]     -- (force a clone)
    end if
    return s
--*/
    return deep_copy(s)
end function

--DEV remaining routines are untested (and undocumented)

global function create_event(object name=0, bool manualreset=false, bool initialstate=false)
--
-- name should be unique and can be used to retrieve an existing event, or NULL.
--      it must not match an existing semaphore, mutex, or file-mapping object.
-- if manualreset is false, the state is reset to nonsignalled when a wait is released.
-- if manualreset is true, you must use reset_event() to reset the state to nonsignalled.
-- initialstate indicates whether the event is initially signalled or not.
-- returns an event handle or {error_code} on error
-- (it is quite reasonable to store the result in an atom and debug any type checks)
--
    if not init then t_init() end if
    if platform()=WINDOWS then
        return c_func(xCreateEvent,{NULL,manualreset,initialstate,name})
    else
        return 9/0
    end if
--atom hRes = ""
--  if hRes=NULL then
--      return {c_func(xGetLastError,{})}
--  end if
--  return hRes
end function

global function set_event(atom hEvent)
-- returns true on success, else false (call GetLastError for details)
-- returns 0 on success, else an error code
    if not init then t_init() end if
    if platform()=WINDOWS then
--      return c_func(xSetEvent,{hEvent})
        if not c_func(xSetEvent,{hEvent}) then
            return c_func(xGetLastError,{})
        end if
    else
        ?9/0
    end if
    return 0
end function

global function reset_event(atom hEvent)
-- returns true on success, else false (call GetLastError for details)
    if not init then t_init() end if
    if platform()=WINDOWS then
        return c_func(xResetEvent,{hEvent})
    else
        return 9/0
    end if
end function

global function close_handle(atom hObject)
--  if not init then t_init() end if
    if platform()=WINDOWS then
        return c_func(xCloseHandle,{hObject})
    else
        return 9/0
    end if
end function

--=========================
--==== ***** OLD **** =====
--=========================
--  (you can ignore everything after this point) [DEV][add anything relevant to the docs and then delete all this]

--  The following are not thread safe:
--
--  2) The GUI.
--      I suspect this is true of any programming language.
--      In particular I have made no attempt to make arwen/win32lib/etc
--      thread-safe, nor have I any plans to try. Instead, background
--      threads should send messages to the main thread when the GUI
--      needs to be updated, rather they try it themselves.
--
--  3) pcfunc.e
--      ** pretty high up on my to-do list **
--      requires thread local storage of ebp_loword/ebp_hiword/ebp_valid.   [this may be ok now]
--      notstring/cstrings/addr minor rework/multiple results
--      Pinit()/table/previd/prevcb require locking
--
--  4) file i/o
--      See (the experimental) builtins/pfileio.e for the sort of problems [pfileio3.e should be ok, ish]
--      faced - the current asm implementation probably has similar.
--      The recommendation is to write wrapper routines with locking.
--      (erm, that should only be required for two threads attacking the same file)
--
--  5) database.e
--      The recommendation here is to either put all database access into
--      a single thread, or write wrapper routines that perform locking.
--
--  6) file.e
--      set_walk_dir_sort_rtn. No solution offered as yet. (TLS?)   [ optional parameters! ]
--
--  7) get.e 
--      input_file/input_string/string_next/ch need reworking, possibly
--      similar to what I did with pprntf.e
--
--  8) image.e
--      fn/error_code/numXPixels/numYPixels/bitCount/numRowBytes ditto
--
--  9) misc.e/pretty_print
--      pretty_end_col..pretty_line ditto
--
--  A) builtins/ppp.e
--      ppp_Maxlen..nindent ditto
--      cl1q and cllq probably trivially fixed by moving into pprntf()
--
--  B) builtins/syswait.e
--      Wait, doevents, and doeventparams are not thread safe.
--      set_system_doevents needs to be replaced (by optional parameters?)
--      si, pi, etc need to be made local (allocated and freed every time)
--
--  C) builtins/pdelete.e
--      [DelRef] in the backend, for one. Also noted this is not unit tested!
--
--  Several other places in builtins are marked
--      --DEV needs locking as per pprntf



