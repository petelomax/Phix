--
-- pThreadN.e
-- ==========
--
-- Phix implementation of threads
--  (NB subject to change when linux version gets implemented)
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

procedure t_init()
puts(1,"pThreadN.e not linux\n")
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

--  xCreateThread = define_c_func(kernel32,"CreateThread",
--      {C_PTR,     --  LPSECURITY_ATTRIBUTES  lpThreadAttributes,  // address of thread security attributes  
--       C_LONG,    --  DWORD  dwStackSize, // initial thread stack size, in bytes 
--       C_PTR,     --  LPTHREAD_START_ROUTINE  lpStartAddress,     // address of thread function 
--       C_PTR,     --  LPVOID  lpParameter,        // argument for new thread 
--       C_LONG,    --  DWORD  dwCreationFlags,     // creation flags 
--       C_PTR},    --  LPDWORD  lpThreadId         // address of returned thread identifier 
--      C_PTR)      -- HANDLE

    xGetExitCodeThread = define_c_func(kernel32,"xGetExitCodeThread",
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

    init = 1
end procedure

type bool(integer flag)
    return (flag=0 or flag=1)
end type

procedure start_thread(sequence s)
-- internal routine, see create_thread()
integer rid
sequence params
    if length(s)!=2 then ?9/0 end if
    {rid,params} = s
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
--  if not init then t_init() end if    -- (not needed)
    params = {rid,params}   -- (btw, this gets freed at the end of start_thread() above)
    #ilASM{
        [PE32]
            mov eax,[params]
            mov ecx,[flags]
            mov [params],ebx                    --  mov [params],0  (without any ref counting)
            jmp :createthread

          ::threadproc
            call :%pNewStack                    -- (still has a dummy T_maintls, btw)
            --
            -- NB: we now have NO ACCESS to /ANY/ params or local variables.
            --  (except for a copy of lpParameter as passed to kernel32:CreateThread)
            --
            add esp,4                           -- discard return address (into knl32)
            mov edx,routine_id(start_thread)    -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
            pop dword[ebp]                      --[1] {rid,params}
            mov dword[ebp+16],:threadret        -- return address
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
            pop al
        [PE64]
            mov rax,[params]
            mov rcx,[flags]
            mov [params],rbx                    --  mov [params],0  (without any ref counting)
            jmp :createthread

          ::threadproc
            call :%pNewStack                    -- (still has a dummy T_maintls, btw)
            --
            -- NB: we now have NO ACCESS to /ANY/ params or local variables.
            --  (except for a copy of lpParameter as passed to kernel32:CreateThread)
            --
            add rsp,8                           -- discard return address (into knl32)
            mov rdx,routine_id(start_thread)    -- mov edx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov ecx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
            pop qword[rbp]                      --[1] {rid,params}
            mov qword[rbp+32],:threadret        -- return address
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
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
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
            call :%pStoreFlt                    -- ([rdi]:=st0, as 31 bit int if possible)
        [ELF64]
            pop al
        []
           }
    return hThread
end function

global procedure suspend_thread(atom hThread)
atom dwError
    if not init then t_init() end if
    if c_func(xSuspendThread,{hThread})=-1 then
        dwError = c_func(xGetLastError,{})
        ?9/0
    end if
end procedure

global procedure resume_thread(atom hThread)
atom dwError
    if not init then t_init() end if
    if c_func(xResumeThread,{hThread})=-1 then
        dwError = c_func(xGetLastError,{})
        ?9/0
    end if
end procedure

global procedure wait_thread(object hThread)
--atom pExitCode
atom dwError
    if sequence(hThread) then
        for i=1 to length(hThread) do
            wait_thread(hThread[i])
        end for
    else
        if not init then t_init() end if
        dwError = c_func(xWaitForSingleObject,{hThread,INFINITE})
        if dwError!=WAIT_OBJECT_0 then  -- [WAIT_FAILED?]
            dwError = c_func(xGetLastError,{})
            ?9/0
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
            pop al
        [PE64]
            push qword[ecode]                   -- dwExitCode
            call :%pFreeStack
            pop rcx                 
            sub rsp,8*5                         -- minimum 4 param shadow space, and align
            call "kernel32.dll","ExitThread"    -- ExitThread(0)
        [ELF64]
            pop al
        []
           }
end procedure

global function get_thread_exitcode(atom hThread)
atom pExitCode, dwExitCode
    if not init then t_init() end if
    pExitCode = allocate(4)
    if c_func(xGetExitCodeThread,{hThread,pExitCode})=0 then
        dwExitCode = c_func(xGetLastError,{}) --[only WAIT_FAILED]
        ?9/0
    end if
    dwExitCode = peek4u(pExitCode)
    free(pExitCode)
    return dwExitCode
end function


--DEV remaining routines are untested (and undocumented)

global function create_event(object name=0, bool manualreset=0, bool initialstate=0)
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
    return c_func(xCreateEvent,{NULL,manualreset,initialstate,name})
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
--  return c_func(xSetEvent,{hEvent})
    if not c_func(xSetEvent,{hEvent}) then
        return c_func(xGetLastError,{})
    end if
    return 0
end function

global function reset_event(atom hEvent)
-- returns true on success, else false (call GetLastError for details)
    if not init then t_init() end if
    return c_func(xResetEvent,{hEvent})
end function

global function close_handle(atom hObject)
    return c_func(xCloseHandle,{hObject})
end function

--=========================
--==== ***** OLD **** =====
--=========================
--  (you can ignore everything after this point) [DEV][add anything relevant to the docs and then delete all this]

--  The following are not thread safe:
--
--  1) Reference Counting (in progress).
--      Throughout the backend and in all emitted code, I need to replace eg
--
--          sub dword[ebx+edx*4-8],1
--
--      with
--      
--          mov eax,-1 ; (any spare reg will do)
--          lock xadd dword[ebx+edx*4-8],eax
--
--      If you see any "*4-8]" that I missed, let me know.
--      There are also some similar changes to the automatic memory leak
--      checking code/tallies.
--
--          --      If two or more threads refer to the same (file-level) variable/data, 
--          --      even in an apparently "read only" fashion, they will require locking 
--          --      to prevent reference counting mishaps:
--          --
--          --          Thread 1                            Thread 2
--          --          var1 = X -- incref X
--          --          ...                                 ...
--          --          var1 = 0 -- decref X                var2 = X -- incref X
--          --                      -- ie read 2                        -- ie read 2
--          --                      --   write 1                        --   write 3    
--          --      At the start of the last line the data held in X has a refcount of 2 
--          --      (one for X and one for var1). When both are finished it should be 2
--          --      (one for X and one for var2). However if both manage to read the 2 at 
--          --      the same time, the refcount is going to end up either 1 or 3, wrong.
--          --
--          --          <aside>
--          --              It is possible that replacing eg
--          --
--          --                  sub [refcount],1
--          --                  jnz @f
--          --                      call dealloc
--          --                 @@:
--          --
--          --              with
--          --
--          --                 tryagain:
--          --                  mov edx,[refcount]      ; will become new value
--          --                  mov eax,[refcount]      
--          --                  mov ecx,[refcount]
--          --                  sub edx,1
--          --                  lock cmpxchg [refcount],edx
--          --  or?             lock cmpxchg edx,[refcount]
--          --                  cmp eax,ecx
--          --                  jne tryagain
--            "cmpxchg" compares the value in the AL, AX, or EAX register with the
--          destination operand. If the two values are equal, the source operand is
--          loaded into the destination operand. Otherwise, the destination operand is
--          loaded into the AL, AX, or EAX register. The destination operand may be a
--          general register or memory, the source operand must be a general register.
--
--              cmpxchg dl,bl    ; compare and exchange with register
--              cmpxchg [bx],dx  ; compare and exchange with memory
--
--          --                  
--          --          </aside>
--          --
--          --      Note it is the data itself, not a particular variable, that matters.
--          --      The same can be true of literal constants; if you replace X above
--          --      with "some string" the same problem occurs. ("constant" is really 
--          --      just a compiler directive; an early version of Phix attempted to 
--          --      avoid pointlessly refcounting constants, but generally they make
--          --      up a tiny fraction of the total and any additional checking cost
--          --      more than it saved. Literals are just "unamed constant variables",
--          --      in fact when the compiler sees constant Name = "Pete", it first
--          --      creates an unnamed symtab entry for "Pete", and then realises it
--          --      can put "Name" on that entry rather then create another entry.)
--          --
--          --      If the data is going to vary over time, locking is the only way
--          --      to prevent such mishaps. For finalised data (including literals)
--          --      the unique routine above may offer an alternative solution.
--          --
--DEV undo some of this: [NO!!]
--      Comparing builtins\pprntf.e with builtins\pprntf-old.e may offer
--      some clues for making legacy code thread-safe. It is interesting
--      to note that it is the initialisation of inf/nan/bases/hexchar
--      that is thread-unsafe, rather than their use. I would also agree 
--      that nan() not "nan" verges on the ridiculous, but hey, it works.
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


--/* from MSDN:

#include <windows.h>
#include <tchar.h>
#include <strsafe.h>

#define MAX_THREADS 3
#define BUF_SIZE 255

DWORD WINAPI MyThreadFunction( LPVOID lpParam );
void ErrorHandler(LPTSTR lpszFunction);

// Sample custom data structure for threads to use.
// This is passed by void pointer so it can be any data type
// that can be passed using a single void pointer (LPVOID).
typedef struct MyData {
    int val1;
    int val2;
} MYDATA, *PMYDATA;


int _tmain()
{
    PMYDATA pDataArray[MAX_THREADS];
    DWORD   dwThreadIdArray[MAX_THREADS];
    HANDLE  hThreadArray[MAX_THREADS]; 

    // Create MAX_THREADS worker threads.

    for( int i=0; i<MAX_THREADS; i++ )
    {
        // Allocate memory for thread data.

        pDataArray[i] = (PMYDATA) HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, sizeof(MYDATA));

        if( pDataArray[i] == NULL )
        {
           // If the array allocation fails, the system is out of memory
           // so there is no point in trying to print an error message.
           // Just terminate execution.
            ExitProcess(2);
        }

        // Generate unique data for each thread to work with.

        pDataArray[i]->val1 = i;
        pDataArray[i]->val2 = i+100;

        // Create the thread to begin execution on its own.

        hThreadArray[i] = CreateThread( 
            NULL,                   // default security attributes
            0,                      // use default stack size  
            MyThreadFunction,       // thread function name
            pDataArray[i],          // argument to thread function 
            0,                      // use default creation flags 
            &dwThreadIdArray[i]);   // returns the thread identifier 


        // Check the return value for success.
        // If CreateThread fails, terminate execution. 
        // This will automatically clean up threads and memory. 

        if (hThreadArray[i] == NULL) 
        {
           ErrorHandler(TEXT("CreateThread"));
           ExitProcess(3);
        }
    } // End of main thread creation loop.

    // Wait until all threads have terminated.

>   WaitForMultipleObjects(MAX_THREADS, hThreadArray, TRUE, INFINITE);

    // Close all thread handles and free memory allocations.

    for(int i=0; i<MAX_THREADS; i++)
    {
        CloseHandle(hThreadArray[i]);
        if(pDataArray[i] != NULL)
        {
            HeapFree(GetProcessHeap(), 0, pDataArray[i]);
            pDataArray[i] = NULL;    // Ensure address is not reused.
        }
    }

    return 0;
}


DWORD WINAPI MyThreadFunction( LPVOID lpParam ) 
{ 
    HANDLE hStdout;
    PMYDATA pDataArray;

    TCHAR msgBuf[BUF_SIZE];
    size_t cchStringSize;
    DWORD dwChars;

    // Make sure there is a console to receive output results. 

    hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
    if( hStdout == INVALID_HANDLE_VALUE )
        return 1;

    // Cast the parameter to the correct data type.
    // The pointer is known to be valid because 
    // it was checked for NULL before the thread was created.
 
    pDataArray = (PMYDATA)lpParam;

    // Print the parameter values using thread-safe functions.

    StringCchPrintf(msgBuf, BUF_SIZE, TEXT("Parameters = %d, %d\n"), 
        pDataArray->val1, pDataArray->val2); 
    StringCchLength(msgBuf, BUF_SIZE, &cchStringSize);
    WriteConsole(hStdout, msgBuf, (DWORD)cchStringSize, &dwChars, NULL);

    return 0; 
} 



void ErrorHandler(LPTSTR lpszFunction) 
{ 
    // Retrieve the system error message for the last-error code.

    LPVOID lpMsgBuf;
    LPVOID lpDisplayBuf;
    DWORD dw = GetLastError(); 

    FormatMessage(
        FORMAT_MESSAGE_ALLOCATE_BUFFER | 
        FORMAT_MESSAGE_FROM_SYSTEM |
        FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL,
        dw,
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        (LPTSTR) &lpMsgBuf,
        0, NULL );

    // Display the error message.

    lpDisplayBuf = (LPVOID)LocalAlloc(LMEM_ZEROINIT, 
        (lstrlen((LPCTSTR) lpMsgBuf) + lstrlen((LPCTSTR) lpszFunction) + 40) * sizeof(TCHAR)); 
    StringCchPrintf((LPTSTR)lpDisplayBuf, 
        LocalSize(lpDisplayBuf) / sizeof(TCHAR),
        TEXT("%s failed with error %d: %s"), 
        lpszFunction, dw, lpMsgBuf); 
    MessageBox(NULL, (LPCTSTR) lpDisplayBuf, TEXT("Error"), MB_OK); 

    // Free error-handling buffer allocations.

    LocalFree(lpMsgBuf);
    LocalFree(lpDisplayBuf);
}

--*/

