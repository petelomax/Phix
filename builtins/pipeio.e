--
-- builtins\pipeio.e
--
--  see demo\capture_console.exw for an example of use
--  DEV: not quite right on linux: doubles-up on output, hangs on eof (fudge below [find '!'])
--
include builtins\ptypes.e
include builtins\syswait.ew
constant BUFSIZE = 4096
 
include cffi.e
constant 
tSA = """
typedef struct _SECURITY_ATTRIBUTES {
  DWORD  nLength;
  LPVOID lpSecurityDescriptor;
  BOOL   bInheritHandle;
} SECURITY_ATTRIBUTES, *PSECURITY_ATTRIBUTES, *LPSECURITY_ATTRIBUTES;
"""
bool p_init = false
atom kernel32, pSA, pRd, pWr, pAvail, chBuf = NULL,
     libc, xerrno
integer xGetExitCodeProcess, xWaitForSingleObject, 
        xRead, xWrite,
        xPipe, xPeekNamedPipe,
        xSetHandleInformation, xClose, 
--      xGetLastError,
        idSA
--      , xWaitpid
constant HANDLE_FLAG_INHERIT = 0x00000001,
         FORTYMS = 40,           -- Forty milliseconds, 1/25th of a second
         STILL_ACTIVE = 259,
         O_NONBLOCK = 0x0004 -- no delay

global enum PIPEIN, PIPOUT, PIPERR
global enum READ_PIPE, WRITE_PIPE

procedure initpi()
    enter_cs()
    if platform()=WINDOWS then
        kernel32 = open_dll("kernel32.dll")
--#without reformat
        xWaitForSingleObject = define_c_func(kernel32, "WaitForSingleObject",
            {C_PTR,     --  HANDLE hObject, // handle of object to wait for
             C_PTR},    --  DWORD   dwTimeout // time-out interval in milliseconds
            C_DWORD)    -- DWORD -- WAIT_ABANDONED, WAIT_OBJECT_0, or WAIT_TIMEOUT
        xGetExitCodeProcess = define_c_func(kernel32, "GetExitCodeProcess",
            {C_PTR,     --  HANDLE hProcess, // handle to the process
             C_PTR},    --  LPDWORD  lpExitCode // address to receive termination status
            C_BOOL)     -- BOOL
        xRead = define_c_func(kernel32,"ReadFile",
            {C_PTR,     --  HANDLE  hFile,  // handle of file to read
             C_PTR,     --  LPVOID  lpBuffer,   // address of buffer that receives data
             C_LONG,    --  DWORD  nNumberOfBytesToRead,    // number of bytes to read
             C_PTR,     --  LPDWORD  lpNumberOfBytesRead,   // address of number of bytes read
             C_PTR},    --  LPOVERLAPPED  lpOverlapped  // address of structure for data
            C_INT)      -- BOOL
        xWrite = define_c_func(kernel32,"WriteFile",
            {C_PTR,     --  HANDLE  hFile,  // handle of file to write to
             C_PTR,     --  LPCVOID  lpBuffer,  // address of data to write to file
             C_LONG,    --  DWORD  nNumberOfBytesToWrite,   // number of bytes to write
             C_PTR,     --  LPDWORD  lpNumberOfBytesWritten,    // address of number of bytes written
             C_PTR},    --  LPOVERLAPPED  lpOverlapped  // addr. of structure needed for overlapped I/O
            C_INT)      -- BOOL
        xPipe = define_c_func(kernel32,"CreatePipe",
            {C_PTR,     --  _Out_     PHANDLE hReadPipe
             C_PTR,     --  _Out_     PHANDLE hWritePipe
             C_PTR,     --  _In_opt_  LPSECURITY_ATTRIBUTES lpPipeAttributes
             C_DWORD},  --  _In_      DWORD nSize
            C_BOOL)     -- BOOL
        xPeekNamedPipe = define_c_func(kernel32,"PeekNamedPipe",
            {C_PTR,     --    _In_       HANDLE hNamedPipe,
             C_PTR,     --    _Out_opt_  LPVOID lpBuffer,
             C_DWORD,   --    _In_       DWORD nBufferSize,
             C_PTR,     --    _Out_opt_  LPDWORD lpBytesRead,
             C_PTR,     --    _Out_opt_  LPDWORD lpTotalBytesAvail,
             C_PTR},    --    _Out_opt_  LPDWORD lpBytesLeftThisMessage
            C_BOOL)     -- BOOL
        xSetHandleInformation = define_c_func(kernel32,"SetHandleInformation",
            {C_PTR,     --  _In_  HANDLE hObject,
             C_DWORD,   --  _In_  DWORD dwMask,
             C_DWORD},  --  _In_  DWORD dwFlags
            C_BOOL)     -- BOOL
        xClose = define_c_func(kernel32, "CloseHandle",
            {C_PTR},    --  HANDLE  hObject // handle of object to close
            C_BOOL)     -- BOOL
--      xGetLastError = define_c_func(kernel32, "GetLastError",
--          {},
--          C_DWORD)    -- DWORD
--#with reformat
        idSA = define_struct(tSA)
        pSA = allocate_struct(idSA)
        -- Set the bInheritHandle flag so pipe handles are inherited. 
        set_struct_field(idSA,pSA,"nLength",get_struct_size(idSA))
        set_struct_field(idSA,pSA,"bInheritHandle",true)
        set_struct_field(idSA,pSA,"lpSecurityDescriptor",NULL)
    else -- LINUX
        libc = open_dll({"libc.so", "libc.dylib", ""})
--      xPipe = define_c_func(libc, "pipe",
--                  {C_PTR},    --  int filedes[2]
--                  C_INT)      -- int
        xPipe = define_c_func(libc, "pipe2",
                    {C_PTR,     --  int filedes[2]
                     C_INT},    --  int flags
                    C_INT)      -- int
        xClose = define_c_func(libc, "close",
                    {C_INT},    --  int filedes
                    C_INT)      -- int
        xRead = define_c_func(libc, "read",
                    {C_INT,     --  int filedes
                     C_PTR,     --  void* buffer
                     C_INT},    --  size_t size
                    C_LONG)     -- ssize_t
        xWrite = define_c_func(libc, "write",
                    {C_INT,     --  int filedes
                     C_PTR,     --  void* buffer
                     C_INT},    --  size_t size
                    C_LONG)     -- ssize_t
--      xWaitpid = define_c_func(libc, "waitpid",
--                  {C_INT,     --  pid_t pid
--                   C_PTR,     --  int* status_ptr
--                   C_INT},    --  int options
--                  C_INT)      -- pid_t
        xerrno = define_c_var(libc, "errno")
    end if
    pRd = allocate(machine_word()*2)
    pWr = pRd+machine_word()
    pAvail = allocate(4) -- (it's a dword)
    leave_cs()
    p_init = 1
end procedure

global function close_handles(object p)
    if sequence(p) then
        for i=1 to length(p) do
            p[i] = close_handles(p[i])
        end for
    elsif p!=NULL then
        if platform()=WINDOWS then
            if not c_func(xClose,{p}) then ?9/0 end if
        else
            if c_func(xClose,{p})!=0 then ?9/0 end if
        end if
        p = NULL
    end if
    return p
end function

global function write_to_pipe(atom hPipe, atom_string x)
    if atom(x) then
        if x=-1 then ?9/0 end if -- (assume it's a programming error)
        x = and_bits(x,#FF)&""
    end if
    integer l = length(x)
    bool bSuccess = iff(platform()=WINDOWS?c_func(xWrite,{hPipe, x, l, pWr, NULL})
                                 /*LINUX*/:c_func(xWrite,{hPipe, x, l})!=-1)
    return bSuccess
end function

global function read_from_pipe(atom hPipe, hProc) 
    if chBuf=NULL then chBuf = allocate(BUFSIZE,true) end if
    integer dwRead
    if platform()=WINDOWS then
        while true do
            {} = c_func(xPeekNamedPipe,{hPipe,NULL,0,NULL,pAvail,NULL})
            if peek4u(pAvail)!=0 then exit end if -- (available bytes non-0)
            if not c_func(xGetExitCodeProcess, {hProc, pWr})
            or peek4u(pWr)!=STILL_ACTIVE then
                return -1
            end if
            {} = c_func(xWaitForSingleObject, {hProc, FORTYMS})
        end while
        bool bSuccess = c_func(xRead,{hPipe, chBuf, BUFSIZE, pRd, NULL})
        if not bSuccess then return -1 end if
        dwRead = peek4u(pRd)
    else -- LINUX
--/*
--tryme:
void read_from_pipe (int file)
{
  FILE *stream;
  int c;
  stream = fdopen (file, "r");
  while ((c = fgetc (stream)) != EOF)
    putchar (c);
  fclose (stream);
}
FILE * fdopen (int filedes, const char *opentype)
--*/
--DEV this blocks, need equivalent to the above[?]
--?"xRead"
        dwRead = c_func(xRead,{hPipe,chBuf,BUFSIZE})
--?dwRead
--/*
--tryme??: (not sure it will help with the hang in read on linux)
    if platform()=LINUX then
        -- This is the parent process.  Wait for the child to complete.
--      void = waitpid(pid, &status, 0)
        {} = waitpid(hProc, &status, 0)
--ah!, and this...
        flags = fcntl(pfd[0], F_GETFL);
        flags |= O_NONBLOCK;
        if (fcntl(pfd[0], F_SETFL, flags)) perror("fcntl");
F_GETFL=        3 ; get file status flags
F_SETFL=        4 ; set file status flags
O_NONBLOCK=     0x0004 ; no delay
    end if
--*/
--goto
        if dwRead=-1 then ?9/0 end if
--      if dwRead=0 then exit end if
    end if
    if dwRead==0 then return -1 end if
    string buff = peek({chBuf,dwRead})
    return buff
--      puts(1,upper(buff))
--      puts(1,lower(buff))
--      puts(1,buff)
end function

--procedure set_handle_information(atom hObject, dwMask, dwFlags)
--  if platform()!=WINDOWS then ?9/0 end if
--  if not c_func(xSetHandleInformation,{hObject, dwMask, dwFlags}) then ?9/0 end if
--end procedure
 
global constant INHERIT_READ = 1, INHERIT_WRITE = 2

global function create_pipe(integer inherit=0)
--  atom hRd, hWr
    sequence res
    if not p_init then initpi() end if
    if platform()=WINDOWS then
        if not c_func(xPipe,{pRd, pWr, pSA, 0}) then ?9/0 end if
--      hRd = peekNS(pRd,machine_word(),false)
--      hWr = peekNS(pWr,machine_word(),false)
        res = peekNS({pRd,2},machine_word(),false)
        if inherit then
--          set_handle_information(res[inherit], HANDLE_FLAG_INHERIT, 0)            
            bool shok = c_func(xSetHandleInformation,{res[inherit], HANDLE_FLAG_INHERIT, 0})
            if shok==0 then ?9/0 end if
        end if
    else -- LINUX
--      if c_func(xPipe,{pRd})=-1 then ?9/0 end if
        if c_func(xPipe,{pRd,O_NONBLOCK})=-1 then
            atom error = peek4s(xerrno)
            ?{"error",error}
            ?9/0
        end if
--      hRd = peek4u(pRd)
--      hWr = peek4u(pRd+4) -- (as tested/needed on 64 bit!)
        res = peek4u({pRd,2})
    end if
--  return {hRd,hWr}
    return res
end function
-- <end of builtins/pipeio.e>

--/* This also works on linux:
--(DEV: surely there must be a way to make pmain.e do most of this (esp 32 vs 64 bit) for me!!)
-- eg res = #iffi{"libc.so.6","popen",cmd,mode}
--  (note that args **MUST** be integer/string/atom; sequence/object/udt are banned)
--                           [ie: as_is/shl 2/%pLoadMint respectively]
--   [res is always %pStoreMint'd, and must be atom/integer (only)]
function popen(string cmd, mode)
-- FILE * popen (const char *command, const char *mode)
    integer res
    #ilASM{
        [ELF32]
            mov eax,[cmd]
            mov ecx,[mode]
            shl eax,2
            shl ecx,2
            push ecx
            push eax
            call "libc.so.6","popen"
            add esp,8
            lea edi,[res]
            call :%pStoreMint
        [ELF64]
            mov rdi,[cmd]
            mov rsi,[mode]
            shl rdi,2
            shl rsi,2
            call "libc.so.6","popen"
            lea rdi,[res]
            call :%pStoreMint
        []
          }
    return res
end function
function fread(atom buffer, integer size, count, stream)
-- size_t fread (void *data, size_t size, size_t count, FILE *stream)
    integer res
    #ilASM{
        [ELF32]
            mov eax,[buffer]
            call :%pLoadMint
            mov ecx,[size]
            mov edx,[count]
            mov esi,[stream]
            push esi
            push edx
            push ecx
            push eax
            call "libc.so.6","fread"
            add esp,16
            lea edi,[res]
            call :%pStoreMint
        [ELF64]
            mov rdi,[buffer]
            call :%pLoadMint
            mov rsi,[size]
            mov rdx,[count]
            mov rcx,[stream]
            call "libc.so.6","fread"
            lea rdi,[res]
            call :%pStoreMint
        []
          }
    return res
end function
function pclose(integer fd)
--int pclose (FILE *stream)
    integer res
    #ilASM{
        [ELF32]
            mov eax,[fd]
            push eax
            call "libc.so.6","pclose"
            add esp,4
            lea edi,[res]
            call :%pStoreMint
        [ELF64]
            mov rdi,[fd]
            call "libc.so.6","pclose"
            lea rdi,[res]
            call :%pStoreMint
        []
          }
    return res
end function
    if platform()=LINUX then
        -- FILE *fd = popen("ls", "r");
        integer fd = popen("ls", "r")
        if fd=NULL then ?9/0 end if
 
        atom buffer = allocate(256)
        string comout = ""
 
        /* Use fread so binary data is dealt with correctly */
        while true do
            integer chread = fread(buffer, 1, 256, fd)
            if chread=0 then exit end if
            comout &= peek({buffer,chread})
        end while
 
        /* We can now work with the output as we please. Just print
         * out to confirm output is as expected */
--      puts(1,upper(comout))
        puts(1,comout)
        {} = pclose(fd)
    end if
puts(1,"done!\n")
{} = wait_key()
--*/
