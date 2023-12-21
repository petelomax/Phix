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
--?{"write_to_pipe:",peek4u(pWr),length(x)}
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

-- routines and constants for demo\rosettacode\IPC_via_named_pipe (also as yet undocumented, and will remain so until they also work on linux)

global constant
-- dwPipeMode: CreateNamedPipe fails if dwOpenMode specifies anything other than 0 or the flags listed below.
    -- One of the following type modes can be specified. The same type mode must be specified for each instance of the pipe.
    PIPE_TYPE_BYTE          = 0x00000000,   -- Data is written to the pipe as a stream of bytes. This mode cannot be used with PIPE_READMODE_MESSAGE. 
                                            -- The pipe does not distinguish bytes written during different write operations. 
    PIPE_TYPE_MESSAGE       = 0x00000004,   -- Data is written to the pipe as a stream of messages. 
                                            -- The pipe treats the bytes written during each write operation as a message unit. 
                                            -- The GetLastError function returns ERROR_MORE_DATA when a message is not read completely. 
                                            -- This mode can be used with either PIPE_READMODE_MESSAGE or PIPE_READMODE_BYTE. 

    -- One of the following read modes can be specified. Different instances of the same pipe can specify different read modes.
    PIPE_READMODE_BYTE      = 0x00000000,   -- Data is read from the pipe as a stream of bytes. 
                                            -- This mode can be used with either PIPE_TYPE_MESSAGE or PIPE_TYPE_BYTE.
    PIPE_READMODE_MESSAGE   = 0x00000002,   -- Data is read from the pipe as a stream of messages. 
                                            -- This mode can be only used if PIPE_TYPE_MESSAGE is also specified.

    -- One of the following wait modes can be specified. Different instances of the same pipe can specify different wait modes.
    PIPE_WAIT               = 0x00000000,   -- Blocking mode is enabled. 
                                            -- When the pipe handle is specified in the ReadFile, WriteFile, or ConnectNamedPipe function, the operations 
                                            -- are not completed until there is data to read, all data is written, or a client is connected. 
                                            -- Use of this mode can mean waiting indefinitely in some situations for a client process to perform an action. 
    PIPE_NOWAIT             = 0x00000001,   -- Nonblocking mode is enabled. 
                                            -- In this mode, ReadFile, WriteFile, and ConnectNamedPipe always return immediately. 
                                            -- Note that nonblocking mode is supported for compatibility with Microsoft LAN Manager version 2.0 and should 
                                            -- not be used to achieve asynchronous I/O with named pipes. 
                                            -- For more information on asynchronous pipe I/O, see Synchronous and Overlapped Input and Output. 

    -- One of the following remote-client modes can be specified. Different instances of the same pipe can specify different remote-client modes.
    PIPE_ACCEPT_REMOTE_CLIENTS = 0x00000000,    -- Connections from remote clients can be accepted and checked against the security descriptor for the pipe.
                                                -- Windows Server 2003 and Windows XP/2000:  This flag is not supported.
    PIPE_REJECT_REMOTE_CLIENTS = 0x00000008,    -- Connections from remote clients are automatically rejected.
                                                -- Windows Server 2003 and Windows XP/2000:  This flag is not supported. 
                                                -- To achieve the same results, deny access to the pipe to the NETWORK ACE.

    PIPE_UNLIMITED_INSTANCES = 255
-- nMaxInstances: The maximum number of instances that can be created for this pipe. 
--                The first instance of the pipe can specify this value; the same number must be specified for other instances of the pipe. 
--                Acceptable values are in the range 1 through PIPE_UNLIMITED_INSTANCES (255). 
--                If this parameter is PIPE_UNLIMITED_INSTANCES, the number of pipe instances that can be created is limited only by system resources. 
--                If nMaxInstances>PIPE_UNLIMITED_INSTANCES, the return value is INVALID_HANDLE_VALUE and GetLastError returns ERROR_INVALID_PARAMETER. 
-- nOutBufferSize: The number of bytes to reserve for the output buffer. For a discussion on sizing named pipe buffers, see the following Remarks section.
-- nInBufferSize: The number of bytes to reserve for the input buffer. For a discussion on sizing named pipe buffers, see the following Remarks section.
-- nDefaultTimeOut: The default time-out value, in milliseconds, if the WaitNamedPipe function specifies NMPWAIT_USE_DEFAULT_WAIT. 
--                  Each instance of a named pipe must specify the same value. 
--                  A value of zero will result in a default time-out of 50 milliseconds.
-- lpSecurityAttributes: A pointer to a SECURITY_ATTRIBUTES structure that specifies a security descriptor for the new named pipe and determines whether 
--                       child processes can inherit the returned handle. If lpSecurityAttributes is NULL, the named pipe gets a default security descriptor 
--                       and the handle cannot be inherited. The ACLs in the default security descriptor for a named pipe grant full control to the 
--                       LocalSystem account, administrators, and the creator owner. They also grant read access to members of the Everyone group and the anonymous account. 
-- Return value: If the function succeeds, the return value is a handle to the server end of a named pipe instance.
--               If the function fails, the return value is INVALID_HANDLE_VALUE. To get extended error information, call GetLastError. 
-- Remarks: To create an instance of a named pipe by using CreateNamedPipe, the user must have FILE_CREATE_PIPE_INSTANCE access to the named pipe object. 
--          If a new named pipe is being created, the access control list (ACL) from the security attributes parameter defines the discretionary access control for the named pipe. 
--          
--  All instances of a named pipe must specify the same pipe type (byte-type or message-type), pipe access (duplex, inbound, or outbound), instance count, and time-out value. 
--  If different values are used, this function fails and GetLastError returns ERROR_ACCESS_DENIED. 
--
--  A client process connects to a named pipe by using the CreateFile or CallNamedPipe function. 
--  The client side of a named pipe starts out in byte mode, even if the server side is in message mode. 
--  To avoid problems receiving data, set the client side to message mode as well. 
--  To change the mode of the pipe, the pipe client must open a read-only pipe with GENERIC_READ and FILE_WRITE_ATTRIBUTES access. 
--
--  The pipe server should not perform a blocking read operation until the pipe client has started. 
--  Otherwise, a race condition can occur. 
--  This typically occurs when initialization code, such as the C run-time, needs to lock and examine inherited handles.
--
--  Every time a named pipe is created, the system creates the inbound and/or outbound buffers using nonpaged pool, which is the physical memory used by the kernel. 
--  The number of pipe instances (as well as objects such as threads and processes) that you can create is limited by the available nonpaged pool. 
--  Each read or write request requires space in the buffer for the read or write data, plus additional space for the internal data structures.
--
--  The input and output buffer sizes are advisory. The actual buffer size reserved for each end of the named pipe is either the system default, 
--  the system minimum or maximum, or the specified size rounded up to the next allocation boundary. The buffer size specified should be small enough 
--  that your process will not run out of nonpaged pool, but large enough to accommodate typical requests.
--
--  Whenever a pipe write operation occurs, the system first tries to charge the memory against the pipe write quota. 
--  If the remaining pipe write quota is enough to fulfill the request, the write operation completes immediately. 
--  If the remaining pipe write quota is too small to fulfill the request, the system will try to expand the buffers to accommodate the data using nonpaged pool reserved for the process. 
--  The write operation will block until the data is read from the pipe so that the additional buffer quota can be released. 
--  Therefore, if your specified buffer size is too small, the system will grow the buffer as needed, but the downside is that the operation will block. 
--  If the operation is overlapped, a system thread is blocked; otherwise, the application thread is blocked.
--
--  To free resources used by a named pipe, the application should always close handles when they are no longer needed, which is accomplished either by calling 
--  the CloseHandle function or when the process associated with the instance handles ends. 
--  Note that an instance of a named pipe may have more than one handle associated with it. 
--  An instance of a named pipe is always deleted when the last handle to the instance of the named pipe is closed. 

-- dwOpenMode: CreateNamedPipe fails if dwOpenMode specifies anything other than 0 or the flags listed below.
global constant
    -- dwOpenMode must specify one of the following pipe access modes. The same mode must be specified for each instance of the pipe.
    PIPE_ACCESS_DUPLEX  = 3,    -- The pipe is bi-directional; both server and client processes can read from and write to the pipe. 
                                -- This mode gives the server the equivalent of GENERIC_READ and GENERIC_WRITE access to the pipe. 
                                -- The client can specify GENERIC_READ or GENERIC_WRITE, or both, when it connects to the pipe using the CreateFile function. 
    PIPE_ACCESS_INBOUND = 1,    -- The flow of data in the pipe goes from client to server only. 
                                -- This mode gives the server the equivalent of GENERIC_READ access to the pipe. 
                                -- The client must specify GENERIC_WRITE access when connecting to the pipe. 
                                -- If the client must read pipe settings by calling the GetNamedPipeInfo or GetNamedPipeHandleState functions, 
                                -- the client must specify GENERIC_WRITE and FILE_READ_ATTRIBUTES access when connecting to the pipe. 
    PIPE_ACCESS_OUTBOUND = 2    -- The flow of data in the pipe goes from server to client only. 
                                -- This mode gives the server the equivalent of GENERIC_WRITE access to the pipe. 
                                -- The client must specify GENERIC_READ access when connecting to the pipe. 
                                -- If the client must change pipe settings by calling the SetNamedPipeHandleState function, 
                                -- the client must specify GENERIC_READ and FILE_WRITE_ATTRIBUTES access when connecting to the pipe. 

--/*
    -- dwOpenMode can also include one or more of the following flags, which enable the write-through and overlapped modes. 
    --            These modes can be different for different instances of the same pipe.
    FILE_FLAG_FIRST_PIPE_INSTANCE = 0x00080000, -- If you attempt to create multiple instances of a pipe with this flag, creation of the first 
                                                -- instance succeeds, but creation of the next instance fails with ERROR_ACCESS_DENIED.
                                                -- This flag is not supported until Windows 2000 SP2 and Windows XP.
    FILE_FLAG_WRITE_THROUGH       = 0x80000000  -- Write-through mode is enabled. This mode affects only write operations on byte-type pipes and, 
                                                -- then, only when the client and server processes are on different computers. 
                                                -- If this mode is enabled, functions writing to a named pipe do not return until the data written 
                                                -- is transmitted across the network and is in the pipe's buffer on the remote computer. 
                                                -- If this mode is not enabled, the system enhances the efficiency of network operations by buffering 
                                                -- data until a minimum number of bytes accumulate or until a maximum time elapses.
--*/

constant
    FILE_FLAG_OVERLAPPED          = 0x40000000  -- Overlapped mode is enabled. If this mode is enabled, functions performing read, write, and connect 
                                                -- operations that may take a significant time to be completed can return immediately. 
                                                -- This mode enables the thread that started the operation to perform other operations while the 
                                                -- time-consuming operation executes in the background. For example, in overlapped mode, a thread can 
                                                -- handle simultaneous input and output (I/O) operations on multiple instances of a pipe or perform 
                                                -- simultaneous read and write operations on the same pipe handle. 
                                                -- If overlapped mode is not enabled, functions performing read, write, and connect operations on the 
                                                -- pipe handle do not return until the operation is finished. The ReadFileEx and WriteFileEx functions 
                                                -- can only be used with a pipe handle in overlapped mode. The ReadFile, WriteFile, ConnectNamedPipe, 
                                                -- and TransactNamedPipe functions can execute either synchronously or as overlapped operations. 

    -- dwOpenMode can also include any combination of the following security access modes.
    --            These modes can be different for different instances of the same pipe.
--  WRITE_DAC               = 0x00040000,   -- The caller will have write access to the named pipe's discretionary access control list (ACL).
--  WRITE_OWNER             = 0x00080000,   -- The caller will have write access to the named pipe's owner.
--  ACCESS_SYSTEM_SECURITY  = 0x01000000,   -- The caller will have write access to the named pipe's SACL.
                                            -- For more information, see Access-Control Lists (ACLs) and SACL Access Right. 

--???
--#withtype bool

constant plat = platform()
--constant plat = LINUX

--DEV/SUG:
function define_win_cffi_func(string lib, string cdef)
--  if platform()=WINDOWS then
    if plat=WINDOWS then
        return define_cffi_func(lib,cdef)
    end if
    return 0
end function
set_unicode(0) -- use CreateNamedPipeA etc

-- Creates an instance of a named pipe and returns a handle for subsequent pipe operations. 
-- A named pipe server process uses this function either to create the first instance of a specific named pipe 
-- and establish its basic attributes or to create a new instance of an existing named pipe.
constant tCreateNamedPipe = """
HANDLE WINAPI CreateNamedPipe(
  _In_      LPCTSTR lpName,
  _In_      DWORD dwOpenMode,
  _In_      DWORD dwPipeMode,
  _In_      DWORD nMaxInstances,
  _In_      DWORD nOutBufferSize,
  _In_      DWORD nInBufferSize,
  _In_      DWORD nDefaultTimeOut,
  _In_opt_  LPSECURITY_ATTRIBUTES lpSecurityAttributes
);"""

constant xCreateNamedPipe = define_win_cffi_func("kernel32.dll",tCreateNamedPipe)

procedure validate_pipename(string szPipename)
    if length(szPipename)>256 then ?9/0 end if
    if length(szPipename)<10 then ?9/0 end if
    if szPipename[1..9]!=`\\.\pipe\` then ?9/0 end if
    if find('\\',szPipename,10)!=0 then ?9/0 end if
end procedure

global function create_named_pipe(string szPipename, atom dwOpenMode, integer dwPipeMode,
                                  integer nMaxInstances=PIPE_UNLIMITED_INSTANCES,
                                  integer nOutBufferSize=0, integer nInBufferSize=0,
                                  integer nDefaultTimeOut=1000, atom lpSecurityAttributes=NULL)
--
-- for now, FILE_FLAG_OVERLAPPED is not supported - let me know if you need it.
--  ( I plan to use threads with blocking calls for all this stuff anyway )
--
    validate_pipename(szPipename)
    -- dwOpenMode must specify FILE_FLAG_(DUPLEX|INBOUND|OUTBOUND):
    --      (erm, I think I mean PIPE_ACCESS_DUPLEX etc)
    if and_bits(dwOpenMode,#3)=0 then ?9/0 end if
    -- (test and) remove this only if someone actually needs it:
    if and_bits(dwOpenMode,FILE_FLAG_OVERLAPPED) then ?9/0 end if   -- not tested [DEV]
    if and_bits(dwPipeMode,#F)!=dwPipeMode then ?9/0 end if
    -- nMaxInstances must be 0..255(255=PIPE_UNLIMITED_INSTANCES):
    if nMaxInstances<=0 then ?9/0 end if
    if nMaxInstances>PIPE_UNLIMITED_INSTANCES then ?9/0 end if
    -- and we may as well verify that sizes/timeout are not negative:
    if nOutBufferSize<0 then ?9/0 end if
    if nInBufferSize<0 then ?9/0 end if
    if nDefaultTimeOut<0 then ?9/0 end if
    -- (ps: I fully expect the compiler to optimise away most of the above)
    atom hPipe = c_func(xCreateNamedPipe,{szPipename, dwOpenMode, dwPipeMode,
                                          nMaxInstances, nOutBufferSize, nInBufferSize,
                                          nDefaultTimeOut, lpSecurityAttributes})
    return hPipe
end function

constant tGetLastError = """
DWORD WINAPI GetLastError(void);"""
constant xGetLastError = define_win_cffi_func("kernel32.dll",tGetLastError)

constant tConnectNamedPipe = """
BOOL WINAPI ConnectNamedPipe(
  _In_         HANDLE hNamedPipe,
  _Inout_opt_  LPOVERLAPPED lpOverlapped
);"""
constant xConnectNamedPipe = define_win_cffi_func("kernel32.dll",tConnectNamedPipe)

constant ERROR_PIPE_CONNECTED = 535

--function connect_named_pipe(atom hPipe, atom pOverlapped=NULL)
global function connect_named_pipe(atom hPipe)
-- (FILE_FLAG_OVERLAPPED not tested/permitted yet)
--  bool res = c_func(xConnectNamedPipe,{szPipename,pOverlapped})
    bool res = c_func(xConnectNamedPipe,{hPipe,NULL})
    if res=0 and c_func(xGetLastError,{})=ERROR_PIPE_CONNECTED then
        res = 1
    end if
    return res
end function

constant tCloseHandle = """
BOOL WINAPI CloseHandle(
  _In_  HANDLE hObject
);"""
constant xCloseHandle = define_win_cffi_func("kernel32.dll",tCloseHandle)

global procedure close_handle(atom hPipe)
    if not c_func(xCloseHandle,{hPipe}) then ?9/0 end if
end procedure

constant tDisconnectNamedPipe = """
BOOL WINAPI DisconnectNamedPipe(
  _In_  HANDLE hNamedPipe
);"""
constant xDisconnectNamedPipe = define_win_cffi_func("kernel32.dll",tDisconnectNamedPipe)

global procedure disconnect_pipe(atom hPipe)
    if not c_func(xDisconnectNamedPipe,{hPipe}) then ?9/0 end if
end procedure   

constant tFlushFileBuffers = """
BOOL WINAPI FlushFileBuffers(
  _In_  HANDLE hFile
);"""
constant xFlushFileBuffers = define_win_cffi_func("kernel32.dll",tFlushFileBuffers)

global procedure flush_pipe(atom hPipe)
    if not c_func(xFlushFileBuffers,{hPipe}) then ?9/0 end if
end procedure       

constant tReadFile = """
BOOL WINAPI ReadFile(
  _In_         HANDLE hFile,
  _Out_        LPVOID lpBuffer,
  _In_         DWORD nNumberOfBytesToRead,
  _Out_opt_    LPDWORD lpNumberOfBytesRead,
  _Inout_opt_  LPOVERLAPPED lpOverlapped
);"""
constant xReadFile = define_win_cffi_func("kernel32.dll",tReadFile)

global function read_pipe(atom hPipe, atom pBuffer, integer dwSize, atom pBytesRead)
    bool bRes = c_func(xReadFile,{hPipe,pBuffer,dwSize,pBytesRead,NULL})
    return bRes
end function

constant tWriteFile = """
BOOL WINAPI WriteFile(
  _In_         HANDLE hFile,
  _In_         LPCVOID lpBuffer,
  _In_         DWORD nNumberOfBytesToWrite,
  _Out_opt_    LPDWORD lpNumberOfBytesWritten,
  _Inout_opt_  LPOVERLAPPED lpOverlapped
);"""
constant xWriteFile = define_win_cffi_func("kernel32.dll",tWriteFile)

--function write_pipe(atom hPipe, atom pBuffer, integer dwSize, atom pBytesWritten)
global function write_pipe(atom hPipe, string msg, atom pBytesWritten)
--  bool bRes = c_func(xWriteFile,{hPipe,pBuffer,dwSize,pBytesWritten,NULL})
    bool bRes = c_func(xWriteFile,{hPipe,msg,length(msg),pBytesWritten,NULL})
    return bRes
end function

constant tCallNamedPipe = """
BOOL WINAPI CallNamedPipe(
  _In_   LPCTSTR lpNamedPipeName,
  _In_   LPVOID lpInBuffer,
  _In_   DWORD nInBufferSize,
  _Out_  LPVOID lpOutBuffer,
  _In_   DWORD nOutBufferSize,
  _Out_  LPDWORD lpBytesRead,
  _In_   DWORD nTimeOut
);"""
constant xCallNamedPipe = define_win_cffi_func("kernel32.dll",tCallNamedPipe)

global constant NMPWAIT_NOWAIT           = 0x00000001,  -- Does not wait for the named pipe. If the named pipe is not available, the function returns an error.
                NMPWAIT_WAIT_FOREVER     = 0xffffffff,  -- Waits indefinitely.
                NMPWAIT_USE_DEFAULT_WAIT = 0x00000000   -- Uses the default time-out specified in a call to the CreateNamedPipe function. 

global function call_named_pipe(string szPipename, string msg)
    validate_pipename(szPipename)
    atom pBuffer = allocate(1024)
    atom pBytesRead = allocate(4)
    integer lm = length(msg)
    bool bRes = c_func(xCallNamedPipe,{szPipename,msg,length(msg),pBuffer,1024,pBytesRead,NMPWAIT_USE_DEFAULT_WAIT})
    string res = ""
    if bRes then
        integer bytesread = peek4s(pBytesRead)
        res = peek({pBuffer,bytesread})
    end if
    free(pBuffer)
    free(pBytesRead)
    return res
end function

-- <end of builtins\pipeio.e>

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
