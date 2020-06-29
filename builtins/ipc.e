--
-- ipc.e
-- =====
--  Interprocess communication for Phix
--
--  Based heavily on the work of Jason Mirwald and Mario Steele (Windows), and Elliott Sales de Andrade (Linux),
--  this file (builtins\ipc.e) merges the two ("memshare") code bases into a single cross-platform file.
--
--  Note: there are (as yet) no synchronisation primitives, for now I suggest using file locking. As it stands,
--        without said, this is only suitable for one-way, 1:1, and non-critical uses, such as:
--          compiler sending error and warning messages to Edix
--          pdemo sending edit requests to Edix
--          p -isense signalling intellisense info ready to read in /tmp
--          implementation of single instance on Linux (may supercede the Windows-only SINGLEINSTANCE of IUP)
--          (ie in all the above, the worst case scenario is only going to be "try clicking it again")
--      [I am assuming that single instance would fail not by creating two instances but when two overlapping 
--       communications mean that eg "C:\UC:\Users\Me\myfile.exwsers\Me\myfile.exw" cannot be opened.]
--
--  It is also reasonable that, slowly, over time, this is extended with implementions of Message Queues,
--  Named Pipes, and Sockets - there is a reason why there are half a dozen wildly different approaches to
--  ipc, which is that "there is no one size fits all". Progress will most likely be rather leisurely, not
--  least because any and all Windows-only or Linux-only solutions are less than worthless.
--

--DEV monitor/test this with the ipcs command on Linux..
-- also see http://beej.us/guide/bgipc/output/html/singlepage/bgipc.html
--  (esp semaphores [would need a separate Windows imlementation as well])

global constant
    SM_CREATE_EXIST = -1,
    SM_CREATE_FAIL = -2,
    SM_OPEN_FAIL = -3,
    SM_MEM_FAIL = -4,
    SM_STAT_FAIL = -5

atom lib = -1,  -- libc/kernel32
     xshmctl, xshmget, xshmat, xshmdt, xerrno,  -- Linux
     xCreateFileMapping, xOpenFileMapping,      -- Windows
     xMapViewOfFile, xUnmapViewOfFile,
     xCloseHandle, xGetLastError

sequence smNames,
         smHandles,
         smPointers

procedure ipc_init()

    if platform()=LINUX then

--      lib = open_dll("libc.so.6")
        lib = open_dll("libc.so")
        xshmctl = define_c_func(lib, "shmctl",
                                {C_INT,     --  int shmid
                                 C_INT,     --  int cmd
                                 C_PTR},    --  struct shmid_ds *buf
                                C_INT)      -- int
        xshmget = define_c_func(lib, "shmget",
                                {C_INT,     --  key_t key
                                 C_UINT,    --  size_t size
                                 C_INT},    --  int shmflg
                                C_INT)      -- int
        xshmat = define_c_func(lib, "shmat",
                               {C_INT,      --  int shmid
                                C_PTR,      --  const void *shmaddr
                                C_INT},     --  int shmflg
                               C_PTR)       -- void *
        xshmdt = define_c_func(lib, "shmdt",
                               {C_PTR},     --  const void *shmaddr
                               C_INT)       -- int
        xerrno = define_c_var(lib, "errno")

    elsif platform()=WINDOWS then

        lib = open_dll("kernel32")
        xCreateFileMapping = define_c_func(lib, "CreateFileMappingA",
                                           {C_PTR,      --  HANDLE hFile
                                            C_PTR,      --  LPSECURITY_ATTRIBUTES lpAttributes
                                            C_ULONG,    --  DWORD flProtect
                                            C_ULONG,    --  DWORD dwMaximumSizeHigh
                                            C_ULONG,    --  DWORD dwMaximumSizeLow
                                            C_PTR},     --  LPCTSTR lpName
                                           C_PTR)       -- HANDLE
        xOpenFileMapping = define_c_func(lib, "OpenFileMappingA",
                                         {C_ULONG,      --  DWORD dwDesiredAccess
                                          C_INT,        --  BOOL bInheritHandle
                                          C_PTR},       --  LPCTSTR lpName
                                         C_PTR)         -- HANDLE
        xMapViewOfFile = define_c_func(lib, "MapViewOfFile",
                                       {C_PTR,          --  HANDLE hFileMappingObject
                                        C_ULONG,        --  DWORD dwDesiredAccess
                                        C_ULONG,        --  DWORD dwFileOffsetHigh
                                        C_ULONG,        --  DWORD dwFileOffsetLow
                                        C_ULONG},       --  SIZE_T dwNumberOfBytesToMap
                                       C_PTR)           -- LPVOID
        xUnmapViewOfFile = define_c_func(lib, "UnmapViewOfFile",
                                         {C_PTR},       --  LPCVOID lpBaseAddress
                                         C_INT)         -- BOOL
        xCloseHandle = define_c_func(lib, "CloseHandle",
                                     {C_PTR},           --  HANDLE hObject
                                     C_INT)             -- BOOL
        xGetLastError = define_c_func(lib, "GetLastError",
                                      {},
                                      C_ULONG)      -- DWORD
    end if
    smNames = {}
    smHandles = {}
    smPointers = {}
end procedure

-- Linux constants
-- ===============

--#define SHM_RDONLY    #1000   /* read-only access */
--#define SHM_RND       #2000   /* round attach address to SHMLBA boundary */
--#define SHM_REMAP     #4000   /* take-over region on attach */
--#define SHM_EXEC      #8000   /* execution access */

constant IPC_CREAT = #200       -- create if key is nonexistent
constant IPC_EXCL  = #400       -- fail if key exists
--#define IPC_NOWAIT #800       /* return error on wait */
--#define SHM_HUGETLB #4000     /* segment will use huge TLB pages */
--#define SHM_NORESERVE #1000   /* don't check for reservations */
constant RWRWRW = 0o666

constant IPC_RMID = 0      -- remove resource
constant IPC_STAT = 2      -- get ipc_perm options

--#define EPERM          1      /* Operation not permitted */
--constant ENOENT      = 2      -- No such file or directory
--#define ESRCH          3      /* No such process */
--#define EINTR          4      /* Interrupted system call */
--#define EIO            5      /* I/O error */
--#define ENXIO          6      /* No such device or address */
--#define E2BIG          7      /* Argument list too long */
--#define ENOEXEC        8      /* Exec format error */
--#define EBADF          9      /* Bad file number */
--#define ECHILD        10      /* No child processes */
--#define EAGAIN        11      /* Try again */
constant ENOMEM       = 12      -- Out of memory
--#define EACCES        13      /* Permission denied */
--#define EFAULT        14      /* Bad address */
--#define ENOTBLK       15      /* Block device required */
--#define EBUSY         16      /* Device or resource busy */
constant EEXIST       = 17      -- File exists
--#define EXDEV         18      /* Cross-device link */
--#define ENODEV        19      /* No such device */
--#define ENOTDIR       20      /* Not a directory */
--#define EISDIR        21      /* Is a directory */
--#define EINVAL        22      /* Invalid argument */
--#define ENFILE        23      /* File table overflow */
--#define EMFILE        24      /* Too many open files */
--#define ENOTTY        25      /* Not a typewriter */
--#define ETXTBSY       26      /* Text file busy */
--#define EFBIG         27      /* File too large */
--#define ENOSPC        28      /* No space left on device */
--#define ESPIPE        29      /* Illegal seek */
--#define EROFS         30      /* Read-only file system */
--#define EMLINK        31      /* Too many links */
--#define EPIPE         32      /* Broken pipe */
--#define EDOM          33      /* Math argument out of domain of func */
--#define ERANGE        34      /* Math result not representable */

function sm_error(integer default_error)    -- (Linux only)
atom error = peek4s(xerrno)
    if error=ENOMEM then
        return SM_MEM_FAIL
    elsif error=EEXIST then
        return SM_CREATE_EXIST
    end if
    return default_error
end function

function getAttached(atom shmid)            -- (Linux only)
atom pshmid_ds, ctlres
integer shm_nattch  -- No. of current attaches in struct shmid_ds (result)
    pshmid_ds = allocate(80)    --DEV size??
    mem_set(pshmid_ds, 0, 80)
    ctlres = c_func(xshmctl, {shmid,IPC_STAT,pshmid_ds})
    if ctlres!=0 then return sm_error(SM_STAT_FAIL) end if
--DEV this wants testing on 64-bit lnx!
    shm_nattch = peek(pshmid_ds+72)+peek(pshmid_ds+73)*#100
    free(pshmid_ds)
    return shm_nattch
end function

function getHash(sequence strHash)          -- (Linux only)
sequence str, lrc
atom hash
integer modulo
    hash = 0
    modulo = 0
    lrc = repeat(0, 8)
    str = "Linux MemShare"
    if length(strHash)>length(str) then
        for i=1 to floor(length(strHash)/length(str)) do
            str &= "Linux MemShare"
        end for
    end if
    for i=1 to length(strHash) do
        hash = or_bits(hash+i, strHash[i]*str[i]*i)
        modulo += strHash[i]
        lrc = sq_add(lrc,int_to_bits(strHash[i], 8))
    end for
    hash = and_bits(hash, #7FFF)*#10000
    hash += remainder(modulo, 256)*#100
    hash += bits_to_int(lrc)
    return hash
end function

-- Windows constants
-- =================
constant PAGE_READWRITE = #4
constant FILE_MAP_WRITE = 2
constant ERROR_ALREADY_EXISTS = 183

-------------------------------------------------

global function sm_create(string name, integer nbytes)
atom szlPointer,
     key, shmsize, shmflg, shmid,   -- (Linux only)
     wHandle, lasterror             -- (Windows only)

    if lib=-1 then ipc_init() end if
    integer k = find(name, smNames)
    if k!=0 then
        return smPointers[k]
    end if
    if platform()=LINUX then
        key = getHash(name)
        shmsize = nbytes
        shmflg = IPC_CREAT+IPC_EXCL+RWRWRW
        shmid = c_func(xshmget, {key,shmsize,shmflg}) -- get id
        if shmid= -1 then return sm_error(SM_CREATE_FAIL) end if

        shmflg = 0  -- (not SMH_RDONLY)
        szlPointer = c_func(xshmat, {shmid,NULL,shmflg})    -- attach
        if szlPointer= -1 then return sm_error(SM_CREATE_FAIL) end if

        smHandles = append(smHandles,shmid)
    elsif platform()=WINDOWS then
        wHandle = c_func(xCreateFileMapping, {-1,               -- hFile
                                              NULL,             -- lpSecurityAttributes
                                              PAGE_READWRITE,   -- flProtect
                                              0,                -- dwMaximumSizeHigh
                                              nbytes,           -- dwMaximumSizeLow
                                              name})            -- lpName
        if wHandle!=NULL then
            szlPointer = c_func(xMapViewOfFile,{wHandle,        -- hFileMappingObject
                                                FILE_MAP_WRITE, -- dwDesiredAccess
                                                0,              -- dwFileOffsetHigh
                                                0,              -- dwFileOffsetLow
                                                0})             -- dwNumberOfBytesToMap
        end if
        if wHandle=NULL
        or szlPointer=NULL then
            lasterror = c_func(xGetLastError, {})
            if wHandle!=NULL then
                {} = c_func(xCloseHandle, {wHandle})
            end if
            if lasterror=ERROR_ALREADY_EXISTS then
                return SM_CREATE_EXIST
            end if
            return SM_CREATE_FAIL
        end if
        smHandles = append(smHandles,wHandle)
    end if
    smNames = append(smNames,name)
    smPointers = append(smPointers,szlPointer)
    return szlPointer
end function

-------------------------------------------------

global function sm_open(string name)
atom szlPointer,
     key, shmid, shmsize, shmflg,   -- (Linux only)
     wHandle                    -- (Windows only)

    if lib=-1 then ipc_init() end if
    integer k = find(name, smNames)
    if k!=0 then
        return smPointers[k]
    end if
    if platform()=LINUX then
        key = getHash(name)
        shmsize = 0 -- (not needed w/o IPC_CREAT)
        shmflg = 0 -- (not IPC_CREAT)
        shmid = c_func(xshmget, {key,shmsize,shmflg}) -- get id
        if shmid= -1 then return sm_error(SM_OPEN_FAIL) end if

        shmflg = 0 -- (not SMH_RDONLY)
        szlPointer = c_func(xshmat, {shmid,NULL,shmflg}) -- attach
        if szlPointer= -1 then return sm_error(SM_OPEN_FAIL) end if

        smHandles = append(smHandles,shmid)
    elsif platform()=WINDOWS then
        wHandle = c_func(xOpenFileMapping,{FILE_MAP_WRITE,  -- dwDesiredAccess
                                           False,           -- bInheritHandle
                                           name})           -- lpName
        if wHandle=NULL then return SM_OPEN_FAIL end if

        szlPointer = c_func(xMapViewOfFile,{wHandle,        -- hFileMappingObject
                                            FILE_MAP_WRITE, -- dwDesiredAccess
                                            0,              -- dwFileOffsetHigh
                                            0,              -- dwFileOffsetLow
                                            0})             -- dwNumberOfBytesToMap
        if szlPointer=NULL then
            {} = c_func(xCloseHandle, {wHandle})
            return SM_OPEN_FAIL
        end if

        smHandles = append(smHandles,wHandle)
    end if
    smNames = append(smNames,name)
    smPointers = append(smPointers,szlPointer)
    return szlPointer
end function

-------------------------------------------------

global procedure sm_close(object id)
atom ack, shmaddr, shmid
integer k
    if lib=-1 then ipc_init() end if
    if string(id) then
        k = find(id, smNames)
    else
        k = find(id, smPointers)
    end if
    if k then
        shmaddr = smPointers[k]
        shmid = smHandles[k]
        if platform()=LINUX then
            ack = c_func(xshmdt, {shmaddr}) -- detach
            if ack!=0 then ?9/0 end if
            ack = getAttached(shmid)
            --?{"sm_close",ack} -- (useful when testing)
            if ack=0 then
                ack = c_func(xshmctl, {shmid,IPC_RMID,NULL})
                if ack!=0 then ?9/0 end if
            end if
        elsif platform()=WINDOWS then
            ack = c_func(xUnmapViewOfFile,{shmaddr})
            if ack=0 then ?9/0 end if
            ack = c_func(xCloseHandle,{shmid})
            if ack=0 then ?9/0 end if
        end if
        smNames = smNames[1..k-1] & smNames[k+1..$]
        smHandles = smHandles[1..k-1] & smHandles[k+1..$]
        smPointers = smPointers[1..k-1] & smPointers[k+1..$]
    end if
end procedure

-------------------------------------------------

global function sm_alloc_lpsz(string name, sequence s)
atom lPointer
    if lib=-1 then ipc_init() end if
    lPointer = sm_create(name, length(s)+1)
    if lPointer>0 then
        poke(lPointer, s)
        poke(lPointer+length(s), 0)
    end if
    return lPointer
end function


