--
-- builtins/isatty.e
-- =================
--
--  Phix implementation of isatty().
--
atom lib = NULL,    -- libc/kernel32
     xGetStdHandle,
     xGetFileType, 
     xGetLastError, 
     x_isatty,
     x_errno

constant NO_ERROR = 0,
         FILE_TYPE_CHAR = 0x0002,   -- The specified file is a character file, typically an LPT device or a console.
--       FILE_TYPE_DISK = 0x0001,   -- The specified file is a disk file.
--       FILE_TYPE_PIPE = 0x0003,   -- The specified file is a socket, a named pipe, or an anonymous pipe.
--       FILE_TYPE_REMOTE = 0x8000, -- Unused.
         FILE_TYPE_UNKNOWN = 0x0000 -- Either the type of the specified file is unknown, or the function failed.

--#define EPERM          1      /* Operation not permitted */
--constant ENOENT      = 2      -- No such file or directory
--#define ESRCH          3      /* No such process */
--#define EINTR          4      /* Interrupted system call */
--#define EIO            5      /* I/O error */
--#define ENXIO          6      /* No such device or address */
--#define E2BIG          7      /* Argument list too long */
--#define ENOEXEC        8      /* Exec format error */
constant EBADF        =  9      /* Bad file number */
--#define ECHILD        10      /* No child processes */
--#define EAGAIN        11      /* Try again */
--constant ENOMEM     = 12      -- Out of memory
--#define EACCES        13      /* Permission denied */
--#define EFAULT        14      /* Bad address */
--#define ENOTBLK       15      /* Block device required */
--#define EBUSY         16      /* Device or resource busy */
--constant EEXIST     = 17      -- File exists
--#define EXDEV         18      /* Cross-device link */
--#define ENODEV        19      /* No such device */
--#define ENOTDIR       20      /* Not a directory */
--#define EISDIR        21      /* Is a directory */
--#define EINVAL        22      /* Invalid argument */
--#define ENFILE        23      /* File table overflow */
--#define EMFILE        24      /* Too many open files */
constant ENOTTY       = 25      /* Not a typewriter */
--#define ETXTBSY       26      /* Text file busy */
--#define EFBIG         27      /* File too large */
--#define ENOSPC        28      /* No space left on device */
--#define ESPIPE        29      /* Illegal seek */
--#define EROFS         30      /* Read-only file system */
--#define EMLINK        31      /* Too many links */
--#define EPIPE         32      /* Broken pipe */
--#define EDOM          33      /* Math argument out of domain of func */
--#define ERANGE        34      /* Math result not representable */

global function isatty(integer fn)
-- fn should be 0/1/2 for stdin/stdout/stderr.
    if fn<0 or fn>2 then ?9/0 end if
    if lib=NULL then
        enter_cs()
        if platform()=WINDOWS then
            lib = open_dll("kernel32")
            xGetStdHandle           = define_c_func(lib, "GetStdHandle", {C_UINT}, C_PTR)
            xGetFileType            = define_c_func(lib, "GetFileType", {C_PTR}, C_INT)
            xGetLastError           = define_c_func(lib, "GetLastError", {}, C_INT)
        elsif platform()=LINUX then
--          lib = open_dll("libc.so.6")
            lib = open_dll("libc.so")
            x_isatty = define_c_func(lib, "isatty", {C_INT}, C_INT)
            x_errno = define_c_var(lib, "errno")
        end if
        leave_cs()
    end if
    if platform()=WINDOWS then
        -- STD_INPUT_HANDLE      = -10, (0)
        -- STD_OUTPUT_HANDLE     = -11, (1)
        -- STD_ERROR_HANDLE      = -12, (2)
        fn = -10-fn     -- -10/-11/-12
        atom hIn = c_func(xGetStdHandle,{fn})
        integer ttype = c_func(xGetFileType,{hIn})
        if ttype=FILE_TYPE_UNKNOWN then
            if c_func(xGetLastError,{})!=NO_ERROR then ?9/0 end if
        end if
        return ttype=FILE_TYPE_CHAR
    else
        integer res = c_func(x_isatty,{fn})
        if res=0 then
            integer errno = peek4s(x_errno)
            if errno = EBADF then ?9/0 end if
            if errno!=ENOTTY then ?9/0 end if
        end if
        return res
    end if
end function

