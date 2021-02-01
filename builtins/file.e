--
-- Phix compatible version of file.e
--
--  This only defines walk_dir for Phix;
--   my_dir has been replaced with set_walk_dir_sort_rtn().
--   for dir() see pdir.e, 
--   for allow_break() and check_break() see pbreak.e [DEV]
--   the rest are defined as opcodes.
--

--
-- Euphoria 2.4
-- Directory and File Operations --

-- (Not strictly required for Phix, would be auto-included as needed, btw):

include sort.e
include misc.e
--/**/include pchdir.e

--/* Not required for Phix:
constant
     M_SEEK         = 19,
     M_WHERE        = 20,
     M_DIR          = 22,
     M_CURRENT_DIR  = 23,
     M_ALLOW_BREAK  = 42,
     M_CHECK_BREAK  = 43,
     M_FLUSH        = 60,
     M_LOCK_FILE    = 61,
     M_UNLOCK_FILE  = 62,
     M_CHDIR        = 63
--*/

--/* Not required for Phix
type file_number(integer f)
    return f>=0
end type

type file_position(atom p)
    return p>=-1
end type

type boolean(integer b)
    return b=0 or b=1
end type
--*/

--/* Not required for Phix (defined as opSeek):
global function seek(file_number fn, file_position pos)
-- Seeks to a byte position in the file, 
-- or to end of file if pos is -1.
-- This function is normally used with
-- files opened in binary mode.
    return machine_func(M_SEEK, {fn, pos})
end function
--*/

--/* Not required for Phix (defined as opWhere):
global function where(file_number fn)
-- Returns the current byte position in the file.
-- This function is normally used with
-- files opened in binary mode.
    return machine_func(M_WHERE, fn)
end function
--*/

--/* Not required for Phix (defined as opFlush):
global procedure flush(file_number fn)
-- flush out the buffer associated with file fn
    machine_proc(M_FLUSH, fn)
end procedure
--*/

--/* Not required for Phix (defined in psym.e)
global constant LOCK_SHARED = 1, 
                LOCK_EXCLUSIVE = 2
--*/

--/* Not required for Phix
type lock_type(integer t)
    if platform()=LINUX then
        return t=LOCK_SHARED or t=LOCK_EXCLUSIVE
    else
        return 1
    end if
end type

type byte_range(sequence br)
    if length(br)=0 then
        return 1
    elsif length(br)=2 and br[1]<=br[2] then
        return 1
    else
        return 0
    end if
end type
--*/

--/* Not required for Phix (defined as op[Un]Lock):
global function lock_file(file_number fn, lock_type t, byte_range br)
-- Attempt to lock a file so other processes won't interfere with it.
-- The byte range can be {} if you want to lock the whole file
    return machine_func(M_LOCK_FILE, {fn, t, br})
end function

global procedure unlock_file(file_number fn, byte_range br) 
-- The byte range can be {} if you want to unlock the whole file.
    machine_proc(M_UNLOCK_FILE, {fn, br})
end procedure
--*/

--/* Not required for Phix (defined in psym.e):
global constant 
    D_NAME = 1,
    D_ATTRIBUTES = 2,
    D_SIZE = 3,

    D_YEAR = 4,
    D_MONTH = 5,
    D_DAY = 6,

    D_HOUR = 7,
    D_MINUTE = 8,
    D_SECOND = 9
--*/

--/* Not required for Phix (defined in pdir.e):
global function dir(sequence name)
-- returns directory information, given the name
-- of a file or directory. Format returned is:
-- {
--  {"name1", attributes, size, year, month, day, hour, minute, second},
--  {"name2", ...                           },
-- }
    return machine_func(M_DIR, name)
end function
--*/

--/* Not required for Phix (defined in autoinclude builtins/pcurrdir.e)
global function current_dir()
-- returns name of current working directory
    return machine_func(M_CURRENT_DIR, 0)
end function
--*/

--/* Not required for Phix (defined in autoinclude builtins/pchdir.e)
global function chdir(sequence newdir)
-- Changes the current directory. Returns 1 - success, 0 - fail.
    return machine_func(M_CHDIR, newdir)
end function
--*/

--/* Not required for Phix (defined in autoinclude pbreak.e)
global procedure allow_break(boolean b)
-- If b is TRUE then allow control-c/control-break to
-- terminate the program. If b is FALSE then don't allow it.
-- Initially they *will* terminate the program, but only when it
-- tries to read input from the keyboard.
    machine_proc(M_ALLOW_BREAK, b)
end procedure

global function check_break()
-- returns the number of times that control-c or control-break
-- were pressed since the last time check_break() was called
    return machine_func(M_CHECK_BREAK, 0)
end function
--*/

-- Generalized recursive directory walker

--/* Not required for Phix (defined psym.e/loadBuiltins):
global constant W_BAD_PATH = -1 -- error code
--*/
-- not strictly required, but helps with opCallOnce/fwd calls/onDeclaration:
--/**/include builtins\pdir.e   -- dir()

--constant SLASH = iff(platform()=LINUX?'/':'\\')

function default_dir(sequence path)
-- Default directory sorting function for walk_dir().
-- * sorts by name *
object d
    d = dir(path)
    if sequence(d) then
        -- sort by name
        d = sort(d)
    end if
    return d
end function


-- override the dir sorting function with your own routine id
constant DEFAULT = -2
--global -- Removed for Phix, use set_walk_dir_sort_rtn() instead
--integer my_dir
--my_dir = DEFAULT  -- it's better not to use routine_id() here,
--    -- or else users will have to bind with clear routine names
--global procedure set_walk_dir_sort_rtn(integer id)
--  my_dir = id
--end procedure

global function walk_dir(sequence path_name, integer your_function, integer scan_subdirs=FALSE, integer my_dir=DEFAULT)
-- Generalized Directory Walker
-- Walk through a directory and (optionally) its subdirectories,
-- "visiting" each file and subdirectory. Your function will be called
-- via its routine id. The visits will occur in alphabetical order.
-- Your function should accept the path name and dir() entry for
-- each file and subdirectory. It should return 0 to keep going,
-- or an error code (greater than 0) to quit, or it can return
-- any sequence or atom other than 0 as a useful diagnostic value.
object d, abort_now
sequence di
--integer SLASH = iff(platform()=LINUX?'/':'\\') 
--integer SLASH
--  if platform()=LINUX then
--      SLASH = '/'
--  else
--      SLASH = '\\'
--  end if

    -- get the full directory information
    if my_dir=DEFAULT then
        d = default_dir(path_name)
    else
        d = call_func(my_dir, {path_name})
    end if
    if atom(d) then
        return W_BAD_PATH
    end if

    -- trim any trailing blanks or '\' characters from the path
    while length(path_name)>0
      and find(path_name[length(path_name)], {' ', SLASH}) do
        path_name = path_name[1..length(path_name)-1]
    end while

    for i=1 to length(d) do
        di = d[i]
        if find('d', di[D_ATTRIBUTES]) then
            -- a directory
            if not find(di[D_NAME], {".", ".."}) then
                abort_now = call_func(your_function, {path_name, di})
                if not equal(abort_now, 0) then
                    return abort_now
                end if
                if scan_subdirs then
                    abort_now = walk_dir(path_name & SLASH & di[D_NAME],
                                         your_function, scan_subdirs, my_dir)

                    if not equal(abort_now, 0) 
                    and not equal(abort_now, W_BAD_PATH) then
                    -- allow BAD PATH, user might delete a file or directory 
                        return abort_now
                    end if
                end if
            end if
        else
            -- a file
            abort_now = call_func(your_function, {path_name, di})
            if not equal(abort_now, 0) then
                return abort_now
            end if
        end if
    end for
    return 0
end function
