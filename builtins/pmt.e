--
-- test pmach.e compatibility.
--  include this in pdiag.e, rebuild Phix, and run all tests (ignoring performance issues)
--
include pmach.e
constant
     M_OPEN_DLL  = 50,
     M_DEFINE_C  = 51,
     M_CALL_BACK = 52,
     M_FREE_CONSOLE = 54,
     M_DEFINE_VAR = 56

global function open_dll(sequence file_name)
    return machine_func(M_OPEN_DLL, file_name)
end function

global function define_c_var(atom lib, sequence variable_name)
    return machine_func(M_DEFINE_VAR, {lib, variable_name})
end function

global function define_c_proc(object lib, object routine_name, sequence arg_sizes)
    return machine_func(M_DEFINE_C, {lib, routine_name, arg_sizes, 0})
end function

global function define_c_func(object lib, object routine_name, sequence arg_sizes, atom return_type)
    return machine_func(M_DEFINE_C, {lib, routine_name, arg_sizes, return_type})
end function

global function call_back(object id)
    return machine_func(M_CALL_BACK, id)
end function

global procedure free_console()
    machine_proc(M_FREE_CONSOLE, 0)
end procedure

constant
     M_SEEK  = 19,
     M_WHERE = 20,
     M_DIR   = 22,
     M_CURRENT_DIR = 23,
     M_ALLOW_BREAK = 42,
     M_CHECK_BREAK = 43,
     M_FLUSH = 60,
     M_LOCK_FILE = 61,
     M_UNLOCK_FILE = 62,
     M_CHDIR = 63

type file_number(integer f)
    return f>=0
end type

type file_position(atom p)
    return p>=-1
end type

type boolean(integer b)
    return b=0 or b=1
end type

global function seek(file_number fn, file_position pos)
    return machine_func(M_SEEK, {fn, pos})
end function

global function where(file_number fn)
    return machine_func(M_WHERE, fn)
end function

global procedure flush(file_number fn)
    machine_proc(M_FLUSH, fn)
end procedure

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

global function lock_file(file_number fn, lock_type t, byte_range br)
    return machine_func(M_LOCK_FILE, {fn, t, br})
end function

global procedure unlock_file(file_number fn, byte_range br)
    machine_proc(M_UNLOCK_FILE, {fn, br})
end procedure

global function dir(sequence name)
    return machine_func(M_DIR, name)
end function

global function current_dir()
    return machine_func(M_CURRENT_DIR, 0)
end function

global function chdir(sequence newdir)
    return machine_func(M_CHDIR, newdir)
end function

global procedure allow_break(boolean b)
    machine_proc(M_ALLOW_BREAK, b)
end procedure

global function check_break()
    return machine_func(M_CHECK_BREAK, 0)
end function

constant M_WAIT_KEY = 26

global function wait_key()
    return machine_func(M_WAIT_KEY, 0)
end function

constant
--   M_CURSOR         = 6,      -- to be made a builtin [DEV]
--   M_WRAP           = 7,      -- to be made a builtin
--   M_SCROLL         = 8,      -- to be made a builtin --DEV
     M_SET_T_COLOR    = 9,      -- opTxtClr
     M_SET_B_COLOR    = 10,     -- opBkClr
--   M_TEXTROWS       = 12      -- to be made a builtin
     M_GET_POSITION   = 25      -- opGetPos

type color(integer x)
    return x>=0 and x<=255
end type


--type positive_int(integer x)
--  return x >= 1
--end type

--/* Not required for Phix [DEV]:
global
procedure cursor(integer style)
-- choose a cursor style
    machine_proc(M_CURSOR, style)
end procedure
--*/

global function get_position()
-- return {line, column} of current cursor position
    return machine_func(M_GET_POSITION, 0)
end function

--DEV:
--global function text_rows(positive_int rows)
--  return machine_func(M_TEXTROWS, rows)
--end function

--DEV:
--global procedure wrap(boolean on)
---- on = 1: characters will wrap at end of long line
---- on = 0: lines will be truncated
--  machine_proc(M_WRAP, on)
--end procedure

--DEV:
--global procedure scroll(integer amount, 
--          positive_int top_line, 
--          positive_int bottom_line)
---- scroll lines of text on screen between top_line and bottom_line
---- amount > 0: scroll text up by amount lines
---- amount < 0: scroll text down by amount lines
---- (had only the first parameter in v1.2) 
--  machine_proc(M_SCROLL, {amount, top_line, bottom_line})
--end procedure

global procedure text_color(color c)
    machine_proc(M_SET_T_COLOR, c)
end procedure

global procedure bk_color(color c)
    machine_proc(M_SET_B_COLOR, c)
end procedure

constant
     M_ALLOC = 16,              -- now a builtin
     M_FREE = 17,               -- now a builtin
     M_SET_RAND = 35,
     M_CRASH_MESSAGE = 37,
     M_A_TO_F64 = 46,           -- now a builtin
     M_F64_TO_A = 47,           -- now a builtin
     M_A_TO_F32 = 48,           -- now a builtin
     M_F32_TO_A = 49,           -- now a builtin
     M_CRASH_FILE = 57,
     M_CRASH_ROUTINE = 66

type positive_int(integer x)
    return x>=1
end type

type machine_addr(atom a)
    return a>0 and a<=#FFFFFFFF and floor(a)=a
end type

type sequence_8(sequence s)
    return length(s)=8
end type

type sequence_4(sequence s)
    return length(s)=4
end type

global function allocate(positive_int n)
    return machine_func(M_ALLOC, n)
end function

global procedure free(machine_addr a)
    machine_proc(M_FREE, a)
end procedure

global procedure set_rand(integer seed)
    machine_proc(M_SET_RAND, seed)
end procedure

global procedure crash_message(sequence msg)
    machine_proc(M_CRASH_MESSAGE, msg)
end procedure

global procedure crash_file(sequence file_path)
    machine_proc(M_CRASH_FILE, file_path)
end procedure

global procedure crash_routine(integer proc)
    machine_proc(M_CRASH_ROUTINE, proc)
end procedure

global function atom_to_float64(atom a)
    return machine_func(M_A_TO_F64, a)
end function

global function atom_to_float32(atom a)
    return machine_func(M_A_TO_F32, a)
end function

global function float64_to_atom(sequence_8 ieee64)
    return machine_func(M_F64_TO_A, ieee64)
end function

global function float32_to_atom(sequence_4 ieee32)
    return machine_func(M_F32_TO_A, ieee32)
end function

constant M_INSTANCE = 55
global function instance()
    return machine_func(M_INSTANCE, 0)
end function

constant M_SLEEP = 64
global procedure sleep(integer t)
    if t>0 then
        machine_proc(M_SLEEP, t)
    end if
end procedure


