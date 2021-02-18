--
-- dll.e  (Phix compatible)
-- =====
--
-- Copied from Euphoria 2.4
-- routines and constants for dynamic linking to C functions
--
-- Note: This file is completely empty as far as Phix is concerned,
--       except for the [newish] sizeof() routine and the include 
--       builtins\VM\pcfunc.e statement, which allows eg:
--          include dll.e as dll
--          constant xFunc = dll:define_c_func(lib,"name",...)
--       to work equally on RDS Eu and Phix.
--
-- See pmach.e for explanation/examples of block comment handling.
--

without trace

--DEV wrong one for newEmit:
--!/**/ include builtins\pcfunc.e   -- (Phix compatible)
--/**/ include builtins\VM\pcfunc.e -- (Phix compatible)

--/* Not required for Phix (see syminit() in psym.e)
-- C types for .dll arguments and return value:
global constant 
     C_CHAR    = #01000001,
     C_UCHAR   = #02000001,
     C_SHORT   = #01000002,
     C_USHORT  = #02000002,
     C_INT     = #01000004,
     C_UINT    = #02000004,
     C_LONG    = C_INT,
     C_ULONG   = C_UINT,
     C_POINTER = C_ULONG,
     C_FLOAT   = #03000004,
--   C_DOUBLE  = #03000008,     (deprecated)

-- Euphoria types for .dll arguments and return value:
     E_INTEGER  = #06000004,
     E_ATOM     = #07000004,
     E_SEQUENCE = #08000004,
     E_OBJECT   = #09000004,

--   P_REF      = #04000004,    -- NB: Phix uses this instead of above four. [DEV]

     NULL = 0 -- NULL pointer
--*/

--/* Not needed for Phix
constant
     M_OPEN_DLL  = 50,
     M_DEFINE_C  = 51,
     M_CALL_BACK = 52,
     M_FREE_CONSOLE = 54,
     M_DEFINE_VAR = 56
--*/

--/* Not required for Phix (defined in pcfunc.e [auto-include])
global
function open_dll(sequence file_name)
-- Open a .DLL file
    return machine_func(M_OPEN_DLL, file_name)
end function
--*/

--/* Not Phix (defined in pcfunc.e [auto-include])
global
function define_c_var(atom lib, sequence variable_name)
-- get the memory address where a global C variable is stored
    return machine_func(M_DEFINE_VAR, {lib, variable_name})
end function
--*/

--/* Not Phix (defined in pcfunc.e [auto-include])
global
function define_c_proc(object lib, object routine_name, 
                  sequence arg_sizes)
-- Define a C function with VOID return type, or where the
-- return value will always be ignored.
-- Alternatively, define a machine-code routine at a given address.
    return machine_func(M_DEFINE_C, {lib, routine_name, arg_sizes, 0})
end function
--*/

--/* Not Phix (defined in pcfunc.e [auto-include])
global
function define_c_func(object lib, object routine_name, 
                  sequence arg_sizes, atom return_type)
-- define a C function (or machine code routine)
    return machine_func(M_DEFINE_C, {lib, routine_name, arg_sizes, return_type})
end function
--*/

--/* Not Phix (defined in pcfunc.e [auto-include])
global
function call_back(object id)
-- return a 32-bit call-back address for a Euphoria routine
-- id can be of the form: 
--     routine_id          - for Linux or Windows stdcall calls, 
-- or 
--     {'+', routine_id}   - for Windows cdecl calls
    return machine_func(M_CALL_BACK, id)
end function
--*/

--/* Not required for Phix (defined as opFreeCons)
global
procedure free_console()
-- delete the console text-window (if one currently exists)
    machine_proc(M_FREE_CONSOLE, 0)
end procedure
--*/

global function sizeof(integer x)
--15/2/21:
--  integer top = floor(x/#100000)
    integer top = floor(x/#1000000)
    if not find(top,{#01,#02,#03}) then ?9/0 end if
    x -= top*#1000000
    if not find(x,{1,2,4,8}) then ?9/0 end if
    return x
end function

