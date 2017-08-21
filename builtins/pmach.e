--
-- pmach.e
-- =======
--
-- Phix compatibility file for machine_func and machine_proc.
-- (Phix automatically includes this file /only/ if needed.)
--
-- The compiler converts eg allocate() to an opAlloc opcode; however 
-- old versions of machine.e and other legacy code might still invoke 
-- machine_func(16,n), so this maps it back.
--
-- Fairly obviously, this file is an unnecessary overhead for Phix, and 
-- there are 'machine_func deprecated' warnings to encourage getting rid.
-- That said, clearly the overhead of these wrappers on eg allocate must
-- be pretty insignificant, otherwise it would have gone long ago, and 
-- twice something pretty insignificant is still pretty insignificant.
--
--  <aside> Phix and RDS Eu use completely different back-ends, with a
--          completely different set of opcodes. There is no code at all
--          in the Phix backend for machine_func/proc, this is it.
--          This file exists purely for compatibility reasons.
--  </aside>
--
-- If you want to avoid warnings about machine_proc and machine_func being 
-- deprecated when running some existing code, just include pmach.e early 
-- on in the offending application, ideally after --/**/ so that RDS Eu
-- treats it as a comment but Phix does not, ie:
--
--      --/**/include pmach.e
--
-- Obviously as far as Phix is concerned, the better idea is to replace 
-- all machine_func(16,n) calls with allocate(n), etc. Phix ships with
-- versions of machine.e etc with special block comments, for example:
--
--      --/* Not required for Phix (defined as opAlloc)
--      global function allocate(positive_int n)
--          return machine_func(M_ALLOC, n)
--      end function
--      --*/
--
-- Phix treats everything between "--/*" and "--*/" as block comment,
-- whereas RDS Eu will process it normally. Also, if you see eg:
--
--  --/**/  if wait_key() then end if           --/*    -- Phix
--          if machine_func(26,0) then end if   --      -- RDS --*/
--
-- Then Phix treats "--/**/" as a block comment, calls wait_key(), and 
-- treats "--/* ... if machine_func(26,0) ... -- RDS --*/" as a comment, 
-- whereas RDS Eu treats the first line as a comment, calls machine_func(), 
-- and finally ignores the trailing "-- -- RDS --*/". (Ugly, but it works.)
--
-- Phix also honours "/*" & "*/" alone, but RDS Eu errors.
--   (btw, feel free to match your "--/*" with "*/" or "--*/".)
-- Note that "--<space>/*" is always treated as a line comment.
-- Some old versions of win32lib contain a "--/*", which is immediately
--  flagged as an error (along with half a dozen implicit sequence ops
--  and a missing "(Phix compatible)" comment, all fixed in the version
--  of win32lib shipped with Phix.)
--
-- Clearly it is sensible, as shown, to label such (cross-platform) code with 
--  '--Phix' and '--RDS' hints, but these are not mandatory. Most such code
--  should, of course, be hidden in library routines/builtin includes.
--
-- The latest version of Edita handles Phix block comments; it shows the
-- last example with wait_key in the builtin colour and machine_func in the 
-- comment colour (ie Edita [0.2.8+] favours the Phix code).
-- Of course, the leading -- above make this whole block comment-coloured; 
-- block comments do not start inside line comments, though they can end 
-- inside them.
--

constant
--  M_SOUND             = 1,    --sound()               DOS only (do nothing on win32)
--  M_LINE              = 2,    --draw_line()           DOS only
--  M_PALETTE           = 3,    --palette()             DOS only
--  ?                   = 4,
--  M_GRAPHICS_MODE     = 5,    --graphics_mode()       DOS only (do nothing on win32)
    M_CURSOR            = 6,    --cursor()
--  M_WRAP              = 7,    --wrap()                DOS only (ish)[DEV] [DEV h_wrap() now implemented in pfileio.e (to be renamed as wrap() one day)]
--  M_SCROLL            = 8,    --scroll()              DOS only (ish)[DEV] [DEV h_scroll() """]
    M_SET_T_COLOR       = 9,    --text_color()
    M_SET_B_COLOR       = 10,   --bk_color()
--  M_POLYGON           = 11,   --polygon()             DOS only
--  M_TEXTROWS          = 12,   --text_rows()           DOS only (ish)[DEV]
    M_VIDEO_CONFIG      = 13,   --video_config()        DOS only
--  M_GET_MOUSE         = 14,   --get_mouse()           DOS only
--  M_MOUSE_EVENTS      = 15,   --mouse_events()        DOS only
    M_ALLOC             = 16,   --allocate()
    M_FREE              = 17,   --free()
--  M_ELLIPSE           = 18,   --ellipse()             DOS only
    M_SEEK              = 19,   --seek()
    M_WHERE             = 20,   --where()
--  ?                   = 21,
    M_DIR               = 22,   --dir()
    M_CURRENT_DIR       = 23,   --current_dir()
--  M_MOUSE_POINTER     = 24,   --mouse_pointer()       DOS only
    M_GET_POSITION      = 25,   --get_postion()
    M_WAIT_KEY          = 26,   --wait_key()
--  M_ALL_PALETTE       = 27,   --all_palette()         DOS only
--  M_GET_DISPLAY_PAGE  = 28,   --get_display_page()    DOS only
--  M_SET_DISPLAY_PAGE  = 29,   --set_display_page()    DOS only
--  M_GET_ACTIVE_PAGE   = 30,   --get_active_page()     DOS only
--  M_SET_ACTIVE_PAGE   = 31,   --set_active_page()     DOS only
--  M_ALLOC_LOW         = 32,   --allocate_low()        DOS only
--  M_FREE_LOW          = 33,   --free_low()            DOS only
--  M_INTERRUPT         = 34,   --dos_interrupt()       DOS only
    M_SET_RAND          = 35,   --set_rand()
--  M_USE_VESA          = 36,   --use_vesa()            DOS only
--  M_CRASH_MESSAGE     = 37,   --crash_message()               DEV
--  M_TICK_RATE         = 38,   --tick_rate()           DOS only
--  M_GET_VECTOR        = 39,   --get_vector()          DOS only
--  M_SET_VECTOR        = 40,   --set_vector()          DOS only
--  M_LOCK_MEMORY       = 41,   --lock_memory()         DOS only
--  M_ALLOW_BREAK       = 42,   --allow_break()                 DEV
--  M_CHECK_BREAK       = 43,   --check_break()                 DEV
--  ?                   = 44,
--  ?                   = 45,
    M_A_TO_F64          = 46,   --atom_to_float64()
    M_F64_TO_A          = 47,   --float64_to_atom()
    M_A_TO_F32          = 48,   --atom_to_float32()
    M_F32_TO_A          = 49,   --float32_to_atom()
    M_OPEN_DLL          = 50,   --open_dll()
    M_DEFINE_C          = 51,   --define_c_func(), define_c_proc()
    M_CALL_BACK         = 52,   --call_back()
--  -153,   --   ?      = 53,
    M_FREE_CONSOLE      = 54,   --free_console()
    M_INSTANCE          = 55,   --instance()
    M_DEFINE_VAR        = 56,   --define_c_var()
--  M_CRASH_FILE        = 57,   --crash_file()                  DEV
    M_GET_SCREEN_CHAR   = 58,   --get_screen_char()
    M_PUT_SCREEN_CHAR   = 59,   --put_screen_char()
    M_FLUSH             = 60,   --flush()
    M_LOCK_FILE         = 61,   --lock_file()
    M_UNLOCK_FILE       = 62,   --unlock_file()
    M_CHDIR             = 63,   --chdir()
    M_SLEEP             = 64    --sleep()
-- from 4.0 [DEV]:
--constant
--      M_CRASH_MESSAGE = 37,
--      M_CRASH_FILE = 57,
--      M_CRASH_ROUTINE = 66,
--      M_CRASH = 67,
--      M_WARNING_FILE = 72


include builtins\pdir.e as pdir
include builtins\pscreen.e as pscreen
include builtins\VM\pcfunc.e as cfunc
--include builtins\dll.e as dll
include builtins\pchdir.e as cd
include builtins\pcurrdir.e as currdir
include builtins\VM\pfileioN.e
include builtins\VM\pFloatN.e as pfloat
include builtins\pAlloc.e as alloc
include builtins\VM\pInstance.e
include builtins\VM\pSleep.e


global function machine_func(atom a, object x)
--
-- Compatibility layer, normally avoided, warnings issued when used (see above).
--
-- Assume: An old version of machine.e defines say allocate(n) as machine_func(M_ALLOC,n).
--         Or some legacy code contains (silly) statements such as machine_func(16,n).
--         We /cannot/ just code allocate(n) since it would cause an infinite loop.
--         Instead we hand-code the standard compiler binary output for such.
--         (See pops.e for an introduction to #ilASM.)
--
-- Also:   An old version of dll.e defines say open_dll as machine_func(M_OPEN_DLL, ..).
--         Therefore we should not define the phix version of open_dll in dll.e and 
--         try "include dll.e as dll" followed by dll:open_dll since that would just
--         get that wrong version of dll.e and loop back here; instead the phix version
--         of open_dll is defined in builtins\pcfunc.e - but fear not, the compiler (as
--         well as this routine) /knows/ that and will auto-include as needed, there is
--         no requirement to change the includes of any legacy code. (Admittedly that
--         is not often true of newer OpenEuphoria "include std\xxx" statements, yet.)
--
-- Aside:
--  The fundamental philosophy of compatibility with RDS Eu/OpenEuphoria is this:
--  Absolute 100% compatibility is an unrealisable dream - and indeed if that was ever
--  achieved, there wouldn't be any point in having an alternative, would there?
--  More realistically, 99.9% compatibility means that on average a 10,000 line program
--  requires 10 changes, and so far that seems a reasonable and achievable target.
--  If it baulks (ie throws a compilation error) on a specific line that you can go
--  and fix, and then it runs fine, we're happy. If it fails silently, or takes you
--  (well, me) more than a few minutes, then (and only then) are we (me) sad ;-() 
--  Let me know if there is anything you are really struggling with (but please give 
--  it your best shot at fixing it yourself first).
--
object res, p1, p2, p3, p4
    if a=M_ALLOC then
--      #ilASM{ lea edi,[res]
--              lea esi,[x]
--              call %opAlloc}      -- [edi] = allocate([esi])
        res = alloc:allocate(x)
    elsif a=M_SEEK then
        p1 = x[1]   -- #ilASM cannot do things like subscripts
        p2 = x[2]   --  "
        #ilASM{ 
            [32]
                lea edi,[res]
                mov eax,[p1]        -- mov eax,fileno
                mov ecx,[p2]        -- mov ecx,pos
                call :%opSeek       -- [edi]:=seek(eax,ecx)
            [64]
                lea rdi,[res]       -- result location
                mov rax,[p1]        -- file number (opUnassigned, integer)
                mov rcx,[p2]        -- position (opUnassigned)
                call :%opSeek       -- [rdi]:=seek(rax,rcx)
              }
    elsif a=M_WHERE then
        #ilASM{
            [32]
                lea edi,[res]
                mov eax,[x]         -- file number (opUnassigned, integer)
                call :%opWhere      -- [edi]:=where(eax)
            [64]
                lea rdi,[res]       -- result location
                mov rax,[x]         -- file number (opUnassigned, integer)
                call :%opWhere      -- [rdi]:=where(rax)
              }
    elsif a=M_DIR then
        res = pdir:dir(x)
    elsif a=M_CURRENT_DIR then
--      #ilASM{ lea edx,[res]
--              call %opCurrDir}    -- [edx] = current_dir()
        res = currdir:current_dir()
    elsif a=M_GET_POSITION then
        #ilASM{
            [32]
                lea edi,[res]       -- result location
                call :%opGetPos     -- [edi]:=get_position()
            [64]
                lea rdi,[res]       -- result location
                call :%opGetPos     -- [rdi]:=get_position()
              }
    elsif a=M_WAIT_KEY then
--DEV edi/:%opWaitKey
        #ilASM{
            [32]
                lea edi,[res]       -- result location
                call :%opWaitKey    -- [edi] = wait_key()
            [64]
                lea rdi,[res]       -- result location
                call :%opWaitKey    -- [rdi] = wait_key()
              }
    elsif a=M_A_TO_F64 then
--      #ilASM{ lea edi,[res]
--              lea edx,[x]
--              call %opAto64}      -- [edi] = atom_to_float64([edx])
        p1 = x[1]
        res = pfloat:atom_to_float64(p1)
    elsif a=M_F64_TO_A then
--      #ilASM{ lea ecx,[res]
--              mov eax,[x]
--              call %op64toA}      -- [ecx] = float64_to_atom(eax)
        p1 = x[1]
        res = pfloat:float64_to_atom(p1)
    elsif a=M_A_TO_F32 then
--      #ilASM{ lea edi,[res]
--              lea edx,[x]
--              call %opAto32}      -- [edi] = atom_to_float32([edx])
        p1 = x[1]
        res = pfloat:atom_to_float32(p1)
    elsif a=M_F32_TO_A then
--      #ilASM{ lea ecx,[res]
--              mov eax,[x]
--              call %op32toA}      -- [ecx] = float32_to_atom(eax)
        p1 = x[1]
        res = pfloat:float32_to_atom(p1)
    elsif a=M_OPEN_DLL then
--      res = dll:open_dll(x)
        res = cfunc:open_dll(x)
    elsif a=M_DEFINE_C then
        p1 = x[1]   -- #ilasm cannot do things like subscripts
        p2 = x[2]   --  "
        p3 = x[3]   --  "
        p4 = x[4]   --  "
        if p4=0 then
            res = cfunc:define_c_proc(p1,p2,p3)
        else
            res = cfunc:define_c_func(p1,p2,p3,p4)
        end if
    elsif a=M_CALL_BACK then
        res = cfunc:call_back(x)
    elsif a=M_INSTANCE then
        #ilASM{
            [32]
                lea edi,[res]
                call :%opInstance   -- [edi] := instance()
            [64]
                lea rdi,[res]
                call :%opInstance   -- [rdi] := instance()
              }
    elsif a=M_DEFINE_VAR then
        p1 = x[1]   -- #ilasm cannot do things like subscripts
        p2 = x[2]   --  "
        res = cfunc:define_c_var(p1,p2)
    elsif a=M_LOCK_FILE then
        p1 = x[1]   -- #ilasm cannot do things like subscripts
        p2 = x[2]   --  "
        p3 = x[3]   --  "
        #ilASM{
            [32]
                lea edi,[res]       -- result location
                mov eax,[p1]        -- file number (opUnassigned, integer)
                mov ecx,[p2]        -- lock type (opUnassigned, integer)
                mov esi,[p3]        -- byte range (opUnassigned, sequence)
                call :%opLock       -- [edi]:=lock_file(eax,ecx,esi)
            [64]
                lea rdi,[res]       -- result location
                mov rax,[p1]        -- file number (opUnassigned, integer)
                mov rcx,[p2]        -- lock type (opUnassigned, integer)
                mov rsi,[p3]        -- byte range (opUnassigned, sequence)
                call :%opLock       -- [rdi]:=lock_file(rax,rcx,rsi)
              }
    elsif a=M_CHDIR then
--      #ilASM{ lea ecx,[res]
--              mov eax,[x]
--              call %opChDir}      -- [ecx] = chdir(eax)
        res = cd:chdir(x)
    elsif a=M_VIDEO_CONFIG then
        res = pscreen:video_config()
    elsif a=M_GET_SCREEN_CHAR then
        p1 = x[1]
        p2 = x[2]
        res = pscreen:get_screen_char(p1,p2)
    else
        -- oops...
        printf(1,"machine_func %d %s\n",{a,sprint(x)})
        a = 9/0
    end if
    return res
end function


global procedure machine_proc(atom a, object x)
--
-- Compatibility layer, normally avoided, warnings issued when used (see above).
--
object p1, p2, p3
--  if a = M_SOUND then
--      -- do nothing[DEV]
--  elsif a = M_GRAPHICS_MODE then
--      -- do nothing
--  els
    if a=M_SET_T_COLOR then
        #ilASM{
            [32]
                mov eax,[x]         -- color (opUnassigned, integer)
                call :%opTxtClr     -- text_color(eax)
            [64]
                mov rax,[x]         -- color (opUnassigned, integer)
                call :%opTxtClr     -- text_color(rax)
              }
    elsif a=M_SET_B_COLOR then
        #ilASM{
            [32]
                mov eax,[x]         -- color (opUnassigned, integer)
                call :%opBkClr      -- bk_color(eax)
            [64]
                mov rax,[x]         -- color (opUnassigned, integer)
                call :%opBkClr      -- bk_color(rax)
              }
    elsif a=M_FREE then
        alloc:free(x)
    elsif a=M_SET_RAND then
        #ilASM{
            [32]
                mov eax,[x]         -- seed value (opUnassigned)
                call :%opSetRand    -- set_rand(eax)
            [64]
                mov rax,[x]         -- seed value (opUnassigned)
                call :%opSetRand    -- set_rand(rax)
              }
    elsif a=M_FREE_CONSOLE then
        #ilASM{
                call :%opFreeCons -- free_console()
              }
    elsif a=M_FLUSH then
        if not integer(x) then ?9/0 end if
        #ilASM{
            [32]
                mov eax,[x]         -- (opUnassigned, should be integer)
                call :%opFlush      -- flush(eax)
            [64]
                mov rax,[x]         -- (opUnassigned)
                call :%opFlush      -- flush(rax)
              }
    elsif a=M_UNLOCK_FILE then
        p1 = x[1]   -- #ilasm cannot do things like subscripts
        p2 = x[2]   --  "
        if not integer(p1) then ?9/0 end if
        if not sequence(p2) then ?9/0 end if
        #ilASM{
            [32]
                mov eax,[p1]        -- file number (opUnassigned, integer)
                mov esi,[p2]        -- byte range (opUnassigned, sequence)
                call :%opUnLock     -- unlock_file(eax,esi)
            [64]
                mov rax,[p1]        -- file number (opUnassigned, integer)
                mov rsi,[p2]        -- byte range (opUnassigned, sequence)
                call :%opUnLock     -- [rdi]:=unlock_file(rax,rsi)
              }
    elsif a=M_SLEEP then
        if not atom(x) then ?9/0 end if
        #ilASM{
            [32]
                mov eax,[x]         -- seconds (opUnassigned)
                call :%opSleep      -- sleep(eax)
            [64]
                mov rax,[x]         -- seconds (opUnassigned)
                call :%opSleep      -- sleep(eax)
              }
    elsif a=M_PUT_SCREEN_CHAR then
        p1 = x[1]
        p2 = x[2]
        p3 = x[3]
        pscreen:put_screen_char(p1,p2,p3)
    elsif a=M_CURSOR then
        p1 = x[1]
        pscreen:cursor(p1)
--/* --DEV (also M_SCROLL)
    elsif a=M_WRAP then
        p1 = x[1]
        pfileio:h_wrap(p1)
--*/
    else
        -- oops...
        printf(1,"machine_proc %d %s\n",{a,sprint(x)})
        a = 9/0
    end if
end procedure

