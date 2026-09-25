--
-- libmpv.e
--
-- best docs yet found: https://www.ccoderun.ca/programming/doxygen/mpv/client_8h.html#a1ec5c0e88f5a261589a2f52f3306c5e0
--                also: https://mpv-player-mpv.mintlify.app/embedding/libmpv
-- https://mpv.io/manual/master/
--
include cffi.e

global constant --MPV_FORMAT_NONE = 0,
                MPV_FORMAT_STRING = 1,
--              MPV_FORMAT_OSD_STRING = 2,
                MPV_FORMAT_FLAG = 3,
--              MPV_FORMAT_INT64 = 4,
--              MPV_FORMAT_INTEGER = 4,
                MPV_FORMAT_INT = 4,
                MPV_FORMAT_DOUBLE = 5,

                MPV_ERROR_SUCCESS = 0

--/*
MPV_ERROR_SUCCESS               0       No error
MPV_ERROR_EVENT_QUEUE_FULL      -1      Client event queue overflowed
MPV_ERROR_NOMEM                 -2      Memory allocation failed
MPV_ERROR_UNINITIALIZED         -3      Core not yet initialized
MPV_ERROR_INVALID_PARAMETER     -4      Invalid or unsupported parameter
MPV_ERROR_OPTION_NOT_FOUND      -5      Option does not exist
MPV_ERROR_OPTION_FORMAT         -6      Unsupported format for option
MPV_ERROR_OPTION_ERROR          -7      Option value could not be parsed
MPV_ERROR_PROPERTY_NOT_FOUND    -8      Property does not exist
MPV_ERROR_PROPERTY_FORMAT       -9      Unsupported format for property
MPV_ERROR_PROPERTY_UNAVAILABLE  -10     Property exists but is not available
MPV_ERROR_PROPERTY_ERROR        -11     Error getting or setting property
MPV_ERROR_COMMAND               -12     Command execution error
MPV_ERROR_LOADING_FAILED        -13     File loading error
MPV_ERROR_AO_INIT_FAILED        -14     Audio output initialization failed
MPV_ERROR_VO_INIT_FAILED        -15     Video output initialization failed
MPV_ERROR_NOTHING_TO_PLAY       -16     No audio or video streams found
MPV_ERROR_UNKNOWN_FORMAT        -17     File format could not be determined
MPV_ERROR_UNSUPPORTED           -18     System requirements not met
MPV_ERROR_NOT_IMPLEMENTED       -19     Function is a stub
MPV_ERROR_GENERIC               -20     Unspecified error
--MPV_ERROR_UNAVAILABLE (return code -1)
--*/

atom mpv = NULL, pData
integer x_mpv_error_string,
        x_mpv_client_api_version,
        x_mpv_create,
        x_mpv_initialize,
        x_mpv_command_string,
        x_mpv_free,
        x_mpv_get_property,
--      x_mpv_observe_property,
        x_mpv_set_option_string,
        x_mpv_set_property_string,
        x_mpv_terminate_destroy
--      x_mpv_wait_event

--global integer id_mpv_event,
--             id_mpv_event_property

sequence ctx_table
integer ctx_free = 0

--local procedure open_mpv_dll(string dll_name="", boolean mpir_only=false, fatal=true)
--global procedure open_mpv_dll(integer help_rtn=0)
local procedure open_mpv_dll(integer help_rtn=0)
--
-- internal: if mpv=NULL then mpv_init() end if
--           since all other functions need a ctx from mpv_create(),
--           that's the only place this needs to be called from.
--?external: provide a help_rtn(string msg) procedure that aborts
--           if the user cancels or returns here to download it.
--
    if platform()=WINDOWS then
        string dll_name = sprintf("libmpv-2.%dbit.ca25.dll",{machine_bits()}), -- client api 2.5 (built in August 2025, latest as of Sept 2026)
        -- aside: this one needed gtk_disable_setlocale().
--      string dll_name = sprintf("libmpv-2.%dbit.ca20.dll",{machine_bits()}), -- client api 2.0 (built in April 2022, not part of distro, but available in repo)
-- https://github.com/shinchiro/mpv-winbuild-cmake/releases
--:0: OpenSSL internal error: assertion failed: lock != NULL
--      string dll_name = sprintf("libmpv-2.%dbitz.dll",{machine_bits()}), -- (garbage...)
               path = "", cd = current_dir()
        while true do -- app, then \builtins (if it exists), then download and retry
            string full_path = join_path({path,dll_name})
            mpv = open_dll(full_path,false)
            if mpv!=0 then exit end if
            bool bError = length(path)>0
            if not bError then
                path = include_paths()[1]
                assert(ends(`\builtins\`,path))
                bError = get_file_type(path)!=FILETYPE_DIRECTORY
                if bError then path = "" end if
            end if
            if bError then
                string either = iff(length(path)?" either":""),
                          msg = sprintf("%s not found in%s:\n%s\n%s\n",
                                          {dll_name,either,cd,path})
                if help_rtn=0 then crash(msg) end if
                msg &= "\nDownload from PCAN?\n"
                help_rtn(msg)
                ?9/0 -- TBC (DEV)
            end if
        end while
--function find_dll(string dll_name)
--  if platform()!=WINDOWS then ?9/0 end if
--  if machine_bits()!=32 then ?9/0 end if
--  atom res = open_dll(dll_name,false)
--  if res=NULL then
--      -- try to load from \builtins
--      sequence s = include_paths()
--      for i=1 to length(s) do
--          if match("builtins",s[i]) then
--              res = open_dll(join_path({s[i],dll_name}),false)
--              if res!=NULL then exit end if
--          end if
--      end for
--      if res=NULL then ?9/0 end if
--  end if
--  return res
--end function

    else
        mpv = open_dll("libmpv.so.2") -- (untested, crash on failure...)
    end if
--/*
    if mpir_dll=NULL then
        if fatal then
            string msg = mpir_dll&" not found. "
            if platform()=LINUX then
                msg &= "Fix your mpfr install.\n"&
                       "Use your package manager, or see https://www.mpfr.org\n"
                dll_name = ""
            elsif platform()=WINDOWS then
--DEV rewrite (create the PCAN page first) alt: http://phix.x10.mx/pmwiki/uploads/Mpfr315Mpir272.zip
--SUG or, (on windows anyway) offer to download?
--BETTER: write a pGUI demo to download something, and unzip/install/run it.
--DEV...
                msg &= "Obtain from http://phix.x10.mx/pmwiki/pmwiki.php?n=Main.Mpfr or\n"&
                       "http://www.atelierweb.com/mpir-and-mpfr, and install to\n"&
                       "system32/syswow64, builtins, or application directory.\n"
            else
                ?9/0 -- unknown platform
            end if
            crash(msg)
        end if
    end if
--*/
    pData = allocate(machine_word())

    x_mpv_error_string  = define_c_func(mpv,"mpv_error_string",
        {C_INT},    --  int error
        C_PTR)      -- char*

    x_mpv_client_api_version  = define_c_func(mpv,"mpv_client_api_version",
        {},         -- (void)
        C_ULONG)    -- unsigned long

    x_mpv_create  = define_c_func(mpv,"mpv_create",
        {},         -- (void)
        C_PTR)      -- mpv_handle*

    x_mpv_initialize  = define_c_func(mpv,"mpv_initialize",
        {C_PTR},    --  mpv_handle* ctx
        C_INT)      -- int (error code)

    x_mpv_command_string  = define_c_func(mpv,"mpv_command_string",
        {C_PTR,     --  mpv_handle* ctx
         C_PTR},    --  const char* args
        C_INT)      -- int (error code)

    x_mpv_free  = define_c_proc(mpv,"mpv_free",
        {C_PTR})    --  void* data

    x_mpv_get_property  = define_c_func(mpv,"mpv_get_property",
        {C_PTR,     --  mpv_handle* ctx
         C_PTR,     --  const char* name
         C_INT,     --  mpv_format format
         C_PTR},    --  void* data
        C_INT)      -- int (error code)

--DEV try again as per #ilASM below...
--MPV_EXPORT int mpv_observe_property(mpv_handle *mpv, uint64_t reply_userdata,
--                                  const char *name, mpv_format format);
--if machine_bits()=64 then
--  x_mpv_observe_property  = define_c_func(mpv,"mpv_observe_property",
--      {C_PTR,     --  mpv_handle* ctx
--       C_INT,     --  uint64_t reply_userdata
--       C_PTR,     --  const char* name
--       C_INT},    --  mpv_format format
--      C_INT)      -- int (error code)
--else
--  x_mpv_observe_property  = define_c_func(mpv,"mpv_observe_property",
--      {C_PTR,     --  mpv_handle* ctx
--       C_INT,     --  uint64_t reply_userdata
--       C_INT,     --  uint64_t reply_userdata (grr)
--       C_PTR,     --  const char* name
--       C_INT},    --  mpv_format format
--      C_INT)      -- int (error code)
--end if

    x_mpv_set_option_string  = define_c_func(mpv,"mpv_set_option_string",
        {C_PTR,     --  mpv_handle* ctx
         C_PTR,     --  const char* name
         C_PTR},    --  const char* data
        C_INT)      -- int (error code)

--  x_mpv_set_property  = define_c_func(mpv,"mpv_set_property",
--      {C_PTR,     --  mpv_handle* ctx
--       C_PTR,     --  const char* name
--       C_INT,     --  mpv_format format
--       C_PTR},    --  void* data
--      C_INT)      -- int (error code)

    x_mpv_set_property_string   = define_c_func(mpv,"mpv_set_property_string",
        {C_PTR,     --  mpv_handle* ctx
         C_PTR,     --  const char* name
         C_PTR},    --  const char* data
        C_INT)      -- int (error code)

    x_mpv_terminate_destroy  = define_c_proc(mpv,"mpv_terminate_destroy",
        {C_PTR})    --  mpv_handle* ctx

--  x_mpv_wait_event    = define_c_func(mpv,"mpv_wait_event",
--      {C_PTR,     --  mpv_handle* ctx
--       C_DBL},    --  double timeout
--      C_PTR)      -- mpv_event*

--  id_mpv_event = define_struct(`typedef struct mpv_event {
--                                  int /*mpv_event_id*/ event_id;
--                                  int error;
--                                  uint64_t reply_userdata;
--                                  void *data;
--                               } mpv_event;`)
--
--  id_mpv_event_property = define_struct(`typedef struct mpv_event_property {
--                                          const char *name;
--                                          int /*mpv_format*/ format;
--                                          void *data;
--                                        } mpv_event_property;`)

    ctx_table = {}

end procedure

local function mpv_error_string(integer err)
    atom pStr = c_func(x_mpv_error_string,{err})
    -- (no need to free pStr)
    string res = peek_string(pStr)
    return res
end function

global function mpv_client_api_version(string rtype="")
    -- the default rtype returns eg "2.5", whereas
    -- "ulong" returns eg #20005 and "mm" returns {2,5}.
    if mpv=NULL then open_mpv_dll() end if
    atom res = c_func(x_mpv_client_api_version,{})
    if rtype="ulong" then return res end if   -- (rarely if ever needed)
    sequence mm = {and_bits(floor(res/#10000),#FFFF), -- major
                   and_bits(res,#FFFF)}              -- minor
    if rtype="mm" then return mm end if -- (best for compatability chks)
    return sprintf("%d.%d",mm)     -- (best for displaying to the user!)
end function

global function mpv_create(bool bCrash=true)
    --
    -- error_handler: (optional) procedure to invoke on failure,
    -- when >0 it should either abort() or throw and catch an
    -- exception to unwind the stack cleanly, or when -1 failure
    -- is signalled by returning null, else (0) crashes if null.
    -- Returns an mpv context (or null when error_handler is -1).
    --
    if mpv=NULL then open_mpv_dll() end if
    atom mpv_ctx = c_func(x_mpv_create,{})
    if mpv_ctx=NULL and bCrash then
        crash("failed creating mpv context")
    end if
    integer mpv_id = ctx_free
    if mpv_id then
        ctx_free = ctx_table[ctx_free]
        ctx_table[mpv_id] = mpv_ctx
    else
        ctx_table &= mpv_ctx
        mpv_id = length(ctx_table)
    end if
    return mpv_id
end function

global procedure mpv_initialize(integer ctx)
    -- aside: if mpv is null, where'd ya get ctx from?
    integer res = c_func(x_mpv_initialize,{ctx_table[ctx]})
    assert(res==0)
end procedure

global function mpv_normalize_path(string raw_path)
    -- Convert backslashes to forward slashes (Windows-safe + mpv-safe)
    -- eg `C:\Program Files (x86)\Phix\demo\theGUI\test_colorbars.mp4`
    -- => `C:/Program Files (x86)/Phix/demo/theGUI/test_colorbars.mp4`
    return substitute(raw_path, "\\", "/")
end function

global procedure mpv_command_string(integer ctx, string command)
--?{"mpv_command_string",command}
    integer res, retries = 0
    do
        res = c_func(x_mpv_command_string,{ctx_table[ctx],command})
        if res==MPV_ERROR_SUCCESS then exit end if
        ?{"mpv_command",command}
        sleep(0.1)
        retries += 1
    until retries>50
    if res!=MPV_ERROR_SUCCESS then
        string estr = mpv_error_string(res)
        crash("mpv_command_string(`%s`)=>%d [%s]",{command,res,estr})
    end if
end procedure

local function mpv_free(atom data)
    c_proc(x_mpv_free,{data})
    return NULL
end function

global function mpv_get_property(integer ctx, string name, integer mpv_format=MPV_FORMAT_STRING, object dflt="?9/0")
    --
    -- aside: some dumbass AI coding agent fed me made-up MPV_FORMAT_XXX constants,
    --        which I swallowed hook line and sinker, and resorted to doing everything
    --        via MPV_FORMAT_STRING, one of just two it got right. Now that I have the
    --        correct constants defined this could perhaps be improved (slightly)...
    --
    integer r, retries = 0
    do
        r = c_func(x_mpv_get_property,{ctx_table[ctx],name,MPV_FORMAT_STRING,pData})
--      if r=MPV_ERROR_SUCCESS then exit end if
        if r=MPV_ERROR_SUCCESS or dflt!="?9/0" then exit end if
--?{"mpv_get_property",name}
        sleep(0.1)
        retries += 1
    until retries>50
    if r!=MPV_ERROR_SUCCESS then
        if dflt="?9/0" then
            string rs = mpv_error_string(r)
            crash("mpv_get_property(%s)=>%d [%s]",{name,r,rs})
        end if
        return dflt
    end if
    atom data = peekns(pData)
    object res = peek_string(data)
    data = mpv_free(data)
    if mpv_format==MPV_FORMAT_FLAG then
        res = iff(res="yes"?true:
              iff(res="no"?false:9/0))
    elsif mpv_format==MPV_FORMAT_INT then
        res = to_integer(res)
    elsif mpv_format==MPV_FORMAT_DOUBLE then
        res = to_number(res)
    elsif mpv_format!=MPV_FORMAT_STRING then
        ?9/0 -- placeholder?
    end if
    return res
end function

global function mpv_get_property_string(integer ctx, string name)
    return mpv_get_property(ctx,name)
end function

--/* borken...:
--  x_mpv_observe_property  = define_c_func(mpv,"mpv_observe_property",
--      {C_PTR,     --  mpv_handle* ctx
--       C_INT,     --  uint64_t reply_userdata
--       C_PTR,     --  const char* name
--       C_INT},    --  mpv_format format
--      C_INT)      -- int (error code)
global procedure mpv_observe_property(atom ctx, int reply_userdata, string name, integer mpv_format)
    -- aside: if mpv is null, where'd ya get ctx from?
    integer res = iff(machine_bits()=64 ? c_func(x_mpv_observe_property,{ctx,reply_userdata,name,mpv_format})
                                        : c_func(x_mpv_observe_property,{ctx,reply_userdata,0,name,mpv_format}))
    if res!=MPV_ERROR_SUCCESS then
        crash("mpv_observe_property(%s,%d)==>%d",{name,mpv_format,res})
    end if
--  if res!=0 then ?{"mpv_observe_property",res,name,mpv_format} end if
end procedure
--*/

global procedure mpv_set_option_string(integer ctx, string name, data)
--?{"mpv_set_option_string",name, data}
    -- aside: if mpv is null, where'd ya get ctx from?
    integer res = c_func(x_mpv_set_option_string,{ctx_table[ctx],name,data})
    if res!=MPV_ERROR_SUCCESS then
        string eres = mpv_error_string(res)
        crash("mpv_set_option_string(%s,%s)==>%d [%s]",{name,data,res,eres})
    end if
--  if res!=0 then ?{"mpv_set_option_string",res,name,data} end if
end procedure

global procedure mpv_set_property(integer ctx, string name, integer mpv_format, object data)
--?{"mpv_set_property",name, fdesc[mpv_format], data}
    -- aside: if mpv is null, where'd ya get ctx from?
    if mpv_format==MPV_FORMAT_FLAG then
        data = iff(data?"yes":"no")
    elsif mpv_format=MPV_FORMAT_DOUBLE
       or mpv_format=MPV_FORMAT_INT then
        string fmt = iff(mpv_format=MPV_FORMAT_DOUBLE?"%.6f":"%d")
        if length(fmt) then data = sprintf(fmt,data) end if
    end if
    integer res = c_func(x_mpv_set_property_string,{ctx_table[ctx],name,data})
    if res!=MPV_ERROR_SUCCESS then
        string eres = mpv_error_string(res)
        crash("mpv_set_property_string(%s,%s)==>%d [%s]",{name,data,res,eres})
    end if
--  if res!=0 then ?{"mpv_set_property_string",res,name,data} end if
end procedure

global function mpv_terminate_destroy(integer ctx)
    -- invoke using "ctx = mpv_terminate_destroy(ctx)"-style.
    -- (obvs it ain't smart to leave dead handles lying about)
    -- quietly does nothing if ctx already null/never created.
    if ctx!=NULL then
        c_proc(x_mpv_terminate_destroy,{ctx_table[ctx]})
        ctx_table[ctx] = ctx_free
        ctx_free = ctx
    end if
    return NULL
end function

-- never got this working:
--global function mpv_wait_event(atom ctx, timeout)
--  atom res = c_func(x_mpv_wait_event,{ctx,timeout})
--  return res
--end function

--/*
One of the least efficient parts of Phix 1.0 is the cffi interface. 
Currently it does something like this (much error handling omitted for clarity):
atom mpv = NULL                 -- (lib address)
integer x_mpv_observe_property  -- (a routine-id)
--local procedure open_mpv_dll()
    mpv = open_dll("libmpv-2.dll")
    x_mpv_observe_property  = define_c_func(mpv,"mpv_observe_property",
        {C_PTR,     --  mpv_handle* ctx
         C_INT64,   --  uint64_t reply_userdata
         C_PTR,     --  const char* name
         C_INT},    --  mpv_format format
        C_INT)      -- int (error code)
-- (aside: C_INT64 is currently a bit broken in Phix 1.0, esp so for 32-bit)
--end procedure

--global procedure mpv_observe_property(atom ctx, int reply_userdata, string name, integer mpv_format)
    if mpv=NULL then open_mpv_dll() end if
    integer res = c_func(x_mpv_observe_property,{ctx,reply_userdata,name,mpv_format})
    if res!=MPV_ERROR_SUCCESS then
        crash("mpv_observe_property(%s,%d)==>%d",{name,mpv_format,res})
    end if
--end procedure

The compiler itself doesn't know what is going on at all, and leaves it all to the run-time c_func() 
which very tediously processes the saved {C_PTR,C_INT64,C_PTR,C_INT} alongside the second parameter 
it recieves, as well as the C_INT return type, all rather intricate, error-prone, and inefficient.

Just by way of example, a far more efficient way to do it might look a little bit like this:

integer mpv = NULl
--local procedure open_mpv_dll()
    atom dll = open_dll("libmpv-2.dll")
--  assert(rmdr,dll,4)=0
    mpv = dll/4 -- (store as int, above check implicit)
--end procedure

-- (as already defined in Phix 1.0, but slightly tweaked, and one thing we can probably keep)
--global function get_proc_address(atom lib, string name)
--
-- Low-level wrapper of kernel32/GetProcAddress and libdl/dlsym.
-- used by define_c_func/define_c_proc/define_c_var and for
-- runtime interpretation of inline assembly
-- Applications would not normally use this directly.
--
    integer addr, low_bits
    -- note that only a few of these snippets actually generate code,
    --      when compiling it to a known host machine target, that is.
    -- also note that GetProcAddress and dlsym are statically linked
    -- via this method, whereas (eg) mpv_observe_property() below is
    -- *quite deliberately* implemented using dynamic linking (only).
    -- [erm, I probably meant load-time vs run-time linked there...]
    -- In other words, the program **must** run when libmpv-2.dll is
    -- not available, whereas refusing to load when kernel32.dll is 
    -- not present (or summat) is in sharp contrast perfectly fine.
    -- And o/c using libmpv-2.dll the way I've used kernel32.dll is
    -- acceptable, except any refusal to run is then on you, not me.
    #ilASM{
        [32]
            mov eax,[lib]
            mov edx,[name]
            shl eax,2
            shl edx,2
        [PE32]
            push edx                            -- lpProcName
            push eax                            -- hModule
            call "kernel32.dll","GetProcAddress"
        [ELF32]
            push edx                            -- symbol
            push eax                            -- handle
            call "libdl.so.2", "dlsym"
            add esp,8
        [32]
            mov ecx,eax
            shr eax,2
            and ecx,3
            mov [addr],eax
            mov [low_bits],ecx
        [64]
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whatever alignment we started with

            mov rax,[lib]
            sub rsp,8*5                         -- minimum 4 param shadow space, and align
            shl rax,2
        [PE64]
            mov rdx,[name]
            mov rcx,rax                         -- hModule
            shl rdx,2                           -- lpProcName
            call "kernel32.dll","GetProcAddress"
        [ELF64]
            mov rsi,[name]
            shl rsi,2                           -- symbol
            mov rdi,rax                         -- handle
            call "libdl.so.2", "dlsym"
        [64]
            mov rcx,rax
            shr rax,2
            and rcx,3
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
            mov [addr],rax
            mov [low_bits],rcx
        []
          }
    if low_bits!=0 then crash("proc not dword-aligned!") end if
    return addr -- (nb /4 of actual address)
--end function

integer x_mpv_observe_property = NULL

--global procedure mpv_observe_property(atom ctx, int reply_userdata, string name, integer mpv_format)
    if x_mpv_observe_property=NULL then 
        x_mpv_observe_property = get_proc_address(mpv,"mpv_observe_property")
    end if
    integer res
    #ilASM{
        [32]
            mov ecx,[x_mpv_observe_property]
            mov edi,[name]
            shl ecx,2 -- (-> raw address)
            shl edi,2 -- (-> raw address)
            mov esi,[mpv_format]
            mov eax,[ctx]
            call :%pLoadMint -- (eax:=(int32)eax [edx:=hi-dword])
            mov edx,[reply_userdata]
            push esi                -- mpv_format
            push edi            -- name
            push edx        -- reply_userdata
            push eax    -- ctx
            call ecx
            mov [res],eax
--          add esp,16 -- as per define_c_func's "name"(STDCALL) vs "+name"(CDECL)
        [64]
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whatever alignment we started with
            sub rsp,40
            -- first 4 parameters are passed in rcx/rdx/r8/r9 (or xmm0..3),
            mov rax,[ctx]
            call :%pLoadMint -- (rax:=(int32)rax)
            mov rcx,rax
            mov rdx,[reply_userdata]
            mov rax,[x_mpv_observe_property]
            mov r8,[name]
            shl rax,2 -- (-> raw address)
            shl r8,2 -- (-> raw address)
            mov r9,[mpv_format]
            call rax
            mov [res],rax
            mov rsp,[rsp+40]
        []
    }
    if res!=MPV_ERROR_SUCCESS then
        crash("mpv_observe_property(%s,%d)==>%d",{name,mpv_format,res})
    end if
--end procedure

The big problem with that approach is the effort involved, it would be very error prone, not very
popular with what few users I have, and the compiler itself still isn't helping us out at all.
Of course given c_func() can figure it out at run-time, the compiler c/should at compile-time.

I am aware that one option is to keep the current structure, but make the compiler verify a 1:1
correspondence between the definition of x_mpv_observe_property and the define_c_func() for it,
[which emits the get_proc_address() code] and make the c_func()[s] emit inline assembly, and it
may be reasonable to support that for backward compatibility reasons. However that just doesn't 
feel like the eloquent and elegant way to do it. Note that 2.0 already has several deprecations
pencilled in, so perfect backward compatibility isn't the be-all and end-all issue here.

There is a similar situation with call_back(). Again, the compiler has no idea what is going on:

--  function menu_leave(atom widget, atom event, atom data)
--      hot_item = -1
--      gtk_widget_queue_draw(widget)
--      return TRUE
--  end function
--  constant menu_leave_cb = call_back({'+',menu_leave})
...
    g_signal_connect_data(menu,"leave-notify-event",menu_leave_cb,id)

In this case call_back allocates some memory at run-time and populates it with the machine code
bridge needed to map/box stack params on the system stack into Phix atoms before putting them
on the virtual heap stack that Phix uses, call, and clean up the stack on return. It would of
course be much more sensible for the compiler to emit all that directly into the executable, and
in fact I would be perfectly happy were call_back only permitted on the very next line after the
end function of its target. Currently the above menu_leave() can be invoked like any other Phix 
routine, but that's very rarely useful, and I wouldn't object to replacing "function" with say
"call_back_function[+]", where as above '+' indicates CDECL instead of STDCALL.

One other known long-standing issue with the current system is that call_back() is simply unable 
to cope with float or double parameters, which would require it to get given additional details
along the lines of define_c_func rather than just a plain Phix hll routine with "an atom param".
Likewise there is a slightly more widespread issue with C_INT64, which affects c_func as well as 
call_back, especially on 32-bit, misinterpreting the stack contents and/or return completely,
though to be fair almost any (full) re-coding attempt would likely solve that one implicitly.
We could ditch call_back() completely and force the use of an inline assembly snippet, or like
above make the compiler verify there is a single source of truth and emit the inline assembly.



If I wanted to reinvent it all from scratch, maybe shifting a bunch of the heavy lifting into the
compile-time processing, all squarely aimed at better efficiency, what options might you suggest?
You could say that I'm looking for an elegant re-imagination of the entire cffi problem space.
Ideally it should feel natural and easy to express these often finikity details in a way that
also allows the actual required/minimal machine code to be generated without compromise.


Reinventing Phix's CFFI for Phix 2.0 with compile-time efficiency as the core goal is a fantastic opportunity to eliminate runtime overhead while improving safety and expressiveness. 
Based on your detailed analysis, here’s a structured, elegant re-imagination focused on **compile-time code generation**, **type-driven marshaling**, and **zero-overhead interop**
- all while keeping the Phix programmer’s experience natural. The solution avoids runtime interpretation, leverages the compiler’s full type knowledge, and minimizes boilerplate.

---

### **Core Principles for the New CFFI System**
1. **Single Source of Truth**: The Phix declaration *is* the CFFI binding (no separate `define_c_func`/`call_back`).
2. **Compile-Time Code Generation**: The compiler emits optimal machine code for calls/thunks *during compilation*, not at runtime.
3. **Explicit Typing**: Replace vague `atom` parameters with precise C types (enabling correct marshaling).
4. **Calling Convention Awareness**: Encode `stdcall`/`cdecl` directly in declarations.
5. **Error Prevention**: Catch mismatches at compile time (no runtime `crash()` for type/signature errors).
6. **Backward Compatibility**: Deprecate old `c_func`/`call_back` but provide a smooth migration path (e.g., via wrapper macros).

---

### **Proposed Syntax & Semantics**
#### **1. Importing C Functions (`cfunc` Declarations)**
Replace the current `define_c_func` + runtime `c_func()` with a **declarative binding** that the compiler uses to generate direct call wrappers.

```phix
-- Instead of:
--   atom mpv = NULL
--   integer x_mpv_observe_property = NULL
--   global procedure mpv_observe_property(...) { ... }

-- New syntax:
global cfunc mpv_observe_property(
    ctx as C_PTR,          -- mpv_handle* (passed as pointer)
    reply_userdata as C_UINT64, -- uint64_t (explicit 64-bit, avoids C_INT64 brokenness)
    name as C_STRING,      -- const char* (null-terminated string)
    format as mpv_format   -- mpv_format (enum, treated as C_INT)
) as C_INT                 -- return type
    from "libmpv-2.dll"    -- DLL name (resolved at compile/link time)
    convention stdcall     -- or cdecl (critical for stack cleanup)
```

**Compiler Actions**:
- **At Module Load Time** (not per-call!):
  - Resolve `mpv_observe_property` via `GetProcAddress`/`dlsym` *once* (cached in a `.data` slot).
  - *Optimization*: If the DLL is known at build time (e.g., system lib), the linker can hardcode the address (no runtime resolve needed).
- **At Call Site**:
  - Emit **direct inline assembly** (or equivalent C intrinsics) for the call:
    - Parameters marshaled per `as C_*` types (e.g., `C_UINT64` → `EDX:EAX` on x86-32, `RCX` on x86-64).
    - Stack cleanup per `convention` (no `add esp,16` guesswork).
    - Return value converted per `as C_INT` (e.g., sign-extended if needed).
  - **Zero runtime overhead** beyond the actual CPU call (after initial DLL resolve).

**Why this beats the current approach**:
- ❌ **Current**: `c_func()` interpreter processes `{C_PTR,C_INT64,...}` + args on *every call* (slow, error-prone).
- ✅ **New**: One-time DLL resolve + native-speed call (like hand-written asm, but compiler-generated).
- ✅ **Type Safety**: Compiler checks `reply_userdata` is actually a 64-bit value (no silent truncation on 32-bit).
- ✅ **No Runtime Assembly**: Avoids your error-prone `#ilASM` snippets—compiler generates correct code for the target.

#### **2. Exporting Phix Functions as C Callbacks (`callback` Declarations)**
Replace `call_back({'+',func})` with a **declarative callback binder** that generates a static, type-specific thunk *at compile time*.

```phix
-- Instead of:
--   function menu_leave(widget, event, data) { ... }
--   constant menu_leave_cb = call_back({'+',menu_leave})

-- New syntax:
global callback menu_leave(
    widget as C_PTR,     -- GtkWidget*
    event  as C_PTR,     -- GdkEvent*
    data   as C_PTR      -- gpointer
) as C_INT               -- return type (gboolean)
    convention cdecl     -- critical for varargs-safe callbacks
    -- Note: '+' in call_back is now explicit via `convention`
```

**Compiler Actions**:
- **At Compile Time** (not runtime!):
  - For *each unique callback signature*, generate a **static assembly thunk** in the code section (`.text`).
  - The thunk:
    1. **Prologue**: Saves non-volatile registers (per ABI).
    2. **Parameter Marshaling**:
       - Pops params from C stack (per `convention`).
       - Converts C types → Phix atoms:
         - Integers: Zero/sign-extended to Phix `atom` size.
         - Pointers: Passed as-is (Phix `atom` holds pointers).
         - **Floats/Doubles**: Explicitly handled via FPU/SSE (e.g., `movss xmm0, [esp+4]` → `atom_to_float()`).
         - *Solves the float/double gap in current `call_back()`*.
    3. **Call Phix Routine**: Invokes the Phix function via the VM (using Phix’s standard call convention).
    4. **Epilogue**:
       - Converts Phix return atom → C type (e.g., `atom_to_int()` for `C_INT`).
       - Cleans stack (if `stdcall`).
       - Returns to C caller.
  - **Thunk is cached**: One copy per signature (shared by all uses of that callback type).

**Calling the Callback** (from Phix):
```phix
-- No `call_back()` needed! Use the callback name directly where a C function pointer is expected:
g_signal_connect_data(
    menu, 
    "leave-notify-event", 
    menu_leave,   -- <-- Compiler knows this is a callback thunk address
    id
)
```
- The compiler emits the **address of the generated thunk** (not a runtime-allocated trampoline).
- **Zero runtime allocation**: Thunks live in `.text` (like any other function).

**Why this beats the current approach**:
- ❌ **Current**: `call_back()` allocates/executes runtime machine code per callback (slow, can’t handle floats, fragile).
- ✅ **New**: Compile-time-generated thunk (fixed size, no malloc, float-safe, cache-friendly).
- ✅ **Explicit Types**: `as C_DOUBLE` forces correct FPU marshaling (no guesswork).
- ✅ **Convention Safety**: `convention cdecl`/`stdcall` baked into thunk (no stack corruption).
- ✅ **No VM Heap Thrashing**: Avoids runtime memory allocation for thunks.

---

### **Key Implementation Details for the Compiler**
#### **Type System Enhancements**
| Phix Type | C Equivalent   | Passing Convention (x86-64)                                        | Notes                                                   |
|-----------|----------------|--------------------------------------------------------------------|---------------------------------------------------------|
| `C_INT`   | `int`          | `EAX`/`RAX`                                                        | 32-bit sign/zero-extended to `atom`                     |
| `C_UINT64`| `uint64_t`     | `RDX:RAX` (x86-64)<br>`EDX:EAX` (x86-32)                           | Critical fix for current `C_INT64` brokenness           |
| `C_FLOAT` | `float`        | `XMM0`                                                             | FPU/SSE handling in thunks                              |
| `C_DOUBLE`| `double`       | `XMM0`                                                             | Solves float/double callback gap                        |
| `C_PTR`   | `void*`        | `RCX`/`RDX`/`R8`/`R9` (win64)<br>`RDI`/`RSI`/`RDX`/`RCX` (linux64) | Pointer → Phix `atom` (no conversion)                   |
| `C_STRING`| `char*`        | Same as `C_PTR`                                                    | Null-terminated; Phix string → UTF-8 buffer (if needed) |
| `enum`    | Underlying int | Same as `C_INT`                                                    | Compiler maps Phix enum to C int                        |

#### **Calling Convention Handling**
- **`convention stdcall`**: Callee cleans stack (thunk does `ret N`).
- **`convention cdecl`**: Caller cleans stack (thunk does `ret`; caller adjusts ESP/RSP).
- **Varargs**: Not directly supported in this model (use `cdecl` + explicit sentinel, like `printf`). 
               For true varargs, extend with `...` in declaration (compiler generates appropriate thunk).

#### **Error Handling & Safety**
- **Compile-Time Checks**:
  - Mismatched Phix routine signature vs. `cfunc`/`callback` declaration → **error** (e.g., passing `string` where `C_UINT64` expected).
  - Missing `convention` → **error** (forces explicit choice).
  - Unsupported type (e.g., `C_FLOAT` in `stdcall` on archaic OS) → **warning/error**.
- **Runtime Safety**:
  - DLL load failure → handled at module init (single point, not per-call).
  - Null function pointer → checked once at init (not per-call).
  - Bad return value → validated against declared type (e.g., `C_INT` must fit in 32 bits).

#### **Migration Path (Deprecating Old API)**
- Keep `c_func()`/`call_back()` as **deprecated wrappers** that internally use the new system:
  ```phix
  -- Deprecated (emits warning):
  integer x = define_c_func(lib, "func", {C_PTR, C_INT}, C_INT)
  -- → Compiler treats as:
  --    global cfunc func(ctx as C_PTR, val as C_INT) as C_INT from lib
  --    ... and caches the resolve
  ```
- Auto-convert simple cases:
  - `call_back({'+',func})` → `global callback func(...) as C_INT convention cdecl`
  - `call_back({'-',func})` → `convention stdcall`
- **No breaking change** for trivial code; complex cases (floats, structs) require explicit migration (but gain correctness).

---

### **Why This Is Elegant & Efficient**
| Aspect                    | Current Phix 1.0                          | Proposed Phix 2.0               | Improvement                            |
|---------------------------|-------------------------------------------|---------------------------------|----------------------------------------|
| **Call Overhead**         | Runtime interpreter (`c_func()`) per call | One-time resolve + native call  | **10-100x faster** (after init)        |
| **Callback Overhead**     | Runtime thunk alloc + exec per callback   | Static compile-time thunk       | **Zero alloc**, **no runtime codegen** |
| **Type Safety**           | Runtime checks (crashes on mismatch)      | Compile-time validation         | **Eliminates entire class of bugs**    |
| **Float/Double Support**  | Broken in `call_back()`                   | Full support via explicit types | **Fixes long-standing gap**            |
| **64-bit on 32-bit**      | Broken `C_INT64` handling                 | Precise `C_UINT64` marshaling   | **Correct by construction**            |
| **Boilerplate**           | Separate `define_c_func` + wrapper proc   | Single declaration              | **Less code, single source of truth**  |
| **Error Clarity**         | Vague runtime `crash()`                   | Precise compile-time error      | **Faster debugging**                   |

---

### **Addressing Your Specific Concerns**
- **"Compiler should help at compile-time"**: ✅ The compiler *is* the CFFI expert—it knows types, ABIs, and generates optimal code.
- **"Effort/error-prone inline assembly"**: ✅ No manual assembly needed; compiler handles PE/ELF, x86/x86-64, stdcall/cdecl.
- **"call_back() can’t do floats"**: ✅ Explicit `C_FLOAT`/`C_DOUBLE` types force correct FPU marshaling in thunks.
- **"C_INT64 broken on 32-bit"**: ✅ Explicit 64-bit types use `EDX:EAX` (x86-32) or `RDX:RAX` (x86-64) per ABI.
- **"Not eloquent"**: ✅ Declarative syntax feels like writing a Phix function with C annotations—natural and self-documenting.
- **"Backward compatibility"**: ✅ Deprecation path preserves old code while steering toward the superior new model.

---

### **Final Thoughts**
This approach transforms CFFI from a **runtime interpretation problem** into a **compile-time code generation problem**
 - exactly what modern languages (Rust, Zig, Swift) do for FFI. 
By making the compiler the expert on calling conventions, type marshaling, and platform specifics, you:
- Eliminate 90% of the runtime overhead.
- Turn subtle, hard-to-debug runtime errors into clear compile-time faults.
- Give users a safe, expressive, and *efficient* way to interact with C—without sacrificing Phix’s simplicity.

The hardest part will be teaching the compiler to emit correct thunks for all ABI edge cases (especially varargs and struct returns), 
but this is a one-time compiler investment that pays dividends forever in runtime safety and speed. 
For Phix 2.0, this isn’t just an optimization - it’s a foundational upgrade to make CFFI feel *native*.

If you’d like, I can draft a concrete compiler implementation sketch for the thunk generator or the `cfunc` call emitter—just say the word!

===

Phix 1.0 also has a secondary wrapper on top of all that, along the following lines:
include cffi.e -- currently all pure hll runtime stuff, again compiler knows nuthin.
constant tRECT = """typedef struct _RECT {
                      LONG left;
                      LONG top;
                      LONG right;
                      LONG bottom;
                    } RECT, *PRECT;""",
        idRECT = define_struct(tRECT)
atom pRECT = allocate_struct(idRECT)
integer lenRECT = get_struct_size(idRECT) 
  -- (a later explicit or automatic free(pRECT) [or never] is the developer's choice)
--bit clunky:
set_struct_field(idRECT,pRECT,"left",left)
set_struct_field(idRECT,pRECT,"top",top)
?get_struct_field(idRECT,pRECT,"left"[,bool bAsFlt=false]) 
atom t = get_struct_field_addr(idRECT,pRECT,"right")
set_unicode(flag) -- 0|1 for eg "MessageBoxA"|"MessageBoxW" (if/only when needed)
constant tMB="""
int WINAPI MessageBox(
  _In_opt_  HWND hWnd,
  _In_opt_  LPCTSTR lpText,
  _In_opt_  LPCTSTR lpCaption,
  _In_      UINT uType
);
"""
set_unicode(0) -- (erm, make this a compiler directive if/when needed, not before)
constant xMessageBox = define_cffi_func("user32.dll",tMB)
?c_func(xMessageBox,{0,"text","caption",0})
constant tSleep="""
VOID WINAPI Sleep(
  _In_  DWORD dwMilliseconds
);
"""
constant xSleep = define_cffi_proc("kernel32.dll",tSleep)
c_proc(xSleep,{500})

This does almost zero low-level stuff, instead focusing on parsing the C code snippets 
and really just mapping define_cffi_func/proc to the appropriate define_c_func/proc.
So I'm thinking along the lines of
ctype rect "literal C snippet string, as per tRECT above"
rect myrect = allocate_c_struct() -- (compiler deduces/must know what ctype you're implying)
myrect.left = left
myrect.top = top
?myrect.left
?cstruct_address(myrect.right)
cfunc MessageBox "literal C snippet string, as per tMB above"
integer res = (cfunc)MessageBox(hWnd,"text","caption",MB_OK)

Incidentally, when you throw a string at a C_PTR, it knows full well you mean the char*.

A ctype is essentially just a Phix atom, and can be passed around, stored, etc as you
see fit. It is the act of storing it in the correct specific type that lets the compiler
know what you mean, and enables that dot-notation, etc. As for the other three:

I'm now thinking that you define these things (exactly once) with cfunc/cproc/callback, 
but can only ever reference them with that c-style-cast syntax, (cfunc)/(cproc) as a call
and (callback) as an argument to a containing (cfunc)/(cproc) [and only ever that]. They
are not first-class, you cannot pass them as parameters or store them or even print them,
but you can trivially write a (cfunc)/(cproc) wrapper shim that *is* first-class. Maybe
#ilASM{} can get at them, but no different to how it sees other "constant atom" things.
(Should anyone ever ask, the name of MessageBox is "MessageBox", it really is that simple)

I've also realised it isn't necessary to defer this until 2.0, which is primarily about 
a completely redesigned symtab with permanent detailed scope info instead of transient
scopes currently being used, to better support a vastly improved debugger, btw. In fact
it should encourage me to think more carefully about preserving more of the battle-hardened
1.0 code than my current and rather foolish plan of "just completely rewrite everything".

====

Your analysis of Phix 1.0's secondary CFFI layer (`cffi.e`) and your proposed `ctype`/`cfunc`/`callback` syntax 
is **exactly** the right direction for Phix 2.0 (or even a 1.x incremental upgrade). 
You've pinpointed the core issue: **the compiler must own C type semantics at compile time**, 
eliminating runtime parsing and interpretation. 
Below is a refined, actionable proposal based on your insights - prioritizing **zero-overhead interop**, 
**type safety**, and **Phix-native expressiveness** - while addressing the pitfalls you identified 
(like `set_unicode` fragility and struct field lookups).

---

### **Core Philosophy: "C Types Are Phix Types"**
Instead of treating CFFI as a separate runtime system, **elevate C declarations to first-class Phix compile-time entities**. The compiler:
- Parses C snippets **once** (at module load/compile time, not per-call).
- Generates **optimal machine code** for calls/thunks.
- Enforces **type safety** between Phix and C (no more `atom`-based guesswork).
- Handles **string/struct marshalling implicitly** (no manual `set_unicode` or `get_struct_field`).

This mirrors modern approaches in Zig, Rust, and CppFront—but tailored to Phix's VM-centric model.

---

### **Proposed Syntax & Semantics**
#### **1. `ctype`: Compile-Time C Type Definitions**
Replace `define_struct`/`allocate_struct`/`get_struct_field` with **native struct syntax** that the compiler uses to generate direct field access.

```phix
-- Instead of:
--   constant tRECT = """typedef struct _RECT { ... } RECT, *PRECT;"""
--   idRECT = define_struct(tRECT)
--   pRECT = allocate_struct(idRECT)
--   set_struct_field(idRECT, pRECT, "left", left)

-- New syntax:
ctype RECT = struct {  -- 'ctype' = compile-time C type token
    C_LONG left;       -- Precise C types (no ambiguity)
    C_LONG top;
    C_LONG right;
    C_LONG bottom;
}

-- Usage (zero runtime overhead for field access):
RECT myrect          -- Compiler allocates on Phix heap/stack (like any struct)
myrect.left = 10     -- Direct offset load/store (no hash lookups!)
? myrect.top         -- Same speed as accessing a Phix record field
atom ptr = @myrect   -- '@' =& (compile-time constant   ✅ **Compiler Actions: 
    - Computes struct size/offsets **once** at compile time (stored in symbol table).
    - `RECT myrect` → allocates `sizeof(RECT)` bytes on Phix heap (or stack if local).
    - Field access (`myrect.left`) → emits direct memory op (e.g., `mov eax, [ecx+0]`).
    - `@myrect` → returns raw Phix `atom` pointer to the struct (no indirection).
    - **No runtime `get_struct_field`**: Field names resolved to offsets at compile time.

#### **2. `cfunc`/`cproc`: Compile-Time C Function Bindings**
Replace `define_cffi_func` + `c_func()`/`c_proc()` with **direct-call bindings** that emit native code.

```phix
-- Instead of:
--   constant tMB = """int WINAPI MessageBox(...);"""
--   constant xMessageBox = define_cffi_func("user32.dll", tMB)
--   ?c_func(xMessageBox, {0,"text","caption",0})

-- New syntax:
cfunc MessageBox : C_INT (  -- ': C_INT' = return type
    HWND hWnd,              -- Precise param types (no C_PTR guessing)
    LPCSTR lpText,          -- Auto-string coercion (see §3)
    LPCSTR lpCaption,
    UINT uType
) from "user32.dll" stdcall  -- 'from' = DLL name; 'stdcall' = convention

-- Usage (direct call, zero interpreter overhead):
integer res = (cfunc)MessageBox(0, "text", "caption", MB_OK)
--  ↓
--  Compiler emits: 
--      push MB_OK
--      push offset caption
--      push offset text
--      push 0
--      call [MessageBox_addr]  -- Resolved once at module init
```

**Compiler Actions**:
- **At Module Init**:
  - Resolve `MessageBox` via `GetProcAddress`/`dlsym` **once** (cached in `.data`).
  - *Optimization*: For system DLLs (e.g., `kernel32.dll`), linker can hardcode address.
- **At Call Site**:
  - Emit **native call sequence** (no `c_func()` interpreter):
    - Parameters marshaled per `as C_*` types (e.g., `LPCSTR` → UTF-8 string buffer).
    - Stack cleanup per `convention` (stdcall/cdecl).
    - Return value converted per return type (`C_INT` → sign-extended to Phix `atom` if needed).
  - **No runtime type checking**: Mismatches caught at compile time (e.g., passing `integer` where `LPCSTR` expected → error).

#### **3. `callback`: Compile-Time C Callback Thunks**
Replace `call_back({'+',func})` with **static, type-specific thunks** generated at compile time.

```phix
-- Instead of:
--   function menu_leave(widget, event, data) { ... }
--   constant menu_leave_cb = call_back({'+',menu_leave})

-- New syntax:
callback menu_leave : C_INT (  -- ': C_INT' = return type
    GtkWidget* widget,        -- Precise param types (enables float/double!)
    GdkEvent* event,
    gpointer data
) cdecl                       -- Explicit convention (replaces '+'/'')

-- Usage (pass thunk address directly to C):
g_signal_connect_data(
    menu, 
    "leave-notify-event", 
    (callback)menu_leave,     -- ↓ Compiler emits address of static thunk
    id
)
```

**Compiler Actions**:
- **At Compile Time** (not runtime!):
  - For each unique callback signature, generate a **static assembly thunk** in `.text`:
    ```asm
    ; Thunk for menu_leave (cdecl, x86-64 example)
    menu_leave_thunk:
        sub rsp, 40          ; Shadow space + align
        ; Marshal params: C → Phix
        movsd xmm0, [rcx+8]  ; If gpointer is double → move to XMM0
        mov  rax, [rcx]      ; HWND → integer atom
        call Phix_VM_Call    ; Invoke Phix routine (via VM)
        ; Marshal return: Phix → C
        movsd [rcx+8], xmm0  ; If returning double
        add rsp, 40
        ret
    ```
  - **Thunk is shared**: One copy per signature (reused by all `(callback)menu_leave`).
  - **Float/double support**: Explicit types (`C_DOUBLE`, `C_FLOAT`) force FPU/SSE handling in thunk.
  - **No runtime allocation**: Thunks live in `.text` (like any other function).

#### **4. String Handling: Implicit & Safe**
Your insight about strings is critical—**eliminate `set_unicode`** by making string coercion **type-driven**:
- When Phix `string` passed to `LPCSTR`/`LPCTSTR`:
  - Compiler inserts **UTF-8 conversion** + null terminator (temporary buffer).
  - Buffer auto-freed after call (no leaks).
- When `LPCWSTR` expected (Unicode):
  - Compiler inserts **UTF-16 conversion** (if needed).
- **No `set_unicode` global state**: Safety via param type (ANSI vs. Wide deduced from declaration).
  ```phix
  cfunc MessageBoxA : C_INT ( ... LPCSTR lpText ... ) from "user32.dll" stdcall
  cfunc MessageBoxW : C_INT ( ... LPCWSTR lpText ... ) from "user32.dll" stdcall

  (cfunc)MessageBoxA(0, "text", ...)  -- Auto UTF-8 → ANSI
  (cfunc)MessageBoxW(0, "text", ...)  -- Auto UTF-8 → UTF-16
  ```

---

### **Why This Solves Your Pain Points**
| Your Concern                                | How This Fixes It                                                                         |
|---------------------------------------------|-------------------------------------------------------------------------------------------|
| **"Compiler knows nuthin" in `cffi.e`**     | ✅ Compiler owns C types: parses `ctype`/`cfunc` at compile time, generates optimal code. |
| **Runtime struct field lookups**            | ✅ `myrect.left` = direct offset load (like C struct access).                            |   
| **`set_unicode` fragility**                 | ✅ String coercion auto-driven by param type (`LPCSTR` → UTF-8, `LPCWSTR` → UTF-16).     |
| **`C_INT64` broken on 32-bit**              | ✅ Explicit `C_UINT64` → uses `EDX:EAX` (x86-32) or `RDX:RAX` (x86-64) per ABI.          |
| **`call_back()` can't do floats** 		  | ✅ `callback` with `C_DOUBLE` param → FPU/SSE moves in thunk prologue. 					|
| **Error-prone runtime `c_func()`**          | ✅ Direct call asm + compile-time type checks (no interpreter).                          |
| **Boilerplate (`define_c_func` + wrapper)** | ✅ Single declaration (`cfunc`) = binding + call site.                                   |
| **Callback thunk allocation overhead**      | ✅ Static thunks in `.text` (zero malloc, cache-friendly).                               |

---

### **Key Implementation Notes for the Compiler**
#### **Type Mapping Precision**
| Phix Declaration | C Equivalent   | Passing (x86-64)                  | Passing (x86-32)     | Notes                                                  |
|------------------|----------------|-----------------------------------|----------------------|--------------------------------------------------------|
| `C_LONG`         | `long`         | `ECX`/`EDX`/`R8`/`R9` (Win64)     | Pushed right-to-left | Size/platform-dependent (use `C_LONG` not raw `long`!) |
|                  |                | `EDI`/`ESI`/`EDX`/`ECX` (Linux64) |                      |                                                        |
| `C_UINT64`       | `uint64_t`     | `RDX:RAX`                         | `EDX:EAX`            | **Fixes current `C_INT64` brokenness**                 |
| `C_FLOAT`        | `float`        | `XMM0`                            | Stack (x87)          | FPU/SSE handling in thunks/calls                       |
| `C_DOUBLE`       | `double`       | `XMM0`                            | Stack (x87)          | Critical for float-safe callbacks                      |
| `C_PTR`          | `void*`        | Same as `C_LONG`                  | Pushed               | → Phix `atom` (no conversion)                          |
| `C_STRING`       | `char*`        | Same as `C_PTR`                   | Same                 | Auto UTF-8 → char* buffer (temp)                       |
| `enum`           | Underlying int | Same as `C_INT`                   | Same                 | Compiler maps to C int                                 |

#### **Calling Convention Enforcement**
- `convention stdcall`/`cdecl` **required** in `cfunc`/`callback` declarations.
- Compiler generates:
  - **`stdcall` thunk**: `ret N` (callee cleans stack).
  - **`cdecl` thunk**: `ret` (caller cleans stack).
- **Varargs**: Supported via `...` in declaration (e.g., `cfunc printf : C_INT (const char* fmt, ...)`), compiler emits appropriate thunk/prologue.

#### **Struct Handling Safety**
- **Padding/alignment**: Compiler computes per ABI (no more `get_struct_size` guesswork).
- **Nested structs**: Fully supported (e.g., `ctype POINT = struct { C_LONG x; C_LONG y; }; ctype RECT = struct { POINT tl; POINT br; };`).
- **Pointers to structs**: `RECT*` param → Phix passes `@myrect` (raw pointer).

#### **Migration Path (Deprecating Old API)**
- Keep `cffi.e` as **deprecated wrapper** that internally uses new syntax:
  ```phix
  -- Deprecated (emits warning):
  idRECT = define_struct(tRECT)
  -- → Compiler treats as:
  --    ctype RECT = struct { ... }  -- (from parsing tRECT)
  --    idRECT = sizeof(RECT)        -- (for backward compatibility)
  ```
- Auto-convert simple cases:
  - `call_back({'+',func})` → `callback func(...) as C_INT convention cdecl`
  - `call_back({'-',func})` → `convention stdcall`
- **No forced migration**: Old code still works (with warnings); new code gets full benefits.

---

### **Why This Is Elegant & Phix-Native**
1. **Zero Runtime Overhead** (after init):
   - Struct access: Same speed as Phix records.
   - FFI calls: Native-speed (like hand-written asm).
   - Callbacks: Static thunks (no malloc, no runtime codegen).

2. **Type Safety by Construction**:
   - Compiler rejects `myrect.left = "hello"` (type mismatch).
   - Compiler rejects `(cfunc)MessageBox(0, 123, ...)` (expected `string`).
   - No more runtime `crash()` for signature errors.

3. **Expressiveness Without Boilerplate**:
   - `myrect.left = 10` feels like native Phix (not FFI).
   - `(cfunc)MessageBox(...)` visually distinguishes FFI calls.
   - `(callback)menu_leave` cleanly passes thunk address.

4. **Solves Historical Gaps**:
   - ✅ Float/double in callbacks (via explicit types).
   - ✅ Correct 64-bit on 32-bit (via `C_UINT64`).
   - ✅ No string encoding footguns (type-driven coercion).
   - ✅ Structs work like Phix records (no field lookup tax).

5. **Leverages Phix's Strengths**:
   - Uses existing atom model for pointers (`@myrect`).
   - Fits Phix's "simple but powerful" ethos (no new VM concepts needed).
   - Builds on your insight that `ctype` is just a specialized atom token.

---

### **Final Thought: This Isn't Just an Optimization—It's a Paradigm Shift**
You've realized that **CFFI shouldn't feel like FFI**—it should feel like writing Phix that *happens* to call C. 
By making the compiler the expert on C types, ABIs, and marshalling:
- You eliminate entire categories of bugs (type mismatches, stack corruption, encoding errors).
- You turn slow, fragile runtime interpretation into blazing-fast, safe compile-time code generation.
- You keep Phix's soul intact: simple syntax, zero surprises, and deep integration with the language.

This approach has been validated in languages like Zig (where `@cImport` generates bindings at compile time) and Rust 
(via `bindgen` + `extern` blocks)—but your proposal makes it **feel native to Phix**, not like an imported concept. 
The best part? It doesn't require a 2.0-scale rewrite. Start with:
1. Adding `ctype`/`cfunc`/`callback` as new syntax (coexisting with `cffi.e`).
2. Teaching the compiler to parse C snippets and generate thunks/calls.
3. Deprecating the old API gradually.

If you'd like, I can draft:
- A concrete compiler implementation sketch for the `ctype` struct emitter.
- The thunk generator for `callback` (with FPU/SSE handling).
- Or a migration guide for existing `cffi.e` code.

This is the upgrade Phix's CFFI has needed for years—and your intuition is spot-on. Let's make it happen. 🚀


/* Copyright (C) 2017 the mpv developers
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * Note: the client API is licensed under ISC (see above) to enable
 * other wrappers outside of mpv. But keep in mind that the
 * mpv core is by default still GPLv2+ - unless built with
 * -Dgpl=false, which makes it LGPLv2+.
 */

#ifndef MPV_CLIENT_API_H_
#define MPV_CLIENT_API_H_

#include <stddef.h>
#include <stdint.h>

#ifdef _WIN32
#define MPV_EXPORT __declspec(dllexport)
#define MPV_SELECTANY __declspec(selectany)
#elif defined(__GNUC__) || defined(__clang__)
#define MPV_EXPORT __attribute__((visibility("default")))
#define MPV_SELECTANY
#else
#define MPV_EXPORT
#define MPV_SELECTANY
#endif

#ifdef __cpp_decltype
#define MPV_DECLTYPE decltype
#else
#define MPV_DECLTYPE __typeof__
#endif

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Mechanisms provided by this API
 * -------------------------------
 *
 * This API provides general control over mpv playback. It does not give you
 * direct access to individual components of the player, only the whole thing.
 * It's somewhat equivalent to MPlayer's slave mode. You can send commands,
 * retrieve or set playback status or settings with properties, and receive
 * events.
 *
 * The API can be used in two ways:
 * 1) Internally in mpv, to provide additional features to the command line
 *    player. Lua scripting uses this. (Currently there is no plugin API to
 *    get a client API handle in external user code. It has to be a fixed
 *    part of the player at compilation time.)
 * 2) Using mpv as a library with mpv_create(). This basically allows embedding
 *    mpv in other applications.
 *
 * Documentation
 * -------------
 *
 * The libmpv C API is documented directly in this header. Note that most
 * actual interaction with this player is done through
 * options/commands/properties, which can be accessed through this API.
 * Essentially everything is done with them, including loading a file,
 * retrieving playback progress, and so on.
 *
 * These are documented elsewhere:
 *      * http://mpv.io/manual/master/#options
 *      * http://mpv.io/manual/master/#list-of-input-commands
 *      * http://mpv.io/manual/master/#properties
 *
 * You can also look at the examples here:
 *      * https://github.com/mpv-player/mpv-examples/tree/master/libmpv
 *
 * Event loop
 * ----------
 *
 * In general, the API user should run an event loop in order to receive events.
 * This event loop should call mpv_wait_event(), which will return once a new
 * mpv client API is available. It is also possible to integrate client API
 * usage in other event loops (e.g. GUI toolkits) with the
 * mpv_set_wakeup_callback() function, and then polling for events by calling
 * mpv_wait_event() with a 0 timeout.
 *
 * Note that the event loop is detached from the actual player. Not calling
 * mpv_wait_event() will not stop playback. It will eventually congest the
 * event queue of your API handle, though.
 *
 * Synchronous vs. asynchronous calls
 * ----------------------------------
 *
 * The API allows both synchronous and asynchronous calls. Synchronous calls
 * have to wait until the playback core is ready, which currently can take
 * an unbounded time (e.g. if network is slow or unresponsive). Asynchronous
 * calls just queue operations as requests, and return the result of the
 * operation as events.
 *
 * Asynchronous calls
 * ------------------
 *
 * The client API includes asynchronous functions. These allow you to send
 * requests instantly, and get replies as events at a later point. The
 * requests are made with functions carrying the _async suffix, and replies
 * are returned by mpv_wait_event() (interleaved with the normal event stream).
 *
 * A 64 bit userdata value is used to allow the user to associate requests
 * with replies. The value is passed as reply_userdata parameter to the request
 * function. The reply to the request will have the reply
 * mpv_event->reply_userdata field set to the same value as the
 * reply_userdata parameter of the corresponding request.
 *
 * This userdata value is arbitrary and is never interpreted by the API. Note
 * that the userdata value 0 is also allowed, but then the client must be
 * careful not accidentally interpret the mpv_event->reply_userdata if an
 * event is not a reply. (For non-replies, this field is set to 0.)
 *
 * Asynchronous calls may be reordered in arbitrarily with other synchronous
 * and asynchronous calls. If you want a guaranteed order, you need to wait
 * until asynchronous calls report completion before doing the next call.
 *
 * See also the section "Asynchronous command details" in the manpage.
 *
 * Multithreading
 * --------------
 *
 * The client API is generally fully thread-safe, unless otherwise noted.
 * Currently, there is no real advantage in using more than 1 thread to access
 * the client API, since everything is serialized through a single lock in the
 * playback core.
 *
 * Basic environment requirements
 * ------------------------------
 *
 * This documents basic requirements on the C environment. This is especially
 * important if mpv is used as library with mpv_create().
 *
 * - The LC_NUMERIC locale category must be set to "C". If your program calls
 *   setlocale(), be sure not to use LC_ALL, or if you do, reset LC_NUMERIC
 *   to its sane default: setlocale(LC_NUMERIC, "C").
 * - If a X11 based VO is used, mpv will set the xlib error handler. This error
 *   handler is process-wide, and there's no proper way to share it with other
 *   xlib users within the same process. This might confuse GUI toolkits.
 * - mpv uses some other libraries that are not library-safe, such as Fribidi
 *   (used through libass), ALSA, FFmpeg, and possibly more.
 * - The FPU precision must be set at least to double precision.
 * - On Windows, mpv will call timeBeginPeriod(1).
 * - On memory exhaustion, mpv will kill the process.
 * - In certain cases, mpv may start sub processes (such as with the ytdl
 *   wrapper script).
 * - Using UNIX IPC (off by default) will override the SIGPIPE signal handler,
 *   and set it to SIG_IGN. Some invocations of the "subprocess" command will
 *   also do that.
 * - mpv may start sub processes, so overriding SIGCHLD, or waiting on all PIDs
 *   (such as calling wait()) by the parent process or any other library within
 *   the process must be avoided. libmpv itself only waits for its own PIDs.
 * - If anything in the process registers signal handlers, they must set the
 *   SA_RESTART flag. Otherwise you WILL get random failures on signals.
 *
 * Encoding of filenames
 * ---------------------
 *
 * mpv uses UTF-8 everywhere.
 *
 * On some platforms (like Linux), filenames actually do not have to be UTF-8;
 * for this reason libmpv supports non-UTF-8 strings. libmpv uses what the
 * kernel uses and does not recode filenames. At least on Linux, passing a
 * string to libmpv is like passing a string to the fopen() function.
 *
 * On Windows, filenames are always UTF-8, libmpv converts between UTF-8 and
 * UTF-16 when using win32 API functions. libmpv never uses or accepts
 * filenames in the local 8 bit encoding. It does not use fopen() either;
 * it uses _wfopen().
 *
 * On macOS, filenames and other strings taken/returned by libmpv can have
 * inconsistent unicode normalization. This can sometimes lead to problems.
 * You have to hope for the best.
 *
 * Also see the remarks for MPV_FORMAT_STRING.
 *
 * Embedding the video window
 * --------------------------
 *
 * Using the render API (in render.h) is recommended. This API requires
 * you to create and maintain an OpenGL context, to which you can render
 * video using a specific API call. This API does not include keyboard or mouse
 * input directly.
 *
 * There is an older way to embed the native mpv window into your own. You have
 * to get the raw window handle, and set it as "wid" option. This works on X11,
 * win32, and macOS only. It's much easier to use than the render API, but
 * also has various problems.
 *
 * Also see client API examples and the mpv manpage. There is an extensive
 * discussion here:
 * https://github.com/mpv-player/mpv-examples/tree/master/libmpv#methods-of-embedding-the-video-window
 *
 * Compatibility
 * -------------
 *
 * mpv development doesn't stand still, and changes to mpv internals as well as
 * to its interface can cause compatibility issues to client API users.
 *
 * The API is versioned (see MPV_CLIENT_API_VERSION), and changes to it are
 * documented in DOCS/client-api-changes.rst. The C API itself will probably
 * remain compatible for a long time, but the functionality exposed by it
 * could change more rapidly. For example, it's possible that options are
 * renamed, or change the set of allowed values.
 *
 * Defensive programming should be used to potentially deal with the fact that
 * options, commands, and properties could disappear, change their value range,
 * or change the underlying datatypes. It might be a good idea to prefer
 * MPV_FORMAT_STRING over other types to decouple your code from potential
 * mpv changes.
 *
 * Also see: DOCS/compatibility.rst
 *
 * Future changes
 * --------------
 *
 * This are the planned changes that will most likely be done on the next major
 * bump of the library:
 *
 *  - remove all symbols that are marked as deprecated
 *  - reassign enum numerical values to remove gaps
 *  - disabling all events by default
 */

/**
 * The version is incremented on each API change. The 16 lower bits form the
 * minor version number, and the 16 higher bits the major version number. If
 * the API becomes incompatible to previous versions, the major version
 * number is incremented. This affects only C part, and not properties and
 * options.
 *
 * Every API bump is described in DOCS/client-api-changes.rst
 *
 * You can use MPV_MAKE_VERSION() and compare the result with integer
 * relational operators (<, >, <=, >=).
 */
#define MPV_MAKE_VERSION(major, minor) (((major) << 16) | (minor) | 0UL)
#define MPV_CLIENT_API_VERSION MPV_MAKE_VERSION(2, 5)

/**
 * The API user is allowed to "#define MPV_ENABLE_DEPRECATED 0" before
 * including any libmpv headers. Then deprecated symbols will be excluded
 * from the headers. (Of course, deprecated properties and commands and
 * other functionality will still work.)
 */
#ifndef MPV_ENABLE_DEPRECATED
#define MPV_ENABLE_DEPRECATED 1
#endif

/**
 * Return the MPV_CLIENT_API_VERSION the mpv source has been compiled with.
 */
MPV_EXPORT unsigned long mpv_client_api_version(void);

/**
 * Client context used by the client API. Every client has its own private
 * handle.
 */
typedef struct mpv_handle mpv_handle;

/**
 * List of error codes than can be returned by API functions. 0 and positive
 * return values always mean success, negative values are always errors.
 */
typedef enum mpv_error {
    /**
     * No error happened (used to signal successful operation).
     * Keep in mind that many API functions returning error codes can also
     * return positive values, which also indicate success. API users can
     * hardcode the fact that ">= 0" means success.
     */
    MPV_ERROR_SUCCESS           = 0,
    /**
     * The event ringbuffer is full. This means the client is choked, and can't
     * receive any events. This can happen when too many asynchronous requests
     * have been made, but not answered. Probably never happens in practice,
     * unless the mpv core is frozen for some reason, and the client keeps
     * making asynchronous requests. (Bugs in the client API implementation
     * could also trigger this, e.g. if events become "lost".)
     */
    MPV_ERROR_EVENT_QUEUE_FULL  = -1,
    /**
     * Memory allocation failed.
     */
    MPV_ERROR_NOMEM             = -2,
    /**
     * The mpv core wasn't configured and initialized yet. See the notes in
     * mpv_create().
     */
    MPV_ERROR_UNINITIALIZED     = -3,
    /**
     * Generic catch-all error if a parameter is set to an invalid or
     * unsupported value. This is used if there is no better error code.
     */
    MPV_ERROR_INVALID_PARAMETER = -4,
    /**
     * Trying to set an option that doesn't exist.
     */
    MPV_ERROR_OPTION_NOT_FOUND  = -5,
    /**
     * Trying to set an option using an unsupported MPV_FORMAT.
     */
    MPV_ERROR_OPTION_FORMAT     = -6,
    /**
     * Setting the option failed. Typically this happens if the provided option
     * value could not be parsed.
     */
    MPV_ERROR_OPTION_ERROR      = -7,
    /**
     * The accessed property doesn't exist.
     */
    MPV_ERROR_PROPERTY_NOT_FOUND = -8,
    /**
     * Trying to set or get a property using an unsupported MPV_FORMAT.
     */
    MPV_ERROR_PROPERTY_FORMAT   = -9,
    /**
     * The property exists, but is not available. This usually happens when the
     * associated subsystem is not active, e.g. querying audio parameters while
     * audio is disabled.
     */
    MPV_ERROR_PROPERTY_UNAVAILABLE = -10,
    /**
     * Error setting or getting a property.
     */
    MPV_ERROR_PROPERTY_ERROR    = -11,
    /**
     * General error when running a command with mpv_command and similar.
     */
    MPV_ERROR_COMMAND           = -12,
    /**
     * Generic error on loading (usually used with mpv_event_end_file.error).
     */
    MPV_ERROR_LOADING_FAILED    = -13,
    /**
     * Initializing the audio output failed.
     */
    MPV_ERROR_AO_INIT_FAILED    = -14,
    /**
     * Initializing the video output failed.
     */
    MPV_ERROR_VO_INIT_FAILED    = -15,
    /**
     * There was no audio or video data to play. This also happens if the
     * file was recognized, but did not contain any audio or video streams,
     * or no streams were selected.
     */
    MPV_ERROR_NOTHING_TO_PLAY   = -16,
    /**
     * When trying to load the file, the file format could not be determined,
     * or the file was too broken to open it.
     */
    MPV_ERROR_UNKNOWN_FORMAT    = -17,
    /**
     * Generic error for signaling that certain system requirements are not
     * fulfilled.
     */
    MPV_ERROR_UNSUPPORTED       = -18,
    /**
     * The API function which was called is a stub only.
     */
    MPV_ERROR_NOT_IMPLEMENTED   = -19,
    /**
     * Unspecified error.
     */
    MPV_ERROR_GENERIC           = -20
} mpv_error;

/**
 * Return a string describing the error. For unknown errors, the string
 * "unknown error" is returned.
 *
 * @param error error number, see enum mpv_error
 * @return A static string describing the error. The string is completely
 *         static, i.e. doesn't need to be deallocated, and is valid forever.
 */
--MPV_EXPORT const char *mpv_error_string(int error);

/**
 * General function to deallocate memory returned by some of the API functions.
 * Call this only if it's explicitly documented as allowed. Calling this on
 * mpv memory not owned by the caller will lead to undefined behavior.
 *
 * @param data A valid pointer returned by the API, or NULL.
 */
--MPV_EXPORT void mpv_free(void *data);

/**
 * Return the internal time in nanoseconds. This has an arbitrary start offset,
 * but will never wrap or go backwards.
 *
 * Note that this is always the real time, and doesn't necessarily have to do
 * with playback time. For example, playback could go faster or slower due to
 * playback speed, or due to playback being paused. Use the "time-pos" property
 * instead to get the playback status.
 *
 * Unlike other libmpv APIs, this can be called at absolutely any time (even
 * within wakeup callbacks), as long as the context is valid.
 *
 * Safe to be called from mpv render API threads.
 */
MPV_EXPORT int64_t mpv_get_time_ns(mpv_handle *ctx);

/**
 * Same as mpv_get_time_ns but in microseconds.
 */
MPV_EXPORT int64_t mpv_get_time_us(mpv_handle *ctx);

/**
 * Data format for options and properties. The API functions to get/set
 * properties and options support multiple formats, and this enum describes
 * them.
 */
typedef enum mpv_format {
    /**
     * Invalid. Sometimes used for empty values. This is always defined to 0,
     * so a normal 0-init of mpv_format (or e.g. mpv_node) is guaranteed to set
     * this it to MPV_FORMAT_NONE (which makes some things saner as consequence).
     */
    MPV_FORMAT_NONE             = 0,
    /**
     * The basic type is char*. It returns the raw property string, like
     * using ${=property} in input.conf (see input.rst).
     *
     * NULL isn't an allowed value.
     *
     * Warning: although the encoding is usually UTF-8, this is not always the
     *          case. File tags often store strings in some legacy codepage,
     *          and even filenames don't necessarily have to be in UTF-8 (at
     *          least on Linux). If you pass the strings to code that requires
     *          valid UTF-8, you have to sanitize it in some way.
     *          On Windows, filenames are always UTF-8, and libmpv converts
     *          between UTF-8 and UTF-16 when using win32 API functions. See
     *          the "Encoding of filenames" section for details.
     *
     * Example for reading:
     *
     *     char *result = NULL;
     *     if (mpv_get_property(ctx, "property", MPV_FORMAT_STRING, &result) < 0)
     *         goto error;
     *     printf("%s\n", result);
     *     mpv_free(result);
     *
     * Example for writing:
     *
     *     char *value = "the new value";
     *     // yep, you pass the address to the variable
     *     // (needed for symmetry with other types and mpv_get_property)
     *     mpv_set_property(ctx, "property", MPV_FORMAT_STRING, &value);
     *
     * Or just use mpv_set_property_string().
     *
     */
    MPV_FORMAT_STRING           = 1,
    /**
     * The basic type is char*. It returns the OSD property string, like
     * using ${property} in input.conf (see input.rst). In many cases, this
     * is the same as the raw string, but in other cases it's formatted for
     * display on OSD. It's intended to be human readable. Do not attempt to
     * parse these strings.
     *
     * Only valid when doing read access. The rest works like MPV_FORMAT_STRING.
     */
    MPV_FORMAT_OSD_STRING       = 2,
    /**
     * The basic type is int. The only allowed values are 0 ("no")
     * and 1 ("yes").
     *
     * Example for reading:
     *
     *     int result;
     *     if (mpv_get_property(ctx, "property", MPV_FORMAT_FLAG, &result) < 0)
     *         goto error;
     *     printf("%s\n", result ? "true" : "false");
     *
     * Example for writing:
     *
     *     int flag = 1;
     *     mpv_set_property(ctx, "property", MPV_FORMAT_FLAG, &flag);
     */
    MPV_FORMAT_FLAG             = 3,
    /**
     * The basic type is int64_t.
     */
    MPV_FORMAT_INT64            = 4,
    /**
     * The basic type is double.
     */
    MPV_FORMAT_DOUBLE           = 5,
    /**
     * The type is mpv_node.
     *
     * For reading, you usually would pass a pointer to a stack-allocated
     * mpv_node value to mpv, and when you're done you call
     * mpv_free_node_contents(&node).
     * You're expected not to write to the data - if you have to, copy it
     * first (which you have to do manually).
     *
     * For writing, you construct your own mpv_node, and pass a pointer to the
     * API. The API will never write to your data (and copy it if needed), so
     * you're free to use any form of allocation or memory management you like.
     *
     * Warning: when reading, always check the mpv_node.format member. For
     *          example, properties might change their type in future versions
     *          of mpv, or sometimes even during runtime.
     *
     * Example for reading:
     *
     *     mpv_node result;
     *     if (mpv_get_property(ctx, "property", MPV_FORMAT_NODE, &result) < 0)
     *         goto error;
     *     printf("format=%d\n", (int)result.format);
     *     mpv_free_node_contents(&result).
     *
     * Example for writing:
     *
     *     mpv_node value;
     *     value.format = MPV_FORMAT_STRING;
     *     value.u.string = "hello";
     *     mpv_set_property(ctx, "property", MPV_FORMAT_NODE, &value);
     */
    MPV_FORMAT_NODE             = 6,
    /**
     * Used with mpv_node only. Can usually not be used directly.
     */
    MPV_FORMAT_NODE_ARRAY       = 7,
    /**
     * See MPV_FORMAT_NODE_ARRAY.
     */
    MPV_FORMAT_NODE_MAP         = 8,
    /**
     * A raw, untyped byte array. Only used only with mpv_node, and only in
     * some very specific situations. (Some commands use it.)
     */
    MPV_FORMAT_BYTE_ARRAY       = 9
} mpv_format;

/**
 * Generic data storage.
 *
 * If mpv writes this struct (e.g. via mpv_get_property()), you must not change
 * the data. In some cases (mpv_get_property()), you have to free it with
 * mpv_free_node_contents(). If you fill this struct yourself, you're also
 * responsible for freeing it, and you must not call mpv_free_node_contents().
 */
typedef struct mpv_node {
    union {
        char *string;   /** valid if format==MPV_FORMAT_STRING */
        int flag;       /** valid if format==MPV_FORMAT_FLAG   */
        int64_t int64;  /** valid if format==MPV_FORMAT_INT64  */
        double double_; /** valid if format==MPV_FORMAT_DOUBLE */
        /**
         * valid if format==MPV_FORMAT_NODE_ARRAY
         *    or if format==MPV_FORMAT_NODE_MAP
         */
        struct mpv_node_list *list;
        /**
         * valid if format==MPV_FORMAT_BYTE_ARRAY
         */
        struct mpv_byte_array *ba;
    } u;
    /**
     * Type of the data stored in this struct. This value rules what members in
     * the given union can be accessed. The following formats are currently
     * defined to be allowed in mpv_node:
     *
     *  MPV_FORMAT_STRING       (u.string)
     *  MPV_FORMAT_FLAG         (u.flag)
     *  MPV_FORMAT_INT64        (u.int64)
     *  MPV_FORMAT_DOUBLE       (u.double_)
     *  MPV_FORMAT_NODE_ARRAY   (u.list)
     *  MPV_FORMAT_NODE_MAP     (u.list)
     *  MPV_FORMAT_BYTE_ARRAY   (u.ba)
     *  MPV_FORMAT_NONE         (no member)
     *
     * If you encounter a value you don't know, you must not make any
     * assumptions about the contents of union u.
     */
    mpv_format format;
} mpv_node;

/**
 * (see mpv_node)
 */
typedef struct mpv_node_list {
    /**
     * Number of entries. Negative values are not allowed.
     */
    int num;
    /**
     * MPV_FORMAT_NODE_ARRAY:
     *  values[N] refers to value of the Nth item
     *
     * MPV_FORMAT_NODE_MAP:
     *  values[N] refers to value of the Nth key/value pair
     *
     * If num > 0, values[0] to values[num-1] (inclusive) are valid.
     * Otherwise, this can be NULL.
     */
    mpv_node *values;
    /**
     * MPV_FORMAT_NODE_ARRAY:
     *  unused (typically NULL), access is not allowed
     *
     * MPV_FORMAT_NODE_MAP:
     *  keys[N] refers to key of the Nth key/value pair. If num > 0, keys[0] to
     *  keys[num-1] (inclusive) are valid. Otherwise, this can be NULL.
     *  The keys are in random order. The only guarantee is that keys[N] belongs
     *  to the value values[N]. NULL keys are not allowed.
     */
    char **keys;
} mpv_node_list;

/**
 * (see mpv_node)
 */
typedef struct mpv_byte_array {
    /**
     * Pointer to the data. In what format the data is stored is up to whatever
     * uses MPV_FORMAT_BYTE_ARRAY.
     */
    void *data;
    /**
     * Size of the data pointed to by ptr.
     */
    size_t size;
} mpv_byte_array;

/**
 * Frees any data referenced by the node. It doesn't free the node itself.
 * Call this only if the mpv client API set the node. If you constructed the
 * node yourself (manually), you have to free it yourself.
 *
 * If node->format is MPV_FORMAT_NONE, this call does nothing. Likewise, if
 * the client API sets a node with this format, this function doesn't need to
 * be called. (This is just a clarification that there's no danger of anything
 * strange happening in these cases.)
 */
MPV_EXPORT void mpv_free_node_contents(mpv_node *node);

/**
 * Set an option. Note that you can't normally set options during runtime. It
 * works in uninitialized state (see mpv_create()), and in some cases in at
 * runtime.
 *
 * Using a format other than MPV_FORMAT_NODE is equivalent to constructing a
 * mpv_node with the given format and data, and passing the mpv_node to this
 * function.
 *
 * Note: this is semi-deprecated. For most purposes, this is not needed anymore.
 *       Starting with mpv version 0.21.0 (version 1.23) most options can be set
 *       with mpv_set_property() (and related functions), and even before
 *       mpv_initialize(). In some obscure corner cases, using this function
 *       to set options might still be required (see
 *       "Inconsistencies between options and properties" in the manpage). Once
 *       these are resolved, the option setting functions might be fully
 *       deprecated.
 *
 * @param name Option name. This is the same as on the mpv command line, but
 *             without the leading "--".
 * @param format see enum mpv_format.
 * @param[in] data Option value (according to the format).
 * @return error code
 */
MPV_EXPORT int mpv_set_option(mpv_handle *ctx, const char *name, mpv_format format,
                              void *data);

/**
 * Convenience function to set an option to a string value. This is like
 * calling mpv_set_option() with MPV_FORMAT_STRING.
 *
 * @return error code
 */
--MPV_EXPORT int mpv_set_option_string(mpv_handle *ctx, const char *name, const char *data);

/**
 * Send a command to the player. Commands are the same as those used in
 * input.conf, except that this function takes parameters in a pre-split
 * form.
 *
 * The commands and their parameters are documented in input.rst.
 *
 * Does not use OSD and string expansion by default (unlike mpv_command_string()
 * and input.conf).
 *
 * @param[in] args NULL-terminated list of strings. Usually, the first item
 *                 is the command, and the following items are arguments.
 * @return error code
 */
MPV_EXPORT int mpv_command(mpv_handle *ctx, const char **args);

/**
 * Same as mpv_command(), but allows passing structured data in any format.
 * In particular, calling mpv_command() is exactly like calling
 * mpv_command_node() with the format set to MPV_FORMAT_NODE_ARRAY, and
 * every arg passed in order as MPV_FORMAT_STRING.
 *
 * Does not use OSD and string expansion by default.
 *
 * The args argument can have one of the following formats:
 *
 * MPV_FORMAT_NODE_ARRAY:
 *      Positional arguments. Each entry is an argument using an arbitrary
 *      format (the format must be compatible to the used command). Usually,
 *      the first item is the command name (as MPV_FORMAT_STRING). The order
 *      of arguments is as documented in each command description.
 *
 * MPV_FORMAT_NODE_MAP:
 *      Named arguments. This requires at least a special entry with the key
 *      "_name" to be present, which must be a string, and contains the command
 *      name. For compatibility, if the key "_name" does not exist, then the
 *      entry with the key "name" will be used instead.
 *      The special entry "_flags" is optional, and if present, must be an
 *      array of strings, each being a command prefix to apply. All other
 *      entries are interpreted as arguments. They must use the argument names
 *      as documented in each command description. Some commands do not
 *      support named arguments at all, and must use MPV_FORMAT_NODE_ARRAY.
 *      Some commands have arguments named "name", and can only be used if
 *      the command name is specified with key "_name" instead of "name".
 *
 * @param[in] args mpv_node with format set to one of the values documented
 *                 above (see there for details)
 * @param[out] result Optional, pass NULL if unused. If not NULL, and if the
 *                    function succeeds, this is set to command-specific return
 *                    data. You must call mpv_free_node_contents() to free it
 *                    (again, only if the command actually succeeds).
 *                    Not many commands actually use this at all.
 * @return error code (the result parameter is not set on error)
 */
MPV_EXPORT int mpv_command_node(mpv_handle *ctx, mpv_node *args, mpv_node *result);

/**
 * This is essentially identical to mpv_command() but it also returns a result.
 *
 * Does not use OSD and string expansion by default.
 *
 * @param[in] args NULL-terminated list of strings. Usually, the first item
 *                 is the command, and the following items are arguments.
 * @param[out] result Optional, pass NULL if unused. If not NULL, and if the
 *                    function succeeds, this is set to command-specific return
 *                    data. You must call mpv_free_node_contents() to free it
 *                    (again, only if the command actually succeeds).
 *                    Not many commands actually use this at all.
 * @return error code (the result parameter is not set on error)
 */
MPV_EXPORT int mpv_command_ret(mpv_handle *ctx, const char **args, mpv_node *result);

/**
 * Same as mpv_command, but use input.conf parsing for splitting arguments.
 * This is slightly simpler, but also more error prone, since arguments may
 * need quoting/escaping.
 *
 * This also has OSD and string expansion enabled by default.
 */
MPV_EXPORT int mpv_command_string(mpv_handle *ctx, const char *args);

/**
 * Same as mpv_command, but run the command asynchronously.
 *
 * Commands are executed asynchronously. You will receive a
 * MPV_EVENT_COMMAND_REPLY event. This event will also have an
 * error code set if running the command failed. For commands that
 * return data, the data is put into mpv_event_command.result.
 *
 * The only case when you do not receive an event is when the function call
 * itself fails. This happens only if parsing the command itself (or otherwise
 * validating it) fails, i.e. the return code of the API call is not 0 or
 * positive.
 *
 * Safe to be called from mpv render API threads.
 *
 * @param reply_userdata the value mpv_event.reply_userdata of the reply will
 *                       be set to (see section about asynchronous calls)
 * @param args NULL-terminated list of strings (see mpv_command())
 * @return error code (if parsing or queuing the command fails)
 */
MPV_EXPORT int mpv_command_async(mpv_handle *ctx, uint64_t reply_userdata,
                                 const char **args);

/**
 * Same as mpv_command_node(), but run it asynchronously. Basically, this
 * function is to mpv_command_node() what mpv_command_async() is to
 * mpv_command().
 *
 * See mpv_command_async() for details.
 *
 * Safe to be called from mpv render API threads.
 *
 * @param reply_userdata the value mpv_event.reply_userdata of the reply will
 *                       be set to (see section about asynchronous calls)
 * @param args as in mpv_command_node()
 * @return error code (if parsing or queuing the command fails)
 */
MPV_EXPORT int mpv_command_node_async(mpv_handle *ctx, uint64_t reply_userdata,
                                      mpv_node *args);

/**
 * Signal to all async requests with the matching ID to abort. This affects
 * the following API calls:
 *
 *      mpv_command_async
 *      mpv_command_node_async
 *
 * All of these functions take a reply_userdata parameter. This API function
 * tells all requests with the matching reply_userdata value to try to return
 * as soon as possible. If there are multiple requests with matching ID, it
 * aborts all of them.
 *
 * This API function is mostly asynchronous itself. It will not wait until the
 * command is aborted. Instead, the command will terminate as usual, but with
 * some work not done. How this is signaled depends on the specific command (for
 * example, the "subprocess" command will indicate it by "killed_by_us" set to
 * true in the result). How long it takes also depends on the situation. The
 * aborting process is completely asynchronous.
 *
 * Not all commands may support this functionality. In this case, this function
 * will have no effect. The same is true if the request using the passed
 * reply_userdata has already terminated, has not been started yet, or was
 * never in use at all.
 *
 * You have to be careful of race conditions: the time during which the abort
 * request will be effective is _after_ e.g. mpv_command_async() has returned,
 * and before the command has signaled completion with MPV_EVENT_COMMAND_REPLY.
 *
 * @param reply_userdata ID of the request to be aborted (see above)
 */
MPV_EXPORT void mpv_abort_async_command(mpv_handle *ctx, uint64_t reply_userdata);

/**
 * Set a property to a given value. Properties are essentially variables which
 * can be queried or set at runtime. For example, writing to the pause property
 * will actually pause or unpause playback.
 *
 * If the format doesn't match with the internal format of the property, access
 * usually will fail with MPV_ERROR_PROPERTY_FORMAT. In some cases, the data
 * is automatically converted and access succeeds. For example, MPV_FORMAT_INT64
 * is always converted to MPV_FORMAT_DOUBLE, and access using MPV_FORMAT_STRING
 * usually invokes a string parser. The same happens when calling this function
 * with MPV_FORMAT_NODE: the underlying format may be converted to another
 * type if possible.
 *
 * Using a format other than MPV_FORMAT_NODE is equivalent to constructing a
 * mpv_node with the given format and data, and passing the mpv_node to this
 * function. (Before API version 1.21, this was different.)
 *
 * Note: starting with mpv 0.21.0 (client API version 1.23), this can be used to
 *       set options in general. It even can be used before mpv_initialize()
 *       has been called. If called before mpv_initialize(), setting properties
 *       not backed by options will result in MPV_ERROR_PROPERTY_UNAVAILABLE.
 *       In some cases, properties and options still conflict. In these cases,
 *       mpv_set_property() accesses the options before mpv_initialize(), and
 *       the properties after mpv_initialize(). These conflicts will be removed
 *       in mpv 0.23.0. See mpv_set_option() for further remarks.
 *
 * @param name The property name. See input.rst for a list of properties.
 * @param format see enum mpv_format.
 * @param[in] data Option value.
 * @return error code
 */
MPV_EXPORT int mpv_set_property(mpv_handle *ctx, const char *name, mpv_format format,
                                void *data);

/**
 * Convenience function to set a property to a string value.
 *
 * This is like calling mpv_set_property() with MPV_FORMAT_STRING.
 */
MPV_EXPORT int mpv_set_property_string(mpv_handle *ctx, const char *name, const char *data);

/**
 * Convenience function to delete a property.
 *
 * This is equivalent to running the command "del [name]".
 *
 * @param name The property name. See input.rst for a list of properties.
 * @return error code
 */
MPV_EXPORT int mpv_del_property(mpv_handle *ctx, const char *name);

/**
 * Set a property asynchronously. You will receive the result of the operation
 * as MPV_EVENT_SET_PROPERTY_REPLY event. The mpv_event.error field will contain
 * the result status of the operation. Otherwise, this function is similar to
 * mpv_set_property().
 *
 * Safe to be called from mpv render API threads.
 *
 * @param reply_userdata see section about asynchronous calls
 * @param name The property name.
 * @param format see enum mpv_format.
 * @param[in] data Option value. The value will be copied by the function. It
 *                 will never be modified by the client API.
 * @return error code if sending the request failed
 */
MPV_EXPORT int mpv_set_property_async(mpv_handle *ctx, uint64_t reply_userdata,
                                      const char *name, mpv_format format, void *data);

/**
 * Read the value of the given property.
 *
 * If the format doesn't match with the internal format of the property, access
 * usually will fail with MPV_ERROR_PROPERTY_FORMAT. In some cases, the data
 * is automatically converted and access succeeds. For example, MPV_FORMAT_INT64
 * is always converted to MPV_FORMAT_DOUBLE, and access using MPV_FORMAT_STRING
 * usually invokes a string formatter.
 *
 * @param name The property name.
 * @param format see enum mpv_format.
 * @param[out] data Pointer to the variable holding the option value. On
 *                  success, the variable will be set to a copy of the option
 *                  value. For formats that require dynamic memory allocation,
 *                  you can free the value with mpv_free() (strings) or
 *                  mpv_free_node_contents() (MPV_FORMAT_NODE).
 * @return error code
 */
MPV_EXPORT int mpv_get_property(mpv_handle *ctx, const char *name, mpv_format format,
                                void *data);

/**
 * Get a property asynchronously. You will receive the result of the operation
 * as well as the property data with the MPV_EVENT_GET_PROPERTY_REPLY event.
 * You should check the mpv_event.error field on the reply event.
 *
 * Safe to be called from mpv render API threads.
 *
 * @param reply_userdata see section about asynchronous calls
 * @param name The property name.
 * @param format see enum mpv_format.
 * @return error code if sending the request failed
 */
MPV_EXPORT int mpv_get_property_async(mpv_handle *ctx, uint64_t reply_userdata,
                                      const char *name, mpv_format format);

/**
 * Get a notification whenever the given property changes. You will receive
 * updates as MPV_EVENT_PROPERTY_CHANGE. Note that this is not very precise:
 * for some properties, it may not send updates even if the property changed.
 * This depends on the property, and it's a valid feature request to ask for
 * better update handling of a specific property. (For some properties, like
 * ``clock``, which shows the wall clock, this mechanism doesn't make too
 * much sense anyway.)
 *
 * Property changes are coalesced: the change events are returned only once the
 * event queue becomes empty (e.g. mpv_wait_event() would block or return
 * MPV_EVENT_NONE), and then only one event per changed property is returned.
 *
 * You always get an initial change notification. This is meant to initialize
 * the user's state to the current value of the property.
 *
 * Normally, change events are sent only if the property value changes according
 * to the requested format. mpv_event_property will contain the property value
 * as data member.
 *
 * Warning: if a property is unavailable or retrieving it caused an error,
 *          MPV_FORMAT_NONE will be set in mpv_event_property, even if the
 *          format parameter was set to a different value. In this case, the
 *          mpv_event_property.data field is invalid.
 *
 * If the property is observed with the format parameter set to MPV_FORMAT_NONE,
 * you get low-level notifications whether the property _may_ have changed, and
 * the data member in mpv_event_property will be unset. With this mode, you
 * will have to determine yourself whether the property really changed. On the
 * other hand, this mechanism can be faster and uses less resources.
 *
 * Observing a property that doesn't exist is allowed. (Although it may still
 * cause some sporadic change events.)
 *
 * Keep in mind that you will get change notifications even if you change a
 * property yourself. Try to avoid endless feedback loops, which could happen
 * if you react to the change notifications triggered by your own change.
 *
 * Only the mpv_handle on which this was called will receive the property
 * change events, or can unobserve them.
 *
 * Safe to be called from mpv render API threads.
 *
 * @param reply_userdata This will be used for the mpv_event.reply_userdata
 *                       field for the received MPV_EVENT_PROPERTY_CHANGE
 *                       events. (Also see section about asynchronous calls,
 *                       although this function is somewhat different from
 *                       actual asynchronous calls.)
 *                       If you have no use for this, pass 0.
 *                       Also see mpv_unobserve_property().
 * @param name The property name.
 * @param format see enum mpv_format. Can be MPV_FORMAT_NONE to omit values
 *               from the change events.
 * @return error code (usually fails only on OOM or unsupported format)
 */
MPV_EXPORT int mpv_observe_property(mpv_handle *mpv, uint64_t reply_userdata,
                                    const char *name, mpv_format format);

/**
 * Undo mpv_observe_property(). This will remove all observed properties for
 * which the given number was passed as reply_userdata to mpv_observe_property.
 *
 * Safe to be called from mpv render API threads.
 *
 * @param registered_reply_userdata ID that was passed to mpv_observe_property
 * @return negative value is an error code, >=0 is number of removed properties
 *         on success (includes the case when 0 were removed)
 */
MPV_EXPORT int mpv_unobserve_property(mpv_handle *mpv, uint64_t registered_reply_userdata);

typedef enum mpv_event_id {
    /**
     * Nothing happened. Happens on timeouts or sporadic wakeups.
     */
    MPV_EVENT_NONE              = 0,
    /**
     * Happens when the player quits. The player enters a state where it tries
     * to disconnect all clients. Most requests to the player will fail, and
     * the client should react to this and quit with mpv_destroy() as soon as
     * possible.
     */
    MPV_EVENT_SHUTDOWN          = 1,
    /**
     * See mpv_request_log_messages().
     */
    MPV_EVENT_LOG_MESSAGE       = 2,
    /**
     * Reply to a mpv_get_property_async() request.
     * See also mpv_event and mpv_event_property.
     */
    MPV_EVENT_GET_PROPERTY_REPLY = 3,
    /**
     * Reply to a mpv_set_property_async() request.
     * (Unlike MPV_EVENT_GET_PROPERTY, mpv_event_property is not used.)
     */
    MPV_EVENT_SET_PROPERTY_REPLY = 4,
    /**
     * Reply to a mpv_command_async() or mpv_command_node_async() request.
     * See also mpv_event and mpv_event_command.
     */
    MPV_EVENT_COMMAND_REPLY     = 5,
    /**
     * Notification before playback start of a file (before the file is loaded).
     * See also mpv_event and mpv_event_start_file.
     */
    MPV_EVENT_START_FILE        = 6,
    /**
     * Notification after playback end (after the file was unloaded).
     * See also mpv_event and mpv_event_end_file.
     */
    MPV_EVENT_END_FILE          = 7,
    /**
     * Notification when the file has been loaded (headers were read etc.), and
     * decoding starts.
     */
    MPV_EVENT_FILE_LOADED       = 8,
#if MPV_ENABLE_DEPRECATED
    /**
     * Idle mode was entered. In this mode, no file is played, and the playback
     * core waits for new commands. (The command line player normally quits
     * instead of entering idle mode, unless --idle was specified. If mpv
     * was started with mpv_create(), idle mode is enabled by default.)
     *
     * @deprecated This is equivalent to using mpv_observe_property() on the
     *             "idle-active" property. The event is redundant, and might be
     *             removed in the far future. As a further warning, this event
     *             is not necessarily sent at the right point anymore (at the
     *             start of the program), while the property behaves correctly.
     */
    MPV_EVENT_IDLE              = 11,
    /**
     * Sent every time after a video frame is displayed. Note that currently,
     * this will be sent in lower frequency if there is no video, or playback
     * is paused - but that will be removed in the future, and it will be
     * restricted to video frames only.
     *
     * @deprecated Use mpv_observe_property() with relevant properties instead
     *             (such as "playback-time").
     */
    MPV_EVENT_TICK              = 14,
#endif
    /**
     * Triggered by the script-message input command. The command uses the
     * first argument of the command as client name (see mpv_client_name()) to
     * dispatch the message, and passes along all arguments starting from the
     * second argument as strings.
     * See also mpv_event and mpv_event_client_message.
     */
    MPV_EVENT_CLIENT_MESSAGE    = 16,
    /**
     * Happens after video changed in some way. This can happen on resolution
     * changes, pixel format changes, or video filter changes. The event is
     * sent after the video filters and the VO are reconfigured. Applications
     * embedding a mpv window should listen to this event in order to resize
     * the window if needed.
     * Note that this event can happen sporadically, and you should check
     * yourself whether the video parameters really changed before doing
     * something expensive.
     */
    MPV_EVENT_VIDEO_RECONFIG    = 17,
    /**
     * Similar to MPV_EVENT_VIDEO_RECONFIG. This is relatively uninteresting,
     * because there is no such thing as audio output embedding.
     */
    MPV_EVENT_AUDIO_RECONFIG    = 18,
    /**
     * Happens when a seek was initiated. Playback stops. Usually it will
     * resume with MPV_EVENT_PLAYBACK_RESTART as soon as the seek is finished.
     */
    MPV_EVENT_SEEK              = 20,
    /**
     * There was a discontinuity of some sort (like a seek), and playback
     * was reinitialized. Usually happens on start of playback and after
     * seeking. The main purpose is allowing the client to detect when a seek
     * request is finished.
     */
    MPV_EVENT_PLAYBACK_RESTART  = 21,
    /**
     * Event sent due to mpv_observe_property().
     * See also mpv_event and mpv_event_property.
     */
    MPV_EVENT_PROPERTY_CHANGE   = 22,
    /**
     * Happens if the internal per-mpv_handle ringbuffer overflows, and at
     * least 1 event had to be dropped. This can happen if the client doesn't
     * read the event queue quickly enough with mpv_wait_event(), or if the
     * client makes a very large number of asynchronous calls at once.
     *
     * Event delivery will continue normally once this event was returned
     * (this forces the client to empty the queue completely).
     */
    MPV_EVENT_QUEUE_OVERFLOW    = 24,
    /**
     * Triggered if a hook handler was registered with mpv_hook_add(), and the
     * hook is invoked. If you receive this, you must handle it, and continue
     * the hook with mpv_hook_continue().
     * See also mpv_event and mpv_event_hook.
     */
    MPV_EVENT_HOOK              = 25,
    // Internal note: adjust INTERNAL_EVENT_BASE when adding new events.
} mpv_event_id;

/**
 * Return a string describing the event. For unknown events, NULL is returned.
 *
 * Note that all events actually returned by the API will also yield a non-NULL
 * string with this function.
 *
 * @param event event ID, see see enum mpv_event_id
 * @return A static string giving a short symbolic name of the event. It
 *         consists of lower-case alphanumeric characters and can include "-"
 *         characters. This string is suitable for use in e.g. scripting
 *         interfaces.
 *         The string is completely static, i.e. doesn't need to be deallocated,
 *         and is valid forever.
 */
MPV_EXPORT const char *mpv_event_name(mpv_event_id event);

typedef struct mpv_event_property {
    /**
     * Name of the property.
     */
    const char *name;
    /**
     * Format of the data field in the same struct. See enum mpv_format.
     * This is always the same format as the requested format, except when
     * the property could not be retrieved (unavailable, or an error happened),
     * in which case the format is MPV_FORMAT_NONE.
     */
    mpv_format format;
    /**
     * Received property value. Depends on the format. This is like the
     * pointer argument passed to mpv_get_property().
     *
     * For example, for MPV_FORMAT_STRING you get the string with:
     *
     *    char *value = *(char **)(event_property->data);
     *
     * Note that this is set to NULL if retrieving the property failed (the
     * format will be MPV_FORMAT_NONE).
     */
    void *data;
} mpv_event_property;

/**
 * Numeric log levels. The lower the number, the more important the message is.
 * MPV_LOG_LEVEL_NONE is never used when receiving messages. The string in
 * the comment after the value is the name of the log level as used for the
 * mpv_request_log_messages() function.
 * Unused numeric values are unused, but reserved for future use.
 */
typedef enum mpv_log_level {
    MPV_LOG_LEVEL_NONE  = 0,    /// "no"    - disable absolutely all messages
    MPV_LOG_LEVEL_FATAL = 10,   /// "fatal" - critical/aborting errors
    MPV_LOG_LEVEL_ERROR = 20,   /// "error" - simple errors
    MPV_LOG_LEVEL_WARN  = 30,   /// "warn"  - possible problems
    MPV_LOG_LEVEL_INFO  = 40,   /// "info"  - informational message
    MPV_LOG_LEVEL_V     = 50,   /// "v"     - noisy informational message
    MPV_LOG_LEVEL_DEBUG = 60,   /// "debug" - very noisy technical information
    MPV_LOG_LEVEL_TRACE = 70,   /// "trace" - extremely noisy
} mpv_log_level;

typedef struct mpv_event_log_message {
    /**
     * The module prefix, identifies the sender of the message. As a special
     * case, if the message buffer overflows, this will be set to the string
     * "overflow" (which doesn't appear as prefix otherwise), and the text
     * field will contain an informative message.
     */
    const char *prefix;
    /**
     * The log level as string. See mpv_request_log_messages() for possible
     * values. The level "no" is never used here.
     */
    const char *level;
    /**
     * The log message. It consists of 1 line of text, and is terminated with
     * a newline character. (Before API version 1.6, it could contain multiple
     * or partial lines.)
     */
    const char *text;
    /**
     * The same contents as the level field, but as a numeric ID.
     * Since API version 1.6.
     */
    mpv_log_level log_level;
} mpv_event_log_message;

/// Since API version 1.9.
typedef enum mpv_end_file_reason {
    /**
     * The end of file was reached. Sometimes this may also happen on
     * incomplete or corrupted files, or if the network connection was
     * interrupted when playing a remote file. It also happens if the
     * playback range was restricted with --end or --frames or similar.
     */
    MPV_END_FILE_REASON_EOF = 0,
    /**
     * Playback was stopped by an external action (e.g. playlist controls).
     */
    MPV_END_FILE_REASON_STOP = 2,
    /**
     * Playback was stopped by the quit command or player shutdown.
     */
    MPV_END_FILE_REASON_QUIT = 3,
    /**
     * Some kind of error happened that lead to playback abort. Does not
     * necessarily happen on incomplete or broken files (in these cases, both
     * MPV_END_FILE_REASON_ERROR or MPV_END_FILE_REASON_EOF are possible).
     *
     * mpv_event_end_file.error will be set.
     */
    MPV_END_FILE_REASON_ERROR = 4,
    /**
     * The file was a playlist or similar. When the playlist is read, its
     * entries will be appended to the playlist after the entry of the current
     * file, the entry of the current file is removed, and a MPV_EVENT_END_FILE
     * event is sent with reason set to MPV_END_FILE_REASON_REDIRECT. Then
     * playback continues with the playlist contents.
     * Since API version 1.18.
     */
    MPV_END_FILE_REASON_REDIRECT = 5,
} mpv_end_file_reason;

/// Since API version 1.108.
typedef struct mpv_event_start_file {
    /**
     * Playlist entry ID of the file being loaded now.
     */
    int64_t playlist_entry_id;
} mpv_event_start_file;

typedef struct mpv_event_end_file {
    /**
     * Corresponds to the values in enum mpv_end_file_reason.
     *
     * Unknown values should be treated as unknown.
     */
    mpv_end_file_reason reason;
    /**
     * If reason==MPV_END_FILE_REASON_ERROR, this contains a mpv error code
     * (one of MPV_ERROR_...) giving an approximate reason why playback
     * failed. In other cases, this field is 0 (no error).
     * Since API version 1.9.
     */
    int error;
    /**
     * Playlist entry ID of the file that was being played or attempted to be
     * played. This has the same value as the playlist_entry_id field in the
     * corresponding mpv_event_start_file event.
     * Since API version 1.108.
     */
    int64_t playlist_entry_id;
    /**
     * If loading ended, because the playlist entry to be played was for example
     * a playlist, and the current playlist entry is replaced with a number of
     * other entries. This may happen at least with MPV_END_FILE_REASON_REDIRECT
     * (other event types may use this for similar but different purposes in the
     * future). In this case, playlist_insert_id will be set to the playlist
     * entry ID of the first inserted entry, and playlist_insert_num_entries to
     * the total number of inserted playlist entries. Note this in this specific
     * case, the ID of the last inserted entry is playlist_insert_id+num-1.
     * Beware that depending on circumstances, you may observe the new playlist
     * entries before seeing the event (e.g. reading the "playlist" property or
     * getting a property change notification before receiving the event).
     * Since API version 1.108.
     */
    int64_t playlist_insert_id;
    /**
     * See playlist_insert_id. Only non-0 if playlist_insert_id is valid. Never
     * negative.
     * Since API version 1.108.
     */
    int playlist_insert_num_entries;
} mpv_event_end_file;

typedef struct mpv_event_client_message {
    /**
     * Arbitrary arguments chosen by the sender of the message. If num_args > 0,
     * you can access args[0] through args[num_args - 1] (inclusive). What
     * these arguments mean is up to the sender and receiver.
     * None of the valid items are NULL.
     */
    int num_args;
    const char **args;
} mpv_event_client_message;

typedef struct mpv_event_hook {
    /**
     * The hook name as passed to mpv_hook_add().
     */
    const char *name;
    /**
     * Internal ID that must be passed to mpv_hook_continue().
     */
    uint64_t id;
} mpv_event_hook;

// Since API version 1.102.
typedef struct mpv_event_command {
    /**
     * Result data of the command. Note that success/failure is signaled
     * separately via mpv_event.error. This field is only for result data
     * in case of success. Most commands leave it at MPV_FORMAT_NONE. Set
     * to MPV_FORMAT_NONE on failure.
     */
    mpv_node result;
} mpv_event_command;

typedef struct mpv_event {
    /**
     * One of mpv_event. Keep in mind that later ABI compatible releases might
     * add new event types. These should be ignored by the API user.
     */
    mpv_event_id event_id;
    /**
     * This is mainly used for events that are replies to (asynchronous)
     * requests. It contains a status code, which is >= 0 on success, or < 0
     * on error (a mpv_error value). Usually, this will be set if an
     * asynchronous request fails.
     * Used for:
     *  MPV_EVENT_GET_PROPERTY_REPLY
     *  MPV_EVENT_SET_PROPERTY_REPLY
     *  MPV_EVENT_COMMAND_REPLY
     */
    int error;
    /**
     * If the event is in reply to a request (made with this API and this
     * API handle), this is set to the reply_userdata parameter of the request
     * call. Otherwise, this field is 0.
     * Used for:
     *  MPV_EVENT_GET_PROPERTY_REPLY
     *  MPV_EVENT_SET_PROPERTY_REPLY
     *  MPV_EVENT_COMMAND_REPLY
     *  MPV_EVENT_PROPERTY_CHANGE
     *  MPV_EVENT_HOOK
     */
    uint64_t reply_userdata;
    /**
     * The meaning and contents of the data member depend on the event_id:
     *  MPV_EVENT_GET_PROPERTY_REPLY:     mpv_event_property*
     *  MPV_EVENT_PROPERTY_CHANGE:        mpv_event_property*
     *  MPV_EVENT_LOG_MESSAGE:            mpv_event_log_message*
     *  MPV_EVENT_CLIENT_MESSAGE:         mpv_event_client_message*
     *  MPV_EVENT_START_FILE:             mpv_event_start_file* (since v1.108)
     *  MPV_EVENT_END_FILE:               mpv_event_end_file*
     *  MPV_EVENT_HOOK:                   mpv_event_hook*
     *  MPV_EVENT_COMMAND_REPLY*          mpv_event_command*
     *  other: NULL
     *
     * Note: future enhancements might add new event structs for existing or new
     *       event types.
     */
    void *data;
} mpv_event;

/**
 * Convert the given src event to a mpv_node, and set *dst to the result. *dst
 * is set to a MPV_FORMAT_NODE_MAP, with fields for corresponding mpv_event and
 * mpv_event.data/mpv_event_* fields.
 *
 * The exact details are not completely documented out of laziness. A start
 * is located in the "Events" section of the manpage.
 *
 * *dst may point to newly allocated memory, or pointers in mpv_event. You must
 * copy the entire mpv_node if you want to reference it after mpv_event becomes
 * invalid (such as making a new mpv_wait_event() call, or destroying the
 * mpv_handle from which it was returned). Call mpv_free_node_contents() to free
 * any memory allocations made by this API function.
 *
 * Safe to be called from mpv render API threads.
 *
 * @param dst Target. This is not read and fully overwritten. Must be released
 *            with mpv_free_node_contents(). Do not write to pointers returned
 *            by it. (On error, this may be left as an empty node.)
 * @param src The source event. Not modified (it's not const due to the author's
 *            prejudice of the C version of const).
 * @return error code (MPV_ERROR_NOMEM only, if at all)
 */
MPV_EXPORT int mpv_event_to_node(mpv_node *dst, mpv_event *src);

/**
 * Enable or disable the given event.
 *
 * Some events are enabled by default. Some events can't be disabled.
 *
 * (Informational note: currently, all events are enabled by default, except
 *  MPV_EVENT_TICK.)
 *
 * Safe to be called from mpv render API threads.
 *
 * @param event See enum mpv_event_id.
 * @param enable 1 to enable receiving this event, 0 to disable it.
 * @return error code
 */
MPV_EXPORT int mpv_request_event(mpv_handle *ctx, mpv_event_id event, int enable);

/**
 * Enable or disable receiving of log messages. These are the messages the
 * command line player prints to the terminal. This call sets the minimum
 * required log level for a message to be received with MPV_EVENT_LOG_MESSAGE.
 *
 * @param min_level Minimal log level as string. Valid log levels:
 *                      no fatal error warn info v debug trace
 *                  The value "no" disables all messages. This is the default.
 *                  An exception is the value "terminal-default", which uses the
 *                  log level as set by the "--msg-level" option. This works
 *                  even if the terminal is disabled. (Since API version 1.19.)
 *                  Also see mpv_log_level.
 * @return error code
 */
MPV_EXPORT int mpv_request_log_messages(mpv_handle *ctx, const char *min_level);

/**
 * Wait for the next event, or until the timeout expires, or if another thread
 * makes a call to mpv_wakeup(). Passing 0 as timeout will never wait, and
 * is suitable for polling.
 *
 * The internal event queue has a limited size (per client handle). If you
 * don't empty the event queue quickly enough with mpv_wait_event(), it will
 * overflow and silently discard further events. If this happens, making
 * asynchronous requests will fail as well (with MPV_ERROR_EVENT_QUEUE_FULL).
 *
 * Only one thread is allowed to call this on the same mpv_handle at a time.
 * The API won't complain if more than one thread calls this, but it will cause
 * race conditions in the client when accessing the shared mpv_event struct.
 * Note that most other API functions are not restricted by this, and no API
 * function internally calls mpv_wait_event(). Additionally, concurrent calls
 * to different mpv_handles are always safe.
 *
 * As long as the timeout is 0, this is safe to be called from mpv render API
 * threads.
 *
 * @param timeout Timeout in seconds, after which the function returns even if
 *                no event was received. A MPV_EVENT_NONE is returned on
 *                timeout. A value of 0 will disable waiting. Negative values
 *                will wait with an infinite timeout.
 * @return A struct containing the event ID and other data. The pointer (and
 *         fields in the struct) stay valid until the next mpv_wait_event()
 *         call, or until the mpv_handle is destroyed. You must not write to
 *         the struct, and all memory referenced by it will be automatically
 *         released by the API on the next mpv_wait_event() call, or when the
 *         context is destroyed. The return value is never NULL.
 */
MPV_EXPORT mpv_event *mpv_wait_event(mpv_handle *ctx, double timeout);

/**
 * Interrupt the current mpv_wait_event() call. This will wake up the thread
 * currently waiting in mpv_wait_event(). If no thread is waiting, the next
 * mpv_wait_event() call will return immediately (this is to avoid lost
 * wakeups).
 *
 * mpv_wait_event() will receive a MPV_EVENT_NONE if it's woken up due to
 * this call. But note that this dummy event might be skipped if there are
 * already other events queued. All what counts is that the waiting thread
 * is woken up at all.
 *
 * Safe to be called from mpv render API threads.
 */
MPV_EXPORT void mpv_wakeup(mpv_handle *ctx);

/**
 * Set a custom function that should be called when there are new events. Use
 * this if blocking in mpv_wait_event() to wait for new events is not feasible.
 *
 * Keep in mind that the callback will be called from foreign threads. You
 * must not make any assumptions of the environment, and you must return as
 * soon as possible (i.e. no long blocking waits). Exiting the callback through
 * any other means than a normal return is forbidden (no throwing exceptions,
 * no longjmp() calls). You must not change any local thread state (such as
 * the C floating point environment).
 *
 * You are not allowed to call any client API functions inside of the callback.
 * In particular, you should not do any processing in the callback, but wake up
 * another thread that does all the work. The callback is meant strictly for
 * notification only, and is called from arbitrary core parts of the player,
 * that make no considerations for reentrant API use or allowing the callee to
 * spend a lot of time doing other things. Keep in mind that it's also possible
 * that the callback is called from a thread while a mpv API function is called
 * (i.e. it can be reentrant).
 *
 * In general, the client API expects you to call mpv_wait_event() to receive
 * notifications, and the wakeup callback is merely a helper utility to make
 * this easier in certain situations. Note that it's possible that there's
 * only one wakeup callback invocation for multiple events. You should call
 * mpv_wait_event() with no timeout until MPV_EVENT_NONE is reached, at which
 * point the event queue is empty.
 *
 * If you actually want to do processing in a callback, spawn a thread that
 * does nothing but call mpv_wait_event() in a loop and dispatches the result
 * to a callback.
 *
 * Only one wakeup callback can be set.
 *
 * @param cb function that should be called if a wakeup is required
 * @param d arbitrary userdata passed to cb
 */
MPV_EXPORT void mpv_set_wakeup_callback(mpv_handle *ctx, void (*cb)(void *d), void *d);

/**
 * Block until all asynchronous requests are done. This affects functions like
 * mpv_command_async(), which return immediately and return their result as
 * events.
 *
 * This is a helper, and somewhat equivalent to calling mpv_wait_event() in a
 * loop until all known asynchronous requests have sent their reply as event,
 * except that the event queue is not emptied.
 */
MPV_EXPORT void mpv_wait_async_requests(mpv_handle *ctx);

/**
 * A hook is like a synchronous event that blocks the player. You register
 * a hook handler with this function. You will get an event, which you need
 * to handle, and once things are ready, you can let the player continue with
 * mpv_hook_continue().
 *
 * Currently, hooks can't be removed explicitly. But they will be implicitly
 * removed if the mpv_handle it was registered with is destroyed. This also
 * continues the hook if it was being handled by the destroyed mpv_handle (but
 * this should be avoided, as it might mess up order of hook execution).
 *
 * Hook handlers are ordered globally by priority and order of registration.
 * Handlers for the same hook with same priority are invoked in order of
 * registration (the handler registered first is run first). Handlers with
 * lower priority are run first (which seems backward).
 *
 * See the "Hooks" section in the manpage to see which hooks are currently
 * defined.
 *
 * Some hooks might be reentrant (so you get multiple MPV_EVENT_HOOK for the
 * same hook). If this can happen for a specific hook type, it will be
 * explicitly documented in the manpage.
 *
 * Only the mpv_handle on which this was called will receive the hook events,
 * or can "continue" them.
 *
 * @param reply_userdata This will be used for the mpv_event.reply_userdata
 *                       field for the received MPV_EVENT_HOOK events.
 *                       If you have no use for this, pass 0.
 * @param name The hook name. This should be one of the documented names. But
 *             if the name is unknown, the hook event will simply be never
 *             raised.
 * @param priority See remarks above. Use 0 as a neutral default.
 * @return error code (usually fails only on OOM)
 */
MPV_EXPORT int mpv_hook_add(mpv_handle *ctx, uint64_t reply_userdata,
                            const char *name, int priority);

/**
 * Respond to a MPV_EVENT_HOOK event. You must call this after you have handled
 * the event. There is no way to "cancel" or "stop" the hook.
 *
 * Calling this will will typically unblock the player for whatever the hook
 * is responsible for (e.g. for the "on_load" hook it lets it continue
 * playback).
 *
 * It is explicitly undefined behavior to call this more than once for each
 * MPV_EVENT_HOOK, to pass an incorrect ID, or to call this on a mpv_handle
 * different from the one that registered the handler and received the event.
 *
 * @param id This must be the value of the mpv_event_hook.id field for the
 *           corresponding MPV_EVENT_HOOK.
 * @return error code
 */
MPV_EXPORT int mpv_hook_continue(mpv_handle *ctx, uint64_t id);

#if MPV_ENABLE_DEPRECATED

/**
 * Return a UNIX file descriptor referring to the read end of a pipe. This
 * pipe can be used to wake up a poll() based processing loop. The purpose of
 * this function is very similar to mpv_set_wakeup_callback(), and provides
 * a primitive mechanism to handle coordinating a foreign event loop and the
 * libmpv event loop. The pipe is non-blocking. It's closed when the mpv_handle
 * is destroyed. This function always returns the same value (on success).
 *
 * This is in fact implemented using the same underlying code as for
 * mpv_set_wakeup_callback() (though they don't conflict), and it is as if each
 * callback invocation writes a single 0 byte to the pipe. When the pipe
 * becomes readable, the code calling poll() (or select()) on the pipe should
 * read all contents of the pipe and then call mpv_wait_event(c, 0) until
 * no new events are returned. The pipe contents do not matter and can just
 * be discarded. There is not necessarily one byte per readable event in the
 * pipe. For example, the pipes are non-blocking, and mpv won't block if the
 * pipe is full. Pipes are normally limited to 4096 bytes, so if there are
 * more than 4096 events, the number of readable bytes can not equal the number
 * of events queued. Also, it's possible that mpv does not write to the pipe
 * once it's guaranteed that the client was already signaled. See the example
 * below how to do it correctly.
 *
 * Example:
 *
 *  int pipefd = mpv_get_wakeup_pipe(mpv);
 *  if (pipefd < 0)
 *      error();
 *  while (1) {
 *      struct pollfd pfds[1] = {
 *          { .fd = pipefd, .events = POLLIN },
 *      };
 *      // Wait until there are possibly new mpv events.
 *      poll(pfds, 1, -1);
 *      if (pfds[0].revents & POLLIN) {
 *          // Empty the pipe. Doing this before calling mpv_wait_event()
 *          // ensures that no wakeups are missed. It's not so important to
 *          // make sure the pipe is really empty (it will just cause some
 *          // additional wakeups in unlikely corner cases).
 *          char unused[256];
 *          read(pipefd, unused, sizeof(unused));
 *          while (1) {
 *              mpv_event *ev = mpv_wait_event(mpv, 0);
 *              // If MPV_EVENT_NONE is received, the event queue is empty.
 *              if (ev->event_id == MPV_EVENT_NONE)
 *                  break;
 *              // Process the event.
 *              ...
 *          }
 *      }
 *  }
 *
 * @deprecated this function will be removed in the future. If you need this
 *             functionality, use mpv_set_wakeup_callback(), create a pipe
 *             manually, and call write() on your pipe in the callback.
 *
 * @return A UNIX FD of the read end of the wakeup pipe, or -1 on error.
 *         On MS Windows/MinGW, this will always return -1.
 */
MPV_EXPORT int mpv_get_wakeup_pipe(mpv_handle *ctx);

#endif

#endif

#ifdef __cplusplus
}
#endif

#endif
--*/
--/*
/*
 * This file is part of mpv.
 *
 * mpv is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * mpv is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with mpv.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef MPLAYER_MP_CORE_H
#define MPLAYER_MP_CORE_H

#include <stdatomic.h>
#include <stdbool.h>

#include "audio/aframe.h"
#include "clipboard/clipboard.h"
#include "common/common.h"
#include "filters/f_output_chain.h"
#include "filters/filter.h"
#include "options/options.h"
#include "osdep/threads.h"
#include "sub/osd.h"
#include "video/mp_image.h"
#include "video/out/vo.h"
#include "osdep/als.h"
#include "demux/stheader.h"

// definitions used internally by the core player code

enum stop_play_reason {
    KEEP_PLAYING = 0,   // playback of a file is actually going on
                        // must be 0, numeric values of others do not matter
    AT_END_OF_FILE,     // file has ended, prepare to play next
                        // also returned on unrecoverable playback errors
    PT_NEXT_ENTRY,      // prepare to play next entry in playlist
    PT_CURRENT_ENTRY,   // prepare to play mpctx->playlist->current
    PT_STOP,            // stop playback / idle mode
    PT_QUIT,            // stop playback, quit player
    PT_ERROR,           // play next playlist entry (due to an error)
};

enum mp_osd_seek_info {
    OSD_SEEK_INFO_BAR           = 1,
    OSD_SEEK_INFO_TEXT          = 2,
    OSD_SEEK_INFO_CHAPTER_TEXT  = 4,
    OSD_SEEK_INFO_CURRENT_FILE  = 8,
};


enum {
    // other constants
    OSD_LEVEL_INVISIBLE = 4,
    OSD_BAR_SEEK = 256,

    MAX_NUM_VO_PTS = 100,
};

enum seek_type {
    MPSEEK_NONE = 0,
    MPSEEK_RELATIVE,
    MPSEEK_ABSOLUTE,
    MPSEEK_FACTOR,
    MPSEEK_FRAMESTEP,
    MPSEEK_CHAPTER,
};

enum seek_precision {
    // The following values are numerically sorted by increasing precision
    MPSEEK_DEFAULT = 0,
    MPSEEK_KEYFRAME,
    MPSEEK_EXACT,
    MPSEEK_VERY_EXACT,
};

enum seek_flags {
    MPSEEK_FLAG_DELAY = 1 << 0, // give player chance to coalesce multiple seeks
    MPSEEK_FLAG_NOFLUSH = 1 << 1, // keeping remaining data for seamless loops
};

struct seek_params {
    enum seek_type type;
    enum seek_precision exact;
    double amount;
    unsigned flags; // MPSEEK_FLAG_*
};

// Information about past video frames that have been sent to the VO.
struct frame_info {
    double pts;
    double duration;        // PTS difference to next frame
    double approx_duration; // possibly fixed/smoothed out duration
    int num_vsyncs;         // scheduled vsyncs, if using display-sync
};

struct track {
    enum stream_type type;

    // Currently used for decoding.
    bool selected;

    // The type specific ID, also called aid (audio), sid (subs), vid (video).
    // For UI purposes only; this ID doesn't have anything to do with any
    // IDs coming from demuxers or container files.
    int user_tid;

    int demuxer_id; // same as stream->demuxer_id. -1 if not set.
    int ff_index; // same as stream->ff_index, or 0.
    int hls_bitrate; // same as stream->hls_bitrate. 0 if not set.

    char *title;
    bool default_track, forced_track, dependent_track;
    bool visual_impaired_track, hearing_impaired_track;
    bool original_track, commentary_track;
    bool forced_select; // if the track was selected because it is forced
    bool image;
    bool attached_picture;
    char *lang;

    // If this track is from an external file (e.g. subtitle file).
    bool is_external;
    bool no_default;            // pretend it's not external for auto-selection
    bool no_auto_select;
    char *external_filename;
    bool auto_loaded;

    bool demuxer_ready; // if more packets should be read (subtitles only)

    struct demuxer *demuxer;
    // Invariant: !stream || stream->demuxer == demuxer
    struct sh_stream *stream;

    // Current subtitle state (or cached state if selected==false).
    struct dec_sub *d_sub;

    /* Heuristic for potentially redrawing subs. */
    bool redraw_subs;

    // Current decoding state (NULL if selected==false)
    struct mp_decoder_wrapper *dec;

    // Where the decoded result goes to (one of them is not NULL if active)
    struct vo_chain *vo_c;
    struct ao_chain *ao_c;
    struct mp_pin *sink;
};

// Returns true if the track belongs to the given program.
static inline bool track_has_program(const struct track *track, int program_id)
{
    return track->stream && sh_stream_has_program(track->stream, program_id);
}

// Summarizes video filtering and output.
struct vo_chain {
    struct mp_log *log;

    struct mp_output_chain *filter;

    struct vo *vo;

    struct track *track;
    struct mp_pin *filter_src;
    struct mp_pin *dec_src;

    // - video consists of a single picture, which should be shown only once
    // - do not sync audio to video in any way
    bool is_coverart;
    // - video consists of sparse still images
    bool is_sparse;
    bool sparse_eof_signalled;

    bool underrun;
    bool underrun_signaled;
};

// Like vo_chain, for audio.
struct ao_chain {
    struct mp_log *log;
    struct MPContext *mpctx;

    bool spdif_passthrough, spdif_failed;

    struct mp_output_chain *filter;

    struct ao *ao;
    struct mp_async_queue *ao_queue;
    struct mp_filter *queue_filter;
    struct mp_filter *ao_filter;
    double ao_resume_time;

    bool out_eof;
    double last_out_pts;

    double start_pts;
    bool start_pts_known;

    bool delaying_audio_start;

    struct track *track;
    struct mp_pin *filter_src;
    struct mp_pin *dec_src;

    double delay;
    bool untimed_throttle;

    bool ao_underrun;   // last known AO state
    bool underrun;      // for cache pause logic
};

/* Note that playback can be paused, stopped, etc. at any time. While paused,
 * playback restart is still active, because you want seeking to work even
 * if paused.
 * The main purpose of distinguishing these states is proper reinitialization
 * of A/V sync.
 */
enum playback_status {
    // code may compare status values numerically
    STATUS_SYNCING,     // seeking for a position to resume
    STATUS_READY,       // buffers full, playback can be started any time
    STATUS_PLAYING,     // normal playback
    STATUS_DRAINING,    // decoding has ended; still playing out queued buffers
    STATUS_EOF,         // playback has ended, or is disabled
};

const char *mp_status_str(enum playback_status st);

extern const int num_ptracks[STREAM_TYPE_COUNT];

// Maximum of all num_ptracks[] values.
#define MAX_PTRACKS 2

typedef struct MPContext {
    bool initialized;
    bool is_cli;
    mp_thread core_thread;
    struct mpv_global *global;
    struct MPOpts *opts;
    struct mp_log *log;
    struct stats_ctx *stats;
    struct m_config *mconfig;
    struct input_ctx *input;
    struct mp_client_api *clients;
    struct mp_dispatch_queue *dispatch;
    struct mp_cancel *playback_abort;
    // Number of asynchronous tasks that still need to finish until MPContext
    // destruction is ok. It's implied that the async tasks call
    // mp_wakeup_core() each time this is decremented.
    // As using an atomic+wakeup would be racy, this is a normal integer, and
    // mp_dispatch_lock must be called to change it.
    int64_t outstanding_async;

    struct mp_thread_pool *thread_pool; // for coarse I/O, often during loading

    struct mp_log *statusline;
    struct osd_state *osd;
    char *term_osd_text;
    char *term_osd_status;
    char *term_osd_subs[2];
    char *term_osd_contents;
    char *term_osd_title;
    char *last_window_title;
    struct voctrl_playback_state vo_playback_state;
    int64_t vo_playback_state_time;

    int add_osd_seek_info; // bitfield of enum mp_osd_seek_info
    double osd_visible; // for the osd bar only
    int osd_function;
    double osd_function_visible;
    double osd_msg_visible;
    double osd_msg_next_duration;
    double osd_last_update;
    bool osd_force_update, osd_idle_update;
    char *osd_msg_text;
    bool osd_show_pos;
    struct osd_progbar_state osd_progbar;

    struct playlist *playlist;
    struct playlist_entry *playing; // currently playing file
    char *filename; // immutable copy of playing->filename (or NULL)
    char *stream_open_filename;
    char **playlist_paths; // used strictly for playlist validation
    int playlist_paths_len;
    enum stop_play_reason stop_play;
    bool playback_initialized; // playloop can be run/is running
    int error_playing;

    struct clipboard_ctx *clipboard;

    // Return code to use with PT_QUIT
    int quit_custom_rc;
    bool has_quit_custom_rc;

    // Global file statistics
    int files_played;       // played without issues (even if stopped by user)
    int files_errored;      // played, but errors happened at one point
    int files_broken;       // couldn't be played at all

    // Current file statistics
    int64_t shown_vframes, shown_aframes;

    struct demux_chapter *chapters;
    int num_chapters;

    struct demuxer *demuxer;
    struct mp_tags *filtered_tags;

    struct track **tracks;
    int num_tracks;

    char *track_layout_hash;

    // Selected tracks. NULL if no track selected.
    // There can be num_ptracks[type] of the same STREAM_TYPE selected at once.
    // Currently, this is used for the secondary subtitle track only.
    struct track *current_track[MAX_PTRACKS][STREAM_TYPE_COUNT];

    struct mp_filter *filter_root;

    struct mp_filter *lavfi;
    char *lavfi_graph;

    struct ao *ao;
    struct mp_aframe *ao_filter_fmt; // for weak gapless audio check
    struct ao_chain *ao_chain;

    struct vo_chain *vo_chain;

    struct vo *video_out;
    // next_frame[0] is the next frame, next_frame[1] the one after that.
    // The +1 is for adding 1 additional frame in backstep mode.
    struct mp_image *next_frames[VO_MAX_REQ_FRAMES + 1];
    int num_next_frames;
    struct mp_image *saved_frame;   // for hrseek_lastframe and hrseek_backstep

    enum playback_status video_status, audio_status;
    bool restart_complete;
    int play_dir;
    // Factors to multiply with opts->playback_speed to get the total audio or
    // video speed (usually 1.0, but can be set to by the sync code).
    double speed_factor_v, speed_factor_a;
    // Redundant values set from opts->playback_speed and speed_factor_*.
    // update_playback_speed() updates them from the other fields.
    double audio_speed, video_speed;
    bool display_sync_active;
    double audio_drift_compensation;
    double avd_filtered;
    // Timing error (in seconds) due to rounding on vsync boundaries
    double display_sync_error;
    // Number of mistimed frames.
    int mistimed_frames_total;
    bool hrseek_active;     // skip all data until hrseek_pts
    bool hrseek_lastframe;  // drop everything until last frame reached
    bool hrseek_backstep;   // go to frame before seek target
    double hrseek_pts;
    struct seek_params current_seek;
    bool ab_loop_clip;      // clip to the "b" part of an A-B loop if available
    // AV sync: the next frame should be shown when the audio out has this
    // much (in seconds) buffered data left. Increased when more data is
    // written to the ao, decreased when moving to the next video frame.
    double delay;
    // AV sync: time in seconds until next frame should be shown
    double time_frame;
    // How much video timing has been changed to make it match the audio
    // timeline. Used for status line information only.
    double total_avsync_change;
    // A-V sync difference when last frame was displayed. Kept to display
    // the same value if the status line is updated at a time where no new
    // video frame is shown.
    double last_av_difference;
    /* timestamp of video frame currently visible on screen
     * (or at least queued to be flipped by VO) */
    double video_pts;
    // Last seek target.
    double last_seek_pts;
    // Frame duration field from demuxer. Only used for duration of the last
    // video frame.
    double last_frame_duration;
    // Video PTS, or audio PTS if video has ended.
    double playback_pts;
    // For logging only.
    double logged_async_diff;

    int last_chapter;

    // Past timestamps etc.
    // The newest frame is at index 0.
    struct frame_info *past_frames;
    int num_past_frames;

    double last_idle_tick;
    double next_cache_update;

    double sleeptime;      // number of seconds to sleep before next iteration

    double mouse_timer;
    unsigned int mouse_event_ts;
    bool mouse_cursor_visible;

    // used to prevent hanging in some error cases
    double start_timestamp;

    // Timestamp from the last time some timing functions read the
    // current time, in nanoseconds.
    // Used to turn a new time value to a delta from last time.
    int64_t last_time;

    struct seek_params seek;

    /* Heuristic for relative chapter seeks: keep track which chapter
     * the user wanted to go to, even if we aren't exactly within the
     * boundaries of that chapter due to an inaccurate seek. */
    int last_chapter_seek;
    bool last_chapter_flag;

    bool paused;            // internal pause state
    bool playback_active;   // not paused, restarting, loading, unloading
    bool in_playloop;

    // step this many frames, then pause
    int step_frames;
    // Counted down each frame, stop playback if 0 is reached. (-1 = disable)
    int max_frames;
    bool playing_msg_shown;

    int remaining_file_loops;
    int remaining_ab_loops;

    bool paused_for_cache;
    bool demux_underrun;
    double cache_stop_time;
    int cache_buffer;
    double cache_update_pts;

    // Set after showing warning about decoding being too slow for realtime
    // playback rate. Used to avoid showing it multiple times.
    bool drop_message_shown;

    struct screenshot_ctx *screenshot_ctx;
    struct command_ctx *command_ctx;
    struct encode_lavc_context *encode_lavc_ctx;

    struct mp_option_callback *option_callbacks;
    int num_option_callbacks;

    struct mp_ipc_ctx *ipc_ctx;

    int64_t builtin_script_ids[9];

    mp_mutex abort_lock;

    // --- The following fields are protected by abort_lock
    struct mp_abort_entry **abort_list;
    int num_abort_list;
    bool abort_all; // during final termination

    // --- Owned by MPContext
    mp_thread open_thread;
    bool open_active; // open_thread is a valid thread handle, all setup
    atomic_bool open_done;
    // --- All fields below are immutable while open_active is true.
    //     Otherwise, they're owned by MPContext.
    struct mp_cancel *open_cancel;
    char *open_url;
    char *open_format;
    int open_url_flags;
    bool open_for_prefetch;
    bool demuxer_changed;
    // --- All fields below are owned by open_thread, unless open_done was set
    //     to true.
    struct demuxer *open_res_demuxer;
    int open_res_error;

    struct mp_als *als_state; // lazily initialized on first use
} MPContext;

// Contains information about an asynchronous work item, how it can be aborted,
// and when. All fields are protected by MPContext.abort_lock.
struct mp_abort_entry {
    // General conditions.
    bool coupled_to_playback;   // trigger when playback is terminated
    // Actual trigger to abort the work. Pointer immutable, owner may access
    // without holding the abort_lock.
    struct mp_cancel *cancel;
    // For client API.
    struct mpv_handle *client;  // non-NULL if done by a client API user
    int client_work_type;       // client API type, e.h. MPV_EVENT_COMMAND_REPLY
    uint64_t client_work_id;    // client API user reply_userdata value
                                // (only valid if client_work_type set)
};

// U+25CB WHITE CIRCLE
// U+25CF BLACK CIRCLE
#define WHITE_CIRCLE "\xe2\x97\x8b"
#define BLACK_CIRCLE "\xe2\x97\x8f"

// audio.c
void reset_audio_state(struct MPContext *mpctx);
void reinit_audio_chain(struct MPContext *mpctx);
int init_audio_decoder(struct MPContext *mpctx, struct track *track);
int reinit_audio_filters(struct MPContext *mpctx);
double playing_audio_pts(struct MPContext *mpctx);
void fill_audio_out_buffers(struct MPContext *mpctx);
double written_audio_pts(struct MPContext *mpctx);
void clear_audio_output_buffers(struct MPContext *mpctx);
void update_playback_speed(struct MPContext *mpctx);
void uninit_audio_out(struct MPContext *mpctx);
void uninit_audio_chain(struct MPContext *mpctx);
void reinit_audio_chain_src(struct MPContext *mpctx, struct track *track);
float audio_get_gain(struct MPContext *mpctx);
void audio_update_volume(struct MPContext *mpctx);
void reload_audio_output(struct MPContext *mpctx);
void audio_start_ao(struct MPContext *mpctx);

// configfiles.c
void mp_parse_cfgfiles(struct MPContext *mpctx);
void mp_load_auto_profiles(struct MPContext *mpctx);
bool mp_load_playback_resume(struct MPContext *mpctx, const char *file);
char *mp_get_playback_resume_dir(struct MPContext *mpctx);
void mp_write_watch_later_conf(struct MPContext *mpctx);
void mp_delete_watch_later_conf(struct MPContext *mpctx, const char *file);
struct playlist_entry *mp_check_playlist_resume(struct MPContext *mpctx,
                                                struct playlist *playlist);

// loadfile.c
void mp_abort_playback_async(struct MPContext *mpctx);
void mp_abort_add(struct MPContext *mpctx, struct mp_abort_entry *abort);
void mp_abort_remove(struct MPContext *mpctx, struct mp_abort_entry *abort);
void mp_abort_recheck_locked(struct MPContext *mpctx,
                             struct mp_abort_entry *abort);
void mp_abort_trigger_locked(struct MPContext *mpctx,
                             struct mp_abort_entry *abort);
int mp_add_external_file(struct MPContext *mpctx, char *filename,
                         enum stream_type filter, struct mp_cancel *cancel,
                         enum track_flags flags);
void mark_track_selection(struct MPContext *mpctx, int order,
                          enum stream_type type, int value);
#define FLAG_MARK_SELECTION 1
void mp_switch_track(struct MPContext *mpctx, enum stream_type type,
                     struct track *track, int flags);
void mp_switch_track_n(struct MPContext *mpctx, int order,
                       enum stream_type type, struct track *track, int flags);
void mp_deselect_track(struct MPContext *mpctx, struct track *track);
struct track *mp_track_by_tid(struct MPContext *mpctx, enum stream_type type,
                              int tid);
void add_demuxer_tracks(struct MPContext *mpctx, struct demuxer *demuxer);
bool mp_remove_track(struct MPContext *mpctx, struct track *track);
struct playlist_entry *mp_next_file(struct MPContext *mpctx, int direction,
                                    bool force, bool update_loop);
void mp_set_playlist_entry(struct MPContext *mpctx, struct playlist_entry *e);
void mp_play_files(struct MPContext *mpctx);
void update_demuxer_properties(struct MPContext *mpctx);
bool track_is_visible(struct MPContext *mpctx, struct track *track);
void print_track_list(struct MPContext *mpctx, const char *msg);
void reselect_demux_stream(struct MPContext *mpctx, struct track *track,
                           bool refresh_only);
void prepare_playlist(struct MPContext *mpctx, struct playlist *pl, bool overwrite_current);
void autoload_external_files(struct MPContext *mpctx, struct mp_cancel *cancel);
struct track *select_default_track(struct MPContext *mpctx, int order,
                                   enum stream_type type);
void prefetch_next(struct MPContext *mpctx);
void update_lavfi_complex(struct MPContext *mpctx);
void update_vo_chain_el_pair(struct MPContext *mpctx);

// main.c
int mp_initialize(struct MPContext *mpctx, char **argv);
struct MPContext *mp_create(void);
void mp_destroy(struct MPContext *mpctx);
void mp_print_version(struct mp_log *log, int always);
void mp_update_logging(struct MPContext *mpctx, bool preinit);
void issue_refresh_seek(struct MPContext *mpctx, enum seek_precision min_prec);

// misc.c
double rel_time_to_abs(struct MPContext *mpctx, struct m_rel_time t);
double get_play_end_pts(struct MPContext *mpctx);
double get_play_start_pts(struct MPContext *mpctx);
bool get_ab_loop_times(struct MPContext *mpctx, double t[2]);
void merge_playlist_files(struct playlist *pl);
void update_content_type(struct MPContext *mpctx, struct track *track);
void update_vo_playback_state(struct MPContext *mpctx);
void update_window_title(struct MPContext *mpctx, bool force);
void error_on_track(struct MPContext *mpctx, struct track *track);
int stream_dump(struct MPContext *mpctx, const char *source_filename);
double get_track_seek_offset(struct MPContext *mpctx, struct track *track);
char *mp_format_track_metadata(void *ctx, struct track *t, bool add_lang);
const char *mp_find_non_filename_media_title(MPContext *mpctx);

// osd.c
void set_osd_bar(struct MPContext *mpctx, int type,
                 double min, double max, double neutral, double val);
bool set_osd_msg(struct MPContext *mpctx, int level, int time,
                 const char* fmt, ...) MP_PRINTF_ATTRIBUTE(4,5);
void set_osd_function(struct MPContext *mpctx, int osd_function);
void term_osd_clear_subs(struct MPContext *mpctx);
void term_osd_set_subs(struct MPContext *mpctx, const char *text, int order);
void get_current_osd_sym(struct MPContext *mpctx, char *buf, size_t buf_size);
void set_osd_bar_chapters(struct MPContext *mpctx, int type);

// playloop.c
void mp_wait_events(struct MPContext *mpctx);
void mp_set_timeout(struct MPContext *mpctx, double sleeptime);
void mp_wakeup_core(struct MPContext *mpctx);
void mp_wakeup_core_cb(void *ctx);
void mp_core_lock(struct MPContext *mpctx);
void mp_core_unlock(struct MPContext *mpctx);
void handle_option_callbacks(struct MPContext *mpctx);
double get_relative_time(struct MPContext *mpctx);
void reset_playback_state(struct MPContext *mpctx);
void set_pause_state(struct MPContext *mpctx, bool user_pause);
void update_internal_pause_state(struct MPContext *mpctx);
void update_core_idle_state(struct MPContext *mpctx);
void add_step_frame(struct MPContext *mpctx, int dir, bool use_seek);
void step_frame_mute(struct MPContext *mpctx, bool mute);
void queue_seek(struct MPContext *mpctx, enum seek_type type, double amount,
                enum seek_precision exact, int flags);
double get_time_length(struct MPContext *mpctx);
double get_start_time(struct MPContext *mpctx, int dir);
double get_current_time(struct MPContext *mpctx);
double get_playback_time(struct MPContext *mpctx);
double get_current_pos_ratio(struct MPContext *mpctx, bool use_range);
int get_current_chapter(struct MPContext *mpctx);
char *chapter_display_name(struct MPContext *mpctx, int chapter);
char *chapter_name(struct MPContext *mpctx, int chapter);
double chapter_start_time(struct MPContext *mpctx, int chapter);
int get_chapter_count(struct MPContext *mpctx);
int get_cache_buffering_percentage(struct MPContext *mpctx);
void execute_queued_seek(struct MPContext *mpctx);
void run_playloop(struct MPContext *mpctx);
void mp_idle(struct MPContext *mpctx);
void idle_loop(struct MPContext *mpctx);
int handle_force_window(struct MPContext *mpctx, bool force);
void seek_to_last_frame(struct MPContext *mpctx);
void update_screensaver_state(struct MPContext *mpctx);
void update_ab_loop_clip(struct MPContext *mpctx);
bool get_internal_paused(struct MPContext *mpctx);

// scripting.c
struct mp_script_args {
    const struct mp_scripting *backend;
    struct MPContext *mpctx;
    struct mp_log *log;
    struct mpv_handle *client;
    const char *filename;
    const char *path;
};
struct mp_scripting {
    const char *name;       // e.g. "lua script"
    const char *file_ext;   // e.g. "lua"
    bool no_thread;         // don't run load() on dedicated thread
    int (*load)(struct mp_script_args *args);
};
bool mp_load_scripts(struct MPContext *mpctx);
void mp_load_builtin_scripts(struct MPContext *mpctx);
int64_t mp_load_user_script(struct MPContext *mpctx, const char *fname);

// sub.c
void redraw_subs(struct MPContext *mpctx);
void reset_subtitle_state(struct MPContext *mpctx);
void reinit_sub(struct MPContext *mpctx, struct track *track);
void reinit_sub_all(struct MPContext *mpctx);
void uninit_sub(struct MPContext *mpctx, struct track *track);
void uninit_sub_all(struct MPContext *mpctx);
void update_osd_msg(struct MPContext *mpctx);
bool update_subtitles(struct MPContext *mpctx, double video_pts);

// video.c
void reset_video_state(struct MPContext *mpctx);
int init_video_decoder(struct MPContext *mpctx, struct track *track);
void reinit_video_chain(struct MPContext *mpctx);
void reinit_video_chain_src(struct MPContext *mpctx, struct track *track);
int reinit_video_filters(struct MPContext *mpctx);
void write_video(struct MPContext *mpctx);
void mp_force_video_refresh(struct MPContext *mpctx);
void uninit_video_out(struct MPContext *mpctx);
void uninit_video_chain(struct MPContext *mpctx);
double calc_average_frame_duration(struct MPContext *mpctx);

#endif /* MPLAYER_MP_CORE_H */
--*/
--/*
MPV_EVENT_END_FILE = 7;
MPV_EVENT_FILE_LOADED = 8;

libmpv-2.32bit.ca25.dll: (just mpv_*, same as libmpv-2.64bit.ca25.dll and .ca20s bar mpv_get_time_ns)
mpv_abort_async_command,
mpv_client_api_version,
mpv_client_id,
mpv_client_name,
mpv_command,
mpv_command_async,
mpv_command_node,
mpv_command_node_async,
mpv_command_ret,
mpv_command_string,
mpv_create,
mpv_create_client,
mpv_create_weak_client,
mpv_del_property,
mpv_destroy,
mpv_error_string,
mpv_event_name,
mpv_event_to_node,
mpv_free,
mpv_free_node_contents,
mpv_get_property,
mpv_get_property_async,
mpv_get_property_osd_string,
mpv_get_property_string,
mpv_get_time_ns, -- (not 2.0)
mpv_get_time_us,
mpv_get_wakeup_pipe,
mpv_hook_add,
mpv_hook_continue,
mpv_initialize,
mpv_load_config_file,
mpv_observe_property,
mpv_render_context_create,
mpv_render_context_free,
mpv_render_context_get_info,
mpv_render_context_render,
mpv_render_context_report_swap,
mpv_render_context_set_parameter,
mpv_render_context_set_update_callback,
mpv_render_context_update,
mpv_request_event,
mpv_request_log_messages,
mpv_set_option,
mpv_set_option_string,
mpv_set_property,
mpv_set_property_async,
mpv_set_property_string,
mpv_set_wakeup_callback,
mpv_stream_cb_add_ro,
mpv_terminate_destroy,
mpv_unobserve_property,
mpv_wait_async_requests,
mpv_wait_event,
mpv_wakeup,

--*/

