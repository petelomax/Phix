--
-- mpfr.e
--
--  mpir is a fork of gmp, and mpfr is a floating-point extension of that.
--  It has been wrapped/is available for several programs languages, including
--  Ada, C++, Fortran, Haskell, Java, Julia, Lisp, .NET, OCaml, Perl, PHP, Pike, 
--  Prolog, Python, R, Racket, Ruby, Rust, Scheme, and now Phix - which means
--  there should be no shortage of examples.
--
--  Download the dlls from: http://www.atelierweb.com/mpir-and-mpfr/
--                 or PCAN: http://phix.x10.mx/pmwiki/pmwiki.php?n=Main.Mpfr
--  Version: mpfr: 3.1.5, mpir:2.7.2, two dlls: 32 bit: 733K (308K zipped), 
--                                              64 bit: 936K (373K zipped)
--  Version 3.1.0-p3 was already installed my Ubuntu box, though I also needed 
--  to install mpfr-dev, in order to compile the recommended version check, 
--  which was just the one click via the ubuntu software centre, at least after 
--  typing "mpfr" into the search box and picking the one that I needed.
--
--  As usual, docs will be build in tandem with this wrapper.
--
-- DEV/SUG the current system, ie/eg:
--
--      integer x_mpz_cmp_si = NULL
--      ...
--      if x_mpz_cmp_si=NULL then
--          x_mpz_cmp_si = link_c_func(mpir_dll, "+__gmpz_cmp_si", {P,I},I)
--      end if
--      integer res = c_func(x_mpz_cmp_si,{op1,op2})
--
-- does not properly lend itself to unit testing of a new dll. Better:
--
--X     constant dx_mpir_dll = mpfr_open_dll("???") -- [hmmm...]
--      enum MPIR,MPFR
--      sequence dll_handles = {NULL,NULL}, -- (erm)
--               dll_links = {}
--
--      /*local/private*/ 
--      function mpfr_link_c_func(integer dll, string name, sequence args, integer res)
--          dll_links = append(dll_links,{dll,name,args,res})
--      --  if flag then ??? end if
--          return length(dll_links)
--      end function
--      ...
--X     constant dx_mpz_cmp_si = mpfr_link_c_func(dx_mpir_dll, "+__gmpz_cmp_si", {P,I},I)
--      constant dx_mpz_cmp_si = mpfr_link_c_func(MPIR, "+__gmpz_cmp_si", {P,I},I)
--      ...
--      integer res = mpfr_c_func(dx_mpz_cmp_si,{op1,op2})
--
--  Obviously we normally defer all opens etc until actually needed, but 
--  we can also set an optional flag to open all asap, as a unit test.
--

include builtins\pincpathN.e -- include_path() (temp/0.8.0+)
include builtins\ptypes.e

constant W = machine_word()     -- (4 or 8)

constant
         D  = C_DOUBLE, 
         I  = C_INT,
--       I  = C_LONG,
--       I  = iff(machine_bits()=32 or platform()=WINDOWS ? C_INT  : C_INT64),
--       I  = iff(machine_bits()=32 ? C_INT  : C_INT64),
--       L  = C_LONG,
         P  = C_POINTER, 
         $
--       F  = C_FLOAT,
--       L  = C_LONG,
--       U  = C_UINT,
--       UC = C_UCHAR,
--       UL = C_ULONG,

--DEV remove if slowdown is neglible... [DEV it is]
constant debug_types = true

function peek4(string s4)
    atom mem = allocate(4)
    poke(mem,s4)
    atom res = peek4u(mem)
    free(mem)
    return res
end function

constant MPZ_T = peek4("mpz\0"),
         MPZ_Q = peek4("mpq\0"),
         MPZ_R = peek4("rnd\0"),
         MPFR_T = peek4("mpfr")

global type mpz(object o)
    if not atom(o) then return false end if
    if debug_types and o!=NULL then
        try
            if peek4s(o+W*3)!=MPZ_T then return false end if
        catch e
            return false
        end try
    end if
    return true
end type

global type mpz_or_string(object o)
    return string(o) or mpz(o)
end type

global type mpq(object o)
    if not atom(o) then return false end if
    if debug_types and o!=NULL then
        try
            if peek4s(o+W*6)!=MPZ_Q then return false end if
        catch e
            return false
        end try
    end if
    return true
end type

global type randstate(object o)
    if not atom(o) then return false end if
    if debug_types and o!=NULL then
        try
            if peek4s(o+W*5)!=MPZ_R then return false end if
        catch e
            return false
        end try
    end if
    return true
end type

global type mpfr(object o)
    if not atom(o) then return false end if
    if debug_types and o!=NULL then
        try
            if peek4s(o+W*4)!=MPFR_T then return false end if
        catch e
            return false
        end try
    end if
    return true
end type

procedure Abort(string msg)
    puts(1,msg)
    {} = wait_key()
    abort(0)
end procedure

-- Technical note: dll=NULL in either of the next two routines **IS** an error,
--                 especially when called from any routine that gets an mpfr/mpz
--                 parameter, but not (quite as much so) from mpfr/mpz_init().

function link_c_func(atom dll, sequence name, sequence args, atom result)
    if dll=NULL then ?9/0 end if
--  integer rid = define_c_func(dll, "+" & name, args, result)
    integer rid = define_c_func(dll, name, args, result)
    if rid<1 then Abort("cannot link "&name) end if
    return rid
end function

function link_c_proc(atom dll, sequence name, sequence args)
    if dll=NULL then ?9/0 end if
--  integer rid = define_c_proc(dll, "+" & name, args)
    integer rid = define_c_proc(dll, name, args)
    if rid<1 then Abort("cannot link "&name) end if
    return rid
end function

atom mpir_dll = NULL,
     mpfr_dll = NULL
string missing_dll  -- see mpir_open_dll()

procedure open_mpir_dll(string dll_name="", boolean mpir_only=false, fatal=true)
--
-- internal: via if mpXr_dll=NULL then open_mpir_dll() end if, or
--           from mpir_open_dll as open_mpir_dll(dll_name,mpir_only,false).
--
    string dll_path = include_path("builtins")
    if platform()=LINUX then
        if dll_name="" then
-- maybe:
--          string arch = iff(machine_bits()=32?"i386":"x86_64")
--          dll_name = "/usr/lib/"&arch&"-linux-gnu/mpir.so"
            dll_name = "libmpir.so"
--          dll_names = {"mpir.so","mpir-3.so"}
        end if
    elsif platform()=WINDOWS then
--DEV copied from pGUI (which uses 120/2013), builtins copy not tested...
--MSVCP100.dll/MSVCR100.dll
        -- Aside: normally I'd expect msvcp/r100.dll to be loaded from system32/syswow64, 
        --        but if someone puts copies in builtins, it should be alright.
        --        You could also try deleting this test and see if it works anyway, but
        --        don't blame me if that gets you an even more cryptic error message.
        --        (This all depends on how the pre-built binaries were built, natch.)
        string curr_dir = current_dir()
        if dll_path!="" and chdir(dll_path)=0 then ?9/0 end if
        if open_dll("msvcp100.dll")=0 then
            puts(1,"fatal error: msvcp100.dll could not be loaded\n")
            puts(1," try installing Visual C++ Redistributable Packages for Visual Studio 2010\n")
            if machine_bits()=32 then
                puts(1," from http://www.microsoft.com/en-in/download/details.aspx?id=5555 \n")
                -- ( http://www.microsoft.com/en-in/download/details.aspx?id=5555 )
            else
                puts(1," from http://www.microsoft.com/en-us/download/details.aspx?id=14632 \n")
                -- ( http://www.microsoft.com/en-us/download/details.aspx?id=14632 )
            end if
            {} = wait_key()
            ?9/0
        end if
        if chdir(curr_dir)=0 then ?9/0 end if

        if dll_name="" then
            dll_name = sprintf("mpir%d.dll",machine_bits()) -- 2.7.2
--          dll_name = "mpir.dll"       -- 2.6.0
--          dll_names = {dll_name}
        end if
    else
        ?9/0 -- unknown platform
    end if
    sequence dll_names = {dll_name}
if mpir_dll!=NULL then ?9/0 end if
if mpfr_dll!=NULL then ?9/0 end if
    for i=1 to length(dll_names) do
        dll_name = dll_names[i]
        mpir_dll = open_dll(dll_name)
        if mpir_dll!=NULL then exit end if
        dll_name = join_path({dll_path,dll_name})
        mpir_dll = open_dll(dll_name)
        if mpir_dll!=NULL then exit end if
    end for
    if mpir_dll!=NULL then
        if not mpir_only then
            dll_name = substitute(dll_name,"mpir","mpfr")
            mpfr_dll = open_dll(dll_name)
            if mpfr_dll=NULL then ?9/0 end if
        end if
--      integer x_mpfr_buildopt_tls_p = link_c_func(mpfr_dll,"mpfr_buildopt_tls_p",{},I)
--      ?{"mpfr_buildopt_tls_p",c_func(x_mpfr_buildopt_tls_p,{})}   -- 0

    else
        missing_dll = dll_names[1]
        if fatal then
            string msg = missing_dll&" not found. "
            if platform()=LINUX then
                msg &= "Fix your mpfr install.\n"&
                       "Use your package manager, or see https://www.mpfr.org\n"
                dll_name = ""
            elsif platform()=WINDOWS then
--DEV rewrite (create the PCAN page first) alt: http://phix.x10.mx/pmwiki/uploads/Mpfr315Mpir272.zip
--SUG or, (on windows anyway) offer to download?
--BETTER: write a pGUI demo to download something, and unzip/install/run it.
                msg &= "Obtain from http://phix.x10.mx/pmwiki/pmwiki.php?n=Main.Mpfr or\n"&
                       "http://www.atelierweb.com/mpir-and-mpfr, and install to\n"&
                       "system32/syswow64, builtins, or application directory.\n"
            else
                ?9/0 -- unknown platform
            end if
            Abort(msg)
        end if
    end if
end procedure

global function mpir_open_dll(string dll_name="", boolean mpir_only=false)
--
-- optional, returns the name of a dll/so file it cannot open, or "" on success.
-- If dll/so are missing, all the other routines terminate with a fatal error.
-- An application might invoke this and on failure display further instructions
-- or help text, or offer to download, etc, rather than said crash.
-- Obviously, the returned missing_dll is more useful when "" was passed in,
-- but length()!=0 remains meaningful even when actual value already known.
--
-- I guess it wouldn't hurt to mpfr_open("mpir.dll") if that's the only one
-- you're using/shipping [untested]. Maybe we could fairly easily just add 
-- mpz_open_dll(), and a flag/index to the internal open_mpir_dll() above?..
--
--  if mpfr_dll=NULL then
    if mpir_dll=NULL then
        missing_dll = ""
        open_mpir_dll(dll_name, mpir_only, false)
        return missing_dll
    end if
    return ""   -- OK
end function

integer x_mpfr_get_version = NULL   -- (aka mpfr_version)
atom p_mpir_version = NULL,
     p_gmp_version = NULL

global function mpir_get_versions(boolean bAsNumSeq=false)
--
-- Returns a 3-element sequence of three versions (mpfr, mpir, and gmp)
-- The default of bAsNumSeq=false returns 3 strings, for example
--      {"3.1.5","2.7.2","5.1.3"}
-- whereas a true yields a triplet of triplets, for example
--      {{3,1,5},{2,7,2},{5,1,3}}.
-- Obviously the strings are easier to display, whereas numbers will
-- give a better answer than comparing some "9" and "10" part might.
-- There is no harm whatsoever in calling this more than once.
--
    if x_mpfr_get_version=NULL then
--      if mpfr_dll=NULL then open_mpir_dll() end if
        if mpir_dll=NULL then open_mpir_dll() end if
        if mpfr_dll!=NULL then
            x_mpfr_get_version = link_c_func(mpfr_dll, "+mpfr_get_version", {}, P)
        end if
        p_mpir_version = define_c_var(mpir_dll, "__mpir_version")
        p_gmp_version = define_c_var(mpir_dll, "__gmp_version")
    end if
    sequence res = {iff(mpfr_dll==NULL?"mpfr_dll==NULL":peek_string(c_func(x_mpfr_get_version,{}))),
--29/4/19:
--                  peek_string(peek4u(p_mpir_version)),
                    peek_string(peekNS(p_mpir_version,W,0)),
--                  peek_string(peek4u(p_gmp_version))}
                    peek_string(peekNS(p_gmp_version,W,0))}
    if bAsNumSeq then
        for i=iff(mpfr_dll==NULL?2:1) to length(res) do
            res[i] = substitute(res[i],".","-")
            {res[i]} = scanf(res[i],"%d-%d-%d")
        end for
    end if
    return res
end function

integer x_mpz_set = NULL

global procedure mpz_set(mpz rop, op)
-- rop := op
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpz_set=NULL then
        x_mpz_set = link_c_proc(mpir_dll, "+__gmpz_set", {P,P})
    end if
    c_proc(x_mpz_set,{rop,op})
end procedure

integer x_mpz_set_si = NULL

global procedure mpz_set_si(mpz rop, integer op)
-- Set x to a phix integer
    if rop=NULL then ?9/0 end if
    if x_mpz_set_si=NULL then
        x_mpz_set_si = link_c_proc(mpir_dll, "+__gmpz_set_si", {P,I})
    end if
    c_proc(x_mpz_set_si,{rop,op})
end procedure

integer x_mpz_set_d = NULL

global procedure mpz_set_d(mpz rop, atom op)
-- Set x to a phix atom
    if rop=NULL then ?9/0 end if
    if x_mpz_set_d=NULL then
        x_mpz_set_d = link_c_proc(mpir_dll, "+__gmpz_set_d", {P,D})
    end if
    c_proc(x_mpz_set_d,{rop,op})
end procedure
if "abc"="def" then mpz_set_d(NULL,0) end if    --DEV/temp

function replace_e(string s)
-- allow strings such as "1e200" or even "2.5e1" (as long as integer overall)
    s = lower(substitute(substitute(s,",",""),"_",""))
    integer e = find('e',s)
    if e then
        {s,e} = {s[1..e-1],to_number(s[e+1..$])}
        integer d = find('.',s)
        if d then
            e -= (length(s)-d)
            s[d..d] = ""
        end if
        s &= repeat('0',e)  -- (-ve e is expected to crash)
    end if
    return s
end function

integer x_mpz_set_str = NULL

global procedure mpz_set_str(mpz rop, string s, integer base=0)
    if rop=NULL then ?9/0 end if
    if base<0 or base=1 or base>62 then ?9/0 end if
    if x_mpz_set_str=NULL then
        x_mpz_set_str = link_c_func(mpir_dll, "+__gmpz_set_str", {P,P,I},I)
    end if
    s = replace_e(s)
    if c_func(x_mpz_set_str,{rop,s,base})!=0 then ?9/0 end if
end procedure
--DEV (NEWGSCAN)
if "abc" = "def" then mpz_set_str(NULL,"") end if

integer x_mpz_import = NULL

global procedure mpz_import(mpz rop, integer count, order, size, endian, nails, atom_string op)
    if rop=NULL then ?9/0 end if
    if count<1 then ?9/0 end if
    if order!=1 and order!=-1 then ?9/0 end if
    if not find(size,{1,2,4,8}) then ?9/0 end if    -- (blind stab)
    if not find(endian,{1,0,-1}) then ?9/0 end if
    if nails<0 then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpz_import=NULL then
        x_mpz_import = link_c_proc(mpir_dll, "+__gmpz_import", {P,I,I,I,I,I,P})
    end if
    c_proc(x_mpz_import,{rop,count,order,size,endian,nails,op})
end procedure

integer x_mpz_export = NULL

global function mpz_export(atom pMem, integer order, size, endian, nails, mpz op)
    if pMem=NULL then ?9/0 end if
    if order!=1 and order!=-1 then ?9/0 end if
    if not find(size,{1,2,4,8}) then ?9/0 end if    -- (blind stab)
    if not find(endian,{1,0,-1}) then ?9/0 end if
    if nails<0 then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpz_export=NULL then
        x_mpz_export = link_c_proc(mpir_dll, "+__gmpz_export", {P,I,I,I,I,I,P})
    end if
    atom pCount = allocate(W,1)
    c_proc(x_mpz_export,{pMem,pCount,order,size,endian,nails,op})
    integer count = peekNS(pCount,W,0)
    return count
end function

integer x_gmp_init = NULL,
        x_gmp_init2 = NULL,
--      x_gmp_init_set = NULL,
        x_gmp_init_set_d = NULL,
        x_gmp_init_set_si = NULL,
        x_gmp_init_set_str = NULL

procedure _mpz_init2(atom x, object v=0, integer bitcount=0)
-- (internal)
    if x=NULL then ?9/0 end if
    if x_gmp_init=NULL then
        if mpir_dll=NULL then open_mpir_dll() end if
        x_gmp_init = link_c_proc(mpir_dll, "+__gmpz_init", {P})
        x_gmp_init2 = link_c_proc(mpir_dll, "+__gmpz_init2", {P,I})
--      x_gmp_init_set = link_c_proc(mpir_dll, "+__gmpz_init_set", {P,P})
        x_gmp_init_set_d = link_c_proc(mpir_dll, "+__gmpz_init_set_d", {P,D})
        x_gmp_init_set_si = link_c_proc(mpir_dll, "+__gmpz_init_set_si", {P,I})
        x_gmp_init_set_str = link_c_func(mpir_dll, "+__gmpz_init_set_str", {P,P,I},I)
    end if
    if bitcount!=0 then
        c_proc(x_gmp_init2,{x,bitcount})
        -- (Ah, now I understand why the C lib doesn't permit a bitcount..)
        -- (The first version of this incorrectly invoked gmp_init's twice)
        if v!=0 then
--          if mpz(v) then                  -- NO!!
--              mpz_set(x,v)
            if integer(v) then
                mpz_set_si(x,v)
            elsif atom(v) then
                mpz_set_d(x,v)
            elsif string(v) then
                v = substitute(v,"_","")
                mpz_set_str(x,v)
            else
                ?9/0 -- what's v??
            end if
        end if
    else
        if v=0 then
            c_proc(x_gmp_init,{x})
--      elsif mpz(v) then                   -- NO!!
--          c_proc(x_gmp_init_set,{x,v})
        elsif integer(v) then
            c_proc(x_gmp_init_set_si,{x,v})
        elsif atom(v) then
            c_proc(x_gmp_init_set_d,{x,v})
        elsif string(v) then
            v = replace_e(v)
            if c_func(x_gmp_init_set_str,{x,v,0})!=0 then ?9/0 end if
        else
            ?9/0 -- what's v??
        end if
    end if
end procedure

integer x_mpz_clear = NULL

procedure _mpz_clear(mpz x)
--
-- (internal)
-- Frees any mpz-side memory associated with x, but not the phix-side
-- as was allocated within mpz_init(); mpz_clear() below does both.
--
    if x=NULL then ?9/0 end if
    if x_mpz_clear=NULL then
        x_mpz_clear = link_c_proc(mpir_dll, "+__gmpz_clear", {P})
    end if
    c_proc(x_mpz_clear,{x})
    poke4(x+3*W,0)
end procedure

procedure free_mpz(atom x)
-- (internal, delete_routine)
    if peek4s(x+W*3)=MPZ_T then _mpz_clear(x) end if
    free(x)
end procedure
constant r_free_mpz = routine_id("free_mpz")

global function mpz_init(object v=0, integer bitcount=0)
--
-- Eg: mpz x = mpz_init()
--
--  Initialise. Covers the C functions mpz_init, mpz_int2, mpz_init_set_d, mpz_init_set_si, and mpz_init_set_str
--
-- Invoke x = mpz_free(x) when the variable is no longer needed, see below (will occur automatically).
--
    atom res = allocate(4*W)    -- (extra dword for MPZ_T)
    res = delete_routine(res,r_free_mpz)
    poke4(res+3*W,MPZ_T)
    _mpz_init2(res,v,bitcount)
    return res  
end function

global function mpz_inits(integer n, object v=0)
--
-- Eg: mpz {x,y,z} = mpz_init(3)
--
--  Initialise n variables to v.
--  v may be integer, atom, string, mpz, or a sequence of length n of said.
--  Obviously the result may be stored in a sequence.
--
-- Invoke {x,y,z} = mpz_free({x,y,z}) when the variables are no longer needed, see below (will occur automatically).
--
    sequence res = repeat(0,n)
    if sequence(v) and not string(v) then
        if length(v)!=n then ?9/0 end if
        for i=1 to n do res[i] = mpz_init(v[i]) end for
    else
        for i=1 to n do res[i] = mpz_init(v) end for
    end if
    return res
end function

integer x_gmp_init_set = NULL

global function mpz_init_set(mpz src)
-- usage eg mpz x = mpz_init(...); mpz y = mpz_init_set(x)
    if src=NULL then ?9/0 end if
    if x_gmp_init_set=NULL then
        x_gmp_init_set = link_c_proc(mpir_dll, "+__gmpz_init_set", {P,P})
    end if
    atom res = mpz_init()
    c_proc(x_gmp_init_set,{res,src})
    return res
--  return mpz_init(src)        -- NO!!
end function

--global function mpz_clear(object x)
global function mpz_free(object x)
--
-- Clear and deallocate any variables created using mpz_init(). (Invoked automatically if rqd)
--
-- usage: x = mpz_free(x)
--    or: {y,z} = mpz_free({y,z})
--
--  (perhaps one day I'll implement var-id so that you won't 
--   have to specify everything twice in cases like this...)
--
    if sequence(x) then
        for i=1 to length(x) do
            _mpz_clear(x[i])
            x[i] = NULL     -- see aside
        end for
        --
        -- aside: The above x[i]=NULL does not immediately trigger 
        --        the cleanup requested (ie 2nd parameter of 1 to 
        --        allocate) in mpz_init(); the (lhs) assignment in 
        --        the calling code does. Just so that you know.
        --
        --        Notice that if we didn't set x[i] like that, the
        --        release might be further deferred by refcounts
        --        remaining in x/the hidden temp variable, that is
        --        until the latter was also released/reused.
        --
        return x
    end if
    _mpz_clear(x)
    return NULL
end function

integer x_mpz_add = NULL

global procedure mpz_add(mpz rop, op1, op2)
-- Set rop to op1 + op2.
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpz_add=NULL then
        x_mpz_add = link_c_proc(mpir_dll, "+__gmpz_add", {P,P,I})
    end if
    c_proc(x_mpz_add,{rop,op1,op2})
end procedure

integer x_mpz_add_ui = NULL

global procedure mpz_add_ui(mpz rop, op1, integer op2)
-- Set rop to op1 + op2.
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if op2<0 then ?9/0 end if
    if x_mpz_add_ui=NULL then
        x_mpz_add_ui = link_c_proc(mpir_dll, "+__gmpz_add_ui", {P,P,I})
    end if
    c_proc(x_mpz_add_ui,{rop,op1,op2})
end procedure

integer x_mpz_addmul = NULL

global procedure mpz_addmul(mpz rop, op1, op2)
-- Set rop to rop + op1 * op2.
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpz_addmul=NULL then
        x_mpz_addmul = link_c_proc(mpir_dll, "+__gmpz_addmul", {P,P,I})
    end if
    c_proc(x_mpz_addmul,{rop,op1,op2})
end procedure

integer x_mpz_addmul_ui = NULL

global procedure mpz_addmul_ui(mpz rop, op1, integer op2)
-- Set rop to rop + op1 * op2.
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if op2<0 then ?9/0 end if
    if x_mpz_addmul_ui=NULL then
        x_mpz_addmul_ui = link_c_proc(mpir_dll, "+__gmpz_addmul_ui", {P,P,I})
    end if
    c_proc(x_mpz_addmul_ui,{rop,op1,op2})
end procedure

integer x_mpz_sub = NULL

global procedure mpz_sub(mpz rop, op1, op2)
-- rop = op1 - op2
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpz_sub=NULL then
        x_mpz_sub = link_c_proc(mpir_dll, "+__gmpz_sub", {P,P,P})
    end if
    c_proc(x_mpz_sub,{rop,op1,op2})
end procedure

integer x_mpz_sub_ui = NULL

global procedure mpz_sub_ui(mpz rop, op1, integer op2)
-- rop = op1 - op2
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if op2<0 then ?9/0 end if
    if x_mpz_sub_ui=NULL then
        x_mpz_sub_ui = link_c_proc(mpir_dll, "+__gmpz_sub_ui", {P,P,I})
    end if
    c_proc(x_mpz_sub_ui,{rop,op1,op2})
end procedure

integer x_mpz_ui_sub = NULL

global procedure mpz_ui_sub(mpz rop, integer op1, mpz op2)
-- rop = op1 - op2
    if rop=NULL then ?9/0 end if
    if op1<0 then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpz_ui_sub=NULL then
        x_mpz_ui_sub = link_c_proc(mpir_dll, "+__gmpz_ui_sub", {P,I,P})
    end if
    c_proc(x_mpz_ui_sub,{rop,op1,op2})
end procedure

global procedure mpz_add_si(atom rop, op1, integer op2)
    if op2<0 then
        mpz_sub_ui(rop,op1,-op2)
    else
        mpz_add_ui(rop,op1,op2)
    end if
end procedure

global procedure mpz_sub_si(atom rop, op1, integer op2)
    if op2<0 then
        mpz_add_ui(rop,op1,-op2)
    else
        mpz_sub_ui(rop,op1,op2)
    end if
end procedure

integer x_mpz_abs = NULL

global procedure mpz_abs(mpz rop, op)
-- rop = abs(op)
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpz_abs=NULL then
        x_mpz_abs = link_c_proc(mpir_dll, "+__gmpz_abs", {P,P})
    end if
    c_proc(x_mpz_abs,{rop,op})
end procedure

integer x_mpz_neg = NULL

global procedure mpz_neg(mpz rop, op)
-- rop = -op
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpz_neg=NULL then
        x_mpz_neg = link_c_proc(mpir_dll, "+__gmpz_neg", {P,P})
    end if
    c_proc(x_mpz_neg,{rop,op})
end procedure

integer x_mpz_mul = NULL

global procedure mpz_mul(mpz rop, op1, op2)
-- rop = op1 * op2
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpz_mul=NULL then
        x_mpz_mul = link_c_proc(mpir_dll, "+__gmpz_mul", {P,P,P})
    end if
    c_proc(x_mpz_mul,{rop,op1,op2})
end procedure

integer x_mpz_mul_si = NULL

global procedure mpz_mul_si(mpz rop, op1, integer op2)
-- Set rop to op1 * op2, where op2 is +/-1GB.
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if x_mpz_mul_si=NULL then
        x_mpz_mul_si = link_c_proc(mpir_dll, "+__gmpz_mul_si", {P,P,I})
    end if
    c_proc(x_mpz_mul_si,{rop,op1,op2})
end procedure

mpz muld = NULL     -- (/not/ thread safe..)

global procedure mpz_mul_d(mpz rop, op1, atom op2)
-- as mpz_mul_si() except op2 is a phix atom.
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
--maybe/untested:
--if integer(op2) then
--  mpz_mul_si(rop,op1,op2)
--else
    if muld=NULL then
        muld = mpz_init(op2)
    else
        mpz_set_d(muld,op2)
    end if
    mpz_mul(rop,op1,muld)
--end if
end procedure

global procedure mpz_si_sub(atom rop, integer op1, mpz op2)
-- mpz_ui_sub() except op1 can be +/-1GB
    mpz_sub_ui(rop,op2,op1)
    mpz_mul_si(rop,rop,-1)
end procedure

integer x_mpz_mul_2exp = NULL

global procedure mpz_mul_2exp(mpz rop, op1, integer op2)
-- Set rop to op1 * 2^op2. This operation can also be defined as a left shift by op2 bits.
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if x_mpz_mul_2exp=NULL then
        x_mpz_mul_2exp = link_c_proc(mpir_dll, "+__gmpz_mul_2exp", {P,P,I})
    end if
    c_proc(x_mpz_mul_2exp,{rop,op1,op2})
end procedure

integer x_mpz_fdiv_q_2exp = NULL

global procedure mpz_fdiv_q_2exp(mpz q, n, integer b)
    if q=NULL then ?9/0 end if
    if n=NULL then ?9/0 end if
    if x_mpz_fdiv_q_2exp=NULL then
        x_mpz_fdiv_q_2exp = link_c_proc(mpir_dll, "+__gmpz_fdiv_q_2exp", {P,P,I})
    end if
    c_proc(x_mpz_fdiv_q_2exp,{q,n,b})
end procedure

integer x_mpz_fdiv_q = NULL

global procedure mpz_fdiv_q(mpz q, n, d)
-- q := floor(n/d)
    if q=NULL then ?9/0 end if
    if n=NULL then ?9/0 end if
    if d=NULL then ?9/0 end if
    if x_mpz_fdiv_q=NULL then
        x_mpz_fdiv_q = link_c_proc(mpir_dll, "+__gmpz_fdiv_q", {P,P,P})
    end if
    c_proc(x_mpz_fdiv_q,{q,n,d})
end procedure

integer x_mpz_fdiv_r = NULL

global procedure mpz_fdiv_r(mpz r, n, d)
-- r := remainder(n,d)
    if r=NULL then ?9/0 end if
    if n=NULL then ?9/0 end if
    if d=NULL then ?9/0 end if
    if x_mpz_fdiv_r=NULL then
        x_mpz_fdiv_r = link_c_proc(mpir_dll, "+__gmpz_fdiv_r", {P,P,P})
    end if
    c_proc(x_mpz_fdiv_r,{r,n,d})
end procedure

integer x_mpz_fdiv_q_ui = NULL

global function mpz_fdiv_q_ui(mpz q, n, integer d)
-- {q,res} := {floor(n/d),remainder(n,d)}
    if q=NULL then ?9/0 end if
    if n=NULL then ?9/0 end if
    if d<=0 then ?9/0 end if
    if x_mpz_fdiv_q_ui=NULL then
        x_mpz_fdiv_q_ui = link_c_func(mpir_dll, "+__gmpz_fdiv_q_ui", {P,P,I},I)
    end if
    integer res = c_func(x_mpz_fdiv_q_ui,{q,n,d})
    return res
end function

integer x_mpz_fdiv_qr = NULL

global procedure mpz_fdiv_qr(mpz q, r, n, d)
-- {q,r} := {floor(n/d),remainder(n,d)}
    if q=NULL then ?9/0 end if
    if r=NULL then ?9/0 end if
    if n=NULL then ?9/0 end if
    if d=NULL then ?9/0 end if
    if x_mpz_fdiv_qr=NULL then
        x_mpz_fdiv_qr = link_c_proc(mpir_dll, "+__gmpz_fdiv_qr", {P,P,P,P})
    end if
    c_proc(x_mpz_fdiv_qr,{q,r,n,d})
end procedure

integer x_mpz_tdiv_q_2exp = NULL

global procedure mpz_tdiv_q_2exp(mpz q, n, integer b)
-- q := trunc(n/2^b) rounds q towards zero
-- For positive n mpz_tdiv_q_2exp is a simple bitwise right shift.
-- For negative n mpz_tdiv_q_2exp effectively treats n as sign and magnitude. [untested...]
-- In all cases q and r will satisfy n = qd + r, and r will satisfy 0 <= |r| < |d|.
    if q=NULL then ?9/0 end if
    if n=NULL then ?9/0 end if
--  if b=NULL then ?9/0 end if
    if x_mpz_tdiv_q_2exp=NULL then
        x_mpz_tdiv_q_2exp = link_c_proc(mpir_dll, "+__gmpz_tdiv_q_2exp", {P,P,I})
    end if
    c_proc(x_mpz_tdiv_q_2exp,{q,n,b})
end procedure

integer x_mpz_tdiv_r_2exp = NULL

global procedure mpz_tdiv_r_2exp(mpz r, n, integer b)
-- r := remainder(n,2^b) r will have the same sign as n
-- In all cases q and r will satisfy n = qd + r, and r will satisfy 0 <= |r| < |d|.
    if r=NULL then ?9/0 end if
    if n=NULL then ?9/0 end if
--  if d=NULL then ?9/0 end if
    if x_mpz_tdiv_r_2exp=NULL then
        x_mpz_tdiv_r_2exp = link_c_proc(mpir_dll, "+__gmpz_tdiv_r_2exp", {P,P,I})
    end if
    c_proc(x_mpz_tdiv_r_2exp,{r,n,b})
end procedure

integer x_mpz_cdiv_q = NULL

global procedure mpz_cdiv_q(mpz q, n, d)
-- q := ceil(n/d)
    if q=NULL then ?9/0 end if
    if n=NULL then ?9/0 end if
    if d=NULL then ?9/0 end if
    if x_mpz_cdiv_q=NULL then
        x_mpz_cdiv_q = link_c_proc(mpir_dll, "+__gmpz_cdiv_q", {P,P,P})
    end if
    c_proc(x_mpz_cdiv_q,{q,n,d})
end procedure

integer x_mpz_divisible_p = NULL

global function mpz_divisible_p(mpz n, d)
    if n=NULL then ?9/0 end if
    if d=NULL then ?9/0 end if
    if x_mpz_divisible_p=NULL then
        x_mpz_divisible_p = link_c_func(mpir_dll, "+__gmpz_divisible_p", {P,P},I)
    end if
    boolean res = c_func(x_mpz_divisible_p,{n,d})
    return res
end function

integer x_mpz_divisible_ui_p = NULL

global function mpz_divisible_ui_p(mpz n, integer d)
    if n=NULL then ?9/0 end if
    if d<0 then ?9/0 end if
    if x_mpz_divisible_ui_p=NULL then
        x_mpz_divisible_ui_p = link_c_func(mpir_dll, "+__gmpz_divisible_ui_p", {P,I},I)
    end if
    boolean res = c_func(x_mpz_divisible_ui_p,{n,d})
    return res
end function

integer x_mpz_divisible_2exp_p = NULL

global function mpz_divisible_2exp_p(mpz n, integer b)
    if n=NULL then ?9/0 end if
    if b<0 then ?9/0 end if
    if x_mpz_divisible_2exp_p=NULL then
        x_mpz_divisible_2exp_p = link_c_func(mpir_dll, "+__gmpz_divisible_2exp_p", {P,I},I)
    end if
    boolean res = c_func(x_mpz_divisible_2exp_p,{n,b})
    return res
end function

integer x_mpz_remove = NULL

global function mpz_remove(mpz rop, op, f)
--Remove all occurrences of the factor f from op and store the result in rop.
--The return value is how many such occurrences were removed.
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if f=NULL then ?9/0 end if
    if x_mpz_remove=NULL then
        x_mpz_remove = link_c_func(mpir_dll, "+__gmpz_remove", {P,P,P},I)
    end if
    integer res = c_func(x_mpz_remove,{rop,op,f})
    return res
end function

integer x_mpz_mod = NULL

global procedure mpz_mod(mpz r, n, d)
-- r = mod(n,d) The sign of the divisor is ignored; the result is always non-negative.
    if r=NULL then ?9/0 end if
    if n=NULL then ?9/0 end if
    if d=NULL then ?9/0 end if
    if x_mpz_mod=NULL then
        x_mpz_mod = link_c_proc(mpir_dll, "+__gmpz_mod", {P,P,P})
    end if
    c_proc(x_mpz_mod,{r,n,d})
end procedure

integer x_mpz_fdiv_ui = NULL

global function mpz_fdiv_ui(mpz n, integer d)
-- returns mod(n,d) [n and d remain unaltered]
    if n=NULL then ?9/0 end if
    if d<0 then ?9/0 end if
    if x_mpz_fdiv_ui=NULL then
        x_mpz_fdiv_ui = link_c_func(mpir_dll, "+__gmpz_fdiv_ui", {P,I},I)
    end if
    integer res = c_func(x_mpz_fdiv_ui,{n,d})
    return res
end function

integer x_mpz_mod_ui = NULL

global function mpz_mod_ui(mpz r, n, integer d)
-- r = mod(n,d)
-- mpz_mod_ui is identical to mpz_fdiv_r_ui, returning the remainder as well as setting
-- r. See mpz_fdiv_ui above if only the return value is wanted.
    if r=NULL then ?9/0 end if
    if n=NULL then ?9/0 end if
    if d<0 then ?9/0 end if
    if x_mpz_mod_ui=NULL then
        x_mpz_mod_ui = link_c_func(mpir_dll, "+__gmpz_fdiv_r_ui", {P,P,I},I)
    end if
    integer res = c_func(x_mpz_mod_ui,{r,n,d})
    return res
end function

integer x_mpz_xor = NULL

global procedure mpz_xor(mpz rop, op1, op2)
--rop := xor_bits(op1,op2)
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpz_xor=NULL then
        x_mpz_xor = link_c_proc(mpir_dll, "+__gmpz_xor", {P,P,P})
    end if
    c_proc(x_mpz_xor,{rop,op1,op2})
end procedure

integer x_mpz_cmp = NULL

global function mpz_cmp(mpz op1, op2)
--Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, or a negative
--value if op1 < op2.
    if op1=NULL then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpz_cmp=NULL then
        x_mpz_cmp = link_c_func(mpir_dll, "+__gmpz_cmp", {P,P},I)
    end if
    integer res = c_func(x_mpz_cmp,{op1,op2})
--if not find(res,{-1,0,+1}) then ?9/0 end if   -- triggered (res==3)...
    res = sign(res)     -- (.. so we /do/ actually need this).
    return res
end function

integer x_mpz_cmp_si = NULL

global function mpz_cmp_si(mpz op1, integer op2)
--Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, or a negative
--value if op1 < op2.
    if op1=NULL then ?9/0 end if
    if x_mpz_cmp_si=NULL then
        x_mpz_cmp_si = link_c_func(mpir_dll, "+__gmpz_cmp_si", {P,I},I)
    end if
    integer res = c_func(x_mpz_cmp_si,{op1,op2})
--if not find(res,{-1,0,+1}) then ?9/0 end if   -- triggered...
    res = sign(res) -- (... so we /do/ actually need this)
    return res
end function

global function mpz_min(sequence s, boolean return_index=false)
    mpz res = s[1]  -- (ioob as per docs...)
    integer rdx = 1
    for i=2 to length(s) do
        if mpz_cmp(s[i],res)=-1 then
            res = s[i]
            rdx = i
        end if
    end for
    return iff(return_index?rdx:res)
end function

global function mpz_max(sequence s, boolean return_index=false)
    mpz res = s[1]  -- (ioob as per docs...)
    integer rdx = 1
    for i=2 to length(s) do
        if mpz_cmp(s[i],res)=+1 then
            res = s[i]
            rdx = i
        end if
    end for
    return iff(return_index?rdx:res)
end function

global function mpz_sign(mpz op1)
    return compare(mpz_cmp_si(op1,0),0) -- -1: op1 -ve, 0: op1=0, +1: op1+ve
end function

integer x_mpz_tstbit = NULL

global function mpz_tstbit(mpz op, integer bit_index)
--Test bit bit_index in op and return 0 or 1 accordingly.
    if op=NULL then ?9/0 end if
    if x_mpz_tstbit=NULL then
        x_mpz_tstbit = link_c_func(mpir_dll, "+__gmpz_tstbit", {P,I},I)
    end if
    integer res = c_func(x_mpz_tstbit,{op,bit_index})
    return res
end function

global function mpz_odd(mpz op1)
    return mpz_tstbit(op1,0)
end function

global function mpz_even(mpz op1)
    return not mpz_tstbit(op1,0)
end function

integer x_mpz_scan0 = NULL

global function mpz_scan0(mpz op, integer starting_bit)
--Find first 0 in op >= starting_bit.
--Scan op, starting from bit starting bit, towards more significant bits, until the first 0 or 1 bit
--(respectively) is found. Return the index of the found bit.
--If the bit at starting bit is already what's sought, then starting bit is returned.
--If there's no bit found, then the largest possible mp_bitcnt_t is returned. This will happen
--in mpz_scan0 past the end of a positive number, or mpz_scan1 past the end of a nonnegative
--number.
    if op=NULL then ?9/0 end if
    if x_mpz_scan0=NULL then
        x_mpz_scan0 = link_c_func(mpir_dll, "+__gmpz_scan0", {P,I},I)
    end if
    integer res = c_func(x_mpz_scan0,{op,starting_bit})
    return res
end function

integer x_mpz_scan1 = NULL

global function mpz_scan1(mpz op, integer starting_bit)
--Find first 1 in op >= starting_bit.
    if op=NULL then ?9/0 end if
    if x_mpz_scan1=NULL then
        x_mpz_scan1 = link_c_func(mpir_dll, "+__gmpz_scan1", {P,I},I)
    end if
    integer res = c_func(x_mpz_scan1,{op,starting_bit})
    return res
end function

integer x_mpz_powm = NULL

global procedure mpz_powm(mpz rop, base, exponent, modulus)
--Set rop to mod(base^exponent,modulus)
--A negative exp is supported in mpz_powm if an inverse base-1 mod mod exists (see mpz_
--invert in Section 5.9 [Number Theoretic Functions], page 36). If an inverse doesn't exist
--then a divide by zero is raised.
    if rop=NULL then ?9/0 end if
    if base=NULL then ?9/0 end if
    if exponent=NULL then ?9/0 end if
    if modulus=NULL then ?9/0 end if
    if x_mpz_powm=NULL then
        x_mpz_powm = link_c_proc(mpir_dll, "+__gmpz_powm", {P,P,P,P})
    end if
    c_proc(x_mpz_powm,{rop,base,exponent,modulus})
end procedure

integer x_mpz_powm_ui = NULL

global procedure mpz_powm_ui(mpz rop, base, integer exponent, mpz modulus)
--Set rop to mod(base^exponent,modulus)
--A negative exp is supported in mpz_powm_ui if an inverse base-1 mod mod exists (see mpz_
--invert in Section 5.9 [Number Theoretic Functions], page 36). If an inverse doesn't exist
--then a divide by zero is raised.
    if rop=NULL then ?9/0 end if
    if base=NULL then ?9/0 end if
    if exponent<0 then ?9/0 end if
    if modulus=NULL then ?9/0 end if
    if x_mpz_powm_ui=NULL then
        x_mpz_powm_ui = link_c_proc(mpir_dll, "+__gmpz_powm_ui", {P,P,I,P})
    end if
    c_proc(x_mpz_powm_ui,{rop,base,exponent,modulus})
end procedure

integer x_mpz_pow_ui = NULL

global procedure mpz_pow_ui(mpz rop, base, integer exponent)
--Set rop to base^exp. The case 0^0 yields 1.
    if rop=NULL then ?9/0 end if
    if base=NULL then ?9/0 end if
    if exponent<0 then ?9/0 end if
    if x_mpz_pow_ui=NULL then
        x_mpz_pow_ui = link_c_proc(mpir_dll, "+__gmpz_pow_ui", {P,P,I})
    end if
    c_proc(x_mpz_pow_ui,{rop,base,exponent})
end procedure

integer x_mpz_ui_pow_ui = NULL

global procedure mpz_ui_pow_ui(mpz rop, integer base, exponent)
--Set rop to base^exp. The case 0^0 yields 1.
    if rop=NULL then ?9/0 end if
    if base<0 then ?9/0 end if
    if exponent<0 then ?9/0 end if
    if x_mpz_ui_pow_ui=NULL then
        x_mpz_ui_pow_ui = link_c_proc(mpir_dll, "+__gmpz_ui_pow_ui", {P,I,I})
    end if
    c_proc(x_mpz_ui_pow_ui,{rop,base,exponent})
end procedure

integer x_mpz_root = NULL

global function mpz_root(mpz rop, op, integer n)
--Set rop to the truncated integer part of the nth root of op.
--Return true if the computation was exact, i.e. rop===op^n.
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if n<1 then ?9/0 end if
    if x_mpz_root=NULL then
        x_mpz_root = link_c_func(mpir_dll, "+__gmpz_root", {P,P,I}, I)
    end if
    bool bExact = (c_func(x_mpz_root,{rop,op,n})!=0)
    return bExact
end function

integer x_mpz_nthroot = NULL

global procedure mpz_nthroot(mpz rop, op, integer n)
--Set rop to the truncated integer part of the nth root of op.
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if n<1 then ?9/0 end if
    if x_mpz_nthroot=NULL then
        x_mpz_nthroot = link_c_proc(mpir_dll, "+__gmpz_nthroot", {P,P,I})
    end if
    c_proc(x_mpz_nthroot,{rop,op,n})
end procedure

integer x_mpz_sqrt = NULL

global procedure mpz_sqrt(mpz rop, op)
--Set rop to the truncated integer part of the square root of op.
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpz_sqrt=NULL then
        x_mpz_sqrt = link_c_proc(mpir_dll, "+__gmpz_sqrt", {P,P})
    end if
    c_proc(x_mpz_sqrt,{rop,op})
end procedure

integer x_mpz_sqrtrem = NULL

global procedure mpz_sqrtrem(mpz rop1, rop2, op)
--Set rop1 to the truncated integer part of the square root of op,
--and rop2 to the remainder op-rop1^2, which will be zero if op is
--a perfect square.
--If rop1 and rop2 are the same variable, the results are undefined.
    if rop1=NULL then ?9/0 end if
    if rop2=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpz_sqrtrem=NULL then
        x_mpz_sqrtrem = link_c_proc(mpir_dll, "+__gmpz_sqrtrem", {P,P,P})
    end if
    c_proc(x_mpz_sqrtrem,{rop1,rop2,op})
end procedure

integer x_mpz_fib_ui = NULL

--global procedure mpz_fib_ui(mpz fn, integer n)
global procedure mpz_fib_ui(mpz fn, atom n)
--Set fn to the n'th Fibonacci number.
    if fn=NULL then ?9/0 end if
    if n<0 then ?9/0 end if
    if x_mpz_fib_ui=NULL then
        x_mpz_fib_ui = link_c_proc(mpir_dll, "+__gmpz_fib_ui", {P,I})
    end if
    c_proc(x_mpz_fib_ui,{fn,n})
end procedure

integer x_mpz_fib2_ui = NULL

global procedure mpz_fib2_ui(mpz fn, fnsub1, integer n)
--Set fn to the n'th Fibonacci number, and fnsub1 to the n-1'th.
    if fn=NULL then ?9/0 end if
    if fnsub1=NULL then ?9/0 end if
    if n<0 then ?9/0 end if
    if x_mpz_fib2_ui=NULL then
        x_mpz_fib2_ui = link_c_proc(mpir_dll, "+__gmpz_fib2_ui", {P,P,I})
    end if
    c_proc(x_mpz_fib2_ui,{fn,fnsub1,n})
end procedure

--/*
--integer x_mpz_fits_slong_p = NULL
--
--global function mpz_fits_slong_p(mpz op)
---- Return non-zero iff the value of op fits in a (signed) long, otherwise, return zero.
--  if op=NULL then ?9/0 end if
--  if x_mpz_fits_slong_p=NULL then
--      x_mpz_fits_slong_p = link_c_func(mpir_dll, "+__gmpz_fits_slong_p", {P},I)
--  end if
--  integer res = c_func(x_mpz_fits_slong_p,{op})
--  return res
--end function
--
--integer x_mpz_fits_ulong_p = NULL
--
--global function mpz_fits_ulong_p(mpz op)
---- Return non-zero iff the value of op fits in an unsigned long, otherwise, return zero.
--  if op=NULL then ?9/0 end if
--  if x_mpz_fits_ulong_p=NULL then
--      x_mpz_fits_ulong_p = link_c_func(mpir_dll, "+__gmpz_fits_ulong_p", {P},I)
--  end if
--  integer res = c_func(x_mpz_fits_ulong_p,{op})
--  return res
--end function
--*/

integer x_mpz_gcd = NULL

global procedure mpz_gcd(mpz rop, op1, op2)
--Set rop to the greatest common divisor of op1 and op2. The result is always positive even if
--one or both input operands are negative.
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpz_gcd=NULL then
        x_mpz_gcd = link_c_proc(mpir_dll, "+__gmpz_gcd", {P,P,P})
    end if
    c_proc(x_mpz_gcd,{rop,op1,op2})
end procedure

integer x_mpz_gcd_ui = NULL

global function mpz_gcd_ui(mpz rop, op1, integer op2)
--Compute the greatest common divisor of op1 and op2. If rop is not NULL, store the result there.
--If the result is small enough to fit in an mpir_ui, it is returned. If the result does not fit, 0
--is returned, and the result is equal to the argument op1. Note that the result will always fit
--if op2 is non-zero.
--  if rop=NULL then ?9/0 end if     -- (NB apparently permitted by the C api)
    if op1=NULL then ?9/0 end if
    if op2<0 then ?9/0 end if
    if x_mpz_gcd_ui=NULL then
        x_mpz_gcd_ui = link_c_func(mpir_dll, "+__gmpz_gcd_ui", {P,P,I},I)
    end if
    integer res = c_func(x_mpz_gcd_ui,{rop,op1,op2})
    return res
end function

integer x_mpz_lcm = NULL

global procedure mpz_lcm(mpz rop, op1, op2)
--Set rop to the least common multiple of op1 and op2. rop is always positive, irrespective of
--the signs of op1 and op2. rop will be zero if either op1 or op2 is zero.
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpz_lcm=NULL then
        x_mpz_lcm = link_c_proc(mpir_dll, "+__gmpz_lcm", {P,P,P})
    end if
    c_proc(x_mpz_lcm,{rop,op1,op2})
end procedure

integer x_mpz_lcm_ui = NULL

global procedure mpz_lcm_ui(mpz rop, op1, integer op2)
--Set rop to the least common multiple of op1 and op2. rop is always positive, irrespective of
--the signs of op1 and op2. rop will be zero if either op1 or op2 is zero.
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpz_lcm_ui=NULL then
        x_mpz_lcm_ui = link_c_proc(mpir_dll, "+__gmpz_lcm_ui", {P,P,I})
    end if
    c_proc(x_mpz_lcm_ui,{rop,op1,op2})
end procedure

integer x_mpz_fac_ui = NULL

global procedure mpz_fac_ui(mpz rop, integer n)
--Set rop to the factorial of n.
    if rop=NULL then ?9/0 end if
    if n<0 then ?9/0 end if
    if x_mpz_fac_ui=NULL then
        x_mpz_fac_ui = link_c_proc(mpir_dll, "+__gmpz_fac_ui", {P,I})
    end if
    c_proc(x_mpz_fac_ui,{rop,n})
end procedure

integer x_mpz_get_si = NULL

--global function mpz_get_si(mpz op)
global function mpz_get_integer(mpz op)
--Return the value of op as a phix integer.
--If op is too big to fit in an integer, it will either crash or return a meaningless result.
--To find out if the value will fit, use the function mpz_fits_integer.
    if op=NULL then ?9/0 end if
    if x_mpz_get_si=NULL then
        x_mpz_get_si = link_c_func(mpir_dll, "+__gmpz_get_si", {P},I)
    end if
--DEV I find myself repeatedly umm-ing and ahh-ing over this...
    integer res = c_func(x_mpz_get_si,{op})
--  atom res = c_func(x_mpz_get_si,{op})
    return res
end function

--global function mpz_get_integer(mpz op)
--  return mpz_get_si(op)
--end function

global function mpz_get_atom(mpz op)
--Return the value of op as a phix atom.
--If op is too big to fit in an atom, it will either crash or return +/-inf.
--To find out if the value will fit, use the function mpz_fits_atom.
    atom res = 0
--  bool signed = mpz_sign(op)=-1
    boolean signed = (mpz_cmp_si(op,0)<0)
    mpz temp = mpz_init_set(op)
    if signed then
        mpz_ui_sub(temp,0,temp)
    end if
    -- keep it simple...
    atom p10 = 1
    while mpz_cmp_si(temp,0) do
        res += mpz_fdiv_q_ui(temp,temp,1000000000)*p10
        p10 *= 1000000000
    end while
    if signed then res *= -1 end if
    return res
end function

--/*
--integer x_mpz_get_ui = NULL
--
--global function mpz_get_ui(mpz op)
----Return the value of op as an mpir_ui.
----If op is too big to fit an mpir_ui then just the least significant bits that do fit are returned.
----The sign of op is ignored, only the absolute value is used.
----If op is too big to fit in a mpir_ui, the returned result is probably not very useful. To find
----out if the value will fit, use the function mpz_fits_ulong_p.
--  if op=NULL then ?9/0 end if
--  if x_mpz_get_ui=NULL then
--      x_mpz_get_ui = link_c_func(mpir_dll, "+__gmpz_get_ui", {P},I)
--  end if
--  integer res = c_func(x_mpz_get_ui,{op})
--  return res
--end function
--*/

integer x_mpz_size = NULL

global function mpz_size(mpz op)
--Return the size of op measured in number of limbs. 
-- (DEV this may not be very useful without mp_bits_per_limb; commented out in docs)
--If op is zero, the returned value will be zero.
    if op=NULL then ?9/0 end if
    if x_mpz_size=NULL then
        x_mpz_size = link_c_func(mpir_dll, "+__gmpz_size", {P},I)
    end if
    integer res = c_func(x_mpz_size,{op})
    return res
end function

integer x_mpz_sizeinbase = NULL

--bool loopy = true
global function mpz_sizeinbase(mpz op, integer base)
--
-- Return the size of op measured in number of digits in the given base.
-- base can vary from 2 to 36. 
-- The sign of op is ignored, just the absolute value is used. 
-- The result will be either exact or 1 too big. 
-- If base is a power of 2, the result is always exact. 
-- If op is zero the return value is always 1.
-- This function can be used to determine the space required when converting op to a string. 
-- The right amount of allocation is normally two more than the value returned by mpz_sizeinbase,
--  one extra for a minus sign and one for the null-terminator.
-- It will be noted that mpz_sizeinbase(op,2) can be used to locate the most significant 1 bit
--  in op, counting from 1. (Unlike the bitwise functions which start from 0, See Section 5.11
--
    if op=NULL then ?9/0 end if
    if base<2 or base>62 then ?9/0 end if
    if x_mpz_sizeinbase=NULL then
        x_mpz_sizeinbase = link_c_func(mpir_dll, "+__gmpz_sizeinbase", {P,I},I)
    end if
    integer res = c_func(x_mpz_sizeinbase,{op,base})
    if mpz_cmp_si(op,0)>0 then
--if loopy then
--  loopy = false
        mpz tmp = mpz_init()
        mpz_ui_pow_ui(tmp,base,res-1)
--?{"mpz_sib",mpz_get_str(op),mpz_get_str(tmp),res}
--      if mpz_cmp(op,tmp)<0 then res -= 1 ?"-1" end if
        if mpz_cmp(op,tmp)<0 then res -= 1 end if
--  loopy = true
--end if
    end if
    return res
end function

global function mpz_fits_integer(mpz op)
-- Return true iff the value of op fits in a (signed) integer, otherwise, return false.
-- Note this actually returns false for -#40000000, which technically fits, but true
-- for -#3FFFFFFF..#3FFFFFFF, and false for (+)#40000000 (on 32-bit).
    if op=NULL then ?9/0 end if
--  integer res = c_func(x_mpz_fits_slong_p,{op})
    return mpz_sizeinbase(op,2)<machine_bits()-1
end function

global function mpz_fits_atom(mpz op, boolean tztrim=false)
-- Return true iff the value of op fits in a phix atom, otherwise, return false.
-- Note: this returns false for 9007199254740992 (on 32 bit), since that is
--       the first value that "accidentally" fits, by ending in a binary 0,
--       that is, when tztrim is false.
    integer n = mpz_sizeinbase(op,2),
            lim = iff(machine_bits()=32?53:64)
    if tztrim and n>lim then
        mpz temp = mpz_init_set(op)
        while n>lim and mpz_even(temp) do
            {} = mpz_fdiv_q_ui(temp,temp,2)
            n -= 1 
        end while
        temp = mpz_free(temp)
    end if
    return n<=lim
end function

integer x_mpz_get_str = NULL

global function mpz_get_str(mpz x, integer base=10, boolean comma_fill=false)
--
-- Note this always allocates memory for the first argument to the C function,
-- and the other two arguments are swapped for default value reasons.
--
--      ie     C: mpz_get_str(0, 10, x)
--      ==> phix: string s = mpz_get_str(x[,10])
--
--      (nb the C API does not have the comma_fill argument or anything similar)
--
-- Convert op to a string of digits in base base. 
-- The base may vary from 2 to 36 or from -2 to -36.
-- For base in the range 2..36, digits and lower-case letters are used; 
--                  for -2..-36, digits and upper-case letters are used; 
--                  for 37..62, digits, upper-case letters, and lower-case letters (in
--                                              that significance order) are used.
--
-- Experimentation (test code below) showed that mpz_get_str() crashed with
-- a stack overflow at 97,882 digits on 32 bit and 104,345 digits on 64 bit. 
-- Therefore we print numbers in blocks of <=20,000 digits and glue together.
-- NB: Only tested to 300,000 digits (which is just 15 such blocks).
--     I also tested that block sizes of 1,2,3,..10 work as well.
--
    if x=NULL then ?9/0 end if
    if base<2 or base>62 then ?9/0 end if
    if x_mpz_get_str=NULL then
        if mpir_dll=NULL then open_mpir_dll() end if
        x_mpz_get_str = link_c_func(mpir_dll, "+__gmpz_get_str", {P,I,P}, P)
    end if
    integer l = mpz_sizeinbase(x, base)
    atom pString = allocate(min(l+2,20002))
    sequence chunks = {}
    if l>20000 then
--      atom d = mpz_init("1e20000"), r = mpz_init()
        atom d = mpz_init(), r = mpz_init()
        mpz_ui_pow_ui(d,10,20000)
        x = mpz_init_set(x) -- copy for modification, relies on auto-free.
        while true do
            mpz_fdiv_qr(x,r,x,d) -- {x,r} := {floor(x/d),remainder(x,d)}
            pString = c_func(x_mpz_get_str,{pString,base,r})
            chunks = prepend(chunks,peek_string(pString))
            l -= 20000
            if l<=20000 then exit end if
        end while
    end if      
    pString = c_func(x_mpz_get_str,{pString,base,x})
    string res = peek_string(pString)
    free(pString)   
    for i=1 to length(chunks) do
        string ci = chunks[i]
        l = length(ci)
        if l<20000 then
            res &= repeat('0',20000-l)
        end if
        res &= ci
    end for
    if comma_fill then
--      res = reverse(join_by(reverse(res),1,3,"",","))[2..$]
        res = reverse(join_by(reverse(res),1,3,repeat(' ',0),repeat(',',1)))[2..$]
    end if
    return res
end function

--/*
-- test code mentioned above (in mpz_get_str)
mpz res = mpz_init(9)
integer e_size = 2, a_size
while e_size<300000 do
    mpz_mul_si(res,res,10)
    mpz_add_ui(res,res,9)
    e_size += 1
--  if e_size>iff(machine_bits()=32?97800:104300) or mod(e_size,100)=0 then
    if mod(e_size,1000)=0 then
        a_size = mpz_sizeinbase(res,10) 
        if a_size!=e_size then ?9/0 end if
        string nines = mpz_get_str(res)
        if length(nines)!=e_size-1 then ?9/0 end if
        if nines!=repeat('9',e_size-1) then ?9/0 end if
        ?{e_size,"ok"}
    end if
end while
--*/

--DEV to go*6...

integer x_mpz_probable_prime_p = NULL

function mpz_probable_prime_p_(mpz n, randstate state, integer prob=5, div=0)
--
-- Determine whether n is a probable prime with the chance of error being at most 1 in 2^prob.
-- return value is 1 if n is probably prime, or 0 if n is definitely composite.
-- This function does some trial divisions to speed up the average case, then some probabilistic
-- primality tests to achieve the desired level of error.
-- div can be used to inform the function that trial division up to div has already been performed
-- on n and so n has NO divisors <= div. Use 0 to inform the function that no trial division has
-- been done.
-- This function interface is preliminary and may change in the future.
--
-- The variable state must be initialized by calling one of the gmp_randinit functions
--
    if n=NULL then ?9/0 end if
    if state=NULL then ?9/0 end if
    if prob<=0 then ?9/0 end if
    if div<0 then ?9/0 end if
    if x_mpz_probable_prime_p=NULL then
        x_mpz_probable_prime_p = link_c_func(mpir_dll, "+__gmpz_probable_prime_p", {P,P,I,I}, I)
    end if
    integer res = c_func(x_mpz_probable_prime_p,{n,state,prob,div})
    return res
end function

integer x_gmp_randseed = NULL

procedure gmp_randseed_(randstate state, atom mpz_seed=NULL)
-- Set an initial seed value into state.
    if state=NULL then ?9/0 end if
    if x_gmp_randseed=NULL then
        x_gmp_randseed = link_c_proc(mpir_dll, "+__gmp_randseed", {P,I})
    end if
    if mpz_seed=NULL then
--      string tmp = ""&('0'+rand(9)) -- (first digit!=0)
        string tmp = repeat('0'+rand(9),1) -- (first digit!=0)
        for x=2 to 200 do
            tmp &= '0'+rand(10)-1     -- (then 199* 0..9)
        end for
        mpz_seed = mpz_init(tmp)
        c_proc(x_gmp_randseed,{state,mpz_seed})
        _mpz_clear(mpz_seed)
--      mpz_seed = NULL -- (happens soon enough anyway)
    else
        c_proc(x_gmp_randseed,{state,mpz_seed})
    end if
end procedure

integer x_gmp_randclear = NULL

function gmp_randclear_(randstate state)
-- Free all memory occupied by state.
    if state=NULL then ?9/0 end if
    if x_gmp_randclear=NULL then
        x_gmp_randclear = link_c_proc(mpir_dll, "+__gmp_randclear", {P})
    end if
    c_proc(x_gmp_randclear,{state})
    poke4(state+5*W,NULL)
    return NULL
end function

procedure free_randstate(atom state)
-- (internal, delete_routine)
    if peek4s(state+5*W)=MPZ_R then {} = gmp_randclear_(state) end if
    free(state)
end procedure
constant r_free_randstate = routine_id("free_randstate")

integer x_gmp_randinit_mt = NULL

function gmp_randinit_mt_()
-- Initialize state for a Mersenne Twister algorithm.
    if x_gmp_randinit_mt=NULL then
        if mpir_dll=NULL then open_mpir_dll() end if
        x_gmp_randinit_mt = link_c_proc(mpir_dll, "+__gmp_randinit_mt", {P})
    end if
    atom state = allocate(W*6)  -- (extra dword for MPZ_R)
    poke4(state+5*W,MPZ_R)
    state = delete_routine(state,r_free_randstate)
    c_proc(x_gmp_randinit_mt,{state})
    gmp_randseed_(state)
    return state
end function

atom state = NULL

integer x_mpz_urandomm = NULL

// deprecated/made private 7/6/21 (due to randstate not being available/possible in p2js.js)
--global 
--procedure mpz_urandomm_(mpz rop, randstate state, mpz n)
procedure mpz_urandomm_(mpz rop, mpz n)
--
--  Generate a uniform random integer in the range 0 to n - 1, inclusive.
--  The variable state must be initialized by calling one of the gmp_randinit functions
--  (Section 9.1 [Random State Initialization], page 66) before invoking this function.
--
    if rop=NULL then ?9/0 end if
    if state=NULL then ?9/0 end if
    if n=NULL then ?9/0 end if
    if x_mpz_urandomm=NULL then
        x_mpz_urandomm = link_c_proc(mpir_dll, "+__gmpz_urandomm", {P,P,P})
    end if
    c_proc(x_mpz_urandomm,{rop,state,n})
end procedure

include builtins\primes.e -- (an autoinclude, but why not)

global procedure mpz_rand(mpz n, range)
    if state=NULL then state = gmp_randinit_mt_() end if
--  if pf_state=NULL then
--      enter_cs()
--      if pf_state=NULL then
--          pf_state = gmp_randinit_mt_()
--          pfs_cs = init_cs()
--      end if
--      leave_cs()
--  end if
--  enter_cs(pfs_cs)
--  mpz_urandomm(n,pf_state,range)
    mpz_urandomm_(n,range)
--  leave_cs(pfs_cs)
end procedure

global procedure mpz_rand_ui(mpz n, integer range)
    mpz_set_si(n,range)
    mpz_rand(n, n)
end procedure

global function mpz_prime(mpz p, integer prob=5)
    if p!=NULL then
        if state=NULL then state = gmp_randinit_mt_() end if
        return mpz_probable_prime_p_(p,state, prob)
    end if
    if state!=NULL then state = gmp_randclear_(state) end if
    return NULL
end function

-- this is transpiled (then manually copied) to mpz_prime() in mpfr.js:
mpz modp47 = NULL, w
sequence witness_ranges
global function mpz_prime_mr(mpz p, integer k = 10)
    -- deterministic to 3,317,044,064,679,887,385,961,981
    constant primes = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47}
    if mpz_cmp_si(p,primes[$])<=0 then
        return find(mpz_get_integer(p),primes)!=0
    end if
    if modp47=NULL then
        modp47 = mpz_init("614_889_782_588_491_410") -- === product(primes), largest < 2^64
        w = mpz_init()
        -- Best known deterministic witnesses for given range and set of bases
        -- https://miller-rabin.appspot.com/
        -- https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
        witness_ranges = {{"341_531",{"9345883071009581737"}},
                          {"1_050_535_501",{"336781006125",
                                            "9639812373923155"}},
                          {"350_269_456_337",{"4230279247111683200",
                                              "14694767155120705706",
                                              "16641139526367750375"}},
                          {"55_245_642_489_451",{"2", "141889084524735",
                                                 "1199124725622454117",
                                                 "11096072698276303650"}},
                          {"7_999_252_175_582_851",{"2", "4130806001517",
                                                    "149795463772692060",
                                                    "186635894390467037", 
                                                    "3967304179347715805"}},
                          {"585_226_005_592_931_977",{"2", "123635709730000",
                                                      "9233062284813009",
                                                      "43835965440333360",
                                                      "761179012939631437",
                                                      "1263739024124850375"}},
                          {"18_446_744_073_709_551_615",{"2", "325", "9375",
                                                         "28178", "450775",
                                                         "9780504", "1795265022"}},
                          {"318_665_857_834_031_151_167_461",{"2", "3", "5", "7", "11",
                                                              "13", "17", "19", "23", 
                                                              "29", "31", "37"}},
                          {"3_317_044_064_679_887_385_961_981",{"2", "3", "5", "7", "11", 
                                                                "13", "17", "19", "23", 
                                                                "29", "31", "37", "41"}}}
        for i=1 to length(witness_ranges) do
            witness_ranges[i][1] = mpz_init(witness_ranges[i][1])
            for j=1 to length(witness_ranges[i][2]) do
                witness_ranges[i][2][j] = mpz_init(witness_ranges[i][2][j])
            end for
        end for
    end if
    mpz_gcd(w,p,modp47)
    if mpz_cmp_si(w,1)!=0 then
        return false    -- eliminates 86.2% of all integers
    end if
    --
    -- Choose input witness bases:
    --
    sequence witnesses
    if mpz_cmp(p,witness_ranges[$][1])>=0 then
        witnesses = repeat(0,k)
        for i=1 to k do
            mpz a = mpz_init()
            mpz_sub_ui(a, p, 2)
            mpz_rand(a,a)       -- a := 0..a-1 (cf rand(n) yields 1..n)
            mpz_add_ui(a, a, 2)
            witnesses[i] = a
        end for
    else
        for i=1 to length(witness_ranges) do
            if mpz_cmp(p,witness_ranges[i][1])<0 then
                witnesses = witness_ranges[i][2]
                exit
            end if
        end for
    end if
    mpz d = mpz_init()
    mpz_sub_ui(d,p,1)
    mpz nm1 = mpz_init_set(d)
--    d >>= 4 while (d & 0xf) == 0                  # suck out factors of 2
--    (d >>= (d & 3)^2; d >>= (d & 1)^1) if d.even? # 4 bits at a time
    while mpz_even(d) do
        mpz_fdiv_q_2exp(d, d, 1)
    end while
    
    for i=1 to length(witnesses) do
        mpz b = witnesses[i]
        if not mpz_divisible_p(b,p) then -- skip multiples of input
            mpz s = mpz_init_set(d),
                y = mpz_init()
            mpz_powm(y, b, d, p)        -- y := b^d % p
            while mpz_cmp_si(y,1)!=0 
              and mpz_cmp(y,nm1)!=0 
              and mpz_cmp(s,nm1)!=0 do
                mpz_powm_ui(y, y, 2, p)     -- y := y^2 mod p
                mpz_mul_2exp(s, s, 1)       -- s << 1
            end while
            if mpz_cmp(y,nm1)!=0 then
                if mpz_even(s) then return false end if
            end if
        end if
      end for
      return true
end function

--randstate pf_state=NULL
--integer pfs_cs = 0

global function mpz_prime_factors(mpz_or_string s, integer maxprime=100)
--
-- Attempts to decompse the integer s into powers of small primes.
-- returns eg 108 ==> {{2,2},{3,3}}  (ie 2^2*3^3==4*27==108)
--         or 10080 ==> {{2,5},{3,2},{5,1},{7,1}}
--         or 1196836 ==> {{2,2},{"299209"}}
--  Each element is a {prime,power} pair, except last may be a lone string.
--  See mpz_factorstring() for more clarification and handling of that.
--
-- The default 100th prime is 541, so at that setting this is exact/complete 
--  for all inputs <= 541^2 == "292681", and you can easily raise (or lower) 
--  that limit, within reason.
-- However, factors of even a 500-digit number is properly hard, so hard that 
--  in fact almost all internet security is based on it being a *really* hard 
--  problem. Hence this is designed to "give up" early/in a sensible fashion, 
--  eg: mpz_prime_factors(sprintf("%d",power(2*get_prime(101),2)),100) yields
--  {{2,2},{"299209"}}. Note that length(res[$]) is 1, which means it greater
--  than either get_prime(maxprime)^2 or power(2,machine_word()), hence it 
--  returns it as a (lone) final string. Also, while all other elements of
--  res are almost certainly phix integer, res[$][1] may be atom (1GB..4GB).
--  Increasing maxprime to 101 above would obviously yield {{2,2},{547,2}},
--  ie from mpz_prime_factors(sprintf("%d",power(2*get_prime(101),2)),101).
--  [[Technically on 32-bit (no such "hole" on 64-bit) it could yield atoms
--    in the range power(2,32..53) as "exact". However a) there are no mpz
--    routines matching mpz_fits_ulong_p/mpz_get_ui, not that a few divides
--    and multiplications should trouble us, along with some calls to say
--    mpz_sizeinbase() to determine when to attempt, but more importantly 
--    b) there does not seem to be very much point in bothering.]]
-- Once again, see below for one suggested way to cope with all of that.
--  
    sequence res = {}
    mpz n = iff(string(s)?mpz_init(s):mpz_init_set(s)),
        f = mpz_init()
    integer c = mpz_cmp_si(n,1), p
    if c<=0 then
        -- (special cases, equivalent to prime_factors(0)==>{}
        --                           and prime_factors(1)==>{1})
        res = iff(c<0?{}:{{2,0}})
    else    
--      if pf_state=NULL then pf_state = gmp_randinit_mt_() end if      
--      if pf_state=NULL then
--          enter_cs()
--          if pf_state=NULL then
--              pf_state = gmp_randinit_mt_()
--              pfs_cs = init_cs()
--          end if
--          leave_cs()
--      end if
--      enter_cs(pfs_cs)
--      boolean bPrime = mpz_probable_prime_p_(n,pf_state,20)
--      leave_cs(pfs_cs)
        boolean bPrime = mpz_prime(n,20)
        if not bPrime then
            for d=1 to maxprime do
--              enter_cs(pfs_cs)
                p = get_prime(d)
--              leave_cs(pfs_cs)
                if mpz_divisible_ui_p(n, p) then
                    mpz_set_si(f,p)
                    integer e = mpz_remove(n, n, f)
                    res = append(res,{p,e})
                    if mpz_cmp_si(n,1)=0 then exit end if
--                  enter_cs(pfs_cs)
--                  bPrime = mpz_probable_prime_p_(n,pf_state,20)
--                  leave_cs(pfs_cs)
                    bPrime = mpz_prime(n,20)
                    if bPrime then exit end if
                end if
            end for
        end if
        if mpz_cmp_si(n,1)!=0 then
--          boolean fits = mpz_fits_integer(n)
            boolean fits = mpz_fits_atom(n)
            if fits and not bPrime then
--              mpz_ui_pow_ui(f, get_prime(maxprime), 2)
--              enter_cs(pfs_cs)
                p = get_prime(maxprime)
--              leave_cs(pfs_cs)
                mpz_ui_pow_ui(f, p, 2)
                if mpz_cmp(n,f)>0 then
                    fits = false
                end if
            end if
            if fits then
--              res = append(res,{mpz_get_ui(n),1})
--              res = append(res,{mpz_get_si(n),1})
--              res = append(res,{mpz_get_integer(n),1})
                res = append(res,{mpz_get_atom(n),1})
            else
                res = append(res,{mpz_get_str(n)})
            end if
        end if
    end if
    {n,f} = mpz_free({n,f})
    return res
end function

--/*
This also looks doable (from http://rosettacode.org/wiki/Prime_decomposition#Perl_6 )
Pure Perl 6
This is a pure perl 6 version that uses no outside libraries. 
It uses a variant of Pollard's rho factoring algorithm and is fairly performent when factoring numbers < 2^80; typically taking well under a second on an i7. 
It starts to slow down with larger numbers, but really bogs down factoring numbers that have more than 1 factor larger than about 2^40.

sub prime-factors ( Int $n where * > 0 ) {
    return $n if $n.is-prime;
    return () if $n == 1;
    my $factor = find-factor( $n );
    sort flat ( $factor, $n div $factor ).map: *.&prime-factors;
}
 
sub find-factor ( Int $n, $constant = 1 ) {
    return 2 unless $n +& 1;
    if (my $gcd = $n gcd 6541380665835015) > 1 { # magic number: [*] primes 3 .. 43
        return $gcd if $gcd != $n
    }
    my $x      = 2;
    my $rho    = 1;
    my $factor = 1;
    while $factor == 1 {
        $rho = $rho +< 1;
        my $fixed = $x;
        my int $i = 0;
        while $i < $rho {
            $x = ( $x * $x + $constant ) % $n;
            $factor = ( $x - $fixed ) gcd $n;
            last if 1 < $factor;
            $i = $i + 1;
        }
    }
    $factor = find-factor( $n, $constant + 1 ) if $n == $factor;
    $factor;
}
 
.put for (2^29-1, 2^14-1, 2^59-1, 2^71-1, 2^79-1, 2^97-1, 2^117-1, 2^241-1,
5465610891074107968111136514192945634873647594456118359804135903459867604844945580205745718497)\
.hyper(:1batch).map: -> $n {
    my $start = now;
   "factors of $n: ",
    prime-factors($n).join(' * '), " \t in ", (now - $start).fmt("%0.3f"), " sec."
}
Output:
factors of 536870911:  233 * 1103 * 2089         in  0.004  sec.
factors of 2199023255551:  13367 * 164511353     in  0.011  sec.
factors of 576460752303423487:  179951 * 3203431780337       in  0.023  sec.
factors of 2361183241434822606847:  228479 * 48544121 * 212885833    in  0.190  sec.
factors of 604462909807314587353087:  2687 * 202029703 * 1113491139767       in  0.294  sec.
factors of 158456325028528675187087900671:  11447 * 13842607235828485645766393       in  0.005  sec.
factors of 166153499473114484112975882535043071:  7 * 73 * 79 * 937 * 6553 * 8191 * 86113 * 121369 * 7830118297          in  0.022  sec.
factors of 3533694129556768659166595001485837031654967793751237916243212402585239551:  22000409 * 160619474372352289412737508720216839225805656328990879953332340439     in  0.085  sec.
factors of 5465610891074107968111136514192945634873647594456118359804135903459867604844945580205745718497:  
            165901 * 10424087 * 18830281 * 53204737 * 56402249 * 59663291 * 91931221 * 95174413 * 305293727939 * 444161842339 * 790130065009             in  28.427  sec.
--*/

global function mpz_factorstring(sequence s)
-- converts eg {{2,2},{3,3}} to "2^2*3^3"
-- s is typically from mpz_prime_factors(), but does not have to be.
-- s[$] may be {string} (ie unfactored/able).
    if s={} then return "0" end if      -- (rather than ""/crash)
    if s={{2,0}} then return "1" end if -- (rather than "2^0")
    string res = ""
--  boolean inexact = length(s[$])=1
    for i=1 to length(s) do
        if length(res) then res &= "*" end if
        object si = s[i]
        if string(si) then
            res &= si
        elsif length(si)=1 then
            res &= si[1]
        else
            {atom p, integer e} = s[i]
            res &= sprintf("%d",p)
            if e!=1 then
                res &= sprintf("^%d",{e})
            end if
        end if
    end for
--  if inexact then
--      if length(res) then res &= "*" end if
--      res &= s[$][1]
--  end if
    return res
end function

global procedure mpz_re_compose(mpz rop, sequence s)
-- takes eg {{2,2},{3,3}} and sets rop to 108,
-- where 108 is 2^2*3^3 ie 4*27.
    if length(s)=0 then
        mpz_set_si(rop,1)
    else
        mpz pn = mpz_init()
        boolean inexact = length(s[$])=1
        if inexact then
            mpz_set_str(rop,s[$][1])
        else
            mpz_set_si(rop,1)
        end if
        for i=1 to length(s)-inexact do
            {atom p, integer e} = s[i]
            mpz_ui_pow_ui(pn,p,e)
            mpz_mul(rop,rop,pn)
        end for
        pn = mpz_free(pn)
    end if
end procedure

function compare_strings(string p, s)
-- private routine for merge(): return true to pick p over s
--  (could be nested but I'm not quite confident enough yet)
    return length(p)<length(s)
       or (length(p)=length(s) and p<s)
end function

function merge(sequence p, s, bool bAsStrings)
--
-- private routine for mpz_pollard_rho()
-- s has {prime,pow} or {string} elements, as per mpz_prime_factors().
-- p the same unless bAsStrings is true, iwc it should be string-only.
-- result is slightly flattened with {prime,pow} or string elements
--  (note I did not say {string} elements), in correct numeric order.
--    (both p and s are expected to be in the right order on entry)
--  (any strings we get are expected to have failed mpz_fits_integer)
-- eg merge({"123"},{{2,2},{"321"}},true) ==> {"2","2","123","321"}
--    merge({"123"},{{2,2},{"321"}},false) ==> {{2,2},"123","321"}
-- Note we are coping with {"321"} in "s" because mpz_prime_factors()
--  spits that out, but as it stands a plain "321" shd be ok too.
--
    sequence res = {}
    atom prime
    integer pow
    bool which
    if bAsStrings then
        -- p should already be string-only (as a prior result of this routine)
        for i=1 to length(s) do
            sequence si = s[i]
            if length(si)=1 then
                res = append(res,si[1])
            else
                {prime,pow} = si
                si = sprintf("%d",prime)
                for j=1 to pow do
                    res = append(res,si)
                end for
            end if
        end for
        if length(p) then
            s = res
            res = {}
            while length(p) and length(s) do
                string ps = p[1],
                       ss = s[1]
                which = compare_strings(ps,ss)
                if which then
                    res = append(res,ps)
                    p = p[2..$]
                else
                    res = append(res,ss)
                    s = s[2..$]
                end if
            end while
            res &= p
            res &= s
        end if
    else
        object p1,s1
        while length(p) and length(s) do
            p1 = p[1]
            s1 = s[1]
            if length(s1)=1 then s1=s1[1] end if
            if string(p1) then
                if string(s1) then
                    which = compare_strings(p1,s1)
                else    
                    {prime,pow} = s1
                    which = compare_strings(p1,sprintf("%d",prime))
                end if
            elsif string(s1) then
                {prime,pow} = p1
                which = compare_strings(sprintf("%d",prime),s1)
            else
                {prime,pow} = s1
                if p1[1] = prime then
                    p1 = deep_copy(p1)
                    p1[2] += pow    -- (merge)
                    s = s[2..$]
                    which = true
                else
                    which = p1[1]<prime 
                end if
            end if
            if which then
                res = append(res,p1)
                p = p[2..$]
            else
                res = append(res,s1)
                s = s[2..$]
            end if
        end while
        res &= p
--      res &= s
        for i=1 to length(s) do
            s1 = s[i]
            if length(s1)=1 then s1=s1[1] end if
            res = append(res,s1)
        end for
    end if
    return res
end function

--/*
-- unit tests for merge:
if merge({},{{2,1}},true)!={"2"} then ?9/0 end if
if merge({},{{2,1}},false)!={{2,1}} then ?9/0 end if
if merge({},{{2,2}},true)!={"2","2"} then ?9/0 end if
if merge({},{{2,2}},false)!={{2,2}} then ?9/0 end if
if merge({},{{2,1},{11,1}},true)!={"2","11"} then ?9/0 end if
if merge({},{{2,2},{11,1}},true)!={"2","2","11"} then ?9/0 end if
if merge({},{{2,1},{11,1}},false)!={{2,1},{11,1}} then ?9/0 end if
if merge({},{{2,2},{11,1}},false)!={{2,2},{11,1}} then ?9/0 end if
if merge({"11"},{{911,1}},true)!={"11","911"} then ?9/0 end if
if merge({"11"},{{911,1}},false)!={"11",{911,1}} then ?9/0 end if
if merge({{11,1}},{{911,1}},false)!={{11,1},{911,1}} then ?9/0 end if
if merge({"911"},{{11,1}},true)!={"11","911"} then ?9/0 end if
if merge({"911"},{{11,1}},false)!={{11,1},"911"} then ?9/0 end if
if merge({{911,1}},{{11,1}},false)!={{11,1},{911,1}} then ?9/0 end if
if merge({"3","3"},{{3,1}},true)!={"3","3","3"} then ?9/0 end if
if merge({{3,2}},{{3,1}},false)!={{3,3}} then ?9/0 end if
if merge({{3,2}},{{2,1}},false)!={{2,1},{3,2}} then ?9/0 end if
if merge({"123"},{{2,2},{"321"}},true)!={"2","2","123","321"} then ?9/0 end if
if merge({"123"},{{2,2},{"321"}},false)!= {{2,2},"123","321"} then ?9/0 end if
-- the following should never actually happen in practice (it is ok to kill it)
if merge({"123"},{{2,2},"321"},false)!= {{2,2},"123","321"} then ?9/0 end if
-- (obviously feel free to add more)
--*/

global function mpz_pollard_rho(mpz_or_string s, bool bAsStrings=false)
--
-- Note that unlike mpz_prime_factors() the result is a list of strings, eg 
--  mpz_pollard_rho("151740406071813") ==> {"3","13","13","54833","5458223"}
-- Update: that now only be true if bAsStrings is true, by default you will
--  now get matching integer {prime,pow} entries when bAsStrings is false,
--  at least for bits that pass mpz_fits_atom().
--
    mpz n = iff(string(s)?mpz_init(s):mpz_init_set(s))
    sequence res = {}
    while mpz_cmp_si(n,100_000_000)>0 
      and not mpz_prime(n) do
        mpz x = mpz_init(2),
            y = mpz_init(2),
            f = mpz_init(1)  -- factor
        integer size = 2
        while mpz_cmp_si(f,1)=0 do
            for count=1 to size do
                mpz_mul(x,x,x)
                mpz_add_si(x,x,1)
                mpz_mod(x,x,n)
                mpz_sub(f,x,y)
                mpz_abs(f,f)
                mpz_gcd(f,f,n)
                if mpz_cmp_si(f,1)!=0 then exit end if
            end for
            size *= 2;
            mpz_set(y,x)
        end while
        if mpz_cmp(f,n)=0 then exit end if
        res = merge(res,mpz_prime_factors(f,10000),bAsStrings)
--?res
        mpz_fdiv_q(n,n,f) -- n := floor(n/f)
    end while
    if mpz_cmp_si(n,1)>0 then
        res = merge(res,mpz_prime_factors(n,10000),bAsStrings)
    end if
--?{"res",res}
    return res
end function

--/* other version...
procedure g(mpz x, n)
    mpz_mul(x,x,x)
    mpz_add_si(x,x,1)
    mpz_mod(x,x,n)
end procedure

function pollard_rho0(mpz n)
    if mpz_prime(n) then return NULL end if
--?{"mpz_pollard_rho",mpz_get_str(n)}
--atom t0 = time()
--  var g = Fn.new { |x, y| (x*x + BigInt.one) % n }
    mpz x = mpz_init(2),
        y = mpz_init(2),
        z = mpz_init(1),
        d = mpz_init(1)
    integer count = 0
--sequence seenz = {}
    while true do
--      x = g.call(x, n)
        g(x,n)
--      y = g.call(g.call(y, n), n)
        g(y,n)
        g(y,n)
--      d = (x - y).abs % n
        mpz_sub(d,x,y)
        mpz_abs(d,d)
        mpz_mod(d,d,n)
--      z = z * d
        mpz_mul(z,z,d)
        count += 1
        if count=100 then
--          d = BigInt.gcd(z, n)
--?{"mpz_gcd",mpz_get_str(d),mpz_get_str(z),mpz_get_str(n)}
            mpz_gcd(d,z,n)
--?{"mpz_gcd",mpz_get_str(d),mpz_get_str(z),mpz_get_str(n)}
--?{"d",mpz_get_str(d)}
--          if (d != BigInt.one) break
            if mpz_cmp_si(d,1)!=0 then exit end if
--string zs = mpz_get_str(z)
--if find(zs,seenz) then ?9/0 end if
--seenz = append(seenz,zs)
--          z = BigInt.one
            mpz_set_si(z,1)
            count = 0
        end if
    end while
--?{"pollard_rho done",elapsed(time()-t0)}
    if mpz_cmp(d,n)=0 then return NULL end if
--  if mpz_cmp(d,n)=0 then 
--      ?{"pollard_rho done","NULL"}
--      return NULL
--  end if
    return d
end function
--*/

--DEV erm, I think this should be mpz_bin_uiui()...
--global function mpz_binom(integer n, k)
global procedure mpz_bin_uiui(mpz rop, integer n, k)
-- equivalent, for small n and k, to builtins/factorial.e's choose()
--  mpz r = mpz_init(1)
    mpz_set_si(rop,1)
    for i=1 to k do
--      r := (r*(n-i+1))/i
        mpz_mul_si(rop,rop,n-i+1)
        if mpz_fdiv_q_ui(rop,rop,i)!=0 then ?9/0 end if
    end for
end procedure

--SUG:
--/*
--function mpz_binom(integer n, k)
procedure mpz_binom(mpz r, integer n, k)
--  mpz r = mpz_init(1)
    mpz_set_si(r,1)
    for i=1 to k do
        mpz_mul_si(r,r,n-i+1)
        if mpz_fdiv_q_ui(r, r, i)!=0 then ?9/0 end if
    end for
--  return mpz_get_str(r)
--end function
end procedure
 
mpz r = mpz_init()
--?mpz_binom(10,4) 
mpz_binom(r,10,4) 
?mpz_get_str(r)
--?k_perm(10,4)
?choose(10,4)

function mpz_vecprod(sequence s, object zlr=1)
--
-- Fast vector multiplication.
-- Multiplying the vector elements in pairs is much faster for essentially 
--  much the same reason that merge sort is faster than insertion sort.
--  Improved rosettacode/Primorial_numbers from 6 minutes to 6 seconds!!!!
-- NB: Input sequence s (must all be mpz) is damaged. Returns an mpz.
--     Obviously zlr allows you to specify the result when {} is passed,
--     since I imagine there'll be cases where you'd rather get a zero,
--     and just like mpz_init(), zlr can be an integer, atom, or string.
--
    if s={} then
        return mpz_init(zlr)
    end if
    while length(s)>1 do
        for i=1 to floor(length(s)/2) do
            mpz_mul(s[i],s[i],s[-i])
        end for
        s = s[1..ceil(length(s)/2)]
    end while
    return s[1]
end function

function mpz_vecprod_si(sequence s, object zlr=1)
--
-- As above except input sequence s must all be integer. Unlike above, 
--  s is not damaged, and this entry point is expected to be used more 
--  often than the above, for such simple practical reasons, that is.
-- Tests suggest halving mpz_init() calls makes this ~20% faster, more
--  likely down to all the implicit free_mpz() than anything else.
--  (Then again, 20% of not much is, erm, not very much...)
--
    if and_bits(length(s),1) then
        s &= 1
    end if
    integer j = 0
    for i=1 to length(s) by 2 do
        mpz two = mpz_init(s[i])
        mpz_mul_si(two,two,s[i+1])
        j += 1
        s[j] = two
    end for
    s = s[1..j] 
    return mpz_vecprod(s,zlr)
end function
--*/

-- Technical note: MPFR_PREC_MAX is really 2^31-257 (or 2^63-257 on 64 bit), but
--                 mpfr.e (delib/implicitly) limits to integer, ie 2^30-1 (2^62-1).
--                 Besides, running out of memory when trying to allocate a 2nd 
--                 variable is not very likely to be useful to anyone anyway...
--                 The default precision of 53/64 matches IEEE-754, which makes
--                 using mpfr.e instead of atom fairly pointless, except perhaps
--                 to make sure it gets (almost?) exactly the same results.

integer x_mpfr_get_default_prec = NULL,
        x_mpfr_set_default_prec = NULL,
--      default_precision = iff(W=4?53:64)
--dev FIXME? (see list.asm/ildump.txt)
        default_precision = iff(machine_bits()=32?53:64)

function precision_in_decimal(integer precision)
-- (internal) convert a binary precision to decimal places (see notes in docs)
    if precision<=0 then ?9/0 end if
    mpz twos = mpz_init()
    mpz_set_str(twos,repeat('2',precision),base:=2)
    precision = mpz_sizeinbase(twos,10)
    twos = mpz_free(twos)
    return precision
end function

global function mpfr_get_default_precision(boolean decimal=false)
    if x_mpfr_get_default_prec=NULL then
        if mpfr_dll=NULL then open_mpir_dll() end if
        x_mpfr_get_default_prec = link_c_func(mpfr_dll, "+mpfr_get_default_prec", {}, I)
    end if
    integer precision = c_func(x_mpfr_get_default_prec,{})
    if default_precision!=precision then ?9/0 end if
    if decimal then precision = precision_in_decimal(precision) end if
    return precision
end function

function precision_in_dp(integer precision)
-- (internal) convert a (-ve) precision specified in decimal places to binary bits
    if precision>=0 then ?9/0 end if
    mpz nines = mpz_init(repeat('9',-precision))
    precision = mpz_sizeinbase(nines,2) + 2 -- (+2 as documented in phix.chm/mpfr_set_default_precision)
--  precision = mpz_sizeinbase(nines,2)
    nines = mpz_free(nines)
    return precision
end function

global procedure mpfr_set_default_precision(integer precision)
    if x_mpfr_set_default_prec=NULL then
        if mpfr_dll=NULL then open_mpir_dll() end if
        x_mpfr_set_default_prec = link_c_proc(mpfr_dll, "+mpfr_set_default_prec", {I})
    end if
    if precision<1 then precision = precision_in_dp(precision) end if
    c_proc(x_mpfr_set_default_prec,{precision})
    default_precision = precision
end procedure

global constant MPFR_RNDN = 0,  -- round to nearest
                MPFR_RNDZ = 1,  -- round toward zero
                MPFR_RNDU = 2,  -- round toward +infinity
                MPFR_RNDD = 3,  -- round toward -infinity
                MPFR_RNDA = 4,  -- round away from zero
--              MPFR_RNDF = 5?  -- faithful rounding. (experimental?)
                GMP_RNDN = MPFR_RNDN,
                GMP_RNDZ = MPFR_RNDZ,
                GMP_RNDU = MPFR_RNDU,
                GMP_RNDD = MPFR_RNDD,
                GMP_RNDA = MPFR_RNDA

integer x_mpfr_get_default_rounding_mode = NULL,    -- (aka __gmp_default_rounding_mode)
        x_mpfr_set_default_rounding_mode = NULL,
        default_rounding = MPFR_RNDN

global function mpfr_get_default_rounding_mode()
    if x_mpfr_get_default_rounding_mode=NULL then
        if mpfr_dll=NULL then open_mpir_dll() end if
        x_mpfr_get_default_rounding_mode = link_c_func(mpfr_dll, "+mpfr_get_default_rounding_mode", {}, I)
    end if
    integer rounding = c_func(x_mpfr_get_default_rounding_mode,{})
    if default_rounding!=rounding then ?9/0 end if
    return rounding
end function

global procedure mpfr_set_default_rounding_mode(integer rounding)
-- Set the default rounding mode. The initial default rounding mode is to nearest (MPFR_RNDN).
    if rounding<MPFR_RNDN or rounding>MPFR_RNDA then ?9/0 end if
    if x_mpfr_set_default_rounding_mode=NULL then
        if mpfr_dll=NULL then open_mpir_dll() end if
        x_mpfr_set_default_rounding_mode = link_c_proc(mpfr_dll, "+mpfr_set_default_rounding_mode", {I})
    end if
    c_proc(x_mpfr_set_default_rounding_mode,{rounding})
    default_rounding = rounding
end procedure

integer x_mpfr_set = NULL

global procedure mpfr_set(mpfr tgt, src, integer rounding=default_rounding)
    if tgt=NULL then ?9/0 end if
    if src=NULL then ?9/0 end if
    if x_mpfr_set=NULL then
        x_mpfr_set = link_c_proc(mpfr_dll, "+mpfr_set", {P,P,I})
    end if
    c_proc(x_mpfr_set,{tgt,src,rounding})
end procedure

integer x_mpfr_set_si = NULL

global procedure mpfr_set_si(mpfr tgt, integer i, integer rounding=default_rounding)
-- set tgt from a phix integer
    if tgt=NULL then ?9/0 end if
    if x_mpfr_set_si=NULL then
        x_mpfr_set_si = link_c_proc(mpfr_dll, "+mpfr_set_si", {P,I,I})
    end if
    c_proc(x_mpfr_set_si,{tgt,i,rounding})
end procedure

integer x_mpfr_set_d = NULL

global procedure mpfr_set_d(mpfr tgt, atom a, integer rounding=default_rounding)
-- set tgt from a phix atom
    if tgt=NULL then ?9/0 end if
    if x_mpfr_set_d=NULL then
        x_mpfr_set_d = link_c_proc(mpfr_dll, "+mpfr_set_d", {P,D,I})
    end if
    c_proc(x_mpfr_set_d,{tgt,a,rounding})
end procedure

integer x_mpfr_set_str = NULL

global procedure mpfr_set_str(mpfr tgt, string s, integer base=0, rounding=default_rounding)
-- set tgt from a string
    if tgt=NULL then ?9/0 end if
    if base<0 or base=1 or base>62 then ?9/0 end if
    if x_mpfr_set_str=NULL then
        x_mpfr_set_str = link_c_func(mpfr_dll, "+mpfr_set_str", {P,P,I,I},I)
    end if
    if c_func(x_mpfr_set_str,{tgt,s,base,rounding})!=0 then ?9/0 end if
end procedure
if "abc"="def" then mpfr_set_str(NULL,"") end if --DEV/temp


--Function: int mpfr_set_q (mpfr_t rop, mpq_t op, mpfr_rnd_t rnd)
--mpfr_set_q might fail if the numerator (or the denominator) can not be represented as a mpfr_t.
integer x_mpfr_set_q = NULL

global procedure mpfr_set_q(mpfr tgt, mpq q, integer rounding=default_rounding)
-- set the mpfr rop from an mpq
    if tgt=NULL then ?9/0 end if
    if q=NULL then ?9/0 end if
    if x_mpfr_set_q=NULL then
--      x_mpfr_set_q = link_c_func(mpfr_dll, "+mpfr_set_q", {P,P,I},I)
        x_mpfr_set_q = link_c_proc(mpfr_dll, "+mpfr_set_q", {P,P,I})
    end if
    -- not quite sure exactly what the result is, seems to be -1/0/+1, 
    -- I would hazard it is some kind of comparison/rounding thing...
--  if c_func(x_mpfr_set_q,{tgt,q,rounding})!=0 then ?9/0 end if
    c_proc(x_mpfr_set_q,{tgt,q,rounding})
end procedure


--__MPFR_DECLSPEC int mpfr_set_z (mpfr_ptr, mpz_srcptr, mpfr_rnd_t);
----__MPFR_DECLSPEC int mpfr_set_z (mpfr_ptr, mpz_srcptr, mpfr_rnd_t);
--Function: int mpfr_set_z (mpfr_t rop, mpz_t op, mpfr_rnd_t rnd)
integer x_mpfr_set_z = NULL

global procedure mpfr_set_z(mpfr tgt, mpz z, integer rounding=default_rounding)
-- set the mpfr rop from an mpz
    if tgt=NULL then ?9/0 end if
    if z=NULL then ?9/0 end if
    if x_mpfr_set_z=NULL then
--      x_mpfr_set_z = link_c_func(mpfr_dll, "+mpfr_set_z", {P,P,I},I)
        x_mpfr_set_z = link_c_proc(mpfr_dll, "+mpfr_set_z", {P,P,I})
    end if
--  if c_func(x_mpfr_set_z,{rop,op,rounding})!=0 then ?9/0 end if
    c_proc(x_mpfr_set_z,{tgt,z,rounding})
end procedure

integer x_mpfr_clear = NULL

procedure _mpfr_clear(mpfr x)
--
-- (internal / call mpfr_free() instead)
-- Frees any mpfr-side memory associated with x, but not the phix-side
-- as was allocated within mpfr_init(). Better: mpfr_free() does both.
--
    if x=NULL then ?9/0 end if
    if x_mpfr_clear=NULL then
        x_mpfr_clear = link_c_proc(mpfr_dll, "+mpfr_clear", {P})
    end if
    c_proc(x_mpfr_clear,{x})
    poke4(x+4*W,NULL)
end procedure

procedure free_mpfr(atom x)
-- (internal, delete_routine)
    if peek4s(x+W*4)=MPFR_T then _mpfr_clear(x) end if
    free(x)
end procedure
constant r_free_mpfr = routine_id("free_mpfr")

integer x_mpfr_init2 = NULL

global function mpfr_init(object v=0, integer precision=default_precision, rounding=default_rounding)
--
-- example: mpfr x = mpfr_init()
-- Invoke x = mpfr_free(x) when the variable is no longer needed, see below.
--
    atom res = allocate(5*W)    -- (extra dword for MPFR_T)
    res = delete_routine(res,r_free_mpfr)
    poke4(res+4*W,MPFR_T)
    if x_mpfr_init2=NULL then
        if mpfr_dll=NULL then open_mpir_dll() end if
        x_mpfr_init2 = link_c_proc(mpfr_dll, "+mpfr_init2", {P,I})
    end if
    if precision<1 then precision = precision_in_dp(precision) end if
    c_proc(x_mpfr_init2,{res,precision})
--  if mpfr(v) then                             -- NO!!
--      mpfr_set(res,v,rounding)
    if integer(v) then
        -- (aside: note, as per docs, the default here /is/ 0 rather than the nan of the raw C api)
        mpfr_set_si(res,v,rounding)
    elsif atom(v) then
        mpfr_set_d(res,v,rounding)
    elsif string(v) then
        mpfr_set_str(res,v,rounding)
    else
        ?9/0 -- what's v??
    end if
    return res  
end function

global function mpfr_inits(integer n, object v=0, precision=default_precision, rounding=default_rounding)
--
-- Eg: mpfr {x,y,z} = mpfr_init(3)
--
--  Initialise n variables to v. Obviously the result my be stored in a sequence.
--
-- Invoke {x,y,z} = mpfr_free({x,y,z}) when the variables are no longer needed, see below (will occur automatically).
--
    sequence res = repeat(0,n)
    if sequence(v) and not string(v) then
        if length(v)!=n then ?9/0 end if
        for i=1 to n do res[i] = mpfr_init(v[i],precision,rounding) end for
    else
        for i=1 to n do res[i] = mpfr_init(v,precision,rounding) end for
    end if
    return res
end function

integer x_mpfr_get_prec = NULL

global function mpfr_get_precision(mpfr x, boolean decimal=false)
    if x=NULL then ?9/0 end if
    if x_mpfr_get_prec=NULL then
        x_mpfr_get_prec = link_c_func(mpfr_dll, "+mpfr_get_prec", {P}, I)
    end if
    integer precision = c_func(x_mpfr_get_prec,{x})
    if decimal then precision = precision_in_decimal(precision) end if
    return precision
end function

global function mpfr_init_set(mpfr src, integer rounding=default_rounding)
    if src=NULL then ?9/0 end if
    integer precision = mpfr_get_precision(src)
--  atom res = mpfr_init(src,precision,rounding)    -- NO!!
    atom res = mpfr_init(0,precision,rounding)
    mpfr_set(res,src)
    return res  
end function

global function mpfr_init_set_q(mpq q, integer rounding=default_rounding)
    mpfr res = mpfr_init()
    mpfr_set_q(res,q,rounding)
    return res
end function

global function mpfr_init_set_z(mpz z, integer rounding=default_rounding)
    mpfr res = mpfr_init()
    mpfr_set_z(res,z,rounding)
    return res
end function

global function mpfr_free(object x)
--
-- Clear and deallocate any variables created using mpfr_init()
--
-- usage: x = mpfr_free(x)
--    or: {y,z} = mpfr_free({y,z})
--
--  (perhaps one day I'll implement var-id so that you won't 
--   have to specify everything twice in cases like this...)
--
    if sequence(x) then
        for i=1 to length(x) do
            atom xi = x[i]  -- typecheck/prohibit deep nesting
            _mpfr_clear(xi)
            x[i] = NULL     -- see aside
        end for
        --
        -- aside: The above x[i]=NULL does not immediately trigger 
        --        the cleanup requested (ie 2nd parameter of 1 to 
        --        allocate) in mpfr_new(); the (lhs) assignment in 
        --        the calling code does. Just so that you know.
        --
        --        Notice that if we didn't set x[i] like that, the
        --        release might be further deferred by refcounts
        --        remaining in x/the hidden temp variable, that is
        --        until the latter was also released/reused.
        --
        return x
    end if
    _mpfr_clear(x)
    return NULL
end function

integer x_mpfr_set_prec = NULL

global procedure mpfr_set_precision(mpfr x, integer precision)
--
-- Reset the precision of x to be exactly prec bits, and set its value to NaN. 
-- The previous value stored in x is lost. It is equivalent to a call to mpfr_clear(x) 
--  followed by a call to mpfr_init2(x, prec), but more efficient as no allocation is 
--  done in case the current allocated space for the significand of x is enough. 
-- The precision prec can be any integer between MPFR_PREC_MIN and MPFR_PREC_MAX. 
-- To keep the previous value stored in x, use mpfr_prec_round instead.
--
-- precision is the number of bits required for the mantissa
--
    if x=NULL then ?9/0 end if
    if x_mpfr_set_prec=NULL then
        x_mpfr_set_prec = link_c_proc(mpfr_dll, "+mpfr_set_prec", {P,I})
    end if
    if precision<1 then precision = precision_in_dp(precision) end if
    c_proc(x_mpfr_set_prec,{x,precision})
end procedure

integer x_mpfr_const_pi = NULL

global procedure mpfr_const_pi(mpfr x, integer rounding=default_rounding)
    if x=NULL then ?9/0 end if
    if x_mpfr_const_pi=NULL then
        if mpfr_dll=NULL then open_mpir_dll() end if
        x_mpfr_const_pi = link_c_proc(mpfr_dll, "+mpfr_const_pi", {P,I})
    end if
    c_proc(x_mpfr_const_pi,{x,rounding})
end procedure

integer x_mpfr_free_str = NULL

procedure _mpfr_free_str(atom pString)
-- (deliberately internal-only)
    if x_mpfr_free_str=NULL then
        x_mpfr_free_str = link_c_proc(mpfr_dll, "+mpfr_free_str", {P})
    end if
    c_proc(x_mpfr_free_str,{pString})
end procedure

integer x_mpfr_get_str = NULL

global function mpfr_get_str(mpfr x, integer base=10, n=0, rounding=default_rounding)
--
-- Note the arguments are in a quite different order to the C function.
-- Here, we let mpfr allocate space (and free it automatically), hence that
--  first arg of the C function gets entirely hidden from the outside world.
-- The second arg (exponent dword pointer) is likewise allocated/freed here.
-- The x argument is moved from 5th to 1st place, for default value reasons.
--
--      ie     C: mpfr_get_str(<string_ptr>, <expptr>, base, n, x, rounding)
--      ==> phix: mpfr_get_str(x, base, n, rounding)
--
-- Returns a string with an implied radix point and an integer exponent, eg
--  when x is -3.1416, mpfr_get_str(x) returns {"-31416",1}
--
-- The mpfr_sprintf() function is generally probably better/easier.
--
    if x=NULL then ?9/0 end if
    if base<2 or base>62 then ?9/0 end if
    if x_mpfr_get_str=NULL then
        if mpfr_dll=NULL then open_mpir_dll() end if
        x_mpfr_get_str = link_c_func(mpfr_dll, "+mpfr_get_str", {P,P,I,I,P,I}, P)
    end if
    bool bztrim = n<=0  -- (also added 15/6/21)
    if bztrim then n = -n end if
    atom pExponent = allocate(W)
    atom pString = c_func(x_mpfr_get_str,{NULL,pExponent,base,n,x,rounding})
    string res = peek_string(pString)
    integer exponent = peek4s(pExponent)
    free(pExponent)
    _mpfr_free_str(pString)
--15/6/21:
--  return {res,exponent}
    if exponent>0 then
        exponent += (res[1]='-')
        integer l = length(res)
        if exponent>=l then
            res &= repeat('0',exponent-l) -- (may add 0 '0's)
        else
            string rest = res[exponent+1..$]
            res = res[1..exponent]
            while bztrim and length(rest) and rest[$]='0' do
                rest = rest[1..$-1]
            end while
            if length(rest) then
                res &= "." & rest
            end if
        end if
    else
        string sgn = ""
        if res[1]='-' then
            sgn = "-"
            res = res[2..$]
        end if
        while bztrim and length(res) and res[$]='0' do
            res = res[1..$-1]
        end while
        if length(res) then
            res = sgn & "0." & repeat('0',-exponent) & res -- (~ditto)
        else
            res = "0"
        end if
    end if
    return res
end function

integer x_mpfr_asprintf = NULL

global function mpfr_sprintf(string fmt, atom x)
--
-- NB: only supports a single 'R' or 'Z' conversion specifier.
--  The R can be followed by a rounding mode:
--      U - round toward plus infinity
--      D - round toward minus infinity
--      Y - round away from zero
--      Z - round toward zero
--      N - round to nearest (with ties to even)
--  and must then be followed by one of abefg or AEFG.
--  The 'Z' must be followed by one of dxX or possibly aceEfigGo.
--  I trust it is obvious this uses the C sprintf, not the phix one.
--
--  Example: string res = mpfr_asprintf(x,"%.1000Rf") -- print to 1000 d.p.
--
--  Under the hood this actually uses mpfr_asprintf (ie allocate the output buffer).
--
    if x=NULL or not (mpfr(x) or mpz(x)) then ?9/0 end if
    if x_mpfr_asprintf=NULL then
        if mpfr_dll=NULL then open_mpir_dll() end if
        x_mpfr_asprintf = link_c_func(mpfr_dll, "+mpfr_asprintf", {P,P,P}, I)
    end if
    atom pString = allocate(W)
    integer len = c_func(x_mpfr_asprintf,{pString,fmt,x})
    atom rawString = peekNS(pString,W,false)
    string res = peek_string(rawString)
    _mpfr_free_str(rawString)
    free(pString)
    return res
end function

global procedure mpfr_printf(integer fn, string fmt, atom x)
    puts(fn,mpfr_sprintf(fmt,x))
end procedure

global function mpfr_get_fixed(mpfr x, integer dp=6)
--global function mpfr_get_fixed(mpfr x, integer base=10, dp=6, rounding=default_rounding)
--  string fmt = iff(dp<=0?"%.0Rf":sprintf("%%.%dRf",dp))
    string fmt = sprintf("%%.%dRf",max(0,dp))
    return mpfr_sprintf(fmt,x)
end function

integer x_mpfr_floor = NULL

global procedure mpfr_floor(mpfr rop, op)
--rop := floor(op)
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpfr_floor=NULL then
        x_mpfr_floor = link_c_proc(mpfr_dll, "+mpfr_floor", {P,P})
    end if
    c_proc(x_mpfr_floor,{rop,op})
end procedure

integer x_mpfr_add = NULL

global procedure mpfr_add(mpfr rop, op1, op2, integer rounding=default_rounding)
--rop := op1+op2
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpfr_add=NULL then
        x_mpfr_add = link_c_proc(mpfr_dll, "+mpfr_add", {P,P,P,I})
    end if
    c_proc(x_mpfr_add,{rop,op1,op2,rounding})
end procedure

integer x_mpfr_add_si = NULL

global procedure mpfr_add_si(mpfr rop, op1, integer op2, integer rounding=default_rounding)
--rop := op1+op2
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if x_mpfr_add_si=NULL then
        x_mpfr_add_si = link_c_proc(mpfr_dll, "+mpfr_add_si", {P,P,I,I})
    end if
    c_proc(x_mpfr_add_si,{rop,op1,op2,rounding})
end procedure

integer x_mpfr_sub = NULL

global procedure mpfr_sub(mpfr rop, op1, op2, integer rounding=default_rounding)
--rop := op1-op2
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpfr_sub=NULL then
        x_mpfr_sub = link_c_proc(mpfr_dll, "+mpfr_sub", {P,P,P,I})
    end if
    c_proc(x_mpfr_sub,{rop,op1,op2,rounding})
end procedure

integer x_mpfr_sub_si = NULL

global procedure mpfr_sub_si(mpfr rop, op1, integer op2, integer rounding=default_rounding)
--rop := op1-op2
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if x_mpfr_sub_si=NULL then
        x_mpfr_sub_si = link_c_proc(mpfr_dll, "+mpfr_sub_si", {P,P,I,I})
    end if
    c_proc(x_mpfr_sub_si,{rop,op1,op2,rounding})
end procedure

integer x_mpfr_si_sub = NULL

global procedure mpfr_si_sub(mpfr rop, integer op1, mpfr op2, integer rounding=default_rounding)
--rop := op1-op2
    if rop=NULL then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpfr_si_sub=NULL then
        x_mpfr_si_sub = link_c_proc(mpfr_dll, "+mpfr_si_sub", {P,I,P,I})
    end if
    c_proc(x_mpfr_si_sub,{rop,op1,op2,rounding})
end procedure

integer x_mpfr_mul = NULL

global procedure mpfr_mul(mpfr rop, op1, op2, integer rounding=default_rounding)
--rop := op1*op2
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpfr_mul=NULL then
        x_mpfr_mul = link_c_proc(mpfr_dll, "+mpfr_mul", {P,P,P,I})
    end if
    c_proc(x_mpfr_mul,{rop,op1,op2,rounding})
end procedure

integer x_mpfr_mul_si = NULL

global procedure mpfr_mul_si(mpfr rop, op1, integer op2, integer rounding=default_rounding)
--rop := op1*op2
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if x_mpfr_mul_si=NULL then
        x_mpfr_mul_si = link_c_proc(mpfr_dll, "+mpfr_mul_si", {P,P,I,I})
    end if
    c_proc(x_mpfr_mul_si,{rop,op1,op2,rounding})
end procedure

integer x_mpfr_div = NULL

global procedure mpfr_div(mpfr rop, op1, op2, integer rounding=default_rounding)
-- rop := op1/op2
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpfr_div=NULL then
        x_mpfr_div = link_c_proc(mpfr_dll, "+mpfr_div", {P,P,P,I})
    end if
    c_proc(x_mpfr_div,{rop,op1,op2,rounding})
end procedure

integer x_mpfr_div_si = NULL

global procedure mpfr_div_si(mpfr rop, op1, integer op2, rounding=default_rounding)
-- rop := op1/op2
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if x_mpfr_div_si=NULL then
        x_mpfr_div_si = link_c_proc(mpfr_dll, "+mpfr_div_si", {P,P,I,I})
    end if
    c_proc(x_mpfr_div_si,{rop,op1,op2,rounding})
end procedure

integer x_mpfr_div_z = NULL

global procedure mpfr_div_z(mpfr rop, op1, mpz op2, integer rounding=default_rounding)
-- rop := op1/op2
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpfr_div_z=NULL then
        x_mpfr_div_z = link_c_proc(mpfr_dll, "+mpfr_div_z", {P,P,P,I})
    end if
    c_proc(x_mpfr_div_z,{rop,op1,op2,rounding})
end procedure


integer x_mpfr_si_div = NULL

global procedure mpfr_si_div(mpfr rop, integer op1, mpfr op2, integer rounding=default_rounding)
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if x_mpfr_si_div=NULL then
        x_mpfr_si_div = link_c_proc(mpfr_dll, "+mpfr_si_div", {P,P,I,I})
    end if
    c_proc(x_mpfr_si_div,{rop,op1,op2,rounding})
end procedure

integer x_mpfr_rootn_ui = NULL

global procedure mpfr_rootn_ui(mpfr rop, op, integer k, rounding=default_rounding)
--Set rop to the kth root of op rounded in the direction rnd. For k = 0, set rop to NaN (or crash). 
--For k odd (resp. even) and op negative (including -Inf), set rop to a negative number (resp. NaN). 
--If op is zero, set rop to zero with the sign obtained by the usual limit rules, i.e., the same sign as op if k is odd, and positive if k is even.
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpfr_rootn_ui=NULL then
        x_mpfr_rootn_ui = link_c_proc(mpfr_dll, "+mpfr_rootn_ui", {P,P,I,I})
    end if
    c_proc(x_mpfr_rootn_ui,{rop,op,k,rounding})
end procedure

integer x_mpfr_sqr = NULL

global procedure mpfr_sqr(mpfr rop, op, integer rounding=default_rounding)
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpfr_sqr=NULL then
        x_mpfr_sqr = link_c_proc(mpfr_dll, "+mpfr_sqr", {P,P,I})
    end if
    c_proc(x_mpfr_sqr,{rop,op,rounding})
end procedure

--__MPFR_DECLSPEC int mpfr_sqrt (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
--__MPFR_DECLSPEC int mpfr_sqrt_ui (mpfr_ptr, unsigned long, mpfr_rnd_t);
--Function: int mpfr_sqrt (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
--Function: int mpfr_sqrt_ui (mpfr_t rop, unsigned long int op, mpfr_rnd_t rnd)
--Set rop to the square root of op rounded in the direction rnd. Set rop to -0 if op is -0, to be consistent with the IEEE 754 standard. Set rop to NaN if op is negative.

integer x_mpfr_sqrt = NULL

global procedure mpfr_sqrt(mpfr rop, op, integer rounding=default_rounding)
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpfr_sqrt=NULL then
        x_mpfr_sqrt = link_c_proc(mpfr_dll, "+mpfr_sqrt", {P,P,I})
    end if
    c_proc(x_mpfr_sqrt,{rop,op,rounding})
end procedure

integer x_mpfr_sqrt_ui = NULL

global procedure mpfr_sqrt_ui(mpfr rop, integer op, rounding=default_rounding)
    if rop=NULL then ?9/0 end if
    if op<0 then ?9/0 end if
    if x_mpfr_sqrt_ui=NULL then
        x_mpfr_sqrt_ui = link_c_proc(mpfr_dll, "+mpfr_sqrt_ui", {P,P,I})
    end if
    c_proc(x_mpfr_sqrt_ui,{rop,op,rounding})
end procedure

--Function: int mpfr_pow (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_rnd_t rnd)
integer x_mpfr_pow = NULL

global procedure mpfr_pow(mpfr rop, op1, op2, integer rounding=default_rounding)
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpfr_pow=NULL then
        x_mpfr_pow = link_c_proc(mpfr_dll, "+mpfr_pow", {P,P,P,I})
    end if
    c_proc(x_mpfr_pow,{rop,op1,op2,rounding})
end procedure

--XFunction: int mpfr_pow_ui (mpfr_t rop, mpfr_t op1, unsigned long int op2, mpfr_rnd_t rnd)
--Function: int mpfr_pow_si (mpfr_t rop, mpfr_t op1, long int op2, mpfr_rnd_t rnd)
integer x_mpfr_pow_si = NULL

global procedure mpfr_pow_si(mpfr rop, op1, integer op2, rounding=default_rounding)
    if rop=NULL then ?9/0 end if
    if op1=NULL then ?9/0 end if
    if x_mpfr_pow_si=NULL then
        x_mpfr_pow_si = link_c_proc(mpfr_dll, "+mpfr_pow_si", {P,P,I,I})
    end if
    c_proc(x_mpfr_pow_si,{rop,op1,op2,rounding})
end procedure

--Function: int mpfr_ui_pow_ui (mpfr_t rop, unsigned long int op1, unsigned long int op2, mpfr_rnd_t rnd)
--Return Value
--Type: Int32
--Return zero, a positive, or a negative value if rop is respectively equal to, greater than, or lower than the exact result. See GNU MPFR - Rounding Modes for details.
integer x_mpfr_ui_pow_ui = NULL

global procedure mpfr_ui_pow_ui(mpfr rop, integer op1, op2, rounding=default_rounding)
    if rop=NULL then ?9/0 end if
    if op1<0 then ?9/0 end if
    if op2<0 then ?9/0 end if
    if x_mpfr_ui_pow_ui=NULL then
        x_mpfr_ui_pow_ui = link_c_proc(mpfr_dll, "+mpfr_ui_pow_ui", {P,I,I,I})
    end if
    c_proc(x_mpfr_ui_pow_ui,{rop,op1,op2,rounding})
end procedure

--Function: int mpfr_ui_pow (mpfr rop, unsigned long int op1, mpfr_t op2, mpfr_rnd_t rnd)
integer x_mpfr_ui_pow = NULL

global procedure mpfr_ui_pow(mpfr rop, integer op1, mpfr op2, integer rounding=default_rounding)
    if rop=NULL then ?9/0 end if
    if op1<0 then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpfr_ui_pow=NULL then
        x_mpfr_ui_pow = link_c_proc(mpfr_dll, "+mpfr_ui_pow", {P,I,P,I})
    end if
    c_proc(x_mpfr_ui_pow,{rop,op1,op2,rounding})
end procedure

--Set rop to op1 raised to op2, rounded in the direction rnd. Special values are handled as described in the ISO C99 and IEEE 754-2008 standards for the pow function:
--pow(+/-0, y) returns plus or minus infinity for y a negative odd integer.
--pow(+/-0, y) returns plus infinity for y negative and not an odd integer.
--pow(+/-0, y) returns plus or minus zero for y a positive odd integer.
--pow(+/-0, y) returns plus zero for y positive and not an odd integer.
--pow(-1, +/-Inf) returns 1.
--pow(+1, y) returns 1 for any y, even a NaN.
--pow(x, +/-0) returns 1 for any x, even a NaN.
--pow(x, y) returns NaN for finite negative x and finite non-integer y.
--pow(x, -Inf) returns plus infinity for 0 < abs(x) < 1, and plus zero for abs(x) > 1.
--pow(x, +Inf) returns plus zero for 0 < abs(x) < 1, and plus infinity for abs(x) > 1.
--pow(-Inf, y) returns minus zero for y a negative odd integer.
--pow(-Inf, y) returns plus zero for y negative and not an odd integer.
--pow(-Inf, y) returns minus infinity for y a positive odd integer.
--pow(-Inf, y) returns plus infinity for y positive and not an odd integer.
--pow(+Inf, y) returns plus zero for y negative, and plus infinity for y positive.
--Note: When 0 is of integer type, it is regarded as +0 by these functions. We do not use the usual limit rules in this case, as these rules are not used for pow.

integer x_mpfr_neg = NULL

global procedure mpfr_neg(mpfr rop, op, integer rounding=default_rounding)
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpfr_neg=NULL then
        x_mpfr_neg = link_c_proc(mpfr_dll, "+mpfr_neg", {P,P,I})
    end if
    c_proc(x_mpfr_neg,{rop,op,rounding})
end procedure

integer x_mpfr_sin = NULL

global procedure mpfr_sin(mpfr rop, op, integer rounding=default_rounding)
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpfr_sin=NULL then
        x_mpfr_sin = link_c_proc(mpfr_dll, "+mpfr_sin", {P,P,I})
    end if
    c_proc(x_mpfr_sin,{rop,op,rounding})
end procedure

integer x_mpfr_log = NULL

global procedure mpfr_log(mpfr rop, op, integer rounding=default_rounding)
--Set rop to the natural logarithm of op, rounded in the direction rnd. 
--Set rop to +0 if op is 1 (in all rounding modes), for consistency with the ISO C99 and IEEE 754-2008 standards. 
--Set rop to -Inf if op is +/-0 (i.e., the sign of the zero has no influence on the result).
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpfr_log=NULL then
        x_mpfr_log = link_c_proc(mpfr_dll, "+mpfr_log", {P,P,I})
    end if
    c_proc(x_mpfr_log,{rop,op,rounding})
end procedure

integer x_mpfr_exp = NULL

global procedure mpfr_exp(mpfr rop, op, integer rounding=default_rounding)
--Set rop to the exponential of op, rounded in the direction rnd.
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpfr_exp=NULL then
        x_mpfr_exp = link_c_proc(mpfr_dll, "+mpfr_exp", {P,P,I})
    end if
    c_proc(x_mpfr_exp,{rop,op,rounding})
end procedure

integer x_mpfr_gamma = NULL

global procedure mpfr_gamma(mpfr rop, op, integer rounding=default_rounding)
-- rop:= Gamma(op), rounded in the direction rnd.
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpfr_gamma=NULL then
        x_mpfr_gamma = link_c_proc(mpfr_dll, "+mpfr_gamma", {P,P,I})
    end if
    c_proc(x_mpfr_gamma,{rop,op,rounding})
end procedure

integer x_mpfr_get_si = NULL

--DEV mpfr_get/fits_integer/atom??
global function mpfr_get_si(mpfr op, integer rounding=default_rounding)
    if op=NULL then ?9/0 end if
    if x_mpfr_get_si=NULL then
        x_mpfr_get_si = link_c_func(mpfr_dll, "+mpfr_get_si", {P,I},I)
    end if
    atom res = c_func(x_mpfr_get_si,{op,rounding})
    return res
end function

integer x_mpfr_get_d = NULL

global function mpfr_get_d(mpfr op, integer rounding=default_rounding)
    if op=NULL then ?9/0 end if
    if x_mpfr_get_d=NULL then
        x_mpfr_get_d = link_c_func(mpfr_dll, "+mpfr_get_d", {P,I},D)
    end if
    atom res = c_func(x_mpfr_get_d,{op,rounding})
    return res
end function

--integer x_mpfr_sgn = NULL
--
--global function mpfr_sgn(mpfr op)
--  if op=NULL then ?9/0 end if
--  if x_mpfr_sgn=NULL then
--      x_mpfr_sgn = link_c_func(mpfr_dll, "+mpfr_sgn", {P},I)
--  end if
--  integer res = c_func(x_mpfr_sgn,{op})
--if not find(res,{-1,0,+1}) then ?9/0 end if
----    res = sign(res)
--  return res
--end function

integer x_mpfr_cmp = NULL

global function mpfr_cmp(mpfr op1, op2)
    if op1=NULL then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpfr_cmp=NULL then
        x_mpfr_cmp = link_c_func(mpfr_dll, "+mpfr_cmp", {P,P},I)
    end if
    integer res = c_func(x_mpfr_cmp,{op1,op2})
if not find(res,{-1,0,+1}) then ?9/0 end if
--  res = sign(res)
    return res
end function

integer x_mpfr_cmp_si = NULL

global function mpfr_cmp_si(mpfr op1, integer op2)
    if op1=NULL then ?9/0 end if
    if x_mpfr_cmp_si=NULL then
        x_mpfr_cmp_si = link_c_func(mpfr_dll, "+mpfr_cmp_si", {P,I},I)
    end if
    integer res = c_func(x_mpfr_cmp_si,{op1,op2})
if not find(res,{-1,0,+1}) then ?9/0 end if
--  res = sign(res)
    return res
end function

--/*
mpfr_cmp
__MPFR_DECLSPEC int mpfr_cmp_ui (mpfr_srcptr, unsigned long);
__MPFR_DECLSPEC int mpfr_cmp_si (mpfr_srcptr, long);
#define mpfr_cmp_ui(b,i) mpfr_cmp_ui_2exp((b),(i),0)
#define mpfr_cmp_si(b,i) mpfr_cmp_si_2exp((b),(i),0)
#define mpfr_cmp(b, c)   mpfr_cmp3(b, c, 1)
#undef mpfr_cmp_si
#define mpfr_cmp_si(_f,_s)                                      \
  (__builtin_constant_p (_s) && (mpfr_long) (_s) >= 0 ?         \
   mpfr_cmp_ui ((_f), (mpfr_ulong) (mpfr_long) (_s)) :          \
   mpfr_cmp_si_2exp ((_f), (_s), 0))
Function: int mpfr_cmp (mpfr_t op1, mpfr_t op2)
Function: int mpfr_cmp_si (mpfr_t op1, long int op2)
Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, and a negative value if op1 < op2. 
Both op1 and op2 are considered to their full own precision, which may differ. 
If one of the operands is NaN, set the erange flag and return zero.

Note: These functions may be useful to distinguish the three possible cases. 
If you need to distinguish two cases only, it is recommended to use the predicate functions (e.g., mpfr_equal_p for the equality) described below; 
they behave like the IEEE 754 comparisons, in particular when one or both arguments are NaN. 
But only floating-point numbers can be compared (you may need to do a conversion first).
mpfr_cmp:               Comparison Functions
mpfr_cmp_si:            Comparison Functions
--*/

integer x_mpq_clear = NULL

procedure _mpq_clear(mpq x)
--
-- (internal)
-- Frees any mpz-side memory associated with x, but not the phix-side
-- as was allocated within mpq_init(); mpq_free() below does both.
--
    if x=NULL then ?9/0 end if
    if x_mpq_clear=NULL then
        x_mpq_clear = link_c_proc(mpir_dll, "+__gmpq_clear", {P})
    end if
    c_proc(x_mpq_clear,{x})
    poke4(x+6*W,0)
end procedure

procedure free_mpq(atom x)
-- (internal, delete_routine)
--?{"free_mpq",x}
    if peek4s(x+6*W)=MPZ_Q then _mpq_clear(x) end if
    free(x)
end procedure
constant r_free_mpq = routine_id("free_mpq")

global function mpq_free(object x)
--?{"mpq_free",x}
    if sequence(x) then
        for i=1 to length(x) do
--          free_mpq(x[i])
            x[i] = NULL
        end for
    else
--if x=NULL then ?9/0 end if
--      free_mpq(x)
        x = NULL
    end if
    return x
end function


integer x_mpq_set = NULL

global procedure mpq_set(mpq tgt, src)
-- tgt := src
    if tgt=NULL then ?9/0 end if
    if src=NULL then ?9/0 end if
    if x_mpq_set=NULL then
        x_mpq_set = link_c_proc(mpir_dll, "+__gmpq_set", {P,P})
    end if
    c_proc(x_mpq_set,{tgt,src})
end procedure

integer x_mpq_canonicalize = NULL

global procedure mpq_canonicalize(mpq op)
-- Remove any factors that are common to the numerator and denominator of op, 
-- and make the denominator positive.
    if op=NULL then ?9/0 end if
    if x_mpq_canonicalize=NULL then
        x_mpq_canonicalize = link_c_proc(mpir_dll, "+__gmpq_canonicalize", {P})
    end if
    c_proc(x_mpq_canonicalize,{op})
end procedure

integer x_mpq_set_si = NULL

global procedure mpq_set_si(mpq tgt, integer n, d=1)
-- tgt := n/d
    if tgt=NULL then ?9/0 end if
    if d<=0 then ?9/0 end if
    if x_mpq_set_si=NULL then
        x_mpq_set_si = link_c_proc(mpir_dll, "+__gmpq_set_si", {P,I,I})
--      x_mpq_set_si = link_c_proc(mpir_dll, "+__gmpq_set_ui", {P,I,I})
    end if
    c_proc(x_mpq_set_si,{tgt,n,d})
    if d!=1 then
        mpq_canonicalize(tgt)
    end if
end procedure

integer x_mpq_set_str = NULL

global procedure mpq_set_str(mpq tgt, string s, integer base=0)
-- tgt := s
    if tgt=NULL then ?9/0 end if
    if base<0 or base=1 or base>62 then ?9/0 end if
    if x_mpq_set_str=NULL then
        x_mpq_set_str = link_c_proc(mpir_dll, "+__gmpq_set_str", {P,P,I})
    end if
    c_proc(x_mpq_set_str,{tgt,s,base})
    mpq_canonicalize(tgt)
end procedure

integer x_mpq_init = NULL

global function mpq_init(object v=0)
    object res = allocate(7*W) -- (extra dword for MPZ_Q)
    if x_mpq_init=NULL then
        if mpir_dll=NULL then open_mpir_dll() end if
        x_mpq_init = link_c_proc(mpir_dll, "+__gmpq_init", {P})
    end if
    c_proc(x_mpq_init,{res})
    if v!=0 then
--      if mpq(v) then              -- NO!!
--          mpq_set(res, v)
--      elsif mpz(v) then           -- NO!!
--          mpq_set_z(res, v)
        if integer(v) then
            mpq_set_si(res, v)
        elsif string(v) then
            mpq_set_str(res, v)
        else
            ?9/0    -- what's v then?? (non-integer atom is invalid)
        end if
    end if
    res = delete_routine(res,r_free_mpq)
    poke4(res+6*W,MPZ_Q)
    return res  
end function

integer x_mpq_div = NULL

global procedure mpq_div(mpq rquotient, dividend, divisor)
-- set rquotient to dividend / divisor.
    if rquotient=NULL then ?9/0 end if
    if dividend=NULL then ?9/0 end if
    if divisor=NULL then ?9/0 end if
    if x_mpq_div=NULL then
        x_mpq_div = link_c_proc(mpir_dll, "+__gmpq_div", {P,P,P})
    end if
    c_proc(x_mpq_div,{rquotient,dividend,divisor})
end procedure

integer x_mpq_set_z = NULL

global procedure mpq_set_z(mpq tgt, mpz n, d=NULL)
-- tgt := op/1
    if tgt=NULL then ?9/0 end if
    if n=NULL then ?9/0 end if
    if x_mpq_set_z=NULL then
        x_mpq_set_z = link_c_proc(mpir_dll, "+__gmpq_set_z", {P,P})
    end if
    c_proc(x_mpq_set_z,{tgt,n})
    if d!=NULL then
        mpq dq = mpq_init()
        mpq_set_z(dq,d)
        mpq_div(tgt,tgt,dq)
        dq = mpq_free(dq) -- (not really rqd)
        mpq_canonicalize(tgt)
    end if
end procedure

global function mpq_init_set_z(mpz n, d=null)
    mpq res = mpq_init()
    mpq_set_z(res,n,d)
    return res
end function

global function mpq_init_set_si(integer n, d=1)
    mpq res = mpq_init()
    mpq_set_si(res, n, d)
    return res
end function

global function mpq_init_set_str(string s, integer base=0)
    mpq res = mpq_init()
    mpq_set_str(res,s,base)
    return res
end function

global function mpq_inits(integer count, object v=0)
    object res = repeat(0,count)
    if sequence(v) and not string(v) then
        if length(v)!=count then ?9/0 end if
        for i=1 to count do res[i] = mpq_init(v[i]) end for
    else
        for i=1 to count do res[i] = mpq_init(v) end for
    end if
    return res  
end function

global function mpq_init_set(mpq q)
    mpq res = mpq_init()
    mpq_set(res,q)
    return res
end function

integer x_mpq_get_num = NULL

global procedure mpq_get_num(mpz numerator, mpq rational)
    if numerator=NULL then ?9/0 end if
    if rational=NULL then ?9/0 end if
    if x_mpq_get_num=NULL then
        x_mpq_get_num = link_c_proc(mpir_dll, "+__gmpq_get_num", {P,P})
    end if
    c_proc(x_mpq_get_num,{numerator,rational})
end procedure

integer x_mpq_get_den = NULL

global procedure mpq_get_den(mpz denominator, mpq rational)
-- rop := op/1
    if denominator=NULL then ?9/0 end if
    if rational=NULL then ?9/0 end if
    if x_mpq_get_den=NULL then
        x_mpq_get_den = link_c_proc(mpir_dll, "+__gmpq_get_den", {P,P})
    end if
    c_proc(x_mpq_get_den,{denominator,rational})
end procedure

integer x_mpq_get_d = NULL

global function mpq_get_d(mpq op)
-- return (atom) floor(mpq_get_num(op)/mpq_get_den(op))
--Convert op to a double, truncating if necessary (ie. rounding towards zero).
--If the exponent from the conversion is too big or too small to fit a double then the result is
--system dependent. For too big an infinity is returned when available. For too small 0:0 is
--normally returned. Hardware overflow, underflow and denorm traps may or may not occur.

    if op=NULL then ?9/0 end if
    if x_mpq_get_d=NULL then
        x_mpq_get_d = link_c_func(mpir_dll, "+__gmpq_get_d", {P},D)
    end if
    atom res = c_func(x_mpq_get_d,{op})
    return res
end function


integer x_mpq_add = NULL

global procedure mpq_add(mpq rsum, addend1, addend2)
-- set rsum to addend1 + addend2.
    if rsum=NULL then ?9/0 end if
    if addend1=NULL then ?9/0 end if
    if addend2=NULL then ?9/0 end if
    if x_mpq_add=NULL then
        x_mpq_add = link_c_proc(mpir_dll, "+__gmpq_add", {P,P,P})
    end if
    c_proc(x_mpq_add,{rsum,addend1,addend2})
end procedure

integer x_mpq_sub = NULL

global procedure mpq_sub(mpq rdifference, minuend, subtrahend)
-- set rdifference to minuend - subtrahend.
    if rdifference=NULL then ?9/0 end if
    if minuend=NULL then ?9/0 end if
    if subtrahend=NULL then ?9/0 end if
    if x_mpq_sub=NULL then
        x_mpq_sub = link_c_proc(mpir_dll, "+__gmpq_sub", {P,P,P})
    end if
    c_proc(x_mpq_sub,{rdifference,minuend,subtrahend})
end procedure

integer x_mpq_mul = NULL

global procedure mpq_mul(mpq rproduct, multiplier, multiplicand)
-- set rproduct to multiplier * multiplicand.
    if rproduct=NULL then ?9/0 end if
    if multiplier=NULL then ?9/0 end if
    if multiplicand=NULL then ?9/0 end if
    if x_mpq_mul=NULL then
        x_mpq_mul = link_c_proc(mpir_dll, "+__gmpq_mul", {P,P,P})
    end if
    c_proc(x_mpq_mul,{rproduct,multiplier,multiplicand})
end procedure

integer x_mpq_mul_2exp = NULL

global procedure mpq_mul_2exp(mpq rop, op, integer bits)
-- set rop to op * 2^bits
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if bits<0 then ?9/0 end if  -- (not sure about that...)
    if x_mpq_mul_2exp=NULL then
        x_mpq_mul_2exp = link_c_proc(mpir_dll, "+__gmpq_mul_2exp", {P,P,I})
    end if
    c_proc(x_mpq_mul_2exp,{rop,op,bits})
end procedure

integer x_mpq_div_2exp = NULL

global procedure mpq_div_2exp(mpq rop, op, integer bits)
-- set rop to op / 2^bits
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if bits<0 then ?9/0 end if  -- (not sure about that...)
    if x_mpq_div_2exp=NULL then
        x_mpq_div_2exp = link_c_proc(mpir_dll, "+__gmpq_div_2exp", {P,P,I})
    end if
    c_proc(x_mpq_div_2exp,{rop,op,bits})
end procedure

integer x_mpq_neg = NULL

global procedure mpq_neg(mpq rop, op)
-- set rop to -op.
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpq_neg=NULL then
        x_mpq_neg = link_c_proc(mpir_dll, "+__gmpq_neg", {P,P})
    end if
    c_proc(x_mpq_neg,{rop,op})
end procedure

integer x_mpq_abs = NULL

global procedure mpq_abs(mpq rop, op)
-- set rop to the absolute value of op.
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpq_abs=NULL then
        x_mpq_abs = link_c_proc(mpir_dll, "+__gmpq_abs", {P,P})
    end if
    c_proc(x_mpq_abs,{rop,op})
end procedure

integer x_mpq_inv = NULL

global procedure mpq_inv(mpq rop, op)
-- set rop to 1/op. If the new denominator is zero, this routine will divide by zero.
    if rop=NULL then ?9/0 end if
    if op=NULL then ?9/0 end if
    if x_mpq_inv=NULL then
        x_mpq_inv = link_c_proc(mpir_dll, "+__gmpq_inv", {P,P})
    end if
    c_proc(x_mpq_inv,{rop,op})
end procedure

global procedure mpq_add_si(mpq rsum, addend1, integer n, d=1)
    mpq addend2 = mpq_init_set_si(n,d)
    mpq_add(rsum, addend1, addend2)
    addend2 = mpq_free(addend2)
end procedure

global function mpq_get_str(mpq op, integer base=10, boolean comma_fill=false)
    mpz nd = mpz_init()
    mpq_get_num(nd, op)
    string res = mpz_get_str(nd,base,comma_fill)
    mpq_get_den(nd, op)
    if mpz_cmp_si(nd,1)!=0 then
        res &= "/" & mpz_get_str(nd,base,comma_fill)
    end if
    nd = mpz_free(nd)
    return res
end function

integer x_mpq_cmp = NULL

global function mpq_cmp(mpq op1, op2)
    if op1=NULL then ?9/0 end if
    if op2=NULL then ?9/0 end if
    if x_mpq_cmp=NULL then
        x_mpq_cmp = link_c_func(mpir_dll, "+__gmpq_cmp", {P,P},I)
    end if
    integer res = c_func(x_mpq_cmp,{op1,op2})
--if not find(res,{-1,0,+1}) then ?9/0 end if
    res = sign(res)
    return res
end function

integer x_mpq_cmp_si = NULL

global function mpq_cmp_si(mpq op1, integer n, d=1)
    if op1=NULL then ?9/0 end if
    if d<0 then ?9/0 end if
    if x_mpq_cmp_si=NULL then
        x_mpq_cmp_si = link_c_func(mpir_dll, "+__gmpq_cmp_si", {P,I,I},I)
--      x_mpq_cmp_si = link_c_func(mpir_dll, "+__gmpq_cmp_ui", {P,I,I},I)
    end if
    integer res = c_func(x_mpq_cmp_si,{op1,n,d})
--if not find(res,{-1,0,+1}) then ?9/0 end if
    res = sign(res)
    return res
end function

--<clean to here>

--/*
char * mpq_get_str (char *str, int base, mpq t op) [Function]
Convert op to a string of digits in base base. The base may vary from 2 to 36. The string
will be of the form ‘num/den’, or if the denominator is 1 then just ‘num’.
If str is NULL, the result string is allocated using the current allocation function (see
Chapter 13 [Custom Allocation], page 86). The block will be strlen(str)+1 bytes, that
being exactly enough for the string and null-terminator.
If str is not NULL, it should point to a block of storage large enough for the result, that being
mpz_sizeinbase (mpq_numref(op), base)
+ mpz_sizeinbase (mpq_denref(op), base) + 3
The three extra bytes are for a possible minus sign, possible slash, and the null-terminator.
A pointer to the result string is returned, being either the allocated block, or the given str.


mpfr_prec_t typedef sdword
mpfr_sign_t typedef sdword
mp_limb_t typedef dword
pmp_limb_t typedef ptr mp_limb_t
mp_prec_t typedef mpfr_prec_t
mp_exp_t typedef sdword

__mpfr_struct struct
        _mpfr_prec mpfr_prec_t ?
        _mpfr_sign mpfr_sign_t ?
        _mpfr_exp int ?
        _mpfr_d pmp_limb_t  ?
__mpfr_struct ends

mpfr_ptr typedef ptr __mpfr_struct
mpfr_srcptr typedef ptr __mpfr_struct

mpfr_t typedef __mpfr_struct

mpfr_rnd_t typedef sdword

--mpfr_init proto C :mpfr_ptr
mpfr_cache proto C :mpfr_ptr, :ptr mpfr_cache_t, :mpfr_rnd_t

dependency walker, mpfr32.dll:
__gmpfr_cache_const_catalan_f
__gmpfr_cache_const_euler_f
__gmpfr_cache_const_log2_f
__gmpfr_cache_const_pi_f
__gmpfr_ceil_exp2
__gmpfr_ceil_log2
__gmpfr_cuberoot
__gmpfr_default_fp_bit_precision_f
__gmpfr_default_rounding_mode_f
__gmpfr_emax_f
__gmpfr_emin_f
__gmpfr_flags_f
__gmpfr_floor_log2
__gmpfr_four
__gmpfr_int_ceil_log2
__gmpfr_isqrt
__gmpfr_l2b
__gmpfr_mpfr_get_sj
__gmpfr_mpfr_get_uj
__gmpfr_one
__gmpfr_set_sj
__gmpfr_set_sj_2exp
__gmpfr_set_uj
__gmpfr_set_uj_2exp
__gmpfr_two
mpfr_abort_prec_max
mpfr_abs
mpfr_acos
mpfr_acosh
--mpfr_add
mpfr_add1
mpfr_add1sp
mpfr_add_d
mpfr_add_q
mpfr_add_z
mpfr_agm
mpfr_ai
mpfr_asin
mpfr_asinh
--mpfr_asprintf
mpfr_atan
mpfr_atan2
mpfr_atanh
mpfr_bernoulli_internal
mpfr_buildopt_decimal_p
mpfr_buildopt_gmpinternals_p
mpfr_buildopt_tls_p
mpfr_buildopt_tune_case
mpfr_cache
mpfr_can_round
mpfr_can_round_raw
mpfr_cbrt
mpfr_ceil
mpfr_ceil_mul
mpfr_check
mpfr_check_range
--mpfr_clear
mpfr_clear_cache
mpfr_clear_divby0
mpfr_clear_erangeflag
mpfr_clear_flags
mpfr_clear_inexflag
mpfr_clear_nanflag
mpfr_clear_overflow
mpfr_clear_underflow
--mpfr_clears
--mpfr_cmp
mpfr_cmp2
mpfr_cmp3
mpfr_cmp_d
mpfr_cmp_ld
mpfr_cmp_q
--mpfr_cmp_si
mpfr_cmp_si_2exp
--mpfr_cmp_ui
mpfr_cmp_ui_2exp
mpfr_cmp_z
mpfr_cmpabs
mpfr_const_catalan
mpfr_const_catalan_internal
--mpfr_const_euler
mpfr_const_euler_internal
mpfr_const_log2
mpfr_const_log2_internal
--mpfr_const_pi
mpfr_const_pi_internal
mpfr_copysign
mpfr_cos
mpfr_cosh
mpfr_cot
mpfr_coth
mpfr_csc
mpfr_csch
mpfr_d_div
mpfr_d_sub
mpfr_digamma
mpfr_dim
--mpfr_div
mpfr_div_2si
mpfr_div_2ui    --(use this instead of mpfr_div_2exp)
mpfr_div_d
mpfr_div_q
--mpfr_div_si
--mpfr_div_ui
mpfr_div_ui2
--mpfr_div_z
mpfr_divby0_p
mpfr_divhigh_n
mpfr_eint
mpfr_equal_p
mpfr_erangeflag_p
mpfr_erf
mpfr_erfc
--mpfr_exp
mpfr_exp10
mpfr_exp2
mpfr_exp_2
mpfr_exp_3
mpfr_expm1
mpfr_extract
mpfr_fac_ui
mpfr_fits_intmax_p
mpfr_fits_sint_p
mpfr_fits_slong_p
mpfr_fits_sshort_p
mpfr_fits_uint_p
mpfr_fits_uintmax_p
mpfr_fits_ulong_p
mpfr_fits_ushort_p
--mpfr_floor
mpfr_fma
mpfr_fmod
mpfr_fms
mpfr_fprint_binary
mpfr_frac
mpfr_free_cache
mpfr_frexp
--mpfr_gamma
mpfr_gamma_one_and_two_third
mpfr_get_d
mpfr_get_d1
mpfr_get_d_2exp
--mpfr_get_default_prec
--mpfr_get_default_rounding_mode
mpfr_get_emax
mpfr_get_emax_max
mpfr_get_emax_min
mpfr_get_emin
mpfr_get_emin_max
mpfr_get_emin_min
mpfr_get_exp
mpfr_get_flt
mpfr_get_ld
mpfr_get_ld_2exp
mpfr_get_patches
--mpfr_get_prec
--mpfr_get_si
--mpfr_get_str
--Xmpfr_get_ui
mpfr_get_z
mpfr_get_z_2exp
mpfr_grandom
mpfr_greater_p
mpfr_greaterequal_p
mpfr_hypot
mpfr_inexflag_p
mpfr_inf_p
--mpfr_init
--mpfr_init2
--mpfr_init_set_str
mpfr_integer_p
mpfr_j0
mpfr_j1
mpfr_jn
mpfr_less_p
mpfr_lessequal_p
mpfr_lessgreater_p
mpfr_lgamma
mpfr_li2
mpfr_lngamma
--mpfr_log
mpfr_log10
mpfr_log1p
mpfr_log2
mpfr_max
mpfr_min
mpfr_min_prec
mpfr_modf
mpfr_mpn_exp
--mpfr_mul
mpfr_mul_2si
mpfr_mul_2ui    -- (use this instead of mpfr_mul_2exp)
mpfr_mul_d
mpfr_mul_q
--mpfr_mul_si
mpfr_mul_z
mpfr_mulhigh_n
mpfr_mullow_n
mpfr_nan_p
mpfr_nanflag_p
--mpfr_neg
mpfr_nextabove
mpfr_nextbelow
mpfr_nexttoinf
mpfr_nexttoward
mpfr_nexttozero
mpfr_number_p
mpfr_overflow
mpfr_overflow_p
--mpfr_pow
mpfr_pow_general
--mpfr_pow_si
--mpfr_pow_ui
mpfr_pow_z
mpfr_powerof2_raw
mpfr_powerof2_raw2
mpfr_prec_round
mpfr_print_binary
mpfr_print_mant_binary
mpfr_print_rnd_mode
mpfr_rand_raw
mpfr_rec_sqrt
mpfr_regular_p
mpfr_remainder
mpfr_remquo
mpfr_rint
mpfr_rint_ceil
mpfr_rint_floor
mpfr_rint_round
mpfr_rint_trunc
--mpfr_root  (deprecated)
mpfr_round
mpfr_round_near_x
mpfr_round_p
mpfr_round_raw
mpfr_round_raw_2
mpfr_round_raw_4
mpfr_scale2
mpfr_sec
mpfr_sech
--mpfr_set
mpfr_set4
mpfr_set_divby0
mpfr_set_emax
mpfr_set_emin
mpfr_set_erangeflag
mpfr_set_exp
mpfr_set_flt
mpfr_set_inexflag
mpfr_set_inf
mpfr_set_ld
mpfr_set_nan
mpfr_set_nanflag
mpfr_set_overflow
--mpfr_set_prec
--mpfr_set_q
--mpfr_set_si
mpfr_set_si_2exp
--mpfr_set_str
mpfr_set_str_binary
--mpfr_set_ui
mpfr_set_ui_2exp
mpfr_set_underflow
--mpfr_set_z
mpfr_set_z_2exp
mpfr_set_zero
mpfr_setmax
mpfr_setmin
mpfr_setsign
mpfr_sgn
--mpfr_si_div
--mpfr_si_sub
mpfr_signbit
--mpfr_sin
mpfr_sin_cos
mpfr_sincos_fast
mpfr_sinh
mpfr_sinh_cosh
--mpfr_sprintf  -- (asprintf)
--mpfr_sqr
mpfr_sqrhigh_n
--mpfr_sqrt
--mpfr_sqrt_ui
mpfr_strtofr
--mpfr_sub
mpfr_sub1
mpfr_sub1sp
mpfr_sub_d
mpfr_sub_q
--mpfr_sub_si
--mpfr_sub_ui
mpfr_sub_z
mpfr_subnormalize
mpfr_sum
mpfr_sum_sort
mpfr_swap
mpfr_tan
mpfr_tanh
mpfr_trunc
mpfr_ui_div
mpfr_ui_pow
mpfr_ui_pow_ui
mpfr_ui_sub
mpfr_underflow
mpfr_underflow_p
mpfr_unordered_p
mpfr_urandom
mpfr_urandomb
mpfr_y0
mpfr_y1
mpfr_yn
mpfr_z_sub
mpfr_zero_p
mpfr_zeta
mpfr_zeta_ui


Dependency walker, mpir32.dll:

??5@YAAAV?$basic_istream@DU?$char_traits@D@std@@@std@@AAV01@PAU__mpf_struct@@@Z
??5@YAAAV?$basic_istream@DU?$char_traits@D@std@@@std@@AAV01@PAU__mpq_struct@@@Z
??5@YAAAV?$basic_istream@DU?$char_traits@D@std@@@std@@AAV01@PAU__mpz_struct@@@Z
??6@YAAAV?$basic_ostream@DU?$char_traits@D@std@@@std@@AAV01@PBU__mpf_struct@@@Z
??6@YAAAV?$basic_ostream@DU?$char_traits@D@std@@@std@@AAV01@PBU__mpq_struct@@@Z
??6@YAAAV?$basic_ostream@DU?$char_traits@D@std@@@std@@AAV01@PBU__mpz_struct@@@Z
--  (undecorated:)
--  class std::basic_ostream<char,struct std::char_traits<char> > & operator<<(class std::basic_ostream<char,struct std::char_traits<char> > &,struct __mpf_struct const *)
--  class std::basic_ostream<char,struct std::char_traits<char> > & operator<<(class std::basic_ostream<char,struct std::char_traits<char> > &,struct __mpq_struct const *)
--  class std::basic_ostream<char,struct std::char_traits<char> > & operator<<(class std::basic_ostream<char,struct std::char_traits<char> > &,struct __mpz_struct const *)
--  class std::basic_istream<char,struct std::char_traits<char> > & operator>>(class std::basic_istream<char,struct std::char_traits<char> > &,struct __mpf_struct *)
--  class std::basic_istream<char,struct std::char_traits<char> > & operator>>(class std::basic_istream<char,struct std::char_traits<char> > &,struct __mpq_struct *)
--  class std::basic_istream<char,struct std::char_traits<char> > & operator>>(class std::basic_istream<char,struct std::char_traits<char> > &,struct __mpz_struct *)
__combine_limbs
__fermat_to_mpz
__gmp_0
__gmp_allocate_func
__gmp_asprintf
__gmp_asprintf_final
__gmp_asprintf_memory
__gmp_asprintf_reps
__gmp_assert_fail
__gmp_assert_header
__gmp_bits_per_limb
__gmp_default_allocate
__gmp_default_fp_limb_precision
__gmp_default_free
__gmp_default_reallocate
__gmp_divide_by_zero
__gmp_doprnt
__gmp_doprnt_integer
__gmp_doprnt_mpf2
__gmp_doscan
__gmp_errno
__gmp_exception
__gmp_extract_double
--__gmp_fib_table
__gmp_fprintf
__gmp_free_func
__gmp_fscanf
__gmp_get_memory_functions
__gmp_init_primesieve
__gmp_invalid_operation
__gmp_jacobi_table
__gmp_junk
__gmp_modlimb_invert_table
__gmp_nextprime
__gmp_primesieve
__gmp_printf
--__gmp_randclear
__gmp_randinit_default
__gmp_randinit_lc_2exp
__gmp_randinit_lc_2exp_size
--__gmp_randinit_mt
__gmp_randinit_mt_noseed
__gmp_randinit_set
__gmp_rands
__gmp_rands_initialized
--__gmp_randseed
__gmp_randseed_ui
__gmp_reallocate_func
__gmp_replacement_vsnprintf
__gmp_scanf
__gmp_set_memory_functions
__gmp_snprintf
__gmp_sprintf
__gmp_sqrt_of_negative
__gmp_sscanf
__gmp_tmp_reentrant_alloc
__gmp_tmp_reentrant_free
__gmp_urandomb_ui
__gmp_urandomm_ui
__gmp_vasprintf
__gmp_vfprintf
__gmp_vfscanf
__gmp_vprintf
__gmp_vscanf
__gmp_vsnprintf
__gmp_vsprintf
__gmp_vsscanf
__gmpn_add
__gmpn_add_1
__gmpn_add_err1_n
__gmpn_add_err2_n
__gmpn_add_n
__gmpn_add_nc
__gmpn_addadd_n
__gmpn_addmul_1
__gmpn_addmul_1c
__gmpn_addmul_2
__gmpn_addsub_n
__gmpn_and_n
__gmpn_andn_n
__gmpn_bases
__gmpn_bc_set_str
__gmpn_bdivmod
__gmpn_clz_tab
__gmpn_cmp
__gmpn_com_n
__gmpn_copyd
__gmpn_copyi
__gmpn_dc_bdiv_q
__gmpn_dc_bdiv_q_n
__gmpn_dc_bdiv_qr
__gmpn_dc_bdiv_qr_n
__gmpn_dc_div_q
__gmpn_dc_div_qr
__gmpn_dc_div_qr_n
__gmpn_dc_divappr_q
__gmpn_dc_set_str
__gmpn_div_2expmod_2expp1
__gmpn_divexact
__gmpn_divexact_1
__gmpn_divexact_by3c
__gmpn_divexact_byff
__gmpn_divexact_byfobm1
__gmpn_divisible_p
__gmpn_divrem
__gmpn_divrem_1
__gmpn_divrem_1c
__gmpn_divrem_2
__gmpn_divrem_euclidean_qr_1
__gmpn_divrem_euclidean_qr_2
__gmpn_divrem_euclidean_r_1
__gmpn_divrem_hensel_qr_1
__gmpn_divrem_hensel_qr_1_1
__gmpn_divrem_hensel_qr_1_2
__gmpn_divrem_hensel_r_1
__gmpn_divrem_hensel_rsh_qr_1
__gmpn_divrem_hensel_rsh_qr_1_preinv
__gmpn_dump
--__gmpn_fib2_ui
__gmpn_gcd
__gmpn_gcd_1
__gmpn_gcd_subdiv_step
__gmpn_gcdext
__gmpn_gcdext_1
__gmpn_gcdext_lehmer_n
__gmpn_get_d
__gmpn_get_str
__gmpn_hamdist
__gmpn_hgcd
__gmpn_hgcd2
__gmpn_hgcd2_jacobi
__gmpn_hgcd_appr
__gmpn_hgcd_appr_itch
__gmpn_hgcd_itch
__gmpn_hgcd_jacobi
__gmpn_hgcd_matrix_adjust
__gmpn_hgcd_matrix_init
__gmpn_hgcd_matrix_mul
__gmpn_hgcd_matrix_mul_1
__gmpn_hgcd_matrix_update_q
__gmpn_hgcd_mul_matrix1_vector
__gmpn_hgcd_reduce
__gmpn_hgcd_reduce_itch
__gmpn_hgcd_step
__gmpn_inv_div_q
__gmpn_inv_div_qr
__gmpn_inv_div_qr_n
__gmpn_inv_divappr_q
__gmpn_inv_divappr_q_n
__gmpn_invert
__gmpn_invert_trunc
__gmpn_ior_n
__gmpn_iorn_n
__gmpn_is_invert
__gmpn_jacobi_2
__gmpn_jacobi_base
__gmpn_jacobi_n
__gmpn_kara_mul_n
__gmpn_kara_sqr_n
__gmpn_lshift
__gmpn_matrix22_mul
__gmpn_matrix22_mul1_inverse_vector
__gmpn_matrix22_mul_itch
__gmpn_matrix22_mul_strassen
__gmpn_mod_1
__gmpn_mod_1_1
__gmpn_mod_1_2
__gmpn_mod_1_3
__gmpn_mod_1_k
__gmpn_mod_1c
__gmpn_mod_34lsub1
__gmpn_modexact_1c_odd
__gmpn_mul
__gmpn_mul_1
__gmpn_mul_2expmod_2expp1
__gmpn_mul_basecase
__gmpn_mul_fft
__gmpn_mul_fft_main
__gmpn_mul_mfa_trunc_sqrt2
__gmpn_mul_n
__gmpn_mul_trunc_sqrt2
__gmpn_mulhigh_n
__gmpn_mullow_basecase
__gmpn_mullow_n
__gmpn_mullow_n_basecase
__gmpn_mulmid
__gmpn_mulmid_basecase
__gmpn_mulmid_n
__gmpn_mulmod_2expm1
__gmpn_mulmod_2expp1_basecase
__gmpn_mulmod_Bexpp1
__gmpn_mulmod_Bexpp1_fft
__gmpn_mulmod_bnm1
__gmpn_nand_n
__gmpn_neg_n
__gmpn_nior_n
__gmpn_normmod_2expp1
__gmpn_perfect_square_p
__gmpn_popcount
__gmpn_pow_1
__gmpn_random
__gmpn_random2
__gmpn_randomb
__gmpn_redc_1
__gmpn_redc_2
__gmpn_rootrem
__gmpn_rootrem_basecase
__gmpn_rrandom
__gmpn_rsh_divrem_hensel_qr_1
__gmpn_rsh_divrem_hensel_qr_1_1
__gmpn_rsh_divrem_hensel_qr_1_2
__gmpn_rshift
__gmpn_sb_bdiv_q
__gmpn_sb_bdiv_qr
__gmpn_sb_div_q
__gmpn_sb_div_qr
__gmpn_sb_divappr_q
__gmpn_scan0
__gmpn_scan1
__gmpn_set_str
__gmpn_set_str_compute_powtab
__gmpn_sqr
__gmpn_sqr_basecase
__gmpn_sqrtrem
__gmpn_sub
__gmpn_sub_1
__gmpn_sub_err1_n
__gmpn_sub_err2_n
__gmpn_sub_n
__gmpn_sub_nc
__gmpn_subadd_n
__gmpn_submul_1
__gmpn_submul_1c
__gmpn_sumdiff_n
__gmpn_tdiv_q
__gmpn_tdiv_qr
__gmpn_toom32_mul
__gmpn_toom3_interpolate
__gmpn_toom3_mul
__gmpn_toom3_mul_n
__gmpn_toom3_sqr_n
__gmpn_toom42_mul
__gmpn_toom42_mulmid
__gmpn_toom4_interpolate
__gmpn_toom4_mul
__gmpn_toom4_mul_n
__gmpn_toom4_sqr_n
__gmpn_toom53_mul
__gmpn_toom8_sqr_n
__gmpn_toom8h_mul
__gmpn_toom_couple_handling
__gmpn_toom_eval_dgr3_pm1
__gmpn_toom_eval_dgr3_pm2
__gmpn_toom_eval_pm1
__gmpn_toom_eval_pm2
__gmpn_toom_eval_pm2exp
__gmpn_toom_eval_pm2rexp
__gmpn_toom_interpolate_16pts
__gmpn_udiv_qrnnd
__gmpn_umul_ppmm
__gmpn_urandomb
__gmpn_urandomm
__gmpn_xnor_n
__gmpn_xor_n
__gmpn_zero
__gmpq_abs
--__gmpq_add
__gmpq_canonicalize
--__gmpq_clear
--__gmpq_clears
__gmpq_cmp
--__gmpq_cmp_si
--__gmpq_cmp_ui
__gmpq_cmp_z
--__gmpq_div
--__gmpq_div_2exp
__gmpq_equal
--__gmpq_get_d
--__gmpq_get_den
--__gmpq_get_num
__gmpq_get_str
--__gmpq_init
--__gmpq_inits
__gmpq_inp_str
__gmpq_inv
--__gmpq_mul
--__gmpq_mul_2exp
--__gmpq_neg
__gmpq_out_str
__gmpq_set
__gmpq_set_d
__gmpq_set_den
__gmpq_set_num
__gmpq_set_si
__gmpq_set_str
__gmpq_set_ui
__gmpq_set_z
--__gmpq_sub
__gmpq_swap
__gmpz_2fac_ui
--__gmpz_abs
--__gmpz_add
--__gmpz_add_ui
__gmpz_addmul
--__gmpz_addmul_ui
__gmpz_and
__gmpz_aorsmul_1
__gmpz_array_init
__gmpz_bin_ui
__gmpz_bin_uiui
--__gmpz_cdiv_q
__gmpz_cdiv_q_2exp
__gmpz_cdiv_q_ui
__gmpz_cdiv_qr
__gmpz_cdiv_qr_ui
__gmpz_cdiv_r
__gmpz_cdiv_r_2exp
__gmpz_cdiv_r_ui
__gmpz_cdiv_ui
--__gmpz_clear
__gmpz_clrbit
--__gmpz_cmp
__gmpz_cmp_d
--__gmpz_cmp_si
--__gmpz_cmp_ui
__gmpz_cmpabs
__gmpz_cmpabs_d
__gmpz_cmpabs_ui
__gmpz_com
__gmpz_combit
__gmpz_congruent_2exp_p
__gmpz_congruent_p
__gmpz_congruent_ui_p
__gmpz_divexact
__gmpz_divexact_gcd
__gmpz_divexact_ui
__gmpz_divisible_2exp_p
--__gmpz_divisible_p
__gmpz_divisible_ui_p
__gmpz_dump
--__gmpz_export
__gmpz_fac_ui
__gmpz_fdiv_q
--__gmpz_fdiv_q_2exp
__gmpz_fdiv_q_ui
__gmpz_fdiv_qr
__gmpz_fdiv_qr_ui
--__gmpz_fdiv_r
__gmpz_fdiv_r_2exp
--__gmpz_fdiv_r_ui
--__gmpz_fdiv_ui
--__gmpz_fib2_ui
--__gmpz_fib_ui
__gmpz_fits_si_p
__gmpz_fits_sint_p
--__gmpz_fits_slong_p
__gmpz_fits_sshort_p
__gmpz_fits_ui_p
__gmpz_fits_uint_p
--__gmpz_fits_ulong_p
__gmpz_fits_ushort_p
--__gmpz_gcd
--__gmpz_gcd_ui
__gmpz_gcdext
__gmpz_get_d
__gmpz_get_d_2exp
--__gmpz_get_si
--__gmpz_get_str
__gmpz_get_sx
--__gmpz_get_ui
__gmpz_get_ux
__gmpz_getlimbn
__gmpz_hamdist
--__gmpz_import
--__gmpz_init
--__gmpz_init2
--__gmpz_init_set
--__gmpz_init_set_d
--__gmpz_init_set_si
--__gmpz_init_set_str
__gmpz_init_set_sx
--__gmpz_init_set_ui
__gmpz_init_set_ux
__gmpz_inp_raw
__gmpz_inp_str
__gmpz_inp_str_nowhite
__gmpz_invert
__gmpz_ior
__gmpz_jacobi
__gmpz_kronecker_si
__gmpz_kronecker_ui
--__gmpz_lcm
--__gmpz_lcm_ui
__gmpz_likely_prime_p
__gmpz_lucnum2_ui
__gmpz_lucnum_ui
__gmpz_mfac_uiui
__gmpz_miller_rabin
__gmpz_millerrabin
--__gmpz_mod
--__gmpz_mul
--__gmpz_mul_2exp
--__gmpz_mul_si
--__gmpz_mul_ui
__gmpz_n_pow_ui
--__gmpz_neg
__gmpz_next_prime_candidate
__gmpz_nextprime
--__gmpz_nthroot
__gmpz_oddfac_1
__gmpz_out_raw
__gmpz_out_str
__gmpz_perfect_power_p
__gmpz_perfect_square_p
__gmpz_popcount
--__gmpz_pow_ui
--__gmpz_powm
--__gmpz_powm_ui
__gmpz_primorial_ui
__gmpz_probab_prime_p
--__gmpz_probable_prime_p
__gmpz_prodlimbs
__gmpz_realloc
__gmpz_realloc2
--__gmpz_remove
--__gmpz_root
__gmpz_rootrem
__gmpz_rrandomb
--__gmpz_scan0
--__gmpz_scan1
--__gmpz_set
--__gmpz_set_d
__gmpz_set_q
--__gmpz_set_si
--__gmpz_set_str
__gmpz_set_sx
--__gmpz_set_ui
__gmpz_set_ux
__gmpz_setbit
__gmpz_si_kronecker
--__gmpz_size
--__gmpz_sizeinbase
--__gmpz_sqrt
--__gmpz_sqrtrem
--__gmpz_sub
--__gmpz_sub_ui
__gmpz_submul
__gmpz_submul_ui
__gmpz_swap
__gmpz_tdiv_q
--__gmpz_tdiv_q_2exp
__gmpz_tdiv_q_ui
__gmpz_tdiv_qr
__gmpz_tdiv_qr_ui
__gmpz_tdiv_r
--__gmpz_tdiv_r_2exp
__gmpz_tdiv_r_ui
__gmpz_tdiv_ui
__gmpz_trial_division
--__gmpz_tstbit
__gmpz_ui_kronecker
--__gmpz_ui_pow_ui
--__gmpz_ui_sub
__gmpz_urandomb
--__gmpz_urandomm
__gmpz_xor
__mpir_butterfly_lshB
__mpir_butterfly_rshB
__mpir_fft_adjust
__mpir_fft_adjust_limbs
__mpir_fft_adjust_sqrt2
__mpir_fft_butterfly
__mpir_fft_butterfly_sqrt2
__mpir_fft_butterfly_twiddle
__mpir_fft_combine_bits
__mpir_fft_mfa_trunc_sqrt2
__mpir_fft_mfa_trunc_sqrt2_inner
__mpir_fft_mfa_trunc_sqrt2_outer
__mpir_fft_mulmod_2expp1
__mpir_fft_naive_convolution_1
__mpir_fft_negacyclic
__mpir_fft_radix2
__mpir_fft_radix2_twiddle
__mpir_fft_split_bits
__mpir_fft_split_limbs
__mpir_fft_trunc
__mpir_fft_trunc1
__mpir_fft_trunc1_twiddle
__mpir_fft_trunc_sqrt2
__mpir_ifft_butterfly
__mpir_ifft_butterfly_sqrt2
__mpir_ifft_butterfly_twiddle
__mpir_ifft_mfa_trunc_sqrt2
__mpir_ifft_mfa_trunc_sqrt2_outer
__mpir_ifft_negacyclic
__mpir_ifft_radix2
__mpir_ifft_radix2_twiddle
__mpir_ifft_trunc
__mpir_ifft_trunc1
__mpir_ifft_trunc1_twiddle
__mpir_ifft_trunc_sqrt2
mpir_is_likely_prime_BPSW
__mpir_revbin
mpir_sqrt

mpfr.h:
/* mpfr.h -- Include file for mpfr.

Copyright 1999-2017 Free Software Foundation, Inc.
Contributed by the AriC and Caramba projects, INRIA.

This file is part of the GNU MPFR Library.

The GNU MPFR Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The GNU MPFR Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MPFR Library; see the file COPYING.LESSER.  If not, see
http://www.gnu.org/licenses/ or write to the Free Software Foundation, Inc.,
51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA. */

#ifndef __MPFR_H
#define __MPFR_H

/* Define MPFR version number */
#define MPFR_VERSION_MAJOR 4
#define MPFR_VERSION_MINOR 0
#define MPFR_VERSION_PATCHLEVEL 0
#define MPFR_VERSION_STRING "4.0.0-dev"

--/* User macros:
--   MPFR_USE_FILE:      Define it to make MPFR define functions dealing
--                       with FILE* (auto-detect).
--   MPFR_USE_INTMAX_T:  Define it to make MPFR define functions dealing
--                       with intmax_t (auto-detect).
--   MPFR_USE_VA_LIST:   Define it to make MPFR define functions dealing
--                       with va_list (auto-detect).
--   MPFR_USE_C99_FEATURE: Define it to 1 to make MPFR support C99-feature
--                       (auto-detect), to 0 to bypass the detection.
--   MPFR_USE_EXTENSION:     Define it to make MPFR use GCC extension to
--                       reduce warnings.
--   MPFR_USE_NO_MACRO:  Define it to make MPFR remove any overriding
--                       function macro.
--*/

/* Macros dealing with MPFR VERSION */
#define MPFR_VERSION_NUM(a,b,c) (((a) << 16L) | ((b) << 8) | (c))
#define MPFR_VERSION \
MPFR_VERSION_NUM(MPFR_VERSION_MAJOR,MPFR_VERSION_MINOR,MPFR_VERSION_PATCHLEVEL)

#include <gmp.h>

/* Avoid some problems with macro expansion if the user defines macros
   with the same name as keywords. By convention, identifiers and macro
   names starting with mpfr_ are reserved by MPFR. */
typedef void            mpfr_void;
typedef int             mpfr_int;
typedef unsigned int    mpfr_uint;
typedef long            mpfr_long;
typedef unsigned long   mpfr_ulong;
typedef size_t          mpfr_size_t;

/* Global (possibly TLS) flags. Might also be used in an mpfr_t in the
   future (there would be room as mpfr_sign_t just needs 1 byte).
   TODO: The tests currently assume that the flags fits in an unsigned int;
   this should be cleaned up, e.g. by defining a function that outputs the
   flags as a string or by using the flags_out function (from tests/tests.c
   directly). */
typedef unsigned int    mpfr_flags_t;

/* Flags macros (in the public API) */
#define MPFR_FLAGS_UNDERFLOW 1
#define MPFR_FLAGS_OVERFLOW 2
#define MPFR_FLAGS_NAN 4
#define MPFR_FLAGS_INEXACT 8
#define MPFR_FLAGS_ERANGE 16
#define MPFR_FLAGS_DIVBY0 32
#define MPFR_FLAGS_ALL (MPFR_FLAGS_UNDERFLOW | \
                        MPFR_FLAGS_OVERFLOW  | \
                        MPFR_FLAGS_NAN       | \
                        MPFR_FLAGS_INEXACT   | \
                        MPFR_FLAGS_ERANGE    | \
                        MPFR_FLAGS_DIVBY0)

--/* Definition of rounding modes (DON'T USE MPFR_RNDNA!).
--   Warning! Changing the contents of this enum should be seen as an
--   interface change since the old and the new types are not compatible
--   (the integer type compatible with the enumerated type can even change,
--   see ISO C99, 6.7.2.2#4), and in Makefile.am, AGE should be set to 0.
--
--   MPFR_RNDU must appear just before MPFR_RNDD (see
--   MPFR_IS_RNDUTEST_OR_RNDDNOTTEST in mpfr-impl.h).
--
--   If you change the order of the rounding modes, please update the routines
--   in texceptions.c which assume 0=RNDN, 1=RNDZ, 2=RNDU, 3=RNDD, 4=RNDA.
--*/
--typedef enum {
--  MPFR_RNDN=0,    /* round to nearest, with ties to even */
--  MPFR_RNDZ,  /* round toward zero */
--  MPFR_RNDU,  /* round toward +Inf */
--  MPFR_RNDD,  /* round toward -Inf */
--  MPFR_RNDA,  /* round away from zero */
--  MPFR_RNDF,  /* faithful rounding */
--  MPFR_RNDNA=-1 /* round to nearest, with ties away from zero (mpfr_round) */
--} mpfr_rnd_t;
--
--/* kept for compatibility with MPFR 2.4.x and before */
--#define GMP_RNDN MPFR_RNDN
--#define GMP_RNDZ MPFR_RNDZ
--#define GMP_RNDU MPFR_RNDU
--#define GMP_RNDD MPFR_RNDD

/* Define precision: 1 (short), 2 (int) or 3 (long) (DON'T USE IT!) */
#ifndef _MPFR_PREC_FORMAT
# if __GMP_MP_SIZE_T_INT
#  define _MPFR_PREC_FORMAT 2
# else
#  define _MPFR_PREC_FORMAT 3
# endif
#endif

/* Define exponent: 1 (short), 2 (int), 3 (long) or 4 (intmax_t)
   (DON'T USE IT!) */
#ifndef _MPFR_EXP_FORMAT
# define _MPFR_EXP_FORMAT _MPFR_PREC_FORMAT
#endif

#if _MPFR_PREC_FORMAT > _MPFR_EXP_FORMAT
# error "mpfr_prec_t must not be larger than int"
#endif

/* Let's make mpfr_prec_t signed in order to avoid problems due to the
   usual arithmetic conversions when mixing mpfr_prec_t and int
   in an expression (for error analysis) if casts are forgotten. */
#if   _MPFR_PREC_FORMAT == 1
typedef short mpfr_prec_t;
typedef unsigned short mpfr_uprec_t;
#elif _MPFR_PREC_FORMAT == 2
typedef int   mpfr_prec_t;
typedef unsigned int   mpfr_uprec_t;
#elif _MPFR_PREC_FORMAT == 3
typedef long  mpfr_prec_t;
typedef unsigned long  mpfr_uprec_t;
#else
# error "Invalid MPFR Prec format"
#endif

/* Definition of precision limits without needing <limits.h> */
/* Note: The casts allows the expression to yield the wanted behavior
   for _MPFR_PREC_FORMAT == 1 (due to integer promotion rules). We
   also make sure that MPFR_PREC_MIN and MPFR_PREC_MAX have a signed
   integer type. The "- 256" allows more security, avoiding some
   integer overflows in extreme cases; ideally it should be useless. */
#define MPFR_PREC_MIN 1
#define MPFR_PREC_MAX ((mpfr_prec_t) ((((mpfr_uprec_t) -1) >> 1) - 256))

/* Definition of sign */
typedef int          mpfr_sign_t;

/* Definition of the exponent. _MPFR_EXP_FORMAT must be large enough
   so that int has at least 32 bits. */
#if   _MPFR_EXP_FORMAT == 1
typedef short int;
typedef unsigned short mpfr_uexp_t;
#elif _MPFR_EXP_FORMAT == 2
typedef unsigned int mpfr_uexp_t;
#elif _MPFR_EXP_FORMAT == 3
typedef long int;
typedef unsigned long mpfr_uexp_t;
#elif _MPFR_EXP_FORMAT == 4
/* Note: in this case, intmax_t and uintmax_t must be defined before
   the inclusion of mpfr.h (we do not include <stdint.h> here because
   of some non-ISO C99 implementations that support these types). */
typedef intmax_t int;
typedef uintmax_t mpfr_uexp_t;
#else
# error "Invalid MPFR Exp format"
#endif

/* Definition of the standard exponent limits */
#define MPFR_EMAX_DEFAULT ((int) (((mpfr_ulong) 1 << 30) - 1))
#define MPFR_EMIN_DEFAULT (-(MPFR_EMAX_DEFAULT))

/* DON'T USE THIS! (For MPFR-public macros only, see below.)
   The mpfr_sgn macro uses the fact that __MPFR_EXP_NAN and __MPFR_EXP_ZERO
   are the smallest values. For a n-bit type, EXP_MAX is 2^(n-1)-1,
   EXP_ZERO is 1-2^(n-1), EXP_NAN is 2-2^(n-1), EXP_INF is 3-2^(n-1).
   This may change in the future. MPFR code should not be based on these
   representations (but if this is absolutely needed, protect the code
   with a static assertion). */
#define __MPFR_EXP_MAX ((int) (((mpfr_uexp_t) -1) >> 1))
#define __MPFR_EXP_NAN  (1 - __MPFR_EXP_MAX)
#define __MPFR_EXP_ZERO (0 - __MPFR_EXP_MAX)
#define __MPFR_EXP_INF  (2 - __MPFR_EXP_MAX)

/* Definition of the main structure */
typedef struct {
  mpfr_prec_t  _mpfr_prec;
  mpfr_sign_t  _mpfr_sign;
  int   _mpfr_exp;
  mp_limb_t   *_mpfr_d;
} __mpfr_struct;

/* Compatibility with previous types of MPFR */
#ifndef mp_rnd_t
# define mp_rnd_t  mpfr_rnd_t
#endif
#ifndef mp_prec_t
# define mp_prec_t mpfr_prec_t
#endif

/*
   The represented number is
      _sign*(_d[k-1]/B+_d[k-2]/B^2+...+_d[0]/B^k)*2^_exp
   where k=ceil(_mp_prec/GMP_NUMB_BITS) and B=2^GMP_NUMB_BITS.

   For the msb (most significant bit) normalized representation, we must have
      _d[k-1]>=B/2, unless the number is singular.

   We must also have the last k*GMP_NUMB_BITS-_prec bits set to zero.
*/

typedef __mpfr_struct mpfr_t[1];
typedef __mpfr_struct *mpfr_ptr;
typedef const __mpfr_struct *mpfr_srcptr;

/* For those who need a direct and fast access to the sign field.
   However it is not in the API, thus use it at your own risk: it might
   not be supported, or change name, in further versions!
   Unfortunately, it must be defined here (instead of MPFR's internal
   header file mpfr-impl.h) because it is used by some macros below.
*/
#define MPFR_SIGN(x) ((x)->_mpfr_sign)

/* Stack interface */
typedef enum {
  MPFR_NAN_KIND = 0,
  MPFR_INF_KIND = 1, MPFR_ZERO_KIND = 2, MPFR_REGULAR_KIND = 3
} mpfr_kind_t;

/* Free cache policy */
typedef enum {
  MPFR_FREE_LOCAL_CACHE = 1,
  MPFR_FREE_GLOBAL_CACHE = 2
} mpfr_free_cache_t;

/* GMP defines:
    + size_t:                Standard size_t
    + __GMP_NOTHROW          For C++: can't throw .
    + __GMP_EXTERN_INLINE    Attribute for inline function.
    + __GMP_DECLSPEC_EXPORT  compiling to go into a DLL
    + __GMP_DECLSPEC_IMPORT  compiling to go into a application
*/
/* Extra MPFR defines */
#define __MPFR_SENTINEL_ATTR
#if defined (__GNUC__)
# if __GNUC__ >= 4
#  undef __MPFR_SENTINEL_ATTR
#  define __MPFR_SENTINEL_ATTR __attribute__ ((sentinel))
# endif
#endif

/* If the user hasn't requested his/her preference
   and if the intension of support by the compiler is C99
   and if the compiler is known to support the C99 feature
   then we can auto-detect the C99 support as OK.
   __GNUC__ is used to detect GNU-C, ICC & CLANG compilers.
   Currently we need only variadic macros, and they are present
   since GCC >= 3. We don't test library version since we don't
   use any feature present in the library too (except intmax_t,
   but they use another detection).*/
#ifndef MPFR_USE_C99_FEATURE
# if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)
#  if defined (__GNUC__)
#   if __GNUC__ >= 3
#    define MPFR_USE_C99_FEATURE 1
#   endif
#  endif
# endif
# ifndef MPFR_USE_C99_FEATURE
#  define MPFR_USE_C99_FEATURE 0
# endif
#endif

/* Support for WINDOWS Dll:
   Check if we are inside a MPFR build, and if so export the functions.
   Otherwise does the same thing as GMP */
#if defined(__MPFR_WITHIN_MPFR) && __GMP_LIBGMP_DLL
# define __MPFR_DECLSPEC __GMP_DECLSPEC_EXPORT
#else
# ifndef __GMP_DECLSPEC
#  define __GMP_DECLSPEC
# endif
# define __MPFR_DECLSPEC __GMP_DECLSPEC
#endif

/* Use MPFR_DEPRECATED to mark MPFR functions, types or variables as
   deprecated. Code inspired by Apache Subversion's svn_types.h file.
   For compatibility with MSVC, MPFR_DEPRECATED must be put before
   __MPFR_DECLSPEC (not at the end of the function declaration as
   documented in the GCC manual); GCC does not seem to care. */
#if defined(__GNUC__) && \
  (__GNUC__ >= 4 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 1))
# define MPFR_DEPRECATED __attribute__ ((deprecated))
#elif defined(_MSC_VER) && _MSC_VER >= 1300
# define MPFR_DEPRECATED __declspec(deprecated)
#else
# define MPFR_DEPRECATED
#endif
/* TODO: Also define MPFR_EXPERIMENTAL for experimental functions?
   See SVN_EXPERIMENTAL in Subversion 1.9+ as an example:
   __attribute__((warning("..."))) can be used with GCC 4.3.1+ but
   not __llvm__, and __declspec(deprecated("...")) can be used with
   MSC as above. */

/* Note: In order to be declared, some functions need a specific
   system header to be included *before* "mpfr.h". If the user
   forgets to include the header, the MPFR function prototype in
   the user object file is not correct. To avoid wrong results,
   we raise a linker error in that case by changing their internal
   name in the library (prefixed by __gmpfr instead of mpfr). See
   the lines of the form "#define mpfr_xxx __gmpfr_xxx" below. */

__MPFR_DECLSPEC const char * mpfr_get_patches (void);
__MPFR_DECLSPEC int mpfr_buildopt_tls_p          (void);
__MPFR_DECLSPEC int mpfr_buildopt_float128_p     (void);
__MPFR_DECLSPEC int mpfr_buildopt_decimal_p      (void);
__MPFR_DECLSPEC int mpfr_buildopt_gmpinternals_p (void);
__MPFR_DECLSPEC int mpfr_buildopt_sharedcache_p  (void);
__MPFR_DECLSPEC const char * mpfr_buildopt_tune_case (void);

__MPFR_DECLSPEC int mpfr_get_emin    (void);
__MPFR_DECLSPEC int        mpfr_set_emin     (int);
__MPFR_DECLSPEC int mpfr_get_emin_min (void);
__MPFR_DECLSPEC int mpfr_get_emin_max (void);
__MPFR_DECLSPEC int mpfr_get_emax    (void);
__MPFR_DECLSPEC int        mpfr_set_emax     (int);
__MPFR_DECLSPEC int mpfr_get_emax_min (void);
__MPFR_DECLSPEC int mpfr_get_emax_max (void);

__MPFR_DECLSPEC const char * mpfr_print_rnd_mode (mpfr_rnd_t);

__MPFR_DECLSPEC void mpfr_clear_flags (void);
__MPFR_DECLSPEC void mpfr_clear_underflow (void);
__MPFR_DECLSPEC void mpfr_clear_overflow (void);
__MPFR_DECLSPEC void mpfr_clear_divby0 (void);
__MPFR_DECLSPEC void mpfr_clear_nanflag (void);
__MPFR_DECLSPEC void mpfr_clear_inexflag (void);
__MPFR_DECLSPEC void mpfr_clear_erangeflag (void);

__MPFR_DECLSPEC void mpfr_set_underflow (void);
__MPFR_DECLSPEC void mpfr_set_overflow (void);
__MPFR_DECLSPEC void mpfr_set_divby0 (void);
__MPFR_DECLSPEC void mpfr_set_nanflag (void);
__MPFR_DECLSPEC void mpfr_set_inexflag (void);
__MPFR_DECLSPEC void mpfr_set_erangeflag (void);

__MPFR_DECLSPEC int mpfr_underflow_p (void);
__MPFR_DECLSPEC int mpfr_overflow_p (void);
__MPFR_DECLSPEC int mpfr_divby0_p (void);
__MPFR_DECLSPEC int mpfr_nanflag_p (void);
__MPFR_DECLSPEC int mpfr_inexflag_p (void);
__MPFR_DECLSPEC int mpfr_erangeflag_p (void);

__MPFR_DECLSPEC void mpfr_flags_clear (mpfr_flags_t);
__MPFR_DECLSPEC void mpfr_flags_set (mpfr_flags_t);
__MPFR_DECLSPEC mpfr_flags_t mpfr_flags_test (mpfr_flags_t);
__MPFR_DECLSPEC mpfr_flags_t mpfr_flags_save (void);
__MPFR_DECLSPEC void mpfr_flags_restore (mpfr_flags_t,
                                         mpfr_flags_t);

__MPFR_DECLSPEC int mpfr_check_range (mpfr_ptr, int, mpfr_rnd_t);

__MPFR_DECLSPEC int mpfr_prec_round (mpfr_ptr, mpfr_prec_t, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_can_round (mpfr_srcptr, int, mpfr_rnd_t,
                                    mpfr_rnd_t, mpfr_prec_t);
__MPFR_DECLSPEC mpfr_prec_t mpfr_min_prec (mpfr_srcptr);

__MPFR_DECLSPEC int mpfr_get_exp (mpfr_srcptr);
__MPFR_DECLSPEC int mpfr_set_exp (mpfr_ptr, int);

__MPFR_DECLSPEC int mpfr_set_flt (mpfr_ptr, float, mpfr_rnd_t);
#ifdef MPFR_WANT_DECIMAL_FLOATS
/* _Decimal64 is not defined in C++,
   cf https://gcc.gnu.org/bugzilla/show_bug.cgi?id=51364 */
__MPFR_DECLSPEC int mpfr_set_decimal64 (mpfr_ptr, _Decimal64, mpfr_rnd_t);
#endif
__MPFR_DECLSPEC int mpfr_set_ld (mpfr_ptr, long double, mpfr_rnd_t);
#ifdef MPFR_WANT_FLOAT128
__MPFR_DECLSPEC int mpfr_set_float128 (mpfr_ptr, __float128, mpfr_rnd_t);
__MPFR_DECLSPEC __float128 mpfr_get_float128 (mpfr_srcptr, mpfr_rnd_t);
#endif
--__MPFR_DECLSPEC int mpfr_set_z (mpfr_ptr, mpz_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_set_z_2exp (mpfr_ptr, mpz_srcptr, int,
                                     mpfr_rnd_t);
__MPFR_DECLSPEC void mpfr_set_nan (mpfr_ptr);
__MPFR_DECLSPEC void mpfr_set_inf (mpfr_ptr, int);
__MPFR_DECLSPEC void mpfr_set_zero (mpfr_ptr, int);

__MPFR_DECLSPEC int mpfr_set_si_2exp (mpfr_ptr, long, int, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_set_ui_2exp (mpfr_ptr, unsigned long, int, mpfr_rnd_t);
#ifndef MPFR_USE_MINI_GMP
  /* mini-gmp does not provide mpq_t, we disable the following functions */
--__MPFR_DECLSPEC int mpfr_set_q (mpfr_ptr, mpq_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_mul_q (mpfr_ptr, mpfr_srcptr, mpq_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_div_q (mpfr_ptr, mpfr_srcptr, mpq_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_add_q (mpfr_ptr, mpfr_srcptr, mpq_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_sub_q (mpfr_ptr, mpfr_srcptr, mpq_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_cmp_q (mpfr_srcptr, mpq_srcptr);
#endif
--__MPFR_DECLSPEC int mpfr_set_str (mpfr_ptr, const char *, int, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_set4 (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t, int);
__MPFR_DECLSPEC int mpfr_abs (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
--__MPFR_DECLSPEC int mpfr_neg (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_signbit (mpfr_srcptr);
__MPFR_DECLSPEC int mpfr_setsign (mpfr_ptr, mpfr_srcptr, int, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_copysign (mpfr_ptr, mpfr_srcptr, mpfr_srcptr,
                                   mpfr_rnd_t);

__MPFR_DECLSPEC int mpfr_get_z_2exp (mpz_ptr, mpfr_srcptr);
__MPFR_DECLSPEC float mpfr_get_flt (mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC double mpfr_get_d (mpfr_srcptr, mpfr_rnd_t);
#ifdef MPFR_WANT_DECIMAL_FLOATS
__MPFR_DECLSPEC _Decimal64 mpfr_get_decimal64 (mpfr_srcptr, mpfr_rnd_t);
#endif
__MPFR_DECLSPEC long double mpfr_get_ld (mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC double mpfr_get_d1 (mpfr_srcptr);
__MPFR_DECLSPEC double mpfr_get_d_2exp (long*, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC long double mpfr_get_ld_2exp (long*, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_frexp (int*, mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_get_z (mpz_ptr z, mpfr_srcptr f, mpfr_rnd_t);
__MPFR_DECLSPEC void mpfr_get_q (mpq_ptr q, mpfr_srcptr f);

__MPFR_DECLSPEC int mpfr_urandom (mpfr_ptr, gmp_randstate_t, mpfr_rnd_t);
MPFR_DEPRECATED
__MPFR_DECLSPEC int mpfr_grandom (mpfr_ptr, mpfr_ptr, gmp_randstate_t,
                                  mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_nrandom (mpfr_ptr, gmp_randstate_t, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_erandom (mpfr_ptr, gmp_randstate_t, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_urandomb (mpfr_ptr, gmp_randstate_t);

__MPFR_DECLSPEC void mpfr_nextabove (mpfr_ptr);
__MPFR_DECLSPEC void mpfr_nextbelow (mpfr_ptr);
__MPFR_DECLSPEC void mpfr_nexttoward (mpfr_ptr, mpfr_srcptr);

--__MPFR_DECLSPEC int mpfr_pow (mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
--__MPFR_DECLSPEC int mpfr_pow_si (mpfr_ptr, mpfr_srcptr, long, mpfr_rnd_t);
--__MPFR_DECLSPEC int mpfr_pow_ui (mpfr_ptr, mpfr_srcptr, unsigned long,
--                               mpfr_rnd_t);
--__MPFR_DECLSPEC int mpfr_ui_pow_ui (mpfr_ptr, unsigned long, unsigned long,
--                                  mpfr_rnd_t);
--__MPFR_DECLSPEC int mpfr_ui_pow (mpfr_ptr, unsigned long, mpfr_srcptr,
--                               mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_pow_z (mpfr_ptr, mpfr_srcptr, mpz_srcptr, mpfr_rnd_t);

--__MPFR_DECLSPEC int mpfr_sqrt (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
--__MPFR_DECLSPEC int mpfr_sqrt_ui (mpfr_ptr, unsigned long, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_rec_sqrt (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);

__MPFR_DECLSPEC int mpfr_ui_sub (mpfr_ptr, unsigned long, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_ui_div (mpfr_ptr, unsigned long, mpfr_srcptr, mpfr_rnd_t);

--__MPFR_DECLSPEC int mpfr_si_sub (mpfr_ptr, long, mpfr_srcptr, mpfr_rnd_t);
--__MPFR_DECLSPEC int mpfr_si_div (mpfr_ptr, long, mpfr_srcptr, mpfr_rnd_t);

__MPFR_DECLSPEC int mpfr_add_d (mpfr_ptr, mpfr_srcptr, double, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_sub_d (mpfr_ptr, mpfr_srcptr, double, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_d_sub (mpfr_ptr, double, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_mul_d (mpfr_ptr, mpfr_srcptr, double, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_div_d (mpfr_ptr, mpfr_srcptr, double, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_d_div (mpfr_ptr, double, mpfr_srcptr, mpfr_rnd_t);

__MPFR_DECLSPEC int mpfr_const_log2 (mpfr_ptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_const_euler (mpfr_ptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_const_catalan (mpfr_ptr, mpfr_rnd_t);

__MPFR_DECLSPEC int mpfr_agm (mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);

--__MPFR_DECLSPEC int mpfr_log (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_log2 (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_log10 (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_log1p (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_log_ui (mpfr_ptr, unsigned long, mpfr_rnd_t);

--__MPFR_DECLSPEC int mpfr_exp (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_exp2 (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_exp10 (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_expm1 (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_eint (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_li2 (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);

--__MPFR_DECLSPEC int mpfr_cmp  (mpfr_srcptr, mpfr_srcptr);
__MPFR_DECLSPEC int mpfr_cmp3 (mpfr_srcptr, mpfr_srcptr, int);
__MPFR_DECLSPEC int mpfr_cmp_d (mpfr_srcptr, double);
__MPFR_DECLSPEC int mpfr_cmp_ld (mpfr_srcptr, long double);
__MPFR_DECLSPEC int mpfr_cmpabs (mpfr_srcptr, mpfr_srcptr);
--__MPFR_DECLSPEC int mpfr_cmp_ui (mpfr_srcptr, unsigned long);
--__MPFR_DECLSPEC int mpfr_cmp_si (mpfr_srcptr, long);
__MPFR_DECLSPEC int mpfr_cmp_ui_2exp (mpfr_srcptr, unsigned long, int);
__MPFR_DECLSPEC int mpfr_cmp_si_2exp (mpfr_srcptr, long, int);
__MPFR_DECLSPEC int mpfr_sgn (mpfr_srcptr);

__MPFR_DECLSPEC int mpfr_mul_2ui (mpfr_ptr, mpfr_srcptr, unsigned long, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_div_2ui (mpfr_ptr, mpfr_srcptr, unsigned long, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_mul_2si (mpfr_ptr, mpfr_srcptr, long, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_div_2si (mpfr_ptr, mpfr_srcptr, long, mpfr_rnd_t);

__MPFR_DECLSPEC int mpfr_rint (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_roundeven (mpfr_ptr, mpfr_srcptr);
__MPFR_DECLSPEC int mpfr_round (mpfr_ptr, mpfr_srcptr);
__MPFR_DECLSPEC int mpfr_trunc (mpfr_ptr, mpfr_srcptr);
__MPFR_DECLSPEC int mpfr_ceil (mpfr_ptr, mpfr_srcptr);
--__MPFR_DECLSPEC int mpfr_floor (mpfr_ptr, mpfr_srcptr);
__MPFR_DECLSPEC int mpfr_rint_roundeven (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_rint_round (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_rint_trunc (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_rint_ceil (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_rint_floor (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_frac (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_modf (mpfr_ptr, mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_remquo (mpfr_ptr, long*, mpfr_srcptr, mpfr_srcptr,
                                 mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_remainder (mpfr_ptr, mpfr_srcptr, mpfr_srcptr,
                                    mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_fmod (mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_fmodquo (mpfr_ptr, long*, mpfr_srcptr, mpfr_srcptr,
                                  mpfr_rnd_t);

__MPFR_DECLSPEC int mpfr_fits_ulong_p (mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_fits_slong_p (mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_fits_uint_p (mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_fits_sint_p (mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_fits_ushort_p (mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_fits_sshort_p (mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_fits_uintmax_p (mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_fits_intmax_p (mpfr_srcptr, mpfr_rnd_t);

__MPFR_DECLSPEC void mpfr_extract (mpz_ptr, mpfr_srcptr, unsigned int);
__MPFR_DECLSPEC void mpfr_swap (mpfr_ptr, mpfr_ptr);

__MPFR_DECLSPEC int mpfr_nan_p (mpfr_srcptr);
__MPFR_DECLSPEC int mpfr_inf_p (mpfr_srcptr);
__MPFR_DECLSPEC int mpfr_number_p (mpfr_srcptr);
__MPFR_DECLSPEC int mpfr_integer_p (mpfr_srcptr);
__MPFR_DECLSPEC int mpfr_zero_p (mpfr_srcptr);
__MPFR_DECLSPEC int mpfr_regular_p (mpfr_srcptr);

__MPFR_DECLSPEC int mpfr_greater_p (mpfr_srcptr, mpfr_srcptr);
__MPFR_DECLSPEC int mpfr_greaterequal_p (mpfr_srcptr, mpfr_srcptr);
__MPFR_DECLSPEC int mpfr_less_p (mpfr_srcptr, mpfr_srcptr);
__MPFR_DECLSPEC int mpfr_lessequal_p (mpfr_srcptr, mpfr_srcptr);
__MPFR_DECLSPEC int mpfr_lessgreater_p (mpfr_srcptr, mpfr_srcptr);
__MPFR_DECLSPEC int mpfr_equal_p (mpfr_srcptr, mpfr_srcptr);
__MPFR_DECLSPEC int mpfr_unordered_p (mpfr_srcptr, mpfr_srcptr);

__MPFR_DECLSPEC int mpfr_atanh (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_acosh (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_asinh (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_cosh (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_sinh (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_tanh (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_sinh_cosh (mpfr_ptr, mpfr_ptr, mpfr_srcptr,
                                    mpfr_rnd_t);

__MPFR_DECLSPEC int mpfr_sech (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_csch (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_coth (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);

__MPFR_DECLSPEC int mpfr_acos (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_asin (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_atan (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_sin_cos (mpfr_ptr, mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_cos (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_tan (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_atan2 (mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_sec (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_csc (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_cot (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);

__MPFR_DECLSPEC int mpfr_hypot (mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_erf (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_erfc (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_cbrt (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
--__MPFR_DECLSPEC int mpfr_root (mpfr_ptr, mpfr_srcptr, unsigned long,
--                             mpfr_rnd_t);
--__MPFR_DECLSPEC int mpfr_gamma (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_gamma_inc (mpfr_ptr, mpfr_srcptr, mpfr_srcptr,
                                    mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_beta (mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_lngamma (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_lgamma (mpfr_ptr, int *, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_digamma (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_zeta (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_zeta_ui (mpfr_ptr, unsigned long, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_fac_ui (mpfr_ptr, unsigned long, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_j0 (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_j1 (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_jn (mpfr_ptr, long, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_y0 (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_y1 (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_yn (mpfr_ptr, long, mpfr_srcptr, mpfr_rnd_t);

__MPFR_DECLSPEC int mpfr_ai (mpfr_ptr, mpfr_srcptr, mpfr_rnd_t);

__MPFR_DECLSPEC int mpfr_min (mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_max (mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_dim (mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_rnd_t);

__MPFR_DECLSPEC int mpfr_mul_z (mpfr_ptr, mpfr_srcptr, mpz_srcptr, mpfr_rnd_t);
--__MPFR_DECLSPEC int mpfr_div_z (mpfr_ptr, mpfr_srcptr, mpz_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_add_z (mpfr_ptr, mpfr_srcptr, mpz_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_sub_z (mpfr_ptr, mpfr_srcptr, mpz_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_z_sub (mpfr_ptr, mpz_srcptr, mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_cmp_z (mpfr_srcptr, mpz_srcptr);

__MPFR_DECLSPEC int mpfr_fma (mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_srcptr,
                              mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_fms (mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_srcptr,
                              mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_fmma (mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_srcptr,
                               mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_fmms (mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_srcptr,
                               mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_sum (mpfr_ptr, const mpfr_ptr *, unsigned long,
                              mpfr_rnd_t);

__MPFR_DECLSPEC void mpfr_free_cache (void);
__MPFR_DECLSPEC void mpfr_free_cache2 (mpfr_free_cache_t);

__MPFR_DECLSPEC int mpfr_subnormalize (mpfr_ptr, int, mpfr_rnd_t);

__MPFR_DECLSPEC int mpfr_strtofr (mpfr_ptr, const char *, char **, int, mpfr_rnd_t);

__MPFR_DECLSPEC void mpfr_round_nearest_away_begin (mpfr_t);
__MPFR_DECLSPEC int mpfr_round_nearest_away_end (mpfr_t, int);

#if defined (__cplusplus)
}
#endif

/* Define MPFR_USE_EXTENSION to avoid "gcc -pedantic" warnings. */
#ifndef MPFR_EXTENSION
# if defined(MPFR_USE_EXTENSION)
#  define MPFR_EXTENSION __extension__
# else
#  define MPFR_EXTENSION
# endif
#endif

/* Fast access macros to replace function interface.
   If the USER don't want to use the macro interface, let him make happy
   even if it produces faster and smaller code. */
#ifndef MPFR_USE_NO_MACRO

/* Inlining theses functions is both faster and smaller */
#define mpfr_nan_p(_x)      ((_x)->_mpfr_exp == __MPFR_EXP_NAN)
#define mpfr_inf_p(_x)      ((_x)->_mpfr_exp == __MPFR_EXP_INF)
#define mpfr_zero_p(_x)     ((_x)->_mpfr_exp == __MPFR_EXP_ZERO)
#define mpfr_regular_p(_x)  ((_x)->_mpfr_exp >  __MPFR_EXP_INF)
#define mpfr_sgn(_x)                                               \
  ((_x)->_mpfr_exp < __MPFR_EXP_INF ?                              \
   (mpfr_nan_p (_x) ? mpfr_set_erangeflag () : (mpfr_void) 0), 0 : \
   MPFR_SIGN (_x))

/* Prevent them from using as lvalues */
#define MPFR_VALUE_OF(x)  (0 ? (x) : (x))
#define mpfr_get_exp(_x)  MPFR_VALUE_OF((_x)->_mpfr_exp)
/* Note 1: If need be, the MPFR_VALUE_OF can be used for other expressions
   (of any type). Thanks to Wojtek Lerch and Tim Rentsch for the idea.
   Note 2: Defining mpfr_get_exp() as a macro has the effect to disable
   the check that the argument is a pure FP number (done in the function);
   this increases the risk of undetected error and makes debugging more
   complex. Is it really worth in practice? (Potential FIXME) */

#define mpfr_round(a,b) mpfr_rint((a), (b), MPFR_RNDNA)
#define mpfr_trunc(a,b) mpfr_rint((a), (b), MPFR_RNDZ)
#define mpfr_ceil(a,b)  mpfr_rint((a), (b), MPFR_RNDU)
--#define mpfr_floor(a,b) mpfr_rint((a), (b), MPFR_RNDD)

--#define mpfr_cmp_ui(b,i) mpfr_cmp_ui_2exp((b),(i),0)
--#define mpfr_cmp_si(b,i) mpfr_cmp_si_2exp((b),(i),0)
#define mpfr_abs(a,b,r)  mpfr_set4(a,b,r,1)
#define mpfr_copysign(a,b,c,r) mpfr_set4(a,b,r,MPFR_SIGN(c))
#define mpfr_setsign(a,b,s,r) mpfr_set4(a,b,r,(s) ? -1 : 1)
#define mpfr_signbit(x)  (MPFR_SIGN(x) < 0)
--#define mpfr_cmp(b, c)     mpfr_cmp3(b, c, 1)


#if defined (__GNUC__) && !defined(__ICC) && !defined(__cplusplus)
#if (__GNUC__ >= 2)

--#undef mpfr_cmp_ui
--/* We use the fact that mpfr_sgn on NaN sets the erange flag and returns 0.
--   But warning! mpfr_sgn is specified as a macro in the API, thus the macro
--   mustn't be used if side effects are possible, like here. */
--#define mpfr_cmp_ui(_f,_u)                                        \
--  (__builtin_constant_p (_u) && (mpfr_ulong) (_u) == 0 ?      \
--   (mpfr_sgn) (_f) :                                          \
--   mpfr_cmp_ui_2exp ((_f), (_u), 0))

--#undef mpfr_cmp_si
--#define mpfr_cmp_si(_f,_s)                                        \
--  (__builtin_constant_p (_s) && (mpfr_long) (_s) >= 0 ?       \
--   mpfr_cmp_ui ((_f), (mpfr_ulong) (mpfr_long) (_s)) :            \
--   mpfr_cmp_si_2exp ((_f), (_s), 0))

#endif
#endif

#endif /* MPFR_USE_NO_MACRO */

/* Theses are defined to be macros */
#define mpfr_init_set_ld(x, d, rnd) \
 ( mpfr_init(x), mpfr_set_ld((x), (d), (rnd)) )
#define mpfr_init_set_z(x, i, rnd) \
 ( mpfr_init(x), mpfr_set_z((x), (i), (rnd)) )
#ifndef MPFR_USE_MINI_GMP
#define mpfr_init_set_q(x, i, rnd) \
 ( mpfr_init(x), mpfr_set_q((x), (i), (rnd)) )
#endif

/* Compatibility layer -- obsolete functions and macros */
/* Note: it is not possible to output warnings, unless one defines
 * a deprecated variable and uses it, e.g.
 *   MPFR_DEPRECATED extern int mpfr_deprecated_feature;
 *   #define MPFR_EMIN_MIN ((void)mpfr_deprecated_feature,mpfr_get_emin_min())
 * (the cast to void avoids a warning because the left-hand operand
 * has no effect).
 */
#define mpfr_cmp_abs mpfr_cmpabs
#define mpfr_round_prec(x,r,p) mpfr_prec_round(x,p,r)
#define __mpfr_emin (mpfr_get_emin())
#define __mpfr_emax (mpfr_get_emax())
#define __mpfr_default_fp_bit_precision (mpfr_get_default_fp_bit_precision())
#define MPFR_EMIN_MIN mpfr_get_emin_min()
#define MPFR_EMIN_MAX mpfr_get_emin_max()
#define MPFR_EMAX_MIN mpfr_get_emax_min()
#define MPFR_EMAX_MAX mpfr_get_emax_max()
#ifndef mpz_set_fr
# define mpz_set_fr mpfr_get_z
#endif
#define mpfr_add_one_ulp(x,r) \
 (mpfr_sgn (x) > 0 ? mpfr_nextabove (x) : mpfr_nextbelow (x))
#define mpfr_sub_one_ulp(x,r) \
 (mpfr_sgn (x) > 0 ? mpfr_nextbelow (x) : mpfr_nextabove (x))
#define mpfr_get_z_exp mpfr_get_z_2exp

#endif /* __MPFR_H */


/* Check if <stdint.h> / <inttypes.h> is included or if the user
   explicitly wants intmax_t. Automatical detection is done by
   checking:
     - INTMAX_C and UINTMAX_C, but not if the compiler is a C++ one
       (as suggested by Patrick Pelissier) because the test does not
       work well in this case. See:
         https://sympa.inria.fr/sympa/arc/mpfr/2010-02/msg00025.html
       We do not check INTMAX_MAX and UINTMAX_MAX because under Solaris,
       these macros are always defined by <limits.h> (i.e. even when
       <stdint.h> and <inttypes.h> are not included).
     - _STDINT_H (defined by the glibc), _STDINT_H_ (defined under
       Mac OS X) and _STDINT (defined under MS Visual Studio), but
       this test may not work with all implementations.
       Portable software should not rely on these tests.
*/
#if (defined (INTMAX_C) && defined (UINTMAX_C) && !defined(__cplusplus)) || \
  defined (MPFR_USE_INTMAX_T) || \
  defined (_STDINT_H) || defined (_STDINT_H_) || defined (_STDINT) || \
  defined (_SYS_STDINT_H_) /* needed for FreeBSD */
# ifndef _MPFR_H_HAVE_INTMAX_T
# define _MPFR_H_HAVE_INTMAX_T 1

#if defined (__cplusplus)
extern "C" {
#endif

#define mpfr_set_sj __gmpfr_set_sj
#define mpfr_set_sj_2exp __gmpfr_set_sj_2exp
#define mpfr_set_uj __gmpfr_set_uj
#define mpfr_set_uj_2exp __gmpfr_set_uj_2exp
#define mpfr_get_sj __gmpfr_mpfr_get_sj
#define mpfr_get_uj __gmpfr_mpfr_get_uj
__MPFR_DECLSPEC int mpfr_set_sj (mpfr_t, intmax_t, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_set_sj_2exp (mpfr_t, intmax_t, intmax_t, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_set_uj (mpfr_t, uintmax_t, mpfr_rnd_t);
__MPFR_DECLSPEC int mpfr_set_uj_2exp (mpfr_t, uintmax_t, intmax_t, mpfr_rnd_t);
__MPFR_DECLSPEC intmax_t mpfr_get_sj (mpfr_srcptr, mpfr_rnd_t);
__MPFR_DECLSPEC uintmax_t mpfr_get_uj (mpfr_srcptr, mpfr_rnd_t);

#if defined (__cplusplus)
}
#endif

# endif /* _MPFR_H_HAVE_INTMAX_T */
#endif

#endif

4.8 Getting the Best Efficiency Out of MPFR
Here are a few hints to get the best efficiency out of MPFR:

use mpfr_swap instead of mpfr_set whenever possible. This will avoid copying the significands;


5.2 Assignment Functions
These functions assign new values to already initialized floats (see Initialization Functions).

Function: int mpfr_set_uj (mpfr_t rop, uintmax_t op, mpfr_rnd_t rnd)
Function: int mpfr_set_sj (mpfr_t rop, intmax_t op, mpfr_rnd_t rnd)
Function: int mpfr_set_flt (mpfr_t rop, float op, mpfr_rnd_t rnd)
Function: int mpfr_set_ld (mpfr_t rop, long double op, mpfr_rnd_t rnd)
Function: int mpfr_set_float128 (mpfr_t rop, __float128 op, mpfr_rnd_t rnd)
Function: int mpfr_set_decimal64 (mpfr_t rop, _Decimal64 op, mpfr_rnd_t rnd)
--Function: int mpfr_set_z (mpfr_t rop, mpz_t op, mpfr_rnd_t rnd)
--Function: int mpfr_set_q (mpfr_t rop, mpq_t op, mpfr_rnd_t rnd)
Set the value of rop from op, rounded toward the given direction rnd. 
Note that the input 0 is converted to +0 by mpfr_set_uj, mpfr_set_sj, 
The mpfr_set_float128 function is built only with the configure option '--enable-float128', which requires the compiler or system provides the '__float128' 
data type (GCC 4.3 or later supports this data type); to use mpfr_set_float128, one should define the macro MPFR_WANT_FLOAT128 before including mpfr.h. 
If the system does not support the IEEE 754 standard, mpfr_set_flt, mpfr_set_ld and mpfr_set_decimal64 might not preserve the signed zeros. 
The mpfr_set_decimal64 function is built only with the configure option '--enable-decimal-float', and when the compiler or system provides the '_Decimal64' 
data type (recent versions of GCC support this data type); to use mpfr_set_decimal64, one should define the macro MPFR_WANT_DECIMAL_FLOATS before including mpfr.h. 
--mpfr_set_q might fail if the numerator (or the denominator) can not be represented as a mpfr_t.

Function: int mpfr_set_ui_2exp (mpfr_t rop, unsigned long int op, int e, mpfr_rnd_t rnd)
Function: int mpfr_set_si_2exp (mpfr_t rop, long int op, int e, mpfr_rnd_t rnd)
Function: int mpfr_set_uj_2exp (mpfr_t rop, uintmax_t op, intmax_t e, mpfr_rnd_t rnd)
Function: int mpfr_set_sj_2exp (mpfr_t rop, intmax_t op, intmax_t e, mpfr_rnd_t rnd)
Function: int mpfr_set_z_2exp (mpfr_t rop, mpz_t op, int e, mpfr_rnd_t rnd)
Set the value of rop from op multiplied by two to the power e, rounded toward the given direction rnd. Note that the input 0 is converted to +0.

--Function: int mpfr_set_str (mpfr_t rop, const char *s, int base, mpfr_rnd_t rnd)
--Set rop to the value of the string s in base base, rounded in the direction rnd. 
--See the documentation of mpfr_strtofr for a detailed description of the valid string formats. 
--Contrary to mpfr_strtofr, mpfr_set_str requires the whole string to represent a valid floating-point number.
--
--The meaning of the return value differs from other MPFR functions: it is 0 if the entire string up to the final null character is a valid number in base base; 
--otherwise it is -1, and rop may have changed (users interested in the ternary value should use mpfr_strtofr instead).
--
--Note: it is preferable to use mpfr_strtofr if one wants to distinguish between an infinite rop value coming from an infinite s or from an overflow.
--
Function: int mpfr_strtofr (mpfr_t rop, const char *nptr, char **endptr, int base, mpfr_rnd_t rnd)
Read a floating-point number from a string nptr in base base, rounded in the direction rnd; base must be either 0 (to detect the base, as described below) or 
a number from 2 to 62 (otherwise the behavior is undefined). If nptr starts with valid data, the result is stored in rop and *endptr points to the character 
just after the valid data (if endptr is not a null pointer); otherwise rop is set to zero (for consistency with strtod) and the value of nptr is stored in the 
location referenced by endptr (if endptr is not a null pointer). The usual ternary value is returned.

Parsing follows the standard C strtod function with some extensions. 
After optional leading whitespace, one has a subject sequence consisting of an optional sign ('+' or '-'), and either numeric data or special data. 
The subject sequence is defined as the longest initial subsequence of the input string, starting with the first non-whitespace character, that is of the expected form.

The form of numeric data is a non-empty sequence of significand digits with an optional decimal point, and an optional exponent 
consisting of an exponent prefix followed by an optional sign and a non-empty sequence of decimal digits. 
A significand digit is either a decimal digit or a Latin letter (62 possible characters), with 'A' = 10, 'B' = 11, ..., 'Z' = 35; 
case is ignored in bases less or equal to 36, in bases larger than 36, 'a' = 36, 'b' = 37, ..., 'z' = 61. 
The value of a significand digit must be strictly less than the base. 
The decimal point can be either the one defined by the current locale or the period (the first one is accepted for consistency with 
the C standard and the practice, the second one is accepted to allow the programmer to provide MPFR numbers from strings in a way 
that does not depend on the current locale). 
The exponent prefix can be 'e' or 'E' for bases up to 10, or '@' in any base; it indicates a multiplication by a power of the base. 
In bases 2 and 16, the exponent prefix can also be 'p' or 'P', in which case the exponent, called binary exponent, indicates a 
multiplication by a power of 2 instead of the base (there is a difference only for base 16); in base 16 for example '1p2' represents 
4 whereas '1@2' represents 256. The value of an exponent is always written in base 10.

If the argument base is 0, then the base is automatically detected as follows. If the significand starts with '0b' or '0B', base 2 is assumed. 
    If the significand starts with '0x' or '0X', base 16 is assumed. Otherwise base 10 is assumed.

Note: The exponent (if present) must contain at least a digit. 
Otherwise the possible exponent prefix and sign are not part of the number (which ends with the significand). 
Similarly, if '0b', '0B', '0x' or '0X' is not followed by a binary/hexadecimal digit, then the subject sequence stops at the character '0', thus 0 is read.

Special data (for infinities and NaN) can be '@inf@' or '@nan@(n-char-sequence-opt)', and if base <= 16, it can also be 'infinity', 
'inf', 'nan' or 'nan(n-char-sequence-opt)', all case insensitive. 
A 'n-char-sequence-opt' is a possibly empty string containing only digits, Latin letters and the underscore (0, 1, 2, ..., 9, a, b, ..., z, A, B, ..., Z, _). 
Note: one has an optional sign for all data, even NaN. For example, '-@nAn@(This_Is_Not_17)' is a valid representation for NaN in base 17.

Function: void mpfr_set_nan (mpfr_t x)
Function: void mpfr_set_inf (mpfr_t x, int sign)
Function: void mpfr_set_zero (mpfr_t x, int sign)
Set the variable x to NaN (Not-a-Number), infinity or zero respectively. 
In mpfr_set_inf or mpfr_set_zero, x is set to plus infinity or plus zero iff sign is nonnegative; in mpfr_set_nan, the sign bit of the result is unspecified.

Function: void mpfr_swap (mpfr_t x, mpfr_t y)
Swap the structures pointed to by x and y. In particular, the values are exchanged without rounding (this may be different from three mpfr_set calls using a third auxiliary variable).

Warning! Since the precisions are exchanged, this will affect future assignments. 
Moreover, since the significand pointers are also exchanged, you must not use this function if the allocation method used for x and/or y does not permit it. 

Next: Conversion Functions, Previous: Assignment Functions, Up: MPFR Interface   [Index]

5.3 Combined Initialization and Assignment Functions
--Macro: int mpfr_init_set (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
--Macro: int mpfr_init_set_ui (mpfr_t rop, unsigned long int op, mpfr_rnd_t rnd)
--Macro: int mpfr_init_set_si (mpfr_t rop, long int op, mpfr_rnd_t rnd)
--Macro: int mpfr_init_set_d (mpfr_t rop, double op, mpfr_rnd_t rnd)
Macro: int mpfr_init_set_ld (mpfr_t rop, long double op, mpfr_rnd_t rnd)
Macro: int mpfr_init_set_z (mpfr_t rop, mpz_t op, mpfr_rnd_t rnd)
Macro: int mpfr_init_set_q (mpfr_t rop, mpq_t op, mpfr_rnd_t rnd)
Initialize rop and set its value from op, rounded in the direction rnd.

5.4 Conversion Functions
Function: float mpfr_get_flt (mpfr_t op, mpfr_rnd_t rnd)
Function: double mpfr_get_d (mpfr_t op, mpfr_rnd_t rnd)
Function: long double mpfr_get_ld (mpfr_t op, mpfr_rnd_t rnd)
Function: __float128 mpfr_get_float128 (mpfr_t op, mpfr_rnd_t rnd)
Function: _Decimal64 mpfr_get_decimal64 (mpfr_t op, mpfr_rnd_t rnd)
Convert op to a float (respectively double, long double or _Decimal64), using the rounding mode rnd. 
If op is NaN, some fixed NaN (either quiet or signaling) or the result of 0.0/0.0 is returned. 
If op is +/-Inf, an infinity of the same sign or the result of +/-1.0/0.0 is returned. 
If op is zero, these functions return a zero, trying to preserve its sign, if possible. 
The mpfr_get_float128 and mpfr_get_decimal64 functions are built only under some conditions: see the documentation of 
mpfr_set_float128 and mpfr_set_decimal64 respectively.

Function: intmax_t mpfr_get_sj (mpfr_t op, mpfr_rnd_t rnd)
Function: uintmax_t mpfr_get_uj (mpfr_t op, mpfr_rnd_t rnd)
Convert op to a long, an unsigned long, an intmax_t or an uintmax_t (respectively) after rounding it to an integer with respect to rnd. 
If op is NaN, 0 is returned and the erange flag is set. 
If op is too big for the return type, the function returns the maximum or the minimum of the corresponding C type, depending on the 
direction of the overflow; the erange flag is set too. 
When there is no such range error, if the return value differs from op, i.e., if op is not an integer, the inexact flag is set. 
See also mpfr_fits_slong_p, mpfr_fits_ulong_p, mpfr_fits_intmax_p and mpfr_fits_uintmax_p.

Function: double mpfr_get_d_2exp (long *exp, mpfr_t op, mpfr_rnd_t rnd)
Function: long double mpfr_get_ld_2exp (long *exp, mpfr_t op, mpfr_rnd_t rnd)
Return d and set exp (formally, the value pointed to by exp) such that 0.5<=abs(d)<1 and d times 2 raised to exp equals op rounded to 
double (resp. long double) precision, using the given rounding mode. 
If op is zero, then a zero of the same sign (or an unsigned zero, if the implementation does not have signed zeros) is returned, and exp is set to 0. 
    If op is NaN or an infinity, then the corresponding double precision (resp. long-double precision) value is returned, and exp is undefined.

Function: int mpfr_frexp (int *exp, mpfr_t y, mpfr_t x, mpfr_rnd_t rnd)
Set exp (formally, the value pointed to by exp) and y such that 0.5<=abs(y)<1 and y times 2 raised to exp equals x rounded to the precision of y, 
using the given rounding mode. 
If x is zero, then y is set to a zero of the same sign and exp is set to 0. 
If x is NaN or an infinity, then y is set to the same value and exp is undefined.

Function: int mpfr_get_z_2exp (mpz_t rop, mpfr_t op)
Put the scaled significand of op (regarded as an integer, with the precision of op) into rop, and return the exponent exp (which may 
be outside the current exponent range) such that op exactly equals rop times 2 raised to the power exp. 
If op is zero, the minimal exponent emin is returned. 
If op is NaN or an infinity, the erange flag is set, rop is set to 0, and the the minimal exponent emin is returned. 
The returned exponent may be less than the minimal exponent emin of MPFR numbers in the current exponent range; in case the exponent 
is not representable in the int type, the erange flag is set and the minimal value of the int type is returned.

Function: int mpfr_get_z (mpz_t rop, mpfr_t op, mpfr_rnd_t rnd)
Convert op to a mpz_t, after rounding it with respect to rnd. 
If op is NaN or an infinity, the erange flag is set, rop is set to 0, and 0 is returned. 
Otherwise the return value is zero when rop is equal to op (i.e., when op is an integer), positive when it is greater than op, 
and negative when it is smaller than op; moreover, if rop differs from op, i.e., if op is not an integer, the inexact flag is set.

Function: void mpfr_get_q (mpq_t rop, mpfr_t op)
Convert op to a mpq_t. If op is NaN or an infinity, the erange flag is set and rop is set to 0. Otherwise the conversion is always exact.

Function: int mpfr_fits_ulong_p (mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_fits_slong_p (mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_fits_uint_p (mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_fits_sint_p (mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_fits_ushort_p (mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_fits_sshort_p (mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_fits_uintmax_p (mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_fits_intmax_p (mpfr_t op, mpfr_rnd_t rnd)
Return non-zero if op would fit in the respective C data type, respectively unsigned long, long, unsigned int, int, unsigned short, short, uintmax_t, 
intmax_t, when rounded to an integer in the direction rnd. 
For instance, with the MPFR_RNDU rounding mode on -0.5, the result will be non-zero for all these functions. 
For MPFR_RNDF, those functions return non-zero when it is guaranteed that the corresponding conversion function (for example mpfr_get_ui for mpfr_fits_ulong_p), 
when called with faithful rounding, will always return a number that is representable in the corresponding type. 
As a consequence, for MPFR_RNDF, mpfr_fits_ulong_p will return non-zero for a non-negative number less or equal to ULONG_MAX.

Next: Comparison Functions, Previous: Conversion Functions, Up: MPFR Interface   [Index]

5.5 Basic Arithmetic Functions
Function: int mpfr_add_d (mpfr_t rop, mpfr_t op1, double op2, mpfr_rnd_t rnd)
Function: int mpfr_add_z (mpfr_t rop, mpfr_t op1, mpz_t op2, mpfr_rnd_t rnd)
Function: int mpfr_add_q (mpfr_t rop, mpfr_t op1, mpq_t op2, mpfr_rnd_t rnd)
Set rop to op1 + op2 rounded in the direction rnd. The IEEE 754 rules are used, in particular for signed zeros. 
But for types having no signed zeros, 0 is considered unsigned (i.e., (+0) + 0 = (+0) and (-0) + 0 = (-0)). 
The mpfr_add_d function assumes that the radix of the double type is a power of 2, with a precision at most that declared by 
the C implementation (macro IEEE_DBL_MANT_DIG, and if not defined 53 bits).

Function: int mpfr_sub (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_rnd_t rnd)
Function: int mpfr_ui_sub (mpfr_t rop, unsigned long int op1, mpfr_t op2, mpfr_rnd_t rnd)
Function: int mpfr_sub_ui (mpfr_t rop, mpfr_t op1, unsigned long int op2, mpfr_rnd_t rnd)
--Function: int mpfr_si_sub (mpfr_t rop, long int op1, mpfr_t op2, mpfr_rnd_t rnd)
Function: int mpfr_sub_si (mpfr_t rop, mpfr_t op1, long int op2, mpfr_rnd_t rnd)
Function: int mpfr_d_sub (mpfr_t rop, double op1, mpfr_t op2, mpfr_rnd_t rnd)
Function: int mpfr_sub_d (mpfr_t rop, mpfr_t op1, double op2, mpfr_rnd_t rnd)
Function: int mpfr_z_sub (mpfr_t rop, mpz_t op1, mpfr_t op2, mpfr_rnd_t rnd)
Function: int mpfr_sub_z (mpfr_t rop, mpfr_t op1, mpz_t op2, mpfr_rnd_t rnd)
Function: int mpfr_sub_q (mpfr_t rop, mpfr_t op1, mpq_t op2, mpfr_rnd_t rnd)
Set rop to op1 - op2 rounded in the direction rnd. The IEEE 754 rules are used, in particular for signed zeros. 
But for types having no signed zeros, 0 is considered unsigned (i.e., (+0) - 0 = (+0), (-0) - 0 = (-0), 0 - (+0) = (-0) and 0 - (-0) = (+0)). 
The same restrictions than for mpfr_add_d apply to mpfr_d_sub and mpfr_sub_d.

Function: int mpfr_mul_d (mpfr_t rop, mpfr_t op1, double op2, mpfr_rnd_t rnd)
Function: int mpfr_mul_z (mpfr_t rop, mpfr_t op1, mpz_t op2, mpfr_rnd_t rnd)
Function: int mpfr_mul_q (mpfr_t rop, mpfr_t op1, mpq_t op2, mpfr_rnd_t rnd)
Set rop to op1 times op2 rounded in the direction rnd. When a result is zero, its sign is the product of the signs of the operands (for 
types having no signed zeros, 0 is considered positive). The same restrictions than for mpfr_add_d apply to mpfr_mul_d.

Function: int mpfr_ui_div (mpfr_t rop, unsigned long int op1, mpfr_t op2, mpfr_rnd_t rnd)
--Function: int mpfr_si_div (mpfr_t rop, long int op1, mpfr_t op2, mpfr_rnd_t rnd)
Function: int mpfr_d_div (mpfr_t rop, double op1, mpfr_t op2, mpfr_rnd_t rnd)
Function: int mpfr_div_d (mpfr_t rop, mpfr_t op1, double op2, mpfr_rnd_t rnd)
--Function: int mpfr_div_z (mpfr_t rop, mpfr_t op1, mpz_t op2, mpfr_rnd_t rnd)
Function: int mpfr_div_q (mpfr_t rop, mpfr_t op1, mpq_t op2, mpfr_rnd_t rnd)
Set rop to op1/op2 rounded in the direction rnd. When a result is zero, its sign is the product of the signs of the operands. 
For types having no signed zeros, 0 is considered positive; but note that if op1 is non-zero and op2 is zero, the result might change 
from +/-Inf to NaN in future MPFR versions if there is an opposite decision on the IEEE 754 side. The same restrictions than for mpfr_add_d apply to mpfr_d_div and mpfr_div_d.

--Function: int mpfr_sqrt (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
--Function: int mpfr_sqrt_ui (mpfr_t rop, unsigned long int op, mpfr_rnd_t rnd)
--Set rop to the square root of op rounded in the direction rnd. Set rop to -0 if op is -0, to be consistent with the IEEE 754 standard. Set rop to NaN if op is negative.

Function: int mpfr_rec_sqrt (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the reciprocal square root of op rounded in the direction rnd. 
Set rop to +Inf if op is +/-0, +0 if op is +Inf, and NaN if op is negative. 
Warning! Therefore the result on -0 is different from the one of the rSqrt function recommended by the IEEE 754-2008 standard (Section 9.2.1), which is -Inf instead of +Inf.

Function: int mpfr_cbrt (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_rootn_ui (mpfr_t rop, mpfr_t op, unsigned long int k, mpfr_rnd_t rnd)
Set rop to the cubic root (resp. the kth root) of op rounded in the direction rnd. For k = 0, set rop to NaN. 
For k odd (resp. even) and op negative (including -Inf), set rop to a negative number (resp. NaN). 
If op is zero, set rop to zero with the sign obtained by the usual limit rules, i.e., the same sign as op if k is odd, and positive if k is even.

These functions agree with the rootn function of the IEEE 754-2008 standard (Section 9.2).

--Function: int mpfr_root (mpfr_t rop, mpfr_t op, unsigned long int k, mpfr_rnd_t rnd)
--This function is the same as mpfr_rootn_ui except when op is -0 and k is even: the result is -0 instead of +0 (the reason was to be consistent with mpfr_sqrt). 
--Said otherwise, if op is zero, set rop to op.
--
--This function predates the IEEE 754-2008 standard and behaves differently from its rootn function. It is marked as deprecated and will be removed in a future release.

--Function: int mpfr_pow (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_rnd_t rnd)
--Function: int mpfr_pow_ui (mpfr_t rop, mpfr_t op1, unsigned long int op2, mpfr_rnd_t rnd)
--Function: int mpfr_pow_si (mpfr_t rop, mpfr_t op1, long int op2, mpfr_rnd_t rnd)
Function: int mpfr_pow_z (mpfr_t rop, mpfr_t op1, mpz_t op2, mpfr_rnd_t rnd)
--Function: int mpfr_ui_pow_ui (mpfr_t rop, unsigned long int op1, unsigned long int op2, mpfr_rnd_t rnd)
--Function: int mpfr_ui_pow (mpfr_t rop, unsigned long int op1, mpfr_t op2, mpfr_rnd_t rnd)
--Set rop to op1 raised to op2, rounded in the direction rnd. Special values are handled as described in the ISO C99 and IEEE 754-2008 standards for the pow function:
--
--pow(+/-0, y) returns plus or minus infinity for y a negative odd integer.
--pow(+/-0, y) returns plus infinity for y negative and not an odd integer.
--pow(+/-0, y) returns plus or minus zero for y a positive odd integer.
--pow(+/-0, y) returns plus zero for y positive and not an odd integer.
--pow(-1, +/-Inf) returns 1.
--pow(+1, y) returns 1 for any y, even a NaN.
--pow(x, +/-0) returns 1 for any x, even a NaN.
--pow(x, y) returns NaN for finite negative x and finite non-integer y.
--pow(x, -Inf) returns plus infinity for 0 < abs(x) < 1, and plus zero for abs(x) > 1.
--pow(x, +Inf) returns plus zero for 0 < abs(x) < 1, and plus infinity for abs(x) > 1.
--pow(-Inf, y) returns minus zero for y a negative odd integer.
--pow(-Inf, y) returns plus zero for y negative and not an odd integer.
--pow(-Inf, y) returns minus infinity for y a positive odd integer.
--pow(-Inf, y) returns plus infinity for y positive and not an odd integer.
--pow(+Inf, y) returns plus zero for y negative, and plus infinity for y positive.
--Note: When 0 is of integer type, it is regarded as +0 by these functions. We do not use the usual limit rules in this case, as these rules are not used for pow.
--
--Function: int mpfr_neg (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_abs (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to -op and the absolute value of op respectively, rounded in the direction rnd. 
Just changes or adjusts the sign if rop and op are the same variable, otherwise a rounding might occur if the precision of rop is less than that of op.

The sign rule also applies to NaN in order to mimic the IEEE 754 negate and abs operations, i.e., for mpfr_neg, the sign is reversed, and for mpfr_abs, the sign is set to positive. 
But contrary to IEEE 754, the NaN flag is set as usual.

Function: int mpfr_dim (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_rnd_t rnd)
Set rop to the positive difference of op1 and op2, i.e., op1 - op2 rounded in the direction rnd if op1 > op2, +0 if op1 <= op2, and NaN if op1 or op2 is NaN.

Function: int mpfr_mul_2ui (mpfr_t rop, mpfr_t op1, unsigned long int op2, mpfr_rnd_t rnd)
Function: int mpfr_mul_2si (mpfr_t rop, mpfr_t op1, long int op2, mpfr_rnd_t rnd)
Set rop to op1 times 2 raised to op2 rounded in the direction rnd. Just increases the exponent by op2 when rop and op1 are identical.

Function: int mpfr_div_2ui (mpfr_t rop, mpfr_t op1, unsigned long int op2, mpfr_rnd_t rnd)
Function: int mpfr_div_2si (mpfr_t rop, mpfr_t op1, long int op2, mpfr_rnd_t rnd)
Set rop to op1 divided by 2 raised to op2 rounded in the direction rnd. Just decreases the exponent by op2 when rop and op1 are identical.

Next: Special Functions, Previous: Basic Arithmetic Functions, Up: MPFR Interface   [Index]

5.6 Comparison Functions
--Function: int mpfr_cmp (mpfr_t op1, mpfr_t op2)
--Function: int mpfr_cmp_ui (mpfr_t op1, unsigned long int op2)
--Function: int mpfr_cmp_si (mpfr_t op1, long int op2)
Function: int mpfr_cmp_d (mpfr_t op1, double op2)
Function: int mpfr_cmp_ld (mpfr_t op1, long double op2)
Function: int mpfr_cmp_z (mpfr_t op1, mpz_t op2)
Function: int mpfr_cmp_q (mpfr_t op1, mpq_t op2)
Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, and a negative value if op1 < op2. 
Both op1 and op2 are considered to their full own precision, which may differ. 
If one of the operands is NaN, set the erange flag and return zero.

Note: These functions may be useful to distinguish the three possible cases. 
If you need to distinguish two cases only, it is recommended to use the predicate functions (e.g., mpfr_equal_p for the equality) described below; 
they behave like the IEEE 754 comparisons, in particular when one or both arguments are NaN. 
But only floating-point numbers can be compared (you may need to do a conversion first).

Function: int mpfr_cmp_ui_2exp (mpfr_t op1, unsigned long int op2, int e)
Function: int mpfr_cmp_si_2exp (mpfr_t op1, long int op2, int e)
Compare op1 and op2 multiplied by two to the power e. Similar as above.

Function: int mpfr_cmpabs (mpfr_t op1, mpfr_t op2)
Compare |op1| and |op2|. Return a positive value if |op1| > |op2|, zero if |op1| = |op2|, and a negative value if |op1| < |op2|. 
If one of the operands is NaN, set the erange flag and return zero.

Function: int mpfr_nan_p (mpfr_t op)
Function: int mpfr_inf_p (mpfr_t op)
Function: int mpfr_number_p (mpfr_t op)
Function: int mpfr_zero_p (mpfr_t op)
Function: int mpfr_regular_p (mpfr_t op)
Return non-zero if op is respectively NaN, an infinity, an ordinary number (i.e., neither NaN nor an infinity), zero, or 
a regular number (i.e., neither NaN, nor an infinity nor zero). Return zero otherwise.

Macro: int mpfr_sgn (mpfr_t op)
Return a positive value if op > 0, zero if op = 0, and a negative value if op < 0. 
If the operand is NaN, set the erange flag and return zero. This is equivalent to mpfr_cmp_ui (op, 0), but more efficient.

Function: int mpfr_greater_p (mpfr_t op1, mpfr_t op2)
Function: int mpfr_greaterequal_p (mpfr_t op1, mpfr_t op2)
Function: int mpfr_less_p (mpfr_t op1, mpfr_t op2)
Function: int mpfr_lessequal_p (mpfr_t op1, mpfr_t op2)
Function: int mpfr_equal_p (mpfr_t op1, mpfr_t op2)
Return non-zero if op1 > op2, op1 >= op2, op1 < op2, op1 <= op2, op1 = op2 respectively, and zero otherwise. 
Those functions return zero whenever op1 and/or op2 is NaN.

Function: int mpfr_lessgreater_p (mpfr_t op1, mpfr_t op2)
Return non-zero if op1 < op2 or op1 > op2 (i.e., neither op1, nor op2 is NaN, and op1 <> op2), zero otherwise (i.e., op1 and/or op2 is NaN, or op1 = op2).

Function: int mpfr_unordered_p (mpfr_t op1, mpfr_t op2)
Return non-zero if op1 or op2 is a NaN (i.e., they cannot be compared), zero otherwise.

Next: Input and Output Functions, Previous: Comparison Functions, Up: MPFR Interface   [Index]

5.7 Special Functions
All those functions, except explicitly stated (for example mpfr_sin_cos), return a ternary value, i.e., zero for an exact return value, 
a positive value for a return value larger than the exact result, and a negative value otherwise.

Important note: in some domains, computing special functions (even more with correct rounding) is expensive, even for small precision, 
for example the trigonometric and Bessel functions for large argument. 
For some functions, the memory usage might depend not only on the output precision: it is the case of the mpfr_rootn_ui function where 
the memory usage is also linear in the argument k, and of the incomplete Gamma function (dependence on the precision of op).

--Function: int mpfr_log (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_log_ui (mpfr_t rop, unsigned long op, mpfr_rnd_t rnd)
Function: int mpfr_log2 (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_log10 (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the natural logarithm of op, log2(op) or log10(op), respectively, rounded in the direction rnd. 
Set rop to +0 if op is 1 (in all rounding modes), for consistency with the ISO C99 and IEEE 754-2008 standards. 
Set rop to -Inf if op is +/-0 (i.e., the sign of the zero has no influence on the result).

Function: int mpfr_log1p (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the logarithm of one plus op, rounded in the direction rnd. Set rop to -Inf if op is -1.

--Function: int mpfr_exp (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_exp2 (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_exp10 (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the exponential of op, to 2 power of op or to 10 power of op, respectively, rounded in the direction rnd.

Function: int mpfr_expm1 (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the exponential of op followed by a subtraction by one, rounded in the direction rnd.

Function: int mpfr_cos (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_tan (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the cosine of op, sine of op, tangent of op, rounded in the direction rnd.

Function: int mpfr_sin_cos (mpfr_t sop, mpfr_t cop, mpfr_t op, mpfr_rnd_t rnd)
Set simultaneously sop to the sine of op and cop to the cosine of op, rounded in the direction rnd with the corresponding precisions of sop and cop, 
which must be different variables. 
Return 0 iff both results are exact, more precisely it returns s+4c where 
s=0 if sop is exact, 
s=1 if sop is larger than the sine of op, 
s=2 if sop is smaller than the sine of op, and similarly for c and the cosine of op.

Function: int mpfr_sec (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_csc (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_cot (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the secant of op, cosecant of op, cotangent of op, rounded in the direction rnd.

Function: int mpfr_acos (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_asin (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_atan (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the arc-cosine, arc-sine or arc-tangent of op, rounded in the direction rnd. 
Note that since acos(-1) returns the floating-point number closest to Pi according to the given rounding mode, this number might not be 
in the output range 0 <= rop < Pi of the arc-cosine function; still, the result lies in the image of the output range by the rounding function. 
The same holds for asin(-1), asin(1), atan(-Inf), atan(+Inf) or for atan(op) with large op and small precision of rop.

Function: int mpfr_atan2 (mpfr_t rop, mpfr_t y, mpfr_t x, mpfr_rnd_t rnd)
Set rop to the arc-tangent2 of y and x, rounded in the direction rnd: 
if x > 0, atan2(y, x) = atan(y/x); 
if x < 0, atan2(y, x) = sign(y)*(Pi - atan(abs(y/x))), thus a number from -Pi to Pi. 
As for atan, in case the exact mathematical result is +Pi or -Pi, its rounded result might be outside the function output range.

atan2(y, 0) does not raise any floating-point exception. Special values are handled as described in the ISO C99 and IEEE 754-2008 standards for the atan2 function:

atan2(+0, -0) returns +Pi.
atan2(-0, -0) returns -Pi.
atan2(+0, +0) returns +0.
atan2(-0, +0) returns -0.
atan2(+0, x) returns +Pi for x < 0.
atan2(-0, x) returns -Pi for x < 0.
atan2(+0, x) returns +0 for x > 0.
atan2(-0, x) returns -0 for x > 0.
atan2(y, 0) returns -Pi/2 for y < 0.
atan2(y, 0) returns +Pi/2 for y > 0.
atan2(+Inf, -Inf) returns +3*Pi/4.
atan2(-Inf, -Inf) returns -3*Pi/4.
atan2(+Inf, +Inf) returns +Pi/4.
atan2(-Inf, +Inf) returns -Pi/4.
atan2(+Inf, x) returns +Pi/2 for finite x.
atan2(-Inf, x) returns -Pi/2 for finite x.
atan2(y, -Inf) returns +Pi for finite y > 0.
atan2(y, -Inf) returns -Pi for finite y < 0.
atan2(y, +Inf) returns +0 for finite y > 0.
atan2(y, +Inf) returns -0 for finite y < 0.
Function: int mpfr_cosh (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_sinh (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_tanh (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the hyperbolic cosine, sine or tangent of op, rounded in the direction rnd.

Function: int mpfr_sinh_cosh (mpfr_t sop, mpfr_t cop, mpfr_t op, mpfr_rnd_t rnd)
Set simultaneously sop to the hyperbolic sine of op and cop to the hyperbolic cosine of op, rounded in the direction rnd 
with the corresponding precision of sop and cop, which must be different variables. 
Return 0 iff both results are exact (see mpfr_sin_cos for a more detailed description of the return value).

Function: int mpfr_sech (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_csch (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_coth (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the hyperbolic secant of op, cosecant of op, cotangent of op, rounded in the direction rnd.

Function: int mpfr_acosh (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_asinh (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_atanh (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the inverse hyperbolic cosine, sine or tangent of op, rounded in the direction rnd.

Function: int mpfr_fac_ui (mpfr_t rop, unsigned long int op, mpfr_rnd_t rnd)
Set rop to the factorial of op, rounded in the direction rnd.

Function: int mpfr_eint (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the exponential integral of op, rounded in the direction rnd. 
This is the sum of Euler's constant, of the logarithm of the absolute value of op, and of the sum for k from 1 to infinity of op to the power k, divided by k and factorial(k). 
For positive op, it corresponds to the Ei function at op (see formula 5.1.10 from the Handbook of Mathematical Functions from Abramowitz and Stegun), and for negative op, 
to the opposite of the E1 function (sometimes called eint1) at -op (formula 5.1.1 from the same reference).

Function: int mpfr_li2 (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to real part of the dilogarithm of op, rounded in the direction rnd. MPFR defines the dilogarithm function as the integral of -log(1-t)/t from 0 to op.

--Function: int mpfr_gamma (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_gamma_inc (mpfr_t rop, mpfr_t op, mpfr_t op2, mpfr_rnd_t rnd)
Set rop to the value of the Gamma function on op, resp. the incomplete Gamma function on op and op2, rounded in the direction rnd. 
(In the literature, mpfr_gamma_inc is called upper incomplete Gamma function, or sometimes complementary incomplete Gamma function.) 
For mpfr_gamma (and mpfr_gamma_inc when op2 is zero), when op is a negative integer, rop is set to NaN.

Note: the current implementation of mpfr_gamma_inc is slow for large values of rop or op, in which case some internal overflow might also occur.

Function: int mpfr_lngamma (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the value of the logarithm of the Gamma function on op, rounded in the direction rnd. 
When op is 1 or 2, set rop to +0 (in all rounding modes). 
When op is an infinity or a nonpositive integer, set rop to +Inf, following the general rules on special values. 
When -2k-1 < op < -2k, k being a nonnegative integer, set rop to NaN. See also mpfr_lgamma.

Function: int mpfr_lgamma (mpfr_t rop, int *signp, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the value of the logarithm of the absolute value of the Gamma function on op, rounded in the direction rnd. 
The sign (1 or -1) of Gamma(op) is returned in the object pointed to by signp. When op is 1 or 2, set rop to +0 (in all rounding modes). 
When op is an infinity or a nonpositive integer, set rop to +Inf. When op is NaN, -Inf or a negative integer, *signp is undefined, and when op is +/-0, *signp is the sign of the zero.

Function: int mpfr_digamma (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the value of the Digamma (sometimes also called Psi) function on op, rounded in the direction rnd. When op is a negative integer, set rop to NaN.

Function: int mpfr_beta (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_rnd_t rnd)
Set rop to the value of the Beta function at arguments op1 and op2. 
Note: the current code does not try to avoid internal overflow or underflow, and might use a huge internal precision in some cases.

Function: int mpfr_zeta (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_zeta_ui (mpfr_t rop, unsigned long op, mpfr_rnd_t rnd)
Set rop to the value of the Riemann Zeta function on op, rounded in the direction rnd.

Function: int mpfr_erf (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_erfc (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the value of the error function on op (resp. the complementary error function on op) rounded in the direction rnd.

Function: int mpfr_j0 (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_j1 (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_jn (mpfr_t rop, long n, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the value of the first kind Bessel function of order 0, (resp. 1 and n) on op, rounded in the direction rnd. 
When op is NaN, rop is always set to NaN. When op is plus or minus Infinity, rop is set to +0. 
When op is zero, and n is not zero, rop is set to +0 or -0 depending on the parity and sign of n, and the sign of op.

Function: int mpfr_y0 (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_y1 (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_yn (mpfr_t rop, long n, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the value of the second kind Bessel function of order 0 (resp. 1 and n) on op, rounded in the direction rnd. 
When op is NaN or negative, rop is always set to NaN. When op is +Inf, rop is set to +0. 
When op is zero, rop is set to +Inf or -Inf depending on the parity and sign of n.

Function: int mpfr_fma (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_t op3, mpfr_rnd_t rnd)
Function: int mpfr_fms (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_t op3, mpfr_rnd_t rnd)
Set rop to (op1 times op2) + op3 (resp. (op1 times op2) - op3) rounded in the direction rnd. 
Concerning special values (signed zeros, infinities, NaN), these functions behave like a multiplication followed by a separate addition or subtraction. 
That is, the fused operation matters only for rounding.

Function: int mpfr_fmma (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_t op3, mpfr_t op4, mpfr_rnd_t rnd)
Function: int mpfr_fmms (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_t op3, mpfr_t op4, mpfr_rnd_t rnd)
Set rop to (op1 times op2) + (op3 times op4) (resp. (op1 times op2) - (op3 times op4)) rounded in the direction rnd. 
In case the computation of op1 times op2 overflows or underflows (or that of op3 times op4), the result rop is computed as if 
the two intermediate products were computed with rounding toward zero.

Function: int mpfr_agm (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_rnd_t rnd)
Set rop to the arithmetic-geometric mean of op1 and op2, rounded in the direction rnd. 
The arithmetic-geometric mean is the common limit of the sequences u_n and v_n, where u_0=op1, v_0=op2, u_(n+1) is the arithmetic mean of u_n and v_n, and 
v_(n+1) is the geometric mean of u_n and v_n. 
If any operand is negative and the other one is not zero, set rop to NaN. 
If any operand is zero and the other one is finite (resp. infinite), set rop to +0 (resp. NaN).

Function: int mpfr_hypot (mpfr_t rop, mpfr_t x, mpfr_t y, mpfr_rnd_t rnd)
Set rop to the Euclidean norm of x and y, i.e., the square root of the sum of the squares of x and y, rounded in the direction rnd. 
Special values are handled as described in the ISO C99 (Section F.9.4.3) and IEEE 754-2008 (Section 9.2.1) standards: 
If x or y is an infinity, then +Inf is returned in rop, even if the other number is NaN.

Function: int mpfr_ai (mpfr_t rop, mpfr_t x, mpfr_rnd_t rnd)
Set rop to the value of the Airy function Ai on x, rounded in the direction rnd. When x is NaN, rop is always set to NaN. 
When x is +Inf or -Inf, rop is +0. The current implementation is not intended to be used with large arguments. 
It works with abs(x) typically smaller than 500. For larger arguments, other methods should be used and will be implemented in a future version.

Function: int mpfr_const_log2 (mpfr_t rop, mpfr_rnd_t rnd)
--Function: int mpfr_const_pi (mpfr_t rop, mpfr_rnd_t rnd)
Function: int mpfr_const_euler (mpfr_t rop, mpfr_rnd_t rnd)
Function: int mpfr_const_catalan (mpfr_t rop, mpfr_rnd_t rnd)
Set rop to the logarithm of 2, the value of Pi, of Euler's constant 0.577..., of Catalan's constant 0.915..., respectively, rounded in the direction rnd. 
These functions cache the computed values to avoid other calculations if a lower or equal precision is requested. 
To free these caches, use mpfr_free_cache or mpfr_free_cache2.

Function: void mpfr_free_cache (void)
Free all caches and pools used by MPFR internally (those local to the current thread and those shared by all threads). 
You should call this function before terminating a thread, even if you did not call mpfr_const_* functions directly (they could have been called internally).

Function: void mpfr_free_cache2 (mpfr_free_cache_t way)
Free various caches and pools used by MPFR internally, as specified by way, which is a set of flags:

those local to the current thread if flag MPFR_FREE_LOCAL_CACHE is set;
those shared by all threads if flag MPFR_FREE_GLOBAL_CACHE is set.
The other bits of way are currently ignored and are reserved for future use; they should be zero.

Note: mpfr_free_cache2(MPFR_FREE_LOCAL_CACHE|MPFR_FREE_GLOBAL_CACHE) is currently equivalent to mpfr_free_cache().

Function: void mpfr_free_pool (void)
Free the pools used by MPFR internally. 
Note: This function is automatically called after the thread-local caches are freed (with mpfr_free_cache or mpfr_free_cache2).

Function: int mpfr_mp_memory_cleanup (void)
This function should be called before calling mp_set_memory_functions. See Memory Handling, for more information. 
Zero is returned in case of success, non-zero in case of error. 
Errors are currently not possible, but checking the return value is recommended for future compatibility.

Function: int mpfr_sum (mpfr_t rop, const mpfr_ptr tab[], unsigned long int n, mpfr_rnd_t rnd)
Set rop to the sum of all elements of tab, whose size is n, correctly rounded in the direction rnd. 
Warning: for efficiency reasons, tab is an array of pointers to mpfr_t, not an array of mpfr_t. 
If n = 0, then the result is +0, and if n = 1, then the function is equivalent to mpfr_set. 
For the special exact cases, the result is the same as the one obtained with a succession of additions (mpfr_add) in infinite precision. 
In particular, if the result is an exact zero and n >= 1:

if all the inputs have the same sign (i.e., all +0 or all -0), then the result has the same sign as the inputs;
otherwise, either because all inputs are zeros with at least a +0 and a -0, or because some inputs are non-zero (but they globally cancel), 
the result is +0, except for the MPFR_RNDD rounding mode, where it is -0.

5.10 Integer and Remainder Related Functions
Function: int mpfr_rint (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_ceil (mpfr_t rop, mpfr_t op)
--Function: int mpfr_floor (mpfr_t rop, mpfr_t op)
Function: int mpfr_round (mpfr_t rop, mpfr_t op)
Function: int mpfr_roundeven (mpfr_t rop, mpfr_t op)
Function: int mpfr_trunc (mpfr_t rop, mpfr_t op)
Set rop to op rounded to an integer. 
mpfr_rint rounds to the nearest representable integer in the given direction rnd, and the other five functions behave in a similar way with some fixed rounding mode:

mpfr_ceil: to the next higher or equal representable integer (like mpfr_rint with MPFR_RNDU);
--mpfr_floor to the next lower or equal representable integer (like mpfr_rint with MPFR_RNDD);
mpfr_round to the nearest representable integer, rounding halfway cases away from zero (as in the roundTiesToAway mode of IEEE 754-2008);
mpfr_roundeven to the nearest representable integer, rounding halfway cases with the even-rounding rule (like mpfr_rint with MPFR_RNDN);
mpfr_trunc to the next representable integer toward zero (like mpfr_rint with MPFR_RNDZ).
When op is a zero or an infinity, set rop to the same value (with the same sign).

The return value is zero when the result is exact, positive when it is greater than the original value of op, and negative when it is smaller. 
More precisely, the return value is 0 when op is an integer representable in rop, 1 or -1 when op is an integer that is not representable in rop, 2 or -2 when op is not an integer.

--When op is NaN, the NaN flag is set as usual. In the other cases, the inexact flag is set when rop differs from op, following the ISO C99 rule for the rint function. 
--If you want the behavior to be more like IEEE 754 / ISO TS 18661-1, i.e., the usual behavior where the round-to-integer function is regarded as any other mathematical 
--function, you should use one the mpfr_rint_* functions instead.

Note that no double rounding is performed; for instance, 10.5 (1010.1 in binary) is rounded by mpfr_rint with rounding to nearest to 12 (1100 in binary) in 2-bit precision, 
because the two enclosing numbers representable on two bits are 8 and 12, and the closest is 12. 
(If one first rounded to an integer, one would round 10.5 to 10 with even rounding, and then 10 would be rounded to 8 again with even rounding.)

Function: int mpfr_rint_ceil (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_rint_floor (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_rint_round (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_rint_roundeven (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Function: int mpfr_rint_trunc (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to op rounded to an integer:

mpfr_rint_ceil: to the next higher or equal integer;
mpfr_rint_floor: to the next lower or equal integer;
mpfr_rint_round: to the nearest integer, rounding halfway cases away from zero;
mpfr_rint_roundeven: to the nearest integer, rounding halfway cases to the nearest even integer;
mpfr_rint_trunc to the next integer toward zero.
If the result is not representable, it is rounded in the direction rnd. 
When op is a zero or an infinity, set rop to the same value (with the same sign). 
The return value is the ternary value associated with the considered round-to-integer function (regarded in the same way as any other mathematical function).

Contrary to mpfr_rint, those functions do perform a double rounding: 
first op is rounded to the nearest integer in the direction given by the function name, then 
this nearest integer (if not representable) is rounded in the given direction rnd. 
Thus these round-to-integer functions behave more like the other mathematical functions, i.e., 
the returned result is the correct rounding of the exact result of the function in the real numbers.

For example, mpfr_rint_round with rounding to nearest and a precision of two bits rounds 6.5 to 7 (halfway cases away from zero), then 
    7 is rounded to 8 by the round-even rule, despite the fact that 6 is also representable on two bits, and is closer to 6.5 than 8.

Function: int mpfr_frac (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
Set rop to the fractional part of op, having the same sign as op, rounded in the direction rnd (unlike in mpfr_rint, rnd affects only 
how the exact fractional part is rounded, not how the fractional part is generated). 
When op is an integer or an infinity, set rop to zero with the same sign as op.

Function: int mpfr_modf (mpfr_t iop, mpfr_t fop, mpfr_t op, mpfr_rnd_t rnd)
Set simultaneously iop to the integral part of op and fop to the fractional part of op, rounded in the direction rnd with the 
corresponding precision of iop and fop (equivalent to mpfr_trunc(iop, op, rnd) and mpfr_frac(fop, op, rnd)). 
The variables iop and fop must be different. 
Return 0 iff both results are exact (see mpfr_sin_cos for a more detailed description of the return value).

Function: int mpfr_fmod (mpfr_t r, mpfr_t x, mpfr_t y, mpfr_rnd_t rnd)
Function: int mpfr_fmodquo (mpfr_t r, long* q, mpfr_t x, mpfr_t y, mpfr_rnd_t rnd)
Function: int mpfr_remainder (mpfr_t r, mpfr_t x, mpfr_t y, mpfr_rnd_t rnd)
Function: int mpfr_remquo (mpfr_t r, long* q, mpfr_t x, mpfr_t y, mpfr_rnd_t rnd)
Set r to the value of x - ny, rounded according to the direction rnd, where n is the integer quotient of x divided by y, defined as follows: 
n is rounded toward zero for mpfr_fmod and mpfr_fmodquo, and to the nearest integer (ties rounded to even) for mpfr_remainder and mpfr_remquo.

Special values are handled as described in Section F.9.7.1 of the ISO C99 standard: 
If x is infinite or y is zero, r is NaN. 
If y is infinite and x is finite, r is x rounded to the precision of r. 
If r is zero, it has the sign of x. 
The return value is the ternary value corresponding to r.

Additionally, mpfr_fmodquo and mpfr_remquo store the low significant bits from the quotient n in *q (more precisely the number of bits in a long minus one), 
with the sign of x divided by y (except if those low bits are all zero, in which case zero is returned). 
Note that x may be so large in magnitude relative to y that an exact representation of the quotient is not practical. 
The mpfr_remainder and mpfr_remquo functions are useful for additive argument reduction.

Function: int mpfr_integer_p (mpfr_t op)
Return non-zero iff op is an integer.

Next: Miscellaneous Functions, Previous: Integer and Remainder Related Functions, Up: MPFR Interface   [Index]

5.11 Rounding-Related Functions
Function: int mpfr_prec_round (mpfr_t x, mpfr_prec_t prec, mpfr_rnd_t rnd)
Round x according to rnd with precision prec, which must be an integer between MPFR_PREC_MIN and MPFR_PREC_MAX (otherwise the behavior is undefined). 
If prec is greater or equal to the precision of x, then new space is allocated for the significand, and it is filled with zeros. 
Otherwise, the significand is rounded to precision prec with the given direction. In both cases, the precision of x is changed to prec.

Here is an example of how to use mpfr_prec_round to implement Newton's algorithm to compute the inverse of a, assuming x is already an approximation to n bits:

  mpfr_set_prec (t, 2 * n);
  mpfr_set (t, a, MPFR_RNDN);         /* round a to 2n bits */
  mpfr_mul (t, t, x, MPFR_RNDN);      /* t is correct to 2n bits */
  mpfr_ui_sub (t, 1, t, MPFR_RNDN);   /* high n bits cancel with 1 */
  mpfr_prec_round (t, n, MPFR_RNDN);  /* t is correct to n bits */
  mpfr_mul (t, t, x, MPFR_RNDN);      /* t is correct to n bits */
  mpfr_prec_round (x, 2 * n, MPFR_RNDN); /* exact */
  mpfr_add (x, x, t, MPFR_RNDN);      /* x is correct to 2n bits */

Function: int mpfr_can_round (mpfr_t b, int err, mpfr_rnd_t rnd1, mpfr_rnd_t rnd2, mpfr_prec_t prec)
Assuming b is an approximation of an unknown number x in the direction rnd1 with error at most two to the power E(b)-err where E(b) is 
the exponent of b, return a non-zero value if one is able to round correctly x to precision prec with the direction rnd2 assuming an 
unbounded exponent range, and 0 otherwise (including for NaN and Inf). 
In other words, if the error on b is bounded by two to the power k ulps, and b has precision prec, you should give err=prec-k. 
This function does not modify its arguments.

If rnd1 is MPFR_RNDN or MPFR_RNDF, the error is considered to be either positive or negative, thus the possible range is twice as large 
as with a directed rounding for rnd1 (with the same value of err).

When rnd2 is MPFR_RNDF, let rnd3 be the opposite direction if rnd1 is a directed rounding, and MPFR_RNDN if rnd1 is MPFR_RNDN or MPFR_RNDF. 
The returned value of mpfr_can_round (b, err, rnd1, MPFR_RNDF, prec) is non-zero iff after the call mpfr_set (y, b, rnd3) with y of 
precision prec, y is guaranteed to be a faithful rounding of x.

Note: The ternary value cannot be determined in general with this function. 
However, if it is known that the exact value is not exactly representable in precision prec, then one can use the following trick to 
    determine the (non-zero) ternary value in any rounding mode rnd2 (note that MPFR_RNDZ below can be replaced by any directed rounding mode):

if (mpfr_can_round (b, err, MPFR_RNDN, MPFR_RNDZ,
                    prec + (rnd2 == MPFR_RNDN)))
  {
    /* round the approximation 'b' to the result 'r' of 'prec' bits
       with rounding mode 'rnd2' and get the ternary value 'inex' */
    inex = mpfr_set (r, b, rnd2);
  }
Indeed, if rnd2 is MPFR_RNDN, this will check if one can round to prec+1 bits with a directed rounding: if so, one can surely round 
to nearest to prec bits, and in addition one can determine the correct ternary value, which would not be the case when b is near from 
a value exactly representable on prec bits.

A detailed example is available in the examples subdirectory, file can_round.c.

Function: mpfr_prec_t mpfr_min_prec (mpfr_t x)
Return the minimal number of bits required to store the significand of x, and 0 for special values, including 0.

Function: const char * mpfr_print_rnd_mode (mpfr_rnd_t rnd)
Return a string ("MPFR_RNDD", "MPFR_RNDU", "MPFR_RNDN", "MPFR_RNDZ", "MPFR_RNDA") corresponding to the rounding mode rnd, or a null pointer if rnd is an invalid rounding mode.

Macro: int mpfr_round_nearest_away (int (foo)(mpfr_t, type1_t, ..., mpfr_rnd_t), mpfr_t rop, type1_t op, ...)
Given a function foo and one or more values op (which may be a mpfr_t, a long, a double, etc.), put in rop the round-to-nearest-away rounding of foo(op,...). 
This rounding is defined in the same way as round-to-nearest-even, except in case of tie, where the value away from zero is returned. 
The function foo takes as input, from second to penultimate argument(s), the argument list given after rop, a rounding mode as final argument, puts in its first 
argument the value foo(op,...) rounded according to this rounding mode, and returns the corresponding ternary value (which is expected to be correct, otherwise 
mpfr_round_nearest_away will not work as desired). Due to implementation constraints, this function must not be called when the minimal exponent emin is the smallest possible one. 
This macro has been made such that the compiler is able to detect mismatch between the argument list op and the function prototype of foo. 
Multiple input arguments op are supported only with C99 compilers. Otherwise, for C89 compilers, only one such argument is supported.

Note: this macro is experimental and its interface might change in future versions.

unsigned long ul;
mpfr_t f, r;
/* Code that inits and sets r, f, and ul, and if needed sets emin */
int i = mpfr_round_nearest_away (mpfr_add_ui, r, f, ul);
Next: Exception Related Functions, Previous: Rounding-Related Functions, Up: MPFR Interface   [Index]

5.12 Miscellaneous Functions
Function: void mpfr_nexttoward (mpfr_t x, mpfr_t y)
If x or y is NaN, set x to NaN; note that the NaN flag is set as usual. If x and y are equal, x is unchanged. Otherwise, if x is different from y, 
replace x by the next floating-point number (with the precision of x and the current exponent range) in the direction of y (the infinite values are 
seen as the smallest and largest floating-point numbers). If the result is zero, it keeps the same sign. No underflow, overflow, or inexact exception is raised.

Function: void mpfr_nextabove (mpfr_t x)
Function: void mpfr_nextbelow (mpfr_t x)
Equivalent to mpfr_nexttoward where y is plus infinity (resp. minus infinity).

Function: int mpfr_min (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_rnd_t rnd)
Function: int mpfr_max (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_rnd_t rnd)
Set rop to the minimum (resp. maximum) of op1 and op2. 
If op1 and op2 are both NaN, then rop is set to NaN. 
If op1 or op2 is NaN, then rop is set to the numeric value. 
If op1 and op2 are zeros of different signs, then rop is set to -0 (resp. +0).

Function: int mpfr_urandomb (mpfr_t rop, gmp_randstate_t state)
Generate a uniformly distributed random float in the interval 0 <= rop < 1. 
More precisely, the number can be seen as a float with a random non-normalized significand and exponent 0, which is then normalized (thus if e denotes 
    the exponent after normalization, then the least -e significant bits of the significand are always 0).

Return 0, unless the exponent is not in the current exponent range, in which case rop is set to NaN and 
a non-zero value is returned (this should never happen in practice, except in very specific cases). 
The second argument is a gmp_randstate_t structure which should be created using the GMP gmp_randinit function (see the GMP manual).

Note: for a given version of MPFR, the returned value of rop and the new value of state (which controls further random values) do not depend on the machine word size.

Function: int mpfr_urandom (mpfr_t rop, gmp_randstate_t state, mpfr_rnd_t rnd)
Generate a uniformly distributed random float. 
The floating-point number rop can be seen as if a random real number is generated according to the continuous uniform distribution on the interval [0, 1] and then rounded in the direction rnd.

The second argument is a gmp_randstate_t structure which should be created using the GMP gmp_randinit function (see the GMP manual).

Note: the note for mpfr_urandomb holds too. 
Moreover, the exact number (the random value to be rounded) and the next random state do not depend on the current exponent range and the rounding mode. 
    However, they depend on the target precision: from the same state of the random generator, if the precision of the destination is changed, then 
        the value may be completely different (and the state of the random generator is different too).

Function: int mpfr_nrandom (mpfr_t rop1, gmp_randstate_t state, mpfr_rnd_t rnd)
Function: int mpfr_grandom (mpfr_t rop1, mpfr_t rop2, gmp_randstate_t state, mpfr_rnd_t rnd)
Generate one (possibly two for mpfr_grandom) random floating-point number according to a standard normal Gaussian distribution (with mean zero and 
variance one). For mpfr_grandom, if rop2 is a null pointer, then only one value is generated and stored in rop1.

The floating-point number rop1 (and rop2) can be seen as if a random real number were generated according to the standard normal Gaussian distribution and then rounded in the direction rnd.

The gmp_randstate_t argument should be created using the GMP gmp_randinit function (see the GMP manual).

For mpfr_grandom, the combination of the ternary values is returned like with mpfr_sin_cos. 
If rop2 is a null pointer, the second ternary value is assumed to be 0 (note that the encoding of the only ternary value is not the same as the 
usual encoding for functions that return only one result). Otherwise the ternary value of a random number is always non-zero.

Note: the note for mpfr_urandomb holds too. In addition, the exponent range and the rounding mode might have a side effect on the next random state.

Note: mpfr_nrandom is much more efficient than mpfr_grandom, especially for large precision. Thus mpfr_grandom is marked as deprecated and will be removed in a future release.

Function: int mpfr_erandom (mpfr_t rop1, gmp_randstate_t state, mpfr_rnd_t rnd)
Generate one random floating-point number according to an exponential distribution, with mean one. Other characteristics are identical to mpfr_nrandom.

Function: int mpfr_get_exp (mpfr_t x)
Return the exponent of x, assuming that x is a non-zero ordinary number and the significand is considered in [1/2,1). 
For this function, x is allowed to be outside of the current range of acceptable values. 
The behavior for NaN, infinity or zero is undefined.

Function: int mpfr_set_exp (mpfr_t x, int e)
Set the exponent of x to e if x is a non-zero ordinary number and e is in the current exponent range, and return 0; otherwise, return a non-zero value (x is not changed).

Function: int mpfr_signbit (mpfr_t op)
Return a non-zero value iff op has its sign bit set (i.e., if it is negative, -0, or a NaN whose representation has its sign bit set).

Function: int mpfr_setsign (mpfr_t rop, mpfr_t op, int s, mpfr_rnd_t rnd)
Set the value of rop from op, rounded toward the given direction rnd, then set (resp. clear) its sign bit if s is non-zero (resp. zero), even when op is a NaN.

Function: int mpfr_copysign (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_rnd_t rnd)
Set the value of rop from op1, rounded toward the given direction rnd, then set its sign bit to that of op2 (even when op1 or op2 is a NaN). 
    This function is equivalent to mpfr_setsign (rop, op1, mpfr_signbit (op2), rnd).

--Function: const char * mpfr_get_patches (void)
--Return a null-terminated string containing the ids of the patches applied to the MPFR library (contents of the PATCHES file), separated by spaces. 
--Note: If the program has been compiled with an older MPFR version and is dynamically linked with a new MPFR library version, the identifiers of 
--the patches applied to the old (compile-time) MPFR version are not available (however this information should not have much interest in general).

Function: int mpfr_buildopt_tls_p (void)
Return a non-zero value if MPFR was compiled as thread safe using compiler-level Thread Local Storage (that is, MPFR was built with 
the '--enable-thread-safe' configure option, see INSTALL file), return zero otherwise.

Function: int mpfr_buildopt_float128_p (void)
Return a non-zero value if MPFR was compiled with '__float128' support (that is, MPFR was built with the '--enable-float128' configure option), return zero otherwise.

Function: int mpfr_buildopt_decimal_p (void)
Return a non-zero value if MPFR was compiled with decimal float support (that is, MPFR was built with the '--enable-decimal-float' configure option), return zero otherwise.

Function: int mpfr_buildopt_gmpinternals_p (void)
Return a non-zero value if MPFR was compiled with GMP internals (that is, MPFR was built with either '--with-gmp-build' or '--enable-gmp-internals' configure option), return zero otherwise.

Function: int mpfr_buildopt_sharedcache_p (void)
Return a non-zero value if MPFR was compiled so that all threads share the same cache for one MPFR constant, like mpfr_const_pi or mpfr_const_log2 (that is, 
MPFR was built with the '--enable-shared-cache' configure option), return zero otherwise. 
If the return value is non-zero, MPFR applications may need to be compiled with the '-pthread' option.

Function: const char * mpfr_buildopt_tune_case (void)
Return a string saying which thresholds file has been used at compile time. This file is normally selected from the processor type.

Next: Compatibility with MPF, Previous: Miscellaneous Functions, Up: MPFR Interface   [Index]

5.13 Exception Related Functions
Function: int mpfr_get_emin (void)
Function: int mpfr_get_emax (void)
Return the (current) smallest and largest exponents allowed for a floating-point variable. 
The smallest positive value of a floating-point variable is one half times 2 raised to the smallest exponent and the largest value has the form 
(1 - epsilon) times 2 raised to the largest exponent, where epsilon depends on the precision of the considered variable.

Function: int mpfr_set_emin (int exp)
Function: int mpfr_set_emax (int exp)
Set the smallest and largest exponents allowed for a floating-point variable. 
Return a non-zero value when exp is not in the range accepted by the implementation (in that case the smallest or largest exponent is not changed), and zero otherwise.

For the subsequent operations, it is the user's responsibility to check that any floating-point value used as an input is in the new exponent range (for example using mpfr_check_range). 
If a floating-point value outside the new exponent range is used as an input, the default behavior is undefined, in the sense of the ISO C standard; the behavior may also be explicitly 
documented, such as for mpfr_check_range.

Note: Caches may still have values outside the current exponent range. This is not an issue as the user cannot use these caches directly via the API (MPFR extends the exponent range 
internally when need be).

If emin > emax and a floating-point value needs to be produced as output, the behavior is undefined (mpfr_set_emin and mpfr_set_emax do not check this condition as it might occur 
    between successive calls to these two functions).

Function: int mpfr_get_emin_min (void)
Function: int mpfr_get_emin_max (void)
Function: int mpfr_get_emax_min (void)
Function: int mpfr_get_emax_max (void)
Return the minimum and maximum of the exponents allowed for mpfr_set_emin and mpfr_set_emax respectively. 
These values are implementation dependent, thus a program using mpfr_set_emax(mpfr_get_emax_max()) or mpfr_set_emin(mpfr_get_emin_min()) may not be portable.

Function: int mpfr_check_range (mpfr_t x, int t, mpfr_rnd_t rnd)
This function assumes that x is the correctly rounded value of some real value y in the direction rnd and some extended exponent range, and that t is the corresponding ternary value. 
For example, one performed t = mpfr_log (x, u, rnd), and y is the exact logarithm of u. 
Thus t is negative if x is smaller than y, positive if x is larger than y, and zero if x equals y. 
This function modifies x if needed to be in the current range of acceptable values: 
It generates an underflow or an overflow if the exponent of x is outside the current allowed range; the value of t may be used to avoid a double rounding. 
This function returns zero if the new value of x equals the exact one y, a positive value if that new value is larger than y, and a negative value if it is smaller than y. 
Note that unlike most functions, the new result x is compared to the (unknown) exact one y, not the input value x, i.e., the ternary value is propagated.

Note: If x is an infinity and t is different from zero (i.e., if the rounded result is an inexact infinity), then the overflow flag is set. 
    This is useful because mpfr_check_range is typically called (at least in MPFR functions) after restoring the flags that could have been set due to internal computations.

Function: int mpfr_subnormalize (mpfr_t x, int t, mpfr_rnd_t rnd)
This function rounds x emulating subnormal number arithmetic: if x is outside the subnormal exponent range of the emulated floating-point system, 
this function just propagates the ternary value t; otherwise, it rounds x to precision EXP(x)-emin+1 according to rounding mode rnd and previous 
ternary value t, avoiding double rounding problems. More precisely in the subnormal domain, denoting by e the value of emin, x is rounded in 
fixed-point arithmetic to an integer multiple of two to the power e-1; as a consequence, 1.5 multiplied by two to the power e-1 when t is zero is 
rounded to two to the power e with rounding to nearest.

PREC(x) is not modified by this function. rnd and t must be the rounding mode and the returned ternary value used when computing x (as in mpfr_check_range). 
The subnormal exponent range is from emin to emin+PREC(x)-1. If the result cannot be represented in the current exponent range of MPFR (due to a too small 
emax), the behavior is undefined. Note that unlike most functions, the result is compared to the exact one, not the input value x, i.e., the ternary value is propagated.

As usual, if the returned ternary value is non zero, the inexact flag is set. 
Moreover, if a second rounding occurred (because the input x was in the subnormal range), the underflow flag is set.

Warning! If you change emin (with mpfr_set_emin) just before calling mpfr_subnormalize, you need to make sure that the value is in the current exponent range of MPFR. 
But it is better to change emin before any computation, if possible.

Function: void mpfr_clear_underflow (void)
Function: void mpfr_clear_overflow (void)
Function: void mpfr_clear_divby0 (void)
Function: void mpfr_clear_nanflag (void)
Function: void mpfr_clear_inexflag (void)
Function: void mpfr_clear_erangeflag (void)
Clear (lower) the underflow, overflow, divide-by-zero, invalid, inexact and erange flags.

Function: void mpfr_clear_flags (void)
Clear (lower) all global flags (underflow, overflow, divide-by-zero, invalid, inexact, erange). Note: a group of flags can be cleared by using mpfr_flags_clear.

Function: void mpfr_set_underflow (void)
Function: void mpfr_set_overflow (void)
Function: void mpfr_set_divby0 (void)
Function: void mpfr_set_nanflag (void)
Function: void mpfr_set_inexflag (void)
Function: void mpfr_set_erangeflag (void)
Set (raise) the underflow, overflow, divide-by-zero, invalid, inexact and erange flags.

Function: int mpfr_underflow_p (void)
Function: int mpfr_overflow_p (void)
Function: int mpfr_divby0_p (void)
Function: int mpfr_nanflag_p (void)
Function: int mpfr_inexflag_p (void)
Function: int mpfr_erangeflag_p (void)
Return the corresponding (underflow, overflow, divide-by-zero, invalid, inexact, erange) flag, which is non-zero iff the flag is set.

The mpfr_flags_ functions below that take an argument mask can operate on any subset of the exception flags: 
a flag is part of this subset (or group) if and only if the corresponding bit of the argument mask is set. 
The MPFR_FLAGS_ macros will normally be used to build this argument. See Exceptions.

Function: void mpfr_flags_clear (mpfr_flags_t mask)
Clear (lower) the group of flags specified by mask.

Function: void mpfr_flags_set (mpfr_flags_t mask)
Set (raise) the group of flags specified by mask.

Function: mpfr_flags_t mpfr_flags_test (mpfr_flags_t mask)
Return the flags specified by mask. To test whether any flag from mask is set, compare the return value to 0. 
You can also test individual flags by AND'ing the result with MPFR_FLAGS_ macros. Example:

mpfr_flags_t t = mpfr_flags_test (MPFR_FLAGS_UNDERFLOW|
                                  MPFR_FLAGS_OVERFLOW)
...
if (t)  /* underflow and/or overflow (unlikely) */
  {
    if (t & MPFR_FLAGS_UNDERFLOW)  { /* handle underflow */ }
    if (t & MPFR_FLAGS_OVERFLOW)   { /* handle overflow  */ }
  }
Function: mpfr_flags_t mpfr_flags_save (void)
Return all the flags. It is equivalent to mpfr_flags_test(MPFR_FLAGS_ALL).

Function: void mpfr_flags_restore (mpfr_flags_t flags, mpfr_flags_t mask)
Restore the flags specified by mask to their state represented in flags.

Next: Custom Interface, Previous: Exception Related Functions, Up: MPFR Interface   [Index]

5.16 Internals
A limb means the part of a multi-precision number that fits in a single word. Usually a limb contains 32 or 64 bits. The C data type for a limb is mp_limb_t.

The mpfr_t type is internally defined as a one-element array of a structure, and mpfr_ptr is the C data type representing a pointer to this structure. The mpfr_t type consists of four fields:

The _mpfr_prec field is used to store the precision of the variable (in bits); this is not less than MPFR_PREC_MIN.
The _mpfr_sign field is used to store the sign of the variable.
The _mpfr_exp field stores the exponent. 
An exponent of 0 means a radix point just above the most significant limb. 
Non-zero values n are a multiplier 2^n relative to that point. 
A NaN, an infinity and a zero are indicated by special values of the exponent field.
Finally, the _mpfr_d field is a pointer to the limbs, least significant limbs stored first. 
The number of limbs in use is controlled by _mpfr_prec, namely ceil(_mpfr_prec/mp_bits_per_limb). 
Non-singular (i.e., different from NaN, Infinity or zero) values always have the most significant bit of the most significant limb set to 1. 
When the precision does not correspond to a whole number of limbs, the excess bits at the low end of the data are zeros.
Next: MPFR and the IEEE 754 Standard, Previous: MPFR Interface, Up: Top   [Index]

6 API Compatibility
The goal of this section is to describe some API changes that occurred from one version of MPFR to another, and how 
to write code that can be compiled and run with older MPFR versions. 
The minimum MPFR version that is considered here is 2.2.0 (released on 20 September 2005).

API changes can only occur between major or minor versions. 
Thus the patchlevel (the third number in the MPFR version) will be ignored in the following. 
If a program does not use MPFR internals, changes in the behavior between two versions differing only by the patchlevel should only 
result from what was regarded as a bug or unspecified behavior.

As a general rule, a program written for some MPFR version should work with later versions, possibly except at a new major version, 
where some features (described as obsolete for some time) can be removed. 
In such a case, a failure should occur during compilation or linking. 
If a result becomes incorrect because of such a change, please look at the various changes below (they are minimal, and most software 
should be unaffected), at the FAQ and at the MPFR web page for your version (a bug could have been introduced and be already fixed); 
and if the problem is not mentioned, please send us a bug report (see Reporting Bugs).

However, a program written for the current MPFR version (as documented by this manual) may not necessarily work with previous versions of MPFR. 
This section should help developers to write portable code.

Note: Information given here may be incomplete. API changes are also described in the NEWS file (for each version, instead of being classified like here), together with other changes.

Next: Added Functions, Previous: API Compatibility, Up: API Compatibility   [Index]

6.1 Type and Macro Changes
The official type for exponent values changed from mp_exp_t to int in MPFR 3.0. 
The type mp_exp_t will remain available as it comes from GMP (with a different meaning). 
These types are currently the same (int is defined as mp_exp_t with typedef), so that programs can still use mp_exp_t; but this may change in the future. 
Alternatively, using the following code after including mpfr.h will work with official MPFR versions, as int was never defined in MPFR 2.x:

#if MPFR_VERSION_MAJOR < 3
typedef mp_exp_t int;
#endif
The official types for precision values and for rounding modes respectively changed from mp_prec_t and mp_rnd_t to mpfr_prec_t and mpfr_rnd_t in MPFR 3.0. 
This change was actually done a long time ago in MPFR, at least since MPFR 2.2.0, with the following code in mpfr.h:

#ifndef mp_rnd_t
# define mp_rnd_t  mpfr_rnd_t
#endif
#ifndef mp_prec_t
# define mp_prec_t mpfr_prec_t
#endif
This means that it is safe to use the new official types mpfr_prec_t and mpfr_rnd_t in your programs. 
The types mp_prec_t and mp_rnd_t (defined in MPFR only) may be removed in the future, as the prefix mp_ is reserved by GMP.

The precision type mpfr_prec_t (mp_prec_t) was unsigned before MPFR 3.0; it is now signed. MPFR_PREC_MAX has not changed, though. 
Indeed the MPFR code requires that MPFR_PREC_MAX be representable in the exponent type, which may have the same size as mpfr_prec_t but has always been signed. 
The consequence is that valid code that does not assume anything about the signedness of mpfr_prec_t should work with past and new MPFR versions. 
This change was useful as the use of unsigned types tends to convert signed values to unsigned ones in expressions due to the usual arithmetic conversions, 
which can yield incorrect results if a negative value is converted in such a way. Warning! A program assuming (intentionally or not) that mpfr_prec_t is 
signed may be affected by this problem when it is built and run against MPFR 2.x.

The rounding modes GMP_RNDx were renamed to MPFR_RNDx in MPFR 3.0. However the old names GMP_RNDx have been kept for compatibility (this might change in future versions), using:

#define GMP_RNDN MPFR_RNDN
#define GMP_RNDZ MPFR_RNDZ
#define GMP_RNDU MPFR_RNDU
#define GMP_RNDD MPFR_RNDD
The rounding mode "round away from zero" (MPFR_RNDA) was added in MPFR 3.0 (however no rounding mode GMP_RNDA exists). 
Faithful rounding (MPFR_RNDF) was added in MPFR 4.0, but currently, it is partially supported.

The flags-related macros, whose name starts with MPFR_FLAGS_, were added in MPFR 4.0 
(for the new functions mpfr_flags_clear, mpfr_flags_restore, mpfr_flags_set and mpfr_flags_test, in particular).

Next: Changed Functions, Previous: Type and Macro Changes, Up: API Compatibility   [Index]

7 MPFR and the IEEE 754 Standard
This section describes differences between MPFR and the IEEE 754 standard, and behaviors that are not specified yet in IEEE 754.

The mpfr_root function predates IEEE 754-2008 and behaves differently from its rootn operation. It is deprecated and mpfr_rootn_ui should be used instead.

Operations with an unsigned zero: For functions taking an argument of integer or rational type, a zero of such a type is unsigned unlike 
the floating-point zero (this includes the zero of type unsigned long, which is a mathematical, exact zero, as opposed to a floating-point 
zero, which may come from an underflow and whose sign would correspond to the sign of the real non-zero value). 
Unless documented otherwise, this zero is regarded as +0, as if it were first converted to a MPFR number with mpfr_set_ui 
(thus the result may not agree with the usual limit rules applied to a mathematical zero). 
This is not the case of addition and subtraction (mpfr_add_ui, etc.), but for these functions, only the sign of a zero result would be affected, 
with +0 and -0 considered equal. 
Such operations are currently out of the scope of the IEEE 754 standard, and at the time of specification in MPFR, the Floating-Point Working 
Group in charge of the revision of IEEE 754 did not want to discuss issues with non-floating-point types in general.

Note also that some obvious differences may come from the fact that in MPFR, each variable has its own precision. 
For instance, a subtraction of two numbers of the same sign may yield an overflow; idem for a call to mpfr_set, mpfr_neg or mpfr_abs, 
if the destination variable has a smaller precision.

Next: References, Previous: MPFR and the IEEE 754 Standard, Up: Top   [Index]

Concept Index
Index Entry                 Section
A               
Accuracy:                   MPFR Interface
Arithmetic functions:           Basic Arithmetic Functions
Assignment functions:           Assignment Functions
C               
Comparison functions:           Comparison Functions
Compatibility with MPF:         Compatibility with MPF
Conditions for copying MPFR:            Copying
Conversion functions:           Conversion Functions
Copying conditions:             Copying
Custom interface:               Custom Interface
E               
Exception related functions:            Exception Related Functions
Exponent:               Nomenclature and Types
F               
Float arithmetic functions:             Basic Arithmetic Functions
Float comparisons functions:            Comparison Functions
Float functions:                MPFR Interface
Float input and output functions:               Input and Output Functions
Float output functions:         Formatted Output Functions
Floating-point functions:               MPFR Interface
Floating-point number:          Nomenclature and Types
G               
GNU Free Documentation License:         GNU Free Documentation License
GNU Free Documentation License:         GNU Free Documentation License
Group of flags:         Nomenclature and Types
I               
I/O functions:          Input and Output Functions
I/O functions:          Formatted Output Functions
Initialization functions:               Initialization Functions
Input functions:                Input and Output Functions
Installation:           Installing MPFR
Integer related functions:              Integer and Remainder Related Functions
Internals:              Internals
intmax_t:               Headers and Libraries
inttypes.h:             Headers and Libraries
L               
libmpfr:                Headers and Libraries
Libraries:              Headers and Libraries
Libtool:                Headers and Libraries
Limb:           Internals
Linking:                Headers and Libraries
M               
Miscellaneous float functions:          Miscellaneous Functions
mpfr.h:         Headers and Libraries
O               
Output functions:               Input and Output Functions
Output functions:               Formatted Output Functions
P               
Precision:              Nomenclature and Types
Precision:              MPFR Interface
R               
Regular number:         Nomenclature and Types
Remainder related functions:            Integer and Remainder Related Functions
Reporting bugs:         Reporting Bugs
Rounding mode related functions:                Rounding-Related Functions
Rounding Modes:         Nomenclature and Types
S               
Special functions:              Special Functions
stdarg.h:               Headers and Libraries
stdint.h:               Headers and Libraries
stdio.h:                Headers and Libraries
T               
Ternary value:          Rounding Modes
U               
uintmax_t:              Headers and Libraries
Jump to:        A   B   C   E   F   G   I   L   M   O   P   R   S   T   U  
Previous: Concept Index, Up: Top   [Index]

Function and Type Index
Jump to:        M  
Index Entry             Section
M               
mpfr_abs:               Basic Arithmetic Functions
mpfr_acos:              Special Functions
mpfr_acosh:             Special Functions
mpfr_add_d:             Basic Arithmetic Functions
mpfr_add_q:             Basic Arithmetic Functions
mpfr_add_z:             Basic Arithmetic Functions
mpfr_agm:               Special Functions
mpfr_ai:                Special Functions
mpfr_asin:              Special Functions
mpfr_asinh:             Special Functions
mpfr_atan:              Special Functions
mpfr_atan2:             Special Functions
mpfr_atanh:             Special Functions
mpfr_beta:              Special Functions
mpfr_buildopt_decimal_p:                Miscellaneous Functions
mpfr_buildopt_float128_p:               Miscellaneous Functions
mpfr_buildopt_gmpinternals_p:           Miscellaneous Functions
mpfr_buildopt_sharedcache_p:            Miscellaneous Functions
mpfr_buildopt_tls_p:            Miscellaneous Functions
mpfr_buildopt_tune_case:                Miscellaneous Functions
mpfr_can_round:         Rounding-Related Functions
mpfr_cbrt:              Basic Arithmetic Functions
mpfr_ceil:              Integer and Remainder Related Functions
mpfr_check_range:               Exception Related Functions
--mpfr_clear:           Initialization Functions
--mpfr_clears:          Initialization Functions
mpfr_clear_divby0:              Exception Related Functions
mpfr_clear_erangeflag:          Exception Related Functions
mpfr_clear_flags:               Exception Related Functions
mpfr_clear_inexflag:            Exception Related Functions
mpfr_clear_nanflag:             Exception Related Functions
mpfr_clear_overflow:            Exception Related Functions
mpfr_clear_underflow:           Exception Related Functions
--mpfr_cmp:             Comparison Functions
mpfr_cmpabs:            Comparison Functions
mpfr_cmp_d:             Comparison Functions
mpfr_cmp_ld:            Comparison Functions
mpfr_cmp_q:             Comparison Functions
--mpfr_cmp_si:          Comparison Functions
mpfr_cmp_si_2exp:               Comparison Functions
--mpfr_cmp_ui:          Comparison Functions
mpfr_cmp_ui_2exp:               Comparison Functions
mpfr_cmp_z:             Comparison Functions
mpfr_const_catalan:             Special Functions
mpfr_const_euler:               Special Functions
mpfr_const_log2:                Special Functions
mpfr_copysign:          Miscellaneous Functions
mpfr_cos:               Special Functions
mpfr_cosh:              Special Functions
mpfr_cot:               Special Functions
mpfr_coth:              Special Functions
mpfr_csc:               Special Functions
mpfr_csch:              Special Functions
mpfr_digamma:           Special Functions
mpfr_dim:               Basic Arithmetic Functions
mpfr_divby0_p:          Exception Related Functions
mpfr_div_2si:           Basic Arithmetic Functions
mpfr_div_2ui:           Basic Arithmetic Functions
mpfr_div_d:             Basic Arithmetic Functions
mpfr_div_q:             Basic Arithmetic Functions
--mpfr_div_z:           Basic Arithmetic Functions
mpfr_d_div:             Basic Arithmetic Functions
mpfr_d_sub:             Basic Arithmetic Functions
mpfr_eint:              Special Functions
mpfr_eq:                Compatibility with MPF
mpfr_equal_p:           Comparison Functions
mpfr_erandom:           Miscellaneous Functions
mpfr_erangeflag_p:              Exception Related Functions
mpfr_erf:               Special Functions
mpfr_erfc:              Special Functions
--mpfr_exp:             Special Functions
mpfr_exp10:             Special Functions
mpfr_exp2:              Special Functions
mpfr_expm1:             Special Functions
mpfr_fac_ui:            Special Functions
mpfr_fits_intmax_p:             Conversion Functions
mpfr_fits_sint_p:               Conversion Functions
mpfr_fits_slong_p:              Conversion Functions
mpfr_fits_sshort_p:             Conversion Functions
mpfr_fits_uintmax_p:            Conversion Functions
mpfr_fits_uint_p:               Conversion Functions
mpfr_fits_ulong_p:              Conversion Functions
mpfr_fits_ushort_p:             Conversion Functions
mpfr_flags_clear:               Exception Related Functions
mpfr_flags_restore:             Exception Related Functions
mpfr_flags_save:                Exception Related Functions
mpfr_flags_set:         Exception Related Functions
mpfr_flags_t:           Nomenclature and Types
mpfr_flags_test:                Exception Related Functions
--mpfr_floor:           Integer and Remainder Related Functions
mpfr_fma:               Special Functions
mpfr_fmma:              Special Functions
mpfr_fmms:              Special Functions
mpfr_fmod:              Integer and Remainder Related Functions
mpfr_fmodquo:           Integer and Remainder Related Functions
mpfr_fms:               Special Functions
mpfr_frac:              Integer and Remainder Related Functions
mpfr_free_cache:                Special Functions
mpfr_free_cache2:               Special Functions
mpfr_free_pool:         Special Functions
mpfr_frexp:             Conversion Functions
--mpfr_gamma:           Special Functions
mpfr_gamma_inc:         Special Functions
mpfr_get_d:             Conversion Functions
mpfr_get_decimal64:             Conversion Functions
mpfr_get_d_2exp:                Conversion Functions
mpfr_get_emax:          Exception Related Functions
mpfr_get_emax_max:              Exception Related Functions
mpfr_get_emax_min:              Exception Related Functions
mpfr_get_emin:          Exception Related Functions
mpfr_get_emin_max:              Exception Related Functions
mpfr_get_emin_min:              Exception Related Functions
mpfr_get_exp:           Miscellaneous Functions
mpfr_get_float128:              Conversion Functions
mpfr_get_flt:           Conversion Functions
mpfr_get_ld:            Conversion Functions
mpfr_get_ld_2exp:               Conversion Functions
mpfr_get_patches:               Miscellaneous Functions
mpfr_get_q:             Conversion Functions
mpfr_get_sj:            Conversion Functions
mpfr_get_uj:            Conversion Functions
mpfr_get_z:             Conversion Functions
mpfr_get_z_2exp:                Conversion Functions
mpfr_grandom:           Miscellaneous Functions
mpfr_greaterequal_p:            Comparison Functions
mpfr_greater_p:         Comparison Functions
mpfr_hypot:             Special Functions
mpfr_inexflag_p:                Exception Related Functions
mpfr_inf_p:             Comparison Functions
mpfr_init_set_ld:               Combined Initialization and Assignment Functions
mpfr_init_set_q:                Combined Initialization and Assignment Functions
mpfr_init_set_z:                Combined Initialization and Assignment Functions
mpfr_integer_p:         Integer and Remainder Related Functions
mpfr_j0:                Special Functions
mpfr_j1:                Special Functions
mpfr_jn:                Special Functions
mpfr_lessequal_p:               Comparison Functions
mpfr_lessgreater_p:             Comparison Functions
mpfr_less_p:            Comparison Functions
mpfr_lgamma:            Special Functions
mpfr_li2:               Special Functions
mpfr_lngamma:           Special Functions
--mpfr_log:             Special Functions
mpfr_log10:             Special Functions
mpfr_log1p:             Special Functions
mpfr_log2:              Special Functions
mpfr_log_ui:            Special Functions
mpfr_max:               Miscellaneous Functions
mpfr_min:               Miscellaneous Functions
mpfr_min_prec:          Rounding-Related Functions
mpfr_modf:              Integer and Remainder Related Functions
mpfr_mp_memory_cleanup:         Special Functions
mpfr_mul_2si:           Basic Arithmetic Functions
mpfr_mul_2ui:           Basic Arithmetic Functions
mpfr_mul_d:             Basic Arithmetic Functions
mpfr_mul_q:             Basic Arithmetic Functions
mpfr_mul_z:             Basic Arithmetic Functions
mpfr_nanflag_p:         Exception Related Functions
mpfr_nan_p:             Comparison Functions
--mpfr_neg:             Basic Arithmetic Functions
mpfr_nextabove:         Miscellaneous Functions
mpfr_nextbelow:         Miscellaneous Functions
mpfr_nexttoward:                Miscellaneous Functions
mpfr_nrandom:           Miscellaneous Functions
mpfr_number_p:          Comparison Functions
mpfr_overflow_p:                Exception Related Functions
mpfr_pow:               Basic Arithmetic Functions
mpfr_pow_si:            Basic Arithmetic Functions
mpfr_pow_ui:            Basic Arithmetic Functions
mpfr_pow_z:             Basic Arithmetic Functions
mpfr_prec_round:                Rounding-Related Functions
mpfr_prec_t:            Nomenclature and Types
mpfr_print_rnd_mode:            Rounding-Related Functions
mpfr_ptr:               Nomenclature and Types
mpfr_rec_sqrt:          Basic Arithmetic Functions
mpfr_regular_p:         Comparison Functions
mpfr_reldiff:           Compatibility with MPF
mpfr_remainder:         Integer and Remainder Related Functions
mpfr_remquo:            Integer and Remainder Related Functions
mpfr_rint:              Integer and Remainder Related Functions
mpfr_rint_ceil:         Integer and Remainder Related Functions
mpfr_rint_floor:                Integer and Remainder Related Functions
mpfr_rint_round:                Integer and Remainder Related Functions
mpfr_rint_roundeven:            Integer and Remainder Related Functions
mpfr_rint_trunc:                Integer and Remainder Related Functions
mpfr_rnd_t:             Nomenclature and Types
mpfr_root:              Basic Arithmetic Functions
mpfr_rootn_ui:          Basic Arithmetic Functions
mpfr_round:             Integer and Remainder Related Functions
mpfr_roundeven:         Integer and Remainder Related Functions
mpfr_round_nearest_away:                Rounding-Related Functions
mpfr_sec:               Special Functions
mpfr_sech:              Special Functions
mpfr_setsign:           Miscellaneous Functions
mpfr_set_decimal64:             Assignment Functions
mpfr_set_divby0:                Exception Related Functions
mpfr_set_emax:          Exception Related Functions
mpfr_set_emin:          Exception Related Functions
mpfr_set_erangeflag:            Exception Related Functions
mpfr_set_exp:           Miscellaneous Functions
mpfr_set_float128:              Assignment Functions
mpfr_set_flt:           Assignment Functions
mpfr_set_inexflag:              Exception Related Functions
mpfr_set_inf:           Assignment Functions
mpfr_set_ld:            Assignment Functions
mpfr_set_nan:           Assignment Functions
mpfr_set_nanflag:               Exception Related Functions
mpfr_set_overflow:              Exception Related Functions
--mpfr_set_q:           Assignment Functions
mpfr_set_si_2exp:               Assignment Functions
mpfr_set_sj:            Assignment Functions
mpfr_set_sj_2exp:               Assignment Functions
mpfr_set_ui_2exp:               Assignment Functions
mpfr_set_uj:            Assignment Functions
mpfr_set_uj_2exp:               Assignment Functions
mpfr_set_underflow:             Exception Related Functions
--mpfr_set_z:           Assignment Functions
mpfr_set_zero:          Assignment Functions
mpfr_set_z_2exp:                Assignment Functions
mpfr_sgn:               Comparison Functions
mpfr_signbit:           Miscellaneous Functions
mpfr_sinh:              Special Functions
mpfr_sinh_cosh:         Special Functions
mpfr_sin_cos:           Special Functions
--mpfr_si_div:          Basic Arithmetic Functions
--mpfr_si_sub:          Basic Arithmetic Functions
--mpfr_sqrt:                Basic Arithmetic Functions
--mpfr_sqrt_ui:         Basic Arithmetic Functions
mpfr_strtofr:           Assignment Functions
mpfr_sub:               Basic Arithmetic Functions
mpfr_subnormalize:              Exception Related Functions
mpfr_sub_d:             Basic Arithmetic Functions
mpfr_sub_q:             Basic Arithmetic Functions
mpfr_sub_si:            Basic Arithmetic Functions
mpfr_sub_ui:            Basic Arithmetic Functions
mpfr_sub_z:             Basic Arithmetic Functions
mpfr_sum:               Special Functions
mpfr_swap:              Assignment Functions
mpfr_t:         Nomenclature and Types
mpfr_tan:               Special Functions
mpfr_tanh:              Special Functions
mpfr_trunc:             Integer and Remainder Related Functions
mpfr_ui_div:            Basic Arithmetic Functions
mpfr_ui_pow:            Basic Arithmetic Functions
mpfr_ui_pow_ui:         Basic Arithmetic Functions
mpfr_ui_sub:            Basic Arithmetic Functions
mpfr_underflow_p:               Exception Related Functions
mpfr_unordered_p:               Comparison Functions
mpfr_urandom:           Miscellaneous Functions
mpfr_urandomb:          Miscellaneous Functions
MPFR_VERSION:           Miscellaneous Functions
MPFR_VERSION_MAJOR:             Miscellaneous Functions
MPFR_VERSION_MINOR:             Miscellaneous Functions
MPFR_VERSION_NUM:               Miscellaneous Functions
MPFR_VERSION_PATCHLEVEL:                Miscellaneous Functions
MPFR_VERSION_STRING:            Miscellaneous Functions
mpfr_y0:                Special Functions
mpfr_y1:                Special Functions
mpfr_yn:                Special Functions
mpfr_zero_p:            Comparison Functions
mpfr_zeta:              Special Functions
mpfr_zeta_ui:           Special Functions
mpfr_z_sub:             Basic Arithmetic Functions

MPIR
The Multiple Precision Integers and Rationals Library

3 MPIR Basics
Using functions, macros, data types, etc. not documented in this manual is strongly discouraged.
If you do so your application is guaranteed to be incompatible with future versions of MPIR.
3.1 Headers and Libraries
All declarations needed to use MPIR are collected in the include file mpir.h. It is designed to
work with both C and C++ compilers.
#include <mpir.h>
Note however that prototypes for MPIR functions with FILE * parameters are only provided if
 <stdio.h> is included too.
#include <stdio.h>
#include <mpir.h>
Likewise <stdarg.h> (or <varargs.h>) is required for prototypes with va_list parameters,
such as gmp_vprintf. And <obstack.h> for prototypes with struct obstack parameters, such
as gmp_obstack_printf, when available.
All programs using MPIR must link against the libmpir library. On a typical Unix-like system
this can be done with '-lmpir' respectively, for example
gcc myprogram.c -lmpir
MPIR C++ functions are in a separate libmpirxx library. This is built and installed if C++
support has been enabled (see Section 2.1 [Build Options], page 3). For example,
g++ mycxxprog.cc -lmpirxx -lmpir
MPIR is built using Libtool and an application can use that to link if desired, see GNU Libtool
If MPIR has been installed to a non-standard location then it may be necessary to use '-I'
and '-L' compiler options to point to the right directories, and some sort of run-time path for a
shared library.
3.2 Nomenclature and Types
In this manual, integer usually means a multiple precision integer, as defined by the MPIR
library. The C data type for such integers is mpz_t. Here are some examples of how to declare
such integers:
mpz_t sum;
struct foo { mpz_t x, y; };
mpz_t vec[20];
Rational number means a multiple precision fraction. The C data type for these fractions is
mpq_t. For example:
mpq_t quotient;
Chapter 3: MPIR Basics 17
The floating point functions accept and return exponents in the C type mp_exp_t. Currently
this is usually a long, but on some systems it's an int for efficiency.
A limb means the part of a multi-precision number that fits in a single machine word. (We chose
this word because a limb of the human body is analogous to a digit, only larger, and containing
several digits.) Normally a limb is 32 or 64 bits. The C data type for a limb is mp_limb_t.
Counts of limbs are represented in the C type mp_size_t. Currently this is normally a long,
but on some systems it's an int for efficiency.
Counts of bits of a multi-precision number are represented in the C type mp_bitcnt_t. Currently
this is always an unsigned long, but on some systems it will be an unsigned long long in the
future .
Random state means an algorithm selection and current state data. The C data type for such
objects is gmp_randstate_t. For example:
gmp_randstate_t rstate;
Also, in general mp_bitcnt_t is used for bit counts and ranges, and size_t is used for byte or
character counts.
3.3 MPIR on Windows x64
Although Windows x64 is a 64-bit operating system, Microsoft has decided to make long integers
32-bits, which is inconsistent when compared with almost all other 64-bit operating systems.
This has caused many subtle bugs when open source code is ported to Windows x64 because
many developers reasonably expect to find that long integers on a 64-bit operating system will
be 64 bits long.
MPIR contains functions with suffixes of _ui and _si that are used to input unsigned and signed
integers into and convert them for use with MPIR's multiple precision integers (mpz types). For
example, the following functions set an mpz_t integer from unsigned and signed long integers
respectively.
--void mpz_set_ui (mpz_t, unsigned long int) 
--void mpz_set_si (mpz_t, signed long int) 
Also, the following functions obtain unsigned and signed long int values from an MPIR multiple
precision integer (mpz_t).
unsigned long int mpz get ui (mpz_t) 
signed long int mpz get si (mpz_t) 
To bring MPIR on Windows x64 into line with other 64-bit operating systems two new types
have been introduced throughout MPIR:
 * mpir_ui defined as unsigned long int on all but Windows x64, defined as unsigned long
long int on Windows x64
 * mpir_si defined as signed long int on all but Windows x64, defined as signed long long
int on Windows x64
The above prototypes in MPIR 2.6.0 are changed to:
--void mpz_set_ui (mpz_t, mpir_ui) 
--void mpz_set_si (mpz_t, mpir_si) 
--mpir_ui mpz_get_ui (mpz_t) 
--mpir_si mpz_get_si (mpz_t) 
These changes are applied to all MPIR functions with _ui and _si suffixes.
3.4 Function Classes
There are five classes of functions in the MPIR library:
1. Functions for signed integer arithmetic, with names beginning with mpz_. The associated type is mpz_t. 
There are about 150 functions in this class. (see Chapter 5 [Integer Functions], page 29)
2. Functions for rational number arithmetic, with names beginning with mpq_. The associated type is mpq_t. 
There are about 40 functions in this class, but the integer functions can be used for arithmetic on the 
numerator and denominator separately. (see Chapter 6 [Rational Number Functions], page 45)
--3. Functions for floating-point arithmetic, with names beginning with mpf_. The associated type is xxx. 
--There are about 60 functions is this class. (see Chapter 7 [Floating-point Functions], page 49)
4. Fast low-level functions that operate on natural numbers. These are used by the functions
in the preceding groups, and you can also call them directly from very time-critical user
programs. These functions' names begin with mpn_. The associated type is array of mp_
limb_t. There are about 30 (hard-to-use) functions in this class. (see Chapter 8 [Low-level
Functions], page 57)
5. Miscellaneous functions. Functions for setting up custom allocation and functions for generating
random numbers. (see Chapter 13 [Custom Allocation], page 86, and see Chapter 9
[Random Number Functions], page 66)
3.5 Variable Conventions
MPIR functions generally have output arguments before input arguments. This notation is by
analogy with the assignment operator.
MPIR lets you use the same variable for both input and output in one call. For example, the
main function for integer multiplication, mpz_mul, can be used to square x and put the result
back in x with
mpz_mul (x, x, x);
Before you can assign to an MPIR variable, you need to initialize it by calling one of the special
initialization functions. When you're done with a variable, you need to clear it out, using one
of the functions for that purpose. Which function to use depends on the type of variable. See
the chapters on integer functions, rational number functions, and floating-point functions for
details.
A variable should only be initialized once, or at least cleared between each initialization. After
a variable has been initialized, it may be assigned to any number of times.
For efficiency reasons, avoid excessive initializing and clearing. In general, initialize near the
start of a function and clear near the end. For example,
void
Chapter 3: MPIR Basics 19
foo (void)
{
mpz_t n;
int i;
mpz_init (n);
for (i = 1; i < 100; i++)
{
mpz_mul (n, ...);
mpz_fdiv_q (n, ...);
...
}
mpz_clear (n);
}
3.6 Parameter Conventions
When an MPIR variable is used as a function parameter, it's effectively a call-by-reference,
meaning if the function stores a value there it will change the original in the caller. Parameters
which are input-only can be designated const to provoke a compiler error or warning on
attempting to modify them.
When a function is going to return an MPIR result, it should designate a parameter that it sets,
like the library functions do. More than one value can be returned by having more than one
output parameter, again like the library functions. A return of an mpz_t etc doesn't return the
object, only a pointer, and this is almost certainly not what's wanted.
Here's an example accepting an mpz_t parameter, doing a calculation, and storing the result to
the indicated parameter.
void
foo (mpz_t result, const mpz_t param, mpir_ui n)
{
mpir_ui i;
mpz_mul_ui (result, param, n);
for (i = 1; i < n; i++)
mpz_add_ui (result, result, i*7);
}
int
main (void)
{
mpz_t r, n;
mpz_init (r);
mpz_init_set_str (n, "123456", 0);
foo (r, n, 20L);
gmp_printf ("%Zd\n", r);
return 0;
}
foo works even if the mainline passes the same variable for param and result, just like the
library functions. But sometimes it's tricky to make that work, and an application might not
want to bother supporting that sort of thing.
For interest, the MPIR types mpz_t etc are implemented as one-element arrays of certain structures.
This is why declaring a variable creates an object with the fields MPIR needs, but then
20 MPIR 2.7.2
using it as a parameter passes a pointer to the object. Note that the actual fields in each mpz_t
etc are for internal use only and should not be accessed directly by code that expects to be
compatible with future MPIR releases.
3.7 Memory Management
The MPIR types like mpz_t are small, containing only a couple of sizes, and pointers to allocated
data. Once a variable is initialized, MPIR takes care of all space allocation. Additional space is
allocated whenever a variable doesn't have enough.
mpz_t and mpq_t variables never reduce their allocated space. Normally this is the best policy,
since it avoids frequent reallocation. Applications that need to return memory to the heap at
some particular point can use mpz_realloc2, or clear variables no longer needed.
3.8 Reentrancy
MPIR is reentrant and thread-safe, with some exceptions:
 * If configured with --enable-alloca=malloc-notreentrant (or with --enablealloca=
notreentrant when alloca is not available), then naturally MPIR is not
reentrant.
 * mp_set_memory_functions uses global variables to store the selected memory allocation
functions.
 * If the memory allocation functions set by a call to mp_set_memory_functions (or malloc
and friends by default) are not reentrant, then MPIR will not be reentrant either.
 * If the standard I/O functions such as fwrite are not reentrant then the MPIR I/O functions
using them will not be reentrant either.
 * It's safe for two threads to read from the same MPIR variable simultaneously, but it's
not safe for one to read while the another might be writing, nor for two threads to write
simultaneously. It's not safe for two threads to generate a random number from the same
gmp_randstate_t simultaneously, since this involves an update of that variable.
3.9 Useful Macros and Constants
const int mp_bits_per_limb [Global Constant]
The number of bits per limb.
__GNU_MP_VERSION [Macro]
__GNU_MP_VERSION_MINOR [Macro]
__GNU_MP_VERSION_PATCHLEVEL [Macro]
The major and minor GMP version, and patch level, respectively, as integers. For GMP i.j.k,
these numbers will be i, j, and k, respectively. These numbers represent the version of GMP
fully supported by this version of MPIR.
22 MPIR 2.7.2
Reallocations
An mpz_t or mpq_t variable used to hold successively increasing values will have
its memory repeatedly realloced, which could be quite slow or could fragment
memory, depending on the C library. If an application can estimate the final size
then mpz_init2 or mpz_realloc2 can be called to allocate the necessary space from
the beginning (see Section 5.1 [Initializing Integers], page 29).
It doesn't matter if a size set with mpz_init2 or mpz_realloc2 is too small, since all
functions will do a further reallocation if necessary. Badly overestimating memory
required will waste space though.
2exp Functions
It's up to an application to call functions like mpz_mul_2exp when appropriate.
General purpose functions like mpz_mul make no attempt to identify powers of two
or other special forms, because such inputs will usually be very rare and testing
every time would be wasteful.
ui and si Functions
The ui functions and the small number of si functions exist for convenience and
should be used where applicable. But if for example an mpz_t contains a value that
fits in an unsigned long (unsigned long long on Windows x64) there's no need
extract it and call a ui function, just use the regular mpz function.
In-Place Operations
mpz_abs, mpq_abs, mpz_neg, mpq_neg are fast when used for
in-place operations like mpz_abs(x,x), since in the current implementation only a
single field of x needs changing. On suitable compilers (GCC for instance) this is
inlined too.
mpz_add_ui, mpz_sub_ui  benefit from an in-place
operation like mpz_add_ui(x,x,y), since usually only one or two limbs of x will
need to be changed. The same applies to the full precision mpz_add etc if y is small.
If y is big then cache locality may be helped, but that's all.

Divisibility Testing (Small Integers)
mpz_divisible_ui_p and mpz_congruent_ui_p are the best functions for testing
whether an mpz_t is divisible by an individual small integer. They use an algorithm
which is faster than mpz_tdiv_ui, but which gives no useful information about the
actual remainder, only whether it's zero (or a particular value).
However when testing divisibility by several small integers, it's best to take a remainder
modulo their product, to save multi-precision operations. For instance to
test whether a number is divisible by any of 23, 29 or 31 take a remainder modulo
23 * 29 * 31 = 20677 and then test that.
Chapter 3: MPIR Basics 23
The division functions like mpz_tdiv_q_ui which give a quotient as well as a remainder
are generally a little slower than the remainder-only functions like mpz_
tdiv_ui. If the quotient is only rarely wanted then it's probably best to just take
a remainder and then go back and calculate the quotient if and when it's wanted
(mpz_divexact_ui can be used if the remainder is zero).
Rational Arithmetic
The mpq functions operate on mpq_t values with no common factors in the numerator
and denominator. Common factors are checked-for and cast out as necessary. In
general, cancelling factors every time is the best approach since it minimizes the
sizes for subsequent operations.
However, applications that know something about the factorization of the values
they're working with might be able to avoid some of the GCDs used for canonicalization,
or swap them for divisions. For example when multiplying by a prime it's
enough to check for factors of it in the denominator instead of doing a full GCD.
Or when forming a big product it might be known that very little cancellation will
be possible, and so canonicalization can be left to the end.
The mpq_numref and mpq_denref macros give access to the numerator and denominator
to do things outside the scope of the supplied mpq functions. See Section 6.5
[Applying Integer Functions], page 47.
The canonical form for rationals allows mixed-type mpq_t and integer additions or
subtractions to be done directly with multiples of the denominator. This will be
somewhat faster than mpq_add. For example,
/* mpq increment */
mpz_add (mpq_numref(q), mpq_numref(q), mpq_denref(q));
/* mpq += unsigned long */
mpz_addmul_ui (mpq_numref(q), mpq_denref(q), 123UL);
/* mpq -= mpz */
mpz_submul (mpq_numref(q), mpq_denref(q), z);
Number Sequences
Functions like mpz_fac_ui, and mpz_bin_uiui are designed for calculating
isolated values. If a range of values is wanted it's probably best to call to get
a starting point and iterate from there.
Text Input/Output
Hexadecimal or octal are suggested for input or output in text form. Power-of-
2 bases like these can be converted much more efficiently than other bases, like
decimal. For big numbers there's usually nothing of particular interest to be seen
in the digits, so the base doesn't matter much.
Maybe we can hope octal will one day become the normal base for everyday use, as
proposed by King Charles XII of Sweden and later reformers.
3.12 Debugging
Stack Overflow
Depending on the system, a segmentation violation or bus error might be the only
indication of stack overflow. See '--enable-alloca' choices in Section 2.1 [Build
Options], page 3, for how to address this.
In new enough versions of GCC, '-fstack-check' may be able to ensure
an overflow is recognised by the system before too much damage is done, or
24 MPIR 2.7.2
'-fstack-limit-symbol' or '-fstack-limit-register' may be able to add
checking if the system itself doesn't do any (see Section "Options for Code
Generation" in Using the GNU Compiler Collection (GCC)). These options must
be added to the 'CFLAGS' used in the MPIR build (see Section 2.1 [Build Options],
page 3), adding them just to an application will have no effect. Note also they're a
slowdown, adding overhead to each function call and each stack allocation.
Heap Problems
The most likely cause of application problems with MPIR is heap corruption. Failing
to init MPIR variables will have unpredictable effects, and corruption arising
elsewhere in a program may well affect MPIR. Initializing MPIR variables more
than once or failing to clear them will cause memory leaks.
In all such cases a malloc debugger is recommended. On a GNU or BSD system
the standard C library malloc has some diagnostic facilities, see Section "Allocation
Debugging" in The GNU C Library Reference Manual, or 'man 3 malloc'. Other
possibilities, in no particular order, include
http://dmalloc.com/
http://www.perens.com/FreeSoftware/ (electric fence)
http://www.gnupdate.org/components/leakbug/
http://wwww.gnome.org/projects/memprof
The MPIR default allocation routines in memory.c also have a simple sentinel scheme
which can be enabled with #define DEBUG in that file. This is mainly designed for
detecting buffer overruns during MPIR development, but might find other uses.
Stack Backtraces
On some systems the compiler options MPIR uses by default can interfere with
debugging. In particular on x86 and 68k systems '-fomit-frame-pointer' is used
and this generally inhibits stack backtracing. Recompiling without such options
may help while debugging, though the usual caveats about it potentially moving a
memory problem or hiding a compiler bug will apply.
GDB, the GNU Debugger
A sample .gdbinit is included in the distribution, showing how to call some undocumented
dump functions to print MPIR variables from within GDB. Note that these
functions shouldn't be used in final application code since they're undocumented
and may be subject to incompatible changes in future versions of MPIR.
Source File Paths
MPIR has multiple source files with the same name, in different directories. For
example mpz, mpq and mpf each have an init.c. If the debugger can't already
determine the right one it may help to build with absolute paths on each C file.
One way to do that is to use a separate object directory with an absolute path to
the source directory.
cd /my/build/dir
/my/source/dir/gmp-2.7.2/configure
This works via VPATH, and might require GNU make. Alternately it might be possible
to change the .c.lo rules appropriately.
Assertion Checking
The build option --enable-assert is available to add some consistency checks to
the library (see Section 2.1 [Build Options], page 3). These are likely to be of limited
value to most applications. Assertion failures are just as likely to indicate memory
corruption as a library or compiler bug.
Applications using the low-level mpn functions, however, will benefit from --enableassert
since it adds checks on the parameters of most such functions, many of which
Chapter 3: MPIR Basics 25
have subtle restrictions on their usage. Note however that only the generic C code
has checks, not the assembler code, so CPU 'none' should be used for maximum
checking.
Temporary Memory Checking
--The build option --enable-alloca=debug arranges that each block of temporary
--memory in MPIR is allocated with a separate call to malloc (or the allocation
--function set with mp_set_memory_functions).
This can help a malloc debugger detect accesses outside the intended bounds, or
detect memory not released. In a normal build, on the other hand, temporary
memory is allocated in blocks which MPIR divides up for its own use, or may be
allocated with a compiler builtin alloca which will go nowhere near any malloc
debugger hooks.
3.13 Profiling
Running a program under a profiler is a good way to find where it's spending most time and
where improvements can be best sought. The profiling choices for a MPIR build are as follows.
'--disable-profiling'
The default is to add nothing special for profiling.
It should be possible to just compile the mainline of a program with -p and use prof
to get a profile consisting of timer-based sampling of the program counter. Most of
the MPIR assembler code has the necessary symbol information.
This approach has the advantage of minimizing interference with normal program
operation, but on most systems the resolution of the sampling is quite low (10
milliseconds for instance), requiring long runs to get accurate information.
26 MPIR 2.7.2
'--enable-profiling=prof'
Build with support for the system prof, which means '-p' added to the 'CFLAGS'.
This provides call counting in addition to program counter sampling, which allows
the most frequently called routines to be identified, and an average time spent in
each routine to be determined.
The x86 assembler code has support for this option, but on other processors the
assembler routines will be as if compiled without '-p' and therefore won't appear in
the call counts.
On some systems, such as GNU/Linux, '-p' in fact means '-pg' and in this case
'--enable-profiling=gprof' described below should be used instead.
'--enable-profiling=gprof'

3.15 Emacs
C-h C-i (info-lookup-symbol) is a good way to find documentation on C functions while
editing (see Section "Info Documentation Lookup" in The Emacs Editor).
The MPIR manual can be included in such lookups by putting the following in your .emacs,
(eval-after-load "info-look"
'(let ((mode-value (assoc 'c-mode (assoc 'symbol info-lookup-alist))))
(setcar (nthcdr 3 mode-value)
(cons '("(gmp)Function Index" nil "^ -.* " "\\>")
(nth 3 mode-value)))))

5 Integer Functions
This chapter describes the MPIR functions for performing integer arithmetic. These functions
start with the prefix mpz_.
MPIR integers are stored in objects of type mpz_t.
5.1 Initialization Functions
The functions for integer arithmetic assume that all integer objects are initialized. You do that
by calling the function mpz_init. For example,
{
mpz_t integ;
mpz_init (integ);
...
mpz_add (integ, ...);
...
mpz_sub (integ, ...);
/* Unless the program is about to exit, do ... */
mpz_clear (integ);
}
As you can see, you can store new values any number of times, once an object is initialized.
--void mpz_init (mpz_t integer) 
--Initialize integer, and set its value to 0.
void mpz_realloc2 (mpz_t integer, mp_bitcnt_t n) 
Change the space allocated for integer to n bits. The value in integer is preserved if it fits,
or is set to 0 if not.
This function can be used to increase the space for a variable in order to avoid repeated
automatic reallocations, or to decrease it to give memory back to the heap.
5.2 Assignment Functions
These functions assign new values to already initialized integers (see Section 5.1 [Initializing
Integers], page 29).
void mpz_set_ux (mpz_t rop, uintmax_t op) 
void mpz_set_sx (mpz_t rop, intmax_t op) 
void mpz_set_q (mpz_t rop, mpq_t op) 
Set the value of rop from op. Note the intmax versions are only available if you include the
stdint.h header before including mpir.h.
mpz_set_q truncate op to make it an integer.
void mpz_swap (mpz_t rop1, mpz_t rop2) 
Swap the values rop1 and rop2 efficiently.
5.3 Combined Initialization and Assignment Functions
For convenience, MPIR provides a parallel series of initialize-and-set functions which initialize
the output and then store the value there. These functions' names have the form mpz_init_
set...
Here is an example of using one:
{
mpz_t pie;
mpz_init_set_str (pie, "3141592653589793238462643383279502884", 10);
...
mpz_sub (pie, ...);
...
mpz_clear (pie);
}
Once the integer has been initialized by any of the mpz_init_set... functions, it can be used
as the source or destination operand for the ordinary integer functions. Don't use an initializeand-
set function on a variable already initialized!
Chapter 5: Integer Functions 31
void mpz_init_set_ux (mpz_t rop, uintmax_t op) 
void mpz_init_set_sx (mpz_t rop, intmax_t op) 
Initialize rop with limb space and set the initial numeric value from op. Note the intmax
versions are only available if you include the stdint.h header before including mpir.h.
--int mpz_init_set_str (mpz_t rop, char *str, int base) 
5.4 Conversion Functions
This section describes functions for converting MPIR integers to standard C types. Functions
for converting to MPIR integers are described in Section 5.2 [Assigning Integers], page 30 and
Section 5.12 [I/O of Integers], page 40.
--mpir_ui mpz_get_ui (mpz_t op) 
--Return the value of op as an mpir_ui.
--If op is too big to fit an mpir_ui then just the least significant bits that do fit are returned.
--The sign of op is ignored, only the absolute value is used.
--mpir_si mpz_get_si (mpz_t op) 
--If op fits into a mpir_si return the value of op. Otherwise return the least significant part
--of op, with the same sign as op.
--If op is too big to fit in a mpir_si, the returned result is probably not very useful. To find
--out if the value will fit, use the function mpz_fits_slong_p.
uintmax_t mpz_get_ux (mpz_t op) 
Return the value of op as an uintmax_t.
If op is too big to fit an uintmax_t then just the least significant bits that do fit are returned.
The sign of op is ignored, only the absolute value is used. Note this function is only available
if you include stdint.h before including mpir.h.
intmax_t mpz_get_sx (mpz_t op) 
If op fits into a intmax_t return the value of op. Otherwise return the least significant part
of op, with the same sign as op.
If op is too big to fit in a intmax_t, the returned result is probably not very useful. Note
this function is only available if you include the stdint.h header before including mpir.h.
double mpz_get_d (mpz_t op) 
Convert op to a double, truncating if necessary (ie. rounding towards zero).
If the exponent from the conversion is too big, the result is system dependent. An infinity is
returned where available. A hardware overflow trap may or may not occur.

double mpz_get_d_2exp (mpir_si *exp, mpz_t op) 
Convert op to a double, truncating if necessary (ie. rounding towards zero), and returning
the exponent separately.
The return value is in the range 0.5 <= |d| < 1 and the exponent is stored to *exp. d * 2^exp is
the (truncated) op value. If op is zero, the return is 0:0 and 0 is stored to *exp.
This is similar to the standard C frexp function (see Section "Normalization Functions" in
The GNU C Library Reference Manual).

5.5 Arithmetic Functions
--void mpz_ui_sub (mpz_t rop, mpir_ui op1, mpz_t op2) 
--Set rop to op1 - op2.
void mpz_addmul (mpz_t rop, mpz_t op1, mpz_t op2) 
--void mpz_addmul_ui (mpz_t rop, mpz_t op1, mpir_ui op2) 
Set rop to rop + op1 * op2.
void mpz_submul (mpz_t rop, mpz_t op1, mpz_t op2) 
void mpz_submul_ui (mpz_t rop, mpz_t op1, mpir_ui op2) 
Set rop to rop - op1 * op2.
--void mpz_neg (mpz_t rop, mpz_t op) 
--Set rop to -op.
--void mpz_abs (mpz_t rop, mpz_t op) 
--Set rop to the absolute value of op.
5.6 Division Functions
--void mpz_cdiv_q (mpz_t q, mpz_t n, mpz_t d) 
void mpz_cdiv_r (mpz_t r, mpz_t n, mpz_t d) 
void mpz_cdiv_qr (mpz_t q, mpz_t r, mpz_t n, mpz_t d) 
mpir_ui mpz_cdiv_q_ui (mpz_t q, mpz_t n, mpir_ui d) 
mpir_ui mpz_cdiv_r_ui (mpz_t r, mpz_t n, mpir_ui d) 
mpir_ui mpz_cdiv_qr_ui (mpz_t q, mpz_t r, mpz_t n, mpir_ui d) 
mpir_ui mpz_cdiv_ui (mpz_t n, mpir_ui d) 
void mpz_cdiv_q_2exp (mpz_t q, mpz_t n, mp_bitcnt_t b) 
void mpz_cdiv_r_2exp (mpz_t r, mpz_t n, mp_bitcnt_t b) 
--void mpz_fdiv_q (mpz_t q, mpz_t n, mpz_t d) 
--void mpz_fdiv_r (mpz_t r, mpz_t n, mpz_t d) 
void mpz_fdiv_qr (mpz_t q, mpz_t r, mpz_t n, mpz_t d) 
--mpir_ui mpz_fdiv_q_ui (mpz_t q, mpz_t n, mpir_ui d) 
>mpir_ui mpz_fdiv_r_ui (mpz_t r, mpz_t n, mpir_ui d) 
mpir_ui mpz_fdiv_qr_ui (mpz_t q, mpz_t r, mpz_t n, mpir_ui d) 
--mpir_ui mpz_fdiv_ui (mpz_t n, mpir_ui d) 
void mpz_fdiv_r_2exp (mpz_t r, mpz_t n, mp_bitcnt_t b) 
void mpz_tdiv_q (mpz_t q, mpz_t n, mpz_t d) 
void mpz_tdiv_r (mpz_t r, mpz_t n, mpz_t d) 
void mpz_tdiv_qr (mpz_t q, mpz_t r, mpz_t n, mpz_t d) 
mpir_ui mpz_tdiv_q_ui (mpz_t q, mpz_t n, mpir_ui d) 
mpir_ui mpz_tdiv_r_ui (mpz_t r, mpz_t n, mpir_ui d) 
mpir_ui mpz_tdiv_qr_ui (mpz_t q, mpz_t r, mpz_t n, mpir_ui d) 
mpir_ui mpz_tdiv_ui (mpz_t n, mpir_ui d) 
--void mpz_tdiv_q_2exp (mpz_t q, mpz_t n, mp_bitcnt_t b) 
--void mpz_tdiv_r_2exp (mpz_t r, mpz_t n, mp_bitcnt_t b) 
Divide n by d, forming a quotient q and/or remainder r. For the 2exp functions, d = 2b. The
rounding is in three styles, each suiting different applications.
 * cdiv rounds q up towards +inf, and r will have the opposite sign to d. The c stands for
"ceil".
 * fdiv rounds q down towards -inf, and r will have the same sign as d. The f stands for
"floor".
 * tdiv rounds q towards zero, and r will have the same sign as n. The t stands for
"truncate".
In all cases q and r will satisfy n = qd + r, and r will satisfy 0 <= |r| < |d|.
34 MPIR 2.7.2
The q functions calculate only the quotient, the r functions only the remainder, and the qr
functions calculate both. Note that for qr the same variable cannot be passed for both q and
r, or results will be unpredictable.
For the ui variants the return value is the remainder, and in fact returning the remainder is
all the div_ui functions do. For tdiv and cdiv the remainder can be negative, so for those
the return value is the absolute value of the remainder.
For the 2exp variants the divisor is 2b. These functions are implemented as right shifts and
bit masks, but of course they round the same as the other functions.
--For positive n mpz_tdiv_q_2exp is a simple bitwise right shift.
--For negative n mpz_tdiv_q_2exp effectively treats n as sign and magnitude.
--void mpz_mod (mpz_t r, mpz_t n, mpz_t d) 
--mpir_ui mpz_mod_ui (mpz_t r, mpz_t n, mpir_ui d) 
--Set r to n mod d. The sign of the divisor is ignored; the result is always non-negative.
--mpz_mod_ui is identical to mpz_fdiv_r_ui above, returning the remainder as well as setting
--r. See mpz_fdiv_ui above if only the return value is wanted.
void mpz_divexact (mpz_t q, mpz_t n, mpz_t d) 
void mpz_divexact_ui (mpz_t q, mpz_t n, mpir_ui d) 
Set q to n/d. These functions produce correct results only when it is known in advance that
d divides n.
These routines are much faster than the other division functions, and are the best choice
when exact division is known to occur, for example reducing a rational to lowest terms.
--int mpz_divisible_p (mpz_t n, mpz_t d) 
--int mpz_divisible_ui_p (mpz_t n, mpir_ui d) 
--int mpz_divisible_2exp_p (mpz_t n, mp_bitcnt_t b) 
--Return non-zero if n is exactly divisible by d, or in the case of mpz_divisible_2exp_p by 2b.
--n is divisible by d if there exists an integer q satisfying n = qd. Unlike the other division
--functions, d = 0 is accepted and following the rule it can be seen that only 0 is considered
--divisible by 0.
int mpz_congruent_p (mpz_t n, mpz_t c, mpz_t d) 
int mpz_congruent_ui_p (mpz_t n, mpir_ui c, mpir_ui d) 
int mpz_congruent_2exp_p (mpz_t n, mpz_t c, mp_bitcnt_t b) 
Return non-zero if n is congruent to c modulo d, or in the case of mpz_congruent_2exp_p
modulo 2b.
n is congruent to c mod d if there exists an integer q satisfying n = c +qd. Unlike the other
division functions, d = 0 is accepted and following the rule it can be seen that n and c are
considered congruent mod 0 only when exactly equal.
Chapter 5: Integer Functions 35
5.7 Exponentiation Functions
--void mpz_pow_ui (mpz_t rop, mpz_t base, mpir_ui exp) 
--Set rop to base^exp
5.8 Root Extraction Functions
--int mpz_root (mpz_t rop, mpz_t op, mpir_ui n) 
--Set rop to the truncated integer part of the nth root of op. Return non-zero if the
--computation was exact, i.e., if op is rop to the nth power.
--void mpz_nthroot (mpz_t rop, mpz_t op, mpir_ui n) 
--Set rop to the truncated integer part of the nth root of op.
void mpz_rootrem (mpz_t root, mpz_t rem, mpz_t u, mpir_ui n) 
Set root to the truncated integer part of the nth root of u. Set rem to the remainder, (u - root^n).
--void mpz_sqrt (mpz_t rop, mpz_t op) 
--Set rop to the truncated integer part of the square root of op.
--void mpz_sqrtrem (mpz_t rop1, mpz_t rop2, mpz_t op) 
--Set rop1 like mpz_sqrt and rop2 to the remainder (op - rop1^2), which will be zero if op is a perfect square.
--If rop1 and rop2 are the same variable, the results are undefined.
int mpz_perfect_power_p (mpz_t op) 
Return non-zero if op is a perfect power, i.e., if there exist integers a and b, with b > 1, such that op = ab.
Under this definition both 0 and 1 are considered to be perfect powers. Negative values of
op are accepted, but of course can only be odd perfect powers.
int mpz_perfect_square_p (mpz_t op) 
Return non-zero if op is a perfect square, i.e., if the square root of op is an integer. Under
this definition both 0 and 1 are considered to be perfect squares.

5.9 Number Theoretic Functions

--void mpz_gcd (mpz_t rop, mpz_t op1, mpz_t op2) 
--Set rop to the greatest common divisor of op1 and op2. The result is always positive even if
--one or both input operands are negative.
--mpir_ui mpz_gcd_ui (mpz_t rop, mpz_t op1, mpir_ui op2) 
--Compute the greatest common divisor of op1 and op2. If rop is not NULL, store the result
--there.
--If the result is small enough to fit in an mpir_ui, it is returned. If the result does not fit, 0
--is returned, and the result is equal to the argument op1. Note that the result will always fit
--if op2 is non-zero.

void mpz_gcdext (mpz_t g, mpz_t s, mpz_t t, const mpz_t a, const mpz_t b) 
Set g to the greatest common divisor of a and b, and in addition set s and t to coefficients
satisfying as + bt = g. The value in g is always positive, even if one or both of a and b
are negative (or zero if both inputs are zero). The values in s and t are chosen such that
normally, |s| < |b|/(2g) and |t| < |a|/(2g), and these relations define s and t uniquely. There
are a few exceptional cases:
If |a| = |b|, then s = 0, t = sgn(b).
Otherwise, s = sgn(a) if b = 0 or |b| = 2g, and t = sgn(b) if a = 0 or |a| = 2g.
In all cases, s = 0 if and only if g = |b|, i.e., if b divides a or a = b = 0.
If t is NULL then that value is not computed.

--void mpz_lcm (mpz_t rop, mpz_t op1, mpz_t op2) 
--void mpz_lcm_ui (mpz_t rop, mpz_t op1, mpir_ui op2) 
--Set rop to the least common multiple of op1 and op2. rop is always positive, irrespective of
--the signs of op1 and op2. rop will be zero if either op1 or op2 is zero.
int mpz_invert (mpz_t rop, mpz_t op1, mpz_t op2) 
Compute the inverse of op1 modulo op2 and put the result in rop. If the inverse exists, the
return value is non-zero and rop will satisfy 0 <= rop < op2. If an inverse doesn't exist the
return value is zero and rop is undefined.
int mpz_jacobi (mpz_t a, mpz_t b) 
Calculate the Jacobi symbol (a over b). This is defined only for b odd.
int mpz_legendre (mpz_t a, mpz_t p) 
Calculate the Legendre symbol (a over p). This is defined only for p an odd positive prime, and
for such p it's identical to the Jacobi symbol.
38 MPIR 2.7.2
int mpz_kronecker (mpz_t a, mpz_t b) 
int mpz_kronecker_si (mpz_t a, mpir_si b) 
int mpz_kronecker_ui (mpz_t a, mpir_ui b) 
int mpz_si_kronecker (mpir_si a, mpz_t b) 
int mpz_ui_kronecker (mpir_ui a, mpz_t b) 
Calculate the Jacobi symbol (a over b) with the Kronecker extension
(a over 2) = (2 over a) when a odd, or (a ove 2) = 0 when a even.
When b is odd the Jacobi symbol and Kronecker symbol are identical, so mpz_kronecker_ui
etc can be used for mixed precision Jacobi symbols too.
For more information see Henri Cohen section 1.4.2 (see Appendix B [References], page 125),
or any number theory textbook. See also the example program demos/qcn.c which uses
mpz_kronecker_ui on the MPIR website.
--mp_bitcnt_t mpz_remove (mpz_t rop, mpz_t op, mpz_t f) 
--Remove all occurrences of the factor f from op and store the result in rop. The return value
--is how many such occurrences were removed.
void mpz_fac_ui (mpz_t rop, unsigned long int n) 
void mpz_2fac_ui (mpz_t rop, unsigned long int n) 
void mpz_mfac_uiui (mpz_t rop, unsigned long int n, unsigned long int m) 
Set rop to the factorial of n: mpz_fac_ui computes the plain factorial n!, mpz_2fac_ui
computes the double-factorial n!!, and mpz_mfac_uiui the m-multi-factorial n!(m).
void mpz_primorial_ui (mpz_t rop, unsigned long int n) 
Set rop to the primorial of n, i.e. the product of all positive prime numbers <= n.
void mpz_bin_ui (mpz_t rop, mpz_t n, mpir_ui k) 
void mpz_bin_uiui (mpz_t rop, mpir_ui n, mpir_ui k) 
Compute the binomial coefficient (n over k) and store the result in rop. Negative values of n are
supported by mpz_bin_ui, using the identity (-n over k) = (-1)^k*(n+k-1 over k), see Knuth volume 1
section 1.2.6 part G.
--void mpz_fib_ui (mpz_t fn, mpir_ui n) 
--void mpz_fib2_ui (mpz_t fn, mpz_t fnsub1, mpir_ui n) 
--mpz_fib_ui sets fn to to Fn, the n'th Fibonacci number. mpz_fib2_ui sets fn to Fn, and
--fnsub1 to Fn-1.
--These functions are designed for calculating isolated Fibonacci numbers. When a sequence of
--values is wanted it's best to start with mpz_fib2_ui and iterate the defining Fn+1 = Fn+Fn-1
--or similar.
void mpz_lucnum_ui (mpz_t ln, mpir_ui n) 
void mpz_lucnum2_ui (mpz_t ln, mpz_t lnsub1, mpir_ui n) 
mpz_lucnum_ui sets ln to to Ln, the n'th Lucas number. mpz_lucnum2_ui sets ln to Ln, and
lnsub1 to Ln-1.
These functions are designed for calculating isolated Lucas numbers. When a sequence of
values is wanted it's best to start with mpz_lucnum2_ui and iterate the defining Ln+1 =
Ln + Ln-1 or similar.
The Fibonacci numbers and Lucas numbers are related sequences, so it's never necessary
to call both mpz_fib2_ui and mpz_lucnum2_ui. The formulas for going from Fibonacci to
Chapter 5: Integer Functions 39
Lucas can be found in Section 15.7.5 [Lucas Numbers Algorithm], page 109, the reverse is
straightforward too.
5.10 Comparison Functions
int mpz_cmp_d (mpz_t op1, double op2) 
--Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, or a negative
--value if op1 < op2.
mpz_cmp_d can be called with an infinity, but results are undefined for a NaN.
int mpz_cmpabs (mpz_t op1, mpz_t op2) 
int mpz_cmpabs_d (mpz_t op1, double op2) 
int mpz_cmpabs_ui (mpz_t op1, mpir_ui op2) 
Compare the absolute values of op1 and op2. Return a positive value if |op1| > |op2|, zero
if |op1| = |op2|, or a negative value if |op1| < |op2|.
mpz_cmpabs_d can be called with an infinity, but results are undefined for a NaN.
int mpz_sgn (mpz_t op) [Macro]
Return +1 if op > 0, 0 if op = 0, and -1 if op < 0.
This function is actually implemented as a macro. It evaluates its argument multiple times.
5.11 Logical and Bit Manipulation Functions
These functions behave as if twos complement arithmetic were used (although sign-magnitude
is the actual implementation). The least significant bit is number 0.
void mpz_and (mpz_t rop, mpz_t op1, mpz_t op2) 
Set rop to op1 bitwise-and op2.
void mpz_ior (mpz_t rop, mpz_t op1, mpz_t op2) 
Set rop to op1 bitwise inclusive-or op2.
void mpz_xor (mpz_t rop, mpz_t op1, mpz_t op2) 
Set rop to op1 bitwise exclusive-or op2.
void mpz_com (mpz_t rop, mpz_t op) 
Set rop to the one's complement of op.
mp_bitcnt_t mpz_popcount (mpz_t op) 
If op >= 0, return the population count of op, which is the number of 1 bits in the binary
representation. If op < 0, the number of 1s is infinite, and the return value is ULONG MAX,
the largest possible mp_bitcnt_t.
mp_bitcnt_t mpz_hamdist (mpz_t op1, mpz_t op2) 
If op1 and op2 are both >= 0 or both < 0, return the hamming distance between the two
operands, which is the number of bit positions where op1 and op2 have different bit values.
If one operand is >= 0 and the other < 0 then the number of bits different is infinite, and the
return value is the largest possible imp_bitcnt_t.
40 MPIR 2.7.2
--mp_bitcnt_t mpz_scan0 (mpz_t op, mp_bitcnt_t starting_bit) 
--mp_bitcnt_t mpz_scan1 (mpz_t op, mp_bitcnt_t starting_bit) 
--Scan op, starting from bit starting bit, towards more significant bits, until the first 0 or 1 bit
--(respectively) is found. Return the index of the found bit.
--If the bit at starting bit is already what's sought, then starting bit is returned.
--If there's no bit found, then the largest possible mp_bitcnt_t is returned. This will happen
--in mpz_scan0 past the end of a positive number, or mpz_scan1 past the end of a nonnegative
--number.
void mpz_setbit (mpz_t rop, mp_bitcnt_t bit_index) 
Set bit bit_index in rop.
void mpz_clrbit (mpz_t rop, mp_bitcnt_t bit_index) 
Clear bit bit_index in rop.
void mpz_combit (mpz_t rop, mp_bitcnt_t bit_index) 
Complement bit bit_index in rop.
--int mpz_tstbit (mpz_t op, mp_bitcnt_t bit_index) 
--Test bit bit index in op and return 0 or 1 accordingly.
5.12 Input and Output Functions
Functions that perform input from a stdio stream, and functions that output to a stdio stream.
Passing a NULL pointer for a stream argument to any of these functions will make them read
from stdin and write to stdout, respectively.
When using any of these functions, it is a good idea to include stdio.h before mpir.h, since
that will allow mpir.h to define prototypes for these functions.
size_t mpz_out_str (FILE *stream, int base, mpz_t op) 
Output op on stdio stream stream, as a string of digits in base base. The base argument may
vary from 2 to 62 or from -2 to -36.
For base in the range 2..36, digits and lower-case letters are used; for -2..-36, digits and
upper-case letters are used; for 37..62, digits, upper-case letters, and lower-case letters (in
that significance order) are used.
Return the number of bytes written, or if an error occurred, return 0.
size_t mpz_inp_str (mpz_t rop, FILE *stream, int base) 
Input a possibly white-space preceded string in base base from stdio stream stream, and put
the read integer in rop.
The base may vary from 2 to 62, or if base is 0, then the leading characters are used: 0x and
0X for hexadecimal, 0b and 0B for binary, 0 for octal, or decimal otherwise.
For bases up to 36, case is ignored; upper-case and lower-case letters have the same value. For
bases 37 to 62, upper-case letter represent the usual 10..35 while lower-case letter represent
36..61.
Return the number of bytes read, or if an error occurred, return 0.
Chapter 5: Integer Functions 41
size_t mpz_out_raw (FILE *stream, mpz_t op) 
Output op on stdio stream stream, in raw binary format. The integer is written in a portable
format, with 4 bytes of size information, and that many bytes of limbs. Both the size and
the limbs are written in decreasing significance order (i.e., in big-endian).
The output can be read with mpz_inp_raw.
Return the number of bytes written, or if an error occurred, return 0.
The output of this can not be read by mpz_inp_raw from GMP 1, because of changes necessary
for compatibility between 32-bit and 64-bit machines.
size_t mpz_inp_raw (mpz_t rop, FILE *stream) 
Input from stdio stream stream in the format written by mpz_out_raw, and put the result in
rop. Return the number of bytes read, or if an error occurred, return 0.
This routine can read the output from mpz_out_raw also from GMP 1, in spite of changes
necessary for compatibility between 32-bit and 64-bit machines.
5.13 Random Number Functions
The random number functions of MPIR come in two groups; older function that rely on a global
state, and newer functions that accept a state parameter that is read and modified. Please see
the Chapter 9 [Random Number Functions], page 66 for more information on how to use and
not to use random number functions.
void mpz_urandomb (mpz_t rop, gmp_randstate_t state, mp_bitcnt_t n) 
Generate a uniformly distributed random integer in the range 0 to 2n - 1, inclusive.
The variable state must be initialized by calling one of the gmp_randinit functions
(Section 9.1 [Random State Initialization], page 66) before invoking this function.
--void mpz_urandomm (mpz_t rop, gmp_randstate_t state, mpz_t n) 
--Generate a uniform random integer in the range 0 to n - 1, inclusive.
--The variable state must be initialized by calling one of the gmp_randinit functions
--(Section 9.1 [Random State Initialization], page 66) before invoking this function.
void mpz_rrandomb (mpz_t rop, gmp_randstate_t state, mp_bitcnt_t n) 
Generate a random integer with long strings of zeros and ones in the binary representation.
Useful for testing functions and algorithms, since this kind of random numbers have proven
to be more likely to trigger corner-case bugs. The random number will be in the range 0 to
2n - 1, inclusive.
The variable state must be initialized by calling one of the gmp_randinit functions
(Section 9.1 [Random State Initialization], page 66) before invoking this function.
5.14 Integer Import and Export
mpz_t variables can be converted to and from arbitrary words of binary data with the following
functions.
--void mpz_import (mpz_t rop, size t count, int order, size t size, int endian, size t nails, const void *op)
--Set rop from an array of word data at op.
--The parameters specify the format of the data. count many words are read, each size bytes.
--order can be 1 for most significant word first or -1 for least significant first. Within each
--word endian can be 1 for most significant byte first, -1 for least significant first, or 0 for the
--native endianness of the host CPU. The most significant nails bits of each word are skipped,
--this can be 0 to use the full words.
--There is no sign taken from the data, rop will simply be a positive integer. An application
--can handle any sign itself, and apply it for instance with mpz_neg.
--There are no data alignment restrictions on op, any address is allowed.
--Here's an example converting an array of mpir_ui data, most significant element first, and
--host byte order within each value.
--mpir_ui a[20];
--mpz_t z;
--mpz_import (z, 20, 1, sizeof(a[0]), 0, 0, a);
--This example assumes the full sizeof bytes are used for data in the given type, which is
--usually true, and certainly true for mpir_ui everywhere we know of. However on Cray vector
--systems it may be noted that short and int are always stored in 8 bytes (and with sizeof
--indicating that) but use only 32 or 46 bits. The nails feature can account for this, by passing
--for instance 8*sizeof(int)-INT_BIT.
--void * mpz_export (void *rop, size t *countp, int order, size t size, int endian, size t nails, mpz_t op)
--Fill rop with word data from op.
--The parameters specify the format of the data produced. Each word will be size bytes and
--order can be 1 for most significant word first or -1 for least significant first. Within each
--word endian can be 1 for most significant byte first, -1 for least significant first, or 0 for the
--native endianness of the host CPU. The most significant nails bits of each word are unused
--and set to zero, this can be 0 to produce full words.
--The number of words produced is written to *countp, or countp can be NULL to discard the
--count. rop must have enough space for the data, or if rop is NULL then a result array of
--the necessary size is allocated using the current MPIR allocation function (see Chapter 13
--[Custom Allocation], page 86). In either case the return value is the destination used, either
--rop or the allocated block.
--If op is non-zero then the most significant word produced will be non-zero. If op is zero then
--the count returned will be zero and nothing written to rop. If rop is NULL in this case, no
--block is allocated, just NULL is returned.
--The sign of op is ignored, just the absolute value is exported. An application can use mpz_sgn
--to get the sign and handle it as desired. (see Section 5.10 [Integer Comparisons], page 39)
--There are no data alignment restrictions on rop, any address is allowed.
--When an application is allocating space itself the required size can be determined with a
--calculation like the following. Since mpz_sizeinbase always returns at least 1, count here
--will be at least one, which avoids any portability problems with malloc(0), though if z is
--zero no space at all is actually needed (or written).
--numb = 8*size - nail;
--count = (mpz_sizeinbase (z, 2) + numb-1) / numb;
--p = malloc (count * size);
--
5.15 Miscellaneous Functions
--int mpz_fits_ulong_p (mpz_t op) 
--int mpz_fits_slong_p (mpz_t op) 
int mpz_fits_uint_p (mpz_t op) 
int mpz_fits_sint_p (mpz_t op) 
int mpz_fits_ushort_p (mpz_t op) 
int mpz_fits_sshort_p (mpz_t op) 
Return non-zero iff the value of op fits in an unsigned long, long, unsigned int, signed
int, unsigned short int, or signed short int, respectively. Otherwise, return zero.
--int mpz_odd_p (mpz_t op) [Macro]
--int mpz_even_p (mpz_t op) [Macro]
--Determine whether op is odd or even, respectively. Return non-zero if yes, zero if no. These
--macros evaluate their argument more than once.

5.16 Special Functions
The functions in this section are for various special purposes. Most applications will not need
them.
mp_limb_t mpz_getlimbn (mpz_t op, mp_size_t n) 
Return limb number n from op. The sign of op is ignored, just the absolute value is used.
The least significant limb is number 0.
mpz_size can be used to find how many limbs make up op. mpz_getlimbn returns zero if n
is outside the range 0 to mpz_size(op)-1.
--size_t mpz_size (mpz_t op) 
--Return the size of op measured in number of limbs. If op is zero, the returned value will be
--zero.
Chapter 6: Rational Number Functions 45
6 Rational Number Functions
This chapter describes the MPIR functions for performing arithmetic on rational numbers. These
functions start with the prefix mpq_.
Rational numbers are stored in objects of type mpq_t.
All rational arithmetic functions assume operands have a canonical form, and canonicalize their
result. The canonical from means that the denominator and the numerator have no common
factors, and that the denominator is positive. Zero has the unique representation 0/1.
Pure assignment functions do not canonicalize the assigned variable. It is the responsibility of
the user to canonicalize the assigned variable before any arithmetic operations are performed on
that variable.
void mpq_canonicalize (mpq_t op) 
Remove any factors that are common to the numerator and denominator of op, and make
the denominator positive.
6.1 Initialization and Assignment Functions
void mpq_init (mpq_t dest_rational) 
Initialize dest rational and set it to 0/1. Each variable should normally only be initialized
once, or at least cleared out (using the function mpq_clear) between each initialization.
void mpq_inits (mpq_t x, ...) 
Initialize a NULL-terminated list of mpq_t variables, and set their values to 0/1.
void mpq_clear (mpq_t rational_number) 
Free the space occupied by rational number. Make sure to call this function for all mpq_t
variables when you are done with them.
void mpq_clears (mpq_t x, ...) 
Free the space occupied by a NULL-terminated list of mpq_t variables.
void mpq_set (mpq_t rop, mpq_t op) 
void mpq_set_z (mpq_t rop, mpz_t op) 
Assign rop from op.
void mpq_set_ui (mpq_t rop, mpir_ui op1, mpir_ui op2) 
void mpq_set_si (mpq_t rop, mpir_si op1, mpir_ui op2) 
Set the value of rop to op1/op2. Note that if op1 and op2 have common factors, rop has to
be passed to mpq_canonicalize before any operations are performed on rop.
int mpq_set_str (mpq_t rop, char *str, int base) 
Set rop from a null-terminated string str in the given base.
The string can be an integer like "41" or a fraction like "41/152". The fraction must be
in canonical form (see Chapter 6 [Rational Number Functions], page 45), or if not then
mpq_canonicalize must be called.
The numerator and optional denominator are parsed the same as in mpz_set_str (see
Section 5.2 [Assigning Integers], page 30). White space is allowed in the string, and is simply
ignored. The base can vary from 2 to 62, or if base is 0 then the leading characters are used:
46 MPIR 2.7.2
0x or 0X for hex, 0b or 0B for binary, 0 for octal, or decimal otherwise. Note that this is done
separately for the numerator and denominator, so for instance 0xEF/100 is 239/100, whereas
0xEF/0x100 is 239/256.
The return value is 0 if the entire string is a valid number, or -1 if not.
void mpq_swap (mpq_t rop1, mpq_t rop2) 
Swap the values rop1 and rop2 efficiently.
6.2 Conversion Functions
--double mpq_get_d (mpq_t op) 
--Convert op to a double, truncating if necessary (ie. rounding towards zero).
--If the exponent from the conversion is too big or too small to fit a double then the result is
--system dependent. For too big an infinity is returned when available. For too small 0:0 is
--normally returned. Hardware overflow, underflow and denorm traps may or may not occur.
void mpq_set_d (mpq_t rop, double op) 
Set rop to the value of op. There is no rounding, this conversion is exact.
char * mpq_get_str (char *str, int base, mpq_t op) 
Convert op to a string of digits in base base. The base may vary from 2 to 36. The string
will be of the form 'num/den', or if the denominator is 1 then just 'num'.
If str is NULL, the result string is allocated using the current allocation function (see
Chapter 13 [Custom Allocation], page 86). The block will be strlen(str)+1 bytes, that
being exactly enough for the string and null-terminator.
If str is not NULL, it should point to a block of storage large enough for the result, that being
mpz_sizeinbase (mpq_numref(op), base)
+ mpz_sizeinbase (mpq_denref(op), base) + 3
The three extra bytes are for a possible minus sign, possible slash, and the null-terminator.
A pointer to the result string is returned, being either the allocated block, or the given str.
6.3 Arithmetic Functions
--void mpq_add (mpq_t sum, mpq_t addend1, mpq_t addend2) 
--Set sum to addend1 + addend2.
--void mpq_sub (mpq_t difference, mpq_t minuend, mpq_t subtrahend) 
--Set difference to minuend - subtrahend.
--void mpq_mul (mpq_t product, mpq_t multiplier, mpq_t multiplicand) 
--Set product to multiplier * multiplicand.
--void mpq_mul_2exp (mpq_t rop, mpq_t op1, mp_bitcnt_t op2) 
--Set rop to op1 * 2^op2.
--void mpq_div (mpq_t quotient, mpq_t dividend, mpq_t divisor) 
--Set quotient to dividend/divisor.
--void mpq_div_2exp (mpq_t rop, mpq_t op1, mp_bitcnt_t op2) 
--Set rop to op1/2^op2.
--void mpq_neg (mpq_t negated_operand, mpq_t operand) 
--Set negated operand to -operand.
--void mpq_abs (mpq_t rop, mpq_t op) 
--Set rop to the absolute value of op.
--void mpq_inv (mpq_t inverted_number, mpq_t number) 
--Set inverted number to 1/number. If the new denominator is zero, this routine will divide
--by zero.
6.4 Comparison Functions
int mpq_cmp (mpq_t op1, mpq_t op2) 
int mpq_cmp_z (const mpq_t op1, const mpz_t op2) 
Compare op1 and op2. Return a positive value if op1 > op2, zero if op1 = op2, and a
negative value if op1 < op2.
To determine if two rationals are equal, mpq_equal is faster than mpq_cmp.
--int mpq_cmp_ui (mpq_t op1, mpir_ui num2, mpir_ui den2) [Macro]
--int mpq_cmp_si (mpq_t op1, mpir_si num2, mpir_ui den2) [Macro]
--Compare op1 and num2/den2. Return a positive value if op1 > num2=den2, zero if op1 =
--num2=den2, and a negative value if op1 < num2=den2.
--num2 and den2 are allowed to have common factors.
--These functions are implemented as a macros and evaluate their arguments multiple times.
int mpq_sgn (mpq_t op) [Macro]
Return +1 if op > 0, 0 if op = 0, and -1 if op < 0.
This function is actually implemented as a macro. It evaluates its arguments multiple times.
int mpq_equal (mpq_t op1, mpq_t op2) 
Return non-zero if op1 and op2 are equal, zero if they are non-equal. Although mpq_cmp can
be used for the same purpose, this function is much faster.
6.5 Applying Integer Functions to Rationals
The set of mpq functions is quite small. In particular, there are few functions for either input
or output. The following functions give direct access to the numerator and denominator of an
mpq_t.
Note that if an assignment to the numerator and/or denominator could take an mpq_t out
of the canonical form described at the start of this chapter (see Chapter 6 [Rational Number
Functions], page 45) then mpq_canonicalize must be called before any other mpq functions are
applied to that mpq_t.
mpz_t mpq_numref (mpq_t op) [Macro]
mpz_t mpq_denref (mpq_t op) [Macro]
Return a reference to the numerator and denominator of op, respectively. The mpz functions
can be used on the result of these macros.
--void mpq_get_num (mpz_t numerator, mpq_t rational) 
--void mpq_get_den (mpz_t denominator, mpq_t rational) 
void mpq_set_num (mpq_t rational, mpz_t numerator) 
void mpq_set_den (mpq_t rational, mpz_t denominator) 
Get or set the numerator or denominator of a rational. These functions are equivalent to
calling mpz_set with an appropriate mpq_numref or mpq_denref. Direct use of mpq_numref
or mpq_denref is recommended instead of these functions.
6.6 Input and Output Functions
When using any of these functions, it's a good idea to include stdio.h before mpir.h, since
that will allow mpir.h to define prototypes for these functions.
Passing a NULL pointer for a stream argument to any of these functions will make them read
from stdin and write to stdout, respectively.
size_t mpq_out_str (FILE *stream, int base, mpq_t op) 
Output op on stdio stream stream, as a string of digits in base base. The base may vary from
2 to 36. Output is in the form 'num/den' or if the denominator is 1 then just 'num'.
Return the number of bytes written, or if an error occurred, return 0.
size_t mpq_inp_str (mpq_t rop, FILE *stream, int base) 
Read a string of digits from stream and convert them to a rational in rop. Any initial whitespace
characters are read and discarded. Return the number of characters read (including
white space), or 0 if a rational could not be read.
The input can be a fraction like '17/63' or just an integer like '123'. Reading stops at the
first character not in this form, and white space is not permitted within the string. If the
input might not be in canonical form, then mpq_canonicalize must be called (see Chapter 6
[Rational Number Functions], page 45).
The base can be between 2 and 36, or can be 0 in which case the leading characters of the
string determine the base, '0x' or '0X' for hexadecimal, '0' for octal, or decimal otherwise.
The leading characters are examined separately for the numerator and denominator of a
fraction, so for instance '0x10/11' is 16=11, whereas '0x10/0x11' is 16=17.

Chapter 8: Low-level Functions 57
8 Low-level Functions
This chapter describes low-level MPIR functions, used to implement the high-level MPIR functions,
but also intended for time-critical user code.
These functions start with the prefix mpn_.
The mpn functions are designed to be as fast as possible, not to provide a coherent calling
interface. The different functions have somewhat similar interfaces, but there are variations that
make them hard to use. These functions do as little as possible apart from the real multiple
precision computation, so that no time is spent on things that not all callers need.
A source operand is specified by a pointer to the least significant limb and a limb count. A
destination operand is specified by just a pointer. It is the responsibility of the caller to ensure
that the destination has enough space for storing the result.
With this way of specifying operands, it is possible to perform computations on subranges of an
argument, and store the result into a subrange of a destination.
A common requirement for all functions is that each source area needs at least one limb. No size
argument may be zero. Unless otherwise stated, in-place operations are allowed where source
and destination are the same, but not where they only partly overlap.
The mpn functions are the base for the implementation of the mpz_, mpf_, and mpq_ functions.
This example adds the number beginning at s1p and the number beginning at s2p and writes
the sum at destp. All areas have n limbs.
cy = mpn_add_n (destp, s1p, s2p, n)
It should be noted that the mpn functions make no attempt to identify high or low zero limbs
on their operands, or other special forms. On random data such cases will be unlikely and it'd
be wasteful for every function to check every time. An application knowing something about its
data can take steps to trim or perhaps split its calculations.
In the notation used below, a source operand is identified by the pointer to the least significant
limb, and the limb count in braces. For example, {s1p, s1n}.
mp_limb_t mpn_add_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p, mp_size_t n)
Add {s1p, n} and {s2p, n}, and write the n least significant limbs of the result to rp. Return
carry, either 0 or 1.
This is the lowest-level function for addition. It is the preferred function for addition, since
it is written in assembly for most CPUs. For addition of a variable to itself (i.e., s1p equals
s2p, use mpn_lshift with a count of 1 for optimal speed.
mp_limb_t mpn_add_1 (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t n, mp_limb_t s2limb)
Add {s1p, n} and s2limb, and write the n least significant limbs of the result to rp. Return
carry, either 0 or 1.
mp_limb_t mpn_add (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t s1n, const mp_limb_t *s2p, mp_size_t s2n)
Add {s1p, s1n} and {s2p, s2n}, and write the s1n least significant limbs of the result to rp.
Return carry, either 0 or 1.
58 MPIR 2.7.2
This function requires that s1n is greater than or equal to s2n.
mp_limb_t mpn_sub_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p, mp_size_t n)
Subtract {s2p, n} from {s1p, n}, and write the n least significant limbs of the result to rp.
Return borrow, either 0 or 1.
This is the lowest-level function for subtraction. It is the preferred function for subtraction,
since it is written in assembly for most CPUs.
mp_limb_t mpn_sub_1 (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t n, mp_limb_t s2limb)
Subtract s2limb from {s1p, n}, and write the n least significant limbs of the result to rp.
Return borrow, either 0 or 1.
mp_limb_t mpn_sub (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t s1n, const mp_limb_t *s2p, mp_size_t s2n)
Subtract {s2p, s2n} from {s1p, s1n}, and write the s1n least significant limbs of the result to
rp. Return borrow, either 0 or 1.
This function requires that s1n is greater than or equal to s2n.
void mpn_neg (mp_limb_t *rp, const mp_limb_t *sp, mp_size_t n) 
Perform the negation of {sp, n}, and write the result to {rp, n}. Return carry-out.
void mpn_mul_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p, mp_size_t n)
Multiply {s1p, n} and {s2p, n}, and write the 2*n-limb result to rp.
The destination has to have space for 2*n limbs, even if the product's most significant limb
is zero. No overlap is permitted between the destination and either source.
If the input operands are the same, mpn_sqr will generally be faster.
mp_limb_t mpn_mul_1 (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t n, mp_limb_t s2limb)
Multiply {s1p, n} by s2limb, and write the n least significant limbs of the product to rp.
Return the most significant limb of the product. {s1p, n} and {rp, n} are allowed to overlap
provided rp <= s1p.
This is a low-level function that is a building block for general multiplication as well as other
operations in MPIR. It is written in assembly for most CPUs.
Don't call this function if s2limb is a power of 2; use mpn_lshift with a count equal to the
logarithm of s2limb instead, for optimal speed.
mp_limb_t mpn_addmul_1 (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t n, mp_limb_t s2limb)
Multiply {s1p, n} and s2limb, and add the n least significant limbs of the product to {rp, n}
and write the result to rp. Return the most significant limb of the product, plus carry-out
from the addition.
This is a low-level function that is a building block for general multiplication as well as other
operations in MPIR. It is written in assembly for most CPUs.
Chapter 8: Low-level Functions 59
mp_limb_t mpn_submul_1 (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t n, mp_limb_t s2limb)
Multiply {s1p, n} and s2limb, and subtract the n least significant limbs of the product from
{rp, n} and write the result to rp. Return the most significant limb of the product, minus
borrow-out from the subtraction.
This is a low-level function that is a building block for general multiplication and division as
well as other operations in MPIR. It is written in assembly for most CPUs.
mp_limb_t mpn_mul (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t s1n, const mp_limb_t *s2p, mp_size_t s2n)
Multiply {s1p, s1n} and {s2p, s2n}, and write the result to rp. Return the most significant
limb of the result.
The destination has to have space for s1n + s2n limbs, even if the result might be one limb
smaller.
This function requires that s1n is greater than or equal to s2n. The destination must be
distinct from both input operands.
void mpn_sqr (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t n) 
Compute the square of {s1p, n} and write the 2*n-limb result to rp.
The destination has to have space for 2*n limbs, even if the result's most significant limb is
zero. No overlap is permitted between the destination and the source.
void mpn_tdiv_qr (mp_limb_t *qp, mp_limb_t *rp, mp_size_t qxn, const mp_limb_t *np, mp_size_t nn, const mp_limb_t *dp, mp_size_t dn)
Divide {np, nn} by {dp, dn} and put the quotient at {qp, nn-dn+1} and the remainder at
{rp, dn}. The quotient is rounded towards 0.
No overlap is permitted between arguments. nn must be greater than or equal to dn. The
most significant limb of dp must be non-zero. The qxn operand must be zero.
mp_limb_t mpn_divrem (mp_limb_t *r1p, mp_size_t qxn, mp_limb_t *rs2p, mp_size_t rs2n, const mp_limb_t *s3p, mp_size_t s3n)
[This function is obsolete. Please call mpn_tdiv_qr instead for best performance.]
Divide {rs2p, rs2n} by {s3p, s3n}, and write the quotient at r1p, with the exception of the
most significant limb, which is returned. The remainder replaces the dividend at rs2p; it will
be s3n limbs long (i.e., as many limbs as the divisor).
In addition to an integer quotient, qxn fraction limbs are developed, and stored after the
integral limbs. For most usages, qxn will be zero.
It is required that rs2n is greater than or equal to s3n. It is required that the most significant
bit of the divisor is set.
If the quotient is not needed, pass rs2p + s3n as r1p. Aside from that special case, no overlap
between arguments is permitted.
Return the most significant limb of the quotient, either 0 or 1.
The area at r1p needs to be rs2n - s3n + qxn limbs large.
60 MPIR 2.7.2
mp_limb_t mpn_divrem_1 (mp_limb_t *r1p, mp_size_t qxn, mp_limb_t *s2p, mp_size_t s2n, mp_limb_t s3limb)
mp_limb_t mpn_divmod_1 (mp_limb_t *r1p, mp_limb_t *s2p, mp_size_t s2n, [Macro]
mp_limb_t s3limb)
Divide {s2p, s2n} by s3limb, and write the quotient at r1p. Return the remainder.
The integer quotient is written to {r1p+qxn, s2n} and in addition qxn fraction limbs are
developed and written to {r1p, qxn}. Either or both s2n and qxn can be zero. For most
usages, qxn will be zero.
mpn_divmod_1 exists for upward source compatibility and is simply a macro calling mpn_
divrem_1 with a qxn of 0.
The areas at r1p and s2p have to be identical or completely separate, not partially overlapping.
mp_limb_t mpn_divexact_by3 (mp_limb_t *rp, mp_limb_t *sp, mp_size_t n) [Macro]
mp_limb_t mpn_divexact_by3c (mp_limb_t *rp, mp_limb_t *sp, mp_size_t n, mp_limb_t carry)
Divide {sp, n} by 3, expecting it to divide exactly, and writing the result to {rp, n}. If 3
divides exactly, the return value is zero and the result is the quotient. If not, the return value
is non-zero and the result won't be anything useful.
mpn_divexact_by3c takes an initial carry parameter, which can be the return value from
a previous call, so a large calculation can be done piece by piece from low to high. mpn_
divexact_by3 is simply a macro calling mpn_divexact_by3c with a 0 carry parameter.
These routines use a multiply-by-inverse and will be faster than mpn_divrem_1 on CPUs with
fast multiplication but slow division.
The source a, result q, size n, initial carry i, and return value c satisfy cbn+a-i = 3q, where
b = 2GMP NUMB BITS. The return c is always 0, 1 or 2, and the initial carry i must also be 0,
1 or 2 (these are both borrows really). When c = 0 clearly q = (a - i)=3. When c 6= 0, the
remainder (a - i) mod 3 is given by 3 - c, because b === 1 mod 3 (when mp_bits_per_limb is
even, which is always so currently).
mp_limb_t mpn_mod_1 (mp_limb_t *s1p, mp_size_t s1n, mp_limb_t s2limb) 
Divide {s1p, s1n} by s2limb, and return the remainder. s1n can be zero.
mp_limb_t mpn_lshift (mp_limb_t *rp, const mp_limb_t *sp, mp_size_t n, unsigned int count)
Shift {sp, n} left by count bits, and write the result to {rp, n}. The bits shifted out at the
left are returned in the least significant count bits of the return value (the rest of the return
value is zero).
count must be in the range 1 to mp_bits_per_limb-1. The regions {sp, n} and {rp, n} may
overlap, provided rp >= sp.
This function is written in assembly for most CPUs.
mp_limb_t mpn_rshift (mp_limb_t *rp, const mp_limb_t *sp, mp_size_t n, unsigned int count)
Shift {sp, n} right by count bits, and write the result to {rp, n}. The bits shifted out at the
right are returned in the most significant count bits of the return value (the rest of the return
value is zero).
Chapter 8: Low-level Functions 61
count must be in the range 1 to mp_bits_per_limb-1. The regions {sp, n} and {rp, n} may
overlap, provided rp <= sp.
This function is written in assembly for most CPUs.
int mpn_cmp (const mp_limb_t *s1p, const mp_limb_t *s2p, mp_size_t n) 
Compare {s1p, n} and {s2p, n} and return a positive value if s1 > s2, 0 if they are equal, or
a negative value if s1 < s2.
mp_size_t mpn_gcd (mp_limb_t *rp, mp_limb_t *s1p, mp_size_t s1n, mp_limb_t *s2p, mp_size_t s2n)
Set {rp, retval} to the greatest common divisor of {s1p, s1n} and {s2p, s2n}. The result can
be up to s2n limbs, the return value is the actual number produced. Both source operands
are destroyed.
{s1p, s1n} must have at least as many bits as {s2p, s2n}. {s2p, s2n} must be odd. Both
operands must have non-zero most significant limbs. No overlap is permitted between {s1p,
s1n} and {s2p, s2n}.
mp_limb_t mpn_gcd_1 (const mp_limb_t *s1p, mp_size_t s1n, mp_limb_t s2limb)
Return the greatest common divisor of {s1p, s1n} and s2limb. Both operands must be nonzero.
mp_size_t mpn_gcdext (mp_limb_t *gp, mp_limb_t *sp, mp_size_t *sn, mp_limb_t *xp, mp_size_t xn, mp_limb_t *yp, mp_size_t yn)
Let U be defined by {xp, xn} and let V be defined by {yp, yn}.
Compute the greatest common divisor G of U and V . Compute a cofactor S such that
G = US + V T. The second cofactor T is not computed but can easily be obtained from
(G - US)=V (the division will be exact). It is required that U >= V > 0.
S satisfies S = 1 or |S| < V/(2G). S = 0 if and only if V divides U (i.e., G = V ).
Store G at gp and let the return value define its limb count. Store S at sp and let |*sn|
define its limb count. S can be negative; when this happens *sn will be negative. The areas
at gp and sp should each have room for xn + 1 limbs.
The areas {xp, xn+1} and {yp, yn+1} are destroyed (i.e. the input operands plus an extra
limb past the end of each).
Compatibility note: MPIR versions 1.3,2.0 and GMP versions 4.3.0,4.3.1 defined S less
strictly. Earlier as well as later GMP releases define S as described here.
mp_size_t mpn_sqrtrem (mp_limb_t *r1p, mp_limb_t *r2p, const mp_limb_t *sp, mp_size_t n)
Compute the square root of {sp, n} and put the result at {r1p, dn=2e} and the remainder
at {r2p, retval}. r2p needs space for n limbs, but the return value indicates how many are
produced.
The most significant limb of {sp, n} must be non-zero. The areas {r1p, dn=2e} and {sp, n}
must be completely separate. The areas {r2p, n} and {sp, n} must be either identical or
completely separate.
If the remainder is not wanted then r2p can be NULL, and in this case the return value is zero
or non-zero according to whether the remainder would have been zero or non-zero.
62 MPIR 2.7.2
A return value of zero indicates a perfect square. See also mpz_perfect_square_p.
mp_size_t mpn_get_str (unsigned char *str, int base, mp_limb_t *s1p, mp_size_t s1n)
Convert {s1p, s1n} to a raw unsigned char array at str in base base, and return the number
of characters produced. There may be leading zeros in the string. The string is not in ASCII;
to convert it to printable format, add the ASCII codes for '0' or 'A', depending on the base
and range. base can vary from 2 to 256.
The most significant limb of the input {s1p, s1n} must be non-zero. The input {s1p, s1n} is
clobbered, except when base is a power of 2, in which case it's unchanged.
The area at str has to have space for the largest possible number represented by a s1n long
limb array, plus one extra character.
mp_size_t mpn_set_str (mp_limb_t *rp, const unsigned char *str, size t strsize, int base)
Convert bytes {str,strsize} in the given base to limbs at rp.
str[0] is the most significant byte and str[strsize-1] is the least significant. Each byte should
be a value in the range 0 to base - 1, not an ASCII character. base can vary from 2 to 256.
The return value is the number of limbs written to rp. If the most significant input byte is
non-zero then the high limb at rp will be non-zero, and only that exact number of limbs will
be required there.
If the most significant input byte is zero then there may be high zero limbs written to rp and
included in the return value.
strsize must be at least 1, and no overlap is permitted between {str,strsize} and the result
at rp.
mp_bitcnt_t mpn_scan0 (const mp_limb_t *s1p, imp_bitcnt_t bit) 
Scan s1p from bit position bit for the next clear bit.
It is required that there be a clear bit within the area at s1p at or beyond bit position bit,
so that the function has something to return.
mp_bitcnt_t mpn_scan1 (const mp_limb_t *s1p, mp_bitcnt_t bit) 
Scan s1p from bit position bit for the next set bit.
It is required that there be a set bit within the area at s1p at or beyond bit position bit, so
that the function has something to return.
void mpn_random (mp_limb_t *r1p, mp_size_t r1n) 
void mpn_random2 (mp_limb_t *r1p, mp_size_t r1n) 
Generate a random number of length r1n and store it at r1p. The most significant limb
is always non-zero. mpn_random generates uniformly distributed limb data, mpn_random2
generates long strings of zeros and ones in the binary representation.
mpn_random2 is intended for testing the correctness of the mpn routines.
These functions are obsolete. They will disappear from future MPIR releases.
void mpn_urandomb (mp_limb_t *rp, gmp_randstate_t state, mpir_ui n) 
Generate a uniform random number of length n bits and store it at rp.
Chapter 8: Low-level Functions 63
This function interface is preliminary and may change in the future.
void mpn_urandomm (mp_limb_t *rp, gmp_randstate_t state, const mp_limb_t *mp, mp_size_t n)
Generate a uniform random number modulo (mp,n) of length n limbs and store it at rp.
This function interface is preliminary and may change in the future.
void mpn_randomb (mp_limb_t *rp, gmp_randstate_t state, mp_size_t n) 
Generate a random number of length n limbs and store it at rp. The most significant limb is
always non-zero.
This function interface is preliminary and may change in the future.
void mpn_rrandom (mp_limb_t *rp, gmp_randstate_t state, mp_size_t n) 
Generate a random number of length n limbs and store it at rp. The most significant limb is
always non-zero. Generates long strings of zeros and ones in the binary representation and
is intended for testing the correctness of the mpn routines.
This function interface is preliminary and may change in the future.
mp_bitcnt_t mpn_popcount (const mp_limb_t *s1p, mp_size_t n) 
Count the number of set bits in {s1p, n}.
mp_bitcnt_t mpn_hamdist (const mp_limb_t *s1p, const mp_limb_t *s2p, mp_size_t n)
Compute the hamming distance between {s1p, n} and {s2p, n}, which is the number of bit
positions where the two operands have different bit values.
int mpn_perfect_square_p (const mp_limb_t *s1p, mp_size_t n) 
Return non-zero iff {s1p, n} is a perfect square.
void mpn_and_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p, mp_size_t n)
Perform the bitwise logical and of {s1p, n} and {s2p, n}, and write the result to {rp, n}.
void mpn_ior_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p, mp_size_t n)
Perform the bitwise logical inclusive or of {s1p, n} and {s2p, n}, and write the result to {rp,
n}.
void mpn_xor_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p, mp_size_t n)
Perform the bitwise logical exclusive or of {s1p, n} and {s2p, n}, and write the result to {rp,
n}.
void mpn_andn_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p, mp_size_t n)
Perform the bitwise logical and of {s1p, n} and the bitwise complement of {s2p, n}, and write
the result to {rp, n}.
64 MPIR 2.7.2
void mpn_iorn_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p, mp_size_t n)
Perform the bitwise logical inclusive or of {s1p, n} and the bitwise complement of {s2p, n},
and write the result to {rp, n}.
void mpn_nand_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p, mp_size_t n)
Perform the bitwise logical and of {s1p, n} and {s2p, n}, and write the bitwise complement
of the result to {rp, n}.
void mpn_nior_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p, mp_size_t n)
Perform the bitwise logical inclusive or of {s1p, n} and {s2p, n}, and write the bitwise
complement of the result to {rp, n}.
void mpn_xnor_n (mp_limb_t *rp, const mp_limb_t *s1p, const mp_limb_t *s2p, mp_size_t n)
Perform the bitwise logical exclusive or of {s1p, n} and {s2p, n}, and write the bitwise
complement of the result to {rp, n}.
void mpn_com (mp_limb_t *rp, const mp_limb_t *sp, mp_size_t n) 
Perform the bitwise complement of {sp, n}, and write the result to {rp, n}.
void mpn_copyi (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t n) 
Copy from {s1p, n} to {rp, n}, increasingly.
void mpn_copyd (mp_limb_t *rp, const mp_limb_t *s1p, mp_size_t n) 
Copy from {s1p, n} to {rp, n}, decreasingly.
void mpn_zero (mp_limb_t *rp, mp_size_t n) 
Zero {rp, n}.

9 Random Number Functions
Sequences of pseudo-random numbers in MPIR are generated using a variable of type gmp_
randstate_t, which holds an algorithm selection and a current state. Such a variable must be
initialized by a call to one of the gmp_randinit functions, and can be seeded with one of the
gmp_randseed functions.
The functions actually generating random numbers are described in Section 5.13 [Integer Random
Numbers], page 41, and Section 7.8 [Miscellaneous Float Functions], page 55.
The older style random number functions don't accept a gmp_randstate_t parameter but instead
share a global variable of that type. They use a default algorithm and are currently
not seeded (though perhaps that will change in the future). The new functions accepting a
gmp_randstate_t are recommended for applications that care about randomness.
9.1 Random State Initialization
void gmp_randinit_default (gmp_randstate_t state) 
Initialize state with a default algorithm. This will be a compromise between speed and
randomness, and is recommended for applications with no special requirements. Currently
this is gmp_randinit_mt.
--void gmp_randinit_mt (gmp_randstate_t state) 
--Initialize state for a Mersenne Twister algorithm. This algorithm is fast and has good randomness
--properties.
void gmp_randinit_lc_2exp (gmp_randstate_t state, mpz_t a, mpir_ui c, mp_bitcnt_t m2exp)
Initialize state with a linear congruential algorithm X = (aX + c) mod 2m2exp.
The low bits of X in this algorithm are not very random. The least significant bit will have
a period no more than 2, and the second bit no more than 4, etc. For this reason only the
high half of each X is actually used.
When a random number of more than m2exp=2 bits is to be generated, multiple iterations
of the recurrence are used and the results concatenated.
int gmp_randinit_lc_2exp_size (gmp_randstate_t state, mp_bitcnt_t size)
Initialize state for a linear congruential algorithm as per gmp_randinit_lc_2exp. a, c and
m2exp are selected from a table, chosen so that size bits (or more) of each X will be used,
ie. m2exp=2 >= size.
If successful the return value is non-zero. If size is bigger than the table data provides then
the return value is zero. The maximum size currently supported is 128.
int gmp_randinit_set (gmp_randstate_t rop, gmp_randstate_t op) 
Initialize rop with a copy of the algorithm and state from op.
--void gmp_randclear (gmp_randstate_t state) 
--Free all memory occupied by state.

9.2 Random State Seeding
--void gmp_randseed (gmp_randstate_t state, mpz_t seed) 
void gmp_randseed_ui (gmp_randstate_t state, mpir_ui seed) 
Set an initial seed value into state.
The size of a seed determines how many different sequences of random numbers that it's
possible to generate. The "quality" of the seed is the randomness of a given seed compared
to the previous seed used, and this affects the randomness of separate number sequences. The
method for choosing a seed is critical if the generated numbers are to be used for important
applications, such as generating cryptographic keys.
Traditionally the system time has been used to seed, but care needs to be taken with this.
If an application seeds often and the resolution of the system clock is low, then the same
sequence of numbers might be repeated. Also, the system time is quite easy to guess, so if
unpredictability is required then it should definitely not be the only source for the seed value.
On some systems there's a special device /dev/random which provides random data better
suited for use as a seed.
9.3 Random State Miscellaneous
mpir_ui gmp_urandomb_ui (gmp_randstate_t state, mpir_ui n) 
Return a uniformly distributed random number of n bits, ie. in the range 0 to 2n-1 inclusive.
n must be less than or equal to the number of bits in an mpir_ui.
mpir_ui gmp_urandomm_ui (gmp_randstate_t state, mpir_ui n) 
Return a uniformly distributed random number in the range 0 to n - 1, inclusive.
68 MPIR 2.7.2
10 Formatted Output
10.1 Format Strings
gmp_printf and friends accept format strings similar to the standard C printf (see Section
"Formatted Output" in The GNU C Library Reference Manual). A format specification is of
the form
% [flags] [width] [.[precision]] [type] conv
MPIR adds types 'Z', 'Q' for mpz_t, mpq_t respectively, 'M' for mp_limb_t,
and 'N' for an mp_limb_t array. 'Z', 'Q', 'M' and 'N' behave like integers. 'Q' will print a '/' and
a denominator, if needed. For example,
mpz_t z;
gmp_printf ("%s is an mpz %Zd\n", "here", z);
mpq_t q;
gmp_printf ("a hex rational: %#40Qx\n", q);
mp_limb_t l;
gmp_printf ("limb %Mu\n", limb);
const mp_limb_t *ptr;
mp_size_t size;
gmp_printf ("limb array %Nx\n", ptr, size);
For 'N' the limbs are expected least significant first, as per the mpn functions (see Chapter 8
[Low-level Functions], page 57). A negative size can be given to print the value as a negative.
All the standard C printf types behave the same as the C library printf, and can be freely
intermixed with the MPIR extensions. In the current implementation the standard parts of the
format string are simply handed to printf and only the MPIR extensions handled directly.
The flags accepted are as follows. GLIBC style ''' is only for the standard C types (not the
MPIR types), and only if the C library supports it.
0 pad with zeros (rather than spaces)
# show the base with '0x', '0X' or '0'
+ always show a sign
(space) show a space or a '-' sign
' group digits, GLIBC style (not MPIR types)
The optional width and precision can be given as a number within the format string, or as a '*'
to take an extra parameter of type int, the same as the standard printf.
The standard types accepted are as follows. 'h' and 'l' are portable, the rest will depend on the
compiler (or include files) for the type and the C library for the output.
h short
hh char
Chapter 10: Formatted Output 69
j intmax_t or uintmax_t
l long or wchar_t
ll long long
L long double
q quad_t or u_quad_t
t ptrdiff_t
z size_t
The MPIR types are
Q mpq_t, integer conversions
M mp_limb_t, integer conversions
N mp_limb_t array, integer conversions
Z mpz_t, integer conversions
The conversions accepted are as follows. 'a', 'A', 'm' and 'p' depend on the C library.
a A hex floats, C99 style
c character
d decimal integer
e E scientific format float
f fixed point float
i same as d
g G fixed or scientific float
m strerror string, GLIBC style
n store characters written so far
o octal integer
p pointer
s string
u unsigned integer
x X hex integer
'o', 'x' and 'X' are unsigned for the standard C types, but for types 'Z', 'Q' and 'N' they are
signed. 'u' is not meaningful for 'Z', 'Q' and 'N'.
'M' is a proxy for the C library 'l' or 'L', according to the size of mp_limb_t. Unsigned conversions
will be usual, but a signed conversion can be used and will interpret the value as a twos
complement negative.
'n' can be used with any type, even the MPIR types.
Other types or conversions that might be accepted by the C library printf cannot be used
through gmp_printf, this includes for instance extensions registered with GLIBC register_
printf_function. Also currently there's no support for POSIX '$' style numbered arguments
(perhaps this will be added in the future).
The precision field has it's usual meaning for integer 'Z' and float 'F' types, but is currently
undefined for 'Q' and should not be used with that.
10.2 Functions
Each of the following functions is similar to the corresponding C library function. The basic
printf forms take a variable argument list. The vprintf forms take an argument pointer, see
Section "Variadic Functions" in The GNU C Library Reference Manual, or 'man 3 va_start'.
It should be emphasised that if a format string is invalid, or the arguments don't match what
the format specifies, then the behaviour of any of these functions will be unpredictable. GCC
format string checking is not available, since it doesn't recognise the MPIR extensions.
The file based functions gmp_printf and gmp_fprintf will return -1 to indicate a write error.
Output is not "atomic", so partial output may be produced if a write error occurs. All the
functions can return -1 if the C library printf variant in use returns -1, but this shouldn't
normally occur.
int gmp_printf (const char *fmt, . . . ) 
int gmp_vprintf (const char *fmt, va list ap) 
Print to the standard output stdout. Return the number of characters written, or -1 if an
error occurred.
int gmp_fprintf (FILE *fp, const char *fmt, . . . ) 
int gmp_vfprintf (FILE *fp, const char *fmt, va list ap) 
Print to the stream fp. Return the number of characters written, or -1 if an error occurred.
int gmp_sprintf (char *buf, const char *fmt, . . . ) 
int gmp_vsprintf (char *buf, const char *fmt, va list ap) 
Form a null-terminated string in buf. Return the number of characters written, excluding
the terminating null.
No overlap is permitted between the space at buf and the string fmt.
These functions are not recommended, since there's no protection against exceeding the space
available at buf.
int gmp_snprintf (char *buf, size t size, const char *fmt, . . . ) 
int gmp_vsnprintf (char *buf, size t size, const char *fmt, va list ap) 
Form a null-terminated string in buf. No more than size bytes will be written. To get the
full output, size must be enough for the string and null-terminator.
The return value is the total number of characters which ought to have been produced,
excluding the terminating null. If retval >= size then the actual output has been truncated to
the first size - 1 characters, and a null appended.
No overlap is permitted between the region {buf,size} and the fmt string.
Chapter 10: Formatted Output 71
Notice the return value is in ISO C99 snprintf style. This is so even if the C library
vsnprintf is the older GLIBC 2.0.x style.
int gmp_asprintf (char **pp, const char *fmt, . . . ) 
int gmp_vasprintf (char **pp, const char *fmt, va list ap) 
Form a null-terminated string in a block of memory obtained from the current memory
allocation function (see Chapter 13 [Custom Allocation], page 86). The block will be the size
of the string and null-terminator. The address of the block in stored to *pp. The return
value is the number of characters produced, excluding the null-terminator.
Unlike the C library asprintf, gmp_asprintf doesn't return -1 if there's no more memory
available, it lets the current allocation function handle that.
int gmp_obstack_printf (struct obstack *ob, const char *fmt, . . . ) 
int gmp_obstack_vprintf (struct obstack *ob, const char *fmt, va list ap) 
Append to the current object in ob. The return value is the number of characters written.
A null-terminator is not written.
fmt cannot be within the current object in ob, since that object might move as it grows.
These functions are available only when the C library provides the obstack feature, which
probably means only on GNU systems, see Section "Obstacks" in The GNU C Library Reference
Manual.

11 Formatted Input
11.1 Formatted Input Strings
gmp_scanf and friends accept format strings similar to the standard C scanf (see Section
"Formatted Input" in The GNU C Library Reference Manual). A format specification is of the
form
% [flags] [width] [type] conv
MPIR adds types 'Z', 'Q' for mpz_t, mpq_t respectively. 'Z' and 'Q' behave
like integers. 'Q' will read a '/' and a denominator, if present.
MPIR variables don't require an & when passed to gmp_scanf, since they're already "call-byreference".
For example,
/* to read say "a(5) = 1234" */
int n;
mpz_t z;
gmp_scanf ("a(%d) = %Zd\n", &n, z);
mpq_t q1, q2;
gmp_sscanf ("0377 + 0x10/0x11", "%Qi + %Qi", q1, q2);
All the standard C scanf types behave the same as in the C library scanf, and can be freely
intermixed with the MPIR extensions. In the current implementation the standard parts of the
format string are simply handed to scanf and only the MPIR extensions handled directly.
The flags accepted are as follows. 'a' and ''' will depend on support from the C library, and '''
cannot be used with MPIR types.
* read but don't store
a allocate a buffer (string conversions)
' grouped digits, GLIBC style (not MPIR types)
The standard types accepted are as follows. 'h' and 'l' are portable, the rest will depend on the
compiler (or include files) for the type and the C library for the input.
h short
hh char
j intmax_t or uintmax_t
l long int, double or wchar_t
ll long long
L long double
q quad_t or u_quad_t
t ptrdiff_t
z size_t
The MPIR types are
Q mpq_t, integer conversions
Z mpz_t, integer conversions
The conversions accepted are as follows. 'p' and '[' will depend on support from the C library,
the rest are standard.
c character or characters
d decimal integer
e E f g
G
float
i integer with base indicator
n characters read so far
o octal integer
p pointer
s string of non-whitespace characters
u decimal integer
x X hex integer
[ string of characters in a set
'e', 'E', 'f', 'g' and 'G' are identical, they all read either fixed point or scientific format, and
either upper or lower case 'e' for the exponent in scientific format.
C99 style hex float format (printf %a, see Section 10.1 [Formatted Output Strings], page 68) for the standard float types depends on the C library.
'x' and 'X' are identical, both accept both upper and lower case hexadecimal.
'o', 'u', 'x' and 'X' all read positive or negative values. For the standard C types these are
described as "unsigned" conversions, but that merely affects certain overflow handling, negatives
are still allowed (per strtoul, see Section "Parsing of Integers" in The GNU C Library Reference
Manual). For MPIR types there are no overflows, so 'd' and 'u' are identical.
'Q' type reads the numerator and (optional) denominator as given. If the value might not be in
canonical form then mpq_canonicalize must be called before using it in any calculations (see
Chapter 6 [Rational Number Functions], page 45).
'Qi' will read a base specification separately for the numerator and denominator. For example
'0x10/11' would be 16/11, whereas '0x10/0x11' would be 16/17.
'n' can be used with any of the types above, even the MPIR types. '*' to suppress assignment
is allowed, though in that case it would do nothing at all.
Other conversions or types that might be accepted by the C library scanf cannot be used
through gmp_scanf.
Whitespace is read and discarded before a field, except for 'c' and '[' conversions.
For float conversions, the decimal point character (or string) expected is taken from the current
locale settings on systems which provide localeconv (see Section "Locales and Internationalization"
in The GNU C Library Reference Manual). The C library will normally do the same
for standard float input.
The format string is only interpreted as plain chars, multibyte characters are not recognised.
Perhaps this will change in the future.
Chapter 11: Formatted Input 75
11.2 Formatted Input Functions
Each of the following functions is similar to the corresponding C library function. The plain
scanf forms take a variable argument list. The vscanf forms take an argument pointer, see
Section "Variadic Functions" in The GNU C Library Reference Manual, or 'man 3 va_start'.
It should be emphasised that if a format string is invalid, or the arguments don't match what
the format specifies, then the behaviour of any of these functions will be unpredictable. GCC
format string checking is not available, since it doesn't recognise the MPIR extensions.
No overlap is permitted between the fmt string and any of the results produced.
int gmp_scanf (const char *fmt, . . . ) 
int gmp_vscanf (const char *fmt, va list ap) 
Read from the standard input stdin.
int gmp_fscanf (FILE *fp, const char *fmt, . . . ) 
int gmp_vfscanf (FILE *fp, const char *fmt, va list ap) 
Read from the stream fp.
int gmp_sscanf (const char *s, const char *fmt, . . . ) 
int gmp_vsscanf (const char *s, const char *fmt, va list ap) 
Read from a null-terminated string s.
The return value from each of these functions is the same as the standard C99 scanf, namely
the number of fields successfully parsed and stored. '%n' fields and fields read but suppressed by
'*' don't count towards the return value.
If end of input (or a file error) is reached before a character for a field or a literal, and if
no previous non-suppressed fields have matched, then the return value is EOF instead of 0. A
whitespace character in the format string is only an optional match and doesn't induce an EOF
in this fashion. Leading whitespace read and discarded for a field don't count as characters for
that field.
For the MPIR types, input parsing follows C99 rules, namely one character of lookahead is used
and characters are read while they continue to meet the format requirements. If this doesn't
provide a complete number then the function terminates, with that field not stored nor counted
towards the return value. 
For the standard C types, in the current implementation MPIR calls the C library scanf functions,
which might have looser rules about what constitutes a valid input.
Note that gmp_sscanf is the same as gmp_fscanf and only does one character of lookahead
when parsing. Although clearly it could look at its entire input, it is deliberately made identical
to gmp_fscanf, the same way C99 sscanf is the same as fscanf.

13 Custom Allocation
By default MPIR uses malloc, realloc and free for memory allocation, and if they fail MPIR
prints a message to the standard error output and terminates the program.
Alternate functions can be specified, to allocate memory in a different way or to have a different
error action on running out of memory.
void mp_set_memory_functions(void *(*alloc_func_ptr) (size t),
                             void *(*realloc_func_ptr) (void *, size t, size t),
                             void (*free_func_ptr) (void *, size t))
Replace the current allocation functions from the arguments. If an argument is NULL, the
corresponding default function is used.
These functions will be used for all memory allocation done by MPIR, apart from temporary
space from alloca if that function is available and MPIR is configured to use it (see
Section 2.1 [Build Options], page 3).
Be sure to call mp_set_memory_functions only when there are no active MPIR objects
allocated using the previous memory functions! Usually that means calling it before any
other MPIR function.
The functions supplied should fit the following declarations:
void * allocate_function (size t alloc_size) 
Return a pointer to newly allocated space with at least alloc size bytes.
void * reallocate_function (void *ptr, size t old_size, size t new_size)
Resize a previously allocated block ptr of old size bytes to be new size bytes.
The block may be moved if necessary or if desired, and in that case the smaller of old size
and new size bytes must be copied to the new location. The return value is a pointer to the
resized block, that being the new location if moved or just ptr if not.
ptr is never NULL, it's always a previously allocated block. new size may be bigger or smaller
than old size.
void free_function (void *ptr, size t size) 
De-allocate the space pointed to by ptr.
ptr is never NULL, it's always a previously allocated block of size bytes.
A byte here means the unit used by the sizeof operator.
The old size parameters to reallocate function and free function are passed for convenience,
but of course can be ignored if not needed. The default functions using malloc and friends for
instance don't use them.
No error return is allowed from any of these functions, if they return then they must have performed
the specified operation. In particular note that allocate function or reallocate function
mustn't return NULL.
Chapter 13: Custom Allocation 87
Getting a different fatal error action is a good use for custom allocation functions, for example
giving a graphical dialog rather than the default print to stderr. How much is possible when
genuinely out of memory is another question though.
There's currently no defined way for the allocation functions to recover from an error such as out
of memory, they must terminate program execution. A longjmp or throwing a C++ exception
will have undefined results. This may change in the future.
MPIR may use allocated blocks to hold pointers to other allocated blocks. This will limit the
assumptions a conservative garbage collection scheme can make.
Any custom allocation functions must align pointers to limb boundaries. Thus if a limb is eight
bytes (e.g. on x86 64), then all blocks must be aligned to eight byte boundaries. Check the
configuration options for the custom allocation library in use. It is not necessary to align blocks
to SSE boundaries even when SSE code is used. All MPIR assembly routines assume limb
boundary alignment only (which is the default for most standard memory managers).
Since the default MPIR allocation uses malloc and friends, those functions will be linked in
even if the first thing a program does is an mp_set_memory_functions. It's necessary to change
the MPIR sources if this is a problem.
void mp_get_memory_functions(void *(**alloc_func_ptr) (size t),
                             void *(**realloc_func_ptr) (void *, size t, size t),
                             void (**free_func_ptr) (void *, size t))
Get the current allocation functions, storing function pointers to the locations given by the
arguments. If an argument is NULL, that function pointer is not stored.
For example, to get just the current free function,
void (*freefunc) (void *, size_t);
mp_get_memory_functions (NULL, NULL, &freefunc);


Agent ransack(C:\MinGW, *gmp*.dll)
C:\MinGW\bin\libgmp-10.dll 669.00 KB 22/08/2013 12:45:58
C:\MinGW\bin\libgmpxx-4.dll 77.00 KB 22/08/2013 12:45:58
C:\MinGW\msys\1.0\bin\msys-gmp-10.dll 1,710.00 KB 04/05/2010 03:04:46

--*/
