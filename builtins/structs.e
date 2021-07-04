--
-- builtins\structs.e
-- ==================
--
--  See Phix.chm/Core Language/Declarations/Structs and Classes
--  and    ""   /Internals/builtins/structs.e
--
--
--  Primary purpose
--  ===============
--
--  The primary purpose of structs is to allow "dot notation", such as:
--
--      struct person
--          -- <other variations, see below>
--          string name
--          atom age
--      end struct
--
--      person p = new()
--
--      p.name = "boris"
--      p.age = 3.75
--
--  For this to happen, the compiler must know that p is a struct/class: in
--  some cases you may need to store things first in an explicitly declared
--  variable of the appropriate type (which can just be the root stuct/class).
--
--  One subtle point first: this handling is really all about what we can get
--  the compiler to understand, and automate for us, rather than *any* of the
--  following actual code. This is not just the run-time implementation, but
--  also the compile-time knowledge centre for all structs it must deal with.
--  The routines below are mainly intended to support the above syntax rather 
--  than being something that should (often) be called directly/explicitly.
--
--  Fairly obviously you cannot declare file-level variables within a struct, 
--  since they would just be treated as fields, nor declare constants/enums, 
--  nor execute any standalone statements within a struct definition.
--
--
--  Reference semantics
--  ===================
--
--  Unlike sequences, all structs have reference rather than copy-on-write semantics:
--
--      sequence a = {1},
--               b = a
--      struct mystruct
--          integer i
--      end struct
--      mystruct s = new({1}),
--               t = s
--      b[1] = 2        -- (creates clone)
--      t.i = 2         -- (changes both)
--      ?{a[1],b[1]}    -- {1,2}
--      ?{s.i,t.i}      -- {2,2}
--
--  With sequences, modifying b preserves a, whereas all structs must be created via
--  an explicit new() call and modifying the t copy also simultaneously modifies s. 
--  Naturally this means that sequences may be a better fit for many things/algorithms 
--  such as all legacy code, whereas structs can offer a more elegant solution, not 
--  least because it is not necessary to always explicitly return and overwrite every 
--  single modified struct, or at least any that have been passed in as parameters.
--
--  [aside: The first draft design had four types of structs, one sequence-semantic
--          and three reference-semantic, before realising it just over-complicated 
--          things, and that structs /should/ behave differently, and in fact that 
--          is probably pretty much the whole point of ever adding them, really.]
--
--
--  Performance note
--  ================
--
--  Assume that integer x,y is the fastest way to perform some calculation.
--  Using enum X,Y sequence pt and pt[X], pt[Y] will be measurably slower.
--  At best, structs are more like pt[4][X], pt[4][Y], yet slower still.
--  Structs also have much finer grained type checking than sequences, so 
--  attempts to set s.age to a string (not atom) trigger an error, and 
--  obviously that sort of thing incurs even further overhead.
--
--  You should use structs when the performance is not critical, and the
--  gain in code clarity outweighs any such overheads. A general rule of
--  thumb might be to avoid structs if you will ever need to set/retrieve 
--  fields more than about one thousand times per second. Note there are
--  cases where 50FPS (frames per second) would not suffer significantly,
--  whereas N-squared collision detection at 50FPS probably would, and I
--  would certainly not recommend trying to represent each screen pixel
--  with an individual struct, at least not when performance matters.
--
--
--  Syntax
--  ======
--
--  The full formal syntax is:
--
--      ["global"] ["abstract"] 
--      ("struct"|"class") <name> ((["nullable"]["dynamic"]["extends" <base>{","<base>}]
--          {["private"[":"]|"public"[":"]]
--           (<type> <field> ["=" <expression>]{"," <field> ["=" <expression>]})|
--           (("procedure"|"function") <method>"("<args>")"(<method_body>|";"))})
--          |<cdef>)
--      "end" ("struct"|"class")
--
--  ("private:"|"public:") are section headers and apply until overidden or the
--  final end struct/class, whereas ("private"|"public") only apply to a single
--  field or method. Both "dynamic" & <cdef> warrant their own sections below.
--
--  A struct/class <name> definition implicitly declares a new user-defined type
--  of the same name, and hence after the end struct/class you can then declare 
--  variables of that type, typically with an "= new()", or invoke it directly 
--  to test whether some value is of that type or not.
--
--
--  Simple structs
--  ==============
--
--      [global][abstract]
--      struct <name> [nullable][extends <base>]
--          {<type> <field> [= <expression>]{, <field> [= <expression>]}
--      end struct
--
--  A struct implicitly declares a new user-defined type, of the same name.
--  An abstract struct cannot be instantiated (new()'d), but of course can be
--  used both in generic code and as a target for some later extends clause[s].
--   (Both "global abstract struct" and "abstract global struct" are legal, as
--    well of course as just "global struct", "abstract struct", and "struct".)
--  There is a builtin (root) abstract type "struct" which can be used for local
--  variables and routine parameters much like "object", but obviously a subset.
--  Note that a terminating ';' becomes mandatory when declaring an unassigned
--  file-level "struct" variable (ie the lowest-level one there is).
--
--  Like traditional variables, struct instances must be assigned before use,
--  and individual fields can be assigned defaults, for example:
--
--      struct person nullable
--          string name = "boris"   -- (who /actually runs/ this country)
--          atom age = 3.75,        -- (in terms of maturity & behaviour)
--               IQ = 0.0004        -- (~0.1% of the average garden slug)
--      end struct
--
--      person p = new()
--      -- and p.name = "boris"
--      --  "  p.age = 3.75
--      --  "  p.IQ = 0.0004
--      --  or p = new({"boris",3.75,0.0004})
--      ?p.name -- "boris" 
--      ?p.age  -- 3.75 
--      ?p.IQ   -- 0.0004 
--  
--  Nullable structs are sometimes useful/necessary for optional fields.
--   (ie/eg you are allowed to set p to NULL without triggering a fatal error)
--  A (constant) default value may optionally be specified for each field
--   (ie it is calculated just once rather than on every new() call).
--   If omitted suitable defaults such as 0, "", {} are automatically provided.
--  You can optionally pass initial values in order as a sequence to new(),
--  or alternatively, and somewhat less error-prone, set them individually.
--   (ie if you changed the fields layout/order, you would also need to change 
--    all the sequence values passed directly to new() to match, whereas you
--    might only need to change 4-line version should a field name change.)
--
--  Extending a structure copies the fields before adding any more, eg
--
--      struct employee extends person
--          integer employee_number = 1
--      end struct
--
--      employee e = new()
--
--  Then e has e.name, e.age, e.IQ, and e.employee_number, which default to
--  {"boris",3.75,0.0004,1} respectively.
--
--  Alternatively structures can be embedded, eg
--
--      struct employee2
--          person p
--          integer employee_number
--      end struct
--
--      employee2 e = new()
--
--  Then e has e.p, e.p.name, e.p.age, e.p.IQ, and e.employee_number.
--
--
--  Invoking new()
--  ==============
--
--  The formal parameters are new(string|integer s, sequence imm={}).
--
--  If omitted, the compiler automatically provides/inserts s from context, ie/eg
--  `person p = new()` is equivalent to `person p = new(routine_id("person"))`.
--  You could also use a string, perhaps for as-yet-undefined structs, as long as 
--  it is actually properly defined by run-time.
--
--  The values in imm, if provided, override existing defaults in numeric order.
--
--
--  C structs
--  =========
--
--      struct rect """
--      typedef struct _RECT {
--        LONG left;
--        LONG top;
--        LONG right;
--        LONG bottom;
--      } RECT, *PRECT;"""
--      end struct
--
--      rect r = new({10,10,350,200}) -- (arg optional)
--      r.left = 10     --\
--      r.top = 10      -- } equivalent to using the
--      r.right = 350   -- }  {10,10,350,200} above
--      r.bottom = 200  --/
--
--      c_proc(xFillRect,{hDC,struct_mem(r),hBrush})
--
--  A c-struct allows the new dot notation syntax to be applied to the existing 
--  cffi.e, which is effectively deprecated (/some functions of which renamed). 
--  c-structs are utterly static and cannot be abstract, extended, or dynamic.
--  [You can embed c-structs and/or use unions, for details read a C manual,
--   or rather instead just use a published C API without fully grokking it.]
--
--
--  Class structs
--  =============
--
--  Classes are superficially similar to structs, however in structs all fields 
--  are implicitly public whereas they are private by default in classes, plus
--  structs cannot be dynamic (see below), and typically classes have methods, 
--  while structs normally do not.
--
--      [global][abstract] 
--      class <name> [nullable][dynamic][extends <base>{,<base>}]
--          {[private[:]|public[:]]
--           (<type> <field> [= <expression>]{, <field> [= <expression>]})|
--           ((procedure|function) <method>(<args>)(<method_body>|;))}
--      [global][abstract] class <name> [nullable][extends base{,base}]
--          {(<builtin>|<class>|"function"|"procedure") <name> ["=" default][",",..]}
--      end class
--
--  A class can extend another as follows:
--
--      class person
--          string name
--          atom age
--      end class
--          
--      class employee extends person
--          integer employee_number
--      end class
--
--  An employee e will have e.name, e.age, and e.employee_number fields.
--  An error will occur should there be any attempt to redefine a field,
--  whereas methods are cheerfully overwritten.
--
--  Alternatively, a class can be embedded in another class as follows:
--
--      class employee2
--          person p
--          integer employee_number
--      end class
--
--  An employee2 e will have e.p, e.p.name, e.p.age, and e.employee_number.
--
--      class point
--          integer x,y
--      end class
--
--      class line
--          point a, b
--      end class
--
--  A line l has l.a, l.b (both points) and l.a.x, l.a.y, l.b.x, and l.b.y.
--  Note that embedding allows two x and two y, which is not possible via
--  extending. The field names of a structure are private, and have no
--  useful meaning outside the structure, or any conflict with similarily
--  named fields in entirely different structures.
--
--  There is in fact a base class type that underpins all of this, ditto
--  struct, which can also be used for fully generic variables/parameters:
--
--      class thing;
--      class thing = something_else
--      class one, two, three
--      function somefunc(class s)
--
--  Note that the trailing ';' on the first line is mandatory, otherwise 
--  it will expect fields/"end class", though the "=,)" above also act
--  as delimiters to indicate "variable not definition" handling is rqd.
--  (class is however syntax coloured as a keyword rather than a type.)
--
--
--  Methods
--  =======
--
--  Methods are stored as plain integer routine_ids. In version 1, no 
--  checking is applied beyond any already present in call_func/proc.
--
--  All methods have an implicit "this" as the first parameter, which is also
--  implicitly supplied on all call statements. Note that c.show is just a plain
--  integer at heart, also dot notation in c.show() provides an implicit this,
--  but otherwise works via a completely separate mechanism to class and struct
--  handling, namely first-class routine_ids. There is significantly less compile-
--  time checking of the number and type of parameters than usual, however.
--   [ie/eg length(1) won't compile but o.len(1) will, even though if shouldn't.]
--
--
--  Dynamic structs
--  ===============
--
--  Externally similar to a class-struct, but "dynamic" is specified after name.
--  Based on an internal dictionary, there is no restriction that fields/attributes 
--  must be pre-defined, which obviously means that simple typos will not be caught, 
--  and likewise without a specified type there is no check on the values being set 
--  on fields which have not been formally declared.
--
--      class flexible dynamic [extends base]
--          <optional formal/fixed field declarations, as above>
--      end class
--
--      flexible f = new()
--      f.thing = "thing"
--
--  All such dynamically declared fields are treated as type object.

--  I am not convinved there is any practical value to extending dynamics.
--  If they are, however, any and all derived classes are also dynamic.
--  At some point I decided not to allow dynamic structs... [??DEV??]
--
--
--  Nullable structs
--  ================
--
--      class employeen nullable extends employee end class
--
--      class boss extends employee
--          employeen assistant
--      end class
--
--      employeen n = myboss.assistant
--
--  Here n declares a variable that can be an employee or NULL, as above this
--  may be used for directly declared fields but it is also equally valid for
--  dynamically specified fields. Were you to code object n = boss.assistant
--  then there would be no "dot notation" on n, even after non-null checking,
--  at least not until it was re-assigned into a suitable struct-variable.
--  Obviously a fatal error occurs should dot notation be used on a NULL n.
--
--  Like dynamic, nullable is propagated non-optionally to all derivatives.
--
--
--  Advanced structs
--  ================
--
--  Internally, fields are referenced by name, so s.age is equivalent to
--  s["age"] (as long as the compiler knows s is a struct). The compiler 
--  is at liberty to translate references to an internal integer form,
--  when it can (early binding), or leave things till run-time (aka late
--  binding). The programmer should not attempt to use integer indexes
--  on structs, except perhaps via explicit calls to the routines below,
--  at which point they should probably question their design decisions.
--  Obviously s.var is equivalent to s["var"] rather than s[var]; use a
--  string for dynamic field references, rather than dot notation. (By
--  dynamic, I mean a field the compiler will not know about, and hence
--  could never apply any optimisations to anyway.)
--
--  You can store structs in traditional phix sequences (lists/tables),
--  however the compiler needs type hints to interpret [] correctly, and
--  will assume integer sequence subscripting unless it detects struct.
--  Hence you cannot code sequence points and expect points[4].x to work,
--  instead you must code (eg) point p = points[4], and then p.x.
--
--
--  Delete Routines
--  ===============
--
--  By default structs are managed and destroyed automatically when the
--  reference count drops to zero, via delete_routine(). This means that
--  you cannot attach another delete_routine (unlike Euphoria, Phix does
--  not allow them to be daisy-chained). You can of course store structs
--  inside a sequence or table entry and attach a delete_routine to that
--  or store a delete_routine()'d atom or sequence inside a struct.
--

--DEV: This didn't make it into the docs:
--
--  Dot notation is however transitive:
--
--      object p
--      --  p.x = 1 -- error (compile-time, run-time, or both)
--      struct s = p -- (weak/inefficient, see below)
--          s.x = 1 -- fine (as long as it exists at run-time)
--      struct nest
--          object o
--      end struct
--      nest n
--          n.o.x = 1   --  "" (, and as below)
--
--  In other words, p.x is invalid, however since n.o is permitted, that
--  permitted behaviour transfers seamlessly to the (unknown) inner o.x;
--  if the leftmost dot notation is acceptable, it carries on rightwards,
--  despite both p.x and (n.o).x being (object).x only the second is ok.
--  Recall that a key goal is the compiler //understanding// structs and
--  making p.x invalid encourages the programmer to offer it better clues.
--  The "struct x = O" is deemed weak in that a more specific class/struct 
--  should allow at least some such references to be properly optimised.
--
--  As well as dot notation, structs can be referenced by string names, eg:
--
--      s["name"] = "boris"
--      s["age"] = 3.75
--
--  Obviously that is more useful with say string field = "name", and then
--  s[field], rather than literal inline strings. Attempting s.field would 
--  be equivalent to s["field"] rather than the intended s["name"], iyswim.
--  Note that for this handling to occur, s must be declared as a struct,
--  and the subscript must be declared as a string, both must be detected
--  at compile-time, otherwise a fatal "subscript is not an atom" triggers.
--      
--  

include cffi.e as cffi
include dict.e  -- (not really rqd, but used below so we may as well)

-- struct flags:
--DEV move into syminit()[??]:
global
constant S_STRUCT       = #01,  -- (declared using "struct" [for error messages])
         S_CLASS        = #02,  -- (declared using "class"  [for error messages])
         S_DYNAMIC      = #03,  -- (dynamic class; dictionary-based)
--?      S_DYNAMIC      = #06,  -- (dynamic class; dictionary-based)
--       S_SEQUENCE     = #04,  -- (struct extends sequence; sequence-based) -- **incomplete/undocumented**
--       S_SEQUENCE     = #05,  -- (struct extends sequence; sequence-based) -- **incomplete/undocumented**
         S_CFFI         = #09   -- (struct c <"def">; cffi-based)
constant S_CORE         = #0F   -- (mask for S_STRUCT..S_CFFI)

global
constant S_ABSTRACT     = #10,
         S_NULLABLE     = #20
--constant C_FLAGS      = #30,  -- (mask for abstract/nullable)
constant S_ALL          = #3F

-- Note: sequence-based structs were reasonably easy to support in this source, but 
--       proved difficult to handle fully in pmain.e, particularly when mix-nested 
--       with non-sequence-based fields, ie s.a.b could be s[4][ax][4][bx], or need
--       (partial) fetch/modify/store, etc. Left incomplete for now, although they 
--       do offer a slim promise of better performance and more familar debugging.

-- field flags:
global 
constant SF_PUBLIC  = #00,  -- (NB: pmain.e uses "false" for this...)
         SF_PRIVATE = #01,  --              (and "true" for this...)
         SF_PROC    = #10,  -- (as integer/routine_id)
         SF_FUNC    = #20,  -- (         """		 )
--constant SF_RTN   = or_bits(SF_PROC,SF_FUNC)
         SF_RTN     = #30

-- struct types:
--DEV/SUG move into syminit():
global constant ST_INTEGER  =  1, -- (==T_integer)
                ST_ATOM     =  3, -- (==T_atom)
                ST_STRING   =  8, -- (==T_string)
                ST_SEQUENCE = 12, -- (==T_sequence)
                ST_OBJECT   = 15  -- (==T_object)
-- (other user defined types and prior structs need the routine_id of the type)

enum S_NAME, S_TID, S_FLAGS, S_SDX, S_BDX, S_FIELDS, S_DEFAULT -- (for use on definitions, first 3 to S_FIELDS as well)
enum I_STRUCT, I_NAME, I_SDX, I_DATA -- (for use on individual instances)

enum C_FREELIST, C_INSTANCES -- (for use on class-struct instances table only)

sequence structs,   -- elements are {name,tid,flags,sdx,base,{fields},default} [S_NAME..S_DEFAULT]
                    -- fields are {name,tid,flags} [also S_NAME..S_FLAGS]
         instances  -- elements are 0 for > S_CLASS (ie dynamic/cffi)
                    -- otherwise {freelist,{{fields}}}. [C_XXX]

integer vtable = -1 -- master multi-use dictionary, of all struct definitions:
                    -- key is struct_name, data is sdx (ie idx to structs),
                    -- key is {"struct",rid}, data is sdx (""),
                    -- key is {sdx,"field"}, data is fdx (ie idx to structs[sdx][S_FIELDS])
                    -- key is rid, data is struct defaults (===structs[sdx][S_DEFAULTS])
                    -- key is {"extends",sdx}, data is flattened list of parents
                    -- key is {"name",rid}, data is struct_name (inc. ST_INTEGER..ST_OBJECT)

function valid_type(integer tid, object v)
-- internal: type check an individual field setting
    switch tid do
        case ST_INTEGER:  return integer(v)
        case ST_ATOM:     return atom(v)
        case ST_STRING:   return string(v)
        case ST_SEQUENCE: return sequence(v)
        case ST_OBJECT:   return true
        else              return call_func(tid,{v})
    end switch
    ?9/0
end function

function is_cdx(object idx, integer sdx)
-- internal: only for use on known <=S_CLASS's [I_DATA]
    if vtable=-1 then ?9/0 end if
    sequence isc = instances[sdx][C_INSTANCES]
    return integer(idx) and idx>=1 and idx<=length(isc) and sequence(isc[idx])
end function

global function is_struct(object s, integer rid)
-- rid should be routine_id of the udt, or 0 for the generic "struct"/"class".
-- (this is global so that it can be called from the implicitly-defined 
--  user-defined-type that is created automatically from eg "struct bridge",
--  that is, rather than something you would ever want to call directly.)
integer sdx, flags, stype
    if vtable!=-1 then
--      if sequence(s)  then
        if sequence(s) and still_has_delete_routine(s) then
            integer l = length(s)
            if l=4 and string(s[I_STRUCT]) and s[I_STRUCT]="struct"
                   and string(s[I_NAME])
                   and integer(s[I_SDX]) then
                sdx = s[I_SDX]
                if sdx>=1 and sdx<=length(structs) and structs[sdx][S_NAME]=s[I_NAME] then
                    flags = structs[sdx][S_FLAGS]
                    stype = and_bits(flags,S_CORE)
                    if rid!=0 and rid!=structs[sdx][S_TID] then
                        if stype=S_CFFI then return false end if
                        integer cdx = getd({"struct",rid},vtable)
                        object cdii = getd({"extends",sdx},vtable)
                        if cdii=NULL or not find(cdx,cdii) then return false end if
                    end if
                    object d = s[I_DATA]
                    switch stype do
                        case S_STRUCT,S_CLASS:  return is_cdx(d,sdx)
                        case S_DYNAMIC:         return is_dict(d)
                        case S_CFFI:            return atom(d) and d!=NULL
                    end switch
                    ?9/0 -- unknown stype??
                end if -- sdx ok
            end if -- length 4 and {"struct",string,integer}
--16/12/20: [struct(0)/class(0) and similar should return false, not crash]
--      elsif s=NULL then
        elsif s=NULL and rid!=0 then
            sdx = getd({"struct",rid},vtable)
            -- (serious internal error, next line is fatal anyway:)
--          if sdx=0 then throw("unknown/invalid struct") end if
            flags = structs[sdx][S_FLAGS]
            flags = and_bits(flags,S_NULLABLE)
--SUG:
--          if flags!=S_NULLABLE then
--              <check callstack for [CRID]>/flags := S_NULLABLE
--          end if
            return flags==S_NULLABLE
        end if
    end if
    return false
end function

global type struct(object s)
--
--  The root-level (abstract) struct type. Aliased to class in psym.e.
--  UDTs auto-generated by "struct struct_name" follow this pattern, but
--  with (eg) routine_id("struct_name") as the 2nd param to is_struct().
--  (See pmain.e/DoStruct() for more details, if you really must.)
--
    return is_struct(s,0)
end type
constant r_struct = routine_id("struct")
            
procedure struct_init()
-- usage: if vtable=-1 then struct_init() end if
    if vtable!=-1 then ?9/0 end if
    vtable = new_dict()
    setd("struct",1,vtable)
--  setd("sequence",2,vtable)
    setd(ST_INTEGER,0,vtable)   -- (==false)
    setd(ST_ATOM,0,vtable)
    setd(ST_STRING,"",vtable)
    setd(ST_SEQUENCE,{},vtable)
    setd(ST_OBJECT,0,vtable)
    integer flags = S_DYNAMIC+S_ABSTRACT
    -- elements are {name,tid,flags,sdx,base,{fields},default}
    structs = {{"struct",r_struct,flags,1,0,{},{}}}
--             {"sequence",r_struct,flagq,2,0,{},{}}}
    instances = {0}
    setd({"name",ST_INTEGER},"ST_INTEGER",vtable)
    setd({"name",ST_ATOM},"ST_ATOM",vtable)
    setd({"name",ST_STRING},"ST_STRING",vtable)
    setd({"name",ST_SEQUENCE},"ST_SEQUENCE",vtable)
    setd({"name",ST_OBJECT},"ST_OBJECT",vtable)
end procedure

function gather(integer bdx)
    sequence res = {bdx}
    object s = getd({"extends",bdx},vtable)
    if sequence(s) then
        for i=1 to length(s) do
            bdx = s[i]
            res &= bdx & gather(bdx)
        end for
    end if
    return res
end function

procedure struct_endorse(integer sdx, bdx)
    -- record "sdx extends bdx" (by now both ints)
    -- anything that bdx has extended also needs a mention...
    -- This satisfies the inner "!=[S_TID]" of is_struct():
    --      if class a=5, b=6, ab=7=={5,6} then passing a 7
    --      to a type wanting 5/6 fetches/scans that {5,6}...
    sequence es = {"extends",sdx},
             gb = gather(bdx)
--??setd_default({},vtable) -- nah, we probably rely on null already...
    object s = getd(es,vtable)
    if s=NULL then s={} end if
    setd(es,s&gb,vtable)
end procedure

integer sdx = 0

global procedure struct_start(integer flags, string struct_name, integer rid=routine_id(struct_name), string base_name="")
--
-- valid flags are: S_STRUCT/S_CLASS/S_DYNAMIC/S_CFFI, + S_ABSTRACT|S_NULLABLE
-- for S_CFFI, base_name is actually the C typdef string, rather than a parent name.
--
    if sdx!=0 then ?9/0 end if
    if vtable=-1 then struct_init() end if
    sdx = getd(struct_name,vtable)
    if sdx!=NULL then ?9/0 end if
    sdx = length(structs)+1
    setd(struct_name,sdx,vtable)
    setd({"struct",rid},sdx,vtable)
    setd({"name",rid},struct_name,vtable)
    if and_bits(flags,S_ALL)!=flags then ?9/0 end if
    integer bdx = 0,
            stype = and_bits(flags,S_CORE)

    if stype=S_CFFI then
        if and_bits(flags,S_ABSTRACT) then ?9/0 end if
--19/2/21:
--      bdx = cffi:define_struct(base_name)
        try
            bdx = cffi:define_struct(base_name)
        catch e
--          crash(e[E_USER],{},1)
            throw(e[E_USER])
        end try
    else
        if base_name!="" then
            if find(base_name,{"class","struct","sequence"}) then ?9/0 end if
            bdx = getd(base_name,vtable)
            if bdx=NULL then ?9/0 end if
            if structs[bdx][S_NAME]!=base_name then ?9/0 end if
            if structs[bdx][S_SDX]!=bdx then ?9/0 end if
            integer pflags = structs[bdx][S_FLAGS]
            pflags -= and_bits(pflags,S_ABSTRACT)
            flags = or_bits(flags,pflags)
        end if
    end if
    -- elements are {name,rid,flags,sdx,base,{fields},default}
    structs = append(structs,{struct_name,rid,flags,sdx,bdx,{},{}})
    instances = append(instances,iff(stype<=S_CLASS?{0,{}}:0))
    if stype=S_CFFI then
        sdx = 0
    elsif bdx then
        sequence fields = structs[bdx][S_FIELDS]
        structs[sdx][S_FIELDS] = fields
        structs[sdx][S_DEFAULT] = structs[bdx][S_DEFAULT]
        for i=1 to length(fields) do
            -- [key is {sdx,"field"}, data is fdx]
            string name = fields[i][S_NAME]
            setd({sdx,name},i,vtable)
        end for
        struct_endorse(sdx,bdx)
    end if
end procedure

global procedure extend_struct(string struct_name, base_name="")
-- "x extends a,b"... ==> start_struct("x","a"); end_struct(); extend_struct("x","b")...
    if sdx!=0 then ?9/0 end if
    if vtable=-1 then ?9/0 end if
    sdx = getd(struct_name,vtable)
    if sdx=0 then ?9/0 end if
    integer flags = structs[sdx][S_FLAGS],
            stype = and_bits(flags,S_CORE)
    -- (the compiler should never attempt this...)
    if stype=S_CFFI then ?9/0 end if
    if base_name!="" then
        if find(base_name,{"class","struct","sequence"}) then ?9/0 end if
        integer bdx = getd(base_name,vtable)
        if bdx=NULL then ?9/0 end if
        if structs[bdx][S_NAME]!=base_name then ?9/0 end if
        if structs[bdx][S_SDX]!=bdx then ?9/0 end if
        integer pflags = structs[bdx][S_FLAGS]
        pflags -= and_bits(pflags,S_ABSTRACT)
        integer eflags = or_bits(flags,pflags)
        if eflags!=flags then ?9/0 end if
        sequence fields = structs[bdx][S_FIELDS],
                 defaults = structs[bdx][S_DEFAULT]
        for i=1 to length(fields) do
            -- [key is {sdx,"field"}, data is fdx]
            sequence field = fields[i]
            string name = field[S_NAME]
            object dflt = defaults[i]
            sequence sn = {sdx,name}
            integer fdx = length(structs[sdx][S_FIELDS])+1,
                    pfx = getd(sn,vtable)
            if pfx!=NULL then
                -- redefinition: valid on methods, but an error on fields.
                fdx = pfx
                integer field_flags = field[S_FLAGS],
                        tid = field[S_TID]
                if structs[sdx][S_FIELDS][fdx][S_FLAGS]!=field_flags then ?9/0 end if
                if structs[sdx][S_FIELDS][fdx][S_TID]!=tid then ?9/0 end if
                if structs[sdx][S_FIELDS][fdx][S_NAME]!=name then ?9/0 end if
                if and_bits(field_flags,SF_RTN) then
                    if not integer(dflt) then ?9/0 end if   -- (rid)
                    structs[sdx][S_DEFAULT][fdx] = dflt     -- overwrite
                else
                    -- [allow identical, for diamond inheritance...]
                    if structs[sdx][S_DEFAULT][fdx]!=dflt then ?9/0 end if
                end if
            else
                setd(sn,fdx,vtable)
                structs[sdx][S_FIELDS] = append(structs[sdx][S_FIELDS],field)
                structs[sdx][S_DEFAULT] = append(structs[sdx][S_DEFAULT],dflt)
            end if
        end for
        struct_endorse(sdx,bdx)
    end if
end procedure

global procedure struct_add_field(string name, integer tid, field_flags=0, object dflt=NULL, bool bDflt=false)
--
-- tid should be one of the ST_INTEGER..ST_OBJECT constants, or the routine_id of a type.
-- field_flags should be SF_PRIVATE|SF_PUBLIC [+SF_PROC|SF_FUNC]
-- dflt/bDflt work in tandem: either neither is passed and a NULL dlft gets replaced 
--  with a more type-safe setting, such as "" for ST_STRING, whereas a true in bDflt
--  means "use NULL as the actual default, if that's what it happens to be".
--
    if sdx=0 then ?9/0 end if
    integer flags = structs[sdx][S_FLAGS],
            stype = and_bits(flags,S_CORE)
    -- (the compiler should never attempt this...)
    if stype=S_CFFI then ?9/0 end if
    if vtable=-1 then ?9/0 end if
    sequence sn = {sdx,name}
    integer fdx = length(structs[sdx][S_FIELDS])+1,
            pfx = getd(sn,vtable)
--p2js:
--  structs[sdx] = deep_copy(structs[sdx])
    if pfx!=NULL then
        -- redefinition: valid on methods, but an error on fields.
        --  (the latter should be caught in DoStruct(), not here)
        if not and_bits(field_flags,SF_RTN) then ?9/0 end if
        if not integer(dflt) then ?9/0 end if
        fdx = pfx
        structs[sdx][S_DEFAULT][fdx] = dflt
    else
        setd(sn,fdx,vtable)
        if dflt=NULL and not bDflt then
            dflt = getd(tid,vtable)
        end if
        sequence field = {name,tid,field_flags}
        structs[sdx][S_FIELDS] = append(structs[sdx][S_FIELDS],field)
        structs[sdx][S_DEFAULT] = append(structs[sdx][S_DEFAULT],dflt)
    end if
end procedure

global procedure end_struct()
    if sdx=0 then ?9/0 end if
    if vtable=-1 then ?9/0 end if
    -- DEV finish setting up the default...? (will this work for DYNAMIC?)
    integer rid = structs[sdx][S_TID]
    object dflt = structs[sdx][S_DEFAULT]
--should this not be a fully-fledged new()??? [no, the default is now NULL]
--  setd(rid,dflt,vtable)
    sdx = 0
end procedure

function struct_dx(object s, bool rid=false)
-- nb: compiler's struct table may differ from run-time version,
--     hence the compiler *must not* emit idx from one 't other.
    integer sdx = 0
    if vtable!=-1 then
        if string(s) then
            sdx = getd(s,vtable)
--      elsif struct(s) then
        elsif sequence(s) then
            sdx = s[I_SDX]
        elsif rid and integer(s) then
            sdx = getd({"struct",s},vtable)
        end if
    end if
--  if sdx=0 then throw("unknown/invalid struct") end if
    return sdx
end function

global function get_struct_type(object s)
--
-- s can be a string, routine_id, or an instance, eg after say
--  "struct person end struct person p = new()", then s can be
--      the string "person", 
--      the integer routine_id("person"), or 
--      the struct p
--  (where possible, the compiler is at liberty to optimise away calls,
--   aka early or compile-time binding w/o any run-time invocations,
--   at least that is in the code it emits rather than code you write,
--   and in practice that will only ever be for the routine_id calls.)
--
--  Returns S_STRUCT/S_CLASS/S_DYNAMIC/S_CFFI
--? Returns S_STRUCT/S_CLASS/S_DYNAMIC/S_CFFI or 0
--
    if vtable=-1 then struct_init() end if -- (structdx() crashes either way)
--? if vtable=-1 then return 0 end if
    integer sdx = struct_dx(s,true)
    if sdx=0 then return 0 end if
    integer flags = structs[sdx][S_FLAGS],
            res = and_bits(flags,S_CORE)
    return res
end function

global function get_struct_name(object s)
--
-- s as per get_struct_type
-- returns a string name
--
    integer sdx = struct_dx(s,true)
    string res = structs[sdx][S_NAME]
    return res
end function

constant StructFlagSet = {{S_ABSTRACT, "S_ABSTRACT"},
                          {S_NULLABLE, "S_NULLABLE"}}

global function get_struct_flags(object s, bool bAsText=false)
--
-- s as per get_struct_type
-- Returns [S_ABSTRACT]+[S_NULLABLE].
--
    integer sdx = struct_dx(s,true),
            flags = structs[sdx][S_FLAGS],
            res = and_bits(flags,S_ABSTRACT+S_NULLABLE)
    if bAsText then
        string text = decode_flags(StructFlagSet,res)
        return text
    end if
    return res
end function

global function get_struct_fields(object s)
-- reflection: returns a read-only copy of fields, to do with for whatever..
-- result is a table of field definitions
    integer sdx = struct_dx(s,true),
            flags = structs[sdx][S_FLAGS],
            stype = and_bits(flags,S_CORE)
    if stype=S_CFFI then ?9/0 end if
    sequence res
    if stype=S_DYNAMIC
    and not string(s)
    and not integer(s) then 
        -- if s is a dynamic instance, get fields as actually set,
        -- otherwise (string name or integer rid) just predefined.
        integer tid = s[I_DATA]
        res = getd_all_keys(tid)
    else
        res = structs[sdx][S_FIELDS]
    end if
    return res
end function

function field_dx(object sdx, string name)
-- NB can return 0 (and will on all S_CFFI structs).
-- sdx can be a string (name), struct (instance), or an already known
--  integer, typically the result from a direct struct_dx() call, but
--  (unlike struct_dx) it /cannot/ be a routine_id.
--  
--  if string(sdx)          \
--  or integer(sdx) then    / NO!!
    if string(sdx) then
        sdx = struct_dx(sdx,true)
    elsif struct(sdx) then
        sdx = sdx[I_SDX]
    end if
    integer fdx = getd({sdx,name},vtable)
    return fdx
end function

procedure no_no_no(sequence s)
--  ?{"no_no_no",s}
    ?9/0
--  trace(1)
end procedure

--procedure destroy_instance(struct s)
procedure destroy_instance(sequence s)
--?{"destroy_instance",s}
    integer sdx = struct_dx(s),
            cdx = s[I_DATA],
            flags = structs[sdx][S_FLAGS],
            stype = and_bits(flags,S_CORE),
            dtor = field_dx(sdx, "~"&structs[sdx][S_NAME])
--2/2/21: (find any parent destructor) [erm.. test w/o this first!]
--/*
    if dtor=NULL then
        object cdii = getd({"extends",sdx},vtable)
        if cdii!=NULL then
            for i=length(cdii) to 1 by -1 do
                integer cdx = cdii[i]
                dtor = field_dx(cdx, "~"&structs[cdx][S_NAME])
                if dtor then exit end if
            end for
        end if
    end if
--*/
    if dtor then -- call destructor, if present
        dtor = structs[sdx][S_DEFAULT][dtor]
        --
        -- Now that struct() invokes still_has_delete_routine(), must
        -- put something (anything, although normal/original is fine)
        -- back temporarily, not that it'll ever be called (we hope).
        -- [btw, delete_routine will barf if s already has something,
        --       but it shd be gone b4 destroy_instance was invoked.]
        --
--      s = delete_routine(s,destroy_instance)
        s = delete_routine(s,no_no_no)
--trace(1)
        dtor(s)
        s = delete_routine(s,0)
--if still_has_delete_routine(s) then ?9/0 end if
    end if
    if stype=S_DYNAMIC then
        destroy_dict(cdx)
    elsif stype=S_CFFI then
        ?9/0    -- should not be delete_routine()'d [by new()]...
        -- (plus, c_structs should never have a de[/con]structor)
    else
        integer freelist = instances[sdx][C_FREELIST]
        instances[sdx][C_FREELIST] = cdx
        instances[sdx][C_INSTANCES][cdx] = freelist
    end if
end procedure
constant r_destroy_instance = routine_id("destroy_instance")

global function new(object sdx, sequence imm={})
--
-- sdx should be a string such as "person" identifying the
--      struct, or equivalently routine_id("person"), with the
--      compiler automatically providing the latter in the case
--      of eg "person p = new()", ie when it can safely be
--      deduced from the variable declaration type (/context).
--      [see pmain.e/ParamList(),T_new].
-- imm [optional] can specify the initial field settings.
--      In the `person p = new({"boris",5.75})` case, the 
--      compiler automatically inserts a suitable default
--      for sdx ahead of imm, ie when it is clearly not a
--      string or int, and only 1 parameter was provided.
--      If a constructor procedure exists, it is invoked
--      with imm as parameters, otherwise the fields are
--      extracted in simple numeric index order from imm.
--
--X
-- The compiler will provide a default for sdx when it can:
-- person p = new() is equivalent to person p = new("person"),
-- though in fact the compiler provides routine_id("person")
-- rather than the string. It will even squidge an sdx ahead
-- of an imm sequence for you. However if you try something
-- like employee e = new({new(),empid}) and expect the inner
-- new() to automatically get "person", it will probably fail.
-- Maybe one day in the future that might work, but not now.
-- Note the outer new() does get "employee" automatically.
-- Obviously e = new({new("person"),empid}) is however fine.
--
-- I should also hope it is equally obviously that something
-- like object o = new() will fail because "object" is not a
-- known structure, however object o = new("person") is fine.
-- Likewise passing a single object parameter to new() means
-- the compiler will have no idea if you meant sdx or imm,
-- however it will simply trust any pair of two arguments.
--
-- Examples:
--      person p1 = new(),
--             p2 = new("person"),
--             p3 = new("person",{"boris",4.75}),
--             p4 = new({"boris",4.75})
--      employee e1 = new({p4,"Prime Minister!!!"})
--
--  As shown in the last example, it is probably smartest to
--  create nested structures individually rather than inline.
--
--  Housekeeping is performed automatically when needed (not 
--  on sequence-structs) via delete_routine(). 
--  In the case of class and dynamic structs, it is on the
--  whole result, so if you want one yourself you will need
--  to put it on a selected (non-struct) field instead.
--  In the case of c-structs, it is on the pMem field, so
--  for that and sequence-structs you are free to put your
--  own delete_routine() on the whole thing.
--  [SUG: allow one to be specified on a per-class basis.]
--
    sdx = struct_dx(sdx,true)
    string sname = structs[sdx][S_NAME]
    integer flags = structs[sdx][S_FLAGS],
            stype = and_bits(flags,S_CORE),
            ctor = field_dx(sdx, sname)
    if and_bits(flags,S_ABSTRACT) then
        -- (more usually caught at compile-time)
        string cs = iff(stype=S_STRUCT?"struct":"class")
        crash("attempt to instantiate abstract %s",{cs},2)
    end if
    sequence dflts = structs[sdx][S_DEFAULT],
             fields = structs[sdx][S_FIELDS]
    if ctor then ctor = dflts[ctor] end if
    if length(imm) then
        if ctor=0 then
--DEV done... [needs more testing]
            --DEV/SUG: make cffi:set_struct_field() accept an object field in
            --  the same way that cffi:get_field_details() does, then we can
            --  use the former to assign each field in this loop.
--          if stype=S_CFFI then ?9/0 end if
            if stype=S_CFFI and dflts!={} then ?9/0 end if
            for i=1 to length(imm) do
                object ii = imm[i]
                if stype=S_CFFI then
                    if not atom(ii) then -- (rqd by cffi:set_struct_field()...)
                        crash("imm[%d] invalid type: %v\n",{i,ii},2)
                    end if
                    dflts &= ii
                else
                    if not valid_type(fields[i][S_TID],ii) then
                        crash("imm[%d] invalid type: %v (%v)\n",{i,ii,fields[i]},2)
                    end if
--p2js:
--                  dflts = deep_copy(dflts)
                    dflts[i] = ii
                end if
            end for
        elsif stype=S_CFFI then
            ?9/0 -- c_structs should never have a con[/de]structor!
        end if
    end if
    sequence res
    if stype<=S_CLASS then
        integer cdx = instances[sdx][C_FREELIST]
        if cdx=0 then
            instances[sdx][C_INSTANCES] = append(instances[sdx][C_INSTANCES],dflts)
            cdx = length(instances[sdx][C_INSTANCES])
        else
            instances[sdx][C_FREELIST] = instances[sdx][C_INSTANCES][cdx]
            instances[sdx][C_INSTANCES][cdx] = dflts
        end if
        res = {"struct",sname,sdx,cdx}
        res = delete_routine(res,r_destroy_instance)
        if ctor then
            res = {res}&imm -- (done that way for ref count reasons)
            res = call_func(ctor,res)
        end if
    elsif stype=S_DYNAMIC then
--      integer pid = structs[sdx][S_BDX]
--      integer tid = new_dict(pid)
--DEV will this work for embedded? (I /think/ its fine for extends)
        integer tid = new_dict()
        for i=1 to length(dflts) do
            string field = fields[i][S_NAME]
            setd(field,dflts[i],tid)
        end for
        res = {"struct",sname,sdx,tid}
        if ctor then res = call_func(ctor,{res}&imm) end if
        res = delete_routine(res,r_destroy_instance)
    elsif stype=S_CFFI then
        if ctor then ?9/0 end if
        integer id = structs[sdx][S_BDX]
        atom pMem = cffi:allocate_struct(id)
        for i=1 to length(dflts) do
--          string field = fields[S_NAME]
--          cffi:set_struct_field(id,pMem,field,dflts[i])
            cffi:set_struct_field(id,pMem,i,dflts[i])
        end for
        res = {"struct",sname,sdx,pMem}
    else
        ?9/0
    end if
    return res
end function

global procedure set_field_default(string struct_name, field_name, object v)
    integer sdx = struct_dx(struct_name),
            flags = structs[sdx][S_FLAGS],
            stype = and_bits(flags,S_CORE)
    -- (the compiler should never attempt this...)
    if stype=S_CFFI then ?9/0 end if -- (no defaults, ever, as per docs)
    integer fdx = getd({sdx,field_name},vtable),
            tid = structs[sdx][S_FIELDS][fdx][S_TID]
    if not valid_type(tid,v) then ?9/0 end if
    structs[sdx][S_DEFAULT][fdx] = v
end procedure

global procedure store_field(struct s, string field_name, object v, context=0)
    integer sdx = s[I_SDX],
            stype = and_bits(structs[sdx][S_FLAGS],S_CORE)
    if stype=S_CFFI then
        integer id = structs[sdx][S_BDX]
        atom pMem = s[I_DATA]
        cffi:set_struct_field(id,pMem,field_name,v)
    else
        integer fdx = getd({sdx,field_name},vtable), tid
        if fdx=NULL then
            if stype!=S_DYNAMIC then
                crash("no such field (%s)",{field_name},2)
            end if
        else
            tid = structs[sdx][S_FIELDS][fdx][S_TID]
            if not valid_type(tid,v) then
                string sname = structs[sdx][S_NAME]
                crash("type error assigning %v to %s.%s",{v,sname,field_name},2)
            end if
            if context!=0 then context = struct_dx(context,true) end if
            if context!=sdx
            and and_bits(structs[sdx][S_FIELDS][fdx][S_FLAGS],SF_PRIVATE) then
                object cdii = getd({"extends",sdx},vtable)
                if cdii=NULL or not find(context,cdii) then
                    -- 22/12/20 (setter handling)
                    if stype<=S_CLASS then
                        integer setter = field_dx(sdx,"set_"&field_name)
                        if setter!=0 then
                            integer flags = structs[sdx][S_FIELDS][setter][S_FLAGS]
                            if flags=SF_PROC then -- (and not SF_PRIVATE, obvs)
                                integer setfn = instances[sdx][C_INSTANCES][s[I_DATA]][setter]
                                -- note: this is ineffective under compilation (itself), due to
                                --       a get_routine_info(-9) in p.exw, that is your app will
                                --       be fine, but p.exe itself does not ever do this stuff.
                                --       obviously symtab name population mid compile not good
                                {integer maxp, integer minp, string sig} = get_routine_info(setfn,false)
                                if maxp>=2 and minp<=2 and sig[1..2] = "PO" then
                                    call_proc(setfn,{s,v}) -- (this,v)
                                    return
                                end if
                            end if
                        end if
                    end if
                    crash("attempt to modify private field (%s)",{field_name},2)
                end if
            end if
        end if
        if stype<=S_CLASS then
            integer cdx = s[I_DATA]
--p2js: (DEV completely unacceptable...)
--          instances[sdx][C_INSTANCES][cdx] = deep_copy(instances[sdx][C_INSTANCES][cdx])
--          instances[sdx][C_INSTANCES] = deep_copy(instances[sdx][C_INSTANCES])
--          instances[sdx] = deep_copy(instances[sdx])
--?"structs.e line 1145"
--          instances = deep_copy(instances)
            instances[sdx][C_INSTANCES][cdx][fdx] := v
        elsif stype=S_DYNAMIC then
            tid = s[I_DATA]
            setd(field_name,v,tid)
        else
            ?9/0 -- stype is 0/has been corrupted
        end if
    end if
end procedure

global function fetch_field(struct s, string field_name, object context=0)
    integer sdx = s[I_SDX],
            stype = and_bits(structs[sdx][S_FLAGS],S_CORE),
            fdx = field_dx(sdx,field_name)
    if context!=0 then context = struct_dx(context,true) end if
    if context!=sdx
    and fdx!=0
    and and_bits(structs[sdx][S_FIELDS][fdx][S_FLAGS],SF_PRIVATE) then
        object cdii = getd({"extends",sdx},vtable)
        if not sequence(cdii)
        or not find(context,cdii) then
            -- 22/12/20 (getter handling)
            if stype<=S_CLASS then
                integer getter = field_dx(sdx,"get_"&field_name)
                if getter!=0 then
                    integer flags = structs[sdx][S_FIELDS][getter][S_FLAGS]
                    if flags=SF_FUNC then -- (and not SF_PRIVATE, obvs)
                        integer getfn = instances[sdx][C_INSTANCES][s[I_DATA]][getter]
                        -- note: this is ineffective under compilation (itself), due to
                        --       a get_routine_info(-9) in p.exw, that is your app will
                        --       be fine, but p.exe itself does not ever do this stuff.
                        --       obviously symtab name population mid compile not good
                        {integer maxp, integer minp, string sig} = get_routine_info(getfn,false)
                        if maxp>=1 and minp<=1 and sig[1..2] = "FO" then
                            return call_func(getfn,{s}) -- (this)
                        end if
                    end if
                end if
            end if
            crash("attempt to read private field (%s)",{field_name},2)
        end if
    end if
    object res = 0
    if fdx=0 and stype<=S_CLASS then
        if fdx=0 then crash("no such field:%s",{field_name},2) end if
        res = s[I_DATA][fdx]
    elsif stype<=S_CLASS then
        integer idx = s[I_DATA]
--object is = instances[sdx],
--     isc = instances[sdx][C_INSTANCES],
--     isci = instances[sdx][C_INSTANCES][idx]
        res = instances[sdx][C_INSTANCES][idx][fdx]
    elsif stype=S_DYNAMIC then
        integer tid = s[I_DATA]
        res = getd(field_name,tid)
    elsif stype=S_CFFI then
        integer id = structs[sdx][S_BDX]
        atom pMem = s[I_DATA]
        res = cffi:get_struct_field(id,pMem,field_name)
    else
        ?9/0 -- stype is 0/has been corrupted?
    end if
    return res
end function

global function get_field_default(object s, string field_name)
    integer sdx = struct_dx(s, true),
            stype = and_bits(structs[sdx][S_FLAGS],S_CORE)
    if stype=S_CFFI then ?9/0 end if -- (no defaults, ever, as per docs)
    integer fdx = field_dx(sdx,field_name)
    object res = iff(fdx=0?NULL:structs[sdx][S_DEFAULT][fdx])
    return res
end function

constant FieldFlagSet = {{SF_PUBLIC, "SF_PUBLIC"},
                         {SF_PRIVATE, "SF_PRIVATE"},
                         {SF_PROC, "SF_PROC"},
                         {SF_FUNC, "SF_FUNC"}}

global function get_field_flags(object s, string field_name, bool bAsText=false)
    integer sdx = struct_dx(s, true),
            flags = structs[sdx][S_FLAGS],
            stype = and_bits(flags,S_CORE)
    if stype=S_CFFI then ?9/0 end if -- (see docs)
    integer fdx = field_dx(sdx,field_name),
            res = iff(fdx=0?NULL:structs[sdx][S_FIELDS][fdx][S_FLAGS])
--  if res=NULL
    if fdx=0    -- (above will be so)
    and bAsText
    and stype=S_DYNAMIC
    and not string(s)
    and not integer(s) then 
        -- if field_name not predefined, and s is a dynamic instance
        -- (not string name or integer rid), and field_name has been
        -- set, then return ST_OBJECT (anything) instead of that NULL.
        integer tid = s[I_DATA]
        if getd_index(field_name,tid)!=NULL then
--          res = SF_PUBLIC -- (already is)
            fdx = 1 -- (let below happen)
        end if
    end if
    if bAsText and fdx!=0 then
        string text = decode_flags(FieldFlagSet,res)
        return text
    end if
    return res
end function

global function get_field_type(object s, string field_name, bool bAsText=false)
    integer sdx = struct_dx(s, true)
    if sdx=0 then return 0 end if
    integer stype = and_bits(structs[sdx][S_FLAGS],S_CORE)
    if stype=S_CFFI then ?9/0 end if -- (as per docs)
    integer fdx = field_dx(sdx,field_name),
            res = iff(fdx=0?NULL:structs[sdx][S_FIELDS][fdx][S_TID])
    if res=NULL
    and stype=S_DYNAMIC
    and not string(s)
    and not integer(s) then 
        -- if field_name not predefined, and s is a dynamic instance
        -- (not string name or integer rid), and field_name has been
        -- set, then return ST_OBJECT (anything) instead of that NULL.
        integer tid = s[I_DATA]
        if getd_index(field_name,tid)!=NULL then
            res = ST_OBJECT
        end if
    end if
    if bAsText and res!=NULL then
        object name = getd({"name",res},vtable) -- (can be NULL)
        return name
    end if
    return res
end function

global function struct_mem(object s)
    integer sdx = struct_dx(s),
            stype = and_bits(structs[sdx][S_FLAGS],S_CORE)
    if stype!=S_CFFI then ?9/0 end if
    return s[I_DATA]
end function


