--
-- p2js_scope.e
-- ============
--
--  a stack of dictionaries
--  [1] contains builtins (and is re-usable for next source)
--  [2] is top-level source. [2+] (maybe) for nested blocks
--  [3] will be a routine [3+] for nested blocks
-- dang, what about includes... [DONE, kinda]
-- key ttidx, data is type - int/atom/string/sequence/object, func/proc (or just int?!)
--? key of {"type",ttidx}, data is ... (Ihandle,complex[n],keyword?(-1?),dictionary...)
-- add_scope(), drop_scope(), add_local(), add_global(), get_type(), Xget_udt()X
-- TOKVTYPE? <- no, TOKALTYPE

-- includes -> getsource (once only), build tree with the right order [NO...]

-- keep top-level dicts, do a union for errors/clashes (also: link in for include statements!)
--  [no, just pile them all in!]

-- This file also deals with include files, and the effect that has on
-- the scope dictionaries. Or we could capture include information during
-- tokenisation, simply scanning for or even logging any T_include we find
-- and (effectively, if not in practice) bolting the token tables together.
-- It might be a good excuse to switch to a one-at-a-time tokeniser...
-- Why not: the tokeniser should deal with include files (push/pop style) [DONE]

-- Note that Phix adds a new scope for an include statement whereas pwa/p2js does not.
-- Since JavaScript does not have an include statement, pwa/p2js inlines the code and
-- therefore may trigger identifier clashes that separate files would not.

-- Note: p2js_parse.e is not calling add/drop_scope() to implement block scoping... [DEV]

sequence scopes = {}
enum BUILTINS,  -- (re-usable, set from p2js_keywords in init_scope just below)
     GLOBALS    -- (not that there's really such a thing in JavaScript........)
constant integer T_high = T_reserved[$]
sequence referenced
--sequence zeroes -- DEV temp

include p2js_auto.e
sequence {auto_names, auto_procfunc, auto_sigs, auto_files} = columnize(p2js_auto)
integer named_args,     -- key is {arg_rtn,ttidx}, 
                        -- data is {arg_idx,vartype}
        arg_defs        -- key is {arg_rtn,arg_idx},
                        -- data is "{}/ident/string/number" (always string)
                        -- nb no support for eg length(),platform() etc yet.    [ length now done ]

function unfudge(sequence s)
    -- remove some unnecessary nesting in p2js_depend, 
    --  which was added purely for aesthetic reasons.
    s = deep_copy(s)
    for i=1 to length(s) do
        s[i][2] = s[i][2][1]
        s[i][3] = s[i][3][1]
    end for
    return s
end function

include p2js_depend.e
sequence {autoincludes,dependencies,globals,arg_names} = columnize(unfudge(p2js_depend)),
          rtn_names = join(apply(true,vslice,{arg_names,1}),""),
          rtn_args  = join(apply(true,vslice,{arg_names,2}),"")

--traverse_dict(integer rid, object user_data=0, integer tid=1, bool rev=false)
-->{{"traverse_dict",{"integer","rid","?"},
--                   {"object","user_data","0"},
--                   {"integer","tid","1"},
--                   {"bool","rev","false"}}}
--  "traverse_dict" must exist in auto_names: {`traverse_dict`, `Proc`, `PIOII`, `dict.e`},
--  raise an error if length(auto_sigs[k]) does not match [that is, assuming auto_sigs is
--              fresh outa psym.e and args is from a fresh transpilation, so, yes, crash.]
--  TYPI..TYPO might be easier/all round better than "integer".."object".

--sequence sna, sad -- DEV/temp

global procedure init_scope()
--?{"init_scope",T_high}
    if length(scopes)=0 then
        integer builtins = new_dict(),
                globals = new_dict()
        scopes = {builtins,globals}
        named_args = new_dict()
        arg_defs = new_dict()
--DEV we probably shouldn't do this for autoincludes??
--better: we should build a repeat(false,length(T_keywords)) and []=true anything < T_reserved[$] [DONE]
--?auto_names
        for i=1 to length(T_reserved) do
if not match("complex",T_keywords[i])
and not find(T_keywords[i],{"from_polar","with_theta","with_rho"}) then
            setd(T_reserved[i],T_toktypes[i],builtins)
--          setd({'[',T_reserved[i]},i,builtins)
            integer k = find(T_keywords[i],auto_names)
            if k then
--?{{'[',T_reserved[i]},k,T_keywords[i]}
                setd({'[',T_reserved[i]},k,builtins)
--elsif i<50 then
--  ?{"?",T_keywords[i]}
            end if
end if
        end for
    else
        -- cleanup any mess left from a previous run...
        while length(scopes)>GLOBALS do
            destroy_dict(scopes[$])
            scopes = scopes[1..$-1]
        end while
        destroy_dict(scopes[GLOBALS], justclear:=true)
        destroy_dict(named_args, justclear:=true)
        destroy_dict(arg_defs, justclear:=true)
    end if
    referenced = repeat(false,length(p2js_auto))
--?T_keywords -- {"string","nullable_string",...
--?T_toktypes -- {8,9,11,3,1,1,1,3,1,12,1,3,3,1,1,... (TYPI..BADT)
--?T_reserved -- {24,88'X',136,140,160,176,192,228,244,... (ttidx)
--  T_keywords = vslice(defs,1)
--  T_toktypes = vslice(defs,2)
--  T_reserved = vslice(defs,3)
--sna = {}
--sad = {}
--zeroes = {}
end procedure

integer arg_rtn = 0,
        arg_idx,
        def_idx

global procedure add_scope(integer rtnttidx=0)
--?"add_scope"
    scopes &= new_dict()    
    arg_rtn = rtnttidx  -- (0 for file scope)
    arg_idx = 0
    def_idx = 0
end procedure

global procedure clear_arg_rtn()
    arg_rtn = 0
end procedure

global procedure drop_scope()
    if length(scopes)<=GLOBALS then ?9/0 end if
    integer d = scopes[$]
    destroy_dict(d)
    scopes = scopes[1..$-1]
end procedure

global procedure final_scope_check()
    if length(scopes)!=GLOBALS then ?9/0 end if
end procedure

function add_id(integer ttidx, vartype, scope)
--if ttidx=27056 then
--  if find(scope,scopes)=2 then ?9/0 end if
--  ?{"add_id",ttidx,vartype,scope,find(scope,scopes)}
--end if
    if getd(ttidx,scopes[BUILTINS])!=NULL then
--/*
{"?find",                       TYPF,   T_find                          := 3284},
current_file = `C:\Program Files (x86)\Phix\builtins\find.e`
auto_files[28..29] = {`find.e`,`find.e`}
auto_names[28..29] = {`find`,`rfind`}
T_keywords[140] = `find`
T_reserved[140] = 3284
tokstack[1..2] = {{`C:\Program Files (x86)\Phix\builtins`},{`C:\Program Files (x86)\Phix`}}
--*/
        string {path,file} = get_file_path_and_name(current_file)
        if path=builtindir then
--/*
            integer k = find(ttidx,T_reserved)
            if k then
                integer l = find(T_keywords[k],auto_names)
                if l then
                    if file=auto_files[l] then return 1 end if
                end if
            end if
--*/
            return 1
        elsif path=pwadir then
            if find(file,{"p2js.js","pGUI.js"}) then
                return 1 -- assume legal
            end if
        end if
        return -1 -- illegal
    end if
    if getd(ttidx,scope)!=NULL then
--DEV we need to save source file and line numbers... [for a proper error message]
        if not find(vartype,{TYPF,TYPR}) then
            ?{"Already defined (p2js_scope.e line 186)",ttidx,get_ttname(ttidx)}
--          ?9/0
        end if
        return 0 -- already defined
    end if
    setd(ttidx,vartype,scope)
    return 1 -- ok
end function

global function add_global(integer ttidx, vartype)
    -- note: global here means file-level, not "global"...
    return add_id(ttidx,vartype,scopes[GLOBALS])
end function

global function add_local(integer ttidx, vartype)
--?{"add_local",ttidx,vartype}
    -- catch/warn this before the call, eg see p2js_parse.e/vardef()
--  if not find(vartype,{TYPI,TYP2,TYPN,TYPQ,TYPS,TYP9,TYPM,TYPP,TYPO}) then ?9/0 end if
    if not find(vartype,{TYPI,TYP2,TYPN,TYPQ,TYPS,TYP9,TY11,TYPP,TYPO}) then ?9/0 end if
    if arg_rtn then
        arg_idx += 1
        if ttidx=0 then return 1 end if
        setd({arg_rtn,ttidx},{arg_idx,vartype},named_args)
--      sna = append(sna,{{arg_rtn,ttidx},{arg_idx,vartype}})
    end if
    return add_id(ttidx,vartype,scopes[$])
end function

global function get_named_param_idx(integer fttidx,ttidx)
    object it = getd({fttidx,ttidx},named_args)
    if it=0 then -- ?9/0
--/*
C:\Program Files (x86)\Phix\pwa\src\p2js_scope.e:199 in function get_named_param_idx()
attempt to divide by 0
    fttidx = 3220
    ttidx = 53492
    it = 0
    rtn_name = `destroy_dict`
    var_name = `justclear`
    idx = <novalue>
    t = <novalue>
k = 6
adx = 4
Global & Local Variables
 C:\Program Files (x86)\Phix\pwa\src\p2js_scope.e:
    auto_names[1..9] = {`assert`,`decode_base64`,`encode_base64`,`binary_search`,`deld`,`destroy_dict`,`dict_name`,`dict_size`,`getd`}
                                                                                         ^^^^^^^^^^^^
    auto_sigs[1..19] = {`PISP`,`FP`,`FPI`,`FOP`,`POI`,`PII`,`FI`,`FI`,`FOI`,`FI`,`FII`,`FOI`,`FOII`,`FOOI`,`FI`,`FS`,`FOI`,`FII`,`FII`}
                                                        ^^
    auto_files[1..11] = {`assert.e`,`base64.e`,`base64.e`,`bsearch.e`,`dict.e`,`dict.e`,`dict.e`,`dict.e`,`dict.e`,`dict.e`,`dict.e`}
                                                                                ^^^^^^
    autoincludes[1..10] = {`assert.e`,`base64.e`,`bsearch.e`,`dict.e`,`factorial.e`,`find.e`,`findrepl.e`,`gcd.e`,`log10.e`,`match.e`}
                                                             ^^^^^^^
    dependencies[1..9] = {{},{`pflatten.e`,`substitute.e`},{},{`pmaths.e`},{},{},{`find.e`},{`pmaths.e`},{}}
    arg_names[4][1..2] = {{`deld`,{{`tid`,`1`},{`key`,`?`}}},{`destroy_dict`,{{`tid`,`?`},{`justclear`,`false`}}}}
                                                               ^^^^^^^^^^^^                 ^^^^^^^^^^^^^^^^^^
--oh crud...
    autoincludes[20..28] = {`permute.e`,`pextract.e`,`pfactors.e`,`pfindall.e`,`pfindany.e`,`pflatten.e`,`pGUI.e`,`porall.e`,`ppp.e`}
                                                                                                          ^^^^^^
    arg_names[26][1] = {`IupTableSetData`,{{`table`,`?`},{`data`,`?`},{`bReset`,`true`}}}
    arg_names[26][2] = {`IupErm2`,{{`path_elements`,`?`},{`trailsep`,`0`}}}
--*/
        string rtn_name = get_ttname(fttidx),
               var_name = get_ttname(ttidx)
--      integer k = find(rtn_name,auto_names)
        integer k = find(rtn_name,rtn_names)
        if k then
--          string sig = auto_sigs[k],
--                 file = auto_files[k]
--          integer adx = find(file,autoincludes)
--          sequence args = arg_names[adx]
            sequence args = rtn_args[k]
--?9/0
--          integer rdx = find(rtn_name,vslice(args,1))
--          args = args[rdx][2]
            integer pdx = find(var_name,vslice(args,1))
--                  sp = sig[pdx+1],
--                  pt = {TYPI}[find(sp,"I")]
--we might want to set args[pdx][2] as a default??? or all of them???
--          return {pdx,pt}
--          return {pdx,TYPO}
            return pdx
--          ?9/0
        end if
        return 0
    end if
    integer {idx,t} = it
    return idx
end function

function arg_rec(sequence node)
    string res = "?"
    if length(node) then    
        object toktype = node[TOKTYPE]
        if find(toktype,{DIGIT,LETTER,'\"','\'',BLK_CMT}) then
            res = tok_string(node)
        elsif toktype='-' then
            res = "-" & arg_rec(node[2][1])
        elsif toktype=`PROC` then
            res = tok_string(node[2][1]) & "("
            for i=2 to length(node[2]) do
                if i>2 then res &= "," end if
                res &= arg_rec(node[2][i])
            end for
            res &= ")"
        elsif toktype='{' then
            res = "{"
            for i=1 to length(node[2]) do
                if i>1 then res &= "," end if
                res &= arg_rec(node[2][i])
            end for
            res &= "}"
        elsif toktype='+'
          and length(node[2])=1
          and find(node[2][1][TOKTYPE],{DIGIT,LETTER}) then
            res = "+" & tok_string(node[2][1])
        else
            ?9/0    -- placeholder for more code...
        end if
    end if
    return res
end function

global procedure set_arg_default(sequence vardef)
--?vardef
    if vardef[1]!=`vardef` then ?9/0 end if
--  sequence v23 = vardef[2][3]
--  string def = "0"
--  string def = "?"
--  vardef = {`vardef`,{{4,139,144,7,15,464},{4,146,151,7,15,17764},{}}}
    for k = 3 to length(vardef[2]) by 2 do
        string def = arg_rec(vardef[2][k])
--/*
    if v23!={} then
        object toktype = v23[TOKTYPE]
--  v23 = {`PROC`,{{4,3055,3060,78'N',19,6628},{4,3062,3069,78'N',12,17560}}}
        if toktype=`PROC` 
        and v23[2][1][TOKTYPE]=LETTER
        and v23[2][1][TOKTTIDX]=T_length
        and v23[2][2][TOKTYPE]=LETTER then
            def = "length(" & tok_string(v23[2][2]) & ")"
        elsif find(toktype,{DIGIT,LETTER,'"'}) then
            def = tok_string(v23)
        elsif toktype='{' and length(v23[2])=0 then
            def = "{}"
--  v23 = {123'{',{{3,1729,1729,63'?',58':'},{45'-',{{3,1732,1732,63'?',61'='}}}}}
        elsif toktype='{' and length(v23[2])=2
--       then
--          sequence {v2321, v2322} = v23[2]
--  v2321 = {3,1729,1729,63'?',58':'}
--  v2322 = {45'-',{{3,1732,1732,63'?',61'='}}}
          and v23[2][1][TOKTYPE]=DIGIT
          and v23[2][2][TOKTYPE]='-'
          and v23[2][2][2][1][TOKTYPE]=DIGIT then
            def = "{" & tok_string(v23[2][1]) & ",-" & tok_string(v23[2][2][2][1]) & "}"
        elsif toktype='-' and length(v23)=2 and v23[2][1][1]=DIGIT then
--  v23 = {45'-',{{3,471,471,18,71'G'}}}
            def = "-" & tok_string(v23[2][1])
        else
            ?9/0    -- placeholder for more code...
        end if
    end if
--*/
        def_idx += 1
--?{"sad",{arg_rtn,def_idx},def}
        setd({arg_rtn,def_idx},def,arg_defs)
    end for
--/*
`C:\Program Files (x86)\Phix\pwa\src\test.exw`
{"9/0: def_idx!=arg_idx",1,0}
{"vardef",{{4,578,584,21,3,388},{6,586,598,21,43'+',21},"",{}}}
function multitext_valuechanged_cb(Ihandle /*multitext*/)
--*/
    if def_idx!=arg_idx then ?{"9/0: def_idx!=arg_idx",def_idx,arg_idx} ?vardef end if
--  sad = append(sad,{{arg_rtn,arg_idx},def})
end procedure

global function get_arg_default(integer fttidx,idx)
    string def = getdd({fttidx,idx},"",arg_defs)
    if def="" then
        string rtn_name = get_ttname(fttidx)
        integer k = find(rtn_name,rtn_names)
        sequence args = rtn_args[k]
        def = args[idx][2]
    end if
    return def
end function

function get_type(integer ttidx, scope, downto=1)
    integer res
    for s=scope to downto by -1 do
        res = getd(ttidx,scopes[s])
        if res!=NULL then
            if ttidx<=T_high then
                integer rdx = getd({'[',ttidx},scopes[BUILTINS])
                if rdx then
                    referenced[rdx] = true
                end if
            end if
            exit
        end if
    end for
--temp:
--if res=0 and downto=1 and not find(ttidx,zeroes) then
--zeroes &= ttidx
--?{"get_type",ttidx,scope,"==>",res}
--end if
    return res -- (NULL/0 == not found)
--  return TYPO
end function

global function get_local_type(integer ttidx)
    -- (mainly for checking things do /not/ exist)
    integer s = length(scopes)
    return get_type(ttidx,s,s)
end function

global function get_global_type(integer ttidx)
    -- (all vartypes must be global or builtin)
    return get_type(ttidx,GLOBALS)
end function

global function get_id_type(integer ttidx)
    return get_type(ttidx,length(scopes))
end function

-- note there is a Euphoria match_any() which is a 100% ridiculous copy of find_any(), right down to the docs..
function match_any(sequence needles, haystack)
    for i=1 to length(needles) do
        if match(needles[i],haystack) then
            return 1
        end if
    end for
    return 0
end function

constant unsupported = {"pcfunc.e","pTask.e","pThreadN.e","structs.e","syswait.ew",
"get_interpreter.e","ptls.ew","pscreen.e","pAlloc.e","peekns.e","pmach.e","file.e",
"panykey.e","database.e","pbreak.e","ppoke2.e","image.e","mouse.e","pgettext.e",
"hll_stubs.e","pfile.e","pchdir.e","pincpathN.e","isatty.e","hasdel.e","msgbox.e",
"pFloatN.e","dll.e","hash.e","pokestr.e","pcurrdir.e","pgetpath.e","peekstr.e",
"pcmdlnN.e","get_routine_info.e","pdir.e","pdelete.e","ldap.e","pScrollN.e","penv.e",
"graphics.e","get.e","machine.e","progress.e","procedure initialAutoEntry(",
-- supported differently (see/directly in p2js.js)
"printf","pprntfN.e","pdate.e","pApply.e","pFilter.e","pCrashN.e","prnd.e","prtnidN.e",
"repeat.e","ubits.e",`serialize.e`}
--??"procedure",

function initialAutoEntries(string line)
    integer k = match("--",line)
    if k then line = line[1..k-1] end if
    return match("initialAutoEntry",line)
       and not match_any(unsupported,line)
--     and not match("pcfunc.e",line)
--     and not match("pTask.e",line)
--     and not match("pThreadN.e",line)
--     and not match("structs.e",line)
--     and not match("syswait.ew",line)
--     and not match("get_interpreter.e",line)
--     and not match("ptls.ew",line)
--     and not match("pscreen.e",line)
--     and not match("pAlloc.e",line)
--     and not match("pApply.e",line)
--     and not match("peekns.e",line)
--     and not match("pmach.e",line)
--     and not match("pCrashN.e",line)
--     and not match("file.e",line)
--     and not match("panykey.e",line)
--     and not match("database.e",line)
--     and not match("pbreak.e",line)
--     and not match("progress.e",line)
--     and not match("ppoke2.e",line)
--     and not match("image.e",line)
--     and not match("mouse.e",line)
--     and not match("prnd.e",line)
--     and not match("pgettext.e",line)
--     and not match("hll_stubs.e",line)
--     and not match("pfile.e",line)
--     and not match("procedure",line)
--     and not match("printf",line)
--     and not match("pchdir.e",line)
--     and not match("pincpathN.e",line)
--     and not match("prtnidN.e",line)
--     and not match("isatty.e",line)
--     and not match("hasdel.e",line)
--     and not match("msgbox.e",line)
--     and not match("pFloatN.e",line)
--     and not match("pprntfN.e",line)
--     and not match("dll.e",line)
--     and not match("hash.e",line)
--     and not match("pokestr.e",line)
--     and not match("pcurrdir.e",line)
--     and not match("pgetpath.e",line)
--     and not match("peekstr.e",line)
--     and not match("pcmdlnN.e",line)
-- we might as well eliminate these, but not a lot of point...
--     and not match("pUnassigned.e",line)
--     and not match("optable.e",line)
--     and not match("pType.e",line)
--     and not match("pTrace.e",line)
--     and not match("pSubssN.e",line)
--     and not match("pSubseN.e",line)
--     and not match("pStack.e",line)
--     and not match("pRepeN.e",line)
--     and not match("pRepsN.e",line)
--     and not match("pRepeatN.e",line)
--     and not match("pProfile.e",line)
--     and not match("pMemChk.e",line)
--     and not match("pMem.e",line)
--     and not match("pMath.e",line)
--     and not match("pJnotx.e",line)
--     and not match("pJcc.e",line)
--     and not match("pHeap.e",line)
--     and not match("pfileioN.e",line)
--     and not match("pfileio.e",line)
--     and not match("pFEH.e",line)
--     and not match("pDiagN.e",line)
--     and not match("pDeleteN.e",line)
--     and not match("pcallfunc.e",line)
--     and not match("pApnd.e",line)
--     and not match("WINCONST.EW",line)
--     and not match("unicode_console.e",line)
--     and not match("tok.e",line)
--     and not match("sockets.e",line)
--     and not match("sha256.e",line)
--     and not match("safe.e",line)
--     and not match("reflections.e",line)
--     and not match("prtnid.e",line)
--     and not match("pprntf.e",line)
--     and not match("pmt.e",line)
--     and not match("pipeio.e",line)
--     and not match("phash.e",line)
--     and not match("pcopyfile.e",line)
--     and not match("pComN.e",line)
--     and not match("pcase8.e",line)
--     and not match("nopoll.e",line)
--     and not match("mpfr.e",line)
--     and not match("LiteZip.e",line)
--     and not match("librsvg.e",line)
--     and not match("libcurl.e",line)
--     and not match("bigatom.e",line)

--     and not match("get_routine_info.e",line)
--     and not match("pdir.e",line)
--     and not match("pdelete.e",line)
--     and not match("ldap.e",line)
--     and not match("pScrollN.e",line)
--     and not match("pdate.e",line)
--     and not match("penv.e",line)
--     and not match("graphics.e",line)
--     and not match("get.e",line)
--     and not match("machine.e",line)
--     and not match("pFilter.e",line)
--     and not match("ubits.e",line)
--temp:
--     and not match("ppp.e",line)
--     and not match("unit_test.e",line)
--     and not match("psqop.e",line)
--     and not match("pmaths.e",line)
--?    and not match("timedate.e",line)
end function

function clean(string line)
    integer k = find(')',line)  -- clip any trailing comments, etc.
    line = trim(line[1..k])
                                -- whitespace:     vv          vv
    sequence r = scanf(line,`initialAutoEntry("%s",%sS_%s,"%s",%s"%s",%s)`)
                                --          name^   F/P^ sig^  file^  ^E_xxx[,minp]
                    -- (since optional parameters are the norm in JavaScript
                    --  we are not going to bother with policing them here. )
    if length(r)!=1 then ?9/0 end if
    return extract(r[1],{1,3,4,6}) -- {name,(Func|Proc),sig,file}
--  return extract(r[1],{1,3,6}) -- {name,(Func|Proc),file}
--  sequence res = extract(r[1],{1,1,3,6})  -- {name,name,(Func|Proc),file}
--  res[1] = get_ttidx(r[1])                --  ^ttidx
--? res[3] = find(res[3],{"Func","Proc"})
--  return res
end function

include builtins\timedate.e

constant auto = """
--
-- p2js_auto.e (nb automatically over-written, all comments get trashed)
--
global constant last_built = "%s"

global constant p2js_auto = """,
--234567890123456789012345678 -- (hence indent of 28)
         dfmt = "Mmmm d yyyy h:mm:sspm"

--DEV not sure when bet to put this...
global sequence rebuild_required = {}

procedure check_builtins()
    string psymname = get_proper_path(join_path({"..","psym.e"}))
    if not file_exists(psymname) then crash("Cannot open "&psymname) end if
    sequence last_mod = get_file_date(psymname)
    atom delta = timedate_diff(last_mod,parse_date_string(last_built,{dfmt}))
    if delta<0 then
--?delta
--?last_mod
--?last_built
        string lm = format_timedate(last_mod,dfmt)

-- psym.e modified (details) - reload? (Y/N/I)
--  Y: reload and save new date/time
--  N: skip, re-prompt next time
--  I: ignore, save new date/time
-- Note that zip/upload/download/unzip has a nasty habit of messing with daylight saving times,
--  so you may get this message on a fresh install because you're in a different dstz to me.
--  The (details) part is an attempt to show something I could use to bypass some such cases.
--puts(1,"psym.e modified - recreate p2js_auto.e?")
--constant psym_last_mod = {2021,0,0}
-- aside: above includes file size, so windows "\r\n" <==> linux "\n" (unzip) may also trigger it.
--A fresh install may also want to rebuild p2js_auto.e (since date/size may not exactly agree)

        sequence p = include_paths(),
                 f = include_files()
        integer fdx = include_file()
        if f[fdx][2]!=`p2js_scope.e` then ?9/0 end if -- (sanity check)
        string path = p[f[fdx][1]],
               filename = join_path({path,"p2js_auto.e"})
        printf(1,"psym.e modified (psym.e:%s, p2js_auto.e:%s)\n",{lm,last_built})
        printf(1,"\nOverwrite %s and restart?",{filename})
        if not find(upper(wait_key()),{'Q','N',#1B}) then
            puts(1,"\n")
            string content = sprintf(auto,{format_timedate(date(),dfmt)})
            sequence s = apply(filter(get_text("../psym.e",GT_LF_STRIPPED),initialAutoEntries),clean)
--          content &= ppf(sort_columns(s,{4,1}),{pp_Nest,1,pp_Indent,28,pp_Maxlen,120})
            s = sort_columns(s,{4,1})
            content &= ppf(s,{pp_Nest,1,pp_Indent,28,pp_Maxlen,120})
            integer fn = open(filename,"w")
            if fn=-1 then crash("cannot open "&filename) end if
            puts(fn,content)
            close(fn)
            requires(-machine_bits(),false) -- restart
        end if
        abort(0)
    end if
--  {names,fp,sigs,files} = columize(p2js_auto)
--{`find`, `Func`, `FOPI`, `VM\\pFind.e`},

--12/5/21:
    for i=1 to length(autoincludes) do
        string ai = strip_builtin(autoincludes[i])
        if ai!="pGUI.e"
        and ai!="mpfr.e"
        and ai!="sha256.e" then
            string aj = substitute(ai,".e",".js"),
                   pb = get_proper_path(join_path({"..","builtins",ai})),
                   jb = get_proper_path(join_path({"builtins",aj}))
            if not file_exists(pb) then crash("Cannot open "&pb) end if
            if not file_exists(jb) then
                printf(1,"%s is missing/will be created\n",{aj})
--erm... (a log file would be good...) [DEV]
--          IupSetStrAttribute(lbl_statusbar,"TITLE","overwriting %s\n",{pwabpath})
                rebuild_required = append(rebuild_required,ai)
            else
                sequence pd = get_file_date(pb),
                         jd = get_file_date(jb)
                delta = timedate_diff(jd,pd)
                if delta>0 then
                    if delta>60 then
                        integer term = iff(delta>timedelta(days:=1)?DT_DAY:DT_MINUTE)
                        delta = timedate_diff(pd,jd,term) 
                    end if
--                  string lp = format_timedate(pd,dfmt),
--                         lj = format_timedate(jd,dfmt)
--                  ?{pb,lp,jb,lj,elapsed(delta)}
--                  ?{ai,elapsed(delta)}
                    printf(1,"%s is %s newer than %s\n",{ai,elapsed(-delta),aj})
                    rebuild_required = append(rebuild_required,ai)
                end if
            end if
--else ?ai
        end if
    end for 
end procedure

if platform()!=JS then check_builtins() end if

--DEV currently manually maintained... [FIXED]
--p2js_depend.e similar to p2js_auto.e, but quietly rewritten (since we update our copy anyway)
--/*
constant ad = {{"assert.e",{}},
               {"base64.e",{`??`}},
               {"bsearch.e",{}},
               {"dict.e",{"pmarths.e"}},
               {"factorial.e",{}},
               {"find.e",{}},
               {"findrepl.e",{`??`}},
               {`gcd.e`,{`pmaths.e`}},
               {`log10.e`,{}},
               {"match.e",{"find.e","pcase.e","pfindall.e"}},
               {"matchrepl.e",{`??`}},
               {`misc.e`,{}},
               {`ordinal.e`,{`??`}},
               {`pmaths.e`,{}},
               {"pcase.e",{"find.e"}},
               {"pcolumn.e",{}},
               {"pdates.e",{}},
               {"pdecodeflags.e",{`??`}},
               {"pelapsed.e",{"match.e","pmaths.e"}},
               {"permute.e",{}},
               {"pextract.e",{}},
               {"pfactors.e",{"bsearch.e","pmaths.e","primes.e"}},
               {"pfindall.e",{"find.e"}},
               {"pfindany.e",{"find.e"}},
               {"pflatten.e",{"find.e"}},
               {"porall.e",{"??"}},
               {"ppp.e",{"find.e"}},
               {"pqueue.e",{"??"}},
               {"premoveall.e",{"??"}},
               {"primes.e",{"bsearch.e","pmaths.e"}},
               {"pseqc.e",{"pmaths.e"}},
               {"pseries.e",{"??"}},
               {"psmall.e",{"??"}},
               {"psplit.e",{"find.e","match.e","pfindany.e"}},
               {"psqop.e",{"pmaths.e","log10.e","misc.e"}},
               {"psum.e",{}},
               {"ptagset.e",{"pmaths.e"}},
               {"ptrim.e",{"find.e","psqop.e","psum.e"}},
               {"punique.e",{"dict.e","sort.e"}},
               {"pvlookup.e",{"??"}},
               {"scanf.e",{"find.e","match.e","pcase.e"}},
               {"shift_bits.e",{`??`}},
               {"shuffle.e",{`??`}},
               {"sort.e",{}},
               {"substitute.e",{"match.e"}},
               {"to_int.e",{`??`}},
               {"to_str.e",{`??`}},
               {"unit_test.e",{`??`}},
               {"utfconv.e",{}},
               {"vslice.e",{`??`}},
               {"wildcard.e",{"find.e","match.e","pcase.e"}}}
--*/
--sequence {autoincludes,dependencies} = columnize(ad)
--,globals  = repeat({},length(autoincludes))
--globals[find("dict.e",autoincludes)] = {"KEY","DATA","LEFT","HEIGHT","RIGHT","trees","treenames","roots","sizes","defaults","freelists","free_trees",
-- "initd","dinit","dictionary","check","newNode","height","setHeight","rotate","getBalance","insertNode",
-- "getNode","getKey","minValueNode","deleteNode","traverse","traverse_key","traverser","peekpop"}
--,arg_names = repeat({},length(autoincludes))

constant depend = """
--
-- p2js_depend.e (nb automatically over-written in/by p2js_scope.e, all comments get trashed)
--
-- Each entry is {"builtin.e",{dependencies, if any},{locals, if any},{{routine,{args}}}} 
-- Rebuild builtins (Ctrl R) verifies no locals clash, and renames them with a $-prefix.
-- Note that "pGUI.e"/"mpfr.e"/"timedate.e" are manually maintained, the rest auto-built.
--
global sequence p2js_depend = 
"""
--23456789012345678901234567890 -- (hence indent of 30)

function get_named_args()
--integer named_args,   -- key is {arg_rtn,ttidx}, 
--                      -- data is {arg_idx,vartype}
--      arg_defs        -- key is {arg_rtn,arg_idx},
--                      -- data is "{}/ident/string/number" (always string)
--                      -- nb no support for eg length(),platform() etc yet.    [ length now done ]
--"dump_named_args"
--{{{3584,17748},{1,15}},{{3584,17752},{2,3}},{{6820,17764},{1,15}},{{6820,17768},{2,3}}}
--"defaults"
--{{{3584,1},"?"},{{3584,2},"0"},{{6820,1},"?"},{{6820,2},"0"}}
--T_gcd = 3584, T_lcm = 6820
--"dump_named_args"
--{{`gcd`, {{`u`, `?`}, {{`v`, `0`}}}}, {`lcm`, {{`m`, `?`}, {{`n`, `0`}}}}}
    sequence s = getd_all_keys(named_args),
             res = {}
    integer last_rtn = 0, bAdd
    for i=1 to length(s) do
        integer {arg_rtn,ttidx} = s[i]
        if arg_rtn!=last_rtn then
            last_rtn = arg_rtn
            string rtn_name = get_ttname(arg_rtn)
            bAdd = find(rtn_name,auto_names)
            if bAdd then
                res = append(res,{rtn_name,{}})
            end if
        end if
        if bAdd then
            integer {arg_idx,arg_typ} = getd(s[i],named_args)
--      --DEV didn't expect anything to be missing... probably worth investigating at some stage [FIXED, I think]
--          object ad = getd({arg_rtn,arg_idx},arg_defs)
            string arg_name = get_ttname(ttidx),
                   arg_dflt = getd({arg_rtn,arg_idx},arg_defs)
--                 arg_dflt = iff(string(ad)?ad:`?????`)
--                 arg_dflt = iff(string(ad)?ad:`?`)
--          sequence arg = {{arg_name,arg_dflt}}
--      if arg_rtn!=last_rtn then
--          last_rtn = arg_rtn
--          res = append(res,{rtn_name,arg})
--      else
--          res[$][2] &= {arg}  -- (added to docs)
            sequence r2 = res[$][2]
            res[$][2] = 0   -- (or use deep_copy)
--          r2 = append(r2,{arg_name,arg_dflt})
            while length(r2)<arg_idx do
                r2 = append(r2,0)
            end while
            r2[arg_idx] = {arg_name,arg_dflt}
            res[$][2] = r2
--untried:
--X         res[$][2] = deep_copy(res[$][2]) & {{arg_name,arg_dflt}}
        end if
    end for
    return res
end function

procedure clash(sequence gi, gj)
    for i=1 to length(gj) do
        if find(gj[i],gi) then ?{"clash",gj[i]} end if
    end for
end procedure

function fudge(sequence s)
    --
    -- add a bit of nesting to p2js_depend, for purely aesthetic reasons only.
    -- ie/eg `base64.e` wants decode_base64 and encode_base64 on separate lines,
    --   but pflatten.e, substitute.e, and base64_init, aleph, etc on one line,
    --   that is, as that entry is output, via ppf(), to p2js_depends.e.
    --
    integer l = length(s)
    sequence res = repeat(0,l)
    for i=1 to l do
        res[i] = {s[i]}
    end for
    return res
end function

global function get_autoincludes()
    sequence res = {}, gk = {}, dk, nk
    string prev = ""
    for i=1 to length(referenced) do
        if referenced[i] and p2js_auto[i][4]!=prev then
            prev = p2js_auto[i][4]
            res = append(res,prev)
        end if
    end for
    string cf = get_file_name(current_file)
    integer k = find(cf,autoincludes)
    if k and platform()!=JS then
        gk = apply(getd_all_keys(scopes[GLOBALS]),get_ttname)
        dk = deep_copy(res)
        nk = get_named_args()
        if length(dk) and dk[1]=cf then dk = dk[2..$] end if
        if dk!=dependencies[k]
        or gk!=globals[k]
        or nk!=arg_names[k] then
            dependencies[k] = dk
            globals[k] = gk
            arg_names[k] = nk
            sequence p = include_paths(),
                     f = include_files()
            integer fdx = include_file()
            if f[fdx][2]!=`p2js_scope.e` then ?9/0 end if -- (sanity check)
            printf(1,"Overwriting p2js_depend.e (for %s)...\n",{autoincludes[k]})
            sequence s = columnize({autoincludes,fudge(dependencies),fudge(globals),arg_names})
            string content = depend & ppf(s,{pp_Nest,3,pp_Indent,0,pp_Maxlen,132}) & "\n\n",
                   filename = join_path({p[f[fdx][1]],"p2js_depend.e"})
--if k=1 then ?9/0 end if
            integer fn = open(filename,"w")
            if fn=-1 then crash("cannot open "&filename) end if
            puts(fn,content)
            close(fn)
        end if
        --
        -- check for any internal clashes
        --
        sequence std_globals = {`catch`,`charArray`,`conCat`,`docBody`,`paranormalise`,`repe`,
                                `repss`,`sidii`,`storeAttr`,`subse`,`subss`,`typeCheckError`}
        for i=1 to length(globals) do
            sequence gi = globals[i]
            if length(gi) then
                clash(std_globals,gi)
                for j=1 to length(globals) do
                    if j!=i then clash(gi,globals[j]) end if
                end for
            end if
        end for
    end if
--?9/0
    res = append(res,"==")  -- (debug aid)
    integer rdx = 1
    while rdx<=length(res) do  -- (process all indirects as well)
        string rr = res[rdx]
        if rr!="==" then
            k = find(rr,autoincludes)
--DEV
--          printf(1,"transpiling %s...\n",{rr})
-- or maybe return {"!!",rr}
            if k=0 then crash("dependencies not defined for "&rr) end if
            sequence d = dependencies[k]
            for j=1 to length(d) do
                string dj = d[j]
                if not find(dj,res) then
                    res = append(res,dj)
                end if
            end for
        end if
        rdx += 1
    end while
    res = {res,gk}
    return res
end function

-- may want/need to be in p2js_scope or p2_basics... [erm, seems ok 26/3/21] [moved here 2/5/21]
sequence tokstack,
         tokseen,
         sources

global procedure tokstack_clean(string filename)
    string {path,file} = get_file_path_and_name(filename)
    tokstack = {{builtindir},
                {rootpath},
                {path,filename,1}}
    tokseen = {file}
    sources = {}
--pp(tokstack)
end procedure

global function tokstack_push(string filename, integer line)
-- gotta handle relative directory includes... activepaths...
--/*
--  C:\Program Files (x86)\Phix\pwa\src\p2js_tok.e:467 in function tokstack_push()
--  crash(ok)
--      filename = `C:\Program Files (x86)\Phix\test\t05inc0b.e`
--      found = `C:\Program Files (x86)\Phix\test\t05inc0b.e`
--      t = 3
--      fntest = `C:\Program Files (x86)\Phix\test\t05inc0b.e`
--      filepath = `C:\Program Files (x86)\Phix\test`
--  Global & Local Variables
--   C:\Program Files (x86)\Phix\pwa\src\p2js_tok.e:
--      tokstack[1..2] = {{`C:\Program Files (x86)\Phix\builtins`},{`C:\Program Files (x86)\Phix`}}
--      tokstack[3] = {`C:\Program Files (x86)\Phix\test`,`C:\Program Files (x86)\Phix\test\t05inc0.exw`}
--      tokseen = {`C:\Program Files (x86)\Phix\test\t05inc0.exw`}
--   C:\Program Files (x86)\Phix\pwa\src\p2js_parse.e:
--      udts = {}
--  --  tdx = 26
--  --  clines
--   C:\Program Files (x86)\Phix\pwa\p2js.exw:
--      src[1..113] = "--\n-- t05inc0.exw\n--\n--without warning -- lots of unused stuff in here\nputs(1,\"inc0a\\n\")\n\nglobal integer z, p\nz=1"
--      src[114..226] = " p=2\ninclude t05inc0b.e  -- another z(=3), and q(=4)\nif z!=1 then crash(\"z!=1\\n\") end if\nif p!=2 then crash(\"p!=2"
--      src[227..292] = "\\n\") end if\nif q!=4 then crash(\"q!=4\\n\") end if\nputs(1,\"inc0\\n\")\n\n"
--  --  fdx = 0
--  --  fokens = {}
--  --  fsrc = ``
--  --  fstacked = {}
--*/
    if src_offset!=0 then ?9/0 end if   -- sanity check
--  filename = get_proper_path(filename)
    if find(filename[1],{'`','"'}) then
        filename = filename[2..$-1]
    end if
--?{"tokstack_push",filename}
    if not file_exists(filename) then
--?tokseen
--?{"tokstack",tokstack}
--?filename
        string found = ""
        for t=length(tokstack) to 1 by -1 do
            string fntest = join_path({tokstack[t][1],filename})
            if file_exists(fntest) then
                found = fntest
                exit
            end if
--?fntest
        end for
        if length(found)=0 then return "NOT FOUND" end if
        filename = found
    end if
    filename = get_proper_path(filename)
    string {filepath,nameonly} = get_file_path_and_name(filename)
    if find(filename,tokseen)
    or (filepath=builtindir and 
        (find(nameonly,autoincludes) or
         find(nameonly,{`pdate.e`,`pcurrdir.e`,`peekstr.e`,`pgetpath.e`,`pfile.e`,
                        `get_routine_info.e`,`pdir.e`,`penv.e`,`get_interpreter.e`,
                        `syswait.ew`})))
    or filepath=builtinVM then
        return "ALREADY DONE"
    end if
--  tokseen = append(tokseen,{filename})
    tokseen = append(tokseen,filename)
    if sources={} then
        sources = append(sources,src)
    end if
    src = get_text(filename)
    sources = append(sources,src)
    integer srcdx = length(sources)
    tokstack = append(tokstack,{filepath,filename,srcdx,tdx,tokens,clines,textlines,current_file})
--?src
    lt = length(src)
    textlines = split(substitute(src,"\r\n","\n"),'\n',false) -- DEV common up? (p2js_basics.e:121)
    current_file = filename
    tokenise()
    tdx = 1
--crash("ok")
--load_text(string txt, ext)--integer edx)
--      return false
    return {T_include,{filename,srcdx,line}}
end function

global function tokstack_pop()
    if length(tokstack)>3 then
        integer srcdx
        {?,?,srcdx,tdx,tokens,clines,textlines,current_file} = tokstack[$]
--      src = sources[srcdx]
        tokstack = tokstack[1..$-1]
        srcdx = tokstack[$][3]
        src = sources[srcdx]
        return {T_include,{"",srcdx}}
    end if
    return "NO MORE"
end function

global procedure restore_source(integer srcdx)
    src = sources[srcdx]
    current_file = tokseen[srcdx]
end procedure
--/*
{"autoincludes:",{"dict.e","pmaths.e","=="}}
{"KEY","DATA","LEFT","HEIGHT","RIGHT","trees","treenames","roots","sizes","defaults","freelists","free_trees",
 "initd","dinit","dictionary","check","newNode","height","setHeight","rotate","getBalance","insertNode",
 "getNode","getKey","minValueNode","deleteNode","traverse","traverse_key","traverser","peekpop"}
--*/
--global procedure dump_globals()
----    return add_id(ttidx,vartype,scopes[GLOBALS])
--  sequence s = apply(getd_all_keys(scopes[GLOBALS]),get_ttname)
--  ?s
--end procedure

--global procedure dump_named_args()
--  ?"dump_named_args"
--  pp(get_named_args())
--end procedure
--?length(s) -- 413
--pp(s)

--pp(sort_columns(s,{4,1}),{pp_Nest,1})
--papply(true,printf,{1,{`{"%s",%d,"%s"
--papply(true,printf,{1,{"%v,\n"},sort_columns(s,{4,1}))

--/*
{{out}}
224
{{`find`, `Func`, `FOPI`, `VM\\pFind.e`},
 {`rfind`, `Func`, `FOPI`, `VM\\pFind.e`},
 {`match`, `Func`, `FOPII`, `VM\\pMatch.e`},
 {`rmatch`, `Func`, `FOPII`, `VM\\pMatch.e`},
 {`assert`, `Proc`, `PISP`, `assert.e`},
 {`decode_base64`, `Func`, `FP`, `base64.e`},
 {`encode_base64`, `Func`, `FPI`, `base64.e`},
 {`binary_search`, `Func`, `FOP`, `bsearch.e`},
 {`deld`, `Proc`, `POI`, `dict.e`},
 {`destroy_dict`, `Proc`, `PII`, `dict.e`},
 {`dict_size`, `Func`, `FI`, `dict.e`},
 {`getd`, `Func`, `FOI`, `dict.e`},
 {`getd_all_keys`, `Func`, `FI`, `dict.e`},
 {`getd_by_index`, `Func`, `FII`, `dict.e`},
 {`getd_index`, `Func`, `FOI`, `dict.e`},
 {`getd_partial_key`, `Func`, `FOII`, `dict.e`},
 {`getdd`, `Func`, `FOOI`, `dict.e`},
 {`is_dict`, `Func`, `FI`, `dict.e`},
 {`new_dict`, `Func`, `FOI`, `dict.e`},
 {`peep_dict`, `Func`, `FII`, `dict.e`},
 {`pop_dict`, `Func`, `FII`, `dict.e`},
 {`setd`, `Proc`, `POOI`, `dict.e`},
 {`setd_default`, `Proc`, `POI`, `dict.e`},
 {`traverse_dict`, `Proc`, `PIOII`, `dict.e`},
 {`traverse_dict_partial_key`, `Proc`, `PIOOII`, `dict.e`},
 {`choose`, `Func`, `FII`, `factorial.e`},
 {`factorial`, `Func`, `FN`, `factorial.e`},
 {`k_perm`, `Func`, `FII`, `factorial.e`},
 {`find_replace`, `Func`, `FOPOI`, `findrepl.e`},
 {`gcd`, `Func`, `FON`, `gcd.e`},
 {`lcm`, `Func`, `FON`, `gcd.e`},
-- {`get`, `Func`, `FI`, `get.e`},
-- {`get_bytes`, `Func`, `FII`, `get.e`},
-- {`prompt_number`, `Func`, `FSP`, `get.e`},
-- {`prompt_string`, `Func`, `FS`, `get.e`},
-- {`value`, `Func`, `FP`, `get.e`},
 {`log10`, `Func`, `FN`, `log10.e`},
 {`log2`, `Func`, `FN`, `log10.e`},
-- {`bits_to_int`, `Func`, `FP`, `machine.e`},
-- {`bytes_to_int`, `Func`, `FPI`, `machine.e`},
-- {`int_to_bits`, `Func`, `FNI`, `machine.e`},
-- {`int_to_bytes`, `Func`, `FNI`, `machine.e`},
 {`match_replace`, `Func`, `FOPOI`, `matchrepl.e`},
 {`arccos`, `Func`, `FN`, `misc.e`},
 {`arcsin`, `Func`, `FN`, `misc.e`},
 {`reverse`, `Func`, `FPP`, `misc.e`},
 {`ord`, `Func`, `FI`, `ordinal.e`},
 {`ordinal`, `Func`, `FII`, `ordinal.e`},
-- {`filter`, `Func`, `FPOOS`, `pFilter.e`}, [yep, done]
 {`islower`, `Func`, `FI`, `pcase.e`},
 {`isupper`, `Func`, `FI`, `pcase.e`},
 {`lower`, `Func`, `FO`, `pcase.e`},
 {`proper`, `Func`, `FSS`, `pcase.e`},
 {`upper`, `Func`, `FO`, `pcase.e`},
 {`columnize`, `Func`, `FPOO`, `pcolumn.e`},
 {`day_of_week`, `Func`, `FIIII`, `pdates.e`},
 {`day_of_year`, `Func`, `FIII`, `pdates.e`},
 {`days_in_month`, `Func`, `FII`, `pdates.e`},
 {`is_leap_year`, `Func`, `FI`, `pdates.e`},
 {`decode_flags`, `Func`, `FPNS`, `pdecodeflags.e`},
 {`elapsed`, `Func`, `FNNS`, `pelapsed.e`},
 {`elapsed_short`, `Func`, `FNNS`, `pelapsed.e`},
 {`permute`, `Func`, `FIP`, `permute.e`},
 {`extract`, `Func`, `FPPI`, `pextract.e`},
 {`reinstate`, `Func`, `FPPPI`, `pextract.e`},
 {`factors`, `Func`, `FNI`, `pfactors.e`},
 {`is_prime`, `Func`, `FN`, `pfactors.e`},
 {`prime_factors`, `Func`, `FNII`, `pfactors.e`},
 {`square_free`, `Func`, `FNI`, `pfactors.e`},
 {`find_all`, `Func`, `FOPI`, `pfindall.e`},
 {`find_any`, `Func`, `FPPI`, `pfindany.e`},
 {`flatten`, `Func`, `FPP`, `pflatten.e`},
 {`join`, `Func`, `FPO`, `pflatten.e`},
 {`join_by`, `Func`, `FPIIOO`, `pflatten.e`},
 {`join_path`, `Func`, `FPI`, `pflatten.e`},
 {`abs`, `Func`, `FN`, `pmaths.e`},
 {`atan2`, `Func`, `FNN`, `pmaths.e`},
 {`bankers_rounding`, `Func`, `FNI`, `pmaths.e`},
 {`ceil`, `Func`, `FN`, `pmaths.e`},
 {`exp`, `Func`, `FN`, `pmaths.e`},
 {`max`, `Func`, `FOO`, `pmaths.e`},
 {`maxsq`, `Func`, `FPI`, `pmaths.e`},
 {`min`, `Func`, `FOO`, `pmaths.e`},
 {`minsq`, `Func`, `FPI`, `pmaths.e`},
 {`mod`, `Func`, `FNN`, `pmaths.e`},
 {`round`, `Func`, `FNN`, `pmaths.e`},
 {`sign`, `Func`, `FN`, `pmaths.e`},
 {`trunc`, `Func`, `FN`, `pmaths.e`},
 {`or_all`, `Func`, `FO`, `porall.e`},
 {`or_allu`, `Func`, `FO`, `porall.e`},
 {`pp`, `Proc`, `POP`, `ppp.e`},
 {`ppEx`, `Proc`, `POP`, `ppp.e`},
 {`ppExf`, `Func`, `FOP`, `ppp.e`},
 {`ppOpt`, `Proc`, `PP`, `ppp.e`},
 {`ppf`, `Func`, `FOP`, `ppp.e`},
 {`pq_add`, `Proc`, `PPI`, `pqueue.e`},
 {`pq_destroy`, `Proc`, `PIII`, `pqueue.e`},
 {`pq_empty`, `Func`, `FI`, `pqueue.e`},
 {`pq_new`, `Func`, `FII`, `pqueue.e`},
 {`pq_peek`, `Func`, `FI`, `pqueue.e`},
 {`pq_pop`, `Func`, `FI`, `pqueue.e`},
 {`pq_pop_data`, `Func`, `FI`, `pqueue.e`},
 {`pq_size`, `Func`, `FI`, `pqueue.e`},
 {`remove_all`, `Func`, `FOP`, `premoveall.e`},
 {`get_maxprime`, `Func`, `FN`, `primes.e`},
 {`get_prime`, `Func`, `FI`, `primes.e`},
 {`get_primes`, `Func`, `FI`, `primes.e`},
 {`get_primes_le`, `Func`, `FI`, `primes.e`},
 {`head`, `Func`, `FPN`, `pseqc.e`},
 {`insert`, `Func`, `FPOI`, `pseqc.e`},
 {`pad_head`, `Func`, `FPIO`, `pseqc.e`},
 {`pad_tail`, `Func`, `FPIO`, `pseqc.e`},
 {`remove`, `Func`, `FPNN`, `pseqc.e`},
 {`replace`, `Func`, `FPONN`, `pseqc.e`},
 {`splice`, `Func`, `FPOI`, `pseqc.e`},
 {`tail`, `Func`, `FPN`, `pseqc.e`},
 {`series`, `Func`, `FOOII`, `pseries.e`},
 {`largest`, `Func`, `FPI`, `psmall.e`},
 {`smallest`, `Func`, `FPI`, `psmall.e`},
 {`split`, `Func`, `FPOII`, `psplit.e`},
 {`split_any`, `Func`, `FPOII`, `psplit.e`},
 {`split_by`, `Func`, `FPI`, `psplit.e`},
 {`split_path`, `Func`, `FPI`, `psplit.e`},
 {`sq_abs`, `Func`, `FO`, `psqop.e`},
 {`sq_add`, `Func`, `FOO`, `psqop.e`},
 {`sq_and`, `Func`, `FOO`, `psqop.e`},
 {`sq_and_bits`, `Func`, `FOO`, `psqop.e`},
 {`sq_arccos`, `Func`, `FO`, `psqop.e`},
 {`sq_arcsin`, `Func`, `FO`, `psqop.e`},
 {`sq_arctan`, `Func`, `FO`, `psqop.e`},
 {`sq_atom`, `Func`, `FO`, `psqop.e`},
 {`sq_ceil`, `Func`, `FO`, `psqop.e`},
 {`sq_cmp`, `Func`, `FOO`, `psqop.e`},
 {`sq_cos`, `Func`, `FO`, `psqop.e`},
 {`sq_div`, `Func`, `FOO`, `psqop.e`},
 {`sq_eq`, `Func`, `FOO`, `psqop.e`},
 {`sq_floor`, `Func`, `FO`, `psqop.e`},
 {`sq_floor_div`, `Func`, `FOO`, `psqop.e`},
 {`sq_ge`, `Func`, `FOO`, `psqop.e`},
 {`sq_gt`, `Func`, `FOO`, `psqop.e`},
 {`sq_int`, `Func`, `FO`, `psqop.e`},
 {`sq_le`, `Func`, `FOO`, `psqop.e`},
 {`sq_log`, `Func`, `FO`, `psqop.e`},
 {`sq_log10`, `Func`, `FO`, `psqop.e`},
 {`sq_log2`, `Func`, `FO`, `psqop.e`},
 {`sq_lower`, `Func`, `FO`, `psqop.e`},
 {`sq_lt`, `Func`, `FOO`, `psqop.e`},
 {`sq_max`, `Func`, `FOO`, `psqop.e`},
 {`sq_min`, `Func`, `FOO`, `psqop.e`},
 {`sq_mod`, `Func`, `FOO`, `psqop.e`},
 {`sq_mul`, `Func`, `FOO`, `psqop.e`},
 {`sq_ne`, `Func`, `FOO`, `psqop.e`},
 {`sq_not`, `Func`, `FO`, `psqop.e`},
 {`sq_not_bits`, `Func`, `FO`, `psqop.e`},
 {`sq_or`, `Func`, `FOO`, `psqop.e`},
 {`sq_or_bits`, `Func`, `FOO`, `psqop.e`},
 {`sq_power`, `Func`, `FOO`, `psqop.e`},
 {`sq_rand`, `Func`, `FO`, `psqop.e`},
 {`sq_rmdr`, `Func`, `FOO`, `psqop.e`},
 {`sq_round`, `Func`, `FOO`, `psqop.e`},
 {`sq_seq`, `Func`, `FO`, `psqop.e`},
 {`sq_sign`, `Func`, `FO`, `psqop.e`},
 {`sq_sin`, `Func`, `FO`, `psqop.e`},
 {`sq_sqrt`, `Func`, `FO`, `psqop.e`},
 {`sq_str`, `Func`, `FO`, `psqop.e`},
 {`sq_sub`, `Func`, `FOO`, `psqop.e`},
 {`sq_tan`, `Func`, `FO`, `psqop.e`},
 {`sq_trunc`, `Func`, `FO`, `psqop.e`},
 {`sq_uminus`, `Func`, `FO`, `psqop.e`},
 {`sq_upper`, `Func`, `FO`, `psqop.e`},
 {`sq_xor`, `Func`, `FOO`, `psqop.e`},
 {`sq_xor_bits`, `Func`, `FOO`, `psqop.e`},
 {`product`, `Func`, `FO`, `psum.e`},
 {`sum`, `Func`, `FO`, `psum.e`},
 {`tagset`, `Func`, `FIIII`, `ptagset.e`},
 {`shorten`, `Func`, `FPSI`, `ptrim.e`},
 {`trim`, `Func`, `FOOI`, `ptrim.e`},
 {`trim_head`, `Func`, `FOOI`, `ptrim.e`},
 {`trim_tail`, `Func`, `FOOI`, `ptrim.e`},
 {`unique`, `Func`, `FPS`, `punique.e`},
 {`vlookup`, `Func`, `FOPIIO`, `pvlookup.e`},
 {`scanf`, `Func`, `FSS`, `scanf.e`},
 {`to_number`, `Func`, `FSOI`, `scanf.e`},
 {`shift_bits`, `Func`, `FOI`, `shift_bits.e`},
 {`shuffle`, `Func`, `FP`, `shuffle.e`},
 {`custom_sort`, `Func`, `FOPOI`, `sort.e`},
 {`sort`, `Func`, `FPI`, `sort.e`},
 {`sort_columns`, `Func`, `FPP`, `sort.e`},
 {`substitute`, `Func`, `FPPPI`, `substitute.e`},
 {`substitute_all`, `Func`, `FPPP`, `substitute.e`},
 {`is_integer`, `Func`, `FS`, `to_int.e`},
 {`to_integer`, `Func`, `FSI`, `to_int.e`},
 {`to_string`, `Func`, `FOII`, `to_str.e`},
-- {`and_bitsu`, `Func`, `FNN`, `ubits.e`},
-- {`not_bitsu`, `Func`, `FN`, `ubits.e`},
-- {`or_bitsu`, `Func`, `FNN`, `ubits.e`},
-- {`xor_bitsu`, `Func`, `FNN`, `ubits.e`},
 {`get_test_abort`, `Func`, `F`, `unit_test.e`},
 {`get_test_logfile`, `Func`, `F`, `unit_test.e`},
 {`get_test_pause`, `Func`, `F`, `unit_test.e`},
 {`get_test_verbosity`, `Func`, `F`, `unit_test.e`},
 {`set_test_abort`, `Proc`, `PI`, `unit_test.e`},
 {`set_test_logfile`, `Proc`, `PS`, `unit_test.e`},
 {`set_test_module`, `Proc`, `PS`, `unit_test.e`},
 {`set_test_pause`, `Proc`, `PI`, `unit_test.e`},
 {`set_test_verbosity`, `Proc`, `PI`, `unit_test.e`},
 {`test_equal`, `Proc`, `POOSI`, `unit_test.e`},
 {`test_fail`, `Proc`, `PS`, `unit_test.e`},
 {`test_false`, `Proc`, `PIS`, `unit_test.e`},
 {`test_not_equal`, `Proc`, `POOSI`, `unit_test.e`},
 {`test_pass`, `Proc`, `PS`, `unit_test.e`},
 {`test_summary`, `Proc`, `PI`, `unit_test.e`},
 {`test_true`, `Proc`, `PIS`, `unit_test.e`},
 {`utf16_to_utf32`, `Func`, `FP`, `utfconv.e`},
 {`utf16_to_utf8`, `Func`, `FP`, `utfconv.e`},
 {`utf32_to_utf16`, `Func`, `FP`, `utfconv.e`},
 {`utf32_to_utf8`, `Func`, `FPI`, `utfconv.e`},
 {`utf8_to_utf16`, `Func`, `FP`, `utfconv.e`},
 {`utf8_to_utf32`, `Func`, `FSI`, `utfconv.e`},
 {`vslice`, `Func`, `FPOO`, `vslice.e`},
 {`wildcard_file`, `Func`, `FPP`, `wildcard.e`},
 {`wildcard_match`, `Func`, `FPP`, `wildcard.e`}}
*/

--?"done"
--{} = wait_key()

--/*
Searching for: type
 Files scanned 203, Directories scanned 2, Lines 133330
C:\Program Files (x86)\Phix\builtins\complex.e:16 --DEV forward types needed before this can be made an autoinclude...
C:\Program Files (x86)\Phix\builtins\complex.e:17 global type complex(object c)
C:\Program Files (x86)\Phix\builtins\complex.e:20 end type
C:\Program Files (x86)\Phix\builtins\complex.e:23 -- The following type allows complex or atom arguments, the latter are treated as
C:\Program Files (x86)\Phix\builtins\complex.e:27 --  of these, by which I mean the type complexn of "either", so theoretically
C:\Program Files (x86)\Phix\builtins\complex.e:33 type complexn(object c)
C:\Program Files (x86)\Phix\builtins\complex.e:36 end type
C:\Program Files (x86)\Phix\builtins\complex.e:459 --global type complex(object z)
C:\Program Files (x86)\Phix\builtins\complex.e:460 ---- the variable type of a complex number  
C:\Program Files (x86)\Phix\builtins\complex.e:468 --end type
C:\Program Files (x86)\Phix\builtins\dict.e:38 --DEV/SUG (requires forward type [erm, may already be working, just not yet tried in psym/init]) ...needs MARKTYPES...
C:\Program Files (x86)\Phix\builtins\dict.e:39 global type dictionary(integer tid)
C:\Program Files (x86)\Phix\builtins\dict.e:41 end type
C:\Program Files (x86)\Phix\builtins\json.e:24 --                       second any of these types. Note the subscript of the first 
C:\Program Files (x86)\Phix\builtins\json.e:27 --                      and the rest any of these types. Note the subscript of the 
C:\Program Files (x86)\Phix\builtins\map.e:75 global type map(object x)
C:\Program Files (x86)\Phix\builtins\map.e:77 end type
C:\Program Files (x86)\Phix\builtins\misc.e:369 --/!**!/    type trig_range(atom x)
C:\Program Files (x86)\Phix\builtins\misc.e:372 --/!**!/    end type
C:\Program Files (x86)\Phix\builtins\misc.e:375 type trig_range(object x)
C:\Program Files (x86)\Phix\builtins\misc.e:387 end type
C:\Program Files (x86)\Phix\builtins\pcase.e:221 -- to correct something just typed in with the wrong caps lock setting.
C:\Program Files (x86)\Phix\builtins\pfrac.e:55 global type frac(object r)
C:\Program Files (x86)\Phix\builtins\pfrac.e:100 end type
C:\Program Files (x86)\Phix\builtins\ptypes.e:2 -- builtins/ptypes.e
C:\Program Files (x86)\Phix\builtins\ptypes.e:8 global type atom_string(object o)
C:\Program Files (x86)\Phix\builtins\ptypes.e:12 end type
C:\Program Files (x86)\Phix\builtins\ptypes.e:14 global type rid_string(object o)
C:\Program Files (x86)\Phix\builtins\ptypes.e:17 end type
C:\Program Files (x86)\Phix\builtins\ptypes.e:19 global type nullable_string(object o)
C:\Program Files (x86)\Phix\builtins\ptypes.e:21 end type
C:\Program Files (x86)\Phix\builtins\ptypes.e:24 --global type bool(object o)
C:\Program Files (x86)\Phix\builtins\ptypes.e:25 global type boolean(object o)
C:\Program Files (x86)\Phix\builtins\ptypes.e:27 end type
C:\Program Files (x86)\Phix\builtins\scanf.e:121             end if
C:\Program Files (x86)\Phix\builtins\text.e:32 include std/types.e
C:\Program Files (x86)\Phix\builtins\text.e:468 --include std/types.e
C:\Program Files (x86)\Phix\builtins\text.e:1112 --#withtype t_text
C:\Program Files (x86)\Phix\builtins\timedate.e:42 --  type timedate (weak validation)
C:\Program Files (x86)\Phix\builtins\timedate.e:45 --      This type speeds development by trapping errors (eg "number expected") faster, as opposed to
C:\Program Files (x86)\Phix\builtins\timedate.e:334 global type timedate(object s)
C:\Program Files (x86)\Phix\builtins\timedate.e:365 end type
C:\Program Files (x86)\Phix\builtins\timedate.e:647 --  likely to want or agree to type in, or for that matter select from a drop-down 
C:\Program Files (x86)\Phix\builtins\timedate.e:1457         end if
C:\Program Files (x86)\Phix\builtins\timedate.e:1545 --  Said messages are intended for a developer/typecheck, not an end user.
C:\Program Files (x86)\Phix\builtins\timedate.e:1565 string fmti     -- (gives an earlier/better typecheck and thus simplifies debugging)
C:\Program Files (x86)\Phix\builtins\timedate.e:1888         end if
--*/
