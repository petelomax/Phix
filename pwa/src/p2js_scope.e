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

--global 
--enum BUILTINS,    -- (re-usable, set from p2js_keywords, etc)
--   GLOBALS        -- oh, no such thing in JavaScript.....
-- Note that Phix adds a new scope for an include statement whereas pwa/p2js does not.
-- Since JavaScript does not have an include statement, pwa/p2js inlines the code and
-- therefore may trigger identifier clashes that separate files would not.

-- Note: p2js_parse.e is not calling add/drop_scope() to implement block scoping... [DEV]

sequence scopes = {}
enum BUILTINS,  -- (re-usable, set from p2js_keywords in init_scope just below)
     GLOBALS    -- (not that there's really such a thing in JavaScript........)
constant integer T_high = T_reserved[$]
sequence referenced
sequence zeroes -- DEV temp

include p2js_auto.e
sequence {auto_names, auto_procfunc, auto_sigs, auto_files} = columnize(p2js_auto)

global procedure init_scope()
--?{"init_scope",T_high}
    if length(scopes)=0 then
        integer builtins = new_dict(),
                globals = new_dict()
        scopes = {builtins,globals}
--DEV we probably shouldn't do this for autoincludes??
--better: we should build a repeat(false,length(T_keywords)) and []=true anything < T_reserved[$] [DONE]
--?auto_names
        for i=1 to length(T_reserved) do
            setd(T_reserved[i],T_toktypes[i],builtins)
--          setd({'[',T_reserved[i]},i,builtins)
            integer k = find(T_keywords[i],auto_names)
            if k then
--?{{'[',T_reserved[i]},k,T_keywords[i]}
                setd({'[',T_reserved[i]},k,builtins)
--elsif i<50 then
--  ?{"?",T_keywords[i]}
            end if
        end for
    else
        -- cleanup any mess left from a previous run...
        while length(scopes)>GLOBALS do
            destroy_dict(scopes[$])
            scopes = scopes[1..$-1]
        end while
        destroy_dict(scopes[GLOBALS], justclear:=true)
    end if
    referenced = repeat(false,length(p2js_auto))
--?T_keywords -- {"string","nullable_string",...
--?T_toktypes -- {8,9,11,3,1,1,1,3,1,12,1,3,3,1,1,... (TYPI..BADT)
--?T_reserved -- {24,88'X',136,140,160,176,192,228,244,... (ttidx)
--  T_keywords = vslice(defs,1)
--  T_toktypes = vslice(defs,2)
--  T_reserved = vslice(defs,3)
zeroes = {}
end procedure

global procedure add_scope()
--?"add_scope"
    scopes &= new_dict()    
end procedure

global procedure drop_scope()
--?"drop_scope"
    if length(scopes)<=GLOBALS then ?9/0 end if
    integer d = scopes[$]
    destroy_dict(d)
    scopes = scopes[1..$-1]
end procedure

global procedure final_scope_check()
    if length(scopes)!=GLOBALS then ?9/0 end if
end procedure

function add_id(integer ttidx, vartype, scope)
--?{"add_id",ttidx,vartype,scope}
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
        string path = get_file_path(current_file)
        if path=builtindir then
            integer k = find(ttidx,T_reserved)
            if k then
                integer l = find(T_keywords[k],auto_names)
                if l then
                    string file = get_file_name(current_file)
                    if file=auto_files[l] then return 1 end if
                end if
            end if
        end if
        return -1 -- illegal
    end if
    if getd(ttidx,scope)!=NULL then
        return 0 -- already defined
    end if
    setd(ttidx,vartype,scope)
    return 1 -- ok
end function

global function add_global(integer ttidx, vartype)
    return add_id(ttidx,vartype,scopes[GLOBALS])
end function

global function add_local(integer ttidx, vartype)
    -- catch/warn this before the call, eg see p2js_parse.e/vardef()
--  if not find(vartype,{TYPI,TYP2,TYPN,TYPQ,TYPS,TYP9,TYPM,TYPP,TYPO}) then ?9/0 end if
    if not find(vartype,{TYPI,TYP2,TYPN,TYPQ,TYPS,TYP9,TY11,TYPP,TYPO}) then ?9/0 end if
    return add_id(ttidx,vartype,scopes[$])
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
if res=0 and downto=1 and not find(ttidx,zeroes) then
zeroes &= ttidx
?{"get_type",ttidx,scope,"==>",res}
end if
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
"graphics.e","get.e","machine.e","procedure initialAutoEntry(",
--??
"ubits.e",
-- supported differently (see/directly in p2js.js)
"printf","pprntfN.e","pdate.e","pApply.e","pFilter.e","pCrashN.e","progress.e",
"prnd.e","prtnidN.e"}

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
            integer fn = open(filename,"w")
            if fn=-1 then crash("cannot open "&filename) end if
            string content = sprintf(auto,{format_timedate(date(),dfmt)})
            sequence s = apply(filter(get_text("../psym.e",GT_LF_STRIPPED),initialAutoEntries),clean)
            content &= ppf(sort_columns(s,{4,1}),{pp_Nest,1,pp_Indent,28,pp_Maxlen,120})
            puts(fn,content)
            close(fn)
            requires(-machine_bits(),false) -- restart
        end if
        abort(0)
    end if
--  {names,fp,sigs,files} = columize(p2js_auto)
--{`find`, `Func`, `FOPI`, `VM\\pFind.e`},
end procedure

if platform()!=JS then check_builtins() end if

--DEV currently manually maintained...
constant ad = {{"assert.e",{}},
               {"bsearch.e",{}},
               {"dict.e",{"pmaths.e"}},
               {"factorial.e",{}},
               {"find.e",{}},
               {`gcd.e`,{`pmaths.e`}},
               {`log10.e`,{}},
               {"match.e",{"find.e","pcase.e","pfindall.e"}},
               {`misc.e`,{}},
               {`pmaths.e`,{}},
               {"pcase.e",{"find.e"}},
               {"pcolumn.e",{}},
               {"pelapsed.e",{"match.e","pmaths.e"}},
               {"pextract.e",{}},
               {"pfactors.e",{"bsearch.e","pmaths.e","primes.e"}},
               {"pfindall.e",{"find.e"}},
               {"pfindany.e",{"find.e"}},
               {"pflatten.e",{"find.e"}},
               {"ppp.e",{"find.e"}},
               {"primes.e",{"bsearch.e","pmaths.e"}},
               {"pseqc.e",{"pmaths.e"}},
               {"psplit.e",{"find.e","match.e","pfindany.e"}},
               {"psqop.e",{"pmaths.e","log10.e","misc.e"}},
               {"psum.e",{}},
               {"ptagset.e",{"pmaths.e"}},
               {"ptrim.e",{"find.e","psqop.e","psum.e"}},
               {"punique.e",{"dict.e","sort.e"}},
               {"scanf.e",{"find.e","match.e","pcase.e"}},
               {"sort.e",{}},
               {"substitute.e",{"match.e"}},
--  res = {`match.e`,`substitute.e`,`==`,`find.e`,`pcase.e`,`pfindall.e`}
               {"wildcard.e",{"find.e","match.e","pcase.e"}}},
         {autoincludes,dependencies} = columnize(ad)

global function get_autoincludes()
    sequence res = {}
    string prev = ""
    for i=1 to length(referenced) do
        if referenced[i] and p2js_auto[i][4]!=prev then
            prev = p2js_auto[i][4]
            res = append(res,prev)
        end if
    end for
    res = append(res,"==")  -- (debug aid)
    integer rdx = 1
    while rdx<=length(res) do  -- (process all indirects as well)
        string rr = res[rdx]
        if rr!="==" then
            integer k = find(rr,autoincludes)
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
    return res
end function

--?length(s) -- 413
--pp(s)

--p2js_builtins.e, or p2js_auto.e?

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
C:\Program Files (x86)\Phix\builtins\misc.e:369 --/**/  type trig_range(atom x)
C:\Program Files (x86)\Phix\builtins\misc.e:372 --/**/  end type
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
