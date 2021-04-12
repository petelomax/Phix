--
-- pwa\src\p2js_tree.e
-- ===================
--
--  Simplified ternary tree for pwa/p2js, for fast identifier lookup.
--
--  A ternary tree is like the more traditional binary tree except:
--   1) it does not hold full strings, just characters, for speed.
--   2) each node is a char and a 3-way branch (<,==,>).
--   3) End of string explicitly signalled by a char='\0' node, which:
--      3a) allows arbitrary data to be stored, in the EQ slot.
--      3b) allows the tree to hold eg "add" as well as "addition".
--
--  Simplified in that identifiers can only be added, never removed.
--
--  Main Usage:
--  ==========
--
--  integer ttidx = tt_idx(integer s, e)
--
--  where s,e are the start and end indexes to text [as defined in p2js_basics]
--  arbitrary data can be stored in tt[ttidx+EQ], if needed
--
--  Defines/allows eg "function" to be tested against T_function, which is
--  of course significantly faster than slicing out a substring and testing
--  against all 8 characters of that. May also have use in (eg) a minifier.
--
--  string name = get_ttname(ttidx)
--
--  Performs a full tree traveral to reconstruct a name, which can be a tad
--  sluggish, and should only be used for error handling, emergencies, etc.
--
--  tt_reset()
--
--  Restores the ternary tree to a pristine state (ie keywords only).
--  For use in a repl-style situation, or unit testing, etc.
--
--without trace
--without type_check
without debug -- (keep ex.err clean, tt gets large and mainly gibberish)

include p2js_basics.e   -- (strictly unnecesary/ensure loaded/uses "text")

sequence tt = {}  -- The ternary tree

constant CH = 1,  -- tt[node+CH]: char this node represents
         LT = 2,  -- tt[node+LT]:                        idx to "<" chs, or 0
         EQ = 3,  -- tt[node+EQ]: if ch=0 then data else idx to next ch, or 0
         GT = 4   -- tt[node+GT]:                        idx to ">" chs, or 0

integer used = 0  -- this goes 0,4,8,12,...

--/* maybe, for minifier?:
global function get_tt_node(integer ttidx) return t[node+EQ] end function
global procedure set_tt_node(integer ttidx, object o) t[node+EQ]=o end procedure
--*/

global function tt_idx(integer s, e)--, bool lowercase)
--
--  obtain a ttidx for text[s..e] (where text is defined in p2js_basics.e)
--
    integer ch = src[s],
            pcurr = 0,
            ttidx = 0
--DOH, we want to use xml on html!
--  if lowercase            -- (is_html())
--  and ch!=lower(ch)       -- (eg "DIV" but not say "onLoad")
--  and s>1
--  and find(src[s-1],"</") then
--      -- I think I'll rip this out the instant it causes me any grief:
--      ch = lower(ch)
--      for i=s to e do
--          src[i] = lower(src[i])
--      end for
--  end if
    if used then
        while 1 do
            integer tch = tt[ttidx+CH]
            if ch=tch then
                if ch='\0' then
                    return ttidx                    -- found !
                end if
                s += 1
                ch = iff(s<=e?src[s]:'\0')
                pcurr = ttidx+EQ
            elsif ch>tch then
                pcurr = ttidx+GT
            else -- ch<tch
                pcurr = ttidx+LT
            end if
            ttidx = tt[pcurr]
            if ttidx=0 then exit end if
        end while
    end if
    --
    -- Not already there, so insert remainder of string
    --
    while 1 do
        ttidx = used
        used += 4
        if used>length(tt) then
            tt = append(tt,ch)      -- CH
            tt = append(tt,0)       -- LT
            tt = append(tt,0)       -- EQ
            tt = append(tt,0)       -- GT
        else
            tt[ttidx+CH] = ch
            tt[ttidx+LT] = 0
            tt[ttidx+EQ] = 0
            tt[ttidx+GT] = 0
        end if
        if pcurr then
            tt[pcurr] = ttidx
        end if
        if ch='\0' then exit end if
        pcurr = ttidx+EQ
        s += 1
        ch = iff(s<=e?src[s]:'\0')
    end while
    return ttidx
end function

--function tt_keyword(sequence txt, integer chk)
--  src = txt               -- (src is defined in p2js_basics.e)
--  integer ttidx = tt_idx(1,length(txt))--,false)
--  if chk!=ttidx then
--      -- you need to correct the (recently-added/moved) 
--      -- keyword entry, as is explained shortly below.
--      printf(2,"%s should be %d, not %d\n",{src,ttidx,chk})
--      {} = wait_key()
--  end if
--  return ttidx
--end function

--
-- To add an entry into p2js_keywords.e, just insert say
--  {"newkeyword",      T_newkeyword            := 2222},
-- where that 2222 is completely made up, or copied from
-- a neighbouring entry, or whatever. This will re-write
-- that file with the corrected value[s], all the way to
-- the end of the table, if and when that need be.
--
constant cont0 = """
--
-- p2js_keywords.e (nb automatically over-written, all comments get trashed)
--
-- clash detection (no single-character symbol should ever match any ttidx):
--""",
        contk = """
--
global constant p2js_keywords = {
""",
        symbols = sort('`'&`!&|#*/+-,.:;<=>?~\$%([{}])"'0A`)
--      symbols = `$,<@\|`&'`' -- #24,#2C,#3C,#40,#5C,#7C & #60
--36,44,60,64,92,96,124

--global (not used anywhere else yet...)
function is_symbol(integer c)
    return c<=127 and find(c,symbols)
end function

procedure tt_keywords(sequence defs)
    integer bRebuild = false, errcount = 0
    sequence lq = repeat("",length(defs))
    for i=1 to length(defs) do
        {sequence txt,integer typ, integer chk} = defs[i]
        -- (A leading ? just means untested. Preserve 'em.)
        if txt[1]='?' then lq[i]="?" txt = txt[2..$] defs[i][1] = txt end if
        if chk<=TOKMAX then ?9/0 end if
        if is_symbol(chk) then ?9/0 end if
        -- ^^
        -- Aside: p2js_keywords.e was carefully rejigged until the
        --  string/nullable_string/atom_string gave ttidx of 'X'
        --  for nullable_string being the only potential clash.
        --
        src = txt               -- (src is defined in p2js_basics.e)
        integer ttidx = tt_idx(1,length(txt))--,false)
        if chk!=ttidx then
            bRebuild = true
            defs[i][3] = ttidx
            errcount += 1
            if errcount<=5 then
                string ts = iff(ttidx>=32 and ttidx<128?sprintf("('%c')",ttidx):"")
                printf(2,"%s should be %d%s, not %d\n",{src,ttidx,ts,chk})
            end if
        end if
--      -- you need to correct the (recently-added/moved) 
--      -- keyword entry, as is explained shortly below.
--      printf(2,"%s should be %d, not %d\n",{src,ttidx,chk})
--      {} = wait_key()
    end for
    if bRebuild then
        if errcount>5 then printf(2,"(%d similar skipped)\n",errcount-5) end if
        --
        -- Aside: While edita/edix/notepad/etc might keep old values,
        --        it should not really matter, apart from the list of
        --        correction messages forever growing, if you keep on
        --        (manually) saving stuff already corrected, assuming
        --        you are also saving something new to fix anyway...
        --
        --        However, if you start putting comments in there, it
        --        will just rudely bin 'em. Oh, I suppose, of course,
        --        by the time this is capable oF parse/tweak/save, it
        --        will all be a little too late... probably
        --
        sequence p = include_paths(),
                 f = include_files()
        integer fdx = include_file()
        if f[fdx][2]!=`p2js_tree.e` then ?9/0 end if -- (sanity check)
        string path = p[f[fdx][1]],
               filename = join_path({path,"p2js_keywords.e"}),
               content = cont0
        --
        -- clash detection: no symbol (eg '-') can clash with any ttidx.
        --   (you'd get a crash above, and have to re-jig, at random)
        --  (but it's also a reasonable debug look-up table, of sorts)
        --
        for i=1 to length(symbols) do
            if remainder(i,10)=1 then content &= "\n--  " end if
            content &= sprintf(" `%c` = %d,",symbols[i])
        end for
        content[$] = '\n'

        content &= contk
        integer clen = 0
        for i=1 to length(defs) do
            {string n, integer t, integer c} = defs[i]
            string nq = `"` & lq[i] & n & `",`,
                   tq = TYPES[t][2]
            if n[1]='$' then n = n[2..$] end if
            string ci = sprintf(`    {%-30s %4s,   T_%-29s := %d},`,{nq,tq,n,c})
            if c>=32 and c<128 then
                if clen=0 then
                    clen = length(ci)+4
                end if
                ci &= repeat(' ',clen-length(ci))&sprintf("-- '%c'",c)
            end if
            ci &= '\n'
            if find(n,{"object","volatile","xor","yield","wait_key"}) then
                ci &= '\n'
            end if
            content &= ci
        end for
        content[-2] = '}'

        printf(1,"\nOverwrite %s and restart?",{filename})
        if not find(upper(wait_key()),{'Q','N',#1B}) then
            puts(1,"\n")
            integer fn = open(filename,"w")
            if fn=-1 then crash("cannot open "&filename) end if
            puts(fn,content)
            close(fn)
            requires(-machine_bits(),false) -- restart
        end if
        abort(0)
    end if
--  return defs[$][1]
--  return vslice(defs,2)
    T_keywords = vslice(defs,1)
    T_toktypes = vslice(defs,2)
    T_reserved = vslice(defs,3)
end procedure

--
-- Keywords etc (T_xxx constants)
-- ==============================
--
--  Coded this way so that eg "=T_atom" can inline the 48,
--  as opposed to global T_atom=tt_keyword(...), which 
--  obviously means T_atom is not known until runtime and
--  hence has to be loaded. Whether that actually makes 
--  for any measurable gain remains untested and unknown.
--
--  Note this should end up with everything needed, or at
--  least supported, for Phix, JavaScript, HTML, CSS, & C.
--
--  Reordering or inserting causes tt_keywords() to offer
--  to overwite p2js_keywords.e and restart.
--
include p2js_keywords.e
--global constant tt_reserved = tt_keywords(p2js_keywords)
--T_reserved = 
tt_keywords(p2js_keywords)
--tt_keywords = p2js_keywords

--/*
    -- to be added as needed and when they can be properly tested, not before: [erm, that idea went out the window!]
--  {"profile",         T_profile           := 384},
--  {"profile_time",    T_profile_time      := 408},
--  {"trace",           T_trace             := 428},
--  {"warning",         T_warning           := 456},
--  {"type_check",      T_type_check        := 484},
--  {"debug",           T_debug             := 504},
--  {"console",         T_console           := 536},
--  {"gui",             T_gui               := 548},
--  {"istype",          T_istype            := 692},
--  {"isinit",          T_isinit            := 712},
--  {"isginfo",         T_isginfo           := 736},
--  {"?is_prime2",                  TYPF,   T_is_prime2                     := 4132},
--  {"MIN",             T_MIN               := 752},
--  {"MAX",             T_MAX               := 764},
--  {"MAXLEN",          T_MAXLEN            := 780},
--  {"machine_func",    T_machine_func      := 832},
--  {"machine_proc",    T_machine_proc      := 852},
--  {"licence",         T_licence           := 932},
--  {"public",          T_public            := 956},
--  {"ifdef",           T_ifdef             := 1004},
--  {"elsifdef",        T_elsifdef          := 1020},
--  {"elsedef",         T_elsedef           := 1036},
--  {"WIN32",           T_WIN32             := 1060},
--  {"WINDOWS",         T_WINDOWS           := 1080},
--  {"FREEBSD",         T_FREEBSD           := 1136},
--  {"SUNOS",           T_SUNOS             := 1160},
--  {"OPENBSD",         T_OPENBSD           := 1192},
--  {"OSX",             T_OSX               := 1204},
--  {"UNIX",            T_UNIX              := 1224},
--  {"WIN32_GUI",       T_WIN32_GUI         := 1244},
--  {"SAFE",            T_SAFE              := 1292},
--  {"DATA_EXECUTE",    T_DATA_EXECUTE      := 1344},
--  {"CRASH",           T_CRASH             := 1420},
--  {"jump_table",      T_jump_table        := 1544},
--  {"strict",          T_strict            := 1652},
--  {"namespace",       T_namespace         := 1688},
    {"format",          T_format            := 3520},
--  {"PE32",            T_PE32              := 3540},
--  {"PE64",            T_PE64              := 3552},
--  {"ELF32",           T_ELF32             := 3576},
--  {"ELF64",           T_ELF64             := 3588},
--  {"DLL",             T_DLL               := 3600},
--  {"SO",              T_SO                := 3608},
--  {"GUI",             T_GUI               := 3624},
--  {"CONSOLE",         T_CONSOLE           := 3652},
--  {"icons",           T_icons             := 3672},
--  {"version",         T_version           := 3704},
--  {"manifest",        T_manifest          := 3732},
--  {"at",              T_at                := 3312},
--  {"on",              T_on                := 3312},
--  {"override",        T_override          := 4864},
--  {"EU4_1",           T_EU4_1             := 4904},
--  {"BITS32",          T_BITS32            := 5052},
--  {"BITS64",          T_BITS64            := 5064},
--  {"DLLMAIN",         T_DLLMAIN           := 5092},
--  {"PHIX",            T_PHIX              := 5116},
--  {"inline",          T_inline            := 5200},
--  {"EU4_0",           T_EU4_0             := 5248},
    {"struct",          T_struct            := 5300},
    {"virtual",         T_virtual           := 5412},
    {"dynamic",         T_dynamic           := 5480},
    {"nullable",        T_nullable          := 5512},
    {"Icallback",       T_Icallback         := 5572},
    {"Icallbacki",      T_Icallbacki        := 5580},
    {"until",           T_until             := 5604},
--*/

constant tt_init = tt,
         tt_used = used

global procedure tt_reset()
    tt = tt_init
    used = tt_used
end procedure

function traverse(integer node, tgt, string r = "")
    string res = ""
    integer ch = tt[node+CH],
            nxt = tt[node+LT]       if nxt then res = traverse(nxt,tgt,r) end if    -- recurse(LT)
    if res="" then
        if ch='\0' then
            if node=tgt then                    res = r end if                      -- found it!
        else
            nxt = tt[node+EQ]       if nxt then res = traverse(nxt,tgt,r&ch) end if -- recurse(EQ)
        end if
        if res="" then
            nxt = tt[node+GT]       if nxt then res = traverse(nxt,tgt,r) end if    -- recurse(GT)
        end if
    end if
    return res
end function

global function get_ttname(integer ttidx)
--
-- Perform a full tree traversal to reconstruct the identifier.
-- Warning: only use following a fatal error; unwise use could be awfully slow.
--
    string res = traverse(0,ttidx,"")
    return res
end function

--trace(1)
--?get_ttname(T_global) -- (seems fine)


