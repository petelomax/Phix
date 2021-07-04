--
--  pltype.e    -- localtypes handling (type inference/constant propagation)
--  ========
--
--  This is used during both initial parsing and dataflow analysis
--                          (ie pmain.e and pilx86.e respectively)
--
--  Maintains, for example:
--
--      object o,p,x
--      ...
--      o = {}
--      p = {}
--      -- o,p are seq here (duh!)
--      if integer(x) then
--          -- x is integer here (duh!)
--          o = 1
--          p = 3
--          -- o,p are integer here (duh!)
--      else
--          -- x is "not integer" here
--          -- o,p are seq here
--          p = 7
--          -- p is int here (duh!)
--      end if
--      -- x is back to object here,
--      -- o is seq|int here, whereas
--      -- p is int (only) here, in the range 3..7
--
--  While some of this may seem ridiculously obvious (duh! above), the real
--   trick, as you may have already guessed, is in correctly "flipping", as
--   per "not integer" above, undoing-but-not-forgetting, and "merging", as
--   in p is "integer in the range 3..7" above. (See also NOTALL below.)
--
--  Of course we want to do this as we parse the hll code, so that, in eg:
--
--              if integer(x) then
--                  ... <no modification of x here>
--                  if integer(x) then
--                     ^ warning: probable logic error (always true)
--                      <this code would always be executed>
--                  elsif/else
--                      <this code would never be executed>
--                  end if
--              else
--                  ...
--                  if integer(x) then return 0 end if
--                     ^ warning: probable logic error (always false)
--              end if
--
--   we can emit the "probable logic error"s as shown. Naturally what 
--   might seem obvious in such a small snippet can easily be missed in
--   more complex code - and if the compiler can help stop you from making 
--   things worse by (re-)coding for cases that cannot happen, it should.
--
--  However, we want to repeat this process for each iteration of dataflow
--   analysis. ((While some people might assume Phix is a statically typed
--   language rather than a dynamically typed one, it is in fact somewhat
--   of a hybrid of the two. Sure you have to declare all variables before
--   use (not a typical feature of dynamically typed languages), and there
--   may be other missing features you might associate with a dynamically
--   typed language, the point I am making here is that you can change all
--   variable declarations in a working program to "object" and expect it
--   to still function properly, although perhaps at reduced performance
--   and either not crashing when ideally it should or giving different
--   and/or less meaningful error messages. Anyway I digress.)) The point
--   I was going to make is that the above analysis must occur based on
--   intermediate code only, rather than any parse-time information.
--
--  Type information is deduced from jumps (opJmp, opJcc, etc) and from
--   assignments (opMove/opCos/opRepeat/etc), and reset/merged at each 
--   label (opLabel). Further, hll info is embedded in the intermediate
--   code using opCtrl.
--
--  The five pieces of information retained for each variable are:
--
--   1  type - builtin or udt,
--   2  minimum integer value,   \ ignore if type says  \
--   3  maximum integer value,   / it could be a float   \ pilx86.e only,
--   4  element type,            \ ignore if type says   /  not pmain.e
--   5  sequence length          / it is not sequence.  /
--
--  Such information is, of course, very useful when emitting code and avoids
--  many unnecessary tests, and even some udt calls.
--
--
--  Implementation strategy.
--  =======================
--
--  The main "localtype" info is stored in a collection of ten sequences:
--       lvno,      --  var no (idx to symtab)
--       lptyp,     --  prev localtype (for undo)
--       lntyp,     --  new localtype
--       ltmin,     --  min (integer) value
--       ltmax,     --  max (integer) value
--       letyp,     --  element type
--       lslen,     --  sequence length
--       lflag,     --  entry type: (SET [+NOTALL] | TEST | FTEST [+FLIPPED]) [+UNDONE]
--       ls5i,      --  il offset (idx to s5)
--       lpldx      --  prev entry for vno, if any

--  DEV we currently use length(lvno), but would probably be better off with
--  lvlen (or lmax) being the master size holder.
--
--  We also maintain a hll construct stack:
--       ctrls,     -- END/IF/ELSIF/ELSE/LOOP
--       ctrli,     -- s5 (il code) indexes
--       ctrll      -- length(lvno) at addition
--  with cmax dictating how much is actually in use.
--      
--  Performance considerations.
--  ==========================
--
--  We should not dismiss the fact we are tracking info for an arbitrary 
--   number of variables over an arbitrary amount of code.
--
--  Theoretically speaking, we should not really care about performance 
--   in the "-c" multiple-passes case, but we do care about the pmain.e/
--   single pilx86.e passes that occur during interpretation.
--   Specifically, a poor implementation might repeatedly scan the whole of
--   any stack/tables; instead we should limit any such scans. To achieve
--   this, all information is cross-linked in as many different ways that
--   I could think of. There are (at least) four different index types in 
--   use here, so a simple naming convention helps to keep us sane:
--
--  Index naming convention.
--  =======================
--
--  v:  symtab: vno or vdx (or v).              symtab[v][S_maxlv] is an ldx.
--  i:  s5 (ilcode): idx (usually/esp s5idx).   most s5[idx] are s5idx (*).
--  l:  localtype (lvno etc): ldx, lmax.        lvno[ldx] is a vno/vdx.
--                                              ls5i[ldx] is an s5idx.
--                                              lpldx[ldx] is an ldx.
--  c:  ctrls/i/l: cdx, cmax.                   ctrli[cdx] is an s5idx.
--                                              ctrll[vdx] is an ldx.
--
-- (*) of course s5 contains all sorts of other things: opcodes, flags,
--  and actually many vnos - but this program should never fetch a vno 
--  from s5, nor will you ever find any ldx or cdx in there. Neither
--  is anything in this program called after s5 has become x86 binary.
--
--  Just remember these as v - variables/var_nos
--                         i - intermediate code
--                         l - localtype tables
--                         c - constructs (if/for/etc).
--
--  In short, lvno/ls5i/lpldx/ctrli/ctrll/s5/symtab are the seven tables 
--  containing links, fairly obviously using an index on the wrong table
--  is the worst of crimes, and any temporary scratch vars/copies should
--  be appropriately named to minimise confusion.
--
--  Hence eg:
--      symtab[ldx] is *wrong*, should be symtab[vno/vdx/v].
--      some_func(vdx) is wrong if some_func accepts an ldx.
--      tmp = ctrll[vno]; lflag[tmp] is *wrong*, because you
--      will forget what index type tmp is actually holding.
--
--with trace    --DEV
--

global function rootType(integer N)
-- eg type hour(integer h) return h>=0 and h<=23 end type
--    type opening_hour(hour h) return h>=8 and h<=19 end type
--    type am_shift(opening_hour h) return h<=12 end type
-- for a variable declared as type am_shift, return the fact
--  that it is an extension of the builtin type integer.
integer nxt
--DEV broken/atom on 64bit...
--!/**/ #isginfo{N,integer,MIN,MAX,object,-1}
    while N>T_object do
        if DEBUG then
            if symtab[N][S_NTyp]!=S_Type then ?9/0 end if
        end if
        nxt = symtab[N][S_sig][2]
        N = nxt
    end while
    return N
end function

--DEV/SUG: (to simplify debugging)
--function tt_stringf(string text)
--  tt_string(text,-2)
--  return ttidx
--end function
--
--sequence ttidii = {}
----global 
--procedure lt_ttidx(object name)
--integer ttidx = iff(string(name)?tt_stringf(name):name)
--  ttidii = append(ttidii,ttidx)
--end procedure
-- and suppress output that does not match a non-empty ttidii
-- alternatively, make lt_ttidx non-global and setup immediately:
--if 0 then
--  lt_ttidx("some_var")
--end if
--
--function of_interest(integer vdx)
--  return length(ttidii)=0
--      or find(symtab[vdx][S_Name],ttidii)!=0
--end function
--(suggested/untested calls to of_interest have been pencilled in below)

--
constant diag = 0   -- TIP: get any misbehaving program as small as possible before turning
--                  --      this on, as it can produce an awful lot of output!
--                  -- TIP: remove any ?<expr> as well, to get a much smaller listing.
constant diagmsg = 0    -- just show ltDiagMsg (if diag is 0)
--constant ltypetoo = 0 -- also show symtab[N][S_ltype] updates --DEV breaks self-host...? (10/3/15)

--DEV should really close(diagfn) formally somewhere...
integer diagfn
        diagfn = 01 -- 1=print to screen (recommended: I really cannot stress enough how
                    --                    much easier things will be if you can get it all 
                    --                    on one screen, - and even then you might still 
                    --                    struggle a bit!!)
                    -- 0 = open ltdiag.txt

    if diag then
--DEV too early... (moved into dprintf)
--      if dumpil then
--          -- (without all the ltAdd etc calls from pilx86.e/ilxlate(), the listing
--          --  is just parse-time stuff, and can be misleading wrt code generation.)
--          puts(1,"\nWarning: pltype.e's diag does not work well with -dumpil...\n\n")
--          {} = wait_key()
--      els
        if not diagfn then
            puts(1,"\nWarning: pltype.e's diag (to ltdiag.txt) is ON...\n\n")
        end if
    end if

integer maxd
        maxd = 50000 -- maximum number of lines to print to ltdiag.txt

global constant SET=1,      -- entry from a var=<expr> statement (or a call)
                            -- NB do not use "if and_bits(flag,SET)" (see FTEST)...
                TEST=2      -- entry from a <type>(var) test
constant        FTEST=3     -- a TEST that can be flipped

constant FLIPPED = 4,   -- an FTEST entry has been flipped
                        --  (eg/ie after "if integer(x) then", well, DOH, x is an
                        --         integer(!); but at the next elsif/else, we "flip"
                        --         that so x is "not integer", likewise for atom/
                        --         string/sequence. NB only applies to the four
                        --         builtin types, not UDTS, and only simple tests,
                        --         not compound - such as "if atom(x) or y then".
                        --         Obviously all (F)TEST entries are trashed at
                        --         appropriate points such as the next "end if".)
                        --
         NOTALL = 4,    -- indicates var-id is not affected (SET) on all branches
                        --  (eg "x={} if a then x=1 else x=3 end if" ==> x is integer;
                        --  but "x={} if a then x=1 else Q=3 end if" ==> x is int|seq.
                        --    so, because x gets a NOTALL in the second case, it gets
                        --     merged with whatever x might have been before the "if".
                        --    NOTALL is only actually applied after an "else", but we
                        --     maintain it on prior elsif's just in case there is one.
                        --
                        --  Note also the handling of eg:
                        --          ...
                        --      (elsif)
                        --          y = c1
                        --          z = d1
                        --          if <cond> then
                        --              w = a2
                        --              x = b2
                        --              y = c2
                        --              z = d2
                        --          end if
                        --          x = b3
                        --          z = d3
                        --      (else)
                        -- ===>
                        --      w gets a2, NOTALL
                        --      x gets b3
                        --      y gets c1|c2
                        --      z gets d3
                        -- Both w,x get NOTALL at the endif (due to the non-presence
                        --  of an "else" and non-presence of anything prior to merge
                        --  with), but then x gets updated immediately afterwards.
                        -- Other things may of course also need to happen at the else,
                        --  and an inner else would leave any vars modified on all 
                        --  branches as a merged and hence *not undone* entry, but 
                        --  otherwise the above simple example essentially covers all 
                        --  the cases needed.)
                        --
         UNDONE = 8     -- entry has been undone

global constant END = #1,       -- 0b000000001
                IF = #2,        -- 0b000000010
                ELSIF = #6,     -- 0b000000110 -- \ (*NB* also used for 
                ELSE = #E,      -- 0b000001110 -- / notall, see pltype.e)
--              EE = #4,        -- 0b000000100 -- elsif/else
--              EEE = #5,       -- 0b000000101 -- elsif/else/end
                LOOP = #10,     -- 0b000010000
--25/2/19...?
--              TRY = #18,      -- 0b000011000
                SWITCH = #20,   -- 0b000100000 -- (used by ilxlate(), should not be passed here[?])
                FALLTHRU = #40, -- 0b001000000 -- ""
                SWTABLE = #80,  -- 0b010000000 -- the "switch with warning" flag, see below.
                SWFOID = #100   -- 0b100000000 -- special: "switch c=x do default" case (aka "else first")
constant        NOSWITCH = #1F  -- 0b000011111
-- (nb: and_bits(stmt,EEE) skips IF/LOOP(top), catches rest)
--DEV pretty sure it should just be ELSIF=4, ELSE=8, EEE=13 (0b01101).  *** TRY THIS ONCE REST ALL WORKING ***

--              ELSIF = 4,      -- \ (*NB* also used for 
--              ELSE = 6,       -- /  notall, see pltype.e)
--              LOOP = 8
-- (nb: and_bits(stmt,END+ELSIF) skips IF/LOOP(top), catches rest)

-- SWITCH/FALLTHRU/SWTABLE flags: (see also docs\switch.txt)
-- ==============================
--
-- Fairly obviously, these flags are left on a switch construct that has been 
--  emitted as if-construct-style-il. The SWITCH flag is informational and 
--  should have little bearing on subsequent processing. The FALLTHRU setting
--  (obviously from a "switch with fallthrough" construct) means that this
--  subsystem should treat it as a set of "if end if; if end if; ...".
--  Lastly the SWTABLE flag means that an error should be generated if for
--  any reason why the construct does not generate a jump table, eg
--      for i=1 to 4 do
--          switch i with fallthrough
--            case 1: puts(1,"one ")
--            case 2: puts(1,"two ")
--            case 3: puts(1,"three ")
--          end switch
--      end for
--  should output "one two three two three three" (DEV make me a test case)
--DEV I think this is gibberish...
--  DOH: we should generate the opposite of "else", ie:
--      if i=1 then
--          puts(1,"one ")
--          #ilasm{jump_rel32,%isJmp,0,0,:label2}
--      end if
--      if i=2 then
--          #ilasm{::label2}
--          puts(1,"two ")
--          #ilasm{jump_rel32,%isJmp,0,0,:label3}
--      end if
--      if i=3 then
--          #ilasm{::label3}
--          puts(1,"three ")
--      end if
--  (this (with appropriate cleanup) would probably deal with all our reg concerns)
--  (SWTABLE goes for a burton then) --LIES

--DEV rename as cstack, csidx?
sequence ctrls,     -- END/IF/ELSIF/ELSE/LOOP [is this really needed?]
         ctrli,     -- s5 indexes
         ctrll      -- length(lvno) at addition (speedwise)
integer cmax
        cmax = -1


sequence lvno,      --  var no (idx to symtab)
         lptyp,     --  prev localtype (for undo)
         lntyp,     --  new localtype
         ltmin,     --  min (integer) value
         ltmax,     --  max (integer) value
         letyp,     --  element type
         lslen,     --  sequence length
         lflag,     --  entry type (SET/TEST/FTEST)
         ls5i,      --  il offset (idx to s5)
         lpldx      --  prev entry for vno, if any
    lvno = {}
    lptyp = {}
    lntyp = {}
    ltmin = {}
    ltmax = {}
    letyp = {}
    lslen = {}
    lflag = {}
    ls5i = {}
    lpldx = {}

--DEV not currently maintained: (aborted attempt to fix this now in pltyp3.e)
integer lmax
        lmax = 0

procedure dprintf(string fmt, object args)
-- (print diagnostics to screen/file, opening if necessary)
    if diagfn=0 then 
        if dumpil then
            -- (without all the ltAdd etc calls from pilx86.e/ilxlate(), the listing
            --  is just parse-time stuff, and can be misleading wrt code generation.)
            puts(1,"\nWarning: pltype.e's diag does not work well with -dumpil...\n\n")
--          {} = wait_key()
            sleep(4)
        end if
        diagfn = open(mainpath&"ltdiag.txt","w")
        if diagfn=-1 then
            puts(1,"error opening "&mainpath&"ltdiag.txt\n")
            ?9/0
        end if
    end if
    if maxd then
        printf(diagfn,fmt,args)
        maxd -= 1
        if maxd=0 then
            puts(1,"\nWarning: pltype.e's diag has max'd out...\n\n")
        end if
    end if
end procedure

global procedure ltDiagMsg(sequence msg)
-- plant a marker to distinguish pmain.e use from pilx86.e use,
--  and/or separate any multiple iterations from pemit.e.
--  (msg is either "*** compiled ***\n" or "*** rescanning ***\n")
    if diag or diagmsg then
        dprintf(msg,{})
    end if
end procedure

function m3(integer i)
-- (helper rtn for showdiag, show number in 3 chars)
    if i=MININT then
        return "MIN"
    elsif i=MAXINT then
        return "MAX"
    elsif i=MAXLEN then
        return "MLN"
    elsif i>999 then
        return ">99"    -- (actually means ">999")
    elsif i<-99 then
        return "<-9"    -- (actually means "<-99")
    else
        return sprintf("%3d",i)
    end if
end function

global function ltSdesc(integer stmt)
-- convert opCtrl's stmt into human readable form.
--  used below by showdiag, and by -dumpil(see pilx86.e)
sequence desc
    desc = ""
    if and_bits(stmt,END) then
        stmt -= END
        desc = "END+"
    end if
--DEV see notes above (ELSE=8,ELSIF=4)
    if and_bits(stmt,ELSE)=ELSE then        -- (ELSE  is 0b01110)
        stmt -= ELSE
        desc &= "ELSE"
    end if
    if and_bits(stmt,ELSIF)=ELSIF then      -- (ELSIF is 0b00110)
        stmt -= ELSIF
        desc &= "ELSIF"
    end if
    if and_bits(stmt,IF) then               -- (IF    is 0b00010)
        stmt -= IF
        desc &= "IF"
    end if
--  if and_bits(stmt,TRY)=TRY then
--      stmt -= TRY
--      desc &= "TRY"
--  end if
    if and_bits(stmt,LOOP) then
        stmt -= LOOP
        desc &= "LOOP"
    end if
    if and_bits(stmt,SWITCH) then
        stmt -= SWITCH
        if length(desc) and desc[length(desc)]!='+' then
            desc &= "+"
        end if
        desc &= "SWITCH"
    end if
    if and_bits(stmt,FALLTHRU) then
        stmt -= FALLTHRU
        if length(desc) and desc[length(desc)]!='+' then
            desc &= "+"
        end if
        desc &= "FALLTHRU"
    end if
    if and_bits(stmt,SWTABLE) then
        stmt -= SWTABLE
        if length(desc) and desc[length(desc)]!='+' then
            desc &= "+"
        end if
        desc &= "SWTABLE"
    end if
    if and_bits(stmt,SWFOID) then
        stmt -= SWFOID
        if length(desc) and desc[length(desc)]!='+' then
            desc &= "+"
        end if
        desc &= "SWFOID"  -- first option is default, ie "switch x do default:" aka "else first"
    end if
    if stmt then ?9/0 end if
    return desc
end function

integer showctrls
        showctrls = 0
procedure showdiag()
-- (diagnostics, only called when constant diag, above, is non-0)
--  ctrl2 is normally 0, or 1 to show ctrls/i/v.
integer lv, flag, vno, ltyp, ptyp, ntyp
sequence flags, last, eqp, eqn
sequence desc
    if showctrls then
--      if cmax>0 then
        for cdx=1 to cmax do
            desc = "ctrls:"&ltSdesc(ctrls[cdx])
            desc &= sprintf(", ctrli:%d, ctrll:%d\n",{ctrli[cdx],ctrll[cdx]})
            dprintf(desc,0)
        end for
--      end if
        showctrls = 0
    end if
    lv = length(lvno)
--lv=0
--  integer header = 0
    if lv then
        dprintf("vno,ptype,ntype,min,max,etyp,len,ilo,pldx,flag\n",0)
        for ldx=1 to lv do
--          vno = lvno[ldx]
--          if of_interest(vno) then
--              if header=0 then
--                  dprintf("vno,ptype,ntype,min,max,etyp,len,ilo,pldx,flag\n",0)
--                  header = 1
--              end if
            flag = lflag[ldx]
            if and_bits(flag,TEST) then
                if and_bits(flag,FTEST)=FTEST then
                    flag -= FTEST
                    flags = "FTEST"
                    if and_bits(flag,FLIPPED) then
                        flag -= FLIPPED
                        flags &= "+FLIPPED"
                    end if
                else
                    flag -= TEST
                    flags = "TEST"
                end if
            elsif and_bits(flag,SET) then
                flags = "SET"
                flag -= SET
                if and_bits(flag,NOTALL) then
                    flag -= NOTALL
                    flags &= "+NOTALL"
                end if
            else
                flags = "??"
            end if
            if and_bits(flag,UNDONE) then
                flag -= UNDONE
                flags &= "+UNDONE"
            end if
            if flag then
                flags &= sprintf("+??%d??",flag)
            end if
            vno = lvno[ldx]
            if symtab[vno][S_maxlv]=ldx then
                last = " (=S_maxlv)"
            else
                last = ""
            end if
            ptyp = lptyp[ldx]
            ltyp = symtab[vno][S_ltype]
            if ptyp = ltyp then
                eqp = "="
            else
                eqp = "-"
            end if
            ntyp = lntyp[ldx]
            if ntyp = ltyp then
                eqn = "="
            else
                eqn = "-"
            end if
            dprintf("%3d,%4d%s,%4d%s,"&
                    "%s,%s,%4d,"&
                    "%s,%3d,%4d,%s%s\n",
                    {vno,ptyp,eqp,ntyp,eqn,
                     m3(ltmin[ldx]),m3(ltmax[ldx]),letyp[ldx],
                     m3(lslen[ldx]),ls5i[ldx],lpldx[ldx],flags,last})
--          end if -- of_interest(vno)
        end for
    end if
end procedure

integer lvlen       -- new value for length(lvno)[etc], after tables are packed

procedure check_links(integer vno)
-- (diagnostic: ensure links are being kept in order)
integer p, q
    p = symtab[vno][S_maxlv]
    while p do
        q = lpldx[p]
        if q>p then
            dprintf("link error!! lvlen=%d\n",lvlen)
            showdiag()
            ?9/0
        end if
        p = q
    end while
end procedure


global integer Ltype    Ltype = 0
global atom    Lmin     Lmin = 0
global atom    Lmax     Lmax = 0
global integer Letyp    Letyp = 0
global integer Lslen    Lslen = 0

--DEV (temp)
global integer ltAddshowmapsymtab = 0

integer dltyp
global procedure ltAdd(integer flag, integer v, integer ptyp, integer ntyp, integer s5idx)
--
-- :: called from Assignment/storeDest() with flag=SET, fairly obvious that one, but also
--                Branch/opJxxx with flag=TEST for Jne v=int/int=v, int/atm/str/seq/udt(v),
--                  ie/eg: object x, integer y
--                          if x=y then
--                              -- clearly x is an integer at this point
--                          end if
--                          -- (remove any type assumptions about x, see below)
--                      and
--                          if integer(x) then
--                              -- clearly x is an integer at this point
--                          else
--                              -- and "not integer" at this point
--                          end if
--                          -- (likewise remove type (TEST/FTEST) assumptions
--                          --  about x, but not necessarily any SET changes
--                          --  which may have occurred within code block(s).)
--
-- flag is SET, TEST or FTEST.
---- flag is always TEST or SET, lt_flippable may later change TEST to FTEST. [DEV]
--  DEV: opJne TESTs must be unflippable, ... or pass FTEST in most cases but
--      not that one, and perform more tests at opLabel/in ltFlip.
-- v is the var_id, ie an index into symtab
-- ptyp is the prev type, usually symtab[v][S_ltype], (which has just been tested [DEV?])
-- ntyp is the new type, ie integer in "if integer(x) then", or string in crlf="\r\n"
--  In addition, pilx86.e (but /not/ pmain.e) maintains these:
-- Lmin/Lmax are integer values, only meaningful if ntyp is non-flt
-- Letyp is <=T_object, only valid if ntyp<=T_object with T_Dsq
-- Lslen is only valid if T_Dsq<=ntyp<=T_object, and/or -1 for "unknown", -2 for "any".
--
-- NOTE: this routine deals with /linear/ code only. ie/eg o=1; o="string" SETs the
--       localtype to integer then overwrites that with string; dirt simple really.
--       More complex circumstances such as o=1 if blah o="string", which require o 
--       to gain the type "integer OR string", are NOT handled by this but via (eg)
--       one or more calls to opCtrl, in particular those with the END bit set.
--
-- Examples for "object o":
--  Assignment "o=1":
--      ltAdd(SET,tidx,T_object,T_integer,s5idx)
--  Branch "if o=1 then":
--      ltAdd(TEST,p1,T_object,T_integer,s5idx)
--          (and in this latter case pilx86 may set Lmin=Lmax=1)
--
integer ldx
    if NOLT then
        if not bind and not lint then ?9/0 end if
    end if

    if diag then
--      if of_interest(v) then
        dprintf("ltAdd(flag=%d,v=%d,ptyp=%d,ntyp=%d,s5idx=%d,emitline=%d)\n",
                {flag,v,ptyp,ntyp,s5idx,emitline})
--      end if
    end if

    if and_bits(flag,TEST)
    and ntyp<=T_object
    and ptyp<=T_object then
        -- in say "if integer(x) then elseif atom(x)" we want type
        --  of x to (temporarily) become flt, not atom(=int|flt),
        --  eg so we can incref it w/o first testing it (again).
        ntyp = and_bits(ntyp,ptyp)
--31/1/21 (tried, total bust/undone)
--  else
--      integer tmp = ptyp
--      while tmp>T_object do
--          if tmp=ntyp then exit end if
--          tmp = symtab[tmp][S_sig][2]
--          if symtab[tmp][S_NTyp]!=S_Type then ?9/0 end if
--      end while
--      if tmp=ntyp then ntyp = ptyp end if
    end if

    if DEBUG then
        dltyp = symtab[v][S_ltype]
--19/9/19: (triggered by (udt) "boolean wild_str = find('?', str) or find('*', str)" in demo\search.exw, 976!=1)
--      if dltyp!=ptyp then
        if dltyp<=T_object
        and ptyp<=T_object
        and dltyp!=ptyp then
--          ?9/0
            ?"9/0 (pltype.e line 676)"
ltAddshowmapsymtab = v
--          if diag then
            if 1 then
                dprintf("*** S_ltype[%d]!=ptyp[%d] on var %d ***\n",{dltyp,ptyp,v})
--          else
--              ?"9/0"
            end if

--          if getc(0) then end if
        end if
    end if
--4/1/16:
--  if not integer(Lmin)
--  or not integer(Lmax) then
    if isFLOAT(Lmin)
    or isFLOAT(Lmax) then
--      ?9/0
--      Lmin = MININT
--      Lmax = MAXINT
        ntyp = or_bits(ntyp,T_N)
    end if
    --
    --  *** NOTE *** we maintain [S_ltype], as (historically) it is 
    --               used throughout the main parser. However, we do
    --               NOT maintain anything else (ie min/man/etyp/len)
    --               on symtab in any way, shape, or form, and as far
    --               as pilx86.e is concerned, it picks up localtype
    --               via the global Ltype, just like the other four.
    --               (and hence while we need lptyp, we do not have 
    --                or need arrays of pmin/pmax/petyp/plen.)
    --
--if diag and ltypetoo then
--  dprintf("ltAdd(639): symtab[%d][S_ltype] = %d\n",{v,ntyp})
--end if
    symtab[v][S_ltype] = ntyp

    --
    -- Reuse SET entry for this var, if a SET one exists late enough.
    --  (btw, we never reuse TEST entries, as they get flipped etc.)
    --
--14/6/21: (problems with qchat)
if length(symtab[v])>=S_maxlv then
    ldx = symtab[v][S_maxlv]
--  ldx = iff(length(symtab[v])>=S_maxlv?symtab[v][S_maxlv]:0)
--07/03/2017: (see readme)
--  if flag=SET
--  and ldx
--  and (cmax<=0 or ls5i[ldx]>=ctrli[cmax])
--  and lflag[ldx]=SET then
--  if 0 then
--      if lvno[ldx]!=v then ?9/0 end if
----        if lntyp[ldx]!=ptyp then ?9/0 end if    -- other TEST may intervene!
--      lntyp[ldx] = ntyp
--      ltmin[ldx] = Lmin
----if ldx=1 and Lmax=6 then ?9/0 end if
--      ltmax[ldx] = Lmax
--      letyp[ldx] = Letyp
--      lslen[ldx] = Lslen
--      if flag!=SET then ?9/0 end if
--      ls5i[ldx] = s5idx   -- pointless??
--  else
        lvno = append(lvno,v)
        lptyp = append(lptyp,ptyp)
        lntyp = append(lntyp,ntyp)
        ltmin = append(ltmin,Lmin)
--if length(ltmax)=0 and Lmax=6 then ?9/0 end if
        ltmax = append(ltmax,Lmax)
        letyp = append(letyp,Letyp)
        lslen = append(lslen,Lslen)
        lflag = append(lflag,flag)
        ls5i = append(ls5i,s5idx)
--14/6/21:
        if flag=SET then    -- keep (F)TEST entries out of this chain
--      if flag=SET and ldx!=0 then -- keep (F)TEST entries out of this chain
            lpldx = append(lpldx,ldx)
            symtab[v][S_maxlv] = length(lvno)
            if diag then
                check_links(v)
            end if
        else
            lpldx = append(lpldx,0)
        end if
end if
--  end if
    if diag then
        showdiag()
    end if
end procedure

global function getLocal(integer vno)
--  if no localinfo found just returns 0, otherwise
--   sets Ltype,Lmin,Lmax,Letyp,Lslen and returns 1
integer ldx
sequence sv
    if NOLT then
        if not bind and not lint then ?9/0 end if
    end if
    sv = symtab[vno]
    if length(sv)>=S_maxlv then
        ldx = sv[S_maxlv]

        if 0 then -- 21/8/9:
            while ldx do
                if lvno[ldx]!=vno then ?9/0 end if
                if not and_bits(lflag[ldx],UNDONE) then
                    Ltype = sv[S_ltype]
-- (note that TEST entries never carry any min/max/etyp/len     [DEV maybe they could??] [or does "if a=b" do a SET?]
--      info, so all of the folllowing are quite safe.)
                    Lmin = ltmin[ldx]
                    Lmax = ltmax[ldx]
                    Letyp = letyp[ldx]
                    Lslen = lslen[ldx]
                    return 1
                end if
                ldx = lpldx[ldx]
            end while
        else
            if ldx then
                if lvno[ldx]!=vno then ?9/0 end if
                if not and_bits(lflag[ldx],UNDONE) then
                    Ltype = sv[S_ltype]
-- (note that TEST entries never carry any min/max/etyp/len (see above DEV)
--      info, so all of the folllowing are quite safe.)
                    Lmin = ltmin[ldx]
                    Lmax = ltmax[ldx]
                    Letyp = letyp[ldx]
                    Lslen = lslen[ldx]
                    return 1
                end if
            end if
        end if
    end if
    return 0    -- no (undone) entries for this var
end function

integer vno

global procedure ltCall(integer lmask, integer gmask, integer s5idx)
-- :: called from Call()/opFrame (with lmask=0), and opLoopTop.
--
-- Cancels entries as indicated by lmask/gmask, ie/eg:
--          x = 0
--          if blah then
--              x = "string"
--              p()             <-- (ltCall called from here)
--          ...
--  Now if p() can affect x, then we assume it could become anything (as
--   limited by ginfo), and reset the "last" entry for x (eg to object).
--   For this to be so, x must be a gvar (a global or file-level var),
--   and the symtab entry for p()'s [S_Efct] value has the appropriate
--   bit set (ie power(2,remainder(var_id(x),29))).
--  Note that in either case an "elsif/else" could reactivate the state
--   of x to integer/0, see ltCtrl()/mergeBlocks() and "last" below.
--
--  Naturally, at "end if" we "merge" x to "int|str" or "object".
--
--  Likewise, in
--      x = 0
--      while <condition> do
--          ...
--          x = "string"
--          ...
--      end while
--
--  The opLoopTop's lmask/gmask indicates that x is affected; as above
--   assume it could become anything (as limited by ginfo), and reset
--   the "last" entry for x (eg to object).
--
--  Thrice above I have used "last": in the first (if) example, we can
--   use the second entry for x, since it is after "if blah then", and
--   the reset of x to integer at elsif/else mentioned above is safe.
--   However when there is no entry, or it isn't suitably late, we 
--   must create a new one, to allow flip/reset/merge/etc.
--
--  As mentioned, opFrame always uses an lmask of 0 since it cannot
--   affect any locals/parameters, whereas opLoopTop can, and of course
--   both calls and loops may contain assignments to global/file vars.
--
--  Finally, the s5idx passed is planted on any new entries, to control
--   proper merging for subsequent "end if" etc.
--
integer lv, flag, undo, sNtyp, mask, ptyp, ntyp, rtyp, etyp
object ginfo
sequence sv     -- copy of symtab[vno]

    if NOLT then
        if not bind and not lint then ?9/0 end if
    end if

    if diag then
        dprintf("ltCall(lmask=%08x,gmask=%08x,s5idx=%d,emitline=%d)\n",{lmask,gmask,s5idx,emitline})
    end if

    if lmask
    or and_bits(gmask,#1FFFFFFF) then -- not E_none and not E_other(only)
--DEV is that not E_vars? ^
        lv = length(lvno)
        for ldx=1 to lv do
            vno = lvno[ldx]
            sv = symtab[vno]
            flag = lflag[ldx]
            if and_bits(flag,TEST) then
                undo = (sv[S_vtype]!=sv[S_ltype])                   -- is an undo needed? (tested again below)
            else    -- SET
                undo = (sv[S_maxlv]=ldx)                            -- last (SET) entry only
            end if
            if undo then
--DEV tryme:
--          sv = symtab[vno]
--          if (and_bits(flag,TEST) or lldx[vno]<=i)                -- last SET entry only
--          and (sv[S_vtype]!=sv[S_ltype]) then
--or just:
--          ntyp = sv[S_vtype]
--          if sv[S_ltype]!=ntyp then
-- (removing same test from below and switching the for from lv to 1 by -1)
                sv = symtab[vno]
                sNtyp = sv[S_NTyp]
                mask = power(2,remainder(vno,29))
                if (sNtyp=S_GVar2 and and_bits(gmask,mask))
                or (sNtyp=S_TVar and and_bits(lmask,mask)) then
                    ntyp = sv[S_vtype]
                    rtyp = rootType(ntyp)
                    ginfo = sv[S_gInfo]
                    if atom(ginfo) then
                        if and_bits(rtyp,T_sequence)=T_string then
                            etyp = T_integer
                        else
                            etyp = T_object
                        end if
                        ginfo = {rtyp,MININT,MAXINT,etyp,-2}
--DEV 1/10/09 (just an untested idea...)
--                  else
--                      rtyp = ginfo[gType]
                    end if
                    ptyp = sv[S_ltype]
                    if ptyp!=ntyp                                   -- not already reset
                    or (    and_bits(rtyp,T_atom)=T_integer
                        and (ltmin[ldx]!=ginfo[gMin] or ltmax[ldx]!=ginfo[gMax]))
                    or (    and_bits(rtyp,T_sequence)
                        and (letyp[ldx]!=ginfo[gEtyp] or lslen[ldx]!=ginfo[gLen])) then
                        sv = {} -- kill refcount/prevent clone
--if diag and ltypetoo then
--  dprintf("ltCall(839): symtab[%d][S_ltype] = %d\n",{vno,ntyp})
--end if
                        symtab[vno][S_ltype] = ntyp

                        if (cmax<=0 or ls5i[ldx]>ctrli[cmax])
                        and not and_bits(flag,TEST) then
                            if lvno[ldx]!=vno then ?9/0 end if
                            lptyp[ldx] = ntyp
                            lntyp[ldx] = ntyp
                            ltmin[ldx] = ginfo[gMin]
--if ldx=1 and ginfo[gMax]=6 then ?9/0 end if
                            ltmax[ldx] = ginfo[gMax]
                            letyp[ldx] = ginfo[gEtyp]
                            lslen[ldx] = ginfo[gLen]
                            ls5i[ldx] = s5idx -- pointless??
                        else
                            lvno = append(lvno,vno)
                            lptyp = append(lptyp,ntyp)
                            lntyp = append(lntyp,ntyp)
                            ltmin = append(ltmin,ginfo[gMin])
--if length(ltmax)=0 and ginfo[gMax]=6 then ?9/0 end if
                            ltmax = append(ltmax,ginfo[gMax])
                            letyp = append(letyp,ginfo[gEtyp])
                            lslen = append(lslen,ginfo[gLen])
                            lflag = append(lflag,SET)
                            ls5i = append(ls5i,s5idx)
                            lpldx = append(lpldx,symtab[vno][S_maxlv])
                            symtab[vno][S_maxlv] = length(lvno)
                            if diag then
                                check_links(vno)
                            end if
                        end if
                    end if  -- ltype!=ntyp
                end if -- and_bits(l/gmask,mask)
            end if  -- last entry only
        end for
    end if
    if diag then
        showdiag()
    end if
end procedure

procedure kill(integer killdx)
integer ldx, pldx, v
    if lvno[killdx]!=vno then ?9/0 end if
    if not and_bits(lflag[killdx],TEST) then    -- SET entries only
        ldx = symtab[vno][S_maxlv]
        if ldx=killdx then
            -- reset the start of chain:
            symtab[vno][S_maxlv] = lpldx[killdx]
        else
            -- reset some mid-chain link:
            --  (NB: fix any problems that occur here by
            --       fixing some other code, not this...)
            while 1 do
                pldx = lpldx[ldx]
                if pldx=killdx then
                    lpldx[ldx] = lpldx[killdx]
                    exit
                end if
                ldx = pldx
            end while
        end if
    end if
    if diag then
        v = 0
    end if
    if killdx<lvlen then
        v = lvno[lvlen]
        lvno[killdx] = v
        lptyp[killdx] = lptyp[lvlen]
        lntyp[killdx] = lntyp[lvlen]
        ltmin[killdx] = ltmin[lvlen]
--if killdx=1 and ltmax[lvlen]=6 then ?9/0 end if
        ltmax[killdx] = ltmax[lvlen]
        letyp[killdx] = letyp[lvlen]
        lslen[killdx] = lslen[lvlen]
        lflag[killdx] = lflag[lvlen]
        ls5i[killdx] = ls5i[lvlen]
        lpldx[killdx] = lpldx[lvlen]
        if symtab[v][S_maxlv]=lvlen then
            symtab[v][S_maxlv] = killdx
        end if
    end if
    lvlen -= 1
    lmax -= 1
    if diag then
        check_links(vno)
        if v then
            check_links(v)
        end if
    end if
end procedure

function or_type(integer t1, integer t2)
--
-- Returns a builtin bit-mask, eg:
--
--      or_type(string(=0b1000),integer(=0b0001)) yields 0b1001, aka (int|str)
--
-- Or the highest common udt type (>T_object), eg:
-- ==
--      type w(object o)
--      type x(w o)
--      type y(w o)
--      type z(y o)
--      --> or_type(x,z) yields w
--      --> or_type(y,z) yields y
--      (admittedly such a hierarchy of udts is rare)
--
-- Or the covering/common builtin bit-mask, eg:
-- ==
--      type w(object o)
--      type x(integer o)
--      type y(atom o)
--      type z(string o)
--      --> or_type(x,z) yields 0b1001
--      --> or_type(x,y) yields atom
--      --> or_type(w,x) yields object
--      (and likewise for any builtin/udt combination)
--
-- Of course, [eg] or_type(integer,integer) just returns integer.
--
    while 1 do
        if t1>T_object and t1>t2 then
            t1 = symtab[t1][S_sig][2]
        elsif t2>T_object and t2>t1 then
            t2 = symtab[t2][S_sig][2]
        else
            -- (btw: in the t1=t2 case, the next does no harm)
            t1 = or_bits(t1,t2)
            exit
        end if
    end while
    return t1
end function

procedure mergeTypes(integer tgtldx, integer srcldx)
--
-- merge the two indicated [SET] entries
--
integer tflag,sflag,Ntype,
--      Ptype,Pmin,Pmax,Petyp,Pslen     -- prev [==tgt]
--DEV (temp?)
        Ptype,Petyp,Pslen   -- prev [==tgt]
atom Pmin,Pmax

    tflag = lflag[tgtldx]
    if and_bits(tflag,TEST) then ?9/0 end if
    sflag = lflag[srcldx]
    if and_bits(sflag,TEST) then ?9/0 end if
    -- 14/3/10: (nb, may not be applicable in all cases??)
    if and_bits(sflag,NOTALL) then
        tflag = or_bits(tflag,NOTALL)
        lflag[tgtldx] = tflag
    end if
    if lvno[tgtldx]!=vno then ?9/0 end if   -- TEST may intervene!
    Ltype = lntyp[srcldx]
    Ntype = lntyp[tgtldx]
    Ntype = or_type(Ltype,Ntype)
    lntyp[tgtldx] = Ntype
    Ltype = lptyp[srcldx]
    Ptype = lptyp[tgtldx]
    Ptype = or_type(Ltype,Ptype)
    lptyp[tgtldx] = Ptype
    if and_bits(tflag,UNDONE) then
        Ntype = Ptype
    end if
--if diag and ltypetoo then
--  dprintf("mergeTypes(1002): symtab[%d][S_ltype] = %d\n",{vno,Ntype})
--end if
    symtab[vno][S_ltype] = Ntype
    Lmin = ltmin[srcldx]
    Pmin = ltmin[tgtldx]
    if Pmin>Lmin then ltmin[tgtldx] = Lmin end if
    Lmax = ltmax[srcldx]
    Pmax = ltmax[tgtldx]
--if Pmax<Lmax and tgtldx=1 and Lmax=6 then ?9/0 end if
    if Pmax<Lmax then ltmax[tgtldx] = Lmax end if
    Letyp = letyp[srcldx]
    Petyp = letyp[tgtldx]
    if Letyp>T_object then ?9/0 end if
    if Petyp>T_object then ?9/0 end if
    Petyp = or_bits(Letyp,Petyp)
    letyp[tgtldx] = Petyp
    Lslen = lslen[srcldx]
    Pslen = lslen[tgtldx]
--24/12/2020...
--  if Pslen=-1 then    -- "no idea yet"
--      Pslen = Lslen
--  els
    if Pslen!=Lslen then
        Pslen = -2      -- "any length"
    end if
    lslen[tgtldx] = Pslen
    -- and pack the table (leaving final slice to the very end):
    vno = lvno[srcldx]
    if vno!=lvno[tgtldx] then ?9/0 end if
    if symtab[vno][S_maxlv]!=srcldx then ?9/0 end if -- TEST may intervene! [DEV don't put TEST on lldx!!!]
    if lpldx[srcldx]!=tgtldx then ?9/0 end if -- ""
--  symtab[vno][S_maxlv] = tgt
    kill(srcldx)
end procedure


procedure add_ctrl(integer stmt, integer s5idx)
integer ldx
    ldx = length(lvno)+1
    if cmax=-1 then
        cmax = 1
        ctrls = {stmt}
        ctrli = {s5idx}
        ctrll = {ldx}
    else
        cmax += 1
        if cmax<=length(ctrls) then
            ctrls[cmax] = stmt
            ctrli[cmax] = s5idx
            ctrll[cmax] = ldx
        else
            ctrls = append(ctrls,stmt)
            ctrli = append(ctrli,s5idx)
            ctrll = append(ctrll,ldx)
        end if
    end if
end procedure

procedure undo__(integer ldx, integer onend)
--
-- called from ltCtrl (only).
--
--  undo a block of localtype info
--  eg/ie
--          if blah then
--              <block a>
--          else
--              <block b>
--          end if
--  At "else", we need to undo block a, and at "end if" we
--  need to undo block b. Typically after the latter, we
--  would "merge" block a with block b, and/or the entire
--  if-construct with whatever was before, but here we are
--  just preparing for that to be done elsewhere/later.
--
--  Undo blocks are always from some point in the localtype
--  table (ldx) to the end of that table.
--
integer vno, flag, ptype, special
    special = 0
    lmax = length(lvno) --DEV temp
--  while ldx<=lmax do
--      flag = lflag[ldx]
----DEV check this in -d for code quality:
--      if not and_bits(flag,UNDONE)
----           and and_bits(flag,what) then
--         and not and_bits(flag,TEST) then
----           and (not and_bits(flag,TEST)
----                or (and_bits(what,TEST) and ls5i[ldx]>ctrli[??])) then
--          vno = lvno[ldx]
--          flag += UNDONE
--          lflag[ldx] = flag
--          symtab[vno][S_ltype] = lptyp[ldx]
--      end if
--      ldx +=1
--  end while
    for l=lmax to ldx by -1 do
        flag = lflag[l]
        if not and_bits(flag,UNDONE) then
            vno = lvno[l]
            if l>ldx
            or not and_bits(flag,TEST)  -- a SET entry
            or (onend and s5[ls5i[ldx]+1]!=isOpCode) then   -- not return

                flag += UNDONE
                lflag[l] = flag
--DEV we might need an "and not and_bits(flag,NOTALL) here...
                if l>ldx and onend
                and lvno[ldx]=vno
                and lflag[ldx]=FTEST+FLIPPED then
                    --
                    -- special handling for:
                    --  if atom(x) then
                    --      x = {x}
                    --  end if
                    --  ==> mark x as sequence.
                    --  (w/o this it would just undo/merge things)
                    --  (NB several slightly more complex cases probably            [DEV]
                    --      also exist that deserve similar treatment...)
                    --
                    ptype = or_type(lntyp[ldx],lntyp[l])
                    lntyp[ldx] = ptype
--                  lptyp[l] = ptype
                    lflag[ldx] = FTEST+FLIPPED+UNDONE
                    special = 1             -- ^ remove (UNDONE) later.
--                  lpldx[ldx] = lpldx[l]
--                  lpldx[l] = ldx
--                  mergeTypes(ldx,l)
                    kill(l)
                else
                    ptype = lptyp[l]
                    if onend then
                        ptype = or_type(ptype,lntyp[l])
                    end if
--                  symtab[vno][S_ltype] = ptype
                end if
--if diag and ltypetoo then
--  dprintf("undo(1136): symtab[%d][S_ltype] = %d\n",{vno,ptype})
--end if
                symtab[vno][S_ltype] = ptype
            elsif and_bits(flag,FLIPPED) then
--if diag and ltypetoo then
--  dprintf("undo(1141): symtab[%d][S_ltype] = %d\n",{vno,lntyp[l]})
--end if
                symtab[vno][S_ltype] = lntyp[l]
            end if
----DEV erm...
--      elsif flag!=FTEST+FLIPPED+UNDONE then
--          ptype = lptyp[l]
--          symtab[vno][S_ltype] = ptype
        end if
    end for
    if special then
        lflag[ldx] = FTEST+FLIPPED
    end if
end procedure

global procedure ltFlip(integer s5idx)
--
--  undo/flip (F)TEST entries at opLabel (or undo at opEndFor)
--
integer opcode,
        chainend,   -- (an s5idx)
        nxt,        -- ("")
        tgt,        -- ("")
        flippable,  -- 1 if a TEST may be flipped, else 0
        flag,       -- copy of lflag[ldx]
        ptype

sequence optxt

    if NOLT then
        if not bind and not lint then ?9/0 end if
    end if

    opcode = s5[s5idx]
--      if diag then
----if s5idx=49 then
--          if opcode=opLabel then
--              optxt = "opLabel"
--          else
--              optxt = "opEndFor"
--          end if
--          dprintf("ltFlip(s5idx=%d,%s)\n",{s5idx,optxt})
--          showctrls = 1
--          showdiag()
----if getc(0) then end if
----trace(1)
----end if
--      end if

    --trace(1)
    lmax = length(lvno) --DEV temp

    if opcode=opLabel then
        if diag then
            optxt = "opLabel"
        end if
        flippable = 1
        chainend = s5[s5idx+3]

        if chainend then
            --
            --  1. follow link chain to end, set flippable
            --
            while 1 do
                nxt = s5[chainend]
                if nxt=0 then exit end if
                if nxt>=chainend then ?9/0 end if
                chainend = nxt
                flippable = 0
            end while
            chainend -= 3   -- shift from link field to opJxx
        end if

    elsif opcode=opEndFor then
        if diag then
            optxt = "opEndFor"
        end if
        chainend = s5[s5idx+2]-5
        flippable = 0
    else
        ?9/0
    end if
    if diag then
        dprintf("ltFlip(s5idx=%d,%s,chainend=%d,emitline=%d)\n",{s5idx,optxt,chainend,emitline})
    end if

    for ldx=lmax to 1 by -1 do  -- (renamed from i 18/8)
        tgt = ls5i[ldx]
        if chainend=0 or tgt<chainend then exit end if
        vno = lvno[ldx]
        flag = lflag[ldx]
        if not and_bits(flag,UNDONE) then
            if and_bits(flag,TEST) then
                if flippable
                and flag=FTEST
                and tgt=chainend then
                    --
                    -- a simple test such as
                    --  if sequence(x) then
                    -- can be "flipped" at else/elsif to "not sequence",
                    -- whereas eg
                    --  if sequence(x) and length(x) then
                    -- cannot, ie the TEST entry marking x a sequence
                    --  remains in force for the if-part, but is just
                    --  undone for the else/elsif part, since when x
                    --  is {} transfer of control will go there (doh).
                    --  
                    opcode = s5[chainend]
                    if opcode=opJtyp then
                        if and_bits(flag,FLIPPED) then ?9/0 end if
                        flag += FLIPPED
                        lflag[ldx] = flag
                        Ltype = lntyp[ldx]
                        --DEV triggered 27/7:
-- Update 1/10: We cannot expect this. Suppose we have:
--
--      if sequence(x) then
--          ...
--          if blah then
--              x = blah
--          end if
--          ...<block B>
--      else
--
--  Now the inner "end if" has to undo in a way that x becomes
--  "before (ie sequence) *OR* (whatever blah is)". Otherwise
--  <block B> will be mistreated. Now it is undone, so we cannot
--  undo it any more, hence at the else, when we flip our test,
--  
--                                                  if symtab[vno][S_ltype]!=Ltype then ?9/0 end if
                        --                      Ltype = xor_bits(Ltype,T_object)
                        if Ltype>T_object then ?9/0 end if
if 0 then -- 23/9/15.
                        if lptyp[ldx]>T_object then ?9/0 end if
                        Ltype = xor_bits(Ltype,lptyp[ldx])
else
--                      Ltype = xor_bits(Ltype,rootType(lptyp[ldx]))
                        ptype = lptyp[ldx]
                        Ltype = xor_bits(Ltype,rootType(ptype))
end if
--if diag and ltypetoo then
--  dprintf("ltFlip(1276): symtab[%d][S_ltype] = %d\n",{vno,Ltype})
--end if
                        symtab[vno][S_ltype] = Ltype
                        lntyp[ldx] = Ltype
                        exit    -- all done, must be (also prevents flip next)
                    end if
                end if
            end if
            flag += UNDONE
            lflag[ldx] = flag
            ptype = lptyp[ldx]
--if diag and ltypetoo then
--  dprintf("ltFlip(1288): symtab[%d][S_ltype] = %d\n",{vno,ptype})
--end if
            symtab[vno][S_ltype] = ptype
        end if
    end for
    if diag then
        showdiag()
    end if
end procedure

constant NEW=1, NONE=2, FULL=3, NORMAL=4

constant DESCS = {"NEW","NONE","FULL","NORMAL"}

procedure mergeBlocks(integer topldx, integer hingeldx, integer how)
--
-- how is one of:
--      NEW, eg:                            <-- top
--                      ... (block a)
--                      if blah then        <-- hinge
--                          ... (block b)
--                      end if              <-- mergeBlocks call
--                                              (also used at end loop)
--              top may be a containing construct or the very start(1).
--              any var modified in block b but not in a must get a
--              NOTALL bit setting, else (in both) get merged.
--              (beware the subtle difference with NORMAL, below)
--
--      NONE, eg:       if blah then        <-- top
--                          ... (block a)
--                      elsif blah then     <-- hinge
--                          ... (block b)
--                      end if              <-- mergeBlocks call
--
--              Just merge any in both a and b. There is no need to
--              flag any NOTALL bits as a second mergeBlocks with NEW 
--              is imminent (with our merged ab becoming b above).
--              [DEV probably possible to merge a/b/prev in one loop]
--
--      FULL, eg:       if blah then        <-- top
--                          ... (block a)
--                      else                <-- hinge (and prior elsifs)
--                          ... (block b)
--                      end if              <-- mergeBlocks call
--
--              NOTALL must be set on any items in a but not b, and
--              any in b but not a, and only left unset on items which
--              are in both. A 2nd mergeBlocks with NORMAL is imminent:
--
--      NORMAL, eg:                         <-- top
--                      ... (block a)
--                      if blah then        <-- hinge
--                          ... \
--                      else     } (block b)
--                          ... /
--                      end if              <-- mergeBlocks call
--
--              top may be a containing construct or the very start(1).
--              Because the else /was/ present, we are now applying the
--              NOTALL bit (or actually just preserving it as needed).
--              Superficially, this is similar to NEW, however that adds
--              NOTALL to things in b not in a, whereas NORMAL removes
--              NOTALL for things in both b and a, else preserves it.
--
integer flag,
        prvldx,
        ldx2,
        ptyp,
        ntyp

    if diag then
        dprintf("mergeBlocks(topldx=%d, hingeldx=%d, how=%s)\n",{topldx,hingeldx,DESCS[how]})
    end if
--if topldx=1 and hingeldx=5 and how=NORMAL then trace(1) end if

    if how=FULL then
        for ldx=topldx to hingeldx-1 do
            flag = lflag[ldx]
            if not and_bits(flag,TEST) then -- SET entries only
                vno = lvno[ldx]
                if symtab[vno][S_maxlv]=ldx then
                    flag = or_bits(flag,NOTALL)
                    lflag[ldx] = flag
                end if
            end if
        end for
    end if

    for ldx=lmax to hingeldx by -1 do
        flag = lflag[ldx]
        if not and_bits(flag,TEST) then -- SET entries only
            vno = lvno[ldx]
            prvldx = lpldx[ldx]
            if prvldx<topldx then   -- in second block but not first
--if prvldx!=0 then
                if how=NEW or how=FULL or how=NORMAL then
                    if how!=NORMAL then -- 14/3
                        flag = or_bits(flag,NOTALL)
                        lflag[ldx] = flag
                    end if
-- 9/1/15!!! (nope)
                    if and_bits(flag,UNDONE) then
--                  if not and_bits(flag,UNDONE) then
                        ptyp = or_type(lptyp[ldx],lntyp[ldx])
--if diag and ltypetoo then
--  dprintf("mergeBlocks(1393): symtab[%d][S_ltype] = %d (ldx=%d)\n",{vno,ptyp,ldx})
--end if
                        symtab[vno][S_ltype] = ptyp
                        lptyp[ldx] = ptyp
                    end if
                end if
--end if
            else
                if how=NORMAL
                and not and_bits(flag,NOTALL) then
                    ptyp = or_type(lptyp[prvldx],lptyp[ldx])
                    lptyp[prvldx] = ptyp
                    ntyp = lntyp[ldx]
                    lntyp[prvldx] = ntyp
--if diag and ltypetoo then
--  dprintf("mergeBlocks(1408): symtab[%d][S_ltype] = %d\n",{vno,ntyp})
--end if
                    symtab[vno][S_ltype] = ntyp
-- 6/12/2011 (test)
--                  kill(ldx)
                    mergeTypes(prvldx,ldx)  -- (kills ldx)
                else
                    mergeTypes(prvldx,ldx)  -- (kills ldx)
                end if
            end if
        elsif how=NEW or how=NORMAL then
            -- remove any TEST entries
--          if not and_bits(flag,UNDONE) then ?"!!9/0!!\n" end if
            -- (btw, above sanity check prolly applies 
            --  equally throughout this routine....)
            if and_bits(flag,UNDONE) then
                vno = lvno[ldx]
                ldx2 = symtab[vno][S_maxlv]
                if ldx2>ldx
                and and_bits(lflag[ldx2],NOTALL) then
                    ptyp = or_type(lptyp[ldx],lptyp[ldx2])
--if diag and ltypetoo then
--  dprintf("mergeBlocks(1430): symtab[%d][S_ltype] = %d (ldx=%d)\n",{vno,ptyp,ldx})
--end if
                    symtab[vno][S_ltype] = ptyp
                    lptyp[ldx2] = ptyp
                end if
                kill(ldx)
            end if
--9/1/15:
        elsif how=FULL then
            -- remove any FLIPPED entries
            if and_bits(flag,FLIPPED) then
                vno = lvno[ldx]
                ntyp = lntyp[ldx]
--if diag and ltypetoo then
--  dprintf("mergeBlocks(1444): symtab[%d][S_ltype] = %d (ldx=%d)\n",{vno,ntyp,ldx})
--end if
                symtab[vno][S_ltype] = ntyp
            end if
        end if
    end for
--DEV temp:
    if lvlen!=length(lvno) then
--DEV ltpack()
        lvno = lvno[1..lvlen]
        lptyp = lptyp[1..lvlen]
        lntyp = lntyp[1..lvlen]
        ltmin = ltmin[1..lvlen]
        ltmax = ltmax[1..lvlen]
        letyp = letyp[1..lvlen]
        lslen = lslen[1..lvlen]
        lflag = lflag[1..lvlen]
        ls5i = ls5i[1..lvlen]
        lpldx = lpldx[1..lvlen]
        lmax = lvlen
    end if
end procedure

global procedure ltCtrl(integer s5idx)
--
--  Process opCtrl,stmt,link[,emitline]
--      and opEndFor,END+LOOP,bpFor
--
--   stmt is one of IF/ELSIF/ELSE/IF+END/LOOP/LOOP+END,
--   link joins them all up, with IF/LOOP pointing at the
--        END, and the other links chaining back up, and
--   s5idx points to a link or bpFor field in s5 on entry
--        (and like the link field on opCtrl, bpFor
--         points to the last field on an opFor opcode,
--         which points right back at the bpFor. The
--         "bp" stands for "backpatch", which is what
--         we do to an opFor opcode once we finally
--         encounter the corresponding "end for".)
--
--  (btw, opEndFor's END+LOOP is strictly unnecessary, but
--        it keeps things simple and does not hurt anyone.)
--
--  Required operation:
--      IF: minimal, just log the start of the construct.
--      ELSIF/ELSE: "flip"(see FLIPPED above) or undo (F)TEST entries,  [DEV*** (bad comments)]
--                  and undo SET entries and merge if >=2nd branch.
--                  eg/ie:  if <cond> then
--                              <blah 1>
--                          elsif[1] <cond> then
--                              <blah 2>
--                          elsif[2]...
--                  At [1], just undo from <blah 1>.
--                  At [2], undo from <blah 2> and merge with <blah 1>:
--                   any vars not modified both get NOTALL bit set.
--      IF+END: similar processing to ELSIF/ELSE, less any flipping,
--              plus: if there was an ELSE, any vars without NOTALL
--                    get to replace previous
--
integer stmt, hingeldx, topldx, topidx, ctnrldx, prev, k, lidx

    if NOLT then
        if not bind and not lint then ?9/0 end if
    end if

    stmt = and_bits(s5[s5idx-1],NOSWITCH)
    if diag then
        dprintf("ltCtrl2(s5idx=%d,%s,emitline=%d)\n",{s5idx,ltSdesc(stmt),emitline})
    end if
--if s5idx=158 then trace(1) end if
    lmax = length(lvno) --DEV temp
    lvlen = lmax

--      if s5[s5idx-2]!=opEndFor then
--          if and_bits(stmt,EEE)
--          and not and_bits(stmt,LOOP) then    -- exitless loop??
--              if s5[s5idx-8??]!=opLabel then ?9/0 end if
--          end if
--      end if
    lidx = s5[s5idx]
    if and_bits(stmt,END) then
        stmt -= END
        if stmt=LOOP then   -- handle end for/while
            -- (always assume 0 or some iterations, ie merge with prior)
            -- (nb link isn't really used here, see opLoopTop/ltlooptop)
            if ctrls[cmax]!=LOOP then ?9/0 end if -- ltskip error?
            topldx = ctrll[cmax]
            undo__(topldx,1)
            cmax -= 1
            if cmax then
                ctnrldx = ctrll[cmax]
            else
                ctnrldx = 1
            end if
            mergeBlocks(ctnrldx,topldx,NEW)
        else                -- handle end if
            if stmt!=IF then ?9/0 end if
            prev = ctrls[cmax]  -- (must be an IF, ELSIF, or ELSE)  --DEV == s5[lidx-1]???
--          if lidx!=ctrli[cmax] then ?9/0 end if
            topldx = ctrll[cmax]
            cmax -= 1
            if prev=IF then             -- a simple if/end if (no elsif and no else)
                if cmax then
                    ctnrldx = ctrll[cmax]
                else
                    ctnrldx = 1
                end if
                undo__(topldx,1)
                mergeBlocks(ctnrldx,topldx,NEW)
            else
                if ctrls[cmax]!=IF then ?9/0 end if
                -- (any preceding elsif blocks have already been merged btw)
                hingeldx = topldx       -- (last elsif/else branch)
                topldx = ctrll[cmax]    -- (if construct)
                cmax -= 1
                if cmax then
                    ctnrldx = ctrll[cmax]
                else
                    ctnrldx = 1
                end if
                undo__(topldx,1)
                if prev=ELSIF then      -- an if/{elsif}/end if (no else)
                    mergeBlocks(topldx,hingeldx,NONE)   -- merge if-block with elsif-block.
                    if diag then
                        showdiag()
                    end if
                    mergeBlocks(ctnrldx,topldx,NEW)     -- merge if-construct with prev.
                elsif prev=ELSE then    -- an if[/{elsif}]/else/end if
                    mergeBlocks(topldx,hingeldx,FULL)   -- merge if-block with else-block.
                    if diag then
                        showdiag()
                    end if
                    mergeBlocks(ctnrldx,topldx,NORMAL)  -- merge if-construct with prev.
                else
-- 29/12/2011:
                    if prev=IF then ?9/0 end if
                    if prev=LOOP then ?9/0 end if
                    ?9/0
                end if
--              cmax -= 1   -- if cmax can be used...
            end if
        end if
    else    -- not END:
        if stmt=LOOP then
            ?9/0 -- now handled by ltlooptop
--          add_ctrl(stmt,s5idx)
--          lmask = s5[s5idx+1]
--          gmask = s5[s5idx+2]
----            s5idx += 2
--          -- clear all localtype info for any (possibly) altered variables
--          ltCall(lmask, gmask, s5idx)
        elsif stmt=IF then
            add_ctrl(stmt,s5idx)
        else
            -- undo and merge/NOTALL entries since the IF
            -- should this be the first ELSIF/ELSE, just undo/stack it; 
            --  otherwise undo and merge/NOTALL the two sets and replace 
            --  the last stack entry.
--          top = s5[lidx]
--          prev = s5[top-1]
            hingeldx = ctrll[cmax]
            undo__(hingeldx,0)
            prev = and_bits(s5[lidx-1],NOSWITCH)
            if prev=IF then
                -- first ELSIF/ELSE for this construct
                add_ctrl(stmt,s5idx)
            else
                -- replace an ELSIF with ELSIF/ELSE (in ctrls, that is),
                -- and merge >=2nd blocks with previous blocks.
                --  eg/ie:  if blah then
                --              x = 1
                --              y = 2
                --              z = 3
                --          elsif blah then
                --              x = 1
                --              y = "str"
                --          elsif/else...       <-- we're here
                --  ===>
                --          if blah then
                --              x = <int>
                --              y = <int|str>
                --              z = <int>,NOTALL
                --          elsif/else...
                --
--DEV 5/12/10 Relaxed for eg "switch default .. case 5" -> IF/ELSE/ELSIF/END+IF
--              if prev!=ELSIF then ?9/0 end if
                if prev!=ELSIF then
                    if prev!=ELSE then ?9/0 end if
                end if
                --                  top = s5[prev]
                ----oops: (I probably meant something like ctrls/i[cmax-1] here...)
                ----(or did I? - this should be just fine...)
                k = cmax-1
                if ctrls[k]!=IF then ?9/0 end if
                topldx = ctrll[k]   -- idx into lvno (etc)
                topidx = ctrli[k]   -- idx into s5
                if and_bits(s5[topidx-1],NOSWITCH)!=IF then ?9/0 end if
                k = s5[topidx]  -- an s5idx
                if k then   -- (if not from pmain.e)
                    -- check whether there is an ELSE for this if-construct:
                    if k<=s5idx then ?9/0 end if    -- (IF must point DOWN to the END+IF)
                    if and_bits(s5[k-1],NOSWITCH)!=END+IF then ?9/0 end if
                    k = s5[k]   -- an s5idx
                    k = and_bits(s5[k-1],NOSWITCH) -- ELSIF/ELSE
                    if k!=ELSE and k!=ELSIF then ?9/0 end if
                end if
                ctrls[cmax] = stmt
                ctrli[cmax] = s5idx
--??erm, added 7/9: (also [maybe] lmax+1)
--              ctrll[cmax] = length(lvno)+1
                if k=ELSIF then
                    mergeBlocks(topldx,hingeldx,NONE)
                else -- (k=0 (pmain.e) or k=ELSE)
                    mergeBlocks(topldx,hingeldx,FULL)
                end if
                ctrll[cmax] = length(lvno)+1
            end if
        end if
    end if
    if diag then
        showctrls = 1
        showdiag()
    end if
end procedure

global procedure ltlooptop(integer s5idx)
integer lmask, gmask

    if NOLT then
        if not bind and not lint then ?9/0 end if
    end if

    if diag then
        dprintf("ltlooptop(s5idx=%d,emitline=%d)\n",{s5idx,emitline})
        showctrls = 1
    end if
    if s5[s5idx]!=opLoopTop then ?9/0 end if
    add_ctrl(LOOP,s5idx)
    lmask = s5[s5idx+1]
    gmask = s5[s5idx+2]
    -- clear all localtype info for any (possibly) altered variables
    ltCall(lmask, gmask, s5idx)
--  if diag then    -- ltCall has just done this
--      showdiag()
--  end if
end procedure

global procedure ltskip(integer s5idx, integer waspc)
--
-- pilx86.e has skipped some code.
--  While we could have made it process any opCtrl/opLoopTop/opEndFor etc,
--  instead we elect to artificially "close" any outstanding hll constructs.
--  An example case is:-
--      procedure p(object x)
--          while 1 do
--              if sequence(x) then
--                  if length(x)=3 then exit end if
--                  <other code>
--              end if
--              <yet more code>
--          end while
--      end procedure
--      p({1,2,3})
--  The initial parser, being none the wiser, emits standard il code for
--  the two tests. However after the entire source code has been parsed, 
--  dataflow analysis reveals that p is only ever called with a sequence
--  of length 3 and hence both tests are always true, thus we can safely 
--  skip <other code> as we have just proved it will never be executed.
--  The problem is that doing so may also skip those two "end if", which
--  would leave ctrls/i/l out-of-balance. ltskip() to the rescue!
--
--  (Erm, in the above example you should consider what might happen if
--   either of the inner tests were "skipped", rather than "both" - and
--   not over-focus on the fact that outer would auto-skip that inner.)
--
integer lidx, endidx, stmt, endstmt, anymod

    if NOLT then
        if not bind and not lint then ?9/0 end if
    end if

    if diag then
        dprintf("ltskip(s5idx=%d,waspc=%d,emitline=%d)\n",{s5idx,waspc,emitline})
        showctrls = 1
        showdiag()
        anymod = 0
    end if
    for i=cmax to 1 by -1 do
        stmt = ctrls[i]
--28/12/2011 (tests added to t46 29/12) [DEV cleanme! (around march 2012 or so)]
--      if stmt=LOOP then exit end if
        lidx = ctrli[i]     -- hll construct start's link (which shd point at end)
        endidx = s5[lidx]   -- hll construct end's link
--      if endidx>s5idx then exit end if
        if stmt=IF then
            if endidx>s5idx then exit end if
            if and_bits(s5[endidx-1],NOSWITCH)!=END+IF then ?9/0 end if
            ltCtrl(endidx)
            anymod = 1
        elsif stmt=LOOP then
            -- hack to deal with a for loop...
            -- you'll have to figure out what to do with "while", I guess...
            if endidx!=opLoopTop then ?9/0 end if
            endidx = s5[lidx+3]
            stmt = s5[lidx+4]
            endstmt = s5[endidx-2]
            if stmt=opLabel then
                --??? (we have no way of finding the loop end...)
--              exit
--              endidx = s5[lidx+3]
--              if s5[endidx-2]!=opCtrl then ?9/0 end if
                if endstmt!=opCtrl then ?9/0 end if
--              if s5[endidx-1]!=END+LOOP then ?9/0 end if
--              if endidx>s5idx then exit end if
--              ltCtrl(endidx)
--              anymod = 1
            elsif stmt=opFor2 then
--              endidx = s5[lidx+8] -- "tgt" of opFor
--              endidx = s5[lidx+9] -- "tgt" of opFor (but clobbered with x86loc on last pass)
--              endidx = s5[lidx+3]
--printf(1,"ltskip:loop [endidx:%d, s5idx:%d, lidx:%d]\n",{endidx,s5idx,lidx})
--?s5
--              if s5[endidx-2]!=opEndFor then ?9/0 end if
                if endstmt!=opEndFor then ?9/0 end if
--              if endidx>s5idx then exit end if
--              ltCtrl(endidx)
--              anymod = 1
            else
                ?9/0
            end if
            if s5[endidx-1]!=END+LOOP then ?9/0 end if
            if endidx>s5idx then exit end if
            ltCtrl(endidx)
            anymod = 1
        elsif and_bits(stmt,LOOP+END) then
            ?9/0    -- oops!
--          if and_bits(s5[endidx-1],NOSWITCH)!=END+LOOP then ?9/0 end if
--          ltCtrl(endidx)
--          anymod = 1
        end if
    end for
    if diag then
        if anymod then
            dprintf("ltskip(s5idx=%d) done:\n",s5idx)
            showctrls = 1
            showdiag()
        else
            dprintf("ltskip(s5idx=%d) done(with no change)\n",s5idx)
        end if
    end if
end procedure

global procedure ltclear(integer rtnidx)
-- ::called from DoRoutineDef (start & end), TopLevel.
--  (parameter is for diag only, btw - just a little
--   hint to where/why we're being called, with
--   negative numbers indicating an end of routine.)
integer lvlen, vno, flag

    if NOLT then
        if not bind and not lint then ?9/0 end if
    end if

    if diag then
        dprintf("ltclear(rtnidx=%d,emitline=%d)\n",{rtnidx,emitline})
    end if
--DEV fixme:
--  if cmax>0 then ?9/0 end if
    if cmax>0 then
        puts(1,"cmax>0...\n")
        dprintf("*** cmax>0 ***",{})
        cmax = 0
    end if
    lvlen = length(lvno)
    if lvlen then
        for i=lvlen to 1 by -1 do
            vno = lvno[i]
            symtab[vno][S_maxlv] = 0
            flag = lflag[i]
            if not and_bits(flag,UNDONE) then
--if diag and ltypetoo then
--  dprintf("ltclear(1818): symtab[%d][S_ltype] = %d\n",{vno,lptyp[i]})
--end if
                symtab[vno][S_ltype] = lptyp[i]
            end if
        end for
        lvlen = 0
--DEV ltpack
        lvno = lvno[1..lvlen]
        lptyp = lptyp[1..lvlen]
        lntyp = lntyp[1..lvlen]
        ltmin = ltmin[1..lvlen]
        ltmax = ltmax[1..lvlen]
        letyp = letyp[1..lvlen]
        lslen = lslen[1..lvlen]
        lflag = lflag[1..lvlen]
        ls5i = ls5i[1..lvlen]
        lpldx = lpldx[1..lvlen]
        if diag then
            showdiag()
        end if
    end if
    -- (since pmain.e wants these 0, for -test handling)
    Lmin = 0
    Lmax = 0
    Letyp = 0
    Lslen = 0
end procedure


--OLD, unused:
--global function and_type(integer t1, integer t2)
----
---- Returns a builtin bit-mask, eg:
----
----        and_type(atom(=0b0011),not integer(0b1110)) yields T_N(0b0010)
----
---- Or the highest common udt type (>T_object), eg:
----        type w(object o)
----        type x(w o)
----        type y(w o)
----        type z(y o)
----        --> or_type(x,z) yields w
----        --> or_type(y,z) yields y
----        (admittedly such a hierarchy of udts is rare)
----
--  while 1 do
--      if t1>T_object and t1>t2 then
--          t1 = symtab[t1][S_sig][2]
--      elsif t2>T_object and t2>t1 then
--          t2 = symtab[t2][S_sig][2]
--      else
--          -- (btw: in the t1=t2 case, the next does no harm)
--          t1 = and_bits(t1,t2)
--          exit
--      end if
--  end while
--  return t1
--end function

