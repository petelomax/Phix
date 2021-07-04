--
-- pilasm.e
--
--  Process ilasm{} (inline assembly) constructs. 
--  Included by and called from pmain.e
--
--  This is a "bare minimum to get by" assembler:
--   There is no real intention of ever implementing "all" x86 opcodes,
--   instead keep it simple(ha) and implement things as actually needed.
--   As a quick example, if you have some assembly experience, you will
--   likely slap down an "enter" and "leave", which will cause an error.
--   My first question will simply be "why?", and I will simply reject
--   "because that's what I've always done". Not that I will object to 
--   someone else adding (and thoroughly testing) those instructions
--   (which may afaIac just be "skip and emit no code").
--
--  If you have not yet read (and understood) some of the ilASM constructs
--   in the builtins directory, then there really is no point reading this
--   any further until you have. No, I mean it, go and read some NOW!
--   Any will do: pHeap.e, pStack.e, pfileioN.e, there are dozens of them,
--   VM\pLen.e, VM\pSleep.e, VM\puts1.e are relatively simple starters.
--
--   There are quite a few "?9/0" in this source; I have quickly run down
--   this file marking (most of) them as either
--                                  -- sanity check (should never trigger)
--   or                             -- placeholder for more code
--   The general idea, of course, is to locate the latter asap (or in fact
--   immediately) and a quick search for similar code coupled with a few
--   minutes experimentation in eg OllyDbg to get examples of the binary
--   needed (or if you prefer a trawl through the intel docs) should mean
--   it is relatively quick to add new bits and pieces as needed. ymmv.
--   Or of course a ?9/0 triggering might be an obvious typo in your code
--   and/or an indication where some better error messages are required.
--
--      TIP: edit binary such as 2A448A 08   (sub al,byte[edx+ecx*4+8])
--                          to   #2A #44 #8A d8 
--      [in Edita] CtrlShiftB*3  0o52 0o104 0o212 d8         
--      and insert a leading 0:  0o052 0o104 0o212 d8 
--      then spot the patterns:  0o0m2 0o1r4 0osib d8 
--      (very well done at home if you followed that on first reading!)
--
--   While there is no real guarantee on the rest of Phix, there is even
--   less of one here. Obviously comments such as
--          -- 0o211 0o0ds   -- mov [dst],src
--   are hints at what we are trying to achieve, rather than 100% truth.
--   (I done me best guv, but I'd lay out serious money on someone being 
--    able to find a glaring mistake in at least one or twelve of them.)
--   (Oh, I have almost certainly confused xrm and sib at several points
--    throughout this code, but only in terms of the local integer used.)
--
--   You should NEVER trust that ilasm constructs output the right code;
--   always carefully inspect the list.asm from a "p -d" and if in doubt
--   use OllyDbg/FDBG or similar and see what that (dis)assembles. Phix
--   uses #ilASM as "the last resort" - quite prevalent in builtins/VM,
--   occasionally in library code, but almost never in application code.
--
-- Note: s5 is maintained as "sequence of integer", via a #isginfo
-- ====  statement in pmain.e; here you must always explicitly use
--       intermediate integers (with the implicit type checking it
--       involves), eg (assuming "object p2details" and "integer k")
--       instead of:
--                          s5 &= p2details
--       do (eg):
--                          k = and_bits(p2details,#FF)
--                          s5 &= k
--       or maybe just:
--                          s5 &= and_bits(p2details,#FF)
--
-- Note2: Be careful not to output any calculated bytes >=#100, as 
-- =====  such will be misinterpreted by scanforShortJmp/blurph()
--        in pemit.e as isOpCode etc and most likely crash. If you 
--        do get any such errors, and have recently modified this, 
--        well, now you know what to look for first.
--
--  TIP: If you get an indecipherable error via Aborp(), try changing
--  ===  it to Aborq(). It clearly isn't a blanket change, just those
--       that involve/occur after (eg) getCh() not getToken(). If it
--       doesn't help, please change it back. Another thing to try is
--       manually setting tokline and tokcol before calling Aborp().
--       Obviously such require either "p -cp" or "p p" to test.

-- Errors and Omissions (beyond the ?9/0 already mentioned)
-- ====================
--  You cannot obtain a hll var address "inline"; lea it first:
--
--      mov eax,[stdin] -- OK, var contents
--      push [stdin]    -- OK, var contents
--      push stdin      -- illegal, var address     <-- ***
--      lea eax,[stdin] -- \ instead use this
--      push eax        -- / sort of pairing.
--      lea eax,[p1]    -- OK, maps to [ebp+nn]
--      mov eax,p1      -- meaningless/impossible in all senses
--
--   It could be possible to introduce some new syntax to half-
--   solve(or address, geddit) this such as:
--
--      mov eax,$stdin
--
--   but there would still be no equivalent for locals/params
--   (because there is no such machine code instruction).
--   Another way to say this is that hll vars must be accessed
--   via "[name]" (ie contents) not "name" (ie address).
--
--  If you have say "constant MODE = 4" then you can reference
--   it directly, eg "push MODE" is the same as "push 4" (with
--   an assumed dword qualifier). However if you have a runtime
--   assigned constant such as "constant MAIN = create(Window.."
--   then, I hope this is obvious, you have to load it using eg
--   "push [MAIN]" instead of "push MAIN".
--
--  There are very few SSE (etc) instructions. My strategy is to
--   translate working FASM examples, as and when I find them.
--
--  Of COURSE I use Intel syntax. No SANE person would ever pick
--  AT&T syntax unless constrained by other closed source or way
--  too difficult to modify tools further down the chain. But as
--  there are no further tools, Intel syntax is FAR easier, both
--  to read AND write than AT&T, and if you disagree then pffft!
--
without trace

-- #ilasm label tables:

sequence lblchain,      -- backpatch chain into s5 for final fixup
         lblpos,        -- label offset (length(s5) at definition)
         lblopa,        -- opAsm addr (to grab actual x86 addr)
         lblcol,        -- for error reporting
         lblline,       --  ""
         lblttidx,      -- to clear tt[idx+EQ] when done
         lblused        -- flag indicating whether label used, avoids
                        --  warnings for eg:
                        --      if DEBUG then   -- (hence emitON=0)
                        --          #ilasm{...,isJmp,0,0,:lbl,...}
                        --      end if
                        --      ...
                        --      #ilasm{::lbl}
                        -- Naturally if emitON is the other way round,
                        --  jmp emitted but label not, it is an error.

-- verify that the compiler is setting these as "sequence of integer":
--/**/  #isginfo{lblchain,0b0100,MIN,MAX,integer,-2}

    lblchain = {}
    lblpos = {}
    lblopa = {}
    lblcol = {}
    lblline = {}
    lblttidx = {}
    lblused = {}

integer ridState = 0    -- bits:
                        --  #0   initial value
                        --  #1   routine_id processed
                        --  #2   $_Ltot processed
                        --  #4   $_il processed
                        -- valid transtions (of ridState):
                        --  (mov edx,)routine_id(): 0 -> 1
                        --  (mov ecx,)$_Ltot:       1 -> 3, 5 -> 0
                        --  (jmp) $_il:             1 -> 5, 3 -> 0
integer ridLtot, ridN

function ridStateError()
string errmsg
    if and_bits(ridState,1)=0 then
        errmsg = "missing routine_id??" -- (should never happen)
    elsif and_bits(ridState,2)=0 then
        errmsg = "missing $_Ltot"
    elsif and_bits(ridState,4)=0 then
        errmsg = "missing $_il"
    end if
    return errmsg
end function

global procedure label_fixup()
--
-- Complete backpatching of any #ilasm labels, and
--  emit errors/warnings for undefined/unused labels.
-- Called direct from ilasm() for top_level code,
--  and/or from end of DoRoutineDef() for routines.
-- A second round of ilasm label fixups occurs at
--  the end of ilxlate() in pilxl.e, btw.
-- For more details, see design notes in pops.e.
--
integer lblidx      -- index to lblchain..lblused
integer lblposi     -- copy of lblpos[i]
integer lblopai     -- copy of lblopa[i]
integer lblbp       -- label backpatch work var
integer tmp

    for i=1 to length(lblchain) do
        lblidx = lblttidx[i]
        tt[lblidx+EQ] = 0       -- (dropscope)
        lblbp = lblchain[i]
        if lblbp then
            lblposi = lblpos[i]
            if lblposi!=-1 then
                lblopai = lblopa[i]
                while 1 do
                    tmp = s5[lblbp]
                    s5[lblbp-1] = lblopai   -- link to containing opAsm
                    s5[lblbp] = lblposi     -- il offset of label definition
                    if not tmp then exit end if
                    lblbp = tmp
                end while
            else
                tokline = lblline[i]
                tokcol = lblcol[i]
                Aborp(getname(lblidx,-3)&" is not defined")
            end if
        elsif not lblused[i] then -- (isJmp may be under emitON=0)
            Warn(getname(lblidx,-3)&" is not used",lblline[i],lblcol[i],0)
        end if
    end for
    lblchain = {}
    lblpos = {}
    lblopa = {}
    lblcol = {}
    lblline = {}
    lblttidx = {}
    lblused = {}
end procedure

--constant m4 = allocate(4),
--       m44 = {m4,4},
--       m22 = {m4,2}

procedure apnds5dword(atom v)
-- break up a dword constant into 4 bytes
string s
atom m4 = allocate(4)
    poke4(m4, v) -- faster than doing divides etc. (idea from database.e)
    s = peek({m4,4})
    free(m4)
    for i=1 to 4 do
        s5 &= s[i]
    end for
end procedure

--/*
procedure apnds5qword(atom v)
-- break up a dword constant into 8 bytes (will not work for >53 significnat bits, eg #8000000000000001 is a definite no-no)
--string s
    poke4(m4, and_bits(v,#FFFFFFFF))
    s5 &= peek(m44)
    poke4(m4, floor(v/#100000000))
    s5 &= peek(m44)
end procedure
--*/

procedure apnds5word(integer v)
-- break up a dword constant into 2 bytes
string s
atom m2 = allocate(2)
    poke2(m2, v) -- faster than doing divides etc. (idea from database.e)
--  s = peek2u(m22)
    s = peek({m2,2})
    free(m2)
    for i=1 to 2 do
        s5 &= s[i]
    end for
end procedure

-- permission bits, further documented in get_operand().
constant P_REG = #0001,
         P_MEM = #0002,
         P_RAW = #0003, -- raw data section offset (eg mov rax,[ds+8] -- symtabptr)
         P_LBL = #0004,
         P_IMM = #0008,
         P_VAR = #0010, -- [hllvar]     (result)
         P_GBL = #0020, -- global label (result)
         P_SEG = #0040, -- segment register [for tls handling] --DEV
         P_FPU = #0080,
         P_XMM = #0100,
         P_RID = #0200,
         P_RM = or_bits(P_REG,P_MEM),
         P_RI = or_bits(P_REG,P_IMM),
         P_LI = or_bits(P_LBL,P_IMM),
         P_RLI = or_bits(P_REG,P_LI),
         P_RMLI = or_bits(P_RM,P_LI),
         P_RMLIS = or_bits(P_RMLI,P_SEG),
         P_ML = or_bits(P_MEM,P_LBL),
         P_MLS = or_bits(P_ML,P_SEG),
         P_MV = or_bits(P_MEM,P_VAR),
         P_XMMM = or_bits(P_XMM,P_MEM)

constant REG32 = {T_eax,T_ecx,T_edx,T_ebx,T_esp,T_ebp,T_esi,T_edi},
         REG16 = {T_ax,T_cx,T_dx,T_bx,T_sp,T_bp,T_si,T_di},
         REG8  = {T_al,T_cl,T_dl,T_bl,T_ah,T_ch,T_dh,T_bh}
--DEV:
        ,
         R64 = {T_rax,T_rcx,T_rdx,T_rbx,T_rsp,T_rbp,T_rsi,T_rdi,T_r8,T_r9,T_r10,T_r11,T_r12,T_r13,T_r14,T_r15},
         R32 = {T_r8d,T_r9d,T_r10d,T_r11d,T_r12d,T_r13d,T_r14d,T_r15d},
         R16 = {T_r8w,T_r9w,T_r10w,T_r11w,T_r12w,T_r13w,T_r14w,T_r15w},
         R8l = {T_r8l,T_r9l,T_r10l,T_r11l,T_r12l,T_r13l,T_r14l,T_r15l},
         R8b = {T_spl,T_bpl,T_sil,T_dil,T_r8b,T_r9b,T_r10b,T_r11b,T_r12b,T_r13b,T_r14b,T_r15b}
--/*
constant r8={"al","cl","dl","bl","ah","ch","dh","bh","r8l","r9l","r10l","r11l","r12l","r13l","r14l","r15l"},
--DEV    r8b={"al","cl","dl","bl","spl","bpl","sil","dil","r8b","r9b","r10b","r11b","r12b","r13b","r14b","r15b"}, -- (if there is a rex prefix? (#40 will do))
-- (r8b..r15b are exactly the same as r8l..r15l)
         r8l={"spl","bpl","sil","dil"},
         r16={"ax","cx","dx","bx","sp","bp","si","di","r8w","r9w","r10w","r11w","r12w","r13w","r14w","r15w"},
--*/
constant XMM = {T_xmm0,T_xmm1,T_xmm2,T_xmm3,T_xmm4,T_xmm5,T_xmm6,T_xmm7}

integer Z64 -- like/copy of pglobals.e/X64, but with ilASM{[PE64]} etc applied. (should ==X64 when emitON=true, I think)
-- comment 19/11/14: I think Z64 should be used to control parsing, X64 to control binary output (when emitON=true!).

constant SIZES = {T_byte,T_word,T_dword,T_qword,T_tbyte,T_tword},
         SIZE  = {   1,     2,      4,      8,     10,    -10}

constant FREG = {T_st0, T_st1, T_st2, T_st3, T_st4, T_st5, T_st6, T_st7}

global procedure Aborq(sequence msg)
    tokline = line
    tokcol = col
    Aborp(msg)
end procedure

function get_offset(integer res, integer sign)
-- common code for get_mem() and get_operand()
--integer res = 0
integer N
sequence symtabN

    if not find(sign,{-1,0,1}) then ?9/0 end if -- (-,*,+)
    while 1 do
        if toktype=LETTER then
            N = InTable(InAny)
            if N<=0 then Undefined() end if
            symtabN = symtab[N]
            if symtabN[S_vtype]!=T_integer
            or symtabN[S_NTyp]!=S_Const
            or not symtabN[S_Init] then
                Aborp("illegal")
            end if
            if sign=0 then
                res *= symtabN[S_value]
            else
                res += sign*symtabN[S_value]
            end if
        elsif toktype=DIGIT then
            if sign=0 then
                res *= TokN
            else
                res += sign*TokN
            end if
        else
            Aborp("unrecognised")
        end if
        if Ch='+' then
            getToken()
            getToken()
            sign = 1
        elsif Ch='-' then
            getToken()
            getToken()
            sign = -1
        elsif Ch='*' then
            getToken()
            getToken()
            sign = 0
        else
            exit
        end if
    end while
    return res
end function

constant valid_ds_offsets = {0,4,8,12,16,20,24}

function findreg(integer rex)
-- factored out mainly for the error handling (eg [ebx+rax]), which I
--  for one tend to trip up on quite alot when converting 32 to 64bit.
integer res
    if rex then
        res = find(ttidx,R64)
    else
        res = find(ttidx,REG32)
    end if
    if res=0 then
        if rex then
            if find(ttidx,REG32) then
                Aborp("invalid: mixed register size")
            end if
        else
            if find(ttidx,R64) then
                Aborp("invalid: mixed register size")
            end if
        end if
    end if
    return res
end function

--function get_mem(integer permitted, integer size)
function get_mem(integer permitted, integer size, bool bSet)
-- see get_operand. (factored out for "[]" and "dword[]" etc handling)
integer scale = 0
integer idx = 0
integer base = 0
integer offset = 0
integer N
sequence symtabN
object vtype
integer state
integer sType
integer lblidx
integer rex

    if not and_bits(permitted,P_MEM) then
        Aborp("not permitted")
    end if
    getCh()
    getToken()
    if toktype!=LETTER then
        -- instead of [imm32] code (say) mov eax,imm32, ...[eax];
        -- instead of [d8+reg] you must code [reg+d8]
        Aborp("register or hllvar expected")
    end if
    rex = 0
    base = find(ttidx,REG32)
    if base=0 and Z64=1 then
        base = find(ttidx,R64)
        rex = #40
--      size = 8?
    end if
    if base!=0 then
        if Ch='*' then  -- [i*s+b]
            idx = base
            base = 0
            getCh()
            getToken()
            if toktype!=DIGIT
            or not find(TokN,{2,4}) then    -- 8? [DEV]
                Aborp("unrecognised")
            end if
            scale = TokN
            if Ch!='+' then
                Aborp("+ expected")
            end if
            getToken()
            if Ch=':' then  -- [i*s+:lbl] form (for jump tables)
                if not and_bits(permitted,P_LBL) then
                    Aborp("not permitted")
                end if
                getCh()
                tokline = line
                tokcol = col
                lblidx = il_search(':')
                if lblidx then  -- extend existing backpatch chain
                    Aborp("not permitted")
                end if
                -- start a new backpatch chain
                lblchain = append(lblchain,0)
                lblidx = length(lblchain)
                lblpos = append(lblpos,-1)
                lblopa = append(lblopa,-1)
                lblline = append(lblline,tokline)
                lblcol = append(lblcol,tokcol)
                lblused = append(lblused,1)
                lblttidx = append(lblttidx,ttidx)
                tt[ttidx+EQ] = length(lblttidx)
                if Ch!=']' then
                    Aborp("] expected")
                end if
                getToken()
--              return {P_MEM+P_LBL,size,{scale,idx,lblidx,rex}}
                return {P_MEM+P_LBL,size,{scale,idx,lblidx}}
            end if
            getToken()
            if toktype=LETTER then
--              base = find(ttidx,REG32)
--              if base=0 and Z64=1 then
--                  base = find(ttidx,R64)
----                    rex = ??
----                    size = 8?
--              end if
                base = findreg(rex)
            end if
            if base=0 then
                Aborp("base register expected")
            end if
        elsif Ch='+' then
            getToken()
            getToken()
            if toktype=LETTER then
--              idx = find(ttidx,REG32)
--              if idx=0 and Z64=1 then
--                  idx = find(ttidx,R64)
----                    rex = ??
----                    size = 8?
--              end if
                idx = findreg(rex)
                if idx=0 then
                    offset = get_offset(0,1)
                else --idx!=0
                    if Ch='*' then  -- [b+i*s]
                        getToken()
                        getToken()
                        if toktype!=DIGIT
--4/9/14:
--                      or not find(TokN,{2,4}) then --8?
                        or not find(TokN,{2,4,8}) then
                            Aborp("unrecognised")
                        end if
--if TokN=8 then trace(1) end if
                        scale = TokN
                    end if
                end if
            else
                if toktype!=DIGIT then
                    Aborp("unrecognised")
                end if
                offset = TokN
            end if
        end if
        if Ch='+' then
            getToken()
            getToken()
--          offset += get_offset(1)
            offset = get_offset(offset,1)
        elsif Ch='-' then
            getToken()
            getToken()
--          offset += get_offset(-1)
            offset = get_offset(offset,-1)
        elsif Ch='*' then
            getToken()
            getToken()
--          offset *= get_offset(1)
            offset = get_offset(offset,0)
        end if
        if Ch!=']' then
            Aborp("] expected")
        end if
        getToken()
--      return {P_MEM,size,{scale,idx,base,offset,rex}}
        return {P_MEM,size,{scale,idx,base,offset}}
    elsif ttidx=T_ds then
        -- raw data section offsets:
        --  [ds+0|4|8|12|16|20|24] (see valid_ds_offsets)
        --  [ds+idx*scale+base+0|4|8|12|16|20|24]
        if not newEmit then -- DEV temp
            if intellisense=0 then
                Aborp("not permitted (newEmit=0)")
            end if
        end if
        if Ch!='+' then
            Aborp("+ expected")
        end if
        getCh()
        getToken()
        if toktype=LETTER then
            base = find(ttidx,REG32)
            if base=0 and Z64=1 then
                base = find(ttidx,R64)
                rex = #40
            end if
            if base=0 then
                Aborp("unrecognised")
            end if
            if Ch='*' then  -- [i*s+n]
                idx = base
                base = 0
                getCh()
                getToken()
                if toktype!=DIGIT
                or not find(TokN,{2,4,8}) then
                    Aborp("unrecognised")
                end if
                scale = TokN
            end if
            if Ch='+' then
                getCh()
                getToken()
--              if toktype=LETTER then
--?9/0 --DEV this can be removed, I think... (only [ds+imm] allowed, for initStack/opRand only)
--                  if base=0 then
----                        base = find(ttidx,REG32)
----                        if base=0 and Z64=1 then
----                            base = find(ttidx,R64)
----                        end if
--                      base = findreg(rex)
--                      if base=0 then
--                          Aborp("unrecognised")
--                      end if
--                  else
----                        idx = find(ttidx,REG32)
----                        if idx=0 and Z64=1 then
----                            idx = find(ttidx,R64)
----                        end if
--                      idx = findreg(rex)
--                      if idx=0 then
--                          Aborp("unrecognised")
--                      end if
--                      if Ch='*' then  -- [i*s+n]
--                          getCh()
--                          getToken()
--                          if toktype!=DIGIT
--                          or not find(TokN,{2,4,8}) then
--                              Aborp("unrecognised")
--                          end if
--                          scale = TokN
--                      end if
--                  end if
--                  if Ch='+' then
--                      getCh()
--                      getToken()
--                      if toktype!=DIGIT
--                      or not find(TokN,valid_ds_offsets) then
--                          Aborp("unrecognised")
--                      end if
--                      offset = TokN
--                  end if
--              else
                    if toktype!=DIGIT
                    or not find(TokN,valid_ds_offsets) then
                        Aborp("unrecognised")
                    end if
                    offset = TokN
--              end if
            end if
        else
            if toktype!=DIGIT
            or not find(TokN,valid_ds_offsets) then
                Aborp("unrecognised")
            end if
            offset = TokN
        end if
        if Ch!=']' then
            Aborp("] expected")
        end if
        getToken()
--      return {P_RAW,size,{scale,idx,base,offset,rex}}
        return {P_RAW,size,{scale,idx,base,offset}}
    else
        N = InTable(InAny)
        if N<=0 then Undefined() end if
        symtabN = symtab[N]
        vtype = symtabN[S_vtype]
        if not integer(vtype) then  -- (ie a proc/func/type)
            Aborp("illegal")
        end if
        state = symtabN[S_State]
        if and_bits(state,K_Fres) then ?9/0 end if      -- internal error
--DEV should set/used be a param [integer hllstateflag]? (spotted in passing) [done as bSet/K_asmm, 24/4/21]
--      if not and_bits(state,S_set) then
        integer mask = iff(bSet ? S_set+K_asmm : S_set)
        if and_bits(state,mask)!=mask then
--          state += S_set
            state = or_bits(state,mask)
            symtab[N] = 0   -- kill refcount
            symtabN[S_State] = state
            symtab[N] = symtabN
        end if
        sType = symtabN[S_NTyp]
        if emitON then
            if sType=S_GVar2 then -- or Const?
                SideEffects = or_bits(SideEffects,power(2,remainder(N,29)))
            elsif sType=S_TVar then
                lMask = or_bits(lMask,power(2,remainder(N,29)))
            end if
        end if
        if Ch!=']' then
            Aborp("] expected")
        end if
        getToken()
if size=0 then -- added 27/8/14
        size = 4
        if X64 then
            size = 8
        end if
end if
        return {P_VAR,size,{N,sType}}
    end if
end function

tt_string("dword",-3)
constant Z_dword = ttidx  -- Allows us to pretend we found T_dword when using
                          -- the wrong (label) terminator. For jmp dword[sib].
tt_string("qword",-3)
constant Z_qword = ttidx  -- ditto, but for jmp qword[sib]

-- Sigh. As soon as I said that was it... Nevermind (small price to pay really)
tt_string("eax",-3) constant Z_eax = ttidx
tt_string("ecx",-3) constant Z_ecx = ttidx
tt_string("edx",-3) constant Z_edx = ttidx
tt_string("ebx",-3) constant Z_ebx = ttidx
tt_string("esp",-3) constant Z_esp = ttidx
tt_string("ebp",-3) constant Z_ebp = ttidx
tt_string("esi",-3) constant Z_esi = ttidx
tt_string("edi",-3) constant Z_edi = ttidx
constant REZ32 = {Z_eax,Z_ecx,Z_edx,Z_ebx,Z_esp,Z_ebp,Z_esi,Z_edi}
tt_string("rax",-3) constant Z_rax = ttidx
tt_string("rcx",-3) constant Z_rcx = ttidx
tt_string("rdx",-3) constant Z_rdx = ttidx
tt_string("rbx",-3) constant Z_rbx = ttidx
tt_string("rsp",-3) constant Z_rsp = ttidx
tt_string("rbp",-3) constant Z_rbp = ttidx
tt_string("rsi",-3) constant Z_rsi = ttidx
tt_string("rdi",-3) constant Z_rdi = ttidx
tt_string("r8" ,-3) constant Z_r8  = ttidx
tt_string("r9" ,-3) constant Z_r9  = ttidx
tt_string("r10",-3) constant Z_r10 = ttidx
tt_string("r11",-3) constant Z_r11 = ttidx
tt_string("r12",-3) constant Z_r12 = ttidx
tt_string("r13",-3) constant Z_r13 = ttidx
tt_string("r14",-3) constant Z_r14 = ttidx
tt_string("r15",-3) constant Z_r15 = ttidx
constant REZ64 = {Z_rax,Z_rcx,Z_rdx,Z_rbx,Z_rsp,Z_rbp,Z_rsi,Z_rdi,Z_r8,Z_r9,Z_r10,Z_r11,Z_r12,Z_r13,Z_r14,Z_r15}


--  <aside>
--      A full explanation of why we need/have both T_xxx and Z_xxx is quite
--      tedious and not really very interesting. Feel free to skip this note.
--      The bulk of the T_xxx live in the main (terminator -2) table and do
--      *NOT* store anything in tt[ttidx+EQ], so will not interfere with the 
--      outside world (hll) in any way, shape, or form. And we can still use
--      getToken() etc with all the added (esp number) goodness that has, and
--      indeed reference hll variables and constants just like normal, with
--      all the proper scope rules (using the standard InTable(InAny)) etc.
--      However ilASM labels /DO/ need to store an idx in tt[ttidx+EQ] and
--      hence live in a "separate" (terminator -3) table (ie they share the
--      letters but end differently). Now, because we want eg "jmp lbl" to
--      behave as "jmp :lbl", we /MUST/ il_search (which goes -3) but a
--      couple of minor cases (jmp dword[], jmp ecx) have wandered off down
--      the wrong tree (for T_xxx constant use). Maybe we could reset col 
--      and run both, or ...nah: just add the Z_s. Told you it was boring.
--  </aside>

--with trace
function get_label()
-- factored out code for get_operand() and jcc()
integer lblidx
integer reg
integer k, gflags
--  if Ch='%' then -- global label
    k = find(Ch,"%!>")
    if k!=0 then -- global label
        tokline = line
        tokcol = col
        lblidx = il_search(':')
if emitON then -- added 10/10/14
        if lblidx then
            if lblidx>length(glblused) then
                Aborq("label index error")
            end if
            if not and_bits(glblused[lblidx],G_used) then
                glblused[lblidx] += G_used
            end if
        else
--          glblused = append(glblused,G_used)
            gflags = G_used
            if k=2 then
                gflags += G_bang
            elsif k=3 then
                gflags += G_init
            end if
            glblused = append(glblused,gflags)
            glboffset = append(glboffset,0)
--          glbttidx = append(glbttidx,ttidx)
            glblabel = append(glblabel,fileno)
            glblline = append(glblline,tokline)
            glblcol = append(glblcol,tokcol)
            glblname = append(glblname,"??("&text[tokcol..col-1]&")")
            lblidx = length(glblused)
            tt[ttidx+EQ] = lblidx
--?{ttidx,glttidxTestM}
--if ttidx=glttidxTestM then trace(1) end if
            agchecktt(ttidx)
        end if
end if
        return {P_GBL,0,lblidx}
    else -- (local label)
        tokline = line
        tokcol = col
        if Ch=':' then Aborp("cannot define a new label here") end if
        lblidx = il_search(':')
--?{"get_label",lblidx,ttidx,tokcol}
--      if ttidx=Z_dword then       -- (as T_dword but -3 terminator)
        if ttidx=Z_dword            -- (as T_dword but -3 terminator)
        or ttidx=Z_qword then
            skipSpacesAndComments()
            if Ch!='[' then
                Expected("'['")
            end if
--          return get_mem(P_MEM,0)
            return get_mem(P_MEM,0,false)
        end if
        reg = find(ttidx,REZ32)
        if reg then
            return {P_REG,4,reg}
        end if
        if Z64=1 then
            reg = find(ttidx,REZ64)
            if reg then
                return {P_REG,8,reg}
            end if
        end if
if emitON then -- added 10/10/14
        if lblidx then  -- extend existing backpatch chain
            if lblidx<=0 or lblidx>length(lblttidx) then
                Aborp("bad lblidx (bug in phix)")
            end if
            if ttidx!=lblttidx[lblidx] then ?9/0 end if -- sanity check
        else            -- start a new backpatch chain
            lblchain = append(lblchain,0)
            lblidx = length(lblchain)
            lblpos = append(lblpos,-1)
            lblopa = append(lblopa,-1)
            lblline = append(lblline,tokline)
            lblcol = append(lblcol,tokcol)
            lblused = append(lblused,1)
            lblttidx = append(lblttidx,ttidx)
            tt[ttidx+EQ] = length(lblttidx)
        end if
end if
        return {P_LBL,0,lblidx}
    end if
end function

--function get_operand(integer permitted)
function get_operand(integer permitted, bool bSet)
--
-- Get an operand, allowed permission bits of:
--  P_REG (#01):    reg32,   (implies dword)
--                  reg8,    (implies byte)
--                  details returned: 1..8 (nb not 0..7)
--  P_MEM (#02):    [hllvar] (implies dword)    -- returns P_VAR
--                  [reg]
--                  [reg+d8]
--                  [reg+d32]
--                  [b+i*s]
--                  [b+i*s+d8]
--                  [b+i*s+d32]
--                  [i*s+b]
--                  [i*s+b+d8]
--                  [i*s+b+d32]
--                  (optionally preceded by a size)
--                  (d8/d32 may be hll constants and literal ints with
--                   simple +/- resolvable at compile-time, eg BUFF-1)
--                  details returned (PMEM): {scale,idx,base,offset}
--                      (note that idx,base are 1..8 not 0..7)
--                  details returned (P_VAR): {N,sType}
--  P_LBL (#04):    :label
--                  :%glbl                      -- returns P_GBL
--                  (opCodes and dll entry points handled elsewhere)
--                  details returned: lblidx (and a size of 0)
--  P_IMM (#08):    imm8        (may be a hll or char constant)
--                  imm16       (may be a hll const)
--                  imm32       (may be a hll const)
--                  h4          (same as "#40000000")
--                  (optionally preceded by a size)
--                  (simple +/-/* allowed, if resolveable at compile-time)
--                  details returned: atom (-#80000000..0..+#FFFFFFFF)
--  P_SEG (#40):    fs:[imm32] or fs:[reg] for tls handling
--  P_FPU (#80):    st0..st7 (etc?)
--
--  Returns a {type,size,details} triplet, where type is one of the six 
--          P_XXX values above (ie P_REG/P_MEM/P_VAR/P_LBL/P_GBL/P_IMM), 
--          size is 0/1/2/4/8, and details depends on type, as above.
--
integer size = 0
integer reg
integer N
sequence symtabN
integer vtype
atom res
integer state

    skipSpacesAndComments()
    if Ch='[' then
        return get_mem(permitted,0,bSet)
    end if
    if Ch=':' then
        if not and_bits(permitted,P_LBL) then
            Aborp("not permitted")
        end if
        getCh()
        return get_label()
    end if
    getToken()
    if toktype=LETTER then
        size = find(ttidx,SIZES)
        if size!=0 then
            if ttidx=T_tword then
                Aborp("I think you mean tbyte")
            end if
            size = SIZE[size]
            skipSpacesAndComments()
            if Ch='[' then
                return get_mem(permitted,size,bSet)
            end if
            getToken()
        else
--hmm, if (not elsif, see P_XMM!) and_bits(permitted,P_REG) then?
            reg = find(ttidx,REG32)
            if reg!=0 then
                return {P_REG,4,reg}
            end if
            reg = find(ttidx,REG16)
            if reg!=0 then
                return {P_REG,2,reg}
            end if
            reg = find(ttidx,REG8)
            if reg!=0 then
                return {P_REG,1,reg}
            end if
            if Z64=1 then
                reg = find(ttidx,R64)
                if reg!=0 then
--                  rex = ??
--                  size = 8?
                    return {P_REG,8,reg}
                end if
                reg = find(ttidx,R32)
                if reg!=0 then
--                  rex = ??
--                  size = 8?
                    return {P_REG,4,reg+8}
                end if
                reg = find(ttidx,R16)
                if reg!=0 then
--                  rex = ??
--                  size = 8?
                    return {P_REG,2,reg+8}
                end if
                reg = find(ttidx,R8l)
                if reg!=0 then
--                  rex = ??
--                  size = 8?
                    return {P_REG,1,reg+8}
                end if
                reg = find(ttidx,R8b)
                if reg!=0 then
--                  rex = ??
--                  size = 8?
                    return {P_REG,1,reg+4}
                end if
            end if
            if and_bits(permitted,P_FPU) then
                reg = find(ttidx,FREG)
                if reg!=0 then
                    return {P_FPU,0,reg}
                end if
            end if
            if and_bits(permitted,P_XMM) then
                reg = find(ttidx,XMM)
                if reg!=0 then
                    return {P_XMM,0,reg}
                end if
            end if
        end if
    end if
    if toktype=LETTER then
        if ttidx=T_h4 then
            TokN = #40000000
        elsif ttidx=T_fs then
            if not and_bits(permitted,P_SEG) then
                Aborp("not permitted")
            end if
--DEV (spotted/added in passing)
            if size!=0 and size!=4 then Abort("invalid") end if
            if Ch!=':' then
                Aborp(": expected")
            end if
            getCh()
            if Ch!='[' then
                Aborp("[ expected")
            end if
            getCh()
            getToken()
            if toktype=LETTER then
                reg = find(ttidx,REG32)
                if reg=0 and Z64=1 then
                    reg = find(ttidx,R64)
--                  rex = ??
--                  size = 8?
                end if
                if reg=0 then
                    Aborp("reg expected")
                end if
            else
                if toktype=FLOAT
                and (TokN==and_bits(TokN,#FFFFFFFF) or
                    (TokN-#100000000)==and_bits(TokN,#FFFFFFFF)) then   -- (-#1,0000,0000 maps large values #80000000..#FFFFFFFF to their -ve bit-identical equivalents) 
                    toktype = DIGIT
                end if
                if toktype!=DIGIT then
                    Aborp("reg or imm32 expected")
                end if
            end if
            if Ch!=']' then
                Aborp("] expected")
            end if
            getCh()
            if toktype=LETTER then
                return {P_SEG,4,{P_REG,T_fs,reg}}
            else
                return {P_SEG,4,{P_IMM,T_fs,TokN}}
            end if
        else
            N = InTable(InAny)
            if N<=0 then Undefined() end if
            symtabN = symtab[N]
            if N=T_routine then     -- (SUG: rename as T_routine_id?)
                if not and_bits(permitted,P_RID) then
                    Aborp("not permitted")
--NESTEDFUNC:
--              elsif ridState!=0 then
                elsif ridState>1 then
                    Aborp("ridState error")
                end if
                if Ch!='(' then
                    Aborp("( expected")
                end if
                getCh()
                getToken()
                if toktype!=LETTER then
                    Aborp("hll routine name (unquoted) expected")
                end if
                ridN = InTable(InAny)
                if ridN<=0 then Undefined() end if
                symtabN = symtab[ridN]
                if not find(symtabN[S_NTyp],{S_Type,S_Func,S_Proc}) then
                    Aborp("procedure, function, or type name expected")
                    -- (nb programmer is expected to code appropriately!)
                end if
                if Ch!=')' then
                    Aborp(") expected")
                end if
                getCh()
                ridState = 1
                ridLtot = symtabN[S_Ltot]
                state = symtabN[S_State]
                if not and_bits(state,K_ridt) then
                    symtabN = {}    -- (kill refcount)
if 01 then
                    Or_K_ridt(ridN, K_used+S_used+K_ridt)
else
--                  state = or_bits(state,K_used+K_ridt)
                    state = or_bits(state,K_used+S_used+K_ridt)
--                  state = or_bits(state,S_used+K_ridt)
--printf(1,"pilasm.e line 1011: setting S_State/K_ridt on symtab[%d]\n",ridN)
--Z_ridN = ridN
                    symtab[ridN][S_State] = state
end if
                end if
--8/11/16:
--              return {P_RID,4,ridN}
                return {P_RID,iff(Z64?8:4),ridN}
            end if
            vtype = symtabN[S_vtype]
            if vtype=T_N then
                TokN = symtabN[S_value]
                if TokN==and_bits(TokN,#FFFFFFFF)
                or (TokN-#100000000)==and_bits(TokN,#FFFFFFFF) then -- (-#1,0000,0000 maps large values #80000000..#FFFFFFFF to their -ve bit-identical equivalents) 
                    vtype = T_integer
                end if
            end if
            if vtype!=T_integer
            or symtabN[S_NTyp]!=S_Const
            or not symtabN[S_Init] then
                Aborp("illegal")
            end if
            TokN = symtabN[S_value]
        end if
        toktype = DIGIT
    elsif toktype='-' then
        getToken()
        if toktype!=DIGIT then
            Aborp("number expected")
        end if
        TokN=-TokN
    elsif toktype=SQUOTE then
        size = 1
        TokN=Tok9
        toktype = DIGIT
    elsif toktype=FLOAT
      and (TokN==and_bits(TokN,#FFFFFFFF) or
           (TokN-#100000000)==and_bits(TokN,#FFFFFFFF)) then    -- (-#1,0000,0000 maps large values #80000000..#FFFFFFFF to their -ve bit-identical equivalents) 
        toktype = DIGIT
    end if
    if not and_bits(permitted,P_IMM)
    or (toktype!=DIGIT and emitON) then
        tokcol = col
        tokline = line
        Aborp("not permitted")
    end if
    res = TokN
    if Ch='+' then
        getToken()
        getToken()
--      res += get_offset(1)
        res = get_offset(res,1)
    elsif Ch='-' then
        getToken()
        getToken()
--      res += get_offset(-1)
        res = get_offset(res,-1)
    elsif Ch='*' then
        getToken()
        getToken()
--      res *= get_offset(1)
        res = get_offset(res,0)
    end if
    return {P_IMM,size,res}
end function

procedure validate_size(integer size, atom N)
-- size is 0/1/2/4/8
    if size then
        if size=1 then
            if N<-#80 or N>#FF then
                Aborp("too big")
            end if
        elsif size=2 then
            if N<-#8000 or N>#FFFF then
                Aborp("too big")
            end if
        elsif size=4 then
            if N<-#80000000 or N>#FFFFFFFF then
                Aborp("too big")
            end if
        elsif size!=8 then
            Aborp("size mismatch")
        end if
    end if
end procedure

procedure emit_xrm_sib(integer rm, integer scale, integer idx, integer base, integer offset)
-- rm is a register, eg 0 for eax .. 7 for edi, or 
-- an octal mod byte, eg 0 for add .. 7 for cmp.
-- base and idx are however 1..8 for eax..edi, with idx=0 meaning no index
-- scale is (0|1)/2/4/8 (ie typically 0 for [base+idx] but 1 is fine too,
--                                    2 for [base+idx*2],
--                                    4 for [base+idx*4])
--                                    8 for [base+idx*8])
integer sib
integer xrm
    if not emitON then ?9/0 end if  -- sanity check

    -- more rex handling needed? (caller should and_bits(rex,??), idx|base -=8,
    --  however when rm is out of range it /may/ be a different [non-rex] bug.)
    -- Tip: start off by looking at the line no in ex.err/pglobals.e (should
    --      the following ?9/0 actually trigger, that is)
    if rm<0 or rm>7 or idx<0 or idx>8 or base<1 or base>8 then ?9/0 end if

    if idx=0 then
        xrm = rm*8+base-1           -- 0o0rb
        if offset=0                                         -- [base]
        and base!=6 then    -- not ebp (use imm8 of 0)
            s5 &= xrm
            if base=5 then -- esp
                s5 &= #24           -- 0o044
            end if
        elsif offset>=-#80 and offset<=#7F then             -- [base+d8]
            xrm += 0o100            -- 0o1rb
            s5 &= xrm
            if base=5 then -- esp
                s5 &= #24           -- 0o044
            end if
            s5 &= offset
        else                                                -- [base+d32]
            xrm += 0o200            -- 0o2rb
            s5 &= xrm
            if base=5 then -- esp
                s5 &= #24           -- 0o044
            end if
            apnds5dword(offset)
        end if
    else
--DEV erm..
if scale=8 then
        sib = 0o300+(idx-1)*0o10+(base-1)
else
        sib = floor(scale/2)*0o100+(idx-1)*0o10+(base-1)
end if
        xrm = 0o004+rm*8            -- 0o0r4
--      if offset=0 then                                    -- [b+i*s]
        if offset=0                                         -- [b+i*s]
        and base!=6 then    -- not ebp (use imm8 of 0)
            s5 &= xrm
            s5 &= sib
        elsif offset>=-#80 and offset<=#7F then             -- [b+i*s+d8]
            xrm += 0o100            -- 0o1r4
            s5 &= xrm
            s5 &= sib
            s5 &= offset
        else                                                -- [base+d32]
            xrm += 0o200            -- 0o2r4
            s5 &= xrm
            s5 &= sib
            apnds5dword(offset)
        end if
    end if
end procedure

-- as above, but for use with P_RAW:
procedure emit_ds_xrm_sib(integer rm, integer scale, integer idx, integer base, integer offset)
-- rm is a register, eg 0 for eax .. 7 for edi, or 
-- an octal mod byte, eg 0 for add .. 7 for cmp.
-- base and idx are however 1..8 for eax..edi, with idx=0 meaning no base/index
-- scale is (0|1)/2/4/8 (ie typically 0 for [base+idx] but 1 is fine too,
--                                    2 for [base+idx*2],
--                                    4 for [base+idx*4],
--                                    8 for [base+idx*8])
integer sib
integer xrm
    if not emitON then ?9/0 end if  -- sanity check
    if rm<0 or rm>7 or idx<0 or idx>8 or base<0 or base>8 then ?9/0 end if -- more rex handling needed?
    if idx=0 then
        if base=0 then
            xrm = rm*8+5                -- 0o0r5
        else
            if base=5 then -- esp
                Aborp("invalid")
            end if
            xrm = 0o200+rm*8+base-1     -- 0o2rb
        end if
        s5 &= {xrm,isData,0,0,offset}
    else
        xrm = 0o204+rm*8                -- 0o2r4
        if base=0 then Aborp("invalid") end if
--DEV erm..
if scale=8 then
        sib = 0o300+(idx-1)*0o10+(base-1)
else
        sib = floor(scale/2)*0o100+(idx-1)*0o10+(base-1)
end if
        s5 &= {xrm,sib,isData,0,0,offset}
    end if
end procedure

procedure emit_ebpN(integer rm, integer N)
-- rm is usually a reg (0..7) but it can be an instruction modifier
-- for [ebp] we must always use [ebp+d8=0]
integer offset
integer xrm
    if not emitON then ?9/0 end if
    if rm<0 or rm>7 then ?9/0 end if -- more rex handling needed?
--  offset = symtab[N][S_Tidx]*4
    offset = symtab[N][S_Tidx]
    if X64=0 then
        offset *= 4
    else
        offset *= 8
    end if
    if offset<-#80 or offset>#7F then
        xrm = #85+rm*8 -- 0o2r5             -- [ebp+d32]
        s5 &= xrm
        apnds5dword(offset)
    else
        xrm = #45+rm*8 -- 0o1r5             -- [ebp+d8]
        s5 &= xrm
        s5 &= offset
    end if
end procedure

integer jlink
procedure emit_quad(integer opcode, integer link1, integer link2, integer offset)
integer k
    if not emitON then ?9/0 end if
    s5 &= {opcode,link1,link2,offset}
    if opcode=isJmp
    or opcode=isAddr then
        k = length(s5)-2
        s5[jlink] = k
        jlink = k
    end if
end procedure

procedure emit_jmp(integer cc, integer opcode, integer link1, integer link2, integer offset)
integer xrm
    if not emitON then ?9/0 end if -- sanity check
    if cc=#10 then -- unconditional (jmp)
        s5 &= 0o351             -- jmp rel32
    else
--      s5 &= {0o017,0o200+cc}  -- jcc rel32
        xrm = 0o200+cc
        s5 &= 0o017
        s5 &= xrm
    end if
    emit_quad(opcode,link1,link2,offset)
end procedure

procedure jlabel(sequence op, integer cc)
integer p1type      -- P_XXX (1/2/4/8) as defined above
integer lblidx      -- index to lblchain..lblused
integer k
integer scale
integer idx
integer base
integer offset
--integer rex
integer xrm

--  if not emitON then ?9/0 end if -- (test) sanity check [?]   -- NO!
    p1type = op[1]
    if p1type=P_GBL then
        if emitON then
            lblidx = op[3]
            emit_jmp(cc,isJmpG,0,0,lblidx)
        end if
    elsif p1type=P_LBL then
        if emitON then
            lblidx = op[3]
            k = lblchain[lblidx]
            emit_jmp(cc,isJmp,0,0,k)
            lblchain[lblidx] = length(s5)
        end if
    elsif p1type=P_MEM then
        --/*
            Not supported:  
            -- 0o377 0o044 si5 imm32    -- jmp dword[i*s+imm32]
            --          (ebp(=5) here means "no base")
        --*/
--      {scale,idx,base,offset,rex} = op[3]
        {scale,idx,base,offset} = op[3]
        if idx!=0 then
            if base=6 then Abort("invalid; ebp(=5) means \"no base\"") end if
            if base=0 then
                ?9/0 -- should not happen. get_mem() forces a base register,
                     -- and it looks like disasm() assumes a jump table
            end if
        end if
        if cc!=#10 then
            -- only (the unconditional) jmp form exists;
            -- there is no machine opcode for jcc [mem]
            Abort("command does not support supplied operands")
        end if
        if emitON then
--          if rex then
--              s5 &= rex
--          end if
            -- 0o377 0o144 sib d8       -- jmp dword[b+i*s+d8]
            s5 &= 0o377
            emit_xrm_sib(4,scale,idx,base,offset)
        end if
    elsif p1type=P_MEM+P_LBL then
--      return {P_MEM+P_LBL,size,{scale,idx,lblidx}}
        -- 0o377 0o044 si5 addr32       -- jmp dword[i*s+:label] (for jump tables)
        if emitON then
            {scale,idx,lblidx} = op[3]
--          {scale,idx,lblidx,rex} = op[3]
--          if rex then
--              s5 &= rex
--          end if
            s5 &= 0o377
            s5 &= 0o044
--          xrm = scale*0o100+idx*0o10+5
--DEV erm..
if scale=8 then
            xrm = 0o300+(idx-1)*0o10+5
else
            xrm = floor(scale/2)*0o100+(idx-1)*0o10+5
end if
            s5 &= xrm
            k = lblchain[lblidx]
            emit_quad(isAddr,0,0,k)
            lblchain[lblidx] = length(s5)
        end if
    elsif p1type=P_VAR then
        Aborp("not permitted")
    elsif p1type=P_REG then
        if cc!=#10 then
            -- only (the unconditional) jmp form exists;
            -- there is no machine opcode for jcc reg
            Abort("command does not support supplied operands")
        end if
        if emitON then
            -- 0o377 0o34r  -- jmp reg
            xrm = 0o340+op[3]-1 -- 0o34r
            s5 &= {0o377,xrm}
        end if
    else
        ?9/0    -- should not happen
    end if
end procedure

integer fwdchain = 0
integer bklpos = -1
integer bklopa = -1

--procedure jcc(integer cc, bool bHllGoto=false)
procedure jcc(integer cc)
--object ilstuff
--integer k

    skipSpacesAndComments()
    if Ch=':' then
        jlabel(get_operand(P_LBL,false),cc)
--  elsif bHllGoto then
--      Aborp("invalid...")
    elsif Ch='[' then
        jlabel(get_mem(P_MEM+P_LBL,0,false),cc)
    elsif Ch='%' then -- opcode
--if newEmit then
--DEV (eventually, drop the whole %opXxxx syntax... [or start a new VMep])
--  if intellisense=0 then -- (temp)
        Aborp("invalid for newEmit")
--  end if
--end if
--      -- eg jz %opDivf2   -- (aka e02atdb0)
--      getCh()
--      ilstuff = il_search(0)
--      if not integer(ilstuff) then Aborp("invalid") end if
--      if emitON then
--          k = ilstuff
--          emit_jmp(cc,isOpCode,0,0,k)
--      end if
    elsif Ch='@' then
        getCh()
        if Ch='f' then 
            if emitON then
                emit_jmp(cc,isJmp,0,0,fwdchain)
                fwdchain = length(s5)
            end if
        elsif Ch='b' then
            if emitON then
                if bklpos=-1 then
                    Aborp("no prior @@:")
                end if
                emit_jmp(cc,isJmp,0,bklopa,bklpos)
            end if
        else
            tokline = line
            tokcol = col
            Expected("@f or @b")
        end if
        Ch = ' '
    elsif Ch='$' then
        getCh()
        if Ch!='_' then
            Aborp("_ expected")
        end if
        getCh()
        getToken()
        if toktype!=LETTER 
        or ttidx!=T_il then
            Aborp("$_il expected")
        end if
        if ridState=1 then
            ridState = 5
        elsif ridState=3 then
            ridState = 0
        else
            Aborp("ridState error")
        end if
        if emitON then
            emit_jmp(#10,isIL,0,0,ridN)
        end if
    else
        -- assume "jcc local" means "jcc :local"
        jlabel(get_label(),cc)
    end if
end procedure

procedure setcc(integer cc)
integer p1type      -- P_XXX (1/2/4/8) as defined above
integer p1size      -- 0/1/2/4/8 (size [qualifier] in bytes)
integer reg
integer xrm
integer sib

    {p1type,p1size,reg} = get_operand(P_REG,false)
    if emitON then
        if p1size=1 then
            -- 0o017 (0o220..0o237) 0o30r       -- eg setl r8
--          s5 &= {0o017,0o220+cc,0o300+reg-1}  -- (oops, isginfo mishap)
            xrm = 0o220+cc
            sib = 0o300+reg-1
            s5 &= {0o017,xrm,sib}
        else
            ?9/0 -- placeholder for more code, maybe
        end if
    end if
end procedure

procedure call_mem()
integer p1type
object p1details
integer scale
integer idx
integer base
integer offset
--integer rex
    {p1type,?,p1details} = get_operand(P_MEM,false)
    if p1type=P_MEM then
        --/*
            Not supported:  
            -- 0o377 0o044 si5 imm32    -- jmp dword[i*s+imm32]
            --          (ebp(=5) here means "no base")
        --*/
--      {scale,idx,base,offset,rex} = p1details
        {scale,idx,base,offset} = p1details
        if idx!=0 then
            if base=6 then Abort("invalid; ebp(=5) means \"no base\"") end if
            if base=0 then
                ?9/0 -- should not happen. get_mem() forces a base register,
                     -- and it looks like disasm() assumes a jump table
            end if
        end if
        if emitON then
--          if rex then
--              s5 &= rex
--          end if
            -- 0o377 0o124 sib d8       -- call dword[b32+i32*s+d8]
            s5 &= 0o377
            emit_xrm_sib(2,scale,idx,base,offset)
        end if
    else
        ?9/0    -- should not happen
    end if
end procedure

procedure comma()
    if Ch!=',' then
        tokline = line
        tokcol = col
        Expected("','")
    end if
    Ch = ' '
    skipSpacesAndComments()
end procedure

procedure local_label(integer illen)
--integer icol = col
    integer lblidx = il_search(':')
--?{"local_label",lblidx,ttidx,icol}
    if emitON then
        if lblidx then                                      -- update existing entry
            if lblidx<=0 or lblidx>length(lblttidx) then
                Aborp("bad lblidx (bug in phix)")
            end if
            if lblttidx[lblidx]!=ttidx then ?9/0 end if
            if lblpos[lblidx]!=-1 then
                Aborp("label already defined")
            end if
            lblpos[lblidx] = length(s5)
            if lblopa[lblidx]!=-1 then ?9/0 end if  -- internal error
            lblopa[lblidx] = illen-3
        else                                                -- start a new entry
            lblpos = append(lblpos,length(s5))
            lblopa = append(lblopa,illen-3)
            lblline = append(lblline,tokline)
            lblcol = append(lblcol,tokcol)
            lblused = append(lblused,0)
            lblchain = append(lblchain,0)
            lblttidx = append(lblttidx,ttidx)
            tt[ttidx+EQ] = length(lblttidx)
        end if -- existing/new label
    end if -- emitON
end procedure

global procedure ilASM(bool bHllGoto=false)
integer wasemitON = emitON
integer opLnpos
integer wasbind
integer illen = 0   -- opAsm locator, to fill lblopa (opAsm addr) and opAsm length
--integer jlink     -- jump link, initially illen, updated as each opJmp emitted
integer ltype       -- label type (1=local, 2=global)
integer plen        -- patch a trailing {opLn,x,opAsm,0,0,0}, and gp scratch var
integer alen        -- opAsm length (work var)
integer b           -- scratch var (byte [0..255], improves type info)
integer p1type      -- P_XXX (1/2/4/8) as defined above
integer p1size      -- 0/1/2/4/8 (size [qualifier] in bytes)
object p1details    -- (meaning of) content depends on p1type
integer p2type      -- P_XXX (1/2/4/8) as defined above
integer p2size      -- 0/1/2/4/8 (size [qualifier] in bytes)
object p2details    -- (meaning of) content depends on p2type
integer permitted
integer reg, k, xrm
integer sType, N
integer scale
integer idx
integer base
integer offset
--integer sib
integer rex               -- rex prefix (64-bit mode) (WRXB=8421)
-- rW = and_bits(rex,#08) -- 64-bit operand size
-- rR = and_bits(rex,#04) -- msb extension to modRM reg field
-- rX = and_bits(rex,#02) -- msb extension to sib index field
-- rB = and_bits(rex,#01) -- msb extension to modRM r/m field
object ilstuff  
string libname
sequence libfunc
integer libidx, fnidx
integer lblidx      -- index to lblchain..lblused
integer lstart
integer gflags
--integer cc
integer op
integer mod
-- 7/6/14:
integer guards = 0  -- 0: no [] found, [#01] any found, [#02], match found
integer unrecognised
-- 7/8/14, (successful) attempt to fix the "#ilASM{\n\n\ncode" case:
--integer kludge = 0
-- 7/8/14 replaced above with skipSpacesAndComments() before opAsm/to "end while",
--          however (eg) "#ilASM{\n\n::label\n\ncode" is still wrong... likewise
--          "@@:", [PE32] etc are also going to interfere with the linetable.
--          of course we want opLn,N,opAsm,length... not opAsm,length,opLn,N...
--          what we really need is to defer things with something like:
--integer opAsmRqd = 0      [DEV]
-- try also constant useopAsmRqd = 1/0 at the top [DEV (sorry, I'm just too busy right now, shouldn't even be typing this!)]

    if safemode then
        integer pathno = filenames[fileno][1]
        if pathno>3 then
            safemode=0
            ?9/0
        end if
    end if
--trace(1)
    bklpos = -1
    fwdchain = 0
if not bHllGoto then
    MatchString(T_ilASM)
    if not equal(toktype,'{') then Expected("'{'") end if
    skipSpacesAndComments()
end if
    Z64 = X64
    if emitON then
--skipSpacesAndComments()
--kludge = 1
-- opAsmRqd = 1      [DEV] (the ltCall probably wants to stay)
        emitline = line
        opLnpos = length(s5)+1
        if NOLT=0 or bind or lint then
            ltCall(E_vars,E_vars,opLnpos)   -- clear all, to be safe
        end if -- NOLT
        wasbind = bind
        bind = 1
        apnds5({opAsm,0,0,0}) -- (opAsm,len/x86loc,0/next,jlnk),then <len bytes>    (see pops.e)
        bind = wasbind
        illen = length(s5)
        jlink = illen
    end if
if bHllGoto then
-- if necessary...
--  if returnvar=-1 then        -- top_level code
--      Aborq("invalid...")
--  end if
    if toktype=LABEL then
        -- eg ::label
--      col -= 2
--      Ch = ':'
        local_label(illen)
    else
--      getToken()
        skipSpacesAndComments()
        sequence slop = iff(Ch=':'?get_operand(P_LBL,false):get_label())
--?1
--      if toktype=':' then ?2 MatchChar(':',false) end if
--      if toktype=':' then getCh() end if
--?{3,toktype}
--      if toktype!=LETTER then Expected("label name") end if
        if slop[1]!=P_LBL then Abort("invalid") end if
        jlabel(slop,#10)
    end if
    if emitON then
        b = length(s5)-illen
        s5[illen-2] = b                 -- fill in the opAsm length
    end if
    if returnvar=-1 then        -- top_level code
        label_fixup()           -- (also called at end of DoRoutineDef)
    end if
    emitON = wasemitON
    return
end if

    while 1 do
--if kludge=0 then
--      skipSpacesAndComments()
--end if
--kludge = 0
        if Ch=':' then
            getCh()
            tokline = line
            tokcol = col
--          ltype = find(Ch,":%")
            ltype = find(Ch,":%>!<")
            if ltype=0 then
                Aborq("define a new label using \"::\" (or :%/:>/:! for global/init/bang labels)")
--              Aborq("define a new label using \"::\" (or :%/:>/:!/:< for global/init/bang/stop labels)")
            end if
            -- add a new label
--          tokline = line
--          tokcol = col
            if ltype=1 then -- (local label)
                getCh()
                local_label(illen)
--/*
                lblidx = il_search(':')
                if emitON then
                    if lblidx then                                      -- update existing entry
                        if lblidx<=0 or lblidx>length(lblttidx) then
                            Aborp("bad lblidx (bug in phix)")
                        end if
                        if lblttidx[lblidx]!=ttidx then ?9/0 end if
                        if lblpos[lblidx]!=-1 then
                            Aborp("label already defined")
                        end if
                        lblpos[lblidx] = length(s5)
                        if lblopa[lblidx]!=-1 then ?9/0 end if  -- internal error
                        lblopa[lblidx] = illen-3
                    else                                                -- start a new entry
                        lblpos = append(lblpos,length(s5))
                        lblopa = append(lblopa,illen-3)
                        lblline = append(lblline,tokline)
                        lblcol = append(lblcol,tokcol)
                        lblused = append(lblused,0)
                        lblchain = append(lblchain,0)
                        lblttidx = append(lblttidx,ttidx)
                        tt[ttidx+EQ] = length(lblttidx)
                    end if -- existing/new label
                end if -- emitON
--*/
            else -- ltype=2 (global label), 3 (init), 4 (bang), 5 (exch)
                lstart = col
                lblidx = il_search(':')
--?? = text[lstart..col-1]
                if returnvar!=-1 then Aborp("global labels may not be defined inside routines") end if
                if emitON then
                    k = find(ttidx,aatidx)
                    if k!=0 then
                        k = aasydx[k]
--                      if and_bits(symtab[k][S_State],S_fwd) then
                        if k!=0 and and_bits(symtab[k][S_State],S_fwd) then
                            symtab[k][S_State] -= S_fwd
                        end if
                    end if
                    gflags = G_declared
                    if ltype=3 then         gflags += G_init
                    elsif ltype=4 then      gflags += G_bang
--                  elsif ltype=5 then      gflags += G_stop
                    elsif ltype=5 then      gflags += G_exch
                    end if
                    if lblidx then
--DEV (it might be better to do ltype=5 completely outside glbxx tables)
                        if lblidx<=0 or lblidx>length(glblused) or ltype=5 then
                            Aborp("bad lblidx (bug in phix)")
                        end if
                        if and_bits(glblused[lblidx],G_declared) then
                            Aborp("label already defined")
                        end if
--                      glblused[lblidx] += G_declared
--                      glblused[lblidx] += gflags
                        glblused[lblidx] = or_bits(glblused[lblidx],gflags)
                        glblabel[lblidx] = currRtn
                        glblname[lblidx] = text[lstart..col-1]
                    else
--                      glblused = append(glblused,G_declared)
                        glblused = append(glblused,gflags)
                        glboffset = append(glboffset,0)
--!                     glbttidx = append(glboffset,ttidx)
--                      glbttidx = append(glbttidx,ttidx)
                        glblabel = append(glblabel,currRtn)
                        glblline = append(glblline,tokline)
                        glblcol = append(glblcol,tokcol)
                        glblname = append(glblname,text[lstart..col-1])
                        lblidx = length(glblused)
                        tt[ttidx+EQ] = lblidx
                        if ltype=5 then
                            --DEV 
                            if gexch!=0 or PE=0 or Z64=0 then
                                Aborp("error")
                            end if
                            gexch = lblidx
                        end if
--DEV should there be the inverse of:
--          agcheck[tt](ttidx)
--  ie we are processing the expected file, and/or mark that file as already included (if manually included?)
                    end if
-- DEV (temp)
--k = find(ttidx,aatidx)
--if k=3 then
--DEV 7/8/14 as per "kludge" above, we may benefit from skipSpacesAndComments() here rather than at "end while".
--opAsmRqd = 1    [DEV]
                    plen = length(s5)
                    alen = plen-illen
                    s5[illen-2] = alen
                    s5 &= {opLogPos,lblidx}
                    wasbind = bind
                    bind = 1        -- prohibit opLnt/p/pt mid-#ilasm, since all regs trashed.
                                    -- (if needbe, hard-code them, wrapped by pushad/popad)
                    emitline = line -- however, maintain LineTab (for list.asm and diag reports)
                    opLnpos = length(s5)+1
                    apnds5({opAsm,0,0,0})
                    illen = length(s5)
                    jlink = illen
                    bind = wasbind
                end if -- emitON
            end if
            if Ch='}' then exit end if
        elsif Ch='@' then
            getCh()
            if Ch!='@' then Aborq("unrecognised") end if
            getCh()
            if Ch!=':' then Aborq("unrecognised") end if
            getCh()
            if emitON then
                bklpos = length(s5)
-- +[2+]opAsmRqd*4?   [DEV]
                bklopa = illen-3
                while fwdchain!=0 do
                    k = s5[fwdchain]
                    s5[fwdchain-1] = bklopa     -- link to containing opAsm
                    s5[fwdchain] = bklpos       -- il offset of label definition
                    fwdchain = k
                end while
            end if
        elsif Ch='[' then
--          if not newEmit then Aborq("needs preceding format statement...") end if --DEV (temp?)
-- added 4/5/14:
            if emitON then
                b = length(s5)-illen
                s5[illen-2] = b                 -- fill in the opAsm length
            end if
            emitON = 0
            guards = or_bits(guards,#01)
            getCh()
            if Ch=']' then
                -- [] is shorthand for [PE32,PE64,ELF32,ELF64]
                emitON = wasemitON
--DEV I think this should be... (19/11/14) [undone w/out ever testing, use X64 to control binary output (when emitON=true!), Z64 to control parsing...][DEV]
                Z64 = 0
--              Z64 = X64
            else
                while 1 do
                    getToken()
                    unrecognised = 0
                    if toktype=DIGIT then
                        -- [32] is shorthand for [PE32,ELF32]
                        -- [64] is shorthand for [PE64,ELF64]
                        -- (obviously there are no equivalent hll compiler directives,
                        --  while theoretically "format PE32,ELF32" could be taken to
                        --  mean "create two executables", the additional complexity
                        --  that would introduce does not bear thinking about..)
                        if TokN=32 then
                            if X64=0 and wasemitON then
                                emitON = 1
                            end if
                            Z64 = 0
                        elsif TokN=64 then
                            if X64=1 and wasemitON then
                                emitON = 1
                            end if
                            Z64 = 1
                        else
                            unrecognised = 1
                        end if
                    elsif toktype=LETTER then
--DEV/SUG: (**NOT IMPLEMENTED**) [and I'm not at all sure about Z64...]
--                      -- [PE] is shorthand for [PE32,PE64]
--                      -- [ELF] is shorthand for [ELF32,ELF64]
--                      if ttidx=T_PE then
--                          if PE=1 and wasemitON then
--                              emitON = 1
--                          end if
--                      elsif ttidx=T_ELF then
--                          if PE=0 and wasemitON then
--                              emitON = 1
--                          end if
--                      els
                        if ttidx=T_PE32 then
                            if PE=1 and X64=0 and wasemitON then
                                emitON = 1
                            end if
                            Z64 = 0
                        elsif ttidx=T_PE64 then
                            if PE=1 and X64=1 and wasemitON then
                                emitON = 1
                            end if
                            Z64 = 1
                        elsif ttidx=T_ELF32 then
                            if PE=0 and X64=0 and wasemitON then
                                emitON = 1
                            end if
                            Z64 = 0
                        elsif ttidx=T_ELF64 then
                            if PE=0 and X64=1 and wasemitON then
                                emitON = 1
                            end if
                            Z64 = 1
                        else
                            unrecognised = 1
                        end if
                    else
                        unrecognised = 1
                    end if
                    if unrecognised then
                        Aborp("unrecognised")
                    end if
                    if Ch!=',' then exit end if
                    getCh()
                end while
            end if
            if Ch!=']' then Aborp("']' expected") end if
            if emitON then
                guards = or_bits(guards,#02)
            end if
            getCh()
        else -- (Ch!=':' and Ch!='@' and Ch!='[')
            if Ch='}' then exit end if
            if Ch=-1 then Aborp("'}' expected") end if
            if emitON and line!=emitline then
--7/8/14 if opAsmRqd then ...?    [DEV]
--      (I think this is the fundamental error, backpatching an empty opLn/opAsm *after* the linetab has been messed up!)
                plen = length(s5)
                alen = plen-illen
                if alen then
--              if alen or opAsmRqd [and illen!=-1?] then
                    s5[illen-2] = alen
                    wasbind = bind
                    bind = 1        -- prohibit opLnt/p/pt mid-#ilasm, since all regs trashed.
                                    -- (if needbe, hard-code them, wrapped by pushad/popad)
                    emitline = line -- however, maintain LineTab (for list.asm and diag reports)
                    opLnpos = length(s5)+1
                    apnds5({opAsm,0,0,0})
                    illen = length(s5)
                    jlink = illen
                    bind = wasbind
                elsif opLnpos=plen-5 and s5[opLnpos]=opLn then
--7/8/14 (opAsmRqd) we could try putting a warning msg here.
                    -- patch a trailing opLn,x,opAsm,0,0,0:
                    emitline = line
                    s5[plen-4] = emitline
                end if
            end if -- emitON and line!=emitline
            getToken()
--          skipSpacesAndComments() -- No!
            if toktype!=LETTER then
                Aborp("unrecognised")
            end if
            if ttidx=T_e_all then
                if emitON then
                    SideEffects = E_all
                end if
            elsif ttidx=T_lea then
--if emitON then trace(1) end if
                {p1type,p1size,p1details} = get_operand(P_REG,false)
                rex = 0
                if p1size=8 then
                    if Z64!=1 then ?9/0 end if
                    rex = #48
                end if
                reg = p1details-1
                if reg>7 then
--                  rex = #49
--                  rex = or_bits(rex,#41)
                    rex = or_bits(rex,#44)
                    reg -= 8
                end if
                if p1type!=P_REG then ?9/0 end if -- sanity check
--              if p1size!=4 then
                if p1size!=4 
--              and not (Z64=1 and p1size=8) then
                and p1size!=8 then
                    Aborp("invalid")
                end if
                comma()
                {p2type,p2size,p2details} = get_operand(P_MLS,true)
                if emitON then
                    if p2type=P_MEM 
                    or p2type=P_RAW then
                        {scale,idx,base,offset} = p2details
                        if idx>8 then
                            rex = or_bits(rex,#42)
                            idx -= 8
                        end if
                        if base>8 then
                            rex = or_bits(rex,#41)
                            base -= 8
                        end if
                    end if
                    if rex then
                        s5 &= rex
                    end if
                    if p2type=P_MEM then
--                      {scale,idx,base,offset} = p2details
                        s5 &= 0o215     -- lea
                        -- 0o215 0o1rb d8       -- lea reg,[base+d8]
                        -- 0o215 0o2rb d32      -- lea reg,[base+d32]
                        -- 0o215 0o1r4 0o044 i8 -- lea reg,[esp+d8]
                        -- 0o215 0o1r4 sib d8   -- lea reg,[b+i*s+d8]
                        emit_xrm_sib(reg,scale,idx,base,offset)
                    elsif p2type=P_RAW then
--                      {scale,idx,base,offset} = p2details
                        s5 &= 0o215     -- lea
--00409959   #8D #05     56341200   LEA EAX,DWORD PTR DS:[123456]
--0040995F   #8D #80     56341200   LEA EAX,DWORD PTR DS:[EAX+123456]
--00409965   #8D #84 #91 56341200   LEA EAX,DWORD PTR DS:[ECX+EDX*4+123456]
--00409959   0o215 0o005       56341200 LEA EAX,DWORD PTR DS:[123456]
--0040995F   0o215 0o200       56341200 LEA EAX,DWORD PTR DS:[EAX+123456]
--00409965   0o215 0o204 0o221 56341200 LEA EAX,DWORD PTR DS:[ECX+EDX*4+123456]
                        emit_ds_xrm_sib(reg,scale,idx,base,offset)
                    elsif p2type=P_LBL then
                        ?9/0 -- should never trigger (lea eax,:lbl? or use mov?)
                    elsif p2type=P_VAR then
                        {N,sType} = p2details
-- 10/09/2013:
--                      if sType=S_GVar2 then -- or Const?
--                      rex = 0
--if newEmit and X64=1 then
--                      s5 &= #48 --rex [DEV]
--?9/0 -- test this properly... (done)
--printf(1,"test plea pilasm.e line 2548, (lea reg,[P_VAR]) tokline is %d\n",{tokline})
--                                  -- (#48:) 0o307 0o30r imm32     -- mov r32,imm32
--                                  s5 &= 0o307
--                                  xrm = 0o300+reg
--                                  s5 &= xrm
--                                  apnds5dword(p2details)          -- imm32
--end if
                        if sType<=S_GVar2 then
if X64=1 then
                            xrm = 0o005+reg*8       -- 0o0r5
                            s5 &= {0o215,xrm,isVar,0,0,N}   -- lea reg,[imm32]
else
                            xrm = 0o270+reg     -- 0o27r    -- mov reg,imm32
                            s5 &= {xrm,isVar,0,0,N}
end if
                        elsif sType=S_TVar then
                            k = symtab[N][S_Tidx]
                            if k=0 then                             -- mov reg,ebp
                                s5 &= 0o213                         -- mov_dword
                                xrm = 0o305 + reg*8 -- 0o3r5
                                s5 &= xrm
                            else
                                s5 &= 0o215                         -- lea
                                emit_ebpN(reg,N)
                            end if
                        else
                            ?9/0 -- sanity check (should never trigger)
                        end if
                    elsif p2type=P_SEG then
                        if p2size!=4 then ?9/0 end if
                        if rex then ?9/0 end if
--                          s5 &= rex
--                      end if
                        {p2type,k,offset} = p2details
-- in case we ever need any others: (for now only FS is supported)
--       sops={#26,  #2E,  #36,  #3E,  #64,  #65},
--       sopt={"ES:","CS:","SS:","DS:","FS:","GS:"},
                        if k!=T_fs then ?9/0 end if
                        -- 0o144 -- segment override prefix (FS)
                        s5 &= 0o144
                        if p2type=P_IMM then
--004099E6   64:8D05 18000000      LEA EAX,DWORD PTR FS:[18]
--#8D = 0o215
--#05 = 0o005
                            xrm = 0o005+reg*8       -- 0o0r5
                            s5 &= {0o215,xrm}   -- lea reg,[imm32]
                            apnds5dword(offset)
                        elsif p2type=P_REG then
--004099E6   64:8D00               LEA EAX,DWORD PTR FS:[EAX]
                            base = offset-1
                            -- 0o215 0o1rb d8       -- lea reg,[base+d8]
                            if base=5 then ?9/0 end if
                            xrm = base+reg*8        -- 0o0rb
                            s5 &= {0o215,xrm}   -- lea reg,[reg]
--                          ?9/0
                        else
                            ?9/0
                        end if
                    else
                        ?9/0 -- sanity check (should never trigger)
                    end if
                end if
            elsif ttidx=T_mov
               or ttidx=T_add
               or ttidx=T_or
               or ttidx=T_adc
               or ttidx=T_sbb
               or ttidx=T_and
               or ttidx=T_sub
               or ttidx=T_xor
               or ttidx=T_cmp
               or ttidx=T_test then
--if fileno=1 then
--  if emitON then trace(1) end if
--end if
                op = ttidx
                mod = find(ttidx,{T_add,T_or,T_adc,T_sbb,T_and,T_sub,T_xor,T_cmp})-1
                {p1type,p1size,p1details} = get_operand(P_RM,true)
                comma()
                if and_bits(p1type,P_MV) then
                    permitted = P_RLI
                elsif p1type=P_REG then
                    permitted = P_RMLIS
                else
                    permitted = P_RMLI
                end if
                if op=T_mov
                and p1type=P_REG
                and ((Z64=0 and p1size=4) or
                     (Z64=1 and p1size=8))
                and p1details=3 then    -- e/rdx
                    permitted += P_RID
                end if
                if Ch='$'
                and op=T_mov
                and p1type=P_REG
--              and p1size=4        --DEV X64 and size=8?
                and ((Z64=0 and p1size=4) or
                     (Z64=1 and p1size=8))
                and p1details=2 then    -- e/rcx
                    getCh()
                    if Ch!='_' then
                        Aborp("_ expected")
                    end if
                    getCh()
                    getToken()
                    if toktype!=LETTER 
                    or ttidx!=T_Ltot then
                        Aborp("$_Ltot expected")
                    end if
                    if ridState=1 then
                        ridState = 3
                    elsif ridState=5 then
                        ridState = 0
                    else
                        Aborp("ridState error")
                    end if
--                  {p2type,p2size,p2details} = {P_IMM,4,ridLtot}
                    {p2type,p2size,p2details} = {P_IMM,p1size,ridLtot}
                else
                    {p2type,p2size,p2details} = get_operand(permitted,false)
                end if


                if op=T_test then
                    if p1size and p2size and p1size!=p2size then
                        Aborp("incompatible sizes")
                    end if
                end if
                p1size = or_bits(p1size,p2size)
                if p1size=0 then
                    if find(p2type,{P_LBL,P_GBL}) then
                        p1size = iff(Z64=1?8:4)
                    else
                        Aborp("size qualifier rqd")
                    end if
                end if
                if emitON then
                    if p1type=P_REG then
--if p2type=P_LBL then trace(1) end if
--DEV: may want moving outside p1type tests, or duplicating on (some of) the other branches.
                        rex = 0
--10/11/16:
--                      if p1size=8 then
                        if p1size=8
                        or p1size=9 then
                            if Z64!=1 then ?9/0 end if
--if p2type!=P_LBL then
--if p2type=P_LBL then trace(1) end if
                            rex = #48
--end if
--sug:                      p1size = 4
                        end if
                        reg = p1details-1
                        if reg>7 then
                            if Z64!=1 then ?9/0 end if
-- help, this is getting desperate!
--11/11/16:
--if p2type=P_IMM then
if p2type=P_IMM
or p2type=P_LBL
or p2type=P_GBL then
                            rex = or_bits(rex,#41)
else
                            rex = or_bits(rex,#44)
end if
--                          rex = or_bits(rex,#41)
                            reg -= 8
                        end if
--added 15/8/14:
if p2type=P_REG then
--  if (p2details-1)>7 then
    if p2details>8 then
        if Z64!=1 then ?9/0 end if
-- 31/7/17 (over sub cl,r8l)
if p1size=1 then
        rex = or_bits(rex,#44)
else
        rex = or_bits(rex,#41)
end if
        p2details -= 8
    end if
elsif p2type=P_MEM then
    {scale,idx,base,offset} = p2details
    if idx>8 then
        rex = or_bits(rex,#42)
        idx -= 8
    end if
    if base>8 then
        rex = or_bits(rex,#41)
        base -= 8
    end if
end if
                        if rex then
                            s5 &= rex
                        end if
                        if p2type=P_REG then
                            if op=T_mov
                            or op=T_test then
                                if op=T_mov then
                                    if p1size=4 
                                    or p1size=8 then
                                        -- 0o213 3rm    -- mov reg,reg
                                        s5 &= 0o213
                                    elsif p1size=1 then
                                        -- 0o212 3rm    -- mov r8,r8
                                        s5 &= 0o212
                                    elsif p1size=2 then
                                        s5 &= 0o146     -- <word prefix>
                                        s5 &= 0o213
                                    else
                                        Aborp("size?")
--                                      ?9/0
                                    end if
                                    xrm = 0o300+reg*8+p2details-1   -- 0o3rm
                                elsif op=T_test then
--                                  if p1size!=4 then ?9/0 end if
                                    if p1size!=4 and p1size!=8 then ?9/0 end if
                                    -- 0o205 3rm    -- test reg,reg
                                    s5 &= 0o205
--                                  xrm = 0o300+reg*8+p2details-1   -- 0o3rm
                                    xrm = 0o300+(p2details-1)*8+reg -- 0o3rm
                                else
                                    ?9/0 -- sanity check (should never trigger)
                                end if
                            else
                                if p1size!=4
                                and p1size!=1
                                and p1size!=2
                                and p1size!=8 then
                                    Aborp("not yet supported?")
--                                  ?9/0
                                end if
                                if mod=-1 then ?9/0 end if -- sanity check
                                -- 0o001 3rm    -- add reg,reg
                                -- 0o011 3rm    -- or reg,reg
                                -- 0o021 3rm    -- adc reg,reg
                                -- 0o031 3rm    -- sbb reg,reg
                                -- 0o041 3rm    -- and reg,reg
                                -- 0o051 3rm    -- sub reg,reg
                                -- 0o061 3rm    -- xor reg,reg
                                -- 0o071 3rm    -- cmp reg,reg  [note: cmp ecx,edx is either 0o071 0o321 or 0o073 0o312]
                                if p1size=1 then
                                    xrm = 0o000
                                else
                                    if p1size=2 then
                                        s5 &=#66
                                    end if
                                    xrm = 0o001
                                end if
-- help me!! (I may have been panicing pointlessly; these are almost certainly equivalent instructions)
if rex!=0 and rex!=#48 then
--if rex!=0 and rex!=#48 and rex!=#41 then
--                              if p1size!=8 then ?9/0 end if
                                if p1size!=8 and p1size!=1 then ?9/0 end if
--                              if p1size!=8 then ?"9/0 line 2174 pilasm.e" end if
if p1size=1 then
                                xrm = 0o000 + mod*8 -- 0o0m0 (where 0 is instruction modifier)
                                s5 &= xrm
                                xrm = 0o300+(p2details-1)*8+reg -- 0o3rm
else
                                xrm = 0o003 + mod*8 -- 0o0m3 (where m is instruction modifier)
                                s5 &= xrm
                                xrm = 0o300+reg*8+p2details-1   -- 0o3rm
end if
--#44 0o050 0o301 sub   cl,r8l 
--; 132             sub     cl,r8l 
--                  sub ecx,r8            ;#0044BE17: 41:053310                  uv 02 102  1 185 02   
--; 132             sub     cl,r8l 
--                  sub ecx,r8            ;#0044BE17: 41:053310                  uv 02 102  1 185 02   
--; 132             sub     cl,r8l 
--                  sub r9l,al            ;#0044BE17: 41:050301                  vu 200 201  1 184      
--; 132             sub     cl,r8l 
--                  sub al,r9l            ;#0044BE17: 44:050310                  vu 01 201  1 184      
--; 130             sub cl,r8l 
--                  sub cl,r8l            ;#0044BE15: 44:050301                  uv 02 102  1 184 02   
else
--                              xrm = 0o001 + mod*8 -- 0o0m1 (where m is instruction modifier)
                                xrm += mod*8 -- 0o0m1 (where m is instruction modifier)
                                s5 &= xrm
                                xrm = 0o300+(p2details-1)*8+reg -- 0o3rm
end if
                            end if
                            s5 &= xrm
                        elsif p2type=P_MEM then
--                          {scale,idx,base,offset} = p2details -- done above
                            if p1size=2 then
                                s5 &= 0o146                                         -- word_prefix
                            end if
-- 27/8/14: (no!)
--if 0 and op=T_mov and reg=0 and scale=0 and idx=0 and base=5 then
--  if p1size=1 then ?9/0 end if -- just use 0o242 instead of 0o241, I think...
--  s5 &= 0o241
--  apnds5dword(offset)
--else

                            if p1size=2 
                            or p1size=4 
--DEV added 28/12/15:
                            or p2size=4 
                            or p1size=8 then
--DEV shorter form for eax/ax (#A1/A3?) (0o241)
                                if op=T_mov then
                                    -- 0o213 0o0rb              -- mov r32,[base]
                                    -- 0o213 0o0r4 0o044        -- mov r32,[esp]
                                    -- 0o213 0o1rb d8           -- mov r32,[base+d8]
                                    -- 0o213 0o2rb d32          -- mov r32,[base+d32]
                                    -- 0o213 0o2r4 0o044 d32    -- mov r32,[esp+d32]
                                    -- 0o213 0o0r4 sib          -- mov r32,[b+i*s]
                                    -- 0o213 0o1r4 sib d8       -- mov r32,[b+i*s+d8]
                                    -- 0o213 0o2r4 sib d32      -- mov r32,[b+i*s+d32]
                                    s5 &= 0o213
                                elsif mod!=-1 then
                                    -- 0o073 0o0rb              -- cmp r32,dword[base]
                                    -- 0o073 0o1rb d8           -- cmp r32,dword[base+d8]
                                    -- 0o003 0o1r4 sib d8       -- add r32,dword[b+i*s+d8]
                                    -- 0o013 0o1r4 sib d8       --  or r32,dword[b+i*s+d8]
                                    -- 0o023 0o1r4 sib d8       -- adc r32,dword[b+i*s+d8]
                                    -- 0o033 0o1r4 sib d8       -- sbb r32,dword[b+i*s+d8]
                                    -- 0o043 0o1r4 sib d8       -- and r32,dword[b+i*s+d8]
                                    -- 0o053 0o1r4 sib d8       -- sub r32,dword[b+i*s+d8]
                                    -- 0o063 0o1r4 sib d8       -- xor r32,dword[b+i*s+d8]
                                    -- 0o073 0o1r4 sib d8       -- cmp r32,dword[b+i*s+d8]
                                    xrm = 0o003+mod*8 -- 0o0m3 (where m is instruction modifier)
                                    s5 &= xrm
                                else
                                    ?9/0 -- placeholder for more code
                                end if
                            elsif p1size=1 then
                                if op=T_mov then
                                    -- 0o212 0o0r4 sib          -- mov r8,[b+i*s]
                                    -- 0o212 0o1r4 sib d8       -- mov r8,[b+i*s+d8]
                                    s5 &= 0o212
                                elsif mod!=-1 then
                                    -- 0o052 0o1r4 sib d8       -- sub r8,byte[b+i*s+d8]
                                    xrm = 0o002+mod*8 -- 0o0m2 (where m is instruction modifier)
                                    s5 &= xrm
                                else
                                    ?9/0 -- placeholder for more code
                                end if
                            else
                                ?9/0 -- placeholder for more code (16bit)
                            end if
                            emit_xrm_sib(reg,scale,idx,base,offset)
--end if
                        elsif p2type=P_RAW then
                            {scale,idx,base,offset} = p2details
--6/11/16: (wrong one, already done)
--                          if p1size=8 then
--                              s5 &= #48
--                          end if
                            s5 &= 0o213
                            emit_ds_xrm_sib(reg,scale,idx,base,offset)
                        elsif p2type=P_LBL then
                            if Z64=1 then
                                if p1size!=8 then Aborp("invalid operand size") end if
--                              s5 &= #48
                                    -- (#48:) 0o307 0o30r imm32     -- mov r32,imm32
--?9/0
--                                  s5 &= 0o307
--                                  xrm = 0o300+reg
--                                  s5 &= xrm
--                                  apnds5dword(p2details)          -- imm32
                            else
                                if p1size!=4 then Aborp("invalid operand size") end if
                            end if
                            if op=T_mov then
                                if p1size=8 then    -- (may need to be Z64=1)
                                    s5 &= 0o307
                                    xrm = 0o300+reg
                                else
                                    xrm = 0o270+reg     -- 0o27r    -- mov reg,imm32
                                end if
                                lblidx = p2details
                                k = lblchain[lblidx]
                                s5 &= xrm
                                emit_quad(isAddr,0,0,k)
                                lblchain[lblidx] = length(s5)
                            else
                                ?9/0
                            end if
                        elsif p2type=P_GBL then
--                          if Z64=1 then
--                              if p1size!=8 then Aborp("invalid operand size") end if
--                              s5 &= #48
----?9/0 --(as above, test me)
--                          else
--                              if p1size!=4 then Aborp("invalid operand size") end if
--                          end if
                            if p1size!=4 then
                                if Z64=1 and p1size=8 then
--                                  s5 &= #48
                                else
                                    Aborp("invalid operand size")
                                end if
                            end if
                            if op=T_mov then
                                if p1size=8 then    -- (may need to be Z64=1)
                                    s5 &= 0o307
                                    xrm = 0o300+reg
                                else
                                    xrm = 0o270+reg     -- 0o27r    -- mov reg,imm32
                                end if
                                lblidx = p2details
--DEV fatal gibberish error...
--                              emit_quad(isAddr,0,0,k)
--                              emit_quad(isAddr,0,0,lblidx)
--                              s5 &= {xrm,isJmpG,0,0,lblidx}
                                s5 &= {xrm,isGaddr,0,0,lblidx}
                            elsif op=T_cmp then
                                if reg=0 then -- eax
                                    -- 0o075 imm32  -- cmp eax,imm32
--if X64=1 then
--  printf(1,"pilasm.e line 2878: please check list.asm (tokline=%d) for cmp eax,imm32\n",{tokline})
--end if
                                    s5 &= {0o075}
                                else
                                    -- 0o201 0o37r imm32    -- cmp reg,imm32
                                    xrm = 0o300+mod*8+reg -- 0o3mr
                                    s5 &= {0o201,xrm}
                                end if
                                lblidx = p2details
                                s5 &= {isGaddr,0,0,lblidx}
                            else
                                ?9/0
                            end if
                        elsif p2type=P_IMM then
                            if op=T_mov then
                                if p1size=8 then
if p2details=#40000000 then
                                    xrm = 0o270+reg
                                    s5 &= xrm
                                    apnds5dword(0)                  -- imm64 in two attempts
                                    apnds5dword(p2details)
else
                                    -- (#48:) 0o307 0o30r imm32     -- mov r32,imm32
                                    s5 &= 0o307
                                    xrm = 0o300+reg
                                    s5 &= xrm
                                    apnds5dword(p2details)          -- imm32
end if
                                elsif p1size=4
                                   or (p1size=5 and p2size=1) then
                                    -- 0o27r imm32                  -- mov r32,imm32
                                    xrm = 0o270+reg
                                    s5 &= xrm
                                    apnds5dword(p2details)          -- imm32
                                elsif p1size=2 then
                                    s5 &= 0o146     -- <word prefix>
                                    -- 0o27r imm32                  -- mov r32,imm32
                                    xrm = 0o270+reg
                                    s5 &= xrm
                                    apnds5word(p2details)           -- imm16
                                elsif p1size=1 then
                                    -- 0o26r imm8                   -- mov reg8,imm8
                                    xrm = 0o260+reg
                                    s5 &= xrm
                                    s5 &= and_bits(p2details,#FF)   -- imm8
                                else
                                    ?9/0    -- placeholder for more code (16-bit)
                                end if
                            elsif mod!=-1 -- add(0)|or(1)..cmp(7)
                               or op=T_test then
--trace(1)
                                -- Note: and_edx,#FF uses a dword (as it is greater than #7F), whereas
                                --       and_edx,byte #FF uses the byte format (equiv to #FFFFFFFF).
                                --       It makes a "best guess" based on the value, but to be sure you
                                --       should always explicitly specify byte/dword, if it matters any.
--DEV/NOTE re the following change and the above: add/sub care deeply about this, eg/ie add eax,byte #82 
--          is eax-=126 whereas add eax,dword#82 is eax+=130 (cmiiw), however cmp/or/and/xor don't give
--          a monkey about such things, **//and that fact is missing from the tests below//** [BUG].
--          At some point (and hopefully soon) these tests should be given a complete rewrite.
-- 4/9/14:
if p1size=8 then
    if p2details=#40000000 then
        Aborp("invalid operand size")
    end if
end if
                                if p1size!=1
                                and (op=T_test  -- (DEV and p1size=4, surely?)
--                              if op=T_test    -- (DEV and p1size=4, surely?)
                                or p2details<-#80
                                or p2size=4
                                or (p2size=1 and p2details>#FF)
                                or (p2size!=1 and p2details>#7F)) then
--                              or (p2size!=1 and p2size!=0 and p2details>#7F) then
                                    if p1size=2 then
                                        s5 &= 0o146     -- <word prefix>
                                    end if
--                                  if reg=0 then -- eax
                                    if reg=0 and X64=0 then -- eax
--if X64=1 then
--  printf(1,"pilasm.e line 2956: please check list.asm (tokline=%d) for cmp[etc] eax,imm32\n",{tokline})
--end if
                                        if mod!=-1 then
                                            -- 0o005 imm32  -- add eax,imm32
                                            -- 0o015 imm32  -- or eax,imm32
                                            -- 0o025 imm32  -- adc eax,imm32
                                            -- 0o035 imm32  -- sbb eax,imm32
                                            -- 0o045 imm32  -- and eax,imm32
                                            -- 0o055 imm32  -- sub eax,imm32
                                            -- 0o065 imm32  -- xor eax,imm32
                                            -- 0o075 imm32  -- cmp eax,imm32
                                            xrm = 0o005+mod*8 -- 0o0m5 (where m is instruction modifier)
                                            s5 &= xrm       -- <op> eax,imm32
                                        elsif op=T_test then
                                            -- 0o251 imm32  -- test eax,imm32
                                            s5 &= 0o251
                                        else
                                            ?9/0 -- sanity check (should never trigger)
                                        end if
                                    elsif mod!=-1 then
                                        -- 0o201 0o30r imm32    -- add reg,imm32
                                        -- 0o201 0o31r imm32    -- or reg,imm32
                                        -- 0o201 0o32r imm32    -- adc reg,imm32
                                        -- 0o201 0o33r imm32    -- sbb reg,imm32
                                        -- 0o201 0o34r imm32    -- and reg,imm32
                                        -- 0o201 0o35r imm32    -- sub reg,imm32
                                        -- 0o201 0o36r imm32    -- xor reg,imm32
                                        -- 0o201 0o37r imm32    -- cmp reg,imm32
                                        xrm = 0o300+mod*8+reg -- 0o3mr
                                        s5 &= {0o201,xrm}
                                    elsif op=T_test then
                                        -- 0o367 0o30r imm32    -- test reg,imm32
                                        xrm = 0o300+reg
                                        s5 &= {0o367,xrm}
                                    else
                                        ?9/0 -- sanity check (should never trigger)
                                    end if
                                    if p1size=2 then
                                        validate_size(2,p2details)
                                        apnds5word(p2details)           -- imm16
                                    else
                                        apnds5dword(p2details)          -- imm32
                                    end if
                                elsif op=T_test then
                                    if p1size!=1 then Aborp("oops!?") end if
                                    if reg=0 then
--if X64=1 then
--  printf(1,"pilasm.e line 3003: please check list.asm (tokline=%d) for test eax,imm8\n",{tokline})    -- (seems fine as-is)
--end if
                                        s5 &= 0o250
                                    else
                                        s5 &= 0o366
                                        xrm = 0o300+reg
                                        s5 &= xrm
                                    end if
                                    validate_size(1,p2details)
                                    s5 &= and_bits(p2details,#FF)                   -- imm8
                                else
--                                  if mod=-1 then ?9/0 end if
                                    if mod=-1 then Aborp("oops!?") end if
                                    if p1size = 2 then
                                        s5 &= #66
                                    end if
                                    if p1size=2
                                    or p1size=4
                                    or p1size=5 
                                    or p1size=8
                                    or p1size=9 then
--if p1size=9 then
--  printf(1,"pilasm.e line 3000: p1size=9: please check list.asm (tokline=%d) for cmp[etc] reg,imm8\n",{tokline}) (done)
--end if
                                        -- 0o203 0o30r imm8 -- add reg,imm8
                                        -- 0o203 0o31r imm8 -- or reg,imm8
                                        -- 0o203 0o32r imm8 -- adc reg,imm8
                                        -- 0o203 0o33r imm8 -- sbb reg,imm8
                                        -- 0o203 0o34r imm8 -- and reg,imm8
                                        -- 0o203 0o35r imm8 -- sub reg,imm8
                                        -- 0o203 0o36r imm8 -- xor reg,imm8
                                        -- 0o203 0o37r imm8 -- cmp reg,imm8
                                        s5 &= 0o203
                                    elsif p1size=1 then
--                                  elsif p1size=1 
--                                     or p2size=1 then     --DEV should that be and?
                                        -- 0o202 0o37r imm8 -- eg cmp_al_imm8
--                                      s5 &= 0o200     -- same, apparently (but kinda cmp r8,imm32 corrected)
--                                      s5 &= 0o202
                                        s5 &= 0o200     -- 0202 gave illegal instruction in fdbg
--00402BDC . 81FC 82000000  CMP ESP,82  (#82=130)
--00402BE2   80FC 80        CMP AH,80   (#FC=252, #80=128)
                                    else
--00430000 >     83 C8 09       OR EAX,9
--00430003   0C 09          OR AL,9
--00430005   66: 83C8 09    OR AX,9
--00430009   66: 0D 0003    OR AX,300
--0043000D   83 C8 30       OR EAX,30
--00430010   0D 00030000    OR EAX,300
                                        ?9/0 -- placeholder for more code (16bit?)
                                    end if
                                    xrm = 0o300+mod*8+reg
                                    s5 &= {xrm,and_bits(p2details,#FF)}
                                end if
                            else
                                ?9/0 -- sanity check (should never trigger)
                            end if
                        elsif p2type=P_RID then
                            if p1size=8 then
                                s5 &= 0o307
                                xrm = 0o300+reg
                            else
                                -- 0o27r imm32                  -- mov r32,imm32
                                xrm = 0o270+reg
                            end if
--                          s5 &= {xrm,isVno,0,0,p2details}
                            s5 &= {xrm,isVno,0,0,and_bits(p2details,#3FFFFFFF)}
                        elsif p2type=P_VAR then
                            {N,sType} = p2details
                            if p1size=2 then
                                s5 &= 0o146     -- <word prefix>
                            end if
-- 10/09/2013:
--                          if sType=S_GVar2 then -- or Const?
                            if sType<=S_GVar2 then
                                if op=T_mov then
--                                  if reg=0 and (X64=0 or p1size!=2) then -- eax
--printf(1,"pilasm.e line 3060: please check list.asm (tokline=%d) for mov eax,[m32]\n",{tokline})
                                    if reg=0 and X64=0 then -- eax
--if X64=1 then
-->?9/0
--      if r=eax and X64=0 then
--          emitHex5v(mov_eax_mem32,N)          -- mov eax,[N]
--
--       mov_eax_mem32  =  #A1,         -- 0o241 m32                -- mov eax,[m32]
--
--      if reg=eax and X64=0 then
--          emitHex5v(mov_mem32_eax,N)  -- mov [N],eax
--
--       mov_mem32_eax  =  #A3,         -- 0o243 m32                -- mov [m32],eax
--end if
                                        -- 0o241 m32                    -- mov eax,[m32]
                                        s5 &= 0o241
                                    else -- not eax
                                        -- 0o213 0o0r5 m32              -- mov reg,[m32]
                                        xrm = 0o005+reg*8
                                        s5 &= {0o213,xrm}
                                    end if
                                elsif op=T_cmp then
                                    -- 0o073 0o0r5 m32                  -- cmp reg,[m32]
                                    xrm = 0o005+reg*8
                                    s5 &= {0o073,xrm}
                                else
                                    ?9/0 -- placeholder for more code
                                end if
                                s5 &= {isVar,0,0,N}
                            elsif sType=S_TVar then
                                if op=T_mov then
                                    s5 &= 0o213                         -- mov_dword
                                elsif op=T_cmp then
                                    -- 0o073 0o1r5 d8                   -- cmp reg,[ebp+d8]
                                    s5 &= 0o073
                                else
--                                  Aborp("oops")
                                    ?9/0 -- placeholder for more code (eg add reg,[hllvar])
                                end if
                                emit_ebpN(reg,N)
                            else
                                ?9/0 -- sanity check (should never trigger)
                            end if
                        elsif p2type=P_SEG then
                            if p2size!=4 then ?9/0 end if
                            {p2type,k,offset} = p2details
-- in case we ever need any others: (for now only FS is supported)
--       sops={#26,  #2E,  #36,  #3E,  #64,  #65},
--       sopt={"ES:","CS:","SS:","DS:","FS:","GS:"},
                            if k!=T_fs then ?9/0 end if
                            -- 0o144 -- segment override prefix (FS)
                            s5 &= 0o144
                            if p2type=P_IMM then
--                              if reg=0 then   -- eax
                                if reg=0 and X64=0 then -- eax
--if X64=1 then
--  printf(1,"pilasm.e line 3116: please check list.asm (tokline=%d) for mov eax,[m32]\n",{tokline})
--end if
                                    -- 0o241 m32                    -- mov eax,[m32]
                                    s5 &= 0o241
                                else
                                    -- 0o213 0o0r5 m32              -- mov reg,[m32]
                                    xrm = 0o005+reg*8
                                    s5 &= {0o213,xrm}
                                end if
                                apnds5dword(offset)
                            elsif p2type=P_REG then
                                ?9/0
                            else
                                ?9/0
                            end if
                        else
                            ?9/0 -- sanity check (should never trigger)
                        end if
                    elsif p1type=P_MEM then
                        {scale,idx,base,offset} = p1details
                        rex = 0
                        if p1size=8 then
                            rex = #48
                        end if
                        if base>8 then
                            rex = or_bits(rex,#41)
                            base -= 8
                        end if
                        if idx>8 then
                            rex = or_bits(rex,#42)
                            idx -= 8
                        end if
                        if p2type=P_REG then
                            if p2details>8 then
                                rex = or_bits(rex,#44)
                                p2details -= 8
                            end if
                        end if
                        if rex then
                            s5 &= rex
                        end if
                        if p2type=P_REG then
                            if p1size=2 then
                                s5 &= 0o146                                         -- word_prefix
                                p1size = 4
                            end if
--                          if p1size=4 then
                            if p1size=4 
                            or p1size=8 then
                                if op=T_mov then
                                    -- 0o211 0o0rb              -- mov [base],r32
                                    -- 0o211 0o1rb d8           -- mov [base+d8],r32
                                    -- 0o211 0o2rb d32          -- mov [base+d32],r32
                                    -- 0o211 0o0r4 sib          -- mov [b+i*s],r32
                                    -- 0o211 0o1r4 sib d8       -- mov [b+i*s+d8],r32
                                    -- 0o211 0o2r4 sib d32      -- mov [b+i*s+d32],r32
                                    -- 0o211 0o1r4 0o044 d8     -- mov [esp+d8],r32
                                    -- 0o211 0o1r4 0o044 d32    -- mov [esp+d32],r32
                                    s5 &= 0o211
                                elsif op=T_test then
                                    s5 &= 0o205
                                elsif mod!=-1 then
                                    -- 0o0i1 0o1r4 sib d8       -- (add..cmp) dword[b+i*s+d8],reg
                                    xrm = 0o001+mod*8 -- 0o0i1
                                    s5 &= xrm
                                else
                                    ?9/0    -- placeholder for more code
                                end if
                            elsif p1size=1 then
                                if op=T_mov then
                                    -- 0o210 0o004 sib          -- mov [b+i*s],r8
                                    -- 0o210 0o1r4 sib d8       -- mov [b+i*s+d8],r8
                                    s5 &= 0o210
                                elsif mod!=-1 then
                                    -- 0o0i0 0o1r4 sib d8       -- (add..cmp) byte[b+i*s+d8],r8
                                    xrm = 0o000+mod*8 -- 0o0i0
                                    s5 &= xrm
                                else
                                    ?9/0    -- placeholder for more code
                                end if
                            else
                                Aborp("size?")
--                              ?9/0    -- placeholder for more code?
                            end if
                            reg = p2details-1
                            emit_xrm_sib(reg,scale,idx,base,offset)
                        elsif p2type=P_MEM then
                            ?9/0 -- sanity check (should never trigger)
                        elsif p2type=P_LBL then
                            if p1size=8 then
--                              s5 &= #48   -- (already just done)
                            elsif p1size!=4 then
                                Aborp("invalid operand size")
                            end if
                            if op=T_mov then
                                s5 &= 0o307
                                emit_xrm_sib(0,scale,idx,base,offset)
                                lblidx = p2details
                                k = lblchain[lblidx]
                                emit_quad(isAddr,0,0,k)
                                lblchain[lblidx] = length(s5)
                            else
                                ?9/0 -- placehoder for more code
                            end if
                        elsif p2type=P_IMM then
                            if op=T_mov then
                                if p1size=4 
                                or p1size=8 then
                                    -- 0o307 0o00r imm32        -- mov dword[reg],imm32
                                    -- 0o307 0o10r d8 imm32     -- mov [reg+d8],imm32
                                    -- 0o307 0o004 sib i32      -- mov [b32+i32*s],imm32
                                    s5 &= 0o307
                                elsif p1size=2 then
                                    -- 0o146 0o307 0o00r imm16      <word prefix> mov word[reg],imm16
                                    s5 &= 0o146                                     -- word_prefix
                                    s5 &= 0o307                                     -- mov_dword
                                elsif p1size=1 then
                                    s5 &= 0o306                                     -- mov_byte
--                              elsif p1size=8 then -- DEV not sure about this... (may need more rex handling?)
----DEV rex (probably wants to be moved up, like p1type=P_REG)
----?9/0
--                                  s5 &= {#48,0o307}
                                else
                                    ?9/0 -- sanity check (should never trigger)
                                end if
                                emit_xrm_sib(0,scale,idx,base,offset)
                                if p1size=4 then
                                    apnds5dword(p2details)                          -- imm32
                                elsif p1size=2 then
                                    validate_size(2,p2details)
                                    apnds5word(p2details)                           -- imm16
                                elsif p1size=1 then
                                    validate_size(1,p2details)
                                    s5 &= and_bits(p2details,#FF)                   -- imm8
                                elsif p1size=8 then
--                                  if p2size!=0 then ?9/0 end if
                                    if find(p2size,{0,8})=0 then ?9/0 end if
--                                  apnds5qword(p2details)                          -- imm64
                                    validate_size(4,p2details) -- (if this triggers we need better rex handling) [actually, 32-bit Phix might struggle to compile 64-bit literals...]
                                    apnds5dword(p2details)                          -- imm64
                                else
                                    ?9/0 -- sanity check (should never trigger)
                                end if
                            elsif mod!=-1
                               or op=T_test then
--trace(1)
--                              if p2details<-#80 or p2details>#7F
                                if p2details<-#80 or p2details>#FF
                                or (p2details>#7F and p1size=4)
                                or (op=T_test and p1size=4) then
                                    if mod!=-1 then
                                        if p1size=2 then
                                            s5 &= 0o146     -- <word prefix>
                                            p2size = 2
                                        else
                                            p2size = 4
                                        end if
                                        -- 0o201 0o17b d8 imm32 -- cmp dword[base+d8],imm32
                                        -- 0o201 0o174 sib d8 imm32 -- cmp dword[b32+i32*s+d8],imm32
                                        -- 0o201 0o274 sib d32 imm32 -- cmp dword[b32+i32*s+d32],imm32
                                        s5 &= 0o201
                                    elsif op=T_test then
                                        -- 0o367 0o10b d8 imm32 -- test dword[base+d8],imm32 
                                        s5 &= 0o367
                                        p2size = 4
                                    else
                                        ?9/0 -- placeholder for more code
                                    end if
--                                  p2size = 4
                                else
                                    if op=T_test then
                                        -- 0o366 0o10b d8 imm8  -- test byte[base+d8],imm8
                                        s5 &= 0o366
                                    else
                                        -- 0o200 0o174 sib d8 i8    -- cmp byte[b32+i32*s+d8],i8
                                        -- 0o203 0o10b d8 i8        -- add dword[base+d8],i8
                                        -- 0o203 0o11b d8 i8        --  or dword[base+d8],i8
                                        -- 0o203 0o12b d8 i8        -- adc dword[base+d8],i8
                                        -- 0o203 0o13b d8 i8        -- sbb dword[base+d8],i8
                                        -- 0o203 0o14b d8 i8        -- and dword[base+d8],i8
                                        -- 0o203 0o15b d8 i8        -- sub dword[base+d8],i8
                                        -- 0o203 0o16b d8 i8        -- xor dword[base+d8],i8
                                        -- 0o203 0o17b d8 i8        -- cmp dword[base+d8],i8
                                        -- 0o203 0o104 sib d8 i8    -- add dword[b32+i32*s+d8],i8
                                        -- 0o203 0o114 sib d8 i8    --  or dword[b32+i32*s+d8],i8
                                        -- 0o203 0o124 sib d8 i8    -- adc dword[b32+i32*s+d8],i8
                                        -- 0o203 0o134 sib d8 i8    -- sbb dword[b32+i32*s+d8],i8
                                        -- 0o203 0o144 sib d8 i8    -- and dword[b32+i32*s+d8],i8
                                        -- 0o203 0o154 sib d8 i8    -- sub dword[b32+i32*s+d8],i8
                                        -- 0o203 0o164 sib d8 i8    -- xor dword[b32+i32*s+d8],i8
                                        -- 0o203 0o174 sib d8 i8    -- cmp dword[b32+i32*s+d8],i8
                                        if p1size=1 then
                                            s5 &= 0o200
                                        elsif p1size=2 then
                                            s5 &= 0o146     -- <word prefix>
                                            s5 &= 0o203
                                        elsif p1size=4
                                           or p1size=8 then
                                            s5 &= 0o203
                                        else
                                            ?9/0 -- placeholder for 16-bit code
                                        end if
                                    end if
                                    if p2details<-#80 or p2details>#FF then
                                        ?9/0
                                    end if
                                    p2size = 1
                                end if
                                if op=T_test then mod = 0 end if
                                if mod=-1 then ?9/0 end if
                                emit_xrm_sib(mod,scale,idx,base,offset)
                                if p2size=4 then
                                    apnds5dword(p2details)                          -- imm32
                                elsif p2size=2 then
                                    apnds5word(p2details)                           -- imm16
                                elsif p2size=1 then
                                    s5 &= and_bits(p2details,#FF)                   -- imm8
                                else
                                    ?9/0 -- placeholder for 16-bit code
                                end if
                            else
                                ?9/0 -- sanity check (should never trigger)
                            end if
                        elsif p2type=P_VAR then
                            ?9/0 -- sanity check (should never trigger)
                        elsif p2type=P_GBL then
                            if (Z64=0 and p1size!=4)
                            or (Z64=1 and p1size!=8) then
                                Aborp("invalid operand size")
                            end if
                            if op=T_mov then
                                s5 &= 0o307
                                emit_xrm_sib(0,scale,idx,base,offset)
                                lblidx = p2details
                                s5 &= {isGaddr,0,0,lblidx}
--                          elsif op=T_cmp then
-- (untested)
--                              s5 &= 0o201
--                              emit_xrm_sib(mod,scale,idx,base,offset)
--                              lblidx = p2details
--                              s5 &= {isGaddr,0,0,lblidx}
                            else
                                ?9/0
                            end if
                        else
                            ?9/0 -- sanity check (should never trigger)
                        end if
                    elsif p1type=P_VAR then
                        {N,sType} = p1details
                        rex = 0
--DEV is p1size set properly? (spotted in passing) [Yes, it is!]
                        if p1size=8 then
                            rex = #48
                        end if
                        if p2type=P_REG then
                            reg = p2details-1
                            if reg>8 then
                                ?9/0 -- fixme
                            end if
                        end if
                        if rex then
                            s5 &= rex
                        end if
                        if p2type=P_REG then
                            if op=T_mov then
                                if p1size=2 then
                                    s5 &= 0o146     -- <word prefix>
                                end if
                                -- eg mov [res],eax
                                if sType=S_GVar2 then -- or Const?
--                                  if reg=0 and (X64=0 or p1size!=2) then -- eax
                                    if reg=0 and X64=0 then -- eax
--if X64=1 then
--  printf(1,"pilasm.e line 3363: please check list.asm (tokline=%d) for mov [m32],eax\n",{tokline})
--end if
                                        -- 0o243 mem32                              -- mov [mem32],eax
                                        s5 &= 0o243
                                    else -- not eax
                                        -- 0o211 0o0r5 mem32                        -- mov [mem32],r32
                                        xrm = reg*8+5   -- 0o0r5
                                        s5 &= {0o211,xrm}
                                    end if
                                    s5 &= {isVar,0,0,N}
                                elsif sType=S_TVar then
                                    -- 0o211 0o1r5 d8                               -- mov [ebp+d8],r32
                                    -- 0o211 0o2r5 d32                              -- mov [ebp+d32],r32
                                    s5 &= 0o211
                                    emit_ebpN(reg,N)
                                else
                                    ?9/0  -- sanity check (should never trigger)
                                end if
                            elsif sType=S_GVar2 then -- or Const??
                                -- 0o001 0r5    -- add [mem32],r32
                                -- 0o011 0r5    -- or [mem32],r32
                                -- 0o021 0r5    -- adc [mem32],r32
                                -- 0o031 0r5    -- sbb [mem32],r32
                                -- 0o041 0r5    -- and [mem32],r32
                                -- 0o051 0r5    -- sub [mem32],r32
                                -- 0o061 0r5    -- xor [mem32],r32
                                -- 0o071 0r5    -- cmp [mem32],r32
                                xrm = 0o001+mod*8 -- 0o0m1 (where m is instruction modifier)
                                s5 &= xrm
                                xrm = 0o005+reg*8 -- 0o0r5
                                s5 &= xrm
--30/5/21!!! (we is not about to output a 4 byte literal next...!)
--if X64=1 then
--                              s5 &= {isVar4,0,0,N}
--else
                                s5 &= {isVar,0,0,N}
--end if
--          cmp [AllowBreak],ebx
--00429B01   #39 0o035 6C2A4000 CMP DWORD PTR DS:[402A6C],EBX
--00429B01   0o071 0o035 6C2A4000   CMP DWORD PTR DS:[402A6C],EBX
--00429B07   0o071 0o005 6C2A4000   CMP DWORD PTR DS:[402A6C],EAX
                            else
                                ?9/0  -- placeholder for more code
                            end if
                        elsif p2type=P_LBL then
                            if op!=T_mov then ?9/0 end if
                            ?9/0  -- placeholder for more code ( "mov eax,:lbl"? )
                        elsif p2type=P_IMM then
--                          if p1size!=4 then ?9/0 end if -- sanity check
--                          if sType=S_GVar2 then -- or Const?
                            if sType=S_GVar2
                            or sType=S_Const then
                                if op=T_mov then
                                    -- 0o307 0o005 m32 i32                          -- mov [m32],imm32
--10/11/16:
--if Z64=1 then
if X64=1 then
                                    s5 &= {0o307,0o005,isVar4,0,0,N}
else
                                    s5 &= {0o307,0o005,isVar,0,0,N}
end if
                                    apnds5dword(p2details)  -- imm32
                                else
                                    -- 0o201 0o075 m32 imm32                        -- cmp dword[m32],imm32
                                    -- 0o203 0o075 m32 imm8                         -- cmp dword[m32],imm8
                                    xrm = 0o005+mod*8   -- 0o0m5
                                    if p2details<-#80 or p2details>#7F then
--if Z64=1 then ?9/0 end if
if X64=1 then ?9/0 end if
                                        s5 &= {0o201,xrm,isVar,0,0,N}
                                        apnds5dword(p2details)  -- imm32
                                    else
--24/1/15:
--if Z64=1 then
if X64=1 then
--?1
                                        s5 &= {0o203,xrm,isVar1,0,0,N}
else
                                        s5 &= {0o203,xrm,isVar,0,0,N}
end if
                                        s5 &= and_bits(p2details,#FF)  -- imm8
                                    end if
                                end if
                            elsif sType=S_TVar then
--DEV does not cope with "test [withjs],1"... (24/4/21)
                                if op=T_mov then
                                    -- 0o307 0o105 d8 imm32                         -- mov [ebp+d8],imm32
                                    -- 0o307 0o205 d32 imm32                        -- mov [ebp+d32],imm32
                                    s5 &= 0o307
                                    emit_ebpN(0,N)
                                    apnds5dword(p2details)  -- imm32
                                elsif mod!=-1 then
                                    -- 0o201 0o1m5 d8 imm32                         -- cmp dword[ebp+d8],imm32
                                    -- 0o201 0o2m5 d32 imm32                        -- and dword[ebp+d32],imm32
                                    -- 0o203 0o1m5 d8 imm8                          -- sub dword[ebp+d8],imm8
                                    -- 0o203 0o2m5 d32 imm8                         -- xor dword[ebp+d32],imm8
                                    if p2details<-#80 or p2details>#7F then
                                        p2size = 4
                                        s5 &= 0o201
                                    else
                                        p2size = 1
                                        s5 &= 0o203
                                    end if
                                    emit_ebpN(mod,N)                                -- 0oxm5
                                    if p2size=4 then
                                        apnds5dword(p2details)                      -- imm32
                                    else
                                        s5 &= and_bits(p2details,#FF)               -- imm8
                                    end if
                                else
                                    ?9/0 -- placeholder for more code
                                end if
                            else
                                ?9/0 -- placeholder for more code
                            end if
                        else
                            ?9/0 -- sanity check (should never trigger)
                        end if
                    elsif p1type=P_RAW then
                        if p2type!=P_REG then ?9/0 end if   -- placeholder for more code
                        {scale,idx,base,offset} = p1details
--6/11/16:
                        if p1size=8 then
                            s5 &= #48
                        end if
                        reg = p2details-1
                        s5 &= 0o211
                        emit_ds_xrm_sib(reg,scale,idx,base,offset)
                    else
                        ?9/0 -- sanity check (should never trigger)
                    end if
                end if -- emitON

            elsif ttidx=T_rol
               or ttidx=T_ror
               or ttidx=T_rcl
               or ttidx=T_rcr
               or ttidx=T_shl
               or ttidx=T_shr
               or ttidx=T_sar
               or ttidx=T_bt then
--if emitON then trace(1) end if
                mod = find(ttidx,{T_rol,T_ror,T_rcl,T_rcr,T_shl,T_shr,-1,T_sar,T_bt})-1 -- (0..7)
                {p1type,p1size,p1details} = get_operand(P_REG,true)
                reg = p1details-1
                if p1type!=P_REG then ?9/0 end if -- sanity check (or are there 8/16 bit register shifts?)
--              if p1size!=4 then
                if p1size!=4
                and p1size!=1
                and p1size!=2
                and not (Z64=1 and p1size=8) then
                    Aborp("invalid")
                end if
                comma()
--DEV should allow cl:
                {p2type,p2size,p2details} = get_operand(P_IMM,false)
                if emitON then
                    if p2type=P_IMM then
                        rex = 0
                        if p1size=8 then
                            if Z64!=1 then ?9/0 end if
                            rex = #48
                        end if
                        if reg>7 then
                            rex = or_bits(rex,#41)
--                          rex = or_bits(rex,#44)
                            reg -= 8
                        end if
                        if rex then
                            s5 &= rex
                        end if
                        if mod=8 then -- T_bt
                            xrm = 0o340+reg
--/*
bt ecx,30       0FBAE11E    (0o017 0o272 0o341 #1E)
shr ecx,30      C1E91E      (0o301 0o351 #1E)
rol eax,5       C1C005      (0o301 0o300 #05)
bt eax,5        0FBAE005    (0o017 0o272 0o340 #05)
0047901D     66:0FBAE1 03   BT CX,3  (#66 0o017 0o272 0o341 3)
--*/
                            if p1size=1 then Aborp("invalid") end if
                            if p1size=2 then
                                s5 &= #66
                            end if
                            s5 &= {0o017,0o272,xrm,and_bits(p2details,#FF)}
                        else
                            xrm = 0o300+mod*8+reg
                            if p2details=1 then
                                -- 0o321 0o31r      -- ror reg,1
                                -- 0o321 0o34r      -- shl reg,1
                                -- 0o321 0o35r      -- shr reg,1
--                              s5 &= {0o321,xrm}
                                if p1size=1 then
                                    if mod=8 then -- T_bt
                                        s5 &= 0o320
                                    else
                                        s5 &= 0o320
                                    end if
                                else
                                    if p1size=2 then
                                        s5 &= #66
                                    end if
                                    s5 &= 0o321
                                end if
                                s5 &= xrm
                            else
                                -- 0o301 0o31r imm8 -- ror reg,imm8
                                -- 0o301 0o34r imm8 -- shl reg,imm8
                                -- 0o301 0o35r imm8 -- shr reg,imm8
--                              s5 &= {0o301,xrm,and_bits(p2details,#FF)}
                                if p1size=1 then
                                    s5 &= 0o300
                                else
                                    if p1size=2 then
                                        s5 &= #66
                                    end if
                                    s5 &= 0o301
                                end if
                                s5 &= {xrm,and_bits(p2details,#FF)}
                            end if

                        end if
                    elsif p2type=P_REG then
--                      if p2size!=1 or p2details!=ecx then
                        if p2size!=1 or p2details!=2 then
                            Aborp("cl expected")
                        end if
                        if p1size=8 then
                            if Z64!=1 then ?9/0 end if
                            s5 &= #48
                        end if
--                      xrm = 0o340+reg
                        xrm = 0o300+mod*8+reg
                        s5 &= {0o323,xrm}
                    else
                        ?9/0
                    end if
                end if
            elsif ttidx=T_inc
               or ttidx=T_dec then
                mod = find(ttidx,{T_inc,T_dec})-1
                {p1type,p1size,p1details} = get_operand(P_RM,true)
                if emitON then
--                  if Z64=1 then
                    if X64 then
                        Aborp("not valid in 64 bit mode (used as REX prefix)")
                    end if
                    if p1size=4 then
                        if p1type=P_REG then
--DEV not valid on x64:
--if c2<=1 then
--  if machine=64 then ?9/0 end if  -- 0o100 .. 0o117 used as REX prefix
--end if
                            -- 0o10r    -- inc reg
                            -- 0o11r    -- dec reg
                            reg = p1details-1
                            xrm = 0o100+mod*8+reg   
                            s5 &= xrm
                        elsif p1type=P_MEM then
                            -- 0o377 0o10b d8           -- inc dword[b32+d8]
                            -- 0o377 0o11b d8           -- dec dword[b32+d8]
                            -- 0o377 0o104 sib d8       -- inc dword[b32+i32*s+d8]
                            -- 0o377 0o114 sib d8       -- dec dword[b32+i32*s+d8]
                            {scale,idx,base,offset} = p1details
                            s5 &= 0o377
                            emit_xrm_sib(mod,scale,idx,base,offset)
                        else
                            ?9/0 -- sanity check (should never happen)
                        end if
                    else
                        ?9/0 -- placeholder for more code
                    end if
                end if
            elsif ttidx=T_push then
                -- 64bit: pop ds/es/ss and push cs/ds/es/ss invalid, but this never supported them anyway
                {p1type,p1size,p1details} = get_operand(P_RMLI,false)
                if not find(p1size,{0,1,4,8}) then ?9/0 end if  -- sanity check (should never trigger)
                if emitON then
                    rex = 0
                    if p1size=8 then
--                      if Z64!=1 then Aborp("invalid") end if
                        if X64!=1 then Aborp("invalid") end if
                        rex = #48
                    end if
                    if p1type=P_REG then
--DEV tryme (save some unnecessary rex bytes, may need that rex=#49)
--                      rex = 0
                        reg = p1details-1
                        if reg>7 then
--                          rex = or_bits(rex,#44)
                            rex = or_bits(rex,#41)
--                          rex = #49
                            reg -= 8
                        end if
                        -- 0o12r                -- push r32
                        xrm = 0o120+reg
                        if rex then
                            s5 &= rex
                        end if
                        s5 &= xrm
                    elsif p1type=P_MEM then
                        {scale,idx,base,offset} = p1details
                        -- 0o377 0o06r          -- push dword[reg]
                        -- 0o377 0o16r          -- push dword[reg+d8]
                        -- 0o377 0o26r d32      -- push dword[reg+d32]
                        -- 0o377 0o064 sib      -- push dword[b+i*s]
                        -- 0o377 0o164 si5 00   -- push dword[ebp+i*4]
                        -- 0o377 0o164 sib d8   -- push dword[b+i*s+d8]
                        -- 0o377 0o264 sib d32  -- push dword[b+i*s+d32]
                        if rex then
                            s5 &= rex
                        end if
                        s5 &= 0o377
                        emit_xrm_sib(6,scale,idx,base,offset)
                    elsif p1type=P_RAW then
                        {scale,idx,base,offset} = p1details
                        if rex then
                            s5 &= rex
                        end if
                        s5 &= 0o377
                        emit_ds_xrm_sib(6,scale,idx,base,offset)
                    elsif p1type=P_VAR then
                        {N,sType} = p1details
                        if rex then
                            s5 &= rex
                        end if
-- 10/09/2013:
--                      if sType=S_GVar2 then -- or Const?
                        if sType<=S_GVar2 then
                            -- 0o377 0o065 mem32        -- push dword[mem32]
                            s5 &= {0o377,0o065,isVar,0,0,N}
                        elsif sType=S_TVar then
                            -- 0o377 0o165 d8       -- push dword[ebp+d8] (d8 of 0 rqd)
                            -- 0o377 0o265 d32      -- push dword[ebp+d32]
                            s5 &= 0o377
                            emit_ebpN(6,N)  -- (6 is an instruction modifier)
                        else
                            ?9/0 -- sanity check (should never happen)
                        end if
                    elsif p1type=P_LBL then
                        s5 &= 0o150
                        lblidx = p1details
                        k = lblchain[lblidx]
                        emit_quad(isAddr,0,0,k)
                        lblchain[lblidx] = length(s5)
                    elsif p1type=P_GBL then
                        ?9/0    -- placeholder for more code, maybe
                    elsif p1type=P_IMM then
                        if rex then
--Aborp("untested")
--?9/0 -- (untested)
                            s5 &= rex
                        end if
                        -- 0o150 imm32              -- push imm32
                        if p1details>=-128 and p1details<=127 then
                            s5 &= 0o152
                            s5 &= and_bits(p1details,#FF)
                        else
                            s5 &= 0o150
                            apnds5dword(p1details)
                        end if
                    else
                        ?9/0 -- sanity check (should never trigger)
                    end if
                end if -- emitON
            elsif ttidx=T_pop then
                {p1type,p1size,p1details} = get_operand(P_RM,true)
                if emitON then
                    if p1size=1 then Aborp("invalid (guard instruction)") end if
                    if p1type=P_REG then
                        -- 0o13r                    -- pop reg
--                      if p1size!=4 then ?9/0 end if
                        reg = p1details-1
                        if p1size=8 then
                            rex = #48
                            if reg>7 then
--                              rex = or_bits(rex,#44)
                                rex = or_bits(rex,#41)
                                reg -= 8
                            end if
                            s5 &= rex
                        elsif p1size!=4 then
                            Aborp("uh?")
                        end if
                        xrm = 0o130+reg
                        s5 &= xrm
                    elsif p1type=P_MEM then
                        -- pop dword[b+i*s+d8] etc maybe...
                        {scale,idx,base,offset} = p1details
if newEmit and X64=1 then
                        if p1size!=8 then ?9/0 end if
----    s5 &= #48
--14/2/17 (over pop qword[rbp], this made it pop qword[ebp])
--  s5 &= #67   -- (0o147)
--  printf(1,"pilasm.e line 3627: please check list.asm (tokline=%d) for pop[mem]\n",{tokline})
else
                        if p1size!=4 then ?9/0 end if   -- use a pop/mov pair, for now... [DEV]
end if
--  p1details = {0,0,7,12}
--CPU Disasm
--Address   Hex dump            Command                                  Comments
--004099F5    #8F #46 12        pop DWORD PTR DS:[ESI+12]
--004099F5    0o217 0o106 12        pop DWORD PTR DS:[ESI+12]

--                      ?9/0    -- placeholder for more code
--?                         reg = p2details-1
                        s5 &= 0o217
                        emit_xrm_sib(0,scale,idx,base,offset)
                    elsif p1type=P_VAR then
                        -- 0o217 0o005 mem32        -- pop dword[mem32]
                        {N,sType} = p1details
--                      if p1size!=4 then ?9/0 end if
if newEmit and X64=1 then
                        if p1size!=8 then ?9/0 end if
                        s5 &= #48
else
                        if p1size!=4 then ?9/0 end if
end if
                        if sType<=S_GVar2 then
                            -- 0o217 0o005 mem32        -- pop dword[mem32]
                            s5 &= {0o217,0o005,isVar,0,0,N}
                        elsif sType=S_TVar then
                            -- 0o217 0o105 d8       -- pop dword[ebp+d8] (d8 of 0 rqd)
                            -- 0o217 0o205 d32      -- pop dword[ebp+d32]
                            s5 &= 0o217
                            emit_ebpN(0,N)  -- (6 is an instruction modifier)
                        else
                            ?9/0 -- sanity check (should never happen)
                        end if
                    else
                        ?9/0 -- sanity check (should never trigger)
                    end if
                end if
            elsif ttidx=T_pushad then
                -- DEV invalid on 64bit
                if emitON then
                    if X64 then Aborp("invalid on 64 bit") end if
                    -- 0o140                    -- pushad
                    s5 &= 0o140
                end if
            elsif ttidx=T_popad then
                -- DEV invalid on 64bit
                if emitON then
                    if X64 then ?9/0 end if
                    -- 0o141                    -- popad
                    s5 &= 0o141
                end if
            elsif ttidx=T_call then
                --DEV 64bit: RIP addressing, 0o232 (#9A) invalid
                skipSpacesAndComments()
--if emitON then trace(1) end if
                if Ch=':' then
                    {p1type,p1size,lblidx} = get_operand(P_LBL,false)
                    if p1type=P_GBL then
                        if emitON then
                            -- 0o350 offset32
                            s5 &= {0o350,isJmpG,0,0,lblidx}
                        end if
                    elsif p1type=P_LBL then
                        if emitON then
                            k = lblchain[lblidx]
                            -- 0o350 offset32
                            s5 &= 0o350
                            emit_quad(isJmp,0,0,k)
                            lblchain[lblidx] = length(s5)
                        end if
                    else
                        ?9/0 -- sanity check (should never trigger)
                    end if
--DEV togo??
                elsif Ch='%' then -- opcode
                    -- eg call %opRetf (call_rel32 %isOpCode implied)
                    getCh()
                    tokline = line
                    tokcol = col
                    ilstuff = il_search(0)
                    if not integer(ilstuff) then Aborp("invalid") end if
                    if emitON then
--if newEmit then
    if intellisense=0 then
        Aborp("invalid for newEmit")
    end if
--  sprintf(1,"warning: emitHex5call(%d=%s) skipped for newEmit (pilx86.e line 952)\n",{opcode,opNames[opcode]})
--else
--                      k = ilstuff
--                      -- 0o350 offset32
--                      s5 &= {0o350,isOpCode,0,0,k}
--end if
                    end if
                elsif Ch='[' then
                    call_mem()
                else -- not label OR OPCODE
                    getToken()
                    if toktype=DQUOTE then
                        -- eg call "user32","GetActiveWindow" (call_mem32 %isAPIfn implied)
                        libname = lower(TokStr)
                        if find('.',libname)=0 then
                            libname &= ".dll"
                        end if
                        libidx = find(libname,APIlibs)
                        if libidx=0 and emitON then
                            APIlibs = append(APIlibs,libname)
                            libidx = length(APIlibs)
                            APIerrlib = append(APIerrlib,{fileno,tokline,tokcol})
                        end if
                        getToken()
                        MatchChar(',')
                        if toktype!=DQUOTE then
                            Aborp("function name (in quotes) expected")
                        end if
                        libfunc = {libidx,TokStr}
                        fnidx = find(libfunc,APINames)
                        if fnidx=0 and emitON then
                            APINames = append(APINames,libfunc)
                            fnidx = length(APINames)
                            APIerritem = append(APIerritem,{fileno,tokline,tokcol})
                        end if
                        if emitON then
                            -- aside: I still put the entries in APIlibs/Names etc so that
                            --        if OLDSTYLE then
                            --          ...
                            --        else
                            --          ...%isAPIfn,...
                            --        end if
                            --        generates appropriate errors while OLDSTYLE=1 rather
                            --        than wait until it gets set to 0. This may need to 
                            --        change if/when we go cross-platform.
                            -- 0o377 0o025 mem32                            -- call [mem32]
                            s5 &= {0o377,0o025,isAPIfn,0,0,fnidx}
                        end if
                    elsif ttidx=T_dword then
                        skipSpacesAndComments()
                        if Ch!='[' then
                            Expected("'['")
                        end if
                        call_mem()
                    else
                        reg = 0
                        if toktype=LETTER then
                            reg = find(ttidx,REG32)
                            if reg=0 and Z64=1 then
                                reg = find(ttidx,R64)
--                              rex = ??
--                              size = 8?
                            end if
                        end if
                        if reg=0 then
                            --DEV/SUG might want to allow "call lbl" to be treated as "call :lbl"
                            --    (like jmp, use il_search/REZ32 after an if Ch='\"' then for the DQUOTE)
                            --    (also, maybe, treat call dword[] the same as call [])
                            Aborp(":loclbl, :%gbllbl, %opCode, \"lib\",\"name\", or register expected")
                        end if
                        if emitON then
                            -- 0o377 0o32r              -- call reg
                            if reg>8 then ?9/0 end if
                            xrm = 0o320+reg-1
                            s5 &= {0o377,xrm}           
                        end if
                    end if
                end if
            elsif ttidx=T_jo then
                jcc(#00)
            elsif ttidx=T_jno then
                jcc(#01)
            elsif ttidx=T_jb
               or ttidx=T_jc
               or ttidx=T_jnae then
                jcc(#02)
            elsif ttidx=T_jnb
               or ttidx=T_jnc
               or ttidx=T_jae then
                jcc(#03)
            elsif ttidx=T_je
               or ttidx=T_jz then
                jcc(#04)
            elsif ttidx=T_jne
               or ttidx=T_jnz then
                jcc(#05)
            elsif ttidx=T_jbe
               or ttidx=T_jna then
                jcc(#06)
            elsif ttidx=T_jnbe
               or ttidx=T_ja then
                jcc(#07)
            elsif ttidx=T_js then
                jcc(#08)
            elsif ttidx=T_jns then
                jcc(#09)
            elsif ttidx=T_jp
               or ttidx=T_jpe then
                jcc(#0A)
            elsif ttidx=T_jnp
               or ttidx=T_jpo then
                jcc(#0B)
            elsif ttidx=T_jl 
               or ttidx=T_jnge then
                jcc(#0C)
            elsif ttidx=T_jnl 
               or ttidx=T_jge then
                jcc(#0D)
            elsif ttidx=T_jle
               or ttidx=T_jng then
                jcc(#0E)
            elsif ttidx=T_jnle
               or ttidx=T_jg then
                jcc(#0F)
            -- note: jcxz/jecxz only have a short form: Phix/pilasm.e works by emitting a long (4-byte) jump,
            --       and pemit.e converts any it can to short (1-byte) offsets, obviously adjusting said by
            --       any shortened jumps it is jumping over. If you wanted to support jcxz/jecxz, I think
            --       you would have to emit an (illegal) long jump here, and add validation in pemit.e/blurph
            --       that ensured it was successfully shortened...
            elsif ttidx=T_jmp then
                jcc(#10)
            elsif ttidx=T_ret then
                skipSpacesAndComments()
                if line=tokline and Ch!='}' then
                    -- 0o302 imm16              -- ret imm16
                    {p1type,p1size,p1details} = get_operand(P_IMM,false)
                    if emitON then
                        s5 &= 0o302
                        apnds5word(p1details)
                    end if
                else
                    -- 0o303                    -- ret
                    if emitON then
                        s5 &= 0o303
                    end if
                end if
            elsif ttidx=T_seto then
                setcc(#00)
            elsif ttidx=T_setno then
                setcc(#01)
            elsif ttidx=T_setb
               or ttidx=T_setc
               or ttidx=T_setnae then
                setcc(#02)
            elsif ttidx=T_setnb
               or ttidx=T_setnc
               or ttidx=T_setae then
                setcc(#03)
            elsif ttidx=T_sete
               or ttidx=T_setz then
                setcc(#04)
            elsif ttidx=T_setne
               or ttidx=T_setnz then
                setcc(#05)
            elsif ttidx=T_setbe
               or ttidx=T_setna then
                setcc(#06)
            elsif ttidx=T_setnbe
               or ttidx=T_seta then
                setcc(#07)
            elsif ttidx=T_sets then
                setcc(#08)
            elsif ttidx=T_setns then
                setcc(#09)
            elsif ttidx=T_setp
               or ttidx=T_setpe then
                setcc(#0A)
            elsif ttidx=T_setnp
               or ttidx=T_setpo then
                setcc(#0B)
            elsif ttidx=T_setl 
               or ttidx=T_setnge then
                setcc(#0C)
            elsif ttidx=T_setnl 
               or ttidx=T_setge then
                setcc(#0D)
            elsif ttidx=T_setle
               or ttidx=T_setng then
                setcc(#0E)
            elsif ttidx=T_setnle
               or ttidx=T_setg then
                setcc(#0F)
            elsif ttidx=T_fld then
                {p1type,p1size,p1details} = get_operand(P_MEM+P_FPU,false)
                if p1type=P_VAR then
                    -- Phix hll vars do not hold floats like this! (it is qword[ref*4],
                    --  eg/ie atom a ... mov eax,[a]; fld qword[ebx+eax*4]; NOT fld[a])
                    Aborp("not permitted")
                end if
                if emitON then
                    if p1type=P_MEM then
                        {scale,idx,base,offset} = p1details
                        if p1size=8 then
                            s5 &= 0o335
                            mod = 0
                        elsif p1size=10 then
                            s5 &= 0o333
                            mod = 5
                        elsif p1size=4 then
                            s5 &= 0o331
                            mod = 0
                        else
                            ?9/0
                        end if
                        emit_xrm_sib(mod, scale, idx, base, offset)
                    elsif p1type=P_FPU then
                        xrm = #BF+p1details     -- 0o28r
                        s5 &= {0o331,xrm}
                    else
                        ?9/0
                    end if
                end if
            elsif ttidx=T_fabs then
                -- 0o331 0o341              -- fabs
                if emitON then
                    s5 &= {0o331,0o341}
                end if
            elsif ttidx=T_fld1 then
                -- 0o331 0o350              -- fld1
                if emitON then
                    s5 &= {0o331,0o350}
                end if
            elsif ttidx=T_fldl2e then
                -- 0o331 0o352              -- fldl2e
                if emitON then
                    s5 &= {0o331,0o352}
                end if
            elsif ttidx=T_fldpi then
                -- 0o331 0o353              -- fldpi
                if emitON then
                    s5 &= {0o331,0o353}
                end if
            elsif ttidx=T_fldz then
                -- 0o331 0o356              -- fldz
                if emitON then
                    s5 &= {0o331,0o356}
                end if
            elsif ttidx=T_f2xm1 then
                -- 0o331 0o360              -- f2xm1
                if emitON then
                    s5 &= {0o331,0o360}
                end if
            elsif ttidx=T_fyl2x then
                -- 0o331 0o361              -- fyl2x
                if emitON then
                    s5 &= {0o331,0o361}
                end if
            elsif ttidx=T_fptan then
                -- 0o331 0o362              -- fptan
                if emitON then
                    s5 &= {0o331,0o362}
                end if
            elsif ttidx=T_fpatan then
                -- 0o331 0o363              -- fpatan
                if emitON then
                    s5 &= {0o331,0o363}
                end if
            elsif ttidx=T_fsqrt then
                -- 0o331 0o372              -- fsqrt
                if emitON then
                    s5 &= {0o331,0o372}
                end if
            elsif ttidx=T_fscale then
                -- 0o331 0o375              -- fscale
                if emitON then
                    s5 &= {0o331,0o375}     -- st0 *= power(2,st1)
--                  s5 &= {#D9,#FD}
                end if
            elsif ttidx=T_fsin then
                -- 0o331 0o376              -- fsin
                if emitON then
                    s5 &= {0o331,0o376}
                end if
            elsif ttidx=T_fcos then
                -- 0o331 0o377              -- fcos
                if emitON then
                    s5 &= {0o331,0o377}
                end if
            elsif ttidx=T_fstp
               or ttidx=T_fst then
                if ttidx=T_fst then
                    mod = 2
                else -- T_fstp
                    mod = 3
                end if
                {p1type,p1size,p1details} = get_operand(P_MEM+P_FPU,false)
                if p1type=P_VAR then
                    -- Phix hll vars do not hold floats like this! (it is qword[ref*4])
                    Aborp("not permitted")
                end if
                if emitON then
                    if p1type=P_MEM then
                        {scale,idx,base,offset} = p1details
                        if p1size=4 then
                            s5 &= 0o331
                        elsif p1size=8 then
                            s5 &= 0o335
                        elsif p1size=10 then
                            s5 &= 0o333
                            if mod=2 then ?9/0 end if   -- (I don't think fst m80 is allowed, must use fstp)
                            mod = 7
                        else
                            ?9/0
                        end if
                        emit_xrm_sib(mod, scale, idx, base, offset)
                    elsif p1type=P_FPU then
                        reg = p1details-1
                        xrm = 0o330+reg
                        s5 &= {0o335,xrm}
                    else
                        ?9/0
                    end if
                end if
            elsif ttidx=T_fild then
                {p1type,p1size,p1details} = get_operand(P_MEM,false)
                if emitON then
                    if p1type=P_VAR then
                        {N,sType} = p1details
if X64=1 then
                        if p1size!=8 then ?9/0 end if -- sanity check
--                      s5 &= #48
                        if sType=S_GVar2
                        or sType=S_Const then
                            -- 0o337 0o055 m32                  -- fild qword[mem32]
                            s5 &= {0o337,0o055,isVar,0,0,N}
                        elsif sType=S_TVar then
                            -- 0o337 0o05r                      -- fild qword[reg]
                            -- 0o337 0o155 00                   -- fild dword[ebp]
                            -- 0o337 0o155 d8                   -- fild dword[ebp+d8]
                            -- 0o337 0o255 d32                  -- fild dword[ebp+d32]
                            s5 &= 0o337
                            emit_ebpN(5,N)
                        else
                            ?9/0
                        end if
else
                        if p1size!=4 then ?9/0 end if -- sanity check
                        if sType=S_GVar2
                        or sType=S_Const then
                            -- 0o333 0o005 m32                  -- fild dword[mem32]
                            s5 &= {0o333,0o005,isVar,0,0,N}
                        elsif sType=S_TVar then
                            -- 0o333 0o00r                      -- fild dword[reg]
                            -- 0o333 0o105 00                   -- fild dword[ebp]
                            -- 0o333 0o105 d8                   -- fild dword[ebp+d8]
                            -- 0o333 0o205 d32                  -- fild dword[ebp+d32]
                            s5 &= 0o333
                            emit_ebpN(0,N)
                        else
                            ?9/0
                        end if
end if
                    elsif p1type=P_MEM then
                        -- 0o337 0o00b          -- fild word[base]
                        -- 0o333 0o00b          -- fild dword[base]
                        -- 0o333 0o004 0o044    -- fild dword[esp]
                        -- 0o333 0o10b d8       -- fild dword[base+d8]
                        -- 0o337 0o05b          -- fild qword[base]
                        -- 0o337 0o054 0o044    -- fild qword[esp]
                        -- 0o337 0o15b d8       -- fild qword[base+d8]
                        {scale,idx,base,offset} = p1details
                        if p1size=8 then
                            xrm = 7
                            mod = 5
                        elsif p1size=4 then
                            xrm = 3
                            mod = 0
                        elsif p1size=2 then
                            xrm = 7
                            mod = 0
                        else
                            ?9/0
                        end if
                        xrm = 0o330+xrm
                        s5 &= xrm
                        emit_xrm_sib(mod,scale,idx,base,offset)
                    else
                        ?9/0 -- sanity check (should never happen)
                    end if
                end if
            elsif ttidx=T_fist
               or ttidx=T_fistp then
                op = ttidx
                {p1type,p1size,p1details} = get_operand(P_MEM,false)
                if p1type=P_VAR then
                    -- (use opStoreFlt instead)
                    Aborp("not permitted")
                end if
                if emitON then
                    {scale,idx,base,offset} = p1details
                    -- 0o333 0o02b          -- fist word[base]
                    -- 0o337 0o03b          -- fistp word[base]
                    -- 0o333 0o03b          -- fistp dword[base]
                    -- 0o333 0o024 0o044    -- fist dword[esp]
                    -- 0o333 0o034 0o044    -- fistp dword[esp]
                    --(0o333 0o034 0o344    -- fistp dword[esp] - same, apparently)
                    -- 0o333 0o12b d8       -- fist dword[base+d8]
                    -- 0o333 0o13b d8       -- fistp dword[base+d8]
                    -- 0o333 0o134 0o044 d8 -- fistp dword[esp+d8]
                    -- 0o337 0o07b          -- fistp qword[base]
                    -- 0o337 0o074 0o044    -- fistp qword[esp]
                    -- 0o337 0o17b d8       -- fistp qword[base+d8]
                    -- 0o337 0o174 0o044 d8 -- fistp qword[esp+d8]
                    -- 0o337 0o174 sib d8   -- fistp qword[b+i*4+d8]
                    if p1size=8 then
                        xrm = 7
                        mod = 7
                        if op=T_fist then Aborp("command does not support specified size (use [fld st0] fistp instead)") end if
                    elsif p1size=4 then
                        xrm = 3
                        mod = 3
                        if op=T_fist then mod = 2 end if
                    elsif p1size=2 then
                        xrm = 7
                        mod = 3
                        if op=T_fist then mod = 2 end if
                    else
--                      ?9/0
                        Aborp("size error") -- (or maybe not)
                    end if
                    xrm = 0o330+xrm
                    s5 &= xrm
                    emit_xrm_sib(mod,scale,idx,base,offset)
                end if
            elsif ttidx=T_fiadd
               or ttidx=T_fimul
               or ttidx=T_ficom
               or ttidx=T_ficomp
               or ttidx=T_fisub
               or ttidx=T_fisubr
               or ttidx=T_fidiv
               or ttidx=T_fidivr then
                mod = find(ttidx,{T_fiadd,T_fimul,T_ficom,T_ficomp,T_fisub,T_fisubr,T_fidiv,T_fidivr})-1
                {p1type,p1size,p1details} = get_operand(P_MEM,false)
                if p1size=8 then
                    Aborp("16 or 32 bit only (in h/w); use fild/fadd etc")
                end if
                if emitON then
                    if p1type=P_VAR then
                        {N,sType} = p1details
--fiadd DA 0 m32, DE 0 m16
--fimul DA 1 m32, DE 1 m16
--ficom DA 2 m32, DE 2 m16      
--ficomp    DA 3 m32, DE 3 m16      
--fisub DA 4 m32, DE 4 m16      
--fisubr    DA 5 m32, DE 5 m16      
--fidiv DA 6 m32, DE 6 m16      
--fidivr    DA 7 m32, DE 7 m16      
--              fisub dword[esp]    -- (not supported at the software level)
-- DA2424
-- 0o332 0o044 #24
--0042C0A1   DE2424         FISUB WORD PTR SS:[ESP]
--0042C09E   DA0424         FIADD DWORD PTR SS:[ESP]
--              fimul dword[onebillion]
-- DA0D mem32
-- 0o332 0o015 mem32
-- #DE
-- 0o336

if X64=1 then
                        ?9/0    -- (should be caught above)
else
                        if p1size!=4 then ?9/0 end if -- sanity check
                        if sType=S_GVar2
                        or sType=S_Const then
                            -- 0o332 0o005 mem32                -- fiadd dword[mem32]
                            -- 0o332 0o015 mem32                -- fimul dword[mem32] (etc)
                            xrm = 0o005+mod*0o010
                            s5 &= {0o332,xrm,isVar,0,0,N}
                        elsif sType=S_TVar then
                            -- 0o332 0o00r                      -- fiadd dword[reg]
                            -- 0o332 0o105 00                   -- fiadd dword[ebp]
                            -- 0o332 0o155 d8                   -- fisub dword[ebp+d8]
                            -- 0o332 0o205 d32                  -- fiadd dword[ebp+d32]
                            s5 &= 0o332
                            emit_ebpN(mod,N)
                        else
                            ?9/0
                        end if
end if
                    elsif p1type=P_MEM then
                        -- 0o336 0o00b          -- fiadd word[base] (not esp)
                        -- 0o332 0o00b          -- fiadd dword[base]
                        -- 0o332 0o004 0o044    -- fiadd dword[esp]
                        -- 0o332 0o10b d8       -- fiadd dword[base+d8]
                        -- 0o332 0o24b d8       -- fisub dword[base+d32]
--0042C09E   DA0424         FIADD DWORD PTR SS:[ESP]
--0042C0A1   DE2424         FISUB WORD PTR SS:[ESP]
--0042C0A4   DA08           FIMUL DWORD PTR DS:[EAX]
--0042C0A6   DA09           FIMUL DWORD PTR DS:[ECX]
--0042C0A8   DA45 00        FIADD DWORD PTR SS:[EBP]
--0042C0AB   DA45 08        FIADD DWORD PTR SS:[EBP+8]
--0042C0AE   DA 0o205 88080000  FIADD DWORD PTR SS:[EBP+888]
--0042C0B4   DA 0o245 88080000  FISUB DWORD PTR SS:[EBP+888]
--0042C0BA   DE 0o245 88080000  FISUB WORD PTR SS:[EBP+888]

                        {scale,idx,base,offset} = p1details
                        if p1size=8 then
                            ?9/0 -- (should be caught above)
                        elsif p1size=4 then
                            xrm = 2
--                          mod = 0
                        elsif p1size=2 then
                            xrm = 6
--                          mod = 0
                        else
                            ?9/0    -- should not occur?
                        end if
                        xrm = 0o330+xrm
                        s5 &= xrm
                        emit_xrm_sib(mod,scale,idx,base,offset)
                    else
                        ?9/0 -- sanity check (should never happen)
                    end if
                end if
            elsif ttidx=T_faddp
               or ttidx=T_fsubp
               or ttidx=T_fdivp
               or ttidx=T_fmulp then
                if ttidx=T_faddp then
                    xrm = 0o301
                elsif ttidx=T_fsubp then
                    xrm = 0o351
                elsif ttidx=T_fdivp then
                    xrm = 0o371
                elsif ttidx=T_fmulp then
                    xrm = 0o311
                else
                    ?9/0
                end if
                skipSpacesAndComments()
                if line=tokline and Ch!='}' then
                    getToken()
                    if toktype!=LETTER then ?9/0 end if
                    reg = find(ttidx,{T_st0,T_st1,T_st2,T_st3,T_st4,T_st5,T_st6,T_st7})
                    if reg=0 then Aborp("unrecognised") end if
                    comma()
                    getToken()
                    if toktype!=LETTER then ?9/0 end if
                    if ttidx!=T_st0 then
                        -- (sorry but "st" is not an alias for "st0")
                        Aborp("must be st0")
                    end if
                    xrm += reg-2    -- (remove st1 default and add actual)
                end if
                if emitON then
                    s5 &= {0o336,xrm}
                end if
            elsif ttidx=T_fsub
               or ttidx=T_fadd then
                if ttidx=T_fadd then
                    xrm = 0o310
                    mod = 0
                elsif ttidx=T_fsub then
                    xrm = 0o350
                    mod = 4
--              elsif ttidx=T_fmul then
--?                 xrm = 0o310
--                  mod = 1
                else
                    ?9/0
                end if
if 0 then -- old code 10/5/15
                skipSpacesAndComments()
                p1type = P_FPU  -- (no param defaults to st1) [DEV??]
                if line=tokline and Ch!='}' then
                    getToken()
                    if toktype!=LETTER then ?9/0 end if
                    reg = find(ttidx,{T_st0,T_st1,T_st2,T_st3,T_st4,T_st5,T_st6,T_st7})
                    if reg=0 then Aborp("unrecognised") end if
                    -- (this does not (yet) allow eg "fsub stn" to be an alias of "fsub st0,stn")
                    comma()
                    getToken()
                    if toktype!=LETTER then ?9/0 end if
                    if reg=1 then -- st0
                        reg = find(ttidx,{T_st0,T_st1,T_st2,T_st3,T_st4,T_st5,T_st6,T_st7})
                        if reg=0 then Aborp("unrecognised") end if
                        xrm -= 0o010
                    else
                        if ttidx!=T_st0 then
                            -- (sorry but "st" is not an alias for "st0")
                            Aborp("must be st0")
                        end if
                    end if
                    xrm += reg-1
                    if emitON then
                        s5 &= {0o334,xrm}
                    end if
                else
                    if emitON then
--if p1type=P_FPU then
                        s5 &= {0o334,xrm}
--end if
                    end if
                end if

else
                {p1type,p1size,p1details} = get_operand(P_MEM+P_FPU,false)
                if p1type=P_VAR then
                    -- Phix hll vars do not hold floats like this! (it is qword[ref*4])
                    Aborp("not permitted")
                end if
                if p1type=P_FPU then
                    -- (this does not (yet) allow eg "fsub stn" to be an alias of "fsub st0,stn")
                    comma()
                    {p2type,p2size,p2details} = get_operand(P_FPU,false)
--004099A9    0o330 0o300           FADD ST,ST
--004099AB    0o330 0o301           FADD ST,ST(1)
--004099AD    0o330 0o302           FADD ST,ST(2)
--004099AF    0o334 0o301           FADD ST(1),ST
--004099B1    0o334 0o302           FADD ST(2),ST
--004099B3    0o334 0o352           FSUB ST(2),ST
--004099B5    0o330 0o342           FSUB ST,ST(2)
--004099A9   0o330 0o311            FMUL ST,ST(1)
--004099AB   0o330 0o310            FMUL ST,ST
--004099AD   0o334 0o311            FMUL ST(1),ST
--004099AF   0o334 0o312            FMUL ST(2),ST

                    if emitON then
                        if p1details=1 then -- st0
                            reg = p2details-1
                            xrm -= 0o010
                            xrm += reg
                            s5 &= {0o330,xrm}
                        else
                            -- eg "fsub stn" is an alias of "fsub st0,stn")
                            reg = p1details-1
if xrm=0o310 then -- (I think 034r is fsubr)
                            xrm -= 0o010
end if
                            xrm += reg
                            s5 &= {0o334,xrm}
                        end if
                    end if

                elsif p1type=P_MEM then
                    -- (an "st0," is implied)
--004099A9   0o330 0o004 24         FADD DWORD PTR SS:[ESP]
--004099AC   0o330 0o104 24 08      FADD DWORD PTR SS:[ESP+8]
--004099B0   0o334 0o104 24 08      FADD QWORD PTR SS:[ESP+8]
--004099B4   0o334 0o004 24         FADD QWORD PTR SS:[ESP]
--004099B7   0o334 0o044 24         FSUB QWORD PTR SS:[ESP]
--004099BA   0o334 0o144 24 08      FSUB QWORD PTR SS:[ESP+8]
--004099BE   0o330 0o144 24 08      FSUB DWORD PTR SS:[ESP+8]
--004099C2   0o330 0o044 24         FSUB DWORD PTR SS:[ESP]
--004099A9   0o330 0o014 #24        FMUL DWORD PTR SS:[ESP]
--004099AC   0o330 0o114 #24 08     FMUL DWORD PTR SS:[ESP+8]
--004099B0   0o334 0o114 #24 08     FMUL QWORD PTR SS:[ESP+8]
--004099B4   0o334 0o014 #24        FMUL QWORD PTR SS:[ESP]

                    {scale,idx,base,offset} = p1details
                    if p1size=4 then
                        s5 &= 0o330
                    elsif p1size=8 then
                        s5 &= 0o334
                    else
                        ?9/0
                    end if
                    emit_xrm_sib(mod, scale, idx, base, offset)
                else
                    ?9/0
                end if
end if
            elsif ttidx=T_fcomp
               or ttidx=T_fcom then
                if ttidx=T_fcomp then
                    xrm = 0o331
                    mod = 3
                elsif ttidx=T_fcom then
                    xrm = 0o321
                    mod = 2
                else
                    ?9/0
                end if
                skipSpacesAndComments()
                p1type = P_FPU  -- (no param defaults to st1)
                if line=tokline and Ch!='}' then
if 0 then -- old code 9/5/15
                    getToken()
                    if toktype!=LETTER then Aborp("unrecognised") end if
                    reg = find(ttidx,{T_st0,T_st1,T_st2,T_st3,T_st4,T_st5,T_st6,T_st7})
                    if reg=0 then Aborp("unrecognised") end if
                    xrm += (reg-2)  -- remove st1 default and add actual reg
else
                    {p1type,p1size,p1details} = get_operand(P_MEM+P_FPU,false)
                    if p1type=P_VAR then
                        -- Phix hll vars do not hold floats like this! (it is qword[ref*4])
                        Aborp("not permitted")
                    elsif p1type=P_FPU then
                        xrm += (p1details-2)    -- remove st1 default and add actual reg
                    elsif p1type=P_MEM then
--004099B1   0o334 0o034 24         FCOMP QWORD PTR SS:[ESP]
--004099B4   0o334 0o134 24 08      FCOMP QWORD PTR SS:[ESP+8]
--004099B8   0o334 0o124 24 08      FCOM QWORD PTR SS:[ESP+8]
--004099BC   0o334 0o024 24         FCOM QWORD PTR SS:[ESP]
--004099BF   0o330 0o024 24         FCOM DWORD PTR SS:[ESP]
--004099C2   0o330 0o124 24 08      FCOM DWORD PTR SS:[ESP+8]
--004099C6   0o330 0o134 24 08      FCOMP DWORD PTR SS:[ESP+8]
                        {scale,idx,base,offset} = p1details
                        if p1size=4 then
                            s5 &= 0o330
                        elsif p1size=8 then
                            s5 &= 0o334
                        else
                            ?9/0
                        end if
                        emit_xrm_sib(mod, scale, idx, base, offset)
                    else
                        ?9/0
                    end if
end if
                end if
                if emitON then
if p1type=P_FPU then
                    s5 &= {0o330,xrm}
end if
                end if
            elsif ttidx=T_fmul then
if 0 then -- old code
                {p1type,p1size,p1details} = get_operand(P_FPU,false)
                if p1type!=P_FPU then ?9/0 end if
                comma()
                {p2type,p2size,p2details} = get_operand(P_FPU,false)
                if p2type!=P_FPU then ?9/0 end if
                if p1details=1 then -- st0
                    if emitON then
                        xrm = 0o310+p2details-1
                        s5 &= {0o330,xrm}
                    end if
                else
                    if p2details!=1 then
                        Aborp("st0 expected")
                    end if
                    if emitON then
                        xrm = 0o310+p1details-1
                        s5 &= {0o334,xrm}
                    end if
                end if
else -- new code 11/5/15
-- at least one of them should be st0
                {p1type,p1size,p1details} = get_operand(P_MEM+P_FPU,false)
                if p1type=P_VAR then
                    -- Phix hll vars do not hold floats like this! (it is qword[ref*4])
                    Aborp("not permitted")
                end if
                if p1type=P_FPU then
                    -- (this does not (yet) allow eg "fsub stn" to be an alias of "fsub st0,stn")
                    comma()
                    {p2type,p2size,p2details} = get_operand(P_FPU,false)
--004099A9   0o330 0o311            FMUL ST,ST(1)
--004099AB   0o330 0o310            FMUL ST,ST
--004099AD   0o334 0o311            FMUL ST(1),ST
--004099AF   0o334 0o312            FMUL ST(2),ST

                    if emitON then
                        if p1details=1 then -- st0
                            reg = p2details-1
                            xrm = 0o310+reg
                            s5 &= {0o330,xrm}
                        else
                            if p2details!=1 then
                                Aborp("st0 expected")
                            end if
                            reg = p1details-1
                            xrm = 0o310+reg
                            s5 &= {0o334,xrm}
                        end if
                    end if

                elsif p1type=P_MEM then
                    -- (an "st0," is implied)
--004099A9   0o330 0o014 #24        FMUL DWORD PTR SS:[ESP]
--004099AC   0o330 0o114 #24 08     FMUL DWORD PTR SS:[ESP+8]
--004099B0   0o334 0o114 #24 08     FMUL QWORD PTR SS:[ESP+8]
--004099B4   0o334 0o014 #24        FMUL QWORD PTR SS:[ESP]

                    {scale,idx,base,offset} = p1details
                    if p1size=4 then
                        s5 &= 0o330
                    elsif p1size=8 then
                        s5 &= 0o334
                    else
                        ?9/0
                    end if
                    mod = 1
                    emit_xrm_sib(mod, scale, idx, base, offset)
                else
                    ?9/0
                end if
end if

            elsif ttidx=T_fxch then
                skipSpacesAndComments()
                if line=tokline and Ch!='}' then
                    getToken()
                    if toktype!=LETTER then Aborp("unrecognised") end if
                    reg = find(ttidx,{T_st0,T_st1,T_st2,T_st3,T_st4,T_st5,T_st6,T_st7})
                    if reg=0 then Aborp("unrecognised") end if
                    xrm = 0o310+(reg-1)
                else
                    -- 0o331 0o311              -- fxch [st1]
                    xrm = 0o311
                end if
                if emitON then
                    s5 &= {0o331,xrm}
                end if
            elsif ttidx=T_fprem then
                if emitON then
                    s5 &= {0o331,0o370}
                end if
            elsif ttidx=T_rep
               or ttidx=T_repe then
                if emitON then
                    s5 &= 0o363
                end if
            elsif ttidx=T_repne then
                if emitON then
                    s5 &= 0o362
                end if
            elsif ttidx=T_movsb then
                if emitON then
                    s5 &= 0o244
                end if
            elsif ttidx=T_movsd then
                skipSpacesAndComments()
                if line=tokline and Ch!='}' then
                    {p1type,p1size,p1details} = get_operand(P_XMMM,true)
                    comma()
                    if p1type=P_XMM then
                        {p2type,p2size,p2details} = get_operand(P_MEM,false)
                        if p2type!=P_MEM then ?9/0 end if
                        if emitON then
                            {scale,idx,base,offset} = p2details
                            s5 &= {0o362,0o017,0o020}
                            reg = p1details-1
                            emit_xrm_sib(reg,scale,idx,base,offset)
                        end if
                    elsif p1type=P_MEM then
                        {p2type,p2size,p2details} = get_operand(P_XMM,false)
                        if p2type!=P_XMM then ?9/0 end if
                        if emitON then
                            {scale,idx,base,offset} = p1details
                            s5 &= {0o362,0o017,0o021}
                            reg = p2details-1
                            emit_xrm_sib(reg,scale,idx,base,offset)
                        end if
                    else
                        ?9/0 -- sanity check (should never trigger)
                    end if
--NB: only MOVSD here, MOVUPS elsewhere, others not attempted:
--; 134 --  movups xmm0,[esi] -- 0x0F, #10, 0o006, //copy 16 bytes of source data
--; 135 --  movups xmm1,[esi+16] -- 0x0F, 0o020, 0o116, 0x10, //copy more 16 bytes
--; 136 --  movups xmm2,[esi+#020] -- 0x0F, 0o020, 0o126, 0x20, //copy more
--;     0F  10  r   MOVUPS  xmm         xmm/m128    Move Unaligned Packed Single-FP Values
--; F3  0F  10  r   MOVSS   xmm         xmm/m32     Move Scalar Single-FP Values
--; 66  0F  10  r   MOVUPD  xmm         xmm/m128    Move Unaligned Packed Double-FP Value
--; F2  0F  10  r   MOVSD   xmm         xmm/m64     Move Scalar Double-FP Value
--;     0F  11  r   MOVUPS  xmm/m128    xmm         Move Unaligned Packed Single-FP Values
--; F3  0F  11  r   MOVSS   xmm/m32     xmm         Move Scalar Single-FP Values
--; 66  0F  11  r   MOVUPD  xmm/m128    xmm         Move Unaligned Packed Double-FP Values
--; F2  0F  11  r   MOVSD   xmm/m64     xmm         Move Scalar Double-FP Value
--; F2  0F  11  r   MOVSD   xmm/m64     xmm         Move Scalar Double-FP Value
                else
                    if emitON then
                        s5 &= 0o245         -- mov dword[esi],[edi]; esi+/-=4; edi+/-=4
                    end if
                end if
            elsif ttidx=T_movsq then        -- 64-bit move (as per movsb, movsw, movsd)
                if emitON then
                    s5 &= #48
                    s5 &= 0o245             -- qword[edi]:=qword[esi]; esi+=8
                end if
            elsif ttidx=T_lodsd
               or ttidx=T_lodsw then
                if emitON then
                    if ttidx=T_lodsw then
                        s5 &= #66
                    end if
                    s5 &= 0o255
                end if
            elsif ttidx=T_lodsq then
                if emitON then
                    s5 &= #48
                    s5 &= 0o255
                end if
            elsif ttidx=T_stosd
               or ttidx=T_stosw then
                if emitON then
                    if ttidx=T_stosw then
                        s5 &= #66
                    end if
                    s5 &= 0o253
                end if
            elsif ttidx=T_stosq then
                if emitON then
                    s5 &= #48
                    s5 &= 0o253
                end if
            elsif ttidx=T_lodsb then
                if emitON then
                    s5 &= 0o254
                end if
            elsif ttidx=T_stosb then
                if emitON then
                    s5 &= 0o252
                end if
            elsif ttidx=T_cmpsb then
                if emitON then
                    s5 &= 0o246
                end if
            elsif ttidx=T_cmpsd then
                if emitON then
                    s5 &= 0o247
                end if
            elsif ttidx=T_cbw then  -- ax<-al
                if emitON then
                    s5 &= {#66,0o230}           -- cbw (al -> ax)
                end if
            elsif ttidx=T_cwde then
                if emitON then
                    -- 0o230                    -- cwde (ax -> eax)
                    s5 &= 0o230
                end if
            elsif ttidx=T_cwd then
                if emitON then
                    -- 0o231                    -- cdq (ax -> dx:ax)
                    s5 &= {#66,0o231}
                end if
            elsif ttidx=T_cdq then
                if emitON then
                    -- 0o231                    -- cdq (eax -> edx:eax)
                    s5 &= 0o231
                end if
            elsif ttidx=T_cqo then
                if emitON then
                    if Z64=0 then ?9/0 end if
                    -- 0o231                    -- cqo (rax -> rdx:rax)
                    s5 &= {#48,0o231}
                end if
            elsif ttidx=T_cdqe then
                -- (note: FDBG did not disassemble this correctly, but tests proved #48,#98 does the job)
                if emitON then
                    if Z64=0 then ?9/0 end if   -- added 22/5/16 (spotted in passing, untested)
                    s5 &= {#48,0o230}           -- cdqe (eax -> rax)
                end if

            elsif ttidx=T_shld
               or ttidx=T_shrd then
                op = ttidx
-- sorry, too messy atm... only supporting shld reg,reg,imm|cl for now
--              {p1type,p1size,p1details} = get_operand(P_RM)
                {p1type,p1size,p1details} = get_operand(P_REG,false)
                comma()
                {p2type,p2size,p2details} = get_operand(P_REG,false)
                if p2type!=P_REG then ?9/0 end if -- sanity check
                reg = p2details-1
                comma()
                {p2type,p1size,p2details} = get_operand(P_RI,false)
                if p2type=P_REG then
                    if p1size!=1 or p2details!=2 then ?9/0 end if   --(must be cl)
                end if
                if emitON then
                    if p2size=2 then
                        s5 &= #66
                    elsif p2size=8 then
                        s5 &= #48
                    end if
                    s5 &= #0F
                    if p2type=P_IMM then -- (p3 really)
                        if op=T_shld then
                            s5 &= #A4
                        else
                            s5 &= #AC
                        end if
                    else   -- P_REG(cl)
                        if op=T_shld then
                            s5 &= #A5
                        else
                            s5 &= #AD
                        end if
                    end if
--                  if p1type=P_REG then
                        xrm = 0o300+reg*8+p1details-1   -- 0o3sd
                        s5 &= xrm
--                  else -- p1type=P_MEM
--{scale,idx,base,offset} = p1details
--                      0o0rr?
--                  end if
                    if p2type=P_IMM then -- (p3 really)
-->!                        s5 &= p2details
                        s5 &= and_bits(p2details,#FF)
                    end if
--004099A9 >    #0F #A4 0o330 01    SHLD AX,BX,1
--004099AE  #0F #A4 0o037 01    SHLD WORD PTR DS:[EDI],BX,1
--004099B3  #0F #A5 0o330       SHLD AX,BX,CL
--004099B7  #0F #A5 0o037       SHLD WORD PTR DS:[EDI],BX,CL
--004099BB  #0F #A4 0o320 04        SHLD EAX,EDX,4

--00425000 >     0FA4D0 06      SHLD EAX,EDX,6                           ;  f06.<ModuleEntryPoint>
--00425004   0FACD0 06      SHRD EAX,EDX,6
--00425008   0FADD0         SHRD EAX,EDX,CL
--0042500B   0FA5D0         SHLD EAX,EDX,CL

--
--
--?9/0
                end if
            elsif ttidx=T_neg
               or ttidx=T_not then
                if ttidx=T_neg then
                    xrm = 0o330
                elsif ttidx=T_not then
                    xrm = 0o320
                else
                    ?9/0
                end if
                -- only doing regs for now...
                {p1type,p1size,p1details} = get_operand(P_REG,false)
                if emitON then
                    rex = 0
                    if p1size=8 then
                        rex = #48
                    end if
                    if p1type=P_REG then
                        reg = p1details-1
                        if reg>8 then
                            ?9/0 -- (erm)
--                          rex = or_bits(rex,?9/0)
                        end if
                        if rex then
                            s5 &= rex
                        end if
                        xrm += reg
                        s5 &= {0o367,xrm}
                    else
                        ?9/0 -- sanity check (should never trigger)[/placeholder for more code]
                    end if
                end if
            elsif ttidx=T_div
               or ttidx=T_idiv
               or ttidx=T_mul
               or ttidx=T_imul then
                -- DEV barely tested, if at all:
                if ttidx=T_div then
                    xrm = 0o360
                elsif ttidx=T_idiv then
                    xrm = 0o370
                elsif ttidx=T_mul then
                    xrm = 0o340
                elsif ttidx=T_imul then
                    xrm = 0o350
                else
                    ?9/0
                end if
                {p1type,p1size,p1details} = get_operand(P_REG,false)
                if emitON then
                    rex = 0
                    if p1size=8 then
                        rex = #48
                    end if
                    if p1type=P_REG then
                        reg = p1details-1
                        if reg>8 then
                            ?9/0 -- (I'm only expecting to have to deal with idiv rcx here)
--                          rex = or_bits(rex,?9/0)
                        end if
                        if rex then
                            s5 &= rex
                        end if
--                      xrm = 0o370+reg
                        xrm += reg
--26/6/19:
--; 175             div cl
--                  div ecx               ;#0043D5A5: 367361                     np 05 07 41 741      
--0043D5A5   F6F1           DIV CL
--0043D5A5   0o366 0o361            DIV CL
--                      s5 &= {0o367,xrm}
                        if p1size=1 then
                            s5 &= 0o366
                        else
                            s5 &= 0o367
                        end if
                        s5 &= xrm
                    else
                        ?9/0 -- sanity check (should never trigger)
                    end if
                end if
            elsif ttidx=T_syscall then
                if emitON then
                    -- ELF64 only:
                    if PE then ?9/0 end if
                    if X64=0 then ?9/0 end if
                    s5 &= {0o017,0o005}
                end if
            elsif ttidx=T_movups then
                {p1type,p1size,p1details} = get_operand(P_XMMM,true)
                comma()
                if p1type=P_XMM then
                    {p2type,p2size,p2details} = get_operand(P_MEM,false)
                    if p2type!=P_MEM then ?9/0 end if
                    if emitON then
                        {scale,idx,base,offset} = p2details
                        s5 &= {0o017,0o020}
                        reg = p1details-1
                        emit_xrm_sib(reg,scale,idx,base,offset)
                    end if
                elsif p1type=P_MEM then
                    {p2type,p2size,p2details} = get_operand(P_XMM,false)
                    if p2type!=P_XMM then ?9/0 end if
                    if emitON then
                        {scale,idx,base,offset} = p1details
                        s5 &= {0o017,0o021}
                        reg = p2details-1
                        emit_xrm_sib(reg,scale,idx,base,offset)
                    end if
                else
                    ?9/0 -- sanity check (should never trigger)
                end if
--NB: only MOVUPS attempted/tested:
--; 134 --  movups xmm0,[esi] -- 0x0F, #10, 0o006, //copy 16 bytes of source data
--; 135 --  movups xmm1,[esi+16] -- 0x0F, 0o020, 0o116, 0x10, //copy more 16 bytes
--; 136 --  movups xmm2,[esi+#020] -- 0x0F, 0o020, 0o126, 0x20, //copy more
--;     0F  10  r   MOVUPS  xmm         xmm/m128    Move Unaligned Packed Single-FP Values
--; F3  0F  10  r   MOVSS   xmm         xmm/m32     Move Scalar Single-FP Values
--; 66  0F  10  r   MOVUPD  xmm         xmm/m128    Move Unaligned Packed Double-FP Value
--; F2  0F  10  r   MOVSD   xmm         xmm/m64     Move Scalar Double-FP Value
--;     0F  11  r   MOVUPS  xmm/m128    xmm         Move Unaligned Packed Single-FP Values
--; F3  0F  11  r   MOVSS   xmm/m32     xmm         Move Scalar Single-FP Values
--; 66  0F  11  r   MOVUPD  xmm/m128    xmm         Move Unaligned Packed Double-FP Values
--; F2  0F  11  r   MOVSD   xmm/m64     xmm         Move Scalar Double-FP Value

            elsif ttidx=T_movntps then
                {p1type,p1size,p1details} = get_operand(P_MEM,true)
                comma()
                if p1type=P_MEM then
                    {p2type,p2size,p2details} = get_operand(P_XMM,false)
                    if p2type!=P_XMM then ?9/0 end if
                    if emitON then
                        {scale,idx,base,offset} = p1details
                        s5 &= {0o017,0o053}
                        reg = p2details-1
                        emit_xrm_sib(reg,scale,idx,base,offset)
                    end if
                else
                    ?9/0 -- sanity check (should never trigger)
                end if
--NB: only MOVNTPS attempted/tested:
--; 151 --  movntps [edi],xmm0 -- 0x0F, 0o53, 0x07, //past first 16 bytes to aligned destination address
--; 152 --  movntps [edi+#010],xmm1 -- 0x0F, 0x2B, 0x4F, 0x10, //past more
--;     0F  2B  r   MOVNTPS m128        xmm         Store Packed Single-FP Values Using Non-Temporal Hint
--; 66  0F  2B  r   MOVNTPD m128        xmm         Store Packed Double-FP Values Using Non-Temporal Hint

            elsif ttidx=T_movaps then
                {p1type,p1size,p1details} = get_operand(P_XMM,true)
                comma()
                if p1type=P_XMM then
                    {p2type,p2size,p2details} = get_operand(P_MEM,false)
                    if p2type!=P_MEM then ?9/0 end if
                    if emitON then
                        {scale,idx,base,offset} = p2details
                        s5 &= {0o017,0o050}
                        reg = p1details-1
                        emit_xrm_sib(reg,scale,idx,base,offset)
                    end if
                else
                    ?9/0 -- sanity check (should never trigger)
                end if
--NB: only MOVNTPS attempted/tested:
--; 162 --  movaps xmm0,[esi] -- 0x0F, 0o50, 0o006, //copy 128 bytes from aligned source address
--; 163 --  movaps xmm1,[esi+#010] -- 0x0F, 0o50, 0o116, 0x10, //copy more
--;     0F  28  r   MOVAPS  xmm     xmm/m128        Move Aligned Packed Single-FP Values
--; 66  0F  28  r   MOVAPD  xmm     xmm/m128        Move Aligned Packed Double-FP Values    

--/*
movdqu... (64 bit only?)
--          elsif ttidx=T_movd then
0042B9EF     F3:3E:         PREFIX REP:                              ;  Superfluous prefix
0042B9F1     0F6F0424       MOVQ MM0,QWORD PTR SS:[ESP]
--*/

            elsif ttidx=T_movd then
--00000000004012FB | 66 0F 6E 05 19 0D 00 00    | movd xmm0,dword ptr ds:[40201C]         |
--0000000000401303 | B8 00 00 00 00         | mov eax,0                               |
--0000000000401308 | 66 0F 6E 0o310             | movd xmm1,eax                           |
--000000000040130C | B8 00 00 00 00         | mov eax,0                               |
--0000000000401311 | 66 0F 6E 0o320             | movd xmm2,eax                           |
--0000000000401315 | B8 00 00 80 3F         | mov eax,3F800000                        |
--000000000040131A | 66 0F 6E 0o330             | movd xmm3,eax                           |
--                   66 0F 6E 04 24             | movd xmm0,dword[rsp]
--constant XMM = {T_xmm0,T_xmm1,T_xmm2,T_xmm3,T_xmm4,T_xmm5,T_xmm6,T_xmm7}
                {p1type,p1size,p1details} = get_operand(P_XMM,true)
                comma()
                if p1type=P_XMM then
                    {p2type,p2size,p2details} = get_operand(P_RM,false)
                    if p2size!=4 then ?9/0 end if   -- 32 bit floats only...
                    if emitON then
                        if p2type=P_MEM then
                            {scale,idx,base,offset} = p2details
                            s5 &= {0o146,0o017,0o156}
                            reg = p1details-1
                            emit_xrm_sib(reg,scale,idx,base,offset)
                        elsif p2type=P_VAR then
                            -- (Phix hll vars will not contain a valid 32-bit float!)
                            Aborp("illegal")
                        elsif p2type=P_REG then
                            reg = p1details-1               -- (xmm0..xmm7)
                            xrm = 0o300+reg*8+p2details-1   -- (+eax..esi)
                            s5 &= {0o146,0o017,0o156,xrm}
                        else
                            ?9/0 -- sanity check (should never trigger)
                        end if
                    end if
                else
                    ?9/0 -- placeholder for more code/error message
                end if
            elsif ttidx=T_prefetchnta then
                {p1type,p1size,p1details} = get_operand(P_MEM,true)
--              if not find(p1size,{0,1,4,8}) then ?9/0 end if  -- sanity check (should never trigger)
                if emitON then
                    rex = 0
                    if p1type=P_MEM then
                        {scale,idx,base,offset} = p1details
                        if rex then
                            s5 &= rex
                        end if
                        s5 &= {0o017,0o030}
                        emit_xrm_sib(0,scale,idx,base,offset)
                    else
                        ?9/0 -- sanity check (should never trigger)
                    end if
                end if -- emitON
            elsif ttidx=T_rdtsc then
                if emitON then
                    -- 0o017 0o061              -- rdtsc (EDX:EAX := Time Stamp Counter)
                    s5 &= {0o017,0o061}
                end if
            elsif ttidx=T_bswap then
                {p1type,p1size,p1details} = get_operand(P_REG,false)
                if emitON then
                    rex = 0
                    if p1size=8 then
                        rex = #48
                    end if
                    if p1type=P_REG then
                        reg = p1details-1
                        if reg>8 then
                            ?9/0 -- (erm)
--                          rex = or_bits(rex,?9/0)
                        end if
                        if rex then
                            s5 &= rex
                        end if
                        -- 0o017 0o31r          -- bswap reg (#01020304<->#04030201)
                        xrm = 0o310+reg
                        s5 &= {0o017,xrm}
                    else
                        ?9/0
                    end if
                end if
            elsif ttidx=T_bsf
               or ttidx=T_bsr then
                if ttidx=T_bsf then
                    xrm = #BC
                else
                    xrm = #BD
                end if
-->#4C 0o17 0o275 0o302 bsr r8,rdx
--      0F      BD              r       03+     D 30                            BSR     r16/32/64       r/m16/32/64                                     o..szapc        ....z...        o..s.apc                Bit Scan Reverse
--; 127             bsr     r8,rdx 
--                  bsr r10d,rax          ;#0044BE0D: 4C:017275320               np 00 401 71 111      
--; 127             bsr r8,rdx 
--                  bsr r8d,rdx           ;#0044BE0D: 4C:017275302               np 00 104 71 111      

                {p1type,p1size,p1details} = get_operand(P_REG,false)
                rex = 0
                if p1size=8 then
                    if Z64!=1 then ?9/0 end if
                    rex = #48
                end if
                reg = p1details-1
                if reg>7 then
                    rex = or_bits(rex,#44)  -- #4C
--                  rex = or_bits(rex,#41)
                    reg -= 8
                end if
                if p1type!=P_REG then ?9/0 end if -- sanity check
                if p1size!=4 
                and p1size!=8 then
                    Aborp("invalid")
                end if
                comma()
                {p2type,p2size,p2details} = get_operand(P_REG,false)
                if p2type=P_REG then
                    if emitON then
                        if p2details>8 then
                            if Z64!=1 then ?9/0 end if
                            rex = or_bits(rex,#41)
                            p2details -= 8
                        end if
                        if rex then
                            s5 &= rex
                        end if
                        s5 &= #0F
                        s5 &= xrm
--                      xrm = 0o300+(p2details-1)*8+reg -- 0o3rm
                        xrm = 0o300+reg*8+(p2details-1) -- 0o3rm
--                      s5 &= {#0F,#BD,xrm}
                        s5 &= xrm
                    end if
                else
                    ?9/0    -- placeholder?
                end if
            elsif ttidx=T_cpuid then
                if emitON then
                    -- 0o017 0o242              -- cpuid (EAX := Processor id.info etc)
                    s5 &= {0o017,0o242}
                end if
            elsif ttidx=T_sfence then
                if emitON then
                    s5 &= {0o017,0o256,0o370}
                end if
            elsif ttidx=T_fnstsw then
                -- note: Ollydbg does not understand fnstsw, and incorrectly encodes fstsw 
                --       without an fwait (#9B), which makes it an fnstsw...
                {p1type,p1size,p1details} = get_operand(P_REG,false)
                if emitON then
                    if p1type=P_REG then
                        -- only ax permitted:
                        if p1details!=1 then ?9/0 end if
                        if p1size!=2 then ?9/0 end if
                        s5 &= {0o337,0o340}
                    else
                        ?9/0 -- placeholder for more code (m2byte)
--                  else
--                      ?9/0 -- sanity check (should never trigger)
                    end if
                end if
            elsif ttidx=T_sahf then
                if emitON then
                    s5 &= 0o236                 -- sahf
                end if
            elsif ttidx=T_nop then
                if emitON then
                    -- 0o220                    -- nop (no-op)
                    s5 &= 0o220
                end if
            elsif ttidx=T_jmp_table_entry then
                skipSpacesAndComments()
                if Ch=':' then
                    {p1type,p1size,lblidx} = get_operand(P_LBL,false)
                    if p1type=P_GBL
                    or lblidx!=length(lblchain)
                    or lblchain[$]!=0 then
                        Aborp("illegal")
                    elsif p1type=P_LBL then
                        if emitON then
                            emit_quad(isAddr,0,0,0)
                            lblchain[lblidx] = length(s5)
                        end if
                    else
                        ?9/0 -- sanity check (should never trigger)
                    end if
                else
                    Aborp(":label expected")
                end if
            elsif ttidx=T_std then
                if emitON then
                    s5 &= 0o375
                end if
            elsif ttidx=T_cld then
                if emitON then
                    s5 &= 0o374
                end if
            elsif ttidx=T_int3 then
                if emitON then
                    s5 &= 0o314
                end if
            elsif ttidx=T_int then
                skipSpacesAndComments()
                {p1type,p1size,p1details} = get_operand(P_IMM,false)
                if emitON then
                    -- ELF32 only:
                    if PE then ?9/0 end if
                    if X64 then ?9/0 end if
                    -- 0o315 imm8               -- int imm8
                    s5 &= 0o315
                    s5 &= and_bits(p1details,#FF)
                end if
            elsif ttidx=T_lock then
                if emitON then
                    s5 &= 0o360
                end if
            elsif ttidx=T_xchg then
                {p1type,p1size,p1details} = get_operand(P_RM,true)
                comma()
                if p1type=P_REG then
                    {p2type,p2size,p2details} = get_operand(P_RM,true)
                else
                    {p2type,p2size,p2details} = get_operand(P_REG,false)
                end if
                if p1size and p2size and p1size!=p2size then
                    Aborp("incompatible sizes")
                end if
                p1size = or_bits(p1size,p2size)
                if p1size=0 then
                    Aborp("size qualifier rqd")
                end if
                if emitON then
                    rex = 0
                    if p1size=8 then
                        rex = #48
                    elsif p1size!=4 then
                        ?9/0    -- placeholder for more code
                    end if
                    if p2type=P_REG then
                        if p2details>8 then
                            rex = or_bits(rex,#44)
                            p2details -= 8
                        end if
                    end if
                    if p1type=P_MEM then
                        {scale,idx,base,offset} = p1details
                        if base>8 then
                            rex = or_bits(rex,#41)
                            base -= 8
                        end if
                        if rex then
                            s5 &= rex
                        end if
                        if p2type=P_REG then
                            s5 &= 0o207
                            reg = p2details-1
                            emit_xrm_sib(reg,scale,idx,base,offset)
                        else
                            ?9/0 -- should never happen
                        end if
                    elsif p1type=P_REG then
                        reg = p1details-1
                        if reg>8 then
                            rex = or_bits(rex,#41)
                            reg -= 8
                        end if
                        if p2type=P_REG then
                            if rex then
                                s5 &= rex
                            end if
                            if p1details=1 then -- eax
                                xrm = 0o220+(p2details-1)
                            elsif p2details=1 and not and_bits(rex,#04) then -- eax
                                xrm = 0o220+reg
                            else
                                s5 &= 0o207
--                              xrm = 0o300+reg*8+p2details-1       -- 0o3rm
                                xrm = 0o300+(p2details-1)*8+reg     -- 0o3rm
                            end if
                            s5 &= xrm
                        elsif p2type=P_MEM then
                            {scale,idx,base,offset} = p2details
                            if base>8 then
                                rex = or_bits(rex,#41)
                                base -= 8
                            end if
                            if rex then
                                s5 &= rex
                            end if
                            -- Note: There is no direction on xchg, so this (xchg reg,mem) will
                            --       disassemble as xchg mem,reg, since that is the same binary.
                            s5 &= 0o207
                            reg = p1details-1
                            emit_xrm_sib(reg,scale,idx,base,offset)
                        else
                            ?9/0 -- placehoder for more code
                        end if
                    else
                        ?9/0 -- placehoder for more code
                    end if
                end if

            elsif ttidx=T_cmpxchg then
                {p1type,p1size,p1details} = get_operand(P_RM,true)
                comma()
                {p2type,p2size,p2details} = get_operand(P_REG,false)
                if p1size and p2size and p1size!=p2size then
                    Aborp("incompatible sizes")
                end if
                p1size = or_bits(p1size,p2size)
                if p1size=0 then
                    Aborp("size qualifier rqd")
                end if
                if emitON then
                    if p1size!=4 then ?9/0 end if   -- placeholder for more code
                    if p1type=P_MEM then
                        {scale,idx,base,offset} = p1details
                        rex = 0
                        if p1size=8 then    -- (DEV obviously untested!)
                            rex = #48
                        end if
                        if base>8 then
                            rex = or_bits(rex,#41)
                            base -= 8
                        end if
                        if p2type=P_REG then
                            if p2details>8 then
                                rex = or_bits(rex,#44)
                                p2details -= 8
                            end if
                        end if
                        if rex then
                            s5 &= rex
                        end if
                        if p2type=P_REG then
                            s5 &= {0o017,0o261}
                            reg = p2details-1
                            emit_xrm_sib(reg,scale,idx,base,offset)
                        else
                            ?9/0 -- placehoder for more code
                        end if
                    else
                        ?9/0 -- placehoder for more code
                    end if
                end if
            elsif ttidx=T_rdrand then
--; rdrand eax      0FC7F0      (0o017 0o307 0o360)
--; rdrand ecx      0FC7F1      (0o017 0o307 0o361)
--; rdrand rax      480FC7F0    (#48 0o017 0o307 0o360)
--; rdrand dx       660FC7F2    (#66 0o017 0o307 0o362)
                {p1type,p1size,p1details} = get_operand(P_REG,false)
                if emitON then
                    rex = 0
                    if p1size=8 then
                        rex = #48
                    end if
                    if p1type=P_REG then
                        reg = p1details-1
                        if reg>8 then
                            ?9/0 -- (erm)
--                          rex = or_bits(rex,?9/0)
                        end if
                        if rex then
                            s5 &= rex
                        end if
                        if p1size=2 then
                            s5 &= #66
                        end if
                        -- 0o017 0o307 036r         -- rdrand reg
                        xrm = 0o360+reg
                        s5 &= {0o017,0o307,xrm}
                    else
                        ?9/0
                    end if
                end if


            elsif ttidx=T_frndint then
                if emitON then
                    s5 &= {0o331,0o374}
                end if
            elsif ttidx=T_fninit then
                if emitON then
                    s5 &= {0o333,0o343}
                end if
            elsif ttidx=T_fcompp then
                if emitON then
                    s5 &= {0o336,0o331}
                end if
            elsif ttidx=T_fchs then
                if emitON then
                    s5 &= {0o331,0o340}
                end if
            elsif ttidx=T_fstcw
               or ttidx=T_fnstcw
               or ttidx=T_fldcw then
                op = ttidx
                {p1type,p1size,p1details} = get_operand(P_MEM,true)
                if emitON then
                    s5 &= {0o331}
                    mod = 7
--00405B5A   . 0o331 0o55 AA144000  FLDCW WORD PTR DS:[4014AA]
                    if op=T_fldcw then
                        mod = 5
                    end if
                    if p1type=P_MEM then
                        if p1size!=2 then ?9/0 end if
                        {scale,idx,base,offset} = p1details
                        emit_xrm_sib(mod,scale,idx,base,offset)
                    elsif p1type=P_VAR then
                        {N,sType} = p1details
                        -- (treat it as a word)
                        if sType<=S_GVar2 then
                            -- 0o331 0o055 mem32        -- fldcw word[mem32]
                            -- 0o331 0o075 mem32        -- fstcw word[mem32]
                            xrm = 0o005 + mod*8 -- 0o0m5 (where m is instruction modifier)
                            s5 &= {xrm,isVar,0,0,N}
                        elsif sType=S_TVar then
                            -- 0o331 0o155 d8           -- fldcw word[ebp+d8] (d8 of 0 rqd)
                            -- 0o331 0o255 d32          -- fldcw word[ebp+d32]
                            emit_ebpN(mod,N)
                        else
                            ?9/0 -- sanity check (should never happen)
                        end if
                    else
                        ?9/0
                    end if
                end if
            else
--              if machine=64 then ?9/0 end if  -- aaa and aas and daa and das invalid (but this never supported them anyway)
                Aborp("unrecognised (missing ttidx test?)") --DEV temp
--              Aborp("unrecognised")
            end if -- ttidx/case
        end if -- Ch=':' (label definition), '@' (@@:), or not
        skipSpacesAndComments()
    end while
    if fwdchain!=0 then
        Aborp("@@: missing")
    end if
    if wasemitON and guards=1 then
        -- eg #ilASM{ [PE32] ret } will compile fine for a PE32 target, but if you try to
        --  compile it for PE64/ELF32/ELF64 it *should* throw a wobbly. If you really want
        --  no code for non-PE32 targets, #ilASM{ [PE32] ret [] } compiles cleanly. Also,
        --  #ilASM{ [32] ret [64] pop al } is the correct way to say "64bit not yet written",
        --  where "pop al" is a guard instruction (parses fine but goes bang on code emit).
        Aborp("machine-specific code missing?")
    end if

    if emitON then
--  if emitON and opAsmRqd=0? then
        b = length(s5)-illen
        s5[illen-2] = b                 -- fill in the opAsm length
    end if

    getToken()
--NESTEDFUNC:
--  if ridState!=0 then
    if ridState>1 then
--      Aborp("ridState error")     -- DEV better message (see ridState defn.) [elsewhere?]
        Aborp(ridStateError())
    end if
    MatchChar('}')

    if returnvar=-1 then        -- top_level code
        label_fixup()           -- (also called at end of DoRoutineDef)
    end if

    if SideEffects=E_none then
        -- assume ilasm does something...
        -- (avoid no side effect warnings on routines that use #ilASM)
        SideEffects = E_other
    end if
    emitON = wasemitON
--  ?s5
--?lastline
--if lastline=308 then trace(1) end if
end procedure


