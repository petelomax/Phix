--
-- p2asm.e
--  Phix: included by plist.e, also used in demo/pGUI/filedump.exw
--
with trace

--DEV BUG: ;   189      mov qword[rsi+16],21
--                      mov [rsi+16], dword 21        ;#00402BCC: 48:307106 10 15000000      vu 00 40  1  18      
--  (Yes, the rhs /is/ a dword, but the target is a qword...)
--
--  Technical note/DEV
--  The appearance or not of qword/dword/word/byte is really appallingly coded. Note that
--  eg mov [reg],reg does /not/ want a size qualifier to appear anywhere. It really needs
--  shifting into a central routine (see qualifier() for the idea) with as many control
--  flags/parameters etc as needed, and callable umpteen times with tiny penalty. 
--  As things stand, it tests and_bits(rex,#08)/dmode=64/showdword/islea/dormspacer, with
--  no real consistency or anything approaching any kind of clarity of design...
--  There is at least one particularly nasty ugly "yuk" (search for "6/9/14" below).

-- 20/08/2013 Added basic/ad-hoc 64-bit support. NOTE: this largely consisted of scanning
--            appendix B of AMD[3] and planting lots of ?9/0 in this code. Users familiar 
--            with 64-bit code may be disappointed to hear there is absolutely no support
--            (as yet) for VEX and XOP (AMD only) prefixes, not that there was ever any 
--            support for SIMD/SSE etc instructions in 32-bit mode.
-- 25/01/2014 Added a few random sse2 instructions, just enough for the fasm mandel demo.
--            (there is no equivalent code in pilasm.e as of yet)

--integer wasopcodewarning = 1  -- DEV (temp)

global integer terminal     -- fatal error; cease decode
               terminal = 0

integer machine         -- should be set to 32 or 64 by decodeinit()
integer machine_arch    -- should be set to arch_PE or arch_ELF ""

global constant arch_PE = 1,
                arch_ELF = 2

--global atom lastpush      -- for opCallOnce extra detail
-- 4/11/14:
--global atom lastmov       -- for opFrame, opCallOnce extra detail
global atom lastmov = 0     -- for opFrame, opCallOnce extra detail
                            -- (used in plist.e/disassemble())
atom lastmoveax = 0

global integer wasOpCode
               wasOpCode = 0

global integer csidx

integer dump_jump_table         -- 0=off, -1=start, 1=rest
        dump_jump_table = 0
integer jump_table_index
atom min_jt_addr                -- for use with ""

global integer r_isdata = -1
global integer r_iscode = -1

--with trace
function isPlausibleJumpTable(atom addr)
--
-- We have just seen a jmp[reg*4+imm32]. Phix emits such, followed by a jump table (ie
--  an array of machine addresses in the middle of the code), and of course we need to
--  decide whether to print the following binary as a jump table or dissassemble it as
--  normal. While the only Phix hll statement to emit said instruction is a switch, it
--  could theoretically be used in #ilASM, or we could be dissassembling a non-Phix
--  program. Since eg "switch -1000,-1001,-1002" is perfectly valid, we cannot examine
--  the valid of imm32 and decide anything based on that. Instead we must examine the 
--  following bytes (the jump table) in excruiating detail: they should be absolute 
--  addresses between addr and end of the section and the thing ends when we (exactly) 
--  hit the lowest address we met while scanning the table. There are no zeroes, since
--  a switch 1,3,5.. patches 2,4,6 to "end switch", nor are there any backward jumps 
--  (as no branch straightening occurs on jump tables).
-- I am going to be a little lax here and treat values +/-ImageBase as in range. [MAYBE]
--
integer tidx = csidx
integer c
atom v
integer wordsize
    min_jt_addr = -1
    while 1 do
        v = 0
        wordsize = 4
        if X64 then
            wordsize = 8
        end if
--      for i=1 to 4 do
        for i=1 to wordsize do
            if tidx>=length(code_section) then
                return 0
            end if
            tidx+=1
            c = and_bits(code_section[tidx],#FF)
            v += c*power(256,i-1)
        end for
--      addr += 4
        addr += wordsize
        -- erm...
--      if machine=64 and hasrm and mode=0 and rm=5 then
--          -- RIP addressing:
----            v += addr+ilen
--          v += addr
--      end if
--      if v<ImageBase+CSvaddr or v>=ImageBase+CSvaddr+CSvsize then
if newEmit then
        if r_iscode=-1 then ?9/0 end if
        if v=0 or call_func(r_iscode,{v})=0 then
            return 0
        end if
else
        if v<addr or v>=ImageBase+CSvaddr+CSvsize then
            return 0
        end if
end if
        if min_jt_addr=-1 then
            min_jt_addr = v
        elsif v<min_jt_addr then
            min_jt_addr = v
        end if
        if min_jt_addr=addr
        or min_jt_addr-ImageBase=addr then
            exit
        end if
    end while
    return 1
end function

--DEV should be moved out of this source:
--with trace
function getbyte()
    if csidx>=length(code_section) then
        terminal = 1
        return 0
    end if
    csidx+=1
    return and_bits(code_section[csidx],#FF)
end function

function nextopisnop()
integer csidx2 = csidx, byte
    while csidx2<length(code_section) do
        csidx2 += 1
        byte = and_bits(code_section[csidx2],#FF)
        if byte!=0o146 then
            if byte=0o220 then return 1 end if
            exit
        end if
    end while
    return 0
end function

constant NP=1, U=2, V=3, UV=4, VU=5, unknown=6
constant pair = {"np","u ","v ","uv","vu","??"}
constant eaxbit = #01,
         ecxbit = #02,
         edxbit = #04,
         ebxbit = #08,
--       espbit = #10,
--       ebpbit = #20,
         esibit = #40,
         edibit = #80,
         regbit = {#01,#02,#04,#08,#10,#20,#40,#80,#100,#200,#400,#800,#1000,#2000,#4000,#8000},
--                 eax,ecx,edx,ebx,esp,ebp,esi,edi
         regbit8 ={#01,#02,#04,#08,#01,#02,#04,#08,#100,#200,#400,#800,#1000,#2000,#4000,#8000},
--                  al, cl, dl, bl, ah, ch, dh, bh
         regbita ={#48,#88,#60,#A0,#40,#80,#20,#08}
--              BX+SI,BX+DI,BP+SI,BP+DI,SI,DI,BP,BX


integer pairing, modified, referenced, clocks, agiset
integer hasdisp

constant hexfmt="%02x"
--constant insfmt="%02x"
constant insfmt="%03o"

sequence sop        -- segment override prefix

object asm
sequence hex

integer c, ilen

constant ddaa={"daa","das","aaa","aas"},
         sops={#26,  #2E,  #36,  #3E,  #64,  #65},
         sopt={"ES:","CS:","SS:","DS:","FS:","GS:"},
         idpp={"inc","dec","push","pop"},
         ppba={"pushad","popad","bound","arpl"}

integer dmode       -- data (word size) mode
        dmode = 32  -- default is 32-bit
                    -- (overidden for DOS stub and by #66 prefix)
integer amode       -- addressing mode
        amode = 32  -- default is 32-bit
                    -- (overidden for DOS stub and by #67 prefix)

integer rex = 0     -- rex prefix (64-bit mode) (WRXB=8421)
-- rW = and_bits(rex,#08) -- 64-bit operand size
-- rR = and_bits(rex,#04) -- msb extension to modRM reg field
-- rX = and_bits(rex,#02) -- msb extension to sib index field
-- rB = and_bits(rex,#01) -- msb extension to modRM r/m field


integer D,W 

constant g0={"add","or","adc","sbb","and","sub","xor","cmp"}

constant o100 = #40,    -- floor(c/o100) obtains first octal digit
         o010 = #08,    -- and_bits(floor(c/o010),o007) obtains second octal digit
         o007 = #07     -- and_bits(c,o007) obtains third octal digit

integer hasrm
integer mode,reg,rm
procedure getrm()
    c = getbyte()
    mode = floor(c/o100)                -- first octal digit (0..3)
    reg = and_bits(floor(c/o010),o007)  -- second octal digit (0..7)
    rm = and_bits(c,o007)               -- third octal digit (0..7)
if machine=64 then
    if and_bits(rex,#04) then reg += 8 end if
    if mode=3 or rm!=4 then
        if and_bits(rex,#01) then rm += 8 end if
    end if
end if
    hex &= sprintf(insfmt,c)
    ilen += 1
    hasrm = 1
end procedure

constant segreg={"ES","CS","SS","DS"}
constant r8={"al","cl","dl","bl","ah","ch","dh","bh","r8l","r9l","r10l","r11l","r12l","r13l","r14l","r15l"},
--DEV    r8b={"al","cl","dl","bl","spl","bpl","sil","dil","r8b","r9b","r10b","r11b","r12b","r13b","r14b","r15b"}, -- (if there is a rex prefix? (#40 will do))
-- (r8b..r15b are exactly the same as r8l..r15l)
         r8l={"spl","bpl","sil","dil"},
         r16={"ax","cx","dx","bx","sp","bp","si","di","r8w","r9w","r10w","r11w","r12w","r13w","r14w","r15w"},
         r32={"eax","ecx","edx","ebx","esp","ebp","esi","edi","r8d","r9d","r10d","r11d","r12d","r13d","r14d","r15d"},
         r64={"rax","rcx","rdx","rbx","rsp","rbp","rsi","rdi","r8","r9","r10","r11","r12","r13","r14","r15"},
         a32={"BX+SI","BX+DI","BP+SI","BP+DI","SI","DI","BP","BX"}

function getr8(integer r)
sequence reg
    if and_bits(rex,#40) and r>=4 and r<=7 then
        reg = r8l[r-3]
        referenced = or_bits(referenced,regbit[r+1])
    else
        reg = r8[r+1]
        referenced = or_bits(referenced,regbit8[r+1])
    end if
    return reg
end function

--/*
procedure getrm()
    c = getbyte()
    mode = floor(c/o100)                -- first octal digit (0..3)
    reg = and_bits(floor(c/o010),o007)  -- second octal digit (0..7)
    rm = and_bits(c,o007)               -- third octal digit (0..7)
if machine=64 then
    if and_bits(rex,#04) then reg += 8 end if
    if mode=3 or rm!=4 then
        if and_bits(rex,#01) then rm += 8 end if
    end if
end if
    hex &= sprintf(insfmt,c)
    ilen += 1
    hasrm = 1
end procedure
--*/
procedure getreg()--integer use_r64=0)
--DEV set a flag that we need no size qualifier? (possibly too late anyway to do it here)
    if W then
--10/10/14: (put back 3/11/14, over mov eax,[rsi] line 451 pMem.e)
        if machine=64 and and_bits(rex,#08) then
--      if machine=64 and (use_r64 or and_bits(rex,#08)) then
--      if machine=64 then
--          elsif machine=64 and (dmode=64 or and_bits(rex,#08)) then   -- (DEV try me!)
--6/11/16: (untried [reg|rm+=8 may|has already have been done in getrm()?])
--if and_bits(rex,#04) then
--  asm &= r64[reg+9]
--else
--end if
            asm &= r64[reg+1]
        elsif dmode=32 then
            asm &= r32[reg+1]
        else
            asm &= r16[reg+1]
        end if
        referenced = or_bits(referenced,regbit[reg+1])
    else
--      asm &= r8[reg+1]
--      referenced = or_bits(referenced,regbit8[reg+1])
        asm &= getr8(reg)
    end if
end procedure

global atom addr
    addr=0

object v

integer useHexBase
        useHexBase=0

-- moved to pglobals 12/8/14
--global sequence knownAddr = {}
--global sequence knownNames = {}

procedure getimm(integer size, integer spacer)
-- a spacer of '[' has the ']' added automatically (and appears in hex!)
integer c,c2
object sc2, si
integer tlsbase
--object dbg

    v = 0
    hex &= ' '
    ilen += size
    for i=1 to size do
        c = getbyte()
        hex &= sprintf(hexfmt,c)
        v += c*power(256,i-1)
    end for
    if size=1 then
        if v>#7F then
            v -= #100
        end if
    elsif size=2 then
        if v>#7FFF then
            v -= #10000
        end if
    elsif size=4 then
        if v>#7FFFFFFF then
            v -= #100000000
        end if
    elsif size!=8 then
        ?9/0
    end if
--DEV might not be good for jump table handling... (and dump_jump_table=0?)
--4/9/14: (erm...) [erm,... NO!]
    if machine=64 and hasrm and mode=0 and rm=5 and find(spacer,"[j") then
--  if machine=64 
--  and ((hasrm and mode=0 and rm=5) or (hasrm=0 and mode=3 and rm=4))
--  and find(spacer,"[j") then
        -- RIP addressing:
        v += addr+ilen
        -- Technical note (15/1/14): Whenever a getimm() (or equally dorm()) is followed by another getimm(),
        --  you need to manually adjust ilen. There were quite a few places (marked 15/1/14) where I did this.
        --  (I have assumed that on non-64 a few ilen +/-k makes absolutely no difference.)
    end if
    if spacer='[' then
        if length(sop) then
            asm &= sop
            sop = ""
        end if
        if size=4 then
--DEV 3/5/08:
--          if v and v>=irange[1] and v<=irange[2] then
--              asm &= getImport(v)
--          else
----                if cfa then trace(1) addref(v) end if
            if v>#40000000 then
                c = find(and_bits(v,#3FFFFFFF)*4-8,craddr)  -- (convert ref to addr refcount)
            else
                c = find(v,craddr)
            end if
            if c then
                c2 = cridx[c]
            else
if newEmit then
--DEV
                c = 0
                c2 = 0
else
                c = floor((v-(ImageBase+DSvaddr))/4)+1          -- calculate var idx
                if c>0 and c<=length(vmap) then
                    c2 = vmap[c]
                else
                    c2 = 0
                end if
end if
            end if
            si = -1
--          if c>0 and c<=length(vmap) then
--              c2 = vmap[c]
                if not newEmit and c2>0 and c2<=length(symtab) then
                    sc2 = symtab[c2]
                    if sequence(sc2) then
                        if sc2[S_NTyp]=S_Const
                        and sc2[S_vtype]=T_integer
                        and and_bits(sc2[S_State],K_lit) then
                            si = sprintf("Const %d",sc2[S_value])
                        else
                            si = sc2[S_Name]
                            if equal(si,-1) then                    -- Unnamed
                                if and_bits(sc2[S_State],K_Fres)    -- Function result
                                and c2<length(symtab) then          -- avoid any possible ioob
                                    si = symtab[c2+1]
                                    if sequence(si)
                                    and find(si[S_NTyp],{S_Type,S_Func})
                                    and not equal(si[S_Name],-1) then
                                        si = "res:"&si[S_Name]
                                    else
                                        si = -1
                                    end if
                                end if
                                if equal(si,-1) then
                                    si = sprintf("symtab[%d]",c2)
                                end if
                            end if
                        end if
                    end if
-- 14/10/13: (for filedump)
                else
                    c = find(v,knownAddr)
                    if c then
                        si = knownNames[c]
                    end if
                end if
--          end if
--DEV:::
--          if not equal(si,-1) then
            if not atom(si) then
--?{si,asm,v}
                if dmode=16 then
                    asm &= "word"
                end if
                if length(asm)+length(si)<23 then
                    asm &= sprintf("[#%08x] (%s)",{v,si})
                else
                    asm &= '['&si&']'
                end if
            else
if machine=64 then
    if and_bits(rex,#08) or dmode=64 then
--yuk: (search for "6/9/14" below)
if length(asm)<5 or asm[-5..-1]!="qword" then
        asm &= "qword"
end if
    elsif W then
        if dmode=128 then
            asm &= "xword"
        elsif dmode=32 then
            asm &= "dword"
        else
            asm &= "word"
        end if
    else
        asm &= "byte"
    end if
end if
                asm &= sprintf("[#%08x]",v)
            end if
        elsif size=2 then
            asm &= sprintf("[#%04x]",v)
        else
            ?9/0  -- should never happen
        end if
    elsif spacer='j' then
--20/8/2013:
--      if size = 2 then ?9/0 end if
        if size = 2 then
            asm &= sprintf(" #%04x",v)
        else
if newEmit then
--  if wasopcodewarning then
--      printf(1,"\nwarning: wasOpCode setting not attempted (p2asm line 366, v=%08x)\n",{v})
--      wasopcodewarning= 0
--  end if
--trace(1)
--dbg = sprintf("%08x",v)
--DEV or should we just use knownAddr for everything?
            v += addr+ilen
--          v += ilen
--          wasOpCode = 0
--          for i=1 to length(glblname) do
----dbg = {i,sprintf("%08x",glboffset[i]),sprintf("%08x",glboffset[i]),sprintf("%08x",symtab[glblabel[i]][S_il])}
----                if v=glboffset[i]+symtab[glblabel[i]][S_il] then
--              if v=glboffset[i]+symtab[glblabel[i]][S_il]+IB+BOC then
--                  wasOpCode = i
--                  exit
--              end if
--          end for
--          v += addr+ilen
--          v += addr
--          if wasOpCode then
--          if 0 then
--              asm &= sprintf(" #%08x (:%s)",{v,glblname[wasOpCode]})
--              wasOpCode = 0
--DEV: (we might want SOD and SOC to go with BOD and BOC)
--          elsif v>=ImageBase+CSvaddr and v<ImageBase+CSvaddr+CSvsize then
--          elsif 0 then
            si = find(v,codeTable)
            if si then
                si = symtab[codeIdx[si]][S_Name]
                if atom(si) then
                    si = sprintf("??%d??",si)
                end if
                if length(asm)+length(si)<20 then
                    asm &= sprintf(" #%08x (code:%s)",{v,si})
                else
                    asm &= " code:"&si
                end if
            else
--                  asm &= sprintf(" #%08x",v)
--              end if
--          else
-- 14/10/13: (for filedump)
                si = find(v,knownAddr)
                if si then
                    si = knownNames[si]
                    if length(asm)+length(si)<20 then
                        asm &= sprintf(" #%08x (%s)",{v,si})
--DEV
if si=":%opFrame" then
    wasOpCode = opFrame
elsif si=":%opCallOnce" then
    wasOpCode = opCallOnce
else
    wasOpCode = 0
end if
                    else
                        asm &= " "&si
                    end if
                else
                    asm &= sprintf(" #%08x",v)
                end if
            end if
else -- not newEmit
            v += addr+ilen
            wasOpCode = find(v,VMep)
            if wasOpCode then
--              asm &= ' '&opNames[wasOpCode]
                asm &= sprintf(" #%08x (%s)",{v,opNames[wasOpCode]})
--20/8/2013
--          elsif v>ImageBase+CSvaddr and v<ImageBase+DSvaddr then
--          elsif v>=ImageBase+CSvaddr and v<ImageBase+DSvaddr then
            elsif v>=ImageBase+CSvaddr and v<ImageBase+CSvaddr+CSvsize then
                si = find(v,codeTable)
                if si then
                    si = symtab[codeIdx[si]][S_Name]
                    if length(asm)+length(si)<20 then
                        asm &= sprintf(" #%08x (code:%s)",{v,si})
                    else
                        asm &= " code:"&si
                    end if
                else
                    asm &= sprintf(" #%08x",v)
                end if
            else
-- 14/10/13: (for filedump)
                si = find(c,knownAddr)
                if si then
                    si = knownNames[si]
                    if length(asm)+length(si)<20 then
                        asm &= sprintf(" #%08x (%s)",{v,si})
                    else
                        asm &= " "&si
                    end if
                else
                    asm &= sprintf(" #%08x",v)
                end if
            end if
end if
--          if cfa then addref(v) end if
        end if
    elsif spacer='#' then
        if size=1 then
            asm &= sprintf("#%02x",v)
        elsif size=2 then
            asm &= sprintf("#%04x",v)
        elsif size=8 then
            asm &= sprintf("#%016x",v)
        else
            asm &= sprintf("#%08x",v)
        end if
    else
--      if cfa then addref(v) end if
        if spacer='+' and v<0 then
            asm &= sprintf("%d",v)
--      else
        elsif spacer!='+' or v then -- 21/12    DEV put this back once char tests (routine manual()) squished....
            if v>#40000000 then
                c = find(and_bits(v,#3FFFFFFF)*4-8,craddr)  -- (convert ref to addr refcount)
            else
                c = find(v,craddr)
            end if
            if size=8 then
                c2 = 0
            elsif c then
                c2 = cridx[c]
            else
if newEmit then
--DEV
                c = 0
else
                c = floor((v-(ImageBase+DSvaddr))/4)+1          -- calculate var idx
end if
                if c>0 and c<=length(vmap) then
                    c2 = vmap[c]
                else
                    c2 = 0
                end if
            end if
            si = -2
--          if c>0 and c<=length(vmap) then
--              c2 = vmap[c]
                if c2>0 and c2<=length(symtab) then
                    si = symtab[c2]
                    if si[S_NTyp]=S_Const
                    and si[S_vtype]=T_integer
                    and and_bits(si[S_State],K_lit) then
                        si = sprintf("Const %d",si[S_value])
                    else
--                      si = symtab[c2][S_Name]
                        si = si[S_Name]
                        if equal(si,-1) then
                            if c2<length(symtab) then
                                si = symtab[c2+1]
                                if sequence(si) and find(si[S_NTyp],{S_Type,S_Func}) then
                                    si = "res:"&si[S_Name]
                                else
                                    si = -1
                                end if
                            end if
                            if equal(si,-1) then
                                si = sprintf("symtab[%d]",c2)
                            end if
                        end if
                    end if
--              end if
            elsif newEmit then
                si = -1 --DEV will knock out h4 etc... [solved, I think]
            elsif v>=ImageBase+CSvaddr and v<ImageBase+CSvaddr+CSvsize then
                si = find(v,codeTable)
                if si then
                    si = "code:"&symtab[codeIdx[si]][S_Name]
                else
                    si = -1
                end if
            end if
            if not atom(si) then
                if length(asm)+length(si)<22 then
                    asm &= sprintf("%s#%08x (%s)",{spacer,v,si})
                else
                    asm &= spacer&si
                end if
            elsif newEmit=0 and (si=-1
               or (v>=ImageBase+CSvaddr and v<ImageBase+CSvaddr+CSvsize)
               or (r_isdata!=-1 and call_func(r_isdata,{v}))    -- (for filedump)
               or (useHexBase and size=4)) then -- for jump tables (added 3/4/2010) [and return addresses 22/2/2012]
                asm &= sprintf("%s#%08x",{spacer,v})
            elsif newEmit 
              and ((r_isdata!=-1 and call_func(r_isdata,{v})) or
                   (useHexBase and (size=4))) then -- for jump tables (added 3/4/2010) [and return addresses 22/2/2012]
                asm &= sprintf("%s#%08x",{spacer,v})
            elsif v=#40000000
               or (X64=1 and v=#4000000000000000) then
                asm &= spacer&"h4"
            elsif v=-128 then
                asm &= spacer&"#80"
            elsif v=-126 then
                asm &= spacer&"#82"
            elsif v=18 then
                asm &= spacer&"#12"
            else
if newEmit then
--  if wasopcodewarning then
--      printf(1,"warning: wasOpCode setting not attempted (p2asm line 506, v=%08x)\n",{v})
----        wasopcodewarning = 0
--  end if
                if v!=0 and r_iscode!=-1 and call_func(r_iscode,{v}) then
                    wasOpCode = 0
--trace(1)
--dbg = sprintf("%08x",v)
                    for i=1 to length(glblname) do
--dbg = {i,sprintf("%08x",glboffset[i]),sprintf("%08x",glboffset[i]),sprintf("%08x",symtab[glblabel[i]][S_il])}
--                      tlsbase = symtab[glblabel[i]][S_il]
                        c2 = glblabel[i]
if c2!=0 then
                        tlsbase = symtab[c2][S_il]
                        if tlsbase!=0 and v=glboffset[i]+tlsbase then
                            wasOpCode = i
                            exit
                        end if
end if
                    end for
                    if wasOpCode then
                        asm &= sprintf("%s#%08x (:%s)",{spacer,v,glblname[wasOpCode]})
                    else
                        asm &= sprintf("%s#%08x",{spacer,v})
                    end if
                    wasOpCode = 0
                else
                    asm &= sprintf("%s%d",{spacer,v})
                end if
else
                wasOpCode = find(v,VMep)
                if wasOpCode and v!=0 then
--                  asm &= spacer&opNames[wasOpCode]
                    asm &= sprintf("%s%08x (%s)",{spacer,v,opNames[wasOpCode]})
                else
                    asm &= sprintf("%s%d",{spacer,v})
                end if
end if
            end if
        end if
    end if
--DEV added 26/02/2012 (untested)
--  useHexBase = 0
end procedure

integer scale,index,base
procedure getsib()
    c = getbyte()
    scale = floor(c/o100)                   -- first octal digit (0..3)
    index = and_bits(floor(c/o010),o007)    -- second octal digit (0..7)
    base = and_bits(c,o007)                 -- third octal digit (0..7)
if machine=64 then
    if and_bits(rex,#02) then index += 8 end if
    if and_bits(rex,#01) then base += 8 end if  --DEV and clear the bit? (see getrm)
end if
    hex &= sprintf(insfmt,c)
    ilen += 1
end procedure

integer wasRetAddr
integer islea = 0
integer dormspacer = 1

integer notphix = 0
global procedure setnotphix()
-- (for filedump.exw)
    notphix = 1
end procedure

procedure dorm(integer showdword=0, integer use_r64=1)
-- cfa is 1 if parameter can be a control flow address (eg a jmp instruction!)
integer p
,wasp
object name
    if dormspacer then
        if not find(asm[length(asm)]," ,:") then asm &= ' ' end if
    end if
    if mode=3 then      -- mmm is register
--DEV use getreg(), but using rm not reg...
        if W then
-- 8/10/14:
--          if machine=64 and and_bits(rex,#08) then
-- 8/11/14 (over or ax,ax)
--          if machine=64 then
            if dmode=16 then
                asm &= r16[rm+1]
-- 23/11/14 (over and esi,-2) [and backed out, breask other stuff...]
--          elsif machine=64 then
--          elsif machine=64 and (dmode=64 or and_bits(rex,#08)) then
            elsif machine=64 and (use_r64 or dmode=64 or rex!=0) then
                asm &= r64[rm+1]
            elsif dmode=32 then
--DEV (should never trigger)
if rm>7 then
    printf(1,"p2asm line 761: check #%08x\n",addr)
                asm &= r64[rm+1]
else
                asm &= r32[rm+1]
end if
            else
------DEV??
----                asm &= sop&r16[rm+1]
----                sop=""
----DEV
----if rm>7 then
----    printf(1,"p2asm line 772: check #%08x\n",addr)
----    asm &= r64[rm+1]
----else
--              asm &= r16[rm+1]
                ?9/0
----end if
            end if
            referenced = or_bits(referenced,regbit[rm+1])
        else
--DEV
--if rm>7 then
--  printf(1,"p2asm line 783 **check #%08x\n",addr)
--  asm &= r64[rm+1]
--else
--          asm &= r8[rm+1]
--          referenced = or_bits(referenced,regbit8[rm+1])
            asm &= getr8(rm)
--end if
        end if
    else
        clocks += 1
--      agiset = 0  --done earlier
        if length(sop) then
--          if length(asm)=0 or asm[length(asm)]=' ' then
--          if find(asm[length(asm)]," ,") then
            asm &= sop
--          else
--              asm &= ' '&sop
--          end if
            sop = ""
        end if
        if mode=0
        and rm=5
        and amode!=16 then
--          asm &= ','
            getimm(4,'[')
--          asm &= ']'
            hasdisp = 1
        else
            if dmode=128 and showdword then
                asm &= "xword"
-- 15/12/14 (over result = result[1..tmp] in prntf/round())
--          elsif dmode=64 then
--14/2/17:
--          elsif dmode=64 and showdword then
            elsif machine=64 and showdword then
                asm &= "qword"
            elsif dmode=32 and showdword then
-- 3/11/14: (?)
--5/1/15 (jump tables)
--if machine=64 and amode=16 then
if machine=64 and (amode=16 or useHexBase) then
    asm &= "qword"
else
                asm &= "dword"
end if
            elsif dmode=16 then
                asm &= "word"
            end if
            if rm=4 and amode!=16 then
                getsib()
--              if index=4 and scale!=0 then
                if index=4 then
                    if scale!=0 then
                    --                  error() --DEV
--                  asm &= "?? ERROR: illegal op (s>0 on i=4) ??"
                        asm = "undefined! (s>0 on i=4)"
                        terminal = 1
                        return
-- eg ;     00 B4 E0 00500000 <=!=> add [eax+esp*8+5000],dh
-- Fasm gives error: invalid address.
-- Ollydebug (1.08b) dissassembles as add[eax+5000],dh and like fasm 
--  rejects attempts to assemble the above with "invalid indexing mode".
-- Update: Some forms could just be ignored, eg
--  CPU Disasm
--  Hex dump    Octal               Command                              
--  DB5C24 04   333 134 044 004     FISTP DWORD PTR [ESP+4]
--  DB5CE4 04   333 134 344 004     FISTP DWORD PTR [ESP+4]
--  8D4424 04   215 104 044 004     LEA EAX,[ESP+4]
--  8D4464 04   215 104 144 004     LEA EAX,[ESP+4]
--  8D44E4 04   215 104 344 004     LEA EAX,[ESP+4]
-- However, pilx86 should only emit the "classical" form (xr4 04b).
                    end if
--                  if machine=64 and and_bits(rex,#08) then
                    if machine=64 then
--                      if dmode=32 and showdword then
                        if dmode=32 and not islea and dormspacer then
                            asm &= "qword"
                        end if
                        asm &= '['&r64[base+1]
                    else
                        asm &= '['&r32[base+1]
                    end if
                    agiset = or_bits(agiset,regbit[base+1])
--              end if
--      xr4 sib        As follows:
--
--"sib" Encoding: (Scale*Index+Base)
--      0r4 sib        DS:[base + scale*index]
--      1r4 sib disp8    DS:[base + scale*index + disp8]
--      2r4 sib disp32        DS:[base + scale*index + disp32]
--  Exceptions:
----        0r4 si5 disp32        DS:[scale*index + disp32]
--      r=4,5        SS: default for ESP, EBP
----        xr4 04b        No Index (scale must be 0)
----        xr4 s4b (s>0)    Undefined!
--

--              if mode=0 and base=5 then
                elsif mode=0 and base=5 then
--DEV
if index>7 then
    printf(1,"p2asm line 884: check #%08x\n",addr)
    asm &= '['&r64[index+1]
--9/11/16: (over jump tables)
elsif machine=64 then
                    asm &= '['&r64[index+1]
else
                    asm &= '['&r32[index+1]
end if
                    agiset = or_bits(agiset,regbit[index+1])
                else
--if index=4 then trace(1) end if
--DEV
--if base>7 or index>7 then
--  printf(1,"p2asm line 897: check #%08x\n",addr)
                    if machine=64 then
--  if not and_bits(rex,#08) then
--      printf(1,"p2asm line 900: check #%08x\n",addr)
--  end if
                        asm &= '['&r64[base+1]&'+'&r64[index+1]
                    else
                        asm &= '['&r32[base+1]&'+'&r32[index+1]
                    end if
                    agiset = or_bits(agiset,regbit[base+1])
                    agiset = or_bits(agiset,regbit[index+1])
                end if
                if scale=0 then
                elsif scale=1 then
                    asm &= "*2"
                elsif scale=2 then
                    asm &= "*4"
                elsif scale=3 then
                    asm &= "*8"
                end if
                if mode=1 then  -- 8-bit displacement
                    getimm(1,'+')
                    hasdisp=1
                elsif mode=2     -- 32-bit displacement
                   or (mode=0 and base=5) then
                    getimm(4,'+')
                    hasdisp=1
                end if
                asm &= ']'
--DEV:
--asm&="ebp?"
            else
--              if machine=64 and and_bits(rex,#08) then
-- 23/11/14 (over cmp dword[esi],imm32, pHeap.e line 2284) [and backed out, breaks other stuff...]
--              if machine=64 then
--              if machine=64 and (use_r64 or dmode=64 or and_bits(rex,#08)) then
                if machine=64 and (use_r64 or dmode=64 or rex!=0) then
--if not and_bits(rex,#08) then
--  printf(1,"p2asm line 935: check #%08x\n",addr)
--end if
                    asm &= '['&r64[rm+1]
                    agiset = or_bits(agiset,regbit[rm+1])
                elsif amode=32 then
--DEV
if rm>7 then
    printf(1,"p2asm line 942: check #%08x\n",addr)
                    asm &= '['&r64[rm+1]
else
                    asm &= '['&r32[rm+1]
end if
                    agiset = or_bits(agiset,regbit[rm+1])
                else
                    if mode=0 and rm=6 then
                        getimm(2,'[')
--                      asm &= ']'
                        hasdisp=1   --DEV??
                    else
                        asm &= sop&'['&a32[rm+1]
                        sop=""
                        agiset = or_bits(agiset,regbita[rm+1])
                    end if
                end if
                if mode=1 then  -- 8-bit displacement
                    getimm(1,'+')
                    hasdisp=1
                elsif mode=2 then   -- 32-bit displacement
--23/1/15 (over mov [ebp-160],ebx,diag()/base)
--                  if amode=32 then
                    if amode=32
                    or amode=64 then
                        getimm(4,'+')
                    else
                        getimm(2,'+')
                    end if
                    hasdisp=1
                end if
                asm &= ']'
--DEV:
--asm &= "ebp2?"
if rm=5 then    -- ebp
--  if v=16 then
--EXCEPT
--  if (X64=0 and v=16) 
--  or (X64=1 and v=32) then
--  if (NEWRETSLOT=0 and ((X64=0 and v=16) or (X64=1 and v=32)))
--  or (NEWRETSLOT=1 and ((X64=0 and v=28) or (X64=1 and v=56))) then
    if (X64=0 and v=28) 
    or (X64=1 and v=56) then
--trace(1)
if notphix=0 then
        asm &= " (retaddr)"
end if
        wasRetAddr = 1
        inFrame = 0
--  elsif v=20 then
    elsif (X64=0 and v=20)
       or (X64=1 and v=40) then
        asm &= " (prevebp)"
    elsif v<=0 then
        if inFrame then
            p = inFrame
        else
            p = outFrame
        end if
wasp = p
        if p!=0 then
            p = symtab[p][S_Parm1]
if X64 then
            for i=v to -8 by 8 do
                if p=0 then exit end if
                p = symtab[p][S_Slink]
            end for
else
            for i=v to -4 by 4 do
                if p=0 then exit end if
if p>length(symtab) then exit end if
--DEV/temp (pre-newEmit problem?)
if atom(symtab[p]) then
    p = 0
    exit
end if
wasp = p
                p = symtab[p][S_Slink]
            end for
end if
        end if
        if p=0 then -- should never happen! (for Phix-generated programs!)
--01/08/2013 (see readme.txt)
--?9/0 -- if this triggers (and it proves difficult to properly fix)
     --  then you can just/temporarily revert to the line below.
--10/01/2014 (removed, at least while I'm working on (disassembling) non-phix programs)
if inFrame!=0 or outFrame!=0 then
            asm &= sprintf(" (??? %d %d)",{inFrame,outFrame})
end if
        else
if p>length(symtab) then
            name = sprintf("??!!%d>length(symtab)[=%d]",{p,length(symtab)})
else
            name = symtab[p][S_Name]
end if
            if name=-1 then
                name = sprintf("symtab[%d]",p)
            end if
            asm &= sprintf(" (%s)",{name})
        end if
    end if
end if
            end if
            referenced = or_bits(referenced,agiset)
        end if
    end if
end procedure

-- Fixed two-byte 017 instructions:
constant ftb017 = {#05,"syscall",
                   #06,"clts",
                   #08,"invd",
                   #09,"wbinvd",
                   #0B,"ud2",
                   #30,"wrmsr",
                   #31,"rdtsc",
                   #32,"rdmsr",
                   #33,"rdpmc",
                   #77,"emms",
                   #A0,"push FS",
                   #A1,"pop FS",
                   #A2,"cpuid",
                   #A8,"push GS",
                   #A9,"pop GS",
                   #AA,"rsm"},
-- Conditional jump codes:
jcc017 = {"o","no","b","ae","e","ne","be","a","s","ns","pe","po","l","ge","le","g"},
-- extensions:
ext017 = {#00,{"sldt","str","lldt","ltr","verr","verw",0,0},
              { 16,  16,     16,    16,   16,    16},
          #01,{"sgdt","sidt","lgdt","lidt","smsw",0,"lmsw","invlpg"},
              { 0,     0,     0,     0,     16,   0, 16,    0},
          #18,{"prefetchnta",0,0,0,0,0,0,0},
              {0,0,0,0,0,0,0,0},
--        #2B,{0,0,0,0,0,0,0,0},
--            {0,0,0,0,0,0,0,0},
          #C7,{0,"cmpxchg8b",0,0,0,0,"rdrand",0},
              {0,0,0,0,0,0,0,0}},

bai017 = {0,0,0,0,"bt","bts","btr","btc"},

mm606B = {"punpcklbw","punpcklwd","punpckldq","packsswb","pcmpgtb","pcmpgtw",
          "pcmpgtd","packuswb","punpckhbw","punpckhwd","punpckhdq","packssdw"},

movdq = {"movd","movq"},
bwdq = "bwdq",

rm32r32 = 501,
rm32r32i8 = 502,
rm32r32cl = 503,
--      rm32m = 505,
rm32r8 = 504,
--rm32r16 = 505,
rm8r8 = 506,
r32m = 507,
r32rm32 = 508,
r32rm8 = 509,
r32rm16 = 510,
mixA3C1 = {#A3,"bt",rm32r32,    -- #A3 = 0o243
           #A4,"shld",rm32r32i8,
           #A5,"shld",rm32r32cl,
           #AB,"bts",rm32r32,
           #AC,"shrd",rm32r32i8,
           #AD,"shrd",rm32r32cl,
           #AF,"imul",r32rm32,
           #B0,"cmpxchg",rm8r8,
           #B1,"cmpxchg",rm32r32,
           #B2,"lss",r32m,
           #B3,"btr",rm32r32,
           #B4,"lfs",r32m,
           #B5,"lgs",r32m,
           #B6,"movzx",r32rm8,
           #B7,"movzx",r32rm16,
           #BB,"btc",rm32r32,
           #BC,"bsf",r32rm32,
           #BD,"bsr",r32rm32,
           #BE,"movsx",r32rm8,
           #BF,"movsx",r32rm16,
           #C0,"xadd",rm8r8,
           #C1,"xadd",rm32r32},


pck017 = {"psrlw","psrld","psrlq",0,"pmullw",0,0,"psubusb","psubusw",0,
        "pand","paddusb","paddusw",0,"pandn",0,"psraw","psrad",0,0,
        "pmulhw",0,0,"psubsb","psubsw",0,"por","paddsb","paddsw",0,
        "pxor",0,"psllw","pslld","psllq",0,"pmaddwd",0,0,
        "psubb","psubw","psubd",0,"paddb","paddw","paddd",0}

--DEV call far not allowed in 64 bit mode?
constant mix989F = {"cwde","cdq","call far ","fwait","pushfd","popfd","sahf","lahf"}
constant sls = {"stos","lods","scas"}
constant rs = {"rol","ror","rcl","rcr","shl","shr",0,"sar"}
constant fs5 = {"fadd","fmul","fcom","fcomp","fsub","fsubr","fdiv","fdivr"}
constant f5a = {"fld",0,"fst","fstp","fldenv","fldcw","fnstenv","fnstcw"}
constant f5b = {"fld","fxch"}
constant f5c = {#D0,"fnop",#E0,"fchs",#E1,"fabs",#E4,"ftst",#E5,"fxam",
                #E8,"fld1",#E9,"fldl2t",#EA,"fldl2e",#EB,"fldpi",#EC,"fldlg2",
                #ED,"fldln2",#EE,"fldz",#F0,"f2xm1",#F1,"fyl2x",#F2,"fptan",
                #F3,"fpatan",#F4,"fxtract",#F5,"fprem1",#F6,"fdecstp",
                #F7,"fincstp",#F8,"fprem",#F9,"fyl2xp1",#FA,"fsqrt",
                #FB,"fsincos",#FC,"frndint",#FD,"fscale",#FE,"fsin",#FF,"fcos"}
constant fcm1 = {"b","e","be","u"}
constant f5d = {"fild",0,"fist","fistp",0,"fld",0,"fstp"}
constant f6d = {#E0,"feni",#E1,"fdisi",#E2,"fnclex",#E3,"fninit"}
constant f5e = {"fld",0,"fst","fstp","frstor",0,"fnsave","fnstsw"}
constant f5f = {"ffree",0,"fst","fstp","fucom","fucomp",0,0}
constant f7a = {"fild word",0,"fist word","fistp word","fbld","fild qword","fbstp","fistp qword"}
constant f7b = {"ffreep",0,0,0,"fnstsw ax","fucomip","fcomip",0}
constant i34x = {"loopnz","loopz","loop","jecxz","in al","in eax","out","out"}
constant i36x = {"lock","int1",0,0,"hlt","cmc"}
constant i36y = {"test",0,"not","neg","mul","imul","div","idiv"}
constant i37x = {"clc","stc","cli","sti","cld","std"}
--constant i37y = {"inc","dec","call","call","jmp",0,"push",0}

--DEV use getimm(size,'j') instead:
--procedure getreljmp(integer size)
--  v = 0
--  hex &= ' '
--  for i=1 to size do
--      c = getbyte()
--      hex &= sprintf(hexfmt,c)
--      v += c*power(256,i-1)
--  end for
--  ilen += size
--  if size=1 then
--      if v>#7F then
--          v -= #100
--      end if
--  elsif size=4 then
--      if v>#7FFFFFFF then
--          v -= #100000000
--      end if
--  else
--      ?9/0
--  end if
--  v += addr+ilen
--  asm &= sprintf(" #%08x",v)
----    addref(v)
--end procedure

procedure qualifier(integer large64=0)
--DEV move this down one line? (spotted in passing)
    if asm[length(asm)]!=' ' then asm &= ' ' end if
    if mode!=3 then
        if W then
            if dmode=32 then
                if machine=64 and (large64 or and_bits(rex,#08)) then
                    asm &= "qword"
                else
                    asm &= "dword"
                end if
            else
                asm &= "word"
            end if
        else
            asm &= "byte"
        end if
    end if
end procedure

procedure getaccimm()
    if W then
        if dmode=32 then
            if machine=64 and and_bits(rex,#08) then
                asm &= " rax"
            else
                asm &= " eax"
            end if
            getimm(4,',')
        else
            asm &= " ax"
            getimm(2,',')
        end if
    else
        asm &= " al"
        getimm(1,',')
    end if
end procedure

procedure unk()
    asm = "unknown"
    terminal = 1
    pairing = unknown
end procedure

procedure disasm()
integer c1,c2,c3,k, k2
--integer dm,drg,drm    -- DEV for debug
sequence repprefix, lockprefix
integer badprefix,c66,c67
string syscall

    terminal = 0
    pairing = unknown -- "??" (as initial setting)
    modified = 0
    referenced = 0
    agiset = 0
    hasdisp = 0
    hasrm = 0
--  hasimm = 0
--DEV:
    clocks = 90
--  cycle = -1      -- DEV calculate this later
--  dependency = 0  -- DEV calculate this later
----    agistall = 0        --DEV set agiset here, calculate agistall later

    c = getbyte()
--  hex = sprintf(insfmt,c)
    if machine=64 and c>=#40 and c<=#4F then
        hex = sprintf(hexfmt,c)
    else
        hex = sprintf(insfmt,c)
    end if
    ilen = 1
    -- deal with all prefixes (in any order)
    repprefix = ""
    lockprefix = ""
    sop = ""
    c66 = 0
    c67 = 0
    badprefix = 0
    while 1 do
        k = find(c,sops)
        if k then
            if length(sop) then  badprefix = 1  exit  end if
            sop = sopt[k]
        elsif machine=64
          and c>=#40
          and c<=#4F then
--          if c!=#48 then ?9/0 end if  -- (dunno about these yet)
--          amode = 32
--          dmode = 64 -- erm... (should really be testing "rex" in many places..??)
            rex = c
--trace(1)
        elsif c=#66 then    -- operand size prefix (0o146)
--27/3/16. Some compilers emit [up to] 146:146:146:220 as padding, apparently (makes sense I guess):
--if c66 then trace(1) end if
--          if c66 then  badprefix = 1  exit  end if
            if c66 and not nextopisnop() then  badprefix = 1    exit  end if
            c66 = 1
            if dmode=32 then
                dmode = 16
--          else
--              dmode = 32
            end if      
        elsif c=#67 then    -- address mode prefix
            if c67 then  badprefix = 1  exit  end if
            c67 = 1
            if amode=32 then
                amode = 16
--          else
--              amode = 32
            end if      
        elsif machine=64
          and c>=#C4
          and c<=#C5 then
--          ?9/0    -- VEX prefixes (whatever they are!)
--DEV
printf(1,"p2asm line 891: VEX prefix?: check #%08x\n",addr)
badprefix = 1
exit
        elsif machine=64
          and c=#8E then
--          ?9/0    -- XOP prefix (whatever that is!) [AMD only]
printf(1,"p2asm line 891: XOP prefix?: check #%08x\n",addr)
badprefix = 1
exit
        elsif c=#F0 then
            if length(lockprefix) then badprefix = 1 exit end if
            lockprefix = "lock "
        elsif c=#F2 then
            if length(repprefix) then badprefix = 1 exit end if
            repprefix = "repne "
            referenced = ecxbit
            modified = ecxbit
        elsif c=#F3 then
            if length(repprefix) then badprefix = 1 exit end if
            repprefix = "rep "
            referenced = ecxbit
            modified = ecxbit
        else
--DEV/SUB why not do the &sprintf(insfmt,c) here? (and the hexfmt just before the next getbyte())
            exit
        end if
        c = getbyte()
if machine=64 and c>=#40 and c<=#4F then
        hex &= ':'&sprintf(hexfmt,c)
else
        hex &= ':'&sprintf(insfmt,c)
end if
        ilen += 1
    end while
    if badprefix then
--      pairing = NP
        asm = "? duplicate/spurious prefix ? Decode aborted"
        terminal = 1
        sop = ""
    else
--if c=0o150 then trace(1) end if
        c1 = floor(c/o100)                  -- first octal digit (0..3)
        c2 = and_bits(floor(c/o010),o007)   -- second octal digit (0..7)
        c3 = and_bits(c,o007)               -- third octal digit (0..7)

        D = and_bits(c,#02)
        W = and_bits(c,#01)
        --
        -- TIP: This is (believe it or not) written to be very easy to maintain.
        --      For example suppose there is a problem with jump far/binary #EA:
        --      #EA = 0o352 (Ctrl Shift B in Edita) and then jumping around this
        --      set of conditions (Ctrl [|] in Edita) should locate the problem
        --      code very quickly (once/now that you know what c1/c2/c3 all are).
        --
--trace(1)
        if c1=0 then
            if c3<6 then
                if c2=2 or c2=3 then    -- adc and sbb only pairable in u pipe
                    pairing = U
                else
                    pairing = UV
                end if
                clocks = 1
--if c2=6 then trace(1) end if
                asm = g0[c2+1]  -- {"add","or","adc","sbb","and","sub","xor","cmp"}
                if c3<4 then    -- r/m,reg and reg,r/m modes
                    getrm()
--                  dm = mode  drg = reg  drm = rm  --DEV for debug only...
                    asm &= ' '
                    if D then   -- reg,r/m
                        getreg()
                        if c2!=7 then   -- cmp does not modify
                            modified = referenced
                        end if
                        asm &= ','
                    end if
dormspacer = 0
--                  dorm()
--6/11/16 as below..
if mode=3 then
                    dorm(0,0)
else
                    dorm()
end if
dormspacer = 1
                    if D=0 then     -- r/m,reg
                        if mode=3 then  -- reg,reg case (the other one!)
                            if c2!=7 then   -- cmp does not modify
                                modified = referenced
                            end if
                        end if
                        asm &= ','
                        getreg()
                        if mode!=3 then     -- not reg,reg case
                            if c2!=7 then   -- cmp does not modify, so one clock less than the rest
                                clocks += 1
                            end if
                        end if
                    end if
                else    -- acc,imm (4=byte, 5=word)
                    if D then
                        asm &= "? illegal (imm,acc) ?"
                        terminal = 1
                    end if
                    getaccimm()
                    referenced = or_bits(referenced,eaxbit)
                    if c2!=7 then   -- cmp does not modify
                        modified = referenced
                    end if
--                  hasimm=1
                end if
            else -- c3>=6
                if c2=1 and c3=7 then
                    -- 017 handled specially:
                    c = getbyte()
                    hex &= sprintf(insfmt,c)
                    ilen += 1
                    -- First get all fixed two-byte instructions out of the way:
                    k = find(c,ftb017)  -- {#05,"syscall",#06,"clts",#08,"invd",#09,"wbinvd",#0B,"ud2",#30,"wrmsr",
                                        --  #31,"rdtsc",#32,"rdmsr",#33,"rdpmc",#77,"emms",#A0,"push FS",
                                        --  #A1,"pop FS",#A2,"cpuid",#A8,"push GS",#A9,"pop GS",#AA,"rsm"},
                                        -- (0o005,"syscall",0o006,"clts",0o010,"invd",0o011,"wbinvd",0o013,"ud2",
                                        --  0o060,"wrmsr",0o061,"rdtsc",0o062,"rdmsr",0o063,"rdpmc",0o167,"emms",
                                        --  0o240,"push FS",0o241,"pop FS",0o242,"cpuid",0o250,"push GS",
                                        --  0o251,"pop GS",0o252,"rsm")
                    if k then
                        asm = ftb017[k+1]
if k=1 then
    if machine_arch=arch_PE then
        syscall = "*** arch_PE??? ***"
    elsif machine_arch=arch_ELF then
        syscall = "???"
        if machine=32 then
            -- (32-bit should be using int 0x80, not syscall)
        elsif machine=64 then
            if lastmoveax=0 then
                syscall = "sys_read"
            elsif lastmoveax=1 then
                syscall = "sys_write"
            elsif lastmoveax=2 then
                syscall = "sys_open"
            elsif lastmoveax=3 then
                syscall = "sys_close"
            elsif lastmoveax=4 then
                syscall = "sys_stat"
            elsif lastmoveax=5 then
                syscall = "sys_fstat"
            elsif lastmoveax=8 then
                syscall = "sys_lseek"
            elsif lastmoveax=13 then
                syscall = "sys_rt_sigaction"
            elsif lastmoveax=35 then
                syscall = "sys_nanosleep"
            elsif lastmoveax=39 then
                syscall = "sys_getpid"
            elsif lastmoveax=60 then
                syscall = "sys_exit"
            elsif lastmoveax=73 then
                syscall = "sys_flock"
            elsif lastmoveax=79 then
                syscall = "sys_getcwd"
            elsif lastmoveax=80 then
                syscall = "sys_chdir"
            elsif lastmoveax=201 then
                syscall = "sys_time"
            elsif lastmoveax=202 then
                syscall = "sys_futex"
            elsif lastmoveax=228 then
                syscall = "sys_clock_gettime"
            end if
        else
            ?9/0
        end if
    else
        ?9/0
    end if
    asm &= " ("&syscall&")"
end if
                        pairing = NP
                        if k=1 then     -- clts
                            clocks = 10
                        elsif k=11 then -- rdtsc
                            clocks = 6
                            modified = eaxbit+edxbit
                        elsif k=23 then -- cpuid
                            clocks = 14
                            modified = eaxbit+ebxbit+ecxbit+edxbit
                        end if
                    elsif c>=#80 and c<=#8F then    -- 0o200 .. 0o217
--if machine=64 then ?9/0 end if    -- RIP addressing in 64-bit mode (change getimm['j']?)
--DEV
--if machine=64 then -- seems OK 12/1/14
--printf(1,"p2asm line 1029: check #%08x\n",addr)
--end if
                        -- and conditional jumps
                        asm = 'j'&jcc017[c-#7F]
--                      getimm(4,' ')
--                      getreljmp(4)
                        getimm(4,'j')   -- 3/4
                        pairing = V
                        clocks = 1
                    elsif c>=#C8 and c<=#CF then    -- 0o310 .. 0o317
                        -- and the bswap instruction
                        if dmode=32 then
                            asm = "bswap "&r32[c-#C7]
                        else
                            asm = "bswap "&r16[c-#C7]
                        end if
                        pairing = NP
                    else
                        -- right, everything else has an r/m byte:
                        c1 = c
                        getrm()                 
--ext017 = {#00,{"sldt","str","lldt","ltr","verr","verw",0,0},
--            { 16,  16,     16,    16,   16,    16},
--        #01,{"sgdt","sidt","lgdt","lidt","smsw",0,"lmsw","invlpg"},
--            { 0,     0,     0,     0,     16,   0, 16,    0},
--        #18,{"prefetchnta",0,0,0,0,0,0,0},
--            {0,0,0,0,0,0,0,0},
--        #C7,{0,"cmpxchg8b",0,0,0,0,"rdrand",0},   (0o307)
--            {0,0,0,0,0,0,0,0}},
                        k = find(c1,ext017)
                        if k then
                            -- those with an extension code in the rm byte:
if k=3 then
    if machine=64 then ?9/0 end if  -- cmpxchg8b should be cmpxchg16b
end if
                            asm = ext017[k+1][reg+1]
                            if integer(asm) then
                                unk()
                            else
                                asm &= ' '
                                k2 = ext017[k+2][reg+1]
                                if k2=0 then
--                                  if mode=3 then
                                    if mode=3 
                                    and (k!=10 or reg!=6) then
                                        asm &= "? invalid ?"
                                        terminal = 1
                                    end if
                                else
--                          elsif k=16 then
--                              W = 0
                                    dmode = k2
                                end if
                                dorm()
                                if k=4 and reg=4 then   -- smsw
                                    pairing = NP
                                    clocks = 4
--DEV if mode=3 then?
                                    modified = referenced
                                    referenced = 0
                                elsif k=7 and reg=0 then -- prefetchnta
                                    pairing = UV        -- (guess)
                                    clocks = 1          -- (guess)
                                elsif k=10 and reg=6 then   -- rdrand
                                    pairing = NP
                                    clocks = 150
                                end if
                            end if
                        elsif c1=#02 then
                            asm = "lar "&r32[reg+1]&','
                            dorm()
                        elsif c1=#03 then
                            if dmode=32 then
                                asm = "lsl "&r32[reg+1]&','
                            elsif dmode=16 then
                                asm = "lsl "&r16[reg+1]&','
                            else
                                ?9/0
                            end if
                            dorm()
                            pairing = NP
                            clocks = 8
                            modified = referenced   --??
--DEV and machine=64?
                        elsif c1=#10            -- 0o020
                           or c1=#11 then       -- 0o021
--                          repprefix = ""
--                          referenced = ecxbit
--                          modified = ecxbit
--                          asm = "movsd xmm"&'0'+reg&','
--                          if mode=3 then
--                              asm &= "xmm"&'0'+rm
--                          else
--                              dmode = 64
--                              dorm()
--                          end if
--                          pairing = NP
--      0F  11  r   P3+ MOVUPS  xmm/m128    xmm     sse1    Move Unaligned Packed Single-FP Values
--F3    0F  11  r   P3+ MOVSS   xmm/m32     xmm     sse1    Move Scalar Single-FP Values
--66    0F  11  r   P4+ MOVUPD  xmm/m128    xmm     sse2    Move Unaligned Packed Double-FP Values
--F2    0F  11  r   P4+ MOVSD   xmm/m64     xmm     sse2    Move Scalar Double-FP Value
--                      elsif c1=#11 then
--                          if c1=#11 then
                                asm = "movups "
--                          else
--                              ?9/0
--                          end if
                            dmode = 128
                            if equal(repprefix,"rep ") then         -- F3 (0o363)
                                repprefix = ""
                                referenced = ecxbit
                                modified = ecxbit
                                asm = "movss "
                                dmode = 32                              
                            elsif equal(repprefix,"repne ") then    -- F2 (0o362)
                                repprefix = ""
                                referenced = ecxbit
                                modified = ecxbit
                                asm = "movsd "
                                dmode=64
                            elsif c66=1 then                        -- 66 (0o146)
                                asm = "movupd "
                            end if
                            D = and_bits(c1,#01)
                            if D=0 then
                                asm &= "xmm"&'0'+reg&','
                            end if
                            if mode=3 then
                                asm &= "xmm"&'0'+rm
                            else
                                dorm()
                            end if
                            if D!=0 then
                                asm &= ",xmm"&'0'+reg
                            end if
                            pairing = NP
                        elsif c1=#14 then   -- 0o024
                            asm = "unpcklps "
                            if c66=1 then
                                asm = "unpcklpd "
                                dmode = 128
                            end if
                            asm &= "xmm"&'0'+reg&','
                            if mode=3 then
                                asm &= "xmm"&'0'+rm
                            else
                                dorm()
                            end if
                            pairing = NP
                        elsif c1=#15 then   -- 0o025
--DEV merge with #14:
                            asm = "unpckhps "
                            if c66=1 then
                                asm = "unpckhpd "
                                dmode = 128
                            end if
                            asm &= "xmm"&'0'+reg&','
                            if mode=3 then
                                asm &= "xmm"&'0'+rm
                            else
                                dorm()
                            end if
                            pairing = NP
                        elsif c1=#1F then   -- 0o037
                            asm = "hint_nop "
                            dorm()
                            pairing = UV
                        elsif c1>=#20 and c1<=#23 then  -- 0o040 .. 0o043
                            if mode!=3 or dmode!=32 then
                                unk()
                            else
                                asm = "mov "
                                D = and_bits(c1,#02)
                                W = 1
                                if D=0 then
                                    dorm()
                                    modified = referenced
                                    referenced = 0
                                    asm &= ','
                                end if
                                if and_bits(c1,#01) then
                                    asm &= "DR"&'0'+reg
                                else
                                    asm &= "CR"&'0'+reg
                                    if reg=1 or reg>5 then
                                        asm &= "? invalid register ?"
                                        terminal = 1
                                    end if
                                end if
                                if D!=0 then
                                    asm &= ','
                                    dorm()
                                end if
                                pairing = NP
                                clocks = 22
                            end if
                        elsif c1=#28            -- 0o050
                           or c1=#29 then       -- 0o051
                            asm = "movaps "
                            if c66=1 then
                                asm = "movapd "
                            end if
                            dmode = 128
                            D = and_bits(c1,#01)
                            if D=0 then
                                asm &= "xmm"&'0'+reg&','
                            end if
                            if mode=3 then
                                asm &= "xmm"&'0'+rm
                            else
                                dorm()
                            end if
                            if D!=0 then
                                asm &= ", xmm"&'0'+reg
                            end if
                            pairing = NP
                        elsif c1=#2A then       -- 0o052
                            k = 4
                            if c1=#2A then
                                k = 7
                                asm = "cvtpi2ps "
                            else
                                ?9/0
                            end if
                            dmode = 64
                            if equal(repprefix,"rep ") then         -- F3 (0o363)
                                repprefix = ""
                                referenced = ecxbit
                                modified = ecxbit
                                asm[4] = 's'
                                asm[k] = 's'        -- eg "mulss "
                                dmode = 32                              
                            elsif equal(repprefix,"repne ") then    -- F2 (0o362)
                                repprefix = ""
                                referenced = ecxbit
                                modified = ecxbit
                                asm[4] = 's'
                                asm[k..k+1] = "sd"  -- eg "mulsd "
                                dmode = 32
                            elsif c66=1 then                        -- 66 (0o146)
                                asm[k+1] = 'd'      -- eg "mulpd "
                            end if
                            asm &= "xmm"&'0'+reg&','
                            if mode=3 then
                                if dmode=64 then
                                    asm &= "mm"&'0'+rm
                                else
                                    asm &= r32[rm+1]
                                end if
                            else
                                dorm()
                            end if
                            pairing = NP
                        elsif c1=#2B then   -- 0o053
                            asm = "movntps "
                            dmode = 128
                            if c66=1 then                       -- 66 (0o146)
                                asm = "movntpd "
                            end if
                            dorm()
                            asm &= ",xmm"&'0'+reg
                            pairing = NP
--;     0F  2B  r   MOVNTPS m128        xmm         Store Packed Single-FP Values Using Non-Temporal Hint
--; 66  0F  2B  r   MOVNTPD m128        xmm         Store Packed Double-FP Values Using Non-Temporal Hint

                        elsif c1=#2F then   -- 0o057
--DEV merge with #28?
                            asm = "comiss "
                            if c66=1 then
                                asm = "comisd "
                                dmode = 64
                            end if
                            asm &= "xmm"&'0'+reg&','
                            if mode=3 then
                                asm &= "xmm"&'0'+rm
                            else
                                dorm()
                            end if
                            pairing = NP
                        elsif c1>=#40 and c1<=#4F then  -- 0o100 .. 0o117
                            pairing = NP
--DEV
--if reg>7 then
--  printf(1,"p2asm line 1765: check #%08x\n",addr)
if machine=64 and and_bits(rex,#08) then
                            asm = "cmov"&jcc017[c1-#3F]&' '&r64[reg+1]&','
else
                            asm = "cmov"&jcc017[c1-#3F]&' '&r32[reg+1]&','
end if
                            dorm()
                        elsif c1=#57 then   -- 0o127
                            asm = "xorps "
                            if c66=1 then
                                asm = "xorpd "
                            end if
                            dmode = 128
                            asm &= "xmm"&'0'+reg&','
                            if mode=3 then
                                asm &= "xmm"&'0'+rm
                            else
                                dorm()
                            end if
                            pairing = NP
                        elsif c1=#51            -- 0o121
                           or c1=#58            -- 0o130
                           or c1=#59            -- 0o131
                           or c1=#5C            -- 0o134
                           or c1=#5E then       -- 0o136
                            k = 4
                            if c1=#51 then
                                k = 5
                                asm = "sqrtps "
                            elsif c1=#58 then
                                asm = "addps "
                            elsif c1=#59 then
                                asm = "mulps "
                            elsif c1=#5C then
                                asm = "subps "
                            elsif c1=#5E then
                                asm = "divps "
                            else
                                ?9/0
                            end if
                            dmode = 128
                            if equal(repprefix,"rep ") then         -- F3 (0o363)
                                repprefix = ""
                                referenced = ecxbit
                                modified = ecxbit
                                asm[k] = 's'        -- eg "mulss "
                                dmode = 32                              
                            elsif equal(repprefix,"repne ") then    -- F2 (0o362)
                                repprefix = ""
                                referenced = ecxbit
                                modified = ecxbit
                                asm[k..k+1] = "sd"  -- eg "mulsd "
--                              if c1=#5C then
                                    dmode=64
--                              end if
                            elsif c66=1 then                        -- 66 (0o146)
                                asm[k+1] = 'd'      -- eg "mulpd "
--                              if c1!=#5C then
--                                  dmode = 64
--                              end if
                            end if
                            asm &= "xmm"&'0'+reg&','
                            if mode=3 then
                                asm &= "xmm"&'0'+rm
                            else
                                dorm()
                            end if
                            pairing = NP
                        elsif c1>=#60 and c1<=#6B then  -- 0o140 .. 0o153
                            pairing = NP
                            asm = mm606B[c1-#5F]&" mm"&'0'+reg&','
                            if mode=3 then
                                asm &= "mm"&'0'+rm
                            else
                                dorm()
                            end if
                        elsif c1>=#6E and c1<=#6F then  -- 0o156 .. 0o157
                            pairing = NP
                            if c1=#6F and equal(repprefix,"rep ") then
                                repprefix = ""
                                modified = 0
                                referenced = 0
                                asm = "movdqu xmm"&'0'+reg&','
                                if mode=3 then
                                    asm &= "xmm"&'0'+rm
                                else
                                    dmode = 128
--                                  asm &= "xword"
                                    dorm()
                                end if
                            else
--10/12/16:
                                if dmode=16 then
                                    asm = movdq[c1-#6D]&" xmm"&'0'+reg&','
                                else
                                    asm = movdq[c1-#6D]&" mm"&'0'+reg&','
                                end if
                                if c1=#6F and mode=3 then
                                    asm &= "mm"&'0'+rm
                                else
-- 26/11/16: 
if dmode=16 then
    dmode = 32
end if
-- 10/12/16: (for movd xmm0,eax)
--                                  dorm()
islea = 1
                                    dorm(1,0)
islea = 0
                                end if
                            end if
                        elsif c1>=#71 and c1<=#73 then  -- 0o161 .. 0o163
                            if mode!=3
                            or (c1=#73 and reg=4) then
                                unk()
                            else
                                asm = "ps"
                                if reg=6 then
                                    asm &= 'l'
                                else
                                    asm &= 'r'
                                end if
                                if reg=4 then
                                    asm &= 'a'
                                else
                                    asm &= 'l'
                                end if
                                pairing = NP
                                asm &= bwdq[c1-#6F]&" mm"&'0'+rm        -- (leading b of bwdq not used)
                                getimm(1,',')
                            end if
                
                        elsif c1>=#74 and c1<=#76 then  -- 0o164 .. 0o166
                            asm = "pcmpeq"&bwdq[c1-#73]&" mm"&'0'+reg&','       -- trailing q of bwdq not used
                            if mode=3 then
                                asm &= "mm"&'0'+rm
                            else
                                dorm()
                            end if
                        elsif c1>=#7E and c1<=#7F then  -- 0o176 .. 0o177
                            pairing = NP
                            if c1=#7F and equal(repprefix,"rep ") then
                                repprefix = ""
                                modified = 0
                                referenced = 0
                                asm = "movdqu "
                                if mode=3 then
                                    asm &= "xmm"&'0'+rm
                                else
                                    dmode = 128
--                                  asm &= "xword"
                                    dorm()
                                end if
                                asm &= ",xmm"&'0'+reg
                            else
                                asm = movdq[c1-#7D]&' '
                                if c1=#7F and mode=3 then
                                    asm &= "mm"&'0'+rm
                                else
                                    dorm()
                                end if
                                asm &= ",mm"&'0'+reg
                            end if
                        elsif c1>=#90 and c1<=#9F then  -- 0o220 .. 0o237
                            asm = "set"&jcc017[c1-#8F]&' '
                            W = 0 -- force 8-bit operand
                            dorm()
                            modified = referenced
                            referenced = 0
                            pairing = NP
                            clocks = 2
                        elsif c1=#AE then   -- 0o256
                            if reg=7 then
                                asm = "sfence"
                            else
                                unk()
                            end if
                            pairing = NP
                            clocks = 1
--              if emitON then
--                  s5 &= {0o017,0o256,0o370}
--              end if
--; 190 --  sfence -- 0o17, 0o256, 0o370, //performs a serializing operation on all store-to-memory instructions
--  0F      AE              7       P3+                                     SFENCE                                  sse1                                            Store Fence

                        elsif c1=#BA then   -- 0o272
                            asm = bai017[reg+1] -- {0,0,0,0,"bt","bts","btr","btc"}
                            if integer(asm) then
                                unk()
                            else
                                qualifier()
                                -- 15/1/14:
                                ilen += 1
                                dorm()
                                ilen -= 1
                                getimm(1,',')
                                pairing = NP
                                if reg=4 then   -- bt
                                    clocks = 4
                                else            -- btc/r/s
                                    modified = referenced
                                    if mode=3 then
                                        clocks = 7
                                    else
                                        clocks = 8
                                    end if
                                end if
                            end if
                        elsif c1=#C6 then   -- 0o306
--DEV merge with #28?
                            asm = "shufps "
                            if c66=1 then
                                asm = "shufpd "
                            end if
                            dmode = 128
                            asm &= "xmm"&'0'+reg&','
                            if mode=3 then
                                asm &= "xmm"&'0'+rm
                            else
                                dorm()
                            end if
--                          if c1=#C6 then
                                getimm(1,',')
--                          end if
                            pairing = NP
                        else
                            k = find(c1,mixA3C1)
--mixA3C1={#A3,"bt",rm32r32,        --1     -- 0o243
--       #A4,"shld",rm32r32i8,      --4     -- 0o244
--       #A5,"shld",rm32r32cl,      --7     -- 0o245
--       #AB,"bts",rm32r32,         --10    -- 0o253
--       #AC,"shrd",rm32r32i8,      --13    -- 0o254
--       #AD,"shrd",rm32r32cl,      --16    -- 0o255
--       #AF,"imul",r32rm32,        --19    -- 0o257
--       #B0,"cmpxchg",rm8r8,       --22    -- 0o260
--       #B1,"cmpxchg",rm32r32,     --25    -- 0o261
--       #B2,"lss",r32m,            --28    -- 0o262
--       #B3,"btr",rm32r32,         --31    -- 0o263
--       #B4,"lfs",r32m,            --34    -- 0o264
--       #B5,"lgs",r32m,            --37    -- 0o265
--       #B6,"movzx",r32rm8,        --40    -- 0o266
--       #B7,"movzx",r32rm16,       --43    -- 0o267
--       #BB,"btc",rm32r32,         --46    -- 0o273
--       #BC,"bsf",r32rm32,         --49    -- 0o274
--       #BD,"bsr",r32rm32,         --52    -- 0o275
--       #BE,"movsx",r32rm8,        --55    -- 0o276
--       #BF,"movsx",r32rm16,       --58    -- 0o277
--       #C0,"xadd",rm8r8,          --61    -- 0o300
--       #C1,"xadd",rm32r32},       --64    -- 0o301

                            if k then
                                asm = ""
                                if find(c1,{#B0,#B1}) then  -- cmpxchg
                                    asm = lockprefix
                                    lockprefix = ""
                                end if
                                asm &= mixA3C1[k+1]&' '
                                pairing = NP
                                if k=1 then     -- bt
                                    pairing = NP
                                    if mode=3 then
                                        clocks = 4
                                    else
                                        clocks = 8  --+1 by dorm
                                    end if
                                elsif find(k,{40,43,55,58}) then    -- movs/zx
                                    pairing = NP
                                    clocks = 3
                                elsif k=49 then -- bsf
                                    pairing = NP
                                    if dmode=16 then
                                        clocks = 34
                                    else
                                        clocks = 42
                                    end if
                                elsif k=52 then -- bsr
--asm &= "? "
                                    pairing = NP
                                    if dmode=16 then
                                        clocks = 39
                                    else
                                        clocks = 71
                                    end if
                                end if
                                k2 = mixA3C1[k+2]
                                if k2<=rm8r8 then   -- bt,shld,shrd,bts,cmpxchg,btr,btc,bsf,bsf,xadd
                                    W = (k2!=rm8r8)
                                    -- 15/1/14:
                                    if k2=rm32r32i8 then
                                        ilen += 1
--8/11/16 (over shld in puts1.e, on 64-bit)
--                                      dorm()
                                        dorm(0,0)
                                        ilen -= 1
                                    else
                                        dorm()
                                    end if
                                    if find(k,{46,31,10}) then  -- btc, btr, bts
                                        pairing = NP
                                        if mode=3 then
                                            clocks = 7
                                        else
                                            clocks = 13
                                        end if
                                        modified = referenced
                                    end if
                                    if k2<rm32r8 then
                                        if dmode=16 then
                                            asm &= ','&r16[reg+1]
                                        elsif machine=64 and and_bits(rex,#08) then
                                            asm &= ','&r64[reg+1]
                                        else
                                            asm &= ','&r32[reg+1]
                                        end if
                                        referenced = or_bits(referenced,regbit[reg+1])
                                        if k2=rm32r32i8 then    -- shld,shrd only
                                            getimm(1,',')
                                            pairing = NP
                                            clocks = 4+(mode!=3)
                                            modified = referenced
                                        elsif k2=rm32r32cl then -- shld,shrd only
                                            asm &= ",cl"
                                            pairing = NP
                                            clocks = 4+(mode!=3)
                                            modified = referenced
                                            referenced = or_bits(referenced,ecxbit)
                                        end if
--                                  elsif k2=rm32r16 then       --DEV unused?
--                                      asm &= ','&r16[reg+1]
--trace(1)
--                                      referenced = or_bits(referenced,regbit[reg+1])
                                    else
--                                      asm &= ','&r8[reg+1]
--trace(1)
--                                      referenced = or_bits(referenced,regbit8[reg+1])
                                        asm &= ','&getr8(reg)
                                    end if
                                else                -- lss,lfs,lgs,movxz,movsx,imul
                                    if dmode=16 then
                                        asm &= r16[reg+1]
                                    else
--if machine=64 then ?9/0 end if
--DEV
--31/78/17 resurrected over bsr...
if machine=64 then
--printf(1,"p2asm line 1290: check #%08x\n",addr)
                                        asm &= r64[reg+1]
else
                                        asm &= r32[reg+1]
end if
                                    end if
                                    asm &= ','
                                    W = 1
                                    if k=19 then -- imul (reg,reg form)
                                        pairing = NP
                                        clocks = 10
                                        modified = regbit[reg+1]
                                        referenced = modified
                                    elsif k2=r32rm8 then        -- movzx,movsx only
                                        modified = regbit[reg+1]
--                                      referenced = 0
                                        pairing = NP
                                        clocks = 3
                                        W = 0
                                        if mode!=3 then
                                            asm &= "byte"
                                        end if
                                    elsif k2=r32rm16 then   -- movzx,movsx only
                                        modified = regbit[reg+1]
--                                      referenced = 0
                                        pairing = NP
                                        clocks = 3
                                        if dmode=32 then
                                            dmode = 16
                                            if mode!=3 then
                                                asm &= "word"
                                            end if
                                        else
                                            asm &= "word??invalid??"
                                            terminal = 1
                                        end if
                                    else
                                        referenced = or_bits(referenced,regbit[reg+1]) --?? erm?
                                    end if
                                    dorm()
                                    if k2=r32m and mode=3 then  -- lss,lfs,lgs only
                                        asm &= "?? invalid ??"
                                        terminal = 1
                                    end if
                                end if
                            else
                                if c1>=#D1 then
--pck017 = {"psrlw","psrld","psrlq",0,"pmullw",0,0,"psubusb","psubusw",0,
--      "pand","paddusb","paddusw",0,"pandn",0,"psraw","psrad",0,0,
--      "pmulhw",0,0,"psubsb","psubsw",0,"por","paddsb","paddsw",0,
--      "pxor",0,"psllw","pslld","psllq",0,"pmaddwd",0,0,
--      "psubb","psubw","psubd",0,"paddb","paddw","paddd",0}
                                    asm = pck017[c1-#D0]
                                    if integer(asm) then
                                        unk()
                                    else
                                        pairing = NP
                                        asm &= " mm"&'0'+reg&','
                                        if mode=3 then
                                            asm &= "mm"&'0'+rm
                                        else
                                            dorm()
                                        end if
                                    end if
                                else
                                    unk()
                                end if
                            end if
                        end if
                    end if

                elsif c2<4 then
if machine=64 then
--  if c2!=1 then ?9/0 end if   -- pop ds/es/ss invalid
--DEV
    if c2!=1 then
        printf(1,"p2asm line 2184: check #%08x\n",addr)
    end if
--  if c3=6 then ?9/0 end if    -- push cs/ds/es/ss invalid
    if c3=6 then
        printf(1,"p2asm line 2188: check #%08x\n",addr)
    end if
end if
                    if c3=6 then
                        asm = "push "
                        clocks = 1
                    else
                        asm = "pop "
                        clocks = 3
                    end if
                    asm &= segreg[c2+1]
                    pairing = NP
                else
--                  if machine=64 then ?9/0 end if  -- aaa and aas and daa and das invalid
--DEV
if machine=64 then
    printf(1,"p2asm line 2204: check #%08x\n",addr)
end if
                    asm = ddaa[c2-3]
                    pairing = NP
                end if
            end if
        elsif c1=1 then
            if c2<4 then
if c2<=1 then
    if machine=64 then ?9/0 end if  -- 0o100 .. 0o117 used as REX prefix
end if
--if machine=64 then ?9/0 end if    -- default operand size is 64 bits
--if machine=64 then
--printf(1,"p2asm line 1417: check #%08x\n",addr)
--end if
                -- {"inc","dec","push","pop"}
--6/11/16:
                if machine=64 and and_bits(rex,#01) then
                    c3 += 8
                end if
                if machine=64 then
--                  if dmode!=32 then ?9/0 end if
                    if dmode!=32 then
    printf(1,"p2asm line 2227: check #%08x\n",addr)
                    end if
--if not and_bits(rex,#08) then
--  printf(1,"p2asm line 2230: check #%08x\n",addr)
--end if
                    asm = idpp[c2+1]&' '&r64[c3+1]
                elsif dmode=32 then
                    asm = idpp[c2+1]&' '&r32[c3+1]
                else
                    asm = idpp[c2+1]&' '&r16[c3+1]
                end if
--if machine=64 then
--  asm &= " *?!?"
--end if
                pairing = UV
                clocks = 1
                if c2=2 then    -- push
                    referenced = or_bits(referenced,regbit[c3+1])
                elsif c2=3 then -- pop
                    modified = or_bits(modified,regbit[c3+1])
                else
                    referenced = or_bits(referenced,regbit[c3+1])
                    modified = or_bits(modified,regbit[c3+1])
                end if
            elsif c2=4 then
                if c3>3 then
                    asm = "? spurious prefix ?"
                    terminal = 1
                else
--if machine=64 then ?9/0 end if -- arpl is movsxd, bound, popad invalid
--DEV
                    if machine=64 then
if not and_bits(rex,#08) then
    printf(1,"p2asm line 2260: check #%08x\n",addr)
end if
                        if c3=3 then
                            pairing = NP
                            asm = "movsxd"
                            c3 = 4
                        else
printf(1,"p2asm line 2267: check #%08x\n",addr)
                            asm = "???"
                        end if
                    else
                        asm = ppba[c3+1]    -- {"pushad","popad","bound","arpl"}
                        pairing = NP
                    end if
                    if c3=2 then    -- bound
                        getrm()
                        asm &= ' '&r32[reg+1]&','
                        dorm()
                        if mode=3 then
                            asm &= "? invalid ?"
                            terminal = 1
                        end if
                    elsif c3=3 then     -- arpl
                        getrm()
                        asm &= ' '
                        dorm()
                        if dmode=32 then
--DEV
if machine=64 and reg>7 then
    printf(1,"p2asm line 2289: check #%08x\n",addr)
    asm &= ','&r64[reg+1]
else
                            asm &= ','&r32[reg+1]
end if
                        else
--DEV
if machine=64 and reg>7 then
    printf(1,"p2asm line 2297: check #%08x\n",addr)
    asm &= ','&r64[reg+1]
else
                            asm &= ','&r16[reg+1]
end if
                        end if
                    elsif c3=4 then
                        getrm()
                        asm &= ' '&r64[reg+1]&','
                        rex = 0
                        dorm()
                    elsif c3=0 then     -- pushad
                        referenced = #FF
                        pairing = NP
                        clocks = 5
                    else                -- popad
                        modified = #FF
                        pairing = NP
                        clocks = 5
                    end if
--if machine=64 then
--  asm &= " *?!?"
--end if
                end if
            elsif c2=5 then
                if c3<=3 then
                    if W then   -- c3 = 1,3
                        getrm()
                        pairing = NP
                        clocks = 10
                        modified = regbit[reg+1]
                        referenced = modified
                        if dmode=16 then
--if machine=64 and reg>7 then
--  printf(1,"p2asm line 2331: check #%08x\n",addr)
--                          asm = "imul "&r64[reg+1]&','
--else
                            asm = "imul "&r16[reg+1]&','
--end if
                        else
--if machine=64 and reg>7 then
--  printf(1,"p2asm line 2338: check #%08x\n",addr)
                            if machine=64 and and_bits(rex,#08) then
                                asm = "imul "&r64[reg+1]&','
                            else
                                asm = "imul "&r32[reg+1]&','
                            end if
                        end if
                        -- 15/1/14:
                        if c3=1 then
                            if dmode=16 then
                                k = 2
                            else
                                k = 4
                            end if
                        else
                            k = 1
                        end if
                        ilen += k
                        dorm()
                        ilen -= k
                        if c3=1 then
                            if dmode=16 then
                                getimm(2,',')
                            else
                                getimm(4,',')
                            end if
                        else --c3=3
                            getimm(1,',')
                        end if
                    else        -- c3 = 0,2
--trace(1)
--if machine=64 then ?9/0 end if    -- default operand size is 64 bits
--DEV
--if machine=64 then -- seems OK 10/1/14
--  printf(1,"p2asm line 2372: check #%08x\n",addr)
--end if
                        asm = "push"
                        if c3=0 then
                            getimm(4,' ')
                        else -- c3 = 2
                            getimm(1,' ')
                        end if
--                      lastpush = v
                        pairing = UV
                        clocks = 1
                    end if
                else
                    asm = repprefix
                    repprefix = ""
                    if c3=4 then
                        asm &= "ins byte[edi],dx"
                    elsif c3=5 then
                        if dmode=16 then
                            asm &= "ins word[edi],dx"
                        else
                            asm &= "ins dword[edi],dx"
                        end if
                    elsif c3=6 then
                        asm &= "outs dx,byte[esi]"
                    elsif c3=7 then
                        if dmode=16 then
                            asm &= "outs dx,word[esi]"
                        else
                            asm &= "outs dx,dword[esi]"
                        end if
                    end if
                    pairing = NP
                end if
            else
--if machine=64 then ?9/0 end if    -- RIP addressing in 64-bit mode (change getimm['j']?)
--if machine=64 then 
--  printf(1,"p2asm line 2409: check #%08x\n",addr)
--  if getc(0) then end if
--end if
                asm = 'j'&jcc017[c-#6F]
--              getimm(1,' ')
--              getreljmp(1)
                getimm(1,'j')   -- 3/4
                pairing = V
                clocks = 1
            end if
        elsif c1=2 then
            if c2=0 then
                if c3<=3 then
--trace(1)
                    getrm()
                    if reg=2 or reg=3 then  -- adc and sbb only pairable in u pipe
                        pairing = U
                    else
                        pairing = UV
                    end if
                    clocks = 1
--DEV
if machine=64 and reg>7 then
    printf(1,"p2asm line 2432: check #%08x\n",addr)
    asm = "??ERM??"
else
                    asm = lockprefix
                    lockprefix = ""
                    asm &= g0[reg+1]        -- {"add","or","adc","sbb","and","sub","xor","cmp"}
end if
if dmode!=16 then -- added 27/8/14
                    qualifier()
                    dormspacer = 0
--else
--                  dormspacer = 1
end if
                    -- 15/1/14:
                    if W and D=0 then
                        if dmode=32 then
                            k = 4
                        else
                            k = 2
                        end if
                    else
                        k = 1
                    end if
                    ilen += k
--dormspacer = 0
--23/11/14:
--                  dorm()
--2/12/14: (over pHeap.e/2887: cmp byte[rdx-1],0x80)
--                  dorm(0,0)
--28/12/14: (over cmp rcx,1 vs cmp ecx,1)
if mode=3 then
                    dorm(0,0)
else
                    dorm()
end if
dormspacer = 1
                    ilen -= k
                    if mode=3 then
                        if reg!=7 then  -- cmp does not modify
                            modified = referenced
                        end if
                    else
                        if reg!=7 then  -- cmp does not modify
                            clocks += 1
                        end if
                    end if
                    if W and D=0 then
                        if dmode=32 then
                            getimm(4,',')
                        else
                            getimm(2,',')
                        end if
                    else
                        getimm(1,',')
                    end if
                    if hasdisp then
--                      pairing = NP
                        pairing = U
                    end if
                else
--trace(1)
                    if c3<=5 then
                        pairing = UV
                        asm = "test "
                        clocks = 1
                    else
                        pairing = NP
--27/3/16:
--                      asm = "xchg "
                        asm = lockprefix & "xchg "
                        lockprefix = ""
                    end if
                    getrm()
--6/11/16:
--                  dorm()
if mode=3 then
                    dorm(0,0)
else
                    dorm()
end if
                    asm &= ','
-->6/11/16: use_r64 as per dorm.. [NO!!]
--                  getreg(1)
                    getreg()
                    if c3>5 then    -- xhcg
                        clocks = 3
                        if mode=3 then
                            modified = referenced
                        else
                            modified = regbit[reg+1]
                        end if
                    end if
                end if
            elsif c2=1 then
                getrm()
                if c3<4 then
--trace(1)
                    asm = "mov "
                    if D=0 then
dormspacer = 0
--29/12/14 (over mov edx,eax / mov rdx,rax, [may need mode=3])
--                      dorm()
--6/11/16 as suggested:
if mode=3 then
                        dorm(0,0)
else
                        dorm()
end if
dormspacer = 1
                        if mode=3 then
                            modified = referenced
                            referenced = 0
                        end if
                        asm &= ','
                    end if
                    getreg()
                    if D then
                        modified = referenced
                        referenced = 0
                        asm &= ','
dormspacer = 0
if mode=3 then
                        dorm(0,0)
else
                        dorm()
end if
dormspacer = 1
                    end if
                    pairing = UV
                    clocks = 1
                elsif c3=5 then
--trace(1)
                    asm = "lea "
                    getreg()
                    modified = referenced
                    referenced = 0
                    asm &= ','
islea = 1
                    dorm()
islea = 0
                    if mode=3 then
                        asm &= "? illegal addressing mode ?"
                        terminal = 1
                    end if
                    pairing = UV
                    clocks = 1
                elsif c3=7 then
--if machine=64 then ?9/0 end if    -- default operand size is 64 bits
--DEV (seems fine 6/9/14)
--if machine=64 then
--  printf(1,"p2asm line 2582: check #%08x\n",addr)
--end if
                    if reg=0 then
--if rm!=5 then
--  trace(1)
--end if
                        asm = "pop "
                        dorm(1)
                        pairing = NP
                        clocks = 3
                    else
                        unk()
                    end if
                else
                    asm = "mov "
                    --DEV force 16-bit mode?
                    W = 1
                    dmode = 16
                    if D=0 then
                        dorm()
                        asm &= ','
                    end if
--              if length(sop) then
--                  asm &= sop[1..2]
--              else
----                    asm &= "? missing prefix ?"
--                  asm &= "ES"
--              end if
                    if reg>=6 then
                        asm &= "? invalid segment register ?"
                        terminal = 1
                    else
                        asm &= sopt[reg+1][1..2]
                    end if
                    if D then
                        asm &= ','
                        dorm()
                    end if
                    pairing = NP
                    clocks = 12
                end if
            elsif c2=2 then
                if c=#90 then
                    asm = "nop"
                    pairing = UV
                    clocks = 1
                else
                    k = c-#8F
                    if dmode=16 then
                        asm = "xchg ax,"&r16[k]
                    elsif machine=64 and and_bits(rex,#08) then
--                  elsif machine=64 and (dmode=64 or and_bits(rex,#08)) then --(dev try me!)
                        asm = "xchg rax,"&r64[k]
                    else
                        asm = "xchg eax,"&r32[k]
                    end if
                    referenced = eaxbit + regbit[k]
                    modified = referenced
                    pairing = NP
                    clocks = 2
                end if
            elsif c2=3 then
                k = c-#97
                asm = mix989F[k]    -- {"cwde","cdq","call","fwait","pushfd","popfd","sahf","lahf"}
if k=6 or k=5 then
--  if machine=64 then ?9/0 end if  -- popfd should be popfq, pushfd should be pushfq
--DEV
    -- note that fdbg won't assemble pushfd, but will assemble pushfq, and immediately disassemble it as pushfd.
    if machine=64 then
--      printf(1,"p2asm line 2651: check #%08x\n",addr)
        asm[$] = 'q'
    end if
end if
                if k=1 then
                    if machine=64 and and_bits(rex,#08) then    -- (added 3/11/14)
                        asm = "cdqe"
                    elsif dmode=16 then --cwde/cbw ""
                        asm = "cbw"
                    end if
                    pairing = NP
                    clocks = 3
                    referenced = eaxbit     -- cwde is ax->eax
                    modified = eaxbit       -- cbw is al->ax
                elsif k=2 then
                    if dmode=16 then    -- cdq/cwd distinguished by prefix
                        asm = "cwd"
                    elsif machine=64 then
                        asm = "cqo"
                    end if
                    pairing = NP
                    clocks = 2
                    referenced = eaxbit     -- cdq is eax->edx:eax
                    modified = edxbit       -- cwd is ax->dx:ax
                elsif k=3 then  -- call imm.
--if machine=64 then ?9/0 end if -- invalid in 64-bit mode
--DEV
if machine=64 then
    printf(1,"p2asm line 2679: check #%08x\n",addr)
end if

--trace(1)
--                  getimm(4,' ')
--2/10/13:
--if 1 then
                    getimm(4,'#')
                    getimm(2,'#')
                    k = length(asm)
                    asm = asm[1..k-13]&asm[k-3..k]&':'&asm[k-12..k-5]
                    pairing = NP
--else
--                  getimm(4,'j')
--                  getimm(2,'j')
--                  k = length(asm)
--                  asm = asm[1..k-13]&asm[k-3..k]&':'&asm[k-12..k-5]
--                  -- NB This instruction is not supported (pairing left as ??)
--end if
                elsif k=4 then  -- fwait
                    pairing = NP
                    clocks = 1
                elsif k=5 then  -- pushfd
                    pairing = NP
                    clocks = 9
                elsif k=6 then  -- popfd
                    pairing = NP
                    clocks = 6
                elsif k=7 then -- sahf
                    referenced = eaxbit
                    pairing = NP
                    clocks = 2
                elsif k=8 then -- lahf
                    modified = eaxbit
                    pairing = NP
                    clocks = 2
                end if
            elsif c2=4 then
                if c3<4 then
                    asm = "mov"
                    pairing = UV
                    clocks = 1
                    if D then
                        asm &= ' '
                        getimm(4,'[')
                        referenced = eaxbit
                    end if
                    if W then
                        if dmode=32 then
                            if D then
                                asm &= ",eax"
                            else
                                asm &= " eax,"
                            end if
                        else
                            if D then
                                asm &= ",ax"
                            else
                                asm &= " ax,"
                            end if
                        end if
                    else
                        if D then
                            asm &= ",al"
                        else
                            asm &= " al,"
                        end if
                    end if
                    if D=0 then
                        modified = eaxbit
                        getimm(4,'[')
                    end if
                else
                    asm = repprefix
                    repprefix = ""
                    if D then
                        if length(asm)=4 then
                            -- replace rep with repe
                            asm = "repe "
                        end if
                        asm &= "cmps"
                        clocks = 5
                    else
                        asm &= "movs"
                        clocks = 4
                    end if
                    pairing = NP
                    agiset = esibit+edibit
                    referenced = or_bits(referenced,agiset)
                    modified = referenced
--if machine=64 then ?9/0 end if -- cmpsd should be cmpsq, movsd should be movsq
--DEV
                    if machine=64 and rex!=0 then
                        if rex=#48 then
                            asm &= 'q'
                        else
--                          ?9/0
    printf(1,"p2asm line 2776: check #%08x\n",addr)
                            asm &= '?'
                        end if
                    elsif W then
                        if dmode=32 then
                            asm &= 'd'
                        else
                            asm &= 'w'
                        end if
                    else
                        asm &= 'b'
                    end if
-- (replaced 5/9/14):
--                  if W then
--                      asm &= 'd'
--                  else
--                      asm &= 'b'
--                  end if
                end if

            elsif c2=5 then
                if c3<2 then
                    asm = "test"
                    pairing = UV
                    referenced = eaxbit
                    clocks = 1
                    getaccimm()
                else
                    asm = repprefix
                    repprefix = ""
                    k = floor(c/2)-#54
                    if k=3 and length(asm)=4 then
                        -- replace rep with repe
                        asm = "repe "
                    end if
                    asm &= sls[k]       -- {"stos","lods","scas"}
--if machine=64 then ?9/0 end if    -- scasd should be scasq
--DEV
                    if machine=64 and rex!=0 then
                        if rex=#48 then
                            asm &= 'q'
                        else
--                          ?9/0
    printf(1,"p2asm line 2819: check #%08x\n",addr)
                            asm &= '?'
                        end if
                    elsif W then
                        if dmode=32 then
                            asm &= 'd'
                        else
                            asm &= 'w'
                        end if
                    else
                        asm &= 'b'
                    end if
                    pairing = NP
                    if k=2 then             -- lodsb/w/d
                        clocks = 2

                        referenced = esibit
                        modified = esibit+eaxbit
                        agiset = esibit
                    else
                        if k=1 then         -- stosb/w/d
                            clocks = 3
                        else                -- scasb/w/d
                            clocks = 4
                        end if
                        referenced = or_bits(referenced,edibit+eaxbit)
                        modified = or_bits(modified,edibit)
                        agiset = edibit
                    end if
                end if
            else
--trace(1)
                asm = "mov "
                pairing = UV
                clocks = 1
                if c2=6 then
--                  asm &= r8[c3+1]
                    asm &= getr8(c3)
                    getimm(1,',')
--                  modified = regbit8[c3+1]
                    modified = referenced
                    referenced = 0
                else --c2=7
                    if machine=64 and and_bits(rex,#01) then
                        c3 += 8
                    end if
                    if machine=64 and and_bits(rex,#08) then
                        asm &= r64[c3+1]
                        getimm(8,',')
-- 23/4/15:
if c3=2 or c3=7 then -- rdx or rdi
                        lastmov = v
elsif c3=0 then
                        lastmoveax = v
end if
                    elsif dmode=32 then
                        asm &= r32[c3+1]
                        getimm(4,',')
--if v=502 then trace(1) end if
-- 23/4/15:
if c3=2 or c3=7 then -- edx or edi
                        lastmov = v
elsif c3=0 then
                        lastmoveax = v
end if
                    else
                        asm &= r16[c3+1]
                        getimm(2,',')
                    end if
                    modified = regbit[c3+1]
                end if
            end if
        else -- c1=3
            if (c2=0 and c3<2)
            or (c2=2 and c3<4) then
                pairing = U
                getrm()
--DEV
if machine=64 and reg>7 then
    printf(1,"p2asm 1922: check #%08x\n",addr)
    asm = "???ERM??"
else
                asm = rs[reg+1] -- {"rol","ror","rcl","rcr","shl","shr",0,"sar"}
end if
                if integer(asm) then -- unknown (reg=6)
                    unk()
                else
                    if remainder(c3,2)=0 then
                        dmode = 8
                    end if
--                  qualifier()
                    if mode!=3 then
                        if dmode=8 then
                            asm &= " byte "
                        elsif dmode=16 then
                            asm &= " word "
                        else
                            asm &= " dword "
                        end if
                    else
                        asm &= ' '
                    end if
                    -- 15/1/14:
                    if c2=0 then
                        ilen += 1
--31/12/14 (over shl ecx,2)
--                      dorm()
if mode=3 then
                        dorm(0,0)
else
                        dorm()
end if
                        ilen -= 1
                    else
--                      dorm()
if mode=3 then
                        dorm(0,0)
else
                        dorm()
end if
                    end if
                    if mode=3 then
                        clocks = 1
                        modified = referenced
                    else
                        clocks = 3
                    end if
                    if c2=0 then
--DEV test for shift constant out of bounds?
                        getimm(1,',')
                        if reg<4 then       -- rotate xx,imm ops not pairable (shifts are)
                            pairing = NP
                            if reg>1 then   -- rcl,rcr
                                if mode=3 then
                                    clocks = 8
                                else
                                    clocks = 10
                                end if
                            end if
                        end if
                    elsif c3<2 then
                        asm &= ",1"
                    else
                        asm &= ",cl"
                        referenced = or_bits(referenced,ecxbit)
                        clocks = 4
                        if reg>1 and reg<4 then -- rcl, rcr
                            if mode=3 then
                                clocks = 7
                            else
                                clocks = 9
                            end if
                        end if
                        pairing = NP
                    end if
                end if
            elsif c2=0 then
                if c3=2 then
                    asm = "ret"
                    getimm(2,' ')
                    pairing = NP
                    clocks = 3
                elsif c3=3 then
                    asm = "ret"
                    pairing = NP
                    clocks = 2
                elsif c3<6 then
--if machine=64 then ?9/0 end if    -- invalid in 64-bit mode
--DEV
if machine=64 then
    printf(1,"p2asm line 2989: check #%08x\n",addr)
end if
                    getrm()
                    if c3=4 then
                        asm = "les "
                        W = 1
                    else
                        asm = "lds "
                    end if
                    asm &= r32[reg+1]&','
                    dorm()
                    if mode=3 then
                        asm &= "? illegal address mode ?"
                        terminal = 1
                    end if
                    pairing = NP
                else
--trace(1)
                    getrm()
                    if reg=0 then
                        asm = "mov "
                        wasRetAddr = 0
                        --15/1/14:
                        if W then
                            if dmode=32 then
                                k = 4
                            else
                                k = 2
                            end if
                        else
                            k = 1
                        end if
                        ilen += k
-- hmmm (search for "6/9/14" above), time to do something about this.
--  (we need this here for retaddr etc, rarely it can clash with "")
if machine=64 and and_bits(rex,#08) and mode!=3 then
    asm &= "qword"
end if
--if machine=64 and and_bits(rex,#08) then
--                      dorm(mode!=3)
--else
dormspacer = 0
                        dorm()
dormspacer = 1
--end if
                        ilen -= k
                        pairing = UV
                        clocks = 1
                        if mode=3 then
                            modified = referenced
                            referenced = 0
                        end if
                        if W then
                            if dmode=32 then
                                if mode!=3 then
                                    if wasRetAddr then
-- 22/2/2012 show return addresses in hex
                                        useHexBase = 1
                                        getimm(4,',')
                                        useHexBase = 0
                                    else
if machine=64 and and_bits(rex,#08) then
                                        getimm(4,',')
else
                                        asm &= ", dword"
                                        getimm(4,' ')
end if
                                    end if
                                else
                                    getimm(4,',')
                                end if
                            else
if machine=64 and and_bits(rex,#08) then
                                getimm(2,',')
else
                                if mode!=3 then
                                    asm &= ", word"
                                    getimm(2,' ')
                                else
                                    getimm(2,',')
                                end if
end if
                            end if
                        else
if machine=64 and and_bits(rex,#08) then
                            getimm(1,',')
else
                            if mode!=3 then
                                asm &= ", byte"
                                getimm(1,' ')
                            else
                                getimm(1,',')
                            end if
end if
                        end if
--erm, added 26/8/14: (removed 4/11/14, no idea what was supposedly fixed 26/8, but it messed alot up... also 0'd it in plist.asm)
-- replaced 8/11/14 (fixes top-level abort() parameter name, for one thing...)
--- 23/4/15:
if mode=3 then
    if rm=2 or rm=7 then -- e/rdx or e/rdi
                        lastmov = v
    elsif rm=0 then
                        lastmoveax = v
    end if
end if
--                      -- while some docs claim pU, all 
--                      -- my tests indicate this is pUV
--                      if hasdisp then
----                            pairing = NP
--                          pairing = U
--                      end if
                    else
                        unk()
                    end if
                end if
            elsif c2=1 then
                if c3=0 then
--if machine=64 then ?9/0 end if    -- default operand size is 64 bits (#66->16)
--DEV
if machine=64 then
    printf(1,"p2asm line 3109: check #%08x\n",addr)
end if

--02/10/13:
if 1 then
                    asm = "enter "
                    --15/1/14
                    ilen += 1
                    getimm(2,'#')
                    ilen -= 1
                    asm &= ","
                    getimm(1,'#')
else
                    asm = "enter"
                    -- 15/1/14
                    ilen += 1
                    getimm(2,' ')
                    ilen -= 1
                    getimm(1,',')
end if

                elsif c3=1 then
--if machine=64 then ?9/0 end if    -- default operand size is 64 bits (#66->16)
--DEV
--if machine=64 then (seems fine 15/1/14)
--  printf(1,"p2asm line 3134: check #%08x\n",addr)
--end if
                    asm = "leave"
                    clocks = 3
                elsif c3=2 then
                    asm = "retf"
                    getimm(2,' ')
                    clocks = 4
                elsif c3=3 then
                    asm = "retf"
                    clocks = 4
                elsif c3=4 then
                    asm = "int3"
                    clocks = 13
                elsif c3=5 then
                    c = getbyte()
                    hex &= ' '&sprintf(hexfmt,c)
                    asm = sprintf("int 0x%02x",c)
if c=#80 then
    if machine_arch=arch_PE then
        syscall = "*** arch_PE??? ***"
    elsif machine_arch=arch_ELF then
        syscall = "???"
        if machine=32 then
            if lastmoveax=1 then
                syscall = "sys_exit"
            elsif lastmoveax=4 then
                syscall = "sys_write"
            elsif lastmoveax=48 then
                syscall = "sys_signal"
            elsif lastmoveax=67 then
                syscall = "sys_sigaction"
            elsif lastmoveax=265 then
                syscall = "sys_clock_gettime"
            end if
        elsif machine=64 then
            -- (64 bit should be using syscall, not int0x80)
        else
            ?9/0
        end if
    else
        ?9/0
    end if
    asm &= " ("&syscall&")"
end if
                    ilen = 2
                elsif c3=6 then
--if machine=64 then ?9/0 end if    -- invalid in 64-bit mode
--DEV
if machine=64 then
    printf(1,"p2asm line 3184: check #%08x\n",addr)
end if
                    asm = "into"
                else --c3=7 then
                    if machine=64 then
                        asm = "iretq"
                    else
                        asm = "iretd"
                    end if
                end if
                pairing = NP
            elsif c2=2 then
                -- rotates handled above
                if c3=7 then
                    asm = "xlat"
                    pairing = NP
                elsif c3=6 then
--if machine=64 then ?9/0 end if    -- invalid in 64-bit mode
--DEV
if machine=64 then
    printf(1,"p2asm line 3204: check #%08x\n",addr)
end if
                    asm = "salc"
                    pairing = NP
                else
--                  if machine=64 then ?9/0 end if -- aad and aam invalid
--DEV
if machine=64 then
    printf(1,"p2asm line 3212: check #%08x\n",addr)
end if
                    if c3=5 then
                        asm = "aad"
                    elsif c3=4 then
                        asm = "aam"
                    else
                        ?9/0    -- should never trigger
                    end if
                    c = getbyte()
                    hex &= ' '&sprintf(hexfmt,c)
                    if c!=10 then
                        asm &= sprintf(" %d",c)
                    end if
                    ilen += 1
                    pairing = NP
                end if
            elsif c2=3 then
                -- 33x are all two-byte FPU instructions
                getrm()
                if c3=0                                     -- fXXX m32, fXXX st,stN
                or (c3=2 and mode!=3)                       -- fiXXX m32
                or c3=4                                     -- fXXX m64, fXXX stN,st
                or (c3=6 and (mode!=3 or reg!=2)) then      -- fiXXX m16, fXXXp stN,st
                    if mode=3 and c3>=4 and reg>=4 then
                        -- sub,subr and div,divr are the "wrong way round" in this case:
                        reg = xor_bits(reg,1)
                    end if
if reg<=7 then
                    asm = fs5[reg+1]    -- {"fadd","fmul","fcom","fcomp","fsub","fsubr","fdiv","fdivr"}
else
    printf(1,"p2asm line 3243: check #%08x\n",addr)
                    asm = "???"
end if
                    if c3=6 or c3=2 then
                        if mode=3 then
                            asm &= 'p'
                        else
                            asm = 'f'&asm
                            asm[2] = 'i'
                        end if
                    end if
                    if mode=3 then
                        if find(reg,{2,3}) then     -- FCOM, FCOMP
                            if c3=6 and reg=3 then
                                -- fcompp takes no parameters...
                                if rm!=1 then unk() end if
                            else
                                if rm!=1 then
                                    asm &= sprintf(" st%d",rm)
                                end if
                            end if
                        else
                            if c3=0 then
--                              if rm!=1 then
                                asm &= sprintf(" st0,st%d",rm)
--                              end if
-- 21/12/14: (we may as well spell things out)
--                          elsif c3!=6 or rm!=1 then
                            else
                                asm &= sprintf(" st%d,st0",rm)
                            end if
                        end if
                    else
                        if c3=4 then
                            asm &= " qword"
                        elsif c3=6 then
                            asm &= " word"
                        else -- c3=0 or 2
                            asm &= " dword"
                        end if
dormspacer = 0
                        dorm()
dormspacer = 1
                    end if
                    if reg<=1 or reg=4 or reg=5 then    -- f[i]add[p], f[i]mul[p], f[i]sub[p]
                        if asm[2]='i' then
                            pairing = NP --i2f2
                            clocks = 6
                        else
                            pairing = NP --Xi2f2
                            clocks = 3
                        end if
                    elsif reg<=3 then   -- f[i]com[p]
                        if asm[2]='i' then
                            pairing = NP
                            clocks = 4
                        else
                            pairing = NP --X
                            clocks = 1
                        end if
                    elsif reg>=6 then   -- fdiv, fdivr, fidiv, fidivr
                        pairing = NP
                        if asm[2]='i' then
                            clocks = 42 --i38f2
                        else
                            clocks = 39 --Xi38f2
                        end if
                    end if

                elsif c3=1 then
--trace(1)
                    if mode!=3 then
if reg<=7 then
                        asm = f5a[reg+1]
                        -- {"fld",0,"fst","fstp","fldenv","fldcw","fnstenv","fnstcw"}
else
    printf(1,"p2asm line 3319: check #%08x\n",addr)
                        asm = "???"
end if
                        if integer(asm) then    --reg=1
                            unk()
                        else
                            if reg<=3 then
                                asm &= " dword"
                            else
-- 27/8/14:
--                              asm &= ' '
                                asm &= " word"
                            end if
dormspacer = 0
                            dorm()
dormspacer = 1
                            if reg=0 then       -- fld m32
                                pairing = NP --X
                                clocks = 1
                            elsif reg<=3 then   -- fst[p]
                                pairing = NP --*
                                clocks = 2
                            elsif reg=4 then    -- fldenv
                                pairing = NP
                                clocks = 42 --?? [DEV, a random guess for 28 bytes restored]
                            elsif reg=5 then    -- fldcw m16
                                pairing = NP
                                clocks = 8
                            elsif reg=6 then    -- fnstenv
                                pairing = NP
                                clocks = 42 --?? [DEV, a random guess for 28 bytes stored]
                            elsif reg=7 then    -- fnstcw m16
                                pairing = NP
                                clocks = 2
                            end if
                        end if
                    elsif reg<2 then
                        asm = f5b[reg+1]    -- {"fld","fxch"}
                        if reg=0 or rm!=1 then
                            asm &= sprintf(" st%d",rm)
                        end if
                        if reg=0 then       -- fld stn
                            pairing = NP --X
                        else                -- fxch
                            pairing = NP
                        end if
                        clocks = 1
                    else
                        k = find(c,f5c)
--constant f5c = {#D0,"fnop",#E0,"fchs",#E1,"fabs",#E4,"ftst",#E5,"fxam",               -- 1..9
--                #E8,"fld1",#E9,"fldl2t",#EA,"fldl2e",#EB,"fldpi",#EC,"fldlg2",        -- 11.19
--                #ED,"fldln2",#EE,"fldz",#F0,"f2xm1",#F1,"fyl2x",#F2,"fptan",          -- 21..29
--                #F3,"fpatan",#F4,"fxtract",#F5,"fprem1",#F6,"fdecstp",#F7,"fincstp",  -- 31..39
--                #F8,"fprem",#F9,"fyl2xp1",#FA,"fsqrt",#FB,"fsincos",#FC,"frndint",    -- 41..49
--                #FD,"fscale",#FE,"fsin",#FF,"fcos"}                                   -- 51..55
                        if k then
                            asm = f5c[k+1]
                            if k=3 or k=5 then      -- fchs, fabs
                                pairing = NP --X
                                clocks = 1
                            elsif k=7 then
                                pairing = NP
                                clocks = 1
                            elsif k=9 then          -- fxam
                                pairing = NP
                            elsif find(k,{11,23}) then  -- fld1,fldz
                                pairing = NP
                                clocks = 2
                            elsif k>=13 and k<=21 then  -- fldl2t,fldl2e,fldpi,fldlg2,fldln2
                                pairing = NP -- i2f2
                                clocks = 5
                            elsif k=25 then -- f2xm1
                                pairing = NP --i2f2
                                clocks = 57
                            elsif k=27 then -- fyl2x
                                pairing = NP
                                clocks = 103
                            elsif k=29 then -- fptan
                                pairing = NP --i36
                                clocks = 173
                            elsif k=31 then -- fpatan
                                pairing = NP --i2f2
                                clocks = 134
                            elsif k=43 then -- fyl2xp1
                                pairing = NP
                                clocks = 105
                            elsif k=35 then -- fprem1
                                pairing = NP --i2f2
                                clocks = 70
                            elsif k=41 then -- fprem
                                pairing = NP --i2f2
                                clocks = 64
                            elsif k=45 then -- fsqrt
                                pairing = NP --i69f2
                                clocks = 70
                            elsif k=47 then -- fsincos
                                pairing = NP --i2f2
                                clocks = 137
                            elsif k=49 then -- frndint
                                pairing = NP
                                clocks = 20
                            elsif k=51 then -- fscale
                                pairing = NP    --i5
                                clocks = 32
                            elsif k=53 or k=55 then -- fsin,fcos
                                pairing = NP --i2f2
                                clocks = 126
                            end if
                        else
                            unk()
                        end if
                    end if
                elsif c3=2 then
                    if reg<4 then
                        asm = "fcmov"&fcm1[reg+1]&sprintf(" st,st%d",rm)
                        pairing = NP
                    elsif c=#E9 then    -- 0o351
                        asm = "fucompp"
                        pairing = NP --X
                        clocks = 1
                    else
                        unk()
                    end if
                elsif c3=3 then
                    if mode=3 then
                        if reg<4 then
                            asm = "fcmovn"&fcm1[reg+1]&sprintf(" st,st%d",rm)
                            pairing = NP
                        elsif reg=4 then
                            k = find(c,f6d) -- {#E0,"feni",#E1,"fdisi",#E2,"fnclex",#E3,"fninit"}
                            if k then
                                asm = f6d[k+1]
                                if k=7 then -- fninit
                                    clocks = 12
                                end if
                                pairing = NP
                            else
                                unk()
                            end if
                        elsif reg<7 then
                            if reg=5 then
                                asm = "fucomi"
                            else
                                asm = "fcomi"
                            end if
                            asm &= sprintf(" st,st%d",rm)
                            pairing = NP
                        else
                            unk()
                        end if
                    else
                        -- {"fild",0,"fist","fistp",0,"fld",0,"fstp"}
                        asm = f5d[reg+1]
                        if integer(asm) then
                            unk()
                        else
                            if reg<4 then
                                asm &= " dword"
                            else
                                asm &= " tbyte"
                            end if
dormspacer = 0
                            dorm()
dormspacer = 1
                            if reg=0 then       -- fild m32
                                pairing = NP --i2f2
                                clocks = 3
                            elsif find(reg,{2,3}) then  -- fist(p) m32
                                pairing = NP
                                clocks = 6
                            elsif reg=5 then    -- fld m80
                                pairing = NP
                                clocks = 3
                            elsif reg=7 then    -- fstp m80
                                pairing = NP --*
                                clocks = 3
                            end if
                        end if
                    end if
                elsif c3=5 then
                    if mode!=3 then
                        -- {"fld",0,"fst","fstp","frstor",0,"fnsave","fnstsw"}
                        asm = f5e[reg+1]
                        if integer(asm) then
                            unk()
                        else
                            if reg<=3 then
                                asm &= " qword"
                                dormspacer = 0
                            end if
                            dorm()
                            dormspacer = 1
                            if reg=0 then   -- fld m64
                                pairing = NP --X
                                clocks = 1
                            elsif reg<=3 then   -- fst[p] m64
                                pairing = NP
                                clocks = 2
                            elsif reg=4 then    -- frstor
                                pairing = NP
                            elsif reg=6 then    -- fnsave
                                pairing = NP
                            elsif reg=7 then
                                pairing = NP
                                clocks = 6
                            end if
                        end if
                    else
                        -- {"ffree",0,"fst","fstp","fucom","fucomp",0,0}
                        asm = f5f[reg+1]
                        if integer(asm) then
                            unk()
                        else
                            if reg<4 or rm!=1 then
                                asm &= sprintf(" st%d",rm)
                            end if
                            if reg=2 or reg=3 then  -- fst(p) stn
                                pairing = NP --*
                                clocks = 1
                            elsif reg>=4 then       -- fucom, fucomp stn
                                pairing = NP --X
                                clocks = 1
                            end if
                        end if
                    end if
                elsif c3=6 then
                    -- just the DED0 get here, most 6 are dealt with above.
                    unk()
                elsif c3=7 then
--trace(1)
                    if mode!=3 then
                        -- {"fild word",0,"fist word","fistp word","fbld","fild qword","fbstp","fistp qword"}
                        asm = f7a[reg+1]
                        if integer(asm) then -- reg=1
                            unk()
                        else
--                          asm&=' '
dormspacer = 0
                            dorm()
dormspacer = 1
                            if reg=0 or reg=5 then          -- fild m16/m64
                                pairing = NP --i2f2
                                clocks = 3
                            elsif find(reg,{2,3,7}) then    -- fist m16, fistp m16/m64
                                pairing = NP
                                clocks = 6
                            end if
                        end if
                    else
                        -- {"ffreep",0,0,0,"fnstsw ax","fucomip","fcomip",0}
                        asm = f7b[reg+1]
                        if integer(asm) then
                            unk()
                        else
                            pairing = NP
                            if reg=0 then
                                asm &= sprintf(" st%d",rm)
                            elsif reg=4 then    -- fnstsw ax
                                modified = eaxbit
                                clocks = 6
                            elsif reg>=5 then
                                asm &= sprintf(" st,st%d",rm)
                            end if
                        end if
                    end if
                else
                    ?9/0    -- should not happen
                end if
            elsif c2=4 then
--if machine=64 then ?9/0 end if    -- RIP addressing in 64-bit mode (change getimm['j']?)
--DEV
--if machine=64 then -- erm, some confusion with loop vs loopne, but I think loop (this/p2asm) was right (and matched the source) whereas fdbg saying loopne was wrong.
--  printf(1,"p2asm line 3591: check #%08x\n",addr)
--end if
                asm = i34x[c3+1]    -- {"loopnz","loopz","loop","jecxz","in al","in eax","out","out"}
                if machine=64 then
                    if c3=3 then
                        asm = "jrcxz"
                    end if
                end if
                if c3>=6 then
                    getimm(1,' ')
                    if c3=6 then
                        asm &= ",al"
                    else
                        asm &= ",eax"
                    end if
                elsif c3>=4 then
                    getimm(1,',')
                else
--trace(1)
-- I think this should be getreljmp: (change below now verified as OK, and better!)
--ppOpt({pp_Pause,16})
--                  getimm(1,' ')
--                  getimm(1,'j')
                    getimm(1,'j')
                end if
                if c3<=1 then       -- loopnz, loopz
--                  pairing = NP
                    clocks = 8
                elsif c3<=3 then    -- loop, jecxz
--                  pairing = NP
                    clocks = 6
                end if
                pairing = NP
            
            elsif c2=5 then
                if c3=0 then
--if machine=64 then ?9/0 end if -- default operand size is 64 bits (#66->16)
--if machine=64 then ?9/0 end if    -- RIP addressing in 64-bit mode (change getimm['j']?)
--printf(1,"p2asm line 2533: check #%08x\n",addr)
--trace(1)
                    asm = "call"
                    --                  getimm(4,' ')
--                  getreljmp(4)
                    getimm(4,'j')
                    clocks = 1
                    pairing = V
                elsif c3=1 then
--if machine=64 then ?9/0 end if    -- RIP addressing in 64-bit mode (change getimm['j']?)
--DEV
--if machine=64 then -- (seems OK 10/1/14)
--  printf(1,"p2asm line 3641: check #%08x\n",addr)
--end if
                    asm = "jmp"
                    getimm(4,'j')
                    clocks = 1
                    pairing = V
                elsif c3=2 then
--if machine=64 then ?9/0 end if    -- invalid in 64-bit mode
--DEV
if machine=64 then
    printf(1,"p2asm line 3651: check #%08x\n",addr)
end if

--                  trace(1)
                    asm = ""
                    -- DEV I think this is invalid in 32-bit code: --DEV and 64 bit code...
                    -- 15/1/14
                    ilen += 2
                    getimm(4,':')
                    ilen -= 2
                    k = length(asm)
                    getimm(2,' ')
                    asm = "jmp ?far? "&asm[k+1..length(asm)]&asm[1..k]
                    terminal = 1
                    -- NB This instruction is not supported (pairing left as ??)
                elsif c3=3 then
--if machine=64 then ?9/0 end if    -- RIP addressing in 64-bit mode (change getimm['j']?)
--printf(1,"p2asm line 2568: check #%08x\n",addr)
                    asm = "jmp"
                    getimm(1,'j')
                    clocks = 1
                    pairing = V
                elsif c3=4 then
                    asm = "in al,dx"
                    pairing = NP
                elsif c3=5 then
                    asm = "in eax,dx"
                    pairing = NP
                elsif c3=6 then
                    asm = "out dx,al"
                    pairing = NP
                elsif c3=7 then
                    asm = "out dx,eax"
                    pairing = NP
                end if
            elsif c2=6 then
                if c3<6 then
                    asm = i36x[c3+1]    -- {"lock","int1",0,0,"hlt","cmc"}
                    if integer(asm) then
                        unk()
                    else
                        if c3=4 then
                            clocks = 4
--                          pairing = NP
                        elsif c3=5 then
                            clocks = 2
--                          pairing = NP
                        end if
                        pairing = NP
                    end if
                else
                    getrm()
--DEV
if machine=64 and reg>7 then
    printf(1,"p2asm line 3705: check #%08x\n",addr)
    asm = "??ERM??"
else
                    asm = i36y[reg+1]   -- {"test",0,"not","neg","mul","imul","div","idiv"}
end if
                    if integer(asm) then --reg=1
                        unk()
                    else
                        qualifier()
                        --15/1/14
                        if reg=0 then
                            if W then
                                if dmode=32 then
                                    k = 4
                                else
                                    k = 2
                                end if
                            else
                                k = 1
                            end if
                            ilen += k
dormspacer = 0
                            dorm()
dormspacer = 1
                            ilen -= k
                        else
dormspacer = 0
-- 2/1/15 (over neg rcx vs neg ecx)
--                          dorm()
if mode=3 then
                            dorm(0,0)
else
                            dorm()
end if
dormspacer = 1
                        end if
                        if reg=0 then       -- test
                            if W then
                                if dmode=32 then        -- test r/m32,imm32
                                    getimm(4,',')
                                else                    -- test r/m16,imm16
                                    getimm(2,',')
                                end if
                            else                        -- test t/m8,imm8
                                getimm(1,',')
                            end if
                            if hasdisp then
--                              pairing = NP
                                pairing = U
                            else
                                pairing = UV
                            end if
                            if mode=3 then
                                clocks = 1
                            else
                                clocks = 2
                            end if
                        elsif reg<=3 then   -- not, neg
                            pairing = NP
                            if mode=3 then
                                clocks = 1
                            else
                                clocks = 3
                            end if
                            modified = referenced
                        elsif reg<=5 then   -- mul,imul (one operand forms)
                            pairing = NP
                            if W then
                                if dmode=32 then
                                    clocks = 10
                                else
                                    clocks = 11
                                end if
                                modified = eaxbit+edxbit
                            else
                                clocks = 11
                                modified = eaxbit
                            end if
                            referenced = or_bits(referenced,eaxbit)
                        elsif reg>=6 then   -- div, idiv
                            pairing = NP
                            if W then
                                if dmode=32 then
                                    clocks = 41
                                else
                                    clocks = 25
                                end if
                                modified = eaxbit+edxbit
                            else
                                clocks = 17
                                modified = eaxbit
                            end if
                            referenced = or_bits(referenced,modified)
                            if reg=7 then   -- idiv
                                clocks += 5
                            end if
                        end if
                    end if
                end if
            elsif c2=7 then
                if c3<6 then
                    asm = i37x[c3+1]    -- {"clc","stc","cli","sti","cld","std"}
                    pairing = NP
                    if c3=2 or c3=3 then    -- sti,cli
                        clocks = 7
                    else
                        clocks = 2
                    end if
                else
                    getrm()
                    if c3=6 and reg>2 then
                        unk()
                    else
                        if reg<=1 then
                            asm = idpp[reg+1]   -- {inc,dec}
--DEV should be a flag to dorm()?
                            qualifier()
                            dorm()
                            pairing = UV
                            if mode=3 then
                                modified = referenced
                                clocks = 1
                            else
                                clocks = 3
                            end if
                        elsif reg=6 then
--if machine=64 then ?9/0 end if    -- default operand size is 64 bits
--DEV
--if machine=64 then
--  printf(1,"p2asm line 3834: check #%08x\n",addr)
--trace(1)
--end if
                            asm = "push "
                            qualifier(1)
if machine=64 then  -- added 15/12/14 (over result = result[1..tmp] in prntf/round())
    dmode = 64
end if
dormspacer = 0
                            dorm()
dormspacer = 1
                            pairing = NP
                            clocks = 2
                        elsif reg<=3 then
--if reg!=3 then
----    if machine=64 then ?9/0 end if -- default operand size is 64 bits (#66->16 bits)
--  puts(1,"p2asm line 2479\n")
--end if
--trace(1)
                            if W and dmode=32 then
                                asm = "call "
if machine=64 then
    dmode = 64
end if
                                dorm(1)
                                pairing = NP
--                              if mode=3 then
                                    clocks = 2
--                              else
--                                  clocks = 4      -- seems unlikely...
--                              end if
                            else
                                unk()
                            end if
                        else
                            if reg=4 then
--if machine=64 then ?9/0 end if    -- RIP addressing in 64-bit mode (change getimm['j']?)?
--DEV
--if machine=64 then -- added qword fix 11/1/14...
--printf(1,"p2asm line 2577: check #%08x\n",addr)
--end if
                                asm = "jmp "
-- 3/4/2010 hack for jump tables
useHexBase = 1
--17/1/15 (over jmp qword[rsp])
if X64=0 then
                                dorm(1)
else
                                dorm(0)
end if
useHexBase = 0
                                clocks = 2
                                pairing = NP
-- 2/4/2010:

if c3=7
and mode=0
--and reg=4
and rm=4
and base=5  -- no base
--and scale=2
and ((X64=0 and scale=2) or (X64=1 and scale=3))
and and_bits(addr+7-v,#03)=0    -- added 1/10/13
and isPlausibleJumpTable(addr+7)
then
--and (v=addr+7 or v=addr+3) then
    -- 1/10/13. We have just seen a jmp[reg*4+imm32]. For Phix code, it *IS* a switch, period.
    --          However since it could be switch 1/2/3 or 1000/1001/1002 etc, we cannot (for the
    --          non-Phix case) look at base and deduce anything much from it, except as already
    --          done above it is modulo-4 the table start. If it is an in-line jump table, we
    --          should expect consecutive forward (absolute not relative) jumps and that the
    --          min_jt_addr (as found during the scan) exactly terminates the table.
    dump_jump_table = -1
--?{addr,v}
  if X64 then
    jump_table_index = (addr+7-v)/8     -- (result should always be an integer)
  else
    jump_table_index = (addr+7-v)/4     -- (result should always be an integer)
                                        -- (7 is length of jmp [reg*4+imm32] instruction)
  end if
end if
                            else
                                unk()
                            end if
                        end if
                    end if
                end if
            else
                ?9/0    -- should not happen
            end if
        end if
        if length(sop) or length(repprefix) or length(lockprefix) then
--trace(1)
            asm &=  "? superfluous prefix ?"
            terminal = 1
            sop=""
        end if
    end if
end procedure

constant false=(1=0), true=(1=1)
integer thismod, lastmod, umod, lastinu, uclocks, cycle

--integer firsty=1

global function decode()
sequence analysis
integer agistall, dependency, thispairing
sequence res

--if addr=#0041A070 then trace(1) end if
--if addr=#00426720 then trace(1) end if
--if addr=#00426727 then trace(1) end if
--if addr=#0041EFC7 then trace(1) end if
--if addr=CSvaddr then trace(1) end if
--if firsty then
--printf(1,"(p2asm.e line 3731) addr=#%08x\n",addr)
--firsty=0
--end if

-- 2/4/10:
    if dump_jump_table then
--trace(1)
        asm = ""
        hex = ""
if X64 then
        getimm(8,'#')
else
        getimm(4,'#')
end if
--12/10/13:
--v -= ImageBase
        if dump_jump_table=-1 then
            min_jt_addr = v
            dump_jump_table = 1
--printf(1,"p2asm line 2798: check #%08x\n",addr)
--if getc(0) then end if
--?9/0
        elsif v<min_jt_addr then
            min_jt_addr = v
        end if
--      res = {asm,addr,hex,""}
--      res = {asm,addr,hex,sprintf("[%d]",jump_table_index)}
--      res = {sprintf("%s [%d]",{asm,jump_table_index}),addr,hex,""}
        res = sprintf("%s [%d]",{asm,jump_table_index})
        res = {res,addr,hex,""}
        jump_table_index += 1
if X64 then
        addr += 8 --ilen
else
        addr += 4 --ilen
end if
--12/10/13:
--      if min_jt_addr=addr then
        if min_jt_addr=addr
        or min_jt_addr-ImageBase=addr then
            dump_jump_table = 0
        end if
        return res
    end if

    disasm()
    --pp(assemble(0,"add [eax+124],al")) -- then return "000100 7C" end if
    --pp(assemble(0,"add [#00401BD4],eax")) -- then return "001005 D41B4000" end if
    --pp(assemble(0,"add dword [#00401BD4],12"))-- then return "203005 D41B4000 0C" end if
    --pp(assemble(0,"lea edi,[eax*4+4201828]"))-- then return "215074205 641D4000" end if
    --pp(assemble(0,"test byte[eax-1],-128"))-- then return "366100 FF 80" end if

    --if getc(0) then end if

    --
    -- Code to report dependencies and AGI stalls.
    -- disassembly/assembly should set pairing (NP,UV,U,V), clocks,
    -- referenced, modified, agiset (see regbit, eg add eax,[ecx+edx] is
    --   #07,       #01,      #06.)
    --  Note that agiset is a bit of a misnomer; it is the registers used 
    --  in address generation which are to be checked for agi stalls, 
    --  rather than actual offences.
    --
    -- Please note the following code is very rough, and often wrong.
    -- However the hard part is gathering the raw data above, which has rather
    -- preoccupied me, and tweaks should be pretty straightforward.
    --
--DEV:
-- keep a mini stack of modified regs by cycle, agistall is all or'd with agiset
--                                              dependency is referenced or'd with last cycle
    dependency = 0
    agistall = and_bits(agiset,or_bits(lastmod,thismod))
    thispairing = pairing
    if lastinu then
--  if lastinu and not agistall then
        dependency = and_bits(umod,or_bits(referenced,modified))
--      if find(pairing,{UV,V}) and not dependency then
        if find(pairing,{UV,V}) and not dependency and not agistall then
            if pairing = UV then thispairing = VU end if
--DEV 10/05/08:
-- pairing only overlaps last cycle of u instruction...
--          if clocks>uclocks then uclocks = clocks end if
--if clocks!=1 then ?9/0 end if
if uclocks>1 then
    cycle += uclocks-1
    uclocks = 1
end if
--DEV 10/05/08 ends
            thismod = or_bits(thismod,modified)
        else
            if not find(pairing,{UV,V}) then
                -- don'r report false/irrelevant dependencies
                dependency = 0
            end if
            cycle += uclocks
            lastmod = thismod
            thismod = modified
            lastinu = false
        end if
    end if
--  agistall = and_bits(agiset,lastmod)
--  agistall = and_bits(agiset,or_bits(lastmod,thismod))
--  agistall = and_bits(agiset,or_bits(lastmod,thismod,umod,modified??)
    if dependency then
        analysis = sprintf("%02x",dependency)
    else
        analysis = "  "
    end if
    if agistall then
        analysis = sprintf("%s *%02x*",{analysis,agistall})
-- cycle += 1??
--if and_bits(agiset,thismod) then
-- NB: AGI stall clocks are a rough guide only.
--      The logic to detect AGI stalls is an extension to the pairing/dependency logic
--      and does not take proper account of instruction cycles, for example:
--
--                  clock 24:   mov eax,5           1 cycle U
--                        24:   nop                 1 cycle V
--                        25:   add esi,[data]      2 cycles U
--                        25:   nop                 1 cycle V
--                        28:   lea eax,[eax+2]     1 cycle     **AGI**
--
--      An AGI is reported on the lea instruction, though in fact eax was calculated in clock 24
--      and hence the AGI would not in fact occur since the add took 2 cycles. I should also
--      point out that I do not know what would happen if the second nop were removed; it is
--      either that the lea executes in the v pipe in clock 26, or the u pipe in clock 27.
--      In short, the AGI detection always treats each pair of instructions as 1 clock,
--      and/or the variables lastmod and thismod are not "advanced" enough, in both senses.
--
-- The following has not been attempted:
--4. If a read/modify/write instruction is paired with a read/modify or read/modify/write
--instruction, then they will pair imperfectly.
--The number of clock cycles used is given in the following table:
--First instruction     --------------------------- Second instruction ----------------------
--                          MOV or register only        read/modify         read/modify/write
--   MOV or register only            1                       2                       3
--   read/modify                     2                       2                       4
--   read/modify/write               3                       3                       5
--
--Table 4.1. Pairing complex instructions
--Examples:
--  ; Example 4.3. P1/PMMX pairing complex instructions
--      add [mem1], eax / add ebx, [mem2] ; 4 clock cycles
--      add ebx, [mem2] / add [mem1], eax ; 3 clock cycles
--
-- One set of notes I have states that pairing only overlaps last cycle of the U instruction
--  with the first cycle of the V instruction, probably true, but not done here.
--  The clock calculation is therefore wrong in this code, and AGI stalls will not occur 
--  from the U op if the V instruction takes longer than one clock...

        if and_bits(agiset,lastmod) then    --??
            cycle += 2
--          cycle += 1
            lastmod = 0
--          thismod = 0 --??
        else
            cycle += 1
--          lastmod = thismod --??
--          thismod = 0
        end if
    else
        analysis &= "   "
    end if
    analysis = sprintf("%2s %02x %02x %2d %3d %s",
                     {pair[thispairing],modified,referenced,clocks,cycle,analysis})

    if not lastinu and find(pairing,{UV,U}) then
        lastinu = true
        uclocks = clocks
        umod = modified
--      thismod = modified
    else
        if lastinu then
            cycle += uclocks
            lastinu = false
        else
            cycle += clocks
        end if
        lastmod = thismod
--      thismod = modified
    end if
    thismod = modified

--      pp(sprintf("%08x %-22s %s",{addr,hex,asm}))     -- for the pause!
--      pp(sprintf("%08x %-22s %s %s",{addr,hex,asm,analysis}))
--      pp(sprintf("         %s",{analysis}))
--      pp(sprintf("%08x %-22s %s",{addr,hex,asm}))
--      pp(sprintf("%08x %-22s %s",{addr,analysis,asm}))
--  printf(1,"%08x %-22s %s\n",{addr,hex,asm})
--if pairing = unknown then trace(1) end if -- unsupported instruction?
if pairing = unknown then
  if not find(addr,{#0002DA17, #0002DB07}) then
    printf(1,"p2asm line 4142: unknown pairing #%08x\n",addr)
  end if
end if
    dmode = 32  -- reset for next instruction
--  amode = machine -- reset for next instruction
--if machine=64 then
--  amode = 64  -- reset for next instruction
--else
    amode = 32  -- reset for next instruction
--end if
    rex = 0     -- reset for next instruction
    res = {asm,addr,hex,analysis}
    addr += ilen
--  lines += 1
    return res
end function

--procedure getascii()
--integer c
--  v = {}
--  while 1 do
--      c = getc(fn)
--      if c=0 then exit end if
--      v &= c
--  end while
----    printf(1,"%08x %s\n",{addr,v})
--  rbs = append(rbs,{addr,length(v),0,v,0,F_ascii})
--  addr += length(v)
--end procedure

--DEV get rid of these defaults
global procedure decodeinit(atom base, integer idx, integer mch=32, integer arch=arch_PE)
--printf(1,"decodeinit(base=%08x,idx=%08x)\n",{base,idx})
    thismod = 0
    lastmod = 0
    umod = 0
    lastinu = false
    cycle = 1
    addr = base
    csidx = idx
    machine = mch
    if not find(machine,{32,64}) then ?9/0 end if
    amode = machine
    machine_arch = arch
end procedure

global procedure resetmods()
-- 17/8/9 remove reg dependencies over a call...
    thismod = 0
    lastmod = 0
    umod = 0
    lastinu = false
end procedure
