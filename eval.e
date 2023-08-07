--
-- eval.e
--
include pmain.e
include builtins\VM\pProfile.e
include plist.e     -- assembly and symbol table dump
include builtins\VM\pMemChk.e   -- check_heap

--DEV mchk broke p -test... (also cmp [mchk],ebx crashes in pilasm.e)
--    (I have not used memory leak checking for a very long time,
--     I can live without, and it probably no longer works anyway)

global procedure Interpret()--bool mchk)
    symtab[T_cmdlnflg] = 2 -- in command_line(), get stuff from symtab[T_fileset]
    integer ebp4,   -- saved ebp, /4 as it shd be dword-aligned, then fits in an integer
            esp4,   -- saved esp, ""
            pst4    -- saved symtabptr, aka [ds+8] (via :%pGetSymPtr), ""
--          ntcb4   -- saved pGtcb.pNxtcb
    object wasEBP = symtab[T_EBP]   -- integer(0) = compiled, else 
                                    -- ({ebp4,esp4,pst4}) = interpreted
    #ilASM{
        -- set ebp4,esp4,pst4 (from ebp,esp,symtabptr)
        [32]
            mov eax,ebp
            mov ecx,esp
            call :%pGetSymPtr
            shr eax,2
            shr ecx,2
            shr esi,2
            mov [ebp4],eax
            mov [esp4],ecx
            mov [pst4],esi
        [64]
            mov rax,rbp
            mov rcx,rsp
            call :%pGetSymPtr
            shr rax,2
            shr rcx,2
            shr rsi,2
            mov [ebp4],rax
            mov [esp4],rcx
            mov [pst4],rsi
          }
    -- (note that esp4 is also used in builtins\VM\Stack.e\opAbort) [DEV]
    symtab[T_EBP] = {ebp4,esp4,pst4}
    -- added 28/5/15:
    if not dumpil
    and not listing 
    and not some_unresolved_rtnids then
        rbldrqd = 1
    end if
    integer CSvaddr4 = floor(CSvaddr/4)
    #ilASM{
        [32]
            mov eax,[with_js]
            call :%pWithJS
--          cmp [mchk],0
--          je @f
--              call :%pNewGtcbChain
--              mov [ntcb4],eax         -- temp save
--        @@:
            mov esi,[symtab]
            mov ecx,[CSvaddr4]
            shl esi,2
            shl ecx,2
            call :%pSetSymPtr
            push ecx
            call :%pNewStack
            mov ecx,[esp]           -- (keep for diagnostics)
            call ecx
            pop ecx
--          cmp [mchk],0
--          je @f
--              call :%RunCleanup
--        @@:
            call :%pGetSymPtr
            mov edx,[esi+21*4]      -- symtab[T_EBP=22]
            mov eax,[ebx+edx*4]     -- symtab[T_EBP][1] = ebp4
            mov ecx,[ebx+edx*4+4]   -- symtab[T_EBP][2] = esp4
            mov esi,[ebx+edx*4+8]   -- symtab[T_EBP][3] = pst4
            shl eax,2
            shl ecx,2
            shl esi,2
            mov ebp,eax             -- restore ebp
            mov esp,ecx             -- restore esp
            call :%pSetSymPtr
--          cmp [mchk],0
--          je @f
--              mov eax,[ntcb4]
--              call :%pRestoreGtcbChain
--              mov [ntcb4],edx
--        @@:
            xor eax,eax
            call :%pWithJS
        [64]
            mov rax,[with_js]
            call :%pWithJS
--          cmp [mchk],0
--          je @f
--              call :%pNewGtcbChain
--              mov [ntcb4],rax         -- temp save
--        @@:
            mov rsi,[symtab]
            mov rcx,[CSvaddr4]
            shl rsi,2
            shl rcx,2
            call :%pSetSymPtr
            push rcx
            call :%pNewStack
            mov rcx,[rsp]           -- (keep for diagnostics)
            call rcx
            pop rcx
--          cmp [mchk],0
--          je @f
--              call :%RunCleanup
--        @@:
            call :%pGetSymPtr
            mov rdx,[rsi+168]       -- symtab[T_EBP=22]
            mov rax,[rbx+rdx*4]     -- symtab[T_EBP][1] = ebp4
            mov rcx,[rbx+rdx*4+8]   -- symtab[T_EBP][2] = esp4
            mov rsi,[rbx+rdx*4+16]  -- symtab[T_EBP][3] = pst4
            shl rax,2
            shl rcx,2
            shl rsi,2
            mov rbp,rax             -- restore ebp
            mov rsp,rcx             -- restore esp
            call :%pSetSymPtr
--          cmp [mchk],0
--          je @f
--              mov rax,[ntcb4]
--              call :%pRestoreGtcbChain
--              mov [ntcb4],rdx
--        @@:
            xor rax,rax
            call :%pWithJS
        []
          }
--  if mchk then
--      check_heap(ntcb4)
--  end if
--puts1("returned from call(CSvaddr)!\n")
    symtab[T_EBP] = wasEBP
    free(CSvaddr)
end procedure

bool first = true

global function eval(string code, sequence rset={}, iset={}, ival={})
    if first then
        first = false
        tt_save_init()
    end if
    text = code & "\n"  -- important!
    fileno = 0
    resetOptions()
    mainpath = current_dir()
    initFilePathSet()
    tokinit()
    syminit()
    DSvaddr = 0
    filenames = {{1,""}} --append(filenames,{pathno,file})
    fileno = 1 --length(filenames)
    allfiles = {""}
    allpfiles = {""}
    exptext = {0}--append(exptext,0)
    expandedYet = {0}--append(expandedYet,0)
    unresolved_routine_ids = {0}--append(unresolved_routine_ids,0)

    ltl = length(text)
    allfiles[1] = text
    allpfiles[1] = text
    exptext[1] = 0
    expandedYet[1] = 0
    unresolved_routine_ids[1] = 0
    finalOptWarn = {optset[OptWarning]}
    col = 0
--  bool mchk = MCHK and length(ptab)=0 and opLntpcalled=0
    Compile()
    currRtn = T_maintls

--end if
    scopelevel = 1

--scopelevel += 1
--  with_js = 0
    if with_js=1 then ?9/0 end if
    profileon = false

    if length(iset)!=length(ival) then
        if length(ival) then ?9/0 end if
        ival = vslice(iset,2)
        iset = vslice(iset,1)
    end if
--  ? = compile(code)
    for i=1 to length(iset) do
        string ii = iset[i]
        tt_string(ii,-2)
        tokno = 0
        integer idx = InTable(InAny)
        if idx=0 then
            crash("%s (in iset[%d]) not found",{ii,i})
        elsif symtab[idx][S_NTyp]!=S_GVar2 then
            crash("%s (in iset[%d]) is not a gvar",{ii,i})
        end if
--      printf(1,"iset[%d] (%s) is symtab[%d], flags:%08x\n",{i,ii,idx,symtab[idx][S_State]})
        symtab[idx][S_value] = ival[i]
        symtab[idx][S_State] = or_bits(symtab[idx][S_State],K_noclr)
--      integer k = get_var_idx(iset[i])
--      set_var(k,ival[i])
--      set_var(iset[i],ival[i])
    end for
--  Interpret()
--?9/0
--/*
                S_NTyp  = 2,    -- Const/GVar/TVar/Nspc/Type/Func/Proc

                S_GVar2 = 2,    -- global or static variable

                S_State = 4,    -- state flag. S_fwd/S_used/S_set etc

                K_aod  = #000040,   -- assignment on declaration (avoid warnings/force cleanup)

                K_noclr= #000200,   -- do not clear on load (ie assignment on declaration occurred)
                                    -- only used for GVars

                K_lit  = #004000,   -- literal flag
                S_lnc  = #4200,     -- K_lit+K_noclr

                    if sv[S_NTyp]<=S_GVar2 then
                        vno = sv[S_Slink]   --DEV +gbase

                        if X64 then
                            offset = DSvaddr+vno*8+24
                            if c=isVar1 then
                                offset -= 1
                            elsif c=isVar4 then
                                offset -= 4
                            end if
                            offset -= CSvaddr+cout+3
                        else --32
                            offset = DSvaddr+vno*4+16
                        end if


--*/
    sequence res = repeat(0,length(rset))
    for i=1 to length(rset) do
        string ri = rset[i]
        tt_string(ri,-2)
        integer rdx = InTable(InAny)
        if rdx=0 then
            crash("%s (in rset[%d]) not found",{ri,i})
        elsif symtab[rdx][S_NTyp]!=S_GVar2 then
            crash("%s (in rset[%d]) is not a gvar",{ri,i})
        end if
--      printf(1,"rset[%d] (%s) is symtab[%d], flags:%08x\n",{i,ri,rdx,symtab[rdx][S_State]})
--?{"S_Tidx",symtab[rdx][S_Tidx],symtab[rdx]}
--?S_Tidx
--      ?{"rset",i,ri,rdx}
--      res[i] = get_var(rset[i])
        rset[i] = rdx
    end for
    finalfixups2("","",0)
    Interpret()
    if safe_mode=0 then
        safe_mode = 1
        #ilASM{ call :%pSafeOff }
    end if

--?9/0

--??
    integer DSvaddr4 = floor(DSvaddr/4)
    for i=1 to length(rset) do
        integer sdx = rset[i],
--              vno = symtab[sdx][S_Slink]
                vno = symtab[sdx][S_Slink]*machine_word()
        object ri
--      atom addr = DSvaddr+vno*machine_word()+iff(machine_bits()=32?16:24)
        #ilASM{
            [32]
                mov eax,[DSvaddr4]
                mov ecx,[vno]
                mov eax,[eax*4+ecx+16]
                mov [ri],eax
--DEV it disney like dis... (under -c)
--              mov [eax*4+ecx+16],ebx
            [64]
                mov rax,[DSvaddr4]
                mov rcx,[vno]
                mov rax,[rax*4+rcx+24]
                mov [ri],rax
--              mov [rax*4+rcx+24],rbx
            []
              }
--      ?{"rset",i,sdx,symtab[sdx],peekns(addr)}

--      res[i] = get_var(rset[i])
--      res[i] = peekns(addr)
        res[i] = ri
        #ilASM{
            [32]
                mov [ri],ebx
            [64]
                mov [ri],rbx
            []
              }
    end for

    free(DSvaddr)
--?{"errorcode",errorcode}
--  Warnings(1)
    tt_init()
    ResetResourceSectionDefaults()
    return res
end function


