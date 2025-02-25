--
-- pProfile.e
--
-- Dump execution profile results to ex.pro
--
-- This file is part of p.exw. It is invoked after code interpretation,
--  if "with profile[_time]" was encountered somewhere in the source.
--  Strictly speaking this is part of the interpreter, with opProfile/
--  opLnp/opLnpt invoked however-many-times by the program being run
--  via the optable, with dump_profile() being called in a more normal
--  hll fashion by p[w].exe when control returns.
--
-- ptab is created in p.exw just before interpretation and is a 1:1 map
--  for filenames/filepaths/allfiles, initially a 0 for each and every 
--  source line read in and compiled.
--

--
-- If showTally is 1, it sums the profile_time percentages as actually printed.
-- Admittedly this is of limited use, except to unequivocally state that::
--
-- ""Naturally this will not necessarily add up to exactly 100% due to rounding.
--   There is an obvious bias in that once-only initialisation code is likely to
--   contribute >0 but <=0.005 more often than not, and perfectly possible that
--   could be true of all lines, leading to a grand total of 0.00. It is equally
--   possible that every line only just rounds up, leading to a total of 199.99%.
--   OTOH, eg 99.93 does give some confidence the numbers are mostly accurate.""
--
constant showTally = 0

--  As per showTally, each "end function/procedure/type" can show a sub-total:

constant showSubTotals = 1

--global procedure dump_profile()
procedure dump_profile()
integer fn, pathno
atom c, ttot, atot, rtot, stot
object ct
sequence path, name, texti, ctab, pti
string p72, tj, stim, dashpathname
integer ch, mark, tlim

--  puts(1,"writing profile results to ex.pro...")
    puts(1,"writing profile results to "&current_dir()&`\ex.pro...`)
    fn = open("ex.pro","w")
    if sequence(opstat) then
        puts(fn,"opstats: [Meaning is dependent on your mods to p.exw, for an example see Branch().]\n")
        for i=1 to length(opNames) do
            name = opNames[i]
            c = opstat[i]
--DEV 64bit..?
            if c=#3FFFFFFF then
                printf(fn,"%-13s ** maxed out **\n",{name})
            else
                printf(fn,"%-13s %10d\n",{name,c})
            end if
        end for
        puts(fn,"\n")
    end if
    if profileon=OptProfileTime then
        puts(fn,"-- Phix execution profile_time listing.\n")
        puts(fn,"-- Left margin shows percentage of time spent executing line.\n")
        puts(fn,"--    ( spaces means 0, 0.00 means >0 and <=0.005,\n")
        puts(fn,"--      \"> markers\" on lines >=0.10 are for Edita\\F4 )\n")
        rtot = 0
        tlim = 1
        stim = sprintf("%7.2f|",tlim/10)
    elsif profileon=OptProfile then
        puts(fn,"-- Phix execution profile_count listing.\n")
        puts(fn,"-- Left margin shows count of times line was executed.\n")
        tlim = 10000
    else
        close(fn)
        return
    end if
    -- While ttot is only needed for OptProfileTime, skip is good for both.
    ttot = 0
    ctab = repeat(0,length(ptab))
    for i=1 to length(ptab) do
        atot = 0
        pti = ptab[i]
        for j=1 to length(pti) do
            atot += pti[j]
        end for
        if atot then
            ttot += atot
            ctab[i] = atot
        else
            ptab[i] = {}    -- skip output if unused
        end if
    end for
    -- second pass to skip insignificant includes
    if profileon=OptProfileTime then
        if ttot then
            for i=1 to length(ptab) do
                c = ctab[i]/ttot
                if c<0.005 then -- < 0.5%
                    ptab[i] = c
                end if
            end for
        else
            ptab = repeat(0,length(ptab))
        end if
    end if
    mark = 0
    stot = 0
    for i=1 to length(allfiles) do
        name = filenames[i]
        pathno = name[1]
        name = name[2]
        path = filepaths[pathno]
        if equal(expandedYet[i],0) then
            text = allfiles[i]
            exptext[i] = expandIntoLines()
            expandedYet[i] = linestarts
        end if
        texti = exptext[i]
        ct = ptab[i]
        if sequence(ct) 
        and length(ct)>0 then
            ctab = ct
            dashpathname = "--"&path&name
            puts(fn,dashpathname&":\n"&repeat('=',length(dashpathname))&"\n")
            for j=1 to length(texti) do
                c = ctab[j]
                if c then
                    if profileon=OptProfileTime then
                        p72 = sprintf("%7.2f|",100*c/ttot)
                        if showTally then
                            -- sum percentages as actually printed
                            c = 0
                            for k=1 to length(p72)-1 do
                                ch = p72[k]
                                if ch>='0' then
--                                  if ch>'0' then
--                                      mark = 1
--                                  end if
                                    c = c*10 + (ch-'0')
                                elsif ch='.' then
                                    for l=k+1 to length(p72)-1 do
                                        ch = p72[l]
--                                      if ch>'0' then
--                                          mark = 1
--                                      end if
                                        c += (ch-'0')/power(10,l-k)
                                    end for
                                    exit
                                end if
                            end for
                            rtot += c
                        end if
--                      if not equal(p72,"   0.00|") then
--                      if compare(p72,"   0.10|")>=0 then
                        if compare(p72,stim)>=0 then
                            mark = 1
                        end if
                    else
                        p72 = sprintf("%7d|",c)
--                      for k=1 to length(p72)-1 do
--                          if p72[k]>'0' then
--                              mark = 1
--                              exit
--                          end if
--                      end for
                        if c>=tlim/10 then
                            mark = 1
                        end if
                    end if
                    stot += c
                else
                    p72 = "       |"
                end if
                if mark and p72[1]=' ' then
                    p72[1] = '>'
                    mark = 0
                end if
                tj = texti[j]
                puts(fn,p72&tj)
                if showSubTotals and length(tj)>=1 and find(tj[1],"gpfte") then
                    if match("global procedure",tj)=1
                    or match("global function",tj)=1
                    or match("global type",tj)=1
                    or match("procedure",tj)=1
                    or match("function",tj)=1
                    or match("type",tj)=1 then
                        stot = 0
                    elsif match("end procedure",tj)=1
                       or match("end function",tj)=1
                       or match("end type",tj)=1 then
                        if stot then
                            if profileon=OptProfileTime then
                                if not showTally then -- stot already a %age, see above
                                    stot = 100*stot/ttot
                                end if
                                p72 = sprintf("=======|\n%7.2f|\n",stot)
                            else
                                p72 = sprintf("=======|\n%7d|\n",stot)
                            end if
                            if stot>=tlim and p72[10]=' ' then
                                p72[10] = '>'
                            end if
                            puts(fn,p72)
                        end if
                        stot = 0
                    end if
                end if
            end for
        elsif atom(ct) then
            -- See above, where we did:
            --  if c<0.005 then -- < 0.5%
            --      ptab[i] = c

--          if profileon=OptProfileTime then    -- added 13/1/09
                printf(fn,"%s%s - skipped (insignificant: %3.2f%%)\n",{path,name,ct*100})
--          else
--              printf(fn,"%s%s - skipped (execution count of zero)\n",{path,name})
--          end if
        else
            -- See above, where we did:
            --          ptab[i] = {}    -- skip output if unused
            -- (because ptab[i] was nowt but zeroes)

            puts(fn,path&name&" - skipped\n")
        end if
    end for
    if showTally and profileon=OptProfileTime then
        printf(fn,"=======\n%7.2f (Total of rounded percentages as printed)\n",rtot)
    end if  
    close(fn)
end procedure

--dev
--/*
integer profileon = 0   -- (in pglobals.e for now)

--NO, we need this in the optable:
global procedure profile(integer level)
-- This just sets profileon, it is opLnp/opLnpt (profile/profile_time) which do the real work.
    if level<0 or level>1 then
        --e90atpmb01        -- argument to profile() must be 0 or 1
        ?9/0
    end if
    profileon := level
end procedure
--*/

procedure profile_count(integer fileno, integer lineno)
    ptab[fileno][lineno] += 1
end procedure

atom pt = 0
integer ptfile, ptline

procedure profile_time(integer fileno, integer lineno)
atom t
    #ilASM{
        [32]
            rdtsc
            push edx
            push eax
            fild qword[esp]
            add esp,8
            lea edi,[t]
            call :%pStoreFlt
        [64]
            rdtsc
            -- (nb: we may need a shl 32 here, or maybe push rax; mov dword[rsp+4],edx)
            push edx
            push eax
            fild qword[rsp]
            add rsp,8
            lea rdi,[t]
            call :%pStoreFlt
          }
    if pt!=0 then
        ptab[ptfile][ptline] += t-pt
    end if
    ptfile = fileno
    ptline = lineno
    pt = t
end procedure

#ilASM{ jmp :!opCallOnceYeNot
--#ilASM{ jmp :fin

--/*
procedure :%opProfout(:%)
end procedure -- (for Edita/CtrlQ)
--*/

    :%opProfout                 -- dump_profile()
---------------
        -- because opProfile/opLnp/opLnpt must be in the optable, so too
        --  must dump_profile, and be invoked in this manner rather than
        --  using a normal hll call.
        [32]
            mov edx,routine_id(dump_profile)    -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[dump_profile][S_Ltot])
            call :%opFrame
--EXCEPT
--          mov dword[ebp+16],:pdret
--          mov dword[ebp+28],:pdret
            pop dword[ebp+28]
            jmp $_il                            -- jmp code:dump_profile
        [64]
            mov rdx,routine_id(dump_profile)    -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[dump_profile][S_Ltot])
            call :%opFrame
--EXCEPT
--          mov qword[rbp+32],:pdret
--          mov qword[rbp+56],:pdret
            pop qword[rbp+56]
            jmp $_il                            -- jmp code:dump_profile
        []
--        ::pdret   
--          ret

--/*
procedure :%opProfile(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opProfile                 -- profile([edi]) -- (where [edi] is 0|1)
---------------
        --  This just sets profileon, it is opLnp/opLnpt (profile/profile_time) which do the real work.
        [32]
            -- calling convention:
            --  mov eax,[p1]        -- (or imm [1/2])
            --  call :%opProfile    -- profile(eax)
            cmp eax,2
            jbe @f
                pop edx
                mov al,90   -- e90atpmb01
                sub edx,1
                jmp :!iDiag
                int3
          @@:
            mov [profileon],eax
            lea edi,[pt]
            fldz
            call :%pStoreFlt
            ret
        [64]
            -- calling convention:
            --  mov rax,[p1]        -- (or imm [1/2])
            --  call :%opProfile    -- profile(rax)
            cmp rax,2
            jbe @f
                pop rdx
                mov al,90   -- e90atpmb01
                sub rdx,1
                jmp :!iDiag
                int3
          @@:
            mov [profileon],rax
            lea rdi,[pt]
            fldz
            call :%pStoreFlt
            ret
        []

--  ::e91pie4
--  pop eax
--  ::e91pie
--  ?9/0

--/*
procedure :%opLnp(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opLnp                 -- profile(count times executed) this line. Only valid when interpreting
-----------
        [32]

--  mov ecx,[ebp+8]         -- current routine
--  mov esi,[esi+8]         -- symtab[current_routine][S_FPno(=3)]
            -- calling convention:
            --  mov eax,imm32   -- lineno
            --  mov ecx,imm32   -- fileno
            --  call :%opLnp    -- profile_count(ecx,eax)
            --  trashes all registers
--          mov edx,[profileon]
--          test eax,eax
--          jle :e91pie             -- profile internal error (lineno<=0)
--          test edx,edx
            cmp [profileon],0
            je @f
--              int3
                push eax                            --[1] lineno
                push ecx                            --[2] fileno
                mov edx,routine_id(profile_count)   -- mov edx,imm32 (sets K_ridt)
                mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[debug][S_Ltot])
                call :%opFrame
                pop dword[ebp]                      --[2] fileno
                pop dword[ebp-4]                    --[1] lineno
--EXCEPT
--              mov dword[ebp+16],:lnpret
--              mov dword[ebp+28],:lnpret
                pop dword[ebp+28]
                jmp $_il                            -- jmp code:profile_count
--            ::lnpret  
--  mov esi,[ebp+24]        -- symtab
--  mov ecx,[ebp+8]         -- current routine
--  mov esi,[esi+ecx*4-4]   -- symtab[current_routine]
--  shl esi,2               -- convert to real address
--  mov esi,[esi+8]         -- symtab[current_routine][S_FPno(=3)]
--  test esi,esi
--  jle e91pie
--  mov edi,[ptab]          -- raw address
--  cmp esi,[edi-12]        -- check length
--  jg e91pie
--  mov edi,[edi+esi*4-4]   -- ptab[fileno]
--  shl edi,2
--  cmp eax,[edi-12]        -- check length
--  jg e91pie
--DEV prevent this going over #3FFFFFFF
--  inc dword[edi+eax*4-4]  -- ptab[fileno][lineno]
        [64]
            -- calling convention:
            --  mov rax,imm32   -- lineno
            --  mov rcx,imm32   -- fileno
            --  call :%opLnp    -- profile_count(rcx,rax)
            --  trashes all registers
            cmp [profileon],0
            je @f
                push rax                            --[1] lineno
                push rcx                            --[2] fileno
                mov rdx,routine_id(profile_count)   -- mov rdx,imm32 (sets K_ridt)
                mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[debug][S_Ltot])
                call :%opFrame
                pop qword[rbp]                      --[2] fileno
                pop qword[rbp-8]                    --[1] lineno
--EXCEPT
--              mov qword[rbp+32],:lnpret
--              mov qword[rbp+56],:lnpret
                pop qword[rbp+56]
                jmp $_il                            -- jmp code:profile_count
--            ::lnpret  
        []
          @@:
            ret


--/*
procedure :%opLnpt(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opLnpt                 -- profile_time this line. Only valid when interpreting
------------
        -- Times are recorded on move to new line; first and last are
        -- not properly accounted for (on this *asm*! backend).
        -- NB: the asm b/end uses rdtsc rather than time()
        [32]
            -- calling convention:
            --  mov eax,imm32   -- lineno
            --  mov ecx,imm32   -- fileno
            --  call :%opLnpt   -- profile_time(ecx,eax)
            --  trashes all registers
--          mov ecx,[profileon]
--          test ecx,ecx
            cmp [profileon],0
            je @f
                push eax                            --[1] lineno
                push ecx                            --[2] fileno
                mov edx,routine_id(profile_time)    -- mov edx,imm32 (sets K_ridt)
                mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[debug][S_Ltot])
                call :%opFrame
                pop dword[ebp]                      --[2] fileno
                pop dword[ebp-4]                    --[1] lineno
--EXCEPT
--              mov dword[ebp+16],:lnptret
--              mov dword[ebp+28],:lnptret
                pop dword[ebp+28]
                jmp $_il                            -- jmp code:profile_time
--            ::lnptret 
--/*
    push eax    -- save lineno [DEV *4?]
    -- first, update [ptaddr] with diff from [pteax/edx].
    cmp [pteax],0               -- DEV should this be [ptaddr]?
    jne :lnpt_eax_not_zero
    cmp [ptedx],0
    jz :lnpt_end_if
  ::lnpt_eax_not_zero
    rdtsc
    sub eax,[pteax]
    sbb edx,[ptedx]
    and edx,0x7FFFFFFF
    push edx
    push eax
    fild qword[esp]
    add esp,8
    
    mov edx,[ptaddr]
    mov edx,[edx]
    cmp edx,h4
    jle :lnpt_new_float
    shl edx,2
    cmp byte[edx-1],0x12
    je :e91pie4
    fld qword[edx]
    faddp
    fstp qword[edx]
    jmp :lnpt_end_if
  ::lnpt_new_float
--DEV check which:
    mov edx,[ptaddr]
    mov edi,[ptaddr]
    call :%pStoreFlt
    mov [edx],eax
  ::lnpt_end_if
    rdtsc
    mov [pteax],eax
    mov [ptedx],edx
    mov ecx,[ebp+8]         -- current routine
    mov esi,[ebp+24]        -- symtab
    mov esi,[esi+ecx*4-4]   -- symtab[current_routine]
--  shl esi,2               -- convert to real address
    mov esi,[ebx+esi*4+8]   -- symtab[current_routine][S_FPno(=3)]
--  test esi,esi
--  jle :e91pie4
    mov edi,[ptab]          -- raw address
--  cmp esi,[edi-12]        -- check length
--  jg :e91pie4
    mov edi,[edi+esi*4-4]   -- ptab[fileno]
    shl edi,2
    pop eax                 -- lineno
--  cmp eax,[edi-12]        -- check length
--  jg :e91pie
    -- save addr of ptab[fno][line] instead of fno&line:
    lea edx,[edi+eax*4-4]
    mov [ptaddr],edx        -- (updateme at next opLnpt)
--*/
        [64]
            -- calling convention:
            --  mov rax,imm32   -- lineno
            --  mov rcx,imm32   -- fileno
            --  call :%opLnpt   -- profile_time(rcx,rax)
            --  trashes all registers
            cmp [profileon],0
            je @f
                push rax                            --[1] lineno
                push rcx                            --[2] fileno
                mov rdx,routine_id(profile_time)    -- mov rdx,imm32 (sets K_ridt)
                mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[debug][S_Ltot])
                call :%opFrame
                pop qword[rbp]                      --[2] fileno
                pop qword[rbp-8]                    --[1] lineno
--EXCEPT
--              mov qword[rbp+32],:lnptret
--              mov qword[rbp+56],:lnptret
                pop qword[rbp+56]
                jmp $_il                            -- jmp code:profile_time
--            ::lnptret 
        []
          @@:
            ret
      }
--!*/


-- Final note:
-- There is no way to profile a pre-built executable, since allfiles is
--  not available, and hence both creating ptab and later printing it
--  is problematic at best.
--  I have considered this and decided it is impractical and pointless.
--  I will however list my findings:
--    First job would be to change lineinfo() in pemit.e, specifically 
--    the 'if not bind then' condition. Obviously I am not about to do 
--    that on any 'official' release due to the performance overhead.
--    It is critical that ptab matches source file lengths at the time
--    of compilation. The best way to do this is save an ltab, which
--    contains the needed lengths of each ptab element. A reasonable
--    place for this is near T_fileset. The user app needs to create
--    a ptab, before any profile or profile_time statement. You would
--    need some way to pass this to the interpreter as opInterp does.
--DEV newEmit:
--    [Tip: you can retrieve symtab/ltab via opGetST, and would need
--          a new op like opSetDbg to pass over the new ptab.]
--    If the sources have been edited since the bind, then at best,
--    costs will show against the wrong lines; were you to base the
--    lengths of ptab on the current source, and they were now shorter,
--    you'd get a memory corruption [opLnp/t do no bounds checking!] 
--    probably followed by some indecipherable machine error, and/or
--    not altogether unlikely a machine lockup.
--    At the end of the program, the app could recreate allfiles and
--    invoke this. It would have to define/set the following globals:
--      global sequence ptab, allfiles, filenames, filepaths
--              (expandedYet/expfiles can be recreated here)
--      global integer profileon
--      global constant OptProfile=1, OptProfileTime=2
--    Obviously, profileon must match that at compilation time, or
--    else the results are undefined, almost certainly meaningless.
--  Of course you should not forget that "with profile" interferes
--    with program execution, eg if I have:
--          integer i,j,k
--              j = i
--              k = j+2
--      then without profile, I get:
--              mov eax,[i]
--              mov [j],eax
--              add eax,2
--              mov [k],eax
--      whereas with it, I get:
--              call opLnp
--              mov eax,[i]
--              mov [j],eax
--              call opLnp
--              mov eax,[j]
--              add eax,2
--              mov [k],eax
--  (verify this by running "p -d! test" and examining list.asm)
