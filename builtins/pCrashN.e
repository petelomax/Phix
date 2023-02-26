--
-- pCrashN.e
-- =========
--
--  Phix implementation of crash() and assert() routines.
--  crash_message (:%pCrashMsg) is part of the optable/pDiagN.e, this is not.
--
without debug -- (crash on user app line, and don't trace into this)

global procedure crash(string msg, object args={}, integer nFrames=1)
--  if atom(args) or length(args) then
    if args!={} then
        msg = sprintf(msg, args)
    end if
    if nFrames<1 then ?9/0 end if
    crash_message(msg)  -- (yes, that increfs msg correctly, I just checked!)
    #ilASM{
        -- while e/rax do issue fake opRetf (including this routine!)
        [32]
            mov eax,[nFrames]
          @@:
            mov edx,[ebp+28]            -- return addr
            mov dword[ebp+28],:fakeRet  -- replace return address
            push edx
            jmp :%opRetf
          ::fakeRet
            pop edx
            sub eax,1
            jnz @b
            sub edx,1
        [64]
            mov rax,[nFrames]
          @@:
            mov rdx,[rbp+56]            -- return addr
            mov dword[rbp+56],:fakeRet  -- replace return address
            push rdx
            jmp :%opRetf
          ::fakeRet
            pop rdx
            sub rax,1
            jnz @b
            sub rdx,1
        []
            mov al,68           -- e68crash
            jmp :!iDiag
            int3
          }
end procedure

global procedure assert(bool condition, string msg="", object args={}, integer nFrames=1)
    string colon = iff(length(msg)?":":"")
    if not condition then crash("assertion failure"&colon&msg,args,nFrames+1) end if
end procedure

global procedure asserteq(object a, b, string msg="", object args={}, integer nFrames=1)
    sequence tests = {"=","==","<","<=",">",">=","!="} -- (nb [2..7] flippable)
    integer k = find(msg,tests), l = max(k,1)
    bool bOK
    switch l do
        case 1,
             2: bOK := a==b
        case 3: bOK := a<b
        case 4: bOK := a<=b
        case 5: bOK := a>b
        case 6: bOK := a>=b
        case 7: bOK := a!=b
    end switch
    if not bOK then
        string m = iff(k or msg=""?"":" ("&sprintf(msg,args)&")"),
               t = tests[9-max(2,l)]
        crash("assertion failure: %v %s %v%s",{a,t,b,m},nFrames+1)
    end if
end procedure
