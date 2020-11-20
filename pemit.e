--
-- pemit.e
--
--constant trimST = 01      --DEV I think this can go now 
constant debug = 0

constant show_full_symtab = 0   -- You should NOT want or need to set this to 1.
                                -- (It was used to improve the use of opCleanup.)
                                -- If anything, if you are new to symtab dumps you
                                -- probably want a cut down version - by using the
                                -- "-nodiag" command line option.

constant -- not global: use only in scanforShortJmp/blurph, not before.
--  isShortJmp  = #004000   -- isJmp that has been found to fit in a byte.
    isShortJmp  = #010000   -- isJmp that has been found to fit in a byte.
                            -- (fixup as isJmp, patch 5->2 or 6->2)

--function o(integer i)
--  return {floor(i/#40)+'0',floor(and_bits(i,#38)/8)+'0',and_bits(i,7)+'0'}
--end function
--for i=0 to 255 do
--  printf(1,"o%s=#%02x,",{o(i),i})
--  if remainder(i,8)=7 then puts(1,"\n") else puts(1," ") end if
--end for
--abort(0)

--
-- Fixup code: branch straighten, check for dword offsets that fit in a byte,
-- ==========   calculate actual code size, convert opcode indexes to dword
--              offsets into VM, relative addresses and variable indexes into
--              absolute addresses, and adjust all other offsets as necessary
--              by the changing of any dword->byte form instructions. (whew)
--
-- if binding then create .exe file (using specified resource file) else
-- if interpreting just poke to memory and execute it immediately.
--
-- if binding, then we need to write symtab to section 6, whereas
-- if interpreting we can just use the existing symtab directly.
--
-- Note that if binding, there is no source at the point of execution, hence
--  there can be no trace, profile, or profile_time going on.
--
-- Also note that critical parts of this process, specifically those where
--  performance matters and anything to do with licencing issues, have been
--  coded in low level assembly (not open source for obvious reasons).
--

--sequence mzpe -- MZ and PE headers, #400 bytes
string mzpe
-- verify the compiler is working properly:
--DEV 31/1/14 (broken on pth)
--/**/ #isginfo{mzpe,0b1000,MIN,MAX,integer,#400} -- verify this is a string

-- standard PE header fields:
-- Note that file offsets given are 0-based, add 1 to index mzpe (as done in getmzpe,setmzpe, etc).
--global atom -- 7/1/09
global integer 
     ImageBase          -- dword @ #B4, must be #400000
--atom SectionAlignment,    -- dword @ #B8, must be #1000
integer SectionAlignment,   -- dword @ #B8, must be #1000
        FileAlignment,  -- dword @ #BC, must be #200 
        SAless1,            -- #00000FFF for rounding to section alignment
        SAmask,         -- #FFFFF000
        FAless1,            -- #000001FF for rounding to file alignment
        FAmask,         -- #FFFFFE00
        SubsystemVersion,   -- dword @ #C8, must be ssv310 or ssv400
        SizeOfImage,        -- dword @ #D0, rounded up to SectionAlignment, eg #8000
        Subsystem,      -- word @ #DC, must be CUI or GUI
        ITaddr,         -- RVA Import Table address, == ISvaddr         -- dword @ #100, eg #2000
        ITsize,         -- RVA Import Table size, == ISvsize            -- dword @ #104, eg #31D
        RTaddr,         -- RVA Resource Table address == RSvaddr        -- dword @ #108, eg #5000
        RTsize          -- RVA Resource Table size == RSvsize           -- dword @ #10C, eg #504
--global atom -- 7/1/09
global integer 
     -- 6 sections:                                     -- offset:examples     vsize      vaddr      rsize      raddr
     DVvsize, DVvaddr, DVrsize, DVraddr, -- data for vm (fixed)         -- #180:#54C  #184:#1000 #188:#600  #18C:#400
     ISvsize, ISvaddr, ISrsize, ISraddr, -- import section (fixed)      -- #1A8:#31D  #1AC:#2000 #1B0:#400  #1B4:#A00
     VMvsize, VMvaddr, VMrsize, VMraddr, -- the virtual machine (fixed) -- #1D0:#1ED8 #1D4:#3000 #1D8:#2000 #1DC:#E00
     RSvsize, RSvaddr, RSrsize, RSraddr, -- resource section (var len)  -- #1F8:#504  #1FC:#5000 #200:#600  #204:#2E00
     CSvsize, CSvaddr, CSrsize, CSraddr, -- user code (var start & len) -- #220:#10   #224:#6000 #228:#200  #22C:#3400
     DSvsize, DSvaddr, DSrsize, DSraddr  -- user data (var start & len) -- #248:#4    #24C:#7000 #250:#200  #254:#3600

constant mzpelen = #400,        -- == DVraddr
         ssv310 = #000A0003,    -- SubsystemVersion: 3.10
         ssv400 = #00000004,    -- SubsystemVersion: 4.00
         CUI = 3,               -- Subsystem: console app
         GUI = 2                -- Subsystem: gui app

    mzpe = repeat(' ',mzpelen)

constant WORD = 2,
         DWORD = 4, 
         bytemul = {0,#100,#10000,#1000000}

function getmzpe(integer offset, integer dsize)
-- reassemble dsize bytes at offset in mzpe
-- Note that offset as passed is 0-based, adjusted here(+1) to index mzpe.
-- dsize is WORD or DWORD
atom res
    res = mzpe[offset+1]
    for i=2 to dsize do
        res += mzpe[offset+i]*bytemul[i]
    end for
    return res
end function

procedure setmzpe(integer offset, atom v, integer dsize)
-- breakup v into dsize bytes in mzpe at offset.
-- Note that offset as passed is 0-based, adjusted here(+1) to index mzpe.
-- dsize is WORD or DWORD
    for i=1 to dsize do
        mzpe[offset+i] = and_bits(v,#FF)
        v = floor(v/#100)
    end for
end procedure

sequence mzpeErrMsg
constant ueof = "unexpected end of file",
         noMZ = "MZ missing",
         bpeo = "bad PE offset",
         noPE = "PE missing",
         nsl4 = "number of sections is less than 4",    -- rsrc.exe files
         nsn6 = "number of sections is not 6",          -- p.exe file
         bibv = "bad ImageBase value",
         bsav = "bad SectionAlignment value",
         bfav = "bad FileAlignment value",
         ussv = "unrecognised Subsystem version",
         uss  = "unrecognised Subsystem",
         sned = "section name in error (.data)",
         snei = "section name in error (.idata)",
         snet = "section name in error (.text)",
         sner = "section name in error (.rsrc)",
         ismm = "import table/section mismatch",
         rsmm = "resource table/section mismatch",
         dvve = "data for vm section 1 virtual address error",
         dvre = "data for vm section 1 raw address error",
         isve = "import section 2 virtual address error",
         isre = "import section 2 raw address error",
         vmve = "vm section 3 virtual address error",
         vmre = "vm section 3 raw address error",
         rsve = "resource section 4 virtual address error",
         rsre = "resource section 4 raw address error",
         csve = "code section 5 virtual address error",
         csre = "code section 5 raw address error",
         dsve = "data section 6 virtual address error",
         dsre = "data section 6 raw address error",
         soie = "size of image/eof of section mismatch"

function mzErr(string msg)
-- parameter is one of the above constants

-- verify the compiler is working properly:
--/**/ #isginfo{mzpeErrMsg,string,MIN,MAX,integer,-2}

    mzpeErrMsg = msg
    return 0

end function

function mzpeCheckSection(atom addr, sequence txt)
-- Note that addr as passed is 0-based, adjusted here(+1) to index mzpe.
integer c
    for i=1 to 8 do
        c = mzpe[addr+i]
        if i>length(txt) then
            if c!=0 then return 0 end if        -- section name in error
        else
            if c!=txt[i] then return 0 end if   -- section name in error
        end if
    end for
    return 1
end function

function readmzpe(integer fn, integer asRsrc)
-- read the mzheaders from p.exe or rsrc.exe's (asRsrc=1 for the latter)
-- returns 1 on success, 0 on failure (see mzpeErrMsg).
-- The following fields must be updated before mzpe is rewritten:
--   SubsystemVersion,SizeOfImage,Subsystem,RTsize,RSvsize,RSrsize,
--   CSvsize,CSvaddr,CSrsize,CSraddr,DSvsize,DSvaddr,DSrsize,DSraddr
-- This routine can be called again on the newly created file as a sanity check.
-- The headers for rsrc.exe files should be read before p.exe, and when both
--  have been read the RSraddr fields should be checked for equality.
integer c
--atom PEoffset, nSections  -- 7/1/09
integer PEoffset, nSections
--
-- The main job being done here is a flat read of a fixed (#400 byte) structure,
--  (although several sub-structures within that do exist); the wobbly and/or
--  non-standard indents I have used are as follows:
--
--  main code (indented thus whether inside "if not asRsrc" or not)
--      if not asRsrc
--          sanity checks (indented thus whether inside "if not asRsrc" or not)
--
--#without reformat
--  mzpe = repeat(0,mzpelen)
--  mzpe = repeat(' ',mzpelen)
    c = 0
    for i=1 to mzpelen do
        c = getc(fn)
        c = and_bits(c,#FF)     -- added 23/2/10 (keeps mzpe T_string)
        mzpe[i] = c
    end for
            if c=-1 then return mzErr(ueof) end if
            if getmzpe(#00,WORD)!=#5A4D then return mzErr(noMZ) end if
    PEoffset = getmzpe(#3C,DWORD)
            if PEoffset!=#80 then return mzErr(bpeo) end if
            if getmzpe(PEoffset,DWORD)!=#4550 then return mzErr(noPE) end if
    nSections = getmzpe(#86,WORD)
        if not asRsrc then
            if nSections!=6 then return mzErr(nsn6) end if
        else
            if nSections<4 then return mzErr(nsl4) end if
        end if
    ImageBase = getmzpe(#B4,DWORD)
    SectionAlignment = getmzpe(#B8,DWORD)
    FileAlignment = getmzpe(#BC,DWORD)
            if ImageBase!=#400000 then return mzErr(bibv) end if
            if SectionAlignment!=#1000 then return mzErr(bsav) end if
            if FileAlignment!=#200 then return mzErr(bfav) end if
    SAless1 = SectionAlignment-1    -- ie #00000FFF
    SAmask = not_bits(SAless1)      -- ie #FFFFF000
    FAless1 = FileAlignment-1       -- ie #000001FF
    FAmask = not_bits(FAless1)      -- ie #FFFFFE00
    SubsystemVersion = getmzpe(#C8,DWORD)                                       -- [updateme -- almost done]
            if SubsystemVersion!=ssv310 
            and SubsystemVersion!=ssv400 then
                return mzErr(ussv)
            end if
    SizeOfImage = getmzpe(#D0,DWORD)                                            -- [updateme -- DONE]
    Subsystem = getmzpe(#DC,WORD)                                               -- [updateme -- almost done]
            if Subsystem!=CUI and Subsystem!=GUI then return mzErr(uss) end if
    ITaddr = getmzpe(#100,DWORD)    -- Import Table address
    ITsize = getmzpe(#104,DWORD)    -- Import Table size
    RTaddr = getmzpe(#108,DWORD)    -- Resource Table address
    RTsize = getmzpe(#10C,DWORD)    -- Resource Table size                          -- [updateme - DONE]
            if not mzpeCheckSection(#178,".data") then return mzErr(sned) end if
        if not asRsrc then
    DVvsize = getmzpe(#180,DWORD)
    DVvaddr = getmzpe(#184,DWORD)
    DVrsize = getmzpe(#188,DWORD)
    DVraddr = getmzpe(#18C,DWORD)
            if DVvaddr!=#1000 then return mzErr(dvve) end if
            if DVraddr!=#400 then return mzErr(dvre) end if
            if not mzpeCheckSection(#1A0,".idata") then return mzErr(snei) end if
    ISvsize = getmzpe(#1A8,DWORD)
    ISvaddr = getmzpe(#1AC,DWORD)
    ISrsize = getmzpe(#1B0,DWORD)
    ISraddr = getmzpe(#1B4,DWORD)
            if ISvaddr!=and_bits(DVvaddr+DVvsize+SAless1,SAmask) then return mzErr(isve) end if
            if ISraddr!=DVraddr+DVrsize then return mzErr(isre) end if
            if ISvsize!=ITsize then return mzErr(ismm) end if
            if ISvaddr!=ITaddr then return mzErr(ismm) end if
            if not mzpeCheckSection(#1C8,".text") then return mzErr(snet) end if
    VMvsize = getmzpe(#1D0,DWORD)
    VMvaddr = getmzpe(#1D4,DWORD)
    VMrsize = getmzpe(#1D8,DWORD)
    VMraddr = getmzpe(#1DC,DWORD)
            if VMvaddr!=and_bits(ISvaddr+ISvsize+SAless1,SAmask) then return mzErr(vmve) end if
            if VMraddr!=ISraddr+ISrsize then return mzErr(vmre) end if
        end if
            if not mzpeCheckSection(#1F0,".rsrc") then return mzErr(sner) end if
    RSvsize = getmzpe(#1F8,DWORD)                                               -- [updateme - DONE]
    RSvaddr = getmzpe(#1FC,DWORD)
    RSrsize = getmzpe(#200,DWORD)                                               -- [updateme - DONE]
    RSraddr = getmzpe(#204,DWORD)                                                   -- [verifyme - DONE]
            if RSvsize!=RTsize then return mzErr(rsmm) end if
            if RSvaddr!=RTaddr then return mzErr(rsmm) end if
        if not asRsrc then
            if RSvaddr!=and_bits(VMvaddr+VMvsize+SAless1,SAmask) then return mzErr(rsve) end if
            if RSraddr!=VMraddr+VMrsize then return mzErr(rsre) end if
            if not mzpeCheckSection(#218,".text") then return mzErr(snet) end if
    CSvsize = getmzpe(#220,DWORD)                                               -- [updateme - DONE]
    CSvaddr = getmzpe(#224,DWORD)                                               -- [updateme - DONE]
    CSrsize = getmzpe(#228,DWORD)                                               -- [updateme - DONE]
    CSraddr = getmzpe(#22C,DWORD)                                               -- [updateme - DONE]
            if CSvaddr!=and_bits(RSvaddr+RSvsize+SAless1,SAmask) then return mzErr(csve) end if
            if CSraddr!=RSraddr+RSrsize then return mzErr(csre) end if
            if not mzpeCheckSection(#240,".data") then return mzErr(sned) end if
    DSvsize = getmzpe(#248,DWORD)                                               -- [updateme - DONE]
    DSvaddr = getmzpe(#24C,DWORD)                                               -- [updateme - DONE]
    DSrsize = getmzpe(#250,DWORD)                                               -- [updateme - DONE]
    DSraddr = getmzpe(#254,DWORD)                                               -- [updateme - DONE]
            if DSvaddr!=and_bits(CSvaddr+CSvsize+SAless1,SAmask) then return mzErr(dsve) end if
            if DSraddr!=CSraddr+CSrsize then return mzErr(dsre) end if
            if SizeOfImage!=and_bits(DSvaddr+DSvsize+SAless1,SAmask) then return mzErr(soie) end if
        else
    CSvaddr = and_bits(RSvaddr+RSvsize+SAless1,SAmask)
    CSraddr = RSraddr+RSrsize
        end if
--#with reformat
    return 1    -- all OK
end function

global string divm  -- used by p2asm.e if dumpVM=1
global sequence VMep    -- used by p2asm.e

-- verify the compiler is working properly:
--!/**/ #isginfo{divm,0b0100,MIN,MAX,integer,-2}    -- 0b1000 better?! (see aside in readdivm())
--!/**/ #isginfo{divm,0b1000,MIN,MAX,integer,-2}    -- (nb 0b1100 (ie 12) is /worse/ )
--!/**/ #isginfo{divm,0b1100,MIN,MAX,integer,-2}    -- I can live with this...
--!/**/ #isginfo{divm,0b1000,MIN,MAX,integer,-2}    -- Yay! (23/02/10)
--!/**/ #isginfo{divm,0b1000,MIN,MAX,integer,-1}    -- OK? (24/06/10)
--/**/  #isginfo{divm,0b1000,MIN,MAX,integer,-2}    -- Yay! (18/01/12)
--/**/  #isginfo{VMep,0b0100,MIN,MAX,integer,-2}

function divmDword(integer i)
-- this routine is used to load VMep (virtual machine entry points table)
-- NB: i is 0-based
    return divm[i+1]+divm[i+2]*#100+divm[i+3]*#10000+divm[i+4]*#1000000
end function

procedure setdivm(integer offset, atom v, integer dsize)
-- breakup v into dsize bytes in divm at offset.
-- used I think only to locate symtab and threadstack.
-- Note that offset as passed is 0-based, adjusted here(+1) to index divm.
-- dsize is WORD or DWORD
    for i=1 to dsize do
        divm[offset+i] = and_bits(v,#FF)
        v = floor(v/#100)
    end for
end procedure

integer fn, fnr,
        asmoptions,
        vmaxpos

procedure readdivm()
-- read the data for vm, imports and vm (sections 1..3) verbatim:
--  If we are bootstrapping on RDS Eu, then assume we have a fresh pstub.exe, which
--  is neither encrypted nor publically distributed, which we can/must read.
--  For Phix-hosted operation, there is already one in the root, albeit in need of
--  some deciphering.
integer c, b
atom magic, VMe
sequence x8

-- verify the compiler is working properly:
--/**/  #isginfo{x8,0b1000,MIN,MAX,integer,8}

    if DEBUG then
        if where(fn)!=#400 then ?9/0 end if
    end if
--  divm = repeat(0,RSraddr-DVraddr)
--  divm = repeat(' ',RSraddr-DVraddr)  -- nope, makes it T_sequence(0b1100 aka 12), not T_string
    divm = repeat(' ',RSraddr-DVraddr)

-- Update 23/2/10: now addressed with and_bits hack below.
--      <aside>
--          OK, divm gets T_Dsq *here*:: c=getc(fn) leaves c as -1..255, obviously (now I see it)
--          that -1 would(/does) make divm a T_Dsq..... No biggie, just look no more. ;-)
--      </aside>

    c = 0
    for i=1 to length(divm) do
        c = getc(fn)
--      divm[i] = c
        b = and_bits(c,#FF)     -- 23/02/10: c is -1..255, b is 0..255, keeps divm T_string...
        divm[i] = b
    end for
    if c=-1 then ?9/0 end if
    VMep = {}
    magic = divmDword(#24)
    if magic!=#65762B50 then ?9/0 end if
    c = #28
    while 1 do
        VMe = divmDword(c)
        c += 4
        if VMe = magic then exit end if
--      VMep = append(VMep,VMe)
        b = VMe
        VMep = append(VMep,b)
    end while
    asmoptions = divmDword(c)   -- (verified in unused_cleanup, in pilxl.e)
    if newEBP then
        if newEBP!=4 then ?9/0 end if
        asmoptions = or_bits(asmoptions,newEBP)
    elsif and_bits(asmoptions,4) then
        asmoptions -= 4
    end if
    -- (8 = batchbit)
    if batchmode then
        asmoptions = or_bits(asmoptions,8)
    elsif and_bits(asmoptions,8) then
        asmoptions -= 8
    end if
    setdivm(c, asmoptions, DWORD)

    --
    -- The following is used to check whether the build involved RDS Eu;
    --  see t28sprntf.exw for more details. Note that "exw p -cp" will
    --  "iron this one out", but "exw p test.exw" won't. [DEV not that you an do that anymore?]
    --
    x8 = atom_to_float64(1e308)
--  printf(1,"x8=#%02x,#%02x,#%02x,#%02x,#%02x,#%02x,#%02x,#%02x\n",x8)
    c += 4
    for i=1 to 8 do
        c += 1
        b = x8[i]
        divm[c] = b
    end for
--DEV (untested) try:
--  divm[c+4..c+11] = atom_to_float64(1e308)
    vmaxpos = c
--?c -- 844
end procedure

function getdivmstring(integer address)
integer ch
string res = ""
    while 1 do -- (until ch = '\0')
        address += 1
        ch = divm[address]
        if ch='\0' then exit end if
        res &= ch
    end while
    return res
end function

function firstatom(sequence sets)
sequence si
object sij
    for i=1 to length(sets) do
        si = sets[i]
        for j=1 to length(si) do
            sij = si[j]
            if atom(sij) then
                if sij=0 then ?9/0 end if
                return sij
            end if
        end for
    end for
    return 0
end function

sequence Names          -- eg {"kernel32.dll",...}
sequence HintNames      -- eg {{{#40470,...},{"AllocConsole",...}},{..}}
sequence thunkaddrs     -- \ scratch vars, set from HintNames[i],
sequence thunknames     -- / where i is eg find("kernel32.dll",Names)

procedure peek_import_table()
-- NB: any fixes here probably also apply to read_import_table() below,
--     and possibly also demo\arwendemo\filedump.exw
integer Base = #00400000
integer e_lfanew
integer machine
integer ImageBase
integer nSections
integer address
integer NumberOfRvaAndSizes
integer RelativeVirtualAddress2
integer RVASize2
integer SH_VirtualSize
integer SH_VirtualAddr
integer PointerToRawData
integer ImportBase
integer ThunkBase
integer Name
integer FirstThunk
integer thunkaddr
integer k
string text
atom RVA
integer signed
integer Hint

    if peek({Base,2})!="MZ" then ?9/0 end if
    e_lfanew = Base+peek4u(Base+#3C)
    if peek({e_lfanew,2})!="PE" then ?9/0 end if
    machine = peek2u(e_lfanew+4)
    if machine=0x014C then
        machine = 32
        ImageBase = peek4u(e_lfanew+#34)
        NumberOfRvaAndSizes = #74 -- (offset)
    elsif machine=0x8664 then
        machine = 64
        ImageBase = peek4u(e_lfanew+#30)
        if peek4u(e_lfanew+#34)!=0 then ?9/0 end if
        NumberOfRvaAndSizes = #84 -- (offset)
    else
        ?9/0 -- return fatal("unknown architecture")
    end if
    nSections = peek2u(e_lfanew+6)
    address = e_lfanew+NumberOfRvaAndSizes
    NumberOfRvaAndSizes = peek4u(address)
    if NumberOfRvaAndSizes<2 then ?9/0 end if
    RelativeVirtualAddress2 = peek4u(address+12)
    RVASize2 = peek4u(address+16)
    address += NumberOfRvaAndSizes*8+4
    for i=1 to nSections do
        SH_VirtualSize = peek4u(address+8)
        SH_VirtualAddr = peek4u(address+12)
        if RelativeVirtualAddress2>=SH_VirtualAddr
        and RelativeVirtualAddress2+RVASize2<=SH_VirtualAddr+SH_VirtualSize then
            PointerToRawData = peek4u(address+20)
            exit
        end if
        address += 40
    end for

    address = SH_VirtualAddr+ImageBase

    ImportBase = address-RelativeVirtualAddress2
    ThunkBase = ImageBase-ImportBase

    --
    -- Collect any pointers we find, and (hopefully) get through them all later.
    --
    Names = {}
    HintNames = {}

    -- An array of IMAGE_IMPORT_DESCRIPTOR (ends with an an all null one)
    while 1 do
        Name = peek4u(address+12)
        FirstThunk = peek4u(address+16)
        address += 20   -- sizeofstruct(IMAGE_IMPORT_DESCRIPTOR)
        if Name=0 and FirstThunk=0 then exit end if
        Names = append(Names,Name+ImportBase)
        HintNames = append(HintNames,FirstThunk+ImportBase)
    end while
    --
    -- The IMAGE_IMPORT_DESCRIPTOR above contain pointers (RVAs), so 
    -- presumably the rest of this section can be written in any order. 
    -- Try to do things in file address order.
    --
    while 1 do
        k = find(address,Names)
        if k!=0 then
            text = peek_string(address)
            Names[k] = lower(text)
            address += length(text)+1
        else
            k = find(address,HintNames)
            if k=0 then
                -- I would prefer to do it in file order, but if  
                --  things have been written out higgledy-piggledy...
                address = firstatom({Names,HintNames})
                if address=0 then exit end if
            else
                -- An array of IMAGE_THUNK_DATA32/64 (which does not help much..)
                -- one d/qword RVA per line, until we hit a null...
                thunkaddrs = {}
                thunknames = {}
                while 1 do
                    signed = 0
                    thunkaddr = address+ThunkBase
                    if machine=32 then
                        RVA = peek4u(address)
                        if and_bits(RVA,#80000000) then
                            signed = 1
                        end if
                        address += 4
                    else
                        RVA = peek(address+7)
                        if and_bits(RVA,#80) then
                            signed = 1
                        else
                            RVA = peek4u(address) -- 8 bytes really...
                            if peek4u(address+4)!=0 then ?9/0 end if
                        end if
                        address += 8
                    end if
                    if RVA=0 then exit end if
                    if not signed then
                        thunkaddrs = append(thunkaddrs,thunkaddr)
                    end if
                end while
                for i=1 to length(thunkaddrs) do
                    Hint = peek2u(address)
                    if Hint!=0 then
                        -- oops (import by ordinal or something?)
                        thunkaddrs = {-1}
                        thunknames = {"some error"}
                        exit
                    end if
                    address += 2
                    text = peek_string(address)
                    thunknames = append(thunknames,text)
                    address += length(text)+1
                end for
--DEV or add to ttree with terminator of -4?
                HintNames[k] = {thunkaddrs,thunknames}
            end if
        end if
    end while
end procedure

procedure read_import_table()
-- NB: any fixes here probably also apply to peek_import_table()
integer e_lfanew
integer machine
integer ImageBase
integer nSections
integer address
integer NumberOfRvaAndSizes
integer RelativeVirtualAddress2
integer RVASize2
integer SH_VirtualSize
integer SH_VirtualAddr
integer PointerToRawData
integer ImportBase
integer ThunkBase
integer Name
integer FirstThunk
integer thunkaddr
integer k
string text
atom RVA
integer signed

    e_lfanew = getmzpe(#3C,4)
    if getmzpe(e_lfanew,4)!=#00004550 then ?9/0 end if
    machine = getmzpe(e_lfanew+4,2)
    if machine=0x014C then
        machine = 32
        ImageBase = getmzpe(e_lfanew+#34,4)
        NumberOfRvaAndSizes = #74 -- (offset)
    elsif machine=0x8664 then
        machine = 64
        ImageBase = getmzpe(e_lfanew+#30,8)
        NumberOfRvaAndSizes = #84 -- (offset)
    else
        ?9/0 -- return fatal("unknown architecture")
    end if
    nSections = getmzpe(e_lfanew+6,2)
    address = e_lfanew+NumberOfRvaAndSizes
    NumberOfRvaAndSizes = getmzpe(address,4)
    if NumberOfRvaAndSizes<2 then ?9/0 end if
    RelativeVirtualAddress2 = getmzpe(address+12,4)
    RVASize2 = getmzpe(address+16,4)
    address += NumberOfRvaAndSizes*8+4

    for i=1 to nSections do
        SH_VirtualSize = getmzpe(address+8,4)
        SH_VirtualAddr = getmzpe(address+12,4)
        if RelativeVirtualAddress2>=SH_VirtualAddr
        and RelativeVirtualAddress2+RVASize2<=SH_VirtualAddr+SH_VirtualSize then
            PointerToRawData = getmzpe(address+20,4)
            exit
        end if
        address += 40
    end for

    address = PointerToRawData+RelativeVirtualAddress2-SH_VirtualAddr-mzpelen
    ImportBase = address-RelativeVirtualAddress2
    ThunkBase = ImageBase-ImportBase

    --
    -- Collect any pointers we find, and (hopefully) get through them all later.
    --
    Names = {}
    HintNames = {}

    -- An array of IMAGE_IMPORT_DESCRIPTOR (ends with an an all null one)
    while 1 do
        Name = divmDword(address+12)
        FirstThunk = divmDword(address+16)
        address += 20   -- sizeofstruct(IMAGE_IMPORT_DESCRIPTOR)
        if Name=0 and FirstThunk=0 then exit end if
        Names = append(Names,Name+ImportBase)
        HintNames = append(HintNames,FirstThunk+ImportBase)
    end while

    --
    -- The IMAGE_IMPORT_DESCRIPTOR above contain pointers (RVAs), so 
    -- presumably the rest of this section can be written in any order. 
    -- Try to do things in file address order.
    --
    while 1 do
        k = find(address,Names)
        if k!=0 then
            text = getdivmstring(address)
            Names[k] = lower(text)
            address += length(text)+1
        else
            k = find(address,HintNames)
            if k=0 then
                -- I would prefer to do it in file order, but if  
                --  things have been written out higgledy-piggledy...
                address = firstatom({Names,HintNames})
                if address=0 then exit end if
            else
                -- An array of IMAGE_THUNK_DATA32/64 (which does not help much..)
                -- one d/qword RVA per line, until we hit a null...
                thunkaddrs = {}
                thunknames = {}
                while 1 do
                    signed = 0
                    thunkaddr = address+ThunkBase
                    if machine=32 then
                        RVA = divmDword(address)
                        if and_bits(RVA,#80000000) then
                            signed = 1
                        end if
                        address += 4
                    else
                        RVA = divmDword(address+4)
                        if and_bits(RVA,#80000000) then
                            signed = 1
                        else
                            if RVA!=0 then ?9/0 end if
                            RVA = divmDword(address)
                        end if
                        address += 8
                    end if
                    if RVA=0 then exit end if
                    if not signed then
                        thunkaddrs = append(thunkaddrs,thunkaddr)
                        thunknames = append(thunknames,getdivmstring(RVA+ImportBase+2))
                    end if
                end while
                HintNames[k] = {thunkaddrs,thunknames}
            end if
        end if
    end while
end procedure

sequence resource_section
function read_resource_section(integer fn)
integer c
    if seek(fn,RSraddr) then ?9/0 end if
    resource_section = repeat(0,CSraddr-RSraddr)
    c = 0
    for i=1 to length(resource_section) do
        c = getc(fn)
        resource_section[i] = c
    end for
    if c=-1 then return 0 end if
    return 1    -- all OK
end function


constant dorsrc = 0
sequence rsfilename
integer rsrcRSraddr, rsrcRSrsize, rsrcRSvsize, rsrcCSvaddr, rsrcCSraddr

-- used in p2asm.e, plist.e
global sequence code_section
--sequence data_section
--type dst(sequence d)
--  return string(d)
--end type
--dst data_section
sequence data_section
--/**/ #isginfo{data_section,0b1000,MIN,MAX,integer,-2} -- verify this is a string

function isString(object x)
-- avoid "probable logic errors" testing that data_section really is a string
--  (because p.exw contains "without type_check"....)
    return string(x)
end function
if isString(0) then end if  -- and prevent the compiler from optimising it away!

constant m4 = allocate(4),
--       m41 = m4+1,
--       m42 = m4+2,
--       m43 = m4+3,
         m44 = {m4,4}

procedure setcsDword(integer i, atom v)
-- set a dword in code_section
-- NB: offset passed here is 1-based index
--integer c,vi  -- intermediate integers to help the compiler optimise
--  c = and_bits(v,#FF)                 code_section[i] = c     vi = floor(v/#100)
--  c = and_bits(vi,#FF)    i += 1      code_section[i] = c     vi = floor(vi/#100)
--  c = and_bits(vi,#FF)    i += 1      code_section[i] = c     vi = floor(vi/#100)
--  c = and_bits(vi,#FF)    i += 1      code_section[i] = c
--integer b
    poke4(m4, v) -- faster than doing divides etc. (idea from database.e)
--  b = peek(m4)    code_section[i] = b i += 1
--  b = peek(m41)   code_section[i] = b i += 1
--  b = peek(m42)   code_section[i] = b i += 1
--  b = peek(m43)   code_section[i] = b
    code_section[i..i+3] = peek(m44)
end procedure

--function gets5Dword(integer i)
---- used to get routine no for patching parameter info on forward calls
---- NB: i is 1-based
--  return s5[i]+s5[i+1]*#100+s5[i+2]*#10000+s5[i+3]*#1000000
--end function

procedure sets5Dword(integer i, atom v)
-- used for patching parameter info on forward calls
-- NB: offset passed here is 1-based index
--integer c,vi  -- intermediate integers to help the compiler optimise
--  c = and_bits(v,#FF)                 s5[i] = c       vi = floor(v/#100)
--  c = and_bits(vi,#FF)    i += 1      s5[i] = c       vi = floor(vi/#100)
--  c = and_bits(vi,#FF)    i += 1      s5[i] = c       vi = floor(vi/#100)
--  c = and_bits(vi,#FF)    i += 1      s5[i] = c
--integer b
    poke4(m4, v) -- faster than doing divides etc. (idea from database.e)
--  b = peek(m4)    s5[i] = b   i += 1
--  b = peek(m41)   s5[i] = b   i += 1
--  b = peek(m42)   s5[i] = b   i += 1
--  b = peek(m43)   s5[i] = b
    s5[i..i+3] = peek(m44)
end procedure

function getdsDword(integer i)
-- used to get refcount for subsequence/substring patch
-- NB: i is 1-based (** unlike setdsDword **)
    return data_section[i]+data_section[i+1]*#100+data_section[i+2]*#10000+data_section[i+3]*#1000000
end function

procedure setdsDword(integer i, atom v)
-- NB: offset passed here is 0-based index
--integer c,vi  -- intermediate integers to help the compiler optimise
--  c = and_bits(v,#FF)     i += 1      data_section[i] = c     vi = floor(v/#100)
--  c = and_bits(vi,#FF)    i += 1      data_section[i] = c     vi = floor(vi/#100)
--  c = and_bits(vi,#FF)    i += 1      data_section[i] = c     vi = floor(vi/#100)
--  c = and_bits(vi,#FF)    i += 1      data_section[i] = c
--integer b
    poke4(m4, v) -- faster than doing divides etc. (idea from database.e)
--  b = peek(m4)    i += 1  data_section[i] = b
--  b = peek(m41)   i += 1  data_section[i] = b
--  b = peek(m42)   i += 1  data_section[i] = b
--  b = peek(m43)   i += 1  data_section[i] = b
    data_section[i+1..i+4] = peek(m44)
end procedure

procedure appenddsDword(atom v)
--integer c,vi
--  c = and_bits(v,#FF)     ds = append(ds,c)       vi = floor(v/#100)
--  c = and_bits(vi,#FF)    ds = append(ds,c)       vi = floor(vi/#100)
--  c = and_bits(vi,#FF)    ds = append(ds,c)       vi = floor(vi/#100)
--  c = and_bits(vi,#FF)    ds = append(ds,c)
    poke4(m4, v) -- faster than doing divides etc. (idea from database.e)
--if not isString(data_section) then ?9/0 end if
    data_section &= peek(m44)
--if not isString(data_section) then ?9/0 end if
end procedure

procedure appenddsType(integer t)
--DEV 30/7/2013 plant a dummy (illegal) delete_routine:
--  data_section = append(data_section,0)
    data_section = append(data_section,1)
    data_section = append(data_section,0)
    data_section = append(data_section,0)
    data_section = append(data_section,t)
end procedure

procedure appenddsBytes(sequence s)
integer ch
    for i=1 to length(s) do
        ch = and_bits(s[i],#FF)
        data_section = append(data_section,ch)
    end for
--if not isString(data_section) then ?9/0 end if
end procedure

procedure APIerror(integer i, string msg)
sequence x = APIerritem[i]
    fileno = x[1]
    tokline = x[2]
    tokcol = x[3]
    Abort(msg)
end procedure

procedure readAllHeaders()
string s
integer k
integer libidx, nameidx
atom offset

    if bind then
        if dorsrc then
            rsfilename = "pf1.exe"      -- DEV from commandline, full path expansion
            fnr = open(rsfilename,"rb")
            if not readmzpe(fnr,0) then
                printf(1,"Error :%s\n",{mzpeErrMsg})
                ?9/0
            end if
            rsrcRSraddr = RSraddr   -- for checking only [DEV I might mean RSvaddr here!!]
            rsrcRSvsize = RSvsize
            rsrcRSrsize = RSrsize
            rsrcCSvaddr = CSvaddr
            rsrcCSraddr = CSraddr
        end if
        fn = open(cl1,"rb")     -- cl1 is from commandline, ie p.exe with full path expansion
        if fn=-1 then
            puts(1,"error opening "&cl1&"\n")
            ?9/0
        end if
        if not readmzpe(fn,0) then
            printf(1,"Error :%s\n",{mzpeErrMsg})
            ?9/0
        end if
        readdivm()
        if dorsrc then
            if rsrcRSraddr!=RSraddr then
                printf(1,"resource file %s error: section 5 @ #%08x, not #%08x (difference of %d)\n",
                         {rsfilename,rsrcRSraddr,RSraddr,rsrcRSraddr-RSraddr})
                ?9/0
            end if
            RSvsize = rsrcRSvsize
            RSrsize = rsrcRSrsize
            CSvaddr = rsrcCSvaddr
            CSraddr = rsrcCSraddr
            RTsize = RSvsize
            setmzpe(#10C, RTsize,  DWORD)
            setmzpe(#1F8, RSvsize, DWORD)
            setmzpe(#200, RSrsize, DWORD)
            setmzpe(#224, CSvaddr, DWORD)
            setmzpe(#22C, CSraddr, DWORD)
            if not read_resource_section(fnr) then ?9/0 end if
        else
            if not read_resource_section(fn) then ?9/0 end if
        end if
        close(fn)
        read_import_table()
    else
        peek_import_table()
--DEV/SUG I could get this using peek...
--!/**/ #ilasm{ mov_edi_imm32,%isVar,0,0,VMep,          -- mov edi,VMep
--!/**/         mov_ecx_imm32,%isVar,0,0,asmoptions,    -- mov ecx,asmoptions
--!/**/         call_rel32,%isOpCode,0,0,%opGetVMep}    -- [edi]=VMep; [ecx]=asmoptions
--/**/  #ilASM{ lea edi,[VMep]
--/**/          lea ecx,[asmoptions]
--/**/          call %opGetVMep}    -- [edi]=VMep; [ecx]=asmoptions
        CSvaddr = 0
        ImageBase = 0
    end if
    if listimports then
        for i=1 to length(Names) do
            printf(1,"%s:\n",Names[i])
            thunkaddrs = HintNames[i][1]
            thunknames = HintNames[i][2]    
            for j=1 to length(thunkaddrs) do
                printf(1,"Thunk[%08x] :%s\n",{thunkaddrs[j],thunknames[j]})
            end for
        end for
--      if getc(0) then end if
        abort(0)
    end if
    --
    -- Map any api functions and issue errors for any not found
    --
if newEmit then ?9/0 end if
    for i=1 to length(APIlibs) do
        s = APIlibs[i]
        k = find(s,Names)
        if k=0 then
            APIerritem = APIerrlib
            APIerror(i, "no such library")
            ?9/0 -- sanity check
        end if
        APIlibs[i] = k
    end for
    for i=1 to length(APINames) do
        libidx = APIlibs[APINames[i][1]]
        nameidx = find(APINames[i][2],HintNames[libidx][2])
        if nameidx=0 then
            APIerror(i, "not found in import table")
            ?9/0 -- sanity check
        end if
        offset = HintNames[libidx][1][nameidx]
        APINames[i] = offset
    end for
end procedure

--with trace
--procedure stripPathInfo()
----
---- Strip alsolute path info, leaving just relative path info,
----
----    eg {"F:\test\", "F:\test\builtins\"} ==> {"","builtins\"}
----
---- If you build an app in say F:\test\ and someone installs it in C:\new\,
---- the last thing wanted is any mention of the (non-existent) F:\test\ in
---- any error reports. nb pdiag.e plugs back in current_dir() info.
----
--sequence fpi, -- copy of filepaths[i]
--       fpj    -- copy of filepaths[j]
--integer lfpi, -- length(fpi)
--      lfpj    -- length(fpj)
--integer k, p, rooti
--
----filepaths={
----    "x:\\misc\\a\\", "x:\\misc\\a\\b\\", "x:\\misc\\"}
--
----filepaths={
----    "c:\\Program Files\\Phix\\builtins\\",
----    "c:\\crud\\",
----    "c:\\crud\\builtins\\",
----    "c:\\Program Files\\Phix\\"}
----filenames={
----    {1, "test1.exw"},
----    {2, "test2.exw"},
------  {4, "test3.exw"},
----    {3, "test4.exw"}}
----pp(filepaths)
----pp(filenames)
--  for i=1 to length(filepaths) do
--      rooti = i
--      fpi = filepaths[i]
--      lfpi = length(fpi)
--      if lfpi>=2 and fpi[2]=':' then
--          -- first make sure we have the shortest root, eg from
--          -- "x:\\misc\\a\\", "x:\\misc\\a\\b\\", "x:\\misc\\",
--          -- pick the third one.
--          for j=1 to length(filepaths) do
--              fpj = filepaths[j]
--              lfpj = length(fpj)
--              if lfpj and lfpj<lfpi and match(fpj,fpi) then
--                  rooti = j
--                  fpi = fpj
--                  lfpi = lfpj
--              end if
--          end for
--          -- now rip "x:\\misc\\" off the lot
--          for j=1 to length(filepaths) do
--              fpj = filepaths[j]
--              if j!=rooti and match(fpi,fpj) then
--                  if rooti then
--                      filepaths[rooti] = ""
--                      rooti = 0
--                  end if
--                  lfpj = length(fpj)
--                  filepaths[j] = fpj[lfpi+1..lfpj]
--              end if
--          end for
--          if rooti and rooti<alwaysactive then
--              -- sole path from mainpath/EUDIR/EUINC/PATH
--              filepaths[rooti] = ""
----DEV     else Warn?
--          end if
--      end if
--  end for
--  --
--  -- Remove any duplicates, for example in
--  --      {"C:\\Program Files\\Phix\\include\\", "C:\\win32lib\\include\\"}
--  --  ==> {"include\\","include\\"}
--  -- it is pointless and confusing to keep them separate, or well at least
--  --  removing duplicates does not really make things any worse...
--  --
--  for i=length(filepaths) to 1 by -1 do
--      k = find(filepaths[i],filepaths)    -- get duplicate
--      if k and k<i then
--          --
--          -- so we have something like {.,.,same,.,.,same,..}
--          --                                  k       i
--          -- anything=i becomes k
--          -- anything>i is decremented
--          -- lastly, filepaths[i] is deleted
--          --
--          for j=1 to length(filenames) do
--              p = filenames[j][1]
--              if p=i then
--                  filenames[j][1] = k
--              elsif p>i then
--                  filenames[j][1] = p-1
--              end if
--          end for
--          filepaths = filepaths[1..i-1]&filepaths[i+1..length(filepaths)]
--      end if
--  end for
----pp(filepaths)
----pp(filenames)
--end procedure

--with trace
without trace

--atom d_addr   -- 7/1/09
integer d_addr
sequence s5sizes, s5v, s5symn

integer thisCSsize

integer vi      -- index into symtab

integer opbyte, i3

constant call_rel32     = #E8           -- 0o350 imm32              -- call rel32
constant jump_rel32     = #E9           -- 0o351 imm32              -- jmp rel32

--with trace
--procedure scanforShortJmp()
procedure scanforShortJmp(integer vi)
--
-- At heart, this routine scans for/counts/flags any possible
--        jmp dword (#E9 xx xx xx xx) to jmp byte (#EB xx)  (ie 5->2)
--    and jcc dword (#0F 8x xx xx xx xx) to jcc byte (#7x xx)  (ie 6->2)
-- obviously, when the offset will fit in a byte, that is.
--
-- It also removes "isDead" blocks, including associated fixups to the LineTab,
--  links up routines to be processed/fixups opFrame parameter info, and 
--  just to be flash about it, performs branch straightening...
--
-- Note that while all offsets are adjusted by isDead removed, we cannot be 
--  certain of the final values as affected by isShortJmp until the loop quits
--  (because someDone=0), by which time it is too late, hence we recalculate 
--  them in (the eloquently named) blurph(), using the linked lists setup here.
--
integer short52,    -- master counts for entire routine
        short62,    -- ""
--dead5j,
--dead6j,
        someLeft,   -- more to do: phase 1: 1=some backward jumps remain
                    --                      2=some fwd but no backward jumps
                    --  phase 2: resets 2->0 as it rescans the fwd (only).
                    -- else 0 means we've done the lot (all isShortJmp'd)
        someDone,   -- outer loop control (phase 3)
        jmpOpRetf,  -- control flow flag for branch straightening to opRetf, phase 1
        i,          -- gp idx to s5 (+1 in phase 1, linked list in phase 2/3)
        c,          -- s5[i] etc. nb long active liverange, take special care
        c2,         -- scratch version of c when we don't want to damage it.
        k,          -- gp dogsbody
--      kfirst,     -- copy of [S_Parm1]
        klast,      -- jmp tgt limit for chainwalks
        s5len,      -- length(s5)
        deadCode,   -- phase 1:flag, phase 2: LineTab fixup & final sanity check
        first,      -- start of linked list of isDead/isJmp
        last,       -- end of linked list of isDead/isJmp
        next,       -- scratch chainwalk var
        prev,       -- ""
        chunkend,   -- block end for code packing (phase 2)
        oidx,       -- output idx for code packing (phase 2)
        ltj         -- LineTab entry (phase 2)

--atom offset       -- offset of the jump currently being examined
integer offset      -- offset of the jump currently being examined      -- 7/1/09

--object symk       -- symtab entry for opFrame patch (phase 1)
--integer u         -- "" used flag

--trace(1)
    --
    -- Phase 1 - dead code removal: linkup all isDead and isJmp entries,
    -- =======                      correct offsets on backward jumps by isDead jumped over,
    --                              fixup opFrame/add to chain of routines to process, (now done in pilx86.e)
    --                              and perform branch straightening.
    --
    --          (phase 2 corrects fwd jmp offsets and removes isDead blocks)
    --          (phase 3 completes the isJmp --> isShortJmp flagging)
    --
    -- A small pseudo-example:
    --
    --   s5 input:                       s5 output (after all 3 phases):
    --  1: {1A,2B,3C,4D,                1: {1A,2B,3C,4D,
    --  5:  isDead+3,0,0,               5:  8E,9F,10,
    --  8:  8E,9F,10,                   8:  isShortJmp,15,0,-8,
    --  11: isJmp,0,0,-11,              12: 15,16,17,
    --  15: 15,16,17,                   15: isShortJmp,0,8,3,
    --  18: isJmp,0,0,6,                19: 22,23,24,
    --  22: 22,23,24,                   22: 28,29,30,31}
    --  25: isDead+3,0,0,
    --  28: 28,29,30,31}
    --
    --  Where nn: is just a guide index to show where s5[nn] is,
    --        1A,2B,3C,4D, .. 31 represent as-is x86 binary,
    --        the -11 and 6 after isJmp are relative offsets, which
    --        as you can see are adjusted to -8 and 3 respectively
    --        after removal of the isDead blocks they jump over[*1],
    --        the 15 and 8 after isShortJmp [*2] are indexes to
    --        the next/prev is[Short]Jmp.
    --        Note [*1] jump offsets are /not/ adjusted for jumping
    --                  over isShortJmps here, but in blurph().
    --             [*2] temporary, ie those two byte positions will 
    --                  be ignored/clobbered in blurph().
    --  The updates as shown above are performed "in situ" within s5.
    --


    short52 = 0
    short62 = 0
--dead5j = 0
--dead6j = 0
if q86 then
--trace(1)
    s5len = length(s5)-5
    deadCode = 0
    someLeft = 0
    first = s5[s5len+1]
    last = s5[s5len+2]
--DEV if q86 is 2 then +3/+4 are other (ie isIL etc) first/last
    i = first
    while i do
        c = s5[i]
next = s5[i+1]
--DEV no longer used under oldil...
        if c>=isDead then
            c -= isDead
            if DEBUG then
                if c<3 then ?9/0 end if
            end if
            deadCode = 1    -- flag some found
-- 14/4/10 all isAddr now left as-is, otherwise it fouls up error handling...
        else -- isAddr/isJmp
--      elsif c!=isAddr then
            i3 = i+3
            offset = s5[i3]
            --
            -- Branch Straighten:
            --  (NB: fairly obvious but worth stating, branch straightening 
            --       does not occur if there is an opLnt/p/pt in the way.)
            --
            jmpOpRetf = 0
--DEV 21/11/10 I think this is here because it messes up error handling?
--if c!=isAddr then
if c>isAddr then
            while 1 do
                c2 = i+4+offset -- target addr
                opbyte = s5[c2]
                if opbyte!=jump_rel32 then exit end if
                -- target is an unconditional jump:
                c2 += 1
                opbyte = s5[c2]
                c2 += 3
--DEV try c2 = s5[c2] here...
                if opbyte<isJmp or opbyte>isShortJmp then   -- nb backwd jump may hit an isShortJmp
                    if opbyte=isOpCode and s5[c2]=opRetf then
-- we need to unlink this from our chain...
prev = s5[i+2]
if prev then
    s5[prev+1] = next
else
    first = next
end if
if next then
    s5[next+2] = prev
end if
--                      if c=isAddr then
--                          -- special case: isAddr,0,0,offset to #E9,isOpCode,0,0,opRetf:
--                          --   --> replace   ""   " "  ""    with byteified VMep[opRetf].
----27/3/10 (fouls up error reporting) [DEV could/should still have this as an option]
----                            sets5Dword(i,VMep[opRetf])
----                        else
--                          exit
--                      end if
                            -- special case: jmp/jcc,isJmp,0,0,offset to #E9,isOpCode,0,0,opRetf:
                            --      -->      jmp/jcc,isOpCode,0,0,opRetf (nb opRetf only)
                            s5[i] = isOpCode
                            s5[i3] = opRetf
--                      end if
                        jmpOpRetf = 1   -- no linkup, resume in outer loop
                    end if
                    exit
                end if
                c2 = s5[c2]+5
                offset += c2        -- ;-)
                s5[i3] = offset
            end while
end if
if c!=isBase then
            if not jmpOpRetf then
                if offset<=0 then
                    --
                    -- walk back down the chain, adjust offset by any isDead we leap over:
                    --
                    k = s5[i+1]
                    klast = i+4+offset
                    while k and k > klast do
                        c2 = s5[k]
                        if c2>isDead then
                            c2 -= isDead
                            offset += c2
                        end if
                        k = s5[k+2]     -- k:=prev
                    end while
                    s5[i3] = offset
--24/4/2013:
--                  if c!=isAddr then
                    if c!=isAddr 
                    and s5[i-1]!=call_rel32 then  -- (there is no "call byte offset" instruction on the x86, only jmp)
                        -- test as if this becomes a short jump:
                        if s5[i-1]=jump_rel32 then
                            k = (offset>=-131)  -- += 3
                        else
                            k = (offset>=-132)  -- += 4
                        end if
                        if k then
--if i=13 then ?9/0 end if
                            s5[i] = isShortJmp
                            if s5[i-1]=jump_rel32 then
                                short52 += 1
                            else
                                short62 += 1
                            end if
                        else
                            someLeft = 1
                        end if
                    end if
                elsif not someLeft then
                    someLeft = 2
                end if
            end if
end if
        end if
--      i = s5[i+1]
        i = next
    end while
else -- (not q86)
?9/0 --isBase will not work here
    s5len = length(s5)
    deadCode = 0
    someLeft = 0
    first = 0
    last = 0
    i = 1
    jmpOpRetf = 0
    while i<=s5len do
        c = s5[i]
        if c<isOpCode then  -- as-is byte
            i += 1
--      elsif c=isOpCode then
--          i3 = i+3
--          c = s5[i3]
--          i += 4
--      elsif c<isAddr then -- c=isVar/isILa/isIL
--      elsif c<isAddr then -- c=isOpCode/isVar/isILa/isIL
        elsif c<isAddr then -- c=isOpCode/isAPIFn/isVar/isConstRef/isConstRefCount/isILa/isIL
            i += 4
        else                -- c=isAddr/isJmp/isDead (no isShortJmp yet, btw)
--DEV no longer used under oldil...
            if c>=isDead then
                c -= isDead
                if DEBUG then
                    if c<3 then ?9/0 end if
                end if
                deadCode = 1    -- flag some found
            else -- isAddr/isJmp
                i3 = i+3
                offset = s5[i3]
                --
                -- Branch Straighten:
                --  (NB: fairly obvious but worth stating, branch straightening 
                --       does not occur if there is an opLnt/p/pt in the way.)
                --
                while 1 do
                    c2 = i+4+offset -- target addr
                    opbyte = s5[c2]
                    if opbyte!=jump_rel32 then exit end if
                    -- target is an unconditional jump:
                    c2 += 1
                    opbyte = s5[c2]
                    c2 += 3
                    if opbyte<isJmp or opbyte>isShortJmp then   -- nb backwd jump may hit an isShortJmp
                        if opbyte=isOpCode and s5[c2]=opRetf then
                            if c=isAddr then
                                -- special case: isAddr,0,0,offset to #E9,isOpCode,0,0,opRetf:
                                --   --> replace   ""   " "  ""    with byteified VMep[opRetf].
                                sets5Dword(i,VMep[opRetf])
                            else
                                -- special case: jmp/jcc,isJmp,0,0,offset to #E9,isOpCode,0,0,opRetf:
                                --      -->      jmp/jcc,isOpCode,0,0,opRetf (nb opRetf only)
                                s5[i] = isOpCode
                                s5[i3] = opRetf
                            end if
                            i += 5
                            jmpOpRetf = 1   -- no linkup, resume in outer loop
                        end if
                        exit
                    end if
                    c2 = s5[c2]+5
                    offset += c2        -- ;-)
                    s5[i3] = offset
                end while

                if not jmpOpRetf then
                    if offset<=0 then
                        --
                        -- walk back down the chain, adjust offset by any isDead we leap over:
                        --
                        k = last
                        klast = i+4+offset
                        while k and k > klast do
                            c2 = s5[k]
                            if c2>isDead then
                                c2 -= isDead
                                offset += c2
                            end if
                            k = s5[k+2]     -- k:=prev
                        end while
                        s5[i3] = offset
                        if c!=isAddr then
                            -- test as if this becomes a short jump:
                            if s5[i-1]=jump_rel32 then
                                k = (offset>=-131)  -- += 3
                            else
                                k = (offset>=-132)  -- += 4
                            end if
                            if k then
--if i=13 then ?9/0 end if
                                s5[i] = isShortJmp
                                if s5[i-1]=jump_rel32 then
                                    short52 += 1
                                else
                                    short62 += 1
                                end if
                            else
                                someLeft = 1
                            end if
                        end if
                    elsif not someLeft then
                        someLeft = 2
                    end if
                    c = 4   -- remainder of isJmp, as opposed to isDead bytes to be skipped
                end if
            end if
            if jmpOpRetf then
                jmpOpRetf = 0
            else
                --
                -- linkup item:
                --
                s5[i+1] = 0     -- <next> = <end of chain>
                s5[i+2] = last  -- <prev> = last
                if last then
                    s5[last+1] = i  -- last's <next> := this
                else
                    first = i
                end if
                last = i
                i += c
            end if
        end if
    end while
end if

    --
    -- Phase 2 - dead code removal: follow isDead/isJmp chain,
    -- =======                      fixup fwd isJmp offsets by isDead jumped over,
    --                              fixup LineTab entries by isDead blocks, and
    --                              pack code (remove isDead blocks, relink chain w/o them)
    --
    if deadCode then
        if someLeft=2 then  -- all backward jumps were in range, so reset,
            someLeft = 0    -- as we are now going to rescan all fwd jumps.
        end if
        i = first
        first = 0   -- create a brand new chain
        last = 0
        while 1 do
            c = s5[i]
            if c>isDead then
                -- from now on, we have to pack the code.
                --  (one of the most likely places for an isDead is a return statement;
                --   avoid any needless s5[i]=s5[i] byte copying as much as possible)
                oidx = i    -- output idx
                --
                -- deal with that pesky LineTab thing first though...
                --
                deadCode = c-isDead
                for j=1 to length(LineTab) do
                    ltj = LineTab[j]
                    if ltj>=i then  -- skip -ve and <i entries at start
                        klast = s5[i+1]
                        k = j
                        while 1 do
                            while klast and ltj>=klast do
                                c2 = s5[klast]
                                if c2>isDead then
                                    deadCode += c2-isDead
                                end if
                                klast = s5[klast+1]
                            end while
                            LineTab[k] = ltj-deadCode
                            k += 1
                            if k>length(LineTab) then exit end if
                            ltj = LineTab[k]
                            if ltj<0 then
                                k += 1
                                ltj = LineTab[k]
                            end if
                        end while
                        exit
                    end if
                end for
--last = i
                --
                -- now onto copying the non-Dead bytes:
                --
                deadCode = 0
                while i do  -- process isDead blocks (outer loop)
                    c -= isDead
                    deadCode += c   -- count size in bytes (for sanity check)
                    next = s5[i+1]
                    while 1 do  -- process/relink isJmp entries (inner loop)
                        -- (c=4 here when processing consecutive isJmp entries, btw)
                        if next then
                            -- copy bytes i+c..next-1 to oidx++
                            chunkend = next-1
                        else
                            -- copy bytes i+c..s5len to oidx++
                            chunkend = s5len
                            -- (aside: next=0 which soon exits)
                        end if
                        for j=i+c to chunkend do
                            c2 = s5[j]
                            s5[oidx] = c2
                            oidx += 1
                        end for
                        i = next

                        if i=0 then exit end if     -- *ALL DONE*

                        c = s5[i]
                        if c>=isDead then exit end if -- resume in outer loop

                        -- isJmp/isShortJmp/isAddr, copy and relink:

                        next = s5[i+1]
                        --
                        -- check for fwd jmp
                        --
                        i3 = i+3
                        offset = s5[i3]
                        if offset>0 then    -- nb can/must not be isShortJmp yet
                            --
                            -- walk up the chain, adjust offset by any isDead we leap over:
                            --
                            k = next
--                          klast = next+4+offset
                            klast = i+4+offset
                            while k and k < klast do
                                c2 = s5[k]
                                if c2>isDead then
                                    c2 -= isDead
                                    offset -= c2
                                end if
                                k = s5[k+1]     -- k:=next
                            end while
--                          s5[i3] = offset         -- nb differs from non-packing chainwalk
                            if c!=isAddr then
                                if offset<=127 then
--                                  s5[i] = isShortJmp  -- nb differs from non-packing chainwalk
--if oidx=13 then ?9/0 end if
                                    c = isShortJmp
                                    if s5[i-1]=jump_rel32 then
                                        short52 += 1
                                    else
                                        short62 += 1
                                    end if
                                else
                                    someLeft = 1
                                end if
                            end if
                        end if

                        -- create a new chain, w/o the isDead, as we copy
                        -- the flagged dword from i..i+3 to oidx..oidx+3:
                        
                        s5[oidx] = c        -- isJmp/isShortJmp/isAddr
                        s5[oidx+1] = 0      -- <next> = <end of chain>
                        s5[oidx+2] = last   -- <prev> = last
                        s5[oidx+3] = offset
                        if last then
                            s5[last+1] = oidx   -- last's <next> := this
                        else
                            first = oidx
                        end if
                        last = oidx
                        oidx += 4
                        c = 4   -- copy i+4..(next-1 or s5len) bytes
                    end while   -- while i (isJmp/inner loop)
                end while   -- while i (isDead/outer loop)
                oidx -= 1
                if DEBUG then
                    if oidx+deadCode!=s5len then ?9/0 end if    -- sanity check
                end if
                s5 = s5[1..oidx]
                s5len = oidx

                exit    -- *ALL DONE* (phase 2 anyway)

            end if
if c!=isBase then
            i3 = i+3
            offset = s5[i3]
            if offset>0 then    -- nb can/must not be isShortJmp yet
                --
                -- walk up the chain, adjust offset by any isDead we leap over:
                --
                k = i
                klast = i+4+offset
                while k and k < klast do
                    c2 = s5[k]
                    if c2>isDead then
                        c2 -= isDead
                        offset -= c2
                    end if
                    k = s5[k+1]     -- k:=next
                end while
                s5[i3] = offset         -- nb differs from packing chainwalk
                if c!=isAddr then
                    if offset<=127 then
--if i=13 then ?9/0 end if
                        s5[i] = isShortJmp  -- nb differs from packing chainwalk
                        if s5[i-1]=jump_rel32 then
                            short52 += 1
                        else
                            short62 += 1
                        end if
                    else
                        someLeft = 1
                    end if
                end if
            end if
end if

            -- create a new chain, w/o the isDead:

            if last then
                s5[last+1] = i  -- last's <next> := this
            else
                first = i
            end if
            s5[i+2] = last
            last = i

            i = s5[i+1]     -- i:=next
            s5[last+1] = 0
        end while
    end if
    --
    -- Phase 3 - complete isJmp -> isShortJmp flagging; 
    -- =======                      repeat until entire pass leaves someDone 0, or someLeft 0
    --
    while someLeft do
        someLeft = 0
        someDone = 0
        i = first
        while i do
            c = s5[i]
--17/1/2013:
--          if c=isJmp then     -- skip already isShortJmp and isAddr entries
            if c=isJmp and      -- skip already isShortJmp and isAddr entries
               s5[i-1]!=call_rel32 then  -- (there is no "call byte offset" instruction on the x86, only jmp)
                i3 = i+3
                offset = s5[i3]
                if offset<0 then    -- backward jump
                    --
                    -- The following diagram explains the start/limit of the scan loop.
                    -- Obviously we're at i, an isJmp dword, looking for isShortJmp dwords
                    -- that we're jumping over, and hence our offset needs decreasing
                    -- (+3/+4 since the offset is -ve):
                    --
                    --  i+4+offset: opcode of destination instruction (at least one byte)
                    --  i+5+offset:  first possible dword/isShortJmp flag here
                    --  i+6+offset:  dword byte 2
                    --  i+7+offset:  dword byte 3
                    --  i+8+offset:  dword byte 4 (may hold big value)
                    --      ...
                    --  i-1 : opcode of at least one byte ("ours")
                    --  i   :  dword byte 1, isJmp flag we just found is here   <**
                    --  i+1 :  dword byte 2
                    --  i+2 :  dword byte 3, <<ptr to prev isJmp/isShortJmp/isDead flag>>
                    --  i+3 :  dword byte 4 (may hold big value)                <<- offset
                    --  i+4 : next instruction (offset is relative to this)
                    --
                    k = s5[i+2]     -- k = prev
                    klast = i+5+offset
                    -- make final test work as if this itself is a short jump:
                    if s5[i-1]=jump_rel32 then offset += 3 else offset += 4 end if
                    while k and k>=klast do
                        c2 = s5[k]
                        if c2=isShortJmp then   -- skip isAddr and still isJmp entries
                            if s5[k-1]=jump_rel32 then
                                -- jmp dword #E9 xx xx xx xx --> jmp byte #EB xx  (ie 5->2)
                                offset += 3
                            else
                                -- jcc dword #0F 80..8F xx xx xx xx -> jcc byte #70..7F xx  (ie 6->2)
                                offset += 4
                            end if
                            if offset>=-128 then exit end if        -- it'll fit, no need to carry on.
                        end if
                        k = s5[k+2]     -- k = prev
                    end while
                    if DEBUG then
                        if offset>0 then ?9/0 end if    -- sanity check
                    end if
                else  -- forward jump
                    --
                    -- The following diagram explains the start/limit of the scan loop.
                    -- Obviously we're at i, an isJmp dword, looking for isShortJmp dwords
                    -- that we're jumping over, and hence our offset needs decreasing
                    -- (the more obvious -3/-4 in this case, since the offset is +ve):
                    --
                    --  i-1 : opcode ("ours"), not that we're interested in it here (may be 2 bytes).
                    --  i   :  dword byte 1, isJmp flag we just found is here   <**
                    --  i+1 :  dword byte 2, <<ptr to next isJmp/isShortJmp/isDead flag>>
                    --  i+2 :  dword byte 3
                    --  i+3 :  dword byte 4 (may hold big value)
                    --  i+4 : opcode of next instruction (offset relative to this) (at least one byte)
                    --      ...
                    --  i+offset: last possible dword/isShortJmp flag here
                    --  i+1+offset:  dword byte 2
                    --  i+2+offset:  dword byte 3
                    --  i+3+offset:  dword byte 4 (may hold big value)
                    --  i+4+offset: opcode of destination instruction (at least one byte)
                    --
                    k = s5[i+1]     -- k = next
                    klast = i+offset
                    while k and k<=klast do
                        c2 = s5[k]
                        if c2=isShortJmp then   -- skip isAddr and still isJmp entries
                            if s5[k-1]=jump_rel32 then
                                -- jmp dword #E9 xx xx xx xx --> jmp byte #EB xx (ie 5->2)
                                offset -= 3
                            else
                                -- jcc dword #0F 80..8F xx xx xx xx -> jcc byte #70..7F xx (ie 6->2)
                                offset -= 4
                            end if
                            if offset<=127 then exit end if     -- it'll fit, no need to carry on.
                        end if
                        k = s5[k+1]     -- k = next
                    end while
                    if DEBUG then
                        if offset<0 then ?9/0 end if    -- sanity check
                    end if
                end if
--DEV should this also check s5[i-1]!=call_rel32?? [fixed 17/1/2013]
                if offset>=-128 and offset<=127 then
--if i=13 then ?9/0 end if
                    s5[i] = isShortJmp      -- flag set, offset remains unaltered.
                    if s5[i-1]=jump_rel32 then
--if offset=0 then
--  dead5j += 5
--else
                        short52 += 1
--end if
                    else
--if offset=0 then
--  dead6j += 6
--else
                        short62 += 1
--end if
                    end if
                    someDone = 1
                else
                    someLeft = 1
                end if
            end if
            i = s5[i+1]     -- i:=next
        end while
        if not someDone then exit end if
    end while
    --
    -- So now we can say how big this code chunk will really be:
    --
    thisCSsize = s5len - (short52*3) - (short62*4)
--  thisCSsize = s5len - (short52*3) - (short62*4) - dead5j -dead6j
    --
    -- One last job: adjust any global labels in this block by preceding shortened jumps
    -- DEV not at all sure this will cope with isDead...
    --
--?length(glblused)
    for gidx=1 to length(glblused) do
        if glblabel[gidx]=vi then
            if and_bits(glblused[gidx],G_declared+G_set)!=G_declared+G_set then ?9/0 end if
            offset = glboffset[gidx]
--?{offset,gidx}
            k = 0
            i = first
            while i!=0 and i<offset do
--          while i!=0 and i<glboffset[gidx] do
                if s5[i]=isShortJmp then
--?i
                    if s5[i-1]=jump_rel32 then
--                      offset -= 3
                        k += 3
                    else
--                      offset -= 4
                        k += 4
                    end if
                end if
                i = s5[i+1]     -- i:=next
--?{i}
            end while
            glboffset[gidx] = offset-k
        end if
    end for
end procedure

--DEV (not a good idea, see notes in e6.exw 17/11/09)
--integer o0        -- emit warning once only
--      o0 = 1

--with trace
procedure blurph()
-- take the fully scanned s5 blocks and pack them to output (code_section),
--  that is now that we know the size of, and hence where, everything will be.
integer cin, cout, cm1, c, c2, k, klast, s5len
--atom offset
integer offset  --7/1/09
integer vno

integer ltidx,  -- LineTab idx
        nilte   -- next interesting line tab entry

sequence sv     -- copy of symtab[vno], speedwise

object constvalue
--object dbg
--trace(1)
--?LineTab
    nilte = -1
    for i=1 to length(LineTab) do
        nilte = LineTab[i]
        if nilte>0 then
            nilte += 1      -- zero-based offset to 1-based idx
            ltidx = i
            exit
        end if
    end for

    cout = CSvsize+1
    CSvsize += thisCSsize
    --
    -- The Final Fixups:
    --
    cin = 1
if q86 then
    s5len = length(s5)-5
else
    s5len = length(s5)
end if
    c = 0  c2 = 0   --DEV test
    while cin<=s5len do
        c = s5[cin]

        if c<isOpCode then          -- ie as-is byte, #00..#FF.

            code_section[cout] = c
            if cin=nilte then   -- next interesting linetab entry?
                LineTab[ltidx] = cout-CSvsize+thisCSsize-1      -- (1-based idx(ie cout) to zero-based offset)
                nilte = 0
                for i=ltidx+1 to length(LineTab) do
                    nilte = LineTab[i]
                    if nilte>0 then
                        nilte += 1                              -- (zero-based offset to 1-based idx(ie cin))
                        ltidx = i
                        exit
                    end if
                end for
            end if
            cin += 1
            cout += 1

        elsif c<=isIL then          -- isOpCode/isAPIFn/isVar/isConstRef/isConstRefCount/isJmpG/isGaddr/isILa/isIL

            vno = s5[cin+3]
            if c=isOpCode then          -- 4 byte relative offset (to VM entry point).
--DEV put back while isApiFn still exists...
--          if c<=isApiFn then          -- isOpCode is 4 byte relative offset (to VM entry point).
--                                      -- isApiFn is 4 byte indirect location
                offset = VMep[vno]
                if DEBUG then
                    if offset=0 then puts(1,"\n\nERROR: "&opNames[vno]&" = 0\n") ?9/0 end if
                end if
--              if c=isOpCode then
                offset -= CSvaddr+ImageBase+cout+3      -- cout+3 is really (cout-1)+4
--              end if
--/* (code from a prior version:)
--DEV 5/4/2013:
--          if c=isOpCode then          -- 4 byte relative offset (to VM entry point).
--              offset = VMep[vno]
--              if DEBUG then
--                  if offset=0 then puts(1,"\n\nERROR: "&opNames[vno]&" = 0\n") ?9/0 end if
--              end if
--              offset -= CSvaddr+ImageBase+cout+3      -- cout+3 is really (cout-1)+4
--*/
            elsif c=isAPIfn then
if newEmit then
                ?9/0
else
                offset = APINames[vno]
end if
--?{offset}
            else
--27/4/15:
                sv = symtab[vno]
--if c!=isData
--and c!=isJmpG
--and c!=isGaddr then
----24/4/15:
--              if atom(symtab[vno]) then
--printf(1,"pemit.e line 1880: symtab[%d]=%d\n",{vno,symtab[vno]})
--                  sv = {-1,S_GVar2,0,0,0,-1}
--              else
--                  sv = symtab[vno]
--              end if
--end if
                if c=isVar              -- 4 byte absolute address (eg mov eax,[p1])
                or c=isVar4 then        -- (newEmit: no effect here, since no [64bit] rip addressing mode here)

                    if sv[S_NTyp]<=S_GVar2 then
                        vno = sv[S_Slink]   --DEV +gbase
                    else
--DEV newEBP (isEBP?)
                        vno = -sv[S_Tidx]   --DEV vmap or reassign!
                    end if
                    offset = DSvaddr+vno*4-4+ImageBase
-- 26/4/15 (transition to newEmit, not expected to be right, for mov edx,routine_id(xxx), copied from pemit2.e)
                elsif c=isVno then
----                    if bind and mapsymtab then
--                  if bind then
--                      vno = symtabmap[vno]
--                  end if
                    offset = vno
                elsif c<=isConstRefCount then
                    if bind then
if newEmit then
    if c=isConstRef then ?9/0 end if -- (isConstRefCount is fine)
end if
                        -- link them up for fixup later (in fixupConstRefs, via DumpXxx)...
                        symtab[vno] = 0                                 -- (kill refcount)
                        if sv[S_NTyp]!=S_Const then ?9/0 end if
                        code_section[cout] = sv[S_ConstChain]
                        sv[S_ConstChain] = cout
                        code_section[cout+1] = (c=isConstRef)
                        symtab[vno] = sv
                    else
                        constvalue = sv[S_value]
                        #ilASM{ mov eax,[constvalue]
                                and eax,#3FFFFFFF           -- (raw address/4)
                                mov [offset],eax }
                        --
                        -- explanation of the above:
                        --  if a sequence(/string/float) is located at say #00040204 (and 
                        --  that address will aways be a multiple of 4, ie end in 0b00), a 
                        --  "ref" is formed by shifting it right two bits and putting 0b01
                        --  (#4-ish) at the front, giving #40010081. The above extracts the 
                        --  #00010081 part (always an integer) from which we can create the 
                        --  right atom value (by adding #40000000, or by calculating the 
                        --  address of the refcount) that we need to emit/poke. If your hex 
                        --  maths is up to it, you can see #00010081 * 4 == #00040204.
                        --
                        if c=isConstRef then
                            setcsDword(cout,offset+#40000000) -- (make an atom value == to the ref)
                        else -- isConstRefCount
                            setcsDword(cout,offset*4-8) -- (make an atom value == to the refcount addr)
                        end if
                    end if
                    c = isDead -- avoid setcsDword call below.
--DEV 28/4/13 maybe:
                elsif c=isJmpG
                   or c=isGaddr then
                    k = glblused[vno]
--copied from pemit2 9/4/15:
if and_bits(k,G_bang+G_declared)=G_bang then -- (added 28/10/14)
            offset = 0
else
                    if and_bits(k,G_declared)=0 then
                        fileno = glblabel[vno]
                        tokline = glblline[vno]
                        tokcol = glblcol[vno]
                        Abort("not declared")
                    end if
                    if and_bits(k,G_set)=0 then ?9/0 end if
                    offset = glboffset[vno]
                    offset += symtab[glblabel[vno]][S_il]
                    if c=isJmpG then
                        offset -= CSvaddr+ImageBase+cout+3
                    end if
end if
                else
                    offset = sv[S_il]   -- isILa = 4 byte absolute address of symtab[routineNo][S_il]
                    if c=isIL then      -- isIL  = 4 byte relative offset to symtab[routineNo][S_il]
                        offset -= CSvaddr+ImageBase+cout+3  -- cout+3 is really (cout-1)+4
                    end if
                end if
            end if
            if c!=isDead then -- except for isConstRef[Count]:
                setcsDword(cout,offset)
            end if
            cin += 4
            cout += 4

        else                    --        else: 4 byte offset. (c=isAddr/isJmp/isShortJmp)
            offset = s5[cin+3]
if c!=isBase then
            --
            -- First adjust offset by any isShortJmps flagged betwixt origin and destination:
            --
            if offset<0 then        -- backward jump
                --
                --  cin+4+offset: opcode of destination instruction (at least one byte)
                --  cin+5+offset: first possible dword/isShortJmp flag here
                --  cin+6+offset:  dword byte 2 <next Jmp/ShortJmp link>
                --  cin+7+offset:  dword byte 3 <prev Jmp/ShortJmp link>    -- ...until 0 or < cin+4+offset
                --  cin+8+offset:  dword byte 4 (actual value/index/offset)
                --      ...
                --  cin-1 : opcode of at least one byte ("ours")
                --  cin   :  dword byte 1, isJmp flag we just found is here     <**
                --  cin+1 :  dword byte 2 <next Jmp/ShortJmp link>
                --  cin+2 :  dword byte 3 <prev Jmp/ShortJmp link>          -- follow this link...
                --  cin+3 :  dword byte 4 (actual value/index/offset)
                --  cin+4 : next instruction (offset is relative to this)
                --
                k = s5[cin+2]       -- k = prev
                klast = cin+5+offset
                while k and k>=klast do
                    c2 = s5[k]
                    if c2=isShortJmp then
                        if s5[k-1]=jump_rel32 then
                            -- jmp dword #E9 xx xx xx xx --> jmp byte #EB xx (ie 5->2)
                            offset += 3
                        else
                            -- jcc dword #0F 80..8F xx xx xx xx -> jcc byte #70..7F xx (ie 6->2)
                            offset += 4
                        end if
                    end if
                    k = s5[k+2]     -- k = prev
                end while
                if DEBUG then
                    if offset>0 then ?9/0 end if    -- sanity check
                end if
            else                        -- forward jump
                --
                --  cin-1 : opcode ("ours"), not that we're interested in it here (may be 2 bytes).
                --  cin   :  dword byte 1, isJmp flag we just found is here     <**
                --  cin+1 :  dword byte 2 <next Jmp/ShortJmp link>              -- follow this link...
                --  cin+2 :  dword byte 3 <prev Jmp/ShortJmp link>
                --  cin+3 :  dword byte 4 (actual value/index/offset)
                --  cin+4 : opcode of next instruction (offset relative to this) (at least one byte)
                --      ...
                --  cin+offset: last possible dword/isShortJmp flag here
                --  cin+1+offset:  dword byte 2 <next Jmp/ShortJmp link>        -- ...until 0 or > cin+4+offset
                --  cin+2+offset:  dword byte 3 <prev Jmp/ShortJmp link>
                --  cin+3+offset:  dword byte 4 (actual value/index/offset)
                --  cin+4+offset: opcode of destination instruction (not marked isJmp [only the dwords are])
                --
                k = s5[cin+1]   -- k = next
                klast = cin+offset
                while k and k<=klast do
                    c2 = s5[k]
                    if c2=isShortJmp then
                        if s5[k-1]=jump_rel32 then
                            -- jmp dword #E9 xx xx xx xx --> jmp byte #EB xx (ie 5->2)
                            offset -= 3
                        else
                            -- jcc dword #0F 80..8F xx xx xx xx -> jcc byte #70..7F xx (ie 6->2)
                            offset -= 4
                        end if
                    end if
                    k = s5[k+1]     -- k = next
                end while
                if DEBUG then
                    if offset<0 then ?9/0 end if    -- sanity check
                end if
            end if
end if
            if c=isShortJmp then        -- isJmp that has been found to fit in a byte
                cm1 = cout-1
                c = code_section[cm1]
                if offset<0 then
                    if c=jump_rel32 then offset += 3 else offset += 4 end if
                end if
                if DEBUG then
                    if offset<-128 or offset>127 then ?9/0 end if -- sanity check
--                  if offset=0 then
--                      if o0 then
--                          puts(1,"pemit.e: warning jmp 0 emitted\n")
----if getc(0) then end if
--                          o0 = 0
--                      end if
--                  end if
                end if
                --
                -- Finally, convert the dword instruction to short form...
                --
--17/11/09 no good... need to do this in scanforShortJmp!!
--if offset=0 then
--  if c=jump_rel32 then
--      cout = cm1
--  else
--      cout -=2
--  end if
--else
--DEV fixed 17/1/2013:
--/**/  -- (DEV pemit.e cannot cope with call offset that fits in a byte...)
--          (there is no call byte offset instruction on the x86, so it
--           would have to leave it as-is, which means we got CSvsize (etc) wrong..)
--          (see (untested) "should this also check s5[i-1]!=call_rel32??" above)
                if c=jump_rel32 then
                    -- jmp dword, #E9 xx xx xx xx --> jmp byte, #EB xx
                    code_section[cm1] = #EB
                else
                    -- jcc dword, #0F 80..8F xx xx xx xx -> jcc byte, #70..7F xx
                    cout -= 1
                    cm1 -= 1
                    if DEBUG then
                        if code_section[cm1]!=#0F then ?9/0 end if
                        if c<#80 or c>#8F then ?9/0 end if
                    end if
                    code_section[cm1] = c-#10
                end if
                code_section[cout] = offset
                cout += 1               -- .. and this is what it was all about !!
                                        --    (if you can fit 25% more code in the on-chip 
                                        --     cache, it'll run alot more than 25% faster!)
--end if
                cin += 4

            else
                if c=isJmp then                     -- 4 byte offset that does not fit in byte
--17/1/2013:
if s5[cin-1]!=call_rel32 then
                    if offset>=-128 and offset<=127 then ?9/0 end if -- sanity check
end if
                elsif c<=isAddr then                -- 4 byte offset, fixup as absolute address (push <return addr>)
                    offset += CSvaddr+ImageBase+cout+3  -- cout+3 is really (cout-1)+4
-- 2/4/10: (needed for jump table entries)
                    if cin=nilte then   -- next interesting linetab entry?
                        LineTab[ltidx] = cout-CSvsize+thisCSsize-1      -- (1-based idx(ie cout) to zero-based offset)
                        nilte = 0
                        for i=ltidx+1 to length(LineTab) do
                            nilte = LineTab[i]
                            if nilte>0 then
                                nilte += 1                              -- (zero-based offset to 1-based idx(ie cin))
                                ltidx = i
                                exit
                            end if
                        end for
                    end if
                end if
                setcsDword(cout,offset)
                cin += 4
                cout += 4
            end if
        end if
    end while
    if cin=nilte then
        LineTab[ltidx] = cout-CSvsize+thisCSsize-1      -- 1-based idx to zero-based offset
    end if
    if cout!=CSvsize+1 then ?9/0 end if -- sanity check
--?LineTab

end procedure

-- results from flatdump(/DEV:dumpString):
atom b_addr,    -- raw address
     s_addr     -- symbolic address (a "ref" in the range #40000001..#7FFFFFFF,
                --  ie a 2-bit-shifted dword-aligned pointer with 0b01 prefix)

procedure dumpString(sequence name, integer refcount)
integer l, l4
    l = length(name)
if newBase then
    s_addr = floor(d_addr/4)+#40000004
    l4 = and_bits(l+4,#3FFFFFFC)+16
    b_addr = d_addr+16  -- DEV [is this used??]
else
    s_addr = floor(d_addr/4)+#40000005
    l4 = and_bits(l+4,#3FFFFFFC)+20
    b_addr = d_addr+20
    appenddsDword(b_addr)           -- base
end if
    d_addr += l4
    appenddsDword(l4)               -- maxlen (in bytes)
    appenddsDword(l)                -- length
    appenddsDword(refcount)         -- refcount
--if not isString(data_section) then ?9/0 end if
    appenddsType(#82)
--  data_section &= {0,0,0,#82}             -- type
--if not isString(data_section) then ?9/0 end if
    appenddsBytes(name)
--  data_section &= name
--if not isString(data_section) then ?9/0 end if
    for i=1 to 4-and_bits(l,3) do
        data_section = append(data_section,0)
    end for
--if not isString(data_section) then ?9/0 end if
--  l = 4-and_bits(l,3)
--  if l then
--      data_section &= repeat(0,l)
--  end if  
    DSvsize += l4
end procedure

sequence flatsym

function DumpId(sequence name, integer node)
integer slink, refcount, k, l
sequence si
    slink = tt[node+EQ]
if slink then
if newBase then
    s_addr = floor(d_addr/4)+#40000004
    b_addr = d_addr+16
else
    s_addr = floor(d_addr/4)+#40000005
    b_addr = d_addr+20
end if
    refcount = 0
    while slink do
        si = flatsym[slink]
        flatsym[slink] = 0  -- kill refcount
        if listing then
            symtab[slink] = 0   -- kill refcount
            si[S_Name] = name
            symtab[slink] = si
        end if
        si[S_Name] = s_addr
        if debug then
            if si[S_NTyp]>=S_Type then
                k = find(slink,s5symn)
                if k then
                    -- patch the cmp eax,["myroutine"] debug aid
                    setcsDword(s5sizes[k],b_addr)
                    l = length(s5symn)
                    s5sizes[k] = s5sizes[l]
                    s5symn[k] = s5symn[l]
                    l -= 1
                    s5sizes = s5sizes[1..l]
                    s5symn = s5symn[1..l]
                end if
            end if
        end if
        flatsym[slink] = si
        refcount += 1
        slink = si[S_Nlink]
    end while
    dumpString(name, refcount)
end if
    return 1
end function
constant r_DumpId = routine_id("DumpId")

function ReconstructIds(sequence name, integer node)
integer slink
----DEV 26/02/2012 Check the reference count on symtab is 1
----/**/integer refcount
----/**/    #ilasm{ mov_eax_mem32,%isVar,0,0,symtab,    -- mov eax,[symtab]
----/**/            mov_eax_sibd8,%ebx_eax4,-8,         -- mov eax,[ebx+eax*4-8]
----/**/            opStoreMem,%eax,refcount}           -- mov [refcount],eax
----/**/    if refcount!=1 then
----/**/        puts(1,"\n\noops, non-1 reference count on symtab!")
----/**/        if getc(0) then end if
----/**/    end if
--object dbg
    slink = tt[node+EQ]
    while slink do
        -- This was to be temp (bugfix 1/3/2013?) but proved worth leaving in.
        if slink>length(symtab) then
--      if slink>length(symtab) 
--      or not sequence(symtab[slink]) then
            puts(1,"oops in ReconstructIds\n")
            exit
        end if
--dbg = symtab[slink]
        symtab[slink][S_Name] = name
        slink = symtab[slink][S_Nlink]
    end while
    return 1
end function
constant r_ReconstructIds = routine_id("ReconstructIds")

--ppOpt({pp_Pause,0})

function rebuild_callback()
-- called (via rbicb, see below) when interpreting (and this has not
--  already been done because of unresolved routine_ids) before 
--  pdiag.e/pdebug.e get invoked, so they can use symtab[i][S_Name] normally.
--BUGFIX 17/03/2012:
--  We must, however, ensure that symtab has a refcount of 1 (- it will not
--  if an opGetST has been called without the result dropping out of scope)
--  so that our efforts to fixup symtab do not cause a clone of it. We must,
--  btw, incref symtab in opGetST, to prevent attempts to deallocate it.
integer StmAddr
atom refaddr
integer refcount
--  #ilasm { mov_eax_mem32,%isVar,0,0,symtab,       -- mov eax,[symtab]
--           and_eax_imm32,#FF,#FF,#FF,#3F,         -- (raw address/4)
--           opStoreMem,%eax,StmAddr}               -- mov [StmAddr],eax
    #ilASM{ mov eax,[symtab]
            and eax,#3FFFFFFF       -- (raw address/4)
            mov [StmAddr],eax }
    refaddr = StmAddr*4-8
    refcount = peek4s(refaddr)
--printf(1,"rebuild_callback: symtab is at #%08x, refcount is %d\n",{StmAddr*4,refcount})
    if refcount!=1 then
        poke4(refaddr,1)
    end if
    relink()
    tt_traverse(r_ReconstructIds,"",-2)             -- identifiers
    if refcount!=1 then
        poke4(refaddr,refcount)
    end if
    return 1    -- (function result is ignored)
end function

procedure fixupConstRefs(integer cref, integer offset, integer idx)
-- This is only called when binding (via DumpXxx), there is no
--  const chain when interpreting (done immediately instead)
integer cnxt
--                  code_section[cout] = sv[S_ConstChain]
--                  sv[S_ConstChain] = cout
--                  code_section[cout+1] = (c=isConstRef)
    if cref then
        if listing then
            craddr = append(craddr,d_addr+offset)
            cridx = append(cridx,idx)
        end if
        while 1 do
            cnxt = code_section[cref]
            if code_section[cref+1] then    -- isConstRef
if newEmit then ?9/0 end if
                setcsDword(cref,s_addr)
            else                -- isConstRefCount
                setcsDword(cref,d_addr+offset)
            end if
            if cnxt=0 then exit end if
            cref = cnxt
        end while
    end if
end procedure

function DumpString(sequence name, integer node)
integer slink, refcount
sequence si
    slink = tt[node+EQ]
if slink then
if newBase then
    s_addr = floor(d_addr/4)+#40000004
else
    s_addr = floor(d_addr/4)+#40000005
end if
    refcount = 0
    while slink do
        flatsym[slink][S_value] = s_addr
        si = flatsym[slink]
        setdsDword(si[S_Slink]*4 - 4, s_addr)
-- 8/3/14:
if newEmit then
    if si[S_NTyp]=S_Const then
        if si[S_ConstChain] then ?9/0 end if
    end if
else
    -- 18/10/9:
    if si[S_NTyp]=S_Const then
        if newBase then
            fixupConstRefs(si[S_ConstChain],8,slink)    -- (8 from: maxlen,length,/refcount/)
        else
            fixupConstRefs(si[S_ConstChain],12,slink)   -- (12 from: base,maxlen,length,/refcount/)
        end if
    end if
end if
        refcount += 1
        slink = si[S_Clink]
    end while
    dumpString(name, refcount)
end if
    return 1
end function
constant r_DumpString = routine_id("DumpString")

--with trace
function DumpAtom(atom v, integer node)
integer slink, refcount
--, vchk
sequence si, x8
--  v = tt[node+CH]
--trace(1)
    if not integer(v) then
        s_addr = floor(d_addr/4)+#40000002
        slink = tt[node+EQ]
        refcount = 0
        while slink do
            si = flatsym[slink]
            if si[S_NTyp]!=S_Const then ?9/0 end if
-- 8/3/14:
if newEmit then
    if si[S_ConstChain] then ?9/0 end if
else
            setdsDword(si[S_Slink]*4 - 4, s_addr)
            fixupConstRefs(si[S_ConstChain],0,slink)
end if
            refcount += 1
            slink = si[S_Clink]
        end while
        appenddsDword(refcount)
--if not isString(data_section) then ?9/0 end if
        appenddsType(#12)
--      data_section &= {0,0,0,#12}     -- type
--if not isString(data_section) then ?9/0 end if
        x8 = atom_to_float64(v)
--      appenddsBytes(x8)
        data_section &= x8
--if not isString(data_section) then ?9/0 end if
        DSvsize += 16
        d_addr += 16
    else
        slink = tt[node+EQ]
        while slink do
            si = flatsym[slink]
            if si[S_NTyp]=S_Const
--DEV K_rtn prolly not on this chain...
            or and_bits(si[S_State],K_noclr+K_rtn) then
-- DEV 17/4/06 if this never triggers, safe to delete the entire else block/
--      not call DumpAtom (from within tt_traverseA) in the first place.
--vchk = getdsDword(si[S_Slink]*4 - 3)  -- (NB: 1-based, unlike setdsDword)
--if vchk!=v then puts(1,"setdsDword *IS* needed\n") end if
-- DEV 17/4/06 sanity check added:
                if v!=si[S_value] then ?9/0 end if
                setdsDword(si[S_Slink]*4 - 4, v)
                fixupConstRefs(si[S_ConstChain],0,slink)
--if si[S_ConstChain] then
----    S_Name
--  ?9/0
--end if
            end if
            slink = si[S_Clink]
        end while
    end if
    return 1
end function
constant r_DumpAtom = routine_id("DumpAtom")

procedure flatdump(sequence s, integer refcount)
integer l, l20
    b_addr = d_addr+20
    s_addr = floor(b_addr/4)+#40000000
    l = length(s)
    l20 = l*4+20
    d_addr += l20
if newBase then
    appenddsDword(0)                -- slack
else
    appenddsDword(b_addr)           -- base
end if
    appenddsDword(l20)              -- maxlen (in bytes)
    appenddsDword(l)                -- length
    appenddsDword(refcount)         -- refcount
--if not isString(data_section) then ?9/0 end if
    appenddsType(#80)
--  data_section &= {0,0,0,#80}             -- type
--if not isString(data_section) then ?9/0 end if
    for i=1 to l do
        appenddsDword(s[i])
    end for
    DSvsize += l20
end procedure


sequence oopsSet,   -- sequence nodes encountered out-of-order
         oopsAddr   -- fixup address
integer oopsLen     -- length(oopSet)
        oopsSet = {}
        oopsAddr = {}
        oopsLen = 0

function DumpSequence(sequence s, integer node)
integer l, tidx, k, refcount, slink
object si
--atom s_addr, b_addr, oops_addr
atom oops_addr
sequence x8
--object dbg
object fk
integer fkt
--trace(1)
--if node = 67556 then trace(1) end if
--if node = 67560 then trace(1) end if
--  printf(1,"%d: ",{node})
--  ?s
    l = length(s)
    -- fixup substrings, subsequences, and floats...
    for i=1 to l do
        si = s[i]
        if not integer(si) then
            if atom(si) then
--DEV should try and pool these...
                s_addr = floor(d_addr/4)+#40000002
                s[i] = s_addr
                appenddsDword(1)    --refcount
--if not isString(data_section) then ?9/0 end if
                appenddsType(#12)
--              data_section &= {0,0,0,#12} -- type
--if not isString(data_section) then ?9/0 end if
                x8 = atom_to_float64(si)
--              appenddsBytes(x8)
                data_section &= x8
--if not isString(data_section) then ?9/0 end if
                DSvsize += 16
                d_addr += 16
            else
                tidx = si[1]
                fkt = T_Dsq
                if tidx<0 then -- a substring
                    tidx = 0-tidx
                    fkt = T_string
                end if
                k = tt[tidx+EQ]
                fk = flatsym[k]
if DEBUG then
    if fk[S_NTyp]!=S_Const then ?9/0 end if
    if fk[S_vtype]!=fkt then ?9/0 end if
end if
                s_addr = fk[S_Slink]*4-3
                s_addr = getdsDword(s_addr)
                s[i] = s_addr
                if s_addr=#40000000 then
--                  puts(1,"oops!\n")
                    oopsSet = append(oopsSet,tidx)
--                  oopsAddr = append(oopsAddr,(d_addr+16+i*4)-DSvaddr-ImageBase)
--DEV:
--  if ((d_addr+16+i*4)-DSvaddr-ImageBase)!=(DSvsize+16+i*4) then ?9/0 end if
                    oopsAddr = append(oopsAddr,DSvsize+16+i*4)
                    oopsLen += 1
                else
                    s_addr = (s_addr-#40000000)*4-DSvaddr-ImageBase-8
                    refcount = getdsDword(s_addr+1)
                    refcount += 1
                    setdsDword(s_addr,refcount)
                end if
            end if
        end if
    end for
    refcount = 0
    s_addr = floor(d_addr/4)+#40000005
    while oopsLen do
        k = find(node,oopsSet)
        if k=0 then exit end if
        oops_addr = oopsAddr[k]
--      if getdsDword(oops_addr+1)!=#40000000 then ?9/0 end if
        setdsDword(oops_addr, s_addr)
        oopsSet[k] = oopsSet[oopsLen]
        oopsAddr[k] = oopsAddr[oopsLen]
        oopsLen -= 1
--DEV unnecessary?? / [1..0] at end of loop??
        oopsSet = oopsSet[1..oopsLen]
        oopsAddr = oopsAddr[1..oopsLen]
        refcount += 1
    end while
    slink = tt[node+EQ]
    while slink do
--      si = symtab[slink]
        si = flatsym[slink]
--      symtab[slink] = 0   -- kill refcount
        flatsym[slink] = 0  -- kill refcount
        si[S_value] = s_addr
--?     si[S_Name] = 0
--      symtab[slink] = si
        flatsym[slink] = si
        if si[S_NTyp]=S_Const
        or and_bits(si[S_State],K_noclr) then
            setdsDword(si[S_Slink]*4 - 4, s_addr)
            fixupConstRefs(si[S_ConstChain],12,slink)   -- (12 from: base,maxlen,length,/refcount/)
        end if
        refcount += 1
--      slink = si[S_Nlink]
        slink = si[S_Clink]
    end while

    flatdump(s,refcount)
    return 1
end function
constant r_DumpSequence = routine_id("DumpSequence")

integer rescan, rescancount
function ReconstructSequence(sequence s, integer node)
integer l, tidx, k, slink, state
object si
sequence sk     -- copy of symtab[k]
integer skt     -- verify sk[S_vtype], check for rescan
--trace(1)
    slink = tt[node+EQ]
    si = symtab[slink]
    state = si[S_State]
    if not and_bits(state,K_sqr) then   -- not already processed
        l = length(s)
        -- fixup substrings and subsequences...
        for i=1 to l do
            si = s[i]
            if not integer(si) then
                if not atom(si) then
                    tidx = si[1]
                    skt = T_Dsq
                    if tidx<0 then -- a substring
                        tidx = 0-tidx
                        skt = T_string
                    end if
                    k = tt[tidx+EQ]
                    sk = symtab[k]
if DEBUG then
                    if sk[S_NTyp]!=S_Const then ?9/0 end if
                    if sk[S_vtype]!=skt then ?9/0 end if
end if
                    if skt = T_Dsq then
                        state = sk[S_State]
                        if not and_bits(state,K_sqr) then
--printf(1,"ReconstructSequence: rescan on %d, ",node) ?s
--if getc(0) then end if
                            rescan = 1
                            return 1
                        end if
                    end if
                    s[i] = sk[S_value]
                end if
            end if
        end for
--printf(1,"ReconstructSequence: fixup on %d, ",node) ?s
--if getc(0) then end if
        while slink do
            si = symtab[slink]
            symtab[slink] = 0   -- kill refcount
            si[S_value] = s
            state = si[S_State]
            state = or_bits(state,K_sqr)
            si[S_State] = state
            symtab[slink] = si
            slink = si[S_Clink]
        end while
    end if
    return 1
end function
constant r_ReconstructSequence = routine_id("ReconstructSequence")


-- pathset is plain {string,string,string}
-- fileset is nested {{n,str[,str,str,..]}}.
procedure dumpPathSet(sequence s)
-- dump pathset, aka symtab[T_pathset], which is {string,string,string,....}
--   or fileset[i], which is {n,string[,string,string,...]}
object si
    for i=1 to length(s) do
        si = s[i]
        if string(si) then
            dumpString(si,1)
            s[i] = s_addr
        end if
    end for
    flatdump(s,1)
end procedure

procedure dumpFileSet(sequence s)
-- dump fileset, aka symtab[T_fileset], which is {{n,string}}, except for
--  the first, which may be {n,string[,string,string,..]}.
    for i=1 to length(s) do
        dumpPathSet(s[i])
        s[i] = s_addr
    end for
    flatdump(s,1)
end procedure


function DumpSignature(sequence s, integer node)
integer refcount, slink
    refcount = 0
    s_addr = floor(d_addr/4)+#40000005
    slink = tt[node+EQ]
    while slink do
        flatsym[slink][S_sig] = s_addr
        refcount += 1
        slink = flatsym[slink][S_Nlink]
    end while
    flatdump(s,refcount)
    return 1
end function
constant r_DumpSignature = routine_id("DumpSignature")

procedure DumpSignaturesAndLineTables()
integer rlink, tEQ
sequence si
object lt
    clear_seq_tree()    -- create a new "signature pool"
    rlink = T_maintls
    while rlink do
        si = flatsym[rlink]
        flatsym[rlink] = 0  -- kill ref count
        -- throw signature into the pool
        tt_sequence(si[S_sig])
--      si[S_sig] = tt[ttidx+EQ]    -- leave this comment in for searches
        tEQ = ttidx+EQ
--13/04/2010:
--      si[S_sig] = tt[tEQ]
        si[S_Nlink] = tt[tEQ]
        tt[tEQ] = rlink
        -- dump the linetab (non-pooled!)
        lt = si[S_ltab]
        if sequence(lt) then
            flatdump(lt,1)
            si[S_ltab] = s_addr
        end if
        flatsym[rlink] = si
        rlink = si[S_Slink]
    end while
    tt_traverseQ(r_DumpSignature)
end procedure

--DEV goes with trimST:
--function DumpInfo(sequence s, integer node)
--integer refcount, slink
--  refcount = 0
--  s_addr = floor(d_addr/4)+#40000005
--  slink = tt[node+EQ]
--  while slink do
--      flatsym[slink][S_gInfo] = s_addr
--      refcount += 1
--      slink = flatsym[slink][S_Nlink]
--  end while
--  flatdump(s,refcount)
--  return 1
--end function
--constant r_DumpInfo = routine_id("DumpInfo")

--procedure DumpGinfo()
--integer tEQ
--object si, sigInfo
--  clear_seq_tree()    -- create a new "gInfo pool"
----    rlink = T_maintls
----    while rlink do
--  for i=1 to length(flatsym) do
--      si = flatsym[i]
--      if sequence(si)
--      and si[S_NTyp]<S_Type
--      and length(si)>=S_gInfo then
--          sigInfo = si[S_gInfo]
--          if sequence(sigInfo) then
--              flatsym[i] = 0  -- kill ref count
--              -- throw gInfo into the pool
--              tt_sequence(sigInfo)
----                si[S_gInfo] = tt[ttidx+EQ]  -- leave this comment in for searches
--              tEQ = ttidx+EQ
--              si[S_gInfo] = tt[tEQ]
--if length(si)>=S_gNew then
--              si[S_gNew] = 0
--end if
--              tt[tEQ] = i
--              flatsym[i] = si
--          end if
--      end if
--  end for
--  tt_traverseQ(r_DumpInfo)
--end procedure

--with trace
procedure DumpSymTab()
object si
--newEBP (to go)
--integer K_Parm1, K_Lend
integer siNTyp, lensi
integer p, N
--  for i=1 to length(flatsym) do
    for i=1 to symlimit do
--if i=867 then trace(1) end if
        si = flatsym[i]
        if sequence(si) then
            siNTyp = si[S_NTyp]
            if siNTyp>=S_Type
            and (sequence(si[S_sig]) or         -- (DumpSignature did not hit)
                 sequence(si[S_il]) or
                 sequence(si[S_ltab])) then     -- (DumpSignaturesAndLineTables did not hit)
--printf(1,"symtab[%d] killed\n",i)
                flatsym[i] = 0                  -- delete unused routine...
--DEV:
--si[S_sig] = 0
--si[S_il] = 0
--si[S_ltab] = 0
--flatdump(si,1)
--flatsym[i] = s_addr
                if siNTyp!=S_Type then
                    if listing then -- else symtab is {}, we're working purely on flatsym!
                        if not show_full_symtab then
                            symtab[i] = 0
                        else
                            symtab[i][S_il] = 0
                            symtab[i][S_FPno] = 0
                        end if
                    end if
                    if i>T_Bin then
                        if siNTyp=S_Func then
                            flatsym[i-1] = 0    -- ... any return var ...
                            if listing then
                                if not show_full_symtab then
                                    symtab[i-1] = 0
                                end if
                            end if
                        end if

                        -- ... and any params & locals.
                        -- NB. tvars only. Avoid any temptation in eg (unused)
                        --          procedure hi()
                        --              puts(1,"hello")
                        --          end procedure
                        -- to get rid of the S_Const "hello" - there is as yet no
                        -- way to tell whether it is used sometime later or not.
                        -- BTW: For builtins and fwd routines, the tvars may be a
                        --      long way from the routine entry; and for unused  
                        --      builtins K_Lend = 0 avoids this loop entirely.
                        p = si[S_Parm1]
                        N = si[S_Ltot]
                        while p>0 do
                            si = flatsym[p]
                            if not sequence(si) then ?9/0 end if
                            siNTyp = si[S_NTyp]
                            if siNTyp!=S_TVar then ?9/0 end if
                            flatsym[p] = 0
                            if listing then
                                if not show_full_symtab then
                                    symtab[p] = 0
                                end if
                            end if
                            p = si[S_Slink]
                            N -= 1
                        end while
--DEV erm...
--                      if N!=0 then ?9/0 end if
                    end if --i>T_Bin
                end if  -- siNTyp!=S_Type
            else
                if siNTyp<=S_TVar then
                    flatsym[i] = 0
if not integer(si[S_value]) then
--puts(1,"si[S_value] = 0 -- [DEV to go?] (pemit.e line 2151)\n")   -- NO!
                    si[S_value] = 0 -- [DEV to go?]
end if
--if trimST then
                    lensi = length(si)
----5/9/09:
--if lensi>=S_gInfo
--and sequence(si[S_gInfo]) then
--?i
--?si
--  si[S_vtype] = si[S_gInfo][gType]
--?si
--end if
                    if siNTyp = S_TVar then
                        if lensi>S_Tidx then
                            si = si[1..S_Tidx]  -- discard compile-time cruft [S_ErrV]
                        end if
                    else
--                  elsif siNTyp<=S_GVar2 then  -- (always true)
                        if lensi>S_Clink then
                            si = si[1..S_Clink] -- discard compile-time cruft [S_ErrV]
                        end if
--else
--  puts(1,"?9/0 (pemit.e line 2173\n")
                    end if
--end if
--DEV 19/12:
                    -- Delete unused entries. Function results depend on
                    -- the routine entry (i+1), and (may) get deleted in 
                    -- the next iteration, some 55 lines above.
--                  if not and_bits(si[S_State],S_used+K_Fres)
                    if siNTyp<=S_GVar2  -- added 8/1/09
                    and not and_bits(si[S_State],S_used+S_set+K_Fres)
                    and not equal(si[S_Name],-1) then
--printf(1,"symtab[%d] killed2\n",i)
                        flatsym[i] = 0
                        if listing then -- else symtab is {}, we're working purely on flatsym!
                            if not show_full_symtab then
                                symtab[i] = 0
                            end if
                        end if
                    else
                        flatdump(si,1)
                        flatsym[i] = s_addr
                    end if

                else -- siNTyp>=S_Nspc
--                if trimST then
--                  if siNTyp>S_Nspc then
                    if siNTyp>S_Rsvd then
                        lensi = length(si)
                        if lensi>S_1stl then
                            si = si[1..S_1stl]      -- discard compile-time cruft [S_ErrR]
                        end if
                    end if
--                end if
--DEV 19/12:
                    flatdump(si,1)
                    flatsym[i] = s_addr
                end if
--DEV 19/12:
--              flatdump(si,1)
--              flatsym[i] = s_addr
            end if
        end if
    end for
    flatdump(flatsym,1)
end procedure


--DEV I've hard-coded 4096 here, might instead want to do something like:
--constant k32 = open_dll("kernel32.dll"),
--       xGetSystemInfo = define_c_proc(k32,"GetSystemInfo",{C_POINTER}),
--       SYSTEM_INFO = allocate(36),
--       SYSTEM_INFO_dwPageSize = 4
--
--  c_proc(xGetSystemInfo,{SYSTEM_INFO})
--
--constant dwPageSize = peek4s(SYSTEM_INFO+SYSTEM_INFO_dwPageSize),
--       psm1 = dwPageSize-1,
--       highbits = xor_bits(psm1,#3FFFFFFF)
--then, instead,
-- round block up to next dwPageSize bytes
--  bytesize = and_bits(highbits,bytesize+psm1)

function AllocateBlock(integer bytesize)
-- round block up to next 4096 bytes
    bytesize = and_bits(#3FFFF000,bytesize+#FFF)
    return allocate(bytesize)
end function

--with trace
global procedure finalfixups(sequence path, sequence outfile, atom t)
-- In bind mode copy the stub cl1 (usually p.exe) to outfile, 
--  dump code linked to this stub, along with the symtab.
-- In interpret mode outfile is not used, code is linked to 
--  the VM under the compiler and existing symtab in memory.
integer vmax, k, symidx, u, nTyp, ridlink, nslink, f, lensi
,vtype
object sv, svil, xi, si
sequence s5sets
atom t0, rbicb
integer fixupLineTab
--integer sfn

integer siNTyp

integer kfirst, klast
integer xType,xMin,xMax

integer isKridt
sequence re 

--integer siNTyp, K_Parm1, K_Lend, lensi
--integer K_Parm1, K_Lend

-- 1/2/10:
integer node, slink, snext

--/**/integer lfn, lch, lidx
--/**/object ltxt

integer firsttime

integer p, maxparams, N

object dbg
object opName -- for dumpil (to dump opNames[])
--integer s5lentmp
----DEV temp!
--puts(1,"finalfixups!\n")

--?symtab[22]
--  if bind then
--      readAllHeaders()
--  else
----/**/    #ilasm{ mov_edi_imm32,%isVar,0,0,VMep,          -- mov edi,VMep
----/**/            mov_ecx_imm32,%isVar,0,0,asmoptions,    -- mov ecx,asmoptions
----/**/            call_rel32,%isOpCode,0,0,%opGetVMep}    -- [edi]=VMep; [ecx]=asmoptions
--      CSvaddr = 0
--      ImageBase = 0
--  end if
    readAllHeaders()
-- 16/2 now includes virtual opcodes...
--  if length(VMep)!=length(opNames) then ?9/0 end if
    if length(VMep)!=maxNVop then ?9/0 end if

    -- check for unused/undefined/unassigned and if debugleak emit cleanup code (see pilxl.e):
--if repl=0 then
    unused_cleanup(asmoptions)
    if listing then
        craddr={}
        cridx={}
    elsif bind>=2 then
        if bind>3 then re="re-" else re="" end if
        printf(1,"Self-host round %d: compiled OK (%3.2fs), %screating %s ",{bind-1,t,re,outfile})
    end if
--end if

--  if not LastStatementWasAbort then
--      Emit1(opRetf)
--  end if
--27/8/14: (does not help)
    s5 = append(s5,opRetf)
--  emitline = line
--  apnds5(opRetf)

    symtab[T_maintls][S_il] = s5

--pp(filepaths)
--pp(filenames)

    --
    -- finalise gvar nos and do initial linkup of udts and toplevelsubs:
    --
    vmax = -TIDX
--?vmax
    vi = 0
--  for v=1 to length(symtab) do

    for v=1 to symlimit do
        sv = symtab[v]
--?{v,sv}
        if sequence(sv) then
            nTyp = sv[S_NTyp]
            if nTyp<=S_GVar2 then   -- S_Const and S_Gvar2
                sv = 0
                vmax += 1
                symtab[v][S_Slink] = vmax
--          elsif nTyp=S_Tvar then
--              sv = 0
--              symtab[v][S_Slink]
            elsif (nTyp=S_Type and v>T_Bin)                             -- user defined types
--             or (nTyp>S_Type and and_bits(sv[S_State],K_used)) then   -- top level subs
--             or (nTyp>S_Type and and_bits(sv[S_State],K_used) and length(sv[S_il])) then  -- top level subs
--repl:
--             or (nTyp>S_Type and and_bits(sv[S_State],K_used) and length(sv[S_il])) -- top level subs
               or (nTyp>S_Type and and_bits(sv[S_State],K_used) and sequence(sv[S_il]) and length(sv[S_il])) -- top level subs
-- 24/6/10
               or (v=T_command_line and Z_command_line) then -- link up T_command_line if rqd
--DEV if nTyp>SType and sv[S_il] = jmp opRetf then we could kill off the entry, as long as 
--      we also kill all opCallOnce to it (or merge to one dummy). Not that you'd ever be
--      able to measure the savings...
                --
                -- We do this because we don't track opCallOnce or opTchk in the
                -- same way that we do with opFrame (see scanforShortJmp).
                -- True, there may be some udt processed that are not strictly
                -- needed, but they are usually quite small, compared to say
                -- pretty_print, wildcard_match or maybe half of win32lib.
                -- Alternatively: don't auto-linkup S_Type here but track opTchk
                --  in scanforShortJmp; I think you'd find that slower.
                --  update: erm, track opTchk would now be in pgscan.e, I think.
                --
----DEV temp!
--printf(1,"pemit.e line 2088: linking symtab[%d] on vi chain\n",v)
                sv = 0
                symtab[v][S_Slink] = vi
                vi = v
-- 1/02/10:
            elsif nTyp>S_Type then
                symtab[v][S_Slink] = -9     -- (not on symtab[T_maintls][S_Slink] chain)
            end if
        end if
    end for

    symtab[T_maintls][S_Slink] = vi -- more entries will be dynamically added to this chain...

--DEV test code (seems fine):
--  u = 0
--  for v=symlimit to 1 by -1 do
--      sv = symtab[v]
--      if sequence(sv) then
--          nTyp = sv[S_NTyp]
--          if nTyp<=S_GVar2 then   -- S_Const and S_Gvar2
--              u = sv[S_Slink]
--              exit
--          elsif nTyp=S_TVar
--            and u=0 then
--              u = sv[S_Tidx]
--          end if
--      end if
--  end for
--  if u!=vmax then ?9/0 end if


--  check_symtab()          -- ... some in here, the rest in scanforShortJmp.
--integer u,f
--integer k, p
--object si     -- symtab[i]
--integer lensi -- length(si)
--integer siNTyp    -- symtab[i][S_NTyp]

--trace(1)
    for i=1 to T_object do
        si = symtab[i]
        if sequence(si) then
            symtab[i] = 0           -- kill refcount
            u = si[S_State]
            u = or_bits(u,K_used)
            si[S_State] = u
            symtab[i] = si
        end if
    end for

    if some_unresolved_rtnids then
        for i=T_Bin+1 to symlimit do
            si = symtab[i]
            if si[S_NTyp]>S_Type then   -- ie func or proc
                u = si[S_State]
--DEV 20/09/2013 try pulling this tooth then... (solves problem of [indirect] routineid("open_dll") getting -1)
--              if i>T_Ainc or and_bits(u,S_used) then
                if not atom(si[S_il]) then
                    f = si[S_FPno]
--DEV (16/03/2013) if we add parentscope to routine_id then we must add locals...
-- while fixing this, try an inner "if atom(si[S_il]) then ?9/0 end if -- sanity check"
-- (5/7/13: in 4.0.5 you can procedure p(integer rid=routine_id("fred")) and it will
--          resolve to a fred declared in the caller, not the callee... ittwima)
                    if and_bits(u,K_gbl)
--                  or (not atom(si[S_il]) and f and unresolved_routine_ids[f]) then
                    or (f and unresolved_routine_ids[f]) then
                        if not and_bits(u,K_used) then  -- not top_level_sub
                            symtab[i] = 0               -- kill refcount
                            u = or_bits(u,K_used+K_ridt)
                            si[S_State] = u
                            si[S_Slink] = symtab[T_maintls][S_Slink]
                            symtab[i] = si
                            symtab[T_maintls][S_Slink] = i
                        end if
                    end if
                end if
            end if
        end for
    end if

--  dbg = symtab[601]
--DEV if not bind and addRoutineId was never called then return

if repl=0 then
    for i=symlimit to 1 by -1 do
--if i=388 then trace(1) end if
        si = symtab[i]
        if sequence(si) then
--          siNTyp = si[S_NTyp]     -- DEV unnecessary?
            u = si[S_State]
            if and_bits(u,K_rtn) then
                if bind then
--if trimST then
                    lensi = length(si)
                    if lensi>S_Clink then
                        symtab[i] = 0
----5/9/09:
--if lensi>=S_gInfo
--and sequence(si[S_gInfo]) then
--?{i,si}
--  si[S_vtype] = si[S_gInfo][gType]
--end if
                        si = si[1..S_Clink] -- discard compile-time cruft [S_Tidx..S_Init]
                        symtab[i] = si
                    end if
--end if
                end if
                k = si[S_value]     -- the routine number
                si = symtab[k]
                u = si[S_State]
                if not and_bits(u,K_used) then
                    u = or_bits(u,K_used)
--DEV (16/03/2013) this is unnecessary??
                    symtab[k] = 0   -- kill refcount
                    si[S_State] = u
                    si[S_Slink] = symtab[T_maintls][S_Slink]
                    symtab[k] = si
                    symtab[T_maintls][S_Slink] = k
                end if
            end if
        end if
    end for
    si = 0  -- kill refcount
end if

    -- finalise code segments
    DSvsize = vmax*4
    CSvsize = 0
    s5sets = {}
    s5sizes = {}
    s5v = {}
    if debug then
        s5symn = {}
    end if
    if showfileprogress then
        printf(1,"performing final fixups, gvar_scan started at %3.2f...\n",time()-t)
        t0 = time()
    end if

--trace(1)
--trace(1)
--  vi = T_maintls  -- follow (volatile) K_used chain from top_level_sub[main], aka symtab[T_maintls][S_Slink].
--  while vi do
--      vi = symtab[vi][S_Slink]
--  end while

if bind and not dumpil then
    rescancount = 0

    while 1 do  -- while gvar_scan improves matters
        vi = T_maintls  -- follow (volatile) K_used chain from top_level_sub[main], aka symtab[T_maintls][S_Slink].
        while vi do
--si = symtab[vi] --DEV temp
            s5thunk(symtab[vi][S_il]) -- sets s5
            symtab[vi][S_il] = 0    -- kill refcount
--s5lentmp = length(s5)
            gvar_scan(vi)
--if s5lentmp!=length(s5) then ?9/0 end if
            symtab[vi][S_il] = s5
            vi = symtab[vi][S_Slink]
        end while
        
--if 01 then -- moved here from pilxl 17/2/09.
        -- kill any gvar info gathered for parameters of routines that could be
        --  invoked via call_proc/call_func/call_back (targets of routine_id),
        --  since that will not include any values/types/etc from such calls.
        vi = T_maintls  -- hop down all routines again
        while vi do
            u = symtab[vi][S_State]
            isKridt = and_bits(u,K_ridt)    -- known routine_id target?
--DEV 20/09/2013 no idea why this lot was commented out, put back in:
--      "" (ah-ha: we are scanning the K_used chain here... no good)
--          if not isKridt then
--              if some_unresolved_rtnids then
--                  if and_bits(u,K_gbl) then
--                      isKridt = 1
--                  else
--                      k = symtab[vi][S_FPno]
--                      if k and unresolved_routine_ids[k] then
--                          isKridt = 1
--                      end if
--                  end if
--              end if
--          end if
-- (20/09 ends)
            if isKridt then
                -- routine is/could be target of routine_id...
                -- so destroy any gvar info gathered by gvar_scan for the 
                --  routine parameters (since that will not include any 
                --  values from call_proc/call_func/call_back)
if newEBP then
                p = symtab[vi][S_Parm1]
                maxparams = length(symtab[vi][S_sig]) - 1
                while maxparams do
                    if DEBUG then
                        if symtab[p][S_NTyp]!=S_TVar then ?9/0 end if
                        -- sanity check: K_noclr must NOT be set on any params!
                        if and_bits(symtab[p][S_State],K_noclr) then ?9/0 end if
                    end if
--                  symtab[i][S_gNew] = 0
                    symtab[p][S_gNew] = {T_object,MININT,MAXINT,T_object,-2}
                    p = symtab[p][S_Slink]
                    maxparams -= 1
                end while
else -- not newEBP
                kfirst = symtab[vi][S_Parm1]
                if kfirst>0 then
                    -- S_ParmN holds the min no of params, length(sig) max+1
                    --  (plus 1 since sig[1] is proc/func/type)
                    klast = kfirst + length(symtab[vi][S_sig]) - 2
                    for i=kfirst to klast do
                        if DEBUG then
                            if symtab[i][S_NTyp]!=S_TVar then ?9/0 end if
                            -- sanity check: K_noclr must NOT be set on any params!
                            if and_bits(symtab[i][S_State],K_noclr) then ?9/0 end if
                        end if
--                      symtab[i][S_gNew] = 0
                        symtab[i][S_gNew] = {T_object,MININT,MAXINT,T_object,-2}
                    end for
                end if
end if
            end if
            vi = symtab[vi][S_Slink]
        end while
--end if

--DEV: (now a sep. loop)
--      if not bind then exit end if
        rescan = 0
        for i=1 to length(symtab) do
--if i=388 then trace(1) end if
            si = symtab[i]
            if sequence(si) then
                siNTyp = si[S_NTyp]
                if siNTyp <= S_TVar
                and (siNTyp!=S_Const or not and_bits(si[S_State],K_lit+K_rtn)) then
                    xi = si[S_gNew]
                    if sequence(xi) then
                        vtype = si[S_vtype]
sv = si[S_Name]
                        si = si[S_gInfo]
                        symtab[i][S_gNew] = 0
                        if not equal(si,xi) then
--                          if xi[gType]=0 then
----                                xi[gType] = T_object
--                              xi[gType] = rootType(vtype)
--                          end if
--                          if xi[gEtyp]=0 then
--                              xi[gEtyp] = T_object
--                          end if
----DEV 9/12:
--  if and_bits(xi[gType],T_atom)!=T_integer
--  or xi[gMin]>xi[gMax] then
--      xi[gMin] = MININT
--      xi[gMax] = MAXINT
--  end if
----DEV 05/01: don't allow MIN/MIN or MAX/MAX...
--   (makes testing easier: problem is that Min=Max means fixed, but MAX is (eg) +=1 indicator;
--    ie MAX/MAX would be created by i=#3FFFFFFF i+=1 but Min=Max would mean all refs(/stores)
--       of i are treated as #3FFFFFFF, and hence no overflow occurs when obviously it should.
--       Minor downside is of course i=#3FFFFFFF i+=1 leaves i as MIN/MAX, a bit "odd" but ok.)
                            xType = xi[gType]
                            if xType=0 then
--DEV 15/4/2010. skip temps (see challenge0001)
if not equal(sv,-1) then
--                              printf(1,"xType=0 on symtab[%d]\n",i})
                                k = symtab[i][S_FPno]
                                printf(1,"xType=0 on symtab[%d] (%s in %s)\n",{i,getname(sv,-2),filenames[k][2]})
end if
--                              xType = rootType(vtype)
                                if vtype>T_object then vtype = rootType(vtype) end if
--                              xi[gType] = xType
                                xi[gType] = vtype
                            end if
                            if xi[gEtyp]=0 then
-- removed 7/2/14:
--                              if and_bits(xType,T_sequence)
--                              and xi[gLen]!=0 then    -- let p={} (and nowt else) pass by unnoticed.
--                                  printf(1,"xEtyp=0 on symtab[%d]\n",i)
--                                  ?xi
--                              end if
                                xi[gEtyp] = T_object
                            end if
                            xMin = xi[gMin]
                            xMax = xi[gMax]
                            if and_bits(xType,T_atom)!=T_integer
                            or xMin>xMax
                            or (xMin=xMax and (xMin=MININT or xMin=MAXINT)) then
                                xi[gMin] = MININT
                                xi[gMax] = MAXINT
                            end if

--No good:
--                      if sequence(si)         -- rely on best when dealing with circulars
--                      and rescancount>1 then  -- avoid early dependence on K_noclr values
--                          xi[gMin] = max(xi[gMin],si[gMin])
--                          xi[gMax] = min(xi[gMax],si[gMax])
--                      end if
                            if not equal(si,xi) then
--                          if sequence(si) then
--                              xi[gMin] = min(xi[gMin],si[gMin])
--                              xi[gMax] = max(xi[gMax],si[gMax])
--                          end if

-- If you get problems with this:
--  1) If it really is an infinite loop, I suspect it really is a problem.
--      (maybe try commenting out getc(0) and let it run for 10 mins?)
--  2) If it resolves in a few more iterations, obviously increase to fit.
--      See example (search for "The process of repeated iteration.") 
--      requiring 9 scans in pilx86.e, if we find a real-world program 
--      that needs a silly number of iterations (eg 7,654), then maybe a 
--      reasonable cap really might be in order...
--  3) You /might/ get a bit further uncommenting the >16 exit below, at
--      least be able to get a list.asm with a few more clues in it.
if rescancount>16 then
    ?i
    ?xi
    ?si
    if getc(0) then end if
end if
--?i
--?xi
                                symtab[i][S_gInfo] = xi
                                rescan = 1
                            end if  -- si!=xi
                        end if  -- si!=xi
                    end if  -- sequence(xi)
                end if  -- tvar/rt-assigned const
            end if  -- sequence(si)
        end for

        si = 0  -- kill refcount
--8/6/10:
        xi = 0  -- ""
-- 15/3/10:
--      if not rescan then exit end if
        if not rescan then
            ltDiagMsg("*** final pass ***\n")   -- (pltype.e diagnostics)
            exit
        end if
        rescancount+=1
--      if rescancount>16 then exit end if      -- RDS/Knuth suggest 7 as a reasonable max
                                                -- (though technically I suspect Knuth was 
                                                --  referring to a different problem.)
        if rescancount>16 then
            if rescancount>18 then exit end if
            puts(1,"\nrescanning...\n")
        end if
        ltDiagMsg("*** rescanning ***\n")   -- (pltype.e diagnostics)
--      puts(1,"*** rescanning ***\n")
    end while

    -- test set is mainly 4; t40,t45 are 3, t21,t36 are 5, t43,t46 are 6.
    --  win32lib is 7, euex.exw 12(!!), arwen 6, edita 9, and p.exw is 6.
--  printf(1,"gvar_scan: %d iterations\n",rescancount)

else -- not bind, or dumpil

    if showfileprogress then
        rescancount = -1
    end if
----DEV temp...
--puts(1,"fixme...\n")
--  if 1 or dumpil then
    if dumpil then
        relink()
        tt_traverse(r_ReconstructIds,"",-2)             -- identifiers
    end if
    vi = T_maintls  -- follow (volatile) K_used chain from top_level_sub[main], aka symtab[T_maintls][S_Slink].
    while vi do
--si = symtab[vi] --DEV temp
--?vi
--?symtab[vi]
        s5thunk(symtab[vi][S_il])   -- set s5
        gvar_scan_nobind(vi)
        vi = symtab[vi][S_Slink]
    end while
    if dumpil then
if 01 then
        puts(dilfn,"\n\n Opcodes:\n")
        puts(dilfn," (obviously, s5 appears in trace() as say {210,1,213,...};\n")
        puts(dilfn,"  and you may need these to map it to {opLn,1,opCtrl,...}.)\n")
        for i=1 to length(opNames) do
            opName = opNames[i]
            if opName=0 then
                opName = "<spare>"
            end if
            printf(dilfn,"%4d:%s\n",{i,opName})
        end for
end if
        Warnings(dilfn)     -- (closes the file)
--      close(dilfn)
        puts(1,"listing file "&mainpath&"ildump.txt created.\n")
        abort(0)
    end if

end if -- bind/dumpil

--  gopshow() -- (see pgscan.e)
--  abort(0)

    if showfileprogress then
--      printf(1,"performing final fixups, scanforShortJmp started at %3.2f...\n",time()-t)
        printf(1,"scanforShortJmp started at %3.2f, gvar_scan[%d] took %3.2f seconds...\n",
                 {time()-t,rescancount,time()-t0})
        t0 = time()
    end if

--trace(1)
    vi = T_maintls  -- follow (volatile) K_used chain from top_level_sub[main], aka symtab[T_maintls][S_Slink].

    while vi do
        sv = symtab[vi]
        symtab[vi] = 0                              -- kill refcount
        s5thunk(sv[S_il])   -- set s5
        sv[S_il] = CSvaddr+CSvsize+ImageBase        -- NB when interpreting CSvaddr[=allocate(CSvsize)] added later
        LineTab = {}    -- build a new one!
        ltline = 0
        symtab[vi] = sv
        sv = sv[S_Name]                             -- save name for debug, also kills refcount
        ilxlate(vi)
--sv = getname(sv,-2)
--puts(1,sv&'\n')
        scanforShortJmp(vi)
        if length(LineTab) then
            symtab[vi][S_ltab] = LineTab
        else
            symtab[vi][S_ltab] = 0
        end if
        s5sets = append(s5sets,s5)
        s5sizes = append(s5sizes,thisCSsize)
        s5v = append(s5v,vi)
        CSvsize += thisCSsize
        if debug then
            if equal(sv,-1) then    -- a top_level_sub (no name)
                s5symn = append(s5symn,0)
            else
                -- reserve space for a cmp eax,<name> debug aide,
                -- the non-zero value in s5symn indicates it is present, and
                --  later in the pre-dump() compacted version of this table
                --  triggers the addr name patch as name is dump()'d [phew]:
                -- (This is probably only of use if you have a debug build of 
                --  the VM, which I do not plan to publically release, btw)
                s5symn = append(s5symn,vi)
                CSvsize += 5
            end if
        end if
        vi = symtab[vi][S_Slink]
    end while

    if countTransTmpFer then
        opshow()
        abort(1)
    end if

    if showfileprogress then
        printf(1,"blurph started at %3.2f, scanforShortJmp took %3.2f seconds...\n",{time()-t,time()-t0})
        t0 = time()
    end if

    if bind then
        setmzpe(#220,CSvsize,DWORD)
        CSrsize = and_bits(CSvsize+FAless1,FAmask)
        setmzpe(#228,CSrsize,DWORD)
        DSvaddr = and_bits(CSvaddr+CSvsize+SAless1,SAmask)
        setmzpe(#24C,DSvaddr,DWORD)
        DSraddr = CSraddr+CSrsize
        setmzpe(#254,DSraddr,DWORD)
    else
-- 24/3/2010:
--      CSvaddr = allocate(CSvsize)
        CSvaddr = AllocateBlock(CSvsize)
--      DSvaddr = allocate(DSvsize)
        DSvaddr = AllocateBlock(DSvsize)
--- 26/2/10:
--      poke4(DSvaddr,#40000000)
--      mem_copy(DSvaddr+4,DSvaddr,DSvsize-4)
        poke4(DSvaddr,{0,           -- slack
                       vmax-5*4,    -- maxlen
                       vmax-5,      -- length
                       1,           -- refcount
                       #80000000,   -- T_Dsq
                       #40000000})  -- unassigned
        mem_copy(DSvaddr+24,DSvaddr+20,DSvsize-24)

        -- for isIL, we must do them all in one go, before calling blurph()
        for v=1 to length(s5sets) do
            symidx = s5v[v]
            -- see emitinit()/readmzpe(). When binding CSvaddr is picked up 
            -- from the .exe, when interpreting we must calculate CSvsize 
            -- first and then allocate() it to obtain CSvaddr.
            svil = symtab[symidx][S_il]
            symtab[symidx][S_il] = svil+CSvaddr
        end for
    end if

--added 24/3/09:
    if not bind then
--      if debug then
--      if debug or listing then    -- 19/8/9
        if debug or listing or some_unresolved_rtnids then  -- 21/09/2013
            relink()
            tt_traverse(r_ReconstructIds,"",-2)             -- identifiers
        end if
--9/10/10: moved here (to be before blurph, for isConstRef[Count])
        -- reconstruct any nested sequences
        while 1 do
            rescan = 0
--puts(1,"ReconstructSequence scan started\n")
            tt_traverseQ(r_ReconstructSequence)             -- sequence constants
            if not rescan then exit end if
        end while
    end if

    code_section = repeat(0,CSvsize)
    CSvsize = 0
--trace(1)
    for v=1 to length(s5sets) do

        s5thunk(s5sets[v])  -- sets s5

        thisCSsize = s5sizes[v]
        symidx = s5v[v]
----DEV temp:
--printf(1,"[%d]:",symidx)
--if atom(symtab[symidx][S_Name]) then
--  puts(1,"blurph -1\n")
--else
--  puts(1,"blurph "&symtab[symidx][S_Name]&"\n")
--end if
--      svil = symtab[symidx][S_ltab]
        xi = symtab[symidx]
        svil = xi[S_ltab]
        xi = xi[S_Name]
        if sequence(svil) then
            LineTab = svil
            svil = 0
        else
--DEV try {} here and 0 below
            LineTab = {-2}
        end if
        if debug then
            fixupLineTab = s5symn[v]
            if fixupLineTab then
                CSvsize += 1
                code_section[CSvsize] = #3D     -- cmp eax,xxx
                if not bind then
                    if atom(xi) then ?9/0 end if -- sanity check
                    -- patch the code start to cmp eax,["myroutine"] to aid debugging.
--!/**/             #ilasm{ mov_edi_mem32,%isVar,0,0,xi,                -- mov edi,[xi]         ; source ref
--!/**/                     shl_edi_imm8,2,                             -- shl edi,2            ; -> raw addr
--!/**/                     mov_mem32_edi,%isVar,0,0,d_addr}            -- mov [d_addr],edi     ; store (as int)
--/**/              #ilASM{ mov edi,[xi]        --; source ref
--/**/                      shl edi,2           --; -> raw addr
--/**/                      mov [d_addr],edi }  --; store (as int)
                    -- (the above may break on .exe larger than 1GB)
                    CSvsize += 1
                    setcsDword(CSvsize,d_addr)
                    CSvsize += 3
                else -- equivalent patch will get done inside dump().
                    s5sizes[v] = CSvsize+1
                    CSvsize += 4
                end if
            end if
        end if
--      if not bind then
--          -- see emitinit()/readmzpe(). When binding CSvaddr is picked up 
--          -- from the .exe, when interpreting we must calculate CSvsize 
--          -- first and then allocate() it to obtain CSvaddr.
--          svil = symtab[symidx][S_il]
--          symtab[symidx][S_il] = svil+CSvaddr
--      end if
        symtab[symidx][S_ltab] = 0
--trace(1) [DEV]
dbg = symtab[symidx]
        blurph()
        if debug then
            if fixupLineTab then
                for i=1 to length(LineTab) do
                    fixupLineTab = LineTab[i]
                    if fixupLineTab>=0 then
                        LineTab[i] = fixupLineTab+5
                    end if
                end for
            end if
        end if
        symtab[symidx][S_ltab] = LineTab
    end for
    if showfileprogress then
        printf(1,"symtab dump started at %3.2f, blurph took %3.2f seconds...\n",{time()-t,time()-t0})
        t0 = time()
    end if

    --
    -- Code done, now the threadstack and global data, and the symtab.
    --

    if bind then
--26/2/10:
--      data_section = repeat(0,DSvsize)
        data_section = repeat(' ',DSvsize)
--if not isString(data_section) then ?9/0 end if
--      for zzi=4 to DSvsize by 4 do    -- mark all threadstack entries unassigned
--          data_section[zzi] = #40
--      end for
--      set up a dummy T_Dsq header at the start of threadstack:
        setdsDword(0,0)             -- slack
        setdsDword(4,vmax*4)        -- maxlen
-- ok here
--!/**/ printf(1,"\ndata_section initially:#%02x%02x%02x%02x%02x%02x%02x%02x\n",data_section[1..8])
        setdsDword(8,vmax-5)        -- length
        setdsDword(12,1)            -- refcount
--      data_section[16] = 1                -- refcount
        setdsDword(16,#80000000)    -- T_Dsq
--      data_section[20] = #80              -- T_Dsq
        for zzi=20 to DSvsize-3 by 4 do -- mark all threadstack entries unassigned
--          data_section[zzi] = #40
            setdsDword(zzi,#40000000)   -- unassigned
--          data_section[zzi] = #40
        end for
-- ok here
--!/**/ printf(1,"\ndata_section initially:#%02x%02x%02x%02x%02x%02x%02x%02x\n",data_section[1..8])
--
--if not isString(data_section) then ?9/0 end if

if OptConsole!=-1 then
        if OptConsole then
            if Subsystem!=CUI then
                setmzpe(#DC, CUI, WORD)
            end if
        else
            if Subsystem!=GUI then
                setmzpe(#DC, GUI, WORD)
            end if
        end if
        if SubsystemVersion!=subvers then
            setmzpe(#C8, subvers, DWORD)
        end if
end if

-- The following fields must be updated before mzpe is rewritten:
--  UserCode dd usercode        ; so we know where it is...                     **DEV
--  SymTab dd symtab            ; patch with raw symtab address [DONE]
        if debug then
            -- pack the table for outputting the cmp eax,<routine_name> debug aids:
            k = 0
            for i=1 to length(s5symn) do
                if s5symn[i] then
                    k += 1
                    s5symn[k] = s5symn[i]
                    s5sizes[k] = s5sizes[i]
                end if
            end for
            if k then
                s5symn = s5symn[1..k]
            end if
        end if

--07/01/2012 moved to end:
--  else    -- not bind:
--
--      if some_unresolved_rtnids then
--          -- repopulate the symbol table with var names...
--          relink()
--          tt_traverse(r_ReconstructIds,"",-2)             -- identifiers
----        elsif debug then
---- Now done above 24/3/09:
----            relink()
----            tt_traverse(r_ReconstructIds,"",-2)             -- identifiers
------/**/  else
----!/**/ elsif not debug then
----/**/    elsif not debug and listing!=-1 then    -- 19/8/9
----/**/        rbicb = call_back(routine_id("rebuild_callback"))
----!/**/       #ilasm{ mov_edi_imm32,%isVar,0,0,rbicb,         -- mov edi,addr rebuild callback
----/**/        #ilasm{ opLeaMov,%edi,rbicb,                    -- mov ecx,addr rebuild callback
----/**/                call_rel32,%isOpCode,0,0,%opRbldIds}    -- save rebuild callback
--          rbicb = 0   -- suppress warnings
--      end if
--DEV 9/10/10 moved above blurph:
--      -- reconstruct any nested sequences
--      while 1 do
--          rescan = 0
----puts(1,"ReconstructSequence scan started\n")
--          tt_traverseQ(r_ReconstructSequence)             -- sequence constants
--          if not rescan then exit end if
--      end while
--puts(1,"ReconstructSequence scans all finished\n")
--if getc(0) then end if
    end if

    -- trash any unused entries in the symtab:

--ok here
--!/**/ printf(1,"\ndata_section initially:#%02x%02x%02x%02x%02x%02x%02x%02x\n",data_section[1..8])
--!/**/ printf(1,"data_section initially(2):#%02x%02x%02x%02x%02x%02x%02x%02x\n",data_section[1..8])
--?symtab[22]
    ridlink = T_maintls
    nslink = T_nslink
--DEV repl?
    for v=1 to symlimit do
--if v=867 then trace(1) end if
        sv = symtab[v]
        if sequence(sv) then
            nTyp = sv[S_NTyp]
            u = sv[S_State]
--          if nTyp>S_Type      -- types are not properly marked as used... yet.
--          and not and_bits(u,K_used) then
-- 1/02/10:
            if nTyp>S_Type      -- types are not properly marked as used... yet.
            and sv[S_Slink]=-9 then -- (not on symtab[T_maintls][S_Slink] chain)
--          if nTyp>=S_Type
--          and (sequence(sv[S_sig]) or
--               sequence(sv[S_il]) or      --??
--               sequence(sv[S_ltab])) then

--DEV delink...
--              symtab[v] = 0
-- -TRY using sequence(symtab[v][S_sig]) to skip...
--DEV 19/8: (copied from DumpSymTab)
if listing=-1 and v!=T_maintls then
--1/02/10 (DEV):
--if 1 then
--  if listing!=-1 then -- else S_Name just replaced (-d! option)
--  node = sv[S_Name]
--  if node!=-1 and sv[S_Nlink]!=-2 then
--      slink = tt[node+EQ]
--      if slink=v then
--          tt[node+EQ] = sv[S_Nlink]
--      else
--          while 1 do
--              snext = symtab[slink][S_Nlink]
--              if snext=v then
--                  symtab[slink][S_Nlink] = sv[S_Nlink]
--                  exit
--              end if
--              slink = snext
--          end while
--      end if
--  end if
--  end if

--?v
--?sv
--?u
--?K_used
--if getc(0) then end if
--printf(1,"deleting symtab[%d]\n",v)
                symtab[v] = 0                   -- delete unused routine...
--DEV:
--si[S_sig] = 0
--si[S_il] = 0
--si[S_ltab] = 0
--flatdump(si,1)
--flatsym[i] = s_addr
--                  if listing then
--                      if not show_full_symtab then
--                          symtab[i] = 0
--                      else
--                          symtab[i][S_il] = 0
--                          symtab[i][S_FPno] = 0
--                      end if
--                  end if
                    if v>T_Bin then
                        if nTyp=S_Func then
                            symtab[v-1] = 0 -- ... any return var ...
--                          if listing then
--                              if not show_full_symtab then
--                                  symtab[i-1] = 0
--                              end if
--                          end if
                        end if
                        -- ... and any params & locals.
                        -- NB. tvars only. Avoid any temptation in eg (unused)
                        --          procedure hi()
                        --              puts(1,"hello")
                        --          end procedure
                        -- to get rid of the S_Const "hello" - there is as yet no
                        -- way to tell whether it is used sometime later or not.
                        -- BTW: For builtins and fwd routines, the tvars may be a
                        --      long way from the routine entry; and for unused  
                        --      builtins K_Lend = 0 avoids this loop entirely.
                        p = sv[S_Parm1]
                        N = sv[S_Ltot]
                        while p do
                            sv = symtab[p]
                            if not sequence(sv) then ?9/0 end if
                            nTyp = sv[S_NTyp]
                            if nTyp!=S_TVar then ?9/0 end if
                            symtab[p] = 0
--                          if listing then
--                              if not show_full_symtab then
--                                  symtab[p] = 0
--                              end if
--                          end if
                            p = sv[S_Slink]
                            N -= 1
                        end while
                        if N!=0 then ?9/0 end if

                    end if --v>T_Bin
end if -- listing=-1 and v!=T_maintls
            else
                -- link up routines and namespaces for faster scan in routine_id
                if nTyp>=S_Type then
--DEV delink later??
--                  if v>T_Bin and not atom(sv[S_Name]) then    -- skip opcodes and toplevelsubs
--DEV:??            if v>T_Bin and not equal(sv[S_Name],-1) then    -- skip opcodes and toplevelsubs
--                  if v>T_Bin then
                    if v>T_Bin and atom(sv[S_il]) then
                        symtab[ridlink][S_Slink] = v
                        ridlink = v
                    end if
                elsif nTyp=S_Nspc then
                    if nslink=T_nslink then
                        symtab[T_nslink] = v
                    else
                        symtab[nslink][S_Slink] = v
                    end if
                    nslink = v
-- 07/01/2012:
--              elsif nTyp<=S_GVar2
--                and not and_bits(u,S_used+S_set+K_Fres+K_lit)
                elsif ( nTyp=S_Rsvd or
                       (nTyp<=S_GVar2 and
                        not and_bits(u,S_used+S_set+K_Fres+K_lit)))
                  and integer(sv[S_Name]) -- 6/3/2010 (was d!'ing at the time)
                  and not equal(sv[S_Name],-1) then
                    node = sv[S_Name]
                    slink = tt[node+EQ]
                    if slink=v then
                        tt[node+EQ] = sv[S_Nlink]
                    else
                        if nTyp=S_Rsvd then ?9/0 end if     -- sanity check (added 07/01/2012)
                        while slink do
                            snext = symtab[slink][S_Nlink]
                            if snext=v then
                                symtab[slink][S_Nlink] = sv[S_Nlink]
                                exit
                            end if
                            slink = snext
                        end while
                    end if
                    nTyp = 999  -- (avoid stuff below! (not strictly necessary))
                    symtab[v] = 0
                end if
                if not bind then
                    if nTyp<=S_GVar2 then
                        xi = sv[S_value]
--?xi
--?string(xi)
                        if not integer(xi)
--16/10/10:
--                      or (nTyp=S_Const and and_bits(u,K_noclr))
--                      or and_bits(u,K_rtn) then
--                      or nTyp=S_Const
                        or and_bits(u,K_noclr+K_rtn) then
                            d_addr = floor(DSvaddr/4)+sv[S_Slink]-1
--/**/                      #ilASM{ mov edi,[d_addr]
--/**/                              mov eax,[xi]
--/**/                              mov [ebx+edi*4],eax }
                        end if
                    end if
                else    -- bind
                    if nTyp<=S_GVar2 then
--See DEV comment in DumpAtom():
                        xi = sv[S_value]
--?xi
--?string(xi)
                        if integer(xi)
                        and and_bits(u,K_noclr+K_rtn) then
                            setdsDword(sv[S_Slink]*4-4, xi)
--!/**/ printf(1,"data_section[%d]:",v)
--!/**/ printf(1,"#%02x%02x%02x%02x%02x%02x%02x%02x\n",data_section[1..8])

--if nTyp=S_Const then
--  if sv[S_ConstChain] then ?9/0 end if    -- 18/10/09     ( removed due to length error on 1st go )
--end if
                        end if
----DEV 22/12:
                    elsif nTyp=S_TVar then
if not newEBP then --?? (added in haste, to avoid a crash, as sv[S_Tidx] was 0)
                        xi = sv[S_value]
                        if integer(xi)
                        and and_bits(u,K_noclr+K_rtn) then
--DEV newEBP
--puts(1,"does this trigger??\n") --YES!
--?v -- in p.exw, two cases; platform() [result always 2], get_r_proemh() [result always -1].
-- but if things are working correctly, we should not need to do this...
--  (but it fails in t21ret1, opAdd still needs 'real' vars even if value is known...)
--  (but changes to saveAllFunctionReturnValues or whatever might fix that?)
                            setdsDword(-sv[S_Tidx]*4-4, xi)
--if nTyp=S_Const then -- oops, never is
--  if sv[S_ConstChain] then ?9/0 end if    -- 18/10/09
--end if
                        end if
end if
                    end if
                end if
            end if
        end if
    end for
-- gone wrong by here...
--!/**/ printf(1,"data_section initially(3):#%02x%02x%02x%02x%02x%02x%02x%02x\n",data_section[1..8])

    if nslink=T_nslink then
        symtab[T_nslink] = 0
    else
        symtab[nslink][S_Slink] = 0
    end if
--?ridlink
    symtab[ridlink][S_Slink] = 0

--DEV verify ridlink (temp)
--if 01 then
--  vi = T_maintls
--  while vi do
--      sv = symtab[vi]
--      if atom(sv) then
--          puts(1,"***BUG***\n\n")
--          if getc(0) then end if
--          exit
--      end if
----        ?symtab[vi]
--      vi = sv[S_Slink]
--  end while
--end if

    if bind then
--if newEBP then
--      setdivm(#0C,DSvaddr+ImageBase,DWORD)        -- threadstack ptr
--else
        setdivm(#08,DSvaddr+ImageBase,DWORD)        -- threadstack ptr
--end if
        setdivm(vmaxpos,vmax,DWORD)                 -- threadstack size
        d_addr = DSvaddr+DSvsize+ImageBase
        relink()
        flatsym = symtab
        if not listing then
            symtab = {}
        end if
        tt_traverse(r_DumpId,"",-2)                 -- identifiers
        tt_traverse(r_DumpString,"",-1)             -- literal strings
        tt_traverseA(r_DumpAtom)                    -- float constants
--      stripPathInfo()
        dumpPathSet(filepaths)                      -- specials 1
        flatsym[T_pathset] = s_addr
        dumpFileSet(filenames)                      -- specials 2
        flatsym[T_fileset] = s_addr
        tt_traverseQ(r_DumpSequence)                -- sequence constants
        if oopsLen then ?9/0 end if

        if listing then
            -- reconstruct any nested sequences
            while 1 do
                rescan = 0
                tt_traverseQ(r_ReconstructSequence)             -- sequence constants
                if not rescan then exit end if
            end while
        end if

        DumpSignaturesAndLineTables()               -- [S_sig] and [s_ltab] on routine entries

--if not trimST then
--      DumpGinfo()                                 -- [S_value], [S_gInfo] etc 
--end if

        DumpSymTab()                                -- the symtab itself
--if newEBP then
--      setdivm(#08,b_addr,DWORD)                   -- symtab ptr
----DEV cannot find where it sets UserCode...
---- (probably just always in the same place, only ever set by fasm)
--else
        setdivm(#10,b_addr,DWORD)                   -- symtab ptr
--end if
-- gone wrong already
--!/**/ printf(1,"data_section initially(4):#%02x%02x%02x%02x%02x%02x%02x%02x\n",data_section[1..8])
        if d_addr != DSvaddr+DSvsize+ImageBase then ?9/0 end if
        if length(data_section)!=DSvsize then ?9/0 end if
        if not isString(data_section) then ?9/0 end if

--sfn = open("symtab.e","w")
--printf(sfn,"global constant DSVSize=%d\n",DSvsize)
--puts(sfn,"global constant symtab=\n")
--ppEx(symtab,{pp_File,sfn})
--close(sfn)

        setmzpe(#248, DSvsize, DWORD)
        DSrsize = and_bits(DSvsize+FAless1,FAmask)
        setmzpe(#250, DSrsize, DWORD)
        SizeOfImage = and_bits(DSvaddr+DSvsize+SAless1,SAmask)
        setmzpe(#D0, SizeOfImage, DWORD)

        if showfileprogress then
            printf(1,"exe write started at %3.2f, symtab dump took %3.2f seconds...\n",{time()-t,time()-t0})
        end if

        outfile = path&outfile
        firsttime = 1   -- (sleep(1) once only)
        while 1 do
            fn = open(outfile,"wb")
            if fn!=-1 then exit end if
            if firsttime then
                sleep(1)
                firsttime = 0
            else
                puts(1,"Error opening "&outfile&" - retry?")
                if find(wait_key(),"nN") then abort(1) end if
                puts(1,"\n")
            end if
        end while

--
-- opLicence implements the "enforced open source" licencing model (ie
--  totally free for all personal and open source use, registration is
--  required (also free) to ship closed-source apps, and while you can
--  share modified compiler sources, you may not ship pre-built/closed
--  source compiler/interpreter executables, ever..) Fairly obviously,
--  simply removing this call will have catastrophic results, and note
--  that applying licencing to p[w].exe will totally cripple it.
--
if 01 then
--/**/  ltxt = 0
--/**/  if OptLicence then
--/**/      lfn = open(rootpath&"licence.txt","rb")
--/**/      if lfn!=-1 then
--/**/          ltxt = repeat(' ',160)
--/**/          lidx = 0
--/**/          while 1 do
--/**/              while 1 do
--/**/                  -- allow/ignore any whitespace in licence.txt
--/**/                  --  (1x160, 2x80, or 4x40 at a minimum)
--/**/                  lch = getc(lfn)
--/**/                  if lch>='2'
--/**/                  and lch<='Z'
--/**/                  and not find(lch,":;<=>?@OI") then
--/**/                      exit
--/**/                  elsif lch=-1 then
--/**/                      exit
--/**/                  elsif not find(lch," \t\r\n") then
--/**/                      lidx = 999  -- force error next
--/**/                      exit
--/**/                  end if
--/**/              end while
--/**/              if lch=-1 then exit end if
--/**/              lidx += 1
--/**/              if lidx > 160 then exit end if
--/**/              ltxt[lidx] = lch
--/**/          end while
--/**/          if lidx!=160 then
--/**/              ? lidx
--/**/              puts(1,"Error: licence.txt is invalid")
--/**/              if getc(0) then end if
--/**/              abort(0)
--!/**/         else -- temp!
--!/**/             puts(1,"licence.txt ok!\n")
--/**/          end if
--/**/          close(lfn)
--/**/      else
--/**/  -- DEV: write this to a log file somewhere?
--/**/          puts(1,"Warning: "&rootpath&"licence.txt not found\n")
--/**/      end if
--/**/  end if
--!/**/ printf(1,"\ndata_section before:#%02x%02x%02x%02x%02x%02x%02x%02x\n",data_section[1..8])
----/**/    #ilasm{ mov_ecx_mem32,%isVar,0,0,mzpe,          -- mov ecx,[mzpe]
----/**/            mov_edx_mem32,%isVar,0,0,divm,          -- mov edx,[divm]
----/**/            mov_edi_mem32,%isVar,0,0,data_section,  -- mov edi,[data_section]
----!/**/       mov_esi_mem32,%isVar,0,0,ltxt,          -- mov esi,[ltxt]
----/**/            opLoadMem,%esi,ltxt,                    -- mov esi,[ltxt]
----/**/            call_rel32,%isOpCode,0,0,%opLicence}
--/**/  #ilASM{ mov ecx,[mzpe]
--/**/          mov edx,[divm]
--/**/          mov edi,[data_section]
--/**/          mov esi,[ltxt]
--/**/          call %opLicence }
--!/**/ printf(1,"data_section after:#%02x%02x%02x%02x%02x%02x%02x%02x\n",data_section[1..8])
end if
                
                
        puts(fn,mzpe)                           -- MZ and PE headers
        puts(fn,divm)                           -- VM data and code
        puts(fn,resource_section)               -- resource section
        puts(fn,code_section)                   -- application code section
        if CSrsize>CSvsize then
            puts(fn,repeat(0,CSrsize-CSvsize))  -- pad to FileAlign
        end if
        puts(fn,data_section)                   -- application data and symtab
        if DSrsize>DSvsize then
            puts(fn,repeat(0,DSrsize-DSvsize))  -- pad to FileAlign
        end if
        close(fn)
    else -- not bind
-- moved here 07/01/2012:
-- moved up to be with "if debug or listing" 21/09/2013:
--      if some_unresolved_rtnids then
--          -- repopulate the symbol table with var names...
--          relink()
--          tt_traverse(r_ReconstructIds,"",-2)             -- identifiers
--      els
        if not debug and listing!=-1 then   -- 19/8/9
            rbicb = call_back(routine_id("rebuild_callback"))
--          #ilasm{ opLeaMov,%edi,rbicb,                    -- mov ecx,addr rebuild callback (!!)
--                  call_rel32,%isOpCode,0,0,%opRbldIds}    -- save rebuild callback
            #ilASM{ lea edi,[rbicb]
                    call %opRbldIds }   -- save rebuild callback
            rbicb = 0   -- suppress warnings
        end if

        if listing then
            symtab[T_pathset] = -999
            symtab[T_fileset] = -999
        else
            symtab[T_pathset] = filepaths
            symtab[T_fileset] = filenames
        end if
        poke(CSvaddr,code_section)
--      poke(DSvaddr,data_section)
    end if

end procedure


