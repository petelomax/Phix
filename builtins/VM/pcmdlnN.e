--
-- pcmdln.e
-- ========
--
-- Phix implementation of command_line().
--
-- This code is automatically included when needed. It should /NOT/ be
--  explicitly included to avoid incompatibility issues with RDS Eu.
--
--  <aside>
--  IE: both p.exe and exw.exe may prompt for a filename, say "test",
--  and pretend the original command line was "p test" or "exw test".
--  This implements the fetch of "test" from p.exe's prompt, but of
--  course cannot do the same for exw.exe's prompt - for that you
--  must use exw.exe's builtin implementation of command_line().
--  </aside>
--
--      Command:                    Required result:
--  1)  p.exe test.exw [opt]        p.exe test.exw [opt]
--  2)  p.exe                       p.exe {from symtab}
--  3)  test.exe [opt]              test.exe test.exe [opt]
--
-- This routine uses standard windows API calls GetCommandLine and SearchPath 
--  to get the command line string and expand pathnames respectively.
--  Note that "p -c xxx" and "exw p xxx" are equivalent to case 3, plus in
--  case 2, where the source is prompted for, the compiler stores extra info
--  in the symtab: The filenames are stored in symtab[T_fileset], in
--  {{pathno,filename}} format, the first pair being the main file, and if
--  longer than 2 elements, the remainder are options entered at the prompt.
--  The filepaths are stored in symtab[T_pathset], indexed by the pathno
--  field from the filenames set. For example, "p t4 -metric" might be:
--
--      symtab[T_fileset]: {{1,"t4.exw","-metric"},
--                          {2,"pcmdln.e"}}
--      symtab[T_pathset]: {`C:\test\`,
--                          `C:\Program Files\Phix\builtins\`}
--
--  ==> {<SearchPath>"p.exe",`C:\test\t4.exw`,"-metric"}
--
--  (where "-metric" is some random application's command line option,
--   presumably p4.exw contains code similar to that for eg "-nodiag"
--   in pmain.e/processCommandLine().)
--  Additionally the symtab[T_cmdlnflg] flag for command_line() is:
--   1 when bound (case 3), or
--   2 when interpreting, so 
--      a) obtain filename from symtab[T_pathset/T_fileset],
--      b) obtain parameters from symtab[T_fileset][1][3..$]
--  A shared reference to the symtab can be obtained via opGetST (see below).
--
--  Note that eg "p -c t4 -metric" where t4 is "?command_line()" simply displays
--  {`C:\Program Files\Phix\t4.exe`,`C:\Program Files\Phix\t4.exe`,"-metric"}
--  which should be no surprise as there is a system_wait() involved.
--  likewise "p -cp xx" reserves the right to completely ignore the "xx".
--    (update: "p -cp xx" is now treated as a fatal error)
--  Reasonable results should be obtained in the "p p test -metric" and
--  "p p p test -metric" etc cases, both of which simply yield:
--      {<SearchPath>"p.exe",<wherever>"test.exw","-metric"}
--  This is quite deliberate, otherwise test.exw would behave differently
--  when run using "p test" and "p p test", which just hampers testing,
--  generally causes confusion, and gains nothing.
--

--!/**/without debug -- (keeps ex.err clean, remove to debug!)

--NB ensure these match pglobals.e:
--   [avoid any temptation to include pglobals.e and/or create a new file
--    which is included by pglobals.e and here; we do not want to add any 
--    globals which are visible to user code.]

--/**/  constant T_pathset  = 16,
--/**/           T_fileset  = 17,
--/**/           T_cmdlnflg = 19


integer init init = 0
atom xKernel32, xGetCommandLine, xSearchPath,
--   xGetLastError, 
     dotEXE, pFilePart
--include builtins\dll.e
--include builtins\machine.e

--p2js:
--include builtins\pcase.e as pcase
--include builtins\peekstr.e as pstr
include builtins\pcase.e
include builtins\peekstr.e

--/**/-- not strictly necessary, but reduces opCallOnce/fwd calls/onDeclaration
--!/!*!*!/!include builtins\machine.e -- allocate_string()
include builtins\pgetpath.e     -- get_proper_path()

include builtins/VM/pStack.e    -- :%opGetST


function expandPath(sequence filepath)
--
-- expand filepath to full path/extension (platform()=WINDOWS only)
--
atom xPath, xRawStr, xBuffer
sequence tmp
integer l
--DEV cleanme:
--puts(1,"expandPath:\n")
--pp(filepath)

    xPath = NULL
    tmp = filepath
    for j=length(tmp) to 1 by -1 do
        if find(tmp[j],`\/`) then
            if j>2 then
--/*
                if tmp[2]=':' then
                    return tmp
                elsif match("..",tmp)=1 then
                    -- xSearchPath returns gibberish in such cases...
                    --  (as sadly true on winXP as it was on win98)
--DEV for removal of `\..\` see eg ptok.e and/or edita/eaxlate.e
--                  return current_dir()&`\`&tmp
--                  return get_proper_path("tmp")
                    return get_proper_path(tmp)
                end if
--*/
                if tmp[2]=':' or match("..",tmp) then
--20/2/14:
--                  tmp = get_proper_path(tmp)
--                  exit
--                  if length(tmp) then exit end if
--                  tmp = filepath
                    tmp = get_proper_path(tmp[1..j])&tmp[j+1..length(tmp)]
                    exit
                end if
            end if
            xPath = allocate_string(tmp[1..j])
            l = j+1
            tmp = tmp[l..length(tmp)]
            exit
        end if
    end for
--puts(1,"tmp(7):\n")
--pp(tmp)

    xRawStr = allocate_string(tmp)
    l = c_func(xSearchPath,{xPath,xRawStr,dotEXE,0,0,pFilePart})
    if l=0 then
--?0
--      l = c_func(xGetLastError,{})
--      printf(1,"xSearchPath error %d\n",l)
--      ?9/0
--      return filepath
--29/7/15:
        free(xRawStr)
        if xPath!=NULL then free(xPath) end if
        return get_proper_path(filepath)
    end if
    xBuffer = allocate(l)
    l = c_func(xSearchPath,{xPath,xRawStr,dotEXE,l,xBuffer,pFilePart})
    tmp = peek({xBuffer,l})
--puts(1,"tmp(8):\n")
--pp(tmp)
--?l
    free(xBuffer)
    free(xRawStr)
    if xPath!=NULL then free(xPath) end if
    if l>2 and tmp[2]!=':' then
        -- sometimes it returns utter gibberish...
--      return filepath
--?{2}
        return get_proper_path(filepath)
    end if
--  return tmp
--?{0}
    return get_proper_path(tmp)
end function

global function command_line()
--/**/object symtab,tmp2
sequence plainstr,res
object tmp
--object current
integer chfirst, chlast, l, lr
integer pArg4,W,N

    if platform()=WINDOWS then
        if not init then
--DEV requires locking as per pprntf.e:
            -- added 25/11/16:
            enter_cs()
            xKernel32 = open_dll("kernel32")
            if xKernel32=0 then xKernel32 = 9/0 end if
--#without reformat
            xGetCommandLine = define_c_func(xKernel32,"GetCommandLineA",
                {},
                C_POINTER)  -- LPTSTR
--          xGetLastError = define_c_func(xKernel32, "GetLastError",
--              {},
--              C_INT)      -- DWORD
            xSearchPath = define_c_func(xKernel32,"SearchPathA",
                {C_POINTER, --  LPCTSTR  lpszPath,  // address of search path 
                 C_POINTER, --  LPCTSTR  lpszFile,  // address of filename 
                 C_POINTER, --  LPCTSTR  lpszExtension, // address of extension 
                 C_INT,      -- DWORD  cchReturnBuffer, // size, in characters, of buffer 
                 C_POINTER, --  LPTSTR  lpszReturnBuffer,   // address of buffer for found filename 
                 C_POINTER},--  LPTSTR  *plpszFilePart  // address of pointer to file component 
                C_INT)      -- DWORD (required) length in buffer
--#with reformat
            if xGetCommandLine=-1 then xGetCommandLine = 9/0 end if
--          if xGetLastError = -1 then xGetLastError = 9/0 end if
            if xSearchPath=-1 then xSearchPath = 9/0 end if
--          dotEXE = allocate_string(".exe")
--          pFilePart = allocate(4)
            init = 1
            leave_cs()
        end if

        dotEXE = allocate_string(".exe")
        pFilePart = allocate(4)
        --
        -- First, get the basic command string:
        --
--      plainstr = pstr:peek_string(c_func(xGetCommandLine,{}))
        plainstr = peek_string(c_func(xGetCommandLine,{}))
--pp(plainstr)
        --
        -- Then parse it into chunks.
        --
        res = {}
        chfirst = 1
        chlast = length(plainstr)
        while find(plainstr[chlast]," \t") do
            chlast -= 1
        end while

        while chfirst<=chlast do
            if plainstr[chfirst]='"' then
                for i=chfirst+1 to chlast+1 do
                    if i>chlast -- closing quote not found, assume one at end...
                    or plainstr[i]='"' then
                        chfirst += 1
                        l = i-1
                        tmp = plainstr[chfirst..l]
                        if length(res)=0 then
                            tmp = get_proper_path(tmp)
                        end if
                        res = append(res,tmp)
                        chfirst = i+1
                        while chfirst<=chlast and find(plainstr[chfirst]," \t") do
                            chfirst += 1
                        end while
                        exit
                    end if
                end for
            else
                tmp = {}
                for i=chfirst to chlast do
                    if i=chlast then
                        tmp = plainstr[chfirst..chlast]
                    elsif find(plainstr[i]," \t") then
                        tmp = plainstr[chfirst..i-1]
                    end if
                    if length(tmp) then
-- 24/4/10:
--                      if length(res)=0 and (length(tmp)<=2 or tmp[2]!=':') then
--                          tmp = expandPath(tmp)
----puts(1,"==> ")
----pp(tmp)
--                      end if
--puts(1,"tmp(2):\n")
--pp(tmp)
                        if length(res)=0 then
                            if length(tmp)<=2 or tmp[2]!=':' then
                                tmp = expandPath(tmp)
--puts(1,"tmp(3):\n")
--pp(tmp)
                            else
                                tmp = get_proper_path(tmp)
--puts(1,"tmp(4):\n")
--pp(tmp)
                            end if
                        end if
--puts(1,"tmp:\n")
--pp(tmp)
                        res = append(res,tmp)
--puts(1,"res(2):\n")
--pp(res)
                        chfirst = i+1
                        while chfirst<=chlast and find(plainstr[chfirst]," \t") do
                            chfirst += 1
                        end while
                        exit
                    end if
                    if i=chlast then chlast = 9/0 end if
                end for
            end if
        end while
--puts(1,"res:\n")
--pp(res)
        free(dotEXE)
        free(pFilePart)

    elsif platform()=LINUX then
        -- see VM\pStack.e, pArg (probably needs a tweak (ie save/restore) when interpreting)
        #ilASM{
            [ELF32]
                call :%opGetArgELF
                mov [pArg4],eax
            [ELF64]
                call :%opGetArgELF
                mov [pArg4],rax
            []
              }
--DEV/SUG pArg4 *= 4, {pArg += W}
        W = machine_word()
        N = peekNS(pArg4*4+W,W,0)
--?N
--if N=0 then ?9/0 end if
        res = {}
        for i=1 to N do
            res = append(res,peek_string(peekNS(pArg4*4+i*W+W,W,0)))
        end for
        string r1 = res[1]
--      if not file_exists(r1) then
        if get_file_type(r1)!=FILETYPE_FILE then
            sequence s = split(getenv("PATH"),':')
            for i=1 to length(s) do
                string sir = join_path({s[i],r1})
                if get_file_type(sir)=FILETYPE_FILE then
--                  if r1=res[2] then
--                      res[2] = sir
--                  end if
                    res[1] = sir
                    exit
                end if 
            end for
        end if
        if length(res)>=2 then
            string r2 = res[2]
            if get_file_type(r2)!=FILETYPE_FILE then
--DEV initialcurrentdir??
                string cdr = join_path({current_dir(),r2})
                if get_file_type(cdr)=FILETYPE_FILE then
                    res[2] = cdr
                end if
            end if
        end if
    end if

    --
    -- If the first entry is ex[u|w[c]][.exe] (ie sources of Phix being
    --  run on RDS Eu) then trash it.
    --
--  plainstr = pcase:lower(res[1])
    plainstr = lower(res[1])
--puts(1,"plainstr:\n")
--pp(plainstr)

    l = length(plainstr)
    if l>4 and equal(plainstr[l-3..l],".exe") then
        l -= 4
    end if
    if (l>=3 and equal(plainstr[l-2..l],"exw"))
    or (l>=3 and equal(plainstr[l-2..l],"exu")) then
        l -= 3
    elsif (l>=4 and equal(plainstr[l-2..l],"exwc")) then
        l -= 4
    elsif (l>=2 and equal(plainstr[l-2..l],"ex")) then
        l -= 2
    end if

    lr = length(res)
    if l=0 or find(plainstr[l],"\\/ \t\"\'") then
        -- ie <path> is null or ends in fwd/backslash/space/tab/dquote/squote,
        --           eg C:\Euphoria\bin\exw.exe but not say C:\flex\flexw.exe.
        res = res[2..lr]            -- then delete the first element completely,
        lr -= 1                     -- eg {"C:\Euphoria\exw","test.exw"} ==> {"test.exw"}
    end if

--pp(res)

    l = 0
    --
    -- Grab a copy of the symtab, where prompted-for filename etc has been stashed.
    --
--  current = 1 -- get current/"live" symtab; callstack not rqd
                -- (pdiag.e wants symtab at error point, which is oft not the
                --  one pdiag.e is actually executing in, whereas here we are
                --  after the one currently running, hence this flag)
--/**/  enter_cs()
--/**/  #ilASM{
--/**/      [32]
--/**/          lea edi,[symtab]
--/**/  --      lea esi,[current]       -- flag/crashmsg (DEV newEmit/X64)
--/**/  --      xor ecx,ecx             -- unused
--/**/          call :%opGetST          -- [edi]=symtab
--/**/      [64]
--/**/          lea rdi,[symtab]
--/**/          call :%opGetST          -- [rdi]=symtab
--/**/      []
--/**/        }
--/**/  l = symtab[T_cmdlnflg]
--/**/  --
--/**/  -- symtab[T_cmdlnflg] is set by compiler:
--/**/  --   initially 0
--/**/  --    bit #01 if bound (a Phix .exe)
--/**/  --    bit #02 if interpreting (so symtab[T_pathset & T_fileset] available/meaningful)
--/**/  --    bit #04 if -batch (and -c or -d) specified on the command line
--/**/
--!/**/ if l=2 then                                 -- interpreter mode:
--/**/  if and_bits(l,2) then                       -- interpreter mode:
--/**/      if l!=2 then ?9/0 end if                -- (sanity check)
--/**/      tmp = symtab[T_fileset][1]              -- ie filenames[1]
--/**/      l = tmp[1]
--/**/      plainstr = symtab[T_pathset][l]         -- ie filepaths[l]
--/**/      tmp2 = plainstr&tmp[2]
--/**/      if lr<2 then
--/**/          res = append(res,tmp2)  -- eg {"p.exe"} ==> {"p.exe","test.exw"}
--/**/      else
--/**/          res[2] = tmp2           -- eg {"p","fred.exw"} ==> {"p",`C:\projects\fred.exw`}
--/**/          if lr>2 then
--/**/              res = res[1..2]     -- discard remainder/only keep res[1]
--/**/          end if
--/**/      end if
--/**/      l = length(tmp)
--/**/      if l>2 then
--/**/          res &= tmp[3..l]    -- any additional options from symtab[T_fileset][1][3..$]
--/**/      end if
--!/**/ elsif l=1 then                      -- pre-compiled (by Phix) executable running:
--/**/  elsif and_bits(l,1) then            -- pre-compiled (by Phix) executable running:
--/**/      res = prepend(res,res[1])       --   eg {"t.exe",..} ==> {"t.exe","t.exe",..}
--/**/  end if
--/**/  tmp2 = 0
--/**/  tmp = 0
--/**/  symtab = 0
--/**/  leave_cs()

--/* Not Phix:
    if lr=1 then
        res = prepend(res,res[1])           -- eg {"t.exe"} ==> {"t.exe","t.exe"}
    end if
--*/
    return res
end function
