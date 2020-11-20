--
-- htmlise.e
--
--with trace

function htmlclr(atom c)
-- convert 0x00bbggrr hex values (as used by eg setbkColor) into #RRGGBB (text) for html.
sequence res
--integer tmp
    res = sprintf("#%06x",c)
--  tmp = res[2]
--  res[2] = res[6]
--  res[6] = tmp
--  tmp = res[3]
--  res[3] = res[7]
--  res[7] = tmp
    return res
end function

bool closing = false
sequence r, ct
--integer toClip, fn
procedure addpCode(object x)
--  if toClip then
-- 14/10/2020:
    if not closing
    and find(x,{"</font>","</i>"})
    and r="" then
        ct[$] &= x
    elsif x="</font>"
      and length(r)>=22
      and r[$]='>'
      and r[-22..-9]=`<font color="#` then
--  r = `<font color="#000000">`
--      32109876543210987654321
        r = r[1..-23]
    else
        r &= x
        if r[$]='\n' then
            ct = append(ct,r[1..-2])
            r = ""
        end if
    end if
--  else
--      puts(fn,x)
--  end if
end procedure

procedure publishCode(integer toClipBoard)
-- 28/4/2010
--  toClipBoard of 2 is for BBcode
--  toClipBoard of 3 is for <eucode> inlining, result left in ct
--  toClipBoard of 4 is "", StripHtml'd code is in ct.
--integer fn
--sequence backdir, pcfile
sequence dt, code, oneline, chunk, url
integer chunklen, cstart, font, bold, ital, uline
atom clr
integer wasSelON, offset, ch
--integer fmt
--DEV Ctrl P -> IE; Ctrl Shift P gives options first
--trace(1)
--  toClip = toClipBoard
--  if toClipBoard then
        r = {}
        if toClipBoard!=4 then
            ct = {}
        end if
--  else
--      backdir = initialcurrentdir&`backup\`
--      if atom(dir(backdir)) then
--          if not createDirectory(backdir) then
--              void = proemh(xl("Error"),xl("Unable to create backup directory")&backdir,0)
--              return
--          end if
--      end if
--
--      pcfile = backdir&"pc.htm"
--      fn = open(pcfile,"w")
--      if fn=-1 then
--          void = proemh(xl("Error"),xl("Unable to create ")&pcfile,0)
--          return
--      end if
--  end if
    if selON and selY!=CursorY then
        code = getSelection(SEL_COPY)
        offset = selY
        if CursorY<selY then
            offset = CursorY
        end if
    elsif toClipBoard=4 then
        code = ct
        offset = 0
        ct = {}
    else
        code = filetext[currfile]
        offset = 0
    end if
    dt = date()
--if toClipBoard=2 then
--  addpCode("[pre][color=#000000]")
--elsif toClipBoard>=3 then
    if toClipBoard=4 then
        addpCode("<pre>\n")
    end if
--trace(1)
    addpCode(`<font color="#000000">`)
--else
--  addpCode(sprintf("<html><head><title>%s %02d/%02d/%04d %02d:%02d</title></head>\n",
--                   {filenames[currfile],dt[3],dt[2],dt[1],dt[4],dt[5]}))
----    puts(fn,"<body bgcolor=\"#FFFFFF\" text=\"#000000\">\n<pre>\n")
----    addpCode(sprintf("<body bgcolor=%s text=\"#000000\">\n",{htmlclr(ColourTab[Background])}))
--  addpCode(sprintf("<body bgcolor=\"%s\" text=\"#000000\">\n",{htmlclr(ColourTab[Background])}))
--  addpCode("<pre>\n")
--  addpCode("<font color=\"#000000\">\n")
--  addpCode("<!-- NB: when posting to EUforum, please use <eucode> tags instead -->\n")
--end if
    bold = False
    ital = False
    uline = False
    clr = 0
    wasSelON = selON
    selON = 0
--trace(1)
    for l=1 to length(code) do
        oneline = ExpandTabs(code[l])
        chunk = syntaxColour(oneline,l+offset,0,0)  -- ={length,txt_clr,bck_clr,attr}*
        cstart = 1
        for i=1 to chunkMax by 4 do
            chunklen = chunk[i]
            font = chunk[i+3]   -- 1=bold, 2=Italic, 3=Bold+Italic, 4=Normal, 5=url
            if font!=1 and font!=3 then
                if bold then
                    addpCode("</b>")
                    bold = False
                end if
            end if
            if font!=2 and font!=3 then
                if ital then
                    addpCode("</i>")
                    ital = False
                end if
            end if
            if font!=5 then
                if uline then
                    addpCode("</u>")
                    if uline=2 then         -- 17/02/08
                        addpCode("</a>")    -- 08/12/07
                    end if
                    uline = False
                end if
            end if
            if clr!=chunk[i+1] then
--if toClipBoard=2 then
--              addpCode("[/color]")
--else
                addpCode("</font>")
--end if
            end if
            if clr!=chunk[i+1] then
                clr = chunk[i+1]
--if toClipBoard=2 then
--              addpCode(sprintf("[color=%s]",{htmlclr(clr)}))
--else
--              addpCode(sprintf("<font color=%s>",{htmlclr(clr)}))
                addpCode(sprintf(`<font color="%s">`,{htmlclr(clr)}))
--end if
            end if
            if font=5 then
                if not uline then
                    uline = true
                    url = oneline[cstart..cstart+chunklen-1]
                    if match("edit:",url)!=1 then           -- 17/02/08
                        while url[length(url)]='\n' do      -- ""
                            url = url[1..length(url)-1]
                        end while
                        addpCode(`<a href="`&url&`">`)
                        uline = 2
                    end if
                    addpCode("<u>")
                end if
            end if
            if font=2 or font=3 then
                if not ital then
                    addpCode("<i>")
                    ital = true
                end if
            end if
            if font=1 or font=3 then
                if not bold then
                    addpCode("<b>")
                    bold = true
                end if
            end if

--          addpCode(oneline[cstart..cstart+chunklen-1])
            for cx=cstart to cstart+chunklen-1 do
                ch = oneline[cx]
                if ch='\n' then
                    if uline then               -- 17/02/08
                        addpCode("</u>")        -- ""
                        if uline=2 then         -- ""
                            addpCode("</a>")    -- ""
                        end if                  -- ""
                        uline = False           -- ""
                    end if                      -- ""
--                  if toClip then
                        ct = append(ct,r[1..length(r)-1])
                        r = {}
--                  else
--                      puts(fn,ch)
--                  end if
                elsif ch='&' and toClipBoard!=2 then addpCode("&amp;")
                elsif ch='<' and toClipBoard!=2 then addpCode("&lt;")
                elsif ch='>' and toClipBoard!=2 then addpCode("&gt;")
                elsif ch='~' and toClipBoard!=2 then addpCode("&tilde;")
                else
--                  if toClip then
                        r &= ch
--                  else
--                      puts(fn,ch)
--                  end if
                end if
            end for
            cstart += chunklen
        end for
        if uline then               -- 17/02/08
            addpCode("</u>")        -- ""
            if uline=2 then         -- ""
                addpCode("</a>")    -- ""
            end if                  -- ""
            uline = False           -- ""
        end if                      -- ""
        addpCode("\n")
        chunk = {}
    end for
    selON=wasSelON
closing = true
    if bold then
        addpCode("</b>")
    end if
    if ital then
        addpCode("</i>")
    end if
    if uline then
        addpCode("</u>")
    end if
closing = false
--if toClipBoard=2 then
--  addpCode("[/color][/pre]\n")
--elsif toClipBoard>=3 then
--trace(1)
--  addpCode("</font>\n")
--  oneline = ct[length(ct)]
--  oneline = oneline[1..length(oneline)-1]&"</font>\n"
--  ct[length(ct)] = oneline
--  ct[length(ct)-1] &= "</font>"
if ct[$]!="" then ?9/0 end if -- sanity check added 14/10/2020
    ct[length(ct)-1] &= r&"</font>"
--  r = {}
    r = ""
    if toClipBoard=4 then
        addpCode("</pre>\n")
    end if
--  return  -- leave in ct
--else
--  addpCode("</font></pre></body></html>\n")
--end if
--  if toClip then
--      if unicodefile[currfile]=0 then
--          fmt = CF_TEXT
--      else
--          fmt = CF_UNICODETEXT
--      end if
--      void = copyTextToClipboard(ct,fmt)
--      ct = {}
--  else
--      close(fn)
----DEV: (works, might want an option to do this instead?)
----    void = openFile(pcfile,1,0)
--      void = shellExecute(NULL, "open", "pc.htm", NULL, initialcurrentdir&`backup\`, SW_SHOWNORMAL)
--      if void = 31 then
--          void = messageBox(xl("Error"),"ERROR_GEN_FAILURE",0)
--      end if
--  end if
end procedure

--global 
function lineify(sequence clip)
integer len, start, p
sequence res
    len = length(clip)
    if len=0 then
        return clip
    end if
    start = 1
    p = 1
    res = {}
    while 1 do
        if clip[p]='\r' then
            res = append(res, clip[start..p-1])
--          p += 2      -- or /maybe/ min(p+2,len+1)
--1/02/10:
            p += 1
            if p<=len and clip[p]='\n' then
                p += 1
            end if
            start = p
        else
            p += 1
        end if
        if p>len then
            res = append(res, clip[start..p-1])
            exit
        end if
    end while
    return res
end function

--constant ampdeadset = {"&gt;","&lt;","&nbsp;","&amp;","<p>","<P>","<BR>"},
--       adrep      = { ">",    "<",   " ",     "&",    "",   "",   ""}

--with trace
--constant newSH = 1    -- DEV just an idea...
integer cfound

constant kampers = {"&gt;","&lt;","&nbsp;","&amp;","&tilde;"},
--       kamprep = {{'>',0,0,0},{'<',0,0,0},{' ',0,0,0,0,0},{'&',0,0,0,0}}
         kamprep = {">",   "<",   " ",     "&",    "~"}

function matchword(sequence code, integer os, integer oe, integer cs)
    for i=os to oe do
        if code[i]!=code[cs+i-os] then return 0 end if
    end for
    return 1
end function
function closedOK(sequence code, integer os, integer oe)
    if cfound then return 1 end if
    -- these don't need a closing tag:
    return find(lower(code[os..oe]),{"br","meta","hr","p","img","li","tr","td"})
end function

sequence hout
         hout = repeat(-1,256)

for i=0 to 9 do
    hout[i+48] = i  -- '0'..'9' => 0..9
end for
for i=10 to 15 do
    hout[i+55] = i  -- 'A'..'F' => 10..15
    hout[i+87] = i  -- 'a'..'f' => 10..15
end for

procedure StripHtml()--integer toClipBoard)
object clip
sequence code
integer k, k2
--, k3, k4, ko,ke
, lc
--, done
integer ci1, ci2
--sequence endtag

--atom hGlobal,pData
--integer fmt

    if selON and selY!=CursorY then
        clip = getSelection(SEL_COPY)
    else
        clip = filetext[currfile]
    end if

--  if unicodefile[currfile]=0 then
--      fmt = CF_TEXT
--  else
--      fmt = CF_UNICODETEXT
--  end if

    if sequence(clip) and length(clip)>=1 then
--      if newSH then
            k = ExpLength(clip[1])
            for i=2 to length(clip) do
                k += 2+ExpLength(clip[i])
            end for
            code = repeat(0,k)
            k = ExpLength(clip[1])
            code[1..k] = ExpandTabs(clip[1])
            for i=2 to length(clip) do
                k += 2
                code[k-1] = '\r'
                code[k] = '\n'
                k2 = ExpLength(clip[i])
                code[k+1..k+k2] = ExpandTabs(clip[i])
                k += k2
            end for
            lc = length(code)
            if k!=lc then ?9/0 end if
            for i=1 to lc-3 do
                if code[i]='<' then
                    if code[i+1]='!'
                    and code[i+2]='-'
                    and code[i+3]='-' then
                        for j=i+4 to length(code) do
                            if code[j]='>'
                            and code[j-1]='-'
                            and code[j-2]='-' then
                                code[i..j] = 0
                                exit
                            end if
                        end for
                    else
                        for j=i+1 to length(code) do
                            if find(code[j],"= >") then
                                cfound = 0
                                for ej=j+1 to length(code)-j+i-1 do
                                    if code[ej]='<'
                                    and code[ej+1]='/'
                                    and code[ej+j-i+1]='>' then
--trace(1)
                                        if matchword(code,i+1,j-1,ej+2) then
                                            code[ej..ej+j-i+1] = 0
                                            cfound = 1
                                            exit
                                        end if
                                    end if
                                end for
                                if closedOK(code,i+1,j-1) then
                                    -- get rid of all eg <font size=2>
                                    for ej=j to length(code) do
                                        if code[ej]='>' then
                                            code[i..ej] = 0
                                            exit
                                        end if
                                    end for
                                end if
                                exit
                            end if
                        end for
                    end if
                elsif code[i]='=' then
                    ci1 = code[i+1]
                    ci2 = code[i+2]
--DEV.. (24/09/2013)
if ci1!=0 and ci2!=0 then
                    if hout[ci1]!=-1 
                    and hout[ci2]!=-1 then
                        code[i] = hout[ci1]*16+hout[ci2]
                        code[i+1..i+2] = 0
                    elsif ci1='\r'
                      and ci2='\n' then
                        code[i..i+2] = 0
                    end if
end if
                end if
            end for
            for i=1 to length(kampers) do
                while 1 do
                    k = match(kampers[i],code)
                    if k=0 then exit end if
                    code[k..k+length(kampers[i])-1] = kamprep[i]
                end while
            end for
--/*
            k=find(0,code)
            if k then
                for i=k+1 to length(code) do
                    k2 = code[i]
                    if k2!=0 then
                        code[k] = k2
                        k += 1
                    end if
                end for
                code = code[1..k-1]
            end if
--*/
--          if toClipBoard then
--              void = copyTextToClipboard({code},fmt)
--          else
                ct = lineify(code)
--          end if
--      else
----DEAD CODE...
--          code = ExpandTabs(clip[1])
--          for i=2 to length(clip) do
--              code &= "\r\n"&ExpandTabs(clip[i])
--          end for
--          clip = {}
--          k2 = 1
--          while 1 do
--              k = match("<!--",code[k2..length(code)])    -- comment start
--              if k=0 then exit end if
--              k += k2-1
--              k2 = match("-->",code[k+4..length(code)]) -- comment end
--              if k2=0 then exit end if
--              k2 += k+3
----        for i=k+4 to k2-1 do                        -- specific spam test
----        ch = code[i]
----        if (ch>='0' and ch<='9')
----        or (ch>='A' and ch<='Z')
----        or (ch>='a' and ch<='z') then
----            if i=k2-1 then
--              code = code[1..k-1]&code[k2+3..length(code)]
--              k2 = k
----            exit
----            end if
----        else
----            k2 = k+1
----            exit
----        end if
----        end for
--          end while
--
--          --
--          -- chop matching, eg <pre>..</pre>
--          --
--          done = 1
--          ko = 1      -- opening '<'
--          ke = 1      -- ending '>', on the opening doobrie
--          while 1 do
--              ko=find('<',code[done..length(code)])
--              if ko=0 then exit end if
--              ke=find('>',code[done+ko..length(code)])
--              if ke=0 then exit end if
--              --
--              -- ok, now we have a tag
--              -- find the closing tag to look for
--              --
--              k3=find(' ',code[done+ko..done+ko+ke-2])-1
--              if k3=-1 then
--                  k3 = ke-1
--              end if
--              endtag = "</"&code[done+ko..done+ko+k3-1]&'>'
--              if not find(endtag,{"</br>","</meta>","</hr>","</p>","</img>",      -- added 8/6/7
--                                  "</BR>","</META>","</HR>","</P>","</IMG>"}) then
--                  --
--                  -- now find the equivalent closing html:
--                  -- eg <pre>... </pre>
--                  --
--                  k4 = match(endtag,code[done+ko+ke-1..length(code)])
--                  if k4 then
--                      code = code[1..done-1+ko-1]&
--                             code[done+ko+ke..done+ko+ke+k4-2]&
--                             code[done+ko+ke+k4+k3+2..length(code)]
----                    done = 1    -- far too hard; start again!
--                  elsif find(endtag,{"</li>"}) then                   -- added 8/6/7
--                      code = code[1..done-1+ko-1]&code[done+ko+ke..length(code)]
--                  else
--                      done += ko+ke
--                  end if
--              else
--                  code = code[1..done-1+ko-1]&code[done+ko+ke..length(code)]
--              end if
--          end while
--          for i=1 to length(ampdeadset) do
--              while 1 do
--                  k = match(ampdeadset[i],code)
--                  if k=0 then exit end if
--                  code = code[1..k-1]&adrep[i]&code[k+length(ampdeadset[i])..length(code)]
--              end while
--          end for
----/*
----            -- instead of eg:
----            --  void = copyTextToClipboard(clip)
----            -- since code already contains embedded \r\n, use direct API calls:
----            if c_func(xOpenClipboard,{c_func(xGetActiveWindow, {})}) then
----                if c_func(xEmptyClipboard,{}) then
----                    hGlobal = c_func(xGlobalAlloc,{GMEM_CLIPBOARD,length(code)})
----                    if hGlobal then
----                        pData = c_func(xGlobalLock,{hGlobal})
----                        if not pData then
----                            void = c_func(xGlobalFree,{hGlobal})
----                        else
----                            poke(pData,code)
----                            void = c_func(xGlobalUnlock,{hGlobal})
----                            void = c_func(xSetClipboardData,{CF_TEXT,hGlobal})
----                            c_proc(xCloseClipboard,{})
----                            --Note that we should NOT GlobalFree hGlobal, since the clipboard now owns it.
----                        end if
----                    end if
----                end if
----            end if
----*/
--          setClipboardText(code, fmt)
--      end if

    end if
end procedure

integer hEstate
procedure hemismatch(integer lineno, integer fileno)
    changeTo(fileno,-1)
    jumpTo(0, lineno-1)
--  void = messageBox("Error",
--                    sprintf("eucode/pre tag mismatch [line %d]",lineno),
--                    0)
    IupMessage("Error","eucode/pre tag mismatch [line %d]",{lineno})
    hEstate = -1
end procedure

global procedure htmliseEucode()
--
-- Processes all open .htm[l]/php files in the current directory.
-- It looks for:
--      <!--eucode>
--      <plaintext>
--      </eucode-->
--      <pre>
--      <colourised text>
--      </pre>
-- Or:
--      <eucode>
--      <colourised text>
--      </eucode>
-- In the first case, it deletes <colourised text> and replaces it with 
--  <plaintext> as passed through publishCode(). In the second case, it
--  copies <colourised text>, and passes it through StripHtml(0) before 
--  publishCode(), and then replaces it. While the second form may look
--  easier, colourising it makes the code unreadable.
--
-- Note: eucode and pre lines should have no whitespace or comments.
--
-- A basic sanity check is made that each file has the right number of
--  pairs of each of the marker lines, and they are in the right order.
--
sequence thisdir, txt, oneline, todos, todo, wasFileNameI
object wasbCfwd
--,d1,d2
integer eustart, euend, prestart, preend
--integer fmt
--trace(1)
integer wascurrfile
    wascurrfile = currfile
    if currfile then
        thisdir = filepaths[currfile]
        for i=1 to length(filenames) do
            if equal(thisdir,filepaths[i])
--          and find(getFileExtension(filenames[i]),{"htm","html","php"}) then
            and find(get_file_extension(filenames[i]),{"htm","html","php"}) then
                txt = filetext[i]
                hEstate = 4
                todos = {}
                for j=length(txt) to 1 by -1 do
                    oneline = txt[j]
                    if equal(oneline,"</pre>") then
                        if hEstate!=4 then hemismatch(j,i) exit end if
                        preend = j-1
                        hEstate = 3
                    elsif equal(oneline,"<pre>") then
                        if hEstate!=3 then hemismatch(j,i) exit end if
                        prestart = j+1
                        hEstate = 4
                        if j>1 then
                            oneline = txt[j-1]
                            if equal(oneline,"</eucode-->") then
                                hEstate = 2
                            end if
                        end if
                    elsif equal(oneline,"</eucode-->") then
                        if hEstate!=2 then hemismatch(j,i) exit end if
                        euend = j-1
                        hEstate = 1
                    elsif equal(oneline,"<!--eucode>") then
                        if hEstate!=1 then hemismatch(j,i) exit end if
                        eustart = j+1
                        todos = append(todos,{eustart,euend,prestart,preend})
                        hEstate = 4
                    elsif equal(oneline,"</eucode>") then
                        if hEstate!=4 then hemismatch(j,i) exit end if
                        euend = j-1
                        hEstate = 5
                    elsif equal(oneline,"<eucode>") then
                        if hEstate!=5 then hemismatch(j,i) exit end if
                        eustart = j+1
                        todos = append(todos,{eustart,euend})--,eustart,euend})
                        hEstate = 4
                    end if
                end for
                if hEstate=-1 then exit end if  -- error occurred
                if hEstate!=4 then
                    if hEstate=3 then
                        hemismatch(preend+1,i)
                    else
                        hemismatch(1,i)
                    end if
                    exit
                end if

                if length(todos) then
--trace(1)
                    changeTo(i,-1)
                    wasbCfwd = bCfwd
                    bCfwd = {}
                    wasFileNameI = filenames[i]
                    filenames[i] = "html.exw"
                    changeTo(i,-1)  -- (set Phix syntax colouring)
?newSyntax
                    clearSelection()
                    for j=1 to length(todos) do
                        todo = todos[j]
                        eustart = todo[1]
                        euend = todo[2]
                        CursorX = 0
                        CursorY = eustart-1     -- 0-based
                        selX = 0
                        selY = euend            -- "", but to end of line
--                      forceCursorOnscreen()
                        if selY!=CursorY then
                            selON = 1
                            if length(todo)=2 then  -- <eucode>...</eucode> form
--                              StripHtml(0)
                                StripHtml()
                                publishCode(4)
                                prestart = eustart
                                preend = euend
                            else                    -- <!--eucode>..</eucode--><pre>..</pre> form
                                publishCode(3)
                                prestart = todo[3]
                                preend = todo[4]
                            end if
                            selON = 0       -- also used as flag, if we need to paste
                            if length(ct)-1!=(preend-prestart+1) then
--trace(1)
                                selON = 1
                            else
                                for k=1 to length(ct)-1 do
                                    if not equal(ct[k],ExpandTabs(txt[prestart+k-1])) then
--trace(1)
--d1 = ct[k]
--d2 = txt[prestart+k-1]
                                        selON = 1
                                        exit
                                    end if
                                end for
                            end if
                            if selON then
                                CursorY = prestart-1    -- 0-based
                                selY = preend           -- "", but to end of line
--                              if unicodefile[currfile]=0 then
--                                  fmt = CF_TEXT
--                              else
--                                  fmt = CF_UNICODETEXT
--                              end if
--                              if copyTextToClipboard(ct,fmt) then
--                              IupSetAttribute(clipboard, "TEXT", NULL)    -- (as per docs)
--                              IupSetAttribute(clipboard, "TEXT", ct)

--                                  Paste()
--                              end if
                                Paste(ct)
                            end if
                        end if
                    end for
                    bCfwd = wasbCfwd
                    filenames[i] = wasFileNameI
                    changeTo(i,-1)  -- (reset html syntax colouring)
                end if
            end if
        end for
        changeTo(wascurrfile,-1)
    end if
end procedure
--constant r_htmliseEucode=routine_id("htmliseEucode")

