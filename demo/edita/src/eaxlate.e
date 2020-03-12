--
-- eaxlate.e
--
without trace
--with trace

-- Load the appropriate elng_XXX.txt file, depending on the locale setting
-- (Control panel, regional settings)
--
-- The file elng_ENG.txt should be considered the master set (and will change
-- with almost every release). Note that elng_ENG.txt is not optional, and 
-- although it may seem wasteful to translate English into English, the ~10K 
-- table is insignificant in practice. (Actually, Edita will run fine 
-- wihout this file, but a few accelerator keys vanish.)
-- The Tools/Translation Assistant option performs a custom compare of
-- elng_ENG.txt with a specified language file, and F4 can be used to jump
-- to the next difference. Be warned there are a few glitches in this
-- function, and Edita needs to be reloaded for the changes to take effect.
--
-- For new languages, see lang/locales.txt
--
-- If you edit and put an error (eg missing ") in an elng_XXX.txt file, you 
-- should get warnings at startup but still be able to edit it.
-- Send me a new/updated elng_XXX.txt and I'll add it to the next release.
--
-- Occasionally, you may find something on the screen, let's say "Converse",
-- but it wasn't in elng_ENG.txt and no warning was given. In that case some
-- line of code probably reads create(Label,"Converse"... when it should be
-- create(Label,xl("Converse")... Let me know & I'll fix it. Likewise I may
-- not have left sufficient space for a longer foreign word, ditto.
--
-- Note that not /everything/ should be translated, especially eg
-- indications of programming errors and some config errors (and indeed
-- the translation dialogue where the user must be bilingual anyway).
-- Obviously if, for example, "overflow on line 345" is replaced with 
-- "gerstaffel droff 345" then I won't be able to provide support, so
-- only translate user messages, not support messages.

--/* 4.0.2
include ..\..\builtins\machine.e
include ..\..\builtins\pgetpath.e
--*/

global sequence initialcurrentdir

include ..\..\builtins\file.e

global function cleanUpPath(sequence name)
integer k   
    --
    -- Replace any / in filepath with \\
    --
    while 1 do
        k = find('/',name)
        if k=0 then exit end if
        name[k]='\\'
    end while
    --
    -- check for and remove any \..\ in filepath
    --
    while 1 do
        k=match("\\..\\",name)
        if k=0 then exit end if
--      if find('\\',name)=k then   -- avoid infinite loop
--          -- should never happen
----            void = proemh("Warning, cannot cleanup", name, 0)
--          puts(1,"Warning, cannot cleanup path "&name)
--          exit
--      end if
        for j=k-1 to 1 by -1 do
            if name[j]='\\' then
                name=name[1..j]&name[k+4..length(name)]
                k=0 -- signal found
                exit
            end if
        end for
        if k!=0 then
            if length(name)>2 and name[2]!=':' then
                name=initialcurrentdir&name
                if name[2]!=':' then ?9/0 end if    -- sanity check
            else
                puts(1,"Warning, cannot cleanup path "&name&'\n')
                exit
            end if
        end if
    end while
    --
    -- Make sure there is a proper path.
    --

    if not find('\\',name) and (length(name)<2 or name[2]!=':') then
        name=initialcurrentdir&name
    end if
    return name
end function

function getInitialCurrentDir()
-- based on Greg Haberek's posting to EUforum, 2005 May 28
sequence res
integer found
    res = command_line()
    res = res[2]
    found = 0
    -- res may be 1) D:\Edita\Edita.exw     (perfick!)
    --            2) D:progs\Edita.exw      (? partial path ? NB: odd case, not handled)
    --            3) D:Edita.exw            (Not enuf info; rely on current_dir instead)
    --            4) Edita.exw              (Zero info given; rely on current_dir instead)
if usegpp then
    res = get_proper_path(res,"")
else
    if length(res)<2 or res[2]!=':' then
        res = current_dir()&'\\'&res
        res = cleanUpPath(res)
    elsif length(res)>3 and res[2]=':' and not find(res[3],"\\/") then
--      res = current_dir()&'\\'&res[4..length(res)]
        res = current_dir()&'\\'&res[3..length(res)]    --DEV 6/10/06 (untested)
        res = cleanUpPath(res)
    end if
end if
    for i = length(res) to 1 by -1 do
        if find(res[i],"\\/") then
            found = i
            exit
        end if
    end for
    if found then
        res = res[1..found]
--DEV untested:
--      if found<2 or res[2]!=':' then
--          if find(res[1],"\\/") then
--              res = current_dir()&res
--          else
--              res = current_dir()&'\\'&res
--          end if
--      end if
    else
        res = current_dir()&'\\'
    end if
    if match("\\pp\\",res)=length(res)-3 then
        res = res[1..length(res)-3]
    end if
    return res
end function
--global function getAppPath()
--  sequence path,path2,cmd
--  integer index
--  cmd=command_line()
--  path=cmd[2]
--  if not find(':',path) then
--      path=current_dir()
--      if cmd[2][1]='\\' then
--      path=path[1..2]
--      elsif path[length(path)]!='\\' then
--          path&='\\'
--      end if
--      path=path & cmd[2]
--  end if
--  path2=reverse(path)
--  index=find('\\',path2)
--  return path[1..length(path)-index] --add +1 to keep backslash
--end function


--global constant initialcurrentdir=current_dir()&'\\'
--global constant 
    initialcurrentdir = getInitialCurrentDir()
sequence debug_cl, debug_cd -- for diagnostic purposes
        debug_cl = command_line()
        debug_cd = current_dir()


global integer Xtrans   -- translate keywords and builtins
                Xtrans=0    -- set by eaxlate.e (on "and")


sequence english, foreign
         english = {}
         foreign = {}

--global object KeyHelpText, ProjHelpText
--            KeyHelpText=0

sequence tfname, tffile

integer firsterror
        firsterror=1

global integer r_Proemh
               r_Proemh=0
                isDebug=0   -- avoid unassigned error (set proper by eaini.e rsn)

global function xl(sequence text)
integer k
    if length(english) then
        k=find(text,english)    --DEV binary chop...
        if k=0 then
            if not find(text,{"-",""})
            and firsterror then
                if r_Proemh and isDebug then
--                  call_proc(r_No_Trans,{text,tffile})
                    k = call_func(r_Proemh,{"Warning",
                                            sprintf("No translation for %s in %s",{text,tffile}),
                                            {"OK","Crash"}})
                    if k=2 then ?9/0 end if
                else
                    void = messageBox("Warning",
                        sprintf("No translation for %s in %s",{text,tffile}),0)
                end if
                firsterror=0
            end if
        else
            text=foreign[k]
        end if
    end if
    return text
end function

global function xlQ(sequence text)  -- no warning, ever.
-- this is used for the sections in syntax files, which are 
-- user-customisable and hence I cannot guarantee they exist.
integer k
    if length(english) then
        k=find(text,english)    --DEV binary chop...
        if k then
            text=foreign[k]
        end if
    end if
    return text
end function

global function xlna(sequence text)
-- load translation without ampersand
integer k
    text=xl(text)
    k=find('&',text)
    if k then
        text=text[1..k-1]&text[k+1..length(text)]
    end if
    return text
end function

global function xla(sequence text)
-- get the ampersanded char, if any
integer k
    text=xl(text)
    k=find('&',text)
    if k and k<length(text) then
        return UpperCase(text[k+1])
    end if
    return 0
end function

global function xlalt(sequence text)
-- assume it is "... Alt x)" and collect the x
integer k
    text=xl(text)
    k = length(text)
    if k>1 and text[k]=')' then
        return text[k-1]
    end if
    return 0
end function

--global function xls(sequence things)
--  for i=1 to length(things) do
--      things[i]=xl(things[i])
--  end for
--  return things
--end function

integer tf, ch, line

procedure Xerr(sequence msg)
    if firsterror then
        void = messageBox("Translation file error",
                sprintf("%s line %d: %s",{tfname,line,msg}),0)
        firsterror=0
        void=seek(tf,-1)
    end if
end procedure

procedure skip()
    while 1 do
        if ch='-' then
            while 1 do
                ch = getc(tf)
                if find(ch,{'\r','\n',-1}) then exit end if
            end while
        elsif ch = '[' then -- versioning
            while 1 do
                ch = getc(tf)
                if find(ch,{'\r','\n',-1,']'}) then exit end if
            end while
        elsif ch = '\\' then -- line continuation
        elsif not find(ch,{' ','\r','\n','\t','='}) then
            exit
        end if
        if ch = '\n' then
            line+=1
        end if
        ch = getc(tf)
    end while
end procedure

constant Escapes="\\trn\'\"",
         EscBytes="\\\t\r\n\'\""

function getString()
-- called when a double quote has been found.
sequence res
object oneline
integer k
--  res={}
    res=""
    while 1 do
        ch=getc(tf)
        if ch = '\"' then exit end if
        if ch = '\\' then
            ch = getc(tf)
            k = find(ch,Escapes)
            if k=0 then
                Xerr("unknown escape sequence \\"&ch)
                return ""
            end if
            ch = EscBytes[k]
        elsif ch = '\t' then
            Xerr("tab char illegal")
            return ""
        elsif find(ch,{'\r','\n',-1}) then
            Xerr("missing closing quote")
            return ""
        end if
        res&=ch
    end while
    ch = getc(tf)   -- discard trailing quote
    if ch = '\"' and length(res)=0 then -- triple quote handling (""")
        ch=getc(tf)
        while 1 do
            oneline=gets(tf)
            if atom(oneline) then
                Xerr("missing closing quote")
                return ""
            end if
            k = length(oneline)
            if k and oneline[k]='\n' then
                k -= 1
                oneline=oneline[1..k]
            end if
            if k or length(res) then
                k = match(`"""`,oneline)
                if k then
                    if k>1 then
                        res=append(res,oneline[1..k-1])
                    end if
                    exit
                end if
                res=append(res,oneline)
            end if
        end while
        ch=getc(tf)
    end if
    return res
end function

--global procedure LoadHelpText()
--object line
--integer len
--sequence kbdfile
--  KeyHelpText={"lang\\.kbd file missing"}
--  ProjHelpText=KeyHelpText
--  if tf!=-2 then  -- skip if already warned
--      kbdfile=tffile[1..length(tffile)-3]&"kbd"
--      tf = open(kbdfile,"r")
--      if tf!=-1 then
--          KeyHelpText={}
--          while 1 do
--              line=gets(tf)
--              if atom(line) then exit end if
--              len=length(line)
--              if len and line[len]='\n' then
--                  line=line[1..len-1]
--              end if
--              if equal(line,"#project help -- do not translate this line") then   --DEV counter...
--                  ProjHelpText={}
--                  while 1 do
--                      line=gets(tf)
--                      if atom(line) then exit end if
--                      len=length(line)
--                      if len and line[len]='\n' then
--                          line=line[1..len-1]
--                      end if
--                      ProjHelpText=append(ProjHelpText,line)
--                  end while
--                  exit
--              end if
--              KeyHelpText=append(KeyHelpText,line)
--          end while
--          close(tf)
--      end if
--  end if
--end procedure

global sequence keywords, fkeywords, builtins, fbuiltins
                keywords={}          builtins={}

global function KtoF(sequence keyword)
-- translate keywords
integer k
    if length(keywords) then
        k=find(keyword,keywords)
        if k then return fkeywords[k] end if
    end if
    return keyword
end function

global function FtoK(sequence keyword)
-- translate keywords
integer k
    if length(keywords) then
        k=find(keyword,fkeywords)
        if k then return keywords[k] end if
    end if
    return keyword
end function

global function BtoF(sequence builtin)
-- translate builtins
integer k
    if length(builtins) then
        k=find(builtin,builtins)
        if k then return fbuiltins[k] end if
    end if
    return builtin
end function

global function FtoB(sequence builtin)
-- translate builtins
integer k
    if length(builtins) then
        k=find(builtin,fbuiltins)
        if k then return builtins[k] end if
    end if
    return builtin
end function


procedure loadBuiltins()
sequence eword
    fbuiltins={}
    while 1 do
        if length(fbuiltins) then
            skip()
            if ch='\"' then
                eword=getString()
--              if equal(eword,"abort") then
--                  stdwords={eword}
--                  loadStdInc()
--                  exit
--              end if
                builtins=append(builtins,eword)
            elsif ch = -1 then
                exit
            end if
        end if
        skip()
        if ch = '\"' then
            fbuiltins=append(fbuiltins,getString())
        else
            -- both english & foreign strings required!
            Xerr("quote expected")
            fbuiltins=append(fbuiltins,"???")
            exit
        end if
    end while
end procedure


procedure loadKeyWords()
sequence eword
    Xtrans=1
    fkeywords={}
    while 1 do
        if length(fkeywords) then
            skip()
            if ch='\"' then
                eword=getString()
                if equal(eword,"abort") then
                    builtins={eword}
                    loadBuiltins()
                    exit
                end if
                keywords=append(keywords,eword)
            elsif ch = -1 then
                exit
            end if
        end if
        skip()
        if ch = '\"' then
            fkeywords=append(fkeywords,getString())
        else
            -- both english & foreign strings required!
            Xerr("quote expected")
            fkeywords=append(fkeywords,"???")
            exit
        end if
    end while
end procedure

procedure buildTranslationTables()
sequence eword
--puts(1,"buildTranslationTables()\n")
    line=0
--  ch=getc(tf)
    while 1 do
        skip()
        if ch = '\"' then   -- error next, then
            eword=getString()
            if equal(eword,"and") then
                keywords={eword}
                loadKeyWords()
                exit
            end if
            english=append(english,eword)
            skip()
--      elsif ch='#' then
--          LoadHelpText()
--          exit
        elsif ch = -1 then
            exit    -- end of file
        end if
        if ch = '\"' then   -- error next, then
            foreign=append(foreign,getString())
        else
            -- both english & foreign strings required!
            Xerr("quote expected")
            foreign=append(foreign,"???")
            exit
        end if
    end while
    close(tf)
--puts(1,"buildTranslationTables() exiting\n")
end procedure


--integer LANG
--LANG=loWord( c_func(xGetUserDefaultLangID,{}) )

--  xGetLocaleInfo = link_c_func(kernel32, "GetLocaleInfo",
--      {C_LONG,    --  LCID  Locale,       // locale identifier 
--       C_LONG,    --  LCTYPE  LCType,     // type of information 
--       C_PTR,     --  LPTSTR  lpLCData,   // address of buffer for information 
--       C_INT},    --  int  cchData         // size of buffer 
--      C_INT),     -- int

integer k
atom pBuf = allocate(4)
--DEV this WORKS!
--#define LOCALE_SABBREVLANGNAME 0x00000003 // abbreviated
constant LOCALE_SABBREVLANGNAME = 3
--LOCALE_USER_DEFAULT = #400,

    k = c_func(xGetLocaleInfo,{LOCALE_USER_DEFAULT,LOCALE_SABBREVLANGNAME,pBuf,4})
--  error if k!=4, , lang files to be renamed.
--include builtins\ppp.e
--  pp({k,peek({pBuf,k})})
    if k!=4 then ?9/0 end if
    tfname = UpperCase(peek({pBuf,3}))
    free(pBuf)
--tfname="DEU"
            --                  LOCALE_USER_DEFAULT,DATE_LONGDATE
--          len = c_func(xGetDateFormat,{#400,2,NULL,NULL,NULL,0})

--LANG=#80C --DEV!!
--LANG=#041D
-- This list is from win32.hlp (1995):
-- If you are looking for a suitable code, try ISO 3166 Country Codes, 
-- but you *MUST* ensure they are unique/not already in use (a simple 
-- search of this file is all that is needed).
--constant knownLang={
----Identifier  Locale
-- { #0436, "", "Afrikaans"}
--,{ #041C, "al", "Albania"}
--,{ #1401, "dz", "Algeria"}
--,{ #0409, "en", "American"}
--,{ #0C09, "en", "Australian"}
--,{ #0C07, "de", "Austrian"}
--,{ #3C01, "bh", "Bahrain"}
--,{ #042D, "", "Basque"}
--,{ #080C, "nl", "Belgian"}
--,{ #0813, "nl", "Belgian (Flemish)"}
--,{ #0809, "en", "British"}
--,{ #0402, "bg", "Bulgaria"}
--,{ #0423, "", "Byelorussia"}
--,{ #1009, "en", "Canadian"}
--,{ #0403, "", "Catalan"}
--,{ #041A, "hr", "Croatian"}
--,{ #0405, "cz", "Czech"}
--,{ #0406, "dk", "Danish"}
--,{ #0413, "nl", "Dutch (Standard)"}
--,{ #0C01, "eg", "Egypt"}
--,{ #0425, "ee", "Estonia"}
--,{ #0429, "", "Farsi"}
--,{ #040B, "fi", "Finnish"}
--,{ #040C, "fr", "French (Standard)"}
--,{ #0C0C, "fr", "French Canadian"}
--,{ #0407, "de", "German (Standard)"}
--,{ #042E, "de", "Germany"}
--,{ #0408, "gr", "Greek"}   --el
--,{ #0C04, "hk", "Hong Kong"}
--,{ #040E, "hu", "Hungarian"}
--,{ #040F, "is", "Icelandic"}
----,{ #041D, "id", "Ido"}
--,{ #0421, "id", "Indonesia"}
--,{ #0801, "iq", "Iraq"}
--,{ #1809, "en", "Ireland"}
--,{ #040D, "il", "Israel"}
--,{ #0410, "it", "Italian (Standard)"}
--,{ #0411, "jp", "Japan"}
--,{ #2C01, "jo", "Jordan"}
--,{ #0412, "kr", "Korea"}
--,{ #3401, "kw", "Kuwait"}
--,{ #0426, "lv", "Latvia"}
--,{ #3001, "lb", "Lebanon"}
--,{ #1001, "ly", "Libya"}
--,{ #1407, "de", "Liechtenstein"}
--,{ #0427, "lt", "Lithuania"}
--,{ #140C, "fr", "Luxembourg (French)"}
--,{ #1007, "de", "Luxembourg (German)"}
--,{ #042F, "mk", "Macedonia"}
--,{ #080A, "mx", "Mexican"}
--,{ #0818, "md", "Moldavia"}
--,{ #0819, "md", "Moldavia"}
--,{ #1801, "ma", "Morocco"}
--,{ #1409, "en", "New Zealand"} --nz
--,{ #0414, "", "Norwegian (Bokmal)"}  --no
--,{ #0814, "", "Norwegian (Nynorsk)"}
--,{ #2001, "om", "Oman"}
--,{ #0415, "pl", "Polish"}
--,{ #0416, "pt", "Portuguese (Brazilian)"}
--,{ #0816, "pt", "Portuguese (Standard)"}
--,{ #0804, "", "PRC"}
--,{ #4001, "qa", "Qatar"}
--,{ #0417, "", "Rhaeto-Romanic"}
--,{ #0418, "ro", "Romania"}
--,{ #0419, "ru", "Russian"}
--,{ #0401, "sa", "Saudi Arabia"}
--,{ #081A, "cs", "Serbian"}    -- Czech?
--,{ #1004, "sg", "Singapore"}
--,{ #041B, "sk", "Slovak"}
--,{ #0424, "si", "Slovenia"}
--,{ #0C0A, "es", "Spanish (Modern Sort)"}
--,{ #040A, "es", "Spanish (Traditional Sort)"}
--,{ #240A, "ec", "Spanish (Colombia)"}
--,{ #2C0A, "es", "Spanish (Argentina)"}
--,{ #0430, "", "Sutu"}
--,{ #041D, "se", "Swedish"}
--,{ #100C, "fr", "Swiss (French)"}
--,{ #0807, "de", "Swiss (German)"}
--,{ #0810, "it", "Swiss (Italian)"}
--,{ #2801, "sy", "Syria"}
--,{ #0404, "tw", "Taiwan"}
--,{ #041E, "th", "Thailand"}
--,{ #0431, "to", "Tsonga"}
--,{ #0432, "", "Tswana"}
--,{ #1C01, "tn", "Tunisia"}
--,{ #041F, "tr", "Turkish"}
--,{ #3801, "ae", "U.A.E."}
--,{ #0422, "ua", "Ukraine"}
--,{ #0420, "", "Urdu"}
--,{ #0433, "", "Venda"}
--,{ #0435, "", "Xhosa"}
--,{ #2401, "ye", "Yemen"}
--,{ #0436, "", "Zulu"}
--}
--

--include builtins\ppp.e

constant lang="lng_"
    -- allow eg elng_ENU.txt (American English) to contain "elng_ENG.txt" (British English)
    for j=1 to 2 do     -- allow at most one such hop
        tffile=initialcurrentdir&"lang\\elng_" & tfname & ".txt"
        tf = open(tffile,"r")
        if tf=-1 then exit end if
        ch=getc(tf)
        if j=2 then exit end if
        if ch!='e' then exit end if
        for i=1 to 4 do
            if getc(tf)!=lang[i] then ?9/0 end if
        end for
        for i=1 to 3 do
            tfname[i]=getc(tf)
        end for
        close(tf)
    end for

    if tf!=-1 then
        buildTranslationTables()
    end if
--sequence cl
--  tf=-2
--  for i = 1 to length(knownLang) do
--      if LANG=knownLang[i][1] then
--          tfname=knownLang[i][2]
--          if length(tfname) then
----                tffile=current_dir()&"\\lang\\ealng_" & tfname & ".txt"
--              tffile=initialcurrentdir&"lang\\ealng_" & tfname & ".txt"
--              tf = open(tffile,"r")
--              if tf!=-1 then
--                  buildTranslationTables()
----pp(keywords)
----if length(keywords) then
----    pp(fkeywords)
----end if
----pp(builtins)
----if length(builtins) then
----    pp(fbuiltins)
----end if
--              else
---- GB Message moved to eaini.e
----                    void = messageBox("Warning",tffile&" not found;\n"&
----                    "Program will continue in English",0)
--                  firsterror=0
--              end if
----            else
----                --DEV should I bother with this?
----                void = messageBox("Sorry",
----                  knownLang[i][3]&" language support is not avalable;\n"&
----                  "please contact the author if you would like to help\n"&
----                  "by translating (see ealng_en.txt)",0)
----                  tf=-1
--          end if
--          exit
--      end if
--  end for
--  if tf = -2 then
----GB Message moved to eanini.e
----        void = messageBox("Warning",
----          sprintf("Locale %x unrecognised.\nProgram will continue in English.",LANG),0)
--      firsterror=0
--  end if
    
--GB new routine
global function getxlstatus()
--  if tf=-2 then return {-2,"",LANG} end if    -- as tfname may not be assigned
    return {tf,tfname}--,LANG}
end function

--PL removed:
--global function getxlatename()    -- for eaxutil.ew
---- can be {-2, #0436}, or eg {"en",#809}
--  if tf=-2 then return {-2,LANG} end if
--  return {tfname,LANG}
--end function

global function getexhname()
sequence exhpath, exhname
    if tf>=0 then
--      exhpath=current_dir()&"\\lang\\"
        exhpath=initialcurrentdir&"lang\\"
--      exhname="ealng_" & tfname & ".exh"
        exhname="elng_" & tfname & ".exh"
--      if not atom(pdir:dir(exhpath&exhname)) then
        if not atom(dir(exhpath&exhname)) then
            return {exhpath,exhname}
        end if
    end if
    return 0
end function
