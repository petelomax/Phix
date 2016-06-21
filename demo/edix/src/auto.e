--
-- eauto.e
--
-- Autocomplete processing.
--
-- The .syn files contain an optional AutoComplete section with lines such as:
--
--      ^ i&f | then^pend if
--
--  The ^ character is interpreted (like \ in strings and % in printf):
--      "^ " denotes whitespace (any combination of spaces and tabs)
--            This may only occur at the start of a line.
--      "^p" denotes a line break
--      "^t" denotes a tab character
--      "^," denotes a ( or , for parameter definitions.
--            If this is not used, cursor must be at the very end of the
--            line for any autocomplete to trigger.
--      "^b" reserved for backspace (not implemented)
--      "^^" reserved for a single hat (not implemented)
--
--  Each autocomplete is defined on one line only, and inline comments are not 
--   allowed (in eg Euphoria.syn); instead they must be on a separate line.
--
--  A single & prefix indicates the "trigger" key. Text to the left must
--      match the current line, text to the right is inserted.
--
--  A | indicates the desired new cursor position.
--
-- So, the example above means:
--  when 'f' is keyed, check that the cursor is at the end of a line which is
--  some whitespace (or none) followed by 'i', and if so, insert "  then" and
--  "end if", and move the cursor one space right from where the f was.
--
-- We also have lines such as:
--  
--  a&tom |
--  global a&tom |
--
-- Which again checks the line when a 't' is keyed and inserts "om ". Note that
--  these must be at the beginning of the line, if you want to allow leading
--  whitespace, then put a "^ " at the start of those lines. As you can see, the
--  syntax of the AutoComplete section does not allow "global" to be defined as
--  optional, instead two separate rules must be entered.
--
-- Lastly there are some lines of the form:
--
--  ^,&atom |
--
-- Which check for the letters a, i, o, and s following either ( or , and
--  expand then to atom, integer, object, or sequence respectively.
--

--Not attempted:
--      "^b" denotes the backspace key
--  ^ ^t&^belse|
--  ^ else&^bif | then
--  ^ elsif | then&^b^<|


global sequence autoComplete        -- set by changeTo() and below

global constant acWHITESPACE=-3,
                acCOMMA=-2,                 -- actually, ( or ,
                acEOL=-1,
                acLINEPOS=1,
                acLINEMATCH=2,
                acTRIGGERCH=3,
                acCURSORMOVE=4,
                acINSERTBLOCK=5


-- see also loadAutoComplete() in easynld.e

function lineMatch(integer aCset, integer ch)
sequence oneline
integer pos, what
    oneline = filetext[currfile][CursorY+1]
    pos = 1
    for i = 1 to length(autoComplete[aCset][acLINEMATCH]) do
        what = autoComplete[aCset][acLINEMATCH][i]
        if what = acWHITESPACE then
            while 1 do
                if pos > length(oneline) then return 0 end if
                if not find(oneline[pos]," \t") then exit end if
                pos += 1
            end while
        else
            if pos > length(oneline) then return 0 end if
            if oneline[pos] != what then return 0 end if
            pos += 1
        end if
    end for
    if pos != length(oneline) then return 0 end if
    if oneline[pos] != ch then return 0 end if
    return 1
end function


global integer acBlockID    -- routine_id of acBlock (in edita.exw)

sequence acRemainder        -- don't drag "then", "do", etc to next line
         acRemainder = ""
integer acRemIdx            -- if user keys "integer", and it auto-completes after
        acRemIdx = 0        --  "int", the "eger" does NOT make it "integer eger"

sequence outerRem           -- don't drag ) onto next line, even if 'a'tom, 'i'nteger, etc
         outerRem = ""      --  have been auto-completed within the ().

global function acNoDrag(integer k, sequence oneline, integer ch)
-- check for autocompleted text still after the cursor:
--  don't drag it onto the next line.
integer leno, lenr, res
    res = 0
    if k and isAutoComplete and length(acRemainder) then
--      if ch = VK_RETURN then
        if ch = '\n' then
            -- match against the end of the line
            oneline = oneline[k..length(oneline)]
            leno = length(oneline)
            if leno then
                lenr = length(acRemainder)
                if lenr>=leno
                and equal(oneline,acRemainder[lenr-leno+1..lenr]) then
                    -- stop "then", "do", etc being dragged onto next line.
                    CursorX += leno
                    res = 1
                else
                    lenr = length(outerRem)
                    if lenr >= leno
                    and equal(oneline,outerRem[lenr-leno+1..lenr]) then
                        -- stop final ")" being dragged onto next line.
                        CursorX += leno
                        res = 1
                    end if
                end if
            end if
            acRemIdx = 0
            acRemainder = ""
            outerRem = ""
        end if
    end if
    return res
end function

procedure doAuto(integer i)
integer nX, nY
sequence ai
    ai = autoComplete[i]
    nX = CursorX + ai[acCURSORMOVE]
    nY = CursorY
    acRemIdx = 0
    call_proc(acBlockID,{ai[acINSERTBLOCK]})
    acRemainder = ai[acINSERTBLOCK][1]
    acRemIdx = 1
    CursorX = nX
    CursorY = nY
end procedure

--with trace
global procedure AutoComplete(integer ch, sequence oneline, integer k)
--
-- check for trigger characters, and autocomplete-able lines
-- (btw, oneline is as of before ch applied, as is k)
--
sequence ai, ailm
integer lailm

    for i=1 to length(autoComplete) do
        ai = autoComplete[i]
        if ch=ai[acTRIGGERCH] then
            if ai[acLINEPOS]=acEOL then
                if k=0 then                                 -- and at end of line
                    if lineMatch(i,ch) then
                        doAuto(i)
                        exit
                    end if
                end if
--          elsif ai[acLINEPOS]=acCOMMA and                 -- must be.
--27/3/2010:
--          elsif k!=1 then                                 -- not start of line
            else
              ailm = ai[acLINEMATCH]
              lailm = length(ailm)
              if k>lailm
              and equal(oneline[k-lailm..k-1],ailm) then
--              for j=k-1 to 1 by -1 do
                for j=k-lailm-1 to 1 by -1 do
                    if not find(oneline[j]," \t") then
                        if find(oneline[j],"(,")
                        and (match("function",oneline) or
                             match("procedure",oneline) or
                             match("type",oneline)) then
                            if length(acRemainder) and not length(outerRem) then
                                outerRem = acRemainder
                            end if
                            doAuto(i)
                        end if
                        exit
                    end if
                end for
              end if
            end if
        end if
    end for
end procedure

global function skipAutoComplete(integer ch)
-- after eg 'int' is autocompleted to 'integer', if the user carries on typing
--  the remainder of the word ('eger'), ignore it.
    if acRemIdx then
        if ch = acRemainder[acRemIdx] then
            acRemIdx += 1
            if acRemIdx > length(acRemainder) then
                acRemIdx = 0
            end if
            return 1
        end if
        acRemIdx = 0
    end if
    return 0
end function
