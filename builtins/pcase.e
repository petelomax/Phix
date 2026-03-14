--
-- pcase.e
-- =======
--
-- plain ASCII version of the more human-readable pcase8.e, see that for comments.
--
without debug -- keep ex.err clean (overshadowed by same in pdiag.e)

integer caseinit = 0
string toUpper, toLower
sequence str_methods
local enum LOWER = 1, UPPER = 2, CAPITALISE = 3, SENTENCE = 4, INVERT = 5

local procedure initcase()
    str_methods = {"LOWER","UPPER","CAPITALISE","SENTENCE","INVERT"}
    integer i32
    toUpper = repeat(255,255)
    toLower = repeat(255,255)
    for i=1 to 254 do
        toUpper[i] = i
        toLower[i] = i
    end for
    for i='A' to 'Z' do
        i32 = i+32
        toLower[i] = i32
        toUpper[i32] = i
    end for
    for i=#C0 to #D6 do -- see pcase8.e
--  for i=192 to 214 do -- see pcase8.e
        i32 = i+32
        toLower[i] = i32
        toUpper[i32] = i
    end for
    for i=#D8 to #DE do -- see pcase8.e
--  for i=216 to 222 do -- see pcase8.e
        i32 = i+32
        toLower[i] = i32
        toUpper[i32] = i
    end for

    -- several odd-balls, see pcase8.e
--  toLower[#8A] = #9A
--  toLower[#8C] = #9C
--  toLower[#9F] = #FF
--  toUpper[#9A] = #8A
--  toUpper[#9C] = #8C
--  toUpper[#FF] = #9F
    caseinit = 1
end procedure

global function upper(object x)
object o
integer c
    if not caseinit then initcase() end if
    if sequence(x) then
        for i=1 to length(x) do
            o = x[i]
            if sequence(o) then
                -- 8/2/23 p2js violation
--              x[i] = upper(o)
                x[i] = 0
                o = upper(o)
                x[i] = o

            elsif integer(o) then
                c = o
                if c>0 and c<=255 then
                    x[i] = toUpper[c]
                end if
            end if
        end for
    elsif integer(x) then
        c = x
        if c>0 and c<=255 then
            x = toUpper[c]
        end if
    end if
    return x
end function

global function lower(object x)
object o
integer c
    if not caseinit then initcase() end if
    if sequence(x) then
        for i=1 to length(x) do
            o = x[i]
            if sequence(o) then
                -- 8/2/23 p2js violation
--              x[i] = lower(o)
                x[i] = 0
                o = lower(o)
                x[i] = o
            elsif integer(o) then
                c = o
                if c>0 and c<=255 then
                    x[i] = toLower[c]
                end if
            end if
        end for
    elsif integer(x) then
        c = x
        if c>0 and c<=255 then
            x = toLower[c]
        end if
    end if
    return x
end function

global function isupper(integer ch)
    if not caseinit then initcase() end if
    return (ch>0 and ch<=255 and ch!=toLower[ch])
end function

global function islower(integer ch)
    if not caseinit then initcase() end if
    return (ch>0 and ch<=255 and ch!=toUpper[ch])
end function

--DEV doc/auto:
--**
-- Convert a text sequence to capitalized words.
--
-- Parameters:
--   # ##x## : A text sequence.
--
-- Returns:
--   A **sequence**, the Capitalized Version of ##x##
--
-- Comments:
-- A text sequence is one in which all elements are either characters or
-- text sequences. This means that if a non-character is found in the input,
-- it is not converted. However this rule only applies to elements on the
-- same level, meaning that sub-sequences could be converted if they are
-- actually text sequences.
--
--
-- Example 1:
-- <eucode>
-- s = proper("euphoria programming language")
-- -- s is "Euphoria Programming Language"
-- s = proper("EUPHORIA PROGRAMMING LANGUAGE")
-- -- s is "Euphoria Programming Language"
-- s = proper({"EUPHORIA PROGRAMMING", "language", "rapid dEPLOYMENT", "sOfTwArE"})
-- -- s is {"Euphoria Programming", "Language", "Rapid Deployment", "Software"}
-- s = proper({'a', 'b', 'c'})
-- -- s is {'A', 'b', c'} -- "Abc"
-- s = proper({'a', 'b', 'c', 3.1472})
-- -- s is {'a', 'b', c', 3.1472} -- Unchanged because it contains a non-character.
-- s = proper({"abc", 3.1472})
-- -- s is {"Abc", 3.1472} -- The embedded text sequence is converted.
-- </eucode>
--
-- See Also:
--     [[:lower]] [[:upper]]

--/*
-- replaced below...
--global function proper(sequence x)
---- Converts text to lowercase and makes each word start with an uppercase.
----integer pos
--integer inword = 0                -- Initially not in a word
----integer convert = string(x) -- Initially convert text
----sequence res
--
----    res = x                 -- Work on a copy of the original, in case we need to restore.
--  if string(x) then
--      for i=1 to length(x) do
--          integer ch = x[i]
--          -- Check for upper case
--          integer pos = isupper(ch)
--          if pos=0 then
--              -- Not upper, so check for lower case
--              pos = islower(ch)
--              if pos=0 then
--                  -- Not lower so check for digits
--                  -- n.b. digits have no effect on if its in a word or not.
--                  pos = ch>='0' and ch<='9'
--                  if pos=0 then
--                      -- not digit so check for special word chars
----                        pos = t_specword(ch)
----                        if pos then
--                      if ch='_' then
--                          inword = 1
--                      else
--                          inword = 0
--                      end if
--                  end if
--              else
--                  if inword=0 then
--                      -- start of word, so convert only lower to upper.
----I have no idea what this test is/was:
----                        if pos<=26 then
--                          x[i] = upper(ch) -- Convert to uppercase
----                        end if
--                      inword = 1      -- now we are in a word
--                  end if
--              end if
--          else
--              if inword=1 then
--                  -- Upper, but as we are in a word convert it to lower.
--                  x[i] = lower(ch) -- Convert to lowercase
--              else
--                  inword = 1      -- now we are in a word
--              end if
--          end if
--      end for
--  else            
--      for i=1 to length(x) do
--          object xi = x[i]
--          if sequence(xi) then
--              x[i] = proper(xi) -- recursive conversion
--          end if
--      end for
--  end if
--  return x
--end function
--*/

sequence proper_nouns -- (re-usable via a method of {})

global function proper(string s, sequence method="CAPITALISE")
    if not caseinit then initcase() end if
    bool eos = true,        -- end of sentence flag
         inQuote = false,   -- within double quotes flag
         islo = false,      -- ch is lowercase
         ishi = false,      -- ch is uppercase
         lowit = false,     -- change it?
         highit = false,    -- change it?
         bProperNouns = not string(method)
    if bProperNouns then
        if length(method) then
            -- (ie {} re-uses one set up earlier, w/o re-lowercasing it)
            proper_nouns = apply(method,lower)
        end if
        method = "SENTENCE"
    end if
    integer mi = find(upper(method),str_methods), -- must be one of those!
            pc = ' ',       -- previous character
            l = length(s),
            word_start = 0
    if mi=0 then crash("proper(s,\"%s\"): invalid method parameter",{method}) end if
    for i,ch in s do
--      islo := (ch>='a' and ch<='z')   lowit = false
--      ishi := (ch>='A' and ch<='Z') highit = false
--      islo := (ch>0 and ch<=255 and ch!=toUpper[ch])  lowit = false
--      ishi := (ch>0 and ch<=255 and ch!=toLower[ch])  highit = false
        islo := (ch!=toUpper[ch])   lowit = false
        ishi := (ch!=toLower[ch])   highit = false
        switch mi do
            case LOWER then lowit := ishi
            case UPPER then highit := islo
            case INVERT then lowit := ishi
                            highit := islo
            case CAPITALISE then
                        if find(pc," \t\r\n\"\'`") then
--                      if find(pc," \r\n\"\'`"&9) then
                            highit := islo
                        else
                            lowit := ishi
                        end if  
            case SENTENCE then
                    if find(pc," \t\r\n") then
--                  if find(pc," \r\n"&9) then
                        if eos then
                            highit := islo
                            eos := false
                        else
                            lowit := ishi
                        end if
                        word_start = i
                    else
                        lowit := ishi
                    end if
                    if find(ch,".!?") then
                        eos := true
                    elsif not find(ch," \t\r\n") then
--                  elsif not find(ch," \r\n"&9) then
                        eos := false
                    end if
                    if ch='\"' then
                        ch := ' '
                        inQuote := not inQuote
--                      eos := inQuote
                        eos := inQuote and i<l and s[i+1]!=toLower[s[i+1]]
                    end if
        end switch
        pc := ch
        if     lowit then ch += 32 s[i] := ch
        elsif highit then ch -= 32 s[i] := ch end if
        if bProperNouns and word_start and (i=l or not (islo or ishi)) then
            if find(lower(s[word_start..i-(i!=l)]),proper_nouns) then
                s[word_start] = upper(s[word_start])
            end if
            word_start = 0
        end if
    end for
    return s
end function

