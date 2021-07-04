--
-- builtins/pvlookup.e
--

global function vlookup(object find_item, sequence grid_data, integer source_col, integer target_col, object def_value = 0)
object gdi

    for i=1 to length(grid_data) do
        gdi = grid_data[i]
        if sequence(gdi) 
        and source_col<=length(gdi)
        and equal(find_item, gdi[source_col]) then
            if target_col>length(gdi) then exit end if
            return gdi[target_col]
        end if
    end for
    
    return def_value

end function

--DEV doc/auto:
--**
-- If the supplied item is in the source list, this returns the corresponding
-- element from the target list.
-- Parameters:
-- # ##find_item##: an object that might exist in ##source_list##.
-- # ##source_list##: a sequence that might contain ##pITem##.
-- # ##target_list##: a sequence from which the corresponding item will be returned.
-- # ##def_value##: an object (defaults to zero). This is returned when ##find_item## 
-- is not in ##source_list## **and** ##target_list## is not longer than ##source_list##.
--
-- Returns:
-- an object
-- * If ##find_item## is found in ##source_list## then this is the corresponding element
-- from ##target_list##
-- * If ##find_item## is not in ##source_list## then if ##target_list## is longer than ##source_list##
-- then the last item in ##target_list## is returned otherwise ##def_value## is returned.
--
-- Examples:
-- <eucode>
-- lookup('a', "cat", "dog") --> 'o'
-- lookup('d', "cat", "dogx") --> 'x'
-- lookup('d', "cat", "dog") --> 0
-- lookup('d', "cat", "dog", -1) --> -1
-- lookup("ant", {"ant","bear","cat"}, {"spider","seal","dog","unknown"}) --> "spider"
-- lookup("dog", {"ant","bear","cat"}, {"spider","seal","dog","unknown"}) --> "unknown"
-- </eucode>
--
global function lookup(object find_item, sequence source_list, sequence target_list, object def_value = 0)
    integer k = find(find_item, source_list)
    if k>=1 and k<=length(target_list) then
        def_value = target_list[k]
    elsif length(target_list)>length(source_list) then
        def_value = target_list[$]
    end if
    return def_value
end function

--DEV doc/auto/move to a better location...
--**
-- Converts a string containing Key/Value pairs into a set of
-- sequences, one per K/V pair.
--
-- Parameters:
-- # ##source## : a text sequence, containing the representation of the key/values.
-- # ##pair_delim## : an object containing a list of elements that delimit one
--                   key/value pair from the next. The defaults are semi-colon (;)
--                   and comma (,).
-- # ##kv_delim## : an object containing a list of elements that delimit the
--                key from its value. The defaults are colon (:) and equal (=).
-- # ##quotes## : an object containing a list of elements that can be used to
--                enclose either keys or values that contain delimiters or
--                whitespace. The defaults are double-quote ("), single-quote (')
--                and back-quote (`)
-- # ##whitespace## : an object containing a list of elements that are regarded
--                as whitespace characters. The defaults are space, tab, new-line,
--                and carriage-return.
-- # ##haskeys## : an integer containing true or false. The default is true. When
-- ##true##, the ##kv_delim## values are used to separate keys from values, but
-- when ##false## it is assumed that each 'pair' is actually just a value.
--
-- Returns:
--              A **sequence**, of pairs. Each pair is in the form {key, value}.
--
-- Comments:
--
-- String representations of atoms are not converted, either in the key or value part, but returned as any regular string instead.
--
-- If ##haskeys## is ##true##, but a substring only holds what appears to be a value, the key
-- is synthesized as ##p[n]##, where ##n## is the number of the pair. See example #2.
--
-- By default, pairs can be delimited by either a comma or semi-colon ",;" and
-- a key is delimited from its value by either an equal or a colon "=:".
-- Whitespace between pairs, and between delimiters is ignored.
--
-- If you need to have one of the delimiters in the value data, enclose it in
-- quotation marks. You can use any of single, double and back quotes, which
-- also means you can quote quotation marks themselves. See example #3.
--
-- It is possible that the value data itself is a nested set of pairs. To do
-- this enclose the value in parentheses. Nested sets can nested to any level.
-- See example #4.
--
-- If a sub-list has only data values and not keys, enclose it in either braces
-- or square brackets. See example #5.
-- If you need to have a bracket as the first character in a data value, prefix
-- it with a tilde. Actually a leading tilde will always just be stripped off
-- regardless of what it prefixes. See example #6.
--
-- Example 1:
-- <eucode>
-- s = keyvalues("foo=bar, qwe=1234, asdf='contains space, comma, and equal(=)'")
-- -- s is { {"foo", "bar"}, {"qwe", "1234"}, {"asdf", "contains space, comma, and equal(=)"}}
-- </eucode>
--
-- Example 2:
-- <eucode>
-- s = keyvalues("abc fgh=ijk def")
-- -- s is { {"p[1]", "abc"}, {"fgh", "ijk"}, {"p[3]", "def"} }
-- </eucode>
--
-- Example 3:
-- <eucode>
-- s = keyvalues("abc=`'quoted'`")
-- -- s is { {"abc", "'quoted'"} }
-- </eucode>
--
-- Example 4:
-- <eucode>
-- s = keyvalues("colors=(a=black, b=blue, c=red)")
-- -- s is { {"colors", {{"a", "black"}, {"b", "blue"},{"c", "red"}}  } }
-- s = keyvalues("colors=(black=[0,0,0], blue=[0,0,FF], red=[FF,0,0])")
-- -- s is { {"colors", {{"black",{"0", "0", "0"}}, {"blue",{"0", "0", "FF"}},{"red", {"FF","0","0"}}}} }
-- </eucode>
--
-- Example 5:
-- <eucode>
-- s = keyvalues("colors=[black, blue, red]")
-- -- s is { {"colors", { "black", "blue", "red"}  } }
-- </eucode>
--
-- Example 6:
-- <eucode>
-- s = keyvalues("colors=~[black, blue, red]")
-- -- s is { {"colors", "[black, blue, red]"}  } }
-- -- The following is another way to do the same.
-- s = keyvalues("colors=`[black, blue, red]`")
-- -- s is { {"colors", "[black, blue, red]"}  } }
-- </eucode>

global function keyvalues(sequence source, object pair_delim = ";,",
                          object kv_delim = ":=", object quotes =  "\"'`",
--                        object whitespace = " \t\n\r", integer haskeys = 1)
                          object whitespace = {' ','\t','\n','\r'}, integer haskeys = 1)

sequence lEndBracket
sequence lBracketed

    source = trim(source)
    if length(source)=0 then
        return {}
    end if

    if atom(pair_delim) then    pair_delim = {pair_delim}   end if
    if atom(kv_delim)   then      kv_delim = {kv_delim}     end if
    if atom(quotes)     then        quotes = {quotes}       end if
    if atom(whitespace) then    whitespace = {whitespace}   end if

    sequence all_delim = whitespace & pair_delim & kv_delim,
             white_pair = whitespace & pair_delim
    lEndBracket   = "}])"

    sequence res = {}
    integer sdx = 1
    while sdx<=length(source) do
        -- ignore leading whitespace
        while sdx<length(source) do
            if find(source[sdx], whitespace)=0 then
                exit
            end if
            sdx += 1
        end while

        -- Get key. Ends at any of unquoted whitespace or unquoted delimiter
        string key = ""
        integer ch = 0, quote_ch = 0, bpos, was_kv = false
        if haskeys then
            while sdx<=length(source) do
                ch = source[sdx]
                if find(ch, quotes)!=0 then
                    if ch=quote_ch then
                        -- End of quoted span
                        quote_ch = 0
                        ch = -1
                    elsif quote_ch=0 then
                        -- Start of quoted span
                        quote_ch = ch
                        ch = -1
                    end if

                elsif quote_ch=0 and find(ch, all_delim)!=0 then
                    exit

                end if
                if ch>0 then
                    key &= ch
                end if
                sdx += 1
            end while

            -- ignore next whitespace
            if find(ch, whitespace)!=0 then
                sdx += 1
                while sdx<=length(source) do
                    ch = source[sdx]
                    if find(ch, whitespace)=0 then
                        exit
                    end if
                    sdx += 1
                end while
            end if
        else
            sdx -= 1        -- Put back the last char.
        end if

        sequence val = ""
        if find(ch, kv_delim)!=0  or not haskeys then

            if find(ch, kv_delim)!=0 then
                was_kv = true
            end if

            -- ignore next whitespace
            sdx += 1
            while sdx<=length(source) do
                ch = source[sdx]
                if find(ch, whitespace)=0 then
                    exit
                end if
                sdx += 1
            end while

            -- Get value. Ends at any of unquoted whitespace or unquoted delimiter
            quote_ch = 0
            ch = 0
            lBracketed = {}
            while sdx<=length(source) do
                ch = source[sdx]
                bpos = find(ch, "{[(")
                if length(lBracketed)=0 and find(ch, quotes)!=0 then
                    if ch=quote_ch then
                        -- End of quoted span
                        quote_ch = 0
                        ch = -1
                    elsif quote_ch=0 then
                        -- Start of quoted span
                        quote_ch = ch
                        ch = -1
                    end if
                elsif bpos then
                    lBracketed &= "}])"[bpos]

                elsif length(lBracketed)!=0 and ch=lBracketed[$] then
                    lBracketed = lBracketed[1..$-1]

                elsif length(lBracketed)=0 and quote_ch=0 and find(ch, white_pair)!=0 then
                    exit

                end if

                if ch>0 then
                    val &= ch
                end if
                sdx += 1
            end while

            if find(ch, whitespace)!=0  then
                -- ignore next whitespace
                sdx += 1
                while sdx<=length(source) do
                    ch = source[sdx]
                    if find(ch, whitespace)=0 then
                        exit
                    end if
                    sdx += 1
                end while
            end if

            if find(ch, pair_delim)!=0  then
                sdx += 1
                if sdx<=length(source) then
                    ch = source[sdx]
                end if
            end if
        end if

        if find(ch, pair_delim)!=0  then
            sdx += 1
        end if

        if length(val)=0
        and length(key)=0 then
            res = append(res, {})
        else

            if length(val)=0 and not was_kv then
                val = key
                key = ""
            end if

            if length(key)=0 and haskeys then
                key = sprintf("p[%d]", length(res)+1)
            end if

            if length(val)>0 then
                ch = val[1]
                bpos = find(ch, "{[(")
                if bpos>0 and val[$]="}])"[bpos] then
                    if ch='(' then
                        val = keyvalues(val[2..$-1], pair_delim, kv_delim, quotes, whitespace, haskeys)
                    else
                        val = keyvalues(val[2..$-1], pair_delim, kv_delim, quotes, whitespace, 0)
                    end if
                elsif ch='~' then
                    val = val[2..$]
                end if
            end if

            key = trim(key)
            val = trim(val)
            if length(key)=0 then
                res = append(res, val)
            else
                res = append(res, {key, val})
            end if
        end if

    end while

    return res
end function

--**
-- Formats a set of arguments in to a string based on a supplied pattern.
--
-- Parameters:
--   # ##format_pattern## : A sequence: the pattern string that contains zero or more tokens.
--   # ##arg_list## : An object: Zero or more arguments used in token replacement.
--
-- Returns:
-- A string **sequence**, the original ##format_pattern## but with tokens replaced by
-- corresponding arguments.
--
-- Comments:
-- The ##format_pattern## string contains text and argument tokens. The resulting string
-- is the same as the format string except that each token is replaced by an
-- item from the argument list.
--
-- A token has the form **##[<Q>]##**, where <Q> is are optional qualifier codes.
--
-- The qualifier. ##<Q>## is a set of zero or more codes that modify the default
-- way that the argument is used to replace the token. The default replacement
-- method is to convert the argument to its shortest string representation and
-- use that to replace the token. This may be modified by the following codes,
-- which can occur in any order.
-- |= Qualifier |= Usage                                              |
-- |  N         | ('N' is an integer) The index of the argument to use|
-- | {id}       | Uses the argument that begins with "id=" where "id" \\
--                is an identifier name.                              |
-- | %envvar%   | Uses the Environment Symbol 'envar' as an argument  |
-- |  w         | For string arguments, if capitalizes the first\\
--                letter in each word                                 |
-- |  u         | For string arguments, it converts it to upper case. |
-- |  l         | For string arguments, it converts it to lower case. |
-- |  <         | For numeric arguments, it left justifies it.        |
-- |  >         | For string arguments, it right justifies it.        |
-- |  c         | Centers the argument.                               |
-- |  z         | For numbers, it zero fills the left side.           |
-- |  :S        | ('S' is an integer) The maximum size of the\\
--                resulting field. Also, if 'S' begins with '0' the\\
--                field will be zero-filled if the argument is an integer|
-- |  .N        | ('N' is an integer) The number of digits after\\
--                 the  decimal point                                 |
-- |  +         | For positive numbers, show a leading plus sign      |
-- |  (         | For negative numbers, enclose them in parentheses   |
-- |  b         | For numbers, causes zero to be all blanks           |
-- |  s         | If the resulting field would otherwise be zero\\
--                length, this ensures that at least one space occurs\\
--                between this token's field                          |
-- |  t         | After token replacement, the resulting string up to this point is trimmed. |
-- |  X         | Outputs integer arguments using hexadecimal digits. |
-- |  B         | Outputs integer arguments using binary digits.      |
-- |  ?         | The corresponding argument is a set of two strings. This\\
--                uses the first string if the previous token's argument is\\
--                not the value 1 or a zero-length string, otherwise it\\
--                uses the second string.                             |
-- |  [         | Does not use any argument. Outputs a left-square-bracket symbol |
-- |  ,X        | Insert thousands separators. The <X> is the character\\
--                to use. If this is a dot "." then the decimal point\\
--                is rendered using a comma. Does not apply to zero-filled\\
--                fields.                         \\
--                N.B. if hex or binary output was specified, the \\
--                separators are every 4 digits otherwise they are \\
--                every three digits. |
--
-- Clearly, certain combinations of these qualifier codes do not make sense and in
-- those situations, the rightmost clashing code is used and the others are ignored.
--
-- Any tokens in the format that have no corresponding argument are simply removed
-- from the result. Any arguments that are not used in the result are ignored.
--
-- Any sequence argument that is not a string will be converted to its
-- //pretty// format before being used in token replacement.
--
-- If a token is going to be replaced by a zero-length argument, all white space
-- following the token until the next non-whitespace character is not copied to
-- the result string.
--
-- Examples:
-- <eucode>
-- format("Cannot open file '[]' - code []", {"/usr/temp/work.dat", 32})
-- -- "Cannot open file '/usr/temp/work.dat' - code 32"
--
-- format("Err-[2], Cannot open file '[1]'", {"/usr/temp/work.dat", 32})
-- -- "Err-32, Cannot open file '/usr/temp/work.dat'"
--
-- format("[4w] [3z:2] [6] [5l] [2z:2], [1:4]", {2009,4,21,"DAY","MONTH","of"})
-- -- "Day 21 of month 04, 2009"
--
-- format("The answer is [:6.2]%", {35.22341})
-- -- "The answer is  35.22%"
--
-- format("The answer is [.6]", {1.2345})
-- -- "The answer is 1.234500"
--
-- format("The answer is [,,.2]", {1234.56})
-- -- "The answer is 1,234.56"
--
-- format("The answer is [,..2]", {1234.56})
-- -- "The answer is 1.234,56"
--
-- format("The answer is [,:.2]", {1234.56})
-- -- "The answer is 1:234.56"
--
-- format("[] [?]", {5, {"cats", "cat"}})
-- -- "5 cats"
--
-- format("[] [?]", {1, {"cats", "cat"}})
-- -- "1 cat"
--
-- format("[<:4]", {"abcdef"})
-- -- "abcd"
--
-- format("[>:4]", {"abcdef"})
-- -- "cdef"
--
-- format("[>:8]", {"abcdef"})
-- -- "  abcdef"
--
-- format("seq is []", {{1.2, 5, "abcdef", {3}}})
-- -- `seq is {1.2,5,"abcdef",{3}}`
--
-- format("Today is [{day}], the [{date}]", {"date=10/Oct/2012", "day=Wednesday"})
-- -- "Today is Wednesday, the 10/Oct/2012"
-- </eucode>
--
-- See Also:
--   [[:sprintf]]
--

global function text_format(sequence format_pattern, object arg_list = {})
sequence result
integer in_token
integer tch
integer i
integer tstart
integer tend
integer cap
integer align
integer psign
integer msign
integer zfill
integer bwz
integer spacer
integer alt
integer width
integer decs
integer pos
integer argn
integer argl
integer trimming
integer hexout
integer binout
integer tsep
object prevargv
object currargv
sequence idname
object envsym
object envvar
integer sp
sequence argtext
object tempv
integer dpos
integer dist
integer bracketed

    if atom(arg_list) then
        arg_list = {arg_list}
    end if

    result = ""
    in_token = 0


    i = 0
    tstart = 0
    tend = 0
    argl = 0
    spacer = 0
    prevargv = 0
    while i<length(format_pattern) do
        i += 1
        tch = format_pattern[i]
        if not in_token then
            if tch='[' then
                in_token = 1
                tstart = i
                tend = 0
                cap = 0
                align = 0
                psign = 0
                msign = 0
                zfill = 0
                bwz = 0
                spacer = 0
                alt = 0
                width = 0
                decs = -1
                argn = 0
                hexout = 0
                binout = 0
                trimming = 0
                tsep = 0
                idname = ""
                envvar = ""
                envsym = ""
            else
                result &= tch
            end if
        else
            if tch=']' then
                in_token = 0
                tend = i

            elsif tch='[' then
                result &= tch
                while i<length(format_pattern) do
                    i += 1
                    if format_pattern[i]=']' then
                        in_token = 0
                        tstart = 0
                        tend = 0
                        exit
                    end if
                end while

            elsif tch='w'
               or tch='u'
               or tch='l' then
                cap = tch

            elsif tch='b' then
                bwz = 1

            elsif tch='s' then
                spacer = 1

            elsif tch='t' then
                trimming = 1

            elsif tch='z' then
                zfill = 1

            elsif tch='X' then
                hexout = 1

            elsif tch='B' then
                binout = 1

            elsif tch='c'
               or tch='<'
               or tch='>' then
                align = tch

            elsif tch='+' then
                psign = 1

            elsif tch='(' then
                msign = 1

            elsif tch='?' then
                alt = 1

            elsif tch=':' then
                while i<length(format_pattern) do
                    i += 1
                    tch = format_pattern[i]
                    pos = find(tch, "0123456789")
                    if pos=0 then
                        i -= 1
                        exit
                    end if
                    width = width*10+pos-1
                    if width=0 then
                        zfill = '0'
                    end if
                end while

            elsif tch='.' then
                decs = 0
                while i<length(format_pattern) do
                    i += 1
                    tch = format_pattern[i]
                    pos = find(tch, "0123456789")
                    if pos=0 then
                        i -= 1
                        exit
                    end if
                    decs = decs*10+pos-1
                end while

            elsif tch='{' then
                -- Use a named argument.

                sp = i+1
                i = sp
                while i<length(format_pattern) do
                    if format_pattern[i]='}' then
                        exit
                    end if
                    if format_pattern[i]=']' then
                        exit
                    end if
                    i += 1
                end while
                idname = trim(format_pattern[sp..i-1]) & '='
                if format_pattern[i]=']' then
                    i -= 1
                end if

                for j=1 to length(arg_list) do
--                  if begins(idname, arg_list[j]) then
                    if match(idname, arg_list[j])=1 then
                        if argn=0 then
                            argn = j
                            exit
                        end if
                    end if
                    if j=length(arg_list) then
                        idname = ""
                        argn = -1
                    end if
                end for
            elsif tch='%' then
                -- Use the environment symbol

                sp = i+1
                i = sp
                while i<length(format_pattern) do
                    if format_pattern[i]='%' then
                        exit
                    end if
                    if format_pattern[i]=']' then
                        exit
                    end if
                    i += 1
                end while
                envsym = trim(format_pattern[sp..i-1])
                if format_pattern[i]=']' then
                    i -= 1
                end if

                envvar = getenv(envsym)

                argn = -1
                if atom(envvar) then
                    envvar = ""
                end if


            elsif tch>='0'
              and tch<='9' then
                if argn=0 then
                    i -= 1
                    while i<length(format_pattern) do
                        i += 1
                        tch = format_pattern[i]
                        pos = find(tch, "0123456789")
                        if pos=0 then
                            i -= 1
                            exit
                        end if
                        argn = argn*10+pos-1
                    end while
                end if

            elsif tch=',' then
                if i<length(format_pattern) then
                    i += 1
                    tsep = format_pattern[i]
                end if

--          else
                -- ignore it

            end if

            if tend>0 then
                -- Time to replace the token.
                argtext = ""

                if argn=0 then
                    argn = argl+1
                end if
                argl = argn

                if argn<1 or argn>length(arg_list) then
                    if length(envvar)>0 then
                        argtext = envvar
                        currargv = envvar
                    else
                        argtext = ""
                        currargv = ""
                    end if
                else
                    if string(arg_list[argn]) then
                        if length(idname)>0 then
                            argtext = arg_list[argn][length(idname)+1..$]
                        else
                            argtext = arg_list[argn]
                        end if

                    elsif integer(arg_list[argn]) then
                        if bwz!=0 and arg_list[argn]=0 then
                            argtext = ""
                        elsif binout=1 then
--/**/                      argtext = sprintf("%b", arg_list[argn])                                     --/*
--/!**!/                    argtext = sq_add(reverse(int_to_bits(arg_list[argn], 32)),'0')              --/!*
                            argtext = stdseq:reverse( convert:int_to_bits(arg_list[argn], 32)) + '0'    --*/
                            for ib=1 to length(argtext) do
                                if argtext[ib]='1' then
                                    argtext = argtext[ib..$]
                                    exit
                                end if
                            end for

                        elsif hexout=0 then
                            argtext = sprintf("%d", arg_list[argn])
                            if zfill!=0 and width>0 then
                                if length(argtext)>0 then
                                    if argtext[1]='-' then
                                        if width>length(argtext) then
                                            argtext = '-' & repeat('0', width-length(argtext)) & argtext[2..$]
                                        end if
                                    else
                                        if width>length(argtext) then
                                            argtext = repeat('0', width-length(argtext)) & argtext
                                        end if
                                    end if
                                else
                                    argtext = repeat('0', width-length(argtext)) & argtext
                                end if
                            end if

                            if arg_list[argn]>0 then
                                if psign then
                                    if zfill=0 then
                                        argtext = '+' & argtext
                                    elsif argtext[1]='0' then
                                        argtext[1] = '+'
                                    end if
                                end if
                            elsif arg_list[argn]<0 then
                                if msign then
                                    if zfill=0 then
                                        argtext = '(' & argtext[2..$] & ')'
                                    else
                                        if argtext[2]='0' then
                                            argtext = '(' & argtext[3..$] & ')'
                                        else
                                            argtext = argtext[2..$] & ')'
                                        end if
                                    end if
                                end if
                            end if
                        else
                            argtext = sprintf("%x", arg_list[argn])
                            if zfill!=0 and width>0 then
                                if width>length(argtext) then
                                    argtext = repeat('0', width-length(argtext)) & argtext
                                end if
                            end if
                        end if

                    elsif atom(arg_list[argn]) then
                        if bwz!=0 and arg_list[argn]=0 then
                            argtext = ""
                        else
                            if hexout then
                                argtext = sprintf("%x", arg_list[argn])
                                if zfill!=0 and width>0 then
                                    if width>length(argtext) then
                                        argtext = repeat('0', width-length(argtext)) & argtext
                                    end if
                                end if
                            else
                                argtext = trim(sprintf("%15.15g", arg_list[argn]))
                                if zfill!=0 and width>0 then
                                    if length(argtext)>0 then
                                        if width>length(argtext) then
                                            if argtext[1]='-' then
                                                argtext = '-' & repeat('0', width-length(argtext)) & argtext[2..$]
                                            else
                                                argtext = repeat('0', width-length(argtext)) & argtext
                                            end if
                                        end if
                                    else
                                        argtext = repeat('0', width-length(argtext)) & argtext
                                    end if
                                end if
                                if arg_list[argn]>0 then
                                    if psign then
                                        if zfill=0 then
                                            argtext = '+' & argtext
                                        elsif argtext[1]='0' then
                                            argtext[1] = '+'
                                        end if
                                    end if
                                elsif arg_list[argn]<0 then
                                    if msign then
                                        if zfill=0 then
                                            argtext = '(' & argtext[2..$] & ')'
                                        else
                                            if argtext[2]='0' then
                                                argtext = '(' & argtext[3..$] & ')'
                                            else
                                                argtext = argtext[2..$] & ')'
                                            end if
                                        end if
                                    end if
                                end if
                            end if
                        end if

                    else
                        if alt!=0 and length(arg_list[argn])=2 then
                            if atom(prevargv) then
                                if prevargv!=1 then
                                    tempv = arg_list[argn][1]
                                else
                                    tempv = arg_list[argn][2]
                                end if
                            else
                                if length(prevargv)=0 then
                                    tempv = arg_list[argn][1]
                                else
                                    tempv = arg_list[argn][2]
                                end if
                            end if

                            if string(tempv) then
                                argtext = tempv
                            elsif integer(tempv) then
                                if bwz!=0 and tempv=0 then
                                    argtext = ""
                                else
                                    argtext = sprintf("%d", tempv)
                                end if

                            elsif atom(tempv) then
                                if bwz!=0 and tempv=0 then
                                    argtext = ""
                                else
                                    argtext = trim(sprintf("%15.15g", tempv))
                                end if
                            else
--/**/                          argtext = ppf(tempv)                                            --/*
                                argtext = pretty_sprint(tempv,
                                                        {2,0,1,1000,"%d","%.15g",32,127,1,0}
                                                       )                                        --*/
                            end if
                        else
--/**/                          argtext = ppf(arg_list[argn])                                   --/*
                            argtext = pretty_sprint(arg_list[argn],
                                                    {2,0,1,1000,"%d","%.15g",32,127,1,0}
                                                   )                                            --*/
                        end if
                    end if
                    currargv = arg_list[argn]
                end if


                if length(argtext)>0 then
                    if cap='u' then
                        argtext = upper(argtext)
                    elsif cap='l' then
                        argtext = lower(argtext)
                    elsif cap='w' then
                        argtext = proper(argtext)
                    elsif cap=0 then
                        -- do nothing
                        cap = cap

                    else
                        crash("logic error: 'cap' mode in format.")

                    end if

                    if atom(currargv) then
                        if find('e', argtext)=0 then
                            -- Only applies to non-scientific notation.
                            if decs!=-1 then
                                pos = find('.', argtext)
                                if pos then
                                    if decs=0 then
                                        argtext = argtext[1..pos-1]
                                    else
                                        pos = length(argtext)-pos
                                        if pos>decs then
                                            argtext = argtext[1..$-pos+decs]
                                        elsif pos<decs then
                                            argtext = argtext & repeat('0', decs-pos)
                                        end if
                                    end if
                                elsif decs>0 then
                                    argtext = argtext & '.' & repeat('0', decs)
                                end if
                            end if

                        end if
                    end if

                    if align=0 then
                        if atom(currargv) then
                            align = '>'
                        else
                            align = '<'
                        end if
                    end if

                    if atom(currargv) then
                        if tsep!=0 and zfill=0 then

                            if binout or hexout then
                                dist = 4
                            else
                                dist = 3
                            end if
                            bracketed = (argtext[1]='(')
                            if bracketed then
                                argtext = argtext[2..$-1]
                            end if
                            dpos = find('.', argtext)
                            if dpos=0 then
                                dpos = length(argtext)+1
                            else
                                if tsep='.' then
                                    argtext[dpos] = ','
                                end if
                            end if
                            while dpos>dist do
                                dpos -= dist
                                if dpos>1 then
                                    argtext = argtext[1..dpos-1] & tsep & argtext[dpos..$]
                                end if
                            end while
                            if bracketed then
                                argtext = '(' & argtext & ')'
                            end if
                        end if
                    end if

                    if width<=0 then
                        width = length(argtext)
                    end if


                    if width<length(argtext) then
                        if align='>' then
                            argtext = argtext[$-width+1..$]
                        elsif align='c' then
                            pos = length(argtext)-width
                            if remainder(pos, 2)=0 then
                                pos = pos/2
                                argtext = argtext[pos+1..$-pos]
                            else
                                pos = floor(pos/2)
                                argtext = argtext[pos+1..$-pos-1]
                            end if
                        else
                            argtext = argtext[1..width]
                        end if
                    elsif width>length(argtext) then
                        if align='>' then
                            argtext = repeat(' ', width-length(argtext)) & argtext
                        elsif align='c' then
                            pos = width-length(argtext)
                            if remainder(pos, 2)=0 then
                                pos = pos/2
                                argtext = repeat(' ', pos) & argtext & repeat(' ', pos)
                            else
                                pos = floor(pos/2)
                                argtext = repeat(' ', pos) & argtext & repeat(' ', pos+1)
                            end if

                        else
                            argtext = argtext & repeat(' ', width-length(argtext))
                        end if
                    end if
                    result &= argtext

                else
                    if spacer then
                        result &= ' '
                    end if
                end if

                if trimming then
                    result = trim(result)
                end if

                tend = 0
                prevargv = currargv
            end if
        end if
    end while

    return result
end function

--**
-- Test whether a sequence is the head of another one.
-- 
-- Parameters:
--      # ##sub_text## : an object to be looked for
--  # ##full_text## : a sequence, the head of which is being inspected.
--
-- Returns:
--              An **integer**, 1 if ##sub_text## begins ##full_text##, else 0.
--
-- Example 1:
-- <eucode>
-- s = begins("abc", "abcdef")
-- -- s is 1
-- s = begins("bcd", "abcdef")
-- -- s is 0
-- </eucode>
--
-- See Also:
--     [[:ends]], [[:head]]

global function begins(object sub_text, sequence full_text)
    integer lf = length(full_text)
    if lf=0 then return false end if
    if atom(sub_text) then
        return sub_text==full_text[1]
    end if
    integer ls = length(sub_text)
    return ls<=lf and sub_text==full_text[1..ls]
end function

