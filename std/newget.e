-- my attempt to bring get.e up to date so it matches the 
-- capabilities specified in the 4.0.x documentation.
--     slowmaker aka craig gilbert

--****
-- == Input Routines
--
-- <>
--
--

namespace stdget

include std/error.e
with trace
include std/io.e


--****
-- === Error Status Constants
-- These are returned from [[:get]] and [[:value]].

public constant GET_SUCCESS = 0,
                GET_EOF = -1,
                GET_FAIL = 1,
                GET_NOTHING = -2

constant GET_TERMINATOR = 2

constant DIGITS = "0123456789",
         HEX_DIGITS = DIGITS & "ABCDEF",
         START_NUMERIC = DIGITS & "-+.#"

enum EXACT_LENGTH_REQUIRED, PLACE_LIMIT_IS_MAX
constant IGNORE_PLACE_LIMIT = #3fffffff

constant TRUE = 1

constant NO_CHAR_YET = -2

type natural(integer x)
    return x>=0
end type

type char(integer x)
    return x>= -2 and x<=255
end type

natural input_file  -- file to be read from

object input_string -- string to be read from

natural string_next

integer first_ch_offset_this_iteration, original_string_next

char ch = NO_CHAR_YET  -- the current character

procedure get_ch()
-- set ch to the next character in the input stream (either string or file)
    if sequence(input_string) then
        if string_next<=length(input_string) then
            ch = input_string[string_next]
            string_next += 1
        else
            ch = GET_EOF
        end if
    else
        ch = getc(input_file)
        if ch!=GET_EOF then
            string_next += 1
        end if
    end if

end procedure

constant WHITE_SPACE = " \t\n\r"

function skip_blanks()
-- skip white space
-- ch is "live" at entry and exit
integer white_count = 0
    while find(ch, WHITE_SPACE) do
        get_ch()
        white_count += 1
    end while

    return white_count
end function

function chars_read()
    return (string_next-original_string_next)
end function

constant ESCAPE_CHARS =  "n'\"t\\reE0",
         ESCAPED_CHARS = "\n'\"\t\\\r\e\E\0"

function escape_char(char c)
-- return escape character
natural i

    i = find(c, ESCAPE_CHARS)
    if i=0 then
        return GET_FAIL
    else
        return ESCAPED_CHARS[i]
    end if
end function

function get_qchar()
-- get a single-quoted character
-- ch is "live" at exit
-- Note: had to beef up the 'c' var type, as the new '\u' and '\U' forms allow
-- the entry of characters with values beyond 255. - slowmaker
atom c, val = -2

    get_ch()
    c = ch
    if ch='\\' then
        get_ch()
        c = escape_char(ch)
        if c=GET_FAIL then
            switch ch do
                case 'b' then
                    get_ch()
                    val = get_bin_val(8, EXACT_LENGTH_REQUIRED)
                case 'x' then
                    get_ch()
                    val = get_hex_val(2,EXACT_LENGTH_REQUIRED)
                case 'u' then
                    get_ch()
                    val = get_hex_val(4,EXACT_LENGTH_REQUIRED)
                case 'U' then
                    get_ch()
                    val = get_hex_val(8,EXACT_LENGTH_REQUIRED)
            end switch
            if val<0 then
                return {GET_FAIL, 0}
            end if
            c = val
        end if
    elsif ch='\'' then
        return {GET_FAIL, 0}
    end if

    if val= -2 then -- didn't use the get_xxx_val() funcs, which had live ch on exit
        get_ch()
    end if
    if ch!='\'' then
        return {GET_FAIL, 0}
    else
        get_ch()
        return {GET_SUCCESS, c}
    end if
end function


function get_heredoc(sequence terminator)
-- In the process of updating get_heredoc() to ignore single leading/trailing
-- crlf's, filter out cr's, and do the indent thing, I uglied up the 
-- original get_heredoc(), which was quite clean. 
-- My apologies, but hey, it works (maybe...) -- slowmaker
--
sequence text = ""
integer ends_at = 1-length(terminator)
integer trimcount = 0, trimchar = '_'

    get_ch()
    if ch='\r' then
        get_ch()
    end if
    if ch='\n' then
        get_ch()
    end if

    while ch=trimchar do
        trimcount += 1
        get_ch()
    end while

    while ends_at<1 or not match(terminator, text, ends_at) do
        if ch=GET_EOF then
            return {GET_FAIL, 0}
        end if

        if ch!='\r' then
            text &= ch
            ends_at += 1
        end if

        if trimcount and ch='\n' then
            for i=1 to trimcount+1 do
                get_ch()
                if (ch!=' ' and ch!='\t') or ch=GET_EOF then
                    exit
                end if
            end for
        else
            get_ch()
        end if
    end while

    text = head(text, length(text)-length(terminator))
    if length(text) then
        if text[$]='\n' then
            text = text[1..$-1]
        end if
    end if

    return {GET_SUCCESS, text}
end function


function get_decimal_integer_val(integer place_limit = 3, integer length_restriction = PLACE_LIMIT_IS_MAX)
-- get value specified by decimal integer characters.
-- underscore allowed, stop on anything that is not decimal integer character or '_'.
-- ch live on return.
-- return integer value if successful
-- otherwise return -1

natural nplaces = 0
integer numeral = -1
atom accumulator = 0

    while nplaces<place_limit do
        if ch=GET_EOF then
            exit
        elsif ch!='_' then
            numeral = find(ch, DIGITS)-1
            if numeral>=0 then
                nplaces += 1
                accumulator = accumulator*10+numeral
            else
                exit
            end if
        end if
        get_ch()
    end while

    if nplaces<1 or (length_restriction=EXACT_LENGTH_REQUIRED and nplaces!=place_limit) then
        return -1
    end if

    return accumulator
end function

function get_octal_val(integer place_limit = 3, integer length_restriction = PLACE_LIMIT_IS_MAX)
-- get value specified by octal characters.
-- underscore allowed, stop on anything that is not octal character or '_'.
-- ch live on return.
-- return integer value if successful
-- otherwise return -1

natural nplaces = 0
integer numeral = -1
atom accumulator = 0

    while nplaces<place_limit do
        if ch=GET_EOF then
            exit
        elsif ch!='_' then
            numeral = find(ch, DIGITS[1..8])-1
            if numeral>=0 then
                nplaces += 1
                accumulator = accumulator*8+numeral
            else
                exit
            end if
        end if
        get_ch()
    end while

    if nplaces<1 or (length_restriction=EXACT_LENGTH_REQUIRED and nplaces!=place_limit) then
        return -1
    end if

    return accumulator
end function

function get_hex_val(integer place_limit = 2, integer length_restriction = PLACE_LIMIT_IS_MAX)
-- get value specified by hex characters.
-- underscore allowed, stop on anything that is not hex character or '_'.
-- ch live on return.
-- return integer value if successful
-- otherwise return -1

natural nplaces = 0
integer numeral = -1
atom accumulator = 0

    while nplaces<place_limit do
        if ch=GET_EOF then
            exit
        elsif ch!='_' then
            numeral = find(ch, HEX_DIGITS)-1
            if numeral<0 then
                -- case insensitive
                numeral = find(and_bits(ch, 0b1101_1111), HEX_DIGITS, 10)-1
            end if

            if numeral>=0 then
                nplaces += 1
                accumulator = accumulator*16+numeral
            else
                exit
            end if
        end if
        get_ch()
    end while

    if nplaces<1 or (length_restriction=EXACT_LENGTH_REQUIRED and nplaces!=place_limit) then
        return -1
    end if

    return accumulator
end function

function get_xstring()
-- get a string specified as hex codes, of the form x"hh hh hh"
-- whitespace allowed, hex numerals allowed
-- underscore allowed
sequence text = ""
integer char_val

    get_ch()
    if ch!='"' then
        return {GET_FAIL, 0}
    end if

    get_ch()
    while TRUE do
        if ch=GET_EOF then
            return  {GET_FAIL, 0}
        elsif ch='"' then
            get_ch()
            exit
        elsif not find(ch, WHITE_SPACE) and ch!='_' then
            char_val = get_hex_val()
            if char_val<0 then
                return  {GET_FAIL, 0}
            else
                text &= char_val
            end if
        else
            get_ch()
        end if
    end while

    return {GET_SUCCESS, text}
end function


function get_bin_val(integer place_limit = 8, integer length_restriction = PLACE_LIMIT_IS_MAX)
-- get value specified by binary characters.
-- underscore allowed, stop on anything that is not '0', '1', or '_'.
-- ch live on entry and exit.
-- return integer value if successful
-- otherwise return -1

natural nplaces = 0
integer numeral = -1, accumulator = 0

    while nplaces<place_limit do
        if ch=GET_EOF then
            exit
        elsif ch!='_' then
            numeral = find(ch, DIGITS[1..2])-1
            if numeral>=0 then
                nplaces += 1
                accumulator = accumulator*2+numeral
            else
                exit
            end if
        end if
        get_ch()
    end while

    if nplaces<1 or (length_restriction=EXACT_LENGTH_REQUIRED and nplaces!=place_limit) then
        return -1
    end if

    return accumulator
end function


function get_bstring()
-- get a string specified as binary codes, of the form b"bbbb bbbb bbbb bbbb"
-- whitespace allowed, binary numeral chars allowed
-- underscore allowed
sequence text = ""
integer char_val

    get_ch()
    if ch!='"' then
        return {GET_FAIL, 0}
    end if

    get_ch()
    while TRUE do
        if ch=GET_EOF then
            return  {GET_FAIL, 0}
        elsif ch='"' then
            get_ch()
            exit
        elsif not find(ch, WHITE_SPACE) and ch!='_' then
            char_val = get_bin_val()
            if char_val<0 then
                return  {GET_FAIL, 0}
            else
                text &= char_val
            end if
        else
            get_ch()
        end if
    end while

    return {GET_SUCCESS, text}
end function

function get_string()
-- get a double-quoted character string
-- ch is "live" at exit
sequence text
atom c, val = -2
    text = ""
    while TRUE do
        if val=-2 then  -- did NOT get extra char left dangling by get_xxxx_val()'s below
            get_ch()
        else
            val = -2
        end if
        if ch=GET_EOF or ch='\n' then
            return {GET_FAIL, 0}
        elsif ch='"' then
            get_ch()
            if length(text)=0 then ----------- and ch = '"' then
                if ch='"' then
                    return get_heredoc(`"""`)
                end if
            end if
            return {GET_SUCCESS, text}
        elsif ch='\\' then
            get_ch()
            c = escape_char(ch)
            if c=GET_FAIL then
                switch ch do
                    case 'b' then
                        get_ch()
                        val = get_bin_val(8, EXACT_LENGTH_REQUIRED)
                    case 'x' then
                        get_ch()
                        val = get_hex_val(2,EXACT_LENGTH_REQUIRED)
                    case 'u' then
                        get_ch()
                        val = get_hex_val(4,EXACT_LENGTH_REQUIRED)
                    case 'U' then
                        get_ch()
                        val = get_hex_val(8,EXACT_LENGTH_REQUIRED)
                end switch
                if val<0 then
                    return {GET_FAIL, 0}
                end if
                c = val
            end if
        else
            c = ch
        end if
        text = text & c
    end while
end function


type plus_or_minus(integer x)
    return x= -1 or x=+1
end type

constant GET_IGNORE = GET_NOTHING
function read_comment()
    if atom(input_string) then
        while ch!='\n' and ch!='\r' and ch!=-1 do
            get_ch()
        end while
        get_ch()
        if ch=-1 then
            return {GET_EOF, 0}
        else
            return {GET_IGNORE, 0}
        end if
    else
        for i=string_next to length(input_string) do
            ch = input_string[i]
            if ch='\n' or ch='\r' then
                string_next = i+1
                return {GET_IGNORE, 0}
            end if
        end for
        return {GET_EOF, 0}
    end if
end function

function read_block_comment()
    while ch!= -1 do
        while ch!='*' and ch!=-1 do
            get_ch()
        end while
        get_ch()

        if ch='/' then
            get_ch()
            skip_blanks()
            return {GET_IGNORE, 0}
        end if
    end while

    return {GET_EOF, 0}
end function

procedure get_num_ch()
    while 1 do
        get_ch()
        if ch!='_' then exit end if
    end while
end procedure

function get_number()
-- read a number or a comment
-- ch is "live" at entry and exit
plus_or_minus n_sign, e_sign
natural ndigits
integer hex_digit
atom mantissa, fraction, dec, e_mag

    n_sign = +1
    mantissa = 0
    ndigits = 0

    -- process sign
    if ch='-' then
        n_sign = -1
        get_ch()
        if ch='-' then
            return read_comment()
        end if
    elsif ch='+' then
        get_ch()
    end if

    -- get mantissa
    if ch='0' then
        get_num_ch()
        switch ch do
            case 'b', 'B' then
                --process binary integer and return
                get_num_ch()
                mantissa = get_bin_val(IGNORE_PLACE_LIMIT)
                if mantissa<0 then
                    return {GET_FAIL, 0}
                else
                    return {GET_SUCCESS, n_sign*mantissa}
                end if
            case 'd', 'D' then
                --process decimal integer and return
                get_num_ch()
                mantissa = get_decimal_integer_val(IGNORE_PLACE_LIMIT)
                if mantissa<0 then
                    return {GET_FAIL, 0}
                else
                    return {GET_SUCCESS, n_sign*mantissa}
                end if
            case 't', 'T' then
                --process octal integer and return
                get_num_ch()
                mantissa = get_octal_val(IGNORE_PLACE_LIMIT)
                if mantissa<0 then
                    return {GET_FAIL, 0}
                else
                    return {GET_SUCCESS, n_sign*mantissa}
                end if
            case 'x', 'X' then
                --process hex integer and return
                get_num_ch()
                mantissa = get_hex_val(IGNORE_PLACE_LIMIT)
                if mantissa<0 then
                    return {GET_FAIL, 0}
                else
                    return {GET_SUCCESS, n_sign*mantissa}
                end if
        end switch
    elsif ch='#' then
        -- process hex integer and return
        get_num_ch()
        mantissa = get_hex_val(IGNORE_PLACE_LIMIT)
        if mantissa<0 then
            return {GET_FAIL, 0}
        else
            return {GET_SUCCESS, n_sign*mantissa}
        end if
    end if

    -- decimal integer or floating point
    while ch>='0' and ch<='9' do
        ndigits += 1
        mantissa = mantissa*10+(ch-'0')
        get_num_ch()
    end while

    if ch='.' then
        -- get fraction
        get_num_ch()
        dec = 1
        fraction = 0
        while ch>='0' and ch<='9' do
            ndigits += 1
            fraction = fraction*10 + ch-'0'
            dec *= 10
            get_num_ch()
        end while
        mantissa += fraction/dec
    end if

    if ndigits=0 then
        return {GET_FAIL, 0}
    end if

    mantissa = n_sign*mantissa

    if ch='e' or ch='E' then
        -- get exponent sign
        e_sign = +1
        e_mag = 0
        get_num_ch()
        if ch='-' then
            e_sign = -1
            get_num_ch()
        elsif ch='+' then
            get_num_ch()
        end if
        -- get exponent magnitude
        if ch>='0' and ch<='9' then
            e_mag = ch-'0'
            get_num_ch()
            while ch>='0' and ch<='9' do
                e_mag = e_mag*10+ch-'0'
                get_num_ch()
            end while
        else
            return {GET_FAIL, 0} -- no exponent
        end if
        e_mag *= e_sign
        if e_mag>308 then
            -- rare case: avoid power() overflow
            mantissa *= power(10, 308)
            if e_mag>1000 then
                e_mag = 1000
            end if
            for i=1 to e_mag-308 do
                mantissa *= 10
            end for
        else
            mantissa *= power(10, e_mag)
        end if
    end if

    return {GET_SUCCESS, mantissa}
end function

integer leading_whitespace

function Get()
-- read a Euphoria data object as a string of characters
-- and return {error_flag, value}
-- Note: ch is "live" at entry and exit of this routine
sequence s, e
integer e1

    -- init
    skip_blanks()

    if ch= -1 then -- string is made of whitespace only
        return {GET_EOF, 0}
    end if

    while 1 do
        if find(ch, START_NUMERIC) then
            e = get_number()
            if e[1]!=GET_IGNORE then -- either a number or something illegal was read, so exit: the other goto
                return e
            end if          -- else go read next item, starting at top of loop
            skip_blanks()
            if ch=-1 or ch='}' then -- '}' is expected only in the "{--\n}" case
                return {GET_NOTHING, 0} -- just a comment
            end if

        elsif ch='/' then
            get_ch()
            if ch='*' then
                e = read_block_comment()
                if e[1]!=GET_IGNORE then
                    return e
                end if
            else
                return {GET_FAIL, 0}
            end if
        elsif ch='{' then
            -- process a sequence
            s = {}
            get_ch()
            skip_blanks()
            if ch='}' then -- empty sequence
                get_ch()
                return {GET_SUCCESS, s} -- empty sequence
            end if

            while TRUE do -- read: comment(s), element, comment(s), comma and so on till it terminates or errors out
                while 1 do -- read zero or more comments and an element
                    e = Get() -- read next element, using standard function
                    e1 = e[1]
                    if e1=GET_SUCCESS then
                        s = append(s, e[2])
                        exit  -- element read and added to result
                    elsif e1=GET_TERMINATOR then
                        if ch='}' then
                            get_ch()
                            return {GET_SUCCESS, s}
                        else
                            return {GET_FAIL, 0}
                        end if
                    elsif e1!=GET_IGNORE then
                        return e
                    -- else it was a comment, keep going
                    elsif ch='}' then
                        get_ch()
                        return {GET_SUCCESS, s} -- empty sequence
                    end if
                end while

                while 1 do -- now read zero or more post element comments
                    skip_blanks()
                    if ch='}' then
                        get_ch()
                        return {GET_SUCCESS, s}
                    elsif ch='/' then
                        get_ch()
                        if ch='*' then
                            e = read_block_comment()
                            if e[1]!=GET_IGNORE then
                                return {GET_FAIL, 0}
                            end if
                        else
                            return {GET_FAIL, 0}
                        end if
                    elsif ch!='-' then
                        exit
                    else -- comment starts after item and before comma
                        e = get_number() -- reads anything starting with '-'
                        if e[1]!=GET_IGNORE then    -- it wasn't a comment, this is illegal
                            return {GET_FAIL, 0}
                        end if
                        -- read next comment or , or }
                    end if
                end while
                if ch!=',' then
                    return {GET_FAIL, 0}
                end if
                get_ch() -- skip comma
            end while

        elsif ch='\"' then
            return get_string()
        elsif ch='`' then
            return get_heredoc("`")
        elsif ch='x' then
            return get_xstring()
        elsif ch='b' then
            return get_bstring()
        elsif ch='\'' then
            return get_qchar()
        elsif ch='$' then
            get_ch()
            skip_blanks()
            return {GET_TERMINATOR, 0}
        else
            return {GET_FAIL, 0}
        end if
    end while

end function


function get_verbose()
-- read a Euphoria data object as a string of characters
-- and return {error_flag, value, total number of characters, leading whitespace}
integer lost_whitespace = 0

    if find(ch, WHITE_SPACE) then
        lost_whitespace = 1
    elsif ch=GET_EOF then
        -- do nothing
        return {GET_EOF,0,0,0}
    end if

    leading_whitespace = skip_blanks()+lost_whitespace

    return Get() & {chars_read()+lost_whitespace, leading_whitespace}
end function

--****
-- === Answer Types

public constant GET_SHORT_ANSWER = routine_id("Get"),
                GET_LONG_ANSWER  = routine_id("get_verbose")

function get_value(object target, integer start_point, integer answer_type)
    if answer_type!=GET_SHORT_ANSWER and answer_type!=GET_LONG_ANSWER then
        error:crash("Invalid type of answer, please only use %s (the default) or %s.", {"GET_SHORT_ANSWER", "GET_LONG_ANSWER"})
    end if
    if atom(target) then -- get()
        input_file = target
        if start_point then
            -- pulled the sum out of the seek call below
            -- for debugging purposes
            start_point = io:where(target)+start_point
            if io:seek(target, start_point) then
                error:crash("Initial seek() for get() failed!")
            end if
        end if

        string_next = 1
        input_string = 0
    else
        input_string = target
        string_next = start_point+(start_point=0)
    end if

    original_string_next = string_next

    get_ch()

    object val = call_func(answer_type, {})
    if val[1]=GET_TERMINATOR then
        val[1] = GET_FAIL
    end if
    return val
end function

--****
-- === Routines
--

--**
-- Input, from an open file, a human-readable string of characters representing a Euphoria object.
-- Convert the string into the numeric value of that object.
--
-- Parameters:
-- # ##file## : an integer, the handle to an open file from which to read
-- # ##offset## : an integer, an offset to apply to file position before reading. Defaults to 0.
-- # ##answer## : an integer, either ##GET_SHORT_ANSWER## (the default) or ##GET_LONG_ANSWER##.
--
-- Returns:
-- A **sequence**, of length 2 (##GET_SHORT_ANSWER##) or 4 (##GET_LONG_ANSWER##), made of
--
-- * an integer, the return status. This is any of:
-- ** ##GET_SUCCESS## ~-- object was read successfully
-- ** ##GET_EOF## ~--     end of file before object was read completely
-- ** ##GET_FAIL## ~--    object is not syntactically correct
-- ** ##GET_NOTHING## ~-- nothing was read, even a partial object string, before end of input
-- * an object, the value that was read. This is valid only if return status is ##GET_SUCCESS##.
-- * an integer, the number of characters read. On an error, this is the point at which the
--   error was detected.
-- * an integer, the amount of initial whitespace read before the first active character was found
--
-- Comments:
-- When ##answer## is not specified, or explicitly ##GET_SHORT_ANSWER##, only the first two
-- elements in the returned sequence are actually returned.
--
-- The ##GET_NOTHING## return status will not be returned if ##answer## is ##GET_SHORT_ANSWER##.
--
-- ##get()## can read arbitrarily complicated Euphoria objects. You
-- could have a long sequence of values in braces and separated by
-- commas and comments, e.g. ##{23, {49, 57}, 0.5, -1, 99, 'A', "john"}##.
-- A single call to get() will read in this entire sequence and return its value as a result,
-- as well as complementary information.
--
-- If a nonzero offset is supplied, it is interpreted as an offset to the current file
-- position, and the file will be seek()ed there first.
--
-- ##get()## returns a 2 or 4 element sequence, like ##[[:value]]()## does:
--
-- * a status code (success/error/end of file/no value at all)
-- * the value just read (meaningful only when the status code is ##GET_SUCCESS##)
--   (optionally)
-- * the total number of characters read
-- * the amount of initial whitespace read.
--
-- Using the default value for answer, or setting it to ##GET_SHORT_ANSWER##, returns 2 elements.
-- Setting it to ##GET_LONG_ANSWER## causes 4 elements to be returned.
--
-- Each call to ##get()## picks up where the previous call left off. For instance, a series of 5
-- calls to ##get()## would be needed to read in
--
-- {{{
-- "99 5.2 {1, 2, 3} "Hello" -1"
-- }}}
--
-- On the sixth and any subsequent call to ##get()## you would see a ##GET_EOF## status. If you had
-- something like
--
-- {{{
-- {1, 2, xxx}
-- }}}
--
-- in the input stream you would see a ##GET_FAIL## error status because xxx is not a Euphoria
-- object. And seeing
--
-- {{{
-- -- something\nBut no value
-- }}}
--
-- and the input stream stops right there, you'll receive a status code of ##GET_NOTHING##,
-- because nothing but whitespace or comments was read. If you had opted for a short answer,
-- you'd get ##GET_EOF## instead.
--
-- Multiple "top-level" objects in the input stream must be
-- separated from each other with one or more "whitespace"
-- characters (blank, tab, \r or \n). At the very least, a top
-- level number must be followed by a white space from the following object.
-- Whitespace is not necessary //within// a top-level object. Comments, terminated by either
-- '\n' or '\r', are allowed anywhere inside sequences, and ignored if at the top level.
-- A call to ##get()## will read one entire top-level object, plus possibly one additional
-- (whitespace) character, after a top level number, even though the next object may have an
-- identifiable starting point.
--
-- The combination of ##[[:print]]()## and ##get()## can be used to save a
-- Euphoria object to disk and later read it back. This technique
-- could be used to implement a database as one or more large
-- Euphoria sequences stored in disk files. The sequences could be
-- read into memory, updated and then written back to disk after
-- each series of transactions is complete. Remember to write out
-- a whitespace character (using ##[[:puts]]()##) after each call to ##[[:print]]()##,
-- at least when a top level number was just printed.
--
-- The value returned is not meaningful unless you have a ##GET_SUCCESS## status.
--
-- Example 1:
-- 
-- -- If he types 77.5, get(0) would return:
-- {GET_SUCCESS, 77.5}
--
-- -- whereas gets(0) would return:
-- "77.5\n"
-- 
--
-- Example 2:
--   See ##bin\mydata.ex##
--
-- See Also:
--   [[:value]]

public function get(integer file, integer offset=0, integer answer=GET_SHORT_ANSWER)
-- Read the string representation of a Euphoria object
-- from a file. Convert to the value of the object.
-- Return {error_status, value}.
-- Embedded comments inside sequences are now supported.
    return get_value(file, offset, answer)
end function

--**
-- Read, from a string, a human-readable string of characters representing a Euphoria object.
-- Convert the string into the numeric value of that object.
--
-- Parameters:
-- # ##st## : a sequence, from which to read text
-- # ##offset## : an integer, the position at which to start reading. Defaults to 1.
-- # ##answer## : an integer, either GET_SHORT_ANSWER (the default) or GET_LONG_ANSWER.
--
-- Returns:
-- A **sequence**, of length 2 (GET_SHORT_ANSWER) or 4 (GET_LONG_ANSWER), made of
--
-- * an integer, the return status. This is any of
-- ** ##GET_SUCCESS## ~-- object was read successfully
-- ** ##GET_EOF## ~--     end of file before object was read completely
-- ** ##GET_FAIL## ~--    object is not syntactically correct
-- ** ##GET_NOTHING## ~-- nothing was read, even a partial object string, before end of input
-- * an object, the value that was read. This is valid only if return status is ##GET_SUCCESS##.
-- * an integer, the number of characters read. On an error, this is the point at which the
--   error was detected.
-- * an integer, the amount of initial whitespace read before the first active character was found
--
-- Comments:
-- When ##answer## is not specified, or explicitly ##GET_SHORT_ANSWER##, only the first two
-- elements in the returned sequence are actually returned.
--
-- This works the same as [[:get]](), but it reads from a string that you supply, rather than
-- from a file or device.
--
-- After reading one valid representation of a Euphoria object, ##value()## will stop reading
-- and ignore any additional characters in the string. For example, "36" and "36P" will
-- both give you ##{GET_SUCCESS, 36}##.
--
-- The function returns ##{return_status, value}## if the answer type is not passed or set to
-- ##GET_SHORT_ANSWER##. If set to ##GET_LONG_ANSWER##, the number of characters read and the
-- amount of leading whitespace are returned in 3rd and 4th position. The ##GET_NOTHING## return
-- status can occur only on a long answer.
--
-- Example 1:
-- 
-- s = value("12345"}
-- s is {GET_SUCCESS, 12345}
-- 
--
-- Example 2:
-- 
-- s = value("{0, 1, -99.9}")
-- -- s is {GET_SUCCESS, {0, 1, -99.9}}
-- 
--
-- Example 3:
-- 
-- s = value("+++")
-- -- s is {GET_FAIL, 0}
-- 
--
-- See Also:
--   [[:get]]

public function value(sequence st, integer start_point=1, integer answer=GET_SHORT_ANSWER)
-- Read the representation of a Euphoria object
-- from a sequence of characters. Convert to the value of the object.
-- Trailing whitespace or comments are not considered.
-- Return {error_status, value}.
-- Embedded comments inside sequence are now supported.
    return get_value(st, start_point, answer)
end function

--**
-- Perform a value() operation on a sequence, returning the value on success or
-- the default on failure.
--
-- Parameters:
--   # ##st## : object to retrieve value from. 
--       # ##def## : the value returned if ##st## is an atom or ##value(st)## fails.
--       # ##start_point## : an integer, the position in ##st## at which to start 
--          getting the value from. Defaults to 1
--
-- Returns:
-- * If ##st##, is an atom then ##def## is returned.
-- * If ##value(st)##, call is a success, then ##value()[2]##, otherwise it will return
--       the parameter #def#.
--
-- Examples:
-- 
-- object i = defaulted_value("10", 0)
-- -- i is 10
--
-- i = defaulted_value("abc", 39)
-- -- i is 39
--
-- i = defaulted_value(12, 42)
-- -- i is 42
--
-- i = defaulted_value("{1,2}", 42)
-- -- i is {1,2}
-- 
--
-- See Also:
--   [[:value]]
--

public function defaulted_value(object st, object def, integer start_point=1)
    if atom(st) then
        return def
    end if

    object result = get_value(st,start_point, GET_SHORT_ANSWER)

    if result[1]=GET_SUCCESS then
        return result[2]
    end if

    return def
end function
