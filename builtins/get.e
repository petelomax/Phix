--
-- Phix compatible version of get.e, 
--  modified for auto-include/forward references
--
--  Note this is an "inherited" include, that I don't much like.
--  Whenever I try to use it, I tend to get stuck, give up, and roll my own. 
--  However if it works for you, then fine.
--
-- Euphoria 2.4
-- Input and Conversion Routines:
-- get()
-- value()
-- wait_key()

--/* Not required for Phix (defined in psym.e)
-- error status values returned from get() and value():
global constant GET_SUCCESS = 0,
                GET_EOF = -1,
                GET_IGNORE = -2,
                GET_FAIL = 1
--*/

--constant TRUE = 1

type natural(integer x)
    return x>=0
end type

type character(integer x)
    return x>=-1 and x<=255
end type

natural input_file -- file to be read from

object input_string -- string to be read from
natural string_next = 0

character ch = 0    -- the current character

global function active_ch()
    return ch
end function

--/* Not required for Phix (defined in pfileioN.e)
constant M_WAIT_KEY = 26
global function wait_key()
-- Get the next key pressed by the user.
-- Wait until a key is pressed.
      return machine_func(M_WAIT_KEY, 0)
end function
--*/

global integer get_line_no = 0

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
        get_line_no += (ch='\n')
    end if
end procedure

global procedure skip_blanks()
-- skip white space
-- ch is "live" at entry and exit

    while find(ch, " \t\n\r") do
        get_ch()
    end while
end procedure

constant ESCAPE_CHARS = "nt'\"\\r",
         ESCAPED_CHARS = "\n\t'\"\\\r"

function escape_char(character c)
-- return escape character
    natural i = find(c, ESCAPE_CHARS)
    if i=0 then
        return GET_FAIL
    else
        return ESCAPED_CHARS[i]
    end if
end function

function get_qchar()
-- get a single-quoted character
-- ch is "live" at exit
    get_ch()
    character c = ch
    if ch='\\' then
        get_ch()
        c = escape_char(ch)
        if c=GET_FAIL then
            return {GET_FAIL, 0}
        end if
    elsif ch='\'' then
        return {GET_FAIL, 0}
    end if
    get_ch()
    if ch!='\'' then
        return {GET_FAIL, 0}
    else
        get_ch()
        return {GET_SUCCESS, c}
    end if
end function

function get_string()
-- get a double-quoted character string
-- ch is "live" at exit
    sequence text = ""
    while TRUE do
        get_ch()
        if ch=GET_EOF or ch='\n' then
            return {GET_FAIL, 0}
        elsif ch='"' then
            get_ch()
            return {GET_SUCCESS, text}
        elsif ch='\\' then
            get_ch()
            ch = escape_char(ch)
            if ch=GET_FAIL then
                return {GET_FAIL, 0}
            end if
        end if
        text = text & ch
    end while
end function

type plus_or_minus(integer x)
    return x=-1 or x=+1
end type

function read_comment()
    while ch!='\n' and ch!='\r' and ch!=-1 do
        get_ch()
    end while
    get_ch()
    if ch=-1 then
        return {GET_EOF, 0}
    else
        return {GET_IGNORE, 0}
    end if
end function

function get_number()
-- read a number
-- ch is "live" at entry and exit
    plus_or_minus n_sign = +1
    natural ndigits = 0
    atom mantissa = 0

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
    if ch='#' then
        -- process hex integer and return
        get_ch()
        while TRUE do
            integer hex_digit = find(ch,"0123456789ABCDEF")-1
            if hex_digit>=0 then
                ndigits += 1
                mantissa = mantissa*16+hex_digit
                get_ch()
            else
                if ndigits>0 then
                    return {GET_SUCCESS, n_sign*mantissa}
                else
                    return {GET_FAIL, 0}
                end if
            end if
        end while
    end if

    -- decimal integer or floating point
    while ch>='0' and ch<='9' do
        ndigits += 1
        mantissa = mantissa*10+(ch-'0')
        get_ch()
    end while

    if ch='.' then
        -- get fraction
        get_ch()
        atom dec = 1,
             fraction = 0
        while ch>='0' and ch<='9' do
            ndigits += 1
            fraction = fraction*10 + ch-'0'
            dec *= 10
            get_ch()
        end while
        mantissa += fraction/dec
    end if

    if ndigits=0 then
        return {GET_FAIL, 0}
    end if

    mantissa = n_sign*mantissa

    if ch='e' or ch='E' then
        -- get exponent sign
        integer e_sign = +1,
                e_mag = 0
        get_ch()
        if ch='-' then
            e_sign = -1
            get_ch()
        elsif ch='+' then
            get_ch()
        end if
        -- get exponent magnitude 
        if ch>='0' and ch<='9' then
            e_mag = ch-'0'
            get_ch()
            while ch>='0' and ch<='9' do
                e_mag = e_mag*10+ch-'0'
                get_ch()
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

function Get()
-- read a data object as a string of characters and return {error_flag, value}
-- Note: ch is "live" at entry and exit of this routine
    sequence e

    skip_blanks()

    if ch= -1 then -- string is made of whitespace only
        return {GET_EOF, 0}
    end if

    while 1 do
        if find(ch, "0123456789-+.#") then
            e = get_number()
            if e[1]!=GET_IGNORE then -- either a number or something illegal was read, so exit: the other goto
                return e
            end if          -- else go read next item, starting at top of loop
            skip_blanks()
            if ch=-1 or ch='}' then -- '}' is expected only in the "{--\n}" case
                return {GET_IGNORE, 0} -- just a comment
            end if

        elsif ch='{' then
            -- process a sequence
            sequence s = {}
            get_ch()
            skip_blanks()
            if ch='}' then -- empty sequence
                get_ch()
                return {GET_SUCCESS, s} -- empty sequence
            end if

            while 1 do -- read: comment(s), element, comment(s), comma and so on till it terminates or errors out
                while 1 do -- read zero or more comments and an element
                    e = Get() -- read next element, using standard function
                    integer e1 = e[1]
                    if e1=GET_SUCCESS then
                        s = append(s, e[2])
                        exit  -- element read and added to result
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
        elsif ch='\'' then
            return get_qchar()
        else
            return {GET_FAIL, 0}
        end if

    end while

end function

global function get(integer file)
-- Read the string representation of a Euphoria object 
-- from a file. Convert to the value of the object.
-- Return {error_status, value}.
    input_file = file
    input_string = 0
    get_ch()
    return Get()
end function

global function value(sequence str)
-- Read the representation of a Euphoria object
-- from a sequence of characters. Convert to the value of the object.
-- Return {error_status, value).
    input_string = str
    string_next = 1
    get_ch()
    return Get()
end function

--global function prompt_number(string prompt, sequence range={})
global function prompt_number(sequence prompt, sequence range={})
-- Prompt the user to enter a number. 
-- A range of allowed values may be specified.
    while 1 do
        puts(1, prompt)
        object answer = gets(0) -- make sure whole line is read
        puts(1, '\n')

        answer = value(answer)
        integer error_status = answer[1]
        answer = answer[2]
        if error_status!=GET_SUCCESS 
        or sequence(answer) then
            puts(1, "A number is expected - try again\n")
        elsif length(range)=2 then
            if range[1]<=answer
            and answer<=range[2] then
                return answer
            end if
            printf(1,"A number from %g to %g is expected here - try again\n",range)
        else
            return answer
        end if
    end while
end function

--global function prompt_string(string prompt)
global function prompt_string(sequence prompt)
-- Prompt the user to enter a string
    puts(1, prompt)
    object answer = gets(0)
    puts(1, '\n')
    if sequence(answer) and length(answer)>0 then
        return answer[1..length(answer)-1] -- trim the \n
    else
        return ""
    end if
end function

global function get_bytes(integer fn, integer n)
-- Return a sequence of n bytes (maximum) from an open file.
-- If n > 0 and fewer than n bytes are returned, 
-- you've reached the end of file.
-- This function is normally used with files opened in binary mode.
    string s = repeat('\0', n)
    for i=1 to n do
        integer ch = getc(fn)
        if ch=GET_EOF then
            s = s[1..i-1]
            exit
        end if
        s[i] = ch
    end for
    return s
end function
