--
-- write_file.e
--
include builtins\unicode.e

global enum
    BINARY_MODE,
    TEXT_MODE,
    UNIX_TEXT,
    DOS_TEXT

global enum
    ANSI,
    UTF,
    UTF_8,
    UTF_16,
    UTF_16BE,
    UTF_16LE,
    UTF_32,
    UTF_32BE,
    UTF_32LE,
    $

global function write_file(object file, sequence data, integer as_text = BINARY_MODE, integer encoding = ANSI, integer with_bom = 1)
integer fn, tmp
atom adr

    if as_text!=BINARY_MODE then
        -- Truncate at first Ctrl-Z
        fn = find(26, data)
        if fn then
            data = data[1 .. fn-1]
        end if
        -- Ensure last line has a line-end marker.
        if length(data)>0 then
            if data[$]!='\n' then
                data &= '\n'
            end if
        else
            data = "\n"
        end if

        if as_text=TEXT_MODE then
            -- Standardize all line endings
            data = substitute(data, "\r\n", "\n")

        elsif as_text=UNIX_TEXT then
            data = substitute(data, "\r\n", "\n")

        elsif as_text = DOS_TEXT then
            data = substitute(data, "\r\n", "\n")
            data = substitute(data, "\n", "\r\n")

        end if
    end if

    if encoding=ANSI then
--      break
            
    elsif encoding=UTF_8 then
        data = toUTF(data, utf_32, utf_8)
        if with_bom = 1 then
            data = x"ef bb bf" & data
        end if
        as_text = BINARY_MODE
            
    elsif encoding=UTF_16LE then
        data = toUTF(data, utf_32, utf_16)
        adr = allocate(length(data)*2)
        poke2(adr, data)
        data = peek({adr,length(data)*2})
        free(adr)
        if with_bom = 1 then
            data = x"ff fe" & data
        end if
        as_text = BINARY_MODE
            
    elsif encoding=UTF_16BE then
        data = toUTF(data, utf_32, utf_16)
        adr = allocate(length(data)*2)
        poke2(adr, data)
        data = peek({adr,length(data)*2})
        free(adr)
        for i=1 to length(data)-1 by 2 do
            tmp = data[i]
            data[i] = data[i+1]
            data[i+1] = tmp
        end for
        if with_bom = 1 then
            data = x"fe ff" & data
        end if
        as_text = BINARY_MODE
            
    elsif encoding=UTF_32LE then
        adr = allocate(length(data)*4)
        poke4(adr, data)
        data = peek({adr,length(data)*4})
        free(adr)
        if with_bom=1 then
            data = x"ff fe 00 00" & data
        end if
        as_text = BINARY_MODE
            
    elsif encoding=UTF_32BE then
        adr = allocate(length(data)*4)
        poke4(adr, data)
        data = peek({adr,length(data)*4})
        free(adr)
        for i=1 to length(data)-3 by 4 do
            tmp = data[i]
            data[i] = data[i+3]
            data[i+3] = tmp
            tmp = data[i+1]
            data[i+1] = data[i+2]
            data[i+2] = tmp
        end for
        if with_bom = 1 then
            data = x"00 00 fe ff" & data
        end if
        as_text = BINARY_MODE
            
    else
        -- Assume ANSI
    end if
            
            
    if sequence(file) then
        if as_text = TEXT_MODE then
            fn = open(file, "w")
        else
            fn = open(file, "wb")
        end if
    else
        fn = file
    end if
    if fn < 0 then return -1 end if

    puts(fn, data)

    if sequence(file) then
        close(fn)
    end if

    return 1
end function
