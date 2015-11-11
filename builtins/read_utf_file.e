--global 
enum
    BINARY_MODE,
    TEXT_MODE,
    UNIX_TEXT,
    DOS_TEXT

--global 
enum
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

include unicode.e
--**
-- Read the contents of a file as a single sequence of bytes.
--
-- Parameters:
--      # ##file## : an object, either a file path or the handle to an open file.
--      # ##as_text## : integer, **BINARY_MODE** (the default) assumes //binary mode// that
--                     causes every byte to be read in,
--                     and **TEXT_MODE** assumes //text mode// that ensures that
--                     lines end with just a Ctrl-J (NewLine) character,
--                     and the first byte value of 26 (Ctrl-Z) is interpreted as End-Of-File.
--      # ##encoding##: An integer. One of  ANSI, UTF, UTF_8, UTF_16, UTF_16BE,
--                     UTF_16LE, UTF_32, UTF_32BE, UTF_32LE. The default is ANSI.
--
-- Returns:
--      A **sequence**, holding the entire file. 
--
-- Comments
-- * When using BINARY_MODE, each byte in the file is returned as an element in
--   the return sequence.
-- * When not using BINARY_MODE, the file will be interpreted as a text file. This
-- means that all line endings will be transformed to a single 0x0A character and
-- the first 0x1A character (Ctrl-Z) will indicate the end of file (all data after this 
-- will not be returned to the caller.)
-- * Text files are always returned as UTF_32 encoded files.
-- * Encoding ...
-- ** ANSI: no interpretation of the file data is done. All bytes are simply returned
-- as characters.
-- ** UTF: The file data is examined to work out which UTF encoding method was used
-- to create the file. If the file starts with a valid Byte Order Marker (BOM) it can
-- quickly decide between UTF_8, UTF_16 and UTF_32. For files without a BOM, 
-- if the file is completely valid UTF_8 encoding then that is what is used. Failing
-- that, if there are no null bytes, the ANSI is assumed. Failing that, it is tested
-- for being a valid UTF_16 or UTF_32 format. As a last resort, it will be assumed to
-- be an ANSI file.
-- ** UTF_8: Any valid UTF_8 BOM is removed and the data is converted to UTF_32 
-- format before returning. This means that if it contains any invalidly encoded
-- Unicode characters, they will be ignored.
-- ** UTF_16: Any valid UTF_16 BOM is removed and the data is converted to UTF_32 
-- format before returning. This means that if it contains any invalidly encoded
-- Unicode characters, they will be ignored.
-- ** UTF_16LE: Any valid little-endian UTF_16 BOM is removed and the data is converted to UTF_32 
-- format before returning. This means that if it contains any invalidly encoded
-- Unicode characters, they will be ignored.
-- ** UTF_16BE: Any valid big-endian UTF_16 BOM is removed and the data is converted to UTF_32 
-- format before returning. This means that if it contains any invalidly encoded
-- Unicode characters, they will be ignored.
-- ** UTF_32: Any valid UTF_32 BOM is removed.
-- ** UTF_32LE: Any valid little-endian UTF_32 BOM is removed.
-- ** UTF_32BE: Any valid big-endian UTF_32 BOM is removed.
-- * If one of the UTF_32 encodings is supplied, invalid Unicode characters are 
-- not stripped out but are returned in the file data.
--
-- Example 1:
-- <eucode>
-- data = read_file("my_file.txt")
-- -- data contains the entire contents of ##my_file.txt##
-- </eucode>
--
-- Example 2:
-- <eucode>
-- fh = open("my_file.txt", "r")
-- data = read_file(fh)
-- close(fh)
--
-- -- data contains the entire contents of ##my_file.txt##
-- </eucode>
--
-- Example 3:
-- <eucode>
-- data = read_file("my_file.txt", TEXT_MODE, UTF_8)
-- -- The UTF encoded contents of ##my_file.txt## is stored in 'data' as UTF_32
-- </eucode>
--
--
-- See Also:
--     [[:write_file]], [[:read_lines]]

--public 
--function read_file(object file, integer as_text = BINARY_MODE, integer encoding = ANSI)
function read_utf_file(object file, integer as_text = BINARY_MODE, integer encoding = ANSI)
--
-- Read a whole text file into a single string, optionally converted to ansi/utf8.
-- file may be a string filename or an already open file handle.
-- as_text may be BINARY_MODE, in which case the file contents are returned verbatim, or
--                TEXT_MODE, in which case the result is converted to ansi/utf8 as 
--                           appropriate, any CRLF sequences are replaced with LF, and
--                           the first 0x1A character (Ctrl-Z) indicates the end of file 
--                           (all data after said will not be returned to the caller).
-- encoding is ignored if the file contains a valid BOM (byte order mark) at the start,
--          otherwise processing continues as if the specifed BOM had been detected.
--          The special value of UTF causes the file data to be examined to try and
--          determine which UTF encoding method was used to encode the file, whereas
--          UFT_16 and UTF_32 perform similar checks but limited to UTF_16BE/UTF_16LE
--          and UTF_32BE/UTF32_LE respectively (rather than all four).
--
integer fn
integer len
string ret
object temp
integer tmp, cz
atom addr

    if sequence(file) then
        fn = open(file, "rb")
    else
        fn = file
    end if
    if fn<0 then return -1 end if

    temp = seek(fn, -1)
    len = where(fn)
    temp = seek(fn, 0)

    ret = repeat(' ', len)
    for i = 1 to len do
        ret[i] = getc(fn)
    end for

    if sequence(file) then
        close(fn)
    end if

    -- Remove any extra -1 (EOF) characters in case file
    -- had been opened in Windows 'text mode'.
    for i=len to 1 by -1 do
        if ret[i]!=-1 then
            if i!=len then
                ret = ret[1..i]
            end if
            exit
        end if
    end for

    if as_text=BINARY_MODE then
        return ret
    end if
    
    -- Treat as a text file.

    if length(ret)>=3
    and equal(ret[1..3],{#EF,#BB,#BF}) then
        encoding = UTF8
        ret = ret[4..$]
    elsif length(ret)>=2
    and equal(ret[1..2],{#FE,#FF}) then
        encoding = UTF_16BE
        ret = ret[3..$]
    elsif length(ret)>=2
    and equal(ret[1..2],{#FF,#FE}) then
        encoding = UTF_16LE
        ret = ret[3..$]
    elsif length(ret)>=4
    and equal(ret[1..4],{#00,#00,#FE,#FF}) then
        encoding = UTF_32BE
        ret = ret[5..$]
    elsif length(ret)>=4
    and equal(ret[1..4],{#FF,#FE,#00,#00}) then
        encoding = UTF_32LE
        ret = ret[5..$]
    end if
    if encoding=UTF then
        if validate(ret, utf_8)=0 then
            encoding = UTF_8
        elsif find(0, ret)=0 then
            -- No nulls, so assume ANSI
        else
            addr = allocate(length(ret))
            poke(addr, ret)
            temp = peek2u({addr, length(ret)/2})
            if validate(temp, utf_16)=0 then
--              encoding = UTF_16LE
                ret = toUTF(temp, utf_16, utf_8)
                encoding = UTF_8
            else
                temp = peek4u({addr, length(ret)/4})
                if validate(temp, utf_32)=0 then
--                  encoding = UTF_32LE
                    ret = toUTF(temp, utf_32, utf_8)
                    encoding = UTF_8
                else
                    temp = ret
                    for i=1 to length(temp)-1 by 2 do
                        tmp = temp[i]
                        temp[i] = temp[i+1]
                        temp[i+1] = tmp
                    end for
                    poke(addr, temp)
                    temp = peek2u({addr, length(ret)/2})
                    if validate(temp, utf_16)=0 then
--                      encoding = UTF_16LE
                        ret = toUTF(temp, utf_16, utf_8)
                        encoding = UTF_8
                    else
                        temp = ret
                        for i=1 to length(temp)-3 by 4 do
                            tmp = temp[i]
                            temp[i] = temp[i+3]
                            temp[i+3] = tmp
                            tmp = temp[i+1]
                            temp[i+1] = temp[i+2]
                            temp[i+2] = tmp
                        end for
                        poke(addr, temp)
                        temp = peek4u({addr, length(ret)/4})
                        if validate(temp, utf_32)=0 then
--                          encoding = UTF_32LE
                            ret = toUTF(temp, utf_32, utf_8)
                            encoding = UTF_8
                        else
                            encoding = ANSI
                        end if
                    end if
                end if
            end if
            free(addr)
        end if
    elsif encoding=UTF_16 then
        if validate(ret, utf_16)=0 then -- is valid
            encoding = UTF_16BE
        else
            encoding = UTF_16LE -- assume little-endian
        end if
    elsif encoding=UTF_32 then
        if validate(ret, utf_32)=0 then -- is valid
            encoding = UTF_32BE
        else
            encoding = UTF_32LE -- assume little-endian
        end if
    end if

    if encoding=UTF_16BE
    or encoding=UTF_16LE then
        if encoding=UTF_16BE then
            for i=1 to length(ret)-1 by 2 do
                temp = ret[i]
                ret[i] = ret[i+1]
                ret[i+1] = temp
            end for
        end if
        addr = allocate(length(ret))
        poke(addr, ret)
        ret = peek2u({addr, length(ret)/2})
        free(adr)
        ret = toUTF(ret, utf_16, utf_8)
    elsif encoding=UTF_32BE
       or encoding=UTF_32LE then
        if encoding=UTF_32BE then
            for i=1 to length(ret)-3 by 4 do
                temp = ret[i]
                ret[i] = ret[i+3]
                ret[i+3] = temp
                temp = ret[i+1]
                ret[i+1] = ret[i+2]
                ret[i+2] = temp
            end for
        end if
        addr = allocate(length(ret))
        poke(addr, ret)
        ret = peek4u({addr, length(ret)/4})
        free(adr)
        ret = toUTF(ret, utf_32, utf_8)
    end if

    cz = find(26, ret) -- Any Ctrl-Z found?
    if cz then
        -- Ok, so truncate the file data
        ret = ret[1..cz-1]
    end if

    -- Convert Windows endings
    ret = substitute(ret, "\r\n", "\n")
    if length(ret)>0 then
        if ret[$]!='\n' then
            ret &= '\n'
        end if
    else
        ret = "\n"
    end if

    return ret
end function


