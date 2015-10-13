--
-- read_lines.e
--
--  Phix implementation of read_lines (autoinclude)
--
--  does not support read_lines(0)
--

global function read_lines(object filename)
sequence lines
integer fn
    if sequence(filename) then
        fn = open(filename, "r")
    else
        fn = filename
    end if
    if fn<0 then return -1 end if

    lines = get_text(fn,GT_LF_STRIPPED)

    if sequence(filename) then
        close(fn)
    end if

    return lines
end function

