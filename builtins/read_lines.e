DEAD (now in pfile.e)
--
-- read_lines.e
--
--  Phix implementation of read_lines (autoinclude)
--
--  does not support read_lines(0)
--
global function read_lines(object file)
    integer fn = iff(string(file)?open(file,"r"):file)
    if fn<0 then return -1 end if
    sequence lines = get_text(fn,GT_LF_STRIPPED)
    if string(file) then
        close(fn)
    end if
    return lines
end function

