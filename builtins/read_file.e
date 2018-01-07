--
-- read_file.e
--
global constant BINARY_MODE = 1, TEXT_MODE = 2

global function read_file(object file, integer as_text = BINARY_MODE)
integer fn
object line
string text = ""
    if sequence(file) then
        fn = open(file,iff(as_text=BINARY_MODE?"rb":"r"))
    else
        fn = file
    end if
    if fn = -1 then return -1 end if
    while 1 do
        line = gets(fn)
        if atom(line) then exit end if
        text &= line
    end while
    if sequence(file) then
        close(fn)    
    end if
    return text
end function


