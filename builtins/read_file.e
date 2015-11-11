--
-- read_file.e
--

global function read_file(object file)
integer fn
object line
--sequence text = {}
string text = ""
    if sequence(file) then
        fn = open(file,"r")
    else
        fn = file
    end if
    if fn = -1 then return -1 end if
    while 1 do
        line = gets(fn)
        if atom(line) then exit end if
--      text = append(text,trim(line,"\r\n"))
        text &= line
    end while
    if sequence(file) then
        close(fn)    
    end if
    return text
end function

--/*
function read_file(sequence filename)
integer f
object line
sequence text

    f = open(filename, "rb")
    if f= -1 then
        return -1
    end if
    line = gets(f)
    text = ""
    while sequence(line) do
        text &= line
        line = gets(f)
    end while
    close(f)
    return text
end function
--*/


