--
-- builtins\file_utils.e (an autoinclude)
-- =====================
--
-- (p2js compatible routines, hand-selected simple string/number manipulatuion only)
--
include builtins\find.e

--DEV needs testing on Linux (get_proper_path)
global function get_file_name(string path)
--  if not finit then initf() end if
    path = get_proper_path(path)
    path = path[rfind(SLASH,path)+1..$]
    return path
end function

global function get_file_extension(string filename)
-- treats eg "libglfw.so.3.1" as "libglfw.so" (both yeilding "so"),
--  however "file.1" ==> "1", and "test.part.4.r01" -> "r01".
--  (ie the first example sets extension to "1" then "3" then "so",
--   and carries on looking all the way down to the first 'l'.)
-- forwardslash and backslash are handled for all platforms.
-- result is lower case (ie "TEST.EXW" and "test.exw" -> "exw")
    string extension = ""
    integer len = length(filename)
    for i=len to 1 by -1 do
        integer ch = filename[i]
        if ch='.' then
            extension = lower(filename[i+1..len])
            if length(extension) then
                bool allnum = true
                for j=length(extension) to 1 by -1 do
                    ch = extension[j]
                    if ch<'0' or ch>'9' then
                        allnum = false
                        exit
                    end if
                end for
                if not allnum then exit end if
            end if
            len = i-1
        elsif find(ch,`\/:`) then
            exit
        end if
    end for
    return extension
end function

--/*
if get_file_extension("libglfw.so.3.1")!="so" then ?9/0 end if
if get_file_extension("libglfw.so")!="so" then ?9/0 end if
if get_file_extension("file.1")!="1" then ?9/0 end if
if get_file_extension("file.r01")!="r01" then ?9/0 end if
if get_file_extension("file.part.4.r01")!="r01" then ?9/0 end if
if get_file_extension("TEST.EXW")!="exw" then ?9/0 end if
if get_file_extension("test.exw")!="exw" then ?9/0 end if
--*/

global function get_file_base(string path)
    path = get_file_name(path)
    path = path[1..find('.',path)-1]
    return path
end function

global function get_file_path(string path, bool dropslash=true)
--  if not finit then initf() end if
    path = get_proper_path(path)
    return path[1..rfind(SLASH, path)-dropslash]
end function

global function get_file_path_and_name(string filepath, bool dropslash=true)
--  if not finit then initf() end if
    filepath = get_proper_path(filepath)
    integer k = rfind(SLASH, filepath)
    string path = filepath[1..k-dropslash],
           name = filepath[k+1..$]
    return {path,name}
end function

global function get_file_name_and_path(string filepath, bool dropslash=true)
    string {path,name} = get_file_path_and_name(filepath, dropslash)
    return {name,path}
end function

--11/2/22 (removed silly "stickness")
global function file_size_k(atom size, integer width=1)
--
-- Trivial routine to convert a size in bytes to a human-readable string, such as "2GB".
--X The width setting is also "sticky", ie whatever is set becomes the new default.
--
    integer sw = max(width,1),
            s2 = iff(sw>=3?sw-2:sw),
            fdx = 0
    string fmt = sprintf("%%%d.0f%%s",sw),      -- eg "%11.0f%s" (the %s gets ""...)
           sfmt = sprintf("%%%d.0f%%s",s2),     -- eg "%9.0f%s" (this %s gets eg "KB")
           dpsfmt = sprintf("%%%d.2f%%s",s2),   -- eg "%9.2f%s" (        ""          )
           res, suffix = ""
    while fdx<=3 do
        atom rsize = round(size/1024,100)       -- (to 2 d.p.)
        if rsize<1 then exit end if
        size = rsize
        fdx += 1
        suffix = "KMGT"[fdx]&'B'
        fmt = sfmt
    end while
    if size!=trunc(size) then fmt = dpsfmt end if
    res = sprintf(fmt, {size,suffix})
    return res
end function

