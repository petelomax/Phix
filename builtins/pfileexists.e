--
-- pfileexists.e    --DEV rename as pfile.e?
--
--  Ripped from std/filesys.e
--

constant W = (platform()=WINDOWS)
constant SLASH = iff(W?'\\':'/')
constant lib = open_dll(iff(W?"kernel32":""))   -- libc.so? libc.dylib?
constant xGetFileAttributes = iff(W?define_c_func(lib, "GetFileAttributesA", {C_POINTER}, C_INT)
                                   :define_c_func(lib, "access", {C_POINTER, C_INT}, C_INT))
constant xMoveFile          = iff(W?define_c_func(lib, "MoveFileA", {C_POINTER, C_POINTER}, C_BOOL)
                                   :define_c_func(lib, "rename", {C_POINTER, C_POINTER}, C_INT))


--DEV not yet documented/autoincluded
--**
-- Check to see if a file exists
--
-- Parameters:
--   # ##name## :  filename to check existence of
--
-- Returns:
--   An **integer**, 1 on yes, 0 on no
--
-- Example 1:
-- <eucode>
-- if file_exists("abc.e") then
--     puts(1, "abc.e exists already\n")
-- end if
-- </eucode>
global function file_exists(string name)
    return iff(W?c_func(xGetFileAttributes, {name})>0
                :c_func(xGetFileAttributes, {name, 0})=0)
end function

--DEV not yet documented/autoincluded
--**
-- Rename a file.
-- 
-- Parameters:
--      # ##src## : a sequence, the name of the file or directory to rename.
--      # ##dest## : a sequence, the new name for the renamed file
--      # ##overwrite## : an integer, 0 (the default) to prevent renaming if destination file exists,
--                                   1 to delete existing destination file first
--
-- Returns:
--     An **integer**, 0 on failure, 1 on success.
--
-- Comments:
--  *   If ##dest## contains a path specification, this is equivalent to moving the file, as 
--      well as possibly changing its name. However, the path must be on the same drive for 
--      this to work.
-- * If ##overwrite## was requested but the rename fails, there is no guarantee that any 
--      existing destination file is preserved, deleted, corrupted, or all three.
--
-- See Also:
-- [[:move_file]], [[:copy_file]]

global function rename_file(string src, string dest, integer overwrite=0)
atom ret

    if not overwrite then
        if file_exists(dest) then
            return 0
        end if
    end if

    ret = c_func(xMoveFile, {src, dest})

    if platform()=LINUX then
        ret = not ret
    end if

    return ret
end function

--DEV not yet documented/autoincluded
--**
-- returns the directory name of a fully qualified filename.
--
-- Parameters:
--      # ##path## : the path from which to extract information
--      # ##pcd## : If not zero and there is no directory name in ##path##
--                 then "." is returned. The default (##0##) will just return
--                 any directory name in ##path##.
--
-- Returns:
--              A **sequence**, the full file name part of ##path##.
--
-- Comments:
-- The host operating system path separator is used.
--
-- Example 1:
-- <eucode>
-- fname = dirname("/opt/euphoria/docs/readme.txt")
-- -- fname is "/opt/euphoria/docs"
-- </eucode>
--
-- See Also:
--   [[:driveid]], [[:filename]], [[:pathinfo]]

global function pathname(string path)
integer stop
        
--  path = canonical_path(path)
    path = get_proper_path(path)    -- (same)
    stop = rfind(SLASH, path)
    return path[1 .. stop - 1]
end function

--DEV not yet documented/autoincluded
--**
-- returns the file name portion of a fully qualified filename.
--
-- Parameters:
--              # ##path## : the path from which to extract information
--
-- Returns:
--              A **sequence**, the file name part of ##path##.
--
-- Comments:
-- The host operating system path separator is used.
--
-- Example 1:
-- <eucode>
-- fname = filename("/opt/euphoria/docs/readme.txt")
-- -- fname is "readme.txt"
-- </eucode>
--
-- See Also:
--   [[:pathinfo]], [[:filebase]], [[:fileext]]
  
global function filename(sequence path)
integer stop
    path = get_proper_path(path)
    stop = rfind(SLASH, path)
    path = path[stop+1..$]
    return path
end function

--DEV not yet documented/autoincluded
--**
-- Return the base filename of path.
--
-- Parameters:
--      # ##path## : the path from which to extract information
--
-- Returns:
--      A **sequence**, the base file name part of ##path##.
--
-- TODO: Test
--
-- Example 1:
-- <eucode>
-- base = filebase("/opt/euphoria/readme.txt")
-- -- base is "readme"
-- </eucode>
--
-- See Also:
--     [[:pathinfo]], [[:filename]], [[:fileext]]

global function filebase(string path)
integer stop
    path = filename(path)
    stop = rfind('.', path)
    path = path[1 .. stop - 1]
    return path
end function

--DEV not yet documented/autoincluded
--**
-- Write a sequence of lines to a file.
--
-- Parameters:
--      # ##file## : an object, either a file path or the handle to an open file.
--      # ##lines## : the sequence of lines to write
--
-- Returns:
--     An **integer**, 1 on success, -1 on failure.
--
-- Errors:
--      If [[:puts]]() cannot write some line of text, a runtime error will occur.
--
-- Comments:
--  If ##file## was a sequence, the file will be closed on completion. Otherwise, it will remain open, but at end of file.
--
-- Whatever integer the lines in ##lines## holds will be truncated to its 8 lowest bits so as to fall in the 0..255 range.
--
-- Example 1:
-- <eucode>
-- if write_lines("data.txt", {"This is important data", "Goodbye"}) != -1 then
--     puts(STDERR, "Failed to write data\n")
-- end if
-- </eucode>
--
-- See Also:
--     [[:read_lines]], [[:write_file]], [[:puts]]

global function write_lines(object file, sequence lines)
integer fn

    if string(file) then
        fn = open(file, "w")
    else
        fn = file
    end if
    if fn<0 then return -1 end if

    for i=1 to length(lines) do
        puts(fn, lines[i])
        puts(fn, '\n')
    end for

    if string(file) then
        close(fn)
    end if

    return 1
end function

