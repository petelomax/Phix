DEAD
--
-- pcopyfile.e
-- ===========
--
--  Phix implementation of copy_file() [autoinclude]
--
--  This is a "first draft", subject to improvement!
--

integer init = 0
atom xCopyFile

procedure Init()
puts(1,"pcopyfile.e not linux\n")
    -- added 25/11/16:
    enter_cs()
    atom kernel32 = open_dll("kernel32")
    xCopyFile = define_c_func(kernel32, "CopyFileA", {C_PTR, C_PTR, C_LONG},C_LONG)
    leave_cs()
    init = 1
end procedure
--**
-- Copy a file.
--
-- Parameters:
--      # ##src## : a sequence, the name of the file or directory to copy
--      # ##dest## : a sequence, the new name or location of the file
--      # ##overwrite## : an integer; 0 (the default) will prevent an existing destination
--                       file from being overwritten. Non-zero will overwrite the
--                       destination file.
--
-- Returns:
--     An **integer**, 0 on failure, 1 on success.
--
-- Comments:
--     If ##overwrite## is true, and if dest file already exists,
--     the function overwrites the existing file and succeeds.
--
-- See Also:
-- [[:move_file]], [[:rename_file]]

global function copy_file(string src, string dest, integer overwrite=0)
--
-- Ideally src and dest should be absolute path and file names; values relative to the current
--  directory should also work, however given that the current directory is a fickle thing, I
--  have to recommend saving a current_dir() or command_line(), early doors, and using that.
--
--atom psrc
--atom pdest
integer res
--ifdef not WIN32 then
--integer in,out
--integer byte
--end ifdef

    if not init then Init() end if

--  if length(dest) then
--      if file_type(dest)=FILETYPE_DIRECTORY then
--          if dest[$]!=SLASH then
--              dest &= SLASH
--          end if
--          info = pathinfo(src)
--          dest &= info[PATH_FILENAME]
--      end if
--  end if

--ifdef WIN32 then
    res = c_func(xCopyFile, {src, dest, not overwrite})

--elsedef
--  res = 0
--
--  if file_exists(src) then
--      if overwrite or not file_exists(dest) then
--          in  = open(src, "rb")
--          out = open(dest, "wb")
--          if in!=-1 and out!=-1 then
--              while 1 do
--                  byte = getc(in)
--                  if byte=-1 then exit end if
--                  puts(out, byte)
--              end while
--              res = 1
--              close(in)
--              close(out)
--          end if
--      end if
--  end if
--
--end ifdef

    return res
end function

