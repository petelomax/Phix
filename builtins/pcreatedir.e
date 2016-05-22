--
-- pcreatedir.e
--
--  Ripped from std/filesys.e
--

--DEV not yet documented/autoincluded
--**
-- Create a new directory.
--
-- Parameters:
--      # ##name## : a sequence, the name of the new directory to create
--      # ##mode## : on //Unix// systems, permissions for the new directory. Default is 
--        0o700 (all rights for owner, none for others).
--      # ##mkparent## : If true (default) the parent directories are also created
--        if needed. 
--
-- Returns:
--     An **integer**, 0 on failure, 1 on success.
--
-- Comments:
--      ##mode## is ignored on non-Unix platforms.
--
-- Example 1:
-- <eucode>
-- if not create_directory("the_new_folder") then
--      crash("Filesystem problem - could not create the new folder")
-- end if
-- 
-- -- This example will also create "myapp/" and "myapp/interface/" if they don't exist.
-- if not create_directory("myapp/interface/letters") then
--      crash("Filesystem problem - could not create the new folder")
-- end if
--
-- -- This example will NOT create "myapp/" and "myapp/interface/" if they don't exist.
-- if not create_directory("myapp/interface/letters",?!,0) then
--      crash("Filesystem problem - could not create the new folder")
-- end if
-- </eucode>
--
-- See Also:
--  [[:remove_directory]], [[:chdir]]

--DEV psym??
--global 
constant SLASH = iff(platform()=WINDOWS?'\\':'/')

constant lib = open_dll(iff(platform()=WINDOWS?"kernel32":""))  -- libc.so? libc.dylib?
constant xCreateDirectory = iff(platform()=WINDOWS?define_c_func(lib, "CreateDirectoryA", {C_POINTER, C_POINTER}, C_BOOL)
                                                  :define_c_func(lib, "mkdir", {C_POINTER, C_INT}, C_INT))

global function create_directory(string name, integer mode=0o700, integer mkparent=1)
atom ret
integer pos

    if length(name)=0 then
        return 1
    end if

    -- Remove any trailing slash.
    if name[$]=SLASH then
        name = name[1..$-1]
    end if

    if mkparent!=0 then
        pos = rfind(SLASH, name)
        if pos!=0 then
            ret = create_directory(name[1..pos-1], mode, mkparent)
        end if
    end if

    if platform()=LINUX then
        ret = not c_func(xCreateDirectory, {name, mode})
    elsif platform()=WINDOWS then
        ret = c_func(xCreateDirectory, {name, 0})
        if mode then end if -- get rid of not used warning
    end if

    return ret
end function

