--
-- installation.e
-- ==============
--  This is run from ppw.bat (via pdemo) to download/extract (eg) phix.0.8.1.1.zip, etc.
--
constant setupver = version(),
         base_url = "http://phix.x10.mx/",
         num_zips = 3   -- should match C:\Program Files (x86)\Phix\phixzip.exw

integer last_lm = 0
procedure progress(string m, sequence args={})
--DEV/SUG:
--  static integer last_lm = 0 (and some way to intentionally clear it?)
    if length(args) then
        m = sprintf(m,args)
    end if
    puts(1,m&repeat(' ',max(0,last_lm-length(m)))&"\r")
    last_lm = length(m)
end procedure

include builtins\libcurl.e
atom curl = NULL,
     pErrorBuffer

function write_callback(atom pData, integer size, integer nmemb, integer fn)
    integer bytes_written = size * nmemb
    puts(fn,peek({pData,bytes_written}))
    return bytes_written
end function
constant write_cb = call_back({'+', routine_id("write_callback")})

function curl_xferinfo_callback64(atom clientp, dltotal, dlnow, ultotal, ulnow)
--
-- On 64-bit, the int64 parameters are handled just as you might expect ...
--
    atom percentage = iff(dltotal=0?0:dlnow*100/dltotal)
    string file = peek_string(clientp)
    progress("Downloading %s:  Current=%,d,  Total=%,d  (%d%%)", {file,dlnow,dltotal,percentage})
    return 0 -- signify success
end function
constant r_xferinfo64 = routine_id("curl_xferinfo_callback64")

function curl_xferinfo_callback32(atom clientp, dltotal, dltotal_hi, dlnow, dlnow_hi,
                                                ultotal, ultotal_hi, ulnow, ulnow_hi)
--
-- ... whereas on 32-bit, parameters dltotal..ulnow are passed as int64, but call_back() grabs
--      32 bits at a time off the system stack, so we (may) need to stitch them back together.
--      (There is no loss of precision unless downloading > 9,007,199,254,740,992 byte files!)
--
    if dltotal_hi!=0 then dltotal += dltotal_hi*#100000000 end if
    if   dlnow_hi!=0 then   dlnow +=   dlnow_hi*#100000000 end if
    if ultotal_hi!=0 then ultotal += ultotal_hi*#100000000 end if
    if   ulnow_hi!=0 then   ulnow +=   ulnow_hi*#100000000 end if

    return curl_xferinfo_callback64(clientp, dltotal, dlnow, ultotal, ulnow)
end function
constant r_xferinfo32 = routine_id("curl_xferinfo_callback32")

constant xferinfo_cb = call_back({'+', iff(machine_bits()=64?r_xferinfo64:r_xferinfo32)})

procedure download(string base_url, filename)
--  string url = base_url&filename,
    string url = base_url&get_file_name(filename),
--         out = substitute(filename,"zip","tmp")
           out = substitute_all(filename,{"zip","exe"},{"tmp","tmp"})
    if filename="" then -- cleanup
        if curl!=NULL then
            curl_easy_cleanup(curl)
            curl = NULL
            free(pErrorBuffer)      
            curl_global_cleanup()
        end if
        return
    end if
    if curl=NULL then
        curl_global_init()
        curl = curl_easy_init()
        pErrorBuffer = allocate(CURL_ERROR_SIZE)
        curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, pErrorBuffer)
    end if
    integer fn = open(out,"wb")
    if fn=-1 then
        crash("Cannot open %s",{filename})
    end if
    curl_easy_setopt(curl, CURLOPT_URL, url)
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_cb)
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, fn)
    curl_easy_setopt(curl, CURLOPT_NOPROGRESS,0)
    curl_easy_setopt(curl, CURLOPT_XFERINFOFUNCTION, xferinfo_cb)
    atom pFilename = allocate_string(filename)
    curl_easy_setopt(curl, CURLOPT_XFERINFODATA, pFilename)
    curl_easy_setopt(curl, CURLOPT_FAILONERROR, true)
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, true)
--  curl_easy_setopt(curl, CURLOPT_VERBOSE, true)

    printf(1,"downloading %s\n",{url})
    integer ret = curl_easy_perform(curl)
    if ret!=CURLE_OK then
        crash("Error %d(%s) downloading file", {ret,peek_string(pErrorBuffer)})
    end if
    close(fn)
    if not rename_file(out,filename) then
        crash("Cannot rename file (%s->%s)",{out,filename})
    end if
    free(pFilename)
    printf(1, "\nFile %s saved\n", {filename})
    last_lm = 0
end procedure

include builtins\LiteZip.e

procedure extract_zip(string filename)
    atom hzip = UnzipOpenFile(filename)

    ZIPENTRY ze = new_ZIPENTRY()
    integer numitems = UnzipGetItems(hzip,ze)

    atom t1 = time()+1
    for i=1 to numitems do
        integer res = UnzipGetItem(hzip, ze, i-1)
        --DEV/SUG maybe delete .zip and try re-downloading?
        if res!=ZR_OK then ?9/0 end if
        res = UnzipItemToFile(hzip, ze)
        if res!=ZR_OK then ?9/0 end if

        if time()>t1 then
            progress("Extracting %s, file %d of %d (%2d%%)",{filename,i,numitems,(i/numitems)*100})
            t1 = time()+1
        end if
    end for
    progress("")
    UnzipClose(hzip)
end procedure

global procedure install()

    integer res, fn
    string setupexe = sprintf("phix.%s.setup.exe",{setupver}),
           setupdir = "",
           cd = current_dir(),
           sfxdir = join_path({cd,"sfx"}),
           sfxctl = join_path({sfxdir,sprintf("phix.%s.txt",{setupver})})

    --
    -- Explanation: phix.version.setup.exe extracts itself, then chdir()s to eg
    --              C:\Program Files x86\Phix and runs ppw.bat, which then runs
    --              'pw pdemo -settings', so this file (installation.e) cannot
    --              simply use command_line() to locate the setup.exe. Hence it
    --              looks in some "sensible places" for it, in the hope that it
    --              can find the other zip files it needs and avoid downloading
    --              them (which is not permitted on a server, and not possible
    --              on machines without an internet connection). On failing to
    --              find setup.exe, it just proceeds to download the zips.
    --
    sequence envset = iff(platform()=WINDOWS?{`C:\Downloads`, "ALLUSERSPROFILE", 
                                              "APPDATA", "LOCALAPPDATA", "PUBLIC",
                                              "USERPROFILE", "TEMP", "PHIXSETUP"}
                                            :{".","HOME","PHIXSETUP"})

    for i=1 to length(envset) do
        object ei = envset[i]
        if i>1 then ei = getenv(ei) end if
--envset[i] = ei -- (DEV/temp)
        if string(ei) then
            if get_file_type(join_path({ei,setupexe}))==FILETYPE_FILE then
                setupdir = ei
                exit
            end if
        end if
    end for     
    if setupdir="" then
        printf(1,"Aside: %s not found\n",{setupexe})
    else
        printf(1,"%s located in %s\n",{setupexe,setupdir})
    end if
    if get_file_type(sfxdir)!=FILETYPE_DIRECTORY then
        -- fatal: setupexe did not work properly, or wrong directory?
        crash("Error: sfx directory not found (%s), aborting",{sfxdir})
    end if

    sequence scd = split_path(cd)
    if lower(scd[$])!="phix" then
        printf(1,"\nWarning: current directory (%s) does not end in \"Phix\". Continue?(Y/N):",{cd})
        res = upper(getc(0))
        printf(1,"%c\n",res)
        if res!='Y' then sleep(1) abort(1) end if
    end if

    if get_file_type(sfxctl)!=FILETYPE_FILE then
        fn = open(sfxctl,"w")
        for i=1 to num_zips do
            printf(fn,"N:phix.%s.%d.zip\n",{setupver,i})
        end for
        close(fn)
    end if
--  download("bleurgh") -- (test failure/404)
    sequence ctrl = get_text(sfxctl,GT_LF_STRIPPED),
             filenames = {}
    for i=1 to length(ctrl) do
        if ctrl[i][1]!='Y' then
            string filename = join_path({setupdir,ctrl[i][3..$]})
            if get_file_type(filename)!=FILETYPE_FILE
            or get_file_size(filename)=0 then
                download(base_url,filename)
                filenames = append(filenames,filename)
            end if
            extract_zip(filename)
            -- mark it done:
            ctrl[i][1]='Y'
            fn = open(sfxctl,"w")
            puts(fn,join(ctrl,"\n"))
            close(fn)
        end if  
    end for
--13/12/20:
    if platform()=WINDOWS then
        -- Aside: pGUI.e allows vc runtimes to be distributed in a copy of demo/pGUI/win32|64/,
        --        whereas Phix itself does not, hence there is no chdir() or similar here.
        sequence vcr = IupCheckVCRuntime(false)
        for i=1 to length(vcr) do -- ("" returns {} if all present and correct)
            string filename = get_file_name(vcr[i]),
                   filepath = join_path({setupdir,filename}),
                   fileroot = vcr[i][1..-length(filename)-1]
            if get_file_type(filepath)!=FILETYPE_FILE
            or get_file_size(filepath)=0 then
                download(fileroot,filepath)
                filenames = append(filenames,filepath)
                printf(1,"downloaded %s\n",{filepath})
            else
                printf(1,"located %s\n",{filepath})
            end if
            puts(1,"About to install the Visual C++ Redistributable Packages for Visual Studio 2015..19 from Microsoft.\n")
            puts(1,"\n\nPress any key to launch the Microsoft installer...")
            {} = wait_key()
            puts(1,"\n\n")
--          puts(1,"\n\n  (Please check for a UAC warning hiding in the background... [grr]) \n\n")
--          integer vcres = system_exec(filepath)
--          integer vcres = system_exec(filepath&" /install /quiet /norestart")
            integer vcres = system_exec(filepath&" /install /norestart")
--          if vcres!=0 then crash("%s failed with error code %d",{filename,vcres}) end if
            if vcres!=0 then
                -- while researching '/install /quiet', I found error code 23 means it cannot
                --  install the KB2999266 update, so for problems like that this is a warning
                --  and not treated as an error
                printf(1,"warning: %s returned error code %d\n",{filepath,vcres})
                sleep(4)
            end if
        end for
    end if
    if length(filenames) then
        printf(1,"\nDelete downloaded zip files?(Y/N):")
        res = upper(getc(0))
        printf(1,"%c\n",res)
        if res='Y' then
            for i=1 to length(filenames) do
                if not delete_file(filenames[i]) then
                    crash("Error deleting file %s",{filenames[i]})
                end if
            end for
        end if
    end if      
end procedure
--abort(0)

--/*
DOC:
During installation, it may display the message "Aside: phix.0.8.0.setup.exe not found", which 
means it cannot check whether you also (manually) downloaded phix.0.8.0.1.zip etc to that same
directory, and will therefore automatically download them into the installation directory. 
Should you be attempting an offline installation, you may wish to know that it checks for it
in C:\Downloads, %ALLUSERSPROFILE%, %APPDATA%, %LOCALAPPDATA%, %PUBLIC%, %TEMP%, %USERPROFILE%, 
and %PHIXSETUP% (obviously the latter should be explicitly set as part of your installation 
process when none of the others are acceptable). On Linux it checks "."/%HOME%/%PHIXSETUP%.

The file Phix/sfx/phix.0.8.0.txt is used to determine whether additional files have already 
been successfully downloaded/extracted/deleted (using a leading Y/N: on each file).

The secondary installation may also display several other messages:
sfx directory not found - the phix.{version}.setup.exe did not complete?
current directory does not end in Phix - running in the wrong directory?
Download errors are assumed to be self explanatory.
Extract errors may (for now) require manual deletion of bad zip files.

asked:
Headers Error

Can you take a quick look at http://phix.x10.mx/phx.0.8.0.zip (7.53MB)?

I created that file using LiteZip, which can extract from it with no errors.
If open it using the 7_Zip gui, I can test/extract with no errors or warnings.
However, if I right-click and either "Extract Here" or "Extract to phx.0.8.0" then I
immediately get "Warnings: Headers Error" on the progress window, apparently exactly 
the same (blank) one I got via the gui, though it then finishes without problem.

Running 64-bit 7_Zip version 19.00 (first noticed on 16.04) on Windows 10.

--*/

--puts(1,"\ndone\n")
