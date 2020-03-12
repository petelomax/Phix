--DEV delete this crud!
--?"src/dir.e incomplete!"
-- NB: unassigned errors on purpose:
--atom xExtractAssociatedIcon, xImageList_AddIcon, xDeleteObject, xImageList_Create, xGetSystemMetrics,
--SM_CXSMICON,SM_CYSMICON,ILC_COLOR8,ILC_MASK
--, TVrecl
--, needVedb
--object void
--
-- edix\src\dir.e
-- ==============
--
--  Directory list handling
--
-- F2/F5 inoperative, now that Main has focus. Buttons? Context menu?
--
-- demo_treeviews.exw
-- fast(ish) treeviews in arwen.
-- 
-- Main features:
--  Lists all drives, and their files and directories.
--  Treeview text is held in RDS table rather than Windows.
--  Subdirectory(subtree) loading delayed until item expanded.
--  Subdirectories initially shown as empty (when they are).
--  Treeview resized to fit window.
--  Selection of previous item (does nothing if C:\Euphoria\include\dll.e not found).
--  Simple implementation of imagelists.
--  In-place editing (via F2, does not change anything on disk).
--  Reload/refresh selected directory (F5). Now uses a freelist.
--  Control F5 and/or Shift F5 now reload the entire treeview.
--  Skips load of diskettes during startup (user must press F5).
--  Ability to toggle checkbox and icon display (of limited practical value).   -- COMMENTED OUT FOR NOW.
--  Assumes you have a C: drive and a C:\Windows directory (for loading 
--   associated icons).
--
-- To Do:
--  Migrate more code to Arwen.ew ?
--  Some trivial crashes likely to remain (eg 11/06/05 in WM_LBUTTONDBLCLK I added
--   a check that treeIdx was not 0 before using it as a subscript)
--
-- Known issues:
--  When checkboxes are displayed but icons are not, the lower edge of the
--   checkbox is missing. I assume this is a win98/my config problem.
--  The 'rename' feature (F2) just modifies the local table, it does NOT call
--  moveFile or similar to affect the disk. Hence if you rename a directory
--  /before/ it (and /all/ it's subdirectories) have been expanded, then you 
--  cannot later expand them, since eg dir(C:\Winducks[\etc]) returns -1.

--/* 4.0.2
include builtins\machine.e
include builtins\dll.e
--include builtins\wildcard.e
--*/
--/**/include builtins\pcase.e as pcase

--include arwen.ew
include builtins\file.e

--with trace
--integer TVdirl
--constant DEMO = create(Window,    "TreeViews in ARWEN", 0, 0,10,10,340,350,0)
--
--constant TVdirl = create(TreeView, "", 0, DEMO,10, 10, 312, 244, TVS_EDITLABELS)
--TVdirl=0
--
--constant CHECK = create(CheckBox, "Show &CheckBoxes?", 0, DEMO, 10, 264, 170, 25, 0)
--constant ICONS = create(CheckBox, "Show &Icons?",   0, DEMO, 180, 264, 330, 25, 0)
--
--constant STATUS = create(StatusBar, "",0, DEMO, 10,10,430,350,0)

---- get the recommended sizes for small icons:
--constant cX = c_func(xGetSystemMetrics,{SM_CXSMICON}),
--       cY = c_func(xGetSystemMetrics,{SM_CYSMICON})
--
--atom himl
----    himl = c_func(xImageList_Create,{cX,cY,ILC_COLOR8+ILC_MASK,1,32})
--  himl = 0
--

--atom mem
--atom icon
--
--
--setCheck(ICONS,True)

sequence extensions,    -- for imagelist lookup
         treeItems,
         projItems,
         backItems
    extensions = {':'}  -- 0 is folder, 1 is drive.
    treeItems = {}
    projItems = {{0,"edita!?!",0,0}}
    backItems = {}

--integer freelist
--      freelist = 0


--integer lTI -- length(treeItems)

--constant tHandle=1, tText=2, tLoaded=3, tPidx=4 -- indexes to treeItems
--constant tDirNo=5, tFileNo=6                  -- extra fields on backItems
--constant tBackNo=5                                -- "" if leaf item ([tLoaded]=-1)

--DEV as tagsort (see Phix.chm)?
--/*
function caseInsensitiveSort(sequence s1,sequence s2)
--/**/  return compare(pcase:upper(s1[D_NAME]),pcase:upper(s2[D_NAME]))
--/*
        return compare(upper(s1[D_NAME]),upper(s2[D_NAME]))
--*/
end function
--*/

--/*
function loadDir(integer pidx, sequence path)
-- wrapper round dir() to add tree items.
atom parent
object d, d2
sequence ext
integer k, hasChildren
integer iconIdx
atom pWord

--DEV
    if path[length(path)]!='\\' then
        path &= '\\'
    end if

    if pidx=-1 then -- skip floppies during initial load (user must press F5)
        d = -1
        pidx = NULL
    else
        d = dir(path)
        if sequence(d)
        and equal(d[1][1],".") then
            d = d[3..length(d)]
        end if
    end if

    if sequence(d) and length(d) then
        if pidx=0 then
            -- add the root parent only once we know whether it has children
            if freelist then
                pidx = freelist
                freelist = treeItems[freelist]
                treeItems[pidx] = {0,path[1..length(path)-1],1,0}
            else
                treeItems = append(treeItems,{0,path[1..length(path)-1],1,0})
                pidx = length(treeItems)
            end if
--          parent = addTVItem(TVdirl,NULL,pidx,path[length(path)-1]=':',1) -- use drive or folder icon
--DEV improve this!
            iconIdx = path[length(path)-1]=':' -- use drive or folder icon
--          parent = addTVItem(TVdirl,NULL,pidx,iconIdx,iconIdx,1) -- use drive or folder icon
?"parent = addTVItem(TVdirl,NULL,pidx,iconIdx,iconIdx,1) -- use drive or folder icon"
            treeItems[pidx][tHandle] = parent
        else
            parent = treeItems[pidx][tHandle]
        end if
        d = custom_sort(routine_id("caseInsensitiveSort"),d)
        for i=1 to length(d) do
            if find('d',d[i][D_ATTRIBUTES]) then        -- a directory
                --
                -- delay the full load, but set the children marker now.
                --
                d2 = dir(path&d[i][D_NAME])
                hasChildren = False
                if sequence(d2) then
                    hasChildren = length(d2)>2  -- assume all subdirs start with . and ..
                end if
                if freelist then
                    lTI = freelist
                    freelist = treeItems[freelist]
                    treeItems[lTI] = {0,d[i][D_NAME],not hasChildren,pidx}
                else
                    treeItems = append(treeItems,{0,d[i][D_NAME],not hasChildren,pidx})
                    lTI = length(treeItems)
                end if
                -- if no children, mark as loaded, else mark as unloaded:
--              treeItems[lTI][tHandle] = addTVItem(TVdirl,parent,lTI,0,hasChildren) -- 0=use folder icon
                iconIdx = 0 -- 0=use folder icon
--              treeItems[lTI][tHandle] = addTVItem(TVdirl,parent,lTI,iconIdx,iconIdx,hasChildren)
?"treeItems[lTI][tHandle] = addTVItem(TVdirl,parent,lTI,iconIdx,iconIdx,hasChildren)"
                -- this is now deferred, via treeItems[i][tLoaded(=3)], to TVN_ITEMEXPANDING:
                --void = loadDir(?p,path&d[i][D_NAME])
            end if
        end for
        for i=1 to length(d) do
            if not find('d',d[i][D_ATTRIBUTES]) then    -- not a directory
--DEV get_file_extension()
--/**/          ext = pcase:lower(d[i][D_NAME])
--/*
                ext = lower(d[i][D_NAME])
--*/
                for j=length(ext) to 1 by -1 do
                    if ext[j]='.' then
--/**/                  ext = pcase:lower(ext[j+1..length(ext)])
--/*
                        ext = lower(ext[j+1..length(ext)])
--*/
                        exit
                    end if
                end for
                k = find(ext,extensions)
                if k=0 or find(ext,{"exe","ico"}) then
                    extensions = append(extensions,ext)
                    k = length(extensions)
--DEV WINDOWS only
--                  mem = allocate_StringZ(path&d[i][D_NAME])   --?IupRawStringPtr
                    string name = path&d[i][D_NAME]
                    pWord = allocate(2)
                    poke2(pWord,0)
--                  icon = c_func(xExtractAssociatedIcon,{instance(),mem,pWord})
                    icon = c_func(xExtractAssociatedIcon,{instance(),name,pWord})
                    free(pWord)
                    void = c_func(xImageList_AddIcon,{himl,icon})
                    void = c_func(xDeleteObject,{icon})
                end if
                if freelist then
                    lTI = freelist
                    freelist = treeItems[freelist]
                    treeItems[lTI] = {0,d[i][D_NAME],-1,pidx}
                else
                    treeItems = append(treeItems,{0,d[i][D_NAME],-1,pidx})
                    lTI = length(treeItems)
                end if
--              treeItems[lTI][tHandle] = addTVItem(TVdirl,parent,lTI,k,0) -- use kth icon, no children
                iconIdx = k -- use kth icon
--              treeItems[lTI][tHandle] = addTVItem(TVdirl,parent,lTI,iconIdx,iconIdx,0) -- no children
?"treeItems[lTI][tHandle] = addTVItem(TVdirl,parent,lTI,iconIdx,iconIdx,0) -- no children"
            end if
        end for
        return 1    -- hasChildren
    elsif pidx=0 then
        -- add the root parent only once we know whether it has children
        if freelist then
            lTI = freelist
            freelist = treeItems[freelist]
            treeItems[lTI] = {0,path[1..length(path)-1],1,0}
        else
            treeItems = append(treeItems,{0,path[1..length(path)-1],1,0})
            lTI = length(treeItems)
        end if
--      treeItems[lTI][tHandle] = addTVItem(TVdirl,NULL,lTI,path[length(path)-1]=':',0) -- use drive or folder icon
        iconIdx = path[length(path)-1]=':' -- use drive or folder icon
--      treeItems[lTI][tHandle] = addTVItem(TVdirl,NULL,lTI,iconIdx,iconIdx,0)
?"treeItems[lTI][tHandle] = addTVItem(TVdirl,NULL,lTI,iconIdx,iconIdx,0)"
    end if
    return 0        -- does not have Children
end function
--*/

--/*
procedure createImageList()
---- create imagelist using the recommended sizes for small icons:
atom pWord = allocate(2)
--DEV WINDOWS only...
    himl = c_func(xImageList_Create,{c_func(xGetSystemMetrics,{SM_CXSMICON}),
                                     c_func(xGetSystemMetrics,{SM_CYSMICON}),
                                     ILC_COLOR8+ILC_MASK,1,32})

    -- get a folder icon
    poke2(pWord,0)
    icon = c_func(xExtractAssociatedIcon,{instance(),`C:\WINDOWS`,pWord})
    void = c_func(xImageList_AddIcon,{himl,icon})
    void = c_func(xDeleteObject,{icon})
    -- get a drive icon
    icon = c_func(xExtractAssociatedIcon,{instance(),`C:\`,pWord})
    void = c_func(xImageList_AddIcon,{himl,icon})
    void = c_func(xDeleteObject,{icon})
    free(pWord)
end procedure
--*/

--with trace
--integer RecInit
--      RecInit = 0     -- flag indicating if loadRecoveryDirs() has been called.

--sequence RecDirList, RecDirNames

--atom recSelItem -- item to select once tree is loaded

--integer RecDir, -- limit of directory entries
--      recUsed -- used part of backItems

--procedure loadRecSubDirs(atom pItem, integer pIdx, sequence parentDir)
--sequence thisDir
--integer slashChar
--atom hItem
--  thisDir = loadSet[rIdx]
--  thisDir = thisDir[length(parentDir)+1..length(thisDir)]
--  slashChar = find('\\',thisDir)
--  if slashChar and slashChar<length(thisDir) then
--      thisDir = thisDir[1..slashChar]
--      rUsed += 1
----    hItem = addTVItem(TVrecl,pItem,rUsed,0,1)    -- id,parent,idx,0,children
--      hItem = addTVItem(TVrecl,pItem,rUsed,0,0,1)  -- id,parent,idx,0,0,children
--      backItems[rUsed] = {hItem,thisDir,0,pIdx}
--      pIdx = rUsed
--      while rIdx<=length(loadSet) and match(parentDir&thisDir,loadSet[rIdx]) = 1 do
--          loadRecSubDirs(hItem,pIdx,parentDir&thisDir)
--          rIdx += 1
--      end while
--  else    
--      rUsed += 1
----    hItem = addTVItem(TVrecl,pItem,rUsed,0,1)    -- id,parent,idx,0,children
--      hItem = addTVItem(TVrecl,pItem,rUsed,0,0,1)  -- id,parent,idx,0,0,children
--      backItems[rUsed] = {hItem,thisDir,0,pIdx}
--  end if
--  rIdx += 1
--end procedure

--DEV one hit, one multiple of 32....
--/*
procedure bulkUpbackItems(integer k)
    while length(backItems)<k do
        -- bulk up table to next multiple of 32
        backItems &= repeat(0,32)
    end while
end procedure
--*/

--with trace
--constant ANY=1

--integer lRDcount lRDcount=1
--/*
procedure loadRecoveryDetails(integer treeIdx, bool bSelect)
?{"loadRecoveryDetails",treeIdx,bSelect}
integer fileNo, backNo, backMod, f
sequence fileSet, backName, backExtension
atom hParent, hItem
--, selItem
--trace(1)
--setText(Main,sprintf("Loading Recovery Details...[%d]",lRDcount))
--lRDcount += 1
--  recSelItem = 0
    fileNo = backItems[treeIdx][tFileNo]
    fileSet = getBackupSet(fileNo)
    if length(fileSet) then
--      void = sendMessage(TVrecl,WM_SETREDRAW,0,0)
        IupSetAttribute(TVrecl,"AUTOREDRAW","NO")
        hParent = backItems[treeIdx][tHandle]
        bulkUpbackItems(recUsed+length(fileSet))
--      backExtension = '.'&getFileExtension(backItems[treeIdx][tText])
        backExtension = '.'&get_file_extension(backItems[treeIdx][tText])
        backMod = 0
        for i=length(fileSet) to 1 by -1 do
            -- check file still exists...
            backNo = fileSet[i]
            backName = sprintf("%09d",backNo)
            if backName[2]='1' then
                backName[3] = backName[3]+'A'-'0'
            end if
            backName = backName[1]&backName[3..9]&backExtension
            f = open(initialcurrentdir&`backup\`&backName,"r")
            if f!=-1 then
                close(f)
--          if find(backName,RecDirNames) then
                recUsed += 1
--              hItem = addTVItem(TVrecl,hParent,recUsed,0,0)    -- id,parent,idx,0,children
--              hItem = addTVItem(TVrecl,hParent,recUsed,0,0,0)  -- id,parent,idx,0,0,children
?"hItem = addTVItem(TVrecl,hParent,recUsed,0,0,0)  -- id,parent,idx,0,0,children"
--DEV (4/12/13) do not understand how this could happen, but twice it has (an ioob on backItems),
--              despite the bulpUpbackItems call immediately above...
--if recUsed<=length(backItems) then
                backItems[recUsed] = {hItem,backName,-1,0,backNo}   -- leaf node
--else
--              backItems = append(backItems,{hItem,backName,-1,0,backNo})
--end if
                if bSelect then
--DEV??
                    recSelItem = hItem
                    bSelect = false
                end if
            else
                backMod = 1
                fileSet = fileSet[1..i-1]&fileSet[i+1..length(fileSet)]
            end if
        end for
        if backMod then
            rewriteBackupSet(fileNo,fileSet)
        end if
--      void = sendMessage(TVrecl,WM_SETREDRAW,1,0)
        IupSetAttribute(TVrecl,"AUTOREDRAW","YES")
    end if
end procedure
--*/

--/*
procedure loadRecoveryFiles(integer treeIdx)
?{"loadRecoveryFiles",treeIdx}
integer dirno
sequence fileSet
atom hParent, hItem
integer selIdx
integer fileNo, hasChildren
--setText(Main,"Loading Recovery Files...")
    selIdx = 0
--  void = sendMessage(TVrecl,WM_SETREDRAW,0,0)
    IupSetAttribute(TVrecl,"AUTOREDRAW","NO")
    dirno = backItems[treeIdx][tDirNo]
    fileSet = getDirFiles(dirno,0)
    hParent = backItems[treeIdx][tHandle]
    bulkUpbackItems(recUsed+length(fileSet))
    for i=1 to length(fileSet) do
        recUsed += 1
        fileNo = fileSet[i][3]
        hasChildren = (length(getBackupSet(fileNo))!=0)
--      hItem = addTVItem(TVrecl,hParent,recUsed,0,0,hasChildren)  -- id,parent,idx,0,0,children
?"hItem = addTVItem(TVrecl,hParent,recUsed,0,0,hasChildren)  -- id,parent,idx,0,0,children"
        backItems[recUsed] = {hItem,fileSet[i][2],0,0,dirno,fileNo}
--      if usegpp then
            if currfile and equal(fileSet[i][2],filenames[currfile]) then
                selIdx = recUsed
                recSelItem = hItem
            end if
--      else
--          if currfile and equal(fileSet[i][2],lower(filenames[currfile])) then
--              selIdx = recUsed
--              recSelItem = hItem
--          end if
--      end if
    end for
    if selIdx and not backItems[selIdx][tLoaded] then
--loadingRecDetails = 1
--      void = sendMessage(TVrecl,TVM_SELECTITEM,TVGN_CARET,backItems[selIdx][tHandle])
--loadingRecDetails = 0
        backItems[selIdx][tLoaded] = 1
        loadRecoveryDetails(selIdx,1)
    end if
    if recSelItem then
--loadingRecDetails = 1
--      void = sendMessage(TVrecl,TVM_SELECTITEM,TVGN_CARET,recSelItem)
--DEV use IupTreeGetId?
        IupSetInt(TVrecl,"VALUE",recSelItem)
--loadingRecDetails = 0
    end if
--  void = sendMessage(TVrecl,WM_SETREDRAW,1,0)
    IupSetAttribute(TVrecl,"AUTOREDRAW","YES")
end procedure
--*/


--/*
procedure loadRecoveryDirs()
atom hItem
sequence loadSet
integer dirno
integer selIdx
--trace(1)
--setText(Main,"Loading Recovery Dirs...")
    selIdx = 0
    recSelItem = 0
--RecDirList = dir(initialcurrentdir&`backup\`)
--RecDirNames = repeat(0,length(RecDirList))
--for i=1 to length(RecDirList) do
--  RecDirNames[i] = RecDirList[i][D_NAME]
--end for
--  void = sendMessage(TVrecl,WM_SETREDRAW,0,0)
    IupSetAttribute(TVrecl,"AUTOREDRAW","NO")
    void = getDir(1)    -- ensures knownDirectories is loaded [DEV]
    backItems = repeat(0,length(knownDirectories))
    RecDir = 0
    for i=length(knownDirectories) to 1 by -1 do
        if sequence(knownDirectories[i]) then
            RecDir = i
            exit
        end if
    end for
    loadSet = sort(knownDirectories[1..RecDir])
    for i=1 to RecDir do
        dirno = find(loadSet[i],knownDirectories)
--      hItem = addTVItem(TVrecl,0,i,0,getDirFiles(dirno,ANY))   -- id,parent,idx,0,children
--      hItem = addTVItem(TVrecl,0,i,0,0,getDirFiles(dirno,ANY))     -- id,parent,idx,0,0,children
?"hItem = addTVItem(TVrecl,0,i,0,0,getDirFiles(dirno,ANY))"  -- id,parent,idx,0,0,children
        backItems[i] = {hItem,loadSet[i],0,0,dirno}
--      if usegpp then
            if currfile and equal(loadSet[i],filepaths[currfile]) then
                selIdx = i
                recSelItem = hItem
            end if
--      else
--          if currfile and equal(loadSet[i],upper(filepaths[currfile])) then
--              selIdx = i
--              recSelItem = hItem
--          end if
--      end if
    end for
    recUsed = RecDir
    if selIdx and not backItems[selIdx][tLoaded] then
--loadingRecDetails = 1
--      void = sendMessage(TVrecl,TVM_SELECTITEM,TVGN_CARET,backItems[selIdx][tHandle])
--loadingRecDetails = 0
        backItems[selIdx][tLoaded] = 1
        loadRecoveryFiles(selIdx)
    end if
--  void = sendMessage(TVrecl,WM_SETREDRAW,1,0)
    IupSetAttribute(TVrecl,"AUTOREDRAW","YES")
--  setEnable(TVrecdel,False)
end procedure
--*/

--include builtins\ppp.e
--/*
procedure loadAllDrives()
--loadDir(NULL,`C:\Euphoria`)
--loadDir(NULL,"C:")
--atom buffer
--integer buflen
--sequence onedrive

    if himl=0 then createImageList() end if

    -- attach imagelist to the treeview
--  void = sendMessage(TVdirl,TVM_SETIMAGELIST,TVSIL_NORMAL,himl)
?"void = sendMessage(TVdirl,TVM_SETIMAGELIST,TVSIL_NORMAL,himl)"

--DEV WINDOWS... (new builtin, get_logical_drives... [just return {{"/",?}} on linux?])
--/*
    buflen = c_func(xGetLogicalDriveStrings,{0,NULL})
    buffer = allocate(buflen)
    void = c_func(xGetLogicalDriveStrings,{buflen,buffer})
    while 1 do
        onedrive = peek_string(buffer)
        buflen = length(onedrive)
        if buflen=0 then exit end if
--      if c_func(xGetDriveType,{buffer})=DRIVE_REMOVABLE then  -- skip floppies (user must key F5)
--          void = loadDir(-1,upper(onedrive[1..buflen-1]))
--      else
--          void = loadDir(NULL,upper(onedrive[1..buflen-1]))
--      end if
        integer pidx = iff(c_func(xGetDriveType,{buffer})=DRIVE_REMOVABLE?-1:NULL)  -- skip floppies (user must key F5)
        void = loadDir(pidx,upper(onedrive[1..buflen-1]))
        buffer += buflen+1  -- skip trailing/separating nulls
    end while
--*/
    sequence drives = get_logical_drives()
    for i=1 to length(drives) do
        string onedrive
        integer drivetype
        {onedrive,drivetype} = drives[i]
        integer pidx = iff(drivetype=DRIVE_REMOVABLE?-1:NULL)   -- skip floppies (user must key F5)
        void = loadDir(pidx,upper(onedrive[1..-2]))
    end for
end procedure
--*/

--integer TVinit    -- flag indicating if loadAllDrives() has been called.
--      TVinit = 0

--integer PRinit    --DEV
--      PRinit = 0

global integer resetPrev
               resetPrev = 1

--/*
function getTreeText(integer treeIdx, integer fullpath)
-- gets the text of the treeview item, optionally returning the full tree path.
-- If fullpath is 0, the result is eg "ascii.bat";
-- if fullpath is 1, the result is eg `C:\Euphoria\bin\ascii.bat`;
-- if fullpath is 2, the result is eg {"C:","Euphoria","bin","ascii.bat"}
--  (the last case is more useful for trees which are not file directories)
sequence text
    if fullpath then
        if fullpath=1 then
            text = treeItems[treeIdx][tText]
            while 1 do
                treeIdx = treeItems[treeIdx][tPidx]
                if treeIdx=0 then exit end if
                text = treeItems[treeIdx][tText]&'\\'&text
            end while
            return text
        else -- fullpath=2
            text = {treeItems[treeIdx][tText]}
            while 1 do
                treeIdx = treeItems[treeIdx][tPidx]
                if treeIdx=0 then exit end if
                text = prepend(text,treeItems[treeIdx][tText])
            end while

            return text
        end if
    end if
    return treeItems[treeIdx][tText]
end function
--*/

--/*
procedure delTree(integer treeIdx)
    for i=1 to length(treeItems) do
        if sequence(treeItems[i]) and treeItems[i][tPidx]=treeIdx then
--          printf(1,"Deleting %s\n",{treeItems[i][tText]})
            delTree(i)
--          deleteTVItem(TVdirl, treeItems[i][tHandle])
?"deleteTVItem(TVdirl, treeItems[i][tHandle])"
            treeItems[i] = freelist
            freelist = i
        end if
    end for
end procedure
--*/

--/*
procedure purgefreelist()
-- Shrink treeItems as much as possible, but don't go mad.
-- This just makes sure that if eg none of the last 1000 entries
-- are actually used, we reclaim memory; but if items 1 and 10,0037
-- are the only two in use, it will leave the freelist with 10,0035
-- spare slots. (Nothing still in use is ever moved.)
integer newmax,k, flscan
    --
    -- work out new extent of treeItems
    --
    newmax = 0
    for i=length(treeItems) to 1 by -1 do
        if sequence(treeItems[i]) then
            newmax = i
            exit
        end if
    end for
    --
    -- Remove all entries at the start of the freelist 
    --  which are past the new extent.
    --  (eg if newmax is 4 and freelist is 10,3,7,1 then
    --   set freelist to 3, as in {3,7,1}).
    --
    while freelist>newmax do
        freelist = treeItems[freelist]
    end while
    --
    -- Remove any embedded entries in the remaining 
    --  freelist which refer past the new extent
    --  (eg if newmax is 4 and freelist is 3,7,1 then
    --   set freelist to 3,1, ie re-link the chain to
    --   skip over each too-high slot we find)
    --
    flscan = freelist
    while flscan do
        k = treeItems[flscan]
        if k>newmax then
            treeItems[flscan] = treeItems[k]
        else
            flscan = k
        end if
    end while
    --
    -- and finally shrink the table
    --
    treeItems = treeItems[1..newmax]
end procedure
--*/

--
-- Select a previous item (does nothing if not found):
--
--/*
procedure selectPreviousItem(sequence previtem)
sequence this
integer previdx, pidx, loaded
atom hItem
    previdx = 1
    --
    -- Use a while loop rather than a for loop as  
    --  treeItems may get extended by loadDir.
    --
    hItem = 0
    pidx = 0
    previtem = upper(previtem)
    while previdx<=length(treeItems) do
        if sequence(treeItems[previdx]) and treeItems[previdx][tPidx]=pidx then
            this = upper(treeItems[previdx][tText])
            loaded = treeItems[previdx][tLoaded]
            if (loaded=-1 and equal(this,previtem))
            or (loaded!=-1 and match(this,previtem)=1) then
                pidx = previdx
--              loaded = treeItems[previdx][tLoaded]
                hItem = treeItems[previdx][tHandle]
                if loaded=-1 then exit end if       -- a leaf node
                if length(previtem)=length(this) then exit end if -- or directory selected
                if not loaded then  -- subdir needs loading
                    treeItems[previdx][tLoaded] = 1
                    void = loadDir(previdx,getTreeText(previdx,1))
                end if
                -- chop target by dir and trailing '\'
                previtem = previtem[length(this)+2..length(previtem)]
            end if
        end if
        previdx += 1
    end while
    if hItem then
--      void = sendMessage(TVdirl,TVM_SELECTITEM,TVGN_CARET,hItem)
?"void = sendMessage(TVdirl,TVM_SELECTITEM,TVGN_CARET,hItem)"
    end if
end procedure
--*/

--selectPreviousItem(`C:\Euphoria\include\dll.e`)

-- Project handling:

sequence lastfile, lastpath
         lastfile = ""
integer lastshow
        lastshow = 0

-- read by eadir.ew & eaqj.ew:
global sequence currProjFileSet
                currProjFileSet = {}

without trace
--with trace
global function resetProject(integer show)
-- returns true if the project details need to be updated
-- show is zero from background processing, 1 if controlling the display;
-- --do not return False if the details have changed since last (1) call.
sequence lp, lf, cpfs
    if currfile=0 then return 0 end if
    lp = filepaths[currfile]
    lf = filenames[currfile]
--  if usegpp then
--      if needVedb then
--          lp = upper(lp)
--          lf = lower(lf)
--      end if
--  else
--      lp = upper(lp)
--      lf = lower(lf)
--  end if
    --  if not rebuildProjectSet and (show>=lastshow) then
    if not rebuildProjectSet and (show=0 or lastshow=1) then
        if equal(lf,lastfile) and equal(lp,lastpath) then return 0 end if
        if inProjectSet(lp,lf,currProjFileSet) then return 0 end if
    end if
    rebuildProjectSet = 0
    cpfs = getProjectSet(lp,lf)
    if not inProjectSet(lp,lf,cpfs) then return 0 end if    -- oops, missing??
    lastshow = show
    lastpath = lp
    lastfile = lf
    currProjFileSet = cpfs
    return 1
end function

--/*
global integer dTrackMenu   -- TrackMenu() in eamenus.ew

integer loadingRecDetails
        loadingRecDetails = 0

integer alreadyResettingProject     -- prevent looping
        alreadyResettingProject = 0

integer qCheckbI
        qCheckbI = 0

integer justDblClick
        justDblClick = 0


object dbg
without trace
--include builtins\ppp.e
function treeHandler(integer id, integer msg, atom wParam, object lParam)
atom hItem
integer treeIdx
--sequence rect
sequence text
sequence f1, f2, textsave   -- for file rename
--atom style, state
--integer flag
integer hasChildren
atom parent
integer oldcurrfile
atom TVI
--trace(3)
    if msg=WM_NOTIFY then
        if wParam=TVN_SELCHANGED        -- selection changed, or
        or wParam=NM_CUSTOMDRAW then    -- occurs after checkbox toggled (and others)
            if id=TVrecl then
                treeIdx = getIndex(TVrecl)
                if treeIdx and treeIdx<=length(backItems) and sequence(backItems[treeIdx]) then
--trace(1)
                    if qCheckbI then
                        if qCheckbI<=length(backItems) then
                            if not isTVChecked(TVrecl,backItems[qCheckbI][tHandle]) then
                                qCheckbI = 0
                            end if
                        else
                            qCheckbI = 0
                        end if
                    end if
                    if qCheckbI=0 then
                        if isTVChecked(TVrecl,backItems[treeIdx][tHandle]) then
--                          setEnable(TVrecdel,True)
                            qCheckbI = treeIdx
                        else
--                          setEnable(TVrecdel,False)
                            for i=1 to length(backItems) do
                                if atom(backItems[i]) then exit end if
                                if isTVChecked(TVrecl,backItems[i][tHandle]) then
--                                  setEnable(TVrecdel,True)
                                    qCheckbI = i
                                    exit
                                end if
                            end for
                        end if
                    end if
                end if
            elsif id=TVprjl then
                treeIdx = getIndex(TVprjl)
                if treeIdx and treeIdx<=length(currProjFileSet) then
                    setText(TVprjt,currProjFileSet[treeIdx][3])
                end if
            end if
        elsif wParam=TVN_GETDISPINFO then
            if and_bits(peek4s(lParam+TVDISPINFO_TVITEM+TVITEM_mask),TVIF_TEXT) then
                treeIdx = peek4s(lParam+TVDISPINFO_TVITEM+TVITEM_lParam)
                text = "???***???"
                if id=TVdirl then
                    if treeIdx<=length(treeItems) then
                        text = treeItems[treeIdx][tText]
                    end if
                elsif id=TVprjl then
                    if treeIdx<=length(currProjFileSet) then
                        text = currProjFileSet[treeIdx][2]
                    end if
                elsif id=TVrecl then
                    if RecInit and treeIdx<=length(backItems) then
--DEV crash here 05/05/06:
--  and again 15/11/06...:
                        dbg = backItems[treeIdx][tText]
                        if sequence(dbg) then
                            text = dbg
                        else
                            text = "??!!??"
                        end if
                    end if
                else
                    ?9/0    -- unknown control id
                end if
                poke(peek4s(lParam+TVDISPINFO_TVITEM+TVITEM_pszText),text&0)
            end if
        elsif wParam=TVN_ITEMEXPANDING then
            treeIdx = peek4s(lParam+NMTREEVIEW_itemNew+TVITEM_lParam)
            if treeIdx then
                if id=TVdirl then
                    if treeIdx<=length(treeItems)
                    and not treeItems[treeIdx][tLoaded] then
                        treeItems[treeIdx][tLoaded] = 1
                        void = loadDir(treeIdx,getTreeText(treeIdx,1))
                    end if
                elsif id=TVrecl then
                    if treeIdx<=length(backItems)
                    and not loadingRecDetails then  --DEV not needed??
                        loadingRecDetails = 1
                        if not backItems[treeIdx][tLoaded] then
                            backItems[treeIdx][tLoaded] = 1
                            if treeIdx<=RecDir then
                                loadRecoveryFiles(treeIdx)
                            else
                                loadRecoveryDetails(treeIdx,0)
                            end if
                        end if
                        loadingRecDetails = 0
                    end if
                end if
            end if
        elsif wParam=TVN_ENDLABELEDIT and id=TVdirl then
            treeIdx = peek4s(lParam+TVDISPINFO_TVITEM+TVITEM_lParam)
            f1 = getTreeText(treeIdx, 1)
            mem = peek4s(lParam+TVDISPINFO_TVITEM+TVITEM_pszText)
            if mem!=0 then  -- 0 means edit cancelled
                text = peek_string(mem)
                textsave = treeItems[treeIdx][tText]
                treeItems[treeIdx][tText] = text
                f2 = getTreeText(treeIdx, 1)
--              if not c_func(xMoveFile,{allocate_StringZ(f1),allocate_StringZ(f2)}) then
                if not c_func(xMoveFile,{IupRawStringPtr(f1),IupRawStringPtr(f2)}) then
                    void = proemh(sprintf("Error %d moving file",getLastError()),
                                  f1&"\n\nto\n\n"&f2,0)
                    treeItems[treeIdx][tText] = textsave
                end if
                repaintWindow(TVdirl,False)
            end if
        end if
    elsif msg=WM_PAINT then
        if id=TVdirl then
            if not TVinit then
                TVinit = 1
                loadAllDrives()
            end if
            if resetPrev then
                resetPrev = 0
                if currfile then
                    selectPreviousItem(filepaths[currfile]&filenames[currfile])
                else
                    selectPreviousItem("C:")
                end if
            end if
        elsif id=TVprjl then
            if not alreadyResettingProject then
                alreadyResettingProject = 1
                if resetProject(1) then
--                  void = sendMessage(TVprjl,WM_SETREDRAW,0,0)
                    IupSetAttribute(TVrecl,"AUTOREDRAW","NO")
                    deleteTVItem(TVprjl, TVI_ROOT)  -- clear
                    for i=1 to length(currProjFileSet) do
                        parent = currProjFileSet[i][4]
                        if parent then
                            parent = currProjFileSet[parent][1]
                        end if
--                      hItem = addTVItem(TVprjl,parent,i,0,currProjFileSet[i][5]) -- id,parent,idx,0,children
                        hItem = addTVItem(TVprjl,parent,i,0,0,currProjFileSet[i][5]) -- id,parent,idx,0,0,children
                        currProjFileSet[i][1] = hItem   -- save handle

--                      if usegpp then
                            if equal(currProjFileSet[i][2],filenames[currfile]) then
                                if equal(currProjFileSet[i][3],filepaths[currfile]) then
                                    void = sendMessage(TVprjl,TVM_SELECTITEM,TVGN_CARET,hItem)
                                end if
                            end if
--                      else
--                          if equal(currProjFileSet[i][2],lower(filenames[currfile])) then
--                              if equal(currProjFileSet[i][3],upper(filepaths[currfile])) then
--                                  void = sendMessage(TVprjl,TVM_SELECTITEM,TVGN_CARET,hItem)
--                              end if
--                          end if
--                      end if
                    end for
                    if length(currProjFileSet) then
                        void = sendMessage(TVprjl,TVM_EXPAND,TVE_EXPAND,currProjFileSet[1][1])
                    end if
--                  void = sendMessage(TVprjl,WM_SETREDRAW,1,0)
                    IupSetAttribute(TVrecl,"AUTOREDRAW","YES")
                end if
                alreadyResettingProject = 0
            end if
        elsif id=TVrecl then
            if not RecInit then
                RecInit = 1
                updateRecoveryTree = 0
                loadRecoveryDirs()
            end if
--      elsif id=TVrecdel then
        else
            ?9/0    -- unknown id!
        end if
    --      void = c_func( xMoveWindow, {getHwnd(CHECK), rect[1]+10, rect[4]-30, rect[1]+160, 25, 1} )
    --      void = c_func( xMoveWindow, {getHwnd(ICONS), rect[1]+180, rect[4]-30, rect[1]+330, 25, 1} )
    elsif msg=WM_CLOSE then -- fake, from edita.exw on shutdown.
        setHandler(TVdirl,-1)   -- makes it slightly faster, I think

    elsif msg=WM_COMMAND then
    elsif  msg=WM_LBUTTONDBLCLK then
        justDblClick = 1    -- focus issues (see eaqj)
    elsif (msg=WM_LBUTTONUP and justDblClick)
       or (msg=WM_KEYDOWN and wParam=VK_RETURN) then
        justDblClick = 0
        if id=TVdirl then
            treeIdx = getIndex(id)
            if treeIdx and treeIdx<=length(treeItems) then
                if treeItems[treeIdx][tLoaded]=-1 then  -- a leaf node
--                  void = openFile(getTreeText(treeIdx,1),1,isLegacyTabHandling)
                    {} = openFile(0,getTreeText(treeIdx,1),1)
                end if
            end if
        elsif id=TVprjl then
            treeIdx = getIndex(id)
            --DEV debug this, why don't I need TVM_EXPAND here/need it elsewhere?
            if treeIdx and treeIdx<=length(currProjFileSet) then
--              void = openFile(currProjFileSet[treeIdx][3]&currProjFileSet[treeIdx][2],1,1)
                {} = openFile(0,currProjFileSet[treeIdx][3]&currProjFileSet[treeIdx][2],1)
            end if
        elsif id=TVrecl then
            treeIdx = getIndex(id)
            if treeIdx and treeIdx<=length(backItems) then
                if backItems[treeIdx][tLoaded]=-1 then
                    oldcurrfile = currfile
--                  void = openFile(initialcurrentdir&`backup\`&backItems[treeIdx][tText],1,1)
                    {} = openFile(0,initialcurrentdir&`backup\`&backItems[treeIdx][tText],1)
-- 10/1/08:
--                  if not isSingleDir then
--                      if currfile>oldcurrfile+2 then
--                          shuffleLoop = 1 -- reduces drawing overhead
--                          while currfile>oldcurrfile+2 do
--                              shuffleTabs(-1)
--                          end while
--                          shuffleLoop = 0
--                      end if
--                      if currfile>oldcurrfile+1 then
--                          shuffleTabs(-1)
--                      end if
--                  end if

--              else    --DEV 7/10
--                  void = sendMessage(TVrecl,TVM_EXPAND,TVE_TOGGLE,backItems[treeIdx][tHandle])
                end if
            end if
--      else                -- Can occur on the delete button!
--          ?9/0    -- unknown id!
        end if
    elsif msg=WM_KEYDOWN then
        if id=TVdirl then
            if wParam=VK_F2 then        -- rename
                void = sendMessage(TVdirl,TVM_EDITLABEL,0,treeItems[getIndex(TVdirl)][tHandle])
            elsif wParam=VK_F5 then -- refresh
                treeIdx = getIndex(TVdirl)
                if getKeyState(VK_CONTROL) then
                    treeIdx = 0
                elsif getKeyState(VK_SHIFT) then
                    treeIdx = 0
                end if
                if treeIdx and treeIdx<=length(treeItems) then
                    text = getTreeText(treeIdx,1) -- fullpath
                    if treeItems[treeIdx][tLoaded]=-1 then      -- leaf node
                        treeIdx = treeItems[treeIdx][tPidx]     -- refresh parent
                    end if
                    if treeItems[treeIdx][tLoaded] then
--                      void = sendMessage(TVdirl,WM_SETREDRAW,False,0)
                        IupSetAttribute(TVrecl,"AUTOREDRAW","NO")
                        delTree(treeIdx)
                        purgefreelist()
--                      void = sendMessage(TVdirl,WM_SETREDRAW,True,0)
                        IupSetAttribute(TVrecl,"AUTOREDRAW","YES")
                    else
                        treeItems[treeIdx][tLoaded] = 1
                    end if
                    hasChildren = loadDir(treeIdx,getTreeText(treeIdx,1))
                    TVI = allocate(sizeofstruct(TVITEM))
                    poke4(TVI+TVITEM_hItem, treeItems[treeIdx][tHandle])
                    poke4(TVI+TVITEM_mask,TVIF_CHILDREN)
                    poke4(TVI+TVITEM_cChildren,hasChildren)
                    void = sendMessage(TVdirl,TVM_SETITEM,0,TVI)
                    free(TVI)
                    selectPreviousItem(text)
                else
--                  void = sendMessage(TVdirl,WM_SETREDRAW,False,0)
                    IupSetAttribute(TVrecl,"AUTOREDRAW","NO")
                    delTree(0)
                    purgefreelist()
                    --DEV don't need this, do I?
                    deleteTVItem(TVdirl, TVI_ROOT) -- clear
                    loadAllDrives()
--                  void = sendMessage(TVdirl,WM_SETREDRAW,True,0)
                    IupSetAttribute(TVrecl,"AUTOREDRAW","YES")
                end if
            end if
        elsif id=TVrecl then
            if wParam=VK_F5 then        -- refresh
                RecInit = 0
                backItems = {}
--              void = sendMessage(TVrecl,WM_SETREDRAW,False,0)
                IupSetAttribute(TVrecl,"AUTOREDRAW","NO")
                deleteTVItem(TVrecl, TVI_ROOT) -- clear
--              void = sendMessage(TVrecl,WM_SETREDRAW,True,0)
                IupSetAttribute(TVrecl,"AUTOREDRAW","YES")
            end if
        end if
    elsif msg=WM_SETFOCUS then
        setFocus(Main)  --DEV softSetFocus?
        return {1}
    end if
    return 0
end function
setHandler({TVdirl,TVprjl,TVrecl},routine_id("treeHandler"))
--*/

