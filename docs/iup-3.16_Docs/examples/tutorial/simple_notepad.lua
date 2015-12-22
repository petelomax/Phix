require("iuplua")
require("iupluaimglib")


--********************************** Utilities *****************************************

function str_nocase(s)
  s = string.gsub(s, "%a", function (c)
      return string.format("[%s%s]", string.lower(c),
                                     string.upper(c))
    end)
  return s
end

function str_find(str, str_to_find, casesensitive, start)
  if (not casesensitive) then
    str_to_find = str_nocase(str_to_find)
    return string.find(str, str_to_find, start)
  end

  return string.find(str, str_to_find, start, true)
end

function str_splitfilename(filename)
  return string.match(filename, "(.-)([^\\/]-%.?([^%.\\/]*))$")
end

function str_filetitle(filename)
  local path, title, ext = str_splitfilename(filename)
  return title
end

function read_file(filename)
  local ifile = io.open(filename, "r")
  if (not ifile) then
    iup.Message("Error", "Can't open file: " .. filename)
    return nil
  end
  
  local str = ifile:read("*a")
  if (not str) then
    iup.Message("Error", "Fail when reading from file: " .. filename)
    return nil
  end
  
  ifile:close()
  return str
end

function write_file(filename, str)
  local ifile = io.open(filename, "w")
  if (not ifile) then
    iup.Message("Error", "Can't open file: " .. filename)
    return false
  end
  
  if (not ifile:write(str)) then
    iup.Message("Error", "Fail when writing to file: " .. filename)
  end
  
  ifile:close()
  return true
end

function new_file(ih)
  local dlg = iup.GetDialog(ih)
  local multitext = dlg.multitext
  
  dlg.title = "Untitled - Simple Notepad"
  multitext.filename = nil
  multitext.dirty = nil
  multitext.value = ""
end

function open_file(ih, filename)
  local str = read_file(filename)
  if (str) then
    local dlg = iup.GetDialog(ih)
    local multitext = dlg.multitext
    local config = multitext.config
  
    dlg.title = str_filetitle(filename).." - Simple Notepad"
    multitext.filename = filename
    multitext.dirty = nil
    multitext.value = str
    
    config:RecentUpdate(filename)
  end
end

function save_file(multitext)
  if (write_file(multitext.filename, multitext.value)) then
    multitext.dirty = nil
  end
end

function saveas_file(multitext, filename)
  if (write_file(filename, multitext.value)) then
    local dlg = iup.GetDialog(multitext)
    local config = multitext.config
    
    dlg.title = str_filetitle(filename).." - Simple Notepad"
    multitext.filename = filename
    multitext.dirty = nil
    
    config:RecentUpdate(filename)
  end
end

function save_check(ih)
  local dlg = iup.GetDialog(ih)
  local multitext = dlg.multitext
  
  if (multitext.dirty) then
    local resp = iup.Alarm("Warning", "File not saved! Save it now?", "Yes", "No", "Cancel")
    if resp == 1 then -- save the changes and continue
      save_file(multitext)
    elseif resp == 3 then  -- cancel
      return false
    else  -- ignore the changes and continue
    end
  end
  return true
end

function toggle_bar_visibility(item, bar)
  if (item.value == "ON") then
    bar.floating = "YES"
    bar.visible = "NO"
    item.value = "OFF"
  else
    bar.floating = "NO"
    bar.visible = "YES"
    item.value = "ON"
  end
  iup.Refresh(bar)  -- refresh the dialog layout
end

function set_find_replace_visibility(find_dlg, show_replace)
  local replace_txt = find_dlg.replace_txt
  local replace_lbl = find_dlg.replace_lbl
  local replace_bt = find_dlg.replace_bt

  if (show_replace) then
    replace_txt.visible = "Yes"
    replace_lbl.visible = "Yes"
    replace_bt.visible = "Yes"
    replace_txt.floating = "No"
    replace_lbl.floating = "No"
    replace_bt.floating  = "No"

    find_dlg.title = "Replace"
  else
    replace_txt.floating = "Yes"
    replace_lbl.floating = "Yes"
    replace_bt.floating = "Yes"
    replace_txt.visible = "No"
    replace_lbl.visible = "No"
    replace_bt.visible = "No"

    find_dlg.title = "Find"
  end

  find_dlg.size = nil  -- force a dialog resize on the iup.Refresh
  iup.Refresh(find_dlg)
end


--********************************** Main (Part 1/2) *****************************************

-- create all the elements that will have callbacks in Lua prior to callbacks definition

config = iup.config{}
config.app_name = "simple_notepad"
config:Load()

lbl_statusbar = iup.label{title = "Lin 1, Col 1", expand = "HORIZONTAL", padding = "10x5"}

multitext = iup.text{
  multiline = "YES",
  expand = "YES",
  config = config,
  dirty = nil,
}

item_new = iup.item{title = "&New...\tCtrl+N", image = "IUP_FileNew"}
item_open = iup.item{title = "&Open...\tCtrl+O", image = "IUP_FileOpen"}
item_save = iup.item{title="&Save\tCtrl+S"}
item_saveas = iup.item{title="Save &As...", image = "IUP_FileSave"}
item_revert = iup.item{title="&Revert"}
item_exit = iup.item{title="E&xit"}
item_find = iup.item{title="&Find...\tCtrl+F", image = "IUP_EditFind"}
item_find_next = iup.item{title = "Find &Next\tF3"}
item_replace = iup.item{title = "&Replace...\tCtrl+H"}
item_copy = iup.item{title="&Copy\tCtrl+C", image = "IUP_EditCopy"}
item_paste = iup.item{title="&Paste\tCtrl+V", image = "IUP_EditPaste"}
item_cut = iup.item{title="Cu&t\tCtrl+X", image = "IUP_EditCut"}
item_delete = iup.item{title="&Delete\tDel", image = "IUP_EditErase"}
item_select_all = iup.item{title="Select &All\tCtrl+A"}
item_goto = iup.item{title="&Go To...\tCtrl+G"}
item_font = iup.item{title="&Font..."}
item_toolbar = iup.item{title="&Toobar", value="ON"}
item_statusbar = iup.item{title="&Statusbar", value="ON"}
item_help = iup.item{title="&Help..."}
item_about = iup.item{title="&About..."}

recent_menu = iup.menu{}

file_menu = iup.menu{
  item_new,
  item_open,
  item_save,
  item_saveas,
  item_revert,
  iup.separator{},
  iup.submenu{title="Recent &Files", recent_menu},
  item_exit
  }

edit_menu = iup.menu{
  item_cut,
  item_copy,
  item_paste,
  item_delete,
  iup.separator{},
  item_find,
  item_find_next,
  item_replace,
  item_goto,
  iup.separator{},
  item_select_all
  }

format_menu = iup.menu{item_font}
view_menu = iup.menu{item_toolbar, item_statusbar}
help_menu = iup.menu{item_help, item_about}

sub_menu_file = iup.submenu{file_menu, title = "&File"}
sub_menu_edit = iup.submenu{edit_menu, title = "&Edit"}
sub_menu_format = iup.submenu{format_menu, title = "F&ormat"}
sub_menu_view = iup.submenu{title = "&View", view_menu}
sub_menu_help = iup.submenu{help_menu, title = "&Help"}

menu = iup.menu{
  sub_menu_file, 
  sub_menu_edit, 
  sub_menu_format, 
  sub_menu_view, 
  sub_menu_help,
  }


--********************************** Callbacks *****************************************


function multitext:dropfiles_cb(filename)
  if (save_check(self)) then
    open_file(self, filename)
  end
end

function multitext:valuechanged_cb()
  self.dirty = "YES"
end

function file_menu:open_cb()
  if (multitext.dirty) then
    item_save.active = "YES"
  else
    item_save.active = "NO"
  end
  if (multitext.dirty and multitext.filename) then
    item_revert.active = "YES"
  else
    item_revert.active = "NO"
  end
end

function edit_menu:open_cb()
  local clipboard = iup.clipboard{}
  if (not clipboard.textavailable) then
    item_paste.active = "NO"
  else
    item_paste.active = "YES"
  end
  clipboard:destroy()

  if (not multitext.selectedtext) then
    item_cut.active = "NO"
    item_delete.active = "NO"
    item_copy.active = "NO"
  else
    item_cut.active = "YES"
    item_delete.active = "YES"
    item_copy.active = "YES"
  end

  if (find_dlg) then
    local find_txt = find_dlg.find_txt
    if (find_txt) and (find_txt.value ~= "") then
      item_find_next.active = "YES"
    else
      item_find_next.active = "NO"
    end
  else
    item_find_next.active = "NO"
  end
end

function config:recent_cb()
  if (save_check(self)) then
    local filename = self.title
    open_file(self, filename)
  end
end

function multitext:caret_cb(lin, col)
  lbl_statusbar.title = "Lin "..lin..", Col "..col
end

function item_new:action()
  if save_check(self) then
    new_file(self)
  end
end

function item_open:action()
  if not save_check(self) then
    return
  end
  
  local filedlg = iup.filedlg{
    dialogtype = "OPEN", 
    extfilter="Text Files|*.txt|All Files|*.*|",
    parentdialog=iup.GetDialog(self),
    directory = config:GetVariable("MainWindow", "LastDirectory"),
    }
    
  filedlg:popup(iup.CENTERPARENT, iup.CENTERPARENT)
  
  if (tonumber(filedlg.status) ~= -1) then
    local filename = filedlg.value
    open_file(self, filename)

    config:SetVariable("MainWindow", "LastDirectory", filedlg.directory)
  end
  
  filedlg:destroy()
end

function item_saveas:action()
  local filedlg = iup.filedlg{
    dialogtype = "SAVE", 
    extfilter="Text Files|*.txt|All Files|*.*|",
    parentdialog = iup.GetDialog(self),
    file = multitext.filename,
    directory = config:GetVariable("MainWindow", "LastDirectory"),
    }

  filedlg:popup(iup.CENTERPARENT, iup.CENTERPARENT)

  if (tonumber(filedlg.status) ~= -1) then
    local filename = filedlg.value
    saveas_file(multitext, filename)    

    config:SetVariable("MainWindow", "LastDirectory", filedlg.directory)
  end
  
  filedlg:destroy()
end

function item_save:action()
  if (not multitext.filename) then
    item_saveas:action()
  else
    -- test again because in can be called using the hot key
    if (multitext.dirty) then
      save_file(multitext)
    end
  end
end

function item_revert:action()
  open_file(self, multitext.filename)
end

function item_exit:action()
  if not save_check(self) then
    return iup.IGNORE  -- to abort the CLOSE_CB callback
  end
  
  config:DialogClosed(iup.GetDialog(self), "MainWindow")
  config:Save()
  config:destroy()
  return iup.CLOSE
end

function item_goto:action()
  local line_count = multitext.linecount
  local lbl_goto = iup.label{title = "Line Number [1-"..line_count.."]:"}
  local txt_goto = iup.text{mask = iup.MASK_UINT, visiblecolumns = 20} --unsigned integer numbers only

  local bt_goto_ok = iup.button{title = "OK", text_linecount = 0, padding = "10x2"} 
  bt_goto_ok.text_linecount = line_count
  function bt_goto_ok:action()
    local line_count = tonumber(self.text_linecount)
    local line = tonumber(txt_goto.value)
    if (line < 1 or line >= line_count) then
      iup.Message("Error", "Invalid line number.")
      return
    end
    goto_dlg.status = 1
    return iup.CLOSE
  end

  local bt_goto_cancel = iup.button{title = "Cancel", padding = "10x2"}
  function bt_goto_cancel:action()
    goto_dlg.status = 0
    return iup.CLOSE
  end

  local box = iup.vbox{
    lbl_goto,
    txt_goto,
    iup.hbox{
      iup.fill{},
      bt_goto_ok,
      bt_goto_cancel,
      normalizesize="HORIZONTAL", 
    },
    margin = "10x10", 
    gap = "5",
  }
  local goto_dlg = iup.dialog{
    box,
    title = "Go To Line", 
    dialogframe = "Yes", 
    defaultenter = bt_goto_ok, 
    defaultesc = bt_goto_cancel,
    parentdialog = iup.GetDialog(self)
  }

  goto_dlg:popup(iup.CENTERPARENT, iup.CENTERPARENT)

  if (tonumber(goto_dlg.status) == 1) then
    local line = txt_goto.value
    local pos = iup.TextConvertLinColToPos(multitext, line, 0)
    multitext.caretpos = pos
    multitext.scrolltopos = pos
  end

  goto_dlg:destroy()
end

function item_find_next:action()
  -- test, because it can be called from the hot key
  if (find_dlg) then
    local find_pos = tonumber(find_dlg.find_pos)

    local find_txt = find_dlg.find_txt
    local str_to_find = find_txt.value

    local find_case = find_dlg.find_case
    local casesensitive = (find_case.value == "ON")

    -- test again, because it can be called from the hot key
    if (not str_to_find or str_to_find:len()==0) then
      return
    end

    if (not find_pos) then
      find_pos = 1
    end

    local str = multitext.value

    local pos, end_pos = str_find(str, str_to_find, casesensitive, find_pos)
    if (not pos) then
      pos, end_pos = str_find(str, str_to_find, casesensitive, 1)  -- try again from the start
    end

    if (pos) and (pos > 0) then
      pos = pos - 1
      find_dlg.find_pos = end_pos

      iup.SetFocus(multitext)
      multitext.selectionpos = pos..":"..end_pos
      multitext.find_selection = pos..":"..end_pos

      local lin, col = iup.TextConvertPosToLinCol(multitext, pos)
      local pos = iup.TextConvertLinColToPos(multitext, lin, 0)  -- position at col=0, just scroll lines 
      multitext.scrolltopos = pos
    else
      find_dlg.find_pos = nil
      iup.Message("Warning", "Text not found: "..str_to_find)
    end
  end
end

function create_find_dialog()
  local find_txt = iup.text{visiblecolumns = "20"}
  local find_case = iup.toggle{title = "Case Sensitive"}
  local bt_find_next = iup.button{title = "Find Next", padding = "10x2"}
  local bt_find_close = iup.button{title = "Close", padding = "10x2"}
  local replace_txt = iup.text{visiblecolumns = "20"}
  local replace_bt = iup.button{title = "Replace", padding = "10x2", action = find_replace_action_cb}
  local replace_lbl = iup.label{title = "Replace with:"}

  function replace_bt:action()
    local find_pos = tonumber(find_dlg.find_pos)
    local selectionpos = multitext.selectionpos
    local find_selection = multitext.find_selection

    if (not find_pos or not selectionpos or not find_selection or (selectionpos ~= find_selection)) then
      item_find_next:action()
    else
      local str_to_replace = replace_txt.value
      multitext.selectedtext = str_to_replace

      -- then find next
      item_find_next:action()
    end
  end

  function bt_find_next:action()
      item_find_next:action()
  end

  function bt_find_close:action()
    config:DialogClosed(find_dlg, "FindDialog")
    find_dlg:hide()  -- do not destroy, just hide
  end

  local box = iup.vbox{
    iup.label{title = "Find What:"},
    find_txt,
    replace_lbl,
    replace_txt,
    find_case,
    iup.hbox{
      iup.fill{},
      bt_find_next,
      replace_bt,
      bt_find_close,
      normalizesize="HORIZONTAL"
    },
    margin = "10x10", 
    gap = "5"
  }

  find_dlg = iup.dialog{  -- create as global, not local
    box, 
    title = "Find", 
    dialogframe = "Yes", 
    defaultenter = bt_next, 
    defaultesc = bt_close,
    parentdialog = iup.GetDialog(multitext),
    close_cb = bt_find_close_action,

    find_txt = find_txt,
    find_case = find_case,
    replace_txt = replace_txt,
    replace_bt = replace_bt, 
    replace_lbl = replace_lbl,
    }

  return find_dlg
end

function item_find:action()
  if (not find_dlg) then
    find_dlg = create_find_dialog()  -- create as global, not local
  end
  set_find_replace_visibility(find_dlg, false)

  config:DialogShow(find_dlg, "FindDialog")

  local str = multitext.selectedtext
  if (str and str:len()~=0) then
    local find_txt = find_dlg.find_txt
    find_txt.value = str
  end
end

function item_replace:action()
  if (not find_dlg) then
    find_dlg = create_find_dialog()  -- create as global, not local
  end
  set_find_replace_visibility(find_dlg, true)

  config:DialogShow(find_dlg, "FindDialog")

  local str = multitext.selectedtext
  if (str and str:len()~=0) then
    local find_txt = find_dlg.find_txt
    find_txt.value = str
  end
end

function selection_find_next()
  local str = multitext.selectedtext
  if (str and str:len()~=0) then
    if (not find_dlg) then
      find_dlg = create_find_dialog()  -- create as global, not local
    end

    local find_txt = find_dlg.find_txt
    find_txt.value = str

    item_find_next:action()
  end
end

function item_copy:action()
  local clipboard = iup.clipboard{text = multitext.selectedtext}
  clipboard:destroy()
end

function item_paste:action()
  local clipboard = iup.clipboard{}
  multitext.insert = clipboard.text
  clipboard:destroy()
  return iup.IGNORE  -- avoid system processing for hot keys, to correctly parse line feed
end

function item_cut:action()
  local clipboard = iup.clipboard{text = multitext.selectedtext}
  multitext.selectedtext = ""
  clipboard:destroy()
end

function item_delete:action()
  multitext.selectedtext = ""
end

function item_select_all:action()
  iup.SetFocus(multitext)
  multitext.selection = "ALL"
end

function item_font:action()
  local font = multitext.font
  local fontdlg = iup.fontdlg{value = font, parentdialog=iup.GetDialog(self)}

  fontdlg:popup(iup.CENTERPARENT, iup.CENTERPARENT)

  if (tonumber(fontdlg.status) == 1) then
    multitext.font = fontdlg.value
    config:SetVariable("MainWindow", "Font", fontdlg.value)
  end

  fontdlg:destroy()
end

function item_toolbar:action()
  toggle_bar_visibility(self, toolbar_hb)
  config:SetVariable("MainWindow", "Toolbar", item_toolbar.value)
end

function item_statusbar:action()
  toggle_bar_visibility(self, lbl_statusbar)
  config:SetVariable("MainWindow", "Statusbar", item_statusbar.value)
end

function item_help:action()
  iup.Help("http://www.tecgraf.puc-rio.br/iup")
end

function item_about:action()
  iup.Message("About", "   Simple Notepad\n\nAutors:\n   Gustavo Lyrio\n   Antonio Scuri")
end


--********************************** Main (Part 2/2) *****************************************


btn_new = iup.button{image = "IUP_FileNew", flat = "Yes", action = item_new.action, canfocus="No", tip = "New (Ctrl+N)"}
btn_open = iup.button{image = "IUP_FileOpen", flat = "Yes", action = item_open.action, canfocus="No", tip = "Open (Ctrl+O)"}
btn_save = iup.button{image = "IUP_FileSave", flat = "Yes", action = item_save.action, canfocus="No", tip = "Save (Ctrl+S)"}
btn_find = iup.button{image = "IUP_EditFind", flat = "Yes", action = item_find.action, canfocus="No", tip = "Find (Ctrl+F)"}
btn_cut = iup.button{image = "IUP_EditCut", flat = "Yes", action = item_cut.action, canfocus="No", tip = "Cut (Ctrl+X)"}
btn_copy = iup.button{image =  "IUP_EditCopy", flat = "Yes", action = item_copy.action, canfocus="No", tip = "Copy (Ctrl+C)"}
btn_paste = iup.button{image = "IUP_EditPaste", flat = "Yes", action = item_paste.action, canfocus="No", tip = "Paste (Ctrl+V)"}

toolbar_hb = iup.hbox{
  btn_new,
  btn_open,
  btn_save,
  iup.label{separator="VERTICAL"},
  btn_cut,
  btn_copy,
  btn_paste,
  iup.label{separator="VERTICAL"},
  btn_find, 
  margin = "5x5",
  gap = 2,
}

vbox = iup.vbox{
  toolbar_hb,
  multitext,
  lbl_statusbar,
}

dlg = iup.dialog{
  vbox,
  title = "Simple Notepad",
  size = "HALFxHALF",
  menu = menu,
  close_cb = item_exit.action,
  multitext = multitext,
  dropfiles_cb = multitext.dropfiles_cb,
}

function dlg:k_any(c)
  if (c == iup.K_cN) then
    item_new:action()
  elseif (c == iup.K_cO) then
    item_open:action()
  elseif (c == iup.K_cS) then
    item_save:action()
  elseif (c == iup.K_cF) then
    item_find:action()
  elseif (c == iup.K_cH) then
    item_replace:action()
    return iup.IGNORE -- replace system processing
  elseif (c == iup.K_cG) then
    item_goto:action()
  elseif (c == iup.K_F3) then
    item_find_next:action()
  elseif (c == iup.K_cF3) then
    selection_find_next()
  elseif (c == iup.K_cV) then
    item_paste:action()  
    return iup.IGNORE -- replace system processing
  end
  -- Ctrl+C, Ctrl+X, Ctrl+A, Del, already implemented inside IupText
end

-- parent for pre-defined dialogs in closed functions (IupMessage and IupAlarm)
iup.SetGlobal("PARENTDIALOG", iup.SetHandleName(dlg))

-- Initialize variables from the configuration file

config:RecentInit(recent_menu, 10)
 
font = config:GetVariable("MainWindow", "Font")
if (font) then
  multitext.font = font
end

show_statusbar = config:GetVariableDef("MainWindow", "Statusbar", "ON")
if (show_statusbar == "OFF") then
  item_statusbar.value = "OFF"
  lbl_statusbar.floating = "YES"
  lbl_statusbar.visible = "NO"
end

show_toolbar = config:GetVariableDef("MainWindow", "Toolbar", "ON")
if (show_toolbar == "OFF") then
  item_toolbar.value = "OFF"
  toolbar_hb.floating = "YES"
  toolbar_hb.visible = "NO"
end

-- show the dialog at the last position, with the last size
config:DialogShow(dlg, "MainWindow")

-- initialize the current file
new_file(dlg)

-- open a file from the command line (allow file association in Windows)
if (arg and arg[1]) then
  filename = arg[1]
  open_file(dlg, filename)
end

-- to be able to run this script inside another context
if (iup.MainLoopLevel()==0) then
  iup.MainLoop()
  iup.Close()
end
