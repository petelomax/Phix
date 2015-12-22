require("iuplua")
require("iupluaimglib")


--********************************** Utilities *****************************************


function str_find(str, str_to_find, casesensitive, start)
  if (not casesensitive) then
    return str_find(string.lower(str), string.lower(str_to_find), true, start)
  end

  return string.find(str, str_to_find, start, true)
end

function str_filetitle(filename)
  local filename = string.gsub(filename, "\\", "/")
  filename = string.reverse(filename)
  final = string.find(filename, '/')
  filename = string.sub(filename, 1, final-1)
  filename = string.reverse(filename)
  return filename
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


--********************************** Main (Part 1/2) *****************************************


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
 
font = config:GetVariable("MainWindow", "Font")
if (font) then
  multitext.font = font
end

item_new = iup.item{title = "&New...\tCtrl+N", image = "IUP_FileNew"}
item_open = iup.item{title = "&Open...\tCtrl+O", image = "IUP_FileOpen"}
item_save = iup.item{title="&Save\tCtrl+S"}
item_saveas = iup.item{title="Save &As...", image = "IUP_FileSave"}
item_font = iup.item{title="&Font..."}
item_about = iup.item{title="&About..."}
item_find = iup.item{title="&Find...\tCtrl+F", image = "IUP_EditFind"}
item_goto = iup.item{title="&Go To..."}
item_copy = iup.item{title="&Copy\tCtrl+C", image = "IUP_EditCopy"}
item_paste = iup.item{title="&Paste\tCtrl+V", image = "IUP_EditPaste"}
item_cut = iup.item{title="Cu&t\tCtrl+X", image = "IUP_EditCut"}
item_delete = iup.item{title="&Delete\tDel", image = "IUP_EditErase"}
item_select_all = iup.item{title="Select &All\tCtrl+A"}
item_revert = iup.item{title="&Revert"}
item_exit = iup.item{title="E&xit"}


--********************************** Callbacks *****************************************


function multitext:dropfiles_cb(filename)
  if (save_check(self)) then
    open_file(self, filename)
  end
end

function multitext:valuechanged_cb()
  self.dirty = "YES"
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
    filter = "*.txt", 
    filterinfo = "Text Files", 
    parentdialog=iup.GetDialog(self)
    }
    
  filedlg:popup(iup.CENTERPARENT, iup.CENTERPARENT)
  
  if (tonumber(filedlg.status) ~= -1) then
    local filename = filedlg.value
    open_file(self, filename)
  end
  
  filedlg:destroy()
end

function item_saveas:action()
  local filedlg = iup.filedlg{
    dialogtype = "SAVE", 
    filter = "*.txt", 
    filterinfo = "Text Files", 
    parentdialog = iup.GetDialog(self),
    file = multitext.filename,
    }

  filedlg:popup(iup.CENTERPARENT, iup.CENTERPARENT)

  if (tonumber(filedlg.status) ~= -1) then
    local filename = filedlg.value
    saveas_file(multitext, filename)    
  end
  
  filedlg:destroy()
end

function item_save:action()
  if (not multitext.filename) then
    item_saveas:action()
  else
    save_file(multitext)
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
  goto_dlg = iup.dialog{
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

function item_find:action()
  local find_dlg = self.find_dialog
  if (not find_dlg) then
    local find_txt = iup.text{visiblecolumns = "20"}
    local find_case = iup.toggle{title = "Case Sensitive"}
    local bt_find_next = iup.button{title = "Find Next", padding = "10x2"}
    local bt_find_close = iup.button{title = "Close", padding = "10x2"}

    function bt_find_next:action()
      local find_pos = tonumber(find_dlg.find_pos)
      local str_to_find = find_txt.value

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

        local lin, col = iup.TextConvertPosToLinCol(multitext, pos)
        local pos = iup.TextConvertLinColToPos(multitext, lin, 0)  -- position at col=0, just scroll lines
        multitext.scrolltopos = pos
      else
        find_dlg.find_pos = nil
        iup.Message("Warning", "Text not found.")
      end
    end

    function bt_find_close:action()
      iup.Hide(iup.GetDialog(self))  -- do not destroy, just hide
    end

    box = iup.vbox{
      iup.label{title = "Find What:"},
      find_txt,
      find_case,
      iup.hbox{
        iup.fill{},
        bt_find_next,
        bt_find_close,
        normalizesize="HORIZONTAL", 
      },
      margin = "10x10", 
      gap = "5",
    }

    find_dlg = iup.dialog{
      box, 
      title = "Find", 
      dialogframe = "Yes", 
      defaultenter = bt_next, 
      defaultesc = bt_close,
      parentdialog = iup.GetDialog(self)
      }

    -- Save the dialog to reuse it 
    self.find_dialog = find_dlg -- from the main dialog */
  end

  -- centerparent first time, next time reuse the last position
  find_dlg:showxy(iup.CURRENT, iup.CURRENT)
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

function item_about:action()
  iup.Message("About", "   Simple Notepad\n\nAutors:\n   Gustavo Lyrio\n   Antonio Scuri")
end


--********************************** Main (Part 2/2) *****************************************


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

edit_menu = iup.menu{
    item_cut,
    item_copy,
    item_paste,
    item_delete,
    iup.separator{},
    item_find,
    item_goto,
    iup.separator{},
    item_select_all
    }

function edit_menu:open_cb()
  local clipboard = iup.clipboard{}

  if (not clipboard.textavailable) then
    item_paste.active = "NO"
  else
    item_paste.active = "YES"
  end

  if (not multitext.selectedtext) then
    item_cut.active = "NO"
    item_delete.active = "NO"
    item_copy.active = "NO"
  else
    item_cut.active = "YES"
    item_delete.active = "YES"
    item_copy.active = "YES"
  end

  clipboard:destroy()
end

format_menu = iup.menu{item_font}
help_menu = iup.menu{item_about}
sub_menu_file = iup.submenu{file_menu, title = "&File"}
sub_menu_edit = iup.submenu{edit_menu, title = "&Edit"}
sub_menu_format = iup.submenu{format_menu, title = "F&ormat"}
sub_menu_help = iup.submenu{help_menu, title = "&Help"}

menu = iup.menu{
  sub_menu_file, 
  sub_menu_edit, 
  sub_menu_format, 
  sub_menu_help,
  }

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
  elseif (c == iup.K_cG) then
    item_goto:action()
  end
end

config:RecentInit(recent_menu, 10)

-- parent for pre-defined dialogs in closed functions (IupMessage)
iup.SetGlobal("PARENTDIALOG", iup.SetHandleName(dlg))

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
