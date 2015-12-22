require("iuplua")


--********************************** Utilities *****************************************


function str_find(str, str_to_find, casesensitive, start)
  if (not casesensitive) then
    return str_find(string.lower(str), string.lower(str_to_find), true, start)
  end

  return string.find(str, str_to_find, start, true)
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
    return
  end
  
  if (not ifile:write(str)) then
    iup.Message("Error", "Fail when writing to file: " .. filename)
  end
  
  ifile:close()
end


--********************************** Main (Part 1/2) *****************************************


multitext = iup.text{
  multiline = "YES",
  expand = "YES"
}

item_open = iup.item{title="Open..."}
item_saveas = iup.item{title="Save As..."}
item_font = iup.item{title="Font..."}
item_about = iup.item{title="About..."}
item_find = iup.item{title="Find..."}
item_goto = iup.item{title="Go To..."}
item_exit = iup.item{title="Exit"}


--********************************** Callbacks *****************************************


function item_open:action()
  local filedlg = iup.filedlg{
    dialogtype = "OPEN", 
    filter = "*.txt", 
    filterinfo = "Text Files",
    parentdialog=iup.GetDialog(self)
    }

  filedlg:popup(iup.CENTERPARENT, iup.CENTERPARENT)

  if (tonumber(filedlg.status) ~= -1) then
    local filename = filedlg.value
    local str = read_file(filename)
    if (str) then
      multitext.value = str
    end
  end
  filedlg:destroy()
end

function item_saveas:action()
  local filedlg = iup.filedlg{
    dialogtype = "SAVE", 
    filter = "*.txt", 
    filterinfo = "Text Files", 
    parentdialog=iup.GetDialog(self)
    }

  filedlg:popup(iup.CENTERPARENT, iup.CENTERPARENT)

  if (tonumber(filedlg.status) ~= -1) then
    local filename = filedlg.value
    write_file(filename, multitext.value)
  end
  filedlg:destroy()
end

function item_exit:action()
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

function item_font:action()
  local font = multitext.font
  local fontdlg = iup.fontdlg{value = font, parentdialog=iup.GetDialog(self)}

  fontdlg:popup(iup.CENTERPARENT, iup.CENTERPARENT)

  if (tonumber(fontdlg.status) == 1) then
    multitext.font = fontdlg.value
  end

  fontdlg:destroy()
end

function item_about:action()
  iup.Message("About", "   Simple Notepad\n\nAutors:\n   Gustavo Lyrio\n   Antonio Scuri")
end


--********************************** Main (Part 2/2) *****************************************


file_menu = iup.menu{item_open,item_saveas,iup.separator{},item_exit}
edit_menu = iup.menu{item_find, item_goto}
format_menu = iup.menu{item_font}
help_menu = iup.menu{item_about}
sub_menu_file = iup.submenu{file_menu, title = "File"}
sub_menu_edit = iup.submenu{edit_menu, title = "Edit"}
sub_menu_format = iup.submenu{format_menu, title = "Format"}
sub_menu_help = iup.submenu{help_menu, title = "Help"}

menu = iup.menu{
  sub_menu_file, 
  sub_menu_edit, 
  sub_menu_format, 
  sub_menu_help,
  }

vbox = iup.vbox{
  multitext
}

dlg = iup.dialog{
  vbox,
  title = "Simple Notepad",
  size = "HALFxHALF",
  menu = menu
}

-- parent for pre-defined dialogs in closed functions (IupMessage)
iup.SetGlobal("PARENTDIALOG", iup.SetHandleName(dlg))

dlg:showxy(iup.CENTER,iup.CENTER)
dlg.usersize = nil

-- to be able to run this script inside another context
if (iup.MainLoopLevel()==0) then
  iup.MainLoop()
  iup.Close()
end
