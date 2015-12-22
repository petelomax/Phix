require("iuplua")

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

multitext = iup.text{
  multiline = "YES",
  expand = "YES"
}

item_open = iup.item{title="Open..."}
item_saveas = iup.item{title="Save As..."}
item_font = iup.item{title="Font..."}
item_about = iup.item{title="About..."}
item_exit = iup.item{title="Exit"}

function item_open:action()
  local filedlg = iup.filedlg{
    dialogtype = "OPEN", 
    filter = "*.txt", 
    filterinfo = "Text Files",
    }

  filedlg:popup(iup.CENTER, iup.CENTER)

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
    }

  filedlg:popup(iup.CENTER, iup.CENTER)

  if (tonumber(filedlg.status) ~= -1) then
    local filename = filedlg.value
    write_file(filename, multitext.value)
  end
  filedlg:destroy()
end

function item_font:action()
  local font = multitext.font
  local fontdlg = iup.fontdlg{value = font}

  fontdlg:popup(iup.CENTER, iup.CENTER)

  if (tonumber(fontdlg.status) == 1) then
    multitext.font = fontdlg.value
  end

  fontdlg:destroy()
end

function item_about:action()
  iup.Message("About", "   Simple Notepad\n\nAutors:\n   Gustavo Lyrio\n   Antonio Scuri")
end

function item_exit:action()
  return iup.CLOSE
end

file_menu = iup.menu{item_open,item_saveas,iup.separator{},item_exit}
format_menu = iup.menu{item_font}
help_menu = iup.menu{item_about}
sub_menu_file = iup.submenu{file_menu, title = "File"}
sub_menu_format = iup.submenu{format_menu, title = "Format"}
sub_menu_help = iup.submenu{help_menu, title = "Help"}

menu = iup.menu{
  sub_menu_file, 
  sub_menu_format, 
  sub_menu_help
  }

vbox = iup.vbox{
  multitext
}

dlg = iup.dialog{
  vbox,
  title = "Simple Notepad",
  size = "QUARTERxQUARTER",
  menu = menu
}

dlg:showxy(iup.CENTER,iup.CENTER)
dlg.usersize = nil

-- to be able to run this script inside another context
if (iup.MainLoopLevel()==0) then
  iup.MainLoop()
  iup.Close()
end
