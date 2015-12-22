require("iuplua")

multitext = iup.text{
  multiline = "YES",
  expand = "YES"
}

item_open = iup.item{title="Open"}
item_save = iup.item{title="Save"}
item_exit = iup.item{title="Exit"}

function item_exit:action()
  return iup.CLOSE
end

file_menu = iup.menu{item_open,item_save,iup.separator{},item_exit}
sub1_menu = iup.submenu{file_menu, title = "File"}
menu = iup.menu{sub1_menu}

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
