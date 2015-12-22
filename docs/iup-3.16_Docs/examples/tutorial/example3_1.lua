require("iuplua")

multitext = iup.text{
  multiline = "YES",
  expand = "YES"
}
vbox = iup.vbox{
  multitext
}

dlg = iup.dialog{
  vbox,
  title = "Simple Notepad",
  size = "QUARTERxQUARTER"
}

dlg:showxy(iup.CENTER,iup.CENTER)
dlg.usersize = nil

-- to be able to run this script inside another context
if (iup.MainLoopLevel()==0) then
  iup.MainLoop()
  iup.Close()
end
