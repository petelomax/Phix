require("iuplua")

label = iup.label{title = "Hello world from IUP."}
dlg = iup.dialog{
  iup.vbox{label},
  title = "Hello World 2",
}

dlg:showxy(iup.CENTER,iup.CENTER)

-- to be able to run this script inside another context
if (iup.MainLoopLevel()==0) then
  iup.MainLoop()
  iup.Close()
end
