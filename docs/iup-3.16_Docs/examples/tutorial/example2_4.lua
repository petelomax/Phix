require("iuplua")

label = iup.label{title = "Hello world from IUP."}
button = iup.button{title = "OK"}

function button:action()
  -- Exits the main loop
  return iup.CLOSE  
end

vbox = iup.vbox{label,button}
dlg = iup.dialog{
  vbox,
  title = "Hello World 4"
}
button.action = btn_exit_cb

dlg:showxy(iup.CENTER,iup.CENTER)

-- to be able to run this script inside another context
if (iup.MainLoopLevel()==0) then
  iup.MainLoop()
  iup.Close()
end
