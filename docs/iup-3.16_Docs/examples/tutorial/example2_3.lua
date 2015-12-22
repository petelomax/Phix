require("iuplua")

function btn_exit_cb(self)
  iup.Message("Hello World Message","Hello world from IUP.")
  -- Exits the main loop
  return iup.CLOSE  
end

button = iup.button{title = "OK", action = btn_exit_cb}

vbox = iup.vbox{button}
dlg = iup.dialog{
  vbox,
  title = "Hello World 3"
}

dlg:showxy(iup.CENTER,iup.CENTER)

-- to be able to run this script inside another context
if (iup.MainLoopLevel()==0) then
  iup.MainLoop()
  iup.Close()
end
