require("iuplua")

iup.Message("Hello World 1","Hello world from IUP.")

-- to be able to run this script inside another context
if (iup.MainLoopLevel()==0) then
  iup.MainLoop()
  iup.Close()
end
