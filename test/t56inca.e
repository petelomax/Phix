-- The implicit forward call is assumed local; when the actual definition is
--  found to be local, it invokes ReLinkAsGlobal in psym.e
Window()
global procedure Window()
    puts(1,"Window\n")
end procedure

