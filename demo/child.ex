puts(1,"This is the child process (stdout)\n")
puts(2,"This is the child process (stderr)\n")
while true do
    object o = gets(0)
--?o
    if atom(o) then exit end if
    puts(1,o)
--printf(1,"length(o)=%d\n",length(o))
--  sleep(1)
end while
puts(1,"Child: stdin done!\n")
puts(2,"Child: stderr done!\n")
--sleep(10)
--{} = wait_key()

