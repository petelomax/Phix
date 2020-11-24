-- The Computer Language Shootout Benchmarks
--   http://shootout.alioth.debian.org/
--
-- contributed by Jason Gade
--
-- run: exu wc.ex [file]
-- Note: Windows/DOS STDIN is broken

without warning
without type_check

constant NEWLINE    = '\n'
constant WHITESPACE = { ' ', '\t', NEWLINE }
constant EOF = -1

integer fn

atom char_count
atom word_count
atom line_count

integer in_word
integer ch

procedure wc()
char_count = 0
word_count = 0
line_count = 0
in_word = 0
ch = getc(fn)

while ch != EOF do

    char_count += 1
    
    if find(ch, WHITESPACE) then
        in_word = 0
        line_count += (ch = NEWLINE)
    else
        if in_word = 0 then
            in_word = 1
            word_count += 1
        end if
    end if
            
    ch = getc(fn)
end while
end procedure

for i=1 to 4000 do
--  fn = open("wcinput.txt","rb")
    fn = open("shootout/wcinput.txt","rb")
    wc()
    close(fn)
    if line_count!=25 then ?9/0 end if
    if word_count!=137 then ?9/0 end if
    if char_count!=6096 then ?9/0 end if
end for
--printf(1, "%d %d %d", {line_count, word_count, char_count})
-- 25 137 6096
--if getc(0) then end if
