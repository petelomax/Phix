-----------------------
-- PRINT.E  v1.2     --
-- by Gabriel Boehme --
-- updated 3/28/2000 --
-----------------------

-- This is an include file with a redefined print() routine, one that prints
-- string-valued sequences as strings.  For example, normally when you code
-- something like this...
-- 
--      print(1, {5,"Hi!\n"})
-- 
-- ...you would get this:
-- 
--      {5,{72,105,33,10}}
-- 
-- However, if you include print.e before this statement, you'll get this:
-- 
--      {5,"Hi!\n"}
-- 
-- This is far more readable, and takes up less space, too.  The redefined
-- print() will find strings at *any* depth in a sequence, and it runs in about
-- the same time as the built-in print() -- faster in some cases!
-- 
-- Since it prints the strings out in a Euphoria-readable fashion, get() will
-- still read your data just the same as it did before.  Basically, you just
-- include print.e at the top of your program, and you don't have to change
-- another line of code!


-- Change History
-- --------------
-- v1.2   in "print", replaced "original_print" stuff with a "printf" statement
--
--
-- Distribution Status
-- -------------------
-- This software is being contributed to the Public Domain.
-- Feel free to use it, abuse it, and/or modify it for any purpose.
-- Send bug reports or feature requests to:
--      gabrielboehme@hotmail.com
--
--
-- Disclaimer
-- ----------
-- I am in no way responsible for any damage, data loss, or any other
-- adverse effects resulting from the use/abuse/etc. of this library.
--
--
-- Acknowledgments
-- ---------------
-- Thanks to Wayne Overman, for convincing me that this was worth sharing.
--
-- Thanks also to Robert Craig, for the beauty of Euphoria.


type string(sequence s)
-- verify that we have a valid Euphoria-representable string
-- (ASCII chars 32-255, 9, 10, 13)
object x
   for i = 1 to length(s) do
      x = s[i]
      if integer(x) then
	 if x < ' ' then
	    if not find(x, "\t\n\r") then
	       return 0
	    end if
	 elsif x > 255 then
	    return 0
	 end if
      else
	 return 0
      end if
   end for
   return 1
end type

integer fn

constant ESCAPE_STRING = {"\\n", "\\t", "\\\"", "\\\\", "\\r"},
	 ESCAPED_CHARS = {'\n',  '\t',  '"',    '\\',   '\r'}

procedure PrintEuString(sequence s)
integer c, f
   puts(fn, '"')
   for i = 1 to length(s) do
      c = s[i]
      f = find(c, ESCAPED_CHARS)
      if f then
	 puts(fn, ESCAPE_STRING[f])
      else
	 puts(fn, c)
      end if
   end for
   puts(fn, '"')
end procedure

procedure Print(sequence s)
integer len
object x
   len = length(s)
   if len and string(s) then
      -- print the sequence out as a string
      PrintEuString(s)
   else
      -- print the sequence out as...well, a sequence
      puts(fn, '{')
      for i = 1 to len do
	 x = s[i]
	 if atom(x) then
	    -- print out the numeric value
	    print(fn, x)
	 else
	    Print(x)
	 end if
	 if i < len then
	    puts(fn, ',')
	 end if
      end for
      puts(fn, '}')
   end if
end procedure

without warning

global procedure print(integer file, object x)
-- print out any Euphoria object, with strings in quotes
   if atom(x) then
      -- just an atom, printf the value
      printf(file, "%g", x)
   else
      -- a sequence!
      fn = file
      Print(x)
   end if
end procedure

with warning

