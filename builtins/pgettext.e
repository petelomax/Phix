--
-- pgettext.e
-- ==========
--
--  constants for use with get_text(integer fn, integer opt)
--
--  This file must be manually included, if needed that is(!!)
--  If you forget to incude this file, then eg get_text(fn,-1)
--  will automatically load pfileio.e, however a call such as
--  get_text(fn,CR_STRIPPED) will complain that CR_STRIPPED
--  has not been defined. It is coded this way because these
--  constants are not part of the language definition in the
--  way that say NULL or DB_OK are.
--
--  OR:: [DEV]
--      GT_WHOLE_FILE, GT_CR_STRIPPED, GT_LF_LEFT, GT_LF_LAST << better
--      (and put the following explaination/(Defined in psym.e) in
--       pfileio.e)
--
global constant
                    --
WHOLE_FILE   = -2,  -- Fastest. Returns file as one long string, ending in \n.
                    --  WHOLE_FILE (-2) leaves any embedded CR,LF,CRLF,LFCR as-is, 
                    --  whereas no CR are returned from the other options. Also,
                    --  dealing with such a string adds some obvious difficulties. 
                    --  WHOLE_FILE is however the fastest way to read a large file.
                    --
CR_STRIPPED  = -1,  -- Returns array of CRLF_stripped lines.
                    --
LF_LEFT      =  0,  -- Returns array of \n-terminated lines.
                    --  In the rather unlikely event that you actually care whether
                    --  a file has a trailing '\n' or not, LF_LEFT (0) yields the 
                    --  most accurate representation, with the obvious overhead of 
                    --  adding more complexity to the subsequent processing code.
                    --  Unless you have a really good reason, avoid using this.
                    --
LF_LAST      = +1   -- as 0, plus last line has \n added if missing.
                    --  CR_STRIPPED or LF_LAST are to be recommended for creating 
                    --  the simplest and most straightforward application code.
                    --  LF_LAST is great for processing lines character-by-character
                    --  because you can just check for \n and never worry about
                    --  length(lines[i]). CR_STRIPPED may be better, for example,
                    --  if you have a long list of words in a one-per-line format.
                    --

-- Example
-- =======
--
--  include pgettext.e
--  constant fn = open("builtins\\pgettext.e","r")
--  sequence rw, rs, rl
--  rw = get_text(fn,WHOLE_FILE)
--  ? length(rw)                        -- prints 2400(ish)
--  rs = get_text(fn,CR_STRIPPED)
--  ? length(rs)                        -- prints 54
--  ? rs[-1]                            -- prints "include pfileio.e"
--  rl = get_text(fn,LF_LAST)
--  ? length(rl)                        -- prints 54
--  ? rl[-1]                            -- prints "include pfileio.e\n"
--  
--include pfileio.e

--!/**/ text = gets({fn,-2})
--!/**/ text = get_text(fn,-2) -- -2=GT_WHOLE_FILE)

global function get_text(integer fn, integer opt)
    return gets({fn,opt})
end function

