--
-- pcore.e
--
--  common incudeset for p.exw and pgui.exw
--
include pglobals.e

--include builtins\pcase.e as pcase -- upper(), lower()
--/*
include builtins\peekstr.e          -- peek_string()
--*/

include builtins\ppp.e

include pops.e      -- opcode table

include pttree.e    -- ternary tree

include pmsgs.e -- Warnings/Warn/Abort/Duplicate/Expected/Fatal/Undefined

include ptok.e      -- tokeniser: getToken()

include pltype.e    -- localtypes handling

include psym.e      -- symbol table handler.

include pilx86.e    -- ilxlate(), (also includes psched.e)

include pemit.e     -- emitHexXX??[DEV] and finalfixups()
include pemit2.e    -- emitHexXX??[DEV] and finalfixups()

include pdebug.e    -- trace routines

include pmain.e     -- main parser

include profile.e   -- profile, profile_time, and opStats reporting

include plist.e     -- assembly and symbol table dump

