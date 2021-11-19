--
-- builtins/progress.e
--
--  A simple console-based progress indicator.
--
--  Adds progress messages to a console app, but
--   does not change the final output in any way.
--
--  See docs for a couple of demos and gui alternatives.
--
without js

integer lp = 0 -- (last progress length, to be wiped out)

global procedure progress(string msg, sequence args = {})
    if length(args) then
        msg = sprintf(msg,args)
    end if
    integer lm = length(msg)
    if lm=0 then
        if lp then
            puts(1,repeat(' ',lp)&"\r")
        end if
        lp = 0
    else
        if find(msg[$],"\r\n")=0 then
            msg &= "\r"
        end if
        if lp>lm then
            msg[-1..-2] = repeat(' ',lp-lm)
        end if
        puts(1,msg)
        lp = iff(msg[$]='\r' ? lm : 0)
    end if
end procedure

