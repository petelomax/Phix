--
-- pdecodeflags.e
-- ==============
--
--  Phix implementation of decode_flags (autoinclude)
--

global function decode_flags(sequence FlagSet, atom v, string sep="+")
--
--  Given a definition such as:
--
--      constant FileFlagSet = {{ FILE_ATTRIBUTE_READONLY,  "R"},
--                              { FILE_ATTRIBUTE_SYSTEM,    "S"},
--                              { FILE_ATTRIBUTE_HIDDEN,    "H"},
--                              { FILE_ATTRIBUTE_DIRECTORY, "D"}}
--  then
--
--      ? decode_flags(FileFlagSet,#17,"")
--
--  yields/displays "RSHD". Just add the obvious "ead" etc and, with the default sep
--  (of "+") the output would instead be "Read+System+Hidden+Directory". (FYI, those
--  "FILE_ATTRIBUTE_XXX" flags used above are defined in arwen\constants.e, should
--  you want to check precisely why I said #17 rather than (eg) #0F)
--
--  This is useful anywhere you have a bitmap field and want to show a human-readable 
--  version. Further examples can be found in demo\arwendemo\filedump.exw
--
--  Obviously each application must define it's own FlagSet(s).
--  The order of FlagSet determines the order things appear in the result.
--  If a description begins with '-', the separator(optional p3) is omitted. 
--  A decription(s) of "" can be used to ignore/suppress specific bit settings.
--  The first entry can be eg {0,"closed"} to specify the "no bits set" meaning.
--  Any bits not recognised are returned as a hex value at the start.
--
atom flag, fset
string desc
string res = ""

    for i=1 to length(FlagSet) do
        desc = FlagSet[i][2]
        flag = FlagSet[i][1]
        if flag=0 then
            if i!=1 then ?9/0 end if -- sanity check
            -- (I suspect ^ means there is some logic error setting up FlagSet,
            -- (else I suppose you could just ignore entries>1 with a flag of 0.)
            if v=0 then
                res = desc
                exit
            end if
        else
            fset = and_bits(v,flag)
--          if fset=flag then                   -- (oops, sign issues with eg #80000000)
--1/10/19:
--          if fset=and_bits(flag,flag) then
--              v -= flag
            if fset!=0 then
                v -= fset
                if length(res)!=0
                and length(sep)!=0
                and length(desc)>=1 
                and desc[1]!='-' then
                    res &= sep
                end if
                res &= desc
            end if
        end if
    end for
    if v!=0 then
        if sep="" then sep = "+" end if
        res = sprintf("0x%x%s%s",{v,sep,res})
    end if
    return res
end function

