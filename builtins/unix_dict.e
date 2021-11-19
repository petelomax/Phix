--
-- builtins\unix_dict.e (an autoinclude)
--
-- Implements unix_dict(integer minlen=0), which simply returns a list of words,
--  optionally filtered to only include words of length minlen or longer.
--  No attempt is (yet) made to change the case, or filter hyphenated or
--  spaced words or those containing numerical digits (eg "10th"), etc.
--
-- Note that pwa\builtins\unix_dict.js is constructed in pwa\p2js.exw,
-- routine rebuild_builtins_and_run_tests(), from unixdict.txt not this,
-- and will be rebuilt when you run that if you delete it (recommended),
-- or modify/"touch" this file, but not when demo\unixdict.txt is changed, 
-- and it will /not/ incorporate any changes made here. Instead of reading
-- unixdict.txt, unix_dict.js simply contains a copy of it, in JavaScript
-- or rather p2js.js compatible syntax form, ie ["sequence","a",...], all
-- 25107 words or 284K of it.
--
global function unix_dict(integer minlen=0, string filename="unixdict.txt")
    function min_len(string word, integer len)
        return length(word)>=len
    end function
    string root = get_file_path(get_interpreter()),
           path = join_path({root,"demo",filename})
    sequence res = get_text(path,GT_LF_STRIPPED)
    if minlen then
        res = filter(res,min_len,minlen)
    end if
    return res
end function
