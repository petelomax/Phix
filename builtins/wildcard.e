-- Euphoria 2.4 (Phix compatible)
-- wild card matching for strings and file names

--/* Not required for Phix:
--include builtins\misc.e   -- constant LINUX
constant LINUX = 3
--*/

--/**/-- not strictly necessary, but reduces opCallOnce/fwd calls/onDeclaration
--pwa/p2js: removed...
--!/**/include builtins\pcase.e -- upper/lower()

--/* Not required for Phix (see pcase.e):
constant TO_LOWER = 'a' - 'A' 

global
function lower(object x)
-- convert atom or sequence to lower case
    return x + (x >= 'A' and x <= 'Z') * TO_LOWER
end function

global
function upper(object x)
-- convert atom or sequence to upper case
    return x - (x >= 'a' and x <= 'z') * TO_LOWER
end function
--*/

function qmatch(sequence p, sequence s)
-- find pattern p in string s
-- p may have '?' wild cards (but not '*')
integer k, pj

    if not find('?', p) then
        return match(p, s) -- fast
    end if
    -- must allow for '?' wildcard
    for i=1 to length(s)-length(p)+1 do
        k = i
        for j=1 to length(p) do
            pj = p[j]
            if  pj!='?'
            and pj!=s[k] then
                k = 0
                exit
            end if
            k += 1
        end for
        if k!=0 then return i end if
    end for
    return 0
end function

constant END_MARKER = -1

global function wildcard_match(sequence pattern, sequence str)
-- returns TRUE if string matches pattern
-- pattern can include '*' and '?' "wildcard" characters
integer p, f, t
sequence match_string
object pch

    pattern = pattern & END_MARKER
    str = str & END_MARKER
    p = 1
    f = 1
    while f<=length(str) do
--      if not find(pattern[p], {str[f], '?'}) then
        pch = pattern[p]
        if equal(pch,'*') 
        or (not equal(pch,str[f]) and 
            not equal(pch,'?')) then
            if pch!='*' then
                return 0
            end if
            while 1 do
                p += 1
                pch = pattern[p]
                if pch!='*' then exit end if
            end while
            if pch=END_MARKER then
                return 1
            end if
            match_string = ""
            while pch!='*' do
                match_string = match_string & pch
                if pch=END_MARKER then
                    exit
                end if
                p += 1
                pch = pattern[p]
            end while
            if pch='*' then
                p -= 1
            end if
            t = qmatch(match_string, str[f..length(str)])
            if t=0 then
                return 0
            end if
            f += t+length(match_string)-2
        end if
        p += 1
        f += 1
        if p>length(pattern) then
            return f>length(str)
        end if
    end while
    return 0
end function

global function wildcard_file(sequence pattern, sequence filename)
-- Return 1 (TRUE) if filename matches the wild card pattern.
-- Similar to DOS wild card matching but better. For example, 
-- "*ABC.*" in DOS will match *all* files, where this function will 
-- only match when the file name part has "ABC" at the end.

    if platform()!=LINUX then
        pattern = upper(pattern)
        filename = upper(filename)
    end if
    if not find('.', pattern) then
        pattern = pattern & '.'
    end if
    if not find('.', filename) then
        filename = filename & '.'
    end if
    return wildcard_match(pattern, filename)
end function

