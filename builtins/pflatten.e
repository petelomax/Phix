--
-- builtins\pflatten.e
-- ===================
--
--  flatten: Remove all nesting from a sequence.
--  join: Concatenate all elements of a sequence.
--  join_by: Interleave elements of a sequence.
--  join_path: Concatenate all elements of a path.
--

global function flatten(sequence s)
sequence res = ""
object si

    for i=1 to length(s) do
        si = s[i]
        if atom(si) 
        or string(si) then
            res &= si
        else
            res &= flatten(si)
        end if
    end for
    return res
end function

global function join(sequence s, object delim=" ")
sequence res = ""

    for i=1 to length(s) do
        if i!=1 then
            res &= delim
        end if
        res &= s[i]
    end for
    return res
end function

global function join_by(sequence s, integer step, integer n, object step_pad="   ", object n_pad="\n")
sequence res = {}
integer nmax = n
    while length(s)>=step do
--  while length(s)>step do     -- (needed for auto-widthwise partial<=step)
        for i=1 to step do
            for j=1 to n-1 do
                if (j+1)*step<=length(s) then
                    integer jdx = i+j*step
                    s[i] = join({s[i],s[jdx]},step_pad)
                elsif nmax=n then
                    nmax = j
                end if
            end for
        end for
        res = append(res,join(s[1..step],n_pad)&n_pad)
        n = nmax
        s = s[step*n+1..$]
    end while
    if length(s) then
        res = append(res,join(s,n_pad)&n_pad)
        -- auto-widthwise partial<=step:
--      res = append(res,join(s,iff(length(s)<=step?step_pad:n_pad))&n_pad)
    end if
    return join(res,n_pad)
end function

global function join_path(sequence path_elements, integer trailsep=0)
integer SLASH = iff(platform()=WINDOWS?'\\':'/')
string elem, fname = ""

    for i=1 to length(path_elements) do

        elem = path_elements[i]

        if length(elem) and find(elem[$],"\\/") then
            elem = elem[1..$-1]
        end if

        if length(fname) and length(elem) and elem[1]!=SLASH then
            if platform()!=WINDOWS
            or elem[$]!=':' then
                elem = SLASH & elem
            end if
        end if

        fname &= elem

    end for

    if trailsep and length(fname) and fname[$]!=SLASH then
        fname &= SLASH
    end if

    return fname
end function

