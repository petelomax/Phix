--
-- pcase.e
-- =======
--
-- plain ASCII version of the more human-readable pcase8.e, see that for comments.
--
without debug -- keep ex.err clean (overshadowed by same in pdiag.e)

integer cinit cinit = 0
string toUpper, toLower

procedure initcase()
integer i32
    toUpper = repeat(255,255)
    toLower = repeat(255,255)
    for i=1 to 254 do
        toUpper[i] = i
        toLower[i] = i
    end for
    for i='A' to 'Z' do
        i32 = i+32
        toLower[i] = i32
        toUpper[i32] = i
    end for
    for i=#C0 to #D6 do -- see pcase8.e
        i32 = i+32
        toLower[i] = i32
        toUpper[i32] = i
    end for
    for i=#D8 to #DE do -- see pcase8.e
        i32 = i+32
        toLower[i] = i32
        toUpper[i32] = i
    end for

    -- several odd-balls, see pcase8.e
    toLower[#8A] = #9A
    toLower[#8C] = #9C
    toLower[#9F] = #FF
    toUpper[#9A] = #8A
    toUpper[#9C] = #8C
    toUpper[#FF] = #9F
    cinit = 1
end procedure

global function upper(object x)
object o
integer c
    if not cinit then initcase() end if
    if sequence(x) then
        for i=1 to length(x) do
            o = x[i]
            if sequence(o) then
                x[i] = upper(o)

            elsif integer(o) then
                c = o
                if c>0 and c<=255 then
                    x[i] = toUpper[c]
                end if
            end if
        end for
    elsif integer(x) then
        c = x
        if c>0 and c<=255 then
            x = toUpper[c]
        end if
    end if
    return x
end function

global function lower(object x)
object o
integer c
    if not cinit then initcase() end if
    if sequence(x) then
        for i=1 to length(x) do
            o = x[i]
            if sequence(o) then
                x[i] = lower(o)
            elsif integer(o) then
                c = o
                if c>0 and c<=255 then
                    x[i] = toLower[c]
                end if
            end if
        end for
    elsif integer(x) then
        c = x
        if c>0 and c<=255 then
            x = toLower[c]
        end if
    end if
    return x
end function

global function isupper(integer ch)
    if not cinit then initcase() end if
    return (ch>0 and ch<=255 and ch!=toLower[ch])
end function

global function islower(integer ch)
    if not cinit then initcase() end if
    return (ch>0 and ch<=255 and ch!=toUpper[ch])
end function

