--
-- pcase8.e
-- ========
--
-- upper() and lower() routines for Phix.
--
-- Note: This is a human-readable utf8 version of pcase.e, any changes applied
--       here should be mirrored in that file. This file works fine on Windows,
--       the problem is that on Linux (the comments in) this file create a non-
--       ASCII list.asm, and it is not sensible to create utf8 listing files.
--
-- This is about 8.4x faster than the legacy sequence-op ones in wildcard.e
--  (obviously any timing results will vary wildly for different inputs).
--
-- High ascii mapping is based on this table, as once spied in a .err file:
--
-- 138'Š',140'Œ',159'Ÿ',
-- 154'š',156'œ',255'ÿ',
-- 192'À',193'Á',194'Â',195'Ã',196'Ä',197'Å',198'Æ',199'Ç',200'È',201'É',202'Ê',
-- 224'à',225'á',226'â',227'ã',228'ä',229'å',230'æ',231'ç',232'è',233'é',234'ê',
-- 203'Ë',204'Ì',205'Í',206'Î',207'Ï',208'Ð',209'Ñ',210'Ò',211'Ó',212'Ô',213'Õ',
-- 235'ë',236'ì',237'í',238'î',239'ï',240'ð',241'ñ',242'ò',243'ó',244'ô',245'õ',
-- 214'Ö',215'×',216'Ø',217'Ù',218'Ú',219'Û',220'Ü',221'Ý',222'Þ',
-- 246'ö',247'÷',248'ø',249'ù',250'ú',251'û',252'ü',253'ý',254'þ',
--
-- avoid 215'×'<-->247'÷', otherwise all of 192..222 <--> 224..254.
-- (bit unsure about 222'Þ'<-->254'þ', but left in).
--
-- Technical note:
--  lower(65.36) is 65.36, not 97.36 as it is in RDS Eu/OpenEu.
--
--!/**/without debug -- keep ex.err clean (overshadowed by same in pdiag.e)

integer cinit cinit = 0
--/**/string toUpper, toLower   --/* -- Phix
sequence toUpper, toLower       --*/ -- RDS

procedure initcase()
integer i32
--DEV lock as per pprntf.e:
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
    for i='À' to 'Ö' do -- see above table
        i32 = i+32
        toLower[i] = i32
        toUpper[i32] = i
    end for
--  -- (missing out 215'×'<-->247'÷' here)
    for i='Ø' to 'Þ' do -- see above table
        i32 = i+32
        toLower[i] = i32
        toUpper[i32] = i
    end for

    -- several odd-balls, see above table
    toLower['Š'] = 'š'
    toLower['Œ'] = 'œ'
    toLower['Ÿ'] = 'ÿ'
    toUpper['š'] = 'Š'
    toUpper['œ'] = 'Œ'
    toUpper['ÿ'] = 'Ÿ'
--  -- and a couple of corrections, ""
--  toLower['×'] = '×'
--  toLower['÷'] = '÷'
--  toUpper['×'] = '×'
--  toUpper['÷'] = '÷'

    cinit = 1
end procedure

function upper8(object x)
object o
integer c --DEV see notes below
    if not cinit then initcase() end if
    if sequence(x) then
        for i=1 to length(x) do
            o = x[i]
            if sequence(o) then
                x[i] = upper8(o)

            elsif integer(o) then
                c = o
                if c>0 and c<=255 then
                    x[i] = toUpper[c]
                end if
--          elsif integer(o) and o>0 and o<=255 then
--              x[i] = toUpper[o]
            end if
        end for
    elsif integer(x) then
        c = x
        if c>0 and c<=255 then
            x = toUpper[c]
        end if
--  elsif integer(x) and x>0 and x<=255 then
--      x = toUpper[x]
    end if
    return x
end function

function lower8(object x)
object o
integer c -- ditto
    if not cinit then initcase() end if
    if sequence(x) then
        for i=1 to length(x) do
            o = x[i]
            if sequence(o) then
                x[i] = lower8(o)
            elsif integer(o) then
                c = o
                if c>0 and c<=255 then
                    x[i] = toLower[c]
                end if
--          elsif integer(o) and o>0 and o<=255 then
--              x[i] = toLower[o]
            end if
        end for
    elsif integer(x) then
        c = x
        if c>0 and c<=255 then
            x = toLower[c]
        end if
--  elsif integer(x) and x>0 and x<=255 then
--      x = toLower[x]
    end if
    return x
end function

function isupper8(integer ch)
    if not cinit then initcase() end if
    return (ch>0 and ch<=255 and ch!=toLower[ch])
end function

function islower8(integer ch)
    if not cinit then initcase() end if
    return (ch>0 and ch<=255 and ch!=toUpper[ch])
end function

-- DEV: (re integer c) performancewise, the commented out versions work fine 
--  when compiled, but not as well when interpreted (ie using opJcc etc).
--  (All because pltype.e is not used during interpretation, see NOLT.)
-- [What I should really do is test NOLT impact on interpretation performance,
--  and if neglible then ditch it... And at the time time I should re-evaluate 
--  the "no gvar scan when interpreted" thing, ditto.]
