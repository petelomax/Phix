--
--  pseries.e
--  =========
--      Phix implementation of series (auto-include)
--      Stolen from OpenEuphoria's std/sequence.e
--

--**
-- checks whether two objects can perform a sequence operation together.
--
-- Parameters:
--              # ##a## : one of the objects to test for compatible shape
--              # ##b## : the other object
--
-- Returns:
--      An **integer**, 1 if a sequence operation is valid between ##a## and ##b##, else 0.
--
-- Example 1:
-- <eucode>
-- i = binop_ok({1,2,3},{4,5})
-- -- i is 0
--
-- i = binop_ok({1,2,3},4)
-- -- i is 1
--
-- i = binop_ok({1,2,3},{4,{5,6},7})
-- -- i is 1
-- </eucode>
--
-- See Also:
--     [[:series]]

--global 
function binop_ok(object a, object b)
    if atom(a) or atom(b) then
        return 1
    end if

    if length(a)!=length(b) then
        return 0
    end if

    for i=1 to length(a) do
        if not binop_ok(a[i], b[i]) then
            return 0
        end if
    end for

    return 1
end function


--**
-- returns a new sequence built as a series from a given object.
--
-- Parameters:
--              # ##start## : the initial value from which to start
--              # ##increment## : the value to recursively add to ##start## to get new elements
--              # ##count## :  an integer, the number of items in the returned sequence. The default is 2.
--              # ##operation## :  an integer, the type of operation used to build the series.
--                         Can be either '+' for a linear series or '*' for a geometric series.
--                         The default is '+'.
--
-- Returns:
--              An **object**, either 0 on failure or a sequence containing the series.
-- 
--
-- Comments:
-- * The first item in the returned series is always ##start##.
-- * A //linear// series is formed by **adding** ##increment## to ##start##.
-- * A //geometric// series is formed by **multiplying** ##increment## by ##start##.
-- * If ##count## is negative, or if ##start## **##op##** ##increment## is invalid,
-- then 0 is returned. Otherwise, a sequence, of length
-- ##count+1##, staring with ##start## and whose adjacent elements differ
-- by ##increment##, is returned.
--
-- Example 1:
-- <eucode>
-- s = series( 1, 4, 5)
-- -- s is {1, 5, 9, 13, 17}
-- s = series( 1, 2, 6, '*')
-- -- s is {1, 2, 4, 8, 16, 32}
-- s = series({1,2,3}, 4, 2)
-- -- s is {{1,2,3}, {5,6,7}}
-- s = series({1,2,3}, {4,-1,10}, 2)
-- -- s is {{1,2,3}, {5,1,13}}
-- </eucode>
--
-- See Also:
--     [[:repeat_pattern]]

global function series(object start, object increment, integer count = 2, integer op = '+')
sequence result

    if count<0 then
        return 0
    end if

    if not binop_ok(start, increment) then
        return 0
    end if

    if count=0 then
        return {}
    end if

    result = repeat(0, count)
    result[1] = start
    switch op do
        case '+' then
            for i=2 to count do
                start = sq_add(start,increment)
                result[i] = start
            end for

        case '*' then
            for i=2 to count do
                start = sq_mul(start,increment)
                result[i] = start
            end for

        case else
            return 0
    end switch
    return result
end function


