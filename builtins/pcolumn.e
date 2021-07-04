--
-- pcolumn.e
-- =========
--

global function columnize(sequence source, object columns={}, defval=0)
--
-- Convert a set of sequences into a set of columns.
--
-- Any atoms found in source are treated as if they were a 1-element sequence.
-- The optional columns parameter can be a specific column number or an ordered set.
-- The default value is used when some elements of source have different lengths.
--
-- Examples
--  ?columnize({{1, 2}, {3, 4}, {5, 6}})            -- {{1,3,5}, {2,4,6}}
--  ?columnize({{1, 2}, {3, 4}, {5, 6, 7}})         -- {{1,3,5}, {2,4,6}, {0,0,7}}
--  ?columnize({{1}, {2}, {3, 4}},defval:=-999)     -- {{1,2,3}, {-999,-999,4}}
--  ?columnize({{1, 2}, {3, 4}, {5, 6, 7}}, 2)      -- {{2,4,6}}
--  ?columnize({{1, 2}, {3, 4}, {5, 6, 7}}, {2,1})  -- {{2,4,6}, {1,3,5}}
--  ?columnize({"abc", "def", "ghi"},defval:=' ')   -- {"adg", "beh", "cfi" }
--
--  (Specifying a default value of <SPACE> on the last example causes it to
--   return a sequence of strings, rather than dword-sequences, which might
--   display as {{97,100,103},{98,101,104},{99,102,105}}, or cause unwanted
--   type checks if the result is later processed using a string variable.)
--
sequence result
integer ncolumns, col
object sj
integer k

    if not sequence(columns) then
        columns = {columns}
----p2js: (untried)
--  else
--      columns = deep_copy(columns)
    end if

    ncolumns = length(columns)
    if ncolumns=0 then
        ncolumns = 0
        for j=1 to length(source) do
            sj = source[j]
            if atom(sj) then
                if ncolumns=0 then
                    ncolumns = 1
                end if
            else
                k = length(sj)
                if ncolumns<k then ncolumns = k end if
            end if
        end for
--p2js:
--      for i=1 to ncolumns do
--          columns &= i
--      end for
        columns = tagset(ncolumns)
    end if

    result = repeat(repeat(defval,length(source)), ncolumns)
    for i=1 to ncolumns do
        col = columns[i]
        for j=1 to length(source) do
            sj = source[j]
            if atom(sj) then
                if col=1 then
                    result[i][j] = sj
                end if
            elsif col<=length(sj) then
                result[i][j] = sj[col]
            end if
        end for
    end for

    return result
end function

