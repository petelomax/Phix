-- Euphoria 2.4
-- Sorting

-- Sort the elements of a sequence into ascending order, using "Shell" sort.

--DEV wrong one for newEmit
--!/**/include builtins\pcfunc.e

--defined in psym.e:
--global enum ASCENDING = -1,  NORMAL_ORDER = ASCENDING,
--           DESCENDING = +1, REVERSE_ORDER = DESCENDING

global function sort(sequence x, integer order=ASCENDING)
--
-- Sort a sequence into ascending order. The elements can be atoms or 
-- sequences. The standard compare() routine is used to compare elements.
--
integer gap, j, first, last
object tempi, tempj

--DEV temp!
--?"sort deep_copy..."
--?x
--x = deep_copy(x)
    last = length(x)
    gap = floor(last/10)+1
    while 1 do
        first = gap+1
        for i=first to last do
            tempi = x[i]
            j = i-gap
            while 1 do
                tempj = x[j]
--              if tempi >= tempj then
--              if compare(tempi, tempj)>=0 then
                if compare(tempi, tempj)!=order then
                    j += gap
                    exit
                end if
                x[j+gap] = tempj
                if j<=gap then
                    exit
                end if
                j -= gap
            end while
            x[j] = tempi
        end for
        if gap=1 then
            return x
        else
            gap = floor(gap/3.5)+1
        end if
    end while
end function

function tagsort(integer i, integer j, sequence data)
-- standard function for a standard tagsort
--26/6/20
--  return compare(data[i],data[j])
    integer c = compare(data[i],data[j])
    if c=0 then c = compare(i,j) end if
    return c
end function
--DEV/SUG column_tagsort???

global function custom_sort(object custom_compare, sequence x, object data = {}, integer order = ASCENDING)
--
-- Sort a sequence. A user-supplied comparison function is used to compare elements. 
-- Note that this sort is not "stable", i.e. elements that are considered equal might 
-- change position relative to each other.
--
    integer gap, j, first, last
    object tempi, tempj

    sequence args = {0,0}
    if atom(data) then
        args &= data
--DEV... (broke self hosting... wtf??) [it ain't psym... afaict anyway] [seems to have righted itself 14/4/21...]
    elsif data!={} then
--  elsif length(data)!=0 then
        if length(data)!=1 then ?9/0 end if
        args = append(args, data[1])
    elsif sequence(custom_compare) then
        --DEV For now. We could add order to tagset() above,
        --             and/or implement that column_tagsort?
        if data!={} or order!=ASCENDING then ?9/0 end if
        args = append(args,custom_compare)
        custom_compare = tagsort
    end if

    last = length(x)
    gap = floor(last/10)+1
    while 1 do
        first = gap+1
        for i=first to last do
            tempi = x[i]
            args[1] = tempi
            j = i-gap
            while 1 do
                tempj = x[j]
                args[2] = tempj
--              if call_func(custom_compare, {tempi, tempj})>=0 then
                if call_func(custom_compare, args)!=order then
--              integer c = call_func(custom_compare, args)
--              if c!=order then
--?{"j+=gap",c,order}
                    j += gap
                    exit
                end if
                x[j+gap] = tempj
                if j<=gap then
                    exit
                end if
                j -= gap
            end while
            x[j] = tempi
        end for
        if gap=1 then
            return x
        else
            gap = floor(gap/3.5)+1
        end if
    end while
end function

function column_compare(object a, object b, sequence cols)
-- Local function used by sort_columns()
    for i=1 to length(cols) do
        integer sgn = 1,
                column = cols[i]
        if column<0 then
            sgn = -1
            column = -column
        end if
        if column<=length(a) then
            if column>length(b) then return -sgn end if
            integer c = compare(a[column], b[column])
            if c!=0 then return sgn*c end if
        else
            if column>length(b) then exit end if
            return sgn
        end if
    end for
    return 0
end function

--**
-- Sort the rows in a sequence according to a user-defined
-- column order.
--
-- Parameters:
-- # ##x## : a sequence, holding the sequences to be sorted.
-- # ##column_list## : a list of columns indexes ##x## is to be sorted by.
--
-- Returns:
--   A **sequence**, a copy of the original sequence in sorted order.
--
-- Comments:
--
-- ##x## must be a sequence of sequences.
--
-- A non-existent column is treated as coming before an existing column. This
-- allows sorting of records that are shorter than the columns in the
-- column list.
--
-- By default,
-- columns are sorted in ascending order. To sort in descending
-- order, make the column number negative.
--
--  This function uses the "Shell" sort algorithm.
-- This sort is not "stable", i.e. elements that are considered equal might
-- change position relative to each other.
--
-- Example 1:
--   <eucode>
--   sequence dirlist
--   dirlist = dir(`c:\temp`)
--   sequence sorted
--   -- Order is Size:descending, Name
--   sorted = sort_columns( dirlist, {-D_SIZE, D_NAME} )
--   </eucode>
--
-- See Also:
--   [[:compare]], [[:sort]]

global function sort_columns(sequence x, sequence column_list)
    x = custom_sort(column_compare, x, {column_list})
    return x
end function

