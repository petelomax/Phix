without trace
--This file contains misc, sequence, find, and file operations.

--May 3, 2004  by Emlyn Merlyn

--This reads a file by its lines and return the result.
global function read_file(sequence name)
    integer fn   --fn = file number
    object line --The current line being read
    sequence lines
    
    lines = {}
    fn = open(name,"r") --Open the file
    if fn = -1 then
	return -1
    end if

    --Read the file
	line = gets(fn)  --Get either a line, or a -1
    while sequence(line) do
        if line[length(line)] = '\n' then
            line = line[1..length(line)-1]
        end if
	    lines = append(lines,line)  --Add the line to sequence lines
    	line = gets(fn)  --Get either a line, or a -1
    end while

    close(fn)    
    return lines
end function

--This routine removes all elements less than index.
global function remove_less_than(sequence seq, integer index)
    if index <= 1 then
	--Nothing happens
    elsif index > length(seq) then
	seq = {}
    else
	seq = seq[index..length(seq)]
    end if
    
    return seq
end function

--This routine removes all elements greater than index.
global function remove_greater_than(sequence seq, integer index)
    if index > length(seq) then
	--Nothing happens
    elsif index < 1 then
	seq = {}
    else
	seq = seq[1 .. index]
    end if
    
    return seq
end function

--Find an object in a sequence at a point from integer where
function find_greater(object what, sequence source, integer where)
    integer n
    source = remove_less_than(source,where)
    n = find(what,source)
    if n != 0 then
    	n += where - 1
    end if
    
    return n
end function

--Find an object in a sequence at a point up to integer where
function find_less(object what, sequence source, integer where)
    source = remove_greater_than(source,where)
    
    return find(what,source)
end function

--This routine removes a element from a sequence using sequence slicing.
--Pass the input to seq and the element number to remove to index
--When 'index' is "$" it removes the last element.
global function remove(sequence seq, object index)
    if equal(index, "$") then
        index = length(seq)
    end if
    if index < 1 or index > length(seq) then
        return seq
    end if

	return seq[1..index-1]&seq[index+1..length(seq)] --Remove it!
end function

--This routine removes all elements of a sequence that match object o.
global function remove_subscripts(sequence seq, object o)
    integer removing
    removing = 1
    
    while removing <= length(seq) do
    	if equal(seq[removing],o)  then  --If they match...
    	    seq = remove(seq,removing)        --...remove the subscript.
    	    --Don't add onto removing because the position is still the same.
    	else
    	    removing = removing + 1 --Don't remove it, move on.
    	end if
    end while
    
    return seq
end function

--This routine returns a list of all different members of a sequence
global function members(sequence seq)
    sequence m --For members
    m = {}
    
    for counter = 1 to length(seq) do
    	if find(seq[counter],m) = 0 then -- If it's new...
    	    m = append(m,seq[counter])   -- ...add it to the list
    	end if
    end for
    
    return m
end function

--This replaces all o1's in seq with o2's
global function replace_all(sequence seq, object o1, object o2)
    --o1 is what to find
    --o2 is what to replace all occurences of o1 with.

    for counter = 1 to length(seq) do
    	if equal(seq[counter],o1) then  -- If it equals o1...
    	    seq[counter] = o2                 -- ...replace it with o2
    	end if
    end for

    return seq
end function
