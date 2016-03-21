--
-- pvlookup.e
--

global function vlookup(object find_item, sequence grid_data, integer source_col, integer target_col, object def_value = 0)
object gdi

    for i=1 to length(grid_data) do
        gdi = grid_data[i]
        if sequence(gdi) 
        and source_col<=length(gdi)
        and equal(find_item, gdi[source_col]) then
            if target_col>length(gdi) then exit end if
            return gdi[target_col]
        end if
    end for
    
    return def_value

end function
