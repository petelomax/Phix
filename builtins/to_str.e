--
-- to_str.e
--
-- Phix implementation of to_string (auto-include)
--

global function to_string(object data_in, integer string_quote=0, integer embed_string_quote='"')
        
    if string(data_in) then
        if string_quote = 0 then
            return data_in
        end if
        data_in = match_replace(`\`, data_in, `\\`)
        data_in = match_replace("\r", data_in, `\r`)
        data_in = match_replace("\n", data_in, `\n`)
--p2js (tough... I previously recommended using sprint() or printf(%v) instead in the docs anyway)
--      data_in = match_replace("\t", data_in, `\t`)
        data_in = match_replace({string_quote}, data_in, `\` & string_quote)
        return string_quote & data_in & string_quote
    end if
    
    if atom(data_in) then
        if integer(data_in) then
            return sprintf("%d", data_in)
        end if
        data_in = trim_tail(sprintf("%.15f", data_in), '0')
        if data_in[$] = '.' then
            data_in = remove(data_in, length(data_in))
        end if
        return data_in
    end if
    
    sequence data_out = "{"
    for i = 1 to length(data_in) do
        data_out &= to_string(data_in[i], embed_string_quote)
        if i != length(data_in) then
            data_out &= ", "
        end if
    end for
    data_out &= '}'
    
    return data_out
end function

