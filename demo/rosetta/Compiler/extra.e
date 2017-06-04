--
-- demo\rosetta\Compiler\extra.e
-- =============================
-- 
--  Routines to reload human-readable files (deviation from task requirement)

--The following can be used to load .lex files, as created by lex.exw:
-- (in place of the existing get_tok() in parse.e)
function get_tok()
    string line = trim(gets(input_file))
    sequence tok = split(line,' ',limit:=4,no_empty:=1)
    integer k = find(tok[3],tkNames)
    if k=0 then ?9/0 end if
    tok[3] = k
    return tok
end function


--The following can be used to load .ast files, as created by parse.exw:
-- (in place of the existing lex()/parse() pairs in cgen.exw and interp.exw)
function load_ast()
    string line = trim(gets(input_file))
    -- Each line has at least one token
    sequence node = split(line,' ',limit:=2,no_empty:=1)
 
    string node_type = node[1]
 
    if node_type == ";" then -- a terminal node
        return NULL
    end if

    integer n_type = find(node_type,tkNames)
    if n_type=0 then ?9/0 end if
 
    -- A line with two tokens is a leaf node
    -- Leaf nodes are: Identifier, Integer, String
    -- The 2nd token is the value
    if length(node)>1 then
        node[1] = n_type
        if n_type=tk_Integer then
            node[2] = to_integer(node[2])
        elsif n_type=tk_String then
            node[2] = unquote(node[2])
        end if
        return node
    end if
    object left = load_ast()
    object right = load_ast()
    return {n_type, left, right}
end function

