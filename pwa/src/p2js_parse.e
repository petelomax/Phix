--
-- p2js_parse.e
--

-- program := ast;
-- ast := {declaration|statement}.
-- declaration := '{' T_xxx ',' ( T_ident | '{' T_ident [',' T_ident] '}' ) '}'.
-- T_xxx := T_integer|T_atom|T_string|T_sequence|T_object. // (nb no udt here)

--DEV I think we need special toktypes here...
--enum /*INCLUDE,*/ VARIABLE, STRICT, /*OPERATOR,*/ /*QU,*/ /*IF,*/ /*FOR,*/
-- SPACE, SYMBOL, 
--   STMAX = 
--$
with trace

constant vartypes = {T_integer,T_atom,T_string,T_sequence,T_object,
                     T_bool,T_boolean,T_dictionary,T_int,
                     T_Ihandle,T_Ihandln,T_cdCanvas,T_atom_string,
--                   T_nullable_string,T_timedate,T_constant,T_static},
                     T_nullable_string,T_constant,T_static,T_mpq,T_mpz,T_mpfr},
         jstypes = {T_const,T_let,T_var}
--DEV something in p2js_scope instead:
sequence udts = {}

--       rtntypes = {T_function,T_procedure}    -- nb no type (or error in rtndef()?)
--       rtntypes = {T_function,T_procedure,T_type}

--DEV these cannot clash with SYMBOL etc: they *MUST* be in that enum...
--/*
-- aside: by making this a sequence rather than a constant, we ensure
--        it shows in the ex.err, as a handy little lookup reference.
sequence s3 = {{INCLUDE,"??INCLUDE",false}, -- include statement
               {VARIABLE,"VARIABLE",false}, -- variable definition
               {STRICT,"STRICT",false},     -- "use strict"; (js only)
               {OPERATOR,"?OPERATOR",false}, -- eg ???
--             {QU,"?",false},              -- ? statement
               {IF,"???IF",false},          -- if statement
               {FOR,"???FOR",false},        -- for statement
--             {SYMBOL,"SYMBOL",false},     -- General symbols !&|*+,.:;<=>?~\$%
--X            {DQUOTE,"DQUOTE",false},     -- Double quotation mark
--             {ILLEGAL,"ILLEGAL",false},   -- Illegal character
--             {DIGIT,"DIGIT",false},       -- 0..9
--             {LETTER,"LETTER",false},     -- A..Z,a..z
----               {HEXSTR,"HEXSTR",false},     -- Hexadecimal Byte String
--             {COMMENT,"COMMENT",false},   -- Phix only, -- .. \n
--             {COMMENT,"COMMENT",true},    -- Phix only, -- .. \n
--             {SPACE,"SPACE",false},       -- 
            },
         {st_chk, st_names, show_st} = columnize(s3)
    if st_chk!=tagset(STMAX) then ?9/0 end if
    --?{st_chk,st_names,show_st} {} = wait_key()
--*/

-- now in pbasics:
--integer tdx -- index to tokens[]/terminate parser

--Xinteger cdrop -- (for warning msg)
--sequence clines

bool parse_bad = false
global function parse_error(object tok=0, string reason="")
    --
    -- parse_error(tok,"why") displays why, sets flag and returns false.
    -- parse_error() returns false or true if "" has happened.
    --
--  static bool parse_bad = false   -- not supported by p2js [DEV, erm, why not??!!]

    if sequence(tok) then
        show_token(tok,reason)
        parse_bad = true
    elsif tok=-1 then
        parse_bad = false
    end if
    return parse_bad
end function

sequence ween
procedure warn(sequence tok, string reason, sequence args={})
    if tok[TOKTYPE]=LETTER then
        integer ttidx = tok[TOKTTIDX]
        if find(ttidx,ween) then return end if
        ween &= ttidx
    end if
    if length(args) then reason = sprintf(reason,args) end if
    show_token(tok,reason)
end procedure

--/*
--function expression(integer p)
--  sequence expr = {}
--  while true do
--      sequence tok = tokens[tdx]
--      integer {toktype,start,finish,line} = tok
--      switch toktype do
--          case SPACE:
--              break
--          case COMMENT:
----[DEV]       expr[2] &= comment..
----                expr = append(expr,tok)
--              cdrop += 1
--              if length(clines)<10 then
--                  clines = append(clines,sprintf("%d",line))
--              elsif length(clines)=10 then
--                  clines = append(clines,"...")
--              end if  
--              break
--          case LETTER:
--              string s = src[start..finish]
--              if s="include" then
----                if ttidx=T_include then
----                    return parse_error(tok,"illegal (include mid-expression?)")
--                  ?9/0
--              end if
--              ?9/0
----                return parse_error(tok,"not yet implemented...")
----/!*
--              if ttidx = T_include then
--                  phix_only()
--                  s = ""
--                  while true do
--                      tok = tokens[tdx]
--                      {toktype,start,finish} = tok
--                      if tok[4]!=line
--                      or (toktype!=LETTER
--                      and (toktype!=SYMBOL or src[start]!='.')) then
--                          exit
--                      end if
--                      s &= src[start..finish]
--                      tdx += 1
--                  end while
----                    tdx -= 1        
--                  ast = append(ast,{T_include,s})
----?ast
--                  exit
--              elsif find(ttidx,vartypes) then
--                  ast = append(ast,vardef(s))
--                  exit
--              else
--                  return parse_error(tok,"unrecognised")
--              end if
----                ast = append(ast,tok)
----*!/
--          case '-':
--              tdx += 1
----DEV 0??
--              expr = append(expr,{toktype,tdx,expression(0)})
--              exit
--          case DIGIT:
--              expr = append(expr,tok)
----temp:
--              tdx += 1
--              exit
--          default: 
----                ?9/0
----                exit
--              return parse_error(tok,"unrecognised")
--      end switch
--  end while
--  return expr
--end function
--*/

--/!*

function expect(integer ttidx)
-- returns false on error, true if all ok
    sequence tok = tokens[tdx]
    integer {toktype,start,finish,line} = tok
--DEV debug:
--string tt = tok_name(toktype),
--     ttval = src[start..finish]   
    if toktype!=LETTER
    or tok[TOKTTIDX]!=ttidx then
        string name = get_ttname(ttidx)
        return not parse_error(tok, name & " expected")
    end if
    tdx += 1
--  return not parse_bad
    return true
end function

--procedure expectt(object t, bool bOptional=false)
procedure expectt(integer t, bool bOptional=false)
--
-- t may be eg ')', if only one option is valid here,
--       or eg {'=',BEQ /* (aka `:=`) */}, if several options are valid here
--
    if tdx>length(tokens) then
        if bOptional then return end if
--      {} = parse_error(tok, t & " expected")
--untried...
--      {} = parse_error(tokens[$], t & " expected")
        {} = fatal("unexpected eof")
--      ?9/0
        return
    end if
    sequence tok = tokens[tdx]
--  integer {toktype,start,finish,line} = tok
--  integer {toktype} = tok
    integer toktype = tok[TOKTYPE]
    if toktype!=t then
--  if (integer(t) and toktype!=t)
--  or (not integer(t) and not find(toktype,t)) then
--  if tok[TOKTYPE]!=t then
        if bOptional then return end if
--      {} = parse_error(tok, toktype & " expected")
--      if t>127 or ??? --DEV...
--      if t>127 then
--          t = get_ttname(t)
--      end if
--      {} = parse_error(tok, t & " expected")
        -- aside: T_string and T_nullable_string are never "expected",
        --        and neither are any ops BEQ(=129)..ARGS(=203).
        {} = parse_error(tok, iff(t>127?get_ttname(t):t) & " expected")
        ?9/0
--      return
    end if
    tdx += 1
end procedure

function expects(sequence s)
    for i=1 to length(s) do
        integer ttidx = s[i]
        if not expect(ttidx) then
            sequence tok = tokens[tdx]
            return not parse_error(tok,get_ttname(ttidx)&" expected")
        end if
    end for
    return true
end function

forward function expr(integer p, skip=0)

integer calling_the_rtn = 0

function rcall(object fp, sequence tok)
--  sequence res = {"PCALL",tok}
--  sequence res = {fp,tok}
    sequence res = {tok}
    integer ttidx = tok[TOKTTIDX]
    if ttidx=T_iif then ttidx = T_iff end if
    integer was_calling_the_rtn = calling_the_rtn
    calling_the_rtn = ttidx
    expectt('(')
--  while toktype[tdx][TOKTYPE]=LETTER do
--      args = append(args,vardef(tdx,1))
--  end while
    if tokens[tdx][TOKTYPE]!=')' then
        while true do
            res = append(res,expr(0))
--DEV factor this out, somehow:?
--          tdx += 1
--          tok = tokens[tdx+1]
--          {toktype,start,finish} = tok
--          if toktype!=',' then exit end if
            integer ttype = tokens[tdx][TOKTYPE]
            if ttype!=',' then
                if ttidx=T_iff and ttype='?' then
                    ttidx = T_iif
                elsif ttidx!=T_iif or ttype!=':' then
                    exit
                end if
            end if
            tdx += 1
        end while
    end if
    calling_the_rtn = was_calling_the_rtn
    expectt(')')
--  return {{fp,res}}
    return {fp,res}
end function

function skip_comments()
--DEV make a count somewhere, and tag them onto something???
--DOC Mid-line comments attached to individual tokens do not appear in the Parse Tree, 
--    only those comments that appear between statements and declarations are shown.
    while tdx<=length(tokens) do
        integer toktype = tokens[tdx][TOKTYPE]
        if toktype!=COMMENT
        and toktype!=BLK_CMT then
            return toktype
        end if
        tdx += 1
    end while
    return 0 -- EOL?
end function

--DEV relocate?
include p2js_scope.e

bool allow_nested_constants = false,
     found_nested_constants = false,
     getting_lhs = false    -- eg/ie {<--true-->} = <--false-->
                            -- so no "vars" like "string" on the lhs,
                            -- in other words catch parse slip-ups &
                            -- a couple of coding constructs we are
                            -- not allowing, eg "{a, {atom b}} = x".

--precedence climbing:

function factor()
--  sequence tok = tokens[tdx]
    sequence tok = deep_copy(tokens[tdx])
    tdx += 1
    integer {toktype,start,finish,line} = tok
--DEV (debug)
string tt = tok_name(toktype),
       tv = src[start..finish]
--toktype = and_bits(toktype,#FF) -- no help...
    switch toktype do
        case DIGIT,
             '\'',
             '"',
             HEXSTR,
             '`',
             '#',
             'b',
             '?',
             '$':
                    break

        case '.':   if tdx>=length(tokens) 
                    or tokens[tdx][TOKTYPE]!=DIGIT
                    or tokens[tdx][2]!=start+1 then
                        return parse_error(tok,"factor expected")
                    end if
                    tok = deep_copy(tokens[tdx])
                    tok[2] -= 1
                    tdx += 1
                    break

        case COMMENT,
             BLK_CMT:
--?tok
                    return factor()
--                  break
        case LETTER:
--19/6/21 (just a thought... for eg "{a, {atom b}} = x")
                    integer ttidx = tok[TOKTTIDX]
                    if getting_lhs
                    and is_phix()
                    and (find(ttidx,vartypes) or
                         find(ttidx,udts)) then
                        return parse_error(tok,"unexpected type")
                    end if
--19/6 (spotted in passing, *2)
--                  if finish-start=2
--                  and src[start..finish]=`not` then
                    if ttidx=T_not then
                        tok = {T_not,{factor()}}
                    elsif not is_phix()
--                    and finish-start=2
--                    and src[start..finish]=`new` then
                      and ttidx=T_new then
--trace(1)
                        tok = {T_new,expr(0)}
                    else
                        integer idtype = get_id_type(ttidx)
                        if idtype=0 then
                            bool bEq = tokens[tdx][TOKTYPE]==BEQ
                            if calling_the_rtn=0 or not bEq
                            or get_named_param_idx(calling_the_rtn,ttidx)=0 then
if allow_nested_constants and bEq then
    found_nested_constants = true
    if add_local(ttidx, TYPO)!=1 then ?9/0 end if
--                              warn(tok,"fatal: already declared")
else
--if not allow_nested_constants or not bEq then
                                warn(tok,"warning: unrecognised")
end if
                            end if
                        else
                            tok[TOKALTYPE] = idtype
                        end if
                        if tdx<=length(tokens) then
--sequence tokdbg = tokens[tdx]
                            toktype = tokens[tdx][TOKTYPE]
                            if toktype='(' then
                                tok = rcall("PROC",tok)
--                              tok = {rcall("PROC",tok)}
--                          elsif not is_phix() and toktype='.' then
--                          elsif find(toktype,{COMMENT,
                            end if
--                      else
--                          tok[TOKALTYPE] = idtype
                        end if
                    end if
--                  break
        case '(':
                    tok = expr(0)
                    expectt(')')
--                  break
        case '-':
                    tok = {'-',{expr(PUNY)}}
--                  break
        case '+':
                    tok = {'+',{expr(PUNY)}}
--                  break
        case '*':
                    tok = {'*',{expr(PUNY)}}
--                  break
        case SPREAD:
                    if is_phix() then
                        return parse_error(tokens[tdx],"factor expected")
                    end if
                    tok = {SPREAD,{expr(PUNY)}}
--                  break
        case '~':
--                  if not is_phix() then
--                      return parse_error(tokens[tdx],"factor expected")
--                  end if
--                  tok = {TWIDDLE,factor()}
--                  tok = {TWIDDLE,expr(PUNY)}
--                  tok = {TWIDDLE,{expr(PUNY)}}
                    tok = {"PROC",{tok,expr(PUNY)}}
--                  break
        case '{':
--                  tok = {'{'}
                    tok = {}
                    while tokens[tdx][TOKTYPE]!='}' do
                        tok = append(tok,expr(0))
--                      toktype = tokens[tdx][TOKTYPE]
--DEV...
                        toktype = skip_comments()
                        if toktype!=',' then
-- DEV :=
                            if toktype!=':' then exit end if
--DEVviolation... (also, should this not be BEQ?)
                            tok[$] = {":=",tok[$],expr(0,1)}
--                          toktype = tokens[tdx][TOKTYPE]
                            toktype = skip_comments()
                            if toktype!=',' then exit end if
                        end if
                        tdx += 1
                        {} = skip_comments()
                    end while
                    if length(tok) and tok[$][TOKTYPE]='$' then
                        tok = tok[1..$-1]
                    end if
                    expectt('}')
                    tok = {'{',tok}
--                  tok = {'{',{tok}}
        case '[':
                    if not is_phix() then
                        -- js arrays, [...], much like phix sequences, {...}.
--                      tok = {'['}
                        tok = {}
                        if toktype=',' then tdx += 1 end if
                        while tokens[tdx][TOKTYPE]!=']' do
                            tok = append(tok,expr(0))
                            toktype = tokens[tdx][TOKTYPE]
                            if toktype!=',' then exit end if
                            tdx += 1
                        end while
                        expectt(']')
--                      tok = {'[',tok}
                        tok = {'{',tok}
                        break
                    end if
                    fallthrough
--      case '.':
--                  if not is_phix() then
--                      elsif toktype='.' and not is_phix() then
--                          tdx += 1
--                          ast = {'.',tok,statement()}
--?9/0
--                  end if
--                  fallthrough
        case '!':
                    if not is_phix() then
                        tok = {T_not,{factor()}}
                        break
                    end if
                    fallthrough
        default:
                    return parse_error(tokens[tdx-1],"factor expected")
--                  ?9/0
    end switch
--tok = tokens[tdx]
--  toktype = tokens[tdx][TOKTYPE]='('
--  toktype = 
--  if toktype='[' then
    integer wastdx = tdx
    while not parse_bad
      and tdx<=length(tokens) do
        toktype = tokens[tdx][TOKTYPE]
        if toktype='[' then
            tok = {tok}
            tdx += 1
            while tokens[tdx][TOKTYPE]!=']' do
                tok = append(tok,expr(0))
                if tokens[tdx][TOKTYPE]!=',' then
                    if tokens[tdx][TOKTYPE]=ELLIPSE then
                        tok[$] = {ELLIPSE,{tok[$],expr(0,1)}}
                    end if
                    exit
                end if
                tdx += 1
            end while
            expectt(']')
            tok = {'[',tok}
            wastdx = tdx
--          exit -- NO!!!
        elsif find(toktype,{COMMENT,BLK_CMT}) then
--DEV...
            tdx += 1
        else
            if toktype='.' then
                tdx += 1
--              tok = {toktype,tok,factor()}
--5/5/21
--              tok = {toktype,factor()}
                tok = {toktype,{tok,factor()}}
                wastdx = tdx
            end if
            exit
        end if
    end while
    tdx = wastdx
    return tok  -- (original, unless modified)
end function

function precedence(integer tdx)
-- (note there's a different/embedded version one in p2js_emit)
    integer p = 0
    if tdx!=0 then
        sequence tok = tokens[tdx]
--      integer {toktype,start,finish,line} = tok
        integer {toktype,start,finish} = tok
--nope...
--      integer {toktype,start,object finish} = tok
--if find(toktype,{T_and,T_or,T_xor}) then ?{"op_prec:",get_ttname(toktype)} end if
        if find(toktype,`(,){}];?:`) then return 0 end if
        string op = src[start..finish]
--      string op = iff(string(finish)?finish:src[start..finish])
        p = find(op,multisym)
        if p=0 then return parse_error(tok,"no precedence") end if
--      if p=0 then trace(1) return parse_error(tok,"no precedence") end if
        p = msprec[p]
    end if
    return p
end function

function next(bool bPeep=true)
--  integer p = 0
    integer ndx = 0, skipped  = 0
    for i=tdx to length(tokens) do
        sequence tok = tokens[i]
        integer {toktype} = tok
        if toktype>SYMBOL
        or (toktype=LETTER and find(tok[TOKTTIDX],{T_and,T_or,T_xor})) then
            if not bPeep then
                tdx = i
            end if
--          p = precedence(i)
--          p = i
            ndx = i
            exit
        elsif toktype!=COMMENT
          and toktype!=BLK_CMT then
--?{"p2jsparse.e line 385, toktype",toktype} -- (LETTER, lots)
            exit
        end if
        skipped += 1
    end for
--  return p
    return ndx
end function

function expr(integer p, skip=0)
    tdx += skip
    sequence res = factor()
--  while next()>p do
--      integer np = next(false)
    while not parse_bad
      and precedence(next())>p do           -- (if ... then)
        integer np = precedence(next(false)) -- (...update tdx)
        {integer op} = tokens[tdx]
        if op=LETTER then
            op = tokens[tdx][TOKTTIDX]
        end if
        sequence rhs = expr(np,1)
--      if rhs[1]="TERNARY" then
        if rhs[1]=T_iff then
--          rhs = rhs[2]
--          rhs[1] = {op,{res,rhs[1]}}
--          res = {T_iff,rhs}
--          res = {T_iff & rhs}
--          res = {T_iff & rhs}
            rhs[2][1] = {op,{res,rhs[2][1]}}
--          rhs[1] = T_iff
            res = rhs
        else
            res = {op,{res,rhs}}
        end if
    end while   
-- on a hunch, 20/3/21...
--  if p=0 and not is_phix() then
    if not is_phix() then
        integer ndx = next()
        if ndx!=0 and tokens[ndx][TOKTYPE]='?' then
            tdx = ndx
            sequence cond = res
            expectt('?') 
            sequence truthy = expr(0)
--5/5/21:
            integer toktype = skip_comments()
            expectt(':') 
            sequence falsy = expr(0)
--          return {`TERNARY`,{cond,truthy,falsy}}
--trace(1)
--          res = {`TERNARY`,{cond,truthy,falsy}}
            res = {T_iff,{cond,truthy,falsy}}
--trace(1)
--?9/0
--function exprt(integer t) expectt(t) return expr(0) end function
--          res = {`TERNARY`,{res,exprt('?'),exprt(':')}}
        end if
    end if
    return res
end function
--*!/

--/* SUG:
    while true do
        integer ndx = next()
        if ndx=0 then exit end if
        integer np = precedence(ndx)
        if np<=p then exit end if
        tdx = ndx
--or:
    while higher_than(p) do
integer np
fn higher_than(integer p)
    integer ndx = next()
    if ndx=0 then return false end if
    np = precedence(ndx)
    if np<=p then return false end if
    tdx = ndx
    return true
end function

--or:
comments can only occur between statements and attached to operators.... [erm, would only help if tokeniser could do that, but it canny]
tokeniser: tokens ==> tokens,comments (which the parser can then pick off, flag, and/or report) -- I think so...
sequence comflags = repeat(1,length(comments)) ... comflags[cdx] = 0 ... integer ucom = sum(comflags); printf(1,"Unhandled comments:%d\n",ucom)
--*/

function get_multi_set(integer vtype)
    sequence ast = {}
    sequence tok = tokens[tdx]
    integer toktype = tok[TOKTYPE]
    tdx += 1
    integer ot = toktype, et = iff(is_phix()?'}':']')
    if not is_phix() and tokens[tdx][TOKTYPE]=',' then tdx += 1 end if
    while tokens[tdx][TOKTYPE]!=et do
        if tokens[tdx][TOKTYPE]='?' then
            ast = append(ast,tokens[tdx])
            tdx += 1
        else
            while true do
                tok = tokens[tdx]
                toktype = tok[TOKTYPE]
                if toktype!=COMMENT and toktype!=BLK_CMT then exit end if
                ast = append(ast,tok)
                tdx += 1
            end while
            if toktype=et then exit end if
            if toktype!=',' then
                if toktype=LETTER then
                    integer ttidx = tok[TOKTTIDX],
                            alres = add_local(ttidx, vtype)
                    if alres!=1 then
?{"p2js_parse.e line 649, add_local(",ttidx,"=",get_ttname(ttidx),")=",alres}
--                       ?9/0 
                    end if
                elsif toktype=ot then
                    tok = {'{',get_multi_set(vtype)}
                else
                    return parse_error(tok,"unrecognised")
                end if
                ast = append(ast,tok)
                tdx += 1
            end if
        end if
        if tokens[tdx][TOKTYPE]!=',' then exit end if
        tdx += 1
    end while
    if tokens[tdx][TOKTYPE]!=et then
        expectt(et)
    end if
    return ast
end function

function extract_nested_constants(sequence res, aod)
    --
    -- convert eg {A:=1, "string"} to res &= {A,1}, {A,"string"}
    -- ie/eg
    --  aod = {'{',{{129,{{4,53,53,3,14,17584},{3,56,56,3,17}}},{'"',59,61,3,20}}}
    -- to
    --  res &= {{4,53,53,3,14,17584},{3,56,56,3,17}}
    --  aod = {'{',{{4,53,53,3,14,17584},{'"',59,61,3,20}}}
    --
    -- likewise (using recursion) {{A:=1,"string"}} to res &= {A,1}, {{A,"string"}}
    -- ie/eg
    --  aod = {'{',{{'{',{{129,{{4,79,79,4,15,17588},{3,82,82,4,18}}},{'"',85,87,4,21}}}}}
    -- to
    --  res &= {{4,79,79,4,15,17588},{3,82,82,4,18}}
    --  aod = {'{',{{'{',{{4,79,79,4,15,17588},{'"',85,87,4,21}}}}}
    -- (returning all the bits needed for the vardef node)
    --
    -- Be warned this has been through a fair few iterations to get this far, and
    -- I consider it probably still incomplete, the ?9/0 are just placeholders[?]
    --
    if length(aod)=2 and aod[TOKTYPE]='{' then
        integer la2 = length(aod[2])
        res = deep_copy(res)
        aod = deep_copy(aod)
        for i=1 to la2 do
            sequence a2i = aod[2][i]
            aod[2][i] = 0
            object toktype = a2i[TOKTYPE]
            if toktype=BEQ then
                if length(a2i[2])!=2 then ?9/0 end if
                if a2i[2][1][TOKTYPE]!=LETTER then ?9/0 end if
                {res,a2i[2][2]} = extract_nested_constants(res,a2i[2][2])
                res &= a2i[2] -- (id & cleaned(expr))
                a2i = a2i[2][1] -- (just keep id)
            elsif toktype='{' then
                {res,a2i} = extract_nested_constants(res,a2i)
            elsif toktype=`PROC` then
                {res,a2i[2][2]} = extract_nested_constants(res,a2i[2][2])
--          elsif not find(toktype,{DIGIT,LETTER}) then -- (I gave up on '+')
--              ?9/0
            end if
            aod[2][i] = a2i
        end for
    end if
    return {res,aod}
end function

function vardef(integer thistdx, skip=0, iForPar=0)
--
-- iForPar of 0 is normal var definition
-- iForPar of 2 is rtndef() parameters [see note[s] therein]
-- iForPar of 4 is a for loop (pun intended)
--
    tdx += skip
    sequence tok = tokens[thistdx],
--  sequence tok = deep_copy(tokens[thistdx]),
--           res = {tok}, ast
             res = {deep_copy(tok)}, ast
    integer ttidx = tok[TOKTTIDX],
            vtype = iff(iForPar=4?TYPI:get_global_type(ttidx))
--  if vtype=0 and iForPar!=4 then ?9/0 end if
    res[1][TOKALTYPE] = vtype
    if vtype=TYPK and ttidx=T_constant then
        vtype=TYPO
        if iForPar=0 then
            allow_nested_constants = true
            found_nested_constants = false
        end if
    elsif vtype<TYPI or vtype>TYPO then
        if ext=PHIX then
            warn(tok,"vtype=0b%04b, TYPO assumed",{vtype})
        end if
        vtype = TYPO
    end if
    integer last_comma
    bool bEq = false
    while true do
        tok = tokens[tdx]
        sequence ntok = "",
                 aod = {}
        integer {toktype,start,finish} = tok
        string name = ""
--      if iForPar=2 and (toktype=COMMENT or toktype=BLK_CMT) then
        while toktype=COMMENT or toktype=BLK_CMT do
            if tdx!=thistdx then
                res = append(res,tok)
--              res &= tdx
                --DEV better: don't use as a name if > 30 characters?
                name = shorten(src[start..finish],"",10)
            end if
            tdx += 1
            tok = tokens[tdx]
--          ntok = tok
            {toktype,start,finish} = tok
        end while
        if toktype=LETTER then
--bit premature...
--          if add_local(tok[TOKTTIDX], vtype)!=1 then ?9/0 end if
--          ntok = tok
            ntok = deep_copy(tok)
            ntok[TOKALTYPE] = vtype
        end if
        if iForPar!=2 or (toktype!=',' and toktype!=')') then
            if iForPar=0
            and ((is_phix() and toktype='{') or
                 (not is_phix() and toktype='[')) then
                ast = {'{',get_multi_set(vtype)}
                if found_nested_constants then
                    ?{"fnc",ast}
                    found_nested_constants = false
?9/0 -- placeholder???
                end if
                bEq = true
            elsif toktype!=LETTER then
                return parse_error(tok,"variable name expected")
            elsif iForPar=2 and length(res) and not is_js() then
                --
                -- in eg (integer a,b,c, sequence d,e,f), quit
                -- on finding sequence: rtndef() will loop on.
                --
                ttidx = tok[TOKTTIDX]
                if find(ttidx,vartypes)
                or find(ttidx,udts) then
                    tdx = last_comma
                    exit
                end if
                if find(ttidx,T_reserved) then
--              if find(ttidx,T_reserved) and ttidx!=T_args then
                    return parse_error(tok,"illegal use of reserved word")
                end if
            end if
            if not bEq then
                if vtype<TYPI or vtype>TYPO then
                    warn(tok,"internal error, vtype is 0b%4b, TYPO assumed",{vtype})
                    vtype = TYPO
                end if
                integer r = add_local(tok[TOKTTIDX], vtype)
                if r!=1 then
--?{"r!=1 line 906 p2js_parse.e",tok}
                    return parse_error(tok,iff(r=-1?"illegal":"already defined"))
                 end if
            end if
--DEV??
            name = src[start..finish]
            tdx += 1
            if tdx>length(tokens) then exit end if
            tok = tokens[tdx]
            {toktype,start,finish} = tok
--          if toktype=SYMBOL and src[start]='=' then
            if toktype='='
            or toktype=BEQ then
--              ?9/0
--              tdx += 1
--              aod = expression(0)
                aod = expr(0,1)
--?{"aod",aod}
            elsif iForPar=4 or bEq then
                expectt('=')
            end if
--13/5/21: [account for eg function multitext_valuechanged_cb(Ihandle /*multitext*/),
--          ie perform arg_idx += 1 so that it will match the later def_idx += 1's.]
--      elsif iForPar=2 and find(toktype,",)") then
        else
            {} = add_local(0, vtype) -- (keep arg_idx in step with future def_idx)
        end if
        if bEq then
--          res &= {{ast,aod}}
            res = append(res,{T_block,{ast,aod}})
--SUG:
--          res = append(res,{"{=}",{ast,aod}})
            bEq = false
        elsif found_nested_constants then
            {res,aod} = extract_nested_constants(res,aod)
            res &= {ntok,aod}
            found_nested_constants = false
        else
--8/3/21:
--          res &= {name,aod}
            res &= {ntok,aod}
        end if
--      tdx += 1
--      if toktype!=SYMBOL or src[start]!=',' then
--      if iForPar=4 or toktype!=',' then
--      if iForPar=4 or tokens[tdx][TOKTYPE]!=',' then
        if iForPar=4 or tdx>length(tokens) or tokens[tdx][TOKTYPE]!=',' then
--?tokens[tdx]
--?tokens[tdx][TOKTYPE]
--          tdx -= 1
            exit
        end if
        last_comma = tdx
        tdx += 1
        while find(tokens[tdx][TOKTYPE],{COMMENT,BLK_CMT}) do
            res = append(res,tokens[tdx])
            tdx += 1
        end while
        if tokens[tdx][TOKTYPE]='$' then
            tdx += 1
            exit
        end if
    end while
    allow_nested_constants = false
--?tdx
--?tokens[tdx]
--  if tokens[tdx][TOKTYPE]=',' then ?9/0 end if
--if tdx=122 then trace(1) end if
-- actually, we might get away with 1/2... they need to be global though....
--puts(1,"Warning: VARIABLE line 632 p2js_parse.e\n") -- (wrong, it needs to be like T_for, or something...)
--  return {VARIABLE,res}
--  return {"vardef",thistdx,res}   -- nb covers constants!
    res = {"vardef",res}    -- nb covers constants!
--?res
    return res              -- nb covers constants!
end function

forward function block(integer skip=0, bool bOpt=false)

integer in_rtn_def = 0  -- or TYPF or TYPR or TYPE

bool bForward = false
sequence forwards = {}

--function rtndef(string rtype)
function rtndef(integer ttidx)
    integer was_in_rtn_def = in_rtn_def,
            rdx = find(ttidx,{T_function,T_procedure,T_type})
    sequence tok = tokens[tdx],
             args = {},
             body = {},
--           res = {ttidx,{tok,args,body}}
             res = {ttidx,deep_copy({tok,args,body})}
    in_rtn_def = {TYPF,TYPR,TYPE}[rdx]
    integer {toktype,start,finish} = tok
    if toktype!=LETTER then
        return parse_error(tok,"name expected")
    end if
    integer rtnttidx = tok[TOKTTIDX]
--erm, or later??
    if ttidx=T_type then
--      udts &= tok[TOKTTIDX]
        udts &= rtnttidx
    end if
--string name = src[start..finish] --DEV/debug
    tdx += 1
    expectt('(')
--trace(1)
    if in_rtn_def=TYPE then
        --
        -- effectively type abc(int x) ==> alias(abc,int), then a
        --       later type def(abc x) ==> alias(def,int) [not abc!]
        --
        if tokens[tdx][TOKTYPE]!=LETTER then
            ?9/0
        end if
        in_rtn_def = get_global_type(tokens[tdx][TOKTTIDX])
        if in_rtn_def=0 then
            ?9/0
        end if
--      res[2][1][TOKALTYPE] = ttype
----        if add_global(ttidx,ttype)!=1 then
--      rag = add_global(rtnttidx,ttype)
--      if rag!=1 then
----            ?9/0
--          return parse_error(tok,"add_global!=1")
--      end if
    end if
--  else
        res[2][1][TOKALTYPE] = in_rtn_def
        integer rag = add_global(rtnttidx,in_rtn_def)
        if rag!=1 then
--erm, not if doing autoincludes...
--          ?9/0
            integer fwdx = find(rtnttidx,forwards)
            if not bForward and fwdx!=0 then
                forwards[fwdx..fwdx] = {}
--          else
            elsif not find(rtnttidx,{T_dictionary}) then
                return parse_error(tok,"add_global!=1")
            end if
        elsif bForward then
            forwards &= rtnttidx
        end if
--  end if
    add_scope(rtnttidx)
    if tokens[tdx][TOKTYPE]!=')' then
        while true do
--          args &= vardef(tdx,1,2)
--          args &= vardef(tdx,is_phix(),2)
            {} = skip_comments()
            args = append(args,vardef(tdx,is_phix(),2))
            set_arg_default(args[$])
            if tokens[tdx][TOKTYPE]!=',' then exit end if
            tdx += 1
--          while find(tokens[tdx][TOKTYPE],{COMMENT,BLK_CMT}) do
--              args = append(args,tokens[tdx])
--              tdx += 1
--          end while
        end while
    end if
--  res[2][1][2] = args
--  res[2][2] = args
--  res[2][2] = {T_args,args}
    res[2][2] = {ARGS,args}
--trace(1)
    expectt(')')
    clear_arg_rtn()
--  if is_js() then
--      expectt('{')
--  end if
    if bForward then
        bForward = false
    else
        body = {T_block,block()}
        if is_phix() then
            if not expects({T_end,ttidx}) then return false end if
--      else
--          expectt('}')
        end if
    end if
    res[2][3] = body
    in_rtn_def = was_in_rtn_def
    drop_scope()
    return res
end function

bool in_switch = false

function statement()
--integer l0 = -1
    sequence ast = {}, aste
--  string s    -- (scratch)
    while tdx<=length(tokens) do
--      sequence tok = tokens[tdx]
        sequence tok = deep_copy(tokens[tdx])
        integer {toktype,start,finish,line} = tok,
--DEV?? (maybe we /will/ have some kind of TOKDX entry, specific for each toktype??)
                thistdx = tdx
--if l0=-1 then l0 = line end if
--if line=601 then trace(1) end if
        tdx += 1
        switch toktype do
            case SPACE:
--?"SPACE!!!!"
?9/0
                break -- (aka loop/continue)

            case COMMENT, BLK_CMT:
                ast = append(ast,tok)

            case LETTER:
                integer ttidx = tok[TOKTTIDX]
                switch ttidx do
--              switch ttidx without jump_table do
                    -- note: there shouldn't be any "hits" > T_xor here
                    --  (otherwise compiler moans jump table too big/sparse)
                    case T_include,
                         T_from,
                         T_import,
                         T_with,
                         T_without:
--?"include!"
--DEV.. (local)
--                      phix_only()
                        if not is_phix()
                        and not is_C() then -- (#include really)
--                      and not is_py() then -- (from)
--                          return parse_error(tok,"phix/c/py only")
                            return parse_error(tok,"phix/c only")
                        end if
                        aste = tok
                        string filename = ""
                        integer toktype2, line2
                        while tdx<=length(tokens) do
                            tok = tokens[tdx]
                            {toktype2,start,finish,line2} = tok
                            if line2!=line
                            or not find(toktype2,INCLUDETOKS) then
                                exit
                            elsif toktype2=LETTER
                              and tok[TOKTTIDX]=T_as then
                                return parse_error(tok,"not supported")
                            end if
                            filename &= src[start..finish]
                            tdx += 1
                        end while

                        filename = strip_builtin(filename)
--DEV (erm, scope handling means we have to process but not output...)
--    (far easier, methinks, for phix->phix to just expand includes)
--                      if ttidx=T_include then
                        if ttidx=T_include
                        and filename!="pGUI.e"
                        and filename!="mpfr.e"
                        and filename!="sha256.e"
                        and filename!="timedate.e" then
                            -- sanity checks:
                            if parse_bad!=0 then ?9/0 end if
                            if in_rtn_def!=0 then ?9/0 end if
                            if bForward!=0 then ?9/0 end if
                            if in_switch!=0 then ?9/0 end if
--                          if length(clines) then ?9/0 end if -- DEV temp/clear now... (no, save/restore)
--                          if ttidx=T_include and o_ext!=PHIX then
                            sequence incres = tokstack_push(filename,line)
                            if incres="NOT FOUND"
                            or tok_error() then
--                              return parse_error(0,"include error")
--trace(1)
--                              {} = parse_error(tok,"include error")
                                {} = parse_error(aste,"include error")
                                return ast
                            elsif incres!="ALREADY DONE" then
                                ast = append(ast,incres)
                            end if
                        elsif ttidx=T_without
                          and find(filename,{"js","javascript","javascript_semantics"}) then
                            {} = parse_error(aste,"explicitly tagged as non-transpilable")
                            return ast
                        else
--?{toktype,filename}
-- spotted in passing...: [ DEV these are ALL just going to be LETTER!! ]
--                          ast = append(ast,{toktype,filename})
--                          ast = append(ast,{ttidx,filename})
                            ast = append(ast,{ttidx,{filename,line2}})
--                          ast = append(ast,{aste,filename})
                        end if
                    case T_forward:
                        bForward = true
--DEV???
--                      fallthrough
--                      ast = append(ast,{toktype})
                        ast = append(ast,{T_forward,{tok}})
                        break -- (aka loop/continue)

                    case T_global:
--DEV erm, how's it handling the ',' then??? (plus this is not the ast you're looking for)
--                       T_constant,
--                       T_static:
--                      ast = append(ast,{toktype})
--                      ast = append(ast,{T_global,line})
                        ast = append(ast,{T_global,{tok}})
--                      ast = append(ast,{tok})
                        break -- (aka loop/continue)

                    case T_enum:
--                      aste = {T_enum,{COMMENT|BLK_CMT|tok|{'=',{tok,'$'|expr}}}}
                        aste = {}
                        while true do
                            while find(tokens[tdx][TOKTYPE],{COMMENT,BLK_CMT}) do
                                aste = append(aste,tokens[tdx])
                                tdx += 1
                            end while
                            tok = tokens[tdx]
                            toktype = tok[TOKTYPE]
                            if toktype!=LETTER then
                                 return parse_error(tok,"name expected")
                            end if
                            sequence onem = deep_copy(tok)
                            integer rag = add_global(onem[TOKTTIDX],TYPI)
                            if rag!=1 then
                                return parse_error(tok,"add_global!=1")
                            end if
                            onem[TOKALTYPE] = TYPI
                            tdx += 1
                            if tdx>length(tokens) then exit end if
                            if tokens[tdx][TOKTYPE]='=' then
                                tdx += 1
                                if tokens[tdx][TOKTYPE]='$' then
                                    onem = {'=',{onem,tokens[tdx]}}
                                    tdx += 1
                                else
                                    onem = {'=',{onem,expr(0)}}
                                end if
                            end if
                            aste = append(aste,onem)
                            for i=tdx to length(tokens)+1 do
                                if i>length(tokens) then break end if
                                toktype = tokens[i][TOKTYPE]
                                if toktype=',' then
                                    if i>tdx then
                                        -- these comments belong to the enum...
                                        --  (but if we break, they don't)
                                        aste &= tokens[tdx..i-1]
                                    end if
                                    tdx = i
                                    exit
                                end if
                                if toktype!=COMMENT and toktype!=BLK_CMT then exit end if
                            end for
                            if tokens[tdx][TOKTYPE]!=',' then exit end if
                            tdx += 1
                        end while
                        ast = append(ast,{T_enum,aste})

                    case T_function,T_procedure,T_type:
                        ast = append(ast,rtndef(ttidx))

                    case T_if:
                        aste = {expr(0)}
                        if is_phix() then
                            if not expect(T_then) then exit end if
--                      else
--                          expectt('{')
                        end if
                        add_scope()
--                      sequence b = block()
                        aste = append(aste,{T_then,block()})
                        drop_scope()
                        while tdx<=length(tokens) do
                            tok = tokens[tdx]
                            if tok[TOKTYPE]!=LETTER then exit end if
                            ttidx = tok[TOKTTIDX]
                            if ttidx!=T_elsif then
                                if is_phix()
                                or tokens[tdx+1][TOKTYPE]!=LETTER
                                or tokens[tdx+1][TOKTTIDX]!=T_if then
                                    exit
                                end if
                                tdx += 1
                            end if
                            -- aside: on non-phix, this quietly consumes (),
                            --                    rather than enforcing them
                            aste = append(aste,{T_elsif,{expr(0,1)}})
                            if is_phix() then
                                if not expect(T_then) then ?9/0 end if
                            end if
                            add_scope()
                            aste = append(aste,{T_then,block()})
                            drop_scope()
                        end while
--                      if not is_phix() then
--                          expectt('}')
--                      end if
--                      if ttidx = T_else then
                        if tdx<=length(tokens) then
                            tok = tokens[tdx]
                            if tok[TOKTYPE]=LETTER 
                            and tok[TOKTTIDX]=T_else then
                                add_scope()
                                aste = append(aste,{T_else,block(1)})
                                drop_scope()
                            end if
                        end if
                        if parse_bad then exit end if
                        if is_phix() then
                            if not expects({T_end,T_if}) then exit end if
                        end if
                        ast = append(ast,{T_if,aste})

                    case T_iff, T_iif:
                        expectt('(')
                        aste = {T_iff,expr(0)}
                        expectt('?')
                        aste = append(aste,expr(0))
                        expectt(':')
                        aste = append(aste,expr(0))
                        expectt(')')
                        ast = append(ast,aste)

                    case T_for:
--DEV very different for js...
--                      bool bLet = false
                        bool bNoVar = false,
                             bPreDef = false
                        if not is_phix() then
                            expectt('(')
                            if tokens[tdx][TOKTYPE]=';' then
                                -- "for (; "-style (no phix output possible)
--DEV/SUG:                      violation(tokens[tdx][TOKLINE],"for (;")
                                bNoVar = true
                            elsif find(tokens[tdx][TOKTTIDX],{T_let,T_var}) then
--                              bLet = true
                                tdx += 1
                            end if
                        end if
tok = tokens[tdx]
--if tokens[tdx][TOKLINE]<3 then trace(1) end if
if not bNoVar then
    integer ctt = get_local_type(tok[TOKTTIDX])
--  if ctt!=0 then return parse_error(tok,"already defined") end if
    if ctt!=0 then bPreDef = true end if
end if

                        add_scope()
--19/5/21 (spotted in passing, we may have just done a tdx +=1 above...)
                        sequence ctrl = iff(bNoVar?{}:vardef(thistdx,0,4))
--                      sequence ctrl = iff(bNoVar?{}:vardef(tdx,0,4))
                        if is_phix() then
                            if not expect(T_to) then exit end if
                        else
                            expectt(';')
--                      elsif bLet then
--                          ctrl = {T_let,ctrl}
                        end if
                        sequence lim = expr(0), step = {}
--                               step = iff(ttidx=T_by?expr(0,1):{})
                        if is_phix() then
                            if tokens[tdx][TOKTTIDX]=T_by then
                                step = expr(0,1)
                            end if
                            if not expect(T_do) then exit end if
                        else
                            expectt(';')
                            step = expr(0)
                            expectt(')')
                        end if
                        sequence body = {T_block,block()}
                        if is_phix() then
                            if not expects({T_end,T_for}) then exit end if
                        end if
                        ast = append(ast,{T_for,{ctrl,bPreDef,lim,step,body}})
                        drop_scope()

                    case T_while:
                        aste = {expr(0)}
                        if is_phix() then
                            if not expect(T_do) then exit end if
--                      else
--                          expectt('{')
                        end if
                        add_scope()
                        aste = append(aste,{T_block,block()})
                        if is_phix() then
                            if not expects({T_end,T_while}) then exit end if
--                      else
--                          expectt('}')
                        end if
                        ast = append(ast,{T_while,aste})
                        drop_scope()

                    case T_switch:
                        aste = {expr(0)}
                        integer was_in_switch = in_switch
                        in_switch = true
                        if not is_phix() then
                            expectt('{')
                        elsif tokens[tdx][TOKTYPE]=LETTER
                          and tokens[tdx][TOKTTIDX]=T_do then
                            tdx += 1
                        end if
                        while true do
                            sequence cases = {}
                            while tokens[tdx][TOKTYPE]=COMMENT do
                                tdx += 1
                            end while
                            tok = tokens[tdx]
                            if tok[TOKTYPE]=='}' then exit end if
                            ttidx = tok[TOKTTIDX]
                            if ttidx=T_case then
                                while true do
                                    cases = append(cases,expr(0,1))
                                    tok = tokens[tdx]
                                    toktype = tok[TOKTYPE]
                                    if toktype!=',' then exit end if
                                end while
--/*
--DEV/DOC Niggles: the desktop switch (perhaps wrongly) permits "case else", but p2js does not - use just "else" or "default" instead.
--                              toktype = tokens[tdx][TOKTYPE]
                                if toktype!=':'
--                              and (toktype!=LETTER or tokens[tdx][TOKTTIDX]!=T_then) then
                                and (toktype!=LETTER or tok[TOKTTIDX]!=T_then) then
--                              and (toktype!=LETTER or not find(tok[TOKTTIDX],{T_then,T_else})) then
                                    expectt(':')
                                    exit
                                end if
                                ast = append(ast,{T_case,cases,block(1,true)})
--*/
                            elsif ttidx=T_default
                               or ttidx=T_else then
--trace(1)
                                tdx += 1
                                tok = tokens[tdx]
                                toktype = tok[TOKTYPE]
                            else
                                exit
                            end if
                            if toktype=':'
                            or (toktype=LETTER and tokens[tdx][TOKTTIDX]=T_then) then
                                tdx += 1
                            end if
                            aste = append(aste,{ttidx,cases})
                            aste = append(aste,{T_block,block(0,true)})
                        end while
                        if is_phix() then
                            if not expects({T_end,T_switch}) then exit end if
                        else
                            expectt('}')
                        end if
                        in_switch = was_in_switch
                        ast = append(ast,{T_switch,aste})

                    case T_fallthru:
                        ttidx = T_fallthrough
                        fallthrough
                    case T_break,
                         T_fallthrough:
                        --
                        -- aside: methinks this will happily parse eg
                        --
                        --  switch
                        --    case
                        --      if cond
                        --         fallthrough  -- (break wd be ok here)
                        --      end if
                        --      other_stuff()
                        --
                        --  which p.exe will (quite rightly) baulk at...
                        --
                        if is_phix() and not in_switch then
                            return parse_error(tok,"illegal")
                        end if
                        ast = append(ast,{ttidx,line})
                        
                    case T_return:
                        if not in_rtn_def then
                            return parse_error(tok,"illegal")
                        elsif in_rtn_def!=TYPR
                          and tokens[tdx][TOKTYPE]!=';' then
                            ast = append(ast,{T_return,{expr(0)}})
                        else
                            ast = append(ast,{T_return,line})
                        end if

                    case T_end,
                         T_elsif,
                         T_else,
                         T_case,
                         T_default:
-- is there a reason/proper need for this?? [might be {{comments},T_end}]
--          [triggered by {res,pos} = getint(sf,8,pos) \n elsif(etc)]
--?{"p2js_parse.e, line 905: backtrack T_end/elsif/else/case?, line:",line}
                        tdx -= 1
                        exit

                    case T_exit:
                        ast = append(ast,{T_exit,line})

                    case T_continue:
                        ast = append(ast,{T_continue,line})
                    
                    case T_try:
                        if not is_phix() then
                            return parse_error(tok,"illegal")
                        end if
                        aste = {T_block,block(0,true)}
                        if not expect(T_catch) then exit end if
                        tok = deep_copy(tokens[tdx])
                        if tokens[tdx][TOKTYPE]!=LETTER then
                            return parse_error(tok,"illegal")
                        end if
                        add_scope()
                        integer r = add_local(tok[TOKTTIDX], TYPQ)
                        if r!=1 then
--?{"r!=1 line 1553 p2js_parse.e",tok}
                            return parse_error(tok,iff(r=-1?"illegal":"already defined"))
                         end if
--                      integer ctt = get_local_type(tok[TOKTTIDX])
--                      if ctt!=0 then return parse_error(tok,"already defined") end if
--                      aste = append(aste,{T_catch,vardef(tdx,0,4)})
                        tok[TOKALTYPE] = TYPQ
                        aste = append(aste,{T_catch,tok})
                        tdx += 1
                        aste = append(aste,{T_block,block(0,true)})
                        if not expects({T_end,T_try}) then exit end if
                        ast = append(ast,{T_try,aste})
                        drop_scope()

                    default:
--                      integer idtype = get_id_type(tok[TOKTTIDX])
                        integer idtype = get_id_type(ttidx)
                        if idtype=0 then
--                          trace(1)
--                          return {parse_error(tok,"unrecognised")}
--                      end if                          
                            warn(tok,"unrecognised")
                        else
--?{"idtype",sprintf("0b%04b",idtype),tok_string(tok),tok}
                            tok[TOKALTYPE] = idtype
                        end if
--DEV via p2js_scope...
--                      integer ttype = get_id_type(ttidx)
--?{"idtype",idtype,"ttype",ttype} -- they are the same.
--printf(1,"%s ttype = 
                        bool bVar = iff(is_js()?find(ttidx,jstypes)!=0
                                               :find(ttidx,vartypes) or
                                                find(ttidx,udts))
                        if bVar then
--if ttidx=T_let then trace(1) end if
                            -- kludge: treat eg "constant string x" as "constant x"
                            if is_phix() and ttidx=T_constant 
                            and tokens[tdx][TOKTYPE]=LETTER
                            and (find(tokens[tdx][TOKTTIDX],vartypes) or 
                                 find(tokens[tdx][TOKTTIDX],udts)) then
                                tdx += 1
--                              tok[TOKALTYPE] = TYPO
                            end if
                            ast = append(ast,vardef(thistdx))
--?ast
--                          ast = append(ast,vardef(tdx,1))
                            break
                        end if
--DEV verify ttidx here???

--skip_spaces()?
                        toktype = tokens[tdx][TOKTYPE]
--                      if tokens[tdx][TOKTYPE]='(' then
                        if toktype='(' then
                            ast = append(ast,rcall("PROC",tok))
--                          ast = {rcall("PROC",tok)}
--DEV (got a "+=" masquerading as a '+' here, length 2 - not an actual '+') ditto '-','*','&',':','/'
--                      elsif toktype='=' then
                        elsif toktype='='
--                         or toktype='+'
                           or toktype=PLUSEQ
--                         or toktype='-'
                           or toktype=MNUSEQ
--                         or toktype='*'
                           or toktype=MULTEQ
--                         or toktype='&'
                           or toktype=AMPSEQ
--                         or toktype=':'
                           or toktype=BEQ
--                         or toktype='/' then
                           or toktype=DIVDEQ then
--                          ast = append(ast,{"ASSIGN",tok,expr(0,1)})
--                          ast = {"ASSIGN",tok,expr(0,1)}
--                          ast = {"ASSIGN",{tok,expr(0,1)}}
--                          ast = {{"ASSIGN",{tok,expr(0,1)}}}
--                          ast = {{toktype,{tok,expr(0,1)}}}
                            ast = append(ast,{toktype,{tok,expr(0,1)}})
--                          ast = {"ASSIGN",expr(0,1)}
                        elsif toktype='[' then
                            integer wastdx = tdx
                            tdx -= 1
                            sequence f = factor()
--  f = {91'[',{{4,1537,1537,48'0',12,25956},{4,1539,1541,48'0',1,25968}}}
--      {91'[',{{4,13920,13920,424,15,27304},{199,{{4,13922,13924,424,1,27376},{4,13927,13929,424,1,27376}}}}}
                            bool bOK = true
                            if length(f[2])!=2 then
                                bOK = false
                            else
                                toktype = f[2][2][TOKTYPE]
                                if not find(toktype,{DIGIT,LETTER,'$'})
                                and (toktype!=ELLIPSE or 
                                     not find(f[2][2][2][1][TOKTYPE],{DIGIT,LETTER}) or
                                     not find(f[2][2][2][2][TOKTYPE],{DIGIT,LETTER})) then
                                    bOK = false
                                end if
                            end if
                            aste = {f}
                            toktype = tokens[tdx][TOKTYPE]
                            if not find(toktype,{'=',PLUSEQ,MNUSEQ,MULTEQ,DIVDEQ,AMPSEQ,BEQ}) then
                                return parse_error(tok,"assignment operator expected")
                            elsif toktype!='=' and toktype!=BEQ and not bOK then
?f
                                return parse_error(tokens[wastdx],"sorry, p2js cannot name the required temp for that")
                            end if
                            aste = append(aste,{toktype,{expr(0,1)}})
                            ast = append(ast,{"SASS",aste})
                        elsif toktype='.' and not is_phix() then
                            tdx += 1
                            ast = append(ast,{'.',{tok,statement()[1]}})
                        else
--                          trace(1)
                            return parse_error(tok,"unrecognised")
                        end if
                end switch
                exit

--          case SYMBOL:
--?9/0
--              -- I assume we don't really need ; in the ast...
----                string sym = src[start..finish]
----                if sym = ";" then break end if  -- skip/loop
----DEV SYMBOL -> '`' anyway...
----                if sym="?" then
----                    ast = append(ast,{'?',expr(0)})
----                    exit
----                end if
----trace(1)
--              return parse_error(tok,"unexpected symbol")

            case '?':
                if is_phix() then
--                  ast = append(ast,{'?',expr(0)})
                    ast = append(ast,{"?",{expr(0)}})
--?ast
--{} = wait_key()
                    exit
                end if
                return parse_error(tok,"unexpected token")

            case '{','[':
--              aste = {'{'}
                aste = {}
--trace(1)
--              while tokens[tdx][TOKTYPE]!='}' do
                integer et = iff(is_phix()?'}':']')
                getting_lhs = true
                while tokens[tdx][TOKTYPE]!=et do
                    if is_phix()
                    and tokens[tdx][TOKTYPE]=LETTER
                    and (find(tokens[tdx][TOKTTIDX],vartypes) or
                         find(tokens[tdx][TOKTTIDX],udts)) then
--trace(1)
--?{111,tdx}
                        if length(aste) and aste[1][1]!="vardef" then
--                          return parse_error(tok,"nested vardef error")
                            return parse_error(tokens[tdx],"nested vardef error")
                        end if
                        aste = append(aste,vardef(tdx,1,2))
--?aste
--?{222,tdx,tokens[tdx]}
                    else    
--?tokens[tdx]
                        sequence e0 = expr(0)
--nope, I'm bouncing this back to p2js_emit:
--                      if not is_phix()
--                      and find(e0[2][2][TOKTYPE],"$-") then
--                          return parse_error(e0[2][2],"unsupported by JavaScript Array destructuring")
--                      end if
                        aste = append(aste,e0)
--DEV/SUG:              violation(tokens[tdx][TOKLINE],"???")
                        if parse_bad then exit end if
                    end if
                    if tokens[tdx][TOKTYPE]!=',' then exit end if
                    tdx += 1
                end while
--              if parse_bad then trace(1) exit end if
                if parse_bad then exit end if
--              expectt('}')
                expectt(et)
                getting_lhs = false

                if not find(tokens[tdx][TOKTYPE],{'=','+','-',BEQ}) then
--sequence tokt = tokens[tdx]
--                  return parse_error(tok,"assignment operator expected")
                    return parse_error(tokens[tdx],"assignment operator expected")
                end if
--              aste = {toktype,aste,expr(0,1)}
--              aste = {toktype,{aste,expr(0,1)}}
--              aste = {"MASS",{T_block,{aste,expr(0,1)}}}
--              aste = {"MASS",{T_block,aste},{T_block,expr(0,1)}}
--              aste = {"MASS",{T_block,aste},expr(0,1)}
--              ast = append(ast,{"MASS",{T_block,aste},expr(0,1)})
--              ast = append(ast,{"MASS",{{T_block,aste},expr(0,1)}})
--              ast = append(ast,{"MASS",{{'{',aste},expr(0,1)}})
                ast = append(ast,{"MASS",{{MASS,aste},expr(0,1)}})
--expectt(';',true)
                exit
            case '"':
                if is_js() then
                    string q = src[start..finish]
                    if q=`"use strict"` then
--                      ast = append(ast,{STRICT,thistdx})
                        ast = append(ast,{"use strict",thistdx})
                        exit
                    end if
--return parse_error(tok,"is_js '"'??")						
--                  break
                end if
                fallthrough
--          case '`':
--              if toktype='`'
--              and is_py() then
--                  ast = append(ast,{toktype,tdx})
--                  exit
--              end if
            default: 
--              return parse_error(tok,"letter expected (erm, ? or { or [ perhaps?)")
                return parse_error(tok,"unexpected token")
        end switch
    end while
    expectt(';',true)   -- (optional/skip)
    return ast
end function

--/*
function end_block()
    if parse_error() then return true end if
    sequence tok = tokens[tdx]
--  integer {toktype,start,finish,line} = tok
    integer {toktype} = tok
    if toktype=LETTER then
        integer ttidx = tok[TOKTTIDX]
        return find(ttidx,{T_end,T_elsif,T_else,T_case,T_default})!=0
--  elsif toktype=SYMBOL
--    and src[start..finish] = "}" then
--  elsif toktype='}' then
--      return true
    end if
--  return false
    return toktype='}'
end function
--*/

function block(integer skip=0, bool bOpt=false)
    tdx += skip
    sequence b = {}
    if not is_phix() then
        if not bOpt or tokens[tdx][TOKTYPE]=='{' then
            expectt('{')
            bOpt = false
        end if
    end if
--  while not end_block() do
    while not parse_error() do
        sequence tok = tokens[tdx]
        integer {toktype} = tok
        if toktype=COMMENT
        or toktype=BLK_CMT then
            b = append(b,tok)
            tdx += 1
        else
            if toktype=LETTER then
                integer ttidx = tok[TOKTTIDX]
                if find(ttidx,{T_end,T_elsif,T_else,T_case,T_default,T_catch}) then
                    exit
                end if
            elsif toktype='}' then
--              tdx -= 1
                exit
            end if
--          b = append(b, statement())
            b &= statement()
        end if
    end while
    if not is_phix() and bOpt==false then
        expectt('}')
    end if
    return b
end function

global function parse()
--DEV this needs to be nested... (oh, but not toplevel...)
    tokstack_clean(current_file)
    init_scope()
    sequence ast = {}
    tdx = 1
--  cdrop = 0
    clines = {}
    udts = {}
    parse_bad = false
ween = {}
    forwards = {}
--trace(1)
    while not parse_error() do
        if tdx>length(tokens) then
            if length(clines) then
                ?9/0 -- DEV temp/deal with now
            end if
            sequence incres = tokstack_pop()
            if incres="NO MORE" then
                final_scope_check()
--?{"autoincludes:",get_autoincludes()}
--dump_globals()
--dump_named_args()
                exit
            end if
            ast = append(ast,incres)
        else
--          ast = append(ast,statement())
            ast &= statement()
        end if
    end while
--  if cdrop>0 then
--      --
--      -- Mid-expression comments are dropped. Tough.
----DEV erm, we might be able to avoid this is peek_next_operator does /not/ advance tdx...
--      -- Some post-statement comments may also be dropped
--      -- as the parser scans for another operator - you 
--      -- can void this by preceding any such with ';'.
--      --
--      printf(1,"warning: %d comments dropped (lines %s)\n",{cdrop,join(clines,",")})
--  end if
--  return ast
    return {"program",ast}
end function

--/*
--
-- demo\rosetta\Compiler\parse.e
-- =============================
--
include lex.e

sequence tok

--procedure errd(sequence msg, sequence args={})
--  {tok_line,tok_col} = tok
--  error(msg,args)
--end procedure

global sequence toks
integer next_tok = 1

--function get_tok()
--  sequence tok = toks[next_tok]
--  next_tok += 1
--  return tok
--end function

--procedure expect(string msg, integer s)
--integer tk = tok[3]
--  if tk!=s then
--      errd("%s: Expecting '%s', found '%s'\n", {msg, tkNames[s], tkNames[tk]})
--  end if
--  tok = get_tok()
--end procedure

--function expr(integer p)
--object x = NULL, node
--integer op = tok[3] 
--
--  switch op do
--      case tk_LeftParen:
--          tok = get_tok()
--          x = expr(0)
--          expect("expr",tk_RightParen)
--      case tk_sub: 
--      case tk_add:
--          tok = get_tok()
--          node = expr(precedences[tk_neg]);
--          x = iff(op==tk_sub?{tk_neg, node, NULL}:node)
--      case tk_not:
--          tok = get_tok();
--          x = {tk_not, expr(precedences[tk_not]), NULL}
--      case tk_Identifier:
--          x = {tk_Identifier, tok[4]}
--          tok = get_tok();
--      case tk_Integer:
--          x = {tk_Integer, tok[4]}
--          tok = get_tok();
--      default:
--          errd("Expecting a primary, found: %s\n", tkNames[op])
--  end switch
-- 
--  op = tok[3]
--  while narys[op]=BINARY 
--    and precedences[op]>=p do
--      tok = get_tok()
--      x = {op, x, expr(precedences[op]+1)}
--      op = tok[3]
--  end while
--  return x;
--end function

--function paren_expr(string msg)
--  expect(msg, tk_LeftParen);
--  object t = expr(0)
--  expect(msg, tk_RightParen);
--  return t
--end function
--
--function stmt()
--object t = NULL, e, s
-- 
--  switch tok[3] do
--      case tk_if:
--          tok = get_tok();
--          object condition = paren_expr("If-cond");
--          object ifblock = stmt();
--          object elseblock = NULL;
--          if tok[3] == tk_else then
--              tok = get_tok();
--              elseblock = stmt();
--          end if
--          t = {tk_if, condition, {tk_if, ifblock, elseblock}}
--      case tk_putc:
--          tok = get_tok();
--          e = paren_expr("Prtc")
--          t = {tk_putc, e, NULL}
--          expect("Putc", tk_Semicolon);
--      case tk_print:
--          tok = get_tok();
--          expect("Print",tk_LeftParen)
--          while 1 do
--              if tok[3] == tk_String then
--                  e = {tk_Prints, {tk_String, tok[4]}, NULL}
--                  tok = get_tok();
--              else
--                  e = {tk_Printi, expr(0), NULL}
--              end if
--              t = {tk_Sequence, t, e}
--              if tok[3]!=tk_Comma then exit end if
--              expect("Print", tk_Comma)
--          end while
--          expect("Print", tk_RightParen);
--          expect("Print", tk_Semicolon);
--      case tk_Semicolon:
--          tok = get_tok();
--      case tk_Identifier:
--          object v
--          v = {tk_Identifier, tok[4]}
--          tok = get_tok();
--          expect("assign", tk_assign);
--          e = expr(0);
--          t = {tk_assign, v, e}
--          expect("assign", tk_Semicolon);
--      case tk_while:
--          tok = get_tok();
--          e = paren_expr("while");
--          s = stmt();
--          t = {tk_while, e, s}
--      case tk_LeftBrace:      /!* {stmt} *!/
--          expect("LeftBrace", tk_LeftBrace)
--          while not find(tok[3],{tk_RightBrace,tk_EOI}) do
--              t = {tk_Sequence, t, stmt()}
--          end while
--          expect("LeftBrace", tk_RightBrace);
--          break;
--      case tk_EOI:
--          break;
--      default: 
--          errd("expecting start of statement, found '%s'\n", tkNames[tok[3]]);
--  end switch
--  return t
--end function

--global 
--function parse()
--object t = NULL
--  tok = get_tok()
--  while 1 do
--      object s = stmt()
--      if s=NULL then exit end if
--      t = {tk_Sequence, t, s}
--  end while
--  return t
--end function

--*/
--</p2js_parse.e>

