--
-- demo\rosetta\Compiler\parse.e
-- =============================
--
--  The reusable part of parse.exw
--

include lex.e

sequence tok

procedure errd(sequence msg, sequence args={})
    {tok_line,tok_col} = tok
    error(msg,args)
end procedure

global sequence toks
integer next_tok = 1

function get_tok()
    sequence tok = toks[next_tok]
    next_tok += 1
    return tok
end function

procedure expect(string msg, integer s)
integer tk = tok[3]
    if tk!=s then
        errd("%s: Expecting '%s', found '%s'\n", {msg, tkNames[s], tkNames[tk]})
    end if
    tok = get_tok()
end procedure

function expr(integer p)
object x = NULL, node
integer op = tok[3] 

    switch op do
        case tk_LeftParen:
            tok = get_tok()
            x = expr(0)
            expect("expr",tk_RightParen)
        case tk_sub: 
        case tk_add:
            tok = get_tok()
            node = expr(precedences[tk_neg]);
            x = iff(op==tk_sub?{tk_neg, node, NULL}:node)
        case tk_not:
            tok = get_tok();
            x = {tk_not, expr(precedences[tk_not]), NULL}
        case tk_Identifier:
            x = {tk_Identifier, tok[4]}
            tok = get_tok();
        case tk_Integer:
            x = {tk_Integer, tok[4]}
            tok = get_tok();
        default:
            errd("Expecting a primary, found: %s\n", tkNames[op])
    end switch
 
    op = tok[3]
    while narys[op]=BINARY 
      and precedences[op]>=p do
        tok = get_tok()
        x = {op, x, expr(precedences[op]+1)}
        op = tok[3]
    end while
    return x;
end function

function paren_expr(string msg)
    expect(msg, tk_LeftParen);
    object t = expr(0)
    expect(msg, tk_RightParen);
    return t
end function

function stmt()
object t = NULL, e, s
 
    switch tok[3] do
        case tk_if:
            tok = get_tok();
            object condition = paren_expr("If-cond");
            object ifblock = stmt();
            object elseblock = NULL;
            if tok[3] == tk_else then
                tok = get_tok();
                elseblock = stmt();
            end if
            t = {tk_if, condition, {tk_if, ifblock, elseblock}}
        case tk_putc:
            tok = get_tok();
            e = paren_expr("Prtc")
            t = {tk_putc, e, NULL}
            expect("Putc", tk_Semicolon);
        case tk_print:
            tok = get_tok();
            expect("Print",tk_LeftParen)
            while 1 do
                if tok[3] == tk_String then
                    e = {tk_Prints, {tk_String, tok[4]}, NULL}
                    tok = get_tok();
                else
                    e = {tk_Printi, expr(0), NULL}
                end if
                t = {tk_Sequence, t, e}
                if tok[3]!=tk_Comma then exit end if
                expect("Print", tk_Comma)
            end while
            expect("Print", tk_RightParen);
            expect("Print", tk_Semicolon);
        case tk_Semicolon:
            tok = get_tok();
        case tk_Identifier:
            object v
            v = {tk_Identifier, tok[4]}
            tok = get_tok();
            expect("assign", tk_assign);
            e = expr(0);
            t = {tk_assign, v, e}
            expect("assign", tk_Semicolon);
        case tk_while:
            tok = get_tok();
            e = paren_expr("while");
            s = stmt();
            t = {tk_while, e, s}
        case tk_LeftBrace:      /* {stmt} */
            expect("LeftBrace", tk_LeftBrace)
            while not find(tok[3],{tk_RightBrace,tk_EOI}) do
                t = {tk_Sequence, t, stmt()}
            end while
            expect("LeftBrace", tk_RightBrace);
            break;
        case tk_EOI:
            break;
        default: 
            errd("expecting start of statement, found '%s'\n", tkNames[tok[3]]);
    end switch
    return t
end function

global function parse()
object t = NULL
    tok = get_tok()
    while 1 do
        object s = stmt()
        if s=NULL then exit end if
        t = {tk_Sequence, t, s}
    end while
    return t
end function

