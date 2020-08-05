--DEV I can kill this now... ;-((
-- NO: it is still used by eawpaint!
--
-- eascan.e
--
-- Source code scanner.
-- Builds routine lists, block start/end sets, etc.
-- Usually run in the background.
--
--constant withFolding = 0
--
--with trace

global integer mode
global constant smCONST = 1,
                smWINDOW = 2
--              ,
--              smROUTINE=3

global sequence Results
                Results = {}

global sequence text    --DEV take a copy of filetext[currfile] from edita.exw.
                text = ""

integer textlen
integer CurrLine, col, tokline, tokstart, tokend, toktype
sequence token
global
constant SYMBOL  = 1,
         DQUOTE  = 2,
         SQUOTE  = 3,
         ELLIPSE = 4,
         HEXDEC  = 5,
         DIGIT   = 6,
         FLOAT   = 7,
         USCORE  = 8,
         LETTER  = 9,
         SLICE   = 10,
         SUBSCR  = 11,
         ILLEGAL = 99

integer Ch

--integer errorcount
--      errorcount=0

procedure Abort(sequence msg)
sequence t1
--  if errorcount=0 then
--DEV messageBox?/proemh?
    if mode<=smWINDOW then
        if tokline and tokline<=textlen then
--          printf(1,"%s\n%s^ %s.\n",{ExpandTabs(text[tokline]),
--                                    repeat(' ',ExpLength(text[tokline][1..tokstart])-1),
--                                    msg})
            t1 = text[tokline]
            while find('\t',t1) do
                t1[find('\t',t1)] = ' '
            end while
--          printf(1,"%s\n%s^ %s.\n",{t1,repeat(' ',tokstart-1),msg})
--DEV use own dialogue, possibly fixed pitch, possibly repositioned.
            void = messageBox(sprintf("Error in line %d",{tokline}),
                              sprintf("%s\n%s^ %s.\n",{t1,repeat(' ',tokstart-1),msg}),0)
        else
--          printf(1,"%s.\n",{msg})
            void = messageBox("Error",msg,0)
        end if

--      errorcount+=1
--trace(1)
--  end if
--  Ch=-1
--  toktype=ILLEGAL
--  token=""
--if getc(0) then end if
--?9/0
        abort(0)
    else
--      Results={-1,sprintf("Line %d",tokline),text[tokline],"^",msg}
        Results = {-1,{"line",tokline},text[tokline],"^",msg}
        Ch = -1
        toktype = ILLEGAL
        token = "bogus"
    end if
end procedure
--      if lower(wait_key())='d' then ?9/0 end if
--  abort(0)

--with trace
procedure SkipBlockComment()
-- Skip nested /* block comments */
-- Note that "*/" inside a string is interpreted as end of comment, 
--  (since it is technically text not code, and for example we must
--   treat '/* if x="*/" then' as '" then'), though a "/*" and "*/" 
--   pair (of strings/within a string) behave as expected.
-- The opening /* has already been processed; if we cannot find a 
-- matching */ before EOF then display an error.
integer oline, ocol
--trace(1)
    ocol = col
    oline = CurrLine
-- if line!=0 then ?9/0 end if  -- we must maintain this!
    while CurrLine<=textlen do
        if col>=length(text[CurrLine]) then
            CurrLine += 1
            col = 1
        else
            Ch = text[CurrLine][col]
            if Ch='*' then
                if text[CurrLine][col+1]='/' then
                    col += 2
                    if col>length(text[CurrLine]) then
                        CurrLine += 1
                        col = 1
                    else
                        Ch = text[CurrLine][col]
                    end if
                    return
                end if
            elsif Ch='/' then
                if text[CurrLine][col+1]='*' then
                    col += 2
                    SkipBlockComment()
                    col -= 1
                end if
            end if
            col += 1
        end if
    end while
    tokline = oline
    tokstart = ocol
    Abort("missing closing block comment")
end procedure

sequence insertion_point,
         next_insertion_point
         next_insertion_point = {1,1}

procedure SkipSpacesAndComments()
    insertion_point = next_insertion_point
    next_insertion_point = {CurrLine,col}
    while 1 do
        while col=1 do
            if CurrLine>textlen then
                Ch = -1
                return
            end if
            if next_insertion_point[2]>1 then
                next_insertion_point = {CurrLine,1}
            end if
            if col<=length(text[CurrLine]) then
                Ch = text[CurrLine][1]
                exit
            end if
            CurrLine += 1
        end while
--      if equal(next_insertion_point,-1) then
--          next_insertion_point={CurrLine,col}
--      end if
        if Ch=' ' or Ch='\t' then
            col += 1
            if col>length(text[CurrLine]) then
                CurrLine += 1
                col = 1
            else
                Ch = text[CurrLine][col]
            end if
--10/07/20
--      elsif Ch='-'
        elsif find(Ch,"-/")
          and col<length(text[CurrLine])
--        and text[CurrLine][col+1]='-' then
          and text[CurrLine][col+1]=Ch then
            if col+3<=length(text[CurrLine])
            and text[CurrLine][col+2]='/'
            and text[CurrLine][col+3]='*' then
--trace(1)
                col += 4
                SkipBlockComment()
            else
                CurrLine += 1
                col = 1
            end if
-- 23/07/2013:
        elsif Ch='/'
          and col<length(text[CurrLine])
          and text[CurrLine][col+1]='*' then
            col += 2
            SkipBlockComment()
        else
            exit
        end if
    end while
end procedure

--integer tokint
atom tokint
atom tokatm
sequence charClass

procedure setCharClass(sequence chars, integer cclass)
    for i=1 to length(chars) do
        charClass[chars[i]] = cclass
    end for
end procedure

--constant Operators={"<","<=",">",">=","=","!=",
--                  "+","+=","-","-=","*","*=",
--                  "/","/=","..","&","&="}
constant Operators = {"==",":=","<=",">=","!=","+=","-=","*=","/=","&="}
    charClass = repeat(ILLEGAL,255)
    setCharClass("<=>!+-*/&()[]{},?:",SYMBOL)
    charClass['\"'] = DQUOTE
    charClass['\''] = SQUOTE
    charClass['#'] = HEXDEC
    charClass['.'] = ELLIPSE
    charClass['0'..'9'] = DIGIT
    charClass['_'] = USCORE
    charClass['A'..'Z'] = LETTER
    charClass['a'..'z'] = LETTER


procedure nextCh()
    tokend = col-1
--  token = text[CurrLine][tokstart..col-1]
    if col<=length(text[CurrLine]) then
        Ch = text[CurrLine][col]
    else
        CurrLine += 1
        col = 1
    end if
    SkipSpacesAndComments()
end procedure

procedure getFloat()
-- finish off a float. The first few DIGITS (if any) have been processed;
-- continue from '.' or 'eE'
integer lenTC, exponent, esign
atom dec, fraction
    lenTC = length(text[CurrLine])
    tokatm = tokint
    if Ch='.' then
        -- make sure it is not an ellipse
        if col<lenTC
        and text[CurrLine][col+1]='.' then
--          col -= 1
        else
            toktype = FLOAT
            dec = 1
            fraction = 0
            while 1 do
                col += 1
                if col>lenTC then exit end if
                Ch = text[CurrLine][col]
                if charClass[Ch]!=DIGIT then exit end if
                fraction = fraction*10 + (Ch-'0')
                dec *= 10
            end while
            tokatm += fraction/dec
        end if
    end if
    if find(Ch,"eE")
    and col<lenTC then
        toktype = FLOAT
        Ch = text[CurrLine][col+1]
        if Ch='-' or Ch='+' then
            col += 1
            Ch = text[CurrLine][col+1]
            esign = -1
        else
            esign = +1
        end if
        exponent = 0
        while 1 do
            col += 1
            if col>lenTC then exit end if
            Ch = text[CurrLine][col]
            if charClass[Ch]!=DIGIT then exit end if
            exponent = exponent*10+Ch-'0'
        end while
        tokatm = tokatm*power(10,exponent*esign)
    end if
--  token = text[CurrLine][tokstart..col-1]
    nextCh()
end procedure

function escMap(integer escChar)
    if escChar='t' then return '\t' end if
    if escChar='r' then return '\r' end if
    if escChar='n' then return '\n' end if
    if escChar='\"' then return '\"' end if
    if escChar='\'' then return '\'' end if
    if escChar='\\' then return '\\' end if
    -- oops!
end function

integer mapEndToMinusOne
        mapEndToMinusOne = 0

--with trace
procedure getToken()
integer nxtCh, lenTC
    if Ch=-1 then return end if
    tokline = CurrLine
    tokstart = col
    token = ""
    toktype = charClass[Ch]
    lenTC = length(text[CurrLine])
    if col<lenTC then
        nxtCh = text[CurrLine][col+1]
    else
        nxtCh = -1
    end if
    if toktype=LETTER then
        while 1 do
            col += 1
            if col>lenTC then exit end if
            nxtCh = charClass[text[CurrLine][col]]
            if nxtCh<DIGIT then exit end if
            if nxtCh>LETTER then exit end if
        end while
        token = text[CurrLine][tokstart..col-1]
        if mapEndToMinusOne and equal(token,"end") then
            token = "-1"
            toktype = DIGIT
        end if
        nextCh()
    elsif toktype=SYMBOL then
--      token = {Ch,nxtCh}
--      if find(token,Operators) then
        if find({Ch,nxtCh},Operators) then
            col += 1
--      else
--          token = {Ch}
--          col += 1
        elsif Ch='?' then
            toktype = LETTER
        end if
        col += 1
        token = text[CurrLine][tokstart..col-1]
        nextCh()
    elsif toktype=DQUOTE then
        while 1 do
            col += 1
            if col>lenTC then
                Abort("missing \"")
                exit
            end if
            nxtCh = text[CurrLine][col]
            if nxtCh='\\' then
                col += 1
            elsif nxtCh='\"' then
                col += 1
                exit
            end if
        end while
        token = text[CurrLine][tokstart..col-1]
--      token = text[CurrLine][tokstart+1..col-2]
        nextCh()
    elsif toktype=SQUOTE then
        if nxtCh='\\' then
            tokint = escMap(text[CurrLine][col+2])
            col += 4
        else
            tokint = text[CurrLine][col+1]
            col += 3
        end if
--      if col>lenTC then ?9/0 end if
--      token = text[CurrLine][tokstart..col-1]
--      col+=1
        nextCh()
    elsif toktype=HEXDEC then
        tokatm = 0
        while 1 do
            col += 1
            if col>lenTC then exit end if
            nxtCh = text[CurrLine][col]
            if nxtCh<'0' then exit end if
            if nxtCh>'9' then
                if nxtCh<'A' then exit end if
                if nxtCh>'F' then
                    if nxtCh<'a' then exit end if
                    if nxtCh>'f' then exit end if
                    tokatm = tokatm*16+nxtCh-'W'
                else
                    tokatm = tokatm*16+nxtCh-'7'
                end if
            else
                tokatm = tokatm*16+nxtCh-'0'
            end if
        end while
-- 19/03/10 (gave up)
-- 18/07/13 (made it happen)
        if nxtCh='i' and tokstart=col-1 then
--          Abort("Sorry, #ilasm etc should be hidden from eawpaint with --/**/")
            nextCh()
            getToken()
            if toktype!=LETTER
            or not find(token,{"ilasm","ilASM"}) then
                Abort("ilASM expected")
            end if
            getToken()
            if toktype!=SYMBOL
            or not equal(token,"{") then
                Abort("{ expected")
            end if
            while 1 do
                getToken()
                if toktype=SYMBOL then
                    if equal(token,"}") then exit end if
                end if
                while find(Ch,"%@") do
                    col += 1
--trace(1)
                    nextCh()
                end while
            end while
--trace(1)
            getToken()
        else
            nextCh()
        end if
--      token = text[CurrLine][tokstart..col-1]
--      nextCh()
    elsif toktype=ELLIPSE then
        if nxtCh='.' then
            col += 2
--          token = text[CurrLine][tokstart..col-1]
            nextCh()
        else
            tokint = 0
            getFloat()
        end if
    elsif toktype<=DIGIT then
        tokint = Ch-'0'
        while 1 do
            col += 1
            if col>lenTC then exit end if
            Ch = text[CurrLine][col]
            if charClass[Ch]!=DIGIT then exit end if
            tokint = tokint*10+Ch-'0'
        end while
        if find(Ch,".eE") then
            getFloat()
        else
--          token = text[CurrLine][tokstart..col-1]
            nextCh()
        end if
    elsif toktype=ILLEGAL
      and mapEndToMinusOne
      and Ch='$' then
        mapEndToMinusOne = '$'
        token = "-1"
        toktype = DIGIT
        col += 1
        nextCh()
    else
        Abort("unrecognised")
    end if
--pp(token)
end procedure

procedure Check(sequence text)
    if not equal(token,text) then
        Abort(text&" expected")
    end if
end procedure

procedure Match(sequence text)
    if not equal(token,text) then
        Abort(text&" expected")
    end if
    getToken()
end procedure

integer rExpression

--with trace
function DoSequence(integer allowqu)
-- Process a sequence
-- allowqu is from DoMultipleAssignment, ie/eg {a,?,b} = <expr>.
integer wasMapEndToMinusOne -- allow {1,2,3,$} [==={1,2,3}]
sequence res
    wasMapEndToMinusOne = mapEndToMinusOne
    mapEndToMinusOne = 1
    Match("{")
    res = {}
    while not equal(token,"}") do
        if mapEndToMinusOne='$' then
            getToken()
            exit
        end if
        mapEndToMinusOne = 0
        if allowqu then
            if token="?" then
                getToken()
            else
                void = call_func(rExpression,{})
            end if
        elsif mode<=smWINDOW then
            res = append(res,call_func(rExpression,{}))
            res = append(res,{SYMBOL,token,tokline,tokstart,tokend})
        else
            void = call_func(rExpression,{})
        end if
        if not equal(token,",") then exit end if
        if Ch='$' then
            nextCh()
            exit
        end if
        mapEndToMinusOne = 1
        Match(",")
    end while
    mapEndToMinusOne = wasMapEndToMinusOne
    Match("}")
    return res
end function

function Params()
sequence res
    res = {}
    getToken()
    Match("(")
    if not equal(token,")") then
        while 1 do
            if mode<=smWINDOW then
                res = append(res,call_func(rExpression,{}))
            else
                void = call_func(rExpression,{})
            end if
            if not equal(token,",") then exit end if
            getToken()
        end while
    end if
    Match(")")
    return res
end function

without trace
function DoSubScripts(object res)
integer wasMapEndToMinusOne
object subscr1, subscr2
    wasMapEndToMinusOne = mapEndToMinusOne
    mapEndToMinusOne = 1
    while Ch='[' do
        getToken()
        Match("[")
        subscr1 = call_func(rExpression,{})
        if toktype=ELLIPSE then
            getToken()
            subscr2 = call_func(rExpression,{})
            Check("]")
            if mode<=smWINDOW then
                res = {SLICE,subscr1,subscr2,res}
            end if
            exit
        end if
        Check("]")
        if mode<=smWINDOW then
            res = {SUBSCR,subscr1,res}
        end if
    end while
    mapEndToMinusOne = wasMapEndToMinusOne
    getToken()
    return res
end function

global constant FUNC = 1,   --DEV
                SEQUENCE = -5,
                UNKNOWN = -1
sequence constname  -- set by DoConstant
         constname = ""
integer constline,conststart,constend
constline = 0
conststart = 0
constend = 0

--with trace
function Factor()
object res
    if mode<=smWINDOW then
        if toktype=LETTER then
            if Ch=':' then getToken() getToken() end if -- skip namespaces
            if Ch='(' then  -- a function, we presume
                res = {FUNC,token,tokline,tokstart,tokend,constname,0,0,0}
                res[9] = {UNKNOWN,constname,constline,conststart,constend}
                constname = ""
                res[7] = Params()
            else            -- a variable, we presume
                res = DoSubScripts({UNKNOWN,token,tokline,tokstart,tokend})
            end if
        elsif toktype=DIGIT then
            res = {DIGIT,tokint,tokline,tokstart,tokend}
            getToken()
        elsif toktype=FLOAT then
            res = {FLOAT,tokatm,tokline,tokstart,tokend}
            getToken()
        elsif toktype=HEXDEC then
            res = {HEXDEC,tokatm,tokline,tokstart,tokend}
            getToken()
        elsif toktype=DQUOTE then
--DEV should we leave the quotes in???
--18/07/2013:
--          res = {DQUOTE,token[2..length(token)-1],tokline,tokstart+1,tokend-1}
            res = {DQUOTE,token,tokline,tokstart,tokend}
            getToken()
        elsif toktype=SQUOTE then
            res = {SQUOTE,tokint,tokline,tokstart,tokend}
            getToken()
        elsif toktype=SYMBOL and equal(token,"{") then
            res = {SEQUENCE,token,tokline,tokstart,tokend}
            res = append(res,DoSequence(0))
        elsif toktype=SYMBOL and equal(token,"(") then
            getToken()
            res = call_func(rExpression,{})
            Match(")")
        else
            Abort("unrecognised")
            res = -1
        end if
        return res
    else
        if toktype=LETTER then
            if Ch=':' then getToken() getToken() end if -- skip namespaces
            if Ch='(' then  -- a function, we presume
                void = Params()
            else            -- a variable, we presume
                void = DoSubScripts(0)
            end if
        elsif find(toktype,{DIGIT,FLOAT,HEXDEC,DQUOTE,SQUOTE}) then
            getToken()
        elsif toktype=SYMBOL and equal(token,"{") then
            void = DoSequence(0)
        elsif toktype=SYMBOL and equal(token,"(") then
            getToken()
            void = call_func(rExpression,{})
            Match(")")
        else
            Abort("unrecognised")
        end if
        return 0
    end if
---- Parse and Translate a Math Factor
--object etype
--integer N
--  if toktype = '(' then
--      Next()
--      call_proc(rExpression,{})
--      Match(")")
--      etype='O' --DEV could do better. E- (lack of effort)
--  else
--      if toktype = LETTER then
----if equal(token,"or_all") then trace(1) end if
--          N=InAnyTable(1)
--          if N=0 then -- forward function call?
--              if Ch!='(' then Undefined(token) end if
--              ForwardProc(token,{'f',GetFnLn()},1)
--              etype='O'
--          else
--              etype=VarType(N)
----                MarkUsed(N)
--              if N<0 and N>=BI_BImax then 
----                    CallBuiltin(token,N,etype,"FfTt")
--                  CallFunc(N,etype)
--              elsif sequence(etype) then
----if sequence(etype[1]) then trace(1)
----DEBUG=globvar[-N]
----DEBUG=globtype[-N]
---- end if
--                  if etype[1]<0 then -- literal constant?
--                      if find(etype[1],{-'S',-'P',-'N'}) then
--                          PushFactor(LiteralValue(N),0,-etype[1])
--                      else
--                          PushFactor(LiteralValue(N),1,-etype[1])
--                      end if
--                      Next()
--                  else
--                      CallFunc(N,etype)
--                  end if
--              else
--                  PushFactor(Uniq(N),0,etype)
--                  Next()
--              end if
--          end if
--      elsif toktype = DIGIT then
--          PushFactor(token,1,'I')
--          Next()
--          etype='I'
--      elsif toktype = FLOAT then
--          PushFactor(token,0,'N') -- Actually a _Const by now
--          Next()
--          etype='N'
--      elsif toktype = HEXDEC then
--          PushFactor(sprintf("%d",getHex()),1,'I')
--          Next() 
--          etype='I'
--      elsif toktype = '{' then
--          DoSequence(0)
--          etype='P'
--      elsif toktype=DQUOTE then
--          PushFactor(token,0,'S') -- Actually a _Const by now
--          Next()
--          etype='S'
--      elsif toktype=SQUOTE then
--          PushFactor(token,1,'I') -- squotes stored as literal integers.
--          Next()
--          etype='I'
--      else
--          Expected("Math Factor")
--      end if
--  end if
--  if toktype='[' then
--      if find(etype,"IN") then Abort("attempt to subscript an atom") end if
--      DoSubScripts()
--  end if
end function

function NotFactor()
object res
---- Parse and translate an optional unary opcode preceding a factor
    if toktype=SYMBOL and equal(token,"-") then
        Match("-")
        res = {"-",Factor(),tokline,tokstart,tokend}
    elsif toktype=SYMBOL and equal(token,"+") then -- (ignore)
        Match("+")
        res = Factor()
    elsif toktype=LETTER and equal(token,"not") then
        Match(token)
        res = {"not",Factor(),tokline,tokstart,tokend}
    else
        res = Factor()
    end if
    return res
end function

function Term()
object res
    res = NotFactor()
    while find(token,{"*","/"}) do
        if mode<=smWINDOW then
            res = {token,res}
            Match(token)
            res = append(res,NotFactor())
        else
            Match(token)
            void = NotFactor()
        end if
    end while
    return res
end function

global constant BPLUS = -12, BMINUS = -11
function Sum()
object res
integer k
    res = Term()
    while 1 do
        k = find(token,{"+","-"})
        if not k then exit end if
        if mode<=smWINDOW then
            if k=1 then
                res = {BPLUS,tokint,tokline,tokstart,tokend,res,0}
            else
                res = {BMINUS,tokint,tokline,tokstart,tokend,res,0}
            end if
            Match(token)
            res[7] = Term()
        else
            Match(token)
            void = Term()
        end if
    end while
    return res
end function

function Concat()
object res
    res = Sum()
    while equal(token,"&") do
        Match("&")
        if mode<=smWINDOW then
            res = {"&",res,Sum()}
        else
            void = Sum()
        end if
    end while
    return res
end function

function Relation()
object res
integer k
    res = Concat()
    while 1 do
        k = find(token,{"<","<=","=","==","!=",">",">="})
        if k=0 then exit end if
        if mode<=smWINDOW then
            res = {token,res}
            Match(token)
            res = append(res,Concat())
        else
            Match(token)
            void = Concat()
        end if
    end while
    return res
end function

function Expression()
object res
    res = Relation()
    while find(token,{"and","or","xor"}) do
        if mode<=smWINDOW then
            res = {token,res}
            Match(token)
            res = append(res,Relation())
        else
            Match(token)
            void = Relation()
        end if
    end while
    return res
end function
rExpression = routine_id("Expression")

--procedure QExpression()
---- AHEM, this was meant to speed things up, but it don't...
--integer wasMapEndToMinusOne
--  while 1 do
--      while 1 do
--          while 1 do
--              while 1 do
--                  while 1 do
--                      if find(token,{"-","+","not"}) then
--                          Match(token)
--                      end if
--                      if toktype=LETTER then
--                          if Ch='(' then  -- a function, we presume
--                              getToken()
--                              Match("(")
--                              if not equal(token,")") then
--                                  while 1 do
--                                      QExpression()
--                                      if not equal(token,",") then exit end if
--                                      getToken()
--                                  end while
--                              end if
--                              Match(")")
--                          else            -- a variable, we presume
--                              wasMapEndToMinusOne=mapEndToMinusOne
--                              mapEndToMinusOne=1
--                              while Ch='[' do
--                                  getToken()
--                                  Match("[")
--                                  QExpression()
--                                  if toktype=ELLIPSE then
--                                      getToken()
--                                      QExpression()
--                                      Check("]")
--                                      exit
--                                  end if
--                                  Check("]")
--                              end while
--                              mapEndToMinusOne=wasMapEndToMinusOne
--                              getToken()
--                          end if
--                      elsif find(toktype,{DIGIT,FLOAT,HEXDEC,DQUOTE,SQUOTE}) then
--                          getToken()
--                      elsif toktype = SYMBOL and equal(token,"{") then
-----                           QDoSequence()
--                          Match("{")
--                          while not equal(token,"}") do
--                              QExpression()
--                              if not equal(token,",") then exit end if
--                              Match(",")
--                          end while
--                          Match("}")
--                      elsif toktype=SYMBOL and equal(token,"(") then
--                          getToken()
--                          QExpression()
--                          Match(")")
--                      else
--                          Abort("unrecognised")
--                      end if
--                      if not find(token,{"*","/"}) then exit end if
--                      Match(token)
--                  end while
--                  if not find(token,{"+","-"}) then exit end if
--                  Match(token)
--              end while
--              if not equal(token,"&") then exit end if
--              Match(token)
--          end while
--          if not find(token,{"<","<=","=","!=",">",">="}) then exit end if
--          Match(token)
--      end while
--      if not find(token,{"and","or","xor"}) then exit end if
--      Match(token)
--  end while
--end procedure
----qExpression=routine_id("QExpression")

--procedure appendX(sequence entry)
--integer line, k
--  line=entry[2]
--  k=length(Results)
--  while k and line<Results[k][2] do
--      k-=1
--  end while
--  if k=length(Results) then
--      Results=append(Results,entry)
--  else
--      Results=Results[1..k]&0&Results[k+1..length(Results)]
--      Results[k+1]=entry
--  end if
--end procedure

integer rBlock

--with trace
procedure DoIf(integer returnexpr)
--
-- Recognize and Translate an IF Construct
-- returnexpr is a flag, 1 for function/type return <expr>,
--                       0 for procedure returns,
--                      -1 for toplevel statements
--
--integer blockstart
--sequence pTok
--  if withFolding then
--      blockstart = tokline
--      pTok = token
--  end if
    Match("if")
    void = Expression()
--QExpression()
--trace(1)
    Match("then")
    call_proc(rBlock,{returnexpr})
    while equal(token,"elsif") do
--      if withFolding and mode=smROUTINE and tokline-1>blockstart then
--          appendX({pTok,blockstart,tokline-1})
--          blockstart = tokline
--          pTok = token
--      end if
        Match("elsif")
        void = Expression()
--QExpression()
        Match("then")
        call_proc(rBlock,{returnexpr})
    end while
    if equal(token,"else") then
--      if withFolding and mode=smROUTINE and tokline-1>blockstart then
--          appendX({pTok,blockstart,tokline-1})
--          blockstart = tokline
--          pTok = token
--      end if
        Match("else")
        call_proc(rBlock,{returnexpr})
    end if
    Match("end")
--  if withFolding and mode=smROUTINE and tokline>blockstart then
----        Results=append(Results,{"if",blockstart,tokline})
--      appendX({pTok,blockstart,tokline})
--  end if
    Match("if")
end procedure

procedure DoWhile(integer returnexpr)
--
-- Parse and Translate a WHILE Statement
-- returnexpr is a flag, 1 for function/type return <expr>,
--                       0 for procedure returns,
--                      -1 for toplevel statements
--
integer blockstart
    blockstart = tokline
    Match("while")
    void = Expression()
--QExpression()
    Match("do")
    call_proc(rBlock,{returnexpr})
    Match("end")
--  if withFolding and mode=smROUTINE and tokline>blockstart then
----        Results=append(Results,{"while",blockstart,tokline})
--      appendX({"while",blockstart,tokline})
--  end if
    Match("while")
end procedure

procedure DoFor(integer returnexpr)
----
---- Parse and Translate a For Statement
---- returnexpr is a flag, 1 for function/type (permitting return <expr>),
----                         0 for procedure (permitting return),
----                        -1 for toplevel statements (prohibiting return),
----                           and forcing control var to be static, not stack.
----
integer blockstart
    blockstart = tokline
    Match("for")
    getToken()
    Match("=")
    void = Expression()
--QExpression()
    Match("to")
    void = Expression()
--QExpression()
    if equal(token,"by") then
        getToken()
        void = Expression()
--QExpression()
    end if
    Match("do")
    call_proc(rBlock,{returnexpr})

    Match("end")
--  if withFolding and mode=smROUTINE and tokline>blockstart then
----        Results=append(Results,{"for",blockstart,tokline})
--      appendX({"for",blockstart,tokline})
--  end if
    Match("for")
end procedure

procedure DoExit()
    Match("exit")
end procedure

procedure DoReturn(integer returnexpr)
---- returnexpr is a flag, 1 for function/type return <expr>,
----                         0 for procedure returns,
----                        -1 for toplevel statements
    if returnexpr=-1 then Abort("return must be inside a procedure or function") end if
    Match("return")
    if returnexpr then
        void = Expression()
--QExpression()
    end if
end procedure

procedure DoQu()
----
---- The '?' shorthand.
----
    getToken()
    void = Expression()
--QExpression()
end procedure

--procedure AssignOrProc()
---- Decide if a Statement is an Assignment or Procedure Call
--object Type
--integer N
----trace(1)
--  N=InAnyTable(1)
--  if N=0 then -- forward procedure call?
--      if Ch!='(' then Undefined(token) end if
--      ForwardProc(token,{'p',GetFnLn()},0)
--  else
--      Type=VarType(N)
--      if N<0 and N>=BI_BImax then --CallBuiltin(token,N,Type,"Pp")
--                                CallProc(N,Type)
--      elsif sequence(Type) then CallProc(N,Type)
--      elsif find(Type,INSPO) then Next() Assignment(N,Type)
--      else Abort("Identifier " & token & " cannot be used here")
--      end if
--  end if
--end procedure

procedure Proc()
    void = Params()
end procedure

procedure Assignment()
    void = DoSubScripts(0)
    if not find(token,{"=",":=","+=","-=","*=","/=","&="}) then
--?9/0
        Abort("assignment operator expected")
    else
        getToken()
        void = Expression()
--QExpression()
    end if
end procedure

procedure DoMultipleAssignment()
--trace(1)
    void = DoSequence(1)
--DEV as above (=/+=/-=..., if it ever goes into Phix)
    if equal(token,":=") then
        Match(":=")
    else
        Match("=")
    end if
    void = call_func(rExpression,{})
end procedure

sequence vartypes
procedure Locals()
    while find(token,vartypes) do
        while 1 do
            getToken()
            if Ch='=' then
                getToken()
                getToken()
                void = Expression()
                if toktype!=SYMBOL
                or token!="," then
                    exit
                end if
            else
                if Ch!=',' then
                    getToken()
                    exit
                end if
                getToken()
            end if
        end while
    end while
end procedure

--with trace
procedure DoRoutineDef(integer rType)
sequence sType, res

    sType = token
    getToken()
    res = {token,tokline,0}
    if rType=3 then vartypes = append(vartypes,token) end if
    getToken()
    Match("(")
    if not equal(token,")") then
--trace(1)
        while 1 do
-- 22/07/2013
if not find(Ch,"=,") then
            if not find(token,vartypes) then
                Abort("a type is expected here")
                exit
            end if
            getToken()
end if
--          while 1 do
                if Ch='=' then
                    getToken()
                    getToken()
                    void = Expression()
                    if toktype!=SYMBOL
                    or token!="," then
                        exit
                    end if
                else
                    getToken()
                    if not equal(token,",") then exit end if
                end if
                getToken()
--              if not find(Ch,"=,") then exit end if
--          end while
        end while
    end if
    Match(")")
    Locals()
    call_proc(rBlock,{rType>1})
    Match("end")
--  if mode=smROUTINE then
--      res[3]=tokline
--      if withFolding then
--          appendX(res)
--      else
--          Results=append(Results,res)
--      end if
--  end if
    Match(sType)
end procedure

procedure Statement(integer returnexpr)
integer stmtidx
    if toktype=LETTER then
        stmtidx = find(token,{"if","for","while","exit","return","?"})
        if stmtidx=1 then DoIf(returnexpr)
        elsif stmtidx=2 then DoFor(returnexpr)
        elsif stmtidx=3 then DoWhile(returnexpr)
        elsif stmtidx=4 then DoExit()
        elsif stmtidx=5 then DoReturn(returnexpr)
        elsif stmtidx=6 then DoQu()
        elsif Ch='(' then Proc()
        else
            if find(token,vartypes) then Locals() end if
            if Ch=':' then -- skip namespaces[?]
                getToken()
                if find(token,{":"}) then
                    Match(":") -- it was indeed a namespace
                else
                    Match(":=")
                    void = Expression()
                end if
            else
                Assignment()
            end if
        end if
--DEV removed 20/07/2013 (as getToken sets toktype to LETTER...???)
--  elsif toktype=SYMBOL and equal(token,"?") then
--      DoQu()
    elsif toktype=SYMBOL and equal(token,"{") then
        DoMultipleAssignment()
    else
        Abort("unrecognised")
    end if
end procedure

--with trace
procedure Block(integer returnexpr)
--  while toktype=LETTER do
--      if find(token,{"elsif","else","end"}) then exit end if
    while 1 do
        if toktype=LETTER then
            if find(token,{"elsif","else","end"}) then exit end if
        else
            if toktype!=SYMBOL or token!="{" then exit end if
--trace(1)
        end if
        Statement(returnexpr)
    end while
end procedure
rBlock = routine_id("Block")

--with trace
procedure TopDecls()
--trace(1)
    while 1 do
--?token
        getToken()
--?{token}
--if token="columns" then trace(1) end if
--if token="crun" then trace(1) end if
-- 17/07/2013:
        if Ch='=' then
            getToken()
            getToken()
            void = Expression()
            if toktype!=SYMBOL
            or token!="," then
                return
            end if
        else
            if Ch!=',' then exit end if
            getToken()
        end if
    end while
    getToken()
end procedure

global sequence constnames, constvals
    constnames = {} constvals = {}
global integer constidx
    constidx = 0

global integer resolvable

--with trace
global function Resolve(object constval)
sequence res
integer idx
--object subscr1, subscr2, Seq
object cv1
    cv1 = constval[1]
    if sequence(cv1) then
        resolvable = 0
        return 0
    elsif cv1=BPLUS then
        return Resolve(constval[6])+Resolve(constval[7])
    elsif cv1=UNKNOWN then
        idx = find(constval[2],constnames)
        if idx then
            return constvals[idx]
        end if
        resolvable = 0
        return 0
    elsif cv1=FUNC then
        if equal(constval[2],"or_all") then
            return or_all(Resolve(constval[7][1]))
        elsif equal(constval[2],"or_bits") then
            return or_bits(Resolve(constval[7][1]),Resolve(constval[7][2]))
        end if
        resolvable = 0
        return 0
--18/07/2013:
    elsif cv1=DQUOTE then
--      return constval[2][2..-2]   -- strip quotes
        return constval[2][2..$-1]  -- strip quotes
--  elsif find(cv1,{DQUOTE,SQUOTE,HEXDEC,DIGIT,FLOAT}) then
    elsif find(cv1,{SQUOTE,HEXDEC,DIGIT,FLOAT}) then
        return constval[2]
    elsif cv1=SEQUENCE then
        res = {}
        for i=1 to length(constval[6]) by 2 do
            res = append(res,Resolve(constval[6][i]))
        end for
        return res
    elsif cv1=SUBSCR then
--DEV this sort of works, but we can't replace it. Maybe it should be
-- {SUBSCR,{seq},{"["},{idx},{"]"}} like SEQUENCE is.
--      if resolvable then
--          subscr1=Resolve(constval[2])
--          if resolvable and integer(subscr1) then
--              Seq=Resolve(constval[3])
--              if resolvable and sequence(Seq)
--              and subscr1>=1 and subscr1<=length(Seq) then
--                  return Seq[subscr1]
--              end if
--          end if
--      end if
        resolvable = 0
        return 0
    elsif cv1=SLICE then
--DEV ditto, with ".." and {endslice}
--      if resolvable then
--          subscr1=Resolve(constval[2])
--          if resolvable and integer(subscr1) then
--              subscr2=Resolve(constval[3])
--              if resolvable and integer(subscr2) then
--                  Seq=Resolve(constval[4])
--                  if resolvable and sequence(Seq)
--                  and subscr1>=1 and subscr2<=length(Seq)
--                  and subscr2>=subscr1-1 then
--                      return Seq[subscr1..subscr2]
--                  end if
--              end if
--          end if
--      end if
        resolvable = 0
        return 0
    end if
--?9/0
    resolvable = 0
    return 0
end function

global sequence insertion_points
                insertion_points = {}
procedure DoConstant()
integer ip
    ip = 0
--  getToken()  -- discard the word "constant".
--trace(1)
    while 1 do
-- 30/12/13:
        getToken()  -- "constant" and ","
        if toktype!=LETTER then
            -- support constant a=1,$ for OpenEu compatibility
            if mapEndToMinusOne='$' and toktype=DIGIT and token = "-1" then
                mapEndToMinusOne = 0
                getToken()
                exit
            end if
            Abort("a name is expected here")
        end if
        mapEndToMinusOne = 0
--17/07/2013:
        if find(token,vartypes) then
            getToken()
        end if
        if mode<=smWINDOW then
            constname = token   -- save the constant name...
            constline = tokline
            conststart = tokstart
            constend = tokend
        end if

        getToken()          -- and discard it
        Match("=")
        if Ch=':' then getToken() getToken() end if -- skip namespaces
        if mode=smWINDOW and equal(token,"create") then
--          Results=append(Results,Expression())
            Results = append(Results,Factor())
            ip = 1  -- mark as valid insertion point
        elsif mode<=smWINDOW then   -- smCONST(1) or smWINDOW(2)
            constidx += 1
            if constidx>length(constnames) then
                constnames &= repeat(0,1024)
                constvals &= repeat(0,1024)
            end if
            constnames[constidx] = constname
            resolvable = 1
            constvals[constidx] = Resolve(Expression())
            if not resolvable then
                constidx -= 1
            end if
        else
            void = Expression()
        end if
        if not equal(token,",") then exit end if
--      getToken()
        mapEndToMinusOne = -1
    end while
    if ip then
        if Ch=-1 then
            insertion_point = next_insertion_point
        end if
        insertion_points = append(insertion_points,insertion_point)
    end if
end procedure

procedure DoEnum()
integer wasMapEndToMinusOne -- allow enum 1,2,3,$ [=== enum 1,2,3]
    wasMapEndToMinusOne = mapEndToMinusOne
    mapEndToMinusOne = 1
    Match("enum")
    while 1 do
        if mapEndToMinusOne='$' then
            getToken()
            exit
        end if
        mapEndToMinusOne = 0
        getToken()
        if equal(token,"=") then
            getToken()
            void = Expression()
        end if
        if not equal(token,",") then exit end if
        mapEndToMinusOne = 1
        Match(",")
    end while
    mapEndToMinusOne = wasMapEndToMinusOne
end procedure

global procedure Scan()
integer t
    CurrLine = 1
    vartypes = {"atom","integer","sequence","string","object"}
    textlen = length(text)
    if textlen
    and match("#!",text[1])=1 then  -- skip shebang
        CurrLine = 2
    end if
    col = 1
    SkipSpacesAndComments()
    getToken()
--c=0
    while Ch!=-1 do
        if toktype=LETTER then
            t = find(token,{"procedure","function","type"})
            if t then
                DoRoutineDef(t)
            elsif equal(token,"constant") then
                DoConstant()
            elsif equal(token,"enum") then
                DoEnum()
            elsif equal(token,"global") then
                getToken()  -- ignore it
            elsif find(token,{"include","with","without"}) then
                CurrLine += 1
                col = 1
                SkipSpacesAndComments()
                getToken()
            else
                if Ch=':' then getToken() getToken() end if -- skip namespaces
                if find(token,vartypes) then
                    TopDecls()
                else
                    Statement(-1)
                end if
            end if
        else
            Abort("invalid")
        end if
--      if c_func(xGetQueueStatus, {QS_ALLINPUT}) > #FFFF then
        doEvents(0)
--      end if
    end while
end procedure
