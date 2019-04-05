--
-- demo\rosetta\Compiler\cgen.e
-- ============================
--
--  The reusable part of cgen.exw
--

include parse.e

global sequence vars = {},
                strings = {},
                stringptrs = {}

global integer chain = 0
global sequence code = {}

function var_idx(sequence inode)
    if inode[1]!=tk_Identifier then ?9/0 end if
    string ident = inode[2]
    integer n = find(ident,vars)
    if n=0 then
        vars = append(vars,ident)
        n = length(vars)
    end if
    return n
end function

function string_idx(sequence inode)
    if inode[1]!=tk_String then ?9/0 end if
    string s = inode[2]
    integer n = find(s,strings)
    if n=0 then
        strings = append(strings,s)
        stringptrs = append(stringptrs,0)
        n = length(strings)
    end if
    return n
end function

function gen_size(object t)
-- note: must be kept precisely in sync with gen_rec!
--        (relentlessly tested via estsize/actsize)
integer size = 0
    if t!=NULL then
        integer n_type = t[1]
        string node_type = tkNames[n_type]
        switch n_type do
            case tk_Sequence:
                size += gen_size(t[2])
                size += gen_size(t[3])
            case tk_assign:
                size += gen_size(t[3])+6
            case tk_Integer:
                size += 5
            case tk_Identifier:
                size += 6
            case tk_String:
                size += 5
            case tk_while:
                -- emit: @@:<condition><topjmp(@f)><body><tailjmp(@b)>@@:
                size += gen_size(t[2])+3
                integer body = gen_size(t[3])
                integer stail = iff(size+body+2>128?5:2)
                integer stop  = iff(body+stail >127?6:2)
                size += stop+body+stail
            case tk_lt:
            case tk_le:
            case tk_ne:
            case tk_eq:
            case tk_gt:
            case tk_ge:
                size += gen_size(t[2])
                size += gen_size(t[3])
                size += 10
            case tk_and:
            case tk_or:
                size += gen_size(t[2])
                size += gen_size(t[3])
                size += 15
            case tk_add:
            case tk_sub:
                size += gen_size(t[2])
                size += gen_size(t[3])
                size += 4
            case tk_mul:
                size += gen_size(t[2])
                size += gen_size(t[3])
                size += 5
            case tk_div:
            case tk_mod:
                size += gen_size(t[2])
                size += gen_size(t[3])
                size += 6
            case tk_putc:
            case tk_Printi:
            case tk_Prints:
                size += gen_size(t[2])
                size += 5
            case tk_if:
                size += gen_size(t[2])+3
                if t[3][1]!=tk_if then ?9/0 end if
                integer truesize = gen_size(t[3][2])
                integer falsesize = gen_size(t[3][3])
                integer elsejmp = iff(falsesize=0?0:iff(falsesize>127?5:2))
                integer mainjmp = iff(truesize+elsejmp>127?6:2)
                size += mainjmp+truesize+elsejmp+falsesize
            case tk_not:
                size += gen_size(t[2])
                size += 9
            case tk_neg:
                size += gen_size(t[2])
                size += 4
            else:
                ?9/0
        end switch
    end if
    return size
end function

procedure gen_rec(object t)
-- the recursive part of code_gen
    if t!=NULL then
        integer initsize = length(code)
        integer estsize = gen_size(t)   -- (test the gen_size function)
        integer n_type = t[1]
        string node_type = tkNames[n_type]
        switch n_type do
            case tk_Sequence:
                gen_rec(t[2])
                gen_rec(t[3])
            case tk_assign:
                integer n = var_idx(t[2])
                gen_rec(t[3])
                code &= {0o217,0o005,chain,1,n,0}   -- pop [i]
                chain = length(code)-3
            case tk_Integer:
                integer n = t[2]
                code &= 0o150&int_to_bytes(n)       -- push imm32
            case tk_while:
                -- emit: @@:<condition><topjmp(@f)><body><tailjmp(@b)>@@:
                integer looptop = length(code)
                gen_rec(t[2])
                code &= {0o130,                                 -- pop eax
                         0o205,0o300}                           -- test eax,eax
                integer bodysize = gen_size(t[3])
                -- can we use short jumps?
                -- disclaimer: size calcs are not heavily tested; if in
                --             doubt reduce 128/7 by 8, and if that works
                --             then yep, you just found a boundary case.
                integer stail = iff(length(code)+bodysize+4-looptop>128?5:2)
                integer offset = bodysize+stail
                integer stop  = iff(offset>127?6:2)
                if stop=2 then
                    code &= {0o164,offset}                      -- jz (short) end
                else
                    code &= {0o017,0o204}&int_to_bytes(offset)  -- jz (long) end
                end if
                gen_rec(t[3])
                offset = looptop-(length(code)+stail)
                if stail=2 then
                    code &= 0o353&offset                        -- jmp looptop (short)
                else
                    code &= 0o351&int_to_bytes(offset)          -- jmp looptop (long)
                end if
            case tk_lt:
            case tk_le:
            case tk_gt:
            case tk_ge:
            case tk_ne:
            case tk_eq:
                gen_rec(t[2])
                gen_rec(t[3])
                integer xrm
                if    n_type=tk_ne then xrm = 0o225 -- (#95)
                elsif n_type=tk_lt then xrm = 0o234 -- (#9C)
                elsif n_type=tk_ge then xrm = 0o235 -- (#9D)
                elsif n_type=tk_le then xrm = 0o236 -- (#9E)
                elsif n_type=tk_gt then xrm = 0o237 -- (#9F)
                else ?9/0
                end if
                code &= { 0o061,0o300,                          -- xor eax,eax
                          0o132,                                -- pop edx
                          0o131,                                -- pop ecx
                          0o071,0o321,                          -- cmp ecx,edx
                          0o017,xrm,0o300,                      -- setcc al
                          0o120}                                -- push eax
            case tk_or:
            case tk_and:
                gen_rec(t[2])
                gen_rec(t[3])
                integer op = find(n_type,{tk_or,0,0,tk_and})
                op *= 0o010
                code &= { 0o130,                                -- pop eax
                          0o131,                                -- pop ecx
                          0o205,0o300,                          -- test eax,eax
                          0o017,0o225,0o300,                    -- setne al
                          0o205,0o311,                          -- test ecx,ecx
                          0o017,0o225,0o301,                    -- setne cl
                          op,0o310,                             -- or/and al,cl
                          0o120}                                -- push eax
            case tk_add:
            case tk_sub:
                gen_rec(t[2])
                gen_rec(t[3])
                integer op = find(n_type,{tk_add,0,0,0,0,tk_sub})
                op = 0o001 + (op-1)*0o010
                code &= { 0o130,                                -- pop eax
                          op,0o004,0o044}                       -- add/or/and/sub [esp],eax
            case tk_mul:
                gen_rec(t[2])
                gen_rec(t[3])
                code &= { 0o131,                                -- pop ecx
                          0o130,                                -- pop eax
                          0o367,0o341,                          -- mul ecx
                          0o120}                                -- push eax
            case tk_div:
            case tk_mod:
                gen_rec(t[2])
                gen_rec(t[3])
                integer push = 0o120+(n_type=tk_mod)*2
                code &= { 0o131,                                -- pop ecx
                          0o130,                                -- pop eax
                          0o231,                                -- cdq (eax -> edx:eax)
                          0o367,0o371,                          -- idiv ecx
                          push}                                 -- push eax|edx
            case tk_Identifier:
                integer n = var_idx(t)
                code &= {0o377,0o065,chain,1,n,0}               -- push [n]
                chain = length(code)-3
            case tk_putc:
            case tk_Printi:
            case tk_Prints:
                gen_rec(t[2])
                integer n = find(n_type,{tk_putc,tk_Printi,tk_Prints})
                code &= {0o350,chain,3,n,0}                     -- call :printc/i/s
                chain = length(code)-3
            case tk_String:
                integer n = string_idx(t)
                code &= {0o150,chain,2,n,0}                     -- push RawStringPtr(string)
                chain = length(code)-3
            case tk_if:
                -- emit: <condition><mainjmp><truepart>[<elsejmp><falsepart>]
                gen_rec(t[2])
                code &= {0o130,                                 -- pop eax
                         0o205,0o300}                           -- test eax,eax
                if t[3][1]!=tk_if then ?9/0 end if
                integer truesize = gen_size(t[3][2])
                integer falsesize = gen_size(t[3][3])
                integer elsejmp = iff(falsesize=0?0:iff(falsesize>127?5:2))
                integer offset = truesize+elsejmp
                integer mainjmp = iff(offset>127?6:2)
                if mainjmp=2 then
                    code &= {0o164,offset}                      -- jz (short) else/end
                else
                    code &= {0o017,0o204}&int_to_bytes(offset)  -- jz (long) else/end
                end if
                gen_rec(t[3][2])
                if falsesize!=0 then
                    offset = falsesize
                    if elsejmp=2 then
                        code &= 0o353&offset                    -- jmp end if (short)
                    else
                        code &= 0o351&int_to_bytes(offset)      -- jmp end if (long)
                    end if
                    gen_rec(t[3][3])
                end if
            case tk_not:
                gen_rec(t[2])
                code &= {0o132,                                 -- pop edx
                         0o061,0o300,                           -- xor eax,eax
                         0o205,0o322,                           -- test edx,edx
                         0o017,0o224,0o300,                     -- setz al
                         0o120}                                 -- push eax
            case tk_neg:
                gen_rec(t[2])
                code &= {0o130,                             -- pop eax
                         0o367,0o330,                       -- neg eax
                         0o120}                             -- push eax
            else:
                error("error in code generator - found %d, expecting operator\n", {n_type})
        end switch
        integer actsize = length(code)
        if initsize+estsize!=actsize then ?"9/0" end if -- (test gen_size)
    end if
end procedure

global procedure code_gen(object t)
--
-- Generates proper machine code.
--
-- Example: i=10; print "\n"; print i; print "\n"
-- Result in vars, strings, chain, code (declared above)
--    where vars is: {"i"},
--          strings is {"\n"},
--          code is { 0o150,#0A,#00,#00,#00,        -- 1: push 10
--                    0o217,0o005,0,1,1,0           -- 6: pop [i]
--                    0o150,8,2,1,0,                -- 12: push ("\n")
--                    0o350,13,3,3,0,               -- 17: call :prints
--                    0o377,0o065,18,1,1,0,         -- 22: push [i]
--                    0o350,24,3,2,0,               -- 28: call :printi
--                    0o150,29,2,1,0,               -- 33: push ("\n")
--                    0o350,34,3,3,0,               -- 38: call :prints
--                    0o303}                        -- 43: ret
--          and chain is 39 (->34->29->24->18->13->8->0)
-- The chain connects all places where we need an actual address before
--  the code is executed, with the byte after the link differentiating
--  between var(1), string(2), and builtin(3), and the byte after that
--  determining the instance of the given type - not that any of them 
--  are actually limited to a byte in the above intermediate form, and
--  of course the trailing 0 of each {link,type,id,0} is just there to
--  reserve the space we will need.
--
    gen_rec(t)
    code = append(code,0o303)   -- ret (0o303=#C3)
end procedure

include builtins/VM/puts1.e -- low-level console i/o routines

function setbuiltins()
atom printc,printi,prints
    #ilASM{ 
        jmp :setbuiltins
    ::printc
        lea edi,[esp+4]
        mov esi,1
        call :%puts1ediesi  -- (edi=raw text, esi=length)
        ret 4
    ::printi
        mov eax,[esp+4]
        push 0              -- no cr
        call :%putsint      -- (nb limited to +/-9,999,999,999)
        ret 4
    ::prints
        mov edi,[esp+4]
        mov esi,[edi-12]
        call :%puts1ediesi  -- (edi=raw text, esi=length)
        ret 4
    ::setbuiltins
        mov eax,:printc
        lea edi,[printc]
        call :%pStoreMint
        mov eax,:printi
        lea edi,[printi]
        call :%pStoreMint
        mov eax,:prints
        lea edi,[prints]
        call :%pStoreMint
          }
    return {printc,printi,prints}
end function

global constant builtin_names = {"printc","printi","prints"}
global constant builtins = setbuiltins()

global atom var_mem, code_mem

function RawStringPtr(integer n)    -- (based on IupRawStringPtr from pGUI.e)
--
-- Returns a raw string pointer for s, somewhat like allocate_string(s), but using the existing memory.
-- NOTE: The return is only valid as long as the value passed as the parameter remains in existence.
--
atom res
    string s = strings[n]
    #ilASM{
            mov eax,[s]
            lea edi,[res]
            shl eax,2
            call :%pStoreMint
          }
    stringptrs[n] = res
    return res
end function

global procedure fixup()
    var_mem = allocate(length(vars)*4)
    mem_set(var_mem,0,length(vars)*4)
    code_mem = allocate(length(code))
    poke(code_mem,code)
    while chain!=0 do
        integer this = chain
        chain = code[this]
        integer ftype = code[this+1]
        integer id = code[this+2]
        switch ftype do
            case 1: -- vars
                poke4(code_mem+this-1,var_mem+(id-1)*4)
            case 2: -- strings
                poke4(code_mem+this-1,RawStringPtr(id))
            case 3: -- builtins
                poke4(code_mem+this-1,builtins[id]-(code_mem+this+3))
        end switch
    end while
end procedure

