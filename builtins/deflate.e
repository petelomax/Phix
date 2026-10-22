--
-- builtins\deflate.e
-- ==================
--
--  Plain memory buffer deflate (compress).
--  See inflate.e for the uncompress routine.
--
--  Requires deflate.(32|64).dll to be installed/distributed.
--  See builtins\LiteZip for C source and gcc build script.
--
local function find_dll(string dll_name)
    -- until someone shows me how they fixed it:
    if platform()!=WINDOWS then ?9/0 end if
    atom res = open_dll(dll_name,false)
    if res=NULL then
        -- try to load from \builtins
        sequence s = include_paths()
        for i=1 to length(s) do
            if match("builtins",s[i]) then
                res = open_dll(join_path({s[i],dll_name}),false)
                if res!=NULL then exit end if
            end if
        end for
        if res=NULL then ?9/0 end if
    end if
    return res
end function

atom x_deflate = NULL, pSize

global function deflate(object src, integer s=1, e=-1)
    if e=-1 then e = length(src) end if
    integer slen = e-s+1, res,
            dlen = slen+ceil(slen/4096)+11
    string dst = repeat(' ',dlen)
    if s!=1 then
        assert(string(src))
        -- res := (integer) rad_addr/4:
        #ilASM{
            [32]
                mov eax,[src]
                shl eax,2
                shr eax,2
                mov [res],eax
            [64]
                mov rax,[src]
                shl rax,2
                shr rax,2
                mov [res],rax
            []
        }
        src = res*4+s-1
    end if
    if x_deflate=NULL then
        -- needs tweaking for linux, and above ?9/0 removing:
        atom lib = find_dll(sprintf("deflate.%d.dll",machine_bits()))
        x_deflate = define_c_func(lib,"simple_deflate",{C_PTR,C_PTR,C_INT,C_INT,C_PTR},C_INT)
        pSize = allocate(machine_word())
    end if
    res = c_func(x_deflate,{src,dst,slen,dlen,pSize})
    if res!=0 then return res end if
    integer cs = peekns(pSize)
    assert(cs>0 and cs<=dlen)
    dst = dst[1..cs]
    return dst
end function

