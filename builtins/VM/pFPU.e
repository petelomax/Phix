--
-- pFPU.e
-- ======
--
--  Floating point precision control
--  implements :%down53, :%near53, :%trunc53, :%down64, :%near64, :%trunc64
--  Used by: pSubseN.e, pMath.e, pHeap.e, etc, etc
--
--  The Control Word rounding control is in bits 10..11:
--      00 = and_bits(ax,0xF3FF) = Round to nearest, or even if equidistant (this is the initialized state)
--      01 = 0x0400 = Round down (toward -infinity)
--      10 = 0x0800 = Round up (toward +infinity)
--      11 = 0x0C00 = Truncate (toward 0)
--
--  The Control Word precision control is in bits 8..9 (0x0300)
--      00 = and_bits(ax,0xFCFF) = 24 bits (REAL4)
--      01 = Not used
--      10 = 0x0200 = 53 bits (REAL8)
--      11 = 0x0300 = 64 bits (REAL10) (this is the initialized state)
--
-- Not strictly relevant, but while that web page is open, some interesting Status Word bits (after compare):
--      C3 = 0x400 = Z flag
--      C2 = 0x040 = P flag
--      C1 = 0x020 = ?
--      C0 = 0x010 = C flag
--
--without debug

-- fpu round/precision control: (these are treated as 16-bit words)
integer near53 = 0  -- usual/default setting for 32-bit
integer down53 = 0  -- for truncating f.p. idx, etc
integer trunc53 = 0 -- for poke etc
integer up53 = 0
integer near64 = 0  -- usual/default setting for 64-bit, unused in 32-bit
integer down64 = 0  -- used by (32-bit) opFloor, and 64-bit for idx, etc
integer trunc64 = 0 -- for poke etc
integer up64 = 0

#ilASM{ jmp :!opCallOnceYeNot
--/*
procedure :>initFPU(:>)
end procedure -- (for Edita/CtrlQ)
--*/
    :>initFPU
-------------
        fninit                      -- initialise FPU
    [32]
        sub esp,4
        fnstcw word[esp]            --                                                  -- 7F 03 (NB: le notation)
        and word[esp],0xF0FF        -- set rounding to nearest or even,                 -- 7F 00
        or word[esp],0x0200         -- and 53 bit precision as the default              -- 7F 02
        fldcw word[esp]
        mov ax,word[esp]
        add esp,4
    [64]
        sub rsp,8
        fnstcw word[rsp]            --                                                  -- 7F 03 (NB: le notation)
        and word[rsp],0xF0FF        -- set rounding to nearest or even,                 -- 7F 00
        or word[rsp],0x0200         -- and 53 bit precision                             -- 7F 02
        mov ax,word[rsp]
    []
        mov word[near53],ax         --                                                  -- 7F 02
        or ax,0x0600                -- round down, 53 bit precision                     -- 7F 06
        mov word[down53],ax
        or ax,0x0700                -- round down, 64 bit precision                     -- 7F 07
        mov word[down64],ax
        or ax,0x0E00                -- truncate, 64 bit precision
        mov word[trunc64],ax
        sub ax,0x0100               -- truncate, 53 bit precision
        mov word[trunc53],ax
    [64]
        mov ax,word[rsp]
        add rsp,8
        or ax,0x0300
        mov word[near64],ax                                                             -- 7F 03
        fldcw word[near64]
    []
        mov ax,word[down53]         --                                                  -- 7F 06
        xor ax,0x0C00
        mov word[up53],ax           --                                                  -- 7F 0A
        mov ax,word[down64]         --                                                  -- 7F 07
        xor ax,0x0C00
        mov word[up64],ax           --                                                  -- 7F 0B
        ret

--/*
procedure :%down53(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%down53
------------
        fldcw word[down53]
        ret

--/*
procedure :%up53(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%up53
----------
        fldcw word[up53]
        ret

--/*
procedure :%near53(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%near53
------------
        fldcw word[near53]
        ret

--/*
procedure :%trunc53(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%trunc53
------------
        fldcw word[trunc53]
        ret

--/*
procedure :%down64(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%down64
------------
        fldcw word[down64]
        ret

--/*
procedure :%up64(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%up64
------------
        fldcw word[up64]
        ret

--/*
procedure :%near64(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%near64
------------
        fldcw word[near64]
        ret

--/*
procedure :%trunc64(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%trunc64
------------
        fldcw word[trunc64]
        ret
      }
