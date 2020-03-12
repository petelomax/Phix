--
-- peekstr.e
-- =========
--
--  Implements peek_string(). This is automatically included when needed; 
--  there should be no need to manually include this file.
--
--  This is a temporary file, in that the point of putting this in a
--  totally separate file was to create a challenge for conversion to
--  an opcode/#ilasm construct thing. Of course that may lead to this 
--  file being deleted, moved, or (ab)used for a different purpose.
--
-- Erm, there should probably be a peek_stringW version of this...
--
global function peek_string(atom addr)
integer ch
string res
--sequence res
    res = ""
    if addr!=NULL then
        while 1 do
--/*
--BOOL WINAPI IsBadReadPtr(
--  _In_    const VOID *lp,
--  _In_    UINT_PTR ucb
--);
        integer r9
        #ilASM{
            mov eax,[addr]
            call :%pLoadMint   
            push 1
            push eax
            call "kernel32.dll", "IsBadReadPtr"
            mov [r9],eax
              }
            if r9!=0 then ?9/0 end if
--*/    
            ch = peek(addr)
            if ch=0 then exit end if
            res &= ch
            addr += 1
        end while
    end if
    return res
end function

-- A challenge exists to recode this something like:
--  #ilASM{%%opPeekStr::
--          -- implements res=peek_str(addr)
--          -- NB: calling convention (as hard-coded in pilx86.e):
--          --  <opUnassigned addr if rqd>
--          --  mov e?x,[addr]
--          --  leamov e?x,res ; (var address)
--          --  call opPeekStr
--          ...
--        }
--  (peek_string was chosen as it seemed relatively straightforward, 
--   boy how wrong can one person be... ;-)

-- Some asm snippets for finding the length (totally untested, would need to time them all anyway)

--peek_string
--===========
--     #57,                        -- push   edi
--     #8B, #7C, #24, #08,         -- mov    edi, [esp+8]
--     #B9, #FF, #FF, #FF, #FF,    -- mov    ecx, #FFFFFFFF
--     #B0, #00,                   -- mov    al, 0
--     #FC,                        -- cld
--     #F2, #AE,                   -- repne  scasb
--     #B8, #FE, #FF, #FF, #FF,    -- mov    eax, #FFFFFFFE
--     #29, #C8,                   -- sub    eax, ecx
--     #5F,                        -- pop    edi
--     #C2, #04, #00               -- ret    4    -- pop 4 bytes off the 

--proc strlen,pointer 
--  push ebx 
--  mov eax,[pointer]           ; get pointer s 
--  lea edx,[eax+3]             ; pointer+3 used in the end 
--  l1:
--  mov ebx,[eax]               ; read 4 bytes of string 
--  add eax,4                   ; increment pointer 
--  lea ecx,[ebx-0x01010101]    ; subtract 1 from each byte 
--  not ebx                     ; invert all bytes 
--  and ecx,ebx                 ; and these two 
--  and ecx,0x80808080          ; test all sign bits 
--  jz l1                       ; no zero bytes, continue loop 
--  mov ebx,ecx 
--  shr ebx,16 
--  test ecx,0x00008080         ; test first two bytes 
--  cmovz ecx,ebx               ; shift if not in first 2 bytes 
--  lea ebx,[eax+2]             ; .. and increment pointer by 2 
--  cmovz eax,ebx 
--  add cl,cl                   ; test first byte 
--  sbb eax,edx                 ; compute length 
--  pop ebx 
--  ret 
--endp
----
----;edx=string start
--  lea ecx,[edx+4]             ; load and increment pointer
--  mov ebx,[edx]               ; read first 4 bytes
--  lea edx,[edx+7]             ; pointer+7 used in the end
--  ._1:
--  lea eax,[ebx-0x01010101]    ; subtract 1 from each byte
--  xor ebx,-1                  ; invert all bytes
--  and eax,ebx                 ; and these two
--  mov ebx,[ecx]               ; read next 4 bytes
--  add ecx,4                   ; increment pointer
--  and eax,0x80808080          ; test all sign bits
--  jz ._1                      ; no zero bytes, continue loop
--  test eax,0x00008080         ; test first two bytes
--  jnz ._2
--  shr eax,16                  ; not in the first 2 bytes
--  add ecx,2
--  ._2:
--  shl al,1                    ; use carry flag to avoid a branch
--  sbb ecx,edx                 ; compute length
--  lea edx,[edx-7]
--
--
--??:
--use32
--strlen: ;uses the 'C' calling convention the pointer is pushed on the stack
--
--  ;registers are saved and result is returned in eax
--  push ecx
--  push esi
--  mov esi,[esp+4+8]
--  mov eax,[esi]
--  and esi,-4
--  .l1:
--  add esi,4
--  lea ecx,[eax-1010101h]
--  not eax
--  and ecx,eax
--  mov eax,[esi]
--  and ecx,80808080h
--  jz .l1
--  bsf eax,ecx
--  sub esi,[esp+4+8]
--  sar eax,3
--  lea eax,[eax+esi-4]
--  pop esi
--  pop ecx
--  ret
--
--

-- ...and here is a copy of opAlloc, as an example/reference:
--
--  Notes: emapedx4 would be inlined, search for "fld qword" in pcfunc.e
--          for examples, and we may need to have a table of addresses
--          (etc) so that pdiag.e can handle eg e30ume->e94 properly.
--         locations of down53 & near53 could easily be added to VMep,
--          as could HeapAlloc, hHeap, FltWrk, not that we are actually
--          translating this here. However if they could be pure hll, 
--          then so much the better.
--         e37atambpi to be handled as per prntf.e (snippet below.)
--
--emapedx4: ; exception here mapped to e92vhnbaavEmapToEsp4feh
--  cmp byte[edx-1],0x12
--  jne @b
--  fld qword[edx]
--  mov edx,FltWrk
--  fldcw [down53]
--  fistp qword[edx]
--  fldcw [near53]
--  mov edx,[edx]
--  ret
--
--opAlloc:              ; [edi]=allocate([esi])
--;-------
--  trc opAlloc
--  mov edx,[esi]
--  cmp edx,h4
--  jl @f
--      shl edx,2
--      mov [emapTo],esi
--      call emapedx4
--      jne e37atambpi  ; argument to allocate must be positive integer
--  @@:
--
--  test edx,edx
--  js e37atambpi       ; argument to allocate must be positive integer
--  push edi
--  invoke HeapAlloc, [hHeap], 0, edx
--; ; just quietly return 0 on failure, when not in debug mode
--if debug
--  test eax,eax
--  jz e77phroomopAlloc
--end if
--  mov ecx,eax
--  pop edi                 ; target addr
--  shl ecx,1
--  jno @f
--      mov [FltWrk],eax
--      fild [FltWrk]
--      jmp StoreFlt        ;; store result (invokes dealloc if needed)
--  @@:
--  mov edx,[edi]
--  xor ebx,ebx
--  cmp edx,h4
--  mov [edi],eax
--  jle @f
--      sub dword[ebx+edx*4-8],1
--      jz deallocX
--  @@:
--  ret
--
---- and (from prntf.e):
----/**/                #ilasm{ mov al,70
----/**/                    --  xor edi,edi     -- ep1 unused
----/**/                    --  xor esi,esi     -- ep2 unused
----/**/                        call :%pRTErn}  -- fatal error (pUnassigned/dev/tmp)
--

-- ...and here is a copy of opPeek:
--      Notes:  it is peek(object) but peek_string(atom), remember.
--              hence plenty of validation here you just don't need.
--              AllocStr could easily be added to VMep.

--opPeek:
--;------
--  trc opPeek
--;calling convention:                              octal:         binary:          code:
--;  mov ecx,p1     ; target addr                   271         B9 imm32        mov ecx,imm32
--;  mov edx,p2     ; addr or {addr,len}            272         BA imm32        mov edx,imm32
--  mov edi,[edx]
--  xor eax,eax
--  cmp edi,h4
--  jl @f
--      shl edi,2
--      mov [emapTo],edx
--      call emapedi4
--;emapedi4:    ; exception here mapped to e92vhnbaavEmapToEsp4feh
--; cmp byte[edi-1],0x12
--; jne PeekSeq
--; fld qword[edi]
--; mov edi,FltWrk
--; fldcw [down53]
--; fistp qword[edi]
--; fldcw [near53]
--; mov edi,[edi]
--      jne PeekSeq
--  @@:
--  mov edx,[ecx]
--  xor ebx,ebx
--  opPeekMLE:                  ; exception here mapped to e99ipmaespfeh (invalid peek memory address)
--  mov al,[edi]
--  cmp edx,h4
--  mov [ecx],eax
--  jle @f
--      sub dword[ebx+edx*4-8],1
--      jz deallocX
--  @@:
--  ret
--
--  PeekSeq:                    ; peek({addr,len}) case
--  ; edi is raw {addr,len}, ecx is tgt addr
--  cmp byte[edi-1],0x80        ; sequence:
--  jnz e43atpmbaoso2a          ; argument to peek must be atom or sequence of 2 atoms
--  mov eax,[edi-12]            ; length
--if newBase
--else
--  mov edi,[edi-20]
--end if
--  cmp eax,2                   ; of length 2:
--  jne e43atpmbaoso2a          ; argument to peek must be atom or sequence of 2 atoms
--  mov edx,[edi+4]
--  cmp edx,h4
--  jle @f
--      lea esi,[edi+4]
--      shl edx,2
--      mov [emapTo],esi
--      call emapedx4
--      jne e43atpmbaoso2a      ; argument to peek must be atom or sequence of 2 atoms
--  @@:
--  mov esi,[edi]
--  push ecx                    ; save tgt addr
--  cmp esi,h4
--  jle @f
--      shl esi,2
--      mov [emapTo],edi
--      call emapesi8
--      jne e43atpmbaoso2ap     ; argument to peek must be atom or sequence of 2 atoms
--  @@:
--  ; OK, len now in edx, and addr in esi
--  xor ebx,ebx
--  call AllocStr           ; damages eax only
--  mov ecx,edx
--  lea edi,[ebx+eax*4]
--  opPeeksMLE:                 ; exception here mapped to e99ipmaespp4feh (invalid peek memory address)
--  rep movsb
--  pop esi
--  mov byte[edi],0
--  mov edx,[esi]
--  mov [esi],eax
--  cmp edx,h4
--  jle @f
--      sub dword[ebx+edx*4-8],1
--      jz deallocX
--  @@:
--  ret
--

--/* old:
global function peek_wstring(atom addr)
atom ptr
    ptr = addr

    while peek2u(ptr) do
        ptr += 2
    end while

    return peek2u({addr, (ptr-addr)/2})
end function
--*/

--!/!* or maybe...
global function peek_wstring(atom addr)
    sequence res = {}
    if addr!=NULL then
        while 1 do
            integer wch = peek2u(addr)
            if wch=0 then exit end if
            res &= wch
            addr += 2
        end while
    end if
    return res
end function
--!*!/
