DEAD
--
-- psleep.e
--
--  Phix implementation of sleep(). Note that, unlike RDS Eu/OpenEu, you can specify fractions of a second.
--
global procedure sleep(atom timeout)

    #ilASM{ mov eax,[timeout]
--  push ebp
--  mov eax,[edi]
--  mov edx,FltWrk
            pushd dword 1000
            cmp eax,h4
            jge :tryfloat
                fild [timeout]
                jmp @f
          tryfloat:
                fld qword[ebx+eax*4]
          @@:
            fimul dword[esp]
            fldcw [down53]
    fistp [FltWrk]
    fldcw [near53]
    invoke Sleep,[FltWrk]
    ret
    mov eax,[edi]                         ;#0040950F: 213007                     vu 01 80  1 8207      
    mov edx,4199560                       ;#00409511: 272 88144000               uv 04 00  1 8208      
    cmp eax,h4                            ;#00409516: 075 00000040               vu 00 01  1 8208      
    jge #00409523                         ;#0040951B: 175 06                     v  00 00  1 8209      
    mov [edx],eax                         ;#0040951D: 211002                     uv 00 05  1 8210      
    fild dword [edx]                      ;#0040951F: 333002                     np 00 04  3 8211      
    jmp #00409538                         ;#00409521: 353 15                     v  00 00  1 8214      
    shl eax,2                             ;#00409523: 301340 02                  u  01 01  1 8215      
    je #0040A52B                          ;#00409526: 017204 FF0F0000            v  00 00  1 8215      
    cmp byte [eax-1],#12                  ;#0040952C: 200170 FF 12               u  00 01  2 8218    *01*
    jne #0040A597                         ;#00409530: 017205 61100000            v  00 00  1 8219      
    fld qword [eax]                       ;#00409536: 335000                     np 00 01  1 8220      
    mov [#00401488], dword 1000           ;#00409538: 307005 88144000 E8030000   uv 00 00  1 8221      
    fimul dword [#00401488]               ;#00409542: 332015 88144000            np 00 00  6 8222      
    fldcw [#004014AA]                     ;#00409548: 331055 AA144000            np 00 00  8 8228      
    fistp dword [#00401488]               ;#0040954E: 333035 88144000            np 00 00  6 8236      
    fldcw [#004014A8]                     ;#00409554: 331055 A8144000            np 00 00  8 8242      
    push dword [#00401488]                ;#0040955A: 377065 88144000            np 00 00  2 8250      
    call [#004040EC]                      ;#00409560: 377025 EC404000            np 00 00  2 8252      
    ret                                   ;#00409566: 303                        np 00 00  2 8254      

    }
--end procedure
