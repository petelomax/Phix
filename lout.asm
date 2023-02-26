    add [edi+eax],edx                     ;#0055486B: 001024007                  uv 00 85  3  65      
    add [ebx+edi*4-8],ecx                 ;#0055197A: 001114273 F8               uv 00 8A  3 380      
    add ecx,eax                           ;#005506D1: 001301                     uv 02 03  1 243      
    add edi,eax                           ;#0055243D: 001307                     uv 80 81  1 115      
    add eax,ecx                           ;#005513EB: 001310                     uv 01 03  1 455      
    add ecx,ecx                           ;#00554EA4: 001311                     vu 02 02  1 272      
    add esi,ecx                           ;#0052D106: 001316                     uv 40 42  1  18      
    add eax,edx                           ;#00551FDA: 001320                     uv 01 05  1 136      
    add ecx,edx                           ;#00551848: 001321                     uv 02 06  1 275      
    add edx,edx                           ;#00555053: 001322                     uv 04 04  1 548      
    add esi,edx                           ;#005520D4: 001326                     uv 40 44  1 214      
    add edi,edx                           ;#005522A4: 001327                     uv 80 84  1 146      
    add ecx,esi                           ;#00553C3C: 001361                     vu 02 42  1 127      
    add edx,esi                           ;#00551A53: 001362                     uv 04 44  1 449      
    add edi,esi                           ;#00551943: 001367                     uv 80 C0  1 351      
    add eax,edi                           ;#005327C4: 001370                     uv 01 81  1 421      
    add ecx,edi                           ;#00551CD8: 001371                     vu 02 82  1 224      
    add edx,edi                           ;#005519FC: 001372                     uv 04 84  1 419      
    add esi,edi                           ;#00550F53: 001376                     uv 40 C0  1  68      
    add edx,[esp+8]                       ;#005519A3: 003124044 08               vu 04 14  2 388      
    add edx,[ebx+eax*4-12]                ;#005514CB: 003124203 F4               uv 04 0D  2 533      
    add edx,[ebx+edi*4-12]                ;#00551A1B: 003124273 F4               uv 04 8C  2 435      
    add esi,[esp+4]                       ;#00551AC2: 003164044 04               uv 40 50  2 498      
    or ah,ch                              ;#00550BA8: 010354                     vu 01 03  1  42      
    or [ebx+esi*4-4],eax                  ;#00554C1B: 011104263 FC               uv 00 49  3  17      
    or ecx,eax                            ;#005508ED: 011301                     uv 02 03  1 747      
    or ecx,ecx                            ;#00550C10: 011311                     uv 02 02  1  95      
    or ecx,[esp]                          ;#005508FC: 013014044                  uv 02 12  2 766      
    jo #00555521 (:%pStoreFlt)            ;#00550A97: 017200 844A0000            v  00 00  1 146      
    jno #00515B5B                         ;#00515A90: 017201 C5000000            v  00 00  1  51      
    jb #00555521 (:%pStoreFlt)            ;#00555956: 017202 C5FBFFFF            v  00 00  1 112      
    jae #005521DB                         ;#005522B8: 017203 1DFFFFFF            v  00 00  1 151      
    je #004DC6FB                          ;#004DC667: 017204 8E000000            v  00 00  1  39      
    jne #0052CC30                         ;#0052CB93: 017205 97000000            v  00 00  1  23      
    ja #0055223D                          ;#0055230E: 017207 29FFFFFF            v  00 00  1 180      
    js #00554F7B                          ;#00555308: 017210 6DFCFFFF            v  00 00  1 797      
    jl #004DCAB5                          ;#004DC990: 017214 1F010000            v  00 00  1 232      
    jge #005005C0                         ;#0050050A: 017215 B0000000            v  00 00  1   8      
    jle #004DCAB5                         ;#004DC7C3: 017216 EC020000            v  00 00  1 119      
    jg #004D3DD1                          ;#004D3BF8: 017217 D3010000            v  00 00  1 110      
    sete al                               ;#0052408A: 017224300                  np 01 00  2  11      
    sete cl                               ;#005323ED: 017224301                  np 02 00  2 220      
    sete dl                               ;#00552A12: 017224302                  np 04 00  2  13      
    setne al                              ;#00552952: 017225300                  np 01 00  2   6      
    setne bl                              ;#005548C3: 017225303                  np 08 00  2 113      
    setl al                               ;#0050C8AB: 017234300                  np 01 00  2 272      
    setl cl                               ;#00553216: 017234301                  np 02 00  2 197      
    setge al                              ;#00552CB3: 017235300                  np 01 00  2  10      
    setg al                               ;#00552DD2: 017237300                  np 01 00  2  10      
    setg cl                               ;#0052D283: 017237301                  np 02 00  2  79      
    shld edx,eax,4                        ;#00554529: 017244302 04               np 05 05  4  60      
    shld eax,edx,4                        ;#005544DE: 017244320 04               np 05 05  4  33      
    shld ecx,edx,4                        ;#00554525: 017244321 04               np 06 06  4  56      
    and ecx,eax                           ;#005508BA: 041301                     uv 02 03  1 723      
    and eax,esi                           ;#0052EE17: 041360                     uv 01 41  1  16      
    and ecx,[esp]                         ;#005508C9: 043014044                  uv 02 12  2 742      
    and eax,1                             ;#0052FC12: 045 01000000               uv 01 01  1  23 01   
    sub ah,dh                             ;#00550BAA: 050364                     uv 01 05  1  43      
    sub ecx,eax                           ;#00553D1C: 051301                     uv 02 03  1 169 02   
    sub edx,eax                           ;#0055DD7E: 051302                     uv 04 05  1  67      
    sub edi,eax                           ;#00555356: 051307                     uv 80 81  1 825 80   
    sub eax,ecx                           ;#00554E4F: 051310                     uv 01 03  1 237      
    sub edx,ecx                           ;#005552F2: 051312                     uv 04 06  1 779      
    sub esi,ecx                           ;#005507E7: 051316                     uv 40 42  1 533 40   
    sub esi,edx                           ;#005513E9: 051326                     vu 40 44  1 454      
    sub edi,edx                           ;#005521D6: 051327                     uv 80 84  1   3      
    sub ecx,esp                           ;#005545BE: 051341                     uv 02 12  1 182      
    sub eax,esi                           ;#00554E22: 051360                     vu 01 41  1 170      
    sub ecx,esi                           ;#005514FC: 051361                     vu 02 42  1 543      
    sub edi,esi                           ;#0050CAEF: 051367                     uv 80 C0  1 375      
    sub eax,edi                           ;#00531A8D: 051370                     uv 01 81  1 144       
    sub ecx,edi                           ;#00552121: 051371                     uv 02 82  1 247      
    sub edx,edi                           ;#00551102: 051372                     vu 04 84  1 194      
    sub ecx,[esp]                         ;#00551F4D: 053014044                  uv 02 12  2  93      
    sub eax,[edx-20]                      ;#005510DC: 053102 EC                  vu 01 05  2 186      
    sub ecx,[esp+16]                      ;#00551A66: 053114044 10               uv 02 12  2 463      
    sub edx,[esp+4]                       ;#00551A1F: 053124044 04               uv 04 14  2 437 04   
    sub edx,[esi-4]                       ;#0055485D: 053126 FC                  uv 04 44  2  59 04   
    sub eax,256                           ;#00553A7F: 055 00010000               uv 01 01  1  54      
    xor eax,eax                           ;#004D6CA1: 061300                     uv 01 01  1  44      
    xor ecx,eax                           ;#00550920: 061301                     uv 02 03  1 771      
    xor ecx,ecx                           ;#0052C6F0: 061311                     uv 02 02  1  15      
    xor edx,edx                           ;#00552A0E: 061322                     vu 04 04  1  11      
    xor ebx,ebx                           ;#004D6E53: 061333                     vu 08 08  1  18      
    xor ebp,ebp                           ;#0055498D: 061355                     uv 20 20  1 192      
    xor eax,esi                           ;#00550517: 061360                     uv 01 41  1  51      
    xor esi,esi                           ;#00555833: 061366                     vu 40 40  1 147       
    xor esi,edi                           ;#00554E58: 061376                     uv 40 C0  1 241 80   
    xor edi,edi                           ;#004DACFD: 061377                     uv 80 80  1  51      
    xor ecx,[esp]                         ;#0055092F: 063014044                  uv 02 12  2 790      
    cmp [esi+edi],cl                      ;#00551BEE: 070014076                  uv 00 C2  2 143      
    cmp [edi],eax                         ;#00550C06: 071007                     uv 00 81  2  89      
    cmp [eax],ebx                         ;#005549A1: 071030                     uv 00 09  2 200 01 *01*
    cmp [#00404A18] (ma_ip),ebx           ;#00552431: 071035 184A4000            vu 00 08  2 113      
    cmp [ebx+esi*4+4],eax                 ;#00554CE9: 071104263 04               uv 00 49  2  47      
    cmp [eax-12],ecx                      ;#0055173C: 071110 F4                  uv 00 03  2 157      
    cmp [ebx+edi*4-12],ecx                ;#00551953: 071114273 F4               uv 00 8A  2 362      
    cmp [esi+8],edx                       ;#00555115: 071126 08                  uv 00 44  2 629      
    cmp [edi+ecx*4+20],ebx                ;#00555318: 071134217 14               vu 00 8A  2 799      
    cmp [ebp+16],ebx                      ;#005548BD: 071135 10                  vu 00 28  2 111      
    cmp [esi+84],ebx                      ;#00554A8C: 071136 54                  uv 00 48  2 304      
    cmp ecx,eax                           ;#00515B89: 071301                     uv 00 03  1 107 02   
    cmp esi,eax                           ;#00500508: 071306                     uv 00 41  1   8      
    cmp edi,eax                           ;#004D3AB9: 071307                     uv 00 81  1  51      
    cmp eax,ecx                           ;#005157C1: 071310                     uv 00 03  1  82 01   
    cmp edx,ecx                           ;#0055502B: 071312                     vu 00 06  1 539      
    cmp esi,ecx                           ;#004E2A03: 071316                     uv 00 42  1  39      
    cmp edi,ecx                           ;#004D3B96: 071317                     uv 00 82  1  93      
    cmp eax,edx                           ;#005558E7: 071320                     uv 00 05  1  52      
    cmp ecx,edx                           ;#004D6885: 071321                     uv 00 06  1  83      
    cmp esi,edx                           ;#00550858: 071326                     uv 00 44  1 681      
    cmp edi,edx                           ;#005523BE: 071327                     uv 00 84  1  57      
    cmp eax,esi                           ;#00532FFE: 071360                     uv 00 41  1 818 40   
    cmp ecx,esi                           ;#004E1A39: 071361                     uv 00 42  1  56 02   
    cmp edi,esi                           ;#004D3911: 071367                     uv 00 C0  1  80      
    cmp eax,edi                           ;#004D3BF6: 071370                     uv 00 81  1 110      
    cmp ecx,edi                           ;#004E29F0: 071371                     uv 00 82  1  33 02   
    cmp esi,edi                           ;#0053387B: 071376                     uv 00 C0  1  31      
    cmp eax,[esi]                         ;#00550B87: 073006                     uv 00 41  2  26      
    cmp eax,[edi]                         ;#00550BE2: 073007                     uv 00 81  2  72      
    cmp ecx,[eax]                         ;#00554E6D: 073010                     uv 00 03  2 258      
    cmp edx,[esp]                         ;#005552FB: 073024044                  uv 00 14  2 794      
    cmp edi,[esp]                         ;#00550EBE: 073074044                  uv 00 90  2  30      
    cmp eax,[ebp+24]                      ;#005549AC: 073105 18                  uv 00 21  2 206    *20*
    cmp eax,[esi+4]                       ;#005553A8: 073106 04                  vu 00 41  2 893      
    cmp ecx,[esi+8]                       ;#00554DBD: 073116 08                  uv 00 42  2 138      
    cmp ecx,[edi-12]                      ;#00550CBF: 073117 F4                  uv 00 82  2 173      
    cmp edx,[esp+4]                       ;#00551265: 073124044 04               uv 00 14  2 305      
    cmp edi,[esp+4]                       ;#00551035: 073174044 04               uv 00 90  2 136      
    cmp eax,ecx                           ;#005155F1: 073301                     uv 00 03  1 206 01   
    cmp eax,edx                           ;#00515F9E: 073302                     uv 00 05  1 327 01   
    cmp eax,esi                           ;#00533402: 073306                     uv 00 41  1 119 01   
    cmp eax,edi                           ;#005529CA: 073307                     uv 00 81  1  28 01   
    cmp ecx,eax                           ;#00553C99: 073310                     uv 00 03  1 145 02   
    cmp ecx,edx                           ;#005164EF: 073312                     uv 00 06  1 615 02   
    cmp ecx,esi                           ;#00517ACA: 073316                     uv 00 42  1 115 02   
    cmp ecx,edi                           ;#00552D8C: 073317                     uv 00 82  1  39 02   
    cmp esi,eax                           ;#00532600: 073360                     uv 00 41  1 323 40   
    cmp esi,ecx                           ;#004E17B0: 073361                     uv 00 42  1  64 40   
    cmp esi,edx                           ;#0055DC10: 073362                     uv 00 44  1 109      
    cmp esi,edi                           ;#004E24C0: 073367                     uv 00 C0  1  43 40   
    cmp edi,eax                           ;#00515B64: 073370                     uv 00 81  1  95 80   
    cmp edi,ecx                           ;#004D3988: 073371                     uv 00 82  1 103 80   
    cmp edi,edx                           ;#0055DB88: 073372                     uv 00 84  1  77 80   
    cmp edi,esi                           ;#0052C8B1: 073376                     uv 00 C0  1  93 80   
    cmp eax,8384                          ;#004D6C11: 075 C0200000               vu 00 01  1  16      
    push eax                              ;#004D6C30: 120                        uv 00 01  1  24      
    push ecx                              ;#004D6B31: 121                        uv 00 02  1  86      
    push edx                              ;#004E668C: 122                        uv 00 04  1  53      
    push ebx                              ;#005551D0: 123                        uv 00 08  1 677      
    push esp                              ;#005544AD: 124                        vu 00 10  1  12      
    push ebp                              ;#005549A6: 125                        vu 00 20  1 202      
    push esi                              ;#004D6B02: 126                        uv 00 40  1  76      
    push edi                              ;#004D6B18: 127                        uv 00 80  1  81      
    pop eax                               ;#005559DD: 130                        uv 01 00  1 445      
    pop ecx                               ;#005553A7: 131                        uv 02 00  1 893      
    pop edx                               ;#005559B2: 132                        uv 04 00  1 374      
    pop ebp                               ;#005549E0: 135                        uv 20 00  1 228      
    pop esi                               ;#0055519E: 136                        uv 40 00  1 665      
    pop edi                               ;#0055519F: 137                        vu 80 00  1 665      
    pushad                                ;#00555417: 140                        np 00 FF  5 958      
    popad                                 ;#0055545A: 141                        np FF 00  5 986      
    or ax,1536                            ;#00553DAF: 146:015 0006               vu 01 01  1  31      
    sub ax,256                            ;#00553DCD: 146:055 0001               vu 01 01  1  34      
    xor ax,3072                           ;#00553DDD: 146:065 000C               uv 01 01  1  36      
    or word[esp],512                      ;#00553D99: 146:201014044 0002         vu 00 10  3  21      
    and word[esp],-3841                   ;#00553D93: 146:201044044 FFF0         uv 00 10  3  19      
    mov word[esi+edx],ax                  ;#00550ED6: 146:211004026              vu 00 45  1  35      
    mov word[ebx+eax*4],cx                ;#00550F64: 146:211014203              vu 00 0B  1  73      
    mov word[edi],cx                      ;#00550FA9: 146:211017                 uv 00 82  1  95      
    mov ax,word[esp]                      ;#00553DA2: 146:213004044              uv 01 10  1  30      
    mov ax,word[#00402218] (down53)       ;#00553DD7: 146:241 18224000           vu 01 00  1  35      
    mov word[#00402214] (near53),ax       ;#00553DA9: 146:243 14224000           uv 00 01  1  31      
    lodsw                                 ;#0055487D: 146:255                    np 41 40  2  84      
    mov word[edi], word 3338              ;#005544F6: 146:307007 0A0D            uv 00 80  1  44      
    push #004D69E0                        ;#004D69C9: 150 E0694D00               uv 00 00  1   2      
    push 43                               ;#0052CBAA: 152 2B                     uv 00 00  1  28      
    jo #004D6889                          ;#004D6892: 160 F5                     v  00 00  1  86      
    jno #0052C7BF                         ;#0052C7AC: 161 11                     v  00 00  1  46      
    jb #0055501E                          ;#0055501B: 162 01                     v  00 00  1 524      
    jnc #0055596A                         ;#005559E0: 163 88                     v  00 00  1 446      
    je #004D69FC                          ;#004D69F0: 164 0A                     v  00 00  1  10      
    jne #004D6BE0                         ;#004D6BCF: 165 0F                     v  00 00  1   5      
    jbe #005558E7                         ;#005558DB: 166 0A                     v  00 00  1  49      
    ja #005558B8                          ;#00555897: 167 1F                     v  00 00  1  29      
    js #00554959                          ;#00554936: 170 21                     v  00 00  1 151      
    jns #00555417                         ;#00555414: 171 01                     v  00 00  1 944      
    jpe #0055044A                         ;#0055044F: 172 F9                     v  00 00  1 187      
    jl #004D6C16                          ;#004D6C0A: 174 0A                     v  00 00  1  13      
    jge #0050C91B                         ;#0050C912: 175 07                     v  00 00  1 288      
    jle #004D6C49                         ;#004D6C39: 176 0E                     v  00 00  1  25      
    jg #00520A82                          ;#00520A02: 177 7E                     v  00 00  1  59      
    cmp byte[eax-1],#82                   ;#00552412: 200170 FF 82               u  00 01  2  93      
    cmp byte[ecx-1],#82                   ;#0055125B: 200171 FF 82               u  00 02  2 303      
    cmp byte[edx-1],#80                   ;#00555665: 200172 FF 80               u  00 04  2 129       
    cmp byte[ebx+eax*4-1],#82             ;#004DCAB8: 200174203 FF 82            u  00 09  2 309    *01*
    cmp byte[ebx+ecx*4-1],#12             ;#00552CC9: 200174213 FF 12            u  00 0A  2   3      
    cmp byte[ebx+edx*4-1],#12             ;#00555572: 200174223 FF 12            u  00 0C  2 116       
    cmp byte[ebx+esi*4-1],#12             ;#004D69EB: 200174263 FF 12            u  00 48  2   9      
    cmp byte[ebx+edi*4-1],#12             ;#004D6A07: 200174273 FF 12            u  00 88  2  14      
    cmp byte[esi-1],#80                   ;#00551B39: 200176 FF 80               u  00 40  2  84      
    cmp byte[edi-1],#12                   ;#0055227D: 200177 FF 12               u  00 80  2 130    *80*
    add al,55                             ;#005544E5: 200300 37                  vu 01 01  1  37      
    add cl,55                             ;#00554530: 200301 37                  vu 02 02  1  64      
    or al,16                              ;#005521D1: 200310 10                  uv 01 01  1   2      
    and ah,127                            ;#00550B9C: 200344 7F                  uv 01 01  1  40      
    and dh,4                              ;#00550B9F: 200346 04                  vu 04 04  1  40      
    sub al,7                              ;#005544ED: 200350 07                  uv 01 01  1  39      
    sub cl,1                              ;#0055549B: 200351 01                  uv 02 02  1 101       
    cmp al,4                              ;#00554866: 200370 04                  uv 00 01  1  64      
    cmp cl,#12                            ;#00550D4C: 200371 12                  uv 00 02  1 237      
    cmp dl,#82                            ;#00550CCD: 200372 82                  uv 00 04  1 177      
    cmp ah,#80                            ;#00552474: 200374 80                  uv 00 01  1 130      
    cmp dword[ebx+ecx*4],#00424354        ;#00555267: 201074213 54434200         vu 00 0A  2 742      
    cmp dword[ebx+edx*4],#00424354        ;#005553BF: 201074223 54434200         uv 00 0C  2 902      
    cmp dword[ebx+esi*4],#00424354        ;#00554CDF: 201074263 54434200         uv 00 48  2  32      
    cmp dword[esi],#00484253              ;#00554DB4: 201076 53424800            uv 00 40  2 123    *40*
    cmp dword[ecx+32],#00424345           ;#00555168: 201171 20 45434200         u  00 02  2 655      
    cmp dword[ebx+edx*4+236],#00424343    ;#005553D7: 201274223 EC000000 43434200 u  00 0C  2 918      
    cmp dword[ebx+esi*4+236],#00424343    ;#0055515A: 201274263 EC000000 43434200 u  00 48  2 640      
    cmp dword[ebx+edi*4+236],#00424343    ;#00554DDF: 201274273 EC000000 43434200 u  00 88  2 144      
    add ecx,2048                          ;#00554904: 201301 00080000            vu 02 02  1 139      
    add edx,240                           ;#00554D90: 201302 F0000000            vu 04 04  1 109      
    add esi,1024                          ;#004EA269: 201306 00040000            vu 40 40  1  71      
    add edi,1024                          ;#004F52B1: 201307 00040000            vu 80 80  1  71      
    and ecx,255                           ;#00552251: 201341 FF000000            vu 02 02  1 107      
    and edx,-4                            ;#0055110C: 201342 FCFFFFFF            uv 04 04  1 199      
    and esi,-2                            ;#00554DAA: 201346 FEFFFFFF            uv 40 40  1 120 40   
    sub edi,65536                         ;#00553AFC: 201357 00000100            uv 80 80  1  75      
    cmp ecx,h4                            ;#004D6C04: 201371 00000040            vu 00 02  1  12      
    cmp edx,h4                            ;#004D6CAB: 201372 00000040            uv 00 04  1  46 04   
    cmp esi,h4                            ;#004D6BC9: 201376 00000040            vu 00 40  1   4      
    cmp edi,h4                            ;#004D69FF: 201377 00000040            uv 00 80  1  13 80   
    cmp dword[#0040219C] (sinit),0        ;#0055481A: 203075 9C214000 00         u  00 00  2  50      
    add dword[eax-8],1                    ;#0055478A: 203100 F8 01               u  00 01  3  13    *01*
    add dword[ecx-8],1                    ;#0055122D: 203101 F8 01               u  00 02  3 286      
    add dword[ebx+eax*4-8],1              ;#004D6A4A: 203104203 F8 01            u  00 09  3  28    *01*
    add dword[ebx+ecx*4-8],1              ;#004D6C0C: 203104213 F8 01            u  00 0A  3  14      
    add dword[ebx+edx*4-8],1              ;#00551DA7: 203104223 F8 01            u  00 0C  3 297      
    add dword[ebx+esi*4-8],1              ;#004D6AF8: 203104263 F8 01            u  00 48  3  73      
    add dword[ebx+edi*4-8],1              ;#004D6C1F: 203104273 F8 01            u  00 88  3  20    *80*
    add dword[esi-8],1                    ;#00551248: 203106 F8 01               u  00 40  3 295      
    sub dword[ebx+eax*4-8],1              ;#004D6C3B: 203154203 F8 01            u  00 09  3  26      
    sub dword[ebx+ecx*4-8],1              ;#004D3343: 203154213 F8 01            u  00 0A  3 157      
    sub dword[ebx+edx*4-8],1              ;#004D6CB6: 203154223 F8 01            u  00 0C  3  48      
    sub dword[ebx+esi*4-8],1              ;#004D6A67: 203154263 F8 01            u  00 48  3  34      
    sub dword[eax+edi*4-8],1              ;#005556E9: 203154270 F8 01            u  00 81  3 133       
    sub dword[ebx+edi*4-8],1              ;#004D30DA: 203154273 F8 01            u  00 88  3  15      
    sub dword[esi-8],1                    ;#00551C00: 203156 F8 01               u  00 40  3 150      
    cmp dword[ecx+8],0                    ;#00554EDC: 203171 08 00               u  00 02  2 297      
    cmp dword[esp+12],0                   ;#00550EB3: 203174044 0C 00            u  00 10  2  28      
    cmp dword[ebx+esi*4-8],1              ;#00551C7C: 203174263 F8 01            u  00 48  2 201      
    cmp dword[edi+esi*4+20],0             ;#005550A2: 203174267 14 00            u  00 C0  2 568    *C0*
    cmp dword[ebx+edi*4-12],0             ;#00551A29: 203174273 F4 00            u  00 88  2 440      
    cmp dword[ebp+8],21                   ;#00554966: 203175 08 15               u  00 20  2 181      
    cmp dword[esi-8],1                    ;#0055213B: 203176 F8 01               u  00 40  2 266      
    cmp dword[edi-8],1                    ;#00551F6E: 203177 F8 01               u  00 80  2 101    *80*
    add eax,1                             ;#00552A08: 203300 01                  uv 01 01  1  10      
    add ecx,1                             ;#00552D89: 203301 01                  uv 02 02  1  38      
    add edx,-1                            ;#00554450: 203302 FF                  uv 04 04  1 148      
    add ebx,1                             ;#00551FE9: 203303 01                  vu 08 08  1 138      
    add esp,8                             ;#005559A2: 203304 08                  uv 10 10  1 365      
    add esi,1                             ;#004E17AD: 203306 01                  uv 40 40  1  63      
    add edi,1                             ;#0052C8AE: 203307 01                  uv 80 80  1  92      
    or esi,1                              ;#00555302: 203316 01                  vu 40 40  1 796      
    or edi,1                              ;#00554EB6: 203317 01                  vu 80 80  1 275      
    and eax,2                             ;#00542B27: 203340 02                  uv 01 01  1 638      
    sub eax,1                             ;#00533379: 203350 01                  uv 01 01  1  95      
    sub ecx,1                             ;#005334CE: 203351 01                  uv 02 02  1 161 02   
    sub edx,1                             ;#00554F7F: 203352 01                  vu 04 04  1 464      
    sub esp,4                             ;#005559D7: 203354 04                  uv 10 10  1 436      
    sub esi,1                             ;#00553A3F: 203356 01                  uv 40 40  1  44      
    sub edi,1                             ;#00515A86: 203357 01                  uv 80 80  1  49      
    xor eax,-1                            ;#00550A43: 203360 FF                  uv 01 01  1  90      
    cmp eax,0                             ;#00552B85: 203370 00                  uv 00 01  1  24      
    cmp ecx,0                             ;#004DC9D1: 203371 00                  uv 00 02  1 249      
    cmp edx,0                             ;#0055CF3C: 203372 00                  uv 00 04  1 738      
    cmp esi,0                             ;#00552B3C: 203376 00                  uv 00 40  1   6      
    cmp edi,0                             ;#004DC734: 203377 00                  uv 00 80  1  85      
    test eax,eax                          ;#00552D78: 205300                     uv 00 01  1  35      
    test ecx,ecx                          ;#004DC655: 205311                     uv 00 02  1  33 02   
    test edx,edx                          ;#004DC665: 205322                     uv 00 04  1  39 04   
    test esi,esi                          ;#0052B6BE: 205366                     uv 00 40  1  65 40   
    test edi,edi                          ;#004DC8DD: 205377                     uv 00 80  1 192 80   
    xchg [esp],edi                        ;#00551F39: 207074044                  np 80 90  3  87      
    xchg esi,ecx                          ;#005511CC: 207316                     np 42 42  3 261      
    xchg esi,edi                          ;#00550BF9: 207376                     np C0 C0  3  82      
    mov [edx],al                          ;#00551D66: 210002                     uv 00 05  1 269      
    mov [edx],cl                          ;#00551C1A: 210012                     uv 00 06  1 165 02   
    mov [eax+edi],cl                      ;#00551CA8: 210014070                  vu 00 83  1 209      
    mov [esi+edi],cl                      ;#00551BF9: 210014076                  uv 00 C2  1 147      
    mov [ebx+eax*4],cl                    ;#00550FA4: 210014203                  vu 00 0B  1  93      
    mov [edi+esi*4],cl                    ;#00551E95: 210014267                  uv 00 C2  1 365      
    mov [edi],cl                          ;#0055453B: 210017                     uv 00 82  1  67 02   
    mov [edi],bl                          ;#00550FA2: 210037                     uv 00 88  1  93      
    mov [ebx+eax*4+2],bl                  ;#00550F68: 210134203 02               uv 00 09  1  74      
    mov [ecx],eax                         ;#0055244B: 211001                     uv 00 03  1 119      
    mov [edx],eax                         ;#005549E7: 211002                     uv 00 05  1 236 04 *04*
    mov [esp],eax                         ;#00554418: 211004044                  uv 00 11  1  59      
    mov [esi+edi],eax                     ;#005553FF: 211004076                  uv 00 C1  1 937      
    mov [esi+edx*4],eax                   ;#0055106C: 211004226                  uv 00 45  1 149      
    mov [#00402008],eax                   ;#00556E32: 211005 08204000            uv 00 01  1 747      
    mov [esi],eax                         ;#005515ED: 211006                     uv 00 41  1  19      
    mov [edi],eax                         ;#004D6894: 211007                     uv 00 81  1  87      
    mov [eax],ecx                         ;#0055539E: 211010                     uv 00 03  1 891      
    mov [esp],ecx                         ;#0055441E: 211014044                  uv 00 12  1  63      
    mov [ebx+eax*4],ecx                   ;#0055119C: 211014203                  uv 00 0B  1 250      
    mov [eax+edi*4],ecx                   ;#00551D00: 211014270                  vu 00 83  1 237      
    mov [#00402E68] (dll_path),ecx        ;#00517C6F: 211015 682E4000            vu 00 02  1 195      
    mov [edi],ecx                         ;#00551C53: 211017                     vu 00 82  1 190      
    mov [eax],edx                         ;#0055543D: 211020                     vu 00 05  1 981      
    mov [eax+edx],edx                     ;#00555312: 211024020                  vu 00 05  1 798      
    mov [edi],edx                         ;#005555CC: 211027                     vu 00 84  1 120       
    mov [eax],ebx                         ;#005554EB: 211030                     uv 00 09  1 108       
    mov [did_iup_controls_open],ebx       ;#0052B3EC: 211035 24364000            uv 00 08  1  19      
    mov [edi],ebx                         ;#00554401: 211037                     vu 00 88  1  38      
    mov [ecx],esi                         ;#00551CE3: 211061                     uv 00 42  1 232      
    mov [ebx+eax*4],esi                   ;#00550F0A: 211064203                  uv 00 49  1  50      
    mov [#0040250C] (cdi_filename),esi    ;#00555FFB: 211065 0C254000            vu 00 40  1   8      
    mov [edi],esi                         ;#00552191: 211067                     vu 00 C0  1 291      
    mov [eax],edi                         ;#005547CE: 211070                     vu 00 81  1  32      
    mov [#00402E58] (libidx),edi          ;#00560070: 211075 582E4000            uv 00 80  1   2      
    mov [ecx+8],eax                       ;#00554EE0: 211101 08                  vu 00 03  1 298      
    mov [ebp+ebx],eax                     ;#00554A19: 211104035 00               vu 00 29  1 258      
    mov [esp+4],eax                       ;#00555194: 211104044 04               uv 00 11  1 663      
    mov [ebx+edx*4-12],eax                ;#005513F4: 211104223 F4               uv 00 0D  1 458      
    mov [edi+edx*4+20],eax                ;#00554EF0: 211104227 14               uv 00 85  1 316      
    mov [ebx+esi*4+4],eax                 ;#00554FEA: 211104263 04               vu 00 49  1 499      
    mov [ebp-12] (x2),eax                 ;#004D6D1E: 211105 F4                  uv 00 21  1  66      
    mov [esi+4],eax                       ;#005551B6: 211106 04                  uv 00 41  1 672      
    mov [edi+4],eax                       ;#005547CB: 211107 04                  uv 00 81  1  32      
    mov [eax-8],ecx                       ;#00555346: 211110 F8                  vu 00 03  1 819      
    mov [esp+8],ecx                       ;#00554F02: 211114044 08               vu 00 12  1 321      
    mov [edx+eax*4-12],ecx                ;#00551116: 211114202 F4               vu 00 07  1 202      
    mov [ebx+edx*4-8],ecx                 ;#00555593: 211114223 F8               uv 00 0E  1 117       
    mov [ebx+esi*4-4],ecx                 ;#00554C5A: 211114263 FC               uv 00 4A  1  59      
    mov [edi+esi*4+20],ecx                ;#005550DE: 211114267 14               uv 00 C2  1 593 02   
    mov [ebp] (ih),ecx                    ;#004D6C01: 211115 00                  uv 00 22  1  12 02   
    mov [esi+4],ecx                       ;#005548BA: 211116 04                  uv 00 42  1 111      
    mov [eax+4],edx                       ;#00555402: 211120 04                  uv 00 05  1 939    *01*
    mov [esp+28],edx                      ;#0055544F: 211124044 1C               vu 00 14  1 984      
    mov [ebx+eax*4-4],edx                 ;#005510BE: 211124203 FC               uv 00 0D  1 170 04   
    mov [ebx+esi*4+8],edx                 ;#00554FE6: 211124263 08               uv 00 4C  1 499 04   
    mov [ebx+edi*4-12],edx                ;#00551468: 211124273 F4               uv 00 8C  1 496      
    mov [ebp-8] (symtab[1723]),edx        ;#00552A15: 211125 F8                  uv 00 24  1  15      
    mov [esi+8],edx                       ;#005551BD: 211126 08                  uv 00 44  1 673      
    mov [edi-24],edx                      ;#00551121: 211127 E8                  uv 00 84  1 204      
    mov [eax+8],ebx                       ;#00555405: 211130 08                  vu 00 09  1 939      
    mov [edx-8],ebx                       ;#00555662: 211132 F8                  uv 00 0C  1 129 04 *04*
    mov [eax+edx+4],ebx                   ;#00555322: 211134020 04               uv 00 0D  1 801      
    mov [edi+esi*4+20],ebx                ;#00554659: 211134267 14               uv 00 C8  1  24    *C0*
    mov [eax+edi*4+16],ebx                ;#00554AA7: 211134270 10               uv 00 89  1 314      
    mov [ebp-8] (symtab[8394]),ebx        ;#004D6CB1: 211135 F8                  vu 00 28  1  46      
    mov [esi+4],ebx                       ;#005547FE: 211136 04                  uv 00 48  1  44    *40*
    mov [edx+20],ebp                      ;#005549C9: 211152 14                  uv 00 24  1 219 04 *04*
    mov [esi+20],ebp                      ;#005548CC: 211156 14                  uv 00 60  1 116      
    mov [eax-4],esi                       ;#00555343: 211160 FC                  uv 00 41  1 819      
    mov [ecx+4],esi                       ;#00555277: 211161 04                  uv 00 42  1 758      
    mov [edx+4],esi                       ;#00554E93: 211162 04                  uv 00 44  1 269      
    mov [eax+edx-4],esi                   ;#0055530E: 211164020 FC               uv 00 45  1 798      
    mov [esp+16],esi                      ;#005510E7: 211164044 10               uv 00 50  1 190      
    mov [edi+ecx*4+20],esi                ;#0055532A: 211164217 14               uv 00 C2  1 802      
    mov [edi+edx*4+20],esi                ;#00554E8D: 211164227 14               vu 00 C4  1 267      
    mov [ecx+edi*4+20],esi                ;#00554E12: 211164271 14               uv 00 C2  1 167      
    mov [ebx+edi*4+8],esi                 ;#00554FEE: 211164273 08               uv 00 C8  1 500      
    mov [ebp] (canvas),esi                ;#004D6BC6: 211165 00                  uv 00 60  1   4 40   
    mov [esi+24],esi                      ;#005551C3: 211166 18                  uv 00 40  1 674      
    mov [eax-4],edi                       ;#00554EBD: 211170 FC                  uv 00 81  1 278    *01*
    mov [esp+28],edi                      ;#00555504: 211174044 1C               vu 00 90  1 108       
    mov [ebx+eax*4+4],edi                 ;#00550F1D: 211174203 04               vu 00 89  1  54      
    mov [ebx+edx*4-8],edi                 ;#005524C7: 211174223 F8               uv 00 8C  1 159      
    mov [ebp-4] (name),edi                ;#004D6C1C: 211175 FC                  uv 00 A0  1  18 80   
    mov [esi+24],edi                      ;#005548D1: 211176 18                  uv 00 C0  1 117      
    mov [ebp-212] (i),eax                 ;#0055746B: 211205 2CFFFFFF            vu 00 21  1 989      
    mov [ebp-212] (i),ecx                 ;#005572A6: 211215 2CFFFFFF            uv 00 22  1 917      
    mov [ebx+esi*4+264],edx               ;#00554CFC: 211224263 08010000         vu 00 4C  1  52      
    mov [edi+esi*4+128],ebx               ;#00555223: 211234267 80000000         vu 00 C8  1 729      
    mov [ebp-180] (symtab[1515]),ebx      ;#00556FDA: 211235 4CFFFFFF            uv 00 28  1 811      
    mov [ebp-216] (symtab[1525]),esi      ;#00559A30: 211265 28FFFFFF            vu 00 60  1 268       
    mov [ebp-136] (varno),edi             ;#00557C39: 211275 78FFFFFF            uv 00 A0  1 124       
    mov ecx,eax                           ;#0052C941: 211301                     vu 02 01  1  26      
    mov edx,eax                           ;#004DC72B: 211302                     uv 04 01  1  83      
    mov esi,eax                           ;#004D3918: 211306                     vu 40 01  1  81      
    mov edi,eax                           ;#0052CC30: 211307                     uv 80 01  1  50      
    mov eax,ecx                           ;#0052CC8C: 211310                     uv 01 02  1  64      
    mov edx,ecx                           ;#00520A76: 211312                     uv 04 02  1  81      
    mov esi,ecx                           ;#00552D0E: 211316                     vu 40 02  1  18      
    mov edi,ecx                           ;#00529977: 211317                     uv 80 02  1  31      
    mov eax,esi                           ;#00552DBF: 211360                     vu 01 40  1   6      
    mov ecx,esi                           ;#0052C51F: 211361                     uv 02 40  1  10      
    mov edx,esi                           ;#004DC828: 211362                     uv 04 40  1 148      
    mov edi,esi                           ;#004DAC03: 211367                     uv 80 40  1   4      
    mov eax,edi                           ;#00521736: 211370                     uv 01 80  1 113      
    mov ecx,edi                           ;#005537FD: 211371                     uv 02 80  1 493      
    mov edx,edi                           ;#004DC68B: 211372                     uv 04 80  1  48      
    mov esi,edi                           ;#0055C1FE: 211376                     uv 40 80  1  35      
    mov al,[esi+edi]                      ;#0055249A: 212004076                  uv 01 C0  1 150      
    mov al,[edi]                          ;#00552443: 212007                     uv 01 80  1 118    *80*
    mov cl,[esi]                          ;#00551DEB: 212016                     uv 02 40  1 316      
    mov cl,[edi]                          ;#0055E2CB: 212017                     uv 02 80  1  18      
    mov al,[ebx+esi*4-1]                  ;#00551D70: 212104263 FF               uv 01 48  1 273      
    mov al,[esi-1]                        ;#00550B6E: 212106 FF                  uv 01 40  1  20    *40*
    mov cl,[ebx+eax*4-1]                  ;#00550D40: 212114203 FF               uv 02 09  1 235      
    mov dl,[edi-1]                        ;#00550CB4: 212127 FF                  uv 04 80  1 171      
    mov ah,[ebx+esi*4-1]                  ;#00552462: 212144263 FF               uv 01 48  1 126      
    mov ah,[edi-1]                        ;#00550B75: 212147 FF                  uv 01 80  1  22      
    mov ch,ah                             ;#00550B98: 212354                     uv 02 01  1  39      
    mov dh,ah                             ;#00550B9A: 212364                     vu 04 01  1  39      
    mov eax,[eax]                         ;#0055E39D: 213000                     uv 01 01  1   6    *01*
    mov eax,[edx]                         ;#00552393: 213002                     vu 01 04  1  35      
    mov eax,[esp]                         ;#00555136: 213004044                  vu 01 10  1 634      
    mov eax,[eax+edi*4]                   ;#005523D1: 213004270                  uv 01 81  1  63    *01*
    mov eax,[esi+edi*4]                   ;#0055249F: 213004276                  uv 01 C0  1 151      
    mov eax,[#00402008]                   ;#00554782: 213005 08204000            uv 01 00  1  10      
    mov eax,[esi]                         ;#00555511: 213006                     uv 01 40  1 109    *40*
    mov eax,[edi]                         ;#004D6E51: 213007                     uv 01 80  1  18 80 *80*
    mov ecx,[eax]                         ;#0055580A: 213010                     uv 02 01  1 146 01 *01*
    mov ecx,[ecx]                         ;#00550E73: 213011                     vu 02 02  1  17      
    mov ecx,[edx]                         ;#00550F2E: 213012                     vu 02 04  1  58      
    mov ecx,[esp]                         ;#00555535: 213014044                  uv 02 10  1 113       
    mov ecx,[#00404A0C] (canvas)          ;#004D6BFB: 213015 0C4A4000            uv 02 00  1  11      
    mov ecx,[edi]                         ;#0055462D: 213017                     uv 02 80  1  16    *80*
    mov edx,[eax]                         ;#00551B91: 213020                     vu 04 01  1 110      
    mov edx,[ecx]                         ;#00552441: 213021                     uv 04 02  1 116      
    mov edx,[esp+ecx]                     ;#00552257: 213024014                  uv 04 12  1 110    *02*
    mov edx,[esp]                         ;#0055509F: 213024044                  uv 04 10  1 566      
    mov edx,[esi+edi]                     ;#005553FC: 213024076                  vu 04 C0  1 936      
    mov edx,[esp+ecx*4]                   ;#00552403: 213024214                  uv 04 12  1  77      
    mov edx,[eax+edi*4]                   ;#00551CEA: 213024270                  uv 04 81  1 233      
    mov edx,[#0040324C]                   ;#0052B71A: 213025 4C324000            vu 04 00  1  82      
    mov edx,[esi]                         ;#00554938: 213026                     uv 04 40  1 152      
    mov edx,[edi]                         ;#0055589E: 213027                     vu 04 80  1  30      
    mov esi,[eax]                         ;#00551B05: 213060                     uv 40 01  1  59      
    mov esi,[esp]                         ;#005504FD: 213064044                  uv 40 10  1  46      
    mov esi,[esi+ecx*4]                   ;#0055E177: 213064216                  uv 40 42  1   6 40 *42*
    mov esi,[esi+edi*4]                   ;#00551F1C: 213064276                  uv 40 C0  1  81      
    mov esi,[#00404A10] (cddbuffer)       ;#004D6BC0: 213065 104A4000            uv 40 00  1   3      
    mov esi,[edi]                         ;#00550E71: 213067                     uv 40 80  1  17      
    mov edi,[eax]                         ;#00552139: 213070                     uv 80 01  1 265      
    mov edi,[edx]                         ;#00551A3C: 213072                     uv 80 04  1 445      
    mov edi,[esp]                         ;#005555B6: 213074044                  uv 80 10  1 120       
    mov edi,[esi+ecx*4]                   ;#005556DC: 213074216                  uv 80 42  1 133    *02*
    mov edi,[#00404A1C]                   ;#004D6C16: 213075 1C4A4000            uv 80 00  1  17      
    mov edi,[esi]                         ;#00554628: 213076                     uv 80 40  1  13    *40*
    mov edi,[edi]                         ;#00551F54: 213077                     vu 80 80  1  95      
    mov eax,[eax+8]                       ;#00553378: 213100 08                  uv 01 01  1 269 01 *01*
    mov eax,[esp+8]                       ;#00554F0E: 213104044 08               uv 01 10  1 324      
    mov eax,[ebx+ecx*4-12]                ;#00507484: 213104213 F4               uv 01 0A  1  11      
    mov eax,[esp+ecx*4+4]                 ;#0055165D: 213104214 04               vu 01 12  1  71      
    mov eax,[ebx+edx*4+40]                ;#005548F7: 213104223 28               vu 01 0C  1 137      
    mov eax,[ebx+esi*4-12]                ;#0052AF54: 213104263 F4               uv 01 48  1  11      
    mov eax,[edi+esi*4+20]                ;#00555038: 213104267 14               uv 01 C0  1 544      
    mov eax,[ecx+edi*4+20]                ;#00554E08: 213104271 14               uv 01 82  1 165 02 *02*
    mov eax,[ebx+edi*4-12]                ;#004DC9D9: 213104273 F4               uv 01 88  1 253 80 *80*
    mov eax,[ebp-8] (symtab[8394])        ;#004D6C31: 213105 F8                  vu 01 20  1  24      
    mov eax,[esi-8]                       ;#00552062: 213106 F8                  uv 01 40  1 184      
    mov eax,[edi-12]                      ;#004D6D03: 213107 F4                  uv 01 80  1  63    *80*
    mov ecx,[eax+4]                       ;#00554EF7: 213110 04                  uv 02 01  1 320      
    mov ecx,[ecx-12]                      ;#005512BB: 213111 F4                  uv 02 02  1 340      
    mov ecx,[edx-16]                      ;#005510D3: 213112 F0                  uv 02 04  1 185      
    mov ecx,[esp+12]                      ;#00554DB0: 213114044 0C               vu 02 10  1 120      
    mov ecx,[ebx+eax*4-12]                ;#0052864A: 213114203 F4               uv 02 09  1  92      
    mov ecx,[ebx+edx*4-8]                 ;#0055556E: 213114223 F8               uv 02 0C  1 116       
    mov ecx,[edi+edx*4+20]                ;#00554ED1: 213114227 14               uv 02 84  1 294      
    mov ecx,[ebx+esi*4-12]                ;#004DC7EC: 213114263 F4               uv 02 48  1 133 40 *40*
    mov ecx,[ebx+edi*4-12]                ;#004DC651: 213114273 F4               uv 02 88  1  32 80 *80*
    mov ecx,[ebp] (width)                 ;#004D6CC5: 213115 00                  vu 02 20  1  52      
    mov ecx,[esi-12]                      ;#00554A5B: 213116 F4                  uv 02 40  1 293 40 *40*
    mov ecx,[edi-12]                      ;#00551F87: 213117 F4                  vu 02 80  1 105      
    mov edx,[eax+8]                       ;#00554E77: 213120 08                  uv 04 01  1 261      
    mov edx,[edx+12]                      ;#00554BE6: 213122 0C                  uv 04 04  1  54    *04*
    mov edx,[esp+16]                      ;#00554EFE: 213124044 10               uv 04 10  1 321      
    mov edx,[ebx+eax*4-12]                ;#004DC661: 213124203 F4               uv 04 09  1  38 01 *01*
    mov edx,[esp+ecx*4+16]                ;#00551B51: 213124214 10               vu 04 12  1  90      
    mov edx,[esp+edx*4+4]                 ;#005515BB: 213124224 04               vu 04 14  1   2      
    mov edx,[ebx+esi*4-12]                ;#0055245E: 213124263 F4               vu 04 48  1 125      
    mov edx,[eax+edi*4+16]                ;#00554A94: 213124270 10               uv 04 81  1 309 01 *01*
    mov edx,[edx+edi*4-4]                 ;#005548EF: 213124272 FC               uv 04 84  1 134    *04*
    mov edx,[ebx+edi*4-12]                ;#004DC7E1: 213124273 F4               uv 04 88  1 128 80 *80*
    mov edx,[ebp-8] (symtab[8394])        ;#004D6CA8: 213125 F8                  uv 04 20  1  45      
    mov edx,[esi+12]                      ;#005553AB: 213126 0C                  uv 04 40  1 894      
    mov edx,[edi-12]                      ;#00550BBC: 213127 F4                  uv 04 80  1  54      
    mov ebx,[edi-4]                       ;#00551FCB: 213137 FC                  uv 08 80  1 133      
    mov ebp,[ebp+20] (prevebp)            ;#005549A9: 213155 14                  vu 20 20  1 203      
    mov esi,[eax-4]                       ;#00554DA7: 213160 FC                  uv 40 01  1 119      
    mov esi,[edx-8]                       ;#005556BB: 213162 F8                  vu 40 04  1 131       
    mov esi,[esp+12]                      ;#005549F5: 213164044 0C               uv 40 10  1 252      
    mov esi,[ebx+eax*4-12]                ;#00515260: 213164203 F4               uv 40 09  1  30 01 *01*
    mov esi,[ebx+ecx*4-12]                ;#004DC8E9: 213164213 F4               uv 40 0A  1 197 02 *02*
    mov esi,[esp+ecx*4+4]                 ;#005515FA: 213164214 04               uv 40 12  1   2      
    mov esi,[esi+edx*4+16]                ;#0055E28C: 213164226 10               uv 40 44  1   6 40 *44*
    mov esi,[ebx+esi*4+8]                 ;#00554CD7: 213164263 08               uv 40 48  1  30      
    mov esi,[ebx+edi*4-12]                ;#00528589: 213164273 F4               uv 40 88  1  57 80 *80*
    mov esi,[ebp-8] (symtab[8394])        ;#004D6C5E: 213165 F8                  uv 40 20  1  35      
    mov esi,[esi+4]                       ;#0055462A: 213166 04                  vu 40 40  1  13      
    mov edi,[eax-4]                       ;#00554E55: 213170 FC                  uv 80 01  1 240    *01*
    mov edi,[ecx-12]                      ;#00551212: 213171 F4                  uv 80 02  1 279    *02*
    mov edi,[edx-4]                       ;#005556BE: 213172 FC                  uv 80 04  1 131       
    mov edi,[esp+12]                      ;#00554EFA: 213174044 0C               vu 80 10  1 320      
    mov edi,[ebx+eax*4-12]                ;#004DC8D9: 213174203 F4               uv 80 09  1 191 01 *01*
    mov edi,[ebx+ecx*4-12]                ;#004DC908: 213174213 F4               uv 80 0A  1 203      
    mov edi,[esi+ecx*4-4]                 ;#00554A5E: 213174216 FC               uv 80 42  1 296 02 *02*
    mov edi,[ebx+edx*4-8]                 ;#005524B8: 213174223 F8               uv 80 0C  1 157      
    mov edi,[ebx+esi*4-12]                ;#00501542: 213174263 F4               uv 80 48  1  43 40 *40*
    mov edi,[ebx+edi*4+8]                 ;#00554D44: 213174273 08               uv 80 88  1  81      
    mov edi,[ebp+20] (prevebp)            ;#004D6D00: 213175 14                  vu 80 20  1  60      
    mov edi,[esi-20]                      ;#00551057: 213176 EC                  vu 80 40  1 142      
    mov edi,[edi-4]                       ;#00550B65: 213177 FC                  uv 80 80  1  17      
    mov eax,[ebx+esi*4+264]               ;#00554CF5: 213204263 08010000         uv 01 48  1  52      
    mov eax,[edi+esi*4+128]               ;#00555044: 213204267 80000000         uv 01 C0  1 546      
    mov eax,[ebp-216] (symtab[1525])      ;#00557318: 213205 28FFFFFF            vu 01 20  1 932      
    mov eax,[esi+176]                     ;#00555C11: 213206 B0000000            uv 01 40  1 119      
    mov eax,[edi-180]                     ;#005573D1: 213207 4CFFFFFF            uv 01 80  1 967    *80*
    mov ecx,[ebp-168] (symtab[1512])      ;#00556EEB: 213215 58FFFFFF            vu 02 20  1 780      
    mov edx,[ecx+184]                     ;#0055469F: 213221 B8000000            uv 04 02  1  46 02 *02*
    mov edx,[ebp-168] (symtab[1512])      ;#005570BC: 213225 58FFFFFF            uv 04 20  1 839      
    mov edx,[esi+184]                     ;#0055462F: 213226 B8000000            vu 04 40  1  16      
    mov esp,[esi+196]                     ;#0055463D: 213246 C4000000            uv 10 40  1  18      
    mov ebp,[esi+180]                     ;#00554635: 213256 B4000000            uv 20 40  1  17      
    mov esi,[ebp-184] (symtab[1516])      ;#00556FF1: 213265 48FFFFFF            vu 40 20  1 813      
    mov esi,[esi+160]                     ;#00554653: 213266 A0000000            vu 40 40  1  21      
    mov edi,[ebp-156] (msgs)              ;#00556C12: 213275 64FFFFFF            vu 80 20  1 642      
    mov edi,[esi+156]                     ;#0055464D: 213276 9C000000            uv 80 40  1  21      
    mov eax,ecx                           ;#00555542: 213301                     uv 01 02  1 114       
    mov eax,edx                           ;#0052BAD8: 213302                     uv 01 04  1   9      
    mov eax,ebx                           ;#00552DD0: 213303                     uv 01 08  1   9      
    mov eax,esp                           ;#00555B51: 213304                     vu 01 10  1  89      
    mov eax,ebp                           ;#0052B401: 213305                     uv 01 20  1   3      
    mov eax,esi                           ;#00551A9F: 213306                     uv 01 40  1 485      
    mov eax,edi                           ;#00555B08: 213307                     vu 01 80  1  77      
    mov ecx,eax                           ;#0055560E: 213310                     uv 02 01  1 125       
    mov ecx,edx                           ;#004D6882: 213312                     vu 02 04  1  80      
    mov ecx,ebp                           ;#004D6C81: 213315                     vu 02 20  1  39      
    mov ecx,esi                           ;#00550F5F: 213316                     vu 02 40  1  72      
    mov ecx,edi                           ;#00554403: 213317                     uv 02 80  1  39      
    mov edx,eax                           ;#004D6C42: 213320                     uv 04 01  1  29      
    mov edx,ecx                           ;#004D334A: 213321                     uv 04 02  1 160      
    mov edx,ebx                           ;#0055146D: 213323                     uv 04 08  1 497      
    mov edx,ebp                           ;#00553343: 213325                     uv 04 20  1 258      
    mov edx,esi                           ;#004D6A6E: 213326                     uv 04 40  1  37      
    mov edx,edi                           ;#004D30E1: 213327                     uv 04 80  1  18      
    mov ebx,esi                           ;#00551FE3: 213336                     uv 08 40  1 137      
    mov esp,edi                           ;#00554664: 213347                     uv 10 80  1  26      
    mov ebp,ecx                           ;#00554960: 213351                     uv 20 02  1 175      
    mov ebp,esi                           ;#00554817: 213356                     vu 20 40  1  47      
    mov esi,eax                           ;#0052BAE8: 213360                     uv 40 01  1  59      
    mov esi,ecx                           ;#005512FC: 213361                     uv 40 02  1 368      
    mov esi,edx                           ;#005521DE: 213362                     vu 40 04  1   5      
    mov esi,esp                           ;#005515C9: 213364                     uv 40 10  1   5      
    mov esi,ebp                           ;#00554931: 213365                     vu 40 20  1 150      
    mov esi,edi                           ;#00554FD6: 213367                     uv 40 80  1 495      
    mov edi,eax                           ;#005252B6: 213370                     uv 80 01  1  40 01   
    mov edi,ecx                           ;#00515AC5: 213371                     uv 80 02  1  60 02   
    mov edi,edx                           ;#0055445D: 213372                     uv 80 04  1 166      
    mov edi,esp                           ;#005547EF: 213374                     uv 80 10  1  40      
    mov edi,ebp                           ;#005005AC: 213375                     uv 80 20  1  45      
    mov edi,esi                           ;#004DBAE9: 213376                     uv 80 40  1  99 40   
    lea eax,[ebx+eax*4]                   ;#005523B9: 215004203                  vu 01 09  1  55      
    lea eax,[ebx+esi*4]                   ;#00551C87: 215004263                  uv 01 48  1 203      
    lea eax,[eax+edi*4]                   ;#005556EE: 215004270                  vu 01 81  1 133       
    lea eax,[ebx+edi*4]                   ;#00552092: 215004273                  uv 01 88  1 196      
    lea eax,[esi+edi*4]                   ;#00551B84: 215004276                  uv 01 C0  1 108      
    lea ecx,[esi+edx*4]                   ;#00551897: 215014226                  uv 02 44  1 295      
    lea ecx,[ebx+edi*4]                   ;#00551CD0: 215014273                  uv 02 88  1 223      
    lea edx,[edi+eax*4]                   ;#00551C11: 215024207                  vu 04 81  1 156      
    lea edx,[eax+edi*4]                   ;#00551C2C: 215024270                  uv 04 81  1 174 01 *01*
    lea esp,[esp+ecx*4]                   ;#005515CB: 215044214                  vu 10 12  1   5      
    lea esi,[eax+edx]                     ;#00555315: 215064020                  uv 40 05  1 799      
    lea esi,[esi+edi]                     ;#00551F26: 215064076                  uv 40 C0  1  83      
    lea esi,[ebx+eax*4]                   ;#00550B68: 215064203                  vu 40 09  1  17      
    lea esi,[esi+eax*4]                   ;#00551829: 215064206                  uv 40 41  1 258 01 *01*
    lea esi,[ebx+edx*4]                   ;#00551AA1: 215064223                  vu 40 0C  1 485      
    lea esi,[esi+edx*4]                   ;#00551FFA: 215064226                  vu 40 44  1 142      
    lea esi,[ebx+esi*4]                   ;#00551CC9: 215064263                  vu 40 48  1 219      
    lea esi,[esi+edi*4]                   ;#00552031: 215064276                  uv 40 C0  1 169      
    lea edi,[eax+edi]                     ;#00551D87: 215074070                  vu 80 81  1 289      
    lea edi,[ebx+eax*4]                   ;#0055203D: 215074203                  uv 80 09  1 171      
    lea edi,[edi+eax*4]                   ;#00550F8F: 215074207                  uv 80 81  1  83      
    lea edi,[esi+edx*4]                   ;#005512F9: 215074226                  vu 80 44  1 367      
    lea edi,[edi+edx*4]                   ;#00551756: 215074227                  vu 80 84  1 163      
    lea edi,[eax+edi*4]                   ;#00551472: 215074270                  uv 80 81  1 498      
    lea edi,[#00402000]                   ;#00554855: 215075 00204000            vu 80 00  1  57      
    lea eax,[ecx+1]                       ;#0055122A: 215101 01                  uv 01 02  1 285      
    lea eax,[edx-20]                      ;#005510D6: 215102 EC                  vu 01 04  1 185      
    lea eax,[edx+eax*4+1]                 ;#0055111A: 215104202 01               uv 01 05  1 203      
    lea eax,[ebp-20] (symtab[8408])       ;#004D6AD3: 215105 EC                  uv 01 20  1  64      
    lea eax,[esi+28]                      ;#005551C6: 215106 1C                  vu 01 40  1 674      
    lea ecx,[eax+4]                       ;#005557E5: 215110 04                  vu 02 01  1 144       
    lea ecx,[edx+17]                      ;#00550EC7: 215112 11                  uv 02 04  1  32      
    lea ecx,[ebx+edx*2+1]                 ;#005510ED: 215114123 01               uv 02 0C  1 191      
    lea ecx,[ebx+ecx*4+20]                ;#005554CF: 215114213 14               uv 02 0A  1 106 08 *08*
    lea ecx,[ebx+edx*4+20]                ;#00551053: 215114223 14               uv 02 0C  1 142      
    lea ecx,[ebp-4] (height)              ;#004D6C5B: 215115 FC                  vu 02 20  1  34      
    lea edx,[eax-1]                       ;#004DE83D: 215120 FF                  vu 04 01  1 209      
    lea edx,[ecx+1]                       ;#0052C7A7: 215121 01                  uv 04 02  1  45      
    lea edx,[ebp-4] (p0)                  ;#0050152F: 215125 FC                  uv 04 20  1  38      
    lea edx,[esi+1]                       ;#0050C621: 215126 01                  uv 04 40  1 131      
    lea esp,[ebx+ecx*4-8]                 ;#00554B1B: 215144213 F8               uv 10 0A  1 382 02 *02*
    lea esp,[esp+ecx*4+4]                 ;#00555D2A: 215144214 04               uv 10 12  1 159 02 *02*
    lea esi,[eax+16]                      ;#005547F9: 215160 10                  vu 40 01  1  41      
    lea esi,[eax+ecx*4+16]                ;#005548B1: 215164210 10               vu 40 03  1 109      
    lea esi,[ebp+ecx*4+32]                ;#0055489C: 215164215 20               vu 40 22  1 103      
    lea esi,[esi-4]                       ;#0055493A: 215166 FC                  vu 40 40  1 152      
    lea edi,[eax+21]                      ;#005554F4: 215170 15                  vu 80 01  1 108       
    lea edi,[edi+edx+1]                   ;#0055234D: 215174027 01               uv 80 84  1 204      
    lea edi,[edx+eax*4+4]                 ;#00551112: 215174202 04               uv 80 05  1 202 04 *04*
    lea edi,[ebx+esi*4-1]                 ;#0055E2AA: 215174263 FF               uv 80 48  1  11      
    lea edi,[ebp-12] (symtab[8395])       ;#004D6CC2: 215175 F4                  uv 80 20  1  52      
    lea edi,[edi-1]                       ;#005523B1: 215177 FF                  vu 80 80  1  53      
    lea eax,[ebp-156] (msgs)              ;#00556048: 215205 64FFFFFF            uv 01 20  1  24      
    lea eax,[edi+12240]                   ;#005548A0: 215207 D02F0000            uv 01 80  1 106    *80*
    lea ecx,[ebx+edx*4+240]               ;#005553D0: 215214223 F0000000         vu 02 0C  1 917      
    lea ecx,[ebx+esi*4+240]               ;#00555153: 215214263 F0000000         uv 02 48  1 639      
    lea ecx,[ebx+edi*4+240]               ;#00554DD8: 215214273 F0000000         vu 02 88  1 143      
    lea ecx,[ebp-168] (symtab[1512])      ;#00556E9E: 215215 58FFFFFF            vu 02 20  1 770      
    lea edx,[ebx+esi*4+240]               ;#00555072: 215224263 F0000000         uv 04 48  1 557      
    lea edx,[ebp-220] (symtab[1526])      ;#00559DAE: 215225 24FFFFFF            uv 04 20  1 284       
    lea esi,[ebx+edx*4+128]               ;#005553C9: 215264223 80000000         uv 40 0C  1 917      
    lea edi,[ebp-172] (symtab[1513])      ;#00556F56: 215275 54FFFFFF            uv 80 20  1 795      
    pop [#00404A0C] (canvas)              ;#004D30E8: 217005 0C4A4000            np 00 00  3  19      
    pop dword[edi]                        ;#00551DFA: 217007                     np 00 80  3 319      
    pop dword[eax+4]                      ;#005549E1: 217100 04                  np 00 01  3 230    *01*
    pop dword[ebx+esi*4+16]               ;#00554F5A: 217104263 10               np 00 48  3 345      
    pop dword[ebp-8] (symtab[8394])       ;#004D6C49: 217105 F8                  np 00 20  3  30      
    pop dword[esi-4]                      ;#005510A5: 217106 FC                  np 00 40  3 161      
    pop dword[edi-8]                      ;#0055112B: 217107 F8                  np 00 80  3 207      
    pop dword[ebp-220] (symtab[1526])     ;#0055741E: 217205 24FFFFFF            np 00 20  3 978      
    nop                                   ;#00554F70: 220                        uv 00 00  1 366      
    xchg eax,ecx                          ;#00550743: 221                        np 03 03  2 268      
    xchg eax,edi                          ;#005519F1: 227                        np 81 81  2 415      
    cdq                                   ;#004D6884: 231                        np 04 01  2  81      
    sahf                                  ;#00555948: 236                        np 00 01  2  96      
    mov eax,[#0040201C]                   ;#004D6CC8: 241 1C204000               uv 01 00  1  53      
    mov [#00404A34] (cb_redraw),eax       ;#004D304E: 243 344A4000               uv 00 01  1  10      
    test al,8                             ;#005521E0: 250 08                     uv 00 01  1   6      
    test eax,1                            ;#005554AE: 251 01000000               uv 00 01  1 102 01   
    stosb                                 ;#005520DE: 252                        np 80 81  3 224    *80*
    stosd                                 ;#00551CDB: 253                        np 80 81  3 227      
    lodsb                                 ;#00554865: 254                        np 41 40  2  62      
    lodsd                                 ;#0055130E: 255                        np 41 40  2 380      
    mov al,120                            ;#0052C796: 260 78                     uv 01 00  1  43      
    mov cl,12                             ;#00551F32: 261 0C                     uv 02 00  1  86      
    mov eax,1                             ;#004D6C4C: 270 01000000               uv 01 00  1  33      
    mov ecx,3                             ;#004D6BB1: 271 03000000               uv 02 00  1   1      
    mov edx,6084                          ;#004D6BB6: 272 C4170000               vu 04 00  1   1      
    mov esi,8385                          ;#004D6BD1: 276 C1200000               uv 40 00  1   6      
    mov edi,486                           ;#004D3014: 277 E6010000               uv 80 00  1   5      
    shl ch,7                              ;#00550BA2: 300345 07                  u  02 02  1  41      
    shl dh,4                              ;#00550BA5: 300346 04                  u  04 04  1  42      
    ror eax,2                             ;#00554791: 301310 02                  np 01 01  1  16      
    ror edx,2                             ;#00555445: 301312 02                  np 04 04  1 983      
    ror ebx,2                             ;#00551FF0: 301313 02                  np 08 08  1 140      
    ror esi,2                             ;#0055218B: 301316 02                  np 40 40  1 290      
    ror edi,2                             ;#005554FA: 301317 02                  np 80 80  1 108       
    shl eax,2                             ;#00551C29: 301340 02                  u  01 01  1 171      
    shl ecx,2                             ;#00555835: 301341 02                  u  02 02  1 147       
    shl edx,2                             ;#00555640: 301342 02                  u  04 04  1 127       
    shl esi,2                             ;#00552466: 301346 02                  u  40 40  1 127      
    shl edi,2                             ;#00555353: 301347 02                  u  80 80  1 824      
    shr eax,2                             ;#00554A4E: 301350 02                  u  01 01  1 287      
    shr ecx,2                             ;#00551501: 301351 02                  u  02 02  1 545      
    shr edx,2                             ;#00554862: 301352 02                  u  04 04  1  61      
    shr esi,2                             ;#005513ED: 301356 02                  u  40 40  1 456      
    shr edi,2                             ;#0055E2ED: 301357 02                  u  80 80  1  24      
    sar edx,31                            ;#0052BADA: 301372 1F                  u  04 04  1  10      
    ret 12                                ;#00554EF4: 302 0C00                   np 00 00  3 317      
    ret                                   ;#005558B7: 303                        np 00 00  2  40      
    mov [esi+ecx], byte 0                 ;#0055209F: 306004016 00               uv 00 42  1 199      
    mov [edi], byte 45                    ;#00554574: 306007 2D                  uv 00 80  1  82      
    mov [eax], dword 1                    ;#005555BC: 307000 01000000            uv 00 01  1 120       
    mov [edx], dword #00424354            ;#00554D6D: 307002 54434200            vu 00 04  1 103      
    mov [esp], dword -2                   ;#00554F49: 307004044 FEFFFFFF         uv 00 10  1 342      
    mov [#00402E50], dword 1              ;#00560060: 307005 502E4000 01000000   uv 00 00  1   1      
    mov [esi], dword #00484253            ;#005551B0: 307006 53424800            vu 00 40  1 671      
    mov [edi], dword h4                   ;#0052B44A: 307007 00000040            vu 00 80  1  16      
    mov [eax+8], dword 1                  ;#00555448: 307100 08 01000000         uv 00 01  1 984      
    mov [edx-8], dword 1                  ;#00555652: 307102 F8 01000000         uv 00 04  1 128       
    mov [esp+4], dword 2146435071         ;#00555997: 307104044 04 FFFFEF7F      uv 00 10  1 363      
    mov [ebx+edi*4+16], dword -1          ;#00554D9E: 307104273 10 FFFFFFFF      uv 00 88  1 116 80 *80*
    mov [ebp+28] (retaddr),#004D6BEC      ;#004D6BE0: 307105 1C EC6B4D00         uv 00 20  1   8      
    mov [esi+8], dword 21                 ;#00554801: 307106 08 15000000         vu 00 40  1  44      
    mov [edi-12], dword h4                ;#004D6D06: 307107 F4 00000040         vu 00 80  1  63      
    mov [eax+12276], dword 1012290370     ;#005547E0: 307200 F42F0000 4253563C   uv 00 01  1  35      
    mov [edx+272], dword #00424345        ;#00554D73: 307202 10010000 45434200   uv 00 04  1 104      
    mov [symtab[1559]], dword 14          ;#00559646: 307205 14FFFFFF 0E000000   uv 00 20  1 250       
    mov [edi-180], dword h4               ;#005573D7: 307207 4CFFFFFF 00000040   vu 00 80  1 967      
    int3                                  ;#00533A13: 314                        np 00 00 13   3      
    shl ecx,1                             ;#00555544: 321341                     u  02 02  1 114       
    shl edx,1                             ;#0052C7AA: 321342                     u  04 04  1  46      
    shl esi,1                             ;#0052BAEE: 321346                     u  40 40  1  61      
    shr eax,1                             ;#005559DE: 321350                     u  01 01  1 446      
    shr edx,1                             ;#00555300: 321352                     u  04 04  1 796      
    sar eax,1                             ;#00531BFE: 321370                     u  01 01  1 151       
    shl eax,cl                            ;#005558A0: 323340                     np 01 03  4  31      
    fcom st2                              ;#005559C4: 330322                     np 00 00  1 411      
    fcomp                                 ;#00555944: 330331                     np 00 00  1  89      
    fldcw word[esp]                       ;#00553D9F: 331054044                  np 00 10  8  22      
    fldcw word[#00402218] (down53)        ;#00553DF8: 331055 18224000            np 00 00  8  42      
    fnstcw word[esp]                      ;#00553D90: 331074044                  np 00 10  2  17    *10*
    fld st0                               ;#0055596C: 331300                     np 00 00  1 232      
    fld st1                               ;#005559C0: 331301                     np 00 00  1 390      
    fxch                                  ;#0055594B: 331311                     np 00 00  1  99      
    fchs                                  ;#005559F4: 331340                     np 00 00  1 669      
    fabs                                  ;#005559BE: 331341                     np 00 00  1 389      
    fld1                                  ;#00555976: 331350                     np 00 00  2 314      
    fldpi                                 ;#00554C0F: 331353                     np 00 00  5   6      
    fldz                                  ;#00555942: 331356                     np 00 00  2  87      
    f2xm1                                 ;#00555974: 331360                     np 00 00 57 257      
    fyl2x                                 ;#0055596A: 331361                     np 00 00 103 12       
    fprem                                 ;#0055044A: 331370                     np 00 00 64 115      
    frndint                               ;#0055596E: 331374                     np 00 00 20 233      
    fscale                                ;#0055597A: 331375                     np 00 00 32 319      
    fild dword[esp]                       ;#00555532: 333004044                  np 00 10  3 112       
    fild dword[esi]                       ;#0055551E: 333006                     np 00 40  3 110       
    fild dword[edi]                       ;#005543FC: 333007                     np 00 80  3  35      
    fist dword[esp]                       ;#0055552D: 333024044                  np 00 10  6 112       
    fistp dword[esp]                      ;#005559DA: 333034044                  np 00 10  6 439    *10*
    fild dword[esp+28]                    ;#00555618: 333104044 1C               np 00 10  3 125       
    fninit                                ;#00553D8B: 333343                     np 00 00 12   2      
    fsub st1,st0                          ;#00555970: 334351                     np 00 00  3 253      
    fld qword[ecx]                        ;#0055041B: 335001                     np 00 02  1  99      
    fld qword[esp]                        ;#00555987: 335004044                  np 00 10  1 354      
    fld qword[ebx+eax*4]                  ;#00550C64: 335004203                  np 00 09  1 136      
    fld qword[ebx+ecx*4]                  ;#005509CD: 335004213                  np 00 0A  1  56      
    fld qword[ebx+edx*4]                  ;#00550C51: 335004223                  np 00 0C  1 125      
    fld qword[ebx+esi*4]                  ;#005504EA: 335004263                  np 00 48  1  34      
    fld qword[ebx+edi*4]                  ;#00550D7C: 335004273                  np 00 88  1 265      
    fld qword[esi]                        ;#00550B92: 335006                     np 00 40  1  31      
    fld qword[edi]                        ;#00552287: 335007                     np 00 80  1 132      
    fstp qword[esp]                       ;#00555527: 335034044                  np 00 10  2 111    *10*
    fstp qword[ebx+edx*4]                 ;#0055557E: 335034223                  np 00 0C  2 116       
    fstp qword[ebx+esi*4]                 ;#00554C18: 335034263                  np 00 48  2  15    *40*
    fstp qword[eax+8]                     ;#005555CE: 335130 08                  np 00 01  2 120       
    fstp st0                              ;#0055597E: 335330                     np 00 00  1 352      
    fstp st1                              ;#00550451: 335331                     np 00 00  1 188      
    faddp st1,st0                         ;#00555978: 336301                     np 00 00  3 316      
    fmulp st1,st0                         ;#00554424: 336311                     np 00 00  3  67      
    fcompp                                ;#00550B94: 336331                     np 00 00  1  32      
    fsubp st1,st0                         ;#0055070C: 336351                     np 00 00  3 258      
    fdivp st1,st0                         ;#0055442B: 336371                     np 00 00 39  76      
    fild qword[esp]                       ;#00555788: 337054044                  np 00 10  3 140       
    fistp qword[esp]                      ;#00552291: 337074044                  np 00 10  6 137    *10*
    fnstsw ax                             ;#00555946: 337340                     np 01 00  6  90      
    call #00554602 (:>initFEH)            ;#004D3000: 350 FD150800               v  00 00  1   1      
    jmp #004FF2B9 (code:cdCanvasActivate) ;#004D6BE7: 351 CD860200               v  00 00  1   8      
    jmp #00552B96                         ;#00552B8D: 353 07                     v  00 00  1  25      
    rep movsb                             ;#005520DA: 363:244                    np C2 C2  4 218    *C0*
    rep movsd                             ;#005515CE: 363:245                    np C2 C2  4   8    *40*
    repe cmpsb                            ;#00550BCC: 363:246                    np C2 C2  5  58      
    rep stosb                             ;#00551945: 363:252                    np 82 83  3 354    *80*
    rep stosd                             ;#005548DF: 363:253                    np 82 83  3 123    *80*
    test byte[ebx+eax*4-1],#80            ;#004DC803: 366104203 FF 80            u  00 09  2 138      
    test byte[ebx+ecx*4-1],#80            ;#004DC5C8: 366104213 FF 80            u  00 0A  2   3      
    test byte[ebx+esi*4-1],#80            ;#005529E6: 366104263 FF 80            u  00 48  2   3      
    test byte[ebx+edi*4-1],#80            ;#0052B964: 366104273 FF 80            u  00 88  2  13      
    test dl,#80                           ;#00550CBA: 366302 80                  uv 00 04  1 172      
    test ah,65                            ;#0055598E: 366304 41                  uv 00 01  1 362      
    test dword[eax-4],16777215            ;#005556F3: 367100 FC FFFFFF00         u  00 01  2 134    *01*
    test dword[edx-4],16777215            ;#00555645: 367102 FC FFFFFF00         u  00 04  2 128    *04*
    test dword[ebx+edi*4+12],512          ;#00554A76: 367104273 0C 00020000      u  00 88  2 300      
    test ecx,2048                         ;#005548FB: 367301 00080000            uv 00 02  1 138      
    test esi,1                            ;#0055535E: 367306 01000000            uv 00 40  1 827      
    test edi,1                            ;#00554E65: 367307 01000000            uv 00 80  1 257      
    neg eax                               ;#00554572: 367330                     np 01 01  1  81      
    neg ecx                               ;#005509A5: 367331                     np 02 02  1  39      
    neg edx                               ;#005558EB: 367332                     np 04 04  1  53      
    imul ecx                              ;#004D687D: 367351                     np 05 03 10  70      
    imul esi                              ;#005558FA: 367356                     np 05 41 10  57      
    div ecx                               ;#00554583: 367361                     np 05 07 41  85      
    idiv ecx                              ;#0052BAE6: 367371                     np 05 07 46  13      
    cld                                   ;#005548E1: 374                        np 00 00  2 126      
    std                                   ;#005548DE: 375                        np 00 00  2 119      
    call [#0040103C] (ExitProcess)        ;#00554B35: 377025 3C104000            np 00 00  2 403      
    jmp dword[esp]                        ;#00555BE3: 377044044                  np 00 10  2 110      
    jmp dword[esi*4+#00557A38]            ;#00557A35: 377044265 387A5500         np 00 40  2 123       
    jmp dword[edi*4+#0052EF20]            ;#0052EF1D: 377044275 20EF5200         np 00 80  2  68      
    push dword[eax]                       ;#005549D1: 377060                     np 00 01  2 224      
    push dword[edx]                       ;#00550DEB: 377062                     np 00 04  2 331      
    push dword[esp]                       ;#0055563D: 377064044                  np 00 10  2 127    *10*
    push dword[#00404A14] (cdcanvas)      ;#004D6E93: 377065 144A4000            np 00 00  2  28      
    push dword[eax+4]                     ;#0055497B: 377160 04                  np 00 01  2 187    *01*
    push dword[ecx-12]                    ;#005512F3: 377161 F4                  np 00 02  2 365      
    push dword[esp+32]                    ;#0055494D: 377164044 20               np 00 10  2 162      
    push dword[ebp] (symtab[8400])        ;#004D69D8: 377165 00                  np 00 20  2   4      
    push dword[esi-4]                     ;#00551109: 377166 FC                  np 00 40  2 197      
    push dword[ebp-180] (symtab[1515])    ;#00556FD4: 377265 4CFFFFFF            np 00 20  2 809      
    call eax                              ;#00554A53: 377320                     np 00 01  2 288      
    call edx                              ;#005505D0: 377322                     np 00 04  2 140      
    call esi                              ;#00550618: 377326                     np 00 40  2 165      
    jmp eax                               ;#00555AEC: 377340                     np 00 01  2  71      
    jmp ecx                               ;#0055392B: 377341                     np 00 02  2 555      
