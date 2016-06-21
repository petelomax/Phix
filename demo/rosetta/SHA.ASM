; ---------------------------------------------------------------------------
; Major.Minor.Build: 0.5.0
; -----------------------------------------------------------------------------
; Copyright (c) 2008, Alex Patterson, Greenville, WI. All rights reserved.
;
; License Terms
;
; The free distribution and use of this software is allowed (with or without
;  changes) provided that:
;
;  1. Source code distributions include the above copyright notice, this
;      list of conditions and the following disclaimer.
;
;  2. Binary distributions include the above copyright notice, this list
;      of conditions and the following disclaimer in their documentation.
;
;  3. The name of the copyright holder is not used to endorse products
;      built using this software without specific written permission.
;
; Disclaimer
;
; This software is provided 'as is' by the author, who assumes no
;  liability for any and all negative results of using this software.
; -----------------------------------------------------------------------------
; Issue Date < release date > day/month/year
;
; A SHA implementation for x86 processors using the FASM assembler, and is
;  mainly for educational use.  This code provides the standard SHA-256
;  operations only, due to personal use requirements.  This code can easily
;  be modified to support 160,224,384,or 512-bit hash sizes.
;
; Calling Interface:
;
;  SHA_HashBlock (byte in[], dword out[8], unsigned int bytes_to_hash);
;
; In this implementation the stdcall convention is used, where the parameters
;  are pushed in reverse order onto the stack, and the callee clears the
;  stack frame.  The standard callback registers are preserved across calls
;  to these functions, including ebx,esi,edi,& ebp.
;
; Error Return Value:
;
;  eax = 0   -> memory allocation error
;
; Test Vectors:
;
; This code passes all FIPS 180-2 test vectors, including others provided by
;  the NESSIE Foundation.  Included is a test file 'SHAvectors.inc', which
;  when compiled into a Win32 binary tests each of the sample vectors to
;  ensure that the code has not been tampered with.

; -----------------------------------------------------------------------------
;
; fasm PE file headers
; -----------------------------------------------------------------------------

 format PE GUI 4.0 DLL
 include '%fasm%\win32ax.inc'

; -----------------------------------------------------------------------------
;
; code constants
; -----------------------------------------------------------------------------

 b equ byte
 d equ dword

; stack frame

 sha_in_block	  = 4  ; stack offset to "in" block parameter
 sha_out_block	  = 8  ; stack offset to "out" block parameter
 sha_size_int	  = 12 ; stack offset to size in bytes of "in" block
 sha_stack_space  = 16 ; size of registers to preserve on stack

; -----------------------------------------------------------------------------
;
; structures section -> organized memory layouts
; -----------------------------------------------------------------------------
 struct sha_working_vars_
    var_t2     rd 1
    var_t1     rd 1
    var_a      rd 1
    var_b      rd 1
    var_c      rd 1
    var_d      rd 1
    var_e      rd 1
    var_f      rd 1
    var_g      rd 1
    var_h      rd 1
 ends

 struct sha_state_
    usr_size   rd 1  ; param - number of bytes
    usr_hout   rd 1  ; param - output addr.
    num_blocks rd 1  ; # of 64-byte blocks
    mem_base   rd 1  ; base address of alloc
    schedule   rd 64 ; per-block message schedule
 ends

; -----------------------------------------------------------------------------
;
; '.udata' section -> memory of constants & variables
; -----------------------------------------------------------------------------
 section '.udata' data readable writeable

 sha_vars sha_working_vars_
 sha_state sha_state_

 align 16

; sha-256 initializes the hashing constants with these 8 dwords

 sha_init  dd 0x6a09e667,0xbb67ae85,0x3c6ef372,0xa54ff53a
	   dd 0x510e527f,0x9b05688c,0x1f83d9ab,0x5be0cd19

; sha-256 uses 64 dwords, one for each round of main hashing routine

 sha_const dd 0x428a2f98,0x71374491,0xb5c0fbcf,0xe9b5dba5
	   dd 0x3956c25b,0x59f111f1,0x923f82a4,0xab1c5ed5
	   dd 0xd807aa98,0x12835b01,0x243185be,0x550c7dc3
	   dd 0x72be5d74,0x80deb1fe,0x9bdc06a7,0xc19bf174
	   dd 0xe49b69c1,0xefbe4786,0x0fc19dc6,0x240ca1cc
	   dd 0x2de92c6f,0x4a7484aa,0x5cb0a9dc,0x76f988da
	   dd 0x983e5152,0xa831c66d,0xb00327c8,0xbf597fc7
	   dd 0xc6e00bf3,0xd5a79147,0x06ca6351,0x14292967
	   dd 0x27b70a85,0x2e1b2138,0x4d2c6dfc,0x53380d13
	   dd 0x650a7354,0x766a0abb,0x81c2c92e,0x92722c85
	   dd 0xa2bfe8a1,0xa81a664b,0xc24b8b70,0xc76c51a3
	   dd 0xd192e819,0xd6990624,0xf40e3585,0x106aa070
	   dd 0x19a4c116,0x1e376c08,0x2748774c,0x34b0bcb5
	   dd 0x391c0cb3,0x4ed8aa4a,0x5b9cca4f,0x682e6ff3
	   dd 0x748f82ee,0x78a5636f,0x84c87814,0x8cc70208
	   dd 0x90befffa,0xa4506ceb,0xbef9a3f7,0xc67178f2

; -----------------------------------------------------------------------------
;
; macro section -> sha-256 functions
; -----------------------------------------------------------------------------

; ch     ->    (x &  y)^(~x &  z)
; maj    ->    (x &  y)^( x &  z)^(y &   z)
; sigma0 ->    (x >  2)^( x > 13)^(x >  22)
; sigma1 ->    (x >  6)^( x > 11)^(x >  25)
; alpha0 ->    (x >  7)^( x > 18)^(x >>  3)
; alpha1 ->    (x > 17)^( x > 19)^(x >> 10)

 macro ch  reg1,reg2,x,y,z {
    mov     reg1, x
    mov     reg2, reg1
    not     reg2
    and     reg1, y
    and     reg2, z
    xor     reg1, reg2	   }
 macro maj reg1,reg2,x,y,z {
    mov     reg1, x
    mov     reg2, reg1
    and     reg1, y
    and     reg2, z
    xor     reg1, reg2
    mov     reg2, y
    and     reg2, z
    xor     reg1, reg2	   }
 macro sigma0 reg1,reg2,x  {
    mov     reg1, x
    mov     reg2, reg1
    ror     reg1,  2
    ror     reg2, 13
    xor     reg1, reg2
    mov     reg2, x
    ror     reg2, 22
    xor     reg1, reg2	   }
 macro sigma1 reg1,reg2,x  {
    mov     reg1, x
    mov     reg2, reg1
    ror     reg1,  6
    ror     reg2, 11
    xor     reg1, reg2
    mov     reg2, x
    ror     reg2, 25
    xor     reg1, reg2	   }
 macro alpha0 reg1,reg2,x  {
    mov     reg1, x
    mov     reg2, reg1
    ror     reg1,  7
    ror     reg2, 18
    xor     reg1, reg2
    mov     reg2, x
    shr     reg2, 3
    xor     reg1, reg2	   }
 macro alpha1 reg1,reg2,x  {
    mov     reg1, x
    mov     reg2, reg1
    ror     reg1, 17
    ror     reg2, 19
    xor     reg1, reg2
    mov     reg2, x
    shr     reg2, 10
    xor     reg1, reg2	   }

; -----------------------------------------------------------------------------
;
; outer sha routines - preprocess | main function
; -----------------------------------------------------------------------------
 macro sha_preprocess {

; calculate the number of 64-byte blocks in user data

    mov     ebx, eax
    and     eax, 63
    shr     ebx, 6
   .if	    eax > 55
    inc     ebx
   .endif
    inc     ebx
    mov     [sha_state.num_blocks], ebx

; allocate enough memory for the blocks ( 64*blocks )

    shl     ebx, 6
    invoke  VirtualAlloc, 0, ebx, 0x3000, 0x04
    cmp     eax, 0
    jz	    .e
    mov     [sha_state.mem_base], eax

; format the data in memory ( copy | append | attach bit size )

    mov     edi, eax
    mov     ecx, [sha_state.usr_size]
    rep     movsb
    mov     b[edi], 0x80
    mov     ecx, [sha_state.usr_size]
    shl     ecx, 3
    bswap   ecx
    mov     [eax+ebx-4], ecx

; set the initial hash values ( hash_h0 -> hash_h7 )

    mov     esi, sha_init
    mov     edi, [sha_state.usr_hout]
    mov     ecx, 8
    rep     movsd

; ebp will always point to the current data block ( base + 64*b )

    mov     ebp, eax
}

 macro sha_hash_block blk_off {

; load user data into schedule, format to little-endian

    mov     esi, ebp
    mov     edi, sha_state.schedule
    mov     ecx, 16
@@: lodsd
    bswap   eax
    stosd
    dec     ecx
    jnz @b

; perform message schedule

    mov     ecx, 48
@@: alpha1  eax, ebx,[edi-8]
    add     eax, [edi-28]
    alpha0  ebx, esi,[edi-60]
    add     eax, ebx
    add     eax, [edi-64]
    stosd
    dec     ecx
    jnz     @b

; initialize eight working variables with hash value

    mov     esi, [sha_state.usr_hout]
    mov     edi, sha_vars.var_a
    mov     ecx, 8
    rep     movsd

; main computation loop

; t1 = h + sigma1(e) + ch(e,f,g) + k(t) + w(t)
; t2 = sigma0(a) + maj(a,b,c)
; h = g
; g = f
; f = e
; e = d + t1
; d = c
; c = b
; b = a
; a = t1 + t2

    xor     ecx, ecx

@@: sigma1  ebx, esi, [sha_vars.var_e]
    add     ebx, [sha_vars.var_h]
    ch	    eax, esi, [sha_vars.var_e], [sha_vars.var_f], [sha_vars.var_g]
    add     eax, ebx
    add     eax, d[sha_const+ecx]
    add     eax, [sha_state.schedule+ecx]
    mov     [sha_vars.var_t1], eax

    sigma0  eax, ebx, [sha_vars.var_a]
    maj     ebx, esi, [sha_vars.var_a], [sha_vars.var_b], [sha_vars.var_c]
    add     eax, ebx
    mov     [sha_vars.var_t2], eax

; main variables

    std
    mov     esi, sha_vars.var_g
    mov     edi, sha_vars.var_h

  rept 3  \{
    lodsd
    stosd \}

    lodsd
    add     eax, [esi-12]

  rept 4  \{
    stosd
    lodsd \}

    add     eax, [esi]
    stosd

    add     ecx, 4
    cmp     ecx, 256
    jnz     @b

; add working variables to hash value

    cld
    mov     esi, sha_vars.var_a
    mov     edi, [sha_state.usr_hout]

    xor     ecx, ecx
@@: lodsd
    add     [edi+ecx], eax
    add     ecx, 4
    cmp     ecx, 32
    jnz @b
 }

 macro sha_endprocess {

; destroy internal memory

    cld
    mov     eax, 0xF0F0F0F0
    mov     ecx, 78
    mov     edi, sha_vars.var_t2
    rep     stosd
    sub     edi, 4
    std
    not     eax; 0x0F0F0F0F
    add     ecx, 78
    rep     stosd
    cld

; de-allocate user data

    invoke VirtualFree,[sha_state.mem_base],0,0x8000
 }

; -----------------------------------------------------------------------------
;
; common stack routines
; -----------------------------------------------------------------------------
 macro preserve_callback_regs {

    sub     esp, sha_stack_space
    mov     [esp+ 0], ebx
    mov     [esp+ 4], ebp
    mov     [esp+ 8], edi
    mov     [esp+12], esi }

 macro restore_callback_regs ret_val {

    mov     ebx, [esp+ 0]
    mov     ebp, [esp+ 4]
    mov     edi, [esp+ 8]
    mov     esi, [esp+12]
    add     esp, sha_stack_space
    ret     ret_val }

; -----------------------------------------------------------------------------
;
; '.code' section
; -----------------------------------------------------------------------------
 section '.code' code readable writeable

; -----------------------------------------------------------------------------
;
; sha hashing - user interface
; -----------------------------------------------------------------------------
 sha_hash:

; preserve registers

    preserve_callback_regs

; handle the parameters

    mov     eax, [esp+sha_stack_space+ sha_size_int]
    mov     esi, [esp+sha_stack_space+ sha_in_block]
    mov     edi, [esp+sha_stack_space+sha_out_block]

    mov     [sha_state.usr_size], eax
    mov     [sha_state.usr_hout], edi

; prepare user data for algorithm

    sha_preprocess

; prepare "global registers" for main algorithm

    cld
    mov     edx, [sha_state.num_blocks]
    mov     ebp, [sha_state.mem_base]

; perform hashing one block at a time

.1: sha_hash_block
    add     ebp, 64
    dec     edx
    jnz     .1

; destroy and free data from memory

    sha_endprocess

    mov     eax, 1

; restore registers and return

.e: restore_callback_regs 12

; -----------------------------------------------------------------------------
;
; '.idata' section - library imports
; -----------------------------------------------------------------------------
 section '.idata' import data readable writeable

 library kernel,'Kernel32.dll'

 import kernel,\
	VirtualAlloc, 'VirtualAlloc',\
	VirtualFree,  'VirtualFree'

; -----------------------------------------------------------------------------
;
; '.edata' section - library exports
; -----------------------------------------------------------------------------
 section '.edata' export data readable

 export 'SHA.dll',\
     sha_hash,	'SHA_HashBlock'

; -----------------------------------------------------------------------------
;
; '.reloc' section - library relocations
; -----------------------------------------------------------------------------
 section '.reloc' fixups data discardable