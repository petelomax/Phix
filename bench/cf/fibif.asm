; Fibonacci iterative method in asm
format PE

include 'macro.inc'

.data

Errmsg	    db "Error",13,10,0
stdout	    dd ?
stdin	    dd ?
bytes_count dd ?
count	    dd ?
InBuf	    rb 512

.code

Fibonacci:
;   calculate fib(edi), result in eax
; leaves ecx intact.
    mov eax,1
    mov ebx,1
    dec edi
    jle fibiret
  @@:
    lea edx,[eax+ebx]
    mov eax,ebx
    mov ebx,edx
    dec edi
    jg @b
  fibiret:
    ret

  start:
    
    mov ecx,200000
  @@:
    mov edi,44
    call Fibonacci
    cmp eax,701408733
    jne FibiErr
    dec ecx
    jnz @b
  @@:
    invoke ExitProcess,0

  FibiErr:
    invoke  GetStdHandle,STD_OUTPUT_HANDLE
    mov     [stdout], eax
    invoke  GetStdHandle,STD_INPUT_HANDLE
    mov     [stdin], eax

    invoke WriteFile,[stdout],Errmsg,7,bytes_count,0

    invoke ReadFile, [stdin], InBuf, 512, bytes_count, 0    ; bytes_count is returned including 0d0a
    invoke ExitProcess,0

.end start
