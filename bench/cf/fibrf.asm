; Fibonacci recursive method in asm
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
    cmp eax,2
    jg @f
    mov eax,1
    ret
  @@:
    dec eax
    push eax
    call Fibonacci
    mov ecx,eax
    pop eax
    push ecx
    dec eax
    call Fibonacci
    pop ecx
    add eax,ecx
    ret

  start:
;   invoke  AllocConsole						    ;  AllocConsole();
;   invoke  SetConsoleTitle, ctitle
    
;   mov eax,26
;   call Fibonacci
;   cmp eax,121393
;   mov eax,27
;   call Fibonacci
;   cmp eax,196418
;   mov eax,28
;   call Fibonacci
;   cmp eax,317811
;   mov eax,29
;   call Fibonacci
;   cmp eax,514229
;   mov eax,30
;   call Fibonacci
;   cmp eax,832040
    mov eax,31
    call Fibonacci
    cmp eax,1346269
    je @f
    invoke  GetStdHandle,STD_OUTPUT_HANDLE
    mov     [stdout], eax
    invoke  GetStdHandle,STD_INPUT_HANDLE
    mov     [stdin], eax

    invoke WriteFile,[stdout],Errmsg,7,bytes_count,0

    invoke ReadFile, [stdin], InBuf, 512, bytes_count, 0    ; bytes_count is returned including 0d0a

  @@:
    invoke ExitProcess,0

.end start
