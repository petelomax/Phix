; Hello World, message_box and console (both put and get).
format PE GUI 4.0

include 'macro.inc'

.data

hw	    db "hello world",13,10,0
ctitle	    db "a console from a PE 4.0 GUI app",0
stdout	    dd ?
stdin	    dd ?
bytes_count dd ?
count	    dd ?
InBuf	    rb 512

.code

  start:
    invoke  AllocConsole						    ;  AllocConsole();
    invoke  SetConsoleTitle, ctitle
    invoke  GetStdHandle,STD_OUTPUT_HANDLE
    mov     [stdout], eax
    invoke  GetStdHandle,STD_INPUT_HANDLE
    mov     [stdin], eax

    mov     esi,hw
    call    display_string

    invoke ReadFile, [stdin], InBuf, 512, bytes_count, 0    ; bytes_count is returned including 0d0a

    invoke MessageBox, HWND_DESKTOP, "Hi! I'm the example program!","Win32 Assembly",MB_OK
    invoke ExitProcess,0

display_string:

; this is strlen - should not be needed (except perhaps for returned things):
    mov edi,esi
    or	ecx,-1
    xor al,al
    repne scasb ; on the masm board this was tagged as dog-slow...
    neg ecx
    sub ecx,2

    invoke WriteFile,[stdout],esi,ecx,bytes_count,0
    ret

.end start
