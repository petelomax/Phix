#include <stdio.h>

int fib(n)
{
    if (n <= 2)
	return 1;
    else
	return fib(n-1) + fib(n-2);
}

int main(int argc, char **argv) 
{
    int n,e,r;

//  n=26;   e=121393;
//  n=27;   e=196418;
//  n=28;   e=317811;
//  n=29;   e=514229;
//  n=30;   e=832040;
    n=31;   e=1346269;
    r=fib(n);
    if (r!=e) {
	printf("fib(%d) = %d, not %d\n", n, r, e);
	return 1;
    }
    return 0;
}

/* This is the disassembly (commented) that I get. The "not great" markers 
   indicate places where the compiler I was using (tinyCC) was not being 
   very bright; other C compilers will probably do better, but not much.
00401000  >55		    PUSH EBP				; fib(n)
00401001   89E5 	    MOV EBP,ESP
00401003   81EC 04000000    SUB ESP,4				; [space for 1 var (work)]
00401009   90		    NOP
0040100A   8B45 08	    MOV EAX,DWORD PTR SS:[EBP+8]
0040100D   83F8 02	    CMP EAX,2				; if (n <= 2)
00401010   0F8F 0F000000    JG FIBRC.00401025
00401016   B8 01000000	    MOV EAX,1				; return 1
0040101B   E9 32000000	    JMP FIBRC.00401052
00401020   E9 2D000000	    JMP FIBRC.00401052			; else		; not great
00401025  >8B45 08	    MOV EAX,DWORD PTR SS:[EBP+8]			; not great(nop)
00401028   83E8 01	    SUB EAX,1
0040102B   50		    PUSH EAX
0040102C   E8 CFFFFFFF	    CALL FIBRC.00401000 		; fib(n-1)
00401031   83C4 04	    ADD ESP,4
00401034   8B4D 08	    MOV ECX,DWORD PTR SS:[EBP+8]			; not great(^)
00401037   83E9 02	    SUB ECX,2
0040103A   51		    PUSH ECX						; not great(v)
0040103B   8945 FC	    MOV DWORD PTR SS:[EBP-4],EAX
0040103E   E8 BDFFFFFF	    CALL FIBRC.00401000 		; fib(n-2);
00401043   83C4 04	    ADD ESP,4
00401046   8B4D FC	    MOV ECX,DWORD PTR SS:[EBP-4]
00401049   01C1 	    ADD ECX,EAX 			; +
0040104B   89C8 	    MOV EAX,ECX 					; not great
0040104D   E9 00000000	    JMP FIBRC.00401052					;  "   "   (nop)
00401052  >C9		    LEAVE
00401053   C3		    RETN
00401054   55		    PUSH EBP				; main()
00401055   89E5 	    MOV EBP,ESP
00401057   81EC 0C000000    SUB ESP,0C				; [space for 3 vars (n,e,r)]
0040105D   90		    NOP
0040105E   B8 1F000000	    MOV EAX,1F				; n=31
00401063   8945 FC	    MOV DWORD PTR SS:[EBP-4],EAX			; not great
00401066   B8 DD8A1400	    MOV EAX,148ADD			; e=1346269
0040106B   8945 F8	    MOV DWORD PTR SS:[EBP-8],EAX			; not great
0040106E   8B45 FC	    MOV EAX,DWORD PTR SS:[EBP-4]
00401071   50		    PUSH EAX
00401072   E8 89FFFFFF	    CALL FIBRC.00401000 		; fib(n)
00401077   83C4 04	    ADD ESP,4
0040107A   8945 F4	    MOV DWORD PTR SS:[EBP-C],EAX	; r=eax
0040107D   8B45 F4	    MOV EAX,DWORD PTR SS:[EBP-C]			; not great
00401080   8B4D F8	    MOV ECX,DWORD PTR SS:[EBP-8]
00401083   39C8 	    CMP EAX,ECX 			; if (r!e)
00401085   0F84 24000000    JE FIBRC.004010AF
0040108B   8B45 F8	    MOV EAX,DWORD PTR SS:[EBP-8]
0040108E   50		    PUSH EAX
0040108F   8B45 F4	    MOV EAX,DWORD PTR SS:[EBP-C]
00401092   50		    PUSH EAX
00401093   8B45 FC	    MOV EAX,DWORD PTR SS:[EBP-4]
00401096   50		    PUSH EAX
00401097   B8 00204000	    MOV EAX,FIBRC.00402000		     ; ASCII "fib(%d) = %d, not %d"
0040109C   50		    PUSH EAX
0040109D   E8 96000000	    CALL <JMP.&msvcrt.printf>		; printf
004010A2   83C4 10	    ADD ESP,10
004010A5   B8 01000000	    MOV EAX,1				; return 1
004010AA   E9 0A000000	    JMP FIBRC.004010B9
004010AF  >B8 00000000	    MOV EAX,0				; return 0
004010B4   E9 00000000	    JMP FIBRC.004010B9					; not great
004010B9   C9		    LEAVE
004010BA   C3		    RETN

*/
