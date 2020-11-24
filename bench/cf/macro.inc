;NULL  = 0
TRUE  = 1
FALSE = 0

macro library [name,string]
 { forward
    local _label
    if ~ name#.needed
    dd 0,0,0,RVA _label,RVA name
    end if
   common
    dd 0,0,0,0,0
   forward
    if ~ name#.needed
    _label db string,0
    end if }

macro import name,[label,string]
 { common
    name:
   forward
    if used label
     local _label
     label dd RVA _label
    end if
   common
    if $ > name
     name#.needed = FALSE
     dd 0
    else
     name#.needed = TRUE
    end if
   forward
    if used label
    _label dw 0
	db string,0
    end if }

macro stdcall proc,[arg]	; call procedure
 { reverse
    pushd arg
   common
    call proc }

macro invoke proc,[arg] 	; invoke procedure (indirect)
 { common
    if ~ arg eq
     stdcall [proc],arg
    else
     call [proc]
    end if }

macro pushd arg
 { if _#arg in <_addr arg,_ADDR arg>
    local ..opcode,..address
    virtual at 0
     label ..address at arg
     mov eax,dword [..address]
     load ..opcode from 0
    end virtual
    if ..opcode = 0A1h
     push arg
    else
     lea edx,[..address]
     push edx
    end if
   else if arg eqtype ""
    local ..continue
    call ..continue
    db arg,0
    ..continue:
   else
    push arg
   end if }

macro .data { section '.data' data readable writeable }

macro .code { section '.text' code readable executable }

macro .end label
 {

   entry label

   section '.idata' import data readable writeable

     library kernel32,'KERNEL32.DLL',\
	     user32,'USER32.DLL',\
	     shell32,'SHELL32.DLL';,\
;	 gdi32,'GDI32.DLL',\
;	 advapi32,'ADVAPI32.DLL',\
;	 comctl32,'COMCTL32.DLL',\
;	 comdlg32,'COMDLG32.DLL',\
;	 wsock32,'WSOCK32.DLL',\
;	 crtdll,'crtdll.dll' 
;
	import kernel32,\
	AllocConsole,'AllocConsole',\
	CloseHandle,'CloseHandle',\
	CreateFile,'CreateFileA',\
	FileTimeToLocalFileTime,'FileTimeToLocalFileTime',\
	FileTimeToSystemTime,'FileTimeToSystemTime',\
	FillConsoleOutputAttribute,'FillConsoleOutputAttribute',\
	FillConsoleOutputCharacter,'FillConsoleOutputCharacterA',\
	FindClose,'FindClose',\
	FindFirstFile,'FindFirstFileA',\
	FindNextFile,'FindNextFileA',\
	FlushFileBuffers,'FlushFileBuffers',\
	FreeConsole,'FreeConsole',\
	GetConsoleScreenBufferInfo,'GetConsoleScreenBufferInfo',\
	GetCurrentDirectory,'GetCurrentDirectoryA',\
	GetCurrentThreadId,'GetCurrentThreadId',\
	GetEnvironmentVariable,'GetEnvironmentVariableA',\
	GetFileInformationByHandle,'GetFileInformationByHandle',\
	GetLastError,'GetLastError',\
	GetLocalTime,'GetLocalTime',\ 
	GetModuleHandle,'GetModuleHandleA',\
	GetProcAddress,'GetProcAddress',\
	GetProcessHeap,'GetProcessHeap',\
	GetStdHandle,'GetStdHandle',\
	GetTickCount,'GetTickCount',\
	HeapAlloc,'HeapAlloc',\
	HeapCreate,'HeapCreate',\
	HeapFree,'HeapFree',\
	HeapReAlloc,'HeapReAlloc',\
	HeapValidate,'HeapValidate',\
	LoadLibrary,'LoadLibraryA',\
	LockFile,'LockFile',\
	ReadConsole,'ReadConsoleA',\
	PeekConsoleInput,'PeekConsoleInputA',\
	ReadConsoleInput,'ReadConsoleInputA',\
	ReadFile,'ReadFile',\
	SetConsoleActiveScreenBuffer,'SetConsoleActiveScreenBuffer',\
	SetConsoleCursorPosition,'SetConsoleCursorPosition',\
	SetConsoleTextAttribute,'SetConsoleTextAttribute',\
	SetConsoleTitle,'SetConsoleTitleA',\
	SetCurrentDirectory,'SetCurrentDirectoryA',\
	SetEnvironmentVariable,'SetEnvironmentVariableA',\
	SetFilePointer,'SetFilePointer',\
	SetUnhandledExceptionFilter,'SetUnhandledExceptionFilter',\
	Sleep,'Sleep',\
	UnlockFile,'UnlockFile',\
	WriteFile,'WriteFile',\
	ExitProcess,'ExitProcess'

	import user32,\
	MessageBox,'MessageBoxA',\
	GetActiveWindow,'GetActiveWindow',\
	SetDlgItemText,'SetDlgItemTextA',\
	SetWindowsHookEx,'SetWindowsHookExA',\
	UnhookWindowsHookEx,'UnhookWindowsHookEx'

	import shell32,\
	ShellExecute,'ShellExecuteA';,\

}

STD_INPUT_HANDLE    = -10
STD_OUTPUT_HANDLE   = -11
HWND_DESKTOP	    = 0
MB_OK		    = 000000h

