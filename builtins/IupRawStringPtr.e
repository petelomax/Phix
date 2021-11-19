--
-- builtins\IupRawStringPtr.e
--
--  Moved out of pGUI.e for safe_mode handling. Not an autoinclude.
--
global function IupRawStringPtr(string s)
--
-- Returns a raw string pointer for s, somewhat like allocate_string(s), but using the existing memory.
-- NOTE: The return is only valid as long as the value passed as the parameter remains in scope.
--       In particular, callbacks must make a semi-permanent copy somewhere other than locals/temps.
--
atom res
    #ilASM{
        [32]
            mov eax,[s]
            lea edi,[res]
            shl eax,2
        [64]
            mov rax,[s]
            lea rdi,[res]
            shl rax,2
        []
            call :%pStoreMint
          }
    return res
end function

