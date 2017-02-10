--/topic Introduction
--Call functions via pointers on Windows and Linux
--<br>
--Matt Lewis<br>
--<a href="mailto:matthewwalkerlewis@yahoo.com">matthewwalkerlewis@yahoo.com</a><br>
--<a href="http://www14.brinkster.com/matthewlewis/projects.html">http://www14.brinkster.com/matthewlewis/projects.html</a>
--<br><br>
--/code
--
--Licence:            
--This software is provided 'as-is', without any express or implied warranty.
--In no event will the author be held liable for any damages arising from
--the use of this software.
--/endcode

include machine.e
include misc.e
include dll.e

without trace
without warning

constant
fptr_asm = {
    #60,                    --    0: pusha
    #BB,#00,#00,#00,#00,    --    1: mov ebx, paramcount (2)
    #B9,#00,#00,#00,#00,    --    6: mov ecx, params (7)
                            --    B: start: 
    #8B,#01,                --    B: mov eax, [ecx]
    #50,                    --    D: push eax
    #83,#C1,#04,            --    E: add ecx, 4
    #4B,                    --   11: dec ebx
    #75,#F7,                --   12: jnz start
    #FF,#15,#00,#00,#00,#00,--   14: call dword ptr [comfunc] (22)
    #A3,#00,#00,#00,#00,    --   1A: mov [retpointer], eax (27)
    #61,                    --   20: popa
    #C3},                   --   21: ret


fptr_paramcount = 2,
fptr_params = 7,
fptr_funcptr = 22+0,
fptr_retptr = 27+0,
--retval = allocate(4),

fptr_asm_addr = allocate( length( fptr_asm ) + 20 * 4 )

constant
fptr_func = fptr_asm_addr + 33,
fptr_retval = fptr_asm_addr + 37,
fptr_param_ptr = fptr_asm_addr + 41

poke( fptr_asm_addr, fptr_asm )
poke4( fptr_asm_addr + fptr_funcptr, fptr_func )
poke4( fptr_asm_addr + fptr_params, fptr_param_ptr )
poke4( fptr_asm_addr + fptr_retptr, fptr_retval )

--/topic Calling functions
--/func call_stdcall( atom fptr, sequence params )
--/ret Return value of fptr
--/desc Calls a function using stdcall
--Calls a function using stdcall
--Use this to call a function using the stdcall calling convention when you
--have a pointer to the function.  This is the most common convention in Windows.
--You can get the pointer to a function in a 
--.dll or .so using /b define_c_var() instead of /b define_c_func().  You should
--discard the return value if you call a routine that does not return a value.

procedure call_stdcall_proc( atom fptr, sequence params )
    -- store the pointer to the function
    poke4( fptr_func, fptr )

    -- reverse the params for stdcall calling convention
    params = reverse(params)

    -- store the params
    poke4( fptr_param_ptr, params )

    -- tell the asm how many params to push
    poke4( fptr_asm_addr + fptr_paramcount, length(params) )
    -- run the asm
    call( fptr_asm_addr )
end procedure

global function call_stdcall( atom fptr, sequence params )
    atom ret
    call_stdcall_proc( fptr, params )
    -- get the value returned from the function
    ret = peek4u( fptr_retval )
    return ret
end function

global function call_stdcall_s( atom fptr, sequence params )
    atom ret
    call_stdcall_proc( fptr, params )
    -- get the value returned from the function
    ret = peek4s( fptr_retval )
    return ret
end function

without trace

--constant 
--func_asm = {
--
--  #55,                    --    0: push ebp
--  #89,#E5,                --    1: mov ebp, esp
--  #51,                    --    3: push ecx
--  #B9,#00,#00,#00,#00,    --    4: mov ecx, argnum (5)
--  #89,#E6,                --    9: mov esi, esp
--  #83,#C6,#0C,            --    B: add esi, 12
--  #BF,#00,#00,#00,#00,    --    E: mov edi, myargs (15)
--  #F3,#A5,                --   13: rep movsd; next we call the proc
--
--  #FF,#15,#00,#00,#00,#00,--   15: call dword ptr [pfunc] (#17)
--
--  #59,                    --   1B: pop ecx
--  #89,#EC,                --   1C: mov esp, ebp
--  #5D,                    --   1E: pop ebp
--  #C2,#00,#00,#00,#00}    --   1F: ret [argnum] (#20)


--constant offset = length( func_asm )


constant
fptr_cdecl_asm = {
    #60,                    --    0: pusha
    #BB,#00,#00,#00,#00,    --    1: mov ebx, paramcount (2)
    #B9,#00,#00,#00,#00,    --    6: mov ecx, params (7)
                            --    B: start: 
    #8B,#01,                --    B: mov eax, [ecx]
    #50,                    --    D: push eax
    #83,#C1,#04,            --    E: add ecx, 4
    #4B,                    --   11: dec ebx
    #75,#F7,                --   12: jnz start
    #FF,#15,#00,#00,#00,#00,--   14: call dword ptr [comfunc] (22)
    #A3,#00,#00,#00,#00,    --   1A: mov [retpointer], eax (27)
    #83, #C4, 00,           --   1F: add esp 0
    #61,                    --   22: popa
    #C3},                   --   23: ret


fptr_cdecl_paramcount = 2,
fptr_cdecl_params = 7,
fptr_cdecl_funcptr = 22+0,
fptr_cdecl_retptr = 27+0,
--cdecl_retval = allocate(4),

fptr_cdecl_asm_addr = allocate( length( fptr_cdecl_asm ) + 20 * 4 )

constant
fptr_cdecl_func = fptr_cdecl_asm_addr + #24,
fptr_cdecl_retval = fptr_cdecl_asm_addr + #24 + 4,
fptr_cdecl_param_ptr = fptr_cdecl_asm_addr + #24 + 8

poke( fptr_cdecl_asm_addr, fptr_cdecl_asm )
poke4( fptr_cdecl_asm_addr + fptr_cdecl_funcptr, fptr_cdecl_func )
poke4( fptr_cdecl_asm_addr + fptr_cdecl_params, fptr_cdecl_param_ptr )
poke4( fptr_cdecl_asm_addr + fptr_cdecl_retptr, fptr_cdecl_retval )

--/topic Calling functions
--/func call_cdecl( atom fptr, sequence params )
--/ret Return value of fptr
--/desc Calls a function using cdecl
--Calls a function using stdcall
--Use this to call a function using the stdcall calling convention when you
--have a pointer to the function.  This is the most common calling convention in
--Linux.  You can get the pointer to a function in a 
--.dll or .so using /b define_c_var() instead of /b define_c_func().  You should
--discard the return value if you call a routine that does not return a value.

procedure call_cdecl_proc( atom fptr, sequence params )
    -- store the pointer to the function
    poke4( fptr_cdecl_func, fptr )

    -- reverse the params for stdcall calling convention
    params = reverse(params)

    -- store the params
    poke4( fptr_cdecl_param_ptr, params )

    -- tell the asm how many params to push/pop
    poke4( fptr_cdecl_asm_addr + fptr_cdecl_paramcount, length(params) )
    poke( fptr_cdecl_asm_addr + #21, length(params) * 4)
    
    -- run the asm
    call( fptr_cdecl_asm_addr )

end procedure

global function call_cdecl( atom fptr, sequence params )
    atom ret
    call_cdecl_proc( fptr, params )
    -- get the value returned from the function
    ret = peek4u( fptr_cdecl_retval )
    return ret
end function

global function call_cdecl_s( atom fptr, sequence params )
    atom ret
    call_cdecl_proc( fptr, params )
    -- get the value returned from the function
    ret = peek4s( fptr_cdecl_retval )
    return ret
end function

constant this_fptrasm =
{
    #60,                    --    0: pusha
    #BB,#00,#00,#00,#00,    --    1: mov ebx, paramcount (2)
    #B9,#00,#00,#00,#00,    --    6: mov ecx, params (7)
                            --    B: start: 
    #8B,#01,                --    B: mov eax, [ecx]
    #50,                    --    D: push eax
    #83,#C1,#04,            --    E: add ecx, 4
    #4B,                    --   11: dec ebx
    #75,#F7,                --   12: jnz start
                            --   14: end:
    #B9,#00,#00,#00,#00,    --   14: mov ecx, this (21)
    #FF,#15,#00,#00,#00,#00,--   19: call near dword ptr [comfunc] (27)
    #A3,#00,#00,#00,#00,    --   1F: mov [retpointer], eax (32)
    #61,                    --   24: popa
    #C3                     --   25: ret
},

this_fptrparamcount = 2,
this_fptrparams = 7,
this_fptrthis = 21,
this_fptrfuncptr = 27,
this_fptrretptr = 32,

this_fptrasm_addr = allocate( length( this_fptrasm ) + 20 * 4 )

constant
this_fptrfunc = this_fptrasm_addr + length(this_fptrasm),
this_fptrretval = this_fptrasm_addr + length(this_fptrasm) + 4,
this_fptrparam_ptr = this_fptrasm_addr + length(this_fptrasm) + 8

poke( this_fptrasm_addr, this_fptrasm )
poke4( this_fptrasm_addr + this_fptrfuncptr, this_fptrfunc )
poke4( this_fptrasm_addr + this_fptrparams, this_fptrparam_ptr )
poke4( this_fptrasm_addr + this_fptrretptr, this_fptrretval )

constant
this_noargs = {
    #60,                    --    0: pusha
    #B9,#00,#00,#00,#00,    --    1: mov ecx, this (2)
    #FF,#15,#00,#00,#00,#00,--    6: call near dword ptr [comfunc] (8)
    #A3,#00,#00,#00,#00,    --    C: mov [retpointer], eax (13)
    #61,                    --   11: popa
    #C3                     --   12: ret
},
this_noargs_this = 2,
this_noargs_fptr = 8,
this_noargs_retpointer = 13,

this_noargs_addr = allocate( length( this_noargs )  )


poke( this_noargs_addr, this_noargs )
poke4( this_noargs_addr + this_noargs_fptr, this_fptrfunc )
poke4( this_noargs_addr + this_noargs_retpointer, this_fptrretval )

--/topic Calling functions
--/func call_thiscall( atom fptr, atom this, sequence params )
--
--/li /i fptr: pointer to the function to call
--/li /i this: the 'this' pointer
--/li /i params: parameters for the function

procedure call_thiscall_proc( atom fptr, atom this, sequence params )
    -- store the pointer to the function
    poke4( this_fptrfunc, fptr )
    -- run the asm
    if length(params) then
        poke4( this_fptrasm_addr + this_fptrthis, this )
        -- reverse the params for stdcall calling convention
        params = reverse(params)
        -- store the params
        poke4( this_fptrparam_ptr, params )
        -- tell the asm how many params to push
        poke4( this_fptrasm_addr + this_fptrparamcount, length(params))
        call( this_fptrasm_addr )
    else
        poke4( this_noargs_addr + this_noargs_this, this )
        call( this_noargs_addr )
    end if
end procedure

global function call_thiscall( atom fptr, atom this, sequence params )
    call_thiscall_proc( fptr, this, params )
    return peek4u( this_fptrretval )
end function

global function call_thiscall_s( atom fptr, atom this, sequence params )
    call_thiscall_proc( fptr, this, params )
    return peek4s( this_fptrretval )
end function


constant stdcallback = {
    #55,                    --    0: push ebp
    #89,#E5,                --    1: mov ebp, esp
    #51,                    --    3: push ecx
    #83,#C5,#28,            --    4: add ebp, 40
    #55,                    --    7: push ebp
    #83,#ED,#28,            --    8: sub ebp, 40
    #C7,#C1,#08,#00,#00,#00,--    B: mov ecx, 8
    #89,#E6,                --   11: mov esi, esp
    #83,#C6,#10,            --   13: add esi, 16
    #83,#EC,#20,            --   16: sub esp, 32
    #89,#E7,                --   19: mov edi, esp
    #F3,#A5,                --   1B: rep movsd; next we call the proc
    #FF,#15,#00,#00,#00,#00,--   1D: call near dword ptr [pfunc] (31)
    #59,                    --   23: pop ecx
    #89,#EC,                --   24: mov esp, ebp
    #5D,                    --   26: pop ebp
    #C2,#24,#00,#00,#00,    --   27: ret 36
    #00,#00,#00,#00}        --   2C: func

constant cdeclcallback = {
    #55,                    --    0: push ebp
    #89,#E5,                --    1: mov ebp, esp
    #51,                    --    3: push ecx
    #C7,#C1,#08,#00,#00,#00,--    4: mov ecx, 8
    #89,#E6,                --   0A: mov esi, esp
    #83,#C6,#0C,            --   0C: add esi, 12
    #83,#EC,#20,            --   0F: sub esp, 32
    #89,#E7,                --   12: mov edi, esp
    #F3,#A5,                --   14: rep movsd; next we call the proc
    #FF,#15,#00,#00,#00,#00,--   16: call near dword ptr [pfunc] (31)
    #59,                    --   1C: pop ecx
    #89,#EC,                --   1D: mov esp, ebp
    #5D,                    --   1F: pop ebp
    #C3,#24,#00,#00,#00,    --   20: ret 36
    #00,#00,#00,#00}        --   25: func

--/topic Callbacks
--/func call_back_cdecl( integer rid, integer args )
--
--/li /i rid is the routine_id to the function for which you'd like to generate a call_back.
--/li /i args is the number of arguments that the function has
--<br><br>
--If you have a function which requires more than 9 arguments, 
--/b call_back_cdecl() will set up a callback for a function taking 9 arguments.  The
--9th argument will actually be a pointer to the actual 9th through the last arguments.
--/code
--ex:
--
--function my_function( atom a1, atom a2, atom a3, atom a4, atom a5, atom a6, atom a7,
--                      atom a8, atom ptr )
--    atom a9, a10
--    a9 = peek4u(ptr)
--    a10 = peek4u(ptr+4)
--    -- do stuff...
--    return 0
--end function
--my_callback = call_back_cdecl( routine_id("my_function"), 10 )
--/endcode

global function call_back_cdecl( integer rid, integer args )
    atom cb, asm_ptr

    cb = call_back( rid )

    if not args then
        return cb
    end if

    if args <= 9 then
        asm_ptr = allocate( length(cdeclcallback) )
        poke( asm_ptr, cdeclcallback )
        poke4( asm_ptr + 6, args )
        poke( asm_ptr + #11, (args+1) *4)
        poke4( asm_ptr + #18, asm_ptr + #21 )
        poke4( asm_ptr + #21, cb )
    else
        asm_ptr = allocate( length(stdcallback) )
        poke( asm_ptr, stdcallback )

        poke4( asm_ptr + #1F, asm_ptr + #2C )
        poke4( asm_ptr + #27, #C3 )
        poke4( asm_ptr + #2C, cb )
    end if

    return asm_ptr
end function

constant thiscall = {
    #55,                    --    0: push ebp
    #89,#E5,                --    1: mov ebp, esp
    #52,                    --    3: push edx
    #89,#CA,                --    4: mov edx, ecx
    #83,#C5,#24,            --    6: add ebp, 36
    #55,                    --    9: push ebp
    #83,#ED,#24,            --    A: sub ebp, 36
    #C7,#C1,#07,#00,#00,#00,--    D: mov ecx, 7
    #89,#E6,                --   13: mov esi, esp
    #83,#C6,#10,            --   15: add esi, 16
    #83,#EC,#1C,            --   18: sub esp, 28
    #89,#E7,                --   1B: mov edi, esp
    #F3,#A5,                --   1D: rep movsd; next we call the proc
    #52,                    --   1F: push edx
    #FF,#15,#00,#00,#00,#00,--   20: call near dword ptr [pfunc] (#22)
    #59,                    --   26: pop ecx
    #89,#EC,                --   27: mov esp, ebp
    #5D,                    --   29: pop ebp
    #C2,                    --   2A: ret
    #00,#00,#00,#00,        --   2B: argnum dd ? (43)
    #00,#00,#00,#00}        --   2F: pfunc dd ? (47)

--/topic Callbacks
--/func call_back_thiscall( integer rid, integer params )
--
--Returns a callback address for the routine with id /i rid, and /i params parameters,
--not including the /i this pointer.  The /i this pointer will be the first argument passed.
--Up to 8 parameters can be passed by value.  If there are more than 8 parameters, the ninth
--parameter will be a pointer to the 8th through last parameters.  Suppose you had a C++
--function prototype:
--/code
--int theClass::theMember( int x1, int x2, , int x3, int x4, int x5, int x6, int x7, int x8, int x9 )
--
---- then your Euphoria function should look like:
--function theClass_theMember( atom this, atom x1, atom x2, atom x3, atom x4, atom x5, atom x6, atom x7, atom ptr)
--    atom x8, x9
--    x8 = peek4s( ptr )
--    x9 = peek4s( ptr + 4 )
--    -- do stuff
--end function
--theClass_theMember_callback = call_back_thiscall( routine_id("theClass_theMember"), 9 )
--/endcode

global function call_back_thiscall( integer rid, integer params )
    atom cb, asm_ptr

    cb = call_back( rid )
    asm_ptr = allocate( length( thiscall) )
    poke( asm_ptr, thiscall )
    poke4( asm_ptr + 47, cb )
    poke4( asm_ptr + 43, params * 4 )
    poke4( asm_ptr + #22, asm_ptr + 47 )

    if params <= 8 then
        poke4( asm_ptr + #0F, params )
        poke4( asm_ptr + 43, (params) * 4)
        poke( asm_ptr + #1A, params * 4 )
    end if

    return asm_ptr
end function


--/topic Callbacks
--/func call_back_stdcall( integer rid, integer args )
--
--/li /i rid is the routine_id to the function for which you'd like to generate a call_back.
--/li /i args is the number of arguments that the function has
--<br><br>
--If you have a function with 9 or fewer arguments, this routine will return a normal
--Euphoria callback.  If you have a function which requires more than 9 arguments, 
--/b call_back_stdcall() will set up a callback for a function taking 9 arguments.  The
--9th argument will actually be a pointer to the actual 9th through the last arguments.
--/code
--ex:
--
--function my_function( atom a1, atom a2, atom a3, atom a4, atom a5, atom a6, atom a7,
--                      atom a8, atom ptr )
--    atom a9, a10
--    a9 = peek4u(ptr)
--    a10 = peek4u(ptr+4)
--    -- do stuff...
--    return 0
--end function
--my_callback = call_back_stdcall( routine_id("my_function"), 10 )
--/endcode

global function call_back_stdcall( integer rid, integer args )
    atom cb, asm_ptr
    cb = call_back( rid )

    if args <= 9 then
        return cb
    end if

    asm_ptr = allocate( length(stdcallback) + 20)
    poke( asm_ptr, stdcallback )

    poke4( asm_ptr + #2C, cb )
    poke4( asm_ptr + #1F, asm_ptr + #2C )
    
    return asm_ptr
end function


--/topic Calling functions
--/desc Calling other functions by pointer
--Calling other functions by pointer
--/info
--
--

--/topic Calling Conventions
--/desc Information about the different calling conventions supported
--Information about the different calling conventions supported
--/info
--
--There are 3 different calling conventions supported by /b fptr.e:
--<br><br>
--/li stdcall (standard calling convention)
--/li cdecl (C language calling convention)
--/li thiscall (MSVC++ default convention for C++ member functions)
--<br><br>
--All conventions push the parameters onto the stack in reverse order:
--/code
--func( 1, 2, 3 )
--  -- asm pseudocode
--  -- push 3
--  -- push 2
--  -- push 1
--  -- call func
--/endcode
--Both stdcall and thiscall require the called function to clean up the stack (move the
--stack pointer above the arguments--whenever you push something onto the stack the stack
--pointer is decreased by 4, not increased), meaning
--that they should be able to know (at compile time) how many arguments are being passed.
--It is possible (in C/C++) to have functions that take variable arguments.  Since the called
--function can't know how many arguments it will be passed, it cannot clean the stack, so
--the calling function must clean the stack.  
--<br><br>
--The thiscall convention also requires that the /i this pointer be passed in the /b ecx register.  
--This is a compiler specific convention, however, and is probably only used by MSVC++.  Most
--compilers (and the COM standard) simply pass the /i this pointer as the first argument.


--/topic Callbacks
--/desc Allowing your functions to be called by pointer
--Allowing your functions to be called by pointer
--/info
--
--
