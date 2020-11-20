--------------------NOTICE-------------------------------*
-- Software ID: w32tk.e
-- This is used to help the transition from the old toolkit
-- routines to the new "w32" prefixed ones.

without warning
without trace
include w32support.e
global object VOID

global function get_bits(atom b32)
    return w32get_bits(b32)
end function

global function signed_word(atom a)
    return w32signed_word(a)
end function

global function pack_word(integer low, integer high)
    return w32pack_word(low, high)
end function

global function lo_word(atom pData)
    return w32lo_word(pData)
end function

global function hi_word(atom pData)
    return w32hi_word(pData)
end function

--/*
global function abs( object a )
return w32abs(a)
end function
--*/

global function shortInt(atom i)
    return w32shortInt(i)
end function

global function TextToNumber(sequence text)
    return w32TextToNumber(text)
end function

--/*
global function or_all( object pData )
return w32or_all(pData)
end function
--*/

global function findKey(object key, sequence list)
    return w32findKey(key, list)
end function

global function removeIndex(integer index, sequence list)
    return w32removeIndex(index, list)
end function

--/*
global function iff( atom test, object ifTrue, object ifFalse )
return w32iff(test, ifTrue, ifFalse)
end function
--*/

global function removeItem(object item, sequence list)
    return w32removeItem(item, list)
end function

global function llSetAbort(integer i)
    return w32llSetAbort(i)
end function

global procedure manage_mem(atom pOwner, atom pAddr)
    w32manage_mem(pOwner, pAddr)
end procedure

global function acquire_mem(atom pOwner, object pData)
    return w32acquire_mem(pOwner, pData)
end function

global procedure release_mem(atom pData)
    w32release_mem(pData)
end procedure

integer xxxx
global procedure release_all_mem()
-- w32release_all_mem()
    xxxx = 1
end procedure

global function allot(object pDataType)
    return w32allot(pDataType)
end function

global function allotted_handle(sequence pHandle)
    return w32allotted_handle(pHandle)
end function

global function allotted_sofar()
    return w32allotted_sofar()
end function

global function allotted_size()
    return w32allotted_size()
end function

global procedure store(atom pStruct, sequence s, object o)
    w32store(pStruct,s,o)
end procedure

--/*
global function peek_string(atom a)
return w32peek_string(a)
end function
--*/

global function fetch(atom pStruct, sequence s)
    return w32fetch(pStruct,s)
end function

global function address(atom addr, object offset)
    return w32address(addr,offset)
end function

global function new_memset()
    return w32new_memset()
end function

--/*
global function trim(sequence pSource)
return w32trim(pSource)
end function
--*/

--/*
global function split(sequence pSource, object pDelim)
return w32split(pSource, pDelim)
end function
--*/

global function lookup(object pItem, sequence pSource, sequence pTarget)
    return w32lookup(pItem, pSource, pTarget)
end function

global function CType(integer pChar, object pSet)
    return w32CType(pChar, pSet)
end function

global function GetCType(object pChar)
    return w32GetCType(pChar)
end function

global procedure SetCType(object pChar, object pSet)
    w32SetCType(pChar,pSet)
end procedure

--/*
global constant False = 0, True = not 0
--*/

--/* Not Phix (now builtin)
global type int(object x)
    return integer(x)
end type

global type seq(object x)
    return sequence(x)
end type
--*/
