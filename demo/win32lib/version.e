-- version.e v0.1 by CChris <oedoc@free.fr>
-- adapted for inclusion into the win32lib package.

sequence s

constant slash='\\'

--/topic Version Info
--/const  notAVersion
--/desc A string that looks like a version, but is not a valid one.
constant notAVersion="0.0.0"

--/topic Version Info
--/func getEuVersion()
--/desc Returns a string, the version of the Euphoria integer used to run this file.
--/ret (SEQUENCE) A version strong, or /notAVersion if unavailable or irrelevant.
-- Bound or translated programs return /notAVersion. Programs executed from spurce 
-- might return "2.3" , "3.0.1", etc. If the version isn't reckoned, the function 
-- returns /notAVersion.
global function getEuVersion()
    integer p,q
    object s1

    s=command_line()
    if not compare(s[1],s[2]) then 
    -- the program is not running as source code, hence this is not needed
	return notAVersion
    end if
    s1=getenv("EUDIR")
    if atom(s1) or not length(s1) then
	s1=getenv("EUINC")
	if atom(s1) or not length(s1) then
	    s1=s[1]
	end if	  
	p=length(s1)
	while s1[p]!=slash do p-=1 end while
    else
	s1&=slash
	p=length(s1)
    end if
    s1=s1[1..p]&"readme.htm"
    if s1[1]='\"' then
	s1&='\"'
    end if 
    p=open(s1,"rb")
    if p=-1 then
	return notAVersion
    end if
    s1=get_bytes(p,1800)
    s1=s1[1600..1800] 
    close(p)
    p=match("version ",s1)  
    if p>0 then
	p+=8
	q=p   
	while find(s1[p],"0123456789.") do p+=1 end while
	return s1[q..p-1]
    else 
	return notAVersion
    end if
end function	
    
    
    
