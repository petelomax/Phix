--**
-- IRC client library
--

namespace irc

--include std/mem.e as m

enum isock, iserver, iport, ibuffer, ifullname, inickname

public enum by -1 OK = 0, DNS_FAILED, CONNECT_FAILED

--**
-- IRC type
--

public type irc(object o)
    if not atom(o) then
        return 0
    end if

    o = m:ram_space[o]
    if not sequence(o) then
        return 0
    end if

    if not socket(o[isock]) then
        return 0
    end if

    if not sequence(o[iserver]) then
        return 0
    end if

    if not integer(o[iport]) then
        return 0
    end if

    if not sequence(o[ibuffer]) then
        return 0
    end if

    if not sequence(o[ifullname]) then
        return 0
    end if

    if not sequence(o[inickname]) then
        return 0
    end if

    return 1
end type

--**
-- Connect to an IRC server
--

public function connect(sequence host, integer port, sequence fullname,
            sequence nickname)
    object addrinfo = host_by_name(host)
    if atom(addrinfo) or length(addrinfo) < 3 or length(addrinfo[3]) = 0 then
        return DNS_FAILED
    end if

    sequence i = { 0, host, port, {}, fullname, nickname }

    i[isock] = sock:create(sock:AF_INET, sock:SOCK_STREAM, 0)
    if sock:connect(i[isock], addrinfo[3][1], port) != sock:OK then
        return CONNECT_FAILED
    end if

    atom oirc = m:malloc()
    m:ram_space[oirc] = i

    return oirc
end function
