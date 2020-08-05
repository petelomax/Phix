--
-- builtins\sockets.e
--
--  Incomplete, windows-only. (should be relatively straightforward to get this working on linux)
--
include cffi.e
--include ptypes.e (already included by cffi)

global constant SOCKET_ERROR = -1,
                AF_UNSPEC = 0,
                AF_UNIX = 1,
                AF_INET = 2,
--/*
AF_NETBIOS = 17 The NetBIOS address family. This address family is only supported if a Windows Sockets provider for NetBIOS is installed.
AF_INET6 = 23   The Internet Protocol version 6 (IPv6) address family.
AF_IRDA = 26    The Infrared Data Association (IrDA) address family. This address family is only supported if the computer has an infrared port and driver installed.
AF_BTH = 32,    The Bluetooth address family. This address family is only supported if a Bluetooth adapter is installed on Windows Server 2003 or later.
--*/ 
                SOCK_STREAM = 1,
                SOCK_DGRAM = 2,
--/* 
SOCK_DGRAM = 2  -- Supports datagrams, which are connectionless, unreliable buffers of a fixed (typically small) maximum length. 
                -- Uses the User Datagram Protocol (UDP) for the Internet address family (AF_INET or AF_INET6).
SOCK_RAW = 3    -- Provides a raw socket that allows an application to manipulate the next upper-layer protocol header. 
                -- To manipulate the IPv4 header, the IP_HDRINCL socket option must be set on the socket. 
                -- To manipulate the IPv6 header, the IPV6_HDRINCL socket option must be set on the socket. 
SOCK_RDM = 4    -- Provides a reliable message datagram. 
                -- An example of this type is the Pragmatic General Multicast (PGM) multicast protocol implementation in Windows, often referred to as reliable multicast programming. 
SOCK_SEQPACKET = 5  Provides a pseudo-stream packet based on datagrams.
--*/
                INVALID_SOCKET = #FFFFFFFF,
                INADDR_ANY = 0,
                INADDR_NONE = INVALID_SOCKET,
--/*
INADDR_LOOPBACK     = #7F000001
INADDR_BROADCAST    = #FFFFFFFF
--constant IPPROTO_TCP = 6
AI_PASSIVE = 0x01,                      -- The socket address will be used in a call to the bind function. 
AI_CANONNAME = 0x02,                    -- The canonical name is returned in the first ai_canonname member.
AI_NUMERICHOST = 0x04,                  -- The nodename parameter passed to the getaddrinfo function must be a numeric string. 
AI_ADDRCONFIG = 0x0400,                 -- The getaddrinfo will resolve only if a global address is configured.
                                        -- The IPv6 and IPv4 loopback address is not considered a valid global address.
                                        -- This option is only supported on Windows Vista or later. 
AI_NON_AUTHORITATIVE = 0x04000,         -- The address information can be from a non-authoritative namespace provider.
                                        -- This option is only supported on Windows Vista or later for the NS_EMAIL namespace.
AI_SECURE = 0x08000,                    -- The address information is from a secure channel.
                                        -- This option is only supported on Windows Vista or later for the NS_EMAIL namespace.
AI_RETURN_PREFERRED_NAMES = 0x010000,   -- The address information is for a preferred name for a user.
                                        -- This option is only supported on Windows Vista or later for the NS_EMAIL namespace.
AI_FILESERVER = 0x00040000,             -- A hint to the namespace provider that the hostname being queried is being used in file share scenario.
                                        -- The namespace provider may ignore this hint. 
--*/

-- structures --
tWSADATA = """
typedef struct WSAData {
  WORD           wVersion;
  WORD           wHighVersion;
  char           szDescription[WSADESCRIPTION_LEN+1];
  char           szSystemStatus[WSASYS_STATUS_LEN+1];
  unsigned short iMaxSockets;
  unsigned short iMaxUdpDg;
  char FAR       *lpVendorInfo;
} WSADATA, *LPWSADATA;""",
tsockaddr_in = """
typedef struct sockaddr_in {
        short   sin_family;
        u_short sin_port;
        u_long  sin_addr;
        char    sin_zero[8];
};""",
taddrinfo = """
typedef struct addrinfo {
  int             ai_flags;
  int             ai_family;
  int             ai_socktype;
  int             ai_protocol;
  size_t          ai_addrlen;
  char            *ai_canonname;
  struct sockaddr  *ai_addr;
  struct addrinfo  *ai_next;
} ADDRINFOA, *PADDRINFOA;""",
tTimeVal = """
typedef struct timeval {
  long tv_sec;
  long tv_usec;
} timeval;""",
id_sockaddr_in  = define_struct(tsockaddr_in),
socklen = get_struct_size(id_sockaddr_in),
sizePeerAddr    = allocate(machine_word()),
id_taddrinfo = define_struct(taddrinfo),
id_TimeVal = define_struct(tTimeVal),

-- functions --
tWSAStartup = """
int WSAStartup(
  _In_   WORD wVersionRequested,
  _Out_  LPWSADATA lpWSAData
);""",
tWSAGetLastError = """
int WSAGetLastError(void);""",
tSocket = """
SOCKET WSAAPI socket(
  _In_  int af,
  _In_  int type,
  _In_  int protocol
);""",
tHtons = """
u_short WSAAPI htons(
  _In_  u_short hostshort
);""",
tNtohs = """
u_short WSAAPI ntohs(
  _In_  u_short netshort
);""",
tHtonl = """
u_long WSAAPI htonl(
  _In_  u_long hostlong
);""",
tNtohl = """
u_long WSAAPI ntohl(
  _In_  u_long netlong
);""",
tGetSockName = """
int getsockname(
  _In_     SOCKET s,
  _Out_    struct sockaddr *name,
  _Inout_  int *namelen
);""",
tBind = """
int bind(
  _In_  SOCKET s,
  _In_  const struct sockaddr *name,
  _In_  int namelen
);""",
tConnect = """
int connect(
  _In_  SOCKET s,
  _In_  const struct sockaddr *name,
  _In_  int namelen
);""",
tListen = """
int listen(
  _In_  SOCKET s,
  _In_  int backlog
);""",
tAccept = """
SOCKET accept(
  _In_     SOCKET s,
  _Out_    struct sockaddr *addr,
  _Inout_  int *addrlen
);""",
tSelect = """
int select(
  _In_     int nfds,
  _Inout_  fd_set *readfds,
  _Inout_  fd_set *writefds,
  _Inout_  fd_set *exceptfds,
  _In_     const struct timeval *timeout
);""",
tSend = """
int send(
  _In_  SOCKET s,
  _In_  const char *buf,
  _In_  int len,
  _In_  int flags
);""",
tRecv = """
int recv(
  _In_   SOCKET s,
  _Out_  char *buf,
  _In_   int len,
  _In_   int flags
);""",
tCloseSocket = """
int closesocket(
  _In_  SOCKET s
);""",
tinet_addr = """
unsigned long inet_addr(
  _In_  const char *cp
);""",
tShutDown = """
int shutdown(
  _In_  SOCKET s,
  _In_  int how
);""",
tSetSockOpt = """
int setsockopt(
  _In_  SOCKET s,
  _In_  int level,
  _In_  int optname,
  _In_  const char *optval,
  _In_  int optlen
);""",
--/*
tGetAddrInfo = """
int WSAAPI getaddrinfo(
  _In_opt_  PCSTR pNodeName,
  _In_opt_  PCSTR pServiceName,
  _In_opt_  const ADDRINFOA *pHints,
  _Out_     PADDRINFOA *ppResult
);""",
tFreeAddrInfo = """
void freeaddrinfo(
  _In_  struct addrinfo *ai
);""",
--*/
tWSACleanup = """
int WSACleanup(void);""",
W = machine_word(),
lib = open_dll(iff(platform()=WINDOWS?"Ws2_32.dll":"libc.so")),
xSocket             = define_cffi_func(lib, tSocket),
xHtons              = define_cffi_func(lib, tHtons),
xNtohs              = define_cffi_func(lib, tNtohs),
xHtonl              = define_cffi_func(lib, tHtonl),
xNtohl              = define_cffi_func(lib, tNtohl),
xGetSockName        = define_cffi_func(lib, tGetSockName),
xBind               = define_cffi_func(lib, tBind),
xConnect            = define_cffi_func(lib, tConnect),
xListen             = define_cffi_func(lib, tListen),
xAccept             = define_cffi_func(lib, tAccept),
xSelect             = define_cffi_func(lib, tSelect),
xSend               = define_cffi_func(lib, tSend),
xRecv               = define_cffi_func(lib, tRecv),
xCloseSocket        = define_cffi_func(lib, tCloseSocket),
xinet_addr          = define_cffi_func(lib, tinet_addr),
xShutDown           = define_cffi_func(lib, tShutDown),
xSetSockOpt         = define_cffi_func(lib, tSetSockOpt),
--xGetAddrInfo      = define_cffi_func(lib, tGetAddrInfo),
--xFreeAddrInfo     = define_cffi_proc(lib, tFreeAddrInfo),
xGetHostByName      = define_c_func(lib,"gethostbyname",{C_PTR}, C_PTR)

--include builtins\sockerr.e
         
constant {ERROR_NO, ERROR_NAME, ERROR_SHORT} = columnize(
{{WSA_INVALID_HANDLE         :=     6,  "WSA_INVALID_HANDLE",           "Specified event object handle is invalid."},
 {WSA_NOT_ENOUGH_MEMORY      :=     8,  "WSA_NOT_ENOUGH_MEMORY",        "Insufficient memory available."},
 {WSA_INVALID_PARAMETER      :=    87,  "WSA_INVALID_PARAMETER",        "One or more parameters are invalid."},
 {WSA_IO_INCOMPLETE          :=   996,  "WSA_IO_INCOMPLETE",            "Overlapped I/O event object not in signaled state."},
 {WSA_IO_PENDING             :=   997,  "WSA_IO_PENDING",               "Overlapped operations will complete later."},
 {WSA_OPERATION_ABORTED      :=   995,  "WSA_OPERATION_ABORTED",        "Overlapped operation aborted."},
 {WSAEINTR                   := 10004,  "WSAEINTR",                     "Interrupted function call."},
 {WSAEBADF                   := 10009,  "WSAEBADF",                     "File handle is not valid."},
 {WSAEACCES                  := 10013,  "WSAEACCES",                    "Permission denied."},
 {WSAEFAULT                  := 10014,  "WSAEFAULT",                    "Bad address."},
 {WSAEINVAL                  := 10022,  "WSAEINVAL",                    "Invalid argument."},
 {WSAEMFILE                  := 10024,  "WSAEMFILE",                    "Too many open files."},
 {WSAEWOULDBLOCK             := 10035,  "WSAEWOULDBLOCK",               "Resource temporarily unavailable. "},
 {WSAEINPROGRESS             := 10036,  "WSAEINPROGRESS",               "Operation now in progress."},
 {WSAEALREADY                := 10037,  "WSAEALREADY",                  "Operation already in progress."},
 {WSAENOTSOCK                := 10038,  "WSAENOTSOCK",                  "Socket operation on nonsocket."},
 {WSAEDESTADDRREQ            := 10039,  "WSAEDESTADDRREQ",              "Destination address required."},
 {WSAEMSGSIZE                := 10040,  "WSAEMSGSIZE",                  "Message too long."},
 {WSAEPROTOTYPE              := 10041,  "WSAEPROTOTYPE",                "Protocol wrong type for socket."},
 {WSAENOPROTOOPT             := 10042,  "WSAENOPROTOOPT",               "Bad protocol option."},
 {WSAEPROTONOSUPPORT         := 10043,  "WSAEPROTONOSUPPORT",           "Protocol not supported."},
 {WSAESOCKTNOSUPPORT         := 10044,  "WSAESOCKTNOSUPPORT",           "Socket type not supported."},
 {WSAEOPNOTSUPP              := 10045,  "WSAEOPNOTSUPP",                "Operation not supported."},
 {WSAEPFNOSUPPORT            := 10046,  "WSAEPFNOSUPPORT",              "Protocol family not supported."},
 {WSAEAFNOSUPPORT            := 10047,  "WSAEAFNOSUPPORT",              "Address family not supported by protocol family."},
 {WSAEADDRINUSE              := 10048,  "WSAEADDRINUSE",                "Address already in use."},
 {WSAEADDRNOTAVAIL           := 10049,  "WSAEADDRNOTAVAIL",             "Cannot assign requested address."},
 {WSAENETDOWN                := 10050,  "WSAENETDOWN",                  "Network is down."},
 {WSAENETUNREACH             := 10051,  "WSAENETUNREACH",               "Network is unreachable."},
 {WSAENETRESET               := 10052,  "WSAENETRESET",                 "Network dropped connection on reset."},
 {WSAECONNABORTED            := 10053,  "WSAECONNABORTED",              "Software caused connection abort."},
 {WSAECONNRESET              := 10054,  "WSAECONNRESET",                "Connection reset by peer."},
 {WSAENOBUFS                 := 10055,  "WSAENOBUFS",                   "No buffer space available."},
 {WSAEISCONN                 := 10056,  "WSAEISCONN",                   "Socket is already connected."},
 {WSAENOTCONN                := 10057,  "WSAENOTCONN",                  "Socket is not connected."},
 {WSAESHUTDOWN               := 10058,  "WSAESHUTDOWN",                 "Cannot send after socket shutdown."},
 {WSAETOOMANYREFS            := 10059,  "WSAETOOMANYREFS",              "Too many references."},
 {WSAETIMEDOUT               := 10060,  "WSAETIMEDOUT",                 "Connection timed out."},
 {WSAECONNREFUSED            := 10061,  "WSAECONNREFUSED",              "Connection refused."},
 {WSAELOOP                   := 10062,  "WSAELOOP",                     "Cannot translate name."},
 {WSAENAMETOOLONG            := 10063,  "WSAENAMETOOLONG",              "Name too long."},
 {WSAEHOSTDOWN               := 10064,  "WSAEHOSTDOWN",                 "Host is down."},
 {WSAEHOSTUNREACH            := 10065,  "WSAEHOSTUNREACH",              "No route to host."},
 {WSAENOTEMPTY               := 10066,  "WSAENOTEMPTY",                 "Directory not empty."},
 {WSAEPROCLIM                := 10067,  "WSAEPROCLIM",                  "Too many processes."},
 {WSAEUSERS                  := 10068,  "WSAEUSERS",                    "User quota exceeded."},
 {WSAEDQUOT                  := 10069,  "WSAEDQUOT",                    "Disk quota exceeded."},
 {WSAESTALE                  := 10070,  "WSAESTALE",                    "Stale file handle reference."},
 {WSAEREMOTE                 := 10071,  "WSAEREMOTE",                   "Item is remote."},
 {WSASYSNOTREADY             := 10091,  "WSASYSNOTREADY",               "Network subsystem is unavailable."},
 {WSAVERNOTSUPPORTED         := 10092,  "WSAVERNOTSUPPORTED",           "Winsock.dll version out of range."},
 {WSANOTINITIALISED          := 10093,  "WSANOTINITIALISED",            "Successful WSAStartup not yet performed."},
 {WSAEDISCON                 := 10101,  "WSAEDISCON",                   "Graceful shutdown in progress."},
 {WSAENOMORE                 := 10102,  "WSAENOMORE",                   "No more results."},
 {WSAECANCELLED              := 10103,  "WSAECANCELLED",                "Call has been canceled."},
 {WSAEINVALIDPROCTABLE       := 10104,  "WSAEINVALIDPROCTABLE",         "Invalid procedure table from service provider."},
 {WSAEINVALIDPROVIDER        := 10105,  "WSAEINVALIDPROVIDER",          "Invalid service provider version number."},
 {WSAEPROVIDERFAILEDINIT     := 10106,  "WSAEPROVIDERFAILEDINIT",       "Unable to initialize a service provider."},
 {WSASYSCALLFAILURE          := 10107,  "WSASYSCALLFAILURE",            "System call failure."},
 {WSASERVICE_NOT_FOUND       := 10108,  "WSASERVICE_NOT_FOUND",         "Service not found."},
 {WSATYPE_NOT_FOUND          := 10109,  "WSATYPE_NOT_FOUND",            "Class type not found."},
 {WSA_E_NO_MORE              := 10110,  "WSA_E_NO_MORE",                "No more results."},
 {WSA_E_CANCELLED            := 10111,  "WSA_E_CANCELLED",              "Call was canceled."},
 {WSAEREFUSED                := 10112,  "WSAEREFUSED",                  "Database query was refused."},
 {WSAHOST_NOT_FOUND          := 11001,  "WSAHOST_NOT_FOUND",            "Host not found."},
 {WSATRY_AGAIN               := 11002,  "WSATRY_AGAIN",                 "Nonauthoritative host not found."},
 {WSANO_RECOVERY             := 11003,  "WSANO_RECOVERY",               "This is a nonrecoverable error."},
 {WSANO_DATA                 := 11004,  "WSANO_DATA",                   "Valid name, no data record of requested type."},
 {WSA_QOS_RECEIVERS          := 11005,  "WSA_QOS_RECEIVERS",            "QoS receivers."},
 {WSA_QOS_SENDERS            := 11006,  "WSA_QOS_SENDERS",              "QoS senders."},
 {WSA_QOS_NO_SENDERS         := 11007,  "WSA_QOS_NO_SENDERS",           "No QoS senders."},
 {WSA_QOS_NO_RECEIVERS       := 11008,  "WSA_QOS_NO_RECEIVERS",         "QoS no receivers."},
 {WSA_QOS_REQUEST_CONFIRMED  := 11009,  "WSA_QOS_REQUEST_CONFIRMED",    "QoS request confirmed."},
 {WSA_QOS_ADMISSION_FAILURE  := 11010,  "WSA_QOS_ADMISSION_FAILURE",    "QoS admission error."},
 {WSA_QOS_POLICY_FAILURE     := 11011,  "WSA_QOS_POLICY_FAILURE",       "QoS policy failure."},
 {WSA_QOS_BAD_STYLE          := 11012,  "WSA_QOS_BAD_STYLE",            "QoS bad style."},
 {WSA_QOS_BAD_OBJECT         := 11013,  "WSA_QOS_BAD_OBJECT",           "QoS bad object."},
 {WSA_QOS_TRAFFIC_CTRL_ERROR := 11014,  "WSA_QOS_TRAFFIC_CTRL_ERROR",   "QoS traffic control error."},
 {WSA_QOS_GENERIC_ERROR      := 11015,  "WSA_QOS_GENERIC_ERROR",        "QoS generic error."},
 {WSA_QOS_ESERVICETYPE       := 11016,  "WSA_QOS_ESERVICETYPE",         "QoS service type error."},
 {WSA_QOS_EFLOWSPEC          := 11017,  "WSA_QOS_EFLOWSPEC",            "QoS flowspec error."},
 {WSA_QOS_EPROVSPECBUF       := 11018,  "WSA_QOS_EPROVSPECBUF",         "Invalid QoS provider buffer."},
 {WSA_QOS_EFILTERSTYLE       := 11019,  "WSA_QOS_EFILTERSTYLE",         "Invalid QoS filter style."},
 {WSA_QOS_EFILTERTYPE        := 11020,  "WSA_QOS_EFILTERTYPE",          "Invalid QoS filter type."},
 {WSA_QOS_EFILTERCOUNT       := 11021,  "WSA_QOS_EFILTERCOUNT",         "Incorrect QoS filter count."},
 {WSA_QOS_EOBJLENGTH         := 11022,  "WSA_QOS_EOBJLENGTH",           "Invalid QoS object length."},
 {WSA_QOS_EFLOWCOUNT         := 11023,  "WSA_QOS_EFLOWCOUNT",           "Incorrect QoS flow count."},
 {WSA_QOS_EUNKOWNPSOBJ       := 11024,  "WSA_QOS_EUNKOWNPSOBJ",         "Unrecognized QoS object."},
 {WSA_QOS_EPOLICYOBJ         := 11025,  "WSA_QOS_EPOLICYOBJ",           "Invalid QoS policy object."},
 {WSA_QOS_EFLOWDESC          := 11026,  "WSA_QOS_EFLOWDESC",            "Invalid QoS flow descriptor."},
 {WSA_QOS_EPSFLOWSPEC        := 11027,  "WSA_QOS_EPSFLOWSPEC",          "Invalid QoS provider-specific flowspec."},
 {WSA_QOS_EPSFILTERSPEC      := 11028,  "WSA_QOS_EPSFILTERSPEC",        "Invalid QoS provider-specific filterspec."},
 {WSA_QOS_ESDMODEOBJ         := 11029,  "WSA_QOS_ESDMODEOBJ",           "Invalid QoS shape discard mode object."},
 {WSA_QOS_ESHAPERATEOBJ      := 11030,  "WSA_QOS_ESHAPERATEOBJ",        "Invalid QoS shaping rate object."},
 {WSA_QOS_RESERVED_PETYPE    := 11031,  "WSA_QOS_RESERVED_PETYPE",      "Reserved policy QoS element type."}}),
--
-- Some (known) more appropriate names on Linux, to be extended as needed
--  (nb ERROR_SHORT overrides deliberately omitted: we do not want any
--      routine-specific descriptions here, that's what google's for,
--      and in fact google searching is why this table exists at all.)
--
{MAPWSA,LNXNAMES} = columnize({
{WSAEWOULDBLOCK,        "EWOULDBLOCK"   },
{WSATRY_AGAIN,          "EAI_AGAIN"     },
{WSAEINVAL,             "EAI_BADFLAGS"  },
{WSANO_RECOVERY,        "EAI_FAIL"      },
{WSAEAFNOSUPPORT,       "EAI_FAMILY"    },
{WSA_NOT_ENOUGH_MEMORY, "EAI_MEMORY"    },
{WSAHOST_NOT_FOUND,     "EAI_NONAME"    },
{WSATYPE_NOT_FOUND,     "EAI_SERVICE"   },
{WSAESOCKTNOSUPPORT,    "EAI_SOCKTYPE"  }})

integer sock_init = false,
        xWSAStartup = 0,
        xWSAGetLastError = 0,
--      xErrno = 0,
        id_WSADATA

global function get_socket_error(integer err=SOCKET_ERROR)
    if err=SOCKET_ERROR then
        if platform()=WINDOWS then
            if xWSAGetLastError=0 then
                xWSAGetLastError = define_cffi_func(lib, tWSAGetLastError)
            end if
            err = c_func(xWSAGetLastError,{})
        else
            ?9/0 -- DEV tbc (obvs. the following blind stab is completely untested)
--          if xErrno=0 then
--              xErrno = define_c_var(lib,"errno")
--              if xErrno=0 then
--                  xErrno = define_c_var(lib,"__errno_location")
--              end if
--          end if
--          err = peek4s(xErrno)
--          err = peeknu(xErrno)
        end if
    end if
    integer edx = find(err,ERROR_NO)
    string id   = ERROR_NAME[edx],
          short = ERROR_SHORT[edx]
    if platform()=LINUX then
        -- convert (a few) to their more appropriate platform-specific names
        integer ldx = find(err,MAPWSA)
        if ldx then id = LNXNAMES[ldx] end if
    end if
    return {err,id,short}
end function

--DEV (rename back once die() [below] gone...)
procedure d2ie(string what, integer err=SOCKET_ERROR)
    crash("ERROR (%s): %v",{what,get_socket_error(err)})
end procedure

procedure init_sock()
    if platform()=WINDOWS then
        if xWSAStartup=0 then
            xWSAStartup = define_cffi_func(lib, tWSAStartup)
            id_WSADATA = define_struct(tWSADATA)
        end if
        integer res = c_func(xWSAStartup,{0x0202,allocate_struct(id_WSADATA)})
        if res!=0 then d2ie("WSAStartup",res) end if
    end if
    sock_init = true
end procedure

global procedure WSAStartup()
-- (This is handled for you automatically, should you forget. Does nowt on Linux.)
    if not sock_init then init_sock() end if
end procedure   

global function socket(integer af, socktype, pf=0)
--
-- Creates a new socket of the specified family and type.
--
-- Parameters:
--     af       - Address family, typically AF_INET for internet addresses.
--     socktype - Socket type, use SOCK_DGRAM for UDP or SOCK_STREAM for TCP.
--     pf       - Protocol family, set to zero and let Windows determine it
--                based on the address family.
-- Returns:
--     If successful, returns a pointer referencing the new socket.
--     Otherwise, INVALID_SOCKET is returned.
--
    if not sock_init then init_sock() end if
    atom sock = c_func(xSocket,{af, socktype, pf})
    return sock
end function

global function htonl(atom long)
-- Perform host to network type conversion on a long (4-byte) integer.
    long = c_func(xHtonl, {long})
    return long
end function

global function ntohl(atom long)
-- Perform network to host type conversion on a long (4-byte) integer.
    long = c_func(xNtohl,{long})
    return long
end function

global function htons(integer short)
-- Perform host to network type conversion on a short (2-byte) integer.
    short = c_func(xHtons, {short})
    return short
end function

global function ntohs(integer short)
-- Perform network to host type conversion on a short (2-byte) integer.
    short = c_func(xNtohs,{short})
    return short
end function

global function getsockname(atom hSocket)
    atom pSockAddr = allocate_struct(id_sockaddr_in),
         pSockLen = allocate_word(socklen,true),
         ret = c_func(xGetSockName, {hSocket, pSockAddr, pSockLen})
    if ret=SOCKET_ERROR then d2ie("getsockname") end if
    return pSockAddr
end function

global function getsockport(atom hSocket)
    atom pSockAddr = getsockname(hSocket)
    integer port = ntohs(get_struct_field(id_sockaddr_in,pSockAddr,"sin_port"))
    return port
end function

global function getsockaddr(atom hSocket)
    atom pSockAddr = getsockname(hSocket)
    atom addr = ntohl(get_struct_field(id_sockaddr_in,pSockAddr,"sin_addr"))
    return addr
end function

global function gethostbyname(string host)
--
-- Retrieves host information corresponding to a host name from a host database.
-- This function returns only the address portion of Winsock hostent structure.
-- (deprecated, should use getaddrinfo...)
--
    atom addr = INADDR_ANY
    if host!="" then
        atom pHostent = c_func(xGetHostByName, {host})
        if pHostent=0 then
            addr = SOCKET_ERROR
        else
            atom ppInAddr = peeknu(pHostent + W*3),
                 pInAddr = peeknu(ppInAddr) 
            addr = peeknu(pInAddr)
        end if
    end if
    return addr
end function

global function sockaddr_in(integer af=AF_INET, string host="", integer port=0)
--
-- Allocate and populate a sockaddr_in structure with an internet address and port number. 
-- This function calls gethostbyname() to convert a domain name or dotted decimal string 
-- into an address (/dword), and takes care of all host-to-network byte order conversions.
-- 
-- To use the IP address of the local host, pass an empty sequence as the host parameter. 
-- To automatically select an unused port on the local host, pass 0 as the port parameter.
--
-- Examples:
--     sockaddr_in(AF_INET,"www.rapideuphoria.com", 80)
--     sockaddr_in(AF_INET,"127.0.0.1", 8080) -- Port 8080 on the localhost
--     sockaddr_in(AF_INET,"", 0)             -- Any port on the localhost
-- 
-- Returns: 
--     A (pointer to a) sockaddr_in structure, or SOCKET_ERROR on failure.
--
    atom InAddr = gethostbyname(host)
    if InAddr=SOCKET_ERROR then return SOCKET_ERROR end if
    atom pSockAddr = allocate_struct(id_sockaddr_in)
    set_struct_field(id_sockaddr_in,pSockAddr,"sin_family",af)
    set_struct_field(id_sockaddr_in,pSockAddr,"sin_port",htons(port))
    set_struct_field(id_sockaddr_in,pSockAddr,"sin_addr",InAddr)
    return pSockAddr
end function

global function bind(atom sock, pSockAddr, integer len=socklen)
--
-- Associates a local address with a socket. Must be used on an unconnected 
-- socket before a subsequent call to the listen() function.
-- Returns zero or SOCKET_ERROR on failure.
--
    integer res = c_func(xBind,{sock,pSockAddr,len})
    return res
end function

--?{"socklen",socklen} -- 16 (on 32-bit)
global function connect(atom sock, pSockAddr, integer len=socklen)
--
-- Establishes a connection to a specified socket.
-- Returns zero if successful, otherwise returns SOCKET_ERROR.
--
    integer res = c_func(xConnect,{sock,pSockAddr,len})
    return res
end function

global function listen(atom sock, integer backlog)
--
-- Places a bound socket in a state in which it is listening for an incoming 
-- connection. 'backlog' specifies the maximum number of pending connections.
-- Returns zero if successful, otherwise returns SOCKET_ERROR.
--
    integer res = c_func(xListen, {sock, backlog})
    return res
end function

--global function accept(atom sock, pSockAddr=NULL, integer len=socklen)
global function accept(atom sock)
--
-- Permits an incoming connection attempt on a socket.
-- The returned value is a new socket on which the actual connection is made,
-- or INVALID_SOCKET in the case of error.
--
--  atom pLen = NULL
--  if pSockAddr!=NULL then
--      pLen = allocate_word(len,true)
--  end if
--  atom peer = c_func(xAccept, {sock, pSockAddr, pLen})
    atom peer = c_func(xAccept, {sock, NULL, NULL})
    return peer
end function

function allocate_fdset(sequence set)
    -- select() helper routine
    atom ptr = NULL
    if set!={} then
        ptr = allocate(length(set)*W+W)
        pokeN(ptr, length(set) & set,W)
    end if
    return ptr
end function

function allocate_timeout(object timeout)
    -- select() helper routine
    atom ptr = NULL
    if atom(timeout) then
        timeout = {floor(timeout/1000000),remainder(timeout,1000000)}
    end if
    if timeout!={} then
        if length(timeout)!=2 then ?9/0 end if
--      ptr = allocate(8,true)
--      poke4(ptr, timeout)
        ptr = allocate_struct(id_TimeVal)
        set_struct_field(id_TimeVal,ptr,"tv_sec",timeout[1])
        set_struct_field(id_TimeVal,ptr,"tv_usec",timeout[2])
    end if
    return ptr
end function

function get_fdset(atom ptr)
    -- select() helper routine
    sequence set = {}
    if ptr!=NULL then 
--      set = peekNS({ptr+W, peekNS(ptr,W,false)},W,false)
        set = peeknu({ptr+W, peeknu(ptr)})
        free(ptr)
    end if
    return set
end function

global function select(sequence read_set={}, write_set={}, error_set={}, object timeout={})
--
-- Check/wait on several sockets until one or more is ready to read or write or has an error.
--
-- read_set: sockets to check for readiness to read.
-- write_set: sockets to check for readiness to write.
-- error_set: sockets to check for errors.
-- timeout: in {seconds,microseconds} (or atom microseconds) format:
--          {0,0} (or 0) means "return immediately",
--          whereas {} means "wait forever".
--
-- returns: {integer result_code, sequence {read_set, write_set, error_set}}
-- result_code = SOCKET_ERROR (-1): An error occurred.
-- result_code = 0 : The time limit expired.
-- result_code > 0 : The total number of sockets that meet the criteria;
--                   sequences contain handles of the matching sockets.
--
-- Examples:
-- ========
--
-- Wait for 2.5 seconds or until sock1 or sock2 have something to read:
--
--      {code,{read_set}} = select({sock1,sock2}, {}, {}, {2,500000})
--
-- Check for errors on sock1 and return immediately:
--
--      {code} = select({}, {}, {sock1}, {0,0})
--      if code=1 then -- (obviously there is no need to disect the sets here)
--
-- Wait indefinitely for incoming sock1 data, or sock2 is ready to send:
--
--      {code,sets} = select({sock1},{sock2},{},{})
--      if code>0 then
--          {read_set,write_set} = sets
--          if length(read_set) then read(sock1) end if
--          if length(write_set) then write(sock2) end if
--
--   (obviously you could use {code,{read_set,write_set}} = select(), if preferred)
--
    atom pRead  = allocate_fdset(read_set),
         pWrite = allocate_fdset(write_set),
         pError = allocate_fdset(error_set),
         pTime  = allocate_timeout(timeout)

    integer ret_code = c_func(xSelect,{0, pRead, pWrite, pError, pTime})

    read_set = get_fdset(pRead)
    write_set = get_fdset(pWrite)
    error_set = get_fdset(pError)

    return {ret_code, {read_set, write_set, error_set}}
end function

global function recv(atom peer, maxlen=2048)
--
-- Receives data from a connected or bound socket.
-- Returns a sequence of the form {res, buffer} where
--     res = the actual number of bytes received or SOCKET_ERROR, and
--     buffer = a (binary) string containing the received bytes.
--
    string buffer = repeat('\0',maxlen)
    integer len = c_func (xRecv, {peer, buffer, maxlen, NULL})
    if len = SOCKET_ERROR then
        buffer = ""
    else
        buffer = buffer[1..len]
    end if
    return {len, buffer}
end function

global function send(atom peer, string message)
--
-- Sends data on a connected socket.
-- Returns the actual number of bytes transmitted, which may be less than the
-- total message length. Returns SOCKET_ERROR on failure.
--
    integer bytes_sent = c_func(xSend, {peer, message, length(message), 0})
-- maybe? (did not seem to make any difference:)
--  integer bytes_sent = c_func(xSend, {peer, message, length(message)+1, 0})
    return bytes_sent
end function

global constant SD_RECEIVE = 0, -- Shutdown receive operations.
                SD_SEND = 1,    -- Shutdown send operations.
                SD_BOTH = 2     -- Shutdown both send and receive operations.
 
global procedure shutdown(atom sock, integer how)
    integer res = c_func(xShutDown,{sock,how})
    if res!=0 then d2ie("shutdown") end if
end procedure

global procedure closesocket(atom sock)
--
-- Closes an existing socket.
-- Returns zero if successful, otherwise returns SOCKET_ERROR.
--
    if sock!=0 and sock!=SOCKET_ERROR then
        integer res = c_func(xCloseSocket, {sock})
        if res!=0 then d2ie("shutdown") end if
    end if
end procedure

integer xWSACleanup = 0

global procedure WSACleanup()
    if platform()=WINDOWS and sock_init then
        if xWSACleanup=0 then
            xWSACleanup = define_cffi_func(lib, tWSACleanup)
        end if
        integer res = c_func(xWSACleanup,{})
        if res=SOCKET_ERROR then d2ie("WSACleanup") end if
        sock_init = false
    end if
end procedure

global function inet_addr(string cp)
    if not sock_init then init_sock() end if
    atom res = c_func(xinet_addr,{cp})
--  if res=INADDR_NONE then d2ie("inet_addr") end if
    return res
end function

global function ip_to_string(atom ip)
-- (limited testing, but works for 0.0.0.0 and 127.0.0.1 anyway...)
    sequence res = repeat(0,4)
    for i=4 to 1 by -1 do
        res[i] = sprintf("%d",and_bits(ip,#FF))
        ip = floor(ip/#100)
    end for
    return join(res,".")
end function

global constant SOL_SOCKET = #FFFF,
                SO_RCVTIMEO = #1006

---- Top-level socket options
--global constant 
--SO_BROADCAST          = #20
--SO_CONDITIONAL_ACCEPT = #3002
--SO_DEBUG              = #1
--SO_DONTROUTE          = #10
--SO_KEEPALIVE          = #8
--SO_LINGER             = #80
--SO_OOBINLINE          = #100
--SO_RCVBUF             = #1002
--SO_REUSEADDR          = #4
--SO_SNDBUF             = #1001
--SO_SNDTIMEO           = #1005
--SO_USELOOPBACK        = #40

global procedure setsockopt(atom sock, integer level, optname, atom pOptVal, integer optlen)
    integer res = c_func(xSetSockOpt,{sock,level,optname,pOptVal,optlen})
    if res=SOCKET_ERROR then d2ie("setsockopt") end if
end procedure

---- Sets the current value for a socket option associated with a socket of any 
---- type, in any state. Only top-level socket options (SOL_SOCKET) are supported
---- here, to make things simple. Returns zero or SOCKET_ERROR on failure.
--global function setsockopt(atom sock, integer Option, object OptVal)
--  atom objLen, pValue, result
--  if atom(OptVal) then -- option is type integer or boolean
--      objLen = 4
--      pValue = allocate(objLen)
--      poke4(pValue,OptVal)
--  else -- option is a structure (option SO_LINGER only)
--      objLen = length(OptVal) 
--      pValue = allocate(objLen)
--      poke(pValue,OptVal)
--  end if
--  result = c_func(xSetOpt, {sock, SOL_SOCKET, Option, pValue, objLen})
--  free(pValue)
--  return result
--end function

--/*
global function new_addrinfo(integer family=NULL, socktype=0, flags=NULL)
--global function new_addrinfo(integer family=NULL, socktype=0, protocol=0, flags=NULL)
--
-- family: among others, AF_UNSPEC, AF_INET.
-- socktype: one of SOCK_STREAM, etc.
-- flags: combination of AI_PASSIVE .. AI_FILESERVER 
--
-- NB: you should NOT invoke freeaddrinfo(res), instead memory will be
--     automatically reclaimed when it drops out of scope or is
--     explicitly set to NULL.
--
    atom addrinfo = allocate_struct(id_taddrinfo)
    set_struct_field(id_taddrinfo,addrinfo,"ai_family",family)
    set_struct_field(id_taddrinfo,addrinfo,"ai_socktype",socktype)
--  set_struct_field(id_taddrinfo,addrinfo,"ai_protocol",protocol)
    set_struct_field(id_taddrinfo,addrinfo,"ai_flags",flags)
    return addrinfo
end function

global function getaddrinfo(nullable_string node_name, service_name, atom pHints, pResult)
    if not sock_init then init_sock() end if
    integer error = c_func(xGetAddrInfo,{node_name, service_name, pHints, pResult})
    return error
end function

global function get_ai_field(atom addrinfo, string field)
    atom res = get_struct_field(id_taddrinfo,addrinfo,field)
    return res
end function

global procedure freeaddrinfo(atom pResult)
--  if not sock_init then init_sock() end if
    c_proc(xFreeAddrInfo,{pResult})
end procedure
--*/

--/*
global function get_sin_port(atom sock_addr)
    atom ip = get_struct_field(id_sockaddr_in,sock_addr,"sin_port")
    return ip
end function
global function get_sin_addr(atom sock_addr)
    atom ip = get_struct_field(id_sockaddr_in,sock_addr,"sin_addr")
    return ip
end function
--*/

--/*
--constant 
--xSendTo  = define_c_func(WS2,"sendto",{C_INT, C_POINTER, C_INT, C_INT, C_POINTER, C_INT}, C_INT)
--xGetPeer = define_c_func(WS2,"getpeername",{C_INT, C_POINTER, C_POINTER}, C_INT)

-------------------------------------------------------------------------------
-- getpeername()
--
-- Retrieves the name of the peer to which a socket is connected.
-- This does not work for unconnected datagram sockets.
--
-- Returns: 
--     A sequence representing the peer address (struct sockaddr_in)
--     or SOCKET_ERROR on failure.
-------------------------------------------------------------------------------
--**DEV see getsockname...
--global function getpeername(integer hSocket)
--  atom name = allocate(SIZEOF_SOCKADDR_IN,true),
--       namelen = allocate(4,true)
--  poke4(namelen, SIZEOF_SOCKADDR_IN)
--  object retval = c_func (xGetPeer, {hSocket, name, namelen})
--  if retval != SOCKET_ERROR then
--      retval = peek({name, peek4u(namelen)}) 
--  end if
--  return retval
--end function

-------------------------------------------------------------------------------
-- sendto()
-- Sends data on an unconnected socket to a specific destination.
-- Returns the actual number of bytes transmitted, or SOCKET_ERROR on failure.
-------------------------------------------------------------------------------
--global function sendto(integer Socket, string message, integer Flags, sequence InetSockAddr)
--  atom AddrPtr = allocate(SIZEOF_SOCKADDR_IN,true)
--  poke(AddrPtr, InetSockAddr)
--  return c_func( xSendTo, {Socket, message, length(message), Flags, AddrPtr, SIZEOF_SOCKADDR_IN} )
--end function

--*/

--/*
This may be helpful. Linux 64-bit, but untested by me, from https://rosettacode.org/wiki/Echo_server#X86_Assembly
 
; x86_64 Linux NASM
 
global _start
 
--%define AF_INET 2
--%define SOCK_STREAM 1
%define default_proto 0
%define sol_sock 1
%define reuse_addr 2
%define reuse_port 15
%define server_port 9001
%define addr_any 0
%define family_offset 0
%define port_offset 2
%define addr_offset 4
%define unused_offset 8
%define addr_len 16
%define buffer_len 64
%define max_connections 3
 
 
section .text
 
; rdi - 16 bit value to be byte swapped
; return - byte swapped value
htn_swap16:
 
  xor rax, rax
  mov rdx, 0x000000ff
 
  mov rsi, rdi
  and rsi, rdx
  shl rsi, 8
  or rax, rsi
  shl rdx, 8
 
  mov rsi, rdi
  and rsi, rdx
  shr rsi, 8
  or rax, rsi
  ret
 
; return - server socket
create_server_socket:
 
  mov rax, 41
  mov rdi, AF_INET
  mov rsi, SOCK_STREAM
  mov rdx, default_proto
  syscall
  push rax
 
  mov rax, 54
  mov rdi, qword [rsp]
  mov rsi, sol_sock
  mov rdx, reuse_addr
  mov qword [rsp - 16], 1
  lea r10, [rsp - 16]
  mov r8, 4
  syscall
 
  mov rax, 54
  mov rdi, qword [rsp]
  mov rsi, sol_sock
  mov rdx, reuse_port
  mov qword [rsp - 16], 1
  lea r10, [rsp - 16]
  mov r8, 4
  syscall
 
 
  pop rax
  ret
 
; rdi - socket
; rsi - port
; rdx - connections
; return - void
bind_and_listen:
 
  push rdi
  push rdx
 
  mov rdi, rsi
  call htn_swap16
 
  lea rsi, [rsp - 16]
  mov word [rsi + family_offset], AF_INET
  mov word [rsi + port_offset], ax
  mov dword [rsi + addr_offset], addr_any
  mov qword [rsi + unused_offset], 0
 
  mov rax, 49
  mov rdi, qword [rsp + 8]
  mov rdx, addr_len
  syscall
 
  mov rax, 50
  pop rsi
  pop rdi
  syscall
  ret
 
; rdi - server socket
; return - client socket
accept:
 
  mov rax, 43
  lea rsi, [rsp - 16]
  lea rdx, [rsp - 24]
  syscall
  ret
 
; rdi - client socket
; return - void
echo:
 
  push rdi
  mov rax, 0
  lea rsi, [rsp - 104]
  mov rdx, buffer_len
  syscall
 
  pop rdi
  mov rdx, rax 
  lea rsi, [rsp - 112]
  mov rax, 1
  syscall
  ret
 
 
_start:
 
  call create_server_socket
  mov r14, rax
 
  mov rdi, rax
  mov rsi, server_port
  mov rdx, max_connections
  call bind_and_listen
 
accept_connection:
 
  mov rdi, r14
  call accept
 
  mov r15, rax
  mov rax, 57
  syscall
 
  test rax, rax
  jz handle_connection
 
  ; close client socket
  mov rax, 3
  mov rdi, r15
  syscall
  jmp accept_connection
 
handle_connection:
 
  mov rdi, r15
  call echo
 
  close_client:
    mov rax, 3
    mov rdi, r15
    syscall
 
  close_server:
    mov rax, 3
    mov rdi, r14
    syscall
 
  exit:
    mov rax, 60
    xor rdi, rdi
    syscall
     
--*/

--/*
-- Simple Sockets Lib by jbrown
-- Access to unix sockets and internet protocols
-- based on Simple Sockets Demo by Irv Mullins
-- server code from RDC.e by Pete Eberlein

include get.e
include dll.e
include misc.e
include machine.e
without warning

constant DEBUG = 0 -- 0 = off, 1 = on

constant
--   AF_UNIX = 1, AF_INET = 2,      -- DOMAINS;
--   SOCK_STREAM = 1, SOCK_DGRAM = 2, -- SOCKET TYPES;
   F_SETFL = 4, O_NONBLOCK = 0o04000, -- PORT MODE PARAMS;
--   INADDR_ANY = 0,
   F_DUPFD = 0, F_GETFD = 1,
   F_SETFD = 2, F_GETFL = 3,
   O_SYNC = 0o010000, O_ASYNC = 0o020000,
   EAGAIN = 11

global constant
SockURL = 1, SockURI = 2, SockNODE = 3, -- host info
SockSERVICE = 1, SockPORT = 2, SockPROTOCOL = 3 -- service info

-- Links to routines in the socks library

function redo(sequence s, object bad)
    for i=1 to length(s)-1 do
        if s[i]!=bad then
            return s[i]
        end if
    end for
    return s[length(s)]
end function

constant
--Linux is ok with "" but FreeBSD might want libc.so
--lib = redo({open_dll(""), open_dll("libc.so")}, 0),
fcntl = define_c_func(lib,"fcntl",{C_POINTER,C_INT,C_INT},C_POINTER),
--sock  = define_c_func(lib,"socket",{C_INT,C_INT,C_INT},C_POINTER),
--htons = define_c_func(lib,"htons",{C_USHORT},C_USHORT),
--htonl = define_c_func(lib,"htonl",{C_ULONG},C_ULONG),
--connect = define_c_func(lib,"connect",{C_INT,C_POINTER,C_INT},C_INT),
read_ = define_c_func(lib,"read",{C_INT,C_POINTER,C_INT},C_INT),
write_ = define_c_func(lib,"write",{C_INT,C_POINTER,C_INT},C_INT),
close_ = define_c_proc(lib,"close",{C_INT}),
--gethostbyname = define_c_func(lib,"gethostbyname",{C_POINTER},C_POINTER),
getservbyname = define_c_func(lib,"getservbyname",{C_POINTER,C_POINTER},C_POINTER),
--bind_ = define_c_func(lib,"bind",{C_INT,C_POINTER,C_INT},C_INT),
--listen = define_c_func(lib,"listen",{C_INT,C_INT},C_INT),
poll = define_c_func(lib,"poll",{C_POINTER,C_UINT,C_INT},C_INT),
--accept = define_c_func(lib,"accept",{C_INT,C_POINTER,C_POINTER},C_INT),
--some systems have errno as a variable, others have it as a macro to
--the function __errno_location (usually threaded libc).
errno = redo({define_c_var(lib,"errno"),define_c_var(lib,"__errno_location")}, 0)

function deallocate_string(atom str)
    object result
    integer i
    i = 0
    result = ""
    while peek(str+i)!=0 do
        result &= peek(str+i)
        i += 1
    end while
    return result
end function

sequence allocs allocs = {}
function fast_allocate(integer a)
    allocs &= allocate(a)
    return allocs[length(allocs)]
end function
procedure fast_free(integer n)
    if n>1 then
        for i=1 to n do --free n blocks, one after the other
            fast_free(1)
        end for
    end if
    if not length(allocs) then return end if
    if allocs[length(allocs)] then
        free(allocs[length(allocs)])
    end if
    allocs = allocs[1..length(allocs)-1]
end procedure
function fast_allocate_string(sequence s)
    allocs &= allocate_string(s)
    return allocs[length(allocs)]
end function

global function SockRead(atom sockfd)
    atom buffer, char, bytes_read
    object line
    buffer = fast_allocate(1)
    line = ""
    while 1 do
        --modifed for non-blocking IO - we can check if its empty
        if c_func(fcntl, {sockfd, F_SETFL, O_NONBLOCK}) then end if
        bytes_read = c_func(read_,{sockfd,buffer,1})
        if bytes_read<1 then
            if peek4s(errno)!=EAGAIN then
                if DEBUG then
                    puts(DEBUG,"READ: errno is "&sprint(peek4s(errno))&'\n')
                end if
            end if
            exit --no data left, or had an error, either way, return
        else
            char = peek(buffer)
            if char=10 then
                char = '\n'
            elsif char=13 then
                char = ' '
            end if
            line &= char
        end if
    end while
    if DEBUG then
        puts(DEBUG,"READ: "&line&'\n')
    end if
    fast_free(1)
    return line
end function

global function SockReadAndWait(atom sockfd)
    atom buffer, char, bytes_read
    object line
    buffer = fast_allocate(1)
    line = ""
    while 1 do
        bytes_read = c_func(read_,{sockfd,buffer,1})
        if bytes_read<1 then
            exit --no data left, or had an error, either way, return
        else
            char = peek(buffer)
            if char=10 then
                char = '\n'
            elsif char=13 then
                char = ' '
            end if
            line &= char
        end if
    end while
    if DEBUG then
        puts(DEBUG,"READ: "&line&'\n')
    end if
    fast_free(1)
    return line
end function

global procedure SockWrite(atom sockfd, string str)
    atom fn
    str &= 13&10
    --modifed for async IO - dont wait for data to be sent
    if c_func(fcntl, {sockfd, F_SETFL, O_ASYNC}) then end if
    fn = c_func(write_,{sockfd,str,length(str)})
    if DEBUG then
        puts(DEBUG,"WRITE: "&str&'\n')
    end if
end procedure

global procedure SockWriteAndWait(atom sockfd, string str)
    atom fn
    str &= 13&10
    fn = c_func(write_,{sockfd,str,length(str)})
    if DEBUG then
        puts(DEBUG,"WRITE: "&str&'\n')
    end if
end procedure

global procedure SockClose(atom sockfd)
    c_proc(close_,{sockfd})
end procedure

global function SockGetHostbyName(string hostname)
-- Returns {name, #addr, node}
    object host
    atom str, fn
    object addr
-- host info structure is;
    object name,
        aliases,
        addrtype,
        length,
        node

    str = fast_allocate_string(hostname)
    host = c_func(gethostbyname,{str})
    fast_free(1)

    if host>0 then
        name = deallocate_string(peek4u(host))
        length = peek4u(host+12)
        addr = peek4u(peek4u(host+16))
        node = peek4u(addr)
        addr = sprintf("%d.%d.%d.%d",peek({addr,length}))
        return {name,addr,node}
    else
        return -1
    end if
end function

global function SockGetServbyName(string service, proto)
-- Returns {name, #port, protocol}
    atom str1, str2, fn
    -- server info structure;
    object name,
        port,
        protocol

    str1 = fast_allocate_string(service)
    str2 = fast_allocate_string(proto)
    fn = c_func(getservbyname,{str1,str2})
    fast_free(2)

    if fn>0 then
        name = deallocate_string(peek4u(fn))
        port = peek({fn+8,4})
        port = port[1]*256+port[2]
        protocol = deallocate_string(peek4u(fn+12))
        return {name,port,protocol}
    else
        return -1
    end if
end function

global function SockConnect(atom hostnode, port)
    atom sockaddr, SOCKET

    SOCKET = c_func(sock,{AF_INET, SOCK_STREAM,0})
    if SOCKET<0 then
        return -1
    else
        sockaddr = fast_allocate(16) -- thanks Pete Eberlein!
        poke4(sockaddr, {
                         AF_INET+
                         c_func(htons,{port})*#10000,
                         hostnode,0,0})
    end if
    if c_func(connect,{SOCKET, sockaddr,16})=0 then
        fast_free(1)
        return SOCKET
    else
        fast_free(1)
        SockClose(SOCKET)
        return -1
    end if
end function

global function HttpGetv10(string hostname, filename)
    object host, service, file, SOCKET

    host = SockGetHostbyName(hostname)
    if atom(host) then
        return -1
    elsif DEBUG then
        printf(DEBUG,"Host: %s Addr: %s  Node: %d\n",host)
    end if

    service = SockGetServbyName("http","tcp")
    if atom(service) then
        return -2
    else
        SOCKET = SockConnect(host[SockNODE],service[SockPORT])
        if SOCKET<0 then
            return -3
        end if
        if DEBUG then
            printf(DEBUG,"Service: %s Port: %d Protocol: %s\n",service)
        end if
    end if

    SockWrite(SOCKET, sprintf("GET /%s HTTP/1.0\n\n",{filename}))
    file = SockRead(SOCKET)
    SockClose(SOCKET)
    return file
end function

global function HttpGetv11(string hostname, filename)
    object host, service, file, SOCKET

    host = SockGetHostbyName(hostname)
    if atom(host) then
        return -1
    elsif DEBUG then
        printf(DEBUG,"Host: %s Addr: %s  Node: %d\n",host)
    end if

    service = SockGetServbyName("http","tcp")
    if atom(service) then
        return -2
    else
        SOCKET = SockConnect(host[SockNODE],service[SockPORT])
        if SOCKET<0 then
            return -3
        end if
        if DEBUG then
            printf(DEBUG,"Service: %s Port: %d Protocol: %s\n",service)
        end if
    end if

    SockWrite(SOCKET, sprintf("GET /%s HTTP/1.1\nHost: %s\n\n",
                              {filename,hostname}))
    file = SockRead(SOCKET)
    SockClose(SOCKET)
    return file
end function

global function CreateServSocket(integer port, queue)
    integer sd
    atom servsock

    sd = c_func(sock, {AF_INET, SOCK_STREAM, 0})
    if sd= -1 then
        return -1
    end if

    servsock = fast_allocate(16)
    poke4(servsock, {AF_INET+c_func(htons, {port})*#10000,
    c_func(htonl, {INADDR_ANY}), 0, 0})

    if c_func(bind_, {sd, servsock, 16})= -1 then
        fast_free(1)
        return -2
    end if
    fast_free(1)

    if c_func(listen, {sd, 20})= -1 then
        return -3
    end if

    return sd
end function

global function SockPoll(sequence fds, integer timeout)
    sequence vfds
    atom fds_

    fds_ = fast_allocate(8*length(fds))
    for i=1 to length(fds) do
        poke4(fds_+i*8-8, fds[i])
        poke4(fds_+i*8-4, #00010001)
    end for

    if c_func(poll, {fds_, length(fds), timeout})= -1 then
        fast_free(1)
        return -1
    end if

    vfds = repeat(0, length(fds))
    for i=1 to length(fds) do
        if peek(fds_+i*8-2) then
            vfds[i] = 1
        else
            vfds[i] = 0
        end if
    end for
    fast_free(1)

    return vfds
end function

--global function SockAccept(integer sd)
--  atom clientsock, intp
--  integer cd
--
--  clientsock = fast_allocate(100)
--  intp = fast_allocate(8)
--  cd = c_func(accept, {sd, clientsock, intp})
--  fast_free(2)
--
--  if cd= -1 then
--      return -1
--  end if
--
--  return cd
--end function

global procedure ServerLoop(integer sd, timeout, handle_client_id)
    sequence cds, vds, del
    --expects to loop forever, handles new client connections, calls
    --handle_client_id to tkae care of client connections when new data
    --is received, handle_client_id returns 0 to if client socket is still open
    --and 1 if it was closed.

    cds = {sd}
    if DEBUG then
        printf(1, "Primary connection: %d\n", cds[1])
    end if
    del = {}
    while 1 do
        vds = SockPoll(cds, timeout)
        for i=1 to length(vds) do
            if vds[i] then
                if i=1 then
                    cds &= SockAccept(cds[i])
                    if DEBUG then
                        printf(1, "Accepted connection: %d\n", cds[length(cds)])
                    end if
                else
                    if call_func(handle_client_id, {cds[i]}) then
                        del &= i
                    end if
                end if
            end if
        end for
        for i=length(del) to 1 by -1 do --backwards to avoid counting errors
            cds = cds[1..del[i]-1] & cds[del[i]+1..length(cds)]
        end for
        del = {}
    end while
end procedure
--*/

--/*
#!/home/irv/euphoria/bin/exu
----------------------------------------------------------
-- Simple Sockets Demo               Retrieves a web page
----------------------------------------------------------
-- USAGE: socks hostname filename [local file]
-- if local file is omitted, retrieved text will be
-- displayed on screen.
----------------------------------------------------------

include get.e
include dll.e
include misc.e
include machine.e
without warning

constant DEBUG = 1 -- 0 = none, 1 = minor, 2 = major

constant
--   AF_UNIX = 1, AF_INET = 2,      -- DOMAINS;
--   SOCK_STREAM = 1, SOCK_DGRAM = 2, -- SOCKET TYPES;
   F_SETFL = 4, O_NONBLOCK = 04000, -- PORT MODE PARAMS;
   P = C_POINTER, I = C_INT         -- MINOR CONVENIENCES;

--type socket(object x)
--  return x> -1
--end type

------------------------------------------------------------
-- Links to routines in the socks library        SOCKS LINKS
------------------------------------------------------------

--socket lib lib = open_dll("")
socket SOCKET

socket fcntl   fcntl = define_c_func(lib,"fcntl",{P,I,I},P)
--socket sock    sock  = define_c_func(lib,"socket",{I,I,I},P)
--socket htons   htons = define_c_func(lib,"htons",{C_USHORT},C_USHORT)
--socket connect connect = define_c_func(lib,"connect",{I,P,I},I)
socket read_   read_ = define_c_func(lib,"read",{I,P,I},I)
socket write_  write_ = define_c_func(lib,"write",{I,P,I},I)
socket close_  close_ = define_c_proc(lib,"close",{I})
--socket gethostbyname gethostbyname = define_c_func(lib,"gethostbyname",{P},P)
socket getservbyname getservbyname = define_c_func(lib,"getservbyname",{P,P},P)

--------------------------------------------------------------
function read(atom sockfd)                    -- socket read
--------------------------------------------------------------
-- reads from a socket until no more bytes remain to   --
-- be read, then returns the input as a text sequence. --
atom buffer, char, bytes_read
object line
    buffer = allocate(1)
    line = ""
    while true do
        bytes_read = c_func(read_,{sockfd,buffer,1})
        if bytes_read<1 then exit end if
        char = peek(buffer)
        if char=10 then
            char = '\n'
        elsif char=13 then
            char = ' '
        end if
        line &= char
    end while
    if DEBUG=3 then
        puts(1,"DEBUG:READ:"&line&'\n')
    elsif DEBUG=1 then
        puts(1,sprintf("DEBUG: READ %d bytes\n",length(line)))
    end if
    return line
end function

--------------------------------------------------------------
procedure write(atom sockfd, string str) -- socket write
--------------------------------------------------------------
-- outputs string to a socket, CR/LF appended automatically --
    atom fn = c_func(write_,{sockfd,str,length(str)})
    if DEBUG=2 then
        puts(1,"DEBUG:WRITE:"&str & '\n')
    end if
end procedure

--------------------------------------------------------------
function GetHostbyName(sequence hostname)                   --
-- Returns {name, #addr, node}                              --
--------------------------------------------------------------
object host
atom str, fn

-- host info structure is;
   object name,
          aliases,
          addrtype,
          len,
          node,
          addr

    host = c_func(gethostbyname,{hostname})

    if host>0 then
        name = peek_string(peek4u(host))
        len  = peek4u(host+12)
        addr = peek4u(peek4u(host+16))
        node = peek4u(addr)
        addr = sprintf("%d.%d.%d.%d",peek({addr,len}))
        return {name,addr,node}
    else
        return -1
    end if

end function

------------------------------------------------------------
function GetServbyName(string service, string proto)      --
-- Returns {name, #port, protocol}                        --
------------------------------------------------------------

-- server info structure;
   object name,
          port,
          protocol

    atom fn = c_func(getservbyname,{service,proto})
    if fn>0 then
        name = peek_string(peek4u(fn))
        port = peek({fn+8,4})
        port = port[1]*256+port[2]
        protocol = peek_string(peek4u(fn+12))
        return {name,port,protocol}
    else
        return -1
    end if
end function

-----------------------------------------------------------
function Connect(atom hostnode, atom port)
-----------------------------------------------------------
atom sockaddr

    ----------------------------------------------
    -- attempt to create a new socket           --
    ----------------------------------------------
    SOCKET = c_func(sock,{AF_INET, SOCK_STREAM,0})
    if SOCKET<0 then return -1

    else
        sockaddr = allocate(16) -- thanks Pete Eberlein!
        poke4(sockaddr, {
                         AF_INET+
                         c_func(htons,{port})*#10000,
                         hostnode,0,0})
    end if

    ---------------------------------------------
    -- attempt to connect  to server             --
    ---------------------------------------------
    return c_func(connect,{SOCKET, sockaddr,16})

end function

--------------------------------------------------------------
constant URL = 1, URI = 2, NODE = 3, -- host info
         SERVICE = 1, PORT = 2, PROTOCOL = 3 -- service info

-------------------------------------------------------------
function Get(string hostname, string filename)
-------------------------------------------------------------
object host, service, file

    host = GetHostbyName(hostname)
    if atom(host) then
        puts(1,"Error getting hostname!\n")
        abort(1)
    elsif DEBUG=1 then
        printf(1,"Host: %s Addr: %s  Node: %d\n",host)
    end if

    service = GetServbyName("http","tcp")
    if atom(service) then
        puts(1,"Error getting service\n")
        abort(2)
    else
        if Connect(host[NODE],service[PORT])<0 then
            puts(1,"Error connecting!\n")
            abort(3)
        end if
        if DEBUG=1 then
            printf(1,"Service: %s Port: %d Protocol: %s\n",service)
        end if
    end if

    write(SOCKET, sprintf("GET /%s HTTP/1.0\nHost: %s\n\n",
                          {filename,hostname}))
    file = read(SOCKET)
    c_proc(close_,{SOCKET})

    return file
end function

-------------------------[ MAIN ]----------------------------

object cmd = command_line()
if length(cmd)<4 then
    puts(1,"Usage: socks hostname filename [localfile]\n")
    abort(0)
end if

if length(cmd)>4 then
    atom fn = open(cmd[5],"w")
    puts(fn,Get(cmd[3],cmd[4]))
    close(fn)
else
    puts(1,Get(cmd[3],cmd[4]))
end if

https://board.flatassembler.net/topic.php?t=16785
https://board.flatassembler.net/topic.php?t=11955
https://board.flatassembler.net/topic.php?t=4760
--*/

