--
-- builtins\sockets.e
--
--  Incomplete, windows-only. (should be relatively straightforward to get this working on linux)
--
include cffi.e
--include ptypes.e (already included by cffi)

global constant AF_UNSPEC = 0,
                AF_INET = 2,
--/*
AF_NETBIOS = 17 The NetBIOS address family. This address family is only supported if a Windows Sockets provider for NetBIOS is installed.
AF_INET6 = 23   The Internet Protocol version 6 (IPv6) address family.
AF_IRDA = 26    The Infrared Data Association (IrDA) address family. This address family is only supported if the computer has an infrared port and driver installed.
AF_BTH = 32,    The Bluetooth address family. This address family is only supported if a Bluetooth adapter is installed on Windows Server 2003 or later.
--*/ 
                SOCK_STREAM = 1
--/* 
SOCK_DGRAM = 2  Supports datagrams, which are connectionless, unreliable buffers of a fixed (typically small) maximum length. Uses the User Datagram Protocol (UDP) for the Internet address family (AF_INET or AF_INET6).
SOCK_RAW = 3  Provides a raw socket that allows an application to manipulate the next upper-layer protocol header. To manipulate the IPv4 header, the IP_HDRINCL socket option must be set on the socket. To manipulate the IPv6 header, the IPV6_HDRINCL socket option must be set on the socket. 
SOCK_RDM = 4  Provides a reliable message datagram. An example of this type is the Pragmatic General Multicast (PGM) multicast protocol implementation in Windows, often referred to as reliable multicast programming. 
SOCK_SEQPACKET = 5  Provides a pseudo-stream packet based on datagrams.
--*/

constant INVALID_SOCKET = #FFFFFFFF,
         SOCKET_ERROR = -1,

--/*
AI_PASSIVE 
0x01  The socket address will be used in a call to the bind function. 
 
AI_CANONNAME 
0x02  The canonical name is returned in the first ai_canonname member.
 
AI_NUMERICHOST 
0x04  The nodename parameter passed to the getaddrinfo function must be a numeric string. 
 
AI_ADDRCONFIG 
0x0400  The getaddrinfo will resolve only if a global address is configured. The IPv6 and IPv4 loopback address is not considered a valid global address. This option is only supported on Windows Vista or later. 
 
AI_NON_AUTHORITATIVE 
0x04000  The address information can be from a non-authoritative namespace provider. This option is only supported on Windows Vista or later for the NS_EMAIL namespace.
 
AI_SECURE 
0x08000  The address information is from a secure channel. This option is only supported on Windows Vista or later for the NS_EMAIL namespace.
 
AI_RETURN_PREFERRED_NAMES 
0x010000  The address information is for a preferred name for a user. This option is only supported on Windows Vista or later for the NS_EMAIL namespace.
 
AI_FILESERVER 
0x00040000  A hint to the namespace provider that the hostname being queried is being used in file share scenario. The namespace provider may ignore this hint. 
 
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
id_sockaddr_in  = define_struct(tsockaddr_in),
socklen = get_struct_size(id_sockaddr_in),
sizePeerAddr    = allocate(machine_word()),
id_taddrinfo = define_struct(taddrinfo),

-- functions --
tWSAStartup = """
int WSAStartup(
  _In_   WORD wVersionRequested,
  _Out_  LPWSADATA lpWSAData
);""",
tsocket = """
SOCKET WSAAPI socket(
  _In_  int af,
  _In_  int type,
  _In_  int protocol
);""",
thtons = """
u_short WSAAPI htons(
  _In_  u_short hostshort
);""",
tbind = """
int bind(
  _In_  SOCKET s,
  _In_  const struct sockaddr *name,
  _In_  int namelen
);""",
tWSAGetLastError = """
int WSAGetLastError(void);""",
tlisten = """
int listen(
  _In_  SOCKET s,
  _In_  int backlog
);""",
taccept = """
SOCKET accept(
  _In_     SOCKET s,
  _Out_    struct sockaddr *addr,
  _Inout_  int *addrlen
);""",
trecv = """
int recv(
  _In_   SOCKET s,
  _Out_  char *buf,
  _In_   int len,
  _In_   int flags
);""",
tsend = """
int send(
  _In_  SOCKET s,
  _In_  const char *buf,
  _In_  int len,
  _In_  int flags
);""",
tgai = """
int WSAAPI getaddrinfo(
  _In_opt_  PCSTR pNodeName,
  _In_opt_  PCSTR pServiceName,
  _In_opt_  const ADDRINFOA *pHints,
  _Out_     PADDRINFOA *ppResult
);""",
lib = iff(platform()=WINDOWS?"Ws2_32.dll":"??clib??"),
xsocket             = define_cffi_func(lib, tsocket),
xhtons              = define_cffi_func(lib, thtons),
xbind               = define_cffi_func(lib, tbind),
xWSAGetLastError    = define_cffi_func(lib, tWSAGetLastError),
xlisten             = define_cffi_func(lib, tlisten),
xaccept             = define_cffi_func(lib, taccept),
xrecv               = define_cffi_func(lib, trecv),
xsend               = define_cffi_func(lib, tsend),
xgai                = define_cffi_func(lib, tgai)

procedure die(string what, integer chk=false)
    if chk then what &= " *** and chk==true! *** " end if
    crash("ERROR (%s): %d",{what,c_func(xWSAGetLastError,{})})
end procedure

integer sock_init = false

procedure init_sock()
    if platform()=WINDOWS then
        integer id_WSADATA = define_struct(tWSADATA),
                xWSAStartup = define_cffi_func(lib, tWSAStartup),
                res = c_func(xWSAStartup,{0x0202,allocate_struct(id_WSADATA)})
        if res!=0 then crash("WSAStartup error:%d",res) end if
    end if
    sock_init = true
end procedure

--function new_sock_addr(integer family=AF_INET, port=80, atom addr=NULL)
global function new_sock_addr(integer family=NULL, port=0, atom addr=NULL)
    atom sock_addr = allocate_struct(id_sockaddr_in)
    set_struct_field(id_sockaddr_in,sock_addr,"sin_family",family)
    set_struct_field(id_sockaddr_in,sock_addr,"sin_port",c_func(xhtons,{port}))
    set_struct_field(id_sockaddr_in,sock_addr,"sin_addr",addr)
    return sock_addr
end function

global function new_addrinfo(integer family=NULL, socktype=0, flags=NULL)
--
-- family: among others, AF_UNSPEC, AF_INET.
-- socktype: one of SOCK_STREAM, etc.
-- flags: combination of AI_PASSIVE .. AI_FILESERVER 
--
-- There is no particular requirement to freeaddrinfo(res).
--
    atom addrinfo = allocate_struct(id_taddrinfo)
    set_struct_field(id_taddrinfo,addrinfo,"ai_family",family)
    set_struct_field(id_taddrinfo,addrinfo,"ai_socktype",socktype)
    set_struct_field(id_taddrinfo,addrinfo,"ai_flags",flags)
    return addrinfo
end function

global function socket(integer family, style, protocol)
    if not sock_init then init_sock() end if
    atom sock = c_func(xsocket,{family,style,protocol})
    if sock=INVALID_SOCKET then die("socket") end if
    return sock
end function

global procedure bind(atom sock, sock_addr, integer len=socklen)
    integer res = c_func(xbind,{sock,sock_addr,len})
    if res!=0 then die("bind",res=SOCKET_ERROR) end if
end procedure

global procedure listen(atom sock, integer queuelen)
    integer res = c_func(xlisten,{sock,queuelen})
    if res!=0 then die("listen",res=SOCKET_ERROR) end if
end procedure

global function accept(atom sock, peerAddr)
    pokeN(sizePeerAddr,socklen,machine_word())
    atom peer = c_func(xaccept,{sock,peerAddr,sizePeerAddr})
    if peer=INVALID_SOCKET then die("accept") end if
    return peer
end function

global procedure send(atom peer, buffer, integer len, flags)
    integer res = c_func(xsend,{peer,buffer,len,flags})
    if res=SOCKET_ERROR then die("send") end if
end procedure

global function recv(atom peer, buffer, integer size, flags)
    integer len = c_func(xrecv,{peer,buffer,size,flags})
    return len
end function

global function get_sin_addr(atom sock_addr)
    atom ip = get_struct_field(id_sockaddr_in,sock_addr,"sin_addr")
    return ip
end function

global function ip_to_string(atom ip)
-- (not been tested on anything other than 0.0.0.0 yet...)
    sequence res = repeat(0,4)
    for i=4 to 1 by -1 do
        res[i] = sprintf("%d",and_bits(ip,#FF))
        ip = floor(ip/#100)
    end for
    return join(res,".")
end function

global function getaddrinfo(nullable_string node_name, service_name, atom pHints, pResult)
    integer error = c_func(xgai,{node_name, service_name, pHints, pResult})
    return error
end function

--/*
This may be helpful. Linux 64-bit, obviously untested by me, from https://rosettacode.org/wiki/Echo_server#X86_Assembly
 
; x86_64 Linux NASM
 
global _start
 
%define af_inet 2
%define sock_stream 1
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
  mov rdi, af_inet
  mov rsi, sock_stream
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
  mov word [rsi + family_offset], af_inet
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
