-- EuNet.e is a series of cross-platform networking routines using
-- native function calls.
-- All routines that ask for a sequence inet_address should have it formed
-- as a string "aaa.bbb.ccc.ddd:port".  Each byte does not need to be
-- three digits, but each byte does need to be separated by a period.  
-- The port needs to be included each time.

-- UPDATES:
-- Version 1.2.0
-- 13 Mar 2008 - Added constants and support for get/setsockopt & multicast
-- 27 Mar 2008 - Added string function eunet_is_inetaddr(sequence s)
--             - Fixed support for constant BLOCK_SIZE
--  4 Apr 2008 - Added eunet_delay for nice millisecond delays (nice = returns time slice to OS)
--  8 Apr 2008 - Added dns/res_query for MX and NS lookups
-- 10 Apr 2008 - Bug fixes in recently added code.
-- Version 1.2.1
-- 29 Apr 2008 - Added accept() for incoming connections.
-- Version 1.2.2
-- 30 Apr 2008 - Fixed leading slash in filename in eunet_get_url()
-- Version 1.3.0 (Changes added by Kathy Smith)
--  1 May 2008 - HTTP enhancements: Added sendheader and corresponding state routines.
--             - Updated eunet_get_http and eunet_get_http_use_cookie to transmit all
--             - header data contained in sendheader.
--  2 May 2008 - (Michael Sabal) Added eunet_http_header_parse().
-- Version 1.3.1
-- 10 May 2008 - (Kathy Smith) Deprecated eunet_http_header_parse in favor of different
--               routines for send and recv headers
--             - Make eunet_get_http global.
--  9 May 2008 - (Michael Sabal) Added error_mode to provide a cross-platform
--               means of evaluating error codes.
-- 13 May 2008 - Corrected documentation for return values in eunet_recv.
-- 16 May 2008 - Fixed bug with numerical port numbers in eunet_get_addrinfo
-- Version 1.3.2
-- 19 May 2008 - Added EUNET_ERROR_NOCONNECTION, EUNET_ERROR_NOTASOCKET, SOL_SOCKET, SO_*.
--             - Added FD_ACCEPT to windows_poll().
-- 20 May 2008 - Removed implicit event cancellation from windows_poll().
--             - Ignore explicit cancellation in linux_poll().
--             - Updated documentation on eunet_poll().
-- 21 May 2008 - Fixed missing accept_ for Linux.
-- TO DO:
--             - (Kathy Smith) Add POST support to eunet_get_http

--/*
include std/dll.e
include std/machine.e
include std/os.e
include std/text.e
include std/get.e
include std/console.e
--*/
--include dll.e
--include misc.e
--include machine.e
--include get.e
--include wildcard.e

global constant AF_INET = 2, SOCK_STREAM = 1, SOCK_DGRAM = 2, SOCK_RAW = 3,
                SOCK_RDM = 4, SOCK_SEQPACKET = 5
global constant    -- RFC 1700
                IPPROTO_IP = 0,
                IPPROTO_ICMP = 1,
                IPPROTO_TCP = 6,
                IPPROTO_EGP = 8,
                IPPROTO_PUP = 12,
                IPPROTO_UDP = 17,
                IPPROTO_IDP = 22,
                IPPROTO_TP = 29,
                IPPROTO_EON = 80,
                IPPROTO_RM = 113
global constant
                SOL_SOCKET = #FFFF,

                SO_DEBUG = 1,
                SO_ACCEPTCONN = 2,
                SO_REUSEADDR = 4,
                SO_KEEPALIVE = 8,
                SO_DONTROUTE = 16,
                SO_BROADCAST = 32,
                SO_USELOOPBACK = 64,
                SO_LINGER = 128,
                SO_OOBINLINE = 256,
                SO_REUSEPORT = 512,
                SO_SNDBUF = #1001,
                SO_RCVBUF = #1002,
                SO_SNDLOWAT = #1003,
                SO_RCVLOWAT = #1004,
                SO_SNDTIMEO = #1005,
                SO_RCVTIMEO = #1006,
                SO_ERROR = #1007,
                SO_TYPE = #1008,

                IP_OPTIONS = 1,
                IP_HDRINCL = 2,
                IP_TOS = 3,
                IP_TTL = 4,
                IP_RECVOPTS = 5,
                IP_RECVRETOPTS = 6,
                IP_RECVDSTADDR = 7,
                IP_RETOPTS = 8,
                IP_MULTICAST_IF = 9,
                IP_MULTICAST_TTL = 10,
                IP_MULTICAST_LOOP = 11,
                IP_ADD_MEMBERSHIP = 12,
                IP_DROP_MEMBERSHIP = 13,
                IP_DONTFRAGMENT = 14,
                IP_ADD_SOURCE_MEMBERSHIP = 15,
                IP_DROP_SOURCE_MEMBERSHIP = 16,
                IP_BLOCK_SOURCE = 17,
                IP_UNBLOCK_SOURCE = 18,
                IP_PKTINFO = 19,
                IPV6_UNICAST_HOPS = 4,
                IPV6_MULTICAST_IF = 9,
                IPV6_MULTICAST_HOPS = 10,
                IPV6_MULTICAST_LOOP = 11,
                IPV6_ADD_MEMBERSHIP = 12,
                IPV6_DROP_MEMBERSHIP = 13,
                IPV6_JOIN_GROUP = IPV6_ADD_MEMBERSHIP,
                IPV6_LEAVE_GROUP = IPV6_DROP_MEMBERSHIP,
                IPV6_PKTINFO = 19,
                IP_DEFAULT_MULTICAST_TTL = 1,
                IP_DEFAULT_MULTICAST_LOOP = 1,
                IP_MAX_MEMBERSHIPS = 20,
                TCP_EXPEDITED_1122 = 2,
                UDP_NOCHECKSUM = 1

global constant MSG_OOB = #1,
                MSG_PEEK = #2,
                MSG_DONTROUTE = #4,
                MSG_TRYHARD = #4,
                MSG_CTRUNC = #8,
                MSG_PROXY = #10,
                MSG_TRUNC = #20,
                MSG_DONTWAIT = #40,
                MSG_EOR = #80,
                MSG_WAITALL = #100,
                MSG_FIN = #200,
                MSG_SYN = #400,
                MSG_CONFIRM = #800,
                MSG_RST = #1000,
                MSG_ERRQUEUE = #2000,
                MSG_NOSIGNAL = #4000,
                MSG_MORE = #8000,

                EUNET_ERRORMODE_EUNET = 1,
                EUNET_ERRORMODE_OS = 2,
                EUNET_ERROR_NOERROR = 0,
                EUNET_ERROR_WRONGMODE = -998,
                EUNET_ERROR_NODATA = -1001,
                EUNET_ERROR_LINUXONLY = -1002,
                EUNET_ERROR_WINDOWSONLY = -1003,
                EUNET_ERROR_NODNSRECORDS = -1501,
                EUNET_ERROR_NOCONNECTION = -1502,
                EUNET_ERROR_NOTASOCKET = -1503,
                EUNET_ERROR_UNKNOWN = -1999,

                EAGAIN = 11,
                EWOULDBLOCK = EAGAIN,
                EBADF = 9,
                ECONNRESET = 104,
                ECONNREFUSED = 111,
                EFAULT = 14,
                EINTR = 4,
                EINVAL = 22,
                EIO = 5,
                ENOBUFS = 105,
                ENOMEM = 12,
                ENOSR = 63,
                ENOTCONN = 107,
                ENOTSOCK = 88,
                EOPNOTSUPP = 95,
                ETIMEDOUT = 110,
                WSA_WAIT_FAILED = -1,
                WSA_WAIT_EVENT_0 = 0,
                WSA_WAIT_IO_COMPLETION = #C0,
                WSA_WAIT_TIMEOUT = #102,

                WSABASEERR         = 10000,
                WSAEINTR           = WSABASEERR+4,
                WSAEBADF           = WSABASEERR+9,
                WSAEACCES          = WSABASEERR+13,
                WSAEFAULT          = WSABASEERR+14,
                WSAEINVAL          = WSABASEERR+22,
                WSAEMFILE          = WSABASEERR+24,
                WSAEWOULDBLOCK     = WSABASEERR+35,
                WSAEINPROGRESS     = WSABASEERR+36,
                WSAEALREADY        = WSABASEERR+37,
                WSAENOTSOCK        = WSABASEERR+38,
                WSAEDESTADDRREQ    = WSABASEERR+39,
                WSAEMSGSIZE        = WSABASEERR+40,
                WSAEPROTOTYPE      = WSABASEERR+41,
                WSAENOPROTOOPT     = WSABASEERR+42,
                WSAEPROTONOSUPPORT = WSABASEERR+43,
                WSAESOCKTNOSUPPORT = WSABASEERR+44,
                WSAEOPNOTSUPP      = WSABASEERR+45,
                WSAEPFNOSUPPORT    = WSABASEERR+46,
                WSAEAFNOSUPPORT    = WSABASEERR+47,
                WSAEADDRINUSE      = WSABASEERR+48,
                WSAEADDRNOTAVAIL   = WSABASEERR+49,
                WSAENETDOWN        = WSABASEERR+50,
                WSAENETUNREACH     = WSABASEERR+51,
                WSAENETRESET       = WSABASEERR+52,
                WSAECONNABORTED    = WSABASEERR+53,
                WSAECONNRESET      = WSABASEERR+54,
                WSAENOBUFS         = WSABASEERR+55,
                WSAEISCONN         = WSABASEERR+56,
                WSAENOTCONN        = WSABASEERR+57,
                WSAESHUTDOWN       = WSABASEERR+58,
                WSAETOOMANYREFS    = WSABASEERR+59,
                WSAETIMEDOUT       = WSABASEERR+60,
                WSAECONNREFUSED    = WSABASEERR+61,
                WSAELOOP           = WSABASEERR+62,
                WSAENAMETOOLONG    = WSABASEERR+63,
                WSAEHOSTDOWN       = WSABASEERR+64,
                WSAEHOSTUNREACH    = WSABASEERR+65,
                WSAENOTEMPTY       = WSABASEERR+66,
                WSAEPROCLIM        = WSABASEERR+67,
                WSAEUSERS          = WSABASEERR+68,
                WSAEDQUOT          = WSABASEERR+69,
                WSAESTALE          = WSABASEERR+70,
                WSAEREMOTE         = WSABASEERR+71,
                WSASYSNOTREADY     = WSABASEERR+91,
                WSAVERNOTSUPPORTED = WSABASEERR+92,
                WSANOTINITIALISED  = WSABASEERR+93,

                POLLIN     = #0001,
                POLLPRI    = #0002,
                POLLOUT    = #0004,
                POLLERR    = #0008,
                POLLHUP    = #0010,
                POLLNVAL   = #0020,
                POLLRDNORM = #0040,
                POLLRDBAND = #0080,
                POLLWRNORM = #0100,
                POLLWRBAND = #0200,
                POLLMSG    = #0400,

                FD_READ    = #01,
                FD_WRITE   = #02,
                FD_OOB     = #04,
                FD_ACCEPT  = #08,
                FD_CONNECT = #10,
                FD_CLOSE   = #20,

                NS_C_IN = 1,
                NS_C_ANY = 255,
                NS_KT_RSA = 1,
                NS_KT_DH = 2,
                NS_KT_DSA = 3,
                NS_KT_PRIVATE = 254,
                NS_T_A = 1,
                NS_T_NS = 2,
                NS_T_PTR = 12,
                NS_T_MX = 15,
                NS_T_AAAA = 28, -- deprecated
                NS_T_A6 = 38,
                NS_T_ANY = 255,

                DNS_QUERY_STANDARD                  = #00000000,
                DNS_QUERY_ACCEPT_TRUNCATED_RESPONSE = #00000001,
                DNS_QUERY_USE_TCP_ONLY              = #00000002,
                DNS_QUERY_NO_RECURSION              = #00000004,
                DNS_QUERY_BYPASS_CACHE              = #00000008,
                DNS_QUERY_NO_WIRE_QUERY             = #00000010,
                DNS_QUERY_NO_LOCAL_NAME             = #00000020,
                DNS_QUERY_NO_HOSTS_FILE             = #00000040,
                DNS_QUERY_NO_NETBT                  = #00000080,
                DNS_QUERY_WIRE_ONLY                 = #00000100,
                DNS_QUERY_RETURN_MESSAGE            = #00000200,
                DNS_QUERY_TREAT_AS_FQDN             = #00001000,
                DNS_QUERY_DONT_RESET_TTL_VALUES     = #00100000,
                DNS_QUERY_RESERVED                  = #FF000000,

                HTTP_HEADER_HTTPVERSION     = 1,
                HTTP_HEADER_GET             = 2,
                HTTP_HEADER_HOST            = 3,
                HTTP_HEADER_REFERER         = 4,
                HTTP_HEADER_USERAGENT       = 5,
                HTTP_HEADER_ACCEPT          = 6,
                HTTP_HEADER_ACCEPTCHARSET   = 7,
                HTTP_HEADER_ACCEPTENCODING  = 8,
                HTTP_HEADER_ACCEPTLANGUAGE  = 9,
                HTTP_HEADER_ACCEPTRANGES    = 10,
                HTTP_HEADER_AUTHORIZATION   = 11,
                HTTP_HEADER_DATE            = 12,
                HTTP_HEADER_IFMODIFIEDSINCE = 13,
                HTTP_HEADER_POST            = 14,
                HTTP_HEADER_POSTDATA        = 15,
                HTTP_HEADER_CONTENTTYPE     = 16,
                HTTP_HEADER_CONTENTLENGTH   = 17,
                HTTP_HEADER_FROM            = 18,
                HTTP_HEADER_KEEPALIVE       = 19,
                HTTP_HEADER_CACHECONTROL    = 20,
                HTTP_HEADER_CONNECTION      = 21

global constant
  SIOCADDRT          = #890B,   /* add routing table entry      */
  SIOCDELRT          = #890C,   /* delete routing table entry   */
  SIOCRTMSG          = #890D,   /* call to routing system       */

  /* Socket configuration controls. */
  SIOCGIFNAME        = #8910,   /* get iface name               */
  SIOCSIFLINK        = #8911,   /* set iface channel            */
  SIOCGIFCONF        = #8912,   /* get iface list               */
  SIOCGIFFLAGS       = #8913,   /* get flags                    */
  SIOCSIFFLAGS       = #8914,   /* set flags                    */
  SIOCGIFADDR        = #8915,   /* get PA address               */
  SIOCSIFADDR        = #8916,   /* set PA address               */
  SIOCGIFDSTADDR     = #8917,   /* get remote PA address        */
  SIOCSIFDSTADDR     = #8918,   /* set remote PA address        */
  SIOCGIFBRDADDR     = #8919,   /* get broadcast PA address     */
  SIOCSIFBRDADDR     = #891A,   /* set broadcast PA address     */
  SIOCGIFNETMASK     = #891B,   /* get network PA mask          */
  SIOCSIFNETMASK     = #891C,   /* set network PA mask          */
  SIOCGIFMETRIC      = #891D,   /* get metric                   */
  SIOCSIFMETRIC      = #891E,   /* set metric                   */
  SIOCGIFMEM         = #891F,   /* get memory address (BSD)     */
  SIOCSIFMEM         = #8920,   /* set memory address (BSD)     */
  SIOCGIFMTU         = #8921,   /* get MTU size                 */
  SIOCSIFMTU         = #8922,   /* set MTU size                 */
  SIOCSIFNAME        = #8923,   /* set interface name           */
  SIOCSIFHWADDR      = #8924,   /* set hardware address         */
  SIOCGIFENCAP       = #8925,   /* get/set encapsulations       */
  SIOCSIFENCAP       = #8926,
  SIOCGIFHWADDR      = #8927,   /* Get hardware address         */
  SIOCGIFSLAVE       = #8929,   /* Driver slaving support       */
  SIOCSIFSLAVE       = #8930,
  SIOCADDMULTI       = #8931,   /* Multicast address lists      */
  SIOCDELMULTI       = #8932,
  SIOCGIFINDEX       = #8933,   /* name -> if_index mapping     */
  SIOGIFINDEX = SIOCGIFINDEX,   /* misprint compatibility :-)   */
  SIOCSIFPFLAGS      = #8934,   /* set/get extended flags set   */
  SIOCGIFPFLAGS      = #8935,
  SIOCDIFADDR        = #8936,   /* delete PA address            */
  SIOCSIFHWBROADCAST = #8937,   /* set hardware broadcast addr  */
  SIOCGIFCOUNT       = #8938,   /* get number of devices */

  SIOCGIFBR          = #8940,   /* Bridging support             */
  SIOCSIFBR          = #8941,   /* Set bridging options         */

  SIOCGIFTXQLEN      = #8942,   /* Get the tx queue length      */
  SIOCSIFTXQLEN      = #8943,   /* Set the tx queue length      */


  /* ARP cache control calls. */
           /* = #8950 - = #8952  * obsolete calls, don't re-use */
  SIOCDARP           = #8953,   /* delete ARP table entry       */
  SIOCGARP           = #8954,   /* get ARP table entry          */
  SIOCSARP           = #8955,   /* set ARP table entry          */

  /* RARP cache control calls. */
  SIOCDRARP          = #8960,   /* delete RARP table entry      */
  SIOCGRARP          = #8961,   /* get RARP table entry         */
  SIOCSRARP          = #8962,   /* set RARP table entry         */

  /* Driver configuration calls */

  SIOCGIFMAP         = #8970,   /* Get device parameters        */
  SIOCSIFMAP         = #8971,   /* Set device parameters        */

  /* DLCI configuration calls */

  SIOCADDDLCI        = #8980,   /* Create new DLCI device       */
  SIOCDELDLCI        = #8981,   /* Delete DLCI device           */

  /* Device private ioctl calls. */

  /* These 16 ioctls are available to devices via the do_ioctl() device
     vector.    Each device should include this file and redefine these
     names as their own. Because these are device dependent it is a good
     idea _NOT_ to issue them to random objects and hope.  */

  SIOCDEVPRIVATE     = #89F0,   /* to 89FF */

  /*
   *        These 16 ioctl calls are protocol private
   */

  SIOCPROTOPRIVATE   = #89E0   /* to 89EF */

constant BLOCK_SIZE = 4096,
         IFF_PROMISC = #100

atom dll_, ipdll_, sockdll_, kerneldll_, dnsdll_,
     wsastart_, wsacleanup_, wsawaitformultipleevents_,
     wsacreateevent_, wsaeventselect_, wsaenumnetworkevents_, wsacloseevent_,
     ioctl_, socket_, bind_, connect_, listen_, accept_,
     send_, sendto_, sendmsg_, recv_, recvfrom_, recvmsg_,
     poll_, close_, shutdown_,
     getsockopts_, setsockopts_,
     getiftable_, dnsquery_, dnsrlfree_, dnsexpand_,
     gethostbyname_, getservbyname_, getaddrinfo_, freeaddrinfo_,
--     read_, write_, getsockname_, getpeername_,
     errno_, error_, delay_

sequence windows_poll_seq = {},
         this_cookiejar = {},
         sendheader = {}, -- HTTP header sequence, sent to somewhere (usually the server)
         recvheader = {}  -- HTTP header sequence, recieved from somewhere (usually the server)

atom error_mode = 1  -- This will indicate whether returned errors will be OS numbers (mode 2)
                     -- or EUNET_ERROR numbers (mode 1).  The default is mode 1.

-- select() will not be supported because Euphoria does not support
-- wrapping of macros.  FD_SET, etc. do not have extern functions
-- available, and are required in order for select() to work.
-- Use poll() instead.

-- gethostbyname() and getservbyname() are deprecated in favor of getaddrinfo(). 
-------------------------------------------------------------------------------
-- Initializations & support routines
-------------------------------------------------------------------------------
atom wsastatus, wsadata

    if platform()=WINDOWS then
        ipdll_ = open_dll("iphlpapi.dll")
        sockdll_ = open_dll("ws2_32.dll")
        kerneldll_ = open_dll("kernel32.dll")
        dnsdll_ = open_dll("dnsapi.dll")
        error_ = define_c_func(sockdll_,"WSAGetLastError",{},C_INT)
        socket_ = define_c_func(sockdll_,"socket",{C_INT,C_INT,C_INT},C_INT)
        getiftable_ = define_c_func(ipdll_,"GetAdaptersInfo",{C_POINTER,C_POINTER},C_LONG)
        bind_ = define_c_func(sockdll_,"bind",{C_INT,C_POINTER,C_INT},C_INT)
        connect_ = define_c_func(sockdll_,"connect",{C_INT,C_POINTER,C_INT},C_INT)
        poll_ = define_c_func(sockdll_,"WSAPoll",{C_POINTER,C_INT,C_INT},C_INT)
        listen_ = define_c_func(sockdll_,"listen",{C_INT,C_INT},C_INT)
--      accept_ = define_c_func(sockdll_,"accept",{C_INT,C_POINTER,C_POINTER},C_INT)
        accept_ = define_c_func(sockdll_,"WSAAccept",{C_INT,C_POINTER,C_POINTER,C_POINTER,C_INT},C_INT)
        socket_ = define_c_func(sockdll_,"socket",{C_INT,C_INT,C_INT},C_INT)
        ioctl_ = define_c_func(sockdll_,"ioctlsocket",{C_INT,C_INT,C_POINTER},C_INT)
        getsockopts_ = define_c_func(sockdll_,"getsockopt",{C_INT,C_INT,C_INT,C_POINTER,C_POINTER},C_INT)
        setsockopts_ = define_c_func(sockdll_,"setsockopt",{C_INT,C_INT,C_INT,C_POINTER,C_INT},C_INT)
        send_ = define_c_func(sockdll_,"send",{C_INT,C_POINTER,C_INT,C_INT},C_INT)
        sendto_ = define_c_func(sockdll_,"sendto",{C_INT,C_POINTER,C_INT,C_INT,C_POINTER,C_INT},C_INT)
        sendmsg_ = define_c_func(sockdll_,"sendmsg",{C_INT,C_POINTER,C_INT},C_INT)
        recv_ = define_c_func(sockdll_,"recv",{C_INT,C_POINTER,C_INT,C_INT},C_INT)
        recvfrom_ = define_c_func(sockdll_,"recvfrom",{C_INT,C_POINTER,C_INT,C_INT,C_POINTER,C_POINTER},C_INT)
        recvmsg_ = define_c_func(sockdll_,"recvmsg",{C_INT,C_POINTER,C_INT},C_INT)
        close_ = define_c_func(sockdll_,"closesocket",{C_INT},C_INT)
        shutdown_ = define_c_func(sockdll_,"shutdown",{C_INT,C_INT},C_INT)
        gethostbyname_ = define_c_func(sockdll_,"gethostbyname",{C_POINTER},C_POINTER)
        getservbyname_ = define_c_func(sockdll_,"getservbyname",{C_POINTER,C_POINTER}, C_POINTER)
        getaddrinfo_ = define_c_func(sockdll_,"getaddrinfo",{C_POINTER,C_POINTER,C_POINTER,C_POINTER},C_INT)
        freeaddrinfo_ = define_c_proc(sockdll_,"freeaddrinfo",{C_POINTER})
        -- WSAStartup() is required when using WinSock.
        wsastart_ = define_c_func(sockdll_,"WSAStartup",{C_USHORT,C_POINTER},C_INT)
        wsadata = allocate(4096)
        wsastatus = c_func(wsastart_,{#0202,wsadata}) -- version 2.2 max
        if wsastatus!=0 then
            puts(1,sprintf("Winsock startup error %d",wsastatus))
        end if
        free(wsadata)
        wsacleanup_ = define_c_func(sockdll_,"WSACleanup",{},C_INT) -- Windows apps need to run this when the program ends
        wsacreateevent_ = define_c_func(sockdll_,"WSACreateEvent",{},C_INT)
        wsacloseevent_ = define_c_func(sockdll_,"WSACloseEvent",{C_INT},C_INT)
        wsaeventselect_ = define_c_func(sockdll_,"WSAEventSelect",{C_INT,C_INT,C_INT},C_INT)
        wsaenumnetworkevents_ = define_c_func(sockdll_,"WSAEnumNetworkEvents",{C_INT,C_INT,C_POINTER},C_INT)
        wsawaitformultipleevents_ = define_c_func(sockdll_,"WSAWaitForMultipleEvents",{C_INT,C_POINTER,C_INT,C_INT,C_INT},C_INT)
        delay_ = define_c_proc(kerneldll_,"Sleep",{C_INT})
        dnsquery_ = define_c_func(dnsdll_,"DnsQuery_A",{C_POINTER,C_USHORT,C_INT,C_POINTER,C_POINTER,C_POINTER},C_INT)
        dnsrlfree_ = define_c_proc(dnsdll_,"DnsRecordListFree",{C_POINTER,C_INT})
    elsif platform()=LINUX then
        dll_ = open_dll("") -- libc
        errno_ = define_c_var(dll_, "errno")
        dnsdll_ = open_dll("libresolv.so")
        error_ = define_c_func(dll_,"__errno_location",{},C_INT)
        ioctl_ = define_c_func(dll_,"ioctl",{C_INT,C_INT,C_INT},C_INT)
        socket_ = define_c_func(dll_,"socket",{C_INT,C_INT,C_INT},C_INT)
        getsockopts_ = define_c_func(dll_,"getsockopt",{C_INT,C_INT,C_INT,C_POINTER,C_POINTER},C_INT)
        setsockopts_ = define_c_func(dll_,"setsockopt",{C_INT,C_INT,C_INT,C_POINTER,C_INT},C_INT)
        bind_ = define_c_func(dll_,"bind",{C_INT,C_POINTER,C_INT},C_INT)
        connect_ = define_c_func(dll_,"connect",{C_INT,C_POINTER,C_INT},C_INT)
        poll_ = define_c_func(dll_,"poll",{C_POINTER,C_INT,C_INT},C_INT)
        listen_ = define_c_func(dll_,"listen",{C_INT,C_INT},C_INT)
        accept_ = define_c_func(dll_,"accept",{C_INT,C_POINTER,C_POINTER},C_INT)
        send_ = define_c_func(dll_,"send",{C_INT,C_POINTER,C_INT,C_INT},C_INT)
        sendto_ = define_c_func(dll_,"sendto",{C_INT,C_POINTER,C_INT,C_INT,C_POINTER,C_INT},C_INT)
        sendmsg_ = define_c_func(dll_,"sendmsg",{C_INT,C_POINTER,C_INT},C_INT)
        recv_ = define_c_func(dll_,"recv",{C_INT,C_POINTER,C_INT,C_INT},C_INT)
        recvfrom_ = define_c_func(dll_,"recvfrom",{C_INT,C_POINTER,C_INT,C_INT,C_POINTER,C_POINTER},C_INT)
        recvmsg_ = define_c_func(dll_,"recvmsg",{C_INT,C_POINTER,C_INT},C_INT)
        close_ = define_c_func(dll_,"close",{C_INT},C_INT)
        shutdown_ = define_c_func(dll_,"shutdown",{C_INT,C_INT},C_INT)
        gethostbyname_ = define_c_func(dll_,"gethostbyname",{C_POINTER},C_POINTER)
        getservbyname_ = define_c_func(dll_,"getservbyname",{C_POINTER,C_POINTER}, C_POINTER)
        getaddrinfo_ = define_c_func(dll_,"getaddrinfo",{C_POINTER,C_POINTER,C_POINTER,C_POINTER},C_INT)
        freeaddrinfo_ = define_c_proc(dll_,"freeaddrinfo",{C_POINTER})
        delay_ = define_c_func(dll_,"nanosleep",{C_POINTER,C_POINTER},C_INT)
        dnsquery_ = define_c_func(dnsdll_,"res_query",{C_POINTER,C_INT,C_INT,C_POINTER,C_INT},C_INT)
        dnsexpand_ = define_c_func(dnsdll_,"dn_expand",{C_POINTER,C_POINTER,C_POINTER,C_POINTER,C_INT},C_INT)
    else
        ?9/0
    end if

-------------------------------------------------------------------------------

function get_sockaddr(atom lpsz)
string s
atom port
    s = ""
    if lpsz!=NULL then
        -- sockaddr
        -- 2bytes: AF_INET (or other type)
        -- 2bytes: port
        -- 4bytes: network address
        -- 8bytes: null
        for ptr=lpsz+4 to lpsz+7 do
            s &= sprintf("%d",peek(ptr))
            if ptr<lpsz+7 then s &= '.' end if
        end for
        port = (peek(lpsz+2)*#100)+peek(lpsz+3)
        s &= sprintf(":%d",port)
    end if
    return s
end function

-------------------------------------------------------------------------------

function make_sockaddr(sequence inet_addr)
atom sockaddr, cpos
sequence temp
sequence addr
    if find('.',inet_addr)=0 or find(':',inet_addr)=0 then
        return 0
    end if
    addr = {0,0,0,0,0}
    cpos = find('.',inet_addr)
    temp = value(inet_addr[1..cpos-1])
    if temp[1]=GET_SUCCESS then
        addr[1] = temp[2]
        inet_addr = inet_addr[cpos+1..length(inet_addr)]
    else
        return 0
    end if
    cpos = find('.',inet_addr)
    temp = value(inet_addr[1..cpos-1])
    if temp[1]=GET_SUCCESS then
        addr[2] = temp[2]
        inet_addr = inet_addr[cpos+1..length(inet_addr)]
    else
        return 0
    end if
    cpos = find('.',inet_addr)
    temp = value(inet_addr[1..cpos-1])
    if temp[1]=GET_SUCCESS then
        addr[3] = temp[2]
        inet_addr = inet_addr[cpos+1..length(inet_addr)]
    else
        return 0
    end if
    cpos = find(':',inet_addr)
    temp = value(inet_addr[1..cpos-1])
    if temp[1]=GET_SUCCESS then
        addr[4] = temp[2]
        inet_addr = inet_addr[cpos+1..length(inet_addr)]
    else
        return 0
    end if
    temp = value(inet_addr)
    if temp[1]=GET_SUCCESS then
        addr[5] = temp[2]
    else
        return 0
    end if
    sockaddr = allocate(16)
    poke(sockaddr,{AF_INET,0})
    poke(sockaddr+2,floor(addr[5]/256))
    poke(sockaddr+3,remainder(addr[5],256))
    poke(sockaddr+4,addr[1..4])
    poke(sockaddr+8,{0,0,0,0,0,0,0,0})
    return sockaddr

end function

-------------------------------------------------------------------------------

global type eunet_is_inetaddr(object s)
-- Checks if s is an IP address in the form (#.#.#.#[:#])
atom numdots = 0,
     numcols = 0

    if not string(s) then
        return 0
    end if
    if length(s)<7 or length(s)>21 then
        return 0
    end if
    for ctr=1 to length(s) do
        integer ch = s[ctr]
        if    ch='.' then numdots += 1
        elsif ch=':' then numcols += 1
        elsif ch<'0' or ch>'9' then
            return 0
        end if
    end for
    if numdots!=3 or numcols>1 then
        return 0
    end if
    return 1
end type

-------------------------------------------------------------------------------

global function eunet_get_errormode()
    return error_mode
end function

-------------------------------------------------------------------------------

global function eunet_set_errormode(integer mode)
    if find(mode,{1,2})=0 then
        return EUNET_ERROR_WRONGMODE
    else
        error_mode = mode
        return 0
    end if
end function

-------------------------------------------------------------------------------

global function eunet_get_error()
  -- returns a 2-element sequence {ERROR_CODE,ERROR_STRING}
sequence rtn
    rtn = {0,""}
    if platform()=WINDOWS then
        rtn[1] = c_func(error_,{})
    elsif platform()=LINUX then
        rtn[1] = peek4u(errno_)
    end if
    if error_mode=EUNET_ERRORMODE_OS then
        return rtn
    end if
    if error_mode=EUNET_ERRORMODE_EUNET then
        if rtn[1]=EAGAIN or rtn[1]=WSAEWOULDBLOCK then
            rtn = {EUNET_ERROR_NODATA,"There is no data waiting."}
        elsif rtn[1]=9501 or rtn[1]=9003 then
            rtn = {EUNET_ERROR_NODNSRECORDS,"DNS could not find an exact match."}
        elsif rtn[1]=EOPNOTSUPP or rtn[1]=WSAEOPNOTSUPP then
            if platform()=WINDOWS then
                rtn = {EUNET_ERROR_LINUXONLY,"This only works on Linux."}
            elsif platform()=LINUX then
                rtn = {EUNET_ERROR_WINDOWSONLY,"This only works on Windows."}
            else
                rtn = {EUNET_ERROR_UNKNOWN,"You are not using Windows or Linux."}
            end if
        elsif rtn[1]=WSAENOTCONN or rtn[1]=ENOTCONN then
            rtn = {EUNET_ERROR_NOCONNECTION,"This socket is not connected to anything."}
        elsif rtn[1]=WSAENOTSOCK or rtn[1]=ENOTSOCK then
            rtn = {EUNET_ERROR_NOTASOCKET,"Either the socket is already closed, or "&
                   "you are\n trying to use a file handle in an IP socket."}
        elsif rtn[1]=0 then
            rtn = {EUNET_ERROR_NOERROR,"OK"}
        else
            rtn = {EUNET_ERROR_UNKNOWN,"Unknown error."}
        end if
    end if
    return rtn

end function

-------------------------------------------------------------------------------

global function eunet_get_blocksize()
    return BLOCK_SIZE
end function

-------------------------------------------------------------------------------

global function eunet_delay(atom millisec)
  -- Delays nicely for a set number of milliseconds (or a few more)
  -- Returns 0 on success or something else on error.

atom result, timptr

    if platform()=WINDOWS then
        c_proc(delay_,{millisec})
        return 0
    elsif platform()=LINUX then
        timptr = allocate(16) -- Reserved for when OSs start using 64bit time_t
        if millisec>=1000 then
            poke4(timptr,floor(millisec/1000))
        else
            poke4(timptr,0)
        end if
        poke4(timptr+4,remainder(millisec,1000)*1000000)
        result = c_func(delay_,{timptr,0})
        free(timptr)
        return result
    end if
    return -1

end function

-------------------------------------------------------------------------------
--Andy Serpas Turbo version
-- c = "object" by Kat ; modded and used in strtok.e
-- c can now be a list {'z','\n','etc'} and s will be parsed by all those in list
-- made case insensitive by Kat
-- mod'd again for eunet
function eunet_parse(sequence s, object c)

integer slen, spt, flag
sequence parsed, upperc, uppers

    upperc = ""
    uppers = ""

    if atom(c) -- kat
    then c = {c}
    end if

    parsed = {}
    slen = length(s)
    spt = 1
    flag = 0

    upperc = upper(c)
    uppers = upper(s)
    for i=1 to slen do
        if find(uppers[i],upperc) then
            if flag=1 then
                parsed = append(parsed,s[spt..i-1])
                flag = 0
                spt = i+1
            else
                spt += 1
            end if
        else
            flag = 1
        end if
    end for
    if flag=1 then
        parsed = append(parsed,s[spt..slen])
    end if

    return parsed

end function

--------------------------------------------------------------------------------
--PL added*2
global function eunet_get_promiscuous_mode(atom sockid, sequence itf)
atom ifreq = allocate(200,1)
    poke(ifreq,itf&0)
    if c_func(ioctl_,{sockid,SIOCGIFFLAGS,ifreq})<0 then  -- Flags
        return -1
    end if
    integer flags = peek2u(ifreq+16)
    return and_bits(flags, IFF_PROMISC)!=0
end function

--------------------------------------------------------------------------------

global function eunet_set_promiscuous_mode(atom sockid, sequence itf, integer set)
atom ifreq = allocate(200,1)
    poke(ifreq,itf&0)
    if c_func(ioctl_,{sockid,SIOCGIFFLAGS,ifreq})<0 then  -- Flags
        return -1
    end if
    integer flags = peek2u(ifreq+16)
    if set then
        flags = or_bits(flags, IFF_PROMISC)
    else
        flags = xor_bits(flags, IFF_PROMISC)
    end if
    poke2(ifreq+16, flags)
    if c_func(ioctl_,{sockid,SIOCSIFFLAGS,ifreq})<0 then  -- Flags
        return -1
    end if
    return 0
end function

-------------------------------------------------------------------------------
-- Get list of networking interfaces
-------------------------------------------------------------------------------

function windows_get_adapters_table()

atom iftable, status
atom iftablesize, ptr
sequence ifaces, row, breadcrumbs
    ifaces = {}
    iftable = allocate((640*1)+4) -- The records are a singly linked list
    poke4(iftable,644)
    status = c_func(getiftable_,{iftable+4,iftable})
    if status!=0 then
        iftablesize = peek4u(iftable)
        free(iftable)
        iftable = allocate(iftablesize+4)
        poke4(iftable,iftablesize)
        status = c_func(getiftable_,{iftable+4,iftable})
        if status!=0 then
            free(iftable)
            return {}
        end if
    end if
    iftablesize = peek4u(iftable)
    breadcrumbs = {iftable+4}
    ptr = iftable+4
    while ptr!=0 do
        breadcrumbs = append(breadcrumbs,peek4u(ptr))
        row = {"","","",0,0,"",""}
        row[1] = trim(peek({ptr+8,256}))      -- Adapter name
        row[2] = trim(peek({ptr+268,128}))   -- Adapter description
        row[3] = peek({ptr+404,peek4u(ptr+400)})  -- HW address (not a string)
        row[4] = peek4u(ptr+412)     -- Index
        row[5] = peek4u(ptr+416)     -- Type
        -- If there is more than one IP address on an adapter, it can be discovered
        -- by peek(peek4u(ptr+428)+4,16).  The first 4 bytes of IP_ADDR_STRING are
        -- a pointer to the next IP_ADDR_STRING struct.
        row[6] = trim(peek({ptr+432,16})) -- Current IP address (string)
        row[7] = trim(peek({ptr+448,16})) -- Netmask (string)
        ifaces = append(ifaces,row)
        ptr = breadcrumbs[length(breadcrumbs)]
    end while
    free(iftable)
    return ifaces

end function

-------------------------------------------------------------------------------
-- Returns sequence of interface names
function linux_get_iface_list()
sequence ifaces
atom cpos
object row
atom fn
    fn = open("/proc/net/dev","r")
    ifaces = {}
    row = gets(fn)
    while sequence(row) do
        if length(row)>1 then
            cpos = find(':',row)
            if cpos>0 then
                ifaces = append(ifaces,trim(row[1..cpos-1]))
            else
                ifaces = append(ifaces,trim(row))
            end if
        end if
        row = gets(fn)
    end while
    close(fn)
    if length(ifaces)>=3 then
        ifaces = ifaces[3..length(ifaces)]
    end if

    return ifaces

end function

-------------------------------------------------------------------------------

function windows_get_iface_list()
sequence ifaces, rtn
    ifaces = windows_get_adapters_table()
    rtn = {}
    for ctr=1 to length(ifaces) do
        rtn = append(rtn,ifaces[ctr][2])
    end for

    return rtn
end function

-------------------------------------------------------------------------------

global function eunet_get_iface_list()
    if platform()=WINDOWS then
        return windows_get_iface_list()
    elsif platform()=LINUX then
        return linux_get_iface_list()
    else
        return {}
    end if
end function

-------------------------------------------------------------------------------
-- Get networking interface details
-------------------------------------------------------------------------------
-- returns a sequence of details as:
-- {seq InterfaceName, int index, int flags, seq MACaddr, int metric, int mtu, int serial_outfill,
--  int serial_keepalive, seq if_map, int tx_Q_len, seq IP_addr, seq PPP_Dest_IP_addr,
--  seq IP_Bcast_addr, seq IP_Netmask, seq stats}
--  Note: if PPP_Dest_IP_addr = IP_addr, serial_keepalive will be the up/down status of the address

function windows_get_iface_details(sequence iface_name)

sequence row, list, list2
atom iptable, tblptr, getipaddrtable_, status, sz

    list = windows_get_adapters_table()
    iptable = allocate(24+(24*(length(list)+1)))
    poke4(iptable,(20+(24*(length(list)+1))))
    getipaddrtable_ = define_c_func(ipdll_,"GetIpAddrTable",{C_POINTER,C_POINTER,C_INT},C_INT)
    status = c_func(getipaddrtable_,{iptable+4,iptable,0})
    if status!=0 then
        sz = peek4u(iptable+8)
        free(iptable)
        iptable = allocate(sz+24)
        poke4(iptable,sz+20)
        status = c_func(getipaddrtable_,{iptable+4,iptable,0})
        if status!=0 then
            free(iptable)
            return {}
        end if
    end if
    list2 = {}
    sz = peek4u(iptable+4)
    tblptr = iptable+8
    for ctr=1 to sz do
        row = {sprintf("%d.%d.%d.%d",peek({tblptr,4})),  -- IP address
               peek4u(tblptr+4),                         -- Index
               sprintf("%d.%d.%d.%d",peek({tblptr+8,4})),   -- Netmask
               sprintf("%d.%d.%d.%d",sq_add(256,sq_or_bits(peek({tblptr,4}),sq_not_bits(peek({tblptr+8,4}))))),  -- Broadcast
               -- a bug (or undefined parameter) in or_bits(not_bits) throws the result into negative numbers.
               -- Adding 256 to the sequence gives us what we're really looking for.
               peek4u(tblptr+16),                           -- Reassemble size
               peek(tblptr+22)}                             -- IP type
        tblptr = tblptr+24
        list2 = append(list2,row)
    end for
    free(iptable)

    for ctr=1 to length(list) do
        if compare(iface_name,list[ctr][2])=0 then
            row = {list[ctr][2],list[ctr][4],0,"",0,0,0,0,{},0,list[ctr][6],"","",list[ctr][7],{}}
            for ctr2=1 to length(list2) do
                if compare(list2[ctr2][2],list[ctr][4])=0 then -- matching index
                    row[4] = ""
                    for ctr3=1 to length(list[ctr][3]) do
                        row[4] = row[4] & sprintf("%02x:",list[ctr][3][ctr3])
                    end for
                    row[4] = row[4][1..length(row[4])-1]
                    row[8] = list2[ctr2][6]
                    row[11] = list2[ctr2][1]
                    row[12] = row[11]
                    row[13] = list2[ctr2][4]
                    row[14] = list2[ctr2][3]
                    return row
                end if
            end for
        end if
    end for

    return {}

end function

-------------------------------------------------------------------------------

function linux_get_iface_details(sequence iface_name)
sequence details
atom ifreq, sockid, status

  --{#8910,#8913,#8927,#891d,#8921,?,?,#8970,#8942,#8915,#8917,#8919,#891b,?}
    details = {iface_name,0,0,"",0,0,0,0,{},0,"","","","",{}}
    ifreq = allocate(200)
    sockid = c_func(socket_,{AF_INET,SOCK_DGRAM,0})
    if sockid<0 then
        free(ifreq)
        return {}
    end if
    poke(ifreq,iface_name&0)
    status = c_func(ioctl_,{sockid,SIOCGIFFLAGS,ifreq}) -- Flags
    if status>=0 then
        details[3] = peek(ifreq+16)+(256*peek(ifreq+17))
    end if
    status = c_func(ioctl_,{sockid,SIOCGIFHWADDR,ifreq}) -- HW Address
    if status>=0 then
        --details[4] = get_sockaddr(ifreq+16)
        details[4] = ""
        for ctr=18 to 25 do
            details[4] = details[4] & sprintf("%02x:",peek(ifreq+ctr))
        end for
        details[4] = details[4][1..length(details[4])-1]
    end if
    status = c_func(ioctl_,{sockid,SIOCGIFMETRIC,ifreq}) -- metric
    if status>=0 then
        details[5] = peek4u(ifreq+16)
    end if
    status = c_func(ioctl_,{sockid,SIOCGIFMTU,ifreq}) -- mtu
    if status>=0 then
        details[6] = peek4u(ifreq+16)
    end if
    status = c_func(ioctl_,{sockid,SIOCGIFTXQLEN,ifreq}) -- transmit queue length
    if status>=0 then
        details[10] = peek4u(ifreq+16)
    end if
    status = c_func(ioctl_,{sockid,SIOCGIFADDR,ifreq}) -- IP address
    if status>=0 then
        details[11] = get_sockaddr(ifreq+16)
    end if
    status = c_func(ioctl_,{sockid,SIOCGIFDSTADDR,ifreq}) -- PPP destination IP address
    if status>=0 then
        details[12] = get_sockaddr(ifreq+16)
    end if
    status = c_func(ioctl_,{sockid,SIOCGIFBRDADDR,ifreq}) -- Broadcast address
    if status>=0 then
        details[13] = get_sockaddr(ifreq+16)
    end if
    status = c_func(ioctl_,{sockid,SIOCGIFNETMASK,ifreq}) -- Netmask
    if status>=0 then
        details[14] = get_sockaddr(ifreq+16)
    end if

    return details

end function

-------------------------------------------------------------------------------

global function eunet_get_iface_details(sequence iface_name)
    if platform()=WINDOWS then
        return windows_get_iface_details(iface_name)
    elsif platform()=LINUX then
        return linux_get_iface_details(iface_name)
    else
        return {}
    end if
end function

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Server-side sockets (bind, listeners, signal hooks, accepters)
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Bind
-------------------------------------------------------------------------------
-- Bind is typically used in TCP and SOCK_STREAM connections.
-- It returns 0 on success and -1 on failure.
function linux_bind(atom socket, sequence inet_addr)
atom sockaddr
    sockaddr = make_sockaddr(inet_addr)
    if sockaddr=0 then
        return -1
    end if
    return c_func(bind_,{socket,sockaddr,16})
end function

-------------------------------------------------------------------------------

function windows_bind(atom socket, sequence inet_addr)
  -- Windows does bind the same as Linux
    return linux_bind(socket,inet_addr)
end function

-------------------------------------------------------------------------------

global function eunet_bind(atom socket, sequence inet_addr)
    if platform()=WINDOWS then
        return windows_bind(socket,inet_addr)
    elsif platform()=LINUX then
        return linux_bind(socket,inet_addr)
    end if
    return -1
end function

-------------------------------------------------------------------------------
-- Listen
-------------------------------------------------------------------------------
-- Listen is typically used in TCP and SOCK_STREAM connections.
-- It returns 0 on success and error_code on failure.
function linux_listen(atom socket, integer pending_conn_len)
    return c_func(listen_,{socket,pending_conn_len})
end function

-------------------------------------------------------------------------------

function windows_listen(atom socket, integer pending_conn_len)
  -- Windows does listen the same as Linux
    return linux_listen(socket,pending_conn_len)
end function

-------------------------------------------------------------------------------

global function eunet_listen(atom socket, integer pending_conn_len)
    if platform()=WINDOWS then
        return windows_listen(socket, pending_conn_len)
    elsif platform()=LINUX then
        return linux_listen(socket, pending_conn_len)
    end if
    return -1
end function

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Accept: Returns {atom new_socket, sequence peer_ip_addres} on success, or
-- -1 on error.

function linux_accept(atom socket)

atom sockptr, result
sequence peer

    sockptr = allocate(20)
    poke4(sockptr,16)
    for ctr=4 to 16 by 4 do
        poke4(sockptr+ctr,0)
    end for
    result = c_func(accept_,{socket,sockptr+4,sockptr})
    if result<0 then
        free(sockptr)
        return -1
    end if
    peer = get_sockaddr(sockptr+4)
    free(sockptr)
    return {result,peer}

end function

-------------------------------------------------------------------------------

function windows_accept(atom socket)
--  return linux_accept(socket)
atom sockptr, result
sequence peer

    sockptr = allocate(20)
    poke4(sockptr,16)
    for ctr=4 to 16 by 4 do
        poke4(sockptr+ctr,0)
    end for
    result = c_func(accept_,{socket,sockptr+4,sockptr,0,0})
    if result<0 then
        free(sockptr)
        return -1
    end if
    peer = get_sockaddr(sockptr+4)
    free(sockptr)
    return {result,peer}

end function

-------------------------------------------------------------------------------

global function eunet_accept(atom socket)
    if platform()=WINDOWS then
        return windows_accept(socket)
    elsif platform()=LINUX then
        return linux_accept(socket)
    end if
    return -1
end function

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Client-side sockets (connect)
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Returns 0 on success and -1 on failure

function linux_connect(atom socket, sequence inet_addr)
atom sockaddr
    sockaddr = make_sockaddr(inet_addr)
    if sockaddr=0 then
        return -1
    end if
    return c_func(connect_,{socket,sockaddr,16})
end function

-------------------------------------------------------------------------------

function windows_connect(atom socket, sequence inet_addr)
  -- Windows connect works the same as Linux
    return linux_connect(socket,inet_addr)
end function

-------------------------------------------------------------------------------

global function eunet_connect(atom socket, sequence inet_addr)
    if platform()=WINDOWS then
        return windows_connect(socket,inet_addr)
    elsif platform()=LINUX then
        return linux_connect(socket,inet_addr)
    end if
    return -1
end function

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Routines for both server & client (socket, read, write, select)
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- New socket
-------------------------------------------------------------------------------

function linux_new_socket(integer family, integer sock_type, integer protocol)
    return c_func(socket_,{family,sock_type,protocol})
end function

-------------------------------------------------------------------------------

function windows_new_socket(integer family, integer sock_type, integer protocol)
  -- Windows does socket the same as Linux
    return linux_new_socket(family,sock_type,protocol)
end function

-------------------------------------------------------------------------------

global function eunet_new_socket(integer family, integer sock_type, integer protocol)

    if platform()=WINDOWS then
        return windows_new_socket(family,sock_type,protocol)
    elsif platform()=LINUX then
        return linux_new_socket(family,sock_type,protocol)
    end if
    return -1

end function

-------------------------------------------------------------------------------
-- Close socket
-------------------------------------------------------------------------------

function linux_close_socket(atom socket)
    return c_func(close_,{socket})
end function

-------------------------------------------------------------------------------

function windows_close_socket(atom socket)
    return c_func(close_,{socket})
end function

-------------------------------------------------------------------------------

global function eunet_close_socket(atom socket)
    if platform()=WINDOWS then
        return windows_close_socket(socket)
    elsif platform()=LINUX then
        return linux_close_socket(socket)
    end if
    return -1
end function

-------------------------------------------------------------------------------
-- Shutdown socket
-------------------------------------------------------------------------------

function linux_shutdown_socket(atom socket, atom method)
    return c_func(shutdown_,{socket,method})
end function

-------------------------------------------------------------------------------

function windows_shutdown_socket(atom socket, atom method)
    return c_func(shutdown_,{socket,method})
end function

-------------------------------------------------------------------------------

global function eunet_shutdown_socket(atom socket, atom method)
    if platform()=WINDOWS then
        return windows_shutdown_socket(socket,method)
    elsif platform()=LINUX then
        return linux_shutdown_socket(socket,method)
    end if
    return -1
end function

-------------------------------------------------------------------------------
-- poll
-- select() is not supported
-------------------------------------------------------------------------------
-- socket list is a sequence of 2-element sequences.  Each sequence contains
-- the socket number, and the flags of what to check for.  Timeout is the
-- number of milliseconds before returning.  0 returns immediately, -1 returns
-- only when an event occurs.
-- eunet_poll may return an atom on no event or error, and a sequence of 
-- return event flags (matching socket_list) when an event has occurred.

function linux_poll(sequence socket_list, atom timeout)

atom fdptr, nfds
object rtn

    fdptr = allocate(length(socket_list)*8)
    nfds = 0

    for ctr=1 to length(socket_list) do
        if sequence(socket_list[ctr]) and length(socket_list[ctr])>=2 and
        atom(socket_list[ctr][1]) and atom(socket_list[ctr][2]) and
        socket_list[ctr][2]>0 then
            poke4(fdptr+(nfds*8),socket_list[ctr][1])
            poke(fdptr+5+(nfds*8),floor(socket_list[ctr][2]/256))
            poke(fdptr+4+(nfds*8),remainder(socket_list[ctr][2],256))
            poke(fdptr+6+(nfds*8),0)
            poke(fdptr+7+(nfds*8),0)
            nfds = nfds+1
        end if
    end for
    if nfds=0 then
        free(fdptr)
        return {}
    end if
    rtn = c_func(poll_,{fdptr,nfds,timeout})
    if rtn>0 then
        rtn = {}
        for ctr=0 to nfds-1 do
            rtn = rtn & ((peek(fdptr+6+(ctr*8)))+(peek(fdptr+7+(ctr*8))*256))
        end for
    end if
    free(fdptr)
    return rtn

end function

-------------------------------------------------------------------------------

function windows_poll(sequence socket_list, atom timeout)
atom wps_found, wps_index, wps_select
atom eventlist
object result
  -- WSAPoll is only available on Windows Vista+.
    if poll_>0 then
        return linux_poll(socket_list,timeout)
    end if
  -- windows_poll_seq = {{atom socket, atom events, atom event_handle, atom closeevent_status},...}
    for ctr=1 to length(socket_list) do
        if sequence(socket_list[ctr]) and length(socket_list[ctr])>=2 and
        atom(socket_list[ctr][1]) and atom(socket_list[ctr][2]) then
            wps_found = 0
            for wps=1 to length(windows_poll_seq) do
                if windows_poll_seq[wps][1]=socket_list[ctr][1] then
                    wps_found = 1
                    wps_index = wps
                    if socket_list[ctr][2]=0 then
                        windows_poll_seq[wps][4] = c_func(wsacloseevent_,{windows_poll_seq[wps][3]})
                        windows_poll_seq[wps][3] = -1
                    elsif socket_list[ctr][2]!=windows_poll_seq[wps][2] then
                        windows_poll_seq[wps][4] = c_func(wsacloseevent_,{windows_poll_seq[wps][3]})
                        windows_poll_seq[wps][3] = 0
                        windows_poll_seq[wps][2] = socket_list[ctr][2]
                    end if
                    exit
                end if
            end for
            if not wps_found then
                windows_poll_seq = append(windows_poll_seq,{socket_list[ctr][1],socket_list[ctr][2],0,0})
                wps_index = length(windows_poll_seq)
            end if
        end if
    end for
    wps_found = 1   -- Remove cancelled polls
    while wps_found<=length(windows_poll_seq) do
        if windows_poll_seq[wps_found][3]!= -1 then
            wps_found = wps_found+1
        else
            windows_poll_seq = windows_poll_seq[1..wps_found-1] &
                               windows_poll_seq[wps_found+1..length(windows_poll_seq)]
        end if
    end while
    if length(windows_poll_seq)>0 then
        eventlist = allocate(4*length(windows_poll_seq))
    else
        return -1
    end if
    for ctr=1 to length(windows_poll_seq) do -- Link an event to a socket poll
        if windows_poll_seq[ctr][3]=0 then
            windows_poll_seq[ctr][3] = c_func(wsacreateevent_,{})
            wps_select = 0
            if and_bits(windows_poll_seq[ctr][2],POLLIN) then
                wps_select = or_bits(wps_select,FD_READ)
                wps_select = or_bits(wps_select,FD_ACCEPT)
            end if
            if and_bits(windows_poll_seq[ctr][2],POLLPRI) then
                wps_select = or_bits(wps_select,FD_OOB)
            end if
            if and_bits(windows_poll_seq[ctr][2],POLLOUT) then
                wps_select = or_bits(wps_select,FD_WRITE)
            end if
            if and_bits(windows_poll_seq[ctr][2],POLLHUP) then
                wps_select = or_bits(wps_select,FD_CLOSE)
            end if
      -- Other poll flags are not supported on Windows other than Vista.
            windows_poll_seq[ctr][4] = c_func(wsaeventselect_,{windows_poll_seq[ctr][1],
                                                               windows_poll_seq[ctr][3],wps_select})
        end if
        poke4(eventlist+(4*(ctr-1)),windows_poll_seq[ctr][3])
    end for
    wps_found = c_func(wsawaitformultipleevents_,{length(windows_poll_seq),
                                                  eventlist,0,timeout,0})
    free(eventlist)
    if wps_found=WSA_WAIT_TIMEOUT then
        return 0
    end if
    if wps_found>=0 and wps_found<length(windows_poll_seq) then
        -- zero-based indexing
        result = {}
        for ctr=1 to length(socket_list) do
            if windows_poll_seq[wps_found+1][1]!=socket_list[ctr][1] then
                result = result & 0
            else
                result = result & windows_poll_seq[wps_found+1][2]
-- Issue: After closing the event, attempting to accept the connection results
-- in WSAENOTSOCK.
--        windows_poll_seq[wps_found+1][4] = c_func(wsacloseevent_,{windows_poll_seq[wps_found+1][3]})
--        windows_poll_seq[wps_found+1][3] = 0
--        windows_poll_seq[wps_found+1][2] = 0
            end if
        end for
        return result
    end if
    return wps_found

end function

-------------------------------------------------------------------------------

global function eunet_poll(sequence socket_list, atom timeout)
    if platform()=WINDOWS then
        return windows_poll(socket_list,timeout)
    elsif platform()=LINUX then
        return linux_poll(socket_list,timeout)
    else
        return -1
    end if
end function

-------------------------------------------------------------------------------
-- Send (requires bound/connected socket)
-------------------------------------------------------------------------------
-- Returns the # of chars sent, or -1 for error
function linux_send(atom socket, sequence data, atom flags)
atom datalen, dataptr, status
    datalen = length(data)
    dataptr = allocate(datalen+1)
    poke(dataptr,data&0)
    status = c_func(send_,{socket,dataptr,datalen,flags})
    free(dataptr)
    return status
end function

function windows_send(atom socket, sequence data, atom flags)
  -- Windows does send the same as Linux
    return linux_send(socket,data,flags)
end function

global function eunet_send(atom socket, sequence data, atom flags)
    if platform()=WINDOWS then
        return windows_send(socket,data,flags)
    elsif platform()=LINUX then
        return linux_send(socket,data,flags)
    else
        return -1
    end if
end function

-------------------------------------------------------------------------------
-- Sendto (good for stateless / broadcast datagrams)
-------------------------------------------------------------------------------
-- Returns the # of chars sent, or -1 for error

function linux_sendto(atom socket, sequence data, atom flags, sequence inet_addr)
atom dataptr, datalen, sockaddr
atom status
    sockaddr = make_sockaddr(inet_addr)
    datalen = length(data)
    dataptr = allocate(datalen+1)
    poke(dataptr,data&0)
    status = c_func(sendto_,{socket,dataptr,datalen,flags,sockaddr,16})
    free(sockaddr)
    free(dataptr)
    return status
end function

function windows_sendto(atom socket, sequence data, atom flags, sequence inet_addr)
  -- Windows does sendto the same as Linux
    return linux_sendto(socket,data,flags,inet_addr)
end function

global function eunet_sendto(atom socket, sequence data, atom flags, sequence inet_addr)
    if platform()=WINDOWS then
        return windows_sendto(socket,data,flags,inet_addr)
    elsif platform()=LINUX then
        return linux_sendto(socket,data,flags,inet_addr)
    else
        return -1
    end if
end function

-------------------------------------------------------------------------------
-- Sendmsg (sends both a message and control data)
-------------------------------------------------------------------------------
-- Returns the # of chars sent, or -1 for error

function linux_sendmsg()
    return -1
end function

function windows_sendmsg()
    return -1
end function

global function eunet_sendmsg()
    if platform()=WINDOWS then
        return windows_sendmsg()
    elsif platform()=LINUX then
        return linux_sendmsg()
    else
        return -1
    end if
end function

-------------------------------------------------------------------------------
-- recv (for connected sockets)
-------------------------------------------------------------------------------
-- Returns either a sequence of data, or a 2-element sequence {ERROR_CODE,ERROR_STRING}.
function linux_recv(atom socket, atom flags)
atom buf, buflen, rtnlen
sequence rtndata
    buflen = BLOCK_SIZE
    buf = allocate(buflen)
    rtnlen = c_func(recv_,{socket,buf,buflen,flags})
    if rtnlen<0 then
        free(buf)
        return eunet_get_error()
    end if
    rtndata = peek({buf,rtnlen})
    free(buf)
    return rtndata
end function

function windows_recv(atom socket, atom flags)
  -- Windows does recv the same as Linux
  -- The flag MSG_DONTWAIT is a Linux-only extension, and should not be used
  -- in cross-platform applications.
  -- From the Linux Man page on recv:
  -- The MSG_DONTWAIT flag requests the call to return when it would block otherwise.
  -- If no data is available, errno is set to EAGAIN. This flag is not available in
  -- strict ANSI or C99 compilation mode.
    return linux_recv(socket,flags)
end function

global function eunet_recv(atom socket, atom flags)
    if platform()=WINDOWS then
        return windows_recv(socket,flags)
    elsif platform()=LINUX then
        return linux_recv(socket,flags)
    else
        return -1
    end if
end function

-------------------------------------------------------------------------------
-- recvfrom (for unconnected sockets)
-------------------------------------------------------------------------------
-- Returns a sequence {sequence data, {atom error_number, sequence error_string}, string peer_address}
function linux_recvfrom(atom socket, atom flags)
atom buf, buflen, rtnlen, sockaddr
sequence rtndata, peer_addr, errno
    buflen = BLOCK_SIZE
    buf = allocate(buflen)
    sockaddr = allocate(20)
    poke4(sockaddr,16)
    rtnlen = c_func(recvfrom_,{socket,buf,buflen,flags,sockaddr+4,sockaddr})
    errno = eunet_get_error()
    if not find(errno[1],{EAGAIN,EBADF,EWOULDBLOCK,ECONNRESET,EFAULT,EINTR,EINVAL,EIO,ENOBUFS,ENOMEM,
                          ENOSR,ENOTCONN,ENOTSOCK,EOPNOTSUPP,ETIMEDOUT}) then
     -- Only these errors are thrown by recvfrom.  Any other error is from a different function call.
        errno = {0,""}
    end if
    peer_addr = get_sockaddr(sockaddr+4)
    free(sockaddr)
    if rtnlen<0 then
        free(buf)
        return {{},errno,peer_addr}
    end if
    rtndata = peek({buf,rtnlen})
    free(buf)
    return {rtndata,errno,peer_addr}

end function

function windows_recvfrom(atom socket, atom flags)
  -- Windows does recvfrom the same as Linux
    return linux_recvfrom(socket,flags)
end function

global function eunet_recvfrom(atom socket, atom flags)
    if platform()=WINDOWS then
        return windows_recvfrom(socket,flags)
    elsif platform()=LINUX then
        return linux_recvfrom(socket,flags)
    else
        return -1
    end if
end function

-------------------------------------------------------------------------------
-- recvmsg (also contains control codes)
-------------------------------------------------------------------------------

--PL unused...
--/*
function linux_recvmsg()
    return -1
end function
--*/

--PL unused...
--/*
function windows_recvmsg()
    return -1
end function
--*/

global function eunet_recvmsg()
    return -1
end function

-------------------------------------------------------------------------------
-- Socket options
-------------------------------------------------------------------------------
-- Get_socket_options returns an OBJECT containing the option value, or {"ERROR",errcode} on error.
-- Set_socket_options returns 0 on success and -1 on error.

function linux_getsockopts(atom socket, integer level, integer optname)
object rtn
atom buf, status, bufsiz
    buf = allocate(1028)
    poke4(buf,1024)
    status = c_func(getsockopts_,{socket,level,optname,buf+4,buf})
    if status=0 then
        bufsiz = peek4u(buf)
        if bufsiz=0 then
            rtn = {}
        elsif bufsiz=4 then
            rtn = peek4u(buf+4)
        else
            rtn = {}
            for ctr=1 to bufsiz do
                rtn = rtn & peek(buf+3+ctr)
            end for
        end if
    else
        rtn = {"ERROR",status}
    end if
    free(buf)
    return rtn

end function

-------------------------------------------------------------------------------

function windows_getsockopts(atom socket, integer level, integer optname)
    return linux_getsockopts(socket,level,optname)
end function

-------------------------------------------------------------------------------

global function eunet_get_socket_options(atom socket, integer level, integer optname)
    if platform()=WINDOWS then
        return windows_getsockopts(socket,level,optname)
    elsif platform()=LINUX then
        return linux_getsockopts(socket,level,optname)
    end if
    return {"ERROR",-999}
end function

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

function linux_setsockopts(atom socket, integer level, integer optname, object val)
object rtn
atom buf, bufsiz
    if atom(val) then
        buf = allocate(8)
        bufsiz = 4
        poke4(buf,bufsiz)
        poke4(buf+4,val)
    else
        buf = allocate(length(val)+4)
        bufsiz = length(val)
        poke4(buf,bufsiz)
        poke(buf+4,val)
    end if
    rtn = c_func(setsockopts_,{socket,level,optname,buf+4,buf})
    return rtn
end function

-------------------------------------------------------------------------------

function windows_setsockopts(atom socket, integer level, integer optname, object val)
    return linux_setsockopts(socket,level,optname,val)
end function

-------------------------------------------------------------------------------

global function eunet_set_socket_options(atom socket, integer level, integer optname, object val)
    if platform()=WINDOWS then
        return windows_setsockopts(socket,level,optname,val)
    elsif platform()=LINUX then
        return linux_setsockopts(socket,level,optname,val)
    end if
    return -999
end function

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Getters
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- DnsQuery
-------------------------------------------------------------------------------
-- Returns a set of sequences of {ip_addr, q_type, order} resolving the IP address for
-- the given domain name and/or host.
-- At present, only A,MX,and NS queries are supported.
-- Error 9501 = No record found

function linux_dnsquery(sequence dname, integer q_type)

atom nameptr, rtnptr, success, ptrptr, dnameptr, qlen
atom answer_start, answer_end, num_as_rec, num_qr_rec
sequence rtn, line, subx
object temp

    nameptr = allocate_string(dname&0)
    rtnptr = allocate(BLOCK_SIZE*8)
    success = c_func(dnsquery_,{nameptr,NS_C_IN,q_type,rtnptr,BLOCK_SIZE*8})
    if success<0 then
        free(nameptr)
        free(rtnptr)
        return success
    end if
  -- The parsing of rtnptr is significantly more difficult in Linux than
  -- in Windows.  Much of the code that follows is based on Postfix-2.5.1/
  -- src/dns/dns_lookup.c.
    if success>BLOCK_SIZE*8 then    -- The Answer has been truncated & is invalid.
        free(nameptr)
        free(rtnptr)
        return -2
    end if
  -- Size of Header = 12 bytes.  # Query = [5..6], # Answer = [7..8], # Auth = [9..10]
  -- # Res = [11..12]
    num_qr_rec = (peek(rtnptr+4)*256)+peek(rtnptr+5)
    num_as_rec = (peek(rtnptr+6)*256)+peek(rtnptr+7)
    if num_as_rec=0 then
        free(nameptr)
        free(rtnptr)
        return 9501  -- No Data
    end if
    ptrptr = rtnptr+12 -- Start of query record
    dnameptr = allocate(1024)
    for ctr=1 to num_qr_rec do
        qlen = c_func(dnsexpand_,{rtnptr,rtnptr+success,ptrptr,dnameptr,1024})
        if qlen<0 then
            free(dnameptr)
            free(nameptr)
            free(rtnptr)
            return qlen
        end if
        ptrptr = ptrptr+qlen+4
    end for
    answer_start = ptrptr
    answer_end = rtnptr+success

  -- Now we're finally at the answer section
    rtn = {}
    for seq=1 to num_as_rec do
        line = repeat(0,8)
        subx = repeat(0,32)
        if ptrptr>=answer_end then
            free(dnameptr)
            free(nameptr)
            free(rtnptr)
            return -4
        end if
        qlen = c_func(dnsexpand_,{rtnptr,answer_end,ptrptr,dnameptr,1024})
        if qlen<0 then
            free(dnameptr)
            free(nameptr)
            free(rtnptr)
            return -5
        end if
        line[2] = peek_string(dnameptr)
        ptrptr = ptrptr+qlen
        if ptrptr+10>=answer_end then
            free(dnameptr)
            free(nameptr)
            free(rtnptr)
            return -4
        end if
        line[3] = (peek(ptrptr)*256)+peek(ptrptr+1) -- type
        line[7] = (peek(ptrptr+2)*256)+peek(ptrptr+3) -- Class
        line[6] = (peek(ptrptr+4)*256*256*256)+(peek(ptrptr+5)*256*256)+
                  (peek(ptrptr+6)*256)+peek(ptrptr+7)  -- TTL
        line[4] = (peek(ptrptr+8)*256)+peek(ptrptr+9)  -- Data Length
        ptrptr = ptrptr+10
        if ptrptr+line[4]-1>=answer_end then
            free(dnameptr)
            free(nameptr)
            free(rtnptr)
            return -4
        end if
        if line[3]=NS_T_NS then
            qlen = c_func(dnsexpand_,{rtnptr,answer_end,ptrptr,dnameptr,1024})
            if qlen>0 then
                subx[1] = peek_string(dnameptr)
                temp = linux_dnsquery(subx[1],NS_T_A)
                if atom(temp) then
                    rtn = append(rtn,{subx[1],line[3],seq})
                else
                    for ctr=1 to length(temp) do
                        rtn = append(rtn,{temp[ctr][1],line[3],seq+ctr-1})
                    end for
                end if
            end if
        elsif line[3]=NS_T_MX then
            subx[2] = (peek(ptrptr)*256)+peek(ptrptr+1)  -- Priority
            qlen = c_func(dnsexpand_,{rtnptr,answer_end,ptrptr+2,dnameptr,1024})
            if qlen>0 then
                subx[1] = peek_string(dnameptr)
                temp = linux_dnsquery(subx[1],NS_T_A)
                if atom(temp) then
                    rtn = append(rtn,{subx[1],line[3],subx[2]})
                else
                    for ctr=1 to length(temp) do
                        rtn = append(rtn,{temp[ctr][1],line[3],subx[2]+ctr-1})
                    end for
                end if
            end if
        elsif line[3]=NS_T_A and line[4]>=4 then
            subx[1] = sprintf("%d.%d.%d.%d",{peek(ptrptr),peek(ptrptr+1),
                                             peek(ptrptr+2),peek(ptrptr+3)})
            if q_type=NS_T_ANY or q_type=NS_T_A then
                rtn = append(rtn,{subx[1],line[3],seq})
            end if
        elsif line[3]=NS_T_PTR then

        end if
        ptrptr = ptrptr+line[4]

    end for

  -- Finally, we're done.
    free(dnameptr)
    free(nameptr)
    free(rtnptr)

    return rtn

end function

-------------------------------------------------------------------------------

function windows_dnsquery(sequence dname, integer q_type, atom options)

  -- NOTE: This function does not work on Windows versions below Windows 2000.

atom success,nameptr, rtnptr, recptr, sqn
sequence rtn, line, subx
object temp

    if dnsquery_<0 then
        return -999
    end if

    nameptr = allocate_string(dname)
    rtnptr = allocate(4)
    success = c_func(dnsquery_,{nameptr,q_type,options,0,rtnptr,0})
    if success!=0 then
        free(nameptr)
        free(rtnptr)
        return success
    end if
    rtn = {}
    recptr = peek4u(rtnptr)
    sqn = 1
    while recptr>0 do
        line = repeat(0,8)
        subx = repeat(0,32)
        line[1] = peek4u(recptr) -- Pointer to the next record
        line[2] = peek4u(recptr+4) -- Pointer to the name string
        line[3] = peek(recptr+8)+(peek(recptr+9)*256) -- type
        line[4] = peek(recptr+10)+(peek(recptr+11)*256) -- Data Length
        line[5] = peek4u(recptr+12) -- Flags
        line[6] = peek4u(recptr+16) -- TTL
        line[7] = peek4u(recptr+20) -- reserved
        if line[3]=NS_T_MX then
            subx[1] = peek_string(peek4u(recptr+24)) -- Mail server name
            subx[2] = peek(recptr+28)+(peek(recptr+29)*256) -- Preference
            temp = windows_dnsquery(subx[1],NS_T_A,options)
            if atom(temp) then
                rtn = append(rtn,{subx[1],line[3],subx[2]})
            else
                for ctr=1 to length(temp) do
                    rtn = append(rtn,{temp[ctr][1],line[3],subx[2]+ctr-1})
                end for
            end if
        elsif line[3]=NS_T_NS then
            subx[1] = peek_string(peek4u(recptr+24)) -- NS server name
            temp = windows_dnsquery(subx[1],NS_T_A,options)
            if atom(temp) then
                rtn = append(rtn,{subx[1],line[3],sqn})
            else
                for ctr=1 to length(temp) do
                    rtn = append(rtn,{temp[ctr][1],line[3],sqn+ctr-1})
                end for
            end if
        elsif line[3]=NS_T_A then
            subx[1] = sprintf("%d.%d.%d.%d",{peek(recptr+24),peek(recptr+25),
                                             peek(recptr+26),peek(recptr+27)})
            if q_type=NS_T_ANY or q_type=NS_T_A then
                rtn = append(rtn,{subx[1],line[3],sqn})
            end if
        elsif line[3]=NS_T_PTR then

        end if
        recptr = line[1]
        sqn += 1
    end while
    c_proc(dnsrlfree_,{peek4u(rtnptr),1})
    free(nameptr)
    free(rtnptr)

    return rtn

end function

-------------------------------------------------------------------------------

global function eunet_dnsquery(sequence dname, integer q_type, atom options)
    if platform()=WINDOWS then
        return windows_dnsquery(dname,q_type,options)
    elsif platform()=LINUX then
        return linux_dnsquery(dname,q_type)
    end if
    return -999
end function

-------------------------------------------------------------------------------
-- getmxrr
-------------------------------------------------------------------------------

global function eunet_getmxrr(sequence dname, atom options)
object rtn

  -- Error 9003 = MS: RCODE_NAME_ERROR - Something's there, but it's not exact.
  -- Error 9501 = No Data Found

    dname = trim(dname)
    rtn = eunet_dnsquery(dname,NS_T_MX,options)
    if sequence(rtn) and length(rtn)>0 then
        return rtn
    end if
    if rtn=9501 or rtn=9003 or rtn= -1 or
    (sequence(rtn) and length(rtn)=0) then
        rtn = eunet_dnsquery("mail."&dname,NS_T_MX,options)
    end if
    return rtn
end function

-------------------------------------------------------------------------------
-- getnsrr
-------------------------------------------------------------------------------

global function eunet_getnsrr(sequence dname, atom options)
    return eunet_dnsquery(dname,NS_T_NS,options)
end function

-------------------------------------------------------------------------------
-- GetHostByName (deprecated - replaced by GetAddrInfo)
-------------------------------------------------------------------------------

function linux_gethostbyname(sequence name)

-- Based on SOCKS.EXU demo from Irv Mullins.

atom hostent,name_ptr,host_addr_ptr
sequence host_addr

    name_ptr = allocate_string(name)
    hostent = c_func(gethostbyname_,{name_ptr})
    free(name_ptr)
    if hostent=0 then
        return ""
    end if
    host_addr_ptr = peek4u(hostent+16)  -- May be hostent+12 on Windows, may be hostent+16 on Linux
    if host_addr_ptr>0 then
        host_addr = sprintf("%d.%d.%d.%d",peek({peek4u(host_addr_ptr),4}))
    else
        host_addr = ""
    end if
    free(hostent)
    return host_addr

end function

-------------------------------------------------------------------------------

function windows_gethostbyname(sequence name)
  -- The returned structure may not be the same on Windows and Linux, but
  -- we'll assume they are for now.

atom hostent,name_ptr,host_addr_ptr
sequence host_addr

    name_ptr = allocate_string(name)
    hostent = c_func(gethostbyname_,{name_ptr})
    free(name_ptr)
    if hostent=0 then
        return ""
    end if
    host_addr_ptr = peek4u(hostent+12)  -- May be hostent+12 on Windows, may be hostent+16 on Linux
    if host_addr_ptr>0 then
        host_addr = sprintf("%d.%d.%d.%d",peek({peek4u(host_addr_ptr),4}))
    else
        host_addr = ""
    end if
  -- Windows does not permit freeing the hostent structure
    return host_addr

end function

-------------------------------------------------------------------------------

global function eunet_gethostbyname(sequence name)
    if platform()=WINDOWS then
        return windows_gethostbyname(name)
    elsif platform()=LINUX then
        return linux_gethostbyname(name)
    else
        return -999
    end if
end function

-------------------------------------------------------------------------------
-- GetServByName (deprecated - replaced by GetAddrInfo)
-------------------------------------------------------------------------------
-- Returns the (integer) port number of the service

function linux_getservbyname(sequence name)
-- Based on SOCKS.EXU demo from Irv Mullins.
atom name_ptr,port_ptr,port

    name_ptr = allocate_string(name)
    port_ptr = c_func(getservbyname_,{name_ptr,0})
    if port_ptr=0 then
        free(name_ptr)
        return 0
    end if
    port = (peek(port_ptr+8)*256)+(peek(port_ptr+9))
    free(name_ptr)
    return port

end function

-------------------------------------------------------------------------------

function windows_getservbyname(sequence name)
    return linux_getservbyname(name)
end function

-------------------------------------------------------------------------------

function eunet_getservbyname(sequence name)
    if platform()=WINDOWS then
        return windows_getservbyname(name)
    elsif platform()=LINUX then
        return linux_getservbyname(name)
    else
        return -999
    end if
end function

-------------------------------------------------------------------------------
-- GetAddrInfo
-------------------------------------------------------------------------------
-- Returns a sequence of sequences {atom flags, atom family, atom socktype, atom protocol, sequence inet_addr}
-- on success or an error code on failure

--memset(&hints, 0, sizeof(hints));
--hints.ai_flags = AI_NUMERICHOST;
--hints.ai_family = PF_UNSPEC;
--hints.ai_socktype = 0;
--hints.ai_protocol = 0;
--hints.ai_addrlen = 0;
--hints.ai_canonname = NULL;
--hints.ai_addr = NULL;
--hints.ai_next = NULL;
--getaddrinfo(ip, port, &hints, &aiList)
--nodename A pointer to a NULL-terminated ANSI string that contains a host (node) name or a numeric host address string. For the Internet protocol, the numeric host address string is a dotted-decimal IPv4 address or an IPv6 hex address.
--servname A pointer to a NULL-terminated ANSI string that contains either a service name or port number represented as a string.
--hints A pointer to an addrinfo structure that provides hints about the type of socket the caller supports. See Remarks.
--res A pointer to a linked list of one or more addrinfo structures that contains response information about the host.

function linux_getaddrinfo(object node, object service, object /*hints*/)
atom addrinfo, success, node_ptr, service_ptr, hints_ptr, addrinfo_ptr, svcport
atom cpos
sequence rtn, val

    addrinfo = allocate(32)
    poke(addrinfo,repeat(0,32))
    if sequence(node) then
        node_ptr = allocate_string(node)
    else
        node_ptr = node
    end if
    svcport = 0
    if sequence(service) then
        service_ptr = allocate_string(service)
        val = value(service)
        if val[1]=GET_SUCCESS then
            svcport = val[2]
        end if
    else
        service_ptr = service
        if service>0 and service<=#FFFF then
            svcport = service
            service_ptr = 0
        end if
    end if
    hints_ptr = 0    -- Not yet implemented
    success = c_func(getaddrinfo_,{node_ptr,service_ptr,hints_ptr,addrinfo})
    if success!=0 then
        free(addrinfo)
        if sequence(node) then free(node_ptr) end if
        if sequence(service) then free(service_ptr) end if
        return success
    end if
    rtn = {}
  -- addrinfo is a pointer to a pointer to a structure in Linux.
    addrinfo_ptr = peek4u(addrinfo)
-- 27 Nov 2007: Only one addrinfo structure is supported
--  while addrinfo_ptr != 0 do
    rtn = append(rtn,{
                      peek4u(addrinfo_ptr),
                      peek4u(addrinfo_ptr+4),
                      peek4u(addrinfo_ptr+8),
                      peek4u(addrinfo_ptr+12),
                      get_sockaddr(peek4u(addrinfo_ptr+20))
    })
    addrinfo_ptr = peek4u(addrinfo_ptr+28)
--  end while
    c_proc(freeaddrinfo_,{peek4u(addrinfo)})
    if svcport=0 and rtn[1][4]>0 then
        svcport = rtn[1][4]
    end if
    if length(rtn[1][5])=0 and sequence(node) then
        rtn[1][5] = eunet_gethostbyname(node)
        if sequence(service) and svcport=0 then
            rtn[1][5] = rtn[1][5] & sprintf(":%d",eunet_getservbyname(service))
        elsif svcport>0 then
            rtn[1][5] = rtn[1][5] & sprintf(":%d",svcport)
        end if
    elsif svcport>0 then
        cpos = find(':',rtn[1][5])
        if cpos=0 or cpos=length(rtn[1][5]) or
        compare(rtn[1][5][length(rtn[1][5])-1..length(rtn[1][5])],":0")=0 then
            if cpos=0 then
                rtn[1][5] = rtn[1][5] & sprintf(":%d",svcport)
            else
                rtn[1][5] = rtn[1][5][1..cpos-1] & sprintf(":%d",svcport)
            end if
        end if
    end if
    free(addrinfo)
    return rtn
end function

-------------------------------------------------------------------------------

function windows_getaddrinfo(object node, object service, object hints)
    return linux_getaddrinfo(node,service,hints)
end function

-------------------------------------------------------------------------------

global function eunet_getaddrinfo(object node, object service, object hints)
    if platform()=WINDOWS then
        return windows_getaddrinfo(node,service,hints)
    elsif platform()=LINUX then
        return linux_getaddrinfo(node,service,hints)
    end if
    return -999
end function

global function eunet_get_addrinfo(object node, object service, object hints)
  -- Added version 1.3.1 for consistency.
    return eunet_getaddrinfo(node,service,hints)
end function

-----------------------------------------------------------------------------------
--URL-encoding
-----------------------------------------------------------------------------------
--HTML form data is usually URL-encoded to package it in a GET or POST submission. In a nutshell, here's how you URL-encode the name-value pairs of the form data:
--   1. Convert all "unsafe" characters in the names and values to "%xx", where "xx" is the ascii value of the character, in hex. "Unsafe" characters include =, &, %, +, non-printable characters, and any others you want to encode-- there's no danger in encoding too many characters. For simplicity, you might encode all non-alphanumeric characters.
--   2. Change all spaces to plusses.
--   3. String the names and values together with = and &, like
--          name1=value1&name2=value2&name3=value3
--   4. This string is your message body for POST submissions, or the query string for GET submissions. 
--For example, if a form has a field called "name" that's set to "Lucy", and a field called "neighbors" that's set to "Fred & Ethel", the URL-encoded form data would be
--    name=Lucy&neighbors=Fred+%26+Ethel <<== note no \n or \r
--with a length of 34.

constant alphanum = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890/", -- encode all else
         hexnums = "0123456789ABCDEF"

global function eunet_urlencode(string url)
-- Function added by Kathy Smith (Kat)(KAT12@coosahs.net), version 1.3.0
sequence encoded = ""
    for i=1 to length(url) do
        integer ch = url[i]
        if find(ch,alphanum) then
            encoded &= ch
        else
            integer nib1 = floor(ch/16),
                    nib2 = floor(ch-(nib1*16))
            encoded &= "%" & hexnums[nib1+1] & hexnums[nib2+1]
        end if
    end for
    return encoded
end function

-------------------------------------------------------------------------------
-- sendheader manipulation
-------------------------------------------------------------------------------

global function eunet_get_sendheader(object field)

  -- if field is 0, return the whole sequence.
  -- if field is 1..length(sendheader), return just that field
  -- if field is invalid, return -1.
  -- if field is a sequence, try to match it to sendheader[x][1].

-- Kat: should i return [1] & [2] as well? Mike: yes
-- most server interfaces return the only value, saves parsing
-- we'll return a {"Name","spacer","value"} format

sequence upperfield

  -- Function added by Kathy Smith (Kat)(KAT12@coosahs.net), version 1.3.0
    if sequence(field) then
        upperfield = upper(field)
        for x=1 to length(sendheader) do
            if equal(upperfield,upper(sendheader[x][1])) then
                return sendheader[x]
            end if
        end for
        return -1
    elsif field<0 or field>length(sendheader) then
        return -1
    elsif field=0 then
        return sendheader
    else
        return sendheader[field]
    end if
end function

-------------------------------------------------------------------------------

global procedure eunet_set_sendheader_default()
  -- sets some defaults
  -- httpversion MUST come before GET in this program: some servers default to 1.0, even if you say 1.1
  -- NO spaces around [3] on httpversion
  -- POSTDATA MUST come before Content-Length in this program
  -- Referer is often used by sites to be sure your fetch was from one of their own pages
  -- headers with [3] = "" won't be sent

  -- Function added by Kathy Smith (Kat)(KAT12@coosahs.net), version 1.3.0
    sendheader = { -- you can add more [1], and modify [3], [2] is the ' ' or ": " (GET has no ": ")
                  {"httpversion","","HTTP/1.0"}, -- not a legal http headerline, but to append to GET later
                  {"GET"," ",""}, -- [3] = the filename you want
                  {"Host",": ",""},
                  {"Referer",": ",""}, -- i know it's misspelled, but that's official!
                  {"User-Agent",": ","Opera/5.02 (Windows 98; U)  [en]"}, --</joke> pick your own :-)
                  {"Accept",": ","*/*"},
                  {"Accept-Charset",": ","ISO-8859-1,utf-8;q=0.7,*;q=0.7"},
                  {"Accept-Encoding",": ","identity"}, -- "identity" = no decoder in eunet so far
                  {"Accept-Language",": ","en-us"}, -- pick your own language abbr here
                  {"Accept-Ranges",": ",""},
                  {"Authorization",": ",""},
                  {"Date",": ",""}, -- who cares if the server has my time?
                  {"If-Modified-Since",": ",""},
                  {"POST"," ",""}, -- if POST, obviously
                  {"POSTDATA","",""}, -- not a legal headerline, but has to go somewhere
                  {"Content-Type",": ",""}, -- if POST transaction
                  {"Content-Length",": ",""}, -- if POST transaction
                  {"From",": ",""}, -- possible in POST or Authorization
                  {"Keep-Alive",": ","0"},
                  {"Cache-Control",": ","no"},
                  {"Connection",": ","close"}
                 }

end procedure --defaultsetsendheaderline()             

--------------------------------------------------------------------------------------------
-- I did this, this way, so you can specify the header
-- without the ": "
-- and regardless of CamelCase or CASE - Kat
global procedure eunet_set_sendheader(object whatheader, sequence whatdata)

    if atom(whatheader) then
        if whatheader>0 and whatheader<=length(sendheader) then
            sendheader[whatheader][3] = whatdata
        end if
        return
    end if

  -- Function added by Kathy Smith (Kat)(KAT12@coosahs.net), version 1.3.0
    for i=1 to length(sendheader) do
        if match(upper(whatheader),upper(sendheader[i][1])) then
            sendheader[i][3] = whatdata
            return
        end if
    end for

--  sendheader &= { whatheader & whatdata } -- you better know what you are doing here!
    sendheader = append(sendheader,{whatheader, ": ",whatdata})

end procedure -- setsendheaderline(sequence whatheader, sequence whatdata)

--------------------------------------------------------------------------------

global procedure eunet_set_sendheader_useragent_msie()
    eunet_set_sendheader("UserAgent","Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)")
end procedure

--------------------------------------------------------------------------------
-- this can also be used to flatten the sendheader record, for printing, etc
function eunet_format_sendheader()
-- Function added by Kathy Smith (Kat)(KAT12@coosahs.net), version 1.3.0
sequence tempheader, temppostdata, httpversion
    tempheader = ""
    temppostdata = ""
    httpversion = ""
    for i=1 to length(sendheader) do
        if not equal("",sendheader[i][3]) and
        not equal("httpversion",sendheader[i][1]) and
        not equal("POSTDATA",sendheader[i][1]) then
            if equal("GET",sendheader[i][1])
            then tempheader &= sendheader[i][1] & sendheader[i][2] & sendheader[i][3] & " " & httpversion & "\n"
            else tempheader &= sendheader[i][1] & sendheader[i][2] & sendheader[i][3] & "\r\n"
            end if
        end if
        if equal("POSTDATA",sendheader[i][1]) and not equal("",sendheader[i][3]) then
--          temppostdata = urlencode(sendheader[i][3])
            temppostdata = sendheader[i][3]
            eunet_set_sendheader("Content-Length",sprintf("%d",length(temppostdata)))
        end if
        if equal("httpversion",sendheader[i][1]) and not equal("",sendheader[i][3]) then
            httpversion = sendheader[i][3]
        end if
    end for

    tempheader &= "\r\n" -- end of header
    if not equal(temppostdata,"") then  -- but if there's POST data,
        tempheader &= temppostdata      -- tack that on too
    end if

    return tempheader

end function

-------------------------------------------------------------------------------

global function eunet_parse_httpheader(sequence header)

  ---*** THIS FUNCTION IS DEPRECATED IN VERSION 1.3.1 ***---

atom cpos, verdone
sequence recvheader, parsedheader, matchlist
    matchlist = {"HTTPVERSION","GET","HOST","REFERER","USER-AGENT","ACCEPT",
                 "ACCEPT-CHARSET","ACCEPT-ENCODING","ACCEPT-LANGUAGE","ACCEPT-RANGES",
                 "AUTHORIZATION","DATE","IF-MODIFIED-SINCE","POST","POSTDATA",
                 "CONTENT-TYPE","CONTENT-LENGTH","FROM","KEEP-ALIVE","CACHE-CONTROL",
                 "CONNECTION"}
    recvheader = {
                  {"httpversion","",""}, -- not a legal http headerline, but to append to GET later
                  {"GET"," ",""}, -- the filename you want
                  {"Host",": ",""},
                  {"Referer",": ",""}, -- i know it's misspelled, but that's official!
                  {"User-Agent",": ",""},
                  {"Accept",": ",""},
                  {"Accept-Charset",": ",""},
                  {"Accept-Encoding",": ",""}, -- "identity" = no decoder in eunet so far
                  {"Accept-Language",": ",""},
                  {"Accept-Ranges",": ",""},
                  {"Authorization",": ",""},
                  {"Date",": ",""}, -- who cares if the server has my time?
                  {"If-Modified-Since",": ",""},
                  {"POST"," ",""}, -- if POST, obviously
                  {"POSTDATA","",""}, -- not a legal headerline, but has to go somewhere
                  {"Content-Type",": ",""}, -- if POST transaction
                  {"Content-Length",": ",""}, -- if POST transaction
                  {"From",": ",""}, -- possible in POST or Authorization
                  {"Keep-Alive",": ",""},
                  {"Cache-Control",": ",""},
                  {"Connection",": ",""}
                 }

    parsedheader = {}
    cpos = find('\n',header)
    while length(header)>0 and cpos>0 do
        parsedheader = append(parsedheader,{trim(header[1..cpos-1])})
        header = header[cpos+1..length(header)]
        cpos = find('\n',header)
    end while
    if length(header)>0 then
        parsedheader = append(parsedheader,{trim(header)})
    end if

    verdone = 0
    for ctr=1 to length(parsedheader) do
        cpos = find(' ',parsedheader[ctr][1])
        if not verdone and match("HTTP/",upper(parsedheader[ctr][1]))>0 then
            if cpos<6 then
                cpos = length(parsedheader[ctr])
            end if
            parsedheader[ctr] = {"httpversion",parsedheader[ctr][1][6..cpos-1]}
            verdone = 1
        elsif cpos>0 then
            parsedheader[ctr] = {parsedheader[ctr][1][1..cpos-1],
                                 parsedheader[ctr][1][cpos+1..length(parsedheader[ctr][1])]}
            if parsedheader[ctr][1][length(parsedheader[ctr][1])]=':' then
                parsedheader[ctr][1] = parsedheader[ctr][1][1..length(parsedheader[ctr][1])-1]
            end if
        end if
    end for
    for i=1 to length(parsedheader) do
        if length(parsedheader[i])>1 then
            cpos = find(upper(parsedheader[i][1]),matchlist)
            if cpos>0 then
                recvheader[cpos][3] = parsedheader[i][2]
            else
                recvheader = append(recvheader,{parsedheader[i][1],": ",parsedheader[i][2]})
            end if
        end if
    end for

    return recvheader

end function

-------------------------------------------------------------------------------

 -- recvheader is a global sequence
 -- what else to call httpversion line? It also has the http reply code, like 200, 404, 206, etc
global procedure eunet_parse_recvheader(sequence header)

  -- Function added by Kathy Smith (Kat)(KAT12@coosahs.net), version 1.3.1
sequence junk
atom place

    junk = {"",""} -- init it, it looks like this
    recvheader = eunet_parse(header,{10,13}) -- could be \n or \r or both
    for i=1 to length(recvheader) do
        place = match(": ",recvheader[i])
        if place then
            junk[1] = recvheader[i][1..place-1]
            junk[2] = recvheader[i][place+2..length(recvheader[i])]
            recvheader[i] = junk
        else
            if match("HTTP/",upper(recvheader[i])) then
                recvheader[i] = {"httpversion",recvheader[i]} -- what else to call that line?
            end if
        end if
    end for
end procedure

-------------------------------------------------------------------------------

global function eunet_get_recvheader(object field)
sequence upperfield
  -- recvheader was parsed out previously in eunet_parse_recvheader()
  -- if field is 0, return the whole sequence.
  -- if field is 1..length(recvheader), return just that field
  -- if field is invalid, return -1.
  -- if field is a sequence, try to match it to recvheader[x][1].

  -- we'll NOT return a {"Name","value"} format
  -- because that leads to using a junk seq to get the [2] from
  -- --> And yet, that's exactly what we're doing.  -- Mike.

  -- Function added by Kathy Smith (Kat)(KAT12@coosahs.net), version 1.3.1

    if sequence(field) and equal(field,"") then return -1 end if
    if atom(field) then
        if ( field<=0 ) or ( field>length(recvheader) ) then return -1 end if
        return recvheader[field]
    end if

    upperfield = upper(field)
    for i=1 to length(recvheader) do
        if equal(upperfield,upper(recvheader[i][1])) then
            return recvheader[i] -- {"header_name","value"}
        end if
    end for

    return -1 -- not found!

end function

-------------------------------------------------------------------------------
-- get_http
-------------------------------------------------------------------------------
-- Returns a 2-element sequence of {sequence headerdata, sequence of data bytes}

global function eunet_get_http(sequence inet_addr, sequence hostname, sequence file)

atom socket, success, last_data_len
sequence header, data, hline

-- Notes for future additions:
--GET /index.html HTTP/1.1
--Host: www.amazon.com
--Accept: */*
--Accept-Language: en-us
--User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)

    if length(inet_addr)=0 then
        return {"",""}
    end if

  -- Modification added by Kathy Smith (Kat)(KAT12@coosahs.net), version 1.3.0 {
    if equal(sendheader,"") then
        eunet_set_sendheader_default() -- it really should be set to something! But isn't **required**
    end if

    if length(file)=0 or file[1]!='/' then file = '/'&file end if
    eunet_set_sendheader("GET",file)
-- TO DO: Allow transmission of POST data by using eunet_set_sendheader("POST",data)
    eunet_set_sendheader("HOST",hostname)
    hline = eunet_get_sendheader("Referer")
    if equal(hline[3],"") then
        eunet_set_sendheader("Referer",hostname)
    end if
    data = {}
    last_data_len = 0
    socket = eunet_new_socket(AF_INET,SOCK_STREAM,0)
    success = eunet_connect(socket,inet_addr)
    if success=0 then
--    success = eunet_send(socket,
--                  sprintf("GET /%s HTTP/1.0\nHost: %s\n\n",{file,hostname}),0)
        success = eunet_send(socket,eunet_format_sendheader(),0)
   -- } end version 1.3.0 mod
        while success>0 do
            data = data & eunet_recv(socket,0)
            success = length(data)-last_data_len
            last_data_len = length(data)
        end while
    end if
    if eunet_close_socket(socket) then end if

    success = match({13,10,13,10},data)
    if success>0 then
        header = data[1..success-1]
        eunet_parse_recvheader(header)
        data = data[success+4..length(data)]
    else
        header = data
        data = {}
    end if
    return {header,data}

end function

-------------------------------------------------------------------------------

global function eunet_get_http_use_cookie(sequence inet_addr, sequence hostname, sequence file)

atom socket, success, last_data_len, cpos, offset
sequence header, header2, body, data, updata, hline
sequence cookielist, request, cookie
    cookielist = {}
    request = ""
  -- cookie = {name,domain,path,expires,encrypted,version}

    if length(inet_addr)=0 then
        return {"",""}
    end if
  -- Modification added by Kathy Smith (Kat)(KAT12@coosahs.net), version 1.3.0 {
    if equal(sendheader,"") then
        eunet_set_sendheader_default() -- it really should be set to something! But isn't **required**
    end if

    if length(file)=0 or file[1]!='/' then file = '/'&file end if
    eunet_set_sendheader("GET",file)
    eunet_set_sendheader("HOST",hostname)
    hline = eunet_get_sendheader("Referer")
    if equal(hline[3],"") then
        eunet_set_sendheader("Referer",hostname)
    end if

    for ctr=1 to length(this_cookiejar) do
        if sequence(this_cookiejar[ctr]) and length(this_cookiejar[ctr])>=2 and
        sequence(this_cookiejar[ctr][1]) and
        (match(hostname,this_cookiejar[ctr][2])>0 or match(this_cookiejar[ctr][2],hostname)>0) and
        (length(file)=0 or match(this_cookiejar[ctr][3],file)>0) then
            cookielist = append(cookielist,this_cookiejar[ctr])
        end if
    end for

  -- TO DO: Sort cookielist by domain, path (longer path before shorter path)
--  request = sprintf("GET /%s HTTP/1.0\nHost: %s\n",{file,hostname})
    for idx=1 to length(cookielist) do
--      if idx = 1 then
--          request = request & "Cookie: "&cookielist[idx][1]
--      else
--          request = request & "        "&cookielist[idx][1]
--      end if
        request = request & cookielist[idx][1]
        if length(cookielist[idx][3])>0 then
            request = request & "; $Path=" & cookielist[idx][3]
        end if
        if idx<length(cookielist) then
--          request = request & ";\n"
            request = request & ";"
--      else
--          request = request & "\n"
        end if
    end for
--  request = request & "\n"
    eunet_set_sendheader("Cookie",request)

    data = {}
    last_data_len = 0
    socket = eunet_new_socket(AF_INET,SOCK_STREAM,0)
    success = eunet_connect(socket,inet_addr)
    if success=0 then
--      success = eunet_send(socket,request,0)
        success = eunet_send(socket,eunet_format_sendheader(),0)
        -- } end version 1.3.0 modification
        while success>0 do
            data = data & eunet_recv(socket,0)
            success = length(data)-last_data_len
            last_data_len = length(data)
        end while
    end if
    if eunet_close_socket(socket) then end if

    success = match({13,10,13,10},data)
    if success>0 then
        header = data[1..success-1]
        eunet_parse_recvheader(header)
        body = data[success+4..length(data)]
    else
        header = data
        body = {}
        data = {}
    end if

    header2 = header
    cpos = match("SET-COOKIE",upper(header2))
    while cpos>0 do
        header2 = header2[cpos+10..length(header2)]
        data = header2
        cpos = find(':',data)
        if cpos>0 then
            data = data[cpos+1..length(data)]
        end if
        offset = 0
        cpos = match(13&10,data)
        while cpos>1 and data[offset+cpos-1]=';' do
            offset = offset+cpos+2
            cpos = match(13&10,data[offset..length(data)])
        end while
        offset = offset+cpos-1
        data = data[1..offset]
        updata = upper(data)
        cookie = {"","","","","N",""}
        offset = match("PATH=",updata)
        if offset>0 then
            cpos = find(';',data[offset..length(data)])
            if cpos=0 then cpos = length(data)-offset+2 end if
            cookie[3] = data[offset+5..offset+cpos-2]
        end if
        cpos = find(';',data)
        if cpos=0 then cpos = length(data)+1 end if
        cookie[1] = trim(data[1..cpos-1])
        if cpos>length(data) then
            data = ""
            updata = ""
        else
            data = data[cpos+1..length(data)]
            updata = updata[cpos+1..length(data)]
        end if
        offset = match("DOMAIN=",updata)
        if offset>0 then
            cpos = find(';',data[offset..length(data)])
            if cpos=0 then cpos = length(data)-offset+2 end if
            cookie[2] = data[offset+7..offset+cpos-2]
         -- Offset is base 1.  If the semicolon is in the first position, cpos
         -- is also 1.  Since we don't want to include the semicolon, we need
         -- to subtract 1 for offset's base and 1 to go to the char before
         -- cpos, thus the subtracting of two.  In the case of end of string
         -- (cpos = 0), we need to add those two back to compensate for the
         -- different scenario (+offset-offset = 0 and +2-2 = 0, therefore
         -- cpos = length(data), which is what we want).
        end if
        offset = match("EXPIRES=",updata)
        if offset>0 then
            cpos = find(';',data[offset..length(data)])
            if cpos=0 then cpos = length(data)-offset+2 end if
            cookie[4] = data[offset+8..offset+cpos-2]
        end if
        offset = match("VERSION=",updata)
        if offset>0 then
            cpos = find(';',data[offset..length(data)])
            if cpos=0 then cpos = length(data)-offset+2 end if
            cookie[6] = data[offset+8..offset+cpos-2]
        end if
        offset = match("MAX-AGE=",updata)
        if offset>0 then
            cpos = find(';',data[offset..length(data)])
            if cpos=0 then cpos = length(data)-offset+2 end if
            cookie[4] = data[offset+8..offset+cpos-2]
        end if
        offset = match("SECURE",updata)
        if offset>0 then
            cookie[5] = "Y"
        end if
        cpos = find('=',cookie[1])
        if cpos>0 then
            request = cookie[1][1..cpos]
        else
            request = "="
        end if
        cpos = 0
        for ctr=1 to length(this_cookiejar) do
            if sequence(this_cookiejar[ctr]) and length(this_cookiejar[ctr])>=2 and
            match(cookie[1],this_cookiejar[ctr][1])>0 and
            compare(cookie[2],this_cookiejar[ctr][2])=0 and
            compare(this_cookiejar[ctr][3],cookie[3])=0 then
                this_cookiejar[ctr] = cookie
                cpos = ctr
                exit
            end if
        end for
        if cpos=0 then
            this_cookiejar = append(this_cookiejar,cookie)
        end if
        cpos = match("SET-COOKIE",upper(header2))
    end while

    return {header,body}

end function

-------------------------------------------------------------------------------
-- get_url
-------------------------------------------------------------------------------
-- Returns a binary sequence of bytes

global function eunet_get_url(sequence url)

sequence node, hostname, protocol, port, file, inet_addr
sequence data
atom cpos
object addrinfo

  -- TO DO: If the changes in version 1.2.2 prove stable, remove redundant
  -- code under the search for '?'.
    cpos = match("://",url)
    if cpos>0 then
        protocol = url[1..cpos-1]
        url = url[cpos+3..length(url)]
    else
        protocol = "http"  -- assumed default
    end if
    cpos = find(':',url)
    if cpos=0 then
        cpos = find('/',url)
        if cpos=0 then
            cpos = find('?',url)
            if cpos=0 then
                hostname = url
                node = url
                port = ""
                file = ""
                url = ""
            else
                node = url[1..cpos-1]
                hostname = url
                port = ""
                file = ""
                url = ""
            end if
        else
            node = url[1..cpos-1]
            url = url[cpos..length(url)]
            cpos = find('?',url)
            if cpos=0 then
                file = url
                port = ""
                hostname = node
                url = ""
            else
                -- hostname = node&url
                -- file = ""
                hostname = node
                file = url
                port = ""
                url = ""
            end if
        end if
    else
        node = url[1..cpos-1]
        url = url[cpos+1..length(url)]
        cpos = find('/',url)
        if cpos=0 then
            cpos = find('?',url)
            if cpos=0 then
                port = url
                hostname = node
                file = ""
                url = ""
            else
                port = url[1..cpos-1]
                hostname = node & url[cpos..length(url)]
                file = ""
                url = ""
            end if
        else
            port = url[1..cpos-1]
            url = url[cpos..length(url)]
            cpos = find('?',url)
            if cpos=0 then
                hostname = node
                file = url
                url = ""
            else
                --hostname = node & url
                -- file = ""
                hostname = node
                file = url
                url = ""
            end if
        end if
    end if

    if length(file)>0 and file[1]='/' then file = file[2..length(file)] end if
    addrinfo = eunet_getaddrinfo(node,protocol,0)
    if atom(addrinfo) or length(addrinfo)<1 or length(addrinfo[1])<5 then
        -- attempt to use deprecated methods
        return {} -- failed
    else
        inet_addr = addrinfo[1][5]
    end if
    data = {}
--PL...
    if compare(lower(protocol),"http")=0
    or compare(lower(protocol),"https")=0 then
        data = eunet_get_http(inet_addr,hostname,file)
    end if

    return data

end function

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Test output
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

procedure test()

sequence iface_list, details, str
    iface_list = eunet_get_iface_list()
    puts(1,"Networking interfaces on this computer:\n")
    for ctr=1 to length(iface_list) do
        puts(1,iface_list[ctr]&'\n')
        details = eunet_get_iface_details(iface_list[ctr])
        if length(details)>=14 then
            str = details[4]&", "&details[11]&", "&details[12]&", "&details[13]&", "&details[14]&"\n"
            puts(1,str)
        end if
    end for

    str = eunet_get_url("https://www.yahoo.com")
--PL
    if length(str)=0 then
        ?"failed"
    else
        puts(1,str[2])
    end if
    if wait_key() then end if

end procedure
if 0 then
    test()
end if
