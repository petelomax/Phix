--/**/ NOT Phix! (it is 100% machine_func)
namespace sockets

--****
-- == Core Sockets
--
-- <<LEVELTOC depth=2>>
--

include std/get.e
include std/regex.e as re
include std/machine.e as mem
include std/sequence.e as seq
include std/net/common.e

enum M_SOCK_GETSERVBYNAME = 77, M_SOCK_GETSERVBYPORT, M_SOCK_SOCKET = 81, M_SOCK_CLOSE, M_SOCK_SHUTDOWN,
    M_SOCK_CONNECT, M_SOCK_SEND, M_SOCK_RECV, M_SOCK_BIND, M_SOCK_LISTEN,
    M_SOCK_ACCEPT, M_SOCK_SETSOCKOPT, M_SOCK_GETSOCKOPT, M_SOCK_SELECT, M_SOCK_SENDTO,
    M_SOCK_RECVFROM, M_SOCK_ERROR_CODE = 96

--****
-- === Error Information
--

--**
-- Get the error code.
--
-- Returns:
--   Integer [[:OK]] on no error, otherwise any one of the ##ERR_## constants to
--   follow.
--

public function error_code()
    return machine_func(M_SOCK_ERROR_CODE, {})
end function


--**
-- No error occurred.

public constant OK                      = 0

--**
-- Permission has been denied. This can happen when using a send_to call on a broadcast
-- address without setting the socket option SO_BROADCAST. Another, possibly more common,
-- reason is you have tried to bind an address that is already exclusively bound by
-- another application.
--
-- May occur on a Unix Domain Socket when the socket directory or file could not be 
-- accessed due to security.

public constant ERR_ACCESS              = -1

--**
-- Address is already in use.

public constant ERR_ADDRINUSE           = -2

--**
-- The specified address is not a valid local IP address on this computer.

public constant ERR_ADDRNOTAVAIL        = -3

--**
-- Address family not supported by the protocol family.

public constant ERR_AFNOSUPPORT         = -4

--**
-- Kernel resources to complete the request are temporarly unavailable.

public constant ERR_AGAIN               = -5

--**
-- Operation is already in progress.

public constant ERR_ALREADY             = -6

--**
-- Software has caused a connection to be aborted.

public constant ERR_CONNABORTED         = -7

--**
-- Connection was refused.

public constant ERR_CONNREFUSED         = -8

--**
-- An incomming connection was supplied however it was terminated by the remote peer.

public constant ERR_CONNRESET           = -9

--**
-- Destination address required.

public constant ERR_DESTADDRREQ         = -10

--**
-- Address creation has failed internally.

public constant ERR_FAULT               = -11

--**
-- No route to the host specified could be found.

public constant ERR_HOSTUNREACH         = -12

--**
-- A blocking call is inprogress.

public constant ERR_INPROGRESS          = -13

--**
-- A blocking call was cancelled or interrupted.

public constant ERR_INTR                = -14

--**
-- An invalid sequence of command calls were made, for instance trying to ##accept##
-- before an actual ##listen## was called.

public constant ERR_INVAL = -15

--**
-- An I/O error occurred while making the directory entry or allocating the
-- inode. (Unix Domain Socket).

public constant ERR_IO = -16

--**
-- Socket is already connected.

public constant ERR_ISCONN              = -17

--**
-- An empty pathname was specified. (Unix Domain Socket).

public constant ERR_ISDIR = -18

--**
-- Too many symbolic links were encountered. (Unix Domain Socket).

public constant ERR_LOOP = -19

--**
-- The queue is not empty upon routine call.

public constant ERR_MFILE               = -20

--**
-- Message is too long for buffer size. This would indicate an internal error to 
-- Euphoria as Euphoria sets a dynamic buffer size.

public constant ERR_MSGSIZE             = -21

--**
-- Component of the path name exceeded 255 characters or the entire path
-- exceeded 1023 characters. (Unix Domain Socket).

public constant ERR_NAMETOOLONG = -22

--**
-- The network subsystem is down or has failed

public constant ERR_NETDOWN             = -23

--**
-- Network has dropped it's connection on reset.

public constant ERR_NETRESET            = -24

--**
-- Network is unreachable.

public constant ERR_NETUNREACH          = -25

--**
-- Not a file. (Unix Domain Sockets).

public constant ERR_NFILE = -26

--**
-- No buffer space is available.

public constant ERR_NOBUFS              = -27

--**
-- Named socket does not exist. (Unix Domain Socket).

public constant ERR_NOENT = -28

--**
-- Socket is not connected.

public constant ERR_NOTCONN             = -29

--**
-- Component of the path prefix is not a directory. (Unix Domain Socket).

public constant ERR_NOTDIR  = -30

--** 
-- Socket system is not initialized (Windows only)

public constant ERR_NOTINITIALISED      = -31

--**
-- The descriptor is not a socket.

public constant ERR_NOTSOCK             = -32

--**
-- Operation is not supported on this type of socket.

public constant ERR_OPNOTSUPP           = -33

--**
-- Protocol not supported.

public constant ERR_PROTONOSUPPORT      = -34

--**
-- Protocol is the wrong type for the socket.

public constant ERR_PROTOTYPE           = -35

--**
-- The name would reside on a read-only file system. (Unix Domain Socket).

public constant ERR_ROFS = -36

--**
-- The socket has been shutdown. Possibly a send/receive call after a shutdown took 
-- place.

public constant ERR_SHUTDOWN            = -37

--**
-- Socket type is not supported.

public constant ERR_SOCKTNOSUPPORT      = -38

--**
-- Connection has timed out.

public constant ERR_TIMEDOUT            = -39

--**
-- The operation would block on a socket marked as non-blocking.

public constant ERR_WOULDBLOCK          = -40

--****
-- === Socket Type Constants
--
-- These values are passed as the ##family## and ##sock_type## parameters of
-- the [[:create]] function.
--

ifdef WIN32 then
public constant
        --**
        -- Address family is unspecified
        AF_UNSPEC = 0,
        --**
        -- Local communications
        AF_UNIX = -1,
        --**
        -- IPv4 Internet protocols
        AF_INET = 2,
        --**
        -- IPv6 Internet protocols
        AF_INET6 = 23,
        --**
        -- Appletalk
        AF_APPLETALK = 17,
        --**
        -- Bluetooth
        AF_BTH = 32,
        --**
        -- Provides sequenced, reliable, two-way, connection-based byte streams.
        -- An out-of-band data transmission mechanism may be supported.
        SOCK_STREAM = 1,
        --**
        -- Supports datagrams (connectionless, unreliable messages of a
        -- fixed maximum length).
        SOCK_DGRAM = 2,
        --**
        -- Provides raw network protocol access.
        SOCK_RAW = 3,
        --**
        -- Provides a reliable datagram layer that does not guarantee ordering.
        SOCK_RDM = 4,
        --**
        -- Obsolete and should not be used in new programs
        SOCK_SEQPACKET = 5

elsifdef LINUX then
public constant
        AF_UNSPEC = 0, AF_UNIX = 1, AF_INET = 2, AF_APPLETALK = 5, AF_INET6 = 10,
        SOCK_STREAM = 1, SOCK_DGRAM = 2, SOCK_RAW = 3, SOCK_RDM = 4, SOCK_SEQPACKET = 5

elsifdef SUNOS then
public constant
        AF_UNSPEC = 0, AF_UNIX = 1, AF_INET = 2, AF_APPLETALK = 16, AF_INET6 = 26,
        SOCK_STREAM = 2, SOCK_DGRAM = 1, SOCK_RAW = 4, SOCK_RDM = 5, SOCK_SEQPACKET = 6

elsifdef OSX then
public constant
        AF_UNSPEC = 0, AF_UNIX = 1, AF_INET = 2, AF_APPLETALK = 16, AF_INET6 = 30,
        SOCK_STREAM = 1, SOCK_DGRAM = 2, SOCK_RAW = 3, SOCK_RDM = 4, SOCK_SEQPACKET = 5

elsifdef FREEBSD then
public constant
        AF_UNSPEC = 0, AF_UNIX = 1, AF_INET = 2, AF_APPLETALK = 16, AF_INET6 = 28,
        SOCK_STREAM = 1, SOCK_DGRAM = 2, SOCK_RAW = 3, SOCK_RDM = 4, SOCK_SEQPACKET = 5
end ifdef

--****
-- === Select Accessor Constants
--
-- Use with the result of [[:select]].
--

public enum
    --**
    -- The socket
    SELECT_SOCKET,

    --**
    -- Boolean (1/0) value indicating the readability.
    SELECT_IS_READABLE,

    --**
    -- Boolean (1/0) value indicating the writeability.
    SELECT_IS_WRITABLE,

    --**
    -- Boolean (1/0) value indicating the error state.
    SELECT_IS_ERROR

--****
-- === Shutdown Options
--
-- Pass one of the following to the ##method## parameter of [[:shutdown]].
--

public constant
    --**
    -- Shutdown the send operations.
    SD_SEND = 0,

    --**
    -- Shutdown the receive operations.
    SD_RECEIVE = 1,

    --**
    -- Shutdown both send and receive operations.
    SD_BOTH = 2

--****
-- === Socket Options
--
-- Pass to the ##optname## parameter of the functions [[:get_option]]
-- and [[:set_option]].
--
-- These options are highly OS specific and are normally not needed for most
-- socket communication. They are provided here for your convenience. If you should
-- need to set socket options, please refer to your OS reference material.
--
-- There may be other values that your OS defines and some defined here are not
-- supported on all operating systems.

ifdef WINDOWS then
public constant
        SOL_SOCKET     = #FFFF,
        SO_DEBUG       = #0001,
        SO_ACCEPTCONN  = #0002,
        SO_REUSEADDR   = #0004,
        SO_KEEPALIVE   = #0008,
        SO_DONTROUTE   = #0010,
        SO_BROADCAST   = #0020,
        SO_USELOOPBACK = #0040,
        SO_LINGER      = #0080,
        SO_DONTLINGER  = not_bits(SO_LINGER),
        SO_OOBINLINE   = #0100,
        SO_REUSEPORT   = #9999,
        SO_SNDBUF      = #1001,
        SO_RCVBUF      = #1002,
        SO_SNDLOWAT    = #1003,
        SO_RCVLOWAT    = #1004,
        SO_SNDTIMEO    = #1005,
        SO_RCVTIMEO    = #1006,
        SO_ERROR       = #1007,
        SO_TYPE        = #1008,
        SO_CONNDATA    = #7000,
        SO_CONNOPT     = #7001,
        SO_DISCDATA    = #7002,
        SO_DISCOPT     = #7003,
        SO_CONNDATALEN = #7004,
        SO_CONNOPTLEN  = #7005,
        SO_DISCDATALEN = #7006,
        SO_DISCOPTLEN  = #7007,
        SO_OPENTYPE    = #7008,
        SO_MAXDG       = #7009,
        SO_MAXPATHDG   = #700A,
        SO_SYNCHRONOUS_ALTERT   = #10,
        SO_SYNCHRONOUS_NONALERT = #20
elsifdef LINUX then
public constant SOL_SOCKET = 1,
SO_REUSEADDR = 2,
SO_TYPE = 3,
SO_KEEPALIVE = 9
end ifdef
--****
-- === Send Flags
--
-- Pass to the ##flags## parameter of [[:send]] and [[:receive]]
--

public constant
    --**
    -- Sends out-of-band data on sockets that support this notion (e.g., of
    -- type [[:SOCK_STREAM]]); the underlying protocol must also support
    -- out-of-band data.

    MSG_OOB       = #1,

    --**
    -- This flag causes the receive operation to return data from the
    -- beginning of the receive queue without removing that data from the
    -- queue.  Thus, a subsequent receive call will return the same data.

    MSG_PEEK      = #2,

    --**
    -- Don't use a gateway to send out the packet, only send to hosts on
    -- directly connected networks.  This is usually used only by diagnostic
    -- or routing programs.  This is only defined for protocol families that
    -- route; packet sockets don't.

    MSG_DONTROUTE = #4,

    MSG_TRYHARD   = #4,

    --**
    -- indicates that some control data were discarded due to lack of space in
    -- the buffer for ancillary data.

    MSG_CTRUNC    = #8,

    MSG_PROXY     = #10,

    --**
    -- indicates that the trailing portion of a datagram was discarded because
    -- the datagram was larger than the buffer supplied.

    MSG_TRUNC     = #20,

    --**
    -- Enables non-blocking operation; if the operation would block, EAGAIN
    -- or EWOULDBLOCK is returned.

    MSG_DONTWAIT  = #40,

    --**
    -- Terminates a record (when this notion is supported, as for sockets of
    -- type [[:SOCK_SEQPACKET]]).

    MSG_EOR       = #80,

    --**
    -- This flag requests that the operation block until the full request is
    -- satisfied.  However, the call may still return less data than requested
    -- if a signal is caught, an error or disconnect occurs, or the next data
    -- to be received is of a different type than that returned.

    MSG_WAITALL   = #100,

    MSG_FIN       = #200,

    MSG_SYN       = #400,

    --**
    -- Tell the link layer that forward progress happened: you got a
    -- successful reply from the other side.  If the link layer doesn't get
    -- this it will regularly reprobe the neighbor (e.g., via a unicast ARP).
    -- Only valid on [[:SOCK_DGRAM]] and [[:SOCK_RAW]] sockets and currently only
    -- implemented for IPv4 and IPv6.

    MSG_CONFIRM   = #800,

    MSG_RST       = #1000,

    --**
    -- indicates that no data was received but an extended error from the
    -- socket error queue.

    MSG_ERRQUEUE  = #2000,

    --**
    -- Requests not to send SIGPIPE on errors on stream oriented sockets when
    -- the other end breaks the connection. The EPIPE error is still
    -- returned.

    MSG_NOSIGNAL  = #4000,

    --**
    -- The caller has more data to send. This flag is used with TCP sockets
    -- to obtain the same effect as the TCP_CORK socket option, with the
    -- difference that this flag can be set on a per-call basis.

    MSG_MORE      = #8000

--****
-- === Server and Client sides
--

-- Not made public as the end user should have no need of accessing one value
-- or the other.
export enum
    --**
    -- Accessor index for socket handle of a socket type

    SOCKET_SOCKET,

    --**
    -- Accessor index for the sockaddr_in pointer of a socket type

    SOCKET_SOCKADDR_IN

--**
-- Socket type
--

public type socket(object o)
    if not sequence(o) then return 0 end if
    if not length(o)=2 then return 0 end if
    if not atom(o[1]) then return 0 end if
    if not atom(o[2]) then return 0 end if

    return 1
end type

procedure delete_socket(object o)
    if not socket(o) then
        return
    end if
    if o[SOCKET_SOCKADDR_IN]=0 then
        return
    end if

    free(o[SOCKET_SOCKADDR_IN])
end procedure
integer delete_socket_rid = routine_id("delete_socket")

--**
-- Create a new socket
--
-- Parameters:
--   # ##family##: an integer
--   # ##sock_type##: an integer, the type of socket to create
--   # ##protocol##: an integer, the communication protocol being used
--
-- ##family## options:
--   * [[:AF_UNIX]]
--   * [[:AF_INET]]
--   * [[:AF_INET6]]
--   * [[:AF_APPLETALK]]
--   * [[:AF_BTH]]
--
-- ##sock_type## options:
--   * [[:SOCK_STREAM]]
--   * [[:SOCK_DGRAM]]
--   * [[:SOCK_RAW]]
--   * [[:SOCK_RDM]]
--   * [[:SOCK_SEQPACKET]]
--
-- Returns:
--   An **object**, -1 on failure, else a supposedly valid socket id.
--
-- Example 1:
-- <eucode>
-- socket = create(AF_INET, SOCK_STREAM, 0)
-- </eucode>

public function create(integer family, integer sock_type, integer protocol)
object o = machine_func(M_SOCK_SOCKET, {family, sock_type, protocol})
    if atom(o) then return o end if
    return delete_routine(o, delete_socket_rid)
end function

--**
-- Closes a socket.
--
-- Parameters:
--   # ##sock##: the socket to close
--
-- Returns:
--   An **integer**, 0 on success and -1 on error. 
--
-- Comments:
--   It may take several minutes for the OS to declare the socket as closed.
-- 

public function sock_close(socket sock)
    return machine_func(M_SOCK_CLOSE, {sock})
end function

--**
-- Partially or fully close a socket. 
--
-- Parameters:
--   # ##sock## : the socket to shutdown
--   # ##method## : the method used to close the socket
--
-- Returns:
--   An **integer**, 0 on success and -1 on error.
--
-- Comments:
--   Three constants are defined that can be sent to ##method##:
--   * [[:SD_SEND]] - shutdown the send operations.
--   * [[:SD_RECEIVE]] - shutdown the receive operations.
--   * [[:SD_BOTH]] - shutdown both send and receive operations.
--
--  It may take several minutes for the OS to declare the socket as closed.
--

public function shutdown(socket sock, atom method=SD_BOTH)
    return machine_func(M_SOCK_SHUTDOWN, {sock, method})
end function

--**
-- Determine the read, write and error status of one or more sockets.
--
-- Using select, you can check to see if a socket has data waiting and
-- is read to be read, if a socket can be written to and if a socket has
-- an error status.
--
-- select allows for fine-grained control over your sockets, allow you
-- to specify that a given socket only be checked for reading or for only
-- reading and writing, etc.
--
-- Parameters:
--   # ##sockets_read## : either one socket or a sequence of sockets to check for reading.
--   # ##sockets_write## : either one socket or a sequence of sockets to check for writing.
--   # ##sockets_err## : either one socket or a sequence of sockets to check for errors.
--   # ##timeout## : maximum time to wait to determine a sockets status, seconds part
--   # ##timeout_micro## : maximum time to wait to determine a sockets status, microsecond part
--
-- Returns:
--   A **sequence**, of the same size of all unique sockets containing
--   { socket, read_status, write_status, error_status } for each socket passed
--  2 to the function.
--   Note that the sockets returned are not guaranteed to be in any particular order.
--

public function select(object sockets_read, object sockets_write,
        object sockets_err, integer timeout=0,
        integer timeout_micro=0)
sequence sockets_all

    if length(sockets_read) and socket(sockets_read) then
        sockets_read = {sockets_read}
    end if
    if length(sockets_write) and socket(sockets_write) then
        sockets_write = {sockets_write}
    end if
    if length(sockets_err) and socket(sockets_err) then
        sockets_err = {sockets_err}
    end if

    -- if the user has passed in one socket (or set of sockets) for
    -- all three lists, then save time by not bothering to sort the list
    -- or remove duplicates
    if equal(sockets_err, sockets_read) and
    equal(sockets_write, sockets_read) then
        sockets_all = sockets_read
    else
        sockets_all = remove_dups(sockets_read & sockets_write
                                  & sockets_err, RD_SORT)
    end if

    return machine_func(M_SOCK_SELECT, {sockets_read, sockets_write,
                                        sockets_err, sockets_all, timeout_micro, timeout})
end function

--**
-- Send TCP data to a socket connected remotely.
--
-- Parameters:
--   # ##sock## : the socket to send data to
--   # ##data## : a sequence of atoms, what to send
--   # ##flags## : flags (see [[:Send Flags]])
--
-- Returns:
--   An **integer**, the number of characters sent, or -1 for an error.
--

public function send(socket sock, sequence data, atom flags=0)
    return machine_func(M_SOCK_SEND, {sock, data, flags})
end function

--**
-- Receive data from a bound socket. 
--
-- Parameters:
--   # ##sock## : the socket to get data from
--   # ##flags## : flags (see [[:Send Flags]])
--
-- Returns:     
--   A **sequence**, either a full string of data on success, or an atom indicating
--   the error code.
--
-- Comments:
--   This function will not return until data is actually received on the socket,
--   unless the flags parameter contains [[:MSG_DONTWAIT]].
--
--   [[:MSG_DONTWAIT]] only works on Linux kernels 2.4 and above. To be cross-platform
--   you should use [[:select]] to determine if a socket is readable, i.e. has data
--   waiting.
--

public function receive(socket sock, atom flags=0)
    return machine_func(M_SOCK_RECV, {sock, flags})
end function

--**
-- Get options for a socket.
-- 
-- Parameters:
--   # ##sock## : the socket
--   # ##level## : an integer, the option level
--   # ##optname## : requested option (See [[:Socket Options]])
--
-- Returns:
--   An **object**, either:
-- * On error, {"ERROR",error_code}.  
-- * On success, either an atom or a sequence containing the option value, depending on the option.
--
-- Comments:
--   Primarily for use in multicast or more advanced socket
--   applications.  Level is the option level, and option_name is the
--   option for which values are being sought. Level is usually
--   [[:SOL_SOCKET]].
--
-- Returns:
--   An **atom**, On error, an atom indicating the error code.\\
--  A **sequence** or **atom**,   On success, either an atom or
--   a sequence containing the option value.
--
-- See also:
--   [[:get_option]]
--

public function get_option(socket sock, integer level, integer optname)
    return machine_func(M_SOCK_GETSOCKOPT, {sock, level, optname})
end function

--**
-- Set options for a socket.
-- 
-- Parameters:
--   # ##sock## : an atom, the socket id
--   # ##level## : an integer, the option level
--   # ##optname## : requested option (See [[:Socket Options]])
--   # ##val## : an object, the new value for the option
--
-- Returns:
--   An **integer**, 0 on success, -1 on error.
--
-- Comments:
--   Primarily for use in multicast or more advanced socket
--   applications.  Level is the option level, and option_name is the
--   option for which values are being set.  Level is usually
--   [[:SOL_SOCKET]].
--
-- See Also:
--   [[:get_option]]

public function set_option(socket sock, integer level, integer optname, object val)
    return machine_func(M_SOCK_SETSOCKOPT, {sock, level, optname, val})
end function

--****
-- === Client side only

--**
-- Establish an outgoing connection to a remote computer. Only works with TCP sockets.
--
-- Parameters:
--   # ##sock## : the socket
--   # ##address## : ip address to connect, optionally with :PORT at the end
--   # ##port## : port number
--
-- Returns: 
--   An **integer**, 0 for success and -1 on failure.
--
-- Comments:
--   ##address## can contain a port number. If it does not, it has to be supplied
--   to the ##port## parameter.
--
-- Example 1:
-- <eucode>
-- success = connect(sock, "11.1.1.1") -- uses default port 80
-- success = connect(sock, "11.1.1.1:110") -- uses port 110
-- success = connect(sock, "11.1.1.1", 345) -- uses port 345
-- </eucode>

public function connect(socket sock, sequence address, integer port=-1)
object sock_data = parse_ip_address(address, port)

    return machine_func(M_SOCK_CONNECT, {sock, sock_data[1], sock_data[2]})
end function

--****
-- === Server side only

--**
-- Joins a socket to a specific local internet address and port so
-- later calls only need to provide the socket. 
--
-- Parameters:
--   # ##sock## : the socket
--   # ##address## : the address to bind the socket to
--   # ##port## : optional, if not specified you must include :PORT in
--     the address parameter.
--
-- Returns: 
--   An **integer**, 0 on success and -1 on failure.
--
-- Example 1:
-- <eucode>
-- -- Bind to all interfaces on the default port 80.
-- success = bind(socket, "0.0.0.0")
-- -- Bind to all interfaces on port 8080.
-- success = bind(socket, "0.0.0.0:8080")
-- -- Bind only to the 243.17.33.19 interface on port 345.
-- success = bind(socket, "243.17.33.19", 345)
-- </eucode>

public function bind(socket sock, sequence address, integer port=-1)
object sock_data = parse_ip_address(address, port)

    return machine_func(M_SOCK_BIND, {sock, sock_data[1], sock_data[2]})
end function

--**
-- Start monitoring a connection. Only works with TCP sockets.
--
-- Parameters:
--   # ##sock## : the socket
--   # ##backlog## : the number of connection requests that
--     can be kept waiting before the OS refuses to hear any more.
--
-- Returns:
--   An **integer**, 0 on success and an error code on failure.
--
-- Comments:
--   Once the socket is created and bound, this will indicate to the
--   operating system that you are ready to being listening for connections.
--
--   The value of ##backlog## is strongly dependent on both the hardware
--   and the amount of time it takes the program to process each
--   connection request.
--
--   This function must be executed after [[:bind]]().
--

public function listen(socket sock, integer backlog)
    return machine_func(M_SOCK_LISTEN, {sock, backlog})
end function

--**
-- Produces a new socket for an incoming connection.
--
-- Parameters:
--   # ##sock##: the server socket
--
-- Returns:
--   An **atom**, on error\\
--  A **sequence**,
--   ##{socket client, sequence client_ip_address}##
--   on success.
--
-- Comments:
--   Using this function allows communication to occur on a
--   "side channel" while the main server socket remains available
--   for new connections.
--
--   ##accept##() must be called after ##bind##() and ##listen##().
--

public function accept(socket sock)
    return machine_func(M_SOCK_ACCEPT, {sock})
end function

--****
-- === UDP only
-- 

--**
-- Send a UDP packet to a given socket
-- 
-- Parameters:
--   # ##sock##: the server socket
--   # ##data##: the data to be sent
--   # ##ip##: the ip where the data is to be sent to (ip:port) 
--     is acceptable
--   # ##port##: the port where the data is to be sent on (if not supplied with the ip)
--   # ##flags## : flags (see [[:Send Flags]])
--
-- Returns:
--   An ##integer## status code.
--   
-- See Also:
--   [[:receive_from]]
--   

public function send_to(socket sock, sequence data, sequence address, integer port=-1,
        atom flags=0)
object sock_data = parse_ip_address(address, port)

    return machine_func(M_SOCK_SENDTO, {sock, data, flags, sock_data[1], sock_data[2]})
end function

--**
-- Receive a UDP packet from a given socket
--
-- Parameters:
--   # ##sock##: the server socket
--   # ##flags## : flags (see [[:Send Flags]])
--
-- Returns:
--   A ##sequence## containing { client_ip, client_port, data } or an ##atom## error code.
--   
-- See Also:
--   [[:send_to]]
--   

public function receive_from(socket sock, atom flags=0)
    return machine_func(M_SOCK_RECVFROM, {sock, flags})
end function

--****
-- === Information

--**
-- Get service information by name.
--
-- Parameters:
--   # ##name## : service name.
--   # ##protocol## : protocol. Default is not to search by protocol.
--
-- Returns:
--   A **sequence**, containing { official protocol name, protocol, port number } or
--   an atom indicating the error code.
--
-- Example 1:
-- <eucode>
-- object result = getservbyname("http")
-- -- result = { "http", "tcp", 80 }
-- </eucode>
--
-- See Also:
--   [[:service_by_port]]

public function service_by_name(sequence name, object protocol=0)
    return machine_func(M_SOCK_GETSERVBYNAME, {name, protocol})
end function

--**
-- Get service information by port number.
--
-- Parameters:
--   # ##port## : port number.
--   # ##protocol## : protocol. Default is not to search by protocol.
--
-- Returns:
--   A **sequence**, containing { official protocol name, protocol, port number } or
--   an atom indicating the error code.
--
-- Example 1:
-- <eucode>
-- object result = getservbyport(80)
-- -- result = { "http", "tcp", 80 }
-- </eucode>
--
-- See Also:
--   [[:service_by_name]]

public function service_by_port(integer port, object protocol=0)
    return machine_func(M_SOCK_GETSERVBYPORT, {port, protocol})
end function
