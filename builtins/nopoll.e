--
-- nopoll.e
--
--  See http://www.aspl.es/nopoll
--
--  Windows only, for now, but should be fine on linux.

procedure Abort(string msg)
    ?msg
    {} = wait_key()
    abort(0)
end procedure

atom nopoll = NULL
global string nopoll_dll_name

procedure nopoll_init()
    if nopoll=NULL then
        if platform()=WINDOWS then
--          nopoll_dll_name = "libnopoll.dll"
            nopoll_dll_name = "libnopoll-debug.dll"
        else
            ?9/0 -- fixme!
        end if
        nopoll = open_dll(nopoll_dll_name)
        if nopoll=NULL then
--          if fatal then
                Abort("cannot open "&nopoll_dll_name)
--          end if
        end if
    end if
end procedure

function link_c_func(atom dll, sequence name, sequence args, atom result)
    if dll=NULL then ?9/0 end if
-- erm, if you don't need this on Windows, you don't need it full stop:
--  integer rid = define_c_func(dll, "+" & name, args, result)
    integer rid = define_c_func(dll, name, args, result)
    if rid<1 then
        Abort("cannot link "&name)
    end if
    return rid
end function

function link_c_proc(atom dll, sequence name, sequence args)
    if dll=NULL then ?9/0 end if
--  integer rid = define_c_proc(dll, "+" & name, args)
    integer rid = define_c_proc(dll, name, args)
    if rid<1 then
        Abort("cannot line "&name)
    end if
    return rid
end function

atom xnopoll_ctx_new = NULL
global function nopoll_ctx_new()
    if xnopoll_ctx_new=NULL then
        if nopoll=NULL then nopoll_init() end if
        xnopoll_ctx_new = link_c_func(nopoll, "nopoll_ctx_new", {}, C_PTR)
    end if
    atom res = c_func(xnopoll_ctx_new, {})
    if res=NULL then ?9/0 end if
    return res
end function

atom xnopoll_conn_connect_timeout = NULL
global procedure nopoll_conn_connect_timeout(atom ctx, integer microseconds_to_wait)
    if xnopoll_conn_connect_timeout=NULL then
        xnopoll_conn_connect_timeout = link_c_proc(nopoll, "nopoll_conn_connect_timeout", {C_PTR,C_INT})
    end if
    c_proc(xnopoll_conn_connect_timeout,{ctx,microseconds_to_wait})
end procedure

atom xnopoll_conn_get_connect_timeout = NULL
global function nopoll_conn_get_connect_timeout(atom ctx)
    if xnopoll_conn_get_connect_timeout=NULL then
        xnopoll_conn_get_connect_timeout = link_c_func(nopoll, "nopoll_conn_get_connect_timeout", {C_PTR}, C_INT)
    end if
    integer timeout = c_func(xnopoll_conn_get_connect_timeout,{ctx})
    return timeout
end function

atom xnopoll_ctx_unref = NULL
global procedure nopoll_ctx_unref(atom ctx)
    if xnopoll_ctx_unref=NULL then
        xnopoll_ctx_unref = link_c_proc(nopoll, "nopoll_ctx_unref", {C_PTR})
    end if
    c_proc(xnopoll_ctx_unref,{ctx})
end procedure

--atom * nopoll_conn_new (atom  * ctx,
--                const char * host_ip, 
--                const char * host_port, 
--                const char * host_name,
--                const char * get_url, 
--                const char * protocols,
--                const char * origin);

type nullable_string(object s)  -- (compatible with the one in pGUI)
    return string(s) or s=NULL
end type

atom xnopoll_conn_new = NULL
global function nopoll_conn_new(atom ctx, nullable_string host_ip, host_port, host_name, get_url, protocols, origin)
    if xnopoll_conn_new=NULL then
        xnopoll_conn_new = link_c_func(nopoll, "nopoll_conn_new", {C_PTR,C_PTR,C_PTR,C_PTR,C_PTR,C_PTR,C_PTR}, C_PTR)
    end if
    atom res = c_func(xnopoll_conn_new, {ctx, host_ip, host_port, host_name, get_url, protocols, origin})
    if res=NULL then ?9/0 end if
    return res
end function

atom xnopoll_conn_is_ok = NULL
global function nopoll_conn_is_ok(atom conn)
    if xnopoll_conn_is_ok=NULL then
        xnopoll_conn_is_ok = link_c_func(nopoll, "nopoll_conn_is_ok", {C_PTR}, C_INT)
    end if
    bool res = c_func(xnopoll_conn_is_ok, {conn})
    return res
end function

atom xnopoll_conn_is_ready = NULL
global function nopoll_conn_is_ready(atom conn)
    if xnopoll_conn_is_ready=NULL then
        xnopoll_conn_is_ready = link_c_func(nopoll, "nopoll_conn_is_ready", {C_PTR}, C_INT)
    end if
    bool res = c_func(xnopoll_conn_is_ready,{conn})
    return res
end function

global function nopoll_conn_wait_until_connection_ready(atom conn, integer timeout)
--
-- Aside: the C function didn't work as expected, so this is a fairly faithful
--  rendition of that logic in Phix, except it sleeps for 1ms rather than 0.5, 
--  which kernel32/Sleep treated as 0 and then this loop, or the C version of 
--  it at least, got through 10,000 iterations way faster than a whole 5s.
--
    atom total_timeout = timeout*1000
    while not nopoll_conn_is_ready(conn) and total_timeout>0 do
        if not nopoll_conn_is_ok(conn) then return false end if
        sleep(0.001)        -- (1ms)
        total_timeout -= 1
    end while
    return nopoll_conn_is_ok(conn) and nopoll_conn_is_ready(conn)
end function

atom xnopoll_conn_get_accepted_protocol = NULL
global function nopoll_conn_get_accepted_protocol(atom conn)
    if xnopoll_conn_get_accepted_protocol=NULL then
        xnopoll_conn_get_accepted_protocol = link_c_func(nopoll, "nopoll_conn_get_accepted_protocol", {C_PTR}, C_PTR)
    end if
    atom pProtocol = c_func(xnopoll_conn_get_accepted_protocol,{conn})
    if pProtocol=NULL then return "" end if
    return peek_string(pProtocol)
end function

atom xnopoll_conn_read = NULL
global function nopoll_conn_read(atom conn, integer bytes, bool block, integer timeout)
    if xnopoll_conn_read=NULL then
        xnopoll_conn_read = link_c_func(nopoll, "nopoll_conn_read", {C_PTR,C_PTR,C_INT,C_INT,C_INT}, C_INT)
    end if
    atom buffer = allocate(bytes)
    integer n = c_func(xnopoll_conn_read,{conn,buffer,bytes,block,timeout})
    string s = iff(n>0?peek({buffer,n}):"")
    free(buffer)
    return {n,s}
end function

atom xnopoll_conn_read_pending = NULL
global function nopoll_conn_read_pending(atom conn)
    if xnopoll_conn_read_pending=NULL then
        xnopoll_conn_read_pending = link_c_func(nopoll, "nopoll_conn_read_pending", {C_PTR}, C_INT)
    end if
    integer pending_bytes = c_func(xnopoll_conn_read_pending,{conn})
    return pending_bytes
end function

--void nopoll_thread_handlers(noPollMutexCreate mutex_create,
--                          noPollMutexDestroy mutex_destroy,
--                          noPollMutexLock mutex_lock,
--                          noPollMutexUnlock mutex_unlock)

--typedef noPollPtr (*noPollMutexCreate) (void);
function mutex_create()
    return init_cs()
end function
constant mutex_create_cb = call_back(routine_id("mutex_create"))

--typedef void (*noPollMutexDestroy) (noPollPtr mutex);
function mutex_destroy(integer cs)
    delete_cs(cs)
    return 0 -- (rqd for call_back, otherwise ignored)
end function
constant mutex_destroy_cb = call_back(routine_id("mutex_destroy"))

--typedef void (*noPollMutexLock) (noPollPtr mutex);    
function mutex_lock(integer cs)
    enter_cs(cs)
    return 0 -- (rqd for call_back, otherwise ignored)
end function
constant mutex_lock_cb = call_back(routine_id("mutex_lock"))

--typedef void (*noPollMutexUnlock) (noPollPtr mutex);
function mutex_unlock(integer cs)
    leave_cs(cs)
    return 0 -- (rqd for call_back, otherwise ignored)
end function
constant mutex_unlock_cb = call_back(routine_id("mutex_unlock"))

atom xnopoll_thread_handlers = NULL
global procedure nopoll_thread_handlers()
    if xnopoll_thread_handlers=NULL then
        xnopoll_thread_handlers = link_c_proc(nopoll, "nopoll_thread_handlers", {C_PTR, C_PTR, C_PTR, C_PTR})
    end if
    c_proc(xnopoll_thread_handlers, {mutex_create_cb, mutex_destroy_cb, mutex_lock_cb, mutex_unlock_cb})
end procedure

atom xnopoll_loop_wait = NULL
global function nopoll_loop_wait(atom ctx, integer timeout)
    if xnopoll_loop_wait=NULL then
        xnopoll_loop_wait = link_c_func(nopoll, "nopoll_loop_wait", {C_PTR, C_INT}, C_INT)
    end if
    integer res = c_func(xnopoll_loop_wait, {ctx, timeout})
    return res
end function

atom xnopoll_loop_stop = NULL
global procedure nopoll_loop_stop(atom ctx)
    if xnopoll_loop_stop=NULL then
        xnopoll_loop_stop = link_c_proc(nopoll, "nopoll_loop_stop", {C_PTR})
    end if
    c_proc(xnopoll_loop_stop, {ctx})
end procedure

atom xnopoll_ctx_set_on_msg = NULL
global procedure nopoll_ctx_set_on_msg(atom ctx, on_msg_cb, pUserData=NULL)
    if xnopoll_ctx_set_on_msg=NULL then
        xnopoll_ctx_set_on_msg = link_c_proc(nopoll, "nopoll_ctx_set_on_msg", {C_PTR, C_PTR, C_PTR})
    end if
    c_proc(xnopoll_ctx_set_on_msg, {ctx, on_msg_cb, pUserData})
end procedure

atom xnopoll_conn_set_on_msg = NULL
global procedure nopoll_conn_set_on_msg(atom conn, on_msg_cb, pUserData=NULL)
    if xnopoll_conn_set_on_msg=NULL then
        xnopoll_conn_set_on_msg = link_c_proc(nopoll, "nopoll_conn_set_on_msg", {C_PTR, C_PTR, C_PTR})
    end if
    c_proc(xnopoll_conn_set_on_msg, {conn, on_msg_cb, pUserData})
end procedure

--typedef void (*noPollOnMessageHandler) (atom  * ctx,
--                  atom * conn,
--                  noPollMsg  * msg,
--                  noPollPtr    user_data);
--function msg_handler(atom ctx, conn, pMsg, pUserData)
--  ?{"msg_handler",ctx,conn,pMsg,pUserData}
--  return 0 -- (rqd for call_back, otherwise ignored)
--end function
--constant msg_handler_cb = call_back(routine_id("msg_handler"))

--typedef void * noPollPtr;

--DEV may deserve (global) cffi:    -- no, should be considered opaque
--typedef struct _noPollMsg noPollMsg;
--struct _noPollMsg {
--  bool           has_fin;
--  short          op_code;
--  bool           is_masked;
--
--  noPollPtr      payload;
--  long int       payload_size;
--
--  int            refs;
--  noPollPtr      ref_mutex;
--
--  char           mask[4];
--  int            remain_bytes;
--
--  bool           is_fragment;
--  int            unmask_desp;
--};

atom xnopoll_conn_get_msg = NULL
global function nopoll_conn_get_msg(atom conn)
    if xnopoll_conn_get_msg=NULL then
        xnopoll_conn_get_msg = link_c_func(nopoll, "nopoll_conn_get_msg", {C_PTR}, C_PTR)
    end if
    atom pMsg = c_func(xnopoll_conn_get_msg, {conn})
    return pMsg
end function

global constant EAGAIN = 11,
                EWOULDBLOCK = EAGAIN,
                NOPOLL_EWOULDBLOCK = EWOULDBLOCK

atom xnopoll_conn_send_text = NULL
global function nopoll_conn_send_text(atom conn, object content, integer len=length(content))
    if xnopoll_conn_send_text=NULL then
        xnopoll_conn_send_text = link_c_func(nopoll, "nopoll_conn_send_text", {C_PTR,C_PTR,C_INT}, C_INT)
    end if
    integer bytes_written = c_func(xnopoll_conn_send_text, {conn, content, len})
    return bytes_written
end function

atom xnopoll_conn_pending_write_bytes = NULL
global function nopoll_conn_pending_write_bytes(atom conn)
    if xnopoll_conn_pending_write_bytes=NULL then
        xnopoll_conn_pending_write_bytes = link_c_func(nopoll, "nopoll_conn_pending_write_bytes", {C_PTR}, C_INT)
    end if
    integer bytes_pending = c_func(xnopoll_conn_pending_write_bytes, {conn})
    return bytes_pending
end function

atom xnopoll_sleep = NULL
global procedure nopoll_sleep(integer microseconds)
-- note microseconds should be >=1000, on Windows anyway.
    if xnopoll_sleep=NULL then
        xnopoll_sleep = link_c_proc(nopoll, "nopoll_sleep", {C_LONG})
    end if
    c_proc(xnopoll_sleep, {microseconds})
end procedure

atom xnopoll_conn_complete_pending_write = NULL
global function nopoll_conn_complete_pending_write(atom conn)
    if xnopoll_conn_complete_pending_write=NULL then
        xnopoll_conn_complete_pending_write = link_c_func(nopoll, "nopoll_conn_complete_pending_write", {C_PTR}, C_INT)
    end if
    integer bytes_written = c_func(xnopoll_conn_complete_pending_write, {conn})
    return bytes_written
end function

--int nopoll_conn_flush_writes(atom * conn, long timeout, int previous_result);
atom xnopoll_conn_flush_writes = NULL
global function nopoll_conn_flush_writes(atom conn, integer timeout, integer previous_result)
    if xnopoll_conn_flush_writes=NULL then
        xnopoll_conn_flush_writes = link_c_func(nopoll, "nopoll_conn_flush_writes", {C_PTR,C_INT,C_INT}, C_INT)
    end if
    integer bytes_written = c_func(xnopoll_conn_flush_writes, {conn, timeout, previous_result})
    return bytes_written
end function

--void nopoll_conn_close(atom * conn);
atom xnopoll_conn_close = NULL
global procedure nopoll_conn_close(atom conn)
    if xnopoll_conn_close=NULL then
        xnopoll_conn_close = link_c_proc(nopoll, "nopoll_conn_close", {C_PTR})
    end if
    c_proc(xnopoll_conn_close,{conn})
end procedure

atom xnopoll_conn_close_ext = NULL
global procedure nopoll_conn_close_ext(atom conn, integer status, string reason)
    if xnopoll_conn_close_ext=NULL then
        xnopoll_conn_close_ext = link_c_proc(nopoll, "nopoll_conn_close_ext", {C_PTR,C_INT,C_PTR,C_INT})
    end if
    c_proc(xnopoll_conn_close_ext,{conn,status,reason,length(reason)})
end procedure


--???
-- nopoll_conn_accept 
--atom   * nopoll_conn_accept (atom * ctx, atom * listener);

global enum NOPOLL_UNKNOWN_OP_CODE = -1,
            NOPOLL_CONTINUATION_FRAME = 0,
            NOPOLL_TEXT_FRAME         = 1,
            NOPOLL_BINARY_FRAME       = 2,
            NOPOLL_CLOSE_FRAME        = 8,
            NOPOLL_PING_FRAME         = 9,
            NOPOLL_PONG_FRAME         = 10
-- noPollOpCode;

--noPollOpCode nopoll_msg_opcode (noPollMsg * msg);
atom xnopoll_msg_opcode = NULL
global function nopoll_msg_opcode(atom pMsg)
    if xnopoll_msg_opcode=NULL then
        xnopoll_msg_opcode = link_c_func(nopoll, "nopoll_msg_opcode", {C_PTR}, C_INT)
    end if
    integer opcode = c_func(xnopoll_msg_opcode, {pMsg})
    return opcode
end function

global function nopoll_msg_opcode_desc(integer opcode)
    switch opcode do
        case NOPOLL_UNKNOWN_OP_CODE:    return "NOPOLL_UNKNOWN_OP_CODE"
        case NOPOLL_CONTINUATION_FRAME: return "NOPOLL_CONTINUATION_FRAME"
        case NOPOLL_TEXT_FRAME:         return "NOPOLL_TEXT_FRAME"
        case NOPOLL_BINARY_FRAME:       return "NOPOLL_BINARY_FRAME"
        case NOPOLL_CLOSE_FRAME:        return "NOPOLL_CLOSE_FRAME"
        case NOPOLL_PING_FRAME:         return "NOPOLL_PING_FRAME"
        case NOPOLL_PONG_FRAME:         return "NOPOLL_PONG_FRAME"
    end switch
    ?9/0
end function

--const unsigned char* nopoll_msg_get_payload(noPollMsg *msg)
atom xnopoll_msg_get_payload = NULL
global function nopoll_msg_get_payload(atom pMsg)
    if xnopoll_msg_get_payload=NULL then
        xnopoll_msg_get_payload = link_c_func(nopoll, "nopoll_msg_get_payload", {C_PTR}, C_INT)
    end if
    atom pPayload = c_func(xnopoll_msg_get_payload, {pMsg})
    return pPayload
end function

--int nopoll_msg_get_payload_size(noPollMsg *msg)
atom xnopoll_msg_get_payload_size = NULL
global function nopoll_msg_get_payload_size(atom pMsg)
    if xnopoll_msg_get_payload_size=NULL then
        xnopoll_msg_get_payload_size = link_c_func(nopoll, "nopoll_msg_get_payload_size", {C_PTR}, C_INT)
    end if
    integer payload_length = c_func(xnopoll_msg_get_payload_size, {pMsg})
    return payload_length
end function

--listenernoPollConn        * nopoll_listener_new (noPollCtx  * ctx,
--                     const char * host,
--                     const char * port)
atom xnopoll_listener_new = NULL
global function nopoll_listener_new(atom ctx, string host, string port)
    if xnopoll_listener_new=NULL then
        xnopoll_listener_new = link_c_func(nopoll, "nopoll_listener_new", {C_PTR,C_PTR,C_PTR}, C_PTR)
    end if
    atom listener = c_func(xnopoll_listener_new,{ctx,host,port})
    return listener
end function


--/*
import websocket
import thread
import time
import json

def on_message(ws, message):
    print(message)

def on_error(ws, error):
    print(error)

def on_close(ws):
    print("### closed ###")

def on_open(ws):
    print("ONOPEN")
    def run(*args):
        # ws.send(json.dumps({'command':'subscribe','channel':1001}))
        ws.send(json.dumps({'command':'subscribe','channel':1002}))
        # ws.send(json.dumps({'command':'subscribe','channel':1003}))
        # ws.send(json.dumps({'command':'subscribe','channel':'BTC_XMR'}))
--erm, try this:
--nopoll_conn_send_text(conn,"{\"command\":\"subscribe\",\"channel\":1002}")
--(or this)
--nopoll_conn_send_text(conn,`{"command":"subscribe","channel":1002}`)
        while True:
            time.sleep(1)
        ws.close()
        print("thread terminating...")
    thread.start_new_thread(run, ())


if __name__ == "__main__":
    websocket.enableTrace(True)
    ws = websocket.WebSocketApp("wss://api2.poloniex.com/",
                              on_message = on_message,
                              on_error = on_error,
                              on_close = on_close)
    ws.on_open = on_open
    ws.run_forever()

1001 = trollbox (you will get nothing but a heartbeat)
1002 = ticker
1003 = base coin 24h volume stats
1010 = heartbeat
'MARKET_PAIR' = market order books
--*/

--/*
--
-- libcurl.e
--
--  A libcurl wrapper, based heavily on the work of Raymond Smith and jmduro, 
--                     rewritten mainly as part of the documentation process.
--  
--  Note that the (windows) dlls are /not/ included in the standard distribution,
--  but can be downloaded from http://phix.x10.mx/pmwiki/pmwiki.php?n=Main.Libcurldlls
--

--atom libcurl = NULL
global string curl_dll_name

procedure curl_init(string dll_name="", bool fatal=true)
--  if libcurl!=NULL then ?9/0 end if -- sanity check
    if libcurl=NULL then
        if dll_name="" then
            if platform() = WINDOWS then
                curl_dll_name = sprintf("LIBCURL.%d.SK.DLL",machine_bits())
            elsif platform() = LINUX then
                string arch = iff(machine_bits()=32?"i386":"x86_64")
                curl_dll_name = "/usr/lib/"&arch&"-linux-gnu/libcurl.so"
            end if
        else
            curl_dll_name = dll_name
        end if
        libcurl = open_dll(curl_dll_name)
        if libcurl=NULL then
            if fatal then
                Abort("cannot open "&curl_dll_name)
            end if
        end if
    end if
end procedure

--------------------------------------------------------------------------------

global function curl_loadlib(string dll_name="")
-- Allow app to helpfully suggest or actually open (eg)
-- http://phix.x10.mx/pmwiki/pmwiki.php?n=Main.Libcurldlls
-- or even automatically download them from wherever (ha!)
    if libcurl=NULL then
        curl_init(dll_name, false)
        return libcurl!=NULL
    end if
    return true
end function

-- Note: peek_pointer() on OE is a wrapper for peek4u() -- !!! ffs, jfc !!!
-- Also note that testing for NULL is meant to be a libcurl-specific thing,
-- fine by me if you wanna test before all calls instead, and/or make some
-- nulls (but probably not all) trigger a fatal error.
function peek_pointer(object pMem)
    if pMem=NULL then return NULL end if
    return peekNS(pMem,machine_word(),false)
end function

function peek_strings(atom pMem)
sequence res = {}
    while 1 do
        atom pString = peek_pointer(pMem)
        if pString=NULL then exit end if
        res = append(res,peek_string(pString))
        pMem += machine_word()
    end while
    return res
end function

function peek_stringn(atom pMem)
    string res = iff(pMem=NULL?"NULL":peek_string(pMem))
    return res
end function

--------------------------------------------------------------------------------

global type CURLcode(integer n)
  return ((n>=CURLE_OK) and (n<CURLE_LAST)) 
      or (n=CURLE_CANT_OPEN_FILE)
end type

global type CURLMcode(integer n)
  return ((n>=CURLM_CALL_MULTI_PERFORM) and (n<CURLM_LAST))
end type

--------------------------------------------------------------------------------

atom xcurl_easy_strerror = NULL
global function curl_easy_strerror(CURLcode code)
    if xcurl_easy_strerror=NULL then
        xcurl_easy_strerror = link_c_func(libcurl, "curl_easy_strerror", {C_INT}, C_PTR)
    end if
    atom pRes = c_func(xcurl_easy_strerror, {code})
    string res = peek_string(pRes)
    return res
end function

nopoll_base64_decode
nopoll_base64_encode
nopoll_calloc
nopoll_cleanup_library
nopoll_cmp
nopoll_conn_accept
nopoll_conn_accept_complete
__nopoll_conn_accept_complete_common
nopoll_conn_accept_socket
__nopoll_conn_call_on_ready_if_defined
nopoll_conn_check_mime_header_repeated
nopoll_conn_close
nopoll_conn_close_ext
nopoll_conn_complete_handshake
nopoll_conn_complete_handshake_check
nopoll_conn_complete_handshake_check_client
nopoll_conn_complete_handshake_check_listener
nopoll_conn_complete_handshake_client
nopoll_conn_complete_handshake_listener
nopoll_conn_complete_pending_write                          -- [DONE]
__nopoll_conn_complete_pending_write_reduce_header
nopoll_conn_connect_timeout                                 -- [DONE]
nopoll_conn_ctx
nopoll_conn_default_receive
nopoll_conn_default_send
nopoll_conn_flush_writes                                    -- [DONE]
nopoll_conn_get_accepted_protocol
__nopoll_conn_get_client_init
nopoll_conn_get_close_reason
nopoll_conn_get_close_status
nopoll_conn_get_connect_timeout
nopoll_conn_get_cookie
nopoll_conn_get_hook
nopoll_conn_get_host_header
nopoll_conn_get_http_url
nopoll_conn_get_id
nopoll_conn_get_listener
nopoll_conn_get_mime_header
nopoll_conn_get_msg                                         -- [DONE]
nopoll_conn_get_origin
nopoll_conn_get_requested_protocol
nopoll_conn_get_requested_url
__nopoll_conn_get_ssl_context
nopoll_conn_host
nopoll_conn_is_ok                                           -- [DONE]
nopoll_conn_is_ready
nopoll_conn_is_tls_on
nopoll_conn_log_ssl
nopoll_conn_mask_content
nopoll_conn_new                                             -- [DONE]
nopoll_conn_new6
__nopoll_conn_new_common
nopoll_conn_new_opts
nopoll_conn_new_with_socket
nopoll_conn_opts_free
__nopoll_conn_opts_free_common
nopoll_conn_opts_new
nopoll_conn_opts_ref
__nopoll_conn_opts_release_if_needed
nopoll_conn_opts_set_cookie
nopoll_conn_opts_set_extra_headers
nopoll_conn_opts_set_interface
nopoll_conn_opts_set_reuse
nopoll_conn_opts_set_ssl_certs
nopoll_conn_opts_set_ssl_protocol
nopoll_conn_opts_skip_origin_check
nopoll_conn_opts_ssl_peer_verify
nopoll_conn_opts_unref
nopoll_conn_pending_write_bytes                                 -- [DONE]
nopoll_conn_port
nopoll_conn_produce_accept_key
nopoll_conn_read                                                -- [DONE]
nopoll_conn_read_pending                                        -- [DONE]
nopoll_conn_readline
__nopoll_conn_receive
nopoll_conn_ref
nopoll_conn_ref_count
nopoll_conn_role
nopoll_conn_send_binary
nopoll_conn_send_binary_fragment
__nopoll_conn_send_common
nopoll_conn_send_frame
nopoll_conn_send_ping
nopoll_conn_send_pong
nopoll_conn_send_text                                           -- [DONE]
nopoll_conn_send_text_fragment
nopoll_conn_set_accepted_protocol
nopoll_conn_set_bind_interface
nopoll_conn_set_hook
nopoll_conn_set_on_close
nopoll_conn_set_on_msg                                          -- [DONE]
nopoll_conn_set_on_ready
nopoll_conn_set_sock_block
nopoll_conn_set_sock_tcp_nodelay
nopoll_conn_set_socket
__nopoll_conn_set_ssl_client_options
nopoll_conn_shutdown
nopoll_conn_sock_connect
nopoll_conn_sock_connect_opts
__nopoll_conn_sock_connect_opts_internal
nopoll_conn_socket
__nopoll_conn_ssl_ctx_debug
__nopoll_conn_ssl_verify_callback
__nopoll_conn_tls_handle_error
nopoll_conn_tls_new
nopoll_conn_tls_new6
nopoll_conn_tls_new_with_socket
nopoll_conn_tls_receive
nopoll_conn_tls_send
nopoll_conn_unref
nopoll_conn_wait_until_connection_ready
nopoll_ctx_conns
nopoll_ctx_find_certificate
nopoll_ctx_foreach_conn
nopoll_ctx_new                                              -- [DONE]
nopoll_ctx_ref
nopoll_ctx_ref_count
nopoll_ctx_register_conn
nopoll_ctx_set_certificate
nopoll_ctx_set_on_accept
nopoll_ctx_set_on_msg                                       -- [DONE]
nopoll_ctx_set_on_open
nopoll_ctx_set_on_ready
nopoll_ctx_set_post_ssl_check
nopoll_ctx_set_protocol_version
nopoll_ctx_set_ssl_context_creator
__nopoll_ctx_sigpipe_do_nothing
nopoll_ctx_unref                                            -- [DONE]
nopoll_ctx_unregister_conn
nopoll_free
nopoll_get_16bit
nopoll_get_32bit
nopoll_get_8bit
nopoll_get_bit
nopoll_int2bin
nopoll_int2bin_print
nopoll_io_get_engine
nopoll_io_release_engine
nopoll_io_wait_select_add_to
nopoll_io_wait_select_clear
nopoll_io_wait_select_create
nopoll_io_wait_select_destroy
nopoll_io_wait_select_is_set
nopoll_io_wait_select_wait
nopoll_is_white_space
nopoll_listener_accept
nopoll_listener_from_socket
nopoll_listener_new
nopoll_listener_new6
nopoll_listener_new_opts
nopoll_listener_new_opts6
__nopoll_listener_new_opts_internal
nopoll_listener_set_certificate
nopoll_listener_sock_listen
__nopoll_listener_sock_listen_internal
nopoll_listener_tls_new
nopoll_listener_tls_new6
nopoll_listener_tls_new_opts
nopoll_listener_tls_new_opts6
__nopoll_listener_tls_new_opts_internal
__nopoll_log
nopoll_log_color_enable
nopoll_log_color_is_enabled
nopoll_log_enable
nopoll_log_is_enabled
nopoll_log_set_handler
nopoll_loop_init
nopoll_loop_process
nopoll_loop_process_data
nopoll_loop_register
nopoll_loop_stop                                            -- [DONE]
nopoll_loop_wait                                            -- [DONE]
nopoll_msg_get_payload                                      -- [DONE]
nopoll_msg_get_payload_size                                 -- [DONE]
nopoll_msg_is_final
nopoll_msg_is_fragment
nopoll_msg_join
nopoll_msg_new
nopoll_msg_opcode                                           -- [DONE]
nopoll_msg_ref
nopoll_msg_ref_count
nopoll_msg_unref
__nopoll_mutex_create
nopoll_mutex_create
__nopoll_mutex_destroy
nopoll_mutex_destroy
__nopoll_mutex_lock
nopoll_mutex_lock
__nopoll_mutex_unlock
nopoll_mutex_unlock
nopoll_ncmp
nopoll_nonce
__nopoll_nonce_init
__nopoll_pack_content
nopoll_realloc
nopoll_set_16bit
nopoll_set_32bit
nopoll_set_bit
nopoll_show_byte
nopoll_sleep                                                -- [DONE]
nopoll_strdup
nopoll_strdup_printf
nopoll_strdup_printfv
nopoll_thread_handlers
nopoll_timeval_substract
__nopoll_tls_was_init
nopoll_trim
nopoll_vprintf_len
nopoll_win32_blocking_enable
__nopoll_win32_blocking_socket_set
nopoll_win32_gettimeofday
nopoll_win32_init
nopoll_win32_nonblocking_enable
__nopoll_win32_was_init
--*/
