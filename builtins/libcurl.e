--
-- libcurl.e
--
--  A libcurl wrapper, based heavily on the work of Raymond Smith and jmduro, 
--                     rewritten mainly as part of the documentation process.
--  
--  Note that the (windows) dlls are /not/ included in the standard distribution,
--  but can be downloaded from http://phix.x10.mx/pmwiki/pmwiki.php?n=Main.Libcurldlls
--

procedure Abort(string msg)
    ?msg
    {} = wait_key()
    abort(0)
end procedure

atom libcurl = NULL
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
            libcurl = open_dll(join_path({"builtins",curl_dll_name}))
        end if
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

function link_c_func(atom dll, sequence name, sequence args, atom result)
    if dll=NULL then ?9/0 end if
    integer rid = define_c_func(dll, "+" & name, args, result)
    if rid<1 then
        Abort("cannot link "&name)
    end if
    return rid
end function

function link_c_proc(atom dll, sequence name, sequence args)
    if dll=NULL then ?9/0 end if
    integer rid = define_c_proc(dll, "+" & name, args)
    if rid<1 then
        Abort("cannot line "&name)
    end if
    return rid
end function

constant W = machine_word()

-- Note: peek_pointer() on OE is a wrapper for peek4u() -- !!! ffs, jfc !!!
-- Also note that testing for NULL is meant to be a libcurl-specific thing,
-- fine by me if you wanna test before all calls instead, and/or make some
-- nulls (but probably not all) trigger a fatal error.
function peek_pointer(object pMem)
    if pMem=NULL then return NULL end if
    return peekNS(pMem,W,false)
end function

function peek_strings(atom pMem)
sequence res = {}
    while 1 do
        atom pString = peek_pointer(pMem)
        if pString=NULL then exit end if
        res = append(res,peek_string(pString))
        pMem += W
    end while
    return res
end function

function peek_stringn(atom pMem)
    string res = iff(pMem=NULL?"NULL":peek_string(pMem))
    return res
end function

--------------------------------------------------------------------------------

-- libcurl error codes (uncomment as needed/documented)

global constant
  CURLE_OK                       =  0,
  CURLE_UNSUPPORTED_PROTOCOL     =  1,  -- 1
  CURLE_FAILED_INIT              =  2,  -- 2
--/*
  CURLE_URL_MALFORMAT            =  3,  -- 3
  CURLE_NOT_BUILT_IN             =  4,  -- 4 -- (was CURLE_URL_MALFORMAT_USER?)
  CURLE_COULDNT_RESOLVE_PROXY    =  5,  -- 5
--*/
  CURLE_COULDNT_RESOLVE_HOST     =  6,  -- 6
--/*
  CURLE_COULDNT_CONNECT          =  7,  -- 7
  CURLE_FTP_WEIRD_SERVER_REPLY   =  8,  -- 8
  CURLE_REMOTE_ACCESS_DENIED     =  9,  -- 9 a service was denied by the server
                                 --     --   due to lack of access - when login fails
                                 --     --   this is not returned.
  CURLE_FTP_ACCEPT_FAILED        = 10,  -- 10   -- (was CURLE_FTP_USER_PASSWORD_INCORRECT?)
  CURLE_FTP_WEIRD_PASS_REPLY     = 11,  -- 11
  CURLE_FTP_ACCEPT_TIMEOUT       = 12,  -- 12 - timeout occurred accepting server
                                              -- (was CURLE_FTP_WEIRD_USER_REPLY?)
  CURLE_FTP_WEIRD_PASV_REPLY     = 13,  -- 13
  CURLE_FTP_WEIRD_227_FORMAT     = 14,  -- 14
  CURLE_FTP_CANT_GET_HOST        = 15,  -- 15
  CURLE_HTTP2                    = 16,  -- 16 - A problem in the http2 framing layer.
                                              -- (was CURLE_FTP_CANT_RECONNECT?)
  CURLE_FTP_COULDNT_SET_TYPE     = 17,  -- 17 -- (was CURLE_FTP_COULDNT_SET_BINARY?)
  CURLE_PARTIAL_FILE             = 18,  -- 18
  CURLE_FTP_COULDNT_RETR_FILE    = 19,  -- 19
  CURLE_OBSOLETE20               = 20,  -- 20 - NOT USED (was CURLE_FTP_WRITE_ERROR?)
  CURLE_QUOTE_ERROR              = 21,  -- 21 - quote command failure
--*/
  CURLE_HTTP_RETURNED_ERROR      = 22,  -- 22   -- (was CURLE_HTTP_NOT_FOUND?)
--/*
  CURLE_WRITE_ERROR              = 23,  -- 23
  CURLE_OBSOLETE24               = 24,  -- 24 - NOT USED (was CURLE_MALFORMAT_USER?)
  CURLE_UPLOAD_FAILED            = 25,  -- 25 - failed upload "command"
                                              -- (was CURLE_FTP_COULDNT_STOR_FILE?)
  CURLE_READ_ERROR               = 26,  -- 26 - couldn't open/read from file
  CURLE_OUT_OF_MEMORY            = 27,  -- 27
  -- Note: CURLE_OUT_OF_MEMORY may sometimes indicate a conversion error
  --         instead of a memory allocation error if CURL_DOES_CONVERSIONS
  --         is defined
  CURLE_OPERATION_TIMEDOUT       = 28,  -- 28 - the timeout time was reached
                                              --  (was CURLE_OPERATION_TIMEOUTED?)
  CURLE_OBSOLETE29               = 29,  -- 29 - NOT USED (was CURLE_FTP_COULDNT_SET_ASCII?)
  CURLE_FTP_PORT_FAILED          = 30,  -- 30 - FTP PORT operation failed
  CURLE_FTP_COULDNT_USE_REST     = 31,  -- 31 - the REST command failed
  CURLE_OBSOLETE32               = 32,  -- 32 - NOT USED (was CURLE_FTP_COULDNT_GET_SIZE)
  CURLE_RANGE_ERROR              = 33,  -- 33 - RANGE "command" didn't work
                                              -- (was CURLE_HTTP_RANGE_ERROR)
  CURLE_HTTP_POST_ERROR          = 34,  -- 34
  CURLE_SSL_CONNECT_ERROR        = 35,  -- 35 - wrong when connecting with SSL
  CURLE_BAD_DOWNLOAD_RESUME      = 36,  -- 36 - couldn't resume download
  CURLE_FILE_COULDNT_READ_FILE   = 37,  -- 37
  CURLE_LDAP_CANNOT_BIND         = 38,  -- 38
  CURLE_LDAP_SEARCH_FAILED       = 39,  -- 39
  CURLE_OBSOLETE40               = 40,  -- 40 - NOT USED (was CURLE_LIBRARY_NOT_FOUND?)
  CURLE_FUNCTION_NOT_FOUND       = 41,  -- 41
  CURLE_ABORTED_BY_CALLBACK      = 42,  -- 42
  CURLE_BAD_FUNCTION_ARGUMENT    = 43,  -- 43
  CURLE_OBSOLETE44               = 44,  -- 44 - NOT USED (was CURLE_BAD_CALLING_ORDER?)
  CURLE_INTERFACE_FAILED         = 45,  -- 45 - CURLOPT_INTERFACE failed
                                              -- (was CURLE_HTTP_PORT_FAILED?)
  CURLE_OBSOLETE46               = 46,  -- 46 - NOT USED (was CURLE_BAD_PASSWORD_ENTERED?)
  CURLE_TOO_MANY_REDIRECTS       = 47,  -- 47 - catch endless re-direct loops
--*/
  CURLE_UNKNOWN_OPTION           = 48,  -- 48 - User specified an unknown option
                                              -- (was CURLE_UNKNOWN_TELNET_OPTION?)
--/*
  CURLE_TELNET_OPTION_SYNTAX     = 49,  -- 49 - Malformed telnet option
  CURLE_OBSOLETE50               = 50,  -- 50 - NOT USED
  CURLE_PEER_FAILED_VERIFICATION = 51,  -- 51 - peer's certificate or fingerprint
                                        --    -- wasn't verified fine
                                              -- (was CURLE_SSL_PEER_CERTIFICATE?)
  CURLE_GOT_NOTHING              = 52,  -- 52 - when this is a specific error
  CURLE_SSL_ENGINE_NOTFOUND      = 53,  -- 53 - SSL crypto engine not found
  CURLE_SSL_ENGINE_SETFAILED     = 54,  -- 54 - can not set SSL crypto engine as default
  CURLE_SEND_ERROR               = 55,  -- 55 - failed sending network data
--*/
  CURLE_RECV_ERROR               = 56,  -- 56 - failure in receiving network data
--/*
  CURLE_OBSOLETE57               = 57,  -- 57 - NOT IN USE
  CURLE_SSL_CERTPROBLEM          = 58,  -- 58 - problem with the local certificate
  CURLE_SSL_CIPHER               = 59,  -- 59 - couldn't use specified cipher
  CURLE_SSL_CACERT               = 60,  -- 60 - problem with the CA cert (path?)
  CURLE_BAD_CONTENT_ENCODING     = 61,  -- 61 - Unrecognized/bad encoding
  CURLE_LDAP_INVALID_URL         = 62,  -- 62 - Invalid LDAP URL
  CURLE_FILESIZE_EXCEEDED        = 63,  -- 63 - Maximum file size exceeded
--*/
  CURLE_USE_SSL_FAILED           = 64,  -- 64 - Requested FTP SSL level failed
--/*
  CURLE_SEND_FAIL_REWIND         = 65,  -- 65 - Sending the data requires a rewind
                                        --    -- that failed
  CURLE_SSL_ENGINE_INITFAILED    = 66,  -- 66 - failed to initialise ENGINE
  CURLE_LOGIN_DENIED             = 67,  -- 67 - user = , password or similar was not
                                        --    -- accepted and we failed to login
  CURLE_TFTP_NOTFOUND            = 68,  -- 68 - file not found on server
  CURLE_TFTP_PERM                = 69,  -- 69 - permission problem on server
  CURLE_REMOTE_DISK_FULL         = 70,  -- 70 - out of disk space on server
  CURLE_TFTP_ILLEGAL             = 71,  -- 71 - Illegal TFTP operation
  CURLE_TFTP_UNKNOWNID           = 72,  -- 72 - Unknown transfer ID
  CURLE_REMOTE_FILE_EXISTS       = 73,  -- 73 - File already exists
  CURLE_TFTP_NOSUCHUSER          = 74,  -- 74 - No such user
  CURLE_CONV_FAILED              = 75,  -- 75 - conversion failed
  CURLE_CONV_REQD                = 76,  -- 76 - caller must register conversion
                                        --    -- callbacks using curl_easy_setopt options
                                        --    -- CURLOPT_CONV_FROM_NETWORK_FUNCTION = ,
                                        --    -- CURLOPT_CONV_TO_NETWORK_FUNCTION = , and
                                        --    -- CURLOPT_CONV_FROM_UTF8_FUNCTION
  CURLE_SSL_CACERT_BADFILE       = 77,  -- 77 - could not load CACERT file = , missing
                                        --    -- or wrong format
  CURLE_REMOTE_FILE_NOT_FOUND    = 78,  -- 78 - remote file not found
  CURLE_SSH                      = 79,  -- 79 - error from the SSH layer = , somewhat
                                        --    -- generic so the error message will be of
                                        --    -- interest when this has happened
  CURLE_SSL_SHUTDOWN_FAILED      = 80,  -- 80 - Failed to shut down the SSL connection
--*/
  CURLE_AGAIN                    = 81,  -- 81 - socket is not ready for send/recv = ,
                                        --    -- wait till it's ready and try again
--/*
  CURLE_SSL_CRL_BADFILE          = 82,  -- 82 - could not load CRL file = , missing or
                                        --    -- wrong format
  CURLE_SSL_ISSUER_ERROR         = 83,  -- 83 - Issuer check failed.
  CURLE_FTP_PRET_FAILED          = 84,  -- 84 - a PRET command failed
  CURLE_RTSP_CSEQ_ERROR          = 85,  -- 85 - mismatch of RTSP CSeq numbers
  CURLE_RTSP_SESSION_ERROR       = 86,  -- 86 - mismatch of RTSP Session Ids
  CURLE_FTP_BAD_FILE_LIST        = 87,  -- 87 - unable to parse FTP file list
  CURLE_CHUNK_FAILED             = 88,  -- 88 - chunk callback reported error
  CURLE_NO_CONNECTION_AVAILABLE  = 89,  -- 89 - No connection available = , the
                                        --    -- session will be queued
  CURLE_SSL_PINNEDPUBKEYNOTMATCH = 90,  -- 90 - specified pinned global key did not
                                        --    -- match
  CURLE_SSL_INVALIDCERTSTATUS    = 91,  -- 91 - invalid certificate status
  CURLE_HTTP2_STREAM             = 92   -- 92 - stream error in HTTP/2 framing layer
--*/ $
constant
  CURLE_LAST                     = 93   -- never use!

global constant
   -- Euphoria specific
   CURLE_CANT_OPEN_FILE = 500          -- curl_easy_get_file() cannot open specified output file

global type CURLcode(integer n)
  return ((n>=CURLE_OK) and (n<CURLE_LAST)) 
      or (n=CURLE_CANT_OPEN_FILE)
end type

global constant
  CURLM_CALL_MULTI_PERFORM  = -1,   -- not really an error, call curl_multi_perform again (pre-7.20.0 only)
  CURLM_OK                  = 0,    -- things are fine
  CURLM_BAD_HANDLE          = 1,    -- the passed-in handle is not a valid CURLM handle.
  CURLM_BAD_EASY_HANDLE     = 2,    -- an easy handle was not good/valid.
  CURLM_OUT_OF_MEMORY       = 3,    -- you are doomed.
  CURLM_INTERNAL_ERROR      = 4,    -- this can only be returned if libcurl bugs. Please report it to us!
  CURLM_BAD_SOCKET          = 5,    -- the passed-in socket is not one known to libcurl. (Added in 7.15.4)
  CURLM_UNKNOWN_OPTION      = 6,    -- curl_multi_setopt() with unsupported option (Added in 7.15.4)
  CURLM_ADDED_ALREADY       = 7     -- an easy handle already added to a multi handle was attempted to 
                                    -- get added a second time. (Added in 7.32.1)
constant
  CURLM_LAST                = 8

global type CURLMcode(integer n)
  return ((n>=CURLM_CALL_MULTI_PERFORM) and (n<CURLM_LAST))
end type

global constant
  CURLSHE_OK           = 0,  -- all is fine
  CURLSHE_BAD_OPTION   = 1,  -- 1
  CURLSHE_IN_USE       = 2,  -- 2
  CURLSHE_INVALID      = 3,  -- 3
  CURLSHE_NOMEM        = 4,  -- 4 out of memory
  CURLSHE_NOT_BUILT_IN = 5   -- 5 feature not present in lib

constant
  CURLSHE_LAST         = 6   -- never use

global type CURLSHcode(integer n)
  return ((n>=CURLSHE_OK) and (n<CURLSHE_LAST))
end type

--global 
constant
  CURLINFO_STRING   = #100000,
  CURLINFO_LONG     = #200000,
  CURLINFO_DOUBLE   = #300000,
  CURLINFO_SLIST    = #400000,
  CURLINFO_SOCKET   = #500000,
--  CURLINFO_MASK   = #0FFFFF,
  CURLINFO_TYPEMASK = #F00000

global constant
--CURLINFO_NONE                     = 0, -- first, never use this
  CURLINFO_EFFECTIVE_URL            = CURLINFO_STRING + 1,
  CURLINFO_RESPONSE_CODE            = CURLINFO_LONG   + 2,
  CURLINFO_TOTAL_TIME               = CURLINFO_DOUBLE + 3,
  CURLINFO_NAMELOOKUP_TIME          = CURLINFO_DOUBLE + 4,
  CURLINFO_CONNECT_TIME             = CURLINFO_DOUBLE + 5,
  CURLINFO_PRETRANSFER_TIME         = CURLINFO_DOUBLE + 6,
  CURLINFO_SIZE_UPLOAD              = CURLINFO_DOUBLE + 7,
  CURLINFO_SIZE_DOWNLOAD            = CURLINFO_DOUBLE + 8,
  CURLINFO_SPEED_DOWNLOAD           = CURLINFO_DOUBLE + 9,
  CURLINFO_SPEED_UPLOAD             = CURLINFO_DOUBLE + 10,
  CURLINFO_HEADER_SIZE              = CURLINFO_LONG   + 11,
  CURLINFO_REQUEST_SIZE             = CURLINFO_LONG   + 12,
  CURLINFO_SSL_VERIFYRESULT         = CURLINFO_LONG   + 13,
  CURLINFO_FILETIME                 = CURLINFO_LONG   + 14,
  CURLINFO_CONTENT_LENGTH_DOWNLOAD  = CURLINFO_DOUBLE + 15,
  CURLINFO_CONTENT_LENGTH_UPLOAD    = CURLINFO_DOUBLE + 16,
  CURLINFO_STARTTRANSFER_TIME       = CURLINFO_DOUBLE + 17,
  CURLINFO_CONTENT_TYPE             = CURLINFO_STRING + 18,
  CURLINFO_REDIRECT_TIME            = CURLINFO_DOUBLE + 19,
  CURLINFO_REDIRECT_COUNT           = CURLINFO_LONG   + 20,
  CURLINFO_PRIVATE                  = CURLINFO_LONG   + 21,         --DEV not supported??
  CURLINFO_HTTP_CONNECTCODE         = CURLINFO_LONG   + 22,
  CURLINFO_HTTPAUTH_AVAIL           = CURLINFO_LONG   + 23,
  CURLINFO_PROXYAUTH_AVAIL          = CURLINFO_LONG   + 24,
  CURLINFO_OS_ERRNO                 = CURLINFO_LONG   + 25,
  CURLINFO_NUM_CONNECTS             = CURLINFO_LONG   + 26,
  CURLINFO_SSL_ENGINES              = CURLINFO_SLIST  + 27,
  CURLINFO_COOKIELIST               = CURLINFO_SLIST  + 28,
  CURLINFO_LASTSOCKET               = CURLINFO_LONG   + 29,
  CURLINFO_FTP_ENTRY_PATH           = CURLINFO_STRING + 30,
  CURLINFO_REDIRECT_URL             = CURLINFO_STRING + 31,
  CURLINFO_PRIMARY_IP               = CURLINFO_STRING + 32,
  CURLINFO_APPCONNECT_TIME          = CURLINFO_DOUBLE + 33,
  CURLINFO_CERTINFO                 = CURLINFO_SLIST  + 34,
  CURLINFO_CONDITION_UNMET          = CURLINFO_LONG   + 35,
  CURLINFO_RTSP_SESSION_ID          = CURLINFO_STRING + 36,
  CURLINFO_RTSP_CLIENT_CSEQ         = CURLINFO_LONG   + 37,
  CURLINFO_RTSP_SERVER_CSEQ         = CURLINFO_LONG   + 38,
  CURLINFO_RTSP_CSEQ_RECV           = CURLINFO_LONG   + 39,
  CURLINFO_PRIMARY_PORT             = CURLINFO_LONG   + 40,
  CURLINFO_LOCAL_IP                 = CURLINFO_STRING + 41,
  CURLINFO_LOCAL_PORT               = CURLINFO_LONG   + 42,
  CURLINFO_TLS_SESSION              = CURLINFO_SLIST  + 43,
  CURLINFO_ACTIVESOCKET             = CURLINFO_SOCKET + 44,
  CURLINFO_TLS_SSL_PTR              = CURLINFO_SLIST  + 45,
  CURLINFO_HTTP_VERSION             = CURLINFO_LONG   + 46
  -- Fill in new entries below here!

--  CURLINFO_LASTONE            = 46

global constant
   CURL_ERROR_SIZE = 256

--global type CURLINFO(integer n)
--  integer rem
--
--  rem = and_bits(n, #0000FF)
--  return ((rem >= #0) and (rem <= #2F))
--end type

--------------------------------------------------------------------------------

-- the kind of data that is passed to information_callback
global constant
  CURLINFO_TEXT         = 0,
  CURLINFO_HEADER_IN    = 1,  -- 1
  CURLINFO_HEADER_OUT   = 2,  -- 2
  CURLINFO_DATA_IN      = 3,  -- 3
  CURLINFO_DATA_OUT     = 4,  -- 4
  CURLINFO_SSL_DATA_IN  = 5,  -- 5
  CURLINFO_SSL_DATA_OUT = 6,  -- 6
  CURLINFO_END          = 7

--global type curl_infotype(integer n)
--  return ((n >= CURLINFO_TEXT) and (n <= CURLINFO_END))
--end type

-- typedef int (*curl_debug_callback)
--        (CURL *handle,      -- the handle/transfer this concerns
--         curl_infotype type, -- what kind of data
--         char *data,        -- points to the data
--         size_t size,       -- size of the data pointed to
--         void *userptr);    -- whatever the user please

--------------------------------------------------------------------------------

atom xcurl_easy_strerror = NULL
global function curl_easy_strerror(CURLcode error)
    if xcurl_easy_strerror=NULL then
        xcurl_easy_strerror = link_c_func(libcurl, "curl_easy_strerror", {C_INT}, C_PTR)
    end if
    atom pRes = c_func(xcurl_easy_strerror, {error})
    string res = peek_string(pRes)
    return res
end function

--------------------------------------------------------------------------------

atom xcurl_version = NULL
global function curl_version()
-- Returns the libcurl version as an ascii string.
    if xcurl_version=NULL then
        if libcurl=NULL then curl_init() end if
        xcurl_version = link_c_func(libcurl, "curl_version", {}, C_PTR)
    end if
    atom pVersion = c_func(xcurl_version, {})
    string res = peek_string(pVersion)
    return res
end function

--------------------------------------------------------------------------------

include builtins\cffi.e

global constant CURLVERSION_FIRST  = 0,
                CURLVERSION_SECOND = 1,
                CURLVERSION_THIRD  = 2,
                CURLVERSION_FOURTH = 3,
                CURLVERSION_FIFTH  = 4,
                CURLVERSION_NOW = CURLVERSION_FIFTH

--global type CURLversion(integer n)
--  return ((n >= CURLVERSION_FIRST) and (n <= CURLVERSION_LAST))
--end type

constant tCVID = """
typedef struct {
  int age;                  // age of the returned struct (CURLversion replaced with int)
  const char *version;      // LIBCURL_VERSION
  unsigned int version_num; // LIBCURL_VERSION_NUM
  const char *host;         // OS/host/cpu/machine when configured
  int features;             // bitmask, see defines below
  const char *ssl_version;  // human readable string
  long ssl_version_num;     // not used anymore, always 0
  const char *libz_version; // human readable string
  // protocols is terminated by an entry with a NULL protoname
  const char * const *protocols;

  // The fields below this were added in CURLVERSION_SECOND
  const char *ares;
  int ares_num;

  // This field was added in CURLVERSION_THIRD
  const char *libidn;

  // These field were added in CURLVERSION_FOURTH

  // Same as '_libiconv_version' if built with HAVE_ICONV
  int iconv_ver_num;

  const char *libssh_version; // human readable string

  // These fields were added in CURLVERSION_FIFTH

  unsigned int brotli_ver_num;  // Numeric Brotli version
                                //  (MAJOR << 24) | (MINOR << 12) | PATCH
  const char *brotli_version; // human readable string.
} curl_version_info_data;""",
--DEV:
idCVID = define_struct(tCVID)

--DOC:
-- age: An internal version field that indicates the info structure size.
--      This version asks for CURLVERSION_FIFTH, because that is the size it allocates,
--      and all the fields it can understand, some dlls may return an earlier version.
--      Initially I might suggest that most applications should ignore this field.

global constant LIBCURL_VERSION_AGE          = 1,
                LIBCURL_VERSION_STRING       = 2,
                LIBCURL_VERSION_NUM          = 3,
                LIBCURL_VERSION_HOST         = 4,
                LIBCURL_VERSION_FEATURES     = 5,
                LIBCURL_VERSION_SSL_VERSION  = 6,
                LIBCURL_VERSION_LIBZ_VERSION = 7,
                LIBCURL_VERSION_PROTOCOLS    = 8,
                -- if LIBCURL_VERSION_AGE>=CURLVERSION_SECOND
                LIBCURL_VERSION_ARES         = 9,
                LIBCURL_VERSION_ARES_NUM     = 10,
                -- if LIBCURL_VERSION_AGE>=CURLVERSION_THIRD
                LIBCURL_VERSION_LIBIDN       = 11,
                -- if LIBCURL_VERSION_AGE>=CURLVERSION_FOURTH
                LIBCURL_VERSION_ICONV_VER_NUM = 12,
                LIBCURL_VERSION_LIBSSH_VERSION = 13,
                -- if LIBCURL_VERSION_AGE>=CURLVERSION_FIFTH
                LIBCURL_VERSION_BROTLI_VER_NUM = 14,
                LIBCURL_VERSION_BROTLI_VERSION = 15

--global 
constant
  CURL_VERSION_IPV6         = #000001,  -- IPv6-enabled
  CURL_VERSION_KERBEROS4    = #000002,  -- Kerberos V4 auth is supported (deprecated)
  CURL_VERSION_SSL          = #000004,  -- SSL options are present
  CURL_VERSION_LIBZ         = #000008,  -- libz features are present
  CURL_VERSION_NTLM         = #000010,  -- NTLM auth is supported
  CURL_VERSION_GSSNEGOTIATE = #000020,  -- Negotiate auth is supported (deprecated)
  CURL_VERSION_DEBUG        = #000040,  -- Built with debug capabilities
  CURL_VERSION_ASYNCHDNS    = #000080,  -- Asynchronous DNS resolves
  CURL_VERSION_SPNEGO       = #000100,  -- SPNEGO auth is supported
  CURL_VERSION_LARGEFILE    = #000200,  -- Supports files larger than 2GB
  CURL_VERSION_IDN          = #000400,  -- Internationized Domain Names are supported
  CURL_VERSION_SSPI         = #000800,  -- Built against Windows SSPI
  CURL_VERSION_CONV         = #001000,  -- Character conversions supported
  CURL_VERSION_CURLDEBUG    = #002000,  -- Debug memory tracking supported
  CURL_VERSION_TLSAUTH_SRP  = #004000,  -- TLS-SRP auth is supported
  CURL_VERSION_NTLM_WB      = #008000,  -- NTLM delegation to winbind helper is suported
  CURL_VERSION_HTTP2        = #010000,  -- HTTP2 support built-in
  CURL_VERSION_GSSAPI       = #020000,  -- Built against a GSS-API library
  CURL_VERSION_KERBEROS5    = #040000,  -- Kerberos V5 auth is supported
  CURL_VERSION_UNIX_SOCKETS = #080000,  -- Unix domain sockets support
  CURL_VERSION_PSL          = #100000,  -- Mozilla's Public Suffix List, used
                                        -- for cookie domain verification
  CURL_VERSION_HTTPS_PROXY  = #200000,  -- HTTPS-proxy support built-in
  CURL_VERSION_MULTI_SSL    = #400000,  -- Multiple SSL backends available
  CURL_VERSION_BROTLI       = #800000   -- Brotli features are present

constant FeatureSet = { {CURL_VERSION_IPV6,         "IPV6"},
                        {CURL_VERSION_KERBEROS4,    "KERBEROS4"},
                        {CURL_VERSION_SSL,          "SSL"},
                        {CURL_VERSION_LIBZ,         "LIBZ"},
                        {CURL_VERSION_NTLM,         "NTLM"},
                        {CURL_VERSION_GSSNEGOTIATE, "GSSNEGOTIATE"},
                        {CURL_VERSION_DEBUG,        "DEBUG"},
                        {CURL_VERSION_ASYNCHDNS,    "ASYNCHDNS"},
                        {CURL_VERSION_SPNEGO,       "SPNEGO"},
                        {CURL_VERSION_LARGEFILE,    "LARGEFILE"},
                        {CURL_VERSION_IDN,          "IDN"},
                        {CURL_VERSION_SSPI,         "SSPI"},
                        {CURL_VERSION_CONV,         "CONV"},
                        {CURL_VERSION_CURLDEBUG,    "CURLDEBUG"},
                        {CURL_VERSION_TLSAUTH_SRP,  "TLSAUTH_SRP"},
                        {CURL_VERSION_NTLM_WB,      "NTLM_WB"},
                        {CURL_VERSION_HTTP2,        "HTTP2"},
                        {CURL_VERSION_GSSAPI,       "GSSAPI"},
                        {CURL_VERSION_KERBEROS5,    "KERBEROS5"},
                        {CURL_VERSION_UNIX_SOCKETS, "UNIX_SOCKETS"},
                        {CURL_VERSION_PSL,          "PSL"},
                        {CURL_VERSION_HTTPS_PROXY,  "HTTPS_PROXY"},
                        {CURL_VERSION_MULTI_SSL,    "MULTI_SSL"},
                        {CURL_VERSION_BROTLI,       "BROTLI"} }

--------------------------------------------------------------------------------

atom xcurl_version_info = NULL
--global function curl_version_info(bool bRawPtr=false)
-- If bRawPtr is true returns a raw pointer to the info, otherwise it
global function curl_version_info()
--
-- Returns NULL if curl_version_info is not supported (eg 7.9.8 from 2002),
-- otherwise a sequence which can be examined using the LIBCURL_VERSION_XXX 
-- constants (elements above LIBCURL_VERSION_PROTOCOLS=8 may be missing).
--
-- res[LIBCURL_VERSION_FEATURES] can contain the following (literal) strings:
-- {"IPV6","KERBEROS4","SSL","LIBZ","NTLM","GSSNEGOTIATE","DEBUG","ASYNCHDNS",
--  "SPNEGO","LARGEFILE","IDN","SSPI","CONV","CURLDEBUG","TLSAUTH_SRP","NTLM_WB",
--  "HTTP2","GSSAPI","KERBEROS5","UNIX_SOCKETS","PSL","HTTPS_PROXY","MULTI_SSL",
--  "BROTLI"}
--
-- res[LIBCURL_VERSION_PROTOCOLS] can contain the following (literal) strings:
-- {"dict", "file", "ftp", "ftps", "gopher", "http", "https", "imap", "imaps",
--  "ldap", "pop3", "pop3s", "rtsp", "smb", "smbs", "smtp", "smtps", "telnet",
--  "tftp"} (and possibly others)
--
-- libraries built with SSH support will have a non-null string returned in
-- res[LIBCURL_VERSION_LIBSSH_VERSION], but tend to be three times bigger.
--
    if xcurl_version_info=NULL then
        if libcurl=NULL then curl_init() end if
        xcurl_version_info = define_c_func(libcurl,"+curl_version_info",{C_INT},C_PTR)
--DEV:
--      idCVID = define_struct(tCVID)
    end if
    if xcurl_version_info=-1 then return {} end if
    atom pCVID = c_func(xcurl_version_info, {CURLVERSION_NOW})
--  if bRawPtr then
--      return pCVID 
--  end if
    integer age = get_struct_field(idCVID,pCVID,"age")
    atom pVersion = get_struct_field(idCVID,pCVID,"version")
    string libcurl_version = peek_string(pVersion)
    int version_int = get_struct_field(idCVID,pCVID,"version_num")
    sequence version_num = {and_bits(version_int,#00FF0000)/#10000,
                            and_bits(version_int,#0000FF00)/#100,
                            and_bits(version_int,#000000FF)/#1}
    atom pHost = get_struct_field(idCVID,pCVID,"host")
    string host = peek_string(pHost)
    atom xFeatures = get_struct_field(idCVID,pCVID,"features")
    sequence features = split(decode_flags(FeatureSet,xFeatures),'+')
    atom pSSL_version = get_struct_field(idCVID,pCVID,"ssl_version")
    string ssl_version = peek_stringn(pSSL_version)
    atom plibz_version = get_struct_field(idCVID,pCVID,"libz_version")
    string libz_version = peek_stringn(plibz_version)
    atom pProtocols = get_struct_field(idCVID,pCVID,"protocols")
    sequence protocols = peek_strings(pProtocols)
    sequence res = {age,
                    libcurl_version,
                    version_num,
                    host,
                    features,
                    ssl_version,
                    libz_version,
                    protocols}
    if age>=CURLVERSION_SECOND then
        atom pAres = get_struct_field(idCVID,pCVID,"ares")
        string ares = peek_stringn(pAres)
        res = append(res,ares)
        integer ares_num = get_struct_field(idCVID,pCVID,"ares_num")
        res = append(res,ares_num)
        if age>=CURLVERSION_THIRD then
            atom plibidn = get_struct_field(idCVID,pCVID,"libidn")
            string libidn = peek_stringn(plibidn)
            res = append(res,libidn)
            if age>=CURLVERSION_FOURTH then
                integer iconv_ver_num = get_struct_field(idCVID,pCVID,"iconv_ver_num")
                res = append(res,iconv_ver_num)
                atom plibssh_version = get_struct_field(idCVID,pCVID,"libssh_version")
                string libssh_version = peek_stringn(plibssh_version)
                res = append(res,libssh_version)
                if age>=CURLVERSION_FIFTH then
                    integer brotli_ver_int = get_struct_field(idCVID,pCVID,"brotli_ver_num")
                    object brotli_ver_num
                    if brotli_ver_int=0 then
                        brotli_ver_num = 0
                    else
                        brotli_ver_num = {and_bits(brotli_ver_int,#FF000000)/#1000000,
                                          and_bits(brotli_ver_int,#00FFF000)/#1000,
                                          and_bits(brotli_ver_int,#00000FFF)/#1}
                    end if
                    res = append(res,brotli_ver_num)
                    atom pbrotli_version = get_struct_field(idCVID,pCVID,"brotli_version")
                    string brotli_version = peek_stringn(pbrotli_version)
                    res = append(res,brotli_version)
                end if
            end if
        end if
    end if
    return res
end function

--------------------------------------------------------------------------------

bool global_init = false

global constant CURL_GLOBAL_SSL       = 1,  -- (deprecated in 7.57.0)
                CURL_GLOBAL_WIN32     = 2,
                CURL_GLOBAL_ALL       = or_bits(CURL_GLOBAL_SSL, CURL_GLOBAL_WIN32),
                CURL_GLOBAL_NOTHING   = 0,
                CURL_GLOBAL_DEFAULT   = CURL_GLOBAL_ALL,
                CURL_GLOBAL_ACK_EINTR = 4

atom xcurl_global_init = NULL
global procedure curl_global_init(integer flags=CURL_GLOBAL_DEFAULT)
    if xcurl_global_init=NULL then
        if libcurl=NULL then curl_init() end if
        xcurl_global_init = link_c_func(libcurl, "curl_global_init", {C_LONG}, C_INT)
    end if
    global_init = true
    integer error_code = c_func(xcurl_global_init, {flags})
    if error_code!=CURLE_OK then ?9/0 end if
end procedure

--------------------------------------------------------------------------------

atom xcurl_global_cleanup = NULL
global procedure curl_global_cleanup()
    if xcurl_global_cleanup=NULL then
        if libcurl=NULL then curl_init() end if
        xcurl_global_cleanup = link_c_proc(libcurl, "curl_global_cleanup", {})
    end if
    global_init = false
    c_proc(xcurl_global_cleanup, {})
end procedure

--------------------------------------------------------------------------------

-- maybe...
--global type CURLhandle(atom h)
--  return h!=NULL
--end type

atom xcurl_easy_init = NULL
global function curl_easy_init()
    if xcurl_easy_init=NULL then
        if libcurl=NULL then curl_init() end if
        xcurl_easy_init = link_c_func(libcurl, "curl_easy_init", {}, C_PTR)
    end if
--  CURLhandle curl = c_func(xcurl_easy_init, {})
    atom curl = c_func(xcurl_easy_init, {})
    if curl=NULL then ?9/0 end if
    return curl
end function
-----------------------------
-- curl_easy_init
-----------------------------
--integer my_curl_easy_init
--my_curl_easy_init = link_func(LibCurlLib, "curl_easy_init", {}, C_PTR)
--global function curl_easy_init()
--   a2dp = allocate(8)
--   return c_func(my_curl_easy_init,{})
--end function

--------------------------------------------------------------------------------

-- CURL_EXTERN void curl_easy_cleanup(CURL *curl);
atom xcurl_easy_cleanup = NULL
global procedure curl_easy_cleanup(atom curl)
    if xcurl_easy_cleanup=NULL then
        xcurl_easy_cleanup = link_c_proc(libcurl, "curl_easy_cleanup",  {C_PTR})
    end if
    c_proc(xcurl_easy_cleanup, {curl})
end procedure
-----------------------------
-- curl_easy_cleanup
-----------------------------
--integer my_curl_easy_cleanup
--my_curl_easy_cleanup = link_proc(LibCurlLib, "curl_easy_cleanup", {C_PTR})
--global procedure curl_easy_cleanup(atom curl)
--
--   -- cleanup allocated strings
--   for i = 1 to length(string_tab) do
--    free(string_tab[i])
--   end for
--
--   free(a2dp)
---- shutdown curl
--   c_proc(my_curl_easy_cleanup,{curl})
--end procedure

--------------------------------------------------------------------------------

-- NAME curl_easy_duphandle()
--
-- DESCRIPTION
--
-- Creates a new curl session handle with the same options set for the handle
-- passed in. Duplicating a handle could only be a matter of cloning data and
-- options, internal state info and things like persistent connections cannot
-- be transferred. It is useful in multithreaded applications when you can run
-- curl_easy_duphandle() for each new thread to avoid a series of identical
-- curl_easy_setopt() invokes in every thread.
--/
-- CURL_EXTERN CURL* curl_easy_duphandle(CURL *curl);
atom xcurl_easy_duphandle = NULL
global function curl_easy_duphandle(atom curl)
    if xcurl_easy_duphandle=NULL then
        xcurl_easy_duphandle = link_c_func(libcurl, "curl_easy_duphandle", {C_PTR}, C_PTR)
    end if
  return c_func(xcurl_easy_duphandle, {curl})
end function

--------------------------------------------------------------------------------

atom xcurl_easy_reset = NULL
global procedure curl_easy_reset(atom curl)
    if xcurl_easy_reset=NULL then
        xcurl_easy_reset = link_c_proc(libcurl, "curl_easy_reset", {C_PTR})
    end if
  c_proc(xcurl_easy_reset, {curl})
end procedure

--------------------------------------------------------------------------------
--
-- options to be used with curl_easy_setopt().
-- PL: uncomment as needed/tested/documented
--
global constant
  CURLOPT_WRITEDATA                 = 10001,    -- This is the FILE * or void * the regular output should be written to.
--/*
  CURLOPT_FILE                      = 10001,    -- name changed in 7.9.7
--*/
  CURLOPT_URL                       = 10002,    -- The full URL to get/put
--/*
  CURLOPT_PORT                      =     3,    -- Port number to connect to, if other than default.
--*/
  CURLOPT_PROXY                     = 10004,    -- Name of proxy to use.
  CURLOPT_USERPWD                   = 10005,    -- "user:password;options" to use when fetching.
--/*
  CURLOPT_PROXYUSERPWD              = 10006,    -- "user:password" to use with proxy.
  CURLOPT_RANGE                     = 10007,    -- Range to get, specified as an ASCII string.
  -- not used                   (ie [1000]8,)
--*/
  CURLOPT_READDATA                  = 10009,    -- Specified file stream to upload from (use as input):
  CURLOPT_ERRORBUFFER               = 10010,    -- Buffer to receive error messages in, must be at least CURL_ERROR_SIZE
                                                -- bytes big. If this is not used, error messages go to stderr instead:
  CURLOPT_WRITEFUNCTION             = 20011,    -- Function that will be called to store the output (instead of fwrite). The
                                                -- parameters will use fwrite() syntax, make sure to follow them.
  CURLOPT_READFUNCTION              = 20012,    -- Function that will be called to read the input (instead of fread). The
                                                -- parameters will use fread() syntax, make sure to follow them.
--/*
  CURLOPT_TIMEOUT                   =    13,    -- Time-out the read operation after this amount of seconds
  CURLOPT_INFILESIZE                =    14,    -- If the CURLOPT_INFILE is used, this can be used to inform libcurl about
                                                -- how large the file being sent really is. That allows better error
                                                -- checking and better verifies that the upload was successful. -1 means
                                                -- unknown size.
                                                --
                                                -- For large file support, there is also a _LARGE version of the key
                                                -- which takes an off_t type, allowing platforms with larger off_t
                                                -- sizes to handle larger files.  See below for INFILESIZE_LARGE. 
--*/
  CURLOPT_POSTFIELDS                = 10015,    -- POST static input fields.
--/*
  CURLOPT_REFERER                   = 10016,    -- Set the referrer page (needed by some CGIs)
  CURLOPT_FTPPORT                   = 10017,    -- Set the FTP PORT string (interface name, named or numerical IP address)
                                                -- Use i.e '-' to use default address.
--*/
  CURLOPT_USERAGENT                 = 10018,    -- Set the User-Agent string (examined by some CGIs)
--/*
                                                -- If the download receives less than "low speed limit" bytes/second
                                                -- during "low speed time" seconds, the operations is aborted.
                                                -- You could i.e if you have a pretty high speed connection, abort if
                                                -- it is less than 2000 bytes/sec during 20 seconds.
  CURLOPT_LOW_SPEED_LIMIT           =    19,    -- Set the "low speed limit"
  CURLOPT_LOW_SPEED_TIME            =    20,    -- Set the "low speed time"
  CURLOPT_RESUME_FROM               =    21,    -- Set the continuation offset.
                                                -- Note there is also a _LARGE version of this key which uses
                                                -- off_t types, allowing for large file offsets on platforms which
                                                -- use larger-than-32-bit off_t's.  Look below for RESUME_FROM_LARGE.
  CURLOPT_COOKIE                    = 10022,    -- Set cookie in request:
--*/
  CURLOPT_HTTPHEADER                = 10023,    -- This points to a linked list of headers, struct curl_slist kind. This
                                                -- list is also used for RTSP (in spite of its name)
--/*
  CURLOPT_HTTPPOST                  = 10024,    -- This points to a linked list of post entries, struct curl_httppost
  CURLOPT_SSLCERT                   = 10025,    -- name of the file keeping your private SSL-certificate
  CURLOPT_KEYPASSWD                 = 10026,    -- password for the SSL or SSH private key
  CURLOPT_CRLF                      =    27,    -- send TYPE parameter?
  CURLOPT_QUOTE                     = 10028,    -- send linked-list of QUOTE commands
  CURLOPT_HEADERDATA                = 10029,    -- send FILE * or void * to store headers to, if you use a callback it
                                                    -- is simply passed to the callback unmodified
--*/
  CURLOPT_COOKIEFILE                = 10031,    -- point to a file to read the initial cookies from, also enables
                                                -- "cookie awareness"
--/*
  CURLOPT_SSLVERSION                =    32,    -- What version to specifically try to use.
                                                -- See CURL_SSLVERSION defines below.
  CURLOPT_TIMECONDITION             =    33,    -- What kind of HTTP time condition to use, see defines
  CURLOPT_TIMEVALUE                 =    34,    -- Time to use with the above condition. Specified in number of seconds
                                                -- since 1 Jan 1970
  -- 35 = OBSOLETE
--*/
  CURLOPT_CUSTOMREQUEST             = 10036,    -- Custom request, for customizing the get command like
                                                -- HTTP: DELETE, TRACE and others
                                                -- FTP: to use a different list command
--/*
  CURLOPT_STDERR                    = 10037,    -- FILE handle to use instead of stderr
  -- 38 is not used
  CURLOPT_POSTQUOTE                 = 10039,    -- send linked-list of post-transfer QUOTE commands
--CURLOPT_OBSOLETE40                = 10040,    -- OBSOLETE, do not use!
--*/
  CURLOPT_VERBOSE                   =    41,    -- talk a lot
  CURLOPT_HEADER                    =    42,    -- throw the header out too
  CURLOPT_NOPROGRESS                =    43,    -- shut off the progress meter
--/*
  CURLOPT_NOBODY                    =    44,    -- use HEAD to get http document
--*/
  CURLOPT_FAILONERROR               =    45,    -- no output on http error codes >= 400
  CURLOPT_UPLOAD                    =    46,    -- this is an upload
  CURLOPT_POST                      =    47,    -- HTTP POST method
--/*
  CURLOPT_DIRLISTONLY               =    48,    -- bare names when listing directories
  CURLOPT_APPEND                    =    50,    -- Append instead of overwrite on upload!
  CURLOPT_NETRC                     =    51,    -- Specify whether to read the user+password from the .netrc or the URL.
                                                -- This must be one of the CURL_NETRC_* enums below.
--*/
  CURLOPT_FOLLOWLOCATION            =    52,    -- use Location: Luke!
--/*
  CURLOPT_TRANSFERTEXT              =    53,    -- transfer data in text/ASCII format
  CURLOPT_PUT                       =    54,    -- HTTP PUT
  -- 55 = OBSOLETE
--*/
  CURLOPT_PROGRESSFUNCTION          = 20056,    -- DEPRECATED
                                                -- Function that will be called instead of the internal progress display
                                                -- global function. This global function should be defined as the curl_progress_callback
                                                -- prototype defines.
  CURLOPT_PROGRESSDATA              = 10057,    -- Data passed to the CURLOPT_PROGRESSFUNCTION and CURLOPT_XFERINFOFUNCTION callbacks
--/*
  CURLOPT_AUTOREFERER               =    58,    -- We want the referrer field set automatically when following locations
  CURLOPT_PROXYPORT                 =    59,    -- Port of the proxy, can be set in the proxy string as well with:
                                                -- "[host]:[port]"
--*/
  CURLOPT_POSTFIELDSIZE             =    60,    -- size of the POST input data, if strlen() is not good to use
--/*
  CURLOPT_HTTPPROXYTUNNEL           =    61,    -- tunnel non-http operations through a HTTP proxy
  CURLOPT_INTERFACE                 = 10062,    -- Set the interface string to use as outgoing network interface
  CURLOPT_KRBLEVEL                  = 10063,    -- Set the krb4/5 security level, this also enables krb4/5 awareness.  This
                                                -- is a string, 'clear', 'safe', 'confidential' or 'private'.  If the string
                                                -- is set but doesn't match one of these, 'private' will be used. 
--*/
  CURLOPT_SSL_VERIFYPEER            =    64,    -- Set if we should verify the peer in ssl handshake, set 1 to verify.
  CURLOPT_CAINFO                    = 10065,    -- The CApath or CAfile used to validate the peer certificate
                                                -- this option is used only if SSL_VERIFYPEER is true
--/*
  -- 66 = OBSOLETE
  -- 67 = OBSOLETE
  CURLOPT_MAXREDIRS                 =    68,    -- Maximum number of http redirects to follow
  CURLOPT_FILETIME                  =    69,    -- Pass a long set to 1 to get the date of the requested document (if
                                                -- possible)! Pass a zero to shut it off.
  CURLOPT_TELNETOPTIONS             = 10070,    -- This points to a linked list of telnet options
  CURLOPT_MAXCONNECTS               =    71,    -- Max amount of cached alive connections
  CURLOPT_OBSOLETE72                =    72,    -- OBSOLETE, do not use!
  -- 73 = OBSOLETE
  CURLOPT_FRESH_CONNECT             =    74,    -- Set to explicitly use a new connection for the upcoming transfer.
                                                -- Do not use this unless you're absolutely sure of this, as it makes the
                                                -- operation slower and is less friendly for the network.
  CURLOPT_FORBID_REUSE              =    75,    -- Set to explicitly forbid the upcoming transfer's connection to be re-used
                                                -- when done. Do not use this unless you're absolutely sure of this, as it
                                                -- makes the operation slower and is less friendly for the network.
  CURLOPT_RANDOM_FILE               = 10076,    -- Set to a file name that contains random data for libcurl to use to
                                                -- seed the random engine when doing SSL connects.
  CURLOPT_EGDSOCKET                 = 10077,    -- Set to the Entropy Gathering Daemon socket pathname
  CURLOPT_CONNECTTIMEOUT            =    78,    -- Time-out connect operations after this amount of seconds, if connects are
                                                -- OK within this time, then fine... This only aborts the connect phase.
--*/
  CURLOPT_HEADERFUNCTION            = 20079,    -- Function that will be called to store headers (instead of fwrite). The
                                                -- parameters will use fwrite() syntax, make sure to follow them.
  CURLOPT_HTTPGET                   =    80,    -- Set this to force the HTTP request to get back to GET. Only really usable
                                                -- if POST, PUT or a custom request have been used first.
  CURLOPT_SSL_VERIFYHOST            =    81,    -- Set if we should verify the Common name from the peer certificate in ssl
                                                -- handshake, set 1 to check existence, 2 to ensure that it matches the
                                                -- provided hostname.
  CURLOPT_COOKIEJAR                 = 10082,    -- Specify which file name to write all known cookies in after completed
                                                -- operation. Set file name to "-" (dash) to make it go to stdout.
--/*
  CURLOPT_SSL_CIPHER_LIST           = 10083,    -- Specify which SSL ciphers to use
  CURLOPT_HTTP_VERSION              =    84,    -- Specify which HTTP version to use! This must be set to one of the
                                                -- CURL_HTTP_VERSION* enums set below.
  CURLOPT_FTP_USE_EPSV              =    85,    -- Specifically switch on or off the FTP engine's use of the EPSV command. By
                                                -- default, that one will always be attempted before the more traditional
                                                -- PASV command.
  CURLOPT_SSLCERTTYPE               = 10086,    -- type of the file keeping your SSL-certificate ("DER", "PEM", "ENG")
  CURLOPT_SSLKEY                    = 10087,    -- name of the file keeping your private SSL-key
  CURLOPT_SSLKEYTYPE                = 10088,    -- type of the file keeping your private SSL-key ("DER", "PEM", "ENG")
  CURLOPT_SSLENGINE                 = 10089,    -- crypto engine for the SSL-sub system
  CURLOPT_SSLENGINE_DEFAULT         =    90,    -- set the crypto engine for the SSL-sub system as default
                                                -- the param has no meaning...
  CURLOPT_DNS_USE_GLOBAL_CACHE      =    91,    -- Non-zero value means to use the global dns cache
                                                -- DEPRECATED, do not use!
  CURLOPT_DNS_CACHE_TIMEOUT         =    92,    -- DNS cache timeout
  CURLOPT_PREQUOTE                  = 10093,    -- send linked-list of pre-transfer QUOTE commands
--*/
  CURLOPT_DEBUGFUNCTION             = 20094,    -- set the debug global function
--/*
  CURLOPT_DEBUGDATA                 = 10095,    -- set the data for the debug global function
  CURLOPT_COOKIESESSION             =    96,    -- mark this as start of a cookie session
  CURLOPT_CAPATH                    = 10097,    -- The CApath directory used to validate the peer certificate
                                                -- this option is used only if SSL_VERIFYPEER is true
  CURLOPT_BUFFERSIZE                =    98,    -- Instruct libcurl to use a smaller receive buffer
  CURLOPT_NOSIGNAL                  =    99,    -- Instruct libcurl to not use any signal/alarm handlers, even when using
                                                -- timeouts. This option is useful for multi-threaded applications.
                                                -- See libcurl-the-guide for more background information.
--*/
  CURLOPT_SHARE                     = 10100,    -- Provide a CURLShare for mutexing non-ts data
  CURLOPT_PROXYTYPE                 =   101,    -- indicates type of proxy. accepted values are CURLPROXY_HTTP (default,
                                                -- CURLPROXY_SOCKS4, CURLPROXY_SOCKS4A and CURLPROXY_SOCKS5.
--/*
  CURLOPT_ACCEPT_ENCODING           = 10102,    -- Set the Accept-Encoding string. Use this to tell a server you would like
                                                -- the response to be compressed. Before 7.21.6, this was known as
                                                -- CURLOPT_ENCODING
--*/
  CURLOPT_PRIVATE                   = 10103,    -- Set pointer to private data
--/*
  CURLOPT_HTTP200ALIASES            = 10104,    -- Set aliases for HTTP 200 in the HTTP Response header
  CURLOPT_UNRESTRICTED_AUTH         =   105,    -- Continue to send authentication (user+password) when following locations,
                                                -- even when hostname changed. This can potentially send off the name
                                                -- and password to whatever host the server decides.
  CURLOPT_FTP_USE_EPRT              =   106,    -- Specifically switch on or off the FTP engine's use of the EPRT command (
                                                -- it also disables the LPRT attempt). By default, those ones will always be
                                                -- attempted before the good old traditional PORT command.
--*/
  CURLOPT_HTTPAUTH                  =   107,    -- Set this to a bitmask value to enable the particular authentications
                                                -- methods you like. Use this in combination with CURLOPT_USERPWD.
                                                -- Note that setting multiple bits may cause extra network round-trips.
--/*
  CURLOPT_SSL_CTX_FUNCTION          = 20108,    -- Set the ssl context callback global function, currently only for OpenSSL ssl_ctx
                                                -- in second argument. The global function must be matching the
                                                -- curl_ssl_ctx_callback proto.
  CURLOPT_SSL_CTX_DATA              = 10109,    -- Set the userdata for the ssl context callback global function's third
                                                -- argument
  CURLOPT_FTP_CREATE_MISSING_DIRS   =   110,    -- FTP Option that causes missing dirs to be created on the remote server.
                                                -- In 7.19.4 we introduced the convenience enums for this option using the
                                                -- CURLFTP_CREATE_DIR prefix.
  CURLOPT_PROXYAUTH                 =   111,    -- Set this to a bitmask value to enable the particular authentications
                                                -- methods you like. Use this in combination with CURLOPT_PROXYUSERPWD.
                                                -- Note that setting multiple bits may cause extra network round-trips.
  CURLOPT_FTP_RESPONSE_TIMEOUT      =   112,    -- FTP option that changes the timeout, in seconds, associated with
                                                -- getting a response.  This is different from transfer timeout time and
                                                -- essentially places a demand on the FTP server to acknowledge commands
                                                -- in a timely manner.
  CURLOPT_IPRESOLVE                 =   113,    -- Set this option to one of the CURL_IPRESOLVE_* defines (see below) to
                                                -- tell libcurl to resolve names to those IP versions only. This only has
                                                -- affect on systems with support for more than one, i.e IPv4 _and_ IPv6.
  CURLOPT_MAXFILESIZE               =   114,    -- Set this option to limit the size of a file that will be downloaded from
                                                -- an HTTP or FTP server.
                                                -- Note there is also _LARGE version which adds large file support for
                                                -- platforms which have larger off_t sizes.  See MAXFILESIZE_LARGE below.
  CURLOPT_INFILESIZE_LARGE          = 30115,    -- See the comment for INFILESIZE above, but in short, specifies
                                                -- the size of the file being uploaded.  -1 means unknown.
  CURLOPT_RESUME_FROM_LARGE         = 30116,    -- Sets the continuation offset.    There is also a LONG version of this;
                                                -- look above for RESUME_FROM.
  CURLOPT_MAXFILESIZE_LARGE         = 30117,    -- Sets the maximum size of data that will be downloaded from
                                                -- an HTTP or FTP server.  See MAXFILESIZE above for the LONG version.
  CURLOPT_NETRC_FILE                = 10118,    -- Set this option to the file name of your .netrc file you want libcurl
                                                -- to parse (using the CURLOPT_NETRC option). If not set, libcurl will do
                                                -- a poor attempt to find the user's home directory and check for a .netrc
                                                -- file in there.
--*/
  CURLOPT_USE_SSL                   =   119,    -- Enable SSL/TLS for FTP, pick one of:
                                                -- CURLUSESSL_TRY     - try using SSL, proceed anyway otherwise
                                                -- CURLUSESSL_CONTROL - SSL for the control connection or fail
                                                -- CURLUSESSL_ALL     - SSL for all communication or fail
  CURLOPT_POSTFIELDSIZE_LARGE       = 30120,    -- The _LARGE version of the standard POSTFIELDSIZE option
--/*
  CURLOPT_TCP_NODELAY               =   121,    -- Enable/disable the TCP Nagle algorithm
  -- 122 OBSOLETE, used in 7.12.3. Gone in 7.13.0
  -- 123 OBSOLETE. Gone in 7.16.0
  -- 124 OBSOLETE, used in 7.12.3. Gone in 7.13.0
  -- 125 OBSOLETE, used in 7.12.3. Gone in 7.13.0
  -- 126 OBSOLETE, used in 7.12.3. Gone in 7.13.0
  -- 127 OBSOLETE. Gone in 7.16.0
  -- 128 OBSOLETE. Gone in 7.16.0
  CURLOPT_FTPSSLAUTH                =   129,    -- When FTP over SSL/TLS is selected (with CURLOPT_USE_SSL, this option
                                                -- can be used to change libcurl's default action which is to first try
                                                -- "AUTH SSL" and then "AUTH TLS" in this order, and proceed when a OK
                                                -- response has been received.

                                                -- Available parameters are:
                                                -- CURLFTPAUTH_DEFAULT - let libcurl decide
                                                -- CURLFTPAUTH_SSL     - try "AUTH SSL" first, then TLS
                                                -- CURLFTPAUTH_TLS     - try "AUTH TLS" first, then SSL
  CURLOPT_IOCTLFUNCTION             = 20130,
  CURLOPT_IOCTLDATA                 = 10131,
  -- 132 OBSOLETE. Gone in 7.16.0
  -- 133 OBSOLETE. Gone in 7.16.0
  CURLOPT_FTP_ACCOUNT               = 10134,    -- zero terminated string for pass on to the FTP server when asked for
                                                -- "account" info
--*/
  CURLOPT_COOKIELIST                = 10135,    -- feed cookie into cookie engine
--/*
  CURLOPT_IGNORE_CONTENT_LENGTH     =   136,    -- ignore Content-Length
  CURLOPT_FTP_SKIP_PASV_IP          =   137,    -- Set to non-zero to skip the IP address received in a 227 PASV FTP server
                                                -- response. Typically used for FTP-SSL purposes but is not restricted to
                                                -- that. libcurl will then instead use the same IP address it used for the
                                                -- control connection.
  CURLOPT_FTP_FILEMETHOD            =   138,    -- Select "file method" to use when doing FTP, see the curl_ftpmethod
                                                -- above.
  CURLOPT_LOCALPORT                 =   139,    -- Local port number to bind the socket to
  CURLOPT_LOCALPORTRANGE            =   140,    -- Number of ports to try, including the first one set with LOCALPORT.
                                                -- Thus, setting it to 1 will make no additional attempts but the first.
--*/
  CURLOPT_CONNECT_ONLY              =   141,    -- no transfer, set up connection and let application use the socket by
                                                -- extracting it with CURLINFO_LASTSOCKET
--/*
  CURLOPT_CONV_FROM_NETWORK_FUNCTION = 20142,   -- Function that will be called to convert from the
                                                -- network encoding (instead of using the iconv calls in libcurl)
  CURLOPT_CONV_TO_NETWORK_FUNCTION  = 20143,    -- Function that will be called to convert to the
                                                -- network encoding (instead of using the iconv calls in libcurl)
  CURLOPT_CONV_FROM_UTF8_FUNCTION   = 20144,    -- Function that will be called to convert from UTF8
                                                -- (instead of using the iconv calls in libcurl)
                                                -- Note that this is used only for SSL certificate processing
                                                -- if the connection proceeds too quickly then need to slow it down
                                                -- limit-rate: maximum number of bytes per second to send or receive
  CURLOPT_MAX_SEND_SPEED_LARGE      = 30145,
  CURLOPT_MAX_RECV_SPEED_LARGE      = 30146,
  CURLOPT_FTP_ALTERNATIVE_TO_USER   = 10147,    -- Pointer to command string to send if USER/PASS fails.
  CURLOPT_SOCKOPTFUNCTION           = 20148,    -- callback global function for setting socket options
  CURLOPT_SOCKOPTDATA               = 10149,
  CURLOPT_SSL_SESSIONID_CACHE       =   150,    -- set to 0 to disable session ID re-use for this transfer, default is
                                                -- enabled (== 1)
  CURLOPT_SSH_AUTH_TYPES            =   151,    -- allowed SSH authentication methods
  CURLOPT_SSH_PUBLIC_KEYFILE        = 10152,    -- Used by scp/sftp to do global/private key authentication
  CURLOPT_SSH_PRIVATE_KEYFILE       = 10153,
  CURLOPT_FTP_SSL_CCC               =   154,    -- Send CCC (Clear Command Channel) after authentication
  CURLOPT_TIMEOUT_MS                =   155,    -- Same as TIMEOUT and CONNECTTIMEOUT, but with ms resolution
  CURLOPT_CONNECTTIMEOUT_MS         =   156,
  CURLOPT_HTTP_TRANSFER_DECODING    =   157,    -- set to zero to disable the libcurl's decoding and thus pass the raw body
                                                -- data to the application even when it is encoded/compressed
  CURLOPT_HTTP_CONTENT_DECODING     =   158,
  CURLOPT_NEW_FILE_PERMS            =   159,    -- Permission used when creating new files and directories on the remote
                                                -- server for protocols that support it, SFTP/SCP/FILE
  CURLOPT_NEW_DIRECTORY_PERMS       =   160,
  CURLOPT_POSTREDIR                 =   161,    -- Set the behaviour of POST when redirecting. Values must be set to one
                                                -- of CURL_REDIR* defines below. This used to be called CURLOPT_POST301
  CURLOPT_SSH_HOST_PUBLIC_KEY_MD5   = 10162,    -- used by scp/sftp to verify the host's global key
  CURLOPT_OPENSOCKETFUNCTION        = 20163,    -- Callback global function for opening socket (instead of socket(2)). Optionally,
                                                -- callback is able change the address or refuse to connect returning
                                                -- CURL_SOCKET_BAD.  The callback should have type
                                                -- curl_opensocket_callback
  CURLOPT_OPENSOCKETDATA            = 10164,
  CURLOPT_COPYPOSTFIELDS            = 10165,    -- POST volatile input fields.
  CURLOPT_PROXY_TRANSFER_MODE       =   166,    -- set transfer mode (;type=<a|i>) when doing FTP via an HTTP proxy
  CURLOPT_SEEKFUNCTION              = 20167,    -- Callback global function for seeking in the input stream
  CURLOPT_SEEKDATA                  = 10168,
  CURLOPT_CRLFILE                   = 10169,    -- CRL file
  CURLOPT_ISSUERCERT                = 10170,    -- Issuer certificate
  CURLOPT_ADDRESS_SCOPE             =   171,    -- (IPv6) Address scope
--*/
  CURLOPT_CERTINFO                  =   172,    -- Collect certificate chain info and allow it to be retrievable with
                                                -- CURLINFO_CERTINFO after the transfer is complete.
  CURLOPT_USERNAME                  = 10173,    -- "name" and "pwd" to use when fetching.
  CURLOPT_PASSWORD                  = 10174,
--/*
  CURLOPT_PROXYUSERNAME             = 10175,    -- "name" and "pwd" to use with Proxy when fetching.
  CURLOPT_PROXYPASSWORD             = 10176,
  CURLOPT_NOPROXY                   = 10177,    -- Comma separated list of hostnames defining no-proxy zones. These should
                                                -- match both hostnames directly, and hostnames within a domain. For
                                                -- example, local.com will match local.com and www.local.com, but NOT
                                                -- notlocal.com or www.notlocal.com. For compatibility with other
                                                -- implementations of this, .local.com will be considered to be the same as
                                                -- local.com. A single * is the only valid wildcard, and effectively
                                                -- disables the use of proxy.
  CURLOPT_TFTP_BLKSIZE              =   178,    -- block size for TFTP transfers
--CURLOPT_SOCKS5_GSSAPI_SERVICE     = 10179,    -- Socks Service
                                                -- DEPRECATED, do not use!
  CURLOPT_SOCKS5_GSSAPI_NEC         =   180,    -- Socks Service
  CURLOPT_PROTOCOLS                 =   181,    -- set the bitmask for the protocols that are allowed to be used for the
                                                -- transfer, which thus helps the app which takes URLs from users or other
                                                -- external inputs and want to restrict what protocol(s) to deal
                                                -- with. Defaults to CURLPROTO_ALL.
  CURLOPT_REDIR_PROTOCOLS           =   182,    -- set the bitmask for the protocols that libcurl is allowed to follow to,
                                                -- as a subset of the CURLOPT_PROTOCOLS ones. That means the protocol needs
                                                -- to be set in both bitmasks to be allowed to get redirected to. Defaults
                                                -- to all protocols except FILE and SCP.
  CURLOPT_SSH_KNOWNHOSTS            = 10183,    -- set the SSH knownhost file name to use
  CURLOPT_SSH_KEYFUNCTION           = 20184,    -- set the SSH host key callback, must point to a curl_sshkeycallback
                                                -- global function
  CURLOPT_SSH_KEYDATA               = 10185,    -- set the SSH host key callback custom pointer
--*/
  CURLOPT_MAIL_FROM                 = 10186,    -- set the SMTP mail originator
  CURLOPT_MAIL_RCPT                 = 10187,    -- set the list of SMTP mail receiver(s)
--/*
  CURLOPT_FTP_USE_PRET              =   188,    -- FTP: send PRET before PASV
  CURLOPT_RTSP_REQUEST              =   189,    -- RTSP request method (OPTIONS, SETUP, PLAY, etc...)
  CURLOPT_RTSP_SESSION_ID           = 10190,    -- The RTSP session identifier
  CURLOPT_RTSP_STREAM_URI           = 10191,    -- The RTSP stream URI
  CURLOPT_RTSP_TRANSPORT            = 10192,    -- The Transport: header to use in RTSP requests
  CURLOPT_RTSP_CLIENT_CSEQ          =   193,    -- Manually initialize the client RTSP CSeq for this handle
  CURLOPT_RTSP_SERVER_CSEQ          =   194,    -- Manually initialize the server RTSP CSeq for this handle
  CURLOPT_INTERLEAVEDATA            = 10195,    -- The stream to pass to INTERLEAVEFUNCTION.
  CURLOPT_INTERLEAVEFUNCTION        = 20196,    -- Let the application define a custom write method for RTP data
  CURLOPT_WILDCARDMATCH             =   197,    -- Turn on wildcard matching
  CURLOPT_CHUNK_BGN_FUNCTION        = 20198,    -- Directory matching callback called before downloading of an
                                                -- individual file (chunk) started
  CURLOPT_CHUNK_END_FUNCTION        = 20199,    -- Directory matching callback called after the file (chunk)
                                                -- was downloaded, or skipped
  CURLOPT_FNMATCH_FUNCTION          = 20200,    -- Change match (fnmatch-like) callback for wildcard matching
  CURLOPT_CHUNK_DATA                = 10201,    -- Let the application define custom chunk data pointer
  CURLOPT_FNMATCH_DATA              = 10202,    -- FNMATCH_FUNCTION user pointer
--*/
  CURLOPT_RESOLVE                   = 10203,    -- send linked-list of name:port:address sets
--/*
  CURLOPT_TLSAUTH_USERNAME          = 10204,    -- Set a username for authenticated TLS
  CURLOPT_TLSAUTH_PASSWORD          = 10205,    -- Set a password for authenticated TLS
  CURLOPT_TLSAUTH_TYPE              = 10206,    -- Set authentication type for authenticated TLS
  CURLOPT_TRANSFER_ENCODING         =   207,    -- Set to 1 to enable the "TE:" header in HTTP requests to ask for
                                                -- compressed transfer-encoded responses. Set to 0 to disable the use of TE:
                                                -- in outgoing requests. The current default is 0, but it might change in a
                                                -- future libcurl release.
                                                -- libcurl will ask for the compressed methods it knows of, and if that
                                                -- isn't any, it will not ask for transfer-encoding at all even if this
                                                -- option is set to 1.
  CURLOPT_CLOSESOCKETFUNCTION       = 20208,    -- Callback global function for closing socket (instead of close(2)). The callback
                                                -- should have type curl_closesocket_callback
  CURLOPT_CLOSESOCKETDATA           = 10209,
  CURLOPT_GSSAPI_DELEGATION         =   210,    -- allow GSSAPI credential delegation
  CURLOPT_DNS_SERVERS               = 10211,    -- Set the name servers to use for DNS resolution
  CURLOPT_ACCEPTTIMEOUT_MS          =   212,    -- Time-out accept operations (currently for FTP only) after this amount
                                                -- of miliseconds.
  CURLOPT_TCP_KEEPALIVE             =   213,    -- Set TCP keepalive
  CURLOPT_TCP_KEEPIDLE              =   214,    -- non-universal keepalive knobs (Linux, AIX, HP-UX, more)
  CURLOPT_TCP_KEEPINTVL             =   215,
  CURLOPT_SSL_OPTIONS               =   216,    -- Enable/disable specific SSL features with a bitmask, see CURLSSLOPT_*
  CURLOPT_MAIL_AUTH                 = 10217,    -- Set the SMTP auth originator
  CURLOPT_SASL_IR                   =   218,    -- Enable/disable SASL initial response
--*/
  CURLOPT_XFERINFOFUNCTION          = 20219,    -- Function that will be called instead of the internal progress display
                                                -- global function. This global function should be defined as the curl_xferinfo_callback
                                                -- prototype defines. (Deprecates CURLOPT_PROGRESSFUNCTION)
--/*
  CURLOPT_XOAUTH2_BEARER            = 10220,    -- The XOAUTH2 bearer token
  CURLOPT_DNS_INTERFACE             = 10221,    -- Set the interface string to use as outgoing network
                                                -- interface for DNS requests.
                                                -- Only supported by the c-ares DNS backend
  CURLOPT_DNS_LOCAL_IP4             = 10222,    -- Set the local IPv4 address to use for outgoing DNS requests.
                                                -- Only supported by the c-ares DNS backend
  CURLOPT_DNS_LOCAL_IP6             = 10223,    -- Set the local IPv4 address to use for outgoing DNS requests.
                                                -- Only supported by the c-ares DNS backend
  CURLOPT_LOGIN_OPTIONS             = 10224,    -- Set authentication options directly
  CURLOPT_SSL_ENABLE_NPN            =   225,    -- Enable/disable TLS NPN extension (http2 over ssl might fail without)
  CURLOPT_SSL_ENABLE_ALPN           =   226,    -- Enable/disable TLS ALPN extension (http2 over ssl might fail without)
  CURLOPT_EXPECT_100_TIMEOUT_MS     =   227,    -- Time to wait for a response to a HTTP request containing an
                                                -- Expect: 100-continue header before sending the data anyway.
  CURLOPT_PROXYHEADER               = 10228,    -- This points to a linked list of headers used for proxy requests only,
                                                -- struct curl_slist kind
  CURLOPT_HEADEROPT                 =   229,    -- Pass in a bitmask of "header options"
  CURLOPT_PINNEDPUBLICKEY           = 10230,    -- The global key in DER form used to validate the peer global key
                                                -- this option is used only if SSL_VERIFYPEER is true
  CURLOPT_UNIX_SOCKET_PATH          = 10231,    -- Path to Unix domain socket
  CURLOPT_SSL_VERIFYSTATUS          =   232,    -- Set if we should verify the certificate status.
  CURLOPT_SSL_FALSESTART            =   233,    -- Set if we should enable TLS false start.
  CURLOPT_PATH_AS_IS                =   234,    -- Do not squash dot-dot sequences
  CURLOPT_PROXY_SERVICE_NAME        = 10235,    -- Proxy Service Name
  CURLOPT_SERVICE_NAME              = 10236,    -- Service Name
  CURLOPT_PIPEWAIT                  =   237,    -- Wait/don't wait for pipe/mutex to clarify
  CURLOPT_DEFAULT_PROTOCOL          = 10238,    -- Set the protocol used when curl is given a URL without a protocol
  CURLOPT_STREAM_WEIGHT             =   239,    -- Set stream weight, 1 - 256 (default is 16)
  CURLOPT_STREAM_DEPENDS            = 10240,    -- Set stream dependency on another CURL handle
  CURLOPT_STREAM_DEPENDS_E          = 10241,    -- Set E-xclusive stream dependency on another CURL handle
  CURLOPT_TFTP_NO_OPTIONS           =   242,    -- Do not send any tftp option requests to the server
  CURLOPT_CONNECT_TO                = 10243,    -- Linked-list of host:port:connect-to-host:connect-to-port,
                                                -- overrides the URL's host:port (only for the network layer)
  CURLOPT_TCP_FASTOPEN              =   244     -- Set TCP Fast Open
--*/ $
constant
  CURLOPT_LASTENTRY                 =   244     -- the last used

-- parameter for the CURLOPT_USE_SSL option
global constant
  CURLUSESSL_NONE    = 0,  -- do not attempt to use SSL
  CURLUSESSL_TRY     = 1,  -- try using SSL, proceed anyway otherwise
  CURLUSESSL_CONTROL = 2,  -- SSL for the control connection or fail
  CURLUSESSL_ALL     = 3,  -- SSL for all communication or fail
  CURLUSESSL_LAST    = 4   -- not an option, never use


global type CURLoption(integer n)
    integer rem = remainder(n,10000)
    return ((rem>=1) and (rem<=CURLOPT_LASTENTRY))
end type

global constant CURL_SOCKET_BAD = -1

global constant  -- this global enum was added in 7.10
  CURLPROXY_HTTP            = 0,  -- added in 7.10, new in 7.19.4 default is
                                  --   to use CONNECT HTTP/1.1
  CURLPROXY_HTTP_1_0        = 1,  -- added in 7.19.4, force to use CONNECT
                                  --   HTTP/1.0 
  CURLPROXY_SOCKS4          = 4,  -- support added in 7.15.2, global enum existed
                                  --   already in 7.10
  CURLPROXY_SOCKS5          = 5,  -- added in 7.10
  CURLPROXY_SOCKS4A         = 6,  -- added in 7.18.0
  CURLPROXY_SOCKS5_HOSTNAME = 7   -- Use the SOCKS5 protocol but pass along
                                  --   the host name rather than the IP address.
                                  --   added in 7.18.0

--global type curl_proxytype(integer n)
--  return find(n, {CURLPROXY_HTTP, CURLPROXY_HTTP_1_0, CURLPROXY_SOCKS4,
--                 CURLPROXY_SOCKS5, CURLPROXY_SOCKS4A, CURLPROXY_SOCKS5_HOSTNAME})
--end type

atom xcurl_easy_setopt = NULL
global function curl_easy_setoptf(atom curl, CURLoption option, object param)
CURLcode res -- CURLE_OK..CURL_LAST-1
    if xcurl_easy_setopt=NULL then
        if libcurl=NULL then ?9/0 end if -- (the curl param cannot possibly be valid)
        xcurl_easy_setopt = link_c_func(libcurl, "curl_easy_setopt", {C_PTR, C_INT, C_PTR}, C_INT)
    end if
    --
    -- NB these are not necessarily set in stone:
    -- (however, "guessing" what to do with eg a sequence of strings
    --            does not strike me as the sensible thing to do...)
    --
    if option<10000 then    -- integer settings
        if not integer(param) then ?9/0 end if
    elsif option<20000 then -- pointer settings (including char*)
        if not atom(param) and not string(param) then ?9/0 end if
    elsif option<30000 then -- callback settings
        if not atom(param) then ?9/0 end if
    elsif option<40000 then -- "LARGE" settings
        if not atom(param) then ?9/0 end if
    else
        ?9/0
    end if
    -- hmm... (earlier version of the above, may as well leave it here)
    if sequence(param) and not string(param) then
        ?9/0    -- placeholder for more code...
    end if
    -- specific fixups (of things I deem a bit daft)
    if option=CURLOPT_SSL_VERIFYHOST then
        -- replace a deprecated debug option (treat true as 2):
        if param=1 then param=2 end if
    end if
    res = c_func(xcurl_easy_setopt, {curl, option, param})
    return res
end function

global procedure curl_easy_setopt(atom curl, CURLoption option, object param)
    CURLcode res = curl_easy_setoptf(curl, option, param)
    if res!=CURLE_OK then ?9/0 end if
end procedure

--------------------------------------------------------------------------------
atom xcurl_easy_perform = NULL
global function curl_easy_perform(atom curl)
    if xcurl_easy_perform=NULL then
        xcurl_easy_perform = link_c_func(libcurl, "curl_easy_perform", {C_PTR}, C_INT)
    end if
    CURLcode res = c_func(xcurl_easy_perform, {curl})
    return res
end function

--------------------------------------------------------------------------------

sequence curl_easy_buffers = {} -- for curl_easy_perform_ex() only
--sequence curl_multi_rids = {}
integer ceb_cs = init_cs()      -- make "" thread-safe

------------------------------
-- curl_easy_write_callback
------------------------------
function curl_easy_write_callback(atom pData, integer size, integer nmemb, integer slot_no)
    integer bytes_written = size*nmemb
    enter_cs(ceb_cs)
    curl_easy_buffers[slot_no] &= peek({pData,bytes_written})
--?{"curl_easy_write_callback",slot_no,bytes_written}
    leave_cs(ceb_cs)
    return bytes_written
end function
constant write_cb = call_back({'+',routine_id("curl_easy_write_callback")})

-----------------------------
-- curl_easy_perform_ex
-----------------------------
--global function curl_easy_perform_ex(atom curl)
--global function curl_easy_perform_ex(atom_string curl)
global function curl_easy_perform_ex(object curl)
-- see also curl_multi_perform_ex, if you modify this.
    enter_cs(ceb_cs)
    integer slot_no = 0
    for i=1 to length(curl_easy_buffers) do
        if integer(curl_easy_buffers[i]) then
            curl_easy_buffers[i] = ""
            slot_no = i
            exit
        end if
    end for
    if slot_no=0 then
        curl_easy_buffers = append(curl_easy_buffers,"")
--      curl_multi_rids = append(curl_multi_rids,0)
        slot_no = length(curl_easy_buffers)
    end if
    leave_cs(ceb_cs)

    bool free_curl = false,
         was_global_init = global_init
    if string(curl) then
        string url = curl
        if not was_global_init then curl_global_init() end if
        curl = curl_easy_init()
        curl_easy_setopt(curl, CURLOPT_URL, url)
        free_curl = true
    end if
    -- set callback function to receive data
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_cb)
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, slot_no)

    -- get file
    integer ret = curl_easy_perform(curl)
    if free_curl then
        curl_easy_cleanup(curl)
        if not was_global_init then curl_global_cleanup() end if
    end if

    enter_cs(ceb_cs)
    string res = curl_easy_buffers[slot_no]
    curl_easy_buffers[slot_no] = 0  -- (can now be reused)
    leave_cs(ceb_cs)

    if ret!=CURLE_OK then
        return ret
    end if

    return res
end function

function curl_easy_write_fn_callback(atom pData, integer size, integer nmemb, integer fn)
    integer bytes_written = size*nmemb
    puts(fn,peek({pData,bytes_written}))
    return bytes_written
end function
constant write_fn_cb = call_back({'+',routine_id("curl_easy_write_fn_callback")})

--------------------------------------------------------------------------------

-- NAME curl_easy_send()
--
-- DESCRIPTION
--
-- Sends data over the connected socket. Use after successful
-- curl_easy_perform() with CURLOPT_CONNECT_ONLY option.
--/
-- CURL_EXTERN CURLcode curl_easy_send(CURL *curl, const void *buffer,
--                                 -- size_t buflen, size_t *n);
atom xcurl_easy_send = NULL
global function curl_easy_send(atom curl, object buffer, integer buflen=length(buffer))
    if xcurl_easy_send=NULL then
--      xcurl_easy_send = link_c_func(libcurl, "curl_easy_send", {C_PTR, C_PTR, C_SIZE_T, C_PTR}, C_INT)
        xcurl_easy_send = link_c_func(libcurl, "curl_easy_send", {C_PTR, C_PTR, C_UINT, C_PTR}, C_INT)
    end if
    atom addr = allocate(4)
    poke4(addr,0)
    CURLcode ret = c_func(xcurl_easy_send, {curl, buffer, buflen, addr})
    integer len = peek4s(addr)
    free(addr)
    return {ret, len}
end function

--------------------------------------------------------------------------------

-- NAME curl_easy_recv()
--
-- DESCRIPTION
--
-- Receives data from the connected socket. Use after successful
-- curl_easy_perform() with CURLOPT_CONNECT_ONLY option.
--/
-- CURL_EXTERN CURLcode curl_easy_recv(CURL *curl, void *buffer, size_t buflen, size_t *n);
atom xcurl_easy_recv = NULL
global function curl_easy_recv(atom curl, atom buffer, integer buflen)
    if xcurl_easy_recv=NULL then
        xcurl_easy_recv = link_c_func(libcurl, "curl_easy_recv", {C_PTR, C_PTR, C_UINT, C_PTR}, C_INT)
    end if
    atom addr = allocate(4)
    poke4(addr,0)
    CURLcode ret = c_func(xcurl_easy_recv, {curl, buffer, buflen, addr})
    integer len = peek4s(addr)
    free(addr)
    return {ret, len}
end function

--------------------------------------------------------------------------------

global function curl_easy_get_file(string url, proxy, filename="")
CURLcode res
    if filename="" then
        -- allow curl_easy_get_file(url,filename) to
        --  mean curl_easy_get_file(url,"",filename)
        if proxy="" then ?9/0 end if
        {filename,proxy} = {proxy,""}
    end if
    integer fn = open(filename,"wb")
    if fn=-1 then
        res = CURLE_CANT_OPEN_FILE
    else
        bool was_global_init = global_init
        if not was_global_init then curl_global_init() end if
        atom curl = curl_easy_init()
        curl_easy_setopt(curl, CURLOPT_URL, url)
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_fn_cb)
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fn)
        if length(proxy)>0 then
            curl_easy_setopt(curl, CURLOPT_PROXY, proxy)
        end if
        res = curl_easy_perform(curl)
        close(fn)
        curl_easy_cleanup(curl)
        if not was_global_init then curl_global_cleanup() end if
    end if
    return res
end function

--------------------------------------------------------------------------------
--
-- NAME curl_slist_append()
--
-- DESCRIPTION
--
-- Appends a string to a linked list. If no list exists, it will be created
-- first. Returns the new list, after appending.
--/
-- CURL_EXTERN struct curl_slist *curl_slist_append(struct curl_slist *, const char *);
atom xcurl_slist_append = NULL
global function curl_slist_append(atom slist, string s)
    if xcurl_slist_append=NULL then
        xcurl_slist_append = link_c_func(libcurl, "curl_slist_append", {C_PTR, C_PTR}, C_PTR)
    end if
    slist = c_func(xcurl_slist_append, {slist, s})
    if slist=NULL then ?9/0 end if
    return slist
end function

--------------------------------------------------------------------------------

-- linked-list structure for the CURLOPT_QUOTE option (and other)
--
-- struct curl_slist {
--   char *data;
--   struct curl_slist *next;
-- };

-- struct curl_slist *
--   slist = curl_slist_append(slist, param)
--   s = peek_curl_slist(slist)

--------------------------------------------------------------------------------

--global 
function peek_curl_slist(atom slist)
sequence strings = {}
atom pstr, next = slist
string str

    while next!=NULL do
        {pstr,next} = peek_pointer({next,2})
        if pstr=NULL then exit end if
        str = peek_string(pstr)
        strings = append(strings, str)
    end while
    return strings
end function

--------------------------------------------------------------------------------

atom xcurl_slist_free_all = NULL
global procedure curl_slist_free_all(atom slist)
    if xcurl_slist_free_all=NULL then
        xcurl_slist_free_all = link_c_proc(libcurl, "curl_slist_free_all", {C_PTR})
    end if
    c_proc(xcurl_slist_free_all, {slist})
end procedure
--------------------------------------------------------------------------------

--DEV...
--/*
constant t_slist = """
typedef struct curl_slist {
  char *data;
  struct curl_slist *next;
};""",
id_slist = define_struct(t_slist)

constant t_certinfo = """
typedef struct curl_certinfo {
  int num_of_certs;             /* number of certificates with information */
  struct curl_slist **certinfo; /* for each index in this array, there's a
                                   linked list with textual information in the
                                   format "name: value" */
};""",
id_certinfo = define_struct(t_certinfo)
--*/

-- NAME curl_easy_getinfo()
--
-- DESCRIPTION
--
-- Request internal information from the curl session with this global function.  The
-- third argument MUST be a pointer to a long, a pointer to a char * or a
-- pointer to a double (as the documentation describes elsewhere).  The data
-- pointed to will be filled in accordingly and can be relied upon only if the
-- global function returns CURLE_OK.  This global function is intended to get used *AFTER* a
-- performed transfer, all results from this global function are undefined until the
-- transfer is completed.
--/
-- CURL_EXTERN CURLcode curl_easy_getinfo(CURL *curl, CURLINFO info, ...);
atom xcurl_easy_getinfo = NULL

global function curl_easy_getinfo(atom curl, integer option)
atom param
object o, res
    if xcurl_easy_getinfo=NULL then
        xcurl_easy_getinfo = link_c_func(libcurl, "curl_easy_getinfo", {C_PTR, C_INT, C_PTR}, C_INT)
    end if

    integer option_type = and_bits(option,CURLINFO_TYPEMASK)

    if option_type=CURLINFO_DOUBLE then
        param = allocate(8)
    elsif option_type=CURLINFO_LONG then
        param = allocate(4)
    elsif option_type=CURLINFO_SLIST then
        param = curl_slist_append(NULL, "")
    elsif option_type=CURLINFO_SOCKET then
--DEV??
--      param = allocate(4)
        param = allocate(W)
    elsif option_type=CURLINFO_STRING then
--      param = allocate_string("")     -- ???
        param = allocate(W)
    else
        ?9/0    -- unknown option_type
    end if
  
    integer curlcode = c_func(xcurl_easy_getinfo, {curl, option, param})

--  analyzeObject(ret, "ret", f_debug, 0)
  
    if curlcode=CURLE_UNKNOWN_OPTION then
        if option_type=CURLINFO_STRING then
            res = ""
        else
            res = NULL
        end if
--?9/0 --[DEV]
--      o = findID(GETINFO_OPTIONS, option, 1)
--      if atom(o) then
--          warnError(sprintf("Unknown option '%d'", {option}), 1)
--      else
--          warnError(sprintf("Unknown option '%s'", {o[2]}), 1)
--      end if
    elsif curlcode!=CURLE_OK then
        printf(1, "Curl curl_easy_getinfo failed: (%d) %s\n",
                  {curlcode, curl_easy_strerror(curlcode)})
?9/0 -- DEV (should never happen??)
    elsif option_type=CURLINFO_STRING then
        atom pstr = peek_pointer(param)
        if pstr=NULL then
            res = ""
        else
            res = peek_string(pstr)
        end if
    elsif option_type=CURLINFO_SOCKET then
--DEV??
--      res = peek4u(param)
        res = peek_pointer(param)
    elsif option_type=CURLINFO_DOUBLE then
        res = float64_to_atom(peek({param,8}))  -- peek8s(param)
    elsif option_type=CURLINFO_LONG then
        res = peek4s(param)
    elsif option=CURLINFO_CERTINFO then  -- struct curl_certinfo *
--DEV
if 0 then
        o = peek_pointer({peek_pointer(param),2})
--    analyzeObject(o, "o", f_debug, 0)
--pp(o)
--DEV
        res = peek_curl_slist(peek_pointer(o[2]))
--          res = peek_curl_slist(peek4u(o[2]))
--pp(res)
--          curl_slist_free_all(param)
else
        {integer ncerts, atom pcerts} = peek_pointer({peek_pointer(param),2})
        res = {}
        for i=1 to ncerts do
            res = append(res,peek_curl_slist(peek_pointer(pcerts)))
            pcerts += W
        end for
end if

    elsif option_type=CURLINFO_SLIST then
        res = peek_curl_slist(peek_pointer(param))
--      res = peek_curl_slist(peek4u(param))
    else
        ?9/0    -- unknown type?
--    printf(f_debug, "Curl curl_easy_getinfo failed: %s\n",
--           {curl_easy_strerror(ret)})
--    res = -1
    end if

    if option_type=CURLINFO_SLIST then
        curl_slist_free_all(param)
    else
        free(param)
    end if

    return {curlcode,res}
end function

atom xcurl_multi_init = NULL
global function curl_multi_init()
    if xcurl_multi_init=NULL then
        if libcurl=NULL then curl_init() end if
        xcurl_multi_init = link_c_func(libcurl, "curl_multi_init", {}, C_PTR)
    end if
    atom mcurl = c_func(xcurl_multi_init, {})
    if mcurl=NULL then ?9/0 end if
    return mcurl
end function

constant 
    CURLM_LONG      =     0,
    CURLM_OBJECT    = 10000,
    CURLM_FUNCTION  = 20000,
    CURLM_OFF_T     = 30000

global constant
    CURLMOPT_SOCKETFUNCTION              = CURLM_FUNCTION + 1,  /* This is the socket callback function pointer */
    CURLMOPT_SOCKETDATA                  =   CURLM_OBJECT + 2,  /* This is the argument passed to the socket callback */
    CURLMOPT_PIPELINING                  =     CURLM_LONG + 3,  /* set to 1 to enable pipelining for this multi handle */
    CURLMOPT_TIMERFUNCTION               = CURLM_FUNCTION + 4,  /* This is the timer callback function pointer */
    CURLMOPT_TIMERDATA                   =   CURLM_OBJECT + 5,  /* This is the argument passed to the timer callback */
    CURLMOPT_MAXCONNECTS                 =     CURLM_LONG + 6,  /* maximum number of entries in the connection cache */
    CURLMOPT_MAX_HOST_CONNECTIONS        =     CURLM_LONG + 7,  /* maximum number of (pipelining) connections to one host */
    CURLMOPT_MAX_PIPELINE_LENGTH         =     CURLM_LONG + 8,  /* maximum number of requests in a pipeline */
    CURLMOPT_CONTENT_LENGTH_PENALTY_SIZE =    CURLM_OFF_T + 9,  /* a connection with a content-length longer than this will not be considered for pipelining */
    CURLMOPT_CHUNK_LENGTH_PENALTY_SIZE   =    CURLM_OFF_T + 10, /* a connection with a chunk length longer than this will not be considered for pipelining */
    CURLMOPT_PIPELINING_SITE_BL          =   CURLM_OBJECT + 11, /* a list of site names(+port) that are blacklisted from pipelining */
    CURLMOPT_PIPELINING_SERVER_BL        =   CURLM_OBJECT + 12, /* a list of server types that are blacklisted from pipelining */
    CURLMOPT_MAX_TOTAL_CONNECTIONS       =     CURLM_LONG + 13, /* maximum number of open connections in total */
    CURLMOPT_PUSHFUNCTION                = CURLM_FUNCTION + 14, /* This is the server push callback function pointer */
    CURLMOPT_PUSHDATA                    =   CURLM_OBJECT + 15  /* This is the argument passed to the server push callback */

global type CURLMoption(integer i)
    integer rem = remainder(i,10000)
    return ((rem>=1) and (rem<=15))
end type

atom xcurl_pushheader_bynum = NULL
global function curl_pushheader_bynum(atom p_curl_pushheaders, integer num)
    if xcurl_pushheader_bynum=NULL then
        xcurl_pushheader_bynum = link_c_func(libcurl, "curl_pushheader_bynum", {C_PTR, C_INT}, C_PTR)
    end if
    atom ptr = c_func(xcurl_pushheader_bynum, {p_curl_pushheaders, num})
    return ptr
end function

atom xcurl_pushheader_byname = NULL
global function curl_pushheader_byname(atom p_curl_pushheaders, string name)
    if xcurl_pushheader_byname=NULL then
        xcurl_pushheader_byname = link_c_func(libcurl, "curl_pushheader_byname", {C_PTR, C_PTR}, C_PTR)
    end if
    atom ptr = c_func(xcurl_pushheader_byname, {p_curl_pushheaders, name})
    return ptr
end function

--as/copy of IupRawStringPtr:
function raw_string_ptr(string s)
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

function make_string_array(sequence strings)
--atom res = allocate((length(strings)+1)*W,cleanup:=true)
atom res = allocate((length(strings)+1)*W,true),
     ptr = res
    for i=1 to length(strings) do
        string si = strings[i]
        pokeN(ptr,raw_string_ptr(si),W)
        ptr += W
    end for
    pokeN(ptr,NULL,W)
    return res
end function

atom xcurl_multi_add_handle = NULL
global procedure curl_multi_add_handle(atom mcurl, curl)
    if xcurl_multi_add_handle=NULL then
        xcurl_multi_add_handle = link_c_func(libcurl, "curl_multi_add_handle", {C_PTR, C_PTR}, C_INT)
    end if
    CURLMcode res = c_func(xcurl_multi_add_handle, {mcurl,curl})
    if res!=CURLM_OK then ?9/0 end if
end procedure

atom xcurl_multi_setopt = NULL
global function curl_multi_setopt(atom mcurl, CURLMoption option, object param)
    if xcurl_multi_setopt=NULL then
        xcurl_multi_setopt = link_c_func(libcurl, "curl_multi_setopt", {C_PTR, C_INT, C_PTR}, C_INT)
    end if
    atom params
    if option=CURLMOPT_PIPELINING_SITE_BL
    or option=CURLMOPT_PIPELINING_SERVER_BL then
        params = make_string_array(param) -- (hosts or servers)
    else
--      if not atom(param) then ?9/0 end if -- (caught next anyway)
        params = param
    end if
    CURLMcode res = c_func(xcurl_multi_setopt, {mcurl, option, params})
    return res
end function

atom xcurl_multi_remove_handle = NULL
global procedure curl_multi_remove_handle(atom mcurl, curl)
    if xcurl_multi_remove_handle=NULL then
        xcurl_multi_remove_handle = link_c_func(libcurl, "curl_multi_remove_handle", {C_PTR, C_PTR}, C_INT)
    end if
    CURLMcode res = c_func(xcurl_multi_remove_handle, {mcurl,curl})
    if res!=CURLM_OK then ?9/0 end if
end procedure

atom xcurl_multi_perform = NULL
global function curl_multi_perform(atom mcurl)
    if xcurl_multi_perform=NULL then
        xcurl_multi_perform = link_c_func(libcurl, "curl_multi_perform", {C_PTR, C_PTR}, C_INT)
    end if
    atom p_running_handles = allocate(W)
    CURLMcode result = c_func(xcurl_multi_perform, {mcurl, p_running_handles})
    integer running_handles = peekNS(p_running_handles,W,true)
    free(p_running_handles)
    return {result,running_handles}
end function

global function curl_multi_perform_ex(atom mcurl, curl)--, integer rid)
-- see also curl_easy_perform_ex, if you modify this.

    enter_cs(ceb_cs)
    integer slot_no = 0
    for i=1 to length(curl_easy_buffers) do
        if integer(curl_easy_buffers[i]) then
            curl_easy_buffers[i] = ""
--          curl_multi_rids[i] = rid
            slot_no = i
            exit
        end if
    end for
    if slot_no=0 then
        curl_easy_buffers = append(curl_easy_buffers,"")
--      curl_multi_rids = append(curl_multi_rids,rid)
        slot_no = length(curl_easy_buffers)
    end if
    leave_cs(ceb_cs)

    -- set callback function to receive data
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_cb)
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, slot_no)

    -- get file
--  integer res = 
    curl_multi_add_handle(mcurl,curl)
--  if res!=CURLM_OK then ?9/0 end if
    return slot_no
end function

--global function curl_multi_complete(atom curl, integer slot_no)
global function curl_multi_complete(integer slot_no)
--
-- note: slot_no is as set via CURLOPT_WRITEDATA, but I cannot 
--       see any way to retrieve that. Hence it is necessary
--       for the calling code to maintain that mapping.
--       Hence keeping curl_multi_rids in here is pointless...
--       Also note that slot_no is automatically marked as
--       available for re-use by this routine, so you must
--       retrieve any/all mappings, before calling this.
--       Lastly, you should only be calling this if
--       curl_multi_info_read() gave you a CURLE_OK.
--
    enter_cs(ceb_cs)
--10/7...
--  string res = curl_easy_buffers[slot_no]
    object res = curl_easy_buffers[slot_no]
    curl_easy_buffers[slot_no] = 0  -- (can now be reused)
    leave_cs(ceb_cs)

--  if ret!=CURLE_OK then
--      return ret
--  end if

    return res
end function

global constant CURL_WAIT_POLLIN  = 0x0001,
                CURL_WAIT_POLLPRI = 0x0002,
                CURL_WAIT_POLLOUT = 0x0004

atom xcurl_multi_wait = NULL
global function curl_multi_wait(atom mcurl, extra_fds, integer nextra_fds, timeout_ms)
    if xcurl_multi_wait=NULL then
        xcurl_multi_wait = link_c_func(libcurl, "curl_multi_wait", {C_PTR, C_PTR, C_INT, C_INT, C_PTR}, C_INT)
    end if
    atom p_numfds = allocate(W)
    CURLMcode result = c_func(xcurl_multi_wait, {mcurl, extra_fds, nextra_fds, timeout_ms, p_numfds})
    integer numfds = peekNS(p_numfds,W,true)
    free(p_numfds)
    return {result,numfds}
end function

atom xcurl_multi_timeout = NULL
global function curl_multi_timeout(atom mcurl)
    if xcurl_multi_timeout=NULL then
        xcurl_multi_timeout = link_c_func(libcurl, "curl_multi_timeout", {C_PTR, C_PTR}, C_INT)
    end if
    atom p_timeout = allocate(W)
    CURLMcode result = c_func(xcurl_multi_timeout, {mcurl, p_timeout})
    integer timeout = peekNS(p_timeout,W,true)
    free(p_timeout)
    return {result,timeout}
end function

--/*
struct CURLMsg {
   CURLMSG msg;       /* what this message means */
   CURL *easy_handle; /* the handle it concerns */
   union {
     void *whatever;    /* message-specific data */
     CURLcode result;   /* return code for transfer */
   } data;
 };
--*/
--global 
constant CURLMSG_DONE = 1

atom xcurl_multi_info_read = NULL
global function curl_multi_info_read(atom mcurl)
    if xcurl_multi_info_read=NULL then
        xcurl_multi_info_read = link_c_func(libcurl, "curl_multi_info_read", {C_PTR, C_PTR}, C_PTR)
    end if
    atom pnmsgs = allocate(W)
    atom pMsg = c_func(xcurl_multi_info_read, {mcurl, pnmsgs})
    atom easy_handle = NULL
    CURLcode result = CURLE_OK
    if pMsg!=NULL then
        integer msg = peekNS(pMsg,W,true)
        easy_handle = peekNS(pMsg+W,W,false)
        result = peekNS(pMsg+2*W,W,true)
        if msg!=CURLMSG_DONE then ?9/0 end if   -- sanity check
    end if
    integer nmsgs = peekNS(pnmsgs,W,true)
    free(pnmsgs)
    return {easy_handle,result,nmsgs}
end function

atom xcurl_multi_cleanup = NULL
global function curl_multi_cleanup(atom mcurl)
    if xcurl_multi_cleanup=NULL then
        xcurl_multi_cleanup = link_c_func(libcurl, "curl_easy_cleanup", {C_PTR}, C_INT)
    end if
    CURLMcode curlmcode = c_func(xcurl_multi_cleanup, {mcurl})
    return curlmcode
end function

atom xcurl_multi_strerror = NULL
global function curl_multi_strerror(CURLMcode error)
    if xcurl_multi_strerror=NULL then
        -- aside: permit eg curl_multi_strerror(CURLM_BAD_EASY_HANDLE), as
        --  opposed to only allowing results from a real call to libcurl.
        if libcurl=NULL then curl_init() end if
        xcurl_multi_strerror = link_c_func(libcurl, "curl_multi_strerror", {C_INT}, C_PTR)
    end if
    atom pRes = c_func(xcurl_multi_strerror, {error})
    string res = peek_string(pRes)
    return res
end function

atom xcurl_share_init = NULL
global function curl_share_init()
    if xcurl_share_init=NULL then
        if libcurl=NULL then curl_init() end if
        xcurl_share_init = link_c_func(libcurl, "curl_share_init", {}, C_PTR)
    end if
    atom curlshare = c_func(xcurl_share_init, {})
    if curlshare=NULL then ?9/0 end if
    return curlshare
end function


-- Setup defines, protos etc for the sharing stuff.

-- Different data locks for a single share
global constant
--  CURL_LOCK_DATA_NONE      = 0,
--  CURL_LOCK_DATA_SHARE     = 1,
  CURL_LOCK_DATA_COOKIE      = 2,
  CURL_LOCK_DATA_DNS         = 3,
  CURL_LOCK_DATA_SSL_SESSION = 4,
  CURL_LOCK_DATA_CONNECT     = 5

global type curl_share_data(integer i)
    return ((i>=CURL_LOCK_DATA_COOKIE) and (i<=CURL_LOCK_DATA_CONNECT))
end type

-- Different lock access types
global constant
--  CURL_LOCK_ACCESS_NONE   = 0, -- unspecified action
  CURL_LOCK_ACCESS_SHARED = 1, -- for read perhaps
  CURL_LOCK_ACCESS_SINGLE = 2  -- for write perhaps
--  CURL_LOCK_ACCESS_LAST   = 4  -- never use

global type curl_lock_access(integer i)
    return ((i>=CURL_LOCK_ACCESS_SHARED) and (i<=CURL_LOCK_ACCESS_SINGLE))
end type
--/*

-- typedef void (*curl_lock_function)(CURL *handle,
--                                 -- curl_share_data data,
--                                 -- curl_lock_access locktype,
--                                 -- void *userptr);
-- typedef void (*curl_unlock_function)(CURL *handle,
--                                 --   curl_share_data data,
--                                 --   void *userptr);


--*/
global constant 
  CURLSHOPT_SHARE      = 1,  -- specify a data type to share
  CURLSHOPT_UNSHARE    = 2,  -- specify which data type to stop sharing
  CURLSHOPT_LOCKFUNC   = 3,  -- pass in a 'curl_lock_function' pointer
  CURLSHOPT_UNLOCKFUNC = 4,  -- pass in a 'curl_unlock_function' pointer
  CURLSHOPT_USERDATA   = 5   -- pass in a user data pointer used in the lock/unlock
                             -- callback functions

global type CURLSHoption(integer i)
    return ((i>=CURLSHOPT_SHARE) and (i<=CURLSHOPT_USERDATA))
end type

-- CURL_EXTERN CURLSHcode curl_share_setopt(CURLSH *, CURLSHoption option, ...);
atom xcurl_share_setopt = NULL
global function curl_share_setopt(atom curlshare, CURLSHoption share_option, atom param)
    if xcurl_share_setopt=NULL then
        if libcurl=NULL then curl_init() end if
        xcurl_share_setopt = link_c_func(libcurl, "curl_share_setopt", {C_PTR, C_INT, C_PTR}, C_INT)
    end if
    CURLSHcode res = c_func(xcurl_share_setopt, {curlshare,share_option,param})
    return res
end function

atom xcurl_share_cleanup = NULL
global function curl_share_cleanup(atom curlshare)
    if xcurl_share_cleanup=NULL then
        if libcurl=NULL then curl_init() end if
        xcurl_share_cleanup = link_c_func(libcurl, "curl_share_cleanup", {C_PTR}, C_INT)
    end if
    CURLSHcode res = c_func(xcurl_share_cleanup, {curlshare})
    return res
end function

atom xcurl_share_strerror = NULL
global function curl_share_strerror(CURLSHcode error)
    if xcurl_share_strerror=NULL then
        if libcurl=NULL then curl_init() end if
        xcurl_share_strerror = link_c_func(libcurl, "curl_share_strerror", {C_INT}, C_PTR)
    end if
    atom pRes = c_func(xcurl_share_strerror, {error})
    string res = peek_string(pRes)
    return res
end function

--------------------------------------------------------------------------------

--/*
constant GETINFO_OPTIONS = {
  {CURLINFO_CONTENT_TYPE,               "CURLINFO_CONTENT_TYPE"},
  {CURLINFO_EFFECTIVE_URL,              "CURLINFO_EFFECTIVE_URL"},
  {CURLINFO_FTP_ENTRY_PATH,             "CURLINFO_FTP_ENTRY_PATH"},
  {CURLINFO_LOCAL_IP,                   "CURLINFO_LOCAL_IP"},
  {CURLINFO_PRIMARY_IP,                 "CURLINFO_PRIMARY_IP"},
  {CURLINFO_PRIVATE,                    "CURLINFO_PRIVATE"},
  {CURLINFO_REDIRECT_URL,               "CURLINFO_REDIRECT_URL"},
  {CURLINFO_RTSP_SESSION_ID,            "CURLINFO_RTSP_SESSION_ID"},
  {CURLINFO_ACTIVESOCKET,               "CURLINFO_ACTIVESOCKET"},
  {CURLINFO_APPCONNECT_TIME,            "CURLINFO_APPCONNECT_TIME"},
  {CURLINFO_CONNECT_TIME,               "CURLINFO_CONNECT_TIME"},
  {CURLINFO_CONTENT_LENGTH_DOWNLOAD,    "CURLINFO_CONTENT_LENGTH_DOWNLOAD"},
  {CURLINFO_CONTENT_LENGTH_UPLOAD,      "CURLINFO_CONTENT_LENGTH_UPLOAD"},
  {CURLINFO_NAMELOOKUP_TIME,            "CURLINFO_NAMELOOKUP_TIME"},
  {CURLINFO_PRETRANSFER_TIME,           "CURLINFO_PRETRANSFER_TIME"},
  {CURLINFO_REDIRECT_TIME,              "CURLINFO_REDIRECT_TIME"},
  {CURLINFO_SIZE_DOWNLOAD,              "CURLINFO_SIZE_DOWNLOAD"},
  {CURLINFO_SIZE_UPLOAD,                "CURLINFO_SIZE_UPLOAD"},
  {CURLINFO_SPEED_DOWNLOAD,             "CURLINFO_SPEED_DOWNLOAD"},
  {CURLINFO_SPEED_UPLOAD,               "CURLINFO_SPEED_UPLOAD"},
  {CURLINFO_STARTTRANSFER_TIME,         "CURLINFO_STARTTRANSFER_TIME"},
  {CURLINFO_TOTAL_TIME,                 "CURLINFO_TOTAL_TIME"},
  {CURLINFO_CONDITION_UNMET,            "CURLINFO_CONDITION_UNMET"},
  {CURLINFO_FILETIME,                   "CURLINFO_FILETIME"},
  {CURLINFO_HEADER_SIZE,                "CURLINFO_HEADER_SIZE"},
  {CURLINFO_HTTP_CONNECTCODE,           "CURLINFO_HTTP_CONNECTCODE"},
  {CURLINFO_HTTP_VERSION,               "CURLINFO_HTTP_VERSION"},
  {CURLINFO_HTTPAUTH_AVAIL,             "CURLINFO_HTTPAUTH_AVAIL"},
  {CURLINFO_LASTSOCKET,                 "CURLINFO_LASTSOCKET"},
  {CURLINFO_LOCAL_PORT,                 "CURLINFO_LOCAL_PORT"},
  {CURLINFO_NUM_CONNECTS,               "CURLINFO_NUM_CONNECTS"},
  {CURLINFO_OS_ERRNO,                   "CURLINFO_OS_ERRNO"},
  {CURLINFO_PRIMARY_PORT,               "CURLINFO_PRIMARY_PORT"},
  {CURLINFO_PROXYAUTH_AVAIL,            "CURLINFO_PROXYAUTH_AVAIL"},
  {CURLINFO_REDIRECT_COUNT,             "CURLINFO_REDIRECT_COUNT"},
  {CURLINFO_REQUEST_SIZE,               "CURLINFO_REQUEST_SIZE"},
  {CURLINFO_RESPONSE_CODE,              "CURLINFO_RESPONSE_CODE"},
  {CURLINFO_RTSP_CLIENT_CSEQ,           "CURLINFO_RTSP_CLIENT_CSEQ"},
  {CURLINFO_RTSP_CSEQ_RECV,             "CURLINFO_RTSP_CSEQ_RECV"},
  {CURLINFO_RTSP_SERVER_CSEQ,           "CURLINFO_RTSP_SERVER_CSEQ"},
  {CURLINFO_SSL_VERIFYRESULT,           "CURLINFO_SSL_VERIFYRESULT"},
  {CURLINFO_CERTINFO,                   "CURLINFO_CERTINFO"},
  {CURLINFO_COOKIELIST,                 "CURLINFO_COOKIELIST"},
  {CURLINFO_SSL_ENGINES,                "CURLINFO_SSL_ENGINES"},
  {CURLINFO_TLS_SESSION,                "CURLINFO_TLS_SESSION"},
  {CURLINFO_TLS_SSL_PTR,                "CURLINFO_TLS_SSL_PTR"}
}


--include dll.e
--include misc.e
--include myLibs/myDebug.e
--include myLibs/common.e
--include std/eumem.e
--
-- If you have libcurl problems, all docs and details are found here:
--   https://curl.haxx.se/libcurl/
--
-- curl-library mailing list subscription and unsubscription web interface:
--   https://cool.haxx.se/mailman/listinfo/curl-library/
--/
--

-- openeuphoria 4.0 types
constant
--  C_BYTE      = C_CHAR,
--  C_WORD      = C_SHORT,
--  C_BOOL      = C_INT,
--  C_HRESULT   = C_INT,
--  C_LPARAM    = C_INT,
--  C_WPARAM    = C_INT,
--  C_UBYTE     = C_UCHAR,
--  C_DWORD     = C_UINT,
--  C_HANDLE    = C_ULONG,
  C_SIZE_T    = C_UINT --,
--  C_DWORDLONG = C_DOUBLE

global integer error_code = 0

global sequence error_msg = ""

global atom slist = NULL

--atom libcurl

--procedure not_found(sequence name)
--  warnError("Couldn't find " & name, 1)
--end procedure

--function link_c_var(atom dll, sequence name)
--  atom address
--
--  address = define_c_var(dll, "+" & name)
--  if address = -1 then
--  not_found(name)
--  end if
--  return address
--end function

--------------------------------------------------------------------------------

object dll_, uname_
atom ptr
sequence arch


---------------------------------------------------------------------------------

global constant
  CURL_HTTPPOST_FILENAME    =   1,  -- specified content is a file name
  CURL_HTTPPOST_READFILE    =   2,  -- specified content is a file name
  CURL_HTTPPOST_PTRNAME     =   4,  -- name is only stored pointer do not
                                    -- free in formfree
  CURL_HTTPPOST_PTRCONTENTS =   8,  -- contents is only stored pointer do
                                    -- not free in formfree
  CURL_HTTPPOST_BUFFER      =  16,  -- upload file from buffer
  CURL_HTTPPOST_PTRBUFFER   =  32,  -- upload file from pointer contents
  CURL_HTTPPOST_CALLBACK    =  64,  -- upload file contents by using the
                                    -- regular read callback to get the data and
                                    -- pass the given pointer as custom pointer
  CURL_HTTPPOST_LARGE       = 128   -- use size in 'contentlen', added in 7.46.0

-- struct curl_httppost {
--   struct curl_httppost *next;       -- next entry in the list
--   char *name;                       -- pointer to allocated name
--   long namelength;                  -- length of name length
--   char *contents;                   -- pointer to allocated data contents
--   long contentslength;              -- length of contents field, see also
--                                 --  -- CURL_HTTPPOST_LARGE
--   char *buffer;                     -- pointer to allocated buffer contents
--   long bufferlength;                -- length of buffer field
--   char *contenttype;                -- Content-Type
--   struct curl_slist* contentheader; -- list of extra headers for this form
--   struct curl_httppost *more;       -- if one field name has more than one
--                                 --  -- file, this link should link to following
--                                 --  -- files
--   long flags;                       -- as defined below
--   char *showfilename;               -- The file name to show. If not set, the
--                                 --  -- actual file name will be used (if this
--                                 --  -- is a file part)
--   void *userp;                      -- custom pointer used for
--                                 --  -- HTTPPOST_CALLBACK posts
--   curl_off_t contentlen;            -- alternative length of contents
--                                 --  -- field. Used if CURL_HTTPPOST_LARGE is
--                                 --  -- set. Added in 7.46.0
-- };

-- This is the CURLOPT_PROGRESSFUNCTION callback proto. It is now considered
--   deprecated but was the only choice up until 7.31.0
--
-- Ttypedef int (*curl_progress_callback)(void *clientp,
--                                 --    double dltotal,
--                                 --    double dlnow,
--                                 --    double ultotal,
--                                 --    double ulnow);

-- This is the CURLOPT_XFERINFOFUNCTION callback proto. It was introduced in
--    7.32.0, it avoids floating point and provides more detailed information.
--
-- typedef int (*curl_xferinfo_callback)(void *clientp,
--                                 --    curl_off_t dltotal,
--                                 --    curl_off_t dlnow,
--                                 --    curl_off_t ultotal,
--                                 --    curl_off_t ulnow);

--   Tests have proven that 20K is a very bad buffer size for uploads on
--      Windows, while 16K for some odd reason performed a lot better.
--      We do the ifndef check to allow this value to easier be changed at build
--      time for those who feel adventurous. The practical minimum is about
--      400 bytes since libcurl uses a buffer of this size as a scratch area
--      (unrelated to network send operations).
global constant CURL_MAX_WRITE_SIZE = 16384

-- The only reason to have a max limit for this is to avoid the risk of a bad
--    server feeding libcurl with a never-ending header that will cause reallocs
--    infinitely
global constant CURL_MAX_HTTP_HEADER = (100*1024)

-- This is a magic return code for the write callback that, when returned,
--    will signal libcurl to pause receiving on the current transfer.
global constant CURL_WRITEFUNC_PAUSE = #10000001

-- enumeration of file types
global constant
  CURLFILETYPE_FILE         = 0,
  CURLFILETYPE_DIRECTORY    = 1,
  CURLFILETYPE_SYMLINK      = 2,
  CURLFILETYPE_DEVICE_BLOCK = 3,
  CURLFILETYPE_DEVICE_CHAR  = 4,
  CURLFILETYPE_NAMEDPIPE    = 5,
  CURLFILETYPE_SOCKET       = 6,
  CURLFILETYPE_DOOR         = 7, -- is possible only on Sun Solaris now
  CURLFILETYPE_UNKNOWN      = 8  -- should never occur

--global type curlfiletype(integer n)
--  return ((n >= CURLFILETYPE_FILE) and (n <= CURLFILETYPE_UNKNOWN))
--end type

global constant
  CURLFINFOFLAG_KNOWN_FILENAME   =   1,
  CURLFINFOFLAG_KNOWN_FILETYPE   =   2,
  CURLFINFOFLAG_KNOWN_TIME       =   4,
  CURLFINFOFLAG_KNOWN_PERM       =   8,
  CURLFINFOFLAG_KNOWN_UID        =  16,
  CURLFINFOFLAG_KNOWN_GID        =  32,
  CURLFINFOFLAG_KNOWN_SIZE       =  64,
  CURLFINFOFLAG_KNOWN_HLINKCOUNT = 128

-- Content of this structure depends on information which is known and is
--    achievable (e.g. by FTP LIST parsing). Please see the url_easy_setopt(3) man
--    page for callbacks returning this structure -- some fields are mandatory,
--    some others are optional. The FLAG field has special meaning.

-- struct curl_fileinfo {
--   char *filename;
--   curlfiletype filetype;
--   time_t time;
--   unsigned int perm;
--   int uid;
--   int gid;
--   curl_off_t size;
--   long int hardlinks;

--   struct {
--     -- If some of these fields is not NULL, it is a pointer to b_data.
--     char *time;
--     char *perm;
--     char *user;
--     char *group;
--     char *target; -- pointer to the target filename of a symlink
--   } strings;

 --  unsigned int flags;

--   -- used internally
--   char * b_data;
--   size_t b_size;
--   size_t b_used;
-- };

-- return codes for CURLOPT_CHUNK_BGN_FUNCTION
global constant
  CURL_CHUNK_BGN_FUNC_OK   = 0,
  CURL_CHUNK_BGN_FUNC_FAIL = 1,  -- tell the lib to end the task
  CURL_CHUNK_BGN_FUNC_SKIP = 2   -- skip this chunk over

-- if splitting of data transfer is enabled, this callback is called before
--    download of an individual chunk started. Note that parameter "remains" works
--    only for FTP wildcard downloading (for now), otherwise is not used

-- typedef long (*curl_chunk_bgn_callback)(const void *transfer_info,
--                                 --      void *ptr,
--                                 --      int remains);

-- return codes for CURLOPT_CHUNK_END_FUNCTION
global constant
  CURL_CHUNK_END_FUNC_OK   = 0,
  CURL_CHUNK_END_FUNC_FAIL = 1  -- tell the lib to end the task

-- If splitting of data transfer is enabled this callback is called after
--    download of an individual chunk finished.
--    Note! After this callback was set then it have to be called FOR ALL chunks.
--    Even if downloading of this chunk was skipped in CHUNK_BGN_FUNC.
--    This is the reason why we don't need "transfer_info" parameter in this
--    callback and we are not interested in "remains" parameter too.

-- typedef long (*curl_chunk_end_callback)(void *ptr);

-- return codes for FNMATCHFUNCTION
global constant
  CURL_FNMATCHFUNC_MATCH   = 0,  -- string corresponds to the pattern
  CURL_FNMATCHFUNC_NOMATCH = 1,  -- pattern doesn't match the string
  CURL_FNMATCHFUNC_FAIL    = 2   -- an error occurred

-- callback type for wildcard downloading pattern matching. If the
--    string matches the pattern, return CURL_FNMATCHFUNC_MATCH value, etc.

-- typedef int (*curl_fnmatch_callback)(void *ptr,
--                                 --   const char *pattern,
--                                 --   const char *string);

-- These are the return codes for the seek callbacks
global constant
  CURL_SEEKFUNC_OK       = 0,
  CURL_SEEKFUNC_FAIL     = 1,  -- fail the entire transfer
  CURL_SEEKFUNC_CANTSEEK = 2   -- tell libcurl seeking can't be done, so
                               --   libcurl might try other means instead
-- typedef int (*curl_seek_callback)(void *instream,
--                                 --curl_off_t offset,
--                                 --int origin); -- 'whence'

-- This is a return code for the read callback that, when returned, will
--    signal libcurl to immediately abort the current transfer.
global constant CURL_READFUNC_ABORT = #10000000
-- This is a return code for the read callback that, when returned, will
--    signal libcurl to pause sending data on the current transfer.
global constant CURL_READFUNC_PAUSE = #10000001

-- typedef size_t (*curl_read_callback)(char *buffer,
--                                 --    size_t size,
--                                 --    size_t nitems,
--                                 --    void *instream);

------------------------------
-- curl_read_callback
------------------------------
--global function curl_read_callback(atom bufferp, integer size, integer nitems, atom instream)
--  instream = instream -- stop warning
--  poke(bufferp, ??curl_easy_buffer??)
--  return size * nitems
--end function

global constant
  CURLSOCKTYPE_IPCXN  = 0,  -- socket created for a specific IP connection
  CURLSOCKTYPE_ACCEPT = 1,  -- socket created by accept() call
  CURLSOCKTYPE_LAST   = 2   -- never use

--global type curlsocktype(integer n)
--  return ((n >= CURLSOCKTYPE_IPCXN) and (n <= CURLSOCKTYPE_LAST))
--end type

-- The return code from the sockopt_callback can signal information back
--    to libcurl:
global constant
  CURL_SOCKOPT_OK                = 0,
  CURL_SOCKOPT_ERROR             = 1,  -- causes libcurl to abort and return
                                 --    -- CURLE_ABORTED_BY_CALLBACK
  CURL_SOCKOPT_ALREADY_CONNECTED = 2

-- typedef int (*curl_sockopt_callback)(void *clientp,
--                                 --   curl_socket_t curlfd,
--                                 --   curlsocktype purpose);

-- struct curl_sockaddr {
--   int family;
--   int socktype;
--   int protocol;
--   unsigned int addrlen; -- addrlen was a socklen_t type before 7.18.0 but it
--                         -- turned really ugly and painful on the systems that
--                         -- lack this type
--   struct sockaddr addr;
-- };

-- typedef curl_socket_t
-- (*curl_opensocket_callback)(void *clientp,
--                             curlsocktype purpose,
--                             struct curl_sockaddr *address);

-- typedef int
-- (*curl_closesocket_callback)(void *clientp, curl_socket_t item);

global constant
  CURLIOE_OK          = 0,  -- I/O operation successful
  CURLIOE_UNKNOWNCMD  = 1,  -- command was unknown to callback
  CURLIOE_FAILRESTART = 2,  -- failed to restart the read
  CURLIOE_LAST        = 3   -- never use

--global type curlioerr(integer n)
--  return ((n >= CURLIOE_OK) and (n <= CURLIOE_LAST))
--end type

global constant
  CURLIOCMD_NOP         = 0,  -- no operation
  CURLIOCMD_RESTARTREAD = 1,  -- restart the read stream from start
  CURLIOCMD_LAST        = 2   -- never use

--global type curliocmd(integer n)
--  return ((n >= CURLIOCMD_NOP) and (n <= CURLIOCMD_LAST))
--end type

-- typedef curlioerr (*curl_ioctl_callback)(CURL *handle,
--                                 --       int cmd,
--                                 --       void *clientp);


global constant
  -- Previously obsolete error code re-used in 7.38.0
  CURLE_OBSOLETE16 = CURLE_HTTP2,

  -- Previously obsolete error codes re-used in 7.24.0
  CURLE_OBSOLETE10 = CURLE_FTP_ACCEPT_FAILED,
  CURLE_OBSOLETE12 = CURLE_FTP_ACCEPT_TIMEOUT,

  --  compatibility with older names
--  CURLOPT_ENCODING = CURLOPT_ACCEPT_ENCODING,

  -- The following were added in 7.21.5, April 2011
  CURLE_UNKNOWN_TELNET_OPTION = CURLE_UNKNOWN_OPTION,

  -- The following were added in 7.17.1
  -- These are scheduled to disappear by 2009
  CURLE_SSL_PEER_CERTIFICATE = CURLE_PEER_FAILED_VERIFICATION,

  -- The following were added in 7.17.0
  -- These are scheduled to disappear by 2009
  CURLE_OBSOLETE                    = CURLE_OBSOLETE50,  -- no one should be using this!
  CURLE_BAD_PASSWORD_ENTERED        = CURLE_OBSOLETE46,
  CURLE_BAD_CALLING_ORDER           = CURLE_OBSOLETE44,
  CURLE_FTP_USER_PASSWORD_INCORRECT = CURLE_OBSOLETE10,
  CURLE_FTP_CANT_RECONNECT          = CURLE_OBSOLETE16,
  CURLE_FTP_COULDNT_GET_SIZE        = CURLE_OBSOLETE32,
  CURLE_FTP_COULDNT_SET_ASCII       = CURLE_OBSOLETE29,
  CURLE_FTP_WEIRD_USER_REPLY        = CURLE_OBSOLETE12,
  CURLE_FTP_WRITE_ERROR             = CURLE_OBSOLETE20,
  CURLE_LIBRARY_NOT_FOUND           = CURLE_OBSOLETE40,
  CURLE_MALFORMAT_USER              = CURLE_OBSOLETE24,
  CURLE_SHARE_IN_USE                = CURLE_OBSOLETE57,
  CURLE_URL_MALFORMAT_USER          = CURLE_NOT_BUILT_IN,

  CURLE_FTP_ACCESS_DENIED      = CURLE_REMOTE_ACCESS_DENIED,
  CURLE_FTP_COULDNT_SET_BINARY = CURLE_FTP_COULDNT_SET_TYPE,
  CURLE_FTP_QUOTE_ERROR        = CURLE_QUOTE_ERROR,
  CURLE_TFTP_DISKFULL          = CURLE_REMOTE_DISK_FULL,
  CURLE_TFTP_EXISTS            = CURLE_REMOTE_FILE_EXISTS,
  CURLE_HTTP_RANGE_ERROR       = CURLE_RANGE_ERROR,
  CURLE_FTP_SSL_FAILED         = CURLE_USE_SSL_FAILED,

  -- The following were added earlier

  CURLE_OPERATION_TIMEOUTED     = CURLE_OPERATION_TIMEDOUT,

  CURLE_HTTP_NOT_FOUND          = CURLE_HTTP_RETURNED_ERROR,
  CURLE_HTTP_PORT_FAILED        = CURLE_INTERFACE_FAILED,
  CURLE_FTP_COULDNT_STOR_FILE   = CURLE_UPLOAD_FAILED,

  CURLE_FTP_PARTIAL_FILE        = CURLE_PARTIAL_FILE,
  CURLE_FTP_BAD_DOWNLOAD_RESUME = CURLE_BAD_DOWNLOAD_RESUME,

  -- This was the error code 50 in 7.7.3 and a few earlier versions, this
  --   is no longer used by libcurl but is instead constantd here only to not
  --   make programs break
  CURLE_ALREADY_COMPLETE = 99999,

  -- Provide defines for really old option names
  CURLOPT_INFILE      = 10009,  -- name changed in 7.9.7
  CURLOPT_WRITEHEADER = 10029,

  -- Since long deprecated options with no code in the lib that does anything
  -- with them.
  CURLOPT_WRITEINFO   = 10040,
  CURLOPT_CLOSEPOLICY = 72

-- This prototype applies to all conversion callbacks
--
-- typedef CURLcode (*curl_conv_callback)(char *buffer, size_t length);

-- typedef CURLcode (*curl_ssl_ctx_callback)(CURL *curl,     -- easy handle
--                                 --        void *ssl_ctx,  -- actually an
--                                 --                        -- OpenSSL SSL_CTX
--                                 --        void *userptr);


--
-- Bitmasks for CURLOPT_HTTPAUTH and CURLOPT_PROXYAUTH options:
--
-- CURLAUTH_NONE         - No HTTP authentication
-- CURLAUTH_NEGOTIATE    - HTTP Negotiate (SPNEGO) authentication
-- CURLAUTH_GSSNEGOTIATE - Alias for CURLAUTH_NEGOTIATE (deprecated)
-- CURLAUTH_NTLM         - HTTP NTLM authentication
-- CURLAUTH_NTLM_WB      - HTTP NTLM authentication delegated to winbind helper
-- CURLAUTH_ONLY         - Use together with a single other type to force no
--                         authentication or just that single type
-- CURLAUTH_ANY          - All fine types set
-- CURLAUTH_ANYSAFE      - All fine types except Basic
--/

global constant CURLAUTH_NONE         = #00000000
--*/
global constant CURLAUTH_BASIC        = #00000001   -- HTTP Basic authentication (default)
global constant CURLAUTH_DIGEST       = #00000002   -- HTTP Digest authentication
global constant CURLAUTH_NEGOTIATE    = #00000004   -- HTTP Negotiate (SPNEGO) authentication.
--/*
-- Deprecated since the advent of CURLAUTH_NEGOTIATE
global constant CURLAUTH_GSSNEGOTIATE = CURLAUTH_NEGOTIATE
--*/
global constant CURLAUTH_NTLM         = #00000008   -- HTTP NTLM authentication.
global constant CURLAUTH_DIGEST_IE    = #00000010   -- HTTP Digest authentication with IE flavour
global constant CURLAUTH_NTLM_WB      = #00000020   -- NTLM delegating to winbind helper.
global constant CURLAUTH_BEARER       = #00000040   -- HTTP Bearer token authentication, used primarily in OAuth 2.0 protocol.
global constant CURLAUTH_ONLY         = #80000000
global constant CURLAUTH_ANY          = not CURLAUTH_DIGEST_IE
global constant CURLAUTH_ANYSAFE      = not or_bits(CURLAUTH_BASIC, CURLAUTH_DIGEST_IE)
--/*

global constant CURLSSH_AUTH_ANY       = not 0     -- all types supported by the server
global constant CURLSSH_AUTH_NONE      = 0      -- none allowed, silly but complete
global constant CURLSSH_AUTH_PUBLICKEY = 1 -- global/private key files
global constant CURLSSH_AUTH_PASSWORD  = 2 -- password
global constant CURLSSH_AUTH_HOST      = 4 -- host key files
global constant CURLSSH_AUTH_KEYBOARD  = 8 -- keyboard interactive
global constant CURLSSH_AUTH_AGENT     = 16 -- agent (ssh-agent, pageant...)
global constant CURLSSH_AUTH_DEFAULT   = CURLSSH_AUTH_ANY

global constant CURLGSSAPI_DELEGATION_NONE        = 0      -- no delegation (default)
global constant CURLGSSAPI_DELEGATION_POLICY_FLAG = 1 -- if permitted by policy
global constant CURLGSSAPI_DELEGATION_FLAG        = 2 -- delegate always

global constant
  CURLKHTYPE_UNKNOWN = 0,
  CURLKHTYPE_RSA1    = 1,
  CURLKHTYPE_RSA     = 2,
  CURLKHTYPE_DSS     = 3

--type curl_khtype(integer x)
--  return (x >= CURLKHTYPE_UNKNOWN) and (x <= CURLKHTYPE_DSS)
--end type

-- struct curl_khkey {
--   const char *key; -- points to a zero-terminated string encoded with base64
--                    -- if len is zero, otherwise to the "raw" data
--   size_t len;
--   global enum curl_khtype keytype;
-- };

-- this is the set of return values expected from the curl_sshkeycallback
--    callback
global constant
  CURLKHSTAT_FINE_ADD_TO_FILE = 0,
  CURLKHSTAT_FINE   = 1,
  CURLKHSTAT_REJECT = 2,  -- reject the connection, return an error
  CURLKHSTAT_DEFER  = 3,  -- do not accept it, but we can't answer right now so
                          --   this causes a CURLE_DEFER error but otherwise the
                          --   connection will be left intact etc
  CURLKHSTAT_LAST   = 4   -- not for use, only a marker for last-in-list

-- this is the set of status codes pass in to the callback
global constant
  CURLKHMATCH_OK       = 0,  -- match
  CURLKHMATCH_MISMATCH = 1,  -- host found, key mismatch!
  CURLKHMATCH_MISSING  = 2,  -- no matching host/key found
  CURLKHMATCH_LAST     = 3   -- not for use, only a marker for last-in-list

--type curl_khmatch(integer x)
--  return (x >= CURLKHMATCH_OK) and (x <= CURLKHMATCH_LAST)
--end type

-- typedef int
--   (*curl_sshkeycallback) (CURL *easy,     -- easy handle
--                           const struct curl_khkey *knownkey, -- known
--                           const struct curl_khkey *foundkey, -- found
--                           global enum curl_khmatch, -- libcurl's view on the keys
--                           void *clientp); -- custom pointer passed from app

---- parameter for the CURLOPT_USE_SSL option
--global constant
--  CURLUSESSL_NONE  = 0,  -- do not attempt to use SSL
--  CURLUSESSL_TRY   = 1,  -- try using SSL, proceed anyway otherwise
--  CURLUSESSL_CONTROL = 2,  -- SSL for the control connection or fail
--  CURLUSESSL_ALL   = 3,  -- SSL for all communication or fail
--  CURLUSESSL_LAST  = 4   -- not an option, never use
--
--global type curl_usessl(integer x)
--  return (x >= CURLUSESSL_NONE) and (x <= CURLUSESSL_LAST)
--end type

-- Definition of bits for the CURLOPT_SSL_OPTIONS argument:

-- - ALLOW_BEAST tells libcurl to allow the BEAST SSL vulnerability in the
--    name of improving interoperability with older servers. Some SSL libraries
--    have introduced work-arounds for this flaw but those work-arounds sometimes
--    make the SSL communication fail. To regain functionality with those broken
--    servers, a user can this way allow the vulnerability back.
global constant CURLSSLOPT_ALLOW_BEAST = 1

-- - NO_REVOKE tells libcurl to disable certificate revocation checks for those
--    SSL backends where such behavior is present.
global constant CURLSSLOPT_NO_REVOKE   = 2

-- Backwards compatibility with older names
-- These are scheduled to disappear by 2009

global constant CURLFTPSSL_NONE    = CURLUSESSL_NONE
global constant CURLFTPSSL_TRY     = CURLUSESSL_TRY
global constant CURLFTPSSL_CONTROL = CURLUSESSL_CONTROL
global constant CURLFTPSSL_ALL     = CURLUSESSL_ALL
global constant CURLFTPSSL_LAST    = CURLUSESSL_LAST
-- curl_ftpssl = curl_usessl

-- parameter for the CURLOPT_FTP_SSL_CCC option
global constant
  CURLFTPSSL_CCC_NONE    = 0,  -- do not send CCC
  CURLFTPSSL_CCC_PASSIVE = 1,  -- Let the server initiate the shutdown
  CURLFTPSSL_CCC_ACTIVE  = 2,  -- Initiate the shutdown
  CURLFTPSSL_CCC_LAST    = 3   -- not an option, never use

-- parameter for the CURLOPT_FTPSSLAUTH option
global constant
  CURLFTPAUTH_DEFAULT = 0,  -- let libcurl decide
  CURLFTPAUTH_SSL     = 1,  -- use "AUTH SSL"
  CURLFTPAUTH_TLS     = 2,  -- use "AUTH TLS"
  CURLFTPAUTH_LAST    = 3   -- not an option, never use

-- parameter for the CURLOPT_FTP_CREATE_MISSING_DIRS option
global constant
  CURLFTP_CREATE_DIR_NONE  = 0,  -- do NOT create missing dirs!
  CURLFTP_CREATE_DIR       = 1,  -- (FTP/SFTP) if CWD fails, try MKD and then CWD
                                 -- again if MKD succeeded, for SFTP this does
                                 -- similar magic
  CURLFTP_CREATE_DIR_RETRY = 2,  -- (FTP only) if CWD fails, try MKD and then CWD
                                 -- again even if MKD failed!
  CURLFTP_CREATE_DIR_LAST  = 3   -- not an option, never use

-- parameter for the CURLOPT_FTP_FILEMETHOD option
global constant
  CURLFTPMETHOD_DEFAULT   = 0, -- let libcurl pick
  CURLFTPMETHOD_MULTICWD  = 1, -- single CWD operation for each path part
  CURLFTPMETHOD_NOCWD     = 2, -- no CWD at all
  CURLFTPMETHOD_SINGLECWD = 3, -- one CWD to full dir, then work on file
  CURLFTPMETHOD_LAST      = 4  -- not an option, never use

--global type curl_ftpmethod(integer x)
--  return (x >= CURLFTPMETHOD_DEFAULT) and (x <= CURLFTPMETHOD_LAST)
--end type

-- bitmask defines for CURLOPT_HEADEROPT
global constant CURLHEADER_UNIFIED  = 0
global constant CURLHEADER_SEPARATE = 1

-- CURLPROTO_ defines are for the CURLOPT_*PROTOCOLS options
global constant
  CURLPROTO_HTTP   = #0000001,
  CURLPROTO_HTTPS  = #0000002,
  CURLPROTO_FTP    = #0000004,
  CURLPROTO_FTPS   = #0000008,
  CURLPROTO_SCP    = #0000010,
  CURLPROTO_SFTP   = #0000020,
  CURLPROTO_TELNET = #0000040,
  CURLPROTO_LDAP   = #0000080,
  CURLPROTO_LDAPS  = #0000100,
  CURLPROTO_DICT   = #0000200,
  CURLPROTO_FILE   = #0000400,
  CURLPROTO_TFTP   = #0000800,
  CURLPROTO_IMAP   = #0001000,
  CURLPROTO_IMAPS  = #0002000,
  CURLPROTO_POP3   = #0004000,
  CURLPROTO_POP3S  = #0008000,
  CURLPROTO_SMTP   = #0010000,
  CURLPROTO_SMTPS  = #0020000,
  CURLPROTO_RTSP   = #0040000,
  CURLPROTO_RTMP   = #0080000,
  CURLPROTO_RTMPT  = #0100000,
  CURLPROTO_RTMPE  = #0200000,
  CURLPROTO_RTMPTE = #0400000,
  CURLPROTO_RTMPS  = #0800000,
  CURLPROTO_RTMPTS = #1000000,
  CURLPROTO_GOPHER = #2000000,
  CURLPROTO_SMB    = #4000000,
  CURLPROTO_SMBS   = #8000000,
  CURLPROTO_ALL    = not 0  -- enable everything

--*/
global constant CURLOPT_XFERINFODATA            = CURLOPT_PROGRESSDATA
--/*
global constant CURLOPT_SERVER_RESPONSE_TIMEOUT = CURLOPT_FTP_RESPONSE_TIMEOUT

-- Backwards compatibility with older names
-- These are scheduled to disappear by 2011

-- This was added in version 7.19.1
global constant CURLOPT_POST301 = CURLOPT_POSTREDIR

-- These are scheduled to disappear by 2009

-- The following were added in 7.17.0
global constant CURLOPT_SSLKEYPASSWD = CURLOPT_KEYPASSWD
global constant CURLOPT_FTPAPPEND = CURLOPT_APPEND
global constant CURLOPT_FTPLISTONLY = CURLOPT_DIRLISTONLY
global constant CURLOPT_FTP_SSL = CURLOPT_USE_SSL

-- The following were added earlier

global constant CURLOPT_SSLCERTPASSWD = CURLOPT_KEYPASSWD
global constant CURLOPT_KRB4LEVEL = CURLOPT_KRBLEVEL


  -- Below here follows defines for the CURLOPT_IPRESOLVE option. If a host
  --   name resolves addresses using more than one IP protocol version, this
  --   option might be handy to force libcurl to use a specific IP version.
global constant CURL_IPRESOLVE_WHATEVER = 0 -- default, resolves addresses to all IP
                                    -- versions that your system allows
global constant CURL_IPRESOLVE_V4       = 1 -- resolve to IPv4 addresses
global constant CURL_IPRESOLVE_V6       = 2 -- resolve to IPv6 addresses

  -- three convenient "aliases" that follow the name scheme better
global constant CURLOPT_RTSPHEADER = CURLOPT_HTTPHEADER

  -- These enums are for use with the CURLOPT_HTTP_VERSION option.
global constant
  CURL_HTTP_VERSION_NONE = 0, -- setting this means we don't care, and that we'd
                              -- like the library to choose the best possible for us!
  CURL_HTTP_VERSION_1_0  = 1,  -- please use HTTP 1.0 in the request
  CURL_HTTP_VERSION_1_1  = 2,  -- please use HTTP 1.1 in the request
  CURL_HTTP_VERSION_2_0  = 3,  -- please use HTTP 2 in the request
  CURL_HTTP_VERSION_2TLS = 4,  -- use version 2 for HTTPS, version 1.1 for HTTP
  CURL_HTTP_VERSION_2_PRIOR_KNOWLEDGE = 5,  -- please use HTTP 2 without HTTP/1.1 Upgrade
  CURL_HTTP_VERSION_LAST = 6   -- *ILLEGAL* http version

-- Convenience definition simple because the name of the version is HTTP/2 and
--   not 2.0. The 2_0 version of the global enum name was set while the version was
--   still planned to be 2.0 and we stick to it for compatibility.
global constant CURL_HTTP_VERSION_2 = CURL_HTTP_VERSION_2_0

--
-- Public API enums for RTSP requests
--/
global constant
  CURL_RTSPREQ_NONE          =  0,  -- first in list
  CURL_RTSPREQ_OPTIONS       =  1,
  CURL_RTSPREQ_DESCRIBE      =  2,
  CURL_RTSPREQ_ANNOUNCE      =  3,
  CURL_RTSPREQ_SETUP         =  4,
  CURL_RTSPREQ_PLAY          =  5,
  CURL_RTSPREQ_PAUSE         =  6,
  CURL_RTSPREQ_TEARDOWN      =  7,
  CURL_RTSPREQ_GET_PARAMETER =  8,
  CURL_RTSPREQ_SET_PARAMETER =  9,
  CURL_RTSPREQ_RECORD        = 10,
  CURL_RTSPREQ_RECEIVE       = 11,
  CURL_RTSPREQ_LAST          = 12   -- last in list

  -- These enums are for use with the CURLOPT_NETRC option.
global constant
  CURL_NETRC_IGNORED  = 0,  -- The .netrc will never be read.
                            -- This is the default.
  CURL_NETRC_OPTIONAL = 1,  -- A user:password in the URL will be preferred
                            -- to one in the .netrc.
  CURL_NETRC_REQUIRED = 2,  -- A user:password in the URL will be ignored.
                            -- Unless one is set programmatically, the .netrc
                            -- will be queried.
  CURL_NETRC_LAST     = 3

global constant
  CURL_SSLVERSION_DEFAULT = 0,
  CURL_SSLVERSION_TLSv1   = 1,  -- TLS 1.x
  CURL_SSLVERSION_SSLv2   = 2,
  CURL_SSLVERSION_SSLv3   = 3,
  CURL_SSLVERSION_TLSv1_0 = 4,
  CURL_SSLVERSION_TLSv1_1 = 5,
  CURL_SSLVERSION_TLSv1_2 = 6,
  CURL_SSLVERSION_LAST    = 7   -- never use, keep last

global constant
  CURL_TLSAUTH_NONE = 0,
  CURL_TLSAUTH_SRP  = 1,
  CURL_TLSAUTH_LAST = 2  -- never use, keep last

-- symbols to use with CURLOPT_POSTREDIR.
--   CURL_REDIR_POST_301, CURL_REDIR_POST_302 and CURL_REDIR_POST_303
--   can be bitwise ORed so that CURL_REDIR_POST_301 | CURL_REDIR_POST_302
--   | CURL_REDIR_POST_303 == CURL_REDIR_POST_ALL

global constant
  CURL_REDIR_GET_ALL  = 0,
  CURL_REDIR_POST_301 = 1,
  CURL_REDIR_POST_302 = 2,
  CURL_REDIR_POST_303 = 4,
  CURL_REDIR_POST_ALL = or_all({CURL_REDIR_POST_301, CURL_REDIR_POST_302, CURL_REDIR_POST_303})

global constant
  CURL_TIMECOND_NONE         = 0,
  CURL_TIMECOND_IFMODSINCE   = 1,
  CURL_TIMECOND_IFUNMODSINCE = 2,
  CURL_TIMECOND_LASTMOD      = 3,
  CURL_TIMECOND_LAST         = 4

--------------------------------------------------------------------------------

-- curl_strequal() and curl_strnequal() are subject for removal in a future
--   libcurl, see lib/README.curlx for details
-- CURL_EXTERN int (curl_strequal)(const char *s1, const char *s2);
--constant xcurl_strequal = link_c_func(libcurl, "curl_strequal",
--                          {C_PTR, C_PTR}, C_INT)
--global function curl_strequal(sequence s1, sequence s2)
--  atom addr_s1, addr_s2
--  integer ret
--
--  addr_s1 = allocate_string(s1)
--  addr_s2 = allocate_string(s2)
--  ret = c_func(xcurl_strequal, {addr_s1, addr_s2})
--  free(addr_s1)
--  free(addr_s2)
--  return ret
--end function

--------------------------------------------------------------------------------

-- CURL_EXTERN int (curl_strnequal)(const char *s1, const char *s2, size_t n);
--constant xcurl_strnequal = link_c_func(libcurl, "curl_strnequal",
--                           {C_PTR, C_PTR, C_SIZE_T}, C_INT)
--global function curl_strnequal(sequence s1, sequence s2, atom n)
--  atom addr_s1, addr_s2
--  integer ret
--
--  addr_s1 = allocate_string(s1)
--  addr_s2 = allocate_string(s2)
--  ret = c_func(xcurl_strnequal, {addr_s1, addr_s2, n})
--  free(addr_s1)
--  free(addr_s2)
--  return ret
--end function

--------------------------------------------------------------------------------

global constant
  CURLFORM_NOTHING        =  0,        --******** the first one is unused ***********
  CURLFORM_COPYNAME       =  1,
  CURLFORM_PTRNAME        =  2,
  CURLFORM_NAMELENGTH     =  3,
  CURLFORM_COPYCONTENTS   =  4,
  CURLFORM_PTRCONTENTS    =  5,
  CURLFORM_CONTENTSLENGTH =  6,
  CURLFORM_FILECONTENT    =  7,
  CURLFORM_ARRAY          =  8,
  CURLFORM_OBSOLETE       =  9,
  CURLFORM_FILE           = 10,
  CURLFORM_BUFFER         = 11,
  CURLFORM_BUFFERPTR      = 12,
  CURLFORM_BUFFERLENGTH   = 13,
  CURLFORM_CONTENTTYPE    = 14,
  CURLFORM_CONTENTHEADER  = 15,
  CURLFORM_FILENAME       = 16,
  CURLFORM_END            = 17,
  CURLFORM_OBSOLETE2      = 18,
  CURLFORM_STREAM         = 19,
  CURLFORM_CONTENTLEN     = 20, -- added in 7.46.0, provide a curl_off_t length
  CURLFORM_LASTENTRY      = 21  -- the last unused

--global type CURLformoption(integer n)
--  return ((n >= CURLFORM_NOTHING) and (n <= CURLFORM_LASTENTRY))
--end type

-- structure to be used as parameter for CURLFORM_ARRAY
--
-- struct curl_forms {
--   CURLformoption option;
--   const char                                 --               --value;
-- };

-- use this for multipart formpost building
-- Returns code for curl_formadd()
--
-- Returns:
-- CURL_FORMADD_OK             on success
-- CURL_FORMADD_MEMORY         if the FormInfo allocation fails
-- CURL_FORMADD_OPTION_TWICE   if one option is given twice for one Form
-- CURL_FORMADD_NULL           if a null pointer was given for a char
-- CURL_FORMADD_MEMORY         if the allocation of a FormInfo struct failed
-- CURL_FORMADD_UNKNOWN_OPTION if an unknown option was used
-- CURL_FORMADD_INCOMPLETE     if the some FormInfo is not complete (or error)
-- CURL_FORMADD_MEMORY         if a curl_httppost struct cannot be allocated
-- CURL_FORMADD_MEMORY         if some allocation for string copying failed.
-- CURL_FORMADD_ILLEGAL_ARRAY  if an illegal option is used in an array
--
--**************************************************************************
global constant
  CURL_FORMADD_OK             = 0, -- first, no error
  CURL_FORMADD_MEMORY         = 1,
  CURL_FORMADD_OPTION_TWICE   = 2,
  CURL_FORMADD_NULL           = 3,
  CURL_FORMADD_UNKNOWN_OPTION = 4,
  CURL_FORMADD_INCOMPLETE     = 5,
  CURL_FORMADD_ILLEGAL_ARRAY  = 6,
  CURL_FORMADD_DISABLED       = 7, -- libcurl was built with this disabled
  CURL_FORMADD_LAST           = 8  -- last

--global type CURLFORMcode(integer n)
--  return ((n >= CURL_FORMADD_OK) and (n <= CURL_FORMADD_LAST))
--end type

--------------------------------------------------------------------------------

-- NAME curl_formadd()
--
-- DESCRIPTION
--
-- Pretty advanced global function for building multi-part formposts. Each invoke
-- adds one part that together construct a full post. Then use
-- CURLOPT_HTTPPOST to send it off to libcurl.
--/
--
-- CURL_EXTERN CURLFORMcode curl_formadd(struct curl_httppost **httppost,
--                                 --    struct curl_httppost **last_post,
--                                 --    ...);

--------------------------------------------------------------------------------

-- callback global function for curl_formget()
-- The void *arg pointer will be the one passed as second argument to
--   curl_formget().
-- The character buffer passed to it must not be freed.
-- Should return the buffer length passed to it as the argument "len" on
--   success.

-- typedef size_t (*curl_formget_callback)(void *arg, const char *buf,
--                                 --      size_t len);

--
-- NAME curl_formget()
--
-- DESCRIPTION
--
-- Serialize a curl_httppost struct built with curl_formadd().
-- Accepts a void pointer as second argument which will be passed to
-- the curl_formget_callback global function.
-- Returns 0 on success.
--/
--
-- CURL_EXTERN int curl_formget(struct curl_httppost *form, void *arg,
--                              curl_formget_callback append);

--------------------------------------------------------------------------------

-- NAME curl_formfree()
--
-- DESCRIPTION
--
-- Free a multipart formpost previously built with curl_formadd().
--
-- CURL_EXTERN void curl_formfree(struct curl_httppost *form);

--------------------------------------------------------------------------------

-- NAME curl_getenv()
--
-- DESCRIPTION
--
-- Returns a malloc()'ed string that MUST be curl_free()ed after usage is
-- complete. DEPRECATED - see lib/README.curlx
--/
-- CURL_EXTERN char *curl_getenv(const char *variable);
--constant xcurl_getenv = link_c_func(libcurl, "curl_getenv",
--                        {C_PTR}, C_PTR)
--global function curl_getenv(sequence variable)
--  atom addr, ret
--  sequence s
--
--  addr = allocate_string(variable)
--  ret = c_func(xcurl_getenv, {addr})
--  s = peek_string(ret)
--  free(addr)
--  return s
--end function

--------------------------------------------------------------------------------


-- NAME curl_easy_escape()
--
-- DESCRIPTION
--
-- Escapes URL strings (converts all letters consider illegal in URLs to their
-- %XX versions). This global function returns a new allocated string or NULL if an
-- error occurred.
--/
-- CURL_EXTERN char *curl_easy_escape(CURL *handle, const char *string, int length);
--constant xcurl_easy_escape = link_c_func(libcurl, "curl_easy_escape",
--                             {C_PTR, C_PTR, C_INT}, C_PTR)
--global function curl_easy_escape(atom handle, string s) 
--  atom ret = c_func(xcurl_easy_escape, {handle, s, length(s)})
--  return peek_string(ret)
--end function

--------------------------------------------------------------------------------

-- the previous version:
-- CURL_EXTERN char *curl_escape(const char *string, int length);
--constant xcurl_escape = link_c_func(libcurl, "curl_escape",
--                        {C_PTR, C_INT}, C_PTR)
--global function curl_escape(string s)
--  atom ret = c_func(xcurl_escape, {s, length(s)})
--  return peek_string(ret)
--end function

--------------------------------------------------------------------------------

-- NAME curl_easy_unescape()
--
-- DESCRIPTION
--
-- Unescapes URL encoding in strings (converts all %XX codes to their 8bit
-- versions). This global function returns a new allocated string or NULL if an error
-- occurred.
-- Conversion Note: On non-ASCII platforms the ASCII %XX codes are
-- converted into the host encoding.
--/
-- CURL_EXTERN char *curl_easy_unescape(CURL *handle, const char *string,
--                                 --   int length, int *outlength);
--constant xcurl_easy_unescape = link_c_func(libcurl, "curl_easy_unescape",
--                               {C_PTR, C_PTR, C_INT, C_PTR},
--                               C_PTR)
--global function curl_easy_unescape(atom handle, string s)
--  atom len = allocate(4,1),
--     ret = c_func(xcurl_easy_unescape, {handle, s, length(s), len})
--  return peek_string(ret)
--end function

--------------------------------------------------------------------------------

-- the previous version
-- CURL_EXTERN char *curl_unescape(const char *string, int length);
--constant xcurl_unescape = link_c_func(libcurl, "curl_unescape",
--                          {C_PTR, C_INT}, C_PTR)
--global function curl_unescape(string s)
--  atom ret = c_func(xcurl_unescape, {s, length(s)})
--  return peek_string(ret)
--end function

--------------------------------------------------------------------------------

-- NAME curl_free()
--
-- DESCRIPTION
--
-- Provided for de-allocation in the same translation unit that did the
-- allocation. Added in libcurl 7.10
--/
-- CURL_EXTERN void curl_free(void *p);
--constant xcurl_free = link_c_proc(libcurl, "curl_free",
--                      {C_PTR})
--global procedure curl_free(atom p)
--  c_proc(xcurl_free, {p})
--end procedure

--------------------------------------------------------------------------------

-- NAME curl_global_init_mem()
--
-- DESCRIPTION
--
-- curl_global_init() or curl_global_init_mem() should be invoked exactly once
-- for each application that uses libcurl.  This global function can be used to
-- initialize libcurl and set user defined memory management callback
-- functions.  Users can implement memory management routines to check for
-- memory leaks, check for mis-use of the curl library etc.  User registered
-- callback routines with be invoked by this library instead of the system
-- memory management routines like malloc, free etc.
--/
--
-- CURL_EXTERN CURLcode curl_global_init_mem(long flags,
--                                 --        curl_malloc_callback m,
--                                 --        curl_free_callback f,
--                                 --        curl_realloc_callback r,
--                                 --        curl_strdup_callback s,
--                                 --        curl_calloc_callback c);

--------------------------------------------------------------------------------

-- NAME curl_getdate()
--
-- DESCRIPTION
--
-- Returns the time, in seconds since 1 Jan 1970 of the time string given in
-- the first argument. The time argument in the second parameter is unused
-- and should be set to NULL.
--/
-- CURL_EXTERN time_t curl_getdate(const char *p, const time_t *unused);
--constant xcurl_getdate = link_c_func(libcurl, "curl_getdate",
--                         {C_PTR, C_PTR}, C_PTR)
--global function curl_getdate(string s)
--  atom ret = c_func(xcurl_getdate, {s, NULL})
--  return ret
--end function

--------------------------------------------------------------------------------

-- info about the certificate chain, only for OpenSSL builds. Asked
--    for with CURLOPT_CERTINFO / CURLINFO_CERTINFO
--
-- struct curl_certinfo {
--   int num_of_certs;             -- number of certificates with information
--   struct curl_slist **certinfo; -- for each index in this array, there's a
--                                 -- linked list with textual information in the
--                                 -- format "name: value"
-- };

-- global enum for the different supported SSL backends
global constant
  CURLSSLBACKEND_NONE      =  0,
  CURLSSLBACKEND_OPENSSL   =  1,
  CURLSSLBACKEND_GNUTLS    =  2,
  CURLSSLBACKEND_NSS       =  3,
  CURLSSLBACKEND_OBSOLETE4 =  4,  -- Was QSOSSL.
  CURLSSLBACKEND_GSKIT     =  5,
  CURLSSLBACKEND_POLARSSL  =  6,
  CURLSSLBACKEND_CYASSL    =  7,
  CURLSSLBACKEND_SCHANNEL  =  8,
  CURLSSLBACKEND_DARWINSSL =  9,
  CURLSSLBACKEND_AXTLS     = 10,
  CURLSSLBACKEND_MBEDTLS   = 11

--global type curl_sslbackend(integer n)
--  return ((n >= CURLSSLBACKEND_NONE) and (n <= CURLSSLBACKEND_MBEDTLS))
--end type

-- aliases for library clones and renames
global constant
  CURLSSLBACKEND_LIBRESSL  = 1,
  CURLSSLBACKEND_BORINGSSL = 1,
  CURLSSLBACKEND_WOLFSSL   = 6

-- Information about the SSL library used and the respective internal SSL
--    handle, which can be used to obtain further information regarding the
--    connection. Asked for with CURLINFO_TLS_SSL_PTR or CURLINFO_TLS_SESSION.
--
-- struct curl_tlssessioninfo {
--   curl_sslbackend backend;
--   void *internals;
-- };


-- CURLINFO_RESPONSE_CODE is the new name for the option previously known as
--   CURLINFO_HTTP_CODE
global constant CURLINFO_HTTP_CODE = CURLINFO_RESPONSE_CODE

global constant
  CURLCLOSEPOLICY_NONE                = 0, -- first, never use this
  CURLCLOSEPOLICY_OLDEST              = 1,
  CURLCLOSEPOLICY_LEAST_RECENTLY_USED = 2,
  CURLCLOSEPOLICY_LEAST_TRAFFIC       = 3,
  CURLCLOSEPOLICY_SLOWEST             = 4,
  CURLCLOSEPOLICY_CALLBACK            = 5,
  CURLCLOSEPOLICY_LAST                = 6  -- last, never use this

--------------------------------------------------------------------------------

-- NAME curl_share_strerror()
--
-- DESCRIPTION
--
-- The curl_share_strerror global function may be used to turn a CURLSHcode value
-- into the equivalent human readable error string.  This is useful
-- for printing meaningful error messages.
--/
--
-- CURL_EXTERN const char *curl_share_strerror(CURLSHcode);

--------------------------------------------------------------------------------

-- NAME curl_easy_pause()
--
-- DESCRIPTION
--
-- The curl_easy_pause global function pauses or unpauses transfers. Select the new
-- state by setting the bitmask, use the convenience defines below.
--
--/
-- CURL_EXTERN CURLcode curl_easy_pause(CURL *handle, int bitmask);
--constant xcurl_easy_pause = link_c_func(libcurl, "curl_easy_pause",
--                            {C_PTR, C_INT}, C_INT)
--global function curl_easy_pause(atom handle, integer bitmask)
--  CURLcode res = c_func(xcurl_easy_pause, {handle, bitmask})
--  return res
--end function

global constant CURLPAUSE_RECV      = 1
global constant CURLPAUSE_RECV_CONT = 0

global constant CURLPAUSE_SEND      = 4
global constant CURLPAUSE_SEND_CONT = 0

global constant CURLPAUSE_ALL  = or_bits(CURLPAUSE_RECV, CURLPAUSE_SEND)
global constant CURLPAUSE_CONT = or_bits(CURLPAUSE_RECV_CONT, CURLPAUSE_SEND_CONT)

--------------------------------------------------------------------------------

===============================================================================================================
--procedure euLibCurl(****************************************************************************) end procedure
--
-- euLibCurl version 0.2 (29-Aug-2002)
--
-- A wrapper for Curl (or more specifically LibCurl)
--
-- Copyright (c) Ray Smith 2002
--

--*************************************************
-- Includes
--*************************************************
--include wrapper.ew



--*************************************************
-- Globals
--*************************************************

--atom LibCurlLib = link_dll("libcurl.dll")

-- string_tab holds all option string data
-- It is built automatically but requires easy_curl__cleanup to de-allocate
sequence string_tab = {}

-- memory allocated to convert to atoms to an 8 byte C double
atom a2dp -- atom_2_double_pointer


--*************************************************
-- Routines
--*************************************************


------------------------------
-- atoms_to_double
------------------------------
-- converts to atoms to a float64
-- thanks to Matt Lewis
--global function atoms_to_double( atom a, atom b)
--  poke4( a2dp, {a,b} )
--  return float64_to_atom( peek( {a2dp,8} ) )
--end function

--*/
