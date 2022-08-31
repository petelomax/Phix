--
--  builtins\url.e
--  ==============
--
--   not [yet] an autoinclude
--
--(with javascript_semantics) - this is just text processing
global enum URL_PROTOCOL,       -- The protocol of the URL
            URL_HOSTNAME,       -- The hostname of the URL
            URL_PORT,           -- The TCP port that the URL will connect to
            URL_PATH,           -- The protocol-specific pathname of the URL
            URL_USER,           -- The username of the URL
            URL_PASSWORD,       -- The password the URL
            URL_QUERY_STRING,   -- The HTTP query string
            URL_FRAGMENT        -- The #name part

global function url_element_desc(integer idx)
    -- (aside: just rewrite your own, rather than try and fiddle/fret with this,
    --         that is should one or more of the following not be to your liking)
    string res= split("scheme domain port path user password query fragment")[idx]
    return res
end function

global function parse_url(string url)
    string protocol = ""
    object host_name = 0,
           path = 0,
           user_name = 0,
           password = 0,
           query_string = 0,
           fragment = 0,
           port = 0
    integer qs_start = 0,
            pos
    bool authority = false
--,
--       all_done = false

    pos = find('#',url)
    if pos!=0 then
        fragment = url[pos+1..$]
        url = url[1..pos-1]
    end if

    pos = find(':', url)

    if pos=0 then
        return 0
    end if

    protocol = url[1..pos-1]
    pos += 1

    -- Can have a maximum of 2 // before we move into the hostname or possibly 
    -- the path (http://john.com) or (file:///home/jeremy/hello.txt)
    if url[pos]='/' then
        pos += 1
    end if
    if url[pos]='/' then
        pos += 1
        authority = true
    end if
    qs_start = find('?', url, pos)
    if authority 
    and url[pos]!='/' then

        integer at = find('@', url)
        if at then

            integer password_colon = find(':', url, pos)
            if password_colon>0 and password_colon<at then
                -- We have a password too!
                user_name = url[pos..password_colon-1]
                password = url[password_colon+1..at-1]
            else
                -- Just a user name
                user_name = url[pos..at-1]
            end if

            pos = at+1
        end if

        integer first_slash = find('/', url, pos),
                 port_colon = find(':', url, pos),
                   port_end = 0
        while port_colon!=0 do
            if first_slash then
                port_end = first_slash-1
            elsif qs_start then
                port_end = qs_start-1
            else
                port_end = length(url)
            end if
            port = scanf(url[port_colon+1..port_end], "%d")
--          if sequence(port) and length(port)=1 
            if length(port)=1 
            and sequence(port[1]) and length(port[1])=1
            and integer(port[1][1]) then
                {{port}} = port
                exit
            end if
            port = 0
            port_colon = find(':', url, port_colon+1)
        end while

        integer host_end
        if port_colon!=0 then
            host_end = port_colon-1
        elsif first_slash then
            host_end = first_slash-1
        elsif qs_start then
            host_end = qs_start-1
        else
            host_end = length(url)
            -- Nothing more to parse
--          all_done = true
        end if
        host_name = url[pos..host_end]
        if port_end then
            host_end = port_end
        end if
        pos = host_end+1
    end if
--  if not all_done then
    if pos<=length(url) then
        if qs_start=0 then
            path = url[pos..$]
        else

            -- Avoid getting a path when there is none.
            if pos!=qs_start then
                path = url[pos..qs_start-1]
            end if

            pos = qs_start

            query_string = url[qs_start+1..$]

        end if
    end if

--  return {protocol, host_name, port, path, user_name, password, query_string, fragment}
    sequence res = repeat(0,URL_FRAGMENT)
    res[URL_PROTOCOL] = protocol
    res[URL_HOSTNAME] = host_name
    res[URL_PORT] = port
    res[URL_PATH] = path
    res[URL_USER] = user_name
    res[URL_PASSWORD] = password
    res[URL_QUERY_STRING] = query_string
    res[URL_FRAGMENT] = fragment
    return res
end function

global function decode_url(string url)
    integer k = 1
    while k<=length(url) do -- (nb url may shrink)
        if url[k]='+' then
            url[k] = ' ' -- space is a special case, converts into +
        elsif url[k]='%' then
            if k=length(url) then
                -- strip empty percent sign
                url = url[1..$-1]
            else
                integer ch = upper(url[k+1])-'0'
                if ch>9 then ch -= 7 end if
                if k+1=length(url) then
                    url[k] = ch
                    url = url[1..k]
                else
                    url[k] = ch*16
                    ch = upper(url[k+2])-'0'
                    if ch>9 then ch -= 7 end if
                    url[k] += ch
                    url = url[1..k] & url[k+3..$]
                end if
            end if
--      else do nothing if it is a regular char ('0' or 'A' or etc)
        end if
        k += 1
    end while
    return url
end function

