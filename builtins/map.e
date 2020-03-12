--
-- builtins\map.e
-- ==============
--
-- Partial compatibility for OpenEuphoria's std\map.e:
--  It is based on builtins\dict.e, rather than hashing.
--  There is no notion of large/small maps,
--  a map (-id) is just a small integer index (as per new_dict), 
--  none of that ADD/SUBTRACT/MULTIPLY/DIVIDE/APPEND/CONCAT/LEAVE nonsense, 
--  no nested_get or nested_put (for which someone should be shot?),
--  no "rehash" (obviously) or "optimise" (simply no need, period), 
--  no compare or copy (would be pretty trivial if needed), 
--  no "new_from_kvpairs" (ditto) or "new_from_string" (yuk), and
--  for_each has no sorted order option (it alway is), nor any of that
--  signal_boundary nonsense (that someone put in and then added the
--  signal_boundary parameter [which this don't have] to get rid of).
--  Also, load_map does not support "bare strings": instead of this=that
--  the input file must contain "this"="that".
--  Offending items have been moved to the .syn "incompatible" section.
--  Finally, while clear(map m) is fine, you should really invoke
--  destroy_dict(map m) when you have no further use for map m.
--
-- Despite such differences, you should be able to achieve anything you can
--  with std\map.e on OpenEuphoria, with this (builtins\map.e) on Phix.
-- Personally, however, I would recommend using builtins\dict.e directly.
--  (You can legally pass the result from load_map as a tid to the dict
--   routines, and use it or a tid from new_dict when invoking save_map.)
--
-- There is NOT even the SLIGHTEST nod to making these things thread-safe,
--  except (perhaps) as documented for the dict.e routines. Certainly the
--  routines pairs/keys/values/for_each/save_map/load_map would all require 
--  an exclusive system-wide lock for their entire duration.
--

--
--  Quick Summary (omitting any "map:" qualifiers):
--  ==============================================
--
--  map m
--  bool r = map(object o)
--  map m = new_map()
--  put(map m, object key, object val)
--  bool b = has(map m, object key)
--  object val = get(map m, object key, object defval=0)
--  remove(map m, object key)
--  sequence full_map = pairs(map m)
--  sequence all_keys = keys(map m)
--  sequence all_vals = values(map m)
--  object res = for_each(map m, integer rid, object user_data=0)
--      [a res of 0 indicates that all items were processed]
--  integer count = size(map m)
--  integer count = save_map(map m, object file)
--  map m = load_map(object file)
--  clear(map m)
--  destroy_dict(map m) -- [part of dict.e]
--
--DOC: map/new_map/put/(get)/has/(remove)/size/pairs/keys/values/for_each/clear/save_map/load_map
-- (add a link from get.e to map.e, for "get", ditto remove)

--
-- Technical note: 
--  The following tweaks were made to get.e in the process of writing this:
--  global function active_ch() [returns ch]
--  global integer get_line_no
--  global procedure skip_blanks() [was local]
--  global constant GET_IGNORE [now in psym.e]
--  function read_comment()
--  function Get() has been replaced with a newer version
--

include dict.e

include get.e as stdget

global type map(object x)
    return is_dict(x)
end type

--17/01/20:
--global function new()
global function new_map()
    return new_dict()
end function

global procedure put(map m, object key, object val)
    putd(key,val,m)
end procedure

global function get(map m, object key, object defval=0)
integer node = getd_index(key,m)
    if node=0 then return defval end if
    return getd_by_index(node,m)
end function

global function has(map m, object key)
    return getd_index(key,m)!=0
end function

global procedure remove(map m, object key)
    deld(key, m)
end procedure

global function size(map m=1)
    return dict_size(m)
end function

sequence res = {}

constant PAIRS = 1, KEYS = 2, VALUES = 3
function visitor(object key, object val, integer pkv)
    res = append(res,iff(pkv=PAIRS?{key,val}:iff(pkv=KEYS?key:val)))
    return 1
end function
constant r_visitor = routine_id("visitor")

global function pairs(map m=1)
    res = {}
    traverse_dict(r_visitor, PAIRS, m)
    return res
end function

global function keys(map m=1)
    res = {}
    traverse_dict(r_visitor, KEYS, m)
    return res
end function

global function values(map m=1)
    res = {}
    traverse_dict(r_visitor, VALUES, m)
    return res
end function

global function for_each(map m, integer rid, object user_data=0)
object ures
    res = pairs(m)
    for i=1 to length(res) do
        ures = call_func(rid, {res[i][1], res[i][2], user_data, i})
        if ures!=0 then
            return ures
        end if
    end for
    return 0
end function

global procedure clear(map m)
-- note: use destroy_dict(m[,0]) when you have no further use for m
    destroy_dict(m,1)
end procedure

integer count = 0

function save_item(object key, object data, integer fn)
    printf(fn,"%s = %s\n",{sprint(key),sprint(data)})
    count += 1
    return 1
end function
constant r_save_item = routine_id("save_item")

global function save_map(map m, object file)
integer fn

    if sequence(file) then
        fn = open(file, "w")
    else
        fn = file
    end if

    if fn<=0 then
        return -1
    end if

    count = 0
    traverse_dict(r_save_item,fn,m)

    if sequence(file) then
        close(fn)
    end if
    return count
end function

function load_fatal(object file)
    if string(file) then
        return sprintf("fatal error loading %s, line %d\n",{file,stdget:get_line_no})
    else
        return sprintf("fatal load_map error, line %d\n",{stdget:get_line_no})
    end if
end function

global function load_map(object file)
-- returns a string error message on failure, otherwise a new map
integer status
object val
object key
integer fn

    if sequence(file) then
        fn = open(file, "r")
    else
        fn = file
    end if
    if fn=-1 then
        return "cannot open "&sprint(file)
    end if

    map m = new_map()

    get_line_no = 1
    while 1 do
        {status,key} = stdget:get(fn)
        if status=GET_EOF then exit end if
        if status=GET_IGNORE and stdget:active_ch()=-1 then exit end if
        if status!=GET_SUCCESS then return load_fatal(file) end if
        stdget:skip_blanks()
        if stdget:active_ch()!='=' then return load_fatal(file) end if
        {status,val} = stdget:get(fn)
        if status!=GET_SUCCESS then return load_fatal(file) end if
        putd(key,val,m)
    end while

    if sequence(file) then
        close(fn)
    end if
    return m
end function

