-- (c) Copyright - See License.txt
namespace regex
--/**/NOT PHIX

include std/math.e as math
include std/text.e as text
include std/types.e
include std/flags.e as flags
include std/error.e
include std/search.e

--#withtype integer_array

--****
-- == Regular Expressions
--
-- <<LEVELTOC depth=2>>
--
-- === Introduction
--
-- Regular expressions in Euphoria are based on the PCRE (Perl Compatible Regular Expressions)
-- library created by Philip Hazel.  
--
-- This document will detail the Euphoria interface to Regular Expressions, not really
-- regular expression syntax. It is a very complex subject that many books have been
-- written on. Here are a few good resources online that can help while learning
-- regular expressions.
--
-- * [[EUForum Article -> http://openeuphoria.org/wiki/euwiki.cgi?EuGuide%20Regular%20Expressions ]]
-- * [[Perl Regular Expressions Man Page -> http://perldoc.perl.org/perlre.html]]
-- * [[Regular Expression Library -> http://regexlib.com/]] (user supplied regular
--   expressions for just about any task).
-- * [[WikiPedia Regular Expression Article -> http://en.wikipedia.org/wiki/Regular_expression]]
-- * [[Man page of PCRE in HTML -> http://www.slabihoud.de/software/archives/pcrecompat.html]]
-- === General Use
--
-- Many functions take an optional ##options## parameter. This parameter can be either
-- a single option constant (see [[:Option Constants]]), multiple option constants or'ed
-- together into a single atom or a sequence of options, in which the function will take
-- care of ensuring the are or'ed together correctly.  Options are like their C equivalents
-- with the 'PCRE_' prefix stripped off.  Name spaces disambiguate symbols so we don't
-- need this prefix.
--
-- All strings passed into this library must be either 8-bit per character strings or
-- UTF which uses multiple bytes to encode UNICODE characters. You can
-- use UTF8 encoded UNICODE strings when you pass the UTF8 option.

enum M_PCRE_COMPILE = 68, M_PCRE_FREE, M_PCRE_EXEC, M_PCRE_REPLACE, M_PCRE_ERROR_MESSAGE = 95, M_PCRE_GET_OVECTOR_SIZE = 97

--****
-- === Option Constants

--****
-- Signature:
-- public constant ANCHORED
--
-- Description:
-- Froces matches to be only from the first place it is asked to
-- try to make a search.  
-- In C, this is called PCRE_ANCHORED.
-- This is passed to all routines including [[:new]].

--****
-- Signature:
-- public constant AUTO_CALLOUT
--
-- Description:
-- In C, this is called PCRE_AUTO_CALLOUT.
-- To get the functionality of this flag in EUPHORIA, you can use:
-- [[:find_replace_callback]] without passing this option.
-- This is passed to [[:new]].

--****
-- Signature:
-- public constant BSR_ANYCRLF
--
-- Description:
-- With this option only ASCII new line sequences are recognized as newlines.  Other UNICODE
-- newline sequences (encoded as UTF8) are not recognized as an end of line marker.  
-- This is passed to all routines including [[:new]].

--****
-- Signature:
-- public constant BSR_UNICODE
--
-- Description:
-- With this option any UNICODE new line sequence is recognized as a newline.
-- The UNICODE will have to be encoded as UTF8, however.
-- This is passed to all routines including [[:new]].

--****
-- Signature:
-- public constant CASELESS
--
-- Description:
-- This will make your regular expression matches case insensitive.  With this 
-- flag for example, [a-z] is the same as [A-Za-z].
-- This is passed to [[:new]].

--****
-- Signature:
-- public constant DEFAULT
--
-- Description:
-- This is a value used for not setting any flags at all.  This can be passed to
-- all routines including [[:new]]

--****
-- Signature:
-- public constant DFA_SHORTEST
--
-- Description:
-- This is NOT used by any standard library routine.

--****
-- Signature:
-- public constant DFA_RESTART
--
-- Description:
-- This is NOT used by any standard library routine.

--****
-- Signature:
-- public constant DOLLAR_ENDONLY
--
-- Description: 
-- If this bit is set, a dollar sign metacharacter in the pattern matches only
-- at the end of the subject string. Without this option,  a  dollar sign  also
-- matches  immediately before a newline at the end of the string (but not
-- before any other newlines). Thus you must include the newline character
-- in the pattern before the dollar sign if you want to match a line that contanis
-- a newline character.  
-- The DOLLAR_ENDONLY option  is  ignored if  MULTILINE  is  set.     
-- There is no way to set this option within a pattern.
-- This is passed to [[:new]].

--****
-- Signature:
-- public constant DOTALL
--
-- Description:
-- With this option the '.' character also matches a newline sequence.
-- This is passed to [[:new]].

--****
-- Signature:
-- public constant DUPNAMES
--
-- Description:
-- Allow duplicate names for named subpatterns.
-- Since there is no way to access named subpatterns this flag has no effect.
-- This is passed to [[:new]].

--****
-- Signature:
-- public constant EXTENDED
--
-- Description:
-- Whitespace and characters beginning with a hash mark to the end of the line 
-- in the pattern will be ignored when searching except when the whitespace or hash
-- is escaped or in a character class.
-- This is passed to [[:new]].

--****
-- Signature:
-- public constant EXTRA
--
-- Description:
-- When an alphanumeric follows a backslash(\) has no special meaning an 
-- error is generated.
-- This is passed to [[:new]].

--****
-- Signature:
-- public constant FIRSTLINE
--
-- Description:
-- If PCRE_FIRSTLINE is set, the match must happen before or at the first
-- newline in the subject (though it may continue over the newline).
-- This is passed to [[:new]].

--****
-- Signature:
-- public constant MULTILINE
--
-- Description:
-- When  MULTILINE  it  is set, the "start of line" and "end of line"
-- constructs match immediately following or immediately  before  internal
-- newlines  in  the  subject string, respectively, as well as at the very
-- start and end.  This is passed to [[:new]].

--****
-- Signature:
-- public constant NEWLINE_CR
--
-- Description:
-- Sets CR as the NEWLINE sequence.
-- The NEWLINE sequence will match $
-- when MULTILINE is set.
-- This is passed to all routines including [[:new]].

--****
-- Signature:
-- public constant NEWLINE_LF
--
-- Description:
-- Sets LF as the NEWLINE sequence.
-- The NEWLINE sequence will match $
-- when MULTILINE is set.
-- This is passed to all routines including [[:new]].

--****
-- Signature:
-- public constant NEWLINE_CRLF
--
-- Description:
-- Sets CRLF as the NEWLINE sequence
-- The NEWLINE sequence will match $
-- when MULTILINE is set.
-- This is passed to all routines including [[:new]].

--****
-- Signature:
-- public constant NEWLINE_ANY
--
-- Description:
-- Sets ANY newline sequence as the NEWLINE sequence including
-- those from UNICODE when UTF8 is also set.  The string will have
-- to be encoded as UTF8, however.
-- The NEWLINE sequence will match $
-- when MULTILINE is set.
-- This is passed to all routines including [[:new]].

--****
-- Signature:
-- public constant NEWLINE_ANYCRLF
--
-- Description:
-- Sets ANY newline sequence from ASCII.
-- The NEWLINE sequence will match $
-- when MULTILINE is set.
-- This is passed to all routines including [[:new]].

--****
-- Signature:
-- public constant NOTBOL
--
-- Description:
-- This indicates that beginning of the passed string does **NOT** start
-- at the **B**eginning **O**f a **L**ine (NOTBOL), so a carrot symbol (^) in the 
-- original pattern will not match the beginning of the string. 
-- This is used by routines other than [[:new]].

--****
-- Signature:
-- public constant NOTEOL
--
-- Description:
-- This indicates that end of the passed string does **NOT** end
-- at the **E**nd **O**f a **L**ine (NOTEOL), so a dollar sign ($) in the 
-- original pattern will not match the end of the string. 
-- This is used by routines other than [[:new]].

--****
-- Signature:
-- public constant NO_AUTO_CAPTURE
--
-- Description:
-- Disables capturing subpatterns except when the subpatterns are
-- named.
-- This is passed to [[:new]].

--****
-- Signature:
-- public constant NO_UTF8_CHECK
--
-- Description:
-- Turn off checking for the validity of your UTF string.  Use this
-- with caution.  An invalid utf8 string with this option could **crash**
-- your program.  Only use this if you know the string is a valid utf8 string.
-- See [[:unicode:validate]].
-- This is passed to all routines including [[:new]].

--****
-- Signature:
-- public constant NOTEMPTY
--
-- Description:
-- Here matches of empty strings will not be allowed.  In C, this is PCRE_NOTEMPTY.
-- The pattern: `A*a*` will match "AAAA", "aaaa", and "Aaaa" but not "".
-- This is used by routines other than [[:new]].

--****
-- Signature:
-- public constant PARTIAL
--
-- Description:
-- This option has no effect with these routines.  Refer to the C documentation
-- for what it does in C.
-- In C, this constant is called PCRE_PARTIAL.
-- This is used by routines other than [[:new]].

--****
-- Signature:
-- public constant STRING_OFFSETS
--
-- Description:
-- This is used by [[:matches]] and [[:all_matches]].

--****
-- Signature:
-- public constant UNGREEDY
-- This modifier sets the pattern such that quantifiers are 
-- not greedy by default, but become greedy if followed by a question mark.  
-- 
-- Description:
-- This is passed to [[:new]].

--****
-- Signature:
-- public constant UTF8
--
-- Description:
-- Makes strings passed in to be interpreted as a UTF8 encoded string.
-- This is passed to [[:new]].

public constant
        DEFAULT            = #00000000,
        CASELESS           = #00000001,
        MULTILINE          = #00000002,
        DOTALL             = #00000004,
        EXTENDED           = #00000008,
        ANCHORED           = #00000010,
        DOLLAR_ENDONLY     = #00000020,
        EXTRA              = #00000040,
        NOTBOL             = #00000080,
        NOTEOL             = #00000100,
        UNGREEDY           = #00000200,
        NOTEMPTY           = #00000400,
        UTF8               = #00000800,
        NO_AUTO_CAPTURE    = #00001000,
        NO_UTF8_CHECK      = #00002000,
        AUTO_CALLOUT       = #00004000,
        PARTIAL            = #00008000,
        DFA_SHORTEST       = #00010000,
        DFA_RESTART        = #00020000,
        FIRSTLINE          = #00040000,
        DUPNAMES           = #00080000,
        NEWLINE_CR         = #00100000,
        NEWLINE_LF         = #00200000,
        NEWLINE_CRLF       = #00300000,
        NEWLINE_ANY        = #00400000,
        NEWLINE_ANYCRLF    = #00500000,
        BSR_ANYCRLF        = #00800000,
        BSR_UNICODE        = #01000000,
        STRING_OFFSETS     = #0C000000

constant option_names = {
                         {DEFAULT, "DEFAULT"},
                         {CASELESS, "CASELESS"},
                         {MULTILINE, "MULTILINE"},
                         {DOTALL, "DOTALL"},
                         {EXTENDED, "EXTENDED"},
                         {ANCHORED, "ANCHORED"},
                         {DOLLAR_ENDONLY, "DOLLAR_ENDONLY"},
                         {EXTRA, "EXTRA"},
                         {NOTBOL, "NOTBOL"},
                         {NOTEOL, "NOTEOL"},
                         {UNGREEDY, "UNGREEDY"},
                         {NOTEMPTY, "NOTEMPTY"},
                         {UTF8, "UTF8"},
                         {NO_AUTO_CAPTURE, "NO_AUTO_CAPTURE"},
                         {NO_UTF8_CHECK, "NO_UTF8_CHECK"},
                         {AUTO_CALLOUT, "AUTO_CALLOUT"},
                         {PARTIAL, "PARTIAL"},
                         {DFA_SHORTEST, "DFA_SHORTEST"},
                         {DFA_RESTART, "DFA_RESTART"},
                         {FIRSTLINE, "FIRSTLINE"},
                         {DUPNAMES, "DUPNAMES"},
                         {NEWLINE_CR, "NEWLINE_CR"},
                         {NEWLINE_LF, "NEWLINE_LF"},
                         {NEWLINE_CRLF, "NEWLINE_CRLF"},
                         {NEWLINE_ANY, "NEWLINE_ANY"},
                         {NEWLINE_ANYCRLF, "NEWLINE_ANYCRLF"},
                         {BSR_ANYCRLF, "BSR_ANYCRLF"},
                         {BSR_UNICODE, "BSR_UNICODE"},
                         {STRING_OFFSETS, "STRING_OFFSETS"}
                        }

--****
-- === Error Constants

public constant
        ERROR_NOMATCH        =  (-1),
        ERROR_NULL           =  (-2),
        ERROR_BADOPTION      =  (-3),
        ERROR_BADMAGIC       =  (-4),
        ERROR_UNKNOWN_OPCODE =  (-5),
        ERROR_UNKNOWN_NODE   =  (-5),
        ERROR_NOMEMORY       =  (-6),
        ERROR_NOSUBSTRING    =  (-7),
        ERROR_MATCHLIMIT     =  (-8),
        ERROR_CALLOUT        =  (-9),
        ERROR_BADUTF8        = (-10),
        ERROR_BADUTF8_OFFSET = (-11),
        ERROR_PARTIAL        = (-12),
        ERROR_BADPARTIAL     = (-13),
        ERROR_INTERNAL       = (-14),
        ERROR_BADCOUNT       = (-15),
        ERROR_DFA_UITEM      = (-16),
        ERROR_DFA_UCOND      = (-17),
        ERROR_DFA_UMLIMIT    = (-18),
        ERROR_DFA_WSSIZE     = (-19),
        ERROR_DFA_RECURSE    = (-20),
        ERROR_RECURSIONLIMIT = (-21),
        ERROR_NULLWSLIMIT    = (-22),
        ERROR_BADNEWLINE     = (-23)

public constant error_names = {
                               {ERROR_NOMATCH         ,"ERROR_NOMATCH"},
                               {ERROR_NULL        ,"ERROR_NULL"},
                               {ERROR_BADOPTION   ,"ERROR_BADOPTION"},
                               {ERROR_BADMAGIC    ,"ERROR_BADMAGIC"},
                               {ERROR_UNKNOWN_OPCODE ,"ERROR_UNKNOWN_OPCODE"},
                               {ERROR_UNKNOWN_NODE   ,"ERROR_UNKNOWN_NODE"},
                               {ERROR_NOMEMORY    ,"ERROR_NOMEMORY"},
                               {ERROR_NOSUBSTRING     ,"ERROR_NOSUBSTRING"},
                               {ERROR_MATCHLIMIT      ,"ERROR_MATCHLIMIT"},
                               {ERROR_CALLOUT         ,"ERROR_CALLOUT"},
                               {ERROR_BADUTF8         ,"ERROR_BADUTF8"},
                               {ERROR_BADUTF8_OFFSET ,"ERROR_BADUTF8_OFFSET"},
                               {ERROR_PARTIAL         ,"ERROR_PARTIAL"},
                               {ERROR_BADPARTIAL      ,"ERROR_BADPARTIAL"},
                               {ERROR_INTERNAL    ,"ERROR_INTERNAL"},
                               {ERROR_BADCOUNT    ,"ERROR_BADCOUNT"},
                               {ERROR_DFA_UITEM   ,"ERROR_DFA_UITEM"},
                               {ERROR_DFA_UCOND   ,"ERROR_DFA_UCOND"},
                               {ERROR_DFA_UMLIMIT     ,"ERROR_DFA_UMLIMIT"},
                               {ERROR_DFA_WSSIZE      ,"ERROR_DFA_WSSIZE"},
                               {ERROR_DFA_RECURSE     ,"ERROR_DFA_RECURSE"},
                               {ERROR_RECURSIONLIMIT ,"ERROR_RECURSIONLIMIT"},
                               {ERROR_NULLWSLIMIT     ,"ERROR_NULLWSLIMIT"},
                               {ERROR_BADNEWLINE      ,"ERROR_BADNEWLINE"}
                              }

constant all_options = or_all({
                               DEFAULT         ,
                               CASELESS        ,
                               MULTILINE           ,
                               DOTALL              ,
                               EXTENDED        ,
                               ANCHORED        ,
                               DOLLAR_ENDONLY      ,
                               EXTRA               ,
                               NOTBOL              ,
                               NOTEOL              ,
                               UNGREEDY        ,
                               NOTEMPTY        ,
                               UTF8            ,
                               NO_AUTO_CAPTURE    ,
                               NO_UTF8_CHECK       ,
                               AUTO_CALLOUT    ,
                               PARTIAL         ,
                               DFA_SHORTEST    ,
                               DFA_RESTART     ,
                               FIRSTLINE           ,
                               DUPNAMES        ,
                               NEWLINE_CR          ,
                               NEWLINE_LF          ,
                               NEWLINE_CRLF    ,
                               NEWLINE_ANY     ,
                               NEWLINE_ANYCRLF    ,
                               BSR_ANYCRLF     ,
                               BSR_UNICODE     ,
                               STRING_OFFSETS})


--****
-- === Create/Destroy

--**
-- Regular expression type

public type regex(object o)
    return sequence(o)
end type

--**
-- Regular expression option specification type
--
-- Although the functions do not use this type (they return an error instead),
-- you can use this to check if your routine is receiving something sane.
public type option_spec(object o)
    if atom(o) then
        if not integer(o) then
            return 0
        else
            if (or_bits(o,all_options)!=all_options) then
                return 0
            else
                return 1
            end if
        end if
    elsif integer_array(o) then
        return option_spec(or_all(o))
    else
        return 0
    end if
end type

--**
-- Converts an option spec to a string.
-- 
-- This can be useful for debugging what options were passed in.
-- Without it you have to convert a number to hex and lookup the
-- constants in the source code.
public function option_spec_to_string(option_spec o)
    return flags:flags_to_string(o, option_names)
end function

--**
-- Converts an regex error to a string. 
-- 
-- This can be useful for debugging and even something rough to give to
-- the user incase of a regex failure.  It's preferable to 
-- a numeric literal.
public function error_to_string(integer i)
    if i>=0 or i<-23 then
        return sprintf("%d",{i})
    else
        return vlookup(i, error_names, 1, 2, "Unknown Error")
    end if
end function

--**
-- Return an allocated regular expression
--
-- Parameters:
--   # ##pattern## : a sequence representing a human readable regular expression
--   # ##options## : defaults to [[:DEFAULT]]. See [[:Option Constants]]. 
--
-- Returns:
--   A **regex**, which other regular expression routines can work on or an atom to indicate an 
--   error. If an error, you can call [[:error_message]] to get a detailed error message.
--
-- Comments:
--   This is the only routine that accepts a human readable regular expression. The string is
--   compiled and a [[:regex]] is returned. Analyzing and compiling a regular expression is a
--   costly operation and should not be done more than necessary. For instance, if your application
--   looks for an email address among text frequently, you should create the regular expression
--   as a constant accessible to your source code and any files that may use it, thus, the regular
--   expression is analyzed and compiled only once per run of your application.
--
--   <eucode>
--   -- Bad Example
--  
--   while sequence(line) do
--       re:regex proper_name = re:new("[A-Z][a-z]+ [A-Z][a-z]+")
--       if re:find(proper_name, line) then
--           -- code
--       end if
--   end while
--   </eucode>
--
--   <eucode>
--   -- Good Example
--   constant re_proper_name = re:new("[A-Z][a-z]+ [A-Z][a-z]+")
--   while sequence(line) do
--       if re:find(re_proper_name, line) then
--           -- code
--       end if
--   end while
--   </eucode>
--
-- Example 1:
--   <eucode>
--   include regex.e as re
--   re:regex number = re:new("[0-9]+")
--   </eucode>
--
-- Note:
--   For simple finds, matches or even simple wildcard matches, the built-in Euphoria
--   routines find, [[:eu:match]] and [[:wildcard_match]] are often times easier to use and
--   a little faster. Regular expressions are faster for complex searching/matching.
--
-- See Also:
--   [[:error_message]], [[:find]], [[:find_all]]

public function new(integer_array pattern, option_spec options=DEFAULT)
    if sequence(options) then options = or_all(options) end if

    -- concatenation ensures we really get a new sequence, and don't just use the
    -- one passed in, which could be another regex previously created...this may
    -- be a bug with the refcount/delete_instance/regex code
    return machine_func(M_PCRE_COMPILE, {pattern & "", options})
end function

--**
-- If ##[[:new]]## returns an atom, this function will return a text error message
-- as to the reason.
--
-- Parameters:
--   # ##re##: Regular expression to get the error message from
--
-- Returns:
--   An atom (0) when no error message exists, otherwise a sequence describing the error.
--
-- Example 1:
-- <eucode>
-- object r = regex:new("[A-Z[a-z]*")
-- if atom(r) then
--   printf(1, "Regex failed to compile: %s\n", { regex:error_message(r) })
-- end if
-- </eucode>
--

public function error_message(object re)
    return machine_func(M_PCRE_ERROR_MESSAGE, {re})
end function

--****
-- === Utility Routines
-- 

--**
-- Escape special regular expression characters that may be entered into a search
-- string from user input.
--
-- Notes:
--   Special regex characters are:
--       {{{
--   . \ + * ? [ ^ ] $ ( ) { } = ! < > | : -
--       }}}
--       
-- Parameters:
--   # ##s##: string sequence to escape
--       
-- Returns:
--   An escaped ##sequence## representing ##s##.
--       
-- Example 1:
-- <eucode>
-- sequence search_s = escape("Payroll is $***15.00")
-- -- search_s = "Payroll is \\$\\*\\*\\*15\\.00"
-- </eucode>
--

public function escape(integer_array s)
    return text:escape(s, ".\\+*?[^]$(){}=!<>|:-")
end function

--**
-- Returns the number of capturing subpatterns (the ovector size) for a regex
--
-- Parameters:
--   # ##ex## : a regex
--   # ##maxsize## : optional maximum number of named groups to get data from
--
-- Returns:
--   An **integer**
--

public function get_ovector_size(regex ex, integer maxsize=0)

integer m = machine_func(M_PCRE_GET_OVECTOR_SIZE, {ex})
    if (m>maxsize) then
        return maxsize
    end if
    return m+1
end function

--****
-- === Find/Match

--**
-- Find the first match of ##re## in ##haystack##. You can optionally start at the position
-- ##from##.
--
-- Parameters:
--   # ##re## : a regex for a subject to be matched against
--   # ##haystack## : a string in which to searched
--   # ##from## : an integer setting the starting position to begin searching from. Defaults to 1
--   # ##options## : defaults to [[:DEFAULT]]. See [[:Option Constants]].  The only options that
--     may be set when calling find are [[:ANCHORED]], [[:NEWLINE_CR]], [[:NEWLINE_LF]],
--     [[:NEWLINE_CRLF]], [[:NEWLINE_ANY]] [[:NEWLINE_ANYCRLF]] [[:NOTBOL]], [[:NOTEOL]], 
--     [[:NOTEMPTY]], [[:NOTEMPTY_ATSTART]], [[:NO_START_OPTIMIZE]], [[:NO_UTF8_CHECK]], [[:PARTIAL_SOFT]],
--     and [[:PARTIAL_HARD]]
--   # ##size## : internal (how large an array the C backend should allocate). Defaults to 90, in rare cases this number may need to be increased in order to accomodate complex regex expressions.
--
-- Returns:
--   An **object**, which is either an atom of 0, meaning nothing found or a sequence of matched pairs.
--   For the explanation of the returned sequence, please see the first example.
--
-- Example 1:
--   <eucode>
--   r = re:new("([A-Za-z]+) ([0-9]+)") -- John 20 or Jane 45
--   object result = re:find(r, "John 20")
--
--   -- The return value will be:
--   -- {
--   --    { 1, 7 }, -- Total match
--   --    { 1, 4 }, -- First grouping "John" ([A-Za-z]+)
--   --    { 6, 7 }  -- Second grouping "20" ([0-9]+)
--   -- }
--   </eucode>
--

--/**/  --Phix (untested!!) [DEV]
--/**/  global function find(regex re, integer_array haystack, integer from=1, option_spec options=DEFAULT, integer size = 0)
--/**/          if sequence(options) then options = or_all(options) end if
--/**/          if size=0 then
--/**/              size = get_ovector_size(re, 30)
--/**/          end if
--/**/          if size<0 then
--/**/              size = 0
--/**/          end if
--/**/
--/**/          return machine_func(M_PCRE_EXEC, {re, haystack, options, from, size})
--/**/  end function
--/* --RDS Eu:
public function find(regex re, integer_array haystack, integer from=1, option_spec options=DEFAULT, integer size = get_ovector_size(re, 30))
    if sequence(options) then options = or_all(options) end if
    if size < 0 then
        size = 0
    end if
        
    return machine_func(M_PCRE_EXEC, { re, haystack, options, from, size })
end function
--*/
--**
-- Find all occurrences of ##re## in ##haystack## optionally starting at the sequence position
-- ##from##.
--
-- Parameters:
--   # ##re## : a regex for a subject to be matched against
--   # ##haystack## : a string in which to searched
--   # ##from## : an integer setting the starting position to begin searching from. Defaults to 1
--   # ##options## : defaults to [[:DEFAULT]]. See [[:Option Constants]]. 
--
-- Returns:
--   A **sequence**, of matches. Please see [[:find]] for a detailed description of the return
--   value.
--
-- Example 1:
--   <eucode>
--   constant re_number = re:new("[0-9]+")
--   object matches = re:find_all(re_number, "10 20 30")
--
--   -- matches is:
--   -- {
--   --     {1, 2},
--   --     {4, 5},
--   --     {7, 8}
--   -- }
--   </eucode>
--

public function find_all(regex re, integer_array haystack, integer from=1, option_spec options=DEFAULT)
object result
sequence results = {}
    if sequence(options) then options = or_all(options) end if

    while 1 do
        result = find(re, haystack, from, options)
        if not sequence(result) then exit end if
        results = append(results, result)
        from = max(result)+1

        if from>length(haystack) then
            exit
        end if
    end while

    return results
end function

--**
-- Determine if ##re## matches any portion of ##haystack##.
--
-- Parameters:
--   # ##re## : a regex for a subject to be matched against
--   # ##haystack## : a string in which to searched
--   # ##from## : an integer setting the starting position to begin searching from. Defaults to 1
--   # ##options## : defaults to [[:DEFAULT]]. See [[:Option Constants]]. 
--
-- Returns:
--   An **atom**, 1 if ##re## matches any portion of ##haystack## or 0 if not.
--

public function has_match(regex re, integer_array haystack, integer from=1, option_spec options=DEFAULT)
    return sequence(find(re, haystack, from, options))
end function

--**
-- Determine if the entire ##haystack## matches ##re##.
--
-- Parameters:
--   # ##re## : a regex for a subject to be matched against
--   # ##haystack## : a string in which to searched
--   # ##from## : an integer setting the starting position to begin searching from. Defaults to 1
--   # ##options## : defaults to [[:DEFAULT]]. See [[:Option Constants]]. 
--
-- Returns:
--   An **atom**,  1 if ##re## matches the entire ##haystack## or 0 if not.
--

public function is_match(regex re, integer_array haystack, integer from=1, option_spec options=DEFAULT)
object m = find(re, haystack, from, options)

    if sequence(m) and length(m)>0 and m[1][1]=1 and m[1][2]=length(haystack) then
        return 1
    end if

    return 0
end function

--**
-- Get the matched text only.
--
-- Parameters:
--   # ##re## : a regex for a subject to be matched against
--   # ##haystack## : a string in which to searched
--   # ##from## : an integer setting the starting position to begin searching from. Defaults to 1
--   # ##options## : defaults to [[:DEFAULT]]. See [[:Option Constants]]. 
--
-- Returns:
--   Returns a **sequence**, of strings, the first being the entire match and subsequent
--   items being each of the captured groups. The size of the sequence is the number
--   of groups in the expression plus one (for the entire match).
--
--   If ##options## contains the bit [[:STRING_OFFSETS]], then the result is different.
--   For each item, a sequence is returned containing the matched text, the starting
--   index in ##haystack## and the ending index in ##haystack##.
--
-- Example 1:
--   <eucode>
--   constant re_name = re:new("([A-Z][a-z]+) ([A-Z][a-z]+)")
--
--   object matches = re:matches(re_name, "John Doe and Jane Doe")
--   -- matches is:
--   -- {
--   --   "John Doe", -- full match data
--   --   "John",     -- first group
--   --   "Doe"       -- second group
--   -- }
--
--   matches = re:matches(re_name, "John Doe and Jane Doe", STRING_OFFSETS)
--   -- matches is:
--   -- {
--   --   { "John Doe", 1, 8 }, -- full match data
--   --   { "John",     1, 4 }, -- first group
--   --   { "Doe",      6, 8 }  -- second group
--   -- }
--   </eucode>
--
-- See Also:
--   [[:all_matches]]
--
public function matches(regex re, integer_array haystack, integer from=1, option_spec options=DEFAULT)
integer str_offsets
object match_data
sequence tmp

    if sequence(options) then options = or_all(options) end if
    str_offsets = and_bits(STRING_OFFSETS, options)
    match_data = find(re, haystack, from, and_bits(options, not_bits(STRING_OFFSETS)))

    if atom(match_data) then return ERROR_NOMATCH end if

    for i=1 to length(match_data) do
        if match_data[i][1]=0 then
            tmp = ""
        else
            tmp = haystack[match_data[i][1]..match_data[i][2]]
        end if
        if str_offsets then
            match_data[i] = {tmp, match_data[i][1], match_data[i][2]}
        else
            match_data[i] = tmp
        end if
    end for

    return match_data
end function

--**
-- Get the text of all matches
-- 
-- Parameters:
--   # ##re## : a regex for a subject to be matched against
--   # ##haystack## : a string in which to searched
--   # ##from## : an integer setting the starting position to begin searching from. Defaults to 1
--   # ##options## : options, defaults to [[:DEFAULT]]. See [[:Option Constants]].
--
-- Returns:
--   Returns a **sequence**, of a sequence of strings, the first being the entire match and
--   subsequent items being each of the captured groups. The size of the sequence is
--   the number of groups in the expression plus one (for the entire match).
--
--   If ##options## contains the bit [[:STRING_OFFSETS]], then the result is different.
--   For each item, a sequence is returned containing the matched text, the starting
--   index in ##haystack## and the ending index in ##haystack##.
--
-- Example 1:
--   <eucode>
--   constant re_name = re:new("([A-Z][a-z]+) ([A-Z][a-z]+)")
--
--   object matches = re:match_all(re_name, "John Doe and Jane Doe")
--   -- matches is:
--   -- {
--   --   {             -- first match
--   --     "John Doe", -- full match data
--   --     "John",     -- first group
--   --     "Doe"       -- second group
--   --   },
--   --   {             -- second match
--   --     "Jane Doe", -- full match data
--   --     "Jane",     -- first group
--   --     "Doe"       -- second group
--   --   }
--   -- }
--
--   matches = re:match_all(re_name, "John Doe and Jane Doe", STRING_OFFSETS)
--   -- matches is:
--   -- {
--   --   {                         -- first match
--   --     { "John Doe",  1,  8 }, -- full match data
--   --     { "John",      1,  4 }, -- first group
--   --     { "Doe",       6,  8 }  -- second group
--   --   },
--   --   {                         -- second match
--   --     { "Jane Doe", 14, 21 }, -- full match data
--   --     { "Jane",     14, 17 }, -- first group
--   --     { "Doe",      19, 21 }  -- second group
--   --   }
--   -- }
--   </eucode>
--
-- See Also:
--   [[:matches]]

public function all_matches(regex re, integer_array haystack, integer from=1, option_spec options=DEFAULT)
integer str_offsets
object match_data
sequence tmp

    if sequence(options) then options = or_all(options) end if
    str_offsets = and_bits(STRING_OFFSETS, options)
    match_data = find_all(re, haystack, from, and_bits(options, not_bits(STRING_OFFSETS)))

    if length(match_data)=0 then return ERROR_NOMATCH end if

    for i=1 to length(match_data) do
        for j=1 to length(match_data[i]) do
            tmp = haystack[match_data[i][j][1]..match_data[i][j][2]]
            if str_offsets then
                match_data[i][j] = {tmp, match_data[i][j][1], match_data[i][j][2]}
            else
                match_data[i][j] = tmp
            end if
        end for
    end for

    return match_data
end function

--****
-- === Splitting

--**
-- Split a string based on a regex as a delimiter
--
-- Parameters:
--   # ##re## : a regex which will be used for matching
--   # ##text## : a string on which search and replace will apply
--   # ##from## : optional start position
--   # ##options## : options, defaults to [[:DEFAULT]]. See [[:Option Constants]].
--
-- Returns:
--   A **sequence**, of string values split at the delimiter.
--   
-- Example 1:
-- <eucode>
-- regex comma_space_re = re:new(`,\s`)
-- sequence data = re:split(comma_space_re, "euphoria programming, source code, reference data")
-- -- data is
-- -- {
-- --   "euphoria programming",
-- --   "source code",
-- --   "reference data"
-- -- }
-- </eucode>
-- 

public function split(regex re, integer_array text, integer from=1, option_spec options=DEFAULT)
    return split_limit(re, text, 0, from, options)
end function

public function split_limit(regex re, integer_array text, integer limit=0, integer from=1, option_spec options=DEFAULT)
sequence match_data
integer last = 1
sequence result

    if sequence(options) then options = or_all(options) end if
    match_data = find_all(re, text, from, options)

    if limit=0 then
        limit = length(match_data)
    end if

    result = repeat(0, limit)

    for i=1 to limit do
        result[i] = text[last..match_data[i][1][1]-1]
        last = match_data[i][1][2]+1
    end for

    if last<length(text) then
        result &= {text[last..$]}
    end if

    return result
end function

--****
-- === Replacement
--

--**
-- Replaces all matches of a regex with the replacement text.
--
-- Parameters:
--   # ##re## : a regex which will be used for matching
--   # ##text## : a string on which search and replace will apply
--   # ##replacement## : a string, used to replace each of the full matches found
--   # ##from## : optional start position
--   # ##options## : options, defaults to [[:DEFAULT]]
--
-- Returns:
--   A **sequence**, the modified ##text##.
--
-- Special replacement operators:
-- 
-- * **##\##**  ~-- Causes the next character to lose its special meaning. 
-- * **##\n##** ~ -- Inserts a 0x0A (LF) character. 
-- * **##\r##** ~-- Inserts a 0x0D (CR) character. 
-- * **##\t##** ~-- Inserts a 0x09 (TAB) character. 
-- * **##\1##** to **##\9##** ~-- Recalls stored substrings from registers (\1, \2, \3, to \9).
-- * **##\0##** ~-- Recalls entire matched pattern. 
-- * **##\u##** ~-- Convert next character to uppercase 
-- * **##\l##** ~-- Convert next character to lowercase 
-- * **##\U##** ~-- Convert to uppercase till ##\E## or ##\e## 
-- * **##\L##** ~-- Convert to lowercase till ##\E## or ##\e##
-- * **##\E##** or **##\e##** ~-- Terminate a ##{{{\\}}}U## or ##\L## conversion
--
-- Example 1:
-- <eucode>
-- regex r = new(`([A-Za-z]+)\.([A-Za-z]+)`)
-- sequence details = find_replace(r, "hello.txt", `Filename: \U\1\e Extension: \U\2\e`)
-- -- details = "Filename: HELLO Extension: TXT"
-- </eucode>
--

public function find_replace(regex ex, integer_array text, sequence replacement, integer from=1,
                option_spec options=DEFAULT)
    return find_replace_limit(ex, text, replacement, -1, from, options)
end function

--**
-- Replaces up to ##limit## matches of ##ex## in ##text##.
--
-- This function is identical to [[:find_replace]] except it allows you to limit the number of
-- replacements to perform. Please see the documentation for [[:find_replace]] for all the
-- details.
--
-- Parameters:
--   # ##re## : a regex which will be used for matching
--   # ##text## : a string on which search and replace will apply
--   # ##replacement## : a string, used to replace each of the full matches found
--   # ##limit## : the number of matches to process
--   # ##from## : optional start position
--   # ##options## : options, defaults to [[:DEFAULT]]
--
-- Returns:
--   A **sequence**, the modified ##text##.
--
-- See Also:
--   [[:find_replace]]
--

public function find_replace_limit(regex ex, integer_array text, sequence replacement,
                        integer limit, integer from=1, option_spec options=DEFAULT)
    if sequence(options) then options = or_all(options) end if

--DEV
    return machine_func(M_PCRE_REPLACE, {ex, text, replacement, options, from, limit})
end function

--**
-- Replaces up to ##limit## matches of ##ex## in ##text## with the result of a user
-- defined callback. The callback should take one sequence which will contain a string
-- representing the entire match and also a integer_array for every group within the regular
-- expression.
--
-- Parameters:
--   # ##re## : a regex which will be used for matching
--   # ##text## : a string on which search and replace will apply
--   # ##rid## : routine id to execute for each match
--   # ##limit## : the number of matches to process
--   # ##from## : optional start position
--   # ##options## : options, defaults to [[:DEFAULT]]
--
-- Returns:
--   A **sequence**, the modified ##text##.
--
-- Example 1:
-- <eucode>
-- function my_convert(sequence params)
--     switch params[1] do
--         case "1" then 
--             return "one "
--         case "2" then
--             return "two "
--         case else
--             return "unknown "
--     end switch
-- end function
--
-- regex r = re:new(`\d`)
-- sequence result = re:find_replace_callback(r, "125", routine_id("my_convert"))
-- -- result = "one two unknown "
-- </eucode>
--
public function find_replace_callback(regex ex, string text, integer rid, integer limit=0,
                integer from=1, option_spec options=DEFAULT)
sequence match_data, replace_data
sequence params

    if sequence(options) then
        options = math:or_all(options)
    end if
    match_data = find_all(ex, text, from, options)

    if limit=0 or limit>length(match_data) then
        limit = length(match_data)
    end if
    replace_data = repeat(0, limit)

    for i=1 to limit do
        params = repeat(0, length(match_data[i]))
        for j=1 to length(match_data[i]) do
            if equal(match_data[i][j],{0,0}) then
                params[j] = 0
            else
                params[j] = text[match_data[i][j][1]..match_data[i][j][2]]
            end if
        end for

        replace_data[i] = call_func(rid, {params})
    end for

    for i=limit to 1 by -1 do
--/**/  text[match_data[i][1][1]..match_data[i][1][2]] = replace_data[i]                --/*
        text = replace(text, replace_data[i], match_data[i][1][1], match_data[i][1][2]) --*/
    end for

    return text
end function
