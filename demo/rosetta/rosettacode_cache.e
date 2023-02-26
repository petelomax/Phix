--
-- demo\rosetta\rosettacode_cache.e
-- ================================
--
-- Common routines for handling rc_cache etc.
--
-- Usage summary:
--      T: get_rc_category(PT/DT) [1592 tasks as of 29/12/22]
--      L: get_rc_category(LANG) [1592 for Phix as of 29/12/22]
--      R: get_rc_raw(TASK)
--      H: get_rc_htm(TASK)
--      I: get_rc_hist(TASK)
--      S: get_rc_stitched(category)
--      U: get_rc_lang_users()
--      f: file_clean(TASK)
--      h: html_clean(TASK)
--      r: directly reading a .raw file from the cache
--      t: text_clean()
-- 1. Count_examples.exw : TLRf
-- 2. Find_bare_lang_tags.exw (part 2) : TRfh
-- 3. Find_unimplemented_tasks.exw : LTf
-- 4. Fix_code_tags.exw : (none, and never did)
-- 5. List_task_authors.exw : TIf
-- 6. Rank_Languages.exw : Sf
-- 7. Rank_Languages_by_users.exw : U
-- 8. Run_examples.exw : TRrh
-- 9. Tasks_without_examples.exw : TLHht
--
-- See also web_scrape.exw for a really easy way to just "grab one thing".
--
without js -- (libcurl, file i/o, peek..)
include builtins\timedate.e
local constant day = timedelta(days:=1) -- (day == 24*60*60)
global integer refresh_cache = 21*day -- 0 for always [NB refresh_cache += day below]

-- Note the following (xx_clean()*4) were all built on an ad-hoc and as needed basis.
--  (So don't be expecting too much in the way of sound logic or certainties here.)

local constant {hhex,hasc} = columnize({{`%27`,`'`},
                                        {`%2A`,`*`},
                                        {`%2B`,`+`},
                                        {`&quot;`,`"`},
                                        {`&#039;`,`'`}})
global function html_clean(string s)
    -- url->readable: just a few minor fixups, usually pretty much ok anyway
    return substitute_all(s,hhex,hasc)
end function

local constant {uhex,uasc} = columnize({{` `,`%20`},
                                        {`+`,`%2B`},
                                        {"\xE2\x80\x93",`%E2%80%93`},
                                        {`'`,`%27`},
                                        {`*`,`%2A`}})
local function url_clean(string s)
    -- Just some fairly standard replacements for some offending url characters.
    -- (probably fair to say it should undo html_clean(), ish, as I said, ad-hoc)
    return substitute_all(s,uhex,uasc)
end function

local constant {fhex,fasc} = columnize({{`*`,`%2A`},
                                        {`"`,`%22`},
                                        {`:`,`%3A`},
                                        {` `,`_`},
                                        {x"C3A0",`a`},
                                        {x"C3A6",`ae`},
                                        {x"C3A8",`e`},
                                        {x"C3A9",`e`},
                                        {x"C3B6",`o`},
                                        {x"C58D",`o`},
                                        {x"C591",`o`},
                                        {x"CE9C",`M`},
                                        {x"D09A",`K`},
                                        {x"D09C",`M`},
                                        {x"E28093",`-`},
                                        {x"E28099",`'`},
                                        -- (the langage "<gunk>/Uyir" ==>"Uyir":)
                                        {x"E0AE89E0AEAFE0AEBFE0AEB0E0AF8D2F",``}})
global function file_clean(string s)
    -- url -> filename: '*' must be replaced (with %2A),
    --                  '"' must be replaced (with %22),
    --                  ':' must be replaced (with %3A),
    --              and I choose to replace ' ' with '_',
    --          plus a few unicode chars that peeve me.
    -- for examples just look in /rc_cache/*
    return substitute_all(s,fhex,fasc)
end function

-- tip: google "html &#160;" disney work, use "html &#xA0;" instead.
local constant {thex,tasc} = columnize({{`&#160;`,` `}, -- (aka &nbsp;)
                                        {`&#183;`,`.`}, -- (aka &middot;)
                                        {`&#215;`,`*`}, -- (aka &times;)
                                        {`&#39;`,`'`}, -- (aka &apos;)
                                        {`&#47;`,`/`}, -- (aka &sol;)
                                        {`&#60;`,`<`}, -- (aka &lt;)
                                        {`&#61;`,`=`}, -- (aka &equals;)
                                        {`&#8202;`,` `}, -- (aka &VeryThinSpace;)
                                        {`&#8211;`,`-`}, -- (aka &ndash;)
                                        {`&#8212;`,`-`}, -- (aka &mdash;)
                                        {`&#8216;`,`'`}, -- (aka &OpenCurlyQuote;)
                                        {`&#8217;`,`'`}, -- (aka &CloseCurlyQuote;)
                                        {`&#8230;`,`...`}, -- (aka &mldr;)
                                        {`&#8242;`,`'`}, -- (aka &prime;)
                                        {`&#8594;`,`->`}, -- (aka &rarr;)
                                        {`&#8722;`,`-`}, -- (aka &minus;)
                                        {`&#8734;`,`(infinity)`}, -- (aka &infin;)
                                        {`&#8801;`,`===`}, -- (aka &equiv;)
                                        {`&#8804;`,`<=`}, -- (aka &leq;)
                                        {`&#91;`,`[`}, -- (aka &lbrack;)
                                        {`&#93;`,`]`}, -- (aka &rbrack;)
                                        {`&#947;`,`(gamma)`}, -- (aka &gamma;)
                                        {`&#9657;`,`* `}, -- (white right-pointing small triangle)
                                        {`&amp;`,`&`},
                                        {`&quot;`,`"`},
                                        {`&lt;`,`<`},
                                        {`&gt;`,`>`},
                                        {`&#x00D7;`,`*`}, -- (aka &times;)
                                        {`&#x00AF;`,`(macron)`}, -- (aka &macr;)
                                        {`&#x03B1;`,`aplha`}, -- (aka &alpha;)
                                        {`&#x2026;`,`...`}, -- (aka &mldr;)
                                        {`&#x2061;`,`(function application)`},
                                        {`&#x2211;`,`(n_ary summation)`}, -- (aka &sum;)
                                        {`&#x2212;`,`-`}, -- (aka &minus;)
                                        {`&#x2217;`,`*`}, -- (aka &lowast;)
                                        {`&#x221E;`,`(infinity)`}, -- (aka &infin;)
                                        {`&#x2265;`,`>=`}, -- (aka &ge;)
                                        {`&#x22C5;`,`(dot operator)`}, -- (aka &sdot;)
                                        {`&#x22EF;`,`...`},
                                        {`&#x3A3;`,`(Sigma)`}, -- (aka &Sigma;)
                                        {`&#x3BB;`,`(lambda)`}, -- (aka &lambda;)
-- stripping unicode completely... (if using a windows console)
                                        {x"C2A2",`c`}, -- (aka &cent;)
--<snip> distro version goes a bit overboard here...
                                        {x"C2AA",`a`}, -- (aka &ordf;)
                                        {x"C2B1",`+/-`}, -- (aka &plusmn;)
                                        {x"C2B2",`^2`}, -- (aka &sup2;)
                                        {x"C2B3",`^3`}, -- (aka &sup3;)
                                        {x"C2B7",`.`}, -- (aka &middot;)
                                        {x"C2B9",`^1`}, -- (aka &sup1;)
                                        {x"C2B0",`(degrees)`}, -- (aka &deg;)
                                        {x"C2BA",`(degrees?)`}, -- (aka &ordm;)
                                        {x"C2BC",`1/4`}, -- (aka &frac14;)
                                        {x"C2BD",`1/2`}, -- (aka &frac12;)
                                        {x"C2BE",`3/4`}, -- (aka &frac34;)
                                        {x"C387",`C`}, -- (aka &Ccedil;)
                                        {x"C396",`O`}, -- (aka &Ouml;)
                                        {x"C397",`*`}, -- (aka &times;)
                                        {x"C39C",`U`}, -- (aka &Uuml;)
                                        {x"C3A0",`a`}, -- (aka &agrave;)
                                        {x"C3A1",`a`}, -- (aka &aacute;)
                                        {x"C3A2",`a`}, -- (aka &acirc;)
                                        {x"C3A3",`a`}, -- (aka &atilde;)
                                        {x"C3A4",`a`}, -- (aka &auml;)
                                        {x"C3A5",`a`}, -- (aka &aring;)
                                        {x"C3A6",`ae`}, -- (aka &aelig;)
                                        {x"C3A7",`c`}, -- (aka &ccedil;)
                                        {x"C3A8",`e`}, -- (aka &egrave;)
                                        {x"C3A9",`e`}, -- (aka &eacute;)
                                        {x"C3AA",`e`}, -- (aka &ecirc;)
                                        {x"C3AB",`e`}, -- (aka &euml;)
                                        {x"C3AC",`i`}, -- (aka &igrave;)
                                        {x"C3AD",`i`}, -- (aka &iacute;)
                                        {x"C3AE",`i`}, -- (aka &icirc;)
                                        {x"C3AF",`i`}, -- (aka &iuml;)
                                        {x"C3B1",`n`}, -- (aka &ntilde;)
                                        {x"C3B2",`o`}, -- (aka &ograve;)
                                        {x"C3B3",`o`}, -- (aka &oacute;)
                                        {x"C3B4",`o`}, -- (aka &ocirc;)
                                        {x"C3B6",`o`}, -- (aka &ouml;)
                                        {x"C3B8",`o`}, -- (aka &oslash;)
                                        {x"C3B9",`o`}, -- (aka &ugrave;)
                                        {x"C3BA",`u`}, -- (aka &uacute;)
                                        {x"C3BB",`u`}, -- (aka &ucirc;)
                                        {x"C3BC",`u`}, -- (aka &uuml;)
                                        {x"C3BD",`y`}, -- (aka &yacute;)
                                        {x"C3BF",`y`}, -- (aka &yuml;)
                                        {x"C483",`(latin a with breve)`},
                                        {x"C493",`(latin e with macron)`},
                                        {x"C591",`o`},
                                        {x"C692",`f`}, -- (aka &fnof;)
                                        {x"C99B",`(latin small letter open e)`},
                                        {x"CE98",`(Theta)`}, -- (aka &Theta;)
                                        {x"CEA3",`(Sigma)`}, -- (aka &Sigma;)
                                        {x"CEB1",`(aplha)`}, -- (aka &alpha;)
                                        {x"CEB2",`(beta)`}, -- (aka &beta;)
                                        {x"CEB3",`(gamma)`}, -- (aka &gamma;)
                                        {x"CEB4",`(delta)`}, -- (aka &delta;)
                                        {x"CEB5",`(epsilon)`}, -- (aka &epsilon;)
                                        {x"CEB6",`(zeta)`}, -- (aka &zeta;)
                                        {x"CEB7",`(eta)`}, -- (aka &eta;)
                                        {x"CEB8",`(theta)`}, -- (aka &theta;)
                                        {x"CEB9",`(iota)`}, -- (aka &iota;)
                                        {x"CEBA",`(kappa)`}, -- (aka &kappa;)
                                        {x"CEBB",`(lambda)`}, -- (aka &lambda;)
                                        {x"CEBC",`(mu)`}, -- (aka &mu;)
                                        {x"CEBD",`(nu)`}, -- (aka &nu;)
                                        {x"CEBE",`(xi)`}, -- (aka &xi;)
                                        {x"CEBF",`(omicron)`}, -- (aka &omicron;)
                                        {x"CF80",`(pi)`}, -- (aka &pi;)
                                        {x"CF81",`(rho)`}, -- (aka &rho;)
                                        {x"CF82",`(sigmaf)`}, -- (aka &sigmaf;)
                                        {x"CF83",`(sigma)`}, -- (aka &sigma;)
                                        {x"CF84",`(tau)`}, -- (aka &tau;)
                                        {x"CF85",`(upsilon)`}, -- (aka &upsilon;)
                                        {x"CF86",`(phi)`}, -- (aka &phi;)
                                        {x"CF87",`(chi)`}, -- (aka &chi;)
                                        {x"CF88",`(psi)`}, -- (aka &psi;)
                                        {x"CF89",`(omega)`}, -- (aka &omega;)
                                        {x"E1BFA6",`(upsilon with perispomeni)`},
                                        {x"E2808A",``}, -- (thinner than a thin space)
                                        {x"E2808B",``}, -- (zero width space)
                                        {x"E2808D",``}, -- (zero width joiner)
                                        {x"E28093",`-`}, -- (aka &ndash;)
                                        {x"E28094",`-`}, -- (aka &mdash;)
                                        {x"E28095",`-`},
                                        {x"E28098",`'`},
                                        {x"E28099",`'`},
                                        {x"E2809C",`"`},
                                        {x"E2809D",`"`},
                                        {x"E280A2",`*`}, -- (aka &bull;)
                                        {x"E280A6",`...`}, -- (aka &hellip;)
                                        {x"E28184",`/`}, -- (aka &frasl;)
                                        {x"E281B0",`^0`}, -- (superscript 0)
                                        {x"E281B4",`^4`}, -- (superscript 4)
                                        {x"E281B5",`^5`}, -- (superscript 5)
                                        {x"E281B6",`^6`}, -- (superscript 6)
                                        {x"E281B7",`^7`}, -- (superscript 7)
                                        {x"E281B8",`^8`}, -- (superscript 8)
                                        {x"E281B9",`^9`}, -- (superscript 9)
                                        {x"E281BA",`^+`}, -- (superscript +)
                                        {x"E281BB",`^-`}, -- (superscript -)
                                        {x"E28496",`No.`}, -- (numero sign)
                                        {x"E28590",`1/7`},
                                        {x"E28591",`1/9`},
                                        {x"E28592",`1/10`},
                                        {x"E28593",`1/3`},
                                        {x"E28594",`2/3`},
                                        {x"E28595",`1/5`},
                                        {x"E28596",`2/5`},
                                        {x"E28597",`3/5`},
                                        {x"E28598",`4/5`},
                                        {x"E28599",`1/6`},
                                        {x"E2859A",`5/6`},
                                        {x"E2859B",`1/8`},
                                        {x"E2859C",`3/8`},
                                        {x"E2859D",`5/8`},
                                        {x"E2859E",`7/8`},
                                        {x"E28689",`0/3`},
                                        {x"E28690",`<-`}, -- (aka &larr;)
                                        {x"E28691",`^`}, -- (aka &uarr;)
                                        {x"E28692",`->`}, -- (aka &rarr;)
                                        {x"E28693",`v`}, -- (aka &darr;)
                                        {x"E28886",`(increment)`},
                                        {x"E28888",`(element of)`}, -- (aka &isin;)
                                        {x"E28892",`-`}, -- (aka &minus;)
                                        {x"E2889A",`(sqrt)`}, -- (aka &radic;)
                                        {x"E2889E",`(infinity)`}, -- (aka &infin;)
                                        {x"E288A9",`(intersection)`}, -- (aka &cap;)
                                        {x"E288AA",`(union)`}, -- (aka &cup;)
                                        {x"E289A4",`<=`}, -- (aka &le;)
                                        {x"E289A5",`>=`}, -- (aka &ge;)
                                        {x"E28A8E",`(union+)`},
                                        {x"E28EA7",`{`},
                                        {x"E28EA8",`{`},
                                        {x"E28EA9",`{`},
                                        {x"E28EAA",`|`},
                                        {x"E28EAB",`}`},
                                        {x"E28EAC",`}`},
                                        {x"E28EAD",`}`},
                                        {x"E28FA8",`(base 10)`},
                                        {x"E29480",`-`}, -- (aka &mdash;)
                                        {x"E29482",`|`},
                                        {x"E2948C",`+`},
                                        {x"E29490",`+`},
                                        {x"E29494",`+`},
                                        {x"E29495",`+`},
                                        {x"E29498",`+`},
                                        {x"E29590",`=`},
                                        {x"E29591",`|`},
                                        {x"E29592",`+`},
                                        {x"E29593",`+`},
                                        {x"E29594",`+`},
                                        {x"E29595",`+`},
                                        {x"E29597",`+`},
                                        {x"E29598",`+`},
                                        {x"E29599",`+`},
                                        {x"E2959A",`+`},
                                        {x"E2959B",`+`},
                                        {x"E2959C",`+`},
                                        {x"E2959D",`+`},
                                        {x"E2959E",`+`},
                                        {x"E2959F",`+`},
                                        {x"E295AA",`+`},
                                        {x"E295AB",`+`},
                                        {x"E295AC",`+`},
                                        {x"E296B6",`* `}, -- (black right-pointing triangle)
                                        {x"E296BA",`* `}, -- (black right-pointing pointer)
                                        {x"E29891",`(:ballot_box_with_check:)`},
                                        {x"E794B2",`(jai3)`},
                                        {x"E794B3",`(Han juice)`},
                                        {x"EFBFBD",`?`}, -- (invalid unicode character)
                                        {x"F09D9192",`(e)`}, -- (as in 2.718281828459045)
                                        {x"F09D9ABA",`(SIGMA)`}, -- (capital bold Sigma)
                                        {x"F09D9ABD",`(PHI)`}, -- (capital bold Phi)
                                        {x"F09D9C8B",`(pi)`}, -- (as in 3.14159265358979)
                                        {x"F09D9C91",`(phi)`},
                                        {x"F09F8CB5",`(:cactus:)`},
                                        {x"F09F8CB6",`(:hot pepper:)`},
                                        {x"F09F8CBD",`(:ear of maize:)`},
                                        {x"F09F8CBE",`(:ear of rice:)`},
                                        {x"F09F91A6",`(:boy:)`},
                                        {x"F09F91A7",`(:girl:)`},
                                        {x"F09F91A8",`(:man:)`},
                                        {x"F09F91A9",`(:woman:)`},
                                        {x"F09FA791",`(:adult:)`},
                                        {x"F09FA794",`(:beardie:)`}})
global function text_clean(string s)
    -- html -> readable [and (incomplete/unneeded) displayable in a windows console]
    -- The one task that uses this (being tasks without examples) specifically requires
    -- .htm[infobox..toc], but it might be more sensible to get "plain text" from .raw
    -- The output as html option certainly works much better, and of course that does 
    -- not invoke or use this at all. Anyway, undo some things rosettacode has applied,
    -- I went a bit overboard with the distro version of thex, for a windows console..
    return substitute_all(s,thex,tasc)
end function

include builtins\libcurl.e
atom curl = NULL, pErrorBuffer

procedure curl_init()
    curl_global_init()
    curl = curl_easy_init()
    pErrorBuffer = allocate(CURL_ERROR_SIZE)
    curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, pErrorBuffer)
end procedure

integer tbwr = 0 -- (debug aid)
function write_callback(atom pData, integer size, nmemb, fn)
    integer bytes_written = size * nmemb
    puts(fn,peek({pData,bytes_written}))
    tbwr += bytes_written
    return bytes_written
end function
constant write_cb = call_back({'+', write_callback})

global string wastitle = `` -- don't clobber "NEED EDITING"/Downloading messages
global integer show_title = progress

local function check_cache(string filename, integer gt_opt=GT_WHOLE_FILE, bool refetch=false)
    object result = ""
    string why = "not found"
    filename = join_path({"rc_cache",file_clean(filename)})
    if not refetch and file_exists(filename) then
        -- use existing file if <= refresh_cache days old
        sequence last_mod = get_file_date(filename)
        atom delta = timedate_diff(last_mod,date())
        refetch = true
        if delta>refresh_cache 
        and not match(".hist",filename) then
            -- no point showing hours etc if days are greater anyway:
            for term in {DT_SECOND, DT_MINUTE, DT_HOUR, DT_DAY} do
                atom delta2 = timedate_diff(last_mod,date(),term)
                if delta2<=refresh_cache then exit end if
                delta = delta2
            end for
            why = elapsed(delta) & " > " & elapsed(refresh_cache)
        elsif get_file_size(filename)=0 then
            why = "filesize of 0"
        else
            result = get_text(filename,gt_opt)
            if length(result)<10 then
                why = "<10 "&iff(gt_opt=GT_WHOLE_FILE?"bytes":"lines")
            else
                if gt_opt=GT_WHOLE_FILE then
                    result = trim(result)
                end if
                refetch = false
            end if
        end if
    else
        refetch = true
        string directory = get_file_path(filename)
        if get_file_type(directory)!=FILETYPE_DIRECTORY then
            if not create_directory(directory,make_parent:=true) then
                crash("cannot create %s directory",{directory})
            end if
        end if
    end if
    return {refetch, {result, why, filename}} -- (a bool & 3 strings)
end function

local function open_download(string name, url, integer i=0, n=0)
    {bool refetch, string {text, why, filename}} = check_cache(name)
--DEV temp:
string urlc = url_clean(url),
       urlu = substitute(url," ","%20")
if urlu!=urlc then ?urlu ?urlc end if
    if refetch then
        url = url_clean(url)
        wastitle = "x" -- don't clobber
        string nofn = iff(n?sprintf("(%d/%d, %.1f%%) ",{i,n,i/n*100}):""),
               title = sprintf("Downloading %s%s (%s)...",{nofn,filename,why})
        show_title(title)
        if curl=NULL then curl_init() end if
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_cb)
        curl_easy_setopt(curl, CURLOPT_URL, url)
        integer fn = open(filename,"wb")
        assert(fn!=-1,"cannot open "&filename)
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fn)
        tbwr = 0
        while true do
            CURLcode res = curl_easy_perform(curl)
            if res=CURLE_OK then
                assert(tbwr!=0,`erm, zero bytes "successfully downloaded??"`)
                exit
            end if
            string error = sprintf("%d",res)
            if res=CURLE_COULDNT_RESOLVE_HOST then
                error &= " [CURLE_COULDNT_RESOLVE_HOST]"
            end if
            progress("Error %s downloading file, retry?(Y/N):",{error})
            if lower(wait_key())!='y' then abort(0) end if
            printf(1,"Y\n")
        end while
        close(fn)
        if refresh_cache!=0 then
            refresh_cache += day -- did I mention it is slow?
        end if
        text = get_text(filename)
    end if
    return text
end function

constant rcorg = "https://rosettacode.org",
      base_cat = rcorg&"/wiki/Category:",
      base_htm = rcorg&"/wiki/",
     base_hist = rcorg&"/w/index.php?title=%s&action=history"&
                       "&dir=prev&limit=1",
    base_query = rcorg&"/w/api.php?action=query&format=xml"&
                       "&list=categorymembers&cmlimit=100",
      base_raw = rcorg&"/w/index.php?title=%s&action=raw"

global function open_category(string filename, integer i=0, n=0)
    string contents = open_download(filename&".htm",base_cat&filename,i,n)
    return contents
end function

global function get_rc_htm(string task)
    string contents = open_download(task&".htm",base_htm&substitute(task," ","_"))
    return contents
end function

global function get_rc_hist(string task, integer i=0, n=0)
    string contents = open_download(task&".hist",sprintf(base_hist,{task}),i,n)
    return contents
end function

include builtins\xml.e

global function get_rc_category(string name, integer i=0, n=0, bool refetch=false)
    -- Note: you /will/ want to file_clean() results before display/file i/o, but
    --       you may also need the pre-version of that for get_rc_raw(), etc.
    sequence result
    string filename = sprintf("cached_category_%s.txt",{name}), why
    {refetch, {result, why, filename}} = check_cache(filename,GT_LF_STRIPPED,refetch)
    if refetch then
        wastitle = "x" -- don't clobber
        string nofn = iff(n?sprintf("(%d/%d, %.1f%%) ",{i,n,i/n*100}):""),
               page = ""
        n = 1 -- (reuse as page number)
        result = {}
        string lang_query := base_query & "&cmtitle=Category:" & name,
               continue_at := ""
        if curl=NULL then curl_init() end if
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL) -- (i/c it got set above)
        do
            string url = lang_query,
                 title = sprintf("Downloading %s%s%s (%s)...",{nofn,name,page,why})
            show_title(title)
            if length(continue_at) then
                url &= "&cmcontinue="&continue_at
            end if
            curl_easy_setopt(curl, CURLOPT_URL, url)
            object res = curl_easy_perform_ex(curl)
            if integer(res) then
                string error = sprintf("%d [%s]",{res,peek_string(pErrorBuffer)})
                crash("Download error: %s\n",{error})
            end if
            if not string(res) then ?9/0 end if
            object xml = xml_parse(res)[XML_CONTENTS]
            sequence cat = xml_get_nodes(xml,"continue")
            continue_at = iff(cat=={}?"":xml_get_attribute(cat[1],"cmcontinue"))
            xml = xml_get_nodes(xml,"query")[1]
            xml = xml_get_nodes(xml,"categorymembers")[1]
            xml = xml_get_nodes(xml,"cm")
            for i=1 to length(xml) do
                title = html_clean(xml_get_attribute(xml[i],"title"))
                if not match("Category:",title) then
                    result = append(result,title)
                end if
            end for
            n += 1
            page = sprintf("/%d",n)
        until continue_at=""
        show_title("")
        -- create/overwrite filename
        integer fn = open(filename,"w")
        puts(fn,join(result,"\n"))
        close(fn)
        if refresh_cache!=0 then
            refresh_cache += day -- did I mention it is slow?
        end if
    end if
    return result
end function

global function get_rc_raw(string task)
    string contents = open_download(task&".raw",sprintf(base_raw,{task}))
    return contents
end function

--https://rosettacode.org/w/index.php?title=Category:Programming_Languages
--(previous page) (<a href="/w/index.php?title=Category:Programming_Languages&amp;subcatfrom=DMS%0ADMS#mw-subcategories" title="Category:Programming Languages">next page</a>)
--... <block we're interested in has the same prev/next links before & after>
--(previous page) (<a href="/w/index.php?title=Category:Programming_Languages&amp;subcatfrom=DMS%0ADMS#mw-subcategories" title="Category:Programming Languages">next page</a>)
global function get_rc_stitched(string name, integer clean_block=0)
    {bool refetch, string {text, why, filename}} = check_cache(name&".html")
    if refetch then
        string url = sprintf("%s/w/index.php?title=Category:%s",{rcorg,name}), 
               ppt = "(previous page) ("
        if curl=NULL then curl_init() end if
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL) -- (i/c it got set above)
        text = ""
        while true do
            curl_easy_setopt(curl, CURLOPT_URL, url)
            object res = curl_easy_perform_ex(curl)
            if integer(res) then
                string error = sprintf("%d [%s]",{res,peek_string(pErrorBuffer)})
                crash("Download error: %s\n",{error})
            end if
            assert(string(res))
            integer ppdx = match(ppt,res),
                    npdx = find('(',res,ppdx+12), -- [12 is length(ppt)~ish]
                    npdz = find(')',res,npdx)
            assert(ppdx!=0)
            assert(npdx!=0)
            assert(npdz!=0)
            ppdx = rfind('(',res,ppdx)
            string ppnpt = res[ppdx..npdz],
                     npt = res[npdx..npdz]
            assert(ppnpt[1..10]=`(<a href="` or ppnpt[1..length(ppt)]=ppt)
            ppdx = match(ppnpt,res,npdz+1)
            assert(ppdx!=0)         
            string block = res[npdz+1..ppdx-1]
            if clean_block then
                block = clean_block(block)
            end if
            text &= block
            npdz = ppdx+length(ppnpt)
            if npt="(next page)" then exit end if
            --(<a href="/w/index.php?title=Category:..." title="Category:...">next page</a>)
            --1234567890                                                     543210987654321
            assert(npt[1..10]=`(<a href="`)
            assert(npt[-15..-1]=`>next page</a>)`)
            url = rcorg & npt[11..find('"',npt,12)]
            url = substitute_all(url,"&amp;","&")
            ppt = "previous page"
        end while
        -- create/overwrite filename
        integer fn = open(filename,"w")
        puts(fn,text)
        close(fn)
    end if
    return text
end function

include builtins\json.e

global function get_rc_lang_users(integer callback)
    --
    -- result is a JSON_ARRAY of JSON_OBJECTS
    --
    -- currently specific to Rank_Languages_by_users.exw but of course 
    -- the filename and url building could be more parameterised, with
    -- more parameters or perhaps even in a similar fashion to the way
    -- the json gets handled via a callee-specific routine.
    --
    {bool refetch, string {text, why, filename}} = check_cache("users.json")
    sequence result = {JSON_ARRAY}
    if refetch then
        string user_query = rcorg&"/w/api.php?action=query&format=json&formatversion=2"&
                            "&generator=categorymembers&gcmtitle=Category:Language%20users"&
                            "&gcmlimit=500&rawcontinue&prop=categoryinfo",
               gmc = ""
        if curl=NULL then curl_init() end if
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL) -- (i/c it got set above)
        do
            curl_easy_setopt(curl, CURLOPT_URL, user_query&gmc)
            object res = curl_easy_perform_ex(curl)
            if integer(res) then
                string error = sprintf("%d [%s]",{res,peek_string(pErrorBuffer)})
                crash("Download error: %s\n",{error})
            end if
            result = callback(result,parse_json(res))
            {result,gmc} = result
        until gmc=""
        close(print_json(open(filename,"w"),result))
    else
        result = parse_json(text)
    end if
    return result
end function
            
global procedure curl_cleanup()
    if curl!=NULL then
        curl_easy_cleanup(curl)
        free(pErrorBuffer)
        curl = NULL
        pErrorBuffer = NULL
    end if
end procedure

