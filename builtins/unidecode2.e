--
-- builtins\unidecode2.e
-- =====================
--
--  Convert unicode text into the best ASCII-only we can.
--
--  Simple translation of https://github.com/thecoderok/Unidecode.NET/blob/master/src/Unidecoder.cs
--
without debug
sequence characters
include unidecoder-decodemap.e -- nb 441K (or about 300K on the binary)

global function unidecode(string utf8)
    if utf8!="" then
        sequence utf32 = utf8_to_utf32(utf8)
--      if max(utf32)>#7F then
        if true then
            if not object(characters) then -- (ie still <novalue>)
--/*
                sequence dmt
                for p in include_paths() do // search builtins first...
                    -- nb do not try and edit this file, you'll only
                    --    screw it right up (tabs <==> spaces, etc..)
                    string path = join_path({p,`unidecoder-decodemap.txt`})
                    if get_file_type(path)=FILETYPE_FILE then
                        dmt = get_text(path,GT_LF_STRIPPED)
--string dmts = get_text(path)
--integer fn = open(`builtins\unidecoder-decodemap.e`,"wb")
--puts(fn,"-- `unidecoder-decodemap.e -- DO NOT EDIT\n")
--puts(fn,"global constant unidecoder_decodemap = {\n")
----puts(fn,substitute(dmts,"\t",`\t`))
----puts(fn,`"""`)
--for d in dmt do
--  d = substitute(d,"\\",`\\`)
--  d = substitute(d,"\"",`\"`)
--  d = substitute(d,"\t",`\t`)
--  printf(fn,"\"%s\",\n",d)
--end for
--puts(fn,`}`)
--close(fn)
                        exit
                    end if
                end for
--sequence dmt2 = unidecoder_decodemap
--?{length(dmt),length(dmt2),equal(dmt,dmt2)}
----for i=1 to length(dmt) do
----    if not equal(dmt[1],dmt2[i]) then ?i end if
----end for
--?dmt[1]
--?dmt2[1]
--              ?length(dmt) -- 190, as expected
--*/
                characters = repeat(0,503)
--              for line in dmt do
--integer p1 = 0, pn = 0
integer fn = open(`builtins\udtest.e`,"w")
                for line in unidecoder_decodemap do
                    assert(line[4]='\t')
                    integer idx = to_integer(trim(line[1..3]))
                    line = line[5..$]
                    sequence pieces = split(line,'\t')
                    assert(length(pieces)=256)
string s = repeat('_',256)
                    for i,p in pieces do
                        p = p[2..$-1] -- strip '`"'
if length(p)=1 then s[i] = p[1] end if
                        -- unescape: nb written in a minimal/as-needed basis.
                        --           if copied, may want "*+?|[[()^^$.# \t\r"...
                        integer start = 1
                        do
                            integer k = find('\\',p,start)
                            if k=0 then exit end if
                            integer ch = p[k+1]
                            if ch='u' then
                                p[k] = to_integer(p[k+2..k+5],0,16)
                                p[k+1..k+5] = ""
                            elsif find(ch,`"\`) then
                                p[k..k] = ""
                            elsif ch='n' then
                                p[k..k+1] = "\n"
                            else
                                ?9/0
                            end if
                            start = k+1
                        until false
--if length(p) then p1 += 1 else pn += 1 end if
--if length(p)=1 then p1 += 1 else pn += 1 end if
--if length(p)=1 then p1 += 1 else pn += length(p) end if
                        pieces[i] = p
                    end for
printf(fn,"%03d \"%s\",\n",{idx+1,s})
                    characters[idx+1] = pieces
                end for
close(fn)
--?{p1,pn} -- {41383,7257} (pn==zero length)
--?{p1,pn} -- {4402,44238}
--?{p1,pn} -- {4402,152880} -- (actual lengths)


            end if
            utf8 = ""
            for c in utf32 do
                if c<#80 then
                    utf8 &= c
                else
                    integer high = c >> 8;
                    if high<length(characters) then
                        integer low = c && 0xff;
                        object bytes = characters[high+1];
                        if bytes!=null then
                            utf8 &= bytes[low+1]
                        end if
                    end if
                end if
            end for
        end if
    end if
    return utf8
end function

