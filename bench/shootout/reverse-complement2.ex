-- The Computer Language Shootout Benchmarks
-- http://shootout.alioth.debian.org/
--
-- Converted to Euphoria by Jason Gade
-- run: exu reverse-complement2.ex < input-file > output-file

without warning
without type_check

constant iubcodes = { "ACBDKRTGVHMYU", "acbdkrtgvhmyu", "TGVHMYACBDKRA" }

sequence iubLookup
iubLookup = repeat(0, 128)

procedure buildIubLookup()

    for i = 1 to 128 do
        iubLookup[i] = i - 1
    end for
    
    for i = 1 to length(iubcodes[1] - 1) do
        iubLookup[iubcodes[1][i] + 1] = iubcodes[3][i]
        iubLookup[iubcodes[2][i] + 1] = iubcodes[3][i]
    end for
 
end procedure

constant LINELEN = 60

procedure process(sequence strand, integer count)

    integer front, back, last, c
    integer lowr, n

    last = count
    front = 1
    back = LINELEN

    -- Reverse and translate
    n = floor(count/2) + 1
    lowr = 1
    for uppr = count to n by -1 do
        c = strand[uppr]
        strand[uppr] = iubLookup[strand[lowr] + 1]
        strand[lowr] = iubLookup[c + 1]
        lowr += 1
    end for

    while count > LINELEN do
        puts(1, strand[front..back] & '\n')
        front += LINELEN
        back += LINELEN
        count -= LINELEN
    end while

    puts(1, strand[front..last] & '\n')

end procedure

constant ALLOC = 1024

procedure main()
    object buffer
    sequence strand
    integer count, index, bufsize, strndsize

    strand = repeat(0, ALLOC)    
    index = 1

    buildIubLookup()

    buffer = gets(0)

    while not atom(buffer) do

        if buffer[$] = '\n' then
            buffer = buffer[1..$-1]
        end if

        if buffer[1] = '>' then
            if index > 1 then
                process(strand, count)
                count = 0
                index = 1
            end if
            printf(1, "%s\n", {buffer})
        else
            bufsize = length(buffer)
            strndsize = length(strand)
            count = index + bufsize - 1

            while count > strndsize do
                strand &= repeat(0, ALLOC)
                strndsize = length(strand)
            end while
            
            strand[index..count] = buffer
            index += bufsize
            
        end if

        buffer = gets(0)

    end while

    if index > 1 then
        process(strand, count)
    end if

end procedure

main()
