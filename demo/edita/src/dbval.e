--
-- dbval.e
-- =======
-- originally posted to euforum by Derek Parnell on 28 Mar 2002
--
include builtins\file.e
without trace
without profile
without warning
without type_check
constant
        DB_MAGIC = 77,
        FREE_COUNT = 7,
        SIZEOF_TABLE_HEADER = 16,
        I2B = 249,   -- 2-byte signed integer follows
        I3B = 250,   -- 3-byte signed integer follows
        I4B = 251,   -- 4-byte signed integer follows
        F4B = 252,   -- 4-byte f.p. number follows
        F8B = 253,   -- 8-byte f.p. number follows
        S1B = 254,   -- sequence, 1-byte length follows, then elements
--      S4B = 255,   -- sequence, 4-byte length follows, then elements
        INIT_FREE = 5,
--      KnownVersions = {{0,5}, {2,3}},
        KnownVersions = {{0,5}, {2,3}, {2,5}},
        BADINT = #100000000

global constant
         kOpenFailed = 1,
         kNotEDS = 2,
         kTableSeek = 3,
         kInitialSeek = 4,
         kTableCount = 5,
         kTableNamePtr = 6,
         kRecCnt = 7,
         kBlkCnt = 8,
         kTableIndexPtr = 9,
         kTableNameSeek = 10,
         kTableName = 11,
         kIndexSeek = 12,
         kRecordCnt = 13,
         kRecordPtr = 14,
         kRecordSeek = 15,
         kKeyPtr = 16,
         kKeySeek = 17,
         kDataPtr = 18,
         kDataSeek = 19,
         kNextTableSeek = 20,
         kFreeCountSeek = 21,
         kFreeChainSeek = 22,
         kFreeChainPtr = 23,
         kFreeCount = 24,
         kFreeChainAddr = 25,
         kFreeChainSize = 26,
         kKeyData = 27,
         kDataData = 28,
         kFreeCountTooHigh = 29,
         kFreeList = 30,
         kFreeListSpace = 31,
         kFreeListSeek = 32,
         kFreeChainSizeChk = 33,
         kVersion = 34


function get_string(integer fn)
-- read a 0-terminated string at current position in database file
    sequence s
    integer c

    s = ""
    while 1 do
        c = getc(fn)
        if c=-1 then
            return -1
        elsif c=0 then
            exit
        end if
        s &= c
    end while
    return s
end function

function get4(integer fn)
-- read 4-byte value at current position in database file
    integer b0, b1, b2, b3
    b0 = getc(fn)
    b1 = getc(fn)
    b2 = getc(fn)
    b3 = getc(fn)
    if b0<0 or b1<0 or b2<0 or b3<0 then
        return BADINT
    else
        return b0 + #100*b1 + #10000*b2 + #1000000*b3
    end if
end function


function validate_data(integer fn)
-- validate a compressed Euphoria(/Phix) object from disk
    integer c, v
    atom len

    c = getc(fn)
    if c<0 then
        return 0
    end if

    if  c<I2B then
        v = 0

    elsif c=I2B then
        v = 2

    elsif c=I3B then
        v = 3

    elsif c=I4B then
        v = 4

    elsif c=F4B then
        v = 4

    elsif c=F8B then
        v = 8
    else
        -- sequence
        if c=S1B then
            len = getc(fn)
            if len<0 then
                return 0
            end if
        else
            len = get4(fn)
            if len=BADINT then
                return 0
            end if
        end if
        for i=1 to len do
            if validate_data(fn)=0 then
                return 0
            end if
        end for
        return 1
    end if

    for i=1 to v do
        c = getc(fn)
        if c<0 then
            return 0
        end if
    end for

    return 1
end function



global function db_validate(sequence pFileName)
-- Validates a EDS database.
    object void, sObj
    integer magic, minor, major
    integer fn
    atom tables, ntables, tname, trecords, t_header, tnrecs,
 key_ptr, data_ptr, size, addr, tindex
    integer  tblocks
    atom free_count, free_list, free_list_space
    atom max

    fn = open(pFileName, "rb")
    if fn=-1 then
        return {0, kOpenFailed}
    end if

    void = seek(fn, -1)
    max = where(fn)

    void = seek(fn, 0)
    if void!=0 then
        return {0, kInitialSeek}
    end if
    magic = getc(fn)
    if magic!=DB_MAGIC then
        close(fn)
        return {0, kNotEDS}
    end if

    major = getc(fn)
    minor = getc(fn)
    if not find({major,minor}, KnownVersions) then
        close(fn)
        return {0, kVersion, {major,minor}}
    end if

    tables = get4(fn)
    if tables<0 or tables=BADINT or tables>max then
        close(fn)
        return {0, kNotEDS}
    end if

    void = seek(fn,tables)
    if void!=0 then
        close(fn)
        return {0, kTableSeek}
    end if

    ntables = get4(fn)
    if ntables<0 or ntables=BADINT then
        close(fn)
        return {0, kTableCount}
    end if

    t_header = where(fn)

    for t=1 to ntables do
        -- display the next table
        tname = get4(fn)
        if tname<0 or tname=BADINT or tname>max then
            close(fn)
            return {0, kTableNamePtr, t}
        end if

        tnrecs = get4(fn)
        if tnrecs<0 or tnrecs=BADINT then
            close(fn)
            return {0, kRecCnt, t}
        end if

        tblocks = get4(fn)
        if tblocks<0 or tblocks=BADINT then
            close(fn)
            return {0, kBlkCnt, t}
        end if

        tindex = get4(fn)
        if tindex<0 or tindex=BADINT or tindex>max then
            close(fn)
            return {0, kTableIndexPtr, t}
        end if

        void = seek(fn,tname)
        if void!=0 then
            close(fn)
            return {0, kTableNameSeek, t}
        end if

        sObj = get_string(fn)
        if equal(sObj, -1) then
            close(fn)
            return {0, kTableName, t}
        end if

        for b=1 to tblocks do
            void = seek(fn, tindex+(b-1)*8)
            if void!=0 then
                close(fn)
                return {0, kIndexSeek,t,b}
            end if

            tnrecs = get4(fn)
            if tnrecs<0 or tnrecs=BADINT then
                close(fn)
                return {0, kRecordCnt, t, b}
            end if

            trecords = get4(fn)
            if trecords<0 or trecords=BADINT or trecords>max then
                close(fn)
                return {0, kRecordPtr, t, b}
            end if

            for r=1 to tnrecs do
            -- check the record data
                void = seek(fn, trecords+(r-1)*4)
                if void!=0 then
                    close(fn)
                    return {0, kRecordSeek, t, b, r}
                end if

                key_ptr = get4(fn)
                if key_ptr<0 or key_ptr=BADINT or key_ptr>max then
                    close(fn)
                    return {0, kKeyPtr, t, b, r}
                end if

                void = seek(fn, key_ptr)
                if void!=0 then
                    close(fn)
                    return {0, kKeySeek, t, b, r}
                end if

                data_ptr = get4(fn)
                if data_ptr<0 or data_ptr=BADINT or data_ptr>max then
                    close(fn)
                    return {0, kDataPtr, t, b, r, data_ptr}
                end if

                if validate_data(fn)=0 then
                    close(fn)
                    return {0, kKeyData, t, b, r}
                end if

                void = seek(fn, data_ptr)
                if void!=0 then
                    close(fn)
                    return {0, kDataSeek, t, b, r}
                end if

                if validate_data(fn)=0 then
                    close(fn)
                    return {0, kDataData, t, b, r}
                end if
            end for
        end for
        t_header += SIZEOF_TABLE_HEADER
        void = seek(fn, t_header)
        if void!=0 then
            close(fn)
            return {0, kNextTableSeek, t}
        end if
    end for

    -- check the free list
    void = seek(fn, FREE_COUNT)
    if void!=0 then
        close(fn)
        return {0, kFreeCountSeek}
    end if

    free_count = get4(fn)
    if free_count<0 or free_count=BADINT then
        close(fn)
        return {0, kFreeCount}
    end if
    if free_count>max/13 then
        close(fn)
        return {0, kFreeCountTooHigh}
    end if

    free_list = get4(fn)
    if free_list<0 or free_list=BADINT then
        close(fn)
        return {0, kFreeChainPtr}
    end if
    if free_list>max then
        close(fn)
        return {0, kFreeList}
    end if

    void = seek(fn, free_list-4)
    if void!=0 then
        close(fn)
        return {0, kFreeChainSeek}
    end if
    free_list_space = get4(fn)
    if free_list_space=BADINT or
       free_list_space>max or
       free_list_space<INIT_FREE*8 then
        close(fn)
        return {0, kFreeListSpace}
    end if

    for i=1 to free_count do
        void = seek(fn, free_list+(i-1)*8)
        if void!=0 then
            close(fn)
            return {0, kFreeListSeek}
        end if

        addr = get4(fn)
        if addr=BADINT or addr<0 or addr>(max-4) then
            close(fn)
            return {0, kFreeChainAddr, i}
        end if

        size = get4(fn)
        if size<0 or size=BADINT then
            close(fn)
            return {0, kFreeChainSize, i}
        end if
        if (size+addr-4)>max then
            close(fn)
            return {0, kFreeChainSize, i}
        end if

        void = seek(fn, addr-4)
        if get4(fn)>size then
            close(fn)
            return {0, kFreeChainSizeChk, i}
        end if

    end for


    -- All clear.
    close(fn)
    return {1}
end function

