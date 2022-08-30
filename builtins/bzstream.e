--
-- builtins\bzstream.e
-- ===================
--
--  Not (intended to ever be) an autoinclude.
--  Used in & written for https://rosettacode.org/wiki/WiktionaryDumps_to_words#Phix
--
--  In memory bzip handling, using any block size (all the way down to just 1 byte).
--
--  Designed to work on a single (internal) stream, but you can (/should be able to)
--  create multiple streams easily enough (eg unpack from one straight into another)
--  [by stream I just mean any source of bytes, manually handled, rather than some
--   weird opaque thing that does some weird thing using some weird kind of syntax.]
--
--  I would suggest it is probably not worthwhile bothering with or fretting over
--  blockSize100k/verbosity/workFactor/sml settings and just rely on the defaults.
--
--  Mainly it is a case of keeping a sharp eye on avail_in and avail_out, sometimes
--  next_in and next_out might be easier, to know when it wants more input and/or
--  when you have to deal with some output, plus BZ_RUN/BZ_FINISH for BZ2_bzCompress.
--  Quite when it will spit out a block is of course a total mystery, until it does.
--
without javascript_semantics    -- incompatible with pwa/p2js (uses a dll/so)

--without debug -- may (yet) be helpful
include builtins\cffi.e
--include builtins\VM\pcfunc.e
--include builtins\VM\pprntfN.e
--include builtins\get.e
--with debug

constant t_bzstream = """
typedef struct {
  char *next_in;
  unsigned int avail_in;
  unsigned int total_in_lo32;
  unsigned int total_in_hi32;
  char *next_out;
  unsigned int avail_out;
  unsigned int total_out_lo32;
  unsigned int total_out_hi32;
  void *state;
  void *(*bzalloc)(void *,int,int);
  void (*bzfree)(void *,void *);
  void *opaque;
} bz_stream;
""",
  t_BZ2_bzCompressInit = """
int BZ2_bzCompressInit ( bz_stream *strm, 
                         int blockSize100k, 
                         int verbosity,
                         int workFactor );
""",
  t_BZ2_bzCompress = """
int BZ2_bzCompress ( bz_stream *strm, int action );
""",
  t_BZ2_bzCompressEnd = """
int BZ2_bzCompressEnd ( bz_stream *strm );
""",
  t_BZ2_bzDecompressInit = """
int BZ2_bzDecompressInit ( bz_stream *strm, int verbosity, int small );
""",
  t_BZ2_bzDecompress = """
int BZ2_bzDecompress ( bz_stream *strm );
""",
  t_BZ2_bzDecompressEnd = """
int BZ2_bzDecompressEnd ( bz_stream *strm );
"""

global constant id_bzs = define_struct(t_bzstream),
                p_bzs = allocate_struct(id_bzs)

string dll = iff(platform()=WINDOWS?sprintf("bz%d.dll",machine_bits()):
             iff(platform()=LINUX  ?"???.so":"???"))

constant bzlib = open_dll(join_path({"builtins",dll})),
         r_BZ2_bzCompressInit   = define_cffi_func(bzlib, t_BZ2_bzCompressInit),
         r_BZ2_bzCompress       = define_cffi_func(bzlib, t_BZ2_bzCompress),
         r_BZ2_bzCompressEnd    = define_cffi_func(bzlib, t_BZ2_bzCompressEnd),
         r_BZ2_bzDecompressInit = define_cffi_func(bzlib, t_BZ2_bzDecompressInit),
         r_BZ2_bzDecompress     = define_cffi_func(bzlib, t_BZ2_bzDecompress),
         r_BZ2_bzDecompressEnd  = define_cffi_func(bzlib, t_BZ2_bzDecompressEnd)

global constant bzid = {{BZ_RUN              :=  0, "BZ_RUN"},
                        {BZ_FLUSH            :=  1, "BZ_FLUSH"},
                        {BZ_FINISH           :=  2, "BZ_FINISH"},   
                        {BZ_OK               :=  0, "BZ_OK"},   
                        {BZ_RUN_OK           :=  1, "BZ_RUN_OK"},   
                        {BZ_FLUSH_OK         :=  2, "BZ_FLUSH_OK"}, 
                        {BZ_FINISH_OK        :=  3, "BZ_FINISH_OK"},    
                        {BZ_STREAM_END       :=  4, "BZ_STREAM_END"},   
                        {BZ_SEQUENCE_ERROR   := -1, "BZ_SEQUENCE_ERROR"},   
                        {BZ_PARAM_ERROR      := -2, "BZ_PARAM_ERROR"},
                        {BZ_MEM_ERROR        := -3, "BZ_MEM_ERROR"},
                        {BZ_DATA_ERROR       := -4, "BZ_DATA_ERROR"},
                        {BZ_DATA_ERROR_MAGIC := -5, "BZ_DATA_ERROR_MAGIC"},
                        {BZ_IO_ERROR         := -6, "BZ_IO_ERROR"},
                        {BZ_UNEXPECTED_EOF   := -7, "BZ_UNEXPECTED_EOF"},
                        {BZ_OUTBUFF_FULL     := -8, "BZ_OUTBUFF_FULL"},
                        {BZ_CONFIG_ERROR     := -9, "BZ_CONFIG_ERROR"}}

constant {bzids,bzdescs} = columnize(bzid)

global function BZ2_desc(integer r)
    return bzdescs[find(r,bzids,4)] -- (first 3 are actions not results/error codes)
end function

global procedure BZ2_bzCompressInit(atom strm=p_bzs, integer blockSize100k=9, verbosity=0, workFactor=0)
    if blockSize100k<1 or blockSize100k>9 then ?9/0 end if
    if verbosity<0 or verbosity>4 then ?9/0 end if
    if workFactor<0 or workFactor>250 then ?9/0 end if -- (0==30)
    integer res = c_func(r_BZ2_bzCompressInit,{strm,blockSize100k,verbosity,workFactor})
    if res!=BZ_OK then ?9/0 end if
end procedure

global function BZ2_bzCompress(atom strm=p_bzs, integer action=BZ_RUN)
    if action!=BZ_RUN and action!=BZ_FINISH then ?9/0 end if
    integer res = c_func(r_BZ2_bzCompress,{strm,action})
    if res!=BZ_STREAM_END and res!=BZ_FINISH_OK and res!=BZ_RUN_OK then ?9/0 end if
    return res
end function

global procedure BZ2_bzCompressEnd(atom strm=p_bzs)
    integer res = c_func(r_BZ2_bzCompressEnd,{strm})
    if res!=BZ_OK then ?9/0 end if
end procedure

global procedure BZ2_bzDecompressInit(atom strm=p_bzs, integer verbosity=0, sml=0)
    integer res = c_func(r_BZ2_bzDecompressInit,{strm,verbosity,sml})
    if res!=BZ_OK then ?9/0 end if
end procedure

global function BZ2_bzDecompress(atom strm=p_bzs)
    integer res = c_func(r_BZ2_bzDecompress,{strm})
    if res!=BZ_STREAM_END and res!=BZ_OK then ?9/0 end if
    return res
end function

global procedure BZ2_bzDecompressEnd(atom strm=p_bzs)
    integer res = c_func(r_BZ2_bzDecompressEnd,{strm})
    if res!=BZ_OK then ?9/0 end if
end procedure

--/* original test code:
--constant BLOCKSIZE=40
--constant BLOCKSIZE=512
--    constant BLOCKSIZE=1024
constant BLOCKSIZE=8192

constant inbuf = allocate(BLOCKSIZE),
         outbuf = allocate(BLOCKSIZE)

string infile = "e06.exw", 
--     outfile = infile&".bz2"
       outfile = "e06.xxw.bz2"
integer infn = open(infile,"rb"),
        outfn = open(outfile,"wb")

-- 1. compress:
BZ2_bzCompressInit()
integer avail_in = 0,
        avail_out,
        action = BZ_RUN
while true do
    if avail_in=0 and action=BZ_RUN then
        string inbytes = get_bytes(infn,BLOCKSIZE)
        avail_in = length(inbytes)
        poke(inbuf,inbytes)
        set_struct_field(id_bzs,p_bzs,"next_in",inbuf)
        set_struct_field(id_bzs,p_bzs,"avail_in",avail_in)
        if avail_in<BLOCKSIZE then
            close(infn)
            ?"fin"
            action = BZ_FINISH
        end if
    end if
    set_struct_field(id_bzs,p_bzs,"next_out",outbuf)
    set_struct_field(id_bzs,p_bzs,"avail_out",BLOCKSIZE)
--?{"initial next_in:",get_struct_field(id_bzs,p_bzs,"next_in")}
--?{"initial avail_in:",get_struct_field(id_bzs,p_bzs,"avail_in")}
--?{"initail next_out:",get_struct_field(id_bzs,p_bzs,"next_out")}
--?{"initial avail_out:",get_struct_field(id_bzs,p_bzs,"avail_out")}
--  integer res = BZ2_bzCompress(action:=action)
    integer res = BZ2_bzCompress(p_bzs,action)
--?{"res",res,BZ2_desc(res)}
--?{"after next_in:",get_struct_field(id_bzs,p_bzs,"next_in")}
    avail_in = get_struct_field(id_bzs,p_bzs,"avail_in")
--?{"after avail_in:",avail_in}
atom next_out = get_struct_field(id_bzs,p_bzs,"next_out")
--?{"after next_out:",next_out}
    avail_out = get_struct_field(id_bzs,p_bzs,"avail_out")
--?{"after avail_out:",avail_out}
--?{"total_in_lo32",get_struct_field(id_bzs,p_bzs,"total_in_lo32")}
--?{"total_out_lo32",get_struct_field(id_bzs,p_bzs,"total_out_lo32")}
--{} = wait_key()
    if avail_out<BLOCKSIZE then
        puts(outfn,peek({outbuf,BLOCKSIZE-avail_out}))
--?{"written",BLOCKSIZE-avail_out,"bytes"}
--{} = wait_key()
        set_struct_field(id_bzs,p_bzs,"next_out",outbuf)
        set_struct_field(id_bzs,p_bzs,"avail_out",BLOCKSIZE)
    end if
    if res=BZ_STREAM_END then
--?{"close",outfn,outfile}
        close(outfn)
--?{outfn,"?"}
        BZ2_bzCompressEnd()
        exit
    end if
    if res!=BZ_RUN_OK and res!=BZ_FINISH_OK then ?9/0 end if
end while

-- 2. decompress:
infn = open(outfile,"rb")   -- nb read this time...
outfn = open(infile,"rb")   -- nb verified this time...
BZ2_bzDecompressInit()
avail_in = 0
string demiline = ""
integer lineno = 1
while true do
    if avail_in=0 then
        string bzbytes = get_bytes(infn,BLOCKSIZE)
        avail_in = length(bzbytes)
        poke(inbuf,bzbytes)
        set_struct_field(id_bzs,p_bzs,"next_in",inbuf)
        set_struct_field(id_bzs,p_bzs,"avail_in",avail_in)
--?{"out",avail_in,"bytes"}
        if avail_in<BLOCKSIZE then
            close(infn)
--          ?"fin"
--          action = BZ_FINISH
        end if
    end if
    set_struct_field(id_bzs,p_bzs,"next_out",outbuf)
    set_struct_field(id_bzs,p_bzs,"avail_out",BLOCKSIZE)
--?{"initial next_in:",get_struct_field(id_bzs,p_bzs,"next_in")}
--?{"initial avail_in:",get_struct_field(id_bzs,p_bzs,"avail_in")}
--?{"initail next_out:",get_struct_field(id_bzs,p_bzs,"next_out")}
--?{"initial avail_out:",get_struct_field(id_bzs,p_bzs,"avail_out")}
    integer res = BZ2_bzDecompress()
--?{"decompress res",res,BZ2_desc(res)}
--?{"after next_in:",get_struct_field(id_bzs,p_bzs,"next_in")}
    avail_in = get_struct_field(id_bzs,p_bzs,"avail_in")
--?{"after avail_in:",avail_in}
--?{"after next_out:",get_struct_field(id_bzs,p_bzs,"next_out")}
    avail_out = get_struct_field(id_bzs,p_bzs,"avail_out")
--?{"after avail_out:",avail_out}
--?{"total_out_lo32",get_struct_field(id_bzs,p_bzs,"total_out_lo32")}
--{} = wait_key()
    if avail_out<BLOCKSIZE then
--      puts(outfn,peek({outbuf,BLOCKSIZE-avail_out}))
        string chk = peek({outbuf,BLOCKSIZE-avail_out})
--?chk
        for i=1 to length(chk) do
            integer ch = getc(outfn)
            if chk[i]!=ch then ?9/0 end if
        end for
        chk = demiline & chk
        integer linestart = 1
        for i=1 to length(chk) do
            if chk[i]='\n' then
--              ?{lineno,chk[linestart..i-1]}
                linestart = i+1
                lineno += 1
            end if
        end for
        demiline = chk[linestart..$]
        
--?{"verified",BLOCKSIZE-avail_out,"bytes"}
--{} = wait_key()
--      set_struct_field(id_bzs,p_bzs,"next_out",outbuf)
--      set_struct_field(id_bzs,p_bzs,"avail_out",BLOCKSIZE)
    end if
    if res=BZ_STREAM_END then
        if length(demiline) then
            ?{"demiline",lineno,demiline}
        end if
        if getc(outfn)!=-1 then ?9/0 end if
        close(outfn)
        BZ2_bzDecompressEnd()
        exit
    end if
    if res!=BZ_OK then ?9/0 end if
end while
--*/

--/*
string wd = `E:\downloads\misc\arm\bz2\bzip2-dll-1.0.8.0-win-x86\wikidump\wikidump.txt`
integer infd = open(wd,"r")
--while doline(gets(infd)) do end while
integer line_no = 1
while doline(line_no,gets(infd)) do line_no+= 1 end while
close(infd)
--*/
