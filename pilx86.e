--
-- pilx86.e
--  (created 14/3/09, from pilxl.e+pgscan.e+pltype.e)

--
-- implements ilxlate(), the translation of intermediate code into x86 binary.
--
--EXCEPT
--Xconstant DBGCALL = 0 -- Phix implements call/return as mov [ebp+16],<return addr>;
constant DBGCALL = 0    -- Phix implements call/return as mov [ebp+28],<return addr>;
                        -- jmp trn_il, to avoid system stack limitations/fiddlies,
                        -- but makes working in OllyDbg etc harder. Setting this to
                        -- 1 replaces the jmp with call;add esp,4 (X32 only).
constant debugleak = 01 -- Backend diagnostic option, checks that the number of
                        -- bytes allocated and the number of bytes freed match.
                        --  (nb internal routines only, does not cover explicit
                        --      calls to allocate() and free(), due to things
                        --      like mem0 = allocate(4) in database.e)
                        -- When this is 1, emit a slew of opCleanup to facilitate
                        -- the test, ie deref all referenced vars to expose the
                        -- presence of any orphaned stuff.
                        -- NB Must match p.exe, read in or running in memory;
                        --    you should NOT try to change this setting.
                        -- I was going to switch this off, to gain a little extra
                        --  speed, for all live releases, but it makes so little
                        --  difference it may as well be left on all the time.
                        -- You can switch off opCleanup by ending the main program
                        --  with abort(0) [at the top level], if that helps any.

constant J_SWITCHED = -53174    -- an opJne/eq/if/not x86 offset when it is not 
                                -- a real jump but instead the corresponding jump 
                                -- table entry was updated - with the address of
                                -- where the jump would have been, rather than 
                                -- the address the jump would have jumped to, btw.
                                -- (btw: -53174 is a naff/"leet" "swit(c)h").

-- in pglobals.e:
--constant newBase = 2  -- 0 = old style, with base @ ref*4-20 on seq/str
                        -- 2 = new style, with slack @ ref*4-20 for seq only.
                        -- (nb: an error occurs if debugleak and newBase do not
                        --      match the backend.)

--
--  Technical note / Vague-ness-ism warning.
--  ========================================
--  The concept of "take part info" is rather vague, although it does produce
--  better results. While I am fairly clear in my own head of the effects of
--  getting it wrong, I cannot really say the same about why it is right, or
--  adequately express its place within a repeated iterative analysis.
--
--  This only ever occurs when a variable is on both sides of an assigmnent.
--  A statement such as v = v&x (along with several other forms of concat,
--  append, prepend, etc) can rely on the existing element types of v, just
--  as long as we know/have proved that v cannot be an atom. This prevents
--  a later eg v = 1.5 from "forgetting that elements of v can be floats".
--  In practice, if we're confident about the element types being ok, then
--  we can be sure all the rest (well, gtype and length) are too. In some
--  cases it can also reduce the number of iterations needed.
--
--  If any of this vagueness worries you, think:
--      * about repeated iterations through the code.
--      * order of code analysis may differ from order of execution.
--      * using info learnt on earlier passes to improve each iteration.
--      * never assuming any info we don't yet fully have.
--
--  Hopefully someone else can explain all this a bit more clearly, and
--  maybe even create a better maxim than "not an atom".
--

--
--constant cstats=01
----integer s1,s1i,s1is,s1ip,stot
----        s1 = 0
----        s1i = 0
----        s1is = 0
----        s1ip = 0
--integer a,ai,aiii,atot
--  a = 0 ai = 0 aiii = 0
--global procedure show_cstats()
--  if cstats then
------stot = s1+s1i+s1is+s1ip
------printf(1,"opSubse1:%d[%3.2f%%], opSubse1i:%d[%3.2f%%], opSubse1is:%d[%3.2f%%], opSubse1ip:%d[%3.2f%%]\n",
------       {s1,s1/stot, s1i,s1i/stot, s1is,s1is/stot, s1ip,s1ip/stot})
--atot = a+ai+aiii
--printf(1,"opAdd:%d[%3.2f%%], opAddi:%d[%3.2f%%], opAddiii:%d[%3.2f%%]\n",
--       {a,a/atot, ai,ai/atot, aiii,aiii/atot})
--  end if
--end procedure


----DEV not Phix...
--function o(integer d1, integer d2, integer d3)
---- octal digit, eg o(3,7,7) returns #FF aka 255
--  return d1*#40+d2*#08+d3     -- d1*0o100+d2*0o10+d3
--end function

--
-- x86 instruction opcodes, minimal required set.
-- =============================================
--
-- The use of octal exposes some of the xrm and sib settings.
-- Credit/blame: I mainly used FASM and OllyDbg to determine the instruction encodings.
-- TIP: run "p -d test.exw" (or better yet "p.exe p.exw -d test.exw", to test mods out
--  before actually attempting to rebuild the compiler) and examine list.asm to verify
--  the binary disassembles correctly. (If there are any idiots in the house, test.exw
--  is something to make yourself.) If you add more instructions here, they probably
--  are already supported by p2asm.e, except perhaps some recentish P6+/64-bit stuff.
-- Please note that these constants are really intended to make mistakes easier to spot,
--  rather than (although I hope they help a bit) trying to make x86 binary easy, which
--  is of course an almost impossible task, given how wierd & convoluted it tends to be.
--  Some items may be badly named, eg "cmp_edx_sib" should probably be "cmp_edx_sibd8".
--  Some instructions have an opcode extension in the xrm, eg shl/shr/sar/etc are all
--  #D1,xSm with S=4/5/7 respectively. These constants are not really a suitable way to
--  encode such instructions, so instead they are just coded using inline hex directly.
--  (update: regimm365 & m_constants do one small subset of such instructions, btw.)
-- Commented out entries below are simply those no longer in use, they should all work.
--
constant
--       add_eax_ecx    = {#01,#C8},    -- 0o001 0o310              -- add eax,ecx
--       add_eax_mem32  = {#03,#05},    -- 0o003 0o005 mem32        -- add eax,[mem32]
--       add_edx_eax    = {#03,#D0},    -- 0o003 0o320              -- add edx,eax
--       add_edx_ecx    = {#03,#D1},    -- 0o003 0o321              -- add edx,ecx
--       add_eax_imm32  =  #05,         -- 0o005 imm32              -- add eax,imm32
--       or_edx_edx     = {#09,#D2},    -- 0o011 0o322              -- or edx,edx
--       or_eax_mem32   = {#0B,#05},    -- 0o013 0o005 mem32        -- or eax,[mem32]
--       or_eax_imm32   =  #0D,         -- 0o015 imm32              -- or eax,imm32                         
--                                                                                                              flags
         jo_rel32       = {#0F,#80},    -- 0o017 0o200 imm32        -- jo rel32                                 O = 1
         jno_rel32      = {#0F,#81},    -- 0o017 0o201 imm32        -- jno rel32                                O = 0
--       jc_rel32       = {#0F,#82},    -- 0o017 0o202 imm32        -- jc rel32     -- == jb, == jnae           C = 1
--       jae_rel32      = {#0F,#83},    -- 0o017 0o203 imm32        -- jae rel32    -- == jnc, == jnb           C = 0
         jnc_rel32      = {#0F,#83},    -- 0o017 0o203 imm32        -- jnc rel32
         je_rel32       = {#0F,#84},    -- 0o017 0o204 imm32        -- je rel32     -- == jz                    Z = 1
         jz_rel32       = {#0F,#84},    -- 0o017 0o204 imm32        -- jz rel32
         jne_rel32      = {#0F,#85},    -- 0o017 0o205 imm32        -- jne rel32    -- == jnz                   Z = 0
         jnz_rel32      = {#0F,#85},    -- 0o017 0o205 imm32        -- jnz rel32
--       jbe_rel32      = {#0F,#86},    -- 0o017 0o206 imm32        -- jbe rel32    -- == jna                   C = 1 or Z = 1
--       ja_rel32       = {#0F,#87},    -- 0o017 0o207 imm32        -- ja rel32     -- == jnbe                  C = 0 and Z = 0
--       js_rel32       = {#0F,#88},    -- 0o017 0o210 imm32        -- js rel32                                 S = 1
         jns_rel32      = {#0F,#89},    -- 0o017 0o211 imm32        -- jns rel32                                S = 0
--       jp_rel32       = {#0F,#8A},    -- 0o017 0o212 imm32        -- jp rel32     -- == jpe                   P = 1
--       jnp_rel32      = {#0F,#8B},    -- 0o017 0o213 imm32        -- jnp rel32    -- == jpo                   P = 0
         jl_rel32       = {#0F,#8C},    -- 0o017 0o214 imm32        -- jl rel32     -- == jnge                  S != O
         jge_rel32      = {#0F,#8D},    -- 0o017 0o215 imm32        -- jge rel32    -- == jnl                   S = O
         jle_rel32      = {#0F,#8E},    -- 0o017 0o216 imm32        -- jle rel32    -- == jng                   Z = 1 or (S!=O)
         jg_rel32       = {#0F,#8F},    -- 0o017 0o217 imm32        -- jg rel32     -- == jnle                  Z = 0 and (S=O)
         sete           = {#0F,#94},    -- 0o017 0o224 xrm          -- setz r/m8
         setne          = {#0F,#95},    -- 0o017 0o225 xrm          -- setne r/m8
         setl           = {#0F,#9C},    -- 0o017 0o234 xrm          -- setl r/m8
--       setge          = {#0F,#9D},    -- 0o017 0o235 xrm          -- setge r/m8
--       setle          = {#0F,#9E},    -- 0o017 0o236 xrm          -- setle r/m8
--       setg           = {#0F,#9F},    -- 0o017 0o237 xrm          -- setg r/m8
--       and_eax_mem32  = {#23,#05},    -- 0o043 0o005 mem32        -- and eax,[mem32]
         and_eax_imm32  =  #25,         -- 0o045 mem32              -- and eax,imm32
--       sub_eax_mem32  = {#2B,#05},    -- 0o053 0o005 mem32        -- sub eax,[mem32]
--       sub_eax_imm32  =  #2D,         -- 0o055 imm32              -- sub eax,imm32
         xor_eax_eax    = {#31,#C0},    -- 0o061 0o300              -- xor eax,eax      (== {#33,#C0})
         xor_ebx_ebx    = {#31,#DB},    -- 0o061 0o333              -- xor ebx,ebx
         xor_ecx_ecx    = {#31,#C9},    -- 0o063 0o311              -- xor ecx,ecx
         xor_edx_edx    = {#31,#D2},    -- 0o063 0o322              -- xor edx,edx
--       xor_esi_esi    = {#31,#F6},    -- 0o063 0o366              -- xor esi,esi
         xor_edi_edi    = {#31,#FF},    -- 0o063 0o377              -- xor edi,edi
--       xor_eax_mem32  = {#33,#05},    -- 0o063 0o005 mem32        -- xor eax,[mem32]
--       xor_eax_imm32  =  #35,         -- 0o065 imm32              -- xor ecx,imm32
--       cmp_reg_regr   =  #39,         -- 0o071 0o3rt              -- cmp tst,reg
         cmp_reg_reg    =  #3B,         -- 0o073 0o3rt              -- cmp reg,tst
         cmp_ecx_edx    = {#39,#D1},    -- 0o071 0o321              -- cmp ecx,edx
--       cmp_eax_mem32  = {#3B,#05},    -- 0o073 0o005 mem32        -- cmp eax,[mem32]
--       cmp_edx_mem32  = {#3B,#15},    -- 0o073 0o025 mem32        -- cmp edx,[mem32]
--       cmp_edx_sib    = {#3B,#54},    -- 0o073 0o124 sib d8       -- cmp edx,[b32+i32*s+d8]
--       cmp_edi_sib    = {#3B,#7C},    -- 0o073 0o174 sib d8       -- cmp edi,[b32+i32*s+d8]
--       cmp_eax_esi    = {#3B,#C6},    -- 0o073 0o306              -- cmp eax,esi
         cmp_eax_imm32  =  #3D,         -- 0o075 imm32              -- cmp eax,imm32
--DEV invalid in 64-bit mode:
         inc_eax        =  #40,         -- 0o100                    -- inc eax
--       inc_ecx        =  #41,         -- 0o101                    -- inc ecx
--       inc_edx        =  #42,         -- 0o102                    -- inc edx
--       dec_eax        =  #48,         -- 0o110                    -- dec eax
--       dec_edx        =  #4A,         -- 0o112                    -- dec edx
--       dec_edi        =  #4F,         -- 0o117                    -- dec edi
         push_eax       =  #50,         -- 0o120                    -- push eax     ..#57 for other regs
         push_edx       =  #52,         -- 0o122                    -- push edx
         push_esi       =  #56,         -- 0o126                    -- push esi
         pop_eax        =  #58,         -- 0o130                    -- pop eax  ..#5F for other regs
         pop_esi        =  #5E,         -- 0o136                    -- pop esi
--       pop_edi        =  #5F,         -- 0o137                    -- pop edi
--       pushad         =  #60,         -- 0o140                    -- pushad
--       popad          =  #61,         -- 0o141                    -- popad
--       wd_prfx        =  #66,         -- 0o146                    -- <word prefix>
         push_imm32     =  #68,         -- 0o150 imm32              -- push imm32
         push_imm8      =  #6A,         -- 0o152 imm8               -- push imm8 (as machine word size)

--
-- WARNING: While it may seem daft to emit a 32-bit jump that you /know/ will fit in
--          an 8-bit offset, I have regretted doing so virtually every time.
--
--          All 32-bit jumps are automatically converted to 8-bit form when possible.
--          This involves examining each offset, adjusting it as needed if/when any
--          surrounding/jumped over instructions get shortened, checking whether it
--          fits in a byte and if so patching it into short form.
--          Because of that complexity, it is actually cheaper to have one method of
--          adjusting offsets, rather than one for dwords and one for bytes. Hence
--          there is no "byte offset adjustment", if you use short jumps that is it,
--          the byte offset specified will not be altered even if it ought to be.
--
--          Technically, completely fixed offsets, that do not jump over any 32-bit
--          jumps or isDead blocks and would not benefit from branch straightening,
--          could be output in short form, and that would indeed shave a fraction
--          off compilation time (never actually measured). In a bit of a hissy fit
--          I commented out all short jumps and inserted 32-bit replacements. Just
--          be warned that over-zealous use of short jumps can really bite. The one
--          that caught me out big-time/I totally missed was branch straightening.
--          As long as there is equivalent 32-bit code next to them in case of
--          future emergencies, if you can prove any short jump use actually saves
--          a measureable time, I do not mind them going back in. I have marked up
--          most jumps with [sj OK] or [sj NOT ok] on the understanding there will
--          be extensive testing on your part and no blame on mine.
--
--          Use the -list option to prove things are ending up the way you'd like,
--          whether that be a 32-bit jump you know fits in a byte or an 8-bit jump
--          you are so keen to emit does not (sometimes) land mid-instruction or
--          miss out on a branch straightening opportunity (land on a jmp), etc.
--
--       jo_rel8        =  #70,         -- 0o160 imm8               -- jo rel8
--       jno_rel8       =  #71,         -- 0o161 imm8               -- jno rel8
--       jc_rel8        =  #72,         -- 0o162 imm8               -- jc rel8
--       jnc_rel8       =  #73,         -- 0o163 imm8               -- jnc rel8
--       jae_rel8       =  #73,         -- 0o163 imm8               -- jae rel8     -- == jnc, == jnb
--       je_rel8        =  #74,         -- 0o164 imm8               -- je rel8
--       jz_rel8        =  #74,         -- 0o164 imm8               -- jz rel8
--       jne_rel8       =  #75,         -- 0o165 imm8               -- jne rel8
--       jnz_rel8       =  #75,         -- 0o165 imm8               -- jnz rel8
--       jbe_rel8       =  #76,         -- 0o166 imm8               -- jbe rel8
--       ja_rel8        =  #77,         -- 0o167 imm8               -- ja rel8
--       js_rel8        =  #78,         -- 0o170 imm8               -- js rel8
--       jns_rel8       =  #79,         -- 0o171 imm8               -- jns rel8
--       jl_rel8        =  #7C,         -- 0o174 imm8               -- jl rel8
--       jge_rel8       =  #7D,         -- 0o175 imm8               -- jge rel8
--       jle_rel8       =  #7E,         -- 0o176 imm8               -- jle rel8
--       jg_rel8        =  #7F,         -- 0o177 imm8               -- jg rel8

         cmpb_sibd8i8   = {#80,#7C},    -- 0o200 0o174 sib d8 i8    -- cmp byte[b32+i32*s+d8],i8
--       cmp_mem32_i32  = {#81,#3D},    -- 0o201 0o075 m32 i32      -- cmp [m32],imm32
--       cmpd_sibd8i32  = {#81,#7C},    -- 0o201 0o174 sib d8 i32   -- cmp dword[b32+i32*s+d8],i32
         cmp_reg_imm32  =  #81,         -- 0o201 0o37r imm32        -- cmp reg,imm32 (not eax)
--       cmp_ecx_imm32  = {#81,#F9},    -- 0o201 0o371 imm32        -- cmp ecx,imm32
--       cmp_edx_imm32  = {#81,#FA},    -- 0o201 0o372 imm32        -- cmp edx,imm32
--       cmp_esi_imm32  = {#81,#FE},    -- 0o201 0o376 imm32        -- cmp esi,imm32
--       cmp_edi_imm32  = {#81,#FF},    -- 0o201 0o377 imm32        -- cmp edi,imm32
--       cmpd_mem32_i8  = {#83,#3D},    -- 0o203 0o075 m32 imm8     -- cmp dword[m32],imm8
         addd_subd8i8   = {#83,#44},    -- 0o203 0o104 sib d8 i8    -- add dword[b32+i32*s+d8],i8
         subd_sibd8i8   = {#83,#6C},    -- 0o203 0o154 sib d8 i8    -- sub dword[b32+i32*s+d8],i8
         cmpd_sibd8i8   = {#83,#7C},    -- 0o203 0o174 sib d8 i8    -- cmp dword[b32+i32*s+d8],i8
--       add_eax_imm8   = {#83,#C0},    -- 0o203 0o300 imm8         -- add eax,imm8
         add_esp_imm8   = {#83,#C4},    -- 0o203 0o304 imm8         -- add esp,imm8
--       or_eax_imm8    = {#83,#C8},    -- 0o203 0o310 imm8         -- or eax,imm8
--       and_eax_imm8   = {#83,#E0},    -- 0o203 0o340 imm8         -- and eax,imm8
--       xor_eax_imm8   = {#83,#F0},    -- 0o203 0o360 imm8         -- xor eax,imm8
--       sub_eax_imm8   = {#83,#E8},    -- 0o203 0o350 imm8         -- sub eax,imm8
         sub_esp_imm8   = {#83,#EC},    -- 0o203 0o354 imm8         -- sub esp,imm8
--       cmp_eax_imm8   = {#83,#F8},    -- 0o203 0o370 imm8         -- cmp eax,imm8
--       cmp_edx_imm8   = {#83,#FA},    -- 0o203 0o372 imm8         -- cmp edx,imm8
         test_reg_reg   =  #85,         -- 0o205 0o3rr              -- test reg,reg
--       test_eax_eax   = {#85,#C0},    -- 0o205 0o300              -- test eax,eax
         test_ecx_ecx   = {#85,#C9},    -- 0o205 0o311              -- test ecx,ecx
         test_edx_edx   = {#85,#D2},    -- 0o205 0o322              -- test edx,edx
         xchg           =  #87,         -- 0o207 0o3rr              -- xchg reg,reg
--       xchg_esi_edi   = {#87,#FE},    -- 0o207 0o376              -- xchg esi,edi
--       mov_sib_al     = {#88,#04},    -- 0o210 0o004 sib          -- mov [b32+i32*s],al
--       mov_medx_eax   = {#89,#02},    -- 0o211 0o002              -- mov [edx],eax
--       mov_sib_eax    = {#89,#04},    -- 0o211 0o004 sib          -- mov [b32+i32*s],eax
--       mov_mesi_eax   = {#89,#06},    -- 0o211 0o006              -- mov [esi],eax
         mov_medi_eax   = {#89,#07},    -- 0o211 0o007              -- mov [edi],eax
--       mov_mem32_ecx  = {#89,#0D},    -- 0o211 0o015 m32          -- mov [m32],ecx
--       mov_mesi_edx   = {#89,#16},    -- 0o211 0o026              -- mov [esi],eax
--       mov_mem32_edx  = {#89,#15},    -- 0o211 0o025 m32          -- mov [m32],edx
         mov_mem32_ebx  = {#89,#1D},    -- 0o211 0o035 m32          -- mov [m32],ebx
         mov_mem32_esi  = {#89,#35},    -- 0o211 0o065 m32          -- mov [m32],esi (or r14 with #4C)
         mov_ebpi8_eax  = {#89,#45},    -- 0o211 0o105 imm8         -- mov [ebp+imm8],eax
--       mov_ebpi8_edx  = {#89,#55},    -- 0o211 0o125 imm8         -- mov [ebp+imm8],edx
         mov_ebpi8_ebx  = {#89,#5D},    -- 0o211 0o135 imm8         -- mov [ebp+imm8],ebx(0)
         mov_rbpi8_r14  = {#89,#75},    -- 0o211 0o165 imm8         -- mov [rbp+imm8],r14 (needs a #4C)
         mov_ebpi32_ebx = {#89,#9D},    -- 0o211 0o235 imm32        -- mov [ebp+imm32],ebx(0)
         mov_rbpi32_r14 = {#89,#B5},    -- 0o211 0o265 imm32        -- mov [rbp+imm32],r14 (needs a #4C)
--       mov_ecx_eax    = {#89,#C1},    -- 0o211 0o301              -- mov ecx,eax  -- see 8B version below
--       mov_edx_eax    = {#89,#C2},    -- 0o211 0o302              -- mov edx,eax  -- ""
--       mov_esi_eax    = {#89,#C6},    -- 0o211 0o306              -- mov esi,eax  -- ""
--       mov_ecx_edx    = {#89,#D1},    -- 0o211 0o321              -- mov ecx,edx  -- ""
         mov_reg        =  #89,         -- 0o211 0o3sd              -- mov dst,src
         mov_byte       =  #8A,         -- 0o212 xrm (sib etc)      -- mov r8, various
--       mov_al_sib     = {#8A,#04},    -- 0o212 0o004 sib          -- mov al,[b32+i32]
--       mov_dl_sib     = {#8A,#14},    -- 0o212 0o024 sib          -- mov dl,[b32+i32]
--       mov_dl_sibd8   = {#8A,#54},    -- 0o212 0o124 sib d8       -- mov dl,[b32+i32*s+d8]
--       mov_al_esid8   = {#8A,#46},    -- 0o212 0o106 d8           -- mov al,[esi+d8]
--       mov_al_esid32  = {#8A,#86},    -- 0o212 0o206 d32          -- mov al,[esi+d32]
--       mov_dl_sibd32  = {#8A,#94},    -- 0o212 0o224 sib d32      -- mov al,[b32+i32*s+d32]
         mov_dword      =  #8B,         -- 0o213 xrm (sib etc)      -- mov r32, various
--       mov_reg2       =  #8B,         -- 0o213 0o3ds              -- mov dst,src
--       mov_eax_sib    = {#8B,#04},    -- 0o213 0o004 sib          -- mov eax,[b32+i32*s]
--       mov_eax_medi   = {#8B,#07},    -- 0o213 0o007              -- mov eax,[edi]
--       mov_ecx_mem32  = {#8B,#0D},    -- 0o213 0o015 m32          -- mov ecx,[m32]
--       mov_ecx_mesi   = {#8B,#0E},    -- 0o213 0o016              -- mov ecx,[esi]
--       mov_edx_sib    = {#8B,#14},    -- 0o213 0o024 sib          -- mov edx,[b32+i32*s]
--       mov_edx_mem32  = {#8B,#15},    -- 0o213 0o025 m32          -- mov edx,[m32]
--       mov_edx_mesi   = {#8B,#16},    -- 0o213 0o026              -- mov edx,[esi]
--       mov_edx_medi   = {#8B,#17},    -- 0o213 0o027              -- mov edx,[edi]
--       mov_ebx_mem32  = {#8B,#1D},    -- 0o213 0o035 m32          -- mov ebx,[m32]
--       mov_esi_mem32  = {#8B,#35},    -- 0o213 0o065 m32          -- mov esi,[m32]
--       mov_edi_medx   = {#8B,#3A},    -- 0o213 0o072              -- mov edi,[edx]
--       mov_edi_mem32  = {#8B,#3D},    -- 0o213 0o075 m32          -- mov edi,[m32]
         mov_eax_ebpd8  = {#8B,#45},    -- 0o213 0o105 d8           -- mov eax,[ebp+d8]
--       mov_eax_esid8  = {#8B,#46},    -- 0o213 0o106 d8           -- mov eax,[esi+d8]
--       mov_eax_edid8  = {#8B,#47},    -- 0o213 0o107 d8           -- mov eax,[edi+d8]
--       mov_edx_ebpd8  = {#8B,#55},    -- 0o213 0o125 d8           -- mov edx,[ebp+d8]
         mov_edi_ebpd8  = {#8B,#7D},    -- 0o213 0o175 d8           -- mov edi,[ebp+d8]
--       mov_ecx_sibd8  = {#8B,#4C},    -- 0o213 0o114 sib d8       -- mov ecx,[b32+i32*s+d8]
--       mov_esi_sibd8  = {#8B,#74},    -- 0o213 0o164 sib d8       -- mov esi,[b32+i32*s+d8]
--       mov_eax_esid32 = {#8B,#86},    -- 0o213 0o206 d32          -- mov eax,[esi+d32]
--       mov_eax_edid32 = {#8B,#87},    -- 0o213 0o207 d32          -- mov eax,[edi+d32]
         mov_eax_edx    = {#8B,#C2},    -- 0o213 0o302              -- mov eax,edx
         mov_eax_ebx    = {#8B,#C3},    -- 0o213 0o303              -- mov eax,edx
         mov_ecx_eax    = {#8B,#C8},    -- 0o213 0o310              -- mov ecx,eax
         mov_ecx_edx    = {#8B,#CA},    -- 0o213 0o312              -- mov ecx,edx
         mov_ecx_esp    = {#8B,#CC},    -- 0o213 0o314              -- mov ecx,esp
         mov_edx_eax    = {#8B,#D0},    -- 0o213 0o320              -- mov edx,eax
--       mov_edx_ebp    = {#8B,#D5},    -- 0o213 0o325              -- mov edx,ebp
         mov_esp_ecx    = {#8B,#E1},    -- 0o213 0o341              -- mov esp,ecx
         mov_esi_eax    = {#8B,#F0},    -- 0o213 0o360              -- mov esi,eax
--       mov_edi_ebp    = {#8B,#FD},    -- 0o213 0o375              -- mov edi,ebp
--       lea_eax_ecdx   = {#8D,#04,#0A},-- 0o215 0o004 0o012        -- lea eax,[ecx+edx]
--       lea_esi_ecdx   = {#8D,#34,#0A},-- 0o215 0o064 0o012        -- lea esi,[ecx+edx]
         lea            =  #8D,         -- 0o215 xrm [d8/32]
         pop_mem32      = {#8F,#05},    -- 0o217 0o005 mem32        -- pop dword[mem32]
         pop_ebpi8      = {#8F,#45},    -- 0o217 0o105 imm8         -- pop dword[ebp+imm8]
         pop_ebpi32     = {#8F,#85},    -- 0o217 0o205 imm32        -- pop dword[ebp+imm32]
         nop            =  #90,         -- 0o220                    -- cdq (eax-> edx:eax)
         cdq            =  #99,         -- 0o231                    -- cdq (eax-> edx:eax)
         mov_eax_mem32  =  #A1,         -- 0o241 m32                -- mov eax,[m32]
         mov_mem32_eax  =  #A3,         -- 0o243 m32                -- mov [m32],eax
         mov_al_imm8    =  #B0,         -- 0o260 imm8               -- mov al,imm8
         mov_eax_imm32  =  #B8,         -- 0o270 imm32              -- mov eax,imm32
--       mov_ecx_imm32  =  #B9,         -- 0o271 imm32              -- mov ecx,imm32
         mov_edx_imm32  =  #BA,         -- 0o272 imm32              -- mov edx,imm32
--       mov_ebx_imm32  =  #BB,         -- 0o273 imm32              -- mov ebx,imm32
--       mov_esi_imm32  =  #BE,         -- 0o276 imm32              -- mov esi,imm32
         mov_edi_imm32  =  #BF,         -- 0o277 imm32              -- mov edi,imm32
         shl_ecx_imm8   = {#C1,#E1},    -- 0o301 0o341 imm8         -- shl ecx,imm8
--       shl_esi_imm8   = {#C1,#E6},    -- 0o301 0o346 imm8         -- shl esi,imm8
--       shr_eax_imm8   = {#C1,#E8},    -- 0o301 0o350 imm8         -- shr eax,imm8
         shr_ecx_imm8   = {#C1,#E9},    -- 0o301 0o351 imm8         -- shr ecx,imm8
--       sar_eax_imm8   = {#C1,#F8},    -- 0o301 0o370 imm8         -- sar eax,imm8
         sar_edx_imm8   = {#C1,#FA},    -- 0o301 0o372 imm8         -- sar edx,imm8
         mov_m32_imm32  = {#C7,#05},    -- 0o307 0o005 m32 i32      -- mov [m32],imm32
         mov_medi_im32  = {#C7,#07},    -- 0o307 0o007 imm32        -- mov [edi],imm32
--       mov_m32sib8i32 = {#C7,#44},    -- 0o307 0o104 sib d8 imm32 -- mov [b32+i32*s+d8],imm32
--       mov_regd8_i32  =  #C7,         -- 0o307 0o10r d8 imm32     -- mov [reg+d8],imm32
         mov_ebpd8_i32  = {#C7,#45},    -- 0o307 0o105 d8 imm32     -- mov [ebp+d8],imm32
         mov_edid8_i32  = {#C7,#47},    -- 0o307 0o107 d8 imm32     -- mov [edi+d8],imm32
--       mov_regd32_i32 =  #C7,         -- 0o307 0o20r d8 imm32     -- mov [reg+d32],imm32
         mov_ebpd32_i32 = {#C7,#85},    -- 0o307 0o205 d32 imm32    -- mov [ebp+d32],imm32
         mov_edid32_i32 = {#C7,#87},    -- 0o307 0o207 d32 imm32    -- mov [edi+d32],imm32
         mov_regimm32   =  #C7,         -- 0o307 0o30r imm32        -- mov reg,imm32
--       shl_eax_1      = {#D1,#E0},    -- 0o321 0o340              -- shl eax,1
         shl_ecx_1      = {#D1,#E1},    -- 0o321 0o341              -- shl ecx,1
         shl_edx_1      = {#D1,#E2},    -- 0o321 0o342              -- shl edx,1
         shl_esi_1      = {#D1,#E6},    -- 0o321 0o346              -- shl esi,1
--       shr_eax_1      = {#D1,#E8},    -- 0o321 0o350              -- shr eax,1
         sar_eax_1      = {#D1,#F8},    -- 0o321 0o370              -- sar eax,1
         shl_eax_cl     = {#D3,#E0},    -- 0o323 0o340              -- shl eax,cl
--       fild_mem32     = {#DB,#05},    -- 0o333 0o005 mem32        -- fild dword[mem32]
--       fistp_d_esp    = {#DB,#1C,#24},-- 0o333 0o034 0o044        -- fistp dword[esp]
--       fistp_q_esp    = {#DF,#3C,#24},-- 0o337 0o074 0o044        -- fistp qword[esp]
         call_rel32     =  #E8,         -- 0o350 imm32              -- call rel32
         jump_rel32     =  #E9,         -- 0o351 imm32              -- jmp rel32
--       jump_rel8      =  #EB,         -- 0o353 imm8               -- jmp rel8
--       rep_movsb      = {#F3,#A4},    -- 0o363 0o244              -- rep movsb
--       rep_movsd      = {#F3,#A5},    -- 0o363 0o244              -- rep movsd
--       rep_stosd      = {#F3,#AB},    -- 0o363 0o253              -- rep stosd
         tstb_sibd8i8   = {#F6,#44},    -- 0o366 0o104 sib d8 i8    -- test byte[b32+i32*s+d8],i8
--       neg_edx        = {#F7,#DA},    -- 0o367 0o332              -- neg edx
         imul_ecx       = {#F7,#E9},    -- 0o367 0o351              -- imul ecx
         idiv_ecx       = {#F7,#F9},    -- 0o367 0o371              -- idiv ecx
         inc_mem32      = {#FF,#05},    -- 0o377 0o005 mem32        -- inc dword[mem32]
--       dec_mem32      = {#FF,#0D},    -- 0o377 0o015 mem32        -- dec dword[mem32]
--nb mov reg,[ecx+eax*4-8]; inc reg; mov [ecx+eax*4-8],reg offers better opportunties for peephole opts....
--       jmp_mem32      = {#FF,#15},    -- 0o377 0o025 mem32        -- jmp dword[mem32]
         jmp_si5_imm32  = {#FF,#24},    -- 0o377 0o044 si5 imm32    -- jmp dword[i*s+imm32]
         push_mem32     = {#FF,#35},    -- 0o377 0o065 mem32        -- push dword[mem32]

--DEV to go under newEmit (*2):
         incd_sib       = {#FF,#44},    -- 0o377 0o104 sib d8       -- inc dword[b32+i32*s+d8]
         decd_sib       = {#FF,#4C},    -- 0o377 0o114 sib d8       -- dec dword[b32+i32*s+d8]

         push_ebpi8     = {#FF,#75},    -- 0o377 0o165 i8           -- push dword[ebp-nn]
         push_ebpi32    = {#FF,#B5}     -- 0o377 0o265 i32          -- push dword[ebp-nnnn]

-- nb: the above tables contains some xrm bytes, for readability, whereas for the
--      most part they are constructed by hand and commented, eg #C0+reg -- 0o30r.

constant -- sib bytes (nb the naming convention is base_idx[scale])
--       esi_edx        =  #32,         -- 0o062 (==0o026|#16)      -- [esi+edx]
--       esi_edi        =  #3E,         -- 0o076 (==0o067|#37)      -- [esi+edi]
--       ecx_eax4       =  #81,         -- 0o201                    -- [ecx+eax*4]
         ebx_eax4       =  #83,         -- 0o203                    -- [ebx+eax*4]
--       eax_ecx4       =  #88,         -- 0o210                    -- [eax+ecx*4]
--       ebx_ecx4       =  #8B,         -- 0o213                    -- [ebx+ecx*4]
--       ecx_edx4       =  #91,         -- 0o221                    -- [ecx+edx*4]
         ebx_edx4       =  #93          -- 0o223                    -- [ebx+edx*4]
--       ebp_edx4       =  #95          -- 0o225                    -- [ebp+edx*4]
--       esi_edx4       =  #96          -- 0o226                    -- [esi+edx*4]
--       ebx_esi4       =  #B3,         -- 0o263                    -- [ebx+esi*4]
--       esi_edi4       =  #BE          -- 0o276                    -- [esi+edi*4]

--
-- generate a most recently used state table.
-- for eax(1), ecx(2), esi(3), edi(4) we want to know which was used last,
--  and when a reg is used promote it to top and lower others as needed.
--
-- Should it appear to be misbehaving, the code from constant eax=0 to
--  procedure merge can easily be copied into a test program, simply
--  comment out any emitHexXx calls.
--
constant eax=0, ecx=1, edx=2, ebx=3,
--       esp=4, 
--       ebp=5, 
         esi=6, edi=7
--if ebx or esp or ebp then end if  -- suppress warnings

    --
    -- Example of isOpCode:
    --  emitHex5s({jump_rel32,isOpCode,0,0,opRetf})
    --      -->     #E9,<byteified VMep[opRetf]-addr_next_instruction>
    --  The translation from isOpCode,0,0,VMidx to an offset cannot be performed
    --  until we know where the actual code address is, ie after we have decided
    --  which routines are required, shortened jumps, removed isDead blocks, etc.
    --
--DEV work in progress:
    -- Example of isApiFn: (old style, unfinished)
    --  #ilasm{call_mem32,%isApiFn,0,0,%opInitialiseCriticalSection}
    --      -->     #FF,#15,<byteified VMep[opInitialiseCriticalSection]>
    --      ==      jmp dword[<somewhere-in-the-RVA>]
    -- Example of isAPIfn: (new style, just started)
    --  #ilASM{call "kernel32","FillConsoleOutputCharacterA"}
    --  #ilasm{call_mem32,%isAPIfn,"kernel32.dll","HeapAlloc"}
    --      -->     #FF,#15,<byteified import section thunk>
    --      ==      jmp dword[import section entry]
    --  Note that only a few selected windows api calls are currently available,    [DEV watch this space]
    --  run p -imports or demo\arwendemo\filedump.exw for the full list. Primarily
    --  for threads, where the initial setup could not be done via pcfunc.e etc 
    --  because they would act on the wrong stack (if called before the new stack 
    --  was properly set up). Also used for fast file I/O [DEV...]
    -- I really only ever intend to make a handful available [why?]
    --  that help migrate code out of the backend into #ilasm, or prevent more code 
    --  being added to the backend, though I may be persuaded into other additions. 
    --
    --  One possible alternative might be to create a whole new import section: use     [ IN PROGRESS ]
    --  (eg) demo/filedump, then in pemit.e make sure you get all those entries plus
    --  any others you need, written somewhere else in the file and patching a few 
    --  RVA and Section values. Point SetExceptionHandler (since that is the first
    --  thing to get called) in the original import table to a new block of code 
    --  which sets the original import table from the new (rather than hunt down
    --  and fixup existing references in the VM code) and finally completes the 
    --  SetExceptionHandler call. Good luck with that ;-)
    --
    -- Example of isVar:
    --  emitHex5v(mov_eax_imm32,p2)     -- mov eax,p2 (address of p2)
    --      -->     #B8,isVar,0,0,idx
    --      -->     #B8,<byteified DSvaddr+idx*4-4>
--DEV newEBP(?)
    --  Note that while tvars get their final idx pretty much at creation time,
    --  gvars and consts don't get theirs until very late on, start of finalfixups
    --  in fact.
    --
--DEV isConstRef no longer valid under newEmit
    -- isConstRef and isConstRefCount are used in eg name="fred". Since the compiler
    --  created that "fred" and knows exactly where it put it, it is quicker to use a
    --  literal ref/address of refcount, rather than load/compute then at run-time.
    -- isILa is currently only used in user defined type checking.
    -- isIL is currently only used in opFrame/set params/Jmp(#E9),isIL,0,0,routine_no.
    --  Obtains the alsolute or relative address of the entry point for a routine.
    --  They were both derived from the isAddr concept, although that has since been
    --  extended with branch straightening/merged a bit more with isJmp.
    --
    -- Explanation of isAddr:
    --  The calling convention for opcodes such as opMkSq (make sequence) is:
    --
    --      mov edx,N
    --      push L1
    --      push items N..1
    --      jmp opMkSq
    --    L1:
    --
    --  so that it can pop N items off the stack and return, rather than have to save
    --  the return address somewhere before it can start popping. (For clarity, there
    --  are two other pushes, result addr and previous value, missing from the above.)
    --  isAddr implements the above as a relative offset which is translated to an
    --  actual code address, you guessed it, once we know where that is.
    --
    -- Explanation of isBase:
    --  Suppose you have:
    --      switch i
    --          case 4004
    --          ...
    --          case 4017
    --      end switch
    --  Then the jump table can/should contain 14 addresses. The same would be true if
    --  the cases ranged from -4 to -17. Rather than add or subtract something from the
    --  index before using it on the jump table, we supply a dummy table start which
    --  is calculated at compile-time, in the above example that would be 16012 bytes 
    --  before the actual table. (Of course we may have to do a range check before we
    --  index the jump table, but that changes nothing.) isBase operates in a similar 
    --  manner to isAddr, but without any adjustments for shortened jumps.
    --
    -- Explanation of isJmp/isShortJmp.
    --  To keep things simple (!) the main routine pretty much always emits dword
    --  jumps, it is scanforShortJmp() which decides whether the offset will fit
    --  in a byte and blurph() which recodes those that do. The scanforShortJmp
    --  is a decidedly non-trivial enterprise since shortening one instruction
    --  from 5 or 6 bytes to 2 means that all the offsets of any jumps over it
    --  must also (ultimately) be adjusted. However, this somewhat frightening
    --  complexity is nicely hidden away inside the two mentioned routines and
    --  the main parser can and should emit the longer forms, confident that
    --  they will be properly dealt with. If you like, you may use "p -d test"
    --  (where you supply the test) to prove this to your own satisfaction.
    --
    -- Explanation of isDead:
    --  (No longer in use, but should still work if you need it.)
    --  Suppose at "end function" you realise the result is always an integer,
    --  hence there is no need to deallocate the result var. Way back, when
    --  pmain.e used to emit binary during parsing, it would often patch the
    --  dealloc code. For example #20005 means "five dead bytes at this point".
    --  scanforShortJmp() is responsible for adjusting the offsets of jumps
    --  over isDead blocks: in phase 1 it does the backward jump offsets (only),
    --  and leaves forward jumps to phase 2, by when we can use the fwd chain
    --  (and by when isDead removal has cleaned out the backward chain).
    --  nb #20000,#20001,#20002 are illegal since removal uses two slots:
    --  if necessary (ie to remove 1 or 2 bytes) you would need to plant a
    --  "dummy" #20003,0,0 which can be patched to #20004/5. Obviously, a
    --  #20000 is nonsense since it would clobber the first byte to keep.
    --  Planting an isDead is of course far easier than doing a slice
    --  on x86 in pmain.e and adjusting all the affected jump offsets
    --  and backpatch links by hand there & then.
    --

global sequence x86         -- also used by schend.e
--!/**/ #isginfo{x86,0b0100,MIN,MAX,integer,-2} -- Dsq of integer (unknown length)

global integer schidx -- see schend.e
               schidx = 0

--
-- dword operands are stored (see eg emitHex5) as follows:
--  flag byte (lsb), [as above, eg isVar]
--    0,
--    0,
--  value/offset/index/backpatch link (msb)
--
-- Note that flag bits may be set on the msb, so always scan forward and skip over it.
--
-- The two zeroes in the middle are used by this program for (temporary) linked lists,
--  so again some of the flag bits may therefore end up being set on bytes 2 and 3.
--

integer reginfo = 0,-- "in use" part of mloc/mreg (see below).
                    -- (makes emptying the table nice and quick)
        pfileno = 0

--with trace --DEV (otherwise we get a memory leak!)
--constant m4 = allocate(4),
--       m44 = {m4,4}
----        ,
----         m42 = {m4,2}

procedure emitHexDword(atom v)
-- break up a dword constant into 4 bytes
--  if v<-#80000000 or v>#7FFFFFFF then ?9/0 end if
    if v<-#80000000 or v>#FFFFFFFF then ?9/0 end if
--  if v<-#80000000 or v>#FFFFFFFF then ?{"emitHexDword",v,sprintf("%08x",v),"emitline",emitline,"pfileno",pfileno} end if
    atom m4 = allocate(4)
    poke4(m4, v) -- faster than doing divides etc. (idea from database.e)
    string s = peek({m4,4})
    free(m4)
--DEV tryme (ditto below)
--  x86 &= s
    for i=1 to 4 do
        x86 &= s[i]
    end for
end procedure

procedure emitHexQuadword(atom v)
-- break up a dword constant into 8 bytes
--DEV we should really check this is only invoked for 64-bit.
--    (above check stands for 32-bit, but the solution would 
--     not be to call this, instead emit a proper float-ref.)
    atom m8 = allocate(8)
    poke8(m8, v) -- faster than doing divides etc. (idea from database.e)
    string s = peek({m8,8})
    free(m8)
--  x86 &= s
    for i=1 to 8 do
        x86 &= s[i]
    end for
end procedure


procedure emitHexWord(atom v)
-- break up a word constant into 2 bytes
string s
atom m2 = allocate(2)
    poke2(m2, v) -- faster than doing divides etc. (idea from database.e)
    s = peek({m2,2})
    free(m2)
    for i=1 to 2 do
        x86 &= s[i]
    end for
end procedure

-- for linking up isBase/isAddr/isJmp(/isDead):
integer q86first,
        q86last,
-- ... and for all isOther: (q86>1)
        q86f2,
        q86l2

--DEV: the following could probably be improved on, since q86first/last
-- have been built in order, in the main part of ilxlate(), and the
-- opAsm chain(s) being processed at the end of ilxlate() are also in
-- order, a "merge" rather than this "one at a time, always starting
-- from q86first" theoretically ought to be faster. However, that said
-- most apps won't have a significant number of opAsm to deal with, &
-- also note opLn breakage means that the "chains" on each opAsm will
-- actually only be one entry, so where we currently call this is not
-- appropriate, instead we'd want to reprocess the entire opAsm chain,
-- but still cope with >1 entry per opAsm block.

--with trace
procedure q86insert(integer k)
--  maintain <isJmp,nxt,prv,offset> as an ordered linked list:
--   (the ordering is simply just the position in the code)
--   (k points at isJmp; q86first/q86last limit existing list)
--   (next/prv pointers are all idx to an isJmp)
--  -- (see also demo\lnklst.exw)
integer this, next
--trace(1)
    if q86first then
        if k<q86first then
            x86[k+1] = q86first     -- set next
            x86[k+2] = 0            -- prev
            x86[q86first+2] = k     -- original first's prev
            q86first = k
        else
            this = q86first
            while 1 do
                next = x86[this+1]
                if next=0 or next>k then exit end if
                this = next
            end while
            x86[this+1] = k         -- link prior node to new
            x86[k+1] = next         -- set next on new
            x86[k+2] = this         -- set prev on new
            if next then
                x86[next+2] = k     -- set prev on any following node
            else
                q86last = k
            end if
        end if
    else
        q86first = k
        x86[k+1] = 0        -- next = 0
        x86[k+2] = 0        -- prev = 0
        q86last = k
    end if
end procedure

procedure quad(integer isFlag, integer offset)
-- Emit & linkup an {isBase/isAddr/isJmp,next,prev,offset} quad.
--  next/prev are used to find these things in pemit.e,
--  scanforShortJmp/blurph, during the final fixups.
integer w
    if q86=0 then ?9/0 end if
    x86 &= isFlag   -- isBase/isAddr/isJmp
    w = length(x86)
    x86 &= 0
    x86 &= q86last
    x86 &= offset
    if q86last then
        x86[q86last+1] = w
    else
        q86first = w
    end if
    q86last = w
end procedure

--DEV not (yet) in use (q86 is still==1, ilasm in pmain.e would also need fixing)
procedure quad2(integer isOther, integer i)
-- Emit & linkup an {isOther,next,prev,i} quad.
-- (ie one of isOpCode/isVar/isConstRef[Count]/isIL[a])
--  next/prev are used to find these things in pemit.e,
--  scanforShortJmp/blurph, during the final fixups.
-- (that is when not binding/fast "in situ" processing)
integer w
    if q86<=1 then ?9/0 end if
if newEmit then
    if isOther=isOpCode then ?9/0 end if
    -- isJmpG/isVar/isVno/isConstRef/isConstRefCount/isIL/isILa
end if
    x86 &= isOther  -- isIL[a]/isOpCode/isConstRef[Count]
    w = length(x86)
    x86 &= 0
    x86 &= q86l2
    x86 &= i        -- routine no, opcode, or var no
    if q86l2 then
        x86[q86l2+1] = w
    else
        q86f2 = w
    end if
    q86l2 = w
end procedure

--DEV 18/6/2013:
--integer opLnv, oplnlen, currRtn
integer opLnv, oplnlen

--q86 checked
--sequence call_op
--       call_op = {call_rel32,isOpCode,0,0,0}

integer thisDbg
        thisDbg = 0

--integer callopTraceFirst = 0

forward procedure emitHex5callG(integer opcode, integer lblidx=0)
forward procedure movRegImm32(integer reg, atom v)

--with trace
--global -- used by psched.e    [DEV]
procedure lineinfo()
-- For a proper explanation of LineTab, see pdiag.e (this was written
--  in a relatively ad-hoc manner, sorry).
integer firstline, skipline
--  if lastline!=emitline then  -- now inlined
    if thisDbg then
        firstline = symtab[currRtn][S_1stl]
--?{skipline,emitline,ltline,lastline,LineTab,oplnlen}
        skipline = emitline-firstline+1
        ltline += 1
        if skipline!=ltline then
            -- add negative count of lines which emitted no code:
            if DEBUG then
                -- (has been caused by not setting emitline correctly)
                if ltline>skipline then ?9/0 end if -- major guff
            end if
            LineTab = append(LineTab,ltline-skipline)
            ltline = skipline
        end if
        -- add start offset for this line:
        if DEBUG then
            if oplnlen!=length(x86) then ?9/0 end if
        end if
--      LineTab = append(LineTab,length(x86))
        LineTab = append(LineTab,oplnlen)
--if sched then shfixup() end if
        if not bind then
            if opLnv then       -- set in ilxlate()
--              if sched then
--                  if schidx then
----        schend()
--                      ?9/0    -- do this asap in opLnt etc
--                  end if
--              end if
--if callopTraceFirst then
--  leamov(edi,callopTraceFirst)                    -- lea edi,[src]/mov edi,src
--  emitHex5call(opTrace)                           -- call opXxxx
--  callopTraceFirst = 0
--end if
--if newEmit then
                lastline = emitline
                movRegImm32(eax,emitline)           -- mov eax,imm32
--              x86 &= mov_eax_imm32
--              emitHexDword(emitline)
                if opLnv=opLnp
                or opLnv=opLnpt then
-->NO!! (fileno will always be 1 here!!!)
--                  movRegImm32(ecx,fileno)         -- mov ecx,imm32
                    movRegImm32(ecx,pfileno)        -- mov ecx,imm32
--                  x86 &= mov_ecx_imm32
--                  emitHexDword(fileno)
                end if
--              ?9/0
--?{opLnv,lastline,emitline}
                emitHex5callG(opLnv)                -- call :%pLnt/p/pt
                opLntpcalled = 1
--else
--              x86 &= mov_eax_imm32
--              emitHexDword(emitline)
--              if q86>1 then
--                  x86 &= call_rel32
--                  quad2(isOpCode,opLnv)
--              else
--                  call_op[5] = opLnv
--                  x86 &= call_op
--              end if
--end if
                -- all regs trashed
                reginfo = 0
            end if
        end if
    end if
    lastline = emitline
end procedure

--include psched.e


-- The emitHexNnn routines
-- =======================
--  There are about 20 of these, catering for different instruction lengths and
--  internal makeup. They are all pretty trivial.
--
--  First they check for lineinfo() needed (we note where we are in opLn/p/t/pt,
--  but delay updating tables etc in case we manage to completely optimise away
--  whole chunks of code, and hence we wait until we actually emit something).
--
--  Then they bolt a few bytes on the end of x86.
--
--  They exist mainly as a kind of self-documenting code, with some rudimentary
--  type-checking, and to group whole instructions together. As an added bonus,
--  they make it easy to calculate block sizes and offsets. I do not know them
--  all off by heart; instead I look them up at the point of use.
--
--  If you haven't seen #isginfo statements before, they are just a simple way
--  of performing compile-time type checking. No code whatsoever is emitted,
--  and apart from compiling cleanly/reporting an error, they do not affect
--  compilation in any other way. TIP: any probs, just comment them out - some
--  optimisations might get thwarted, but they are probably insignificant.

--DEV can h5 go with localtype info??

procedure emitHex1(integer op1)
    -- emit a one byte opcode, or a one byte literal immediate
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 = append(x86,op1)
end procedure

procedure emitHex2(integer op1, integer op2)
    -- emit two literal bytes
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 = append(x86,op1)
    x86 = append(x86,op2)
end procedure

--DEV temp replace of above... (do the same for emitHex2s)
procedure emitHexx2(integer op1, integer op2)
    -- emit two literal bytes
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    if X64 
    and op1!=push_imm8
    and op1!=mov_al_imm8 then
        emitHex1(#48)
    end if
    x86 = append(x86,op1)
    x86 = append(x86,op2)
end procedure

procedure emitHex2s(sequence op2)
-- emit a two byte opcode, passed as a sequence
--/**/  #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
    if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 &= op2
end procedure

procedure emitHex3(sequence op2, integer i8)
-- emit a two byte opcode and a one byte immediate
--/**/  #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
    if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 &= op2
    x86 = append(x86,i8)
end procedure

procedure emitHex3l(integer op1, integer op2, integer op3)
    -- emit three literal bytes
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 = append(x86,op1)
    x86 = append(x86,op2)
    x86 = append(x86,op3)
end procedure

--procedure emitHex3s(sequence op3)
---- emit a three byte operation
----/**/    #isginfo{op3,0b0100,MIN,MAX,integer,3}  -- sequence of integer length 3
--  if length(op3)!=3 then ?9/0 end if  -- compiler should optimise this away!
----if not sched then
--  if lastline!=emitline then lineinfo() end if
----end if
--  x86 &= op3
--end procedure

procedure emitHex4l(integer op1, integer op2, integer op3, integer op4)
    -- emit four literal bytes
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 &= {op1,op2,op3,op4}
end procedure

procedure emitHex4sib(sequence op2, integer op3, integer op4)
-- emit an {opcode,xrm},sib,disp8 instruction, eg dec dword[ebx+edx*4-8]
--/**/  #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
    if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 &= op2
    x86 = append(x86,op3)
    x86 = append(x86,op4)
end procedure

--q86 checked
sequence v4
         v4 = {isVar,0,0,0}
procedure emitHex4v(integer N)
    if q86>1 then ?9/0 end if
    if symtab[N][S_NTyp]=S_TVar then ?9/0 end if
    if and_bits(symtab[N][S_State],K_Fres) then ?9/0 end if
    v4[4] = N
    x86 &= v4   -- {isVar,0,0,N}
end procedure

--DEV migrate this to main loop, call opStat at loop end if we emitted an isOpCode
--procedure emitHex5(integer op1, integer flags, integer v)
---- emit a one byte opcode and a dword operand, ie (opcode,flags,0,0,value)
----    if emitON then
----        if not sched then
--      if lastline!=emitline then lineinfo() end if
----        end if
--      if collectstats then
--          if not bind and flags=isOpCode then
--              -- Example use of opStat.
--              -- Count times each opcode is actually executed.
--              -- NB does not count "inlined" opcodes, in fact must avoid any partially inlined,
--              --  ie with a jcc @f around it, where the offset is fixed, or anything, eg opMkSq,
--              --  implemented using a push <return addr>. Also note that anything like
--                  -- NB: p2/3 obtained from [esp]-9/-14 on error
--              --  will be completely spannered by this intrusion, not that it matters much as
--              --  long as your test program does not actually crash.
--              if not find(v,{opTcFail,opMovsi,opMovti,
--                              opSubse,opConcatN,opSubss,opSubse1,opAddiii,
--                              opDivi2,opDiviii,opMuliii,opMovbi,opMkSq,opRepe1,
--                              opReps,opRepe,opFrame}) then
--                  x86 = append(x86,#60)       -- pushad = o(1,4,0)        -- 60       -- pushad
--                  x86 = append(x86,#B8)       -- mov_eax_imm32 = o(2,7,0) -- B8 imm32  -- mov eax,opcode
--                  emitHexDword(v)
----                    emitHex5(call_rel32,isOpCode,opStat)
--                  x86 = append(x86,#E8)       -- call_rel32    = o(3,5,0) -- E8 imm32 -- call rel32
--                  x86 = append(x86,isOpCode)
--                  x86 = append(x86,0)
--                  x86 = append(x86,0)
--                  x86 = append(x86,opStat)
--                  x86 = append(x86,#61)       -- popad = o(1,4,1)         -- 61       -- popad
--              end if
--          end if
--      end if
--      x86 = append(x86,op1)
--      x86 = append(x86,flags)
--      x86 = append(x86,0)
--      x86 = append(x86,0)
--      x86 = append(x86,v)
--      if flags=isOpCode then
--          reginfo = 0
--      end if
----    end if
--end procedure

--DEV newEBP this should (probably) go...
--(q86 checked)
--sequence p5addr
--       p5addr = {push_imm32,isAddr,0,0,0}     -- push <return addr>
function emitHex5addr()
-- push a return address for opTchk/MkSq/Subss/Subse/ConcatN/Repe/Reps
-- plants a zero offset and returns the location to backpatch later.
--if not sched then
    if lastline!=emitline then lineinfo() end if    -- (not currently needed)
--end if
    if q86 then
        x86 &= push_imm32
        quad(isAddr,0)
    else
        x86 &= {push_imm32,isAddr,0,0,0}
    end if
    return length(x86)
end function

--DEV so why don't I just add aliases to pops.e?! (opNames problem to be solved
-- These constants are here mainly because I don't want to keep explaining that
--  eg opMovsi is always inlined and VMep[opMovsi] is in fact deallocX, etc ;-)

--DEV kill all these for newEmit:
--constant c_dealloc        = opMovsi,  -- (yes, VMep[opMovsi] is deallocX)
--       c_e01tcfediDiv = opDiviii, -- (yes, VMep[opDiviii] is e01tcfediDiv)
--       c_e01tcfDivi2  = opDivi2,  -- (yes, VMep[opDivi2] is e01tcfDivi2)
--       c_e01tcfediMul = opMuliii, -- (yes, VMep[opMuliii] is e01tcfediMul)
--       c_e01tcfAddiii = opAddiii, -- (yes, VMep[opAddiii] is e01tcfAddiii)
--       c_e01tcfIncDec = opInc,    -- (yes, VMep[opInc] is e01tcfIncDec)
--       c_e02atdb0     = opDivf2   -- (yes, VMep[opDivf2] is e02atdb0)         --(kill for newEmit)
--       c_e92movti     = opMovti   -- (yes, VMep[opMovti] is e92movti)

--procedure emitHex5call(integer opcode)
--  -- Call an opcode. Btw, there is no real difference between calling this
--  --  and emitHex5s with one of the constants delared below, except for
--  --  the latter being easier to read in some cases, and those that need a
--  --  jump rather than a call obviously won't work here (see also the list
--  --  of opcodes given in the comments of emitHex5addr).
----    if not sched then
--  if lastline!=emitline then lineinfo() end if
----    end if
----if newEmit then
--  printf(1,"warning: emitHex5call(%d=%s) skipped for newEmit (pilx86.e line 984, emitline=%d)\n",{opcode,opNames[opcode],emitline})
--?9/0
--  norun = 1
----else
----    if q86>1 then
----        x86 &= call_rel32
----        quad2(isOpCode,opcode)
----    else
----        call_op[5] = opcode
----        x86 &= call_op
----    end if
----end if
--end procedure

procedure emitHex5cr(integer opcode, integer N)
-- emit a 5 byte mov reg,constref instruction
if newEmit then ?9/0 end if
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 &= opcode
    if q86>1 then
        quad2(isConstRef,N)
    else
        x86 &= {isConstRef,0,0}
        x86 &= N
    end if
end procedure

--DEV newEBP this should (probably) go....
--if q86 then ?9/0 end if   -- search for j5
--sequence j5
--       j5 = {jump_rel32,isJmp,0,0,0}
procedure emitHex5j(integer offset)
-- emit a 5 byte jump (auto-packed to 2 bytes when possible, see pemit.e/scanforShortJmp)
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    if q86 then
        x86 &= jump_rel32
        quad(isJmp,offset)
    else
--      j5[5] = offset
--      x86 &= j5   -- {jump_rel32,isJmp,0,0,offset}
        x86 &= {jump_rel32,isJmp,0,0}
        x86 &= offset
    end if
end procedure

--q86 checked
sequence jump_G
         jump_G = {jump_rel32,isJmpG,0,0,0}
--      x86 &= {call_rel32,isJmpG,0,0,lblidx}
--              x86 &= {call_rel32,isJmpG,0,0,lblidx}
--procedure emitHex5jmpG(integer lblidx)
procedure emitHex5jmpG(integer opcode)
--integer lblidx = tt[aatidx[opcode]+EQ]
    integer ato = aatidx[opcode],
            lblidx = tt[ato+EQ]
    if lblidx=0 then ?9/0 end if    -- (means that no #ilASM{} actually defined that label...
                                    --  check for things commented out, in one case there was
                                    --  a missing [], so it would only work on 64-bit...)
if not newEmit then ?9/0 end if
    -- Jump to an opcode(global label). ?Used for opRetf and in tandem with emitHex5addr.
--  if not sched then
        if lastline!=emitline then
--dev?
            if opcode!=opRetf then ?9/0 end if
            lineinfo()
        end if
--  end if
if suppressopRetf then
    if opcode=opRetf then
        puts(1,"pilx86.e line 1054 (opRetf)\n")
--?9/0
    end if
end if
    if q86>1 then
        x86 &= jump_rel32
        quad2(isJmpG,lblidx)
    else
        jump_G[5] = lblidx
        x86 &= jump_G
    end if
end procedure

sequence call_G
         call_G = {call_rel32,isJmpG,0,0,0}
--         x86 &= {call_rel32,isJmpG,0,0,lblidx}
--procedure emitHex5callG(integer lblidx)
procedure emitHex5callG(integer opcode, integer lblidx=0)
    if lblidx=0 then
        lblidx = tt[aatidx[opcode]+EQ]
        if lblidx=0 then ?9/0 end if
--DEV (temp)
--      if lblidx=0 then
--if opcode=opAtom0 then
--  ?tt[aatidx[opInt0]+EQ]
--end if
--          ?9/0
--      end if
    end if
    -- call an opcode(global label).
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    if q86>1 then
        x86 &= call_rel32
        quad2(isJmpG,lblidx)
    else
        call_G[5] = lblidx
        x86 &= call_G
    end if
end procedure


-- These constants are here mainly because I don't want to keep explaining that
--  eg opMovsi is always inlined and VMep[opMovsi] is in fact deallocX, etc ;-)

--q86 checked
--constant call_dealloc     = {call_rel32,isOpCode,0,0,opMovsi},    -- (yes, VMep[opMovsi] is deallocX)
--       call_e01tcfediDiv  = {call_rel32,isOpCode,0,0,opDiviii},   -- (yes, VMep[opDiviii] is e01tcfediDiv)
--       call_e01tcfDivi2   = {call_rel32,isOpCode,0,0,opDivi2},    -- (yes, VMep[opDivi2] is e01tcfDivi2)
--       call_e01tcfediMul  = {call_rel32,isOpCode,0,0,opMuliii},   -- (yes, VMep[opMuliii] is e01tcfediMul)
--       call_e01tcfAddiii  = {call_rel32,isOpCode,0,0,opAddiii},   -- (yes, VMep[opAddiii] is e01tcfAddiii)
--       call_e01tcfIncDec  = {call_rel32,isOpCode,0,0,opInc},      -- (yes, VMep[opInc] is e01tcfIncDec)
--       call_e02atdb0      = {call_rel32,isOpCode,0,0,opDivf2}     -- (yes, VMep[opDivf2] is e02atdb0)
--       call_e92movti      = {call_rel32,isOpCode,0,0,opMovti}     -- (yes, VMep[opMovti] is e92movti)

procedure emitHex5s(sequence op5)
-- emit a five byte opcode, eg call an opcode.
--/**/  #isginfo{op5,0b0100,MIN,MAX,integer,5}  -- sequence of integer length 5
    if length(op5)!=5 then ?9/0 end if  -- compiler should optimise this away!
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
if newEmit and op5[2]=isOpCode then ?9/0 end if
    x86 &= op5
--  reginfo = 0     --NB callee's responsibility. last re-checked 6/3/09.
end procedure

procedure emitHex5sib(sequence op2, integer o3, integer o4, integer o5)
-- emit an {opcode,xrm},sib,disp8,imm8 instruction, eg cmp byte[ecx+edx*4-1],0x12
--/**/  #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
    if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 &= op2
    x86 = append(x86,o3)
    x86 = append(x86,o4)
    x86 = append(x86,o5)
end procedure

procedure emitHex5v(integer op1, integer N)
-- emit a 1 byte opcode and a 4 byte var addr
--!/**/ #isginfo{op1,integer,104,191,object,-1} --dev allow "ANY"?
--!/**/ #isginfo{op1,integer,61,191,object,-1} --dev allow "ANY"?
--DEV broken on 64bit:
--!/**/ #isginfo{op1,integer,161,191,object,-1} --dev allow "ANY"?
--!/**/ #isginfo{op1,atom,MIN,MAX,object,-1}
--DEV::
--!/**/ #isginfo{N,integer,MIN,MAX,object,-2}
--!/**/ #isginfo{N,object,MIN,MAX,object,-2}
--!/**/ #isginfo{N,atom,MIN,MAX,object,-1}
--DEV broken on 64bit:
--!/**/ #isginfo{N,integer,MIN,MAX,object,-1}
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    if symtab[N][S_NTyp]=S_TVar then ?9/0 end if
    if and_bits(symtab[N][S_State],K_Fres) then ?9/0 end if
    x86 &= op1
    if q86>1 then
        quad2(isVar,N)
    else
        emitHex4v(N)    -- {isVar,0,0,N}
--      v4[4] = N
--      x86 &= v4   -- {isVar,0,0,N}
    end if
end procedure

procedure emitHex5vno(integer op1, integer N)
-- emit a 1 byte opcode and a 4 byte literal symtab index (which needs mapping)
-- see also cmp_eax_srcid()
--!/**/ #isginfo{op1,integer,104,191,object,-1} --dev allow "ANY"?
--!/**/ #isginfo{op1,integer,61,191,object,-1} --dev allow "ANY"?
--!/**/ #isginfo{op1,integer,161,191,object,-1} --dev allow "ANY"?
--!/**/ #isginfo{op1,atom,MIN,MAX,object,-1}
--DEV broken/atom on 64bit
--!/**/ #isginfo{N,integer,MIN,MAX,object,-1}
if not newEmit then ?9/0 end if
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
--  if X64 then ?9/0 end if     -- use emit movRegVno or emitHex62vno
    if X64 then
--printf(1,"warning: emitHex5vno for newEmit (pilx86.e line 1174, emitline=%d)\n",{emitline})
        emitHex1(#48)
    end if
    x86 &= op1
    if q86>1 then
        quad2(isVno,N)
    else
--      emitHex4v(N)    -- {isVar,0,0,N}
--      v4[4] = N
        x86 &= {isVno,0,0,N}
    end if
end procedure

procedure emitHex5w(integer op1, atom v)
-- emit a one byte opcode and a literal dword
-- NB: use emitHex5vno() if v is a symtab index
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 = append(x86,op1)
    emitHexDword(v)
end procedure

--q86 checked
sequence cr4
         cr4 = {isConstRef,0,0,-1}
--dev cf emitHex5cr...
procedure emitHex5constref(integer op1, integer N)
-- emit an opcode,isConstRef,0,0,N instruction
if newEmit then ?9/0 end if
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 &= op1
    if q86>1 then
        quad2(isConstRef,N)
    else
        cr4[4] = N
        x86 &= cr4
    end if
end procedure

procedure cmp_h4(integer reg)
-- reg is 0..7 for eax..edi (as returned by loadReg etc)
-- reg is 0..15 for rax..r15 on 64-bit
-- nb any scheduling to be done by callee
    if X64 then
        if reg=15 then ?9/0 end if  -- (got a better idea?) [push( imm)/(pop|add rsp 8) might be an option, or make r15(?) permanently h4?]
--      yea, r15 is now permanently h4 (oops/DEV, need to do a search for r15, it get clobbered a few times...)
--      (would also need to save/set/restore r15 in cbhandler)
        x86 &= {#49,#BF,0,0,0,0,0,0,0,#40}  -- mov r15,h4   (10 byte ins)
        emitHex1(#4C)
        if reg>7 then ?9/0 end if   -- (placeholder for more code)
        reg = 0o370+reg     
        x86 &= {#39,reg}                        -- cmp reg,r15
    else
        if reg=eax then
            x86 &= {cmp_eax_imm32,0,0,0,#40}    -- cmp eax,h4   (5 byte ins)
        else
            reg = #F8+reg   -- 0o37r, cmp/reg
            x86 &= {#81,reg,0,0,0,#40}          -- cmp reg,h4   (6 byte ins)
        end if
    end if
end procedure

procedure cmp_eax_srcid(integer vno)
--puts(1,"warning: cmp eax,var_id handling incomplete (pilx86.e line 1199)\n") (seems alright to me!)
----    if not sched then
--      if lastline!=emitline then lineinfo() end if
----    end if
if not newEmit then ?9/0 end if --(isVno handling is only in pemit2.e)
if lastline!=emitline then ?9/0 end if --(counts as suspicious use)
--if X64 then ?9/0 end if -- erm? (fine)
    x86 &= {cmp_eax_imm32,isVno,0,0,vno}
end procedure

procedure cmp_imm32(integer reg, integer v)
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    if X64 then
        emitHex1(#48)
    end if
    if reg=eax then
        x86 &= cmp_eax_imm32
    else
        x86 &= cmp_reg_imm32
        reg = 0o370+reg
        x86 &= reg
    end if
    emitHexDword(v)
end procedure


--if q86 then ?9/0 end if -- search for j4
--sequence j4
--       j4 = {isJmp,0,0,0}
procedure emitHex6j(sequence op2, integer offset)
-- emit a 6 byte jump (auto-packed to 2 bytes when possible, see pemit.e/scanforShortJmp)
--broken 24/4/21 (p2js)
--!/**/ #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
    if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
    --we could, if we wanted, further test op2(see jo..jg_rel32):
    --  if op2[1]!=#0F then ?9/0 end if
    --  if or_bits(op2[2],#0F)!=#8F then ?9/0 end if    -- (ie in range #80..#8F) \ only one of these
    --  if and_bits(op2[2],#F0)!=#80 then ?9/0 end if   -- (ie in range #80..#8F) /  would be needed
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 &= op2
    if q86 then
        quad(isJmp,offset)
    else
--      j4[4] = offset
--      x86 &= j4   -- {isJmp,0,0,offset}
        x86 &= {isJmp,0,0}
        x86 &= offset
    end if
end procedure

--procedure emitHex6ret(sequence op2)
---- emit a conditional return
----/**/    #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
--  if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
--  --(ditto as per emitHex6j)
----    if sched then
----        schend()
----    else
--      if lastline!=emitline then lineinfo() end if
----    end if
--  x86 &= op2
--if newEmit then ?9/0 end if -- see eg emitHex5jmpG
--if suppressopRetf then
--  puts(1,"emitHex6ret\n")
--end if
--  if q86>1 then
--      quad2(isOpCode,opRetf)
--  else
--      x86 &= {isOpCode,0,0,opRetf}
--  end if
--end procedure

procedure emitHex6retg(sequence op2)
-- emit a conditional return
integer lblidx = tt[aatidx[opRetf]+EQ]
--/**/  #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
    if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
    --(ditto as per emitHex6j)
--  if sched then
--      schend()
--  else
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 &= op2
if not newEmit then ?9/0 end if
--printf(1,"pilx86.e/emitHex6retg line 1335, emitline=%d\n",{emitline}) -- OK!
--printf(1,"checkme: line 7714 pilx86.e, (emitline=%d, %s)\n",{emitline,filenames[symtab[vi][S_FPno]][2]})
if suppressopRetf then
    puts(1,"emitHex6ret\n")
end if
    if q86>1 then
--!     quad2(isOpCode,opRetf)
        quad2(isJmpG,lblidx)
    else
--!     x86 &= {isOpCode,0,0,opRetf}
        x86 &= {isJmpG,0,0,lblidx}
    end if
end procedure

--constant cmp_edx_h4 = {cmp_edx_imm32,0,0,0,#40}

procedure emitHex6v(sequence op2, integer N)
-- emit a 2 byte opcode and a 4 byte var addr
--/**/  #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
    if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    if symtab[N][S_NTyp]=S_TVar then ?9/0 end if
    if and_bits(symtab[N][S_State],K_Fres) then ?9/0 end if
    x86 &= op2
    if q86>1 then
        quad2(isVar,N)
    else
        emitHex4v(N)    -- {isVar,0,0,N}
    end if
end procedure

procedure emitHex62v(integer op1, integer op2, integer N)
-- emit 2 opcode bytes and a 4 byte var addr
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    if symtab[N][S_NTyp]=S_TVar then ?9/0 end if
    if and_bits(symtab[N][S_State],K_Fres) then ?9/0 end if
    x86 &= op1
    x86 &= op2
    if q86>1 then
        quad2(isVar,N)
    else
        emitHex4v(N)    -- {isVar,0,0,N}
    end if
end procedure

procedure emitHex62vno(integer op1, integer op2, integer N)
-- emit 2 opcode bytes and a 4 byte symtab index
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
if not newEmit then ?9/0 end if
    x86 &= op1
    x86 &= op2
    if q86>1 then
        quad2(isVno,N)
    else
        x86 &= {isVno,0,0,N}
    end if
end procedure

procedure movRegVno(integer reg, integer N)
-- set the specified register to a 4 byte literal symtab index (which needs mapping)
-- see also cmp_eax_srcid()
--!/**/ #isginfo{reg,integer,0,15,object,-1} --dev allow "ANY"?
--DEV broken on 64bit:
--!/**/ #isginfo{N,integer,MIN,MAX,object,-1}
integer op1, xrm
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
if not newEmit then ?9/0 end if
    if X64 then
        emitHex1(#48)
        xrm = 0o300+reg
--DEV only place this is used...
        emitHex62vno(mov_regimm32,xrm,N)            -- mov reg,symtab idx
    else
        op1 = mov_eax_imm32+reg -- 0o27r
        emitHex5vno(op1,N)                          -- mov reg,symtab idx
    end if
end procedure

procedure emitHex6w(sequence op2, atom v)
-- emit a 2 byte opcode and a literal dword
--/**/  #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
    if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 &= op2
    emitHexDword(v)
end procedure

--procedure emitHex6w2(sequence op2, atom v)
---- emit a 2 byte opcode and a literal word
----/**/    #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
--  if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
----    if not sched then
--  if lastline!=emitline then lineinfo() end if
----    end if
--  x86 &= op2
--  emitHexWord(v)
--end procedure

procedure emitHex62w(integer op1, integer op2, atom v)
-- emit 2 opcode bytes and a literal dword
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 &= op1
    x86 &= op2
    emitHexDword(v)
end procedure

--q86 checked
sequence crc4
         crc4 = {isConstRefCount,0,0,-1}
procedure emitHex6constrefcount(sequence op2, integer N)
-- emit an {opcode,xrm},isConstRefCount,0,0,N instruction
--/**/  #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
    if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 &= op2
    if q86>1 then
        quad2(isConstRefCount,N)
    else
        crc4[4] = N
        x86 &= crc4
    end if
end procedure

--DEV newEBP this should (probably) go...
--q86 checked
sequence a4
         a4 = {isAddr,0,0,0}
procedure emitHex7a(sequence op2, integer op3, integer offset)
-- emit an opcode,xrm,disp8,{isAddr,0,0,offset} instruction,
--EXCEPT
--X ie/eg mov [ebp+16],<return addr>
--  ie/eg mov [ebp+28],<return addr>
--  if lastline!=emitline then lineinfo() end if    -- should never be needed
    if lastline!=emitline then ?9/0 end if  --  (currently only used by opCall) [DEV?]
    x86 &= op2
    x86 = append(x86,op3)
    if q86 then
        quad(isAddr,offset)
    else
        a4[4] = offset
        x86 &= a4
    end if
end procedure

procedure emitHex7d8constref(sequence op2, integer offset, integer N)
-- emit an {opcode,xrm},d8,imm32 instruction, eg mov [ebp-4],0x12345678
--/**/  #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
if newEmit then ?9/0 end if
    if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    if symtab[N][S_NTyp]=S_TVar then ?9/0 end if
    if and_bits(symtab[N][S_State],K_Fres) then ?9/0 end if
    x86 &= op2
    x86 = append(x86,offset)
    if q86>1 then
        quad2(isConstRef,N)
    else
        cr4[4] = N
        x86 &= cr4
    end if
end procedure

procedure emitHex7d8v(sequence op2, integer offset, atom v)
-- emit an {opcode,xrm},d8,imm32 instruction, eg mov [ebp-4],0x12345678
--/**/  #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
    if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 &= op2
    x86 = append(x86,offset)
    emitHexDword(v)
end procedure

procedure emitHex7i(integer op1, integer op2, integer op3, atom d32)
-- emit 3 opcode bytes and an imm32, eg mov idx,[ebx+reg*4+d32]
--  if not sched then
    if lastline!=emitline then lineinfo() end if    -- (not actually necessary)
--  end if
    x86 = append(x86,op1)
    x86 = append(x86,op2)
    x86 = append(x86,op3)
    emitHexDword(d32)
end procedure

--DEV wrong! it's a length 5!
--procedure emitHex7sib(sequence op2, integer o3, integer o4, integer o5)
---- emit an {opcode,xrm},sib,imm32 instruction, eg jmp [ecx*4+edx*4-1],0x12
----/**/    #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
--  if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
----    if not sched then
--  if lastline!=emitline then lineinfo() end if
----    end if
--  x86 &= op2
--  x86 = append(x86,o3)
--  x86 = append(x86,o4)
--  x86 = append(x86,0)
--  x86 = append(x86,0)
--  x86 = append(x86,o5)
--end procedure

procedure emitHex7base(sequence op2, integer o3, integer o5)
-- emit an {opcode,xrm},sib,<isBase,0,0,offset> instruction, eg jmp dword[i*s+imm32],
--  where the imm32 contains the base adjustment for a switch (see lone use below)
--/**/  #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
    if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 &= op2
    x86 = append(x86,o3)
    if q86 then
        quad(isBase,o5)
    else
        x86 &= {isBase,0,0,o5}
    end if
end procedure

-- no longer in use...
--procedure emitHex8a(sequence op2, integer op3, integer op4, integer offset)
---- emit an {opcode,xrm},sib,disp8,{isAddr,0,0,offset} instruction,
----    ie/eg mov [ebp+edx*4+20],<return addr>
----/**/    #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
--  if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
----    if lastline!=emitline then lineinfo() end if    -- should never be needed
--  if lastline!=emitline then ?9/0 end if  --  (currently only used by opCall)
--  x86 &= op2
--  x86 = append(x86,op3)
--  x86 = append(x86,op4)
--  if q86 then
--      quad(isAddr,offset)
--  else
--      a4[4] = offset
--      x86 &= a4
--  end if
--end procedure

--q86 checked
sequence cr8
    cr8 = {isVar,0,0,-1,isConstRef,0,0,-1}

procedure emitHex10constref(sequence op2, integer dest, integer src)
-- emit an {opcode,xrm},isVar,0,0,dest,isConstRef,0,0,src instruction,
--  ie/eg mov [dest],<dword literal (greater than h4)>
--/**/  #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
if newEmit then ?9/0 end if
    if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
-- should not be needed as always follows an emitHex6constrefcount() call...
----    if not sched then
--  if lastline!=emitline then lineinfo() end if
----    end if
    if symtab[dest][S_NTyp]=S_TVar then ?9/0 end if
    if and_bits(symtab[dest][S_State],K_Fres) then ?9/0 end if
    x86 &= op2
    if q86>1 then
        quad2(isVar,dest)
        quad2(isConstRef,src)
    else
        cr8[4] = dest
        cr8[8] = src
        x86 &= cr8
    end if
end procedure

procedure emitHex10wconstref(sequence op2, integer offset, integer src)
-- emit an {opcode,xrm},<dword offset>,isConstRef,0,0,src instruction,
--  ie/eg mov [ebp-256],<dword literal ref(greater than h4)>
--/**/  #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
if newEmit then ?9/0 end if
    if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
--DEV re-check:
-- should not be needed as always follows an emitHex6constrefcount() call...
----    if not sched then
--  if lastline!=emitline then lineinfo() end if
----    end if
    x86 &= op2
    emitHexDword(offset)
    if q86>1 then
        quad2(isConstRef,src)
    else
        cr4[4] = src
        x86 &= cr4
    end if
end procedure

procedure emitHex10ww(sequence op2, integer offset, atom v)
-- emit 2 opcode bytes, a 32-bit offset and a literal dword
--  ie/eg mov [ebp-256],#01020304
--/**/  #isginfo{op2,0b0100,MIN,MAX,integer,2}  -- sequence of integer length 2
    if length(op2)!=2 then ?9/0 end if  -- compiler should optimise this away!
--  if not sched then
    if lastline!=emitline then lineinfo() end if
--  end if
    x86 &= op2
    emitHexDword(offset)
    emitHexDword(v)
end procedure


procedure markConstUsed(integer n)
-- ensures that constants (esp unnamed ones) appear properly in list.asm
--  (btw I already tried disabling/removing this and saw no gain)
sequence symn
integer snNTyp, state
    if n<=0 then ?9/0 end if    -- sanity check
    symn = symtab[n]
    snNTyp = symn[S_NTyp]
    if snNTyp=S_Const then
        state = symn[S_State]
        if not and_bits(state,S_used) then
            state += S_used
            symtab[n] = 0   -- kill refcount
            symn[S_State] = state
            symtab[n] = symn
        end if
    end if
end procedure

procedure markConstUseds(sequence s)
--DEV spannered: (this should start working again when localtypes are in)
----/**/    #isginfo{s,0b0100,MIN,MAX,integer,-1}   -- sequence of integer
    for i=1 to length(s) do
        markConstUsed(s[i])
    end for
end procedure

--procedure cleanup(integer firstvar, integer lastvar, integer firstTidx, integer lastTidx)
---- common code for unused_cleanup:
--  if firstvar then
--      if q86>1 then ?9/0 end if   -- need to do something better here then...
--      s5 &= mov_esi_imm32
--      s5 &= {isVar,0,0}
--      s5 &= firstTidx
--      if firstvar=lastvar then
--          s5 &= {call_rel32,isOpCode,0,0}
--          s5 &= opCleanUp1
--      else
--          s5 &= mov_edi_imm32
--          s5 &= {isVar,0,0}
--          s5 &= lastTidx
--          s5 &= {call_rel32,isOpCode,0,0}
--          s5 &= opCleanUp
--      end if
--  end if
--end procedure

procedure Aborc(sequence msg)
--DEV make this a subroutine in pmsg.e?
    if equal(expandedYet[fileno],0) then
        text = allfiles[fileno]
        exptext[fileno] = expandIntoLines()
        expandedYet[fileno] = linestarts
        text = allpfiles[fileno]
    else
        linestarts = expandedYet[fileno]
    end if
    convertToLineColumn(tokcol)
    tokline = eLine
    Abort(msg)
end procedure

global procedure BackPatchForwardCalls(integer N, integer nParams, sequence paramCols, sequence paramNames)
--
-- called from pmain.e/DoRoutineDef() when we find a routine declaration which has some forward calls (and
--                                      we must do this immediately as we are about to replaced [S_il]),
--         and pilx86.e/unused_cleanup() (hopefully, DEV) In this case we may [DEV] need to create a
--                                      "forwarding" table to replace N with i (see unused_cleanup).
--
-- symtab[N][S_il] contains the backpatch list, if any
-- nParams is the number of formal parameters on the routine definition
-- paramCols (badly named) is the symtab idx of the formal parameters
-- paramNames is the ttidx of the formal parameters (for named parameter handling)
--
object bcptr
integer pN, rtn, cidx
integer pidx
sequence bj, bj1, bjz, pdone

    bcptr = symtab[N][S_il]
    if sequence(bcptr) then
        if length(bcptr)=0 then
            if nParams!=0 then
                pN = paramCols[1]
                if not and_bits(symtab[pN][S_State],K_dlft) then
--                  tokcol = rtntokcol
                    tokcol = symtab[N][S_ErrR]
--                  tokline = rtntokline
                    fileno = symtab[N][S_FPno]
--                  Aborc("routine previously called with no parameters")
                    Aborc("missing parameters")
                end if
            end if
        else
            for j=1 to length(bcptr) do
                bj = bcptr[j]   -- (all params for one forward call)
                bj1 = bj[1]     -- {tokcol, fileno, routineNo} of the call statement
                rtn = bj1[3]
                pdone = {}
                for z=2 to length(bj) do
                    bjz = bj[z] -- {tokcol, offset[, ttidx]} for each parameter
                    if length(bjz)=2 then   -- normal/numbered parameter
                        pidx = z-1
                    else                    -- named parameters
                        if length(pdone)=0 then
                            pdone = repeat(1,z-2)
                        end if
                        pidx = find(bjz[3],paramNames)
                        for addslots=length(pdone)+1 to pidx do
                            pdone &= 0
                        end for
                        if pidx=0
                        or pdone[pidx] then
                            fileno = bj1[2]
--                              tokcol = bjz[1]     -- \ see notes
                            tokcol = bjz[4]     -- / (top of this file)
                            no_oops = 1
                            if pidx=0 then
--                              Aborp("no such parameter")
                                Aborc("no such parameter")
                            end if
--                          Aborp("duplicate parameter")
                            Aborc("duplicate parameter")
                        end if
                        pdone[pidx] = 1
                    end if
                    cidx = bjz[2]
                    if symtab[rtn][S_il][cidx]!=-9 then ?9/0 end if
--object dbg = symtab[rtn]  --{-1,8,1,2304,0,0,"P",0,0,0,0,0,1,0}
--                  if symtab[rtn][S_il][cidx]!=-9 then Aborc("uh?") end if
                    if pidx>nParams then
                        fileno = bj1[2]
                        tokcol = bjz[1]
                        no_oops = 1
--                      Aborp("too many parameters")
                        Aborc("too many parameters")
                    end if
                    pN = paramCols[pidx]
                    symtab[rtn][S_il][cidx] = pN
                end for -- z=2 to length(bj)
                if length(pdone)=0 then
                    if pidx<nParams then
                        pidx += 1
                    else
                        pidx = 0
                    end if
                else
                    pidx = find(0,pdone)
                end if
                if pidx then
                    pN = paramCols[pidx]
                    if not and_bits(symtab[pN][S_State],K_dlft) then
                        fileno = bj1[2]
                        tokcol = bj1[1]
                        no_oops = 1
--                      Aborp("missing non-defaulted parameter ["&getname(symtab[pN][S_Name],-2)&"]")
                        Aborc("missing non-defaulted parameter ["&getname(symtab[pN][S_Name],-2)&"]")
                    end if
                end if
            end for -- i=1 to length(bcptr)
        end if -- length(bcptr)=/!=0
    end if -- sequence(bcptr)
end procedure

sequence forwardingtable,
         forwardretarget

with trace
global procedure unused_cleanup(integer asmoptions)
--
-- check for undefined forward (implicit) references,
--           unused and unassigned variables, and
--           if debugleak issue cleanup code (reset all
--           vars to 0 for memory leak checks).
--
integer dlAsm
--, k
integer firstGvar, lastGvar
object si
integer siNTyp, siState
--, vtype
integer N, nParams, p
sequence paramCols, paramNames
    --
    -- First ensure front-end settings match the back-end.
    -- (all official releases should have debugleak set to 0, btw)
    --
if not newEmit and repl=0 then
    if debugleak!=and_bits(asmoptions,#01) then ?9/0 end if
    -- (newBase is temporary, unless complaints flood in about the
    --  relatively poor performance of StringA=prepend(StringA,char),
    --  demanding it be fixed at ~10% overall loss elsewhere.)
    if newBase!=and_bits(asmoptions,#02) then ?9/0 end if
--DEV:
end if
--  if newEBP!=and_bits(asmoptions,#04) then
--      puts(1,"newEBP error\n") --DEV no check on the one in pdiag.e...
--      ?9/0
--  end if

    lastline = emitline     -- suppress lineinfo() etc
    some_unresolved_rtnids = find(1,unresolved_routine_ids)

if not newEmit then
    if debugleak and repl=0 then    -- (all official releases should have debugleak set to 0, btw)
        if not LastStatementWasAbort then
            if listing or dumpil or bind then
--DEV make this a subroutine in pmsg.e?
                if equal(expandedYet[1],0) then
                    text = allfiles[fileno]
                    exptext[1] = expandIntoLines()
                    expandedYet[1] = linestarts
                    text = allpfiles[fileno]
                end if
                line = length(exptext[1])
                if lastline<line then
                    s5 &= {opLn,line}
                end if
            end if
--DEV should be a single opCleanup, and ilxlate to scan (remaining)gvar(map)?
            s5 &= {opAsm,0,0,0}
            dlAsm = length(s5)
        end if
    end if
end if

    forwardingtable = {}
    forwardretarget = {}

    firstGvar = 0   lastGvar = -1
    for i=1 to symlimit do
--if i=385 then trace(1) end if
        si = symtab[i]
--?{i,si}
        if sequence(si) then
            siNTyp = si[S_NTyp]
            siState = si[S_State]
            if siNTyp>=S_Type then
--              if i>T_Bin and and_bits(siState,S_fwd) then
                if i>T_Asm and and_bits(siState,S_fwd) then
--                  if and_bits(siState,S_used) then
                    if and_bits(siState,S_used) 
                    and (not repl or integer(si[S_Name])) then
-- test 14/2/14:
ttidx = si[S_Name]
N = InTable(-InAny)
if N!=0  --(must be a global, btw)
and N!=i
--and 0
and symtab[N][S_NTyp]>=S_Type
and (symtab[N][S_NTyp]=S_Proc)=(siNTyp=S_Proc)
and and_bits(symtab[N][S_State],K_gbl) then
--      printf(1,"backpatching forward call (%d->%d)...\n",{i,N})
--?getname(ttidx,-2)

        -- Avoid these warnings by planting approriate "forward global routine Xxx()" statements.
        -- For more details see test\t59mri.exw. BTW (example in t59 of A2) for two or more calls
        -- you only get the one warning, on the first, as this backpatches both calls.
        WarnU(siNTyp,si,sprintf(" implicit (local) call resolved to global [%d].",N),i)
        nParams = length(symtab[N][S_sig])-1
        paramCols = repeat(0,nParams)
        paramNames = repeat(0,nParams)
        p = symtab[N][S_Parm1]
        for j=1 to nParams do
if p=0 then ?"oops, p=0 line 1938 pilx86.e" exit end if
            si = symtab[p]
            paramCols[j] = p
            paramNames[j] = si[S_Name]
            p = si[S_Slink]
        end for
        -- (call the code that was originally written inside DoRoutineDef():)
--      BackPatchForwardCalls(N,nParams,paramCols,paramNames)
        BackPatchForwardCalls(i,nParams,paramCols,paramNames)
--      symtab[i][S_il] = {}
--      symtab[i][S_il] = 0
        -- (as this occurs before the relink(), S_Nlink should be alright:)
        symtab[i] = 0
        if siNTyp!=S_Proc then
            symtab[i-1] = 0
        end if
        forwardingtable &= i
        forwardretarget &= N
-- need to rebuild nParams, paramCols (symtabidx of params), and paramNames (ttidx of same).
-- need some tests to get this started.
else
--?{i,si}
                        WarnU(siNTyp,si,-1,i) -- Undefined() [Aborts]
end if
                    elsif i>T_Ainc then
--if siNTyp=S_Type then
--                      WarnU(siNTyp,si,-1,i) -- Undefined() [Aborts]
--else
                        WarnU(siNTyp,si," is not actually defined (or used) anywhere.",i)
--end if
                    end if
                elsif i>T_Ainc
                  and not some_unresolved_rtnids
                  and not and_bits(siState,S_used) then         -- and this routine not (directly) used
--if siNTyp=S_Type then trace(1) end if
                    WarnU(siNTyp,si," is not used.",i)
                    if siNTyp=S_Type then
--?{"WarnU(",siNTyp,si," is not used.",i}
--DEV see what damage this does...
--oops2 in ReconstructIds(six)
--      ss = symtab[scopechain]
--      tnxt = ss[S_Nlink]
--      if tnxt!=-2 then
--          tidx = ss[S_Name]
--          symtab[scopechain] = 0      -- reduce refcount on ss
--          ss[S_Nlink] = -2
--          symtab[scopechain] = ss
--          tt[tidx+EQ] = tnxt
--      end if
                        integer tnxt = symtab[i][S_Nlink]
                        if tnxt!=-2 then
                            integer tidx = symtab[i][S_Name]
                            symtab[i][S_Nlink] = -2
                            tt[tidx+EQ] = tnxt
                        end if
                        symtab[i] = 0
                        symtab[i-1] = 0
                    end if
                end if
            else -- <S_TYpe
--29/1/10:
--              if i>T_Bin and siNTyp<=S_TVar
                if siNTyp<=S_TVar and si[S_FPno]!=0
                and not equal(si[S_Name],-1)
                and and_bits(siState,S_used_and_set)!=S_used_and_set then
                    if and_bits(siState,S_used) then    -- therefore not set
--                      if not and_bits(siState,K_noclr) then
                        if not and_bits(siState,K_noclr+K_aod) then
                            WarnU(siNTyp,si," is never assigned a value.",i)
                        end if
                    elsif not and_bits(siState,S_set)
                       or and_bits(siState,K_used)  -- added 8/1/09
                       or siNTyp=S_Const then
--?si
                        WarnU(siNTyp,si," is not used.",i)
                    -- set but not used is OK (eg void=f())
                    end if
                end if
--DEV 18/10/09: (new isConstRef/isConstRefCount handling)
if siNTyp=S_Const
and and_bits(siState,K_noclr) then
        symtab[i] = 0           -- kill refcount
        si[S_ConstChain] = 0    -- initialise fixup chain
        symtab[i] = si
end if
--if not newEmit then --DEV
--              if debugleak and repl=0 then    -- (all official releases should have debugleak set to 0, btw)
--                  if not LastStatementWasAbort then
--                      -- cleanup any non-integers which have actually been set
--                      if si[S_vtype]!=T_integer
----                    vtype = si[S_vtype]
----                    if length(si)>=S_gNew
----                    and sequence(si[S_gNew]) then
----                        vtype = si[S_gNew][gType]
----                    end if
----                    if vtype!=T_integer
----DEV 15/6:
----                        and and_bits(siState,S_set) then
--                      and and_bits(siState,S_set+K_aod) then
--                          if siNTyp=S_GVar2
--                          or (siNTyp=S_Const and and_bits(siState,K_noclr)=0) then
--                              if i!=lastGvar+1 then
--                                  cleanup(firstGvar,lastGvar,firstGvar,lastGvar)
--                                  firstGvar = i
--                              end if
--                              lastGvar = i
--                          end if
--                      end if -- S_set non-integer
--                  end if -- LastStatementWasAbort
--              end if  -- debugleak
--end if
            end if -- S_Type/S_Gvar
        end if -- sequence(si)
    end for
--if not newEmit then
--  if debugleak and repl=0 then    -- (all official releases should have debugleak set to 0, btw)
--      if not LastStatementWasAbort then
--          cleanup(firstGvar,lastGvar,firstGvar,lastGvar)
--          k = length(s5)-dlAsm
--          s5[dlAsm-2] = k
--      end if
--  end if
--end if
end procedure

sequence states,        -- The state table, eg if states[state] is {1,3,4,2}
                        -- then 1 (eax) is the most recently used, followed by
                        --      4 (edi), then 2 (ecx), lastly 3 (esi), or iow
                        --      s[eax]=1, s[edi]=2, s[ecx]=3, s[esi]=4 means
                        --      that eax is mru because s[eax] is 1, and esi
                        --      is lru because s[esi] is 4. (See below re
                        --      1/2/3/4 idx vs 0/1/6/7 reg codes). In practice
                        --      this (states) is not used beyond initialisation,
                        --      but getting rid of it would make regstate (below)
                        --      even more uber-cryptic...
                        -- Note that if mloc is empty then state is meaningless,
                        -- and if mloc only contains entries for ecx then states[state]
                        -- should/will be {x,1,y,z} where x,y,z are meaningless.
                        -- Hence the initial state(=1) matters not, so long as bumping ecx
                        -- transits to an {x,1,y,z} state, which it does, and then bumping
                        -- (say) eax yields a {1,2,-,-} state, and so on.
         state4,        -- state4[i] is quick lookup for which of states[i][1..4] is 4
         transitions,   -- the transitions table, ie state = transitions[state][picked]
                        -- makes states[state][picked]=1 and bumps other entries accordingly.
         snartitions    -- the inverse of transitions, makes states[state][picked]=4.

--without trace

integer regstate    -- idx to states

function perm(integer n, sequence set)
--
-- return the nth permute of the given set.
--  (only called from build_state_tables init loop [24 times]).
--
-- n should be an integer in the range 1 to factorial(length(set))
-- all valid permutes are eventually given (tested to length 8),
-- with no duplicates (as long as all emements of set are unique),
-- very fast, however (NB) they are in no specific order.
--
sequence res,rem
integer ls, w
    n -= 1
    ls = length(set)
    res = repeat(0,ls)
--p2js:
--  rem = set -- remaining set is initially full set
    rem = deep_copy(set) -- remaining set is initially full set
    for i=ls to 1 by -1 do
        w = remainder(n,i)+1
        res[i] = rem[w]
        rem[w] = rem[i]     -- eg {1,2,3,4} -> {1,4,3[,4]} after picking [a] 2
        n = floor(n/i)
    end for
    return res
end function

procedure build_state_tables()
--
-- create three things:
--  1. states, all permutations from {1,2,3,4} to {4,3,2,1}.
--              (in practice entries occur in no particular order)
--              Note that eax/ecx/esi/edi are 1/2/3/4 for internal
--              table indexing, whereas 0/1/6/7 are seen externally.
--              There are 24 (factorial(4)) such entries.
--              Indicates the most recently used order, eg after
--              use(edi), use(esi), use(ecx), use(eax) then
--              states[regstate] will be {1,2,3,4} which means that
--              eax is the most recently used, and both ecx & esi
--              have been used more recently than edi.
--  2. state4, a quick lookup to find which of states[regstate] is 4,
--              ie which reg is last in most recently used order.
--              Generally, to follow on from the above example, we
--              are only interested in edi being last/the one to
--              be re-used next. Admittedly this may be a naive
--              strategy since edi may in fact be needed but none
--              of eax/ecx/esi. (improvements welcome)
--  3. transitions, which for each entry in states is a 4-way
--                  decision table indicating the entry in states
--                  which we should shift to. A 96-way switch.
--                  Maintains the most recently used order.
--                  Eg if states[regstate] is {1,4,3,2} then using edi
--                  should regstate = transitions[regstate][edi] such
--                  that states[regstate] is now {2,4,3,1}, ie edi
--                  becomes top dog, eax drops a slot, ecx/esi asis.
--                  It is of course hundreds of times faster to
--                  calculate and index all these (96) permutations
--                  once at the start rather than do (part of) this
--                  each and every single time we use a register.
--  4. snartitions, which is the inverse of transitions, for use in
--                  demoteReg(), ie to force eax to be next-in-line,
--                  we demoteReg(eax). Eg if states[regstate] is
--                  {2,4,3,1} then regstate = snartitions[regstate][eax]
--                  should set regstate such that states[regstate] is
--                  {4,3,2,1}, ie eax becomes the runt of the litter
--                  and other regs promoted as required.
--
sequence transit,       -- one set of transitions
         snartit,       
         work           -- for building transit

integer s4,     -- for setting state4
        wj,     -- copy of work[j], speedwise
        wk      -- copy of work[k], speedwise

    states = repeat(0,24)   -- (24 = 4!)
    for i=1 to 24 do    -- generate all possible permutations of the mru state
        states[i] = perm(i,{1,2,3,4})
    end for
-- sorting makes it easier to see what is going on, but actually has no impact:
--  as above any and all initial states are 100% equivalent for the empty set.
--include builtins\sort.e
--states = sort(states)
    transitions = repeat(0,24)
    snartitions = repeat(0,24)
    state4 = repeat(0,24)
    for i=1 to 24 do            -- for each possible initial state
        transit = repeat(0,4)
        snartit = repeat(0,4)
        for j=1 to 4 do         -- for promoting each entry to 1
--p2js:
--          work = states[i]    -- (uses four fresh copies)
            work = deep_copy(states[i]) -- (uses four fresh copies)
            wj = work[j]
            for k=1 to 4 do     -- map <wj by +1 ...
                wk = work[k]
                if wk<wj then
                    work[k] = wk+1
                end if
            end for
            work[j] = 1         -- ... and wj becomes 1, eg
                                -- {1,2,3,4} pick 3rd -> {2,3,1,4}
                                --                 ie {+1,+1,=1,as_is}
            if wj=4 then
                s4 = j
            end if

            transit[j] = find(work,states)

--p2js:
--          work = states[i]    -- (uses four fresh copies)
            work = deep_copy(states[i]) -- (uses four fresh copies)
            wj = work[j]
            for k=1 to 4 do     -- map >wj by -1 ...
                wk = work[k]
                if wk>wj then
                    work[k] = wk-1
                end if
            end for
            work[j] = 4         -- ... and wj becomes 4, eg
                                -- {4,3,2,1} demote 3rd -> {3,2,4,1}
                                --                 ie {-1,-1,=4,as_is}
            snartit[j] = find(work,states)

        end for
        transitions[i] = transit
        snartitions[i] = snartit
        state4[i] = s4
    end for
    regstate = 1
end procedure
build_state_tables()

sequence mloc                   -- var-ids (idx to symtab) which are already in a register
         mloc = repeat(0,4)     -- ignore all entries > reginfo
sequence mreg                   -- register (1/2/3/4 = eax/ecx/esi/edi) to go with each mloc entry
         mreg = repeat(0,4)
--integer reginfo               -- "in use" part of mloc/mreg.  -- (declared above)
--        reginfo = 0           -- (makes emptying the table nice and quick)
--
constant x0167 = {eax,ecx,esi,edi},     -- we track these using 1/2/3/4,
         y0167 = {1,2,0,0,0,0,3,4}      -- but code is emitted using 0/1/6/7


integer ireg    -- 1/2/3/4 form of reg

procedure clearIreg()
-- kill off any entries in mreg for ireg:
    for i=reginfo to 1 by -1 do
        if mreg[i]=ireg then
            mreg[i] = mreg[reginfo]
            mloc[i] = mloc[reginfo]
            reginfo -= 1
        end if
    end for
end procedure

procedure clearReg(integer reg)
-- kill off any entries in mreg for reg:
    ireg = y0167[reg+1] -- 0/1/6/7 -> 1/2/3/4
    clearIreg()
end procedure

procedure promoteReg(integer reg)
-- used when we spot an important register, eg when we transtmpfer something
--  from one instruction to the next, move it up in the most recently used
--  stakes, so it does not get clobbered in a subsequent loadReg/spareReg.
    ireg = y0167[reg+1]
    if ireg then    -- not edx/ebx (but instead a tracked register)
        regstate = transitions[regstate][ireg]
    end if
end procedure

procedure demoteReg(integer reg)
-- used to force a register to be selected next, eg if we want to load a
--  value, not already in a register (as proved by loadReg(,,1) returing
--  -1) into eax, we demoteReg(eax) before calling loadReg again without 
--  the noload option.
--DEV
if reg=edx then ?9/0 end if
if reg=ebx then ?9/0 end if
    ireg = y0167[reg+1]
    if ireg then    -- not edx/ebx (but instead a tracked register)
        regstate = snartitions[regstate][ireg]
    end if
end procedure

procedure addReg(integer N)
-- add reg info that [N] is in ireg:
    reginfo += 1
    if reginfo>length(mreg) then
        mreg = append(mreg,ireg)
        mloc = append(mloc,N)
    else
        mreg[reginfo] = ireg
        mloc[reginfo] = N
    end if
end procedure

procedure clearMem(integer N)
-- remove any reference to var N as we just clobbered it
integer k
    k = find(N,mloc)
    if k and k<=reginfo then
        mreg[k] = mreg[reginfo]
        mloc[k] = mloc[reginfo]
        reginfo -= 1
    end if
end procedure

procedure loadMem(integer r, integer N)
integer k, xrm
    if X64 then
        emitHex1(#48)
        if r>7 then ?9/0 end if -- (placeholder for more code)
    end if
    if symtab[N][S_NTyp]=S_TVar then
        k = symtab[N][S_Tidx]
        if X64=0 then
            k *= 4
        else
            k *= 8
        end if
        if k<-128 then
            xrm = #85+r*8   -- 0o2r5
            emitHex62w(mov_dword,xrm,k)         -- mov reg,[ebp-nnnn]
        else
            xrm = #45+r*8   -- 0o1r5
            emitHex3l(mov_dword,xrm,k)          -- mov reg,[ebp-nn]
        end if
    else
        if r=eax and X64=0 then
            emitHex5v(mov_eax_mem32,N)          -- mov eax,[N]
        else
            xrm = #05+r*8 -- 0o0r5
            emitHex62v(mov_dword,xrm,N)         -- mov reg,[N]
        end if
    end if
end procedure

--integer load_to_edx   -- 1="if var not already in a register, load it into edx please"
--      load_to_edx     -- NB callee must reset this to 0 after calling loadReg.
--with trace

--DEV/SUG 
constant CLEAR  = #01
constant NOLOAD = #02
function loadReg(integer N, integer flags=0)
--function loadReg(integer N, integer clr, integer noload=0)
--
-- If var N is already in a register, return that
-- else load to new reg (inverse most recently used order) and return that.
-- To keep things simple never use ebx (base=0) or edx (the dealloc reg).
--
-- set CLEAR in flags if we are about to damage the reg (eg opAddiii), so:
--  if we find N: kill all info, including N, about that reg,
--  otherwise grab a new reg, but save no info before returning.
--
-- if NOLOAD is set in flags return -1 when [N] is not already loaded
--
-- The return value is always promoted to the most recently used register.
-- Returns 0/1/6/7 for eax/ecx/esi/edi.
--
integer k, r
    markConstUsed(N)
    if lastline!=emitline then
        -- ensure opLnt/reginfo=0 happens now, if it is about to...
--DEV (leave for now, see comments at top of psched.e)
--      skipFixup = 1   -- ...but don't spanner the scheduler!
        lineinfo()
    end if
    if symtab[N][S_NTyp]=S_TVar 
    and and_bits(symtab[N][S_State],K_Fres) then
        promoteReg(eax)     -- (added 31/8/15)
        return eax
    end if
    k = find(N,mloc)
    if k and k<=reginfo then    -- found it
        ireg = mreg[k]
        if and_bits(flags,CLEAR) then
            -- trash all references to reg, including N
--DEV when is ireg 0 here?
            if ireg then clearIreg() end if
        end if
        r = x0167[ireg] -- 1/2/3/4 -> 0/1/6/7
        regstate = transitions[regstate][ireg]
        return r
    end if
    if and_bits(flags,NOLOAD) then return -1 end if
--  if load_to_edx then
--      r = edx
--  else
    ireg = state4[regstate]
    r = x0167[ireg] -- 1/2/3/4 -> 0/1/6/7
    -- kill off any entries in mreg for r/ireg,
    clearIreg()
    -- then add info about the var just loaded:
    if and_bits(flags,CLEAR)=0 then addReg(N) end if
    -- and mark r/ireg as "most recently used"
    regstate = transitions[regstate][ireg]
    --  end if
--  if sched then
--      schedule(0,0,regbit[r+1],pUV,0,N)
--  end if
    loadMem(r,N)
    return r
end function

--function loadEdx(integer N)
----
---- If var N is already in a register, return that,
----    otherwise load to edx and return that.
----
---- IE don't move/copy it out of an existing register yet,
----    (wait until after cmp h4 etc), but put it in edx if
----    it ain't already somewhere else.
----
---- Returns 0/1/3/6/7 for eax/ecx/edx/esi/edi.
----
--integer k, r, xrm
--  markConstUsed(N)
--  if lastline!=emitline then   --??
--      -- ensure opLnt/reginfo=0 happens now, if it is about to...
----DEV (leave for now, see comments at top of psched.e)
----        skipFixup = 1   -- ...but don't spanner the scheduler!
--      lineinfo()
--  end if
--  k = find(N,mloc)
--  if k and k<=reginfo then    -- found it
--      ireg = mreg[k]
--      r = x0167[ireg] -- 1/2/3/4 -> 0/1/6/7
--      regstate = transitions[regstate][ireg]
--      return r
--  end if
--  if sched then
--      schedule(0,0,edxbit,pUV,0,N)    -- ?? not sure we need this...
--  end if
--if newEBP then (this may not be right, for edx)
--  loadToReg(edx,N)                    -- mov edx,[N]
--else
--  emitHex6v(mov_edx_mem32,N)          -- mov edx,[N]
--end if
--  return edx
--end function

procedure loadToReg(integer tgtreg, integer src, integer clr=0)
-- mov tgtreg,[src] (nb variable length instructions emitted)
-- clr, if present, should be CLEAR as per loadReg, ie if we are
-- about to damage tgtreg (or simply no benefit to logging it).
integer currreg
integer xrm
if tgtreg=edx then ?9/0 end if
    currreg = loadReg(src,NOLOAD)
    if currreg=-1 then -- not already loaded
        demoteReg(tgtreg)
        currreg = loadReg(src,clr)                  -- mov tgtreg,[src]
    elsif currreg!=tgtreg then
        if X64 then
            if currreg>7 then ?9/0 end if -- (placeholder for more code)
            if tgtreg>7 then ?9/0 end if -- (placeholder for more code)
        end if
        xrm = #C0 + tgtreg + currreg*8 -- 0o3ct
        emitHexx2(mov_reg,xrm)                      -- mov tgtreg,currreg
--DEV sug/untried...
        clearReg(tgtreg)
    end if
end procedure

procedure storeMem(integer N, integer reg)
integer k, xrm
    if X64 then
        emitHex1(#48)
        if reg>7 then ?9/0 end if   -- (placeholder for more code)
    end if
    if symtab[N][S_NTyp]=S_TVar then
        if and_bits(symtab[N][S_State],K_Fres) then ?9/0 end if
        k = symtab[N][S_Tidx]
        if X64=0 then
            k *= 4
        else
            k *= 8
        end if
        if k<-128 then
            xrm = #85+reg*8 -- 0o2r5
            emitHex62w(mov_reg,xrm,k)   -- mov [ebp-n],reg
        else
            xrm = #45+reg*8 -- 0o1r5
            emitHex3l(mov_reg,xrm,k)    -- mov [ebp-n],reg
        end if
    else
-- 5/10/14: (newEmit)
--      if reg=eax then
        if reg=eax and X64=0 then
            emitHex5v(mov_mem32_eax,N)  -- mov [N],eax
        else
            xrm = #05+reg*8 -- 0o0r5
            emitHex62v(mov_reg,xrm,N)   -- mov [N],reg
        end if
    end if
end procedure

--DEV/SUG const NOEMIT = 0, MODIFIED = 1
--procedure storeReg(integer reg, integer N, integer emit=1, integer modified=0)
procedure storeReg(integer reg, integer N, integer emit, integer modified)
--
-- Update regstate as needed for mov[N],reg.
--
-- if emit is 1, actually perform the move, else it is eg call opLen (with
-- an integer tgt) left reg==[N], aka ecx==[tgt], that we are just logging.
--
-- if modified is 1, the reg no longer holds what we thought it did, hence
--  we should/must remove any other vars logged as being available in reg.
-- However there is a counter-intuitive spin to this simple flag: while of
--  course it should be 0 if the register has not been modified, it should
--  *not* always be 1 if it has - instead that depends on whether there is
--  any point in it being set:
--      1) a previous reg = loadReg(N,CLEAR) has recently just cleaned
--          up, therefore using storeReg(reg,,,0) avoids a pointless scan,
--          that is when passing a result from loadReg back to storeReg.
--      2) after reg = spareReg(x) there will be no entries for reg, hence
--          again a storeReg(reg,,,0) is likewise better/faster.
--      3) when reginfo is 0, a storeReg(,,,1) just wastes time by looking
--          for something that cannot be present.
--      4) It will not break anything by accidentally passing 1, except to
--         make the compiler slower and/or generate slower code, whereas a
--         wrong 0 can cause all manner of nasty problems.
--
-- reg may be ebx/edx, in which case we need to discard any mloc for [N]
-- but do not save ebx/edx anywhere in mreg since they are not tracked.
--
-- N may be 0 for the forward ParamList case, ie the actual var-id is
--  backpatched when we later process the routine. This just means that
--  we cannot save any reginfo (we would probably never want to anyway).
--  Update: N=0 was originally allowed for when this was called from the
--          middle of pmain.e, it can probably no longer happen now.
--
integer k
--, xrm
    markConstUsed(N)
    ireg = y0167[reg+1]
    -- first kill off any entries in mreg/mloc for reg:
    if ireg and modified and reginfo then clearIreg() end if
    if ireg=0 then -- ebx/edx - just remove any entry for N
        clearMem(N)
    elsif N then
        k = find(N,mloc)
        if k and k<=reginfo then
            mreg[k] = ireg
        else
            addReg(N)
        end if
        regstate = transitions[regstate][ireg]
    end if
    if emit then
--      if sched then
--          schedule(regbit[reg+1],0,0,pUV,0,N)
--      end if
        storeMem(N,reg)
    end if
end procedure

--DEV/SUG const ALLOW_EDX=1
--function spareReg(integer allow_edx=0)
function spareReg(integer allow_edx)
--
--  Allocate a work register, returns one of eax/ecx/edx/esi/edi.
--  allow_edx, fairly obviously, indicates whether, when the main four
--  are in use, it should return edx, or trash the least recently used.
--  In all cases, the result of spareReg will not clash with the
--  last 3 loadReg/storeReg, eg in "a[b]=c" we loadReg() the ref(a),
--  the index(b), and the replacement(c), and then need another reg
--  for the base - but edx is going to be used to decref the prev
--  value of a[b], so base = spareReg(0) is just the ticket.
--   (ps/btw: the above example is no longer strictly accurate,
--            seeing as the base field has since been removed.)
--
integer r, k
    ireg = state4[regstate]
    k = find(ireg,mreg)
    if k and k<=reginfo then
        if allow_edx then
            return edx
        else
            clearIreg()
        end if
    end if
    regstate = transitions[regstate][ireg]
    r = x0167[ireg]
    return r
end function

-- mergeSets.
-- =========
-- Create subsets of reginfo/mloc/mreg for later restore, eg:
--
--          mov eax,[a]     <== so we know [a] is in eax,
--          mov ecx,[c]     <==        and [c] is in ecx
--          mov edi,[d]     <==        and [d] is in edi
--          if <some condition> then    (assume eg eax>=ecx)
--              xor edi,edi     <== edi trashed
--                              <== eax/ecx aka [a]/[c] valid here
--          else
--              xor ecx,ecx     <== ecx trashed
--                              <== eax/edi aka [a]/[d] valid here
--          end if
--          <== Here we want just [a] is in eax
--
-- Since this is a one-pass compiler, loop tops are always "reginfo=0"'d, as		[ DEV.. ]
--  we have no idea what the register state will be at the end for/end while.
-- The start of every routine is also "reginfo=0"'d, as again we have no idea
--  what the register state will be at all the (future) calling locations.
-- However there are plenty of opportunities in eg an if/elsif/else construct
--  for re-using stuff we know is in a register. Of course there is some
--  saving over not re-loading, but probably more from removing dependencies
--  and in particular the dreaded AGI stalls. Naturally a big hll construct
--  is highly likely to have no common register use, it is the tiny ones
--  that damage just one register but would otherwise throw away all info
--  about the other register contents that we are really targetting here.
--
-- The easiest one to explain is scMerge: eg "if a or b or c then code" ==>
--
--      mov reg1,[a]    -- (unless already in a reg)
--      cmp reg1,0
--      jnz <scBP>      -- [1]
--      mov reg2,[b]    -- (unless already in a reg)
--      cmp reg2,0
--      jnz <scBP>      -- [2]
--      mov reg3,[c]    -- (unless already in a reg)
--      cmp reg3,0
--      jz <otherBP>    -- [3]
--      <code>
--
--  ie two (short-circuit) jumps get placed on the scBP chain (the fail after
--  cmp[c] ends up on some other backpatch chain), which are backpatched to
--  <code> when we get to it. What we want is to take a copy of the register
--  state at 1, keep what is still there at 2, keep what is still there at 3,
--  and restore that set as we start to process <code>. Got that?
--
--          <aside> cmp [x],0 is rarely a good idea; not only is it two clocks,
--                  x is not left in a register for possible later reuse, and
--                  it eliminates any chance of rescheduling the loads, eg if
--                  the above load of b were to be moved up two instructions,
--                  it could get rid of two stalls. </aside>
--
-- The other five mergesets are as follows:
--      exprMerge is the logical counterpart to scMerge, ie this is the
--                          conditional expression fail, often just moved
--                          straight into ifMerge, whereas scMerge is the
--                          conditional expression succeed.
--      exitMerge is, fairly obviously, the set of jump points to the end of
--                          a loop. On small loops this will just be the top
--                          condition; on larger loops any common register
--                          state is quickly likely to become empty.
--      breakMerge is practically identical to exitMerge, except that it
--                          applies to select statements not loops.
--      ifMerge covers the if/elsif/else chain, ie conditional (fail) jumps
--                          to try with the next branch of the if construct.
--      endIfMerge covers branches leading to an "end if", ie the
--                          unconditional jumps over following elsif/else.
--
--  Note the need to save/restore these sets over nested if statements.
--  There are matching backpatch chains to all these sets, scBP, exprBP,
--  exitBP, breakBP, ifBP, and EndIfBP.
--
-- Now in pglobals.e:
--global constant scMerge=1, exprMerge=2, exitMerge=3, ifMerge=4, endIfMerge=5, breakMerge=6

constant Mtgt=1, Minfo=2, Mloc=3, Mreg=4

integer tgt     -- jump target/label destination, set before calling merge.

sequence tilr   -- target/info/loc/reg, one for each mergeSet (working/current copy)
sequence tilrs  -- stacked "" "" "" "", one stack "" "" "".
--
--  ie/eg tilr[ifMerge] is {tgt,reginfo,mloc,mreg}, -- (see above)
--        tilrs[ifMerge] is stacked copies of tilr[ifMerge]:
--
--      if test then                                        | stacked/static
--          if test2 then                                   |   copies of
--              if test3 then   | tilr[if/endIfMerge]       .
--              else            |  being used here          .
--              end if          |                           .
--          end if                                          | tilr[if/endifMerge]
--      end if                                              | kept for later use
--
--  (The five mergesets have been very carefully chosen so that they do not interfere
--   with each other, much like the way that the hll constructs if/for/while can be
--   nested, except that this also occurs at a very much lower level/smaller scale.)
--  tilr[ifMerge] is in fact the reginfo common to all the jumps on that ifBP chain.

    tilr = repeat({-1},6)   -- dummy/non-existent target for each mergeSet
    tilrs = repeat({},6)    -- empty stacks for each mergeSet

--with trace
--DEV/SUG const RESTORE = -1, MERGE = 0, MERGE_AND_RESTORE = 1
procedure merge(integer mergeSet, integer restore)
-- copy/trim the appropriate reginfo set
-- restore is a 3-way flag:
--  -1 : just restore (eg after an "else", can ignore reginfo)
--   0 : just merge (eg after an "and"/for all opJxxx opcodes)
--   1 : merge and restore (eg at "end if", ie "fall-thru" handling)
-- restore is only non-zero (true) when processing an opLabel.
integer tms, ims, N, k
sequence tmp
sequence tilrx -- working copy/new replacement value of tilr[mergeSet]
sequence tloc,treg

    tms = tilr[mergeSet][Mtgt]
    if tgt!=tms then
        -- first branchpoint for this set/tgt, so take a copy of reginfo,
        --  but push the previous mergeset onto a stack for later use.
        -- (if restore!=0, this is an opLabel, and since tgt!=tms we
        --  assume it is an opLabel with all branches to it optimised
        --  away. Since we never saved anything, no harm no foul.)
        if restore=0 then
            tilrx = {tgt,reginfo,0,0}
            if reginfo then
                tilrx[Mloc] = mloc
                tilrx[Mreg] = mreg
            end if
            tilrs[mergeSet] = append(tilrs[mergeSet],tilr[mergeSet])
            tilr[mergeSet] = tilrx
        end if
    else
        ims = tilr[mergeSet][Minfo]
        if restore>=0 then
            -- extending the set with another branchpoint,
            -- so delete any reginfo that is not still valid/common.
            -- (killing refcounts to minimise copy/clone overheads)
            if reginfo and ims then
                tloc = tilr[mergeSet][Mloc]  tilr[mergeSet][Mloc] = 0
                treg = tilr[mergeSet][Mreg]  tilr[mergeSet][Mreg] = 0
                for i=ims to 1 by -1 do
                    N = tloc[i]
                    k = find(N,mloc)
                    if k=0 or k>reginfo or mreg[k]!=treg[i] then
                        treg[i] = treg[ims]
                        tloc[i] = tloc[ims]
                        ims -= 1
                    end if
                end for
                tilr[mergeSet][Mloc] = tloc
                tilr[mergeSet][Mreg] = treg
            else
                ims = 0
            end if
            tilr[mergeSet][Minfo] = ims
        end if
        if restore then
            -- at a label, restore the (merged) reginfo, and pop
            --  the appropriate mergeset stack.
            -- (killing refcounts to minimise copy/clone overheads)
            tilrx = tilr[mergeSet]
            reginfo = tilrx[Minfo]
            if reginfo then
                mloc = tilrx[Mloc]
                mreg = tilrx[Mreg]
--DEV try else tilr[mergeSet] = 0, Mloc/Mreg=0??
            end if
            tmp = tilrs[mergeSet]   tilrs[mergeSet] = 0
            k = length(tmp)         tilrx = tmp[k]
            tmp = tmp[1..k-1]       tilrs[mergeSet] = tmp
            tilr[mergeSet] = tilrx
        end if
    end if
end procedure


-- globals (also) used by pmain.e:
global constant Bcde = {opJlt,opJle,opJeq,opJne,opJge,opJgt}
                -- ie  { "<" ,"<=" ,"=(=)","!=", ">=",">"  }    -- (= and == treated the same, for now)
global constant eJmp = {  0,    0,    1,    1,    0,    0 } -- equality tests (when either is an integer)
                -- ie   BOTH,BOTH,EITHER,EITHER,BOTH,BOTH   --  ie/eg 1.1=1 can be tested using alu/cmp,
                                                            --      but 1.1>1 needs fld/fcmp/fnstsw etc.
constant        setc = {0o234,0o236,0o224,0o225,0o235,0o237}
global constant Scde = {opSlt,opSle,opSeq,opSne,opSge,opSgt}
                --        1,    2,    3,    4,    5,    6       -- ie find(opcode,Scde/Bcde):
global constant tnot = {  5,    6,    4,    3,    1,    2  }    -- as in eg not(a=b)
global constant trev = {  6,    5,    3,    4,    2,    1  }    -- cmp a,b -> cmp b,a (imm8/32 must be 2nd)
constant ccde = {jl_rel32,jle_rel32,je_rel32,jne_rel32,jge_rel32,jg_rel32}

--constant opMath = {opAdd,opAddi,opSub,opSubi,opMul,opMuli,opDiv,opDivf,opDivi}
----                    64      65  67     68       70  71     73    78     74

--with trace

--DEV rename as svtype:
integer sudt
        sudt = 0

sequence symtabN
object gi
integer isKridt, gtype

function needstypecheck(integer isParam)
-- symtabN and sudt must be set before call.
-- Must not be called if not binding (gvar_scan incomplete, we
--  don't have enuf info for this check, or rather what we did
--  know would have been used to avoid emitting the opTchk, so
--  always type_check)
-- NB Only valid for builtin, not user defined types.
integer tmask
--/* RDS only (see call stmt)
    if not bind then return 1 end if
--*/
    -- always check rtype in type xxx(rtype p) ...  -- may no longer be necessary, see opTchk in pgscan [DEV]
    if and_bits(symtabN[S_State],K_type) then return 1 end if
    -- always check params of routines that ...
    if isParam then
        -- ... are/could be target of routine_id
        if isKridt then return 1 end if
    end if
    gi = symtabN[S_gInfo]
--DEV return 0 might be in order, if nowt actually sets it...?
    if atom(gi) then return 1 end if
--  if atom(gi) then return 1/0 end if  -- triggered in t38
    gtype = gi[gType]
    --
    -- sudt is the declaration type, from symtabN[S_vtype],
    --          checked to be <=T_object before this call.
    --          [[ie if sudt>T_object, this is not called.]]
    -- gtype is accumulated possible types from the gvar_scan.
    -- so if gvar_scan shows var p1 could get assigned something
    --  which is not covered by sudt, return non-zero (true):
    --
    tmask = xor_bits(sudt,T_object) -- ie "not" the low 4 bits (T_object=#0F)
    --
    return and_bits(tmask,gtype)
    --
    -- examples:
    --  If lhs/tgt is integer, tmask = 0b1110, aka signal tc rqd if
    --   rhs/src (gtype) could be any of T_Seq, T_string, or T_atom.
    --  Whereas if tgt is object, tmask=0b0000, aka signal tc rqd if
    --   rhs could be any of {}, aka never.
    --  Although sudt=gtype would always return 0, in practice
    --   pmain/localtypes ensure no such opTchk is emitted, and in
    --   fact the latter example of tgt is object is also bogus in
    --   that such code does not actually ever occur.
    --
end function

-- dest/src/src2 are for eg dest = src[src2], dest[src]=src2, etc...
--  for each of the vars/temps/constants involved, we set up a bunch of other
--  info (beyind the var no/symtab index in dest/src/src2) such as type info,
--  min/max integer values, element type, sequence length, state bitmask, and
--  var type (ie gvar/tvar/const/namespace/type/proc/func)
--
--  The type info is (at least) 3-way: the declared type, the localtype (see
--  pltype.e for the convoluted mechanisms that maintain this), and rootType(s)
--  which are always in the range 1 (T_integer) to 15 (T_object). The handling
--  of these is perhaps the most difficult and error-prone aspect of the entire
--  compilation process...
--
--  <aside>
--   Initialising these to 0 helps the compiler a bit, by removing any need to
--   check for unassigned vars, ie use of opJcc/opJccE instead of cmp, etc.
--  </aside>
--
integer src     src = 0
integer sltype  sltype = 0  -- localtype (ie symtab[src][S_ltype])
integer slroot  slroot = 0  -- == rootType(sltype)
--DEV 19/2/2012:
--integer smin  smin = 0    -- min integer value \ ignore if T_N bit
atom smin   smin = 0    -- min integer value \ ignore if T_N bit
--integer smax  smax = 0    -- max integer value / if set in slroot.
atom smax   smax = 0    -- max integer value / if set in slroot.
integer setyp   setyp = 0   -- element type     \ ignore if T_sequence bits
integer slen    slen = 0    -- sequence length  /  are not set in slroot.
integer state1  state1 = 0  -- (ie symtab[src][S_State])
integer ssNTyp1 ssNTyp1 = 0 -- (ie symtab[src][S_NTyp])

integer src2    src2 = 0
integer sltype2 sltype2 = 0
integer slroot2 slroot2 = 0     -- == rootType(sltype2)
--DEV 19/2/2012:
--integer smin2 smin2 = 0
atom smin2  smin2 = 0
--integer smax2 smax2 = 0
atom smax2  smax2 = 0
integer setyp2  setyp2 = 0
integer slen2   slen2 = 0
integer state2  state2 = 0
integer ssNTyp2 ssNTyp2 = 0

integer dest    dest = 0
integer dtype   dtype = 0   --DEV document this carefully...
atom dmin   dmin = 0
atom dmax   dmax = 0
integer detyp   detyp = 0
integer dlen    dlen = 0
integer dname   dname = 0

-- transtmpfer etc control:
integer tmpd    tmpd = 0    -- 0=nowt doing, else reg/value can be used next instruction
integer tmpr    tmpr = 0    -- -1: use tmpv, else use this register (0..7)
integer tmpv    tmpv = 0    -- constant literal value to use (when tmpr=-1)
 -- tmpd can be -1 just to signal "we know", eg in an opJcc case, or
 -- a var/tmp no/symtab index, in which case tmpr/v hold something to
 -- use in place of that (and thus may avoid unnecessary store/load).

--DEV document/rename these:::
integer vroot   vroot = 0
atom vmin   vmin = 0
atom vmax   vmax = 0
integer vlen    vlen = 0

--with trace
procedure getSrc()
object ss
object ssgi
--if src=408 then trace(1) end if
    if src<=0 then ?9/0 end if  -- sanity check
    ss = symtab[src]
--  TIP: see opJif, 7/12/15 as one possible cause
--  if atom(ss) then ?9/0 end if
    ssNTyp1 = ss[S_NTyp]
    state1 = ss[S_State]
    sudt = ss[S_vtype]  -- for opTchk
    if length(ss)<S_ltype then
        --DEV how often does this trigger, and why?
if ssNTyp1!=S_Const then
    puts(1,"see getSrc[1]\n")
end if
        sltype = sudt
    else
        sltype = ss[S_ltype]
    end if
if NOLT=0 or bind or lint then
    if getLocal(src) then
--15/1/16. We cannot know if optset[OptTypeCheck] is/was on, so we cannot do this...
        slroot = and_bits(rootType(Ltype),rootType(sudt))
if slroot=0 then
        slroot = or_bits(rootType(Ltype),rootType(sudt))
end if
        sltype = Ltype
--      slroot = rootType(Ltype)
        smin = Lmin
        smax = Lmax
--      setyp = Letyp
        if and_bits(slroot,T_sequence)=T_string then
            setyp = T_integer
        else
            setyp = Letyp
        end if
        slen = Lslen
--DEV move these up top, along with the two below??
--      state1 = ss[S_State]
--      ssNTyp1 = ss[S_NTyp]    -- for opMovbi (and not NewBase)
        return
    end if
end if -- NOLT
--  slroot = and_bits(rootType(sltype),rootType(sudt))
    if sltype>T_object or sudt>T_object then
        slroot = and_bits(rootType(sltype),rootType(sudt))
    else
        slroot = and_bits(sltype,sudt)
    end if
    --DEV ?? use gType as per getDest??
    --  if and_bits(stype,T_integer) then   --DEV is this necessary??
    smin = MININT
    smax = MAXINT
--  else
--      smin = MAXINT
--      smax = MININT
--  end if
    if and_bits(slroot,T_sequence)=T_string then
        setyp = T_integer
    else
        setyp = T_object
    end if
    slen = -1
--  state1 = ss[S_State]
--  ssNTyp1 = ss[S_NTyp]
    if ssNTyp1=S_Const then
        if slroot=T_integer
        and (and_bits(state1,K_rtn) or ss[S_Init]) then
            smin = ss[S_value]
            smax = smin
            return
        end if
        if and_bits(slroot,T_sequence)
        and and_bits(state1,K_litnoclr)=K_litnoclr then
            slen = length(ss[S_value])
        end if
--      if and_bits(state1,K_lit) then return end if
        if length(ss)<S_gInfo then return end if
    end if
    --  if not and_bits(state1,K_type) then -- gInfo meaningless on type routine parameters
    if bind then
        ssgi = ss[S_gInfo]
        if sequence(ssgi) then
            gtype = ssgi[gType]
            if slroot>T_object or gtype>T_object then ?9/0 end if
            slroot = and_bits(slroot,gtype)
            smin = ssgi[gMin]
            smax = ssgi[gMax]
--if DEBUG then
--          if smin=smax and ssNTyp1!=S_Const then
--              if smin=MININT then ?9/0 end if
--              if smin=MAXINT then ?9/0 end if
--          end if
--end if
--          setyp = ssgi[gEtyp]
            if and_bits(slroot,T_sequence)=T_string then
                setyp = T_integer
            else
                setyp = ssgi[gEtyp]
            end if
            slen = ssgi[gLen]
--      elsif bind then
        else
            if slroot=T_integer and and_bits(state1,K_noclr) then
--              smin = ss[S_value]
--              smax = smin
                ssgi = ss[S_gNew]
                if atom(ssgi) then
                    ss = ss[S_value]    -- also kills refcount
                    symtab[src][S_gNew] = {T_integer,ss,ss,0,-1}
                end if
            end if
        end if
    end if
    --  end if
end procedure

procedure getSrc2()
object ss
object ssgi
integer vtype2, vroot2  -- declared type/root
--if src2=408 then trace(1) end if
    if src2<=0 then ?9/0 end if     -- sanity check
    ss = symtab[src2]
    vtype2 = ss[S_vtype]
    if vtype2>T_object then
        vroot2 = rootType(vtype2)
    else
        vroot2 = vtype2
    end if
if NOLT=0 or bind or lint then
    if getLocal(src2) then
--??        stype2 = rootType(Ltype)
--if Ltype>T_object then ?9/0 end if
        sltype2 = Ltype
        slroot2 = and_bits(rootType(Ltype),vroot2)
--15/1/16 [as per GetSrc()]
if slroot2=0 then
        slroot2 = or_bits(rootType(Ltype),vroot2)
end if
        --      slroot2 = rootType(Ltype)
        smin2 = Lmin
        smax2 = Lmax
        --      setyp2 = Letyp
        if and_bits(slroot2,T_sequence)=T_string then
            setyp2 = T_integer
        else
            setyp2 = Letyp
        end if
        slen2 = Lslen
--DEV move these up top, along with the two below??
        state2 = ss[S_State]
        ssNTyp2 = ss[S_NTyp]    -- for opMovbi (and not NewBase)
        return
    end if
end if -- NOLT
--  sltype2 = ss[S_ltype]
-- 20/9:
    if length(ss)<S_ltype then
--      sltype2 = ss[S_vtype]
        sltype2 = vtype2
    else
        sltype2 = ss[S_ltype]
    end if
--  slroot2 = and_bits(rootType(sltype2),vroot2)
    if sltype2>T_object then
        slroot2 = and_bits(rootType(sltype2),vroot2)
    else
        slroot2 = and_bits(sltype2,vroot2)
    end if
    --  slroot2 = rootType(sltype2)
    -- DEV ditto above
    --  if and_bits(slroot2,T_integer) then
    smin2 = MININT
    smax2 = MAXINT
--  else
--      smin2 = MAXINT
--      smax2 = MININT
--  end if
    if and_bits(slroot2,T_sequence)=T_string then
        setyp2 = T_integer
    else
        setyp2 = T_object
    end if
    slen2 = -1
    state2 = ss[S_State]
    ssNTyp2 = ss[S_NTyp]
    if ssNTyp2=S_Const then
        if slroot2=T_integer
        and (and_bits(state2,K_rtn) or ss[S_Init]) then
            smin2 = ss[S_value]
            smax2 = smin2
            return
        end if
        if and_bits(slroot2,T_sequence)
        and and_bits(state2,K_litnoclr)=K_litnoclr then
            slen2 = length(ss[S_value])
        end if
--      if and_bits(state2,K_lit) then return end if
        if length(ss)<S_gInfo then return end if
    end if
    --  if not and_bits(state2,K_type) then     -- gInfo meaningless on type routine parameters
    if bind then
        ssgi = ss[S_gInfo]
        if sequence(ssgi) then
            gtype = ssgi[gType]
            if slroot2>T_object or gtype>T_object then ?9/0 end if
            slroot2 = and_bits(slroot2,gtype)
            smin2 = ssgi[gMin]
            smax2 = ssgi[gMax]
--if DEBUG then
--          if smin2=smax2 and ssNTyp2!=S_Const then
--              if smin2=MININT then ?9/0 end if
--              if smin2=MAXINT then ?9/0 end if
--          end if
--end if
--          setyp2 = ssgi[gEtyp]
            if and_bits(slroot2,T_sequence)=T_string then
                setyp2 = T_integer
            else
                setyp2 = ssgi[gEtyp]
            end if
            slen2 = ssgi[gLen]
--      elsif bind then
        else
            if slroot2=T_integer and and_bits(state2,K_noclr) then

-- DEV 17/3/09: a) this *WAS*INDEED*STILL* smin/smax, not smin2/smax2 (!!!)
--              b) as per getSrc(), changed to create an S_gNew.

--              smin = ss[S_value]
--              smax = smin
                ssgi = ss[S_gNew]
                if atom(ssgi) then
                    ss = ss[S_value]    -- also kills refcount
                    symtab[src2][S_gNew] = {T_integer,ss,ss,0,-1}
                end if
            end if
        end if
    end if
    --  end if
end procedure

integer vtype   --DEV document/rename...
,vetyp  -- for opRepe1

procedure getDest()
sequence sd
object sdgi, sdgn
integer state
--, vtype
--if dest=388 then trace(1) end if
    if dest<=0 then ?9/0 end if     -- sanity check
    sd = symtab[dest]
if repl then
    object name = sd[S_Name]
    dname = iff(not string(name)?name:iff(name="-1"?-1:0))
else
    dname = sd[S_Name]  -- for testing whether dest is a temp
end if
--  if 0 then   --DEV not a good idea...
--      vroot = rootType(sd[S_vtype])
--      if getLocal(dest) then
--          dtype = Ltype
--          dmin = Lmin
--          dmax = Lmax
--          detyp = Letyp
--          dlen = Lslen
--          return
--      end if
--  end if
    --  dmin = MININT
    --  dmax = MAXINT
    dmin = MAXINT
    dmax = MININT
--21/8/10 (possibly temp?)
--  sdgi = sd[S_gInfo]
    if length(sd)<S_gInfo then
        sdgi = 0
    else
        sdgi = sd[S_gInfo]
    end if
--21/8/10 (possibly temp?)
--  vtype = sd[S_ltype]
--  vroot = rootType(sd[S_vtype])
    vroot = sd[S_vtype]
    if vroot>T_object then vroot = rootType(vroot) end if
    if length(sd)<S_ltype then
        vtype = vroot
    else
        vtype = sd[S_ltype]
    end if

    vmin = MININT
    vmax = MAXINT

    vlen = -1
    if sequence(sdgi) then
        gtype = sdgi[gType]
        if vroot>T_object or gtype>T_object then ?9/0 end if
        vroot = and_bits(vroot,gtype)
        vetyp = sdgi[gEtyp]
        vmin = sdgi[gMin]
        vmax = sdgi[gMax]
        vlen = sdgi[gLen]
    end if

    if bind then
        dtype = 0
        detyp = 0
        dlen = -1
--21/8/10 (possibly temp?)
--      sdgn = sd[S_gNew]
        if length(sd)<S_gNew then
            sdgn = 0
        else
            sdgn = sd[S_gNew]
        end if
        if sequence(sdgn) then
            dtype = sdgn[gType]
            dmin = sdgn[gMin]
            dmax = sdgn[gMax]
            detyp = sdgn[gEtyp]
--8/6/10:
if not sequence(sdgi) then
    vetyp = detyp
end if
            dlen = sdgn[gLen]
--DEV 21/2/14: (not entirely convinced this is right, but it did the trick)
if dlen>0 then
    vlen = dlen
end if
        else
            state = sd[S_State]
            if vroot=T_integer then
                dtype = T_integer
            end if
--8/6/10:
if not sequence(sdgi) then
    vetyp = T_object
end if
            if and_bits(state,K_noclr) then
                dtype = T_integer
                if sequence(sdgi) then
                    dmin = sdgi[gMin]
                    dmax = sdgi[gMax]
                else
                    -- no need to create gNew here as storeDest imminent.
                    dmin = sd[S_value]
                    dmax = dmin
                end if
            end if
        end if
    else
--DEV put back 26/1/10 (?9/0 on line 7792/opRmdr)
        dtype = vroot
--      dtype = vtype   -- 3/8/9 (big change?!)
        detyp = T_object
--8/6/10:
vetyp = T_object
        dlen = -2
--DEV 21/10/09:
        dmin = MININT
        dmax = MAXINT
    end if

end procedure

integer forceSeq        -- treat as sequence in storeDest
        forceSeq = 0    -- (nb slen and setyp *must* be set when this is 1)

integer pc

integer callpending     -- set to 1 between each opFrame/opCall pair. (There
        callpending = 0 --  is no point adding localtype info when all we are
                        --  doing is setting up external function parameters.)
integer gtypeonly       -- set to 1 to suppress ltAdd
        gtypeonly = 0   -- (15/11/09) now only used in opTchk, to set the globaltype
                        --  on type check calls

--integer from_opRepCh = 0

procedure storeDest()
object sdgi
if NOLT=0 or bind or lint then
 --
 -- This is a reasonable place to trap #isginfo problems, best
 --  for the compiler under "p p -d p" (with doOneInclude=1 in
--   plist.e), and concentrating on the last time it occurs,
--   rather than the first. Also, do *NOT* forget to ignore
--   this if it is off in the first few rounds but gets itself
--   sorted out by the time it hits list.asm!!!
 --  You need to get a var no (eg 1155) from looking at the
 --  list.asm in the first place, and of course that number
 --  will change if interpreting/compiling/-nodiag/etc.
 --
-- if dest=1155 then
--  if and_bits(slroot,T_string)
--  and not and_bits(dtype,T_string) then
--      printf(1,"storeDest Tstring: emitline is %d\n",emitline)
--  end if
--  if and_bits(setyp,T_N)
--  and not and_bits(detyp,T_N) then
--      printf(1,"storeDest etype: emitline is %d\n",emitline)
--  end if
-- end if
-- if dest=710 then
----    if dtype=0 then ?9/0 end if
--  printf(1,"storeDest 710: emitline is %d, callpending=%d, dtype=%d,slroot=%d,sltype=%d\n",{emitline,callpending,dtype,slroot,sltype})
-- end if
    if and_bits(slroot,T_atom)=T_integer then
        if dmin>smin then dmin = smin end if
        if dmax<smax then dmax = smax end if
    else
        smin = MININT
        smax = MAXINT
    end if
    dtype = rootType(dtype)
    dtype = or_bits(dtype,slroot)
    if and_bits(slroot,T_sequence)
    or forceSeq then
        detyp = or_bits(detyp,setyp)
        if dlen=-1 then
            dlen = slen
        elsif dlen!=slen then
--puts(1,"\n\nbingo!\n\n")
            dlen = -2
        end if
        forceSeq = 0
    else
        setyp = T_object
        slen = -1   -- ?? or -2 ?? [DEV]
    end if

--if NOLT=0 or bind or lint then
--if from_opRepCh then
--  ?{dtype,callpending,gtypeonly,dest}
--end if
    if not callpending
    and not gtypeonly then
        Lmin = smin
        Lmax = smax
        Letyp = setyp
        Lslen = slen
--DEV test/replace:
-- triggered 29/9, having worked fine for months
--   (during self-host: more investigation required)
--sdgi = symtab[dest] --DEV temp for ex.err (sdgi is not otherwise used until later)
--if vtype!=symtab[dest][S_ltype] then ?9/0 end if

--21/8/10 (possibly temp?)
if length(symtab[dest])>=S_ltype then
--if from_opRepCh then
--?1
--end if
        ltAdd(SET,dest,symtab[dest][S_ltype],sltype,pc-1)
--15/2/19:
elsif NEWGSCAN then
        ltAdd(SET,dest,dtype,sltype,pc-1)
end if
    end if
--if from_opRepCh then
--?0
--end if
--end if -- NOLT
    gtypeonly = 0
--  if and_bits(stype,T_integer) then
--  stype = rootType(stype)
--  dtype = rootType(dtype)
--  if and_bits(slroot,T_atom)=T_integer then
--      if dmin>smin then dmin = smin end if
--      if dmax<smax then dmax = smax end if
--  end if
--  dtype = or_bits(dtype,slroot)
--DEV?? (Nah, [gType] must be a rootType... I think)
--  dtype = or_type(dtype,sltype)
--  if and_bits(slroot,T_sequence)
--  or forceSeq then
--      detyp = or_bits(detyp,setyp)
--      if dlen=-1 then
--          dlen = slen
--      elsif dlen!=slen then
--          dlen = -2
--      end if
--      forceSeq = 0
--  end if
--
--if dest=388 then trace(1) end if
--  sd = symtab[dest]
--  sdgi = sd[S_gInfo]
--  if sequence(sdgi) then
--  end if
--  sdgi = {dtype,dmin,dmax}
--  if and_bits(dtype,T_atom)!=T_integer then
----        dmin = 0
----        dmax = 0
--      dmin = MAXINT
--      dmax = MININT
--  end if
--DEV re-try this later: (bug in s={"fred"} for one)
--  if and_bits(dtype,T_string) then
    if and_bits(dtype,T_sequence)=T_string then
--      detyp = or_bits(detyp,T_integer)
        if not and_bits(detyp,T_integer) then detyp=9/0 end if
    end if
--DEV re-try this later: (maybe not, see t5)
--  if dtype=0 or dtype>T_object then dtype=9/0 end if
    if dtype>T_object then dtype=9/0 end if
    if detyp>T_object then detyp=9/0 end if
    sdgi = {dtype,dmin,dmax,detyp,dlen}
    if dest<=0 then ?9/0 end if     -- sanity check
--21/8/10 (possibly temp?)
 if length(symtab[dest])>=S_gNew then
--DEV experimental 18/1/2012:
if sequence(symtab[dest][S_gNew]) then
    if symtab[dest][S_gNew][gLen]=-2
    and sdgi[gLen]=-1 then
        ?9/0    -- sanity check
    end if
end if
    symtab[dest][S_gNew] = sdgi
 end if
if dest=710 then
--  ?sdgi
    sdgi = symtab[dest]
end if
end if -- NOLT
end procedure


integer bcode, reg, wrk

--with trace
function SetCC(integer opcode, integer soon)
--
-- Common to opScc and opJcc (the latter passes opJcc translated to equiv opScc)
--
-- If the result is preditable set tmpv to 0/1, tmpd to -1, and return 1
--
-- soon handles some final common code:
--  0: from opScc/tii and isInt: common setup for a cmp
--  schoon: from opJcc/tii: common setup for a cmp (schoon due to jump)
--      (actually can be lastJump (== 0 or schoon) from opJcc)
--  -1: from opJcc/not tii: no common setup, opScc about to be called.
--
integer rtype, rtype2   --DEV use slroot, slroot2??
    getSrc()
    getSrc2()
    if tmpd then
        if src=tmpd then
            if tmpr<0 then
                smin = tmpv
                smax = tmpv
                sltype = T_integer  --DEV slroot?
            end if
        else
            if src2!=tmpd then ?9/0 end if  -- bug in transtmpfer?
            if tmpr<0 then
                smin2 = tmpv
                smax2 = tmpv
                sltype2 = T_integer --DEV slroot2?
            end if
        end if
    end if
--DEV: put this in if needed?:
-- if src=src2 then smin = 0 smax = 0 smin2 = 0 smax2 = 0 end if

--  if sltype=T_integer
--  and sltype2=T_integer then
    rtype = sltype      if rtype>T_object then rtype=rootType(sltype) end if
    rtype2 = sltype2    if rtype2>T_object then rtype2=rootType(sltype2) end if
    if rtype=T_integer
    and rtype2=T_integer then
        if opcode=opSeq then
        --
        -- The following examples are "smin..smax is always/never (relop) smin2..smax2";
        --  the (n<op>m) shows which of the given example numbers we are comparing, and
        --  the [1..6] show the correct mirror-pairing we should expect.
        --
--DEV ideally gscan should have done this... (once local values in place)
            if smin=smax and smin2=smax2 then
                tmpv = (smin=smin2)
                tmpd = -1
            elsif smin>smax2        -- eg 3..4 is never == 1..2     (3>2)   [1]
               or smax<smin2 then   -- eg 1..2 is never == 3..4     (2<3)   [2]
                tmpv = 0
                tmpd = -1
            end if
        elsif opcode=opSne then
            if smin=smax and smin2=smax2 then
                tmpv = (smin!=smin2)
                tmpd = -1
            elsif smin>smax2        -- eg 3..4 is always != 1..2    (3>2)   [1]
               or smax<smin2 then   -- eg 1..2 is always != 3..4    (2<3)   [2]
                tmpv = 1
                tmpd = -1
            end if
        elsif opcode=opSlt then
            if smax<smin2 then      -- eg 1..2 is always < 3..4     (2<3)   [3]
                tmpv = 1
                tmpd = -1
            elsif smin>=smax2 then  -- eg 2..3 is never < 1..2      (2>=2)  [4]
                tmpv = 0
                tmpd = -1
            end if
        elsif opcode=opSle then
            if smax<=smin2 then     -- eg 1..2 is always <= 2..3    (2<=2)  [5]
                tmpv = 1
                tmpd = -1
            elsif smin>smax2 then   -- eg 3..4 is never <= 1..2     (3>2)   [6]
                tmpv = 0
                tmpd = -1
            end if
        elsif opcode=opSgt then
            if smin>smax2 then      -- eg 3..4 is always > 1..2     (3>2)   [6]
                tmpv = 1
                tmpd = -1
            elsif smax<=smin2 then  -- eg 1..2 is never > 2..3      (2<=2)  [5]
                tmpv = 0
                tmpd = -1
            end if
        elsif opcode=opSge then
            if smin>=smax2 then     -- eg 2..3 is always >= 1..2    (2>=2)  [4]
                tmpv = 1
                tmpd = -1
            elsif smax<smin2 then   -- eg 1..2 is never >= 3..4     (2<3)   [3]
                tmpv = 0
                tmpd = -1
            end if
        else
            ?9/0 -- opcode in error?!?
        end if
    else    -- not two ints
        if opcode=opSeq
        or opcode=opSne then
--          if not and_bits(slroot,slroot2) then
            if not and_bits(rtype,rtype2) then
                -- but, "a"==={'a'}, remember...
--              if not and_bits(slroot,T_sequence)
--              or not and_bits(slroot2,T_sequence) then
                if not and_bits(rtype,T_sequence)
                or not and_bits(rtype2,T_sequence) then
                    tmpv = (opcode=opSne)
                    tmpd = -1
                end if
-- 9/9/9:
            elsif not and_bits(rtype,T_atom)            -- p1 is sequence
              and not and_bits(rtype2,T_atom)           -- p2 is sequence
              and slen>=0                               -- p1 of known length
              and slen2>=0                              -- p2 of known length
              and slen!=slen2 then                      -- not equal
                tmpv = (opcode=opSne)
                tmpd = -1
            end if
        elsif ( not and_bits(rtype,T_sequence)          -- if (     p1 is atom
               and not and_bits(rtype2,T_atom))         --      and p2 is sequence)
           or ( not and_bits(rtype,T_atom)              -- or (     p1 is sequence
               and not and_bits(rtype2,T_sequence)) then    --      and p2 is atom)
            --           Scde = {opSlt,opSle(,opSeq,opSne),opSge,opSgt}
            --atom(p1),seq(p2):    T     T  (   F     T  )   F     F
            --seq(p1),atom(p2):    F     F  (   F     T  )   T     T
            tmpv = 0
            if not and_bits(rtype,T_sequence) then  -- p1 is atom (top line)
                if opcode=opSlt or opcode=opSle then
                    tmpv = 1
                end if
            else                                    -- p1 is seq (btm line)
                if opcode=opSge or opcode=opSgt then
                    tmpv = 1
                end if
            end if
            tmpd = -1
        end if
    end if
    if tmpd!=-1 and soon!=-1 then   -- final common setup if rqd:
--      if sched then
--          sch00n = soon
--      end if
        if tmpd and tmpr>=0 then
            promoteReg(tmpr)    -- prevent any loadReg/spareReg clash
        end if
        if src=tmpd and tmpr>=0 then
            reg = tmpr
            tmpd = 0
        elsif smin=smax and rtype=T_integer then -- (same as src=tmpd and tmpr<0, see above)
            if smin2=smax2 and rtype2=T_integer then ?9/0 end if -- should have been handled above
            bcode = trev[bcode] -- invert test (imm8/32 must be 2nd)
            reg = -1
        else
--31/8/15: (loadReg wanted the promoteReg)
            if (src2!=tmpd or tmpr<0)
            and (smin2!=smax2 or rtype2!=T_integer)
--          and (smin2!=smax2 or rtype2!=T_integer or reg=eax)
            and symtab[src2][S_NTyp]=S_TVar 
            and and_bits(symtab[src2][S_State],K_Fres) then
                promoteReg(eax)
            end if
            reg = loadReg(src)                          -- mov reg,[src]
        end if
        if src2=tmpd and tmpr>=0 then
            wrk = tmpr
            tmpd = 0
        elsif smin2=smax2 and rtype2=T_integer then -- (same as src2=tmpd, see above)
            if reg=-1 then ?9/0 end if -- should have been handled above
            wrk = reg
            tmpd = -1
        else
            wrk = loadReg(src2)                         -- mov wrk,[src2]
        end if
        if reg=-1 then
            reg = wrk
            smin2 = smin
            tmpd = -1
        end if
        return 1
    end if
    return 0    -- result is not predictable
end function

--DEV check this for eax target...?? [DONE, I think]
--procedure emitHex10sdi(integer dest, integer v)
procedure emitHex10sdi(integer dest, atom v)
-- store the literal integer v in [dest]
--  (v may be #40000000 or MAXINT+1 for unassigned)
--if X64 then ?9/0 end if -- (breaks rip addressing, because of that trailing dword...) [if you /really/ wanted to, you could have an "isVar4"...]
    if lastline!=emitline then lineinfo() end if
    if symtab[dest][S_NTyp]=S_TVar then ?9/0 end if
    if and_bits(symtab[dest][S_State],K_Fres) then ?9/0 end if
--  if sched then
--      schedule(0,0,0,pUV,0,dest)
--  end if
--  bool r14 = false
    if X64=1 and (v>#7FFFFFFF or v<-#80000000) then
        -- mov r14,imm64:
        emitHex1(#49)
        emitHex1(#BE)
        emitHexDword(and_bits(v,#FFFFFFFF))
        v = floor(v/#100000000)
        emitHexDword(and_bits(v,#FFFFFFFF))
--      emitHex1(#48)
--      r14 = true
        emitHex1(#4C)
--          if k<-128 then
--              emitHex6w(mov_rbpi32_r14,k)     -- mov [rbp+imm32],r14
--          else
--              emitHex3(mov_rbpi8_r14,k)       -- mov [rbp+imm8],r14
--       mov_rbpi8_r14  = {#89,#75},    -- 0o211 0o165 imm8         -- mov [rbp+imm8],r14 (needs a #4C)
--       mov_rbpi32_r14 = {#89,#B5},    -- 0o211 0o265 imm32        -- mov [rbp+imm32],r14 (needs a #4C)
--       mov_ebpi32_ebx = {#89,#9D},    -- 0o211 0o235 imm32        -- mov [ebp+imm32],ebx(0)
--          end if
--      v = 0
        x86 &= mov_mem32_esi                    -- mov [dest],r14
        x86 &= {isVar,0,0}
        x86 &= dest
    else
        if v then
            x86 &= mov_m32_imm32                -- mov [dest],imm32
        else
            x86 &= mov_mem32_ebx                -- mov [dest],ebx (0)
        end if
        if q86>1 then
            quad2(isVar4,dest)
        else
--          emitHex4v(dest) -- {isVar,0,0,dest}
            x86 &= {isVar4,0,0}
            x86 &= dest
        end if
        if v then
            emitHexDword(v)
        end if
    end if
end procedure

--integer rb, rw, rx, xrm, opcode, op1
integer xrm, opcode, op1

integer mod -- middle octal digit:
                        --    m
constant m_add = #00,   -- 0o000    add
         m_or  = #08,   -- 0o010    or (bitwise)
--       m_adc = #10,   -- 0o020    adc (unused)
--       m_sbb = #18,   -- 0o030    sbb (unused)
         m_and = #20,   -- 0o040    and (bitwise)
         m_sub = #28,   -- 0o050    sub
         m_xor = #30,   -- 0o060    xor (bitwise)
         m_cmp = #38    -- 0o070    cmp

--7/4/16. Values such as #7FFFFFFF are now valid for 32bit p.exe creating 64bit exe:
--procedure regimm365(integer imm)
procedure regimm365(atom imm)
--removed 25/1/20 (over an 4294967295 aka #FFFFFFFF when running "p64 -c -norun p32.exu")
--  if isFLOAT(imm) then ?9/0 end if
    --
    --  Perform one of the above 8 operations on a register using the specified literal.
    --   (ie add/or/adc/sbb/and/sub/xor/cmp reg,imm8/32)
    --   reg, mod must be set before the call. The 365 not only indicates that this can
    --   be used any day of the year, but that it emits a 3, 6, or 5 byte instruction.
    --
--  if sched then
--      rb = regbit[reg+1]
--      schedule(rb,0,rb,pUV,1,0)
--  end if
    if X64 then
        emitHex1(#48)
    end if
    xrm = #C0+mod+reg   -- 0o3mr
    if imm<=127 and imm>=-128 then
        emitHex3l(#83,xrm,imm)                      -- <op> reg,imm8
    elsif reg=eax then
        op1 = mod+5 -- 0o0m5
        emitHex5w(op1,imm)                          -- <op> eax,imm32
    else
        emitHex62w(#81,xrm,imm)                     -- <op> reg,imm32
    end if
end procedure

procedure reg_src2()
-- perform one of the above 8 operations (m_add..m_cmp) 
--  on reg using src2 (already getSrc2()'d)
--DEV 6/4/16:
--  if slroot2=T_integer and smin2=smax2 then
--30/5/21:
--  if slroot2=T_integer and smin2=smax2 and integer(smin2) then
    if slroot2=T_integer and smin2=smax2 and integer(smin2) and smin2<=#FFFFFFFF then
        -- (dev: trouble here if it's a K_rtn)
        regimm365(smin2)                            -- <op> reg,imm
    else
        wrk = loadReg(src2)                         -- mov wrk,[src2]
--      if sched then
--          rb = regbit[reg+1]
--          rw = regbit[wrk+1]
----        if mod<m_cmp then rx=rb else rx=0 end if
----        schedule(rb+rw,0,rx,pUV,1,0)
--          schedule(rb+rw,0,rb,pUV,1,0)
--      end if
        op1 = mod+1         -- 0o0m1
        xrm = #C0+wrk*8+reg -- 0o3wr
        emitHexx2(op1,xrm)                          -- <op> reg,wrk
        -- (eg xor edi,ecx is 0o061 0o317.
        --  btw, we could equivalently say:
        --  op1 = mod+3         -- 0o0m3
        --  xrm = #C0+reg*8+wrk -- 0o3rw
        --  but only for xor - I only mention
        --  this in case an xor "looks wrong"
        --  compared to some other assembler )
    end if
end procedure

procedure XxxBits()
-- common code to opXxxBits and opJBits
    getSrc2()
    -- first select the middle octal digit opcode (m)
    if opcode=opAndBits then    mod = m_and
    elsif opcode=opOrBits then  mod = m_or
    elsif opcode=opXorBits then mod = m_xor
    end if
    reg_src2()
end procedure

constant by4 = #80  -- 0o200: scale for [base+idx*4]; use 0 for [base+idx]
constant dl = {edx}

procedure set_op1(object res)
    op1 = mov_dword
    if sequence(res) then
        op1 = mov_byte
        res = res[1]
        if res>edx then ?9/0 end if
    end if
    mod = res*8         -- 0o0r0
--  if sched then
--      rw = regbit[res+1]
--  end if
end procedure

procedure sibLoad743(object res, integer scale, integer idx, integer base, integer offset)
--
-- Emit a mov res,[base+idx*scale+offset] instruction (7, 4, or 3 bytes).
--  res is 0..7 (eax..edi) or {0..2} (al..dl)
--  scale is by4(0o200) for [base+idx*4], 0 for [base+idx] (by2,by8 possible as well)
--  idx,base are 0..7 (eax..edi)
--  offset is any integer value (shortest allowable instruction form selected)
--
integer sib
    set_op1(res)    -- set op1 to mov_dword/byte, mod to 0o0r0, rw to regbit[res+1]
    sib = scale+idx*8+base  -- 0osib ;-)
--  if sched then
--      rb = regbit[base+1]
--      rx = regbit[idx+1]
--      schedule(0,rb+rx,rw,pUV,0,0)
--  end if
    if X64 then
        emitHex1(#48)
    end if
    if offset>127 or offset<-128 then
        xrm = #84+mod   -- 0o2r4
        emitHex7i(op1,xrm,sib,offset)           -- mov res,[base+idx*scale+d32]
    elsif offset then
        xrm = #44+mod   -- 0o1r4
        emitHex4l(op1,xrm,sib,offset)           -- mov res,[base+idx*scale+d8]
    else
        xrm = #04+mod   -- 0o0r4
        emitHex3l(op1,xrm,sib)                  -- mov res,[base+idx*scale]
    end if
end procedure

--DEV inline.. and fix the emitHex2...
procedure baseLoad632(object res, integer base, integer offset)
--
-- Emit a mov res,[base+offset] instruction (6, 3, or 2 bytes).
--  res and base are eax..edi (0..7) [may be same]
--  offset is any integer value (shortest allowable instruction form selected)
--
    set_op1(res)    -- set op1 to mov_dword/byte, mod to 0o0r0, rw to regbit[res+1]
--  if sched then
--      rb = regbit[base+1]
--      schedule(0,rb,rw,pUV,0,0)
--  end if
    xrm = mod+base      -- 0o0rb
    if offset>127 or offset<-128 then
        xrm += #80      -- 0o2rb
        emitHex62w(op1,xrm,offset)          -- mov reg,[base+d32]
    elsif offset then
        xrm += #40      -- 0o1rb
        emitHex3l(op1,xrm,offset)           -- mov reg,[base+d8]
    else
        emitHex2(op1,xrm)                   -- mov reg,[reg]
    end if
end procedure

procedure leamov(integer reg, integer N)
integer k
    if symtab[N][S_NTyp]=S_TVar then
        if and_bits(symtab[N][S_State],K_Fres) then ?9/0 end if
        k = symtab[N][S_Tidx]
        if X64=0 then
            k *= 4
        else
            emitHex1(#48)
            k *= 8
        end if
        if k<-128 then
            xrm = 0o205 + reg*8 -- 0o2r5
            emitHex62w(lea,xrm,k)               -- lea reg,[ebp-nnnn]
        elsif k=0 then
--DEV (tryme)
--  mov_reg       =  #89,         -- 0o211 0o3sd              -- mov dst,src
--  and 0o350+reg
--CPU Disasm
--Address   Hex dump            Command                                  Comments
--0040A001  89EA            MOV EDX,EBP
--0040A003  8BD5            MOV EDX,EBP
            xrm = 0o305 + reg*8 -- 0o3r5
            emitHex2(mov_dword,xrm)             -- mov reg,ebp
            -- (btw lea reg,[ebp] must be encoded as [ebp+(byte)0])
        else
            xrm = 0o105 + reg*8 -- 0o1r5
            emitHex3l(lea,xrm,k)                -- lea reg,[ebp-nn]
        end if
    elsif X64 then
        emitHex1(#48)
        if reg>7 then ?9/0 end if -- (placeholder for more code)
        xrm = 0o005+reg*8
        emitHex62v(lea,xrm,N)                   -- lea reg,addr N
    else
        emitHex5v(mov_eax_imm32+reg,N)          -- mov reg,addr N
    end if
    clearReg(reg)
end procedure

procedure zero(integer N)
integer k
-- 28/1/2012
--if and_bits(symtab[N][S_State],K_Fres) then ?9/0 end if --DEV/sug xor eax,eax would be fine!
--  if symtab[N][S_NTyp]=S_TVar then
--if X64 then ?9/0 end if -- might yet want #48 [DEV]
    if X64 then
        emitHex1(#48)
    end if
    if and_bits(symtab[N][S_State],K_Fres) then
        emitHex2s(xor_eax_eax)                  -- xor eax,eax
    elsif symtab[N][S_NTyp]=S_TVar then
        k = symtab[N][S_Tidx]
        if X64=0 then
            k *= 4
        else
            k *= 8
--          emitHex1(#48)
        end if
        if k<-128 then
-- 23/1/15:
--if X64 then
--          if k<-#8000 then ?9/0 end if
--          emitHex6w2(mov_ebpi32_ebx,k)        -- mov [ebp-nnnn],ebx(0)
--end if
            emitHex6w(mov_ebpi32_ebx,k)         -- mov [ebp-nnnn],ebx(0)
        else -- (byte offset still rqd even if 0)
            emitHex3(mov_ebpi8_ebx,k)           -- mov [ebp-n],ebx(0)
        end if
    else
--      if X64 then
--          emitHex1(#48)
--      end if
        emitHex6v(mov_mem32_ebx,N)              -- mov [N],ebx (0)
    end if
end procedure

procedure pushvar(integer N)
integer k
-- 28/01/2012:
--if and_bits(symtab[N][S_State],K_Fres) then ?9/0 end if -- (push eax would be fine?)
--  if symtab[N][S_NTyp]=S_TVar then
--if X64 then ?9/0 end if
    if and_bits(symtab[N][S_State],K_Fres) then
        emitHex1(push_eax)                      -- push eax
    elsif symtab[N][S_NTyp]=S_TVar then
        k = symtab[N][S_Tidx]
        if X64=0 then
            k *= 4
        else
            k *= 8
        end if
        if k<-128 then
            emitHex6w(push_ebpi32,k)            -- push dword[ebp-nnnn]
        else -- (byte offset still rqd even if 0)
            emitHex3(push_ebpi8,k)              -- push dword[ebp-n]
        end if
    else
        emitHex6v(push_mem32,N)                 -- push dword[N]
    end if
end procedure

procedure popvar(integer N)
integer k
--if X64 then ?9/0 end if
    if and_bits(symtab[N][S_State],K_Fres) then ?9/0 end if -- (pop eax would be fine?)
    if symtab[N][S_NTyp]=S_TVar then
        k = symtab[N][S_Tidx]
        if X64=0 then
            k *= 4
        else
            k *= 8
        end if
        if k<-128 then
            emitHex6w(pop_ebpi32,k)             -- push dword[ebp-nnnn]
        else -- (byte offset still rqd even if 0)
            emitHex3(pop_ebpi8,k)               -- pop dword[ebp-n]
        end if
    else
        emitHex6v(pop_mem32,N)                  -- pop dword[N]
    end if
end procedure

procedure movRegImm32(integer reg, atom v)
integer mov_reg_imm32
    if X64 then
        emitHex1(#48)
--16/1/16:
--      xrm = 0o300+reg
--      emitHex62w(mov_regimm32,xrm,v)      -- mov reg,imm32
        if v>#7FFFFFFF or v<-#80000000 then
--          if v>#FFFFFFFF or v<-#80000000 then ?9/0 end if -- placeholder for more code...
            mov_reg_imm32 = mov_eax_imm32+reg
            emitHex5w(mov_reg_imm32,and_bits(v,#FFFFFFFF))      -- mov reg,imm64(lsd)
            v = floor(v/#100000000)
            emitHexDword(and_bits(v,#FFFFFFFF))                 --      ...imm64(msd)
        else
            xrm = 0o300+reg
            emitHex62w(mov_regimm32,xrm,v)      -- mov reg,imm32
        end if
    else
        mov_reg_imm32 = mov_eax_imm32+reg
        emitHex5w(mov_reg_imm32,v)          -- mov reg,imm32
    end if
end procedure

procedure storeImm(integer dest, atom v)
integer isFresDest
integer k
    isFresDest = and_bits(symtab[dest][S_State],K_Fres)
    if v=0 then
        if isFresDest then
            if X64 then
                emitHex1(#48)
            end if
            emitHex2s(xor_eax_eax)              -- xor eax,eax
        else
            zero(dest)
        end if
    elsif isFresDest then
-- #48, maybe fine without (done inside movRegImm32)
--      emitHex5w(mov_eax_imm32,v)              -- mov eax,imm32
--      if X64 then
--14/2/16 removed (now covered by movRegImm32):
--          if v>#7FFFFFFF then ?9/0 end if -- placeholder for more code...
--      end if
        movRegImm32(eax,v)                      -- mov eax,imm32
    elsif symtab[dest][S_NTyp]=S_TVar then
        k = symtab[dest][S_Tidx]
        if X64=1 and (v>#7FFFFFFF or v<-#80000000) then
            k *= 8
            -- mov r14,imm64:
            emitHex1(#49)
            emitHex1(#BE)
            emitHexDword(and_bits(v,#FFFFFFFF))
            v = floor(v/#100000000)
            emitHexDword(and_bits(v,#FFFFFFFF))
            emitHex1(#4C)
            if k<-128 then
                emitHex6w(mov_rbpi32_r14,k)     -- mov [rbp+imm32],r14
            else
                emitHex3(mov_rbpi8_r14,k)       -- mov [rbp+imm8],r14
            end if
        else
            if X64 then
                k *= 8
                emitHex1(#48)
                if v>#7FFFFFFF then ?9/0 end if -- placeholder for more code...
            else
                k *= 4
            end if
            if k<-128 then
                emitHex10ww(mov_ebpd32_i32,k,v)     -- mov [ebp-nnnn],imm32
            else -- (when k is 0 we still need a byte offset of 0)
                emitHex7d8v(mov_ebpd8_i32,k,v)      -- mov [ebp-nn],imm32
            end if
        end if
    else
        if X64 then
--          emitHex1(#90)
            emitHex1(#48)
        end if
        --DEV is this used anywhere else?
        emitHex10sdi(dest,v)                    -- mov [dest],imm32
    end if
end procedure

procedure storeVno(integer dest, integer N)
-- Typically N is a routine_id, which may need mapping...
integer isFresDest
integer k
if not newEmit then ?9/0 end if
    isFresDest = and_bits(symtab[dest][S_State],K_Fres)
    if isFresDest then
        if X64 then
            emitHex3l(#48,0o307,0o300)  -- #48 0o307 0o300 imm32        -- mov rax,imm32
        else
            emitHex1(0o270)             -- 0o270 imm32                  -- mov eax,imm32
        end if
    elsif symtab[dest][S_NTyp]=S_TVar then
        k = symtab[dest][S_Tidx]
        if X64 then
            k *= 8
            emitHex1(#48)
        else
            k *= 4
        end if
        if k<-128 then
            emitHex2(0o307,0o205)       -- 0o307 0o205 d32 imm32    -- mov [ebp-d32],imm32
            emitHexDword(k)
        else -- (when k is 0 we still need a byte offset of 0)
            emitHex3l(0o307,0o105,k)    -- 0o307 0o105 d8 imm32     -- mov [ebp-d8],imm32
        end if
    else
        if X64 then
            emitHex1(#48)
        end if
--if crash then
--  ?9/0 -- erm, not isVar but isVar4, surely?
--end if
--      emitHex6v(mov_m32_imm32,dest)   -- 0o307 0o005 m32 i32      -- mov [m32],imm32
--bugfix 30/7/15: (p7 -d demo\db01 gave out one of those "*** ERROR: decode of this segment ended at wrong address ***")
        if lastline!=emitline then lineinfo() end if
        x86 &= mov_m32_imm32                    -- mov [dest],imm32
        if q86>1 then
            quad2(isVar4,dest)
        else
            x86 &= {isVar4,0,0}
            x86 &= dest
        end if
    end if
    if N=0 then ?9/0 end if
    if q86>1 then
        quad2(isVno,N)
    else
        x86 &= {isVno,0,0,N}
    end if
end procedure

--!!sequence code   -- il (was in s5), x86 is built in s5

sequence opcount
if countTransTmpFer or showOpCounts then
    opcount = repeat(0,length(opNames))
end if

integer nextop, npc

--DEV we should probably make these permanent:
--integer tmppc, ttrap
--              ttrap = 0

--7/4/16: values such as -#80000000 now valid when 32bit p.exe is creating a 64bit exe:
--procedure transtmpfer(integer val, integer reg)
procedure transtmpfer(atom val, integer reg)
    if isFLOAT(val) then ?9/0 end if
--
-- See if the next (interesting) instruction can use a temp reg/value
--  directly, rather than via a back-to-back store/load, or perhaps
--  just to avoid an otherwise pointless store of the temp.
--
-- val is only used if reg is -1 (ie we have a literal/known value
--  which is not in a register), otherwise reg>=0 hold the value.
--
-- Also when storing a fixed literal integer, you must always store it,
--  but maybe also use it direct, ie/eg N=5 k=N+1 could be coded as
--  mov [N],5; mov [k],6 with this; without it would more likely be
--  mov [N],5; mov eax,[N], add eax,1 mov [k],eax, albeit gvar/ltype
--  handling may often avoid the worst of such excesses.
--
-- Some instructions are "non-players" (eg opLn).
-- Some instructions can be thought of as "producers" (being the ones
--  that call transtmpfer), and some as "consumers" (being the ones
--  that test/use tmpd/r/v), which must be paired in that order, and
--  the remaining other instructions should be treated as "blockers".
--  A producer can pass something to a consumer even with a gaggle of
--  non-players between them, unless an unknown/blocker is present.
--  Note that labels and frames/calls are both treated as blockers.
--  Theoretically there is nothing to prevent an instruction being
--  both producer and consumer.
--
-- Successful handover is indicated by dest being zero on return.
--

    -- must be cleared by callee (presumably both consumer and producer):
    if tmpd then ?9/0 end if
--
-- First, skip any "non-players". These opcodes do not transfer control
--  (aka jump) or damage any registers.
--
    npc = pc
    while 1 do
        nextop = s5[npc]
        -- skip any opLn (nb not opLnp/pt/t)
--      if nextop!=opLn then exit end if
--      npc += 2
--DEV 12/10:
        if nextop=opLn then
            npc += 2
        else
--15/11/20 (over switch length(r) do)
            exit
--/*
            if nextop!=opCtrl then exit end if
if pfileno=1 then
    ?s5[npc..npc+3]
    ?pfileno
?9/0
end if
            npc += 4
--*/
        end if
    end while

-- Then see if this "next interesting opcode" accepts transtmpfer info,
--  and is consuming the correct temp var(s).

    if nextop>=opJge and nextop<=opJle then
        if s5[npc+4]=dest
        or s5[npc+5]=dest then
            if s5[npc+6] then   -- tii case only...
                tmpd = dest
            end if
        end if

    elsif nextop>=opSge and nextop<=opSle then
        if s5[npc+2]=dest
        or s5[npc+3]=dest then
            if s5[npc+4]        -- tii case only...
            and s5[npc+6] then  -- isInt case only...
                tmpd = dest
            end if
        end if

    elsif nextop=opSubse1 or nextop=opSubse1i or nextop=opSubse1is then
        --  accepts transtmpfer() for idx/src2 only.
        if s5[npc+3]=dest then
            tmpd = dest
            if dname=-1 then
                -- We must store temps for subscripted compound ops, ie/eg
                --  s[i+j] += k, since the temp (of i+j) is used twice.
                --  (btw, similar conditions exist for opSubse/ss/etc, but
                --   only when/if transtmpfer starts playing with them...)
                if s5[npc+5] then   -- isCompound
                    if reg>=0 then
                        storeReg(reg,dest,1,0)                  -- mov [dest],reg
                    else
                        storeImm(dest,val)                      -- mov [dest],imm32
                        if reginfo then clearMem(dest) end if
                    end if
                end if
            end if
        end if

    elsif nextop=opRepe1 then
        --  accepts transtmpfer() for idx only.
        if s5[npc+2]=dest then
            tmpd = dest
        end if

    elsif nextop=opPow then
        --  accepts transtmpfer() for src2 only.
        if s5[npc+3]=dest then
            tmpd = dest
        end if

--DEV count next opcodes and report in sorted order...
    elsif countTransTmpFer then
        if  nextop!=opFrame -- must store [DEV remove later]
        and nextop!=opRetf  -- ""
        and nextop!=opLabel -- ""   ?? and ((link is non-zero) or (mergeType=0)) ??
----    and nextop!=opLn        -- ""   ???
        and nextop!=opLnt   -- ""
        and nextop!=opFor2  -- ""
        and nextop!=opEndFor    -- ""
--      and nextop!=opTime  -- ??!! (as opJmp - no params anyway)
--      and nextop!=opOpen  -- ""
        and nextop!=opJmp   -- ??!! (oh, bug if it's a tmp, else OK)
--      and nextop!=opFloor -- DEV (more investigation rqd)
--      and nextop!=opScmp  -- DEV (more investigation rqd) -- t22
--      and nextop!=opJnot  -- DEV (more investigation rqd) -- p.exw
--      and nextop!=opSubi  -- DEV (more investigation rqd)
--      and nextop!=opRmdr
--      and nextop!=opPuts
--      and nextop!=opSubse
--      and nextop!=opUminus -- 1 or 2 in tNN, 0 in p.exw.
-- compiler 20/2:
--      and nextop!=opRepe1 -- DEV (more investigation rqd) -- p.exw    (33)
--      and nextop!=opAddiii    -- DEV (more investigation rqd)         (25)
--      and nextop!=opAdd   -- DEV (more investigation rqd)             (17)
--      and nextop!=opSubss -- DEV (more investigation rqd)             (15)
--      and nextop!=opSub   -- DEV (more investigation rqd) -- p.exw    ( 9)
--      and nextop!=opApnd  -- ""                                       ( 7)
--      and nextop!=opMul                                               ( 7)
--      and nextop!=opFind                                              ( 7)
--X     and nextop!=opRepeat                                            ( 4)
--      and nextop!=opPow                                               ( 4)
--      and nextop!=opAndBits                                           ( 4)
--      and nextop!=opSubiii                                            ( 4)
--      and nextop!=opJif                                               ( 4)
--      and nextop!=opLen                                               ( 3)
--      and nextop!=opMkSq                                              ( 3)
--      and nextop!=opRepe                                              ( 2)
--      and nextop!=opJbits                                             ( 1)
--      and nextop!=opAlloc                                             ( 1)
--      and nextop!=opAddi                                              ( 1)
--      and nextop!=opMovbi -- 2 in t28, 0 in p.exw.                    ( 1)
--      and nextop!=opMove                                              ( 1)
          then
--trace(1)
            opcount[nextop] += 1
        end if
    end if
    if tmpd then
        tmpv = val
--      tmppc = npc
        tmpr = reg
        dest = 0
    end if
end procedure

integer mergeSet
integer lastJmp
        lastJmp = 1

procedure jinit(integer mpc)
    -- common code to conditional jumps.
    -- mpc is location of mergeSet
    if mpc!=pc+1 then ?9/0 end if   --DEV in prep. for removal of param
    mergeSet = s5[mpc]
    if mergeSet=isOpCode then
        if s5[mpc+2]!=opRetf then ?9/0 end if
    else
        tgt = s5[mpc+1]
--      link = s5[mpc+2]
        if s5[tgt-3]!=opLabel then ?9/0 end if
--      if s5[tgt-2]!=mergeSet then ?9/0 end if
        integer tmSet = s5[tgt-2]
        if tmSet!=mergeSet then ?9/0 end if
-- 15/10/2020 (until)
if tmSet!=0 then
        if tgt<mpc then ?9/0 end if -- see opJmp
end if
        lastJmp = (s5[tgt]=(mpc+2))
    end if
end procedure

--global
integer isGscan
        isGscan = 0

procedure detach(integer list, integer item)
--
-- "unlink" an item(a jump) from a linked list.
--
integer next
--if not isGscan then -- (hail mary)    -- NEWGSCAN?
    while 1 do
        next = s5[list]
        if next=item then
            next = s5[item]
            s5[list] = next
            exit
        end if
        list = next
--error 36 loop...
--if list=0 then ?"pilx86.2 line 4237: detach/exit" exit end if
    end while
--end if
end procedure

--  opJif = 26,         -- if b then
--  opJnot = 27,        -- if not b then
--  opJge = 28,         -- if b>=c then
--  opJlt = 29,         -- if b<c then
--  opJeq = 30,         -- if b=c then
--  opJne = 31,         -- if b!=c then
--  opJgt = 32,         -- if b>c then
--  opJle = 33,         -- if b<=c then
----DEV14: dead (see invert on opJnotx)
--  opJifx = 34,        -- if b[c] then
--  opJnotx = 35,       -- if not b[c] then
--
--  opJmp = 167,
--  opJtyp = 168,
--  opJbits = 169,
--  opJlen = 170,

--  if (?code>=opJif and ?code<=opJnotx)
--  or (?code>=opJmp and ?code<=opJlen) then
--
--  end if

--DEV/SUG: is we're here from jend, tmpd=1, and tmpv (always taken), 
--          then if we actually hit the tgt, set opcode to lastop....
function jskip()
--
-- For an always taken jump/return, skip following code
--  up to the next referenced label. In the case of a
--  return, set tgt to -1 to indicate it is OK to fall
--  off the end, otherwise it is an error to do so
--  without hitting the label we always jump to.
--
-- The real gain/main purpose here is to "propagate"
--  the effect of previous jumps being "never taken", eg
--  if we had "if sequence(x) then ... elsif/else ..."
--  and we proved (via dataflow analysis) x is always a
--  sequence, then not only can we drop the test on x,
--  but also all the elsif/else. Just before the elsif,
--  there is a jmp over them, when this gets there it
--  can omit that jump and everything up to the end if,
--  as long as all interim opLabel chains are 0, which
--  they will be, once we've detach()'d the jumps we
--  met along the way.
--
-- Returns 1 to indicate that all code between the jump
--  and the target opLabel was skipped, 0 otherwise.
--
-- While "constant DEBUG=0  if DEBUG then xxx end if" is
--  handled via emitON=0 in pmain.e, this does the same
--  for eg "if var then xxx end if", in the case where
--  gscan() has deduced that var is only ever set to 0
--  (plus many others, even say "if sequence(x[i])").
--
-- NB callee should save pc (npc = pc) before calling,
--    and use that (npc) for backpatch linkup on fail.
--
integer skip, noofitems
--, jtgt
integer skiptgt
--, mergeSet2
integer waspc = pc, xpc
    while 1 do
        nextop = s5[pc]
--28/7/17
        if nextop=opCatch then return 0 end if
--25/10/17 (nope...)
--      if nextop=opEndFor then return 0 end if
        if nextop=opLabel then
--DEV 28/4/13: (umm)
--      if nextop=opLabel
--      or nextop=opLogPos then
            if tgt=pc+3 then        -- all interim code skipped
                if NOLT=0 or bind or lint then
                    if pc!=waspc then
                        ltskip(pc,waspc)
                    end if
                end if -- NOLT
--DEV tryme:
--  pc3 = pc+3
--  ?? = s5[pc3]
--  ...
--  if ??=0 then
--      pc += 4
--  end if
                return 1
            elsif s5[pc+3]  -- label which has been referenced
               or (s5[pc+4]=opCtrl and and_bits(s5[pc+5],SWITCH)) then
                if NOLT=0 or bind or lint then
                    if pc!=waspc then
                        ltskip(pc,waspc)
                    end if
                end if -- NOLT
                -- while we cannot "skip" them, don't actually emit
                --  a jump over a bunch of opLabels and opCtrls...
                --  (ie stop incrementing pc, use the temporary
                --   work variable xpc for this test instead.)
                -- DEV nice try, but see notes in pemit.e (o0), and e6:
                --  The problem is this occurs too late/out-of-order,
                --  ie we may be jumping over a jmp 0 which is later
                --  optimised away by this same code....
                --  (can we not just isDead it at some point??)
                xpc = pc+4
                while xpc<tgt-3 do
--eg:
--  21:  opJmp,5,35,0,                       opJmp,endIfMerge,tgt,link
--  25:  opLabel,4,0,9,                      opLabel,ifMerge,0/x86loc,link
--  29:  opCtrl,6,5,n,                       opCtrl,ELSIF,link,emitline
--  32:  opLabel,5,0,24,                     opLabel,endIfMerge,0/x86loc,link
--                   ^^ tgt (=35)
--
--              for j=pc+4 to tgt-7 by 4 do
                    nextop = s5[xpc]
                    if nextop=opLabel then
                        xpc += 4
                    elsif nextop=opCtrl then
                        xpc += 4
--  62:  opLchk,389,8,17,569,1,              opLchk,N,typ,tokline,tokcol,fileno
                    elsif nextop=opLchk then
                        xpc += 6
                    else
                        return 0
                    end if
--              end for
                end while
                return 1
            end if
            pc += 4
--          pc += 5
--          pc += 6

--25/10/17:
--/!* --(nope)
        elsif nextop=opEndFor
--        and s5[waspc+1] = exitMerge
          and mergeSet=exitMerge
--        and ((s5[waspc+1]=exitMerge) or
--             (s5[waspc]=opLn and s5[waspc+
          and pc = tgt-6 then
--trace(1)
--27/9/19: (unconditional exit from for loop, copied from opEndFor)
--(nextop = s5[pc]==opEndFor: opEndFor,END+LOOP,bpFor)
            integer p1 = s5[pc+2],  -- last byte of opFor (holds loopTop addr)
                    tg2 = s5[p1-5] -- (was flags, now backpatch chain)
            if s5[p1-6]!=opFor2 then ?9/0 end if
            while tg2 do
                integer k = length(x86)-tg2,
                        backpatch = x86[tg2]
                x86[tg2] = k                        
                tg2 = backpatch
            end while
--/*
?{"pilx86.e line 4415, isGscan=",isGscan}
--?9/0
            tokline = emitline
            while 1 do
                tokcol = match("exit",exptext[fileno][tokline])
                if tokcol!=0 then exit end if
                tokline += 1
            end while
            tokcol += linestarts[tokline]-1
--          Abort("illegal/unsupported construct")
            ?`Abort("illegal/unsupported construct") (line 4425)`
--*/
--  81:  opLn,13,                            --:  exit
--  83:  opJmp,3,95,0,                       opJmp,exitMerge,tgt,link
--  87:  opLn,14,                            --: end for
--  89:  opEndFor,17,55,                         opEndFor,END+LOOP,bpFor
--  92:  opLabel,3,0,86,                         opLabel,exitMerge,0/x86loc,link
--          if NOLT=0 or bind or lint then
--              if pc!=waspc then
----                    ltskip(pc,waspc)
--                  ltskip(pc-2,waspc)
--              end if
--          end if -- NOLT
----            return 1
--          return 0
--*!/
--27/9/19
            skip = opSkip[nextop] -- (shd always be 3, from/see pops.e)
            if skip<=0 then ?9/0 end if
            pc += skip
        else
--      end if
            skip = opSkip[nextop]
            if skip>0 then
--              jtgt = jtgts[nextop]
--              if jtgt then
--DEV:
                if (nextop>=opJif and nextop<=opJnotx)
                or (nextop>=opJmp and nextop<=opJlen) then
                    integer mergeSet2 = s5[pc+1]
                    if mergeSet2!=isOpCode then -- ignore opRetfs
                        -- detach any skipped jumps:
                        skiptgt = s5[pc+2]
                        if skiptgt>pc then      -- ignore end whiles (backward jumps)
--if not isGscan then --(hail mary)
                            detach(skiptgt,pc+3)
--end if
                        end if
                    end if
                end if
                pc += skip
            elsif skip<0 then
                noofitems = s5[pc+1]
                pc += noofitems-skip
            elsif nextop=opFrame then
                -- (skip is deliberately 0 for K_used handling in gvar_scan_nobind)
                pc += 2
            elsif nextop=opRetf
               or nextop=opBadRetf then
                pc += 1
                if pc>length(s5) then
                    if tgt!=-1 then ?9/0 end if -- (ie: where's me label gone?)
if NOLT=0 or bind or lint then
--              if pc!=waspc then
                    ltskip(pc,waspc)
--              end if
end if -- NOLT
                    return 1 -- remainder of routine skipped
                end if
            else -- unknown opcode
                ?9/0
            end if
        end if
    end while
end function


integer lastop

function jend(integer ilen)
--
-- Common code to finish jump processing.
--
--  bcode is index to ccde, ilen is the (il) instruction length.
--
--  if tmpd is non-zero, then tmpv indicates always/never taken.
--      If the jump is never taken, we remove it from the list
--      of items to backpatch & resume on the next instruction.
--      If the jump is always taken we can skip any following
--      unreachable code (see jskip), and if this is in fact all
--      code from the jump to the target we can completely omit
--      (and unlink) the jump as well.
--  otherwise we emit the conditional jump indicated by bcode.
--
-- Called from:
--      opJmp:  tmpd=1, tmpv=1  (ie always taken)   [bcode=0]
--      opJif/Jnot: (all features here used)
--      opJtyp: (ditto, if tmpd=1 we get a dummy bcode of 0)
--      opJcc:  (ditto, note tmpd/tmpv set by SetCC)
--      opJbits: currently tmpd is always 0, could do better.   [DEV]
--      opJlen: currently tmpd is always 0, could do better.
--      opJnotx: [tmpd will/must always be 0]
--
integer k
sequence jcode
--trace(1)
    if tmpd then
        tmpd = 0
        npc = pc
        pc += ilen
        if tmpv then    -- always taken:
            if mergeSet=isOpCode then
if not isGscan then
--              if sched then
--                  -- hmm:
--                  -- a: would be done by opLabel rsn anyways,
--                  -- b: necessary before that return 1/exit main loop,
--                  --    (though o/c we could "then schend() return 1").
--                  schend()
--              end if
if suppressopRetf then
    puts(1,"pilx86.e line 4218 (jend/opRetf)\n")
end if
                emitHex5jmpG(opRetf)                        -- jmp opRetf
end if
                opcode = opRetf
                tgt = -1    -- let jskip fall off end of code
                if jskip() then return 1 end if -- if "", all done --> end while
            else
                if jskip() then
                    opcode = lastop
                    detach(tgt,npc+3)
                else
if not isGscan then
--                  if sched then
--                      -- hmm:
--                      -- a: would be done by opLabel anyways,
--                      -- b: must be done before that merge(), (probably no longer true?)
--                      -- c: opJmp kinda assumes this will happen (I think).
--                      schend()
--                  end if
                    merge(mergeSet,0)
--                  emitHex5s(jump_0) -- jmp xxx (backpatched later)
                    emitHex5j(0)                -- jmp xxx (backpatched later)
                    opcode = opJmp
                    s5[npc+2] = length(x86)
end if
                end if
            end if
        else -- tmpv=0/never taken
            opcode = lastop
            if mergeSet!=isOpCode then
--if not isGscan then -- (hail mary)
                detach(tgt,npc+3)
--end if
            end if
        end if

    else -- not tmpd, must test:

if not isGscan then
        k = ccde[bcode][2]
        jcode = {#0F,k}
        if mergeSet=isOpCode then
--if sched then
--          schend() --??   -- a schedule is prolly fine here (untried):
----            schedule(0,0,0,pV,1,0)  -- oops, cannot set anything after the Retf...
--end if
            emitHex6retg(jcode)                 -- jcc opRetf
        else
--          if sched then
--              if schidx then  --***DEV***??
--                  if s5[tgt]=pc+3 then
--                      if not lastJmp then ?9/0 end if
--                      schend()
--                  else
--                      if lastJmp then ?9/0 end if
--                      schedule(0,0,0,pV,1,0)
--                      -- and build a list for schend()
--                      k = pc+4
--                      s5[k] = schall
--                      schall = k
--                  end if
--              end if -- schidx
--          end if -- sched
-- 15/10/2020 (until)
if mergeSet!=0 then
            merge(mergeSet,0)
end if
            emitHex6j(jcode,0)                  -- jcc xxx (to be backpatched)
            s5[pc+2] = length(x86)
        end if
end if
        pc += ilen
    end if -- tmpd
    return 0    -- (not an opRetf skipping the rest of the code)
end function

--SUG: s5->x86, elsewhere replace s5 with ilcode, delete "s5","code"

sequence opTyp0
--       opTyp0 = {opInt0,0,opAtom0,opSq0,0,0,0,opStr0}
         opTyp0 = {opInt0,0,opAtom0,0,0,0,0,opStr0,0,0,0,opSq0}
--DEV? (newEmit)
--       opTyp0 = {opInt0,0,opAtom0,0,0,0,0,opStr0,0,0,0,opSq0,0,0,opObj0}

    if DEBUG then
        if opTyp0[T_integer]!=opInt0 then ?9/0 end if
        if opTyp0[T_atom]!=opAtom0 then ?9/0 end if
        if opTyp0[T_sequence]!=opSq0 then ?9/0 end if
        if opTyp0[T_string]!=opStr0 then ?9/0 end if
--      if opTyp0[T_object]!=opObj0 then ?9/0 end if
    end if

sequence b1, b2

-- "backup" values from getSrc()/getSrc2(). These are needed for cases
--  such as x = remainder(x,y). We typically perform the following steps:
--      getDest()/getSrc()/getSrc2()
--      calculate result type/range in smin etc [1]
--      storeDest()
--      emit final code.
--  What we must not do is call getSrc() again (there may be a few places
--  where that still happens!) in case the storeDest() has modified src.
--  Nor do we want to emit final code before calculating a result, since
--  that can clobber eg a mov [dest],imm32 simplicification.
--  [1] if you wanna rewrite all of this to setup nmin/nmax etc,
--      rather than clobber smin etc, be my guest (please!).
--
integer blroot
atom bmin, bmax, bmin2, bmax2

with trace
--constant linkFwd = 01 --DEV check docs for opAsm; pc+2 is next not prev..
constant false=(1=0), true=(1=1)

--integer trapme
--      trapme=0

--integer zzcount = 0
global integer x86showmapsymtab = 0

global procedure ilxlate(integer vi)
integer p1, p2, p4, pc3, pc6,
        sib, k, res, idx, def,
        isInit,     -- (actually a chain: 0 is False, anything else, including the -1 terminator, is True)
        onDeclaration, ltype, prev,
        routineNo, first, ltot, invert,
--      rs, rx, 
        tii, noofitems,
        lim, step, isInt,
        flag,
        pcTchk,     -- limit of routine vi's parameter's opTchks
        tmp,
fskip
,thisAsm
,lastAsm,firstAsm
,x86jloc
,jnxt
,tgtAsm
,x86tloc
,joffset
,toffset
,lmask
,gmask      -- used in opFrame/opCall (nb must not be damaged betwixt), and opLabel.
--,ctnr
--,brpt
,bothInit
,mul2
,tvar
,ref
,rep
,pbr        -- pass by reference optimisation (if = 2), used in opFrst
,cmph4,
        wasemitline, -- for opCallOnce & opLabel
        waspc,      -- for opConcatN, opRepe, opMkSq, opJne, opMath, opMovxx
--flippable,
useAndBits,     -- see opRmdr
--doNotXor,     -- see opJcc, opPuts
dbpos,          -- backpatch point for defaulted params (see opTchk)
backpatch,      -- general purpose jump backpatch
backpatch2,     -- ""
backpatch3,     -- ""
flags,          -- (currently only used by opFor2)
lblidx
--newEBP: (replace with prev_ebp @ [ebp+20]...)
--integer banedi    -- when 1, we have an opFrst between opFrame & opCall,
                -- so ban use of edi in opMove/si/ti/bi. (affects
                -- recursive routine calls only)
integer isFresSrc
integer isFresDst
--integer nmax  -- new max, used in opRmdr, opUminus, opAddiii
atom nmax   -- new max, used in opRmdr, opUminus, opAddiii
integer notString   -- used in opConcatN
sequence symk
sequence h5 -- (scratch var for keeping emitHex5s's #ginfo happy) [DEV temp?]
object jcode, gi
--integer opc
integer po
--atom minmax
-- == was gvar_scan:
integer mtype, u
atom nMin, nMax, w
integer iroot, limitreg, stepreg, chkreg

integer stmt,link,tlink,switchable,orcount,
        svar    -- "switch" var

integer wasOptTypeCheck, pDefault

--integer from, fmin, fmax

-- 7/11/10 switch vars and stacks (for nested switch statements):
integer switch_x86loc,      -- location of first jump table entry
        switch_first,       -- value that equates to
        switch_length,      -- doh, length of the jump table    [DEV??]
        switch_boundc,      -- chain of bounds check jumps to backpatch
        switch_var,         -- the control variable
--      switch_min,         -- \ of the control var
--      switch_max,         -- /
        switch_flags,       -- scratch, not preserved between opCtrl etc
        switch_errline,     -- scratch, for error reporting
        switch_stack_idx,   -- index to the stacks
        fallthrew           -- top-level/opening flag (as switch_flags damaged)
sequence switch_x86locs,    -- \
         switch_firsts,     --  \
         switch_lengths,    --   } stacks (for nested switch statements)
         switch_boundcs,    --  /
         switch_vars        -- /
--       switch_mins,
--       switch_maxs

sequence casesfound,        -- error checking (when processing an if/switch stmt)
         caseslines

integer switch_duplicate_found,
        switch_duplicate_line,
        switch_duplicate_value,
        swecode             -- error code (1..1 to indicate which swecode = nn line triggered)

integer swroot
atom swmin, swmax   -- scratch vars, not preserved between opCtrls

integer raoffset

integer Tsmin,Tsmax     -- temp/test [DEV]
string opName

object dbg
    --  dbg = symtab[vi]
    --  dbg = dbg[S_FPno]
    --  dbg = filenames[dbg]&{0,0}
    --  k = dbg[1]
    --  dbg[1] = filepaths[k]
    ltDiagMsg(sprintf("ilxlate(%d)\n",vi))
--if showfileprogress then
--  printf(1,"ilxlate(%d), %s\n",{vi,filenames[symtab[vi][S_FPno]][2]})
--end if

    switch_stack_idx = -1
    switch_x86locs = {}
    switch_firsts  = {}
    switch_lengths = {}
    switch_boundcs = {}
    switch_vars = {}


if NOLT=0 or bind or lint then
    ltclear(vi)
end if -- NOLT

if q86 then
    -- for linking up isAddr/isJmp(/isDead):
    q86first = 0
    q86last = 0
    if q86>1 then
        q86f2 = 0
        q86l2 = 0
    end if
end if

    currRtn = vi    -- DEV (temp)

    if not isGscan then
        currRtn = vi    -- for lineinfo()

        x86 = {}

        reginfo = 0
        oplnlen = 0
        reg = 0
        idx = 0
        k = 0
--if linkFwd then
        lastAsm = 0
        firstAsm = 0
--else
--      thisAsm = 0
--end if

        symtabN = symtab[vi]
        flag = symtabN[S_State]

        thisDbg = and_bits(flag,K_wdb)  -- for lineinfo()

        -- known routine_id target [as used in needstypecheck()]
        isKridt = and_bits(flag,K_ridt)

--trace(1)
        lastline = -1
        emitline = symtabN[S_1stl]
        pfileno = symtabN[S_FPno]   -- for lineinfo()
        opLnv = 0
--TEMP:
else pfileno = symtab[vi][S_FPno]
    end if

    -- Check for parameter opTchk at the start:
    --  When processing opTchk, if pc<pcTchk then it is a parameter
    --  typecheck (of routine vi), and the type routine must always
    --  be called if the routine could be a target of routine_id, 
    --  otherwise rely on gtype - as we do for any other opTchk 
    --  after opAdd, opMove, opRepe, etc.

    pcTchk = 1
    while 1 do
        opcode = s5[pcTchk]
        if opcode=opTchk then
            pcTchk += 4
        else
            if not find(opcode,{opLn,opLnp,opLnpt,opLnt}) then exit end if
            pcTchk += 2
        end if
    end while

    opcode = 0
--  opc = 0
    pc = 1
    while 1 do
--if trapme then
--  dbg = x86[46]
--  if dbg=50 then
--      trace(1)
--  end if
--end if
--if tmpd and pc>=tmppc then
--  ttrap = 1
--end if
--trace(1)
        lastop = opcode
        opcode = s5[pc]

--opset = opsets[opcode]
if showOpCounts then
    opcount[opcode] += 1
end if

        if opcode=opLn                  -- 177
        or opcode=opLnt                 -- 6
        or opcode=opLnp                 -- 7
        or opcode=opLnpt then           -- 8
            --if s5[pc+1]=21 then trace(1) end if
            if not isGscan then
                --  if emitline=108 then trace(1) end if
                --          lineinfo()  -- now deferred
                if opcode=opLn then
                    opLnv = 0
                    opcode = lastop
                else
--                  if sched then
--                      if schidx then
--                  -- don't call lineinfo via here this:
----                    wasemitline = emitline  -- not necessary
--                          emitline = lastline
--                          schend()
----                    emitline = wasemitline  -- not necessary
--                      end if
--                  end if
                    opLnv = opcode
                    -- all regs will get trashed before they could be used:
                    reginfo = 0
                end if
                oplnlen = length(x86)
            end if
            emitline = s5[pc+1]
            pc += 2
--if not isGscan then
--  if emitline=4 then trace(1) end if
--end if

--            opcode=opMove             -- 1
--            opcode=opMovbi            -- 2
--            opcode=opMovsi            -- 3
--            opcode=opMovti            -- 4
--      elsif opcode<=opMovti then
        elsif opcode=opMove
           or opcode=opMovbi
           or opcode=opMovsi
           or opcode=opMovti then
--if not isGscan then
--  if vi!=21 then trace(1) end if
--end if
            waspc = pc
            dest = s5[pc+1]
            src = s5[pc+2]
            isInit = s5[pc+3]           -- (src)
            getDest()
            if src=-1 then
                -- from makeBool: a dummy opMovbi to set dest to 0/1.
                if opcode!=opMovbi then ?9/0 end if
                -- opMovbi,dest,src,isInit
--30/10/10: breaks t46; the problem is that code from makeBool sets
--          dest to 0..0, then 1..1, and leaves it like that. What we 
--          need is a new opcode, opBool, like opMovbi but linked up
--          like opCtrl, with start/one/zero/finish flags, and a
--          separate "done" flag. start just clears the "done" flags,
--          one/zero set it if not skipped, finish checks for both
--          and if needed does this.)
--if dmin<0 or dmin>1 or dmax<0 or dmax>1 then
                smin = 0
                smax = 1
                sltype = T_integer
                slroot = T_integer
                pc += 4
                storeDest()
--else
--              pc += 4
--end if
            else
--              dest = -1   -- avoid any mimicry
                if atom(symtab[src]) then
                    src = forwardretarget[find(src+1,forwardingtable)]-1
                    s5[pc+2] = src
                end if
--if dest=808 then
--?{dest,src,isInit,dtype,slroot,sltype,opcode,opMove}
--  trace(1)
--end if
                getSrc()
--if dest=391 then trace(1) end if
--symk = symtab[src]
--if isGscan then
                if opcode=opMove then
--?symtab[src]
                    -- opMove,dest,src,isInit,onDeclaration,ltype:
--16/10/10:
                    if slroot=T_integer and isInit then
--                  if slroot=T_integer then
--DEV tryme:
--  if vroot=T_integer then
--                      s5[pc] = opMovbi
--                      s5[pc+4] = opNop
--  else
                        s5[pc] = opMovsi
--  end if
                        s5[pc+5] = opNop            -- (ltype)
                    elsif vroot=T_integer then
--DEV if stype!=T_integer then stype=9/0 end if, or use stype as mod below?
--DEV surely this should be opMovbi??!
                        s5[pc] = opMovti
--17/3:                 s5[pc+3] = opNop            -- (isInit) [== opMovbi]
                        s5[pc+4] = opNop            -- (onDec)  [== opMovbi]
                        s5[pc+5] = opNop            -- (ltype)
                    end if
                    pc += 6
                else
                    if opcode!=opMovti then -- opcode selected due to non-init state of dest
                        --                  dtype = or_bits(dtype,T_integer)
                        --31/7/09: (replaced above line with:)
                        --                  if rootType(stype)!=T_integer then
                        --                      stype = T_integer
                        --                  end if
                        slroot = T_integer
                    end if
                    --              if opcode=opMovsi and ltype=T_integer then
                    --DEV test:
                    --              if opcode=opMovsi and symtab[dest][S_vtype]=T_integer then
                    --              if opcode=opMovsi and dtype=T_integer then
                    --              if opcode=opMovsi and vroot=T_integer then
                    --                  s5[pc-1] = opNop    -- DEV or always append onDeclaration to opMovbi?
                    --                  s5[pc-5] = opMovbi
                    --                  s5[pc] = opMovbi
                    if opcode=opMovsi then
    --DEV is this not just vroot=T_integer???
                        src2 = dest
    --                  dest = -1   -- avoid any mimicry
                        getSrc2()
    --                  dest = src2
                        if slroot2=T_integer then
                            s5[pc] = opMovbi
                            s5[pc+4] = opNop
                        end if
                        -- opMovsi,dest,src,isInit,onDeclaration
                        pc += 5
                    else
                        -- opMovti,dest,src,isInit
                        -- opMovbi,dest,src,isInit
                        pc += 4
                    end if
                end if
                --else
                --      if opcode=opMovbi
                --      or opcode=opMovsi then
                ----            dtype = or_bits(dtype,T_integer)
                ----31/7/09: (replaced above line with:)
                --          stype = T_integer
                --      end if
                --end if
--31/12/15 (for suppressopRetf)
--              while 1 do
                while pc<=length(s5) do
                    k = s5[pc]
                    if k!=opNop then exit end if
                    pc += 1
                end while

-- 2/4/10: (removed 13/4/10, failed in t46, #istype{R2,tj})
--         (and as usual I can find NOT ONE SINGLE CLUE to why I added this... Grr at myself)
--sltype = slroot

-- 16/10/10:
--              isInit = s5[waspc+3]            -- (src)
--              if not isInit then
--                  sltype = T_object
--                  slroot = T_object
--              end if

--30/12/14: (avoid constant propagation of K_rtn values, as we cannot later map them)
                if and_bits(state1,K_rtn) then
                    smin = T_const1+1
                    smax = length(symtab)
                end if

--?symtab[dest]
--trace(1)
                storeDest()
                if not isGscan then
--                  isInit = s5[waspc+3]            -- (src)
                    isFresSrc = and_bits(symtab[src][S_State],K_Fres)
                    isFresDst = and_bits(symtab[dest][S_State],K_Fres)
                    if opcode=opMove then       -- 1
                        onDeclaration = s5[waspc+4]
                        --DEV 3/8/9: use localtype instead!
                        --              ltype = s5[waspc+5]
                        ltype = slroot -- as ordered, 25/9/9...
                        markConstUseds({src,dest})
                        if isFresSrc then
                            if not isFresDst then
                                if vroot!=T_integer and not onDeclaration then
                                    emitHex1(push_eax)                              -- save eax/rax (src)
--DEV prefer edx...
                                    prev = loadReg(dest)                            -- mov prev,[dest]
                                    cmp_h4(prev)                                    -- cmp prev,h4
                                    emitHex6j(jle_rel32,0)                          -- jl @f [sj NOT ok]
                                    backpatch = length(x86)
                                    sib = #83+prev*8 -- 0o2r3, ebx+prev*4
                                    if X64 then
                                        if prev>7 then ?9/0 end if -- (placeholder for more code)
                                        emitHex1(#48)
                                        emitHex5sib(subd_sibd8i8,sib,-16,1)         -- sub dword[ebx+prev*4-16],1 (decref)
                                    else
                                        emitHex5sib(subd_sibd8i8,sib,-8,1)          -- sub dword[ebx+prev*4-8],1 (decref)
                                    end if
                                    emitHex6j(jnz_rel32,0)                          -- jnz @f [sj NOT ok]
                                    backpatch2 = length(x86)
--DEV if prev!=edx? (just checked, it cannot be, as things stand, it seems)
                                    xrm = #D0+prev -- 0o32p
                                    emitHexx2(mov_dword,xrm)                        -- mov edx,prev
                                    emitHex5callG(opDealloc)                        -- call :%pDealloc
                                    x86[backpatch] = length(x86)-backpatch          -- @@:
                                    x86[backpatch2] = length(x86)-backpatch2
                                    popvar(dest)
                                    reginfo = 0  -- dealloc trashes all registers
                                else
                                    storeReg(eax,dest,1,0)
                                end if
                            end if
--10/11/15:
--                      elsif symtab[src][S_NTyp]=S_TVar
--                      and (isFresDst or onDeclaration=2 or symtab[src][S_Name]=-1) then
                        elsif (symtab[src][S_Name]=-1 and symtab[src][S_NTyp]=S_GVar2)
                           or (symtab[src][S_NTyp]=S_TVar and (isFresDst or onDeclaration=2 or symtab[src][S_Name]=-1)) then
-- bad idea!
--                      elsif isFresDst
--                         or (symtab[src][S_NTyp]=S_TVar and 
--                             (onDeclaration=2 or symtab[src][S_Name]=-1)) then
                            -- dest:=src, src=0, ie no incref
--if newEmit and X64 then ?9/0 end if
                            if isFresDst then -- no decref/dealloc, but incref maybe:
                                loadToReg(eax,src)
                                storeReg(ebx,src,1,0)                           -- move [src],ebx(0)
--10/11/15:
                            elsif onDeclaration then -- no need to decref/dealloc
--                          elsif onDeclaration -- no need to decref/dealloc
--                             or symtab[src][S_Name]=-1 then
                                reg = loadReg(src)                              -- mov reg,[src]
--                              if sched then
--                                  schedule(0,0,0,pUV,0,src)
--                              end if
if onDeclaration=2 then
    -- pbr optimisation:
    -- (note that [src] (now in reg) may possibly be uninitialised here, 
    --                  but the callee/opCode has to deal with that)
                                if X64 then ?9/0 end if -- (placeholder for more code)
                                storeImm(src,#40000000)                         -- mov [src],h4
else
                                zero(src)                                       -- mov [src],ebx(0)
end if
                                if reginfo then clearMem(src) end if
                                storeReg(reg,dest,1,0)                          -- mov [dest],reg
                            else
--                              if sched then
--                                  sch00n = schoon
--                              end if
                                vroot = vtype
                                if vroot>T_object then vroot = rootType(vtype) end if
                                reg = loadReg(src)                              -- mov reg,[src]    (nb reg!=edx)
    --DEV set a flag to load into edx? (untried)
    --  load_to_edx = 1
--15/6/10:
if vroot!=T_integer then
--DEV prefer edx
                                prev = loadReg(dest)                            -- mov prev,[dest]
end if
--                              if sched then
--                                  schedule(0,0,0,pUV,0,src)
--                              end if
                                zero(src)                                       -- mov [src],ebx(0)
                                if vroot=T_integer then
                                    storeReg(reg,dest,1,0)                      -- mov [dest],reg
                                else
--                                  if sched then
--                                      schedule(regbit[prev+1],0,0,pUV,1,0)
--                                  end if
                                    cmp_h4(prev)                                    -- cmp prev,h4
                                    storeReg(reg,dest,1,0)                          -- mov [dest],reg
--                                  if sched then
--                                      schend()
--                                  end if
                                    emitHex6j(jle_rel32,0)                          -- jle @f [sj NOT ok]
                                    backpatch = length(x86)
                                    sib = #83+prev*8 -- 0o2r3, ebx+prev*4
-- (should not be possible to get exception on this statement, though note pbr above)
if newEmit then
                                    if X64 then
                                        emitHex1(#48)
                                        if prev>7 then ?9/0 end if  -- (placeholder for more code)
                                        emitHex5sib(subd_sibd8i8,sib,-16,1)         -- sub qword[rbx+prev*4-16],1 (decref prev)
                                    else
                                        emitHex5sib(subd_sibd8i8,sib,-8,1)          -- sub dword[ebx+prev*4-8],1 (decref prev)
                                    end if
else
                                    emitHex4sib(decd_sib,sib,-8)                    -- dec dword[ebx+prev*4-8]  ; decref prev
end if
                                    emitHex6j(jnz_rel32,0)                          -- jnz @f [sj NOT ok]
                                    backpatch2 = length(x86)
--DEV use mov_reg?/if not already edx
                                    xrm = #D0+prev -- 0o32p
                                    emitHexx2(mov_dword,xrm)                        -- mov edx,prev
                                    emitHex5callG(opDealloc)                        -- call :%pDealloc
                                    x86[backpatch] = length(x86)-backpatch          -- @@:
                                    x86[backpatch2] = length(x86)-backpatch2
                                    reginfo = 0  -- dealloc trashes all registers
                                end if -- vroot=T_integer
                            end if

--                      elsif onDeclaration then    -- no decref/dealloc, but incref maybe:
                        elsif onDeclaration or isFresDst then   -- no decref/dealloc, but incref maybe:
--11/01/10:
-- (failed because edx gets damaged between opFrame and opCall...)
-- (btw, was trying to get better error messages on unassigned vars)
--                      elsif onDeclaration         -- no decref/dealloc, but incref maybe:
--                        and (isInit or ssNTyp1=S_Const) then
--DEV added 18/10/09:
-- 3/10/10 (breaks p t04 and p p)
--if bind and ssNTyp1=S_Const and and_bits(state1,K_noclr) then
--if newEmit and X64 then ?9/0 end if
if not newEmit and ssNTyp1=S_Const and and_bits(state1,K_noclr) then
                            -- (we should not get here (opMove) if src is
                            --  an integer, instead of opMovsi/opMovbi:)
                            if slroot=T_integer then ?9/0 end if
--                          if sched then
--                              schedule(0,0,0,pUV,0,dest)
--                          end if
                            emitHex6constrefcount(inc_mem32, src)                   -- inc dword[#xxxxxxxx]
                            if isFresDst then
                                emitHex5constref(mov_eax_imm32,src)                 -- mov eax,#xxxxxxxx
-- 18/1/2013:
--                          elsif symtab[dest][S_NTyp]=S_TVar then
                            else
                                if symtab[dest][S_NTyp]=S_TVar then
--DEV storeconstref(dest,src)?
                                    k = symtab[dest][S_Tidx]
                                    if X64 then
                                        k *=8
                                    else
                                        k *=4
                                    end if
                                    if k<-128 then
                                        emitHex10wconstref(mov_ebpd32_i32,k,src)        -- mov [ebp-nnnn],#xxxxxxxx
                                    else -- (when k is 0 we still need a byte offset of 0)
                                        emitHex7d8constref(mov_ebpd8_i32,k,src)         -- mov [ebp-nn],#xxxxxxxx
                                    end if
                                else
                                    emitHex10constref(mov_m32_imm32, dest, src)         -- mov [dest],#xxxxxxxx
                                end if
                                clearMem(dest)
                            end if
--DEV try instead:
--mov edx,[rc]
--mov [dest],ref
--add edx,1
--mov [rc],edx

--symtab[389]:{-1,S_Const,1,(S_used+S_set+K_sqr+K_noclr+K_lit),0,191/#0040D2F8,T_Dsq,{T_Dsq,0,0,integer,2},{1,2}}
--  mov esi,[#0040D2F8] (symtab[389])     ;#0040C014: 213065 F8D24000            uv 40 00  1   3
--  mov [#0040D14C] (op2),esi             ;#0040C01A: 211065 4CD14000            uv 00 40  1   4 40
--  inc dword [ebx+esi*4-8]               ;#0040C020: 377104263 F8               uv 00 48  3   6    *40*

--      state1 = ss[S_State]
--      ssNTyp1 = ss[S_NTyp]    -- for opMovbi (and not NewBase)
--elsif isFresDst then
--                          loadToReg(eax,src)                                      -- mov eax,[src]
else -- K_noclr S_Const
  if isFresDst then
                            loadToReg(eax,src)                                      -- mov eax,[src]
                            reg = eax
  else
                            reg = loadReg(src)                                      -- mov reg,[src]
                            storeReg(reg,dest,1,0)                                  -- mov [dest],reg
  end if
                            cmph4 = 1   -- assume we need a cmp h4...
                            if isInit then
                                -- init seq/str; just incref w/o h4 check
                                cmph4 = 0
                                if and_bits(ltype,T_atom) then
                                    if ltype!=T_N   -- unless "init const atom"
--DEV use ssNTyp1 here? (untried)
                                    or symtab[src][S_NTyp]!=S_Const then
                                        cmph4 = 1   -- signal we need a cmp h4 then.
                                    end if
                                    --
                                    -- Perhaps "init const atom" deserves a wee explanation:
                                    -- 1) See DoConstant; [S_Init] is left zero if a StoreVar is involved.
                                    -- 2) The type of a constant is deduced from the RHS; so atom const with
                                    --      [S_Init]=1 really means an atom, like 3.1415926, as opposed to
                                    --      an atom var, or indeed an [S_Init]=0 atom const, which can hold
                                    --      either an integer or a float.
                                    -- 3) Of course we are not "increfing a const pointlessly" - we are
                                    --      increfing a var that happens to be taking a copy of a const and
                                    --      at some later time a decref of that var should not try to free
                                    --      the const - and clearly it is always faster to just decref a
                                    --      var than it is to "if not a copy of a const then decref" it.
                                    --
                                end if
                            end if
--                          if sched then
--                              schedule(regbit[reg+1],0,0,pNP,1,0) -- treat next 3 as one big instruction
--                          end if
    --DEV if not cmph4, this could/should be split (and scheduled):
--
-- NB we /CAN/ get an exception on the incd_sib below, for unassigned vars. Of course that
--  only occurs when isInit is zero. Rather than try to decode variable(length) assembly
--  backwards(!!), emit a helper instruction just in case (pdiag.e can and should verify
--  the cmp_eax_imm32 and check that src is indeed unassigned before issuing the error.
--  As shown, we could help pdiag.e a bit by emitting a var no rather than a var address,
--  but firstly pdiag.e is capable of that conversion anyway, and secondly doing so would
--  spanner any efforts to pack/renumber things in pemit.e. Lastly, it remains 3 clocks
--  with or without the cmp.):
--
                            if cmph4 then
                                cmp_h4(reg)                             -- cmp reg,h4
--DEVBPM backpatch me: [DONE]
--                              if not isInit then
--                                  emitHex6j(jl_rel32,9)               -- jl @f [sj NOT ok]
--                              else
--                                  emitHex6j(jl_rel32,4)               -- jl @f [sj NOT ok]
--                              end if
                                emitHex6j(jl_rel32,0)                   -- jl @f [sj NOT ok]
                                backpatch = length(x86)
                            end if
                            sib = 0o203+reg*8 -- 0o2r3, ebx+reg*4
if newEmit then
                            if X64 then
                                if reg>7 then ?9/0 end if -- (placeholder for more code)
                                emitHex1(#48)
                                emitHex5sib(addd_subd8i8,sib,-16,1)     -- add qword[rbx+reg*4-16],1    ; incref
                            else
                                emitHex5sib(addd_subd8i8,sib,-8,1)      -- add dword[ebx+reg*4-8],1     ; incref
                            end if
else
                            emitHex4sib(incd_sib,sib,-8)                -- inc dword[ebx+reg*4-8]       ; incref
end if
                            if not isInit then
if newEmit then
                                cmp_eax_srcid(src)                      -- cmp eax,src_id   (effectively a no-op)
else
                                emitHex5w(cmp_eax_imm32,src)            -- cmp eax,src_id   (effectively a no-op)
end if
                            end if
                            if cmph4 then
                                x86[backpatch] = length(x86)-backpatch  -- @@:
                            end if
end if -- K_noclr S_Const
                        else    -- a more normal opMove, with incref/decref/dealloc etc:
                            -- (not onDeclaration, not from ParamList)
                            prev = loadReg(dest,NOLOAD)                 -- (if already loaded, promote)
                            reg = loadReg(src)                          -- mov reg,[src]
                            if prev=-1 then -- not already loaded
                                prev = edx
                                loadMem(edx,dest)                       -- mov edx,[dest]
                            end if
--                          if cmph4 then?
                            cmp_h4(reg)                                 -- cmp reg,h4
                            emitHex6j(jl_rel32,0)                       -- jl @f [sj ok]
                            backpatch = length(x86)
                            sib = 0o203+reg*8 -- 0o2r3, ebx+reg*4
                            if X64 then
                                if reg>7 then ?9/0 end if -- (placeholder for more code)
                                emitHex1(#48)
                                emitHex5sib(addd_subd8i8,sib,-16,1)     -- add qword[rbx+reg*4-16],1    ; incref
                            else
                                emitHex5sib(addd_subd8i8,sib,-8,1)      -- add dword[ebx+reg*4-8],1     ; incref
                            end if
                            if not isInit then
if newEmit then
                                cmp_eax_srcid(src)                      -- cmp eax,src_id   (effectively a no-op)
else
                                emitHex5w(cmp_eax_imm32,src)            -- cmp eax,src_id   (effectively a no-op)
end if
                            end if
                            x86[backpatch] = length(x86)-backpatch      -- @@:
                            storeReg(reg,dest,1,0)                      -- mov [dest],reg
--                          if cmph4 then?
                            cmp_h4(prev)                                -- cmp prev,h4
                            emitHex6j(jle_rel32,0)                      -- jle @f [sj NOT ok]
                            backpatch = length(x86)
                            sib = 0o203+prev*8 -- 0o2p3
                            if X64 then
                                if prev>7 then ?9/0 end if -- (placeholder for more code)
                                emitHex1(#48)
                                emitHex5sib(subd_sibd8i8,sib,-16,1)     -- sub dword[ebx+prev*4-16],1 (decref)
                            else
                                emitHex5sib(subd_sibd8i8,sib,-8,1)      -- sub dword[ebx+prev*4-8],1 (decref)
                            end if
                            emitHex6j(jnz_rel32,0)                      -- jnz @f [sj NOT ok]
                            backpatch2 = length(x86)
                            if prev!=edx then
                                xrm = #C2 + prev*8 -- 0o3r2
                                emitHexx2(mov_reg,xrm)                  -- mov edx,prev
                            end if
                            emitHex5callG(opDealloc)                    -- call :%pDealloc
                            x86[backpatch] = length(x86)-backpatch      -- @@:
                            x86[backpatch2] = length(x86)-backpatch2
--;newEBP:: (not yet attempted) (extend this with builtin typechecking? - no, see opMovti)
--; (obvs: use any regs here)
--; (sug: drop variants, have localtypes in the il?)
--;  mov src,[p2]       ; source                    (any, TVars are [ebp-n])
--;  mov dst,[p1]       ; target (optional,edx ok)      ("")
--;  cmp src,h4         ; (optional)                (cmp_h4(src))
--;  jl @f
--;   add dword[ebx+src*4-8],1          2031042s3 F8 01          u  00 09  3 2321      
--;   cmp eax,p2        ; <var no>      075 imm32
--; @@:
--;  mov [p1],src       ; blat target
--;  cmp dst,h4         ; (optional)
--;  jle @f                                         176 005     7E 05           jle @f
--;  dec dword[ebx+dst*4-8]
--;  jnz @f
--;   mov edx,dst
--;   call deallocX                                 350         E8 rel32        call rel32
--;  (test using a pushad/popad here to preserve registers)
--;  (no need to "trash" eax either, btw)
--; @@:
--
-- or go with the older one:
--;opMove:
--;------
--;calling convention:                              octal:         binary:          code:
--; mov src,[src]       ; source                    241         A1 m32          mov eax,[m32]
--; mov ?edx,[dst]      ; prev ref                  213 025     8B 15 m32       mov edx,[m32]
--; cmp src,h4
--; jl @f
--;     add dword[ebx+src*4-8],1    ; incref (exception here mapped to e92)
--      if not isInit then
--if newEmit then
--          cmp_eax_srcid(src)                      -- cmp eax,src_id   (effectively a no-op)
--else
--          emitHex5w(cmp_eax_imm32,src)            -- cmp eax,src_id   (effectively a no-op)
--endif
--      end if
--;  @@:
--; mov [dst],eax
--; cmp ?edx,h4
--; jle @f
--;     sub dword[ebx+?edx*4-8],1   ; decref
--;     jnz @f
--;     mov edx,dst (optional)
--;     call deallocX
--;  @@:
--; ret
--
                            reginfo = 0 -- trashes all registers
                        end if

                    elsif opcode=opMovbi then       -- both integer
                        getSrc()

                        -- NB must not damage edx (when onDeclaration is 1), see opFrame/opCall

                        if (isInit or ssNTyp1=S_Const) and slroot=T_integer and smin=smax then
if newEmit and and_bits(state1,K_rtn) then
--  if isFresDst then ?9/0 end if -- placeholder for more code
    --emitHex5vno(integer op1, integer N)
--  ?9/0
--printf(1,"checkme: storeVno, line 5219 pilx86.e, (emitline=%d, %s)\n",{emitline,filenames[symtab[vi][S_FPno]][2]})
                            -- Aside: for K_rtn values, we use the original symtab index for constant propagation,
                            --        etc. but isVno at the last moment to trigger symtabmapping. I hope that's OK!
                            storeVno(dest,smin)
else
                            storeImm(dest,smin)                                 -- mov [dest],imm32
end if
                            if reginfo then clearMem(dest) end if
                        else
                            if isFresDst then
                                if isFresSrc then
                                    reg = eax
                                else
--DEV LoadToReg?
                                    reg = loadReg(src,NOLOAD)                   -- is [src] in a reg?
                                    if reg=-1 then
                                        demoteReg(eax)
                                        reg = loadReg(src)                      -- mov eax,[src]
                                    elsif reg!=eax then
--DEV
                                        xrm = #C0 + reg*8 -- 0o3r0
                                        emitHexx2(mov_reg,xrm)                  -- mov eax,reg
                                    end if
                                end if
                            else
                                if isFresSrc then
                                    reg = eax
                                else
                                    reg = loadReg(src)
                                end if
                                storeReg(reg,dest,1,0)                          -- mov [dest],reg
                            end if
                        end if

                    elsif opcode=opMovsi then       -- source is integer
                        onDeclaration = s5[waspc+4]
                        markConstUseds({src,dest})
                        if not onDeclaration 
                        and not isFresDst then
                            -- need to decref/dealloc:
--                          if sched then
--                              sch00n = schoon
--                              schedule(0,0,edxbit,pUV,0,dest)
--                          end if
                            loadMem(edx,dest)
                        end if
                        getSrc()
                        if isFresDst then -- no decref/dealloc:
                            if slroot=T_integer and smin=smax then
if and_bits(state1,K_rtn) then ?9/0 end if
--                              emitHex5w(mov_eax_imm32,smin)                   -- mov eax,smin
                                movRegImm32(eax,smin)                           -- mov eax,smin
                            elsif not isFresSrc then
--DEV LoadToReg?
                                reg = loadReg(src,NOLOAD)
                                if reg=-1 then -- not already loaded
                                    demoteReg(eax)
                                    reg = loadReg(src)
                                elsif reg!=eax then
--DEV
                                    xrm = #C0 + reg*8 -- 0o3r0
                                    emitHexx2(mov_reg,xrm)                      -- mov eax,reg
                                end if
--                              storeReg(ebx,src,1,0)
                            end if
                        else
                            if slroot!=T_integer or smin!=smax then
                                if isFresSrc then
                                    reg = eax
                                else
                                    reg = loadReg(src)                          -- mov reg,[src]
                                end if
                            end if
                            if not onDeclaration then
--                              if sched then
--                                  schedule(edxbit,0,0,pUV,1,0)
--                              end if
                                cmp_h4(edx)                                     -- cmp edx,h4
                            end if
                            if slroot=T_integer and smin=smax then
if newEmit and and_bits(state1,K_rtn) then
                                storeVno(dest,smin)
else
                                storeImm(dest,smin)                             -- mov [dest],imm32
end if
                                if reginfo then clearMem(dest) end if
                            else
                                storeReg(reg,dest,1,0)                          -- mov [dest],reg
                            end if
                            if not onDeclaration then
--                              if sched then
--                                  schend()
--                              end if
--DEVBPM backpatch me: [DONE]
--                              emitHex6j(jle_rel32,15)                         -- jle @f [sj NOT ok]
                                emitHex6j(jle_rel32,0)                          -- jle @f [sj NOT ok]
                                backpatch = length(x86)
if newEmit then
                                if X64 then
                                    emitHex1(#48)
                                    emitHex5sib(subd_sibd8i8,ebx_edx4,-16,1)    -- sub qword[rbx+rdx*4-16],1 (decref prev)
                                else
                                    emitHex5sib(subd_sibd8i8,ebx_edx4,-8,1)     -- sub dword[ebx+edx*4-8],1 (decref prev)
                                end if
else
                                emitHex4sib(decd_sib,ebx_edx4,-8)               -- dec dword[ebx+edx*4-8]   ; decref prev
end if
--DEVBPM backpatch me: [DONE]
--                              emitHex6j(jnz_rel32,5)                          -- jnz @f [sj NOT ok]
                                emitHex6j(jnz_rel32,0)                          -- jnz @f [sj NOT ok]
                                backpatch2 = length(x86)
                                emitHex5callG(opDealloc)                        -- call :%pDealloc
                                x86[backpatch] = length(x86)-backpatch          -- @@:
                                x86[backpatch2] = length(x86)-backpatch2
                                reginfo = 0 -- trashes all registers
                            end if -- onDeclaration
                        end if -- isFresDst

                    elsif opcode=opMovti then       -- target is integer
                        getDest()
                        if isFresSrc then
                            reg = eax
                        else
--10/01/2012:
-- (DEV this might be masking a deeper problem:
--      why would return <int> become opMovti not opMovbi?)
--                          reg = loadReg(src)                      -- mov reg,[src]
                            if isFresDst then
--DEV LoadToReg(
                                reg = loadReg(src,NOLOAD)
                                if reg=-1 then -- not already loaded
                                    demoteReg(eax)
                                    reg = loadReg(src)
                                elsif reg!=eax then
--DEV do we need to clearReg(eax) or something here?
                                    xrm = #C0 + reg*8 -- 0o3r0
                                    emitHexx2(mov_reg,xrm)          -- mov eax,reg
                                end if
                                -- (erm, this might be unnecessary)
-- 28/2/2012 seems so!
--                              storeReg(ebx,src,1,0)               -- move [src],ebx(0)
                            else
                                reg = loadReg(src)                  -- mov reg,[src]
                            end if
                        end if

                        if not isFresDst then
                            storeReg(reg,dest,1,0)                  -- mov [dest],reg
                        end if
--25/2/2013:
----DEV dtype?! (spotted in passing)
--                      if dtype!=T_integer or not isInit then
                        if slroot!=T_integer or not isInit then
--                          if sched then
--                              schedule(regbit[reg+1],0,0,pUV,1,0)
--                          end if
                            cmp_h4(reg)                             -- cmp reg,h4
--                          if sched then
--                              schedule(0,0,0,pV,1,0)  -- treat next 4 as one instruction
--                          end if
-- 30/1/15: for callpending, we just want unassigned on the call; typecheck error we can leave until the routine is called...
    if callpending then
                            emitHex6j(jne_rel32,0)                  -- jne @f [sj NOT ok]
--                          backpatch = length(x86)
--  ?9/0 (note: there may yet be edge cases where agcheckop(opUnassigned) never got called... esp that dodgy opMovti that should prolly be opMovbi above...)
--                          h5 = {push_imm32,isILa,0,0,routineNo}   -- (keeps that ginfo happy) [DEV temp?]
--                          emitHex5s(h5)                           -- push <routine_code>
--DEV might want an add [esp],1 here... (but w/o upsetting the jz?, so pop edx lea edx,[edx+1] push edx?? or isILam1? or opUnassigned0?)
--                          emitHex5jmpG(opUnassigned)
--                          movRegVno(esi,src)                      -- mov esi,src (var no for unassigned)
--                          movRegVno(edi,dest)                     -- mov edi,dest (var no for type check error)
--                          emitHex5callG(opUnassigned)
--printf(1,"checkme: dodgy use of opUnassigned, line 5436 pilx86.e, (emitline=%d, %s)\n",{emitline,filenames[symtab[vi][S_FPno]][2]})
    else
                            emitHex6j(jl_rel32,0)                   -- jl @f [sj NOT ok]
    end if
                            backpatch = length(x86)
                            movRegVno(esi,src)                      -- mov esi,src (var no for unassigned)
                            movRegVno(edi,dest)                     -- mov edi,dest (var no for type check error)
                            emitHex5callG(opUnassigned)
                            x86[backpatch] = length(x86)-backpatch
                            -- e92movti is unassigned [esi] or type check [edx], does not return

                            -- <aside>: we don't really need to typecheck here from ParamList, but
        --BLUFF!
                        --          we do need(/ought) to check for unassigned, so we may as well.
                        --          This means Phix may typecheck on the call statement,
                        --          whereas RDS Eu always leaves it up to the routine.</aside>
                        end if
                    end if -- opMove/opMovbi/opMovsi/opMovti
                --          -- skip any opNop here, since opMove et al are a major source of them:
                --          while 1 do
                --              nextop = s5[pc]
                --              if nextop!=opNop then exit end if
                --              pc += 1
                --          end while
                end if -- isGscan
            end if -- src=-1/from makeBool

        elsif opcode=opFrame then       -- 12
            routineNo = s5[pc+1]
-- 14/2/14:
            k = find(routineNo,forwardingtable)
            if k then
                routineNo = forwardretarget[k]
                s5[pc+1] = routineNo
            end if
            symk = symtab[routineNo]
            if DEBUG then
                if callpending then ?9/0 end if
            end if
            callpending = 1
            gmask = symk[S_Efct]    -- (save for the opCall)
            pc += 2
            if isGscan then
if NEWGSCAN then
                if symk[S_NTyp]>=S_Type
                and g_scan[routineNo]==0 then
--if pfileno=1 and symtab[routineNo][S_FPno]=1 then
--?{"pilx86.e line 5587 (opFrame), adding",routineNo,vi}
--end if
                    g_scan[routineNo] = g_scan[vi]
                    g_scan[vi] = routineNo
                end if
else
                u = symk[S_State]
                if not and_bits(u,K_used) then
                    symtab[routineNo] = 0       -- kill refcount (prevent pointless clone)
                    u += K_used
                    symk[S_State] = u
--MARKTYPES
                    if symk[S_NTyp]>S_Type then     -- all S_Type are already on the list; [DEV!!!]
--                  if symk[S_NTyp]>=S_Func-MARKTYPES then
--                  if symk[S_NTyp]>=S_Type then
                                                -- alternatively: linkup opTchks here.
                                                -- (I think you'd find that slower)
                        -- add to chain of routines to be processed:
----DEV temp!
--printf(1,"pilx86.e line 3369: S_linking symtab[%d]\n",vi)
--printf(1,"pilx86.e line 3370: S_linking symtab[%d]\n",routineNo)
                        symk[S_Slink] = symtab[vi][S_Slink]
                        symtab[vi][S_Slink] = routineNo
                    end if
                    symtab[routineNo] = symk
                end if
end if
                -- DEV flag opFrame as being "do nowt" (ie/eg as opLn) before second pass.
            else -- (not isGscan)
                first = symk[S_Parm1]
                ltot = symk[S_Ltot]
                -- avoid the call if result is known and routine has no side effects:
                fskip = 0
                if symk[S_Efct]=E_none then
                    if symk[S_NTyp]=S_Proc then
                        fskip = 1
                    else
                        k = routineNo-1
                        symk = symtab[k]
                        gi = symk[S_gInfo]
                        if sequence(gi)
                        and gi[gType]=T_integer
                        and gi[gMin]=gi[gMax] then
                            fskip = 1
                            -- ensure the function result is setup properly
                            flag = symk[S_State]
                            if not and_bits(flag,K_noclr) then
                                flag += K_noclr
                                symtab[k] = 0               -- (kill refcount to avoid clone)
                                symk[S_State] = flag
                                symk[S_value] = gi[gMin]
                                symtab[k] = symk
                            end if
                        end if
                    end if
                end if
                symk = {}

                if fskip then
                    -- locate the opCall and resume on next statement
                    while 1 do
                        opcode = s5[pc]
                        if opcode=opCall then
                            pc += 1
                            exit
                        end if
                        if opcode=opMove then
                            pc += 6
                        elsif opcode=opMovbi
                           or opcode=opMovti then
                            pc += 4
                        elsif opcode=opMovsi then
                            pc += 5
                        elsif opcode=opNop then     -- (from opMove -> opMovbi etc)
                            pc += 1
                        elsif opcode=opFrst then
                            pc += 5
                        else -- other instructions should not occur between
                             -- opFrame and opCall (see pmain.e for proof!)
                            ?9/0
                        end if
                    end while
                    callpending = 0
                else -- not fksip
--                  if sched then
----                if schidx then schend() end if -- treat next 4 as one big instruction [DEV??]
--                      sch00n = schoon
--                  end if
                    if first>0 then
                        movRegImm32(ecx,ltot)                           -- mov ecx,no of params
                    else
                        if X64 then
                            emitHex1(#48)
                        end if
--                      if sched then
--                          schedule(0,0,ecxbit,pUV,1,0)
--                      end if
                        emitHex2s(xor_ecx_ecx)                          -- xor ecx,ecx
                    end if
--                  if sched then
--                      schedule(0,0,edxbit,pUV,0,0)
--                  end if
                    if newEmit then
                        movRegVno(edx,routineNo)                        -- mov e/rdx,routineNo
                    else
                        emitHex5w(mov_edx_imm32,routineNo)              -- mov edx,routineNo
                    end if
                    -- Warning: under no circumstances emit an opLnt/p/pt between
                    -- now and the opCall, ie emitline/lastline must not be altered
                    -- by the next few instructions lines nor owt they call.
--                  if sched then
--                      schend()
--                  end if
                    emitHex5callG(opFrame)
                    reginfo = 0 -- leaves registers as:
                                -- eax is h4, ebx is 0, ecx is 0, 
                                -- edx is threadstack (==[ebp+28])
                                -- esi is ebp-N*4 or thereabouts,
                                -- edi is vsb_root (==[ebp+32])     -- DEV check this... (vsb_prev?)
                end if  -- fskip
            end if -- isGscan

        elsif opcode=opCall then        -- 165
--  if emitline=31 then trace(1) end if

            if not callpending then ?9/0 end if
            callpending = 0
if NOLT=0 or bind or lint then
            if and_bits(gmask,E_vars) then  -- (gmask set by opFrame)
                ltCall(0,gmask,pc)
            end if
end if -- NOLT
            pc += 1

            if not isGscan then
                -- NB should not be calling lineinfo here
--              if sched then
--              --  schedule(0,0,0,pU,0,0)  -- while some docs claim pU, all
--              --  schedule(0,0,0,pUV,0,0) -- my tests indicate this is pUV
--              --  schedule(0,0,ebpbit+edxbit,pU,0)    --DEV?
---- strictly, this is if schidx or lastline!=emitline... (and elsewhere)???
----            if schidx then schend() end if  -- isAddr not relocatable (yet)
--                  schend() -- isAddr not relocatable (yet?)
--              end if
if X64 then
                emitHex1(#48)
--EXCEPT
--  if NEWRETSLOT then
                emitHex7a(mov_ebpd8_i32,56,5)                       -- mov [rbp+56],<return addr>
--  else
--              emitHex7a(mov_ebpd8_i32,32,5)                       -- mov [rbp+32],<return addr>
--  end if
else
--EXCEPT
--  if NEWRETSLOT then
                emitHex7a(mov_ebpd8_i32,28,5)                       -- mov [ebp+28],<return addr>
--  else
--              emitHex7a(mov_ebpd8_i32,16,5)                       -- mov [ebp+16],<return addr>
--  end if
end if
if q86>1 then
--  ?"9/0 (x86 3861)\n" -- separate chain for isIL??
            if DBGCALL then
                x86 &= call_rel32
            else
                x86 &= jump_rel32
            end if
                quad2(isIL,routineNo)
else
            if DBGCALL then
                h5 = {call_rel32,isIL,0,0,routineNo}  -- (keeps that ginfo happy) [DEV temp?]
            else
                h5 = {jump_rel32,isIL,0,0,routineNo}  -- (keeps that ginfo happy) [DEV temp?]
            end if
                emitHex5s(h5)                                       -- jmp <routine_code>
end if
            if DBGCALL then
                if X64=0 then
                    emitHex3(add_esp_imm8,4)                        -- add esp,4
                else
                    emitHex3(add_esp_imm8,8)                        -- add esp,8
                end if
            end if
                reginfo = 0 -- trashes all registers
            end if

        elsif opcode=opFrst then        -- 166
            dest = s5[pc+1]
            src = s5[pc+2]
--DEV seems little point in this (ie ltype) anymore...
            ltype = s5[pc+3]    -- source type
            pbr = s5[pc+4]
--if isGscan then
            getSrc()
            getDest()
            if ltype!=slroot then
                if ltype>T_object then ?9/0 end if
                slroot = and_bits(slroot,ltype)
                s5[pc+3] = slroot
            end if
            storeDest()
            --else
            if not isGscan then
if X64 then
                emitHex1(#48)
                emitHex3(mov_edi_ebpd8,40) -- mov rdi,[rbp+40] ; prev_ebp
else
                emitHex3(mov_edi_ebpd8,20) -- mov edi,[ebp+20] ; prev_ebp
end if
                clearReg(edi)
                k = (symtab[src][S_Tidx]-symtab[first][S_Tidx]) -- (-ve) var offset
                if X64=0 then
                    k *= 4
                else
                    k *= 8
                    emitHex1(#48)
                end if
                baseLoad632(eax,edi,k)                          -- mov eax,[edi+imm]
                if ltype!=T_integer then
--26/4/21
--                  if pbr=2 then
--!/!*
                    if (symtab[src][S_Name]=-1 and symtab[src][S_NTyp]=S_GVar2)
--                  or (symtab[src][S_NTyp]=S_TVar and (isFresDst or onDeclaration=2 or symtab[src][S_Name]=-1)) then
                    or (symtab[src][S_NTyp]=S_TVar and (pbr=2 or symtab[src][S_Name]=-1)) then
--!*!/
if X64 then
--done, but [DEV] needs r15 handling in pcfunc.e, cbhandler, etc..
--  printf(1,"pbr=2 incomplete line 5565 in pilx86.e, emitline=%d\n",{emitline})
--/*
(some ideas from cmph4():)
        x86 &= {#49,#BF,0,0,0,0,0,0,0,#40}  -- mov r15,h4   (10 byte ins)
        emitHex1(#4C)
        if reg>7 then ?9/0 end if   -- (placeholder for more code)
        reg = 0o370+reg     
        x86 &= {#39,reg}                        -- cmp reg,r15
or
        storeMem(dest,r15)                          -- mov [dest],eax but with edi instead of ebp (and allow "[edi]")
--procedure storeMem(integer N, integer reg)
integer k, xrm
--  if X64 then
--      emitHex1(#48) -- might want to be #4C?
        emitHex1(#49)
--      if reg>7 then ?9/0 end if   -- (placeholder for more code)
--              if reg>7 then
----                    rex = or_bits(rex,#41)
--                  rex = or_bits(rex,#44)
--                  reg -= 8
--              end if
--              if reg>7 then
--                  rex = or_bits(rex,#41)
----                    rex = or_bits(rex,#44)
--                  reg -= 8
--              end if
--              if reg>7 then
----                    rex = or_bits(rex,#44)
--                  rex = or_bits(rex,#41)
--                  reg -= 8
--              end if

--  end if
--end procedure
--*/
                        emitHex1(#4C)
                        if k<-128 then
                            xrm = 0o277
                            emitHex62w(mov_reg,xrm,k)   -- mov [edi-nnnn],r15
                        elsif k!=0 then
                            xrm = 0o177
                            emitHex3l(mov_reg,xrm,k)    -- mov [edi-nn],r15
                        else
                            xrm = 0o077
                            emitHex2(mov_reg,xrm)       -- mov [edi],r15
                        end if
else
                        if k>127 or k<-128 then
                            emitHex10ww(mov_edid32_i32,k,#40000000) -- mov [edi+d32],h4
                        elsif k then
                            emitHex7d8v(mov_edid8_i32,k,#40000000)  -- mov [edi+d8],h4
                        else
                            emitHex6w(mov_medi_im32,#40000000)      -- mov [edi],h4
                        end if
end if
--9/5/19:
                        cmp_h4(eax)
                        emitHex6j(jne_rel32,0)                      -- jne @f [sj OK]
                        backpatch = length(x86)
                        -- unassigned (fatal runtime error)
--DEV: this could do with an "opRetn", ie pop an opFrame but return here
                        movRegVno(esi,src)                      -- mov esi,src (var no for unassigned)
                        emitHex5callG(opUnassigned)
                        x86[backpatch] = length(x86)-backpatch  -- @@:

                    else
--                      if sched then
--                          schedule(0,0,ebxbit,pUV,1,0)
--                      end if
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(xor_ebx_ebx)                  -- xor ebx,ebx
--DEV if and_bits(dtype[nb not loaded yet],T_integer) then...
--                      if sched then
--                          schedule(eaxbit,0,0,pNP,1,0)    -- treat next 3 as one instruction
--                      end if
                        cmp_h4(eax)                             -- cmp eax,h4
--PL 3/5/3013:
--                      emitHex6j(jl_rel32,4)                   -- jl @f [sj OK]
                        emitHex6j(jl_rel32,0)                   -- jl @f [sj OK]
                        backpatch = length(x86)
if newEmit then
                        sib = ebx_eax4
                        if X64 then
                            emitHex1(#48)
                            emitHex5sib(addd_subd8i8,sib,-16,1) -- add qword[rbx+rax*4-16],1    ; incref
                        else
                            emitHex5sib(addd_subd8i8,sib,-8,1)  -- add dword[ebx+eax*4-8],1     ; incref
                        end if
                        cmp_eax_srcid(src)                      -- cmp eax,src_id   (effectively a no-op)
else
                        emitHex4sib(incd_sib,ebx_eax4,-8)       -- inc dword[ebx+eax*4-8]   ; incref
                        emitHex5w(cmp_eax_imm32,src)            -- cmp eax,src_id   (effectively a no-op)
end if
                        x86[backpatch] = length(x86)-backpatch  -- @@:
                    end if
                end if
                storeReg(eax,dest,1,1)                          -- mov [dest],eax
            end if
            pc += 5

        elsif opcode=opCtrl then    -- opCtrl,stmt,link
            --
            -- stmt is IF/LOOP/ELSIF/ELSE[+END]; maintains a mini-
            -- table of the hll constructs we are currently inside.
            -- links point back up a hll construct, except for the
            -- first, which points at the last.
            --
--if pc=59 then trace(1) end if
--?pc
--DEV 29/3/10:
-- check for switchable constructs:
--
--1) all jumps act on same var
--2) jne the norm (a plain =c).
--      jif is also ok (ie treat "not i then" as "i=0 then")
--3) {jeq}/jne/label allowed (=a or =b or =c).
--      jnot is also ok (ie treat "not i or" as "i=0 or")
--4) rhs must have init/bothInit must be 1 on all jumps
--5) follow the jne to the label, must point directly back at the jne...... << YES!! <<
--6) likewise in 3, label must link back through the {jeq}, and nowhere else.
--7) "all int?" and min/max to be kept separately...
--
-- Add K_jumptable to pglobals/plist.e, and some S_Const entries...
--
            pc += 2
            if NOLT=0 or bind or lint then
                ltCtrl(pc)
            end if -- NOLT
            if not isGscan then
                stmt = s5[pc-1]
                switch_flags = 0
                fallthrew = 0
                if and_bits(stmt,FALLTHRU) then
                    stmt -= FALLTHRU
                    switch_flags += FALLTHRU
                    fallthrew = 1
                end if
                if and_bits(stmt,SWTABLE) then
                    stmt -= SWTABLE
                    switch_flags += SWTABLE
                end if
                if and_bits(stmt,SWFOID) then
                    stmt -= SWFOID
                    switch_flags += SWFOID
                end if
--printf(1,"switch_flags[1] = %d\n",switch_flags)
                if stmt=IF then --2
                    -- see docs/switch.txt for details
                    link = s5[pc]
                    if s5[link-2]!=opCtrl then ?9/0 end if
                    if s5[link-1]!=END+IF then ?9/0 end if --3
                    tlink = s5[link]
--                  if s5[tlink-2]!=opCtrl then ?9/0 end if
--                  if s5[tlink-1]=ELSE then
--                      -- we have a default (no tests on this branch, obvs.)
--                      link = tlink
--                      tlink = s5[tlink]
--                  end if
                    switchable = 1
--puts(1,"s00\n")
                    svar = 0
                    smin = MAXINT
                    smax = MININT
                    casesfound = {}
                    caseslines = {}
                    switch_duplicate_found = 0
                    swecode = 0
                    while 1 do -- for each opCtrl (backwards, btw, but doesn't matter)
                        if s5[tlink-2]!=opCtrl then ?9/0 end if
                        k = s5[tlink-1]
                        if k=ELSE then --14
                            link = tlink
                            tlink = s5[tlink]
                            if s5[tlink-2]!=opCtrl then ?9/0 end if
                            k = s5[tlink-1]
                        elsif and_bits(k,SWFOID) then
                            exit
                        end if
                        if not find(k,{ELSIF,IF,IF+FALLTHRU,IF+FALLTHRU+SWTABLE,IF+SWTABLE}) then ?9/0 end if
--                      if not find(k,{ELSIF,IF,IF+FALLTHRU,IF+FALLTHRU+SWTABLE,IF+SWTABLE,IF+SWTABLE+SWFOID,IF+FALLTHRU+SWTABLE+SWFOID}) then ?9/0 end if
--                      npc = tlink+1
                        npc = tlink+2
                        orcount = 0
--                      switch_errline = emitline
                        switch_errline = s5[tlink+1]
                        while 1 do -- for ({opLn|Jeq|Jnot} (opJne|opJif) [opLabel])
                            opcode = s5[npc]
                            -- (btw, the following if-construct is itself "switchable"!)
                            if opcode=opLn
                            or opcode=opLnp
                            or opcode=opLnt
                            or opcode=opLnpt then
                                switch_errline = s5[npc+1]
                                npc += 2
                            elsif opcode=opJeq          -- eg "i=1 or"
                               or opcode=opJnot then    -- eg "not i or"
                                -- eg if not i or {i=1 or} i=2 then
                                --      is equivalent to
                                --    if i=0 or {i=1 or} i=2 then
                                --      and generates
                                --  opJnot,,, (not necessarily first)
                                --  {opJeq,...}
                                --  opJne (must be last)

                                -- check these are "xxx or" cases:
                                if s5[npc+1]!=scMerge then
                                    swecode = 1
                                    switchable = 0
                                    tlink = pc
                                    exit
                                end if

                                -- check all jumps act on same var:
                                tvar = s5[npc+4]
                                if svar=0 then
--                              if svar=0 and symtab[tvar][S_]=T_integer then
                                    svar = tvar
                                    src2 = svar
                                    getSrc2()
--13/11/16:
                                    if slroot2!=T_integer then
                                        swecode = 2
                                        switchable = 0
                                        tlink = pc
                                        exit
                                    end if
                                    swroot = slroot2
                                    swmin = smin2   -- \ scratch
                                    swmax = smax2   -- /  vars
                                elsif svar!=tvar then
                                    swecode = 13
                                    switchable = 0
                                    tlink = pc
                                    exit
                                end if

    --for reference only:
    --    36:  opJnot,1,54,0,409,1,0,                opJnot,scMerge,tgt,link,p1,oii,lastparam
    --    43:  opJeq, 4,67,0,409,52,1,1,             opJeq,ifMerge,tgt,link,p2,p3,tii,bothInit

                                if opcode=opJeq then
                                    -- opJeq,ifMerge,tgt,link,p2,p3,tii,bothInit
                                    -- check rhs is init, save/collect isInteger
                                    tii = s5[npc+6]
                                    bothInit = s5[npc+7]
                                    if not tii
                                    or not bothInit then
                                        swecode = 3
                                        switchable = 0
                                        tlink = pc
                                        exit
                                    end if
                                    src2 = s5[npc+5]
                                    getSrc2()
--DEV tryme:
--                                  if and_bits(slroot2,swroot) and
--                                     (slroot2!=T_integer or smin2!=smax2) then
-- or
--                                     and_bits(and_bits(swroot,T_sequence),
--                                              and_bits(slroot2,T_sequence))=0 and
--                                     (and_bits(slroot2,T_atom)!=T_integer or smin2!=smax2) then
                                    if slroot2!=T_integer or smin2!=smax2 then
                                        --[DEV: this could be improved; if we 
                                        --      know svar is integer, then if
                                        --      src2 is say "fred", just skip
                                        --      the test instead. ditto opJne]
                                        -- (ie, the tryme above/below, methinks)
                                        if and_bits(slroot2,swroot) then
                                            swecode = 4
                                            switchable = 0
                                            tlink = pc
                                            exit
                                        end if
                                    end if
                                    npc += 8
                                else -- opJnot (treat "not i or" as "i=0 or")
                                    smin2 = 0
                                    npc += 7
                                end if

                                k = find(smin2,casesfound)
                                if k then
                                    switch_duplicate_found = 1
                                    switch_duplicate_line = caseslines[k]
                                    switch_duplicate_value = smin2
                                    caseslines[k] = switch_errline
                                else
                                    casesfound &= smin2
                                    caseslines &= switch_errline
                                end if
--DEV tryme:
--                              if and_bits(slroot2,swroot) and
--                                 smin2>=swmin and
--                                 smin2<=swmax then
                                if smin2>=swmin and smin2<=swmax then
                                    if smin>smin2 then smin=smin2 end if
                                    if smax<smin2 then smax=smin2 end if
                                    switchable += 1
                                end if
                                orcount += 1

                            elsif opcode=opJne          -- eg "i=1 then"        \ must be
                               or opcode=opJif then     -- eg "not i then"      /  last
                                -- ("not i then" is equivalent to "i=0 then")
                                -- opJne,ifMerge,tgt,link,p2,p3,tii,bothInit
                                -- opJif,ifMerge,tgt,link,p1,oii,lastparam

                                if s5[npc+1]!=ifMerge then
                                    swecode = 5
                                    switchable = 0
                                    tlink = pc
                                    exit
                                end if

                                -- check all jumps act on same var:
                                tvar = s5[npc+4]
                                if svar=0 then
                                    svar = tvar
                                    src2 = svar
                                    getSrc2()
--13/11/16:
                                    if slroot2!=T_integer then
                                        swecode = 14
                                        switchable = 0
                                        tlink = pc
                                        exit
                                    end if
                                    swroot = slroot2
                                    swmin = smin2
                                    swmax = smax2
                                elsif svar!=tvar then
                                    swecode = 6
                                    switchable = 0
                                    tlink = pc
                                    exit
                                end if

--  39:  opCtrl,6,5,16,                      opCtrl,ELSIF,link,emitline
--  43:  opLn,17,                            --:  elsif toktype=LETTER and ttidx=3 then
--  45:  opJne,4,74,0,501,503,1,1,           opJne,ifMerge,tgt,link,p2,p3,tii,bothInit
--  53:  opJne,4,74,48,502,50,1,1,           opJne,ifMerge,tgt,link,p2,p3,tii,bothInit
--  61:  opLn,19,                            --:   emitON = 0
--  63:  opMovbi,500,23,1804,                opMovbi,dest,src,isInit
--  67:  opJmp,5,88,34,                      opJmp,endIfMerge,tgt,link
--  71:  opLabel,4,0,56,                         opLabel,ifMerge,0/x86loc,link

                                -- Immediately before the opCtrl we should find an opLabel,
                                --  which should link (only) to this (final?) jump.
                                --  (ie this enforces the "must be last" rule)

    --for reference only:
    --    66:  opLabel,4,0,52,                       opLabel,ifMerge,0/x86loc,link
    --    70:  opCtrl,6,46,n,                        opCtrl,ELSIF,link,emitline
    --                  ^ link (ie 72, tlink would be that 46 here, above the opJne/eq/not/if just processed)
                                if s5[link-6]!=opLabel then
-- 09/07/2013:
--                                  ?9/0
                                    swecode = 7
                                    switchable = 0
                                    tlink = pc
                                    exit
                                end if
--20/6/17 (after two below...)
--                              if s5[link-5]=endIfMerge then
                                if find(s5[link-5],{endIfMerge,breakMerge}) then
                                    link -= 4
                                    if s5[link-6]!=opLabel then
-- 10/07/2020:
--                                      ?9/0
                                        swecode = 15
                                        switchable = 0
                                        tlink = pc
                                        exit
                                    end if
                                end if
--17/6/16. triggered in simple_notepad.exw - I added flags[NO_BREAK] to Block() and it went away, so I undid this change
                                if s5[link-5]!=ifMerge then ?9/0 end if
--                              if s5[link-5]!=ifMerge then printf(1,"pilx86.e:6111 s5[link-5](%d)!=ifMerge(%d)\n",{s5[link-5],ifMerge}) end if
--                              if s5[link-5]!=ifMerge then ?"pilx86.e:6113 if s5[link-5]!=ifMerge" end if
--25/8/2012: BUG!!!
--DEV 7/12/10 (else not last)
--if 0 then
--20/6/17: (case fallthrough with no default, first got breakMerge not ifMerge above...) [and undone as above]
                                if s5[link-3]!=npc+3 then
--                              if s5[link-5]=ifMerge
--                              and s5[link-3]!=npc+3 then
                                    swecode = 8
                                    switchable = 0
                                    tlink = pc
                                    exit
                                end if
--end if


    --for reference only:
    --    36:  opJnot,1,54,0,409,1,0,                opJnot,scMerge,tgt,link,p1,oii,lastparam
    --    43:  opJne, 4,67,0,409,52,1,1,             opJne,ifMerge,tgt,link,p2,p3,tii,bothInit

                                if opcode=opJne then
                                    -- check rhs is init, save/collect isInteger
                                    bothInit = s5[npc+7]
                                    if not bothInit then
                                        swecode = 9
                                        switchable = 0
                                        tlink = pc
                                        exit
                                    end if
                                    src2 = s5[npc+5]
                                    getSrc2()
                                    if slroot2!=T_integer or smin2!=smax2 then
                                        --[DEV: as above]
                                        if and_bits(slroot2,swroot) then
                                            swecode = 10
                                            switchable = 0
                                            tlink = pc
                                            exit
                                        end if
                                    end if
                                    npc += 8
                                else -- opJif (treat "not i then" as "i=0 then")
                                -- (rhs is implied literal 0)
                                    smin2 = 0
                                    npc += 7
                                end if
--DEV duplicate code:
                                k = find(smin2,casesfound)
                                if k then
                                    switch_duplicate_found = 1
                                    switch_duplicate_line = caseslines[k]
                                    switch_duplicate_value = smin2
                                    caseslines[k] = switch_errline
                                else
                                    casesfound &= smin2
                                    caseslines &= switch_errline
                                end if

                                if smin2>=swmin and smin2<=swmax then
                                    if smin>smin2 then smin=smin2 end if
                                    if smax<smin2 then smax=smin2 end if
                                    switchable += 1
                                end if
                                --
                                -- Sanity check:
                                --
                                if orcount then
                                    --
                                    -- We should now expect an opLabel, which links back exactly
                                    -- through "orcount" opJeq/not, and ends up with 0 at tlink+4:
                                    --
-- for reference only:
--     8:  opCtrl,2,120,n,                       opCtrl,IF,link,emitline
--    11:  opJeq,1,30,0,409,24,1,1,              opJeq,scMerge,tgt,link,p2,p3,tii,bothInit
--    19:  opJne,4,43,0,409,42,1,1,              opJne,ifMerge,tgt,link,p2,p3,tii,bothInit
--    27:  opLabel,1,0,14,                       opLabel,scMerge,0/x86loc,link

--    31:  opCtrl,6,10,n,                        opCtrl,ELSIF,link,emitline
--    34:  opLn,5,                               --: elsif not i or i=7 then
--    36:  opJnot,1,54,0,409,1,0,                opJnot,scMerge,tgt,link,p1,oii,lastparam
--    43:  opJne, 4,67,0,409,52,1,1,             opJne,ifMerge,tgt,link,p2,p3,tii,bothInit
--    51:  opLabel,1,0,39,                       opLabel,scMerge,0/x86loc,link

--    68:  opCtrl,6,33,n,                        opCtrl,ELSIF,link,emitline
--    71:  opLn,7,                               --: elsif i=7 or not i then
--    73:  opJeq,1, 91,0,409,52,1,1,             opJeq,scMerge,tgt,link,p2,p3,tii,bothInit
--    81:  opJif,4,104,0,409,1,0,                opJif,ifMerge,tgt,link,p1,oii,lastparam
--    88:  opLabel,1,0,76,                       opLabel,scMerge,0/x86loc,link

--8/12/10:
--                                  if s5[npc]!=opLabel then ?9/0 end if
                                    if s5[npc]!=opLabel then
                                        swecode = 11
                                        switchable = 0
                                        tlink = pc
                                        exit
                                    end if
--DEV 31/10 surely:
--                                  if s5[npc+1]!=scMerge then ?9/0 end if
                                    link = s5[npc+3]
                                    while 1 do
                                        opcode = s5[link-3]
                                        if opcode!=opJeq
                                        and opcode!=opJnot then
                                            ?9/0
                                        end if
                                        orcount -= 1
                                        if orcount=0 then
                                            if s5[link]!=0 then ?9/0 end if
--dev NOT SURE HERE... (seems to be OK) [from adding emitline to opCtrl)
--                                          if link!=tlink+4        -- no opLn/p/t/pt case
--                                          and link!=tlink+6 then  -- opLn/p/t/pt case
--                                              ?9/0
--                                          end if
                                            if link!=tlink+5        -- no opLn/p/t/pt case
                                            and link!=tlink+7 then  -- opLn/p/t/pt case
                                                ?9/0
                                            end if
                                            exit
                                        end if
                                        link = s5[link]     -- [0] here is error!
                                    end while
                                end if

                                exit
                            else -- all other opcodes (opJle etc)
                                swecode = 12
                                switchable = 0
                                tlink = pc
                                exit
                            end if
                        end while
                        link = tlink
                        if link=pc then exit end if
                        tlink = s5[link]
                    end while

                    if switchable!=0 then
                        if switch_duplicate_found=1 then
--                          if switch_flags then
                                fileno = symtab[vi][S_FPno]
--DEV subroutine in pmsg.e?
                                if equal(expandedYet[fileno],0) then
                                    text = allfiles[fileno]
                                    exptext[fileno] = expandIntoLines()
                                    expandedYet[fileno] = linestarts
                                else
                                    linestarts = expandedYet[fileno]
                                end if
                                tokline = switch_duplicate_line
                                tokcol = linestarts[tokline]
                                Abort(sprintf("duplicate case value [%d, il offset:%d]",{switch_duplicate_value,npc}))
--                          else
--printf(1,"oh1 %d\n",switch_duplicate_line)
--                              switchable = 0
--                              tlink = pc
----                                exit
--                          end if
                        elsif (switch_flags or switchable>8) -- based on test/swtime results
--2/3/21 (pwa/p2js.exw genuinely needed 116+... (so it's now 0.5% populated)
--24/3/21                                                    0.25%
--2/5/21                                                     0.125%
--DEV is there not a point where we should just do a find?
--                        and smax-smin <= 20*switchable then
--                        and smax-smin <= 200*switchable then
--                        and smax-smin <= 400*switchable then
                          and smax-smin <= 800*switchable then
--trace(1)
if 0 then   -- should be 0 for release!
    dbg = symtab[vi]
    dbg = dbg[S_FPno]
    dbg = filenames[dbg]
    printf(1,"switchable[%d] if-construct found (line %d, %s)\n",{switchable,emitline,dbg[2]})
--  if getc(0) then end if
end if
                            --
                            -- set the SWITCH bit on all opCtrl;
                            --
                            switch_flags += SWITCH
                            stmt += switch_flags
                            s5[pc-1] = stmt
                            link = s5[pc]
                            while 1 do
                                if s5[link-2]!=opCtrl then ?9/0 end if
                                stmt = s5[link-1]
                                if and_bits(stmt,SWITCH) then exit end if
                                stmt += switch_flags                    
                                s5[link-1] = stmt
                                link = s5[link]
                            end while
                            if link!=pc then ?9/0 end if
                            switch_flags = 0
--else
--  puts(1,"not switchable!\n")
                        end if
                    end if

--2/3/2021 (!!) - it certainly //looks// like it should have always been thus....
                    if switch_flags then
--                  if swecode then
                        fileno = symtab[vi][S_FPno]
--DEV make this a subroutine in pmsg.e?
                        if equal(expandedYet[fileno],0) then
                            text = allfiles[fileno]
                            exptext[fileno] = expandIntoLines()
                            expandedYet[fileno] = linestarts
                        else
                            linestarts = expandedYet[fileno]
                        end if
                        tokline = emitline
                        tokcol = linestarts[tokline]+match("switch",exptext[fileno][tokline])-1
                        -- (we may be able to generate more helpful messages than this...)
                        string details
                        if swecode=8 then
                            details = "[default not at end]"
                        else
--                          details = sprintf("[swecode=%d,npc=%d,opcode=%d(%s)]",{swecode,npc,opcode,opNames[opcode]})
                            details = sprintf("[swecode=%d,npc=%d,opcode=%d(%s),smin=%d,smax=%d]",{swecode,npc,opcode,opNames[opcode],smin,smax})
                        end if                              
                        Abort("cannot create jump table "&details)
                    end if

--      :  opLn,2,                               --: if i=1 or i=2 then
--     8:  opCtrl,2,120,n,                       opCtrl,IF,link,emitline
--    11:  opJeq,1,30,0,409,24,1,1,              opJeq,scMerge,tgt,link,p2,p3,tii,bothInit
--    19:  opJne,4,43,0,409,42,1,1,              opJne,ifMerge,tgt,link,p2,p3,tii,bothInit
--    27:  opLabel,1,0,14,                       opLabel,scMerge,0/x86loc,link
--    31:  opLn,4,                               --:  puts(1,"umm")
--          opPuts,24,410,
--    36:  opJmp,5,117,0,                        opJmp,endIfMerge,tgt,link
--    40:  opLabel,4,0,22,                       opLabel,ifMerge,0/x86loc,link
--    44:  opCtrl,6,10,n,                        opCtrl,ELSIF,link,emitline
--    47:  opLn,5,                               --: elsif i=3 then
--    49:  opJne,4,69,0,409,43,1,1,              opJne,ifMerge,tgt,link,p2,p3,tii,bothInit
--    57:  opLn,6,                               --:  puts(1,"umm")
--          opPuts,24,410,
--    62:  opJmp,5,117,39,                       opJmp,endIfMerge,tgt,link
--    66:  opLabel,4,0,52,                       opLabel,ifMerge,0/x86loc,link
--    70:  opCtrl,6,46,n,                        opCtrl,ELSIF,link,emitline
--    73:  opLn,7,                               --: elsif i=4 and i>3 then      -- kills it
--    75:  opJne,4,105, 0,409,49,1,1,            opJne,ifMerge,tgt,link,p2,p3,tii,bothInit
--    83:  opJle,4,105,78,409,43,1,1,            opJle,ifMerge,tgt,link,p2,p3,tii,bothInit
--    91:  opLn,8,                               --:  puts(1,"umm")
--          opPuts,24,410,
--    96:  opLn,9,                               --:--  elsif i=5 or j=6 then       -- kills it
--    98:  opJmp,5,117,65,                       opJmp,endIfMerge,tgt,link
--   102:  opLabel,4,0,86,                       opLabel,ifMerge,0/x86loc,link
--   106:  opCtrl,14,72,n,                       opCtrl,ELSE,link,emitline
--   109:  opLn,11,                              --:  puts(1,"umm")
--          opPuts,24,410,
--   114:  opLabel,5,0,101,                      opLabel,endIfMerge,0/x86loc,link
--   118:  opCtrl,3,108,n,                       opCtrl,END+IF,link,emitline
--
--  special:
--     6:  opLn,3,                               --: if not i then
--     8:  opCtrl,2,145,n,                       opCtrl,IF,link,emitline
--    11:  opJif,4,30,0,409,1,0,                 opJif,ifMerge,tgt,link,p1,oii,lastparam
--    18:  opLn,4,                               --:  puts(1,"0\n")
--    20:  opPuts,24,410,
--    23:  opJmp,5,142,0,                        opJmp,endIfMerge,tgt,link
--    27:  opLabel,4,0,14,                       opLabel,ifMerge,0/x86loc,link
--    31:  opCtrl,6,10,n,                        opCtrl,ELSIF,link,emitline
--    34:  opLn,5,                               --: elsif not i or i=7 then
--    36:  opJnot,1,54,0,409,1,0,                opJnot,scMerge,tgt,link,p1,oii,lastparam
--    43:  opJne, 4,67,0,409,52,1,1,             opJne,ifMerge,tgt,link,p2,p3,tii,bothInit
--    51:  opLabel,1,0,39,                       opLabel,scMerge,0/x86loc,link
--    55:  opLn,6,                               --:  puts(1,"umm")
--    57:  opPuts,24,411,
--    60:  opJmp,5,179,26,                       opJmp,endIfMerge,tgt,link
--    64:  opLabel,4,0,46,                       opLabel,ifMerge,0/x86loc,link
--    68:  opCtrl,6,33,n,                        opCtrl,ELSIF,link,emitline
--    71:  opLn,7,                               --: elsif i=7 or not i then
--    73:  opJeq,1, 91,0,409,52,1,1,             opJeq,scMerge,tgt,link,p2,p3,tii,bothInit
--    81:  opJif,4,104,0,409,1,0,                opJif,ifMerge,tgt,link,p1,oii,lastparam
--    88:  opLabel,1,0,76,                       opLabel,scMerge,0/x86loc,link
--    92:  opLn,8,                               --:  puts(1,"umm")
--    94:  opPuts,24,411,
--          end if -- k and ifMerge/scMerge
                end if -- IF
                if and_bits(stmt,SWITCH) then
                    stmt -= SWITCH
                    switch_flags = and_bits(stmt,FALLTHRU+SWTABLE)
--printf(1,"switch_flags[2] = %d\n",switch_flags)
                    stmt -= switch_flags
--                  -- process any opLn etc first
--                  pc += 1
--                  opcode = s5[pc]
                    
                    if stmt=IF
                    or stmt=IF+SWFOID
                    or stmt=ELSIF then
                        if stmt=IF
                        or stmt=IF+SWFOID then
                            switch_stack_idx += 1
                            if switch_stack_idx then
                                if switch_stack_idx>length(switch_x86locs) then
                                    switch_x86locs = append(switch_x86locs,switch_x86loc)
                                    switch_firsts  = append(switch_firsts, switch_first)
                                    switch_lengths = append(switch_lengths,switch_length)
                                    switch_boundcs = append(switch_boundcs,switch_boundc)
--                                  switch_fallthrus = append(switch_fallthrus,switch_fallthru)
                                    switch_vars = append(switch_vars,switch_var)
                                else
                                    switch_x86locs[switch_stack_idx] = switch_x86loc
                                    switch_firsts [switch_stack_idx] = switch_first
                                    switch_lengths[switch_stack_idx] = switch_length
                                    switch_boundcs[switch_stack_idx] = switch_boundc
--                                  switch_fallthrus[switch_stack_idx] = switch_fallthru
                                    switch_vars[switch_stack_idx] = switch_var
                                end if
                            end if
                            switch_boundc = 0
                            switch_var = svar
                            --
                            -- first off, do we need a bounds check?
                            --  (upper bound can double as a typecheck if rqd)
                            --  (we may elect to extend range a bit instead)
                            --              
                            -- swmin/swmax are the range of svar
                            -- smin/smax are the range of the switch
--15/10/15: (in a switch v with fallthru, even if we know v=3 (say), we still need to generate the full construct,
--           although admittedly we could [possibly] ditch the actual jump table... and even do a "start from"..)
--if tmpd and tmpr=-1 then
--  
--else
                            if tmpd then
--                          if tmpd and tmpr!=-1 then
                                if tmpd!=switch_var then ?9/0 end if -- oops?
                                if tmpr=-1 then ?9/0 end if -- oops?
                                reg = tmpr
                                tmpd = 0
                            else
                                reg = loadReg(switch_var)                   -- mov reg,[switch_var]
-- just an idea, no help:
--                              reg = loadReg(switch_var,CLEAR)             -- mov reg,[switch_var]
                            end if
--DEV if lower bound is 0 (and high bound needs [type]checking) then use an unsigned jump...
                            if smin<swmin then ?9/0 end if -- sanity check
                            if swmin<smin then
                                -- extend range/lower bounds check
                                if (smin-swmin)<10 then
                                    smin = swmin
                                else
                                    mod = m_cmp
                                    regimm365(smin)                     -- cmp reg,imm
                                    emitHex6j(jl_rel32,switch_boundc)   -- jl ?? (to be backpatched)
                                    switch_boundc = length(x86)         -- (this is a chain ptr)
                                end if
                            end if
                            if smax>swmax then ?9/0 end if  -- sanity check
                            if smax<swmax or swroot!=T_integer then
                                -- extend range/type check/upper bounds check
                                if (swmax-smax)<10 and swroot=T_integer then
                                    smax = swmax
                                else
                                    mod = m_cmp
                                    regimm365(smax)                     -- cmp reg,imm
                                    emitHex6j(jg_rel32,switch_boundc)   -- jg ?? (to be backpatched)
                                    switch_boundc = length(x86)         -- (this is a chain ptr)
                                end if
                            end if
--trace(1)
--printf(1,"switch offset: %d (%d)\n",{smin,-4*smin})
                            switch_first = smin
                            switch_length = smax-smin+1     -- [DEV??]
--DEV tryme:
-- if switch_length!=1 then
if X64 then
                            sib = 0o305 + reg*8                         
                            emitHex7base(jmp_si5_imm32,sib,-8*smin) -- jmp qword[i*s+imm32]
else
                            sib = 0o205 + reg*8                         
                            emitHex7base(jmp_si5_imm32,sib,-4*smin) -- jmp dword[i*s+imm32]
end if
--end if -- 15/10/15
                            -- (of course if the table has a first entry corresponding to say 7,
                            --  then pretend it starts at -28; only if smin==0 does imm32 actually 
                            --  point at the table (immediately following).)
                            switch_x86loc = length(x86)
--15/10/15: (nope)
                            for i=smin to smax do
--                          for i=swmin to swmax do
                                if q86 then
                                    quad(isAddr,-1)     -- "-1" indicates "unused" entries.
                                else
                                    x86 &= {isAddr,0,0,-1}  -- "-1" indicates "unused" entries.
                                end if
--5/1/14:
                                if X64 then
                                    x86 &= {0,0,0,0}
                                end if
                            end for
if stmt=IF+SWFOID then
                            -- fill in any jump table entries that are still -1:
if X64 then
                            k = switch_x86loc-4
else
                            k = switch_x86loc
end if
                            x86tloc = length(x86)
                            for i=1 to switch_length do
if X64 then
                                k += 8
else
                                k += 4
end if
                                if x86[k]=-1 then
if X64 then
                                    toffset = x86tloc-switch_x86loc-i*8+4
else
                                    toffset = x86tloc-switch_x86loc-i*4
end if
                                    x86[k] = toffset
                                    -- (while the actual offset we want to generate is from
                                    --  switch_x86loc(+1), the isAddr needs an offset which
                                    --  is relative to the isAddr itself.. hence -i*4)
                                end if
                            end for
--trace(1)
                            -- and any bounds checks
                            while switch_boundc do
                                tgt = switch_boundc
                                k = x86tloc-switch_boundc
                                switch_boundc = x86[switch_boundc]
                                x86[tgt] = k
                            end while
end if
                        else -- ELSIF
                            -- re-grab swmin/swmax
                            src2 = switch_var
                            getSrc2()
                            swroot = slroot2
                            swmin = smin2
                            swmax = smax2
                        end if
if stmt!=IF+SWFOID then
                        -- (common code to IF/ELSIF)
                        -- process ({opLn|Jeq|Jnot} (opJne|opJif) [opLabel]):
--                      pc += 1
                        pc += 2
                        orcount = 0
                        flag = false    --DEV what is this? (false==resume processing at next opCtrl)
                        while 1 do
                            npc = pc
                            opcode = s5[pc]
                            if opcode=opLn
                            or opcode=opLnp
                            or opcode=opLnt
                            or opcode=opLnpt then
                                if opcode=opLn then
                                    opLnv = 0
                                else
--                                  if sched then
--                                      if schidx then
--                                          emitline = lastline
--                                          schend()
--                                      end if
--                                  end if
                                    opLnv = opcode
                                    -- all regs will get trashed before they could be used:
                                    reginfo = 0
                                end if
                                oplnlen = length(x86)
                                emitline = s5[pc+1]
--printf(1,"switch line %d\n",emitline)
                                pc += 2
                            elsif opcode=opJeq          -- eg "i=1 or"
                               or opcode=opJnot then    -- eg "not i or"
--Did not fix the problem, ditto below (because tgt was not set? - YES!)
                                mergeSet = s5[pc+1]
                                tgt = s5[pc+2]
                                merge(mergeSet,0)
                                if opcode=opJeq then
                                    src2 = s5[pc+5]
                                    getSrc2()
                                    pc += 8
                                else -- opJnot (treat "not i or" as "i=0 or")
                                    smin2 = 0
                                    pc += 7
                                end if
                                if smin2>=swmin and smin2<=swmax then
--    11:  opJeq,1,30,0,409,24,1,1,              opJeq,scMerge,tgt,link,p2,p3,tii,bothInit
--    36:  opJnot,1,54,0,409,1,0,                opJnot,scMerge,tgt,link,p1,oii,lastparam
                                    flag = true
                                    s5[npc+2] = J_SWITCHED
if X64 then
                                    joffset = (smin2-switch_first+1)*8
                                    k = switch_x86loc-4+joffset
                                    toffset = length(x86)-switch_x86loc-joffset+4
else
                                    joffset = (smin2-switch_first+1)*4
                                    k = switch_x86loc+joffset
                                    toffset = length(x86)-switch_x86loc-joffset
end if
--DEV tryme:
--                                  toffset = length(x86)-k
-- 26/8/2012 (see other one[+51 lines])
--if x86[k]!=-1 then
----DEV may need linestarts (and fileno) to be setup (spotted in passing)
--  -- (we are inside "if and_bits(stmt,SWITCH) then" here)
--  tokline = emitline
--  tokcol = linestarts[tokline]
--  Abort("duplicate case value (should have been caught earlier!)")
--end if
                                    x86[k] = toffset
                                    -- (while the actual offset we want to generate is from
                                    --  switch_x86loc(+1), the isAddr needs an offset which
                                    --  is relative to the isAddr itself.. hence -joffset)
-- 17/6/2012:
                                elsif fallthrew then
                                    flag = true
                                    s5[npc+2] = J_SWITCHED
                                else
--DEV if not fallthrough??
--                              elsif not and_bits(switch_flags,FALLTHRU) then
--printf(1,"detach1(%d,%d)\n",{s5[npc+2],npc+3})
                                    detach(s5[npc+2],npc+3)
                                end if
                                orcount += 1
                            else
                                exit
                            end if
                        end while
                        -- process (opJne|opJif) [opLabel]:
                        npc = pc
                        mergeSet = s5[pc+1]
                        tgt = s5[pc+2]
                        merge(mergeSet,0)
                        if opcode=opJne then
                            src2 = s5[pc+5]
                            getSrc2()
                            pc += 8
                        else -- opJif (treat "not i then" as "i=0 then")
                            -- (rhs is implied literal 0)
                            if opcode!=opJif then ?9/0 end if
                            smin2 = 0
                            pc += 7
                        end if
                        if smin2>=swmin and smin2<=swmax then
                            flag = true
                            s5[npc+2] = J_SWITCHED
if X64 then
                            joffset = (smin2-switch_first+1)*8
                            k = switch_x86loc-4+joffset
                            toffset = length(x86)-switch_x86loc-joffset+4
else
                            joffset = (smin2-switch_first+1)*4
                            k = switch_x86loc+joffset
                            toffset = length(x86)-switch_x86loc-joffset
end if
--DEV tryme:
--                          toffset = length(x86)-k
-- 26/8/2012 not valid for default first anyway:
--if x86[k]!=-1 then
--                              fileno = symtab[vi][S_FPno]
----DEV subroutine in pmsg.e?
--                              if equal(expandedYet[fileno],0) then
--                                  text = allfiles[fileno]
--                                  exptext[fileno] = expandIntoLines()
--                                  expandedYet[fileno] = linestarts
--                              else
--                                  linestarts = expandedYet[fileno]
--                              end if
--                              -- (we are inside "if and_bits(stmt,SWITCH) then" here)
--                              tokline = emitline
--                              tokcol = linestarts[tokline]
--                              Abort("duplicate case value (should have been caught earlier!)")
--end if
                            x86[k] = toffset
                            -- (while the actual offset we want to generate is from
                            --  switch_x86loc(+1), the isAddr needs an offset which
                            --  is relative to the isAddr itself.. hence -joffset)
-- 17/6/2012:
                        elsif fallthrew then
                            flag = true
                            s5[npc+2] = J_SWITCHED
                        else
--DEV if not fallthrough?
--printf(1,"detach2(%d,%d)\n",{s5[npc+2],npc+3})
                            detach(s5[npc+2],npc+3)
                        end if
                        if orcount then
                            if s5[pc]!=opLabel then ?9/0 end if
                            pc += 4
                        end if
                        if not flag then
                            -- resume processing at next opCtrl....
                            -- (DEV may need to delink a fallthrough jump)
                            pc = tgt+1
                            if s5[pc]!=opCtrl then ?9/0 end if
                        end if
--                      pc -= 1     -- (to counteract the +=1 below)
                        pc -= 2     -- (to counteract the +=2 below)
end if
                    elsif stmt=ELSE
--                     or stmt=IF+SWFOID
                       or stmt=END+IF then
                        if stmt=END+IF then
                            -- was there no ELSE?
                            link = s5[pc]
                            if s5[link-2]!=opCtrl then ?9/0 end if
                            flag = (s5[link-1]!=ELSE) -- true if no else, ie "end if" has work to do
--reginfo = 0 -- desperation! [DEV]
                        end if

--DEV: for say
--  switch 
--      default:
--      case 1:
--  end switch
-- We have to defer this processing to end if anyway...
--  (ie just save length(x86) somewhere, and flag(s) to control things)

                        if stmt=ELSE
--                      or stmt=IF+SWFOID
                        or flag then
                            -- fill in any jump table entries that are still -1:
if X64 then
                            k = switch_x86loc-4
else
                            k = switch_x86loc
end if
                            x86tloc = length(x86)
                            for i=1 to switch_length do
if X64 then
                                k += 8
else
                                k += 4
end if
                                if x86[k]=-1 then
if X64 then
                                    toffset = x86tloc-switch_x86loc-i*8+4
else
                                    toffset = x86tloc-switch_x86loc-i*4
end if
                                    x86[k] = toffset
                                    -- (while the actual offset we want to generate is from
                                    --  switch_x86loc(+1), the isAddr needs an offset which
                                    --  is relative to the isAddr itself.. hence -i*4)
                                end if
                            end for
--trace(1)
                            -- and any bounds checks
                            while switch_boundc do
                                tgt = switch_boundc
                                k = x86tloc-switch_boundc
                                switch_boundc = x86[switch_boundc]
                                x86[tgt] = k
                            end while
                        end if

                        if stmt=END+IF then
                            if switch_stack_idx then
                                switch_x86loc = switch_x86locs[switch_stack_idx]
                                switch_first  = switch_firsts [switch_stack_idx]
                                switch_length = switch_lengths[switch_stack_idx]
                                switch_boundc = switch_boundcs[switch_stack_idx]
                                switch_var = switch_vars[switch_stack_idx]
                            end if
                            switch_stack_idx -= 1
                        end if
                    else ?9/0
                    end if
--if getc(0) then end if
                else -- not and_bits(stmt,SWITCH)
                    opcode = lastop
                end if
--trace(1)
--dbg = x86[46]
            end if -- not isGscan
            pc += 2

        elsif opcode=opLabel then
--if pc=43 then
--  trace(1)
--end if
            -- opLabel,mergeSet,ifpt/x86offset,link
--DEV14:
            mergeSet = s5[pc+1]
            nextop = 0
            if mergeSet then    -- DoWhile/just handled by opLoopTop
if NOLT=0 or bind or lint then
                ltFlip(pc)
end if -- NOLT
            end if
            if not isGscan then
                --if vi!=21 then ?s5 ?pc ?x86 end if
                --          ifpt = s5[pc+2]
                tgt = pc+3              -- opLabel's link field
-- ctrl = s5[pc+4]
-- ctnr = s5[pc+5]
                if mergeSet=0 then      -- from DoWhile (loop top)
                    reginfo = 0
                else
                    if lastop=opJmp
                    or lastop=opRetf
                    or lastop=opRTErn
                    or lastop=opAbort then
                        flag = -1   -- just restore
                    else
                        flag =  1   -- merge and restore
                    end if
                    merge(mergeSet,flag)
                end if
--          tgt = s5[pc+3]
--          while tgt do
-- 24/10/10:
                x86tloc = length(x86)               -- opLabel's x86 offset
                flag = 0
                while 1 do
                    tgt = s5[tgt]
                    if tgt=0 then exit end if
                    if DEBUG then
--DEV 7/12 (else not last)
--                      if tgt>pc then ?9/0 end if  -- bckwd jmps should not be linked
                        if s5[tgt-2]!=mergeSet then ?9/0 end if
                    end if
-- 8/12 (else not last)
if tgt<pc then
                    wrk = s5[tgt-1]                 -- Jmp's x86 offset
-- 16/11/10:
  if wrk!=J_SWITCHED then
                    k = x86tloc-wrk
                    x86[wrk] = k
                    flag = 1
  end if
end if
                end while
                if not flag then
                    opcode = lastop
                end if
                s5[pc+2] = x86tloc                  -- save Label's x86 offset
--              if sched then
----            if schidx then schend() end if
---- but don't lineinfo() yet!
--                  wasemitline = emitline
--                  emitline = lastline
--                  schend()
--                  emitline = wasemitline
--              end if
            --if vi!=21 then ?x86 end if
            end if
            pc += 4 -- (nb matching one of these in opEndFor2)
            --          pc += 5
            --          if nextop=opCtrl then
            --              pc += 9 (10?)
            --          else
--          pc += 6
--          end if

        elsif opcode=opTchk then        -- 9
            -- opTchk,varno,wasOptTypeCheck,default
            src = s5[pc+1]
            wasOptTypeCheck = s5[pc+2]
            pDefault = s5[pc+3]
--          reg = 0
            if pDefault then
                if not isGscan then
                    reg = loadReg(src)                  -- mov reg,[src]
                    cmp_h4(reg)                         -- cmp reg,h4 (ie is unassigned/omitted param?)
                    emitHex6j(jne_rel32,0)              -- jne @f [sj NOT ok]
                    dbpos = length(x86)
                    -- nb: reg must be preserved over defaulting, until the typechecking code after it
                    --      (in the case, obviously, that above jne is taken)
                end if -- not isGscan
                if pDefault<0 then -- command_line() or length(-default)
                    pDefault = 0-pDefault
                    if pDefault=T_command_line then
                        -- set dest as T_Dsq of strings, any length:
                        dest = src
                        getDest()
                        slroot = T_Dsq
                        sltype = T_Dsq
                        setyp = T_string
                        slen = -2
                        storeDest()

                        routineNo = T_command_line
                        symk = symtab[routineNo]
                        if DEBUG then
                            if callpending then ?9/0 end if
                        end if
                        gmask = symk[S_Efct]    -- (save for the opCall)
                        if not isGscan then
                            first = symk[S_Parm1]
                            ltot = symk[S_Ltot]
                            symk = {}

--                          if sched then
--                              sch00n = schoon
--                          end if
                            if X64 then
                                emitHex1(#48)
                            end if
                            if ltot then
--                              if sched then
--                                  schedule(0,0,ecxbit,pUV,0,0)
--                              end if
--                              emitHex5w(mov_ecx_imm32,ltot)       -- mov ecx,no of params+locals
                                movRegImm32(ecx,ltot)               -- mov ecx,no of params+locals
                            else
--                              if sched then
--                                  schedule(0,0,ecxbit,pUV,1,0)
--                              end if
                                emitHex2s(xor_ecx_ecx)              -- xor ecx,ecx
                            end if
--                          if sched then
--                              schedule(0,0,edxbit,pUV,0,0)
--                          end if
if newEmit then
                            movRegVno(edx,routineNo)                -- mov e/rdx,routineNo
else
                            emitHex5w(mov_edx_imm32,routineNo)      -- mov edx,routineNo
end if
--                          if sched then
--                              schend()
--                          end if
                            emitHex5callG(opFrame)
                            reginfo = 0 -- opFrame leaves registers as:
                                        -- eax is h4, ebx is 0, ecx is 0, 
                                        -- edx is threadstack (==[ebp+28])
                                        -- esi is ebp-N*4 or thereabouts,
                                        -- edi is vsb_root (==[ebp+32])
                            if NOLT=0 or bind or lint then
                                if and_bits(gmask,E_vars) then  -- (gmask set by opFrame)
                                    ltCall(0,gmask,pc)
                                end if
                            end if -- NOLT

                            -- NB should not be calling lineinfo here
--                          if sched then
--                              schend() -- isAddr not relocatable (yet?)
--                          end if
if X64 then
                            emitHex1(#48)
--EXCEPT
--                          emitHex7a(mov_ebpd8_i32,32,5)           -- mov [rbp+32],<return addr>
                            emitHex7a(mov_ebpd8_i32,56,5)           -- mov [rbp+56],<return addr>
else
--EXCEPT
--                          emitHex7a(mov_ebpd8_i32,16,5)           -- mov [ebp+16],<return addr>
                            emitHex7a(mov_ebpd8_i32,28,5)           -- mov [ebp+28],<return addr>
end if
                            if q86>1 then
--  ?"9/0 (x86 3861)\n" -- separate chain for isIL??
                                x86 &= jump_rel32
                                quad2(isIL,routineNo)
                            else
                                h5 = {jump_rel32,isIL,0,0,routineNo}  -- (keeps that ginfo happy) [DEV temp?]
                                emitHex5s(h5)                                       -- jmp <routine_code>
                            end if
--                          reginfo = 0 -- trashes all registers

                            storeMem(dest,eax)                      -- mov [dest],eax
                        end if -- not isGscan
--DEV...
--                  elsif pDefault=T_current_dir then
                    else -- pDefault=T_length, length(-default)
                        -- set dest[src] as 0..MAXLEN, unless [src2] is fixed length
                        dest = src
                        getDest()
                        src2 = pDefault
                        getSrc2()

                        sltype = T_integer
                        slroot = T_integer
                        if slen2>=0 then
                            smin = slen2
                            smax = slen2
                        else
                            smin = 0
                            smax = MAXLEN
                        end if
                        storeDest()
                        if not isGscan then
                            if ssNTyp2=S_TVar -- we can assume init then!
                            and not and_bits(slroot2,T_atom) then
                                if slen2>=0 then
                                    storeImm(dest,slen2)                -- mov [dest],imm32
                                    if reginfo then clearMem(dest) end if
                                else
--PL we don't want to accumulate any reg info here...
--                                  def = loadReg(src2)                 -- mov def,[src2]
                                    def = loadReg(src2,CLEAR)           -- mov def,[src2]
                                    wrk = spareReg(0)
if X64 then
                                    sibLoad743(wrk,by4,def,ebx,-24)     -- mov wrk,[rbx+def*4-24]
else
                                    sibLoad743(wrk,by4,def,ebx,-12)     -- mov wrk,[ebx+def*4-12]
end if
--oops: (DEV so why is this worng?!) [Answer: it was missing the clearMem, seems OK now (10/12/2011)]
--                                  storeReg(wrk,src,1,0)               -- mov [src],wrk
                                    storeMem(src,wrk)                   -- mov [src],wrk
                                    clearMem(src)
                                end if
                            else
                                leamov(edi,src)                         -- lea edi,[src]/mov edi,src (addr result)
                                loadToReg(esi,src2)                     -- mov esi,[src2]
                                movRegVno(edx,src2)                     -- mov e/rdx,src2 (var no)
                                emitHex5callG(opLen)                    -- call :%opLen
--                              clearReg(edi)
--                              storeReg(eax,src2,0,1)  -- just record that eax==[src2]
--                              storeReg(ecx,src,0,1)   -- just record that ecx==[src]
                                reginfo = 0
                            end if
                        end if -- not isGscan
                    end if
                else -- (pDefault>0)
                    -- set dest as per opMove etc...
                    dest = src
                    if pDefault!=2 then -- not divide by 0
                        getDest()
                        src = pDefault  -- (src is restored/unclobber'd later)
                        getSrc()
--if pfileno=1 then printf(1,"pilx86.e line 7086 (opTchk pDefault>0): slroot=%04b (dest=%d,src=%d)\n",{slroot,dest,src}) end if
                        gtypeonly = 1
                        storeDest()
                    end if
                    if not isGscan then

                        -- NB: we are moving [src2] (aka pDefault) to [src] (aka var being typechecked) here...
                        --  (ie there is no "dest", and this is instead of typecheck, since src is unassigned)

                        if pDefault=2 then  -- divide by zero
                            emitHex5callG(opDiv0)                               -- call :%e02atdb0 (see pDiagN.e)
                            -- fatal error, does not return
--                          opcode = opRTErn -- for opLabel
-- (DEV almost removed 24/11, but I think more code follows in this case...)
                        elsif slroot=T_integer then
                            if smin=smax then
                                storeImm(dest,smin)                         -- mov [dest],imm32
                                if reginfo then clearMem(dest) end if
                            else
                                loadToReg(reg,src)                          -- mov reg,[src]
                                storeReg(reg,dest,1,1)                      -- mov [dest],reg
                                clearReg(eax)
                                clearReg(reg)
                            end if
                        else
--3/10/10: (breaks t52)
--                          if bind and ssNTyp1=S_Const and and_bits(state1,K_noclr) then
                            if not newEmit and ssNTyp1=S_Const and and_bits(state1,K_noclr) then
?9/0
                                emitHex6constrefcount(inc_mem32, src)           -- inc dword[#xxxxxxxx]
                                if symtab[dest][S_NTyp]=S_TVar then
--DEV storeconstref(dest,src)?
                                    k = symtab[dest][S_Tidx]
                                    if X64=0 then
                                        k *= 4
                                    else
                                        k *= 8
                                    end if
                                    if k<-128 then
                                        emitHex10wconstref(mov_ebpd32_i32,k,src)-- mov [ebp-nnnn],#xxxxxxxx
                                    else -- (when k is 0 we still need a byte offset of 0)
                                        emitHex7d8constref(mov_ebpd8_i32,k,src) -- mov [ebp-nn],#xxxxxxxx
                                    end if
                                else
                                    emitHex10constref(mov_m32_imm32, dest, src) -- mov [dest],#xxxxxxxx
                                end if
                                clearMem(dest)

--24/5/21: (p2js special handling of {} as param default)
--/*
  11:  opTchk,1213,1,1162,                   opTchk,varno,wasOptTypeCheck,d_efault
 136:  opMkSq,0,1213,                        opMkSq,N,dest,eN..e1
symtab[630]:{-1,S_Const,3,(S_used+S_set+K_sqr+K_noclr+K_lit),0,171/#004022BC,T_Dsq,{}}
--symtab[1162]:{-1,S_TVar,0,(S_set),0,0,T_Dsq,{T_Dsq,MININT,MAXINT,object,2},[esp-12]}
symtab[1162]:{-1,S_Const,2,(S_used+S_set+K_sqr+K_noclr+K_lit),0,236/#0079DC88,T_Dsq,{}}
--if name=-1 and S_Const and K+noclr+K_lit and value={} then
--*/
                            elsif symtab[src][S_Name]=-1
                              and ssNTyp1=S_Const 
                              and and_bits(state1,K_noclr+K_lit)=K_noclr+K_lit
                              and sudt=T_Dsq
                              and symtab[src][S_value]={} then
                                leamov(eax,dest)
                                emitHex2s(xor_edx_edx)              -- mov edx,noofitems (0)
                                raoffset = emitHex5addr()           -- push <return addr> [backpatched below]
                                emitHex1(push_eax)                  -- push dest addr (leamov'd above)
                                loadToReg(edi,dest)                 -- mov edi,[dest] (prev)
                                emitHex5jmpG(opMkSq)
                                x86[raoffset] = length(x86)-raoffset
                                reginfo = 0 -- all regs trashed     -- <return addr>:

                            else -- K_noclr S_Const
                                loadToReg(reg,src)                      -- mov reg,[src]
                                storeReg(reg,dest,1,1)                  -- mov [dest],reg
                                backpatch = 0
                                if and_bits(slroot,T_integer) then
                                    cmp_h4(reg)                         -- cmp reg,h4
--DEVBPM backpatch me: [DONE]
--                                  emitHex6j(jl_rel32,9)               -- jl @f [sj NOT ok]
                                    emitHex6j(jl_rel32,0)               -- jl @f [sj NOT ok]
                                    backpatch = length(x86)
                                end if
                                sib = 0o203+reg*8 -- 0o2r3, ebx+reg*4
                                -- NB: the incref /CAN/ get an exception for unassigned pDefault, hence we
                                --     we emit the following helper instruction for pdiag.e. There is no 
                                --     penalty for doing this, incref is three clocks with or without it.
if newEmit then
                                if X64 then
                                    if reg>7 then ?9/0 end if -- (placeholder for more code)
                                    emitHex1(#48)
                                    emitHex5sib(addd_subd8i8,sib,-16,1) -- add qword[rbx+reg*4-16],1    ; incref
                                else
                                    emitHex5sib(addd_subd8i8,sib,-8,1)  -- add dword[ebx+reg*4-8],1     ; incref
                                end if
                                cmp_eax_srcid(src)                      -- cmp eax,src_id   (effectively a no-op)
else
                                emitHex4sib(incd_sib,sib,-8)            -- inc dword[ebx+reg*4-8]   ; incref
                                emitHex5w(cmp_eax_imm32,src)            -- cmp eax,src (effectively a no-op)
end if
                                if backpatch!=0 then
                                    x86[backpatch] = length(x86)-backpatch  -- @@:
                                end if
                                clearReg(reg)
                            end if -- K_noclr S_Const
                        end if
                    end if -- not isGscan
                    src = dest  -- unclobber (we put pDefault in it above)
                end if
--DEV (now done later)
--              if wasOptTypeCheck then
--                  -- emit a jump (else)
--                  emitHex5j(0)                                -- jmp xxx (backpatched later)
--              end if
--              x86[dbpos] = length(x86)-dbpos                  -- @@:
--              dbpos=length(x86)
            end if  -- pDefault
            getSrc()
--if pfileno=1 then printf(1,"pilx86.e line 7179 (opTchk): slroot=%04b (src=%d)\n",{slroot,src}) end if

            if wasOptTypeCheck then
                if sudt>T_object then
--18/9/15 no: follow the chain!
--                  dest = sudt+1
                    dest = symtab[sudt][S_Parm1]
                    -- sanity check: verify we have located a type routine's parameter:
--puts(1,"warning: pilx86.e line 7159, rudely removed code...\n")
                    if not and_bits(symtab[dest][S_State],K_type) then dest=9/0 end if
--                  if not and_bits(symtab[dest][S_State],K_type) then 
--?dest
--?symtab[dest]
--?symtab[dest][S_State]
--                      dest=9/0 end if
                    getDest()
                    gtypeonly = 1
                    storeDest()
--MARKTYPES
if MARKTYPES then ?9/0 end if
if NEWGSCAN then
--                  if isGscan
                    if isGscan
--                  if sudt>T_object then
                    and sudt>T_object then
                        symk = symtab[sudt]
                        u = symk[S_State]
                        if not and_bits(u,K_used) then
                            symtab[sudt] = 0        -- kill refcount (prevent pointless clone)
                            u += K_used
                            symk[S_State] = u
--erm...
                        end if
                            if symk[S_NTyp]!=S_Type then ?9/0 end if    -- sanity check
                            if sudt=vi then ?9/0 end if                 -- sanity check
                                                -- alternatively: linkup opTchks here.
                                                -- (I think you'd find that slower)
                            -- add to chain of routines to be processed:
----DEV temp!
--printf(1,"pilx86.e line 3369: S_linking symtab[%d]\n",vi)
--printf(1,"pilx86.e line 3370: S_linking symtab[%d]\n",routineNo)
--if NEWGSCAN then
                            if g_scan[sudt]=0 then
--if pfileno=1 then
if pfileno=1 and symk[S_FPno]=1 then
?{"pilx86.e line 7206 (opTchk), adding",sudt,vi}
end if
                                g_scan[sudt] = g_scan[vi]
                                g_scan[vi] = sudt
                            end if
--else
--                          symk[S_Slink] = symtab[vi][S_Slink]
--                          symtab[vi][S_Slink] = routineNo
--end if
                            symtab[sudt] = symk
--                      end if
                    end if
end if
                end if
                if NOLT=0 or bind or lint then
                    if pc>pcTchk then
                        Lmin = smin
                        Lmax = smax
                        Letyp = setyp
                        Lslen = slen
                        ltAdd(SET,src,sltype,sudt,pc)
                    end if
                end if -- NOLT
                if not isGscan then
                    symtabN = symtab[src]   -- (must be set before needstypecheck() call)
                    if sudt<=T_object then
                    -- builtin typechecks:
--/**/                  if not bind or needstypecheck(pc<pcTchk) then   --/*
                        if needstypecheck(pc<pcTchk) then   -- RDS version (avoids warning..)
--*/
                            if pDefault then
                                -- emit a jump (else)
                                emitHex5j(0)                                -- jmp xxx (backpatched later)
                                x86[dbpos] = length(x86)-dbpos              -- @@:
                                dbpos = length(x86)
                                -- (reg still set from above)
                                -- (a cmp h4/jne got us here)
                            else
                                reg = loadReg(src)                              -- mov reg,[src]
--                              if sched then
--                                  schedule(regbit[reg+1],0,0,pNP,1,0) -- treat as one big instruction!
--                              end if
--DEV if and_bits(dtype[not currently set!],T_integer) then
                                cmp_h4(reg)                                         -- cmp reg,h4
                            end if
                            if sudt=T_integer then
                                emitHex6j(jl_rel32,0)                           -- jl @f (ok) [sj NOT ok]
                            else
                                sib = #83+reg*8 -- 0o2r3, ebx+reg*4
                                if sudt=T_atom then
                                    emitHex6j(jl_rel32,0)                       -- jl @f (ok) [sj NOT ok]
                                    backpatch2 = length(x86)
                                    emitHex5sib(cmpb_sibd8i8,sib,-1,#12)        -- cmp byte[ebx+reg*4-1],0x12
                                    emitHex6j(je_rel32,0)                       -- je @f (ok) [sj NOT ok]
                                elsif sudt=T_sequence then
--DEVBPM backpatch me: [DONE]
--                                  emitHex6j(jl_rel32,11)                      -- jl (mov edx/call opTcFail) [sj NOT ok]
                                    emitHex6j(jl_rel32,0)                       -- jl (mov edx/call opTcFail) [sj NOT ok]
                                    backpatch2 = length(x86)
                                    emitHex5sib(tstb_sibd8i8,sib,-1,#80)        -- test byte[ebx+reg*4-1],0x80
                                    emitHex6j(jnz_rel32,0)                      -- jnz @f (ok) [sj NOT ok]
                                elsif sudt=T_string then
--DEVBPM backpatch me: [DONE]
--                                  emitHex6j(jl_rel32,11)                      -- jl (mov edx/call opTcFail) [sj NOT ok]
                                    emitHex6j(jl_rel32,0)                       -- jl (mov edx/call opTcFail) [sj NOT ok]
                                    backpatch2 = length(x86)
                                    emitHex5sib(cmpb_sibd8i8,sib,-1,#82)        -- cmp byte[ebx+reg*4-1],0x82
                                    emitHex6j(je_rel32,0)                       -- je @f (ok) [sj NOT ok]
                                else ?9/0 -- sudt=T_object??
                                end if
                            end if
                            backpatch = length(x86)
                            if sudt>T_atom then
                                x86[backpatch2] = length(x86)-backpatch2
                            end if
                            movRegVno(ecx,src)                              -- mov e/rcx,varno of src
                            emitHex5callG(opTcFail)
                            -- "" fatal error, does not return              -- @@:
                            x86[backpatch] = length(x86)-backpatch
--                          if sudt=T_atom then
--DEVBPM backpatch me: [DONE]
--                          if sudt!=T_integer then
                            if sudt=T_atom then
                                x86[backpatch2] = length(x86)-backpatch2
                            end if
                        end if -- needstypecheck()
                    else -- udt
                        if pDefault then
                            -- emit a jump (else)
                            emitHex5j(0)                                    -- jmp xxx (backpatched later)
                            x86[dbpos] = length(x86)-dbpos                  -- @@:
                            dbpos=length(x86)
                        end if
--; mov ecx,(p3-p2)/4   ; noofparams/locals/tmps    271         B9 imm32        mov ecx,imm32
--; mov edx,N           ; type check routineNo      277         BF imm32        mov edi,imm32
--; push <return addr>                              150         68 imm32        push imm32
--; push <varno>        ; var number                150         68 imm32        push imm32
--; push <code>         ; addr of typecheck code    150         68 imm32        push imm32
--;                     ; (aka symtab[tcr][S_il])
--; push dword[var]     ; value to check            377 065     FF 35 mem32     push dword[mem32]
--; jmp opTchk                                      351         E9 rel32        jmp rel32
                        symk = symtab[sudt] -- type check routine symtab entry
--                      if sched then
--                          schedule(0,0,ecxbit,pUV,0,0)
--                      end if
                        k = symk[S_Ltot]
--                      emitHex5w(mov_ecx_imm32,k)                          -- mov ecx,no of items to save
                        movRegImm32(ecx,k)                                  -- mov ecx,no of items to save
--                      if sched then
--                          schedule(0,0,edxbit,pUV,0,src)
--                      end if
if newEmit then
                        movRegVno(edx,sudt)                                 -- mov e/rdx,routineNo
else
                        emitHex5w(mov_edx_imm32,sudt)                       -- mov edx,routineNo
end if

--                      if sched then
--                          schend()
--                      end if
                        backpatch = emitHex5addr()                          -- push <return addr>
--if newEmit then
                        emitHex5vno(push_imm32,src)                         -- push var no
--else
--                      if src>=-128 and src<=127 then
--                          emitHex2(push_imm8,src)
--                      else
--                          emitHex5w(push_imm32,src)                       -- push var no
--                      end if
--end if
--                      emitHex5v(push_imm32,sudt-1)                        -- push addr typecheck res
                        if q86>1 then
--  ?"9/0 (x86 4185)\n" -- separate chain for isIL[a]?
                            x86 &= push_imm32
                            quad2(isILa,sudt)
                        else
                            h5 = {push_imm32,isILa,0,0,sudt}                -- (keeps that isginfo happy)
                            emitHex5s(h5)                                   -- push addr typecheck code
                        end if
                        pushvar(src)                                        -- push dword[src]
                        emitHex5jmpG(opTchk)
                        x86[backpatch] = length(x86)-backpatch
                        -- NB src obtained from [esp]-29 on error
                        reginfo = 0
                        -- all regs trashed
                        symk = {}                                           -- <return addr>:
                    end if
                end if -- not isGscan
            end if -- wasOptTypeCheck
            if not isGscan then
--              if pDefault and wasOptTypeCheck then
                if pDefault then
                    -- patch dbpos to here
                    x86[dbpos] = length(x86)-dbpos                                  -- @@:
                end if
            end if
            pc += 4

        elsif opcode=opRetf             -- 14
           or opcode=opBadRetf then     -- 15
            if isGscan then
                pc += 1
                if pc>length(s5) then exit end if
            else
--              if sched then
--                  schend()
--              end if
                if opcode=opRetf then
                    -- 7/12/15: (mt program)
--                  if lastline=-1 then
--                  if lastline=-1 and currRtn=T_maintls then
                    if lastline=-1 then
                        if currRtn=T_maintls then
--?symtab[currRtn][S_1stl]
--                      emitline = -1
    lastline = 0
    emitline = 1
                        else
--added 15/1/17 (mt one-line procedure (actually used), eg "procedure text_mode() end procedure" (no \n)):
    lastline = 0
    emitline = symtab[currRtn][S_1stl]
                        end if
                    end if
                    emitHex5jmpG(opRetf)
                    pc += 1
                    --
                    -- if an opRetf is immediately followed by an
                    --  opBadRetf (ie no opLabel, so it must be
                    --  unreachable) then skip it. Obviously the
                    --  one thing we need to avoid (in pmain.e)
                    --  is clobbering something like:
                    --              return
                    --          end if
                    --      end function
                    -- which might carry on executing the next
                    --  block of code rather than terminating
                    --  with an appropriate error message.
                    --
                    if pc<=length(s5) and s5[pc]=opBadRetf then
                        pc += 1
                    end if
                    if pc>length(s5) then exit end if
                else
                    emitHex5callG(opBadRetf)    -- call :%opBadRetf
                    if pc<length(s5) then ?9/0 end if
                    exit
                end if
            end if

--      elsif opcode>=opJge             -- 28
--        and opcode<=opJle then        -- 33
        elsif opcode=opJge              -- 28
           or opcode=opJlt              -- 29
           or opcode=opJeq              -- 30
           or opcode=opJne              -- 31
           or opcode=opJgt              -- 32
           or opcode=opJle then         -- 33
--  opJge = 28, opJlt = 29, opJeq = 30, opJne = 31, opJgt = 32, opJle = 33
            -- opJcc,mergeSet,tgt,link,p2,p3,tii,bothInit
--if not isGscan then
-- if pc=439 then
--  trace(1)
-- end if
--end if
--20/4/19:
if 0 then   -- new, failed...
--          src = 0
--          src2 = 0
            src = s5[pc+4]
            src2 = s5[pc+5]
            waspc = pc
            tii = s5[pc+6]  -- twoInitialisedInts   [re-check with gvar]
            bothInit = s5[pc+7]
--if pfileno=1 then ?{isGscan,symtab[src]} end if

            if isGscan then
                -- if bothInit and not tii, yet analysis reveals p2,p3 are in fact ints then tii=1:
--              tii = s5[pc+6]  -- twoInitialisedInts   [re-check with gvar]
                if not tii then
--                  isInit = s5[pc+7]   -- both Init            (for """")
--                  if isInit then
                    if bothInit then
--                  dest = -1   -- avoid any mimicry
--                      src = s5[pc+4]
                        getSrc()
--if tmpd then
--  trace(1)
--end if
                        if opcode=opJeq or opcode=opJne then
                            -- either will do
--DEV do we want to be using rootType here?? (*4)
                            if slroot=T_integer then
--                              s5[pc+6] = 1    -- tii
                                tii = 1
                            else
--                              src2 = s5[pc+5]
                                getSrc2()
                                if slroot2=T_integer then
--                                  s5[pc+6] = 1    -- tii
                                    tii = 1
                                end if
                            end if
                        elsif slroot=T_integer then
                            -- need both
--                          src2 = s5[pc+5]
                            getSrc2()
                            if slroot2=T_integer then
--                              s5[pc+6] = 1    -- tii
                                tii = 1
                            end if
                        end if
                        s5[pc+6] = tii
                    end if
                end if
if NEWGSCAN then
--if pfileno=1 then ?{"pilx86.e line 7473, pc=",pc} end if
                jinit(pc+1) -- DEV param can go, I think
                bcode = find(opcode,Bcde)
                if tmpd then ?9/0 end if    -- bug in transtmpfer?
                if tii or bothInit then
--if pfileno=1 then trace(1) end if
                    if SetCC(Scde[bcode],-1) then end if -- but no common setup, use the opcode instead
                else
                    src = 0
                    src2 = 0
                end if

--if pfileno=1 then ?{"pilx86.e line 7478, tmpd=",tmpd} end if
--                  ?9/0
--              else
--                  ?9/0
--              end if
                -- Jcc:
                if jend(8) then exit end if -- an opRetf fell off end of code, --> end while
--              tmpd = 0 (done by above)
--/*
                pc += 8
--if pfileno=1 then ?{"pilx86.e line 7487, pc=",pc} end if
                if tmpd then
                    tmpd = 0
                    if tmpv then    -- always taken:
                        if mergeSet=isOpCode then
                            tgt = -1    -- let jskip fall off end of code
--                          if jskip() then return 1 end if -- if "", all done --> end while
--                      else
--                          if jskip() then
--                              opcode = lastop
--                              detach(tgt,npc+3)
--                          end if
                        end if
                        if jskip() then end if
                    end if
                end if
--if pfileno=1 then ?{"pilx86.e line 7503, pc=",pc} end if
--*/
else
                pc += 8
end if
            else -- not isGscan
                --if opcode=opJeq then trace(1) end if
                -- Jcc,mergeSet,tgt,link,src,src2,tii,bothInit
--              src = s5[pc+4]
--              src2 = s5[pc+5]
--if vi=1525 and src=303 and src2=1521 then
--?symtab[src]
--?symtab[src2]
--  trace(1)
--end if
--              tii = s5[pc+6]
--              bothInit = s5[pc+7] -- (used in pgscan)
--if pfileno=1 then ?{"pilx86.e line 7522, pc=",pc} end if
                jinit(pc+1) -- DEV param can go, I think
                bcode = find(opcode,Bcde)
                if tii then         -- twoInitInts (Jne/Jeq do EITHER, rest need BOTH).
                    -- if result predictable set tmpv to 0/1 and tmpd to -1
                    k = 0
--                  if sched then
--                      if mergeSet!=isOpCode then
--                          k = lastJmp -- equivalent to schoon or 0 (can schedule non-last jumps)
--                      end if
--                  end if
                    if SetCC(Scde[bcode],k) then
                        -- we must do a compare:
                        mod = m_cmp
                        if tmpd=-1 then
                            regimm365(smin2)                            -- cmp reg,imm
                        else
--                          if sched then
--                              rb = regbit[reg+1]
--                              rw = regbit[wrk+1]
--                              schedule(rb+rw,0,0,pUV,1,0)
--                          end if
                            if X64 then
                                emitHex1(#48)
                            end if
                            op1 = mod+1         -- 0o071
--if vi=1525 then
--  ?{wrk,reg,1525}
--end if
--                          if wrk=reg then ?9/0 end if -- sanity check (added 31/8/15)
if wrk=reg then
    printf(1,"wrk=reg! line 7465 pilx86.e, (emitline=%d, %s)\n",{emitline,filenames[symtab[vi][S_FPno]][2]})
end if
                            xrm = #C0+wrk*8+reg -- 0o3wr
                            emitHex2(op1,xrm)                           -- cmp reg,wrk
                        end if
                        tmpd = 0
                    end if -- not setCC (compare not rqd, tmpd==-1)
                else -- not twoInitialisedInts
                    if tmpd then ?9/0 end if    -- bug in transtmpfer?
-- 21/2/09:
                    if bothInit then
                        -- if result predictable set tmpv to 0/1 and tmpd to -1
                        if SetCC(Scde[bcode],-1) then end if -- but no common setup, use the opcode instead
                    end if
                    if not tmpd then
--                      if sched then
----                    if schidx then
--                          schend()    -- [esp]-15/-9
----                    end if
--                      end if
if not bothInit then
    getSrc()
    getSrc2()
end if
-- 3/10/10: (breaks p t18 and p p t13)
--if bind and ssNTyp2=S_Const and and_bits(state2,K_noclr) then
if ssNTyp2=S_Const and and_bits(state2,K_noclr) then
--if 0 then
        if slroot2=T_integer then
            if and_bits(state2,K_rtn) then ?9/0 end if
            if smin2!=smax2 then ?9/0 end if
            if smin2 then
--                      emitHex5w(mov_edi_imm32,smin2)              -- mov edi,imm32
                        movRegImm32(edi,smin2)                      -- mov edi,imm32
            else
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(xor_edi_edi)                      -- xor edi,edi
            end if
        else
if newEmit then
                        loadToReg(edi,src2,CLEAR)                   -- mov edi,[src2]
else
                        emitHex5cr(mov_edi_imm32,src2)              -- mov edi,constref
end if
        end if
                        clearReg(edi)
--                      doNotXor = 0
else
                        loadToReg(edi,src2,CLEAR)                   -- mov edi,[src2]
                        --
                        -- ensure src2 can be had from [esp]-14 in this (not bothInit) case.
                        --  (really only matters whether src2 is init, but close enough...)
                        --
--DEV this can now go...
--                      doNotXor = not bothInit
end if
-- 3/10/10: (breaks t18)
--if bind and ssNTyp1=S_Const and and_bits(state1,K_noclr) then
if ssNTyp1=S_Const and and_bits(state1,K_noclr) then
--if 0 then
        if slroot=T_integer then
            if and_bits(state1,K_rtn) then ?9/0 end if
            if smin!=smax then ?9/0 end if
--          if smin or doNotXor then
            if smin then
--                      emitHex5w(mov_eax_imm32,smin)               -- mov eax,imm32
                        movRegImm32(eax,smin)                       -- mov eax,imm32
            else
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(xor_eax_eax)                      -- xor eax,eax
            end if
        else
if newEmit then
                        loadToReg(eax,src)                          -- mov eax,[src]
else
                        emitHex5cr(mov_eax_imm32,src)               -- mov eax,constref
end if
        end if
else -- not S_Const, K_noclr
                        loadToReg(eax,src)                          -- mov eax,[src]
end if
                        movRegVno(esi,src2)                         -- mov e/rsi,src2 (var no)
                        movRegVno(edx,src)                          -- mov e/rdx,src (var no)
                        emitHex5callG(opcode)                       -- call :%opJcc/JccE
                        -- NB: src/2 obtained from [esp]-9/-14 on error.
                        reginfo = 0 -- all regs trashed
                    end if
--DEV this should be else'd, I think...
                    if not bothInit then
                        src = 0
                        src2 = 0
                    end if

                end if
--if pfileno=1 then ?{"pilx86.e line 7651, pc=",pc} end if
                -- Jcc:
                if jend(8) then exit end if -- an opRetf fell off end of code, --> end while
--if pfileno=1 then ?{"pilx86.e line 7654, pc=",pc} end if
            end if -- (not isGscan)

if NOLT=0 or bind or lint then
            if opcode=opJne then
--trace(1)
                --
                -- In say 'if h=4 then', since p2 is an integer, p1 must be one too
                -- Naturally a similar thing is due for 'if name="pete" then', also
                -- '4=h' and '"pete"=name' affecting p2 from p1, plus 'if a=b then'
                -- possibly altering types of both p1 and p2, with any and all such
                -- settings being undone at the appropriate opLabel.
                --
                -- Now if h is defined as an atom or an object, then 'integer' is a
                -- better thing, whereas should h be say the udt 'hour', then leave
                -- it alone (since not calling hour far outweighs a few cmp h4).
                --
                if src=0 then
                    src = s5[waspc+4]
                    getSrc()
                end if
                if src2=0 then
                    src2 = s5[waspc+5]
                    getSrc2()
                end if
                if sltype!=sltype2
                and sltype<=T_object
                and sltype2<=T_object then
                    lmask = and_bits(sltype,sltype2)
                    if and_bits(sltype,T_sequence)      -- Since {'a'}=="a", convert
                    and and_bits(sltype2,T_sequence) then -- any combo of 01xx/10xx/11xx
                        lmask = or_bits(lmask,T_sequence)   -- ==> 11xx
                    end if
                    ltype = and_bits(sltype,lmask)
                    if ltype and sltype!=ltype then
                        Lmin = smin
                        Lmax = smax
                        Letyp = setyp
                        Lslen = slen
                        ltAdd(TEST,src,sltype,ltype,waspc)
                    end if
                    ltype = and_bits(sltype2,lmask)
                    if ltype and sltype2!=ltype then
                        Lmin = smin2
                        Lmax = smax2
                        Letyp = setyp2
                        Lslen = slen2
                        ltAdd(TEST,src2,sltype2,ltype,waspc)
                    end if
                end if
            end if -- opJne
end if -- NOLT
else -- 20/4/19
            src = 0
            src2 = 0
            waspc = pc
--if fileno=1 then
--  ?{opcode,isGscan,emitline}
--if fileno=1 and emitline=16 then
--  trace(1)
--end if

            if isGscan then
                -- if bothInit and not tii, yet analysis reveals p2,p3 are in fact ints then tii=1:
                tii = s5[pc+6]  -- twoInitialisedInts   [re-check with gvar]
                if not tii then
                    isInit = s5[pc+7]   -- both Init            (for """")
                    if isInit then
--                  dest = -1   -- avoid any mimicry
                        src = s5[pc+4]
                        getSrc()
--if tmpd then
--  trace(1)
--end if
                        if opcode=opJeq or opcode=opJne then
                        -- either will do
--DEV do we want to be using rootType here?? (*4)
                            if slroot=T_integer then
                                s5[pc+6] = 1    -- tii
                            else
                                src2 = s5[pc+5]
                                getSrc2()
                                if slroot2=T_integer then
                                    s5[pc+6] = 1    -- tii
                                end if
                            end if
                        elsif slroot=T_integer then
                            -- need both
                            src2 = s5[pc+5]
                            getSrc2()
                            if slroot2=T_integer then
                                s5[pc+6] = 1    -- tii
                            end if
                        end if
                    end if
                end if
                pc += 8
            else -- not isGscan
                --if opcode=opJeq then trace(1) end if
                -- Jcc,mergeSet,tgt,link,src,src2,tii,bothInit
                src = s5[pc+4]
                src2 = s5[pc+5]
--if vi=1525 and src=303 and src2=1521 then
--?symtab[src]
--?symtab[src2]
--  trace(1)
--end if
                tii = s5[pc+6]
                bothInit = s5[pc+7] -- (used in pgscan)
                jinit(pc+1) -- DEV param can go, I think
                bcode = find(opcode,Bcde)
                if tii then         -- twoInitInts (Jne/Jeq do EITHER, rest need BOTH).
                    -- if result predictable set tmpv to 0/1 and tmpd to -1
                    k = 0
--                  if sched then
--                      if mergeSet!=isOpCode then
--                          k = lastJmp -- equivalent to schoon or 0 (can schedule non-last jumps)
--                      end if
--                  end if
                    if SetCC(Scde[bcode],k) then
                        -- we must do a compare:
                        mod = m_cmp
                        if tmpd=-1 then
                            regimm365(smin2)                            -- cmp reg,imm
                        else
--                          if sched then
--                              rb = regbit[reg+1]
--                              rw = regbit[wrk+1]
--                              schedule(rb+rw,0,0,pUV,1,0)
--                          end if
                            if X64 then
                                emitHex1(#48)
                            end if
                            op1 = mod+1         -- 0o071
--if vi=1525 then
--  ?{wrk,reg,1525}
--end if
--                          if wrk=reg then ?9/0 end if -- sanity check (added 31/8/15)
if wrk=reg then
    printf(1,"wrk=reg! line 7465 pilx86.e, (emitline=%d, %s)\n",{emitline,filenames[symtab[vi][S_FPno]][2]})
end if
                            xrm = #C0+wrk*8+reg -- 0o3wr
                            emitHex2(op1,xrm)                           -- cmp reg,wrk
                        end if
                        tmpd = 0
                    end if -- not setCC (compare not rqd, tmpd==-1)
                else -- not twoInitialisedInts
                    if tmpd then ?9/0 end if    -- bug in transtmpfer?
-- 21/2/09:
                    if bothInit then
                        -- if result predictable set tmpv to 0/1 and tmpd to -1
                        if SetCC(Scde[bcode],-1) then end if -- but no common setup, use the opcode instead
                    end if
                    if not tmpd then
--                      if sched then
----                    if schidx then
--                          schend()    -- [esp]-15/-9
----                    end if
--                      end if
if not bothInit then
    getSrc()
    getSrc2()
end if
-- 3/10/10: (breaks p t18 and p p t13)
--if bind and ssNTyp2=S_Const and and_bits(state2,K_noclr) then
if ssNTyp2=S_Const and and_bits(state2,K_noclr) then
--if 0 then
        if slroot2=T_integer then
            if and_bits(state2,K_rtn) then ?9/0 end if
            if smin2!=smax2 then ?9/0 end if
            if smin2 then
--                      emitHex5w(mov_edi_imm32,smin2)              -- mov edi,imm32
                        movRegImm32(edi,smin2)                      -- mov edi,imm32
            else
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(xor_edi_edi)                      -- xor edi,edi
            end if
        else
if newEmit then
                        loadToReg(edi,src2,CLEAR)                   -- mov edi,[src2]
else
                        emitHex5cr(mov_edi_imm32,src2)              -- mov edi,constref
end if
        end if
                        clearReg(edi)
--                      doNotXor = 0
else
                        loadToReg(edi,src2,CLEAR)                   -- mov edi,[src2]
                        --
                        -- ensure src2 can be had from [esp]-14 in this (not bothInit) case.
                        --  (really only matters whether src2 is init, but close enough...)
                        --
--DEV this can now go...
--                      doNotXor = not bothInit
end if
-- 3/10/10: (breaks t18)
--if bind and ssNTyp1=S_Const and and_bits(state1,K_noclr) then
if ssNTyp1=S_Const and and_bits(state1,K_noclr) then
--if 0 then
        if slroot=T_integer then
            if and_bits(state1,K_rtn) then ?9/0 end if
            if smin!=smax then ?9/0 end if
--          if smin or doNotXor then
            if smin then
--                      emitHex5w(mov_eax_imm32,smin)               -- mov eax,imm32
                        movRegImm32(eax,smin)                       -- mov eax,imm32
            else
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(xor_eax_eax)                      -- xor eax,eax
            end if
        else
if newEmit then
                        loadToReg(eax,src)                          -- mov eax,[src]
else
                        emitHex5cr(mov_eax_imm32,src)               -- mov eax,constref
end if
        end if
else -- not S_Const, K_noclr
                        loadToReg(eax,src)                          -- mov eax,[src]
end if
                        movRegVno(esi,src2)                         -- mov e/rsi,src2 (var no)
                        movRegVno(edx,src)                          -- mov e/rdx,src (var no)
                        emitHex5callG(opcode)                       -- call :%opJcc/JccE
                        -- NB: src/2 obtained from [esp]-9/-14 on error.
                        reginfo = 0 -- all regs trashed
                    end if
--DEV this should be else'd, I think...
                    if not bothInit then
                        src = 0
                        src2 = 0
                    end if

                end if
                -- Jcc:
                if jend(8) then exit end if -- an opRetf fell off end of code, --> end while
            end if

if NOLT=0 or bind or lint then
            if opcode=opJne then
--trace(1)
                --
                -- In say 'if h=4 then', since p2 is an integer, p1 must be one too
                -- Naturally a similar thing is due for 'if name="pete" then', also
                -- '4=h' and '"pete"=name' affecting p2 from p1, plus 'if a=b then'
                -- possibly altering types of both p1 and p2, with any and all such
                -- settings being undone at the appropriate opLabel.
                --
                -- Now if h is defined as an atom or an object, then 'integer' is a
                -- better thing, whereas should h be say the udt 'hour', then leave
                -- it alone (since not calling hour far outweighs a few cmp h4).
                --
                if src=0 then
                    src = s5[waspc+4]
                    getSrc()
                end if
                if src2=0 then
                    src2 = s5[waspc+5]
                    getSrc2()
                end if
                if sltype!=sltype2
                and sltype<=T_object
                and sltype2<=T_object then
                    lmask = and_bits(sltype,sltype2)
                    if and_bits(sltype,T_sequence)      -- Since {'a'}=="a", convert
                    and and_bits(sltype2,T_sequence) then -- any combo of 01xx/10xx/11xx
                        lmask = or_bits(lmask,T_sequence)   -- ==> 11xx
                    end if
                    ltype = and_bits(sltype,lmask)
                    if ltype and sltype!=ltype then
                        Lmin = smin
                        Lmax = smax
                        Letyp = setyp
                        Lslen = slen
                        ltAdd(TEST,src,sltype,ltype,waspc)
                    end if
                    ltype = and_bits(sltype2,lmask)
                    if ltype and sltype2!=ltype then
                        Lmin = smin2
                        Lmax = smax2
                        Letyp = setyp2
                        Lslen = slen2
                        ltAdd(TEST,src2,sltype2,ltype,waspc)
                    end if
                end if
            end if -- opJne
end if -- NOLT

end if -- 20/4/19

        elsif opcode=opJmp then

            if isGscan then
                pc += 4
            else
--trace(1)
                -- opJmp,mergeSet,tgt,link
                mergeSet = s5[pc+1]
                tgt = s5[pc+2]      -- (an opLabel's link field)
--              link = s5[pc+3]     -- (prev jmp to same tgt, or 0; used by opLabel)
                if DEBUG then
                    if s5[tgt-3]!=opLabel then ?9/0 end if
                    if s5[tgt-2]!=mergeSet then ?9/0 end if
                end if
                if tgt<pc then  -- a backward jump
                    if DEBUG then
                        if mergeSet then ?9/0 end if    -- expecting 0 from end while
                    end if
                    -- If other opJxx hit tgt<pc (eg a repeat... until <condition>
                    --  construct is added to the language), this code can be used.
                    --  The same thing occurs in opEndFor (another bkwd jmp), btw.
                    --   (btw, not sure about detach() for new constructs...)
--                  if sched then
----                if schidx then schend() end if
--                      schend()
--                  end if
                    tgt = s5[tgt-1] -- saved x86 offset (see opLabel in this source)
--added 13/1/14:
--  if not sched then
                    if lastline!=emitline then lineinfo() end if
--  end if

--DEVBPM backpatch me: [DONE]
if newEmit then
                    emitHex5j(0)
                    backpatch = length(x86)
                    x86[backpatch] = tgt-backpatch
else
                    joffset = tgt-(length(x86)+5)
--if q86 then ?9/0 end if -- search for h5
--                  h5 = {jump_rel32,isJmp,0,0,joffset} -- (keeps ginfo happy)
--                  emitHex5s(h5)       -- jmp xxx
                    emitHex5j(joffset)
end if
                    pc += 4
                else
                    tmpd = 1    -- we know
                    tmpv = 1    -- always taken
                    -- Jmp:
                    if jend(4) then ?9/0 end if -- (there should be no jmp opRetf to fall off end of code!)
                end if
            end if

        elsif opcode=opSubse1           -- 23   res := s[idx], when res is object
           or opcode=opSubse1i          -- 24   res := s[idx], when res is integer
           or opcode=opSubse1is then    -- 25   res := s[idx], "" and s is string
--         or opcode=opSubse1ip then    -- 160  res := s[idx], "" and s is sequence of integer
            --                          (NB opSubse1ip is only selected here, never in pmain.e)
            --
            -- opSubse1 is dest, src, idx, isInit[src and idx]:
            --  implements dest:=src[idx], aka dest=src[src2].
            --  accepts transtmpfer() for idx/src2 only.
            --
--if not isGscan then
--trace(1)
--end if
            dest = s5[pc+1]     -- res
            src = s5[pc+2]      -- ref/p1
            getDest()
--if isGscan then
--          dest = -1   -- avoid any mimicry
--DEV/NB: dest=dest[idx] cannot take part info (unlike append/prepend/concat[N])
            getSrc()
            src2 = s5[pc+3]     -- idx
            isInit = s5[pc+4]   -- (ref and idx): 3=both, 1=p1 only, 2=p2 only, 0=neither
--          isCompound = s5[pc+5]   -- eg s[i] += x, used in/loaded by transtmpfer().
            pc += 6
            if opcode=opSubse1is        -- int:=string[idx]
            or and_bits(slroot,T_sequence)=T_string then
                smin = 0
                smax = 255
            else
-- newEmit/X64=1?
                smin = MININT
                smax = MAXINT
            end if
            sltype = setyp
            setyp = and_bits(setyp,T_sequence)
            if setyp=T_string then
                -- s is a sequence of string(/flt/int), so when
                --  dest is sequence, all elements/chars are int:
                setyp = T_integer
            elsif setyp then -- T_Dsq or T_Dsq|T_string
                -- if s[i] may be Dsq, then dest's elements may
                --  be absolutely anything:
                setyp = T_object
            else
                -- s is a sequence of atoms, hence dest is atom,
                --  with therefore no meaningful element type:
                setyp = 0
            end if
            --DEV 19/8:
            if opcode=opSubse1 then
                if vroot>T_object then ?9/0 end if
--DEV 14/2/10:
--              slroot = and_bits(sltype,vroot)
                slroot = sltype
            else
                slroot = T_integer
            end if
            slen = -2
            storeDest()

            --if isGscan then
            --          pc += 6
            --else
            ----            dest = s5[pc+1] -- res
            ----            src = s5[pc+2]  -- s
            ----            src2 = s5[pc+3] -- idx
            ----            isInit = s5[pc+4] -- (s and idx)
            ----            isCompound = s5[pc+5]   -- eg s[i+j] += k, used in/loaded by transtmpfer().
            --          pc += 6
            if not isGscan then
                markConstUseds({dest,src,src2})
--DEV 11/12/09:
--              if dtype=T_integer then     -- result is integer
                if vroot=T_integer then     -- result is integer
                    getSrc()    --DEV already done above??
                    getSrc2()
--DEV K_rtn??
                    if tmpd then
                        if tmpd!=src2 then ?9/0 end if
                        if tmpr<0 then
                            smin2=tmpv
                            smax2=tmpv
                        end if
                    end if
                    if slroot=T_string then
                        opcode = opSubse1is
                    elsif slroot=T_Dsq and setyp=T_integer then
                        opcode = opSubse1ip
                    elsif opcode=opSubse1
                      and not and_bits(slroot,T_atom) then  -- nb opSubse1i chokes on float/int refs
                        opcode = opSubse1i
                    end if
                    -- inline only if we are certain that both s and idx are init,
                    --  and no bounds check is required (and not -ve/f.p. idx)
-- 31/3/2012: p2(idx) is opUnassigned
--                  if isInit=3 and slroot2=T_integer and smin2>0 and smax2<=slen then
                    if and_bits(isInit,1) and slroot2=T_integer and smin2>0 and smax2<=slen then
                        idx = -1
                        if tmpd then
                            if tmpr>=0 then
                                idx = tmpr
                                promoteReg(idx) -- prevent loadReg(src) clobbering our idx register
--                      else
--                          (smin2,smax2 already set above)
                            end if
                        end if
                        if opcode=opSubse1ip then
                            if idx=-1               -- not already set
                            and smin2!=smax2 then   -- not a fixed value
                                idx = loadReg(src2,CLEAR) -- idx (if we can't use a fixed literal offset)
                            end if
                            reg = loadReg(src)      -- mov reg,[s]
                            if idx!=-1 then
                                -- Desired value is at [reg*4+idx*4-4], which can be done 3 ways:
                                --  1: reg+=idx;    mov reg,[ebx+reg*4-4]
                                --  2: idx+=reg;    mov idx,[ebx+idx*4-4]
                                --  3: res=idx+reg; mov res,[ebx+res*4-4]
                                -- since 3 uses more registers and adds an extra clock/dependency,
                                -- and "s" is more likely to be reused than "idx", we go option 2:
                                -- 64bit:
                                --  2: idx*=8;      mov idx,[idx+reg*4-8]
--                              if sched then
--                                  rb = regbit[reg+1]
--                                  rx = regbit[idx+1]
--                                  schedule(rb+rx,0,rx,pUV,1,0)
--                              end if
if X64 then
--printf(1,"checkme: line 7635 pilx86.e, (emitline=%d, %s)\n",{emitline,filenames[symtab[vi][S_FPno]][2]})
                                xrm = 0o340+idx
                                emitHex4l(#48,#C1,xrm,3)                -- shl idx,3
                                sibLoad743(idx,by4,reg,idx,-8)          -- mov idx,[idx+reg*4-8]        [AGI..]
else
                                k = idx*8           -- 0o0i0
                                xrm = #C0+k+reg     -- 0o3ir
--(#03 = m_add+3):
                                emitHexx2(#03,xrm)                      -- add idx,reg                  dependency?
                                sibLoad743(idx,by4,idx,ebx,-4)          -- mov idx,[ebx+idx*4-4]        [AGI..]
end if
                            else
                                -- Desired value is at [reg*4+smin2*4-4]. We may even avoid AGI,
                                --  if s/reg was already loaded. Use idx as res to match above.
                                idx = spareReg(1)   -- for result
--DEV 12/10:
--                                  idx = spareReg(0)   -- for result
if X64 then
                                smin2 = (smin2-1)*8
-- removed 20/8/15:
--                                  emitHex1(#48)
                                sibLoad743(idx,by4,reg,ebx,smin2)       -- mov idx,[ebx+reg*4+smin2]
else
                                smin2 = (smin2-1)*4
                                sibLoad743(idx,by4,reg,ebx,smin2)       -- mov idx,[ebx+reg*4+smin2]
end if
                            end if
                            tmpd = 0
                            if dname=-1 then
                                transtmpfer(0,idx)
                            end if
                            if dest then
                                storeReg(idx,dest,1,0)                  -- mov [dest],idx
--12/10:
                                if dname!=-1 then
                                    transtmpfer(0,idx)
                                end if
                                dest = 0    -- flag code as inlined
                            end if
                        elsif opcode=opSubse1is then
                            if idx=-1               -- not already set
                            and smin2!=smax2 then   -- not a fixed value
                                idx = loadReg(src2) -- idx (if we can't use a fixed literal offset)
                            end if
                            reg = loadReg(src)      -- s
--                          if sched then
--                              rb = regbit[reg+1]
--                              schedule(0,0,edxbit,pUV,1,0)
--                          end if
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(xor_edx_edx)                          -- xor edx,edx
                            if idx!=-1 then
                                -- nb idx is used as base below
                                smin2 = -1
                            else
                                smin2 -= 1
                                idx = ebx   -- the usual 0 base
                            end if
                            sibLoad743(dl,by4,reg,idx,smin2)                -- mov dl,[base+reg*4+smin2]
                            tmpd = 0
--3/4/17: (over switch s[idx] do)
--                          if dname=-1 then        -- a temp
--                              transtmpfer(0,edx)
--                              -- (sch00nOK)
--                          end if
--                          if dest then
                                storeReg(edx,dest,1,0)                      -- mov [dest],edx
--12/10
--                              if dname!=-1 then
                                    transtmpfer(0,edx)
--                              end if
                                dest = 0    -- flag code as inlined
--                          end if
                        end if
                    end if
                end if
-- opSubse1 calling convention:
--; mov edi,[src2]      ; idx                       213 075     8B 3D m32       mov edi,[m32]   u 7
--; mov ecx,dest        ; addr res                  271         B9 imm32        mov ecx,imm32   u 7
--; mov esi,[src]       ; s                         213 065     8B 35 m32       mov esi,[m32]   v 8
--; call opSubse1       ; [ecx]:=s[idx]             350         E8 rel32        call rel32      v 8
--; NB: src/2 obtained from [esp]-9/-20 on error
--; (opcode deallocates previous content of [ecx] as&when rqd)
-- opSubse1i/ip/is calling convention:
--; mov edi,[src2]      ; idx                       213 075     8B 3D m32       mov edi,[m32]   u 7
--; mov esi,[src]       ; s                         213 065     8B 35 m32       mov esi,[m32]   v 8
--; call opSubse1i[p|s] ; eax:=s[idx]               350         E8 rel32        call rel32      v 8
--; NB: src/2 obtained from [esp]-9/-15 on error
--; opSubse1i contains builtin type-check (that result/eax is 31-bit integer), and gets dest from
--  [esp]+1 on error (similar is /not/ needed for p/s, we simply *know* this.)
--; mov [dest],eax      ; res                       271         A3 imm32        mov ecx,imm32   u 7
-- all regs trashed, unless result is integer, in eax (and [dest])
                if dest then    -- not inlined above
--                  if sched then
--              --DEV there may be some scheduling possibilities here, especially for init idx, res addr.
----                if schidx then schend() end if
--                      schend()
--                  end if
                    --DEV if idx already in a register, use that.
                    --  Naively, for eg r=s[i+1], we might be tempted to load s/dest before idx to prevent a
                    --  memory dependency but better would be transtmpfer/some (new) variant of loadReg, and
                    --  leave idx first for better handling of "we know it is not <no value>" cases, iyswim.
                    -- DEV this may not be worth doing (makes -list output look better tho)
                    if tmpd then
                        if tmpr>=0 then
                            if tmpr!=edi then
--DEV is this right?/use mov_reg (ok, that would be mov_reg,0o3t7, no matter)
                                xrm = 0o370+tmpr -- 0o37t
                                emitHexx2(mov_dword,xrm)            -- mov edi,tmpr
                            end if
                        else
--                          emitHex5w(mov_edi_imm32,tmpv)           -- mov edi,imm32
                            movRegImm32(edi,tmpv)                   -- mov edi,imm32
                        end if
                        tmpd = 0
                    else
--DEV breaks self host:
--                  getSrc2()
--                  if slroot2=T_integer and smin2=smax2 then
--                      emitHex5w(mov_edi_imm32,smin2)              -- mov edi,imm32
                        symk = symtab[src2]
--20/12/15:
--                      if symk[S_NTyp]=S_Const and symk[S_vtype]=T_integer and symk[S_Init] then
--                          if and_bits(symk[S_State],K_rtn) then ?9/0 end if
                        if symk[S_NTyp]=S_Const 
                        and symk[S_vtype]=T_integer 
                        and and_bits(symk[S_State],K_rtn)=0
                        and symk[S_Init] then
                            k = symk[S_value]
                            movRegImm32(edi,k)                      -- mov edi,k
                        else
                            loadToReg(edi,src2,CLEAR)               -- mov edi,[src2] (idx)
                        end if
                    end if
                    clearReg(edi)
-- 27/3/2013:
--                  if opcode=opSubse1 then
                    if opcode=opSubse1 
                    or opcode=opSubse1i then
                        leamov(ecx,dest)                            -- lea ecx,[dest]/mov ecx,addr dest
                    end if
--DEV we may need opUnassigned here for opcode!=opSubse1? (testing will tell rsn)
                    loadToReg(esi,src)                              -- mov esi,[src] (s)
                    movRegVno(edx,src)                              -- mov e/rdx,varno of s
                    emitHex5callG(opcode)                           -- call opSubse1[i[s|p]] ([ecx]/eax:=esi[edi])
                    reginfo = 0
-- 27/3/2013:
--                  if opcode!=opSubse1 then
                    if opcode!=opSubse1 
                    and opcode!=opSubse1i then
--3/4/17 (over switch s[idx] do)
--                      if dname=-1 then
--                          transtmpfer(0,eax)
--                          -- (never need sch00n when dname=-1, btw)
--                      end if
--                      if dest then
--                          if sched then
--                              if opcode=opSubse1i then
--                                  -- needed for [esp]+1 error handling:
--                                  sch00n = schoon
--                              end if
--                          end if
                            storeReg(eax,dest,1,0)                  -- mov [dest],eax
--12/10
--                          if dname!=-1 then
                                transtmpfer(0,eax)
--                          end if
--                          if sched then
--                              sch00n = 0  -- (just the above line)
--                          end if
--                      end if
--9/4/15:
--                  elsif dtype=T_integer then
                    elsif vroot=T_integer then  -- result is integer

                        --
--DEV...
                        -- BTW: In case you are wondering why we have used opSubse1
                        --  when dtype is an integer: opSubse1i chokes on s=int/flt;
                        --  IOW it does NOT do proper "attempt to subscript an atom"
                        --  errors, so in some cases we must not use it. Eg assuming
                        --  o is object, after o=s[i][j], we cannot be certain that
                        --  o is not an int/flt, so in a subsequent i=o[k], we must
                        --  use the slightly slower opSubse1 instead of opSubse1i.
                        --  However, if it does succeed, then eax will indeed hold
                        --  the integer i (since it won't have just dealloc'd) and
                        --  it is still worth recording that fact (although it may
                        --  get wiped out by a typecheck rsn). Obviously, if s is
                        --  declared as a sequence, we use opSubse1i for i=s[j].
                        --
                        storeReg(eax,dest,0,0)  -- just record that eax==[dest]
--27/3/2013:
                        clearReg(ecx)
                    end if
                end if  -- not inlined opSubse1ip/s
            --if not bind then
            --  ?{opcode,dtype}
            --end if
            --if cstats then
            --  if opcode=opSubse1 then
            --      s1 += 1
            --  elsif opcode=opSubse1i then
            --      s1i += 1
            --  elsif opcode=opSubse1is then
            --      s1is += 1
            --  elsif opcode=opSubse1ip then
            --      s1ip += 1
            --  end if
            --end if
            end if -- isGscan

--      elsif opcode=opAdd and opcode<=opDivf2 then
        elsif opcode=opAdd
           or opcode=opAddi
           or opcode=opAddiii
           or opcode=opSub
           or opcode=opSubi
           or opcode=opSubiii
           or opcode=opMul
           or opcode=opMuli
           or opcode=opMuliii
           or opcode=opDiv
           or opcode=opDivi
           or opcode=opDiviii
           or opcode=opDiv2
           or opcode=opDivi2
           or opcode=opDivf
           or opcode=opDivf2 then
--                      opAdd = 64,         -- a = b+c      -- NB b,c must be atoms (see sq_add etc)
--                      opAddi = 65,        -- a = b+c      --                      when a is integer
--                      --opAddiii = 66,    -- a = b+c      --                      .. and b,c init ints
--                      opSub = 67,         -- a = b-c      -- ""
--                      opSubi = 68,        -- a = b-c                              -- ""
--                      --opSubiii = 69,    -- a = b-c                                -- ""
--                      opMul = 70,         -- a = b*c      -- ""
--                      opMuli = 71,        -- a = b*c                              -- ""
--                      --opMuliii = 72,    -- a = b*c                                -- ""
--                      opDiv = 73,         -- a = b/c      -- ""
--                      opDivi = 74,        -- a = b/c                              -- ""
--                      --opDiviii = 75,    -- a = b/c                                -- ""
--                      (opDiv2 = 76),      -- a = b/2              -- b is init int, a is atom or int
--                      --opDivi2 = 77,     -- a = b/2              -- b is init int, a is integer
--                      opDivf = 78,        -- a = floor(b/c)       -- a,b are atom or int
--                      --opDivf2 = 79,     -- a = floor(b/2)       -- b is init int, a is atom or int
--  (opDiv2 is only ever selected below, btw)
--
            waspc = pc
            dest = s5[pc+1]
            src = s5[pc+2]
            src2 = s5[pc+3]
            pc += 4
            if opcode=opAddiii              -- 66
            or opcode=opSubiii then         -- 69
--if not isGscan then trace(1) end if
                -- btw, twoInitialisedInts yielded true:
                getDest()
                getSrc()
                if not isGscan then
                    isInt = (slroot=T_integer and smin=smax)
                    nmax = smax
                end if
                getSrc2()
--if isGscan then

                slroot = T_integer
                if opcode=opAddiii then
                    nMin = smin+smin2
                    nMax = smax+smax2
                else
                    nMin = smin-smax2
                    nMax = smax-smin2
                end if
--DEV overkill!
                if smin=MININT or smin=MAXINT
                or smax=MININT or smax=MAXINT
                or smin2=MININT or smin2=MAXINT
                or smax2=MININT or smax2=MAXINT then
                    slroot = T_atom
                    smin = MININT
                    smax = MAXINT
                else
--3/1/16:
--                  if not integer(nMin) then
                    if isFLOAT(nMin) then
                        slroot = T_atom
                        smin = MININT
                    else
                        smin = nMin
                    end if
--                  if not integer(nMax) then
                    if isFLOAT(nMax) then
                        slroot = T_atom
                        smax = MAXINT
                    else
                        smax = nMax
                    end if
--DEV maybe....
--                  if dest=src then
--                      if smin2>0 then
--                          smax = MAXINT
--                      elsif smax2<0 then
--                          smin = MININT
--                      else
--                          smax = MAXINT
--                          smin = MININT
--                      end if
--                  end if
--                  if dest=src2 then
--                      if smin>0 then
--                          smax = MAXINT
--                      elsif smax<0 then
--                          smin = MININT
--                      else
--                          smax = MAXINT
--                          smin = MININT
--                      end if
--                  end if
                end if
                sltype = slroot
-- not needed (not and_bits(slroot,T_sequence))
--  setyp = detyp   -- no change
--  slen = dlen     -- no change
                storeDest()
                --DEV calc new min max if possible...
                -- (nb we may have gotten opAddii because of localtype, ie
                --  we still need to test gtypes before setting range)

                --else -- isGscan/opAddiii/opSubiii
                if not isGscan then
--trace(1)
                    --if cstats then
                    --  if opcode=opAddiii then
                    --      aiii += 1
                    --  end if
                    --end if
                    markConstUseds({dest,src,src2})
--if sched then
--  ?9/0    -- warning: completely untested code follows!
--end if
--if 0 then
--              getSrc()    -- (as sltype/slroot/smin/smax etc trashed since above call)
--end if

--              if sltype=T_integer and smin=smax then
--              if slroot=T_integer and smin=smax then
                    if isInt then -- (saved result of "slroot=T_integer and smin=smax")
                        -- (and nmax is original/unaltered smax(=smin) from src)
                        reg = -1
--                      if slroot2=T_integer and smin2=smax2 then
                        if slroot=T_integer and smin=smax then
                            if and_bits(state1,K_rtn) then ?9/0 end if
--                          if 0 then
--                              if opcode=opAddiii then
--                                  smin += smin2   -- see note above
--                              else
--                                  smin -= smin2   -- see note above
--                              end if
--                          end if
                            -- (<<< here, see below)
--DEV can't this be moved above? [to "(<<< here, see below)" above]
--      (and the test on reg=-1 removed)
--                      if reg=-1 then
                            -- store our literal smin in dest...
                            -- (NB smin is as modified for the add/sub above)
--                          if dname=-1 and dtype=T_integer and dmax!=MAXINT and dmin!=MININT then
                            if dname=-1 then
                                transtmpfer(smin,-1)
                            end if
                            if dest then
                                storeImm(dest,smin)                         -- mov [dest],imm32
                                if reginfo then clearMem(dest) end if
                                -- as above, we are not (currently) checking for overflow at compile-time,
                                --  which would be better than emitting run-time checking anyway.
                                --   (hence the lack of any such testing being emitted here)
--                              if dname!=-1    -- else above would/should have taken care of it...
--                              and dtype=T_integer and dmax!=MAXINT and dmin!=MININT then
                                if dname!=-1 then   -- else above would/should have taken care of it...
                                    transtmpfer(smin,-1)
                                end if
                                dest = 0
                            end if
--                      end if

                        else
--getSrc()
                            if opcode=opAddiii then
                                reg = loadReg(src2,CLEAR)                   -- mov reg,[src2]
                                --if smin then
                                if nmax then
                                    mod = m_add
--                                  regimm365(smin)                         -- add reg,imm
                                    regimm365(nmax)                         -- add reg,imm
                                end if
                            else
                                --
                                -- NB this is the wrong way round for regimm365;
                                --    ie edx:=const-reg, cannot use reg:=reg-const
                                --
                                reg = loadReg(src2)
--                              if sched then
--                                  schedule(0,0,edxbit,pUV,0,0)
--                              end if
                                if nmax then
--DEV test set did not catch this being commented out::
--SUG: EmitHex1(nop)
                                    movRegImm32(edx,nmax)                   -- mov edx,imm32
                                else
                                    --DEV what about neg reg instead??
                                    if X64 then
                                        emitHex1(#48)
                                    end if
                                    emitHex2s(xor_edx_edx)                  -- xor edx,edx
                                end if
--                              if sched then
--                                  rb = regbit[reg+1]
--                                  schedule(rb,0,rb+edxbit,pUV,0,0)
--                              end if
                                op1 = m_sub+1   -- 0o051
                                xrm = #C2+reg*8 -- 0o3r2
                                emitHexx2(op1,xrm)                          -- sub edx,reg
                                reg = edx
                            end if
                            if dname=-1 and dtype=T_integer and dmax!=MAXINT and dmin!=MININT then
                                transtmpfer(0,reg)
                            end if
                        end if
                    else    -- non-fixed src
                        reg = loadReg(src,CLEAR)                            -- mov reg,[src]
                        if opcode=opAddiii then
                            mod = m_add
                        else
                            mod = m_sub
                        end if
                        reg_src2()                                          -- add/sub reg,<src2>
                        if dname=-1 and dtype=T_integer and dmax!=MAXINT and dmin!=MININT then
                            transtmpfer(0,reg)
                        end if
                    end if  -- fixed src
                    if dest then
                        if dtype!=T_integer or dmax=MAXINT or dmin=MININT then
                            if reg!=edx then
--                              if sched then
--                                  rb = regbit[reg+1]
--                                  schedule(rb,0,edxbit,pUV,0,0)
--                              end if
--DEV use mov_reg?
                                xrm = #D0+reg   -- 0o32r
                                emitHexx2(mov_dword,xrm)                        -- mov edx,reg
                            end if
                            --                      storeReg(reg,dest,1,1)                          -- mov [dest],reg
                            --DEV if we reg=loadReg(src2,CLEAR) above (ie for sub as well as add):
                            -- actually, I think we're OK (re-check when we simplify further)
                            --  update: I now /think/ this is OK because of that reg = edx (64 lines up).
                            storeReg(reg,dest,1,0)                          -- mov [dest],reg
--                          if sched then
--                              schedule(edxbit,0,edxbit,pU,1,0) -- treat following as one instruction:
--                          end if
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(shl_edx_1)                            -- shl edx,1
                            emitHex6j(jno_rel32,0)                          -- jno @f [sj NOT ok]
                            backpatch = length(x86)
                            leamov(edi,dest)                                -- lea edi,[dest]
                            emitHex5callG(opAddiii)                         -- call :%e01tcfAddiii (same for addiii & subiii)
                            -- fatal error, does not return
                            x86[backpatch] = length(x86)-backpatch          -- @@:
                        else
                            storeReg(reg,dest,1,0)                          -- mov [dest],reg
                        end if
                    end if
                end if

            elsif opcode=opDiviii then      -- 75
                -- dest=src/src2
                getSrc()
                getSrc2()
--if isGscan then
-- DEV plenty of opportunity to go OTT here,
--      eg if src2 > #1FFFFFFF then dest is -1/0/1;
--          "  "   > #15555555  "	" "	-2..+2...
--      (the above is probably true for +/-)
-- From RDS Eu:
--          if right_val[MIN]>=2 then
--              -- narrow the result range */
--              result[MIN] = left_val[MIN] / right_val[MIN] - 1
--              result[MAX] = left_val[MAX] / right_val[MIN] + 1
--          end if
-- Also noted:
--      src     src2        dest
--      -5..4   -2..-1      -4..5   \
--      -5..4   1..2        -5..4    \  so many variations seem to be
--      -5..4   -2..-2      -5..5    /  evident, it is quite scary...
--      4..10   2..4        1..5    /
--
-- So... we'll just:
-- added 6/4/2012:
if dest!=0 then
                getDest()
--
--          0) deal with /2 first:
--
                sltype = T_integer
                if slroot2=T_integer and smin2=smax2 and smax2=2
                and not and_bits(slroot,T_N)
                and smin=smax then
                    nMin = smin/2
--                  if integer(nMin) then
                    if not isFLOAT(nMin) then
                        smin = nMin
                        smax = nMin
                    else
                        sltype = T_atom
                    end if
--
--          1) if src2 is positive, the result is bound to be in the range of src...
--
                elsif smin2>0 then -- (hence smax2 is also positive)
                    --
                    -- ... divided by smin2/smax2 according to sign:
                    --
                    --  if we have say -10..10/2..5 then:
                    --      [1] -10/2=-5,   [2] -10/5=-2
                    --      [3]  10/2=5     [4]  10/5=2
                    -- hence when smin/max is negative, the minimum is /min2[1], the maximum is /max2[2],
                    -- whereas for a positive smin/max, the minimum is /max2[4], the maximum is /min2[3].
                    --   (nb, as said, this is for +ve src2 only, not so for -1,-2 etc!)
                    --
                    if smin>=0 then                 -- for positive nos,
                        smin = floor(smin/smax2)    --.. min is nearer zero
                    else                            -- for negative nos,
                        smin = floor(smin/smin2)    --.. min is further away from zero
                    end if
                    if smax>=0 then                 -- for positive nos,
                        smax = floor(smax/smin2)    --.. max is further away from zero
                    else                            -- for negative nos,
                        smax = floor(smax/smax2)    --.. max is nearer zero
                    end if
                else
--
--          2) else since src2 is an integer any valid result is bound
--              to be within the range +/- max(abs(smin),abs(smax)),
--              eg 45..49/?? is -49..+49, -17..3/?? is -17..+17.
--
--              2a) if this is definitely /0, a forced crash, then
--                  don't propagate the issue:
                    if smin2!=smax2 or smin2!=0 then
                        if smin<0 then          -- flip for abs(smin)
                            if smin=MININT then     -- avoiding gotcha
                                smin = MAXINT
                            else
                                smin = -smin
                            end if
                        end if
                        if smax<0 then          -- flip for abs(smax)
                            if smax=MININT then     -- avoiding gotcha
                                smax = MAXINT
                            else
                                smax = -smax
                            end if
                        end if
                        if smax<smin then
                            smax = smin     -- max(abs(smin),abs(smax))
                        end if
                        if smax=MAXINT then
                            smin = MININT
                        else
                            smin = -smax
                        end if
                    end if
                end if
                slroot = sltype
--Not needed (not and_bits(sltype,T_sequence))
--  setyp = detyp   -- no change
--  slen = dlen     -- no change
                storeDest()
end if
                --DEV
                -- Oh, before I forget, we also want:
                --              if and_bits(slroot,T_atom)=T_integer        \
                --              and smin>MININT and smin=smax               /   (!before the messing about above!)
                --              and and_bits(slroot2,T_atom)=T_integer
                --              and smin2>MININT and smin2=smax2 then
                --                  nMin = smin/smax2
                --                  if not integer(nMin) then nMin=9/0 end if   --DEV proper message (and/or flag for list.asm?)
                --                  ?if not integer(nMin) then
                --                  if isFLOAT?(nMin) then
                --                      gscanerror()  -- or:
                --                      msg = sprintf("division result of %g",nMin)
                --                      Warn(msg,??line,1,0)
                --                  else
                --  smin = nMin
                --  smax = smin
                --                  end if
                --
                -- integer seelistdotasm
                --          seelistdotasm = 0
                --  procedure gscanerror()
                --      if not seelistdotasm then
                --          seelistdotasm = 1
                -- getname()...
                --          if Fatal(sprintf("gvar_scan error, see symtab[%d] in list.asm\n",dest)) then end if
                --or:       Fatal(sprintf("gvar_scan error, see symtab[%d] in list.asm\n",dest))
                --      end if
                --      ?symtab[dest][S_State] = or_bits(symtab[dest][S_State],S_slda)
                --  end procedure
                --              end if
                --else -- isGscan/opDiviii
                if not isGscan then
                    getSrc()
-- 6/4/2012:
--                  markConstUseds({dest,src,src2})
                    if dest then
                        markConstUsed(dest)
                    end if
                    markConstUseds({src,src2})
                    if slroot2=T_integer and smin2=2 and smax2=2 then   -- /2 (DEV what about 4,8,etc?)
--DEV tryme
--  k = 0
--  if slroot2=T_integer and smin2=smax2 then
--      k = find(smin2,{#2,#4,#8,
--                      #10,#20,#40,#80,
--                      #100,#200,#400,#800,
--                      #1000,#2000,#4000,#8000,
--                      #10000,#20000,#40000,#80000,
--                      #100000,#200000,#400000,#800000,
--                      #1000000,#2000000,#4000000,#8000000,
--                      #10000000,#20000000})
--  end if
--  if k then
                        opcode = opDivi2    -- dev is this needed (opLabel?)
                        if slroot=T_integer and smin=smax then
--                          if sched then
--                              schedule(0,0,eaxbit,pUV,0,0)
--                          end if
                            if and_bits(smin,1) then    -- (oops, heading for a fatal error)
                                                        -- (let the backend catch it)
--                              emitHex5w(mov_eax_imm32,smin)           -- mov eax,imm32
                                movRegImm32(eax,smin)                   -- mov eax,imm32
                            else
                                smin = smin/2
                                storeImm(dest,smin)                     -- mov [dest],imm32
                                if reginfo then clearMem(dest) end if
                                src = 0 -- flag no sar etc rqd
                            end if
                        else
--                          if sched then
--                              schedule(0,0,eaxbit,pUV,0,src)
--                          end if
                            loadToReg(eax,src)                          -- mov eax,[src]
                        end if
                        if src then
--                          if sched then
--                              schedule(0,0,edxbit,pUV,0,0)
--                          end if
                            leamov(edx,dest)                            -- lea edx,[dest]/mov edx,addr dest
--                          if sched then
--                              schedule(eaxbit,0,eaxbit,pU,1,0)
--                          end if
--DEV tryme
-- if k>1 then
--  test eax,smin2-1
--  jz @f
--  mov ecx,k (or smin2)
--  call e01++
--  @@:
--  sar eax,k
-- else
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(sar_eax_1)                        -- sar eax,1
--                          if sched then
--                              schedule(0,0,0,pV,1,0)  -- treat next pair as one instruction
--                          end if
--DEVBPM backpatch me: [DONE, for newEmit anyway]
                            emitHex6j(jnc_rel32,0)                      -- jnc @f [sj NOT ok]
                            backpatch = length(x86)
                            emitHex5callG(opcode)                       -- call :%e01tcfDivi2
                            x86[backpatch] = length(x86)-backpatch
                            -- fatal error, does not return ([edx]:=eax+0.5)
-- end if
                            storeReg(eax,dest,1,1)                      -- mov [dest],eax
                        end if
                    else
--                      if sched then
--                          schend()    -- [esp]+13/-24/-18
--                          -- nb error handling below assumes all [sj OK] are so when all's finished
--                      end if
                        if slroot2=T_integer and smin2=0 and smax2=0 then
                            emitHex5callG(opDiv0)                               -- call :%e02atdb0 (see pDiagN.e)
                            -- fatal error, does not return
                            opcode = opRTErn -- for opLabel
                        else
                            if and_bits(symtab[src][S_State],K_Fres) then ?9/0 end if
                            loadMem(edx,src)                                    -- mov edx,[src]    init int
                            loadToReg(ecx,src2)                                 -- mov ecx,[src2]   init int
if 1 then -- new code 2/11/15
                            clearReg(ecx)   -- (btw, this block leaves edi intact)
                            clearReg(esi)
--DEV cdq??
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(mov_eax_edx)                              -- mov eax,edx
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex3(sar_edx_imm8,#1F)                          -- sar edx,31
-- </cdq>
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(test_ecx_ecx)                             -- test ecx,ecx (check for /0)
                            emitHex6j(jnz_rel32,0)                              -- jnz @f [sj OK]
                            backpatch = length(x86)
                            emitHex5callG(opDiv0)                               -- call :%e02atdb0 (see pDiagN.e)
                            x86[backpatch] = length(x86)-backpatch              -- @@:
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(idiv_ecx)                                 -- idiv ecx [eax:edx/=ecx; eax:=quotient, edx:=remainder]
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(mov_esi_eax)                              -- mov esi,eax
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(test_edx_edx)                             -- test edx,edx
                            emitHex6j(jnz_rel32,0)      -- if rmdr, not int     -- jnz @f (call e01, dest is flt) [sj OK]
                            backpatch = length(x86)
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(shl_esi_1)                                -- shl esi,1
                            emitHex6j(jno_rel32,0)      -- >31 bit result       -- jno @f [sj OK]
                                                        -- (can only happen for -#40000000/-1, btw)
                            x86[backpatch] = length(x86)-backpatch              -- @@:
                            backpatch = length(x86)
                                leamov(edi,dest)                                --    lea edi,[dest]
                                emitHex5callG(opDiviii)                         --    call :%e01tcfediDiv (fatal error)
                                -- fatal error, does not return. (calc [edi]=(eax*ecx+edx)/ecx, as a float, and tcf it.)
                            x86[backpatch] = length(x86)-backpatch              -- @@:
                            storeReg(eax,dest,1,1)                          -- mov [dest],eax
else -- (old code)
                            clearReg(ecx)   -- (btw, this block leaves esi,edi intact)
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(mov_eax_edx)                              -- mov eax,edx
--                      clearReg(eax) not needed, storeReg(,,,1,) below
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex3(sar_edx_imm8,#1F)                          -- sar edx,31
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(test_ecx_ecx)                             -- test ecx,ecx (check for /0)
--                      emitHex2(jnz_rel8,5)                                -- jnz @f [sj OK] (nb above)
--DEVBPM backpatch me: [DONE]
--                          emitHex6j(jnz_rel32,5)                              -- jnz @f
                            emitHex6j(jnz_rel32,0)                              -- jnz @f
--DEVBPM backpatch me: [DONE] (that next backwards target is here)
                            backpatch = length(x86)
                            emitHex5callG(opDiviii)                             -- call :%e01tcfediDiv (fatal error)
                            -- fatal error, does not return. (if [src2]=0 attempt to divide by zero,
                            -- else calculate [dest]=[src]/[src2], as a float, and tcf it.)
                            x86[backpatch] = length(x86)-backpatch              -- @@:
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(idiv_ecx)                                 -- idiv ecx
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(mov_ecx_eax)                              -- mov ecx,eax
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(test_edx_edx)                             -- test edx,edx
--DEVBPM backpatch me: [DONE] (backwards)
--                          emitHex6j(jnz_rel32,-17)    -- if rmdr, not int     -- jnz (call e01, dest is flt) [sj OK]
                            emitHex6j(jnz_rel32,0)      -- if rmdr, not int     -- jnz (call e01, dest is flt) [sj OK]
                            x86[length(x86)] = backpatch-length(x86)
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(shl_ecx_1)                                -- shl ecx,1
--DEVBPM backpatch me: [DONE] (backwards)
--                          emitHex6j(jo_rel32,-25)                             -- jo (call e01, dest too big) [sj OK]
                            emitHex6j(jo_rel32,0)                               -- jo (call e01, dest too big) [sj OK]
                            x86[length(x86)] = backpatch-length(x86)
                                                                            -- (can only happen for -#40000000/-1, btw)
--                          if sched then
--                              -- needed for [esp]+13 error handling:
--                              sch00n = schoon
--                          end if
                            storeReg(eax,dest,1,1)                          -- mov [dest],eax
--                          if sched then
--                              sch00n = 0 -- (just the above line)
--                          end if
end if
                        end if
                    end if
                end if

            elsif opcode=opMuliii then      -- 72
                -- dest=src*src2
                -- (tii was true, btw)
                getDest()
--if isGscan then
                getSrc()
                getSrc2()
                -- calculate all four corners of the matrix to cover sign issues:
                --  (examples/rationale can be found in t49ginfo.exw)
                w = smin*smin2
                nMin = w
                nMax = w
                w = smin*smax2
                if nMin>w then nMin = w end if
                if nMax<w then nMax = w end if
                w = smax*smin2
                if nMin>w then nMin = w end if
                if nMax<w then nMax = w end if
                w = smax*smax2
                if nMin>w then nMin = w end if
                if nMax<w then nMax = w end if
                slroot = T_integer
--              if integer(nMin) then
                if not isFLOAT(nMin) then
                    smin = nMin
                else
                    slroot = T_atom
                    smin = MININT
                end if
--              if integer(nMax) then
                if not isFLOAT(nMax) then
                    smax = nMax
                else
                    slroot = T_atom
                    smax = MAXINT
                end if
                sltype = slroot
-- (setyp,slen not needed here)
                storeDest()
                --else -- isGscan/opMuliii
                if not isGscan then
                    markConstUseds({dest,src,src2})
--              if dmin=dmax then
                    if and_bits(dtype,T_atom)=T_integer and dmin=dmax then
                        storeImm(dest,dmin)                             -- mov [dest],imm32
                        if reginfo then clearMem(dest) end if
                    else
                        getSrc()
--DEV may be unnecessary:
                        getSrc2()
                        mul2 = 0
                        if slroot=T_integer and smin=smax then
                            if slroot2=T_integer and smin2=smax2 then   -- both fixed
--DEV sensible message on error (not integer) here:
                                dmin = smin*smax2
                                storeImm(dest,dmin)                     -- mov [dest],imm32
                                if reginfo then clearMem(dest) end if
                                dest = 0    -- (mul2 will remain 0, btw)
                            elsif smin=2 then   -- i=2*j
                                mul2 = src2 -- nb 2 only, or 4/8/16 etc iff 32-bit result/overflow-checkable.
                            elsif smin=1 then   -- i=1*j
                                mul2 = -1
                                src = src2
                                smin = smin2
                            elsif smin=0 then   -- i=0*j
                                mul2 = -1
                                src = 0
                            end if
                        elsif slroot2=T_integer and smin2=smax2 then
                            if smax2=2 then     -- i=j*2
                                mul2 = src -- nb as above (ie/eg not #20000000*8 => reg=#00000000, oops!)
                            elsif smax2=1 then  -- i=j*1
                                mul2 = -1
                            elsif smax2=0 then  -- i=j*0
                                mul2 = -1
                                src = 0
                            end if
                        end if
                        if mul2=-1 then -- i=(1|0)*j or i=j*(1|0) case (for j read src)
                            if src!=dest then
                                if src=0 then
                                    storeImm(dest,0)                    -- mov [dest],ebx(0)
                                    if reginfo then clearMem(dest) end if
                                else
                                    reg = loadReg(src)                  -- mov reg,[src(/2)]
                                    storeReg(reg,dest,1,0)              -- mov [dest],reg
                                end if
                            end if
                        elsif mul2 then -- i=2*j or i=j*2 case (for j read mul2)
                            reg = loadReg(mul2,CLEAR)                   -- mov reg,[src/2]
--                          if sched then
--                              rb = regbit[reg+1]
--                              schedule(rb,0,rb,pU,1,0)
--                          end if
                            xrm = 0o340+reg -- 0o34r, shl/reg
                            emitHexx2(0o321,xrm)                        -- shl reg,1
                            xrm = 0o320+reg -- 0o32r
                            emitHexx2(mov_dword,xrm)                    -- mov edx,reg
                            storeReg(reg,dest,1,0)                      -- mov [dest],reg
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(shl_edx_1)                        -- shl edx,1
                            emitHex6j(jno_rel32,0)                      -- jno @f [sj NOT ok]
                            backpatch = length(x86)
--DEV would it not be easier to load var no and value separately?
                            leamov(edi,dest)                            -- lea edi,[dest]
                            emitHex5callG(opAddiii)                     -- call :%e01tcfAddiii (same for addiii & subiii)
-- (spotted in passing) are we quite sure that should not be: [update: yes, we are.]
--                          emitHex5callG(opMuliii)                     -- call :%e01tcfediMul
                            -- fatal error, does not return
                            -- (we can use e01tcfAddiii here as long as we have a 32-bit result,
                            --  whereas e01tcfediMul needs to do the full 64-bit ecx:eax thing.)
                            x86[backpatch] = length(x86)-backpatch      -- @@:
                        elsif dest then
                            reginfo = 0     -- as we trash eax,ecx,esi,edi.
                                        -- (DEV we could use edx instead of esi below, to preserve that,
                                        --      and storeReg(eax,dest,1,1) under sch00n=schoon, and
                                        --          get dest from [esp]+7 to preserve edi...)
                            if slroot2=T_integer and smin2=smax2 then
--                              if sched then
--                                  sch00n = schoon
--                                  schedule(0,0,ecxbit,pUV,0,0)
--                              end if
--                              emitHex5w(mov_ecx_imm32,smin2)          -- mov ecx,imm32
                                movRegImm32(ecx,smin2)                  -- mov ecx,imm32
                            else
--                              if sched then
--                                  sch00n = schoon
--                                  schedule(0,0,ecxbit,pUV,0,src2)
--                              end if
                                loadToReg(ecx,src2)                     -- mov ecx,[src2]
                            end if
                            if slroot=T_integer and smin=smax then
--                              if sched then
--                                  schedule(0,0,eaxbit,pUV,0,0)
--                              end if
--                              emitHex5w(mov_eax_imm32,smin)           -- mov eax,imm32
                                movRegImm32(eax,smin)                   -- mov eax,imm32
                            else
--                              if sched then
--                                  schedule(0,0,eaxbit,pUV,0,src)
--                              end if
                                loadToReg(eax,src)                      -- mov eax,[src]
                            end if
--                          if sched then
--                              schend()    -- (may possibly schedule more here, up to je,
--                                          --  and that store eax, but je..jo non-sched.
--                                          --  I doubt such would be worthwhile, though.)
--                          end if
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(imul_ecx)                         -- imul ecx
                            leamov(edi,dest)                            -- lea edi,[dest]/mov edi,dest (addr result)
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(mov_ecx_edx)                      -- mov ecx,edx
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex1(cdq)                               -- cdq (cqo on 64-bit)
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(cmp_ecx_edx)                      -- cmp ecx,edx  -- >32 bits?
--DEVBPM backpatch me: [DONE]
                            emitHex6j(je_rel32,0)                       -- je @f [sj OK]
                            backpatch = length(x86)
                            emitHex5callG(opMuliii)                     -- call :%e01tcfediMul
                            -- ^ fatal error, does not return (loads ecx:eax as 64-bit float)
                            x86[backpatch] = length(x86)-backpatch      -- @@:
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(mov_esi_eax)                      -- mov esi,eax
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(shl_esi_1)                        -- shl esi,1
--DEVBPM backpatch me: (backwards) [DONE]
                            emitHex6j(jo_rel32,0)                       -- jo (call e01tcfediMul) [sj OK]
                            x86[length(x86)] = backpatch-length(x86)
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(mov_medi_eax)                     -- mov [edi],eax
--7/12/2011:
--                          storeReg(eax,dest,0,0) -- just record the fact that eax==[dest]
                            if reginfo then clearReg(ecx) end if
                            if reginfo then clearReg(esi) end if
                            storeReg(eax,dest,0,1) -- record the fact that eax==[dest]
                        end if  -- not inlined as smin &/or smax fixed/reduced to add op
                    end if -- not inlined (to imm32) as dest fixed
                end if

            elsif opcode=opDivf2 then       -- 79
                -- dest=floor(src/2^src2); aka src sar (src2-1).
                -- src is (/must be) init int, dest is int
                -- src2 is 1/2/3/4/5... for 1/2/4/8/16... [DEV probably better to map that here than pmain.e]
--if isGscan then
                getDest()
                getSrc()
                slroot = and_bits(slroot,T_atom)
                if slroot=T_integer then
                    if src=dest then
                        smin = 0
-- removed 1/11/15:
--                      smax = 0
                    else
                        p2 = power(2,src2-1)
                        smin = floor(smin/p2)
                        smax = floor(smax/p2)
                    end if
--??:
--              else
----                slroot = T_atom
--                  smin = MININT
--                  smax = MAXINT
                end if
-- (setyp,slen not needed here)
                sltype = slroot
                storeDest()
                --else -- isGscan/opDivf2
                if not isGscan then
                    markConstUseds({dest,src})
                    reg = loadReg(src,CLEAR)                        -- mov reg,[src]
--                  if sched then
--                      rb = regbit[reg+1]
--                      schedule(rb,0,rb,pU,1,0)
--                  end if
                    xrm = #F8+reg -- 0o37r, sar/reg
                    src2 -= 1
                    if src2=1 then
                        emitHexx2(0o321,xrm)                        -- sar reg,1
                    elsif src2 then
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex3l(#C1,xrm,src2)                     -- sar reg,nn
                    end if
                    storeReg(reg,dest,1,0)                          -- mov [dest],reg
                end if

            else -- other opMath not covered above
                -- (warning: pc+=4 inappropriate here)
--20/4 now I use waspc
--if isGscan then
                getDest()
                if opcode>=opAdd and opcode<=opSubi then -- NB 66 (opAddiii) already done
--aim: convert opAdd/opSub to opAddi/opSubi, (using stype/2)
--        and opAddi/opSubi to opAddiii/opSubiii (using smin/max/2);
                    if src=dest or src2=dest then
                        smin = MININT
                        smax = MAXINT
                        sltype = and_bits(vroot,T_atom)
                    else
--DEV 7/7/17...
--                      if vroot=T_integer and vmin!=MININT and vmax!=MAXINT then
                        if vroot=T_integer and vmin!=MININT and vmax!=MAXINT and X64=0 then
                            if opcode=opAdd then
                                opcode = opAddi
                                s5[waspc] = opAddi
                            elsif opcode=opSub then
                                opcode = opSubi
                                s5[waspc] = opSubi
                            end if
                        end if
                        getSrc()
                        sltype = T_atom
                        if and_bits(slroot,T_atom)=T_integer then
                            getSrc2()
                            if and_bits(slroot2,T_atom)=T_integer then
                                if opcode=opAdd or opcode=opAddi then
                                    nMin = smin+smin2
                                    nMax = smax+smax2
                                else
                                    nMin = smin-smax2
                                    nMax = smax-smin2
                                end if
                                sltype = T_integer
                                if smin=MININT or smin=MAXINT
                                or smax=MININT or smax=MAXINT
                                or smin2=MININT or smin2=MAXINT
                                or smax2=MININT or smax2=MAXINT then
                                    smin = MININT
                                    smax = MAXINT
                                    sltype = T_atom
                                else
--                                  if not integer(nMin) then
                                    if isFLOAT(nMin) then
                                        smin = MININT
                                        sltype = T_atom
                                    else
                                        smin = nMin
                                    end if
--                                  if not integer(nMax) then
                                    if isFLOAT(nMax) then
                                        smax = MAXINT
                                        sltype = T_atom
                                    else
                                        smax = nMax
                                    end if
                                end if
--DEV not tried yet/do we want to leave this for pilxl?:
--DEV 7/7/17...
--                              if sltype=T_integer then
                                if sltype=T_integer and X64=0 then
--                              and and_bits(dtype,T_atom)=T_integer then
                                    if opcode=opAddi then
                                        s5[waspc] = opAddiii
                                    elsif opcode=opSubi then
                                        s5[waspc] = opSubiii
                                    end if
                                end if
                            else
                                smin = MININT
                                smax = MAXINT
                            end if
                        else
                            smin = MININT
                            smax = MAXINT
                        end if
                    end if
                elsif opcode=opMul or opcode=opMuli then
                    -- dest=src*src2
--DEV: try to convert opMul/opMuli to opMuliii (using stype/2)
                    getSrc()
--                  dtype = or_bits(dtype,T_atom)
                    sltype = T_atom
                    if and_bits(slroot,T_atom)=T_integer then
                        getSrc2()
                        if and_bits(slroot2,T_atom)=T_integer then
                            -- calculate all four corners of the matrix to cover sign issues:
                            --  (examples/rationale can be found in t49rmdr.exw)
                            w = smin*smin2
                            nMin = w
                            nMax = w
                            w = smin*smax2
                            if nMin>w then nMin = w end if
                            if nMax<w then nMax = w end if
                            w = smax*smin2
                            if nMin>w then nMin = w end if
                            if nMax<w then nMax = w end if
                            w = smax*smax2
                            if nMin>w then nMin = w end if
                            if nMax<w then nMax = w end if
                            sltype = T_integer
--DEV common code?
--                          if integer(nMin) then
                            if not isFLOAT(nMin) then
                                smin = nMin
                            else
                                smin = MININT
                                sltype = T_atom
                            end if
--                          if integer(nMax) then
                            if not isFLOAT(nMax) then
                                smax = nMax
                            else
                                smax = MAXINT
                                sltype = T_atom
                            end if
                        end if
                    end if

                elsif opcode=opDiv
                   or opcode=opDivi
                   or opcode=opDivf then
--DEV UNTESTED:
--constant opMath = {opAdd,opAddi,opSub,opSubi,opMul,opMuli,opDiv,opDivf,opDivi}
----                    64      65  67     68       70  71     73    78     74
--                                  (66=opAddiii, 69=opSubiii, 75=opDiviii)
--DEV: try to convert opDiv to opDivi, (using stype/2)
--                and opDivi to opDiviii (using smin/max/2)
--                and possibly to opDivi2 if smin2=smax2=1/2/4/8/16...
--                and possibly opDivf to opDivf2 if smin2=smax2=1/2/4/8/16...
                    getSrc()
                    getSrc2()
                    sltype = T_atom
                    if slroot=T_integer and smin=smax
                    and slroot2=T_integer and smin2=smax2 and smin2!=0 then
                        if opcode=opDivf then
--                      if opcode=opDivf
--                      or and_bits(vroot,T_atom)=T_integer then
                            nMin = floor(smin/smin2)
                        else
                            nMin = smin/smin2
                        end if
--                      if integer(nMin) then
                        if not isFLOAT(nMin) then
                            sltype = T_integer
                            smin = nMin
                            smax = smin
                        end if
                    end if
                else
                    ?9/0    -- oops!
                end if
                slroot = sltype
                storeDest()
                --else -- isGscan/other math ops
                if not isGscan then
                    --          if opcode<=opSubi then
                    ----                if cstats then
                    ----                    if opcode=opAdd then
                    ----                        a += 1
                    ----                    elsif opcode=opAddi then
                    ----                        ai += 1
                    ----                    end if
                    ----                end if
                    --              pc += 5
                    --          else
                    --              pc += 4
                    --          end if
                    --getDest()
                    --if dtype=T_integer then
                    --  if opcode=opAdd then
                    ----        trace(1)
                    --      getSrc()
                    --  else
                    ----        trace(1)
                    --  end if
                    --end if
                    --if 0 then
                    markConstUseds({dest,src,src2})
                    slroot2 = 0
                    if opcode>=opDiv then   -- {opDiv,opDivi,opDivf}
                        getSrc2()
                    end if
--DEV handle +-*0 as well here:
                    if slroot2=T_integer and smin2=0 and smax2=0 then
                    -- an opDiv of /0 flavour
--                      if sched then
--                          schend()
--                      end if
                        emitHex5callG(opDiv0)                               -- call :%e02atdb0 (see pDiagN.e)
                        -- fatal error, does not return
                        opcode = opRTErn -- for opLabel
--DEV handle */1 here?
                    else
--                      if sched then
--                          sch00n = schoon
--                          schedule(0,0,edibit,pUV,0,0)
--                      end if
                        leamov(edi,dest)                            -- lea edi,[dest]/mov edi,dest (addr result)
--DEV ideally, these should be mov esi,[src] & NB src/2 obtained from [esp]-?/? on error:
-- (will have to be done one by one though...)
--constant opMath = {opAdd,opAddi,opSub,opSubi,opMul,opMuli,opDiv,opDivf,opDivi},
-- ... but because of the error handling we cannot do eg:
--       mov_edx_mem32  = {#8B,#15},    -- 0o213 0o025 m32          -- mov edx,[m32]
-- vs.   mov_edx_imm32  = #BA,          -- 0o272 imm32              -- mov edx,imm32


--DEV damn. have to knock this on the head till we have an isInit for src: (but we now opUnassign things, so...)
--                      if opcode=opDiv and srcInit
--                      if newEmit and opcode=opDiv
                        if opcode=opDiv
                        and slroot2=T_integer and smin2=smax2 and smin2=2 then
                            getSrc()
                            if slroot=T_integer then
                                opcode = opDiv2
                            end if
                        end if
                        if opcode=opDiv2 then
--  if sched then
--                      schedule(0,0,eaxbit,pUV,0,src)
--  end if
                            loadToReg(eax,src)                          -- mov eax,[src]
                        else
                            loadToReg(ecx,src)                          -- mov ecx,[src]
                            loadToReg(eax,src2)                         -- mov eax,[src2]
                        end if
--                      if sched then
--                          schend()
--                      end if
                        emitHex5callG(opcode)                           -- call opXxx (mathop)
                        reginfo = 0 -- all regs trashed
                    end if
                end if -- isGscan
            end if -- other math ops

        elsif opcode=opJif              -- 26
           or opcode=opJnot then        -- 27

            src = s5[pc+4]
if NOLT=0 or bind or lint then
            src2 = s5[pc+6]     -- lastparam
            if src2 then
                -- This is an "if udt(src2) then" style op,
                --  where src is the result var of udt() and
                --  hence determines its routine_id/vartype.
                -- Create appropriate localtype info for src2.
                --
                getSrc2()
                -- (nb we can do this now because src2 is not ref'd again below)
                Lmin = smin2
                Lmax = smax2
                Letyp = setyp2
                Lslen = slen2
                ltAdd(TEST,src2,sltype2,src+1,pc)
                -- eg/ie if mytype(x) then
                --  ==>  opFrame..opCall for mytype(x);
                --       opJif/not,mS,tgt,link,<res:mytype>,1,<x>
                --  hence ltAdd(TEST,x,<ptyp>,mytype,pc):
                --   (obviously we do not modify src/res:mytype here)
            end if
end if  -- NOLT
            if isGscan then
                pc += 7
            else
                -- Jif|Jnot,mergeSet,tgt,link,src,isInit,lastparam
                src = s5[pc+4]
--7/12/15:
                if atom(symtab[src]) then
                    src = forwardretarget[find(src+1,forwardingtable)]-1
                    s5[pc+4] = src
                end if
                isInit = s5[pc+5]   -- copy of src's [S_Init]
                jinit(pc+1)
                getSrc()
                if isInit and slroot<=T_atom then
                    if slroot=T_integer then
                        if smin=smax
                        or smax<0           -- eg -4..-2 is never 0/false (use the -4)
                        or smin>0 then      -- eg 12..14 is never 0/false (use the 12)
                            if and_bits(state1,K_rtn) then ?9/0 end if
                            tmpd = src
                            tmpv = smin
                            tmpr = -1
                        end if
                    end if
                    if tmpd then
                        if tmpd!=src then ?9/0 end if
                        if tmpr>=0 then
                            reg = tmpr
--                      promoteReg(reg)     -- don't need, nowt below will damage it.
                            tmpr = -1
                            tmpd = 0
                        else
                            if opcode=opJif then
                                tmpv = (tmpv!=0)
                            else    --opJnot
                                tmpv = (tmpv=0)
                            end if
                        end if
                    else
--                      if sched then
--                          if mergeSet!=isOpCode then
--                              sch00n = lastJmp    -- (can schedule non-last-jumps)
--                          end if
--                      end if
                        reg = loadReg(src)                          -- mov reg,[src]
                    end if
                    if tmpd=0 then
--                      if sched then
--                          if mergeSet!=isOpCode then
--                              sch00n = lastJmp    -- (can schedule non-last-jumps)
--                          end if
--                          schedule(regbit[reg+1],0,0,pUV,1,0)
--                      end if
                        xrm = 0o300+reg*9 -- 0o3rr
                        emitHexx2(test_reg_reg,xrm)                 -- test reg,reg
                    end if
                else    -- not init/stype>T_atom; use opcode.
                    if tmpd then ?9/0 end if
--                  if sched then
--                      schend()    -- [esp]-9
--                  end if
                    loadToReg(eax,src)              -- mov eax,[src]
                    storeReg(eax,src,0,1)           -- just record that now eax==[src]
                    if X64 then
                        emitHex1(#48)
                    end if
                    movRegVno(edx,src)              -- mov e/rdx,src
                    emitHex5callG(opJif)            -- call :%opJif -- handles unassigned, non-ATOM errors
                    -- NB src obtained from [esp]-9 on error
                    -- ebx=0, all other regs left unaltered
                end if
                bcode = 3+(opcode=opJif)    -- if opJif then jnz[4] else jz[3]  (idx to ccde)
                -- Jif/Jnot:
                if jend(7) then exit end if -- an opRetf fell off end of code, --> end while
            end if

        elsif opcode=opLen then         -- 51
            dest = s5[pc+1]
            src = s5[pc+2]
            getDest()
--if not isGscan then trace(1) end if
--if isGscan then
-- init
-- opstype
--trace(1)
            getSrc()
            sltype = T_integer
            slroot = T_integer
            slen2 = slen    -- save for use below (storeDest can damage slen)
            if slen>=0 then
                smin = slen
                smax = slen
            else
                smin = 0
                smax = MAXLEN
            end if
            storeDest()
            --else
            if isGscan then
                pc += 5
            else
                isInit = s5[pc+3]   -- (of src)
                ltype = s5[pc+4]        -- (of src, <= T_object)
                pc += 5 -- nb must occur before transtmpfer...
                -- inline if: target is integer, source is init and not possibly int|flt
                if dtype=T_integer and isInit and not and_bits(ltype,T_atom) then
--                  getSrc()
                    -- if length is known, try to transtmpfer it...
--                  if slen>=0 then
                    if slen2>=0 and dname=-1 then
--DEV: test "l = length({1,2,3}) if l=3 then"
--                      transtmpfer(slen,-1)
                        transtmpfer(slen2,-1)
                        -- ...dest will now be 0 if we can/succeeded
                    end if
                    if dest then
--                      if slen>=0 then
                        if slen2>=0 then
                            storeImm(dest,slen2)                            -- mov [dest],imm32
                            if reginfo then clearMem(dest) end if
                            if dname!=-1 then
                                transtmpfer(slen2,-1)
                            end if
                        else
                            reg = loadReg(src)                              -- mov reg,[src]
--DEV collect some metrics for this decision. I recon edx/transtmpfer will win, but
--    to be honest I have no idea how often a pukka tracked register would be better.
--    (it probably makes absolutely no measurable difference anyway...)
--                      wrk = spareReg(1)   --DEV 0 might prove better? - prolly about to use it rsn!!
                            wrk = spareReg(0)   --DEV 0 might prove better? - prolly about to use it rsn!!
if X64 then
--removed 20/8/15:
--                          emitHex1(#48)
                            sibLoad743(wrk,by4,reg,ebx,-24)                 -- mov wrk,[rbx+reg*4-24]
else
                            sibLoad743(wrk,by4,reg,ebx,-12)                 -- mov wrk,[ebx+reg*4-12]
end if
                            if dname=-1 then
                                transtmpfer(0,wrk)
                            end if
                            if dest then
                                storeReg(wrk,dest,1,0)                      -- mov [dest],wrk
--12/10:
                                if dname!=-1 then
                                    transtmpfer(0,wrk)
                                end if
                            end if
                        end if
                    end if  -- dest!=0 (transtmpfer fixed length)
                else -- not inline (tgt may need dealloc, src may be uninit or int|flt)
--                  if sched then
--                      sch00n = schoon
--                      schedule(0,0,edibit,pUV,0,0)
--                  end if
                    leamov(edi,dest)                                -- lea edi,[dest]/mov edi,dest (addr result)
--                  if sched then
--                      schend()    -- [esp]-9
--                  end if
                    loadToReg(esi,src,CLEAR)                        -- mov esi,[src]
                    movRegVno(edx,src)                              -- mov e/rdx,src (var no)
                    emitHex5callG(opLen)                            -- call :%opLen
                    -- NB: src is determined from [esp]-9 on error [DEV]
                    -- all regs trashed, unless result is integer, in ecx (and [dest]), esi untouched
                    if dtype=T_integer then
                        clearReg(edi)
    --              ?regstate = transitions[regstate][esi]  ?promote over edi?
    --              ?regstate = transitions[regstate][3]        ?promote esi over edi?
if newEmit then
                        storeReg(esi,src,0,1)   -- just record that esi==[src]
else
                        storeReg(eax,src,0,1)   -- just record that eax==[src]
end if
                        storeReg(ecx,dest,0,1)  -- just record that ecx==[dest]
                    else
                        reginfo = 0
                    end if
                end if -- inline
            end if -- not isGscan
--          pc += 5

        elsif opcode=opApnd             -- 106
           or opcode=opPpnd then        -- 107
            dest = s5[pc+1] -- dest
            src = s5[pc+2]  -- seq [not necc a sequence anymore]
            src2 = s5[pc+3] -- item
            pc += 4
--if isGscan then
--DEV p1 is sequence, length max is MAXLEN
            getDest()
--if src=dest then
-- see comment in opConcat. while first param to append must be sequence, we know,
--  if it /might/ be an atom then all bets are off... or are they???...
--   (not that it is a major burden to leave something {object,min,max,object,-2}...)
            if src=dest and not and_bits(vroot,T_atom) then
                -- dest=ap/prepend(dest,x) case
                -- (can take part info but /only if/ dest cannot be atom)
                if dtype>T_object then ?9/0 end if
                slroot = dtype
--              if dtype then
--                  sltype = dtype
--              else
--                  sltype = and_bits(vroot,T_sequence)
--              end if
                smin = dmin
                smax = dmax
                setyp = detyp
                slen = -2   -- may as well
            else
                getSrc()    -- the sequence
            end if
--DEV: (untested) actually I think this would probably be OK:
--DEV/NB: dest=append(blah,dest) /cannot/ take part info...??
--      eg/ie dest=1; dest=append({1},dest) ==> sequence of integer, but...
--             a later dest={1} would /NOT/ make etype sequence, iyswim.
--              (actually that soi is immediately wrong all by itself)
--          if src2=dest and not and_bits(vroot,T_atom) then
--              sltype2 = dtype
--              smin2 = dmin
--              smax2 = dmax
--              setyp2 = detyp
--              slen2 = -2
--          else
            getSrc2()   -- the new element
--          end if
            setyp = or_bits(setyp,slroot2)
            -- check for x=append(string,non-char)
            slroot = and_bits(slroot,T_sequence)
            if slroot=T_string then
                if slroot2!=T_integer
                or smin2<0 or smax2>255 then
                    slroot = T_sequence
                end if
            end if
            sltype = slroot
--DEV (untried)
--          if src!=dest and slen>=0 then
--              slen += 1
--          else
            slen = -2
--          end if
            forceSeq = 1
--?:
--  smin = dmin
--  smax = dmax
--?{src,stype,vroot,setyp,slen}
--puts(1,"opApnd:")
--?symtab[dest]
            storeDest()
            --else
            if not isGscan then
                markConstUseds({dest,src,src2})
--              if sched then
--                  sch00n = schoon
--                  schedule(0,0,edxbit,pUV,0,0)
--              end if
                leamov(edx,dest)                                -- lea edx,[dest]/mov edx,addr dest
--              if sched then
--                  schedule(0,0,edibit,pUV,0,0)
--              end if
                leamov(edi,src)                                 -- lea edi,[src]/mov edi,src
--              if sched then
--                  schedule(0,0,ecxbit,pUV,0,0)
--              end if
                leamov(ecx,src2)                                -- lea ecx,[src2]/mov ecx,addr src2
--              if sched then
--                  schend()
--              end if
                if opcode=opApnd then
                    if X64 then
                        emitHex1(#48)
                    end if
                    emitHex2s(xor_eax_eax)                      -- xor eax,eax (eax:=0)
                elsif opcode=opPpnd then
                    movRegImm32(eax,1)                          -- mov eax,1
                else
                    ?9/0
                end if
--              emitHex5callG(opcode)                           -- call :%opApnd/Ppnd (this line wd work fine btw)
                emitHex5callG(opApnd)                           -- call :%opApnd (same for both opApnd and opPpnd)
                reginfo = 0 -- all regs trashed
            end if
        elsif opcode=opMkSq then        -- 16
            waspc = pc
            noofitems = s5[pc+1]
            dest = s5[pc+2]
            pc += 3
--if isGscan then
            getDest()
--          sltype = or_bits(dtype,T_Dsq)
            sltype = T_Dsq
            slen = noofitems
            -- accumulate element types:
            setyp = 0
            while noofitems do
                src2 = s5[pc]
                getSrc2()
                setyp = or_bits(setyp,slroot2)
                if setyp=T_object then -- (full set of bits, that further or_bits will not change)
                    pc += noofitems
                    exit
                end if
                noofitems -= 1
                pc += 1
            end while
            slroot = sltype
--?:
--  smin = dmin
--  smax = dmax
            storeDest()
            --else
            if not isGscan then
                pc = waspc
                noofitems = s5[pc+1]    -- as damaged above
--; calling convention:
--; mov edx,length  ; (aka n)
--; mov esi,dest    ; dest addr
--; mov al,soi      ; 1 if tgt is sequence of integer (and therefore not an element)
--; push return addr
--; push esi        ; dest addr
--; push [e1]..[en]
--; jmp opMkSq
--              if sched then
--                  sch00n = schoon
--                  schedule(0,0,edxbit,pUV,0,0)
--              end if
                leamov(eax,dest)
--              emitHex5w(mov_edx_imm32,noofitems)                  -- mov edx,noofitems (literal int)
                movRegImm32(edx,noofitems)                          -- mov edx,noofitems (literal int)
--              if sched then
--                  schedule(0,0,esibit,pUV,0,0)
--              end if
                getDest()
--              if sched then
--                  schend()    -- isAddr not relocatable
--              end if
                raoffset = emitHex5addr()                           -- push <return addr> [backpatched below]
                emitHex1(push_eax)                                  -- push dest addr (leamov'd above)
                pc += 3
                for j=1 to noofitems do
                    -- push a pre-incref'd ref (or push temps and zero them)
                    src = s5[pc]
                    getSrc()
                    if slroot=T_integer then
--30/12/14:
--                      if smin=smax then
--                      if smin=smax and not and_bits(state1,K_rtn) then
                        if integer(smin) and smin=smax and not and_bits(state1,K_rtn) then
--                      if not isFLOAT(smin) and smin=smax and not and_bits(state1,K_rtn) then
                            -- a known integer value
                            if smin>=-128 and smin<=127 then
                                emitHex2(push_imm8,smin)
                            elsif smin>=-#80000000 and smin<=#7FFFFFFF then
                                emitHex5w(push_imm32,smin)          -- push imm32
                            else
--?"pilxl86.e line 10003 just sayin hi"
--                              emit ??
--      x86 &= {#49,#BF,0,0,0,0,0,0,0,#40}  -- mov r15,h4   (10 byte ins)
                                x86 &= {#48,#B8}
                                emitHexQuadword(smin)
                                emitHex1(push_eax)
                            end if
                        else
                            reg = loadReg(src,NOLOAD)               -- is [src] already loaded?
                            if reg!=-1 then
                                emitHex1(push_eax+reg)              -- push reg
                            else
                                pushvar(src)                        -- push dword[src]
                            end if
                        end if
--25/4/21:
--                  elsif ssNTyp1=S_TVar
                    elsif (ssNTyp1=S_TVar or ssNTyp1=S_GVar2)
                      and symtab[src][S_Name]=-1 then
--                  elsif symtab[src][S_Name]=-1 then
--DEV may need some more work here if we don't automatically saveFunctionResultVars all the time in pmain.e...
                        -- a temp or function return var:
                        reg = loadReg(src,NOLOAD)                   -- is [src] already loaded?
                        if reg!=-1 then
                            emitHex1(push_eax+reg)                  -- push reg
                        else
                            pushvar(src)                            -- push dword[src]
                        end if
                        zero(src)                                   -- mov [src],ebx(0)
                    elsif not newEmit
                      and ssNTyp1=S_Const 
                      and and_bits(state1,K_noclr) then
                        emitHex6constrefcount(inc_mem32,src)        -- inc dword[#xxxxxxxx]
                        emitHex5constref(push_imm32,src)            -- push #xxxxxxxx
                    else
                        reg = loadReg(src)                          -- mov reg,[src]
                        cmp_h4(reg)                                 -- cmp reg,h4
--DEVBPM backpatch me: [DONE]
--                      emitHex6j(jl_rel32,9)                       -- jl @f [sj NOT ok]
                        emitHex6j(jl_rel32,0)                       -- jl @f [sj NOT ok]
                        backpatch = length(x86)
                        sib = 0o203+reg*8 -- 0o2r3, ebx+reg*4
                        -- NB: the inc /CAN/ get an exception for unassigned vars, hence we
                        --      emit the following helper instruction for pdiag.e. There is no 
                        --      penalty for doing this, inc is three clocks with or without it.
if newEmit then
                        if X64 then
                            if reg>7 then ?9/0 end if -- (placeholder for more code)
                            emitHex1(#48)
                            emitHex5sib(addd_subd8i8,sib,-16,1)     -- add qword[rbx+reg*4-16],1    ; incref
                        else
                            emitHex5sib(addd_subd8i8,sib,-8,1)      -- add dword[ebx+reg*4-8],1     ; incref
                        end if
                        cmp_eax_srcid(src)                          -- cmp eax,src_id   (effectively a no-op)
else
                        emitHex4sib(incd_sib,sib,-8)                -- inc dword[ebx+reg*4-8]       ; incref
                        emitHex5w(cmp_eax_imm32,src)                -- cmp eax,src_id (effectively a no-op)
end if
                        x86[backpatch] = length(x86)-backpatch      -- @@:
                        emitHex1(push_eax+reg)                      -- push reg
                    end if
                    pc += 1
                end for
                loadToReg(edi,dest)                                 -- mov edi,[dest] (prev)
                emitHex5jmpG(opMkSq)
--DEV:
--  if newEmit then
--                  emitHex5jmpG(opMkSq)
--  else
--                  -- (both these are actually a call, return pushed above)
--                  if dtype=T_Dsq and detyp=T_integer then
--                      emitHex5jmpop(opMkSqi)                          -- jmp opMkSqi
--                  else
--                      emitHex5jmpop(opMkSq)                           -- jmp opMkSq
--                  end if
--  end if
                -- set return addr offset:
                x86[raoffset] = length(x86)-raoffset
                reginfo = 0 -- all regs trashed                     -- <return addr>:
            end if

        elsif opcode=opRepe1 then       -- 22
            -- opRepe1,dest,idx,rep     ; aka dest[src]=src2, or ref[idx]:=rep

            dest = s5[pc+1]     -- ref
            src = s5[pc+2]      -- idx
            src2 = s5[pc+3]     -- rep
            pc += 4
--if isGscan then
--DEV UNTESTED:
--DEV so p1 is a sequence or string, with p1[p2] existing,
--  if p3 is a char then it can be a string, if it is sequence(/string), or an atom with range outside
--  char (or possibly say between 41.01 and 41.99, aka floor(min)=floor(max) and both not integer)
--  then p1 must be dseq.

            getDest()
--24/6/10:
--if vlen<0 then
--18/1/12: (tried removing 21/2/14, but it immediately broke self-host badly)
if vlen<0 and dlen!=-2 then
--  printf(1,"setting dlen to %d, was %d\n",{vlen,dlen})
    dlen = vlen
end if

--DEV/NB: dest[idx]=dest cannot take part info (unlike append/prepend/concat[N])
            getSrc2()
--          setyp = or_bits(detyp,stype2)
            -- check for string[i]=non-char:

            --DEV/SUG: use slroot throughout here

--DEV triggered under NOLT 17/10/09:
--DEV put back 26/1/10 (and 3/8/9 change to getDest undone)
--  (reason: ?9/0 line 7792/opRmdr)
            if dtype>T_object then ?9/0 end if
--          if dtype>T_object then dtype = rootType(dtype) end if
            if detyp>T_object then ?9/0 end if
--if sltype>T_object then ?9/0 end if
            sltype = and_bits(dtype,T_sequence)
--DEV 12/10/09:
--sltype = rootType(vtype)
--sltype = and_bits(rootType(vtype),and_bits(dtype,T_sequence))
            --24/9:
            forceSeq = 1
--8/6/10: (nope!)
            setyp = or_bits(detyp,slroot2)
--          setyp = or_bits(vetyp,slroot2)
--DEV 12/10/9:
--          setyp = slroot2

            if sltype=T_string then
                if slroot2!=T_integer
                or smin2<0 or smax2>255 then
                    sltype = T_sequence
                end if
            end if

            slroot = sltype
            slen = dlen
            smin = dmin
            smax = dmax
            storeDest()

            if not isGscan then
                markConstUseds({dest,src,src2})
--              if sched then
----            if schidx then
--                  schend()    -- [esp]-15/-9/-21
----            end if
--              end if
                --DEV what's wrong with dtype here??	[20/9 erm, it may be a roottype!]
--DEV why am I not using dtype and detyp here??
--trace(1)
                if sltype=T_string then
-- 26/7/2014 [DEV] this broke in pbinary.e (independent workaround applied), so I'm going to drop it.
if newEmit then -- resurrected 9/10/14 (for newEmit only!)
                    opcode = opRepe1is
end if
                elsif sltype=T_Dsq
--8/6/10
--                and setyp=T_integer then
                  and vetyp=T_integer then
--may need "and bind" here... (try x={1,2,3} x[2]="fred", interpreted)
                    opcode = opRepe1ip
----DEV 12/10/9 (third try)
--elsif rootType(vtype)=T_Dsq
--  and detyp=T_integer then
--elsif blroot=T_Dsq
--  and bmin=T_integer  -- saved setyp of dest
--  and slroot=T_integer then
--  opcode = opRepe1ip
                end if
                -- first, find out what's already loaded
                ref = loadReg(dest,NOLOAD)
                if tmpd!=src2
                or slroot2!=T_integer or smin2!=smax2 then
                    rep = loadReg(src2,NOLOAD)
                else
                    rep = -1
                end if
                if tmpd=src then
                    if tmpr>=0 then
                        idx = tmpr
                    else
                        idx = -1                        
                    end if
                else
                    idx = loadReg(src,NOLOAD)
                end if
                -- then move without trashing anything
                if ref!=-1
                and ref!=esi then
                    xrm = #C6 + ref*8 -- 0o3r6
                    if X64 then
                        if ref>7 then ?9/0 end if -- (placeholder for more code)
--                      if rep>7 then ?9/0 end if -- (placeholder for more code)
                    end if
                    if rep=esi then
                        emitHexx2(xchg,xrm)                     -- xchg ref,rep(=esi)
                        rep = ref
                        -- in case ref==ecx:
                        storeReg(rep,src2,0,1) -- (rep==[src2])
                    elsif idx=esi then
                        emitHexx2(xchg,xrm)                     -- xchg ref,idx(=esi)
                        idx = ref
                        -- in case idx==edi:
                        storeReg(idx,src,0,1) -- (idx==[src])
                    else
                        emitHexx2(mov_reg,xrm)                  -- mov esi,ref
                    end if
                    ref = esi
                    storeReg(esi,dest,0,1) -- (esi==[dest])
                end if
                if rep!=-1
                and rep!=ecx then
                    xrm = #C1 + rep*8 -- 0o3r1
                    if X64 then
                        if rep>7 then ?9/0 end if -- (placeholder for more code)
--                      if tgtreg>7 then ?9/0 end if -- (placeholder for more code)
                    end if
                    if idx=ecx then
                        emitHexx2(xchg,xrm)                     -- xchg rep,idx(=ecx)
                        idx = rep
                        -- in case idx==edi:
                        storeReg(idx,src,0,1) -- (idx==[src])
                    else
                        emitHexx2(mov_reg,xrm)                  -- mov ecx,rep
                    end if
                    rep = ecx
                    storeReg(ecx,src2,0,1) -- (ecx==[src2])
                end if
                if idx!=-1
                and idx!=edi then
                    xrm = #C7 + idx*8 -- 0o3r7
                    if X64 then
                        if idx>7 then ?9/0 end if -- (placeholder for more code)
                    end if
                    emitHexx2(mov_reg,xrm)                      -- mov edi,idx
                    idx = edi
                    storeReg(edi,src,0,1) -- (edi==[src])
                end if

--              if tmpd then
--                  if tmpd!=src then ?9/0 end if
if idx!=edi then
                if tmpd=src then
                    if tmpr>=0 then
                        if tmpr!=edi then
                            xrm = 0o370+tmpr    -- 0o37r
                            if X64 then
                                if tmpr>7 then ?9/0 end if -- (placeholder for more code)
                            end if
                            emitHexx2(mov_dword,xrm)    -- mov edi,tmpr
                        end if
                    else
--                      emitHex5w(mov_edi_imm32,tmpv)   -- mov edi,imm32
                        movRegImm32(edi,tmpv)           -- mov edi,imm32
                    end if
                    clearReg(edi)
--                  tmpd = 0
--DEV tryme pt1of2 (new code):
--              elsif slroot=T_integer and smin=smax then
--                  emitHex5w(mov_edi_imm32,smin)       -- mov edi,imm32
                else
                    loadToReg(edi,src)                  -- mov edi,[src]        ; idx
                end if
end if
if rep!=ecx then
--              if tmpd
--              if tmpd=src2
--DEV tryme pt2of2 (replacing the line above):
--              if (tmpd or (slroot=T_integer and smin=smax))
--              and slroot2=T_integer and smin2=smax2 then
                if slroot2=T_integer and smin2=smax2 then
--                  if tmpd!=src2 then ?9/0 end if
--                  emitHex5w(mov_ecx_imm32,smin2)      -- mov ecx,imm32
                    movRegImm32(ecx,smin2)              -- mov ecx,imm32
                else
                    loadToReg(ecx, src2)                -- mov ecx,[src2]       ; rep
                end if
end if
--DEV
--clearReg(ecx)
                tmpd = 0
                if ref!=esi then
                    loadToReg(esi,dest)                 -- mov esi,[dest]       ; ref
                end if
                leamov(eax,dest)                        -- lea/mov eax,addr ref
                emitHex5callG(opcode)                   -- call opRepe1[is/p]   ; ref[idx]:=rep
                -- NB addr idx/rep/ref obtained from [esp]-21/-15/-9 on error [DEV]
                reginfo = 0 -- all regs trashed
-- as above (26/7/14)
                if opcode=opRepe1is         -- opRepe1is has a builtin (char) typecheck,
                or opcode=opRepe1ip then    -- and/or gscan has proved rep is an integer.
--              if opcode=opRepe1ip then    -- and/or gscan has proved rep is an integer.
                    -- hence we can safely omit builtin typechecks, but not udts:
                    if symtab[dest][S_vtype]<=T_object then -- declared type
--DEV tryme: (erm, this is probably wrong... do we want dudt/dvtype?)
--                  if dtype<=T_object then
                        opcode = s5[pc]
                        if opcode=opTchk then
                            if s5[pc+1]!=dest then ?9/0 end if
                            pc += 4
                        end if
                    end if
                end if
            end if

        elsif opcode=opJtyp then
            -- implements eg if sequence(x) then

            src = s5[pc+7]
            invert = s5[pc+8]
            ltype = s5[pc+9]        -- T_integer/T_atom/T_sequence/T_string
            if ltype>T_object then ?9/0 end if
            sltype2 = ltype
            getSrc()
--yep, already 0 here...
--if slroot=0 then printf(1,"pilx86.e line 9882 (opJtyp): slroot=0 (src=%d)\n",src) x86showmapsymtab = src end if

if NOLT=0 or bind or lint then
            if not and_bits(state1,K_Fres) then
                if not invert then
                    sltype2 = xor_bits(sltype2,T_object)
                end if
--26/6/20 this is not getting undone, so I've just removed it completely...
--/*
                if sltype!=sltype2 then
                    Lmin = smin
                    Lmax = smax
                    Letyp = setyp
                    Lslen = slen
                    flippable = s5[pc+5]
                    ltAdd(TEST+flippable,src,sltype,sltype2,pc)
                    -- (nb do not getSrc() again below)
                end if
--*/
            end if  -- K_Fres
end if -- NOLT

--          if isGscan then
            if isGscan and not NEWGSCAN then
                pc += 10
            else
                --
                -- opJtyp,mergeSet,tgt,link,0,0(/flippable),0(/isInit),src,invert,ltype
                --
                isInit = s5[pc+6] --see above
                jinit(pc+1)
                flag = -1
                lmask = xor_bits(T_object,ltype)
                --
                -- We can prove some things are always true/false here, eg
                --  sequence(integer) is always false (0b1100,0b0001 => flag:=1) [as no bits match]
                --  atom(integer) is always true (0b0011,0b0001 => flag:=0) [as and_bits(0b1100,0b0001)==0]
                --  string(sequence) is unknown (0b1000,0b1100 => leave flag -1)
                --
                -- A suitable test (using -list, ie in the full-compile case, -d! may differ):
                --
                --          procedure p(object o)
                --              -- There should be no code emitted for this:
                --              if sequence(o) then puts(1,"ugh?\n") end if
                --              -- The following two lines should emit exactly the same code:
                --              if integer(o) then puts(1,"yep\n") end if
                --              puts(1,"yep\n")
                --          end procedure
                --          p(1)  --(because we only call it like this, with an integer parameter)
                --
                -- Note that front-end generated "probable logic errors" are
                --  similar but different to this handling: here we may have a
                --  more precise value for slroot, following repeated analysis,
                --  and here we're more concerned with stripping out code from
                --  general library routines, which the current program does 
                --  not need, but others might.
                --
-->slroot==0???
--DEV/temp/ugh... (15/2/19)
--if slroot=0 then printf(1,"pilx86.e line 9936 (opJtyp): slroot=0 (src=%d)\n",src) x86showmapsymtab = src end if
--if slroot!=0 then
                if not and_bits(ltype,slroot) then
                    flag = 1
                elsif not and_bits(lmask,slroot) then
                    flag = 0
                end if
--end if
--              if sched then
----        if mergeSet=isOpCode then   ? (seems far too messy to schedule this lot anyways)
--                  sch00n = schoon
----            if mergeSet!=isOpCode then
----                sch00n = lastJmp    -- (can schedule non-last-jumps)
----            end if
--              end if
                backpatch = 0
                if flag=-1 then
if not isGscan then
                    -- we must test
--DEV and ltype!=T_object?
                    if not isInit then -- non-init param
--                      if sched then
--                          schend()    -- [esp-9]
--                      end if
                        loadToReg(eax,src)                          -- mov eax,[src] (var ref)
--DEV 18/1/2013 expect errors here...
                        ltype = opTyp0[ltype]
                        movRegVno(ecx,src)                          -- mov e/rcx,varno of src
                        clearReg(ecx)
                        emitHex5callG(ltype)                        -- call :%opInt0/Atom0/Sq0/Str0
                        -- NB: src is determined from [esp]-9 on error [DEV testme]
                        --  edx is now 0/1, and eax is still [src]
                        storeReg(eax,src,0,1) -- just record that eax==[src]
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(test_edx_edx)                     -- test edx,edx
                        bcode = 4-invert    -- invert=jz[3], not invert=jnz[4] (idx to ccde)
                    else -- isInit
                        reg = loadReg(src)                          -- mov reg,[src]
                        if and_bits(slroot,T_integer) then
--                          if sched then
--                              rb = regbit[reg+1]
--                              schedule(rb,0,0,pNP,1,0) -- treat remainder as one instruction
--                          end if
                            cmp_h4(reg)                             -- cmp reg,h4
--                      else
--  if sched then
--                          rb = regbit[reg+1]
--                          schedule(rb,rb,0,pNP,1,0) -- treat remainder as one instruction
--  end if

                        end if
--                      if sched then
--                          schend()    -- must catch jmp 1 of 2...
--                      end if
--                      bcode = 0   -- temp, ioob if we fail to set it
                        if ltype=T_integer then
                            bcode = 1+invert*5  -- invert=jg[6], not invert=jl[1]   (idx to ccde)
                        else
                            sib = #83+reg*8 -- 0o2r3, ebx+reg*4
                            if ltype=T_sequence or ltype=T_string then
                                if and_bits(slroot,T_integer) then
                                    slroot -= T_integer
                                    slroot = and_bits(slroot,lmask)
                                    if slroot then
--if sched then
--  schedule(0,0,0,pV,1,0)
--end if
                                        if invert then
                                            if mergeSet=isOpCode then
                                                emitHex6retg(jl_rel32)              -- jl opRetf
                                            else
                                                emitHex6j(jl_rel32,0)               -- jl xxx (backpatched later)
-- Since we have two jumps to backpatch, aim for:
--      opJtyp,mergeSet[1],x86[2],il[3],mergeSet[4],86[5],il[6],src,invert,ltype
--   with [6] ending up pointing at [3], as tgt already points at 3, and as
--   [2] will be filled in below for the one jump case.
--   (pltype.e must check for this case (6 pointing at 3) and skip it)
                                                s5[pc+4] = mergeSet
                                                s5[pc+5] = length(x86)
                                                k = tgt
                                                pc3 = pc+3
                                                while s5[k]!=pc3 do
                                                    k = s5[k]
                                                end while
                                                pc6 = pc+6
                                                s5[pc6] = pc3
                                                s5[k] = pc6
                                            end if
                                        else
--DEVBPM backpatch me: [DONE]
--printf(1,"backpatch line 9703 pilx86.e, (emitline=%d, %s)\n",{emitline,filenames[symtab[vi][S_FPno]][2]})
--                                          emitHex6j(jl_rel32,11)                  -- jl @f [sj NOT ok]
                                            emitHex6j(jl_rel32,0)                   -- jl @f [sj NOT ok]
                                            backpatch = length(x86)
                                        end if
                                    else
                                        bcode = 6-invert*5  -- invert=jl[1], not invert=jg[6]   (idx to ccde)
                                    end if
                                end if
                                if slroot then
--if sched then
--  rb = regbit[reg+1]
--  schedule(0,ebxbit+rb,0,pU,1,0)
--end if
                                    if ltype=T_sequence then
                                        emitHex5sib(tstb_sibd8i8,sib,-1,#80)        -- test byte[ebx+reg*4-1],0x80
                                        invert = not invert
                                    else -- T_string
                                        emitHex5sib(cmpb_sibd8i8,sib,-1,#82)        -- cmp byte[ebx+reg*4-1],0x82
                                    end if
                                end if
                            else -- ltype = T_atom
                                if and_bits(slroot,T_integer) then
                                    slroot = and_bits(slroot,T_N)
                                    if slroot then
--if sched then
--  schedule(0,0,0,pV,1,0)
--end if
                                        if invert then
--DEVBPM backpatch me: [DONE]
--printf(1,"backpatch line 9731 pilx86.e, (emitline=%d, %s)\n",{emitline,filenames[symtab[vi][S_FPno]][2]})
--                                          emitHex6j(jl_rel32,11)                  -- jl @f [sj NOT ok]
                                            emitHex6j(jl_rel32,0)                   -- jl @f [sj NOT ok]
                                            backpatch = length(x86)
                                        else
                                            if mergeSet=isOpCode then
                                                emitHex6retg(jl_rel32)              -- jl opRetf
                                            else
                                                emitHex6j(jl_rel32,0)               -- jl xxx (backpatched later)
                                                --  as above, two jumps to link:
                                                s5[pc+4] = mergeSet
                                                s5[pc+5] = length(x86)
                                                k = tgt
                                                pc3 = pc+3
                                                while s5[k]!=pc3 do
                                                    k = s5[k]
                                                end while
                                                pc6 = pc+6
                                                s5[pc6] = pc3
                                                s5[k] = pc6
                                            end if
                                        end if
                                    else
                                        bcode = 1+invert*5  -- invert=jg[6], not invert=jl[1] (idx to ccde)
                                    end if
                                end if
                                if slroot then
--  if sched then
--                              rb = regbit[reg+1]
--                              schedule(0,ebxbit+rb,0,pU,1,0)
--  end if
                                    emitHex5sib(cmpb_sibd8i8,sib,-1,#12)            -- cmp byte[ebx+reg*4-1],0x12
                                end if
                            end if
                            if slroot then
                                bcode = 3+invert    -- invert=jnz[4], not invert=jz[3] (idx to ccde)
                            end if
                        end if
                    end if -- isInit
end if -- (not isGscan)
                else    -- flag!=-1
                    tmpd = 1                -- known
                    tmpv = (invert=flag)    -- taken/not taken
                    bcode = 0               -- (not needed)
                end if
                -- Jtyp:
                if jend(10) then exit end if -- an opRetf fell off end of code, --> end while
                if backpatch!=0 then
                    x86[backpatch] = length(x86)-backpatch                      -- @@:
                end if
            end if

        elsif opcode=opConcat then      -- 108
            dest = s5[pc+1]     -- dest=src&src2
            src = s5[pc+2]
            src2 = s5[pc+3]
            pc += 4
--if isGscan then
            getDest()

            if src=dest and not and_bits(vroot,T_atom) then
--DEV/NB: any of dest=dest&blah, dest&=blah, dest=blah&dest /can/ take part info:
--  ah: as long as dest must be a sequence, eg/ie dest=1 dest=dest&dest would be a
--      sequence of integer, but dest=1.5/{}/"" would not then impact etype properly
--      (whereas dest={1}/{1.5}/{{}}/{""} would be just fine).
-- aka (4/4/9):
--  for i=1 to 2 do
--      if i=2 then
--          dest=dest&dest  -- sequence of 0
--      else
--          dest=3          -- oops... (does not suddenly make dest sequence of integer)
--      end if
--  end for
--  (OK, contrived, but the point is clear: that should gscan hit the & before the 3,
--   it cannot take part info; (whereas .. fine) above stands true.)
                sltype = dtype
-- not needed:
--              smin = dmin
--              smax = dmax
                setyp = detyp
                slen = -2   -- may as well
            else
                getSrc()
            end if
            mtype = and_bits(slroot,T_atom)
            setyp = or_bits(setyp,mtype)

            if src2=dest and not and_bits(vroot,T_atom) then
                sltype2 = dtype
                --              slroot2 = rootType(dtype)   --?? (20/9...)
                if dtype>T_object then ?9/0 end if
                slroot2 = dtype
--              smin2 = dmin
--              smax2 = dmax
                setyp2 = detyp
                slen2 = -2  -- may as well
            else
                getSrc2()
            end if
            mtype = and_bits(slroot2,T_atom)
            setyp = or_bits(setyp,mtype)
--printf(1,"setyp:%d\n",setyp)
-- 27/4:
            if and_bits(slroot2,T_sequence) then
                setyp = or_bits(setyp,setyp2)
            end if

            -- if src/src2 can be atom, need to update detyp:
            mtype = or_bits(slroot,slroot2)
            slroot = and_bits(mtype,T_sequence)
            ltype = and_bits(mtype,T_atom)
            if ltype then
--              detyp = or_bits(detyp,ltype)
                if slroot=T_integer
                and slroot2=T_integer
                and smin>=0 and smax<=255
                and smin2>=0 and smax2<=255 then
                    -- char & char is a string
                    slroot = T_string
                else
                    -- a & b, where either is provably NOT a char,
                    --  must yield T_Dsq, otherwise we cannot tell
                    --  & must mark dest T_sequence (T_Dsq|T_str):
                    if and_bits(slroot,T_integer)=0
                    or and_bits(slroot2,T_integer)=0
                    or smax<0 or smin>255
                    or smax2<0 or smin2>255 then
                        slroot = T_Dsq
                    else
                        slroot = T_sequence
                    end if
                end if
            end if
            -- update dtype with src/src2's string/seq...
--          mtype = and_bits(mtype,T_sequence)
--          if mtype then
--              slroot = or_bits(slroot,mtype)
                -- .. and detyp with corresponding setyp/2
--              mtype = or_bits(setyp,setyp2)
----                detyp = or_bits(detyp,mtype)
--              ltype = or_bits(ltype,mtype)
--          end if
--          setype = ltype
--          slroot = and_bits(slroot,T_sequence)
            if slen>=0 and slen2>=0 then
                slen += slen2
            else
                slen = -2
            end if
            sltype = slroot
            storeDest()
            --else
            if not isGscan then
                markConstUseds({dest,src,src2})
--28/11/09
--if bind                   -- (else no gvar_scan, so no type info)
----and not newEBP --DEV (rewrite opCatsi)  [done, test with newEmit]
--and dest=src              -- p1 &= p2, aka p1 = p1 & p2
--and dest!=src2                -- but not p1 &= p1 (erm, why exactly not [also 28/11]?!)
--  -- DEV: just removing the above "dest!=src2" should be ok, but untried, and needs much testing!
--and slroot=T_Dsq          -- \dest is sequence of integer, and hence
--and setyp=T_integer then  -- /p3 is in fact (int|sequence of integer)
--              --calling convention:                               octal:         binary:          code:
--              --  mov edi,[p1]    -- (ref)
--              --  lea edx,[p1]    -- (address)    -- (must be init sequence of integer)
--              --  lea esi,[p2]    --   ""         -- (must be init [sequence of] integer)
--              --  call :%opCatsi
--              if and_bits(symtab[dest][S_State],K_Fres) then ?9/0 end if -- (unlikely)
--              loadMem(edi,dest)
--              leamov(edx,dest)
--              leamov(esi,src2)                                    -- lea esi,[src2]/mov esi,addr src2
--              emitHex5callG(opCatsi)                              -- call :%opCatsi (in VM/pApnd.e)
--else -- not opCatsi
                -- (these now match opApnd)
                leamov(edx,dest)                                    -- lea/mov edx,addr result var
                leamov(edi,src)                                     -- lea edi,[src]/mov edi,addr src
                leamov(ecx,src2)                                    -- lea ecx,[src2]/mov ecx,addr src2
                emitHex5callG(opConcat)                             -- call :%opConcat (in VM/pApnd.e)
--end if
                reginfo = 0 -- all regs trashed
            end if

        elsif opcode=opLoopTop then     -- opLoopTop,lmask,gmask
--DEV14:
            if callpending then ?9/0 end if -- (shd not be emitted between opFrame & opCall)
if NOLT=0 or bind or lint then
            ltlooptop(pc)
end if -- NOLT (opLoopTop should not actually be emitted?)
--          pc += 3
            pc += 4
            opcode = lastop

        elsif opcode=opFor2 then
--DEV: "for i=1 to 4 do" should really just "mov [i],1"...
--if isGscan then
--if pc=87 then trace(1) end if
--if not isGscan then
--  trace(1)
--end if
--          flags = s5[pc+1]
            src = s5[pc+2]  -- initial value
            dest = s5[pc+3] -- control var
            getSrc()
            nMin = smin -- save init for use below
            nMax = smax
            iroot = slroot
            getDest()
            src2 = s5[pc+4] -- step value
            src = s5[pc+5]  -- limit value
            if and_bits(slroot,T_N) then -- (init)
                -- so min/max (of NI) are meaningless, hence we
                --  cannot deduce anything about dest (N).
                slroot = T_atom
            else
                getSrc()    -- (limit)
                getSrc2()   -- (step)
                if and_bits(slroot,T_N)
                or and_bits(slroot2,T_N) then
                    slroot = T_atom
                else
                    slroot = T_integer
                    if nMin<smin then smin = nMin end if
                    if nMax>smax then smax = nMax end if
                end if
            end if
            if slroot=T_atom then
                smin = MININT
                smax = MAXINT
            end if
            sltype = slroot
            storeDest()
            npc = 0
--DEV add zero iterations test here... (see "triggered 3/6/2012" below)
            if not isGscan then
                flags = s5[pc+1]
                p1 = s5[pc+2]   -- initial value
--              reg = s5[pc+3]  -- control var      [==dest]
--              step = s5[pc+4] -- step value       [==src2]
--              lim = s5[pc+5]  -- limit value      [==src]
                tgt = s5[pc+6]
                if s5[tgt-2]!=opEndFor then ?9/0 end if
                if s5[tgt]!=pc+6 then ?9/0 end if
--              markConstUseds({p1,step,lim})
                markConstUseds({p1,src2,src})
--              if sched then
--                  schend()    -- [esp]-26/-9/-15
--              end if
--              loadToReg(eax,p1)                   -- mov eax,[p1] (initial value)
--              leamov(edi,reg)                     -- lea/mov edi,p2 (control var addr)
--              loadToReg(ecx, step)                -- mov ecx,[p2] (step value)
--              loadToReg(esi,lim)                  -- mov esi,[p4] (limit value)
--  no          emitHex5call(opFor)                 -- call opFor (check ints, set carry for zero iterations)
                -- NB p1/3/4 obtained from [esp]-26/-15/-9  [DEV?][DEV newEBP!]
                -- edx trashed, all other regs preserved (not that we can take any advantage of that)
--              reginfo = 0 -- ??
--NEW:
--  opFor2,flags,init,ctl,step,limit,tgt/x86loc
--          ^^^ new
    --
    -- Note There is a little bit more to for loops than just the typical
    --      "for i=1 to length(s) do". Such a loop should emit relatively
    --      little code, but some for loop constructs can generate quite
    --      a surprisingly large amount of machine instructions:
    --
    --      The control variable may already exist, in which case it persists
    --      after the end of the loop; it may also have been declared as atom
    --      or object, and may need a decref. A test may or may not be needed
    --      at the start of the loop to check for zero iterations. The limit,
    --      initial value, and step may all be literal or variable, and hence
    --      in the latter case need checking for unassigned, and (see below)
    --      whether they are (currently) floating-point. Lastly, when the
    --      step is >0, the loop terminates when ctrl>limit, whereas when it
    --      is <0 (obviously 0 is an error), it terminates when ctrl<limit. 
    --      If we don't know the sign of step at compile-time (eg t29for.exw)
    --      then we must emit both, and test step to decide which applies.
    --
    --DEV/DOC:
    --      Aside: In eg for i=length(s) to 1 do, /you/ might think we know
    --              which way to go (ie step=-1) but in fact, we don't. We
    --              may (deliberately) want to iterate {0,1} (ie step=+1)
    --              when length(s) is 0. Without a by clause, a step of +1
    --              is /always/ assumed, and seems the only sensible way.
    --
    --      In Phix, the control variable/values must be integer, because 
    --      floating-point loops are inherently unreliable, for example 
    --      try the following on RDS Eu/OpenEu:
    --          for i=0.6 to 1 by 0.1 do
    --              printf(1,"%f ",i)
    --          end for
    --          puts(1,"\n")
    --          for i=1.6 to 2 by 0.1 do
    --              printf(1,"%f ",i)
    --          end for
    --          puts(1,"\n")
    --      And you will get the output:
    --          0.600000 0.700000 0.800000 0.900000 1.000000
    --          1.600000 1.700000 1.800000 1.900000
    --      The second loop performs one less iteration. Therefore I took
    --      the (unilateral) decision to bar them. Ultimately, of course, 
    --      Phix is open source so feel free to experiment. While I have 
    --      no plans, I am not against re-allowing floating-point loops, 
    --      as long as the vast majority of (simple) loops run no slower.
    --
    --      One subtle consequence (of prohibiting floating-point loops and 
    --      emitting accordingly optimised code) is that (limit+step) must 
    --      also be within the valid 31-bit (/63-bit!) integer range.
    --
                if sltype!=T_integer then
--              and not and_bits(symtab[dest][S_State],S_for) then
                    -- control var pre-declared as non-integer, may need dealloc:
                    prev = loadReg(dest,NOLOAD)                 -- (if already loaded, promote)
                    if prev=-1 then -- not already loaded
                        prev = edx
                        loadMem(edx,dest)                       -- mov edx,[dest] (ctrl var)
                    end if
                    cmp_h4(prev)                                -- cmp prev,h4
                    emitHex6j(jle_rel32,0)                      -- jle @f [sj prolly ok]
                    backpatch = length(x86)
                    sib = 0o203+prev*8 -- 0o2p3
                    if X64 then
                        if prev>7 then ?9/0 end if -- (placeholder for more code)
                        emitHex1(#48)
                        emitHex5sib(subd_sibd8i8,sib,-16,1)     --   sub dword[ebx+prev*4-16],1 (decref)
                    else
                        emitHex5sib(subd_sibd8i8,sib,-8,1)      --   sub dword[ebx+prev*4-8],1 (decref)
                    end if
                    emitHex6j(jnz_rel32,0)                      --   jnz @f [sj NOT ok]
                    backpatch2 = length(x86)
                    if prev!=edx then
                        xrm = 0o320+prev    -- 0o32r
                        emitHexx2(mov_dword,xrm)                -- mov edx,reg
                    end if
                    emitHex5callG(opDealloc)                    -- call :%pDealloc
                    x86[backpatch] = length(x86)-backpatch
                    x86[backpatch2] = length(x86)-backpatch2
                end if
                if iroot!=T_integer
                or nMin!=nMax then
                    reg = loadReg(p1)                           -- mov reg,[init]
                    if and_bits(flags,#01)=0 -- init was not init
                    or iroot!=T_integer then
                        cmp_h4(reg)                             -- cmp reg,h4
                        emitHex6j(jl_rel32,0)                   -- jl @f [sj prolly ok]
                        backpatch = length(x86)
                        emitHexx2(mov_al_imm8,120)              -- mov al,120 (for e120fle[init])
                        movRegVno(edi,p1)                       -- ep1 (=var no of init)
                        movRegImm32(esi,1)                      -- "init"
                        emitHex5callG(opRTErn)
                        x86[backpatch] = length(x86)-backpatch
                    end if
                    storeReg(reg,dest,1,0)                      -- mov [ctl],reg
                else
                    reg = -1
                    storeImm(dest,nMin)                         -- mov [ctl],imm32
                end if
                getSrc()    -- limit
                if and_bits(flags,#02)=0 -- limit was not init
                or slroot!=T_integer
                or smin!=smax then
                    limitreg = loadReg(src)                     -- mov limitreg,[limit]
                else
                    limitreg = -1   -- (use smin)
                end if
                getSrc2()   -- step
                if and_bits(flags,#04)=0 -- step not init
                or slroot2!=T_integer
                or smin2!=smax2 then
                    stepreg = loadReg(src2)                     -- mov stepreg,[step]
                    if and_bits(flags,#04)=0    -- step was not init
                    or slroot2!=T_integer then
                        cmp_h4(stepreg)                         -- cmp stepreg,h4
                        emitHex6j(jl_rel32,0)                   -- jl @f [sj prolly ok]
                        backpatch = length(x86)
                        emitHexx2(mov_al_imm8,120)              -- mov al,120 (for e120fle[step])
                        movRegVno(edi,src2)                     -- ep1 (=var no of step)
                        movRegImm32(esi,4)                      -- "step"
                        emitHex5callG(opRTErn)
                        x86[backpatch] = length(x86)-backpatch
                    end if
                else
                    stepreg = -1    -- (use smin2)
                end if
                chkreg = -1
-- added 4/6/2012: [DEV we may need a couple of "or not =T_integer" here]
--if not integer(smin+smin2) 
if isFLOAT(smin+smin2) 
--or not integer(smax+smax2) then
or isFLOAT(smax+smax2) then
                if limitreg!=-1 then
                    if and_bits(flags,#02)=0    -- limit was not init
                    or slroot!=T_integer then
                        cmp_h4(limitreg)                        -- cmp limitreg,h4
                        emitHex6j(jl_rel32,0)                   -- jl @f [sj prolly ok]
                        backpatch = length(x86)
                        emitHexx2(mov_al_imm8,120)              -- mov al,120 (for e120fle[limit])
                        movRegVno(edi,src)                      -- ep1 (=var no of limit)
                        movRegImm32(esi,2)                      -- "limit"
                        emitHex5callG(opRTErn)
                        x86[backpatch] = length(x86)-backpatch
                    end if  
                    if X64 then
                        emitHex1(#48)
                    end if
                    if stepreg!=-1 then
                        sib = 0o024 --(edx,exception case)
                        xrm = limitreg*8+stepreg -- 0o0ls
                        emitHex3l(lea,sib,xrm)                  -- lea edx,[limitreg+stepreg]
                    else
                        if smin2=0 then ?9/0 end if -- sanity check
                        if smin2<-128 or smin2>127 then
                            xrm = 0o220 + limitreg -- 0o2dr
                            emitHex62w(lea,xrm,smin2)           -- lea edx,[limitreg+nnnn]
                        else
                            xrm = 0o120 + limitreg -- 0o1dr
                            emitHex3l(lea,xrm,smin2)            -- lea edx,[limitreg+nn]
                        end if
                    end if
                    chkreg = edx
                elsif stepreg!=-1 then
                    if smin=0 then ?9/0 end if -- sanity check
                    if X64 then
                        emitHex1(#48)
                    end if
                    if smin<-128 or smin>127 then
                        xrm = 0o220 + stepreg -- 0o2dr
                        emitHex62w(lea,xrm,smin)                -- lea edx,[stepreg+nnnn]
                    else
                        xrm = 0o120 + stepreg -- 0o1dr
                        emitHex3l(lea,xrm,smin)                 -- lea edx,[stepreg+nn]
                    end if
                    chkreg = edx
--              elsif not integer(smin+smin2) then
                elsif isFLOAT(smin+smin2) then
                    ?9/0 --DEV make this a proper compilation error if needed
                end if
                if chkreg!=-1 then
                    if limitreg=edx or stepreg=edx then ?9/0 end if -- sanity check
                    if chkreg!=edx then ?9/0 end if                 -- ""
                    if X64 then
                        emitHex1(#48)
                    end if
                    emitHex2s(shl_edx_1)                        -- shl edx,1
                    emitHex6j(jno_rel32,0)                      -- jno @f [sj prolly ok]
                    backpatch = length(x86)
                    emitHexx2(mov_al_imm8,121)                  -- mov al,121 (for e121flelimstep)
                    movRegVno(edi,src)                          -- ep1 (=var no of limit)
                    movRegImm32(esi,src2)                       -- ep2 (=var no of step)
                    emitHex5callG(opRTErn)
                    x86[backpatch] = length(x86)-backpatch
                end if
end if
                backpatch = 0
                if (stepreg=-1 or slroot2=T_integer) and smin2>0 then
                    -- step +ve
                    if iroot!=T_integer     -- init not integer
                    or slroot!=T_integer    -- limit not integer
                    or nMax>smin then       -- init can be > limit
                        if reg=-1 then      -- init not loaded; use nMin (and reverse the condition)
--DEV triggered 3/6/2012: (see below)
--                          if limitreg=-1 then ?9/0 end if -- sanity check
if limitreg=-1 then
                            npc = tgt-2
else
                            cmp_imm32(limitreg,nMin)            -- cmp limit,imm32
                            emitHex6j(jl_rel32,0)               -- jl endfor [sj NOT ok]
end if
                        else
                            if limitreg=-1 then -- limit not loaded, use smin
                                cmp_imm32(reg,smin)             -- cmp init,imm32
                            else
                                xrm = 0o300+reg*8+limitreg
                                emitHexx2(cmp_reg_reg,xrm)      -- cmp init,limit
                            end if
                            emitHex6j(jg_rel32,0)               -- jg endfor [sj NOT ok]
                        end if
                        backpatch = length(x86)
                    end if
                elsif (stepreg=-1 or slroot2=T_integer) and smax2<0 then
                    -- step -ve
                    if iroot!=T_integer     -- init not integer
                    or slroot!=T_integer    -- limit not integer
                    or nMin<smax then       -- init can be < limit
                        if reg=-1 then      -- init not loaded; use nMin (and reverse the condition)
--triggered 3/6/2012: (put this back in as per the notes below)
--                          if limitreg=-1 then ?9/0 end if -- sanity check
if limitreg=-1 then
--DEV we should probably do this test earlier (see npc=0 above) 
--  nb we must still emit ctrl:=imm32 if not and_bits(symtab[dest][S_State],S_for)...
                            npc = tgt-2
else
                            cmp_imm32(limitreg,nMin)            -- cmp limit,imm32
                            emitHex6j(jg_rel32,0)               -- jg endfor [sj NOT ok]
end if
                        else
                            if limitreg=-1 then -- limit not loaded, use smin
                                cmp_imm32(reg,smin)             -- cmp init,imm32
                            else
                                xrm = 0o300+reg*8+limitreg
                                emitHexx2(cmp_reg_reg,xrm)      -- cmp init,limit
                            end if
                            emitHex6j(jl_rel32,0)               -- jl endfor [sj NOT ok]
                        end if
                        backpatch = length(x86)
                    end if
                elsif reg=-1 and limitreg=-1 then
                    if stepreg=-1 then ?9/0 end if --DEV?
                    xrm = #C0+stepreg*9 -- 0o3ss
                    emitHexx2(test_reg_reg,xrm)                 -- test stepreg,stepreg
                    if nMin=smin then
                        -- one iteration, just check stepreg is not 0
                        emitHex6j(jnz_rel32,0)                  -- jnz looptop [sj NOT ok]
                        backpatch2 = length(x86)
                        -- note that backpatch remains 0; there is no jmp endfor
                    else
                        if nMin<smin then -- init<limit eg for i=0 to 1 by step
                            -- if step +ve goto looptop
                            emitHex6j(jg_rel32,0)               -- jg looptop [sj NOT ok]
                        else -- nMin>smin    -- init>limit eg for i=1 to 0 by step
                            -- if step -ve goto looptop
                            emitHex6j(jl_rel32,0)               -- jl looptop [sj NOT ok]
                        end if
                        backpatch2 = length(x86)
                        -- if step not 0 goto endfor [else e121]
--spotted 6/6/2012:
--                      emitHex6j(jl_rel32,0)                   -- jnz endfor [sj NOT ok]
                        emitHex6j(jnz_rel32,0)                  -- jnz endfor [sj NOT ok]
                        backpatch = length(x86)
                    end if
                    emitHexx2(mov_al_imm8,120)                  -- mov al,120 (for e120fle[step])
                    movRegVno(edi,src2)                         -- ep1 (=var no of step)
                    movRegImm32(esi,4)                          -- "step"
                    emitHex5callG(opRTErn)
                    x86[backpatch2] = length(x86)-backpatch2    -- looptop:
                else
                    if stepreg=-1 then ?9/0 end if --DEV?
                    xrm = 0o300+stepreg*9 -- 0o3ss
                    emitHexx2(test_reg_reg,xrm)                 -- test stepreg,stepreg
                    if iroot!=T_integer     -- init not integer
                    or slroot!=T_integer    -- limit not integer
                    or nMax>smin then       -- init can be > limit
                        emitHex6j(jle_rel32,0)                  -- jle @f [sj prolly ok]
                        backpatch2 = length(x86)
                        if reg=-1 then      -- init not loaded; use nMin (and reverse the condition)
                            if limitreg=-1 then ?9/0 end if -- sanity check
                            cmp_imm32(limitreg,nMin)            -- cmp limit,imm32
                            emitHex6j(jge_rel32,0)              -- jge looptop [sj NOT ok]
                        else
                            if limitreg=-1 then -- limit not loaded, use smin
                                cmp_imm32(reg,smin)             -- cmp init,imm32
                            else
                                xrm = 0o300+reg*8+limitreg
                                emitHexx2(cmp_reg_reg,xrm)      -- cmp init,limit
                            end if
                            emitHex6j(jle_rel32,0)              -- jle looptop [sj NOT ok]
                        end if
--                      backpatch2 = length(x86) -- (done rsn)
                        backpatch3 = length(x86)
                        emitHex5j(0)                            -- jmp endfor
                        backpatch = length(x86)
                        x86[backpatch2] = length(x86)-backpatch2 -- @@:
--DEVBPM backpatch me: (check this! or save a temp [backpatch2new] 3 lines above)
--printf(1,"backpatch line 10351 pilx86.e, (emitline=%d, %s)\n",{emitline,filenames[symtab[vi][S_FPno]][2]})
--                      backpatch2 = length(x86)-5
                        backpatch2 = backpatch3
                    else
                        emitHex6j(jg_rel32,0)                   -- jg looptop [sj NOT ok]
                        backpatch2 = length(x86)
                    end if

                    -- jz e121flelimstep:
                    emitHex6j(jnz_rel32,0)                      -- jnz @f [sj NOT ok]
                    backpatch3 = length(x86)
                    emitHexx2(mov_al_imm8,120)                  -- mov al,120 (for e121fle[step])
                    movRegVno(edi,src2)                         -- ep1 (=var no of step) [==0]
                    movRegImm32(esi,4)                          -- "step" (as part of error msg)
                    emitHex5callG(opRTErn)                      -- fatal error
                    x86[backpatch3] = length(x86)-backpatch3

                    if iroot!=T_integer     -- init not integer
                    or slroot!=T_integer    -- limit not integer
                    or nMin<smax then       -- init can be < limit
                        if reg=-1 then      -- init not loaded; use nMin (and reverse the condition)
                            if limitreg=-1 then ?9/0 end if -- sanity check
                            cmp_imm32(limitreg,nMin)            -- cmp limit,imm32
                            emitHex6j(jg_rel32,backpatch)       -- jg endfor [sj NOT ok]
                        else
                            if limitreg=-1 then -- limit not loaded, use smin
                                cmp_imm32(reg,smin)             -- cmp init,imm32
                            else
                                xrm = 0o300+reg*8+limitreg
                                emitHexx2(cmp_reg_reg,xrm)      -- cmp init,limit
                            end if
                            emitHex6j(jl_rel32,backpatch)       -- jl endfor [sj NOT ok]
                        end if
                        backpatch = length(x86)
                    end if
                    x86[backpatch2] = length(x86)-backpatch2    -- looptop:
                end if
                if npc=0 then -- (not that it really matters)
                    s5[pc+6] = length(x86)
                    s5[pc+1] = backpatch    -- replace flags with backpatch chain
                end if
                reginfo = 0
            end if -- not isGscan (ie last pass)
            if npc=0 then
                pc += 7
            else
                -- (we've deduced loop will iterate 0 times)
                -- Note: We need to undo whatever opLoopTop may have done.
                --       (You would need to "p -dumpil -c" to emit one.)
                --       I simply copied from opEndFor; not entirely sure
                --       why ltCtrl then ltFlip would be needed, btw. If
                --       there is a problem it may need deeper analysis.
                if s5[npc]!=opEndFor then ?9/0 end if
                pc = npc
                if NOLT=0 or bind or lint then
                    ltCtrl(pc+2)
                    ltFlip(pc)
                end if -- NOLT
                pc += 3 -- (nb matching one of these in opEndFor)
-- added 8/6/2012: kill the opLabel if we skipped all the exit(??)
-- 132:  opEndFor,17,14,                         opEndFor,END+LOOP,bpFor
-- 135:  opLabel,3,0,84,                         opLabel,exitMerge,0/x86loc,link
--DEV 7/6/17: (not entirely sure about this...)
--              if s5[pc]=opLabel then
----                    if s5[pc+1]!=exitMerge then ?9/0 end if -- more investigation rqd?
--                  if s5[pc+1]!=exitMerge then ?{"9/0",pc,s5[pc+1]} end if -- more investigation rqd?
                if s5[pc]=opLabel
                and s5[pc+1]=exitMerge then
                    pc += 4             
                end if
            end if

--opRTErn
        elsif opcode=opEndFor then
            -- opEndFor,END+LOOP,bpFor

if NOLT=0 or bind or lint then
            ltCtrl(pc+2)
end if -- NOLT

--          if not isGscan then
                p1 = s5[pc+2]   -- last byte of opFor (holds loopTop addr)
                if s5[p1-6]!=opFor2 then ?9/0 end if
                --  ([p1-5] is flags, replaced with backpatch chain in the not isGscan case)
                --  ([p1-4] is intial value, may get used below)
                p2 = s5[p1-3]   -- control var  (p2 is varno[1..len(symtab)]; [reg is eax..edi])
                step = s5[p1-2] -- step         (becomes literal value or register below)
                lim = s5[p1-1]  -- limit        (becomes register when loaded below)
                tgt = s5[p1]    -- loopTop

--DEV:
if not and_bits(symtab[p2][S_State],S_for) then
    -- control var may live on past the end of the loop...
                    dest = p2
                    getDest()
                    if and_bits(dtype,T_atom)=T_integer then    -- min/max are meaningful
                        src = s5[p1-4]  -- initial value
                        getSrc()
                        nMin = smin -- save init for use below
                        nMax = smax
                        src = lim
                        getSrc()    -- (limit)
                        src2 = step
                        getSrc2()   -- (step)
                        slroot = T_integer
                        w = smin+smin2  -- (limit+step)
--                      if integer(w) then
                        if not isFLOAT(w) then
                            if w<smin then smin = w end if
                            w = smax+smax2  -- (limit+step)
--                          if integer(w) then
                            if not isFLOAT(w) then
                                if w>smax then smax = w end if
                                if nMin<smin then smin = nMin end if
                                if nMax>smax then smax = nMax end if
                            else
                                slroot = T_atom
                            end if
                        else
                            slroot = T_atom
                        end if
                        if slroot=T_atom then
                            smin = MININT
                            smax = MAXINT
--DEV (untried, seems the right thing to do 31/7/09:)
--slroot = T_integer
                        end if
                        sltype = slroot
                        storeDest()
                    end if
end if

            if not isGscan then

                -- *NB* jumps are auto-converted to 8-bit form where possible by pemit.e;
                --      attempts to emit short jumps here /will/ cost more than they save.

                src = step
                getSrc()
                src2 = lim
                getSrc2()
--              if sched then
--                  sch00n = schoon
--              end if
--?         if slroot=T_integer and smin=smax then
                if smin=smax then   -- optimise for known literal step value
                    reg = loadReg(p2)                           -- mov reg,[p2] (control var)
--DEV if wasLtrlNL then !!! (gain if the mov esi is pairing with something the add could be pairing with)
--  ?!          emitHex6v(mov_esi_mem32,NL)                     -- mov esi,[lim] (limit value)
--tryme pt1:
--if not wasLtrlNL then
                    if smin2=smax2 then
-- is there any point trying cmp reg,imm?? I think not...
                        lim = edx
                        movRegImm32(edx,smin2)                  -- mov edx,imm32
                    else
                        lim = loadReg(lim)                      -- mov lim,[lim] (limit value)
                    end if
--end if
--DEV if tmp=1 then cmp/inc/store/jnc should save one clock cycle (unless partial flags stall spanners the plan)
                    mod = m_add
                    regimm365(smin)                             -- add reg,imm
                    --tryme pt2:
                    --if wasLtrlNL then
                    --              lim = symtab[NL][S_value]
                    --              mod = m_cmp
                    --              regimm365(lim)                              -- cmp reg,imm
                    --else
--                  if sched then
--                      schedule(rb+regbit[lim+1],0,0,pUV,1,0)
--                  end if
                    xrm = #C0+reg*8+lim -- 0o3rl
-- (#3B = m_cmp+3):
                    emitHexx2(0o073,xrm)                        -- cmp reg,lim
--end if
                    storeReg(reg,p2,1,1)                        -- mov [p2],reg (control var)

--                  if sched then
--                      schend()
--                  end if
                    jcode = jge_rel32
                    if smin>0 then
                        jcode = jle_rel32
                    end if
--DEVBPM backpatch me: (ish, or for once can we just assume jcc is always going to be length 6? - nah, do it properly!)
--printf(1,"backpatch line 10539 pilx86.e, (emitline=%d, %s)\n",{emitline,filenames[symtab[vi][S_FPno]][2]})
--                  joffset = tgt-(length(x86)+6)
--                  emitHex6j(jcode,joffset)                    -- jcc bpFor+1 [sj NOT ok!]
                    emitHex6j(jcode,0)                          -- jcc bpFor+1 [sj NOT ok!]
                    x86[length(x86)] = tgt-length(x86)
                else

--            if smin>0 then                -- positive step
--            if smax<0 then                -- negative step
--            if smin<=0 and smax>=0 then   -- unknown step sign

                    -- we require the fully general purpose code (unknown step value/sign):
                    reg = loadReg(p2)                           -- mov reg,[p2] (control var)
                    step = loadReg(step)                        -- mov step,[step] (step value)
                    if smin2=smax2 then
                        lim = edx
                        movRegImm32(edx,smin2)                  -- mov edx,imm32 (limit)
                    else
                        lim = loadReg(lim)                      -- mov lim,[lim] (limit value)
                    end if
--                  if sched then
--                      rb = regbit[reg+1]
--                      rs = regbit[step+1]
--                      schedule(rb+rs,0,rb,pUV,1,0)
--                  end if
                    xrm = 0o300+step*8+reg --0o3sr
-- (#01 = m_add+1):
                    emitHexx2(0o001,xrm)                        -- add reg,step
--                  if sched then
--                      schedule(rs,0,0,pUV,1,0)
--                  end if
                    if smin<=0 and smax>=0 then
                        xrm = 0o300+step*9 -- 0o3ss
                        emitHexx2(test_reg_reg,xrm)             -- test step,step
                    end if
                    storeReg(reg,p2,1,1)                        -- mov [p2],reg (control var)
--                  if sched then
--                      schend()
--                  end if
                    xrm = #C0+reg*8+lim -- 0o3rl
-- (#3B = m_cmp+3):
                    backpatch2 = 0
                    if smin<=0 then
                        if smax>=0 then
--DEVBPM backpatch me: [DONE]
--printf(1,"backpatch line 10586 pilx86.e, (emitline=%d, %s)\n",{emitline,filenames[symtab[vi][S_FPno]][2]})
--                          emitHex6j(jns_rel32,13)     -- jns positive_step (one less jump needed for +ve steps)
                            emitHex6j(jns_rel32,0)      -- jns positive_step (one less jump needed for +ve steps)
                            backpatch2 = length(x86)
                        end if
                        emitHexx2(0o073,xrm)            -- cmp reg,lim
--DEVBPM backpatch me: [DONE]
--printf(1,"backpatch line 10592 pilx86.e, (emitline=%d, %s)\n",{emitline,filenames[symtab[vi][S_FPno]][2]})
--                      joffset = tgt-(length(x86)+6)
--                      emitHex6j(jge_rel32,joffset)    -- jge <instruction following the opFor> [sj NOT ok!]
                        emitHex6j(jge_rel32,0)          -- jge <instruction following the opFor> [sj NOT ok!]
                        backpatch = length(x86)
                        x86[backpatch] = tgt-backpatch
--DEV try (lots of, both sides) nops here, to avoid any jump mispredictions:
                        if smax>=0 then
--                          emitHex5s(jump_8)           -- jmp <next instruction> [sj NOT ok]
--DEVBPM backpatch me: [DONE]
--printf(1,"backpatch line 10599 pilx86.e, (emitline=%d, %s)\n",{emitline,filenames[symtab[vi][S_FPno]][2]})
--                          emitHex5j(8)                -- jmp <next instruction> [sj NOT ok]
                            emitHex5j(0)                -- jmp <next instruction> [sj NOT ok]
                            x86[backpatch2] = length(x86)-backpatch2    -- positive_step:
                            backpatch2 = length(x86)
                        end if
                    end if
                    if smax>=0 then
                        emitHexx2(0o073,xrm)            -- cmp reg,lim
--DEVBPM backpatch me: [DONE]
--printf(1,"backpatch line 10608 pilx86.e, (emitline=%d, %s)\n",{emitline,filenames[symtab[vi][S_FPno]][2]})
--                      joffset = tgt-(length(x86)+6)
--                      emitHex6j(jle_rel32,joffset)    -- jle <instruction following the opFor>
                        emitHex6j(jle_rel32,0)          -- jle <instruction following the opFor>
                        backpatch = length(x86)
                        x86[backpatch] = tgt-backpatch
                        if backpatch2 then
                            x86[backpatch2] = length(x86)-backpatch2
                        end if
                                                        -- @@: <next instruction>
                    end if
                end if
--27/9/19 this code copied into jskip:
                -- finally, batchpatch zero-iteration jmp(s) [if any] at loop start:
                tgt = s5[p1-5] -- (was flags, now backpatch chain)
                while tgt do
                    k = length(x86)-tgt
                    backpatch = x86[tgt]
                    x86[tgt] = k                        
                    tgt = backpatch
                end while
-- 27/4/09:
                reginfo = 0
            end if -- not isGScan

if NOLT=0 or bind or lint then
            ltFlip(pc)
end if -- NOLT

--          pc += 2
            pc += 3     -- (nb matching one of these in opFor2)

        elsif opcode=opAsm then

            -- opAsm,len/x86loc,0/next,jlnk,<len bytes>     (see pops.e)

            k = s5[pc+1]
if NOLT=0 or bind or lint then
            if k
            and length(s5)>=pc+k+4  -- [needed when opRetf omitted for EvoGen testing]
            and s5[pc+k+4]!=opRetf
            and (s5[pc+k+4]!=opLn or s5[pc+k+6]!=opAsm) then
                ltCall(E_vars,E_vars,pc)    -- clear all, unless:
                -- an empty/dummy opAsm (k=0), or
                -- immediately followed by a return, or
                -- immediately followed by opLn and another opAsm
                --  (in latter case ltCall is left to ""     "")
            end if
end if -- NOLT

            if isGscan then
                pc += k+4
            else
                s5[pc+1] = length(x86) -- save actual s5/x86 location
--if linkFwd then
                if lastAsm then
                    s5[lastAsm+2] = pc
                else
                    firstAsm = pc
                end if
                lastAsm = pc
                if s5[pc+2]!=0 then ?9/0 end if
--else
--              s5[pc+2] = thisAsm
--              thisAsm = pc
--end if
--              if sched then
--                  schend()    -- if k=0 this is probably a label!
--              end if
                if k then
                    if lastline!=emitline then lineinfo() end if
                    pc += 4
--                  for i=1 to k do
while k do
                        p1 = s5[pc]
--DEV q86 probably needs to linkup things here... (with skip thingy)
--DEV this is temporary (and not guaranteed to work):
-- (moved "back" to ilasm() because it was messing up jump offsets)
--if and_bits(p1,#10000000) then    --NO!
--if p1=opLoadMem+#10000000
--or p1=opLeaMov+#10000000
--or p1=opStoreMem+#10000000 then
--  p1 -= #10000000
--  pc += 1
--  reg = s5[pc]
--  pc += 1
--  tvar = s5[pc]   -- src/dest
--  if p1=opLoadMem then
--      loadMem(reg,tvar)
--  elsif p1=opLeaMov then
--      leamov(reg,tvar)
--  elsif p1=opStoreMem then
--      storeMem(tvar,reg)
--  else
--      ?9/0
--  end if
--  k -= 2
--else
                        x86 = append(x86,p1)
--end if
                        pc += 1
--                  end for
k -= 1
end while

--DEV or:? (untried)
--              npc = pc+1
--              pc += k
----                npc = pc+4
----                pc += k+3
--              x86 &= s5[npc..pc]
--DEV/SUG: (failed miserably... tests pass but breaks self-host??) (still 2/3...)
--              if X64 then
--                  emitHex1(#48)
--              end if
--              emitHex2s(xor_ebx_ebx)                          -- xor ebx,ebx
                else
                    pc += 4
                end if
                if pc>length(s5) then exit end if --??
                reginfo = 0 -- 18/2/09!!
            end if
            if pc>length(s5) then exit end if   -- [needed for when opRetf omitted in EvoGen tests]

--      elsif opcode=opFind             -- 43
--         or opcode=opMatch then       -- 44
--if newEmit then ?9/0 end if
--          dest = s5[pc+1]
--          src = s5[pc+2]
--          from = s5[pc+4]
--          src2 = from
--          getSrc2()
--          fmin = smin2
--          fmax = smax2
--          src2 = s5[pc+3]
--          pc += 5
--          getDest()
--          getSrc()
--          getSrc2()
---- DEV    We could also try Find: slroot vs setyp2, Match: setyp vs setyp2,
----        and if no overlap result is just 0... maybe even ple(0) it...
--          if slen2>=0 then
--              if opcode=opMatch
--              and slen>=0 then
--                  slen2 -= (slen-1)
--                  if slen2<0 then
--                      slen2 = 0
--                  end if
--              end if
--          else
--              slen2 = MAXLEN
--          end if
--          slroot = T_integer
--          sltype = slroot
--          smin = 0
--          smax = slen2
--          storeDest()
--          --else
--          if not isGscan then
----if opcode=opFind then
----;new 29/6/10:
----;calling convention:                                octal:         binary:          code:
----; mov ecx,p1        ; result location               271         B9 imm32        mov ecx,imm32
----; mov esi,imm32 ; start from (known/1)          276         BE imm32        mov esi,imm32
----;> or
----; mov esi,[p4]  ; start from (var)              213 065     8B 35 m32       mov esi,[m32]
----;                                           or mov esi,[ebp-nn]
----;                                           or mov esi,[ebp-nnnn]
----; mov eax,[p2]  ; ref of p2                     241         A1 m32          mov eax,[m32]
----; mov edi,[p3]  ; ref of p3                     213 075     8B 3D m32       mov edi,[m32]
----; call opFind   ; [ecx] = find(eax,edi)         350         E8 rel32        call rel32
----;   nb p2/3/4 obtained from [esp]-15/-9/-20 on error (dev newEBP)
--              markConstUseds({dest,src,src2,from})
--              if sched then
----                    schend()
--                  ?9/0 --(??)
--              end if
--              leamov(ecx,dest)                            -- lea ecx,[dest]/mov ecx,addr dest
--              if fmin=fmax then
----if fmin=0 then ?9/0 end if  --DEV proper message
----                    emitHex5w(mov_esi_imm32,fmin)           -- mov esi,imm32
--                  movRegImm32(esi,fmin)                   -- mov esi,imm32
--                  clearReg(esi)
--              else    
--                  loadToReg(esi,from)                     -- mov esi,[from]
--              end if
--              loadToReg(eax,src)                          -- mov eax,[src]
--              loadToReg(edi,src2)                         -- mov edi,[src2]
--if newEmit then
--              movRegVno(ebx,src2)                         -- mov ebx,src2 (var no)
--              movRegVno(edx,src)                          -- mov edx,src (var no)
--?9/0
--else
--              emitHex5w(mov_ebx_imm32,src2)               -- mov ebx,src2 (var no)
--              emitHex5w(mov_edx_imm32,src)                -- mov edx,src (var no)
--end if
--              emitHex5call(opcode) -- call opFind/Match ; [ecx] = find/match([esi],[edx])
--              -- all regs trashed, unless result is integer, in ecx (and [dest])
--              reginfo = 0
--              if symtab[dest][S_vtype]=T_integer then
----DEV tryme:
----                if dtype=T_integer then
--                  storeReg(ecx,dest,0,0)  -- just record that ecx==[dest]
--              end if
--          end if
--
        elsif opcode=opNop then
            opcode = lastop
            pc += 1

        elsif opcode=opPuts then        -- 131
--if newEmit then ?9/0 end if -- (now in pfileio.e, erm, but still using opPuts)
            if not isGscan then
                src = s5[pc+1]  -- fileno
                src2 = s5[pc+2] -- object to print
                -- calling convention:
                -- mov eax,[src]    ; fileno (opUnassigned)
                -- mov edx,[src2]   ; object to print (opUnassigned)
                -- call :%opPuts
                markConstUseds({src,src2})
                getSrc()
                getSrc2()
                ref = loadReg(src2,NOLOAD)
                if ref=eax then
                    xrm = #C2 + ref*8 -- 0o3r2
                    emitHexx2(mov_reg,xrm)                  -- mov edx,ref
                    ref = edx
                    clearReg(edx)
                end if
                if slroot=T_integer and smin=smax then
                    movRegImm32(eax,smin)                       -- mov eax,imm32
                else
                    loadToReg(eax,src)                          -- mov eax,[src] (fileno)
                end if
                if ref!=edx then
--                  if ssNTyp2=S_Const and and_bits(state2,K_noclr) and slroot2=T_integer then
--                      if smin2!=smax2 then ?9/0 end if
                    if slroot2=T_integer and smin2=smax2 then
                        if smin2 then
                            movRegImm32(edx,smin2)              -- mov edx,imm32
                        else
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(xor_edx_edx)              -- xor edx,edx
                        end if
                    elsif ref=-1 then
                        loadMem(edx,src2)                       -- mov edx,[src2] (object to print)
                    else
                        if ref=eax then ?9/0 end if             -- sanity check (cannot happen)
                        xrm = 0o302 + ref*8 -- 0o3r2
                        emitHexx2(mov_reg,xrm)                  -- mov edx,ref
                    end if
                end if
                emitHex5callG(opPuts)                           -- call :%opPuts
                -- NB src2 obtained from [esp]-9 on error
                reginfo = 0
            end if
            pc += 3

--/* -- (now in builtins/repeat.e)
        elsif opcode=opRepeat
           or opcode=opRepCh then
--if newEmit then ?9/0 end if
            -- opRepeat,dest,item,count
            dest = s5[pc+1]
            src = s5[pc+2]
            src2 = s5[pc+3]
            pc += 4
--if isGscan then
            getDest()
            getSrc()
            getSrc2()
            --
            -- While a char is strictly speaking 0..255, the implementation
            --  of opRepeat uses 7..255 so that eg x=repeat(0,5) produces a
            --  dword-sequence at the get-go, rather than a byte-string of
            --  nulls which will more than likely be auto-expanded to Dsq
            --  by the very next instruction. Of course programmers also
            --  expect eg repeat('=',5) to produce the string "=====".
            --
            -- BTW: this 0/7 idiosyncracy only bites should you later use
            --  the builtin string() function to test things. It does not
            --  alter screen or file I/O, or equality or other relational
            --  tests [ie/eg {'f','r','e','d'}="fred" yields a 1].
            --
            iroot = slroot
            bmin = smin --\ save for
            bmax = smax --/ use below
            setyp = slroot
--12/1/16:
--          if slroot=T_integer and smin>=7 and smax<=255 then
            if opcode=opRepCh or (slroot=T_integer and smin>=7 and smax<=255) then
                slroot = T_string
            else
--DEV (20/9) tryme instead: (ie can we trust min/max at all when T_N bit is set??
--                           -- aka can T_atom,-1,-1 actually hold say 35, if we've
--                                  stopped bothering with min/max once T_N got set?)
--              if not and_bits(slroot,T_integer)       -- not an integer,
--              or (and_bits(slroot,T_atom=T_integer)   -- or an integer
--                  and (smax<7 or smin>255)) then      --  which is definitely out of range
                if not and_bits(slroot,T_integer) or smax<7 or smin>255 then
                    slroot = T_Dsq
                else
                    slroot = T_sequence
                end if
            end if
            if smin2=smax2 then
                slen = smin2
            else
                slen = -2
            end if
--?{iroot,slroot,smin,smax,{opcode,opRepCh,opRepeat}}
--if slroot = 0 then ?9/0 end if
            sltype = slroot
--from_opRepCh = 1
            storeDest()
--from_opRepCh = 0
            --else
            if not isGscan then
                markConstUseds({dest,src,src2})
--DEV if src2 is in edi or eax then end if
--DEV if src is in edi or ecx then end if
-- (went with clearReg() for now)
                leamov(edi,dest)                                -- lea edi,[dest]/mov edi,addr dest (result)
                if iroot=T_integer and bmin=bmax then
                    movRegImm32(eax,bmin)                       -- mov eax,imm32
                    clearReg(eax)
                else
                    loadToReg(eax,src)                          -- mov eax,[src] (item to repeat)
                end if
                if slroot2=T_integer and smin2=smax2 then
                    movRegImm32(ecx,smin2)                      -- mov ecx,imm32
                else
                    loadToReg(ecx,src2)                         -- mov ecx,[src2] (count)
                end if
                emitHex5callG(opcode)                           -- call opRepeat/opRepCh
                reginfo = 0 -- all regs trashed
            end if
--*/
        elsif opcode=opFloor then       -- 82
            dest = s5[pc+1]
            src = s5[pc+2]
            pc += 3
--if isGscan then
            getDest()
-- Since we don't store atom ranges [DEV?], we cannot do much here,
--  except ensure dest is properly marked as int|flt:
--  (Of course, eg floor(1e308) is 1e308, an atom not an int, and
--   hence even say floor(3.5) must therefore assume an atom result...
--   We may as well ignore the floor(int) case since it is so rare.)
            slroot = T_atom
            sltype = slroot
            storeDest()
            --else
            if not isGscan then
--;calling convention:                              octal:         binary:          code:
--; mov edi,p1      ; result location               277         BF imm32        mov edi,imm32
--;;    mov ecx,p2      ; addr p2                       271         B9 imm32        mov ecx,imm32
--; mov eax,[p2]    ; ref p2                        241         A1 m32          mov eax,[m32]
--; call opFloor    ; p1 = floor(p2)                350         E8 rel32        call rel32
--; all regs trashed, unlesss result is integer, in eax (and [p1]), esi,ecx also trashed
--; nb p2 obtained from [esp]-9 on error
-- 11/6/2012:
--if 0 then -- temp (while I test mods in pmain)
if vtype=T_integer then -- dest is integer
    getSrc()
--symtab[504]:{-1,S_Const,1,(S_used+S_set+K_noclr+K_lit),0,249/#0040F3E0,atom,{atom,127.5,127.5,0,-1},127.5}
    if ssNTyp1=S_Const and sltype=T_atom then
        w = floor(symtab[src][S_value])
--      if integer(w) then
        if not isFLOAT(w) then
            clearMem(dest)
            storeImm(dest,w)                                    -- mov [dest],imm32
            dest = 0
        end if
    end if
end if
if dest then
--              if sched then
--                  sch00n = schoon
--                  schedule(0,0,edibit,pUV,0,0)
--              end if
                leamov(edi,dest)                                -- lea edi,[dest]/mov edi,dest (addr result)
--              if sched then
--                  schedule(0,0,eaxbit,pUV,0,src)
--              end if
                loadToReg(eax,src)                              -- mov eax,[src]
--              if sched then
--                  schend()
--              end if
                emitHex5callG(opFloor)                          -- call :%opFloor
                -- all regs trashed, unless result is integer, in eax (and [dest])
                reginfo = 0
--DEV is it worth doing rootType, if a udt typecheck is about to occur??
--              if rootType(symtab[dest][S_vtype])=T_integer then
--DEV tryme (above query still stands...):
                if vroot=T_integer then
                    storeReg(eax,dest,0,0) -- just record that eax==[dest]
                end if
end if
            end if

        elsif opcode=opPow then         -- 89
--DEV merge with opRmdr??
            dest = s5[pc+1]
            src = s5[pc+2]
            src2 = s5[pc+3]
            tii = s5[pc+4]      -- 1=src/src2 both init (not necc. integer)
--          pc += 5
            getDest()
            -- generate min/max for the result...
            -- see t49ginfo.exw for more info on this algorithm.
            getSrc()
            getSrc2()
            if and_bits(slroot,T_atom)!=T_integer
            or and_bits(slroot2,T_atom)!=T_integer
            or (smin2<0 and (smin<-1 or smax>1))
            or (smin<=0 and smax>=0 and smin2<=0) then
                slroot = T_atom
            else
                slroot = T_integer
--DEV 25/8/15: (crash compiling demo\rosetta\Average_loop_length.exw)
--if smax2=MAXINT then
--8/10/15: rosetta/perfect numbers.. (limit found by binary chop of a test loop for power(2,i))
--7/11/18:
--if smax2>=1024 then
if smax2>=1024
or smax>=MAXINT then
    nMax = MAXINT+1
else
--15/5/20:
  try
                nMax = power(smax,smax2)
  catch e
    nMax = MAXINT+1
  end try
end if
                if smin<0 then       -- x can be negative
                    k = smax2
                    if smin2<smax2 then
                        if not and_bits(k,1) then
                            k -= 1                      -- odd-ize
                        end if
                        nMin = power(smin,k)
                    elsif and_bits(k,1) then
                        nMin = power(smin,k)
                    elsif smax>=0 then
                        nMin = 0
                    else
--                      nMin = power(smax,smax2)
                        nMin = nMax
                    end if
                    k = smax2
                    if smin2<smax2 then
                        if and_bits(k,1) then
                            k -= 1                      -- even-ize
                        end if
                    end if
--7/11/18:
if smin=MININT then
                    w = MAXINT+1
else
                    w = power(smin,k)
end if
                    if w>nMax then
                        nMax = w
                    end if
                elsif smin=0 then   -- x can be 0
--                  if smax2>0 then nMin = 0 else nMin = 1 end if
                    nMin = (smax2<=0)
                else                -- x is positive
--15/5/20
  try
                    nMin = power(smin,smin2)
  catch e
    nMax = MAXINT+1
  end try
                end if
--              if not integer(nMax)
--              or not integer(nMin) then
                if isFLOAT(nMax)
                or isFLOAT(nMin) then
                    slroot = T_atom
                else
                    bmin = smin --\ save for
                    bmax = smax --/ use below
                    smin = nMin
                    smax = nMax
                end if
            end if
            sltype = slroot
            storeDest()
            --else
            if not isGscan then
                if slroot=T_integer and vroot=T_integer and tii then
                    -- nb: smin/smax now relate to dest, bmin/bmax are saved from src
--DEV (6/7/17) borken on X64...?? (==>mov [god_knows_where],h4 !?!?!?!?)
--                  if smin=smax then
                    if smin=smax and X64=0 then
if X64 then
    emitHex1(nop)
end if
                        -- just store dest
                        storeImm(dest,smin)                         -- mov [dest],imm32
                        if reginfo then clearMem(dest) end if
                        dest = 0
-- 22/2/14:
                        tmpd = 0
                    elsif bmin=bmax and bmin=2 then
--DEV 6/7/17... (mebbe just needs the ">29" improved?? - but right now I just need a real quick fix)
if X64=0 then
                        if smin2<0 or smax2>29 or smin2=smax2 then ?9/0 end if
end if
                        clearReg(ecx)
                        if tmpd then
                            if tmpr>=0 then
                                if tmpr!=ecx then
                                    xrm = 0o310+tmpr    -- 0o31r
                                    emitHexx2(mov_dword,xrm)        -- mov ecx,tmpr
                                end if
                            else
                                ?9/0    --should have got smin=smax above
                            end if
                            tmpd = 0
                        else
                            loadToReg(ecx,src2)                 -- mov ecx,[src2]   init int
                        end if
--                      emitHex5w(mov_eax_imm32,1)              -- mov eax,1
                        movRegImm32(eax,1)                      -- mov eax,1
                --DEV (untried, in place of clearReg(ecx) above:)
                --      storeReg(ecx,src2,0,1)
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(shl_eax_cl)                   -- shl eax,cl
                        storeReg(eax,dest,1,1)                  -- mov [dest],eax
                        dest = 0
                    elsif smin2=smax2 and smin2=0
                      and (bmin>0 or bmax<0) then
                        ?9/0    -- should have gotten smin=smax=1...
                    end if
                end if
    if dest then
                markConstUseds({dest,src,src2})
                --calling convention
                --  mov ecx,[p3]        ; source2 (opUnassigned)
                --  mov eax,[p2]        ; source (opUnassigned)
                --  lea edi,[p1]        ; addr of target
                --  call :%opPow        ; [edi]:=power(eax,ecx)
----                if sched then
----                    sch00n = schoon
----                    schedule(0,0,edibit,pUV,0,0)
----                end if
--21/3/17 (moved below tmpr - may be edi) [above killed off in the process]
--              leamov(edi,dest)                                -- lea edi,[dest]/mov edi,dest (addr result)
if tmpd then
    if tmpr>=0 then
        if tmpr!=ecx then
--              if sched then
--                  schedule(regbit[tmpr+1],0,ecxbit,pUV,0,0)
--              end if
                xrm = 0o310+tmpr    -- 0o31r
                emitHexx2(mov_dword,xrm)                        -- mov ecx,tmpr
        end if
    else
--              if sched then
--                  schedule(0,0,ecxbit,pUV,0,0)
--              end if
--              emitHex5w(mov_ecx_imm32,smin2)                  -- mov ecx,imm32
                movRegImm32(ecx,smin2)                          -- mov ecx,imm32
    end if
    tmpd = 0
else -- not tmpd
--              if sched then
--                  schedule(0,0,ecxbit,pUV,0,src2)
--              end if
--8/6/16:
--              loadToReg(ecx,src2)                             -- mov ecx,[src2]
                if slroot2=T_integer and smin2=smax2 then
                    movRegImm32(ecx,smin2)                      -- mov ecx,imm32
                else
                    loadToReg(ecx,src2)                         -- mov ecx,[src2]
                end if
end if -- tmpd
--              if sched then
--                  schedule(0,0,eaxbit,pUV,0,src)
--              end if
--8/6/16:
--              loadToReg(eax,src)                              -- mov eax,[src]
                clearReg(ecx)       -- (added 5/12/19)
                getSrc()
                if and_bits(state1,K_rtn+K_Fres) then ?9/0 end if
                if slroot=T_integer and smin=smax then
                    if smin=0 then
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(xor_eax_eax)                  -- xor eax,eax
                    else
                        movRegImm32(eax,smin)                   -- mov eax,imm32
                    end if
                else
                    loadToReg(eax,src)                          -- mov eax,[src]
                end if
--              if sched then
--                  schend()
--              end if
--21/3/17 (moved from above)
                leamov(edi,dest)                                -- lea edi,[dest]/mov edi,dest (addr result)
                emitHex5callG(opPow)
--trace(1)
    end if -- dest!=0
                reginfo = 0 -- all regs trashed
            end if
            pc += 5

        elsif opcode=opXor then         -- 45
            dest = s5[pc+1]
            src = s5[pc+2]
            src2 = s5[pc+3]
            getDest()
            smin = 0
            smax = 1
            --DEV:
            --  if p1 is always zero then
            --      if p2 is always zero then
            --          smax = 0                -- 0 xor 0 -> 0
            --      elsif p2 is never zero then
            --          smin = 1                -- 0 xor 1 -> 1
            --      end if
            --  elsif p1 is never zero then
            --      if p2 is always zero
            --          smin = 1                -- 1 xor 0 -> 1
            --      elsif p2 is never zero then
            --          smax = 0                -- 1 xor 1 -> 0
            --      end if
            --  end if
            slroot = T_integer
            sltype = slroot
            storeDest()
            --else
            if not isGscan then
                markConstUseds({dest,src,src2})
--              if sched then
--                  sch00n = schoon
--                  schedule(0,0,edibit,pUV,0,0)
--              end if
                leamov(edi,dest)                                -- lea edi,[dest]/mov edi,dest (addr result)
--; mov edi,dest    ; addr dest                     277         BF imm32        mov edi,imm32
--; mov eax,[p2]    ; ref p2                        241         A1 m32          mov eax,[m32]
--; mov esi,[p3]    ; ref p3                        213 065     8B 35 m32       mov esi,[m32]
--; call opXor      ; [edi] = [edx] xor [ecx]       350         E8 rel32        call rel32
--; nb: p2/3 determined from [esp]-15/-9 on error
--              if sched then
--                  schedule(0,0,eaxbit,pUV,0,src)
--              end if
                loadToReg(eax,src)                              -- mov eax,[src]
--              if sched then
--                  schedule(0,0,esibit,pUV,0,src2)
--              end if
                loadToReg(esi,src2)                             -- mov esi,[src2]
--              if sched then
--                  schend()
--              end if
                movRegVno(ecx,src)                              -- mov ecx,varno of src
                movRegVno(edx,src2)                             -- mov edx,varno of src2
                emitHex5callG(opXor)
--DEV (as implemented below) all regs only trashed if result is not integer; else
--  (when result is integer), result in eax, esi is only trashed if [BLUFF:]
--  it is/can be non-integer, edi/ecx unchanged (edx is also trashed)
--  [[ then again, above does not loadReg esi anyway... so it is more
--      of a 'if integer then storeReg else clearReg'(esi), and also
--      that would be a storeReg(eax,dest,0,/1/) we'd be needing]]
--              if rootType(symtab[dest][S_vtype])=T_integer then
--DEV tryme:
--              if dtype=T_integer then
                if vroot=T_integer then
                    -- ecx/edi undamaged. (edx is also modified, btw)
                    -- record that eax==[dest] (and clear any existing use of eax)
                    storeReg(eax,dest,0,1)
--oops. (see p.asm, this might reawaken) as things stand esi is now 0 or 1...
--                  if rootType(symtab[src2][S_vtype])=T_integer then
--                      -- (as loaded above the call to opXor)
--                      storeReg(esi,src2,0,1)
--                  else
--                      -- (if src2 is a float, esi will now contain a
--                      --  [possibly 32-bit and therefore completely
--                      --   unusable] truncated integer version of it)
                        clearReg(esi)
--                  end if
                    clearReg(ecx)
                else
                    -- (all regs trashed, since dealloc could have been called)
                    reginfo = 0
                end if
            end if
            pc += 4

        elsif opcode=opRmdr then        -- 83
--trace(1)
--puts(1,"opRmdr!!!\n")
--if pc=58 then trace(1) end if
            dest = s5[pc+1]     -- res
            src = s5[pc+2]
            src2 = s5[pc+3]
            tii = s5[pc+4]      -- 1=src/src2 both init (not necc. integer)
            pc += 5
--if not isGscan then
--  if s5[pc]=opJnot then
----?11748
--      trace(1)
--  end if
--end if
            getDest()
            getSrc()
            getSrc2()
            blroot = slroot
            bmin = smin
            bmax = smax
            bmin2 = smin2
            bmax2 = smax2
            --if isGscan then
            if dtype>T_object then ?9/0 end if
            mtype = and_bits(dtype,T_atom)
            -- 11 as is, 10 -> 11, 01/00 -> 11/01
            slroot = 0
            useAndBits = 0
--19/7/15:
--          if mtype=T_N then
            if and_bits(mtype,T_N) then
--              slroot = T_integer
                slroot = T_atom
            elsif mtype<=T_integer then
--              getSrc()
--              getSrc2()
--              if and_bits(slroot,T_atom)=T_integer
                if and_bits(blroot,T_atom)=T_integer
                and and_bits(slroot2,T_atom)=T_integer then
                    slroot = T_integer
                    -- 1) can we fold?
                    if smin=smax and smin2=smax2 and smin2!=0 then
                        smin = remainder(smin,smin2)
                        smax = smin
                    else
--DEV:
if smin2=smax2 then
                        useAndBits = find(smin2,{#1,#2,#4,#8,
                                                  #10,#20,#40,#80,
                                                  #100,#200,#400,#800,
                                                  #1000,#2000,#4000,#8000,
                                                  #10000,#20000,#40000,#80000,
                                                  #100000,#200000,#400000,#800000,
                                                  #1000000,#2000000,#4000000,#8000000,
                                                  #10000000,#20000000})
end if

                        -- see t49ginfo.exw for the evolution/verification of this algorithm.
                        -- 1) for any two positive numbers, remainder(i,j)<=j-1 and <=i and >=0
                        -- 2) remainder(i,-j) = remainder(i,j)
                        -- 3) remainder(-i,j) = -remainder(i,j),
                        --  ie if i<0 then remainder(i,j)<=0 and >=-i and >=-(|j|-1)
                        --
                        -- First: calculate |j|-1 (in smax2)
                        --  (aka smax2 = max(abs(smin2),abs(smax2))-1)
                        --
                        if smax2<0 then
                            if smax2 = MININT then
                                smax2 = MAXINT
                            else
                                smax2 = -smax2
                            end if
                        end if
                        if smin2<0 then
                            if smin2=MININT then
                                smin2 = MAXINT
                            else
                                smin2 = -smin2
                            end if
                        end if
                        if smin2>smax2 then
                            smax2 = smin2
                        end if
                        smax2 -= 1  -- (mod j)-1
                        --
                        -- Then apply |j|-1, i rules:
                        --
                        if smax<0 then
                            if smax=MININT then
                                smax = MAXINT
                            else
                                smax = -smax
                            end if
                            nmax = 0
                        else
--                          nmax = min(smax,smax2)          -- (yes, I mean min())
                            if smax<smax2 then
                                nmax = smax
                            else
                                nmax = smax2
                            end if
                        end if

                        if smin<-1 then
                            if smin=MININT then
                                smin = MAXINT
                            else
                                smin = -smin
                            end if
--                          if smin>smax then
--                              smax = smin
--                          end if
--                          if smax<0 then smax = 0 end if
                            if smin<smax2 then
                                smin = -smin
                            else
                                smin = -smax2
                            end if
                        else
                            smin = 0
                        end if
                        smax = nmax
                    end if -- fold
                else -- not two ints
                    slroot = T_atom
                end if
            end if
            if slroot then -- (if dest not already fully T_atom'd up)
                sltype = slroot
                storeDest()
            end if
            --else
            if not isGscan then
                --DEV make this accept transtmpfer??
                markConstUseds({dest,src,src2})
--DEV dodgy!
--              getSrc2()
--              if slroot2=T_integer and smin2=0 and smax2=0 then
                if slroot2=T_integer and bmin2=0 and bmax2=0 then
                -- e103atgrondb0esp  ; attempt to get remainder of a number divided by 0
                --  (this message can also occur below, inlined and within opRmdr)
--                  if sched then
--                      schend()
--                  end if
--                  emitHex5w(mov_eax_imm32,103)        -- e103atgrondb0esp
                    emitHexx2(mov_al_imm8,103)          -- e103atgrondb0esp
                    emitHex5callG(opRTErn)
                    opcode = opRTErn    -- for opLabel
                else
                    if tii and dtype=T_integer then     -- inline the int/int/int case
--DEV dodgy!
--                      getSrc()
--                      if slroot=T_integer and smin=smax
                        if blroot=T_integer and bmin=bmax
--                      and slroot2=T_integer and smin2=smax2
                        and slroot2=T_integer and bmin2=bmax2
--                      and smin2!=0 then
                        and bmin2!=0 then   --DEV unnecessary?
                            dmin = remainder(bmin,bmin2)
                            dmax = dmin
                        end if
--DEV ^ unnecessary, just use smin/smax here?
                        if dmin=dmax then
                            if dname=-1 then
                                transtmpfer(dmin,-1)
                            end if
                            if dest then
                                storeImm(dest,smin)                         -- mov [dest],imm32
                                if reginfo then clearMem(dest) end if
                                if dname!=-1 then
                                    transtmpfer(dmin,-1)
                                end if
                                dest = 0
                            end if
--                      elsif slroot=T_integer
                        elsif blroot=T_integer
                          and slroot2=T_integer then
--DEV:
if useAndBits then
                                if bmin=bmax then ?9/0 end if   -- should have been dealt with above
--                              if sched then
--                                  schedule(0,0,eaxbit,pUV,0,src)
--                              end if
                                loadToReg(eax,src)                              -- mov eax,[src]
                                clearReg(eax)
                                bmin2 -= 1
                                if bmin2 then
                                    if X64 then
                                        emitHex1(#48)
                                    end if
                                    emitHex5w(and_eax_imm32,bmin2)              -- and eax,imm32
                                end if
                                if dname=-1 then
                                    transtmpfer(0,eax)
                                end if
                                if dest then
                                    storeReg(eax,dest,1,1)                      -- mov [dest],edx
                                    if dname!=-1 then
                                        transtmpfer(0,eax)
                                    end if
                                    dest = 0
                                end if
else -- not useAndBits

--                          if smin2=smax2 then
                            if bmin2=bmax2 then
--                              if smin=smax then ?9/0 end if   -- should have been dealt with above
                                if bmin=bmax then ?9/0 end if   -- should have been dealt with above
--                              if sched then
--                                  schedule(0,0,eaxbit,pUV,0,src)
--                              end if
                                loadToReg(eax,src)                          -- mov eax,[src]
                                clearReg(eax)
--                              if sched then
--                                  schedule(0,0,ecxbit,pUV,0,0)
--                              end if
--                              emitHex5w(mov_ecx_imm32,smin2)              -- mov ecx,imm32
--                              emitHex5w(mov_ecx_imm32,bmin2)              -- mov ecx,imm32
                                movRegImm32(ecx,bmin2)                      -- mov ecx,imm32
--                          else -- smin2!=smax2
                            else -- bmin2!=bmax2
--                              if smin=smax then
                                if bmin=bmax then
--                                  if sched then
--                                      schedule(0,0,eaxbit,pUV,0,0)
--                                  end if
--                                  emitHex5w(mov_eax_imm32,smin)           -- mov eax,imm32
--                                  emitHex5w(mov_eax_imm32,bmin)           -- mov eax,imm32
                                    movRegImm32(eax,bmin)                   -- mov eax,imm32
                                else
--                                  if sched then
--                                      schedule(0,0,eaxbit,pUV,0,src)
--                                  end if
                                    loadToReg(eax,src)                      -- mov eax,[src]
                                end if
                                clearReg(eax)
--                              if sched then
--                                  schedule(0,0,ecxbit,pUV,0,src2)
--                              end if
                                loadToReg(ecx,src2)                         -- mov ecx,[src2]
                            end if
                            clearReg(ecx)
--                          if sched then
--                              -- treat rest as one bit instruction
--                              schedule(eaxbit+ecxbit,0,edxbit,pUV,1,src)
--                          end if
--                          if smin2>0 then
                            if bmin2>0 then     -- always +ve
                                if X64 then
                                    emitHex1(#48)
                                end if
                                emitHex2s(xor_edx_edx)                      -- xor edx,edx
--                          elsif smax2<0 then
                            elsif bmax2<0 then  -- always -ve
--                              emitHex5w(mov_edx_imm32,-1)                 -- mov edx,-1
                                movRegImm32(edx,-1)                         -- mov edx,-1
                            else                -- must test for 0/sign-extend
                                if X64 then
                                    emitHex1(#48)
                                end if
                                emitHex2s(test_ecx_ecx)                     -- test ecx,ecx
--                              emitHex2(jnz_rel8,10)                       -- jnz @f [sj OK]
--DEVBPM backpatch me: [DONE]
--printf(1,"backpatch line 11613 pilx86.e, (emitline=%d, %s)\n",{emitline,filenames[symtab[vi][S_FPno]][2]})
--                              emitHex6j(jnz_rel32,10)                     -- jnz @f
                                emitHex6j(jnz_rel32,0)                      -- jnz @f
                                backpatch = length(x86)
                                -- e103atgrondb0esp  ; attempt to get remainder of a number divided by 0
--                              emitHex5w(mov_eax_imm32,103)
                                emitHexx2(mov_al_imm8,103)                  -- e103atgrondb0esp
                                emitHex5callG(opRTErn) -- (fatal error)
                                x86[backpatch] = length(x86)-backpatch      -- @@:
                                if X64 then
                                    emitHex1(#48)
                                end if
                                emitHex2s(mov_edx_eax)                      -- mov edx,eax
                                if X64 then
                                    emitHex1(#48)
                                end if
                                emitHex3(sar_edx_imm8,#1F)                  -- sar edx,31
                            end if
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(idiv_ecx)                             -- idiv ecx
                            if dname=-1 then
                                transtmpfer(0,edx)
                            end if
                            if dest then
                                storeReg(edx,dest,1,1)                      -- mov [dest],edx
                                if dname!=-1 then
                                    transtmpfer(0,edx)
                                end if
                                dest = 0
                            end if
end if -- useAndBits
                        end if
                    end if
                    if dest then
--                      if sched then
--                          sch00n = schoon
--                          schedule(0,0,edibit,pUV,0,0)
--                      end if
                        leamov(edi,dest)                                -- lea edi,[dest]/mov edi,dest (addr result)
--                      if sched then
--                          schend()    -- [esp]-9/-15
--                      end if
                        loadToReg(eax,src)                              -- mov eax,[src]
                        loadToReg(ecx,src2)                             -- mov ecx,[src2]
                        emitHex5callG(opRmdr)                           -- call :%opRmdr
                        -- NB src/2 obtained from [esp]-15/-9 on error [DEV]
                        reginfo = 0 -- all regs trashed
                    end if -- not inlined
                end if -- not /0 case
            end if

        elsif opcode=opNot then         -- 52
            dest = s5[pc+1]
            src = s5[pc+2]
            pc += 3
            getSrc()
            getDest()
            if and_bits(slroot,T_atom)=T_integer then
                -- not(0)=1, not(true)=0, else 0..1
                if smin=0 and smax=0 then
                    smin = 1
                    smax = 1
--              elsif (smin<0 and smax<0)
--                 or (smin>0 and smax>0) then
                elsif smax<0 or smin>0 then
                    smin = 0
                    smax = 0
                else
                    smin = 0
                    smax = 1
                end if
            end if
            slroot = T_integer
            sltype = slroot
            storeDest()
            -- [edi]=not([esi]), ie sets [edi] to 0 or 1
            if not isGscan then
                getSrc()
                if dtype=T_integer and slroot<=T_atom then
                    if slroot=T_integer and smin=smax then
                        smin = not(smin)    -- (smin:=0/1)
                        if dname=-1 then    -- a temp
                            transtmpfer(smin,-1)
                        end if
                        if dest then
                            storeImm(dest,smin)                                 -- mov [dest],imm32
                            if reginfo then clearMem(dest) end if
                            if dname!=-1 then
                                transtmpfer(smin,-1)
                            end if
                            dest = 0
                        end if
                    else
                        -- inline (dest is int and no need to check for sequence op)
                        wrk = loadReg(src)                  -- [mov wrk,[src]]
--                      if sched then
--                          schedule(0,0,edxbit,pUV,1,0)
--                      end if
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(xor_edx_edx)              -- xor edx,edx
--                      if sched then
--                          rw = regbit[wrk+1]
--                          schedule(rw,0,0,pUV,1,0)
--                      end if
                        xrm = 0o300+wrk*9 -- 0o3ww
                        emitHexx2(test_reg_reg,xrm)         -- test wrk,wrk
--                      if sched then
--                          schedule(0,0,edxbit,pUV,1,0)
--                      end if
                        xrm = #C2 -- 0o302
                        emitHex3(sete,xrm)                  -- sete dl
                        if dname=-1 then -- a temp
                            transtmpfer(0,edx)
                        end if
                        if dest then
                            storeReg(edx,dest,1,0)          -- mov [dest],edx   ; 0 or 1
--12/10
                            if dname!=-1 then
                                transtmpfer(0,edx)
                            end if
                            dest = 0
                        end if
                    end if
                end if --dtype=T_integer and slroot<=T_atom
                if dest then -- (not inlined)
--;calling convention:                              octal:         binary:          code:
--; mov edi,p1      ; result location (->0/1)       277         BF imm32        mov edi,imm32
--; mov ecx,[p2]    ; source ref                    213 015     8B 0D m32       mov ecx,[m32]
--; call opNot      ; [edi]=not(ecx)                350         E8 rel32        call rel32
--; nb p2 obtained from [esp]-9 on error
--; all registers trashed unless result is integer, else result in eax
--                  if sched then
--                      sch00n = schoon
--                      schedule(0,0,edibit,pUV,0,0)
--                  end if
                    leamov(edi,dest)                                -- lea edi,[dest]/mov edi,dest (addr result)
--                  if sched then
--                      schedule(0,0,eaxbit,pUV,0,src)
--                  end if
                    loadToReg(ecx,src)                              -- mov ecx,[src]
--                  if sched then
--                      schend()
--                  end if
                    emitHex5callG(opcode)                           -- call opNot
                    if dtype=T_integer then
                        clearReg(edi)
                        -- just record that eax==[dest], and remove any existing use of eax
                        storeReg(eax,dest,0,1)
                    else
                        -- (all regs are trashed if dealloc gets called)
                        reginfo = 0
                    end if
                end if -- dest=0
            end if

        elsif opcode=opUminus then      -- 84
            dest = s5[pc+1]
            src = s5[pc+2]
            pc += 3
            getSrc()
            getDest()
            slroot = and_bits(slroot,T_atom)
            if slroot=T_integer then
--05/01/17: (avoid constant propagation of K_rtn values, as we cannot later map them)
                if and_bits(state1,K_rtn) then
                    smin = T_const1+1
                    smax = length(symtab)
                end if
                if smin>MININT then
                    nmax = -smin
                else
                    nmax = MAXINT
                end if
                if smax>MININT then
                    smin = -smax
                end if
                smax = nmax
            end if
            sltype = slroot
            storeDest()
            -- [edi] = -[esi]
            if not isGscan then
--05/01/17 removed (and immediately undone):
                getSrc()
--05/01/17: (avoid constant propagation of K_rtn values, as we cannot later map them)
                if and_bits(state1,K_rtn) then
                    smin = T_const1+1
                    smax = length(symtab)
                end if
                if dtype=T_integer
                and slroot=T_integer
--11/11/16:
--              and smin!=MININT then
                and smin>MININT
                and smax<=MAXINT then
                    if smin=smax then
-->K_rtn
                        smin = -smin
                        if dname=-1 then        -- a temp
                            transtmpfer(smin,-1)
                        end if
                        if dest then
                            storeImm(dest,smin)                                 -- mov [dest],imm32
                            if reginfo then clearMem(dest) end if
                            if dname!=-1 then
                                transtmpfer(smin,-1)
                            end if
                            dest = 0
                        end if
                    else
                        wrk = loadReg(src,CLEAR)            -- mov wrk,[src]
--                      if sched then
--                          rw = regbit[wrk+1]
--                          schedule(rw,0,rw,pUV,1,0)
--                      end if
                        xrm = 0o330+wrk -- 0o33w, neg/wrk
                        emitHexx2(0o367,xrm)                -- neg wrk
                        if dname=-1 then -- a temp
                            transtmpfer(0,wrk)
                        end if
                        if dest then
                            storeReg(wrk,dest,1,0)          -- mov [dest],wrk   ; 0 or 1
                            if dname!=-1 then
                                transtmpfer(0,wrk)
                            end if
                            dest = 0
                        end if
                    end if
                end if
                if dest then -- (not inlined)
--;calling convention:                              octal:         binary:          code:
--; mov edi,p1          ; address of target         277         BF imm32        mov edi,imm32
--; mov ecx,[p2]        ; ref p2                    213 015     8B 0D m32       mov ecx,[m32]
--; call opUminus                                   350         E8 rel32        call rel32
--;   nb: p2 obtained from [esp]-9 on error
--;   all registers trashed unless result is integer, else result in ecx
--                  if sched then
--                      sch00n = schoon
--                      schedule(0,0,edibit,pUV,0,0)
--                  end if
                    leamov(edi,dest)                                -- lea edi,[dest]/mov edi,dest (addr result)
--                  if sched then
--                      schedule(0,0,eaxbit,pUV,0,src)
--                  end if
                    loadToReg(ecx,src)                              -- mov ecx,[src]
--                  if sched then
--                      schend()
--                  end if
                    emitHex5callG(opcode)                           -- call opUminus
                    if dtype=T_integer then
                        clearReg(edi)
                        -- just record that ecx==[dest], and remove any existing use of ecx
                        storeReg(ecx,dest,0,1)
                    else
                        -- (all regs are trashed if dealloc gets called)
                        reginfo = 0
                    end if
                end if -- dest=0
            end if

        elsif opcode=opSubss then       -- 20
            --
            -- opSubss is N, res, sliceend, idxN..idx1, ref                 -- res := ref[idx1]~[idxN..sliceend]
            --
            noofitems = s5[pc+1]
            dest = s5[pc+2]
            src = s5[pc+noofitems+4]        -- ref
            getDest()
--DEV/NB: dest=dest[i..j] can/??/ take part info iff N is 1??
--  eg/ie dest={1} gives etype of int, dest=dest[1..1] stays the same, which may seem like
--          an error, but eg dest[1]="string" merges in with it no problems... I think...
--          may even be OK for N>1?
            getSrc()
            if noofitems=1 then
                -- dest=src[i..j]
                if dest=src and bind then
                    slroot = and_bits(dtype,T_sequence)
                    setyp = detyp
--                  selen = -2
                else
--                  getSrc()    -- (plus two more calls below)
-- 14/01/10 put pack...
--10/12/09, sadly:
                    slroot = and_bits(slroot,T_sequence)
--                  slroot = and_bits(rootType(sudt),T_sequence)
                --  setyp asis
                end if
            elsif noofitems=2
              and setyp=T_string then
--DEV try instead:
--          elsif noofitems=2 then
--              -- dest=src[i][j..k]; set dtype from setyp,
--              slroot = and_bits(setyp,T_sequence)
--              <if slroot=0 then log a once-only ple() for it?>
--              if slroot=T_string then
--                  setyp = T_integer
--              else
--                  setyp = T_object
--              end if
                slroot = T_string
                setyp = T_integer
            else
                slroot = T_sequence
                setyp = T_object
            end if
            sltype = slroot
            if dlen=-2 then
                slen = -2
            else
                src2 = s5[pc+4]     -- slice start (idxN above)
                getSrc2()
                if and_bits(slroot2,T_atom)!=T_integer
                or smin2!=smax2 then
                    -- slice start not fixed
                    slen = -2
                else
                    nMin = smin2        -- (save slice start)
                    src2 = s5[pc+3]     -- slice end
                    getSrc2()
                    if and_bits(slroot2,T_atom)!=T_integer
                    or smin2!=smax2 then
                        -- slice end not fixed
                        slen = -2
                    else
                        slen = smin2-nMin+1 -- slice length
                    end if
                end if
            end if
            k = pc+noofitems+5
--DEV 15/03/10 removed (done below)
----DEV 15/11/09:
--if 0 then
--          if s5[k]=opTchk then
--              gtypeonly = 1
--          else
--              storeDest()
--          end if
--else
--          storeDest()
--end if
            if isGscan then
                pc += noofitems+5
            else
            --;calling convention:                              octal:         binary:          code:
            --; mov ecx,N           ; (literal int)             271         B9 0A000000         MOV ECX,0000000A
            --; push <return address>                           150         68 0000D041         PUSH 41D00000
            --; push dest           ; result var addr           <varies>
            --; push sliceend                                   <varies>
            --; push [idxn]..[idx1] ; pre-opUnassigned          <varies>
            --; mov esi,[src]       ; ref ("")                  <varies>
            --; jmp opSubss         ; actually a call           350         E9 <relative dword>
            --;<return address>
-- 11/10/10: (breaks p t17 [but oddly enough, not p p t17])
-- 16/10/10: fixed (reginfo=0 was missing). [DEV] gain is near zero ;-((
                if noofitems=1 
                and slroot = T_string then
--if 0 then
--?1
                    -- new: opSubsss
--;calling convention:                                  octal:         binary:          code:
--; mov edi,res         ; address of target             277         BF imm32        mov edi,imm32
--; mov eax,[p2]        ; slice start (opUnassigned)    241         A1 m32          mov eax,[m32]
--;                     ; (or imm32)                    270         B8 imm32        mov eax,imm32
--; mov ecx,[p3]        ; slice end (opUnassigned)      213 015     8B 0D m32       mov ecx,[m32]
--;                     ; (or imm32)                    271         B9 imm32        mov ecx,imm32
--; mov esi,[p1]        ; ref                           213 065     8B 35 m32       mov esi,[m32]
--;                     ; (or constref)                 276         BE imm32        mov esi,imm32
--; mov edx,p1          ; varno of ref
--; call opSubsss       ; [edi]=esi[eax..edx]           350         #E8 rel32       call rel32
--; NB p1/2/3 obtained from [esp]-9/-21/-15 on error
--; -- (p2/slicestart can always be imm32'd, (if constant)
--;     p3/sliceend can be imm32'd iff p2 is, (if constant)
--;     p2/ref can be constref'd iff p2 and p3 are imm32'd (if constant))
-- opSubsss is 1, res, sliceend, slicestart, ref                -- res := string[slicestart..sliceend]
--                  if sched then
--                      schend()    -- no attempt to/thought about schedule this
--                  end if
                    leamov(eax,dest)                                -- lea eax,[dest]/mov eax,dest (addr result)
                    src2 = s5[pc+4] -- slice start
                    getSrc2()
                    if slroot2=T_integer and smin2=smax2 then
                        if smin2 then
--DEV/SUG improve this... (put the check for 0/xor inside movRegImm32, but search first)
                            movRegImm32(edi,smin2)                  -- mov edi,imm32
                        else
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(xor_edi_edi)                  -- xor edi,edi
                        end if
                        clearReg(edi)
                    else
                        loadToReg(edi,src2)                         -- mov edi,[src2]
                    end if
                    src2 = s5[pc+3] -- slice end
                    getSrc2()
                    if slroot2=T_integer and smin2=smax2 then
                        if smin2 then
                            movRegImm32(ecx,smin2)              -- mov ecx,imm32
                        else
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(xor_ecx_ecx)              -- xor ecx,ecx
                        end if
                    else
                        loadToReg(ecx,src2)                     -- mov ecx,[src2]
                    end if
                    clearReg(ecx)
                    loadToReg(esi,src)                              -- mov esi,[src]
--                  if flag=0 then
--                      movRegVno(edx,src)                          -- mov e/rdx, varno of src
--                  end if
                    movRegVno(edx,src)                              -- mov e/rdx, varno of src
--puts(1,"warning: opSubsss skipped for newEmit (pilx86.e line 11717)\n")
                    emitHex5callG(opSubsss)                         -- call opSubsss
                    reginfo = 0 -- trashes all registers
                    pc += 6
                else
--                  if sched then
--                      sch00n = schoon
--                      schedule(0,0,ecxbit,pUV,0,0)
--                  end if
                    leamov(edx,dest)                                -- lea/mov edx,result addr
--                  emitHex5w(mov_ecx_imm32,noofitems)              -- mov ecx,noofsubscripts
                    movRegImm32(ecx,noofitems)                      -- mov ecx,noofsubscripts
--                  if sched then
--                      schend()    -- isAddr not [yet] relocatable
--                  end if
                    backpatch = emitHex5addr()                      -- push <return addr>
--DEV: (ought to be done in pmain.e?)
--  if storeReturnVar and (p2<0) then
--      -- replace rv=a[i..j] with a=a[i..j] rv=a a=0...
--      emitHex5v(push_imm32,p2)        -- push res
--  else
--      storeReturnVar = 0
                    clearReg(ecx)
                    emitHex1(push_edx)                              -- push addr res (leamov'd above)
--  end if
                    pc += 3
                    for j=1 to noofitems+1 do
--DEVlit push literals if possible... (see eg opRepe)
if 0 then
                        k = s5[pc]
                        pushvar(k)                                  -- push [sliceend],[idxn]..[idx1]
                        markConstUsed(k)
else
                        src2 = s5[pc]
                        getSrc2()
                        if slroot2=T_integer and smin2=smax2 then
                            -- a known integer value
                            if smin2>=-128 and smin2<=127 then
                                emitHexx2(push_imm8,smin2)
                            else
                                emitHex5w(push_imm32,smin2)         -- push imm32
                            end if
                        else
                            reg = loadReg(src2,NOLOAD)              -- is [src] already loaded?
                            if reg!=-1 then
                                emitHex1(push_eax+reg)              -- push reg
                            else
                                pushvar(src2)                       -- push dword[src2]
                            end if
                        end if
                        markConstUsed(src2)
end if
                        pc += 1
                    end for
--                  loadToReg(esi,s5[pc])                           -- mov esi,[ref]
                    src = s5[pc]
                    loadToReg(esi,src)                              -- mov esi,[ref]
                    movRegVno(edx,src)                              -- mov e/rdx, varno of src
                    emitHex5jmpG(opSubss)                           -- jmp opSubss (actually a call, return pushed above)
                    x86[backpatch] = length(x86)-backpatch
                    reginfo = 0 -- all regs trashed
                    pc += 1
                end if
            end if
            storeDest()

        elsif opcode=opSubse then       -- 24
            --
            -- opSubse is N, res, idxN..idx1, ref       -- res := ref[idx1][idx2]~[idxN]
            --
            noofitems = s5[pc+1]
            dest = s5[pc+2]
            getDest()
            slroot = T_object
            sltype = T_object
            setyp = T_object    -- noofitems>1 (else opSubse1[i[s|p]]), so no idea
            slen = -2
            storeDest()
            if isGscan then
                pc += noofitems+4
            else
--              if sched then
--                  schend()    -- isAddr not [yet] relocatable
--              end if
                leamov(eax,dest)
                raoffset = emitHex5addr()                       -- push <return addr> [backpatched below]
                emitHex1(push_eax)                              -- push res addr (leamov'd above)
                pc += 3
                for j=1 to noofitems do
                    -- push the index value (opUnassigned already called if rqd)
                    src = s5[pc]
                    getSrc()
                    if slroot=T_integer and smin=smax then
                        -- a known integer value
                        if smin>-128 and smin<=127 then
                            emitHexx2(push_imm8,smin)
                        else
                            emitHex5w(push_imm32,smin)          -- push imm32
                        end if
                    else
                        reg = loadReg(src,NOLOAD)               -- is [src] already loaded?
                        if reg!=-1 then
                            emitHex1(push_eax+reg)              -- push reg
                        else
                            pushvar(src)                        -- push dword[src]
                        end if
                    end if
                    markConstUsed(src)
                    pc += 1
                end for
                movRegImm32(ecx,noofitems)                      -- mov ecx,no of params
                src = s5[pc]
                leamov(edx,src)                                 -- lea edx,[src]/mov edx,addr src
                emitHex5jmpG(opSubse)                           -- jmp %pSubse (actually a call, return pushed above)
                -- set return addr offset:
                x86[raoffset] = length(x86)-raoffset
                -- all regs trashed, unless result is integer, in eax (and [dest])
                reginfo = 0
                if vroot=T_integer then
                    storeReg(eax,dest,0,0)  -- just record the fact that eax==[dest]
                end if
                pc += 1
            end if

        elsif opcode=opUnassigned then
            if not isGscan then
                src = s5[pc+1]
                reg = loadReg(src)                          -- mov reg,[src]
                cmp_h4(reg)                                 -- cmp reg,h4
                emitHex6j(jne_rel32,0)                      -- jne @f [sj prolly ok]
                backpatch = length(x86)
                movRegVno(esi,src)                          --  mov esi,src (var no/symtab index)
                emitHex5callG(opUnassigned)                 --  call :%pUnassigned (in pDiagN.e)
                x86[backpatch] = length(x86)-backpatch      -- @@:
            end if
            pc += 2

        elsif opcode=opAndBits          -- 85
           or opcode=opOrBits           -- 86
           or opcode=opXorBits then     -- 87

--if not isGscan then
--  trace(1)
--end if
            dest = s5[pc+1]
            src = s5[pc+2]
            src2 = s5[pc+3]
            tii = s5[pc+4]      --DEV shd be bothInit, with and_bits(slroot[/2],T_atom)=T_integer => tii ?
            pc += 5
--if isGscan then
            -- 1) and/or/xor_bits(int,int) yields an int (see pmain.e)
            -- 2) andbits (int,flt) | and_bits(flt,int) yields an int
--          isInt = (rootType(symtab[dest][S_vtype])=T_integer)
            getDest()
            getSrc()
            getSrc2()
--DEV: (it is refcounting up the swannie!)
--if X64=1
--and vi=7405
--and waspc=233 then
----    ?{smin,smax,smin2,smax2}
--  zzcount += 1
--  if zzcount>=7 then
--      puts(1,"count>7\n")
--  end if
----    dest = 0    -- (trapme)
--end if
            if dtype>T_object then ?9/0 end if
            if tii then
                slroot = T_integer
            else
                if opcode=opAndBits
                and (and_bits(slroot,T_atom)=T_integer
                     or and_bits(slroot2,T_atom)=T_integer) then
                    slroot = T_integer
                elsif and_bits(slroot,T_atom)=T_integer
                  and and_bits(slroot2,T_atom)=T_integer then
                    slroot = T_integer
                else
                    slroot = T_atom
                end if
            end if
            sltype = slroot
-- see t49ginfo.exw:
--6/6/10:
--          if and_bits(dtype,T_atom)=T_integer then
--          if and_bits(sltype,T_atom)=T_integer then
            if sltype=T_integer then
                if opcode=opAndBits then
                    -- if either is definitely positive, then 0..max(smax,smax2)...
                    if smin>=0 then
                        if smin2<0 then
                            smin2 = 0
                            if smax2<smax then
                                smax2 = smax
                            end if
                        end if
                    elsif smin2>=0 then
                        if smin<0 then
                            smin = 0
                            if smax<smax2 then
                                smax = smax2
                            end if
                        end if
                    end if
                    if smin=smax
                    and smin2=smax2 then
                        smin = and_bits(smin,smin2)
                        smax = smin
                    else
                        if smin>=0 and smin2>=0 then
                            if smax>smax2 then smax = smax2 end if
                        else
                            if smax<smax2 then smax = smax2 end if
                        end if
                        if smin>=0 or smin2>=0 then
                            smin = 0
                        else
                            nMin = smin+smin2
--                          if not integer(nMin) then
                            if isFLOAT(nMin) then
                                -- (see pmain.e for why and_bits(a,b)
                                --  where either a,b is an int always
                                --  yields an integer)
                                nMin = MININT
                            end if
                            smin = nMin
                        end if
                    end if
                elsif opcode=opOrBits then
-- 1) or_bits of a negative number will not yield anything lower, hence min
--  is min(smin,smin2). If both numbers are positive, then min = max(smin,smin2)
-- 2) or_bits of a negative number will not yield a positive integer, for two
--  positive integers max+max2 should suffice (note that if eg smax=#100 and
--  smax2=#100, or_bits(#100,#100) yields #100, whereas clearly if in fact p1
--  is #100 and p2 is #0FF, then #1FF would result, hence use + not or_bits.)
--trace(1)
                    if smin=smax
                    and smin2=smax2 then
                        smin = or_bits(smin,smin2)
                        smax = smin
                    else
                        if smin<0 or smin2<0 then
                            if smin>smin2 then smin = smin2 end if
                        else
                            if smin<smin2 then smin = smin2 end if
                        end if
                        if smax<0 or smax2<0 then
                            smax = -1
                        else
                            nMax = smax+smax2
--                          if not integer(nMax) then
                            if isFLOAT(nMax) then
                                nMax = MAXINT
                            end if
                            smax = nMax
                        end if
                    end if
                elsif opcode=opXorBits then
--DEV untried/untested:
--                  if smin=smax
--                  and smin2=smax2 then
--                      smin = xor_bits(smin,smin2)
--                      smax = smin
--                  else
--xor_bits: min/max of +/-(|a|+|b|)??   <<-- YUP, nuff said.    --[DEV]
--trace(1)
                        smin = MININT
                        smax = MAXINT
--                  end if
                end if
            end if

            storeDest()
            --else
            if not isGscan then
--              isInt = (rootType(symtab[dest][S_vtype])=T_integer)
--DEV tryme:
--              isInt = (dtype=T_integer)
                isInt = (vroot=T_integer)

                markConstUseds({dest,src,src2})
                if tii then -- twoInitialisedInts
                    -- result will also be integer, just in case that is not obvious:
                    -- first two bits are either 11 or 00 (for all short integers):
                    -- ===  . 11 00   ==  . 11 00   ===  . 11 00
                    -- and 11 11 00   or 11 11 11   xor 11 00 00
                    -- === 00 00 00   == 00 11 00   === 00 00 00
                    -- hence first two bits of result are 11 or 00 - which means we
                    --  get a short integer in all 12 possible cases.
if smin=smax then
                    if dname=-1 then
                        transtmpfer(smin,-1)
                    end if
                    if dest then
                        storeImm(dest,smin)                         -- mov [dest],imm32
                        if reginfo then clearMem(dest) end if
                        if dname!=-1 then   -- else above would/should have taken care of it...
                            transtmpfer(smin,-1)
                        end if
                        dest = 0
                    end if
else
--                  if sched then
--                      if not isInt then
--                          sch00n = schoon
--                      end if
--                  end if
                    reg = loadReg(src,CLEAR)                        -- mov reg,[src]
                    if not isInt then -- target is non-integer
--                      if sched then
--                          schedule(0,0,edxbit,pUV,0,dest)
--                      end if
                        if and_bits(symtab[dest][S_State],K_Fres) then ?9/0 end if
                        loadMem(edx,dest)
                    end if
                    XxxBits()   -- common code to opXxxBits and opJBits
--DEV or storeReturnVar
--              if NTyp = T_integer then -- target is integer
                    if isInt then -- target is integer
--                      storeReg(reg,dest,1,0)                      -- mov [dest],reg
                        if dname=-1 then
                            transtmpfer(0,reg)
                        end if
                        if dest then
                            storeReg(reg,dest,1,0)                  -- mov [dest],reg
                            if dname!=-1 then
                                transtmpfer(0,reg)
                            end if
                        end if
                    else
--                      if sched then
--                          schend()    -- trashes all registers
--                          sch00n = schoon -- (avoid scheduling the following storeReg)
--                      end if
                        cmp_h4(edx)                                     -- cmp edx,h4
--DEV testset missed this being commented out...
                        storeReg(reg,dest,1,0)                          -- mov [dest],reg
--DEVBPM backpatch me: [DONE]
--                      emitHex6j(jle_rel32,15)                         -- jle @f [sj NOT ok]
                        emitHex6j(jle_rel32,0)                          -- jle @f [sj NOT ok]
                        backpatch = length(x86)
if newEmit then
                        if X64 then
                            emitHex1(#48)
                            emitHex5sib(subd_sibd8i8,ebx_edx4,-16,1)    -- sub qword[rbx+rdx*4-16],1 (decref prev)
                        else
                            emitHex5sib(subd_sibd8i8,ebx_edx4,-8,1)     -- sub dword[ebx+edx*4-8],1 (decref prev)
                        end if
else
                        emitHex4sib(decd_sib,ebx_edx4,-8)               -- dec dword[ebx+edx*4-8]   ; decref prev
end if
--DEVBPM backpatch me: [DONE]
--                      emitHex6j(jnz_rel32,5)                          -- jnz @f [sj NOT ok]
                        emitHex6j(jnz_rel32,0)                          -- jnz @f [sj NOT ok]
                        backpatch2 = length(x86)
                        emitHex5callG(opDealloc)                        -- call :%pDealloc
                        x86[backpatch] = length(x86)-backpatch          -- @@:
                        x86[backpatch2] = length(x86)-backpatch2
--                      if sched then
--                          sch00n = 0 -- (actually only affects that storeReg, tho all 6 above are non-sched)
--                      end if
                        reginfo = 0 -- all regs trashed
                    end if
end if
                else    -- not twoInitialisedInts()
--                  if sched then
--                      sch00n = schoon
--                      schedule(0,0,edibit,pUV,0,0)
--                  end if
                    leamov(edi,dest)                                -- lea edi,[dest]/mov edi,dest (addr result)
                    loadToReg(ecx,src)                              -- mov ecx,[src]
                    loadToReg(eax,src2)                             -- mov eax,[src2]
--  printf(1,"warning: emitHex5callG(%d=%s) skipped for newEmit (pilx86.e line 12189)\n",{opcode,opNames[opcode]})
                    emitHex5callG(opcode)                           -- call opXxxBits
                    -- all regs trashed, unless result is integer, in eax (and [dest])
                    reginfo = 0
--                  if isInt then
--                      storeReg(eax,dest,0,0)  -- just record that eax==[dest]
--                  end if
                end if
            end if

        elsif opcode=opJbits then

            if isGscan then
                pc += 8
            else
                -- opJbits,mergeSet,tgt,link,opcode,src,src2,invert
                opcode = s5[pc+4] -- opAndBits,opOrBits,opXorBits
                src = s5[pc+5]
                src2 = s5[pc+6]
                invert = s5[pc+7]
                jinit(pc+1)
--              if sched then
--                  if mergeSet!=isOpCode then
--                      sch00n = lastJmp    -- (can schedule non-last-jumps)
--                  end if
--              end if
if 1 then   -- new code 10/12/09:
--              tmpd = 0
                if tmpd then ?9/0 end if
                getSrc()
--              if (isInit or ssNTyp1=S_Const) and slroot=T_integer and smin=smax then
                if ssNTyp1=S_Const and slroot=T_integer and smin=smax then
                    if and_bits(state1,K_rtn) then ?9/0 end if
                    getSrc2()
                    if ssNTyp2=S_Const and slroot2=T_integer and smin2=smax2 then
                        if and_bits(state2,K_rtn) then ?9/0 end if
                        tmpr = -1
                        if opcode=opAndBits then    tmpv = and_bits(smin,smin2)
                        elsif opcode=opOrBits then  tmpv = or_bits(smin,smin2)
                        elsif opcode=opXorBits then tmpv = xor_bits(smin,smin2)
                        end if
if invert then
    tmpv = not tmpv
end if
                        tmpd = 1
                    else
                        reg = spareReg(1)
--                      emitHex5w(mov_eax_imm32+reg,smin)                       -- mov reg,imm32
                        movRegImm32(reg,smin)                                   -- mov reg,imm32
                        XxxBits()   -- common code to opXxxBits and opJBits
                    end if
                else
                    reg = loadReg(src,CLEAR)                                    -- mov reg,[src]
                    XxxBits()   -- common code to opXxxBits and opJBits
                end if
else
                reg = loadReg(src,CLEAR)                -- mov reg,[src]
                XxxBits()   -- common code to opXxxBits and opJBits
end if
                bcode = 4-invert    -- invert=jz[3], not invert=jnz[4]  (idx to ccde)
--              tmpd = 0    -- DEV could do better! (same for opXxxBits)
                -- Jbits:
--              if jend(8) then ?9/0 end if -- (until tmpd fixed)
                if jend(8) then exit end if -- an opRetf fell off end of code, --> end while
            end if

--      elsif opcode>=opSge             -- 36
--        and opcode<=opSle then        -- 41
        elsif opcode=opSge              -- 36
           or opcode=opSlt              -- 37
           or opcode=opSeq              -- 38
           or opcode=opSne              -- 39
           or opcode=opSgt              -- 40
           or opcode=opSle then         -- 41
--if opcode=opSne   and emitline=55 then trace(1) end if

            dest = s5[pc+1]
            src = s5[pc+2]  -- a
            src2 = s5[pc+3] -- b
            tii = s5[pc+4]  -- twoInitialisedInts   [re-checked with gvar]
            isInit = s5[pc+5]   -- both Init            (for """")
            isInt = s5[pc+6]    -- NTyp=T_integer       [""]
            -- (warning: pc+=7 inadvisable here)
            getDest()
--if isGscan then
            getSrc()
            getSrc2()
            if not tii and isInit then
                -- both init but we didn't prove both integer in pmain.e:
                if slroot=T_integer and slroot2=T_integer then
                    tii = 1
                    s5[pc+4] = 1
                end if
            end if
            if not isInt then
                if vroot=T_integer then
                    isInt = 1
                    s5[pc+6] = 1
                end if
            end if
--          pc += 7
--DEV SetCC?
--DEV possibly extra things can be done based on types, eg int >= string is always 0
--  also 1..4<10..15 is always 1, etc.
--          dmin = min(dmin,0)
--          dmax = max(dmax,1)
--          if dmin>0 then dmin = 0 end if
--          if dmax<1 then dmax = 1 end if
            if tii then
                if opcode=opSeq then
                    if smin=smax and smin2=smax2 then
                        smin = (smin=smin2)
                    else
                        tii = 0
                    end if
                elsif opcode=opSne then
                    if smin=smax and smin2=smax2 then
                        smin = (smin!=smin2)
                    else
                        tii = 0
                    end if
                --
                -- The following examples are "smin..smax is always/never (relop) smin2..smax2";
                --  the [1..4] show the correct mirror-pairing we should expect.
                --
--DEV ideally gscan should have done this... (once local values in place)
                elsif opcode=opSlt then
                    if smax<smin2 then      -- eg 1..2 is always < 3..4     [1]
                        smin = 1
                    elsif smin>=smax2 then  -- eg 2..3 is never < 1..2      [2]
                        smin = 0
                    else
                        tii = 0
                    end if
                elsif opcode=opSle then
                    if smax<=smin2 then     -- eg 1..2 is always <= 2..3    [3]
                        smin = 1
                    elsif smin>smax2 then   -- eg 3..4 is never <= 1..2     [4]
                        smin = 0
                    else
                        tii = 0
                    end if
                elsif opcode=opSgt then
                    if smin>smax2 then      -- eg 3..4 is always > 1..2     [4]
                        smin = 1
                    elsif smax<=smin2 then  -- eg 1..2 is never > 2..3      [3]
                        smin = 0
                    else
                        tii = 0
                    end if
                elsif opcode=opSge then
                    if smin>=smax2 then     -- eg 2..3 is always >= 1..2    [2]
                        smin = 1
                    elsif smax<smin2 then   -- eg 1..2 is never >= 3..4     [1]
                        smin = 0
                    else
                        tii = 0
                    end if
                else
                    ?9/0 -- opcode in error?!?
                end if
                smax = smin
            end if -- tii
            if not tii then -- NB not else!
                smin = 0
                smax = 1
            end if
            slroot = T_integer
            sltype = T_integer
            storeDest()
--else

            -- opSxx handle integers, floats, strings, [nested] sequences,
            --  report unassigned arguments, and deallocate [edi] if rqd.
            --  However if we are certain src/2 are init integers, then an
            --  inline cmp will do just fine. (NB: the use of EITHER in
            --  twoInitialisedInts refers to "are integer"; it returns
            --  0/false if either unassigned [except for S_Consts])

            --       Scde = {opSlt,opSle,opSeq,opSne,opSge,opSgt}
            --                  37    41    38    39    36    40
            if isGscan then
                pc += 7
            else
                markConstUseds({dest,src,src2})
                tii = s5[pc+4]  -- as damaged above
                pc += 7

                if tii and isInt then
                    bcode = find(opcode,Scde)
                    -- if result predictable set tmpv to 0/1 and tmpd to -1
                    if SetCC(opcode,0) then
                        --
                        -- we must do a compare.
                        --
                        -- we need an r8 reg for the result (ie eax, ecx, or edx,
                        --  but not esi/edi. ebx would also theoretically do but
                        --  we like to keep that as zero, for use as a base.)
                        --
                        if reg!=eax and wrk!=eax then
                            res = eax
                            if reginfo then clearReg(eax) end if
                        elsif reg!=ecx and wrk!=ecx then
                            res = ecx
                            if reginfo then clearReg(ecx) end if
                        else
                            res = edx
                        end if
--                      if sched then
--                          rb = regbit[reg+1]
--                          rs = regbit[res+1]
--                          schedule(0,0,rs,pUV,1,0)
--                      end if
                        xrm = 0o300+res*9 -- 0o3rr
-- (#31 = m_xor+1):
                        emitHexx2(0o061,xrm)                            -- xor res,res
                        mod = m_cmp
                        if tmpd=-1 then
                            regimm365(smin2)                            -- cmp reg,imm
                        else
--                          if sched then
--                              rw = regbit[wrk+1]
--                              schedule(rb+rw,0,0,pUV,1,0)
--                          end if
                            op1 = mod+1         -- 0o071
                            xrm = 0o300+wrk*8+reg -- 0o3wr
                            emitHexx2(op1,xrm)                          -- cmp reg,wrk
                        end if
--                      if sched then
--                          schedule(rs,0,rs,pNP,1,0)
--                      end if
                        k = ccde[bcode][2]+#10
                        jcode = {#0F,k}
                        xrm = #C0+res   -- 0o30r
                        emitHex3(jcode,xrm)                             -- setcc res8
                        tmpd = 0
                        if dname=-1 then
                            transtmpfer(0,res)
                        end if
                        if dest then
                            storeReg(res,dest,1,0)                      -- mov [dest],res
--12/10:
                            if dname!=-1 then
                                transtmpfer(0,res)
                            end if
                        end if
                    else
                        -- tmpd=-1, tmpv is 0 or 1 fixed result
                        tmpd = 0
                        if dname=-1 then    -- a temp
                            transtmpfer(tmpv,-1)
                        end if
                        if dest then    -- not transtmpfer'd
                            storeImm(dest,tmpv)                         -- mov [dest],imm32
                            if reginfo then clearMem(dest) end if
                            if dname!=-1 then
                                transtmpfer(tmpv,-1)
                            end if
                        end if
                    end if
                else   -- use opSxx to compare floats/strings/sequences, trap unassigned, dealloc prev, etc
                    if tmpd then ?9/0 end if
--                  if sched then
--                      sch00n = schoon
--                      schedule(0,0,edxbit,pUV,0,0)
--                  end if
--                  if sched then
--                      schend()    -- [esp]-9/-15
--                  end if
                    loadToReg(edi,src2,CLEAR)                           -- mov edi,[src2]
                    loadToReg(eax,src,CLEAR)                            -- mov eax,[src]
                    movRegVno(esi,src2)                                 -- mov e/rsi,src2 (var no)
                    movRegVno(edx,src)                                  -- mov e/rdx,src (var no)
                    if opcode=opSeq
                    or opcode=opSne then
                        emitHex5callG(opJeq)                            -- call :%opJccE  (opJne would do the same)
                    else
                        emitHex5callG(opJle)                            -- call :%opJcc   (opJlt/opJge/opJgt "")
                    end if
                    if X64 then
                        emitHex1(#48)
                    end if
                    emitHex2s(mov_eax_ebx)                              -- mov eax,ebx(0)
                    if not isInt then
                        loadMem(edx,dest)                               -- mov edx,[dest]
                    end if
                    emitHex3l(#0F,setc[find(opcode,Scde)],0o300)        -- setcc al
                    reginfo = 0
                    if isInt then
                        storeReg(eax,dest,1,0)                          -- mov [dest],eax
                    else
                        storeMem(dest,eax)                              -- mov [dest],eax
                        cmp_h4(edx)                                     -- cmp edx,h4
                        emitHex6j(jle_rel32,0)                          -- jle @f [sj NOT ok]
                        backpatch = length(x86)
                        if X64 then
                            emitHex1(#48)
                            emitHex5sib(subd_sibd8i8,ebx_edx4,-16,1)    -- sub qword[rbx+rdx*4-16],1 (decref prev)
                        else
                            emitHex5sib(subd_sibd8i8,ebx_edx4,-8,1)     -- sub dword[ebx+edx*4-8],1 (decref prev)
                        end if
                        emitHex6j(jnz_rel32,0)                          -- jnz @f [sj NOT ok]
                        backpatch2 = length(x86)

                        emitHex5callG(opDealloc)                        -- call :%pDealloc

                        x86[backpatch] = length(x86)-backpatch          -- @@:
                        x86[backpatch2] = length(x86)-backpatch2
                    end if
                end if
            end if

        elsif opcode=opOpen
           or opcode=opSeek
           or opcode=opMemCopy
           or opcode=opMemSet then
--if isGscan then
            if opcode=opMemCopy
            or opcode=opMemSet then
                -- do nowt, other than pc+=4 below
            else
                dest = s5[pc+1]
                getDest()
                slroot = T_integer
                if opcode=opSeek then
--                  slroot = T_integer
                    smin = 0
                    smax = 1
                elsif opcode=opOpen then
--                  slroot = T_integer
                    smin = -1
                    smax = 10000    -- reasonable limit?
                else
                    ?9/0
                end if
                sltype = slroot
                storeDest()
            end if
            --else
            if not isGscan then
                dest = s5[pc+1] -- result (except for memcopy/set)
                src = s5[pc+2]
                src2 = s5[pc+3]
                markConstUseds({dest,src,src2})
                if opcode=opOpen
                or opcode=opSeek then
                    leamov(edi,dest)                -- lea edi,[dest]           -- result location
                    loadToReg(eax,src)              -- mov eax,[filepath/fn]    -- (opUnassigned)
                else
                    loadToReg(edi,dest)             -- mov edi,[p1] (dest addr) -- (opUnassigned)
                    if opcode=opMemCopy then
                        loadToReg(esi,src)          -- mov esi,[p2] (src addr)  -- (opUnassigned)
                    elsif opcode=opMemSet then
                        loadToReg(eax,src)          -- mov eax,[p3] (byte value)-- (opUnassigned)
                    end if
                end if
                loadToReg(ecx,src2)                 -- mov ecx,[p3] -- (mode/pos/length, opUnassigned)
                emitHex5callG(opcode)               -- call :%opOpen/opSeek/opMemCopy/opMemSet
                reginfo = 0 -- all regs trashed
                if opcode=opSeek then
                    if vroot=T_integer then     -- (so no dealloc)
                        storeReg(eax,dest,0,0)  -- just record that eax==[dest]
                    end if
                end if
            end if
            pc += 4

        elsif opcode=opAlloc            -- 91
           or opcode=opNotBits          -- 88
           or opcode=opRand then        -- 61
--trace(1)
            dest = s5[pc+1]
            src = s5[pc+2]
--if isGscan then
            getDest()
            getSrc()
            slroot2 = slroot
            smin2 = smin
            smax2 = smax
            if opcode=opAlloc then      -- 91
                slroot = T_atom
            elsif opcode=opNotBits then     -- 88
                if and_bits(slroot,T_atom)=T_integer then
                    --
                    -- An integer is anything starting 0b11 or 0b00,
                    -- so not_bits(<integer>) always yields integer:
                    --
                    --  not_bits(MININT) is MAXINT
                    --   (aka not_bits(-1073741824) is +1073741823)
                    --  ...
                    --  not_bits(-2) is 1
                    --  not_bits(-1) is 0   (aka ~#FFFF = #0000)
                    --  not_bits(0) is -1   (aka ~#0000 = #FFFF)
                    --  not_bits(1) is -2
                    --  ...
                    --  not_bits(MAXINT) is MININT
                    --   (aka not_bits(1073741823) is -1073741824)
                    --
--DEV can we get away with:
--  tmp = not_bits(smin)
--  smin = not_bits(smax)
--  smax = tmp
--  (and no test)
-- or even {smin,smax} = {not_bits(smax),not_bits(smin)}
-- so: added this 24/12/14, give it a couple of months yet... (and even if nowt caught, do some more testing, using a pair of nested loops, on MININT,-1,0,1,MAXINT, and more)
{Tsmin,Tsmax} = {not_bits(smax),not_bits(smin)}                 
                    smin = not_bits(smin)
                    smax = not_bits(smax)
                    slroot = T_integer
                    if smin>smax then
                        tmp = smin
                        smin = smax
                        smax = tmp
                    end if
if {smin,smax}!={Tsmin,Tsmax} then ?9/0 end if
                else
                    -- not_bits(atom) may be atom or integer...
                    --  eg: not_bits(0.1) is -1, (an integer), but
                    --      not_bits(MAXINT+1) is MININT-1.
                    slroot = T_atom
                end if
            elsif opcode=opRand then        -- 61
                smin = 1
                if and_bits(slroot,T_atom)!=T_integer then
                    -- (min/max invalid if var might be atom, so assume worst)
                    smax = MAXINT
                end if
                slroot = T_integer
            end if
            sltype = slroot
            storeDest()
            --else
            if not isGscan then
                --DEV unused??
                isInit = s5[pc+3]
                markConstUseds({src,dest})
    if opcode=opNotBits
    or opcode=opRand then
--DEVimm enhanced 24/12/14:
                if slroot=T_integer and smin=smax then          -- (output is fixed, eg not_bits(-1), or rand(1), possibly only known after constant propagation)
                    storeImm(dest,smin)                         -- mov [dest],imm32
                else
                    if slroot2=T_integer and smin2=smax2 then   -- (input is fixed)
                        movRegImm32(eax,smin2)                  -- mov eax,smin
                    else
                        loadToReg(eax,src)                      -- mov eax,[src]
                    end if
                    leamov(edi,dest)                            -- lea edi,[dest]/mov edi,dest (addr result)
                    emitHex5callG(opcode)                       -- call opAlloc/NotBits/Rand
                end if
    elsif opcode=opAlloc then
                -- size in ecx, result addr in edi
                if slroot2=T_integer and smin2=smax2 then       -- (input is fixed)
                    movRegImm32(ecx,smin2)                      -- mov ecx,smin
                else
                    loadToReg(ecx,src)                          -- mov eax,[src]
                end if
                leamov(edi,dest)                                -- lea edi,[dest]/mov edi,dest (addr result)
                emitHex5callG(opcode)                           -- call opAlloc
    else
--puts(1,"opAlloc missing line 12697 pilx86.e\n")   -- (currently hll, but should really be :%opAlloc)
                ?9/0
    end if
                reginfo = 0 -- all regs trashed
            end if
            pc += 4
--DEV (temp)
--      elsif opcode=opMalloc then
--          dest = s5[pc+1]
--          src = s5[pc+2]
--          getDest()
--          slroot = T_atom
--          sltype = slroot
--          storeDest()
--          if not isGscan then
--              markConstUseds({src,dest})
----opMalloc
---- /**/       #ilASM{ lea edi,[result]
---- /**/               mov eax,[size]
---- /**/               call :%opMalloc }   -- [edi] := malloc(eax)
--
----                if sched then
----                    sch00n = schoon
----                    schedule(0,0,edibit,pUV,0,0)
----                end if
--              leamov(edi,dest)                                -- lea edi,[dest]/mov edi,dest (addr result)
--              getSrc()
--              if slroot=T_integer and smin=smax then
----                    if sched then
----                        schedule(0,0,eaxbit,pUV,0,0)
----                    end if
--                  emitHex5w(mov_eax_imm32,smin)               -- mov eax,imm32
--                  movRegImm32(eax,smin)                       -- mov eax,imm32
--              else
----                    if sched then
----                        schedule(0,0,eaxbit,pUV,0,src)
----                    end if
--                  loadToReg(eax,src,CLEAR)                    -- mov eax,[src]
--              end if
----                if sched then
----                    schend()
----                end if
----                lblidx = tt[aatidx[opMalloc]+EQ]
----                if lblidx=0 then ?9/0 end if
----                x86 &= {call_rel32,isJmpG,0,0,lblidx}
--              emitHex5callG(opMalloc)
--              reginfo = 0 -- all regs trashed
--          end if
--          pc += 3
--      elsif opcode=opMfree then
--          if not isGscan then
--              src = s5[pc+1]
--              markConstUsed(src)
----opMfree
---- /**/       #ilASM{ mov eax,[addr]
---- /**/               call :%opMfree }    -- mfree(eax)
----                if sched then
----                    schedule(0,0,eaxbit,pUV,0,src)
----                end if
--              loadToReg(eax,src,CLEAR)                    -- mov eax,[src]
----                if sched then
----                    schend()
----                end if
----                lblidx = tt[aatidx[opMfree]+EQ]
----                if lblidx=0 then ?9/0 end if
----                x86 &= {call_rel32,isJmpG,0,0,lblidx}
--              emitHex5callG(opMfree)
--              reginfo = 0 -- all regs trashed
--          end if
--          pc += 2
        elsif opcode=opCallOnce then    -- 13
            routineNo = s5[pc+1]
if NOLT=0 or bind or lint then
            symk = symtab[routineNo]
            gmask = symk[S_Efct]
            if and_bits(gmask,E_vars) then
                ltCall(0,gmask,pc)  -- clear rqd gvar info
            end if
end if -- NOLT
            if not isGscan then
                -- don't opLn/t/p/pt for this:
--2/12/14 why?? (removed, under newEmit just(/only/simply) to be on the safe side)
if newEmit then
else
                wasemitline = emitline
                emitline = lastline
end if
--              if sched then
--                  sch00n = schoon
--                  schedule(0,0,edibit,pUV,0,0)
--              end if
if newEmit then
                movRegVno(edi,routineNo)                    -- mov e/rdi,routineNo
else
                emitHex5w(mov_edi_imm32,routineNo)          -- mov edi,routineNo
end if
--              if sched then
--                  schend()
--              end if
                emitHex5callG(opCallOnce)
                reginfo = 0 -- all regs trashed
if newEmit then
else
                emitline = wasemitline
end if
                oplnlen = length(x86)
            end if
            pc += 2

        elsif opcode=opConcatN then     -- 21
    --
    -- opConcatN is N, refN..ref1, res                              -- res := ref1&ref2..&refN
    --
            waspc = pc
            noofitems = s5[pc+1]
            dest = s5[pc+noofitems+2]
            getDest()
            pc += 2
            slroot = 0
            setyp = 0
            slen = 0
            notString = 0
            while noofitems do
                -- accumulate sequence length and element type
                if slen=-2
                and setyp=T_object
                and notString then
                    -- no point continuing once {0b01xx,-,-,T_object,-2}
                    pc += noofitems
                    exit
                end if
                src2 = s5[pc]
                getSrc2()
                if not and_bits(slroot2,T_sequence) then
                    -- atom only
                    setyp = or_bits(setyp,slroot2)
                    if slroot2=T_integer
                    and smin>=0 and smax<=255 then
                        slroot2 = T_string
                    elsif and_bits(slroot2,T_integer)
                      and smin<=255 and smax>=0 then
                        slroot2 = T_sequence
                    else
--                      slroot2 = T_Dsq
                        notString = 1
--                      slroot = T_sequence -- becomes T_Dsq below
                    end if
                    slroot = or_bits(slroot,slroot2)
                    if slen>=0 then
                        slen += 1
                    end if
                else
                    mtype = and_bits(slroot2,T_atom)
                    if mtype then
                        -- can be atom or sequence:
                        if slroot2=T_string+T_integer
                        and smin>=0 and smax<=255 then
                            slroot2 = T_string
                        elsif mtype=T_N
                           or smin>255 or smax<0 then
--                          slroot2 = T_Dsq
                            notString = 1
--                          slroot = T_sequence -- becomes T_Dsq below
                        else
                            slroot2 = T_sequence
                        end if
                        slroot = or_bits(slroot,slroot2)
                        if slen2=1 and slen>=0 then
                            slen += 1
                        else
                            slen = -2
                        end if
                        setyp = or_bits(setyp,or_bits(mtype,setyp2))
                    else
                        -- sequence/string only
                        slroot = or_bits(slroot,slroot2)
                        if setyp2=T_Dsq then
                            notString = 1
--                          slroot = T_sequence -- becomes T_Dsq below
                        end if
                        setyp = or_bits(setyp,setyp2)
                        if slen2>=0 and slen>=0 then
                            slen += slen2
                        else
                            slen = -2
                        end if
                    end if
                end if
                noofitems -= 1
                pc += 1
            end while
            if notString then
                slroot = T_Dsq
            end if
            sltype = slroot
            pc += 1     -- skip dest (already retrieved above)
            storeDest()
            --else
            if not isGscan then
                pc = waspc
                noofitems = s5[pc+1]    -- as damaged above...
--              if sched then
--                  sch00n = schoon
--                  schedule(0,0,ecxbit,pUV,0,0)
--              end if
--              emitHex5w(mov_ecx_imm32,noofitems)                  -- mov ecx,noofitems
                movRegImm32(ecx,noofitems)                          -- mov ecx,noofitems
--              if sched then
--                  schend()    -- isAddr not [yet] relocatable
--              end if
                backpatch = emitHex5addr()                          -- push <return addr>
                pc += 2
                for j=1 to noofitems do
                    k = s5[pc]
                    pushvar(k)                                      -- push [refN]..[ref1]
                    markConstUsed(k)
                    pc += 1
                end for
                leamov(eax,s5[pc])                                  -- lea/mov eax,addr res
                emitHex5jmpG(opConcatN)                             -- jmp opConcatN (actually a call, return pushed above)
                x86[backpatch] = length(x86)-backpatch
                reginfo = 0 -- all regs trashed
                pc += 1
            end if

        elsif opcode=opFree             -- 125
           or opcode=opTrace            -- 139
           or opcode=opProfile          -- 140
           or opcode=opSetRand          -- 135
           or opcode=opSleep then       -- 136
--         or opcode=opCallA            -- 120
--         or opcode=opCrshMsg
--         or opcode=opCrshFile
--         or opcode=opCrshRtn then

--          if opcode=opCallA then
--if NOLT=0 or bind or lint then
--              ltCall(0,E_vars,pc) -- clear all, to be safe
--end if -- NOLT
--          end if

            if not isGscan then
                --(gscan already dealt with above) [removed 17/3/09]
                src = s5[pc+1]
                markConstUsed(src)
--if opcode=opTrace and lastline!=emitline then
--  callopTraceFirst = src
--  lineinfo()
--else
    if opcode=opSetRand 
    or opcode=opSleep
    or opcode=opFree
--DEV??
    or opcode=opTrace
    or opcode=opProfile then
                getSrc()
                if slroot=T_integer and smin=smax then
                    if smin then
                        movRegImm32(eax,smin)                   -- mov eax,imm32
                    else
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(xor_eax_eax)                  -- xor eax,eax
                    end if
                else
                    loadToReg(eax,src)                          -- mov eax,[src]
                end if
                emitHex5callG(opcode)                           -- call opXxxx
    else
printf(1,"warning: emitHex5call(%d=%s) skipped for newEmit, pilx86.e line 13580\n",{opcode,opNames[opcode]})
    end if
--end if
                reginfo = 0
            end if
            pc += 2

        elsif opcode=opJlen then

            if isGscan then
                pc += 8
            else
                -- opJlen,mergeSet,tgt,link,src,tvar,isInit,invert
                src = s5[pc+4]  -- we're implementing "if [not] length(src) then"
                tvar = s5[pc+5] -- a temp var we might need:
-- tvar is zero iff src is init non-atom, else it is a temp var for calling opLen.
--DEV also, for fixed length sequences, jump always/jump never.
--          isInit = s5[pc+6]   --DEV!
--DEV (31/7/09): try this
--          if tvar
--          and isInit then
--              -- so pmain.e thought we needed a tvar because src might
--              --  be a non-sequence (ie call opLen to catch errors);
--              --  hence if gvar_scan has now proved that src is not
--              --  an atom (,ever,) then we can inline.
--              -- **DEV** (Alternatively we could invent opLen0 to
--              --          get round needing/not needing a tvar,
--              --          which just leaves the result in ecx..)
--              getSrc()
--              if not and_bits(slroot,T_atom) then
--                  tvar = 0    -- can inline
--              end if
--          end if
                invert = s5[pc+7]
                jinit(pc+1)
--DEV getSrc/plen>=0/plen=0/jskip!!!
--DEV schoon or 0 for isOpCode?
                if tvar=0 then
--                  if sched then
--                      if mergeSet!=isOpCode then
--                          sch00n = lastJmp    -- (can schedule non-last-jumps)
--                      end if
--                  end if
                    reg = loadReg(src)                              -- mov reg,[src]
                    wrk = spareReg(1)   --DEV 0 might prove better? - prolly about to use it rsn!!
                                    -- (again, need to collect metrics before deciding)
if X64 then
                    sibLoad743(wrk,by4,reg,ebx,-24)                 -- mov wrk,[rbx+reg*4-24]
else
                    sibLoad743(wrk,by4,reg,ebx,-12)                 -- mov wrk,[ebx+reg*4-12]
end if
                else
--                  if sched then
--                      sch00n = schoon
--                      schedule(0,0,edibit,pUV,0,0)
--                  end if
                    leamov(edi,tvar)                                -- lea edi,[tvar]/mov edi,tvar
--                  if sched then
--                      schend()    -- [esp]-9
--                  end if
                    loadToReg(esi,src)                              -- mov esi,[src]
                    movRegVno(edx,src)                              -- mov e/rdx,src (var no)
                    emitHex5callG(opLen)                            -- call :%opLen
                    -- all regs trashed, unless result is integer, in ecx (and [tvar]), esi untouched
                    --                   (\ which it always is /), since tvar=newTmpVar(T_integer)
                    if reginfo then clearReg(edi) end if
                    -- (strictly, ecx==[tvar], but tvar is a tmp, so no point recording that fact:)
                    if reginfo then clearReg(ecx) end if
--              storeReg(ecx,tvar,0,1) -- record that ecx==[tvar]
--13/5/15:
--                  storeReg(eax,src,0,1) -- record that eax==[src]
if newEmit then
                    storeReg(esi,src,0,1)   -- record that esi==[src]
else
                    storeReg(eax,src,0,1)   -- record that eax==[src]
end if
                    wrk = ecx
--DEV create localtype info that src is a sequence???
                end if
--              if sched then
--                  if mergeSet!=isOpCode then
--                      sch00n = lastJmp    -- (can schedule non-last-jumps)
--                  end if
--                  schedule(regbit[wrk+1],0,0,pUV,1,0)
--              end if
                xrm = 0o300+wrk*9 -- 0o3ww
                emitHexx2(test_reg_reg,xrm)             -- test wrk,wrk

                bcode = 4-invert    -- invert=jz[3], not invert=jnz[4]  (idx to ccde)
                tmpd = 0    -- DEV could do better! (as noted above, getSrc/slen)
                -- Jlen:
                if jend(8) then ?9/0 end if -- (until tmpd fixed)
--              if jend(8) then exit end if -- an opRetf fell off end of code, --> end while
            end if

        elsif opcode=opRepe then        -- 17
            --
            -- opRepe is n (noofsubscripts), rep, idxn..idx1, ref
            --   implements ref[idx1][idx2]~[idxn] := rep
            -- see also opRepe1
            --
--;calling convention:                              octal:         binary:          code:
--; push <return addr>                              150         68 imm32        push imm32
--; push [replacement]  ; (as below)
--; push [idxn]..[idx1] ; (opUnassign'd)			150 		68 imm32		push imm32
--;                     ;                   or      push dword[m32]
--;                     ;                   or      push dword[ebp+d8]
--;                     ;                   or      push dword[ebp+d32]
--; mov ecx,p1          ; n                         271         B9 imm32        mov ecx,imm32
--;                                         or      lea edi,[ebp+d8]
--;                                         or      lea edi,[ebp+d32]
--; mov eax,p2          ; addr of ref               270         B8 imm32        mov eax,imm32
--;                                         or      lea eax,[ebp+d8]
--;                                         or      lea eax,[ebp+d32]
--; jmp opRepe          ; actually a call           351         E9 rel32        jmp rel32
--;<return addr>
            waspc = pc
            noofitems = s5[pc+1]
            src = s5[pc+2]  -- aka rep
--if isGscan then
            pc += noofitems+4
            --
            -- The only thing we need to watch out for here is eg:
            --  x={"fred"}; x[i][j]=<not char>, which will convert
            --  that string element to a dseq, hence we must make
            --  etyp include T_Dsq.
            --
            if noofitems=2 then
                dest = s5[pc-1]
                getDest()
                mtype = and_bits(detyp,T_sequence)
--              if mtype=0 then mtype=9/0 end if    -- sanity check
                if mtype=T_string then
                    getSrc()
                    if slroot!=T_integer or smin<0 or smax>255 then
                        slroot = T_Dsq  -- (to force update of detyp, must be T_Dsq anyway)
                        sltype = T_Dsq
                        setyp = T_Dsq   -- (catch that x={"fred"}; x[i][j]=<not char>)
                        slen = dlen     -- no change
                        storeDest()
                    end if
                end if
--DEV removed (we never even called getDest!) 27/2/09
--          elsif DEBUG then
----            else -- noofitems>=3 (1 would be opRepe1)
--              if not and_bits(dtype,T_Dsq) then dtype=9/0 end if
            end if
            --          storeDest()
            --else
            if not isGscan then
                pc = waspc
--              if sched then
--                  schend()    -- isAddr not [yet] relocatable
--              end if
                raoffset = emitHex5addr()                       -- push <return addr> [backpatched below]
                pc += 2
                for j=1 to noofitems+1 do                       -- push rep,idxn..idx1
                    -- push the rep/index value (opUnassigned already called if rqd)
                    src = s5[pc]
                    getSrc()
                    if slroot=T_integer and smin=smax then
                        -- a known integer value
                        if smin>=-128 and smin<=127 then
                            emitHexx2(push_imm8,smin)
                        else
                            emitHex5w(push_imm32,smin)          -- push imm32
                        end if
                    else
                        reg = loadReg(src,NOLOAD)               -- is [src] already loaded?
                        if reg!=-1 then
                            emitHex1(push_eax+reg)              -- push reg
                        else
                            pushvar(src)                        -- push dword[src]
                        end if
                    end if
                    markConstUsed(src)
                    pc += 1
                end for
--              emitHex5w(mov_ecx_imm32,noofitems)              -- mov ecx,noofsubscripts
                movRegImm32(ecx,noofitems)                      -- mov ecx,noofsubscripts
                leamov(eax,s5[pc])                              -- lea/mov eax,addr ref
                emitHex5jmpG(opRepe)                            -- jmp :%opRepe (actually a call, return pushed above)
                -- set return addr offset:
                x86[raoffset] = length(x86)-raoffset
                reginfo = 0 -- all regs trashed
                pc += 1
            end if

        elsif opcode=opDeSeq then
            if not isGscan 
            and with_js=1 then
                src = s5[pc+1]                                  -- T_const0 or T_const1
                if src=T_const0 then
                    if X64 then
                        emitHex1(#48)
                    end if
                    emitHex2s(xor_eax_eax)                      -- xor eax,eax (eax:=0)
                else
                    movRegImm32(eax,1)                          -- mov eax,1
                end if
--              movRegImm32(eax,v)
--              loadToReg(eax,src)                              -- mov eax,[src]
                emitHex5callG(opDeSeq)                          -- call :%opXxxx
                clearReg(eax)
            end if
            pc += 2

        elsif opcode=opReps then        -- 18
            --
            -- opReps is n (noofsubscripts), rep, sliceend, idxn..1, ref    -- ref[idx1]~[idxn..sliceend] := rep
            --
            noofitems = s5[pc+1]
            src2 = s5[pc+2]         -- replacement
            dest = s5[pc+noofitems+4]
            getDest()
            getSrc2()               -- nb slen2 used later
            if dtype>T_object then ?9/0 end if
            -- first default to "do nowt":
            slroot = and_bits(dtype,T_sequence)
            setyp = detyp
            slen = dlen
            -- then:
            if noofitems=1 then
                --
                -- first deal with s[i..j]=atom:
                --
                mtype = and_bits(slroot2,T_atom)
                if mtype then
                    -- The only concern here is eg x="string" x[i..j]=-1, which
                    --  yields {'s','t',-1,-1,'n','g'}, so ensure T_Dsq bit set.
                    if mtype!=T_integer or smin2<0 or smin2>255 then
                        slroot = T_Dsq
                    end if
--15/10/14:
                    setyp = or_bits(setyp,mtype)
                end if
                --
                -- then deal with s[i..j]=sequence:
                --  (nb rep/slroot2 may be object, this is most definitely NOT an elsif to the above!)
                --
                mtype = and_bits(slroot2,T_sequence)
                if mtype then
                    if and_bits(mtype,T_Dsq) then
                        -- cover string[i..j]=T_Dsq, but not T_Dsq[i..j]=string, iygwim
                        -- (the former converts eg "fred" to {'f',-1,-2,'d'}, whereas
                        --  latter is {'f',-1,-2,'d'} to {'f','r','e','d'}, no problemo.)
                        slroot = T_Dsq
                    end if
                    setyp = setyp2
                    if dlen!=-2 then
                        -- if we haven't already said "any length",
                        -- trash dlen if length(rep)!=slice length
                        src2 = s5[pc+4] -- slice start
                        getSrc2()
                        if and_bits(slroot2,T_atom)!=T_integer
                        or smin2!=smax2 then
                            -- slice start not fixed
                            slen = -2
                        else
                            slen = slen2    -- save replacement length
                            smin = smin2    -- and slice start
                            src2 = s5[pc+3] -- slice end
                            getSrc2()
                            if and_bits(slroot2,T_atom)!=T_integer
                            or smin2!=smax2 then
                                -- slice end not fixed
                                slen = -2
                            else
                                smin2 = smin2-smin+1    -- slice length
                                if smin2!=slen then
                                    slen = -2
                                else
                                    slen = dlen
                                end if
                            end if
                        end if
                    end if
                end if
            elsif and_bits(detyp,T_sequence)=T_string then
                -- cover eg s={"fred"}; s[1][2..3]=-1
                if noofitems!=2
                or (    (slroot2!=T_integer or smin2<0 or smax2>255)    -- s[1][2..3]='X' (any byte) ok
                    and (slroot2!=T_string)) then                       -- s[2][2..3]="abc" (any length) ok
                    setyp = T_Dsq
                end if
            end if
            sltype = slroot
            storeDest()
            if isGscan then
                pc += noofitems+5
            else
--DEV out of date: (see VM/pRepsN.e)
                --;calling convention:                              octal:         binary:          code:
                --; mov ecx,n       ; (literal int)                 271         B9 0A000000         MOV ECX,0000000A
                --; push <return address>                           150         68 0000D041         PUSH 41D00000
                --; mov edi,rep     ; replacment addr               277         BF C4194000         MOV EDI,P.004019C4
                --; mov esi,sliceend                                276         BE 34174000         MOV ESI,P.00401734
                --; push [idxn]..[idx1]                             ?!? 150         68 0000D041         PUSH 41D00000
                --; mov eax,ref     ; ref                           270         B8 18354200         MOV EAX,BTEXW.00423518
                --; jmp opReps      ; actually a call               350         E9 <relative dword>
                --;<return address>
                rep = s5[pc+2]  -- rep
                src2 = s5[pc+3] -- sliceend
--              if sched then
--                  sch00n = schoon
--                  schedule(0,0,ecxbit,pUV,0,0)
--              end if
--              emitHex5w(mov_ecx_imm32,noofitems)                  -- mov ecx,noofsubscripts
                movRegImm32(ecx,noofitems)                          -- mov ecx,noofsubscripts
--              if sched then
--                  schend()    -- isAddr not [yet] relocatable
--              end if
                backpatch = emitHex5addr()                          -- push <return addr>
if newEmit then
--  printf(1,"opReps, emitline=%d (pilx86 line 13136)\n",emitline)
--trace(1)
                pushvar(rep)                                        -- push [rep]
--DEVlit push literals if possible... (see eg opRepe)
                pushvar(src2)                                       -- push [sliceend]
else
                leamov(edi,rep)                                     -- lea/mov edi,addr rep
                leamov(esi,src2)                                    -- lea/mov esi,addr sliceend
end if
                markConstUsed(src2)
                pc += 4
                for j=1 to noofitems do
                    k = s5[pc]
--DEVlit ditto...
                    pushvar(k)                                      -- push [idxn]..[idx1]
                    markConstUsed(k)
                    pc += 1
                end for
                leamov(eax,s5[pc])                                  -- lea/mov eax,addr res
                emitHex5jmpG(opReps)                                -- jmp opReps (actually a call, return pushed above)
                x86[backpatch] = length(x86)-backpatch
                reginfo = 0 -- all regs trashed
                pc += 1
            end if

        elsif opcode=opJnotx then       -- 35
            -- opJnotx,mergeSet,tgt,link,src,src2,invert
            --  implements: if (not) src[src2] then
            --   (avoiding any incref that might otherwise
            --    occur in tmp=src[src2]; if (~)tmp then.)
--?9/0
            if isGscan then
                pc += 7
            else
                src = s5[pc+4]
                src2 = s5[pc+5]
                invert = s5[pc+6]
                jinit(pc+1)
                markConstUsed(src2)
                getSrc()
                idx = -1
                if slen>0 then
                    getSrc2()
                    if smin2>0 and smax2<=slen then
                    -- we can inline, and assume that it is acceptable to treat
                    --  x={1,2,"fred",4} if x[3] then as true (not zero) instead
                    --  of bothering with "true/false condition must be an atom".
                    -- (see ** below if you really want that error to occur.)
--                      if sched then
--                          if mergeSet!=isOpCode then
--                              sch00n = lastJmp    -- (can schedule non-last-jumps)
--                          end if
--                      end if
                        if slroot=T_string then
                            reg = loadReg(src)                      -- mov reg,[src] (s)
                            idx = loadReg(src2)                     -- mov idx,[src2] (idx)
--                          if sched then
--                              rs = regbit[reg+1]
--                              rx = regbit[idx+1]
--                              schedule(0,rs+rx,0,pUV,1,0)
--                          end if
                            sib = #80+reg*8+idx -- 0o2ri, idx+reg*4
                            emitHex5sib(cmpb_sibd8i8,sib,-1,0)      -- cmp byte[idx+reg*4-1],0
                        elsif slroot=T_Dsq then
--                  elsif slroot=T_Dsq and not and_bits(setyp,T_sequence) then --(**)
                            reg = loadReg(src)                      -- mov reg,[src] (s)
                            idx = loadReg(src2,CLEAR)               -- mov idx,[src2] (idx)
--                          if sched then
--                              rs = regbit[reg+1]
--                              rx = regbit[idx+1]
--                              schedule(rx,0,rx,pUV,1,0)
--                          end if
                            xrm = #E0+idx -- 0o34i, shl/idx
if X64 then
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex3l(0o301,xrm,3)                  -- shl idx,3
--                          if sched then
--                              schedule(0,rs+rx,0,pUV,1,0)
--                          end if
                            sib = 0o200+reg*8+idx -- 0o2ri, idx+reg*4
                            emitHex5sib(cmpd_sibd8i8,sib,-8,0)      -- cmp dword[idx+reg*4-8],0
else
                            emitHex3l(0o301,xrm,2)                  -- shl idx,2
--                          if sched then
--                              schedule(0,rs+rx,0,pUV,1,0)
--                          end if
                            sib = 0o200+reg*8+idx -- 0o2ri, idx+reg*4
                            emitHex5sib(cmpd_sibd8i8,sib,-4,0)      -- cmp dword[idx+reg*4-4],0
end if
                        end if
                    end if
                end if
                if idx=-1 then  -- not inlined
--                  if sched then
--                      schend()    -- [esp]-15/-9
--                  end if
                    loadToReg(edi,src2)                 -- mov edi,[src2] (idx)
                    loadToReg(esi,src)                  -- mov esi,[src] (s)
--;calling convention:                              octal:         binary:          code:
--; mov edi,[p3]        ; idx                       213 075     8B 3D m32       mov edi,[m32]
--; mov esi,[p2]        ; s                         213 065     8B 35 m32       mov esi,[m32]
--; mov ecx,p3          ; var no of idx             271         B9 imm32        mov ecx,imm32
--; mov edx,p2          ; var no of s               272         BA imm32        mov edx,imm32
--; call opJnotx                                    350         E8 rel32        call rel32
--;eax=p2[p3], edx=length(p2), edi=normalised (+ve) idx, esi=raw(p2)
--31/12/2012:
                    movRegVno(ecx,src2)                 -- mov e/rcx,src2 (var no)
                    movRegVno(edx,src)                  -- mov e/rdx,src (var no)
                    emitHex5callG(opJnotx)              -- call :%opJnotx   (test s[idx])
                    reginfo = 0 -- all regs trashed
                    -- DEV (regs as above, but no way as yet to store or use such info)
                end if
                bcode = 4-invert    -- invert=jz[3], not invert=jnz[4]  (idx to ccde)
                tmpd = 0    -- always test
                -- Jnotx:
                if jend(7) then ?9/0 end if -- (no unconditional opRetf should occur)
            end if

        elsif opcode=opInt
           or opcode=opAtom
           or opcode=opStr
           or opcode=opSq
           or opcode=opObj then
            dest = s5[pc+1]
            src = s5[pc+2]
if newEmit then
            isInit = s5[pc+3]
end if
--if isGscan then
            getDest()
            getSrc()
            smin = 0
            smax = 1
            if opcode=opInt then
                if slroot=T_integer then
                    smin = 1
                elsif not and_bits(slroot,T_integer) then
                    smax = 0
                end if
            elsif opcode=opAtom then
                if slroot<=T_atom then
                    smin = 1
                elsif not and_bits(slroot,T_atom) then
                    smax = 0
                end if
            elsif opcode=opStr then
                if slroot=T_string then
                    smin = 1
                elsif not and_bits(slroot,T_string) then
                    smax = 0
                end if
            elsif opcode=opSq then
                if not and_bits(slroot,T_atom) then
                    smin = 1
                elsif not and_bits(slroot,T_sequence) then
                    smax = 0
                end if
--DEV if we know src is assigned, opObj should become 1..1
            elsif opcode=opObj then
if newEmit then
                if isInit then
                    smin = 1
                end if
end if
            end if
            slroot = T_integer
            sltype = T_integer
            storeDest()
            --else
            if not isGscan then
if not newEmit then
                isInit = s5[pc+3]
end if
--DEV not quite right...
                if (isInit or opcode=opObj) and symtab[dest][S_vtype]=T_integer then
--DEV tryme:
--          if isInit and dtype=T_integer then
                    reg = loadReg(src)                                  -- mov reg,[src]
--DEV ah, I remember now that I didn't want to use edx since it is likely to be used rsn;
--      since then opJtyp has taken the heat off, and I've invented transtmpfer: use it!
--      the acid test is a=integer(x) b=atom(y) c=string(z) if a and b and c then...
                    if reg=eax then
                        res = ecx
--                      if sched then
--                          schedule(0,0,ecxbit,pUV,1,0)
--                      end if
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(xor_ecx_ecx)                          -- xor ecx,ecx
                    else
                        res = eax
--                      if sched then
--                          schedule(0,0,eaxbit,pUV,1,0)
--                      end if
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(xor_eax_eax)                          -- xor eax,eax
                    end if
--                  if sched then
--                      rb = regbit[reg+1]
--                      rs = regbit[res+1]
--                      schedule(rb,0,0,pUV,1,0)
--                  end if
--DEV if and_bits(slroot,T_integer) then...
                    cmp_h4(reg)                                         -- cmp reg,h4
                    if opcode=opInt
                    or opcode=opObj then
--                      if sched then
--                          schedule(0,0,rs,pNP,1,0)
--                      end if
                        xrm = #C0+res   -- 0o30r
                        if opcode=opInt then
                            emitHex3(setl,xrm)                          -- setl res8
                        else
                            emitHex3(setne,xrm)
                        end if
                    else
--                      if sched then
--                          schedule(0,rb,rs,pV,1,0)    -- treat as one big instruction
--                      end if
                        sib = #83+reg*8 -- 0o2r3, ebx+reg*4
                        backpatch = 0
                        backpatch2 = 0
                        backpatch3 = 0
                        if opcode=opAtom then
--DEVBPM backpatch me: [DONE]
--                          emitHex6j(jl_rel32,11)                      -- jl l2 (result of 1) [sj OK]
                            emitHex6j(jl_rel32,0)                       -- jl l2 (result of 1) [sj OK]
                            backpatch = length(x86)
                            emitHex5sib(cmpb_sibd8i8,sib,-1,#12)        -- cmp byte[ebx+reg*4-1],0x12
--DEVBPM backpatch me: [DONE]
--                          emitHex6j(jne_rel32,1)                      -- jne l3 (result of 0) [sj OK]
                            emitHex6j(jne_rel32,0)                      -- jne l3 (result of 0) [sj OK]
                            backpatch2 = length(x86)
                        elsif opcode=opStr then
--DEVBPM backpatch me: [DONE]
--                          emitHex6j(jl_rel32,12)                      -- jl l3 (result of 0) [sj OK]
                            emitHex6j(jl_rel32,0)                       -- jl l3 (result of 0) [sj OK]
                            backpatch2 = length(x86)
                            emitHex5sib(cmpb_sibd8i8,sib,-1,#82)        -- cmp byte[ebx+reg*4-1],0x82
--DEVBPM backpatch me: [DONE]
--                          emitHex6j(jne_rel32,1)                      -- jne l3 (result of 0) [sj OK]
                            emitHex6j(jne_rel32,0)                      -- jne l3 (result of 0) [sj OK]
                            backpatch3 = length(x86)
                        elsif opcode=opSq then
--DEVBPM backpatch me: [DONE]
--                          emitHex6j(jl_rel32,12)                      -- jl l3 (result of 0) [sj OK]
                            emitHex6j(jl_rel32,0)                       -- jl l3 (result of 0) [sj OK]
                            backpatch2 = length(x86)
                            emitHex5sib(tstb_sibd8i8,sib,-1,#80)        -- test byte[ebx+reg*4-1],0x80
--DEVBPM backpatch me: [DONE]
--                          emitHex6j(jz_rel32,1)                       -- jne l3 (result of 0) [sj OK]
                            emitHex6j(jz_rel32,0)                       -- jne l3 (result of 0) [sj OK]
                            backpatch3 = length(x86)
                        end if
                                                                        -- l2:
                        if backpatch!=0 then
                            x86[backpatch] = length(x86)-backpatch
                        end if
if newEmit then
                        mod = m_add
                        reg = res
                        regimm365(1)                                    -- add res,1
else
                        emitHex1(inc_eax+res)                           -- inc res
end if
                                                                        -- l3:
                        if backpatch2!=0 then
                            x86[backpatch2] = length(x86)-backpatch2
                        end if
                        if backpatch3!=0 then
                            x86[backpatch3] = length(x86)-backpatch3
                        end if
                    end if
                    storeReg(res,dest,1,1)                              -- mov [dest],res
                else --opINSPO with non-int result or non-init param:
--                  if sched then
--                      sch00n = schoon
--                      schedule(0,0,edibit,pUV,0,0)
--                  end if
                    leamov(edi,dest)                                -- lea edi,[dest]/mov edi,dest (addr result)
--                  if sched then
--                      schend()    -- [esp-9]
--                  end if
                    loadToReg(eax,src)                              -- mov eax,[src] (var ref)
--?                        ltype = opTyp0[ltype] (NO)
                    if opcode!=opObj then
                        movRegVno(ecx,src)                          -- mov e/rcx,varno of src
                    end if
                    emitHex5callG(opcode)                           -- call opInt/Atom/Sq/Str/Obj
                    -- all regs trashed, unless result is integer,
                    --  in which case eax is still [src] and only edx was damaged
                    --  (but we never care about edx anyway)
                    if symtab[dest][S_vtype]=T_integer then
--DEV tryme:
--              if dtype=T_integer then
                        clearReg(edi)   -- (as set to result addr just above the call)
                        clearMem(dest)  -- 3/4/9
                        storeReg(eax,src,0,1) -- just record that eax==[src]
                                          -- nb: [dest] is not in a register at all...
                        clearReg(ecx)
                    else
                        reginfo = 0
                    end if
                end if
            end if
            pc += 4

        elsif opcode=opScmp then        -- 42
            dest = s5[pc+1]
--if isGscan then
            getDest()
            --DEV if two integers with non-overlapping ranges,
            --      then we could fix result as -1 or 1 accordingly;
            --    else ditto if one if sequence and other atom.
            --      (see also SetCC() for ideas if nowt else)
            smin = -1
            smax = 1
            slroot = T_integer
            sltype = T_integer
            storeDest()
            --else
            if not isGscan then
                src = s5[pc+2]
                src2 = s5[pc+3]
                markConstUseds({src,src2})
--              if sched then
--                  sch00n = schoon
--                  schedule(0,0,edxbit,pUV,0,0)
--              end if
                leamov(edx,dest)                                -- lea edx,[dest]/mov edx,addr dest (result)
--              if sched then
--                  schend()    -- [esp]-9/-14
--              end if
                loadToReg(edi,src2)                             -- mov edi,[src2]
                loadToReg(eax,src)                              -- mov eax,[src]
                emitHex5callG(opScmp)                           -- call opScmp
                --; NB: src/2 obtained from [esp]-9/-14 on error
                -- all regs trashed, unless result is integer, in eax (and [dest])
                reginfo = 0
                if symtab[dest][S_vtype]=T_integer then
--DEV tryme:
--          if dtype=T_integer then
                    storeReg(eax,dest,0,0)  -- just record that eax==[dest]
                end if
            end if
            pc += 4

        elsif opcode=opGetc then        -- 54
            dest = s5[pc+1]
            getDest()
            smin=-1
            smax = 255
            slroot = T_integer
            sltype = slroot
            storeDest()
            if not isGscan then
                src = s5[pc+2]  -- fileno/name
                markConstUsed(src)
                leamov(edi,dest)                                -- lea edi,[dest]
                getSrc()
                if slroot=T_integer and smin=smax then
                    if smin=0 then
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(xor_eax_eax)                  -- xor eax,eax
                    else
                        movRegImm32(eax,smin)                   -- mov eax,imm32
                    end if
                else
                    loadToReg(eax,src)                          -- mov eax,[src] (fileno)
                end if
                emitHex5callG(opcode)                           -- call :%opGetc
                reginfo = 0 -- all regs trashed
                if slroot=T_integer then
                    storeReg(ecx,dest,0,0)  -- just record that ecx==[dest]
                end if
            end if
            pc += 3

        elsif opcode=opGets then        -- 117
            dest = s5[pc+1]
            getDest()
            slroot = T_integer+T_string
            setyp = T_integer
            slen = -2
            sltype = slroot
            storeDest()
            if not isGscan then
                src = s5[pc+2]   -- fileno
                isInit = s5[pc+3]
                markConstUsed(src)
                leamov(edi,dest)                        -- lea edi,[dest]
                loadToReg(eax,src)                      -- mov eax,[src]
                emitHex5callG(opcode)                   -- call :%opGets
                reginfo = 0 -- all regs trashed
            end if
            pc += 4

--      elsif opcode=opWhere            -- 58
        elsif opcode=opWhere then
--         or opcode=opChDir            -- 62
--         or opcode=op32toA            -- 98
--         or opcode=op64toA then       -- 99
            dest = s5[pc+1]
--if isGscan then
            getDest()
--          if opcode=opWhere then      -- 58
                smin = 0
                smax = MAXINT
                -- reasonably safe to assume integer result [?]
                -- (T_atom might be better)
                slroot = T_integer
--          elsif opcode=opChDir then       -- 62
--              smin = 0
--              smax = 1
--              slroot = T_integer
--          elsif opcode=op32toA            -- 98
--             or opcode=op64toA then       -- 99
--              slroot = T_atom
--          end if
            sltype = slroot
            storeDest()
            --          pc += 4
            --else
            if not isGscan then
                src = s5[pc+2]   -- path/seq4/8
                isInit = s5[pc+3]
--          pc += 4
                markConstUsed(src)
--              if opcode!=opWhere then ?9/0 end if
                leamov(edi,dest)                                -- lea edi,[dest]
                loadToReg(eax,src)                              -- mov eax,[src]
                emitHex5callG(opcode)                           -- call opXxx
                reginfo = 0 -- all regs trashed
--              if opcode=opWhere and symtab[dest][S_vtype]=T_integer then
                if symtab[dest][S_vtype]=T_integer then
--DEV tryme:
--              if dtype=T_integer then
                    storeReg(eax,dest,0,0)  -- just record that eax==[dest]
                end if
            end if
            pc += 4

--      elsif (opcode>=opCos and opcode<=opSqrt)    -- 92/93/94/95/96/97
        elsif opcode=opCos
           or opcode=opSin
           or opcode=opTan
           or opcode=opArcTan
           or opcode=opLog
           or opcode=opSqrt then
--         or opcode=opAto32            -- 110
--         or opcode=opAto64 then       -- 111
            dest = s5[pc+1]
--if isGscan then
            getDest()
--          if opcode>=opCos and opcode<=opSqrt then -- 92/93/94/95/96/97
                slroot = T_atom
--          else
----            elsif opcode=opAto32            -- 110
----               or opcode=opAto64 then       -- 111
--              setyp = T_integer
----                slroot = T_Dsq
--              slroot = T_string   -- 10/01/10
--              if opcode=opAto32 then
--                  slen = 4
--              else
--                  slen = 8
--              end if
--          end if
            sltype = slroot
            storeDest()
            --else
            if not isGscan then
                src = s5[pc+2]
                markConstUseds({dest,src})
                leamov(edi,dest)                                -- lea edi,[dest]
                loadToReg(eax,src)                              -- mov eax,[src]
                emitHex5callG(opcode)                           -- call opXxx
                reginfo = 0
            end if
            pc += 3

        elsif opcode=opPeek
           or opcode=opPeek1s
           or opcode=opPeek1u
           or opcode=opPeek2s
           or opcode=opPeek2u
           or opcode=opPeek4s
           or opcode=opPeek4u
           or opcode=opPeek8s
           or opcode=opPeek8u
           or opcode=opPeekNS then
            dest = s5[pc+1]
            src = s5[pc+2]
--          pc += 3
--if isGscan then
--DEV UNTESTED:
--  opPeeki = 63,       -- a = peek(b) where a is integer
--  opPeek = 114,       -- a = peek(b)      -- (returns integer if b is atom, string if b is sequence)
--  opPeek4s = 115,     -- a = peek4s(b)    -- (returns atom if b is atom, sequence if b is sequence)
--  opPeek4u = 116,     -- a = peek4u(b)    -- (returns atom if b is atom, sequence if b is sequence)
            getDest()
            getSrc()
            sltype = 0
--DEV or opPeek1?
            if opcode=opPeek then
                if and_bits(slroot,T_sequence) then
                    sltype = T_string
                    setyp = T_integer
                    slen = -2
                end if
                if and_bits(slroot,T_atom) then
                    sltype += T_integer
                    smin = 0
                    smax = 255
                end if
            else
                if and_bits(slroot,T_sequence) then
                    sltype = T_Dsq
--DEV T_integer for opPeek2x
                    setyp = T_atom
                    slen = -2
                end if
                if and_bits(slroot,T_atom) then
--""
                    sltype += T_atom
                end if
            end if
--13/12/16 (seems unimportant)
--          if sltype=0 then ?9/0 end if
--DEV/temp:
--if sltype=0 then
--printf(1,"sltype=0: line 13961 pilx86.e, (emitline=%d, %s)\n",{emitline,filenames[symtab[vi][S_FPno]][2]})
--sltype = T_object
--end if

            slroot = sltype
            storeDest()
            --else
            if not isGscan then
                --calling convention:
                --  lea edi,[dest]  ; target addr
                --  mov esi,[src]   ; addr or {addr,len} (opUnassigned)
                --  mov ecx,???
                --  mov edx,???
                --  call opPeek[i|4(u|s)]
                leamov(edi,dest)                        -- lea edi,[dest]
                loadToReg(esi,src)                      -- mov esi,[src]
                if opcode=opPeek1s
                or opcode=opPeek2s
                or opcode=opPeek4s
                or opcode=opPeek8s then
                    movRegImm32(ecx,-1)                 -- mov ecx,-1
                elsif opcode=opPeekNS then
                    src = s5[pc+4]                      -- size
                    getSrc()
                    if slroot=T_integer and smin=smax then
                        if and_bits(state1,K_rtn) then ?9/0 end if
                        if smin then
                            movRegImm32(ecx,smin)       -- mov ecx,imm32
                        else
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(xor_ecx_ecx)      -- xor ecx,ecx
                        end if
--clearReg(ecx)
                    else
                        loadToReg(ecx,src)              -- mov ecx,[src]
                    end if
                else
                    if X64 then
                        emitHex1(#48)
                    end if
                    emitHex2s(xor_ecx_ecx)              -- xor ecx,ecx
                end if
                if opcode=opPeek
                or opcode=opPeek1s
                or opcode=opPeek1u then
                    movRegImm32(edx,1)                  -- mov edx,1
                elsif opcode=opPeek2s
                   or opcode=opPeek2u then
                    movRegImm32(edx,2)                  -- mov edx,2
                elsif opcode=opPeek4s
                   or opcode=opPeek4u then
                    movRegImm32(edx,4)                  -- mov edx,4
                elsif opcode=opPeek8s
                   or opcode=opPeek8u then
                    movRegImm32(edx,8)                  -- mov edx,8
                elsif opcode=opPeekNS then
                    src = s5[pc+3]                      -- size
                    getSrc()
                    if slroot=T_integer and smin=smax then
                        if and_bits(state1,K_rtn) then ?9/0 end if
                        if smin then
                            movRegImm32(edx,smin)       -- mov edx,imm32
                        else
                            if X64 then
                                emitHex1(#48)
                            end if
                            emitHex2s(xor_edx_edx)      -- xor edx,edx
                        end if
--clearReg(edx)
                    else
--                      loadToReg(edx,src)              -- mov edx,[src]
                        loadMem(edx,src)                -- mov edx,[src]
                    end if
                else
                    ?9/0
                end if
                emitHex5callG(opcode)                   -- call opXxxx
                reginfo = 0
            end if
            if opcode=opPeekNS then
                pc += 5
            else
                pc += 3
            end if

        elsif opcode=opPeeki then       -- 63
            dest = s5[pc+1]
--if isGscan then
            getDest()
            smin = 0
            smax = 255
            slroot = T_integer
            sltype = T_integer
            storeDest()
            --else
            if not isGscan then
                src = s5[pc+2]
                isInit = s5[pc+3]
                markConstUsed(dest)
--;calling convention:                              octal:         binary:          code:
--; mov edi,[src]   ; mem addr                      213 075     8B 3D C8194000      MOV EDI,DWORD PTR DS:[4019C8]
--; call opPeeki                                    350         E8 <relative dword>
--; mov [dest],eax                                  243         A3 C4194000     MOV DWORD PTR DS:[4019C4],EAX

-- 16/11/08: Invalid use of S_Init (may be used on S_Const, but if you want
--  BUGFIX   to use it on S_Gvar/S_Tvar then you must include it in the il
--   ====    stream, as per eg opMove/isInit). Secondly, we should check
--    ==     that src is an atom (needs [S_ltype] added to the ilstream).
--           Also, fistp does not store unsigned 32-bit values (ie above
--           #80000000) as expected/needed.

--          if symtab[src][S_Init] then
--DEV (12/01/17) inlining disabled on x64, see below
--              if isInit then  -- src is initialised atom
                if isInit and X64=0 then    -- src is initialised atom
                    -- (we already know tgt is an integer, since opPeeki was emitted)
                    reg = loadReg(src,CLEAR)                    -- mov reg,[src]
                    if reg=eax then
                        res = ecx
--                      if sched then
--                          schedule(0,0,ecxbit,pUV,1,0)
--                      end if
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(xor_ecx_ecx)                  -- xor ecx,ecx
                    else
                        res = eax
--                      if sched then
--                          schedule(0,0,eaxbit,pUV,1,0)
--                      end if
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(xor_eax_eax)                  -- xor eax,eax
                    end if
--                  if sched then
--                      schedule(regbit[reg+1],0,0,pUV,1,0)
--                  end if
                    cmp_h4(reg)                                 -- cmp reg,h4
--                  if sched then
--                      schedule(regbit[reg+1],0,regbit[res+1],pV,1,0)  -- treat as one big instruction
--                  end if
--DEVBPM backpatch me: [DONE]
--                  emitHex6j(jl_rel32,13)                      -- jl l1 [sj OK]
                    emitHex6j(jle_rel32,0)                      -- jl @f [sj OK]
                    backpatch = length(x86)
                    xrm = #83+reg*8 -- 0o2r3, ebx+reg*4
if X64 then
--                      if p1size=8 then
--                          s5 &= 0o335
--                          mod = 0
--                      elsif p1size=10 then
--                          s5 &= 0o333
--                          mod = 5
--   ?9/0 
--printf(1,"pilx86.e: check list.asm for fld tbyte, emitline=%d\n",{emitline}) (done)
                    emitHex3l(0o333,0o054,xrm)                  -- fld tbyte[ebx+reg*4]
--DEV (12/01/17) would need something like the following, so disabled inlining on x64 instead..
--               (plus, comment below suggests this (opPeeki) is no longer used anyway...)
--/*
    mov r15,h4

    -- if uint>#7FFF... then uint-=#1_0000...
    push r15            -- #4000_0000_0000_0000
    fild qword[rsp]
    fadd st0,st0        -- #8000_0000_0000_0000
    fld st1
    fcomp
    fnstsw ax
    sahf
    jb @f
        fadd st0,st0    -- #1_0000_0000_0000_0000
        fsub st1,st0
  @@:
    fstp st0            -- discard

--*/
else
--                  emitHex3l(#DD,#04,xrm)                      -- fld qword[ebx+reg*4]
                    emitHex3l(0o335,0o004,xrm)                  -- fld qword[ebx+reg*4]
end if
                    emitHex3(sub_esp_imm8,8)                    -- sub esp,8
                    -- (note that fistp dword stores eg 2155085935 as #80000000, not #8074006F)
                    emitHex3l(#DF,#3C,#24)                      -- fistp qword[esp]
                    emitHex1(pop_eax+reg)                       -- pop reg
                    if X64=0 then
                        emitHex3(add_esp_imm8,4)                -- add esp,4
                    end if
                    x86[backpatch] = length(x86)-backpatch      -- @@:
                    xrm = res*8+reg -- 0o0sr
                    emitHexx2(mov_byte,xrm)                     -- mov res8,[reg]
                    storeReg(res,dest,1,1)                      -- mov [dest],res
                else    -- not S_Init (the b in a=peek(b))
--                  if sched then
--                      schend()    -- [esp]-9
--                  end if
--DEV do we not need a var no? (or are we using opUnassigned and tce is misleading and this code is dead?)
                    loadToReg(esi,src)                          -- mov esi,[src]
--DEV calls opAddiii??? (simply stopped using this)
                    emitHex5callG(opPeeki)                      -- call opPeeki -- flt->eax/unassigned/tce
                    -- result in eax, damages edx/edi, ecx/esi/ebx unaltered
--                  if sched then
--                      -- needed for [esp]+1 error handling:
--                      sch00n = schoon
--                  end if
                    storeReg(eax,dest,1,1)                      -- mov [dest],eax
--                  if sched then
--                      sch00n = 0  -- (just the above line)
--                  end if
                    clearReg(edi)
                end if
            end if
            pc += 4

        elsif opcode=opPoke
           or opcode=opPoke1
           or opcode=opPoke2
           or opcode=opPoke4
           or opcode=opPoke8
           or opcode=opPokeN
           or opcode=opPosition then

--          if isGscan then
            if not isGscan then

                --NB: poke/poke4/cproc _could_ crap on anything; compiler assumes not...
                --    To avoid any problems poking about with object X, you may need
                --    to X=1.5 X="string" X={-1}...
                --DEV doc suggestion:
                --  WARNING: The compiler makes huge assumptions about what the hll
                --           code does to hll variables, in fact as many assumptions
                --           as it possibly can. Consider the following code:
                --              object P
                --              sequence Q
                --                  P={1,2,3}
                --                  #ilASM{xor eax,eax
                --                         mov [P],eax }
                --                  Q=P
                --           Since the compiler "knows" P is assigned to a sequence
                --           it 1) tries to incref it during Q=P by updating memory
                --           location -4 (fatal crash), and 2) does not typecheck Q
                --           after the assigment, and 3) the above would also cause
                --           a memory leak btw, but ignore that for the moment, and
                --           4) were Q an integer it would in fact fail to compile.
--DEV I think it does this now... (nope.)
                --           One attempt to prevent this behaviour might be to "wipe"
                --           all type/init/etc on any var referenced in any way by an
                --           #ilASM construct, but that would probably counteract any
                --           savings... See also pdiag.e, which uses peek/4/s/u to
                --           fetch data; were you to employ similar tricks but with
                --           poke/4 to change hll vars, similar problems might arise.
                --           The "proper" solution is to keep your hll and low-level
                --           variables separate and fully managed, eg:
                --           atom Q, P
                --              Q = <expr>
                --              #ilASM{ mov eax,[Q] -- (and maybe fld qword[eax*4])
                --                      mov [P], eax
                --                      -- let's do (eg) some 32 bit maths...
                --                      xor eax,eax
                --                      mov [P],eax }
                --           You must zeroise any hll vars used to hold 32-bit
                --            or non-refcounted values to prevent later mishap.
                --
--              pc += 3
--          else
                src = s5[pc+1]
                src2 = s5[pc+2]
--              pc += 3
                markConstUseds({src,src2})
if opcode=opPoke
or opcode=opPoke1
or opcode=opPoke2
or opcode=opPoke4
or opcode=opPoke8
or opcode=opPokeN then
--;  mov eax,[p2] ; object to poke                      241         A1 m32          mov eax,[m32]
--;  mov edi,[p1] ; addr                                213 075     8B 3D m32       mov edi,[m32]
--;  call opPoke    ; poke(edi,eax)                     350         E8 rel32        call rel32
--; nb p1/p2 obtained from [esp]-9/-15 on error
--              if sched then
--                  sch00n = schoon
--                  schedule(0,0,eaxbit,pUV,0,src2)
--              end if
--DEV constant, transtmpfer?
                loadToReg(eax,src2)                     -- mov eax,[src2]
--              if sched then
--                  schedule(0,0,edibit,pUV,0,src)
--              end if
                loadToReg(edi,src)                      -- mov edi,[src]
--              if sched then
--                  schend()
--              end if
                if opcode=opPoke1 then
                    movRegImm32(ecx,1)                  -- mov ecx,1
                elsif opcode=opPoke2 then
                    movRegImm32(ecx,2)                  -- mov ecx,2
                elsif opcode=opPoke4 then
                    movRegImm32(ecx,4)                  -- mov ecx,4
                elsif opcode=opPoke8 then
                    movRegImm32(ecx,8)                  -- mov ecx,8
                    if X64 then
                        emitHex1(#48)
                    end if
                    emitHex2s(xor_edx_edx)              -- xor edx,edx
                elsif opcode=opPokeN then
--                  ?9/0
                    src = s5[pc+3]
                    getSrc()
                    if slroot=T_integer and smin=smax then
                        movRegImm32(ecx,smin)           -- mov eax,imm32
--                      clearReg(ecx)
                    else
                        loadToReg(ecx,src)              -- mov ecx,[src]
                    end if
                    if X64 then
                        emitHex1(#48)
                    end if
                    emitHex2s(xor_edx_edx)              -- xor edx,edx
                else
                    ?9/0
                end if
                emitHex5callG(opcode)                   -- call opXxxx
else -- opPosition
            -- calling convention
            --  mov eax,[line]      -- line (opUnassigned, integer)
            --  mov ecx,[col]       -- col (opUnassigned, integer)
            --  call :%opPosition   -- position(eax,ecx)
--DEV smin/smax...
                loadToReg(eax,src)                      -- mov eax,[src]
                loadToReg(ecx,src2)                     -- mov ecx,[src2]
                emitHex5callG(opcode)                   -- call opXxxx
end if
                reginfo = 0 -- all regs trashed
            end if
            if opcode=opPokeN then
                pc += 4
            else
                pc += 3
            end if


--/*
        elsif opcode=opPokeN then
            -- opPokeN,size,addr,value
            --  size is 1/2/4/8
if not newEmit then ?9/0 end if
            if isGscan then
                pc += 4
            else
                size = s5[pc+1]
                src = s5[pc+2]
                src2 = s5[pc+3]
                pc += 4
                markConstUseds({src,src2})
                loadToReg(eax,src2)                     -- mov eax,[src2]
                loadToReg(edi,src)                      -- mov edi,[src]
                movRegImm32(ecx,size)                   -- mov ecx,1/2/4/8
                if size=8 then
                    if X64 then
                        emitHex1(#48)
                    end if
                    emitHex2s(xor_edx_edx)              -- xor edx,edx
                end if
                emitHex5callG(opcode)                   -- call opPokeN
                reginfo = 0 -- all regs trashed
            end if
        elsif opcode=opPokeNS then  -- DEV pointless?
            -- opPokeN,size,base,offset,value
            --  size is 1/2/4/8
if not newEmit then ?9/0 end if
            if isGscan then
                pc += 5
            else
                size = s5[pc+1]
                src = s5[pc+2]
                offset = s5[pc+3]
                src2 = s5[pc+4]
                pc += 5
                markConstUseds({src,offset,src2})
                loadToReg(esi,offset)
                loadToReg(eax,src3)                     -- mov eax,[src3]
                loadToReg(edi,src)                      -- mov edi,[src]
                movRegImm32(ecx,size)                   -- mov ecx,1/2/4/8
                if size=8 then
                    if X64 then
                        emitHex1(#48)
                    end if
                    emitHex2s(xor_edx_edx)              -- xor edx,edx
                end if
                emitHex5callG(opcode)                   -- call opPokeNS
                reginfo = 0 -- all regs trashed
            end if
--*/
        elsif opcode=opWaitKey          -- 56   -- wait_key()
           or opcode=opGetKey           -- 55   -- get_key()
           or opcode=opTime             -- 90   -- time()
           or opcode=opInstance         -- 103  -- instance()
           or opcode=opCurrDir          -- 105  -- current_dir()
           or opcode=opGetPos           -- 113  -- get_position()
           or opcode=opClose            -- 133
           or opcode=opTxtClr           -- 137
           or opcode=opBkClr            -- 138
           or opcode=opFlush then       -- 133
--if isGscan then
            if opcode=opClose           -- 133
            or opcode=opTxtClr          -- 137
            or opcode=opBkClr           -- 138
            or opcode=opFlush then      -- 133
                -- do nowt, except the pc += 2 below
            else
                dest = s5[pc+1]
                getDest()
                if opcode=opCurrDir then
                    slroot = T_string
                    setyp = T_integer
                    slen = -2
                elsif opcode=opInstance
                   or opcode=opTime then
                    slroot = T_atom
                elsif opcode=opGetPos then
                    slroot = T_Dsq
                    setyp = T_integer
                    slen = 2
                else
--              elsif opcode=opWaitKey
--                 or opcode=opGetKey then
                    slroot = T_integer
                    if opcode=opWaitKey then
                        smin = 0
                    elsif opcode=opGetKey then
                        smin = -1
                    else
                        ?9/0    -- sanity check
                    end if
                    smax = #100FF   -- in practice #200 (512) is probably the limit
                end if
                sltype = slroot
                storeDest()
            end if
            --else
            if not isGscan then
                dest = s5[pc+1] -- (actually src for last 4)
                markConstUsed(dest)
                if opcode=opTime
                or opcode=opGetKey
                or opcode=opInstance
                or opcode=opWaitKey
                or opcode=opGetPos then
                    leamov(edi,dest)                                -- lea edi,[dest]
                    emitHex5callG(opcode)                           -- call opXxxx
                elsif opcode=opFlush
                   or opcode=opClose
                   or opcode=opTxtClr
                   or opcode=opBkClr then
                    src = dest
                    loadToReg(eax,src)                              -- mov eax,[src]
                    emitHex5callG(opcode)                           -- call opXxxx
                else
printf(1,"warning: emitHex5call(%d=%s) skipped for newEmit, pilx86.e line 15009\n",{opcode,opNames[opcode]})
                end if
                reginfo = 0 -- all regs trashed
            end if
            pc += 2

        elsif opcode=opLock then        -- 59
            dest = s5[pc+1]
--if isGscan then
            getDest()
            slroot = T_integer
            sltype = T_integer
            smin = 0
            smax = 1
            storeDest()
            --else
            if not isGscan then
                src = s5[pc+2]  -- fileno
                src2 = s5[pc+3] -- locktype
                p4 = s5[pc+4]   -- byterange
                markConstUseds({src2,p4})
                leamov(edi,dest)                                -- lea edi,[dest]/mov edi,addr dest
                loadToReg(eax,src)                              -- mov eax,[src] (fn)
                getSrc2()
                if slroot2=T_integer and smin2=smax2 then
                    if smin2=0 then
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(xor_ecx_ecx)                  -- xor ecx,ecx
                    else
                        movRegImm32(ecx,smin2)                  -- mov ecx,imm32
                    end if
                else
                    loadToReg(ecx,src2)                         -- mov eax,[src] (locktype)
                end if
                loadToReg(esi,p4)                               -- mov esi,[p4] (byterange)
                emitHex5callG(opLock)                           -- call :%opLock
            -- calling convention
            --  lea edi,[res]       -- result location
            --  mov eax,[fn]        -- file number (opUnassigned, integer)
            --  mov ecx,[locktype]  -- lock type (opUnassigned, integer)
            --  mov esi,[byterange] -- byte range (opUnassigned, sequence)
            --  call :%opLock       -- [edi]:=lock_file(eax,ecx,esi)
                reginfo = 0
--              if rootType(symtab[dest][S_vtype])=T_integer then
--DEV tryme:
--              if dtype=T_integer then
                if vroot=T_integer then
                    storeReg(eax,dest,0,0)  -- just record that eax==[dest]
                end if
            end if
            pc += 5

        elsif opcode=opUnLock then      -- 60
            if not isGscan then
                src = s5[pc+1]      -- fileno
                src2 = s5[pc+2]     -- byterange
                loadToReg(eax,src)                              -- mov eax,[src] (fn)
                loadToReg(esi,src2)                             -- mov esi,[p4] (byterange)
                emitHex5callG(opUnLock)                         -- call :%opUnLock
            --  mov eax,[fn]        -- file number (opUnassigned, integer)
            --  mov esi,[byterange] -- byte range (opUnassigned, sequence)
            --  call :%opUnLock     -- unlock_file(eax,esi)
                reginfo = 0
            end if
            pc += 3

        elsif opcode=opClrScrn
           or opcode=opFreeCons then -- 124
            if not isGscan then
                emitHex5callG(opcode)                           -- call :%opXxxx
                reginfo = 0
            end if
            pc += 1

        elsif opcode=opGchk then
            if not isGscan then
--trace(1)
                --  s5 &= {opGchk,N,typ,min,max,etyp,slen,tokline,tokcol,fileno}
                --   (only emitted when bind=1, ie multiple passes, btw)
                p1 = s5[pc+1]   --DEV should be N really
                symk = symtab[p1]
                gi = symk[S_gInfo]
                symk = s5[pc+2..pc+6] -- typ..slen
                if not equal(gi,symk) then
                    tokline = s5[pc+7]
                    tokcol = s5[pc+8]
                    fileno = s5[pc+9]
--DEV probably not needed:
                    text = allfiles[fileno]
                    if sequence(gi) then
--DEV added 11/12/2011, should (perhaps) be a temporary measure (newEBP) {warning not error, that is)
--DEV output p1[2] as eg 0b1101, MIN/MAX etc...
                        slen = s5[pc+6]
--27/4/21
--                      if slen<0 and gi[gLen]<0 and gi[1..gEtyp]=symk[1..gEtyp] then
                        if slen<0 and gi[gLen]<0 and length(symk)>=gEtyp and gi[1..gEtyp]=symk[1..gEtyp] then
                            Warn(sprintf("gInfo is {%d,%d,%d,%d,%d,%d}",p1&gi),tokline,tokcol,0)
                        else
                            Abort(sprintf("gInfo is {%d,%d,%d,%d,%d,%d}",p1&gi))
                        end if
                    else
                        Abort("no gInfo for this variable...")
                    end if
                end if
            end if
            opcode = lastop
            pc += 10

        elsif opcode=opLchk then

            --  s5 &= {opLchk,N,typ,tokline,tokcol,fileno} (from #istype{N,typ})

            p1 = s5[pc+1]   --DEV should be N really
            symk = symtab[p1]
            mtype = s5[pc+2]
ltDiagMsg(sprintf("opLchk,N=%d,typ=%d,line %d\n",{p1,mtype,s5[pc+3]}))
            ltype = symk[S_ltype]
            if ltype!=mtype then
                tokline = s5[pc+3]
                tokcol = s5[pc+4]
                fileno = s5[pc+5]
                text = allfiles[fileno]
                b1 = "0000"
                b2 = "0000"
                k = 8
                for i=1 to 4 do
                    if and_bits(ltype,k) then b1[i] = '1' end if
                    if and_bits(mtype,k) then b2[i] = '1' end if
                    k = floor(k/2)
                end for
                if ltype>15 then
                    b1 = getname(symtab[ltype][S_Name],-2)
                else
                    b1 = "0b"&b1
                    if find(ltype,typeINSPO) then
                        b1 = b1 & '(' & getname(symtab[ltype][S_Name],-2) & ')'
                    end if
                end if
                if mtype>15 then
                    b2 = getname(symtab[mtype][S_Name],-2)
                else
                    b2 = "0b"&b2
                    if find(mtype,typeINSPO) then
                        b2 = b2 & '(' & getname(symtab[mtype][S_Name],-2) & ')'
                    end if
                end if
                b2 &= sprintf(" (pilx86, pc=%d, vno=%d)",{pc,p1})
                puts(1,"\n**NB** (reminder to self) Do *NOT* assume this is an error;\n\n")
                -- It may simply be the compiler has just gotten smarter....
                Abort("fail: ltype is "&b1&", not "&b2)
            end if
            opcode = lastop
            pc += 6

        elsif opcode=opLogPos then
--DEV 28/4/13:
--          ?9/0
            if not isGscan then
                lblidx = s5[pc+1]
                flags = glblused[lblidx]
                -- erm, can't actually happen...
                if not and_bits(flags,G_declared) then ?9/0 end if
if not repl then
                if and_bits(flags,G_set) then ?9/0 end if
end if
--?glblused
                glblused[lblidx] = flags+G_set
--?glblused
                glboffset[lblidx] = length(x86)
--?glboffset
            end if
            pc += 2
        elsif opcode=opInit then
            --DEV... call any :>init style labels which have been defined
if not newEmit then ?9/0 end if
            if not isGscan then
--puts(1,"opInit in pilx86.e doing nowt (line 13215)\n")
--DEV try removing this:
                if X64 then
--                  sub rsp,8   -- align stack
--                  emitHex1(push_eax)                  -- align stack
--                  sub_rsp_imm8    = {#48,#83,#EC},    -- #48 0o203 0o354 imm8         -- sub Rsp,imm8
--                  if not sched then
                    if lastline!=emitline then lineinfo() end if
--                  end if
--                  x86 &= {#48,#83,#EC,#08}            -- align stack (sub rsp,8)
--                  x86 &= {#48,#83,#EC,#28}            -- align stack (sub rsp,8*5)
--                  emitHex4l(integer op1, integer op2, integer op3, integer op4)
                end if
                integer exportaddr = length(x86)
--temp!:
--emitHex1(0o314) -- int3
--emitHex1(pushad) -- pushad
--if length(glblused) then  -- (added 10/11/16 [no help])
--emitHex1(push_esi) -- push esi
                integer pushesi = 0

                for lblidx=1 to length(glblused) do
                    if and_bits(glblused[lblidx],G_init) then
--                      x86 &= {call_rel32,isJmpG,0,0,lblidx}
                        if PE=0 
                        or X64=0 
                        or glblname[lblidx]!=">initFEH" then
                            -- (initFEH is not used on PE64, and
                            --  has been subverted for exch64)
                            if DLL and pushesi=0 then
                                emitHex1(push_esi) -- push esi (needs to be saved for dlls!)
                                pushesi = 1
                            end if
                            emitHex5callG(0,lblidx)
                        end if
--                      tt[aatidx[opInit]+EQ] = lblidx
--                      emitHex5callG(opInit)
                    end if
                end for
--emitHex1(popad) -- popad
--end if
                if DLL then
--DEV if no DllMain, we could just emit mov eax,1 ret (and reset exportaddr)... [DONE]
--temp!
--emitHex1(0o314) -- int3
                    if pushesi then
                        -- (note that :%cbhandler preserves esi)
                        emitHex1(pop_esi) -- pop esi 
                    end if

                    if length(exports)=0
                    or symtab[exports[1]][S_Name]!=T_DLLMAIN then
                        movRegImm32(eax,1)  -- mov eax,1
                        emitHex1(#C3)       -- ret
                        exportaddr = length(x86)
                    end if
                    -- emit the static callbacks.
                    -- make sure VM/pcfunc.e has been included...
                    -- check vi==T_maintls and s5=={opInit}
                    for i=1 to length(exports) do
                        -- save address against exports[i] 
                        -- exportaddrs[i] = length(x86) (see glboffset)
                        exportaddrs[i] = exportaddr
                        integer ei = exports[i]
-- push rtnid!
--                      movRegImm32(eax,ei)                 -- mov eax,imm32
--              mov byte[edi],0o150     -- push (#68)
--if X64 then ?9/0 end if
--                      emitHex5w(push_imm32,ei)            -- push imm32
                        emitHex5vno(push_imm32,ei)          -- push symtab index
                        -- emit call :%cbhandler[64]
--                      emitHex5callG(0,symtab[ei][S_Name])         -- call :%cbhandler[64]
                        emitHex5callG(opCbHandler)          -- call :%cbhandler
                        -- emit ret [imm16]
--DEV and X64=0??
                        if PE then  -- Windows
                            integer noofparams = symtab[ei][S_ParmN]
                            emitHex1(#C2)   -- ret imm16 (STDCALL)
                            emitHexWord(noofparams*4)
                        else        -- Linux
                            emitHex1(#C3)   -- ret (CDECL)
                        end if
                        exportaddr = length(x86)
                    end for
--                  ?9/0
                end if
            end if
            pc += 1
--      elsif opcode=opTestN
--         or opcode=opTestM then
----puts(1,"opTestN in pilx86.e doing nowt (line 13219)\n")
--          if not isGscan then
---- (DEV)      lblidx = tt[aatidx[opcode]+EQ]
--              lblidx = s5[pc+1]
--              if lblidx=0 then ?9/0 end if
----                x86 &= {call_rel32,isJmpG,0,0,lblidx}
--?9/0
----                emitHex5callG(0,lblidx)
--              reginfo = 0 -- all regs trashed
--          end if
--          pc += 2
        elsif opcode=opPlatform
           or opcode=opMachine
           or opcode=opVersion then
            ?9/0 -- should have been resolved by pmain.e
        elsif opcode=opAbort then
            if not isGscan then
                src = s5[pc+1]      -- error code
                markConstUsed(src)
--              loadToReg(ecx,src)
                getSrc()
                if slroot=T_integer and smin=smax then
                    if smin=0 then
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(xor_eax_eax)              -- xor eax,eax
                    else
                        movRegImm32(eax,smin)               -- mov eax,imm32
                    end if
                else
                    loadToReg(eax,src)                      -- mov eax,[src] (error_code)
                end if
                emitHex5callG(opAbort)                      -- call :%opAbort
                reginfo = 0 -- all regs trashed
            end if
            pc += 2
            if pc>length(s5) then exit end if

        elsif opcode=opInitCS then
            dest = s5[pc+1]
            getDest()
            slroot = T_integer
            sltype = slroot
            smin = 0
            smax = MAXINT
            storeDest()
            if not isGscan then
                markConstUsed(dest)
                leamov(edi,dest)                                -- lea edi,[dest]
                emitHex5callG(opcode)                           -- call opInitCS
                reginfo = 0 -- all regs trashed
            end if
            pc += 2
        elsif opcode=opDeleteCS then
            if not isGscan then
                src = s5[pc+1]
                getSrc()
                markConstUsed(src)
                loadToReg(eax,src)                              -- mov eax,[src]
                emitHex5callG(opcode)                           -- call opDeleteCS
                reginfo = 0 -- all regs trashed
            end if
            pc += 2
        elsif opcode=opEnterCS
           or opcode=opLeaveCS then
            if not isGscan then
                src = s5[pc+1]
                getSrc()
                markConstUsed(src)
                if slroot=T_integer and smin=smax then
                    if smin=0 then
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(xor_ecx_ecx)                  -- xor ecx,ecx
                    else
                        ?9/0 -- DEV better message?
--                      movRegImm32(ecx,smin)                   -- mov ecx,imm32
                    end if
                else
                    loadToReg(ecx,src)                          -- mov ecx,[src]
                end if
                emitHex5callG(opcode)                           -- call opEnterCS/opLeaveCS
                reginfo = 0 -- all regs trashed
            end if
            pc += 2
        elsif opcode=opTryCS then
            dest = s5[pc+1]
            getDest()
            slroot = T_integer
            sltype = slroot
            smin = 0
            smax = 1
            storeDest()
            if not isGscan then
                src = s5[pc+2]
                getSrc()
                markConstUsed(src)
                if slroot=T_integer and smin=smax then
--                  ?9/0
--                  if smin=0 then
--                      if X64 then
--                          emitHex1(#48)
--                      end if
--                      emitHex2s(xor_ecx_ecx)                  -- xor ecx,ecx
--                  else
                        ?9/0 -- DEV better message?
--                      movRegImm32(ecx,smin)                   -- mov ecx,imm32
--                  end if
                else
                    loadToReg(ecx,src)                          -- mov ecx,[src]
                end if
                leamov(edi,dest)                                -- lea edi,[dest]
                emitHex5callG(opcode)                           -- call opTryCS
                reginfo = 0 -- all regs trashed
                storeReg(eax,dest,0,1) -- record the fact that eax==[dest]
            end if
            pc += 3
--/*
    opInitCS = 234,     -- opInitCS,dest
    opDeleteCS = 235,   -- opDeleteCS,src
    opEnterCS = 236,    -- opEnterCS,src(/T_const0)
    opTryCS = 237,      -- opTryCS,dest,src
    opLeaveCS = 238,    -- opLeaveCS,src(/T_const0)
--*/
        elsif opcode=opCrashFile
           or opcode=opCrashMsg
           or opcode=opCrashRtn then
            if not isGscan then
                src = s5[pc+1]
--              getSrc()
                loadToReg(eax,src)                              -- mov e/rax,[src]
--DEV??
--              movRegVno(esi,src)                              -- mov e/rsi,src (var no)
                emitHex5callG(opcode)                           -- call :%pCrashFile/Msg/Rtn (see pDiagN.e)
                reginfo = 0 -- all regs trashed
            end if
            pc += 2
        elsif opcode=opProfout then
            if not isGscan then
                emitHex5callG(opcode)                           -- dump_profile()
                reginfo = 0 -- all regs trashed
            end if
            pc += 1
        elsif opcode=opGetText 
           or opcode=opDcvar then
--30/5/17:
--DEV and opDcvar??
            if opcode=opGetText then
                dest = s5[pc+1]
                getDest()
--DEV we might be able to optimise this further:
-- if src2(aka option) is -2(GT_WHOLE_FILE) then dest is string/int elsif not -2 then dest is T_Dsq/string else as below(?)
                slroot = T_Dsq+T_string
                setyp = T_integer+T_string
                slen = -2
                sltype = slroot
                storeDest()
            end if
            if not isGscan then
                dest = s5[pc+1]
                src = s5[pc+2]
                src2 = s5[pc+3]
--              getSrc()
                leamov(edi,dest)                                -- lea edi,[dest]
                loadToReg(eax,src)                              -- mov eax,[fn]
--DEV constant handling (opGetText only)...
                loadToReg(ecx,src2)                             -- mov ecx,[option]
                emitHex5callG(opcode)                           -- call opGetText
                reginfo = 0 -- all regs trashed
            end if
            pc += 4
        elsif opcode=opWrap then
            if not isGscan then
                src = s5[pc+1]
--              getSrc()
--DEV constant handling...
                loadToReg(eax,src)                              -- mov eax,[flag]
                emitHex5callG(opcode)                           -- call opWrap
                reginfo = 0 -- all regs trashed
            end if
            pc += 2
        elsif opcode=opScroll then
            -- calling convention
            --  mov eax,[amount]    -- +/-lines (opUnassigned, integer)
            --  mov ecx,[top]       -- topline (opUnassigned, integer)
            --  mov esi,[bottom]    -- btmline (opUnassigned, integer)
            --  call :%opScroll     -- scroll(eax,ecx,esi)
            if not isGscan then
                src = s5[pc+1]
--              getSrc()
--DEV constant handling...
                loadToReg(eax,src)                              -- mov eax,[amount]
                src = s5[pc+2]
                src2 = s5[pc+3]
                loadToReg(ecx,src)                              -- mov ecx,[top]
                loadToReg(esi,src)                              -- mov ecx,[btm]
                emitHex5callG(opcode)                           -- call opScroll
                reginfo = 0 -- all regs trashed
            end if
            pc += 4
        elsif opcode=opTextRows
           or opcode=opOpenDLL
           or opcode=opCallback then
            -- calling convention
            --  lea edi,[res]       -- result location
            --  mov eax,[lines]     -- lines (opUnassigned, integer)
            --  call :%opXXX        -- [edi]:=text_rows/open_dll/call_back(eax)
            --DEV potentially merge-able with opFloor/opRand/opGetc/opGets/opWhere/opCos(etc)...
            if not isGscan then
                dest = s5[pc+1]
                src = s5[pc+2]
--              getSrc()
                leamov(edi,dest)                                -- lea edi,[dest]
--DEV constant handling... (opTextRows/opCallback only)
                loadToReg(eax,src)                              -- mov eax,[fn]
                emitHex5callG(opcode)                           -- call opTextRows/opOpenDLL/opCallback
                reginfo = 0 -- all regs trashed
            end if
            pc += 3
        elsif opcode=opDcfunc then
            -- calling convention
            --  lea edi,[res]       -- result location
            --  mov eax,[lib]       -- (opUnassigned)
            --  mov ecx,[fname]     -- (opUnassigned)
            --  mov esi,[args]      -- (opUnassigned)
            --  mov edx,[rtyp]      -- (opUnassigned) [0 for define_c_proc]
            --  call :%opDcfunc     -- [edi]:=define_c_func(eax,ecx,esi,edx)
            if not isGscan then
                dest = s5[pc+1]
                src = s5[pc+2]
                src2 = s5[pc+3]
                leamov(edi,dest)                                -- lea edi,[dest]
                loadToReg(eax,src)                              -- mov eax,[lib]
                loadToReg(ecx,src2)                             -- mov eax,[fname]
                src = s5[pc+4]
                src2 = s5[pc+5]
                loadToReg(esi,src)                              -- mov esi,[args]
                markConstUsed(src2)
                getSrc2()
                if and_bits(state2,K_rtn) then ?9/0 end if
                if and_bits(state2,K_Fres) then ?9/0 end if
                if slroot2=T_integer and smin2=smax2 then
                    if smin2=0 then
                        if X64 then
                            emitHex1(#48)
                        end if
                        emitHex2s(xor_edx_edx)                  -- xor edx,edx
                    else
                        movRegImm32(edx,smin2)                  -- mov edx,imm32
                    end if
                else
                    loadMem(edx,src2)                           -- mov edx,[src2]
                end if
                emitHex5callG(opcode)                           -- call opTextRows/opOpenDLL
                reginfo = 0 -- all regs trashed
            end if
            pc += 6

        elsif opcode=opDelRtn then
            --  lea edi,[res]       -- result location
            --  mov esi,[o]         -- (opUnassigned)
            --  mov eax,[rid]       -- (opUnassigned)
            --  call :%opDelRtn     -- [edi]:=delete_routine(esi,eax)
--DEV see call_func? (storeDest)
            if not isGscan then
                dest = s5[pc+1]
                src = s5[pc+2]
                src2 = s5[pc+3]
--              getSrc2()
                leamov(edi,dest)                                -- lea edi,[dest]
                loadToReg(esi,src)                              -- mov eax,[o]
                loadToReg(eax,src2)                             -- mov eax,[rid]
--              if slroot2=T_integer and smin2=smax2 then
--                  if smin2=0 or and_bits(state2,K_rtn)=0 then
----                        if X64 then
----                            emitHex1(#48)
----                        end if
----                        emitHex2s(xor_eax_eax)                  -- xor eax,eax
--                      ?9/0 -- DEV better message?
--                  else
----                        ?9/0 -- DEV better message?
--                      movRegImm32(eax,smin2)                  -- mov eax,imm32
--                  end if
--              else
--                  loadToReg(eax,src2)                         -- mov eax,[rid]
--              end if
                emitHex5callG(opcode)                           -- call opDelRtn
                reginfo = 0 -- all regs trashed
            end if
            pc += 4

        elsif opcode=opDelete then
            -- calling convention
            --  mov eax,[o]     -- (opUnassigned)
            --  call :%opDelete     -- delete(eax)
            if not isGscan then -- (30/01/21!!)
                src = s5[pc+1]
                loadToReg(eax,src)                              -- mov eax,[o]
                emitHex5callG(opcode)                           -- call opDelete
                reginfo = 0
            end if
            pc += 2
        elsif opcode=opCallFunc then
            -- calling convention
            --  lea edi,[res]       -- result location
            --  mov eax,[rid]       -- (opUnassigned)
            --  mov esi,[args]      -- (opUnassigned)
            --  call :%opCallFunc   -- [edi]:=call_func(eax,esi)
            dest = s5[pc+1]
            getDest()
            slroot = T_object
            sltype = T_object
            setyp = T_object
            slen = -2
            storeDest()
            if not isGscan then
                src = s5[pc+2]
                src2 = s5[pc+3]
                leamov(edi,dest)                                -- lea edi,[dest]
--DEV imm32?
                loadToReg(eax,src)                              -- mov eax,[rid]
                loadToReg(esi,src2)                             -- mov esi,[args]
                emitHex5callG(opcode)                           -- call opCallFunc
                reginfo = 0
            end if
            pc += 4
        elsif opcode=opCallProc then
            -- calling convention
            --  mov eax,[rid]       -- (opUnassigned)
            --  mov esi,[args]      -- (opUnassigned)
            --  call :%opCallProc   -- call_proc(eax,esi)
            if not isGscan then
                src = s5[pc+1]
                src2 = s5[pc+2]
--DEV imm32?
                loadToReg(eax,src)                              -- mov eax,[rid]
                loadToReg(esi,src2)                             -- mov esi,[args]
                emitHex5callG(opcode)                           -- call opCallProc
                reginfo = 0
            end if
            pc += 3

        elsif opcode=opTry then
            -- opTry,prev,tgt,esp4
            --   prev is previous handler, ie [ebp+16/rbp+32],
            --   tgt is link (il->x86) for setting the new catch handler address,
            --   esp4 is previous value of (esp|rsp)/4, to rebalance the stack.
            if not isGscan then
                tvar = s5[pc+1]             -- a temp to hold prev_handler
                integer esp4 = s5[pc+3]     -- a temp to hold (esp|rsp)/4
                if X64 then
                    emitHex1(#48)
                end if
                emitHex2s(mov_ecx_esp)                          -- mov ecx,esp
                if X64 then
                    emitHex1(#48)
                    emitHex3(mov_eax_ebpd8,32)                  -- mov rax,[rbp+32] ; exception handler
                else
                    emitHex3(mov_eax_ebpd8,16)                  -- mov eax,[ebp+16] ; exception handler
                end if
                if X64 then
                    emitHex1(#48)
                end if
                emitHex3(shr_ecx_imm8,2)                        -- shr ecx,2
                leamov(edi,tvar)                                -- lea edi,[tvar]/mov edi,tvar (addr tmp)
                storeReg(ecx,esp4,1,0)                          -- mov [esp4],ecx
                emitHex5callG(opStoreMint)                      -- call :%pStoreMint
                if X64 then
                    emitHex1(#48)
                    emitHex7a(mov_ebpd8_i32,32,0)               -- mov [rbp+32],<catch addr>
                else
                    emitHex7a(mov_ebpd8_i32,16,0)               -- mov [ebp+16],<catch addr>
                end if
                backpatch = length(x86)
                s5[pc+2] = backpatch
                -- (or just kill eax,ecx,edi:)
                reginfo = 0
            end if
            pc += 4

        elsif opcode=opTryend then
            -- opTryend,mergeSet(0),tgt,link,tlnk
            --  (next op will be opCatch, or opLn then opCatch)
            if not isGscan then
                tlink = s5[pc+4]
                if s5[tlink]!=opTry then ?{"pilx86 line 15142 not opTry",opTry} end if
                tvar = s5[tlink+1]
                loadToReg(eax,tvar)                             -- mov eax,[tvar]
                emitHex5callG(opLoadMint)                       -- call :%pLoadMint
                if X64 then
                    emitHex1(#48)
                    emitHex3(mov_ebpi8_eax,32)                  -- mov [rbp+32],rax
                else
                    emitHex3(mov_ebpi8_eax,16)                  -- mov [ebp+16],eax
                end if
                emitHex5j(0)                                    -- jmp :endtry (backpatched later)
                backpatch = length(x86)
                s5[pc+2] = backpatch
                backpatch = s5[tlink+2]
                x86[backpatch] = length(x86)-backpatch          --::catch
                reginfo = 0
            end if
            pc += 5

        elsif opcode=opCatch then
            -- opCatch,tlnk,e
            --  tlnk locates the opTry (for tvar and esp4)
            --  e is the (sequence) exception variable
            if not isGscan then
                dest = s5[pc+2]
                tlink = s5[pc+1]
                if s5[tlink]!=opTry then ?{"pilx86 line 15177 not opTry",opTry} end if
                tvar = s5[tlink+1]
                integer esp4 = s5[tlink+3]
                storeReg(eax,dest,1,0)                          -- mov [dest],eax
                loadToReg(ecx,esp4)                             -- mov ecx,[esp4]
                loadToReg(eax,tvar)                             -- mov eax,[tvar]
                emitHex5callG(opLoadMint)                       -- call :%pLoadMint
                if X64 then
                    emitHex1(#48)
                end if
                emitHex3(shl_ecx_imm8,2)                        -- shl ecx,2
                if X64 then
                    emitHex1(#48)
                    emitHex3(mov_ebpi8_eax,32)                  -- mov [rbp+32],rax
                else
                    emitHex3(mov_ebpi8_eax,16)                  -- mov [ebp+16],eax
                end if
                if X64 then
                    emitHex1(#48)
                end if
                emitHex2s(mov_esp_ecx)                          -- mov esp,ecx
                reginfo = 0
            end if
            pc += 3

        elsif opcode=opThrow then
            -- opThrow,e,user
            if not isGscan then
                src = s5[pc+1]
                src2 = s5[pc+2]
                loadToReg(eax,src)                              -- mov e/rax,[src]
                loadToReg(ecx,src2)                             -- mov e/rcx,[src2]
                emitHex5callG(opThrow)                          -- call :%pThrow (in pDiagN.e)
                emitHex1(0o314)                                 -- int3
                reginfo = 0 -- all regs trashed
            end if
            pc += 3

        else
            if opcode>=1 and opcode<=length(opNames) then
                opName = opNames[opcode]
            else
                opName = "???"
            end if
            printf(1,"\n\n**internal error pilx86.e line 15225, unknown opcode(%d=%s), emitline=%d)**\n\n\n",{opcode,opName,emitline})
            opcode = opcode/0
        end if
        --31/12/15:
        if suppressopRetf then
            if pc>length(s5) then exit end if
        end if
--if ttrap then
--  if tmpd and pc>=tmppc then
--      ?9/0
--  end if
--  ttrap = 0
--end if
        po = opcode
--      if opc=pc then trace(1) end if
--      opc = pc
--      if sch00n then ?9/0 end if
    end while

    if not isGscan then
        --
        --  Lastly fixup any #ilasm jumps (see also design notes in pops.e):
        --
        --       s5/il:                                     x86:
        --       ======                                     =======
        --      thisAsm                    +--------------> x86jloc (start of block where jump is)
        --          x86jloc ---------------+                            <
        --          <next opAsm>                            isJmp        |
        --      +-- first jump                              -*1          | joffset
        --      |   ...                 <                   -*2          |
        --      |   isJmp:               |                  x86 offset  <       ***<<<***
        --      +-->    jnxt             | joffset
        --              tgtAsm           |
        --              code/il index   <                   ...
        --
        --      tgtAsm                     +--------------> x86tloc (start of block where target is)
        --          x86tloc ---------------+                        <
        --          <next>                                           | toffset
        --          <jnxt>                                  ::label <
        --          ...         <
        --          ...          | toffset
        --         index:       <
        --
        --
        --  Our goal here is to put the correct x86 jump offset at the ***<<<*** marker.
        --  ---------------------------------------------------------------------------
        --
        --  Note that we care not whether dealing with jmp/jge/etc.
        --  For each jump we start with the code/il index (plus thisAsm,tgtAsm idx to il).
        --  We calculate joffset and toffset, which are unchanged as opAsm copied verbatim.
        --    (ie the distances of jmp/lbl from their opening #ilasm statements.)
        --    (also x86jloc,x86tloc were saved as we copied the opAsm content to x86.)
        --    (we use the same code when jmp/tgt are in the same block, no problem.)
        --  Hence we require x86[x86jloc+joffset] := (x86tloc+toffset)-(x86jloc+joffset).
        --  In case you are wondering, we cannot do this in pmain.e for jumps between
        --    opAsm blocks; we might know the length of the il but we do not know the
        --    length of any x86 between said opAsm's until the final (not isGscan)
        --    pass. True, we could encode the correct offsets for jumps within the
        --    same opAsm block (even when they are heavily split up by opLns), but
        --    it is much easier just to treat them all the same.
        --
        -- *1,*2: if q86 then these need to be linked up here as well.
        --
--if linkFwd then
        thisAsm = firstAsm
--end if
        while thisAsm do
            -- for each opAsm block
            if s5[thisAsm]!=opAsm then ?9/0 end if
            thisAsm += 1
            x86jloc = s5[thisAsm]                   -- start of block where the jump is
            jnxt = s5[thisAsm+2]                    -- jmp (code/il idx)
            while jnxt do
                -- for each jump in that block
                if not find(s5[jnxt-1],{isJmp,isAddr}) then ?9/0 end if
                joffset = jnxt-thisAsm              -- offset relative to block start
                tgtAsm = s5[jnxt+1]                 -- get target opAsm block
                if s5[tgtAsm]!=opAsm then ?9/0 end if
                x86tloc = s5[tgtAsm+1]              -- start of block where the target is
                toffset = s5[jnxt+2]                -- target (code/il idx)
                tgtAsm += 3                         -- adjust for header fields
                toffset -= tgtAsm                   -- offset relative to block start
                -- x86[x86jloc+joffset] := (x86tloc+toffset)-(x86jloc+joffset):
                joffset += x86jloc
                toffset += x86tloc
                toffset -= joffset
                k = joffset-3
                if not find(x86[k],{isJmp,isAddr}) then ?9/0 end if
                x86[joffset] = toffset
if q86 then
                q86insert(k)
end if
                jnxt = s5[jnxt]
            end while
            thisAsm = s5[thisAsm+1]
        end while
        -- ( ^ Now that is what I call a bit of proper code ;-) ^ )


        if tmpd then ?9/0 end if    -- transtmpfer not picked up correctly?
--  if tmpd then puts(1,"tmpd BUG\n\n") tmpd = 0 end if     --DEV temp

        if ltline then
            LineTab = append(LineTab,length(x86))
--  else
--      LineTab = 0
        end if

        if q86 then
--?9/0  -- to be completed:
--puts(1,"q86: final block of zeroes\n")
            if q86>1 then
--?{q86first,q86last,q86f2,q86l2,0}
                x86 &= {q86first,q86last,q86f2,q86l2,0}
            else
--?{q86first,q86last,0,0,0}
                x86 &= {q86first,q86last,0,0,0}
            end if
        end if

        s5 = x86
        symtabN = {}
    end if

    --  This is most definitely needed, but not really sure why...
    --  (in theory the ltclear() at the start should be enough)
if NOLT=0 or bind or lint then
    ltclear(-vi)
end if -- NOLT

end procedure

--/**/-- not strictly necessary, but reduces opCallOnce/fwd calls/onDeclaration
--/**/include sort.e -- sort()

global procedure opshow()
sequence s
integer l, fn
    l = length(opcount)
    s = repeat(0,l)
    for i=1 to l do
        s[i] = {opcount[i],opNames[i]}
    end for
    s = sort(s)
    fn = open("ildump.txt","w")
    if countTransTmpFer then
        puts(fn,"possible transtmpfer opcodes:\n")
    elsif showOpCounts then
        puts(fn,"OpCount listing:\n")
    else
        ?9/0
    end if
    for i=l to 1 by -1 do
        printf(fn,"%d %s\n",s[i])
    end for
    close(fn)
end procedure


-- Summary of register usage:
-- ==========================
--  Purpose: As a reference and to expose common themes and unify code.
--           (warning: the contents of this table are not verified)
--  Effort and risk: Huge.
--  Gains: Insignificant.
--  Legend: for "etc", search above
--          dest/src/src2/etc   -- var addresses
--          [src]/[src2]        -- var refs
--          <dest>  -- value to be saved on return
--          src*    -- var addr that sh/cd? be a ref:
--     (Theoretically, a mov reg,[src] would save a half clock over
--      the existing mov reg,src & some later mov reg2,[reg].
--      However, such changes would not only be fiddly and risky,
--      but there is no chance, frankly, of even the most brutal
--      and specifically targetted benchmark seeing a gain of 5%.
--newEBP
--      Plus using that [esp]-9/-15 style error handling means we
--      must schend() early & possibly loose any gains that way.
--      The most significant *s have long since been removed.)
--
--                  eax     ecx     edx     esi     edi
--  dealloc                         [prev]
--  e92movti                        dest    src
--  e01tcfIncDec                    BF40    dest                [DEV gone, where is e01tcfAddiii?]
--  opCleanUp1                              first
--  opCleanUp                               first   last
--  opMove          [src]           [dest]          dest
--  opFrame         first   N                       rtnid
--  opTcFail                        src
--  opSubse1                dest            [src]   [idx]
--  opSubse1i[s/p]  <dest>                  [src]   [idx]
--  opJcc/JccE      [src]                           [src2]
--  opAdd etc                       src2*   src*    dest
--  opRepe1                 [src2]          [dest]  [idx]
--  opJif           [src]
--  opOpen etc                      dest    src2*   src*        [DEV out-of-date]
--  opOpen          [path]  [mode]                  dest
--  opPeek[4s/u]            dest    src*
--  opFind/Match            dest    src2*   src*
--  opConcat        dest                    src*    src2*
--  opApnd/Ppnd             src2*   dest            src*
--  opPoke etc                      src*    src2*
--  opXxxBits                       src*    src2*   dest
--  opPuts                          [src2]          [src]
--  opCallProc      [src2]  [src]
--  opSxx           [src]           dest            [src2]
--  opAlloc etc                             src*    dest
--  opCallOnce                                      rtnid
--  opFloor                 src*                    dest
--  opFree etc                                      src*
--  opUminus/Not                            src*    dest
--  opJnotx                                 [src]   [idx]
--  opPow           [src]   [src2]                  dest
--  opXor                           src*            dest
--  opAbort                         [src]
--  opINSP          [src]                           dest
--  opLen           [src]                           dest
--  opDiv2          [src]                           dest
--  opScmp          [src]           dest            [src2]
--  opGetc/OpenDLL  [src]           dest
--  opWhere etc     [src]   dest
--  opRmdr          [src]   [src2]                  dest
--X opRepeat        src*    dest                    src2*
--  opCos etc                       src*            dest
--  opPeeki         <dest>                          [src]
--  opWaitKey etc                                   src*/dest
--  opLock                  lktyp*  dest    range*  fn*
--  opUnLock                                src2*   src*

--
-- pgscan.e
-- ========
--
-- implements gvar_scan(), to identify global optimisations.
--
-- This calculates the type, min/max integer values, element type, and sequence
--  length for all variables, at least as best it can. Suppose that atom a is
--  in fact only ever assigned an integer, then we can avoid (testing for) use
--  of the FPU, as well as incref/decref/dealloc. Further suppose that a is
--  known to be eg in the range 0 to 17, then we know that a+1 is an integer,
--  and so on. Credit is due to RDS Eu for this concept, although this is a
--  quite different implementation, expected to be both faster and weaker, see
--  comments (invite) below.
--
--DEV reinvestigate these once pltype.e is fully working:
--
-- The process of repeated iteration.
-- =================================
-- Consider the following code:
--
--  integer a
--          a = 1
--  integer b,c,d,e,f,g,h,i,j
--  procedure p()
--          j = a
--          i = j
--          h = i
--          g = h
--          f = g
--          e = f
--          d = e
--          c = d
--          b = c
--  end procedure
--          p()
--          a = 2
--          p()
--
--  It is not until the end of the first iteration, through all code, that
--  we can be certain that a has the range 1..2. Hence it is not until the
--  second iteration that we can know that j has that same range. It is in
--  the third iteration before we can say the same about variable i. Hence
--  it takes some 9 iterations until we know the range of b. In real world
--  programs such dependency chains are expected to be much shorter.
--
--  Similar examples could be created to illustrate type/etype/length.
--
--  Circularity:
--  ===========
--
--  integer a,b
--      a = 1
--      b = a
--      a = b
--
--DEV out of date:
--  Viewed from a global perspective, from "b=a", you cannot fully know the
--  value of b until you fully know the value of a, likewise from "a=b", you
--  cannot fully know the value of a until you fully know the value of b.
--  You might say that this (pgscan.e) has no "temporal logic" (groan).
--   (the RDS Eu implementation fares much better on such simple cases)
--  In the strange world of "global scan", we simply visit each statement
--  once, effectively in no particular order, rather than attempt to take
--  account of loops, conditionals, or multiple routines. Nor is there *any*
--  concept of eg at "a=b" there is/was/only a "b=a" to be considered.
--
--  As a result, the above leads to a,b being given {MININT,MAXINT} status.
--   (Not exactly a catastrophy, btw, just avoiding confusing ourselves.)
--
--  Note that such circularity is oddly enough quite easy to deal with.
--  Suppose we add "a=b" to the first example. Since there is no improvement,
--  the scan terminates after just two iterations, rather than nine.
--
--  It takes no genius on my part to predict you are now thinking that you
--  could do better; this is your invite to try. Be warned, however, it is
--  not as easy as it looks. I am hopeful the approach implemented below
--  will be correct on a high enough percentage and/or any such increased
--  accuracy would not in fact deliver any further significant gains.
--

-- ?Is this applicable somewhere/sometime?:
--      -- NB src may have a localtype of integer, but that does not necessarily
--      --    mean it has a gtype of atom and therefore meaningful min/max.


--GVAR_SCAN: (DEV use a second param to ilxlate?)
global procedure gvar_scan(integer vi)
    isGscan = 1
    ilxlate(vi)
    isGscan = 0
end procedure

sequence jdesc
constant mdesc = {"scMerge", "exprMerge", "exitMerge", "ifMerge", "endIfMerge", "breakMerge"}



procedure initj()
sequence d
integer k
string options
    jdesc = repeat(0,length(opNames))
    jdesc[opJmp] = "opJmp,mergeSet,tgt,link\n"
    for i=opJge to opJle do
        jdesc[i] = opNames[i]&",mergeSet,tgt,link,p2,p3,tii,bothInit\n"
    end for
    jdesc[opJif] = "opJif,mergeSet,tgt,link,p1,oii,lastparam\n"
    jdesc[opJnot] = "opJnot,mergeSet,tgt,link,p1,oii,lastparam\n"
--      (If lastparam is non-zero, p1 is the result var of a type routine
--       just called on it; this is used to create some localtype info.)
    jdesc[opJtyp] = "opJtyp,mergeSet,tgt,link,0,0/flippable,0/init,src,invert,ltype\n"
--      (btw, since -dumpil does not generate x86 code, which creates the
--       second {mergeSet,tgt,link}, we always get 0,0,x in ildump.txt)
    jdesc[opJbits] = "opJbits,mergeSet,tgt,link,opcode,src,src2,invert\n"
    jdesc[opJlen] = "opJlen,mergeSet,tgt,link,p1,tvar,isInit,invert\n"
    jdesc[opJnotx] = "opJnotx,mergeSet,tgt,link,src,src2,invert\n"
--DEV theoretically ctnr should be removed from opLabel/opLoopTop/opEndFor
--      but it may yet be useful as a sanity check. Actually you could also put
--      ctnr/brpt on opLoopTop and opEndFor rather than always pairing them with
--      an opCtrl..
    jdesc[opLabel] = "opLabel,mergeSet,0/x86loc,link\n"
    jdesc[opLoopTop] = "opLoopTop,lmask,gmask,end\n"
    jdesc[opEndFor] = "opEndFor,END+LOOP,bpFor\n"
    jdesc[opCall] = "opCall\n"  -- (routine to call saved in opFrame/on stack)
    jdesc[opMove] = "opMove,dest,src,isInit,onDeclaration,ltype\n"
        -- isInit is actually a chain; 0 is False, anything else, including the
        --  -1 terminator, is True (as far as you & this pgm are concerned).
    jdesc[opMovsi] = "opMovsi,dest,src,isInit,onDeclaration\n"
    jdesc[opMovti] = "opMovti,dest,src,isInit\n"
    jdesc[opMovbi] = "opMovbi,dest,src,isInit\n"
    jdesc[opApnd] = "opApnd,dest,seq,item\n"
    jdesc[opPpnd] = "opPpnd,dest,seq,item\n"
    jdesc[opConcat] = "opConcat,dest,a,b\n"
    jdesc[opConcatN] = "opConcatN,N,ref1..refN,res\n"
--  jdesc[opRepeat] = "opRepeat,dest,item,count\n"
    jdesc[opRepe] = "opRepe,N,rep,idxN..idx1,ref\n"
    jdesc[opRepe1] = "opRepe1,dest,idx,rep\n"
-- isCompound is 1 for eg s[i] += x; in the s[i+j] += (etc) case, we refer to
--  the (tmp) i+j twice, hence it must be stored. See transtmpfer(), which
--  checks for this condition and stores as well as transfers, if need be.
    jdesc[opSubse1] = "opSubse1,dest,ref,idx,isInit(ref and idx),isCompound\n"
    jdesc[opSubse1i] = "opSubse1i,dest,ref,idx,isInit(ref and idx),isCompound\n"
    jdesc[opSubse1is] = "opSubse1is,dest,ref,idx,isInit(ref and idx),isCompound\n"
    jdesc[opGchk] = "opGchk,var,type,min,max,etyp,len,line,col,fileno\n"
    jdesc[opLen] = "opLen,dest,src,isInit(src),ltype(src, <= T_object)\n"
--  jdesc[opFor] = "opFor,init,ctl,step,limit,tgt\n"
    jdesc[opFor2] = "opFor,flags,init,ctl,step,limit,tgt\n"
    jdesc[opSubse] = "opSubse,N,dest,idxN..idx1,ref\n"
    jdesc[opMkSq] = "opMkSq,N,dest,eN..e1\n"
    jdesc[opSubss] = "opSubss,N,dest,sliceend,idxN..idx1,ref\n"
    jdesc[opReps] = "opReps,N,ref,sliceend,idxN..idx1,dest\n"
    jdesc[opFrst] = "opFrst,dest,src,srctype,pbr\n"
    jdesc[opAndBits] = "opAndBits,dest,src,src2,tii\n"
    jdesc[opOrBits] = "opOrBits,dest,src,src2,tii\n"
    jdesc[opXorBits] = "opXorBits,dest,src,src2,tii\n"
    jdesc[opAdd] = "opAdd,dest,src,src2\n"
    jdesc[opAddi] = "opAddi,dest,src,src2\n"
    jdesc[opAddiii] = "opAddiii,dest,src,src2\n"
    jdesc[opSub] = "opSub,dest,src,src2\n"
    jdesc[opSubi] = "opSubi,dest,src,src2\n"
    jdesc[opSubiii] = "opSubiii,dest,src,src2\n"
    jdesc[opMul] = "opMul,dest,src,src2\n"
    jdesc[opMuli] = "opMuli,dest,src,src2\n"
    jdesc[opMuliii] = "opMuliii,dest,src,src2\n"
    jdesc[opDiv] = "opDiv,dest,src,src2\n"
    jdesc[opDivi] = "opDivi,dest,src,src2\n"
    jdesc[opDiviii] = "opDiviii,dest,src,src2\n"
    jdesc[opDiv2] = "opDiv2,dest,src,src2\n"
    jdesc[opDivi2] = "opDivi2,dest,src,src2\n"
    jdesc[opDivf] = "opDivf,dest,src,src2\n"
    jdesc[opDivf2] = "opDivf2,dest,src,src2\n"
    jdesc[opRmdr] = "opRmdr,dest,src,src2,bothInit\n"
    jdesc[opCtrl] = "opCtrl,stmt,link,emitline\n"
    jdesc[opLchk] = "opLchk,N,typ,tokline,tokcol,fileno\n"
    jdesc[opAsm] = "opAsm,len/x86loc,0/next,jlnk, then <len bytes>\n" -- (see pops.e)
    jdesc[opTchk] = "opTchk,varno,wasOptTypeCheck,default\n"
    jdesc[opOpen] = "opOpen,dest,filepath,mode\n"
--  jdesc[opMalloc] = "opMalloc,dest,size\n"
--  jdesc[opMfree] = "opMfree,addr\n"
--  jdesc[opXxx] = "res\n"
    jdesc[opTry] = "opTry,tmp,tgt\n"
--  jdesc[opCatch] = "opCatch,mergeSet(0),tgt,link,tlnk,e\n"
    jdesc[opCatch] = "opCatch,tlnk,e\n"
    jdesc[opThrow] = "opThrow,e,user\n"
    jdesc[opPow] = "opPow,dest,src1,src2,tii[==1]\n"

-- Other descriptions can be added here as needed, or possibly this lot could be moved into pops.e
--  for use elsewhere. Things like opLn etc are pretty self-evident and not worth adding?

    puts(dilfn,"Phix intermediate code listing (to be used in conjunction with -d/list.asm)\n")
    puts(dilfn,"===========================================================================\n\n")
    puts(dilfn,"Note: This may be useful when debugging the compiler, but is highly unlikely\n")
    puts(dilfn,"      to be of much use when debugging application code. (cmiiw)\n\n")
    puts(dilfn,"      If you change diagfn in pltype.e, to create an ltdiag.txt file, you will\n")
    puts(dilfn,"      also need a matching version of this file to make head or tail of it.\n\n")
    puts(dilfn,"      Also, pdiag.e et al are not included when interpreting (it relies on\n")
    puts(dilfn,"      the ones in p.exe) so to get properly matching ildump.txt and list.asm,\n")
    puts(dilfn,"      you need to use one of the following command pairs:\n")
    puts(dilfn,"        'p -dumpil test'            and 'p -d! test' (poor quality code)\n")
    puts(dilfn,"        'p -c -nodiag -dumpil test' and 'p -c -nodiag -d test' (recommended)\n")
    puts(dilfn,"        'p -c -dumpil test'         and 'p -d test' (>3700 lines)\n\n")

    d = date()
    d = reverse(d[1..6])    -- d is now {second,minute,hour,day,month,year}
    k = remainder(d[6],100) -- (or show as 4 digits)
    d[6] = k
    k = d[1]
    d[1] = d[3]
    d[3] = k                -- d is now {hour,minute,second,day,month,year}
    printf(dilfn,"      Generated at %d:%02d:%02d on %02d/%02d/%02d. ",d)
--  options = ""
    if bind then
        options = "-c -dumpil"
    else
        options = "-dumpil"
    end if
    if nodiag then
        options &= " -nodiag"
    end if
    printf(dilfn," (%s)\n\n",{options})

end procedure

--integer pc, opcode
string bcomma
--, cleancode
integer Dfileno

--DEV//SUG use decode_flags()
function forflags(integer flags)
sequence desc
    desc = "("
    if and_bits(flags,#01) then
        desc &= "INIT"
    end if
    if and_bits(flags,#02) then
        if length(desc)=1 then
            desc &= "LIMIT"
        else
            desc &= "+LIMIT"
        end if
    end if
    if and_bits(flags,#04) then
        if length(desc)=1 then
            desc &= "STEP"
        else
            desc &= "+STEP"
        end if
    end if
    desc &= ")"
    return desc
end function

procedure pskip(integer skip)
integer byte, k, mpc, ms, ch, clean
object desc
    if dumpil then
        pc += 1
        mpc = pc
        for i=2 to skip do
            byte = s5[pc]
            if byte=-1073741824 then
                bcomma = "MIN,"
            elsif byte=1073741823 then
                bcomma = "MAX,"
            else
                bcomma = sprintf("%d,",byte)
            end if
            dlen += length(bcomma)
            puts(dilfn,bcomma)
            pc += 1
            if opcode=opAsm and i=4 then exit end if
        end for
        desc = jdesc[opcode]
        if sequence(desc) then
            if dlen<45 then
                puts(dilfn,repeat(' ',45-dlen))
            end if
            k = match("mergeSet",desc)
            if k then
                ms = s5[mpc]
                if ms then
                    if ms=isOpCode then
--DEV not newEmit...
                        if s5[mpc+1]!=0 then ?9/0 end if
                        if s5[mpc+2]!=opRetf then ?9/0 end if
                        if not equal(desc[k..k+17],"mergeSet,tgt,link,") then ?9/0 end if
                        desc = desc[1..k-1] & "isOpCode,0,opRetf" & desc[k+17..length(desc)]
                    else
                        desc = desc[1..k-1] & mdesc[ms] & desc[k+8..length(desc)]
                    end if
                end if
            elsif match("opCtrl,stmt,",desc)=1 then
                if desc[7]!=',' then ?9/0 end if
                if desc[12]!=',' then ?9/0 end if
                desc = desc[1..7] & ltSdesc(s5[mpc]) & desc[12..length(desc)]
--tryme:
--              desc[8..11] = ltSdesc(s5[mpc])
            elsif match("opFor,flags,",desc)=1 then
                if desc[12]!=',' then ?9/0 end if       
                desc = desc[1..11] & forflags(s5[mpc]) & desc[12..length(desc)]
--tryme:
--              desc[12..11] = forflags(s5[mpc])
--              desc[11..10] = "(" & decode_flags({{#01,"INIT"},{#02,"LIMIT"},{#04,"STEP"}},s5[mpc]) & ")"
--              -- (I have checked, there are no constants for this (1/2/4 ^^^) in pglobals.e, 5/6/2013)
            end if
            puts(dilfn,desc)
            if opcode=opAsm and skip>4 then
                dlen = 8
                puts(dilfn,"\t")
                clean = 1
                for i=5 to skip do
                    byte = s5[pc]
                    if byte=-1073741824 then
                        bcomma = "MIN,"
                    elsif byte=1073741823 then
                        bcomma = "MAX,"
                    else
                        bcomma = sprintf("%d,",byte)
                    end if
                    dlen += length(bcomma)
                    if dlen>45 and i<skip then
                        puts(dilfn,"\n\t")
                        dlen = length(bcomma)+8
                    end if
                    puts(dilfn,bcomma)
                    -- explain our opCleanUp code (only has to deal with 3 instructions,
                    --  really not much point adding a full disassembler here, given the
                    --  ready availability of list.asm to go with this ildump.txt... but
                    --  on dirt-simple code with no #ilasm{}s, let 'em know what it is).
--removed 23/10/15...
--                  if clean
--                  and remainder(i+1,5)=0 then -- only bother with 5-byte instructions!
--                      k = s5[pc-4]
--                      cleancode = "\t\t\t-- "
----DEV use find?
--                      if k=mov_esi_imm32
--                      or k=mov_edi_imm32
--                      or k=mov_ecx_imm32 then
--                          if s5[pc-3]=isVar then
--                              cleancode &= "mov "
--                              if k=mov_esi_imm32 then
--                                  cleancode &= "esi"
--                              elsif k=mov_edi_imm32 then
--                                  cleancode &= "edi"
--                              elsif k=mov_ecx_imm32 then
--                                  cleancode &= "ecx"
--                              else
----                                    cleancode &= "???"
--                                  clean = 0
--                              end if
--                              cleancode &= sprintf(",[symtab[%d]]",byte)
--                          else
--                              clean = 0
--                          end if
--                      elsif k=call_rel32 then
--                          if s5[pc-3]=isOpCode then
----DEV newEmit??
----DEV use find?
--                              if byte=opCleanUp1 then
--                                  cleancode &= "call opCleanUp1"
--                              elsif byte=opCleanUp then
--                                  cleancode &= "call opCleanUp"
--                              elsif byte=opGetST then
--                                  cleancode &= "call opGetST"
--                              elsif byte=opRTErn then
--                                  cleancode &= "call opRTErn"
--                              elsif byte=opCrshRtn then
--                                  cleancode &= "call opCrshRtn"
--                              else
--                                  clean = 0
--                              end if
--                          else
--                              clean = 0
--                          end if
--                      else
--                          clean = 0   -- (unrecognised code)
--                      end if
--                      if clean then
--                          puts(dilfn,cleancode)
--                          if i<skip then
--                              puts(dilfn,"\n\t")
--                              dlen = 8
--                          end if
--                      end if
--                  end if
                    pc += 1
                end for
                puts(dilfn,"\n")
            end if
        elsif opcode=opLn
           or opcode=opLnt
           or opcode=opLnp
           or opcode=opLnpt then
            k = s5[mpc] -- lineno
            if equal(expandedYet[Dfileno],0) then
                exptext[Dfileno] = expandIntoLines()
                expandedYet[Dfileno] = linestarts
            end if
-- 8/10/2014:
if k>length(exptext[Dfileno]) then
            desc = "<eof>"
else
            desc = exptext[Dfileno][k]
end if
            k = 0
            for i=1 to length(desc) do
                ch = desc[i]
                if ch=' ' then
                    k += 1
                elsif ch='\t' then
                    k += 8
                else
                    desc = "--:"&repeat(' ',floor(k/4))&desc[i..length(desc)]
                    exit
                end if
            end for
            if dlen<45 then
                puts(dilfn,repeat(' ',45-dlen))
            end if
            puts(dilfn,desc)
        elsif opcode=opFrame then
            if dlen<45 then
                puts(dilfn,repeat(' ',45-dlen))
            end if
            k = s5[mpc] -- routine no
            desc = "-- ("&symtab[k][S_Name]&")\n"
            puts(dilfn,desc)
        else
            puts(dilfn,"\n")
        end if
    else -- not dumpil
        pc += skip
    end if
end procedure

--with trace
global procedure gvar_scan_nobind(integer vi)
--
-- In the no bind case, scan the il much more quickly to link up
--  all actually used routines. Obviously, as above in gvar_scan,
--  when binding we repeatedly call ilxlate() with isGscan set to
--  1. Although we probably could use ilxlate, with lots of added
--  "if not bind then" everywhere, this also proved an excellent
--  choice for handling the -dumpil command line option.
--
integer skip, noofitems
sequence symk           -- copy of symtab[k]
integer routineNo, u, k -- used by opFrame
object rname    -- routine name (may temp be -1)
sequence fname  -- file name
integer Dpathno
--DEV:
integer lastop, lastln
    if dumpil then
        if vi=T_maintls then initj() end if
--      printf(dilfn,"routine %d:\n",vi)
--DEV (aborted due to problem with e110Repe1is)
        symk = symtab[vi]
        rname = symk[S_Name]
        if equal(rname,-1) then
            rname = "_top_level_sub_ for"
        else
            rname = NTdesc[symk[S_NTyp]]&rname&"() in"
        end if
        Dfileno = symk[S_FPno]
--DEV removed as unnecessary 2/11/09:
        text = allfiles[Dfileno]
        fname = filenames[Dfileno]
        Dpathno = fname[1]
        fname = filepaths[Dpathno]&fname[2]
        printf(dilfn,"routine %d (%s %s):\n===========\n",{vi,rname,fname})
    end if
    pc = 1
    while 1 do
if repl then
    if not integer(s5[pc]) then exit end if
end if
        opcode = s5[pc]
--if opcode=opInit then trace(1) end if
        if dumpil then
            bcomma = sprintf("%4d:  %s,",{pc,opNames[opcode]})
            dlen = length(bcomma)
            puts(dilfn,bcomma)
        end if
        skip = opSkip[opcode]
if skip=-20000 then ?9/0 end if
        if skip>0 then
            pskip(skip)
        else
            if skip<0 then              -- eg opMkSq is -3 (as defined in pops.e)
                noofitems = s5[pc+1]
                pskip(noofitems-skip)   -- eg/ie opMkSq is pc += noofitems+3.
            else
                if opcode=opFrame then      -- 12
                    routineNo = s5[pc+1]
-- 14/2/14:
                    k = find(routineNo,forwardingtable)
                    if k then
                        routineNo = forwardretarget[k]
                        s5[pc+1] = routineNo
                    end if
                    pskip(2)
                    symk = symtab[routineNo]
                    u = symk[S_State]
                    if not and_bits(u,K_used) then
                        symtab[routineNo] = 0       -- kill refcount (prevent pointless clone)
                        u += K_used
                        symk[S_State] = u
if NEWGSCAN then
--(fingers crossed this is ok inside the K_used check)
                        if symk[S_NTyp]>=S_Type
                        and g_scan[routineNo]==0 then
--?{"pilx86.e line 16102 (opFrame/gvar_scan_nobind), adding",routineNo,vi}
                            g_scan[routineNo] = g_scan[vi]
                            g_scan[vi] = routineNo
                        end if
else
--MARKTYPES (or opTchk...??)
                        if symk[S_NTyp]>S_Type then     -- all S_Type are already on the list; [DEV!!!]
--BUG:: (MARKTYPES 29/7/17)
--                      if symk[S_NTyp]>S_Func-MARKTYPES then
--                      if symk[S_NTyp]>=S_Type then
                                                        -- alternatively: linkup opTchks here.
                                                        -- (I think you'd find that slower)
                            -- add to chain of routines to be processed:
----DEV temp!
--printf(1,"pilx86.e line 9599: S_linking symtab[%d]\n",vi)
--printf(1,"pilx86.e line 9600: S_linking symtab[%d]\n",routineNo)
                            symk[S_Slink] = symtab[vi][S_Slink]
                            symtab[vi][S_Slink] = routineNo
                        end if
end if
                        symtab[routineNo] = symk
                    end if
                elsif opcode=opRetf             -- 14
                   or opcode=opBadRetf then     -- 15
                    pskip(1)
                    if pc>length(s5) then exit end if
                else
                    opcode = 9/0
                end if
            end if
        end if
        lastop = opcode
        if lastop=opLn then
            lastln = s5[pc-1]
        end if
        --12/01/16:
        if suppressopRetf then
            if pc>length(s5) then exit end if
        end if
    end while
    if dumpil then
        puts(dilfn,"\n")
    end if
end procedure

