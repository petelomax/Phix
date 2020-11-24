-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org
--
-- Contributed by James W. Hofmann 2007
-- Modified by Jason Gade
-- run: exu reverse-complement.ex < input-file > output-file

without type_check
without warning

sequence in,dseq,headers,lseq
integer char
integer head -- the header we're on
integer charcount, newlinecount
integer ALLOCATE -- guessed length of input sequence
constant MAXLINE = 60 -- specified line length
ALLOCATE = 10000 -- guess how many chars needed; adjustable for different N-size
in = {repeat(0,ALLOCATE),repeat(0,ALLOCATE),repeat(0,ALLOCATE)}
headers = {{},{},{}}
dseq = {{},{},{}}

-- optimization for each of the three sequence types

dseq[1] = {"ACGT",
	"TGCA"} -- uppercase only
dseq[2] = {"acgtMRWSYKVHDBN",
	"TGCAKYWSRMBDHVN"} -- lowercase acgt, uppercase ambiguous codes 
dseq[3] = {"acgt",
	"TGCA"} -- lowercase only

lseq = {1,1,1} -- real length of each sequence
head = 0
charcount = 0 -- overall position in a sequence's output
newlinecount = 0 -- we have to recreate newlines when reversing

function getComp(integer code, integer headerstyle)
		for m=1 to length(dseq[headerstyle][1]) do
			if code=dseq[headerstyle][1][m] then
				code=dseq[headerstyle][2][m]
				return code
			end if
		end for 
		return code -- don't change unrecognized stuff
end function

while 1 do
    char = getc(0)    
    if char=-1 then
	exit   -- EOF
	elsif char='>' then
		head+=1
		while char!='\n' do -- write header
			if char>13 then -- take only printable stuff here
				headers[head]&=char				
			end if
			char = getc(0)
		end while
		headers[head]&=char -- add LF (but not CR)
	else
		if char>13 then -- a real character
		    lseq[head]+=1
		    in[head][lseq[head]]=getComp(char,head) -- convert chars one at a time
		    -- we will add newlines when we write
		end if
			
		if lseq[head]>=ALLOCATE then -- anticipate more space being needed
			in[1] = in[1] & repeat(0,ALLOCATE)
			in[2] = in[2] & repeat(0,ALLOCATE)
			in[3] = in[3] & repeat(0,ALLOCATE)
			ALLOCATE *= 2 -- this expands the expected allocation with larger requests
		end if

	end if
end while

for seqnum=1 to 3 do
	
	puts(1, headers[seqnum])

	newlinecount = lseq[seqnum]-MAXLINE+1
	for n = lseq[seqnum] to 1 by -1 do
		puts(1, in[seqnum][n])
		if n<=newlinecount and n>2 then -- n>2 lets the end of the sequence 
										-- terminate naturally
			puts(1,'\n')
			newlinecount-=MAXLINE
		end if
	end for

	puts(1,'\n')

end for

if getc(0) then end if
