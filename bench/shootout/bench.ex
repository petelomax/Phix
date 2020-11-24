-- benchmark testing harness program
-- By Jason Gade
-- January 9, 2007
--
-- Fill this list with the names of the files to test along with any arguments.
-- constant FILE_LIST = {"partial-sums.ex 2500000", "fasta.ex 2500000", "nsieve.ex 9",
--    "reverse-complement.ex < fasta-out.txt", "reverse-complement2.ex < fasta-out.txt",
--    "binary-trees.ex 16", "fannkuch.ex 10", "sumfile.ex < sumcol-input.txt", "startup.ex" }
constant FILE_LIST = { "nbody3.ex 2000000", "nbody.ex 2000000" }
-- Number of loops to run each test.
constant LOOPS = 3

-- List of interpreters to run
constant PREFIX = { "exwc.exe" }

sequence uut
sequence time_list 
time_list = repeat(repeat(0, LOOPS+1), length(FILE_LIST))
atom time_mark

for a = 1 to length(PREFIX) do
    for i = 1 to length(FILE_LIST) do
        puts(1, "\n\nTesting " & FILE_LIST[i] & "\n")
        uut = PREFIX[a] & " " & FILE_LIST[i]
        system(uut & " > output.txt", 2)
    
        time_list[i][$] = FILE_LIST[i]

        for j = 1 to LOOPS do
            time_mark = time()
            system(uut & " > NUL", 2)
            time_list[i][j] = time() - time_mark
	    printf(1, "%fs ", time_list[i][j])
        end for
    end for
end for

