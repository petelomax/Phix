========================================================================
This is the Euphoria version of the Great Computer Language Shootout benchmarks.
http://shootout.alioth.debian.org/

The Euphoria mailing list:
http://www.listfilter.com/EUforum



Notes on the various programs:
========================================================================

Ackermann.ex by Derek Parnell
Thanks, Derek!

binary-trees by Jason Gade
Translated from the C version, but using sequences.

count-words by Jason Gade
When I tested this in Windows not all files got counted correctly due to newline handling. I wrote a version (not included here) that handled DOS-style line terminations correctly but it took its input as an argument rather than stdin. I haven't tested this under Linux yet, so YMMV. Note that the input file on the website is *not* 6121 characters long! It is actually 6096 bytes. 

fannkuch by Jason Gade
I was going to translate the fortran or Pascal versions of this program, but neither one put out the permutations in the same order as the website specifies. I basically translated from the C# source. I don't completely understand the algorithms here, so improvements could probably be made.

random by Jason Gade
I was looking at the description of the fasta benchmark and I saw that it depended on the random benchmark. It was easy so I did it.

harmonic by Jason Gade

nsieve by Jason Gade

startup by Jason Gade

takfp by Jason Gade

fasta by Jason Gade
This is an important step in creating the rest of the benchmarks. Many other benchmarks use the output of this file. It wasn't too difficult once I understood the algorithms better. I used mainly the C source and the Pascal source to come up with my own way of doing it.

sumfile by Jason Gade

mandelbrot by Jason Gade

nbody by Jason Gade

reverse-complement by James W. Hofmann

reverse-complement2 by Jason Gade

partial-sums by András Szabó and Matt Lewis

TODO:
========================================================================
The rest of the benchmarks!  ;^)

Either get the Perl framework from the shootout website for conducting my own tests, or write a new one myself in Euphoria or another language. It will be difficult to gauge memory usage.

Submit the benchmarks to http://shootout.alioth.debian.org/ once we have at least 15 finished.

Brush up on my HTML skills and make a table showing current, deprecated, and 
orginal benchmarks.


Goals and Methodology:
========================================================================
When Euphoria 2.5 was released I was interested in comparing its performance to that of earlier versions. That's when I discovered the shootout website. I put off doing anything about it until Brent W. Hughes mentioned it on the Euphoria mailing list, and then Derek Parnell submitted the Ackermann benchmark. That finally got me off my backside (well, kinda) and got started on the rest of it. Hopefully it will be finished in this lifetime, or at least not too long after the release of the *next* version of Euphoria!

Once I get 8-10 benchmarks finished I need to actually start testing and recording the results. Since computer speeds differ, I will measure one C program (probably sieve) and use it as my time standard. I'll compare all other results to that.

I will need to either write my testing framework in Euphoria or use the Perl framework that the Shootout already uses. Since I'm already running Debian this might be the best idea.

I want to compare the following at a minimum:
Euphoria 2.4    interpreted
Euphoria 2.5    interpreted
Euphoria 2.5    translated
eu.ex       interpreted
eu.ex       translated



To Run the Tests:
========================================================================
edit bench.ex to contain the list of files to run (and their inputs) and the list of interpreters to run. Then execute bench.ex. The methodology is very similar to the actual shootout; that is each file is run once to lower cache effects and then run three times timed with output redirected to NUL (change that to /dev/null if using Linux or BSD).
