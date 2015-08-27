--#
--# Compare text files
--#  based on Wu, Manber and Myers.
--#  Author: Pete Lomax.
--#  Credit and thanks to Ricardo Forno; although he wrote no lines in this particular source,
--#  it would definitely never have been written without his help.
--#
--# Usage: function diff(a,b): returns a stack of change points, see below:
--#   a and b are files (as sequences) to compare.
--#   the number of changes (inserts+deletes) can be determined as length(result)/2 - 1.
--#   the result is a list of points in the form {x1,y1,x2,y2,..xp,yp}.
--#   the final xp,yp pair should always be equal to length(a)+1,length(b)+1.
--#   the result is created this way for speed in this program, so requires some interpretation:
--#
--#   First, here is the code you will need:
--#
--#   prevx=1  prevy=1  prevdiag=0
--#   for i=1 to length(result) by 2 do
--#      thisx=result[i]  thisy=result[i+1]
--#      diagresult=thisx-thisy
--#      if diagresult<prevdiag then insert(b[prevy]) prevy+=1 end if
--#      if diagresult>prevdiag then delete(a[prevx]) prevx+=1 end if
--#      for j=prevx to thisx-1 do copy(a[j]) end for
--#--    for j=prevy to thisy-1 do copy(b[j]) end for -- should be equivalent
--#      prevx=thisx  prevy=thisy  prevdiag=diagresult
--#   end for
--#
--#   And the equivalent explaination in English:
--#
--#   Each (x,y) pair in the result represents the *end* of a matching group, with an
--#   implied *single* insertion or deletion from the previous (x,y) pair (initially 1,1).
--#   To detect if it is an insertion or deletion (or neither possible at start of file), use
--#   the difference between x and y of this point vs the previous point; -1 indicates insertion
--#   and +1 indicates a deletion. A crude illustration:
--#
--#     ----------...
--#     |\
--#     | \
--#     |  A-
--#     |  |  \
--#     |   \  \
--#     |    B  C
--#     ...
--#
--#  Here the point A represents a couple of lines at the start of the files which match, then
--#  there is a difference. B represents one path, whereby a line insertion is followed by one
--#  matching line, and C represents another path, whereby a line deletion is followed by two
--#  matching lines. If C is in fact the end of both files, then that path is selected, and
--#  {A,C} is returned. Note that (Ax-Ay)-(Bx-By) is +1 and (Ax-Ay)-(Cx-Cy) is -1. The number
--#  of subsequent matching lines leading to C can be deduced from either Cx-Ax or Cy-Ay.
--#
--# Performance notes.
--#   Some routines (I evaluated at least 14 other programs) will give reasonable results in
--#   perhaps 1/20th of the time.  But mediocre results in 0.1 seconds really don't compare
--#   to optimal results in 2 seconds, not if you have to fix up all the errors made by the
--#   computer manually. Occasionally, the files will be sufficiently different that the file
--#   compare could take hours or even days, and unfortunately there are equally cases where
--#   progress starts well but grinds to a crawl. If you really do hit problems then the very
--#   best answer is to manually break up a large program into chunks, possibly individual
--#   routines. I will say that this algorithm will usually complete in under a second unless
--#   there are more than say 2,000 differences between the files, so you probably don't really
--#   want any answer that takes more than a few seconds to compute anyway.
--#
--# The performance indicators.
--#   Looking back to the crude diagram above, it may help to think of a plant (as in a tree,
--#   shrub, or vegetable) growing roots from the top left, trying to reach some water at the
--#   bottom right. Of course we are only really interested in the very first root to reach
--#   water, but we can't afford not to grow the others. One tendril might race through some 
--#   dry sand, but then it will hit a rock and end up going the long way round. As the search 
--#   continues, a few tiny feelers can develop into a mass of tangled roots:
--#
--#     Width is fairly obvious, it is how far we have thus far strayed from the most direct
--#           path, needless to say because differences have been encountered. Naturally, the 
--#           wider this is, the more things we have to examine every iteration, and we may
--#           experience exponential performance degradation.
--#     Stretch is the basic distance we have dug down, and the closest we have to a true
--#           progress indicator, but early performance is no guarantee of continued success.
--#     Complexity can be thought of as the total weight of the root ball. Once this exceeds
--#           say 10,000,000 then there may be severe memory shortages and disk thrashing.
--#     Edit Distance is the guaranteed minimum number of differences there will be between
--#           the two files when (if) the algorithm completes. (Or at least it was intended
--#           to be, if the first file is say 6 lines longer than the second and there are
--#           12 additional differences, it may start on -6 and end on +6, rather than start
--#           on +6 and end on +18. Even so it is still a valid "progress" indicator.)
--#
--#   It may seem like overkill to use four distinct measures of progress, but none is 
--#   reliable, and the performance overhead of maintaining these is completely insignificant
--#   when compared to the rest of the algorithm.
--#
--#   Of course I had to call it plan9 as it was my 9th attempt, albeit not from outer space.
--#   Identical files are recognised as such at least 95% as fast as a simple program  
--#   designed just to return a blunt yes/no would.
--#
--#   Officially, the raw performance or the algorithm used is worst case NP (size of larger 
--#   file times number of deletions) with expected time being N+PD (size of larger file plus
--#   [number of deletions times number of differences]). In other words pretty darn fast!
--#
--#   However, although the "raw" algorithm is very fast, the information required to
--#   return meaningful results causes severe performance penalties.
--#
--#   That hit is solely due to use of the variable "stack", which is my implementation
--#   and should not be attributed to the good work of Wu, Manber, and Myers.
--#
--#   You will get better performance by passing the larger file as the second parameter,
--#   although in practice the difference is pretty much neglible.
--#
--# Programming notes
--#
--# Be warned this is not a trivial problem!
--#
--# Reproduced below is a list of major advances in the field as reviewed by Claus Rick in 1994:
--# No. Year Author(s)          Time
--#  1  1974 Wagner, Fischer    O(mn)
--#  2  1977 Hunt, Szymanski    O(m + r log p) + O(n log s)
--#  3  1977 Hirschberg         O(pn) + O(n log s)
--#                             O((m-p) p log n) + O(n log s)
--#  4  1980 Masek, Paterson    O(n^2/log n)
--#  5  1982 Nakatsu et al.     O(n(m - p))
--#  6  1984 Hsu, Du            O(pm log( n/p ) + pm) + O(n log s)
--#  7  1986 Myers              O(n(n - p))
--#  8  1987 Apostolico, Guerra O(pm log(min{s; m; 2n/m})) + O(n log s)
--#                             O(m log n + d log( 2mn/d )) + O(n log s)
--#  9  1990 Chin, Poon         O(ns + min{ds; pm})
--# 10  1990 Wu, Manber, Myers  O(n + pd)
--# 11  1992 Apostolico et al.  O(n(m - p))
--#
--# More recent work seems to have been strongly biased towards DNA sequence matching.
--#
--# I have selected the Wu, Manber, and Myers approach as it is far simpler than Apostolico et al
--# and has not to my knowledge ever been shown to be worse (performancewise).
--# It is more than likely though that, by now, a (fractionally!!) better approach exists!
--#
--# Are we sitting comfortably? Then I'll begin:
--#
--# Imagine a grid where each element of a is placed down the left hand side and each element
--# of b is placed along the top. G[1,1] represents the "source" whereby no elements have been
--# inserted, deleted, or left alone, and G[m+1,n+1] represents the "sink", which when reached
--# indicates we have found the optimal solution (ie the minimum number of deletions and 
--# insertions required to convert a (of length m) into b (of length n).
--#
--# Note firstly that the optimal solution not only has the minumum number of (deletions plus
--# insertions), it also has the minimum number of deletions. This is a very key point!
--# Also, without loss of generality, assume n>=m, ie file b is same or larger than file a.
--# [the logic is easier to understand but codewise if n-m is negative it still works fine, bar
--# a few minor sequencing mods (all done through the variable "step").]
--#
--# Back to the grid G, let a horizontal line from G[i,j-1] to G[i,j] represent an insertion,
--# a diagonal G[i-1,j-1] to G[i,j] represent leaving the line alone (obviously only possible
--# if a[i]=b[j]), and a vertical line from G[i-1,j] to G[i,j] represent a deletion.
--#
--# You should now be able to imagine a wiggly line from the top left to the bottom right
--# representing a possible way to convert a into b. Down the left and along the bottom is one
--# overkill way: delete all of a then insert all of b (similarly for top & right edges).
--# If a and b are identical, G will be square and the optimal path a single straight line.
--#
--# The idea is to find [the] shortest route, which will have the fewest number of horizontal 
--# and vertical lines, and the maximum number of diagonal lines (remember: only available 
--# when a[i]=b[j]). [there may be several equivalently short routes].
--#
--# Bits of such an optimal path are *diagonals* running top left to bottom right.
--#
--# Further we have an edit cost at each G[i,j] which represents (so far) the least number of
--# insertions and deletions to reach that node.
--#
--# The 1974 algorithm calculated the edit cost for every node on the grid: O(m*n) performance.
--# In 1986 Myers refined this by only calculating a subset +/-d from the (0,0) diagonal,
--#   where d is the [unknown] quantity (insertions+deletions) on the whole optimal path
--# In 1989 Wu et al further reduced this to -p..n-m+p from the (0,0) diagonal,
--#   where p is the [unknown] quantity of deletions on the whole optimal path
--#   (and in so doing changed edit cost at G[i,j] to deletions only)
--#
--# The maths/logic/proof of this is non-trivial, but in laymans terms it actually makes sense,
--# in that any optimal path above the G[m,n] diagonal cannot be more than the [unknown] 
--# quantity p above it (obviously since that would require more than p deletions (which is
--# what p stands for) to reach G[m,n]);
--# and equally any optimal path more than p insertions below the G[0,0] diagonal would have
--# already required more than p deletions to get there, let alone G[m,n].
--#
--#
--# Is *ANYONE* still with me on this? ;-)
--#
--#
--# So, what we do is calculate the furthest point on each diagonal starting from G[0,0] with a 
--# maximum edit cost of p (the number of deletions), limiting the diagonals we are bothered 
--# with to the band {-p..m-n+p}; starting with an initial value of p at 0 (zero) and 
--# incrementing it for each iteration.
--#
--# within each iteration, we can find the value of p on diagonal k=i-j by using the high values 
--# of p-1 (or p) on diagonals k-1 and k+1 (p if we have just did that diagonal this iteration,
--# p-1 if it is next diagonal to be processed).
--#
--# It doesn't matter if we cannot proceed along diagonal k any further than one deletion from
--# k+1 or one insertion from diagonal k-1; that is now the lowest edit cost on diagonal k.
--# [Lemma that's non-obvious, I should add a guard for when that is not the case]
--#
--# The process terminates when the value for diagonal k=m-n reaches G[m,n].
--#
--# At each iteration, we have to record the x,y co-ordinates from either diagonal k+1 or k-1
--# that each result has been extrapolated from; hence when we reach G[m,n] we have a full set
--# of points on the minimum cost line from G[0,0} ro G[m,n}.
--#
--# Bright sparks might notice a distinct lack of edit cost array G[]. In fact keeping the  
--# furthest point fp[] on each diagonal for each value of p iterated through is enough;
--# G[] is not needed.
--#
--# Finally, by way of lame explanation, I quote "The correctness of the recurrence depends on
--# a proper treatment of the boundary cases p=0, k=-p, and k=m-n+p." In the main body this is
--# achieved by calculating from -p to -1, then p to 0. Actually, I'm not convinced either, 
--# but there it is.
--# It is clear, from experimentation, that the fact that a is NOT longer than b is important 
--# factor in this case, hence extra code (see variable "step") has been added to cope.
--#
--# Now that's all cleared up nicely (or not), Enjoy!
--#

sequence s1, s2 -- 'global' copies of parameters passed to diff
integer m, n -- lengths of s1, s2 respectively
integer delta -- n-m

sequence stack -- partial results, lots of them.
sequence used -- number of used entries in each stack[i]

--#
--# Some global variables for the progress window
--# 
global integer  width, maxwidth,        -- width of search band 
                stretch, maxstretch,    -- proximity to n
                complexity,             -- size of stack/number of active corners
                editdistance,           -- delta+2p
                terminate               -- cancel button pressed?


function snake(integer diag, integer lower, integer higher)
--
-- returns highest y (column) on diagonal with edit cost p.
-- programming note: the edit cost is increased to p by +1 in the
-- calling line on either the higher or lower parameter.
-- this routine does not know p, it just moves down the diagonal as
-- far as possible without adding any more cost.
-- It saves a stack of corner points needed to reach each max point
-- it finds on the diagonal.
--
integer x, y
integer sd, usd
--  complexity -= length(stack[diag])

    complexity -= used[diag]
    if lower>higher then
        y = lower
--      stack[diag] = stack[diag-1]
        sd = diag-1
    else
        y = higher
--      stack[diag] = stack[diag+1]
        sd = diag+1
    end if
    x = y-(diag-m-2)
    while x>=1 and x<=m 
      and y>=1 and y<=n 
      and compare(s1[x],s2[y])=0 do
        x += 1
        y += 1
        if terminate then return 0 end if
    end while
--  stack[diag] &= {x,y}
--  stack[diag] = stack[sd]&{x,y}
    usd = used[sd]
    if length(stack[diag])<=usd then
        stack[diag] = repeat(0,usd+32)
    end if
    stack[diag][1..usd] = stack[sd][1..usd]
--  stack[diag][usd+1] = x
--  stack[diag][usd+2] = y
--  used[diag] = usd+2
--  complexity += usd+2
    usd += 1
    stack[diag][usd] = x
    usd += 1
    stack[diag][usd] = y
    used[diag] = usd
    complexity += usd
    if y>stretch then
        stretch = y
    end if
    return y
end function 

global function diff(sequence a, sequence b) 
-------------------------------------
sequence fp -- furthest points reachable on each diagonal at cost p
integer p -- number of deletions
integer step -- things need to be done backwards if a is longer than b

    s1 = a  s2 = b  --# kinda need them global
    m = length(a)
    n = length(b)
    delta = n-m
    step = 1
    if delta<0 then step = -1 end if -- length(a) > length(b)
    width = 0
    maxwidth = (m+n-delta)/2
    stretch = 0
    maxstretch = n
    complexity = 0
    editdistance = delta
    terminate = 0
    fp = repeat(0,m+n+3) -- offset is m+2
    stack = repeat({},m+n+3)
    used = repeat(0,m+n+3)
    p = -1
    while fp[m+2+delta]!= n+1 do
        p += 1
        for k=m+2-p*step to m+2+delta-step by step do
--          doEvents(0) -- allow cancel button to be pressed
--          if terminate then exit end if
            fp[k] = snake(k,fp[k-1]+1,fp[k+1])
        end for
        for k=m+2+delta+p*step to m+2+delta by -step do
--          doEvents(0) -- allow cancel button to be pressed
--          if terminate then exit end if
            fp[k] = snake(k,fp[k-1]+1,fp[k+1])
        end for
        width = p
        editdistance = delta+2*p
        doEvents(0) -- allow cancel button to be pressed
        if terminate then exit end if
    end while
    s1 = {}  s2 = {} -- release memory
    fp = stack[m+2+delta][1..used[m+2+delta]]
    stack = {} -- release memory
    used = {}
    return fp -- stack[m+2+delta]
end function
