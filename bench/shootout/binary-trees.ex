-- The Computer Language Shootout Benchmarks
--  http://shootout.alioth.debian.org/
-- 
--  Converted to Euphoria by Jason Gade
--  run: exu binary-trees.ex [N=0]
--/**/with console

without warning
without type_check

constant displayResults=0

constant LEFT =  1,
         RIGHT = 2,
         ITEM =  3
         
constant NULL = {} 



function ItemCheck(sequence tree)

    if equal(tree[LEFT], NULL) then 
        return tree[ITEM]
    else 
        return tree[ITEM] + ItemCheck(tree[LEFT]) - ItemCheck(tree[RIGHT])

    end if

end function -- ItemCheck



function BottomUpTree(atom item, integer depth)

    if depth > 0 then
        return {BottomUpTree(2 * item - 1, depth - 1), 
                BottomUpTree(2 * item, depth - 1),
                item}
    else
        return {NULL, NULL, item}
    
    end if
end function -- BottomUpTree



procedure main(atom N, sequence expected)

    atom iterations, check
    integer minDepth, maxDepth, stretchDepth
    sequence stretchTree, longLivedTree, tempTree
    
    minDepth = 4

    if (minDepth + 2) > N then
        maxDepth = minDepth + 2
    else
        maxDepth = N
    end if

    stretchDepth = maxDepth + 1

    stretchTree = BottomUpTree(0, stretchDepth)
    if stretchDepth!=expected[1] then ?9/0 end if
    if ItemCheck(stretchTree)!=-1 then ?9/0 end if
if displayResults then
    printf(1, "stretch tree of depth %d\t  check: %d\n",
              {stretchDepth,
              ItemCheck(stretchTree)})
end if

    stretchTree = {} 

    longLivedTree = BottomUpTree(0, maxDepth)

    for depth = minDepth to maxDepth by 2 do

       iterations = power(2, maxDepth - depth + minDepth)

       check = 0

       for i = 1 to iterations do

           tempTree = BottomUpTree(i, depth)
           check += ItemCheck(tempTree)
           tempTree = {}

           tempTree = BottomUpTree(-i, depth)
           check += ItemCheck(tempTree)
           tempTree = {}

       end for -- i

if depth=minDepth then
    if iterations*2!=expected[2] then ?9/0 end if
    if depth!=expected[3] then ?9/0 end if
elsif depth=minDepth+2 then
    if iterations*2!=expected[4] then ?9/0 end if
    if depth!=expected[5] then ?9/0 end if
elsif depth=minDepth+4 then
    if iterations*2!=expected[6] then ?9/0 end if
    if depth!=expected[7] then ?9/0 end if
elsif depth=minDepth+8 then
    if iterations*2!=expected[8] then ?9/0 end if
    if depth!=expected[9] then ?9/0 end if
end if
    if check!=iterations*-2 then ?9/0 end if
if displayResults then
        printf(1, "%d\t  trees of depth %d\t  check: %d\n",
                  {iterations * 2, depth, check })
end if
    end for -- depth

    if maxDepth!=expected[10] then ?9/0 end if
    if ItemCheck(longLivedTree)!=-1 then ?9/0 end if
if displayResults then
    printf(1, "long lived tree of depth %d\t  check: %d\n",
              {maxDepth, ItemCheck(longLivedTree)})
end if

end procedure -- main

for i=1 to 10 do
    --N = 0..6:
    main(6,{7,128,4,32,6,0,0,0,0,6})
    --stretch tree of depth 7   check: -1
    --128     trees of depth 4        check: -128
    --32          trees of depth 6        check: -32
    --long lived tree of depth 6          check: -1
    --N = 7:
    main(7,{8,256,4,64,6,0,0,0,0,7})
    --stretch tree of depth 8   check: -1
    --256     trees of depth 4        check: -256
    --64          trees of depth 6        check: -64
    --long lived tree of depth 7          check: -1
    --N = 8:
    main(8,{9,512,4,128,6,32,8,0,0,8})
    --stretch tree of depth 9   check: -1
    --512     trees of depth 4        check: -512
    --128     trees of depth 6        check: -128
    --32          trees of depth 8        check: -32
    --long lived tree of depth 8          check: -1
    --N = 9:
    main(9,{10,1024,4,256,6,64,8,0,0,9})
    --stretch tree of depth 10        check: -1
    --1024    trees of depth 4        check: -1024
    --256     trees of depth 6        check: -256
    --64          trees of depth 8        check: -64
    --long lived tree of depth 9          check: -1
    --N = 9:
    main(10,{11,2048,4,512,6,128,8,32,10,10})
    --stretch tree of depth 11        check: -1
    --2048    trees of depth 4        check: -2048
    --512     trees of depth 6        check: -512
    --128     trees of depth 8        check: -128
    --32          trees of depth 10       check: -32
    --long lived tree of depth 10     check: -1
end for

