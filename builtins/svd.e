--
-- builtins\svd.e
-- ==============
--  Singulare value decomposition
--  Translated from http://stitchpanorama.sourceforge.net/Python/svd.py
--
function pythag(atom a,b)
    atom absa = abs(a),
         absb = abs(b)
    if absa > absb then
        return absa*sqrt(1.0+power(absb/absa,2))
    elsif absb == 0.0 then
        return 0.0
    else
        return absb*sqrt(1.0+power(absa/absb,2))
    end if
end function

global function svd(sequence a)
    -- Compute the singular value decomposition of array a
    -- Golub and Reinsch state that eps should not be smaller than the
    -- machine precision, ie the smallest number
    -- for which 1+e>1. tol should be beta/e where beta is the smallest
    -- positive number representable in the computer.
    atom eps = 1e-15,  -- assumes double precision
         tol = 1e-64/eps
    assert(1.0+eps > 1.0) -- if this fails, make eps bigger
    assert(tol > 0.0)    -- if this fails, make tol bigger
    integer itmax = 50
    sequence u = deep_copy(a)
    integer m = length(a),
            n = length(a[1]), l, l1
    --if __debug__: print 'a is ',m,' by ',n

    if m < n then
--      if __debug__: print 'Error: m is less than n'
        crash("SVD Error: m(%d) is less than n(%d)",{m,n})
    end if
    sequence e = repeat(0,n),  -- allocate arrays
             q = repeat(0,n),
             v = repeat(repeat(0,n),n)
--  for k in range(n): v.append([0.0]*n)
 
    -- Householder's reduction to bidiagonal form

    atom g = 0.0,
         x = 0.0,
         c, z, h, s, f, y

    for i=1 to n do
        e[i] = g
        s = 0.0
        l = i+1
        for j=i to m do s += (u[j][i]*u[j][i]) end for
--?s -- ** 25,0.251 vs py 25,9
        if s <= tol then
            g = 0.0
        else
            f = u[i][i]
            if f < 0.0 then
                g = sqrt(s)
            else
                g = -sqrt(s)
            end if
            h = f*g-s
            u[i][i] = f-g
            for j=l to n do
                s = 0.0
                for k=i to m do s += u[k][i]*u[k][j] end for
                f = s/h
                for k=i to m do u[k][j] += f*u[k][i] end for
            end for
        end if
        q[i] = g
        s = 0.0
        for j=l to n do s += u[i][j]*u[i][j] end for
        if s <= tol then
            g = 0.0
        else
            f = u[i][i+1]
            if f < 0.0 then
                g = sqrt(s)
            else
                g = -sqrt(s)
            end if
            h = f*g - s
            u[i][i+1] = f-g
            for j=l to n do e[j] = u[i][j]/h end for
            for j=l to m do
                s=0.0
                for k=l to n do s += (u[j][k]*u[i][k]) end for
                for k=l to n do u[j][k] = u[j][k]+(s*e[k]) end for
            end for
        end if
        y = abs(q[i])+abs(e[i])
        if y>x then x=y end if
--?e
--?q
--?v
--?u
--?"==="
    end for
    -- accumulation of right hand gtransformations
--  for i in range(n-1,-1,-1):
--  for i=n-1 to -1 by -1 do    -- (untested)
    for i=n to 1 by -1 do
        if g != 0.0 then
            h = g*u[i][i+1]
            for j=l to n do v[j][i] = u[i][j]/h end for
            for j=l to n do
                s = 0.0
                for k=l to n do s += (u[i][k]*v[k][j]) end for
                for k=l to n do v[k][j] += (s*v[k][i]) end for
            end for
        end if
        for j=l to n do
            v[i][j] = 0.0
            v[j][i] = 0.0
        end for
        v[i][i] = 1.0
        g = e[i]
        l = i
    end for
    --accumulation of left hand transformations
--  for i in range(n-1,-1,-1):
    for i=n to 1 by -1 do
        l = i+1
        g = q[i]
        for j=l to n do u[i][j] = 0.0 end for
        if g != 0.0 then
            h = u[i][i]*g
            for j=l to n do
                s = 0.0
                for k=l to m do s += (u[k][i]*u[k][j]) end for
                f = s/h
                for k=i to m do u[k][j] += (f*u[k][i]) end for
            end for
            for j=i to m do u[j][i] = u[j][i]/g end for
        else
            for j=i to m do u[j][i] = 0.0 end for
        end if
        u[i][i] += 1.0
    end for
    -- diagonalization of the bidiagonal form
    eps = eps*x
--  for k in range(n-1,-1,-1):
    for k=n to 1 by -1 do
        for iteration=1 to itmax do
            -- test f splitting
--          for l in range(k,-1,-1):
            bool goto_test_f_convergence
--          for l=k+1 to 1 by -1 do
            for l=k to 1 by -1 do
                goto_test_f_convergence = false
                if abs(e[l]) <= eps then
                    -- goto test f convergence
                    goto_test_f_convergence = true
                    exit  -- break out of l loop
                end if
                if abs(q[l-1]) <= eps then
                    -- goto cancellation
                    exit  -- break out of l loop
                end if
            end for
            if not goto_test_f_convergence then
                -- cancellation of e[l] if l>0
                c = 0.0
                s = 1.0
                l1 = l-1
--              for i in range(l,k+1):
                for i=1 to k do
                    f = s*e[i]
                    e[i] = c*e[i]
                    if abs(f) <= eps then
                        -- goto test f convergence
                        exit
                    end if
                    g = q[i]
                    h = pythag(f,g)
                    q[i] = h
                    c = g/h
                    s = -f/h
--                  for j in range(m):
                    for j=1 to m do
                        y = u[j][l1]
                        z = u[j][i]
                        u[j][l1] = y*c+z*s
                        u[j][i] = -y*s+z*c
                    end for
                end for
            end if
            -- test f convergence
            z = q[k]
            if l == k then
                -- convergence
                if z<0.0 then
                    -- q[k] is made non-negative
                    q[k] = -z
                    for j=1 to n do
                        v[j][k] = -v[j][k]
                    end for
                end if
                exit  -- break out of iteration loop and move on to next k value
            end if
            if iteration >= itmax-1 then
--              if __debug__: print 'Error: no convergence.'
                -- should this move on the the next k or exit with error??
                --raise ValueError,'SVD Error: No convergence.' -- exit the program with error
                exit  -- break out of iteration loop and move on to next k
            end if
            -- shift from bottom 2x2 minor
            x = q[l]
            y = q[k-1]
            g = e[k-1]
            h = e[k]
            f = ((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y)
            g = pythag(f,1.0)
            if f < 0 then
                f = ((x-z)*(x+z)+h*(y/(f-g)-h))/x
            else
                f = ((x-z)*(x+z)+h*(y/(f+g)-h))/x
            end if
            -- next QR transformation
            c = 1.0
            s = 1.0
--          for i in range(l+1,k+1):
            for i=l+1 to k do
                g = e[i]
                y = q[i]
                h = s*g
                g = c*g
                z = pythag(f,h)
                e[i-1] = z
                c = f/z
                s = h/z
                f = x*c+g*s
                g = -x*s+g*c
                h = y*s
                y = y*c
--              for j in range(n):
                for j=1 to n do
                    x = v[j][i-1]
                    z = v[j][i]
                    v[j][i-1] = x*c+z*s
                    v[j][i] = -x*s+z*c
                end for
                z = pythag(f,h)
                q[i-1] = z
                c = f/z
                s = h/z
                f = c*g+s*y
                x = -s*g+c*y
                for j=1 to m do
                    y = u[j][i-1]
                    z = u[j][i]
                    u[j][i-1] = y*c+z*s
                    u[j][i] = -y*s+z*c
                end for
            end for
            e[l] = 0.0
            e[k] = f
            q[k] = x
            -- goto test f splitting
        end for
    end for
            
    --vt = transpose(v)
    --return (u,q,vt)
    return {u,q,v}
end function

global function matrix_transpose(sequence a)
--  Compute the transpose of a matrix
    integer m = length(a),
            n = length(a[1])
    sequence at = repeat(repeat(0,m),n)
    for i=1 to m do
        for j=1 to n do
            at[j][i]=a[i][j]
        end for
    end for
    return at
end function

global function matrix_multiply(sequence a, b)
--  Multiply two matrices.
--  a must be two dimensional
--  b can be one or two dimensional
--  
    bool d2 = sequence(b[1])
    integer am = length(a), cm = am,
            an = length(a[1]),
            bm = length(b),
            bn = iff(d2?length(b[1]):1), cn = bn
--  try:
--      bn = len(b[0])
--  except TypeError:
--      bn = 1
    if an != bm then
        crash("matrix_multiply error: array sizes do not match")
    end if
    sequence c = iff(d2?repeat(repeat(0,cn),cm):repeat(0,cm))
--  cm = am
--  cn = bn
--  if bn == 1:
--      c = [0.0]*cm
--  else:
--      c = []
--      for k in range(cm): c.append([0.0]*cn)
    for i=1 to cm do
        for j=1 to cn do
            for k=1 to an do
                if d2 then
                    c[i][j] += a[i][k]*b[k][j]
                else 
                    c[i] += a[i][k]*b[k]
                end if
            end for
        end for
    end for
    return c
end function
 
--/*
--a = [[22.,10., 2.,  3., 7.],
--   [14., 7.,10.,  0., 8.],
--   [-1.,13.,-1.,-11., 3.],
--   [-3.,-2.,13., -2., 4.],
--   [ 9., 8., 1., -2., 4.],
--   [ 9., 1.,-7.,  5.,-1.],
--   [ 2.,-6., 6.,  5., 1.],
--   [ 4., 5., 0., -2., 2.]]
sequence a = {{3,0},
              {4,5}}
--
-- import svd
-- import math
sequence {u,w,vt} = svd(a)
?w
--
-- [35.327043465311384, 1.2982256062667619e-15,
--  19.999999999999996, 19.595917942265423, 0.0]
--
-- the correct answer is (the order may vary)
--
--print (math.sqrt(1248.),20.,math.sqrt(384.),0.,0.)
--
-- (35.327043465311391, 20.0, 19.595917942265423, 0.0, 0.0)
?u
?vt
--*/

--me:
--{2.236067977,6.708203932}
--{{0.9486832981,0.316227766},{-0.316227766,0.9486832981}}
--{{0.7071067812,0.7071067812},{-0.7071067812,0.7071067812}}
--"0.0s"

--  C:\Program Files (x86)\Phix>py27 svd.py
--  [2.23606797749979, 6.708203932499368]
--  [[0.9486832980505138, 0.316227766016838], [-0.3162277660168379, 0.9486832980505138]]
--  [[0.7071067811865475, 0.7071067811865475], [-0.7071067811865475, 0.7071067811865475]]

--  Sample Output
--     
--  0.31622776601683794 -0.9486832980505138
--  0.9486832980505138 0.31622776601683794
--
--  6.708203932499369 0
--  0 2.23606797749979
--
--  0.7071067811865475 -0.7071067811865475
--  0.7071067811865475 0.7071067811865475

