--
-- builtins\matrix.e
--
-- proposed addition, not documented or tested or in phix7zip.lst as yet...
-- see also builtins\glmath.e [erm, maybe...]
--

-- from/see/update https://rosettacode.org/wiki/Matrix_multiplication#Phix
function matrix_mul(sequence a, b)
    integer {arows, acols, brows, bcols} =
          apply({a, a[1],  b, b[1]}, length)
    if acols!=brows then return 0 end if
    sequence c = repeat(repeat(0,bcols),arows)
    for i=1 to arows do
        for j=1 to bcols do
            for k=1 to acols do
                c[i][j] += a[i][k]*b[k][j]
            end for
        end for
    end for
    return c
end function

-- from/see/update https://rosettacode.org/wiki/Matrix_transposition#Phix
global function matrix_transpose(sequence mat)
    integer rows = length(mat),
            cols = length(mat[1])
    sequence res = repeat(repeat(0,rows),cols)
    for r=1 to rows do
        for c=1 to cols do
            res[c][r] = mat[r][c]
        end for
    end for
    return res
end function

