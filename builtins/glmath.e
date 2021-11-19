--
-- demo\pGUI\glmath.e
-- ==================
--
 /**
   * Computes a 4-by-4 perspective transformation matrix given the angular height
   * of the frustum, the aspect ratio, and the near and far clipping planes.  The
   * arguments define a frustum extending in the negative z direction.  The given
   * angle is the vertical angle of the frustum, and the horizontal angle is
   * determined to produce the given aspect ratio.  The arguments near and far are
   * the distances to the near and far clipping planes.  Note that near and far
   * are not z coordinates, but rather they are distances along the negative
   * z-axis.  The matrix generated sends the viewing frustum to the unit box.
   * We assume a unit box extending from -1 to 1 in the x and y dimensions and
   * from -1 to 1 in the z dimension.
   * @param {number} fieldOfViewInRadians field of view in y axis.
   * @param {number} aspect aspect of viewport (width / height)
   * @param {number} near near Z clipping plane
   * @param {number} far far Z clipping plane
   * @return {new Matrix4}
   */
global function m4_perspective(atom fieldOfViewInRadians, aspect, near, far)
    atom f = tan(PI*0.5 - 0.5*fieldOfViewInRadians),
        rangeInv = 1.0/(near-far),
        a = f/aspect, 
        b = (near+far)*rangeInv,
        c = near*far*rangeInv*2
    sequence dst = {a,0,0,0,
                    0,f,0,0,
                    0,0,b,-1,
                    0,0,c,0}
    return dst
end function

  /**
   * normalizes a vector.
   * @param {Vector3} v vector to normalize
   * @return {new Vector3}
   */
function m4_normalize(sequence v)
    atom {v1,v2,v3} = v,
         l = sqrt(v1*v1+v2*v2+v3*v3)
    if l>0.00001 then   // make sure we don't divide by 0.
        v = {v1/l,v2/l,v3/l}
    end if
    return v
end function

  /**
   * subtracts 2 vector3s
   * @param {Vector3} a a
   * @param {Vector3} b b
   * @return {new Vector3}
   */
function m4_subtractVectors(sequence a, b)
    atom {a1,a2,a3} = a,
         {b1,b2,b3} = b
    sequence res = m4_normalize({a1-b1,
                                 a2-b2,
                                 a3-b3})
    return res
end function

  /**
   * Computes the cross product of 2 vector3s
   * @param {Vector3} a a
   * @param {Vector3} b b
   * @return {new Vector3}
   */
function m4_crossProduct(sequence a, b)
    atom {a1,a2,a3} = a,
         {b1,b2,b3} = b
    sequence res = m4_normalize({a2*b3-a3*b2,
                                 a3*b1-a1*b3,
                                 a1*b2-a2*b1})
    return res
end function

  /**
   * Creates a lookAt matrix.
   * This is a world matrix for a camera. In other words it will transform
   * from the origin to a place and orientation in the world. For a view
   * matrix take the inverse of this.
   * @param {Vector3} cPosn position of the camera
   * @param {Vector3} tgt position of the target
   * @param {Vector3} up direction
   * @return {new Matrix4}
   */
global function m4_lookAt(sequence cPosn, tgt, up)
    sequence zAxis = m4_subtractVectors(cPosn, tgt),
             xAxis = m4_crossProduct(up, zAxis),
             yAxis = m4_crossProduct(zAxis, xAxis)
    atom {x1,x2,x3} = xAxis,
         {y1,y2,y3} = yAxis,
         {z1,z2,z3} = zAxis,
         {c1,c2,c3} = cPosn
    sequence dst = {x1,x2,x3,0,
                    y1,y2,y3,0,
                    z1,z2,z3,0,
                    c1,c2,c3,1}
    return dst
end function

  /**
   * Computes the inverse of a matrix.
   * @param {Matrix4} m matrix to compute inverse of
   * @param {Matrix4} [dst] optional matrix to store result
   * @return {Matrix4} dst or a new matrix if none provided
   */
//DEV there's gotta be a better way than this! [or maybe there really isn't!]
global function m4_inverse(sequence m)

    atom {m11,m12,m13,m14,
          m21,m22,m23,m24,
          m31,m32,m33,m34,
          m41,m42,m43,m44} = m,

        // 34 temps, save some multiplications, why not

        m3344 = m33*m44,
        m4334 = m43*m34,
        m2344 = m23*m44,
        m4324 = m43*m24,
        m2334 = m23*m34,
        m3324 = m33*m24,
        m1344 = m13*m44,
        m4314 = m43*m14,
        m1334 = m13*m34,
        m3314 = m33*m14,
        m1324 = m13*m24,
        m2314 = m23*m14,
        m3142 = m31*m42,
        m4132 = m41*m32,
        m2142 = m21*m42,
        m4122 = m41*m22,
        m2132 = m21*m32,
        m3122 = m31*m22,
        m1142 = m11*m42,
        m4112 = m41*m12,
        m1132 = m11*m32,
        m3112 = m31*m12,
        m1122 = m11*m22,
        m2112 = m21*m12,

        d11 = (m3344*m22 + m4324*m32 + m2334*m42)
            - (m4334*m22 + m2344*m32 + m3324*m42),
        d12 = (m4334*m12 + m1344*m32 + m3314*m42)
            - (m3344*m12 + m4314*m32 + m1334*m42),
        d13 = (m2344*m12 + m4314*m22 + m1324*m42)
            - (m4324*m12 + m1344*m22 + m2314*m42),
        d14 = (m3324*m12 + m1334*m22 + m2314*m32)
            - (m2334*m12 + m3314*m22 + m1324*m32),
        d21 = (m4334*m21 + m2344*m31 + m3324*m41)
            - (m3344*m21 + m4324*m31 + m2334*m41),
        d22 = (m3344*m11 + m4314*m31 + m1334*m41)
            - (m4334*m11 + m1344*m31 + m3314*m41),
        d23 = (m4324*m11 + m1344*m21 + m2314*m41)
            - (m2344*m11 + m4314*m21 + m1324*m41),
        d24 = (m2334*m11 + m3314*m21 + m1324*m31)
            - (m3324*m11 + m1334*m21 + m2314*m31),
        d31 = (m3142*m24 + m4122*m34 + m2132*m44)
            - (m4132*m24 + m2142*m34 + m3122*m44),
        d32 = (m4132*m14 + m1142*m34 + m3112*m44)
            - (m3142*m14 + m4112*m34 + m1132*m44),
        d33 = (m2142*m14 + m4112*m24 + m1122*m44)
            - (m4122*m14 + m1142*m24 + m2112*m44),
        d34 = (m3122*m14 + m1132*m24 + m2112*m34)
            - (m2132*m14 + m3112*m24 + m1122*m34),
        d41 = (m2142*m33 + m3122*m43 + m4132*m23)
            - (m2132*m43 + m3142*m23 + m4122*m33),
        d42 = (m1132*m43 + m3142*m13 + m4112*m33)
            - (m1142*m33 + m3112*m43 + m4132*m13),
        d43 = (m1142*m23 + m2112*m43 + m4122*m13)
            - (m1122*m43 + m2142*m13 + m4112*m23),
        d44 = (m1122*m33 + m2132*m13 + m3112*m23)
            - (m1132*m23 + m2112*m33 + m3122*m13),

        d = 1.0 / (m11*d11 +
                   m21*d12 +
                   m31*d13 +
                   m41*d14)

    sequence dst = {d*d11,d*d12,d*d13,d*d14,
                    d*d21,d*d22,d*d23,d*d24,
                    d*d31,d*d32,d*d33,d*d34,
                    d*d41,d*d42,d*d43,d*d44}

    return dst
end function

//http://www.java2s.com/ref/javascript/javascript-array-invertmatrix-matrix.html
//just as an experiment, this works fine too:
/*
  function inverse2(_A) {
    var temp,
//  N = _A.length,
    N = 4,
    E = [];
   
    for (var i = 0; i < N; i++)
      E[i] = [];
   
    for (i = 0; i < N; i++)
      for (var j = 0; j < N; j++) {
        E[i][j] = 0;
        if (i == j)
          E[i][j] = 1;
      }
   
    for (var k = 0; k < N; k++) {
//    temp = _A[k][k];
      temp = _A[k*4+k];
   
      for (var j = 0; j < N; j++)
      {
//      _A[k][j] /= temp;
        _A[k*4+j] /= temp;
        E[k][j] /= temp;
      }
   
      for (var i = k + 1; i < N; i++)
      {
//      temp = _A[i][k];
        temp = _A[i*4+k];
   
        for (var j = 0; j < N; j++)
        {
//        _A[i][j] -= _A[k][j] * temp;
          _A[i*4+j] -= _A[k*4+j] * temp;
          E[i][j] -= E[k][j] * temp;
        }
      }
    }
   
    for (var k = N - 1; k > 0; k--)
    {
      for (var i = k - 1; i >= 0; i--)
      {
//      temp = _A[i][k];
        temp = _A[i*4+k];
   
        for (var j = 0; j < N; j++)
        {
//        _A[i][j] -= _A[k][j] * temp;
          _A[i*4+j] -= _A[k*4+j] * temp;
          E[i][j] -= E[k][j] * temp;
        }
      }
    }
   
    for (var i = 0; i < N; i++)
      for (var j = 0; j < N; j++)
//      _A[i][j] = E[i][j];
        _A[i*4+j] = E[i][j];
    return _A;
  }

I translated it to Phix, got problems...
function inverse2(sequence A)
    A = deep_copy(A)
    atom temp
    integer N = sqrt(length(A)), kN, iN
    sequence E = repeat(repeat(0,N),N)
    for i=1 to N do
        E[i][i] = 1
    end for
    kN = 0
    for k=1 to N do
        temp = A[kN+k];
--if temp!=0 then -- not the answer!
        for j=1 to N do
--Ah, occasional /0 here that m4_inverse does not seem to suffer from...
--  (eg s = {1,2,3,1,2,4,1,3,2,8,7,11,11,6,2,5})
            A[kN+j] /= temp
            E[k][j] /= temp;
        end for
--end if
        for i=k+1 to N do
            iN = (i-1)*N
            temp = A[iN+k]
            for j=1 to N do
                A[iN+j] -= A[kN+j] * temp;
                E[i][j] -= E[k][j] * temp;
            end for
        end for
        kN += N
    end for
    for k=N to 2 by -1 do
        kN = (k-1)*N
        for i=k-1 to 1 by -1 do
            iN = (i-1)*N
            temp = A[iN+k];
            for j=1 to N do
                A[iN+j] -= A[kN+j] * temp;
                E[i][j] -= E[k][j] * temp;
            end for
        end for
    end for
    for i=1 to N do
        iN = (i-1)*N
        for j=1 to N do
            A[iN+j] = E[i][j];
        end for
    end for
    return A;
end function

sequence s = sq_rand(tagset(16))
--s = {1,1,1,1,5,1,2,2,2,2,9,11,7,6,9,15}
?m4_inverse(s)
?inverse2(s)

*/

  /**
   * Takes two 4-by-4 matrices, a and b, and computes the product in the order
   * that pre-composes b with a.  In other words, the matrix returned will
   * transform by b first and then a.  Note this is subtly different from just
   * multiplying the matrices together.  For given a and b, this function returns
   * the same object in both row-major and column-major mode.
   * @param {Matrix4} a A matrix.
   * @param {Matrix4} b A matrix.
   * @return {new Matrix4}
   */
global function m4_multiply(sequence a, b)

    atom {a11,a12,a13,a14,
          a21,a22,a23,a24,
          a31,a32,a33,a34,
          a41,a42,a43,a44} = a,

         {b11,b12,b13,b14,
          b21,b22,b23,b24,
          b31,b32,b33,b34,
          b41,b42,b43,b44} = b,

         d11 = b11*a11 + b12*a21 + b13*a31 + b14*a41,
         d12 = b11*a12 + b12*a22 + b13*a32 + b14*a42,
         d13 = b11*a13 + b12*a23 + b13*a33 + b14*a43,
         d14 = b11*a14 + b12*a24 + b13*a34 + b14*a44,
         d21 = b21*a11 + b22*a21 + b23*a31 + b24*a41,
         d22 = b21*a12 + b22*a22 + b23*a32 + b24*a42,
         d23 = b21*a13 + b22*a23 + b23*a33 + b24*a43,
         d24 = b21*a14 + b22*a24 + b23*a34 + b24*a44,
         d31 = b31*a11 + b32*a21 + b33*a31 + b34*a41,
         d32 = b31*a12 + b32*a22 + b33*a32 + b34*a42,
         d33 = b31*a13 + b32*a23 + b33*a33 + b34*a43,
         d34 = b31*a14 + b32*a24 + b33*a34 + b34*a44,
         d41 = b41*a11 + b42*a21 + b43*a31 + b44*a41,
         d42 = b41*a12 + b42*a22 + b43*a32 + b44*a42,
         d43 = b41*a13 + b42*a23 + b43*a33 + b44*a43,
         d44 = b41*a14 + b42*a24 + b43*a34 + b44*a44

    sequence dst = {d11,d12,d13,d14,
                    d21,d22,d23,d24,
                    d31,d32,d33,d34,
                    d41,d42,d43,d44}
    return dst
end function

  /**
   * Multiply by an x rotation matrix
   * @param {Matrix4} m matrix to multiply
   * @param {number} angleInRadians amount to rotate
   * @return {new Matrix4}
   */
global function m4_xRotate(sequence m, atom angleInRadians)

    // this is the optimized version of
    // return multiply(m, xRotation(angleInRadians), dst);

    atom c = cos(angleInRadians),
         s = sin(angleInRadians),

        {m11,m12,m13,m14,
         m21,m22,m23,m24,
         m31,m32,m33,m34,
         m41,m42,m43,m44} = m,

        d21 = c*m21 + s*m31,
        d22 = c*m22 + s*m32,
        d23 = c*m23 + s*m33,
        d24 = c*m24 + s*m34,
        d31 = c*m31 - s*m21,
        d32 = c*m32 - s*m22,
        d33 = c*m33 - s*m23,
        d34 = c*m34 - s*m24

    sequence dst = {m11,m12,m13,m14,
                    d21,d22,d23,d24,
                    d31,d32,d33,d34,
                    m41,m42,m43,m44}
    return dst
end function

  /**
   * Multiply by an y rotation matrix
   * @param {Matrix4} m matrix to multiply
   * @param {number} angleInRadians amount to rotate
   * @return {new Matrix4}
   */
global function m4_yRotate(sequence m, atom angleInRadians)
    // this is the optimized version of
    // return multiply(m, yRotation(angleInRadians), dst);

    atom c = cos(angleInRadians),
         s = sin(angleInRadians),

        {m11,m12,m13,m14,
         m21,m22,m23,m24,
         m31,m32,m33,m34,
         m41,m42,m43,m44} = m,

        d11 = c*m11 - s*m31,
        d12 = c*m12 - s*m32,
        d13 = c*m13 - s*m33,
        d14 = c*m14 - s*m34,

        d31 = c*m31 + s*m11,
        d32 = c*m32 + s*m12,
        d33 = c*m33 + s*m13,
        d34 = c*m34 + s*m14

    sequence dst = {d11,d12,d13,d14,
                    m21,m22,m23,m24,
                    d31,d32,d33,d34,
                    m41,m42,m43,m44}
    return dst
end function

--  function radToDeg(r) {
--  return r * 180 / Math.PI;
--  }

--  function degToRad(d) {
--  return d * Math.PI / 180;
--  }

-- see also (maybe) E:\downloads\misc\js\svg\webgl\3Dmath.ew
