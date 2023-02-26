--
-- demo\rosetta\turtle_projection.e
-- ================================
--
--  3D-specific projection, copied from demo/PGUI/rubik.e
--
enum X, Y, Z

global sequence view_rotations = {}

constant naxes = {{Y,Z},    -- (rotate about the X-axis)
                  {X,Z},    -- (rotate about the Y-axis)
                  {X,Y}}    -- (rotate about the Z-axis)

function rotate(sequence points, atom angle, integer axis)
--
-- rotate points by the specified angle about the given axis
--
    atom radians = angle*CD_DEG2RAD,
         sin_r = sin(radians),
         cos_r = cos(radians)
    integer {nx,ny} = naxes[axis]
    for i=1 to length(points) do
        atom x = points[i][nx],
             y = points[i][ny]
        points[i][nx] = x*cos_r - y*sin_r
        points[i][ny] = y*cos_r + x*sin_r
    end for
    return points
end function

function projection(sequence points, atom d)
--
-- project points from {0,0,d} onto the perpendicular plane through {0,0,0}
--
    for i=1 to length(points) do
        atom {x,y,z} = points[i],
             denom = (1-z/d)
        points[i][X] = x/denom
        points[i][Y] = y/denom
    end for
    return points
end function

global function rotate_and_project(sequence points)
    points = deep_copy(points)
    for i=1 to length(view_rotations) do
        {integer axis, atom angle} = view_rotations[i]
        points = rotate(points,angle,axis)
    end for
    points = projection(points,1000)
    return points
end function


