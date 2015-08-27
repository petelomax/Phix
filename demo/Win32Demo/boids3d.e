-- boid.exw
include misc.e

global integer END_TIME, BOIDS, OBSTACLES
global atom N_DIST, DIST, X_MAX, Y_MAX, V_MAX, V_MIN, DIST_FACTOR, X_MIN, Y_MIN, Z_MIN, Z_MAX
sequence BOIDS0
OBSTACLES = 3
END_TIME = 1000
BOIDS = 60
N_DIST = 75.0
DIST = 30.0
X_MIN = 0.0
Y_MIN = 0.0
Z_MIN = 0.0
X_MAX = 600.0
Y_MAX = 600.0
Z_MAX = 600.0
V_MAX = 10.0
V_MIN = 4.0
DIST_FACTOR = 1.0/100.0

global constant
B_X = 1,
B_Y = 2,
B_Z = 3,
B_XV = 4,
B_YV = 5,
B_ZV = 6,
B_ELEMENTS = 6,
O_X = 1,
O_Y = 2,
O_Z = 3,
O_R = 4


global sequence boidsn, boidsnp1, goal, obstacles

-- rotates a vector in 2D
global function rotate(sequence v, atom rad)
atom c, s
    c = cos(rad)
    s = sin(rad)

    return {c*v[1]-s*v[2], s*v[1]+c*v[2]}
end function

-- return the scalar magnitude of a 2D vector
global function magnitude2(sequence v)
    return sqrt(v[1]*v[1]+v[2]*v[2])
end function

-- return the scalar magnitude of a 3D vector
global function magnitude3(sequence v)
    return sqrt(v[1]*v[1]+v[2]*v[2]+v[3]*v[3])
end function

-- change the scalar magnitude of a 3D vector
global function make_length(sequence v, atom l)
atom mag
        mag = magnitude3(v)
--/**/  return sq_mul(v,l/mag)  --/* -- Phix
        return v*( l/mag )      --*/ -- RDS
end function

-- return the dot product of 2 2D vectors
global function dot(sequence u, sequence v)
    return u[1]*v[1]+u[2]*v[2]
end function

-- return the dot product of 2 3D vectors
global function dot3(sequence u, sequence v)
    return u[1]*v[1]+u[2]*v[2]+u[3]*v[3]
end function

-- return the distance between two 2D vectors
global function distance2(sequence v1, sequence v2)
--/**/  return magnitude2(sq_sub(v1[1..2],v2[1..2]))    --/* -- Phix
        return magnitude2(v1[1..2]-v2[1..2])            --*/ -- RDS
end function

-- return the distance between two 3D vectors
global function distance3(sequence v1, sequence v2)
--/**/  return magnitude3(sq_sub(v1[1..3],v2[1..3]))    --/* -- Phix
        return magnitude3(v1[1..3]-v2[1..3])            --*/ -- RDS
end function

-- return the distance between two boids, identified
-- by their position in the boidsn sequence
function boids_dist(integer b1, integer b2)
    return distance3(boidsn[b1], boidsn[b2])
end function

-- return a list of boids within the N_DIST radius
-- of a specified boid
sequence n_id, n_dist

function neighbors(integer bid, atom distance)
atom dist
integer ix
sequence n, nid, ndist

    if bid=1 then
        n_id = repeat("", BOIDS)
        n_dist = n_id
    end if

    nid = n_id[bid]
    ndist = n_dist[bid]
    n = repeat({}, BOIDS)
    ix = 0
    for i=1 to length(n_id[bid]) do
        ix += 1
        n[ix] =  {nid[i], ndist[i]}
    end for

    for i=bid+1 to BOIDS do
        dist = boids_dist(bid, i)
        if dist<=distance then
            ix += 1
            n[ix] = {i,dist}
            n_id[i] &= bid
            n_dist[i] &= dist
        end if
    end for

    return n[1..ix]
end function

-- alter a boids velocity to try to stay at least
-- DIST away from other boids
procedure maintain_distance(integer bid, sequence n)
atom dx, dy, dz
sequence this, other

    dx = 0.0
    dy = 0.0
    dz = 0.0

    this = boidsn[bid]

    for i=1 to length(n) do
        if n[i][2]<DIST then

            other = boidsn[n[i][1]]

            dx -= (other[B_X]-this[B_X])*2
            dy -= (other[B_Y]-this[B_Y])*2
            dz -= (other[B_Z]-this[B_Z])*2

        end if

    end for

    dx *= DIST_FACTOR
    dy *= DIST_FACTOR
    dz *= DIST_FACTOR

    boidsnp1[bid][B_XV] += dx
    boidsnp1[bid][B_YV] += dy
    boidsnp1[bid][B_ZV] += dz
end procedure


-- avoid the boundaries of MAX and MIN for 
-- each dimension (X, Y, Z)
procedure avoid_walls(integer bid)
sequence this
atom dx, dy, dz, t
    dx = 0.0
    dy = 0.0
    dz = 0.0
    this = boidsn[bid]
    t = this[B_X]
    if t<DIST+X_MIN then
        dx += 1
    elsif t>X_MAX-DIST then
        dx -= 1
    end if

    t = this[B_Y]
    if t<DIST+Y_MIN then
        dy += 1
    elsif this[B_Y]>Y_MAX-DIST then
        dy -= 1
    end if

    t = this[B_Z]
    if t<DIST+Z_MIN then
        dz += 1
    elsif t>Z_MAX-DIST then
        dz -= 1
    end if

    boidsnp1[bid][B_XV] += dx
    boidsnp1[bid][B_YV] += dy
    boidsnp1[bid][B_ZV] += dz
end procedure

-- try to match the velocity of a boid to its
-- neighbors
procedure match_velocity(integer bid, sequence n)
atom dx, dy, dz
sequence this, other

    if not length(n) then
        return
    end if

    dx = 0.0
    dy = 0.0
    dz = 0.0
    this = boidsn[bid]

    for i=1 to length(n) do
        other = boidsn[n[i][1]]

        dx += other[B_XV]
        dy += other[B_YV]
        dz += other[B_ZV]
    end for

    dx /= length(n)
    dy /= length(n)
    dz /= length(n)

    dx -= this[B_XV]
    dy -= this[B_YV]
    dz -= this[B_ZV]

    dx *= DIST_FACTOR
    dy *= DIST_FACTOR
    dz *= DIST_FACTOR

    boidsnp1[bid][B_XV] += dx
    boidsnp1[bid][B_YV] += dy
    boidsnp1[bid][B_ZV] += dz
end procedure

-- try to move a boid toward the center of 
-- its neighbors
procedure move_to_center(integer bid, sequence n)
atom x, y, z
sequence other
    if not length(n) then
        return
    end if

    x = 0.0
    y = 0.0
    z = 0.0
    for i=1 to length(n) do
        other = boidsn[n[i][1]]
        x += other[B_X]
        y += other[B_Y]
        z += other[B_Z]
    end for

    -- compute the center
    x /= length(n)
    y /= length(n)
    z /= length(n)

    -- figure out the direction...
    other = boidsn[bid]
    x -= other[B_X]
    y -= other[B_Y]
    z -= other[B_Z]

    x *= DIST_FACTOR
    y *= DIST_FACTOR
    z *= DIST_FACTOR

    boidsnp1[bid][B_XV] += x
    boidsnp1[bid][B_YV] += y
    boidsnp1[bid][B_ZV] += z
end procedure

-- don't let them go too fast or too slow
procedure constrain(integer bid)
atom mag
    mag = magnitude3(boidsnp1[bid][B_XV..B_ZV])
    if mag>V_MAX then
--/**/  boidsnp1[bid][B_XV..B_ZV] = sq_div(boidsnp1[bid][B_XV..B_ZV],mag/V_MAX)                 --/* -- Phix
        boidsnp1[bid][B_XV..B_ZV] /= ( mag/V_MAX )                                              --*/ -- RDS
    elsif mag<V_MIN then

        if mag then
--/**/      boidsnp1[bid][B_XV..B_ZV] = sq_mul(boidsnp1[bid][B_XV..B_ZV],V_MIN/mag)             --/* -- Phix
            boidsnp1[bid][B_XV..B_ZV] *= ( V_MIN/mag )                                          --*/ -- RDS
        else
            boidsnp1[bid][B_XV] = V_MIN*rand(100)/100
            boidsnp1[bid][B_YV] = V_MIN*rand(100)/100
            boidsnp1[bid][B_ZV] = V_MIN*rand(100)/100
        end if
    end if
end procedure


procedure move(integer bid)
--/**/  boidsnp1[bid][B_X..B_Z] = sq_add(boidsnp1[bid][B_X..B_Z],boidsnp1[bid][B_XV..B_ZV])     --/* -- Phix
        boidsnp1[bid][B_X..B_Z] += boidsnp1[bid][B_XV..B_ZV]                                    --*/ -- RDS
end procedure


global procedure setup()
atom mag
    BOIDS0 = repeat(repeat(0.0, B_ELEMENTS), BOIDS)
    boidsn = BOIDS0
    boidsnp1 = BOIDS0

    -- place them randomly
    for boid=1 to BOIDS do
        boidsnp1[boid][B_X] = rand(X_MAX)
        boidsnp1[boid][B_Y] = rand(Y_MAX)
        boidsnp1[boid][B_Z] = rand(Z_MAX)

        boidsnp1[boid][B_XV] = V_MAX-rand(2*V_MAX)
        boidsnp1[boid][B_YV] = V_MAX-rand(2*V_MAX)
        boidsnp1[boid][B_ZV] = V_MAX-rand(2*V_MAX)

        mag = magnitude3(boidsnp1[boid])/V_MAX

        if mag>1.0 then
--/**/      boidsnp1[boid][B_XV..B_ZV] = sq_div(boidsnp1[boid][B_XV..B_ZV],mag)     --/* -- Phix
            boidsnp1[boid][B_XV..B_ZV] /= mag                                       --*/ -- RDS
        end if

    end for

    obstacles = repeat({},OBSTACLES)
    for o=1 to OBSTACLES do
        obstacles[o] = {rand(X_MAX), rand(Y_MAX), rand(Z_MAX), 30}
    end for

    boidsn = boidsnp1

end procedure

global procedure move_boids()
sequence n
    for boid=1 to BOIDS do
        n = neighbors(boid, N_DIST)
        maintain_distance(boid, n)
        match_velocity(boid, n)
        move_to_center(boid, n)
        avoid_walls(boid)
        constrain(boid)
        move(boid)
    end for
    boidsn = boidsnp1
end procedure
