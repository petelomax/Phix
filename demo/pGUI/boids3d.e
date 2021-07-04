global integer BOIDS = 60
--             OBSTACLES = 3
global atom N_DIST = 75.0,
            DIST = 30.0,
            V_MIN = 4.0,
            V_MAX = 10.0,
            X_MIN = 0.0,
            Y_MIN = 0.0,
            Z_MIN = 0.0,
            X_MAX = 600.0,
            Y_MAX = 600.0,
            Z_MAX = 600.0

constant DIST_FACTOR = 1.0/100.0

--global enum B_X, B_Y, B_Z, B_XV, B_YV, B_ZV, B_ELEMENTS = B_ZV
global enum B_X, B_Y, B_Z, B_XV, B_YV, B_ZV, B_ELEMENTS = $

global sequence boidsn, boidsnp1
--, obstacles

function magnitude3(sequence v)
-- return the scalar magnitude of a 3D vector
    return sqrt(v[1]*v[1]+v[2]*v[2]+v[3]*v[3])
end function

global function make_length(sequence v, atom l)
-- change the scalar magnitude of a 3D vector
    return sq_mul(v,l/magnitude3(v))
end function

global function dot(sequence u, sequence v)
-- return the dot product of 2 2D vectors
    return u[1]*v[1]+u[2]*v[2]
end function

global function distance3(sequence v1, sequence v2)
-- return the distance between two 3D vectors
    return magnitude3(sq_sub(v1[1..3],v2[1..3]))
end function

function boids_dist(integer b1, integer b2)
-- return the distance between two boids, identified by their position in the boidsn sequence
    return distance3(boidsn[b1], boidsn[b2])
end function

sequence n_id, n_dist

function neighbors(integer bid, atom distance)
-- return a list of boids within the N_DIST radius of a specified boid
atom dist
integer ix
sequence n, nid, ndist

    if bid=1 then
        n_id = repeat("", BOIDS)
        n_dist = deep_copy(n_id)
    end if

    nid = deep_copy(n_id[bid])
    ndist = deep_copy(n_dist[bid])
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
--          n_id[i] &= bid
            n_id[i] = deep_copy(n_id[i]) & bid
--          n_dist[i] &= dist
            n_dist[i] = deep_copy(n_dist[i]) & dist
        end if
    end for

    return n[1..ix]
end function

procedure maintain_distance(integer bid, sequence n)
-- alter a boids velocity to try to stay at least DIST away from other boids

    atom dx = 0.0,
         dy = 0.0,
         dz = 0.0

    sequence bbid = boidsn[bid], other

    for i=1 to length(n) do
        if n[i][2]<DIST then

            other = boidsn[n[i][1]]

            dx -= (other[B_X]-bbid[B_X])*2
            dy -= (other[B_Y]-bbid[B_Y])*2
            dz -= (other[B_Z]-bbid[B_Z])*2

        end if

    end for

    dx *= DIST_FACTOR
    dy *= DIST_FACTOR
    dz *= DIST_FACTOR

--  boidsnp1[bid][B_XV] += dx
--  boidsnp1[bid][B_YV] += dy
--  boidsnp1[bid][B_ZV] += dz
    bbid = deep_copy(boidsnp1[bid])
    bbid[B_XV] += dx
    bbid[B_YV] += dy
    bbid[B_ZV] += dz
    boidsnp1[bid] = bbid
end procedure

procedure avoid_walls(integer bid)
-- avoid the boundaries of MAX and MIN for each dimension (X, Y, Z)
    sequence bbid = boidsn[bid]
    atom dx = 0.0,
         dy = 0.0,
         dz = 0.0,
         t = bbid[B_X]
    if t<DIST+X_MIN then
        dx += 1
    elsif t>X_MAX-DIST then
        dx -= 1
    end if

    t = bbid[B_Y]
    if t<DIST+Y_MIN then
        dy += 1
    elsif t>Y_MAX-DIST then
        dy -= 1
    end if

    t = bbid[B_Z]
    if t<DIST+Z_MIN then
        dz += 1
    elsif t>Z_MAX-DIST then
        dz -= 1
    end if

    boidsnp1[bid][B_XV] += dx
    boidsnp1[bid][B_YV] += dy
    boidsnp1[bid][B_ZV] += dz
end procedure

procedure match_velocity(integer bid, sequence n)
-- try to match the velocity of a boid to its neighbors

    if length(n) then
        atom dx = 0.0,
             dy = 0.0,
             dz = 0.0
        sequence bbid = boidsn[bid], other

        for i=1 to length(n) do
            other = boidsn[n[i][1]]

            dx += other[B_XV]
            dy += other[B_YV]
            dz += other[B_ZV]
        end for

        dx /= length(n)
        dy /= length(n)
        dz /= length(n)

        dx -= bbid[B_XV]
        dy -= bbid[B_YV]
        dz -= bbid[B_ZV]

        dx *= DIST_FACTOR
        dy *= DIST_FACTOR
        dz *= DIST_FACTOR

        boidsnp1[bid][B_XV] += dx
        boidsnp1[bid][B_YV] += dy
        boidsnp1[bid][B_ZV] += dz
    end if
end procedure

procedure move_to_center(integer bid, sequence n)
-- try to move a boid toward the center of its neighbors
    sequence other
    if length(n) then
        atom x = 0.0,
             y = 0.0,
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
    end if
end procedure

procedure constrain(integer bid)
-- don't let them go too fast or too slow
    atom mag = magnitude3(boidsnp1[bid][B_XV..B_ZV])
    if mag>V_MAX then
        boidsnp1[bid][B_XV..B_ZV] = sq_div(boidsnp1[bid][B_XV..B_ZV],mag/V_MAX)
    elsif mag<V_MIN then
        if mag then
            boidsnp1[bid][B_XV..B_ZV] = sq_mul(boidsnp1[bid][B_XV..B_ZV],V_MIN/mag)
        else
            boidsnp1[bid][B_XV] = V_MIN*rand(100)/100
            boidsnp1[bid][B_YV] = V_MIN*rand(100)/100
            boidsnp1[bid][B_ZV] = V_MIN*rand(100)/100
        end if
    end if
end procedure

procedure move(integer bid)
    boidsnp1[bid][B_X..B_Z] = sq_add(boidsnp1[bid][B_X..B_Z],boidsnp1[bid][B_XV..B_ZV])
end procedure

global procedure setup()
--  boidsn = repeat(repeat(0.0, B_ELEMENTS), BOIDS)
    boidsnp1 = repeat(repeat(0.0, B_ELEMENTS), BOIDS)
--  boidsnp1 = boidsn

    -- place them randomly
    for boid=1 to BOIDS do
        boidsnp1[boid][B_X] = rand(X_MAX)
        boidsnp1[boid][B_Y] = rand(Y_MAX)
        boidsnp1[boid][B_Z] = rand(Z_MAX)

        boidsnp1[boid][B_XV] = V_MAX-rand(2*V_MAX)
        boidsnp1[boid][B_YV] = V_MAX-rand(2*V_MAX)
        boidsnp1[boid][B_ZV] = V_MAX-rand(2*V_MAX)

        atom mag = magnitude3(boidsnp1[boid])/V_MAX

        if mag>1.0 then
            boidsnp1[boid][B_XV..B_ZV] = sq_div(boidsnp1[boid][B_XV..B_ZV],mag)
        end if
    end for

--  obstacles = repeat({},OBSTACLES)
--  for o=1 to OBSTACLES do
--      obstacles[o] = {rand(X_MAX), rand(Y_MAX), rand(Z_MAX), 30}
--  end for

--  boidsn = boidsnp1
    boidsn = deep_copy(boidsnp1)

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
    boidsn = deep_copy(boidsnp1)
end procedure

