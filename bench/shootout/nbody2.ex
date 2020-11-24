-- The Computer Language Shootout Benchmarks
--  http://shootout.alioth.debian.org/
-- 
--  Converted to Euphoria by Jason Gade
--  run: exu nbody.ex N

without warning
without type_check

include get.e

constant    PI = 3.141592653589793,
            SOLAR_MASS = 4 * PI * PI,
            DAYS_PER_YEAR = 365.24

-- struct planet
constant    NAME    = 1,
            X       = 2,
            Y       = 3,
            Z       = 4,
            VX      = 5,
            VY      = 6,
            VZ      = 7,
            MASS    = 8
-- end struct

sequence bodies
bodies =    {
            { "Sun", 0, 0, 0, 0, 0, 0, SOLAR_MASS},
            { "Jupiter",
              4.84143144246472090e+00,
             -1.16032004402742839e+00,
             -1.03622044471123109e-01,
              1.66007664274403694e-03 * DAYS_PER_YEAR,
              7.69901118419740425e-03 * DAYS_PER_YEAR,
             -6.90460016972063023e-05 * DAYS_PER_YEAR,
              9.54791938424326609e-04 * SOLAR_MASS },
            { "Saturn",
              8.34336671824457987e+00,
              4.12479856412430479e+00,
             -4.03523417114321381e-01,
             -2.76742510726862411e-03 * DAYS_PER_YEAR,
              4.99852801234917238e-03 * DAYS_PER_YEAR,
              2.30417297573763929e-05 * DAYS_PER_YEAR,
              2.85885980666130812e-04 * SOLAR_MASS },
            { "Uranus",
              1.28943695621391310e+01,
             -1.51111514016986312e+01,
             -2.23307578892655734e-01,
              2.96460137564761618e-03 * DAYS_PER_YEAR,
              2.37847173959480950e-03 * DAYS_PER_YEAR,
             -2.96589568540237556e-05 * DAYS_PER_YEAR,
              4.36624404335156298e-05 * SOLAR_MASS },
            { "Neptune",
              1.53796971148509165e+01,
             -2.59193146099879641e+01,
              1.79258772950371181e-01,
              2.68067772490389322e-03 * DAYS_PER_YEAR,
              1.62824170038242295e-03 * DAYS_PER_YEAR,
             -9.51592254519715870e-05 * DAYS_PER_YEAR,
              5.15138902046611451e-05 * SOLAR_MASS }
             }

constant NBODIES = length(bodies)



procedure advance(atom dt)
    atom dx, dy, dz, distance, mag
    sequence tempi, tempj

    for i = 1 to NBODIES do
        tempi = bodies[i]
        for j = i + 1 to NBODIES do
            tempj = bodies[j]
            dx = tempi[X] - tempj[X]
            dy = tempi[Y] - tempj[Y]
            dz = tempi[Z] - tempj[Z]
            distance = sqrt(dx*dx + dy*dy + dz*dz)
            mag = dt / (distance*distance*distance)
            
            tempi[VX] -= dx * tempj[MASS] * mag
            tempi[VY] -= dy * tempj[MASS] * mag
            tempi[VZ] -= dz * tempj[MASS] * mag
            tempj[VX] += dx * tempi[MASS] * mag
            tempj[VY] += dy * tempi[MASS] * mag
            tempj[VZ] += dz * tempi[MASS] * mag
            bodies[j] = tempj
        end for
        bodies[i] = tempi
    end for
    
    for i = 1 to NBODIES do
        tempi = bodies[i]
        tempi[X] += dt * tempi[VX]
        tempi[Y] += dt * tempi[VY]
        tempi[Z] += dt * tempi[VZ]
        bodies[i] = tempi
    end for
    
end procedure -- advance



function energy()
    atom e, dx, dy, dz, distance
    sequence tempi, tempj
    
    e = 0.0
    
    for i = 1 to NBODIES do
        tempi = bodies[i]
        e += 0.5 * tempi[MASS] * (tempi[VX]*tempi[VX] +
                                      tempi[VY]*tempi[VY] +
                                      tempi[VZ]*tempi[VZ])
        for j = i + 1 to NBODIES do
            tempj = bodies[j]
            dx = tempi[X] - tempj[X]
            dy = tempi[Y] - tempj[Y]
            dz = tempi[Z] - tempj[Z]
            distance = sqrt(dx*dx + dy*dy + dz*dz)
            e -= (tempi[MASS]*tempj[MASS])/distance
            bodies[j] = tempj
        end for
    end for
    
    return e
end function -- energy



procedure offset_momentum()
    atom px, py, pz
    sequence tempi
    
    px = 0.0
    py = 0.0
    pz = 0.0
    
    for i = 1 to NBODIES do
        tempi = bodies[i]
        px += tempi[VX] * tempi[MASS]
        py += tempi[VY] * tempi[MASS]
        pz += tempi[VZ] * tempi[MASS]
        bodies[i] = tempi
    end for
    
    bodies[1][VX] = - px / SOLAR_MASS
    bodies[1][VY] = - py / SOLAR_MASS
    bodies[1][VZ] = - pz / SOLAR_MASS
    
end procedure -- offset_momentum



procedure main(sequence argv)
    object n
    
    if length(argv) > 2 then
        n = value(argv[3])
        n = n[2]
    else
        n = 1000
    end if
    
    offset_momentum()
    printf(1, "%.9f\n", energy())
    
    for i = 1 to n do
        advance(0.01)
    end for
    
    printf(1, "%.9f\n", energy())
    
end procedure -- main

main(command_line())

