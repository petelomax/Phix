--
--  builtins\beep.e
--  ===============
--
--   (Not an autoinclude)
--
atom xBeep=0

global procedure beep(object frequency=440, duration=500, atom /*volume*/=1)
    --
    -- frequency can be an atom or a sequence of atoms, in Hz
    -- duration can be an atom or a sequnce of atoms, in milliseconds
    -- When both sequences, an error occurs if their lengths don't match.
    -- When atom(frequency) & sequence(duration) even elements are silent.
    -- The volume parameter does nothing on desktop/Phix, but under p2js
    -- it should be a floating point value between 0 (muted) and 1 (full)
    -- Note that a web browser will refuse permission to initiate a beep
    -- unless triggered by a user action such as clicking on a button.
    -- examples: demo\rosetta\Audio_frequency_generator.exw
    --
    if platform()=WINDOWS and xBeep=0 then
        atom k32 = open_dll("kernel32.dll")
        xBeep = define_c_proc(k32, "Beep", {C_INT,C_INT})
    end if
    integer len = iff(sequence(frequency)?length(frequency):
                  iff(sequence(duration)?length(duration):1))
    assert(atom(frequency) or atom(duration) or length(duration)==len)
    for i=1 to len do
        atom f = iff(sequence(frequency)?frequency[i]:
                 iff(sequence(duration) and even(i)?0:frequency)),
             d = iff(sequence(duration)?duration[i]:duration)
        if platform()=WINDOWS then
            c_proc(xBeep,{f,d})
        elsif platform()=LINUX then
            system(sprintf("play -n synth %f sine %d", {d/1000, f}))
--      elsif platform()=JS then
--          (manually implemented in p2js.js: p2js.exw should cope,
--           and not transpile this, even when explicitly included.)
        else
            ?9/0
        end if
    end for
end procedure
