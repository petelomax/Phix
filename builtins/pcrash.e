DEAD
--
-- pcrash.e
--
-- Phix implementation of crash()
--

without debug
global procedure crash(sequence fmt, object data={})
    if atom(data) or length(data) then
        fmt = sprintf(fmt, data)
    end if
    #ilASM{ lea edi,[fmt]
            call %opCrshMsg }
    ?9/0
end procedure

