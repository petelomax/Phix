sequence paramNames

         paramNames={}

-- verify the compiler is setting these as "sequence of integer":
--/**/  #isginfo{paramNames,0b0100,MIN,MAX,integer,-2}

global procedure ParamsAndLocals()

    if 1>length(paramNames) then
        paramNames = append(paramNames,0)
    else
        paramNames[1] = 0
    end if
end procedure

--ParamsAndLocals()

