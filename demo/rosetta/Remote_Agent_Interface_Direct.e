--
-- demo\rosetta\Remote_Agent_Interface_Direct.e
-- ============================================
--
include Remote_Agent_Simulator.exw
include Remote_Agent_Agent_Logic.exw

integer accept_command=0, get_event, get_world,
        get_command=0, accept_event, get_agent

procedure main_event_loop()
    integer handshake = get_event()
    if handshake!='A' then ?9/0 end if
    show_at(1,65,"server handshake recieved")
    handshake = get_command()
    if handshake!='A' then ?9/0 end if
    show_at(2,65,"agent handshake recieved")
    while true do
        integer command = get_command()
        show("agent",0,22,get_agent)
        accept_command(command) 
        show("world",command,1,get_world)
        while true do
            integer event = get_event()
            if event='+' then
                -- game over
                show("world",0,1,get_world)
                show("agent",'.',22,get_agent)
                show_at(5,66,"game over")
                {} = wait_key()
                return
            end if
            accept_event(event)
            show("world",0,1,get_world)
            show("agent",event,22,get_agent)
            if event='.' then exit end if   -- stop
        end while
    end while
end procedure

global procedure register_world(integer _accept_command, _get_event, _get_world=NULL)
    {accept_command, get_event, get_world} = {_accept_command, _get_event, _get_world}
    if get_command!=0 then main_event_loop() end if
end procedure

global procedure register_agent(integer _get_command, _accept_event, _get_agent=NULL)
    {get_command, accept_event, get_agent} = {_get_command, _accept_event, _get_agent}
    if accept_command!=0 then main_event_loop() end if
end procedure

