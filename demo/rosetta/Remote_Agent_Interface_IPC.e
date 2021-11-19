--
-- demo\rosetta\Remote_Agent_Interface_IPC.e
-- =========================================
--
include builtins\ipc.e

-- The shared memory contains two bytes:
--  byte[1] (command) can only be set by the agent and cleared by the world,
--  byte[2] ( event ) can only be set by the world and cleared by the agent.

constant ra = "remote_agent"
atom pSharedMem = sm_open(ra)
if pSharedMem=SM_OPEN_FAIL then
    pSharedMem = sm_create(ra,2)
    if pSharedMem<=0 then
        crash("unable to create shared memory")
    end if
    poke2(pSharedMem,0) -- initialised to zero bytes
end if

procedure send_command_via_ipc(integer command)
    while peek(pSharedMem)!=0 do
        sleep(0.1)
    end while
    poke(pSharedMem,command)
end procedure

function get_command_from_ipc()
    integer command
    while true do
        command = peek(pSharedMem)
        if command!=0 then exit end if
        sleep(0.1)
    end while
    poke(pSharedMem,0)
    return command
end function

procedure send_event_via_ipc(integer event)
    while peek(pSharedMem+1)!=0 do
        sleep(0.1)
    end while
    poke(pSharedMem+1,event)
end procedure

function get_event_from_ipc()
    integer event
    while true do
        event = peek(pSharedMem+1)
        if event!=0 then exit end if
        sleep(0.1)
    end while
    poke(pSharedMem+1,0)
    return event
end function

procedure game_over()
    show_at(32,62,"game over")
    {} = wait_key()
end procedure

global procedure register_world(integer accept_command, get_event, get_world=0)
    integer event = get_event()
    if event!='A' then die("rw1") end if
    send_event_via_ipc(event)
    integer command = get_command_from_ipc()
    if command!='A' then die("rw2") end if
    show_at(1,60,"server handshake recieved")
    -- server loop:
    while true do
        command = get_command_from_ipc()
        if command='+' then
            game_over()
            return
        end if
        accept_command(command) 
        show("world",command,1,get_world)
        while true do
            event = get_event()
            send_event_via_ipc(event)
            if event='.' then exit end if   -- stop
        end while
    end while
end procedure

global procedure register_agent(integer get_command, accept_event, get_agent=0)
    integer command = get_command()
    if command!='A' then die("ra1") end if
    send_command_via_ipc(command) 
    integer event = get_event_from_ipc()
    if event!='A' then die("ra2") end if
    show_at(2,60,"agent handshake recieved")
    -- agent loop:
    while true do
        command = get_command()
        send_command_via_ipc(command) 
        while true do
            event = get_event_from_ipc()
            if event='+' then
                send_command_via_ipc(event) 
                show("agent",'.',22,get_agent)
                game_over()
                return
            end if
            accept_event(event)
            show("agent",event,22,get_agent)
            if event='.' then exit end if   -- stop
        end while
    end while
end procedure

