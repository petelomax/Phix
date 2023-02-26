--
-- demo\rosetta\turtle.e
-- =====================
--
--  Common code for 2D and 3D versions
--
--  You c/should perhaps move walk() etc here, but that
--  probably wants a better way to store x/y/direction
--  for 2D and (p|h|n)(x|y|z) for 3D (ideas welcome), and
--  maybe new_2D_turtle() and new_3D_turtle() functions.
--  In the end the code I really wanted to squirrel away
--  (for 3D only) is now living in turtle_projection.e
--
without debug                       --  (optional)
include pGUI.e
include builtins\VM\pcfunc.e        --   vv""vv
include builtins\pfile.e            --
include builtins\VM\pprntfN.e       --   (keeps
include builtins\get_routine_info.e --     any
include builtins\scanf.e            --    ex.err
include builtins\pdir.e             --    clean)
include builtins\penv.e             --
with debug                          --   ^^""^^
global Ihandle canvas, dlg
global cdCanvas cdcanvas
global bool pen_down = false

global procedure pendown(atom colour=CD_BLACK)
    pen_down = true
    cdCanvasSetForeground(cdcanvas, colour) 
end procedure

global procedure penup()
    pen_down = false
end procedure



