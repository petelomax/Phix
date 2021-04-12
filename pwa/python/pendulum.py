#''' Python 3.6.5 code using Tkinter graphical user interface.''' 
""" Python 3.6.5 code using Tkinter graphical user interface."""
 
from tkinter import *
import math
 
def Animation(root):
    xoff = 300
    yoff = 100
    angle = 0
    sina = math.sin(0)
    cosa = math.cos(0)
    rodhyp = 170
    bobr = 30
    bobhyp = rodhyp + bobr
    rodx0 = xoff
    rody0 = yoff
    ra = rodx0
    rb = rody0
    rc = xoff + rodhyp*sina
    rd = yoff + rodhyp*cosa
    ba = xoff - bobr + bobhyp*sina
    bb = yoff - bobr + bobhyp*cosa
    bc = xoff + bobr + bobhyp*sina
    bd = yoff + bobr + bobhyp*cosa
    da = math.pi / 360
 
    # create / fill canvas:
    cnv = Canvas(root, bg='lemon chiffon')
    cnv.pack(fill=BOTH, expand=True)
 
    cnv.create_line(0, 100, 600, 100, fill='dodger blue',width=3)
    radius = 8
    cnv.create_oval(300-radius, 100-radius, 300+radius, 100+radius, fill='navy')     
    bob = cnv.create_oval(ba,bb,bc,bd,fill='red',width=2)
    rod = cnv.create_line(ra,rb,rc,rd,fill='dodger blue',width=6)
 
    def animate():
        nonlocal angle, da
        if abs(angle) > math.pi / 2:
            da = - da
        angle += da
        sina = math.sin(angle)
        cosa = math.cos(angle)
        ra = rodx0
        rb = rody0
        rc = xoff + rodhyp*sina
        rd = yoff + rodhyp*cosa
        ba = xoff - bobr + bobhyp*sina
        bb = yoff - bobr + bobhyp*cosa
        bc = xoff + bobr + bobhyp*sina
        bd = yoff + bobr + bobhyp*cosa
 
        cnv.coords(rod,ra,rb,rc,rd)
        cnv.coords(bob,ba,bb,bc,bd)
        root.update()
        cnv.after(5, animate)

    animate()
         
root = Tk()
root.title('Pendulum')
root.geometry('600x400+100+50')
root.resizable(False, False)
a = Animation(root)
root.mainloop()
 
