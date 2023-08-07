--/*
I've written a quick and dirty class-based wrapper/drop-in replacement for pGUI.e

A terse version of the above demo : https://openeuphoria.org/pastey/351.wc

Or longhand, you take your pick : https://openeuphoria.org/pastey/352.wc

To run either the above, you'll need : https://openeuphoria.org/pastey/350.wc
--*/

--
-- demo\pGUI\pGUIC.e
-- =================
--
--  A quick and dirty class-based wrapper/drop-in replacement for pGUI.e
--  This is a proof-of-concept: there is no proper error handling and only
--  those items/features needed for a single demo were included. 
--  I have no immediate plans to do anything more with this.
--
include pGUI.e 

integer hdict = new_dict("handle lookup") 

class Element nullable
    Ihandln handle
    string title
    string image
    string padding
    string tooltip_text
    procedure set_handle(Ihandle handle)
        this.handle = handle
--      printf(1,"Adding %v %d\n",{this,this.handle})
        setd(this.handle,this,hdict) 
    end procedure
    function get_handle()
        return this.handle
    end function
    procedure set_title(string title)
        this.title = title
        IupSetAttribute(this.handle,"TITLE",title)
    end procedure
    procedure set_image(string image)
        this.image = image
        IupSetAttribute(this.handle,"IMAGE",image)
    end procedure
    procedure set_padding(string padding)
        this.padding = padding
        IupSetAttribute(this.handle,"PADDING",padding)
    end procedure
    function get_tooltip_text()
        return IupGetAttribute(this.handle,"TIP")
    end function
    procedure set_tooltip_text(string tip)
        this.tooltip_text = tip
        IupSetAttribute(this.handle,"TIP",tip)
    end procedure
    procedure discard()
        while IupGetChildCount(this.handle) do
            Ihandln child = IupGetChild(this.handle,0)
            IupDetach(child)
            Element ce = getd(child,hdict)
            -- Check for eg IupExpander uses an IupBackgroundBox,
            -- neither created by nor accounted for by this code.
            if ce!=NULL then
                ce.discard()
                delete(ce)
            end if
        end while
--      printf(1,"Deleting %v %d\n",{this,this.handle})
        deld(this.handle,hdict) 
        this.handle = IupDestroy(this.handle)
    end procedure
    procedure add(Element e)
        IupAppend(this.handle,e.handle)
    end procedure
    function handles(sequence s)
        for i=1 to length(s) do
            Element si = s[i]
            s[i] = si.handle
        end for
        return s
    end function
    procedure set_data(string name, data)
        IupSetAttribute(this.handle,upper(name),data)
    end procedure
    function get_data(string name)
        string data = IupGetAttribute(this.handle,upper(name))
        return data
    end function
end class

function get_element_by_handle(Ihandle handle)
    Element element = getd(handle,hdict)
    return element
end function 

class Window extends Element
    string size
    Ihandln parent
    function Window(Element child=NULL,string attr="")
        set_handle(IupDialog(iff(child!=NULL?child.handle:child),attr))
        return this
    end function
    procedure ~Window()
        -- (destructors on other elements may also be desired...)
        this.discard()
    end procedure
    procedure set_size(string size)
        this.size = size
        IupSetAttribute(this.handle,"RASTERSIZE",size)
    end procedure
    procedure set_parent(Ihandle parent)
        this.parent = parent
        IupSetAttributeHandle(this.handle,"PARENTDIALOG",parent)
    end procedure
    procedure show()
        IupShow(this.handle)
    end procedure
    procedure popup(integer x = IUP_CURRENT, y = IUP_CURRENT)
        IupPopup(this.handle,x,y)
    end procedure
end class

class Fill extends Element
    function Fill()
        set_handle(IupFill())
        return this
    end function
end class

function Filler() return new(Fill) end function

class Container extends Element
    integer gap
    string margin
    string normalizesize
    procedure set_gap(integer gap)
        this.gap = gap
        IupSetInt(this.handle,"GAP",gap)
    end procedure
    procedure set_margin(string margin)
        this.margin = margin
        IupSetAttribute(this.handle,"MARGIN",margin)
    end procedure
    procedure set_normalizesize(string normalizesize)
        this.normalizesize = normalizesize
        IupSetAttribute(this.handle,"NORMALIZESIZE",upper(normalizesize))
    end procedure
end class

class Vbox extends Container
    function Vbox(sequence children={}, string attr="")
        set_handle(IupVbox(handles(children),attr))
        return this
    end function
end class

class Hbox extends Container
    function Hbox(sequence children={}, string attr="")
        set_handle(IupHbox(handles(children),attr))
        return this
    end function
end class

class Button extends Element
    integer action
    procedure set_action(integer action)
        this.action = action
        IupSetCallback(this.handle,"ACTION",iff(action?Icallback(action):action))
    end procedure
    function Button(string title="", atom action=NULL, string attr="", sequence data={})
        set_handle(IupButton(title,attr,data))
        if action!=NULL then set_action(action) end if
        return this
    end function
end class

class Label extends Element
    string font
    function Label(string title="", string attr="", sequence data={})
        set_handle(IupLabel(title,attr,data))
        return this
    end function
    procedure set_font(string font)
        this.font = font
        IupSetAttribute(this.handle,"FONT",font)
    end procedure
end class

class Expander extends Element
    string state
    function Expander(Element child=NULL, string attr="")
        set_handle(IupExpander(iff(child!=NULL?child.handle:child),attr))
        return this
    end function
    procedure set_state(string state)
        this.state = state
        IupSetAttribute(this.handle,"STATE",state)
    end procedure
end class

-- longhand/one item per line:
include pGUIC.e
constant docs = ` 

    Elements have a Data space 
    which you can use to declare and pass  
    multiple key/value pairs` 

constant -- answers are encrypted to avoid overturning the legumes; 
  
q1 = "What is purple and conquered the world?", 
a1 = "PGk+QWxleGFuZGVyIHRoZSBHcmFwZSE8L2k+", 
 
q2 = "What lies at the bottom of the ocean and twitches?", 
a2 = "PGk+QSBuZXJ2b3VzIHdyZWNrLjwvaT4=", 
 
q3 = "Why is Christmas just like another day at the office?", 
a3 = "WW91IGRvIGFsbCBvZiB0aGUgd29yayBhbmQgdGhlIGZhdCBndXkKaW4gdGhlIHN1aXQgZ2V0cyBhbGwgdGhlIGNyZWRpdC4=" 

function ShowMe(Ihandle ctl)
--?{"dict_dize",dict_size(hdict)}
    Element button = get_element_by_handle(ctl)
    string title = button.tooltip_text,
           question = button.get_data("Question"), -- get data items by name; 
           answer = button.get_data("Answer") 
     
    answer = substitute_all(decode_base64(answer),{"<i>","</i>"},{"",""})
     
    -- now we build our own custom dialog; 
     
    Label ans = new()
          ans.title = answer
          ans.font = "Courier Bold Italic 18" 
         
    Hbox bx = new()
         bx.add(Filler())
         bx.add(ans)
         bx.add(Filler())
         bx.margin = "10x20"
         
    Expander expd = new()
             expd.title = "Click here to see the answer..." 
             expd.add(bx)  
             expd.state = "CLOSE"
     
    Label icon = new()
          icon.image = "IUP_IconMessageHelp"
          icon.padding = "10x10"

    Label qlbl = new()
          qlbl.title = question

    Hbox qbox = new()
         qbox.add(Filler())
         qbox.add(icon)
         qbox.add(qlbl)
         qbox.add(Filler())
         qbox.margin = "0x20"
         qbox.normalizesize = "vertical"

    Vbox vbox = new()
         vbox.add(qbox)
         vbox.add(expd)

    Window dlg = new()
           dlg.add(vbox)
           dlg.title = title
           dlg.size = "600x250"
           dlg.parent = ctl
     
    dlg.popup(IUP_LEFTPARENT,IUP_TOPPARENT)
     
--  delete(dlg) -- NB crashes on <= 0.8.3

    return 1 
end function  

IupOpen() 
IupImageLibOpen() 

Label lbl = new({docs})
Hbox lbox = new({{Filler(),lbl,Filler()}}) 

Button btn1 = new() 
       btn1.title = "Question _1" 
       btn1.action = ShowMe
       btn1.tooltip_text = "World History" 
       btn1.image = "IUP_MessageHelp" 
       btn1.padding = "5x5" 
       btn1.set_data("Question",q1) 
       btn1.set_data("Answer",a1) 
 
Button btn2 = new() 
       btn2.title = "Question _2" 
       btn2.action = ShowMe
       btn2.tooltip_text = "Science" 
       btn2.image = "IUP_MessageHelp" 
       btn2.padding = "5x5" 
       btn2.set_data("Question",q2) 
       btn2.set_data("Answer",a2) 
       
Button btn3 = new() 
       btn3.title = "Question _3" 
       btn3.action = ShowMe
       btn3.tooltip_text = "Business" 
       btn3.image = "IUP_MessageHelp" 
       btn3.padding = "5x5"
       btn3.set_data("Question",q3) 
       btn3.set_data("Answer",a3) 
 
Hbox box = new()
     box.add(btn1)
     box.add(Filler())
     box.add(btn2)
     box.add(Filler())
     box.add(btn3)
     box.gap = 5

--procedure set(struct s, string field, object v)
--  s[field] = v
--end procedure

Vbox pan = new()
     pan.add(lbox)
     pan.add(Filler())
     pan.add(box)
     pan.gap = 10
--   set(pan,"gap",10)
     pan.margin = "10x10"
--   set(pan,"margin","10x10")
 
Window win = new()
    win.add(pan)
    win.title = `IupExpander and Data Passing` 

win.show() 
IupMainLoop()
--delete(win) -- (may crash on <= 0.8.3)

--?{"dict_dize",dict_size(hdict)}
--?"done"
--{} = wait_key()

abort(0) 

--terse:
include pGUIC.e 
constant docs = ` 

    Elements have a Data space 
    which you can use to declare and pass  
    multiple key/value pairs` 

constant -- answers are encrypted to avoid overturning the legumes; 
  
q1 = "What is purple and conquered the world?", 
a1 = "PGk+QWxleGFuZGVyIHRoZSBHcmFwZSE8L2k+", 
 
q2 = "What lies at the bottom of the ocean and twitches?", 
a2 = "PGk+QSBuZXJ2b3VzIHdyZWNrLjwvaT4=", 
 
q3 = "Why is Christmas just like another day at the office?", 
a3 = "WW91IGRvIGFsbCBvZiB0aGUgd29yayBhbmQgdGhlIGZhdCBndXkKaW4gdGhlIHN1aXQgZ2V0cyBhbGwgdGhlIGNyZWRpdC4=" 

function ShowMe(Ihandle ctl)
    Element button = get_element_by_handle(ctl)
    string title = button.tooltip_text,
           question = button.get_data("Question"), -- get data items by name; 
           answer = button.get_data("Answer") 
     
    answer = substitute_all(decode_base64(answer),{"<i>","</i>"},{"",""})
     
    -- now we build our own custom dialog; 
     
    Label ans = new({answer,`FONT="Courier Bold Italic 18"`})
         
    Hbox bx = new({{Filler(),ans,Filler()},"MARGIN=10x20"})
         
    Expander expd = new({bx,`TITLE="Click here to see the answer...", STATE=CLOSE`})
     
    Label icon = new({"","IMAGE=IUP_IconMessageHelp, PADDING=10x10"})

    Label qlbl = new(Label,{question})
    Hbox qbox = new({{Filler(),icon,qlbl,Filler()},"MARGIN=0x20, NORMALIZESIZE=VERTICAL"})

    Vbox vbox = new({{qbox,expd}})

    Window dlg = new({vbox})
           dlg.title = title
           dlg.size = "600x250"
           dlg.parent = ctl
     
    dlg.popup(IUP_LEFTPARENT,IUP_TOPPARENT)
     
--  delete(dlg) -- NB crashes on <= 0.8.3
     
    return 1 
end function  

IupOpen() 
IupImageLibOpen() 

Label lbl = new({docs})
Hbox lbox = new({{Filler(),lbl,Filler()}}) 

Button btn1 = new({"Question _1",ShowMe,`TIP="World History", IMAGE=IUP_MessageHelp, PADDING=5x5`})
       btn1.set_data("Question",q1) 
       btn1.set_data("Answer",a1) 
 
Button btn2 = new({"Question _2",ShowMe,`TIP="Science", IMAGE=IUP_MessageHelp, PADDING=5x5`})
       btn2.set_data("Question",q2) 
       btn2.set_data("Answer",a2) 
       
Button btn3 = new({"Question _3",ShowMe,`TIP="Business", IMAGE=IUP_MessageHelp, PADDING=5x5`})
       btn3.set_data("Question",q3) 
       btn3.set_data("Answer",a3) 
 
Hbox box = new({{btn1,Filler(),btn2,Filler(),btn3},"GAP=5"})

Vbox pan = new({{lbox,Filler(),box},"GAP=5,MARGIN=10x10"})
 
Window win = new({pan,`TITLE="IupExpander and Data Passing"`})

win.show() 
IupMainLoop()
--delete(win) -- (may crash on <= 0.8.3)
--?{"dict_dize",dict_size(hdict)}
?"done"
{} = wait_key()

abort(0) 





-- non-class:
include pGUI.e 

constant docs = ` 

    Elements have a Data space 
    which you can use to declare and pass  
    multiple key/value pairs` 

constant -- answers are encrypted to avoid overturning the legumes; 
  
q1 = "What is purple and conquered the world?", 
a1 = "PGk+QWxleGFuZGVyIHRoZSBHcmFwZSE8L2k+", 
 
q2 = "What lies at the bottom of the ocean and twitches?", 
a2 = "PGk+QSBuZXJ2b3VzIHdyZWNrLjwvaT4=", 
 
q3 = "Why is Christmas just like another day at the office?", 
a3 = "WW91IGRvIGFsbCBvZiB0aGUgd29yayBhbmQgdGhlIGZhdCBndXkKaW4gdGhlIHN1aXQgZ2V0cyBhbGwgdGhlIGNyZWRpdC4=" 

function ShowMe(Ihandle ctl)  
    string title = IupGetAttribute(ctl,"TIP"),
           question = IupGetAttribute(ctl,"QUESTION"),
           answer = IupGetAttribute(ctl,"ANSWER")
     
    answer = substitute_all(decode_base64(answer),{"<i>","</i>"},{"",""})
     
    -- now we build our own custom dialog; 
     
    Ihandle ans = IupLabel(answer,`FONT="Courier Bold Italic 18"`)
         
    Ihandle bx = IupHbox({IupFill(),ans,IupFill()},"MARGIN=10x20")
         
    Ihandle expd = IupExpander(bx,`TITLE="Click here to see the answer...",STATE=CLOSE`)
     
    Ihandle icon = IupLabel("","IMAGE=IUP_IconMessageHelp, PADDING=10,10")
    Ihandle qu = IupHbox({IupFill(),icon,IupLabel(question),IupFill()},"MARGIN=0x20")
    IupSetAttribute(qu,"NORMALIZESIZE","VERTICAL")
    Ihandln dlg = IupDialog(IupVbox({qu,expd}),"RASTERSIZE=600x250")
    IupSetAttribute(dlg,"TITLE",title)
    IupSetAttributeHandle(dlg,"PARENTDIALOG",IupGetDialog(ctl))
     
    IupPopup(dlg,IUP_CURRENT,IUP_MOUSEPOS)
    dlg = IupDestroy(dlg)
     
    return IUP_DEFAULT
end function

IupOpen() 
IupImageLibOpen() 

Ihandle lbl = IupHbox({IupFill(),IupLabel(docs),IupFill()})

Ihandle btn1 = IupButton("Question _1",Icallback(ShowMe),`TIP="World History", IMAGE=IUP_MessageHelp, PADDING=5x5`)
IupSetAttribute(btn1,"QUESTION",q1)
IupSetAttribute(btn1,"ANSWER",a1)
 
Ihandle btn2 = IupButton("Question _2",Icallback(ShowMe),`TIP="Science", IMAGE=IUP_MessageHelp, PADDING=5x5`)
IupSetAttribute(btn2,"QUESTION",q2)
IupSetAttribute(btn2,"ANSWER",a2)
       
Ihandle btn3 = IupButton("Question _3",Icallback(ShowMe),`TIP="Business", IMAGE=IUP_MessageHelp, PADDING=5x5`)
IupSetAttribute(btn3,"QUESTION",q3)
IupSetAttribute(btn3,"ANSWER",a3)
 
Ihandle box = IupHbox({btn1,IupFill(),btn2,IupFill(),btn3},"GAP=5")

Ihandle pan = IupVbox({lbl,IupFill(),box},"GAP=10,MARGIN=10x10")
 
Ihandle win = IupDialog(pan,`TITLE="IupExpander and Data Passing"`)

IupShow(win)
IupMainLoop()

