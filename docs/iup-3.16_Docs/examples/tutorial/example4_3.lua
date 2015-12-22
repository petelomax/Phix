require("imlua")
require("iuplua")
require("iupluaimglib")
require("iupluaim")
require("cdlua")
require("iupluacd")


--********************************** Utilities *****************************************

function str_splitfilename(filename)
  return string.match(filename, "(.-)([^\\/]-%.?([^%.\\/]*))$")
end

function str_fileext(filename)
  local path, title, ext = str_splitfilename(filename)
  return ext
end

function str_filetitle(filename)
  local path, title, ext = str_splitfilename(filename)
  return title
end

function show_error(message, is_error)
  local dlg = iup.messagedlg{
    parentdialog = iup.GetGlobal("PARENTDIALOG"),
    buttons = "OK",
    value = message,
  }
  if (is_error) then
    dlg.dialogtype = "ERROR"
    dlg.title = "Error"
  else
    dlg.dialogtype = "WARNING"
    dlg.title = "Warning"
  end
  dlg:popup(iup.CENTERPARENT, iup.CENTERPARENT)
  dlg:destroy()
end

function read_file(filename)
  local image, err = im.FileImageLoadBitmap(filename, 0)
  if (err) then
    show_error(im.ErrorStr(err), true)
  else
    -- we are going to support only RGB images with no alpha
    image:RemoveAlpha()
    if (image:ColorSpace() ~= im.RGB) then
      local new_image = im.ImageCreateBased(image, nil, nil, im.RGB, nil)        

      im.ConvertColorSpace(image, new_image)
      image:Destroy()

      image = new_image
    end
  end
  return image
end

function write_file(filename, image)
  local format = image:GetAttribString("FileFormat")
  local err = im.FileImageSave(filename, format, image)
  if (err and err ~= im.ERR_NONE) then
    show_error(im.ErrorStr(err), true)
    return false
  end
  return true
end

function new_file(ih, image)
  local dlg = iup.GetDialog(ih)
  local canvas = dlg.canvas
  local old_image = canvas.image
  
  dlg.title = "Untitled - Simple Paint"
  canvas.filename = nil
  canvas.dirty = nil
  canvas.image = image

  iup.Update(canvas)

  if (old_image) then
    old_image:Destroy()
  end
end

function check_new_file(dlg)
  local canvas = dlg.canvas
  local image = canvas.image
  if (not image) then
    local config = canvas.config
    local width = config:GetVariableDef("NewImage", "Width", 640)
    local height = config:GetVariableDef("NewImage", "Height", 480)

    local image = im.ImageCreate(width, height, im.RGB, im.BYTE)

    new_file(dlg, image)
  end
end

function open_file(ih, filename)
  local image = read_file(filename)
  if (image) then
    local dlg = iup.GetDialog(ih)
    local canvas = dlg.canvas
    local config = canvas.config
    local old_image = canvas.image
  
    dlg.title = str_filetitle(filename).." - Simple Paint"
    canvas.filename = filename
    canvas.dirty = nil
    canvas.image = image

    iup.Update(canvas)

    if (old_image) then
      old_image:Destroy()
    end
    
    config:RecentUpdate(filename)
  end
end

function save_file(canvas)
  if (write_file(canvas.filename, canvas.image)) then
    canvas.dirty = nil
  end
end

function set_file_format(image, filename)
  local ext = str_fileext(filename)
  ext:lower()
  local format = "JPEG"
  if (ext == "jpg" or ext == "jpeg") then
    format = "JPEG"
  elseif (ext == "bmp") then
    format = "BMP"
  elseif (ext == "png") then
    format = "PNG"
  elseif (ext == "tga") then
    format = "TGA"
  elseif (ext == "tif" or ext == "tiff") then
    format = "TIFF"
  end
  image:SetAttribString("FileFormat", format)
end

function saveas_file(canvas, filename)
  local image = canvas.image

  set_file_format(image, filename)

  if (write_file(filename, image)) then
    local dlg = iup.GetDialog(canvas)
    local config = canvas.config
    
    dlg.title = str_filetitle(filename).." - Simple Paint"
    canvas.filename = filename
    canvas.dirty = nil
    
    config:RecentUpdate(filename)
  end
end

function save_check(ih)
  local dlg = iup.GetDialog(ih)
  local canvas = dlg.canvas
  
  if (canvas.dirty) then
    local resp = iup.Alarm("Warning", "File not saved! Save it now?", "Yes", "No", "Cancel")
    if resp == 1 then -- save the changes and continue
      save_file(canvas)
    elseif resp == 3 then  -- cancel
      return false
    else  -- ignore the changes and continue
    end
  end
  return true
end

function toggle_bar_visibility(item, bar)
  if (item.value == "ON") then
    bar.floating = "YES"
    bar.visible = "NO"
    item.value = "OFF"
  else
    bar.floating = "NO"
    bar.visible = "YES"
    item.value = "ON"
  end
  iup.Refresh(bar)  -- refresh the dialog layout
end


--********************************** Main (Part 1/2) *****************************************

-- create all the elements that will have callbacks in Lua prior to callbacks definition

config = iup.config{}
config.app_name = "simple_paint"
config:Load()

statusbar = iup.label{title = "(0, 0) = [0   0   0]", expand = "HORIZONTAL", padding = "10x5"}

canvas = iup.canvas{
  config = config,
  dirty = nil,
}

item_new = iup.item{title = "&New...\tCtrl+N", image = "IUP_FileNew"}
item_open = iup.item{title = "&Open...\tCtrl+O", image = "IUP_FileOpen"}
item_save = iup.item{title="&Save\tCtrl+S"}
item_saveas = iup.item{title="Save &As...", image = "IUP_FileSave"}
item_revert = iup.item{title="&Revert"}
item_pagesetup = iup.item{title="Page Set&up..."}
item_print = iup.item{title="&Print\tCtrl+P"}
item_exit = iup.item{title="E&xit"}
item_copy = iup.item{title="&Copy\tCtrl+C", image = "IUP_EditCopy"}
item_paste = iup.item{title="&Paste\tCtrl+V", image = "IUP_EditPaste"}
item_background = iup.item{title="&Background..."}
item_toolbar = iup.item{title="&Toobar", value="ON"}
item_statusbar = iup.item{title="&Statusbar", value="ON"}
item_help = iup.item{title="&Help..."}
item_about = iup.item{title="&About..."}

recent_menu = iup.menu{}

file_menu = iup.menu{
  item_new,
  item_open,
  item_save,
  item_saveas,
  item_revert,
  iup.separator{},
  item_pagesetup,
  item_print,
  iup.separator{},
  iup.submenu{title="Recent &Files", recent_menu},
  iup.separator{},
  item_exit
  }

edit_menu = iup.menu{
  item_copy,
  item_paste,
  }

view_menu = iup.menu{
  item_background,
  iup.separator{},
  item_toolbar, 
  item_statusbar, 
  }
help_menu = iup.menu{item_help, item_about}

sub_menu_file = iup.submenu{file_menu, title = "&File"}
sub_menu_edit = iup.submenu{edit_menu, title = "&Edit"}
sub_menu_view = iup.submenu{title = "&View", view_menu}
sub_menu_help = iup.submenu{help_menu, title = "&Help"}

menu = iup.menu{
  sub_menu_file, 
  sub_menu_edit, 
  sub_menu_view, 
  sub_menu_help,
  }


--********************************** Callbacks *****************************************


function canvas:action()
  local image = canvas.image
  local canvas_width, canvas_height = string.match(canvas.drawsize,"(%d*)x(%d*)")
  local cd_canvas = canvas.cdCanvas

  cd_canvas:Activate()

  -- draw the background 
  local background = config:GetVariableDef("Canvas", "Background", "255 255 255")
  local r, g, b = string.match(background, "(%d*) (%d*) (%d*)")
  cd_canvas:Background(cd.EncodeColor(r, g, b))
  cd_canvas:Clear()

  -- draw the image at the center of the canvas 
  if (image) then
    local x = (canvas_width - image:Width()) / 2
    local y = (canvas_height - image:Height()) / 2
    image:cdCanvasPutImageRect(cd_canvas, x, y, image:Width(), image:Height(), 0, 0, 0, 0)
  end

  cd_canvas:Flush()
end

function canvas:map_cb()
  cd_canvas = cd.CreateCanvas(cd.IUPDBUFFER, canvas)
  canvas.cdCanvas = cd_canvas
end

function canvas:unmap_cb()
  local cd_canvas = canvas.cdCanvas
  cd_canvas:Kill()
end

function canvas:dropfiles_cb(filename)
  if (save_check(self)) then
    open_file(self, filename)
  end
end

function file_menu:open_cb()
  if (canvas.dirty) then
    item_save.active = "YES"
  else
    item_save.active = "NO"
  end
  if (canvas.dirty and canvas.filename) then
    item_revert.active = "YES"
  else
    item_revert.active = "NO"
  end
end

function edit_menu:open_cb()
  local clipboard = iup.clipboard{}
  if (clipboard.imageavailable == "NO") then
    item_paste.active = "NO"
  else
    item_paste.active = "YES"
  end
  clipboard:destroy()
end

function config:recent_cb()
  if (save_check(self)) then
    local filename = self.title
    open_file(self, filename)
  end
end

function item_new:action()
  if save_check(self) then
    local width = config:GetVariableDef("NewImage", "Width", 640)
    local height = config:GetVariableDef("NewImage", "Height", 480)

    local ret, new_width, new_height = iup.GetParam("New Image", nil, "Width: %i[1,]\nHeight: %i[1,]\n", width, height)
    if (ret) then
      local new_image = im.ImageCreate(new_width, new_height, im.RGB, im.BYTE)

      config:SetVariable("NewImage", "Width", new_width)
      config:SetVariable("NewImage", "Height", new_height)

      new_file(item_new, new_image)
    end
  end
end

function select_file(parent_dlg, is_open)
  local filedlg = iup.filedlg{
    extfilter="Image Files|*.bmp;*.jpg;*.png;*.tif;*.tga|All Files|*.*|",
    parentdialog = parent_dlg,
    directory = config:GetVariable("MainWindow", "LastDirectory"),
    }
    
  if (is_open) then
    filedlg.dialogtype = "OPEN"
  else
    filedlg.dialogtype = "SAVE"
    filedlg.file = canvas.filename
  end

  filedlg:popup(iup.CENTERPARENT, iup.CENTERPARENT)
  
  if (tonumber(filedlg.status) ~= -1) then
    local filename = filedlg.value
    if (is_open) then
      open_file(parent_dlg, filename)
    else
      saveas_file(canvas, filename)    
    end

    config:SetVariable("MainWindow", "LastDirectory", filedlg.directory)
  end
  
  filedlg:destroy()
end

function item_open:action()
  if not save_check(self) then
    return
  end

  select_file(dlg, true)
end

function item_saveas:action()
  select_file(dlg, false)
end

function item_save:action()
  if (not canvas.filename) then
    item_saveas:action()
  else
    -- test again because in can be called using the hot key
    if (canvas.dirty) then
      save_file(canvas)
    end
  end
end

function item_revert:action()
  open_file(self, canvas.filename)
end

function item_pagesetup:action()
  local width = config:GetVariableDef("Print", "MarginWidth", 20)
  local height = config:GetVariableDef("Print", "MarginHeight", 20)

  local ret, new_width, new_height = iup.GetParam("Page Setup", nil, "nMargin Width (mm): %i[1,]\nnMargin Height (mm): %i[1,]\n", width, height)
  if (ret) then
    config:SetVariable("Print", "MarginWidth", new_width)
    config:SetVariable("Print", "MarginHeight", new_height)
  end
end

function view_fit_rect(canvas_width, canvas_height, image_width, image_height)
  local view_width = canvas_width
  local view_height = (canvas_width * image_height) / image_width

  if (view_height > canvas_height) then 
    view_height = canvas_height
    view_width = (canvas_height * image_width) / image_height
  end
  
  return view_width, view_height
end

function item_print:action()
  local title = dlg.title
  local cd_canvas = cd.CreateCanvas(cd.PRINTER, title.." -d")
  if (not cd_canvas) then
    return
  end

  -- draw the background 
  local background = config:GetVariableDef("Canvas", "Background", "255 255 255")
  local r, g, b = string.match(background, "(%d*) (%d*) (%d*)")
  cd_canvas:Background(cd.EncodeColor(r, g, b))
  cd_canvas:Clear()

  -- draw the image at the center of the canvas
  local image = canvas.image
  if (image) then
    local margin_width = config:GetVariableDef("Print", "MarginWidth", 20)
    local margin_height = config:GetVariableDef("Print", "MarginHeight", 20)

    local canvas_width, canvas_height, canvas_width_mm, canvas_height_mm = cd_canvas:GetSize()

    -- convert to pixels
    margin_width = (margin_width * canvas_width) / canvas_width_mm
    margin_height = (margin_height * canvas_height) / canvas_height_mm

    local view_width, view_height = view_fit_rect(
       canvas_width - 2 * margin_width, canvas_height - 2 * margin_height, 
       image:Width(), image:Height())

    local x = (canvas_width - view_width) / 2
    local y = (canvas_height - view_height) / 2

    image:cdCanvasPutImageRect(cd_canvas, x, y, view_width, view_height, 0, 0, 0, 0)
  end

  cd_canvas:Kill()
end

function item_exit:action()
  local image = canvas.image

  if not save_check(self) then
    return iup.IGNORE  -- to abort the CLOSE_CB callback
  end

  if (image) then
    image:Destroy()
  end
  
  config:DialogClosed(iup.GetDialog(self), "MainWindow")
  config:Save()
  config:destroy()
  return iup.CLOSE
end

function item_copy:action()
  local clipboard = iup.clipboard{}
  -- must use iup.SetAttribute because it is an userdata
  iup.SetAttribute(clipboard, "NATIVEIMAGE", iup.GetImageNativeHandle(canvas.image))
  clipboard:destroy()
end

function item_paste:action()
  if save_check(self) then
    local clipboard = iup.clipboard{}
    local old_image = canvas.image

    local image = iup.GetNativeHandleImage(clipboard.nativeimage)

    -- we are going to support only RGB images with no alpha
    image:RemoveAlpha()
    if (image:ColorSpace() ~= im.RGB) then
      local new_image = im.ImageCreateBased(image, nil, nil, im.RGB, nil)        

      im.ConvertColorSpace(image, new_image)
      image:Destroy()

      image = new_image
    end

    image:SetAttribString("FileFormat", "JPEG")

    canvas.dirty = "Yes"
    canvas.image = image
    canvas.filename = nil
    dlg.title = "Untitled - Simple Paint"

    iup.Update(canvas)

    if (old_image) then
      old_image:Destroy()
    end

    clipboard:destroy()
  end
end

function item_background:action()
  local colordlg = iup.colordlg{}
  local background = config:GetVariableDef("Canvas", "Background", "255 255 255")
  colordlg.value = background
  colordlg.parentdialog = iup.GetDialog(self)

  colordlg:popup(iup.CENTERPARENT, iup.CENTERPARENT)

  if (tonumber(colordlg.status) == 1) then
    background = colordlg.value
    config:SetVariable("Canvas", "Background", background)

    iup.Update(canvas)
  end

  colordlg:destroy()
end

function item_toolbar:action()
  toggle_bar_visibility(self, toolbar)
  config:SetVariable("MainWindow", "Toolbar", item_toolbar.value)
end

function item_statusbar:action()
  toggle_bar_visibility(self, statusbar)
  config:SetVariable("MainWindow", "Statusbar", item_statusbar.value)
end

function item_help:action()
  iup.Help("http://www.tecgraf.puc-rio.br/iup")
end

function item_about:action()
  iup.Message("About", "   Simple Paint\n\nAutors:\n   Gustavo Lyrio\n   Antonio Scuri")
end


--********************************** Main (Part 2/2) *****************************************


btn_new = iup.button{image = "IUP_FileNew", flat = "Yes", action = item_new.action, canfocus="No", tip = "New (Ctrl+N)"}
btn_open = iup.button{image = "IUP_FileOpen", flat = "Yes", action = item_open.action, canfocus="No", tip = "Open (Ctrl+O)"}
btn_save = iup.button{image = "IUP_FileSave", flat = "Yes", action = item_save.action, canfocus="No", tip = "Save (Ctrl+S)"}
btn_copy = iup.button{image =  "IUP_EditCopy", flat = "Yes", action = item_copy.action, canfocus="No", tip = "Copy (Ctrl+C)"}
btn_paste = iup.button{image = "IUP_EditPaste", flat = "Yes", action = item_paste.action, canfocus="No", tip = "Paste (Ctrl+V)"}

toolbar = iup.hbox{
  btn_new,
  btn_open,
  btn_save,
  iup.label{separator="VERTICAL"},
  btn_copy,
  btn_paste,
  margin = "5x5",
  gap = 2,
}

vbox = iup.vbox{
  toolbar,
  canvas,
  statusbar,
}

dlg = iup.dialog{
  vbox,
  title = "Simple Paint",
  size = "HALFxHALF",
  menu = menu,
  close_cb = item_exit.action,
  canvas = canvas,
  dropfiles_cb = canvas.dropfiles_cb,
}

function dlg:k_any(c)
  if (c == iup.K_cN) then
    item_new:action()
  elseif (c == iup.K_cO) then
    item_open:action()
  elseif (c == iup.K_cS) then
    item_save:action()
  elseif (c == iup.K_cV) then
    item_paste:action()  
  elseif (c == iup.K_cC) then
    item_copy:action()  
  elseif (c == iup.K_cP) then
    item_print:action()  
  end
end

-- parent for pre-defined dialogs in closed functions (IupMessage and IupAlarm)
iup.SetGlobal("PARENTDIALOG", iup.SetHandleName(dlg))

-- Initialize variables from the configuration file

config:RecentInit(recent_menu, 10)
 
show_statusbar = config:GetVariableDef("MainWindow", "Statusbar", "ON")
if (show_statusbar == "OFF") then
  item_statusbar.value = "OFF"
  statusbar.floating = "YES"
  statusbar.visible = "NO"
end

show_toolbar = config:GetVariableDef("MainWindow", "Toolbar", "ON")
if (show_toolbar == "OFF") then
  item_toolbar.value = "OFF"
  toolbar.floating = "YES"
  toolbar.visible = "NO"
end

-- show the dialog at the last position, with the last size
config:DialogShow(dlg, "MainWindow")

-- open a file from the command line (allow file association in Windows)
if (arg and arg[1]) then
  filename = arg[1]
  open_file(dlg, filename)
end

-- initialize the current file, if not already loaded
check_new_file(dlg)

-- to be able to run this script inside another context
if (iup.MainLoopLevel()==0) then
  iup.MainLoop()
  iup.Close()
end
