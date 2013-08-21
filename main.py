#Main python file
import sys
import os
import platform
import mltalk

#Otherwise, we're responding to the ocaml side: make the GUI.
import wx
import thread

#Make the application
app = wx.PySimpleApp()

frame = wx.Frame(None, wx.ID_ANY, "moo")

#Build the application icon
icon = wx.Icon("winicon.ico", wx.BITMAP_TYPE_ICO)

#For OS X, need to build a tbicon
#if os.name != 'nt' and os.name != 'dos':
#  tbicon = wx.TaskBarIcon()
#  tbicon.SetIcon(icon, 'Coherence Demo')

#We need to create a dummy window to get wxpython to initialise properly
#frame = wx.Frame(None, wx.ID_ANY, "moo")

#The dictionary of window ID, window instance pairs
frames = {}
reverse_frames = {}

#Other things, keyed on window
scrolls = {}
buffers = {}

#Unique source of IDs for newframe
id = 0

#Callables used from within the dispatcher.
#They are sent with wx.CallAfter so they happen in the GUI thread.
def getscreensize():
  w, h = wx.GetDisplaySize()
  send (["Internal", "ScreenSize", w, h])

def windowwidth(win):
  w, h = scrolls[win].GetClientSizeTuple()
  send (["Internal", "WindowWidth", w])

def windowheight(win):
  w, h = scrolls[win].GetClientSizeTuple()
  send (["Internal", "WindowHeight", h])

def windowscroll(win):
  x, y = scrolls[win].CalcUnscrolledPosition (0, 0)
  send (["Internal", "WindowScroll", x, y])

#Return the platform we're running on.
def getplatform():
  if platform.system() == 'Darwin':
    p = "Mac"
  else:
    p = "Windows"
  send (["Internal", "Platform", p])

def capturemouse(win):
  #frames[win].CaptureMouse()
  pass

def releasemouse(win):
  #frames[win].ReleaseMouse()
  pass

# Return the window ID
def makewindow(name, sx, sy, dx, dy, ex, ey, hastoolbar):
  global id
  w = MainWindow(None, wx.ID_ANY, name, sx, sy, dx, dy, ex, ey, hastoolbar)
  frames[id] = w
  reverse_frames[w] = id
  scrolls[id] = w.scroll
  bitmap = wx.EmptyBitmap(1280, 1024)
  #Initiate to a nice grey colour
  dc = wx.MemoryDC(bitmap=bitmap)
  dc.SetBrush(wx.Brush((211,211,211), wx.SOLID))
  dc.DrawRectangle(-1, -1, 1280, 1024)
  dc.SelectObject(wx.NullBitmap)
  del dc
  #build buffer
  buffers[id] = wx.ImageFromBitmap(bitmap)
  w.Show(True)
  send(["Internal", "MakeWindow", id])
  id = id + 1

# Window close initiated from ML
def closewindow(w):
  try:
    fr = frames[w]
    fr.Close()
    send(["Internal", "CloseWindow"])
  except KeyError:
    pass

# Copy data from the an ocaml string to the window
def refreshwindow(win, x, y, w, h, str):
  try:
    i = wx.ImageFromData(w, h, str)
    buffers[win].Paste(i, x, y)
    dc = wx.ClientDC(scrolls[win])
    x, y = scrolls[win].CalcScrolledPosition (x, y)
    dc.DrawBitmap(wx.Image.ConvertToBitmap(i), x, y)
    send(["Internal", "RefreshWindow"])
  except KeyError:
    send(["Internal", "RefreshWindow"])

ID_ABOUT = 101
ID_EXIT = 110
ID_OPEN = 102

select_icon = wx.EmptyBitmap(1, 1)
zoom_icon = wx.EmptyBitmap(1, 1)
totop_icon = wx.EmptyBitmap(1, 1)
tobottom_icon = wx.EmptyBitmap(1, 1)

#Make an image from the raw data, and paste to a bitmap.
def bitmap_of_icon(icon, alpha, w, h):
  image = wx.EmptyImage(w, h)
  image.SetData(icon)
  image.InitAlpha()
  image.SetAlphaData(alpha)
  return(wx.Image.ConvertToBitmap(image)) 

def seticons(select, sa, zoom, za, totop, ta, tobottom, ba):
  global select_icon, zoom_icon, totop_icon, tobottom_icon
  select_icon = bitmap_of_icon(select, sa, 32, 32)
  zoom_icon = bitmap_of_icon(zoom, za, 32, 32)
  totop_icon = bitmap_of_icon(totop, ta, 32, 32)
  tobottom_icon = bitmap_of_icon(tobottom, ba, 32, 32)

class CacheWindow(wx.Frame):
  def __init__(self, parent, id, title):
    wx.Frame.__init__(self, parent, id, title, size=(200, 400))
    self.textctrl = wx.TextCtrl(self, 1, style=wx.TE_MULTILINE | wx.TE_READONLY)
    self.textctrl.SetFont(wx.Font(12, wx.FONTFAMILY_MODERN, wx.NORMAL, wx.NORMAL))

class MainWindow(wx.Frame):
  def __init__(self, parent, id, title, sx, sy, dx, dy, ex, ey, hastoolbar):
    wx.Frame.__init__(self, parent, id, title, size=(sx, sy), pos=(dx, dy))
    self.scroll = wx.ScrolledWindow(self, -1, style=wx.NO_FULL_REPAINT_ON_RESIZE)
    self.scroll.SetScrollbars(20, 20, ex/20, ey/20)
    self.scroll.SetBackgroundStyle(wx.BG_STYLE_CUSTOM); 
    self.CreateStatusBar()
    #Menus
    self.cx, self.cy = 0, 0
    menubar = wx.MenuBar()
    filemenu = wx.Menu()
    helpmenu = wx.Menu()
    open_item = filemenu.Append(ID_OPEN, "&Open...\tCtrl-O", " Open a file")
    quit_item = filemenu.Append(ID_EXIT, "E&xit\tAlt-X", " Exit the program")
    about_item = helpmenu.Append(ID_ABOUT, "&About...", " Coherence Renderer")
    menubar.Append(helpmenu, "&Help")
    menubar.Append(filemenu, "&File")
    self.SetMenuBar(menubar)
    self.Bind(wx.EVT_MENU, self.OnExit, quit_item)
    self.Bind(wx.EVT_MENU, self.OnAbout, about_item)
    self.Bind(wx.EVT_MENU, self.OnOpen, open_item)
    #Toolbars
    if hastoolbar:
      toolBar = self.CreateToolBar(wx.TB_HORIZONTAL, -1, "toolBar")
      toolBar.SetToolBitmapSize((32, 32))
      toolBar.AddSimpleTool(1, select_icon, "Select")
      #toolBar.AddSimpleTool(2, zoom_icon, "Zoom")
      toolBar.AddSimpleTool(3, totop_icon, "Put to top")
      toolBar.AddSimpleTool(4, tobottom_icon, "Put to bottom")
      toolBar.AddSeparator()
      text = wx.StaticText(toolBar, -1, "Blur ")
      toolBar.AddControl(text)
      self.slider = wx.Slider(toolBar, 5, 0, 0, 10, wx.DefaultPosition, wx.Size(100, -1), wx.SL_HORIZONTAL)
      toolBar.AddControl(self.slider)
      transtext = wx.StaticText(toolBar, -1, "Opacity ")
      toolBar.AddControl(transtext)
      self.transslider = wx.Slider(toolBar, 6, 0, 0, 255, wx.DefaultPosition, wx.Size(100, -1), wx.SL_HORIZONTAL)
      toolBar.AddControl(self.transslider)
      toolBar.Realize()
    self.Bind(wx.EVT_SLIDER, self.OnBlurSlider, id=5)
    self.Bind(wx.EVT_SLIDER, self.OnTransSlider, id=6)
    self.Bind(wx.EVT_TOOL, self.OnSelectToolClick, id=1) 
    self.Bind(wx.EVT_TOOL, self.OnZoomToolClick, id=2) 
    self.Bind(wx.EVT_TOOL, self.OnTopToolClick, id=3) 
    self.Bind(wx.EVT_TOOL, self.OnBottomToolClick, id=4)
    self.Bind(wx.EVT_CLOSE, self.OnClose)
    self.scroll.Bind(wx.EVT_KEY_DOWN, self.OnKeyDown)
    self.scroll.Bind(wx.EVT_KEY_UP, self.OnKeyUp)
    self.scroll.Bind(wx.EVT_LEFT_DOWN, self.OnLeftDown)
    self.scroll.Bind(wx.EVT_LEFT_UP, self.OnLeftUp)
    self.scroll.Bind(wx.EVT_RIGHT_DOWN, self.OnRightDown)
    self.scroll.Bind(wx.EVT_RIGHT_UP, self.OnRightUp)
    self.scroll.Bind(wx.EVT_MOTION, self.OnLeftDragging)
    self.scroll.Bind(wx.EVT_ERASE_BACKGROUND, self.OnEraseBackground)
    self.scroll.Bind(wx.EVT_PAINT, self.OnPaint)
    self.SetIcon(icon)
  def OnBlurSlider(self, e):
    w = reverse_frames[self]
    send(["BlurSlider", w, self.slider.GetValue()])
  def OnTransSlider(self, e):
    w = reverse_frames[self]
    send(["TransSlider", w, self.transslider.GetValue()])
  def OnClose(self, e):
    #cachewindow.Close(True)
    w = reverse_frames[self]
    send(["CloseWindow", w])
    del frames[w]
    del reverse_frames[self]
    del scrolls[w]
    del buffers[w]
    self.Destroy()
    e.Skip()
  def OnEraseBackground(self, e):
    dc = wx.ClientDC(self.scroll)
    x, y = self.scroll.CalcScrolledPosition (0, 0)
    dc.DrawBitmap(wx.Image.ConvertToBitmap(buffers[reverse_frames[self]]), x, y)
  def OnPaint(self, e):
    dc = wx.PaintDC(self.scroll) #We must create a PaintDC even if we don't use it.
    i = wx.RegionIterator(self.scroll.GetUpdateRegion())
    while i:
      win = reverse_frames[self]
      x, y = scrolls[win].CalcUnscrolledPosition((i.GetX (), i.GetY ()))
      send(["PaintRect", win, x, y, i.GetWidth(), i.GetHeight()])
      i.Next()
  def OnOpen(self, e):
    filters = "PDF files (*.pdf)|*.pdf|All files (*.*)|*.*"
    dialog = wx.FileDialog(None, style = wx.OPEN | wx.MULTIPLE, wildcard = filters)
    if dialog.ShowModal() == wx.ID_OK:
      files = dialog.GetPaths()
      for file in files:
        send(["OpenFile", file.encode('ascii', 'ignore')])
  def OnSelectToolClick(self, e):
    send(["Button", 1, reverse_frames[self]])
  def OnZoomToolClick(self, e):
    send(["Button", 2, reverse_frames[self]])
  def OnTopToolClick(self, e):
    send(["Button", 3, reverse_frames[self]])
  def OnBottomToolClick(self, e):
    send(["Button", 4, reverse_frames[self]])
  def OnAbout(self, e):
    d = wx.MessageDialog(self, " Coherence Renderer Demo", "About This Program", wx.OK)
    d.ShowModal()
    d.Destroy()
  def OnExit(self, e):
    #if os.name != 'nt' and os.name != 'dos':
    #  tbicon.Destroy()
    send(["AppClose"])
  def OnKeyDown(self, e):
    send(["Shift", e.ShiftDown()])
    send(["Command", e.CmdDown()])
    send(["Option", e.AltDown()])
    send(["KeyDown", reverse_frames[self], e.GetKeyCode()])
  def OnKeyUp(self, e):
    send(["Shift", e.ShiftDown()])
    send(["Command", e.CmdDown()])
    send(["Option", e.AltDown()])
    send(["KeyUp", reverse_frames[self], e.GetKeyCode()])
  def OnLeftDown(self, e):
    pt = e.GetPosition();
    pt = self.scroll.CalcUnscrolledPosition(pt)
    self.cx = pt.x
    self.cy = pt.y
    send(["LeftDown", reverse_frames[self], self.cx, self.cy])
  def OnLeftUp(self, e):
    pt = e.GetPosition();
    pt = self.scroll.CalcUnscrolledPosition(pt)
    self.cx = pt.x
    self.cy = pt.y
    send(["LeftUp", reverse_frames[self], self.cx, self.cy])
  def OnRightDown(self, e):
    pt = e.GetPosition();
    pt = self.scroll.CalcUnscrolledPosition(pt)
    self.cx = pt.x
    self.cy = pt.y
    send(["RightDown", reverse_frames[self], self.cx, self.cy])
  def OnRightUp(self, e):
    pt = e.GetPosition();
    pt = self.scroll.CalcUnscrolledPosition(pt)
    self.cx = pt.x
    self.cy = pt.y
    send(["RightUp", reverse_frames[self], self.cx, self.cy])
  def OnLeftDragging(self, e):
    pt = e.GetPosition();
    pt = self.scroll.CalcUnscrolledPosition(pt)
    self.cx = pt.x
    self.cy = pt.y
    send(["LeftDragging", reverse_frames[self], self.cx, self.cy])

#Establish the connection to OCaml
#If no port number is given, we're initiating the start up procedure
#from the python side, so call the ocaml executable and exit.
if len(sys.argv) == 1:
  print "calling engine.exe findport - we're starting from python"
  send, poll, close = mltalk.pystarts_establish_connection()
else:
  print "calling normal establish_connection"
  send, poll, close = mltalk.establish_connection(int(sys.argv[1]))

def setstatusbar(win, text):
  try:
    wx.CallAfter(frames[win].SetStatusText, text)
  except KeyError:
    pass

def setblurslider(win, val):
  try:
    wx.CallAfter(frames[win].slider.SetValue, val)
  except KeyError:
    pass

def settransslider(win, val):
  try:
    wx.CallAfter(frames[win].transslider.SetValue, val)
  except KeyError:
    pass

def getmousenow(win):
  try:
    send(["Internal", "MouseNow", frames[win].cx, frames[win].cy])
  except KeyError:
    pass

# Dispatcher for responses coming from OCaml
def dispatch(obj):
  if obj[0] == "SetStatusBar":
    setstatusbar(obj[1], obj[2])
  elif obj[0] == "OpenCacheWindow":
    wx.CallAfter(opencachewindow)
  elif obj[0] == "SetCacheText":
    wx.CallAfter(setcachetext, obj[1])
  elif obj[0] == "MouseNow":
    wx.CallAfter(getmousenow, obj[1])
  elif obj[0] == "SetBlurSlider":
    setblurslider(obj[1], obj[2])
  elif obj[0] == "SetTransSlider":
    settransslider(obj[1], obj[2])
  elif obj[0] == "ScreenSize":
    wx.CallAfter(getscreensize)
  elif obj[0] == "Platform":
    wx.CallAfter(getplatform)
  elif obj[0] == "MakeWindow":
    wx.CallAfter(makewindow, obj[1], obj[2], obj[3], obj[4], obj[5], obj[6], obj[7], obj[8])
  elif obj[0] == "WindowWidth":
    wx.CallAfter(windowwidth, obj[1])
  elif obj[0] == "WindowHeight":
    wx.CallAfter(windowheight, obj[1])
  elif obj[0] == "WindowScroll":
    wx.CallAfter(windowscroll, obj[1])
  elif obj[0] == "CaptureMouse":
    wx.CallAfter(capturemouse, obj[1])
  elif obj[0] == "ReleaseMouse":
    wx.CallAfter(releasemouse, obj[1])
  elif obj[0] == "RefreshWindow":
    wx.CallAfter(refreshwindow, obj[1], obj[2], obj[3], obj[4], obj[5], obj[6])
  elif obj[0] == "CloseWindow":
    wx.CallAfter(closewindow, obj[1])
  elif obj[0] == "AppClose":
    #tbicon.Destroy()
    frame.Close(True)
    #self.Destroy()
  elif obj[0] == "Startup":
    print "PY: Application startup data received"
    sys.stdout.flush()
    seticons(obj[1], obj[2], obj[3], obj[4], obj[5], obj[6], obj[7], obj[8])
  else:
    print "PY: Unknown message from caml..."
    print obj[0]
    sys.stdout.flush()

def poll_repeatedly():
  try:
    while 1:
      dispatch(poll())
  except:
    print "PY: Killed comms thread"

sockthread = thread.start_new_thread(poll_repeatedly, ())

def setcachetext(text):
  global cachewindow
  cachewindow.textctrl.Clear()
  cachewindow.textctrl.AppendText(text)
  cachewindow.textctrl.SetInsertionPoint(0)

def opencachewindow():
  global cachewindow
  cachewindow = CacheWindow(None, wx.ID_ANY, "Cache Statistics")
  cachewindow.Show(True)

# Main code, polling for the initial AppStart request
try:
  print "PY: Sending AppStart...."
  send(["AppStart"])
  app.MainLoop()
  print "PY: MainLoop ended"
finally:
  print "PY: ERROR IN MAIN LOOP, SHUTTING DOWN"
  close()

print "PY: Startup finisihed, main thread has reached end of file."


