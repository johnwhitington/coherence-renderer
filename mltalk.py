#An analogue of pytalk.ml -- see that for documentation
import socket
import pycaml
import sys
import os


#Buffer for incoming data
buffer = ""

#Returns send, poll, close
def establish_connection (port):
  print("PY: Connecting to Caml on port ", str(port))
  sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  sock.connect(('localhost', port))
  print("PY: Connected.")
  #sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 0)
  def send(message):
    marshalled = pycaml.marshall(message)
    while marshalled != "":
      try:
        done = sock.send(marshalled)
        marshalled = marshalled[done:]
      except:
        print("PY: Connection broken trying to send")
        sock.close()
        sys.exit(2)
  def poll():
    #Ask for four bytes: this is the event size
    s = sock.recv(4)
    if not s:
      sock.close()
    datalength = pycaml.int_of_bytes(ord(s[0]), ord(s[1]), ord(s[2]), ord(s[3]))
    #Now ask as many as we need until we've got them
    chunks = []
    while datalength > 0:
      data = sock.recv(datalength)
      if not data:
        sock.close()
        break
      chunks.append(data)
      datalength -= len(data)
    #unmarshall and return
    alldata = ''.join(chunks)
    event = pycaml.unmarshall(alldata)
    return(event)
  def close():
    print("PY: close() called, closing socket")
    sock.close()
  return (send, poll, close)


#Same, but python starts, picks the port and calls OCaml. The argument is the
#command to start the ocaml executable and the intial try port number.
def pystarts_establish_connection_inner (port):
  #Create a socket
  sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  #Bind it to loopback and port
  sock.bind(('localhost', port))
  #Listen on it
  sock.listen(1)
  runengine(port)
  #Accept the connection from Ocaml
  print("PY: runengine called, now trying to accept connection")
  sock.accept()
  print("PY: sock.accept() returned")
  #Build send, poll, close
  def send(message):
    marshalled = pycaml.marshall(message)
    while marshalled != "":
      try:
        done = sock.send(marshalled)
        marshalled = marshalled[done:]
      except:
        print("PY: Connection broken trying to send")
        sock.close()
        sys.exit(2)
  def poll():
    #Ask for four bytes: this is the event size
    s = sock.recv(4)
    if not s:
      sock.close()
    datalength = pycaml.int_of_bytes(ord(s[0]), ord(s[1]), ord(s[2]), ord(s[3]))
    #Now ask as many as we need until we've got them
    chunks = []
    while datalength > 0:
      data = sock.recv(datalength)
      if not data:
        sock.close()
        break
      chunks.append(data)
      datalength -= len(data)
    #unmarshall and return
    alldata = ''.join(chunks)
    event = pycaml.unmarshall(alldata)
    return(event)
  def close():
    print("PY: close() called, closing socket")
    sock.close()
  return (send, poll, close)

#For now, doing it with a temp file, so ignore above...

#Create the Ocaml process, returning immediately
#On Windows, a native code executable will be called "main.exe", a bytecode "main"
#If main.exe doesn't exist, then, we call "ocamlrun main"
#On Mac / Unix we can just call "./main" in either case
def runengine():
  #if os.name == 'nt' or os.name == 'dos':
   # if os.path.exists('engine.exe'):
   #   exename = 'engine.exe'
   # else:
   #   exename = 'ocamlrun engine'
  #else:
  #  exename = './engine'
  #os.system(exename + ' ' + str(port))
  os.spawnv(os.P_NOWAIT, 'engine', ('engine', 'findport'))

def pystarts_establish_connection ():
  print("PY: pystarts_establish_connection ")
  #Delete any file 'findport'
  try:
    os.remove('findport')
  except OSError:
    pass
  #Run the engine
  runengine()
  #Wait until there is a file 'findport' which we can open
  found = 0
  while found == 0:
    try:
      f = open('findport', 'rb')
      print("PYSTARTS:got a file findport!")
      found = 1
      port = f.readline ()
      print(int(port))
      return (establish_connection (int(port)))
    except IOError:
      pass

