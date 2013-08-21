#Marshall and unmarshall data
import types

#For format, see camlpy.ml
TAG_TUPLE = 0
TAG_UNIT = 1
TAG_INT = 2
TAG_STRING = 3
TAG_BOOL = 4

#Flatten a list of lists
def flatten(ls):
  result = []
  for e in ls:
    result.extend(e)
  return result

#List of integer byte values for a string
def ints_of_string(s):
  result = []
  for e in s:
    result.append(ord(e))
  return result

#String of list of byte values
def string_of_ints(ints):
  return ("".join(map(chr, ints)))

#Make 4 bytes from an integer
def bytes_of_int(i):
  return [(i >> 24) % 256, (i & 0x00FF0000) >> 16, (i & 0x0000FF00) >> 8, i & 255]

#The inverse
def int_of_bytes(i0, i1, i2, i3):
  return ((i0 << 24) | (i1 << 16) | (i2 << 8) | i3)

#Marshall makes a list of numbers representing the
#bytes in the marshalled data. This is then converted to a string.
def marshall_inner(m):
  if type(m) is types.NoneType:
    return [TAG_UNIT]
  elif (m == True) and (type(m) is types.BooleanType):
    return [TAG_BOOL, 1]
  elif (m == False) and (type(m) is types.BooleanType):
    return [TAG_BOOL, 0]
  elif type(m) is types.IntType:
    tag = [TAG_INT]
    tag.extend(bytes_of_int(m))
    return tag
  elif type(m) is types.StringType:
    tag = [TAG_STRING]
    tag.extend (bytes_of_int(len(m)))
    tag.extend (ints_of_string (m))
    return tag
  elif type(m) is types.ListType:
    tail = flatten(map(marshall_inner, m))
    tag = [TAG_TUPLE]
    tag.extend(bytes_of_int(len(tail)))
    tag.extend(tail)
    return tag
  else:
    print m,type(m)
    raise Exception('Invalid Data')

def marshall(m):
  bytes = string_of_ints(marshall_inner(m))
  numbytes = string_of_ints(bytes_of_int(len(bytes)))
  return(numbytes + bytes)

#Unmarshall from string s at position pos
def unmarshall_inner(s, pos):
  if ord(s[pos]) == TAG_UNIT:
    return (None, pos + 1)
  elif ord(s[pos]) == TAG_BOOL:
    if ord(s[pos + 1]) == 0:
      return (False, pos + 2)
    else:
      return (True, pos + 2)
  elif ord(s[pos]) == TAG_INT:
    return (int_of_bytes (ord(s[pos + 1]), ord(s[pos + 2]), ord(s[pos + 3]), ord(s[pos + 4])), pos + 5)
  elif ord(s[pos]) == TAG_STRING:
    l = int_of_bytes (ord(s[pos + 1]), ord(s[pos + 2]), ord(s[pos + 3]), ord(s[pos + 4]))
    return (s[pos + 5:pos + 5 + l], pos + 5 + l)
  elif ord(s[pos]) == TAG_TUPLE:
    l = int_of_bytes (ord(s[pos + 1]), ord(s[pos + 2]), ord(s[pos + 3]), ord(s[pos + 4]))
    p = pos + 5
    vals = []
    while p < pos + l + 5:
      val, p = unmarshall_inner(s, p)
      vals.append(val)
    return (vals, p)
  else:
    raise Exception('Invalid data')

#Returns bytes taken, marshallable or None if not enough data
def unmarshall(s):
  return(unmarshall_inner(s, 0)[0])

