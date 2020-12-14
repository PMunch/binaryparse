import ../binaryparse, bitstreams, unittest

suite "Aligned":
  createParser(p):
    16: beword
    l32: ledword
    f32: befloat
    lf64: ledouble
    s24: str
    s: term
  var fbs = newFileBitStream("tests/aligned.hex")
  defer: close(fbs)
  var data: typeGetter(p)
  try: data = p.get(fbs)
  except:
    echo getCurrentExceptionMsg()
    fail()
  test "integers":
    check data.beword == 0x1234
    check data.ledword == 0x1234_5678
  test "floats":
    check data.befloat == 0x1234_5678'f32
    check data.ledouble == 0x1234_5678_90AB_CDEF'f64
  test "strings":
    check data.str == "ABC"
    check data.term == "DEF"
  test "serialization":
    var sbs = newStringBitStream()
    defer: close(sbs)
    try:
      p.put(sbs, data)
      sbs.seek(0)
    except:
      echo getCurrentExceptionMsg()
      fail()
    check data == p.get(sbs)

suite "Unaligned":
  createParser(p):
    1: a
    5: b
    10: c
    r12: d
    r20: e
    7: f
    64: g
    57: h

  var fbs = newFileBitStream("tests/unaligned.hex")
  defer: close(fbs)
  var data: typeGetter(p)
  try: data = p.get(fbs)
  except:
    echo getCurrentExceptionMsg()
    fail()
  test "values":
    check data.a == 1
    check data.b == 5
    check data.c == 10
    check data.d == 12
    check data.e == 20
    check data.f == 7
    check data.g == 64
    check data.h == 57
  test "serialization":
    var sbs = newStringBitStream()
    defer: close(sbs)
    try:
      p.put(sbs, data)
      sbs.seek(0)
    except:
      echo getCurrentExceptionMsg()
      fail()
    check data == p.get(sbs)

suite "Complex":
  createParser(inner):
    8: x
  createParser(innerWithArgs, a: int8, b: typeGetter(inner)):
    8: x = a
    8: y = b.x
  createParser(outer):
    8: x
    *inner: y
    *innerWithArgs(x, y): z
  var fbs = newFileBitStream("tests/complex.hex")
  defer: close(fbs)
  var data: typeGetter(outer)
  try: data = outer.get(fbs)
  except:
    echo getCurrentExceptionMsg()
    fail()
  test "serialization":
    var sbs = newStringBitStream()
    defer: close(sbs)
    try:
      outer.put(sbs, data)
      sbs.seek(0)
    except:
      echo getCurrentExceptionMsg()
      fail()
    check data == outer.get(sbs)

suite "Repetition":
  createParser(p):
    8: size
    4: nibbles[size]
    8: bytes{e == 2}
    2: duets{2*i > 7}
    3: trios{s.atEnd}
  var fbs = newFileBitStream("tests/repetition.hex")
  defer: close(fbs)
  var data: typeGetter(p)
  try: data = p.get(fbs)
  except:
    echo getCurrentExceptionMsg()
    fail()
  test "for":
    check data.nibbles == @[0'i8, 1, 2, 3]
  test "until":
    check data.bytes == @[0'i8, 1, 2]
    check data.duets == @[0'i8, 1, 2, 3]
    check data.trios == @[3'i8, 4, 5, 6, 7, 0, 1, 2]
  test "serialization":
    var sbs = newStringBitStream()
    defer: close(sbs)
    try:
      p.put(sbs, data)
      sbs.seek(0)
    except:
      echo getCurrentExceptionMsg()
      fail()
    check data == p.get(sbs)

suite "Assertions":
  createParser(inner):
    8: bytes[4]
  createParser(outer):
    s24: str = "ABC"
    8: x = 1
    8: y = 2
    8: z = x + y
    *inner: bytes = (@[0'i8, 1, 2, 3],)
  var fbs = newFileBitStream("tests/assertions.hex")
  defer: close(fbs)
  var data: typeGetter(outer)
  try: data = outer.get(fbs)
  except:
    echo getCurrentExceptionMsg()
    fail()
  test "serialization":
    var sbs = newStringBitStream()
    defer: close(sbs)
    try:
      outer.put(sbs, data)
      sbs.seek(0)
    except:
      echo getCurrentExceptionMsg()
      fail()
    check data == outer.get(sbs)

suite "Unnamed fields":
  createParser(p):
    16: _ = 0x1234
    l32: _
    f32: _
    lf64: _
    s24: _
    s: _ = "DEF"
  var fbs = newFileBitStream("tests/aligned.hex")
  defer: close(fbs)
  var data: typeGetter(p)
  try: data = p.get(fbs)
  except:
    echo getCurrentExceptionMsg()
    fail()
  test "serialization":
    var sbs = newStringBitStream()
    defer: close(sbs)
    try:
      p.put(sbs, data)
      sbs.seek(0)
    except:
      echo getCurrentExceptionMsg()
      fail()
    check data == p.get(sbs)

suite "Parser options":
  createParser(p, endian = l, bitEndian = r):
    16: little
    b16: big
    4: first
    4: second
    n4: third
    n4: fourth
  var fbs = newFileBitStream("tests/options.hex")
  defer: close(fbs)
  var data: typeGetter(p)
  try: data = p.get(fbs)
  except:
    echo getCurrentExceptionMsg()
    fail()
  test "endians":
    check data.little == 128
    check data.big == -32768
  test "bit-endians":
    check data.first == 1
    check data.second == 2
    check data.third == 3
    check data.fourth == 4
  test "serialization":
    var sbs = newStringBitStream()
    defer: close(sbs)
    try:
      p.put(sbs, data)
      sbs.seek(0)
    except:
      echo getCurrentExceptionMsg()
      fail()
    check data == p.get(sbs)