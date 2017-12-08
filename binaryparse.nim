## This module implements a macro to create binary parsers. The parsers
## generated reads from a Stream and returns a tuple with each named field.
## The general format the macro takes is:
##
## ``[type]<size>: <name>[options]``
##
## Where optional fields are in [] brackets and required fields are in <>
## brackets. Each field has separate meanings, as described in the table below:
##
## ========== ==================================================================
## Name       Description
## ---------- ------------------------------------------------------------------
## type       This is the type of value found in this field, if no type is
##            specified then it will be parsed as an integer. Supported types
##            are ``u`` to get unsigned integers, ``f`` for floating point,
##            ``s`` for strings, and ``*`` for custom parser.
## size       The size, in *bits*, of the field to read. For uint and int values
##            from 1 to 64 inclusive are supported. For floats only 32 and 64
##            are supported. Strings use this field to specify the amount of
##            characters to read into the string. If they don't specify a size
##            they will be read to the first NULL byte (this only applies to
##            strings). When the custom parser type is specified the size field
##            is used to name the custom parser procedure.
## name       The name of the value, this will be used as the name in the
##            resulting tuple. If the value doesn't need to be stored one can
##            use ``_`` as the name and it will not get a field in the result.
## options    These will change the regular behaviour of reading into a field.
##            Since they are so different in what they do they are described
##            below instead of in this table.
## ========== ==================================================================
##
## Many binary formats include special "magic" sequences to identify the file
## or regions within it. The option ``= <value>`` can be used to check if a
## field has a certain value. If the value doesn't match a MagicError is
## raised. Value must match the value of the field it checks. When the field is
## a string type the exact length of the magic string is read, to include a
## terminating NULL byte use ``\0`` in the string literal.
##
## To read more fields of a certain kind into a sequence you can use the option
## ``[[count]]`` (that is square brackets with an optional count inside). If no
## count is specified and the brackets left empty the next field needs to be a
## magic number and will be used to terminate the sequence. As count you can use
## the name of any previous field, literals, previously defined variables, or a
## combination.
##
## Another thing commonly found in binary formats are repeating blocks or
## formats within the format. These can be read by using a custom parser.
## Custom parsers technically supports any procedure that takes a Stream as the
## first argument, however care must be taken to leave the Stream in the correct
## position. You can also define the inner format with a parser from this module
## and then pass that parser to the outer parser. This means that you can easily
## nest parsers. If you need values from the outer parser you can add parameters
## to the inner parser by giving it colon expressions before the body (e.g the
## call ``createParser(list, size: uint16)`` would create a parser
## ``proc list(stream: Stream, size: uint16): <return type>``). To call a parser
## use the ``*`` type as described above and give it the name of the parser and
## any optional arguments. The stream object will get added automatically as the
## first parameter.
##
## Example:
## In lieu of proper examples the binaryparse.nim file contains a ``when
## isMainModule()`` block showcasing how it can be used. The table below
## describes that block in a bit more detail:
##
## ======================= =====================================================
## Format                  Description
## ----------------------- -----------------------------------------------------
## ``u8: _ = 128``         Reads an unsigned 8-bit integer and checks if it
##                         equals 128 without storing the value as a field in
##                         returned tuple
## ``u16: size``           Reads an unsigned 16-bit integer and names it
##                         ``size`` in the returned tuple
## ``4: data[size*2]``     Reads a sequence of 4-bit integers into a ``data``
##                         field in the returned tuple. Size is the value read
##                         above, and denotes the count of integers to read.
## ``s: str[]``            Reads null terminated strings into a ``str`` field in
##                         the returned tuple. Since it's given empty brackets
##                         the next field needs to be a magic field and the
##                         sequence will be read until the magic is found.
## ``s: _ = "9xC\0"``      Reads a non-null terminated string and checks if it
##                         equals the magic sequence.
## ``*list(size): inner``  Uses a pre-defined procedure ``list`` which is called
##                         with the current Stream and the ``size`` read
##                         earlier. Stores the return value in a field ``inner``
##                         in the returned tuple.
## ``u8: _ = 67``          Reads an unsigned 8-bit integer and checks if it
##                         equals 67 without storing the value.
## ======================= =====================================================

import macros
import streams
import strutils

type
  MagicError* = object of Exception
    ## Error raised from the parser procedure when a magic sequence is not
    ## matching the specified value.

macro typeGetter*(body: typed): untyped =
  ## Helper macro to get the return type of custom parsers
  body.getTypeImpl[0][0]

proc replace(node: var NimNode, seenFields: seq[string]) =
  if node.kind == nnkIdent:
    if $node.ident in seenFields:
      node = newDotExpr(newIdentNode("result"), node)
  else:
    var i = 0
    while i < len(node):
      var n = node[i]
      n.replace(seenFields)
      node[i] = n
      inc i


proc decodeType(t: NimNode, stream: NimNode, seenFields: seq[string]):
  tuple[size: BiggestInt, kind: NimNode, custom: NimNode] =
  var
    size: BiggestInt
    kindPrefix: string
    custom: NimNode
  case t.kind:
    of nnkIntLit:
      size = t.intVal
      kindPrefix = "int"
    of nnkIdent:
      case ($t.ident)[0]:
        of 'u':
          size = ($t.ident)[1..^1].parseBiggestInt
          kindPrefix = "uint"
        of 'f':
          size = ($t.ident)[1..^1].parseBiggestInt
          kindPrefix = "float"
          if size != 32 and size != 64:
            raise newException(AssertionError,
              "Only 32 and 64 bit floats are supported")
        of 's':
          if ($t.ident).len != 1:
            size = ($t.ident)[1..^1].parseBiggestInt
          else:
            size = 0
          return (
            size: BiggestInt(size),
            kind: newIdentNode("string"),
            custom: nil
          )
        else:
          raise newException(AssertionError,
            "Format " & $t.ident & " not supported")
    of nnkCall:
      var
        t0 = t[0]
        customProc = newCall(t[0].ident, stream)
        retType = quote do:
          typeGetter(`t0`)
        i = 1
      while i < t.len:
        customProc.add(t[i])
        inc i
      customProc.replace(seenFields)
      return (size: BiggestInt(0), kind: retType, custom: customProc)
    else:
      raise newException(AssertionError,
        "Unknown kind: " & $t.kind)
  if size > 64:
    raise newException(AssertionError,
      "Unable to parse values larger than 64 bits")
  if size == 0:
    raise newException(AssertionError,
      "Unable to parse values with size 0")
  let containSize =
    if size > 32:
      64
    elif size > 16:
      32
    elif size > 8:
      16
    else:
      8
  return (
    size: size,
    kind: newIdentNode(kindPrefix & $containSize),
    custom: custom
  )


proc getBitInfo(size, offset: BiggestInt):
  tuple[read, skip, shift , mask: BiggestInt] =
  result.read = (size+7) div 8
  result.skip = (size+offset) div 8
  result.shift = result.read*8 - size - offset
  result.mask = (1 shl size) - 1


proc createReadStatement(
  field: NimNode,
  info: tuple[size: BiggestInt, kind: NimNode, custom: NimNode],
  offset: var BiggestInt, stream: NimNode): NimNode {.compileTime.} =
  let
    size = info.size
    custom = info.custom
  if custom != nil:
    result = (quote do:
      `field` = `custom`
    )
  elif info.kind.kind == nnkIdent and $info.kind.ident == "string":
    if offset != 0:
      raise newException(AssertionError, "Strings must be on byte boundry")
    if size == 0:
      result = (quote do:
        `field` = ""
        var c = `stream`.readChar()
        var i = 0
        while c != '\0':
          `field`.add(c)
          c = `stream`.readChar()
          inc i
      )
    else:
      result = (quote do:
        `field` = $`stream`.readStr(`size`)
      )
  else:
    let
      bitInfo = getBitInfo(size, offset)
      read = bitInfo.read
      skip = bitInfo.skip
      shift = bitInfo.shift
      mask = bitInfo.mask
    result = newStmtList()
    if read == skip:
      result.add(quote do:
        if `stream`.readData(`field`.addr, `read`) != `read`:
          raise newException(IOError,
            "Unable to read the requested amount of bytes from file")
      )
    else:
      result.add(quote do:
        if `stream`.peekData(`field`.addr, `read`) != `read`:
          raise newException(IOError,
            "Unable to peek the requested amount of bytes from file")
      )
    if skip != 0 and skip != read:
      result.add(quote do:
        `stream`.setPosition(`stream`.getPosition() + `skip`)
      )
    if (size != 8 and size != 16 and size != 32 and size != 64) or shift != 0:
      result.add(quote do:
        `field` = (`field` shr `shift`) and `mask`
      )
    offset += size
    offset = offset mod 8


macro createParser*(name: untyped, paramsAndDef: varargs[untyped]): untyped =
  ## The main macro in this module. It takes the ``name`` of the procedure to
  ## create along with a block on the format described above and creates a
  ## parser for it on the form ``proc <name>(Stream): tuple[<fields>]``
  let
    body = paramsAndDef[^1]
    res = newIdentNode("result")
    stream = genSym(nskParam)
  var
    inner = newStmtList()
    tupleMeat = nnkTupleTy.newNimNode
    i = 0
    field = 0
    offset: BiggestInt = 0
    seenFields = newSeq[string]()
    extraParams = newSeq[NimNode]()
  while i < paramsAndDef.len - 1:
    let p = paramsAndDef[i]
    if p.kind != nnkExprColonExpr:
      raise newException(AssertionError,
        "Extra arguments must be colon expressions")
    let s = parseStmt("proc hello(" & $p.toStrLit & ")")
    extraParams.add(s[0][3][1])
    inc i
  i = 0
  while i < body.len:
    var
      def = body[i]
    if def.kind == nnkPrefix:
      if def[1].kind != nnkCall:
        def = newCall(newCall(def[1]), def[2])
      else:
        def = newCall(def[1], def[2])
    var
      info = decodeType(def[0], stream, seenFields)
    let
      size = info.size
      kind = info.kind
    case def[1][0].kind:
      of nnkAsgn:
        let magic = def[1][0][1]
        var sym: NimNode
        if $def[1][0][0].ident == "_":
          sym = genSym(nskVar)
          inner.add(quote do:
            var `sym`: `kind`
          )
          dec field
        else:
          sym = (quote do: `res`[`field`])
          let
            resfield = newIdentNode($def[1][0][0])
          seenFields.add $def[1][0][0]
          tupleMeat.add(nnkIdentDefs.newTree(resfield, kind, newEmptyNode()))
        if $info.kind == "string":
          info.size = ($magic).len
        inner.add(createReadStatement(sym, info, offset, stream))
        inner.add(quote do:
          if `sym` != `magic`:
            raise newException(MagicError,
              "Magic with size " & $`size` &
              " didn't match value " & $`magic` & ", read: " & $`sym`)
        )
      of nnkBracketExpr:
        let
          sym = def[1][0][0]
          resfield = newIdentNode($sym)
        seenFields.add $sym
        tupleMeat.add(
          nnkIdentDefs.newTree(
            resfield,
            nnkBracketExpr.newTree(newIdentNode("seq"), kind),
            newEmptyNode()
          )
        )
        let
          ii = genSym(if def[1][0].len == 2: nskForvar else: nskVar)
          startOffset = offset
        var readFieldOps = @[
          createReadStatement(
            (quote do: `res`[`field`][`ii`]), info, offset, stream
          )
        ]
        while startOffset != offset:
          readFieldOps.add createReadStatement(
            (quote do: `res`[`field`][`ii`]), info, offset, stream
          )
        let readFieldCount = readFieldOps.len
        var readField = nnkCaseStmt.newTree(
          (quote do: `ii` mod `readFieldCount`)
        )
        var iii = 0
        for curField in readFieldOps:
          readField.add(nnkOfBranch.newTree(
            newLit(iii), curField
          ))
          inc iii
        readField.add(
          nnkElse.newTree(
            nnkDiscardStmt.newTree(nnkEmpty.newNimNode())
          )
        )
        if def[1][0].len == 2:
          var
            fields = def[1][0][1]
          fields.replace(seenFields)
          inner.add(quote do:
            `res`[`field`] = newSeq[`kind`](`fields`)
            for `ii` in 0..<(`fields`).int:
              `readField`
          )
        else:
          let endMagic = body[i+1]
          if endMagic[1][0].kind != nnkAsgn:
            raise newException(AssertionError,
              "Open arrays must be followed by magic for termination")
          let
            endInfo = decodeType(endMagic[0], stream, seenFields)
            magic = endMagic[1][0][1]
            endKind = endInfo.kind
            peek = newIdentNode("peek" & $endInfo.kind)
            bitInfo = getBitInfo(endInfo.size, offset)
            shift = bitInfo.shift
            mask = bitInfo.mask
          inner.add(quote do:
            `res`[`field`] = newSeq[`kind`]()
            var `ii` = 0
          )
          if $endInfo.kind != "string":
            inner.add(quote do:
              while ((`stream`.`peek`() shr
                    (((`readFieldCount` - 1) - (`ii` mod `readFieldCount`)) *
                    `shift`)) and `mask`).`endKind` != `magic`.`endKind`:
                `res`[`field`].setLen(`res`[`field`].len+1)
                `readField`
                inc `ii`
              `stream`.setPosition(`stream`.getPosition()+sizeof(`endKind`))
            )
            inc i
          else:
            inner.add(quote do:
              while `stream`.peekStr(len(`magic`)) != `magic`:
                `res`[`field`].setLen(`res`[`field`].len+1)
                `readField`
                inc `ii`
            )
        offset = 0
      of nnkIdent:
        if $def[1][0].ident == "_":
          if def[0].kind == nnkIdent and $def[0].ident == "s":
            inner.add(quote do:
              while (`stream`.readint8() != 0): discard
            )
            dec field
          else:
            inner.add(quote do:
              `stream`.setPosition(`stream`.getPosition()+`size` div 8)
            )
            offset += size
            offset = offset mod 8
            dec field
        else:
          let
            sym = def[1][0]
            resfield = newIdentNode($sym)
          seenFields.add $sym
          tupleMeat.add(nnkIdentDefs.newTree(resfield, kind, newEmptyNode()))
          inner.add(
            createReadStatement(
              (quote do: `res`[`field`]), info, offset, stream
            )
          )
      else:
        discard
    inc i
    inc field
  result = quote do:
    proc `name`(`stream`: Stream): `tupleMeat` =
      `inner`
  for p in extraParams:
    result[3].add p

  echo result.toStrLit

when isMainModule:
  createParser(list, size: uint16):
    u8: _
    u8: data[size]

  createParser(myParser):
    u8: _ = 128
    u16: size
    4: data[size*2]
    s: str[]
    s: _ = "9xC\0"
    *list(size): inner
    u8: _ = 67

  block parse:
    var fs = newFileStream("data.hex", fmRead)
    defer: fs.close()
    if not fs.isNil:
      var data = myParser(fs)
      echo data.size
      echo data.data
      echo data.str
      echo data.inner.data
