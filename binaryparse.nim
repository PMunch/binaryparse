## This module implements a macro to create binary parsers. The parsers
## generated reads from a Stream and returns a tuple with each named field.
## The general format the macro takes is:
##
## ``[endian][type]<size>: <name>[options]``
##
## Where optional fields are in [] brackets and required fields are in <>
## brackets. Each field has separate meanings, as described in the table below:
##
## ========== ==================================================================
## Name       Description
## ---------- ------------------------------------------------------------------
## endian     This controls whether the bytes will be read with big or little
##            endian convention. Possible values are ``l`` for little endian
##            and ``b`` for big endian. If not specified then big endian is
##            used.
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
## count is specified and the brackets left empty it must be the last field or
## the next field needs to be a magic number and will be used to terminate the
## sequence. If it is the last field it will read until the end of the stream.
## As count you can use the name of any previous field, literals, previously
## defined variables, or a combination. Note that all sequences are assumed to
## terminate on a byte border, even if given a statically evaluatable size.
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
## ``proc (stream: Stream, size: uint16): <return type>``). To call a parser
## use the ``*`` type as described above and give it the name of the parser and
## any optional arguments. The stream object will get added automatically as the
## first parameter.
##
## When creating a parser you get a tuple with two members, ``get`` and ``put``
## which is stored by a let as the identifier given when calling createParser.
## These are both procedures, the first only takes a stream (and any optional
## arguments as described above) and returns a tuple containing all the fields.
## The second takes a stream and a tuple containing all the fields, this is the
## same tuple returned by the ``get`` procedure and writes the format to the
## stream.
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
  MagicError* = object of Defect
    ## Error raised from the parser procedure when a magic sequence is not
    ## matching the specified value.

macro typeGetter*(body: typed): untyped =
  ## Helper macro to get the return type of custom parsers
  body.getTypeImpl[0][1][0][0]

template writeDataBE*(stream: Stream, buffer: pointer, size: int) =
  for i in 0..<size:
    let tmp = cast[pointer](cast[int](buffer) + ((size-1)-i))
    stream.writeData(tmp, 1)

template writeDataLE*(stream: Stream, buffer: pointer, size: int) =
  for i in 0..<size:
    let tmp = cast[pointer](cast[int](buffer) + i)
    stream.writeData(tmp, 1)

template readDataBE*(stream: Stream, buffer: pointer, size: int) =
  for i in 0..<size:
    let tmp = cast[pointer](cast[int](buffer) + ((size-1)-i))
    if stream.readData(tmp, 1) != 1:
      raise newException(IOError,
        "Unable to read the requested amount of bytes from file")

template readDataLE*(stream: Stream, buffer: pointer, size: int) =
  for i in 0..<size:
    let tmp = cast[pointer](cast[int](buffer) + i)
    if stream.readData(tmp, 1) != 1:
      raise newException(IOError,
        "Unable to read the requested amount of bytes from file")

template peekDataBE*(stream: Stream, buffer: pointer, size: int) =
  let startPos = stream.getPosition()
  for i in 0..<size:
    let tmp = cast[pointer](cast[int](buffer) + ((size-1)-i))
    if stream.readData(tmp, 1) != 1:
      raise newException(IOError,
        "Unable to peek the requested amount of bytes from file")
  stream.setPosition(startPos)

template peekDataLE*(stream: Stream, buffer: pointer, size: int) =
  let startPos = stream.getPosition()
  for i in 0..<size:
    let tmp = cast[pointer](cast[int](buffer) + i)
    if stream.readData(tmp, 1) != 1:
      raise newException(IOError,
        "Unable to peek the requested amount of bytes from file")
  stream.setPosition(startPos)

proc replace(node: var NimNode, seenFields: seq[string], parent: NimNode) =
  if node.kind == nnkIdent:
    if node.strVal in seenFields:
      node = newDotExpr(parent, node)
  else:
    var i = 0
    while i < len(node):
      var n = node[i]
      n.replace(seenFields, parent)
      node[i] = n
      inc i


proc decodeType(t: NimNode, stream: NimNode, seenFields: seq[string]):
  tuple[size: BiggestInt; endian: Endianness; kind, customReader, customWriter: NimNode] =
  const defaultEndian = bigEndian
  var
    size: BiggestInt
    endian: Endianness
    ensureIsByteMult: bool
    kindPrefix: string
    customReader, customWriter: NimNode
  case t.kind:
    of nnkIntLit:
      size = t.intVal
      endian = defaultEndian
      kindPrefix = "int"
    of nnkIdent:
      var typ = t.strVal

      case typ[0]
      of 'l':
        endian = littleEndian
        ensureIsByteMult = true
        typ.delete(0, 0)
      of 'b':
        endian = bigEndian
        ensureIsByteMult = true
        typ.delete(0, 0)
      else:
        endian = defaultEndian

      case typ[0]
      of 'u':
        kindPrefix = "uint"
        size = typ[1..^1].parseBiggestInt
      of 'f':
        kindPrefix = "float"
        size = typ[1..^1].parseBiggestInt
        if size != 32 and size != 64:
          raise newException(Defect, "Only 32 and 64 bit floats are supported")
      of 's':
        if typ.len != 1:
          size = typ[1..^1].parseBiggestInt
        else:
          size = 0
        return (
          size: BiggestInt(size),
          endian: defaultEndian,
          kind: newIdentNode("string"),
          customReader: nil,
          customWriter: nil
        )
      else:
        try: size = typ.parseBiggestInt
        except ValueError:
          raise newException(Defect, "Format " & t.strVal & " not supported")
        kindPrefix = "int"
    of nnkCall:
      var
        t0 = t[0]
        customProcRead = newCall(nnkDotExpr.newTree(t[0], newIdentNode("get")), stream)
        customProcWrite = newCall(nnkDotExpr.newTree(t[0], newIdentNode("put")), stream)
        retType = quote do:
          typeGetter(`t0`)
        i = 1
      while i < t.len:
        var
          readArg = t[i].copyNimTree
          writeArg = t[i].copyNimTree
        customProcRead.add(readArg)
        customProcWrite.add(writeArg)
        inc i
      customProcRead.replace(seenFields, newIdentNode("result"))
      customProcWrite.replace(seenFields, newIdentNode("input"))
      return (
        size: BiggestInt(0),
        endian: defaultEndian,
        kind: retType,
        customReader: customProcRead,
        customWriter: customProcWrite
      )
    else:
      raise newException(Defect, "Unknown kind: " & $t.kind)
  if size > 64:
    raise newException(Defect, "Unable to parse values larger than 64 bits")
  if size == 0:
    raise newException(Defect, "Unable to parse values with size 0")
  if ensureIsByteMult and size mod 8 != 0:
    raise newException(Defect, "l/b is only valid for multiple-of-8 sizes")
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
    endian: endian,
    kind: newIdentNode(kindPrefix & $containSize),
    customReader: customReader,
    customWriter: customWriter
  )


proc getBitInfo(size, offset: BiggestInt):
  tuple[read, skip, shift, mask: BiggestInt] =
  result.read = (size+7) div 8
  result.skip = (size+offset) div 8
  result.shift = result.read*8 - size - offset
  result.mask = (1 shl size) - 1


proc createReadStatement(
  field: NimNode,
  info: tuple[size: BiggestInt; endian: Endianness; kind, customReader, customWriter: NimNode],
  offset: var BiggestInt, stream: NimNode): NimNode {.compileTime.} =
  let
    size = info.size
    custom = info.customReader
  if custom != nil:
    result = (quote do:
      `field` = `custom`
    )
  elif info.kind.kind == nnkIdent and info.kind.strVal == "string":
    if offset != 0:
      raise newException(Defect, "Strings must be on byte boundry")
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
    var
      bitInfo = getBitInfo(size, offset)
      read = bitInfo.read
      skip = bitInfo.skip
      shift = bitInfo.shift
      mask = bitInfo.mask
    if shift < 0:
      shift+=8
      read+=1
    result = newStmtList()
    if read == skip:
      case info.endian
      of littleEndian:
        result.add(quote do:
          `stream`.readDataLE(`field`.addr, `read`)
        )
      of bigEndian:
        result.add(quote do:
          `stream`.readDataBE(`field`.addr, `read`)
        )
    else:
      case info.endian
      of littleEndian:
        result.add(quote do:
          `stream`.peekDataLE(`field`.addr, `read`)
        )
      of bigEndian:
        result.add(quote do:
          `stream`.peekDataBE(`field`.addr, `read`)
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


proc createWriteStatement(
  field: NimNode,
  info: tuple[size: BiggestInt; endian: Endianness; kind, customReader, customWriter: NimNode],
  offset: var BiggestInt, stream: NimNode,
  tmpVar: NimNode = nil): NimNode {.compileTime.} =
  let
    size = info.size
    kind = info.kind
    custom = info.customWriter
  if custom != nil:
    custom.insert(2, field)
    result = (quote do:
      `custom`
    )
  elif info.kind.kind == nnkIdent and info.kind.strVal == "string":
    if offset != 0:
      raise newException(Defect, "Strings must be on byte boundry")
    if size == 0:
      result = (quote do:
        `stream`.write(`field`)
        `stream`.write(0'u8)
      )
    else:
      let fieldName = field.toStrLit
      result = (quote do:
        if `size` != `field`.len:
          raise newException(Defect, "String " & `fieldName` & " of given size not matching")
        `stream`.write(`field`)
      )
  else:
    let
      bitInfo = getBitInfo(size, offset)
      write = bitInfo.read
      skip = bitInfo.skip
      shift = bitInfo.shift
      mask = bitInfo.mask
    result = newStmtList()
    if info.size mod 8 == 0:
      case info.endian
      of littleEndian:
        result.add(quote do:
          `stream`.writeDataLE(`field`.addr, `write`)
        )
      of bigEndian:
        result.add(quote do:
          `stream`.writeDataBE(`field`.addr, `write`)
        )
    else:
      #[
      if tmpVar == nil:
        raise newException(Defect, "tmpVar cannot be nil when size mod" &
          "8 != 0 and info.kind = " & $info.kind)
      ]#
      if tmpVar != nil:
        if skip != 0 and skip != size div 8:
          let addspace = (size div 8) * 8
          result.add(quote do:
            `tmpVar` = `tmpVar` shl `addspace`
          )
        if shift >= 0:
          result.add(quote do:
            `tmpVar` = `tmpVar` or (`field` and `mask`).int64 shl `shift`
          )
        else:
          result.add(quote do:
            `tmpVar` = `tmpVar` or (`field` and `mask`).int64 shr -`shift`
          )
      if skip != 0:
        if tmpVar != nil:
          case info.endian
          of littleEndian:
            result.add(quote do:
              `stream`.writeDataLE(`tmpVar`.addr, `skip`)
              `tmpVar` = 0
            )
          of bigEndian:
            result.add(quote do:
              `stream`.writeDataBE(`tmpVar`.addr, `skip`)
              `tmpVar` = 0
            )
        else:
          case info.endian
          of littleEndian:
            result.add(quote do:
              `stream`.writeDataLE(`field`.addr, `skip`)
            )
          of bigEndian:
            result.add(quote do:
              `stream`.writeDataBE(`field`.addr, `skip`)
            )
          #[
      if offset + size > (offset + size) mod 8:
        result.add(quote do:
          `tmpVar` = (`field` and `mask`) shl (8 + `shift`)
        )
        ]#
    offset += size
    offset = offset mod 8


macro createParser*(name: untyped, paramsAndDef: varargs[untyped]): untyped =
  ## The main macro in this module. It takes the ``name`` of the tuple to
  ## create along with a block on the format described above and creates a
  ## reader and a writer for it. The output is a tuple with ``name`` that has
  ## two fields ``get`` and ``put``. Get is on the form
  ## ``proc (stream: Stream): tuple[<fields>]`` and put is
  ## ``proc (stream: Stream, input: tuple[<fields>])``
  let
    body = paramsAndDef[^1]
    res = newIdentNode("result")
    stream = newIdentNode("stream")
    input = newIdentNode("input")
    tmpVar = genSym(nskVar)
  var
    inner = newStmtList()
    writer = newStmtList()
    tupleMeat = nnkTupleTy.newNimNode
    i = 0
    field = 0
    readOffset: BiggestInt = 0
    writeOffset: BiggestInt = 0
    seenFields = newSeq[string]()
    extraParams = newSeq[NimNode]()
    skipMagicReader = false
  while i < paramsAndDef.len - 1:
    let p = paramsAndDef[i]
    if p.kind != nnkExprColonExpr:
      raise newException(Defect, "Extra arguments must be colon expressions")
    extraParams.add(newIdentDefs(p[0], p[1]))
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
        var
          sym: NimNode
          writeSym: NimNode
        if def[1][0][0].strVal == "_":
          sym = genSym(nskVar)
          writeSym = genSym(nskVar)
          inner.add(quote do:
            var `sym`: `kind`
          )
          writer.add(quote do:
            var `writeSym`: `kind` = `magic`
          )
          dec field
        else:
          sym = (quote do: `res`[`field`])
          writeSym = (quote do: `input`[`field`])
          let
            resfield = newIdentNode($def[1][0][0])
          seenFields.add $def[1][0][0]
          tupleMeat.add(nnkIdentDefs.newTree(resfield, kind, newEmptyNode()))
        if $info.kind == "string":
          info.size = ($magic).len
        if not skipMagicReader:
          inner.add(createReadStatement(sym, info, readOffset, stream))
          inner.add(quote do:
            if `sym` != `magic`:
              raise newException(MagicError,
                "Magic with size " & $`size` &
                " didn't match value " & $`magic` & ", read: " & $`sym`)
          )
        else:
          skipMagicReader = false
        writer.add(createWriteStatement(writeSym, info, writeOffset, stream))
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
          iiRead = genSym(nskVar)
          iiWrite = genSym(nskVar)
          startReadOffset = readOffset
          startWriteOffset = writeOffset
        var
          readFieldOps = @[
            createReadStatement(
              (quote do: `res`[`field`][`iiRead`]), info, readOffset, stream
            )
          ]
        while startReadOffset != readOffset:
          readFieldOps.add createReadStatement(
            (quote do: `res`[`field`][`iiRead`]), info, readOffset, stream
          )
        var
          writeFieldOps = @[
            createWriteStatement(
              (quote do: `input`[`field`][`iiWrite`]), info, writeOffset, stream, tmpVar
            )
          ]
        while startWriteOffset != writeOffset:
          writeFieldOps.add createWriteStatement(
            (quote do: `input`[`field`][`iiWrite`]), info, writeOffset, stream, tmpVar
          )
        let
          readFieldCount = readFieldOps.len
          writeFieldCount = writeFieldOps.len
        var
          readField = nnkCaseStmt.newTree(
            (quote do: `iiRead` mod `readFieldCount`)
          )
          writeField = nnkCaseStmt.newTree(
            (quote do: `iiWrite` mod `writeFieldCount`)
          )
        var iii = 0
        for curField in readFieldOps:
          readField.add(nnkOfBranch.newTree(
            newLit(iii), curField
          ))
          inc iii
        iii = 0
        for curField in writeFieldOps:
          writeField.add(nnkOfBranch.newTree(
            newLit(iii), curField
          ))
          inc iii
        readField.add(
          nnkElse.newTree(
            nnkDiscardStmt.newTree(nnkEmpty.newNimNode())
          )
        )
        writeField.add(
          nnkElse.newTree(
            nnkDiscardStmt.newTree(nnkEmpty.newNimNode())
          )
        )
        let x = genSym(nskForvar)
        var writeNull =
          if kind.kind == nnkIdent and kind.strVal == "string":
            if size == 0:
              (quote do:
                `stream`.write(0'u8))
            else:
              (quote do:
                if `size` != `x`.len:
                  raise newException(Defect,
                    "String for field of static length not right size"))
          else:
            newStmtList()
        writer.add(quote do:
          var `iiWrite` = 0
          while `iiWrite` < (`input`[`field`].len).int:
            `writeField`
            `writeNull`
            inc `iiWrite`
        )
        if def[1][0].len == 2:
          var readFields = def[1][0][1].copyNimTree
          readFields.replace(seenFields, res)
          inner.add(quote do:
            `res`[`field`] = newSeq[`kind`](`readFields`)
            var `iiRead` = 0
            while `iiRead` < (`readFields`).int:
              `readField`
              inc `iiRead`
          )
        else:
          if body.len > i+1:
            let endMagic = body[i+1]
            if endMagic[1][0].kind != nnkAsgn:
              raise newException(Defect,
                "Open arrays must be followed by magic for termination")
            let
              endInfo = decodeType(endMagic[0], stream, seenFields)
              magic = endMagic[1][0][1]
              endKind = endInfo.kind
              peek = newIdentNode("peek" & $endInfo.kind)
              bitInfo = getBitInfo(endInfo.size, readOffset)
              shift = bitInfo.shift
              mask = bitInfo.mask
            inner.add(quote do:
              `res`[`field`] = newSeq[`kind`]()
              var `iiRead` = 0
            )
            if $endInfo.kind != "string":
              inner.add(quote do:
                while ((`stream`.`peek`() shr
                      (((`readFieldCount` - 1) - (`iiRead` mod `readFieldCount`)) *
                      `shift`)) and `mask`).`endKind` != `magic`.`endKind`:
                  `res`[`field`].setLen(`res`[`field`].len+1)
                  `readField`
                  inc `iiRead`
                `stream`.setPosition(`stream`.getPosition()+sizeof(`endKind`))
              )
              skipMagicReader = true
            else:
              inner.add(quote do:
                while `stream`.peekStr(len(`magic`)) != `magic`:
                  `res`[`field`].setLen(`res`[`field`].len+1)
                  `readField`
                  inc `iiRead`
              )
          else:
            inner.add(quote do:
              `res`[`field`] = newSeq[`kind`]()
              var `iiRead` = 0
              while not `stream`.atEnd:
                `res`[`field`].setLen(`res`[`field`].len+1)
                `readField`
                inc `iiRead`
            )
        readOffset = 0
        writeOffset = 0
      of nnkIdent:
        if def[1][0].strVal == "_":
          if def[0].kind == nnkIdent and def[0].strVal == "s":
            writer.add(quote do:
              `stream`.write(0'u8)
            )
            inner.add(quote do:
              while (`stream`.readint8() != 0): discard
            )
            dec field
          else:
            let
              readJump =
                if size mod 8 != 0:
                  if (readOffset+size) mod 8 < readOffset+size: 1 else: 0
                else:
                  0
              writeJump =
                if size mod 8 != 0:
                  if (writeOffset+size) mod 8 < writeOffset+size: 1 else: 0
                else:
                  0
            writer.add(quote do:
              for i in 0..<(`size` div 8 + `writeJump`):
                `stream`.write(0'u8)
            )
            inner.add(quote do:
              `stream`.setPosition(`stream`.getPosition()+`size` div 8 + `readJump`)
            )
            readOffset += size
            readOffset = readOffset mod 8
            writeOffset += size
            writeOffset = writeOffset mod 8
            dec field
        else:
          let
            sym = def[1][0]
            resfield = newIdentNode($sym)
          seenFields.add $sym
          tupleMeat.add(nnkIdentDefs.newTree(resfield, kind, newEmptyNode()))
          inner.add(
            createReadStatement(
              (quote do: `res`[`field`]), info, readOffset, stream
            )
          )
          writer.add(
            createWriteStatement(
              (quote do: `input`[`field`]), info, writeOffset, stream, tmpVar
            )
          )
      else:
        discard
    when defined(binaryparseLog):
      # This can be used for debugging
      inner.add(quote do:
        when `field` >= 0:
          echo "Done reading field " & $`i` & ": " & $`res`[`field`]
        else:
          echo "Done reading field " & $`i`
      )
    inc i
    inc field
  let
    readerName = genSym(nskProc)
    writerName = genSym(nskProc)
  result = quote do:
    proc `readerName`(`stream`: Stream): `tupleMeat` =
      `inner`
    proc `writerName`(`stream`: Stream, `input`: var `tupleMeat`) =
      var `tmpVar`: int64 = 0
      `writer`
    let `name` = (get: `readerName`, put: `writerName`)
  for p in extraParams:
    result[0][3].add p
    result[1][3].add p.copyNimTree

  when defined(binaryparseEcho):
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
    bu16: _ = 258
    lu16: _ = 513

  createParser(tert):
    3: test[8]

  createParser(ccsds_header):
    u3: version
    u1: packet_type
    u1: secondary_header
    u11: apid

  createParser(debug):
    u8: _ = 128
    u16: size

  createParser(twoInts):
    8: first
    8: second

  createParser(test):
    *twoInts: fields[]

  createParser(terminatedInts):
    u32: myInts[]

  block parse:
    var fs = newFileStream("data.hex", fmRead)
    defer: fs.close()
    if not fs.isNil:
      var data = myParser.get(fs)
      echo data.size
      echo data.data
      echo data.str
      echo data.inner.data
      var fs3 = newFileStream("data_out.hex", fmWrite)
      defer: fs3.close()
      if not fs3.isNil:
        myParser.put(fs3, data)
    var fs2 = newFileStream("out.hex", fmWrite)
    defer: fs2.close()
    if not fs2.isNil:
      var data: typeGetter(tert)
      data.test = @[1'i8, 2, 3, 4, 5, 6, 7, 0]
      tert.put(fs2, data)
    var ss = newStringStream("Hello world!")
    var readData = test.get(ss)
    echo readData
    var ss2 = newStringStream()
    test.put(ss2, readData)
    ss2.setPosition(0)
    echo ss2.readAll()

    var
      ss3 = newStringStream()
      outData: typeGetter(terminatedInts)
    outData.myInts = @[1'u32,2,3,4,5,6]
    terminatedInts.put(ss3, outData)
    ss3.setPosition(0)
    echo terminatedInts.get(ss3)

