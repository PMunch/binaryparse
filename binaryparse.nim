## This module implements a DSL for creating binary parsers/encoders.
## It exports the macro ``createParser`` which generates a ``tuple[get: proc, put: proc]``.
## ``get`` returns a tuple with each parsed field and ``put`` writes a compatible tuple to the stream.
##
## The macro accepts 3 kind of things:
##
## -  Parser options
## -  Parser parameters
## -  Block with DSL statements
##
## Parser options
## --------------
##
## Each specified option must be in the form ``option = value``:
##
## - ``endian``: sets the default byte endianness for the whole parser
##    - *default*: big endian
##    - ``b``: **big** endian
##    - ``l``: **little** endian
## - ``bitEndian``: sets the default bit endianness for the whole parser
##    - *default*: left -> right
##    - ``n``: left -> right (**normal**)
##    - ``r``: left <- right (**reverse**)
##
## Parser parameters
## -----------------
##
## Each parameter must be in the form ``symbol: type``. The generated
## ``get``/``put`` procs will then have this additional parameter appended.
##
## DSL
## ----
##
## Each statement corresponds to 1 field. The general syntax is:
##
## .. code:: nim
##
##     Type {Operations}: Value
##
## where ``{Operations}`` is optional and refers to a plugin system (see
## below).
##
## Type
## ~~~~
##
## The **kind**, **endianness** and **size** are encoded in a identifier
## made up of:
##
## - 1 optional letter specifying the kind:
##    - *default*: signed integer
##    - ``u``: **unsigned** integer
##    - ``f``: **float**
##    - ``s``: **string**
##    - ``*``: complex (see below)
## - 1 optional letter specifying byte endianness:
##    - *default*: big endian
##    - ``b``: **big** endian
##    - ``l``: **little** endian
## - 1 optional letter specifying bit endianness for unaligned reads:
##    - *default*: left -> right
##    - ``n``: left -> right (**normal**)
##    - ``r``: left <- right (**reverse**)
## - 1 number specifying size in **bits**:
##    - for a string it refers to the size of each individual and defaults to ``8``
##    - for an integer the allowed values are ``1 .. 64``
##    - for a float the allowed values are ``32`` and ``64``
##    - for a custom it can't be used (use the secondary ``size`` operation)
##
## You can order options however you want, but size must come last (e.g. ``lru16`` and ``url16`` are valid but not ``16lru``).
##
## Value
## ~~~~~
##
## This section includes the following features (only name is mandatory):
##
## - name
## - repetition
## - assertion
##
## If you don't *really* want a name, you can discard the symbol by using ``_`` in its place:
##
## .. code:: nim
##
##     createParser(myParser):
##       8: _
##
## Alignment
## ~~~~~~~~~
##
## If any of the following is violated, binaryparse should generate an exception:
##
## - Byte endianness can only be used with byte-multiple integers
## - Bit endianness must be uniform between **byte boundaries**
## - Spec must finish on a byte boundary
##
## .. code:: nim
##
##    createParser(myParser, bitEndian = n):
##      b9: a # error: cannot apply byte endianness
##      r6: b # error: shares bits with previous byte
##      10: c # error: spec does not finish on a byte boundary
##
## Moreover, unaligned reads for strings are not supported:
##
## .. code:: nim
##
##     createParser(myParser):
##       6: x
##       s: y # invalid, generates an exception
##
## Assertion
## ~~~~~~~~~
##
## Use ``= expr`` for producing an exception if the parsed value doesn't
## match ``expr``:
##
## .. code:: nim
##
##     s: x = "binaryparse is awesome"
##     8: y[5] = @[0, 1, 2, 3, 4]
##
## Assertion can also be used in a special manner to terminate the previous
## field if it's a **string** or a **sequence indicated as magic-terminated**.
## This is discussed in later sections.
##
## Complex types
## ~~~~~~~~~~~~~
##
## Instead of the described identifier for specifying ``Type``, you can
## call a previously defined parser by using ``*`` followed by the name of
## the parser. If your parser is parametric you must pass arguments to it
## with standard call syntax.
##
## Example:
##
## .. code:: nim
##
##     createParser(inner):
##       32: a
##       32: b
##
##     createParser(innerWithArgs, size: int32):
##       32: a
##       32: b[size]
##
##     createParser(outer):
##       *inner: x
##       *innerWithArgs(x.a): y
##
## Repetition
## ~~~~~~~~~~
##
## There are 3 ways to produce a ``seq`` of your ``Type``:
##
## - ``for``: append ``[expr]`` to the name for repeating ``expr``
##   times
## - ``until``: append ``{expr}`` to the name for repeating until
##   ``expr`` is evaluated to ``true``
## - ``magic``: enclose name with ``{}`` and use assertion with
##   your **next** field
##
## In until repetition you can use 3 special symbols:
##
## - ``e``: means 'last element read'
## - ``i``: means 'current loop index'
## - ``s``: means 'stream'
##
## .. code:: nim
##
##     8: a[5] # reads 5 8-bit integers
##     8: b{e == 103 or i > 9} # reads until it finds the value 103 or completes 10th iteration
##     8: {c} # reads 8-bit integers until next field is matches
##     3: _ = 0b111 # magic value can be of any type
##     u8: {d[5]} # reads byte sequences each of length 5 until next field matches
##     s: _ = "END"
##
## Substreams
## ~~~~~~~~~~
##
## Call syntax forces the creation of a substream:
##
## .. code:: nim
##
##     createParser(aux, size: int):
##       8: x[size]
##     createParser(myParser):
##       8: x = 4
##       8: limit = 8
##       *aux(x): fixed(limit)
##
## In the above example, ``limit`` bytes (8 in this case) will be read from the main ``BitStream``.
## Then, a substream will be created out of them, which will then be used as the stream for parsing ``fixed``.
## Since ``fixed`` will only use 4 of them, the remaining 4 will effectively be discarded.
##
## Note that unlike in ``Type``, here size is counted bytes. It is implied that you cannot create
## a substream if your bitstream is unaligned.
##
## This feature is **not implemented for repetition** because it would increase complexity with little benefits.
## The following syntax is **invalid** and you should use the technique with the auxiliary complex type shown above:
##
## .. code:: nim
##
##     createParser(myParser):
##       u8: a[4](6) # does substream refer to each individual element or the whole sequence?
##
## Strings
## ~~~~~~~
##
## Strings are special because they don't have a fixed size. Therefore, you
## must provide enough information regarding their termination. This can be
## achieved with one of the following:
##
## - Use of substream
## - Assertion
## - Magic
##
## .. code:: nim
##
##     s: a # a null-terminated string
##     s: _ # error: encoder unknows how many bytes to encode
##     s: b(5) # string of length 5 (either null byte or end of substream terminates it)
##     s: c = "ABC" # reads a string of length 3 that must match "ABC"
##     s: d # reads a string until next field is matched
##     s: _ = "MAGIC"
##     s: e[5] # reads 5 null-terminated strings
##     s: {f} # reads null-terminated strings until next field matches
##     3: term = 0b111 # terminator of the above sequence
##     s: {g[5]} # sequence of 5-length sequences of null-terminated strings
##     s: _ = "END_NESTED"
##
## Clarifications:
##
## - Strings are always null-terminated except when using assertion or magic
##   on it directly (then, null byte must be included in the value)
## - When using both a substream and an assertion in the next field, the
##   substream takes precedence and the next field is not magic
##
## Custom parser API
## ~~~~~~~~~~~~~~~~~
##
## Since a binaryparse parser is just a ``tuple[get: proc, set: proc]``,
## you can write parsers by hand that are compatible with the DSL. Just be
## sure that ``get`` and ``set`` have a proper signature:
##
## .. code:: nim
##
##     type parserTy = tuple[...]
##     proc get(s: BitStream): parserTy
##     proc put(s: BitStream, input: parserTy)
##     let parser = (get: get, put: put)
##
## If you want your custom parser to be parametric, simply append more
## parameters to your procs. These extra parameters must be identical and
## in the same order in the two procs.
##
## Example:
##
## .. code:: nim
##
##     type parserTy = tuple[...]
##     proc get(s: BitStream, x: int, y: float): parserTy
##     proc put(s: BitStream, input: parserTy, x: int, y: float)
##     let parser = (get: get, put: put)
##
## Operations (plugins)
## ~~~~~~~~~~~~~~~~~~~~
##
## Plugins are **user-defined** keys which define an operation on a field.
## They are parametric, which means they also have a value. The API for
## writing plugins is not designed yet, but the syntax for using them is:
##
## .. code:: nim
##
##     Type {plugin: expr}: Value
##
## Examples of plugins
## ~~~~~~~~~~~~~~~~~~~
##
## - ``pos``: positions the ``stream`` at byte ``value`` before parsing and then
##   resets it to the previous position
## - ``cond``: wraps the field into an ``Option`` type and will only parse it if
##   ``value`` is evaluated to ``true``
## - ``size``: reads ``value`` bytes from the stream and creates a *substream*
##
## You can combine multiple operations which will be applied to the field
## in the specified order:
##
## .. code:: nim
##
##     8: shouldParse
##     16 {cond: shouldParse.bool, size: 4}: x
##
## First ``shouldParse.bool`` will be evaluted. If it's ``false``, parsing
## won't happen; if it's true, then 4 bytes will be read from the stream
## and a substream with them will be created. Then, 16 bits will be read
## from this substeam. Finally, these bits will be wrapped into an
## ``Option`` and the resulting field will be an Option[int16].
##
## When you produce a sequence, ``Operations`` apply to **the whole**
## sequence (not each individual element).
##
## Special notes
## ~~~~~~~~~~~~~
##
## - Nim expressions may contain:
##    - a previously defined field
##    - a parser parameter
##    - the ``e`` symbol if it's a repetition until expression
##    - the ``i`` symbol if it's a repetition until expression
##    - the ``s`` symbol if it's a repetition until or assertion expression
##
## These last 3 symbols might conflict with your variables or fields, so you
## shouldn't use them for something else.
##

import macros, tables, strutils, sugar, strformat
import bitstreams

type
  MagicError* = object of Defect
    ## Error raised from the parser procedure when a magic sequence is not
    ## matching the specified value.
  Options = tuple
    endian: Endianness
    bitEndian: Endianness
  OptionSet = enum
    osEndian
    osBitEndian
  Kind = enum
    kInt, kUInt, kFloat, kStr, kCustom
  Type = ref object
    case kind: Kind
    of kCustom:
      symbol: NimNode
      args: seq[NimNode]
    else:
      size: BiggestInt
    endian: Endianness
    bitEndian: Endianness
  Operations = OrderedTable[string, NimNode]
  Repeat = enum
    rNo
    rFor
    rUntil
  Value = ref object
    name: string
    case repeat: Repeat
    of rFor, rUntil: repeatExpr: NimNode
    of rNo: discard
    valueExpr: NimNode
    sizeExpr: NimNode
    isMagic: bool
  Field = tuple
    typ: Type
    opts: Operations
    val: Value

const defaultOptions: Options = (
  endian: bigEndian,
  bitEndian: bigEndian)

macro typeGetter*(body: typed): untyped =
  ## Helper macro to get the return type of custom parsers
  body.getTypeImpl[0][1][0][0]

proc syntaxError() = raise newException(Defect, "Invalid syntax")

proc getImpl(typ: Type): NimNode = # TODO
  case typ.kind
  of kInt, kUInt:
    var s = ""
    if typ.kind == kUInt:
      s &= "u"
    s &= "int"
    if typ.size == 0: discard
    elif typ.size > 32: s &= "64"
    elif typ.size > 16: s &= "32"
    elif typ.size > 8: s &= "16"
    else: s &= "8"
    result = ident(s)
  of kFloat:
    result = ident("float" & $typ.size)
  of kStr:
    result = ident"string"
  of kCustom:
    var sym = typ.symbol
    result = quote do: typeGetter(`sym`)

proc prefixFields(node: var NimNode, st, params: seq[string], with: NimNode) =
  if node.kind == nnkIdent:
    if node.strVal in st:
      node = newDotExpr(with, node)
  elif node.kind == nnkDotExpr and node[0].strVal in params:
    return
  else:
    var i = 0
    while i < len(node):
      var n = node[i]
      prefixFields(n, st, params, with)
      node[i] = n.copyNimTree
      inc i

proc getCustomReader(typ: Type, bs: NimNode, st, params: seq[string]): NimNode =
  result = newCall(nnkDotExpr.newTree(typ.symbol, ident"get"), bs)
  for arg in typ.args:
    result.add(arg)
  result.prefixFields(st, params, ident"result")

proc getCustomWriter(typ: Type, bs: NimNode, st, params: seq[string]): NimNode =
  result = newCall(nnkDotExpr.newTree(typ.symbol, ident"put"), bs)
  for arg in typ.args:
    result.add(arg)
  result.prefixFields(st, params, ident"input")

proc replaceWith(node: var NimNode; what, with: NimNode) =
  if node.kind == nnkIdent:
    if eqIdent(node, what):
      node = with
  else:
    var i = 0
    while i < len(node):
      var n = node[i]
      n.replaceWith(what, with)
      node[i] = n
      inc i

proc decodeType(t: NimNode; seenFields, params: seq[string]; opts: Options): Type =
  result = Type()
  var
    endian = opts.endian
    bitEndian = opts.bitEndian
  case t.kind
  of nnkIntLit:
    result.kind = kInt
    result.size = t.intVal
    if result.size > 64:
      raise newException(Defect, "Unable to parse values larger than 64 bits")
  of nnkIdent:
    var
      kind = kInt
      letters: set[char]
      size: int
    for i, c in t.strVal:
      case c
      of 'u', 'f', 's':
        if letters * {'u', 'f', 's'} != {}:
          raise newException(Defect, "Type was specified more than once")
        if c == 'u':
          kind = kUInt
        elif c == 'f':
          kind = kFloat
        elif c == 's':
          kind = kStr
      of 'l', 'b':
        if letters * {'l', 'b'} != {}:
          raise newException(Defect, "Endianness was specified more than once")
        if c == 'b':
          endian = bigEndian
        else:
          endian = littleEndian
      of 'n', 'r':
        if letters * {'n', 'r'} != {}:
          raise newException(Defect,
            "Bit endianness was specified more than once")
        if c == 'n':
          bitEndian = bigEndian
        else:
          bitEndian = littleEndian
      else:
        try: size = t.strVal[i..^1].parseInt
        except ValueError:
          raise newException(Defect, &"Format {t.strVal} not supported")
        break
      letters.incl c
    result.kind = kind
    result.size = size
    if letters * {'l', 'b'} != {} and 's' in letters:
      raise newException(Defect, "Endianness for strings is not supported")
    if size > 64:
      raise newException(Defect, "Unable to parse values larger than 64 bits")
    if kind in {kInt, kUInt, kFloat} and size == 0:
      raise newException(Defect, "Unable to parse values with size 0")
    if kind == kFloat and size != 32 and size != 64:
      raise newException(Defect, "Only 32 and 64 bit floats are supported")
    if kind == kStr and size mod 8 != 0:
      raise newException(Defect, "Unaligned strings are not supported")
    if letters * {'l', 'b'} != {} and (size == 8 or size mod 8 != 0):
      raise newException(Defect, "l/b is only valid for multiple-of-8 sizes")
  of nnkCall:
    result.kind = kCustom
    result.symbol = t[0]
    var i = 1
    while i < t.len:
      result.args.add(t[i].copyNimTree)
      inc i
  else:
    syntaxError()
  result.endian = endian
  result.bitEndian = bitEndian

proc decodeOperations(node: NimNode): Operations =
  result = initOrderedTable[string, NimNode]()
  for child in node:
    result[child[0].strVal] = child[1]

proc decodeValue(node: NimNode, st: var seq[string], params: seq[string]): Value =
  var node = node
  result = Value()
  if node.kind == nnkAsgn:
    result.valueExpr = node[1]
    node = node[0]
  if node.kind == nnkCurly:
    if result.valueExpr != nil:
      raise newException(Defect,
        "Magic and assertion can't be used together in the same field")
    result.isMagic = true
    node = node[0]
  if node.kind == nnkBracketExpr:
    result.repeat = rFor
    result.repeatExpr = node[1]
    node = node[0]
  elif node.kind == nnkCurlyExpr:
    result.repeat = rUntil
    result.repeatExpr = node[1]
    node = node[0]
  elif node.kind == nnkCall:
    result.sizeExpr = node[1]
    node = node[0]
  if node.kind != nnkIdent:
    syntaxError()
  if node.strVal != "_":
    result.name = node.strVal
    st.add(result.name)

proc decodeHeader(input: seq[NimNode]): tuple[params: seq[NimNode], opts: Options] =
  result.opts = defaultOptions
  var specifiedOpts: set[OptionSet]
  for n in input:
    case n.kind
    of nnkExprColonExpr:
      result.params.add(newIdentDefs(n[0], n[1]))
    of nnkExprEqExpr:
      case n[0].strVal
      of "endian":
        if osEndian in specifiedOpts:
          raise newException(Defect,
            "Option 'endian' was specified more than once")
        case n[1].strVal
        of "b": result.opts.endian = bigEndian
        of "l": result.opts.endian = littleEndian
        else:
          raise newException(Defect,
            "Invalid value for endian option (valid values: l, b)")
        specifiedOpts.incl osEndian
      of "bitEndian":
        if osBitEndian in specifiedOpts:
          raise newException(Defect,
            "Option 'bitEndian' was specified more than once")
        case n[1].strVal
        of "n": result.opts.bitEndian = bigEndian
        of "r": result.opts.bitEndian = littleEndian
        else:
          raise newException(Defect,
            "Invalid value for 'bitEndian' option (valid values: n, r)")
        specifiedOpts.incl osBitEndian
      else:
        raise newException(Defect, &"Unknown option: {$n[0]}")
    else:
      syntaxError()

proc decodeField(def: NimNode, st: var seq[string], params: seq[string],
                 opts: Options): Field =
  var a, b, c: NimNode
  case def.kind
  of nnkPrefix:
    c = def[2][0].copyNimTree
    case def[1].kind
    of nnkIdent:
      a = newCall(def[1].copyNimTree)
    of nnkCall:
      a = def[1].copyNimTree
    of nnkCommand:
      a = def[1][0].copyNimTree
      b = def[1][1].copyNimTree
    else: syntaxError()
  of nnkCall:
    a = def[0].copyNimTree
    c = def[1][0].copyNimTree
  of nnkCommand:
    a = def[0].copyNimTree
    b = def[1].copyNimTree
    c = def[2][0].copyNimTree
  else: syntaxError()
  result = (
    decodeType(a, st, params, opts),
    decodeOperations(b),
    decodeValue(c, st, params))

proc createReadStatement(sym, bs: NimNode, f: Field, st, params: seq[string]): NimNode {.compileTime.} =
  result = newStmtList()
  let
    kind = f.typ.kind
    impl = f.typ.getImpl
    endian = if f.typ.endian == littleEndian: 'l' else: 'b'
    endianNode = newLit(f.typ.endian)
    bitEndian = if f.typ.bitEndian == littleEndian: 'l' else: 'b'
    procUnaligned = ident("readbits" & bitEndian & "e")
  case kind
  of kInt, kUInt:
    let
      size = f.typ.size
      sizeNode = newLit(size.int)
      procUnalignedCall = quote do: `procUnaligned`(`bs`, `sizeNode`, `endianNode`)
    if size in {8, 16, 32, 64}:
      let numKind = if kind == kInt: 's' else: 'u'
      var procAlignedStr = "read" & numKind & $size
      if size != 8: procAlignedStr &= endian & "e"
      let procAligned = ident(procAlignedStr)
      result.add(quote do:
        if isAligned(`bs`):
          `sym` = `procAligned`(`bs`)
        else:
          `sym` = `impl`(`procUnalignedCall`))
    else:
      result.add(quote do:
        if isAligned(`bs`):
          resetBuffer(`bs`)
        `sym` = `impl`(`procUnalignedCall`))
  of kFloat:
    let
      size = f.typ.size
      sizeNode = newLit(size.int)
      procAligned = ident("readf" & $size & endian & "e")
      procUnalignedCall = quote do: `procUnaligned`(`bs`, `sizeNode`, `endianNode`)
      floatCast =
        if size == 64: quote do: cast[float64](`procUnalignedCall`)
        else: quote do: float32(cast[float64](`procUnalignedCall`))
    result.add(quote do:
      if isAligned(`bs`):
        `sym` = `procAligned`(`bs`)
      else:
        `sym` = `floatCast`)
  of kStr:
    let expr = f.val.valueExpr
    result.add(quote do:
      if not isAligned(`bs`):
        raise newException(IOError, "Stream must be aligned to read a string"))
    result.add(
      if expr != nil:
        quote do:
          `sym` = readStr(`bs`, 8 * `expr`.len)
      else:
        quote do:
          `sym` = readStr(`bs`))
  of kCustom:
    let call = getCustomReader(f.typ, bs, st, params)
    result.add(quote do: `sym` = `call`)

proc createWriteStatement(f: Field, sym, bs: NimNode, st, params: seq[string]): NimNode {.compileTime.} =
  result = newStmtList()
  let
    kind = f.typ.kind
    impl = f.typ.getImpl
    endian = if f.typ.endian == littleEndian: 'l' else: 'b'
    endianNode = newLit(f.typ.endian)
    bitEndian = if f.typ.bitEndian == littleEndian: 'l' else: 'b'
  case kind
  of kInt, kUInt, kFloat:
    let
      size = f.typ.size
      sizeNode = newLit(size.int)
      procUnaligned = ident("writebits" & bitEndian & "e")
    if sym == nil:
      result.add(quote do:
        `procUnaligned`(`bs`, `sizeNode`, 0))
    else:
      let tmp = genSym(nskVar)
      result.add(quote do:
        var `tmp` = `sym`)
      if size in {8, 16, 32, 64}:
        let procAligned = ident("write" & endian & "e")
        result.add(quote do:
          if isAligned(`bs`):
            `procAligned`(`bs`, `impl`(`tmp`))
          else:
            `procUnaligned`(`bs`, `sizeNode`, `tmp`, `endianNode`))
      else:
        result.add(quote do: `procUnaligned`(`bs`, `sizeNode`, `tmp`, `endianNode`))
  of kStr:
    result.add(quote do:
      if not isAligned(`bs`):
        raise newException(IOError, "Stream must be aligned to write a string"))
    if sym != nil:
      let tmp = genSym(nskVar)
      result.add(quote do:
        var `tmp` = `sym`
        writeStr(`bs`, `tmp`))
    if f.val.valueExpr == nil and not f.val.isMagic:
      result.add(quote do:
        writeBe(`bs`, 0'u8))
  of kCustom:
    let call = getCustomWriter(f.typ, bs, st, params)
    call.insert(2, sym)
    result.add(quote do: `call`)

proc createReadField(f: Field, bs: NimNode, st: var seq[string], params: seq[string]): NimNode =
  result = newStmtList()
  let
    field = f.val.name
    fieldIdent = ident(field)
    res = ident"result"
    tmp = genSym(nskVar)
    impl = f.typ.getImpl
  var value = f.val.valueExpr
  value.prefixFields(st, params, res)
  if f.val.repeat == rNo:
    result.add(quote do:
      var `tmp`: `impl`)
    result.add createReadStatement(tmp, bs, f, st, params)
  else:
    result.add(quote do:
      var `tmp`: seq[`impl`])
    var expr = f.val.repeatExpr.copyNimTree
    expr.prefixFields(st, params, res)
    case f.val.repeat
    of rFor:
      let
        loopIdx = genSym(nskForVar)
        sym = quote do: `tmp`[`loopIdx`]
        readStmt = createReadStatement(sym, bs, f, st, params)
      result.add(quote do:
        `tmp` = newSeq[`impl`](`expr`)
        for `loopIdx` in 0 ..< int(`expr`):
          `readStmt`)
    of rUntil:
      let
        sym = genSym(nskVar)
        loopIdx = genSym(nskVar)
      expr.replaceWith(ident"e", sym)
      expr.replaceWith(ident"i", loopIdx)
      expr.replaceWith(ident"s", bs)
      let readStmt = createReadStatement(sym, bs, f, st, params)
      result.add (quote do:
        `tmp` = newSeq[`impl`]()
        var
          `loopIdx`: int
          `sym`: `impl`
        while true:
          `readStmt`
          `tmp`.add(`sym`)
          inc `loopIdx`
          if `expr`: break)
    else: discard
  if value != nil:
    result.add(quote do:
      if `tmp` != (`value`):
        raise newException(MagicError, "field '" & $`field` & "' was " &
                          $`tmp` & " instead of " & $`value`))
  if field != "":
    result.add(quote do: `res`.`fieldIdent` = `tmp`)

proc createWriteField(f: Field, bs: NimNode, st: var seq[string], params: seq[string]): NimNode =
  result = newStmtList()
  let
    field = f.val.name
    fieldIdent = ident(field)
    input = ident"input"
    tmp = genSym(nskVar)
    impl = f.typ.getImpl
  var value = f.val.valueExpr
  value.prefixFields(st, params, input)
  if f.val.repeat == rNo:
    let sym = if field == "": (if value == nil: nil else: value)
              else: quote do: `input`.`fieldIdent`
    result.add createWriteStatement(f, sym, bs, st, params)
  else:
    var expr = f.val.repeatExpr.copyNimTree
    expr.prefixFields(st, params, input)
    case f.val.repeat
    of rFor:
      result.add(
        if field == "":
          if value == nil:
            quote do:
              var `tmp` = newSeq[`impl`](`expr`)
          else:
            quote do:
              var `tmp` = `value`
        else:
          quote do:
            var `tmp` = `input`.`fieldIdent`)
      let
        loopElem = genSym(nskForVar)
        writeStmt = createWriteStatement(f, loopElem, bs, st, params)
      result.add(quote do:
        for `loopElem` in `tmp`:
          `writeStmt`)
    of rUntil:
      let
        sym = genSym(nskForVar)
        loopIdx = genSym(nskForVar)
      expr.replaceWith(ident"e", sym)
      expr.replaceWith(ident"i", loopIdx)
      expr.replaceWith(ident"s", bs)
      result.add(
        if field == "":
          if value == nil:
            quote do:
              var `tmp` = newSeq[`impl`](`expr`)
          else:
            quote do:
              var `tmp` = `value`
        else:
          quote do:
            var `tmp` = `input`.`fieldIdent`)
      let writeStmt = createWriteStatement(f, sym, bs, st, params)
      result.add(quote do:
        for `loopIdx`, `sym` in `tmp`: `writeStmt`)
    else: discard

macro createParser*(name: untyped, rest: varargs[untyped]): untyped =
  ## The main macro in this module. It takes the ``name`` of the tuple to
  ## create along with a block on the format described above and creates a
  ## reader and a writer for it. The output is a tuple with ``name`` that has
  ## two fields ``get`` and ``put``. Get is on the form
  ## ``proc (bs: BitStream): tuple[<fields>]`` and put is
  ## ``proc (bs: BitStream, input: tuple[<fields>])``
  var
    tupleMeat = newTree(nnkTupleTy)
    fieldsSymbolTable = newSeq[string]()
    reader = newStmtList()
    writer = newStmtList()
    bs = ident"s"
  let
    (params, parserOptions) = decodeHeader(rest[0 .. ^2])
    paramsSymbolTable = collect(newSeq):
      for p in params:
        p[0].strVal
    fields = collect(newSeq):
      for def in rest[^1]:
        decodeField(def, fieldsSymbolTable, paramsSymbolTable, parserOptions)
  for f in fields:
    let field = f.val.name
    if field != "":
      var fieldTypeImpl = f.typ.getImpl
      if f.val.repeat != rNo:
        fieldTypeImpl = quote do: seq[`fieldTypeImpl`]
      tupleMeat.add(newIdentDefs(ident(field), fieldTypeImpl))
    if f.val.sizeExpr != nil:
      var
        rSize = f.val.sizeExpr
        wSize = f.val.sizeExpr
      block generateRead:
        rSize.prefixFields(fieldsSymbolTable, paramsSymbolTable, ident"result")
        let
          ss = genSym(nskVar)
          rf = createReadField(f, ss, fieldsSymbolTable, paramsSymbolTable)
        reader.add(quote do:
          var `ss` = createSubstream(`bs`, `rSize`)
          `rf`)
      block generateWrite:
        wSize.prefixFields(fieldsSymbolTable, paramsSymbolTable, ident"input")
        let
          ss = genSym(nskVar)
          wf = createWriteField(f, ss, fieldsSymbolTable, paramsSymbolTable)
        writer.add(quote do:
          var `ss` = newPaddedBitStream(`wSize`)
          `wf`
          `ss`.seek(0)
          `bs`.writeFromSubstream(`ss`, `wSize`))
    else:
      block generateRead:
        reader.add createReadField(f, bs, fieldsSymbolTable, paramsSymbolTable)
      block generateWrite:
        writer.add createWriteField(f, bs, fieldsSymbolTable, paramsSymbolTable)
  let
    input = ident"input"
    readerName = genSym(nskProc)
    writerName = genSym(nskProc)
  if tupleMeat.len == 0:
    let dummy = genSym(nskField)
    tupleMeat.add(newIdentDefs(dummy, ident"int"))
  result = quote do:
    proc `readerName`(`bs`: BitStream): `tupleMeat` =
      `reader`
    proc `writerName`(`bs`: BitStream, `input`: `tupleMeat`) =
      `writer`
    let `name` = (get: `readerName`, put: `writerName`)
  for p in params:
    result[0][3].add p.copyNimTree
    result[1][3].add p.copyNimTree

  when defined(binaryparseEcho):
    echo repr result