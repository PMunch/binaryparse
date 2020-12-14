binaryparse
===========
This module implements a DSL for creating binary parsers/encoders.
It exports the macro ``createParser`` which generates a ``tuple[get: proc, put: proc]``.
``get`` returns a tuple with each parsed field and ``put`` writes a compatible tuple to the stream.

The macro accepts 3 kind of things:

-  Parser options
-  Parser parameters
-  Block with DSL statements

Parser options
--------------

Each specified option must be in the form ``option = value``:

- ``endian``: sets the default byte endianness for the whole parser
   - *default*: big endian
   - ``b``: **big** endian
   - ``l``: **little** endian
- ``bitEndian``: sets the default bit endianness for the whole parser
   - *default*: left -> right
   - ``n``: left -> right (**normal**)
   - ``r``: left <- right (**reverse**)

Parser parameters
-----------------

Each parameter must be in the form ``symbol: type``. The generated
``get``/``put`` procs will then have this additional parameter appended.

DSL
----

Each statement corresponds to 1 field. The general syntax is:

.. code:: nim

    Type {Operations}: Value

where ``{Operations}`` is optional and refers to a plugin system (see
below).

Type
~~~~

The **kind**, **endianness** and **size** are encoded in a identifier
made up of:

- 1 optional letter specifying the kind:
   - *default*: signed integer
   - ``u``: **unsigned** integer
   - ``f``: **float**
   - ``s``: **string**
   - ``*``: complex (see below)
- 1 optional letter specifying byte endianness:
   - *default*: big endian
   - ``b``: **big** endian
   - ``l``: **little** endian
- 1 optional letter specifying bit endianness for unaligned reads:
   - *default*: left -> right
   - ``n``: left -> right (**normal**)
   - ``r``: left <- right (**reverse**)
- 1 number specifying size in **bits**:
   - for a string it refers to the size of each individual and defaults to ``8``
   - for an integer the allowed values are ``1 .. 64``
   - for a float the allowed values are ``32`` and ``64``
   - for a custom it can't be used (use the secondary ``size`` operation)

You can order options however you want, but size must come last (e.g. ``lru16`` and ``url16`` are valid but not ``16lru``).

Value
~~~~~

This section consists of a the following attributes in order:

- **name** for the field (mandatory)
- substream **size** (optional)
- **repetition** (optional)
- **assertion** optional)

For primitive types (except null-terminated strings) you can instruct
binaryparse to not produce a symbol and discard the field by using ``_``
for the name:

.. code:: nim

    createParser(magic):
      8: _

Strings
~~~~~~~

You can use call syntax to specify length or an assertion to specify an exact expected value (see below),
otherwise strings they are null-terminated:

.. code:: nim

    s: s(5) # a string of length 5
    s: term # a null-terminated string
    s: _ = "MAGIC" # a string that must match the value "MAGIC"
    s: m() # a string that is terminated when a value matches the next field
    16: x = 0xABCD
    s: arr1[5] # a seq of 5 null-terminated strings
    s: arr2(4)[5] # a seq of 5 strings each one of which has length 4
    s: arr3[] # a seq of null-terminated strings until next field is matched
    16: _ = x + 3
    s: arr4(x)[] # a seq of strings of length x until next field is matched
    f32: _ = 2.5

Substreams
~~~~~~~~~~

The call syntax described in the previous section in not limited to strings.
In fact, it more generally forces the creation of a **substream**:

.. code:: nim

    createParser(inner):
      8: x
      16: y
    createParser(myParser):
      8: size = 4
      *inner: fixed(8*size)

In the above example, ``size`` bits (32 in this case) will be read from the main ``BitStream``.
Then, a substream will be created out of them, which will then be used as the stream for parsing ``inner``.
Since ``inner`` will only use 24 of them, the remaining 8 bits will effectively be discarded.

Alignment
~~~~~~~~~

Currently unaligned reads for strings are not supported:

.. code:: nim

    createParser(myParser):
      6: x
      s: y # invalid, generates an exception

If any of the following is violated, binaryparse should generate an exception:

- Byte endianness can only be used with byte-multiple integers.
- Bit endianness must be uniform between **byte boundaries**.
- Spec must finish on a byte boundary.

.. code:: nim

   createParser(myParser, bitEndian = n):
     b9: a # error: cannot apply byte endianness
     r6: b # error: shares bits with previous byte
     10: c # error: spec does not finish on a byte boundary

Assertion
~~~~~~~~~

Use ``= expr`` for producing an exception if the parsed value doesn't
match ``expr``.

Example:

.. code:: nim

    s: x = "binaryparse is awesome"
    8: y[5] = @[0, 1, 2, 3, 4]

Repetition
~~~~~~~~~~

There are 3 ways to produce a ``seq`` of your ``Type``:

- ``for``: append ``[expr]`` to the name for repeating ``expr``
  times
- ``until``: append ``{expr}`` to the name for repeating until
  ``expr`` is evaluated to ``true``
- ``magic``: append ``[]`` to the name and use assertion with
  your **next** field

In until repetition you can use 3 special symbols:

- ``e``: means 'last element read'
- ``i``: means 'current loop index'
- ``s``: means 'stream'

.. code:: nim

    16: a[5] # seq[int16] of size 5
    u8: b{e == 103 or i > 9} # reads until it finds the value 103 or completes 10th iteration
    s: str[] # reads chars into a string until next field is matched
    3: _ = 0b111 # magic value is discarded
    16: c{s.atEnd} # seq[int16] until end of stream

Complex types
~~~~~~~~~~~~~

Instead of the described identifier for specifying ``Type``, you can
call a previously defined parser by using ``*`` followed by the name of
the parser. If your parser is parametric you must pass arguments to it
with standard call syntax.

Example:

.. code:: nim

    createParser(inner):
      32: a
      32: b

    createParser(innerWithArgs, size: int32):
      32: a
      32: b[size]

    createParser(outer):
      *inner: x
      *innerWithArgs(x.a): y

Custom parser API
~~~~~~~~~~~~~~~~~

Since a binaryparse parser is just a ``tuple[get: proc, set: proc]``,
you can write parsers by hand that are compatible with the DSL. Just be
sure that ``get`` and ``set`` have a proper signature:

.. code:: nim

    type parserTy = tuple[...]
    proc get(s: BitStream): parserTy
    proc put(s: BitStream, input: parserTy)
    let parser = (get: get, put: put)

If you want your custom parser to be parametric, simply append more
parameters to your procs. These extra parameters must be identical and
in the same order in the two procs.

Example:

.. code:: nim

    type parserTy = tuple[...]
    proc get(s: BitStream, x: int, y: float): parserTy
    proc put(s: BitStream, input: parserTy, x: int, y: float)
    let parser = (get: get, put: put)

Operations (plugins)
~~~~~~~~~~~~~~~~~~~~

Plugins are **user-defined** keys which define an operation on a field.
They are parametric, which means they also have a value. The API for
writing plugins is not designed yet, but the syntax for using them is:

.. code:: nim

    Type {plugin: expr}: Value

Examples of plugins
~~~~~~~~~~~~~~~~~~~

- ``pos``: positions the ``stream`` at byte ``value`` before parsing and then
  resets it to the previous position
- ``cond``: wraps the field into an ``Option`` type and will only parse it if
  ``value`` is evaluated to ``true``
- ``size``: reads ``value`` bytes from the stream and creates a *substream*

You can combine multiple operations which will be applied to the field
in the specified order:

.. code:: nim

    8: shouldParse
    16 {cond: shouldParse.bool, size: 4}: x

First ``shouldParse.bool`` will be evaluted. If it's ``false``, parsing
won't happen; if it's true, then 4 bytes will be read from the stream
and a substream with them will be created. Then, 16 bits will be read
from this substeam. Finally, these bits will be wrapped into an
``Option`` and the resulting field will be an Option[int16].

When you produce a sequence, ``Operations`` apply to **the whole**
sequence (not each individual element).

Special notes
~~~~~~~~~~~~~

- Nim expressions may contain:
   - a previously defined field
   - a parser parameter
   - the ``e`` symbol if it's a repetition until expression
   - the ``i`` symbol if it's a repetition until expression
   - the ``s`` symbol if it's a repetition until or assertion expression

These last 3 symbols might conflict with your variables or fields, so you
shouldn't use them for something else.

This file is automatically generated from the documentation found in
binaryparse.nim. Use ``nim doc2 binaryparse.nim`` to get the full documentation.
