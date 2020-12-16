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

This section includes the following features (only name is mandatory):

- name
- repetition
- assertion

If you don't *really* want a name, you can discard the symbol by using ``_`` in its place:

.. code:: nim

    createParser(myParser):
      8: _

Alignment
~~~~~~~~~

If any of the following is violated, binaryparse should generate an exception:

- Byte endianness can only be used with byte-multiple integers
- Bit endianness must be uniform between **byte boundaries**
- Spec must finish on a byte boundary

.. code:: nim

   createParser(myParser, bitEndian = n):
     b9: a # error: cannot apply byte endianness
     r6: b # error: shares bits with previous byte
     10: c # error: spec does not finish on a byte boundary

Moreover, unaligned reads for strings are not supported:

.. code:: nim

    createParser(myParser):
      6: x
      s: y # invalid, generates an exception

Assertion
~~~~~~~~~

Use ``= expr`` for producing an exception if the parsed value doesn't
match ``expr``:

.. code:: nim

    s: x = "binaryparse is awesome"
    8: y[5] = @[0, 1, 2, 3, 4]

Assertion can also be used in a special manner to terminate the previous
field if it's a **string** or a **sequence indicated as magic-terminated**.
This is discussed in later sections.

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

Repetition
~~~~~~~~~~

There are 3 ways to produce a ``seq`` of your ``Type``:

- ``for``: append ``[expr]`` to the name for repeating ``expr``
  times
- ``until``: append ``{expr}`` to the name for repeating until
  ``expr`` is evaluated to ``true``
- ``magic``: enclose name with ``{}`` and use assertion with
  your **next** field

In until repetition you can use 3 special symbols:

- ``e``: means 'last element read'
- ``i``: means 'current loop index'
- ``s``: means 'stream'

.. code:: nim

    8: a[5] # reads 5 8-bit integers
    8: b{e == 103 or i > 9} # reads until it finds the value 103 or completes 10th iteration
    8: {c} # reads 8-bit integers until next field is matches
    3: _ = 0b111 # magic value can be of any type
    u8: {d[5]} # reads byte sequences each of length 5 until next field matches
    s: _ = "END"

Substreams
~~~~~~~~~~

Call syntax forces the creation of a substream:

.. code:: nim

    createParser(aux, size: int):
      8: x[size]
    createParser(myParser):
      8: use = 4
      8: limit = 8
      *inner(size): aux(limit)

In the above example, ``limit`` bytes (8 in this case) will be read from the main ``BitStream``.
Then, a substream will be created out of them, which will then be used as the stream for parsing ``inner``.
Since ``inner`` will only use 4 of them, the remaining 4 will effectively be discarded.

Note that unlike in ``Type``, here size is counted bytes. It is implied that you cannot create
a substream if your bitstream is unaligned.

This feature is **not implemented for repetition** because it would increase complexity with little benefits.
The following syntax is **invalid** and you should use the technique with the auxiliary complex type shown above:

.. code:: nim

    createParser(myParser):
      u8: a[4](6) # does substream refer to each individual element or the whole sequence?

Strings
~~~~~~~

Strings are special because they don't have a fixed size. Therefore, you
must provide enough information regarding their termination. This can be
achieved with one of the following:

- Use of substream
- Assertion
- Magic

.. code:: nim

    s: a # invalid: next field doesn't use assertion
    s: b(5) # string of length 5
    s: c = "ABC" # reads a string of length 3 that must match "ABC"
    s: d # reads a string until next field is matched
    s: _ = "MAGIC"
    s: e[5] # reads 5 null-terminated strings
    s: {f} # reads null-terminated strings until next field matches
    3: term = 0b111 # terminator of the above sequence
    s: {g[5]} # sequence of 5-length sequences of null-terminated strings
    s: _ = "END_NESTED"

Clarifications:

- **When and only when using repetition** on strings they are implicitly
  null-terminated
- When using both a substream and an assertion in the next field, the
  substream takes precedence and the next field is not magic

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
