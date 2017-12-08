binaryparse
===========
This module implements a macro to create binary parsers. The parsers
generated reads from a Stream and returns a tuple with each named field.
The general format the macro takes is:

``[type]<size>: <name>[options]``

Where optional fields are in [] brackets and required fields are in <>
brackets. Each field has separate meanings, as described in the table below:

========== ==================================================================
Name       Description
---------- ------------------------------------------------------------------
type       This is the type of value found in this field, if no type is
           specified then it will be parsed as an integer. Supported types
           are ``u`` to get unsigned integers, ``f`` for floating point,
           ``s`` for strings, and ``*`` for custom parser.
size       The size, in *bits*, of the field to read. For uint and int values
           from 1 to 64 inclusive are supported. For floats only 32 and 64
           are supported. Strings use this field to specify the amount of
           characters to read into the string. If they don't specify a size
           they will be read to the first NULL byte (this only applies to
           strings). When the custom parser type is specified the size field
           is used to name the custom parser procedure.
name       The name of the value, this will be used as the name in the
           resulting tuple. If the value doesn't need to be stored one can
           use ``_`` as the name and it will not get a field in the result.
options    These will change the regular behaviour of reading into a field.
           Since they are so different in what they do they are described
           below instead of in this table.
========== ==================================================================

Many binary formats include special "magic" sequences to identify the file
or regions within it. The option ``= <value>`` can be used to check if a
field has a certain value. If the value doesn't match a MagicError is
raised. Value must match the value of the field it checks. When the field is
a string type the exact length of the magic string is read, to include a
terminating NULL byte use ``\0`` in the string literal.

To read more fields of a certain kind into a sequence you can use the option
``[[count]]`` (that is square brackets with an optional count inside). If no
count is specified and the brackets left empty the next field needs to be a
magic number and will be used to terminate the sequence. As count you can use
the name of any previous field, literals, previously defined variables, or a
combination.

Another thing commonly found in binary formats are repeating blocks or
formats within the format. These can be read by using a custom parser.
Custom parsers technically supports any procedure that takes a Stream as the
first argument, however care must be taken to leave the Stream in the correct
position. You can also define the inner format with a parser from this module
and then pass that parser to the outer parser. This means that you can easily
nest parsers. If you need values from the outer parser you can add parameters
to the inner parser by giving it colon expressions before the body (e.g the
call ``createParser(list, size: uint16)`` would create a parser
``proc list(stream: Stream, size: uint16): <return type>``). To call a parser
use the ``*`` type as described above and give it the name of the parser and
any optional arguments. The stream object will get added automatically as the
first parameter.

Example:
In lieu of proper examples the binaryparse.nim file contains a ``when
isMainModule()`` block showcasing how it can be used. The table below
describes that block in a bit more detail:

======================= =====================================================
Format                  Description
----------------------- -----------------------------------------------------
``u8: _ = 128``         Reads an unsigned 8-bit integer and checks if it
                        equals 128 without storing the value as a field in
                        returned tuple
``u16: size``           Reads an unsigned 16-bit integer and names it
                        ``size`` in the returned tuple
``4: data[size*2]``     Reads a sequence of 4-bit integers into a ``data``
                        field in the returned tuple. Size is the value read
                        above, and denotes the count of integers to read.
``s: str[]``            Reads null terminated strings into a ``str`` field in
                        the returned tuple. Since it's given empty brackets
                        the next field needs to be a magic field and the
                        sequence will be read until the magic is found.
``s: _ = "9xC\0"``      Reads a non-null terminated string and checks if it
                        equals the magic sequence.
``*list(size): inner``  Uses a pre-defined procedure ``list`` which is called
                        with the current Stream and the ``size`` read
                        earlier. Stores the return value in a field ``inner``
                        in the returned tuple.
``u8: _ = 67``          Reads an unsigned 8-bit integer and checks if it
                        equals 67 without storing the value.
======================= =====================================================

This file is automatically generated from the documentation found in
binaryparse.nim. Use ``nim doc2 binaryparse.nim`` to get the full documentation.
