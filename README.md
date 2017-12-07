<p>This module implements a macro to create binary parsers. The parsers generated reads from a Stream and returns a tuple with each named field. The general format the macro takes is:</p>
<p><code>[type]&lt;size&gt;: &lt;name&gt;[options]</code></p>
<p>Where optional fields are in [] brackets and required fields are in &lt;&gt; brackets. Each field has separate meanings, as described in the table below:</p>
<table>
<thead>
<tr class="header">
<th>Name</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>type</td>
<td>This is the type of value found in this field, if no tpe is</td>
</tr>
<tr class="even">
<td></td>
<td>specified then it will be parsed as an integer. Supperted types</td>
</tr>
<tr class="odd">
<td></td>
<td>are <code>u</code> to get unsigned integers, <code>f</code> for floating point,</td>
</tr>
<tr class="even">
<td></td>
<td><code>s</code> for strings, and <code>*</code> for custom parser.</td>
</tr>
<tr class="odd">
<td>size</td>
<td>The size, in <em>bits</em>, of the field to read. For uint and int values</td>
</tr>
<tr class="even">
<td></td>
<td>from 1 to 64 inclusive are supported. For floats only 32 and 64</td>
</tr>
<tr class="odd">
<td></td>
<td>are supported. Strings use this field to specify the amount of</td>
</tr>
<tr class="even">
<td></td>
<td>characters to read into the string. If they don't specify a size</td>
</tr>
<tr class="odd">
<td></td>
<td>they will be read to the first NULL byte (this only applies to</td>
</tr>
<tr class="even">
<td></td>
<td>strings). When the custom parser type is specified the size field</td>
</tr>
<tr class="odd">
<td></td>
<td>is used to name the custom parser procedure.</td>
</tr>
<tr class="even">
<td>name</td>
<td>The name of the value, this will be used as the name in the</td>
</tr>
<tr class="odd">
<td></td>
<td>resulting tuple. If the value doesn't need to be stored one can</td>
</tr>
<tr class="even">
<td></td>
<td>use <code>_</code> as the name and it will not get a field in the result.</td>
</tr>
<tr class="odd">
<td>options</td>
<td>These will change the regular behaviour of reading into a field.</td>
</tr>
<tr class="even">
<td></td>
<td>Since they are so different in what they do they are described</td>
</tr>
<tr class="odd">
<td></td>
<td>below instead of in this table.</td>
</tr>
</tbody>
</table>
<p>Many binary formats include special &quot;magic&quot; sequences to identify the file or regions within it. The option <code>= &lt;value&gt;</code> can be used to check if a field has a certain value. If the value doesn't match a <a href="">MagicError</a> is raised. Value must match the value of the field it checks. When the field is a string type the exact length of the magic string is read, to include a terminating NULL byte use <code>\0</code> in the string literal.</p>
<p>To read more fields of a certain kind into a sequence you can use the option <code>[[count]]</code> (that is square brackets with an optional count inside). If no count is specified and the brackets left empty the next field needs to be a magic number and will be used to terminate the sequence. As count you can use the name of any previous field, literals, previously defined variables, or a combination.</p>
<p>Another thing commonly found in binary formats are repeating blocks or formats within the format. These can be read by using a custom parser. Custom parsers technically supports any procedure that takes a Stream as the first argument, however care must be taken to leave the Stream in the correct position. You can also define the inner format with a parser from this module and then pass that parser to the outer parser. This means that you can easily nest parsers. If you need values from the outer parser you can add parameters to the inner parser by giving it colon expressions before the body (e.g the call <code>createParser(list, size: uint16)</code> would create a parser <code>proc list(stream: Stream, size: uint16): &lt;return type&gt;</code>). To call a parser use the <code>*</code> type as described above and give it the name of the parser and any optional arguments. The stream object will get added automatically as the first parameter.</p>
<p>Example: In lieu of proper examples the binaryparse.nim file contains a <code>when isMainModule()</code> block showcasing how it can be used. The table below describes that block in a bit more detail:</p>
<table>
<thead>
<tr class="header">
<th>Format</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>u8: _ = 128</code></td>
<td>Reads an unsigned 8-bit integer and checks if it</td>
</tr>
<tr class="even">
<td></td>
<td>equals 128 without storing the value as a field in</td>
</tr>
<tr class="odd">
<td></td>
<td>returned tuple</td>
</tr>
<tr class="even">
<td><code>u16: size</code></td>
<td>Reads an unsigned 16-bit integer and names it</td>
</tr>
<tr class="odd">
<td></td>
<td><code>size</code> in the returned tuple</td>
</tr>
<tr class="even">
<td><code>4: data[size*2]</code></td>
<td>Reads a sequence of 4-bit integers into a <code>data</code></td>
</tr>
<tr class="odd">
<td></td>
<td>field in the returned tuple. Size is the value read</td>
</tr>
<tr class="even">
<td></td>
<td>above, and denotes the count of integers to read.</td>
</tr>
<tr class="odd">
<td><code>s: str[]</code></td>
<td>Reads null terminated strings into a <code>str</code> field in</td>
</tr>
<tr class="even">
<td></td>
<td>the returned tuple. Since it's given empty brackets</td>
</tr>
<tr class="odd">
<td></td>
<td>the next field needs to be a magic field and the</td>
</tr>
<tr class="even">
<td></td>
<td>sequence will be read until the magic is found.</td>
</tr>
<tr class="odd">
<td><code>s: _ = &quot;9xC\0&quot;</code></td>
<td>Reads a non-null terminated string and checks if it</td>
</tr>
<tr class="even">
<td></td>
<td>equals the magic sequence.</td>
</tr>
<tr class="odd">
<td><code>*list(size): inner</code></td>
<td>Uses a pre-defined procedure <code>list</code> which is called</td>
</tr>
<tr class="even">
<td></td>
<td>with the current Stream and the <code>size</code> read</td>
</tr>
<tr class="odd">
<td></td>
<td>earlier. Stores the return value in a field <code>inner</code></td>
</tr>
<tr class="even">
<td></td>
<td>in the returned tuple.</td>
</tr>
<tr class="odd">
<td><code>u8: _ = 67</code></td>
<td>Reads an unsigned 8-bit integer and checks if it</td>
</tr>
<tr class="even">
<td></td>
<td>equals 67 without storing the value.</td>
</tr>
</tbody>
</table>
