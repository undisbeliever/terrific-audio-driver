spc700asm - An spc700 assembler
===============================

A basic spc-700 assembler, explicitly designed to compile the Terrific Audio Driver.


Features
--------
 * Usable in a rust `build.rs` script
 * Assembly instructions use the sony instruction convention as
   [documented on the SNESdev Wiki](https://snes.nesdev.org/wiki/SPC-700_instruction_set)
 * Forward referenced `.inline` procedures
 * `.proc` procedures with scoped names
 * Direct Page flag tracking
 * Structs
 * Mesen mlb label file output



Limitations
-----------
This assembler is designed to only compile the Terrific Audio Driver.
It has the following deliberate limitations:

 * There is only a single code bank
 * Direct page addresses must be known before they are used in an instruction
 * No `-` or `+` anonymous labels
 * No namespaces
 * No substitutions or preprocessing (think `.IDENT(.SPRINTF(..))` in ca65 or `{}` in bass)
 * No macros
 * An `.inline` can only be called once
 * Unknown symbol error messages do not print the unknown symbol name



Licensing
---------
The spc-700 assembler is licensed under the MIT License.



Syntax
------
 * Everything after a `;` semicolon is a comment.
 * `<name> = <expression>` - create a constant
 * `<name>:` - define a label


Commands:

 * `.include "filename.ext"` - includes a file
    * Only the main assembly file can `.include` files

 * `.codebank <start_address>..<end_address>` - allocates the code block (can only be used once)
 * `.varbank <name> <start_address>..<end_address` - defines a variable bank

 * `.p0` - Addresses from $00 - $ff are direct-page when selecting the instruction
 * `.p1` - Addresses from $100 - $1ff are direct-page when selecting the instruction

 * `.db <expr> [, <expr>]` - 8-bit byte data
 * `.dw <expr> [, <expr>]` - 16-bit word data
 * `.repeatdb <name> in <start>..<end>, <expr>` - Repeat `.db` with an incrementing counter symbol
    * `.repeatdb i in 0..5, i * 3` is the equivalent of `.db 0, 3, 6, 9, 12`

 * `.assert <expr>` - Adds an assertion
    * The `PC` symbol will point to the current address of the assert statement.
    * The asserts are checked after the binary has been assembled.


---


Structure syntax:
```
    .struct StructName
        byte : u8
        word : u16
        array : [u16 : COUNT]
        child_struct : StructName
        child_array : [StructName : COUNT]
    .endstruct
```


Variables must be declared inside a `.var` statement block:
```
    .vars varbank_name
        byte : u8
        pointer : ptr
        struct : StructName
        array : [u8 : COUNT]
    .endvars
```

 * The following types are pre-defined: `u8`, `i8`, `u16`, `i16`, `ptr`.
 * Struct variables and fields will emit field name - ie, `struct.byte`
 * The 16 bit types (`u16`, `i16`, `ptr`) will automatically create `.l` and `.h` child labels.
   Ie, `pointer.l`, `pointer.h`, `struct.array.l` and `struct.array.h`.


---


Procedures are enclosed between `.proc` and `.endproc` statements.
 * Labels and constants inside the procedure will be scoped to the procedure.
 * A procedure's labels and constants can be accessed with `.`, ie, `ProcName.Label`
 * `.proc` cannot be nested

Inline procedures are enclosed between `.inline` and `.endinline` statements.
 * The same scoping rules as `.proc` procedures apply
 * The symbol file will contain the start and end address of the `.inline`
   (as `name` and `name.__END__`)
 * The `.inline` will be placed in a new scope and cannot access the caller's scope
 * An `.inline` is invoked by using its name on a line without arguments
 * An `.inline` can be forward-referenced (defined after use)
 * An `.inline` can only be used once
 * An error is emitted if the `.inline` is unused

Procedure and inline example:
```
    .proc Proc
        mov A, #1
    
        InlineProc

        ; A = 6

        ret
    .endproc

    .inline InlineProc
        clrc
        adc A, #5
    .endinline
```


----


Number formats:
 * `123456789` - decimals
 * `$ff` - hexadecimal
 * `%0110` - binary
 * Numbers can contain `_` digit separator (ie, `%1011_0011`)


Expressions support the following helper functions:
 * `lobyte(expr)` - the low-byte of the expression (`expr & 0xff`)
 * `hibyte(expr)` - the second byte of the expression (`(expr >> 8) & 0xff`)
 * `page(expr)` - the address's page (`expr >> 8`)
 * `inZeropage(address)` - true if the address is in zeropage ($00..=$ff)
 * `inFirstpage(address)` - true if the address is in first page ($100..=$1ff)
 * `offsetof(Struct, field`) - returns the struct field offset


