# Ender

![Crates.io Version](https://img.shields.io/crates/v/ender)

A rust **EN**coding and **DE**coding library for writing custom protocols and file formats.

It aims to be **intuitive**, **expandable** and **correct**.

## Example

```rust
let mut the_matrix = vec![0u8; 256];

// Summon John into existence!
let john = Person {
    name: String::from("John"),
    age: 35, 
    height: 1.75,
    eye_color: EyeColor::Brown
};

// Encode John into the matrix
encode_with(SliceMut::new(&mut the_matrix), Context::default(), &john)?;

// Bring him back
let john_2: Person = decode_with(Slice::new(&the_matrix), Context::default())?;

// But is he really the same John?
assert_eq!(john, john_2);
```

## Encoding format

The encoding process aims at being **correct** and **unsurprising**.



Ende supports a series of options, which can be changed *during*
the encoding/decoding process to get certain parts of a binary format
to be represented exactly how you want them.

This can be done in a manual implementation as well as with the derive
macro, using the custom attributes provided.

Certain types also support "flattening", which means omitting information
known from the context.
For instance, you can omit writing whether an `Option` is present if
that information is already stored somewhere else in the file format.

For *integer primitives*, *usize*, and *enum variants* you can customize the endianness,
the numerical encoding (read: var-ints), the bit-width (how many bytes
does a `usize` or enum variant take up in your encoding format?),
the max-size (to prevent maliciously crafted binary formats to cause
unlimited-size allocations).

#### Var-int format
- Fixed - not var-int, simply encode the number as-is
- Leb128
- Protobuf - both its zigzag and "wasteful" variants

#### String formats
As for strings, currently length-prefixed, null-terminated (with and without
a maximum length) strings are supported, as well as the following encoding formats.
- Ascii
- Utf8
- Utf16
- Utf32
- Windows1252

If you need a new var-int encoding or string encoding added, feel free
to open a PR!

## Motivation

One of the main reasons I made this library is because I found myself
needing more sophisticate macros and runtime flexibility for existing
binary formats.

While for example [`bincode`](https://crates.io/crates/bincode) is perfectly
ok for many applications, `ender` was made with compatibility with existing
data formats in mind.

For this very purpose, many internal details of the encoder are exposed
through settings or the derive macros themselves, for the purpose of fine-tuning
the data format exactly how you want it, while providing an easy-to-understand interface.

## Deriving

A big selling point of `ender` are its macros, which allow you to heavily
customize the codegen through a series of attributes.
To learn more about those, check out `DERIVE.md` in this crate's repository root.

## MSRV

This crate will always target the latest version of rust, in order
to get access to new features as soon as they are released and
update the code accordingly if beneficial.
Of course, breaking API changes will be accompanied by a major version
bump.

## Future plans

I plan on adding support for `async` io through a feature gate.