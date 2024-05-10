# ende

Ende is an **EN**coding and **DE**coding library for writing custom protocols and file formats.

It aims to be **intuitive** and **expandable**, as well as **time efficient**.

## Example

```rust
use ende::*;
use ende::io::*;

#[derive(Debug, PartialEq, Encode, Decode)]
#[ende(crate: ende)]
struct Person {
	name: String,
    age: u64,
    height: f32,
    eye_color: EyeColor,
}

#[derive(Debug, PartialEq, Encode, Decode)]
#[ende(crate: ende)]
enum EyeColor {
    Brown,
    Blue,
    Green,
    Gray,
    Black
}

let mut data = vec![0u8; 256];

// Summon John into existence!
let john = Person {
    name: String::from("John"),
    age: 35,
	height: 1.75,
	eye_color: EyeColor::Brown
};

// Encode John into the matrix
encode_with(
	SliceMut::new(&mut data),
	Context::new(),
	&john
).unwrap();

// Bring him back
let john_2: Person = decode_with(
	Slice::new(&data),
	Context::new()
).unwrap();

// But is he really the same John?
assert_eq!(john, john_2);
```

## Motivation

One of the main reasons I made this library is because I found myself
needing more sophisticate macros and runtime flexibility for existing
binary formats.

While for example [`bincode`](https://crates.io/crates/bincode) is perfectly
ok for many applications, `ende` was made with compatibility with existing
data formats in mind.

For this very purpose, many internal details of the encoder are exposed
through settings or the derive macros themselves, for the purpose of fine-tuning
the data format exactly how you want it, while providing an easy-to-understand interface.

## Deriving

A big selling point of `ende` are its macros, which allow you to heavily
customize the codegen through a series of attributes.
To learn more about those, check out `DERIVE.md` in this crate's repository root.

## Where to start

TODO add examples

## MSRV

This crate will always target the latest version of rust, in order
to get the access to new features as soon as they are released and
update the code accordingly if beneficial.
Of course, breaking API changes will be accompanied by a major version
bump.

## Future plans

I plan on adding support for a `SeekEncode` and `SeekDecode` set of traits
and relative macro attributes for file formats where the data is not laid sequentially.

I also plan on adding support for `async` io through a feature gate.

License: MIT
