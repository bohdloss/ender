#![allow(unused)]

use crate::facade::fake::*;

use ende_derive::{BorrowDecode, Decode, Encode};

#[derive(BorrowDecode)]
pub struct Borrow<'a, 'b> {
    string: &'a str,
    data: &'b [u8],
}

#[derive(Encode, Decode)]
#[ende(num: little_endian)]
#[allow(unused)]
#[cfg(feature = "std")]
pub struct StructStruct {
    #[ende(skip)]
    #[ende(default: 100)]
    value1: u64,
    #[ende(if: *value1 > 10)]
    #[ende(default: *value1 as f32)]
    #[ende(as: u8)]
    value2: f32,
    unit: (),
    #[ende(with: rsa(&[], &[]))]
    secret: Vec<u8>,
}

#[derive(Encode, Decode)]
pub struct TupleStruct(
    u64,
    #[ende(num: leb128, big_endian; size: max = 15, 16, little_endian, fixed; variant: 128, fixed, big_endian; flatten: 12)]
     f64,
);

#[derive(Encode, Decode)]
pub struct UnitStruct;

#[derive(Encode, Decode)]
#[repr(isize)]
#[ende(size: 64; variant: 64, leb128)]
pub enum Enum {
    StructVariant {
        value1: u64,
        #[ende(validate: *value2 == 16.0, "Expected 16.0, got {}", value2)]
        value2: f64,
    },
    TupleVariant(u64, #[ende(if: *m0 >= 154)] f64) = u16::MAX as isize,
    UnitVariant,
}

#[derive(Encode, Decode)]
pub struct EmptyStructStruct {}

#[derive(Encode, Decode)]
pub struct EmptyTupleStruct();

#[derive(Encode, Decode)]
#[ende(variant: little_endian)]
pub enum EmptyVariantEnum {
    StructVariant {},
    TupleVariant(),
}

#[derive(Encode, Decode)]
pub enum EmptyEnum {}

// #[derive(Encode, Decode)]
// pub union Union {
// 	value1: u64,
// 	value2: f64
// }
//
// Will yield the following errors:
//
// error: Encode cannot be derived on unions
//   --> src/test.rs:42:5
//    |
// 42 | pub union Union {
//    |     ^^^^^
//
// error: Decode cannot be derived on unions
//   --> src/test.rs:42:5
//    |
// 42 | pub union Union {
//    |     ^^^^^

#[derive(Encode, Decode)]
#[ende(with: aes)]
#[cfg(feature = "std")]
pub struct VersionContainer {
    name_present: bool,
    #[ende(if: *name_present)]
    name: String,
}

#[test]
pub fn test() {
    // let mut mem = [0u8; 1024];
    // let mut options = BinSettings::default();
    // options.num_repr.num_encoding = NumEncoding::Leb128;
    // let mut stream = Encoder::new(&mut mem, Context::with_options(options));
    //
    // let orig = i128::MIN;
    // println!("{:#0130b}", orig);
    // println!("{orig}");
    // stream.write_i128(orig).unwrap();
    // stream.stream.flush().unwrap();
    // stream.stream.rewind().unwrap();
    // let val = stream.read_i128().unwrap();
    // println!("{:#0130b}", val);
    // println!("{val}");
}
