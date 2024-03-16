use std::fs::File;
use std::io::{Seek, Write};
use crate::{BinOptions, BinStream, Decode, Encode, NumEncoding};

#[derive(Encode, Decode)]
#[ende(num: little_endian)]
pub struct StructStruct {
	#[ende(skip)]
	value1: u64,
	value2: f64,
	unit: ()
}

#[derive(Encode, Decode)]
#[ende(num: little_endian)]
pub struct TupleStruct(
	#[ende(skip)]
	u64,
	f64
);

#[derive(Encode, Decode)]
pub struct UnitStruct;

#[derive(Encode, Decode)]
#[repr(isize)]
#[ende(size: 64; variant: 64)]
pub enum Enum {
	StructVariant {
		value1: u64,
		value2: f64
	},
	TupleVariant(
		u64,
		#[ende(if = *m0 >= 154)]
		f64
	) = u16::MAX as isize,
	UnitVariant
}

#[derive(Encode, Decode)]
pub struct EmptyStructStruct {}

#[derive(Encode, Decode)]
pub struct EmptyTupleStruct();

#[derive(Encode, Decode)]
#[ende(variant: little_endian)]
pub enum EmptyVariantEnum {
	StructVariant {},
	TupleVariant()
}

#[derive(Encode, Decode)]
pub enum EmptyEnum {
}

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
pub struct VersionContainer {
	name_present: bool,
	#[ende(if = *name_present)]
	name: String,
}

#[test]
pub fn test() {
	let mem = File::options().create(true).write(true).read(true).open("./test.bin").unwrap();
	let mut options = BinOptions::default();
	options.num_repr.num_encoding = NumEncoding::Leb128Int;
	let mut stream = BinStream::new(mem, options);

	let orig = i128::MIN;
	println!("{:#0130b}", orig);
	println!("{orig}");
	stream.write_i128(orig).unwrap();
	stream.stream.flush().unwrap();
	stream.stream.rewind().unwrap();
	let val = stream.read_i128().unwrap();
	println!("{:#0130b}", val);
	println!("{val}");
}