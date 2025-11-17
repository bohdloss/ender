#![allow(unused)]

// #[derive(BorrowDecode)]
// pub struct Borrow<'a, 'b> {
//     string: &'a str,
//     #[ender(borrow)]
//     data: &'b [u8],
// }
//
// #[derive(Encode, Decode)]
// #[ender(num: little_endian)]
// #[allow(unused)]
// #[cfg(feature = "std")]
// pub struct StructStruct {
//     #[ender(skip)]
//     #[ender(default: 100)]
//     value1: u64,
//     #[ender(if: *value1 > 10)]
//     #[ender(default: *value1 as f32)]
//     #[ender(as: u8)]
//     value2: f32,
//     unit: (),
//     #[ender(with: rsa(&[], &[]))]
//     secret: Vec<u8>,
// }
//
// #[derive(Encode, Decode)]
// struct MyStruct {
//     secret_key: Vec<u8>,
//     iv: Vec<u8>,
//     /// While **encoding**, this field is compressed -> encrypted.
//     /// While **decoding**, this field is decrypted -> decompressed.
//     #[ender(redir: gzip(9))]
//     #[ender(redir: aes(iv, secret_key))]
//     super_secret_data: Vec<u8>,
//     file_pointer: usize,
//     /// Marks the current offset, seeks to `file_pointer` bytes from the start of the file,
//     /// encodes/decodes the field, then seeks back.
//     #[ender(ptr start: * file_pointer)]
//     apple_count: u64,
//     /// This field is effectively laid *right after* `file_pointer`
//     /// in the binary representation.
//     other_data: i32,
// }
//
// #[derive(Encode, Decode)]
// pub struct TupleStruct(
//     u64,
//     #[ender(num: leb128, big_endian; size: max = 15, bit16, little_endian, fixed; variant: bit128, fixed, big_endian)]
//      f64,
// );
//
// #[derive(Encode, Decode)]
// pub struct UnitStruct;
//
// #[derive(Encode, Decode)]
// #[repr(isize)]
// #[ender(size: bit64; variant: bit64, leb128)]
// pub enum Enum {
//     StructVariant {
//         value1: u64,
//         #[ender(validate: *value2 == 16.0, "Expected 16.0, got {}", value2)]
//         value2: f64,
//     },
//     TupleVariant(u64, #[ender(if: *m0 >= 154)] f64) = u16::MAX as isize,
//     UnitVariant,
// }
//
// #[derive(Encode, Decode)]
// pub struct EmptyStructStruct {}
//
// #[derive(Encode, Decode)]
// pub struct EmptyTupleStruct();
//
// #[derive(Encode, Decode)]
// #[ender(variant: little_endian)]
// pub enum EmptyVariantEnum {
//     StructVariant {},
//     TupleVariant(),
// }
//
// #[derive(Encode, Decode)]
// pub enum EmptyEnum {}
//
// // #[derive(Encode, Decode)]
// // pub union Union {
// // 	value1: u64,
// // 	value2: f64
// // }
// //
// // Will yield the following errors:
// //
// // error: Encode cannot be derived on unions
// //   --> src/test.rs:42:5
// //    |
// // 42 | pub union Union {
// //    |     ^^^^^
// //
// // error: Decode cannot be derived on unions
// //   --> src/test.rs:42:5
// //    |
// // 42 | pub union Union {
// //    |     ^^^^^
//
// #[derive(Encode, Decode)]
// #[ender(with: aes)]
// #[cfg(feature = "std")]
// pub struct VersionContainer {
//     name_present: bool,
//     #[ender(if: *name_present)]
//     name: String,
// }

use crate::io::{Read, Slice, SliceMut, VecStream};
use crate::{BinSettings, BitWidth, Context, Decode, Encode, Encoder, EncodingResult, Endianness, NumEncoding, NumRepr, SizeRepr, StrEncoding, StrLen, StringRepr, VariantRepr};
use std::hash::{DefaultHasher, Hasher};

const SETTINGS: BinSettings = BinSettings {
    num_repr: NumRepr {
        endianness: Endianness::LittleEndian,
        num_encoding: NumEncoding::Fixed,
    },
    size_repr: SizeRepr {
        endianness: Endianness::LittleEndian,
        num_encoding: NumEncoding::Fixed,
        width: BitWidth::Bit64,
        max_size: isize::MAX as usize,
    },
    variant_repr: VariantRepr {
        endianness: Endianness::LittleEndian,
        num_encoding: NumEncoding::Fixed,
        width: BitWidth::Bit32,
    },
    string_repr: StringRepr {
        encoding: StrEncoding::Utf8,
        endianness: Endianness::LittleEndian,
        len: StrLen::LengthPrefixed,
    },
};

macro_rules! test_num_encoding {
    ($fn_name:ident, $encoding_name:ident) => {
        #[test]
        pub fn $fn_name() {
            #[derive(PartialEq, Eq, Debug, Encode, Decode)]
            #[ender(variant: bit8)]
            enum MaybeSigned {
                Signed(i64),
                Unsigned(u64),
            }

            let mut settings = SETTINGS;
            settings.num_repr.num_encoding = NumEncoding::$encoding_name;
            settings.size_repr.num_encoding = NumEncoding::$encoding_name;
            settings.variant_repr.num_encoding = NumEncoding::$encoding_name;

            let mut data = vec![0u8; 1000000];
            let mut encoder =
                Encoder::new(SliceMut::new(&mut data), Context::with_settings(settings));

            let mut orig = Vec::new();

            let mut hash = DefaultHasher::new();
            for x in 0..4196 {
                hash.write_i32(x);
                let val = hash.finish();
                let val = if (val % 2) == 0 {
                    MaybeSigned::Unsigned(val)
                } else {
                    MaybeSigned::Signed(-(val as i64))
                };

                orig.push(val);
            }
            orig.push(MaybeSigned::Unsigned(u64::MAX));
            orig.push(MaybeSigned::Signed(i64::MAX));
            orig.push(MaybeSigned::Signed(i64::MIN));

            orig.encode(&mut encoder).unwrap();

            let mut decoder = Encoder::new(Slice::new(&data), Context::with_settings(settings));

            let other: Vec<MaybeSigned> = Vec::decode(&mut decoder).unwrap();

            assert_eq!(orig, other);
        }
    };
}

test_num_encoding!(fixed_encoding, Fixed);
test_num_encoding!(leb128, Leb128);
test_num_encoding!(protobuf_wasteful, ProtobufWasteful);
test_num_encoding!(protobuf_zz, ProtobufZigzag);

macro_rules! test_str_encoding {
    (fn $fn_name:ident($settings:ident, max: $lit:literal) { $($tt:tt)* }) => {
	    #[test]
	    pub fn $fn_name() {
		    fn internal_test(mut settings: BinSettings) {
			    fn super_internal_test($settings: BinSettings) {
				    $($tt)*
			    }

			    settings.string_repr.len = StrLen::LengthPrefixed;
			    super_internal_test(settings);
			    settings.string_repr.len = StrLen::NullTerminated;
			    super_internal_test(settings);
			    settings.string_repr.len = StrLen::NullTerminatedFixed($lit);
			    super_internal_test(settings);

		    }

		    // Test it for every string encoding
		    let mut settings = SETTINGS;
		    settings.string_repr.encoding = StrEncoding::Utf8;
		    internal_test(settings);
		    settings.string_repr.encoding = StrEncoding::Utf16;
		    internal_test(settings);
		    settings.string_repr.encoding = StrEncoding::Utf32;
		    internal_test(settings);
	    }
    };
}

test_str_encoding! {
    fn simple(settings, max: 200) {
        let mut data = vec![0u8; 1000000];
        let mut encoder = Encoder::new(SliceMut::new(&mut data), Context::with_settings(settings));

        const STRING: &str = "Hello, world!";

        encoder.write_str(STRING.chars()).unwrap();

        let mut decoder = Encoder::new(Slice::new(&data), Context::with_settings(settings));

        let string: String = decoder.read_str().unwrap();
        assert_eq!(&string as &str, STRING);
    }
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

fn decode_owned<T: for<'de> Decode<Slice<'de>>>(bytes: &[u8]) -> T {
    let mut encoder = Encoder::new(Slice::new(bytes), Context::default());
    T::decode(&mut encoder).unwrap()
}

#[derive(Decode)]
struct IImplDecodeOwned {
    pub a: i32,
    pub b: u64,
    pub c: String,
}

#[derive(Decode)]
struct IImplDecode<'a> {
    #[ender(borrow)]
    pub a: &'a [u8]
}

#[test]
fn impls_test() {
    // let _ = decode_owned::<IImplDecodeOwned>(&[]); // Should compile
    // let _ = decode_owned::<IImplDecode>(&[]); // Should fail to compile
}

// Neither works due to trait method signature mismatching. YAY!

// struct BorrowFromEncoderReference<'a> {
//     ctx: &'a crate::BinSettings
// }
//
// impl<'a, R: Read> Decode<R> for BorrowFromEncoderReference<'a> {
//     fn decode(decoder: &'a mut Encoder<R>) -> EncodingResult<Self> {
//         Ok(Self{
//             ctx: &decoder.ctxt.settings
//         })
//     }
// }

// struct BorrowFromCtx<'ctx> {
//     ctx: Context<'ctx>
// }
//
// impl<'ctx, R: Read> Decode<R> for BorrowFromCtx<'ctx> {
//     fn decode(decoder: &mut Encoder<'ctx, R>) -> EncodingResult<Self> {
//         Ok(Self {
//             ctx: decoder.ctxt
//         })
//     }
// }