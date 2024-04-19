//! Contains some fake functions for encryption/decryption, compression/decompression
//! to test the derive macros.

/// In case that was not made clear: the functions in this module are fake!<br>
/// They are just here to allow testing the derive macros in the playground.
pub mod fake {
	pub mod rsa {
		use embedded_io::{Read, Write};

		use crate::{Decode, Encode, Encoder, EncodingResult};

		pub fn encode<V: Encode, T: Write>(_value: V, _encoder: &mut Encoder<T>, _public_key: &[u8], _private_key: &[u8]) -> EncodingResult<(), T::Error> {
			// Rsa encryption code here
			unimplemented!()
		}

		pub fn decode<V: Decode, T: Read>(_decoder: &mut Encoder<T>, _public_key: &[u8], _private_key: &[u8]) -> EncodingResult<V, T::Error> {
			// Rsa decryption code here
			unimplemented!()
		}
	}

	pub mod aes {
		use embedded_io::{Read, Write};

		use crate::{Encoder, EncodingResult};

		pub fn encode<Orig, F>(_encoder: &mut Encoder<Orig>, _fun: F, _iv: &[u8], _key: &[u8]) -> EncodingResult<(), Orig::Error>
			where Orig: Write,
			      F: FnOnce(&mut Encoder<Orig>) -> EncodingResult<(), Orig::Error>
		{
			// Aes encryption code here
			unimplemented!()
		}

		pub fn decode<Orig, Val, F>(_encoder: &mut Encoder<Orig>, _fun: F, _iv: &[u8], _key: &[u8]) -> EncodingResult<Val, Orig::Error>
			where Orig: Read,
			      F: FnOnce(&mut Encoder<Orig>) -> EncodingResult<Val, Orig::Error>
		{
			// Aes decryption code here
			unimplemented!()
		}
	}

	pub mod zlib {
		pub use super::gzip::*;
	}

	pub mod gzip {
		use embedded_io::{Read, Write};

		use crate::{Encoder, EncodingResult};

		pub fn encode<Orig, F>(_encoder: &mut Encoder<Orig>, _fun: F, _compression_level: u32) -> EncodingResult<(), Orig::Error>
			where Orig: Write,
			      F: FnOnce(&mut Encoder<Orig>) -> EncodingResult<(), Orig::Error>
		{
			// Compression code here
			unimplemented!()
		}

		pub fn decode<Orig, Val, F>(_encoder: &mut Encoder<Orig>, _fun: F, _compression_level: u32) -> EncodingResult<Val, Orig::Error>
			where Orig: Read,
			      F: FnOnce(&mut Encoder<Orig>) -> EncodingResult<Val, Orig::Error>
		{
			// Decompression code here
			unimplemented!()
		}
	}
}