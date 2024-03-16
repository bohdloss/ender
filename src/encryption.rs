use std::io::{BufRead, BufReader, Read, Write};
use cryptostream::read::Decryptor;
use openssl::error::ErrorStack;
use openssl::symm::Cipher;
use parse_display::Display;
use thiserror::Error;
use crate::{Encode, Decode, EncodingError, EncodingResult, BinStream};

pub struct RsaBlock<const BLOCK_SIZE: usize>([u8; BLOCK_SIZE]);

impl Encode for RsaBlock<1024> {
	fn encode<T: Write>(&self, _encoder: &mut BinStream<T>) -> EncodingResult<()> {
		todo!()
	}
}

impl Decode for RsaBlock<1024> {
	fn decode<T: Read>(_decoder: &mut BinStream<T>) -> EncodingResult<Self> where Self: Sized {
		todo!()
	}
}

impl Encode for RsaBlock<2048> {
	fn encode<T: Write>(&self, _encoder: &mut BinStream<T>) -> EncodingResult<()> {
		todo!()
	}
}

impl Decode for RsaBlock<2048> {
	fn decode<T: Read>(_decoder: &mut BinStream<T>) -> EncodingResult<Self> where Self: Sized {
		todo!()
	}
}

impl Encode for RsaBlock<4096> {
	fn encode<T: Write>(&self, _encoder: &mut BinStream<T>) -> EncodingResult<()> {
		todo!()
	}
}

impl Decode for RsaBlock<4096> {
	fn decode<T: Read>(_decoder: &mut BinStream<T>) -> EncodingResult<Self> where Self: Sized {
		todo!()
	}
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Display, Encode, Decode)]
#[repr(u8)]
#[ende(bit_width = 8)]
pub enum AesBits {
	#[display("128")]
	N128,
	#[display("192")]
	N192,
	#[display("256")]
	N256
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Display, Encode, Decode)]
#[repr(u8)]
#[ende(bit_width = 8)]
pub enum AesMode {
	#[display("ECB")]
	Ecb,
	#[display("CBC")]
	Cbc
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Display, Encode, Decode)]
#[ende(bit_width = 8)]
pub enum Encryption {
	#[display("no encryption")]
	None,
	#[display("{1} {0}-bit AES")]
	Aes(AesBits, AesMode)
}

impl Encryption {
	pub fn decrypt<T: Read>(&self, mut input: T, key: Option<&[u8]>) -> EncodingResult<Decrypt<T>> {
		match self {
			Encryption::None => {
				Ok(Decrypt(DecryptInner::None(BufReader::new(input))))
			}
			Encryption::Aes(bits, mode) => {
				let key = key.ok_or(CryptoError::NoKey)?;
				let cipher = match mode {
					AesMode::Ecb => match bits {
						AesBits::N128 => Cipher::aes_128_ecb(),
						AesBits::N192 => Cipher::aes_192_ecb(),
						AesBits::N256 => Cipher::aes_256_ecb()
					}
					AesMode::Cbc => match bits {
						AesBits::N128 => Cipher::aes_128_cbc(),
						AesBits::N192 => Cipher::aes_192_cbc(),
						AesBits::N256 => Cipher::aes_256_cbc()
					}
				};
				if cipher.key_len() != key.len() {
					return Err(CryptoError::WrongKeySize(cipher.key_len()).into());
				}

				let iv_size = cipher.iv_len().unwrap_or(0);
				let mut iv = Vec::with_capacity(iv_size);

				input.read_exact(&mut iv)?;

				let crypto_stream = Decryptor::new(input, cipher, &key, &iv)
					.map_err(|x| EncodingError::EncryptionError(x.into()))?;
				Ok(Decrypt(DecryptInner::Decryptor(BufReader::new(crypto_stream))))
			}
		}
	}
}

enum DecryptInner<T: Read> {
	None(BufReader<T>),
	Decryptor(BufReader<Decryptor<T>>)
}

impl<T: Read> DecryptInner<T> {
	#[inline]
	fn finish(self) -> T {
		match self {
			DecryptInner::None(x) => x.into_inner(),
			DecryptInner::Decryptor(x) => x.into_inner().finish()
		}
	}
}

impl<T: Read> Read for DecryptInner<T> {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
		match self {
			DecryptInner::None(x) => x.read(buf),
			DecryptInner::Decryptor(x) => x.read(buf)
		}
	}
}

impl<T: Read> BufRead for DecryptInner<T> {
	#[inline]
	fn fill_buf(&mut self) -> std::io::Result<&[u8]> {
		match self {
			DecryptInner::None(x) => x.fill_buf(),
			DecryptInner::Decryptor(x) => x.fill_buf()
		}
	}

	#[inline]
	fn consume(&mut self, amt: usize) {
		match self {
			DecryptInner::None(x) => x.consume(amt),
			DecryptInner::Decryptor(x) => x.consume(amt)
		}
	}
}

pub struct Decrypt<T: Read>(DecryptInner<T>);

impl<T: Read> Decrypt<T> {
	#[inline]
	pub fn finish(self) -> T {
		self.0.finish()
	}
}

impl<T: Read> Read for Decrypt<T> {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
		self.0.read(buf)
	}
}

impl<T: Read> BufRead for Decrypt<T> {
	#[inline]
	fn fill_buf(&mut self) -> std::io::Result<&[u8]> {
		self.0.fill_buf()
	}

	#[inline]
	fn consume(&mut self, amt: usize) {
		self.0.consume(amt)
	}
}

#[derive(Debug, Error)]
pub enum CryptoError {
	#[error("Missing decryption key")]
	NoKey,
	#[error("Wrong key size ({0} bytes expected)")]
	WrongKeySize(usize),
	#[error("Wrong decryption key provided")]
	WrongKey,
	#[error("Generic Encryption/Decryption error: {0}")]
	Generic (
		#[source]
		#[from]
		ErrorStack
	),
}