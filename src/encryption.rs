use std::io;
use std::io::{Read, Write};
use cryptostream::read::Decryptor;
use cryptostream::write::Encryptor;
use openssl::error::ErrorStack;
use openssl::symm::Cipher;
use parse_display::Display;
use thiserror::Error;
use crate::{Encode, Decode, EncodingResult, BinStream};

pub struct RsaBlock<const BLOCK_SIZE: usize>([u8; BLOCK_SIZE]);

impl Encode for RsaBlock<128> {
	fn encode<T: Write>(&self, _encoder: &mut BinStream<T>) -> EncodingResult<()> {
		todo!()
	}
}

impl Decode for RsaBlock<128> {
	fn decode<T: Read>(_decoder: &mut BinStream<T>) -> EncodingResult<Self> where Self: Sized {
		todo!()
	}
}

impl Encode for RsaBlock<256> {
	fn encode<T: Write>(&self, _encoder: &mut BinStream<T>) -> EncodingResult<()> {
		todo!()
	}
}

impl Decode for RsaBlock<256> {
	fn decode<T: Read>(_decoder: &mut BinStream<T>) -> EncodingResult<Self> where Self: Sized {
		todo!()
	}
}

impl Encode for RsaBlock<512> {
	fn encode<T: Write>(&self, _encoder: &mut BinStream<T>) -> EncodingResult<()> {
		todo!()
	}
}

impl Decode for RsaBlock<512> {
	fn decode<T: Read>(_decoder: &mut BinStream<T>) -> EncodingResult<Self> where Self: Sized {
		todo!()
	}
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Display, Encode, Decode)]
#[repr(u8)]
#[ende(variant: 8)]
pub enum AesBits {
	#[display("128")]
	N128,
	#[display("192")]
	N192,
	#[display("256")]
	N256
}

impl AesBits {
	pub fn bits(&self) -> u32 {
		match self {
			AesBits::N128 => 128,
			AesBits::N192 => 129,
			AesBits::N256 => 256,
		}
	}
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Display, Encode, Decode)]
#[repr(u8)]
#[ende(variant: 8)]
pub enum CfbFeedback {
	#[display("1")]
	N1,
	#[display("8")]
	N8,
	#[display("128")]
	N128
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Display, Encode, Decode)]
#[repr(u8)]
#[ende(variant: 8)]
pub enum AesMode {
	#[display("ECB")]
	Ecb,
	#[display("CBC")]
	Cbc,
	#[display("CFB{0}")]
	Cfb(CfbFeedback),
	#[display("OFB")]
	Ofb,
	#[display("CTR")]
	Ctr
}

impl AesMode {
	pub fn is_ecb(&self) -> bool {
		match self {
			AesMode::Ecb => true,
			_ => false
		}
	}

	pub fn is_cbc(&self) -> bool {
		match self {
			AesMode::Cbc => true,
			_ => false
		}
	}

	pub fn is_cfb(&self) -> bool {
		match self {
			AesMode::Cfb(..) => true,
			_ => false
		}
	}

	pub fn is_ofb(&self) -> bool {
		match self {
			AesMode::Ofb => true,
			_ => false
		}
	}

	pub fn is_ctr(&self) -> bool {
		match self {
			AesMode::Ctr => true,
			_ => false
		}
	}
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Display, Encode, Decode)]
#[ende(variant: 8)]
pub enum Encryption {
	#[display("no encryption")]
	None,
	#[display("{0}-bit key AES/{1}")]
	Aes(AesBits, AesMode)
}

impl Encryption {
	pub fn is_none(&self) -> bool {
		match self {
			Encryption::None => true,
			_ => false
		}
	}

	pub fn is_aes(&self) -> bool {
		match self {
			Encryption::Aes(..) => true,
			_ => false
		}
	}
}

impl Encryption {
	fn cipher(&self) -> Option<Cipher> {
		match self {
			Encryption::None => None,
			Encryption::Aes(bits, mode) => Some(match mode {
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
				AesMode::Cfb(cfb_bits) => match cfb_bits {
					CfbFeedback::N1 => match bits {
						AesBits::N128 => Cipher::aes_128_cfb1(),
						AesBits::N192 => Cipher::aes_192_cfb1(),
						AesBits::N256 => Cipher::aes_256_cfb1()
					},
					CfbFeedback::N8 => match bits {
						AesBits::N128 => Cipher::aes_128_cfb8(),
						AesBits::N192 => Cipher::aes_192_cfb8(),
						AesBits::N256 => Cipher::aes_256_cfb8()
					},
					CfbFeedback::N128 => match bits {
						AesBits::N128 => Cipher::aes_128_cfb128(),
						AesBits::N192 => Cipher::aes_192_cfb128(),
						AesBits::N256 => Cipher::aes_256_cfb128()
					}
				}
				AesMode::Ofb => match bits {
					AesBits::N128 => Cipher::aes_128_ofb(),
					AesBits::N192 => Cipher::aes_192_ofb(),
					AesBits::N256 => Cipher::aes_256_ofb()
				}
				AesMode::Ctr => match bits {
					AesBits::N128 => Cipher::aes_128_ctr(),
					AesBits::N192 => Cipher::aes_192_ctr(),
					AesBits::N256 => Cipher::aes_256_ctr()
				}
			})
		}
	}

	pub fn encrypt<T: Write>(&self, input: T, key: Option<&[u8]>, iv: Option<&[u8]>) -> Result<Encrypt<T>, CryptoError> {
		match self {
			Encryption::None => {
				Ok(Encrypt(EncryptInner::None(input)))
			}
			_ => {
				let key = key.ok_or(CryptoError::NoKey)?;
				let cipher = self.cipher().unwrap();

				if cipher.key_len() != key.len() {
					return Err(CryptoError::WrongKeySize(cipher.key_len()));
				}

				if let Some(iv_len) = cipher.iv_len() {
					let iv = iv.ok_or(CryptoError::NoIV)?;
					if iv.len() != iv_len {
						return Err(CryptoError::WrongIvSize(iv_len));
					}
				}

				let crypto_stream = Encryptor::new(input, cipher, &key, iv.unwrap_or(&[]))?;
				Ok(Encrypt(EncryptInner::Cryptor(crypto_stream)))
			}
		}
	}

	pub fn decrypt<T: Read>(&self, input: T, key: Option<&[u8]>, iv: Option<&[u8]>) -> Result<Decrypt<T>, CryptoError> {
		match self {
			Encryption::None => {
				Ok(Decrypt(DecryptInner::None(input)))
			}
			_ => {
				let key = key.ok_or(CryptoError::NoKey)?;
				let cipher = self.cipher().unwrap();

				if cipher.key_len() != key.len() {
					return Err(CryptoError::WrongKeySize(cipher.key_len()));
				}

				if let Some(iv_len) = cipher.iv_len() {
					let iv = iv.ok_or(CryptoError::NoIV)?;
					if iv.len() != iv_len {
						return Err(CryptoError::WrongIvSize(iv_len));
					}
				}

				let crypto_stream = Decryptor::new(input, cipher, &key, iv.unwrap_or(&[]))?;
				Ok(Decrypt(DecryptInner::Decryptor(crypto_stream)))
			}
		}
	}
}

enum EncryptInner<T: Write> {
	None(T),
	Cryptor(Encryptor<T>)
}

impl<T: Write> EncryptInner<T> {
	#[inline]
	fn finish(self) -> Result<T, CryptoError> {
		match self {
			EncryptInner::None(x) => Ok(x),
			EncryptInner::Cryptor(x) => Ok(x.finish()?)
		}
	}
}

impl<T: Write> Write for EncryptInner<T> {
	#[inline]
	fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
		match self {
			EncryptInner::None(x) => x.write(buf),
			EncryptInner::Cryptor(x) => x.write(buf)
		}
	}
	#[inline]
	fn flush(&mut self) -> std::io::Result<()> {
		match self {
			EncryptInner::None(x) => x.flush(),
			EncryptInner::Cryptor(x) => x.flush()
		}
	}
}

#[repr(transparent)]
pub struct Encrypt<T: Write>(EncryptInner<T>);

impl<T: Write> Encrypt<T> {
	#[inline]
	pub fn finish(self) -> Result<T, CryptoError> {
		self.0.finish()
	}
}

impl<T: Write> Write for Encrypt<T> {
	#[inline]
	fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
		self.0.write(buf)
	}
	#[inline]
	fn flush(&mut self) -> std::io::Result<()> {
		self.0.flush()
	}
}

enum DecryptInner<T: Read> {
	None(T),
	Decryptor(Decryptor<T>)
}

impl<T: Read> DecryptInner<T> {
	#[inline]
	fn finish(self) -> Result<T, CryptoError> {
		match self {
			DecryptInner::None(x) => Ok(x),
			DecryptInner::Decryptor(x) => Ok(x.finish()),
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

#[repr(transparent)]
pub struct Decrypt<T: Read>(DecryptInner<T>);

impl<T: Read> Decrypt<T> {
	#[inline]
	pub fn finish(self) -> Result<T, CryptoError> {
		self.0.finish()
	}
}

impl<T: Read> Read for Decrypt<T> {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
		self.0.read(buf)
	}
}

#[derive(Debug, Error)]
pub enum CryptoError {
	#[error("IO Error occurred: {0}")]
	IOError(
		#[source]
		#[from]
		io::Error
	),
	#[error("Missing encryption/decryption key")]
	NoKey,
	#[error("Missing initialization vector")]
	NoIV,
	#[error("Wrong key size ({0} bytes expected)")]
	WrongKeySize(usize),
	#[error("Wrong IV size ({0} bytes expected)")]
	WrongIvSize(usize),
	#[error("Wrong decryption key provided")]
	WrongKey,
	#[error("Generic Encryption/Decryption error: {0}")]
	Generic (
		#[source]
		#[from]
		ErrorStack
	),
}