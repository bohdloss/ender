use std::io;
use std::io::{Read, Write};
use cryptostream::read::Decryptor;
use cryptostream::write::Encryptor;
use openssl::error::ErrorStack;
use openssl::pkey::{Private, Public};
use openssl::rsa::{Padding, Rsa};
use openssl::symm::Cipher;
use parse_display::Display;
use thiserror::Error;
use crate::{Encode, Decode, EncodingResult, BinStream, EncodingError, Finish};
use crate::encryption::CryptoError::NoKey;

pub fn encode_with_encryption<T, F>(
	encoder: &mut BinStream<T>,
	encryption: Encryption,
	key: Option<&[u8]>,
	iv: Option<&[u8]>,
	f: F
) -> EncodingResult<()>
where T: Write, 
      F: FnOnce(&mut BinStream<Encrypt<&mut T>>) -> EncodingResult<()>
{
	let mut encoder = encoder.add_encryption(encryption, key, iv)?;
	let v = f(&mut encoder);
	encoder.finish()?.finish()?;
	v
}

pub fn decode_with_encryption<T, F, V>(
	decoder: &mut BinStream<T>,
	encryption: Encryption,
	key: Option<&[u8]>,
	iv: Option<&[u8]>,
	f: F
) -> EncodingResult<V>
	where T: Read,
	      F: FnOnce(&mut BinStream<Decrypt<&mut T>>) -> EncodingResult<V>, 
	      V: Decode
{
	let mut decoder = decoder.add_decryption(encryption, key, iv)?;
	let v = f(&mut decoder);
	decoder.finish()?.finish()?;
	v
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, Display)]
#[display("rsa = ({rsa}), symm = ({symm})")]
pub struct CryptoState {
	pub rsa: RsaState,
	pub symm: SymmState,
}

impl CryptoState {
	pub const fn new() -> Self {
		Self {
			rsa: RsaState::new(),
			symm: SymmState::new(),
		}
	}
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display, Encode, Decode)]
#[repr(u8)]
#[ende(variant: 8)]
#[allow(non_camel_case_types)]
pub enum RsaPadding {
	None,
	Pkcs1,
	Pkcs1Oaep,
	Pkcs1Pss
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display, Encode, Decode)]
#[repr(u8)]
#[ende(variant: 8)]
#[allow(non_camel_case_types)]
pub enum RsaBits {
	N1024,
	N2048,
	N4096,
}

impl RsaPadding {
	fn to_openssl_padding(&self) -> Padding {
		match self {
			RsaPadding::None => Padding::NONE,
			RsaPadding::Pkcs1 => Padding::PKCS1,
			RsaPadding::Pkcs1Oaep => Padding::PKCS1_OAEP,
			RsaPadding::Pkcs1Pss => Padding::PKCS1_PSS,
		}
	}
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display, Encode, Decode)]
#[repr(u8)]
#[ende(variant: 8)]
pub enum RsaMode {
	Normal,
	Reverse
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, Display)]
#[display("padding: {padding}, mode = {mode}")]
pub struct RsaState {
	key: Vec<u8>,
	pub padding: RsaPadding,
	pub mode: RsaMode,
}

impl RsaState {
	pub const fn new() -> Self {
		Self {
			key: Vec::new(),
			padding: RsaPadding::None,
			mode: RsaMode::Normal,
		}
	}
	
	pub fn store_key<T: AsRef<[u8]>>(&mut self, bytes: &T) {
		self.reset_key();
		for byte in bytes.as_ref() {
			self.key.push(*byte);
		}
	}

	pub fn reset_key(&mut self) {
		for byte in self.key.iter_mut() {
			*byte = 0;
		}
		self.key.clear();
	}
	
	pub fn get_key(&self) -> Option<&[u8]> {
		if self.key.is_empty() {
			Some(&self.key)
		} else {
			None
		}
	}
}

impl Drop for RsaState {
	fn drop(&mut self) {
		self.reset_key();
	}
}

#[derive(Clone, Eq, PartialEq, Hash, Debug, Display)]
#[display("encryption: {encryption}")]
pub struct SymmState {
	iv: Vec<u8>,
	key: Vec<u8>,
	pub encryption: Encryption
}

impl SymmState {
	pub const fn new() -> Self {
		Self {
			iv: Vec::new(),
			key: Vec::new(),
			encryption: Encryption::None
		}
	}

	pub fn store_iv<T: AsRef<[u8]>>(&mut self, bytes: &T) {
		self.reset_iv();
		for byte in bytes.as_ref() {
			self.iv.push(*byte);
		}
	}

	pub fn reset_iv(&mut self) {
		for byte in self.iv.iter_mut() {
			*byte = 0;
		}
		self.iv.clear();
	}

	pub fn get_iv(&self) -> Option<&[u8]> {
		if self.iv.is_empty() {
			Some(&self.iv)
		} else {
			None
		}
	}

	pub fn store_key<T: AsRef<[u8]>>(&mut self, bytes: &T) {
		self.reset_key();
		for byte in bytes.as_ref() {
			self.key.push(*byte);
		}
	}

	pub fn reset_key(&mut self) {
		for byte in self.key.iter_mut() {
			*byte = 0;
		}
		self.key.clear();
	}

	pub fn get_key(&self) -> Option<&[u8]> {
		if self.key.is_empty() {
			Some(&self.key)
		} else {
			None
		}
	}
}

impl Drop for SymmState {
	fn drop(&mut self) {
		self.reset_iv();
		self.reset_key();
	}
}

pub struct RsaBlock<const BLOCK_SIZE: usize>([u8; BLOCK_SIZE]);

fn rsa_encrypt<const BLOCK_SIZE: usize, T: Write>(encoder: &mut BinStream<T>, data: &[u8; BLOCK_SIZE]) -> EncodingResult<()> {
	let mut temp = [0u8; BLOCK_SIZE];
	match encoder.crypto.rsa.mode {
		RsaMode::Normal => {
			let rsa: Rsa<Public> = Rsa::public_key_from_der(encoder.crypto.rsa.get_key().ok_or(NoKey)?)
				.map_err(ese_to_ee)?;

			rsa.public_encrypt(data, &mut temp, encoder.crypto.rsa.padding.to_openssl_padding())
				.map_err(ese_to_ee)?;
		}
		RsaMode::Reverse => {
			let rsa: Rsa<Private> = Rsa::private_key_from_der(encoder.crypto.rsa.get_key().ok_or(NoKey)?)
				.map_err(ese_to_ee)?;

			rsa.private_encrypt(data, &mut temp, encoder.crypto.rsa.padding.to_openssl_padding())
				.map_err(ese_to_ee)?;
		}
	}
	encoder.write_raw_bytes(&temp)
}

fn rsa_decrypt<const BLOCK_SIZE: usize, T: Read>(decoder: &mut BinStream<T>) -> EncodingResult<[u8; BLOCK_SIZE]> {
	let mut temp = [0u8; BLOCK_SIZE];
	let mut data = [0u8; BLOCK_SIZE];
	decoder.read_raw_bytes(&mut temp)?;
	match decoder.crypto.rsa.mode {
		RsaMode::Normal => {
			let rsa: Rsa<Private> = Rsa::private_key_from_der(decoder.crypto.rsa.get_key().ok_or(NoKey)?)
				.map_err(ese_to_ee)?;

			rsa.private_decrypt(&temp, &mut data, decoder.crypto.rsa.padding.to_openssl_padding())
				.map_err(ese_to_ee)?;
		}
		RsaMode::Reverse => {
			let rsa: Rsa<Public> = Rsa::public_key_from_der(decoder.crypto.rsa.get_key().ok_or(NoKey)?)
				.map_err(ese_to_ee)?;

			rsa.public_decrypt(&temp, &mut data, decoder.crypto.rsa.padding.to_openssl_padding())
				.map_err(ese_to_ee)?;
		}
	}
	Ok(data)
}

impl Encode for RsaBlock<128> {
	fn encode<T: Write>(&self, encoder: &mut BinStream<T>) -> EncodingResult<()> {
		rsa_encrypt(encoder, &self.0)
	}
}

impl Decode for RsaBlock<128> {
	fn decode<T: Read>(decoder: &mut BinStream<T>) -> EncodingResult<Self> where Self: Sized {
		Ok(Self(rsa_decrypt(decoder)?))
	}
}

impl Encode for RsaBlock<256> {
	fn encode<T: Write>(&self, encoder: &mut BinStream<T>) -> EncodingResult<()> {
		rsa_encrypt(encoder, &self.0)
	}
}

impl Decode for RsaBlock<256> {
	fn decode<T: Read>(decoder: &mut BinStream<T>) -> EncodingResult<Self> where Self: Sized {
		Ok(Self(rsa_decrypt(decoder)?))
	}
}

impl Encode for RsaBlock<512> {
	fn encode<T: Write>(&self, encoder: &mut BinStream<T>) -> EncodingResult<()> {
		rsa_encrypt(encoder, &self.0)
	}
}

impl Decode for RsaBlock<512> {
	fn decode<T: Read>(decoder: &mut BinStream<T>) -> EncodingResult<Self> where Self: Sized {
		Ok(Self(rsa_decrypt(decoder)?))
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

impl CfbFeedback {
	pub fn bits(&self) -> u32 {
		match self {
			CfbFeedback::N1 => 1,
			CfbFeedback::N8 => 8,
			CfbFeedback::N128 => 128,
		}
	}
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display, Encode, Decode)]
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

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display, Encode, Decode)]
#[ende(variant: 8)]
pub enum Encryption {
	#[display("no encryption")]
	None,
	#[display("{0}-bit AES/{1}")]
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

impl<T: Write> Finish for EncryptInner<T> {
	type Output = T;

	#[inline]
	fn finish(self) -> EncodingResult<T> {
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

impl<T: Write> Finish for Encrypt<T> {
	type Output = T;
	#[inline]
	fn finish(self) -> EncodingResult<T> {
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

impl<T: Read> Finish for DecryptInner<T> {
	type Output = T;
	#[inline]
	fn finish(self) -> EncodingResult<T> {
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

impl<T: Read> Finish for Decrypt<T> {
	type Output = T;
	#[inline]
	fn finish(self) -> EncodingResult<T> {
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

/// Error Stack Error to Encoding Error
fn ese_to_ee(x: ErrorStack) -> EncodingError {
	EncodingError::EncryptionError(CryptoError::Generic(x))
}