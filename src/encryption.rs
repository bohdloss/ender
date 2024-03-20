use std::fmt::{Debug, Formatter};
use std::io;
use std::io::{Read, Write};
use std::ops::{Deref, DerefMut};
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
	
	pub fn with_rsa_key<T: AsRef<[u8]>>(key: &T) -> Self {
		let mut self_ = Self::new();
		self_.rsa.store_key(key);
		self_
	}

	pub fn with_symm_iv_and_key<T: AsRef<[u8]>, F: AsRef<[u8]>>(key: &T, iv: &F) -> Self {
		let mut self_ = Self::new();
		self_.symm.store_key(key);
		self_.symm.store_iv(iv);
		self_
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
	#[display("1024")]
	N1024,
	#[display("2048")]
	N2048,
	#[display("4096")]
	N4096,
}

impl RsaBits {
	pub fn bits(&self) -> u32 {
		match self {
			RsaBits::N1024 => 1024,
			RsaBits::N2048 => 2048,
			RsaBits::N4096 => 4096,
		}
	}
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
	pub bits: RsaBits,
	pub padding: RsaPadding,
	pub mode: RsaMode,
}

impl RsaState {
	pub const fn new() -> Self {
		Self {
			key: Vec::new(),
			bits: RsaBits::N2048,
			padding: RsaPadding::Pkcs1,
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
			None
		} else {
			Some(&self.key)
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
			None
		} else {
			Some(&self.iv)
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
			None
		} else {
			Some(&self.key)
		}
	}
}

impl Drop for SymmState {
	fn drop(&mut self) {
		self.reset_iv();
		self.reset_key();
	}
}

pub struct RsaBlock(pub Vec<u8>);

impl Deref for RsaBlock {
	type Target = [u8];
	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl DerefMut for RsaBlock {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}

impl Debug for RsaBlock {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}-bit RSA block", self.0.len() * 8)
	}
}

fn rsa_encrypt<T: Write>(encoder: &mut BinStream<T>, data: &[u8]) -> EncodingResult<()> {
	let expected_bytes = (encoder.crypto.rsa.bits.bits() / 8) as usize;
	let mut temp = vec![0u8; data.len()];
	match encoder.crypto.rsa.mode {
		RsaMode::Normal => {
			let rsa: Rsa<Public> = Rsa::public_key_from_der(encoder.crypto.rsa.get_key().ok_or(NoKey)?)
				.map_err(ese_to_ee)?;
			let key_length = rsa.n().num_bytes() as usize;
			if key_length != expected_bytes || data.len() != expected_bytes {
				return Err(EncodingError::EncryptionError(CryptoError::WrongKeySize(key_length)))
			}

			rsa.public_encrypt(data, &mut temp, encoder.crypto.rsa.padding.to_openssl_padding())
				.map_err(ese_to_ee)?;
		}
		RsaMode::Reverse => {
			let rsa: Rsa<Private> = Rsa::private_key_from_der(encoder.crypto.rsa.get_key().ok_or(NoKey)?)
				.map_err(ese_to_ee)?;
			let key_length = rsa.n().num_bytes() as usize;
			if key_length != expected_bytes || data.len() != expected_bytes {
				return Err(EncodingError::EncryptionError(CryptoError::WrongKeySize(key_length)))
			}

			rsa.private_encrypt(data, &mut temp, encoder.crypto.rsa.padding.to_openssl_padding())
				.map_err(ese_to_ee)?;
		}
	}
	
	// Our custom flatten condition is handled just fine
	// by the default encode implementation
	data.encode(encoder)
}

fn rsa_decrypt<T: Read>(decoder: &mut BinStream<T>) -> EncodingResult<Vec<u8>> {
	let expected_bytes = (decoder.crypto.rsa.bits.bits() / 8) as usize;
	let temp: Vec<u8> = {
		// Flatten behaves differently here:
		// we ignore its value and use the number of rsa bits if it is present
		if decoder.options.flatten != 0 {
			decoder.options.flatten = expected_bytes;
		}
		
		Vec::decode(decoder)?
	};
	let mut data = vec![0u8; temp.len()];
	match decoder.crypto.rsa.mode {
		RsaMode::Normal => {
			let rsa: Rsa<Private> = Rsa::private_key_from_der(decoder.crypto.rsa.get_key().ok_or(NoKey)?)
				.map_err(ese_to_ee)?;
			let key_length = rsa.n().num_bytes() as usize;
			if key_length != expected_bytes || data.len() != expected_bytes {
				return Err(EncodingError::EncryptionError(CryptoError::WrongKeySize(key_length)))
			}

			rsa.private_decrypt(&temp, &mut data, decoder.crypto.rsa.padding.to_openssl_padding())
				.map_err(ese_to_ee)?;
		}
		RsaMode::Reverse => {
			let rsa: Rsa<Public> = Rsa::public_key_from_der(decoder.crypto.rsa.get_key().ok_or(NoKey)?)
				.map_err(ese_to_ee)?;
			let key_length = rsa.n().num_bytes() as usize;
			if key_length != expected_bytes || data.len() != expected_bytes {
				return Err(EncodingError::EncryptionError(CryptoError::WrongKeySize(key_length)))
			}

			rsa.public_decrypt(&temp, &mut data, decoder.crypto.rsa.padding.to_openssl_padding())
				.map_err(ese_to_ee)?;
		}
	}
	Ok(data)
}

impl Encode for RsaBlock {
	fn encode<T: Write>(&self, encoder: &mut BinStream<T>) -> EncodingResult<()> {
		rsa_encrypt(encoder, &self.0)
	}
}

impl Decode for RsaBlock {
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

				let crypto_stream = Encryptor::new(input, cipher, &key, iv.unwrap_or(&[]))
					.map_err(|_| CryptoError::Generic)?;
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

				let crypto_stream = Decryptor::new(input, cipher, &key, iv.unwrap_or(&[]))
					.map_err(|_| CryptoError::Generic)?;
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
	#[error("Generic Encryption/Decryption error")]
	Generic,
}

/// Error Stack Error to Encoding Error
fn ese_to_ee(_: ErrorStack) -> EncodingError {
	EncodingError::EncryptionError(CryptoError::Generic)
}