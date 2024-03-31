use std::borrow::Cow;
use std::io::{Read, Write};
use std::str::FromStr;
use parse_display::Display;
use openssl::symm::Cipher;
use zeroize::Zeroize;
use crate::{Decode, Encode, Encoder, EncodingError, EncodingResult, Finish};
use crate::encryption::{Decrypt, Encrypt};

/// Function for convenience.<br>
/// It calls [`Encoder::add_encryption`] on the encoder with the given encryption,
/// key and iv parameters, calls the closure with the transformed encoder,
/// then finalizes the encryptor before returning
pub fn encode_with_encryption<T, F>(
	encoder: &mut Encoder<T>,
	encryption: Option<SymmEncryption>,
	key: Option<&[u8]>,
	iv: Option<&[u8]>,
	f: F
) -> EncodingResult<()>
	where T: Write,
	      F: FnOnce(&mut Encoder<Encrypt<&mut T>>) -> EncodingResult<()>
{
	let mut encoder = encoder.add_encryption(encryption, key, iv)?;
	let v = f(&mut encoder);
	encoder.finish()?.0.finish()?;
	v
}

/// Function for convenience.<br>
/// It calls [`Encoder::add_decryption`] on the decoder with the given encryption,
/// key and iv parameters, calls the closure with the transformed decoder,
/// then finalizes the decryptor before returning
pub fn decode_with_encryption<T, F, V>(
	decoder: &mut Encoder<T>,
	encryption: Option<SymmEncryption>,
	key: Option<&[u8]>,
	iv: Option<&[u8]>,
	f: F
) -> EncodingResult<V>
	where T: Read,
	      F: FnOnce(&mut Encoder<Decrypt<&mut T>>) -> EncodingResult<V>,
	      V: crate::Decode
{
	let mut decoder = decoder.add_decryption(encryption, key, iv)?;
	let v = f(&mut decoder);
	decoder.finish()?.0.finish()?;
	v
}

/// The state of symmetric encryption/decryption.
/// Contains they key, only accessible through [`SymmState::get_key`],
/// the iv, only accessible through [`SymmState::get_iv`],
/// and the encryption mode
#[derive(Clone, Eq, PartialEq, Debug, Display)]
#[display("encryption: {encryption}")]
pub struct SymmState<'a> {
	key: Cow<'a, [u8]>,
	iv: Cow<'a, [u8]>,
	pub encryption: SymmEncryption
}

impl<'a> SymmState<'a> {
	/// Constructs a new symmetric encryption state, without storing any key or iv, with
	/// the encryption algorithm set to None
	pub const fn new() -> Self {
		Self {
			key: Cow::Borrowed(&[]),
			iv: Cow::Borrowed(&[]),
			encryption: SymmEncryption::None
		}
	}

	/// Constructs a new symmetric encryption state, storing the given key and iv, with
	/// the encryption algorithm set to None
	pub fn with_key_and_iv<T: AsRef<[u8]>, U: AsRef<[u8]>>(key: &'a T, iv: &'a U) -> Self {
		Self {
			key: Cow::Borrowed(key.as_ref()),
			iv: Cow::Borrowed(iv.as_ref()),
			encryption: SymmEncryption::None,
		}
	}
	
	/// Similar to clone, but hints that the key and iv being stored should not be cloned to
	/// a new memory location, but simply borrowed
	pub fn borrow_clone(&self) -> SymmState {
		SymmState {
			iv: Cow::Borrowed(&self.iv),
			key: Cow::Borrowed(&self.key),
			encryption: self.encryption
		}
	}

	/// Stores the given key, discarding the previous one.
	/// If the key length is 0, the old key is still discarded but no value is stored
	pub fn store_key<T: AsRef<[u8]>>(&mut self, bytes: &T) {
		self.reset_key();
		if bytes.as_ref().len() == 0 { return; }
		let key = self.key.to_mut();
		key.reserve(bytes.as_ref().len());
		key.extend(bytes.as_ref());
	}

	/// Discards the previously stored key, if any.
	pub fn reset_key(&mut self) {
		if let Cow::Owned(ref mut key) = self.key {
			key.zeroize();
		} else {
			self.key = Cow::Borrowed(&[]);
		}
	}

	/// Returns the stored key, or None if no key is stored
	pub fn get_key(&self) -> Option<&[u8]> {
		(self.key.len() > 0).then_some(&self.key)
	}

	/// Stores the given iv, discarding the previous one.
	/// If the iv length is 0, the old iv is still discarded but no value is stored
	pub fn store_iv<T: AsRef<[u8]>>(&mut self, bytes: &T) {
		self.reset_iv();
		if bytes.as_ref().len() == 0 { return; }
		let iv = self.iv.to_mut();
		iv.reserve(bytes.as_ref().len());
		iv.extend(bytes.as_ref());
	}

	/// Discards the previously stored iv, if any.
	pub fn reset_iv(&mut self) {
		if let Cow::Owned(ref mut iv) = self.iv {
			iv.zeroize();
		} else {
			self.iv = Cow::Borrowed(&[]);
		}
	}

	/// Returns the stored iv, or None if no iv is stored
	pub fn get_iv(&self) -> Option<&[u8]> {
		(self.iv.len() > 0).then_some(&self.iv)
	}
}

impl Drop for SymmState<'_> {
	fn drop(&mut self) {
		self.reset_key();
		self.reset_iv();
	}
}

/// Encryption algorithm, or None to indicate absence of encryption.
/// Can be used to wrap a type implementing Write/Read in order to provide Encryption/Decryption
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
pub enum SymmEncryption {
	#[display("no encryption")]
	None,
	#[display("{0}-bit AES/{1}")]
	Aes(AesBits, AesMode)
}

impl FromStr for SymmEncryption {
	type Err = &'static str;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		const USAGE: &str = r#"Invalid encryption format. Usage: "{key_size}-bit {cipher}/{mode}/{padding}""#;

		if s == "None" {
			return Ok(SymmEncryption::None);
		}
		
		let (key_size, rest) = s.split_once("-").ok_or(USAGE)?;
		let (bit_token, rest) = rest.split_once(" ").ok_or(USAGE)?;
		let (cipher, rest) = rest.split_once("/").ok_or(USAGE)?;
		let (mode, padding) = rest.split_once("/").unwrap_or((rest, ""));

		if bit_token != "bit" {
			return Err(USAGE);
		}

		Ok(match cipher {
			"AES" => {
				let bits = match key_size {
					"128" => AesBits::N128,
					"192" => AesBits::N192,
					"256" => AesBits::N256,
					_ => return Err(r#"Allowed key sizes for AES are: 128, 192, 256"#)
				};

				let mode = if mode.starts_with("CFB") {
					let cfb_feedback = match &mode[3..] {
						"1" => CfbFeedback::N1,
						"8" => CfbFeedback::N8,
						"128" => CfbFeedback::N128,
						_ => return Err(r#"Allowed CFB feedback sizes are: 1, 8, 128"#)
					};
					AesMode::Cfb(cfb_feedback)
				} else {
					match mode {
						"ECB" => AesMode::Ecb,
						"CBC" => AesMode::Cbc,
						"OFB" => AesMode::Ofb,
						"CTR" => AesMode::Ctr,
						_ => return Err(r#"Allowed modes for AES are: ECB, CBC, CFB, OFB, CTR"#)
					}
				};

				let _padding = match padding {
					"" => {},
					_ => return Err(r#"Allowed padding modes for AES are: <empty>"#)
				};

				SymmEncryption::Aes(bits, mode)
			},
			
			_ => return Err(r#"Allowed ciphers are: AES, RSA, revRSA"#)
		})
	}
}

impl Encode for SymmEncryption {
	fn encode<T: Write>(&self, encoder: &mut Encoder<T>) -> EncodingResult<()> {
		match self {
			Self::None => encoder.write_u8(0)?,
			Self::Aes(m0, m1) => {
				encoder.write_u8(1)?;
				m0.encode(encoder)?;
				m1.encode(encoder)?;
			},
		}
		Ok(())
	}
}

impl Decode for SymmEncryption {
	fn decode<T: Read>(decoder: &mut Encoder<T>) -> EncodingResult<Self> {
		Ok(match decoder.read_u8()? {
			0 => Self::None,
			1 => Self::Aes(
				Decode::decode(decoder)?,
				Decode::decode(decoder)?,
			),
			_ => return Err(EncodingError::InvalidVariant),
		})
	}
}

impl SymmEncryption {
	/// Returns the key length in bytes for the encryption algorithm represented by `self`.
	/// Always returns 0 if `self` is set to None
	pub fn key_len(&self) -> usize {
		self.cipher().map(|cipher| cipher.key_len()).unwrap_or(0)
	}

	/// Returns the iv length in bytes for the encryption algorithm represented by `self`.
	/// Always returns 0 if `self` is set to None
	pub fn iv_len(&self) -> usize {
		self.cipher().and_then(|cipher| cipher.iv_len()).unwrap_or(0)
	}

	/// Returns the block size in bytes for the encryption algorithm represented by `self`.
	/// Always returns 1 if `self` is set to None
	pub fn block_size(&self) -> usize {
		self.cipher().map(|cipher| cipher.block_size()).unwrap_or(1)
	}
}

impl SymmEncryption {
	/// Returns true if `self` is None
	pub fn is_none(&self) -> bool {
		match self {
			SymmEncryption::None => true,
			_ => false
		}
	}

	/// Returns true if `self` is Aes
	pub fn is_aes(&self) -> bool {
		match self {
			SymmEncryption::Aes(..) => true,
			_ => false
		}
	}
}

impl SymmEncryption {
	// Internal convenience function
	pub(super) fn cipher(&self) -> Option<Cipher> {
		match self {
			SymmEncryption::None => None,
			SymmEncryption::Aes(bits, mode) => Some(match mode {
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
}

/// The number of bits of an AES key
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Display)]
pub enum AesBits {
	#[display("128")]
	N128,
	#[display("192")]
	N192,
	#[display("256")]
	N256
}

impl Encode for AesBits {
	fn encode<T: Write>(&self, encoder: &mut Encoder<T>) -> EncodingResult<()> {
		match self {
			Self::N128 => encoder.write_u8(0),
			Self::N192 => encoder.write_u8(1),
			Self::N256 => encoder.write_u8(2),
		}
	}
}

impl Decode for AesBits {
	fn decode<T: Read>(decoder: &mut Encoder<T>) -> EncodingResult<Self> {
		Ok(match decoder.read_u8()? {
			0 => Self::N128,
			1 => Self::N192,
			2 => Self::N256,
			_ => return Err(EncodingError::InvalidVariant),
		})
	}
}

impl AesBits {
	/// Returns the number of AES key length bits `self` represents
	pub fn bits(&self) -> u32 {
		match self {
			AesBits::N128 => 128,
			AesBits::N192 => 129,
			AesBits::N256 => 256,
		}
	}
}

/// Number of feedback bits used in CFB mode
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Display)]
pub enum CfbFeedback {
	#[display("1")]
	N1,
	#[display("8")]
	N8,
	#[display("128")]
	N128
}

impl Encode for CfbFeedback {
	fn encode<T: Write>(&self, encoder: &mut Encoder<T>) -> EncodingResult<()> {
		match self {
			Self::N1 => encoder.write_u8(0),
			Self::N8 => encoder.write_u8(1),
			Self::N128 => encoder.write_u8(2),
		}
	}
}

impl Decode for CfbFeedback {
	fn decode<T: Read>(decoder: &mut Encoder<T>) -> EncodingResult<Self> {
		Ok(match decoder.read_u8()? {
			0 => Self::N1,
			1 => Self::N8,
			2 => Self::N128,
			_ => return Err(EncodingError::InvalidVariant),
		})
	}
}

impl CfbFeedback {
	/// Returns the number of Cfb feedback bits `self` represents
	pub fn bits(&self) -> u32 {
		match self {
			CfbFeedback::N1 => 1,
			CfbFeedback::N8 => 8,
			CfbFeedback::N128 => 128,
		}
	}
}

/// AES operation mode
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display)]
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

impl Encode for AesMode {
	fn encode<T: Write>(&self, encoder: &mut Encoder<T>) -> EncodingResult<()> {
		match self {
			Self::Ecb => encoder.write_u8(0)?,
			Self::Cbc => encoder.write_u8(1)?,
			Self::Cfb(m0) =>  {
				encoder.write_u8(2)?;
				m0.encode(encoder)?;
			},
			Self::Ofb => encoder.write_u8(3)?,
			Self::Ctr => encoder.write_u8(4)?,
		}
		Ok(())
	}
}

impl Decode for AesMode {
	fn decode<T: Read>(decoder: &mut Encoder<T>) -> EncodingResult<Self> {
		Ok(match decoder.read_u8()? {
			0 => Self::Ecb,
			1 => Self::Cbc,
			2 => Self::Cfb(
				Decode::decode(decoder)?	
			),
			3 => Self::Ofb,
			4 => Self::Ctr,
			_ => return Err(EncodingError::InvalidVariant),
		})
	}
}

impl AesMode {
	/// Returns true if `self` is Ecb
	pub fn is_ecb(&self) -> bool {
		match self {
			AesMode::Ecb => true,
			_ => false
		}
	}

	/// Returns true if `self` is Cbc
	pub fn is_cbc(&self) -> bool {
		match self {
			AesMode::Cbc => true,
			_ => false
		}
	}

	/// Returns true if `self` is any variation of Cfb
	pub fn is_cfb(&self) -> bool {
		match self {
			AesMode::Cfb(..) => true,
			_ => false
		}
	}

	/// Returns true if `self` is Ofb
	pub fn is_ofb(&self) -> bool {
		match self {
			AesMode::Ofb => true,
			_ => false
		}
	}

	/// Returns true if `self` is Ctr
	pub fn is_ctr(&self) -> bool {
		match self {
			AesMode::Ctr => true,
			_ => false
		}
	}
}