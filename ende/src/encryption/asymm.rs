use std::borrow::Cow;
use parse_display::Display;
use std::fmt::Debug;
use std::io::{Read, Write};
use std::str::FromStr;
use openssl::rsa::{Padding, Rsa};
use openssl::pkey::{Private, Public};
use zeroize::Zeroize;
use crate::{Encoder, EncodingError, EncodingResult, encryption};
use crate::encryption::CryptoError;
use crate::encryption::CryptoError::NoKey;

/// Function for convenience.<br>
/// It calls [`Encoder::add_encryption`] on the encoder with the given encryption,
/// key and iv parameters, calls the closure with the transformed encoder,
/// then finalizes the encryptor before returning
pub fn encode_asymm_block<T: Write>(
	encoder: &mut Encoder<T>,
	encryption: Option<AsymmEncryption>,
	public_key: Option<&[u8]>,
	private_key: Option<&[u8]>,
	data: &[u8],
) -> EncodingResult<()> {
	let encryption = encryption.unwrap_or(encoder.ctxt.crypto.asymm.encryption);

	match encryption {
		AsymmEncryption::Rsa(bits, padding, mode) => {
			rsa_encrypt(encoder, public_key, private_key, bits, padding, mode, data)
		}
	}
}

/// Function for convenience.<br>
/// It calls [`Encoder::add_encryption`] on the encoder with the given encryption,
/// key and iv parameters, calls the closure with the transformed encoder,
/// then finalizes the encryptor before returning
pub fn decode_asymm_block<T: Read>(
	encoder: &mut Encoder<T>,
	encryption: Option<AsymmEncryption>,
	public_key: Option<&[u8]>,
	private_key: Option<&[u8]>,
) -> EncodingResult<Vec<u8>> {
	let encryption = encryption.unwrap_or(encoder.ctxt.crypto.asymm.encryption);

	match encryption {
		AsymmEncryption::Rsa(bits, padding, mode) => {
			rsa_decrypt(encoder, public_key, private_key, bits, padding, mode)
		}
	}
}

/// The state of asymmetric encryption/decryption.
/// Contains the public key, only accessible through [`AsymmState::get_public`],
/// the private key, only accessible through [`AsymmState::get_private`],
/// and the encryption mode
#[derive(Clone, Eq, PartialEq, Debug, Display)]
#[display("encryption = {encryption}")]
pub struct AsymmState<'a> {
	public_key: Cow<'a, [u8]>,
	private_key: Cow<'a, [u8]>,
	pub encryption: AsymmEncryption,
}

impl<'a> AsymmState<'a> {
	/// Constructs a new asymmetric encryption state, without storing any public or private key,
	/// with the encryption algorithm set to "2048-bit RSA/ECB/PKCS1"
	pub const fn new() -> Self {
		Self {
			public_key: Cow::Borrowed(&[]),
			private_key: Cow::Borrowed(&[]),
			encryption: AsymmEncryption::Rsa(RsaBits::N2048, RsaPadding::Pkcs1, RsaMode::Standard),
		}
	}

	/// Constructs a new asymmetric encryption state, storing references to the given keys,
	/// with the encryption algorithm set to "2048-bit RSA/ECB/PKCS1"
	pub fn with_keys<T: AsRef<[u8]>, U: AsRef<[u8]>>(public: &'a T, private: &'a U) -> Self {
		Self {
			public_key: Cow::Borrowed(public.as_ref()),
			private_key: Cow::Borrowed(private.as_ref()),
			encryption: AsymmEncryption::Rsa(RsaBits::N2048, RsaPadding::Pkcs1, RsaMode::Standard),
		}
	}

	/// Similar to clone, but hints that the keys being stored should not be cloned to a new memory
	/// location, but simply borrowed
	pub fn borrow_clone(&self) -> AsymmState {
		AsymmState {
			public_key: Cow::Borrowed(&self.public_key),
			private_key: Cow::Borrowed(&self.private_key),
			encryption: self.encryption
		}
	}

	/// Stores the given public key, discarding the previous one.
	/// If the key length is 0, the old key is still discarded but no value is stored
	pub fn store_public<T: AsRef<[u8]>>(&mut self, bytes: &T) {
		self.reset_public();
		if bytes.as_ref().len() == 0 { return; }
		let key = self.public_key.to_mut();
		key.reserve(bytes.as_ref().len());
		key.extend(bytes.as_ref());
	}

	/// Discards the previously stored public key, if any.
	pub fn reset_public(&mut self) {
		if let Cow::Owned(ref mut key) = self.public_key {
			key.zeroize();
		} else {
			self.public_key = Cow::Borrowed(&[]);
		}
	}

	/// Returns the stored public key, or None if no key is stored
	pub fn get_public(&self) -> Option<&[u8]> {
		(self.public_key.len() > 0).then_some(&self.public_key)
	}

	/// Stores the given private key, discarding the previous one.
	/// If the key length is 0, the old key is still discarded but no value is stored
	pub fn store_private<T: AsRef<[u8]>>(&mut self, bytes: &T) {
		self.reset_private();
		if bytes.as_ref().len() == 0 { return; }
		let key = self.private_key.to_mut();
		key.reserve(bytes.as_ref().len());
		key.extend(bytes.as_ref());
	}

	/// Discards the previously stored private key, if any.
	pub fn reset_private(&mut self) {
		if let Cow::Owned(ref mut key) = self.private_key {
			key.zeroize();
		} else {
			self.private_key = Cow::Borrowed(&[]);
		}
	}

	/// Returns the stored private key, or None if no key is stored
	pub fn get_private(&self) -> Option<&[u8]> {
		(self.private_key.len() > 0).then_some(&self.private_key)
	}
}

impl<'a> Drop for AsymmState<'a> {
	fn drop(&mut self) {
		self.reset_public();
		self.reset_private();
	}
}

/// A public-key encryption algorithm. Contrary to [`SymmEncryption`], these cannot be used
/// as stream ciphers, and therefore only operate on blocks rather than `Write`'s and `Read`'s
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display, ende_derive::Encode, ende_derive::Decode)]
#[ende(variant: fixed, 8)]
pub enum AsymmEncryption {
	#[display("{0}-bits {2} RSA/ECB/{1}")]
	Rsa(RsaBits, RsaPadding, RsaMode)
}

impl FromStr for AsymmEncryption {
	type Err = &'static str;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		const USAGE: &str = r#"Invalid encryption format. Usage: "{key_size}-bit {cipher}/{mode}/{padding}""#;

		let (key_size, rest) = s.split_once("-").ok_or(USAGE)?;
		let (bit_token, rest) = rest.split_once(" ").ok_or(USAGE)?;
		let (cipher, rest) = rest.split_once("/").ok_or(USAGE)?;
		let (mode, padding) = rest.split_once("/").ok_or(USAGE)?;

		if bit_token != "bit" {
			return Err(USAGE);
		}

		Ok(match cipher {
			"RSA" | "revRSA" => {
				let bits = match key_size {
					"1024" => RsaBits::N1024,
					"2048" => RsaBits::N2048,
					"4096" => RsaBits::N4096,
					_ => return Err(r#"Allowed key sizes for RSA are: 1024, 2048 (recommended), 4096"#)
				};

				match mode {
					"ECB" => {},
					_ => return Err(r#"Allowed modes for RSA are: ECB"#)
				}

				let padding = match padding {
					"None" => RsaPadding::None,
					"PKCS1" => RsaPadding::Pkcs1,
					"PKCS1_OAEP" => RsaPadding::Pkcs1Oaep,
					"PKCS1_PSS" => RsaPadding::Pkcs1Pss,
					_ => return Err(r#"Allowed padding modes for RSA are: None, PKCS1, PKCS1_OAEP, PKCS1_PSS"#)
				};

				let rsa_mode = if cipher.starts_with("rev") { RsaMode::Reverse } else { RsaMode::Standard };

				AsymmEncryption::Rsa(bits, padding, rsa_mode)
			}
			_ => return Err(r#"Allowed ciphers are: AES, RSA, revRSA"#)
		})
	}
}

#[allow(unreachable_patterns)]
impl AsymmEncryption {
	/// Returns true if `self` is Rsa
	pub fn is_rsa(&self) -> bool {
		match self {
			AsymmEncryption::Rsa(..) => true,
			_ => false,
		}
	}
}

/// RSA padding mode. Using None is discouraged as it has been proven insecure.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display, ende_derive::Encode, ende_derive::Decode)]
#[ende(variant: fixed, 8)]
pub enum RsaPadding {
	/// No padding, only use if strictly necessary
	None,
	Pkcs1,
	Pkcs1Oaep,
	Pkcs1Pss
}

impl RsaPadding {
	// Unstable and for convenience only
	fn to_openssl_padding(&self) -> Padding {
		match self {
			RsaPadding::None => Padding::NONE,
			RsaPadding::Pkcs1 => Padding::PKCS1,
			RsaPadding::Pkcs1Oaep => Padding::PKCS1_OAEP,
			RsaPadding::Pkcs1Pss => Padding::PKCS1_PSS,
		}
	}
}

/// The RSA key length in bits. Using anything under 2048 bits is discouraged because insecure.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display, ende_derive::Encode, ende_derive::Decode)]
#[ende(variant: fixed, 8)]
pub enum RsaBits {
	/// Only use if strictly necessary
	#[display("1024")]
	N1024,
	#[display("2048")]
	N2048,
	#[display("4096")]
	N4096,
}

impl RsaBits {
	/// Returns the number of RSA key length bits `self` represents
	pub fn bits(&self) -> u32 {
		match self {
			RsaBits::N1024 => 1024,
			RsaBits::N2048 => 2048,
			RsaBits::N4096 => 4096,
		}
	}
}

/// The mode the RSA algorithm will operate in. This also affects how the RSA key is interpreted.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Display, ende_derive::Encode, ende_derive::Decode)]
#[ende(variant: fixed, 8)]
pub enum RsaMode {
	/// If you are unsure which to pick, then you probably want to use this one.<br>
	/// <br>
	/// During encoding, the public key is used for encryption.<br>
	/// During decoding, the private key is used for decryption.
	Standard,
	/// Used for signatures.<br>
	/// <br>
	/// During encoding, the private key is used for encryption.<br>
	/// During decoding, the public key is used for decryption.<br>
	Reverse
}

// Internal utility function for encrypting data to RSA
fn rsa_encrypt<T: Write>(
	encoder: &mut Encoder<T>,
	public: Option<&[u8]>,
	private: Option<&[u8]>,
	bits: RsaBits,
	padding: RsaPadding,
	mode: RsaMode,
	data: &[u8],
) -> EncodingResult<()> {
	let key_len = (bits.bits() / 8) as usize;
	let mut temp = vec![0u8; data.len()];

	// A mismatch between the data length and the key length is only expected
	// when the padding scheme is set to None, otherwise we return an error
	// However, if the data length is greater than the key length we error out anyway
	if data.len() > key_len || (data.len() != key_len && padding != RsaPadding::None) {
		return Err(EncodingError::EncryptionError(CryptoError::WrongKeySize(key_len)))
	}

	match mode {
		RsaMode::Standard => {
			// In "normal" RSA encryption mode, we interpret the key as a public key
			let public = public.or(encoder.ctxt.crypto.asymm.get_public());
			let rsa: Rsa<Public> = Rsa::public_key_from_der(public.ok_or(NoKey)?)
				.map_err(encryption::ese_to_ee)?;

			// A mismatch in key lengths is always an error
			if rsa.n().num_bytes() as usize != key_len {
				return Err(EncodingError::EncryptionError(CryptoError::WrongKeySize(key_len)))
			}

			// Encrypt with the public key - as expected
			rsa.public_encrypt(data, &mut temp, padding.to_openssl_padding())
				.map_err(encryption::ese_to_ee)?;
		}
		RsaMode::Reverse => {
			// In "reverse" mode, we interpret the key as a private key
			let private = private.or(encoder.ctxt.crypto.asymm.get_private());
			let rsa: Rsa<Private> = Rsa::private_key_from_der(private.ok_or(NoKey)?)
				.map_err(encryption::ese_to_ee)?;

			if rsa.n().num_bytes() as usize != key_len {
				return Err(EncodingError::EncryptionError(CryptoError::WrongKeySize(key_len)))
			}

			// And do a private key encryption (useful for signatures)
			rsa.private_encrypt(data, &mut temp, padding.to_openssl_padding())
				.map_err(encryption::ese_to_ee)?;
		}
	}

	// Our custom flatten condition is handled just fine
	// by the default encode implementation
	crate::Encode::encode(&data, encoder)
}

// Internal utility function for decrypting RSA data
fn rsa_decrypt<T: Read>(
	decoder: &mut Encoder<T>,
	public: Option<&[u8]>,
	private: Option<&[u8]>,
	bits: RsaBits,
	padding: RsaPadding,
	mode: RsaMode,
) -> EncodingResult<Vec<u8>> {
	let key_len = (bits.bits() / 8) as usize;
	let temp: Vec<u8> = {
		// Flatten behaves differently here:
		// it can be inferred to be the number of rsa bits.
		// When the padding is set to None, we cannot infer it and actually use
		// the flatten value
		if decoder.ctxt.flatten.is_some() && padding != RsaPadding::None {
			decoder.ctxt.flatten = Some(key_len);
		}

		crate::Decode::decode(decoder)?
	};
	let mut data = vec![0u8; temp.len()];

	// A mismatch between the data length and the key length is only expected
	// when the padding scheme is set to None, otherwise we return an error
	// However, if the data length is greater than the key length we error out anyway
	if data.len() > key_len || (data.len() != key_len && padding != RsaPadding::None) {
		return Err(EncodingError::EncryptionError(CryptoError::WrongKeySize(key_len)))
	}

	match mode {
		RsaMode::Standard => {
			// In "normal" RSA decryption mode, we interpret the key as a private key
			let private = private.or(decoder.ctxt.crypto.asymm.get_private());
			let rsa: Rsa<Private> = Rsa::private_key_from_der(private.ok_or(NoKey)?)
				.map_err(encryption::ese_to_ee)?;

			if rsa.n().num_bytes() as usize != key_len {
				return Err(EncodingError::EncryptionError(CryptoError::WrongKeySize(key_len)))
			}

			// And use it to decrypt just like you would expect
			rsa.private_decrypt(&temp, &mut data, padding.to_openssl_padding())
				.map_err(encryption::ese_to_ee)?;
		}
		RsaMode::Reverse => {
			// In "reverse" mode we instead interpret it as a public key
			let public = public.or(decoder.ctxt.crypto.asymm.get_public());
			let rsa: Rsa<Public> = Rsa::public_key_from_der(public.ok_or(NoKey)?)
				.map_err(encryption::ese_to_ee)?;

			if rsa.n().num_bytes() as usize != key_len {
				return Err(EncodingError::EncryptionError(CryptoError::WrongKeySize(key_len)))
			}

			// And do a public key decryption (useful for signatures)
			rsa.public_decrypt(&temp, &mut data, padding.to_openssl_padding())
				.map_err(encryption::ese_to_ee)?;
		}
	}
	Ok(data)
}