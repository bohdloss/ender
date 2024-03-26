use std::io::{Read, Write};
use cryptostream::read::Decryptor;
use cryptostream::write::Encryptor;
use crate::{EncodingResult, Finish};
use crate::encryption::{CryptoError, SymmEncryption};

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

/// A writer that encrypts the data written to it before
/// forwarding it to the underlying stream.<br>
/// This value can be constructed by calling [`SymmEncryption::encrypt`]
/// with a type implementing [`std::io::Write`]
#[repr(transparent)]
pub struct Encrypt<T: Write>(EncryptInner<T>);

impl<T: Write> Finish for Encrypt<T> {
	type Output = T;

	/// Flushes all the data yet to be encrypted and potentially pads it to the nearest full
	/// block before returning the inner stream
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

/// A reader that decrypts the data read from the underlying
/// stream before returning it.<br>
/// This value can be constructed by calling [`SymmEncryption::decrypt`]
/// with a type implementing [`std::io::Read`]
#[repr(transparent)]
pub struct Decrypt<T: Read>(DecryptInner<T>);

impl<T: Read> Finish for Decrypt<T> {
	type Output = T;

	/// Potentially reads the remaining bytes needed for padding up to a
	/// full block, then returns the inner stream
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

impl SymmEncryption {
	/// Wraps a type implementing [`std::io::Write`] in a [`Encrypt`] using `self` as the algorithm,
	/// and key and iv as the parameters.
	pub fn encrypt<T: Write>(&self, input: T, key: Option<&[u8]>, iv: Option<&[u8]>) -> Result<Encrypt<T>, CryptoError> {
		match self {
			SymmEncryption::None => {
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

	/// Wraps a type implementing [`std::io::Read`] in a [`Decrypt`] using `self` as the algorithm,
	/// and key and iv as the parameters.
	pub fn decrypt<T: Read>(&self, input: T, key: Option<&[u8]>, iv: Option<&[u8]>) -> Result<Decrypt<T>, CryptoError> {
		match self {
			SymmEncryption::None => {
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