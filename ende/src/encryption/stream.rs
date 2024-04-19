use cryptostream::read::Decryptor;
use cryptostream::write::Encryptor;
use embedded_io::{Error, ErrorType};

use crate::{Read, Write};
use crate::encryption::{CryptoError, error_stack_err, SymmEncryption};

fn io_err<T: Error>(value: T) -> CryptoError<T> {
	CryptoError::IOError(value)
}

#[cfg(feature = "std")]
fn internal_err<T: Error>(value: std::io::Error) -> CryptoError<T> {
	if let std::io::ErrorKind::UnexpectedEof = value.kind() {
		CryptoError::UnexpectedEOF
	} else {
		CryptoError::Internal
	}
}

#[cfg(feature = "std")]
fn finalize_err<T: Error>(value: std::io::Error) -> CryptoError<T> {
	if let std::io::ErrorKind::UnexpectedEof = value.kind() {
		CryptoError::UnexpectedEOF
	} else {
		CryptoError::Finalize
	}
}

enum EncryptInner<T: Write> {
	None(T),
	#[cfg(feature = "std")]
	Cryptor(Encryptor<embedded_io_adapters::std::ToStd<T>>)
}

impl<T: Write> EncryptInner<T> {
	#[inline]
	fn finish(self) -> Result<T, CryptoError<T::Error>> {
		match self {
			EncryptInner::None(x) => Ok(x),
			#[cfg(feature = "std")]
			EncryptInner::Cryptor(x) => Ok(x.finish().map_err(finalize_err)?.into_inner())
		}
	}
}

impl<T: Write> ErrorType for EncryptInner<T> {
	type Error = CryptoError<T::Error>;
}

impl<T: Write> Write for EncryptInner<T> {
	#[inline]
	fn write(&mut self, buf: &[u8]) -> Result<usize, Self::Error> {
		match self {
			EncryptInner::None(x) => x.write(buf).map_err(io_err),
			#[cfg(feature = "std")]
			EncryptInner::Cryptor(x) => std::io::Write::write(x, buf).map_err(internal_err),
		}
	}
	#[inline]
	fn flush(&mut self) -> Result<(), Self::Error> {
		match self {
			EncryptInner::None(x) => x.flush().map_err(io_err),
			#[cfg(feature = "std")]
			EncryptInner::Cryptor(x) => std::io::Write::flush(x).map_err(internal_err),
		}
	}
}

/// A writer that encrypts the data written to it before
/// forwarding it to the underlying stream.<br>
/// This value can be constructed by calling [`SymmEncryption::encrypt`]
/// with a type implementing [`std::io::Write`]
#[repr(transparent)]
pub struct Encrypt<T: Write>(EncryptInner<T>);

impl<T: Write> Encrypt<T> {
	/// Flushes all the data yet to be encrypted and potentially pads it to the nearest full
	/// block before returning the inner stream
	#[inline]
	pub fn finish(self) -> Result<T, CryptoError<T::Error>> {
		self.0.finish()
	}
}

impl<T: Write> ErrorType for Encrypt<T> {
	type Error = CryptoError<T::Error>;
}

impl<T: Write> Write for Encrypt<T> {
	#[inline]
	fn write(&mut self, buf: &[u8]) -> Result<usize, Self::Error> {
		self.0.write(buf)
	}
	#[inline]
	fn flush(&mut self) -> Result<(), Self::Error> {
		self.0.flush()
	}
}

enum DecryptInner<T: Read> {
	None(T),
	#[cfg(feature = "std")]
	Decryptor(Decryptor<embedded_io_adapters::std::ToStd<T>>)
}

impl<T: Read> DecryptInner<T> {
	#[inline]
	fn finish(self) -> Result<T, CryptoError<T::Error>> {
		match self {
			DecryptInner::None(x) => Ok(x),
			#[cfg(feature = "std")]
			DecryptInner::Decryptor(x) => Ok(x.finish().into_inner()),
		}
	}
}

impl<T: Read> ErrorType for DecryptInner<T> {
	type Error = CryptoError<T::Error>;
}

impl<T: Read> Read for DecryptInner<T> {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error> {
		match self {
			DecryptInner::None(x) => x.read(buf).map_err(io_err),
			#[cfg(feature = "std")]
			DecryptInner::Decryptor(x) => std::io::Read::read(x, buf).map_err(internal_err),
		}
	}
}

/// A reader that decrypts the data read from the underlying
/// stream before returning it.<br>
/// This value can be constructed by calling [`SymmEncryption::decrypt`]
/// with a type implementing [`std::io::Read`]
#[repr(transparent)]
pub struct Decrypt<T: Read>(DecryptInner<T>);

impl<T: Read> Decrypt<T> {
	/// Potentially reads the remaining bytes needed for padding up to a
	/// full block, then returns the inner stream
	#[inline]
	pub fn finish(self) -> Result<T, CryptoError<T::Error>> {
		self.0.finish()
	}
}

impl<T: Read> ErrorType for Decrypt<T> {
	type Error = CryptoError<T::Error>;
}

impl<T: Read> Read for Decrypt<T> {
	#[inline]
	fn read(&mut self, buf: &mut [u8]) -> Result<usize, Self::Error> {
		self.0.read(buf)
	}
}

impl SymmEncryption {
	/// Wraps a type implementing [`std::io::Write`] in a [`Encrypt`] using `self` as the algorithm,
	/// and key and iv as the parameters.
	pub fn encrypt<T: Write>(&self, input: T, key: Option<&[u8]>, iv: Option<&[u8]>) -> Result<Encrypt<T>, CryptoError<T::Error>> {
		match self {
			SymmEncryption::None => {
				Ok(Encrypt(EncryptInner::None(input)))
			}
			#[cfg(feature = "std")]
			_ => {
				let input = embedded_io_adapters::std::ToStd::new(input);
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
					.map_err(error_stack_err)?;
				Ok(Encrypt(EncryptInner::Cryptor(crypto_stream)))
			}
		}
	}

	/// Wraps a type implementing [`std::io::Read`] in a [`Decrypt`] using `self` as the algorithm,
	/// and key and iv as the parameters.
	pub fn decrypt<T: Read>(&self, input: T, key: Option<&[u8]>, iv: Option<&[u8]>) -> Result<Decrypt<T>, CryptoError<T::Error>> {
		match self {
			SymmEncryption::None => {
				Ok(Decrypt(DecryptInner::None(input)))
			}
			#[cfg(feature = "std")]
			_ => {
				let input = embedded_io_adapters::std::ToStd::new(input);
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
					.map_err(error_stack_err)?;
				Ok(Decrypt(DecryptInner::Decryptor(crypto_stream)))
			}
		}
	}
}