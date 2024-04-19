use std::fmt::Debug;

use embedded_io::{Error, ErrorKind, ReadExactError};
use openssl::error::ErrorStack;
use parse_display::Display;

pub use asymm::*;
pub use stream::*;
pub use symm::*;

mod symm;
mod asymm;
mod stream;

fn error_stack_err<E: Error>(_: ErrorStack) -> CryptoError<E> {
	CryptoError::Internal
}

/// Contains asymmetric and symmetric encryption data that is known at a higher level than
/// the encoding/decoding step. Consists of a [`AsymmState`] and a [`SymmState`].
#[derive(Copy, Clone, Eq, PartialEq, Debug, Display)]
#[display("asymm = ({asymm}), symm = ({symm})")]
pub struct CryptoState<'a> {
	pub asymm: AsymmState<'a>,
	pub symm: SymmState<'a>,
}

impl<'a> CryptoState<'a> {
	/// Constructs the default `CryptoState`, no key or iv is stored,
	/// the encryption parameters are undefined
	pub const fn new() -> Self {
		Self {
			asymm: AsymmState::new(),
			symm: SymmState::new(),
		}
	}
	
	/// Equivalent to [`CryptoState::new`] except a public and private key are stored in the
	/// asymmetric encryption state before returning
	pub fn from_keypair<T: AsRef<[u8]>, U: AsRef<[u8]>>(public: &'a T, private: &'a U) -> Self {
		Self {
			asymm: AsymmState::from_keypair(public, private),
			symm: SymmState::new(),
		}
	}

	/// Equivalent to [`CryptoState::new`] except a symmetric key and iv are stored
	/// in the symmetric encryption state before returning
	pub fn from_key_and_iv<T: AsRef<[u8]>, U: AsRef<[u8]>>(key: &'a T, iv: &'a U) -> Self {
		Self {
			asymm: AsymmState::new(),
			symm: SymmState::from_key_and_iv(key, iv),
		}
	}
}

/// An error type for anything that might go wrong during Encryption/Decryption.<br>
/// FIXME This is still subject to change
#[derive(Debug, Display)]
pub enum CryptoError<E: Error> {
	/// Generic IO error
	#[display("IO Error occurred: {:0?}")]
	IOError(
		E
	),
	/// The end of the file or buffer was reached but more data was expected
	#[display("Unexpected end of file/buffer")]
	UnexpectedEOF,
	/// The user provided no key, but an encryption method different from `None`
	#[display("Missing encryption/decryption key")]
	NoKey,
	/// The user provided no IV, but an encryption method that requires it
	#[display("Missing initialization vector")]
	NoIV,
	/// The size of the user-provided key differs from what the algorithm expected
	#[display("Wrong key size ({0} bytes expected)")]
	WrongKeySize(usize),
	/// The size of the user-provided IV differs from what the algorithm expected
	#[display("Wrong IV size ({0} bytes expected)")]
	WrongIvSize(usize),
	/// The provided key was not valid
	#[display("Wrong decryption key provided")]
	WrongKey,
	/// An error occurred while initializing the encryptor
	#[display("Initialization error")]
	Initialize,
	/// A generic/internal encryption error occurred, no further details are provided
	#[display("Generic Encryption/Decryption error")]
	Internal,
	/// An error occurred while finalizing the encryptor
	#[display("Finalization error")]
	Finalize,
}

impl<E: Error> Error for CryptoError<E> {
	fn kind(&self) -> ErrorKind {
		match self {
			CryptoError::IOError(io_error) => io_error.kind(),
			CryptoError::Internal => ErrorKind::Other,
			CryptoError::Initialize => ErrorKind::Other,
			CryptoError::Finalize => ErrorKind::Other,
			_ => ErrorKind::InvalidInput,
		}
	}
}

#[cfg(feature = "unstable")]
impl<E: Error> core::error::Error for CryptoError<E> {}

#[cfg(all(not(feature = "unstable"), feature = "std"))]
impl<E: Error> std::error::Error for CryptoError<E> {}

impl<E: Error> From<E> for CryptoError<E> {
	fn from(value: E) -> Self {
		Self::IOError(value)
	}
}

impl<E: Error> From<ReadExactError<E>> for CryptoError<E> {
	fn from(value: ReadExactError<E>) -> Self {
		match value {
			ReadExactError::UnexpectedEof => Self::UnexpectedEOF,
			ReadExactError::Other(io_error) => Self::IOError(io_error),
		}
	}
}