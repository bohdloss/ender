mod symm;
mod asymm;
mod stream;

use std::fmt::Debug;
use std::io;
use openssl::error::ErrorStack;
use parse_display::Display;
use thiserror::Error;
use crate::{EncodingError};

pub use symm::*;
pub use asymm::*;
pub use stream::*;

/// Contains asymmetric and symmetric encryption data that is known at a higher level than
/// the encoding/decoding step. Consists of a [`AsymmState`] and a [`SymmState`].
#[derive(Clone, Eq, PartialEq, Debug, Display)]
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

	/// Similar to clone, but hints that the keys being stored should not be cloned to
	/// a new memory location, but simply borrowed
	pub fn borrow_clone(&self) -> CryptoState {
		CryptoState {
			asymm: self.asymm.borrow_clone(),
			symm: self.symm.borrow_clone(),
		}
	}
	
	/// Equivalent to [`CryptoState::new`] except a public and private key are stored in the
	/// asymmetric encryption state before returning
	pub fn from_pub_priv_key<T: AsRef<[u8]>, U: AsRef<[u8]>>(public: &'a T, private: &'a U) -> Self {
		Self {
			asymm: AsymmState::with_keys(public, private),
			symm: SymmState::new(),
		}
	}

	/// Equivalent to [`CryptoState::new`] except a symmetric key and iv are stored
	/// in the symmetric encryption state before returning
	pub fn from_iv_and_key<T: AsRef<[u8]>, U: AsRef<[u8]>>(key: &'a T, iv: &'a U) -> Self {
		Self {
			asymm: AsymmState::new(),
			symm: SymmState::with_key_and_iv(key, iv),
		}
	}
}

/// An error type for anything that might go wrong during Encryption/Decryption.<br>
/// FIXME This is still subject to change
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

// Error Stack Error to Encoding Error
fn ese_to_ee(_: ErrorStack) -> EncodingError {
	EncodingError::EncryptionError(CryptoError::Generic)
}