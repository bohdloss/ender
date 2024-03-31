//! The following file contains enums and functions copied from the
//! `ende` library. Any modifications done to the enums in the original library
//! must be ALWAYS reflected here.

use std::str::FromStr;
use parse_display::{Display, FromStr};

#[derive(Copy, Clone, Eq, PartialEq, Display)]
pub enum Endianness {
	LittleEndian,
	BigEndian
}

#[derive(Copy, Clone, Eq, PartialEq, Display)]
pub enum NumEncoding {
	Fixed,
	Leb128
}

#[derive(Copy, Clone, Eq, PartialEq, Display)]
pub enum BitWidth {
	Bit8,
	Bit16,
	Bit32,
	Bit64,
	Bit128
}

#[derive(Copy, Clone, Eq, PartialEq, Display)]
pub enum AsymmEncryption {
	#[display("Rsa")]
	Rsa(RsaBits, RsaPadding, RsaMode),
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

#[derive(Copy, Clone, Eq, PartialEq, Display)]
pub enum RsaBits {
	N1024,
	N2048,
	N4096,
}

#[derive(Copy, Clone, Eq, PartialEq, Display)]
pub enum RsaPadding {
	None,
	Pkcs1,
	Pkcs1Oaep,
	Pkcs1Pss,
}

#[derive(Copy, Clone, Eq, PartialEq, Display)]
pub enum RsaMode {
	Standard,
	Reverse,
}

#[derive(Copy, Clone, Eq, PartialEq, Display)]
pub enum SymmEncryption {
	None,
	#[display("Aes")]
	Aes(AesBits, AesMode),
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

#[derive(Copy, Clone, Eq, PartialEq, Display)]
pub enum AesBits {
	N128,
	N192,
	N256,
}

#[derive(Copy, Clone, Eq, PartialEq, Display)]
pub enum AesMode {
	Ecb,
	Cbc,
	#[display("Cfb")]
	Cfb(CfbFeedback),
	Ofb,
	Ctr,
}

#[derive(Copy, Clone, Eq, PartialEq, Display)]
pub enum CfbFeedback {
	N1,
	N8,
	N128,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Display, FromStr)]
pub enum ZStdLevel {
	#[display("1")]
	L1 = 1,
	#[display("2")]
	L2 = 2,
	#[display("3")]
	L3 = 3,
	#[display("4")]
	L4 = 4,
	#[display("5")]
	L5 = 5,
	#[display("6")]
	L6 = 6,
	#[display("7")]
	L7 = 7,
	#[display("8")]
	L8 = 8,
	#[display("9")]
	L9 = 9,
	#[display("10")]
	L10 = 10,
	#[display("11")]
	L11 = 11,
	#[display("12")]
	L12 = 12,
	#[display("13")]
	L13 = 13,
	#[display("14")]
	L14 = 14,
	#[display("15")]
	L15 = 15,
	#[display("16")]
	L16 = 16,
	#[display("17")]
	L17 = 17,
	#[display("18")]
	L18 = 18,
	#[display("19")]
	L19 = 19,
	#[display("20")]
	L20 = 20,
	#[display("21")]
	L21 = 21,
	#[display("22")]
	L22 = 22,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Display, FromStr)]
pub enum ZLibLevel {
	#[display("0")]
	L0 = 0,
	#[display("1")]
	L1 = 1,
	#[display("2")]
	L2 = 2,
	#[display("3")]
	L3 = 3,
	#[display("4")]
	L4 = 4,
	#[display("5")]
	L5 = 5,
	#[display("6")]
	L6 = 6,
	#[display("7")]
	L7 = 7,
	#[display("8")]
	L8 = 8,
	#[display("9")]
	L9 = 9,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Display, FromStr)]
pub enum DeflateLevel {
	#[display("0")]
	L0 = 0,
	#[display("1")]
	L1 = 1,
	#[display("2")]
	L2 = 2,
	#[display("3")]
	L3 = 3,
	#[display("4")]
	L4 = 4,
	#[display("5")]
	L5 = 5,
	#[display("6")]
	L6 = 6,
	#[display("7")]
	L7 = 7,
	#[display("8")]
	L8 = 8,
	#[display("9")]
	L9 = 9,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Display, FromStr)]
pub enum GZipLevel {
	#[display("1")]
	L1 = 1,
	#[display("2")]
	L2 = 2,
	#[display("3")]
	L3 = 3,
	#[display("4")]
	L4 = 4,
	#[display("5")]
	L5 = 5,
	#[display("6")]
	L6 = 6,
	#[display("7")]
	L7 = 7,
	#[display("8")]
	L8 = 8,
	#[display("9")]
	L9 = 9,
}

#[derive(Copy, Clone, Eq, PartialEq, Display)]
pub enum Compression {
	None,
	#[display("ZStd")]
	ZStd(ZStdLevel),
	#[display("ZLib")]
	ZLib(ZLibLevel),
	#[display("Deflate")]
	Deflate(DeflateLevel),
	#[display("GZip")]
	GZip(GZipLevel),
}

impl FromStr for Compression {
	type Err = &'static str;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		const USAGE: &str = r#"Invalid compression format. Usage: "{format}/{level}""#;

		if s == "None" {
			return Ok(Compression::None);
		}

		let (format, level) = s.split_once("/").ok_or(USAGE)?;

		Ok(match format {
			"ZStd" => Compression::ZStd(ZStdLevel::from_str(level).map_err(|_| "Out of range 1-22")?),
			"ZLib" => Compression::ZLib(ZLibLevel::from_str(level).map_err(|_| "Out of range 0-9")?),
			"Deflate" => Compression::Deflate(DeflateLevel::from_str(level).map_err(|_| "Out of range 0-9")?),
			"GZip" => Compression::GZip(GZipLevel::from_str(level).map_err(|_| "Out of range 1-9")?),
			_ => return Err(r#"Allowed compression formats are: ZStd, ZLib, Deflate, GZip"#)
		})
	}
}