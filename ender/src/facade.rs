//! Contains some fake functions for encryption/decryption, compression/decompression
//! to test the derive macros.

/// In case that was not made clear: the functions in this module are fake!<br>
/// They are just here to allow testing the derive macros in the playground.
pub mod fake {
    pub mod rsa {
        use crate::{Read, Write};

        use crate::{Decode, Encode, Encoder, EncodingResult};

        pub fn encode<V: Encode<T>, T: Write>(
            _value: V,
            _encoder: &mut Encoder<T>,
            _public_key: &[u8],
            _private_key: &[u8],
        ) -> EncodingResult<()> {
            // Rsa encryption code here
            unimplemented!()
        }

        pub fn decode<'de, 'ctx, V: Decode<'de, T>, T: Read<'de>>(
            _decoder: &mut Encoder<'ctx, T>,
            _public_key: &[u8],
            _private_key: &[u8],
        ) -> EncodingResult<V>
        where 'de: 'ctx
        {
            // Rsa decryption code here
            unimplemented!()
        }
    }

    pub mod aes {
        use crate::{Read, Write};

        use crate::{Encoder, EncodingResult};

        pub fn encode<Orig, F>(
            _encoder: &mut Encoder<Orig>,
            _fun: F,
            _iv: &[u8],
            _key: &[u8],
        ) -> EncodingResult<()>
        where
            Orig: Write,
            F: FnOnce(&mut Encoder<Orig>) -> EncodingResult<()>,
        {
            // Aes encryption code here
            unimplemented!()
        }

        pub fn decode<'de, 'ctx, Orig, Val, F>(
            _encoder: &mut Encoder<'ctx, Orig>,
            _fun: F,
            _iv: &[u8],
            _key: &[u8],
        ) -> EncodingResult<Val>
        where
            Orig: Read<'de>,
            F: FnOnce(&mut Encoder<'ctx, Orig>) -> EncodingResult<Val>,
            'de: 'ctx
        {
            // Aes decryption code here
            unimplemented!()
        }
    }

    pub mod zlib {
        pub use super::gzip::*;
    }

    pub mod gzip {
        use crate::{Read, Write};

        use crate::{Encoder, EncodingResult};

        pub fn encode<Orig, F>(
            _encoder: &mut Encoder<Orig>,
            _fun: F,
            _compression_level: u32,
        ) -> EncodingResult<()>
        where
            Orig: Write,
            F: FnOnce(&mut Encoder<Orig>) -> EncodingResult<()>,
        {
            // Compression code here
            unimplemented!()
        }

        pub fn decode<'de, 'ctx, Orig, Val, F>(
            _encoder: &mut Encoder<'ctx, Orig>,
            _fun: F,
            _compression_level: u32,
        ) -> EncodingResult<Val>
        where
            Orig: Read<'de>,
            F: FnOnce(&mut Encoder<'ctx, Orig>) -> EncodingResult<Val>,
            'de: 'ctx
        {
            // Decompression code here
            unimplemented!()
        }
    }
}
