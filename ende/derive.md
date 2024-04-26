Helper macros to derive [`Encode`] and [`Decode`] for a `struct` or `enum`.<br>
This macro supports a series of helper flags to aid customization<br>
<br>
All flags follow the following format:<br>
`#[ende(flag1; flag2; flag2; ...)]`<br>
<br>
The 2 special flags `en` and `de`, called Scope flags, can be used only at the beginning
of the list to indicate that all the flags in the attribute declaration only apply to the
encoding process (`en`) or the decoding process (`de`).<br>
<br>
If neither of those flags are specified, then it is assumed that all the flags in the
attribute declaration apply to both encoding and decoding<br>
<br>
The flags currently implemented are split into 5 groups:<br>
<br>
# 1. Setting Modifiers
Setting-Modifier flags temporarily change certain settings of the encoder and can be applied
to Fields or Items (the whole struct or enum).<br>
Multiple can be specified at the same time, as long as they don't overlap.<br>
* `$target: big_endian, little_endian` - `$target` can be `num`, `size`, `variant`, `string`
* `$target: fixed, leb128, protobuf_wasteful, protobuf_zz` - `$target` can be `num`, `size`, `variant`
* `$target: 8, 16, 32, 64, 128` - `$target` can be `size`, `variant`
* `$target: max = $expr` - `$target` can only be `size`<br>
* `$target: ascii, utf_8, utf_16, utf_32` - `$target` can only be `string`<br>
* `$target: nul_term, len_prefix` - `$target` can only be `string`<br>
<br>
### Example:
```rust
use ende::{Encode, Decode};
#[derive(Encode, Decode)]
#[ende(crate: ende)]
/// The variants of this enum will be encoded in the little endian ordering,
/// using a fixed numerical encoding and a 32-bit width.
#[ende(variant: little_endian, fixed, 32)]
enum MyEnum {
    VariantA {
        /// The length of this String will be encoded using big endian ordering,
        /// fixed numerical encoding and 16-bit width, with a max length of 100
        #[ende(size: big_endian, fixed, 16, max = 100)]
        /// field: String,
        /// Encode this String to utf16 with a length prefix
        #[ende(string: utf_16, len_prefix, big_endian)]
        utf_16: String,
        /// Encode this String as a null-terminated ascii string
        #[ende(string: ascii, nul_term)]
        ascii: String,
    },
    VariantB {
        /// This number will be encoded using little endian ordering, and the
        /// leb128 [NumEncoding][`ende::NumEncoding`]
        #[ende(num: little_endian, leb128)]
        field: u64,
        #[ende(num: protobuf_zz)]
        zigzag: i128,
	},
}
```
# 2. Stream Modifiers
Stream-Modifier flags temporarily change the underlying reader/writer, and can be applied
to Fields or Items.<br>
Note that the order in which stream modifiers are declared is very important:<br>
They are applied in the declaration order during encoding, but in the reverse order during
decoding, for consistency. However, the item-level modifiers take priority over the field-level
modifiers (see [example](#ambiguous-example)).<br>
* `redir: $path(...)` - Currently the only supported stream modifier, but more may
be added in the future. Uses the given path to find an encoding/decoding function which
alters the writer/reader and passes a modified encoder to a closure.<br>
This can be used to implement encryption, compression and other transformations of the
underlying stream, or even redirections to another stream.<br>
The implementation of a redir function can be non-trivial and the signature can be
cryptic, therefore it is recommended you only create your own stream transforms if you know what
you're doing, in which case you should take a look at the [facade module][`facade`].<br>
    * If the scope is Encode, the path must be callable as `encode`.<br>
    * If the scope is Decode, the path must be callable as `decode`.<br>
    * If no scope is specified, the path must point to a module with encoding and decoding functions
with the same signatures as above.
### Example:
```rust
use ende::{Encode, Decode};
use ende::facade::fake::*;
#[derive(Encode, Decode)]
#[ende(crate: ende)]
struct MyStruct {
    secret_key: Vec<u8>,
    iv: Vec<u8>,
    /// While **encoding**, this field is compressed -> encrypted.
    /// While **decoding**, this field is decrypted -> decompressed.
    #[ende(redir: gzip(9))]
    #[ende(redir: aes(iv, secret_key))]
    super_secret_data: Vec<u8>,
}
```
### Ambiguous example:
```rust
use ende::{Encode, Decode};
use ende::facade::fake::*;
#[derive(Encode, Decode)]
#[ende(crate: ende)]
/// Because of the priority rules of items over fields, this is ambiguous, see below
#[ende(redir: gzip(9))]
struct MyStruct {
    /// While **encoding**, this field is encrypted -> compressed.
    /// While **decoding**, this field is decompressed -> decrypted.
    /// Because the "compressed" flag is declared before the "encrypted" flag, one might
    /// think they are applied in that order. However, since the item-level flag takes priority,
    /// it is applied *after* the field flag.
    /// 
    /// According to your needs, this might not be what you want, so be careful when mixing
    /// item-level and field-level stream modifiers.
    #[ende(redir: aes(&[], &[]))]
    super_secret_data: Vec<u8>,
}
```
# 3. Function Modifiers
Function-Modifier flags change the function that is used to encode/decode a field or item.
* `serde: $crate` - Field will be serialized/deserialized with a serde compatibility layer.<br>
Optionally, the serde crate name can be specified (useful if the serde crate was re-exported under
another name).
* `with: $path` - Uses the given path to find the encoding/decoding function.<br>
    * If the scope is Encode, the path must be callable as
`fn<T: Write>(&V, &mut ende::Encoder<T>) -> EncodingResult<()>`
where `V` is the type of the field (the function is allowed to be generic over `V`).<br>
    * If the scope is Decode, the path must be callable as
`fn<T: Read>(&mut ende::Encoder<T>) -> EncodingResult<V>`
where `V` is the type of the field (the function is allowed to be generic over `V`).<br>
    * If no scope is specified, the path must point to a module with encoding and decoding functions
with the same signatures as above.
### Example:
```rust
use ende::{Encode, Decode};
use ende::facade::fake::rsa;
use uuid::Uuid;
///
#[derive(Encode, Decode)]
#[ende(crate: ende)]
struct Friends {
	/// Has Serialize/Deserialize implementations, but no Encode/Decode implementations.
    /// A perfect fit for integrating with serde!
    #[ende(serde)]
    uuid: Uuid,
    /// Here we demonstrate how the with flag changes based on whether or not a scope
    /// is declared. This:
    #[ende(with: person_encoder)]
    friend1: person_encoder::Person,
    /// ...is equivalent to this!
    #[ende(en; with: person_encoder::encode)]
    #[ende(de; with: person_encoder::decode)]
    friend2: person_encoder::Person,
    /// Not the smartest way to store a private key!
    private_key: Vec<u8>,
    public_key: Vec<u8>,
    /// This block of data will be encrypted before being encoded using the public key,
    /// and decrypted after being decoded using the private key.
    #[ende(with: rsa(public_key, private_key))]
    even_more_secret_data: Vec<u8>,
}

mod person_encoder {
    use ende::io::{Write, Read};
	use ende::{Encoder, EncodingResult, Encode};
	
    pub struct Person {
        name: String,
        surname: String,
        age: u32,
    }
    
	pub fn encode<T: Write>(person: &Person, encoder: &mut Encoder<T>) -> EncodingResult<()> {
        person.name.encode(encoder)?;
        person.surname.encode(encoder)?;
        person.age.encode(encoder)?;
        Ok(())
	}
	
	pub fn decode<T: Read>(encoder: &mut Encoder<T>) -> EncodingResult<Person> {
        Ok(Person {
            name: encoder.decode_value()?,
            surname: encoder.decode_value()?,
            age: encoder.decode_value()?,
		})
	}
}
```
# 4. Type Modifiers
Type-Modifier flags change the type of the value that's encoded, and change it back after
decoding it.<br>
* `as: $ty` - Converts the value of the field to `$ty` before encoding it
and back to the original field type after decoding it.<br>
The conversion is done through the `as` keyword.
* `into: $ty` - Converts the value of the field to `$ty` before encoding it
and back to the original field type after decoding it.<br>
The conversion is done through the `From` and `Into` traits.
### Example:
```rust
use ende::{Encode, Decode};

#[derive(Encode, Decode)]
#[ende(crate: ende)]
struct Mountain {
    /// Height is encoded as a `u16`, then decoded back to a `f64`.
    /// These operations are performed using the `as` keyword.
    #[ende(as: u16)]
    height: f64,
    /// Boulder is encoded as a `BigRock`, then decoded back to a `Boulder`.
    /// This can be done because `BigRock` implements `From<Boulder>`, and
    /// `Boulder` implements `From<BigRock>`.
    ///
    /// Note: `BigRock` is required to implement `Encode` and `Decode`,
    /// but `Boulder` is not.
    #[ende(into: BigRock)]
    boulder: Boulder,
}

#[derive(Encode, Decode)]
#[ende(crate: ende)]
struct BigRock {
    weight: f64
}

impl From<Boulder> for BigRock {
    fn from(value: Boulder) -> Self {
		Self {
            weight: value.weight
		}
    }
}

#[derive(Clone)]
struct Boulder {
    weight: f64,
    radius: f64
}

impl From<BigRock> for Boulder {
    fn from(value: BigRock) -> Self {
		Self {
			weight: value.weight,
            radius: 1.0
		}
	}
}
```
# 5. Helpers
Helper flags change certain parameters or add conditions for when a field
or item should be encoded/decoded.<br>
* `crate: $crate` - Overwrites the default crate name which is assumed to be `ende`.
Can only be applied to items.
* `if: $expr` - The field will only be encoded/decoded if the given expression
evaluates to true, otherwise the default value is computed
* `default: $expr` - Overrides the default fallback for when a value can't be
deserialized (`Default::default()`)
* `skip` - Will not encode/decode this field.
When decoding, computes the default value
* `validate: $expr, $format_string, $arg1, $arg2, $arg3, ...` - Before encoding/after decoding, returns an error if the
expression evaluates to false. The error message will use the given formatting (if present)
* `flatten: $expr` - Indicates that the length of the given field (for example
a Vec or HashMap) doesn't need to be encoded/decoded, because it is known from the context.
Can also be used with an `Option` in conjunction with the `if` flag and without the `$expr`
to indicate that the presence of an optional value is known from the context.<br>
<br>
### Example:
```rust
use ende::{Encode, Decode};
use uuid::Uuid;

// Hehe >:3
extern crate ende as enderman;

#[derive(Encode, Decode)]
/// We specify the name of the re-exported ende crate.
#[ende(crate: enderman)]
struct PersonEntry {
    /// Will come in handy later
    name_present: bool,
    /// Similar to the previous example, but with the addition of the flatten flag!
    /// We know a Uuid is always 16 bytes long, so we omit writing/reading that data.
    #[ende(serde; flatten: 16)]
    uuid: Uuid,
	/// Just the string version of the uuid, not present in the binary data.
    /// Skip the Encoding step, and Decode it from the uuid.
    #[ende(skip; default: uuid.to_string())]
	uuid_string: String,
    /// We know whether this is present from the context, therefore we don't write whether
    /// the optional value is present, and when reading we assume it is.
    /// Since the "if" flag is also present, the field will only be decoded if the expression
    /// evaluates to true, making the previous operation safe~~~~
	/// (no risk of decoding garbage data)
    #[ende(flatten: some; if: *name_present)]
    name: Option<String>,
    /// Only present if the name is also present, but we want to provide a custom default!
    #[ende(default: String::from("Smith"); if: *name_present)]
    surname: String,
    /// No-one allowed before 18!
    #[ende(validate: *age >= 18, "User is too young: {}", age)]
    age: u32,
    /// This is temporary data, we don't care about including it in the binary format.
    #[ende(skip; default: 100)]
    health: u64,
}
```