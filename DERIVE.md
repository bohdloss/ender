# The Ender Derivonomicon

The following is a full and up-to-date breakdown of the `Encode` and `Decode` derive macros,
their attributes, how they work and all the possible ways you can modify the codegen.

## Flags Essentials

All flags follow the following format:
`#[ender(flag1; flag2; flag2; ...)]`

The 2 special flags `en` and `de`, called Scope flags, can be used only at the beginning
of the list to indicate that all the flags in the attribute declaration only apply to the
encoding process (`en`) or the decoding process (`de`).

If neither of those flags are specified, then it is assumed that all the flags in the
attribute declaration apply to both encoding and decoding.

Whenever a flag is said to accept an `$expr`, this means any expression is accepted,
and that it will have access to an immutable reference to all the fields that have been
decoded so far (actually all the fields while encoding), but the `validate` flag additionally
provides a reference to the field it is applied on.<br>
If the fields are part of a tuple struct/variant, the references will be named `m{idx}` where `idx` are the
indexes of the tuple fields (E.G. `m0`, `m1`, ...), otherwise their names will match those of the fields themselves.

A seeking impl is an implementation of [`Encode`] or [`Decode`] that has an additional [`Seek`]
trait bound
When a flag is said to be a `seek` flag, it means that when used anywhere it will switch the
impl to a seeking impl.

A borrowing impl is an implementation of [`Encode`] or [`Decode`] that has a [`BorrowRead`]
trait bound rather than a [`Read`] one.
When a flag is said to be a `borrow` flag, it means that when used anywhere it will switch the
impl to a borrowing impl.

Borrowing and seeking impls can be combined.

The flags currently implemented are split into 5 groups:
# 1. Setting Modifiers
Setting-Modifier flags temporarily change certain settings of the encoder and can be applied
to Fields or Items (the whole struct or enum).<br>
Multiple can be specified at the same time, as long as they don't overlap.<br>
All setting modifiers follow the `$target: $mod1, $mod2, ...` pattern.

- Endianness modifiers: `big_endian`, `little_endian`
  - Available targets:
    - `num`
    - `size`
    - `variant`
    - `string`
- Numerical encoding modifiers: `fixed`, `leb128`, `protobuf_wasteful`, `protobuf_zz`
  - Available targets:
    - `num`,
    - `size`,
    - `variant`
- Bit-width modifiers: `bit8`, `bit16`, `bit32`, `bit64`, `bit128`
  - Available targets:
    - `size`
    - `variant`
- Max-size modifier: `max = $expr`
  - Available targets:
    - `size`
- String encoding modifier: `ascii`, `utf8`, `utf16`, `utf32`, `windows1252`
  - Available targets:
    - `string`
- String length encoding modifier: `len_prefix`, `null_term`, `null_term($max:expr)`
  - Available targets:
    - `string`
    <br>
### Example:
```rust
#[derive(Encode, Decode)]
/// The variants of this enum will be encoded in the little endian ordering,
/// using a fixed numerical encoding and a 32-bit width.
#[ender(variant: little_endian, fixed, bit32)]
enum MyEnum {
    VariantA {
        /// The length of this String will be encoded using big endian ordering,
        /// fixed numerical encoding and 16-bit width, with a max length of 100
        #[ender(size: big_endian, fixed, bit16, max = 100)]
        field: String,
        /// Encode this String with utf16 big endian, and prefix it with its length
        #[ender(string: utf16, big_endian, len_prefix)]
        utf_16: String,
        /// Encode this String as an utf8 null-terminated string
        #[ender(string: utf8, null_term)]
        utf_8: String,
        /// Encode this String as an utf8 null-terminated string with a fixed length of `15`
        /// Exactly `15` bytes will be read in all cases, and the length of the string is given
        /// by the first occurrence of a null byte or the `15` byte mark.
        #[ender(string: utf8, null_term(15))]
        max_15: String,
    },
    VariantB {
        /// This number will be encoded using little endian ordering, and the
        /// leb128 [NumEncoding][`ender::NumEncoding`]
        #[ender(num: little_endian, leb128)]
        field: u64,
        #[ender(num: protobuf_zz)]
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
modifiers (see [ambiguous example](#ambiguous-example)).<br>
* `redir: $path(...)` - Uses the given path to find an encoding/decoding function which
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
* `ptr $seek: $expr` - This is a `seek` flag. Seeks to the location given by $expr
(which must be of type usize or isize) relative to $seek - which can be
"start", "end" or "cur"rrent - before encoding/decoding this field, then seeks back to the
previous location.
### Example:
```rust
#[derive(Encode, Decode)]
struct MyStruct {
    secret_key: Vec<u8>,
    iv: Vec<u8>,
    /// While **encoding**, this field is compressed -> encrypted.
    /// While **decoding**, this field is decrypted -> decompressed.
    #[ender(redir: gzip(9))]
    #[ender(redir: aes(iv, secret_key))]
    super_secret_data: Vec<u8>,
    file_pointer: usize,
    /// Marks the current offset, seeks to `file_pointer` bytes from the start of the file,
    /// encodes/decodes the field, then seeks back.
    #[ender(ptr start: *file_pointer)]
    apple_count: u64,
    /// This field is effectively laid *right after* `file_pointer`
    /// in the binary representation.
    other_data: i32,
}
```
### Ambiguous example:
```rust
#[derive(Encode, Decode)]
/// Because of the priority rules of items over fields, this is ambiguous, see below
#[ender(redir: gzip(9))]
struct MyStruct {
    /// While **encoding**, this field is encrypted -> compressed.
    /// While **decoding**, this field is decompressed -> decrypted.
    /// Because the "compressed" flag is declared before the "encrypted" flag, one might
    /// think they are applied in that order. However, since the item-level flag takes priority,
    /// it is applied *after* the field flag.
    ///
    /// According to your needs, this might not be what you want, so be careful when mixing
    /// item-level and field-level stream modifiers.
    #[ender(redir: aes(&[], &[]))]
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
`fn<T: Write>(&V, &mut ender::Encoder<T>) -> EncodingResult<()>`
where `V` is the type of the field (the function is allowed to be generic over `V`).<br>
    * If the scope is Decode, the path must be callable as
`fn<T: Read>(&mut ender::Encoder<T>) -> EncodingResult<V>`
where `V` is the type of the field (the function is allowed to be generic over `V`).<br>
    * If no scope is specified, the path must point to a module with encoding and decoding functions
with the same signatures as above.
### Example:
```rust
#[derive(Encode, Decode)]
struct Friends {
    /// Has Serialize/Deserialize implementations, but no Encode/Decode implementations.
    /// A perfect fit for integrating with serde!
    #[ender(serde)]
    uuid: Uuid,
    /// Here we demonstrate how the with flag changes based on whether a scope
    /// is declared. This:
    #[ender(with: person_encoder)]
    friend1: Person,
    /// ...is equivalent to this!
    #[ender(en; with: person_encoder::encode)]
    #[ender(de; with: person_encoder::decode)]
    friend2: Person,
    /// Not the smartest way to store a private key!
    private_key: Vec<u8>,
    public_key: Vec<u8>,
    /// This block of data will be encrypted before being encoded using the public key,
    /// and decrypted after being decoded using the private key.
    #[ender(with: rsa(public_key, private_key))]
    even_more_secret_data: Vec<u8>,
}
mod person_encoder {
     pub fn encode<T: Write>(person: &Person, encoder: &mut Encoder<T>) -> EncodingResult<()> {
         /* ... */
     }
     pub fn decode<T: Read>(encoder: &mut Encoder<T>) -> EncodingResult<Person> {
         /* ... */
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
The conversion is done through the `Into` trait.
* `from: $ty` - Converts the value of the field to `$ty` before encoding it
and back to the original field type after decoding it.<br>
The conversion is done through the `From` trait.
### Example:
```rust
#[derive(Encode, Decode)]
struct Mountain {
    /// Height is encoded as a `u16`, then decoded back to a `f64`.
    /// These operations are performed using the `as` keyword.
    #[ender(as: u16)]
    height: f64,
    /// Boulder is encoded as a `BigRock`, then decoded back to a `Boulder`.
    /// This can be done because `BigRock` implements `From<Boulder>`, and
    /// `Boulder` implements `From<BigRock>`.
    #[ender(into: BigRock)]
    boulder: Boulder,
}

/// Note: `BigRock` is required to implement `Encode` and `Decode`,
/// but `Boulder` is not.
#[derive(Encode, Decode)]
struct BigRock {
    weight: f64
}

/* From<Boulder> and From<BigRock> impls here... */
```
# 5. Helpers
Helper flags change certain parameters or add conditions for when a field
or item should be encoded/decoded.<br>
* `crate: $crate` - Overwrites the default crate name which is assumed to be `ender`.
Can only be applied to items.
* `if: $expr` - The field will only be encoded/decoded if the given expression
evaluates to true, otherwise the default value is computed.
* `default: $expr` - Overrides the default fallback for when a value can't be
decoded, which is `Default::default()`
* `skip` - Will not encode/decode this field.
When decoding, computes the default value.
* `validate: $expr, $format_string, $arg1, $arg2, $arg3, ...` - Before encoding/after decoding, returns an error if the
expression evaluates to false. The error message will use the given formatting (if present).
* `flatten: $expr` - Indicates that the length of the given field (for example
a Vec or HashMap) doesn't need to be encoded/decoded, because it is known from the context.
Can also be used with an `Option` in conjunction with the `if` flag and without the `$expr`
to indicate that the presence of an optional value is known from the context.
* `borrow: $lif1, $lif2, $lif3, ...` - This is a `borrow` flag. Indicates this field
should be decoded using its borrowing decode implementation, and allows you to optionally specify a
set of lifetimes to override those normally inferred by the macro. These lifetimes will be bound
to the lifetime of the encoder's data.
* `goto $seek: $expr` - This is a `seek` flag. Indicates a jump to a different stream position
before encoding this field or item.
`$seek` can be any of "start", "end" or "cur", while `$expr` must produce a value of
type usize or isize relative to `$seek`.<br>
If you need the stream position to be restored after encoding/decoding the field, see the
`ptr` *stream modifier`.
* `pos_tracker: $ident` - This is a `seek` flag. Stores the current stream position in a
variable with the given name.
Note that the position is stored *before* the `ptr` and `goto` flags, if any.
* `pull $user as $ty: $var <= $expr` - Attempts to retrieve the `$user` field from the context and
downcast it to `$ty`, early returning an error if it fails.
The reference is available to the `$expr` through the `$user` pattern, which is executed and its value
stored in a local variable `$val`.
This flag is applied *before* `push`
* `push $user as $ty: $expr` - Attempts to retrieve the `user` field from the context and
downcast it to `$ty`, early returning an error if it fails.
The reference is available to the `$expr` through the `$user` pattern, which is executed and its value
ignored.
* `seeking` - This is a `seek` flag. Does nothing, but simply forces a seeking impl to be used.
This can only be applied to the whole item, as it doesn't make sense on individual fields.
<br>
### Example:

```rust
/// Hehe >:3
extern crate ender as enderman;

#[derive(Encode, Decode)]
/// We specify the name of the re-exported ender crate.
#[ender(crate: enderman)]
/// We specify this should use a seeking impl
/// This is redundant of course, but we include it for completeness :P
#[ender(seeking)]
struct PersonEntry<'record> {
  /// Will come in handy later
  name_present: bool,
  /// Similar to the previous example, but with the addition of the flatten flag!
  /// We know a Uuid is always 16 bytes long, so we omit writing/reading that data.
  #[ender(serde; flatten size: 16)]
  uuid: Uuid,
  /// Just the string version of the uuid, not present in the binary data.
  /// Skip the Encoding step, and Decode it from the uuid.
  #[ender(skip; default: uuid.to_string())]
  uuid_string: String,
  /// We know whether this is present from the context, therefore we don't write whether
  /// the optional value is present, and when reading we assume it is.
  /// Since the "if" flag is also present, the field will only be decoded if the expression
  /// evaluates to true, making the previous operation safe
  /// (no risk of decoding garbage data)
  #[ender(flatten bool: true; if: * name_present)]
  name: Option<String>,
  /// Contains a file offset to the rest of the data
  pointer_to_data: usize,
  /// Go to the location in the specified file offset from this point onwards.
  ///
  /// This might be too long to clone from the decoder, so we borrow it instead.
  /// Decode impl -> Cow::Owned -- (NOT YET - WILL WORK WHEN SPECIALIZATION IS STABILIZED)
  /// BorrowDecode impl -> Cow::Borrowed
  /// The macro will infer the borrow lifetime to be `'record`.
  #[ender(goto start: *pointer_to_data; borrow)]
  criminal_record: Cow<'record, str>,
  /// Only present if the name is also present, but we want to provide a custom default!
  #[ender(default: String::from("Smith"); if: * name_present)]
  surname: String,
  /// No-one allowed before 18!
  #[ender(validate: * age >= 18, "User is too young: {}", age)]
  age: u32,
  /// This is temporary data, we don't care about including it in the binary format.
  #[ender(skip; default: 100)]
  health: u64,
}
```
# Relationship between seek flags

```rust
#[derive(Encode, Decode)]
/// This is the same...
struct Ptr {
    pointer: usize,
    #[ender(ptr start: *pointer)]
    data: /* ... */
}

#[derive(Encode, Decode)]
/// As this!
struct Goto {
    pointer: usize,
    #[ender(pos_tracker: current)]
    #[ender(goto start: *pointer)]
    data: /* ... */
    #[ender(goto start: current)]
    seek_back: (),
}
```