# Ende
Rust Encoding/Decoding library and macros for writing custom protocols and file formats

It's currently working but incomplete. What is there to do?

- [X] Base functionality and macros
- [X] Add String encodings
- [X] Add more Encode/Decode impls for standard types
- [X] Add other num encodings
- [X] Documentation
- [ ] More examples
- [X] Refactor the proc-macro derive code
- [ ] Write tests
- [X] Zero copy Decoding.
- [X] `#[no_std]` support

Other features that might be added:
- [ ] Support for `async`
- [ ] `SeekEncode` and `SeekDecode` traits for formats that require moving backwards and forwards in the data.