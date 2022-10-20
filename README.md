ocaml-multihash
---------------

Multihashes are self-describing hashes. That means that they encode both the hash digest
of some value and also the digest length and the hash function identifier as per the [multicodec](https://github.com/patricoferris/ocaml-multicodec).

```ocaml
# #require "multihash";;
# #require "digestif.ocaml";;
# #require "multihash-digestif";;
```

## Usage

The following section explains how to use `multihash` and `multihash-digestif`.

### Multihash 

The multihash library is implementation-agnostic. It provides a functor for creating a new module which can actually perform the digesting of values. This might be useful because OCaml has quite a few hashing libraries including:

 - [digestif](https://ocaml.org/p/digestif)
 - [hacl-star](https://ocaml.org/p/hacl-star)

```ocaml
# #show_module_type Multihash.Hasher;;
module type Hasher =
  sig
    val digest :
      Multicodec.multihash ->
      Cstruct.t -> (Cstruct.t, [ `Msg of string | `Unsupported ]) result
    val is_supported : Multicodec.multihash -> bool
  end
```

An implementation must provide a means to digest a value and also a function detailing which hash functions are supported by the implementation.

### Multihash-digestif

`multihash-digestif` is a multihash library implemented with [digestif](https://ocaml.org/p/digestif). Note this library will force you to decide between the C and OCaml implementation of digestif just like any other library with a digestif dependency. 

```ocaml
module Md = Multihash_digestif
let s = "Merkle–Damgård"
```

It might be nice to alias the library as we've done here to `Md`. Creating a new multihash from some string is as simple as.

```ocaml
# let v = Md.of_string `Sha2_256 s |> Result.get_ok;;
val v : Md.t = <abstr>
```

Having now digested the data, it can be converted to the full sequence of bytes. We `write` the bytes out as a `Cstruct.t`.

```ocaml
# let data = Md.write v ;;
val data : Cstruct.t = {Cstruct.buffer = <abstr>; off = 0; len = 34}
# let () = hexdump data;;
12 20 41 dd 7b 64 43 54  2e 75 70 1a a9 8a 0c 23
59 51 a2 8a 0d 85 1b 11  56 4d 20 02 2a b1 1d 25
89 a8
```

And of course the hash function used and the length are recoverable from the data by reading it back in.

```ocaml
# let v = Md.read data |> Result.get_ok;;
val v : Md.t = <abstr>
# let (ident, length, digest) = 
  let ident = Multicodec.multihash_to_string (Md.get_hash v) in
  (ident, Md.get_length v, Md.get_digest v);;
val ident : string = "sha2-256"
val length : int = 32
val digest : Cstruct.t = {Cstruct.buffer = <abstr>; off = 2; len = 32}
# let () = hexdump digest;;
41 dd 7b 64 43 54 2e 75  70 1a a9 8a 0c 23 59 51
a2 8a 0d 85 1b 11 56 4d  20 02 2a b1 1d 25 89 a8
```
