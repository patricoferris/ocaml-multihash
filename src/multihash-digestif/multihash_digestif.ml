module Hasher = struct
  open Digestif

  let digest_cstruct hash f v = f v |> to_raw_string hash |> Cstruct.of_string

  type supported =
    [ `Identity
    | `Blake2b_64
    | `Blake2s_64
    | `Sha1
    | `Sha2_256
    | `Sha2_512
    | `Sha3_224
    | `Sha3_256
    | `Sha3_384
    | `Sha3_512
    | `Ripemd_160
    | `Keccak_256 ]

  let is_supported = function #supported -> true | _ -> false

  let digest (ident : Multicodec.multihash) (v : Cstruct.t) =
    let v = Cstruct.to_bigarray v in
    match ident with
    | `Identity -> Ok Cstruct.empty
    | `Blake2b_64 -> Ok (digest_cstruct blake2b (digest_bigstring blake2b) v)
    | `Blake2s_64 -> Ok (digest_cstruct blake2s (digest_bigstring blake2s) v)
    | `Sha1 -> Ok (digest_cstruct sha1 (digest_bigstring sha1) v)
    | `Sha2_256 -> Ok (digest_cstruct sha256 (digest_bigstring sha256) v)
    | `Sha2_512 -> Ok (digest_cstruct sha512 (digest_bigstring sha512) v)
    | `Sha3_224 -> Ok (digest_cstruct sha3_224 (digest_bigstring sha3_224) v)
    | `Sha3_256 -> Ok (digest_cstruct sha3_256 (digest_bigstring sha3_256) v)
    | `Sha3_384 -> Ok (digest_cstruct sha3_384 (digest_bigstring sha3_384) v)
    | `Sha3_512 -> Ok (digest_cstruct sha3_512 (digest_bigstring sha3_512) v)
    | `Md5 -> Ok (digest_cstruct md5 (digest_bigstring md5) v)
    | `Ripemd_160 -> Ok (digest_cstruct rmd160 (digest_bigstring rmd160) v)
    | `Keccak_256 ->
        Ok (digest_cstruct keccak_256 (digest_bigstring keccak_256) v)
    | _ -> Error `Unsupported
end

include Multihash.Make (Hasher)
