module Hasher = struct
  open Digestif

  let digest_cstruct hash f v = f v |> to_raw_string hash |> Cstruct.of_string

  type supported =
    [ `Blake2b_64
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
  let string = Digestif.to_raw_string

  let wrap (f : Cstruct.t iter) : bigstring iter =
   fun bs -> f (fun x -> bs (Cstruct.to_bigarray x))

  let make_iter (type v) (module H : Digestif.S with type t = v) (h : v hash)
      (lift : v -> v t) bs =
    H.digesti_bigstring (wrap bs) |> fun t ->
    Digestif.to_raw_string h (lift t) |> Cstruct.of_string |> fun v -> Ok v

  let iter ident bs =
    match ident with
    | `Blake2b_64 -> make_iter (module BLAKE2B) blake2b of_blake2b bs
    | `Blake2s_64 -> make_iter (module BLAKE2S) blake2s of_blake2s bs
    | `Sha1 -> make_iter (module SHA1) sha1 of_sha1 bs
    | `Sha2_256 -> make_iter (module SHA256) sha256 of_sha256 bs
    | `Sha2_512 -> make_iter (module SHA512) sha512 of_sha512 bs
    | `Sha3_224 -> make_iter (module SHA3_224) sha3_224 of_sha3_224 bs
    | `Sha3_256 -> make_iter (module SHA3_256) sha3_256 of_sha3_256 bs
    | `Sha3_384 -> make_iter (module SHA3_384) sha3_384 of_sha3_384 bs
    | `Sha3_512 -> make_iter (module SHA3_512) sha3_512 of_sha3_512 bs
    | `Md5 -> make_iter (module MD5) md5 of_md5 bs
    | `Ripemd_160 -> make_iter (module RMD160) rmd160 of_rmd160 bs
    | `Keccak_256 -> make_iter (module KECCAK_256) keccak_256 of_keccak_256 bs
    | _ -> Error `Unsupported

  let digest (ident : Multicodec.multihash) (v : Cstruct.t) =
    let v = Cstruct.to_bigarray v in
    match ident with
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
