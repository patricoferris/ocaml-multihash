(*
 * Copyright (c) 2021 Patrick Ferris <patrick@sirref.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let () = Alcotest.run "multihash-digestif" [ ("encoding", Test_encode.tests) ]

type _ s = Bytes : Bytes.t s | String : String.t s | Bigstring : bigstring s

and bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let title : type a k. [ `HMAC | `Digest ] -> k Digestif.hash -> a s -> string =
 fun computation hash input ->
  let pp_computation ppf = function
    | `HMAC -> Fmt.string ppf "hmac"
    | `Digest -> Fmt.string ppf "digest"
  in
  let pp_hash : type k. k Digestif.hash Fmt.t =
   fun ppf -> function
    | Digestif.MD5 -> Fmt.string ppf "md5"
    | Digestif.SHA1 -> Fmt.string ppf "sha1"
    | Digestif.RMD160 -> Fmt.string ppf "rmd160"
    | Digestif.SHA224 -> Fmt.string ppf "sha224"
    | Digestif.SHA256 -> Fmt.string ppf "sha256"
    | Digestif.SHA384 -> Fmt.string ppf "sha384"
    | Digestif.SHA512 -> Fmt.string ppf "sha512"
    | Digestif.SHA3_224 -> Fmt.string ppf "sha3_224"
    | Digestif.SHA3_256 -> Fmt.string ppf "sha3_256"
    | Digestif.KECCAK_256 -> Fmt.string ppf "keccak_256"
    | Digestif.SHA3_384 -> Fmt.string ppf "sha3_384"
    | Digestif.SHA3_512 -> Fmt.string ppf "sha3_512"
    | Digestif.WHIRLPOOL -> Fmt.string ppf "whirlpool"
    | Digestif.BLAKE2B -> Fmt.string ppf "blake2b"
    | Digestif.BLAKE2S -> Fmt.string ppf "blake2s"
  in
  let pp_input : type a. a s Fmt.t =
   fun ppf -> function
    | Bytes -> Fmt.string ppf "bytes"
    | String -> Fmt.string ppf "string"
    | Bigstring -> Fmt.string ppf "bigstring"
  in
  Fmt.str "%a:%a:%a" pp_computation computation pp_hash hash pp_input input

let bytes = Bytes
let string = String
let bigstring = Bigstring

let test_digest : type k a. a s -> k Digestif.hash -> a -> k Digestif.t -> unit
    =
 fun kind hash input expect ->
  let title = title `Digest hash kind in
  let test_hash = Alcotest.testable (Digestif.pp hash) (Digestif.equal hash) in
  match kind with
  | Bytes ->
      let result = Digestif.digesti_bytes hash (fun f -> f input) in
      Alcotest.(check test_hash) title expect result
  | String ->
      let result = Digestif.digesti_string hash (fun f -> f input) in
      Alcotest.(check test_hash) title expect result
  | Bigstring ->
      let result = Digestif.digesti_bigstring hash (fun f -> f input) in
      Alcotest.(check test_hash) title expect result

let x = test_digest string Digestif.blake2b "Hello World"
