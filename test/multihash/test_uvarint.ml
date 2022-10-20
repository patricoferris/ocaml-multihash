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

open Multihash

let cstruct = Alcotest.testable Cstruct.hexdump_pp Cstruct.equal

let list_to_cstruct xs =
  let open Cstruct in
  let buf = create (List.length xs) in
  List.iteri (set_uint8 buf) xs;
  buf

(* Borrowed from https://w3c-ccg.github.io/multihash/index.xml#rfc.section.2.1.1 *)
let encodings =
  [
    (1, list_to_cstruct [ 0x01 ]);
    (127, list_to_cstruct [ 0x7F ]);
    (128, list_to_cstruct [ 0x80; 0x01 ]);
    (255, list_to_cstruct [ 0xFF; 0x01 ]);
    (300, list_to_cstruct [ 0xAC; 0x02 ]);
    (16384, list_to_cstruct [ 0x80; 0x80; 0x01 ]);
  ]

let test_encode i expect () =
  let encoding = Uvarint.encode i in
  Alcotest.(check cstruct) "same cstruct" expect encoding

let test_decode cstruct expect () =
  let decoding, _ = Uvarint.decode cstruct in
  Alcotest.(check int) "same int" expect decoding

let tests =
  let encoding =
    List.map
      (fun (i, c) -> (Fmt.str "encode_%i" i, `Quick, test_encode i c))
      encodings
  in
  let decoding =
    List.map
      (fun (i, c) -> (Fmt.str "decode_%i" i, `Quick, test_decode c i))
      encodings
  in
  encoding @ decoding
