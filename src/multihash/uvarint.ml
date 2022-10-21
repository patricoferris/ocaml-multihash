(* Simple variable length integers *)
let max_varint_len_64 = 10

(* Uses the go-lang implementation: https://golang.org/src/encoding/binary/varint.go *)
type t = int

let byte t = t lor 0x80

let put_uvarint buff v =
  let i = ref 0 in
  let x = ref v in
  while !x >= 0x80 do
    Cstruct.set_uint8 buff !i (byte !x);
    x := !x asr 7;
    incr i
  done;
  Cstruct.set_uint8 buff !i !x;
  !i + 1

let encode t =
  let buff = Cstruct.create max_varint_len_64 in
  let bytes_written = put_uvarint buff t in
  Cstruct.sub buff 0 bytes_written

let decode buff =
  let x = ref 0 in
  let s = ref 0 in
  let rec loop i =
    if i >= max_varint_len_64 then (!x, i)
    else
      let b = Cstruct.get_uint8 buff i in
      if b < 0x80 then
        if i == max_varint_len_64 - 1 && b > 1 then (!x, i)
        else (!x lor (b lsl !s), i)
      else (
        x := !x lor ((b land 0x7f) lsl !s);
        s := !s + 7;
        loop (i + 1))
  in
  let r, i = loop 0 in
  (r, i + 1)

let decode_string (buff : string) =
  let x = ref 0 in
  let s = ref 0 in
  let rec loop i =
    if i >= max_varint_len_64 then (!x, i)
    else
      let b = String.get_uint8 buff i in
      if b < 0x80 then
        if i == max_varint_len_64 - 1 && b > 1 then (!x, i)
        else (!x lor (b lsl !s), i)
      else (
        x := !x lor ((b land 0x7f) lsl !s);
        s := !s + 7;
        loop (i + 1))
  in
  let r, i = loop 0 in
  (r, i + 1)
