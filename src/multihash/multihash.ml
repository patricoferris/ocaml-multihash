module Uvarint = Uvarint
include Multihash_intf

type _ repr = C : Cstruct.t repr | S : string repr

module Make (H : Hasher) = struct
  type 'a t = {
    ident : Multicodec.multihash;
    repr : 'a repr;
    length : int;
    digest : 'a;
  }

  let is_supported = H.is_supported
  let v ident length (digest : Cstruct.t) = { ident; repr = C; length; digest }

  let make (type a)
      (digest :
        Multicodec.multihash ->
        'a ->
        (a, [ `Msg of string | `Unsupported ]) result) (repr : a repr)
      (length : a -> int) ident v =
    Result.map
      (fun digest ->
        let length = length digest in
        { ident; repr; length; digest })
      (digest ident v)

  let of_cstruct = make H.digest C Cstruct.length
  let iter_cstruct = make H.iter C Cstruct.length
  let of_string = make H.digest_string S String.length
  let iter_string = make H.iter_string S String.length
  let get_hash { ident; _ } = ident
  let get_length { length; _ } = length
  let get_digest { digest; _ } = digest
  let ( <+> ) = Cstruct.append

  let write (type a) ({ ident; repr; length; digest } : a t) : a =
    let ident = Uvarint.encode (Multicodec.multihash_to_code ident) in
    let length = Uvarint.encode length in
    match repr with
    | C -> ident <+> length <+> digest
    | S -> Cstruct.to_string (ident <+> length) ^ digest

  let read_buff buff =
    let l = Cstruct.length buff in
    let ident, len = Uvarint.decode buff in
    let length, len' = Uvarint.decode (Cstruct.sub buff len (l - len)) in
    match Multicodec.multihash_of_code ident with
    | Some ident ->
        Ok
          {
            ident;
            length;
            repr = C;
            digest = Cstruct.sub buff (len + len') (l - len - len');
          }
    | None ->
        Error
          (`Msg ("Unknown idenfitifer for multihash: " ^ string_of_int ident))

  let read_string buff =
    let l = String.length buff in
    let ident, len = Uvarint.decode_string buff in
    let length, len' = Uvarint.decode_string (String.sub buff len (l - len)) in
    match Multicodec.multihash_of_code ident with
    | Some ident ->
        Ok
          {
            ident;
            length;
            repr = S;
            digest = String.sub buff (len + len') (l - len - len');
          }
    | None ->
        Error
          (`Msg ("Unknown idenfitifer for multihash: " ^ string_of_int ident))

  let pp (type a) ppf ({ ident; repr; length; digest } : a t) =
    let digest : Cstruct.t =
      match repr with C -> digest | S -> Cstruct.of_string digest
    in
    Format.fprintf ppf "ident(%s) length(%i) digest(%a)"
      (Multicodec.multihash_to_string ident)
      length Cstruct.hexdump_pp digest

  let equal (type a) (a : a t) (b : a t) =
    a.ident = b.ident && a.length = b.length
    &&
    match a.repr with
    | C -> Cstruct.equal a.digest b.digest
    | S -> String.equal a.digest b.digest

  module Conv = struct
    let to_cstruct t = { t with repr = C; digest = Cstruct.of_string t.digest }
    let to_string t = { t with repr = S; digest = Cstruct.to_string t.digest }
  end
end
