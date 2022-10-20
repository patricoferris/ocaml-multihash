module Uvarint = Uvarint

module type Hasher = sig
  val digest :
    Multicodec.multihash ->
    Cstruct.t ->
    (Cstruct.t, [ `Unsupported | `Msg of string ]) result

  val iter :
    Multicodec.multihash ->
    ((Cstruct.t -> unit) -> unit) ->
    (Cstruct.t, [ `Unsupported | `Msg of string ]) result

  val is_supported : Multicodec.multihash -> bool
end

module Make (H : Hasher) = struct
  type t = { ident : Multicodec.multihash; length : int; digest : Cstruct.t }

  let is_supported = H.is_supported
  let v ident length digest = { ident; length; digest }

  let of_cstruct ident v =
    Result.map
      (fun digest ->
        let length = Cstruct.length digest in
        { ident; length; digest })
      (H.digest ident v)

  let of_string ident v = of_cstruct ident (Cstruct.of_string v)
  let get_hash { ident; _ } = ident
  let get_length { length; _ } = length
  let get_digest { digest; _ } = digest
  let ( <+> ) = Cstruct.append

  let write { ident; length; digest } =
    let ident = Uvarint.encode (Multicodec.multihash_to_code ident) in
    let length = Uvarint.encode length in
    ident <+> length <+> digest

  let read buff =
    let l = Cstruct.length buff in
    let ident, len = Uvarint.decode buff in
    let length, len' = Uvarint.decode (Cstruct.sub buff len (l - len)) in
    match Multicodec.multihash_of_code ident with
    | Some ident ->
        Ok
          {
            ident;
            length;
            digest = Cstruct.sub buff (len + len') (l - len - len');
          }
    | None ->
        Error
          (`Msg ("Unknown idenfitifer for multihash: " ^ string_of_int ident))

  let pp ppf { ident; length; digest } =
    Format.fprintf ppf "ident(%s) length(%i) digest(%a)"
      (Multicodec.multihash_to_string ident)
      length Cstruct.hexdump_pp digest

  let equal a b =
    a.ident = b.ident && a.length = b.length && Cstruct.equal a.digest b.digest
end
