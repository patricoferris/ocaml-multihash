module Uvarint = Uvarint
(** Unsigned Variable Length Integers *)

module type Hasher = sig
  val digest :
    Multicodec.multihash ->
    Cstruct.t ->
    (Cstruct.t, [ `Unsupported | `Msg of string ]) result
  (** [digest ident v] should digest [v] using the hash function related to [ident] *)

  val is_supported : Multicodec.multihash -> bool
  (** A subset of {! Multicodec.multihash} that this hasher supports *)
end

module Make : functor (H : Hasher) -> sig
  type t
  (** The type for multihashes *)

  val of_cstruct :
    Multicodec.multihash ->
    Cstruct.t ->
    (t, [ `Unsupported | `Msg of string ]) result
  (** [of_cstruct hash s] constructs a multihash for [s] using hashing strategy [hash]. *)

  val of_string :
    Multicodec.multihash ->
    string ->
    (t, [ `Unsupported | `Msg of string ]) result
  (** Same as {!of_cstruct} only using [string]. *)

  val is_supported : Multicodec.multihash -> bool
  (** Whether this particular multihash library supported a given hash implementation. *)

  val get_hash : t -> Multicodec.multihash
  (** [get_hash v] returns the hashing stategy used for this particular multihash. *)

  val get_length : t -> int
  (** The length of the digest. *)

  val get_digest : t -> Cstruct.t
  (** The hash digest. *)

  val write : t -> Cstruct.t
  (** [write v] returns a buffer filled with the multihash value. *)

  val read : Cstruct.t -> (t, [ `Msg of string ]) result
  (** [read buf] tries to read a multihash from the buffer [buf]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf v] pretty prints a multihash in human-readable format. *)

  val equal : t -> t -> bool
  (** [equal a b] tests if two multihashes are equal. *)
end
