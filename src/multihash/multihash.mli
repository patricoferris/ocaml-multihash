module Uvarint = Uvarint
(** Unsigned Variable Length Integers *)

module type Hasher = sig
  val digest :
    Multicodec.multihash ->
    Cstruct.t ->
    (Cstruct.t, [ `Unsupported | `Msg of string ]) result
  (** [digest ident v] should digest [v] using the hash function related to [ident] *)

  val digest_string :
    Multicodec.multihash ->
    string ->
    (string, [ `Unsupported | `Msg of string ]) result
  (** [digest ident v] should digest [v] using the hash function related to [ident] *)

  val iter :
    Multicodec.multihash ->
    ((Cstruct.t -> unit) -> unit) ->
    (Cstruct.t, [ `Unsupported | `Msg of string ]) result

  val iter_string :
    Multicodec.multihash ->
    ((string -> unit) -> unit) ->
    (string, [ `Unsupported | `Msg of string ]) result
  (** [digest ident v] should digest [v] using the hash function related to [ident] *)

  val is_supported : Multicodec.multihash -> bool
  (** A subset of {! Multicodec.multihash} that this hasher supports *)
end

module Make : functor (H : Hasher) -> sig
  type 'repr t
  (** The type for multihashes *)

  val v : Multicodec.multihash -> int -> Cstruct.t -> Cstruct.t t
  (** [v hash length digest] constructs a new multihash. No checks are made at all 
      on the user-supplied information. *)

  val of_cstruct :
    Multicodec.multihash ->
    Cstruct.t ->
    (Cstruct.t t, [ `Unsupported | `Msg of string ]) result
  (** [of_cstruct hash s] constructs a multihash for [s] using hashing strategy [hash]. *)

  val of_string :
    Multicodec.multihash ->
    string ->
    (string t, [ `Unsupported | `Msg of string ]) result
  (** Same as {!of_cstruct} only using [string]. *)

  val iter_cstruct :
    Multicodec.multihash ->
    ((Cstruct.t -> unit) -> unit) ->
    (Cstruct.t t, [ `Unsupported | `Msg of string ]) result
  (** Like {!of_cstruct} but we digest the value using the iterator function. *)

  val iter_string :
    Multicodec.multihash ->
    ((string -> unit) -> unit) ->
    (string t, [ `Unsupported | `Msg of string ]) result
  (** Like {!of_string} but we digest the value using the iterator function. *)

  val is_supported : Multicodec.multihash -> bool
  (** Whether this particular multihash library supported a given hash implementation. *)

  val get_hash : _ t -> Multicodec.multihash
  (** [get_hash v] returns the hashing stategy used for this particular multihash. *)

  val get_length : _ t -> int
  (** The length of the digest. *)

  val get_digest : 'a t -> 'a
  (** The hash digest. *)

  val write : 'a t -> 'a
  (** [write v] returns a buffer filled with the multihash value. *)

  val read_buff : Cstruct.t -> (Cstruct.t t, [ `Msg of string ]) result
  (** [read buf] tries to read a multihash from the buffer [buf]. *)

  val read_string : string -> (string t, [ `Msg of string ]) result
  (** Like {! read_buff} but for [string]s. *)

  val pp : Format.formatter -> 'a t -> unit
  (** [pp ppf v] pretty prints a multihash in human-readable format. *)

  val equal : 'a t -> 'a t -> bool
  (** [equal a b] tests if two multihashes are equal. *)

  module Conv : sig
    val to_cstruct : string t -> Cstruct.t t
    val to_string : Cstruct.t t -> string t
  end
end
