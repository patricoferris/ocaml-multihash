type t = int

val encode : t -> Cstruct.t
(** Encode an integer as a varint *)

val decode : Cstruct.t -> t * int
(** Deocode an integer as a varint, also returns length read to decode *)
