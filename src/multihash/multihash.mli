(** {1: Self-describing Hash Functions}
    
    The multihash library allows you to create hash functions that self-describe.
    This means that just given the digest, the multihash library can tell you the
    length of the digest along with the kind of hash it is (one of {!Multicodec.multihash}).
    
    This can be useful if you are writing libraries that want to be able to handle
    multiple hashing functions in the future (e.g. moving away from SHA1 because it
    is no longer cryptographically secure) or if you want to distribute your hashes
    to someone else along with the content so they can verify the two independently.
    
    For an implementation of this library have a look at {!Multihash_digestif}. *)

include Multihash_intf.Intf
(** @inline *)

module Uvarint = Uvarint
(** Unsigned Variable Length Integers *)
