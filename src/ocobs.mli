(** > Consistent Overhead Byte Stuffing (COBS) is a framing method for binary
    > streams and is useful any time you need to send binary datagrams over a
    > stream interface (TCP socket / Serial Port / Etc). In a nutshell, COBS works
    > by stripping all `delimiter` bytes (usually `0x00`) out of a binary packet
    > and places a single `delimiter` at the end, allowing recipients to simply
    > read from the stream until a `delimiter` is encountered (effectively
    > allowing a 'readline' like interface for binary data). The encoding/decoding
    > are very fast and encoding is guaranteed to only add 1 + max(1, (len/255))
    > overhead bytes (making decoding extremely deterministic). For an in-depth
    > breakdown of the algorithm, please see
    > https://en.wikipedia.org/wiki/Consistent_Overhead_Byte_Stuffing

    Copied from {{: https://github.com/keyme/nim_cobs} nim_cobs}
*)

open! Base

(** A coder can encode and decode a collections of bytes.
    The bytes are currently just represented by [int]s.
*)
module type Coder = sig
  (** The base type of a collection of bytes.

      The type is left opaque in the specification, so that implementations of
      [Coder] have the option to represent the bytes in any way they wish.
  *)
  type t

  (** [encode ?delim bytes] is the cobs encoding of the [bytes], using the
      delimiter [delim]. [delim] defaults to [0].
  *)
  val encode : ?delim:int -> t -> t

  (** [decode ?delim bytes] is the decoding of the cobs encoded [bytes], using
      the delimiter [delim]. [delim] defaults to [0].
  *)
  val decode : ?delim:int -> t -> t

  val of_list : int list -> t
  val to_list : t -> int list
end

(** A naive implementation of the COBS encoding algorithm as described with
    linked lists. It is useful for understanding how the algorithm works
    conceptually, but is very inefficient due to its liberal use of non-tail
    recursion and list appends.

    Real world usage should probably favor {!Seqs}.

    This implementation expose its underlying representation.
*)
module Naive : Coder with type t = int list

(** An implementation of the COBS encoding algorithm using [Base]'s
    {!Sequence.t}. This implementation nearly matches the {!Naive} in clarity
    (when squinting to blur out the bind operators, the encoding and decoding
    algorithms are essentially the same), but the codings should only take time
    linear on the length of the input. Additionally, it computes its bytes on
    demand, making it a good fit for operating on streams of data

    This implementation expose its underlying representation.
*)
module Seqs : Coder with type t = int Sequence.t
