open! Base

module type Coder = sig
  type t
  val encode : ?delim:int -> t -> t
  val decode : ?delim:int -> t -> t
  val of_list : int list -> t
  val to_list : t -> int list
end

module Naive = (Naive : Coder with type t = int list)
module Seqs = (Seqs : Coder with type t = int Sequence.t)
