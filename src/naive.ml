(** NOTE This is a naive implementation of the COBS algorithm as described
    with linked lists. It is useful for understanding how the algorithm works
    conceptually, but is very inefficient due to its liberal use of non-tail
    recursion and list appends. *)
open! Base
open Shared

type t = int list

let to_list = List.to_list
let of_list = List.of_list

(* From wikipedia's description of the linked list conception of algorithm:
 *
 * > First, insert a zero byte at the beginning of the packet, and after every run of
 * > 254 non-zero bytes. This encoding is obviously reversible. It is not necessary
 * > to insert a zero byte at the end of the packet if it happens to end with exactly
 * > 254 non-zero bytes.
 * >
 * > Second, replace each zero byte with the offset to the next zero byte, or the
 * > end of the packet. Because of the extra zeros added in the first step, each
 * > offset is guaranteed to be at most 255. *)

let insert_zeroes xs =
  let rec inserter count = function
    | [] -> []
    | 0 :: xs -> 0 :: inserter 0 xs
    | x :: xs ->
      if count = 254 then
        0 :: inserter 0 (x :: xs)
      else
        x :: inserter (count + 1) xs
  in
  (* We omit insertion of a leading 0, to make the encoding cleaner *)
  inserter 0 xs

let%test "inserting zeroes" =
  let nums = List.range 1 (255 * 2) |> insert_zeroes in
  let num_zeroes = List.count ~f:Int.(equal 0) nums in
  num_zeroes = 2
  &&
  List.equal Int.equal (insert_zeroes [0]) [0]
  &&
  List.equal Int.equal (insert_zeroes []) []

let encode ?(delim=0) bytes =
  let is_not_delim = Int.(<>) delim in
  let rec encoder bytes =
    let chunk, rest = List.split_while ~f:is_not_delim bytes in
    let count = List.length chunk + 1 in
    match rest with
    | _ :: rest' -> count :: chunk @ encoder rest'
    | []         -> count :: chunk @ [0]
  in
  bytes
  |> insert_zeroes
  |> encoder

let%test "encodings" =
  let (=) = List.equal equal in
  encode [00] = [01;01;00]
  &&
  encode [00;00] = [01;01;01;00]
  &&
  encode [11;22;00;33] = [03;11;22;02;33;00]
  &&
  encode [11;22;33;44] = [05;11;22;33;44;00]
  &&
  encode [11;00;00;00] = [02;11;01;01;01;00]

let decode ?(delim=0) bytes =
  let check_terminal x =
    if x <> delim then raise (Not_terminated_by_delim x)
  in
  let rec decoder = function
    | []  -> []
    | code :: xs ->
      match List.split_n xs (code - 1) with
      | chunk, [x] -> check_terminal x;
        chunk
      | chunk, rest ->
        if code < max_packet_size then
          chunk @ [delim] @ decoder rest
        else
          chunk @ decoder rest
  in
  decoder bytes

let%test "decodings" =
  let (=) = List.equal Int.equal in
  decode [01;01;00] = [00]
  &&
  decode [01;01;01;00] = [00;00]
  &&
  decode [03;11;22;02;33;00] = [11;22;00;33]
  &&
  decode [05;11;22;33;44;00] = [11;22;33;44]
  &&
  decode [02;11;01;01;01;00] = [11;00;00;00]
