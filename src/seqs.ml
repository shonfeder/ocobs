open! Base
module S = Sequence
open Shared

type t = int S.t

let to_list = S.to_list
let of_list = S.of_list

module Byte = struct
  type t = int
  [@@deriving eq, sexp]
end

module Count = struct
  type t = int
  [@@deriving eq, sexp]
end

module Precoded = struct
  exception Unexpected_delim

  type data =
    | Data of Byte.t
    | Delim
  [@@deriving eq, sexp]

  type t = {count: Count.t; data: data}
  [@@deriving eq, sexp]

  let make_delim count = {count; data = Delim}
  let make_data count data = {count; data = Data data}

  let make ~delim ~count ~byte =
      let mod_count = Int.rem count max_packet_size in
      if byte = delim || mod_count = 0 then
        {count = mod_count; data = Delim}
      else
        {count = mod_count; data = Data byte}

  let is_delim = function
    | {data = Delim; _} -> true
    | _ -> false

  let is_data = function
    | {data = Data _; _} -> true
    | _ -> false

  let data_exn {data; _} =
    match data with
    | Data d -> d
    | Delim -> raise Unexpected_delim
end

let precoded delim bytes =
  let open S.Generator in
  let open S.Generator.Let_syntax
  in
  let rec precode_generator count bytes =
    let count = count + 1
    in
    match S.next bytes with
    | None ->
      (* Always suffix a terminating delimiter *)
      yield Precoded.(make_delim count)
    | Some (b, bytes') ->
      if b = delim && count < max_packet_size then
        let%bind () = yield Precoded.(make_delim count) in
        precode_generator 0 bytes'
      else if count = max_packet_size then
        let%bind () = yield Precoded.(make_delim count) in
        (* NOTE We reuse the previous generator state here with an incremented
           count. This corresponds to our insertion of an overhead byte *)
        precode_generator 0 bytes
      else
        let%bind () = yield Precoded.(make_data count b) in
        precode_generator count bytes'
  in
  bytes
  |> precode_generator 0
  |> run

let%test "precoded byte sequence identifies delimiting terms" =
  let (=) = List.equal Precoded.equal in
  let expected = Precoded.[
      {count = 1; data = Delim};
      {count = 1; data = Data 1};
      {count = 2; data = Data 2};
      {count = 3; data = Delim};
      {count = 1; data = Data 3};
      {count = 2; data = Delim}  (* The precoding adds a delim at the end *)
    ]
  and actual = [0;1;2;0;3] |> S.of_list |> precoded 0 |> S.to_list
  in
  expected = actual

let%test "precoded byte sequence adds delims" =
  let bytes = List.range 1 (max_packet_size * 2) in
  let precoded = bytes |> S.of_list |> precoded 0 |> S.to_list in
  let expected_delim = List.nth precoded 254 |> Option.value_exn in
  (* expected_delim |> Precoded.sexp_of_t |> Sexp.to_string_hum |> Stdio.print_endline; *)
  Precoded.equal_data expected_delim.data Delim
  &&
  expected_delim.count = 255

let iter_chunk ~f =
  let open S.Generator in
  S.iter_m ~bind ~return ~f

let split_while ~f seq =
  (S.take_while ~f seq, S.drop_while_option ~f seq)

let%test "split_while" =
  let (=) = List.equal Int.equal in
  let input = [1;2;3;4;5;6] in
  let expected = [1;2;3] in
  let expected_divider = 4 in
  let expected_rest = [5;6] in
  let (actual, actual_rest) =
    input
    |> S.of_list
    |> split_while ~f:(Int.(>) 4)
  in
  let divider, rest = Option.value_exn actual_rest
  in
   S.to_list actual = expected
  &&
  Int.equal divider expected_divider
  &&
  S.to_list rest = expected_rest

let encode ?(delim=0) bytes =
  let open S.Generator in
  let open S.Generator.Let_syntax
  in
  let yield_data data = yield (Precoded.data_exn data) in
  let yield_chunk = iter_chunk ~f:yield_data
  in
  (* Compare with the [encoder] in {!Naive.encode} *)
  let rec encoded_generator coded =
    let chunk, rest = split_while ~f:Precoded.is_data coded in
    match rest with
    | None ->
      yield delim (* The precoding guarantees we always terminate with an empty chunk *)
    | Some (delim, rest) ->
      yield delim.count >>= fun () -> yield_chunk chunk >>= fun () -> encoded_generator rest
  in
  bytes
  |> precoded delim
  |> encoded_generator
  |> run

let%test "encodings" =
  let encode bs = bs |> S.of_list |> encode |> S.to_list in
  let (=) = List.equal Int.equal
  in
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
  let open S.Generator in
  let open S.Generator.Let_syntax
  in
  let check_terminal = function
    | Some x when x <> delim -> raise (Not_terminated_by_delim x)
    | None -> raise (Not_terminated_by_delim delim)
    | _ -> ()
  in
  let yield_chunk = iter_chunk ~f:yield
  in
  (* Compare with the [decoder] in {!Naive.decode} *)
  let rec decode_generator bytes =
    match S.next bytes with
    | None -> return ()
    | Some (code, bytes') ->
      let (chunk, rest) = S.split_n bytes' (code - 1) in
      let chunk = S.of_list chunk in
      match S.bounded_length rest ~at_most:1 with
      | `Is 1 -> check_terminal (S.hd rest);
        yield_chunk chunk
      | `Is _ | `Greater ->
        if code < max_packet_size then
          yield_chunk chunk
          >>= fun () -> yield delim
          >>= fun () -> decode_generator rest
        else
          yield_chunk chunk
          >>= fun () -> decode_generator rest
  in
  bytes
  |> decode_generator
  |> run

let%test "decodings" =
  let decode bs = bs |> S.of_list |> decode |> S.to_list in
  let (=) = List.equal Int.equal
  in
  decode [01;01;00] = [00]
  &&
  decode [01;01;01;00] = [00;00]
  &&
  decode [03;11;22;02;33;00] = [11;22;00;33]
  &&
  decode [05;11;22;33;44;00] = [11;22;33;44]
  &&
  decode [02;11;01;01;01;00] = [11;00;00;00]
