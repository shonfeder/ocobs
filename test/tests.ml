open! Base
open QCheck

let num_zeroes = List.count ~f:(fun x -> x = 0)

module Test (Coder : Ocobs.Coder) = struct
  let byte_gen = Gen.(int_bound 255)
  let bytes_gen = Gen.(list byte_gen |> map Coder.of_list)
  let arbitrary_bytes =
    let print t = t |> Coder.to_list |> Print.(list int) in
    let shrink t =
      t
      |> Coder.to_list
      |> Shrink.(list ~shrink:int)
      |> Iter.map Coder.of_list
    in
    (make bytes_gen ~print ~shrink)

  let test_bytes name = Test.make ~count:1000 ~name arbitrary_bytes

  let tests = [
    test_bytes "int encoded bytes are 8 bit"
      begin fun bs ->
        let int_is_8_bit = Int.between ~low:0 ~high:255 in
        let encoded = Coder.(bs |> encode |> to_list) in
        List.for_all encoded ~f:int_is_8_bit
      end
    ;
    test_bytes "encoded bytes only contain a single zero"
      begin fun bs ->
        bs
        |> Coder.encode
        |> Coder.to_list
        |> num_zeroes
        |> Int.equal 1
      end
    ;
    test_bytes "decoding of encoding is identity on bytes"
      begin fun bs ->
        let initial_bytes = Coder.to_list bs in
        let coded_bytes = Coder.(bs |> encode |> decode |> to_list) in
        List.equal ~equal:Int.equal coded_bytes initial_bytes
      end
  ]
end

module Naive = Test (Ocobs.Naive)
module Seqs = Test (Ocobs.Seqs)

let test_suite name tests =
  let suite = List.map ~f:QCheck_alcotest.to_alcotest tests in
  (name, suite)

let () =
  Alcotest.run "ocobs tests"
    [
      test_suite "naive" Naive.tests;
      test_suite "sequences" Seqs.tests;
    ]
