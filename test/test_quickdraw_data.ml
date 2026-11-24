open OUnit2
open Quicksketch

let with_rows rows check =
  let path, output =
    Filename.open_temp_file ~mode:[ Open_binary ] "quickdraw" ".tsv"
  in
  Fun.protect
    ~finally:(fun () ->
      close_out_noerr output;
      Sys.remove path)
    (fun () ->
      List.iter (fun row -> output_string output (row ^ "\n")) rows;
      close_out output;
      check path (Quickdraw_data.load path))

let pixels first =
  String.concat "," (first :: List.init 783 (fun _ -> "0"))

let row split label first = String.concat "\t" [ split; label; pixels first ]

let test_splits _ =
  with_rows
    [
      row "train" "cat" "255" ^ "\r";
      row "test" "tree" "128";
      row "train" "house" "0";
    ]
    (fun _ -> function
      | Error error -> assert_failure error
      | Ok { training = [ cat; house ]; testing = [ tree ] } ->
          assert_equal "cat" cat.label;
          assert_equal "house" house.label;
          assert_equal "tree" tree.label;
          assert_equal 784 (Array.length cat.features);
          assert_equal 1. cat.features.(0);
          assert_equal 0. cat.features.(1);
          assert_equal (128. /. 255.) tree.features.(0)
      | Ok _ -> assert_failure "unexpected split or row count")

let invalid_row invalid expected _ =
  with_rows [ row "train" "cat" "0"; invalid ] (fun path -> function
    | Ok _ -> assert_failure "expected an invalid row error"
    | Error error -> assert_equal ~printer:Fun.id (path ^ ":2: " ^ expected) error)

let () =
  run_test_tt_main
    ("Quick, Draw! loader"
    >::: [
           "split and pixel conversion" >:: test_splits;
           "split name" >:: invalid_row (row "holdout" "cat" "0") "Expected train or test split";
           "blank label" >:: invalid_row (row "train" "  " "0") "Missing category";
           "pixel count" >:: invalid_row "test\tcat\t0,1" "Expected 784 pixels";
           "pixel range" >:: invalid_row (row "train" "cat" "256") "Expected integer pixels in 0..255";
           "pixel type" >:: invalid_row (row "train" "cat" "nan") "Expected integer pixels in 0..255";
         ])
