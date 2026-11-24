open OUnit2
open Quicksketch

(* A k-NN prediction. The adapter ignores [nearest_distance]. *)
let pred label votes vote_fraction : Knn.prediction =
  { label; votes; vote_fraction; nearest_distance = 0.0 }

let sample label features : Knn.sample = { label; features }

let show_guesses gs =
  "["
  ^ String.concat "; "
      (List.map (fun (l, s) -> Printf.sprintf "%s %.3f" l s) gs)
  ^ "]"

let assert_guesses expected actual =
  assert_equal ~printer:show_guesses expected actual

(* Two categories only, so k = 3 can return at most two predictions. *)
let training =
  [
    sample "cat" [| 0.0; 0.0 |];
    sample "cat" [| 0.1; 0.0 |];
    sample "tree" [| 1.0; 1.0 |];
  ]

(* Play one round of a fresh game on "cat" and feed it [guesses]. *)
let judge_cat guesses =
  let cfg = { Config.default with rounds = 1 } in
  let st = Game.init cfg ~words:[ "cat" ] in
  let st = List.fold_left Game.update st [ Start; Ready; Submit ] in
  Game.update st (Guesses guesses)

let outcome_of (st : Game.state) =
  match st.phase with
  | Result { outcome; _ } -> outcome
  | _ -> assert_failure "expected a Result phase"

let adapter_tests =
  [
    ( "ok: keeps order, score is vote_fraction" >:: fun _ ->
      assert_guesses
        [ ("cat", 0.6); ("tree", 0.2); ("house", 0.2) ]
        (Classifier.of_knn
           (Ok [ pred "cat" 3 0.6; pred "tree" 1 0.2; pred "house" 1 0.2 ])) );
    ( "ok: fewer than three predictions" >:: fun _ ->
      assert_guesses
        [ ("cat", 1.0) ]
        (Classifier.of_knn (Ok [ pred "cat" 3 1.0 ])) );
    ( "ok: no predictions" >:: fun _ ->
      assert_guesses [] (Classifier.of_knn (Ok [])) );
    ( "error: no guesses" >:: fun _ ->
      assert_guesses [] (Classifier.of_knn (Error Knn.Empty_training_set));
      assert_guesses []
        (Classifier.of_knn
           (Error (Knn.Dimension_mismatch { expected = 784; actual = 0 }))) );
    ( "real Knn.predict with only two categories" >:: fun _ ->
      let gs = Classifier.of_knn (Knn.predict ~k:3 ~training [| 0.0; 0.0 |]) in
      assert_equal ~printer:string_of_int 2 (List.length gs);
      assert_equal "cat" (fst (List.hd gs)) );
  ]

(* The adapter's output must score correctly in the game. *)
let game_tests =
  [
    ( "error scores as a miss" >:: fun _ ->
      let st = judge_cat (Classifier.of_knn (Error Knn.Empty_vector)) in
      assert_equal Game.Miss (outcome_of st);
      assert_equal 0 st.score );
    ( "two guesses, target second, scores top 3" >:: fun _ ->
      let gs =
        Classifier.of_knn (Ok [ pred "tree" 2 0.67; pred "cat" 1 0.33 ])
      in
      let st = judge_cat gs in
      assert_equal (Game.Hit 1) (outcome_of st);
      assert_equal 25 st.score );
    ( "one guess, target first, scores a hit" >:: fun _ ->
      let st = judge_cat (Classifier.of_knn (Ok [ pred "cat" 3 1.0 ])) in
      assert_equal (Game.Hit 0) (outcome_of st) );
  ]

let guess_tests =
  [
    ( "blank drawing gives no guesses" >:: fun _ ->
      assert_guesses [] (Classifier.guess ~k:3 ~training Drawing.empty) );
    ( "non-blank drawing runs through k-NN" >:: fun _ ->
      let d = Drawing.of_strokes [ [ (10, 10); (20, 20) ] ] in
      let training =
        [ sample "line" (Preprocess.features d); sample "blank" (Array.make 784 0.) ]
      in
      assert_guesses [ ("line", 1.) ] (Classifier.guess ~k:1 ~training d) );
  ]

let preprocess_tests =
  [
    ( "blank is 784 background pixels" >:: fun _ ->
      assert_equal (Array.make 784 0.) (Preprocess.features Drawing.empty) );
    ( "single click becomes centered ink" >:: fun _ ->
      let image = Preprocess.features (Drawing.of_strokes [ [ (0, 279) ] ]) in
      assert_equal 784 (Array.length image);
      assert_bool "valid grayscale" (Array.for_all (fun p -> 0. <= p && p <= 1.) image);
      assert_bool "center has ink" (image.((13 * 28) + 13) > 0.);
      assert_equal 0. image.(0) );
    ( "sparse strokes join and the top edge stays on top" >:: fun _ ->
      let d = Drawing.of_strokes [ [ (30, 30); (30, 230); (230, 230) ] ] in
      let image = Preprocess.features d in
      assert_bool "top middle is ink" (image.((2 * 28) + 14) > 0.);
      assert_equal 0. image.((25 * 28) + 14) );
    ( "separate strokes keep the gap" >:: fun _ ->
      let d = Drawing.of_strokes [ [ (30, 100) ]; [ (230, 100) ] ] in
      let image = Preprocess.features d in
      assert_bool "left dot" (image.((13 * 28) + 2) > 0.);
      assert_bool "right dot" (image.((13 * 28) + 25) > 0.);
      assert_equal 0. image.((13 * 28) + 14) );
    ( "translation and uniform scale preserve features" >:: fun _ ->
      let first = Drawing.of_strokes [ [ (20, 20); (60, 60); (20, 60) ] ] in
      let second = Drawing.of_strokes [ [ (100, 120); (180, 200); (100, 200) ] ] in
      assert_equal (Preprocess.features first) (Preprocess.features second) );
  ]

let () =
  run_test_tt_main
    ("classifier"
    >::: [
           "of_knn" >::: adapter_tests;
           "in the game" >::: game_tests;
           "preprocessing" >::: preprocess_tests;
           "guess" >::: guess_tests;
         ])
