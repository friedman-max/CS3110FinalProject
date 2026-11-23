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
    (* These two check the current placeholder [Classifier.features]. Update
       them when the normalization step is connected. *)
    ( "placeholder features is the empty vector" >:: fun _ ->
      let d = Drawing.of_strokes [ [ (10, 10); (20, 20) ] ] in
      assert_equal 0 (Array.length (Classifier.features d)) );
    ( "placeholder: non-blank drawing gives no guesses yet" >:: fun _ ->
      let d = Drawing.of_strokes [ [ (10, 10); (20, 20) ] ] in
      assert_guesses [] (Classifier.guess ~k:3 ~training d) );
  ]

let () =
  run_test_tt_main
    ("classifier"
    >::: [
           "of_knn" >::: adapter_tests;
           "in the game" >::: game_tests;
           "guess" >::: guess_tests;
         ])
