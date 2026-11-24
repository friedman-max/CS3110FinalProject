open OUnit2
open Quicksketch

(** [sample label features] makes one labeled drawing. *)
let sample label features : Knn.sample = { label; features }

let training =
  [
    sample "cat" [| 0.; 0. |];
    sample "cat" [| 0.2; 0. |];
    sample "tree" [| 1.; 1. |];
  ]

(** [test_manhattan_distance _] checks a 3-4 offset has Manhattan distance 7. *)
let test_manhattan_distance _ =
  assert_equal (Ok 7.)
    (Knn_experiments.distance Knn_experiments.Manhattan [| 0.; 0. |]
       [| 3.; 4. |])

(** [test_weighted_vote_prefers_near_label _] checks a nearby class gets the
    largest inverse-distance vote share. *)
let test_weighted_vote_prefers_near_label _ =
  let result =
    Knn_experiments.predict ~k:3 ~metric:Knn_experiments.Euclidean
      ~voting:Knn_experiments.Inverse_distance ~training [| 0.1; 0. |]
  in
  match result with
  | Ok (best :: _) -> assert_equal "cat" best.label
  | Ok [] -> assert_failure "expected a ranked prediction"
  | Error error -> assert_failure (Knn.error_to_string error)

(** [test_exact_match_weight _] checks exact matches win inverse-distance voting
    without dividing by zero. *)
let test_exact_match_weight _ =
  let result =
    Knn_experiments.predict ~k:3 ~metric:Knn_experiments.Euclidean
      ~voting:Knn_experiments.Inverse_distance ~training [| 0.; 0. |]
  in
  match result with
  | Ok (best :: _) ->
      assert_equal "cat" best.label;
      assert_equal 1. best.score
  | Ok [] -> assert_failure "expected a ranked prediction"
  | Error error -> assert_failure (Knn.error_to_string error)

(** [test_invalid_k _] checks nonpositive neighbor counts return an error. *)
let test_invalid_k _ =
  assert_equal (Error (Knn.Invalid_k 0))
    (Knn_experiments.predict ~k:0 ~metric:Knn_experiments.Euclidean
       ~voting:Knn_experiments.Uniform ~training [| 0.; 0. |])

(** [test_euclidean_uniform_matches_baseline _] checks the experimental
    Euclidean/uniform configuration agrees with the existing classifier. *)
let test_euclidean_uniform_matches_baseline _ =
  let query = [| 0.1; 0. |] in
  let baseline = Knn.predict ~k:3 ~training query in
  let experiment =
    Knn_experiments.predict ~k:3 ~metric:Knn_experiments.Euclidean
      ~voting:Knn_experiments.Uniform ~training query
  in
  match (baseline, experiment) with
  | Ok baseline, Ok experiment ->
      let baseline =
        List.map
          (fun (prediction : Knn.prediction) ->
            ( prediction.label,
              prediction.vote_fraction,
              prediction.nearest_distance ))
          baseline
      in
      let experiment =
        List.map
          (fun (prediction : Knn_experiments.prediction) ->
            (prediction.label, prediction.score, prediction.nearest_distance))
          experiment
      in
      assert_equal baseline experiment
  | Error error, _ | _, Error error ->
      assert_failure (Knn.error_to_string error)

(** [test_evaluation_accuracy _] checks correct and per-category totals. *)
let test_evaluation_accuracy _ =
  let testing = [ sample "cat" [| 0.; 0. |]; sample "tree" [| 1.; 1. |] ] in
  let predict features =
    match
      Knn_experiments.predict ~k:1 ~metric:Knn_experiments.Euclidean
        ~voting:Knn_experiments.Uniform ~training features
    with
    | Ok (best :: _) -> Ok best.label
    | Ok [] -> Error Knn.Empty_training_set
    | Error error -> Error error
  in
  match Evaluation.evaluate ~repeats:2 ~predict testing with
  | Ok result ->
      assert_equal 2 result.correct;
      assert_equal 1. result.accuracy;
      assert_equal 2 (List.length result.by_class)
  | Error error -> assert_failure (Evaluation.error_to_string error)

let suite =
  "knn_experiments"
  >::: [
         "manhattan" >:: test_manhattan_distance;
         "weighted_vote" >:: test_weighted_vote_prefers_near_label;
         "exact_match" >:: test_exact_match_weight;
         "invalid_k" >:: test_invalid_k;
         "baseline_compatibility" >:: test_euclidean_uniform_matches_baseline;
         "evaluation" >:: test_evaluation_accuracy;
       ]

let () = run_test_tt_main suite
