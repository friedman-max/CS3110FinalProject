open OUnit2
open Quicksketch

let cfg = Config.default (* time_limit 20, top1 100, bonus 50, top3 25 *)
let guesses = [ ("cat", 0.6); ("Tree", 0.2); (" cloud ", 0.1); ("sun", 0.1) ]
let show_rank = function None -> "None" | Some i -> "Some " ^ string_of_int i

let rank_test name target gs expected =
  name >:: fun _ ->
  assert_equal ~printer:show_rank expected (Scoring.rank_of ~target gs)

let rank_tests =
  [
    rank_test "first guess" "cat" guesses (Some 0);
    rank_test "fourth guess" "sun" guesses (Some 3);
    rank_test "target case ignored" "CAT" guesses (Some 0);
    rank_test "label case ignored" "tree" guesses (Some 1);
    rank_test "label whitespace ignored" "cloud" guesses (Some 2);
    rank_test "target whitespace ignored" "  sun\t" guesses (Some 3);
    rank_test "absent" "dog" guesses None;
    rank_test "empty guesses" "cat" [] None;
    rank_test "first match wins" "cat"
      [ ("x", 0.5); ("cat", 0.3); ("cat", 0.2) ]
      (Some 1);
  ]

let outcome_tests =
  [
    ( "rank 0" >:: fun _ ->
      assert_equal Scoring.(Hit 0) (Scoring.outcome_of_rank (Some 0)) );
    ( "rank 2" >:: fun _ ->
      assert_equal Scoring.(Hit 2) (Scoring.outcome_of_rank (Some 2)) );
    ( "rank 3" >:: fun _ ->
      assert_equal Scoring.Miss (Scoring.outcome_of_rank (Some 3)) );
    ( "negative" >:: fun _ ->
      assert_equal Scoring.Miss (Scoring.outcome_of_rank (Some (-1))) );
    ( "none" >:: fun _ ->
      assert_equal Scoring.Miss (Scoring.outcome_of_rank None) );
  ]

let score_test ?(config = cfg) name rank time_left expected =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int expected
    (Scoring.score_round config ~rank ~time_left)

let score_tests =
  [
    score_test "top1, full time" (Some 0) 20.0 150;
    score_test "top1, no time left" (Some 0) 0.0 100;
    score_test "top1, half time" (Some 0) 10.0 125;
    (* bonus 2.5 rounds to 3 *)
    score_test "top1, bonus rounds half up" (Some 0) 1.0 103;
    (* bonus 35.75 rounds to 36 *)
    score_test "top1, bonus rounds up" (Some 0) 14.3 136;
    (* bonus 0.4 rounds to 0 *)
    score_test "top1, bonus rounds down" (Some 0) 0.16 100;
    score_test "top1, time above limit clamped" (Some 0) 30.0 150;
    score_test "top1, negative time clamped" (Some 0) (-5.0) 100;
    score_test "top1, NaN time counts as 0" (Some 0) Float.nan 100;
    score_test "top1, infinite time clamped" (Some 0) Float.infinity 150;
    score_test "rank 1, no time bonus" (Some 1) 20.0 25;
    score_test "rank 2, no time bonus" (Some 2) 0.0 25;
    score_test "rank 3" (Some 3) 20.0 0;
    score_test "absent" None 20.0 0;
    score_test "negative rank" (Some (-1)) 20.0 0;
    score_test "zero time limit: no bonus, no crash"
      ~config:{ cfg with time_limit = 0.0 }
      (Some 0) 5.0 100;
    score_test "infinite time limit: no bonus"
      ~config:{ cfg with time_limit = Float.infinity }
      (Some 0) Float.infinity 100;
    score_test "large finite time limit: full bonus"
      ~config:{ cfg with time_limit = Float.max_float }
      (Some 0) Float.max_float 150;
    score_test "custom points"
      ~config:{ cfg with points_top1 = 10; time_bonus_max = 0; points_top3 = 3 }
      (Some 0) 20.0 10;
  ]

let float_eq a b = Float.abs (a -. b) < 1e-9

let acc_test name outcomes expected =
  name >:: fun _ ->
  assert_equal ~cmp:float_eq ~printer:string_of_float expected
    (Scoring.accuracy outcomes)

let accuracy_tests =
  Scoring.
    [
      acc_test "no rounds" [] 0.0;
      acc_test "one hit" [ Hit 0 ] 1.0;
      acc_test "one miss" [ Miss ] 0.0;
      acc_test "top-3 is not a top-1 hit" [ Hit 1; Hit 2 ] 0.0;
      acc_test "mixed, skip counts as played"
        [ Hit 0; Miss; Hit 1; Skipped ]
        0.25;
      acc_test "two of three" [ Hit 0; Hit 0; Miss ] (2.0 /. 3.0);
    ]

let tests =
  "scoring"
  >::: [
         "rank_of" >::: rank_tests;
         "outcome_of_rank" >::: outcome_tests;
         "score_round" >::: score_tests;
         "accuracy" >::: accuracy_tests;
       ]

let () = run_test_tt_main tests
