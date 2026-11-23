open OUnit2
open Quicksketch
open Game

(* Small game: 3 rounds of 10 seconds. *)
let cfg = { Config.default with rounds = 3; time_limit = 10.0 }
let words = [ "cat"; "tree"; "sun"; "fish" ]
let fresh () = init cfg ~words
let play st events = List.fold_left update st events

(* Classifier output with [w] as the top guess. *)
let hit w = [ (w, 0.9); ("zzz", 0.06); ("yyy", 0.04) ]

let show_phase = function
  | Menu -> "Menu"
  | Prompt w -> "Prompt " ^ w
  | Drawing { word; time_left } ->
      Printf.sprintf "Drawing %s %.2f" word time_left
  | Judging { word; time_left } ->
      Printf.sprintf "Judging %s %.2f" word time_left
  | Result { word; outcome; time_left; points; _ } ->
      Printf.sprintf "Result %s %s %.2f %d" word
        (outcome_to_string outcome)
        time_left points
  | Final -> "Final"

let assert_phase expected st =
  assert_equal ~printer:show_phase expected st.phase

(* State at the start of round 1's drawing, with the full 10 seconds. *)
let drawing () = play (fresh ()) [ Start; Ready ]

(* ---------- Happy path ---------- *)

(* Three first-guess hits with 8, 10 and 5 seconds left. *)
let happy_events =
  [
    Start;
    Ready;
    Tick 2.0;
    Submit;
    Guesses (hit "cat");
    Next;
    Ready;
    Submit;
    Guesses (hit "tree");
    Next;
    Ready;
    Tick 5.0;
    Submit;
    Guesses (hit "sun");
    Next;
  ]

let happy_tests =
  [
    ( "start shows first word" >:: fun _ ->
      let st = update (fresh ()) Start in
      assert_phase (Prompt "cat") st;
      assert_equal [ "tree"; "sun"; "fish" ] st.words_left );
    ( "ready starts the clock" >:: fun _ ->
      assert_phase (Drawing { word = "cat"; time_left = 10.0 }) (drawing ()) );
    ( "submit keeps remaining time" >:: fun _ ->
      assert_phase
        (Judging { word = "cat"; time_left = 8.0 })
        (play (drawing ()) [ Tick 2.0; Submit ]) );
    ( "full game ends in Final" >:: fun _ ->
      let st = play (fresh ()) happy_events in
      assert_phase Final st;
      assert_bool "is_over" (is_over st);
      (* 100+40, 100+50, 100+25 *)
      assert_equal ~printer:string_of_int 415 st.score;
      assert_equal 3 (rounds_played st);
      assert_equal 3 (correct_count st);
      assert_equal [ "fish" ] st.words_left;
      assert_equal
        [
          { word = "sun"; outcome = Hit 0; time_left = 5.0; points = 125 };
          { word = "tree"; outcome = Hit 0; time_left = 10.0; points = 150 };
          { word = "cat"; outcome = Hit 0; time_left = 8.0; points = 140 };
        ]
        st.history );
    ( "result phase carries the round" >:: fun _ ->
      let gs = hit "cat" in
      let st = play (drawing ()) [ Tick 2.0; Submit; Guesses gs ] in
      assert_phase
        (Result
           {
             word = "cat";
             guesses = gs;
             outcome = Hit 0;
             time_left = 8.0;
             points = 140;
           })
        st;
      assert_equal 140 st.score );
    ( "summary lists rounds and totals" >:: fun _ ->
      let s = summary (play (fresh ()) happy_events) in
      let lines = String.split_on_char '\n' s in
      assert_equal ~printer:string_of_int 5 (List.length lines);
      assert_equal ~printer:Fun.id "Score: 415   Correct: 3/3   Accuracy: 100%"
        (List.nth lines 4);
      assert_bool "first row is cat"
        (String.starts_with ~prefix:" 1  cat" (List.nth lines 1)) );
    ( "is_over is false mid-game" >:: fun _ ->
      assert_bool "not over" (not (is_over (drawing ()))) );
  ]

(* ---------- Round outcomes ---------- *)

let result_of events = play (drawing ()) events

let outcome_tests =
  [
    ( "miss: target ranked 4th" >:: fun _ ->
      let gs = [ ("dog", 0.4); ("sun", 0.3); ("car", 0.2); ("cat", 0.1) ] in
      let st = result_of [ Submit; Guesses gs ] in
      assert_phase
        (Result
           {
             word = "cat";
             guesses = gs;
             outcome = Miss;
             time_left = 10.0;
             points = 0;
           })
        st;
      assert_equal 0 st.score;
      assert_equal 0 (correct_count st) );
    ( "top-3 partial" >:: fun _ ->
      let gs = [ ("dog", 0.5); ("CAT", 0.3); ("sun", 0.2) ] in
      let st = result_of [ Submit; Guesses gs ] in
      assert_phase
        (Result
           {
             word = "cat";
             guesses = gs;
             outcome = Hit 1;
             time_left = 10.0;
             points = 25;
           })
        st;
      assert_equal 25 st.score;
      assert_equal 0 (correct_count st) );
    ( "skip" >:: fun _ ->
      let st = result_of [ Tick 3.0; Skip ] in
      assert_phase
        (Result
           {
             word = "cat";
             guesses = [];
             outcome = Skipped;
             time_left = 7.0;
             points = 0;
           })
        st;
      assert_equal
        [ { word = "cat"; outcome = Skipped; time_left = 7.0; points = 0 } ]
        st.history );
    ( "skip ignored when not allowed" >:: fun _ ->
      let st =
        play (init { cfg with allow_skip = false } ~words) [ Start; Ready ]
      in
      assert_equal st (update st Skip) );
    ( "blank canvas (no guesses) is a miss" >:: fun _ ->
      let st = result_of [ Submit; Guesses [] ] in
      assert_phase
        (Result
           {
             word = "cat";
             guesses = [];
             outcome = Miss;
             time_left = 10.0;
             points = 0;
           })
        st;
      assert_equal 1 (rounds_played st) );
    ( "mixed game totals" >:: fun _ ->
      let st =
        play (fresh ())
          [
            Start;
            Ready;
            Submit;
            Guesses [ ("x", 0.5); ("cat", 0.5) ];
            Next;
            Ready;
            Skip;
            Next;
            Ready;
            Tick 10.0;
            Guesses (hit "sun");
            Next;
          ]
      in
      assert_phase Final st;
      (* 25 + 0 + 100 *)
      assert_equal ~printer:string_of_int 125 st.score;
      assert_equal 1 (correct_count st);
      assert_equal [ Hit 0; Skipped; Hit 1 ]
        (List.map (fun (r : round_record) -> r.outcome) st.history) );
  ]

(* ---------- Timer ---------- *)

let timer_tests =
  [
    ( "ticks count down" >:: fun _ ->
      assert_phase
        (Drawing { word = "cat"; time_left = 1.0 })
        (play (drawing ()) [ Tick 3.0; Tick 3.0; Tick 3.0 ]) );
    ( "expiry over several ticks goes to Judging at 0" >:: fun _ ->
      assert_phase
        (Judging { word = "cat"; time_left = 0.0 })
        (play (drawing ()) [ Tick 3.0; Tick 3.0; Tick 3.0; Tick 3.0 ]) );
    ( "tick landing exactly on 0 expires" >:: fun _ ->
      assert_phase
        (Judging { word = "cat"; time_left = 0.0 })
        (update (drawing ()) (Tick 10.0)) );
    ( "expired round scores with no time bonus" >:: fun _ ->
      let st = play (drawing ()) [ Tick 4.0; Tick 7.0; Guesses (hit "cat") ] in
      assert_equal 100 st.score );
    ( "negative dt ignored" >:: fun _ ->
      let st = drawing () in
      assert_equal st (update st (Tick (-5.0))) );
    ( "zero dt ignored" >:: fun _ ->
      let st = drawing () in
      assert_equal st (update st (Tick 0.0)) );
    ( "NaN dt ignored" >:: fun _ ->
      let st = drawing () in
      assert_equal st (update st (Tick Float.nan)) );
    ( "infinite dt expires" >:: fun _ ->
      assert_phase
        (Judging { word = "cat"; time_left = 0.0 })
        (update (drawing ()) (Tick Float.infinity)) );
  ]

(* ---------- Live guessing (stretch) ---------- *)

let live_tests =
  [
    ( "matching top guess ends round early" >:: fun _ ->
      let gs = hit "Cat" in
      let st = play (drawing ()) [ Tick 4.0; Guesses gs ] in
      assert_phase
        (Result
           {
             word = "cat";
             guesses = gs;
             outcome = Hit 0;
             time_left = 6.0;
             points = 130;
           })
        st );
    ( "non-matching top guess keeps drawing" >:: fun _ ->
      let st = drawing () in
      assert_equal st (update st (Guesses [ ("dog", 0.6); ("cat", 0.4) ])) );
    ( "empty live guesses keep drawing" >:: fun _ ->
      let st = drawing () in
      assert_equal st (update st (Guesses [])) );
  ]

(* ---------- Events in the wrong phase ---------- *)

let all_events =
  [ Start; Ready; Tick 1.0; Submit; Skip; Guesses (hit "cat"); Next ]

(* Every event in [all_events] except [allowed] must leave [st] unchanged. *)
let unchanged_except name st allowed =
  name >:: fun _ ->
  List.iter
    (fun ev ->
      if not (List.mem ev allowed) then begin
        assert_equal ~printer:show_phase st.phase (update st ev).phase;
        assert_equal st (update st ev)
      end)
    all_events

let wrong_phase_tests =
  let menu = fresh () in
  let prompt = update menu Start in
  let drawing = update prompt Ready in
  let judging = update drawing Submit in
  let result = update judging (Guesses (hit "cat")) in
  let final = play menu happy_events in
  [
    unchanged_except "menu" menu [ Start ];
    unchanged_except "prompt" prompt [ Ready ];
    unchanged_except "drawing" drawing
      [ Tick 1.0; Submit; Skip; Guesses (hit "cat") ];
    unchanged_except "judging" judging [ Guesses (hit "cat") ];
    unchanged_except "result" result [ Next ];
    unchanged_except "final" final [];
  ]

(* ---------- init validation ---------- *)

let raises_invalid name f =
  name >:: fun _ ->
  match f () with
  | _ -> assert_failure "expected Invalid_argument"
  | exception Invalid_argument _ -> ()

let init_tests =
  [
    ( "valid init" >:: fun _ ->
      let st = fresh () in
      assert_phase Menu st;
      assert_equal words st.words_left;
      assert_equal [] st.history;
      assert_equal 0 st.score );
    ( "exactly enough words" >:: fun _ ->
      let st = init cfg ~words:[ "a"; "b"; "c" ] in
      assert_equal 3 (List.length st.words_left) );
    raises_invalid "empty list" (fun () -> init cfg ~words:[]);
    raises_invalid "duplicate" (fun () ->
        init cfg ~words:[ "cat"; "sun"; "cat" ]);
    raises_invalid "duplicate up to case and spaces" (fun () ->
        init cfg ~words:[ "cat"; "sun"; " Cat " ]);
    raises_invalid "fewer words than rounds" (fun () ->
        init cfg ~words:[ "cat"; "sun" ]);
    raises_invalid "blank word" (fun () ->
        init cfg ~words:[ "cat"; "  "; "sun"; "tree" ]);
    raises_invalid "zero rounds" (fun () -> init { cfg with rounds = 0 } ~words);
    raises_invalid "zero time limit" (fun () ->
        init { cfg with time_limit = 0.0 } ~words);
    raises_invalid "NaN time limit" (fun () ->
        init { cfg with time_limit = Float.nan } ~words);
    raises_invalid "infinite time limit" (fun () ->
        init { cfg with time_limit = Float.infinity } ~words);
  ]

(* ---------- Shuffle and word order ---------- *)

let ints = List.init 20 Fun.id
let shuffled seed xs = shuffle (Random.State.make [| seed |]) xs

let shuffle_tests =
  [
    ( "is a permutation" >:: fun _ ->
      List.iter
        (fun seed -> assert_equal ints (List.sort compare (shuffled seed ints)))
        (List.init 50 Fun.id) );
    ( "deterministic for a fixed seed" >:: fun _ ->
      assert_equal (shuffled 42 ints) (shuffled 42 ints) );
    ( "different seeds give different orders" >:: fun _ ->
      let orders = List.init 10 (fun seed -> shuffled seed ints) in
      assert_bool "not all the same"
        (List.exists (fun o -> o <> List.hd orders) orders) );
    ("empty list" >:: fun _ -> assert_equal [] (shuffled 1 []));
    ("singleton" >:: fun _ -> assert_equal [ 7 ] (shuffled 1 [ 7 ]));
    ( "does not modify input" >:: fun _ ->
      let xs = [ 1; 2; 3 ] in
      ignore (shuffled 3 xs);
      assert_equal [ 1; 2; 3 ] xs );
  ]

(* Play a whole game, answering each prompt with the right word. *)
let play_through st =
  let rec go st =
    match st.phase with
    | Final -> st
    | Menu -> go (update st Start)
    | Prompt _ -> go (update st Ready)
    | Drawing _ -> go (update st Submit)
    | Judging { word; _ } -> go (update st (Guesses (hit word)))
    | Result _ -> go (update st Next)
  in
  go st

let no_repeat_tests =
  [
    ( "no word repeats across a game" >:: fun _ ->
      let c = Config.default in
      List.iter
        (fun seed ->
          let st = play_through (init c ~words:(shuffled seed c.words)) in
          let played = List.map (fun (r : round_record) -> r.word) st.history in
          assert_equal ~printer:string_of_int c.rounds (List.length played);
          assert_equal (List.length played)
            (List.length (List.sort_uniq compare played)))
        (List.init 30 Fun.id) );
    ( "game ends early if words run out" >:: fun _ ->
      let st = play (fresh ()) [ Start; Ready; Submit; Guesses [] ] in
      let st = { st with words_left = [] } in
      assert_phase Final (update st Next) );
  ]

(* ---------- Text classifier (terminal stand-in) ---------- *)

let stub_tests =
  [
    ("blank" >:: fun _ -> assert_equal [] (Stub_classifier.predict ~typed:""));
    ( "whitespace only" >:: fun _ ->
      assert_equal [] (Stub_classifier.predict ~typed:"  \t") );
    ( "typed word first, three guesses, descending" >:: fun _ ->
      let gs = Stub_classifier.predict ~typed:" dog " in
      assert_equal 3 (List.length gs);
      assert_equal "dog" (fst (List.hd gs));
      let scores = List.map snd gs in
      assert_equal (List.sort (fun a b -> compare b a) scores) scores );
    ( "distractors never repeat typed word" >:: fun _ ->
      let gs = Stub_classifier.predict ~typed:"Cat" in
      assert_equal 3 (List.length gs);
      assert_equal 1
        (List.length
           (List.filter (fun (l, _) -> Scoring.normalize l = "cat") gs)) );
    ( "other guesses are dataset categories" >:: fun _ ->
      let gs = Stub_classifier.predict ~typed:"dog" in
      List.iter
        (fun (l, _) -> assert_bool l (List.mem l Config.default.words))
        (List.tl gs) );
  ]

(* ---------- Defaults ---------- *)

let default_tests =
  let c = Config.default in
  [
    ( "default words are the dataset categories" >:: fun _ ->
      assert_equal [ "cat"; "tree"; "house"; "bicycle"; "airplane" ] c.words );
    ( "default rounds fit the word list" >:: fun _ ->
      assert_bool "rounds <= words" (c.rounds <= List.length c.words);
      assert_equal ~printer:string_of_int 5 c.rounds );
    ( "init accepts the default config" >:: fun _ ->
      let st = init c ~words:c.words in
      assert_phase Menu st );
    ( "a default game plays all five words" >:: fun _ ->
      let st = play_through (init c ~words:c.words) in
      assert_equal ~printer:string_of_int 5 (rounds_played st);
      assert_equal [] st.words_left );
  ]

let tests =
  "game"
  >::: [
         "defaults" >::: default_tests;
         "happy path" >::: happy_tests;
         "outcomes" >::: outcome_tests;
         "timer" >::: timer_tests;
         "live guessing" >::: live_tests;
         "wrong phase" >::: wrong_phase_tests;
         "init" >::: init_tests;
         "shuffle" >::: shuffle_tests;
         "word order" >::: no_repeat_tests;
         "stub classifier" >::: stub_tests;
       ]

let () = run_test_tt_main tests
