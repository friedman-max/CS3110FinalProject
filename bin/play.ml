open Quicksketch

let rec wait_for_enter () =
  match Graphics.read_key () with
  | '\r' | '\n' -> true
  | '\027' | 'q' | 'Q' -> false
  | _ -> wait_for_enter ()

let rec play training ?(drawing = Drawing.empty) (state : Game.state) =
  match state.phase with
  | Menu -> play training (Game.update state Start)
  | Prompt word ->
      Canvas.render Drawing.empty;
      Canvas.show_text
        (Printf.sprintf "Round %d/%d | Draw: %s | Score: %d\nEnter start  Q quit"
           (Game.rounds_played state + 1) state.config.rounds word state.score);
      if wait_for_enter () then play training (Game.update state Ready)
  | Drawing { word; time_left } ->
      let allow_skip = state.config.allow_skip in
      let skip_control = if allow_skip then "  S skip" else "" in
      let prompt seconds =
        Printf.sprintf "Draw: %s | %.0fs left\nEnter submit  C clear%s  Q quit"
          word seconds skip_control
      in
      let result, remaining =
        Canvas.get_timed_drawing ~prompt ~allow_skip ~time_limit:time_left
      in
      let state = Game.update state (Tick (time_left -. remaining)) in
      (match result with
      | Canvas.Quit -> ()
      | Canvas.Skipped -> play training (Game.update state Skip)
      | Canvas.Timed_out drawing -> play training ~drawing state
      | Canvas.Submitted drawing ->
          play training ~drawing (Game.update state Submit))
  | Judging _ ->
      let guesses = Classifier.guess ~k:3 ~training drawing in
      play training (Game.update state (Guesses guesses))
  | Result { word; guesses; outcome; points; time_left } ->
      let labels =
        match guesses with
        | [] -> "(none)"
        | _ -> String.concat ", " (List.map fst guesses)
      in
      Canvas.show_text
        (Printf.sprintf "%s | %s | +%d pts | Score %d | %.1fs left\nGuesses: %s   Enter next  Q quit"
           word (Game.outcome_to_string outcome) points state.score time_left
           labels);
      if wait_for_enter () then play training (Game.update state Next)
  | Final ->
      Canvas.render Drawing.empty;
      let outcomes =
        List.map (fun (round : Game.round_record) -> round.outcome) state.history
      in
      Canvas.show_text
        (Printf.sprintf "Game over | Score: %d | Accuracy: %.0f%% (%d/%d)\nEnter close  Q quit"
           state.score (100. *. Scoring.accuracy outcomes)
           (Game.correct_count state) (Game.rounds_played state));
      print_endline (Game.summary state);
      ignore (wait_for_enter ())

let load_training words =
  let fail message =
    prerr_endline message;
    prerr_endline "Run python3 scripts/prepare_quickdraw.py to prepare the data.";
    exit 1
  in
  match Quickdraw_data.load "data/quickdraw_demo.tsv" with
  | Error message -> fail message
  | Ok { training; _ } ->
      let has_training word =
        List.exists (fun (sample : Knn.sample) -> sample.label = word) training
      in
      match List.find_opt (fun word -> not (has_training word)) words with
      | Some word -> fail (Printf.sprintf "No training drawings for %s." word)
      | None -> training

let () =
  let config = Config.default in
  let training = load_training config.words in
  let words = Game.shuffle (Random.State.make_self_init ()) config.words in
  let game = Game.init config ~words in
  Canvas.open_window ();
  Fun.protect ~finally:Canvas.close_window (fun () ->
      Graphics.set_window_title "QuickSketch - Canvas Game";
      try play training game with Graphics.Graphic_failure _ -> ())
