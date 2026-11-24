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
  | Drawing { word; _ } ->
      let started = Unix.gettimeofday () in
      (match
         Canvas.get_drawing
           ~prompt:(Printf.sprintf "Draw: %s\nEnter submit  C clear  Q quit" word)
       with
      | None -> ()
      | Some drawing ->
          let elapsed = Unix.gettimeofday () -. started in
          let state = Game.update state (Tick elapsed) in
          play training ~drawing (Game.update state Submit))
  | Judging _ ->
      let guesses = Classifier.guess ~k:3 ~training drawing in
      play training (Game.update state (Guesses guesses))
  | Result { word; guesses; outcome; _ } ->
      let correct = match outcome with Hit _ -> true | Miss | Skipped -> false in
      Canvas.show_result ~target:word ~guesses:(List.map fst guesses) ~correct;
      play training (Game.update state Next)
  | Final ->
      Canvas.render Drawing.empty;
      Canvas.show_text
        (Printf.sprintf "Game over | Score: %d | Rounds played: %d\nEnter close"
           state.score (Game.rounds_played state));
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
  let config = { Config.default with allow_skip = false } in
  let training = load_training config.words in
  let words = Game.shuffle (Random.State.make_self_init ()) config.words in
  let game = Game.init config ~words in
  Canvas.open_window ();
  Fun.protect ~finally:Canvas.close_window (fun () ->
      Graphics.set_window_title "QuickSketch - Canvas Game";
      try play training game with Graphics.Graphic_failure _ -> ())
