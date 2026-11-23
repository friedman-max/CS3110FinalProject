open Quicksketch

let rec wait_for_enter () =
  match Graphics.read_key () with
  | '\r' | '\n' -> true
  | '\027' | 'q' | 'Q' -> false
  | _ -> wait_for_enter ()

let rec play (state : Game.state) =
  match state.phase with
  | Menu -> play (Game.update state Start)
  | Prompt word ->
      Canvas.render Drawing.empty;
      Canvas.show_text
        (Printf.sprintf "Round %d/%d | Draw: %s | Score: %d\nEnter start  Q quit"
           (Game.rounds_played state + 1) state.config.rounds word state.score);
      if wait_for_enter () then play (Game.update state Ready)
  | Drawing { word; _ } ->
      let started = Unix.gettimeofday () in
      (match
         Canvas.get_drawing
           ~prompt:(Printf.sprintf "Draw: %s\nEnter submit  C clear  Q quit" word)
       with
      | None -> ()
      | Some _ ->
          let elapsed = Unix.gettimeofday () -. started in
          let state = Game.update state (Tick elapsed) in
          play (Game.update state Submit))
  | Judging _ ->
      (* Sketch recognition is a separate integration step. *)
      play (Game.update state (Guesses []))
  | Result { word; guesses; outcome; _ } ->
      let correct = match outcome with Hit _ -> true | Miss | Skipped -> false in
      Canvas.show_result ~target:word ~guesses:(List.map fst guesses) ~correct;
      play (Game.update state Next)
  | Final ->
      Canvas.render Drawing.empty;
      Canvas.show_text
        (Printf.sprintf "Game over | Score: %d | Rounds played: %d\nEnter close"
           state.score (Game.rounds_played state));
      print_endline (Game.summary state);
      ignore (wait_for_enter ())

let () =
  let config = { Config.default with allow_skip = false } in
  let words = Game.shuffle (Random.State.make_self_init ()) config.words in
  let game = Game.init config ~words in
  Canvas.open_window ();
  Fun.protect ~finally:Canvas.close_window (fun () ->
      Graphics.set_window_title "QuickSketch - Canvas Game";
      try play game with Graphics.Graphic_failure _ -> ())
