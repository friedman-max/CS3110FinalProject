(* Terminal driver for the game.

   A thin imperative shell around [Game.update]. This is the only file that
   touches the clock, stdin/stdout and randomness; all game rules live in the
   pure [Game] and [Scoring] modules.

   This is the text version of the game: the player "draws" by typing the word
   they drew, and [Stub_classifier.predict] turns that text into guesses in the
   same format as [Classifier.guess]. A windowed version would use
   [Canvas.get_drawing] and [Classifier.guess] instead.

   Run with: dune exec bin/main.exe -- [--seed N] [--rounds N] [--time
   SECONDS] *)

open Quicksketch

(* Raised to leave the game: on "quit", end of input, or Ctrl-C. *)
exception Quit

(* ---------- Output helpers ---------- *)

(* Only use ANSI colors and line clearing when writing to a real terminal. *)
let fancy = Unix.isatty Unix.stdout
let style code s = if fancy then "\027[" ^ code ^ "m" ^ s ^ "\027[0m" else s
let bold = style "1"
let green = style "32"
let yellow = style "33"
let red = style "31"
let dim = style "2"

(* The status line is redrawn in place (for the countdown and typed text).
   [last_drawn] avoids reprinting when nothing changed. *)
let last_drawn = ref ""

let draw_status s =
  if s <> !last_drawn then begin
    print_string ((if fancy then "\r\027[K" else "\r") ^ s);
    flush stdout;
    last_drawn := s
  end

(* Finish the status line so later output starts on a fresh line. *)
let end_status () =
  print_newline ();
  last_drawn := ""

(* ---------- Terminal mode ---------- *)

(* In a terminal, we turn off line buffering and echo so we can read keys while
   the countdown keeps redrawing; we echo typed text ourselves. If stdin is not
   a terminal (piped input), we leave it alone. *)
let saved_tio : Unix.terminal_io option ref = ref None

let enter_raw_mode () =
  if not Sys.win32 && Unix.isatty Unix.stdin then
    try
      let tio = Unix.tcgetattr Unix.stdin in
      saved_tio := Some tio;
      Unix.tcsetattr Unix.stdin Unix.TCSANOW
        { tio with c_icanon = false; c_echo = false; c_vmin = 1; c_vtime = 0 }
    with Unix.Unix_error _ -> ()

let restore_terminal () =
  match !saved_tio with
  | Some tio ->
      (try Unix.tcsetattr Unix.stdin Unix.TCSANOW tio
       with Unix.Unix_error _ -> ());
      saved_tio := None
  | None -> ()

(* ---------- Input ---------- *)

(* What [poll] found: a finished line, end of input, or nothing yet. *)
type input = Line of string | Eof | Nothing

let pending = Buffer.create 32 (* the line being typed *)
let ready_lines : string Queue.t = Queue.create () (* lines ended by Enter *)
let at_eof = ref false

type escape_state = Plain | Escape | Sequence

let escape_state = ref Plain

(* Handle one byte of input as a tiny line editor. *)
let rec feed_char c =
  match (!escape_state, c) with
  | _, '\027' -> escape_state := Escape
  | Escape, ('[' | 'O') -> escape_state := Sequence
  | Sequence, ('@' .. '~') -> escape_state := Plain
  | Sequence, (' ' .. '?') -> ()
  | (Escape | Sequence), _ ->
      (* A standalone Esc or an interrupted sequence leaves this byte intact. *)
      escape_state := Plain;
      feed_char c
  | Plain, '\n' ->
      Queue.push (Buffer.contents pending) ready_lines;
      Buffer.clear pending
  | Plain, ('\127' | '\b') ->
      let n = Buffer.length pending in
      if n > 0 then Buffer.truncate pending (n - 1)
  | Plain, '\004' (* Ctrl-D *) -> if Buffer.length pending = 0 then at_eof := true
  | Plain, c when c >= ' ' -> Buffer.add_char pending c
  | Plain, _ -> () (* other control characters, including '\r' *)

let read_buf = Bytes.create 256

(* Wait up to [timeout] seconds for input. [Unix.select] lets us wait with a
   timeout, so the countdown keeps going without threads. *)
let poll timeout =
  if Queue.is_empty ready_lines && not !at_eof then begin
    let readable =
      match Unix.select [ Unix.stdin ] [] [] timeout with
      | r, _, _ -> r <> []
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> false
    in
    if readable then
      match Unix.read Unix.stdin read_buf 0 (Bytes.length read_buf) with
      | 0 ->
          (* End of input. Keep a last line that had no newline. *)
          if Buffer.length pending > 0 then feed_char '\n';
          at_eof := true
      | n ->
          for i = 0 to n - 1 do
            feed_char (Bytes.get read_buf i)
          done
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
  end;
  if not (Queue.is_empty ready_lines) then Line (Queue.pop ready_lines)
  else if !at_eof then Eof
  else Nothing

(* Normalize a typed line for command matching. *)
let cmd s = String.lowercase_ascii (String.trim s)

(* Show [label], let the player type a line and return it. Raises [Quit] on end
   of input or if they type "quit". *)
let ask label =
  let rec loop () =
    draw_status (label ^ Buffer.contents pending);
    match poll 0.1 with
    | Nothing -> loop ()
    | Eof ->
        end_status ();
        raise Quit
    | Line s ->
        draw_status (label ^ s);
        end_status ();
        if cmd s = "quit" then raise Quit else s
  in
  loop ()

(* ---------- Rendering ---------- *)

let show_menu (config : Config.t) seed =
  print_endline "";
  print_endline
    (Printf.sprintf
       "%d round%s, %.0f seconds each. Draw the word; the AI guesses."
       config.rounds
       (if config.rounds = 1 then "" else "s")
       config.time_limit);
  print_endline
    "No canvas yet: type the word you \"drew\" and press Enter. A blank line \
     is a blank canvas.";
  print_endline
    ((if config.allow_skip then "Type 'skip' to skip a word, " else "Type ")
    ^ "'quit' to quit.");
  print_endline (dim (Printf.sprintf "(seed %d)" seed))

(* A bar that shrinks as time runs out, e.g. [##########----------] 10s. *)
let status_line (config : Config.t) word time_left typed =
  let width = 20 in
  let frac = Float.max 0.0 (time_left /. config.time_limit) in
  let filled = int_of_float (Float.ceil (frac *. float_of_int width)) in
  let filled = min width filled in
  let bar = String.make filled '#' ^ String.make (width - filled) '-' in
  let secs = Printf.sprintf "%2.0fs" (Float.ceil time_left) in
  let color =
    if time_left <= 5.0 then red
    else if time_left <= 10.0 then yellow
    else green
  in
  Printf.sprintf "%s %s  Draw %s > %s"
    (color ("[" ^ bar ^ "]"))
    (color secs) (bold word) typed

let show_result (st : Game.state) =
  match st.phase with
  | Result { word; guesses; outcome; time_left; points } ->
      (match (outcome, guesses) with
      | Skipped, _ -> ()
      | _, [] ->
          print_endline
            "The canvas was blank, so the AI couldn't guess anything."
      | _, _ ->
          print_endline "The AI guessed:";
          List.iteri
            (fun i (label, score) ->
              let line =
                Printf.sprintf "  %d. %-10s %3.0f%%" (i + 1) label
                  (100.0 *. score)
              in
              let matches = Scoring.normalize label = Scoring.normalize word in
              print_endline (if matches then bold line else line))
            guesses);
      let msg =
        match outcome with
        | Hit 0 ->
            green
              (Printf.sprintf
                 "Correct! The AI got %S with %.1fs left. +%d points" word
                 time_left points)
        | Hit r ->
            yellow
              (Printf.sprintf "Close! %S was the AI's guess #%d. +%d points"
                 word (r + 1) points)
        | Miss -> red (Printf.sprintf "The AI didn't get %S. +0 points" word)
        | Skipped -> dim (Printf.sprintf "Skipped %S. +0 points" word)
      in
      print_endline msg;
      print_endline
        (Printf.sprintf "Score: %d  (round %d of %d)" st.score
           (Game.rounds_played st) st.config.rounds)
  | _ -> ()

(* ---------- Game loop ---------- *)

(* Send what the player typed to the classifier stand-in and score it. *)
let judge st typed =
  Game.update st (Game.Guesses (Stub_classifier.predict ~typed))

(* Run one drawing phase: redraw the countdown, read keys, and send [Tick]s with
   the real elapsed time (so the loop speed does not change the round length).
   Returns the state once the round has a result. *)
let drawing_loop (st : Game.state) =
  let config = st.config in
  let rec loop (st : Game.state) last =
    match st.phase with
    | Drawing { word; time_left } -> (
        draw_status
          (status_line config word time_left (Buffer.contents pending));
        let input = poll 0.05 in
        let now = Unix.gettimeofday () in
        let st = Game.update st (Game.Tick (now -. last)) in
        let finish_line s =
          draw_status (status_line config word time_left s);
          end_status ()
        in
        match (st.phase, input) with
        | _, Eof ->
            end_status ();
            raise Quit
        | _, Line s when cmd s = "quit" ->
            finish_line s;
            raise Quit
        | Drawing _, Line s when cmd s = "skip" && config.allow_skip ->
            finish_line s;
            Game.update st Game.Skip
        | Drawing _, Line s ->
            finish_line s;
            judge (Game.update st Game.Submit) s
        | Judging _, Line s ->
            (* Enter was pressed just as time ran out: use what they typed. *)
            finish_line s;
            judge st s
        | Judging _, Nothing ->
            (* Time ran out: judge whatever was typed so far. *)
            let typed = Buffer.contents pending in
            Buffer.clear pending;
            draw_status (status_line config word 0.0 typed);
            end_status ();
            print_endline (red "Time's up!");
            judge st typed
        | Drawing _, Nothing -> loop st now
        | _ -> st)
    | _ -> st
  in
  loop st (Unix.gettimeofday ())

(* Drive a game to the end, offering to play again with a fresh shuffle. *)
let rec run new_game (st : Game.state) =
  match st.phase with
  | Menu ->
      ignore (ask "Press Enter to start ");
      run new_game (Game.update st Game.Start)
  | Prompt word ->
      print_endline "";
      print_endline
        (bold
           (Printf.sprintf "Round %d of %d"
              (Game.rounds_played st + 1)
              st.config.rounds));
      print_endline ("Draw: " ^ bold (String.uppercase_ascii word));
      ignore
        (ask
           (Printf.sprintf "Press Enter when ready (%.0fs) "
              st.config.time_limit));
      run new_game (drawing_loop (Game.update st Game.Ready))
  | Drawing _ -> run new_game (drawing_loop st)
  | Judging _ ->
      run new_game (Game.update st (Game.Guesses [])) (* not reached *)
  | Result _ ->
      show_result st;
      ignore (ask "Press Enter to continue ");
      run new_game (Game.update st Game.Next)
  | Final ->
      print_endline "";
      print_endline (bold "=== Game over ===");
      print_endline (Game.summary st);
      print_endline "";
      if cmd (ask "Play again? (y/n) ") = "y" then
        run new_game (Game.update (new_game ()) Game.Start)

let () =
  let seed = ref None and rounds = ref None and time = ref None in
  let specs =
    [
      ( "--seed",
        Arg.Int (fun n -> seed := Some n),
        "N  Seed for a reproducible word order" );
      ( "--rounds",
        Arg.Int (fun n -> rounds := Some n),
        "N  Rounds per game (default 5)" );
      ( "--time",
        Arg.Float (fun t -> time := Some t),
        "SECONDS  Time limit per round (default 20)" );
    ]
  in
  let usage = "Usage: main.exe [--seed N] [--rounds N] [--time SECONDS]" in
  Arg.parse specs (fun a -> raise (Arg.Bad ("unexpected argument " ^ a))) usage;
  (* Interactive key-by-key input uses POSIX terminal controls. *)
  if Sys.win32 && Unix.isatty Unix.stdin then (
    prerr_endline "Run the interactive terminal demo in WSL: dune exec bin/main.exe";
    exit 2);
  (* Without --seed, pick a random seed but print it so a game can be
     replayed. *)
  let seed =
    match !seed with
    | Some s -> s
    | None -> Random.State.bits (Random.State.make_self_init ())
  in
  let rng = Random.State.make [| seed |] in
  let d = Config.default in
  let config =
    {
      d with
      Config.rounds = Option.value !rounds ~default:d.rounds;
      time_limit = Option.value !time ~default:d.time_limit;
    }
  in
  let new_game () = Game.init config ~words:(Game.shuffle rng config.words) in
  match new_game () with
  | exception Invalid_argument msg ->
      prerr_endline ("Error: " ^ msg);
      exit 2
  | st -> (
      Sys.catch_break true;
      at_exit restore_terminal;
      enter_raw_mode ();
      show_menu config seed;
      try Fun.protect ~finally:restore_terminal (fun () -> run new_game st)
      with Quit | Sys.Break ->
        if !last_drawn <> "" then end_status ();
        print_endline "Bye!")
