type outcome = Scoring.outcome = Hit of int | Miss | Skipped

type phase =
  | Menu
  | Prompt of string
  | Drawing of { word : string; time_left : float }
  | Judging of { word : string; time_left : float }
  | Result of {
      word : string;
      guesses : (string * float) list;
      outcome : outcome;
      time_left : float;
      points : int;
    }
  | Final

type round_record = {
  word : string;
  outcome : outcome;
  time_left : float;
  points : int;
}

type state = {
  config : Config.t;
  phase : phase;
  words_left : string list;
  history : round_record list;
  score : int;
}

type event =
  | Start
  | Ready
  | Tick of float
  | Submit
  | Skip
  | Guesses of (string * float) list
  | Next

(* ---------- Creating a game ---------- *)

(* The first word that appears twice (after normalizing), if any. *)
let find_duplicate words =
  let rec go seen = function
    | [] -> None
    | w :: rest ->
        let n = Scoring.normalize w in
        if List.mem n seen then Some w else go (n :: seen) rest
  in
  go [] words

let init (config : Config.t) ~words =
  if config.rounds <= 0 then invalid_arg "Game.init: rounds must be positive";
  if not (Float.is_finite config.time_limit && config.time_limit > 0.0) then
    invalid_arg "Game.init: time_limit must be finite and positive";
  if words = [] then invalid_arg "Game.init: word list is empty";
  if List.exists (fun w -> Scoring.normalize w = "") words then
    invalid_arg "Game.init: word list contains a blank word";
  (match find_duplicate words with
  | Some w -> invalid_arg (Printf.sprintf "Game.init: duplicate word %S" w)
  | None -> ());
  let n = List.length words in
  if n < config.rounds then
    invalid_arg
      (Printf.sprintf "Game.init: %d rounds need at least %d words, got %d"
         config.rounds config.rounds n);
  { config; phase = Menu; words_left = words; history = []; score = 0 }

(* Fisher-Yates shuffle on a copy of the list. All randomness comes from the
   caller's [rng], so the same seed always gives the same order. *)
let shuffle rng xs =
  let a = Array.of_list xs in
  for i = Array.length a - 1 downto 1 do
    let j = Random.State.int rng (i + 1) in
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  done;
  Array.to_list a

(* ---------- Queries ---------- *)

let is_over st = match st.phase with Final -> true | _ -> false
let rounds_played st = List.length st.history

let correct_count st =
  List.length
    (List.filter (fun (r : round_record) -> r.outcome = Hit 0) st.history)

(* ---------- Transitions ---------- *)

(* Start the next round, or end the game if the rounds or words ran out. *)
let next_round st =
  match st.words_left with
  | w :: rest when rounds_played st < st.config.rounds ->
      { st with phase = Prompt w; words_left = rest }
  | _ -> { st with phase = Final }

(* Record a finished round and show its result. *)
let finish_round st ~word ~guesses ~outcome ~time_left ~points =
  let record = { word; outcome; time_left; points } in
  {
    st with
    phase = Result { word; guesses; outcome; time_left; points };
    history = record :: st.history;
    score = st.score + points;
  }

(* Score the classifier's guesses for [word] and finish the round. *)
let judge st ~word ~time_left guesses =
  let rank = Scoring.rank_of ~target:word guesses in
  let outcome = Scoring.outcome_of_rank rank in
  let points = Scoring.score_round st.config ~rank ~time_left in
  finish_round st ~word ~guesses ~outcome ~time_left ~points

(* Whether the classifier's top guess is [word]. *)
let top_guess_is word = function
  | (label, _) :: _ -> Scoring.normalize label = Scoring.normalize word
  | [] -> false

let update st ev =
  match (st.phase, ev) with
  | Menu, Start -> next_round st
  | Prompt word, Ready ->
      { st with phase = Drawing { word; time_left = st.config.time_limit } }
  | Drawing { word; time_left }, Tick dt ->
      (* [dt > 0.0] is false for NaN too, so bad ticks are ignored. *)
      if dt > 0.0 then
        let time_left = time_left -. dt in
        if time_left <= 0.0 then
          { st with phase = Judging { word; time_left = 0.0 } }
        else { st with phase = Drawing { word; time_left } }
      else st
  | Drawing { word; time_left }, Submit ->
      { st with phase = Judging { word; time_left } }
  | Drawing { word; time_left }, Skip when st.config.allow_skip ->
      finish_round st ~word ~guesses:[] ~outcome:Skipped ~time_left ~points:0
  | Drawing { word; time_left }, Guesses gs when top_guess_is word gs ->
      (* Live guessing: the AI got it mid-drawing, so end the round now. *)
      judge st ~word ~time_left gs
  | Judging { word; time_left }, Guesses gs -> judge st ~word ~time_left gs
  | Result _, Next -> next_round st
  | _ -> st

(* ---------- Summary ---------- *)

let outcome_to_string = function
  | Hit 0 -> "Correct"
  | Hit r -> Printf.sprintf "Top 3 (#%d)" (r + 1)
  | Miss -> "Missed"
  | Skipped -> "Skipped"

let summary st =
  let header =
    Printf.sprintf "%2s  %-10s  %-12s  %9s  %6s" "#" "Word" "Result" "Time left"
      "Points"
  in
  let row i (r : round_record) =
    Printf.sprintf "%2d  %-10s  %-12s  %8.1fs  %6d" (i + 1) r.word
      (outcome_to_string r.outcome)
      r.time_left r.points
  in
  let rows = List.mapi row (List.rev st.history) in
  let outcomes = List.map (fun (r : round_record) -> r.outcome) st.history in
  let footer =
    Printf.sprintf "Score: %d   Correct: %d/%d   Accuracy: %.0f%%" st.score
      (correct_count st) (rounds_played st)
      (100.0 *. Scoring.accuracy outcomes)
  in
  String.concat "\n" ((header :: rows) @ [ footer ])
