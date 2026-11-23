(** The game is a value of type {!state}. A driver (terminal, GUI, tests) feeds
    it {!event}s through {!update} and renders whatever {!phase} comes back.
    This module does no I/O, never reads the clock, and uses no global
    randomness: time arrives as [Tick] events and the word order is fixed when
    the state is created. *)

(** How one round ended. Same type as {!Scoring.outcome}. *)
type outcome = Scoring.outcome =
  | Hit of int  (** Target was the AI's guess number [rank + 1] (0..2). *)
  | Miss  (** Not in the top three, or no guesses at all. *)
  | Skipped  (** The player skipped. *)

(** Where the game currently is. *)
type phase =
  | Menu  (** Before the first round. *)
  | Prompt of string  (** Showing the word to draw; waiting for [Ready]. *)
  | Drawing of { word : string; time_left : float }
      (** The player is drawing and the clock is running. *)
  | Judging of { word : string; time_left : float }
      (** Drawing is over; waiting for the classifier's [Guesses]. *)
  | Result of {
      word : string;
      guesses : (string * float) list;
      outcome : outcome;
      time_left : float;
      points : int;
    }  (** The round has been scored; waiting for [Next]. *)
  | Final  (** All rounds are done. *)

type round_record = {
  word : string;
  outcome : outcome;
  time_left : float;  (** Seconds left when the round ended. *)
  points : int;
}
(** One finished round, as kept in the history. *)

type state = {
  config : Config.t;
  phase : phase;
  words_left : string list;
      (** Upcoming words, already shuffled. One is consumed per round. *)
  history : round_record list;  (** Finished rounds, newest first. *)
  score : int;  (** Sum of the points in [history]. *)
}

(** Inputs to the game. *)
type event =
  | Start  (** Leave the menu and show the first word. *)
  | Ready  (** The player has seen the word; start the clock. *)
  | Tick of float  (** [Tick dt]: [dt] seconds of real time have passed. *)
  | Submit  (** The player is done drawing early. *)
  | Skip  (** The player gives up on this word. *)
  | Guesses of (string * float) list
      (** Classifier output, best first. An empty list means nothing was
          recognizable (for example, a blank canvas). *)
  | Next  (** Continue from the result screen. *)

val init : Config.t -> words:string list -> state
(** [init c ~words] is a new game in the [Menu] phase that will use [words] in
    the given order (shuffle them first with {!shuffle}). [c.words] is not read;
    pass it in as [words] if you want it.

    @raise Invalid_argument
    if [c.rounds <= 0], [c.time_limit] is outside the finite positive range,
    [words] is empty, any word is blank, two words are the same (ignoring case
    and surrounding whitespace), or [words] has fewer than [c.rounds] words. *)

val shuffle : Random.State.t -> 'a list -> 'a list
(** [shuffle rng xs] is a uniformly random permutation of [xs]. It is
    deterministic for a given state of [rng], and advances [rng]. *)

val update : state -> event -> state
(** [update st ev] is the state after [ev]. It is total: an event that makes no
    sense in the current phase returns [st] unchanged.

    Transitions:
    - [Menu], [Start]: [Prompt] with the next word.
    - [Prompt w], [Ready]: [Drawing] with the full time limit.
    - [Drawing], [Tick dt]: subtract [dt]; at or below 0 go to [Judging] with
      [time_left = 0]. A [dt] that is not positive (or NaN) is ignored.
    - [Drawing], [Submit]: [Judging], keeping the remaining time.
    - [Drawing], [Skip]: if [allow_skip], [Result] with [Skipped] and 0 points;
      otherwise ignored.
    - [Drawing], [Guesses gs]: live guessing. If the top guess matches the word,
      the round ends early as [Hit 0] with the current time left; otherwise
      ignored.
    - [Judging], [Guesses gs]: [Result], scored with {!Scoring.score_round};
      updates [score] and [history]. [gs = []] scores as [Miss].
    - [Result], [Next]: [Prompt] with the next word, or [Final] once [c.rounds]
      rounds are done or no words remain.
    - [Final] is terminal: to play again, make a new state with {!init}. *)

val is_over : state -> bool
(** [is_over st] is whether [st] is in the [Final] phase. *)

val rounds_played : state -> int
(** [rounds_played st] is the number of finished rounds. *)

val correct_count : state -> int
(** [correct_count st] is the number of finished rounds the AI got on its first
    guess ([Hit 0]). *)

val outcome_to_string : outcome -> string
(** [outcome_to_string o] is a short label for [o], such as ["Correct"],
    ["Top 3 (#2)"], ["Missed"] or ["Skipped"]. *)

val summary : state -> string
(** [summary st] is a multi-line text table of the finished rounds, oldest
    first, with each round's word, result, time left and points, then a final
    line with the total score, correct count and accuracy. *)
