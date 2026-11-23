(** The main game settings such as rounds, time limits, etc. *)

type t = {
  rounds : int;  (** Number of rounds in one game. *)
  time_limit : float;  (** Seconds the player gets to draw each word. *)
  points_top1 : int;  (** Base points when the AI's first guess is right. *)
  time_bonus_max : int;
      (** Extra points for a first-guess hit with the full [time_limit] left;
          scaled linearly by the time remaining. *)
  points_top3 : int;
      (** Points when the target is the AI's second or third guess. *)
  allow_skip : bool;  (** Whether the player may skip a word. *)
  words : string list;
      (** Categories the game draws prompts from. Each must be a label in the
          training data, so the classifier can predict it. Must have at least
          [rounds] entries. *)
}

val default : t
(** [default] is 5 rounds of 20 seconds, 100 points for a first-guess hit plus
    up to 50 time bonus, 25 points for a top-3 hit, skipping allowed, and the
    five categories of the current dataset: cat, tree, house, bicycle and
    airplane. *)
