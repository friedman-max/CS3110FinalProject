(** The scoring rules for the game based on user guesses. *)

(** How one round ended. *)
type outcome =
  | Hit of int
      (** The target was among the AI's top three guesses. The [int] is its
          0-based rank: [Hit 0] means the first guess was right. *)
  | Miss  (** Not in the top three, or the AI made no guesses. *)
  | Skipped  (** The player skipped the word. *)

val normalize : string -> string
(** [normalize s] is [s] with surrounding whitespace removed, in lowercase.
    Labels are compared after normalizing. *)

val rank_of : target:string -> (string * float) list -> int option
(** [rank_of ~target guesses] is [Some i] where [i] is the 0-based position of
    the first guess whose label equals [target], compared case-insensitively and
    ignoring surrounding whitespace. [None] if no guess matches. [guesses] is
    assumed best first. *)

val outcome_of_rank : int option -> outcome
(** [outcome_of_rank r] is [Hit i] if [r = Some i] with [0 <= i <= 2], and
    [Miss] otherwise. *)

val score_round : Config.t -> rank:int option -> time_left:float -> int
(** [score_round c ~rank ~time_left] is the points for one round:
    - rank [Some 0]:
      [c.points_top1 + round (c.time_bonus_max * t / c.time_limit)], where [t]
      is [time_left] clamped into [[0, c.time_limit]] (NaN counts as 0). The
      bonus is 0 if [c.time_limit] is outside the finite positive range.
    - rank [Some 1] or [Some 2]: [c.points_top3], with no time bonus.
    - anything else, including [None]: 0. *)

val accuracy : outcome list -> float
(** [accuracy outcomes] is the fraction of rounds that were first-guess hits
    ([Hit 0]), from 0.0 to 1.0. Skipped rounds count as played. It is 0.0 if
    [outcomes] is empty. *)
