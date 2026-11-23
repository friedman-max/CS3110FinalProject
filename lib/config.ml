type t = {
  rounds : int;
  time_limit : float;
  points_top1 : int;
  time_bonus_max : int;
  points_top3 : int;
  allow_skip : bool;
  words : string list;
}

let default =
  {
    (* At most the number of words, or [Game.init] rejects the config. *)
    rounds = 5;
    time_limit = 20.0;
    points_top1 = 100;
    time_bonus_max = 50;
    points_top3 = 25;
    allow_skip = true;
    (* The five categories in the current QuickDraw dataset
       (CATEGORIES in scripts/prepare_quickdraw.py). Update this list if the
       dataset's categories change. *)
    words = [ "cat"; "tree"; "house"; "bicycle"; "airplane" ];
  }
