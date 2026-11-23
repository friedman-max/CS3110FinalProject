type outcome = Hit of int | Miss | Skipped

let normalize s = String.lowercase_ascii (String.trim s)

let rank_of ~target guesses =
  let target = normalize target in
  (* Walk the list, counting positions, until a label matches. *)
  let rec go i = function
    | [] -> None
    | (label, _score) :: rest ->
        if normalize label = target then Some i else go (i + 1) rest
  in
  go 0 guesses

let outcome_of_rank = function
  | Some r when r >= 0 && r <= 2 -> Hit r
  | _ -> Miss

(* Clamp [t] into [0, limit]. NaN would slip through both comparisons, so it is
   treated as 0 explicitly. *)
let clamp_time limit t =
  if Float.is_nan t || t < 0.0 then 0.0 else if t > limit then limit else t

let time_bonus (c : Config.t) time_left =
  if not (Float.is_finite c.time_limit && c.time_limit > 0.0) then 0
  else
    let t = clamp_time c.time_limit time_left in
    let bonus = float_of_int c.time_bonus_max *. (t /. c.time_limit) in
    int_of_float (Float.round bonus)

let score_round (c : Config.t) ~rank ~time_left =
  match rank with
  | Some 0 -> c.points_top1 + time_bonus c time_left
  | Some (1 | 2) -> c.points_top3
  | _ -> 0

let accuracy outcomes =
  match outcomes with
  | [] -> 0.0
  | _ ->
      let hits = List.length (List.filter (fun o -> o = Hit 0) outcomes) in
      float_of_int hits /. float_of_int (List.length outcomes)
