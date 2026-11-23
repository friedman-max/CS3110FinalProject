(* Text-based stand-in for Classifier, used by the terminal driver. *)

(* The other guesses come from the dataset's categories, like the labels the
   k-NN classifier can return. *)
let distractors = Config.default.words

let predict ~typed =
  let typed = String.trim typed in
  if typed = "" then []
  else
    let others =
      List.filter
        (fun d -> Scoring.normalize d <> Scoring.normalize typed)
        distractors
    in
    match others with
    | d1 :: d2 :: _ -> [ (typed, 0.80); (d1, 0.12); (d2, 0.08) ]
    (* Only reached if fewer than three categories are configured. *)
    | _ -> [ (typed, 0.80) ]
