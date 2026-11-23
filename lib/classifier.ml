(* Placeholder for the Drawing.t -> 784-float normalization step
   Returning [||] makes Knn.predict return Error Empty_vector, so a round
   is scored as a miss instead of crashing. *)
let features (_ : Drawing.t) : float array = [||]

(* Keep k-NN's ranking and use the vote fraction as the score. An error means
   the classifier could not judge the drawing, so the game gets no guesses. *)
let of_knn = function
  | Ok predictions ->
      List.map
        (fun (p : Knn.prediction) -> (p.label, p.vote_fraction))
        predictions
  | Error _ -> []

let guess ~k ~training drawing =
  (* Blank-canvas rule: nothing drawn means nothing to recognize. *)
  if Drawing.is_empty drawing then []
  else of_knn (Knn.predict ~k ~training (features drawing))
