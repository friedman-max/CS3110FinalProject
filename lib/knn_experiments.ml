(** Experimental k-nearest-neighbor prediction options. *)

type metric = Euclidean | Manhattan
type voting = Uniform | Inverse_distance
type prediction = { label : string; score : float; nearest_distance : float }

(** [is_finite x] is [true] exactly when [x] is neither infinite nor NaN. *)
let is_finite x =
  match classify_float x with
  | FP_nan | FP_infinite -> false
  | FP_normal | FP_subnormal | FP_zero -> true

(** [distance metric left right] is the distance between two feature vectors, or
    an error if either vector is empty, non-finite, or has the wrong length. *)
let distance metric left right =
  match metric with
  | Euclidean -> (
      match Knn.squared_distance left right with
      | Error _ as error -> error
      | Ok squared_distance ->
          let distance = sqrt squared_distance in
          if is_finite distance then Ok distance else Error Knn.Non_finite_value
      )
  | Manhattan ->
      let expected = Array.length left in
      let actual = Array.length right in
      if expected = 0 || actual = 0 then Error Knn.Empty_vector
      else if expected <> actual then
        Error (Knn.Dimension_mismatch { expected; actual })
      else
        let rec sum_differences index sum =
          if index = expected then Ok sum
          else if not (is_finite left.(index) && is_finite right.(index)) then
            Error Knn.Non_finite_value
          else
            let difference = abs_float (left.(index) -. right.(index)) in
            let sum = sum +. difference in
            if is_finite difference && is_finite sum then
              sum_differences (index + 1) sum
            else Error Knn.Non_finite_value
        in
        sum_differences 0 0.

(** [ranking_distance metric left right] is the value used to order neighbors.
    Squared Euclidean distance preserves Euclidean ranking and avoids computing
    a square root for every training example. *)
let ranking_distance metric left right =
  match metric with
  | Euclidean -> Knn.squared_distance left right
  | Manhattan -> distance Manhattan left right

(** [ordinary_distance metric distance] converts a ranking distance to the
    distance used for reporting and voting. *)
let ordinary_distance metric distance =
  match metric with Euclidean -> sqrt distance | Manhattan -> distance

module LabelMap = Map.Make (String)

type tally = { total_weight : float; nearest_distance : float }

(** [take n items] is the prefix of [items] containing at most [n] elements. *)
let rec take n items =
  if n <= 0 then []
  else match items with [] -> [] | head :: tail -> head :: take (n - 1) tail

(** [compare_neighbors (d1, l1) (d2, l2)] orders neighbors by increasing
    distance, then alphabetically by label. *)
let compare_neighbors (distance1, label1) (distance2, label2) =
  let by_distance = Float.compare distance1 distance2 in
  if by_distance <> 0 then by_distance else String.compare label1 label2

(** [compare_predictions first second] orders predictions by decreasing vote
    share, then increasing nearest supporting distance, then label. *)
let compare_predictions first second =
  let by_score = Float.compare second.score first.score in
  if by_score <> 0 then by_score
  else
    let by_distance =
      Float.compare first.nearest_distance second.nearest_distance
    in
    if by_distance <> 0 then by_distance
    else String.compare first.label second.label

(** [predict ~k ~metric ~voting ~training query] returns the ranked category
    predictions for [query]. If [k] exceeds the training-set size, every sample
    is selected. In inverse-distance mode, exact matches share all vote weight.
    Returns an error for invalid [k], an empty training set, invalid labels, or
    invalid feature vectors. *)
let predict ?(k = 3) ~metric ~voting ~training query =
  if k <= 0 then Error (Knn.Invalid_k k)
  else if training = [] then Error Knn.Empty_training_set
  else
    let rec calculate_distances acc = function
      | [] -> Ok acc
      | (sample : Knn.sample) :: rest -> (
          if String.trim sample.label = "" then Error Knn.Empty_label
          else
            match ranking_distance metric query sample.features with
            | Error _ as error -> error
            | Ok distance ->
                calculate_distances ((distance, sample.label) :: acc) rest)
    in
    match calculate_distances [] training with
    | Error _ as error -> error
    | Ok neighbors ->
        let nearest_for_ranking =
          neighbors
          |> List.sort compare_neighbors
          |> take (min k (List.length neighbors))
        in
        let nearest =
          List.map
            (fun (distance, label) ->
              (ordinary_distance metric distance, label))
            nearest_for_ranking
        in
        let has_exact_match = List.exists (fun (d, _) -> d = 0.) nearest in
        let nearest_distance =
          List.fold_left
            (fun closest (distance, _) -> min closest distance)
            infinity nearest
        in
        let weight distance =
          match voting with
          | Uniform -> 1.
          | Inverse_distance when has_exact_match ->
              if distance = 0. then 1. else 0.
          | Inverse_distance -> nearest_distance /. distance
        in
        let add_vote votes (distance, label) =
          let vote_weight = weight distance in
          let previous =
            Option.value
              (LabelMap.find_opt label votes)
              ~default:{ total_weight = 0.; nearest_distance = infinity }
          in
          LabelMap.add label
            {
              total_weight = previous.total_weight +. vote_weight;
              nearest_distance = min previous.nearest_distance distance;
            }
            votes
        in
        let votes = List.fold_left add_vote LabelMap.empty nearest in
        let total_weight =
          LabelMap.fold
            (fun _ tally total -> total +. tally.total_weight)
            votes 0.
        in
        let to_prediction label tally =
          {
            label;
            score = tally.total_weight /. total_weight;
            nearest_distance = tally.nearest_distance;
          }
        in
        let predictions =
          votes |> LabelMap.bindings
          |> List.map (fun (label, tally) -> to_prediction label tally)
          |> List.sort compare_predictions
        in
        Ok predictions
