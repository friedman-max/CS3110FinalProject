(** Experimental k-nearest-neighbor prediction options. *)

type metric = Euclidean | Manhattan
type voting = Uniform | Inverse_distance
type prediction = { label : string; score : float; nearest_distance : float }

let validate_vector vector =
  if Array.length vector = 0 then Error Knn.Empty_vector
  else if Array.for_all Float.is_finite vector then Ok ()
  else Error Knn.Non_finite_value

let squared_euclidean left right =
  let sum = ref 0. in
  for i = 0 to Array.length left - 1 do
    let difference = left.(i) -. right.(i) in
    sum := !sum +. (difference *. difference)
  done;
  !sum

let manhattan left right =
  let sum = ref 0. in
  for i = 0 to Array.length left - 1 do
    sum := !sum +. abs_float (left.(i) -. right.(i))
  done;
  !sum

(* The query is validated once by the caller. Keep Euclidean distances squared
   until the nearest neighbors are selected. *)
let ranking_distance metric left =
  let sum =
    match metric with Euclidean -> squared_euclidean | Manhattan -> manhattan
  in
  let expected = Array.length left in
  fun right ->
    match validate_vector right with
    | Error _ as error -> error
    | Ok () ->
        let actual = Array.length right in
        if actual <> expected then
          Error (Knn.Dimension_mismatch { expected; actual })
        else
          let distance = sum left right in
          if Float.is_finite distance then Ok distance
          else Error Knn.Non_finite_value

(** [ordinary_distance metric distance] converts a ranking distance to the
    distance used for reporting and voting. *)
let ordinary_distance metric distance =
  match metric with Euclidean -> sqrt distance | Manhattan -> distance

let distance metric left right =
  match validate_vector left with
  | Error _ as error -> error
  | Ok () ->
      Result.map (ordinary_distance metric) (ranking_distance metric left right)

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
  let valid_query =
    if k <= 0 then Error (Knn.Invalid_k k)
    else if training = [] then Error Knn.Empty_training_set
    else validate_vector query
  in
  match valid_query with
  | Error _ as error -> error
  | Ok () ->
    let distance_to_query = ranking_distance metric query in
    let rec calculate_distances acc = function
      | [] -> Ok acc
      | (sample : Knn.sample) :: rest -> (
          if String.trim sample.label = "" then Error Knn.Empty_label
          else
            match distance_to_query sample.features with
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
