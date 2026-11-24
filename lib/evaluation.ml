(** Evaluation of a predictor on labeled held-out examples. *)

type class_result = {
  label : string;
  total : int;
  correct : int;
  accuracy : float;
}

type result = {
  total : int;
  correct : int;
  accuracy : float;
  average_prediction_seconds : float;
  by_class : class_result list;
}

type error =
  | Invalid_repeats of int
  | Empty_testing_set
  | Invalid_test_sample of Knn.error
  | Prediction_error of Knn.error

module LabelMap = Map.Make (String)

type class_tally = { total : int; correct : int }

(** [error_to_string error] is a readable description of an evaluation error. *)
let error_to_string = function
  | Invalid_repeats repeats ->
      Printf.sprintf "repeats must be positive (got %d)" repeats
  | Empty_testing_set -> "evaluation set must not be empty"
  | Invalid_test_sample error ->
      "invalid test sample: " ^ Knn.error_to_string error
  | Prediction_error error -> "prediction failed: " ^ Knn.error_to_string error

(** [validate sample] returns an error if [sample] has an empty label or an
    invalid feature vector. *)
let validate (sample : Knn.sample) =
  if String.trim sample.label = "" then Error Knn.Empty_label
  else
    match Knn.squared_distance sample.features sample.features with
    | Error _ as error -> error
    | Ok _ -> Ok ()

(** [measure ~repeats ~predict features] returns the final predicted label and
    total elapsed time for [repeats] calls to [predict]. *)
let measure ~repeats ~predict features =
  let started = Unix.gettimeofday () in
  let rec repeat remaining last_prediction =
    if remaining = 0 then Ok last_prediction
    else
      match predict features with
      | Error error -> Error (Prediction_error error)
      | Ok label -> repeat (remaining - 1) label
  in
  match repeat repeats "" with
  | Error _ as error -> error
  | Ok prediction ->
      let elapsed = Unix.gettimeofday () -. started in
      Ok (prediction, elapsed)

(** [add_result label prediction tallies] adds one observation for [label]. *)
let add_result label prediction tallies =
  let previous =
    Option.value
      (LabelMap.find_opt label tallies)
      ~default:{ total = 0; correct = 0 }
  in
  let is_correct = prediction = label in
  LabelMap.add label
    {
      total = previous.total + 1;
      correct = (previous.correct + if is_correct then 1 else 0);
    }
    tallies

(** [evaluate ~repeats ~predict examples] evaluates top-label predictions on
    [examples]. Timing repeats each prediction, but accuracy counts each example
    once. *)
let evaluate ?(repeats = 1) ~predict (examples : Knn.sample list) =
  if repeats <= 0 then Error (Invalid_repeats repeats)
  else if examples = [] then Error Empty_testing_set
  else
    let rec evaluate_examples correct elapsed tallies = function
      | [] -> Ok (correct, elapsed, tallies)
      | (sample : Knn.sample) :: rest -> (
          match validate sample with
          | Error error -> Error (Invalid_test_sample error)
          | Ok () -> (
              match measure ~repeats ~predict sample.features with
              | Error _ as error -> error
              | Ok (prediction, sample_elapsed) ->
                  let is_correct = prediction = sample.label in
                  let correct = correct + if is_correct then 1 else 0 in
                  let tallies = add_result sample.label prediction tallies in
                  evaluate_examples correct
                    (elapsed +. sample_elapsed)
                    tallies rest))
    in
    match evaluate_examples 0 0. LabelMap.empty examples with
    | Error _ as error -> error
    | Ok (correct, elapsed, tallies) ->
        let total = List.length examples in
        let to_class_result label tally =
          {
            label;
            total = tally.total;
            correct = tally.correct;
            accuracy = float_of_int tally.correct /. float_of_int tally.total;
          }
        in
        let by_class =
          LabelMap.bindings tallies
          |> List.map (fun (label, tally) -> to_class_result label tally)
        in
        Ok
          {
            total;
            correct;
            accuracy = float_of_int correct /. float_of_int total;
            average_prediction_seconds =
              elapsed /. float_of_int (total * repeats);
            by_class;
          }
