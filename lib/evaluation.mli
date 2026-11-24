type class_result = {
  label : string;
  total : int;
  correct : int;
  accuracy : float;
}
(** Held-out accuracy for one category. *)

type result = {
  total : int;
  correct : int;
  accuracy : float;
  average_prediction_seconds : float;
  by_class : class_result list;
}
(** Overall held-out accuracy and prediction timing, with per-category results.
    [average_prediction_seconds] averages all repeated predictor calls. *)

(** Errors that can occur while evaluating a predictor. *)
type error =
  | Invalid_repeats of int
  | Empty_testing_set
  | Invalid_test_sample of Knn.error
  | Prediction_error of Knn.error

val error_to_string : error -> string
(** [error_to_string error] is a readable description of [error]. *)

val evaluate :
  ?repeats:int ->
  predict:(float array -> (string, Knn.error) Stdlib.result) ->
  Knn.sample list ->
  (result, error) Stdlib.result
(** [evaluate ~repeats ~predict examples] evaluates the top label returned by
    [predict] for each labeled sample in [examples]. Each example is counted
    once for accuracy. [repeats] calls are timed per example, and their average
    duration is reported.

    Returns [Error Empty_testing_set] for an empty list,
    [Error (Invalid_repeats n)] when [n] is nonpositive, or an error if a sample
    or prediction is invalid. *)
