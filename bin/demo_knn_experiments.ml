open Quicksketch

let categories = [ "cat"; "tree"; "house"; "bicycle"; "airplane" ]
let dataset_path = "data/quickdraw_demo.tsv"
let training_per_category = 100
let testing_per_category = 25
let pixel_count = 28 * 28

type configuration = {
  name : string;
  k : string;
  metric : string;
  voting : string;
  predict : float array -> (string, Knn.error) result;
}

(** [is_finite x] is [true] exactly when [x] is neither infinite nor NaN. *)
let is_finite x =
  match classify_float x with
  | FP_nan | FP_infinite -> false
  | FP_normal | FP_subnormal | FP_zero -> true

(** [parse_row line_number line] parses one row of the prepared dataset. A row
    contains a split name, label, and 784 byte-valued pixels. *)
let parse_row line_number line =
  let error message =
    Error (Printf.sprintf "line %d: %s" line_number message)
  in
  match String.split_on_char '\t' line with
  | [ split; label; pixels ] -> (
      try
        let bytes =
          pixels |> String.split_on_char ',' |> List.map float_of_string
          |> Array.of_list
        in
        if Array.length bytes <> pixel_count then error "expected 784 pixels"
        else if
          not
            (Array.for_all
               (fun pixel -> is_finite pixel && pixel >= 0. && pixel <= 255.)
               bytes)
        then error "pixel values must be finite numbers from 0 through 255"
        else if String.trim label = "" then error "label must not be empty"
        else
          let features = Array.map (fun pixel -> pixel /. 255.) bytes in
          let sample : Knn.sample = { label; features } in
          match split with
          | "train" -> Ok (`Training sample)
          | "test" -> Ok (`Testing sample)
          | _ -> error "split must be train or test"
      with Failure _ -> error "pixel values must be numbers")
  | _ -> error "expected split, label, and pixel values"

(** [load_dataset filename] loads and validates the project's fixed-size
    training and held-out split from [filename]. *)
let load_dataset filename =
  try
    let input = open_in filename in
    Fun.protect
      ~finally:(fun () -> close_in_noerr input)
      (fun () ->
        let rec read line_number training testing =
          match input_line input with
          | exception End_of_file -> Ok (List.rev training, List.rev testing)
          | line when String.trim line = "" ->
              read (line_number + 1) training testing
          | line -> (
              match parse_row line_number line with
              | Error _ as error -> error
              | Ok (`Training sample) ->
                  read (line_number + 1) (sample :: training) testing
              | Ok (`Testing sample) ->
                  read (line_number + 1) training (sample :: testing))
        in
        read 1 [] [])
  with Sys_error message -> Error message

(** [count_label label examples] is the number of [examples] with [label]. *)
let count_label label examples =
  List.fold_left
    (fun count (sample : Knn.sample) ->
      if sample.label = label then count + 1 else count)
    0 examples

(** [validate_split name expected examples] checks that [examples] has the
    expected count for every category and contains no unknown labels. *)
let validate_split name expected examples =
  match
    List.find_opt
      (fun (sample : Knn.sample) -> not (List.mem sample.label categories))
      examples
  with
  | Some sample ->
      Error
        (Printf.sprintf "unknown category in %s split: %s" name sample.label)
  | None -> (
      match
        List.find_opt
          (fun label -> count_label label examples <> expected)
          categories
      with
      | Some label ->
          Error
            (Printf.sprintf "expected %d %s examples in %s split, found %d"
               expected label name
               (count_label label examples))
      | None -> Ok ())

(** [make_baseline training] adapts the original k-NN implementation to the
    evaluator's top-label predictor interface. *)
let make_baseline training features =
  match Knn.predict ~k:3 ~training features with
  | Error _ as error -> error
  | Ok (best :: _) -> Ok best.label
  | Ok [] -> Error Knn.Empty_training_set

(** [make_experiment training k metric voting] builds a top-label predictor for
    one experimental configuration. *)
let make_experiment training k metric voting features =
  match Knn_experiments.predict ~k ~metric ~voting ~training features with
  | Error _ as error -> error
  | Ok (best :: _) -> Ok best.label
  | Ok [] -> Error Knn.Empty_training_set

(** [configurations training] lists the baseline and all requested experiments.
*)
let configurations training =
  let baseline =
    {
      name = "baseline";
      k = "3";
      metric = "euclidean";
      voting = "uniform";
      predict = make_baseline training;
    }
  in
  let experiments =
    List.concat_map
      (fun k ->
        List.concat_map
          (fun (metric, metric_name) ->
            List.map
              (fun (voting, voting_name) ->
                {
                  name = "experiment";
                  k = string_of_int k;
                  metric = metric_name;
                  voting = voting_name;
                  predict = make_experiment training k metric voting;
                })
              [
                (Knn_experiments.Uniform, "uniform");
                (Knn_experiments.Inverse_distance, "inverse-distance");
              ])
          [
            (Knn_experiments.Euclidean, "euclidean");
            (Knn_experiments.Manhattan, "manhattan");
          ])
      [ 1; 3; 5 ]
  in
  baseline :: experiments

(** [evaluate_all testing configurations] evaluates each predictor on the same
    held-out examples. *)
let evaluate_all testing configurations =
  let rec loop results = function
    | [] -> Ok (List.rev results)
    | configuration :: rest -> (
        match Evaluation.evaluate ~predict:configuration.predict testing with
        | Error error -> Error (Evaluation.error_to_string error)
        | Ok result -> loop ((configuration, result) :: results) rest)
  in
  loop [] configurations

(** [class_accuracy label result] is the accuracy text for [label] in [result].
*)
let class_accuracy label result =
  match
    List.find_opt
      (fun class_result -> class_result.Evaluation.label = label)
      result.Evaluation.by_class
  with
  | None -> "n/a"
  | Some class_result -> Printf.sprintf "%.1f%%" (100. *. class_result.accuracy)

(** [print_results results] prints overall and per-category accuracy and average
    prediction time for each classifier configuration. *)
let print_results results =
  Printf.printf "%-12s %-4s %-12s %-18s %-9s %-7s %-7s %-7s %-9s %-9s\n"
    "configuration" "k" "metric" "voting" "correct" "cat" "tree" "house"
    "bicycle" "airplane";
  List.iter
    (fun (configuration, result) ->
      Printf.printf
        "%-12s %-4s %-12s %-18s %3d/%-5d %-7s %-7s %-7s %-9s %-9s  %.4f \
         ms/example\n"
        configuration.name configuration.k configuration.metric
        configuration.voting result.Evaluation.correct result.total
        (class_accuracy "cat" result)
        (class_accuracy "tree" result)
        (class_accuracy "house" result)
        (class_accuracy "bicycle" result)
        (class_accuracy "airplane" result)
        (result.average_prediction_seconds *. 1000.))
    results

let () =
  match load_dataset dataset_path with
  | Error message ->
      prerr_endline message;
      prerr_endline
        "Run python3 scripts/prepare_quickdraw.py to prepare the dataset.";
      exit 1
  | Ok (training, testing) -> (
      match
        ( validate_split "training" training_per_category training,
          validate_split "test" testing_per_category testing )
      with
      | Error message, _ | _, Error message ->
          prerr_endline message;
          exit 1
      | Ok (), Ok () -> (
          Printf.printf "Loaded %d training and %d held-out drawings from %s\n"
            (List.length training) (List.length testing) dataset_path;
          match evaluate_all testing (configurations training) with
          | Error message ->
              prerr_endline message;
              exit 1
          | Ok results -> print_results results))
