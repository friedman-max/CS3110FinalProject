type t = { training : Knn.sample list; testing : Knn.sample list }

exception Invalid_row of int * string

let load filename =
  let rec read_rows input line training testing =
    match input_line input with
    | exception End_of_file ->
        { training = List.rev training; testing = List.rev testing }
    | row ->
        let invalid message = raise (Invalid_row (line, message)) in
        match String.split_on_char '\t' row with
        | [ split; label; pixels ] ->
            let label = String.trim label in
            if label = "" then invalid "Missing category";
            let values =
              String.trim pixels |> String.split_on_char ',' |> Array.of_list
            in
            if Array.length values <> 784 then invalid "Expected 784 pixels";
            let features =
              Array.map
                (fun value ->
                  match int_of_string_opt value with
                  | Some byte when byte >= 0 && byte <= 255 ->
                      float_of_int byte /. 255.
                  | _ -> invalid "Expected integer pixels in 0..255")
                values
            in
            let sample : Knn.sample = { label; features } in
            (match split with
            | "train" -> read_rows input (line + 1) (sample :: training) testing
            | "test" -> read_rows input (line + 1) training (sample :: testing)
            | _ -> invalid "Expected train or test split")
        | _ -> invalid "Expected three tab-separated fields"
  in
  try
    let input = open_in filename in
    Ok
      (Fun.protect
         ~finally:(fun () -> close_in_noerr input)
         (fun () -> read_rows input 1 [] []))
  with
  | Sys_error message -> Error message
  | Invalid_row (line, message) ->
      Error (Printf.sprintf "%s:%d: %s" filename line message)
