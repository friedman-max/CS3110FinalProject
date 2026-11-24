open Graphics

let scale = 2
let text_height = 50
let pen_width = 4 * scale

(* Size of the drawing area on screen, in window pixels. *)
let area_w = Drawing.width * scale
let area_h = Drawing.height * scale
let displayed_text = ref ""

let open_window () =
  open_graph (Printf.sprintf " %dx%d" area_w (area_h + text_height));
  set_window_title "Quickdraw";
  auto_synchronize false;
  displayed_text := ""

let close_window () = try close_graph () with Graphic_failure _ -> ()

(* Drawing points are in canvas coordinates; multiply by [scale] to get window
   pixels. *)
let draw_stroke stroke =
  match List.map (fun (x, y) -> (x * scale, y * scale)) stroke with
  | [] -> ()
  | [ (x, y) ] -> fill_circle x y (pen_width / 2)
  | (x, y) :: rest ->
      moveto x y;
      List.iter (fun (x, y) -> lineto x y) rest

let draw_text s =
  set_color (rgb 230 230 230);
  fill_rect 0 (area_h + 1) area_w (text_height - 1);
  set_color black;
  let lines = String.split_on_char '\n' s in
  let _, line_h = text_size "Ag" in
  (* Stack lines from the top of the strip downward. *)
  List.iteri
    (fun i line ->
      moveto 8 (area_h + text_height - ((i + 1) * (line_h + 4)));
      draw_string line)
    lines

let render d =
  set_color white;
  fill_rect 0 0 area_w area_h;
  set_color black;
  set_line_width pen_width;
  List.iter draw_stroke (Drawing.strokes d);
  set_line_width 1;
  moveto 0 area_h;
  lineto area_w area_h;
  (* The pen can extend above the drawing area at its top edge. Repaint the
     prompt strip before presenting the completed frame. *)
  draw_text !displayed_text;
  synchronize ()

let show_text s =
  draw_text s;
  displayed_text := s;
  synchronize ()

type drawing_result =
  | Submitted of Drawing.t
  | Timed_out of Drawing.t
  | Skipped
  | Quit

type drawing_event = Input of status | Deadline

let rec loop ~next ~allow_skip d down =
  let continue = loop ~next ~allow_skip in
  match next () with
  | Deadline -> Timed_out (Drawing.end_stroke d)
  | Input st when st.keypressed -> (
      match st.key with
      | 'c' | 'C' ->
          render Drawing.empty;
          continue (Drawing.clear d) st.button
      | '\r' | '\n' ->
          if Drawing.is_empty d then continue d down
          else Submitted (Drawing.end_stroke d)
      | 's' | 'S' when allow_skip -> Skipped
      | '\027' | 'q' | 'Q' -> Quit
      | _ -> continue d down)
  | Input st ->
      let p = (st.mouse_x / scale, st.mouse_y / scale) in
      let in_window =
        0 <= st.mouse_x && st.mouse_x < area_w
        && 0 <= st.mouse_y && st.mouse_y < area_h + text_height
      in
      let d' =
        match (st.button, down) with
        | true, false when in_window -> Drawing.start_stroke p d
        | true, true -> Drawing.add_point p d
        | false, true -> Drawing.end_stroke d
        | _ -> d
      in
      if d' != d then render d';
      continue d' st.button

let get_drawing ~prompt =
  show_text prompt;
  render Drawing.empty;
  let next () =
    Input (wait_next_event [ Button_down; Button_up; Mouse_motion; Key_pressed ])
  in
  try
    match loop ~next ~allow_skip:false Drawing.empty false with
    | Submitted drawing -> Some drawing
    | Timed_out _ | Skipped | Quit -> None
  with Graphic_failure _ -> None

let get_timed_drawing ~prompt ~allow_skip ~time_limit =
  if not (Float.is_finite time_limit && time_limit > 0.) then
    invalid_arg "Canvas.get_timed_drawing: time_limit must be finite and positive";
  try
    let shown = ref (ceil time_limit) in
    show_text (prompt !shown);
    render Drawing.empty;
    let deadline = Unix.gettimeofday () +. time_limit in
    let remaining () = max 0. (min time_limit (deadline -. Unix.gettimeofday ())) in
    let time_left = ref time_limit in
    let next () =
      (* Wait at most one sampling interval so inactivity cannot pause the clock. *)
      Unix.sleepf (min 0.008 (remaining ()));
      let st = wait_next_event [ Poll ] in
      let st = if st.keypressed then { st with key = read_key () } else st in
      time_left := remaining ();
      if !time_left = 0. then Deadline
      else (
        let seconds = ceil !time_left in
        if seconds <> !shown then (
          shown := seconds;
          show_text (prompt seconds));
        Input st)
    in
    let result = loop ~next ~allow_skip Drawing.empty false in
    (result, !time_left)
  with Graphic_failure _ -> (Quit, 0.)

let show_result ~target ~guesses ~correct =
  let verdict = if correct then "Correct!" else "Not quite." in
  let guess_str =
    match guesses with
    | [] -> "(none)"
    | gs -> String.concat ", " gs
  in
  show_text
    (Printf.sprintf "Target: %s - %s\nGuesses: %s  (press a key)" target verdict
       guess_str);
  try ignore (wait_next_event [ Key_pressed ]) with Graphic_failure _ -> ()
