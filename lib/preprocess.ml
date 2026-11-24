let side = 28
let samples_per_pixel = 4
let mask_side = side * samples_per_pixel

(* Quick, Draw! uses a 256-unit drawing with 16 units of padding on each side
   and a 16-unit round pen. Set the geometry in output pixels. *)
let bitmap_scale = float_of_int side /. (256. +. 2. *. 16. +. 16.)
let drawing_span = 255. *. bitmap_scale
let pen_radius = 8. *. bitmap_scale

let paint_segment mask (x0, y0) (x1, y1) =
  let resolution = float_of_int samples_per_pixel in
  let lower a b =
    max 0 (int_of_float (floor ((min a b -. pen_radius) *. resolution)))
  in
  let upper a b =
    min (mask_side - 1)
      (int_of_float (ceil ((max a b +. pen_radius) *. resolution)))
  in
  let dx = x1 -. x0 and dy = y1 -. y0 in
  let length_squared = (dx *. dx) +. (dy *. dy) in
  for row = lower y0 y1 to upper y0 y1 do
    let y = (float_of_int row +. 0.5) /. resolution -. y0 in
    for col = lower x0 x1 to upper x0 x1 do
      let x = (float_of_int col +. 0.5) /. resolution -. x0 in
      let along =
        if length_squared = 0. then 0.
        else max 0. (min 1. (((x *. dx) +. (y *. dy)) /. length_squared))
      in
      let px = x -. (along *. dx) and py = y -. (along *. dy) in
      if (px *. px) +. (py *. py) <= pen_radius *. pen_radius then
        mask.((row * mask_side) + col) <- true
    done
  done

let features drawing =
  match Drawing.bounding_box drawing with
  | None -> Array.make (side * side) 0.
  | Some ((left, bottom), (right, top)) ->
      let center_x = (float_of_int left +. float_of_int right) /. 2. in
      let center_y = (float_of_int bottom +. float_of_int top) /. 2. in
      let scale =
        drawing_span /. float_of_int (max 1 (max (right - left) (top - bottom)))
      in
      let center = float_of_int side /. 2. in
      let to_bitmap (x, y) =
        ( center +. ((float_of_int x -. center_x) *. scale),
          center +. ((center_y -. float_of_int y) *. scale) )
      in
      let mask = Array.make (mask_side * mask_side) false in
      List.iter
        (function
          | [] -> ()
          | first :: rest ->
              let first = to_bitmap first in
              paint_segment mask first first;
              ignore
                (List.fold_left
                   (fun previous point ->
                     let point = to_bitmap point in
                     paint_segment mask previous point;
                     point)
                   first rest))
        (Drawing.strokes drawing);
      Array.init (side * side) (fun index ->
          let row = index / side * samples_per_pixel in
          let col = index mod side * samples_per_pixel in
          let covered = ref 0 in
          for dy = 0 to samples_per_pixel - 1 do
            for dx = 0 to samples_per_pixel - 1 do
              if mask.(((row + dy) * mask_side) + col + dx) then incr covered
            done
          done;
          float_of_int !covered
          /. float_of_int (samples_per_pixel * samples_per_pixel))
