(** The game window: a drawing area where the player sketches with the
    mouse, plus a text strip for prompts and results.

    This module is a thin layer over the [graphics] library. It translates
    mouse and keyboard events into calls on {!Drawing} and redraws the
    screen; it holds no game logic, so the rules for strokes, clearing and
    submitting can be tested through {!Drawing} without opening a window.

    Window layout: the drawing area occupies
    ([scale] * [Drawing.width]) x ([scale] * [Drawing.height]) pixels at the
    bottom-left. Each {!Drawing.point} is shown as a [scale] x [scale] block,
    and mouse positions are divided by [scale] before being recorded, so the
    window can be enlarged without changing drawing coordinates. A text strip
    of {!text_height} pixels sits above it. Mouse positions in the text strip
    are clamped into the drawing area.

    Controls while drawing:
    - hold the left mouse button and move: draw a stroke
    - [c]: clear the drawing
    - [Enter]: submit (ignored while the drawing is empty)
    - [Esc] or [q]: quit *)

val scale : int
(** On-screen magnification of the drawing area (2 means a 560x560 area
    for a 280x280 drawing). Change this to resize the window. *)

val text_height : int
(** Height in pixels of the text strip above the drawing area. *)

val open_window : unit -> unit
(** [open_window ()] opens the game window, sized
    ([scale] * [Drawing.width]) x ([scale] * [Drawing.height] + [text_height]).
    Must be called before any other function in this module.
    Raises [Graphics.Graphic_failure] if no display is available (for
    example, XQuartz not installed or not running on macOS). *)

val close_window : unit -> unit
(** [close_window ()] closes the game window. Does nothing if it is not
    open. *)

val render : Drawing.t -> unit
(** [render d] clears the drawing area and draws every stroke of [d],
    connecting consecutive points of each stroke with line segments. A
    single-point stroke is drawn as a dot. Does not touch the text strip. *)

val show_text : string -> unit
(** [show_text s] replaces the contents of the text strip with [s]. *)

val get_drawing : prompt:string -> Drawing.t option
(** [get_drawing ~prompt] shows [prompt] in the text strip (e.g.
    ["Draw: cat   [Enter] submit  [c] clear"]), starts from an empty drawing,
    and runs the input loop until the player submits or quits.
    Returns [Some d] with the submitted, non-empty drawing [d], or [None] if
    the player quit (including closing the window). Any stroke in progress
    at submission is finished first. *)

type drawing_result =
  | Submitted of Drawing.t
  | Timed_out of Drawing.t
  | Skipped
  | Quit

val get_timed_drawing :
  prompt:(float -> string) ->
  allow_skip:bool ->
  time_limit:float ->
  drawing_result * float
(** Draw with the usual controls and a deadline in seconds. The prompt receives
    the displayed seconds remaining, rounded up. [S] skips if [allow_skip] is
    true. Expiry returns the drawing so far, including any stroke in progress.
    The second result is the time remaining when input ended. Closing the
    window returns [Quit]. The limit must be finite and positive. *)

val show_result : target:string -> guesses:string list -> correct:bool -> unit
(** [show_result ~target ~guesses ~correct] shows the round's outcome in
    the text strip: the target category, the guesses in ranked order (best
    first), and whether the round was won. Then it waits for any key press
    before returning, so the player can read it. [guesses] may be empty. *)
