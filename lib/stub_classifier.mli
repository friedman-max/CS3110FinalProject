(** Text-based stand-in for {!Classifier}, used by the terminal driver.

    In the terminal there is no canvas, so the player types the word they would
    have drawn. This module turns that text into a guess list in the same format
    {!Classifier.guess} produces, so the full game loop can be played and tested
    without a window. Only the terminal driver calls it, so [Game] and [Scoring]
    do not depend on it. *)

val predict : typed:string -> (string * float) list
(** [predict ~typed] pretends the player drew [typed]. It returns three guesses,
    best first with descending scores: [typed] (trimmed) first, then two other
    categories from [Config.default.words] (the dataset's categories), never
    repeating [typed]. If [typed] is blank it returns [[]], meaning nothing was
    recognizable.

    Because the other guesses are real categories, a wrong answer can still
    place the target second or third and earn top-3 points. *)
