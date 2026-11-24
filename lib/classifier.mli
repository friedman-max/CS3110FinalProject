(** Connects player drawing, model classification, and the main game loop.

    The game ({!Game}) only sees guesses as a [(label, score) list], best first.
    This module turns a {!Drawing.t} into that list:
    + a blank drawing gives no guesses ({!Drawing.is_empty});
    + otherwise the drawing becomes a feature vector ({!features});
    + {!Knn.predict} ranks the categories;
    + {!of_knn} converts the result into the game's format. *)

val features : Drawing.t -> float array
(** [features d] is the feature vector for [d] that {!Knn.predict} compares
    against the training set: 784 floats (28x28 grayscale, 0.0 background, 1.0
    strokes). See {!Preprocess.features}. *)

val of_knn : (Knn.prediction list, Knn.error) result -> (string * float) list
(** [of_knn r] is the game's guess list for the classifier result [r]:
    - [Ok ps] gives one [(p.label, p.vote_fraction)] per prediction, in the same
      order (best first, higher score is better). [ps] may have fewer than three
      entries, or none; the list is never padded.
    - [Error _] gives [[]], which the game scores as a miss. *)

val guess :
  k:int -> training:Knn.sample list -> Drawing.t -> (string * float) list
(** [guess ~k ~training d] is the game's guess list for drawing [d]: [[]] if [d]
    is empty (a blank canvas), and otherwise
    [of_knn (Knn.predict ~k ~training (features d))]. *)
