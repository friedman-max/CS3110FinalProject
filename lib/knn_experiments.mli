(** Experimental k-nearest-neighbor prediction options. The original
    [Knn.predict] remains available as the baseline. *)

type metric = Euclidean | Manhattan
type voting = Uniform | Inverse_distance

type prediction = { label : string; score : float; nearest_distance : float }
(** A category prediction. [score] is the fraction of total neighbor vote weight
    assigned to [label]; it is not a calibrated probability. *)

val distance : metric -> float array -> float array -> (float, Knn.error) result
(** [distance metric left right] is the distance between [left] and [right].
    Euclidean distance is ordinary (not squared) Euclidean distance. Returns
    [Error] if either vector is empty or non-finite, if the vector dimensions
    differ, or if calculating the distance overflows. *)

val predict :
  ?k:int ->
  metric:metric ->
  voting:voting ->
  training:Knn.sample list ->
  float array ->
  (prediction list, Knn.error) result
(** [predict ~k ~metric ~voting ~training query] classifies [query] using the
    [k] nearest samples in [training]. If [k] exceeds the number of samples, all
    samples are selected. In inverse-distance mode, exact matches share all vote
    weight when any are present. The result contains categories represented
    among the selected samples, ranked by decreasing vote score, increasing
    nearest supporting distance, then alphabetical label.

    Returns [Error] if [k] is nonpositive, [training] is empty, a label is
    blank, or a vector is invalid. Arrays are not modified. *)
