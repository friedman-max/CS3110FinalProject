type t = { training : Knn.sample list; testing : Knn.sample list }

val load : string -> (t, string) result
(** Read a prepared Quick, Draw! TSV, keeping the split and order of its rows.
    Each row must use a [train] or [test] split, a nonblank label, and 784 byte
    values. Pixels are converted to [0.0, 1.0]. Errors identify the file and,
    for malformed data, the line. *)
