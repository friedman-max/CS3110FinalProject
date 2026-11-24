(** Convert canvas strokes to the Quick, Draw! bitmap feature format. *)

val features : Drawing.t -> float array
(** [features drawing] returns 784 top-to-bottom, row-major grayscale values.
    The drawing is uniformly scaled and centered, with background 0 and ink
    coverage up to 1. A blank drawing returns all zeros. *)
