(** Damerau-Levenshtein distance and "did you mean?" lookups. *)

(** Distance (edit + adjacent transposition) between two strings. *)
val distance : string -> string -> int

(** [closest ~max_distance needle haystack] returns the candidate(s) in
    [haystack] with distance <= [max_distance] from [needle], sorted by
    distance ascending. Empty result means no candidate is close enough. *)
val closest : ?max_distance:int -> string -> string list -> string list
