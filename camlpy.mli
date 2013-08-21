(** Marshalling OCaml data for Python *)

type marshallable =
  | Tuple of marshallable list
  | Bool of bool
  | Int of int
  | Unit
  | String of string

(** Marshall to a string *)
val marshall : marshallable -> string

(** Unmarshall, signalling failure by None *)
val unmarshall : string -> (int * marshallable) option

(** For debug purposes only, return a human-readable string representation of a
marshallable *)
val debug_string_of_marshallable : marshallable -> string

