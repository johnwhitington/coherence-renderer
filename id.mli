(** Unique identifiers*)

(** An id element. *)
type id

(** An id set contains object and generation IDs, together with a hash key
calculated from them. *)
type idset = id * int

(** Debug string of an ID set. *)
val string_of_idset : idset -> string

(** Test two IDs for equality *)
val eq : id -> id -> bool

(** Equality on ID sets. *)
val set_eq : idset -> idset -> bool

(** Calculate a fresh set of IDs. *)
val new_ids : unit -> idset

(** Deterministically combine two ID sets to build another. *)
val combine : idset -> idset -> idset

