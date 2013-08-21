(** Caching for shapes and sprites *)

(** Record for holding hit and miss counts for shapes and sprites. *)
type cachestats =
  {mutable shphit : int;
   mutable shpmis : int;
   mutable sprhit : int;
   mutable sprmis : int}

type properties =
  {mutable cachemaxsize : int;
   mutable cachesize : int;
   mutable cachetimer : int}

(** The global counter. *)
val cachestats : cachestats

(** The calling program can turn the cache off to save memory. On by default. *)
val usecache : bool ref

(** Print a summary of the cache. *)
val string_of_cachestate : unit -> string

(** Clear the cache. *)
val clear : unit -> unit

(** Set the size of the cache in bytes. *)
val setsize : int -> unit

(** Add a new shape and minshape to the cache under key [idset], replacing any
already there. *)
val addshape : Id.idset -> Sprite.shape -> Sprite.shape -> unit

(** Add a new sprite and its shape to the cache. If already present, replaces
any part of the sprite that overlaps, but retains any of the old which
doesn't. This allows partial sprites to be extended. *)
val addsprite : Id.idset -> Sprite.sprite -> Sprite.shape -> unit

(** Get a shape and minshape, if they are in the cache. *)
val getshape : Id.idset -> (Sprite.shape * Sprite.shape) option

(** Get a partial sprite and its shape, if they are in the cache. *)
val getsprite : Id.idset -> (Sprite.sprite * Sprite.shape) option

(** [addtranslation idset target_idset dx dy] adds a translation under the id
[idset] of the object stored under [target_idset], translating by [(dx, dy)].*)
val addtranslation :
  Id.idset -> Id.idset -> Coord.pixbin -> Coord.pixbin -> unit

