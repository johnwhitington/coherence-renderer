(* \part{Rendering scenes} *)
(* \chaptertitle{Cache}{Memoisation for rendering} *)

(* The cache module provides a specialised cache for the renderer, storing
shapes and sprites keyed by their object ID. *)
open Pdfutil

(* \intf The calling program can turn the cache off, to save memory or for
debugging purposes. *)
let usecache = ref true

(* Each entry is stored with some metrics: the size of any sprite (0 if none);
the size of any shape (0 if none); the time (see below) it was last used; and
the time of its entry into the cache. These are sufficient to support most
common cache strategies. *)
type metrics =
  {mutable spritesize : int;
   mutable shapesize : int;
   mutable lastused : int;
   mutable entry : int}

(* \intf We collect statistics as the program runs; the number of cache hits and
misses for shapes and sprites.  *)
type cachestats =
  {mutable shphit : int;
   mutable shpmis : int;
   mutable sprhit : int;
   mutable sprmis : int}

(* \intf The initial state. *)
let cachestats =
  {shphit = 0; shpmis = 0; sprhit = 0; sprmis = 0}

(* Functions to update the statistics. *)
let shphit () = cachestats.shphit <- cachestats.shphit + 1
let shpmis () = cachestats.shpmis <- cachestats.shpmis + 1
let sprhit () = cachestats.sprhit <- cachestats.sprhit + 1
let sprmis () = cachestats.sprmis <- cachestats.sprmis + 1

(* We support \emph{aliased objects}, where one object in the cache refers to
another. This is used for objects which differ only by translation through
alias to an object. *)
type copied = Id.idset list

(* The shape and minshape respectively for a shape stored in the cache. *)
type cacheshape = Sprite.shape * Sprite.shape

(* The partial sprite and its partial shape for a shape portion stored in the
cache. *)
type cachesprite = Sprite.sprite * Sprite.shape

(* \intf A cache object is either a copy list, one or more of shape and sprite
and its metrics, or an aliased object referring to one of another ID set, with
[x] and [y] integer translations. The translated object does not need metrics
--- they are calculated for and stored with the base object and all its
translations together. *)
type cacheobject =
  | CacheObject of copied * cacheshape option * cachesprite option * metrics
  | TranslatedObject of int * int * Id.idset

(* Global properties for the cache: the maximum size, current size and global
time (a monotonically increasing integer used to inform cache dropping
strategies). All sizes in bytes. *)
type properties =
  {mutable cachemaxsize : int;
   mutable cachesize : int;
   mutable cachetimer : int}

(* The type for the cache itself: the properties and a hash table mapping from
ID sets to cache objects. We hash on an integer key calculated by the Id module
for speed. *)
let properties =
  {cachemaxsize = 50 * 1024 * 1024; cachesize = 0; cachetimer = 0}

(* Advance the time counter. This is called by operations which alter the cache.
*)
let advance_time () =
  properties.cachetimer <- succ properties.cachetimer

(* The cache. 100001 is an estimate of the number of objects in the cache at
any one time. It is a hint to the hash table module. *)
let cache =
  Hashtbl.create 100001

(* Predicate to determine if a cache object contains a shape. *)
let hasshape = function
  | CacheObject (_, Some _, _, _) -> true
  | _ -> false

(* Predicate to determine if a cache object contains a sprite. *)
let hassprite = function
  | CacheObject (_, _, Some _, _) -> true
  | _ -> false

(* Find an object in the cache with a given [Id.idset]. Throws [Not_found] the
object is not in the cache. The inner function [lookup], given a list of (idset,
cacheobject) pairs (the contents of a single hash bucket), returns the cache
object associated with that idset. This is used to find the required object from
the list of objects which are contained within one hash bucket. *)
let findobj (oid, key) =
  let rec lookup oid = function
    | [] -> raise Not_found
    | ((oid', _), cacheobject)::t ->
        if Id.eq oid oid'
          then cacheobject else lookup oid t
  in
    try lookup oid (Hashtbl.find_all cache key) with
      Not_found -> raise Not_found

(* Remove all objects stored under a key. *)
let removeall key =
  let canfind key =
    try ignore (Hashtbl.find cache key); true with
      Not_found -> false
  in
    while canfind key do Hashtbl.remove cache key done

(* Remove the object with a given idset. We get all the objects which hash to a
certain key, remove the one where the ID actually matches and return the rest
to the hash table. *)
let removeobj ((oid, key) : Id.idset) =
  let objs = Hashtbl.find_all cache key in
    let objs' =
      lose
        (fun ((oid', _), _) -> Id.eq oid oid')
        objs
    in
      removeall key;
      iter (Hashtbl.add cache key) objs'

(* Add an object with a given idset. *)
let addobj (_, key) =
  Hashtbl.add cache key

(* Replace an object with a given idset. Find all, modify one, remove all,
insert all. *)
let replaceobj (oid, key) obj' =
  let objects = Hashtbl.find_all cache key in
    let modified =
      replaceinlist
        (fun ((oid', key'), _) -> oid = oid')
        obj' objects
    in
      removeall key;
      iter
        (fun (idset, cacheobject) -> addobj idset (idset, cacheobject))
        modified

(* Find the metrics of an object. *)
let rec metricsin = function
  | CacheObject (_, _, _, metrics) -> metrics
  | TranslatedObject (_, _, idset) ->
      try metricsin (findobj idset) with
        Not_found -> failwith "metricsin: Malformed cache"

(* \intf Print a cache summary, for debug purposes. *)
let string_of_cachestate () =
  let text = ref "" in
  text := !text ^
    Printf.sprintf
    "Maximum size: %i, Current size: %i, Cache time: %i\n"
    properties.cachemaxsize properties.cachesize properties.cachetimer
  ^
    "oid\tshp?\tspr?\ttr?\tshp\tspr\tin\tlast\n";
  Hashtbl.iter
    (fun key (idset, cacheobject) ->
      let metrics = metricsin cacheobject in
        text := !text ^
        Printf.sprintf "%s\t%s\t%s\t%s\t%i\t%i\t\t%i\t\t%i\n"
          (Id.string_of_idset idset)
          (match cacheobject with
           | CacheObject (_, Some _, _, _) -> "Y"
           | TranslatedObject (_, _, idset)
               when hasshape (findobj idset) -> "Y"
           | _ -> "N")
          (match cacheobject with
           | CacheObject (_, _, Some _, _)  -> "Y"
           | TranslatedObject (_, _, idset)
               when hassprite (findobj idset) -> "Y"
           | _ -> "N")
          (match cacheobject with TranslatedObject _ -> "Y" | _ -> "N")
          metrics.shapesize metrics.spritesize
          metrics.entry metrics.lastused)
     cache;
     !text

(* \intf Clear the cache. *)
let clear () =
  Hashtbl.clear cache;
  properties.cachesize <- 0

(* Drop a whole object from the cache. *)
let dropobject idset =
  let obj =
    try findobj idset with
      Not_found -> failwith "dropobject: object not found"
  in
    removeobj idset;
    match obj with
     | CacheObject (copylist, _, _, _)  -> iter removeobj copylist
     | _ -> ()

(* Drop just the shape or just the sprite from an object in the cache, leaving
any sprite. *)
let dropshape idset =
  try
    match findobj idset with
    | TranslatedObject _ -> () (*r will be removed with its parent. *)
    | CacheObject (cl, Some _, Some spr, metrics) ->
        let cacheobject' =
          CacheObject (cl, None, Some spr, {metrics with shapesize = 0})
        in
          replaceobj idset (idset, cacheobject') 
    | CacheObject (_, Some _, None, _)  -> dropobject idset
    | CacheObject (_, None, Some _, _) -> ()
    | CacheObject (_, None, None, _) ->
        failwith "dropshape: malformed cache entry"
  with
    Not_found -> ()

(* The same, but dropping the sprite instead. *)
let dropsprite idset =
  try 
    match findobj idset with
    | TranslatedObject _ -> () (*r will be removed with its parent. *)
    | CacheObject (cl, Some shp, Some _, metrics) ->
        let cacheobject' =
          CacheObject (cl, Some shp, None, {metrics with spritesize = 0})
        in
          replaceobj idset (idset, cacheobject')
    | CacheObject (_, None, Some _, _) -> dropobject idset
    | CacheObject (_, Some _, None, _) -> ()
    | CacheObject (_, None, None, _) ->
        failwith "dropsprite: malformed cache entry"
  with
    Not_found -> ()

(* This function drops at least size [n] bytes from the cache, dropping sprites
by preference to shapes (shapes are smaller and likely to be more useful in the
long term). We'll need a much better model eventually, or several models for
different domains.*)
let dropfromcache n =
  (* Calculate list of possible objects to drop (excludes translated objects). *)
  let sizes = ref [] in
    Hashtbl.iter
      (fun _ (idset, cacheobject) ->
         match cacheobject with
         | CacheObject (_, _, _, metrics) ->
             sizes := (idset, metrics.shapesize, metrics.spritesize)::!sizes
         | _ -> ())
      cache;
   (* Remove until we've removed enough, prefering sprites to shapes. *)
   let sizes' = ref !sizes
   and sizeremoved = ref 0 in
     while !sizeremoved < n && notnull !sizes do
       let idset, _, spritesize = hd !sizes in
         dropsprite idset;
         sizes := tl !sizes;
         sizeremoved += spritesize
     done;
     while !sizeremoved < n && notnull !sizes' do
       let idset, shapesize, _ = hd !sizes' in
         dropshape idset;
         sizes' := tl !sizes';
         sizeremoved += shapesize
     done;
     properties.cachesize <- properties.cachesize - !sizeremoved

(* A very basic cache policy. Drops half the cache. *)
let drophalf () =
  dropfromcache (properties.cachesize / 2)

(* \intf Set the size of the cache, removing objects from it if necessary. *)
let setsize n =
  dropfromcache (properties.cachemaxsize - n);
  properties.cachemaxsize <- n

(* \intf Add a new shape and minshape to the cache under key [idset], replacing
any already there. *)
let addshape idset shp minshp =
  if not !usecache then () else
  let size = Sprite.shapesize shp + Sprite.shapesize minshp in
    if size > properties.cachemaxsize / 2 then () else
    if properties.cachesize + size > properties.cachemaxsize then drophalf ();
  begin
    (*i flprint (Id.string_of_idset idset); i*)
    try
      match findobj idset with
      | TranslatedObject (_, _, idset) ->
          begin try match findobj idset with
             | TranslatedObject _ ->
                 failwith "addshape: malformed cache"
             | CacheObject (_, Some _, _, _) -> ()
             | CacheObject (cp, None, spr, metrics) ->
                 properties.cachesize <-
                   properties.cachesize - metrics.spritesize + size;
                 replaceobj idset
                   (idset, CacheObject (cp, Some (shp, minshp), spr,
                      {metrics with
                         lastused = properties.cachetimer;
                         shapesize = size}))
          with
            Not_found -> failwith "addshape: malformed cache"
          end
      | CacheObject (_, Some _, _, _) -> ()
      | CacheObject (cp, None, spr, metrics) ->
          properties.cachesize <-
            properties.cachesize - metrics.spritesize + size;
          replaceobj idset
            (idset, CacheObject (cp, Some (shp, minshp), spr,
               {metrics with
                  lastused = properties.cachetimer; shapesize = size}))
    with
      Not_found ->
        properties.cachesize <- properties.cachesize + size;
        addobj idset
          (idset,
            CacheObject ([], Some (shp, minshp), None,
              {shapesize = size;
               spritesize = 0;
               lastused = properties.cachetimer;
               entry = properties.cachetimer}))
  end;
  advance_time ()

(* \intf Add a sprite. If already present, replaces what exists---this allows
partial sprites to be extended. *)
let addsprite idset spr shp =
  if not !usecache then () else
  let size = Sprite.spritesize spr + Sprite.shapesize shp in
    if size > properties.cachemaxsize / 2 then () else
    if properties.cachesize + size > properties.cachemaxsize then drophalf ();
  begin try
    (*i flprint (Id.string_of_idset idset); i*)
    match findobj idset with
    | TranslatedObject (xo, yo, target_idset) ->
        (try match findobj target_idset with
           | TranslatedObject _ ->
               failwith "addsprite: malformed cache"
           | CacheObject (cp, shape, sprite, metrics) ->
               properties.cachesize <-
                 properties.cachesize - metrics.shapesize - metrics.spritesize + size;
               let shp' = Sprite.translate_shape ~-xo ~-yo shp
               and spr' = Sprite.translate_sprite ~-xo ~-yo spr in
                 replaceobj target_idset
                 (target_idset,
                   CacheObject (cp, shape, Some (spr', shp'),
                   {metrics with
                      lastused = properties.cachetimer; spritesize = size}))
         with
           Not_found -> failwith "addsprite: malformed cache")
    | CacheObject (cp, shape, sprite, metrics) ->
        properties.cachesize <-
          properties.cachesize - metrics.shapesize - metrics.spritesize + size;
        replaceobj idset
          (idset, CacheObject (cp, shape, Some (spr, shp),
             {metrics with
                lastused = properties.cachetimer; spritesize = size}))
  with
     Not_found ->
       properties.cachesize <- properties.cachesize + size;
       addobj idset
         (idset, CacheObject ([], None, Some (spr, shp),
           {shapesize = 0; spritesize = size; lastused =
             properties.cachetimer; entry = properties.cachetimer}))
  end;
  advance_time ()
  
(* \intf Get a shape or minshape from the cache, returning None if it's not there. *)
let rec getshape idset =
  if not !usecache then None else
    try
      match findobj idset with
      | CacheObject (cp, shp, spr, metrics) ->
          let metrics' = {metrics with lastused = properties.cachetimer} in
            advance_time ();
            if shp = None then shpmis () else shphit ();
            replaceobj idset (idset, (CacheObject (cp, shp, spr, metrics')));
            shp
      | TranslatedObject (dx, dy, idset) ->
          match getshape idset with
          | Some (shape, minshape) ->
              Some (Sprite.translate_shape dx dy shape,
                    Sprite.translate_shape dx dy minshape)
          | None -> None
    with
      Not_found -> shpmis (); None

(* \intf Get a partial sprite and its shape from the cache, returning None if it's not there *)
let rec getsprite idset =
  if not !usecache then None else
    try
      match findobj idset with
      | CacheObject (cp, shp, spr, metrics) ->
          advance_time ();
          if spr = None then sprmis () else sprhit ();
          let metrics' = {metrics with lastused = properties.cachetimer} in
            replaceobj idset (idset, CacheObject (cp, shp, spr, metrics'));
          spr
      | TranslatedObject (dx, dy, idset) ->
          match getsprite idset with
          | Some (spr, shp) ->
              Some (Sprite.translate_sprite dx dy spr,
                    Sprite.translate_shape dx dy shp)
          | None -> None
    with
      Not_found -> sprmis (); None

(* Add [idset] to the copy list of [target_idset]. *)
let addtocopied hashtable idset target_idset =
  try
    match findobj target_idset with
    | CacheObject (cp, shp, spr, metrics) ->
        replaceobj target_idset
          (target_idset, CacheObject (idset::cp, shp, spr, metrics))
    | TranslatedObject _ ->
        failwith "addtocopied: target is translated"
  with
    Not_found ->
      failwith "addtocopied: target not found"

(* \intf Add a translation [idset] translated by [(dx, dy)] from [target_idset]. *)
let addtranslation idset target_idset dx dy =
  if not !usecache then () else
    advance_time ();
    let target =
      try Some (findobj target_idset) with Not_found -> None
    in
      match target with
      | None -> () (*r Not in the cache, so can't add translation. *)
      | Some (CacheObject (cp, shp, spr, metrics)) ->
          addobj idset (idset, TranslatedObject (dx, dy, target_idset));
          addtocopied cache idset target_idset
      | Some (TranslatedObject (dx', dy', target_idset')) ->
          addobj idset (idset, TranslatedObject (dx + dx', dy + dy', target_idset'));
          addtocopied cache idset target_idset'

