(* \part{Preliminaries} *)

(* \chaptertitle{Id}{Unique identifiers} *)

(* \epigraph{Hello. I'm Chevy Chase, and you're not.}{\textit{Chevy Chase,
1943--}} *)
open Pdfutil

(* Our scenes will consist of a number of objects in a stack. These must have a
distinct identifier set so that they may be tested for equality, stored in
caches etc. We can combine two identifiers, always getting the same result. The
type of identifiers is abstract, so we may freely alter the implementation.
\smallgap *)

(* An ID element. We use a 64 bit signed integer, giving us about $2^{63}$ IDs
until a duplicate is created. There is no detection of duplication. *)
type idelt = int64

(* \intfless An ID consists of a list of ID elements. This allows us to combine IDs
deterministically simply by appending them. *)
type id = idelt list

(* \intf A type abbreviation to make the interface file easier to read. *)
type hashkey = int

(* \intf The object and generation IDs, together with a hash key calculated from them
for efficient caching. *)
type idset = id * hashkey

let cid = ref 0L

(* \intf Make a new ID by drawing from the unique source and updating the source. *)
let new_id () =
  cid := Int64.succ !cid;
  [Int64.pred !cid]

(* \intf Equality test on IDs. *)
let eq = ( = )

(* \intf Equality test on Idsets *)
let set_eq = ( = )

(* We wish to produce a key based upon quite a lot of the available
information, since lookups are much more frequent than insertions so instruct
the \emph{Hashtbl} library to walk lots of the structure to find it (see
documention to [Hashtbl] for details). *)
let calc_hash =
  Hashtbl.hash_param 20 20

(* \intf Make a new set of IDs --- an object ID and its hash. *)
let new_ids () =
  let id = new_id () in
    id, calc_hash id

(* \intf Combine two ID sets to form another. Deterministic. *)
let combine (oid, _) (oid2, _) =
  let id = oid @ oid2 in
    id, calc_hash id

(* String of [idelt] *)
let string_of_idelt a =
  Int64.to_string a ^ ";"

(* String of [id] *)
let string_of_id a =
  fold_left ( ^ ) "" (map string_of_idelt a)

(* \intf String of [idset] *)
let string_of_idset (o, _) =
  string_of_id o

