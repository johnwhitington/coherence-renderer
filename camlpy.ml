open Pdfutil

type marshallable =
  | Tuple of marshallable list
  | Bool of bool
  | Int of int
  | Unit
  | String of string

let rec debug_string_of_marshallable = function
  | Int i -> string_of_int i
  | Unit -> "unit"
  | String s -> s
  | Bool b -> string_of_bool b
  | Tuple ls ->
      "(" ^ fold_left ( ^ ) "" (interleave "," (map debug_string_of_marshallable ls)) ^ ")"

(* Format:
     Int: Int tag + 4 bytes
     Unit: Unit tag
     String : String tag + 4 bytes length + data
     Bool: Bool tag + 0 or 1
     Tuple: Tuple tag + 4 bytes length of data + elements of tuple
*)
let tag_tuple = 0
and tag_unit = 1
and tag_int = 2
and tag_string = 3
and tag_bool = 4

(* Make 4 bytes from an integer *)
let bytes_of_int s p i =
  Bytes.set s p (char_of_int ((i land (255 lsl 24)) lsr 24));
  Bytes.set s (p + 1) (char_of_int ((i land (255 lsl 16)) lsr 16));
  Bytes.set s (p + 2) (char_of_int ((i land (255 lsl 8)) lsr 8));
  Bytes.set s (p + 3) (char_of_int (i land 255))

let rec marshall_flatten s pos = function
  | Unit ->
      Bytes.set s pos (char_of_int tag_unit);
      pos + 1
  | Int i ->
      Bytes.set s pos (char_of_int tag_int);
      bytes_of_int s (pos + 1) i;
      pos + 5
  | Bool b ->
      Bytes.set s pos (char_of_int tag_bool);
      Bytes.set s (pos + 1) (char_of_int (if b then 1 else 0));
      pos + 2
  | String st ->
      let l = String.length st in
        Bytes.set s pos (char_of_int tag_string);
        bytes_of_int s (pos + 1) l;
        String.blit st 0 s (pos + 5) l;
        pos + 5 + l
  | Tuple ls ->
      Bytes.set s pos (char_of_int tag_tuple);
      let p = ref (pos + 5) in
        iter (fun x -> p := marshall_flatten s !p x) ls;
        bytes_of_int s (pos + 1) (!p - (pos + 5));
        !p

let rec size_of_marshallable_inner = function
  | [] -> 0
  | Int _::m -> 1 + 4 + size_of_marshallable_inner m
  | Unit::m -> 1 + size_of_marshallable_inner m
  | Bool _::m -> 2 + size_of_marshallable_inner m
  | String s::m -> 1 + 4 + String.length s + size_of_marshallable_inner m
  | Tuple []::m -> 1 + 4 + size_of_marshallable_inner m
  | Tuple (h::t)::m -> size_of_marshallable_inner [h] + size_of_marshallable_inner (Tuple t::m)

let size_of_marshallable m =
  size_of_marshallable_inner [m] + 4

(* Main marshalling function *)
let marshall m =
  let size = size_of_marshallable m in
    let str = Bytes.create size in
      bytes_of_int str 0 (size - 4);
      ignore (marshall_flatten str 4 m);
      Bytes.to_string str

(* Unmarshall *)
exception Invalid_data

(* Extract the integer from a 4-byte word. May overflow, since Ocaml ints are only 31 bits *)
let int_of_bytes i0 i1 i2 i3 =
  (i0 lsl 24) lor (i1 lsl 16) lor (i2 lsl 8) lor i3

let rec unmarshall_inner = function
  | [] -> []
  | t::i0::i1::i2::i3::more when t = tag_int ->
      Int (int_of_bytes i0 i1 i2 i3)::unmarshall_inner more
  | t::more when t = tag_unit ->
      Unit::unmarshall_inner more
  | t::b::more when t = tag_bool ->
      Bool (b <> 0)::unmarshall_inner more
  | t::l1::l2::l3::l4::more when t = tag_string ->
      let len = int_of_bytes l1 l2 l3 l4 in
        String (implode <| map char_of_int (take more len))
        ::unmarshall_inner (drop more len)
  | t::l1::l2::l3::l4::inside when t = tag_tuple ->
      let len = int_of_bytes l1 l2 l3 l4 in
        Tuple (unmarshall_inner (take inside len))
        ::unmarshall_inner (drop inside len)
  | _ -> raise Invalid_data

(* Unmarshall the first complete object (if any) from a string.
Returns (bytes taken * marshallable) option. *)
let unmarshall str =
  let ioc = int_of_char in
    if String.length str < 4 then None else
      let len = int_of_bytes (ioc str.[0]) (ioc str.[1]) (ioc str.[2]) (ioc str.[3]) in
        assert (len >= 0);
        if String.length str < 4 + len then None else
          let str = String.sub str 4 len in
            Some
              (len + 4, 
                 try
                   match unmarshall_inner (map int_of_char <| explode str) with
                   | [x] -> x
                   | _ -> raise Invalid_data
                 with
                 _ -> raise Invalid_data)

