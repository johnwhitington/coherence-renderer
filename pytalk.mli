(** Communication with a Python process *)

type send = Camlpy.marshallable -> unit

type poll = unit -> Camlpy.marshallable

type close = unit -> unit

(** Passing name of python interpreter (e.g "python") and script filename,
returns (send, poll, close). When close() is called, the python process on the
other end closes its connection too. *)
val establish_connection :
  string -> string -> send * poll * close

(** Establish a connection where python has started the process. Takes a port
number and starts up *)
(*i val pystarts_establish_connection : int -> send * poll * close i*)

