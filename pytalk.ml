open Pdfutil
open Camlpy

(*i external tcp_nodelay : Unix.file_descr -> bool -> unit = "tcp_nodelay" i*)

(* Buffer for bytes coming in from network *)
let buffer = ref ""

(* The raw buffer for network input *)
let rawbuf = String.create 1024

(* The event queue *)
let events = Queue.create ()

(* The poll function. This looks for network input, unmarshalling events as they
come in and storing them in a queue. When there is an event ready, it's
returned. If there is an event in the queue on entry, we use that, not asking
the network for any more. *)
let rec poll sock send close =
  if Queue.is_empty events then
    let events_this_time = ref [] in
      while !events_this_time = [] do
        let recv_result = Unix.recv sock rawbuf 0 (String.length rawbuf) [] in
          buffer := !buffer ^ String.sub rawbuf 0 recv_result;
          let finished = ref false in
          while not !finished do
            match unmarshall !buffer with
            | Some (taken, obj) ->
                buffer := String.sub !buffer taken (String.length !buffer - taken);
                events_this_time =| obj
            | None ->
                set finished
          done
      done;
      iter (fun e -> Queue.add e events) (rev !events_this_time);
      poll sock send close
  else
    Queue.take events

(* Send data to python *)
let rec send_inner sock str startpos =
  if startpos < String.length str then
    let sent =
      Unix.send sock str startpos (String.length str - startpos) []
    in
      send_inner sock str (startpos + sent)
 
let send sock str =
  send_inner sock str 0

(* Create the python process, negotiating an available port *)
let rec connect python python_file port =
  let connected = ref false
  and sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    begin try
      Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
      Unix.listen sock 1;
  Printf.printf "ML: Pytalk.connect: There are %i things in Sys.argv\n" (Array.length Sys.argv);
  flush stdout;
  begin match Sys.argv with
  | [|_; "findport"|] ->
     flprint "ML: Started from Python, stick port number in findport file...\n";
     let file = open_out_bin "findport" in
       output_string file (string_of_int port);
       output_string file "\n";
       close_out file
  | _ ->
     flprint "ML: Started from ML, run python process on command line\n";
     ignore
       (Unix.create_process
          python [|python; python_file; string_of_int port|]
          Unix.stdin Unix.stdout Unix.stderr)
  end;
        let sock, _ = Unix.accept sock in
          Printf.printf "ML: Connected to python on port %i.\n" port;
          connected := true;
          (*i tcp_nodelay sock true; i*)
          let send =
            function x -> send sock (marshall x)
          and close =
            function () ->
              try
                flprint "ML: close() called, closing socket.\n";
                Unix.close sock
              with _ -> ()
          in
            let poll =
              function () -> poll sock send close
            in
              send, poll, close
    with
      Unix.Unix_error (err, ctx1, ctx2) as exn ->
        Printf.printf "connect: Unix error: %s, %s, %s\n" (Unix.error_message err) ctx1 ctx2;
        if not !connected
          then connect python python_file (port + 1)
          else raise exn
    end

type send = Camlpy.marshallable -> unit

type poll = unit -> Camlpy.marshallable

type close = unit -> unit

(* Main function, exposed in the interface. Given the name of the python
interpreter and the main python file, establish a connection *)
let establish_connection python python_file =
  try connect python python_file 50000 with
  | Unix.Unix_error (err, ctx1, ctx2) as exn ->
      Printf.printf "ML: Unix error: %s, %s, %s\n" (Unix.error_message err) ctx1 ctx2;
      raise exn

(* Same, but python starts, giving us the port number *)
(*i let pystarts_establish_connection port =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    flprint "ML: pystarts_establish_connection about to Unix.connect\n";
    Unix.connect sock (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
    flprint "ML: unix.connect has returned\n";
    let send =
      function x -> send sock (marshall x)
    and close =
      function () ->
        try
          flprint "ML: close() called, closing socket.\n";
          Unix.close sock
        with _ -> ()
    in
     let poll =
       function () -> poll sock send close
     in
       send, poll, close i*)

