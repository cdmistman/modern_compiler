(**
  Executable to be executed as a Dune action.
  Downloads merge.tig, queens.tig, and test{1-49}.tig into the `sample_programs`
  subdirectory in this directory's sandbox.
  Note that, since this program runs as a Dune action, we expect to `mkdir sample_programs`
  exactly once, although the output files are truncated.
*)

open Eio
open Cohttp_eio

let host = ("www.cs.princeton.edu", Some 443)

let wrap_tls connection =
  let authenticator ?ip:_ ~host:_ _ = Ok None in
  let mypsk = ref None in
  let ticket_cache =
    {
      Tls.Config.lookup = (fun _ -> None);
      ticket_granted = (fun psk epoch -> mypsk := Some (psk, epoch));
      lifetime = 0l;
      timestamp = Ptime_clock.now;
    }
  in
  let cfg =
    Tls.Config.(
      client ~authenticator ~version:(`TLS_1_0, `TLS_1_3) ?cached_ticket:!mypsk
        ~ticket_cache ~ciphers:Ciphers.supported ())
  in
  let host =
    Result.get_ok
      (Result.bind (Domain_name.of_string @@ fst host) Domain_name.host)
  in
  traceln "host is %s" @@ Domain_name.to_string host;
  Tls_eio.client_of_flow cfg ~host connection

let print_test conn testname =
  traceln "downloading file for test %s" testname;
  let dl_res =
    Client.get ~conn host @@ "/~appel/modern/testcases/" ^ testname ^ ".tig"
  in
  traceln "received test file for `%s`" testname;
  let contents = Client.read_fixed dl_res in
  print_endline ("let " ^ testname ^ " = \"" ^ String.escaped contents ^ "\";;")

let main ~env ~sw =
  traceln "starting...";
  let tests =
    List.append [ "merge"; "queens" ]
      (List.init 49 @@ fun i -> "test" ^ string_of_int (i + 1))
  in
  traceln "downloading %d tests" @@ List.length tests;
  let host_entry = Unix.gethostbyname @@ fst host in
  traceln "got host entry";
  let addr = `Tcp (Eio_unix.Ipaddr.of_unix host_entry.h_addr_list.(0), 443) in
  traceln "connecting";
  let tcp_conn = Net.connect ~sw env#net addr in
  traceln "initiating tls handshake";
  let conn = wrap_tls tcp_conn in
  (* TODO: use Fiber.fork once cohttp_eio supports http multiplexing (not
     enabled in 6.0.0~alpha0) *)
  List.iter (print_test conn) tests

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run ~sleep:(Int64.of_int 1000)
    (module Mirage_crypto_rng.Fortuna)
    env
  @@ fun () ->
  Switch.run @@ fun sw -> main ~env ~sw
