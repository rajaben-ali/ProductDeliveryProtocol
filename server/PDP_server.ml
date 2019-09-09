open Printf
open Sodium
open Data_managment
open Product
open Client

module Static = Ctypes_static

let signature_phase outchan inchan product client =
  send_data product outchan;
  client#edit_proof (input_line inchan);;

let introduction () =
  fprintf stdout "Welcome to our Product Devlivery Protocol!\n\n";
  flush stdout;
  fprintf stdout "To sell something you'll need to type de description of the product and specify his cost in tz.\n";
  flush stdout;
  fprintf stdout "If a customer is found, he'll connect to the service and start a new contract.\n";
  flush stdout;;

let welcome_message outchan client =
  let message = (String.concat "" [client#get_name;" is your client with the address: ";client#get_addr;"\n"]) in
  print_string message;
  flush stdout;
  output_string outchan (Printf.sprintf "Welcome %s! and thank you for your \
  purchase.\n" client#get_name);
  flush outchan;;

let establish_connexion =
  introduction ();
  let product = product_generation () in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", 8080));
  fprintf stdout "Waiting for client's connexion...\n";
  flush stdout;
  Unix.listen sock 5;
  while true do
      let (s, _caller) = Unix.accept sock in
      match Unix.fork() with
        0 -> if Unix.fork() <> 0 then exit 0;
          let inchan = Unix.in_channel_of_descr s in
          let outchan = Unix.out_channel_of_descr s in
          product#create_public_key;
          product#create_secret_key;
          fprintf stdout "A Client is connected!\n\n";
          flush stdout;
          let answer = make_offer product outchan inchan in
          if answer = "Refused" then (print_string
          "The Client refused the offer, closing...\n"; flush stdout; ())
          else let client = get_new_client answer in
          welcome_message outchan client;
          signature_phase outchan inchan product client;
          print_string "BLOCKCHAIN secret_key = " ;
          print_endline (bin_to_hex (Box.Bytes.of_secret_key product#get_secret_key));
          flush stdout;
        | id -> Unix.close s; ignore(Unix.waitpid [] id)
  done;;