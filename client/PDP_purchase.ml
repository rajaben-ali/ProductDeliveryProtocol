open Sodium;;
open Timestamp;;
open Printf;;
open PDP_conversion;;

class info_tezos =
object
        val mutable client_signature = ""
        val mutable client_addr = ""
        val mutable serv_signature = ""
        val mutable contract_address = ""
        val mutable client_name = ""

        method get_client_signature = client_signature
        method get_serv_signature = serv_signature
        method get_contract_address = contract_address
        method get_identity = client_name
        method get_client_addr = client_addr

        method edit_tz_infos the_client_sign the_serv_sign the_contract_addr =
            client_signature <- the_client_sign;
            serv_signature <- the_serv_sign;
            contract_address <- the_contract_addr;
        method edit_identity the_name the_cl_addr =
            client_name <- the_name;
            client_addr <- the_cl_addr;
        method print_all_data =
        sprintf "Client :\nNickname : %s\nSignature : %s\nAddress : %s\n\nServer \
        :\nSignature : %s\nContract Address : %s\n\n" client_signature
        client_addr serv_signature contract_address client_name;


end;;

class production_to_purchase_info =
object
        val mutable price = ""
        val mutable name = ""
        val mutable description = ""
        val mutable pizza_crypted = ""
        val mutable nonce : Box.nonce = Box.random_nonce ()
        val mutable sec_key : secret Box.key option =  None
        val mutable pub_key : public Box.key option = None

        method get_price = price;
        method get_description = description;
        method get_pizza_crypted = pizza_crypted
        method get_nonce = nonce
        method get_sec_key = match sec_key with
            | None -> raise (Invalid_argument "Missing secret key")
            | Some x -> x
        method get_pub_key =  match pub_key with
            | None -> raise (Invalid_argument "Missing public key")
            | Some x -> x


        method edit_infos the_name the_price the_description  =
            name <- the_name;
            price <- the_price;
            description <- the_description;
        method edit_decryp_info the_pizza_crypted the_nonce the_key =
            pizza_crypted <- the_pizza_crypted;
            nonce <- the_nonce;
            match pub_key with
            | Some _ -> ()
            | None -> pub_key <- Some the_key
        method edit_sec_key the_key =
            match sec_key with
            | Some _ -> ()
            | None -> sec_key <- Some the_key
end;;

let display_received_msg ic =
  output_string stdout ((input_line ic) ^ "\n");
  flush stdout
;;

let send_money tz_info pizza_price contract_addr =
  printf "\n\ndisplaying for now instead of withdraw : %s %s %s\n\n" tz_info#get_identity pizza_price contract_addr;
  flush stdout;
;;

let send_msg msg  oc = 
    output_string oc (msg ^ "\n");
    flush oc
;;

let ask_for_pizza oc ic tz_info pizza_price contract_addr =
  let pizza_received = ref [||] in
  send_msg (String.concat "" [tz_info#get_identity;";";tz_info#get_client_addr]) oc;
  printf "\nAs for now, %stz will be withdrawn from your wallet,\
   then you will receive your crypted pizza\n\n" pizza_price;
  flush stdout;
  send_money tz_info pizza_price contract_addr;
  display_received_msg ic;
  print_string "\n\n\tMoney was sent successfuly!\n\
   You will receive the crypted pizza if the required conditions are met.\n\n";
  flush  stdout;
  let str = (input_line ic) in
  pizza_received := (str_to_word_array str ';');
  !pizza_received
;;

let decrypt product =
  print_string "\n\nYou inserted a secret key, decryption in progress . . .\n";
  flush stdout;
  try
  let d_data =
    Box.Bytes.box_open product#get_sec_key product#get_pub_key (hex_to_bin product#get_pizza_crypted) product#get_nonce in
    printf "\nThe pizza is decrypted successfully\n\nDecrypted pizza : %s\n\n" (Bytes.to_string d_data);
  with
    _ -> failwith "Decryption failed!!\n";
;;

let procede_purchase  offer_by_pdp info_received oc ic client_addr =
  let tz_info = new info_tezos in
  let product = new production_to_purchase_info in
  product#edit_infos  !offer_by_pdp.(1) !offer_by_pdp.(3) !offer_by_pdp.(2);
  print_string "\n\nYou accepted the offer from the pdp.\n\nEnter your nickname : ";
  flush stdout;
  tz_info#edit_identity (input_line stdin) client_addr;
  info_received := ask_for_pizza oc ic tz_info product#get_price !offer_by_pdp.(0);(*at 0: pizzacryp|at 1: nonce| at 2: public_key |at 3:timestamp| at 4:sign*)
  print_string "Everything is in order and you received the informations with the verdor's signature.\
  \n\nThe signature will now be sent back ...\n\n";
  flush stdout;
  tz_info#edit_tz_infos client_addr !info_received.(4) !offer_by_pdp.(0);
  product#edit_decryp_info
  !info_received.(0) (Box.Bytes.to_nonce (hex_to_bin !info_received.(1))) (Box.Bytes.to_public_key (hex_to_bin !info_received.(2)));
  send_msg (String.concat ";" [product#get_pizza_crypted; !info_received.(1);(Bytes.to_string (Box.Bytes.of_public_key product#get_pub_key));!info_received.(3);
  tz_info#get_serv_signature;get_timestamp ();tz_info#get_client_signature]) oc;
  print_string "Signature sent back successfully\n"; flush stdout;
  print_string "\nPizza received. Enter the key to decrypt : ";
  flush  stdout;
  product#edit_sec_key (Box.Bytes.to_secret_key (hex_to_bin (input_line stdin)));
  decrypt product;
;;