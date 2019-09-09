open Printf
open Sodium 

class selling_product = 
  object
    val mutable price = 0
    val mutable name = ""
    val mutable descr = ""
    val mutable data = ""
    val mutable data_crypted = ""
    val mutable nonce : Box.nonce = Box.random_nonce ();
    val mutable contract_addr = ""
    val mutable secret_key : secret Box.key option = None
    val mutable public_key : public Box.key option = None

    method create_public_key = public_key <- Some (Box.Bytes.to_public_key (Random.Bytes.generate Stream.key_size));
    method create_secret_key = secret_key <- Some (Box.Bytes.to_secret_key (Random.Bytes.generate Stream.key_size));
    method get_public_key = match public_key with
    | Some x -> x;
    | None -> (Box.Bytes.to_public_key (Random.Bytes.generate Stream.key_size));
    method get_secret_key = match secret_key with
    | Some x -> x;
    | None -> (Box.Bytes.to_secret_key (Random.Bytes.generate Stream.key_size));

    method get_contract_addr = contract_addr
    method get_price = price
    method get_name = name
    method get_descr = descr
    method get_data = data
    method get_data_crypted = data_crypted
    method get_nonce = nonce

    method edit_contract_addr new_addr = contract_addr  <- new_addr
    method edit_price new_price = price  <- new_price
    method edit_name new_name = name  <- new_name
    method edit_descr new_descr = descr <- new_descr
    method edit_data new_data = data <- new_data
    method edit_data_crypted new_data = data_crypted <- new_data

end;;

let user_input product =
  let empty_field_err = "Missing field, please try again.\n" in
  let rec ask_product_name () =
    fprintf stdout "Product's name: ";
    flush stdout;
    let user_input = input_line stdin in
    if user_input = "" then (print_string empty_field_err; flush stdout; ask_product_name ())
    else product#edit_name user_input in
  ask_product_name ();
  
  let rec ask_product_data () =
    fprintf stdout "Product's datas: ";
    flush stdout;
    let user_input = input_line stdin in
    if user_input = "" then (print_string empty_field_err; flush stdout; ask_product_data ())
    else product#edit_data user_input in
  ask_product_data ();
    
  let rec ask_product_descr () =
    fprintf stdout "Product's description: ";
    flush stdout;
    let user_input = input_line stdin in
    if user_input = "" then (print_string empty_field_err; flush stdout; ask_product_descr ())
    else product#edit_descr user_input in
  ask_product_descr ();

  let rec ask_product_price () =
    fprintf stdout "Product's price (in tz): ";
    flush stdout;
    try
      product#edit_price (int_of_string (input_line stdin));
    with _ -> fprintf stdout "Incorrect number, please try again.\n"; flush stdout; ask_product_price () in
  ask_product_price ();
    
  let rec ask_product_addr () =
    fprintf stdout "Product's contract address: ";
    flush stdout;
    let user_input = input_line stdin in
    if user_input = "" then (print_string empty_field_err; flush stdout; ask_product_addr ())
    else product#edit_contract_addr user_input in
  ask_product_addr ();
;;

let product_generation () =
  let product = new selling_product in
  user_input product;
  product;;