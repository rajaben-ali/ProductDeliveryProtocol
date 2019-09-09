open Data_managment

class client (name : string) (addr : string) = 
object 
  val mutable name = name
  val mutable addr = addr
  val mutable signed_proof = ""

  method get_proof = signed_proof
  method edit_proof new_data = signed_proof <- new_data
  method get_name = name
  method get_addr = addr
end;;

let get_new_client data =
  let my_list = str_to_word_array data ';' in
  let client = new client my_list.(0) my_list.(1) in
  client;;  

let make_offer product outchan inchan =
  output_string outchan (String.concat ";" [product#get_contract_addr;";";product#get_name;";";
  product#get_descr;";";(string_of_int product#get_price);"\n"]);
  flush outchan;
  let answer = input_line inchan in
  answer;;