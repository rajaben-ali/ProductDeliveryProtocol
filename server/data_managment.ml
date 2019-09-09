open Sodium
open Timestamp

let signature = "tz1ZeaFExmxVdcti9tWsNPmNrKE44j3u6gxy";;

let str_to_word_array str sep =
  let str_size = ((String.length str) - 1) in
  let result = ref [||] in
  let keep = ref "" in
  let add_to_list list str_to_add =
      Array.append list [|str_to_add|] in
  let end_loop_cases keep count result = match !keep with
      | "" -> !result
      | _ -> keep := !keep ^ (String.make 1 str.[count]);
      result := (add_to_list !result !keep); keep := ""; !result
  in
  let rec stw_loop count progress =
      if (str_size = count) then (end_loop_cases keep count result)
      else
          (if (str.[count] == sep)
              then (stw_loop (count + 1) progress)
          else
              (if ((count + 1) = String.length str || str.[count + 1] == sep)
                  then (keep := !keep ^ (String.make 1 str.[count]);
                      result := (add_to_list !result !keep); keep := "";
                      (stw_loop (count + 1) (progress + 1)))
              else
                      (keep := !keep ^ (String.make 1 str.[count]);
                      (stw_loop (count + 1) progress))))
  in
  stw_loop 0 0
;;

let print_list list =
  List.map (fun x -> Printf.printf "%X" x) list;;

let ad_to_list str =
let list = ref [] in
    String.iter (fun x -> list := !list @ [(Char.code x)]) str;
    !list;;

let rec get_hex_str hex_str = function
| [] -> hex_str
| h :: t -> get_hex_str (String.concat "`" [string_of_int h; hex_str]) t;;

let bin_to_hex bin =
    let mylist = ref [] in
    mylist := ad_to_list (Bytes.to_string bin);
    mylist := List.rev !mylist;
    get_hex_str "" !mylist;;

let crypt_my_data product =
  let crypted = Box.Bytes.box product#get_secret_key product#get_public_key (Bytes.of_string product#get_data)
  product#get_nonce in
  crypted;;

let send_data product outchan =
    product#edit_data_crypted (Bytes.to_string (crypt_my_data product));
    let data = (bin_to_hex (Bytes.of_string product#get_data_crypted)) in
    let nonce = (bin_to_hex (Box.Bytes.of_nonce product#get_nonce)) in
    let pub_key = (bin_to_hex (Box.Bytes.of_public_key product#get_public_key)) in
  output_string outchan (String.concat ";" [data;nonce;pub_key;get_timestamp ();signature;"\n"]);
  flush outchan;