
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


let hex_to_bin hex =
  let hex_arr = str_to_word_array hex '`' in
  let bin_arr = Bytes.make (Array.length hex_arr) '\000' in
  for i = 0 to (Array.length hex_arr) - 1 do
    Bytes.set bin_arr i  (Char.chr (int_of_string (Array.get hex_arr i)));
  done;
  bin_arr;
;;

let ad_to_list str =
  let list = ref [] in
      String.iter (fun x -> list := !list @ [(Char.code x)]) str;
      !list;;  

let rec get_hex_str hex_str = function
| [] -> hex_str
| h :: t -> get_hex_str (String.concat "" [string_of_int h; hex_str]) t;;

let bin_to_hex bin =
  let mylist = ref [] in
  mylist := ad_to_list (Bytes.to_string bin);
  mylist := List.rev !mylist;
  get_hex_str "" !mylist;;