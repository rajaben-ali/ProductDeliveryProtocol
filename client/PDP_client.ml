open Printf;;
open PDP_purchase;;
open PDP_conversion;;

let ask_for_retry () =
    printf "Unable to connect to the server !\nWould you like to retry ? [y/n] ";
    flush stdout;
    let rep = input_line stdin in
    if (String.compare rep "y" == 0) then 1
    else 0
;;

let ask_for_procede () =
    printf "Would you like to procede ? [y/n] ";
    flush stdout;
    let rep = input_line stdin in
    if (String.compare rep "y" == 0) then 1
    else 0
;;

let offer_refused oc =
    print_string "You refused the offer by the Pizza Delivry Protocol\n\n";
    print_string "Closing in progress . . .\n\n";
    flush stdout;
    (send_msg "Refused" oc);
;;

let rec client_func () =
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    printf "\nWelcome to our Pizza Delivery Protocol, PDP, written in OCaml.\n\n
    Establishing connexion to the server ...\n\n";
    flush stdout;
    try
        Unix.connect sock (Unix.ADDR_INET (Unix.inet_addr_of_string "192.168.1.1", 8080));
        let offer_by_pdp = ref [||] in
        let info_received = ref [||] in
        let oc = (Unix.out_channel_of_descr sock) in
        let ic = (Unix.in_channel_of_descr sock) in
        let client_addr = "KT1WMW94x4HzboffkfYmFVceHMPDDvtVUVaY" in
        print_string "Looking for an offer from the PDP ...\n\n";
        flush stdout;
        offer_by_pdp := (str_to_word_array (input_line ic) ';'); (*index 0: contract addr | index 1: name| index 2: descrip |index 3 : price*)
        printf "Here is the offer :\n\n\tName : %s\n\n\tDescription : %s\n\n\tPrice \
        : %stz\n\n\tContract address : %s\n\n"
        !offer_by_pdp.(1) !offer_by_pdp.(2) !offer_by_pdp.(3) !offer_by_pdp.(0);
        flush stdout;
        if (ask_for_procede ()) == 0 then (offer_refused oc ;())
        else
            (procede_purchase offer_by_pdp info_received oc ic client_addr)
            
    with
        _ -> Unix.close sock ; if ((ask_for_retry ()) == 0) then ()
        else client_func ()
;;

client_func ();;