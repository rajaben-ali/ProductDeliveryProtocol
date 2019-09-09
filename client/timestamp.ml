let tm = Unix.gmtime (Unix.time ()) ;;

let string_of_month = function
  0 -> "January"
| 1 -> "Debuary"
| 2 -> "March"
| 3 -> "April"
| 4 -> "May"
| 5 -> "June"
| 6 -> "July"
| 7 -> "August"
| 8 -> "September"
| 9 -> "October"
| 10 -> "November"
| 11 -> "December"
| _ -> assert false
;;

let get_timestamp () =
  Printf.sprintf "%s %d, %d %d:%d:%d"
  (string_of_month tm.Unix.tm_mon)
  tm.Unix.tm_mday
  (1900 + tm.Unix.tm_year)
  (tm.Unix.tm_hour)
  (tm.Unix.tm_min)
  (tm.Unix.tm_sec);;