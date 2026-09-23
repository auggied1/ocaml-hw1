(* hw1_test.ml
   Tests for hw1.ml
*)

let check name result =
  if result then
    print_endline ("PASS: " ^ name)
  else
    print_endline ("FAIL: " ^ name)


(* 1. is_older *)

let () =
  check "is_older before"
  (is_older ((1,1,2020),(1,1,2021)))

let () =
  check "is_older same date"
  (not (is_older ((1,1,2020),(1,1,2020))))


(* 2. number_in_month *)

let dates =
  [(1,1,2020);
   (5,2,2020);
   (10,2,2021);
   (20,3,2020)]

let () =
  check "number_in_month"
  (number_in_month (dates,2) = 2)


(* 3. number_in_months *)

let () =
  check "number_in_months"
  (number_in_months (dates,[1;2]) = 3)


(* 4. dates_in_month *)

let () =
  check "dates_in_month"
  (dates_in_month (dates,2)
   =
   [(5,2,2020);(10,2,2021)])


(* 5. dates_in_months *)

let () =
  check "dates_in_months"
  (dates_in_months (dates,[1;3])
   =
   [(1,1,2020);(20,3,2020)])


(* 6. get_nth *)

let () =
  check "get_nth"
  (get_nth (["a";"b";"c"],2) = "b")


(* 7. string_of_date *)

let () =
  check "string_of_date"
  (string_of_date (10,9,2015)
   =
   "September-10-2015")


(* 8. number_before_reaching_sum *)

let () =
  check "number_before_reaching_sum"
  (number_before_reaching_sum
     (10,[3;4;5]) = 2)


(* 9. what_month *)

let () =
  check "what_month February"
  (what_month 33 = 2)

let () =
  check "what_month December"
  (what_month 365 = 12)


(* 10. month_range *)

let () =
  check "month_range"
  (month_range (31,34)
   =
   [1;2;2;2])


(* 11. oldest *)

let () =
  check "oldest"
  (oldest
     [(5,5,2020);
      (1,1,2019);
      (9,9,2021)]
   =
   Some (1,1,2019))


let () =
  check "oldest empty"
  (oldest [] = None)


(* 12. cumulative_sum *)

let () =
  check "cumulative_sum"
  (cumulative_sum [12;27;13]
   =
   [12;39;52])

let () =
  check "cumulative_sum empty"
  (cumulative_sum [] = [])

