(* hw1.ml
   Calendar date functions
   Date format: (day, month, year)
*)

let fst3 (x, y, z) = x
let snd3 (x, y, z) = y
let thd3 (x, y, z) = z


(* 1. is_older *)

let is_older ((d1, m1, y1), (d2, m2, y2)) =
  if y1 < y2 then true
  else if y1 > y2 then false
  else if m1 < m2 then true
  else if m1 > m2 then false
  else d1 < d2


(* 2. number_in_month *)

let rec number_in_month (dates, month) =
  if dates = [] then 0
  else
    if snd3 (List.hd dates) = month then
      1 + number_in_month (List.tl dates, month)
    else
      number_in_month (List.tl dates, month)


(* 3. number_in_months *)

let rec number_in_months (dates, months) =
  if months = [] then 0
  else
    number_in_month (dates, List.hd months)
    + number_in_months (dates, List.tl months)


(* 4. dates_in_month *)

let rec dates_in_month (dates, month) =
  if dates = [] then []
  else
    if snd3 (List.hd dates) = month then
      List.hd dates :: dates_in_month (List.tl dates, month)
    else
      dates_in_month (List.tl dates, month)


(* 5. dates_in_months *)

let rec dates_in_months (dates, months) =
  if months = [] then []
  else
    dates_in_month (dates, List.hd months)
    @ dates_in_months (dates, List.tl months)


(* 6. get_nth *)

let rec get_nth (strings, n) =
  if n = 1 then
    List.hd strings
  else
    get_nth (List.tl strings, n - 1)


(* 7. string_of_date *)

let string_of_date date =
  let months =
    ["January"; "February"; "March"; "April";
     "May"; "June"; "July"; "August";
     "September"; "October"; "November"; "December"]
  in
  get_nth (months, snd3 date)
  ^ "-" ^ string_of_int (fst3 date)
  ^ "-" ^ string_of_int (thd3 date)


(* 8. number_before_reaching_sum *)

let rec number_before_reaching_sum (sum, numbers) =
  if List.hd numbers >= sum then
    0
  else
    1 + number_before_reaching_sum
      (sum - List.hd numbers, List.tl numbers)


(* 9. what_month *)

let what_month day =
  let month_days =
    [31;28;31;30;31;30;
     31;31;30;31;30;31]
  in
  let rec helper (day_left, months) =
    if day_left <= List.hd months then
      1
    else
      1 + helper (day_left - List.hd months, List.tl months)
  in
  helper (day, month_days)


(* 10. month_range *)

let rec month_range (day1, day2) =
  if day1 > day2 then
    []
  else
    what_month day1 :: month_range (day1 + 1, day2)


(* 11. oldest *)

let rec oldest dates =
  if dates = [] then
    None
  else
    let rest = oldest (List.tl dates) in
    if rest = None then
      Some (List.hd dates)
    else
      if is_older (List.hd dates, Option.get rest) then
        Some (List.hd dates)
      else
        rest


(* 12. cumulative_sum *)

let cumulative_sum numbers =
  let rec helper (numbers, total) =
    if numbers = [] then
      []
    else
      let new_total = total + List.hd numbers
      in
      new_total :: helper (List.tl numbers, new_total)
  in
  helper (numbers, 0)

  
