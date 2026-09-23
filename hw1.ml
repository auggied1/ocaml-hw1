(* CS 314, Homework 1 *)

(* Use these functions to extract parts of a date *)
let fst3 (x,_,_) = x
let snd3 (_,x,_) = x
let thd3 (_,_,x) = x


(* 1. is_older *)

let is_older ((date1 : int * int * int), (date2 : int * int * int)) =
  if thd3 date1 < thd3 date2 then
    true
  else if thd3 date1 > thd3 date2 then
    false
  else if snd3 date1 < snd3 date2 then
    true
  else if snd3 date1 > snd3 date2 then
    false
  else
    fst3 date1 < fst3 date2


(* 2. number_in_month *)

let rec number_in_month ((dates : (int * int * int) list), (month : int)) =
  if dates = [] then
    0
  else
    let first = List.hd dates in
    let rest = List.tl dates in
    if snd3 first = month then
      1 + number_in_month (rest, month)
    else
      number_in_month (rest, month)


(* 3. number_in_months *)

let rec number_in_months ((dates : (int * int * int) list), (months : int list)) =
  if months = [] then
    0
  else
    number_in_month (dates, List.hd months)
    + number_in_months (dates, List.tl months)


(* 4. dates_in_month *)

let rec dates_in_month ((dates : (int * int * int) list), (month : int)) =
  if dates = [] then
    []
  else
    let first = List.hd dates in
    let rest = List.tl dates in
    if snd3 first = month then
      first :: dates_in_month (rest, month)
    else
      dates_in_month (rest, month)


(* 5. dates_in_months *)

let rec dates_in_months ((dates : (int * int * int) list), (months : int list)) =
  if months = [] then
    []
  else
    dates_in_month (dates, List.hd months)
    @ dates_in_months (dates, List.tl months)


(* 6. get_nth *)

let rec get_nth ((strings : string list), (n : int)) =
  if n = 1 then
    List.hd strings
  else
    get_nth (List.tl strings, n - 1)


(* 7. string_of_date *)

let string_of_date (date : int * int * int) =
  let months =
    ["January"; "February"; "March"; "April";
     "May"; "June"; "July"; "August";
     "September"; "October"; "November"; "December"]
  in
  get_nth (months, snd3 date)
  ^ "-" ^ string_of_int (fst3 date)
  ^ "-" ^ string_of_int (thd3 date)


(* 8. number_before_reaching_sum *)

let rec number_before_reaching_sum ((sum : int), (numbers : int list)) =
  if List.hd numbers >= sum then
    0
  else
    1 + number_before_reaching_sum
      (sum - List.hd numbers, List.tl numbers)


(* 9. what_month *)

let what_month day =
  let days_in_month =
    [31;28;31;30;31;30;
     31;31;30;31;30;31]
  in
  let rec helper (days_left, months_left) =
    if days_left <= List.hd months_left then
      1
    else
      1 + helper
        (days_left - List.hd months_left, List.tl months_left)
  in
  helper (day, days_in_month)


(* 10. month_range *)

let rec month_range ((day1 : int), (day2 : int)) =
  if day1 > day2 then
    []
  else
    what_month day1 :: month_range (day1 + 1, day2)


(* 11. oldest *)

let rec oldest (dates : (int * int * int) list) =
  if dates = [] then
    None
  else
    let first = List.hd dates in
    let rest = oldest (List.tl dates) in
    if rest = None then
      Some first
    else
      if is_older (first, Option.get rest) then
        Some first
      else
        rest


(* 12. cumulative_sum *)

let cumulative_sum numbers =
  let rec helper (nums, total) =
    if nums = [] then
      []
    else
      let new_total = total + List.hd nums in
      new_total :: helper (List.tl nums, new_total)
  in
  helper (numbers, 0)
  
