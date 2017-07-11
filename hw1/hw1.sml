(* Tejveer Rai - HW1 *)

(*1. is_older *)
fun is_older (x: (int*int*int), y:(int*int*int)) : bool =
  if
      ((#3 x) < (#3 y))
  then true
  else
      if
	  ((#3 x) = (#3 y)) andalso ((#2 x) < (#2 y))
      then
	  true
      else
	  if
	      ((#3 x) = (#3 y)) andalso ((#2 x) = (#2 y)) andalso ((#1 x) < (#1 y))
	  then
	      true
	  else
	      false

(*2. number_in_month *)
fun number_in_month (dates : (int*int*int) list, month : int) : int =
  if
      null(dates)
  then 0
  else if
      #2 (hd(dates)) = month
  then
      1 + number_in_month(tl(dates), month)
  else
      0 + number_in_month(tl(dates), month)

(*3. number_in_months *)
fun number_in_months (dates : (int*int*int) list, months : int list) : int = 
  if null(dates)
  then 0
  else if null(months)
  then 0
  else
      number_in_month(dates, hd(months)) + number_in_months(dates, tl(months)) 

(*4. dates_in_month *)
fun dates_in_month (dates : (int*int*int) list, month : int) : ((int*int*int) list) =
  if null(dates)
  then []
  else if
      #2 (hd(dates)) = month
  then
     (hd(dates)) :: dates_in_month((tl(dates)),month)
  else
       dates_in_month((tl(dates)), month)
	  
(*5. dates_in_months *)
fun dates_in_months (dates : (int*int*int) list, months : int list) : ((int*int*int) list) =
  if null(dates)
  then []
  else if null(months)
  then []
  else
      dates_in_month(dates, hd(months)) @  dates_in_months(dates, tl(months)) 

(*6. get_nth *)
fun get_nth (strings : string list, n : int) : string =
  if n=1
  then
      hd(strings)
  else
      get_nth(tl(strings),(n-1))

(*7. date_to_string *)
fun date_to_string (date : (int*int*int)) : string =
  let
      val months : string list = ["January","February","March","April","May","June","July","August","September","October","November","December"]
  in
      get_nth(months,(#2 date))^"-"^Int.toString((#1 date))^"-"^Int.toString((#3 date))
  end
      
(*8. number_before_reaching_sum	*)						  
fun number_before_reaching_sum (sum : int, intlist : int list) : int =
  if
      sum <= (hd(intlist))
  then 0
  else
      1 + number_before_reaching_sum( (sum-hd(intlist)) , tl(intlist) )

(*9. what_month*)
fun what_month (day: int) : int =
  let
      (* number of days in each month *)
      val months : int list = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      1+number_before_reaching_sum(day, months)
  end

(*10. month_range *)
fun month_range (day1 : int, day2 : int) : int list =
  if
      day1>day2
  then []
  else
      what_month(day1) :: month_range(day1+1,day2)

(*11. oldest *)
fun oldest (dates : (int*int*int) list) : (int*int*int) option =
  if null(dates)
  then NONE
  else if null(tl(dates))
  then
      SOME(hd(dates))
  else if
      is_older(hd(dates), hd(tl(dates)))
  then
      SOME(hd(dates)) 
  else 
      oldest(tl(dates))

(*12. cumulative_sum *)
fun cumulative_sum (ints : int list) : int list =
  let
      fun add (x:int, y:int list) : int list =
	if null(y)
	then []
	else
	     ([x+hd(y)]) @ add(x+hd(y), tl(y))
  in
      if null(tl(ints))
      then
	  hd(ints)::[]
      else
	  hd(ints)::add(hd(ints),(tl(ints)))
  end
      

     
      
	  
