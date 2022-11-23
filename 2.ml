let rec ser y l =
    if l = [] then 0
    else
    (if (List.hd l) = y then 1 else (ser y (List.tl l)));;



let rec findpos  y l=
  if (ser y l) = 0 then 0
  else 
      (if (List.hd l) = y then 1
       else 1 + findpos  y (List.tl l));;



let rec delete x l = 
    if (findpos x l) = 0
    then l
    else  (if (findpos x l)  = 1 then (delete x (List.tl l)) else (List.hd l)::(delete x (List.tl l)));;





