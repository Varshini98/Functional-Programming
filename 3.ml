let rec count x l =
     if l = [] then 0
     else (if (List.hd l) = x then (1 + count x (List.tl l)) else (count x (List.tl l)));;
           

let rec dele x l =
    if l =[] then []
    else (if (List.hd l) = x then  (dele x (List.tl l)) else (List.hd l)::(dele x (List.tl l)));;



let rec encode l =
    if l = [] 
       then []
    else
       ((List.hd l),(count (List.hd l) l))::(encode (dele (List.hd l) l));;


