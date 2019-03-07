module Synthesis

let abelar a =  //Create a function abelar to return true if the input is greater than 12, and less than 3097, and is a multiple of 12.
    match ((a > 12) && (a < 3097)) && (a % 12 = 0) with | true -> true | _ -> false // [target: 1 line]
    

let area a b =  // Create a function area which, given a base and height, finds the area of a triangle. If either the base or the height is negative, throw an exception. 
    match (a<0.0)||(b<0.0) with             //[target: 3 lines] 
    | true -> failwith "Cannot be negitive"
    | _ ->  ((a/2.0)*b)

let zollo a =           //Create a function zollo which returns a positive number if given a negative number,or doubles the number if the input is positive.
    match (a<0) with                 //[target: 3 lines]
    |true -> (a*(-1))
    |_ -> (a*2)
    

let min a b =   //Create a function min which chooses the smaller of two values.
    match (a<b) with // [target: 3 lines]
    |true -> a
    |_ -> b

let max a b =   //Create a function max which chooses the larger of two values.
    match (a>b) with // [target: 3 lines]
    |true -> a
    |_ -> b

let ofTime h m s = //Create a function ofTime to convert hours, minutes, and seconds to a number of seconds.
    ((h*60)*60) + (m*60) + (s)                           //[target: 1 line]

let toTime s =      //Create a function toTime to convert a number of seconds to hours, minutes, and seconds.
     match (s>0) with   //[target: 7 lines]
     |false -> 0,0,0
     |true -> 
     let h = (s/3600)  
     let m = ((s-(3600*h))/60)
     let sec = (s-(3600*h)-(m*60))
     h,m,sec
     
   

let digits _ =
    failwith "Not implemented"

let minmax _ =
    failwith "Not implemented"

let isLeap _ =
    failwith "Not implemented"

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"