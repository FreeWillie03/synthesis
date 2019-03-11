module Synthesis

let abelar a =  //Create a function abelar to return true if the input is greater than 12, and less than 3097, and is a multiple of 12.
 ((a > 12) && (a < 3097)) && (a % 12 = 0) // [target: 1 line]
    

let area a b =  // Create a function area which, given a base and height, finds the area of a triangle. If either the base or the height is negative, throw an exception. 
   match (a<0.0)||(b<0.0) with             //[target: 3 lines] 
|  true -> failwith "Cannot be negitive"
|  _ ->  ((a/2.0)*b)

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
    
   //Done without tests

let digits a = //Create a function ​digits​ to count the number of digits in a number.  The input maybe positive or negative. ​
   let rec dig t i =  // [target: 5 lines]
      match (t<10)&&(t> -10) with 
      |true -> i+1 
      |_ -> dig (t/10) (i+1)
   match (a<10)&&(a> -10) with 
  |true -> 1 
  |_ -> dig (a/10) (1)

let minmax (a,b,c,d) = //Create a function ​minmax​ which finds the largest and smallest values out of fourvalues that are provided. ​[target: 3 lines]
    (min (min a b) (min c d)),(max (max a b) (max c d))


let isLeap a = //Create a function ​isLeap​ which returns true if the given year is a leap year.  
    match (a>=1582)&&(a>0) with //Everyyear that is divisible by 4 is a leap year, unless it is also divisible by 100.
    |true -> match (a%4=0) with//However, if it is also divisible by 400, then it is still a leap year. 
      |true -> match (a%100=0)&&(a%400=0) with |true -> true |_ -> (a%100<>0)   //The function should throw an exception of the input year is less than 1582. [target: 5 lines]
      |_ -> false   
    |_ -> failwith "less than 1582 or 0"   
      
    
let month a = match a with  
      |1 -> ("January",31) 
      |2 -> ("February",28) 
      |3 -> ("March",31) 
      |4 -> ("April",30) 
      |5 -> ("May",31) 
      |6 -> ("June",30) 
      |7 -> ("July",31) 
      |8 -> ("August",31) 
      |9 -> ("September",30)
      |10 -> ("October",31) 
      |11 -> ("November",30)
      |12 -> ("December",31)
      |_ -> failwith "cannot be less then 0 ot greater then 12"  //Create a function ​month​ which accepts an integer between 1 and 12 inclusive, and returns the corresponding month and the number of days in that month,
   //assuming that it is ​not​ a leap year.  If an integer less than 1 or greater than 12 is supplied, anexception should be thrown.//[target: 13 lines]

let toBinary a = //Create a function ​toBinary​ which converts a positive integer to a binary string.Throw an exception if a negative integer is supplied.
  let rec bin i =
    match i with
    | 0 | 1 -> string i
    | _ ->
        let bit = string (i % 2)
        (bin (i / 2)) + bit
  match (a>=0) with
  |true -> bin a 
  |_ -> failwith "num < 0 exception"


let bizFuzz n = //Create a function ​bizFuzz ​to accept an integer ​n​ and return the number of times anumber between 1 and ​n​ inclusive is divisible by 3, divisible by 5, and divisible byboth 3 and 5.//[target: 10 lines]
 failwith "not implemented"

let monthDay _ _ = //Create a function ​monthDay​ which accepts an integer ​d​ and a year ​y​, and returns astring for the month that the day ​d​ falls within. 
   failwith "Not implemented" //The function must accept a range ofd​ from 1 to 365 if ​y​ isn’t a leap year, and must accept ​d​ between 1 and 366 if ​y​ is aleap year.
//If ​d​ is out of range, or if ​y​ is less than 1582, then an exception must bethrown.  
//Remember that:a.April, June, September, and November have 30 days.
//b.January, March, May, July, August, October, and December have 31 days.
//c.February has 29 days in a leap year, and 28 days otherwise
    

let coord _ = failwith "Not implemented" //Create a function ​coord​ which is given a Cartesian coordinate and returns functionsto calculate:
//a.the straight-line distance to another Cartesian coordinate, as calculated by.
//istd=√(xx)(yy)1−22+1−22Hint​: ​you developed a square-root function in a tutorial on page 35 of yourtextbook​.
//b.whether a rectangle (described by top-left coordinate, width, and height, inthat order) will contain the initial coordinate.
//[target: 6 lines, excluding ​sqrt​ function]
    