let e = 0.001
//var 2, f - (2, 3, 4)
let f1 x = cos(x) - exp(-(x**2.)/2.) + x - 1.
let f2 x = 1. - x + sin(x) - log(1. + x)
let f3 x = 3. * x - 14. + exp(x) - exp(-x)

let df1 x = x * exp(-x**2./2.) - sin(x) + 1.
let df2 x = cos(x) - 1. - 1./(x + 1.)
let df3 x = exp(x) + 3. + exp(-x)

let phi1 x = -cos(x) + exp(-(x**2.)/2.) + 1.
let phi2 x = 1. + sin(x) - log(1. + x)
let phi3 x = log(-(3. * x - 14. - exp(-x)))

let sgn a = if a > 0. then 1. else if a < 0. then -1. else 0.

let rec bis f a b = 
  if sgn(f a) = sgn(f b) then
    
    b
  else
    let c = (a + b) / 2. 
    if f c = 0. || (c - a) < e then
      c
    else if ((f a) * (f c) < 0.) then
      bis f a c
    else
      bis f c b

let iter f a b = 
  let rec func f x = 
    let new_x = f x
    if abs(new_x - x) < e then
      new_x
    else
      func f new_x
  
  let x = (a + b) / 2.
  func f x

let nueton f df a b =
  let rec func f df x = 
    let new_x = x - (f x) / (df x)
    if abs(new_x - x) < e then new_x
    else 
      func f df new_x
      
  let x = (a + b) / 2.
  func f df x
  
  
let main = 
  printfn "---------------------------------------------------------------------------"
  printfn "|    f    |  a  |  b  |    Diho    |    Iter    |   Nueton   |  Apr  Val  |"
  printfn "---------------------------------------------------------------------------"
  printfn "|    1    |  1  |  2  |  %10.6f|  %10.6f|  %10.6f|  %10.6f|" (bis f1 1. 2.) (iter phi1 1. 2.) (nueton f1 df1 1. 2.) 1.0804
  printfn "---------------------------------------------------------------------------" 
  printfn "|    2    |  1  | 1.5 |  %10.6f|  %10.6f|  %10.6f|  %10.6f|" (bis f2 1. 1.5) (iter phi2 1. 1.5) (nueton f2 df2 1. 1.5) 1.1474
  printfn "---------------------------------------------------------------------------"
  printfn "|    3    |  1  |  3  |  %10.6f|  %10.6f|  %10.6f|  %10.6f|" (bis f3 1. 3.) (iter phi3 1. 3.) (nueton f3 df3 1. 3.) 2.0692
  printfn "---------------------------------------------------------------------------"


main
