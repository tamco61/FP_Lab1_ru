let a = 0.0
let b = 0.5
let n = 10.
let e = 0.001

let f x = log((1.0 + x) / (1.0 - x))

let rec loop i n func x =
  if i <= n then
    let x = func x
    loop (i + 1.) n func x
  else
    x

let m a b = a * b

let pow x n = loop 1. n (m x) x

let rec taylor x i e sum = 
  let last_y = (pow x (2. * (i) + 1.)) / (2. * (i) + 1.)
  let new_y = (pow x (2. * (i + 1.) + 1.)) / (2. * (i + 1.) + 1.)
  if (abs(new_y - last_y) < e) then
    (2.*(sum + last_y), i)
  else
    let sum = sum + last_y
    taylor x (i + 1.) e sum


let rec taylorSmart x last_y i e sum = 
  let new_y = pow x 2. * last_y * (2.*(i - 1.) + 1.)  / (i * 2. + 1.)
  if (abs(new_y - last_y) < e) then
    (2. * sum, (i - 1.) / 2.)
  else
    let sum = sum + new_y
    taylorSmart x new_y (i + 2.) e sum

let osnova i = 
  let x = a+(float i)/(float n)*(b-a)
  let t, ti = taylor x 0. e 0.
  let tS, tSi = taylorSmart x x 1. e x  
  printfn "|%5.2f|  %10.6f|  %10.6f|   %10.0f|  %10.6f|   %10.0f|" x (f x) t ti tS tSi
  i + 1.


let main =
    printfn "--------------------------------------------------------------------------"
    printfn "|  x  |    f(x)    |   Naive    |    Iters    |   Smart    |    Iters    |"
    printfn "--------------------------------------------------------------------------"
    loop 0. n osnova 0.
    printfn "--------------------------------------------------------------------------"

main
