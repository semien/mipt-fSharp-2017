module lab1

open System
open System.Net
open System.IO
open System.Collections.Specialized 

// почтовый адрес
let email = "semien9797@gmail.com"
// общий тип для возвращаемых вашими функциями значений, где первая часть кортежа это само значение функции, вторая - кол-во операций
type Result = float * int
let delta = 1e-10

// *** Первая часть

let fTailor x : float = sinh(x) // функция, которую раскладываем
let n, a, b = 20., 0., 1. // интервал

let tailor x : Result = 
    let rec tailor1 (result:float) (m:float) (n:int) x = 
        let add = (m*x*x)/float(n*(n-1))
        match add with
        | add when (add > delta) -> tailor1 (result + add) add (n+2) x
        | _ -> (result + add, (n+1)/2)
    tailor1 x x 3 x

let tailorA x : Result = 
    let rec fac (r:float): float = if r <= 0. then 1. else r * fac (r-1.)
    let rec tailor1 (result:float) (n:int) x =
        let add = (x ** float(n))/(fac (float n))
        if (add > delta)
            then tailor1 (result + add) (n+2) x
        else (result + add, (n+1)/2)
    tailor1 0. 1 x

let printTailor () = 
    [a .. (b-a)/n .. b] 
    |> List.map (fun x -> let (firstRes, firstCou), (secondRes, secondCou) = tailor x, tailorA x in (x, firstRes, firstCou, secondRes, secondCou, fTailor x))
    |> List.iter (fun (a,b,c,d,e,f) -> printf "%f\t%f\t%d\t%f\t%d\t%f\n" a b c d e f )
// *** Вторая часть

let fSolve1i = fun x -> -cos( x ** 0.52 + 2.)
let fSolve1 = fun x -> x + cos( x ** 0.52 + 2.)
let dfSolve1 = fun x -> 1. - sin( x ** 0.52 + 2.)*0.52*(x ** -0.48)

let fSolve2 = fun x -> 3. * (Math.Log x) ** 2. + 6.*(Math.Log x) - 5.
let fSolve2i = fun x -> x - 0.2 * fSolve2 x
let dfSolve2 = fun x -> 6./x * (Math.Log x + 1.)

let fSolve3 = fun x -> 0.6 * (3. ** x) - 3.-2.3*x
let fSolve3i = fun x -> x - 0.2 * fSolve3 x
let dfSolve3 = fun x -> 0.6*(3. ** x)/log(3.) - 2.3

let iter f a b : Result = 
    let rec iter' x num =
        if (abs(x - f(x))> delta)
            then iter' (f(x)) (num+1)
            else (f x, num + 1)
    iter' ((a+b)/2.) 0

let newton f a b df : Result = 
    let rec newton' x num = 
        if (abs(f(x) - 0.)> delta)
            then newton' (x - f(x)/df(x)) (num+1)
            else (x,num)
    newton' ((a+b)/2.) 0

let dichotomy =
    // для функций с аккумулятором удобно ставить его в начало
    let rec dichotomyA (i:int) (f:float->float) (a:float) (b:float) =
        let middle = (a+b)/2.
        if (abs(f(middle))<delta) then middle, i
        elif (f(middle)*f(a) < 0.)
                then dichotomyA (i+1) f a middle
                else dichotomyA (i+1) f middle b
    dichotomyA 0 // чтобы воспользоваться каррированием

let printSolve () =
    ((iter fSolve1i 0.5 1.)::(newton fSolve1 0.5 1. dfSolve1)::(dichotomy fSolve1 0.5 1.)::(iter fSolve2i 1. 3.)::(newton fSolve2 1. 3. dfSolve2)::(dichotomy fSolve2 1. 3.)::(iter fSolve3i 2. 3.)::(newton fSolve3 2. 3. dfSolve3)::(dichotomy fSolve3 1. 3.)::[])
    |> List.iter (fun (res, cou) -> printf "%f\t%d\n" res cou)

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.143.158:13666/lab1"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString