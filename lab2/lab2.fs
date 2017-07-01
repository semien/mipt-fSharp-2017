open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

// почтовый адрес
let email = "semien9797@gmail.com"

type JSON = Null | Boolean of bool | String of string | Number of int
            | Array of JSON list | Object of (string * JSON) list
            
type Token = Null | OpenBrace | CloseBrace | OpenBracket | CloseBracket
             | Colon | Comma | Boolean of bool | String of string 
             | Number of int

let explode (s:string) = [for i in s -> i]

let tokenize source =
    let rec parseNumber acc = function
        | c :: t when List.exists (fun x -> x = c) [','; '}'; ']' ] -> (acc, c :: t)
        | c :: t -> parseNumber (acc + c.ToString()) t
        | [] -> parseNumber (acc) []
        | _ -> failwith "parseNumber failed"
    let rec parseString acc = function
        | '\\' :: 'n' :: t -> parseString (acc + "\n") t
        | '"' :: t -> (acc, t) //"
        | c :: t -> parseString (acc + c.ToString()) t
        | _ -> failwith "parseString failed"
    let rec tokenize' acc = function
        | w :: t when Char.IsWhiteSpace(w) -> tokenize' acc t
        | '{' :: t -> tokenize' (OpenBrace :: acc) t
        | '}' :: t -> tokenize' (CloseBrace :: acc) t
        | '[' :: t -> tokenize' (OpenBracket :: acc) t
        | ']' :: t -> tokenize' (CloseBracket :: acc) t
        | ':' :: t -> tokenize' (Colon :: acc) t
        | ',' :: t -> tokenize' (Comma :: acc) t
        | 't' :: 'r' :: 'u' :: 'e' :: t -> tokenize' (Boolean true :: acc) t
        | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: t -> tokenize' (Boolean false:: acc) t
        | 'n' :: 'u' :: 'l' :: 'l' :: t -> tokenize' (Null :: acc) t
        | '"' :: t -> //"
            let (s, t') = parseString "" t
            tokenize' (String s :: acc) t'
        | c :: t when Char.IsDigit(c) ->
            let (n, t') = parseNumber (c.ToString()) t
            let num = int n
            tokenize' (Number num :: acc) t'
        | [] -> List.rev acc
        | _ -> failwith "Parse error"
    tokenize' [] source

let parse str = 
    let rec parseObject (obj:(string*JSON) list) = 
        let rec parsePair =
            let rec parseValue =
                let rec parseArray acc = 
                    function
                    | (Token.OpenBracket | Token.Comma)::ts -> 
                        let (parsed, ts') = parseValue ts
                        parseArray (parsed::acc) ts'
                    | Token.CloseBracket::t -> (JSON.Array (List.rev acc), t)
                function 
                | Token.OpenBrace::t -> 
                    let (parsed, t') = parseObject [] (Token.OpenBrace::t)
                    (parsed, t')
                | Token.OpenBracket::t -> 
                    let (parsed, t') = parseArray [] (Token.OpenBracket::t)
                    (parsed, t')
                | Token.Null::t -> (JSON.Null, t)
                | Token.Boolean bool::t -> (JSON.Boolean bool, t)
                | Token.String str::t -> (JSON.String str, t)
                | Token.Number num::t -> (JSON.Number num, t)
            function
            | Token.String name::Colon::t -> 
                let (parsed, t') = parseValue t
                ((name, parsed), t')
        function
        | (Token.OpenBrace | Token.Comma)::t -> 
            let (parsed, t') = parsePair t
            parseObject (parsed::obj) t'  
        | Token.CloseBrace::t -> (JSON.Object (List.rev obj), t)           
    match parseObject [] (tokenize (explode str)) with 
    res, [] -> res
    | _ , _ -> failwith "Wrong token list"


let lab3 obj1 obj2 = 
    match (obj1,obj2) with
    | (JSON.Object(x1),JSON.Object(x2)) ->
        let rec keys acc = function
            | (key,obj)::t -> keys (key::acc) t
            | [] -> acc
        let rec commonKeys acc list1 = function
            | x::[] -> 
            if (List.exists (fun elem -> elem = x) list1) then (x::acc) else acc
            | x::xs -> if ((List.exists (fun elem -> elem = x) list1))
                then
                    if (x.Equals(List.head xs)) then commonKeys acc list1 xs
                    else commonKeys (x::acc) list1 xs
                else commonKeys acc list1 xs
            | [] -> acc
        commonKeys [] (keys [] (List.sort x1)) (keys [] (List.sort x2)) 
    | _,_ -> []

let rec stringify = 
    let rec strObj (acc:string) = function
        | (key,obj)::[] -> acc + "\"" + key + "\":" + (stringify obj) + "}"
        | (key,obj)::t -> strObj(acc + "\"" + key + "\":" + (stringify obj) + ",") t
        | [] -> "}"
    let rec strArray acc = function 
        | h::[] -> acc + (stringify h) + "]"
        | h::t -> strArray (acc + (stringify h) + ",") t
        | [] -> "]"
    function 
    | JSON.Object(x) -> "{" + (strObj "" x)
    | JSON.Array(x) -> "[" + (strArray "" x)
    | JSON.Number(x) -> string(x)
    | JSON.String(x) -> "\"" + x + "\""
    | JSON.Boolean(true) -> "true"
    | JSON.Boolean(false) -> "false"
    | JSON.Null -> "null"

let symbols = "abcdefghi"
let rec generate() = 
    let rnd = new Random()
    let rec genArray acc = 
        match rnd.Next(1) with
        | 0 -> JSON.Array acc
        | _ -> genArray(generate()::acc)
    let rec genString acc = 
        match rnd.Next(10) with
        | 9 -> acc
        | num -> genString (acc+string(symbols.[num]))
    let rec genObj acc = 
        match rnd.Next(2) with
        | 0 -> JSON.Object acc
        | _ -> genObj ((genString "", generate())::acc)
    match rnd.Next(15) with
        | 0 -> JSON.Null
        | 1 -> JSON.Boolean(true)
        | 2 -> JSON.Boolean(false)
        | 3 -> JSON.String (genString "")
        | 4 -> genArray []
        | 5 -> JSON.Number(rnd.Next(50))
        | _ -> genObj []

//tests
let w1 = parse """{"a":1,"b":[1,2],"c":3}""" //val w1 : JSON = Object [("a", Number 1); ("b", Array [Number 1; Number 2]); ("c", Number 3)]
let w2 = parse """{"a":1,"c":[1,2],"c":3}""" //val w2 : JSON = Object [("a", Number 1); ("c", Array [Number 1; Number 2]); ("c", Number 3)]
lab3 w1 w2 //val it : string list = ["a"; "c"]

let w1str = stringify w1 // val w1str : string = "{"a":1,"b":[1,2],"c":3}"

let b1 = parse """{"abcd":4,"cdef":[56,43],"qwert":"qwert"}"""
//val b1 : JSON = Object [("abcd", Number 4); ("cdef", Array [Number 56; Number 43]); ("qwert", String "qwert")]

lab3 b1 w1 // val it : string list = []

let obj = generate()
stringify obj


let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab2"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString
