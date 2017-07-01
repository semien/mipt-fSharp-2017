// подключаем FSharp.Data
#r "../packages/FSharp.Data.2.3.3/lib/net40/FSharp.Data.dll"
open FSharp.Data
open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

// почтовый адрес
let email = "semien9797@gmail.com"

let startLink = "http://wikimipt.org/wiki/Категория:Преподаватели_по_алфавиту"
let prefixLink = "http://wikimipt.org"

let lab3() =
    let rec parsePage (link:string) =
       
        // if there is next page: return url, else: return null
        let getNext (nodes:seq<HtmlNode>) = 
            let linkseq = nodes |> Seq.filter (fun x -> (x.InnerText() = "Следующая страница"))
            if Seq.isEmpty linkseq then null
            else (Seq.item(0) linkseq).Attribute("href").Value()
        
        // getting links on social network
        let getVkFacebook(tLink:string) = 
            let teacherPage = HtmlDocument.Load(prefixLink + tLink)
            let nodeSeq = teacherPage.Descendants["a"]
            let VKSeq = Seq.filter (fun (x:HtmlNode) -> (x.InnerText() = "ВКонтакт")) nodeSeq
            let FBSeq = Seq.filter (fun (x:HtmlNode) -> (x.InnerText() = "Facebook")) nodeSeq
            let MNSeq = Seq.filter (fun (x:HtmlNode) -> (x.InnerText() = "Math-Net.Ru")) nodeSeq 
            // я заменил linkedin на Math-Net, хотя это и не соц.сеть, но этот ресурс очень популярен.
            let VK = if Seq.isEmpty VKSeq then "VK: - " else ("VK: " + (Seq.item(0) VKSeq).Attribute("href").Value())
            let FB = if Seq.isEmpty FBSeq then "Facebook: - " else ("Facebook: " + (Seq.item(0) FBSeq).Attribute("href").Value())
            let MN = if Seq.isEmpty MNSeq then "Math-Net: - " else ("Math-Net: " + (Seq.item(0) MNSeq).Attribute("href").Value())
            (VK + ", " + FB + ", " + MN)

        let pageNodes = HtmlDocument.Load(link).Descendants["a"]
        let sequenceOfThisPage = 
            pageNodes |> Seq.filter (fun x->x.HasAttribute("title",x.InnerText())) 
            |> Seq.map (fun y -> (y.InnerText() + ", " + getVkFacebook( y.Attribute("href").Value() )))
        let nextPage = getNext(pageNodes)
        if nextPage = null then sequenceOfThisPage
        else (Seq.append sequenceOfThisPage (parsePage(prefixLink + nextPage))) 
    parsePage(startLink) |> Seq.toList

let list = lab3()

List.item(694) <| list

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("result", lab3().ToString())
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab3"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString

