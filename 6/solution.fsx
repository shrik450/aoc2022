open System.IO

let solution windowSize input =
    input
    |> Seq.windowed windowSize
    |> Seq.map Set
    |> Seq.findIndex (fun s -> Set.count s = windowSize)
    |> (+) windowSize

let input = File.ReadAllBytes "./input"

let main _ = input |> solution 14 |> printfn "%d"

main ()
