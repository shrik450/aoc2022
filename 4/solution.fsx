open System.IO

let input = File.ReadLines "./input"


let rangeOfString (str: string) =
    match str.Split('-') with
    | [| left; right |] -> seq { int left .. int right } |> Set
    | _ -> failwith "This shouldn't happen"

let bothRangesOfLine (line: string) =
    match line.Split(',') with
    | [| left; right |] -> (rangeOfString left, rangeOfString right)
    | _ -> failwith "This shouldn't happen"


let first_solution (input: seq<string>) =
    input
    |> Seq.map bothRangesOfLine
    |> Seq.filter (fun (left, right) -> Set.isSubset right left || Set.isSubset left right)
    |> Seq.length

let second_solution (input: seq<string>) =
    input
    |> Seq.map (bothRangesOfLine >> (fun (left, right) -> Set.intersect left right) >> Set.count)
    |> Seq.filter (fun count -> count > 0)
    |> Seq.length

let main _ = printfn "%d" <| second_solution input

main ()
