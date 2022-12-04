open System.IO

let splitIntoHalf arr =
    let halfwayPoint = Array.length arr / 2
    arr[.. (halfwayPoint - 1)], arr[halfwayPoint..]

let valueOfChar char =
    let asciiValue = int char
    let inter = asciiValue - 96
    if inter < 0 then inter + 58 else inter

let arrayOfString (str: string) = str.ToCharArray()


let first_solution input =
    input
    |> Seq.map (
        arrayOfString
        >> splitIntoHalf
        >> (fun (l, r) -> Set(l), Set(r))
        >> (fun (l, r) -> Set.intersect l r)
        >> Set.toList
        >> List.head
        >> valueOfChar
    )
    |> Seq.sum

let second_solution (input: seq<string>) =
    input
    |> Seq.map (arrayOfString >> Set)
    |> Seq.chunkBySize 3
    |> Seq.map (Set.intersectMany >> Set.toList >> List.head >> valueOfChar)
    |> Seq.sum

let input = File.ReadLines "./input"

let main _ = printfn "%d" <| second_solution input

main ()
