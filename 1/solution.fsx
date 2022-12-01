open System.IO

let input = File.ReadLines "./input"

let chunk totalCalories currentCalories =
    if currentCalories = -1 then
        0 :: totalCalories
    else
        List.head totalCalories + currentCalories :: totalCalories

let result_first () =
    input
    |> Seq.map (fun line -> if line.Length = 0 then -1 else int line)
    |> Seq.fold chunk [ 0 ]
    |> List.max

let result_second () =
    input
    |> Seq.map (fun line -> if line.Length = 0 then -1 else int line)
    |> Seq.fold chunk [ 0 ]
    |> List.sortDescending
    |> List.take 3
    |> List.sum

let main _ = printfn "%d" <| result_second ()

main ()
