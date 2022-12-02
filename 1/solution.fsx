open System.IO

let input = File.ReadLines "./input"

/// The line parsed as an integer, with a blank line becoming -1.
///
/// This allows for chunking by value without having to use any mutable values
/// for indexing, as in the answers of
/// https://stackoverflow.com/questions/6736464/split-seq-in-f
let parsed line =
    if String.length line = 0 then -1 else int line

/// The list of total calories carried by each reindeer.
let totals totalCalories currentCalories =
    if currentCalories = -1 then
        0 :: totalCalories
    else
        match totalCalories with
        | head :: tail -> head + currentCalories :: tail
        | _ -> failwith "This isn't supposed to happen."

// The two result values are functions so the file is only read when called.

let result_first () =
    input |> Seq.map parsed |> Seq.fold totals [ 0 ] |> List.max

let result_second () =
    input
    |> Seq.map parsed
    |> Seq.fold totals [ 0 ]
    |> List.sortDescending
    |> List.take 3
    |> List.sum

let main _ = printfn "%d" <| result_second ()

main ()
