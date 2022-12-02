open System.IO

type Move =
    | Rock
    | Paper
    | Scissors

let scoreOfMove =
    function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

type Outcome =
    | Win
    | Draw
    | Loss

let scoreOfOutcome =
    function
    | Win -> 6
    | Draw -> 3
    | Loss -> 0

let outcomeOfGame =
    function
    | Rock, Rock
    | Paper, Paper
    | Scissors, Scissors -> Draw
    | Rock, Paper
    | Paper, Scissors
    | Scissors, Rock -> Win
    | Rock, Scissors
    | Paper, Rock
    | Scissors, Paper -> Loss


let myMoveOfOutcome (oppMove, outcome) =
    match oppMove with
    | Rock ->
        match outcome with
        | Win -> Paper
        | Draw -> Rock
        | Loss -> Scissors
    | Paper ->
        match outcome with
        | Win -> Scissors
        | Draw -> Paper
        | Loss -> Rock
    | Scissors ->
        match outcome with
        | Win -> Rock
        | Draw -> Scissors
        | Loss -> Paper


let moveOfChar =
    function
    | 'A'
    | 'X' -> Rock
    | 'B'
    | 'Y' -> Paper
    | 'C'
    | 'Z' -> Scissors
    | c -> failwith <| sprintf "Invalid input character %c to moveOfChar." c

let outcomeOfChar =
    function
    | 'X' -> Loss
    | 'Y' -> Draw
    | 'Z' -> Win
    | c -> failwith <| sprintf "Invalid input character %c to moveOfChar." c


let parseLineFirst (line: string) =
    let oppMove = moveOfChar line[0]
    let myMove = moveOfChar line[2]
    (oppMove, myMove)

let scoreOfGameFirst game =
    let moveScore = game |> snd |> scoreOfMove
    let outcomeScore = game |> outcomeOfGame |> scoreOfOutcome
    moveScore + outcomeScore

let first_solution input =
    input |> Seq.map parseLineFirst |> Seq.map scoreOfGameFirst |> Seq.sum


let parseLineSecond (line: string) =
    let oppMove = moveOfChar line[0]
    let outcome = outcomeOfChar line[2]
    (oppMove, outcome)

let scoreOfGameSecond game =
    let moveScore = game |> myMoveOfOutcome |> scoreOfMove
    let outcomeScore = game |> snd |> scoreOfOutcome
    moveScore + outcomeScore

let second_solution input =
    input |> Seq.map parseLineSecond |> Seq.map scoreOfGameSecond |> Seq.sum

let input = File.ReadLines "./input"

let main () = printfn "%d" <| second_solution input

main ()
