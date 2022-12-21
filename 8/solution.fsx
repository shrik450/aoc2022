open System.IO

let visibleLine (row: int seq) =
    row
    |> Seq.fold
        (fun (arr, max) height ->
            if height > max then (1 :: arr, height) else (0 :: arr, max))
        ([], -1)
    |> fst
    |> List.rev

let visibilities (row: int seq) =
    row
    |> Seq.fold
        (fun accum height ->
            (height, 0, false)
            :: (List.map
                    (fun (h, l, met) ->
                        if met then (h, l, met)
                        else if h <= height then (h, l + 1, true)
                        else (h, l + 1, met))
                    accum))
        ([])
    |> List.map (fun (_, a, _) -> a)
    |> List.rev

let merge1 (l1, l2) =
    List.zip l1 l2
    |> List.map (fun (left, right) -> if (left = 1 || right = 1) then 1 else 0)

let merge2 (l1, l2) =
    List.zip l1 l2 |> List.map (fun (left, right) -> left * right)

// Wild how simple this is
let rotate (matrix: 'a list list) : 'a list list =
    let rowToColumn (newMatrix: 'a list list) (row: 'a list) =
        List.zip row newMatrix
        |> List.map (fun (item, newRow) -> item :: newRow)

    let initial = List.init (List.length matrix) (fun _ -> [])
    List.fold rowToColumn initial matrix

let parseInput (input: string seq) : int list list =
    input
    |> Seq.map (fun (line: string) ->
        line.ToCharArray() |> Array.toList |> List.map (string >> int))
    |> Seq.toList

let input = File.ReadLines "./input"

let test = File.ReadLines "./test"

let first_solution (input: string seq) =
    let matrix = input |> parseInput
    let size = List.length matrix
    let initialState = List.init size (fun _ -> List.init size (fun _ -> 0))

    seq { 1..4 }
    |> Seq.fold
        (fun (state, board) _ ->
            let visibility = List.map visibleLine board
            let newState = visibility |> List.zip state |> List.map merge1
            (rotate newState, rotate board))
        (initialState, matrix)
    |> fst
    |> List.map List.sum
    |> List.sum

let second_solution (input: string seq) =
    let matrix = input |> parseInput
    let size = List.length matrix
    let initialState = List.init size (fun _ -> List.init size (fun _ -> 1))

    seq { 1..4 }
    |> Seq.fold
        (fun (state, board) _ ->
            let visibility = List.map visibilities board
            let newState = visibility |> List.zip state |> List.map merge2
            (rotate newState, rotate board))
        (initialState, matrix)
    |> fst
    |> List.map List.max
    |> List.max

printfn "%A" <| second_solution input
