open System.IO

type Pos = int * int

let adjacent (pos1: Pos) (pos2: Pos) =
    let x1, y1 = pos1
    let x2, y2 = pos2
    abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

let moveToAdjacent (pos1: Pos) (pos2: Pos) : Pos =
    let x1, y1 = pos1
    let x2, y2 = pos2

    match x1 - x2, y1 - y2 with
    | 0, 2 -> (x1, y1 - 1)
    | 0, -2 -> (x1, y1 + 1)
    | 2, 0 -> (x1 - 1, y1)
    | -2, 0 -> (x1 + 1, y1)
    | 2, 1 -> (x1 - 1, y1)
    | 2, -1 -> (x1 - 1, y1)
    | -2, 1 -> (x1 + 1, y1)
    | -2, -1 -> (x1 + 1, y1)
    | 1, 2 -> (x1, y1 - 1)
    | 1, -2 -> (x1, y1 + 1)
    | -1, 2 -> (x1, y1 - 1)
    | -1, -2 -> (x1, y1 + 1)
    | 2, 2 -> (x1 - 1, y1 - 1)
    | 2, -2 -> (x1 - 1, y1 + 1)
    | -2, 2 -> (x1 + 1, y1 - 1)
    | -2, -2 -> (x1 + 1, y1 + 1)
    | _ -> failwith <| sprintf "%A and %A cannot be made adjacent" pos1 pos2

let moveInDirection (pos: Pos) (dir: char) : Pos =
    let x, y = pos

    match dir with
    | 'U' -> (x, y - 1)
    | 'D' -> (x, y + 1)
    | 'L' -> (x - 1, y)
    | 'R' -> (x + 1, y)
    | _ -> failwith <| sprintf "Invalid direction: %c" dir

module Solution1 =
    type State =
        { headPos: Pos
          tailPos: Pos
          tailVisited: Pos Set }

    let step (state: State) (dir: char) =
        let newHeadPos = moveInDirection state.headPos dir

        let newTailPos =
            if adjacent newHeadPos state.tailPos then
                state.tailPos
            else
                moveToAdjacent newHeadPos state.tailPos

        let newTailVisited = Set.add newTailPos state.tailVisited

        { headPos = newHeadPos
          tailPos = newTailPos
          tailVisited = newTailVisited }

    let move (state: State) (line: string) =
        let dir = line.[0]
        let times = int line.[2..]
        Seq.fold (fun state _ -> step state dir) state [ 1..times ]

    let sol1 (input: string array) =
        (input
         |> Seq.fold
             move
             { headPos = (0, 0)
               tailPos = (0, 0)
               tailVisited = Set.empty })
            .tailVisited
            .Count

module Solution2 =
    type State =
        { positions: Pos array
          tailVisited: Pos Set }

    let step (state: State) (dir: char) =
        let headPos = state.positions.[0]
        let newHeadPos = moveInDirection headPos dir

        let newPositions = Array.create state.positions.Length (0, 0)
        newPositions.[0] <- newHeadPos

        Seq.iter
            (fun index ->
                let prevPos = newPositions.[index - 1]
                let pos = state.positions.[index]

                let newPos =
                    try
                        if adjacent prevPos pos then
                            pos
                        else
                            moveToAdjacent prevPos pos
                    with _ ->
                        failwith <| sprintf "%A" newPositions

                newPositions.[index] <- newPos)
            [ 1 .. state.positions.Length - 1 ]

        let newTailPos = Array.last newPositions
        let newTailVisited = Set.add newTailPos state.tailVisited

        { positions = newPositions
          tailVisited = newTailVisited }

    let move (state: State) (line: string) =
        let dir = line.[0]
        let times = int line.[2..]
        Seq.fold (fun state _ -> step state dir) state [ 1..times ]

    let sol2 (input: string array) =
        (input
         |> Seq.fold
             move
             { positions = Array.create 10 (0, 0)
               tailVisited = Set.empty })
            .tailVisited
            .Count

let input = File.ReadAllLines "input"

printfn "Solution 1: %d" (Solution1.sol1 input)
printfn "Solution 2: %d" (Solution2.sol2 input)
