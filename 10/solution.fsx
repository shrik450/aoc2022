open System.IO

let registerAtAllClocks (lines: string array) : int array =
    // Not too happy with this
    Seq.fold
        (fun (register: int, clocks: int array) (line: string) ->
            let parts = line.Split ' '
            let instruction = parts.[0]

            match instruction with
            | "noop" -> (register, Array.append clocks [| register |])
            | "addx" ->
                let newValue = register + int parts.[1]
                (newValue, Array.append clocks [| register; register |])
            | _ -> failwith <| sprintf "Invalid instruction: %s" instruction)
        (1, [||])
        lines
    |> snd

module Solution1 =
    let sol (lines: string array) : int =
        let clocks = registerAtAllClocks lines
        Seq.sumBy (fun index -> clocks.[index - 1] * index) [ 20..40..220 ]

module Solution2 =
    let sol (lines: string array) : string seq =
        let clocks = registerAtAllClocks lines

        let pixels =
            Seq.mapi
                (fun index spritePos ->
                    if abs <| (index % 40) - spritePos <= 1 then '#' else '.')
                clocks

        pixels |> Seq.chunkBySize 40 |> Seq.map (System.String)

let input = File.ReadAllLines "./input"

printfn "Solution 1: %d" <| Solution1.sol input
printfn "Solution 2:"
input |> Solution2.sol |> Seq.iter (printfn "%s")
