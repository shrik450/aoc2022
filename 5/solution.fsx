// This solution sucks major ass.
// But at least it's correct :3

open System.IO
open System.Text.RegularExpressions

type inp = string array * string seq

module Crates =
    type t = char list array

    // lmao
    let empty: t = [| []; []; []; []; []; []; []; []; [] |]

    let parse (input: string array) : t =
        let indexes = seq { 1..4..34 }

        let parseLine (line: string) =
            indexes |> Seq.map (fun ind -> line[ind]) |> Seq.toArray

        let addLine (array: t) (line: char array) : t =
            array
            |> Array.zip line
            |> Array.map (fun (ch, l) -> if ch = ' ' then l else ch :: l)

        input
        |> Array.fold (fun (state: t) (line: string) -> line |> parseLine |> addLine state) empty
        |> Array.map (List.rev)

    let move (crates: t) (count: int) (from: int) (other: int) : t =
        let lists = (crates[from], crates[other])

        let newLists =
            seq { 1..count }
            |> Seq.fold (fun (hd :: tl, right) _ -> (tl, hd :: right)) lists

        // I'm mutating here because it's just kind of a mess otherwise.
        crates[from] <- fst newLists
        crates[other] <- snd newLists

        crates

    let move2 (crates: t) (count: int) (from: int) (other: int) : t =
        let left = crates[from]
        let right = crates[other]

        let (temp, newLeft) = List.splitAt count left
        let newRight = List.concat [ temp; right ]

        crates[from] <- newLeft
        crates[other] <- newRight

        crates

    let tops (crates: t) = Array.map List.head crates

module Moves =
    type t = { count: int; from: int; other: int }

    let regex = Regex(@"^move (\d+) from (\d) to (\d)$", RegexOptions.Compiled)

    let parse (input: string) : t =
        let m = regex.Match(input)

        { count = m.Groups[ 1 ].ToString() |> int
          // Off by ones are fun!
          from = (m.Groups[ 2 ].ToString() |> int) - 1
          other = (m.Groups[ 3 ].ToString() |> int) - 1 }

    let run (crates: Crates.t) (move: t) =
        Crates.move2 crates move.count move.from move.other


let first_solution (input: inp) =
    let crates = input |> fst |> Crates.parse

    input
    |> snd
    |> Seq.fold (fun cr line -> line |> Moves.parse |> Moves.run cr) crates
    |> Crates.tops
    |> Seq.map string
    |> String.concat ""
    |> sprintf "%s"

let fileContents = File.ReadLines "./input"

let input = (fileContents |> Seq.take 8 |> Seq.toArray, fileContents |> Seq.skip 10)
printfn "%s" <| first_solution input
