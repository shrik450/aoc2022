open System.IO
open System.Text.RegularExpressions

type directory = { name: string; size: int }

type node =
    { directory: directory
      children: node list }

type step =
    { currentNode: node
      otherNodesOfThisLevel: node list }

type zipper = step list

let rec unzip (zip: zipper) : node =
    match zip with
    | hd :: prev :: rest ->
        let newChildren = hd.currentNode :: hd.otherNodesOfThisLevel

        let newHead =
            { prev with currentNode = { prev.currentNode with children = newChildren } }

        unzip <| newHead :: rest
    | hd :: [] -> hd.currentNode
    | [] -> failwith "Empty zipper given to unzip."

let rec flatten (tree: node) : directory list =
    List.concat <| [ tree.directory ] :: List.map flatten tree.children


let cdRegex = Regex(@"^\$ cd (.+)$", RegexOptions.Compiled)

let handleCommand (state: zipper) (line: string) =
    let m = cdRegex.Match(line)

    if m.Success then
        let newDirName = m.Groups[ 1 ].ToString()

        if newDirName = ".." then
            match state with
            | hd :: prev :: rest ->
                let newChildren = hd.currentNode :: hd.otherNodesOfThisLevel

                let newHead =
                    { prev with currentNode = { prev.currentNode with children = newChildren } }

                newHead :: rest
            | _ -> failwith ".. with no previous directory?"
        else
            let newDir = { name = newDirName; size = 0 }

            let newOtherNodesOfThisLevel =
                match state with
                | hd :: _ -> hd.currentNode.children
                | [] -> []

            let newNode = { directory = newDir; children = [] }

            let newStep =
                { currentNode = newNode
                  otherNodesOfThisLevel = newOtherNodesOfThisLevel }

            newStep :: state

    else
        state

let fileRegex = Regex(@"^(\d+) ", RegexOptions.Compiled)

let handleLsLine (state: zipper) (line: string) : zipper =
    let m = fileRegex.Match(line)

    if m.Success then
        let size = m.Groups[ 1 ].ToString() |> int

        match state with
        | currentStep :: rest ->
            let { currentNode = { directory = currentDir } } = currentStep

            let newStep =
                { currentStep with
                    currentNode =
                        { currentStep.currentNode with directory = { currentDir with size = currentDir.size + size } } }

            newStep :: rest
        | [] -> failwith "No current dir while handling ls line?"
    else
        state

let takeStep (state: zipper) (line: string) =
    match line.[0] with
    | '$' -> handleCommand state line
    | _ -> handleLsLine state line

let rec calculateTotalSizes (tree: node) : node =
    let newChildren = List.map calculateTotalSizes tree.children
    let childrenSize = newChildren |> List.map (fun n -> n.directory.size) |> List.sum

    { directory = { tree.directory with size = tree.directory.size + childrenSize }
      children = newChildren }

let directoryList (input: string seq) =
    input |> Seq.fold takeStep [] |> unzip |> calculateTotalSizes |> flatten

let first_solution (input: string seq) =
    input
    |> directoryList
    |> List.map (fun dir -> dir.size)
    |> List.filter (fun size -> size <= 100000)
    |> List.sum

let second_solution (input: string seq) (totalSize: int) (requiredSpace: int) =
    let dirList = directoryList input
    let availableSpace = totalSize - dirList[0].size
    let mustClear = requiredSpace - availableSpace

    dirList
    |> List.map (fun dir -> dir.size)
    |> List.filter (fun size -> size >= mustClear)
    |> List.min

let input = File.ReadLines "./input"

printfn "%A" <| second_solution input 70000000 30000000
