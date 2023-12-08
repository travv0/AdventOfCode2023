open System
open System.IO

type Dir =
    | L
    | R

type Node =
    { label: string
      left: string
      right: string }

let parseNode (s: string) =
    let parts = s.Split(" = ")
    let children = parts[1].Split(", ")

    { label = parts[0]
      left = String.filter Char.IsLetter children[0]
      right = String.filter Char.IsLetter children[1] }

let makeGraph (nodes: Node seq) : Map<string, Node> =
    nodes |> Seq.fold (fun map node -> Map.add node.label node map) Map.empty

let parseInput (lines: string array) =
    let dirLine = lines[0]

    let dirs =
        Seq.map
            (fun d ->
                match d with
                | 'L' -> L
                | 'R' -> R
                | _ -> failwith "invalid input")
            dirLine

    let nodes = lines[2..] |> Seq.map parseNode
    dirs, makeGraph nodes

let run graph dirs nodeLabel =
    let rec run' ds nodeLabel count =
        match ds with
        | [] -> run' dirs nodeLabel count
        | h :: t ->
            if nodeLabel = "ZZZ" then
                count
            else
                let { left = left; right = right } = Map.find nodeLabel graph

                run'
                    t
                    (match h with
                     | L -> left
                     | R -> right)
                    (count + 1)

    run' dirs nodeLabel 0

let input = File.ReadAllLines "input.txt"

let dirs, graph = parseInput input
run graph (List.ofSeq dirs) "AAA" |> printfn "Part 1: %d"
