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
      left = String.filter Char.IsLetterOrDigit children[0]
      right = String.filter Char.IsLetterOrDigit children[1] }

let makeGraph (nodes: Node seq) : Map<string, Node> =
    nodes |> Seq.fold (fun map node -> Map.add node.label node map) Map.empty

let parseInput (lines: string array) =
    let dirLine = lines[0]

    let dirs =
        Seq.map
            (function
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

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let input = File.ReadAllLines fileName

let dirs, graph = parseInput input
run graph (List.ofSeq dirs) "AAA" |> printfn "Part 1: %d"

let runParallel graph dirs nodeLabels =
    let rec run' ds nodeLabels count =
        match ds with
        | [] -> run' dirs nodeLabels count
        | h :: t ->
            if Array.forall (fun (l: string) -> l.EndsWith('Z')) nodeLabels then
                count
            else
                let nextLabels =
                    Array.map
                        (fun label ->
                            let { left = left; right = right } =
                                Map.find label graph

                            match h with
                            | L -> left
                            | R -> right)
                        nodeLabels

                run' t nextLabels (count + 1)

    run' dirs nodeLabels 0

let startLabels =
    graph
    |> Map.keys
    |> Seq.filter (fun (l: string) -> l.EndsWith('A'))
    |> Seq.toArray

runParallel graph (List.ofSeq dirs) startLabels |> printfn "Part 2: %d"
