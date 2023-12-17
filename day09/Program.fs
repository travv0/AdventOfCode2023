module Day09

open System

let differences (history: int list) : int list =
    let rec differences' (history: int list) (acc: int list) =
        match history with
        | a :: b :: rest -> differences' (b :: rest) (acc @ [ b - a ])
        | _ -> acc

    differences' history []

let reduceHistory (history: int list) : int list list =
    let rec reduceHistory' (history: int list) (acc: int list list) =
        if List.forall (fun x -> x = 0) history then
            acc
        else
            let history' = differences history
            reduceHistory' history' (acc @ [ history' ])

    reduceHistory' history [ history ]

let nextValue (history: int list) : int =
    let histories =
        reduceHistory history
        |> List.rev
        |> List.map List.rev

    List.fold (fun acc history -> acc + List.head history) 0 histories

let previousValue (history: int list) : int =
    let histories = reduceHistory history |> List.rev
    List.fold (fun acc history -> List.head history - acc) 0 histories

let sumExtrapolatedValues nextValFn (histories: int list list) : int =
    let rec sumExtrapolatedValues' (histories: int list list) (acc: int) =
        match histories with
        | [] -> acc
        | history :: rest -> sumExtrapolatedValues' rest (acc + nextValFn history)

    sumExtrapolatedValues' histories 0

let input = IO.File.ReadAllLines "input.txt"

let parseInput (lines: string seq) : int list list =
    lines
    |> Seq.map (fun line ->
        line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
        |> Array.toList)
    |> Seq.toList

parseInput input
|> sumExtrapolatedValues nextValue
|> printfn "Part 1: %d"

parseInput input
|> sumExtrapolatedValues previousValue
|> printfn "Part 2: %d"
