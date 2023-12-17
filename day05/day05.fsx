open System
open System.Collections.Generic

type Range =
    { sourceStart: uint64
      destStart: uint64
      length: uint64 }

type ConversionMap = Range list

[<RequireQualifiedAccess>]
module Option =
    let ofList =
        function
        | head :: _ -> Some head
        | [] -> None

[<RequireQualifiedAccess>]
module List =
    let makePairs l =
        let rec makePairs' l acc =
            match l with
            | [] -> acc
            | [ _ ] -> failwith "List passed to makePairs had odd number of elements"
            | a :: b :: rest -> (a, b) :: acc |> makePairs' rest

        makePairs' l [] |> List.rev

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let input = System.IO.File.ReadAllText(fileName)

let createMap (text: string) : ConversionMap =
    let lines = text.Split([| "\r\n"; "\n" |], StringSplitOptions.None) |> Array.tail

    [ for line in lines do
          let parts = line.Split(' ')
          let destStart = uint64 parts.[0]
          let sourceStart = uint64 parts.[1]
          let length = uint64 parts.[2]

          { sourceStart = sourceStart
            destStart = destStart
            length = length } ]

let parseInput (input: string) : uint64 list * ConversionMap list =
    let parts = input.Split([| "\r\n\r\n"; "\n\n" |], StringSplitOptions.TrimEntries)
    let seeds = parts.[0].Split(": ").[1].Split(' ') |> Array.map uint64 |> List.ofArray
    seeds, parts |> Array.tail |> Array.map createMap |> List.ofArray

let inRange seed (range: Range) : bool =
    seed >= range.sourceStart && seed < range.sourceStart + range.length

let seeds, maps = parseInput input

let rec iterateMaps maps seed =
    match maps with
    | [] -> seed
    | map :: restMaps ->
        match List.filter (inRange seed) map |> Option.ofList with
        | Some validRange -> seed - validRange.sourceStart + validRange.destStart
        | None -> seed
        |> iterateMaps restMaps

seeds
|> List.map (fun seed -> iterateMaps maps seed)
|> List.min
|> printfn "Part 1: %d"

seeds
|> List.makePairs
|> List.fold
    (fun acc (seedStart, length) ->
        let mutable minDistance = UInt64.MaxValue

        for seed in seedStart .. seedStart + length - 1UL do
            let distance = iterateMaps maps seed

            if distance < minDistance then
                minDistance <- distance

        Math.Min(minDistance, acc))
    UInt64.MaxValue
|> printfn "Part 2: %d"
