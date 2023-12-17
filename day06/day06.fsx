open System

type Race = { time: uint64; recordDist: uint64 }

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let input = IO.File.ReadAllLines(fileName)

let parseInputPart1 (lines: string array) =
    let times =
        lines.[0].Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.tail
        |> Seq.map uint64

    let dists =
        lines.[1].Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.tail
        |> Seq.map uint64

    Seq.zip times dists |> Seq.map (fun (t, d) -> { time = t; recordDist = d })

let parseInputPart2 (lines: string array) =
    let time =
        lines.[0].Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.tail
        |> String.concat ""
        |> uint64

    let dist =
        lines.[1].Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.tail
        |> String.concat ""
        |> uint64

    { time = time; recordDist = dist }

let calculateWinningTimes race =
    [ for heldTime in 1UL .. race.time - 1UL do
          if heldTime * (race.time - heldTime) > race.recordDist then
              heldTime ]

[ for race in parseInputPart1 input do
      calculateWinningTimes race ]
|> List.map List.length
|> List.fold (fun x y -> x * y) 1
|> printfn "Part 1: %d"

parseInputPart2 input
|> calculateWinningTimes
|> List.length
|> printfn "Part 2: %d"
