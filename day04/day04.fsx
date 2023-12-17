open System

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let lines = System.IO.File.ReadAllLines(fileName)

type Scratchcard =
    { number: int
      yourNumbers: Set<int>
      winningNumbers: Set<int>
      matchingNumberCount: int
      mutable count: int }

let cards =
    [ for line in lines do
          let parts = line.Split(": ")
          let cardNum = parts.[0].Split(' ', StringSplitOptions.RemoveEmptyEntries).[1] |> int
          let numbersSections = parts.[1].Split(" | ")

          let yourNumbers =
              numbersSections.[0].Split(' ', StringSplitOptions.RemoveEmptyEntries)
              |> Seq.map int
              |> Set.ofSeq

          let winningNumbers =
              numbersSections.[1].Split(' ', StringSplitOptions.RemoveEmptyEntries)
              |> Seq.map int
              |> Set.ofSeq

          let matchingNumbers = Set.intersect yourNumbers winningNumbers

          (cardNum,
           { number = cardNum
             yourNumbers = yourNumbers
             winningNumbers = winningNumbers
             count = 1
             matchingNumberCount = Set.count matchingNumbers }) ]
    |> Map.ofList

cards
|> Map.values
|> Seq.map (fun c ->
    match c.matchingNumberCount with
    | 0 -> 0
    | 1 -> 1
    | n -> pown 2 (n - 1))
|> Seq.sum
|> printfn "Part 1: %d"

for { number = number
      matchingNumberCount = matches
      count = count } in Map.values cards do
    for i in number + 1 .. number + matches do
        let iCard = Map.find i cards
        iCard.count <- iCard.count + count

cards
|> Map.values
|> Seq.map (fun c -> c.count)
|> Seq.sum
|> printfn "Part 2: %d"
