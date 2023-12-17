open System
open System.IO

type Symbol = { symbol: char; x: int; y: int }

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let input = File.ReadAllLines(fileName) |> Array.map (fun line -> line + ".")

let width = String.length input[0]
let height = Array.length input

let getAdjacentSymbol x y (input: string[]) : Symbol option =
    let isSymbol c = c |> Char.IsDigit |> not && c <> '.'

    if y > 0 && isSymbol input.[y - 1].[x] then
        Some
            { symbol = input.[y - 1].[x]
              x = x
              y = y - 1 }
    elif y < height - 1 && isSymbol input.[y + 1].[x] then
        Some
            { symbol = input.[y + 1].[x]
              x = x
              y = y + 1 }
    elif x > 0 && isSymbol input.[y].[x - 1] then
        Some
            { symbol = input.[y].[x - 1]
              x = x - 1
              y = y }
    elif x < width - 1 && isSymbol input.[y].[x + 1] then
        Some
            { symbol = input.[y].[x + 1]
              x = x + 1
              y = y }
    elif y > 0 && x > 0 && isSymbol input.[y - 1].[x - 1] then
        Some
            { symbol = input.[y - 1].[x - 1]
              x = x - 1
              y = y - 1 }
    elif y < height - 1 && x > 0 && isSymbol input.[y + 1].[x - 1] then
        Some
            { symbol = input.[y + 1].[x - 1]
              x = x - 1
              y = y + 1 }
    elif y > 0 && x < width - 1 && isSymbol input.[y - 1].[x + 1] then
        Some
            { symbol = input.[y - 1].[x + 1]
              x = x + 1
              y = y - 1 }
    elif y < height - 1 && x < width - 1 && isSymbol input.[y + 1].[x + 1] then
        Some
            { symbol = input.[y + 1].[x + 1]
              x = x + 1
              y = y + 1 }
    else
        None

let mutable partNumbers = Map.empty

for y in 0 .. height - 1 do
    let mutable number = ""
    let mutable adjacentSymbol = None

    for x in 0 .. width - 1 do
        let c = input.[y].[x]

        if Char.IsDigit c then
            number <- number + string c

            match getAdjacentSymbol x y input with
            | Some symbol -> adjacentSymbol <- Some symbol
            | None -> ()
        elif number <> "" then
            match adjacentSymbol with
            | Some symbol ->
                partNumbers <-
                    Map.change
                        symbol
                        (function
                        | Some pns -> Some(int number :: pns)
                        | None -> Some [ int number ])
                        partNumbers
            | None -> ()

            number <- ""
            adjacentSymbol <- None

partNumbers |> Map.values |> Seq.concat |> Seq.sum |> printfn "Part 1: %d"

partNumbers
|> Map.keys
|> Seq.filter (fun { symbol = symbol } -> symbol = '*')
|> Seq.map (fun symbol ->
    match Map.find symbol partNumbers with
    | [ num1; num2 ] -> num1 * num2
    | _ -> 0)
|> Seq.sum
|> printfn "Part 2: %d"
