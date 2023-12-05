open System
open System.IO

let input =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line -> line + ".")

let width = String.length input[0]
let height = Array.length input

let isBySymbol x y (input: string[]) =
    let isSymbol c = c |> Char.IsDigit |> not && c <> '.'
    y > 0 && isSymbol input.[y - 1].[x]
    || y < height - 1 && isSymbol input.[y + 1].[x]
    || x > 0 && isSymbol input.[y].[x - 1]
    || x < width - 1 && isSymbol input.[y].[x + 1]
    || y > 0 && x > 0 && isSymbol input.[y - 1].[x - 1]
    || y < height - 1 && x > 0 && isSymbol input.[y + 1].[x - 1]
    || y > 0 && x < width - 1 && isSymbol input.[y - 1].[x + 1]
    || y < height - 1 && x < width - 1 && isSymbol input.[y + 1].[x + 1]

let mutable partNumbers = []
for y in 0 .. height - 1 do
    let mutable number = ""
    let mutable bySymbol = false

    for x in 0 .. width - 1 do
        let c = input.[y].[x]

        if Char.IsDigit c then
            number <- number + string c

            if isBySymbol x y input then
                bySymbol <- true
        elif number <> "" then
            if bySymbol then
                partNumbers <- int number :: partNumbers

            number <- ""
            bySymbol <- false

partNumbers |> List.sum |> printfn "Part 1: %d"
