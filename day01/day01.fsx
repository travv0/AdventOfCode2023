open System
open System.IO

let rec replaceDigitWords =
    function
    | "" -> ""
    | line ->
        let newDigit =
            if line.StartsWith("one") then "1"
            elif line.StartsWith("two") then "2"
            elif line.StartsWith("three") then "3"
            elif line.StartsWith("four") then "4"
            elif line.StartsWith("five") then "5"
            elif line.StartsWith("six") then "6"
            elif line.StartsWith("seven") then "7"
            elif line.StartsWith("eight") then "8"
            elif line.StartsWith("nine") then "9"
            elif Char.IsDigit line[0] then line.Substring(0, 1)
            else ""

        newDigit + (line.Substring(1) |> replaceDigitWords)

let getCalibrationValue (line: string) =
    let firstDigit = Seq.find Char.IsDigit line
    let lastDigit = Seq.findBack Char.IsDigit line
    int <| $"%c{firstDigit}%c{lastDigit}"

let lines = File.ReadLines("input.txt")

lines |> Seq.map getCalibrationValue |> Seq.sum |> printfn "Part 1: %d"

lines
|> Seq.map (replaceDigitWords >> getCalibrationValue)
|> Seq.sum
|> printfn "Part 2: %d"
