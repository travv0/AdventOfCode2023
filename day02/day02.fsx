#r "nuget: FParsec"

open System
open FParsec

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let input = IO.File.ReadAllText fileName

type Color =
    | Blue
    | Green
    | Red

type Set = Map<Color, int>

type Game = { id: int; sets: Set list }

let numberOfColor color set =
    Map.tryFind color set |> Option.defaultValue 0

let gameImpossible (game: Game) =
    game.sets
    |> List.exists (fun set ->
        numberOfColor Red set > 12
        || numberOfColor Green set > 13
        || numberOfColor Blue set > 14)

let fewestCubesForGame (game: Game) =
    game.sets
    |> List.map (fun set ->
        (numberOfColor Red set, numberOfColor Blue set, numberOfColor Green set))
    |> List.reduce (fun (r1, b1, g1) (r2, b2, g2) ->
        (max r1 r2, max b1 b2, max g1 g2))

let pcolor =
    stringReturn "blue" Blue
    <|> stringReturn "green" Green
    <|> stringReturn "red" Red

let pcount =
    parse {
        let! count = pint32 .>> pstring " "
        let! color = pcolor
        return color, count
    }

let pset = sepBy1 pcount (pstring ", ") |>> Map.ofList

let pgame =
    parse {
        let! gameId = pstring "Game " >>. pint32 .>> pstring ": "
        let! sets = sepBy1 pset (pstring "; ")
        return { id = gameId; sets = sets }
    }

match run (many1 (pgame .>> newline)) input with
| Success(games, _, _) ->
    games
    |> List.filter (not << gameImpossible)
    |> List.sumBy (_.id)
    |> printfn "Part 1: %d"

    games
    |> List.map (fun game ->
        let (red, green, blue) = fewestCubesForGame game in red * green * blue)
    |> List.sum
    |> printfn "Part 2: %d"
| Failure(e, _, _) -> printfn "%s" e
