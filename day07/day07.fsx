open System

type Hand = { cards: string; bid: int }

type HandTypes =
    | HighCard
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind

let parseInput (lines: string seq) =
    [ for line in lines do
          let parts = line.Split(' ')
          { cards = parts[0]; bid = int parts[1] } ]

let replaceJokers (hand: string) : string list =
    let cards = hand |> Seq.filter ((<>) 'J') |> Seq.distinct

    let replacements =
        hand
        |> Seq.map (function
            | 'J' -> cards
            | other -> [ other ])

    match
        replacements
        |> Seq.fold
            (fun hands cardOptions ->
                [ for hand in hands do
                      for card in cardOptions do
                          yield hand + (string card) ])
            [ "" ]
    with
    | [] -> [ "JJJJJ" ]
    | r -> r

let classifyHand hand =
    let numRank n r = Seq.exists (Seq.length >> (=) n) r

    let countRanks n r =
        Seq.filter (Seq.length >> (=) n) r |> Seq.length

    let ranks = hand |> Seq.groupBy id |> Seq.map snd |> List.ofSeq

    match () with
    | _ when numRank 5 ranks -> FiveOfAKind
    | _ when numRank 4 ranks -> FourOfAKind
    | _ when numRank 3 ranks && numRank 2 ranks -> FullHouse
    | _ when numRank 3 ranks -> ThreeOfAKind
    | _ when countRanks 2 ranks = 2 -> TwoPair
    | _ when numRank 2 ranks -> OnePair
    | _ -> HighCard

let classifyHandWithJokers (hand: string) : HandTypes =
    replaceJokers hand
    |> Seq.fold
        (fun bestHand hand ->
            let handType = classifyHand hand
            if handType > bestHand then handType else bestHand)
        HighCard

let cardComparer jValue (card1: char) (card2: char) : int =
    let cardToValue =
        function
        | 'A' -> 14
        | 'K' -> 13
        | 'Q' -> 12
        | 'J' -> jValue
        | 'T' -> 10
        | n -> int $"%c{n}"

    cardToValue card1 - cardToValue card2

let handComparer classifyFn jValue (hand1: Hand) (hand2: Hand) : int =
    let hand1Type = classifyFn hand1.cards
    let hand2Type = classifyFn hand2.cards

    if hand1Type < hand2Type then
        -1
    elif hand1Type > hand2Type then
        1
    else
        match Seq.zip hand1.cards hand2.cards |> Seq.tryFind (fun (c1, c2) -> c1 <> c2) with
        | Some cards -> cards ||> cardComparer jValue
        | None -> 0

let fileName =
    match fsi.CommandLineArgs |> Array.toList with
    | _ :: fn :: _ -> fn
    | _ -> "input.txt"

let input = IO.File.ReadAllLines(fileName)
let hands = parseInput input

hands
|> Seq.sortWith (handComparer classifyHand 11)
|> Seq.zip [ 1 .. List.length hands ]
|> Seq.map (fun (rank, hand) -> rank * hand.bid)
|> Seq.sum
|> printfn "Part 1: %d"

hands
|> Seq.sortWith (handComparer classifyHandWithJokers 1)
|> Seq.zip [ 1 .. List.length hands ]
|> Seq.map (fun (rank, hand) -> rank * hand.bid)
|> Seq.sum
|> printfn "Part 2: %d"
