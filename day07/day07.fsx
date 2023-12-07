open System

type Hand = { cards: string; bid: int }

type HandTypes =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard

let parseInput (lines: string seq) =
    [ for line in lines do
          let parts = line.Split(' ')

          { cards = parts.[0]
            bid = int parts.[1] } ]

let classifyHand hand =
    let groups = Seq.groupBy (fun c -> c) hand |> Seq.map snd

    if Seq.exists (fun g -> Seq.length g = 5) groups then
        FiveOfAKind
    elif Seq.exists (fun g -> Seq.length g = 4) groups then
        FourOfAKind
    elif
        Seq.exists (fun g -> Seq.length g = 3) groups
        && Seq.exists (fun g -> Seq.length g = 2) groups
    then
        FullHouse
    elif Seq.exists (fun g -> Seq.length g = 3) groups then
        ThreeOfAKind
    elif Seq.filter (fun g -> Seq.length g = 2) groups |> Seq.length = 2 then
        TwoPair
    elif Seq.exists (fun g -> Seq.length g = 2) groups then
        OnePair
    else
        HighCard

let cardComparer (card1: char) (card2: char) : int =
    let cardToValue =
        function
        | 'A' -> 14
        | 'K' -> 13
        | 'Q' -> 12
        | 'J' -> 11
        | 'T' -> 10
        | n -> int $"%c{n}"

    let value1 = cardToValue card1
    let value2 = cardToValue card2
    value1 - value2

let handComparer (hand1: Hand) (hand2: Hand) : int =
    let hand1Type = classifyHand hand1.cards
    let hand2Type = classifyHand hand2.cards

    if hand1Type > hand2Type then
        -1
    elif hand2Type > hand1Type then
        1
    else
        Seq.zip hand1.cards hand2.cards
        |> Seq.find (fun (c1, c2) -> c1 <> c2)
        ||> cardComparer

let input = IO.File.ReadAllLines("input.txt")
let hands = parseInput input


hands
|> Seq.sortWith handComparer
|> Seq.map (fun hand -> hand.cards)
|> Seq.toList
|> printfn "%A"

hands
|> Seq.sortWith handComparer
|> Seq.zip [ 1 .. List.length hands ]
|> Seq.map (fun (rank, hand) -> rank * hand.bid)
|> Seq.sum
|> printfn "Part 1: %d"
