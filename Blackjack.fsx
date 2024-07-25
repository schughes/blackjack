(*
REQUIREMENTS
    x shuffle deck
    x draw cards
    x calculate hand value
        x all cards are worth their face value
        x special case: aces
            x ace is worth either an 11 or a 1 , whatever gets the player closer to 21 without busting
    x initialize game
    x play game
    x determine winner
*)
open System

// Union type; have choices; Vertical bars are separators
type Suit = Clubs | Diamonds | Hearts | Spades
(*
    public enum Suit 
    {
        Clubs,
        Diamonds,
        Hearts,
        Spades
    }
*)

type Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

// Record
type Card = { Suit: Suit; Rank: Rank }
(*
    public class Card 
    {
        public Suit Suit {get;set;}
        public Rank Rank {get;set;}
    }
*)

// create deck
let allSuits = [Clubs ; Diamonds ; Hearts ; Spades]
let allRanks = [Two ; Three ; Four ; Five ; Six ; Seven ; Eight ; Nine ; Ten ; Jack ; Queen ; King ; Ace]

let deck = 
    [ for suit in allSuits do
        for rank in allRanks do 
            yield {Suit = suit; Rank = rank} ]

let shuffleDeck deck =
    let rnd = Random()
    deck
    |> List.sortBy(fun _ -> rnd.Next())
(*
    public static List<Card> ShuffleDeck(List<Card> deck)
    {
        var rnd = new Random();
        return deck.OrderBy(_ => rnd.Next()).ToList();
    }
*)

let printCard (card:Card) =
    let rankToString rank = 
        match rank with 
        | Two -> "2" | Three -> "3" | Four -> "4" | Five -> "5" | Six -> "6"
        | Seven -> "7" | Eight -> "8" | Nine -> "9" | Ten -> "10"
        | Jack -> "J" | Queen -> "Q" | King -> "K" | Ace -> "A"
    let suitToString suit = 
        match suit with 
        | Clubs -> "C"
        | Diamonds -> "D"
        | Hearts -> "H"
        | Spades -> "S"
    sprintf "%s%s" (rankToString card.Rank) (suitToString card.Suit)

let drawCard deck = 
    let card, remaining = List.head deck, List.tail deck
    card, remaining

let rankValue rank = 
    match rank with 
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Eight -> 8
    | Nine -> 9
    | Ten -> 10
    | Jack | Queen | King -> 10
    | Ace -> 11

let calculateHandValue cards =
    let rec calcValue cards total aces = 
        match cards with 
        // base case, no more cards
        | [] -> total, aces
        // head :: tail
        | card :: rest ->
            let cardValue = rankValue card.Rank
            if card.Rank = Ace
                then calcValue rest (total + cardValue) (aces + 1)
            else 
                calcValue rest (total + cardValue) aces

    let rec adjustForAces total aces =
        if total > 21 && aces > 0 then
            adjustForAces (total - 10) (aces - 1)
        else
            total

    let total, aces = calcValue cards 0 0
    adjustForAces total aces
(*
    public static int CalculateHandValue(List<Card> cards)
    {
        int total = 0;
        int aces = 0;

        // calculate the total value of the hand and count the number of aces
        foreach (var card in cards)
        {
            int cardValue = RankValue(card.Rank);
            total += cardValue;

            if (card.Rank == Rank.Ace)
            {
                aces++;
            }
        }

        // adjust the total value for Aces
        while (total > 21 && aces > 0)
        {
            total -= 10;
            aces--;
        }

        return total;
    }
*)

let printHand name hand =
    let handString = 
        hand 
        |> List.map printCard
        |> String.concat ","
    printfn "%s's hand: %s (Value: %i)" name handString (calculateHandValue hand)


let initGame () = 
    let drawTwo deck = 
        let card1, deck1 = drawCard deck
        let card2, deck2 = drawCard deck1
        ([card1;card2], deck2)

    let shuffledDeck = shuffleDeck deck
    let playerHand, deckAfterPlayer = drawTwo shuffledDeck
    let dealerHand, deckAfterDealer = drawTwo deckAfterPlayer

    (playerHand, dealerHand, deckAfterDealer)

let jackBlack () = 
    let playerHand, dealerHand, deckAfterDealer = initGame ()
    printHand "Player" playerHand
    printHand "Dealer" dealerHand

    let rec playerTurn hand deck = 
        let handValue = calculateHandValue hand
        printfn "Player hand value %i" handValue
        if handValue < 21 then
            printfn "\nDo you want to hit (h) or stand (s)?"
            match Console.ReadLine() with 
            | "s" -> hand, deck
            | "h" -> 
                let card, remainingDeck = drawCard deck
                printfn "You drew %s" (printCard card)
                playerTurn (card::hand) remainingDeck 
            | _ -> 
                printfn "Invalid choice. Try again."
                playerTurn hand deck
        else
            hand, deck
(*
    public static (List<Card> hand, List<Card> deck) PlayerTurn(List<Card> hand, List<Card> deck)
    {
        int handValue = CalculateHandValue(hand);
        Console.WriteLine($"Player's hand value: {handValue}");
        
        if (handValue < 21)
        {
            Console.WriteLine("\nDo you want to hit (h) or stand (s)?");
            var choice = Console.ReadLine();
            
            switch (choice)
            {
                case "s":
                    return (hand, deck);
                case "h":
                    var (card, remainingDeck) = DrawCard(deck);
                    Console.WriteLine($"You drew {PrintCard(card)}");
                    hand.Add(card);
                    return PlayerTurn(hand, remainingDeck);
                default:
                    Console.WriteLine("Invalid choice. Try again");
                    return PlayerTurn(hand, deck);
            }
        }
        else
        {
            return (hand, deck);
        }
    }

*)


    let rec dealerTurn hand deck = 
        let handValue = calculateHandValue hand
        if handValue < 17 then
            let card, remainingDeck = drawCard deck
            printfn "Dealer drew %s" (printCard card)
            dealerTurn (card::hand) remainingDeck 
        else hand, deck
(*
    public static (List<Card> hand, List<Card> deck) DealerTurn(List<Card> hand, List<Card> deck)
    {
        int handValue = CalculateHandValue(hand);

        if (handValue < 17)
        {
            var (card, remainingDeck) = DrawCard(deck);
            Console.WriteLine($"Dealer drew {PrintCard(card)}");
            hand.Add(card);
            return DealerTurn(hand, remainingDeck);
        }
        else
        {
            return (hand, deck);
        }
    }
*)


    let finalPlayerHand, finalDeck = playerTurn playerHand deckAfterDealer
    let finalDealerHand, _ = dealerTurn dealerHand finalDeck 

    let determineWinner player dealer = 
        match (player > 21 , dealer > 21) with
        | (true, _) -> "Player busts, Dealer wins!"
        | (_, true) -> "Dealer busts, Player wins!"
        | (false, false) -> 
            match player, dealer with 
            | _ when player > dealer -> "Player wins!"
            | _ when player < dealer -> "Dealer wins!"
            | _ -> "It's a tie!"
(*
    public static string DetermineWinner(int player, int dealer)
    {
        if (player > 21)
        {
            return "Player busts, Dealer wins!";
        }
        else if (dealer > 21)
        {
            return "Dealer busts, Player wins!";
        }
        else
        {
            if (player > dealer)
            {
                return "Player wins!";
            }
            else if (player < dealer)
            {
                return "Dealer wins!";
            }
            else
            {
                return "It's a tie!";
            }
        }
    }
*)

    printfn "Final Hands"
    printHand "Dealer" finalDealerHand
    printHand "Player" finalPlayerHand

    printfn "\n%s\n" (determineWinner (calculateHandValue finalPlayerHand) (calculateHandValue finalDealerHand))

jackBlack ()