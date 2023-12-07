import Mathlib.Data.Nat.Basic

def exampleA := [
"32T3K 765",
"T55J5 684",
"KK677 28",
"KTJJT 220",
"QQQJA 483"
]

inductive Card where
  | Num (val : Nat)
  | T | J | Q | K | A
 deriving Repr, Ord, BEq

inductive HandType where
  | Five | Four | FullHouse | Three | TwoPair | OnePair | High
 deriving Repr, Ord, BEq

structure Hand where
  cards : List Card
  bid   : Nat
  type  : HandType
 deriving Repr

-- Lean does not have list.sort, here is an implem
-- From https://codereview.stackexchange.com/questions/197868/bubble-sort-in-haskell
def swapTill [Min α] [Max α] (x : α) : List α -> List α
  | [] => [x]
  | y :: xs => min x y :: swapTill (max x y) xs
def List.bubbleSort [Min α] [Max α]: List α -> List α := List.foldr swapTill []

-- Instances for min/max
def minCard : Card -> Card -> Card
  | a,b => match compare a b with | Ordering.lt => a | _ => b

instance : Min Card where min := minCard

def maxCard : Card -> Card -> Card
  | a,b => match compare a b with | Ordering.lt => b | _ => a

instance : Max Card where max := maxCard

def handType (cards : List Card) : HandType :=
   let cg := cards.bubbleSort.groupBy (.==.)
   let hasGroup (size : Nat) := (cg.find? (fun g => g.length == size)).isSome
   let rec countPair (acc : Nat)
         | [] => acc
         | x :: xs => countPair (acc + if x.length == 2 then 1 else 0) xs
   if hasGroup 5 then HandType.Five
   else if hasGroup 4 then HandType.Four
   else if hasGroup 3 && hasGroup 2 then HandType.FullHouse
   else if hasGroup 3 then HandType.Three
   else if countPair 0 cg == 2 then HandType.TwoPair
   else if hasGroup 2 then HandType.OnePair
   else HandType.High

def parseHand (str : String) : Hand :=
  let rec go (acc : List Card) : List Char -> List Card
   | [] => acc.reverse
   | c :: cs =>
     let card := match c with
       | 'A' => Card.A
       | 'K' => Card.K
       | 'Q' => Card.Q
       | 'J' => Card.J
       | 'T' => Card.T
       | c => Card.Num (c.toNat - '0'.toNat)
     go (card :: acc) cs
  let cardStr := str.takeWhile (. != ' ')
  let cards := go [] cardStr.toList
  let score := str.drop (cardStr.length + 1)
  Hand.mk cards score.toNat! (handType cards)

def compareCards : List Card -> List Card -> Ordering
  | x::xs, y::ys => match compare y x with
    | Ordering.eq => compareCards xs ys
    | other => other
  | _, _ => Ordering.eq

def compareHand (x : Hand) (y : Hand) : Ordering :=
  match compare x.type y.type with
    | Ordering.eq => compareCards x.cards y.cards
    | other => other

-- Instances for min/max
def minHand : Hand -> Hand -> Hand
  | a,b => match compareHand a b with | Ordering.lt => a | _ => b

instance : Min Hand where min := minHand

def maxHand : Hand -> Hand -> Hand
  | a,b => match compareHand a b with | Ordering.lt => b | _ => a

instance : Max Hand where max := maxHand

-- #eval (exampleA.map parseHand).bubbleSort

def solveA (input : List String) :=
  let hands := (input.map parseHand).bubbleSort
  let scores := hands.reverse.toArray.mapIdx (fun pos hand => (pos + 1) * hand.bid)
  Nat.sum scores.toList

-- #eval solveA exampleA

def doSolveA : IO Unit := do
  let file <- IO.FS.lines "./data/day07.txt"
  IO.println (solveA file.toList)

-- #eval doSolveA

def jokerHandType (cards : List Card) : HandType :=
   let baseScore := handType (cards.filter (. != Card.J))
   let jokerCount := (cards.filter (. == Card.J)).length
   if jokerCount >= 4 then HandType.Five
   else if jokerCount == 3 then match baseScore with
     | HandType.OnePair => HandType.Five
     | _ => HandType.Four
   else if jokerCount == 2 then match baseScore with
     | HandType.Three => HandType.Five
     | HandType.OnePair => HandType.Four
     | _ => HandType.Three
   else if jokerCount == 1 then match baseScore with
     | HandType.Four => HandType.Five
     | HandType.Three => HandType.Four
     | HandType.TwoPair => HandType.FullHouse
     | HandType.OnePair => HandType.Three
     | _ => HandType.OnePair
   else baseScore

structure JokerHand where
  hand : Hand

def parseJokerHand (str : String) : JokerHand :=
  let hand := parseHand str
  JokerHand.mk {hand with type := jokerHandType hand.cards}

def compareJokerCards : List Card -> List Card -> Ordering
  | x::xs, y::ys => match compare y x with
    | Ordering.eq => compareJokerCards xs ys
    | other => if x == Card.J then Ordering.gt else if y == Card.J then Ordering.lt else other
  | _, _ => Ordering.eq

def compareJokerHand (x : JokerHand) (y : JokerHand) : Ordering :=
  match compare x.hand.type y.hand.type with
    | Ordering.eq => compareJokerCards x.hand.cards y.hand.cards
    | other => other

-- Instances for min/max
def minJokerHand : JokerHand -> JokerHand -> JokerHand
  | a,b => match compareJokerHand a b with | Ordering.lt => a | _ => b

instance : Min JokerHand where min := minJokerHand

def maxJokerHand : JokerHand -> JokerHand -> JokerHand
  | a,b => match compareJokerHand a b with | Ordering.lt => b | _ => a

instance : Max JokerHand where max := maxJokerHand

def solveB (input : List String) :=
  let hands := (input.map parseJokerHand).bubbleSort
  let scores := hands.reverse.toArray.mapIdx (fun pos hand => (pos + 1) * hand.hand.bid)
  Nat.sum scores.toList

#eval solveB exampleA

def doSolveB : IO Unit := do
  let file <- IO.FS.lines "./data/day07.txt"
  IO.println (solveB file.toList)

#eval doSolveB
