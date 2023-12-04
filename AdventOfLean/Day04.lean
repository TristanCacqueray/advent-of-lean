import Mathlib.Data.Nat.Basic
import Std.Data.RBMap.Basic

def exampleA := [
"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
"Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
"Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
"Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
"Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
"Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
]

structure Card where
  num : Nat
  winning : Std.RBSet Nat compare
  mine : Std.RBSet Nat compare
deriving Repr

def mkSet (nums : List String) :=
  let vals := nums.filterMap (fun s => s.toNat?)
  Std.RBSet.ofList vals compare

def parseCard (line : String) : Card :=
  let cardNumStart := (line.drop 4).dropWhile Char.isWhitespace
  let num := (cardNumStart.takeWhile Char.isDigit).toNat!
  let words := (cardNumStart.split (fun c => c == ' ')).drop 1
  let (winning, mine) := words.span (fun c => c != "|")
  Card.mk num (mkSet winning) (mkSet $ mine.drop 1)

def exampleACards := exampleA.map parseCard

def Card.scoreA (card : Card) : Nat :=
  let incrScore
        | 0 => 1
        | n => n * 2
  let updateScore (val : Nat) (acc : Nat) :=
    if card.winning.contains val then incrScore acc else acc
  Std.RBSet.foldr updateScore 0 card.mine

#eval exampleACards.head?.map (Card.scoreA)

def solveA (input : List String) : Nat :=
  let cards := input.map parseCard
  Nat.sum (cards.map Card.scoreA)

#eval solveA exampleA

def doSolveA : IO Unit := do
  let file <- IO.FS.lines "./data/day04.txt"
  IO.println (solveA file.toList)

-- #eval doSolveA

abbrev Pile : Type := Std.RBMap Nat Nat compare

def scoreB (card : Card) : Pile :=
  let updatePile (val : Nat) (acc : Pile) :=
    if card.winning.contains val then acc.insert (acc.max!.fst + 1) 1 else acc
  Std.RBSet.foldr updatePile (Std.RBMap.single card.num 1) card.mine

def makePile (cards : List Card) : Pile :=
  let piles := cards.map scoreB
  let updatePile (acc : Pile) (cur : Pile × Card) : Pile :=
        let count := 1 + (acc.find? cur.snd.num).getD 0
        let pile := cur.fst.mapVal (fun _card v => v * count)
        -- dbg_trace "updatePile: {cur.snd.num} {count} {pile.toList} {acc.toList}"
        acc.mergeWith (fun card x y => if card == cur.snd.num then count else x + y) pile
  (piles.zip cards).foldl updatePile Std.RBMap.empty

-- #eval makePile exampleACards

def solveB (input : List String) : Nat :=
  Nat.sum (makePile (input.map parseCard)).valuesList

#eval solveB exampleA

def doSolveB : IO Unit := do
  let file <- IO.FS.lines "./data/day04.txt"
  IO.println (solveB file.toList)

-- #eval doSolveB
