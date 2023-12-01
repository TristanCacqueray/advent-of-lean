def exampleA := [
  "1abc2",
  "pqr3stu8vwx",
  "a1b2c3d4e5f",
  "7reb7uchet"
]

def Char.digitValue (c : Char) := c.toNat - 48

def getDigitNumber (s : String) : Nat :=
  let numbers := s.toList.filter Char.isDigit
  numbers.head!.digitValue  * 10 + numbers.reverse.head!.digitValue

-- question: how to import that from https://leanprover-community.github.io/mathlib4_docs/Std/Data/Nat/Basic.html#Nat.sum
def Nat.sum (l) := List.foldr (fun (x x_1 : Nat) => x + x_1) 0 l

#eval Nat.sum ( exampleA.map getDigitNumber )

def solveA : IO Unit := do
  let file <- IO.FS.lines "./data/day01.txt"
  IO.println (Nat.sum (file.toList.map getDigitNumber))

#eval solveA

def exampleB := [
  "two1nine",
  "eightwothree",
  "abcone2threexyz",
  "xtwone3four",
  "4nineeightseven2",
  "zoneight234",
  "7pqrstsixteen"
]

def getNumber (s : String) : Option Nat :=
  if let some c := (s.get? 0) then
    if c >= '0' && c <= '9' then
      some (c.toNat - 48)
    else if s.startsWith "one" then some 1
    else if s.startsWith "two" then some 2
    else if s.startsWith "three" then some 3
    else if s.startsWith "four" then some 4
    else if s.startsWith "five" then some 5
    else if s.startsWith "six" then some 6
    else if s.startsWith "seven" then some 7
    else if s.startsWith "eight" then some 8
    else if s.startsWith "nine" then some 9
    else
      none
  else
    none

def getNumbers (str : String) : Nat :=
  let rec first : Nat -> Nat
    | 0 => 0
    | Nat.succ pos => if let some num := getNumber (str.drop (str.length - pos - 1)) then num
                      else first pos
  let rec last : Nat -> Nat
    | 0 => 0
    | Nat.succ pos => if let some num := getNumber (str.takeRight (str.length - pos)) then num
                      else last pos
  first str.length * 10 + last str.length

#eval Nat.sum (exampleB.map getNumbers)

def solveB : IO Unit := do
  let file <- IO.FS.lines "./data/day01.txt"
  IO.println (Nat.sum (file.toList.map getNumbers))

#eval solveB
