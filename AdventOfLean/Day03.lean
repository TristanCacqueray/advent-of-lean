import Mathlib.Data.Nat.Basic

def exampleA := [
"467..114..",
"...*......",
"..35..633.",
"......#...",
"617*......",
".....+.58.",
"..592.....",
"......755.",
"...$.*....",
".664.598.."
]

structure SymPos where
  sym : Char
  row : Nat
  col : Nat
deriving Repr

structure NumPos where
  row  : Nat
  col  : Nat
  len  : Nat
  val  : Nat
deriving Repr

abbrev Schema : Type := List NumPos × List SymPos

def parseNumber (s : String) : Option (Nat × Nat) :=
  let numStr := s.takeWhile Char.isDigit
  match numStr.toNat? with
    | some n => some (n, numStr.length)
    | none => none

partial def parse (input : List String) : Schema :=
  let rec parseLine (acc : Schema) (row : Nat) : (Nat × String) -> Schema
    | (_, "") => acc
    | (col, line) =>
        match parseNumber line with
          | some (n, len) => parseLine (NumPos.mk row col len n :: acc.fst, acc.snd) row (col + len, line.drop len)
          | none =>
              let newAcc := match line.front with
                            | '.' => acc
                            | c => (acc.fst, SymPos.mk c row col :: acc.snd)
              parseLine newAcc row (col + 1, line.drop 1)

  let rec go (acc : Schema) (row : Nat) : List String -> Schema
    | [] => acc
    | line :: rest => go (parseLine acc row (0, line)) (row + 1) rest

  go ([], []) 0 input

#eval (parse exampleA)

def isTouching (num : NumPos) (sym : SymPos) : Bool :=
  (sym.row >= num.row - 1 && sym.row <= num.row + 1) &&
  (sym.col >= num.col - 1 && sym.col <= num.col + num.len)

def isPartNumber (num : NumPos) : List SymPos -> Bool
  | [] => false
  | sym :: rest =>
     if isTouching num sym then true
     else isPartNumber num rest

def solveA (schema : Schema) :=
  let valid := schema.fst.filter (fun n => isPartNumber n schema.snd)
  Nat.sum $ valid.map (fun n => n.val)

#eval solveA (parse exampleA)

def doSolveA := do
  let file <- IO.FS.lines "./data/day03.txt"
  IO.println (solveA (parse file.toList))

-- #eval doSolveA


def gearPos (stars : List SymPos) (num : NumPos) : Option SymPos :=
  stars.find? (fun star => isTouching num star)

def getGear (stars : List SymPos) (nums : List NumPos) : List (Nat × Nat) :=
  let rec go (acc : List (Nat × Nat)) : List NumPos -> List (Nat × Nat)
    | [] => acc
    | num :: rest =>
        let newAcc := match gearPos stars num with
                       | some star => match rest.find? (fun other => isTouching other star) with
                                       | some other => (num.val, other.val) :: acc
                                       | none => acc
                       | none => acc
        go newAcc rest
  go [] nums

def getGears (schema : Schema) : List (Nat × Nat) :=
  let stars := schema.snd.filter (fun sym => sym.sym == '*')
  getGear stars schema.fst

def solveB (schema : Schema) : Nat :=
  let gears := getGears schema
  Nat.sum (gears.map (fun gear => gear.fst * gear.snd))

#eval (solveB $ parse exampleA)

def doSolveB := do
  let file <- IO.FS.lines "./data/day03.txt"
  IO.println (solveB (parse file.toList))

-- #eval doSolveB
