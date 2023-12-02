import Mathlib.Control.Traversable.Basic

namespace Day2
open Lean.Parsec

def exampleA := [
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
  "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
  "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
  "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
  "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
]

structure CubesCount where
  red   : Nat
  green : Nat
  blue  : Nat
deriving Repr

def cubeCount : Lean.Parsec CubesCount := do
  let countStr <- manyChars digit
  ws
  let count := countStr.toNat!
  let redCube := pstring "red" >>= fun _ => pure (CubesCount.mk count 0 0)
  let greenCube := pstring "green" >>= fun _ => pure (CubesCount.mk 0 count 0)
  let blueCube := pstring "blue" >>= fun _ => pure (CubesCount.mk 0 0 count)
  redCube <|> greenCube <|> blueCube

partial def sepBy (p : Lean.Parsec α) (sep : Char) : Lean.Parsec (List α) := do
  let rec go (acc : List α) := do
    let item <- p
    let newAcc := item :: acc
    let c <- peek?
    if c == some sep then skip >>= fun _ => ws >>= fun _ => go newAcc
    else pure newAcc
  let xs <- go []
  pure $ xs.reverse

#eval Lean.Parsec.run (sepBy cubeCount ',') "42 red, 23 blue"

def cubeMappend (c1 : CubesCount) (c2 : CubesCount) := CubesCount.mk
  (c1.red + c2.red) (c1.green + c2.green) (c1.blue + c2.blue)

instance : Inhabited CubesCount where
  default := CubesCount.mk 0 0 0

def cubeList : Lean.Parsec CubesCount := do
  let xs <- sepBy cubeCount ','
  pure $ xs.foldr cubeMappend default

#eval Lean.Parsec.run cubeList "42 red, 23 blue"

def cubesList : Lean.Parsec (List CubesCount) := sepBy cubeList ';'

#eval Lean.Parsec.run cubesList "42 red, 23 blue; 7 green"

structure Game where
  id    : Nat
  cubes : List CubesCount
deriving Repr

def game : Lean.Parsec Game := do
  let _ <- pstring "Game "
  let idStr <- manyChars digit
  let _ <- pstring ": "
  let cubes <- cubesList
  pure $ Game.mk idStr.toNat! cubes

#eval Lean.Parsec.run game exampleA.head!

def isCubesCountValid (cubes : CubesCount) : Bool :=
  cubes.red <= 12 && cubes.green <= 13 && cubes.blue <= 14

def isGameValid (game : Game) : Bool :=
  game.cubes.all isCubesCountValid

def solveA (input : List String) : IO Unit := do
  match List.filter isGameValid <$> sequence (input.map (Lean.Parsec.run game)) with
    | .ok xs => IO.println (Nat.sum $ xs.map Game.id)
    | .error err => IO.println err

def doSolveA := do
  let file <- IO.FS.lines "./data/day02.txt"
  solveA file.toList

#eval doSolveA
