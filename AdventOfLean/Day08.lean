import Std.Data.RBMap.Basic
import Std.Data.Nat.Gcd

def exampleA := [
"RL",
"",
"AAA = (BBB, CCC)",
"BBB = (DDD, EEE)",
"CCC = (ZZZ, GGG)",
"DDD = (DDD, DDD)",
"EEE = (EEE, EEE)",
"GGG = (GGG, GGG)",
"ZZZ = (ZZZ, ZZZ)"
]

def exampleB := [
"LLR",
"",
"AAA = (BBB, BBB)",
"BBB = (AAA, ZZZ)",
"ZZZ = (ZZZ, ZZZ)"
]

abbrev Node := String
abbrev Nodes := Std.RBMap Node (Node × Node) compare

structure Map where
  dirs : List Char
  nodes : Nodes
deriving Repr

def Map.parse (input : List String): Map :=
  let dirs := input.head!.toList
  let rec parseNodes (acc : Nodes) : List String -> Nodes
    | [] => acc
    | s :: rest =>
        let src := s.take 3
        let dst := ((s.drop 7).take 3, (s.drop 12).take 3)
        parseNodes (acc.insert src dst) rest
  Map.mk dirs (parseNodes Std.RBMap.empty (input.drop 2))

#eval Map.parse exampleA

partial def solveA (map : Map) : Nat :=
  let rec go (acc : (Node × Nat)) : List Char -> Nat
    | [] => go acc map.dirs
    | c :: rest =>
        match map.nodes.find? acc.fst with
          | some next =>
            let newNode := if c == 'R' then next.snd else next.fst
            if newNode == "ZZZ" then
              acc.snd
            else
              go (newNode, acc.snd + 1) rest
          | none => 0
  go ("AAA", 1) map.dirs

-- #eval solveA (Map.parse exampleA)
-- #eval solveA (Map.parse exampleB)

def doSolveA : IO Unit := do
  let file <- IO.FS.lines "./data/day08.txt"
  IO.println (solveA $ Map.parse file.toList)

-- #eval doSolveA

partial def solveB (map : Map) : Nat :=
  let rec go (acc : (Node × Nat)) : List Char -> Nat
    | [] => go acc map.dirs
    | c :: rest =>
        match map.nodes.find? acc.fst with
          | some next =>
            let newNode := if c == 'R' then next.snd else next.fst
            if newNode.back == 'Z' then acc.snd else go (newNode, acc.snd + 1) rest
          | _ => 0

  let starts := map.nodes.keysList.filter (fun n => n.back == 'A')
  starts.foldl (fun acc start => acc.lcm (go (start, 1) map.dirs)) 1

def exampleC := [
"LR",
"",
"11A = (11B, XXX)",
"11B = (XXX, 11Z)",
"11Z = (11B, XXX)",
"22A = (22B, XXX)",
"22B = (22C, 22C)",
"22C = (22Z, 22Z)",
"22Z = (22B, 22B)",
"XXX = (XXX, XXX)"
]

#eval solveB (Map.parse exampleC)

def doSolveB : IO Unit := do
  let file <- IO.FS.lines "./data/day08.txt"
  IO.println (solveB $ Map.parse file.toList)

#eval doSolveB
