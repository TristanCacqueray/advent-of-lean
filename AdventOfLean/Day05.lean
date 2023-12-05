import Std.Data.RBMap.Basic

def exampleA := [
"seeds: 79 14 55 13",
"",
"seed-to-soil map:",
"50 98 2",
"52 50 48",
"",
"soil-to-fertilizer map:",
"0 15 37",
"37 52 2",
"39 0 15",
"",
"fertilizer-to-water map:",
"49 53 8",
"0 11 42",
"42 0 7",
"57 7 4",
"",
"water-to-light map:",
"88 18 7",
"18 25 70",
"",
"light-to-temperature map:",
"45 77 23",
"81 45 19",
"68 64 13",
"",
"temperature-to-humidity map:",
"0 69 1",
"1 0 69",
"",
"humidity-to-location map:",
"60 56 37",
"56 93 4"
]

structure Range where
  dest   : Nat
  source : Nat
  size   : Nat
deriving Repr

structure Map where
  source : String
  dest   : String
  ranges : List Range
deriving Repr

structure Almanac where
  seeds : List Nat
  maps  : List Map
deriving Repr

partial def Almanac.parse (input : List String) : Almanac :=
  let rec parseNums (acc : List Nat) (line : String) : List Nat :=
    let word := line.takeWhile Char.isDigit
    match word.toNat? with
     | none => acc.reverse
     | some n => parseNums (n :: acc) (line.drop (word.length + 1))
  let seeds := parseNums [] (input.head!.drop "seeds: ".length)

  let rec parseMaps (acc : List Map) (lines : List String) : List Map :=
    match lines.head? with
     | none => acc.reverse
     | some header =>
         let source := header.takeWhile (. != '-')
         let dest := (header.drop (source.length + "-to-".length)).takeWhile (. != ' ')
         let rec parseRanges (acc : List Range) : List String -> List Range
           | x :: xs =>
             match parseNums [] x with
              | [a, b, c] => parseRanges (Range.mk a b c :: acc) xs
              | _other => parseRanges acc []
           | _ => acc.reverse
         let ranges := parseRanges [] (lines.drop 1)
         let newAcc := Map.mk source dest ranges :: acc
         parseMaps newAcc (lines.drop (2 + ranges.length))
  Almanac.mk seeds (parseMaps [] (input.drop 2))

#eval Almanac.parse exampleA

def Range.apply (range : Range) (val : Nat) : Option Nat :=
  if val >= range.source && val < (range.source + range.size) then
     some (range.dest + (val - range.source))
  else none

def Map.apply (map : Map) (seed : Nat) : Nat :=
  let rec go : List Range -> Nat
   | [] =>
       seed
   | range :: rest =>
       match range.apply seed with
        | none => go rest
        | some v => v
  go map.ranges

def Almanac.solveA (almanac : Almanac) : Nat :=
  let rec seedLoc (seed : Nat) : List Map -> Nat
    | [] => seed
    | map :: rest => seedLoc (map.apply seed) rest
  let seeds := almanac.seeds.map (seedLoc . almanac.maps)
  seeds.minimum?.getD 0

#eval (Almanac.parse exampleA).solveA

def doSolveA : IO Unit :=
  IO.FS.lines "./data/day05.txt" >>= fun file => IO.println (Almanac.parse file.toList).solveA

#eval doSolveA
