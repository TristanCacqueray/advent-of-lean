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

-- #eval Almanac.parse exampleA

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

-- #eval doSolveA

abbrev SeedRange : Type := Nat × Nat

-- | Check how seed range overlap with a map range. Return the list of unaffected seeds × maybe relocated seeds
def Range.applyB (range : Range) (seeds : SeedRange) : (List SeedRange × Option SeedRange) :=
  if seeds.fst >= range.source + range.size || seeds.snd < range.source then
    -- seeds are outside
    ([seeds], none)
  else if seeds.fst >= range.source && seeds.snd < range.source + range.size then
    -- seeds are inside
    ([], some (range.dest + (seeds.fst - range.source), range.dest + (seeds.snd - range.source)))
  else if seeds.fst <= range.source && seeds.snd >= range.source && seeds.snd < range.source + range.size then
    -- lower overlap
    ([(seeds.fst, range.source - 1)], some (range.dest, range.dest + (seeds.snd - range.source)))
  else if seeds.fst < range.source + range.size then
    -- upper overlap
    ([(range.source + range.size, seeds.snd)], some (range.dest + (seeds.fst - range.source), range.dest + range.size - 1))
  else
    -- full overlap
    ([(seeds.fst, range.source - 1), (range.source + range.size, seeds.snd)], some (range.dest, range.dest + range.size - 1))

-- | Apply a map to a given seed range
def Map.applyB (map : Map) (seeds : SeedRange) : List SeedRange :=
  -- return the list of (unaffected, relocated) seeds for a given range
  let rec gos (range : Range) (acc : (List SeedRange × List SeedRange)) : (todo : List SeedRange) -> (List SeedRange × List SeedRange)
   | [] => acc
   | seeds :: rest => match range.applyB seeds with
      | (seeds, none) => gos range (seeds ++ acc.fst, acc.snd) rest
      | (seeds, some done) => gos range (seeds ++ acc.fst, done :: acc.snd) rest

  -- return the list of seeds modified by the given map
  let rec go (acc : List SeedRange) (todo : List SeedRange): List Range -> List SeedRange
   | [] => acc.reverse ++ todo
   | range :: rest =>
       let (newTodo, done) := gos range ([], []) todo
       -- dbg_trace "Applied range {range.source}-{range.source + range.size} for {todo} to {range.dest}-{range.dest + range.size}, newTodo {newTodo} done {done}"
       go (done ++ acc) newTodo rest
  -- dbg_trace "Applying map {map.source} to {seeds}"
  let res := go [] [seeds] map.ranges
  -- dbg_trace "=> {res}\n"
  res

def Almanac.solveB (almanac : Almanac) : Nat :=
  let rec getRanges (acc : List SeedRange) : List Nat -> List SeedRange
    | a :: b :: rest => getRanges ((a, a + b) :: acc) rest
    | _ => acc.reverse
  let seeds := getRanges [] almanac.seeds
  -- dbg_trace "got {seeds}\n"

  -- apply every maps
  let rec go (seeds : List SeedRange) : List Map -> List SeedRange
    | [] => seeds
    | map :: rest => go (List.join (seeds.map (map.applyB .))) rest
  let finals := go seeds almanac.maps
  -- dbg_trace "{finals}"

  -- return the lowest seed
  (finals.map (fun final => final.fst)).minimum?.getD 0

#eval (Almanac.parse exampleA).solveB

def doSolveB : IO Unit :=
  IO.FS.lines "./data/day05.txt" >>= fun file => IO.println (Almanac.parse file.toList).solveB

-- #eval doSolveB
-- too high 122674836
