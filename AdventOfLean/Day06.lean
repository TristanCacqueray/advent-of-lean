def winningMove (time : Float) (distance : Float) : Nat :=
  let sqrt := (time ^ 2 - 4 * distance).sqrt
  let res := ((time + sqrt) / 2).ceil - ((time - sqrt) / 2).floor - 1
  res.toUSize.toNat

def solveP (input: List (Float × Float)) : Nat :=
  (input.map (fun (t, d) => winningMove t d)).foldr (. * .) 1

#eval solveP [(7, 9), (15, 40), (30, 200)]

#eval solveP [(47, 282), (70, 1079), (75, 1147), (66, 1062)]

#eval solveP [(71530, 940200)]

#eval solveP [(47707566, 282107911471062)]
