-- My notes from https://leanprover.github.io/functional_programming_in_lean
namespace FP

#eval "Hello, world!"

def add1 (n : Nat) : Nat := n + 1

#eval add1 7

#eval String.append "Hello, " "Lean!"
#eval String.append "it is " (if 1 > 2 then "yes" else "no")

-- Without the anotation, literal number are natural, so the result would have been 0
#eval (1 - 2 : Int)


-- 1.3 Definitions
def hello := "Hello"
def lean : String := "Lean"
#eval String.append hello (String.append " " lean)

def maximum (n : Nat) (k : Nat) : Nat :=
  if n < k then
    k
  else n

-- Use check for :type
#check maximum
#check (maximum)
#check (maximum 9000)

def joinStringsWith (sep : String) (s1 : String) (s2 : String) :=
  String.append s1 (String.append sep s2)

#check (joinStringsWith)
#eval joinStringsWith ", " hello lean

def volume (height : Nat) (width : Nat) (depth : Nat) := height * width * depth

#eval volume 1 1 1

-- defining type
def Str : Type := String

end FP

-- reducible types
abbrev N : Type := Nat

def thirtyNine : N := 39


-- | structures
structure Point where
  x : Float
  y : Float
deriving Repr

-- Type annocation inside the curly braces
def origin : Point := { x := 0.0, y := 0.0 : Point }

def distance (p1 : Point) (p2 : Point) : Float :=
  Float.sqrt (((p2.x - p1.x) ^ 2.0) + ((p2.y - p1.y) ^ 2.0))

-- update:
def zeroX (p : Point) : Point :=
  { p with x := 0 }

-- constructor:
#check (Point.mk)

-- selector:
#check (Point.x)

structure RectangularPrism where
  width  : Float
  height : Float
  depth  : Float
deriving Repr

def volume (shape : RectangularPrism) : Float :=
  shape.width * shape.height * shape.depth

-- custom constructor name:
structure Book where
  makeBook ::
  title : String
  author : String
  price : Float

#check (Book.makeBook)

-- | pattern matching
def isZero (n : Nat) : Bool :=
  match n with
  | Nat.zero => true
  | Nat.succ _k => false

-- explicit type param:
def lengthExplicit (α : Type) (xs : List α) : Nat :=
  match xs with
  | [] => 0
  | _y :: ys => Nat.succ (lengthExplicit α ys)

#eval (lengthExplicit Nat [0, 2, 40])

-- implicit type param:
def length {α : Type} (xs : List α) : Nat :=
  match xs with
  | [] => 0
  | y :: ys => Nat.succ (length ys)

#eval (length [0, 2, 40])

-- automatic type params:
def lengthAuto (_xs : List a) : Nat :=
  42

-- making implicit param explicit:
#eval (length [] (α := Float))


-- tuple
def fivesExplicit : String × Int := { fst := "five", snd := 5 }

def fives : String × Int × Float := ("five", 5, 5)

def last? {a : Type} (xs : List a) : Option a :=
  match xs with
    | [] => none
    | x :: [] => some x
    | _ :: rest => last? rest

#eval (last? [true, false])

def zip {α β : Type} (xs : List α) (ys : List β) : List (α × β) :=
  match xs, ys with
    | x :: xrest, y :: yrest => {fst := x, snd := y} :: (zip xrest yrest)
    | _,_ => []

def take {a : Type} (count : Nat) (xs : List a) : List a :=
  match count, xs with
    | Nat.succ n, x :: rest => x :: take (n - 1) rest
    | Nat.zero, x :: _ => [x]
    | _, _ => []

#eval (take 3 [1, 2, 3, 4])

-- lambda functions:
#check (fun x => x + 41)
#check (. + 41)

-- | namespace
namespace NewNamespace
def triple (x : Nat) : Nat := 3 * x
def quadruple (x : Nat) : Nat := 2 * x + 2 * x
end NewNamespace

-- add to namespace:
def NewNamespace.quint := (. * 5)
#check NewNamespace.quint
